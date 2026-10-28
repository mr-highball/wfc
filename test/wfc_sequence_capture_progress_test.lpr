(*
MIT License

Copyright (c) 2026 mr-highball

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)
program wfc_sequence_capture_progress_test;

{$mode delphi}{$H+}

uses SysUtils, wfc, wfc_model, wfc_sequence, wfc_sequence_learn,
  wfc_sequence_text, wfc_sequence_graph;

type
  TObserver = class
  public
    Enabled: Boolean;
    Phase: TWfcSequencePreparationPhase;
    Mode: Integer;
    Marker: Exception;
    Calls: Integer;
    Last: array[TWfcSequencePreparationPhase] of Integer;
    Seen: array[TWfcSequencePreparationPhase] of Boolean;
    procedure Reset;
    procedure Observe(const AWork: TWfcSequencePreparationProgress);
  end;

var
  GChecks: Integer;
  GAborts: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GChecks);
  if not ACondition then
    raise Exception.Create(AMessage);
end;

procedure TObserver.Reset;
var
  LPhase: TWfcSequencePreparationPhase;
begin
  for LPhase := Low(LPhase) to High(LPhase) do
  begin
    Last[LPhase] := 0;
    Seen[LPhase] := False;
  end;
end;

procedure TObserver.Observe(const AWork: TWfcSequencePreparationProgress);
var
  LPoint: Boolean;
begin
  Inc(Calls);
  Check((AWork.Done >= 0) and (AWork.Total >= AWork.Done), 'Invalid actual row count');
  Check(AWork.Done >= Last[AWork.Phase], 'Phase row count went backwards');
  Last[AWork.Phase] := AWork.Done;
  Seen[AWork.Phase] := True;
  LPoint := ((Mode = 0) and (AWork.Done = 0)) or
    ((Mode = 1) and (AWork.Done > 0) and (AWork.Done < AWork.Total)) or
    ((Mode = 2) and (AWork.Total > 0) and (AWork.Done = AWork.Total));
  if Enabled and (AWork.Phase = Phase) and LPoint then
    raise Marker;
end;

function AuthoredGraph(const AModel: TWfcSequenceModel;
  const ATokens: TWfcModelTokens; const ACells: Integer): TGraph;
var
  LGraph: TGraph;
  LMasks: TWfcSequenceTokenConstraints;
  LValues: TGraphValues;
  LIndex: Integer;
begin
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(ACells, 1, 1);
    LGraph.WrapNeighbors := False;
    ApplySequenceModelToGraph(AModel, LGraph, wseFragment);
    SetLength(LMasks, ACells);
    for LIndex := 0 to ACells - 1 do
      LMasks[LIndex] := MakeWfcSequenceTokenConstraint(LIndex,
        [ATokens[LIndex mod Length(ATokens)]]);
    IntersectSequenceTokenConstraints(AModel, LGraph, LMasks);
    for LIndex := 0 to ACells - 1 do
    begin
      LValues := LGraph.CopyAllowedValues(LIndex, 0, 0);
      Check(Length(LValues) = 1, 'Authored public mask is not singleton');
      LGraph.Entry[LIndex, 0, 0].Value := LValues[0];
    end;
    Result := LGraph;
  except
    LGraph.Free;
    raise;
  end;
end;

procedure SameReport(const A, B: TWfcSequenceGraphValidationReport);
begin
  Check((A.Valid = B.Valid) and (A.CheckedStates = B.CheckedStates) and
    (A.CheckedTransitions = B.CheckedTransitions) and (A.Issue.Kind = B.Issue.Kind) and
    (A.Issue.Position = B.Issue.Position) and
    (A.Issue.RelatedPosition = B.Issue.RelatedPosition) and
    (A.Issue.StateIndex = B.Issue.StateIndex) and
    (A.Issue.RelatedStateIndex = B.Issue.RelatedStateIndex), 'Validation report differs');
end;

procedure SamePath(const A, B: TWfcSequenceStateIndices;
  const ATokens, BTokens: TWfcModelTokens);
var
  LIndex: Integer;
begin
  Check((Length(A) = Length(B)) and (Length(ATokens) = Length(BTokens)), 'Path length differs');
  for LIndex := 0 to High(A) do
    Check((A[LIndex] = B[LIndex]) and (ATokens[LIndex] = BTokens[LIndex]),
      'Authored state/token replay differs');
end;

procedure RunControls;
var
  LTokens: TWfcModelTokens;
  LModel: TWfcSequenceModel;
  LGraph: TGraph;
  LObserver: TObserver;
  LPathModel: TWfcSequenceModel;
  LExpected: TWfcGeneratedSequence;
  LSequence: TWfcGeneratedSequence;
  LExpectedSegment: TWfcGeneratedSequenceSegment;
  LSegment: TWfcGeneratedSequenceSegment;
  LExpectedReport: TWfcSequenceGraphValidationReport;
  LReport: TWfcSequenceGraphValidationReport;
  LOtherReport: TWfcSequenceGraphValidationReport;
  LProjected: TWfcModelTokens;
  LStates: TWfcSequenceStateIndices;
  LValues: TGraphValues;
  LModelText: String;
  LDomain: TGraphValues;
  LIndex: Integer;
  LOperation: Integer;
  LMode: Integer;
  LPhase: TWfcSequencePreparationPhase;
  LBadPosition: Integer;
  LPreviousBad: Integer;
  LResult: Boolean;
  LAborted: Boolean;
  LSupports: Boolean;

  procedure RunOperation(const AObserved: Boolean);
  var
    LCallback: TWfcSequencePreparationObserver;
  begin
    LCallback := nil;
    if AObserved then
    begin
      LObserver.Reset;
      LCallback := LObserver.Observe;
    end;
    case LOperation of
      0: LResult := CaptureSolvedSequence(LModel, LGraph, wseFragment,
        LSequence, LReport, LCallback);
      1: LResult := CaptureSolvedSequence(LModel, LGraph,
        LSequence, LReport, LCallback);
      2: LResult := CaptureSolvedSequenceSegment(LModel, LGraph,
        MakeWfcSequenceInitialSegmentBoundary(False), LSegment, LReport, LCallback);
      3: LResult := ValidateSequenceStatePath(LModel, LExpected.StateIndices,
        wseFragment, LReport, LCallback);
      4: LResult := ValidateSequenceStatePath(LModel, LExpected.StateIndices,
        wmbOpen, LReport, LCallback);
      5: LResult := ValidateSequenceSegmentStatePath(LModel, LExpected.StateIndices,
        MakeWfcSequenceInitialSegmentBoundary(False), LReport, LCallback);
      6: LResult := SequenceStatesSatisfyEntryConstraints(LModel, LGraph,
        LExpected.StateIndices, LBadPosition, LCallback);
      7: begin
        LProjected := LModel.ProjectStateIndices(LExpected.StateIndices, LCallback);
        LResult := True;
      end;
    end;
  end;

  procedure CheckGraph;
  var
    LCell: Integer;
    LCurrent: TGraphValues;
  begin
    Check(EncodeWfcSequenceText(LModel) = LModelText, 'Read-only observer changed model');
    for LCell := 0 to High(LValues) do
    begin
      Check(LGraph.Entry[LCell, 0, 0].Value = LValues[LCell], 'Capture changed graph value');
      LCurrent := LGraph.CopyAllowedValues(LCell, 0, 0);
      Check((Length(LCurrent) = 1) and (LCurrent[0] = LValues[LCell]),
        'Capture changed graph domain');
    end;
  end;
begin
  { Sixty-five model rows and130 cells cross every64-row interior checkpoint
    without multiplying heap-trace cost across the full abort matrix. }
  SetLength(LTokens, 65);
  for LIndex := 0 to High(LTokens) do
    LTokens[LIndex] := 'item:' + IntToStr(LIndex);
  LModel := LearnSequenceModelCorpus([MakeWfcSequenceSample(LTokens)], 1);
  LGraph := nil;
  LObserver := TObserver.Create;
  try
    LModelText := EncodeWfcSequenceText(LModel);
    LGraph := AuthoredGraph(LModel, LTokens, 130);
    SetLength(LValues, 130);
    for LIndex := 0 to High(LValues) do
      LValues[LIndex] := LGraph.Entry[LIndex, 0, 0].Value;
    Check(CaptureSolvedSequence(LModel, LGraph, wseFragment, LExpected,
      LExpectedReport), 'Unobserved capture failed');
    Check(CaptureSolvedSequenceSegment(LModel, LGraph,
      MakeWfcSequenceInitialSegmentBoundary(False), LExpectedSegment,
      LOtherReport), 'Unobserved segment failed');
    SameReport(LExpectedReport, LOtherReport);
    for LIndex := 0 to High(LValues) do
      Check((LExpected.StateIndices[LIndex] = LIndex mod Length(LTokens)) and
        (LExpected.Tokens[LIndex] = LTokens[LIndex mod Length(LTokens)]), 'Authored ordinal differs');

    for LOperation := 0 to 7 do
    begin
      for LPhase := wspValidateGraphModel to High(LPhase) do
      begin
        LSupports := ((LOperation <= 2) and (LPhase in [wspValidateGraphModel,
          wspCaptureSequenceCells, wspValidatePathStates, wspValidatePathTransitions,
          wspValidateProjectionStates, wspProjectStateTokens, wspCaptureReady])) or
          ((LOperation in [3, 4, 5]) and
            (LPhase in [wspValidatePathStates, wspValidatePathTransitions])) or
          ((LOperation = 6) and (LPhase in [wspValidateGraphModel, wspValidateEntryCells])) or
          ((LOperation = 7) and (LPhase in [wspValidateProjectionStates, wspProjectStateTokens]));
        if not LSupports then
          Continue;
        for LMode := 0 to 2 do
        begin
          if (LPhase = wspCaptureReady) and (LMode <> 2) then
            Continue;
          LSequence := LExpected;
          LSequence.StateIndices := [-11];
          LSequence.Tokens := ['prior sequence'];
          LSequence.Boundary := wmbWrap;
          LSequence.Extent := wseWrap;
          LSegment := LExpectedSegment;
          LSegment.StateIndices := [-12];
          LSegment.Tokens := ['prior segment'];
          LSegment.Boundary := MakeWfcSequenceContinuingSegmentBoundary(55, True);
          LProjected := ['prior projection'];
          LObserver.Phase := LPhase;
          LObserver.Mode := LMode;
          LObserver.Enabled := True;
          LObserver.Marker := Exception.Create('Big Boss original capture marker');
          LAborted := False;
          try
            try
              RunOperation(True);
            except
              on LError: Exception do
              begin
                Check(LError = LObserver.Marker, 'Original capture exception changed');
                LObserver.Marker := nil;
                LAborted := True;
                Inc(GAborts);
              end;
            end;
          finally
            LObserver.Marker.Free;
            LObserver.Marker := nil;
            LObserver.Enabled := False;
          end;
          Check(LAborted, 'Expected phase/mode was not reached');
          Check((Length(LSequence.Tokens) = 1) and (LSequence.Tokens[0] = 'prior sequence') and
            (LSequence.StateIndices[0] = -11) and (LSequence.Boundary = wmbWrap) and
            (LSequence.Extent = wseWrap), 'Aborted capture changed caller sequence');
          Check((Length(LSegment.Tokens) = 1) and (LSegment.Tokens[0] = 'prior segment') and
            (LSegment.StateIndices[0] = -12) and LSegment.Boundary.HasPrevious and
            (LSegment.Boundary.PreviousState = 55) and LSegment.Boundary.RequireObservedEnd,
            'Aborted capture changed caller segment');
          Check((Length(LProjected) = 1) and (LProjected[0] = 'prior projection'),
            'Aborted projection changed caller result');
          CheckGraph;
          RunOperation(True);
          Check(LResult, 'Clean same-graph retry failed');
          if LOperation < 2 then
            SamePath(LExpected.StateIndices, LSequence.StateIndices, LExpected.Tokens, LSequence.Tokens);
          if LOperation = 2 then
            SamePath(LExpected.StateIndices, LSegment.StateIndices, LExpected.Tokens, LSegment.Tokens);
          if LOperation < 6 then
            SameReport(LExpectedReport, LReport);
          if LOperation = 6 then
            Check(LBadPosition = -1, 'Successful domain proof reports a failure');
          if LOperation = 7 then
            SamePath(LExpected.StateIndices, LExpected.StateIndices, LExpected.Tokens, LProjected);
        end;
      end;
    end;

    LObserver.Reset;
    LStates := Copy(LExpected.StateIndices);
    LStates[0] := -1;
    Check(not ValidateSequenceStatePath(LModel, LStates, wseFragment, LReport), 'Invalid state accepted');
    Check(not ValidateSequenceStatePath(LModel, LStates, wseFragment, LOtherReport,
      LObserver.Observe), 'Observed invalid state accepted');
    SameReport(LReport, LOtherReport);
    Check(LReport.Issue.Kind = wsgikStateIndex, 'Invalid state issue differs');
    { Order1 has no preceding history and does not assert observed adjacency.
      Use an actual order2 model for transition and incoming-seam rejection. }
    LPathModel := LearnSequenceModelCorpus([MakeWfcSequenceSample(['a','b','c','d'])], 2);
    try
      LStates := [0, 2, 1, 3];
      Check(not ValidateSequenceStatePath(LPathModel, LStates, wseWhole, LReport), 'Broken path accepted');
      LObserver.Reset;
      Check(not ValidateSequenceStatePath(LPathModel, LStates, wseWhole, LOtherReport,
        LObserver.Observe), 'Observed broken path accepted');
      SameReport(LReport, LOtherReport);
      Check(LReport.Issue.Kind = wsgikTransition, 'Broken path issue differs');
      LStates := [1, 2, 3];
      LObserver.Reset;
      Check(ValidateSequenceSegmentStatePath(LPathModel, LStates,
        MakeWfcSequenceContinuingSegmentBoundary(0, True), LReport,
        LObserver.Observe), 'Valid incoming seam rejected');
      Check((LReport.CheckedTransitions = 3) and
        (LObserver.Last[wspValidatePathTransitions] = 3), 'Incoming seam count differs');
      LStates := [2, 3];
      LObserver.Reset;
      Check(not ValidateSequenceSegmentStatePath(LPathModel, LStates,
        MakeWfcSequenceContinuingSegmentBoundary(0, False), LReport,
        LObserver.Observe), 'Wrong incoming seam accepted');
      Check((LReport.Issue.Kind = wsgikTransition) and (LReport.Issue.Position = -1),
        'Incoming seam coordinates differ');
    finally
      LPathModel.Free;
    end;
    LDomain := LGraph.CopyAllowedValues(0, 0, 0);
    LGraph.SetAllowedValues(0, 0, 0, []);
    LObserver.Reset;
    Check(not SequenceStatesSatisfyEntryConstraints(LModel, LGraph,
      LExpected.StateIndices, LPreviousBad), 'Empty domain accepted');
    Check(not SequenceStatesSatisfyEntryConstraints(LModel, LGraph,
      LExpected.StateIndices, LBadPosition, LObserver.Observe), 'Observed empty domain accepted');
    Check((LPreviousBad = 0) and (LBadPosition = 0), 'Failed domain position differs');
    LGraph.SetAllowedValues(0, 0, 0, LDomain);
    LGraph.Entry[0, 0, 0].ClearValue;
    LSequence := LExpected;
    LObserver.Reset;
    Check(not CaptureSolvedSequence(LModel, LGraph, wseFragment, LSequence,
      LReport, LObserver.Observe), 'Empty captured cell accepted');
    Check((Length(LSequence.Tokens) = 0) and (Length(LSequence.StateIndices) = 0) and
      (LReport.Issue.Kind = wsgikEmptyCell), 'False capture contract changed');
    LGraph.Entry[0, 0, 0].Value := LValues[0];
    CheckGraph;
    LObserver.Free;
    LObserver := nil;
    Check(CaptureSolvedSequence(LModel, LGraph, wseFragment, LSequence,
      LReport), 'Borrowed observer survived call lifetime');
    SamePath(LExpected.StateIndices, LSequence.StateIndices, LExpected.Tokens, LSequence.Tokens);
  finally
    LObserver.Free;
    LGraph.Free;
    LModel.Free;
  end;
end;

procedure RunLarge;
var
  LTokens: TWfcModelTokens;
  LSample: TWfcModelTokens;
  LModel: TWfcSequenceModel;
  LGraph: TGraph;
  LObserver: TObserver;
  LSequence: TWfcGeneratedSequence;
  LReport: TWfcSequenceGraphValidationReport;
  LIndex: Integer;
begin
  SetLength(LTokens, 256);
  for LIndex := 0 to High(LTokens) do
    LTokens[LIndex] := 'large:' + IntToStr(LIndex);
  LSample := Copy(LTokens);
  SetLength(LSample, 512);
  for LIndex := 256 to High(LSample) do
    LSample[LIndex] := LTokens[LIndex - 256];
  LModel := LearnSequenceModelCorpus([MakeWfcSequenceSample(LSample)], 1);
  LGraph := nil;
  LObserver := TObserver.Create;
  try
    LGraph := AuthoredGraph(LModel, LTokens, 1024);
    LObserver.Reset;
    Check(CaptureSolvedSequence(LModel, LGraph, wseFragment, LSequence,
      LReport, LObserver.Observe), 'Large observed capture failed');
    Check((LObserver.Last[wspValidateGraphModel] = 256) and
      (LObserver.Last[wspCaptureSequenceCells] = 1024) and
      (LObserver.Last[wspValidatePathStates] = 1024) and
      (LObserver.Last[wspValidatePathTransitions] = 1023) and
      (LObserver.Last[wspProjectStateTokens] = 1024) and
      (LObserver.Last[wspCaptureReady] = 1), 'Large actual phase totals differ');
    for LIndex := 0 to 1023 do
      Check((LSequence.StateIndices[LIndex] = LIndex mod 256) and
        (LSequence.Tokens[LIndex] = LTokens[LIndex mod 256]), 'Large exact replay differs');
  finally
    LObserver.Free;
    LGraph.Free;
    LModel.Free;
  end;
end;

begin
  RunControls;
  RunLarge;
  WriteLn('Sequence capture progress: ', GChecks, ' checks; ', GAborts,
    ' original start/interior/final aborts; exact replay');
end.
