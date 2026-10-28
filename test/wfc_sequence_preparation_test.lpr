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
program wfc_sequence_preparation_test;
{$mode delphi}{$H+}
uses SysUtils, wfc, wfc_model, wfc_sequence, wfc_sequence_learn,
  wfc_sequence_text, wfc_sequence_graph;
type
  TDomainSnapshot = record
    Present: array of Boolean;
    Values: array of TGraphValues;
  end;
  TObserver = class
  public
    Calls: Integer;
    Enabled: Boolean;
    FinalOnly: Boolean;
    Phase: TWfcSequencePreparationPhase;
    Marker: Exception;
    Completions: array[TWfcSequencePreparationPhase] of Integer;
    Last: array[TWfcSequencePreparationPhase] of Integer;
    procedure Observe(const AWork: TWfcSequencePreparationProgress);
    procedure Reset;
  end;
var
  GChecks: Integer;
  GAborts: Integer;
  GObserver: TObserver;
procedure Check(const AValue: Boolean; const AMessage: String);
begin
  Inc(GChecks);
  if not AValue then
    raise Exception.Create(AMessage);
end;
procedure TObserver.Reset;
var LPhase: TWfcSequencePreparationPhase;
begin
  for LPhase := Low(LPhase) to High(LPhase) do
  begin
    Completions[LPhase] := 0;
    Last[LPhase] := 0;
  end;
end;
procedure TObserver.Observe(const AWork: TWfcSequencePreparationProgress);
begin
  Inc(Calls);
  Check((AWork.Done >= 0) and (AWork.Total >= AWork.Done), 'Invalid preparation count');
  Check(AWork.Done >= Last[AWork.Phase], 'Completed phase units went backwards');
  Last[AWork.Phase] := AWork.Done;
  if (AWork.Total > 0) and (AWork.Done = AWork.Total) then
    Inc(Completions[AWork.Phase]);
  if Enabled and (AWork.Phase = Phase) and (AWork.Done > 0) and
      ((FinalOnly and (AWork.Done = AWork.Total)) or
      ((not FinalOnly) and (AWork.Done < AWork.Total))) then
    raise Marker;
end;
procedure CheckMalformedProgress;
var
  LCalls: Integer;
  {$ifdef PAS2JS} LBad: Integer; LBadPhase: TWfcSequencePreparationPhase; {$endif}
  procedure Reject(const ADone, ATotal: Integer);
  var LRejected: Boolean;
  begin
    LRejected := False;
    try
      ReportSequencePreparation(GObserver.Observe, wspSnapshotDomains, ADone, ATotal);
    except
      on EWfcSequence do LRejected := True;
    end;
    Check(LRejected, 'Malformed public progress accepted');
  end;
begin
  LCalls := GObserver.Calls;
  Reject(-1, 1);
  Reject(2, 1);
  Reject(0, -1);
  {$ifdef PAS2JS}
  asm LBad = NaN; end;
  Reject(LBad, 1);
  asm LBad = Infinity; end;
  Reject(0, LBad);
  asm LBad = 0.5; end;
  Reject(LBad, 1);
  asm LBadPhase = 0.5; end;
  try
    ReportSequencePreparation(GObserver.Observe, LBadPhase, 0, 1);
    Check(False, 'Fractional phase accepted');
  except
    on EWfcSequence do Check(True, 'Fractional phase rejected');
  end;
  {$endif}
  Check(GObserver.Calls = LCalls, 'Malformed progress reached observer');
end;
function Snapshot(const AGraph: TGraph): TDomainSnapshot;
var I: Integer;
begin
  Result := Default(TDomainSnapshot);
  SetLength(Result.Present, Integer(AGraph.Dimension.Width));
  SetLength(Result.Values, Length(Result.Present));
  for I := 0 to High(Result.Present) do
  begin
    Result.Present[I] := AGraph.HasAllowedValues(I, 0, 0);
    Result.Values[I] := AGraph.CopyAllowedValues(I, 0, 0);
  end;
end;
procedure Same(const ALeft, ARight: TDomainSnapshot);
var I: Integer; J: Integer;
begin
  Check(Length(ALeft.Present) = Length(ARight.Present), 'Domain width differs');
  for I := 0 to High(ALeft.Present) do
  begin
    Check((ALeft.Present[I] = ARight.Present[I]) and
      (Length(ALeft.Values[I]) = Length(ARight.Values[I])), 'Domain presence/length differs');
    for J := 0 to High(ALeft.Values[I]) do
      Check(ALeft.Values[I][J] = ARight.Values[I][J], 'Domain value/order differs');
  end;
end;
function MakeGraph(const AModel: TWfcSequenceModel; const ACells: Integer;
  const ARestrictExisting: Boolean = True): TGraph;
var LValues: TGraphValues;
begin
  Result := TGraph.Create;
  try
    Result.Reshape(ACells, 1, 1);
    Result.WrapNeighbors := True;
    Result.Seed := 731;
    ApplySequenceModelToGraph(AModel, Result, wseWrap);
    if ARestrictExisting then
    begin
      LValues := Result.CopyAllowedValues(1, 0, 0);
      Check(Length(LValues) > 0, 'Authored graph has no initial domain');
      Result.SetAllowedValues(1, 0, 0, [LValues[0]]);
    end;
  except
    Result.Free;
    raise;
  end;
end;
procedure RunControls;
var
  LModel: TWfcSequenceModel;
  LGraph: TGraph;
  LObserved: TGraph;
  LReference: TGraph;
  LMasks: TWfcSequenceTokenConstraints;
  LBad: TWfcSequenceTokenConstraints;
  LBefore: TDomainSnapshot;
  LPristine: TDomainSnapshot;
  LPhase: TWfcSequencePreparationPhase;
  LFinal: Boolean;
  LRejected: Boolean;
  LExact: Boolean;
  LText: String;
  I: Integer;
  LReport: TGraphSolveReport;
begin
  LModel := LearnSequenceModelCorpus([MakeWfcSequenceSample(
    ['A', 'A', 'B', 'A', 'C', 'B', 'B', 'C', 'C', 'A'])], 1);
  LGraph := nil;
  LObserved := nil;
  LReference := nil;
  try
    LText := EncodeWfcSequenceText(LModel);
    LMasks := [MakeWfcSequenceTokenConstraint(0, ['A', 'B']),
      MakeWfcSequenceTokenConstraint(0, ['B', 'C']),
      MakeWfcSequenceTokenConstraint(1, ['A', 'B', 'C']),
      MakeWfcSequenceTokenConstraint(3, ['A', 'C', 'A'])];
    LGraph := MakeGraph(LModel, 128);
    LObserved := MakeGraph(LModel, 128);
    LReference := MakeGraph(LModel, 128);
    for I := 0 to High(LMasks) do
      IntersectSequenceAllowedTokens(LModel, LReference, LMasks[I].Position, LMasks[I].AllowedTokens);
    IntersectSequenceTokenConstraints(LModel, LGraph, LMasks);
    GObserver.Reset;
    IntersectSequenceTokenConstraints(LModel, LObserved, LMasks, GObserver.Observe);
    Same(Snapshot(LReference), Snapshot(LGraph));
    Same(Snapshot(LGraph), Snapshot(LObserved));
    for LPhase := Low(LPhase) to High(LPhase) do
      Check(GObserver.Completions[LPhase] = 1, 'A bulk phase was not completed exactly once');
    Check(LGraph.TrySolve(DefaultGraphSolveOptions, LReport), 'Authored masked graph did not solve');
    Check(LObserved.TrySolve(DefaultGraphSolveOptions, LReport), 'Observed graph did not solve');
    for I := 0 to 127 do
      Check(LGraph.Entry[I, 0, 0].Value = LObserved.Entry[I, 0, 0].Value, 'Seeded replay differs');
    FreeAndNil(LGraph);
    LGraph := MakeGraph(LModel, 128);
    LPristine := Snapshot(LGraph);
    LGraph.ClearAllowedValues(4, 0, 0);
    LGraph.SetAllowedValues(5, 0, 0, []);
    LBefore := Snapshot(LGraph);
    for LPhase := Low(LPhase) to High(LPhase) do
      for LFinal := False to True do
      begin
        GObserver.Reset;
        GObserver.Enabled := True;
        GObserver.Phase := LPhase;
        GObserver.FinalOnly := LFinal;
        GObserver.Marker := Exception.Create('Big Boss generic sequence marker');
        LExact := False;
        try
          try
            IntersectSequenceTokenConstraints(LModel, LGraph, LMasks, GObserver.Observe);
          except
            on LException: Exception do
            begin
              if LException <> GObserver.Marker then
                raise;
              GObserver.Marker := nil;
              LExact := True;
              Inc(GAborts);
            end;
          end;
        finally
          GObserver.Enabled := False;
          GObserver.Marker.Free;
          GObserver.Marker := nil;
        end;
        Check(LExact, 'Original observer exception did not escape');
        Same(LBefore, Snapshot(LGraph));
      end;
    for I := 0 to 1 do
    begin
      LBad := Copy(LMasks);
      if I = 0 then
        LBad[High(LBad)].Position := 128
      else
        LBad[High(LBad)] := MakeWfcSequenceTokenConstraint(3, ['missing']);
      LRejected := False;
      try
        IntersectSequenceTokenConstraints(LModel, LGraph, LBad);
      except
        on Exception do LRejected := True;
      end;
      Check(LRejected, 'Malformed last row accepted');
      Same(LBefore, Snapshot(LGraph));
    end;
    for I := 4 to 5 do
      if LPristine.Present[I] then
        LGraph.SetAllowedValues(I, 0, 0, LPristine.Values[I])
      else
        LGraph.ClearAllowedValues(I, 0, 0);
    GObserver.Reset;
    IntersectSequenceTokenConstraints(LModel, LGraph, LMasks, GObserver.Observe);
    Same(Snapshot(LReference), Snapshot(LGraph));
    IntersectSequenceTokenConstraints(LModel, LGraph, nil);
    IntersectSequenceTokenConstraints(LModel, LGraph, [MakeWfcSequenceTokenConstraint(2, nil)]);
    Check(LGraph.HasAllowedValues(2, 0, 0) and
      (Length(LGraph.CopyAllowedValues(2, 0, 0)) = 0), 'Empty token set was not explicit unsatisfiability');
    Check(EncodeWfcSequenceText(LModel) = LText, 'Borrowed model changed');
  finally
    LReference.Free;
    LObserved.Free;
    LGraph.Free;
    LModel.Free;
  end;
end;
procedure RunLarge;
var
  LTokens: TWfcModelTokens;
  LModel: TWfcSequenceModel;
  LGraph: TGraph;
  LMasks: TWfcSequenceTokenConstraints;
  LPhase: TWfcSequencePreparationPhase;
  LGenerated: TWfcGeneratedSequence;
  LReport: TGraphSolveReport;
  LValidation: TWfcSequenceGraphValidationReport;
  I: Integer;
  {$ifndef PAS2JS} LStart: QWord; {$endif}
begin
  SetLength(LTokens, 512);
  for I := 0 to High(LTokens) do
    LTokens[I] := 't:' + IntToStr(I mod 256);
  LModel := LearnSequenceModelCorpus([MakeWfcSequenceSample(LTokens)], 1);
  LGraph := nil;
  try
    LGraph := MakeGraph(LModel, 1024, False);
    SetLength(LMasks, 1024);
    for I := 0 to High(LMasks) do
      LMasks[I] := MakeWfcSequenceTokenConstraint(I, [LTokens[I mod Length(LTokens)]]);
    GObserver.Reset;
    {$ifndef PAS2JS} LStart := GetTickCount64; {$endif}
    IntersectSequenceTokenConstraints(LModel, LGraph, LMasks, GObserver.Observe);
    {$ifndef PAS2JS}
    WriteLn('Authored 256-state/1024-cell bulk intersection ms=', GetTickCount64 - LStart);
    {$endif}
    for LPhase := Low(LPhase) to High(LPhase) do
      Check(GObserver.Completions[LPhase] = 1, 'Large bulk repeated or missed a whole-phase completion');
    Check(GObserver.Last[wspValidateGraphModel] = 256, 'Model-state row count differs');
    Check((GObserver.Last[wspSnapshotDomains] = 1024) and
      (GObserver.Last[wspIntersectDomains] = 1024), 'Large cell/mask counts differ');
    Check(LGraph.TrySolve(DefaultGraphSolveOptions, LReport), 'Large constrained graph did not solve');
    Check(CaptureSolvedSequence(LModel, LGraph, LGenerated, LValidation), 'Large state path failed validation');
    for I := 0 to High(LGenerated.Tokens) do
      Check(LGenerated.Tokens[I] = LTokens[I mod Length(LTokens)], 'Large public-token replay differs');
  finally
    LGraph.Free;
    LModel.Free;
  end;
end;
begin
  GObserver := TObserver.Create;
  try
    CheckMalformedProgress;
    RunControls;
    RunLarge;
    WriteLn('Sequence preparation: ', GChecks, ' checks; ', GAborts,
      ' original interior/final aborts; ', GObserver.Calls, ' callbacks');
  finally
    GObserver.Free;
  end;
end.
