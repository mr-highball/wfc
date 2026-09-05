(*
MIT License

Copyright (c) 2021 mr-highball

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
program wfc_trace_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_trace;

const
  EXPECTED_DEPENDENCY_TRACE_HASH =
    TGraphTraceSignature($46715F2C);

var
  GChecks: Integer = 0;
  GFailures: Integer = 0;
  GGoldenHash: TGraphTraceSignature = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GChecks);
  if ACondition then
    Exit;
  Inc(GFailures);
  WriteLn('FAIL: ', AMessage);
end;

function NewChoiceFixture(const ASeed: TGraphSeed): TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := ASeed;
  Result.Reshape(4, 1, 1);
  Result.WrapNeighbors := False;
  Result.CurrentPass := 'choices';
  Result.AddValue('A');
  Result.AddValue('B');
  Result.AddValue('C');
end;

function NewEscapeRing: TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := 0;
  Result.Reshape(3, 1, 1);
  Result.AddValue('A').NewRule([gdEast, gdWest], 'B');
  Result.AddValue('C').NewRule([gdEast, gdWest], 'C');
end;

function NewDependencyFixture(const ASeed: TGraphSeed): TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := ASeed;
  Result.Reshape(1, 1, 1);
  Result.WrapNeighbors := False;
  Result.CurrentPass := 'terrain';
  Result.AddValue('land');
  Result.SwitchToPass('settlement');
  Result.PassMode := gpmOverlay;
  Result.AddValue('none');
  Result.AddValue('home').RequireFromPass('terrain', 'water');
end;

function CountKind(const AReport: TGraphSolveReport;
  const AKind: TGraphTraceEventKind): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(AReport.Trace) do
    if AReport.Trace[I].Kind = AKind then
      Inc(Result);
end;

function FindEvent(const AReport: TGraphSolveReport;
  const AKind: TGraphTraceEventKind; const APassIndex,
  AEntryIndex, AValueIndex: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to High(AReport.Trace) do
    if (AReport.Trace[I].Kind = AKind)
      and ((APassIndex < -1) or
        (AReport.Trace[I].PassIndex = APassIndex))
      and ((AEntryIndex < -1) or
        (AReport.Trace[I].EntryIndex = AEntryIndex))
      and ((AValueIndex < -1) or
        (AReport.Trace[I].ValueIndex = AValueIndex)) then
      Exit(I);
  Result := -1;
end;

function SameTraceEvent(const A, B: TGraphTraceEvent): Boolean;
begin
  Result := (A.EventId = B.EventId)
    and (A.CauseEventId = B.CauseEventId)
    and (A.Kind = B.Kind)
    and (A.CauseKind = B.CauseKind)
    and (A.PassIndex = B.PassIndex)
    and (A.EntryIndex = B.EntryIndex)
    and (A.ValueIndex = B.ValueIndex)
    and (A.Value = B.Value)
    and (A.NeighborIndex = B.NeighborIndex)
    and (A.HasDirection = B.HasDirection)
    and (A.Direction = B.Direction)
    and (A.DependencyPassIndex = B.DependencyPassIndex)
    and (A.DecisionDepth = B.DecisionDepth)
    and (A.DomainCountBefore = B.DomainCountBefore)
    and (A.DomainCountAfter = B.DomainCountAfter);
end;

function SameTrace(const A, B: TGraphSolveReport): Boolean;
var
  I: Integer;
begin
  if (A.TraceCaptured <> B.TraceCaptured)
    or (A.TraceHash <> B.TraceHash)
    or (Length(A.Trace) <> Length(B.Trace)) then
    Exit(False);
  for I := 0 to High(A.Trace) do
    if not SameTraceEvent(A.Trace[I], B.Trace[I]) then
      Exit(False);
  Result := True;
end;

procedure CheckTraceStructure(const AGraph: TGraph;
  const AReport: TGraphSolveReport; const AContext: String);
var
  I, J: Integer;
  LValidation: TGraphTraceValidationReport;
begin
  Check(AReport.TraceCaptured, AContext + ': capture flag');
  Check(AReport.TraceHash = CalculateGraphTraceHash(AReport),
    AContext + ': signature recomputes');
  Check(ValidateGraphTrace(AGraph, AReport, LValidation),
    AContext + ': public utility validates the captured trace');
  for I := 0 to High(AReport.Trace) do
  begin
    Check(AReport.Trace[I].EventId = I,
      AContext + ': event IDs are chronological');
    Check((AReport.Trace[I].CauseEventId = -1)
      or ((AReport.Trace[I].CauseEventId >= 0)
        and (AReport.Trace[I].CauseEventId < I)),
      AContext + ': causes refer backward');
    if (AReport.Trace[I].Kind = gtekDecision)
      and (AReport.Trace[I].CauseEventId >= 0) then
      Check(AReport.Trace[I].CauseKind =
        AReport.Trace[AReport.Trace[I].CauseEventId].CauseKind,
        AContext + ': decisions inherit their public cause kind');
    Check(AReport.Trace[I].DecisionDepth >= 0,
      AContext + ': public decision depth is normalized');
    Check((AReport.Trace[I].DomainCountBefore >= 0)
      and (AReport.Trace[I].DomainCountAfter >= 0),
      AContext + ': public domain counts are normalized');
  end;
  for I := 0 to High(AReport.Passes) do
  begin
    if AReport.Passes[I].TraceCount = 0 then
      Check(AReport.Passes[I].TraceStart = -1,
        AContext + ': empty pass slice sentinel')
    else
    begin
      Check((AReport.Passes[I].TraceStart >= 0)
        and (AReport.Passes[I].TraceStart
          + AReport.Passes[I].TraceCount <= Length(AReport.Trace)),
        AContext + ': pass slice bounds');
      for J := AReport.Passes[I].TraceStart to
        AReport.Passes[I].TraceStart
          + Pred(AReport.Passes[I].TraceCount) do
        Check(AReport.Trace[J].PassIndex = I,
          AContext + ': pass slice ownership');
    end;
  end;
end;

procedure TestOptInAndReplay;
var
  I: Integer;
  LDisabled, LEnabled: TGraph;
  LDisabledOptions, LEnabledOptions: TGraphSolveOptions;
  LDisabledReport, LEnabledReport: TGraphSolveReport;
begin
  LDisabledOptions := DefaultGraphSolveOptions;
  Check(not LDisabledOptions.CaptureTrace,
    'trace capture is disabled by default');
  LEnabledOptions := LDisabledOptions;
  LEnabledOptions.CaptureTrace := True;
  LDisabled := NewChoiceFixture(17);
  LEnabled := NewChoiceFixture(17);
  try
    Check(LDisabled.TrySolve(LDisabledOptions, LDisabledReport),
      'disabled trace fixture solves');
    Check(LEnabled.TrySolve(LEnabledOptions, LEnabledReport),
      'enabled trace fixture solves');
    for I := 0 to 3 do
      Check(LDisabled.Entry[I, 0, 0].Value
        = LEnabled.Entry[I, 0, 0].Value,
        'capture does not alter assignments');
    Check((LDisabledReport.Passes[0].Decisions
        = LEnabledReport.Passes[0].Decisions)
      and (LDisabledReport.Passes[0].Propagations
        = LEnabledReport.Passes[0].Propagations)
      and (LDisabledReport.Passes[0].Contradictions
        = LEnabledReport.Passes[0].Contradictions)
      and (LDisabledReport.Passes[0].Backtracks
        = LEnabledReport.Passes[0].Backtracks),
      'capture does not alter counters');
    Check(LDisabled.RandomIndex(1000) = LEnabled.RandomIndex(1000),
      'capture does not alter the random stream');
    Check((not LDisabledReport.TraceCaptured)
      and (Length(LDisabledReport.Trace) = 0)
      and (LDisabledReport.TraceHash = 0)
      and (LDisabledReport.Passes[0].TraceStart = -1)
      and (LDisabledReport.Passes[0].TraceCount = 0),
      'disabled capture returns an empty trace surface');
    CheckTraceStructure(LEnabled, LEnabledReport, 'enabled replay');
    Check((LEnabledReport.Trace[0].Kind = gtekPassBegin)
      and (LEnabledReport.Trace[High(LEnabledReport.Trace)].Kind
        = gtekPipelineCommit),
      'successful capture brackets work with begin and commit events');
  finally
    LEnabled.Free;
    LDisabled.Free;
  end;
end;

procedure TestDeterminismAndHash;
var
  LFirst, LSecond, LDifferent: TGraph;
  LOptions: TGraphSolveOptions;
  LFirstReport, LSecondReport, LDifferentReport: TGraphSolveReport;
  LTampered: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;
  LOptions.CaptureTrace := True;
  LFirst := NewChoiceFixture(99);
  LSecond := NewChoiceFixture(99);
  LDifferent := NewChoiceFixture(100);
  try
    Check(LFirst.TrySolve(LOptions, LFirstReport)
      and LSecond.TrySolve(LOptions, LSecondReport)
      and LDifferent.TrySolve(LOptions, LDifferentReport),
      'trace replay fixtures solve');
    Check(SameTrace(LFirstReport, LSecondReport),
      'same model and seed reproduce every event');
    Check(LFirstReport.TraceHash <> LDifferentReport.TraceHash,
      'the seed is part of trace identity');
    LTampered := LFirstReport;
    LTampered.Trace := Copy(LFirstReport.Trace);
    LTampered.Trace[1].EntryIndex :=
      Succ(LTampered.Trace[1].EntryIndex);
    Check(CalculateGraphTraceHash(LTampered)
      <> LFirstReport.TraceHash,
      'numeric event tampering changes the signature');
  finally
    LDifferent.Free;
    LSecond.Free;
    LFirst.Free;
  end;
end;

procedure TestInitialCauses;
var
  I: Integer;
  LDomain, LLock: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;
  LOptions.CaptureTrace := True;
  LDomain := TGraph.Create;
  try
    LDomain.Reshape(1, 1, 1);
    LDomain.WrapNeighbors := False;
    LDomain.AddValue('A');
    LDomain.AddValue('B');
    LDomain.AddValue('C');
    LDomain.SetAllowedValues(0, 0, 0, 'A');
    Check(LDomain.TrySolve(LOptions, LReport),
      'caller-domain trace fixture solves');
    Check(CountKind(LReport, gtekInitialCandidateRemoved) = 2,
      'caller domain records both removed candidates');
    for I := 0 to High(LReport.Trace) do
      if LReport.Trace[I].Kind = gtekInitialCandidateRemoved then
        Check((LReport.Trace[I].CauseKind = gtckCallerDomain)
            and (LReport.Trace[I].CauseEventId = -1),
          'caller-domain removal has an exact external cause');
  finally
    LDomain.Free;
  end;

  LLock := TGraph.Create;
  try
    LLock.Reshape(1, 1, 1);
    LLock.WrapNeighbors := False;
    LLock.AddValue('A');
    LLock.AddValue('B');
    LLock.AddValue('C');
    LLock.Entry[0, 0, 0].Value := 'A';
    Check(LLock.TrySolve(LOptions, LReport),
      'caller-lock trace fixture solves');
    Check(CountKind(LReport, gtekInitialCandidateRemoved) = 2,
      'caller lock records both removed candidates');
    for I := 0 to High(LReport.Trace) do
      if LReport.Trace[I].Kind = gtekInitialCandidateRemoved then
        Check((LReport.Trace[I].CauseKind = gtckCallerLock)
            and (LReport.Trace[I].CauseEventId = -1),
          'caller-lock removal has an exact external cause');
  finally
    LLock.Free;
  end;
end;

procedure TestPassDependencyCause;
var
  LCauseIndex, LRemovalIndex: Integer;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;
  LOptions.CaptureTrace := True;
  LGraph := NewDependencyFixture(42);
  try
    Check(LGraph.TrySolve(LOptions, LReport),
      'cross-pass trace fixture solves');
    CheckTraceStructure(LGraph, LReport, 'cross-pass');
    LRemovalIndex := FindEvent(LReport,
      gtekInitialCandidateRemoved, 1, 0, 1);
    Check(LRemovalIndex >= 0,
      'the unavailable home candidate is recorded');
    if LRemovalIndex >= 0 then
    begin
      Check((LReport.Trace[LRemovalIndex].CauseKind
          = gtckPassDependency)
        and (LReport.Trace[LRemovalIndex].DependencyPassIndex = 0),
        'downstream removal names its provider pass');
      LCauseIndex := LReport.Trace[LRemovalIndex].CauseEventId;
      Check((LCauseIndex >= 0)
        and (LReport.Trace[LCauseIndex].PassIndex = 0)
        and (LReport.Trace[LCauseIndex].Kind = gtekPassStaged),
        'downstream removal links to the provider staging event');
    end;
    GGoldenHash := LReport.TraceHash;
    Check(GGoldenHash = EXPECTED_DEPENDENCY_TRACE_HASH,
      'the dependency trace has a native/pas2js golden signature');
  finally
    LGraph.Free;
  end;
end;

procedure TestFailedPassDependencyCause;
var
  LContradictionIndex: Integer;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LRemovalIndex: Integer;
  LReport: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;
  LOptions.CaptureTrace := True;
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('land');
    LGraph.SwitchToPass('settlement');
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('home').RequireFromPass('terrain', 'water');
    Check(not LGraph.TrySolve(LOptions, LReport),
      'failed cross-pass trace fixture rejects its only candidate');
    CheckTraceStructure(LGraph, LReport, 'failed-cross-pass');
    LRemovalIndex := FindEvent(LReport,
      gtekInitialCandidateRemoved, 1, 0, 0);
    LContradictionIndex := FindEvent(LReport,
      gtekContradiction, 1, 0, -2);
    Check((LRemovalIndex >= 0) and (LContradictionIndex >= 0),
      'failed cross-pass trace records removal and contradiction');
    if (LRemovalIndex >= 0) and (LContradictionIndex >= 0) then
      Check((LReport.Trace[LContradictionIndex].CauseEventId =
          LRemovalIndex)
        and (LReport.Trace[LContradictionIndex].CauseKind =
          gtckPassDependency)
        and (LReport.Trace[LContradictionIndex].DependencyPassIndex = 0),
        'initial contradiction retains its refined provider cause');
  finally
    LGraph.Free;
  end;
end;

procedure TestPropagationAndRecovery;
var
  I: Integer;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRetryDecisions: Integer;
begin
  LOptions := DefaultGraphSolveOptions;
  LOptions.CaptureTrace := True;
  LGraph := TGraph.Create;
  try
    LGraph.Seed := 0;
    LGraph.Reshape(3, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A').NewRule([gdEast, gdWest], 'A');
    LGraph.AddValue('B').NewRule([gdEast, gdWest], 'B');
    LGraph.Entry[0, 0, 0].Value := 'A';
    Check(LGraph.TrySolve(LOptions, LReport),
      'propagation trace fixture solves');
    Check(CountKind(LReport, gtekCandidateRemoved) >= 2,
      'fixed-point propagation records candidate removals');
    for I := 0 to High(LReport.Trace) do
      if (LReport.Trace[I].Kind = gtekCandidateRemoved)
        and (LReport.Trace[I].CauseKind = gtckAdjacency) then
        Check(LReport.Trace[I].CauseEventId >= 0,
          'adjacency removal links to a prior domain change');
  finally
    LGraph.Free;
  end;

  LOptions.MaxBacktracks := 1;
  LGraph := NewEscapeRing;
  try
    Check(LGraph.TrySolve(LOptions, LReport),
      'recovery trace fixture solves');
    CheckTraceStructure(LGraph, LReport, 'recovery');
    Check((CountKind(LReport, gtekContradiction) = 1)
      and (CountKind(LReport, gtekBacktrack) = 1)
      and (CountKind(LReport, gtekCandidateRestored) > 0),
      'failed branch, backtrack, and restoration remain observable');
    LRetryDecisions := 0;
    for I := 0 to High(LReport.Trace) do
      if (LReport.Trace[I].Kind = gtekDecision)
          and (LReport.Trace[I].CauseKind = gtckBacktrack) then
      begin
        Inc(LRetryDecisions);
        Check((LReport.Trace[I].CauseEventId >= 0)
            and (LReport.Trace[
              LReport.Trace[I].CauseEventId].Kind = gtekBacktrack),
          'retry decision links to the backtrack that enabled it');
      end;
    Check(LRetryDecisions = 1,
      'recovery trace contains one backtrack-caused retry decision');
    Check(LReport.Trace[High(LReport.Trace)].Kind
      = gtekPipelineCommit,
      'successful recovery ends in a commit');
  finally
    LGraph.Free;
  end;

  LOptions.MaxBacktracks := 0;
  LGraph := NewEscapeRing;
  try
    Check(not LGraph.TrySolve(LOptions, LReport),
      'limited recovery trace fixture fails');
    CheckTraceStructure(LGraph, LReport, 'rollback');
    Check((LReport.Status = gssBacktrackLimit)
      and (CountKind(LReport, gtekPassFailed) = 1)
      and (LReport.Trace[High(LReport.Trace)].Kind
        = gtekPipelineRollback),
      'failed transaction records pass failure and rollback');
    for I := 0 to 2 do
      Check(LGraph.Entry[I, 0, 0].Empty,
        'trace capture preserves rollback atomicity');
  finally
    LGraph.Free;
  end;
end;

procedure TestSelectiveTrace;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := TGraph.Create;
  try
    LGraph.Seed := 5;
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('land');
    LGraph.AddValue('water');
    LGraph.SwitchToPass('foliage');
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('tree').RequireFromPass('terrain', 'land');
    LGraph.AddValue('bare');
    Check(LGraph.TrySolve(LOptions, LReport),
      'selective trace fixture establishes output');
    LOptions.CaptureTrace := True;
    Check(LGraph.TryRegenerateFrom('foliage', LOptions, LReport),
      'selective trace fixture regenerates its leaf');
    CheckTraceStructure(LGraph, LReport, 'selective');
    Check((LReport.Passes[0].Disposition = gpdReused)
      and (LReport.Passes[0].TraceCount = 1)
      and (LReport.Trace[LReport.Passes[0].TraceStart].Kind
        = gtekPassSkipped),
      'selective trace explicitly records the reused provider');
    Check(LReport.Passes[1].Executed
      and (LReport.Passes[1].TraceCount > 1)
      and (LReport.Trace[LReport.Passes[1].TraceStart].Kind
        = gtekPassBegin),
      'selective trace brackets the dirty consumer');
  finally
    LGraph.Free;
  end;
end;

procedure TestZeroCellTrace;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;
  LOptions.CaptureTrace := True;
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(0, 1, High(TGraphCoordinate));
    LGraph.AddValue('A');
    Check(LGraph.TrySolve(LOptions, LReport),
      'zero-cell trace fixture solves');
    CheckTraceStructure(LGraph, LReport, 'zero-cell');
    Check((LReport.Status = gssSolved)
      and (Length(LReport.Trace) = 3)
      and (LReport.Passes[0].TraceCount = 2)
      and (LReport.Trace[0].Kind = gtekPassBegin)
      and (LReport.Trace[1].Kind = gtekPassStaged)
      and (LReport.Trace[2].Kind = gtekPipelineCommit),
      'zero-cell trace contains only its transaction lifecycle');
  finally
    LGraph.Free;
  end;
end;

procedure TestDefinitionlessCopyTrace;
var
  LCauseIndex: Integer;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;
  LOptions.CaptureTrace := True;
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.CurrentPass := 'source';
    LGraph.AddValue('A');
    LGraph.SwitchToPass('copy');
    Check(LGraph.TrySolve(LOptions, LReport),
      'definitionless copy trace fixture solves');
    CheckTraceStructure(LGraph, LReport, 'definitionless-copy');
    Check((LReport.Passes[1].Disposition = gpdCopied)
      and (LReport.Trace[LReport.Passes[1].TraceStart + 1].Kind
        = gtekPassStaged),
      'definitionless copy is represented as staged copied output');
    LCauseIndex := LReport.Trace[
      LReport.Passes[1].TraceStart + 1].CauseEventId;
    Check((LCauseIndex >= 0)
      and (LReport.Trace[LCauseIndex].PassIndex = 0)
      and (LReport.Trace[LCauseIndex].Kind = gtekPassStaged),
      'definitionless copy staging links to its provider staging event');
  finally
    LGraph.Free;
  end;
end;

procedure TestDefinitionlessFailureTrace;
var
  LContradictionIndex: Integer;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LProviderEventIndex: Integer;
  LReport: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;
  LOptions.CaptureTrace := True;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.CurrentPass := 'source';
    LGraph.AddValue('A');
    LGraph.SwitchToPass('copy');
    LGraph.SetAllowedValues(0, 0, 0, []);
    Check(not LGraph.TrySolve(LOptions, LReport),
      'definitionless destination rejects an ineligible source value');
    CheckTraceStructure(LGraph, LReport,
      'definitionless-source-failure');
    LContradictionIndex := FindEvent(LReport,
      gtekContradiction, 1, 0, -2);
    Check(LContradictionIndex >= 0,
      'source-derived definitionless failure records a contradiction');
    if LContradictionIndex >= 0 then
    begin
      LProviderEventIndex :=
        LReport.Trace[LContradictionIndex].CauseEventId;
      Check((LReport.Trace[LContradictionIndex].CauseKind =
          gtckPassDependency)
        and (LReport.Trace[LContradictionIndex].DependencyPassIndex = 0)
        and (LProviderEventIndex >= 0)
        and (LReport.Trace[LProviderEventIndex].PassIndex = 0)
        and (LReport.Trace[LProviderEventIndex].Kind = gtekPassStaged),
        'source-derived failure links to provider staging');
    end;
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.CurrentPass := 'source';
    LGraph.AddValue('A');
    LGraph.SwitchToPass('copy');
    LGraph.Entry[0, 0, 0].Value := 'B';
    LGraph.SetAllowedValues(0, 0, 0, []);
    Check(not LGraph.TrySolve(LOptions, LReport),
      'definitionless destination rejects its own caller override');
    CheckTraceStructure(LGraph, LReport,
      'definitionless-caller-failure');
    LContradictionIndex := FindEvent(LReport,
      gtekContradiction, 1, 0, -2);
    Check(LContradictionIndex >= 0,
      'caller-owned definitionless failure records a contradiction');
    if LContradictionIndex >= 0 then
      Check((LReport.Trace[LContradictionIndex].CauseKind =
          gtckCallerDomain)
        and (LReport.Trace[LContradictionIndex].CauseEventId = -1)
        and (LReport.Trace[LContradictionIndex].DependencyPassIndex = -1),
        'caller-owned failure remains an external causal root');
  finally
    LGraph.Free;
  end;
end;

begin
  Check(WFC_TRACE_VERSION = 1,
    'the causal trace schema is explicitly versioned');
  Check(WFC_TRACE_HASH_VERSION = 1,
    'the causal trace hash is explicitly versioned');
  TestOptInAndReplay;
  TestDeterminismAndHash;
  TestInitialCauses;
  TestPassDependencyCause;
  TestFailedPassDependencyCause;
  TestPropagationAndRecovery;
  TestSelectiveTrace;
  TestZeroCellTrace;
  TestDefinitionlessCopyTrace;
  TestDefinitionlessFailureTrace;
  WriteLn('TRACE_HASH=', IntToHex(GGoldenHash, 8));
  WriteLn('checks=', GChecks, ' failures=', GFailures);
  if GFailures <> 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d public trace checks failed',
      [GFailures]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
