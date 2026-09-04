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
program wfc_negotiation_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_trace;

const
  EXPECTED_ONE_CELL_BASELINE_TRACE =
    TGraphTraceSignature($42AF302E);
  EXPECTED_ONE_CELL_ORACLE_TRACE =
    TGraphTraceSignature($AE6B5907);
  EXPECTED_TWO_CELL_BASELINE_TRACE =
    TGraphTraceSignature($F63B2846);
  EXPECTED_TWO_CELL_ORACLE_TRACE =
    TGraphTraceSignature($0F79873D);
  EXPECTED_ONE_CELL_TRANSCRIPT =
    TGraphTraceSignature($6F76591D);
  EXPECTED_TWO_CELL_TRANSCRIPT =
    TGraphTraceSignature($04CCD398);
  EXPECTED_PROVIDER_EXHAUSTION_TRANSCRIPT =
    TGraphTraceSignature($11F3B018);
  EXPECTED_JOIN_EXHAUSTION_TRANSCRIPT =
    TGraphTraceSignature($41AF694A);

type
  TTestProcedure = procedure;

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GCheckCount);
  if ACondition then
    WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailureCount);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try
    ATest;
  except
    on E: Exception do
    begin
      Inc(GFailureCount);
      WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function CapturedSolveOptions(const AMaxBacktracks: Integer):
  TGraphSolveOptions;
begin
  Result := DefaultGraphSolveOptions;
  Result.MaxBacktracks := AMaxBacktracks;
  Result.CaptureTrace := True;
end;

function NegotiationOptions(const AMaxPassBacktracks,
  AMaxSolverBacktracks: Integer): TGraphNegotiationOptions;
begin
  Result := DefaultGraphNegotiationOptions;
  Result.MaxPassBacktracks := AMaxPassBacktracks;
  Result.SolveOptions := CapturedSolveOptions(AMaxSolverBacktracks);
end;

function NewOneCellFixture: TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Seed := 0;
    Result.Reshape(1, 1, 1);
    Result.WrapNeighbors := False;

    Result.CurrentPass := 'terrain';
    Result.PassMode := gpmOverlay;
    Result.AddValue('marsh');
    Result.AddValue('meadow');

    Result.SwitchToPass('housing');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('cottage').RequireFromPass('terrain', 'meadow');
  except
    Result.Free;
    raise;
  end;
end;

function NewTwoCellFixture: TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Seed := 0;
    Result.Reshape(2, 1, 1);
    Result.WrapNeighbors := False;

    Result.CurrentPass := 'terrain';
    Result.PassMode := gpmOverlay;
    Result.AddValue('meadow');
    Result.AddValue('marsh');

    Result.SwitchToPass('housing');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('cottage').RequireFromPass('terrain', 'meadow');
  except
    Result.Free;
    raise;
  end;
end;

function NewProviderExhaustionFixture: TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Seed := 0;
    Result.Reshape(1, 1, 1);
    Result.WrapNeighbors := False;

    Result.CurrentPass := 'provider';
    Result.PassMode := gpmOverlay;
    Result.AddValue('P1');
    Result.AddValue('P2');

    Result.SwitchToPass('consumer');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    //The bounded +X coordinate is absent, so both provider assignments lead
    //to the same downstream contradiction and must be exhausted exactly.
    Result.AddValue('C').RequireFromPassAt('provider',
      MakeGraphOffset(1, 0, 0), 'P1');
  except
    Result.Free;
    raise;
  end;
end;

function NewJoinExhaustionFixture: TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Seed := 0;
    Result.Reshape(1, 1, 1);
    Result.WrapNeighbors := False;

    Result.CurrentPass := 'A';
    Result.PassMode := gpmOverlay;
    Result.AddValue('A1');
    Result.AddValue('A2');

    Result.SwitchToPass('B');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('B2');
    //Pass B's derived seed-zero stream selects its second registered value,
    //so this order makes the named chronological path B1 then B2.
    Result.AddValue('B1');

    Result.SwitchToPass('C');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('C')
      .RequireFromPass('A', 'A2')
      .RequireFromPass('B', 'B1');
  except
    Result.Free;
    raise;
  end;
end;

function EntryState(const AEntry: TGraphEntry): String;
begin
  if AEntry.Empty then
    Result := '-'
  else if AEntry.Generated then
    Result := '+' + AEntry.Value
  else
    Result := '!' + AEntry.Value;
end;

function GraphState(const AGraph: TGraph): String;
var
  I, J: Integer;
begin
  Result := IntToStr(AGraph.CurrentPassIndex) + ':';
  for I := 0 to Pred(AGraph.TotalPassCount) do
  begin
    if I > 0 then
      Result := Result + '|';
    for J := 0 to Integer(AGraph.Dimension.Width) - 1 do
    begin
      if J > 0 then
        Result := Result + ',';
      Result := Result + EntryState(
        AGraph.PassGraph[I].Entry[J, 0, 0]);
    end;
  end;
end;

function IndicesEqual(const AValues: TGraphValueIndices;
  const AExpected: array of Integer): Boolean;
var
  I: Integer;
begin
  if Length(AValues) <> Length(AExpected) then
    Exit(False);
  for I := 0 to High(AValues) do
    if AValues[I] <> AExpected[I] then
      Exit(False);
  Result := True;
end;

function ExecutionOrderEquals(const AReport: TGraphSolveReport;
  const AExpected: array of Integer): Boolean;
var
  I: Integer;
begin
  if Length(AReport.ExecutionOrder) <> Length(AExpected) then
    Exit(False);
  for I := 0 to High(AReport.ExecutionOrder) do
    if AReport.ExecutionOrder[I] <> AExpected[I] then
      Exit(False);
  Result := True;
end;

function PassCountersEqual(const AReport: TGraphSolveReport;
  const APass, ADecisions, APropagations, AContradictions,
  ABacktracks: Integer): Boolean;
begin
  Result := (APass >= 0) and (APass < Length(AReport.Passes));
  if not Result then
    Exit;
  Result := (AReport.Passes[APass].Decisions = ADecisions)
    and (AReport.Passes[APass].Propagations = APropagations)
    and (AReport.Passes[APass].Contradictions = AContradictions)
    and (AReport.Passes[APass].Backtracks = ABacktracks);
end;

function FailureIsHousingDependency(
  const AReport: TGraphSolveReport; const AEntry: Integer): Boolean;
begin
  Result := (AReport.Status = gssContradiction)
    and (AReport.FailedPassIndex = 1)
    and (AReport.Contradiction.Kind = gckPassDependency)
    and (AReport.Contradiction.PassIndex = 1)
    and (AReport.Contradiction.EntryIndex = AEntry)
    and (AReport.Contradiction.DependencyPassIndex = 0);
end;

function TraceKindsEqual(const AReport: TGraphSolveReport;
  const AExpected: array of TGraphTraceEventKind): Boolean;
var
  I: Integer;
begin
  if Length(AReport.Trace) <> Length(AExpected) then
    Exit(False);
  for I := 0 to High(AReport.Trace) do
    if AReport.Trace[I].Kind <> AExpected[I] then
      Exit(False);
  Result := True;
end;

function SolveFingerprint(const AReport: TGraphSolveReport): String;
var
  I: Integer;
begin
  Result := IntToStr(Ord(AReport.Status)) + ':'
    + IntToStr(AReport.FailedPassIndex) + ':'
    + IntToStr(Ord(AReport.Contradiction.Kind)) + ':'
    + IntToStr(AReport.Contradiction.PassIndex) + ':'
    + IntToStr(AReport.Contradiction.EntryIndex) + ':'
    + IntToStr(AReport.Contradiction.DependencyPassIndex) + ':'
    + GraphTraceSignatureHex(AReport.TraceHash);
  for I := 0 to High(AReport.ExecutionOrder) do
    Result := Result + ':e' + IntToStr(AReport.ExecutionOrder[I]);
  for I := 0 to High(AReport.Passes) do
    Result := Result + ':p' + IntToStr(I) + '='
      + IntToStr(AReport.Passes[I].Decisions) + ','
      + IntToStr(AReport.Passes[I].Propagations) + ','
      + IntToStr(AReport.Passes[I].Contradictions) + ','
      + IntToStr(AReport.Passes[I].Backtracks) + ','
      + IntToStr(AReport.Passes[I].ExcludedAssignments) + ','
      + IntToStr(Ord(AReport.Passes[I].Disposition));
end;

function NegotiationFingerprint(
  const AReport: TGraphNegotiationReport): String;
var
  I, J: Integer;
begin
  Result := IntToStr(Ord(AReport.Status)) + ':'
    + IntToStr(AReport.PassBacktracks) + ':'
    + GraphTraceSignatureHex(AReport.TranscriptHash);
  for I := 0 to High(AReport.Attempts) do
  begin
    Result := Result + ':a' + IntToStr(I) + '='
      + IntToStr(AReport.Attempts[I].BacktrackedPassIndex) + ','
      + IntToStr(AReport.Attempts[I].BacktrackedExecutionOrdinal) + '[';
    for J := 0 to High(AReport.Attempts[I].ExcludedAssignment) do
      Result := Result
        + IntToStr(AReport.Attempts[I].ExcludedAssignment[J]) + ';';
    Result := Result + ']' +
      SolveFingerprint(AReport.Attempts[I].SolveReport);
  end;
  Result := Result + ':final=' + SolveFingerprint(AReport.FinalReport);
end;

procedure CheckTraceValid(const AGraph: TGraph;
  const AReport: TGraphSolveReport; const AContext: String);
var
  LValidation: TGraphTraceValidationReport;
begin
  Check(AReport.TraceCaptured, AContext + ' captures a trace');
  Check(AReport.TraceHash = CalculateGraphTraceHash(AReport),
    AContext + ' trace hash recomputes');
  Check(ValidateGraphTrace(AGraph, AReport, LValidation),
    AContext + ' trace validates independently');
end;

procedure CheckOneCellOutput(const AGraph: TGraph;
  const AContext: String);
begin
  Check((AGraph.PassGraph[0].Entry[0, 0, 0].Value = 'meadow')
    and AGraph.PassGraph[0].Entry[0, 0, 0].Generated
    and (AGraph.PassGraph[1].Entry[0, 0, 0].Value = 'cottage')
    and AGraph.PassGraph[1].Entry[0, 0, 0].Generated,
    AContext + ' commits meadow and cottage');
end;

procedure TestPublicDefaults;
var
  LOptions: TGraphNegotiationOptions;
begin
  LOptions := DefaultGraphNegotiationOptions;
  Check(WFC_PASS_NEGOTIATION_ALGORITHM_VERSION = 1,
    'pass negotiation algorithm version is one');
  Check(WFC_PASS_NEGOTIATION_HASH_VERSION = 1,
    'pass negotiation transcript hash version is one');
  Check(LOptions.MaxPassBacktracks = 64,
    'default negotiated solving has a finite pass-backtrack budget');
  Check(LOptions.SolveOptions.MaxBacktracks =
      DefaultGraphSolveOptions.MaxBacktracks,
    'default negotiation uses the reference solver backtrack budget');
  Check(not LOptions.SolveOptions.CaptureTrace,
    'default negotiation keeps trace capture opt-in');
end;

procedure TestOneWayBaselineAndOracle;
const
  FAILURE_KINDS: array[0..8] of TGraphTraceEventKind = (
    gtekPassBegin, gtekDecision, gtekCandidateRemoved,
    gtekPassStaged, gtekPassBegin, gtekInitialCandidateRemoved,
    gtekContradiction, gtekPassFailed, gtekPipelineRollback);
  ORACLE_KINDS: array[0..5] of TGraphTraceEventKind = (
    gtekPassBegin, gtekInitialCandidateRemoved, gtekPassStaged,
    gtekPassBegin, gtekPassStaged, gtekPipelineCommit);
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
begin
  LOptions := CapturedSolveOptions(1);
  LGraph := NewOneCellFixture;
  try
    Check(not LGraph.TrySolve(LOptions, LReport),
      'the current one-way coordinator cannot revisit marsh');
    Check(FailureIsHousingDependency(LReport, 0),
      'the one-way failure identifies housing, terrain, and entry zero');
    Check(ExecutionOrderEquals(LReport, [0, 1]),
      'the one-way failure follows terrain then housing');
    Check(PassCountersEqual(LReport, 0, 1, 0, 0, 0)
      and (LReport.Passes[0].Disposition = gpdSolved)
      and PassCountersEqual(LReport, 1, 0, 0, 1, 0)
      and (LReport.Passes[1].Disposition = gpdFailed),
      'the one-way failure has exact pass counters and dispositions');
    Check(GraphState(LGraph) = '1:-|-',
      'the failed one-way transaction commits neither pass');
    Check(TraceKindsEqual(LReport, FAILURE_KINDS),
      'the losing one-way trace retains the complete causal branch');
    Check(LReport.TraceHash = EXPECTED_ONE_CELL_BASELINE_TRACE,
      'the one-cell one-way trace matches its portable golden');
    CheckTraceValid(LGraph, LReport, 'one-way baseline');
  finally
    LGraph.Free;
  end;

  LGraph := NewOneCellFixture;
  try
    LGraph.SwitchToPass('terrain');
    LGraph.SetAllowedValues(0, 0, 0, 'meadow');
    LGraph.SwitchToPass('housing');
    Check(LGraph.TrySolve(LOptions, LReport),
      'a public caller domain proves the compatible composition exists');
    CheckOneCellOutput(LGraph, 'the satisfiability oracle');
    Check(PassCountersEqual(LReport, 0, 0, 0, 0, 0)
      and PassCountersEqual(LReport, 1, 0, 0, 0, 0),
      'the constrained oracle is propagation-only');
    Check(TraceKindsEqual(LReport, ORACLE_KINDS),
      'the oracle trace distinguishes caller pruning from negotiation');
    Check(LReport.TraceHash = EXPECTED_ONE_CELL_ORACLE_TRACE,
      'the one-cell oracle trace matches its portable golden');
    CheckTraceValid(LGraph, LReport, 'one-cell oracle');
  finally
    LGraph.Free;
  end;
end;

procedure TestOneCellNegotiation;
var
  LGraph: TGraph;
  LOriginalIndex: Integer;
  LOptions: TGraphNegotiationOptions;
  LReport: TGraphNegotiationReport;
begin
  LOptions := NegotiationOptions(1, 1);
  LGraph := NewOneCellFixture;
  try
    Check(LGraph.TrySolveNegotiated(LOptions, LReport)
      and (LReport.Status = gnsSolved),
      'one provider reopen solves the one-cell composition');
    Check((LReport.PassBacktracks = 1)
      and (Length(LReport.Attempts) = 1)
      and (LReport.Seed = 0)
      and (LReport.NegotiationAlgorithmVersion =
        WFC_PASS_NEGOTIATION_ALGORITHM_VERSION),
      'the successful report retains one rejected round');
    if Length(LReport.Attempts) = 1 then
    begin
      Check(FailureIsHousingDependency(
          LReport.Attempts[0].SolveReport, 0)
        and (LReport.Attempts[0].SolveReport.TraceHash =
          EXPECTED_ONE_CELL_BASELINE_TRACE),
        'the rejected round is the exact one-way housing failure');
      Check((LReport.Attempts[0].BacktrackedPassIndex = 0)
        and (LReport.Attempts[0].BacktrackedExecutionOrdinal = 0),
        'the rejected round reopens the chronologically latest provider');
      Check(IndicesEqual(LReport.Attempts[0].ExcludedAssignment,
          [0]),
        'the learned nogood owns the complete terrain value-index assignment');
    end;

    Check((LReport.FinalReport.Status = gssSolved)
      and (LReport.FinalReport.FailedPassIndex = -1)
      and ExecutionOrderEquals(LReport.FinalReport, [0, 1]),
      'the sole terminal round reports a complete solved pipeline');
    Check(PassCountersEqual(LReport.FinalReport, 0, 2, 0, 1, 1)
      and (LReport.FinalReport.Passes[0].ExcludedAssignments = 1)
      and PassCountersEqual(LReport.FinalReport, 1, 0, 0, 0, 0),
      'the reopened provider rejects marsh then takes meadow chronologically');
    CheckOneCellOutput(LGraph, 'negotiated generation');
    Check((not LGraph.PassGraph[0].HasAllowedValues(0, 0, 0))
      and (not LGraph.PassGraph[1].HasAllowedValues(0, 0, 0)),
      'learned exact assignments do not leak into caller domains');
    Check((LGraph.CurrentPassIndex = 1)
      and (LReport.TranscriptHash = EXPECTED_ONE_CELL_TRANSCRIPT)
      and (LReport.TranscriptHash =
        CalculateGraphNegotiationTranscriptHash(LOptions, LReport)),
      'negotiation restores selection and matches its transcript golden');
    LOriginalIndex := LReport.Attempts[0].ExcludedAssignment[0];
    LReport.Attempts[0].ExcludedAssignment[0] := 1;
    Check(CalculateGraphNegotiationTranscriptHash(LOptions, LReport)
        <> EXPECTED_ONE_CELL_TRANSCRIPT,
      'the transcript signature covers exact excluded assignments');
    LReport.Attempts[0].ExcludedAssignment[0] := LOriginalIndex;
    CheckTraceValid(LGraph, LReport.Attempts[0].SolveReport,
      'one-cell rejected round');
    CheckTraceValid(LGraph, LReport.FinalReport,
      'one-cell terminal round');
    WriteLn('  [INFO] one-cell transcript: ',
      GraphTraceSignatureHex(LReport.TranscriptHash));
  finally
    LGraph.Free;
  end;
end;

procedure TestTwoCellExactAssignment;
var
  LGraph: TGraph;
  LOptions: TGraphNegotiationOptions;
  LOracleOptions: TGraphSolveOptions;
  LOracleReport: TGraphSolveReport;
  LReport: TGraphNegotiationReport;
begin
  LOptions := NegotiationOptions(1, 1);
  LGraph := NewTwoCellFixture;
  try
    Check(LGraph.TrySolveNegotiated(LOptions, LReport)
      and (LReport.Status = gnsSolved),
      'one exact vector exclusion solves the two-cell composition');
    Check((LReport.PassBacktracks = 1)
      and (Length(LReport.Attempts) = 1),
      'the two-cell report retains one rejected round');
    if Length(LReport.Attempts) = 1 then
    begin
      Check(FailureIsHousingDependency(
          LReport.Attempts[0].SolveReport, 1)
        and (LReport.Attempts[0].SolveReport.TraceHash =
          EXPECTED_TWO_CELL_BASELINE_TRACE),
        'the rejected two-cell round fails on its second coordinate');
      Check(IndicesEqual(LReport.Attempts[0].ExcludedAssignment,
          [0, 1]),
        'the two-cell nogood retains the complete ordered value indices');
      Check((LReport.Attempts[0].BacktrackedPassIndex = 0)
        and (LReport.Attempts[0].BacktrackedExecutionOrdinal = 0),
        'the two-cell rejection identifies the provider execution');
    end;
    Check(PassCountersEqual(LReport.FinalReport, 0, 3, 0, 1, 1)
      and (LReport.FinalReport.Passes[0].ExcludedAssignments = 1)
      and PassCountersEqual(LReport.FinalReport, 1, 0, 0, 0, 0),
      'the reopened two-cell solve retries the vector and latest decision');
    Check((LGraph.PassGraph[0].Entry[0, 0, 0].Value = 'meadow')
      and (LGraph.PassGraph[0].Entry[1, 0, 0].Value = 'meadow')
      and (LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'cottage')
      and (LGraph.PassGraph[1].Entry[1, 0, 0].Value = 'cottage'),
      'the exact vector ban commits meadow/meadow and two cottages');
    Check((LReport.TranscriptHash = EXPECTED_TWO_CELL_TRANSCRIPT)
      and (LReport.TranscriptHash =
        CalculateGraphNegotiationTranscriptHash(LOptions, LReport)),
      'the two-cell negotiation matches its transcript golden');
    CheckTraceValid(LGraph, LReport.Attempts[0].SolveReport,
      'two-cell rejected round');
    CheckTraceValid(LGraph, LReport.FinalReport,
      'two-cell terminal round');
    WriteLn('  [INFO] two-cell transcript: ',
      GraphTraceSignatureHex(LReport.TranscriptHash));
  finally
    LGraph.Free;
  end;

  LOracleOptions := CapturedSolveOptions(1);
  LGraph := NewTwoCellFixture;
  try
    LGraph.SwitchToPass('terrain');
    LGraph.SetAllowedValues(0, 0, 0, 'meadow');
    LGraph.SetAllowedValues(1, 0, 0, 'meadow');
    LGraph.SwitchToPass('housing');
    Check(LGraph.TrySolve(LOracleOptions, LOracleReport),
      'the two-cell caller-domain oracle solves');
    Check(LOracleReport.TraceHash = EXPECTED_TWO_CELL_ORACLE_TRACE,
      'the two-cell oracle matches its portable golden trace');
    CheckTraceValid(LGraph, LOracleReport, 'two-cell oracle');
  finally
    LGraph.Free;
  end;
end;

procedure TestProviderAssignmentExhaustion;
var
  LBefore: String;
  LGraph, LReplay: TGraph;
  LOptions: TGraphNegotiationOptions;
  LReport, LReplayReport: TGraphNegotiationReport;
begin
  LOptions := NegotiationOptions(4, 8);
  LGraph := NewProviderExhaustionFixture;
  LReplay := NewProviderExhaustionFixture;
  try
    LBefore := GraphState(LGraph);
    Check(not LGraph.TrySolveNegotiated(LOptions, LReport),
      'an unsatisfiable consumer exhausts both provider assignments');
    Check((LReport.Status = gnsContradiction)
      and (LReport.PassBacktracks = 2)
      and (Length(LReport.Attempts) = 2),
      'provider exhaustion terminates after two exact exclusions');
    if Length(LReport.Attempts) = 2 then
    begin
      Check((LReport.Attempts[0].BacktrackedPassIndex = 0)
        and (LReport.Attempts[0].BacktrackedExecutionOrdinal = 0)
        and IndicesEqual(
          LReport.Attempts[0].ExcludedAssignment, [0]),
        'provider exhaustion first excludes P1');
      Check((LReport.Attempts[1].BacktrackedPassIndex = 0)
        and (LReport.Attempts[1].BacktrackedExecutionOrdinal = 0)
        and IndicesEqual(
          LReport.Attempts[1].ExcludedAssignment, [1]),
        'provider exhaustion then excludes P2');
      CheckTraceValid(LGraph,
        LReport.Attempts[0].SolveReport,
        'provider exhaustion P1 round');
      CheckTraceValid(LGraph,
        LReport.Attempts[1].SolveReport,
        'provider exhaustion P2 round');
    end;
    Check((LReport.FinalReport.Status = gssContradiction)
      and (LReport.FinalReport.FailedPassIndex = 0)
      and (LReport.FinalReport.Contradiction.Kind =
        gckExcludedAssignment)
      and (LReport.FinalReport.Passes[0].ExcludedAssignments = 2)
      and (not LReport.FinalReport.Passes[1].Executed),
      'terminal round reports that the provider search space is empty');
    Check(GraphState(LGraph) = LBefore,
      'complete provider exhaustion rolls back atomically');
    Check((LReport.TranscriptHash =
        EXPECTED_PROVIDER_EXHAUSTION_TRANSCRIPT)
      and (LReport.TranscriptHash =
        CalculateGraphNegotiationTranscriptHash(LOptions, LReport)),
      'provider exhaustion matches its transcript golden');
    CheckTraceValid(LGraph, LReport.FinalReport,
      'provider exhaustion terminal round');

    Check(not LReplay.TrySolveNegotiated(LOptions, LReplayReport),
      'provider exhaustion replays as a contradiction');
    Check(NegotiationFingerprint(LReplayReport) =
      NegotiationFingerprint(LReport),
      'provider exhaustion replays the exact ordered transcript');
    WriteLn('  [INFO] provider-exhaustion transcript: ',
      GraphTraceSignatureHex(LReport.TranscriptHash));
  finally
    LReplay.Free;
    LGraph.Free;
  end;
end;

procedure TestJoinBacktracksPastExhaustedProvider;
var
  LGraph, LReplay: TGraph;
  LOptions: TGraphNegotiationOptions;
  LReport, LReplayReport: TGraphNegotiationReport;
begin
  LOptions := NegotiationOptions(3, 8);
  LGraph := NewJoinExhaustionFixture;
  LReplay := NewJoinExhaustionFixture;
  try
    Check(LGraph.TrySolveNegotiated(LOptions, LReport),
      'the A/B join solves after backtracking past exhausted B');
    Check((LReport.Status = gnsSolved)
      and (LReport.PassBacktracks = 3)
      and (Length(LReport.Attempts) = 3),
      'the join retains three rejected rounds before its terminal solve');
    if Length(LReport.Attempts) = 3 then
    begin
      Check((LReport.Attempts[0].BacktrackedPassIndex = 1)
        and (LReport.Attempts[0].BacktrackedExecutionOrdinal = 1)
        and IndicesEqual(
          LReport.Attempts[0].ExcludedAssignment, [1]),
        'A1B1 rejects B1 in the A1 prefix');
      Check((LReport.Attempts[1].BacktrackedPassIndex = 1)
        and (LReport.Attempts[1].BacktrackedExecutionOrdinal = 1)
        and IndicesEqual(
          LReport.Attempts[1].ExcludedAssignment, [0]),
        'A1B2 rejects B2 in the same A1 prefix');
      Check((LReport.Attempts[2].BacktrackedPassIndex = 0)
        and (LReport.Attempts[2].BacktrackedExecutionOrdinal = 0)
        and IndicesEqual(
          LReport.Attempts[2].ExcludedAssignment, [0]),
        'exhausted B backtracks chronologically to exclude A1');
      Check((LReport.Attempts[1].SolveReport.Passes[1]
          .ExcludedAssignments = 1)
        and (LReport.Attempts[2].SolveReport.FailedPassIndex = 1)
        and (LReport.Attempts[2].SolveReport.Contradiction.Kind =
          gckExcludedAssignment)
        and (LReport.Attempts[2].SolveReport.Passes[1]
          .ExcludedAssignments = 2),
        'the rejected reports expose B1 then complete B exhaustion');
      CheckTraceValid(LGraph,
        LReport.Attempts[0].SolveReport, 'join A1B1 round');
      CheckTraceValid(LGraph,
        LReport.Attempts[1].SolveReport, 'join A1B2 round');
      CheckTraceValid(LGraph,
        LReport.Attempts[2].SolveReport,
        'join exhausted-B round');
    end;
    Check((LReport.FinalReport.Status = gssSolved)
      and (LReport.FinalReport.Passes[0].ExcludedAssignments = 1)
      and (LReport.FinalReport.Passes[1].ExcludedAssignments = 0),
      'changing A clears every B exclusion before the terminal round');
    Check((LGraph.PassGraph[0].Entry[0, 0, 0].Value = 'A2')
      and (LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'B1')
      and (LGraph.PassGraph[2].Entry[0, 0, 0].Value = 'C'),
      'the cleared B frame reaches the valid A2B1 join');
    Check((LReport.TranscriptHash =
        EXPECTED_JOIN_EXHAUSTION_TRANSCRIPT)
      and (LReport.TranscriptHash =
        CalculateGraphNegotiationTranscriptHash(LOptions, LReport)),
      'join matches its transcript golden');
    CheckTraceValid(LGraph, LReport.FinalReport,
      'join terminal round');

    Check(LReplay.TrySolveNegotiated(LOptions, LReplayReport),
      'the A/B join independently replays');
    Check(NegotiationFingerprint(LReplayReport) =
      NegotiationFingerprint(LReport),
      'the A/B join replays the exact B,B,A backtrack transcript');
    WriteLn('  [INFO] join-exhaustion transcript: ',
      GraphTraceSignatureHex(LReport.TranscriptHash));
  finally
    LReplay.Free;
    LGraph.Free;
  end;
end;

procedure TestPassBudgetAndRollback;
var
  I: Integer;
  LBefore: String;
  LGraph, LTwin: TGraph;
  LOptions: TGraphNegotiationOptions;
  LReport: TGraphNegotiationReport;
begin
  LOptions := NegotiationOptions(0, 1);
  LGraph := NewOneCellFixture;
  LTwin := NewOneCellFixture;
  try
    LBefore := GraphState(LGraph);
    Check(not LGraph.TrySolveNegotiated(LOptions, LReport),
      'a zero pass-backtrack budget preserves the one-way boundary');
    Check((LReport.Status = gnsPassBacktrackLimit)
      and (LReport.PassBacktracks = 0)
      and (Length(LReport.Attempts) = 0),
      'the zero-budget report has no rejected-and-reopened round');
    Check(FailureIsHousingDependency(LReport.FinalReport, 0)
      and (LReport.FinalReport.TraceHash =
        EXPECTED_ONE_CELL_BASELINE_TRACE),
      'the zero-budget terminal round is the exact baseline failure');
    Check(GraphState(LGraph) = LBefore,
      'zero-budget failure rolls every entry and selection back');
    for I := 0 to Pred(LGraph.TotalPassCount) do
      Check(LGraph.PassGraph[I].RandomIndex(1000)
          = LTwin.PassGraph[I].RandomIndex(1000),
        Format('zero-budget failure restores random stream %d', [I]));
  finally
    LTwin.Free;
    LGraph.Free;
  end;

  LGraph := NewOneCellFixture;
  try
    LOptions := NegotiationOptions(1, 1);
    Check(LGraph.TrySolveNegotiated(LOptions, LReport),
      'rollback fixture first establishes negotiated output');
    LBefore := GraphState(LGraph);
    LOptions.MaxPassBacktracks := 0;
    Check(not LGraph.TrySolveNegotiated(LOptions, LReport),
      'a later zero-budget replay still rejects marsh');
    Check((LReport.Status = gnsPassBacktrackLimit)
      and (GraphState(LGraph) = LBefore),
      'a failed replay preserves previously committed generated output');
  finally
    LGraph.Free;
  end;
end;

procedure TestSolverBudgetAndRollback;
var
  I: Integer;
  LBefore: String;
  LGraph, LTwin: TGraph;
  LOptions: TGraphNegotiationOptions;
  LReport: TGraphNegotiationReport;
begin
  LOptions := NegotiationOptions(1, 0);
  LGraph := NewOneCellFixture;
  LTwin := NewOneCellFixture;
  try
    LBefore := GraphState(LGraph);
    Check(not LGraph.TrySolveNegotiated(LOptions, LReport),
      'zero local backtracks cannot escape the exact assignment ban');
    Check((LReport.Status = gnsSolverBacktrackLimit)
      and (LReport.PassBacktracks = 1)
      and (Length(LReport.Attempts) = 1),
      'the local limit occurs after exactly one provider reopen');
    Check((LReport.FinalReport.Status = gssBacktrackLimit)
      and (LReport.FinalReport.FailedPassIndex = 0)
      and PassCountersEqual(LReport.FinalReport, 0, 1, 0, 1, 0)
      and (LReport.FinalReport.Passes[0].ExcludedAssignments = 1)
      and (not LReport.FinalReport.Passes[1].Executed),
      'the terminal provider reports its unrecovered banned assignment');
    Check(GraphState(LGraph) = LBefore,
      'the local solver limit is transactionally inert');
    for I := 0 to Pred(LGraph.TotalPassCount) do
      Check(LGraph.PassGraph[I].RandomIndex(1000)
          = LTwin.PassGraph[I].RandomIndex(1000),
        Format('local-limit failure restores random stream %d', [I]));
  finally
    LTwin.Free;
    LGraph.Free;
  end;
end;

procedure TestReplayAndOrdinarySolveIsolation;
var
  LDiscard: Integer;
  LFirstFingerprint: String;
  LGraph, LReplay, LRngTwin: TGraph;
  LNegotiationOptions: TGraphNegotiationOptions;
  LNegotiationReport, LReplayReport: TGraphNegotiationReport;
  LSolveOptions: TGraphSolveOptions;
  LSolveReport: TGraphSolveReport;
  LSuccessState: String;
  LTranscript: TGraphTraceSignature;
begin
  LNegotiationOptions := NegotiationOptions(1, 1);
  LGraph := NewOneCellFixture;
  LReplay := NewOneCellFixture;
  LRngTwin := NewOneCellFixture;
  try
    Check(LGraph.TrySolveNegotiated(LNegotiationOptions,
        LNegotiationReport)
      and LReplay.TrySolveNegotiated(LNegotiationOptions,
        LReplayReport),
      'independent same-seed negotiated fixtures solve');
    LFirstFingerprint := NegotiationFingerprint(LNegotiationReport);
    LTranscript := LNegotiationReport.TranscriptHash;
    Check(NegotiationFingerprint(LReplayReport) = LFirstFingerprint,
      'independent fixtures reproduce reports and transcript signatures');

    LDiscard := LRngTwin.PassGraph[0].RandomIndex(2);
    Check(LDiscard = 0,
      'the seed-zero provider ticket chooses marsh first');
    Check(LGraph.PassGraph[0].RandomIndex(1000)
        = LRngTwin.PassGraph[0].RandomIndex(1000),
      'provider retries freeze alternatives without consuming a new ticket');
    Check(LGraph.PassGraph[1].RandomIndex(1000)
        = LRngTwin.PassGraph[1].RandomIndex(1000),
      'the singleton consumer consumes no random ticket');

    Check(LGraph.TrySolveNegotiated(LNegotiationOptions,
        LNegotiationReport),
      'the same graph can replay negotiated generation');
    Check((LNegotiationReport.TranscriptHash = LTranscript)
      and (NegotiationFingerprint(LNegotiationReport) =
        LFirstFingerprint),
      'same-graph replay reproduces every negotiated report field');

    LSuccessState := GraphState(LGraph);
    LSolveOptions := CapturedSolveOptions(1);
    Check(not LGraph.TrySolve(LSolveOptions, LSolveReport),
      'ordinary TrySolve remains the one-way algorithm after negotiation');
    Check(FailureIsHousingDependency(LSolveReport, 0)
      and (LSolveReport.TraceHash = EXPECTED_ONE_CELL_BASELINE_TRACE),
      'ordinary solving still reproduces its original failure golden');
    Check((GraphState(LGraph) = LSuccessState)
      and (not LGraph.PassGraph[0].HasAllowedValues(0, 0, 0)),
      'ordinary failure preserves output and sees no leaked learned domain');
  finally
    LRngTwin.Free;
    LReplay.Free;
    LGraph.Free;
  end;
end;

procedure TestForcedProviderAndNegativeBounds;
var
  I: Integer;
  LBefore: String;
  LGraph, LTwin: TGraph;
  LOptions: TGraphNegotiationOptions;
  LRaised: Boolean;
  LReport: TGraphNegotiationReport;
begin
  LOptions := NegotiationOptions(4, 4);
  LGraph := NewOneCellFixture;
  try
    LGraph.SwitchToPass('terrain');
    LGraph.Entry[0, 0, 0].Value := 'marsh';
    LGraph.SwitchToPass('housing');
    Check(not LGraph.TrySolveNegotiated(LOptions, LReport),
      'a caller-owned provider lock is never negotiated away');
    Check((LReport.Status = gnsContradiction)
      and (LReport.PassBacktracks = 0)
      and (Length(LReport.Attempts) = 0)
      and FailureIsHousingDependency(LReport.FinalReport, 0),
      'a forced provider terminates as a contradiction without spinning');
    Check((LGraph.PassGraph[0].Entry[0, 0, 0].Value = 'marsh')
      and (not LGraph.PassGraph[0].Entry[0, 0, 0].Generated),
      'the failed forced-provider solve preserves caller ownership');
  finally
    LGraph.Free;
  end;

  LGraph := NewOneCellFixture;
  LTwin := NewOneCellFixture;
  try
    LBefore := GraphState(LGraph);
    LOptions := NegotiationOptions(-1, 1);
    LRaised := False;
    try
      LGraph.TrySolveNegotiated(LOptions, LReport);
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised and (GraphState(LGraph) = LBefore),
      'negative pass-backtrack bounds fail preflight without mutation');

    LOptions := NegotiationOptions(1, -1);
    LRaised := False;
    try
      LGraph.TrySolveNegotiated(LOptions, LReport);
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised and (GraphState(LGraph) = LBefore),
      'negative local bounds fail preflight without mutation');
    for I := 0 to Pred(LGraph.TotalPassCount) do
      Check(LGraph.PassGraph[I].RandomIndex(1000)
          = LTwin.PassGraph[I].RandomIndex(1000),
        Format('invalid options preserve random stream %d', [I]));
  finally
    LTwin.Free;
    LGraph.Free;
  end;
end;

begin
  WriteLn('WFC pass negotiation conformance suite');
  WriteLn('======================================');
  RunTest('public defaults', @TestPublicDefaults);
  RunTest('one-way baseline and satisfiability oracle',
    @TestOneWayBaselineAndOracle);
  RunTest('one-cell exact assignment negotiation',
    @TestOneCellNegotiation);
  RunTest('two-cell exact assignment identity',
    @TestTwoCellExactAssignment);
  RunTest('single-provider assignment exhaustion',
    @TestProviderAssignmentExhaustion);
  RunTest('join backtracks past an exhausted provider',
    @TestJoinBacktracksPastExhaustedProvider);
  RunTest('pass budget and atomic rollback',
    @TestPassBudgetAndRollback);
  RunTest('solver budget and atomic rollback',
    @TestSolverBudgetAndRollback);
  RunTest('replay and ordinary solve isolation',
    @TestReplayAndOrdinarySolveIsolation);
  RunTest('forced provider and negative bounds',
    @TestForcedProviderAndNegativeBounds);
  WriteLn('======================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d pass negotiation checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
