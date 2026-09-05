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
program wfc_restart_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  Classes, SysUtils, Math, wfc, wfc_trace;

type
  TTestProcedure = procedure;
  TClockMode = (cmFlat, cmIncreasing, cmUnavailable, cmRaises, cmNegative,
    cmNaN, cmInfinity, cmTooLarge, cmBackwards, cmReentrant);

  TObservedGraph = class(TGraph)
  strict protected
    function DoReadMonotonicMilliseconds(out AValue: Double): Boolean;
      override;
    function DoValidateCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean; override;
  public
    ClockMode: TClockMode;
    ClockCalls, ValidationCalls: Integer;
    ReentryChecks, ReentryRejections: Integer;
    RejectCommit, RaiseCommit: Boolean;
    ExpectedPublicSeed: TGraphSeed;
    PublicSeedStayedFixed: Boolean;
  end;

  TFailingEntry = class(TGraphEntry)
  strict protected
    procedure DoBeforeSetValue(const AValue: TGraphValue); override;
  end;

  TFailingEntryGraph = class(TGraph)
  strict protected
    function DoCreateEntry: TGraphEntry; override;
  end;

var
  GChecks, GFailures: Integer;
  GWriteCalls, GThrowAtWrite: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GChecks);
  if ACondition then WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailures);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try ATest;
  except
    on E: Exception do
    begin
      Inc(GFailures);
      WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function TObservedGraph.DoReadMonotonicMilliseconds(
  out AValue: Double): Boolean;
var R: TGraphSolveReport; Restart: TGraphRestartReport;
begin
  Inc(ClockCalls);
  AValue := 0;
  Result := True;
  case ClockMode of
    cmFlat: AValue := 42;
    cmIncreasing: AValue := 100 + ClockCalls * 2.5;
    cmUnavailable: Result := False;
    cmRaises: raise Exception.Create('intentional unavailable clock');
    cmNegative: AValue := -1;
    cmNaN: AValue := NaN;
    cmInfinity: AValue := Infinity;
    cmTooLarge: AValue := 9007199254740992.0;
    cmBackwards: AValue := 1000 - ClockCalls;
    cmReentrant:
      begin
        AValue := 42;
        Inc(ReentryChecks);
        try TrySolve(DefaultGraphSolveOptions, R);
        except on E: EInvalidOperation do Inc(ReentryRejections); end;
        Inc(ReentryChecks);
        try TrySolveRestarted(DefaultGraphSolveOptions,
          DefaultGraphRestartOptions, Restart);
        except on E: EInvalidOperation do Inc(ReentryRejections); end;
      end;
  end;
end;

function TObservedGraph.DoValidateCommit(out AFailedPassIndex,
  AFailedEntryIndex: Integer): Boolean;
begin
  Inc(ValidationCalls);
  PublicSeedStayedFixed := PublicSeedStayedFixed and
    (Seed = ExpectedPublicSeed);
  if RaiseCommit then
    raise Exception.Create('intentional final validation exception');
  AFailedPassIndex := 0;
  AFailedEntryIndex := 0;
  Result := not RejectCommit;
end;

procedure TFailingEntry.DoBeforeSetValue(const AValue: TGraphValue);
begin
  inherited DoBeforeSetValue(AValue);
  Inc(GWriteCalls);
  if (GThrowAtWrite > 0) and (GWriteCalls = GThrowAtWrite) then
    raise Exception.Create('intentional entry publication exception');
end;

function TFailingEntryGraph.DoCreateEntry: TGraphEntry;
begin
  Result := TFailingEntry.Create;
end;

function SolveOptions(const ABacktracks: Integer;
  const ATrace: Boolean = True): TGraphSolveOptions;
begin
  Result := DefaultGraphSolveOptions;
  Result.MaxBacktracks := ABacktracks;
  Result.CaptureTrace := ATrace;
end;

function RestartOptions(const ARestarts: Integer): TGraphRestartOptions;
begin
  Result := DefaultGraphRestartOptions;
  Result.MaxRestarts := ARestarts;
end;

function NegotiationOptions(const ABacktracks,
  APassBacktracks: Integer): TGraphNegotiationOptions;
begin
  Result := DefaultGraphNegotiationOptions;
  Result.SolveOptions := SolveOptions(ABacktracks);
  Result.MaxPassBacktracks := APassBacktracks;
end;

procedure ConfigureRing(const AGraph: TGraph; const AEscape: Boolean);
begin
  AGraph.Reshape(3, 1, 1);
  AGraph.WrapNeighbors := True;
  AGraph.AddValue('A').NewRule([gdEast, gdWest], 'B');
  if AEscape then
    AGraph.AddValue('C').NewRule([gdEast, gdWest], 'C');
end;

function NewRing(const AEscape: Boolean; const ASeed: TGraphSeed;
  const AClass: TGraphClass = nil): TGraph;
begin
  if Assigned(AClass) then Result := AClass.Create
  else Result := TGraph.Create;
  try
    Result.Seed := ASeed;
    ConfigureRing(Result, AEscape);
  except Result.Free; raise; end;
end;

function NewChoicePipeline(const ASeed: TGraphSeed): TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Seed := ASeed;
    Result.Reshape(4, 1, 1);
    Result.WrapNeighbors := False;
    Result.CurrentPass := 'terrain';
    Result.PassMode := gpmOverlay;
    Result.AddValue('A');
    Result.AddValue('B');
    Result.Entry[0, 0, 0].Value := 'A';
    Result.SwitchToPass('detail');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('X').RequireFromPass('terrain', 'A');
    Result.AddValue('Y').RequireFromPass('terrain', 'B');
    Result.SwitchToPass(0);
  except Result.Free; raise; end;
end;

function NewNegotiation(const ASeed: TGraphSeed;
  const AWithRing: Boolean = False; const AClass: TGraphClass = nil): TGraph;
begin
  if Assigned(AClass) then Result := AClass.Create
  else Result := TGraph.Create;
  try
    Result.Seed := ASeed;
    if AWithRing then Result.Reshape(3, 1, 1)
    else Result.Reshape(1, 1, 1);
    Result.WrapNeighbors := False;
    Result.CurrentPass := 'terrain';
    Result.PassMode := gpmOverlay;
    Result.AddValue('marsh');
    Result.AddValue('meadow');
    if AWithRing then
    begin
      Result.Entry[1, 0, 0].Value := 'meadow';
      Result.Entry[2, 0, 0].Value := 'meadow';
    end;
    Result.SwitchToPass('housing');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('cottage').RequireFromPass('terrain', 'meadow');
    if AWithRing then
    begin
      Result.SwitchToPass('ring');
      Result.PassMode := gpmOverlay;
      Result.ClearDependencies;
      Result.WrapNeighbors := True;
      Result.AddValue('A').NewRule([gdEast, gdWest], 'B');
      Result.AddValue('C').NewRule([gdEast, gdWest], 'C');
    end;
  except Result.Free; raise; end;
end;

function OutputState(const AGraph: TGraph): String;
var P, X, Y, Z: Integer; E: TGraphEntry;
begin
  Result := IntToStr(AGraph.CurrentPassIndex) + ':';
  for P := 0 to AGraph.TotalPassCount - 1 do
    for Z := 0 to Integer(AGraph.Dimension.Depth) - 1 do
      for Y := 0 to Integer(AGraph.Dimension.Height) - 1 do
        for X := 0 to Integer(AGraph.Dimension.Width) - 1 do
        begin
          E := AGraph.PassGraph[P].Entry[X, Y, Z];
          Result := Result + IntToStr(P) + ':' +
            IntToStr(Length(E.Value)) + ':' + E.Value + ':' +
            IntToStr(Ord(E.Empty)) + ':' + IntToStr(Ord(E.Generated)) + ';';
        end;
end;

function SameNextStreams(const A, B: TGraph): Boolean;
var I, J: Integer;
begin
  Result := A.TotalPassCount = B.TotalPassCount;
  if not Result then Exit;
  for I := 0 to A.TotalPassCount - 1 do
    for J := 1 to 5 do
      if A.PassGraph[I].RandomIndex(1000003) <>
          B.PassGraph[I].RandomIndex(1000003) then Result := False;
  for J := 1 to 5 do
    if A.RandomIndex(1000003) <> B.RandomIndex(1000003) then Result := False;
end;

procedure AdvanceStreams(const AGraph: TGraph);
var I, J: Integer;
begin
  for I := 0 to AGraph.TotalPassCount - 1 do
    for J := 1 to I + 3 do AGraph.PassGraph[I].RandomIndex(1000003);
  AGraph.RandomIndex(1000003);
end;

function SolveFingerprint(const R: TGraphSolveReport): String;
var I: Integer; P: TGraphPassSolveReport; E: TGraphTraceEvent;
begin
  Result := IntToHex(R.Seed, 8) + ':' + IntToStr(Ord(R.Status)) + ':' +
    IntToStr(R.RandomAlgorithmVersion) + ':' +
    IntToStr(R.SolverAlgorithmVersion) + ':' + IntToStr(R.GraphModelVersion) +
    ':' + IntToStr(R.PipelineAlgorithmVersion) + ':' +
    IntToStr(R.FailedPassIndex) + ':' + IntToStr(Ord(R.Contradiction.Kind)) +
    ':' + IntToStr(R.Contradiction.PassIndex) + ':' +
    IntToStr(R.Contradiction.EntryIndex) + ':' +
    IntToStr(R.Contradiction.NeighborIndex) + ':' +
    IntToStr(Ord(R.Contradiction.HasDirection)) + ':' +
    IntToStr(Ord(R.Contradiction.Direction)) + ':' +
    IntToStr(R.Contradiction.DependencyPassIndex) + ':' +
    IntToStr(Ord(R.TraceCaptured)) + ':' + IntToHex(R.TraceHash, 8);
  for I := 0 to High(R.ExecutionOrder) do
    Result := Result + '|o' + IntToStr(R.ExecutionOrder[I]);
  for I := 0 to High(R.Passes) do
  begin
    P := R.Passes[I];
    Result := Result + '|p' + IntToStr(P.Decisions) + ',' +
      IntToStr(P.Propagations) + ',' + IntToStr(P.Contradictions) + ',' +
      IntToStr(P.Backtracks) + ',' + IntToStr(P.ExcludedAssignments) + ',' +
      IntToStr(Ord(P.Executed)) + ',' + IntToStr(P.ExecutionOrdinal) + ',' +
      IntToStr(Ord(P.Disposition)) + ',' + IntToStr(P.TraceStart) + ',' +
      IntToStr(P.TraceCount);
  end;
  for I := 0 to High(R.Trace) do
  begin
    E := R.Trace[I];
    Result := Result + '|t' + IntToStr(E.EventId) + ',' +
      IntToStr(E.CauseEventId) + ',' + IntToStr(Ord(E.Kind)) + ',' +
      IntToStr(Ord(E.CauseKind)) + ',' + IntToStr(E.PassIndex) + ',' +
      IntToStr(E.EntryIndex) + ',' + IntToStr(E.ValueIndex) + ',' +
      IntToStr(Length(E.Value)) + ':' + E.Value + ',' +
      IntToStr(E.NeighborIndex) + ',' + IntToStr(Ord(E.HasDirection)) + ',' +
      IntToStr(Ord(E.Direction)) + ',' + IntToStr(E.DependencyPassIndex) +
      ',' + IntToStr(E.DecisionDepth) + ',' +
      IntToStr(E.DomainCountBefore) + ',' + IntToStr(E.DomainCountAfter);
  end;
end;

function NegotiationFingerprint(const R: TGraphNegotiationReport): String;
var I, J: Integer;
begin
  Result := IntToStr(Ord(R.Status)) + ':' + IntToHex(R.Seed, 8) + ':' +
    IntToStr(R.NegotiationAlgorithmVersion) + ':' +
    IntToStr(R.PassBacktracks) + ':' + IntToHex(R.TranscriptHash, 8);
  for I := 0 to High(R.Attempts) do
  begin
    Result := Result + '|n' +
      IntToStr(R.Attempts[I].BacktrackedPassIndex) + ',' +
      IntToStr(R.Attempts[I].BacktrackedExecutionOrdinal);
    for J := 0 to High(R.Attempts[I].ExcludedAssignment) do
      Result := Result + ',' + IntToStr(R.Attempts[I].ExcludedAssignment[J]);
    Result := Result + '{' + SolveFingerprint(R.Attempts[I].SolveReport) + '}';
  end;
  Result := Result + '|final{' + SolveFingerprint(R.FinalReport) + '}';
end;

function RestartFingerprint(const R: TGraphRestartReport): String;
var I: Integer;
begin
  { Deliberately excludes every diagnostic timing field, but compares all
    nested solve events instead of trusting only the transcript signature. }
  Result := IntToHex(R.BaseSeed, 8) + ':' + IntToStr(Ord(R.Strategy)) + ':' +
    IntToStr(Ord(R.Status)) + ':' + IntToStr(R.RestartAlgorithmVersion) + ':' +
    IntToStr(R.Restarts) + ':' + IntToHex(R.TranscriptHash, 8);
  for I := 0 to High(R.Attempts) do
    Result := Result + '|a' + IntToStr(R.Attempts[I].Index) + ',' +
      IntToHex(R.Attempts[I].Seed, 8) + ',' +
      IntToStr(R.Attempts[I].MaxBacktracks) + '{' +
      SolveFingerprint(R.Attempts[I].SolveReport) + '}{' +
      NegotiationFingerprint(R.Attempts[I].NegotiationReport) + '}';
  Result := Result + '|final{' + SolveFingerprint(R.FinalReport) + '}';
end;

procedure CheckTrace(const AGraph: TGraph; const R: TGraphSolveReport;
  const AContext: String);
var V: TGraphTraceValidationReport;
begin
  Check(R.TraceCaptured and (R.TraceHash = CalculateGraphTraceHash(R)) and
    ValidateGraphTrace(AGraph, R, V), AContext);
end;

procedure TestDefaultsAndSeeds;
const
  INDEXES: array[0..6] of Integer = (0, 1, 2, 3, 17, 65536, 2147483647);
  MIXES: array[0..6] of TGraphSeed = ($00000000, $514E28B7, $30F4C306,
    $85F0B427, $D8D09EE8, $0EA34562, $F9CC0EA8);
  BASES: array[0..2] of TGraphSeed = ($00000000, $12345678, $FFFFFFFF);
var O: TGraphRestartOptions; I, J: Integer; Good, Raised: Boolean;
begin
  O := DefaultGraphRestartOptions;
  Check((WFC_RESTART_ALGORITHM_VERSION = 1) and
    (WFC_RESTART_HASH_VERSION = 1), 'restart search and transcript versions are one');
  Check((O.MaxRestarts = 0) and (O.MaxBacktracksPerAttempt = High(Integer))
    and (O.Schedule = grschFixed) and not O.MeasureTime,
    'defaults preserve one ordinary attempt and disable diagnostic timing');
  Good := True;
  { Constants were independently evaluated using arbitrary-precision integer
    multiply followed by modulo 2^32, not the implementation's lane helper. }
  for I := 0 to High(INDEXES) do
    for J := 0 to High(BASES) do
      Good := Good and (DeriveGraphRestartSeed(BASES[J], INDEXES[I]) =
        (BASES[J] xor MIXES[I]));
  Check(Good, 'twenty-one independently derived full-width seed vectors');
  Raised := False;
  try DeriveGraphRestartSeed(0, -1);
  except on E: ERangeError do Raised := True; end;
  Check(Raised, 'negative restart attempt indices reject');
end;

procedure TestAttemptZero;
var A, B: TGraph; S: TGraphSolveOptions; O: TGraphRestartOptions;
  R: TGraphRestartReport; Plain: TGraphSolveReport; I: Integer;
begin
  O := RestartOptions(0);
  for I := 0 to 2 do
  begin
    if I = 0 then
    begin
      A := NewChoicePipeline($12345678);
      B := NewChoicePipeline($12345678);
      S := SolveOptions(16);
    end
    else
    begin
      A := NewRing(I = 1, 0);
      B := NewRing(I = 1, 0);
      if I = 1 then S := SolveOptions(0) else S := SolveOptions(16);
    end;
    try
      AdvanceStreams(A);
      AdvanceStreams(B);
      Check(A.TrySolveRestarted(S, O, R) = B.TrySolve(S, Plain),
        'attempt zero Boolean equals the ordinary solver, fixture ' + IntToStr(I));
      Check((Length(R.Attempts) = 1) and (R.Restarts = 0) and
        (R.Attempts[0].Index = 0) and (R.Attempts[0].Seed = B.Seed) and
        (R.Attempts[0].MaxBacktracks = S.MaxBacktracks),
        'attempt zero is retained with its original seed and budget');
      Check((SolveFingerprint(R.Attempts[0].SolveReport) =
        SolveFingerprint(Plain)) and (SolveFingerprint(R.FinalReport) =
        SolveFingerprint(Plain)), 'attempt zero report and full trace are unchanged');
      Check((OutputState(A) = OutputState(B)) and SameNextStreams(A, B),
        'attempt zero preserves ordinary output and advanced stream behavior');
      Check((R.BaseSeed = B.Seed) and (A.Seed = B.Seed) and
        (R.Strategy = grstOneWay), 'one-way restart identity does not mutate Seed');
      Check(NegotiationFingerprint(R.Attempts[0].NegotiationReport) =
        NegotiationFingerprint(Default(TGraphNegotiationReport)),
        'one-way attempts leave the unused negotiated report entirely default');
      if Plain.Status = gssSolved then Check(R.Status = grsSolved, 'solved maps exactly')
      else if Plain.Status = gssContradiction then
        Check(R.Status = grsContradiction, 'contradiction maps exactly')
      else Check(R.Status = grsRestartLimit, 'zero restart allowance maps solver limit');
      CheckTrace(A, R.FinalReport, 'attempt zero trace independently validates');
    finally A.Free; B.Free; end;
  end;
end;

procedure TestNegotiatedAttemptZero;
var A, B: TGraph; N: TGraphNegotiationOptions; O: TGraphRestartOptions;
  R: TGraphRestartReport; Plain: TGraphNegotiationReport; I: Integer;
begin
  O := RestartOptions(0);
  for I := 0 to 1 do
  begin
    A := NewNegotiation(0);
    B := NewNegotiation(0);
    try
      N := NegotiationOptions(16, I);
      Check(A.TrySolveNegotiatedRestarted(N, O, R) =
        B.TrySolveNegotiated(N, Plain), 'negotiated attempt zero Boolean is unchanged');
      Check((Length(R.Attempts) = 1) and (R.Restarts = 0) and
        (R.Strategy = grstNegotiated) and
        (NegotiationFingerprint(R.Attempts[0].NegotiationReport) =
          NegotiationFingerprint(Plain)),
        'nested negotiation exclusions, trace, counters and signature are unchanged');
      Check((SolveFingerprint(R.FinalReport) = SolveFingerprint(Plain.FinalReport))
        and (SolveFingerprint(R.Attempts[0].SolveReport) =
          SolveFingerprint(Plain.FinalReport)),
        'outer and attempt solve reports retain the terminal negotiated round');
      Check((OutputState(A) = OutputState(B)) and SameNextStreams(A, B),
        'negotiated attempt zero matches committed output and every stream');
      if I = 0 then
        Check(R.Status = grsPassBacktrackLimit, 'pass-budget failure is terminal')
      else Check(R.Status = grsSolved, 'negotiated recovery commits once');
    finally A.Free; B.Free; end;
  end;
end;

procedure TestEscapeAndReplay;
var A, B, Again: TGraph; S: TGraphSolveOptions; O: TGraphRestartOptions;
  R, Repeated: TGraphRestartReport; Plain: TGraphSolveReport;
  I: Integer; Before: String; AllExact: Boolean;
begin
  A := NewRing(True, 0);
  Again := NewRing(True, 0);
  try
    S := SolveOptions(0);
    O := RestartOptions(32);
    Check(A.TrySolveRestarted(S, O, R),
      'independent seeded restarts escape an initially failing zero-budget ring');
    Check((R.Status = grsSolved) and (R.Restarts > 0) and
      (Length(R.Attempts) = R.Restarts + 1) and
      (R.Attempts[0].SolveReport.Status = gssBacktrackLimit),
      'the transcript retains every failed attempt and the terminal winner');
    Check((R.Restarts = 3) and (R.Attempts[3].Seed = $85F0B427) and
      (R.TranscriptHash = $36DF8F92),
      'the portable escape winner and complete transcript match frozen version-one values');
    AllExact := True;
    for I := 0 to High(R.Attempts) do
    begin
      { These fixtures have no base-seed-dependent domain hooks. A hook that
        intentionally consults the caller seed requires whole-policy replay. }
      B := NewRing(True, DeriveGraphRestartSeed(0, I));
      try
        B.TrySolve(S, Plain);
        AllExact := AllExact and (R.Attempts[I].Index = I) and
          (R.Attempts[I].Seed = B.Seed) and
          (R.Attempts[I].MaxBacktracks = 0) and
          (SolveFingerprint(R.Attempts[I].SolveReport) =
            SolveFingerprint(Plain));
        if I < High(R.Attempts) then
          AllExact := AllExact and (Plain.Status = gssBacktrackLimit)
        else
        begin
          Check((OutputState(A) = OutputState(B)) and SameNextStreams(A, B),
            'winning effective-seed replay matches output and committed streams');
          Check(SolveFingerprint(R.FinalReport) = SolveFingerprint(Plain),
            'terminal report is the independently reproduced winning attempt');
        end;
        CheckTrace(A, R.Attempts[I].SolveReport,
          'restart attempt ' + IntToStr(I) + ' has a valid independent trace');
      finally B.Free; end;
    end;
    Check(AllExact, 'every restart equals a fresh ordinary solve at its effective seed');
    Check((A.Seed = 0) and (R.BaseSeed = 0),
      'a winning later attempt does not replace the caller public seed');
    Check(Again.TrySolveRestarted(S, O, Repeated) and
      (RestartFingerprint(R) = RestartFingerprint(Repeated)),
      'the complete ordered restart transcript replays deterministically');
    Before := RestartFingerprint(R);
    Check(A.TrySolveRestarted(S, O, Repeated),
      'an already generated graph can run the same whole-pipeline policy again');
    Check((RestartFingerprint(R) = Before) and
      (RestartFingerprint(R) = RestartFingerprint(Repeated)),
      'earlier detached reports survive subsequent solves and reproduce exactly');
    WriteLn('  escape winner: attempt=', R.Restarts,
      ', seed=', IntToHex(R.Attempts[High(R.Attempts)].Seed, 8),
      ', transcript=', IntToHex(R.TranscriptHash, 8));
  finally A.Free; Again.Free; end;
end;

procedure TestRetryBoundaries;
var A: TGraph; S: TGraphSolveOptions; O: TGraphRestartOptions;
  N: TGraphNegotiationOptions; R: TGraphRestartReport; I: Integer;
begin
  S := SolveOptions(0);
  O := RestartOptions(4);
  A := NewRing(False, 0);
  try
    Check(not A.TrySolveRestarted(S, O, R),
      'an unsatisfiable ring cannot manufacture a solution through restarts');
    Check((R.Status = grsRestartLimit) and (R.Restarts = 4) and
      (Length(R.Attempts) = 5), 'exactly the allowed four restarts are attempted');
    for I := 0 to High(R.Attempts) do
      Check((R.Attempts[I].SolveReport.Status = gssBacktrackLimit) and
        (R.Attempts[I].MaxBacktracks = 0),
        'limit-only retry retains the zero budget at attempt ' + IntToStr(I));
  finally A.Free; end;
  A := NewRing(False, 0);
  try
    S.MaxBacktracks := 16;
    Check(not A.TrySolveRestarted(S, O, R) and
      (R.Status = grsContradiction) and (Length(R.Attempts) = 1) and
      (R.Restarts = 0), 'a fully proven odd-ring contradiction is never restarted');
    Check((R.FinalReport.Status = gssContradiction) and
      (R.FinalReport.Passes[0].Backtracks = 2),
      'proof exhaustion remains distinct from the restart allowance');
  finally A.Free; end;
  A := NewNegotiation(0);
  try
    N := NegotiationOptions(16, 0);
    Check(not A.TrySolveNegotiatedRestarted(N, O, R) and
      (R.Status = grsPassBacktrackLimit) and (Length(R.Attempts) = 1),
      'outer restarts cannot circumvent an exhausted pass-negotiation budget');
    Check((R.Attempts[0].NegotiationReport.Status = gnsPassBacktrackLimit) and
      (R.Restarts = 0), 'the nested terminal pass-budget report remains visible');
  finally A.Free; end;
  A := NewNegotiation(0);
  try
    Check(not A.TrySolveRestarted(S, O, R) and
      (R.Status = grsContradiction) and (Length(R.Attempts) = 1),
      'a downstream one-way contradiction does not silently become seed search');
  finally A.Free; end;
end;

function NewRollbackFixture: TGraph;
var R: TGraphSolveReport;
begin
  Result := TGraph.Create;
  try
    Result.Seed := $10203040;
    Result.Reshape(3, 1, 1);
    Result.CurrentPass := 'existing';
    Result.PassMode := gpmOverlay;
    Result.AddValue('kept');
    Result.Entry[0, 0, 0].Value := 'kept';
    if not Result.TrySolve(SolveOptions(16), R) then
      raise Exception.Create('rollback fixture must establish generated entries');
    Result.SwitchToPass('odd-ring');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    { Reshape would clear the established generated cells in other passes. }
    Result.WrapNeighbors := True;
    Result.AddValue('A').NewRule([gdEast, gdWest], 'B');
    Result.SwitchToPass(0);
  except Result.Free; raise; end;
end;

procedure TestExhaustedTransactionRollback;
var A, Twin: TGraph; S: TGraphSolveOptions; O: TGraphRestartOptions;
  R: TGraphRestartReport; Before: String; I: Integer; Good: Boolean;
begin
  A := NewRollbackFixture;
  Twin := NewRollbackFixture;
  try
    AdvanceStreams(A);
    AdvanceStreams(Twin);
    Before := OutputState(A);
    S := SolveOptions(0);
    O := RestartOptions(7);
    Check(not A.PassGraph[1].TrySolveRestarted(S, O, R),
      'a pass-instance restart call delegates to the complete root transaction');
    Check((R.Status = grsRestartLimit) and (Length(R.Attempts) = 8),
      'the multi-pass failure exhausts all eight attempts');
    Check((OutputState(A) = Before) and (A.CurrentPassIndex = 0),
      'exhaustion restores every value, empty/generated flag, lock and selected pass');
    Check(A.PassGraph[0].Entry[1, 0, 0].Generated and
      not A.PassGraph[0].Entry[0, 0, 0].Generated,
      'previous generated output and caller-owned locks keep distinct ownership');
    Check((A.Seed = $10203040) and SameNextStreams(A, Twin),
      'exhaustion restores public seed and all advanced root/pass random streams');
    Good := True;
    for I := 0 to High(R.Attempts) do
      Good := Good and (R.Attempts[I].SolveReport.FailedPassIndex = 1) and
        (R.Attempts[I].SolveReport.Passes[0].Disposition = gpdSolved) and
        (R.Attempts[I].SolveReport.Passes[1].Disposition = gpdFailed);
    Check(Good, 'each rejected attempt staged the earlier pass without publishing it');
    A.SwitchToPass(1);
    A.AddValue('C').NewRule([gdEast, gdWest], 'C');
    Check(A.TrySolveRestarted(SolveOptions(16), RestartOptions(0), R),
      'a repaired model remains usable after full restart exhaustion');
  finally A.Free; Twin.Free; end;
end;

procedure TestClockIsolation;
var A, B: TObservedGraph; R, Baseline: TGraphRestartReport;
  S: TGraphSolveOptions; O: TGraphRestartOptions; Mode: TClockMode;
  I: Integer; Valid, Good: Boolean;
begin
  S := SolveOptions(0);
  O := RestartOptions(32);
  B := TObservedGraph(NewRing(True, 0, TObservedGraph));
  try
    B.ExpectedPublicSeed := 0;
    B.PublicSeedStayedFixed := True;
    B.ClockMode := cmRaises;
    Check(B.TrySolveRestarted(S, O, Baseline),
      'timing-disabled control solves even when its clock hook would raise');
    Check((B.ClockCalls = 0) and not Baseline.TimingAvailable and
      (Baseline.ElapsedMilliseconds = 0),
      'timing-disabled solving makes zero clock calls and returns unavailable zero');
    Good := True;
    for I := 0 to High(Baseline.Attempts) do
      Good := Good and not Baseline.Attempts[I].TimingAvailable and
        (Baseline.Attempts[I].ElapsedMilliseconds = 0);
    Check(Good, 'timing-disabled attempts also contain unavailable zero diagnostics');
    O.MeasureTime := True;
    for Mode := Low(TClockMode) to High(TClockMode) do
    begin
      A := TObservedGraph(NewRing(True, 0, TObservedGraph));
      try
        A.ClockMode := Mode;
        A.ExpectedPublicSeed := 0;
        A.PublicSeedStayedFixed := True;
        Check(A.TrySolveRestarted(S, O, R),
          'clock mode ' + IntToStr(Ord(Mode)) + ' cannot prevent successful commit');
        Check((A.ClockCalls > 0) and
          (RestartFingerprint(R) = RestartFingerprint(Baseline)),
          'clock mode ' + IntToStr(Ord(Mode)) + ' cannot affect choices or signatures');
        Check((OutputState(A) = OutputState(B)) and
          (A.ValidationCalls = 1) and A.PublicSeedStayedFixed and (A.Seed = 0),
          'clock handling keeps one complete commit and the caller seed visible');
        Valid := Mode in [cmFlat, cmIncreasing, cmReentrant];
        Good := R.TimingAvailable = Valid;
        for I := 0 to High(R.Attempts) do
          Good := Good and (R.Attempts[I].TimingAvailable = Valid);
        if Mode = cmIncreasing then
          Good := Good and (R.ElapsedMilliseconds > 0)
        else Good := Good and (R.ElapsedMilliseconds = 0);
        Check(Good, 'clock mode ' + IntToStr(Ord(Mode)) +
          ' distinguishes a valid coarse zero from unavailable timing');
        if Mode = cmReentrant then
          Check((A.ReentryChecks > 0) and
            (A.ReentryChecks = A.ReentryRejections),
            'ordinary and restarted solve reentry from a clock hook is rejected');
      finally A.Free; end;
    end;
  finally B.Free; end;
end;

procedure TestValidationAndHookFailure;
var A, Twin: TGraph; Observed: TObservedGraph; R: TGraphRestartReport;
  O: TGraphRestartOptions; S: TGraphSolveOptions;
  Before: String; Raised: Boolean; I: Integer;
begin
  O := RestartOptions(8);
  S := SolveOptions(16);
  for I := 0 to 1 do
  begin
    Observed := TObservedGraph(NewRing(True, 0, TObservedGraph));
    try
      Observed.PublicSeedStayedFixed := True;
      Observed.ExpectedPublicSeed := 0;
      Observed.RejectCommit := I = 0;
      Observed.RaiseCommit := I = 1;
      Before := OutputState(Observed);
      Raised := False;
      try
        Check(not Observed.TrySolveRestarted(S, O, R),
          'semantic commit rejection returns failure');
      except on E: Exception do
        Raised := E.Message = 'intentional final validation exception'; end;
      Check((Observed.ValidationCalls = 1) and (Raised = (I = 1)),
        'semantic rejection or exception is not retried as a solver limit');
      Check((OutputState(Observed) = Before) and (Observed.Seed = 0),
        'semantic rejection or exception restores the complete candidate');
      if I = 0 then Check((R.Status = grsContradiction) and
        (Length(R.Attempts) = 1) and
        (R.FinalReport.Contradiction.Kind = gckFinalValidation),
        'terminal semantic rejection retains final-validation diagnostics');
    finally Observed.Free; end;
  end;

  A := TFailingEntryGraph.Create;
  Twin := TGraph.Create;
  try
    A.Seed := $12345678;
    Twin.Seed := A.Seed;
    A.Reshape(3, 1, 1);
    Twin.Reshape(3, 1, 1);
    A.AddValue('A');
    Twin.AddValue('A');
    A.SwitchToPass('second');
    Twin.SwitchToPass('second');
    A.AddValue('X');
    Twin.AddValue('X');
    A.Entry[2, 0, 0].Value := 'X';
    Twin.Entry[2, 0, 0].Value := 'X';
    AdvanceStreams(A);
    AdvanceStreams(Twin);
    Before := OutputState(A);
    GWriteCalls := 0;
    GThrowAtWrite := 2;
    Raised := False;
    try A.TrySolveRestarted(S, O, R);
    except on E: Exception do
      Raised := E.Message = 'intentional entry publication exception'; end;
    GThrowAtWrite := 0;
    Check(Raised and (GWriteCalls = 2),
      'an entry publication exception propagates immediately without retry');
    Check((OutputState(A) = Before) and SameNextStreams(A, Twin),
      'partial publication failure restores entries, caller locks and advanced streams');
    Check(A.TrySolveRestarted(S, O, R) and (Length(R.Attempts) = 1),
      'the running guard is released after an entry hook exception');
  finally GThrowAtWrite := 0; A.Free; Twin.Free; end;
end;

function BudgetRaises(const AInitial, AIndex: Integer;
  const O: TGraphRestartOptions): Boolean;
begin
  Result := False;
  try GraphRestartBacktrackBudget(AInitial, AIndex, O);
  except on E: ERangeError do Result := True; end;
end;

procedure TestBudgetArithmetic;
const EXPECTED: array[0..6] of Integer = (7, 14, 28, 56, 100, 100, 100);
var O: TGraphRestartOptions; I, Invalid: Integer; Good: Boolean;
begin
  O := RestartOptions(High(Integer) - 1);
  O.Schedule := grschCappedDoubling;
  O.MaxBacktracksPerAttempt := 100;
  Good := True;
  for I := 0 to High(EXPECTED) do
    Good := Good and (GraphRestartBacktrackBudget(7, I, O) = EXPECTED[I]);
  Check(Good, 'capped doubling follows exact integer budgets 7,14,28,56,100');
  Check(GraphRestartBacktrackBudget(7, O.MaxRestarts, O) = 100,
    'a huge attempt index stops calculating once the cap is reached');
  Check(GraphRestartBacktrackBudget(0, O.MaxRestarts, O) = 0,
    'doubling a zero budget remains zero even at a huge index');
  O.MaxBacktracksPerAttempt := High(Integer);
  Check((GraphRestartBacktrackBudget(High(Integer) div 2 + 1, 0, O) =
      High(Integer) div 2 + 1) and
    (GraphRestartBacktrackBudget(High(Integer) div 2 + 1, 1, O) =
      High(Integer)) and
    (GraphRestartBacktrackBudget(High(Integer), O.MaxRestarts, O) =
      High(Integer)), 'doubling saturates without signed overflow near MaxInt');
  O.MaxBacktracksPerAttempt := 5;
  Check(BudgetRaises(17, 0, O) and BudgetRaises(17, O.MaxRestarts, O),
    'a ceiling below the initial budget rejects rather than changing attempt zero');
  O.Schedule := grschFixed;
  Check(GraphRestartBacktrackBudget(3, O.MaxRestarts, O) = 3,
    'fixed schedules preserve the initial budget at huge indices');
  O.MaxBacktracksPerAttempt := 0;
  Check(GraphRestartBacktrackBudget(0, O.MaxRestarts, O) = 0,
    'a zero ceiling is a genuine zero-backtrack policy');
  Check(BudgetRaises(-1, 0, O) and BudgetRaises(0, -1, O),
    'negative initial budget and attempt index reject before early returns');
  O.MaxRestarts := 3;
  Check(BudgetRaises(0, 4, O), 'an attempt beyond the declared allowance rejects');
  O.MaxRestarts := -1;
  Check(BudgetRaises(0, 0, O), 'a negative restart allowance rejects');
  O.MaxRestarts := High(Integer);
  Check(BudgetRaises(0, 0, O),
    'an allowance whose terminal attempt count would overflow rejects');
  O := RestartOptions(0);
  O.MaxBacktracksPerAttempt := -1;
  Check(BudgetRaises(0, 0, O), 'a negative ceiling rejects');
  O := RestartOptions(0);
  Invalid := Ord(High(TGraphRestartSchedule)) + 1;
  O.Schedule := TGraphRestartSchedule(Invalid);
  Check(BudgetRaises(0, 0, O), 'an invalid schedule rejects before a zero fast path');
end;

procedure TestAppliedBudgetsAndTraceOptOut;
var A, B: TGraph; S: TGraphSolveOptions; O: TGraphRestartOptions;
  R, Silent: TGraphRestartReport; I: Integer; Good: Boolean;
begin
  O := RestartOptions(3);
  O.Schedule := grschCappedDoubling;
  O.MaxBacktracksPerAttempt := 2;
  A := NewRing(False, 0);
  try
    Check(not A.TrySolveRestarted(SolveOptions(1), O, R) and
      (R.Status = grsContradiction) and (Length(R.Attempts) = 2),
      'growth from one to two backtracks reaches an actual contradiction proof');
    Check((R.Attempts[0].MaxBacktracks = 1) and
      (R.Attempts[0].SolveReport.Status = gssBacktrackLimit) and
      (R.Attempts[1].MaxBacktracks = 2) and
      (R.Attempts[1].SolveReport.Status = gssContradiction),
      'the applied solver budgets match the public schedule exactly');
  finally A.Free; end;
  O.MaxBacktracksPerAttempt := 1;
  A := NewRing(False, 0);
  try
    Check(not A.TrySolveRestarted(SolveOptions(1), O, R) and
      (R.Status = grsRestartLimit) and (Length(R.Attempts) = 4),
      'a capped budget never exceeds its ceiling to prove the odd ring');
    Good := True;
    for I := 0 to High(R.Attempts) do
      Good := Good and (R.Attempts[I].MaxBacktracks = 1) and
        (R.Attempts[I].SolveReport.Passes[0].Backtracks = 1);
    Check(Good, 'a reached ceiling applies to every later attempt without further growth');
  finally A.Free; end;
  O := RestartOptions(32);
  O.MaxBacktracksPerAttempt := 0;
  A := NewRing(True, 0);
  B := NewRing(True, 0);
  try
    S := SolveOptions(0);
    Check(A.TrySolveRestarted(S, O, R), 'a zero ceiling still permits seeded recovery');
    S.CaptureTrace := False;
    Check(B.TrySolveRestarted(S, O, Silent) and
      (OutputState(A) = OutputState(B)) and (R.Restarts = Silent.Restarts),
      'disabling trace capture does not alter the restart search');
    Good := True;
    for I := 0 to High(Silent.Attempts) do
      Good := Good and not Silent.Attempts[I].SolveReport.TraceCaptured and
        (Length(Silent.Attempts[I].SolveReport.Trace) = 0) and
        (Silent.Attempts[I].MaxBacktracks = 0);
    Check(Good and not Silent.FinalReport.TraceCaptured,
      'the original trace opt-out is honored by every bounded attempt');
  finally A.Free; B.Free; end;
end;

procedure TestInvalidOptions;
var A, Twin: TGraph; S: TGraphSolveOptions; O: TGraphRestartOptions;
  R: TGraphRestartReport; N: TGraphNegotiationOptions;
  Before: String; Raised: Boolean; I, Invalid: Integer;
begin
  for I := 0 to 5 do
  begin
    A := NewChoicePipeline(17);
    Twin := NewChoicePipeline(17);
    try
      AdvanceStreams(A);
      AdvanceStreams(Twin);
      Before := OutputState(A);
      S := SolveOptions(1);
      O := RestartOptions(0);
      case I of
        0: S.MaxBacktracks := -1;
        1: O.MaxRestarts := -1;
        2: O.MaxBacktracksPerAttempt := -1;
        3: begin
          Invalid := Ord(High(TGraphRestartSchedule)) + 1;
          O.Schedule := TGraphRestartSchedule(Invalid);
        end;
        5: O.MaxBacktracksPerAttempt := 0;
      end;
      Raised := False;
      try
        if I = 4 then
        begin
          N := NegotiationOptions(1, -1);
          A.TrySolveNegotiatedRestarted(N, O, R);
        end
        else A.TrySolveRestarted(S, O, R);
      except on E: ERangeError do Raised := True; end;
      Check(Raised and (OutputState(A) = Before) and SameNextStreams(A, Twin),
        'invalid option ' + IntToStr(I) + ' rejects without state or stream mutation');
    finally A.Free; Twin.Free; end;
  end;
end;

procedure TestNestedNegotiationRestart;
var A, Oracle: TGraph; N, Effective: TGraphNegotiationOptions;
  O: TGraphRestartOptions; R: TGraphRestartReport;
  Plain: TGraphNegotiationReport; I, J: Integer;
  SawExclusion, Exact: Boolean;
begin
  N := NegotiationOptions(0, 16);
  O := RestartOptions(32);
  A := NewNegotiation(0, True);
  try
    A.TrySolveNegotiatedRestarted(N, O, R);
    Check((R.Status = grsSolved) and (R.Restarts = 12) and
      (Length(R.Attempts) = 13) and (R.Attempts[12].Seed = $7C88AD73) and
      (R.TranscriptHash = $DB3CA510),
      'the nested recovery winner and transcript match frozen version-one values');
    Check((Length(R.Attempts) > 1) and
      (Length(R.Attempts[0].NegotiationReport.Attempts) > 0),
      'outer restart retains the rejected negotiated transcript before retrying');
    Exact := True;
    SawExclusion := False;
    for I := 0 to High(R.Attempts) do
    begin
      Oracle := NewNegotiation(DeriveGraphRestartSeed(0, I), True);
      try
        Effective := N;
        Effective.SolveOptions.MaxBacktracks := R.Attempts[I].MaxBacktracks;
        Oracle.TrySolveNegotiated(Effective, Plain);
        Exact := Exact and (NegotiationFingerprint(Plain) =
          NegotiationFingerprint(R.Attempts[I].NegotiationReport));
        for J := 0 to High(Plain.Attempts) do
        begin
          SawExclusion := True;
          CheckTrace(A, R.Attempts[I].NegotiationReport.Attempts[J].SolveReport,
            'a retained inner negotiated round has a valid causal trace');
        end;
        CheckTrace(A, R.Attempts[I].SolveReport,
          'a terminal inner negotiated round has a valid causal trace');
        if (I = High(R.Attempts)) and (R.Status = grsSolved) then
          Check((OutputState(A) = OutputState(Oracle)) and
            SameNextStreams(A, Oracle),
            'negotiated winner matches a fresh effective-seed transaction');
      finally Oracle.Free; end;
    end;
    Check(Exact and SawExclusion,
      'each restart begins with fresh exclusions and reproduces independent negotiation');
    Check(A.Seed = 0, 'negotiated restarts preserve the public base seed');
    WriteLn('  nested witness: base=0, attempts=', Length(R.Attempts),
      ', transcript=', IntToHex(R.TranscriptHash, 8));
  finally A.Free; end;
end;

procedure TestNegotiatedClockIsolation;
var A, B: TObservedGraph; N: TGraphNegotiationOptions;
  O: TGraphRestartOptions; R, Control: TGraphRestartReport;
  Actual, Expected: Boolean;
begin
  A := TObservedGraph(NewNegotiation(0, True, TObservedGraph));
  B := TObservedGraph(NewNegotiation(0, True, TObservedGraph));
  try
    A.ClockMode := cmRaises;
    B.ClockMode := cmRaises;
    A.ExpectedPublicSeed := 0;
    B.ExpectedPublicSeed := 0;
    A.PublicSeedStayedFixed := True;
    B.PublicSeedStayedFixed := True;
    N := NegotiationOptions(0, 16);
    O := RestartOptions(32);
    Expected := B.TrySolveNegotiatedRestarted(N, O, Control);
    Check(B.ClockCalls = 0, 'negotiated timing opt-out makes no clock calls');
    O.MeasureTime := True;
    Actual := A.TrySolveNegotiatedRestarted(N, O, R);
    Check((Actual = Expected) and (RestartFingerprint(R) =
      RestartFingerprint(Control)) and (A.ClockCalls > 0),
      'negotiated restart choices and every inner exclusion survive clock errors');
    Check((OutputState(A) = OutputState(B)) and SameNextStreams(A, B) and
      (A.ValidationCalls = B.ValidationCalls),
      'negotiated clock errors preserve the exact committed or rolled-back state');
    Check(not R.TimingAvailable and (R.ElapsedMilliseconds = 0) and
      (A.Seed = 0) and A.PublicSeedStayedFixed,
      'negotiated clock errors are unavailable diagnostics, not domain failures');
  finally A.Free; B.Free; end;
end;

procedure TestTranscriptIdentity;
var A: TGraph; S: TGraphSolveOptions; N: TGraphNegotiationOptions;
  O, ChangedOptions: TGraphRestartOptions; R, Changed: TGraphRestartReport;
  H: TGraphTraceSignature;
begin
  S := SolveOptions(0);
  O := RestartOptions(32);
  A := NewRing(True, 0);
  try
    Check(A.TrySolveRestarted(S, O, R), 'transcript fixture solves');
    H := CalculateGraphRestartTranscriptHash(S, O, R);
    Check(H = R.TranscriptHash, 'the one-way transcript signature recomputes');
    Changed := R;
    Changed.TranscriptHash := Changed.TranscriptHash xor 1;
    Check(CalculateGraphRestartTranscriptHash(S, O, Changed) = H,
      'recomputation does not trust the stored outer derived signature');
    ChangedOptions := O;
    ChangedOptions.MeasureTime := True;
    Changed := R;
    Changed.ElapsedMilliseconds := 999;
    Changed.TimingAvailable := True;
    Changed.Attempts := Copy(R.Attempts, 0, Length(R.Attempts));
    Changed.Attempts[0].ElapsedMilliseconds := 123;
    Changed.Attempts[0].TimingAvailable := True;
    Check(CalculateGraphRestartTranscriptHash(S, ChangedOptions, Changed) = H,
      'measurement opt-in and all elapsed diagnostics are excluded from identity');
    ChangedOptions := O;
    Inc(ChangedOptions.MaxRestarts);
    Check(CalculateGraphRestartTranscriptHash(S, ChangedOptions, R) <> H,
      'the declared restart allowance participates in transcript identity');
    Changed := R;
    Changed.Attempts := Copy(R.Attempts, 0, Length(R.Attempts));
    Changed.Attempts[0].Seed := Changed.Attempts[0].Seed xor 1;
    Check(CalculateGraphRestartTranscriptHash(S, O, Changed) <> H,
      'an altered attempt seed cannot retain the same transcript signature');
    Changed := R;
    Changed.Attempts := Copy(R.Attempts, 0, Length(R.Attempts));
    Inc(Changed.Attempts[0].MaxBacktracks);
    Check(CalculateGraphRestartTranscriptHash(S, O, Changed) <> H,
      'an altered effective budget changes transcript identity');
    Changed := R;
    Changed.Attempts := Copy(R.Attempts, 0, Length(R.Attempts));
    Inc(Changed.Attempts[0].Index);
    Check(CalculateGraphRestartTranscriptHash(S, O, Changed) <> H,
      'attempt ordering participates in transcript identity');
  finally A.Free; end;
  A := NewNegotiation(0);
  try
    N := NegotiationOptions(16, 1);
    O := RestartOptions(0);
    Check(A.TrySolveNegotiatedRestarted(N, O, R),
      'negotiated transcript fixture solves');
    Check(CalculateGraphRestartTranscriptHash(N, O, R) = R.TranscriptHash,
      'the negotiated transcript signature recomputes with original options');
    Changed := R;
    Changed.Attempts := Copy(R.Attempts, 0, Length(R.Attempts));
    Changed.Attempts[0].NegotiationReport.TranscriptHash :=
      Changed.Attempts[0].NegotiationReport.TranscriptHash xor 1;
    Check(CalculateGraphRestartTranscriptHash(N, O, Changed) = R.TranscriptHash,
      'outer identity recomputes rather than trusting an inner derived signature');
    O.MeasureTime := True;
    Check(CalculateGraphRestartTranscriptHash(N, O, R) = R.TranscriptHash,
      'negotiated identity also excludes diagnostic timing opt-in');
  finally A.Free; end;
end;

procedure TestPassForwarding;
var A, B: TGraph; R, RootReport: TGraphRestartReport;
  O: TGraphRestartOptions; N: TGraphNegotiationOptions;
begin
  A := NewChoicePipeline(17);
  B := NewChoicePipeline(17);
  try
    O := RestartOptions(3);
    Check(A.PassGraph[1].TrySolveRestarted(SolveOptions(16), O, R) and
      B.TrySolveRestarted(SolveOptions(16), O, RootReport) and
      (RestartFingerprint(R) = RestartFingerprint(RootReport)),
      'a successful pass-instance call forwards the exact whole-pipeline policy');
    Check((OutputState(A) = OutputState(B)) and SameNextStreams(A, B),
      'one-way forwarding preserves selected pass and committed streams');
  finally A.Free; B.Free; end;
  A := NewNegotiation(0);
  B := NewNegotiation(0);
  try
    N := NegotiationOptions(16, 1);
    Check(A.PassGraph[0].TrySolveNegotiatedRestarted(N, O, R) and
      B.TrySolveNegotiatedRestarted(N, O, RootReport) and
      (RestartFingerprint(R) = RestartFingerprint(RootReport)),
      'negotiated pass-instance forwarding preserves the nested transaction');
    Check((OutputState(A) = OutputState(B)) and SameNextStreams(A, B),
      'negotiated forwarding preserves selected pass and committed streams');
  finally A.Free; B.Free; end;
end;

begin
  RunTest('defaults and seed vectors', @TestDefaultsAndSeeds);
  RunTest('ordinary attempt-zero equivalence', @TestAttemptZero);
  RunTest('negotiated attempt-zero equivalence', @TestNegotiatedAttemptZero);
  RunTest('escaping a failed branch and effective-seed replay', @TestEscapeAndReplay);
  RunTest('retry status boundaries', @TestRetryBoundaries);
  RunTest('exhausted whole-transaction rollback', @TestExhaustedTransactionRollback);
  RunTest('diagnostic clock isolation', @TestClockIsolation);
  RunTest('semantic validation and publication hook failures', @TestValidationAndHookFailure);
  RunTest('bounded schedule arithmetic', @TestBudgetArithmetic);
  RunTest('applied budgets and trace opt-out', @TestAppliedBudgetsAndTraceOptOut);
  RunTest('invalid options are non-mutating', @TestInvalidOptions);
  RunTest('nested negotiation starts fresh at each restart', @TestNestedNegotiationRestart);
  RunTest('negotiated diagnostic clock isolation', @TestNegotiatedClockIsolation);
  RunTest('restart transcript identity', @TestTranscriptIdentity);
  RunTest('pass-instance forwarding', @TestPassForwarding);
  WriteLn('Checks: ', GChecks, ', failures: ', GFailures);
  if GFailures <> 0 then Halt(1);
end.
