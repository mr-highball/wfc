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
unit restart_policies_demo;

{$mode delphi}{$H+}

interface

uses SysUtils, wfc;

type
  TRestartPoliciesDemoResult = record
    Escape: TGraphRestartReport;
    Contradiction: TGraphRestartReport;
    CappedGrowth: TGraphRestartReport;
    Negotiated: TGraphRestartReport;
    EscapeOutput: String;
    NegotiatedOutput: String;
    TimingRequested: Boolean;
  end;

function RunRestartPoliciesDemo(
  const AMeasureTime: Boolean = False): TRestartPoliciesDemoResult;
function FormatRestartPoliciesDemo(
  const AResult: TRestartPoliciesDemoResult): String;
function RestartPoliciesSelfTest: Integer;

implementation

const
  ESCAPE_TRANSCRIPT = TGraphTraceSignature($36DF8F92);
  NEGOTIATED_TRANSCRIPT = TGraphTraceSignature($DB3CA510);

procedure Require(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise Exception.Create('restart policies demo: ' + AMessage);
end;

function SolveOptions(const ABacktracks: Integer): TGraphSolveOptions;
begin
  Result := DefaultGraphSolveOptions;
  Result.MaxBacktracks := ABacktracks;
  Result.CaptureTrace := True;
end;

function RestartOptions(const ARestarts, ACap: Integer;
  const ASchedule: TGraphRestartSchedule;
  const AMeasureTime: Boolean): TGraphRestartOptions;
begin
  Result := DefaultGraphRestartOptions;
  Result.MaxRestarts := ARestarts;
  Result.MaxBacktracksPerAttempt := ACap;
  Result.Schedule := ASchedule;
  Result.MeasureTime := AMeasureTime;
end;

function NewRing(const AEscape: Boolean): TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Seed := 0;
    Result.Reshape(3, 1, 1);
    Result.WrapNeighbors := True;
    Result.AddValue('A').NewRule([gdEast, gdWest], 'B');
    if AEscape then
      Result.AddValue('C').NewRule([gdEast, gdWest], 'C');
  except
    Result.Free;
    raise;
  end;
end;

function NewNegotiatedRing: TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Seed := 0;
    Result.Reshape(3, 1, 1);
    Result.WrapNeighbors := False;
    Result.CurrentPass := 'terrain';
    Result.PassMode := gpmOverlay;
    Result.AddValue('marsh');
    Result.AddValue('meadow');
    Result.Entry[1, 0, 0].Value := 'meadow';
    Result.Entry[2, 0, 0].Value := 'meadow';

    Result.SwitchToPass('housing');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('cottage').RequireFromPass('terrain', 'meadow');

    Result.SwitchToPass('ring');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.WrapNeighbors := True;
    Result.AddValue('A').NewRule([gdEast, gdWest], 'B');
    Result.AddValue('C').NewRule([gdEast, gdWest], 'C');
  except
    Result.Free;
    raise;
  end;
end;

function RingOutput(const AGraph: TGraph): String;
var
  I: Integer;
  LEntry: TGraphEntry;
begin
  Result := '';
  for I := 0 to 2 do
  begin
    LEntry := AGraph.Entry[I, 0, 0];
    Require(not LEntry.Empty, 'successful ring contains an empty cell');
    Require((LEntry.Value = 'A') or (LEntry.Value = 'B') or
      (LEntry.Value = 'C'), 'successful ring contains an unknown value');
    Result := Result + LEntry.Value;
  end;
  { This fixture's only valid wrapped odd-ring witness is the self-compatible
    escape token. This check samples the committed cells directly. }
  Require(Result = 'CCC', 'escape-ring output failed independent validation');
end;

function NegotiatedOutput(const AGraph: TGraph): String;
var
  I: Integer;
  LTerrain, LHousing, LRing: TGraphEntry;
begin
  Result := '';
  for I := 0 to 2 do
  begin
    LTerrain := AGraph.PassGraph[0].Entry[I, 0, 0];
    LHousing := AGraph.PassGraph[1].Entry[I, 0, 0];
    LRing := AGraph.PassGraph[2].Entry[I, 0, 0];
    Require((not LTerrain.Empty) and (LTerrain.Value = 'meadow'),
      'negotiated terrain does not support housing');
    Require((not LHousing.Empty) and (LHousing.Value = 'cottage'),
      'negotiated housing is missing');
    Require((not LRing.Empty) and (LRing.Value = 'C'),
      'negotiated odd ring did not use its compatible escape');
    if I > 0 then Result := Result + ',';
    Result := Result + LTerrain.Value + '|' + LHousing.Value + '|' +
      LRing.Value;
  end;
end;

procedure RequireRolledBack(const AGraph: TGraph; const AContext: String);
var
  I: Integer;
begin
  for I := 0 to 2 do
    Require(AGraph.Entry[I, 0, 0].Empty,
      AContext + ' retained a generated cell');
end;

procedure CheckTranscript(const AOptions: TGraphSolveOptions;
  const ARestarts: TGraphRestartOptions;
  const AReport: TGraphRestartReport; const AContext: String); overload;
begin
  Require(CalculateGraphRestartTranscriptHash(AOptions, ARestarts,
    AReport) = AReport.TranscriptHash, AContext + ' transcript mismatch');
end;

procedure CheckTranscript(const AOptions: TGraphNegotiationOptions;
  const ARestarts: TGraphRestartOptions;
  const AReport: TGraphRestartReport; const AContext: String); overload;
begin
  Require(CalculateGraphRestartTranscriptHash(AOptions, ARestarts,
    AReport) = AReport.TranscriptHash, AContext + ' transcript mismatch');
end;

function RunRestartPoliciesDemo(
  const AMeasureTime: Boolean): TRestartPoliciesDemoResult;
var
  G: TGraph;
  S: TGraphSolveOptions;
  N: TGraphNegotiationOptions;
  O: TGraphRestartOptions;
begin
  Result := Default(TRestartPoliciesDemoResult);
  Result.TimingRequested := AMeasureTime;

  G := NewRing(True);
  try
    S := SolveOptions(0);
    O := RestartOptions(32, High(Integer), grschFixed, AMeasureTime);
    Require(G.TrySolveRestarted(S, O, Result.Escape),
      'escape ring did not recover');
    Result.EscapeOutput := RingOutput(G);
    CheckTranscript(S, O, Result.Escape, 'escape ring');
  finally
    G.Free;
  end;

  G := NewRing(False);
  try
    S := SolveOptions(16);
    O := RestartOptions(4, High(Integer), grschFixed, AMeasureTime);
    Require(not G.TrySolveRestarted(S, O, Result.Contradiction),
      'impossible ring unexpectedly solved');
    RequireRolledBack(G, 'contradiction');
    CheckTranscript(S, O, Result.Contradiction, 'contradiction');
  finally
    G.Free;
  end;

  G := NewRing(False);
  try
    S := SolveOptions(1);
    O := RestartOptions(3, 2, grschCappedDoubling, AMeasureTime);
    Require(not G.TrySolveRestarted(S, O, Result.CappedGrowth),
      'capped-growth impossible ring unexpectedly solved');
    RequireRolledBack(G, 'capped growth');
    CheckTranscript(S, O, Result.CappedGrowth, 'capped growth');
  finally
    G.Free;
  end;

  G := NewNegotiatedRing;
  try
    N := DefaultGraphNegotiationOptions;
    N.SolveOptions := SolveOptions(0);
    N.MaxPassBacktracks := 16;
    O := RestartOptions(32, High(Integer), grschFixed, AMeasureTime);
    Require(G.TrySolveNegotiatedRestarted(N, O, Result.Negotiated),
      'negotiated ring did not recover');
    Result.NegotiatedOutput := NegotiatedOutput(G);
    CheckTranscript(N, O, Result.Negotiated, 'negotiated ring');
  finally
    G.Free;
  end;
end;

function RestartStatusName(const AStatus: TGraphRestartStatus): String;
begin
  Result := 'unknown';
  case AStatus of
    grsSolved: Result := 'solved';
    grsContradiction: Result := 'contradiction';
    grsRestartLimit: Result := 'restart-limit';
    grsPassBacktrackLimit: Result := 'pass-backtrack-limit';
  end;
end;

function TimingSuffix(const AReport: TGraphRestartReport;
  const ARequested: Boolean): String;
begin
  Result := '';
  if not ARequested then Exit;
  if AReport.TimingAvailable then
    Result := ' elapsed-ms=' + FloatToStr(AReport.ElapsedMilliseconds)
  else
    Result := ' timing=unavailable';
end;

function ReportLine(const AName: String; const AReport: TGraphRestartReport;
  const AOutput: String; const ATimingRequested: Boolean): String;
var
  I: Integer;
  LWinningSeed: TGraphSeed;
  LStatus: String;
begin
  Require(Length(AReport.Attempts) > 0,
    AName + ' report contains no attempts');
  LWinningSeed := AReport.Attempts[High(AReport.Attempts)].Seed;
  Result := AName + ': status=' + RestartStatusName(AReport.Status) +
    ' attempts=' + IntToStr(Length(AReport.Attempts)) +
    ' restarts=' + IntToStr(AReport.Restarts) +
    ' terminal-seed=$' + IntToHex(LWinningSeed, 8) +
    ' transcript=$' + IntToHex(AReport.TranscriptHash, 8);
  Result := Result + ' budgets=';
  for I := 0 to High(AReport.Attempts) do
  begin
    if I > 0 then Result := Result + ',';
    Result := Result + IntToStr(AReport.Attempts[I].MaxBacktracks);
  end;
  Result := Result + ' attempt-statuses=';
  for I := 0 to High(AReport.Attempts) do
  begin
    if I > 0 then Result := Result + ',';
    LStatus := 'unknown';
    case AReport.Attempts[I].SolveReport.Status of
      gssSolved: LStatus := 'solved';
      gssContradiction: LStatus := 'contradiction';
      gssBacktrackLimit: LStatus := 'local-limit';
    end;
    Result := Result + LStatus;
  end;
  if AOutput <> '' then Result := Result + ' output=' + AOutput;
  Result := Result + TimingSuffix(AReport, ATimingRequested);
end;

function FormatRestartPoliciesDemo(
  const AResult: TRestartPoliciesDemoResult): String;
begin
  Result := 'Deterministic restart policies v' +
    IntToStr(WFC_RESTART_ALGORITHM_VERSION) + LineEnding +
    ReportLine('escape-fixed', AResult.Escape, AResult.EscapeOutput,
      AResult.TimingRequested) +
    LineEnding +
    ReportLine('proved-contradiction', AResult.Contradiction, '',
      AResult.TimingRequested) +
    LineEnding +
    ReportLine('capped-doubling', AResult.CappedGrowth, '',
      AResult.TimingRequested) + LineEnding +
    ReportLine('negotiated-fixed', AResult.Negotiated,
      AResult.NegotiatedOutput, AResult.TimingRequested);
end;

function RestartPoliciesSelfTest: Integer;
var
  D: TRestartPoliciesDemoResult;

  procedure Check(const ACondition: Boolean; const AMessage: String);
  begin
    Inc(Result);
    Require(ACondition, AMessage);
  end;

begin
  Result := 0;
  D := RunRestartPoliciesDemo(False);

  Check(D.Escape.BaseSeed = 0, 'escape report changed the public base seed');
  Check((D.Escape.Status = grsSolved) and (D.Escape.Restarts = 3) and
    (Length(D.Escape.Attempts) = 4), 'escape retry boundary changed');
  Check((D.Escape.Attempts[0].SolveReport.Status = gssBacktrackLimit) and
    (D.Escape.Attempts[1].SolveReport.Status = gssBacktrackLimit) and
    (D.Escape.Attempts[2].SolveReport.Status = gssBacktrackLimit),
    'escape pre-winner attempts are not local limits');
  Check((D.Escape.Attempts[3].Seed = TGraphSeed($85F0B427)) and
    (D.EscapeOutput = 'CCC'), 'escape winner or independent output changed');
  Check(D.Escape.TranscriptHash = ESCAPE_TRANSCRIPT,
    'escape transcript changed');

  Check((D.Contradiction.Status = grsContradiction) and
    (D.Contradiction.Restarts = 0) and
    (Length(D.Contradiction.Attempts) = 1),
    'proved contradiction was retried');
  Check(D.Contradiction.FinalReport.Status = gssContradiction,
    'proved contradiction lost its solver status');
  Check(D.Contradiction.TranscriptHash = TGraphTraceSignature($3521B245),
    'proved-contradiction transcript changed');

  Check((D.CappedGrowth.Status = grsContradiction) and
    (D.CappedGrowth.Restarts = 1) and
    (Length(D.CappedGrowth.Attempts) = 2),
    'capped-growth terminal status changed');
  Check((D.CappedGrowth.Attempts[0].MaxBacktracks = 1) and
    (D.CappedGrowth.Attempts[0].SolveReport.Status = gssBacktrackLimit) and
    (D.CappedGrowth.Attempts[1].MaxBacktracks = 2) and
    (D.CappedGrowth.Attempts[1].SolveReport.Status = gssContradiction),
    'capped-growth budgets or termination changed');
  Check(D.CappedGrowth.TranscriptHash = TGraphTraceSignature($F2B4A204),
    'capped-growth transcript changed');
  Check(GraphRestartBacktrackBudget(1, 3,
    RestartOptions(3, 2, grschCappedDoubling, False)) = 2,
    'capped doubling did not remain at its ceiling');

  Check((D.Negotiated.Strategy = grstNegotiated) and
    (D.Negotiated.Status = grsSolved) and (D.Negotiated.Restarts = 12) and
    (Length(D.Negotiated.Attempts) = 13),
    'negotiated retry boundary changed');
  Check(D.Negotiated.Attempts[12].Seed = TGraphSeed($7C88AD73),
    'negotiated effective winner seed changed');
  Check(D.Negotiated.TranscriptHash = NEGOTIATED_TRANSCRIPT,
    'negotiated transcript changed');
  Check(D.NegotiatedOutput =
    'meadow|cottage|C,meadow|cottage|C,meadow|cottage|C',
    'negotiated independent output changed');

  Check((not D.Escape.TimingAvailable) and
    (D.Escape.ElapsedMilliseconds = 0),
    'disabled diagnostic timing leaked into the report');
  Check(Pos('elapsed-ms=', FormatRestartPoliciesDemo(D)) = 0,
    'deterministic formatting included diagnostic time');
end;

end.
