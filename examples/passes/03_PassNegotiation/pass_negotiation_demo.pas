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
unit pass_negotiation_demo;

{$mode delphi}{$H+}

interface

procedure RunPassNegotiationDemo;

implementation

uses
  SysUtils,
  wfc,
  wfc_trace;

const
  DEMO_SEED = TGraphSeed(0);
  EXPECTED_ONE_WAY_TRACE = TGraphTraceSignature($42AF302E);
  EXPECTED_TRANSCRIPT = TGraphTraceSignature($6F76591D);

  PASS_TERRAIN = 'terrain';
  PASS_HOUSING = 'housing';
  TERRAIN_MARSH = 'marsh';
  TERRAIN_MEADOW = 'meadow';
  HOUSING_COTTAGE = 'cottage';

type
  EPassNegotiationDemo = class(Exception);

  TNegotiatedRun = record
    Report: TGraphNegotiationReport;
    ExcludedTerrain: String;
    Terrain: String;
    Housing: String;
  end;

procedure Require(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise EPassNegotiationDemo.Create(AMessage);
end;

function CapturedSolveOptions: TGraphSolveOptions;
begin
  Result := DefaultGraphSolveOptions;
  Result.MaxBacktracks := 1;
  Result.CaptureTrace := True;
end;

function NegotiationOptions: TGraphNegotiationOptions;
begin
  Result := DefaultGraphNegotiationOptions;
  Result.SolveOptions := CapturedSolveOptions;
  Result.MaxPassBacktracks := 1;
end;

procedure ConfigureGraph(const AGraph: TGraph);
begin
  AGraph.Seed := DEMO_SEED;
  AGraph.Reshape(1, 1, 1);
  AGraph.WrapNeighbors := False;

  AGraph.CurrentPass := PASS_TERRAIN;
  AGraph.PassMode := gpmOverlay;
  //Seed zero chooses marsh first. Meadow remains a valid alternative that an
  //ordinary forward-only pass transaction has no reason to revisit.
  AGraph.AddValue(TERRAIN_MARSH);
  AGraph.AddValue(TERRAIN_MEADOW);

  AGraph.SwitchToPass(PASS_HOUSING);
  AGraph.PassMode := gpmOverlay;
  AGraph.ClearDependencies;
  AGraph.AddValue(HOUSING_COTTAGE)
    .RequireFromPass(PASS_TERRAIN, TERRAIN_MEADOW);
end;

function FailureIsHousingDependency(
  const AReport: TGraphSolveReport): Boolean;
begin
  Result := (AReport.Status = gssContradiction)
    and (AReport.FailedPassIndex = 1)
    and (AReport.Contradiction.Kind = gckPassDependency)
    and (AReport.Contradiction.PassIndex = 1)
    and (AReport.Contradiction.EntryIndex = 0)
    and (AReport.Contradiction.DependencyPassIndex = 0);
end;

procedure RequireTraceValid(const AGraph: TGraph;
  const AReport: TGraphSolveReport; const AContext: String);
var
  LValidation: TGraphTraceValidationReport;
begin
  Require(AReport.TraceCaptured, AContext + ' did not capture a trace');
  Require(AReport.TraceHash = CalculateGraphTraceHash(AReport),
    AContext + ' trace hash did not recompute');
  Require(ValidateGraphTrace(AGraph, AReport, LValidation),
    AContext + ': ' +
      DescribeGraphTraceValidationIssue(LValidation.Issue));
  Require(LValidation.CheckedEvents = Length(AReport.Trace),
    AContext + ' validator did not inspect every event');
end;

procedure RequirePassCounters(const AReport: TGraphSolveReport;
  const APass, ADecisions, APropagations, AContradictions,
  ABacktracks, AExcludedAssignments: Integer;
  const ADisposition: TGraphPassDisposition;
  const AContext: String);
begin
  Require((APass >= 0) and (APass < Length(AReport.Passes)),
    AContext + ' has no requested pass report');
  Require((AReport.Passes[APass].Decisions = ADecisions)
      and (AReport.Passes[APass].Propagations = APropagations)
      and (AReport.Passes[APass].Contradictions = AContradictions)
      and (AReport.Passes[APass].Backtracks = ABacktracks)
      and (AReport.Passes[APass].ExcludedAssignments =
        AExcludedAssignments)
      and (AReport.Passes[APass].Disposition = ADisposition),
    AContext + ' counters or disposition changed');
end;

function RegisteredValueAt(const AGraph: TGraph;
  const APassIndex, AValueIndex: Integer): TGraphValue;
var
  LValues: TGraphValues;
begin
  Require((APassIndex >= 0) and
    (APassIndex < AGraph.TotalPassCount),
    'excluded assignment names an unknown pass');
  LValues := AGraph.PassGraph[APassIndex].CopyRegisteredValues;
  Require((AValueIndex >= 0) and (AValueIndex < Length(LValues)),
    'excluded assignment names an unknown registered value');
  Result := LValues[AValueIndex];
end;

procedure RunOneWay(out AReport: TGraphSolveReport);
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
begin
  LGraph := TGraph.Create;
  try
    ConfigureGraph(LGraph);
    LOptions := CapturedSolveOptions;
    Require(not LGraph.TrySolve(LOptions, AReport),
      'ordinary one-way solve unexpectedly succeeded');
    Require(FailureIsHousingDependency(AReport),
      'ordinary solve did not fail at housing from terrain');
    Require((Length(AReport.ExecutionOrder) = 2)
        and (AReport.ExecutionOrder[0] = 0)
        and (AReport.ExecutionOrder[1] = 1),
      'ordinary solve execution order changed');
    RequirePassCounters(AReport, 0, 1, 0, 0, 0, 0,
      gpdSolved, 'ordinary terrain pass');
    RequirePassCounters(AReport, 1, 0, 0, 1, 0, 0,
      gpdFailed, 'ordinary housing pass');
    Require(AReport.TraceHash = EXPECTED_ONE_WAY_TRACE,
      'ordinary failure trace no longer matches the portable golden');
    RequireTraceValid(LGraph, AReport, 'ordinary one-way solve');
    Require(LGraph.PassGraph[0].Entry[0, 0, 0].Empty
        and LGraph.PassGraph[1].Entry[0, 0, 0].Empty,
      'failed ordinary transaction committed a partial result');
  finally
    LGraph.Free;
  end;
end;

procedure RunNegotiated(out ARun: TNegotiatedRun);
var
  LGraph: TGraph;
  LOptions: TGraphNegotiationOptions;
begin
  ARun := Default(TNegotiatedRun);
  LGraph := TGraph.Create;
  try
    ConfigureGraph(LGraph);
    LOptions := NegotiationOptions;
    Require(LGraph.TrySolveNegotiated(LOptions, ARun.Report),
      'bounded pass negotiation did not find the composition');
    Require((ARun.Report.Status = gnsSolved)
        and (ARun.Report.Seed = DEMO_SEED)
        and (ARun.Report.NegotiationAlgorithmVersion =
          WFC_PASS_NEGOTIATION_ALGORITHM_VERSION),
      'negotiation terminal metadata changed');
    Require((WFC_PASS_NEGOTIATION_ALGORITHM_VERSION = 1)
        and (WFC_PASS_NEGOTIATION_HASH_VERSION = 1),
      'demo golden requires negotiation and transcript version one');
    Require((ARun.Report.PassBacktracks = 1)
        and (Length(ARun.Report.Attempts) = 1),
      'negotiation did not retain exactly one rejected round');

    Require(FailureIsHousingDependency(
        ARun.Report.Attempts[0].SolveReport),
      'rejected round is not the original housing contradiction');
    Require(ARun.Report.Attempts[0].SolveReport.TraceHash =
      EXPECTED_ONE_WAY_TRACE,
      'rejected round does not retain the ordinary trace golden');
    Require((ARun.Report.Attempts[0].BacktrackedPassIndex = 0)
        and (ARun.Report.Attempts[0].BacktrackedExecutionOrdinal = 0),
      'negotiation reopened the wrong provider execution');
    Require((Length(ARun.Report.Attempts[0].ExcludedAssignment) = 1)
        and (ARun.Report.Attempts[0].ExcludedAssignment[0] = 0),
      'negotiation did not record the exact marsh assignment');
    ARun.ExcludedTerrain := RegisteredValueAt(LGraph, 0,
      ARun.Report.Attempts[0].ExcludedAssignment[0]);
    Require(ARun.ExcludedTerrain = TERRAIN_MARSH,
      'excluded value index no longer maps to marsh');

    Require((ARun.Report.FinalReport.Status = gssSolved)
        and (ARun.Report.FinalReport.FailedPassIndex = -1)
        and (Length(ARun.Report.FinalReport.ExecutionOrder) = 2)
        and (ARun.Report.FinalReport.ExecutionOrder[0] = 0)
        and (ARun.Report.FinalReport.ExecutionOrder[1] = 1),
      'terminal round is not the solved terrain-to-housing pipeline');
    RequirePassCounters(ARun.Report.FinalReport, 0,
      2, 0, 1, 1, 1, gpdSolved, 'negotiated terrain pass');
    RequirePassCounters(ARun.Report.FinalReport, 1,
      0, 0, 0, 0, 0, gpdSolved, 'negotiated housing pass');
    RequireTraceValid(LGraph, ARun.Report.Attempts[0].SolveReport,
      'rejected negotiation round');
    RequireTraceValid(LGraph, ARun.Report.FinalReport,
      'terminal negotiation round');

    Require(not LGraph.PassGraph[0].Entry[0, 0, 0].Empty
        and LGraph.PassGraph[0].Entry[0, 0, 0].Generated
        and not LGraph.PassGraph[1].Entry[0, 0, 0].Empty
        and LGraph.PassGraph[1].Entry[0, 0, 0].Generated,
      'negotiation did not commit generated values atomically');
    ARun.Terrain := LGraph.PassGraph[0].Entry[0, 0, 0].Value;
    ARun.Housing := LGraph.PassGraph[1].Entry[0, 0, 0].Value;
    Require((ARun.Terrain = TERRAIN_MEADOW)
        and (ARun.Housing = HOUSING_COTTAGE),
      'negotiation did not commit meadow and cottage');
    Require((not LGraph.PassGraph[0].HasAllowedValues(0, 0, 0))
        and (not LGraph.PassGraph[1].HasAllowedValues(0, 0, 0)),
      'negotiation leaked an internal exclusion into caller domains');
    Require(ARun.Report.TranscriptHash = EXPECTED_TRANSCRIPT,
      'negotiation transcript no longer matches the portable golden');
    Require(ARun.Report.TranscriptHash =
      CalculateGraphNegotiationTranscriptHash(LOptions, ARun.Report),
      'negotiation transcript hash did not recompute');
  finally
    LGraph.Free;
  end;
end;

procedure RequireReplay(const AFirst, ASecond: TNegotiatedRun);
begin
  Require((AFirst.Terrain = ASecond.Terrain)
      and (AFirst.Housing = ASecond.Housing)
      and (AFirst.ExcludedTerrain = ASecond.ExcludedTerrain),
    'same-seed negotiation changed its values');
  Require((AFirst.Report.TranscriptHash = ASecond.Report.TranscriptHash)
      and (AFirst.Report.PassBacktracks = ASecond.Report.PassBacktracks)
      and (Length(AFirst.Report.Attempts) =
        Length(ASecond.Report.Attempts))
      and (AFirst.Report.Attempts[0].BacktrackedPassIndex =
        ASecond.Report.Attempts[0].BacktrackedPassIndex)
      and (AFirst.Report.Attempts[0].ExcludedAssignment[0] =
        ASecond.Report.Attempts[0].ExcludedAssignment[0]),
    'same-seed negotiation changed its transcript');
end;

procedure RunPassNegotiationDemo;
var
  LOneWay: TGraphSolveReport;
  LRun: TNegotiatedRun;
  LReplay: TNegotiatedRun;
begin
  RunOneWay(LOneWay);
  RunNegotiated(LRun);
  RunNegotiated(LReplay);
  RequireReplay(LRun, LReplay);

  WriteLn('PassNegotiation: terrain -> housing');
  WriteLn('Seed: ', DEMO_SEED);
  WriteLn('Negotiation versions: algorithm=',
    WFC_PASS_NEGOTIATION_ALGORITHM_VERSION, ' hash=',
    WFC_PASS_NEGOTIATION_HASH_VERSION);
  WriteLn('One-way: failed pass=', LOneWay.FailedPassIndex,
    ' kind=pass-dependency provider=',
    LOneWay.Contradiction.DependencyPassIndex, ' trace=',
    GraphTraceSignatureHex(LOneWay.TraceHash));
  WriteLn('Rejected round 0: terrain=[', LRun.ExcludedTerrain,
    '] housing=contradiction');
  WriteLn('Excluded provider assignment: terrain=[',
    LRun.ExcludedTerrain, ']');
  WriteLn('Negotiated: ', LRun.Terrain, '|', LRun.Housing);
  WriteLn('Rounds: ', Length(LRun.Report.Attempts) + 1);
  WriteLn('Pass backtracks: ', LRun.Report.PassBacktracks);
  WriteLn('Terminal terrain: decisions=',
    LRun.Report.FinalReport.Passes[0].Decisions,
    ' contradictions=',
    LRun.Report.FinalReport.Passes[0].Contradictions,
    ' backtracks=', LRun.Report.FinalReport.Passes[0].Backtracks,
    ' exclusions=',
    LRun.Report.FinalReport.Passes[0].ExcludedAssignments);
  WriteLn('Transcript hash: ',
    GraphTraceSignatureHex(LRun.Report.TranscriptHash));
  WriteLn('Deterministic replay: identical transcript and output');
  WriteLn('Self-check: passed');
end;

end.
