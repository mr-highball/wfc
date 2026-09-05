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
unit negotiated_repair_demo;

{$mode delphi}{$H+}

interface

procedure RunNegotiatedRepairDemo;

implementation

uses
  SysUtils,
  wfc,
  wfc_trace;

const
  DEMO_SEED = TGraphSeed(0);
  EXPECTED_LEAF_TRANSCRIPT = TGraphTraceSignature($7A595E38);
  EXPECTED_ROADS_SEARCH_TRANSCRIPT = TGraphTraceSignature($80926222);
  EXPECTED_ROADS_TRANSCRIPT = TGraphTraceSignature($E29050A0);
  EXPECTED_FULL_REPAIR_TRANSCRIPT = TGraphTraceSignature($9DB789E4);

  PASS_TERRAIN = 'terrain';
  PASS_CLIMATE = 'climate';
  PASS_ROADS = 'roads';
  PASS_HOUSING = 'housing';
  PASS_DECOR = 'decor';

  TERRAIN_MEADOW = 'meadow';
  TERRAIN_RIDGE = 'ridge';
  CLIMATE_SUN = 'sun';
  CLIMATE_RAIN = 'rain';
  ROADS_TRAIL = 'trail';
  ROADS_PLAZA = 'plaza';
  HOUSING_HOME = 'home';
  HOUSING_MARKET = 'market';
  DECOR_GARDEN = 'garden';
  DECOR_PORCH = 'porch';
  DECOR_MARKET_DAY = 'market-day';
  DECOR_MARKET_LIGHTS = 'market-lights';

type
  ENegotiatedRepairDemo = class(Exception);

  TRepairValues = record
    Terrain: String;
    Climate: String;
    Roads: String;
    Housing: String;
    Decor: String;
  end;

  TSelectiveRun = record
    Baseline: TRepairValues;
    TargetHousing: String;
    ExcludedRoads: String;
    Repaired: TRepairValues;
    Report: TGraphSelectiveNegotiationReport;
    ReusedRandomStreams: Boolean;
  end;

procedure Require(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise ENegotiatedRepairDemo.Create(AMessage);
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

function ValuesEqual(const A, B: TRepairValues): Boolean;
begin
  Result := (A.Terrain = B.Terrain)
    and (A.Climate = B.Climate)
    and (A.Roads = B.Roads)
    and (A.Housing = B.Housing)
    and (A.Decor = B.Decor);
end;

function IndicesEqual(const AValues: TGraphPassIndices;
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

function ExecutionOrderEqual(const AReport: TGraphSolveReport;
  const AExpected: array of Integer): Boolean;
begin
  Result := IndicesEqual(AReport.ExecutionOrder, AExpected);
end;

procedure ConfigureRepairGraph(const AGraph: TGraph);
begin
  AGraph.Seed := DEMO_SEED;
  AGraph.Reshape(1, 1, 1);
  AGraph.WrapNeighbors := False;

  AGraph.CurrentPass := PASS_TERRAIN;
  AGraph.PassMode := gpmOverlay;
  AGraph.ClearDependencies;
  AGraph.AddValue(TERRAIN_MEADOW);
  AGraph.AddValue(TERRAIN_RIDGE);

  AGraph.SwitchToPass(PASS_CLIMATE);
  AGraph.PassMode := gpmOverlay;
  AGraph.ClearDependencies;
  AGraph.AddValue(CLIMATE_SUN);
  AGraph.AddValue(CLIMATE_RAIN);

  AGraph.SwitchToPass(PASS_ROADS);
  AGraph.PassMode := gpmOverlay;
  AGraph.ClearDependencies.DependsOn(PASS_TERRAIN);
  AGraph.AddValue(ROADS_TRAIL);
  AGraph.AddValue(ROADS_PLAZA);

  AGraph.SwitchToPass(PASS_HOUSING);
  AGraph.PassMode := gpmOverlay;
  AGraph.ClearDependencies;
  AGraph.AddValue(HOUSING_HOME)
    .RequireFromPass(PASS_ROADS, ROADS_TRAIL);
  AGraph.AddValue(HOUSING_MARKET)
    .RequireFromPass(PASS_ROADS, ROADS_PLAZA);

  AGraph.SwitchToPass(PASS_DECOR);
  AGraph.PassMode := gpmOverlay;
  AGraph.ClearDependencies;
  AGraph.AddValue(DECOR_GARDEN)
    .RequireFromPass(PASS_HOUSING, HOUSING_HOME)
    .RequireFromPass(PASS_CLIMATE, CLIMATE_SUN);
  AGraph.AddValue(DECOR_PORCH)
    .RequireFromPass(PASS_HOUSING, HOUSING_HOME)
    .RequireFromPass(PASS_CLIMATE, CLIMATE_RAIN);
  AGraph.AddValue(DECOR_MARKET_DAY)
    .RequireFromPass(PASS_HOUSING, HOUSING_MARKET)
    .RequireFromPass(PASS_CLIMATE, CLIMATE_SUN);
  AGraph.AddValue(DECOR_MARKET_LIGHTS)
    .RequireFromPass(PASS_HOUSING, HOUSING_MARKET)
    .RequireFromPass(PASS_CLIMATE, CLIMATE_RAIN);
end;

function NewRepairGraph: TGraph;
begin
  Result := TGraph.Create;
  try
    ConfigureRepairGraph(Result);
  except
    Result.Free;
    raise;
  end;
end;

function CaptureValues(const AGraph: TGraph): TRepairValues;
begin
  Result.Terrain := AGraph.PassGraph[0].Entry[0, 0, 0].Value;
  Result.Climate := AGraph.PassGraph[1].Entry[0, 0, 0].Value;
  Result.Roads := AGraph.PassGraph[2].Entry[0, 0, 0].Value;
  Result.Housing := AGraph.PassGraph[3].Entry[0, 0, 0].Value;
  Result.Decor := AGraph.PassGraph[4].Entry[0, 0, 0].Value;
end;

function ExpectedDecor(const AHousing, AClimate: String): String;
begin
  if AHousing = HOUSING_HOME then
  begin
    if AClimate = CLIMATE_SUN then
      Exit(DECOR_GARDEN);
    if AClimate = CLIMATE_RAIN then
      Exit(DECOR_PORCH);
  end
  else if AHousing = HOUSING_MARKET then
  begin
    if AClimate = CLIMATE_SUN then
      Exit(DECOR_MARKET_DAY);
    if AClimate = CLIMATE_RAIN then
      Exit(DECOR_MARKET_LIGHTS);
  end;
  raise ENegotiatedRepairDemo.Create(
    'the generated housing/climate pair has no decoration');
end;

function OpposingHousing(const ARoads: String): String;
begin
  if ARoads = ROADS_TRAIL then
    Exit(HOUSING_MARKET);
  if ARoads = ROADS_PLAZA then
    Exit(HOUSING_HOME);
  raise ENegotiatedRepairDemo.Create('the baseline road value is unknown');
end;

procedure RequireValidValues(const AValues: TRepairValues;
  const AContext: String);
begin
  Require(((AValues.Roads = ROADS_TRAIL)
      and (AValues.Housing = HOUSING_HOME))
    or ((AValues.Roads = ROADS_PLAZA)
      and (AValues.Housing = HOUSING_MARKET)),
    AContext + ' violates the roads-to-housing relation');
  Require(AValues.Decor = ExpectedDecor(AValues.Housing,
      AValues.Climate),
    AContext + ' violates the housing/climate decoration join');
end;

procedure PrepareRepair(const AGraph: TGraph;
  out ABaseline: TRepairValues; out ATargetHousing: String);
var
  LReport: TGraphSolveReport;
begin
  Require(AGraph.TrySolve(CapturedSolveOptions(4), LReport),
    'the baseline five-pass district did not solve');
  Require(ExecutionOrderEqual(LReport, [0, 1, 2, 3, 4]),
    'the baseline execution order changed');
  ABaseline := CaptureValues(AGraph);
  RequireValidValues(ABaseline, 'baseline district');
  ATargetHousing := OpposingHousing(ABaseline.Roads);
  AGraph.PassGraph[3].SetAllowedValues(0, 0, 0, ATargetHousing);
  AGraph.SwitchToPass(PASS_DECOR);
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
    AContext + ' trace validator did not inspect every event');
end;

procedure RequireSearchTraces(const AGraph: TGraph;
  const AReport: TGraphNegotiationReport; const AContext: String);
var
  I: Integer;
begin
  for I := 0 to High(AReport.Attempts) do
    RequireTraceValid(AGraph, AReport.Attempts[I].SolveReport,
      AContext + ' rejected round ' + IntToStr(I));
  RequireTraceValid(AGraph, AReport.FinalReport,
    AContext + ' terminal round');
end;

function RegisteredValueAt(const AGraph: TGraph;
  const APassIndex, AValueIndex: Integer): String;
var
  LValues: TGraphValues;
begin
  LValues := AGraph.PassGraph[APassIndex].CopyRegisteredValues;
  Require((AValueIndex >= 0) and (AValueIndex < Length(LValues)),
    'excluded assignment names an unknown registered value');
  Result := LValues[AValueIndex];
end;

procedure RunLeafHorizon(out AReport: TGraphSelectiveNegotiationReport;
  out ABaseline: TRepairValues; out ATargetHousing: String);
var
  LBefore: TRepairValues;
  LGraph: TGraph;
  LOptions: TGraphNegotiationOptions;
begin
  LGraph := NewRepairGraph;
  try
    PrepareRepair(LGraph, ABaseline, ATargetHousing);
    LBefore := CaptureValues(LGraph);
    LOptions := NegotiationOptions(4, 4);
    Require(not LGraph.TryRegenerateNegotiatedFrom(PASS_HOUSING,
        LOptions, AReport),
      'a housing-only repair unexpectedly changed immutable roads');
    Require((AReport.Search.Status = gnsContradiction)
        and (AReport.Search.PassBacktracks = 0)
        and (Length(AReport.Search.Attempts) = 0),
      'the leaf horizon did not stop without a choice frame');
    Require(IndicesEqual(AReport.RequestedRootIndices, [3])
        and IndicesEqual(AReport.ActivePassIndices, [3, 4]),
      'the leaf horizon scope changed');
    Require((AReport.Search.FinalReport.FailedPassIndex = 3)
        and (AReport.Search.FinalReport.Contradiction.Kind =
          gckPassDependency)
        and (AReport.Search.FinalReport.Contradiction.DependencyPassIndex =
          2),
      'the leaf horizon did not report immutable roads as its cause');
    Require(AReport.Search.FinalReport.Passes[2].Disposition = gpdReused,
      'the roads provider was not reported as reused');
    Require(ValuesEqual(CaptureValues(LGraph), LBefore),
      'the failed leaf repair changed committed output');
    Require(LGraph.CurrentPassIndex = 4,
      'the failed leaf repair changed pass selection');
    Require(AReport.TranscriptHash =
      CalculateGraphSelectiveNegotiationTranscriptHash(LOptions, AReport),
      'the leaf selective transcript did not recompute');
    Require(AReport.TranscriptHash = EXPECTED_LEAF_TRANSCRIPT,
      'the leaf selective transcript changed from its portable golden');
    RequireSearchTraces(LGraph, AReport.Search, 'leaf repair');
  finally
    LGraph.Free;
  end;
end;

procedure RunSelectiveRoadRepair(out ARun: TSelectiveRun);
var
  LControl: TGraph;
  LControlBaseline: TRepairValues;
  LControlTarget: String;
  LGraph: TGraph;
  LOptions: TGraphNegotiationOptions;
begin
  ARun := Default(TSelectiveRun);
  LGraph := NewRepairGraph;
  LControl := NewRepairGraph;
  try
    PrepareRepair(LGraph, ARun.Baseline, ARun.TargetHousing);
    PrepareRepair(LControl, LControlBaseline, LControlTarget);
    Require(ValuesEqual(ARun.Baseline, LControlBaseline)
        and (ARun.TargetHousing = LControlTarget),
      'the selective repair control did not replay its baseline');

    LOptions := NegotiationOptions(1, 1);
    Require(LGraph.TryRegenerateNegotiatedFrom(PASS_ROADS,
        LOptions, ARun.Report),
      'the roads repair horizon did not find a compatible district');
    Require((ARun.Report.ScopeAlgorithmVersion =
          WFC_SELECTIVE_NEGOTIATION_ALGORITHM_VERSION)
        and (ARun.Report.Search.Status = gnsSolved)
        and (ARun.Report.Search.PassBacktracks = 1)
        and (Length(ARun.Report.Search.Attempts) = 1),
      'the roads repair did not retain one rejected round');
    Require(IndicesEqual(ARun.Report.RequestedRootIndices, [2])
        and IndicesEqual(ARun.Report.ActivePassIndices, [2, 3, 4]),
      'the roads repair scope changed');
    Require((ARun.Report.Search.Attempts[0].BacktrackedPassIndex = 2)
        and (ARun.Report.Search.Attempts[0].BacktrackedExecutionOrdinal = 0)
        and (Length(ARun.Report.Search.Attempts[0].ExcludedAssignment) = 1),
      'the rejected round did not reopen the active roads pass');
    ARun.ExcludedRoads := RegisteredValueAt(LGraph, 2,
      ARun.Report.Search.Attempts[0].ExcludedAssignment[0]);
    Require(ARun.ExcludedRoads = ARun.Baseline.Roads,
      'the exact excluded road is not the baseline road');
    Require((ARun.Report.Search.Attempts[0].SolveReport.FailedPassIndex = 3)
        and (ARun.Report.Search.Attempts[0].SolveReport.Contradiction.Kind =
          gckPassDependency)
        and (ARun.Report.Search.Attempts[0].SolveReport.Contradiction.
          DependencyPassIndex = 2),
      'the rejected roads round did not retain its housing contradiction');
    Require(ExecutionOrderEqual(ARun.Report.Search.FinalReport, [2, 3, 4]),
      'the terminal selective round did not execute the exact closure');
    Require((ARun.Report.Search.FinalReport.Passes[0].Disposition = gpdReused)
        and (ARun.Report.Search.FinalReport.Passes[1].Disposition = gpdReused)
        and (ARun.Report.Search.FinalReport.Passes[2].Disposition = gpdSolved)
        and (ARun.Report.Search.FinalReport.Passes[3].Disposition = gpdSolved)
        and (ARun.Report.Search.FinalReport.Passes[4].Disposition = gpdSolved),
      'the terminal report does not distinguish reused and active passes');

    ARun.Repaired := CaptureValues(LGraph);
    Require((ARun.Repaired.Terrain = ARun.Baseline.Terrain)
        and (ARun.Repaired.Climate = ARun.Baseline.Climate),
      'the selective repair changed an immutable provider');
    Require((ARun.Repaired.Roads <> ARun.Baseline.Roads)
        and (ARun.Repaired.Housing = ARun.TargetHousing),
      'the selective repair did not change roads for the requested housing');
    RequireValidValues(ARun.Repaired, 'selectively repaired district');
    Require(LGraph.CurrentPassIndex = 4,
      'successful selective repair changed pass selection');

    ARun.ReusedRandomStreams :=
      (LGraph.PassGraph[0].RandomIndex(1000003) =
        LControl.PassGraph[0].RandomIndex(1000003))
      and (LGraph.PassGraph[1].RandomIndex(1000003) =
        LControl.PassGraph[1].RandomIndex(1000003));
    Require(ARun.ReusedRandomStreams,
      'selective repair advanced an immutable provider random stream');
    Require(ARun.Report.Search.TranscriptHash =
      CalculateGraphNegotiationTranscriptHash(LOptions, ARun.Report.Search),
      'the nested negotiation transcript did not recompute');
    Require(ARun.Report.TranscriptHash =
      CalculateGraphSelectiveNegotiationTranscriptHash(LOptions,
        ARun.Report),
      'the selective negotiation transcript did not recompute');
    Require(ARun.Report.Search.TranscriptHash =
      EXPECTED_ROADS_SEARCH_TRANSCRIPT,
      'the nested roads transcript changed from its portable golden');
    Require(ARun.Report.TranscriptHash = EXPECTED_ROADS_TRANSCRIPT,
      'the scoped roads transcript changed from its portable golden');
    RequireSearchTraces(LGraph, ARun.Report.Search,
      'roads repair');
  finally
    LControl.Free;
    LGraph.Free;
  end;
end;

procedure RunFullRepair(out AReport: TGraphNegotiationReport;
  out AValues: TRepairValues; out ABaseline: TRepairValues;
  out ATargetHousing: String);
var
  LGraph: TGraph;
  LOptions: TGraphNegotiationOptions;
begin
  LGraph := NewRepairGraph;
  try
    PrepareRepair(LGraph, ABaseline, ATargetHousing);
    LOptions := NegotiationOptions(1, 1);
    Require(LGraph.TrySolveNegotiated(LOptions, AReport),
      'the full-pipeline negotiated baseline did not solve');
    Require((AReport.Status = gnsSolved)
        and (AReport.PassBacktracks = 1)
        and (Length(AReport.Attempts) = 1)
        and (AReport.Attempts[0].BacktrackedPassIndex = 2),
      'the full-pipeline baseline changed its repair path');
    Require(ExecutionOrderEqual(AReport.FinalReport, [0, 1, 2, 3, 4]),
      'the full-pipeline baseline did not activate every pass');
    AValues := CaptureValues(LGraph);
    Require((AValues.Housing = ATargetHousing)
        and (AValues.Roads <> ABaseline.Roads),
      'the full-pipeline baseline did not make the requested repair');
    RequireValidValues(AValues, 'full-pipeline repaired district');
    Require(AReport.TranscriptHash =
      CalculateGraphNegotiationTranscriptHash(LOptions, AReport),
      'the full-pipeline transcript did not recompute');
    Require(AReport.TranscriptHash = EXPECTED_FULL_REPAIR_TRANSCRIPT,
      'the full-pipeline repair transcript changed from its fixture golden');
    RequireSearchTraces(LGraph, AReport, 'full-pipeline repair');
  finally
    LGraph.Free;
  end;
end;

procedure WriteValues(const APrefix: String; const AValues: TRepairValues);
begin
  WriteLn(APrefix, ': terrain=', AValues.Terrain,
    ' climate=', AValues.Climate, ' roads=', AValues.Roads,
    ' housing=', AValues.Housing, ' decor=', AValues.Decor);
end;

procedure RunNegotiatedRepairDemo;
var
  LFullBaseline: TRepairValues;
  LFullReport: TGraphNegotiationReport;
  LFullTarget: String;
  LFullValues: TRepairValues;
  LLeafBaseline: TRepairValues;
  LLeafReport: TGraphSelectiveNegotiationReport;
  LLeafTarget: String;
  LReplay: TSelectiveRun;
  LRun: TSelectiveRun;
begin
  Require((WFC_SELECTIVE_NEGOTIATION_ALGORITHM_VERSION = 1)
      and (WFC_SELECTIVE_NEGOTIATION_HASH_VERSION = 1),
    'the demo requires Selective Negotiation v1');
  RunLeafHorizon(LLeafReport, LLeafBaseline, LLeafTarget);
  RunSelectiveRoadRepair(LRun);
  RunSelectiveRoadRepair(LReplay);
  RunFullRepair(LFullReport, LFullValues, LFullBaseline, LFullTarget);

  Require(ValuesEqual(LLeafBaseline, LRun.Baseline)
      and ValuesEqual(LRun.Baseline, LReplay.Baseline)
      and ValuesEqual(LRun.Baseline, LFullBaseline)
      and (LLeafTarget = LRun.TargetHousing)
      and (LRun.TargetHousing = LReplay.TargetHousing)
      and (LRun.TargetHousing = LFullTarget),
    'the independent repair fixtures did not share one baseline request');
  Require(ValuesEqual(LRun.Repaired, LReplay.Repaired)
      and ValuesEqual(LRun.Repaired, LFullValues),
    'selective replay and full search produced different repaired output');
  Require((LRun.Report.TranscriptHash = LReplay.Report.TranscriptHash)
      and (LRun.Report.Search.TranscriptHash =
        LReplay.Report.Search.TranscriptHash),
    'same-seed selective repair changed its transcript');

  WriteLn('NegotiatedRepair: terrain -> roads -> housing -> decor <- climate');
  WriteLn('Seed: ', DEMO_SEED);
  WriteLn('Selective negotiation versions: algorithm=',
    WFC_SELECTIVE_NEGOTIATION_ALGORITHM_VERSION, ' hash=',
    WFC_SELECTIVE_NEGOTIATION_HASH_VERSION);
  WriteValues('Baseline', LRun.Baseline);
  WriteLn('Requested housing: ', LRun.TargetHousing);
  WriteLn('Leaf horizon: requested=[3] active=[3,4] status=contradiction ',
    'roads=reused hash=',
    GraphTraceSignatureHex(LLeafReport.TranscriptHash));
  WriteLn('Roads horizon: requested=[2] active=[2,3,4]');
  WriteLn('Rejected round 0: roads=[', LRun.ExcludedRoads,
    '] housing=contradiction');
  WriteValues('Repaired', LRun.Repaired);
  WriteLn('Selective rounds: ', Length(LRun.Report.Search.Attempts) + 1,
    ' pass backtracks: ', LRun.Report.Search.PassBacktracks);
  WriteLn('Reused providers: terrain,climate values-and-rng=preserved');
  WriteLn('Nested negotiation hash: ',
    GraphTraceSignatureHex(LRun.Report.Search.TranscriptHash));
  WriteLn('Selective transcript hash: ',
    GraphTraceSignatureHex(LRun.Report.TranscriptHash));
  WriteLn('Full baseline: active=[0,1,2,3,4] transcript=',
    GraphTraceSignatureHex(LFullReport.TranscriptHash));
  WriteLn('Deterministic replay: identical transcript and output');
  WriteLn('Self-check: passed');
end;

end.
