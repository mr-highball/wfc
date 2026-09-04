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
unit trace_inspector_demo;

{$mode delphi}{$H+}

interface

procedure RunTraceInspector;

implementation

uses
  SysUtils,
  wfc,
  wfc_trace;

const
  DEMO_WIDTH = 4;
  DEMO_SEED = TGraphSeed(0);
  EXPECTED_TRACE_HASH = TGraphTraceSignature($73C4B9A2);

  PASS_TERRAIN = 'terrain';
  PASS_SETTLEMENT = 'settlement';
  PASS_FOLIAGE = 'foliage';

  TERRAIN_LAND = 'land';
  TERRAIN_WATER = 'water';

  SETTLEMENT_VACANT = 'vacant';
  SETTLEMENT_HOME = 'home';

  FOLIAGE_BARE = 'bare';
  FOLIAGE_TREE = 'tree';
  FOLIAGE_REEDS = 'reeds';

type
  ETraceInspector = class(Exception);

  TInspectorRun = record
    Terrain: String;
    Settlement: String;
    Foliage: String;
    Report: TGraphSolveReport;
    ChainStartEventId: Integer;
    ChainProviderEventId: Integer;
  end;

procedure Require(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise ETraceInspector.Create(AMessage);
end;

function BooleanName(const AValue: Boolean): String;
begin
  if AValue then
    Result := 'true'
  else
    Result := 'false';
end;

function PassDispositionName(
  const ADisposition: TGraphPassDisposition): String;
begin
  case ADisposition of
    gpdNotRun: Result := 'not-run';
    gpdReused: Result := 'reused';
    gpdCleared: Result := 'cleared';
    gpdCopied: Result := 'copied';
    gpdSolved: Result := 'solved';
    gpdFailed: Result := 'failed';
  end;
end;

procedure ConfigureGraph(const AGraph: TGraph);
begin
  AGraph.Reshape(DEMO_WIDTH, 1, 1);
  AGraph.WrapNeighbors := False;
  AGraph.Seed := DEMO_SEED;

  AGraph.CurrentPass := PASS_TERRAIN;
  AGraph.AddValue(TERRAIN_LAND);
  AGraph.AddValue(TERRAIN_WATER);
  AGraph.Entry[0, 0, 0].Value := TERRAIN_LAND;
  AGraph.Entry[1, 0, 0].Value := TERRAIN_WATER;
  AGraph.Entry[2, 0, 0].Value := TERRAIN_LAND;
  AGraph.Entry[3, 0, 0].Value := TERRAIN_LAND;

  AGraph.SwitchToPass(PASS_SETTLEMENT);
  AGraph.PassMode := gpmOverlay;
  AGraph.AddValue(SETTLEMENT_VACANT);
  AGraph.AddValue(SETTLEMENT_HOME)
    .RequireFromPass(PASS_TERRAIN, TERRAIN_LAND);
  //This caller anchor guarantees that foliage observes occupied land.
  AGraph.Entry[0, 0, 0].Value := SETTLEMENT_HOME;

  AGraph.SwitchToPass(PASS_FOLIAGE);
  AGraph.PassMode := gpmOverlay;
  AGraph.AddValue(FOLIAGE_TREE)
    .RequireFromPass(PASS_TERRAIN, TERRAIN_LAND)
    .RequireFromPass(PASS_SETTLEMENT, SETTLEMENT_VACANT);
  AGraph.AddValue(FOLIAGE_REEDS)
    .RequireFromPass(PASS_TERRAIN, TERRAIN_WATER);
  //The unconstrained fallback is last so seed zero visibly exercises both
  //constrained alternatives where their provider passes permit them.
  AGraph.AddValue(FOLIAGE_BARE);
end;

function CaptureLayer(const AGraph: TGraph;
  const APassIndex: Integer): String;
var
  I: Integer;
  LEntry: TGraphEntry;
begin
  Result := '';
  for I := 0 to DEMO_WIDTH - 1 do
  begin
    LEntry := AGraph.PassGraph[APassIndex].Entry[I, 0, 0];
    Require(not LEntry.Empty, 'solved pass contains an empty entry');
    if I > 0 then
      Result := Result + ',';
    Result := Result + LEntry.Value;
  end;
end;

procedure ValidateLayers(const AGraph: TGraph);
var
  I: Integer;
  LFoliage: TGraphValue;
  LSettlement: TGraphValue;
  LTerrain: TGraphValue;
begin
  Require(AGraph.TotalPassCount = 3, 'demo pass count changed');
  Require(AGraph.PassGraph[0].Entry[0, 0, 0].Value = TERRAIN_LAND,
    'terrain anchor 0 changed');
  Require(AGraph.PassGraph[0].Entry[1, 0, 0].Value = TERRAIN_WATER,
    'terrain anchor 1 changed');
  Require(AGraph.PassGraph[0].Entry[2, 0, 0].Value = TERRAIN_LAND,
    'terrain anchor 2 changed');
  Require(AGraph.PassGraph[0].Entry[3, 0, 0].Value = TERRAIN_LAND,
    'terrain anchor 3 changed');

  for I := 0 to DEMO_WIDTH - 1 do
  begin
    LTerrain := AGraph.PassGraph[0].Entry[I, 0, 0].Value;
    LSettlement := AGraph.PassGraph[1].Entry[I, 0, 0].Value;
    LFoliage := AGraph.PassGraph[2].Entry[I, 0, 0].Value;

    Require((LSettlement = SETTLEMENT_VACANT) or
      (LSettlement = SETTLEMENT_HOME),
      'settlement pass contains an unknown value');
    if LSettlement = SETTLEMENT_HOME then
      Require(LTerrain = TERRAIN_LAND,
        'a home was generated on non-land terrain');

    Require((LFoliage = FOLIAGE_BARE) or
      (LFoliage = FOLIAGE_TREE) or (LFoliage = FOLIAGE_REEDS),
      'foliage pass contains an unknown value');
    if LFoliage = FOLIAGE_TREE then
      Require((LTerrain = TERRAIN_LAND) and
        (LSettlement = SETTLEMENT_VACANT),
        'a tree violated terrain or settlement constraints')
    else if LFoliage = FOLIAGE_REEDS then
      Require(LTerrain = TERRAIN_WATER,
        'reeds were generated on non-water terrain');
  end;

  Require(AGraph.PassGraph[1].Entry[0, 0, 0].Value = SETTLEMENT_HOME,
    'settlement anchor changed');
  Require(AGraph.PassGraph[1].Entry[1, 0, 0].Value =
    SETTLEMENT_VACANT, 'terrain did not filter the water-cell home');
  Require(AGraph.PassGraph[2].Entry[0, 0, 0].Value = FOLIAGE_BARE,
    'settlement did not filter the occupied-cell tree');
end;

function FindDependencyRemoval(const AReport: TGraphSolveReport;
  const AConsumerPass, AProviderPass, AEntryIndex: Integer;
  const AValue: TGraphValue): TGraphTraceEvent;
var
  I: Integer;
begin
  for I := 0 to High(AReport.Trace) do
    if (AReport.Trace[I].Kind = gtekInitialCandidateRemoved) and
        (AReport.Trace[I].CauseKind = gtckPassDependency) and
        (AReport.Trace[I].PassIndex = AConsumerPass) and
        (AReport.Trace[I].DependencyPassIndex = AProviderPass) and
        (AReport.Trace[I].EntryIndex = AEntryIndex) and
        (AReport.Trace[I].Value = AValue) then
      Exit(AReport.Trace[I]);
  raise ETraceInspector.CreateFmt(
    'missing dependency removal pass=%d provider=%d entry=%d value=%s',
    [AConsumerPass, AProviderPass, AEntryIndex, AValue]);
end;

procedure RequireProviderCause(const AReport: TGraphSolveReport;
  const ARemoval: TGraphTraceEvent);
var
  LProvider: TGraphTraceEvent;
begin
  Require(ARemoval.CauseEventId >= 0,
    'dependency removal has no provider cause event');
  LProvider := FindGraphTraceEvent(AReport.Trace,
    ARemoval.CauseEventId);
  Require(LProvider.PassIndex = ARemoval.DependencyPassIndex,
    'dependency removal does not point into its provider pass');
end;

function ResolveProviderEventId(const AReport: TGraphSolveReport;
  const AStartEventId: Integer): Integer;
var
  I: Integer;
  LCurrent: TGraphTraceEvent;
  LProviderPass: Integer;
begin
  LCurrent := FindGraphTraceEvent(AReport.Trace, AStartEventId);
  LProviderPass := LCurrent.DependencyPassIndex;
  Require(LProviderPass >= 0,
    'causal chain start is not a pass-dependency event');
  for I := 0 to Length(AReport.Trace) do
  begin
    if LCurrent.PassIndex = LProviderPass then
      Exit(LCurrent.EventId);
    Require(LCurrent.CauseEventId >= 0,
      'causal chain ended before reaching its provider pass');
    Require(LCurrent.CauseEventId < LCurrent.EventId,
      'causal chain does not move backward');
    LCurrent := FindGraphTraceEvent(AReport.Trace,
      LCurrent.CauseEventId);
  end;
  raise ETraceInspector.Create('causal chain contains a cycle');
end;

procedure ValidateTrace(const AGraph: TGraph;
  const AReport: TGraphSolveReport; out AChainStartEventId,
  AChainProviderEventId: Integer);
var
  LRemoval: TGraphTraceEvent;
  LValidation: TGraphTraceValidationReport;
begin
  Require(AReport.Status = gssSolved, 'trace run did not solve');
  Require(AReport.TraceCaptured, 'trace capture was not enabled');
  Require(Length(AReport.Trace) > 0, 'captured trace is empty');
  Require(AReport.TraceHash <> 0, 'captured trace hash is zero');
  Require(AReport.TraceHash = CalculateGraphTraceHash(AReport),
    'portable trace hash does not match the event stream');
  if AReport.TraceHash <> EXPECTED_TRACE_HASH then
    raise ETraceInspector.CreateFmt(
      'seed-zero trace no longer matches the native/pas2js golden [%s <> %s]',
      [GraphTraceSignatureHex(AReport.TraceHash),
        GraphTraceSignatureHex(EXPECTED_TRACE_HASH)]);
  Require(ValidateGraphTrace(AGraph, AReport, LValidation),
    DescribeGraphTraceValidationIssue(LValidation.Issue));
  Require(LValidation.CheckedEvents = Length(AReport.Trace),
    'trace validator did not inspect every event');
  Require(Length(AReport.ExecutionOrder) = 3,
    'pipeline execution order length changed');
  Require((AReport.ExecutionOrder[0] = 0) and
    (AReport.ExecutionOrder[1] = 1) and
    (AReport.ExecutionOrder[2] = 2),
    'pipeline did not execute terrain, settlement, foliage');

  LRemoval := FindDependencyRemoval(AReport, 1, 0, 1,
    SETTLEMENT_HOME);
  RequireProviderCause(AReport, LRemoval);
  LRemoval := FindDependencyRemoval(AReport, 2, 0, 0,
    FOLIAGE_REEDS);
  RequireProviderCause(AReport, LRemoval);
  LRemoval := FindDependencyRemoval(AReport, 2, 1, 0,
    FOLIAGE_TREE);
  RequireProviderCause(AReport, LRemoval);
  AChainStartEventId := LRemoval.EventId;
  AChainProviderEventId := ResolveProviderEventId(AReport,
    AChainStartEventId);
end;

procedure SolveInspectorGraph(const AGraph: TGraph;
  out ARun: TInspectorRun);
var
  LOptions: TGraphSolveOptions;
begin
  ARun := Default(TInspectorRun);
  ARun.ChainStartEventId := -1;
  ARun.ChainProviderEventId := -1;
  LOptions := DefaultGraphSolveOptions;
  LOptions.CaptureTrace := True;
  Require(AGraph.TrySolve(LOptions, ARun.Report),
    'deterministic pass pipeline did not solve');
  ValidateLayers(AGraph);
  ValidateTrace(AGraph, ARun.Report, ARun.ChainStartEventId,
    ARun.ChainProviderEventId);
  ARun.Terrain := CaptureLayer(AGraph, 0);
  ARun.Settlement := CaptureLayer(AGraph, 1);
  ARun.Foliage := CaptureLayer(AGraph, 2);
end;

function SameTraceEvent(const A, B: TGraphTraceEvent): Boolean;
begin
  Result := (A.EventId = B.EventId) and
    (A.CauseEventId = B.CauseEventId) and (A.Kind = B.Kind) and
    (A.CauseKind = B.CauseKind) and (A.PassIndex = B.PassIndex) and
    (A.EntryIndex = B.EntryIndex) and (A.ValueIndex = B.ValueIndex) and
    (A.Value = B.Value) and (A.NeighborIndex = B.NeighborIndex) and
    (A.HasDirection = B.HasDirection) and (A.Direction = B.Direction) and
    (A.DependencyPassIndex = B.DependencyPassIndex) and
    (A.DecisionDepth = B.DecisionDepth) and
    (A.DomainCountBefore = B.DomainCountBefore) and
    (A.DomainCountAfter = B.DomainCountAfter);
end;

procedure RequireDeterministicReplay(const A, B: TInspectorRun);
var
  I: Integer;
begin
  Require((A.Terrain = B.Terrain) and
    (A.Settlement = B.Settlement) and (A.Foliage = B.Foliage),
    'same-seed layer output changed');
  Require(A.Report.TraceHash = B.Report.TraceHash,
    'same-seed trace hash changed');
  Require(Length(A.Report.Trace) = Length(B.Report.Trace),
    'same-seed trace length changed');
  for I := 0 to High(A.Report.Trace) do
    Require(SameTraceEvent(A.Report.Trace[I], B.Report.Trace[I]),
      'same-seed trace event changed at index ' + IntToStr(I));
  Require((A.ChainStartEventId = B.ChainStartEventId) and
    (A.ChainProviderEventId = B.ChainProviderEventId),
    'same-seed causal chain changed');
end;

procedure PrintPassSummaries(const AGraph: TGraph;
  const AReport: TGraphSolveReport);
var
  I: Integer;
  LPass: TGraphPassSolveReport;
  LTraceRange: String;
begin
  WriteLn('Pass summaries:');
  for I := 0 to High(AReport.Passes) do
  begin
    LPass := AReport.Passes[I];
    if LPass.TraceCount = 0 then
      LTraceRange := 'none'
    else
      LTraceRange := IntToStr(LPass.TraceStart) + '..' +
        IntToStr(LPass.TraceStart + LPass.TraceCount - 1);
    WriteLn('  pass=', I, ' label=', AGraph.PassGraph[I].CurrentPass,
      ' executed=', BooleanName(LPass.Executed),
      ' ordinal=', LPass.ExecutionOrdinal,
      ' disposition=', PassDispositionName(LPass.Disposition),
      ' decisions=', LPass.Decisions,
      ' propagations=', LPass.Propagations,
      ' contradictions=', LPass.Contradictions,
      ' backtracks=', LPass.Backtracks,
      ' trace=', LTraceRange);
  end;
end;

procedure PrintChronologicalTrace(const AReport: TGraphSolveReport);
var
  I: Integer;
begin
  WriteLn('Chronological events (', Length(AReport.Trace), '):');
  for I := 0 to High(AReport.Trace) do
    WriteLn('  ', FormatGraphTraceEvent(AReport.Trace[I]));
end;

procedure PrintCausalChain(const AGraph: TGraph;
  const ARun: TInspectorRun);
var
  I: Integer;
  LCurrent: TGraphTraceEvent;
  LProviderPass: Integer;
begin
  LCurrent := FindGraphTraceEvent(ARun.Report.Trace,
    ARun.ChainStartEventId);
  LProviderPass := LCurrent.DependencyPassIndex;
  WriteLn('Backward causal chain:');
  for I := 0 to Length(ARun.Report.Trace) do
  begin
    WriteLn('  ', FormatGraphTraceEvent(LCurrent));
    if LCurrent.EventId = ARun.ChainProviderEventId then
      Break;
    LCurrent := FindGraphTraceEvent(ARun.Report.Trace,
      LCurrent.CauseEventId);
  end;
  WriteLn('  provider reached: pass=', LProviderPass,
    ' label=', AGraph.PassGraph[LProviderPass].CurrentPass,
    ' event=', ARun.ChainProviderEventId);
end;

procedure RunTraceInspector;
var
  LGraph: TGraph;
  LReplayGraph: TGraph;
  LRun: TInspectorRun;
  LReplay: TInspectorRun;
begin
  LGraph := TGraph.Create;
  LReplayGraph := TGraph.Create;
  try
    ConfigureGraph(LGraph);
    ConfigureGraph(LReplayGraph);
    SolveInspectorGraph(LGraph, LRun);
    SolveInspectorGraph(LReplayGraph, LReplay);
    RequireDeterministicReplay(LRun, LReplay);

    WriteLn('TraceInspector: terrain -> settlement -> foliage');
    WriteLn('Seed: ', DEMO_SEED);
    WriteLn('Trace versions: schema=', WFC_TRACE_VERSION,
      ' hash=', WFC_TRACE_HASH_VERSION,
      ' utility=', WFC_TRACE_UTILITY_VERSION);
    WriteLn('Portable trace hash: ',
      GraphTraceSignatureHex(LRun.Report.TraceHash));
    WriteLn('Layers:');
    WriteLn('  terrain:    ', LRun.Terrain);
    WriteLn('  settlement: ', LRun.Settlement);
    WriteLn('  foliage:    ', LRun.Foliage);
    PrintPassSummaries(LGraph, LRun.Report);
    PrintChronologicalTrace(LRun.Report);
    PrintCausalChain(LGraph, LRun);
    WriteLn('Trace validation: checked=', Length(LRun.Report.Trace),
      ' valid=true');
    WriteLn('Deterministic replay: identical layers and event stream');
    WriteLn('Self-check: passed');
  finally
    LReplayGraph.Free;
    LGraph.Free;
  end;
end;

end.
