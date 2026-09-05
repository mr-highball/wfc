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
  wfc_trace,
  wfc_trace_stream;

const
  DEMO_WIDTH = 4;
  DEMO_SEED = TGraphSeed(0);
  EXPECTED_TRACE_HASH = TGraphTraceSignature($73C4B9A2);
  LATE_FAILURE_SEED = TGraphSeed(55);
  EXPECTED_LATE_FAILURE_HASH = TGraphTraceSignature($B27D0AE0);
  STREAM_WINDOW_CAPACITY = 5;

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

  TInspectorLiveSink = class(TGraphTraceWindowSink)
  public
    Graph: TGraph;
    procedure AppendEvent(const AEvent: TGraphTraceEvent); override;
  end;

  TInspectorRejectingGraph = class(TGraph)
  protected
    function DoValidateCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean; override;
  end;

  TInspectorRun = record
    Terrain: String;
    Settlement: String;
    Foliage: String;
    Report: TGraphSolveReport;
    Layout: TGraphTraceLayout;
    ChainStartEventId: Integer;
    ChainProviderEventId: Integer;
  end;

procedure Require(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise ETraceInspector.Create(AMessage);
end;

procedure TInspectorLiveSink.AppendEvent(const AEvent: TGraphTraceEvent);
begin
  inherited AppendEvent(AEvent);
  Require(Assigned(Graph) and Graph.Running,
    'stream callback was not delivered during the guarded transaction');
  //This is called while the solver is working, not by replaying Report.Trace.
  //The inherited sink retains only the newest configured number of events.
  WriteLn('  live ', FormatGraphTraceEvent(AEvent));
end;

function TInspectorRejectingGraph.DoValidateCommit(
  out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
begin
  AFailedPassIndex := 0;
  AFailedEntryIndex := 0;
  Result := False;
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
  const AReport: TGraphSolveReport; out ALayout: TGraphTraceLayout;
  out AChainStartEventId, AChainProviderEventId: Integer);
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
  Require(TryBuildGraphTraceLayout(AGraph, AReport, ALayout,
      LValidation),
    DescribeGraphTraceValidationIssue(LValidation.Issue));
  Require(ValidateGraphTraceLayout(AGraph, AReport, ALayout,
      LValidation),
    'derived trace layout: ' +
      DescribeGraphTraceValidationIssue(LValidation.Issue));
  Require(LValidation.CheckedEvents = Length(AReport.Trace),
    'trace validator did not inspect every event');
  Require((ALayout.Version = WFC_TRACE_LAYOUT_VERSION) and
      ALayout.TraceCaptured and
      (ALayout.TraceHash = AReport.TraceHash) and
      (ALayout.EventCount = Length(AReport.Trace)) and
      (ALayout.TerminalEventIndex = High(AReport.Trace)) and
      (Length(ALayout.Passes) = Length(AReport.Passes)),
    'derived trace layout does not describe the captured report');
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
  ValidateTrace(AGraph, ARun.Report, ARun.Layout,
    ARun.ChainStartEventId, ARun.ChainProviderEventId);
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
    (A.DomainCountAfter = B.DomainCountAfter) and
    (A.ConstraintIndex = B.ConstraintIndex);
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

function TraceRangesText(const ARanges: TGraphTraceRanges): String;
var
  I: Integer;
begin
  if Length(ARanges) = 0 then
    Exit('none');
  Result := '';
  for I := 0 to High(ARanges) do
  begin
    if I <> 0 then
      Result := Result + ',';
    Result := Result + IntToStr(ARanges[I].Start) + '..' +
      IntToStr(ARanges[I].Start + ARanges[I].Count - 1);
  end;
end;

procedure PrintPassSummaries(const AGraph: TGraph;
  const AReport: TGraphSolveReport; const ALayout: TGraphTraceLayout);
var
  I: Integer;
  LPass: TGraphPassSolveReport;
begin
  Require(Length(ALayout.Passes) = Length(AReport.Passes),
    'trace layout pass count changed before inspection');
  WriteLn('Pass summaries:');
  for I := 0 to High(AReport.Passes) do
  begin
    LPass := AReport.Passes[I];
    WriteLn('  pass=', I, ' label=', AGraph.PassGraph[I].CurrentPass,
      ' executed=', BooleanName(LPass.Executed),
      ' ordinal=', LPass.ExecutionOrdinal,
      ' disposition=', PassDispositionName(LPass.Disposition),
      ' decisions=', LPass.Decisions,
      ' propagations=', LPass.Propagations,
      ' contradictions=', LPass.Contradictions,
      ' backtracks=', LPass.Backtracks,
      ' trace-ranges=', TraceRangesText(ALayout.Passes[I].Ranges));
  end;
end;

procedure RunLateCommitLayoutExample;
var
  LGraph: TInspectorRejectingGraph;
  LLayout: TGraphTraceLayout;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LValidation: TGraphTraceValidationReport;
begin
  LGraph := TInspectorRejectingGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.Seed := LATE_FAILURE_SEED;
    LGraph.CurrentPass := 'provider';
    LGraph.AddValue('A');
    LGraph.SwitchToPass('consumer');
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('B');

    LOptions := DefaultGraphSolveOptions;
    LOptions.CaptureTrace := True;
    Require(not LGraph.TrySolve(LOptions, LReport),
      'late commit validator unexpectedly accepted the transaction');
    Require((LReport.Status = gssContradiction) and
        (LReport.Contradiction.Kind = gckFinalValidation) and
        (LReport.FailedPassIndex = 0),
      'late commit rejection classification changed');
    Require(LReport.TraceHash = EXPECTED_LATE_FAILURE_HASH,
      'late commit rejection trace hash changed');
    Require(not ValidateGraphTrace(LGraph, LReport, LValidation),
      'legacy slice validator unexpectedly accepted a split pass');
    Require(LValidation.Issue.Kind = gtvikPassSlice,
      'legacy validator rejected the late failure for an unexpected reason');
    Require(TryBuildGraphTraceLayout(LGraph, LReport, LLayout,
        LValidation),
      'layout rejected a real late commit failure: ' +
        DescribeGraphTraceValidationIssue(LValidation.Issue));
    Require(ValidateGraphTraceLayout(LGraph, LReport, LLayout,
        LValidation),
      'derived late-failure layout did not revalidate: ' +
        DescribeGraphTraceValidationIssue(LValidation.Issue));
    Require((LLayout.EventCount = 7) and
        (LLayout.TerminalEventIndex = 6) and
        (Length(LLayout.Passes) = 2) and
        (Length(LLayout.Passes[0].Ranges) = 2) and
        (LLayout.Passes[0].Ranges[0].Start = 0) and
        (LLayout.Passes[0].Ranges[0].Count = 2) and
        (LLayout.Passes[0].Ranges[1].Start = 4) and
        (LLayout.Passes[0].Ranges[1].Count = 2) and
        (Length(LLayout.Passes[1].Ranges) = 1) and
        (LLayout.Passes[1].Ranges[0].Start = 2) and
        (LLayout.Passes[1].Ranges[0].Count = 2),
      'late commit rejection ranges changed');
    Require(LGraph.PassGraph[0].Entry[0, 0, 0].Empty and
        not LGraph.PassGraph[0].Entry[0, 0, 0].Generated and
        LGraph.PassGraph[1].Entry[0, 0, 0].Empty and
        not LGraph.PassGraph[1].Entry[0, 0, 0].Generated,
      'late commit rejection did not roll back both passes');

    WriteLn('Late commit rejection:');
    WriteLn('  seed=', LATE_FAILURE_SEED,
      ' hash=', GraphTraceSignatureHex(LReport.TraceHash),
      ' failed-pass=', LReport.FailedPassIndex,
      ' rollback=true');
    WriteLn('  legacy-validator=rejected');
    WriteLn('  pass=0 label=provider trace-ranges=',
      TraceRangesText(LLayout.Passes[0].Ranges));
    WriteLn('  pass=1 label=consumer trace-ranges=',
      TraceRangesText(LLayout.Passes[1].Ranges));
  finally
    LGraph.Free;
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

procedure RunStreamingWindowExample(const AOracle: TInspectorRun);
var
  LGraph: TGraph;
  LSink: TInspectorLiveSink;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LEvents: TGraphTraceEvents;
  I, LFirstEventId, LOutsideCauses: Integer;
begin
  Require((Length(AOracle.Report.Trace) = 27) and
      (AOracle.Report.TraceHash = EXPECTED_TRACE_HASH),
    'the small full-trace comparison fixture changed');
  LGraph := TGraph.Create;
  LSink := nil;
  try
    ConfigureGraph(LGraph);
    LSink := TInspectorLiveSink.Create(STREAM_WINDOW_CAPACITY);
    LSink.Graph := LGraph;
    LGraph.TraceSink := LSink;
    LOptions := DefaultGraphSolveOptions;
    LOptions.CaptureTrace := False;

    WriteLn('Live stream: terrain -> settlement -> foliage');
    WriteLn('  capture=false window-capacity=', LSink.Capacity,
      ' (synchronous callbacks, not an interactive stepper)');
    Require(LGraph.TrySolve(LOptions, LReport),
      'streamed pass pipeline did not solve');
    ValidateLayers(LGraph);
    Require((CaptureLayer(LGraph, 0) = AOracle.Terrain) and
        (CaptureLayer(LGraph, 1) = AOracle.Settlement) and
        (CaptureLayer(LGraph, 2) = AOracle.Foliage),
      'stream observation changed a solved layer');
    Require(not LReport.TraceCaptured and (Length(LReport.Trace) = 0) and
        (LReport.TraceHash = 0),
      'streamed attempt unexpectedly retained legacy full-trace history');
    for I := 0 to High(LReport.Passes) do
      Require((LReport.Passes[I].TraceStart = -1) and
          (LReport.Passes[I].TraceCount = 0),
        'capture-disabled stream unexpectedly populated a legacy pass slice');
    Require((LReport.TraceDelivery.Status = gtdsComplete) and
        (LReport.TraceDelivery.ProducedEventCount = 27) and
        (LReport.TraceDelivery.DeliveredEventCount = 27) and
        (LReport.TraceDelivery.TraceHash = AOracle.Report.TraceHash),
      'stream production did not match the full-trace oracle');
    Require((LSink.Header.Seed = DEMO_SEED) and (LSink.Header.PassCount = 3) and
        (LSink.Header.TraceVersion = WFC_TRACE_VERSION) and
        (LSink.Header.TraceHashVersion = WFC_TRACE_HASH_VERSION),
      'stream header did not identify the actual transaction');
    Require((LSink.Delivery.Status = gtdsComplete) and
        (LSink.Delivery.ProducedEventCount = 27) and
        (LSink.Delivery.DeliveredEventCount = 27) and
        (LSink.Delivery.TraceHash = AOracle.Report.TraceHash),
      'the bounded sink did not receive the complete delivery summary');
    Require((LSink.RetainedEventCount = STREAM_WINDOW_CAPACITY) and
        (LSink.DroppedEventCount = 22) and not LSink.Complete,
      'the bounded suffix was mislabeled as a complete retained trace');
    LEvents := LSink.CopyEvents;
    Require(Length(LEvents) = STREAM_WINDOW_CAPACITY,
      'copied window length differs from retained event count');
    LFirstEventId := 22;
    for I := 0 to High(LEvents) do
    begin
      Require(LEvents[I].EventId = LFirstEventId + I,
        'window rewrote an original chronological event ID');
      Require(SameTraceEvent(LEvents[I], AOracle.Report.Trace[LFirstEventId + I]),
        'window altered a retained event or its original cause');
    end;

    WriteLn('Stream summary: produced=', LReport.TraceDelivery.ProducedEventCount,
      ' delivered=', LReport.TraceDelivery.DeliveredEventCount,
      ' hash=', GraphTraceSignatureHex(LReport.TraceDelivery.TraceHash));
    WriteLn('  report-trace-events=', Length(LReport.Trace),
      ' report-trace-hash=', GraphTraceSignatureHex(LReport.TraceHash),
      ' layers-match=true');
    WriteLn('  retained=', LSink.RetainedEventCount,
      ' dropped=', LSink.DroppedEventCount,
      ' delivery-complete=true window-complete=', BooleanName(LSink.Complete));
    WriteLn('Retained suffix: original event IDs 22..26');
    LOutsideCauses := 0;
    for I := 0 to High(LEvents) do
    begin
      WriteLn('  ', FormatGraphTraceEvent(LEvents[I]));
      if LEvents[I].CauseEventId < 0 then
        WriteLn('    cause=external-model-input')
      else if LEvents[I].CauseEventId < LFirstEventId then
      begin
        Inc(LOutsideCauses);
        WriteLn('    cause=', LEvents[I].CauseEventId,
          ' outside-window (not retained; original ID preserved)');
      end
      else
        WriteLn('    cause=', LEvents[I].CauseEventId, ' retained');
    end;
    Require(LOutsideCauses > 0,
      'the streaming fixture no longer demonstrates an outside-window cause');
    WriteLn('  The earlier full 27-event fixture is a comparison oracle;');
    WriteLn('  this streamed attempt retains only its newest five events.');
  finally
    //The graph borrows its sink. Detach before freeing the caller-owned sink.
    LGraph.TraceSink := nil;
    LSink.Free;
    LGraph.Free;
  end;
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
      ' utility=', WFC_TRACE_UTILITY_VERSION,
      ' layout=', WFC_TRACE_LAYOUT_VERSION);
    WriteLn('Portable trace hash: ',
      GraphTraceSignatureHex(LRun.Report.TraceHash));
    WriteLn('Layers:');
    WriteLn('  terrain:    ', LRun.Terrain);
    WriteLn('  settlement: ', LRun.Settlement);
    WriteLn('  foliage:    ', LRun.Foliage);
    PrintPassSummaries(LGraph, LRun.Report, LRun.Layout);
    PrintChronologicalTrace(LRun.Report);
    PrintCausalChain(LGraph, LRun);
    WriteLn('Trace validation: checked=', Length(LRun.Report.Trace),
      ' valid=true');
    WriteLn('Deterministic replay: identical layers and event stream');
    RunLateCommitLayoutExample;
    RunStreamingWindowExample(LRun);
    WriteLn('Self-check: passed');
  finally
    LReplayGraph.Free;
    LGraph.Free;
  end;
end;

end.
