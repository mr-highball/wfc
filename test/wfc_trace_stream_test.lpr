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
program wfc_trace_stream_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_trace;

type
  TFixtureKind = (fkChoices, fkDependency, fkEscapeRing, fkConnectivity,
    fkCopy, fkCopyFailure, fkCallerCopyFailure, fkEmpty, fkZeroCells,
    fkRejectEarly, fkRejectLate, fkNegotiation, fkNegotiationRing);

  TReceivedTrace = record
    Header: TGraphTraceHeader;
    Events: TGraphTraceEvents;
    Delivery: TGraphTraceDelivery;
    EndCalled: Boolean;
    CallbackFailed: Boolean;
  end;
  TReceivedTraces = array of TReceivedTrace;

  TCollectingSink = class(TGraphTraceSink)
  public
    Graph: TGraph;
    Traces: TReceivedTraces;
    ThrowPhase: TGraphTraceDeliveryPhase;
    ThrowEvent: Integer;
    BeginCalls, EventCalls, EndCalls: Integer;
    GuardMutation: Boolean;
    procedure BeginTrace(const AHeader: TGraphTraceHeader); override;
    procedure AppendEvent(const AEvent: TGraphTraceEvent); override;
    procedure EndTrace(const ADelivery: TGraphTraceDelivery); override;
    destructor Destroy; override;
  end;

  TRejectingGraph = class(TGraph)
  protected
    function DoValidateCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean; override;
  public
    RejectPass: Integer;
    HookCalls: Integer;
    Sink: TCollectingSink;
    RaiseInHook: Boolean;
  end;

var
  Checks, Failures, DestroyedSinks: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if ACondition then Exit;
  Inc(Failures);
  WriteLn('FAIL: ', AMessage);
end;

procedure TCollectingSink.BeginTrace(const AHeader: TGraphTraceHeader);
var I: Integer;
begin
  Inc(BeginCalls);
  I := Length(Traces);
  if I > 0 then
    Check(Traces[I - 1].EndCalled or Traces[I - 1].CallbackFailed,
      'begin never overlaps an unclosed successful transaction');
  SetLength(Traces, I + 1);
  Traces[I] := Default(TReceivedTrace);
  Traces[I].Header := AHeader;
  Check(Assigned(Graph) and Graph.Running, 'begin is delivered during guarded solve');
  if GuardMutation then
  begin
    Traces[I].CallbackFailed := True;
    Graph.TraceSink := nil;
  end;
  if ThrowPhase = gtdpBegin then
  begin
    Traces[I].CallbackFailed := True;
    raise Exception.Create('sink begin failure');
  end;
end;

procedure TCollectingSink.AppendEvent(const AEvent: TGraphTraceEvent);
var I, J: Integer;
begin
  Inc(EventCalls);
  I := High(Traces);
  Check(I >= 0, 'an event has a preceding begin');
  if I < 0 then Exit;
  Check(not Traces[I].EndCalled, 'an event precedes its end');
  Check(Graph.Running, 'event is delivered during guarded solve');
  if (ThrowPhase = gtdpEvent) and (AEvent.EventId = ThrowEvent) then
  begin
    Traces[I].CallbackFailed := True;
    raise Exception.Create('sink event failure');
  end;
  J := Length(Traces[I].Events);
  Check(AEvent.EventId = J, 'event IDs form a contiguous accepted prefix');
  Check((AEvent.CauseEventId = -1) or ((AEvent.CauseEventId >= 0)
    and (AEvent.CauseEventId < J)), 'stream causes refer only backward');
  SetLength(Traces[I].Events, J + 1);
  Traces[I].Events[J] := AEvent;
end;

procedure TCollectingSink.EndTrace(const ADelivery: TGraphTraceDelivery);
var I: Integer;
begin
  Inc(EndCalls);
  I := High(Traces);
  Check(I >= 0, 'end has a preceding begin');
  if I < 0 then Exit;
  Check(not Traces[I].EndCalled, 'a transaction ends exactly once');
  Check(Graph.Running, 'end remains inside the running guard');
  Traces[I].EndCalled := True;
  Traces[I].Delivery := ADelivery;
  if ThrowPhase = gtdpEnd then
  begin
    Traces[I].CallbackFailed := True;
    raise Exception.Create('sink end failure');
  end;
end;

destructor TCollectingSink.Destroy;
begin
  Inc(DestroyedSinks);
  inherited;
end;

function TRejectingGraph.DoValidateCommit(out AFailedPassIndex,
  AFailedEntryIndex: Integer): Boolean;
begin
  Inc(HookCalls);
  if Assigned(Sink) then
  begin
    Check((Sink.BeginCalls > 0) and (Length(Sink.Traces) > 0),
      'the actual commit hook observes a live stream');
    Check((Length(Sink.Traces[High(Sink.Traces)].Events) > 0)
      and not Sink.Traces[High(Sink.Traces)].EndCalled,
      'events are delivered before commit validation, not report replay');
  end;
  if RaiseInHook then raise Exception.Create('producer commit failure');
  AFailedPassIndex := RejectPass;
  if RejectPass < 0 then AFailedEntryIndex := -1 else AFailedEntryIndex := 0;
  Result := RejectPass < 0;
end;

function Position(const AX: Integer): TGraphPosition;
begin
  Result.X := AX; Result.Y := 0; Result.Z := 0;
end;

function Fixture(const AKind: TFixtureKind): TGraph;
var
  I: Integer;
  Terminals: TGraphPositions;
  Profiles: TGraphConnectivityValues;
begin
  if AKind in [fkRejectEarly, fkRejectLate] then
  begin
    Result := TRejectingGraph.Create;
    if AKind = fkRejectEarly then TRejectingGraph(Result).RejectPass := 0
    else TRejectingGraph(Result).RejectPass := 1;
  end
  else Result := TGraph.Create;
  try
    Result.Seed := 0;
    Result.Reshape(1, 1, 1);
    Result.WrapNeighbors := False;
    case AKind of
      fkChoices:
        begin
          Result.Seed := 17;
          Result.Reshape(4, 1, 1);
          Result.CurrentPass := 'choices';
          Result.AddValue('A'); Result.AddValue('B'); Result.AddValue('C');
        end;
      fkDependency:
        begin
          Result.Seed := 42;
          Result.CurrentPass := 'terrain'; Result.AddValue('land');
          Result.SwitchToPass('settlement'); Result.PassMode := gpmOverlay;
          Result.AddValue('none');
          Result.AddValue('home').RequireFromPass('terrain', 'water');
        end;
      fkEscapeRing:
        begin
          Result.Reshape(3, 1, 1); Result.WrapNeighbors := True;
          Result.AddValue('A').NewRule([gdEast, gdWest], 'B');
          Result.AddValue('C').NewRule([gdEast, gdWest], 'C');
        end;
      fkConnectivity:
        begin
          Result.Seed := 55; Result.Reshape(3, 1, 1);
          Result.CurrentPass := 'routes';
          Result.AddValue('none'); Result.AddValue('a'); Result.AddValue('b');
          SetLength(Terminals, 1); Terminals[0] := Position(2);
          SetLength(Profiles, 2);
          Profiles[0] := MakeGraphConnectivityValue('a', [gdEast, gdWest]);
          Profiles[1] := MakeGraphConnectivityValue('b', [gdEast, gdWest]);
          Result.RequireConnectivity(MakeGraphConnectivityConstraint(
            'route', Position(0), Terminals, Profiles));
        end;
      fkCopy, fkCopyFailure, fkCallerCopyFailure:
        begin
          Result.CurrentPass := 'source'; Result.AddValue('A');
          Result.SwitchToPass('copy');
          if AKind = fkCallerCopyFailure then Result.Entry[0, 0, 0].Value := 'B';
          if AKind <> fkCopy then Result.SetAllowedValues(0, 0, 0, []);
        end;
      fkEmpty: Result.CurrentPass := 'unconfigured';
      fkZeroCells:
        begin
          Result.Reshape(0, 1, High(TGraphCoordinate)); Result.AddValue('A');
        end;
      fkRejectEarly, fkRejectLate:
        begin
          Result.Seed := 55; Result.CurrentPass := 'first';
          Result.AddValue('A'); Result.SwitchToPass('second');
          Result.PassMode := gpmOverlay; Result.AddValue('B');
        end;
      fkNegotiation, fkNegotiationRing:
        begin
          if AKind = fkNegotiationRing then Result.Reshape(3, 1, 1);
          Result.CurrentPass := 'terrain'; Result.PassMode := gpmOverlay;
          Result.AddValue('marsh'); Result.AddValue('meadow');
          if AKind = fkNegotiationRing then
            for I := 1 to 2 do Result.Entry[I, 0, 0].Value := 'meadow';
          Result.SwitchToPass('housing'); Result.PassMode := gpmOverlay;
          Result.ClearDependencies;
          Result.AddValue('cottage').RequireFromPass('terrain', 'meadow');
          if AKind = fkNegotiationRing then
          begin
            Result.SwitchToPass('ring'); Result.PassMode := gpmOverlay;
            Result.ClearDependencies; Result.WrapNeighbors := True;
            Result.AddValue('A').NewRule([gdEast, gdWest], 'B');
            Result.AddValue('C').NewRule([gdEast, gdWest], 'C');
          end;
        end;
    end;
  except Result.Free; raise; end;
end;

function SameEvent(const A, B: TGraphTraceEvent): Boolean;
begin
  Result := (A.EventId = B.EventId) and (A.CauseEventId = B.CauseEventId)
    and (A.Kind = B.Kind) and (A.CauseKind = B.CauseKind)
    and (A.PassIndex = B.PassIndex) and (A.EntryIndex = B.EntryIndex)
    and (A.ValueIndex = B.ValueIndex) and (A.Value = B.Value)
    and (A.NeighborIndex = B.NeighborIndex) and (A.HasDirection = B.HasDirection)
    and (A.Direction = B.Direction)
    and (A.DependencyPassIndex = B.DependencyPassIndex)
    and (A.DecisionDepth = B.DecisionDepth)
    and (A.DomainCountBefore = B.DomainCountBefore)
    and (A.DomainCountAfter = B.DomainCountAfter)
    and (A.ConstraintIndex = B.ConstraintIndex);
end;

function OutputState(const AGraph: TGraph): String;
var
  P, X, Y, Z: Integer;
  E: TGraphEntry;
begin
  Result := IntToStr(AGraph.CurrentPassIndex) + ':';
  //Zero volume need not imply that the other dimensions are small.
  if (AGraph.Dimension.Width = 0) or (AGraph.Dimension.Height = 0)
    or (AGraph.Dimension.Depth = 0) then Exit;
  for P := 0 to AGraph.TotalPassCount - 1 do
    for Z := 0 to Integer(AGraph.Dimension.Depth) - 1 do
      for Y := 0 to Integer(AGraph.Dimension.Height) - 1 do
        for X := 0 to Integer(AGraph.Dimension.Width) - 1 do
        begin
          E := AGraph.PassGraph[P].Entry[X, Y, Z];
          Result := Result + IntToStr(P) + ':' + IntToStr(Length(E.Value))
            + ':' + E.Value + ':' + IntToStr(Ord(E.Empty)) + ':'
            + IntToStr(Ord(E.Generated)) + ';';
        end;
end;

procedure CheckStreams(const A, B: TGraph; const AContext: String);
var I, J: Integer;
begin
  Check(A.TotalPassCount = B.TotalPassCount, AContext + ' pass count');
  for I := 0 to A.TotalPassCount - 1 do
    for J := 1 to 5 do
      Check(A.PassGraph[I].RandomIndex(1000003) =
        B.PassGraph[I].RandomIndex(1000003), AContext + ' pass RNG');
  for J := 1 to 5 do
    Check(A.RandomIndex(1000003) = B.RandomIndex(1000003), AContext + ' root RNG');
end;

procedure CheckReport(const A, B: TGraphSolveReport; const AContext: String);
var I: Integer;
begin
  Check((A.Status = B.Status) and (A.Seed = B.Seed)
    and (A.RandomAlgorithmVersion = B.RandomAlgorithmVersion)
    and (A.SolverAlgorithmVersion = B.SolverAlgorithmVersion)
    and (A.GraphModelVersion = B.GraphModelVersion)
    and (A.PipelineAlgorithmVersion = B.PipelineAlgorithmVersion)
    and (A.FailedPassIndex = B.FailedPassIndex), AContext + ' report header');
  Check((A.Contradiction.Kind = B.Contradiction.Kind)
    and (A.Contradiction.PassIndex = B.Contradiction.PassIndex)
    and (A.Contradiction.EntryIndex = B.Contradiction.EntryIndex)
    and (A.Contradiction.NeighborIndex = B.Contradiction.NeighborIndex)
    and (A.Contradiction.HasDirection = B.Contradiction.HasDirection)
    and (A.Contradiction.Direction = B.Contradiction.Direction)
    and (A.Contradiction.DependencyPassIndex = B.Contradiction.DependencyPassIndex)
    and (A.Contradiction.ConstraintIndex = B.Contradiction.ConstraintIndex),
    AContext + ' contradiction');
  Check(Length(A.ExecutionOrder) = Length(B.ExecutionOrder), AContext + ' order length');
  if Length(A.ExecutionOrder) = Length(B.ExecutionOrder) then
    for I := 0 to High(A.ExecutionOrder) do
      Check(A.ExecutionOrder[I] = B.ExecutionOrder[I], AContext + ' execution order');
  Check(Length(A.Passes) = Length(B.Passes), AContext + ' pass length');
  if Length(A.Passes) = Length(B.Passes) then
    for I := 0 to High(A.Passes) do
      Check((A.Passes[I].Decisions = B.Passes[I].Decisions)
        and (A.Passes[I].Propagations = B.Passes[I].Propagations)
        and (A.Passes[I].Contradictions = B.Passes[I].Contradictions)
        and (A.Passes[I].Backtracks = B.Passes[I].Backtracks)
        and (A.Passes[I].ExcludedAssignments = B.Passes[I].ExcludedAssignments)
        and (A.Passes[I].Executed = B.Passes[I].Executed)
        and (A.Passes[I].ExecutionOrdinal = B.Passes[I].ExecutionOrdinal)
        and (A.Passes[I].Disposition = B.Passes[I].Disposition),
        AContext + ' pass counters');
end;

function CountKind(const AEvents: TGraphTraceEvents;
  const AKind: TGraphTraceEventKind): Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to High(AEvents) do if AEvents[I].Kind = AKind then Inc(Result);
end;

procedure CheckDisabledDelivery(const AReport: TGraphSolveReport;
  const AContext: String);
begin
  Check((AReport.TraceDelivery.Version = WFC_TRACE_DELIVERY_VERSION)
    and (AReport.TraceDelivery.Status = gtdsDisabled)
    and (AReport.TraceDelivery.ProducedEventCount = 0)
    and (AReport.TraceDelivery.DeliveredEventCount = 0)
    and (AReport.TraceDelivery.TraceHash = 0)
    and (AReport.TraceDelivery.FailurePhase = gtdpNone)
    and (AReport.TraceDelivery.FailureEventId = -1)
    and (AReport.TraceDelivery.FailureMessage = ''), AContext + ' disabled delivery');
end;

procedure CheckLegacyCapture(const AReport, AOracle: TGraphSolveReport;
  const ACapture: Boolean; const AContext: String);
var I: Integer;
begin
  Check(AReport.TraceCaptured = ACapture, AContext + ' legacy capture flag');
  if ACapture then
  begin
    Check((AReport.TraceHash = AOracle.TraceHash)
      and (Length(AReport.Trace) = Length(AOracle.Trace)), AContext + ' legacy trace identity');
    if Length(AReport.Trace) = Length(AOracle.Trace) then
      for I := 0 to High(AReport.Trace) do
        Check(SameEvent(AReport.Trace[I], AOracle.Trace[I]), AContext + ' legacy event');
    for I := 0 to High(AReport.Passes) do
      Check((AReport.Passes[I].TraceStart = AOracle.Passes[I].TraceStart)
        and (AReport.Passes[I].TraceCount = AOracle.Passes[I].TraceCount),
        AContext + ' legacy pass trace slice');
  end
  else
  begin
    Check((AReport.TraceHash = 0) and (Length(AReport.Trace) = 0),
      AContext + ' sink-only report retains no legacy events or hash');
    for I := 0 to High(AReport.Passes) do
      Check((AReport.Passes[I].TraceStart = -1) and (AReport.Passes[I].TraceCount = 0),
        AContext + ' sink-only pass retains disabled slice');
  end;
end;

procedure CheckReceived(const AGraph: TGraph; const AReceived: TReceivedTrace;
  const AReport, AOracle: TGraphSolveReport; const AContext: String);
var
  I: Integer;
  Rebuilt: TGraphSolveReport;
  Layout: TGraphTraceLayout;
  Validation: TGraphTraceValidationReport;
begin
  Check((AReceived.Header.DeliveryVersion = WFC_TRACE_DELIVERY_VERSION)
    and (AReceived.Header.TraceVersion = WFC_TRACE_VERSION)
    and (AReceived.Header.TraceHashVersion = WFC_TRACE_HASH_VERSION)
    and (AReceived.Header.Seed = AOracle.Seed)
    and (AReceived.Header.RandomAlgorithmVersion = AOracle.RandomAlgorithmVersion)
    and (AReceived.Header.SolverAlgorithmVersion = AOracle.SolverAlgorithmVersion)
    and (AReceived.Header.GraphModelVersion = AOracle.GraphModelVersion)
    and (AReceived.Header.PipelineAlgorithmVersion = AOracle.PipelineAlgorithmVersion)
    and (AReceived.Header.PassCount = Length(AOracle.Passes)), AContext + ' header');
  Check(AReceived.EndCalled, AContext + ' end was delivered');
  Check((AReport.TraceDelivery.Version = WFC_TRACE_DELIVERY_VERSION)
    and (AReport.TraceDelivery.Status = gtdsComplete)
    and (AReport.TraceDelivery.ProducedEventCount = Length(AOracle.Trace))
    and (AReport.TraceDelivery.DeliveredEventCount = Length(AOracle.Trace))
    and (AReport.TraceDelivery.TraceHash = AOracle.TraceHash)
    and (AReport.TraceDelivery.FailurePhase = gtdpNone)
    and (AReport.TraceDelivery.FailureEventId = -1)
    and (AReport.TraceDelivery.FailureMessage = ''), AContext + ' report delivery');
  Check((AReceived.Delivery.Version = AReport.TraceDelivery.Version)
    and (AReceived.Delivery.Status = AReport.TraceDelivery.Status)
    and (AReceived.Delivery.ProducedEventCount = AReport.TraceDelivery.ProducedEventCount)
    and (AReceived.Delivery.DeliveredEventCount = AReport.TraceDelivery.DeliveredEventCount)
    and (AReceived.Delivery.TraceHash = AReport.TraceDelivery.TraceHash)
    and (AReceived.Delivery.FailurePhase = AReport.TraceDelivery.FailurePhase)
    and (AReceived.Delivery.FailureEventId = AReport.TraceDelivery.FailureEventId)
    and (AReceived.Delivery.FailureMessage = AReport.TraceDelivery.FailureMessage),
    AContext + ' end summary matches report');
  Check(Length(AReceived.Events) = Length(AOracle.Trace), AContext + ' event count');
  if Length(AReceived.Events) = Length(AOracle.Trace) then
    for I := 0 to High(AReceived.Events) do
      Check(SameEvent(AReceived.Events[I], AOracle.Trace[I]),
        AContext + ' all event fields match ' + IntToStr(I));
  //Validate a report reconstructed only from delivered events and ordinary
  //solve metadata. Rebuild the legacy per-pass slices independently.
  Rebuilt := AReport;
  Rebuilt.Trace := CopyGraphTraceEvents(AReceived.Events);
  Rebuilt.TraceCaptured := True;
  Rebuilt.TraceHash := AReceived.Delivery.TraceHash;
  Rebuilt.Passes := Copy(AReport.Passes);
  for I := 0 to High(Rebuilt.Passes) do
  begin
    Rebuilt.Passes[I].TraceStart := -1;
    Rebuilt.Passes[I].TraceCount := 0;
  end;
  for I := 0 to High(Rebuilt.Trace) do
    if Rebuilt.Trace[I].PassIndex >= 0 then
      with Rebuilt.Passes[Rebuilt.Trace[I].PassIndex] do
      begin
        if TraceStart < 0 then TraceStart := I;
        Inc(TraceCount);
      end;
  Check(CalculateGraphTraceHash(Rebuilt) = Rebuilt.TraceHash,
    AContext + ' delivered bytes independently rehash');
  Check(TryBuildGraphTraceLayout(AGraph, Rebuilt, Layout, Validation),
    AContext + ' delivered chronology independently validates');
end;

procedure TestPair(const AKind: TFixtureKind; const ABacktracks: Integer);
var
  Graph, Oracle: TGraph;
  Sink: TCollectingSink;
  Options, Captured: TGraphSolveOptions;
  Report, Expected: TGraphSolveReport;
  CaptureMode, I, Cause: Integer;
  ActualResult, ExpectedResult: Boolean;
  Context: String;
begin
  for CaptureMode := 0 to 1 do
  begin
    Context := 'fixture ' + IntToStr(Ord(AKind)) + '/' + IntToStr(ABacktracks)
      + '/' + IntToStr(CaptureMode);
    Graph := Fixture(AKind); Oracle := Fixture(AKind);
    Sink := TCollectingSink.Create;
    try
      Sink.Graph := Graph; Graph.TraceSink := Sink;
      if Graph is TRejectingGraph then TRejectingGraph(Graph).Sink := Sink;
      Captured := DefaultGraphSolveOptions;
      Captured.CaptureTrace := True; Captured.MaxBacktracks := ABacktracks;
      Options := Captured; Options.CaptureTrace := CaptureMode = 1;
      ExpectedResult := Oracle.TrySolve(Captured, Expected);
      ActualResult := Graph.TrySolve(Options, Report);
      Check(ActualResult = ExpectedResult, Context + ' Boolean outcome');
      Check(OutputState(Graph) = OutputState(Oracle), Context + ' committed or restored entries');
      CheckReport(Report, Expected, Context);
      CheckDisabledDelivery(Expected, Context + ' no sink');
      CheckLegacyCapture(Report, Expected, Options.CaptureTrace, Context);
      Check((Sink.BeginCalls = 1) and (Sink.EndCalls = 1)
        and (Length(Sink.Traces) = 1), Context + ' one transaction');
      if Length(Sink.Traces) = 1 then
        CheckReceived(Graph, Sink.Traces[0], Report, Expected, Context);
      Check(Graph.TraceSink = Sink, Context + ' graph retains borrowed sink');
      CheckStreams(Graph, Oracle, Context);
      if AKind = fkDependency then
        Check(Report.TraceDelivery.TraceHash = TGraphTraceSignature($46715F2C),
          'stream preserves the frozen dependency Trace-v1 golden');
      if (AKind = fkEscapeRing) and (ABacktracks > 0) then
        Check((CountKind(Sink.Traces[0].Events, gtekContradiction) = 1)
          and (CountKind(Sink.Traces[0].Events, gtekBacktrack) = 1)
          and (CountKind(Sink.Traces[0].Events, gtekCandidateRestored) > 0),
          'abandoned branch restoration is genuinely delivered');
      if AKind = fkConnectivity then
      begin
        Cause := 0;
        for I := 0 to High(Sink.Traces[0].Events) do
          if Sink.Traces[0].Events[I].CauseKind = gtckConnectivity then
          begin
            Inc(Cause);
            Check(Sink.Traces[0].Events[I].ConstraintIndex = 0,
              'live connectivity causes retain descriptor ordinals');
          end;
        Check(Cause > 0, 'connectivity fixture exercises live global pruning');
      end;
    finally
      Graph.Free; Oracle.Free; Sink.Free;
    end;
  end;
end;

procedure TestFixtureMatrix;
var Kind: TFixtureKind;
begin
  for Kind := Low(TFixtureKind) to High(TFixtureKind) do TestPair(Kind, 1);
  TestPair(fkEscapeRing, 0);
end;

procedure TestSelectiveReuseAndRepeatedSolve;
var
  Graph, Oracle: TGraph;
  Sink: TCollectingSink;
  Options, Captured: TGraphSolveOptions;
  Report, Expected, Baseline: TGraphSolveReport;
  I, J, Cause, Removals: Integer;
begin
  Graph := Fixture(fkDependency); Oracle := Fixture(fkDependency);
  Sink := TCollectingSink.Create;
  try
    Options := DefaultGraphSolveOptions;
    Check(Graph.TrySolve(Options, Baseline) and Oracle.TrySolve(Options, Baseline),
      'selective stream fixtures establish matching baselines');
    Sink.Graph := Graph;
    Graph.PassGraph[0].TraceSink := Sink;
    Check((Graph.TraceSink = Sink) and (Graph.PassGraph[1].TraceSink = Sink),
      'sink property forwards through every pipeline pass');
    Captured := Options; Captured.CaptureTrace := True;
    for I := 0 to 2 do
    begin
      Check(Graph.TryRegenerateFrom('settlement', Options, Report)
        and Oracle.TryRegenerateFrom('settlement', Captured, Expected),
        'repeated selective streaming transaction succeeds');
      CheckReceived(Graph, Sink.Traces[I], Report, Expected, 'selective ' + IntToStr(I));
      CheckReport(Report, Expected, 'selective ordinary metadata');
      CheckLegacyCapture(Report, Expected, False, 'selective no capture');
      Check((Report.Passes[0].Disposition = gpdReused)
        and (CountKind(Sink.Traces[I].Events, gtekPassSkipped) = 1),
        'reused provider remains an explicit live event');
      Removals := 0;
      for J := 0 to High(Sink.Traces[I].Events) do
        if Sink.Traces[I].Events[J].CauseKind = gtckPassDependency then
        begin
          Inc(Removals);
          Cause := Sink.Traces[I].Events[J].CauseEventId;
          Check((Cause >= 0) and (Cause < J), 'reused dependency cause is backward');
          if (Cause >= 0) and (Cause < J) then
            Check(Sink.Traces[I].Events[Cause].Kind = gtekPassSkipped,
              'dependent candidate links to reused provider, not stale prior session');
        end;
      Check(Removals > 0, 'selective fixture exercises reused-provider cause refinement');
    end;
    Check((Sink.BeginCalls = 3) and (Sink.EndCalls = 3),
      'repeated solves retain separate complete transaction boundaries');
    CheckStreams(Graph, Oracle, 'selective repeated RNG');
    Graph.TraceSink := nil;
    Check(Graph.TrySolve(Options, Report), 'ordinary solve after detaching succeeds');
    CheckDisabledDelivery(Report, 'detached graph');
    Check(Sink.BeginCalls = 3, 'detached sink receives no later callbacks');
  finally Graph.Free; Oracle.Free; Sink.Free; end;
end;

procedure TestSinkFailure(const APhase: TGraphTraceDeliveryPhase;
  const AEventPosition: Integer; const ACapture: Boolean);
var
  Graph, Oracle: TGraph;
  Sink: TCollectingSink;
  Options, Captured: TGraphSolveOptions;
  Report, Expected: TGraphSolveReport;
  EventId, Delivered, EventCalls, I: Integer;
  Context: String;
begin
  Graph := Fixture(fkChoices); Oracle := Fixture(fkChoices);
  Sink := TCollectingSink.Create;
  try
    Context := 'sink failure ' + IntToStr(Ord(APhase)) + '/'
      + IntToStr(AEventPosition) + '/' + IntToStr(Ord(ACapture));
    Captured := DefaultGraphSolveOptions; Captured.CaptureTrace := True;
    Check(Oracle.TrySolve(Captured, Expected), Context + ' independent baseline');
    Sink.Graph := Graph; Sink.ThrowPhase := APhase;
    if AEventPosition < 0 then Sink.ThrowEvent := High(Expected.Trace)
    else Sink.ThrowEvent := AEventPosition;
    Graph.TraceSink := Sink; Options := Captured; Options.CaptureTrace := ACapture;
    Check(Graph.TrySolve(Options, Report), Context + ' observer exception does not fail solve');
    Check(OutputState(Graph) = OutputState(Oracle), Context + ' same composition');
    CheckReport(Report, Expected, Context);
    CheckLegacyCapture(Report, Expected, ACapture, Context);
    CheckStreams(Graph, Oracle, Context);
    EventId := -1;
    if APhase = gtdpBegin then begin Delivered := 0; EventCalls := 0; end
    else if APhase = gtdpEvent then
    begin
      EventId := Sink.ThrowEvent; Delivered := EventId; EventCalls := EventId + 1;
    end
    else begin Delivered := Length(Expected.Trace); EventCalls := Delivered; end;
    Check((Report.TraceDelivery.Status = gtdsSinkFailed)
      and (Report.TraceDelivery.FailurePhase = APhase)
      and (Report.TraceDelivery.FailureEventId = EventId)
      and (Report.TraceDelivery.FailureMessage <> '')
      and (Report.TraceDelivery.ProducedEventCount = Length(Expected.Trace))
      and (Report.TraceDelivery.DeliveredEventCount = Delivered)
      and (Report.TraceDelivery.TraceHash = Expected.TraceHash),
      Context + ' exact failed prefix and complete production identity');
    Check((Sink.BeginCalls = 1) and (Sink.EventCalls = EventCalls)
      and (Sink.EndCalls = Ord(APhase = gtdpEnd)), Context + ' no callbacks after failure');
    Check(Length(Sink.Traces[0].Events) = Delivered, Context + ' stored prefix');
    for I := 0 to Delivered - 1 do
      Check(SameEvent(Sink.Traces[0].Events[I], Expected.Trace[I]), Context + ' prefix event');
    Check(Graph.TraceSink = Sink, Context + ' borrowed attachment survives failed transaction');
    Sink.ThrowPhase := gtdpNone;
    Check(Graph.TrySolve(Options, Report), Context + ' next transaction retries observer');
    CheckReceived(Graph, Sink.Traces[1], Report, Expected, Context + ' recovered');
  finally Graph.Free; Oracle.Free; Sink.Free; end;
end;

procedure TestObserverFailures;
var Mode: Integer;
begin
  for Mode := 0 to 1 do
  begin
    TestSinkFailure(gtdpBegin, 0, Mode = 1);
    TestSinkFailure(gtdpEvent, 0, Mode = 1);
    TestSinkFailure(gtdpEvent, 2, Mode = 1);
    TestSinkFailure(gtdpEvent, -1, Mode = 1);
    TestSinkFailure(gtdpEnd, 0, Mode = 1);
  end;
end;

procedure CheckNegotiation(const AGraph: TGraph; const ASink: TCollectingSink;
  const AReport, AOracle: TGraphNegotiationReport; var AIndex: Integer;
  const ACapture: Boolean; const AContext: String);
var I: Integer;
begin
  Check((AReport.Status = AOracle.Status) and (AReport.PassBacktracks = AOracle.PassBacktracks)
    and (Length(AReport.Attempts) = Length(AOracle.Attempts)), AContext + ' negotiation outcome');
  if Length(AReport.Attempts) <> Length(AOracle.Attempts) then Exit;
  for I := 0 to High(AReport.Attempts) do
  begin
    CheckReceived(AGraph, ASink.Traces[AIndex], AReport.Attempts[I].SolveReport,
      AOracle.Attempts[I].SolveReport, AContext + ' rejected round ' + IntToStr(I));
    CheckReport(AReport.Attempts[I].SolveReport, AOracle.Attempts[I].SolveReport,
      AContext + ' rejected counters');
    CheckLegacyCapture(AReport.Attempts[I].SolveReport, AOracle.Attempts[I].SolveReport,
      ACapture, AContext + ' rejected legacy');
    Inc(AIndex);
  end;
  CheckReceived(AGraph, ASink.Traces[AIndex], AReport.FinalReport, AOracle.FinalReport,
    AContext + ' final round');
  CheckReport(AReport.FinalReport, AOracle.FinalReport, AContext + ' final counters');
  CheckLegacyCapture(AReport.FinalReport, AOracle.FinalReport, ACapture, AContext + ' final legacy');
  Inc(AIndex);
end;

procedure TestNegotiatedTransactions;
var
  Graph, Oracle, Plain: TGraph;
  Sink: TCollectingSink;
  Options, Captured: TGraphNegotiationOptions;
  Report, Expected, Silent: TGraphNegotiationReport;
  Index, CaptureMode: Integer;
begin
  for CaptureMode := 0 to 1 do
  begin
    Graph := Fixture(fkNegotiation); Oracle := Fixture(fkNegotiation);
    Plain := Fixture(fkNegotiation); Sink := TCollectingSink.Create;
    try
      Sink.Graph := Graph; Graph.TraceSink := Sink;
      Captured := DefaultGraphNegotiationOptions;
      Captured.MaxPassBacktracks := 1; Captured.SolveOptions.MaxBacktracks := 1;
      Captured.SolveOptions.CaptureTrace := True;
      Options := Captured; Options.SolveOptions.CaptureTrace := CaptureMode = 1;
      Check(Graph.TrySolveNegotiated(Options, Report)
        and Oracle.TrySolveNegotiated(Captured, Expected)
        and Plain.TrySolveNegotiated(Options, Silent), 'negotiated streaming recovers provider');
      Check((Length(Report.Attempts) = 1) and (Report.PassBacktracks = 1),
        'negotiation fixture actually rejects and reopens a provider');
      Check((Sink.BeginCalls = 2) and (Sink.EndCalls = 2),
        'each negotiated round owns a begin/end, no wrapper pseudo-trace');
      Index := 0;
      CheckNegotiation(Graph, Sink, Report, Expected, Index, CaptureMode = 1, 'negotiated');
      Check(Index = Length(Sink.Traces), 'negotiated stream covers exactly the report rounds');
      Check((Report.TranscriptHash = Silent.TranscriptHash)
        and (CalculateGraphNegotiationTranscriptHash(Options, Report) = Report.TranscriptHash),
        'stream observation does not alter negotiation transcript identity');
      Check(OutputState(Graph) = OutputState(Oracle), 'negotiated stream matches final composition');
      CheckStreams(Graph, Oracle, 'negotiated');
    finally Graph.Free; Oracle.Free; Plain.Free; Sink.Free; end;
  end;
end;

procedure TestRestartTransactions;
var
  Graph, Oracle, Plain: TGraph;
  Sink: TCollectingSink;
  Options, Captured: TGraphNegotiationOptions;
  Restarts: TGraphRestartOptions;
  Report, Expected, Silent: TGraphRestartReport;
  I, Index, Strategy, CaptureMode: Integer;
begin
  for Strategy := 0 to 1 do for CaptureMode := 0 to 1 do
  begin
    if Strategy = 0 then
    begin
      Graph := Fixture(fkEscapeRing); Oracle := Fixture(fkEscapeRing);
      Plain := Fixture(fkEscapeRing);
    end
    else
    begin
      Graph := Fixture(fkNegotiationRing); Oracle := Fixture(fkNegotiationRing);
      Plain := Fixture(fkNegotiationRing);
    end;
    Sink := TCollectingSink.Create;
    try
      Sink.Graph := Graph; Graph.TraceSink := Sink;
      Captured := DefaultGraphNegotiationOptions;
      Captured.MaxPassBacktracks := 16; Captured.SolveOptions.MaxBacktracks := 0;
      Captured.SolveOptions.CaptureTrace := True;
      Options := Captured; Options.SolveOptions.CaptureTrace := CaptureMode = 1;
      Restarts := DefaultGraphRestartOptions; Restarts.MaxRestarts := 32;
      if Strategy = 0 then
        Check(Graph.TrySolveRestarted(Options.SolveOptions, Restarts, Report)
          and Oracle.TrySolveRestarted(Captured.SolveOptions, Restarts, Expected)
          and Plain.TrySolveRestarted(Options.SolveOptions, Restarts, Silent),
          'ordinary restart stream escapes the zero-budget ring')
      else
        Check(Graph.TrySolveNegotiatedRestarted(Options, Restarts, Report)
          and Oracle.TrySolveNegotiatedRestarted(Captured, Restarts, Expected)
          and Plain.TrySolveNegotiatedRestarted(Options, Restarts, Silent),
          'nested negotiated restart stream escapes the zero-budget ring');
      Check((Report.Restarts > 0) and (Report.Restarts = Expected.Restarts)
        and (Length(Report.Attempts) = Length(Expected.Attempts)),
        'restart fixture exercises separate failed and winning attempts');
      Index := 0;
      for I := 0 to High(Report.Attempts) do
        if Strategy = 0 then
        begin
          CheckReceived(Graph, Sink.Traces[Index], Report.Attempts[I].SolveReport,
            Expected.Attempts[I].SolveReport, 'restart ' + IntToStr(I));
          CheckReport(Report.Attempts[I].SolveReport, Expected.Attempts[I].SolveReport,
            'restart counters');
          CheckLegacyCapture(Report.Attempts[I].SolveReport, Expected.Attempts[I].SolveReport,
            CaptureMode = 1, 'restart legacy');
          Inc(Index);
        end
        else
          CheckNegotiation(Graph, Sink, Report.Attempts[I].NegotiationReport,
            Expected.Attempts[I].NegotiationReport, Index, CaptureMode = 1,
            'nested restart ' + IntToStr(I));
      Check((Index = Length(Sink.Traces)) and (Sink.BeginCalls = Index)
        and (Sink.EndCalls = Index), 'restart wrappers do not add spurious stream boundaries');
      Check(Report.TranscriptHash = Silent.TranscriptHash,
        'observer metadata does not change restart transcript identity');
      if Strategy = 0 then
        Check(CalculateGraphRestartTranscriptHash(Options.SolveOptions, Restarts, Report)
          = Report.TranscriptHash, 'streamed ordinary restart transcript recomputes')
      else
        Check(CalculateGraphRestartTranscriptHash(Options, Restarts, Report)
          = Report.TranscriptHash, 'streamed negotiated restart transcript recomputes');
      Check(OutputState(Graph) = OutputState(Oracle), 'restart output matches retained-trace oracle');
      CheckStreams(Graph, Oracle, 'restart');
    finally Graph.Free; Oracle.Free; Plain.Free; Sink.Free; end;
  end;
end;

function SelectiveFixture: TGraph;
var Baseline: TGraphSolveReport;
begin
  Result := TGraph.Create;
  try
    Result.Seed := 55; Result.Reshape(1, 1, 1); Result.WrapNeighbors := False;
    Result.CurrentPass := 'context'; Result.AddValue('land');
    Result.SwitchToPass('terrain'); Result.PassMode := gpmOverlay;
    Result.AddValue('A').RequireFromPass('context', 'land');
    Result.AddValue('B').RequireFromPass('context', 'land');
    Result.SwitchToPass('housing'); Result.PassMode := gpmOverlay;
    Result.AddValue('HA').RequireFromPass('terrain', 'A');
    Result.AddValue('HB').RequireFromPass('terrain', 'B');
    if not Result.TrySolve(DefaultGraphSolveOptions, Baseline) then
      raise Exception.Create('selective negotiation baseline failed');
    if Result.Entry[0, 0, 0].Value = 'HA' then
      Result.SetAllowedValues(0, 0, 0, 'HB')
    else Result.SetAllowedValues(0, 0, 0, 'HA');
  except Result.Free; raise; end;
end;

procedure TestSelectiveNegotiatedTransactions;
var
  Graph, Oracle, Plain: TGraph;
  Sink: TCollectingSink;
  Options, Captured: TGraphNegotiationOptions;
  Report, Expected, Silent: TGraphSelectiveNegotiationReport;
  Index, CaptureMode: Integer;
begin
  for CaptureMode := 0 to 1 do
  begin
    Graph := SelectiveFixture; Oracle := SelectiveFixture; Plain := SelectiveFixture;
    Sink := TCollectingSink.Create;
    try
      Sink.Graph := Graph; Graph.TraceSink := Sink;
      Captured := DefaultGraphNegotiationOptions;
      Captured.MaxPassBacktracks := 1; Captured.SolveOptions.MaxBacktracks := 1;
      Captured.SolveOptions.CaptureTrace := True;
      Options := Captured; Options.SolveOptions.CaptureTrace := CaptureMode = 1;
      Check(Graph.TryRegenerateNegotiatedFrom('terrain', Options, Report)
        and Oracle.TryRegenerateNegotiatedFrom('terrain', Captured, Expected)
        and Plain.TryRegenerateNegotiatedFrom('terrain', Options, Silent),
        'selective negotiation reopens the active provider while reusing context');
      Check((Report.Search.PassBacktracks = 1) and (Length(Report.Search.Attempts) = 1)
        and (Sink.BeginCalls = 2) and (Sink.EndCalls = 2),
        'selective negotiation exposes two actual transaction boundaries');
      Index := 0;
      CheckNegotiation(Graph, Sink, Report.Search, Expected.Search, Index,
        CaptureMode = 1, 'selective negotiated');
      Check((Index = Length(Sink.Traces))
        and (Report.Search.FinalReport.Passes[0].Disposition = gpdReused)
        and (CountKind(Sink.Traces[0].Events, gtekPassSkipped) = 1)
        and (CountKind(Sink.Traces[1].Events, gtekPassSkipped) = 1),
        'every selective round announces its own reused context');
      Check((Report.TranscriptHash = Silent.TranscriptHash)
        and (CalculateGraphSelectiveNegotiationTranscriptHash(Options, Report)
          = Report.TranscriptHash), 'selective observer metadata stays outside transcript identity');
      Check(OutputState(Graph) = OutputState(Oracle), 'selective negotiation output parity');
      CheckStreams(Graph, Oracle, 'selective negotiation');
    finally Graph.Free; Oracle.Free; Plain.Free; Sink.Free; end;
  end;
end;

procedure TestFailureInEveryNegotiatedRound;
var
  Graph, Oracle: TGraph;
  Sink: TCollectingSink;
  Options: TGraphNegotiationOptions;
  Report, Expected: TGraphNegotiationReport;
  I: Integer;
begin
  Graph := Fixture(fkNegotiation); Oracle := Fixture(fkNegotiation);
  Sink := TCollectingSink.Create;
  try
    Sink.Graph := Graph; Sink.ThrowPhase := gtdpEvent; Sink.ThrowEvent := 0;
    Graph.TraceSink := Sink;
    Options := DefaultGraphNegotiationOptions;
    Options.MaxPassBacktracks := 1; Options.SolveOptions.MaxBacktracks := 1;
    Options.SolveOptions.CaptureTrace := True;
    Check(Graph.TrySolveNegotiated(Options, Report)
      and Oracle.TrySolveNegotiated(Options, Expected),
      'a persistently failing observer cannot disable provider negotiation');
    Check((Sink.BeginCalls = 2) and (Sink.EventCalls = 2) and (Sink.EndCalls = 0),
      'each ordinary round retries its borrowed observer once');
    Check((Report.TranscriptHash = Expected.TranscriptHash)
      and (Report.FinalReport.TraceHash = Expected.FinalReport.TraceHash),
      'repeated observer failures preserve full negotiated trace identities');
    for I := 0 to High(Report.Attempts) do
      Check((Report.Attempts[I].SolveReport.TraceDelivery.Status = gtdsSinkFailed)
        and (Report.Attempts[I].SolveReport.TraceDelivery.DeliveredEventCount = 0)
        and (Report.Attempts[I].SolveReport.TraceDelivery.ProducedEventCount =
          Length(Expected.Attempts[I].SolveReport.Trace))
        and (Report.Attempts[I].SolveReport.TraceDelivery.TraceHash =
          Expected.Attempts[I].SolveReport.TraceHash),
        'failed round retains complete production after observer detaches');
    Check((Report.FinalReport.TraceDelivery.Status = gtdsSinkFailed)
      and (Report.FinalReport.TraceDelivery.DeliveredEventCount = 0)
      and (Report.FinalReport.TraceDelivery.ProducedEventCount = Length(Expected.FinalReport.Trace))
      and (Report.FinalReport.TraceDelivery.TraceHash = Expected.FinalReport.TraceHash),
      'winning round independently reports its own observer failure');
    Check(OutputState(Graph) = OutputState(Oracle), 'failing nested observer output parity');
    CheckStreams(Graph, Oracle, 'failing nested observer');
  finally Graph.Free; Oracle.Free; Sink.Free; end;
end;

procedure TestBorrowedLifetimeAndGuard;
var
  Graph: TGraph;
  Sink: TCollectingSink;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
  Before: Integer;
begin
  Before := DestroyedSinks;
  Graph := Fixture(fkChoices); Sink := TCollectingSink.Create;
  try
    Options := DefaultGraphSolveOptions;
    Check(Graph.TraceSink = nil, 'new graph defaults to no trace observer');
    Sink.Graph := Graph; Sink.GuardMutation := True; Graph.TraceSink := Sink;
    Check(Graph.TrySolve(Options, Report), 'illegal observer mutation cannot fail generation');
    Check((Graph.TraceSink = Sink) and (Report.TraceDelivery.Status = gtdsSinkFailed)
      and (Report.TraceDelivery.FailurePhase = gtdpBegin),
      'running guard rejects replacing the active sink and detaches this attempt');
    Graph.Free; Graph := nil;
    Check(DestroyedSinks = Before, 'destroying a graph does not free its borrowed sink');
  finally Graph.Free; Sink.Free; end;
  Check(DestroyedSinks = Before + 1, 'the caller destroys the borrowed sink exactly once');
end;

procedure TestProducerInterruption;
var
  Graph: TRejectingGraph;
  Sink: TCollectingSink;
  Report: TGraphSolveReport;
  Raised: Boolean;
begin
  Graph := TRejectingGraph(Fixture(fkRejectEarly)); Sink := TCollectingSink.Create;
  try
    Graph.RaiseInHook := True; Graph.Sink := Sink;
    Sink.Graph := Graph; Graph.TraceSink := Sink;
    Raised := False;
    try Graph.TrySolve(DefaultGraphSolveOptions, Report);
    except on E: Exception do Raised := E.Message = 'producer commit failure'; end;
    Check(Raised, 'a producer exception retains its original exception contract');
    Check(not Graph.Running and Graph.PassGraph[0].Entry[0, 0, 0].Empty
      and Graph.PassGraph[1].Entry[0, 0, 0].Empty, 'producer interruption rolls entries back');
    Check((Sink.BeginCalls = 1) and (Sink.EndCalls = 1)
      and Sink.Traces[0].EndCalled, 'interrupted producer closes the active stream');
    Check((Sink.Traces[0].Delivery.Status = gtdsInterrupted)
      and (Report.TraceDelivery.Status = gtdsInterrupted),
      'producer interruption is not presented as a complete ordinary trace');
    Graph.RaiseInHook := False; Graph.RejectPass := -1;
    Check(Graph.TrySolve(DefaultGraphSolveOptions, Report)
      and (Report.TraceDelivery.Status = gtdsComplete),
      'fresh transaction after producer exception delivers normally');
    Check((Sink.BeginCalls = 2) and (Sink.EndCalls = 2)
      and (Sink.Traces[1].Events[0].EventId = 0),
      'interrupted event IDs never bleed into the next attempt');
  finally Graph.Free; Sink.Free; end;
end;

begin
  try
    Check((WFC_TRACE_DELIVERY_VERSION = 1) and (WFC_TRACE_VERSION = 1)
      and (WFC_TRACE_HASH_VERSION = 1), 'additive delivery version preserves Trace v1');
    TestFixtureMatrix;
    TestSelectiveReuseAndRepeatedSolve;
    TestObserverFailures;
    TestNegotiatedTransactions;
    TestRestartTransactions;
    TestSelectiveNegotiatedTransactions;
    TestFailureInEveryNegotiatedRound;
    TestBorrowedLifetimeAndGuard;
    TestProducerInterruption;
  except
    on E: Exception do
    begin
      Inc(Failures); WriteLn('UNEXPECTED: ', E.ClassName, ': ', E.Message);
    end;
  end;
  WriteLn('TRACE_STREAM_DEPENDENCY_GOLDEN=46715F2C');
  WriteLn('checks=', Checks, ' failures=', Failures);
  if Failures <> 0 then Halt(1);
end.
