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
program wfc_trace_window_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_trace, wfc_trace_stream;

var Checks, Failures: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then
  begin
    Inc(Failures);
    WriteLn('FAIL: ', AMessage);
  end;
end;

function Header: TGraphTraceHeader;
begin
  Result := Default(TGraphTraceHeader);
  Result.DeliveryVersion := WFC_TRACE_DELIVERY_VERSION;
  Result.TraceVersion := WFC_TRACE_VERSION;
  Result.TraceHashVersion := WFC_TRACE_HASH_VERSION;
  Result.RandomAlgorithmVersion := WFC_RANDOM_ALGORITHM_VERSION;
  Result.SolverAlgorithmVersion := WFC_SOLVER_ALGORITHM_VERSION;
  Result.GraphModelVersion := WFC_GRAPH_MODEL_VERSION;
  Result.PipelineAlgorithmVersion := WFC_PIPELINE_ALGORITHM_VERSION;
  Result.PassCount := 1;
  Result.Seed := 55;
end;

function Event(const AId: Integer; const ATerminal: Boolean = False): TGraphTraceEvent;
begin
  Result := Default(TGraphTraceEvent);
  Result.EventId := AId;
  Result.CauseEventId := AId - 1;
  Result.PassIndex := 0;
  Result.Kind := gtekPassStaged;
  Result.CauseKind := gtckTransaction;
  Result.EntryIndex := -1;
  Result.ValueIndex := -1;
  Result.NeighborIndex := -1;
  Result.DependencyPassIndex := -1;
  Result.ConstraintIndex := -1;
  if AId = 0 then Result.Kind := gtekPassBegin;
  if ATerminal then
  begin
    Result.Kind := gtekPipelineCommit;
    Result.PassIndex := -1;
  end;
  Result.Value := 'event-' + IntToStr(AId);
end;

function Delivery(const ACount: Integer;
  const AStatus: TGraphTraceDeliveryStatus = gtdsComplete): TGraphTraceDelivery;
begin
  Result := Default(TGraphTraceDelivery);
  Result.Version := WFC_TRACE_DELIVERY_VERSION;
  Result.Status := AStatus;
  Result.ProducedEventCount := ACount;
  Result.DeliveredEventCount := ACount;
  Result.TraceHash := 123;
  Result.FailureEventId := -1;
end;

procedure Deliver(const ASink: TGraphTraceWindowSink; const ACount: Integer);
var I: Integer;
begin
  ASink.BeginTrace(Header);
  for I := 0 to ACount - 1 do ASink.AppendEvent(Event(I, I = ACount - 1));
  ASink.EndTrace(Delivery(ACount));
end;

procedure TestWindows;
var Capacity, Count, I, Retained: Integer; Sink: TGraphTraceWindowSink;
  Events, Earlier: TGraphTraceEvents; H: TGraphTraceHeader;
begin
  for Capacity := 0 to 7 do
  begin
    Sink := TGraphTraceWindowSink.Create(Capacity);
    try
      Check((Sink.Capacity = Capacity) and not Sink.Complete
        and (Sink.RetainedEventCount = 0) and (Sink.DroppedEventCount = 0),
        'new window is empty and incomplete');
      for Count := 1 to 25 do
      begin
        Deliver(Sink, Count);
        Retained := Count;
        if Retained > Capacity then Retained := Capacity;
        Check(Sink.RetainedEventCount = Retained, 'retention obeys caller capacity');
        Check(Sink.DroppedEventCount = Count - Retained, 'drops count every displaced event');
        Check(Sink.Complete = (Count <= Capacity), 'complete requires an undropped complete delivery');
        Check((Sink.Delivery.Status = gtdsComplete)
          and (Sink.Delivery.ProducedEventCount = Count), 'full delivery remains separate from window truncation');
        Events := Sink.CopyEvents;
        Check(Length(Events) = Retained, 'copy has exactly retained size');
        for I := 0 to High(Events) do
        begin
          Check(Events[I].EventId = Count - Retained + I, 'wrapped copy keeps chronological original IDs');
          Check(Events[I].CauseEventId = Events[I].EventId - 1, 'window does not rewrite missing causal predecessors');
          Check(Events[I].Value = 'event-' + IntToStr(Events[I].EventId), 'event strings survive ring reuse');
        end;
      end;
      Earlier := Sink.CopyEvents;
      H := Header; H.Seed := 91;
      Sink.BeginTrace(H);
      Check((Sink.Header.Seed = 91) and not Sink.Complete
        and (Sink.RetainedEventCount = 0) and (Sink.DroppedEventCount = 0),
        'new transaction clears old counters and events');
      Sink.EndTrace(Delivery(0, gtdsInterrupted));
      Check(not Sink.Complete and (Sink.Delivery.Status = gtdsInterrupted),
        'empty interrupted transaction does not become complete');
      for I := 0 to High(Earlier) do
        Check(Earlier[I].Value = 'event-' + IntToStr(Earlier[I].EventId),
          'detached earlier snapshot outlives next begin');
    finally
      Sink.Free;
    end;
  end;
end;

procedure TestDetachedCopies;
var Sink: TGraphTraceWindowSink; E: TGraphTraceEvent;
  Events, Other: TGraphTraceEvents; H: TGraphTraceHeader; D: TGraphTraceDelivery;
begin
  Sink := TGraphTraceWindowSink.Create(3);
  try
    Sink.BeginTrace(Header);
    E := Event(0); Sink.AppendEvent(E); E.Value := 'source mutation';
    Events := Sink.CopyEvents;
    Check(Events[0].Value = 'event-0', 'source mutation does not edit retained event');
    Events[0].Value := 'copy mutation'; Events[0].EventId := 77;
    Other := Sink.CopyEvents;
    Check((Other[0].Value = 'event-0') and (Other[0].EventId = 0),
      'caller copy has detached event records');
    H := Sink.Header; H.Seed := 77;
    Check(Sink.Header.Seed = 55, 'header is a copied record');
    Sink.AppendEvent(Event(1, True)); Sink.EndTrace(Delivery(2));
    D := Sink.Delivery; D.TraceHash := 77;
    Check(Sink.Delivery.TraceHash = 123, 'delivery is a copied record');
    Check(Sink.Complete, 'full two-event observation is complete delivery');
  finally
    Sink.Free;
  end;
  Check(Other[0].Value = 'event-0', 'copied strings survive sink destruction');
end;

procedure TestRejectedCallbacks;
var Sink: TGraphTraceWindowSink; H: TGraphTraceHeader;
  E: TGraphTraceEvent; D: TGraphTraceDelivery; CaseIndex: Integer; Raised: Boolean;
begin
  for CaseIndex := 0 to 15 do
  begin
    Sink := TGraphTraceWindowSink.Create(3);
    try
      Raised := False;
      try
        case CaseIndex of
          0: Sink.AppendEvent(Event(0));
          1: Sink.EndTrace(Delivery(0));
          2: begin Sink.BeginTrace(Header); Sink.BeginTrace(Header); end;
          3: begin Deliver(Sink, 2); Sink.EndTrace(Delivery(2)); end;
          4: begin Deliver(Sink, 2); Sink.AppendEvent(Event(2)); end;
          5: begin Sink.BeginTrace(Header); Sink.AppendEvent(Event(1)); end;
          6: begin Sink.BeginTrace(Header); Sink.AppendEvent(Event(-1)); end;
          7: begin Sink.BeginTrace(Header); E := Event(0); E.CauseEventId := 0; Sink.AppendEvent(E); end;
          8: begin Sink.BeginTrace(Header); E := Event(0); E.CauseEventId := -2; Sink.AppendEvent(E); end;
          9: begin Sink.BeginTrace(Header); Sink.AppendEvent(Event(0, True)); Sink.AppendEvent(Event(1)); end;
          10: begin Sink.BeginTrace(Header); Sink.AppendEvent(Event(0)); Sink.EndTrace(Delivery(1)); end;
          11: begin Sink.BeginTrace(Header); Sink.AppendEvent(Event(0, True)); Sink.EndTrace(Delivery(2)); end;
          12: begin H := Header; H.TraceVersion := 99; Sink.BeginTrace(H); end;
          13: begin Sink.BeginTrace(Header); Sink.AppendEvent(Event(0, True)); D := Delivery(1); D.Version := 99; Sink.EndTrace(D); end;
          14: begin Sink.BeginTrace(Header); Sink.AppendEvent(Event(0, True)); Sink.EndTrace(Delivery(1, gtdsSinkFailed)); end;
          15: begin Sink.BeginTrace(Header); E := Event(0, True); E.PassIndex := 0; Sink.AppendEvent(E); end;
        end;
      except
        on EWfcTraceStream do Raised := True;
      end;
      Check(Raised, 'malformed callback rejected: ' + IntToStr(CaseIndex));
      Check(not Sink.Complete and (Sink.Delivery.Status = gtdsSinkFailed),
        'malformed callback invalidates complete delivery');
      Deliver(Sink, 2);
      Check(Sink.Complete, 'fresh begin recovers after rejected callback');
    finally
      Sink.Free;
    end;
  end;
end;

procedure ExpectCapacityRejected(const ACapacity: Integer);
var Sink: TGraphTraceWindowSink; Raised: Boolean;
begin
  Sink := nil;
  Raised := False;
  try
    try
      Sink := TGraphTraceWindowSink.Create(ACapacity);
    except
      on ERangeError do Raised := True;
    end;
    Check(Raised, 'invalid capacity rejected before array allocation');
  finally
    Sink.Free;
  end;
end;

{$IFDEF PAS2JS}
procedure TestHostNumbers;
var Sink: TGraphTraceWindowSink; E: TGraphTraceEvent;
  D: TGraphTraceDelivery; N, CaseIndex: Integer; Raised: Boolean;
begin
  for CaseIndex := 0 to 3 do
  begin
    case CaseIndex of
      0: asm N = NaN; end;
      1: asm N = Infinity; end;
      2: asm N = 0.5; end;
      3: asm N = 2147483648; end;
    end;
    ExpectCapacityRejected(N);
    Sink := TGraphTraceWindowSink.Create(1);
    try
      Sink.BeginTrace(Header);
      E := Event(0); E.EventId := N;
      Raised := False;
      try Sink.AppendEvent(E); except on EWfcTraceStream do Raised := True; end;
      Check(Raised and (Sink.RetainedEventCount = 0), 'malformed host ID cannot index ring storage');
      Sink.BeginTrace(Header); Sink.AppendEvent(Event(0, True));
      D := Delivery(1); D.ProducedEventCount := N;
      Raised := False;
      try Sink.EndTrace(D); except on EWfcTraceStream do Raised := True; end;
      Check(Raised and not Sink.Complete, 'malformed host count cannot certify delivery');
    finally
      Sink.Free;
    end;
  end;
end;
{$ENDIF}

function Fixture: TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := 17;
  Result.Reshape(4, 1, 1);
  Result.WrapNeighbors := False;
  Result.AddValue('A'); Result.AddValue('B'); Result.AddValue('C');
end;

procedure TestGraphIntegration;
var Graph, Oracle: TGraph; Options: TGraphSolveOptions;
  Expected, Actual, Reconstructed: TGraphSolveReport;
  Sink: TGraphTraceWindowSink; Events: TGraphTraceEvents;
  Layout: TGraphTraceLayout; Validation: TGraphTraceValidationReport;
  Capacity, I, Offset: Integer;
begin
  Options := DefaultGraphSolveOptions; Options.CaptureTrace := True;
  Oracle := Fixture;
  try
    Check(Oracle.TrySolve(Options, Expected), 'buffered integration oracle solves');
    for Capacity := 0 to Length(Expected.Trace) do
    begin
      Graph := Fixture;
      Sink := TGraphTraceWindowSink.Create(Capacity);
      try
        Graph.TraceSink := Sink;
        Options.CaptureTrace := False;
        Check(Graph.TrySolve(Options, Actual), 'streaming graph solves with window');
        Check((not Actual.TraceCaptured) and (Length(Actual.Trace) = 0)
          and (Actual.TraceHash = 0), 'window leaves legacy capture disabled');
        Check(Actual.TraceDelivery.Status = gtdsComplete, 'bounded storage does not fail delivery');
        Check((Sink.Delivery.TraceHash = Expected.TraceHash)
          and (Actual.TraceDelivery.TraceHash = Expected.TraceHash), 'whole-stream hash is independent of window size');
        Check(Sink.Complete = (Capacity = Length(Expected.Trace)), 'only untruncated graph window is complete');
        Events := Sink.CopyEvents;
        Offset := Length(Expected.Trace) - Length(Events);
        for I := 0 to High(Events) do
          Check(FormatGraphTraceEvent(Events[I]) = FormatGraphTraceEvent(Expected.Trace[Offset + I]),
            'graph window is an exact original event suffix');
        Check(TryBuildGraphTraceLayout(Graph, Actual, Layout, Validation),
          'ordinary disabled report still validates');
        if Sink.Complete then
        begin
          Reconstructed := Actual;
          Reconstructed.TraceCaptured := True;
          Reconstructed.Trace := Sink.CopyEvents;
          Reconstructed.TraceHash := Sink.Delivery.TraceHash;
          Reconstructed.Passes := Copy(Actual.Passes);
          for I := 0 to High(Reconstructed.Passes) do
          begin
            Reconstructed.Passes[I].TraceStart := Expected.Passes[I].TraceStart;
            Reconstructed.Passes[I].TraceCount := Expected.Passes[I].TraceCount;
          end;
          Check(TryBuildGraphTraceLayout(Graph, Reconstructed, Layout, Validation),
            'full reconstruction with matching pass metadata validates independently');
        end;
      finally
        Graph.TraceSink := nil;
        Sink.Free;
        Graph.Free;
      end;
    end;
  finally
    Oracle.Free;
  end;
end;

begin
  TestWindows;
  TestDetachedCopies;
  TestRejectedCallbacks;
  ExpectCapacityRejected(-1);
  {$IFDEF PAS2JS}TestHostNumbers;{$ENDIF}
  TestGraphIntegration;
  WriteLn(Checks, ' trace window checks, ', Failures, ' failures');
  if Failures <> 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d trace window checks failed', [Failures]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
