# Streaming causal traces and bounded windows

WFC can deliver its causal events while a transaction is running without
retaining the attempted search in memory. The events remain Trace v1: their
order, numeric IDs, cause IDs, and whole-trace hash match ordinary full capture.
The solver and its sinks are project-owned Pascal under the MIT license, using
the standard FPC or pas2js RTL.

Streaming is independent of `Options.CaptureTrace`. That existing option still
means **retain the complete trace in the solve report**. Attaching a sink does
not silently enable it, truncate it, or turn a suffix into a complete report.

## Attach a bounded inspection window

The reusable `wfc_trace_stream.TGraphTraceWindowSink` keeps the newest
caller-selected number of events. For example, after configuring a graph:

```pascal
uses
  SysUtils, wfc, wfc_trace, wfc_trace_stream;

procedure InspectSolve(const Graph: TGraph; const EventCapacity: Integer);
var
  Window: TGraphTraceWindowSink;
  PreviousSink: TGraphTraceSink;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
  Events: TGraphTraceEvents;
  I: Integer;
begin
  Window := TGraphTraceWindowSink.Create(EventCapacity);
  PreviousSink := Graph.TraceSink;
  try
    Graph.TraceSink := Window;
    Options := DefaultGraphSolveOptions;
    Options.CaptureTrace := False;
    if not Graph.TrySolve(Options, Report) then
      WriteLn('Attempt rolled back in pass ', Report.FailedPassIndex);

    WriteLn('Produced events: ', Report.TraceDelivery.ProducedEventCount);
    WriteLn('Retained events: ', Window.RetainedEventCount);
    WriteLn('Dropped from window: ', Window.DroppedEventCount);
    WriteLn('Whole-trace hash: ',
      GraphTraceSignatureHex(Report.TraceDelivery.TraceHash));
    Events := Window.CopyEvents;
    for I := 0 to High(Events) do
      WriteLn(FormatGraphTraceEvent(Events[I]));
  finally
    Graph.TraceSink := PreviousSink;
    Window.Free;
  end;
end;
```

`EventCapacity` is application input, not a prescribed target. Any representable
nonnegative `Integer` is accepted subject to available memory. Capacity zero
retains no events but still observes delivery metadata. Capacity one can retain
just the terminal event. The capacity is fixed for the sink's lifetime; create
another window to choose a different budget.

`WFC_TRACE_WINDOW_VERSION = 1` identifies this bounded-window contract. It does
not change the schema, identity, or hash of the events being inspected.

`CopyEvents` returns a detached chronological array, oldest retained event
first. It never exposes the internal ring. Editing or releasing the returned
array cannot edit the sink; a copy remains usable after another solve or after
the sink is destroyed. Event strings have ordinary Pascal value semantics.

The original IDs are important. A three-event suffix might contain IDs
`997, 998, 999`, with a cause pointing to event `32`. That cause is outside the
window, not missing from the original trace. Do not renumber the suffix or
replace unavailable cause IDs with `-1`: either change would falsify evidence.

## Delivery is not retained completeness

`WFC_TRACE_DELIVERY_VERSION` versions the observation contract independently of
the existing event schema, hash, and layout versions.

Each ordinary transaction calls these methods on a borrowed `TGraphTraceSink`:

1. `BeginTrace(Header)` supplies the seed, algorithm versions, and pass count.
2. `AppendEvent(Event)` delivers each chronological event synchronously.
3. `EndTrace(Delivery)` supplies the outcome of observation and the complete
   produced-event count/hash available for that attempt.

The graph does not own or destroy a sink. Keep it alive through the solve and
detach it before freeing it. The sink is attached to the pass root; selective
regeneration, negotiated rounds, and restarts use the same ordinary-transaction
observation boundary. Each negotiation/restart round begins again at event ID
zero. A window resets at each `BeginTrace`, so after several rounds it contains
the final round's suffix, not an aggregate history. A caller-owned sink can
store round headers and footers separately when earlier rounds are needed.

`Report.TraceDelivery` distinguishes:

| Status | Meaning |
| --- | --- |
| `gtdsDisabled` | No sink was attached to this ordinary attempt. |
| `gtdsComplete` | Begin, every event, and End were accepted by the sink. |
| `gtdsSinkFailed` | A sink callback raised; the failure phase, event ID where applicable, and message identify the failed delivery. |
| `gtdsInterrupted` | The producer exited through an exception rather than returning an ordinary solve result. |

`ProducedEventCount` covers emitted events even after a sink fails.
`DeliveredEventCount` covers only the successfully accepted prefix. Failure of
End means delivery failed even if every event was accepted. `TraceHash` is the
producer's whole emitted numeric trace hash, not a hash of a window or proof
that a failing sink stored the same bytes.

A window's `Header` is its latest accepted header. Its `Delivery` is default
until End is received, or describes a callback rejected by the window. A valid
End stores the supplied producer metadata. The window checks versions,
callback order, consecutive IDs, backward cause IDs, terminal-event placement,
and exact delivery counts. It rejects malformed input and becomes incomplete;
a new Begin can start a fresh transaction after that rejection.

`Window.Complete` is true only after a valid `gtdsComplete` End with **no events
dropped from the window**. This is delivery completeness, not a successful
solve, semantic graph validation, independent hash recomputation, or proof of
all causal rules. A complete failed attempt ends in pipeline rollback. A
successfully delivered but truncated window has `Delivery.Status =
gtdsComplete` and `Complete = False`. An interrupted attempt is never complete.
An interrupted footer may have no terminal event: a checked event-capacity or
allocation failure can prevent the producer from appending that event. The
footer does not invent a successful commit or a complete rollback trace.
Catastrophic allocation failure cannot guarantee footer publication either.

## Atomic solve outcomes and observer failures

Observation does not choose candidates or consume random values. A sink
exception detaches that sink for the remainder of the current ordinary
attempt. The solver continues; its assignments, counters, random-stream
position, commit/rollback, and optional full retained trace do not depend on
whether the observer accepted its events. No additional callbacks are made to
that failing observer within the same attempt. A later attempt starts a new
delivery.

Callbacks are synchronous, borrowed observation. Do not mutate the graph,
draw from its random stream, reenter its solve methods, change its sink, or
destroy it from a callback. Do not block a browser callback waiting for an
asynchronous operation to finish. File/network/browser adapters must implement
their own suitable buffering or transaction handling; asynchronous backpressure
is not part of this contract.

Events describe the complete attempted search, including abandoned branches
and provisional staging. Streamed events are not withdrawn when a solve rolls
back. Only the final pipeline-commit event certifies a successful transaction;
pipeline rollback records that the staged assignment did not commit. A sink's
own external side effects remain its responsibility.

## Compatibility and inspection boundaries

With `CaptureTrace = False`, the ordinary report still has an empty `Trace`,
zero `TraceHash`, and disabled legacy pass trace metadata. Its independent
`TraceDelivery` can nevertheless record successful streaming. With full
capture enabled, the ordinary report and all existing Trace-v1 golden hashes
remain unchanged. Observation metadata is not mixed into existing negotiation
or restart transcript signatures.

The `wfc_trace` validators operate on complete ordinary reports, not isolated
windows. To validate a stored stream, reconstruct the complete original events
and matching solve/pass metadata against the matching graph, then call
`TryBuildGraphTraceLayout`. Its chronological ranges also handle late rejection
of an earlier staged pass. Never manufacture a captured report from a truncated
suffix or reinterpret a validator failure as a successful solve. See
[the Trace-v1 contract](traces.md).

Event `Value` remains raw graph-level data. In particular, sequence adapters
can register model-private keys. Attaching a sink to an exposed adapter graph
does not apply the adapter's public-token projection. Domain-specific consumers
must preserve that boundary just as they do for raw buffered graph traces.

## Memory, portability, and real limits

Sink-only capture no longer retains a kernel event array, a public event array,
or an event-by-event local/global mapping. Its causal bookkeeping is
proportional to the active pass's cells plus pass metadata, independent of how
many branches were explored. Ordinary model/domain preparation still has its
own costs, including cell-by-value input-cause tables; streaming does not claim
to eliminate solver-model memory. A window adds storage proportional to its
caller-selected event capacity. Copying it temporarily adds one more array of
at most that capacity. Full `CaptureTrace` deliberately retains complete
search-length-proportional evidence as before.
An event-count budget is not a byte quota: inspection strings and model data
have their own sizes.

With no sink and `CaptureTrace = False`, fixed-size recorder bookkeeping still
exists, but no event arrays or per-cell causal arrays are allocated.

There is no arbitrary default trace window, duration target, or event cap
imposed on sink-only observation. Finite machines still have finite resources:
Trace-v1 IDs and counts use checked `Integer` values, so one transaction can
produce at most `High(Integer)` events with IDs `0..High(Integer)-1`. IDs never
wrap or silently become approximate browser numbers. Changing that event
identity format would require an explicit future contract, not a hidden
compatibility change.

The same Pascal units compile for native FPC and pas2js. Browser conformance
uses the repository's included FPC server, harness generator, and checker;
no additional language runtime or external media library is required. The
maintained native and browser gates discover
[`wfc_trace_reference_stream_test`](../test/wfc_trace_reference_stream_test.lpr),
[`wfc_trace_stream_test`](../test/wfc_trace_stream_test.lpr), and
[`wfc_trace_window_test`](../test/wfc_trace_window_test.lpr). They exercise
stream/buffer parity, real graph transactions, observer failure, ring wrapping,
zero capacity, reset, detached ownership, and malformed callback inputs.
The [research record](research/streaming-causal-traces-v1.md) fixes the
comparison oracle, causal-summary argument, finite fixtures, and unmeasured
performance boundaries.
