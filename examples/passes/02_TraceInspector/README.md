# causal trace inspector

This dependency-free example makes a complete pass transaction inspectable.
One shared Pascal unit solves three layers in order:

```text
terrain -> settlement -> foliage
```

Terrain filters where homes can exist. Terrain and settlement together then
filter trees and reeds. The program captures the complete chronological trace,
validates its structure, derived pass ranges, and portable signature, prints
every pass range and event, and walks one rejected tree candidate backward to
the settlement event that caused the rejection. The range layout also handles
late validation suffixes without changing Trace-v1 events or hashes.

The example deliberately performs the same solve twice. It requires identical
layers, events, cause links, and hashes before printing `Self-check: passed`.
It then runs a two-pass transaction whose commit validator rejects pass zero
after pass one has staged. That second fixture proves rollback, preserves trace
hash `B27D0AE0`, shows pass-zero ranges `0..1,4..5`, and demonstrates why the
strict legacy slice validator rejects the report while Trace Layout v1 accepts
it.

Finally, a separate solve sends the original events to a live callback while
retaining only the newest five in `TGraphTraceWindowSink`. That attempt sets
`CaptureTrace=False`; it does not construct a full history before delivering
events. Its output, event count, and production hash must still match the
earlier small full-capture fixture.

The implementation uses only repository units and the standard FPC or pas2js
RTL.

## build and run

From the repository root on PowerShell:

```powershell
New-Item -ItemType Directory -Force `
  build\trace-inspector\native\units, build\trace-inspector\native\bin |
  Out-Null
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc `
  -Fuexamples\passes\02_TraceInspector `
  -FUbuild\trace-inspector\native\units `
  -FEbuild\trace-inspector\native\bin `
  examples\passes\02_TraceInspector\TraceInspector.lpr
.\build\trace-inspector\native\bin\TraceInspector.exe
```

On a POSIX shell:

```bash
mkdir -p build/trace-inspector/native/units build/trace-inspector/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc \
  -Fuexamples/passes/02_TraceInspector \
  -FUbuild/trace-inspector/native/units \
  -FEbuild/trace-inspector/native/bin \
  examples/passes/02_TraceInspector/TraceInspector.lpr
./build/trace-inspector/native/bin/TraceInspector
```

The repository build scripts also compile and run the native inspector as a
smoke gate. CI compiles and runs the same shared implementation through
pas2js.

## seed-zero contract

The checked fixture has these solved layers:

```text
terrain:    land,water,land,land
settlement: home,vacant,home,vacant
foliage:    bare,reeds,bare,tree
```

Its trace contains 27 events and has portable signature `73C4B9A2`. Event 15
records `tree` being removed from foliage entry zero because of settlement;
its cause chain reaches the settlement staging event 13. Native FPC and
pas2js must print the same output, including the separate late-rejection and
streaming fixtures. The original captured trace and late-failure signatures
remain unchanged.

## live delivery with bounded retention

The live sink subclasses `TGraphTraceWindowSink` and overrides `AppendEvent`.
It calls the inherited method to retain the bounded suffix, then prints the
unmodified event with `FormatGraphTraceEvent`. The callback checks that the
graph is still running; these lines are delivered during generation, not by
replaying a completed report. Callbacks are synchronous observation, not
interactive pause/resume points, and must not mutate or reenter the graph.

The streaming fixture verifies and prints:

```text
Stream summary: produced=27 delivered=27 hash=73C4B9A2
  report-trace-events=0 report-trace-hash=00000000 layers-match=true
  retained=5 dropped=22 delivery-complete=true window-complete=false
Retained suffix: original event IDs 22..26
```

Delivery and retention describe different things. All 27 events reached the
sink, but only five remain in memory. `Delivery.Status=gtdsComplete` confirms
completed delivery; `Complete=False` warns that this retained window is not a
complete trace. The window leaves every `EventId` and `CauseEventId` unchanged.
When a cause precedes event 22, the example prints its exact original ID and
`outside-window (not retained; original ID preserved)`. It never relabels that
cause as an external model input or pretends the suffix is a full Trace-v1
report.

`TGraph.TraceSink` is borrowed. The example detaches its sink before freeing
it. A callback failure is reported through `Report.TraceDelivery` without
changing generation or the legacy capture fields. Retention capacity is a
caller-selected event count, not a solver or composition-length limit.

This complete demonstration intentionally keeps its earlier, fixed 27-event
full captures as comparison oracles. The streamed attempt itself keeps no
full history: its solve report has an empty `Trace`, a zero legacy `TraceHash`,
and disabled pass trace slices. The window holds five events; copying that
window for inspection creates only another five-event suffix. To use live
delivery without a full oracle, attach the sink to your graph and solve with
`CaptureTrace=False`. Console output may still be retained by the terminal or
browser hosting this example; that output storage is separate from the
solver's and sink's event retention.

See [the causal trace contract](../../../docs/traces.md) for the event schema,
hash encoding, query utilities, validation rules, cost, and current limits.

## scope

This is a deterministic command-line inspector and conformance fixture. It is
also a working live-delivery and bounded-window example. It is not yet an
interactive decision stepper, graphical timeline, failed-clause expansion,
or minimal-unsatisfiable-core extractor. Those features can build on this
versioned trace surface without introducing a mandatory third-party runtime.
