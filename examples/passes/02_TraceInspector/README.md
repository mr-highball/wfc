# causal trace inspector

This dependency-free example makes a complete pass transaction inspectable.
One shared Pascal unit solves three layers in order:

```text
terrain -> settlement -> foliage
```

Terrain filters where homes can exist. Terrain and settlement together then
filter trees and reeds. The program captures the complete chronological trace,
validates its structure and portable signature, prints every pass slice, and
walks one rejected tree candidate backward to the settlement event that caused
the rejection.

The example deliberately performs the same solve twice. It requires identical
layers, events, cause links, and hashes before printing `Self-check: passed`.
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
pas2js must print the same 47 lines.

See [the causal trace contract](../../../docs/traces.md) for the event schema,
hash encoding, query utilities, validation rules, cost, and current limits.

## scope

This is a deterministic command-line inspector and conformance fixture. It is
not yet an interactive decision stepper, graphical timeline, streaming trace
sink, failed-clause expansion, or minimal-unsatisfiable-core extractor. Those
features can build on this versioned trace surface without introducing a
mandatory third-party runtime.
