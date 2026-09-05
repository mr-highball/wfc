# bounded pass negotiation

This one-cell fixture makes the difference between staged solving and bounded
cross-pass negotiation visible without hiding it behind a large world. The
`terrain` pass registers `marsh` before `meadow`, so seed `0` selects `marsh`.
The only `housing` value is `cottage`, and it requires `meadow` from terrain.

An ordinary `TrySolve` transaction therefore fails in housing and rolls both
passes back. `TrySolveNegotiated` keeps that behavior separate: with one pass
backtrack available, it records the rejected round, excludes the exact
provider assignment `terrain=[marsh]`, reruns from the same seed, and commits
`meadow|cottage` atomically.

The example is an executable native golden. It requires:

- one-way trace `42AF302E`;
- one rejected round and two total rounds;
- exactly one pass backtrack, reopening terrain at execution ordinal `0`;
- the complete excluded value-index assignment `[0]`, decoded as `marsh`;
- terminal terrain counters of two decisions, one contradiction, one
  backtrack, and one excluded assignment; and
- negotiation transcript `6F76591D`, reproduced by a second fresh run.

The transcript owns the ordered rejected-round reports, exact assignments,
terminal report, seed, and options. `Attempts` contains rejected rounds only;
`FinalReport` is the sole terminal round. Version 1 uses chronological,
exact-assignment pass backtracking bounded by `MaxPassBacktracks`. The fuller
[conformance suite](../../../test/wfc_negotiation_test.lpr) also covers a
two-cell exact vector, exhaustion, multi-provider backtracking, solver and pass
budgets, rollback, caller locks, and replay.

From the repository root, create the named output directories and build the
native host:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/passes/03_PassNegotiation -FUbuild/pass-negotiation/native/units -FEbuild/pass-negotiation/native/bin examples/passes/03_PassNegotiation/PassNegotiation.lpr
build/pass-negotiation/native/bin/PassNegotiation
```

Both paths use only repository units and the applicable standard RTL. Either
host exits nonzero if an assignment, counter, report, trace, transcript, or
replay invariant changes.
