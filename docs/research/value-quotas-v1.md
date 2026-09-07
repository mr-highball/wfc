# Whole-pass value-set quotas v1: implementation record

This records the project's implementation and finite verification evidence.
It does not claim that cardinality constraints or their bounds propagation are
new inventions, or that this solver is faster than another framework.

## Semantics

For cells `c`, nonempty domains `D[c]`, and an accepted set `S`, a complete
assignment `x` satisfies one quota exactly when:

`minimum <= sum(c: x[c] in S) <= maximum`.

Multiple quota sets are conjunctive and may overlap. Each distinct cell has
unit contribution; weights and wrapped neighbor aliasing do not change it.

Define `F` as the number of domains wholly contained in `S`, and `U` as the
number that intersect `S`. Every completion has an accepted count between
`F` and `U`. Therefore:

- `F > maximum` or `U < minimum` proves contradiction.
- If `F = maximum`, no mixed domain may select an accepted member.
- If `U = minimum`, every mixed domain must select an accepted member.

The first pruning rule leaves `F` unchanged; the second leaves `U` unchanged.
Both retain a nonempty side of each mixed domain. Repeated sweeps after
adjacency/connectivity propagation or another quota reach a common fixed
point. Search is still necessary where these bounds alone cannot detect an
inconsistent intersection of clauses.

## Implementation boundaries

The numeric reference model stores accepted value indices and inclusive
integer bounds. It validates descriptor numbers before descriptor-driven
matrix arithmetic or allocation. Pruning uses ordinary candidate removal,
queueing, and the existing trail. No quota-specific counters survive across
backtracks. Descriptors, cells, and removed values are visited in stable order.

The implementation scans each accepted-value list directly. For `Q` quotas,
`C` cells, `V` registered values, and largest set size `S`, a pruning sweep has
a conservative `O(Q * C * V * S)` upper bound; this is not an incremental
cardinality data structure. Performance refinements must preserve the
same soundness and replay contracts.

Complete assignment validation recounts numeric values independently of
propagation state. A separate public-value recount guards the graph/compiler
boundary, preserved providers, and final publication. The final numeric
validation recovery branch includes quotas, so a rejected complete assignment
may backtrack instead of being misclassified as a terminal implementation
failure.

## Reproducible checks

The native build gates include three portable programs, also discovered by
the pas2js browser gate:

- `test/wfc_value_quota_reference_test.lpr`: complete-assignment enumeration
  and trace-prefix feasibility checks, including overlapping accepted sets,
  adjacency, locks, impossible/zero/loose bounds, restoration, malformed
  descriptors, and exact browser-number rejection.
- `test/wfc_value_quota_test.lpr`: fluent ownership, canonical sets, scoped
  transactional behavior, and a layered application fixture.
- `test/wfc_value_quota_trace_test.lpr`: constraint identities, rehashed forged
  events, independent trace/layout validation, capture-disabled negotiation,
  and bounded-window/full-capture parity.

The initial numeric oracle exhaustively covers 6,256 finite models and checks
4,282 quota removals against feasible completions. That finite evidence is not
an exhaustive proof over arbitrary graph sizes. Its runtime checks complement
the bounds argument above. Existing unconstrained replay goldens must remain
unchanged; new quota models have their own opt-in version and trace identity.

Future work includes portable recipe encoding, scoped regions, more efficient
count maintenance, and stronger propagation for interacting sets. Those are
separate contracts, not implied capabilities of v1.
