# selective pass negotiation

`TGraph.TryRegenerateNegotiatedFrom` combines two existing contracts without
silently weakening either one:

- selective regeneration supplies an explicit descendant-closed set of passes
  that the caller authorizes to change; and
- Pass Negotiation v1 chronologically rejects exact completed assignments, but
  only inside that authorized set.

The result is a bounded repair search over part of an acyclic pipeline. Clean
passes remain immutable inputs. In particular, the method never walks through
a dependency edge in the provider direction and never widens a narrow request
because a clean provider contributed to a contradiction.

This opt-in contract is identified independently from full-pipeline
negotiation:

```pascal
WFC_SELECTIVE_NEGOTIATION_ALGORITHM_VERSION = 1
WFC_SELECTIVE_NEGOTIATION_HASH_VERSION = 1
```

The full `TrySolveNegotiated` algorithm and its versions remain unchanged. See
[bounded pass negotiation](pass-negotiation.md) for the chronological search,
exact exclusions, ordinary round reports, and two-budget model shared by both
entry points.

## basic use

```pascal
var
  Options: TGraphNegotiationOptions;
  Report: TGraphSelectiveNegotiationReport;
begin
  Options := DefaultGraphNegotiationOptions;
  Options.SolveOptions.MaxBacktracks := 1024;
  Options.SolveOptions.CaptureTrace := True;
  Options.MaxPassBacktracks := 32;

  if not Graph.TryRegenerateNegotiatedFrom(
      ['hydrology'], Options, Report) then
    WriteLn('repair ended with status ', Ord(Report.Search.Status));
end;
```

The single-label overload is equivalent to a one-element
`TGraphPassLabels` array. As with ordinary selective regeneration, at least
one root is required, every label must exist, and negative local or pass
backtrack bounds are rejected before a solve round begins.

## the exact repair horizon

Let the pipeline contain dependency edges `provider -> consumer`.

First resolve every requested label to its stable pass index. Let `R` be those
indices after removing duplicates and sorting by stable creation index. The
active set `A` is the least set satisfying:

```text
R is a subset of A
if provider is in A and provider -> consumer, then consumer is in A
```

Dependency roles implied by compatibility behavior such as
`RequirePrevious` are synchronized before the descendant closure is computed.
`A` is therefore the union of the requested roots and all of their transitive
dependency descendants, not an index suffix and not an ancestor closure.

`RequestedRootIndices` records `R`. `ActivePassIndices` records `A` in the
stable full-pipeline topological execution order. The two arrays intentionally
use different canonical orders: root identity is independent of caller label
order, while active order describes actual scheduling and chronological frame
selection.

The requested roots are the earliest passes the caller explicitly authorizes
to change. An ancestor or provider outside `A`:

- is reused from its committed pre-call state;
- never becomes a negotiation choice frame;
- retains its entry values, empty state, and generated/lock ownership;
- retains its random-stream state; and
- is not added merely because a contradiction cites it.

This is the **repair horizon**. A request rooted at a leaf contains only that
leaf. If the leaf is impossible under a clean provider, the search can exhaust
or contradict even when changing that provider would admit a solution. The
caller must name the provider, or another sufficiently early pass, as an
explicit root to authorize the wider descendant closure.

### branches and joins

For this graph:

```text
terrain -> hydrology --\
                         > roads -> housing
terrain -> biome -------/
```

requesting `hydrology` activates `hydrology`, `roads`, and `housing`. `terrain`
and `biome` remain clean immutable inputs, even though `roads` reads both.
Requesting `terrain` activates the complete shown graph. Multiple requested
roots form one deduplicated union; their overlapping descendants execute only
once per round.

## search inside the horizon

Every negotiation round is an ordinary atomic selective attempt over `A`:

1. Clean passes are staged as reused immutable inputs.
2. Active passes execute in stable topological order.
3. If the active closure succeeds, it commits once and the search ends.
4. If it fails, the coordinator scans that round's active execution order
   backward for the latest completed defined mutable pass.
5. The complete stable value-index assignment of that pass is recorded and
   excluded, later active exclusions are cleared when an earlier prefix
   changes, and another selective round begins.

Definitionless passes and passes made entirely from caller locks are not
choice frames. A clean pass is never a choice frame regardless of whether it
is defined or mutable in some future call. The terminal failed pass and passes
that did not complete are also unavailable, exactly as in full negotiation.

Frame selection remains global chronological **within `ActivePassIndices`**.
It is not conflict-directed: the selected frame need not equal the provider
named by the terminal contradiction. The scope prevents the search from
choosing an unrelated clean pass, but it does not make the active repair
minimal.

## two distinct budgets

Selective negotiation reuses `TGraphNegotiationOptions` unchanged:

- `SolveOptions.MaxBacktracks` limits local reference-solver restoration per
  active pass and per round; and
- `MaxPassBacktracks` limits complete active pass assignments rejected by the
  outer search.

There is no third implicit scope budget. Encountering a previously excluded
complete assignment is a local contradiction, so accumulated outer exclusions
also consume local backtracks while the pass advances to another assignment.
A positive outer budget cannot compensate for a zero or exhausted local
budget.

With `MaxPassBacktracks = 0`, the method performs exactly one ordinary
selective-regeneration attempt over `A`. `Search.Attempts` is empty and
`Search.PassBacktracks` is zero. Its `Search.FinalReport` is identical to the
ordinary `TryRegenerateFrom` terminal report for the same graph, canonical
roots, solve options, and pre-call state. If that ordinary attempt failed after
an active choice frame completed, the outer status is
`gnsPassBacktrackLimit`; that status does not alter the ordinary final report.

## atomicity and clean-state invariants

Rejected rounds do not commit. A successful call commits the terminal active
closure once. On success:

- every pass outside `ActivePassIndices` retains its exact pre-call entries,
  empty flags, and generated/lock ownership;
- every clean pass retains its exact pre-call random-stream state;
- caller domains and locks remain caller-owned; and
- caller pass selection is restored.

If the call ends in contradiction, local backtrack exhaustion, pass-backtrack
exhaustion, malformed input, or a commit-hook exception, every pass entry,
ownership flag, random stream, and caller selection is restored to its
pre-call state. Failed scope preflight, including an unknown label, cannot
partially regenerate the pipeline.

Active streams follow ordinary selective replay: they rewind from their stable
creation-index-derived seeds for each attempted transaction and retain only
the terminal successful transaction's committed stream state. Clean streams
are neither rewound nor consumed.

## report and transcript

`TGraphSelectiveNegotiationReport` contains:

| Field | Meaning |
| --- | --- |
| `ScopeAlgorithmVersion` | The selective-scope algorithm used by this call. |
| `RequestedRootIndices` | Deduplicated requested roots in ascending stable index order. |
| `ActivePassIndices` | Exact descendant closure in stable topological execution order. |
| `Search` | The complete ordinary `TGraphNegotiationReport` for search inside the active closure. |
| `TranscriptHash` | Portable hash of the scope identity and nested search transcript. |

`Search.Status`, `Search.Attempts`, `Search.FinalReport`, exact copied
assignments, ordinary round traces, and both backtrack counters retain the
meanings documented for full negotiation. Every ordinary round report contains
only the passes actually executed in its `ExecutionOrder`; clean layers are
represented through ordinary reused-pass dispositions and optional skip trace
events rather than being misreported as executed work.

`CalculateGraphSelectiveNegotiationTranscriptHash` recomputes the outer
transcript. Its versioned input includes:

- the selective negotiation algorithm and hash versions;
- `ScopeAlgorithmVersion`;
- the complete canonical requested-root array;
- the complete active-pass array; and
- a fresh recomputation of the nested Pass Negotiation v1 transcript from the
  supplied options and `Search` report.

The calculator does not trust the stored nested `Search.TranscriptHash`.
Likewise, the resulting 32-bit FNV-1a value is a portable replay diagnostic,
not assignment identity or cryptographic integrity. Exact arrays and ordinary
attempt reports remain authoritative.

## replay identity and version isolation

Exact replay requires the ordinary graph, model, pipeline, solver, random, and
trace inputs; both ordinary pass-negotiation versions and budgets; the two
selective-negotiation versions; and the canonical root and active arrays.
Caller label order and duplicate labels canonicalize to the same root indices,
scope, search, and outer transcript.

Selective Negotiation v1 is layered rather than folded into an existing
version:

- `TrySolve` and `TryRegenerateFrom` retain Pipeline v2 semantics and goldens;
- `TrySolveNegotiated` retains Pass Negotiation v1 semantics and goldens;
- the nested `Search` report remains an ordinary Pass Negotiation v1 report;
  and
- only the explicit selective wrapper adds scope versions, scope arrays, and
  the outer transcript.

An incompatible change to root canonicalization, closure construction, clean
state handling, active frame selection, or outer hash encoding requires a
selective version change. It does not silently redefine either pre-existing
algorithm.

## portable fixture

The dependency-free
[`NegotiatedRepair`](../examples/2D/04_NegotiatedRepair/README.md) fixture makes
the horizon boundary executable on native FPC and pas2js/Node. With seed `0`,
a housing root reports `[3] -> [3,4]`, fails without a choice frame, and has
outer transcript `7A595E38`. A roads root reports `[2] -> [2,3,4]`, rejects the
complete `roads=[trail]` assignment, and commits `plaza | market |
market-lights` while preserving terrain and climate. Its nested Pass
Negotiation v1 transcript is `80926222`; its selective-scope transcript is
`E29050A0`. The isolated five-pass full-pipeline comparison is `9DB789E4`;
the pre-existing one-cell Pass Negotiation v1 golden remains `6F76591D`.

The focused conformance suite additionally pins duplicate/reordered root
canonicalization, zero-budget ordinary-final-report parity, caller-lock
ownership, malformed-input atomicity, full Negotiation v1 isolation, and a
contextual join that clears later exclusions when an earlier active prefix
changes. That join's outer transcript is `DE454137`.

## complexity and deliberate limits

Selective scope can avoid searching unrelated passes, but it does not change
the worst-case nature of exact chronological enumeration. The number of
complete assignments for active passes remains exponential in cell and value
counts, and every rejected vector is retained whole. Finite local and outer
budgets are mandatory operational controls.

Version 1 does **not** claim:

- a cell-minimal repair;
- the fewest changed passes or assignments;
- conflict-directed backjumping or clause learning;
- partial-assignment, coordinate-level, or generalized nogoods;
- an automatically minimal or sufficient repair horizon;
- cyclic dependency repair;
- negotiated changes to clean providers; or
- superiority to an equivalent flattened model.

The implementation, reports, hashing, native and pas2js examples, and
conformance fixtures use repository units plus the applicable standard RTL
only. No external solver, graph package, serializer, or runtime service defines
the algorithm.

The experimental fixtures and measured observations are recorded in
[Selective Pass Negotiation v1 research notes](research/selective-pass-negotiation-v1.md).
