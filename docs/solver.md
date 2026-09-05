# reference solver

`TGraph.TrySolve` is the opt-in propagating solver. It maintains a domain for
every cell, propagates constraints to a fixed point, observes by deterministic
weighted Shannon entropy, and uses bounded chronological backtracking when a
decision causes a contradiction. Unit-weight models take the exact original
minimum-remaining-values path. The existing `TGraph.Run` traversal remains
available for source and behavior compatibility.

The reference solver coordinates the pass dependency DAG. A full solve stages
every pass in stable topological order and commits entry values only after all
executed assignments have been validated. A selective solve stages only the
requested roots and their transitive dependents while reusing skipped layers
as immutable inputs.

`TGraph.TrySolveNegotiated` is a separate opt-in coordinator around complete
ordinary pipeline attempts. It can chronologically reopen completed pass
assignments after a downstream contradiction without changing `TrySolve` or
`TryRegenerateFrom`. See [bounded pass negotiation](pass-negotiation.md).
`TGraph.TryRegenerateNegotiatedFrom` restricts that same search to an explicit
descendant-closed repair horizon while preserving every clean layer and random
stream; see [selective pass negotiation](selective-negotiation.md).

## basic use

```pascal
uses
  SysUtils,
  wfc;

var
  Graph: TGraph;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
begin
  Graph := TGraph.Create.Reshape(16, 16, 1);
  try
    Graph.Seed := $DEADBEEF;
    Graph.AddValue('land', 4)
      .NewRule(AllDirections, ['land', 'water']);
    Graph.AddValue('water', 1)
      .NewRule(AllDirections, ['land', 'water']);

    Options := DefaultGraphSolveOptions;
    Options.MaxBacktracks := 1024;
    if not Graph.TrySolve(Options, Report) then
      WriteLn('Unsolved pass: ', Report.FailedPassIndex);
  finally
    Graph.Free;
  end;
end;
```

`DefaultGraphSolveOptions` currently permits 256 backtracks per pass. A value
of zero permits propagation and first choices but stops at the first required
branch restoration. A negative value raises `ERangeError`.

`DefaultGraphNegotiationOptions` embeds those solver defaults and permits 64
outer pass backtracks. Its local and pass budgets have different units and
must both be initialized and persisted for replay.

Always initialize a local `TGraphSolveOptions` with
`DefaultGraphSolveOptions` before overriding fields. Local Pascal records are
not guaranteed to be zeroed; assigning only one field can leave another field
undefined.

## pipeline transaction

`TrySolve` performs one transaction over the complete dependency plan:

1. save the selected pass and every pass random-stream state;
2. rewind the versioned per-pass streams from `Seed`;
3. build a stable topological order from pass dependencies;
4. solve, clear, or copy each pass according to its mode into a staging buffer;
5. validate each solved assignment independently;
6. tentatively write every staged value under an exact entry/RNG snapshot;
7. run any derived whole-pipeline final validator; and
8. publish the commit only after the complete pipeline succeeds.

A definitionless legacy pass zero remains exactly as supplied. A later
definitionless legacy pass copies its predecessor; a definitionless transform
copies its selected source; and a definitionless overlay clears generated
cells. In every mode, caller locks take precedence. An entry with a nonempty
value and `Generated = False` is a caller lock. Existing generated output is
not a lock and is regenerated.

`TryRegenerateFrom` uses the same transaction over the requested pass roots and
their transitive dependents. Skipped entry state and random streams remain
unchanged. See [pass DAGs](pass-dags.md) for mode-specific staging and the
selective closure definition.

When `TrySolve` returns `False`, entry values, `Generated` flags, selected pass,
and pre-call random-stream states are unchanged. A later-pass contradiction
therefore cannot leave an earlier pass half committed. A successful call marks
new solver output as generated while preserving caller locks.

For ordinary `TrySolve`, atomicity is a commit/rollback guarantee, not a
flattened global search across every pass. `MaxBacktracks` is applied
independently while solving each pass. Once a provider has been staged and the
coordinator moves to a consumer, a consumer contradiction can backtrack within
that consumer and can roll the pipeline transaction back, but it does not
reopen the provider's choices.

`TrySolveNegotiated` supplies that reopening through separately versioned,
whole-assignment chronological search. Every rejected round remains atomic and
only the final successful round commits. It does not add cyclic edges or
reinterpret an ordinary solve; see [the negotiation contract](pass-negotiation.md).

`TryRegenerateNegotiatedFrom` stages ordinary selective rounds instead. Its
canonical requested roots and transitive descendants are the complete active
set. Completed mutable passes outside that set cannot be reopened, even when a
failure names one as a dependency provider. A successful call commits only the
active closure once; clean entries, ownership, and random streams remain
unchanged.

Entry setters are still used during commit so derived entry behavior remains
available. Each hook observes the pass currently being committed. If a setter
raises, or any entry differs from its exact staged `Value`, `Empty`, and
`Generated` state after all setters finish, those three fields are restored and
the exception is re-raised. A transient rewrite of a later solver-owned entry
can therefore be overwritten by that entry's own commit. Side effects outside
those three fields cannot be undone. Commit hooks must not mutate topology,
rules, pass configuration, or other model state; such mutations are unsupported,
and validation applies to the prepared pre-commit snapshot.

A derived graph may override protected `DoValidateCommit` to inspect the
complete tentatively written candidate as one domain object. Returning `False`
with a valid active failed pass and optional entry produces
`gckFinalValidation`,
restores every entry and random stream, and returns an ordinary failed solve.
Pass negotiation can therefore exclude that exact rejected assignment and
continue within its existing budget. Returning `True` is followed by the same
exact staged-entry mutation guard used for setter hooks. The override must be
deterministic and inspection-only: external side effects are not
transactional, and model/topology mutation while a pipeline is running is not
supported. The default implementation accepts the candidate, so ordinary
`TGraph` behavior and replay remain unchanged.

## constraint semantics

For a cell value `C`, a neighbor value `N`, and direction `D` from the cell to
that neighbor, define `Allows(S, D, T)` as the directional rule on source value
`S` permitting target value `T`. An absent directional rule or a rule with an
empty value list is a wildcard, preserving the original public contract.
`Rules[S].DenyAll([D])` is the distinct explicit state that makes
`Allows(S, D, T)` false for every `T`. Reference-solver compatibility is the
two-sided conjunction:

```text
Allows(N, D, C) and Allows(C, Inverse(D), N)
```

This makes compatibility independent of which endpoint is assigned first.
Every directional arc is checked separately, even when wrapping makes several
directions point to the same physical entry. A wrapped self-arc supports only
the same candidate value; one candidate cannot use a different candidate as
its own support. A nil neighbor imposes no adjacency constraint, including for
an explicitly denied direction.

`DenyAll` removes finite support in the selected directions and synchronizes
the corresponding inverse support maintained by the fluent graph builder. If
an inverse allow-list loses its final value, it becomes explicitly denied
rather than a present-empty wildcard. A later `NewRule` in that direction
restores finite support, including its inverse. Directly replacing the public
`Rules` array does not implicitly change `DeniedDirections`; defining and
denying the same direction is malformed state and is rejected before solving.

Required rules retain their force/trigger meaning. A value whose rule group
contains a required rule is required-only. Unless the cell is a caller lock,
that candidate needs at least one physical neighbor whose nonempty required
directional rule explicitly lists it. Support is existential across the six
directions and must also be an adjacency-compatible pair. Nil neighbors cannot
support a candidate, and a self-arc can support only that same candidate.
Caller locks are exempt from required-support eligibility, matching the legacy
behavior, but they still have to satisfy adjacency and previous-pass
constraints.

`RequirePrevious` is an additional unary constraint. It compares a candidate
with the staged value at the same coordinate in the immediately preceding
pass. Thus later passes always read the result of the current transaction, not
stale output left by an earlier run.

The reference solver rejects the empty string as a registered model value. It
is reserved for the public empty-entry state.

## caller-owned entry domains

`SetAllowedValues(X, Y, Z, Values)` assigns a pass-local initial domain to one
entry. The array overload removes duplicates and stores values in that pass's
`AddValue` order; the single-value overload creates a singleton. All supplied
values are validated before mutation, so an unknown value leaves the prior
domain unchanged. `CopyAllowedValues` returns detached storage,
`HasAllowedValues` distinguishes assigned state, and `ClearAllowedValues`
removes it.

An assigned empty array means “allow nothing”; it is not the same as clearing
the domain. `Run` intersects its valid-value list with the assigned domain.
`TrySolve` intersects locks, the caller domain, pass requirements, and
propagated adjacency. An empty domain or a caller lock outside its domain
returns `gckEntryDomain` with the zero-based entry index. A nonempty domain
that is later eliminated by adjacency retains the more specific propagated
contradiction kind. A definitionless pass has no local value registry, so its
only assignable domain is the explicit empty domain, which contradicts its
staged preserved, copied, or cleared state before commit.

Domains persist across `Run`, `TrySolve`, selective regeneration,
`Entry.ClearValue`, and the public entry `Reset`; resetting the graph or
reshaping replaces their storage. They belong to their individual pass and
never propagate or copy to another pass. Full and selective failures leave
every caller domain unchanged. Domain mutation through a selection,
invalid-state, or commit hook while the pipeline is running is rejected with
`EInvalidOperation`.

## deterministic algorithm

After initial lock and `RequirePrevious` filtering, the solver repeatedly
propagates adjacency and required-support removals through a queue until no
domain changes.

Every registered value has a positive pass-local `Integer` weight. The
one-argument `AddValue` overload assigns weight `1`; the two-argument overload
sets an explicit relative frequency, and `Rules[Value].Weight` may update
it later. Calling the one-argument overload for an existing value never resets
its weight. Zero and negative weights raise `ERangeError` and do not change the
group. Weights bias choices only in `TrySolve`; legacy `Run` remains uniform
unless its caller supplies a custom selection callback.

Before solving a defined pass, the solver divides the complete raw weight
vector by its greatest common divisor. Consequently `[1, 2]` and `[2, 4]` have
the same replay identity, random bounds, reports, and output. The normalized
sum must fit in `Integer`; an overflow is malformed model state and raises
without committing entries or advancing the caller-visible streams. A
definitionless pass has no solver weight vector and retains its copy semantics.

If every normalized weight is `1`, the solver executes its original
minimum-remaining-values observation path verbatim. Otherwise it observes the
uncollapsed cell with the lowest Q16 approximation of base-2 Shannon entropy:

```text
W = sum(weight)
L = sum(weight * Log2Q16(weight))
EntropyQ16 = floor((W * Log2Q16(W) - L) / W)
```

`Log2Q16` is computed with shifts and sixteen fixed-point squaring steps, not a
host math-library logarithm. All intermediate integer-valued operations stay
within the exact range of an IEEE `Double` on native FPC and JavaScript. Equal
or Q16-colliding scores keep the first cell in pipeline `Mode` Z order and then
ascending entry order; there is no random entropy noise or secondary tie-break.

Candidates retain `AddValue` order. Each decision draws one unbiased ticket in
the active normalized weight sum and maps it through cumulative weights. That
candidate becomes the starting offset of a cyclic candidate order. Failed
alternatives are restored from a removal trail and retried in that frozen order
without another draw. Thus weights bias the first branch while chronological
backtracking remains complete and reproducible. Unit weights draw with the old
domain-count bound and preserve the exact previous candidate order.

Negotiated rounds can pass exact complete-assignment exclusions into this
kernel. An exclusion is tested only after every cell has a value. Matching it
records an entryless `gckExcludedAssignment` contradiction, increments
`ExcludedAssignments`, and uses ordinary chronological restoration to reach
the next assignment. Replaying accumulated exclusions therefore consumes
local `MaxBacktracks`; the outer `MaxPassBacktracks` budget does not replace or
subsidize those restorations. Ordinary `TrySolve` supplies no exclusions and
retains its prior assignments, counters, and traces.

`WFC_SOLVER_ALGORITHM_VERSION = 2` identifies these propagation, observation,
candidate-ordering, and backtracking rules.
`WFC_GRAPH_MODEL_VERSION = 1` identifies the additive deny-all and caller-domain
semantics without reinterpreting legacy wildcard rules. A replay identity for
`TrySolve` includes graph-model, solver, and random algorithm versions, the
seed, graph topology and mode, pass order, value/rule construction order,
denied directions, caller domains in canonical value order, Pipeline v2
cross-pass clauses with signed offsets and ordered any-terms, canonical
normalized weights, locks, and solve options. Every call rewinds its streams,
so an unchanged graph and seed replay on both native FPC and pas2js. A failed
call restores the stream state that existed before the call.

That identity guarantees replay of solver decisions. Whole-call completion and
external side effects also require deterministic entry-setter hooks with the
same configuration and external state. Hooks can raise or perform effects that
the solver neither controls nor includes in its algorithm identity.

Legacy callbacks and traversal hooks are deliberately outside this algorithm.
`TrySolve` does not invoke `SelectionCallback`, `InvalidStateCallback`,
`DoGetStartCoord`, `DoGetSelection`, or `DoValidate`.

## reports and failures

`TGraphSolveReport.Status` is one of:

- `gssSolved`: every pass validated and committed;
- `gssContradiction`: the search proved the current bounded search space
  contradictory; or
- `gssBacktrackLimit`: another failed branch needed restoration after the
  configured limit had been reached.

The report records `Seed`, `RandomAlgorithmVersion`,
`SolverAlgorithmVersion`, `GraphModelVersion`, `PipelineAlgorithmVersion`,
executed `ExecutionOrder`, `FailedPassIndex`, terminal contradiction evidence,
one `TGraphPassSolveReport` per pass, and the opt-in causal-trace fields.
Per-pass records identify whether and when a pass executed and whether it was
reused, cleared, copied, solved, or failed.
Terminal contradiction kinds are
`gckInvalidLock`, `gckEntryDomain`, `gckEmptyDomain`, `gckAdjacency`,
`gckPreviousPass`, `gckPassDependency`, `gckRequiredSupport`, and
`gckFinalValidation`. The latter covers both the reference solver's final
assignment relation check and a derived `DoValidateCommit` rejection. A
negotiated pass attempt can additionally report
`gckExcludedAssignment` when every locally reachable complete assignment is
among its exact outer exclusions. `gckNone` is used on success. Named
dependency failures also identify the stable source index through
`DependencyPassIndex`.
Entry and neighbor indices are zero-based. Inspect `HasDirection` before using
the direction field. `FailedPassIndex` and unavailable contradiction pass,
entry, or neighbor indices are `-1`. Report contents are unspecified when
`TrySolve` raises instead of returning.

Per-pass counters have deliberately narrow definitions:

- `Decisions` counts every attempted branch alternative, including retries;
- `Propagations` counts candidate removals caused by adjacency or
  required-support propagation;
- `Contradictions` counts invalid-lock failures, zero-domain discoveries, and
  final-validation or exact-assignment-exclusion failures;
- `Backtracks` counts completed failed-branch restorations; and
- `ExcludedAssignments` counts complete local assignments rejected by exact
  negotiation exclusions.

Counters saturate at `High(Integer)` so exceptionally large searches retain
the same report behavior under checked native builds and pas2js.

Initial domain construction for locks and `RequirePrevious`, and the direct
restriction to a chosen branch value, are not propagations. A solved pass may
have nonzero contradiction and backtrack counters if it recovered from failed
alternatives; the report's terminal contradiction is cleared on success.

Negotiation has its own `TGraphNegotiationStatus` because the two limits must
not be conflated. `gnsSolverBacktrackLimit` identifies an exhausted local pass
search; `gnsPassBacktrackLimit` identifies an exhausted outer assignment
budget. `gnsContradiction` means no completed negotiable choice frame remains,
and `gnsSolved` means the final complete round committed. Rejected rounds live
in `Attempts`; the sole terminal round lives in `FinalReport`. See
[statuses and reports](pass-negotiation.md#statuses-and-reports).

Selective negotiation nests that report in
`TGraphSelectiveNegotiationReport.Search`. The wrapper additionally records
canonical requested-root indices, the active topological closure, a separate
scope algorithm version, and an outer transcript hash. With a zero pass budget,
`Search.FinalReport` retains exact ordinary `TryRegenerateFrom` parity for the
same roots, solve options, and initial graph state.

## causal traces

`TGraphSolveOptions.CaptureTrace` enables a deterministic chronological record
of the complete attempted transaction. The default is `False`. With capture
disabled, `TraceCaptured` is false, `TraceHash` is zero, `Trace` is empty, and
each pass reports `TraceStart = -1` and `TraceCount = 0`. Assignments, status,
counters, and random-stream positions remain identical to the non-tracing path,
and the solver does not allocate its causal arrays.

With capture enabled, `TGraphSolveReport.Trace` records caller-domain and lock
filtering, decisions, propagated removals, contradictions, backtracks,
restorations, pass begin/stage/fail/skip events, and the final pipeline commit
or rollback. Every `CauseEventId` is either `-1` or a strictly earlier event.
Cross-pass candidate removals identify both consumer and provider and link to a
provider-pass event, so a downstream rejection can be followed back across the
pass DAG. Abandoned branches remain in the trace even when a later alternative
solves.

An exact whole-assignment exclusion is an entryless contradiction with cause
`gtckExactAssignmentExclusion`. During negotiated solving, each round retains
one ordinary Trace-v1 report; the coordinator never interleaves revisited pass
events into a single trace.

Each `TGraphPassSolveReport` exposes its contiguous half-open trace slice using
`TraceStart` and `TraceCount`; the final pipeline event is outside every pass
slice. `WFC_TRACE_VERSION` versions the event schema, and
`WFC_TRACE_HASH_VERSION` versions the portable numeric signature.
`CalculateGraphTraceHash` recomputes it without depending on host string
encoding.

The project-owned `wfc_trace` unit provides stable event/cause names, event-ID
lookup, detached pass/entry subsets, entry-index-to-coordinate conversion, a
line-safe formatter, and `ValidateGraphTrace`. Validation checks event IDs and
causes, graph and value bounds, dependency links, per-pass slices, field
invariants, terminal status, and the recomputed hash. See
[causal solve traces](traces.md) for the complete schema and the shared
native/pas2js terrain -> settlement -> foliage inspector.

Trace v1 retains the whole attempted search in memory. It does not yet provide
interactive stepping, live domain snapshots, every failed term of a compound
cross-pass clause, minimal-unsatisfiable-core extraction, an event cap, or a
streaming sink.

Impossible but structurally valid input returns `False`, including unknown
caller locks on a defined solver pass and unsatisfied constraints. Malformed
model or topology state raises before committing entries. Examples include a
negative backtrack limit, an empty registered symbol, a rule target outside the
value registry, duplicate directional records in a directly replaced public
rule array, a direction that is both explicitly denied and directly defined,
a normalized pass-weight sum larger than `High(Integer)`, or, on a defined
pass, a neighbor object outside that pass's own entry storage.
Definitionless passes copy or preserve caller state without compiling a solver
model. The external-neighbor restriction freezes the topology used by the
current reference solver; the legacy `Run` path continues to support public
external links.

## implementation unit

`wfc_solver_reference` contains the flattened, string-free kernel used by the
public `wfc` facade. It is installed because package managers must compile it,
but its record layout and helper declarations are implementation details and do
not carry a source-compatibility promise. Applications should use
`TGraph.TrySolve`; direct kernel use is reserved for framework development and
tests until a separately versioned low-level API is deliberately published.

## current scope

Solver version 2 provides deterministic integer weights and fixed-point
Shannon observation; graph-model version 1 adds explicit deny-all adjacency
and caller-owned entry domains; pipeline version 2 adds acyclic dependency
planning, named same-coordinate and signed-offset requirements, explicit
finite any-of-neighborhood clauses, pass modes, and selective regeneration.
The separate count-range extension bounds matching offsets or distinct
provider cells over an explicit finite stencil; see [pass counts](pass-counts.md).
Causal Trace v1 adds stable native/pas2js hashes and public inspection and
validation helpers. Pass Negotiation v1 adds bounded full-pipeline
chronological search over exact completed pass assignments with a separately
versioned transcript. Selective Negotiation v1 adds a separately versioned,
explicit descendant-closed repair horizon around that same search while
keeping clean passes immutable. The separate [restart coordinator](restarts.md)
adds deterministic whole-transaction retries after local-budget exhaustion and
optional diagnostic elapsed timing, leaving ordinary solve reports unchanged.
Automatic horizon expansion, conflict-directed or cell-minimal repair,
local-pass or selective restarts,
interactive stepping, bounded or streaming trace capture, soft constraints,
inferred neighborhoods and distance expressions, cyclic repair, richer failed-clause
evidence, and minimal-unsatisfiable-core analysis remain roadmap work rather
than hidden or partially specified behavior.
