# Pattern-Projected Pass Composition v1 research record

Execution note: pas2js figures below are historical measurements. Maintained
execution paths are native FPC and the documented browser demos; those earlier
figures do not establish browser coverage for this experiment.

- **Status:** experimental, measured, reproducible representation bridge
- **Graph adapter:** `WFC_PATTERN_2D_GRAPH_ADAPTER_VERSION = 1`
- **Reusable owner:** `WFC_PATTERN_2D_PASS_PIPELINE_VERSION = 1`
- **Composition signature:** `WFC_PATTERN_2D_COMPOSITION_SIGNATURE_VERSION = 1`
- **Pattern extraction:** `WFC_OVERLAPPING_2D_ALGORITHM_VERSION = 1`
- **Independent projection:** `WFC_OVERLAPPING_2D_PROJECTION_VERSION = 1`
- **Pipeline contract:** `WFC_PIPELINE_ALGORITHM_VERSION = 2`
- **Published fixture:** `examples/2D/05_LearnedPatternWorld`
- **Published seed set:** `{0}`
- **Version-1 boundary:** wrapped, depth one, same-shape, hard constraints

## question and hypothesis

An overlapping-pattern solve does not produce the public token grid that its
sample appears to describe. It produces a grid of private pattern anchors.
Each anchor selects a complete rectangular payload, and the public token at
one coordinate is the agreement of all payload cells that project there.

The research question is:

> Can that many-contribution projection be materialized as an ordinary public
> pass, inside the same atomic dependency DAG as its latent pattern provider
> and later semantic consumers, using only the existing exact cross-pass
> clause algebra?

The version-1 hypothesis is deliberately narrow. For a wrapped, depth-one
anchor grid, one exact-offset hard clause per footprint coordinate and public
token should reproduce the existing independent projector exactly. The
materialized public pass should then be safe for ordinary downstream passes to
consume, without exposing private pattern keys, changing the latent pattern
distribution, or weakening whole-pipeline rollback.

The experiment tests representational fidelity, transaction boundaries,
portable replay, and configuration cost. It does not hypothesize a speedup,
an optimal output, a learned semantic layer, or a general projection or
resampling language.

## the representation mismatch

`TWfcOverlappingModel2D.CompiledModel` uses one private graph value per latent
pattern. Its stable keys have forms such as `@p0`, `@p1`, and so on. A key
identifies a complete `PatternWidth x PatternHeight` row-major palette-index
payload; it is not a palette token and must not be rendered, serialized as
domain output, or compared directly with a semantic value such as `water`,
`land`, or `road`.

The existing standalone path makes the boundary explicit:

1. solve a graph whose cells contain private pattern keys;
2. capture and validate the latent pattern-index grid;
3. project every payload contribution into a public token grid; and
4. independently validate that projection.

That path is an authoritative oracle for this experiment, but projection
happens after the graph transaction. A downstream terrain, foliage, or
structure pass cannot read that post-solve array while participating in the
same staged commit. Conversely, pretending that the latent pass already
contains public tokens loses the footprint coordinate and is semantically
wrong.

Pattern-Projected Pass Composition v1 introduces a real defined
`gpmOverlay` pass between those representations:

```text
private latent patterns -> materialized public projection -> semantic passes
```

The reusable two-pass owner uses the labels `patterns` and `projection`. The
published four-pass world fixture uses the lower-level adapter and names the
same public boundary `terrain`:

```text
patterns -> terrain -> foliage
                    \-> structure
```

`terrain`, `foliage`, and `structure` contain public tokens only. Downstream
passes depend on `terrain`; they never depend on or decode the `patterns`
layer.

## exact wrapped coordinate contract

Let the latent anchor grid and materialized public grid both have width `GW`,
height `GH`, and depth `1`. Let the pattern footprint have width `PW` and
height `PH`. Coordinates are zero based:

```text
0 <= x < GW, 0 <= y < GH
0 <= px < PW, 0 <= py < PH
z = 0
```

Define mathematical floor-modulo, which is always nonnegative, as:

```text
wrap(n, i) = i - n * floor(i / n), for n > 0
```

Thus `0 <= wrap(n, i) < n`, including when `i` is negative. This equation is
the contract; it does not rely on a host language's negative-remainder rule.
The core signed-offset resolver implements the equivalent checked operation
without overflowing unsigned graph coordinates.

For latent anchor `a = (ax, ay, 0)`, footprint coordinate `(px, py)` writes to
public coordinate:

```text
project(a, px, py) =
  (wrap(GW, ax + px), wrap(GH, ay + py), 0)
```

Equivalently, the anchor whose `(px, py)` payload cell contributes to public
coordinate `c = (x, y, 0)` is:

```text
anchor(c, px, py) =
  (wrap(GW, x - px), wrap(GH, y - py), 0)
```

The provider offset declared on the public pass is therefore exactly:

```text
MakeGraphOffset(-px, -py, 0)
```

All footprint coordinates contribute, even when a footprint dimension is
larger than the graph dimension and two declared offsets resolve to the same
wrapped anchor. They remain distinct semantic clauses because they inspect
different cells of that anchor's payload.

Version 1 requires `WrapNeighbors = True`, positive `GW` and `GH`, and graph
depth exactly `1`. It does not define the expanded open projection of size
`(GW + PW - 1) x (GH + PH - 1)`, because every pass in one `TGraph` must share
one shape.

## hard-clause compilation

Let `payload(p, px, py)` be the public palette token stored at footprint
coordinate `(px, py)` of latent pattern `p`, and let `key(p)` be its private
graph key. For public token `t`, define the complete supporting set:

```text
support(t, px, py) =
  { key(p) | payload(p, px, py) = t }
```

For every public token `t` and every footprint coordinate `(px, py)`, the
adapter adds one exact-offset clause to the materialized projection value:

```text
provider[anchor(c, px, py)] is in support(t, px, py)
```

In the existing fluent API this is equivalent to:

```pascal
ProjectionRulesFor(t).RequireFromPassAt(
  PatternPass,
  MakeGraphOffset(-px, -py, 0),
  support(t, px, py));
```

Values inside one support set are alternatives and therefore OR together.
The separate calls for all footprint coordinates are conjunctive. A public
candidate is consequently legal at `c` exactly when:

```text
allowed(t, c) =
  AND over py = 0 .. PH - 1
  AND over px = 0 .. PW - 1
    (latent[anchor(c, px, py)] is in support(t, px, py))
```

Ordinary same-layer rules or additional pass requirements on the public pass
remain conjunctive with this expression. The bridge neither adds a callback
predicate nor gives the solver access to opaque model objects.

The adapter registers public palette tokens in stable palette order with the
unit `WFC_DEFAULT_VALUE_WEIGHT`. Learned frequency belongs to the latent
pattern states and is already represented by their weights. Importing another
frequency distribution into the projection pass would count the same
observations twice. Unit public weights are therefore part of the adapter-v1
identity, even though a valid latent assignment normally leaves exactly one
public candidate at each cell.

For a palette of size `T`, footprint area `PW * PH`, and `P` latent patterns,
the uncompressed bridge contains:

```text
public values       = T
exact-offset clauses = T * PW * PH
support references   = P * PW * PH
```

The last equality follows because every latent pattern contributes to exactly
one token's support set at every footprint coordinate. Version 1 deliberately
retains this explicit representation so its cost and causal identity remain
inspectable.

Before the first mutation,
`ValidateOverlappingProjectionFromPass2D` checks the complete applied latent
graph semantics (ordered keys, weights, directional rules, and denials), source
and target pass identities, shared root and shape, wrapped depth-one topology,
dependency direction and cycle safety, the empty target definition, public
palette conversion, reserved-key separation, and every support set needed by
the complete unit-weight target definition. Two model wrappers with identical
compiled latent semantics are intentionally interchangeable at this graph
boundary; the result signature still includes the full palette and payload
interpretation.
`ApplyOverlappingProjectionFromPass2D` performs the same complete preflight
before registering the public values and clauses. A malformed later term
cannot leave an earlier clause installed.

## private latent/public boundary

The boundary is an invariant, not a naming convention:

- `patterns` stores only stable private pattern-index keys;
- `projection` or the fixture's `terrain` pass stores only public palette
  tokens;
- the bridge expands private keys only while compiling hard support sets or
  independently validating a solved composition;
- downstream passes name the public pass as their provider;
- public captures and signatures contain no private keys; and
- canonical `wfcp=1` artifacts continue to contain palette tokens and pattern
  payloads, never compiled private graph keys.

The projection adapter reserves public tokens matching `@p` followed by a
nonempty decimal-digit suffix, including leading-zero lookalikes. Rejecting
that lexical collision keeps diagnostics, captures, and downstream public
values unambiguously separate from latent graph keys. Standalone pattern
learning and projection do not need this graph-bridge restriction.

`CaptureSolvedOverlappingProjectionPass2D` independently captures the latent
assignment, validates every wrapped overlap, recomputes every footprint
contribution, and compares the complete materialized public pass. It does not
trust the clauses merely because they were installed by the adapter.

## transaction and final-validation contract

The lower-level adapter and the reusable owner provide two intentionally
distinct validation surfaces.

For an arbitrary caller-owned `TGraph`, the adapter compiles ordinary hard
clauses. `TrySolve`, `TryRegenerateFrom`, and the negotiated entry points then
receive their normal Pipeline-v2 atomicity: all selected passes stage before
one commit, and a solver contradiction or limit publishes none of their
generated values. After a successful call, a caller may use
`CaptureSolvedOverlappingProjectionPass2D` as an independent post-solve
validator. At that lower level a validation call made after commit is evidence;
it does not retroactively undo the successful transaction.

`TWfcPattern2DPassPipeline` strengthens that boundary. Its graph override runs
the same independent latent capture and public projection comparison from
`DoValidateCommit`, after the complete candidate has been tentatively written
but while entry and random-stream snapshots remain rollback capable. The
published four-pass demo uses the same commit-boundary hook in its
domain-specific owner; it does not rely only on post-commit inspection.

If independent capture inside that commit hook returns false:

- the ordinary solve reports `gssContradiction` with
  `gckFinalValidation`;
- the failed public pass and exact entry are reported when the issue is
  cell-local;
- every entry's value, empty flag, and generated/lock ownership are restored;
- every pass random stream is restored;
- no pending public capture is published; and
- a negotiated caller may treat the failed candidate as an ordinary atomic
  round and, within its explicit budget and scope, reject an earlier complete
  assignment.

If the validator raises, the entry and random snapshots are restored and the
exception propagates. If it mutates a live entry and returns success, the
generic commit mutation guard rejects the transaction. A successful owner
publishes only a detached public capture produced by the validated terminal
candidate.

Final validation is deliberately retained even though the hard clauses should
make the projection exact. It detects a mismatched model, direct public graph
tampering, a bridge/compiler defect, an entry-hook mutation, or a later change
that would otherwise turn a representation assumption into unchecked output.

## baselines

The primary semantic baseline is the existing standalone overlapping-pattern
path:

1. apply the same immutable `TWfcOverlappingModel2D` to an isolated graph;
2. solve it with the same dimensions, wrapping, seed, and solve options;
3. call `CaptureSolvedPatternGrid2D`;
4. call `TryProjectOverlappingPatternGrid2D`; and
5. call `ValidateOverlappingProjection2D`.

The pass-composed public grid must equal this independently projected token
grid cell for cell. The baseline is intentionally post-solve and has no
downstream semantic pass; it establishes representation identity rather than
feature parity.

Ordinary one-way `TrySolve` is the transaction baseline. The first experiment
does not need pass negotiation to construct a result. Selective regeneration
is used only to prove that an incompatible downstream edit rolls back without
altering its already committed providers.

Two tempting shortcuts are negative baselines, not acceptable
implementations:

- a zero-offset map from one private pattern key to one public token loses the
  footprint coordinate; and
- projection after commit cannot participate in the same transaction as a
  downstream consumer.

## published fixture

`examples/2D/05_LearnedPatternWorld` learns at runtime from two ordered wrapped
token samples with shapes `5x5` and `7x5`. Their public vocabulary is
`~`, `.`, and `#`. The learner uses a wrapped `2x2` footprint with D4
augmentation and produces exactly `17` unique latent patterns on all three
target runtimes.

The generated graph is `8x6x1`, wrapped, and uses seed `0`. All four passes are
defined overlays with stable creation and execution order:

| Index | Label | Representation | Direct semantic role |
| ---: | --- | --- | --- |
| 0 | `patterns` | private pattern keys | learned latent anchors |
| 1 | `terrain` | public palette tokens | exact materialized projection |
| 2 | `foliage` | public semantic tokens | consumes terrain only |
| 3 | `structure` | public semantic tokens | consumes terrain only |

The bridge creates `3` public terrain values, `12` exact-offset clauses, and
`68` private-key support references: `3 * 2 * 2` clauses and
`17 * 2 * 2` references.

The fixture performs these checked phases:

1. Learn, apply, solve, and commit the four-pass baseline.
2. Independently capture and validate all latent overlaps and all projected
   contributions.
3. Compare `terrain` exactly with the standalone projection oracle.
4. Validate every foliage and structure rule using public terrain tokens,
   without reading private keys.
5. Scan every public layer and prove that no `@p...` key escaped.
6. Add one deliberately incompatible caller-owned `structure` lock and run
   selective regeneration from `structure`.
7. Require that attempt to fail atomically while `patterns`, `terrain`, and
   `foliage` are reported reused. Compare the selected pass plus the `Value`,
   `Empty`, and `Generated` fields of all `4 * 8 * 6 = 192` pass entries with
   both the committed baseline snapshot and an untouched independently solved
   same-seed control. Compare the next `RandomIndex(1000003)` draw for every
   pass stream with the same control.
8. Clear the incompatible lock, regenerate `structure`, and recover the
   independently valid committed output, the exact baseline entry snapshot,
   and the same three public-layer hashes. Make the control perform the same
   structure-only regeneration, then require exact entry and all-pass random
   next-draw parity again.
9. Replay the complete seed-zero fixture under current native FPC, stable FPC
   3.2.2, and historical pas2js runs.

The deliberate lock is rejected as `gckPassDependency` before final
validation. It proves selective solve rollback and provider reuse; it is not
an injected `gckFinalValidation` failure. The commit hook is nevertheless on
the actual publication path. A targeted owner test that deliberately rejects
an otherwise complete candidate is required to prove the final-validation
failure branch and its entry/RNG rollback independently.

The published seed set is exactly `{0}`. No additional seeds are searched for
a more attractive output.

## measured observables and portable identities

The fixture records:

- source count and shapes, palette order, footprint, symmetry, and learned
  pattern count;
- canonical `wfcp=1` byte identity when the learned model is encoded;
- public value, clause, and support-reference counts derived from the complete
  bridge definition;
- pass creation indices and actual topological execution order;
- per-pass disposition and active selective horizon;
- independent checked-pattern, checked-relation, contribution, and projected
  cell counts;
- standalone-versus-pass projection equality;
- latent assignment, terrain, foliage, structure, and complete-pipeline
  signatures;
- failed selective-call status, pass and entry, rollback state, and reused
  provider dispositions;
- all-entry and per-pass next-random-draw parity with an independent same-seed
  control after the failed call; and
- exact recovery parity with that control, the committed entry snapshot,
  public hashes, and the next probe of every pass random stream.

The fixed structural counts and checks are:

| Evidence | Fixed value |
| --- | --- |
| Source shapes | `5x5`, `7x5` |
| Public palette | `~`, `.`, `#` |
| Footprint and output topology | wrapped `2x2`, output `8x6x1` |
| Unique latent patterns | `17` |
| Materialized public values | `3`, all unit weight |
| Exact-offset clauses | `12` |
| Private-key support references | `68` |
| Pass order | `patterns, terrain, foliage, structure` |
| Published seed set | `{0}` |
| Independent counts | `48` patterns, `96` overlaps, `192` contributions |
| Public semantic checks | `144` public cells, `96` consumer relations, `0` private-key leaks |
| Deliberate failure | `gssContradiction / gckPassDependency`, pass `3`, provider `1`, entry `28` |
| Rollback replay | all `192` entry states and the next RNG probe for all `4` passes exact against control |

The fixture identities use unsigned 32-bit FNV-1a with offset basis
`2166136261`, multiplier `16777619`, and arithmetic modulo `2^32`. Cardinals
are hashed little endian. Canonical ASCII strings are hashed as a little-endian
length followed by their bytes. Domain labels separate the model
(`LearnedPatternWorld/model-v1`), latent assignment
(`LearnedPatternWorld/latent-v1`), public layers
(`LearnedPatternWorld/layer-v1` plus the pass label), and complete result
(`LearnedPatternWorld/pipeline-v1`). The model identity hashes canonical
`wfcp=1`; the latent identity hashes shape, boundary, and row-major pattern
indices; each public-layer identity hashes shape and row-major graph values;
and the complete identity hashes the adapter version followed by the model,
latent, terrain, foliage, and structure identities. These demo identities are
distinct from the separately versioned generic
`TWfcPattern2DComposition.Signature`.

Current native FPC 3.3.1, stable native FPC 3.2.2, and historical pas2js runs
agree exactly on these seed-zero identities:

| Evidence | Value |
| --- | --- |
| Canonical learned `wfcp=1` size/signature | `2130 bytes / 9BF802CC` |
| Latent pattern assignment signature | `D800EC4B` |
| Materialized terrain signature | `EBBC9390` |
| Foliage signature | `92D4BC87` |
| Structure signature | `8FA9D854` |
| Complete pipeline signature | `38FE98C4` |
| Recovered structure/pipeline signature | `8FA9D854 / 38FE98C4` |

| Runtime | Result |
| --- | --- |
| FPC 3.3.1 native | all identities and structural counts exact |
| FPC 3.2.2 native | all identities and structural counts exact |
| historical pas2js 3.3.1 runs | all identities and structural counts exact |

These uppercase values are fixed acceptance criteria, not attractive-output
targets selected after searching additional seeds.

## stopping rule

The experiment has no adaptive seed, corpus, footprint, or parameter search.
It stops as follows:

- configuration stops before mutation when applied latent semantics, topology,
  palette coverage, reserved-key separation, dependency, cycle, or public-token
  preflight fails;
- a solve stops with the ordinary `gssSolved`, `gssContradiction`, or
  `gssBacktrackLimit` contract under its declared finite local budget;
- the deliberately incompatible structure edit stops at its first ordinary
  selective failure and is not reported as a proof of global
  unsatisfiability;
- recovery stops at the first independently validated successful structure
  regeneration; and
- the milestone stops only when the standalone projector and materialized
  pass agree exactly, final-validation rollback is proven, no private key is
  public, and native/stable/pas2js evidence matches.

A budget-limited run is recorded as budget-limited evidence. It is not
silently rerun with a larger limit until it passes.

## findings and negative results

The measured fixture establishes only these positive findings:

1. A finite wrapped footprint projection can be expressed using the existing
   conjunction of exact-offset pass clauses.
2. A real public-token pass lets ordinary downstream semantic layers join the
   same atomic transaction without understanding latent pattern keys.
3. Unit-weight public values preserve the latent model as the sole learned
   frequency source.
4. Independent capture at the tentative commit boundary can make a
   representation bridge fail closed rather than publish unchecked output.
5. The same explicit clause model and validation path can replay under native
   FPC and pas2js.

The design also records the following negative findings rather than hiding
them:

- **A pattern key is not a token.** A simple same-coordinate lookup is
  insufficient because one latent value describes several footprint cells.
- **Post-hoc projection is too late for pass composition.** It remains a good
  oracle but cannot feed a consumer staged in the same solve.
- **One contribution is not the full contract.** Structural compatibility
  should make all writes agree, but compiling and validating every footprint
  coordinate detects corrupted or mismatched inputs instead of trusting that
  assumption.
- **Open projection does not fit the shared-shape pass model.** It expands the
  output and needs a different topology or an explicitly versioned crop/pad
  policy.
- **The explicit bridge can be large.** Clause count grows with palette size
  times footprint area, and support storage grows with pattern count times
  footprint area. Version 1 has no compressed predicate or index-backed
  clause form.
- **A second learned weight vector would double-count evidence.** Public
  projection values therefore use unit weights.
- **Hard equality is not an objective.** The first valid projection is not
  smoother, closer, prettier, or otherwise optimal.
- **Independent validation remains necessary.** Hard-clause compilation alone
  is not accepted as proof that future mutation, integration, or compiler
  changes preserved the public representation.
- **One RNG draw is a regression probe, not serialized-state identity.** The
  same-seed control makes rollback divergence observable at the next draw for
  every pass, but the fixture does not publish or compare private generator
  state.

## limitations and non-claims

Pattern-Projected Pass Composition v1 is deliberately limited to:

- wrapped output;
- depth exactly one and `DeltaZ = 0`;
- one shared anchor/public resolution and shape;
- cardinal 2D rectangular overlapping-pattern models;
- finite palette tokens and finite private pattern sets;
- public tokens outside the reserved private `@p` plus decimal-digit syntax;
- exact hard equality clauses; and
- an acyclic provider-to-consumer pass DAG.

It is not a general resampling or projection language. In particular, it does
not define open-output expansion, cropping, padding, scaling, interpolation,
rotation at application time, coordinate transforms between different graph
shapes, reductions, counts, distances, arbitrary predicates, soft penalties,
or weighted objectives. It does not learn downstream foliage or structure
relations, ingest images, project 3D patterns, stream chunks, repair cyclic
dependencies, or claim parallel execution.

The fixture does not compare against an equivalent flattened state space and
makes no runtime, memory, propagation, or backtracking superiority claim. It
does not test conflict-directed negotiation, minimal edits, or general
pattern-aware selective repair. Those require separate algorithms, fixtures,
metrics, and version identities.

## reproducibility and next experiments

Reproduction requires the ordered source samples and shapes, palette,
footprint, source boundary, symmetry, graph dimensions and wrapping, pass
topology, seed, solve options, and overlapping-model, adapter, projection,
pipeline, solver, random, and graph-model versions. The implementation
uses project Pascal units plus the applicable FPC or pas2js RTL; no external
solver, projection package, image library, serializer, or hosted service
participates in the canonical result.

Only after the version-1 fixture is fixed should later experiments consider:

- an open-output owner with an explicitly different public shape;
- a project-owned indexed or compressed hard-clause representation;
- aligned training for the downstream semantic passes;
- 3D footprints and vertical contributions;
- chunk-boundary reconciliation for large wrapped worlds;
- negotiated reopening of a latent pattern assignment after public or
  semantic final validation; and
- a controlled comparison with a faithful flattened model.

Each successor needs its own hypothesis, fixture and seed set, counters,
portable goldens, stopping rule, and negative findings.
