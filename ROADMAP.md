# WFC roadmap

WFC began as a small Free Pascal experiment: describe values, describe what may
surround them, and let the graph produce something that satisfies those rules.
The larger idea is more interesting than a tile generator. A world, song,
building, or piece of text can be built in deliberate passes, with every pass
respecting what came before it.

The goal is to make WFC the strongest constraint-collapse ecosystem available
for Free Pascal: useful from an ordinary FPC program, portable to the browser
with pas2js, easy to extend in the style of the existing API, and honest about
what has been proven. When an experiment goes beyond the existing literature,
we will publish the method, fixtures, measurements, and failures as well as the
result.

This roadmap is ordered. Later milestones should not be considered complete
while their foundations are still demonstrations or stubs.

## Current reality

The repository already contains the beginnings of the ecosystem:

- a fluent `TGraph` API with 2D and 3D neighbors, custom string values,
  required rules, selection callbacks, and extension hooks;
- a versioned dependency-DAG pass coordinator with stable labeled passes,
  isolated rules and values, deterministic topological execution, legacy,
  overlay, and transform modes, named same-coordinate constraints, signed
  exact-offset, finite any-of-neighborhood clauses, and atomic
  descendant-only regeneration;
- [finite count-range pass requirements](docs/pass-counts.md) with required
  matching-offset or distinct-cell semantics, duplicate-safe canonicalization,
  zero-count absence, portable recipe encoding, independent validation, and a
  shared native/browser neighborhood inspection and bounded-repair demo;
- a versioned portable random source with an explicit pipeline seed,
  independent index-derived pass streams, run-to-run rewind, and matching
  native/pas2js golden fixtures;
- an opt-in reference solver with maintained domains, queue-based fixed-point
  propagation, deterministic fixed-point weighted Shannon entropy, an exact
  unit-weight minimum-domain path, bounded chronological backtracking,
  independent final validation, and structured per-pass reports;
- opt-in [whole-transaction restart policies](docs/restarts.md) for ordinary
  and negotiated solving, with deterministic effective seeds, fixed or capped
  doubling of local budgets, unchanged caller seed identity, separate attempt
  transcripts, and optional diagnostic elapsed timing;
- atomic reference-solver staging across full and selectively regenerated pass
  closures, including locks, named dependencies, definitionless-pass behavior,
  skipped-layer preservation, and rollback on any failed descendant;
- an opt-in Pass Negotiation v1 coordinator that chronologically excludes exact
  completed pass assignments across atomic full-pipeline rounds, separates
  local and pass budgets, preserves ordinary Trace-v1 reports per attempt, and
  publishes a versioned native/pas2js transcript;
- an isolated Selective Negotiation v1 wrapper that applies the same bounded
  whole-assignment search only to explicit roots and their dependency
  descendants, canonically reports that repair horizon, and preserves every
  reused provider value and random stream;
- an opt-in Causal Trace v1 contract covering initial filters, decisions,
  propagation, contradictions, backtracking/restoration, pass lifecycle, and
  atomic commit/rollback; compatible legacy pass slices plus additive
  versioned derived ranges for late earlier-pass commit failures,
  provider-pass cause links, stable portable hashes, public query/validation
  helpers, and matching native/pas2js conformance fixtures;
- synchronous Trace Delivery v1, independent of full capture, with streaming
  through the kernel and public graph without retaining event history;
  isolated observer failures, per-attempt hashes/counts, and a caller-sized
  recent-event window that preserves original IDs and reports dropped evidence
  explicitly (see [streaming traces](docs/trace-streaming.md));
- a dependency-free shared native/pas2js console inspector that validates and
  prints a terrain -> settlement -> foliage trace, then follows a rejected
  downstream candidate backward to its provider-pass event, demonstrates a real
  late earlier-pass rollback, and compares full capture with live five-event
  suffix retention on the same validated fixture;
- a dependency-free native/pas2js Pipeline v2 fixture that contrasts bounded
  and wrapped spatial reads, independently validates terrain -> settlement and
  foliage clauses, and rejects an out-of-bounds requirement;
- a reusable `wfc_world2d` terrain/biome/foliage library, a separate semantic
  validator, fixed-token portable layer signatures, and a documented
  native/pas2js multi-pass demonstration;
- a reusable six-layer selective-settlement domain and validator demonstrating
  terrain branching into hydrology and biome, then joining through roads,
  housing, and foliage with deterministic edit/regenerate/recover fixtures;
- an interactive pas2js browser world with synchronized layer canvases,
  caller-owned locks, responsive controls, and a seeded headless-browser
  conformance fixture;
- an immutable one-layer model IR, deterministic 1D/2D corpus learner with
  heterogeneous ordered samples, open/wrapped boundaries, explicit D4
  augmentation, checked model merging, strict canonical `.wfcm` text I/O, and
  native/pas2js learning demonstrations;
- additive [full-volume cardinal learning](docs/volume-learning.md) with
  explicit source depth, six-direction observations, gravity-preserving D4
  and cube policies, exact observation-orbit aggregation, literal-transform
  oracles, compatible merging, depth-aware source/model artifacts and
  provenance, XYZ workspace constraints, and native/browser Z-slice inspection;
- an immutable overlapping-pattern 2D model with heterogeneous extraction,
  exact structural compatibility, latent assignment capture, independently
  checked projection, strict canonical `.wfcp` text I/O, and a portable seeded
  demonstration;
- Pattern-Projected Pass Composition v1 with a wrapped, depth-one adapter that
  compiles every overlapping footprint contribution into exact cross-pass
  clauses, a reusable validated `patterns -> projection` owner, and a
  four-pass learned terrain/foliage/structure fixture on native FPC and pas2js;
- an immutable bounded sequence model with typed BOS history, deterministic
  order-N learning from pretokenized UTF-8 corpora, raw counts, structural
  suffix/prefix recombination, whole/prefix/suffix/fragment and derived wrapped
  graph adaptation, atomic bulk token constraints, exact feasible-domain
  analysis, latent/public pass projection, and strict canonical `wfcs=1` text
  I/O;
- a project-owned text constraint-completion foundation with lossless
  Unicode-scalar tokenization across native UTF-8 and pas2js UTF-16,
  caller-defined tokenizer learning, anchored infill, prefix continuation,
  structured semantic failure, independent text validation, and a shared
  native/pas2js demonstration;
- a reusable three-pass text owner with separate structure, lexical, and
  punctuation sequence models; atomic N-source public projection maps;
  exact versioned surface fragments; independent cross-layer validation;
  capture and semantic validation inside the entry/random-state transaction;
  private-key-safe causal traces; matching native/pas2js fixtures; and an
  interactive dependency-free browser workbench;
- a project-owned Music Foundation v1 with an immutable exact score IR,
  reduced rational helpers, strict `wm1`/`wr1`/`wh1` fixed-quantum cells,
  lossless aligned monophonic projection/rebuild, and exact rhythm and
  pitch-class maps across private latent sequence passes;
- a separately versioned [polyphonic ensemble layer](docs/music-ensemble.md)
  with chord-capable synchronized voices, exact score round-trips, rhythm
  vectors, distinct exact/allowed pitch-class sets, safe continuation-model
  preflight, common-excerpt training, a transactional three-pass owner, and
  shared native/browser Ensemble Studio;
- [Independent Voices v1](docs/music-voices.md) with detached common-excerpt
  role training, one chord-capable vocabulary per voice, shared harmony/rhythm
  providers, pitch ranges and optional pair gaps, existential exact-coverage
  witnesses, independent semantic capture, bounded continuation, and the
  native/browser Voice Studio proof corpus;
- strict canonical `wfcmusic=1` score text, a defensive project-owned SMF
  format-0/1 byte codec, deterministic format-0 score export, four focused
  conformance suites, and a native/pas2js three-pass composition example;
- Music Negotiated Variation v1 with a reusable persistent three-pass owner,
  public motif locks, atomic ordinary and bounded negotiated regeneration,
  explicit selective repair horizons, immutable independently validated
  results, portable signatures, strict `wfcmusicpass=1` replay, and matching
  native/pas2js fixtures;
- a project-owned Voxel Foundation v1 with immutable prototypes, deterministic
  yaw variants, explicit socket pairs, support-aware six-face graph
  compilation, immutable scenes with portable signatures, independent
  socket/support/entrance and reachability validation, and renderer-neutral
  integer surface meshes;
- a project-owned Building 3D v1 with a full-depth typed massing blueprint,
  checked complete prototype projections and target-yaw spatial clauses over
  private voxel keys, a transactional footprint -> structure -> envelope/roof
  -> props owner, independent cross-layer validation, and one shared native
  FPC/pas2js depth-three console and mesh fixture;
- a project-owned Building presentation layer with immutable four-pass public
  lineage, exact structure/prop mesh composition, signed fixed-subcell
  commands, four checked integer isometric yaws, explicit stable painter
  ordering, reverse hit testing, portable view signatures, and canonical SVG;
- a deterministic native Building SVG host and an interactive pas2js Canvas2D
  workbench over the same commands, with seeds, selective regeneration,
  presentation modes, Z clipping, picking, lineage inspection, and an exact
  seed-zero browser fixture;
- a complete Portable Pipeline Execution v1 path that compiles immutable
  recipes into fresh transactional graphs, applies typed adapters, projection
  bridges, public requirements, and independent commit validators, then accepts
  canonical recipe-bound locks and domains through transform aliases; current
  Pattern2D and Sequence bridge v2 recipes deterministically lower those public
  constraints back into intersected private source domains, while bridge v1
  remains forward-only for exact replay compatibility;
- strict canonical `wfcpipeline-run=1` and `wfcpipeline-result=1` artifacts,
  deterministic one-way and bounded negotiated replay, public-only successful
  layers, structured non-solved results, fixed allocation/encoding limits, and
  matching native FPC/pas2js fixtures, including a canonical bridge-v2
  LearnedPatternWorld recipe/run/result bundle with eight public terrain locks
  and the established three public layer hashes;
- a project-owned collision-safe token lookup plus shared Pascal application
  logic for strict recipe validation and headless recipe-plus-run execution,
  with thin bounded-I/O native hosts;
- Portable Training v1: immutable pretokenized corpora, strict editable
  `wfclearn=1` text, explicit capacity preflights, and a shared native
  `wfc-learn` host producing cardinal, overlapping-pattern, or sequence
  models and executable recipes with source/license and per-sample identity;
  four checked source/model/recipe/run/result bundles and real-process
  file/stdin conformance cover the complete bounded workflow;
- a shared editable training workspace with strict source/run/result
  invalidation, detached outputs, explicit interactive resource policy, and
  a project-owned Unicode-scalar raw-text import bridge; Training Studio
  connects those APIs to a pas2js corpus/lock/export workbench and five
  independently validated native presets;
- stack-safe iterative traversal of large planes while preserving the legacy
  solver's north/east/south/west depth-first order;
- checked one-command native build gates, an FPM package, a runtime-only
  Lazarus package, and a hosted stable/development CI workflow;
- single-pass and multi-pass console world examples;
- the original A-major and manually inferred riff grammars, now portable Pascal
  studies with native console hosts, owned MIDI and streaming WAVE output,
  user-selected note counts, and cross-target conformance tests;
- the original building-kit constraints and console renderer, alongside the
  maintained native SVG and browser Building 3D views;
- native/pas2js conformance runners covering the core, pass contracts, 2D and
  voxel-3D domain semantics, learning, sequence, music, atomic failure, and
  replay signatures, including the reference-kernel, public pipeline, and
  trace-utility and pass-negotiation contracts.

It is not yet the finished system described above:

- the version-2 reference solver now supports scale-canonical integer weights,
  explicit zero-support adjacency, and caller-owned per-cell domains. Separate
  whole-transaction restart policies add local-budget recovery and elapsed
  timing, but more scalable domain representations, local-pass or selectively
  scoped restart policies, richer failed-clause/minimal-core explanations,
  interactive stepping, and compressed persisted trace artifacts remain;
- pass DAGs, named overlay layers, same-coordinate and exact signed-offset
  requirements, finite any-of-neighborhood reads, selective regeneration, and
  structured dependency diagnostics are now operational; sequence maps now
  provide atomic N-source bridges between unlike public vocabularies, and the
  first bounded acyclic full and selectively scoped backward-negotiation
  baselines now exist; inferred neighborhoods and distance expressions, automatic
  or minimal repair-horizon selection, conflict-directed or cyclic repair, and
  general cross-representation projection schemas beyond the exact wrapped
  pattern and sequence adapters remain to be designed;
- the examples index now records targets, dependencies, build commands, and
  honest completion status, but full per-example tutorials, invariants,
  expected output, and troubleshooting guides remain to be written;
- the reusable voxel substrate and footprint -> structure -> envelope/roof ->
  props wrapper are now independent of the legacy depth-one building kit and
  have matching native/pas2js textual and mesh output, deterministic native
  SVG, and an interactive browser workbench; there is still no standard-RTL
  interactive native window; the unconnected external-engine shell is removed, and
  arbitrary cameras or intersecting geometry need a later generalized
  visibility/depth renderer;
- the interactive 2D, three-pass text, and Building 3D pas2js demos and browser
  test hosts now exist, and the first console pass/causal-trace inspector plus
  stable native/pas2js trace parity are checked; live global domain snapshots,
  interactive trace stepping, search controls, and richer domain-specific
  browser inspection remain;
- model learning now covers pretokenized cardinal radius-one corpora,
  structurally compatible overlapping 2D footprints, and bounded order-N
  sequences with explicit projection; Unicode-scalar text tokenization and
  exact completion domains now sit above that sequence layer, while a
  project-owned versioned word-boundary table, wrapped sequence training,
  overlapping 3D footprints, structured multi-license provenance, media import and
  word-oriented text import, larger/background browser training, artifact-family
  lint, and general inspection tools remain;
- the hosted CI definition is present, but its first remote run still needs to
  be observed before the Phase 0 exit gate is claimed complete;
- the portable music path now has project-owned PCM/WAVE rendering, native
  file export, MIDI-to-score import and selected-excerpt training, and an
  interactive browser Music Studio with HTML audio and user-defined-duration
  streamed arrangements. Both original music studies now use that owned media
  stack too; the external playback submodule and GUI dependencies are removed;
- Music Foundation v1 and Negotiated Variation v1 prove quantized single-voice
  pitch-class composition, public motif preservation, bounded pass-level
  repair, result replay, and score-to-SMF export. Ensemble and independent-role
  layers now add polyphonic cell projection and collective exact/subset harmony.
  Richer key and chord semantics, expressive-performance learning, phrase-aware
  coordinate repair, voicing, ornamentation, and the complete music exit gate
  remain open.

These are starting points, not embarrassments. The first job of the roadmap is
to preserve what is interesting while making the foundation trustworthy.

## Design commitments

- **Free Pascal first.** Supported releases of FPC are the reference native
  targets. Lazarus projects are welcome, but the core must not require Lazarus.
- **Pascal remains the source of truth.** Browser behavior is compiled with
  pas2js. JavaScript should be a thin host, not a second solver implementation.
- **Preserve the character of the API.** Fluent calls, `TGraph`, user-defined
  values, callbacks, protected `Do*` hooks, and straightforward Pascal remain
  part of the public experience. New internals may be split into focused units
  without forcing existing users to rewrite working code.
- **Determinism is a feature.** Mature solver runs will accept an explicit
  seed. A seed, model version, and input must be enough to replay a result and
  its trace.
- **Contradictions are data.** A solver must report why it failed; examples
  must not silently replace an impossible state and call the result valid.
- **Passes add information.** Prior committed output remains available for
  constraints, inspection, and selective regeneration.
- **Claims require fixtures.** A feature is complete when tests and examples
  demonstrate its invariants, not when its public methods merely exist.
- **MIT by default.** Project-authored source and assets remain MIT. External
  code and data must be optional, documented, and license-compatible with the
  way they are distributed.
- **Own the portable foundation.** Whenever a capability can reasonably be
  implemented in portable Pascal instead of adding a library, implement and
  maintain the FPC/pas2js version here. If library inclusion is a judgment
  call, choose the project-owned implementation. Keep runtime dependencies
  minimal: repository units and the applicable standard RTL only. Third-party
  engines, viewers, and media backends may be isolated optional adapters.
  Optional development tooling must not leak into core/runtime APIs or define
  canonical models, algorithms, artifact formats, validation, or replay
  behavior.

## Phase 0: a trustworthy baseline

Stabilize the merged issue #1 and issue #3 history before expanding the public
surface.

### Deliverables

- Turn the console checks into an automated test executable that returns a
  nonzero exit code on failure and never waits for input.
- Add tests for every existing direction, wrapping mode, inverse rule,
  required rule, pre-seeded entry, invalid state, reshape, reset, and run mode.
- Introduce an explicit seeded random source and remove repeated global
  `Randomize`/`RandSeed` mutation during a run.
- Add a one-command native build and test entry point.
- Add CI for a supported stable FPC release and the chosen development
  compiler on the primary desktop platforms.
- Add an FPM package and a Lazarus package while keeping direct unit-path use
  possible.
- Put compiler output under ignored build directories so a test run leaves a
  clean worktree.

### Exit gate

A fresh clone builds and tests without IDE interaction; an invalid test makes
CI fail; no core tests are disabled; the same seed and fixture produce the same
validated result across two consecutive native runs.

## Phase 1: the reference solver

Keep the existing traversal behavior available for compatibility, but build a
solver whose operation can be described, measured, and compared with other WFC
implementations.

The version-2 solver now covers domains, fixed-point propagation, positive
integer weights, deterministic Q16 Shannon observation with an exact
unit-weight minimum-remaining-values path, locks, bounded backtracking,
structured contradictions, independent validation, atomic pass staging,
explicit deny-all directions, and pass-local caller domain masks.
The opt-in restart coordinator retries only local backtrack-limit exhaustion,
using fixed or capped-doubling budgets and separately versioned seed derivation.
Its attempt-zero contract preserves ordinary and negotiated replay; diagnostic
elapsed timing never influences search or portable hashes. This supplies the
restart/timing path without changing the existing solve-options record.
Causal Trace v1 supplies the native/pas2js stable trace-hash parity contract.
Phase completion still requires the full exit-gate evidence below; local-pass
restarts, selective restart horizons, and wall-clock cancellation are not
implied by this whole-transaction feature.

### Deliverables

- Separate topology, rule model, cell domains, random selection, solving, and
  diagnostics behind small Pascal units and interfaces.
- Maintain a domain of possible values for every uncollapsed cell.
- Select the next cell by minimum entropy or minimum remaining values, with a
  documented tie-break rule.
- Propagate changes through a queue until a fixed point is reached.
- Support weights, locks/anchors, boundary policies, bounded backtracking, and
  deterministic restart policies.
- Return a structured run report containing status, seed, decisions,
  propagations, contradictions, backtracks, restarts, and timing.
- Add an independent validator that checks every emitted adjacency rather than
  trusting the solver that produced it.
- Retain `wfc.pas` as a compatibility facade over the new implementation.

### Exit gate

Canonical satisfiable fixtures always pass the independent validator;
canonical unsatisfiable fixtures return a contradiction instead of an empty or
invented value; weighted and seeded golden fixtures replay exactly; native FPC
and pas2js produce the same final value grid and stable trace hash.

## Phase 2: the pass pipeline

Complete the original issue #1 contract, then extend it so passes can compose
different kinds of information instead of merely replacing one string grid
with another.

### Pass model

Each pass has a stable label, index, rule set, seed, input dependencies, output
layer, and run report. All passes share topology and coordinates.

Two explicit pass modes extend the legacy compatibility mode:

- **transform pass:** begins with a copy-on-write view of the previous output
  and refines values in the same domain;
- **overlay pass:** writes a new named layer while reading committed layers
  such as terrain, roads, structures, or harmony.

Running a pass follows `prepare -> solve -> validate -> commit`. A failed pass
does not corrupt earlier committed layers. Sequential dependencies remain the
compatibility default. Version-2 acyclic dependency graphs, named
same-coordinate and signed-offset constraints, finite any-of-neighborhood
clauses, deterministic topological execution, and selective descendant
regeneration are now implemented. Pass Negotiation v1 supplies a separate
termination and replay contract for bounded chronological reopening over the
complete acyclic pipeline; it does not change ordinary one-way or selective
semantics. Selective Negotiation v1 preserves that isolation while restricting
choice frames to caller-requested roots and their transitive dependency
descendants. Providers outside that canonical horizon remain immutable inputs;
the algorithm never widens a horizon automatically.

Count Constraints v1 extends finite spatial reads with inclusive lower/upper
ranges and explicit wrapped-alias semantics. The
[Neighborhood Counts workbench](examples/passes/04_NeighborhoodCounts/README.md)
composes terrain, roads, and a market probe, contrasts matching offsets with
distinct cells, and demonstrates selective road repair without changing
terrain. Counts are not a global cardinality or connectivity propagator.
The separate [Rooted Connectivity v1](docs/connectivity.md) primitive now adds
reciprocal-port possible-graph pruning, mandatory cut participation, exact
search-time validation, and reversible domains. Its two spatial demo cases
exercise roads and multi-floor circulation; existing settlement/voxel model
versions retain their earlier semantics. Portable recipe encoding, stronger
port-specific filtering, chunk-boundary summaries, and global cardinality
remain future work. The [research record](docs/research/rooted-connectivity-v1.md)
states the soundness argument and finite oracle scope without a generalized
arc-consistency or relative-performance claim.

The first packaged domain fixture proves the sequential subset with terrain →
biome → foliage. The selective-settlement fixture expands that proof to
terrain → hydrology/biome → roads → housing → foliage, including named
overlays, non-linear dependency closure, an intentionally illegal descendant
lock, transactional rollback, independent validation, exact recovery, and
matching native/pas2js layer signatures. Pass inspection and richer causal
explanations now have a first checked vertical slice: Causal Trace v1 records
chronological eliminations and provider links, and the console inspector walks
one chain across passes. Delivery v1 now emits those same events synchronously
without retaining search history; Window v1 keeps an explicitly incomplete,
caller-sized suffix. Interactive stepping, live domain views, complete
failed-clause evidence, minimal contradiction sets, and persisted trace
artifacts remain open.
A focused spatial fixture additionally proves bounded out-of-bounds rejection
and wrapped edge sampling for terrain consumers without adding domain knowledge
or a runtime dependency to the core.
Pattern-Projected Pass Composition v1 now proves that a private overlapping-
pattern assignment can feed a real public-token pass through one exact offset
clause per footprint coordinate. Its reusable two-pass owner and four-pass
world fixture validate the representation boundary before commit, keep public
weights neutral, and roll back both entries and random streams on rejection.
The focused pass-negotiation fixture adds one-cell, two-cell, provider-
exhaustion, and independent-provider join cases. It records exact rejected
assignments, distinguishes local and outer limits, proves atomic rollback and
ordinary-solver isolation, and replays attempt transcripts across native FPC
and pas2js.
The negotiated-repair fixture then compares two explicit horizons over
terrain/climate -> roads -> housing -> decor. A housing-root horizon fails
without reopening roads; a roads-root horizon excludes one whole roads
assignment, repairs housing and decor, and leaves terrain, climate, their
values, caller ownership, and their random streams unchanged. The selective
wrapper's scope and transcript are versioned independently from full-pipeline
negotiation.

### Deliverables

- Make `CurrentPass`, entries, planes, values, rules, callbacks, wrapping, run
  mode, reshape, and reset consistently pass-aware.
- Run every pass in stable order and restore the caller's selected pass after
  callbacks or a complete pipeline run.
- Copy prior output for an empty transform pass, as required by issue #1.
- Add cross-layer predicates and constraints without forcing domain-specific
  knowledge into the core.
- Derive pass seeds from the pipeline seed and stable pass identity, so adding
  a later pass cannot change an earlier committed result.
- Preserve Causal Trace v1 evidence for caller filters, neighbors, passes,
  decisions, contradictions, and abandoned branches.
- Support selective regeneration with locked unaffected cells.
- Preserve ordinary one-way replay while offering separately versioned bounded
  full-pipeline negotiation with exact chronological evidence.
- Offer separately versioned bounded selective negotiation over a canonical
  explicit descendant horizon, preserving all providers outside that horizon.
- Extend the checked console inspector with interactive stepping, live domain
  snapshots, richer clause evidence, and bounded/streaming capture.

### Exit gate

An automated terrain -> hydrology/biome -> roads/housing -> foliage fixture
proves that houses never occupy water, foliage respects terrain and structures,
all earlier layers remain unchanged after later commits, a failed later pass
rolls back cleanly, and native FPC and pas2js produce identical layer hashes.

## Phase 3: models, priming, and learning

Rules should be easy to author by hand and possible to infer from examples.
The learner is a deterministic constraint-model builder, not a hidden machine
learning service.

The observation and corpus version-1 primitives now preserve first-seen token
order and raw frequency counts, observe cardinal radius-one relations in
ordered heterogeneous 1D/2D samples without inventing cross-sample seams,
support open/wrapped boundaries and explicit D4 augmentation, checked-merge
compatible models, store ordered source shapes in immutable model data, adapt
representable models to `TGraph`, and round-trip strict canonical `.wfcm`
documents using the `wfcm=1` and `wfcm=2` profiles on native FPC and pas2js.
The additive volume learner extends the same IR to explicit depth and six
directions with `wfcm=3`, while retaining legacy dense arrays and bytes.
Its exact group-orbit counts match an independently materialized transform
oracle for open/wrapped rectangles and singleton axes. Cube symmetries act
on coordinates, not token meanings, and do not encode scalar chirality.
The graph adapter now maps active zero-support rows to explicit denial instead
of rejecting or widening them. Portable single-sample and corpus fixtures
prove those vertical slices. The
overlapping-pattern version-1 layer additionally extracts deterministic
rectangular 2D payloads, supports open/wrapped heterogeneous sources and
square-footprint D4 augmentation, compiles exact structural overlap, keeps
latent assignments separate from projected tokens, independently validates
both, and round-trips strict standalone `wfcp=1` artifacts. Its first pass
adapter now materializes wrapped, same-size, depth-one projections for ordinary
downstream semantic layers without exposing private pattern keys.
The sequence version-1 layer learns bounded order-N latent states from ordered
pretokenized UTF-8 corpora, resets typed BOS history at every sample boundary,
retains raw observation/start/end counts, derives exact suffix/prefix
compatibility, adapts whole/prefix/suffix/fragment paths or BOS-free wrapped
cycles, composes latent and public-token passes in either direction, and
round-trips strict standalone `wfcs=1` artifacts. Atomic positional masks and
exact forward/backward domain analysis now support project-owned Unicode-scalar
text completion. Corpus seams are absent, while compatible states may
intentionally recombine. Portable Recipe Foundation v1 now adds strict
`wfcrules=1` artifacts for exact hand-authored rank-1/2/3 rules and an
immutable `wfcpipeline=1` recipe IR. The recipe owns canonical `wfcm`,
`wfcrules`, `wfcp`, and `wfcs` resources; pins replay-relevant versions;
validates provenance, topology, visibility, dependencies, typed projection
bridges, public vocabularies, and signed-offset requirements; and round-trips
through the same dependency-free codec on native FPC and pas2js. Portable
Pipeline Execution v1 now compiles that recipe into a fresh transactional
graph, installs every supported adapter, bridge, requirement, and independent
validator, resolves public inputs through transform aliases, and uses current
Pattern2D and Sequence bridge v2 declarations to lower locks and domains into
globally intersected private source domains. Bridge v1 remains forward-only
for replay compatibility; the bounded inverse lowering, sorting, lookup, and
intersection machinery is project-owned Pascal shared by native FPC and
pas2js. Execution emits strict canonical `wfcpipeline-run=1` and
`wfcpipeline-result=1` replay artifacts.
Shared Pascal application logic now drives a strict recipe validator and a
headless recipe-plus-run executor through a thin native host.
Portable Training v1 now adds editable, pretokenized source documents and a
shared `wfc-learn` application. It preserves sample boundaries, explicit
options, source/license labels and content fingerprints while exporting
cardinal, overlapping-pattern, or whole-sequence recipes. Four bundled corpora
replay exact source-to-result artifacts on native FPC.
Training Studio now connects editable corpora and explicit Unicode-scalar raw
text to that workflow through one reusable invalidation-aware workspace, six
native presets, and a browser lock/inspection/export workbench. Full-volume
training uses `wfclearn=2`, depth-sensitive provenance, XYZ locks/domains, and
labeled browser Z slices. A fifth exact CLI bundle replays the volume preset.
Its
interactive envelope is bounded and synchronous; raw media/voxel/music
extraction, larger background jobs, and general recipe authoring remain open.
Word-boundary tables, wrapped sequence training, overlapping 3D extraction,
broader artifact lint/inspection, and the complete exit gate remain open.

### Deliverables

- Define a versioned, human-readable model format for symbols, topology,
  neighborhoods, weights, symmetry, boundary behavior, passes, dependencies,
  provenance, and source licenses.
- Implement the same model reader and writer for native FPC and pas2js.
- Extend the current pretokenized `wfc-learn` host beyond cardinal grids,
  overlapping 2D patterns, and sequences with project-owned raw-text/media,
  voxel, and musical-event importers and richer domain extraction.
- Extend the current strict recipe `wfc-validate` host across the run/result
  family and add unreachable-value, asymmetric-rule, impossible-input, and
  missing-asset lint profiles.
- Extend the current deterministic recipe-plus-run `wfc-run` host with explicit
  reproducible batch orchestration.
- Generalize the existing fixed-fixture causal-trace inspector into
  `wfc-inspect` for arbitrary rule graphs, pass layers, entropy/domain views,
  interactive decision replay, and richer contradiction explanations.
- Preserve source hashes and corpus licenses in generated model metadata.
- Add compact binary caching only after the portable text format is stable.

### Exit gate

A known sample can be learned, serialized, loaded, and used to regenerate only
valid neighborhoods; model round-trips retain semantics; malformed input gives
actionable diagnostics; native and browser loaders agree on conformance
fixtures; every bundled learned model names its source and license.

## Phase 4: four domain ecosystems

Each ecosystem is a specialized library built on the same core, accompanied by
native FPC and pas2js demos. A screenshot without reusable units, fixtures,
tests, and an explanation is not a finished ecosystem.

### 2D worlds

Model version 1 now provides a bounded/wrapped depth-one wrapper, typed
terrain/biome/foliage layers, caller locks, an independent semantic validator,
portable CRC signatures, a focused conformance suite, and a shared-source
console demo. An interactive browser field instrument now runs the same model,
shows all three layers, supports cell locks, and exposes a deterministic
headless self-test. This is the first vertical slice, not the completed exit
gate.

The learned-pattern vertical slice now trains a wrapped `2x2` model at runtime,
materializes its private anchors as public terrain, and feeds foliage and
structure in the same atomic four-pass DAG. Native FPC replay
the same seed-zero signatures, deliberate contradiction, exact rollback, and
selective recovery without an external runtime library.

- Extend the current bounded/wrapped grid with masked and chunked topologies.
- Extend the current terrain/biome/foliage model with coast/hydrology, roads,
  settlements, and richer decoration helpers.
- Image/tile-set learning with rotation and reflection policies.
- Feed the implemented causal trace into the browser field instrument and add
  live domains, interactive stepping, search controls, and richer
  contradiction views across the complete multi-pass pipeline.

**Exit gate:** every displayed map passes terrain, connectivity, occupancy, and
cross-pass invariants; the same model and seed match between native and web.

### 3D structures

- Learned Terraces now connects six-direction volume learning to authored
  socket/support variants and spatial foliage in a three-pass transaction.
  Complete model-token-to-voxel maps and world/target-yaw clauses are reusable
  across ranks; independent final validation, selective preservation,
  negotiated upstream repair, and shared native/browser SVG are tested.
  [Contract](docs/learned-terraces3d.md),
  [experiment](docs/research/model-to-voxel-pass-v1.md).
  This is full-volume cardinal learning, not overlapping 3D footprints,
  chunked generation, physics inference, or serialized private consumers.

- Voxel Foundation v1 now provides immutable weighted prototypes, explicit
  symmetric socket pairs, deterministic quarter-turn yaw, vertical support,
  exact six-direction graph compilation, immutable captured scenes, portable
  signatures, independent local/global validation, and renderer-neutral
  integer quad extraction on native FPC and pas2js.
- Building 3D v1 now provides the depth-aware four-pass owner, typed massing
  blueprints, complete private-key-safe prototype projections, target-yaw
  spatial clauses, selective regeneration, independent cross-layer and voxel
  validation, and a shared native/pas2js depth-three fixture with public layer
  output and renderer-neutral mesh extraction.
- Building presentation v1 now provides immutable footprint, structure,
  envelope/roof, and complete views with public four-pass lineage; exact
  structure and prop mesh composition; fixed-integer four-yaw commands; stable
  painter ordering and hit testing; deterministic native SVG; and an
  interactive pas2js Canvas2D workbench. Seed zero is pinned to pipeline
  `1:F1EF0EB6`, complete view `AC7290C0`, and `140` faces.
- Voxel and modular-building topologies with complete vertical constraints.
- Rotation-aware sockets, support/load rules, empty space, entrances,
  connectivity, roofs, and multi-floor relationships.
- Extend the implemented footprint -> structure -> facade/roof -> props
  pipeline with multi-floor circulation, attachment offsets, and richer load
  states.
- Generalize the current four-yaw painter projector when arbitrary cameras,
  intersecting surfaces, or a depth buffer become justified by a concrete
  domain fixture.
- Optionally connect a native interactive window or engine adapter to the
  public command model. It must remain an edge integration; it may not define
  canonical geometry, ordering, signatures, validation, or replay.

The foundation deliberately excludes the legacy binary FBX from the standard
path because its source and license provenance are not established. The
delivered standard Building presentations use generated unit geometry and
project-authored materials.

**Exit gate:** the building kit uses depth greater than one, contains no
unsupported structural pieces or disconnected required entrances, presents
the same validated public structure through deterministic native SVG and the
browser workbench, and uses only licensed source assets. A native interactive
adapter is optional and cannot be required by the portable ecosystem.

### Music

- Music Foundation v1 now provides an immutable event model covering exact
  time, duration, pitch, voice, velocity, meter, tempo, rests, chords, and
  complete timeline boundaries. `wfcmusic=1` preserves it canonically.
- Version-1 fixed-quantum cells model melody attacks/holds/rests, rhythm
  actions, and harmony pitch classes. Exact projection/rebuild is available
  for aligned monophonic voices; chords remain available in scores and export.
- Ensemble v1 separately represents independently sustained chord-capable
  voices in joint frames. Harmony and rhythm providers constrain an ensemble
  consumer, with voice-slot locks, independent validation, full/selective
  negotiation, and common-excerpt training. Ensemble Studio exposes this
  pipeline in native FPC and pas2js without changing monophonic replay formats.
  Joint frames preserve observed vertical combinations. The separate
  [ensemble streaming protocol](docs/music-ensemble-stream.md) carries exact
  sequence-state frontiers across bounded local graphs, including partial BOS
  history and independently held chords. Incremental PCM rendering preserves
  phase and release envelopes with a fixed pending buffer. The wider global
  timeline stays separate from Integer-indexed local sections. The separate
  [MIDI stream](docs/music-midi-stream.md) counts and deterministically replays
  events into a forward-only format-0 file, preserving held voices and long
  silent gaps. Global long-range feasibility and infinite storage are not claimed.
- [Independent Voices v1](docs/music-voices.md) separates chord-capable role
  models while preserving common excerpts and shared rhythm/harmony providers.
  Per-role subset clauses plus existential coverage suppliers enforce exact
  collective harmony without a Cartesian joint vocabulary. Inclusive role
  ranges and explicit pair-gap/rest policies compose with that relation.
  A bounded stream carries exact H/R/role frontiers, including held chords and
  early BOS context, and feeds incremental audio/MIDI. Voice Studio exposes
  observed-versus-new verticals and supplier evidence. The
  [research record](docs/research/independent-voices-v1.md) separates the tiny
  graph truth table from the full two-excerpt temporal corpus and records
  search degeneracy and the absence of global feasibility guarantees.
- A portable three-pass fixture solves harmony and rhythm before melody,
  composes both constraints through latent sequence projection maps,
  independently validates the public relation, rebuilds a score, and checks
  canonical text and MIDI bytes on native FPC.
- A reusable persistent music-pass owner now supports public-token constraints
  and motif locks, immutable composition capture, ordinary regeneration,
  bounded full-pipeline negotiation, and selective negotiated repair from an
  explicit typed provider horizon. `wfcmusicpass=1` preserves only public
  cells, score semantics, seed/configuration, and the recomputed signature;
  latent sequence keys remain private.
- The project-owned SMF layer reads and writes format 0 and 1 event streams;
  the score exporter emits deterministic format 0. Semantic MIDI import now
  provides exact note pairing, voice lanes, explicit unsupported-event policy,
  and import receipts. Caller-selected fixed-grid voice excerpts become
  melody/rhythm/harmony training documents with explicit provenance. General
  expressive-performance learning and automatic polyphonic reduction remain
  separate work; see [music import](docs/music-import.md).
- Music Audio v1 now provides exact tempo-to-frame conversion, bounded
  fixed-point PCM16 synthesis, and a canonical RIFF/WAVE writer. Music Studio
  connects the persistent pass owner to native artifact exporters and a
  pas2js lock/repair/piano-roll workbench with user-initiated HTML playback.
  All corpus phrases and synthesis code are project-owned. Its model remains
  one monophonic voice over a fixed two-bar form.
- A lazy arrangement iterator now separates total duration from local section
  size. Music Studio accepts user-defined seconds with visible bar rounding,
  solves fresh context-constrained sections, and streams PCM to RIFF/RF64
  through native FPC or browser file APIs. There is no arbitrary duration cap;
  numeric/storage capacity and finite local search still apply. The supplied
  source prevents immediate harmonic-form repeats, not global recurrence or
  long-range thematic failure; see [arrangements](docs/music-arrangement.md).
- Included native FPC development tools serve all maintained browser demos and
  validate executed browser evidence. Portable conformance runs in real
  browsers; OS/process checks remain native. See
  [development tools](docs/development-tools.md).
- Extend the pipeline through meter/phrase -> rhythm -> harmony -> melody ->
  bass/voicing -> dynamics/ornamentation, extending the independent-role baseline
  with key/scale spelling, general temporal voice-leading constraints,
  instrument-specific range models, independently learned rhythms, and locked motifs.
- Extend the current native WAV export and browser HTML-audio presentation
  with optional embedded native playback or a thin WebAudio host only when
  needed, without making a media backend part of the exact runtime foundation.
- Extend motif work from exact public-token locks and pass-level repair to
  phrase-aware coordinate scopes, transformations, and deterministic musical
  objectives.

**Exit gate:** generated measures have valid duration totals, pitches and
harmonies meet the selected model's constraints, semantic score/MIDI
round-trips are tested, native and web agree on the event list, audio
presentations consume the same validated result, and the standard demo has no
required GPL dependency. Foundation v1, Negotiated Variation v1, Music Studio,
Ensemble v1, and Independent Voices v1 satisfy important parts of this gate, including browser UIs,
portable audible output, and explicit-policy semantic MIDI import, but not the full
polyphonic/harmonic breadth or expressive-performance learning.

### Text and sequences

- Version 1 now provides pretokenized UTF-8 bounded learning, typed BOS,
  explicit observed start/end domains, raw counts, order-N latent states,
  structural recombination, derived BOS-free wrapped cycles, projection-aware
  pass helpers, independent path validation, and canonical `wfcs=1` text on
  native FPC and pas2js.
- Text Completion Foundation v1 now adds whole, prefix, suffix, fragment, and
  cycle extents; atomic prefix/suffix/mask/locked-span helpers; exact globally
  feasible token domains with aggregate learned weights; a lossless
  project-owned Unicode-scalar tokenizer; a caller-defined tokenizer learning
  contract; anchored infill; prefix continuation; structured failure; and
  independent text/token validation.
- A shared native FPC seed-zero fixture proves `e | c` at one
  unresolved position, completes `the quick fox rests.`, rejects a truncated
  whole sample while accepting its prefix extent, and replays identically with
  no external dependency.
- Text Pass Composition v1 now owns a structure -> lexical -> punctuation DAG.
  Lexical depends on structure; punctuation depends directly on both prior
  passes through an atomic two-source projection bundle. Complete map
  preflight, exact `@wfctf1:` fragments, public-token trace projection,
  independent three-layer validation, transactional failure/recovery, and
  selective descendant regeneration are checked on native FPC and pas2js.
- The same project-owned showcase now has console and interactive
  browser hosts. Its seed-zero `A sun rises brightly!` fixture and portable
  signature are checked headlessly, including a deliberate contradiction and
  exact recovery after clearing constraints.
- Add a standard lossless word tokenizer only with project-owned, versioned
  Unicode word-boundary data, then extend learned models with useful offset
  constraints.
- Generalize the completion and pass-composition demonstrations into
  arbitrary-corpus training/generation CLIs and an editor that exposes exact
  single-model domains plus causal reasons for token acceptance or rejection.
- Evaluate exposing generic Pass Negotiation v1 through the text owner. The
  current owner remains an honest one-way atomic cascade; integration must
  preserve public-token trace projection, dirty-root semantics, and its own
  replay version rather than silently changing `TryGenerate`.

**Exit gate:** output contains no forbidden learned neighborhoods, boundary and
lock behavior is covered by tests, seeded completions match across native and
web, and documentation describes this accurately as constraint-driven
completion rather than an LLM replacement.

## Phase 5: make the literature

Novel mechanisms will live in reproducible experiments before becoming stable
API. Promising research areas include:

- conflict-directed, partial-nogood, and minimal-change successors to the exact
  chronological full and selectively scoped negotiation baselines;
- automatic, broader, and provably minimal repair-horizon selection beyond the
  caller-chosen descendant closure used by Selective Negotiation v1;
- soft constraints and objective functions alongside hard constraints;
- explanation graphs and minimal contradiction sets;
- persisted or streaming forms of the derived trace layout, beyond the current
  detached in-memory ranges that preserve Trace v1 chronology and legacy
  report fields (see [traces](docs/traces.md));
- streaming and chunk-boundary reconciliation for large or infinite worlds;
- minimal-change counterfactual search beyond dependency-closure regeneration;
- constraint transfer between representations, such as rhythm influencing a
  visual layout or semantic layers guiding 3D decoration.

Each experiment must state a hypothesis, baseline, fixtures, seed set, metrics,
and stopping rule. Multi-pass models should be compared with equivalent
flattened single-pass models using rule/state count, validation success,
propagations, contradictions, backtracks, runtime, memory, and edit locality.
Raw results and negative findings belong in `docs/research/` beside the prose.

The [streaming trace record](docs/research/streaming-causal-traces-v1.md)
describes event-history-free observation with unchanged causal identities and
explicitly incomplete recent windows. It documents the bounded bookkeeping
argument and finite native/browser comparisons, not a new search algorithm or
an unmeasured runtime advantage.

The first [Pass Negotiation v1 record](docs/research/pass-negotiation-v1.md)
publishes its algorithm, fixed fixtures, seed set, counters, portable goldens,
stopping rules, and negative findings. It does **not** yet compare the
multi-pass search against an equivalent flattened model; that measurement is a
required next experiment, so no relative-efficiency claim is made.

The companion
[Selective Negotiation v1 record](docs/research/selective-pass-negotiation-v1.md)
fixes the descendant-horizon equation, immutable-provider boundary, comparison
fixtures, portable transcripts, costs, and negative findings. It makes no
cell-minimal, conflict-directed, horizon-minimal, or relative-efficiency claim.

The [Music Studio form experiment](docs/research/music-studio-form-v1.md)
compares short and whole-form rhythm contexts on a fixed 16-seed set. Its
checked 32-row CSV includes all failures and counters. It explicitly changes
the model language and therefore makes no equivalent-model efficiency claim.

### Exit gate

Any mechanism described as an advantage of pass-based WFC has a reproducible
benchmark and documented counterexamples. Experimental APIs remain marked as
such until at least two domain ecosystems use them successfully.

## Documentation and developer experience

Documentation grows with every phase rather than being postponed until 1.0.

- Expand the examples index and give every example prerequisites, commands,
  expected output, invariants, and troubleshooting notes.
- Document the algorithm, terminology, pass semantics, model format, public
  API, extension points, native builds, pas2js builds, and compatibility policy.
- Generate API reference material from Pascal comments and keep small runnable
  snippets beside conceptual explanations.
- Add architecture decisions and research notes when behavior is subtle or
  intentionally novel.
- Add `CONTRIBUTING.md`, `CHANGELOG.md`, `SECURITY.md`, a code of conduct,
  issue templates, release checklists, and a citation file.
- Publish live browser demos and a versioned documentation site from CI.
- Use semantic versions. Keep experimental work in 0.x releases and publish
  1.0 only when the core and pass contracts are stable.

## Licensing and provenance

The project remains under the MIT license. To make that promise meaningful:

- add SPDX identifiers or an equivalent clear license reference to maintained
  project source;
- maintain a third-party notice and asset manifest containing origin, author,
  version, license, modifications, and hashes;
- keep the project-owned MIT FPC/pas2js media and presentation stack as the
  standard path; the original music studies have been ported, and the old
  playback submodule and engine shell are removed, not retained as exceptions;
- verify or replace sheet-music images, samples, FBX models, icons, fonts, and
  browser assets whose redistribution terms are not recorded;
- require corpus and learned-model metadata to retain source provenance and
  license information;
- run a license/provenance check as part of release CI.

### Exit gate

A release archive can be built from source, contains no unexplained binary or
asset, includes all required notices, and every standard library and demo can
be distributed under the repository's MIT terms.

## Definition of 1.0

WFC 1.0 is ready when:

- the reference solver and pass pipeline have stable, documented contracts;
- native FPC and pas2js pass the same conformance fixtures;
- deterministic replay, validation, contradiction diagnostics, and model I/O
  are production-quality;
- the 2D, 3D, music, and text ecosystems each contain a reusable library plus
  polished native and browser demonstrations;
- learning, validation, running, and inspection tools are documented and
  tested;
- research claims are reproducible;
- a new contributor can build, test, understand, and extend the project from a
  clean clone; and
- the complete release is legally and practically redistributable under MIT.

That is a large destination. The intent is not to rush there with four fragile
demos. It is to make each layer strong enough that the next strange idea has a
solid place to land.
