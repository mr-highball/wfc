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
- an operational first pass-pipeline slice: stable labeled passes, isolated
  rules and values, sequential execution, prior-output copying, and
  same-coordinate constraints against the immediately preceding pass;
- a versioned portable random source with an explicit pipeline seed,
  independent index-derived pass streams, run-to-run rewind, and matching
  native/pas2js golden fixtures;
- an opt-in reference solver with maintained domains, queue-based fixed-point
  propagation, deterministic minimum-domain observation, bounded chronological
  backtracking, independent final validation, and structured per-pass reports;
- atomic reference-solver staging across the complete sequential pass pipeline,
  including locks, previous-pass constraints, definitionless-pass copying, and
  rollback on a failed later pass;
- a reusable `wfc_world2d` terrain/biome/foliage library, a separate semantic
  validator, fixed-token portable layer signatures, and a documented
  native/pas2js multi-pass demonstration;
- stack-safe iterative traversal of large planes while preserving the legacy
  solver's north/east/south/west depth-first order;
- checked one-command native build gates, an FPM package, a runtime-only
  Lazarus package, and a hosted stable/development CI workflow;
- single-pass and multi-pass console world examples;
- two music experiments, including manually inferred note adjacency;
- building-kit constraints, a console renderer, and a Castle Game Engine
  project skeleton;
- native/pas2js conformance runners covering the core, pass contracts, 2D
  domain semantics, atomic failure, and layer-signature parity.

It is not yet the finished system described above:

- the reference solver is an unweighted version-1 foundation; weights,
  deterministic restarts, timing, a stable trace hash, richer explanations,
  and more scalable domain representations remain to be built;
- atomic prepare/solve/validate/commit now covers the sequential `TrySolve`
  path, but named overlay layers, dependency graphs, selective regeneration,
  and general cross-layer diagnostics remain to be built; the legacy `Run`
  path intentionally retains its nontransactional callback behavior;
- the examples index now records targets, dependencies, build commands, and
  honest completion status, but full per-example tutorials, invariants,
  expected output, and troubleshooting guides remain to be written;
- the Castle example does not yet render generated building geometry, and the
  building-kit demo currently runs with a depth of one;
- there is no interactive pas2js demo, browser test host, or complete stable
  native/pas2js decision-trace parity suite (canonical seeded value fixtures
  do already match);
- the hosted CI definition is present, but its first remote run still needs to
  be observed before the Phase 0 exit gate is claimed complete;
- music playback currently depends on the GPL-licensed SoundShop submodule,
  which cannot be a required dependency of an MIT-only demo distribution
  without resolving or replacing that dependency.

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

The version-1 MVP now covers domains, fixed-point propagation, deterministic
minimum-remaining-values selection, locks, bounded backtracking, structured
contradictions, independent validation, and atomic pass staging. Phase 1 stays
open until weights, deterministic restarts, timing, and native/pas2js stable
trace-hash parity satisfy the exit gate below.

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

Two initial pass modes cover the intended uses:

- **transform pass:** begins with a copy-on-write view of the previous output
  and refines values in the same domain;
- **overlay pass:** writes a new named layer while reading committed layers
  such as terrain, roads, structures, or harmony.

Running a pass follows `prepare -> solve -> validate -> commit`. A failed pass
does not corrupt earlier committed layers. Sequential dependencies are the
default; explicit dependency graphs and bounded feedback can be added after
the sequential behavior is proven.

The first packaged domain fixture now proves the sequential subset with
terrain → biome → foliage, an intentionally illegal foliage lock, rollback,
independent validation, and matching native/pas2js layer hashes. Hydrology,
roads/housing, selective regeneration, named overlays, and dependency graphs
remain necessary before the full Phase 2 exit gate is satisfied.

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
- Record which constraint, neighbor, pass, and decision forced each value.
- Support selective regeneration with locked unaffected cells.
- Add a pass inspector that can step through layers and contradiction traces.

### Exit gate

An automated terrain -> hydrology/biome -> roads/housing -> foliage fixture
proves that houses never occupy water, foliage respects terrain and structures,
all earlier layers remain unchanged after later commits, a failed later pass
rolls back cleanly, and native FPC and pas2js produce identical layer hashes.

## Phase 3: models, priming, and learning

Rules should be easy to author by hand and possible to infer from examples.
The learner is a deterministic constraint-model builder, not a hidden machine
learning service.

### Deliverables

- Define a versioned, human-readable model format for symbols, topology,
  neighborhoods, weights, symmetry, boundary behavior, passes, dependencies,
  provenance, and source licenses.
- Implement the same model reader and writer for native FPC and pas2js.
- Build `wfc-learn` to extract adjacency, frequencies, neighborhoods, rotations,
  and reflections from grids, sequences, voxel samples, text, and musical event
  streams.
- Build `wfc-validate` to lint schemas, unreachable values, asymmetric rules,
  impossible requirements, missing assets, and contradictory pass inputs.
- Build `wfc-run` for headless, seeded generation and reproducible batch runs.
- Build `wfc-inspect` for rule graphs, pass layers, entropy/domain views,
  decision replay, and contradiction explanations.
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
console demo. This is the first vertical slice, not the completed exit gate.

- Extend the current bounded/wrapped grid with masked and chunked topologies.
- Extend the current terrain/biome/foliage model with coast/hydrology, roads,
  settlements, and richer decoration helpers.
- Image/tile-set learning with rotation and reflection policies.
- A console example, a native visual inspector, and an interactive browser
  playground showing the complete multi-pass world pipeline.

**Exit gate:** every displayed map passes terrain, connectivity, occupancy, and
cross-pass invariants; the same model and seed match between native and web.

### 3D structures

- Voxel and modular-building topologies with complete vertical constraints.
- Rotation-aware sockets, support/load rules, empty space, entrances,
  connectivity, roofs, and multi-floor relationships.
- Passes for footprint -> structure -> facade/roof -> props.
- A completed Castle Game Engine native viewer and a lightweight pas2js voxel
  viewer using the same generated model and output data.

**Exit gate:** the building kit uses depth greater than one, contains no
unsupported structural pieces or disconnected required entrances, renders the
same validated structure in both viewers, and includes licensed source assets.

### Music

- A musical event model covering time, duration, pitch, voice, velocity,
  meter, key, rests, and boundaries.
- Passes for meter/rhythm -> harmony -> melody -> ornamentation/voicing.
- Manual rule authoring plus MIDI/event-stream learning, MIDI export, native
  playback, and WebAudio playback from pas2js.
- Seeded variation and locked motifs so part of a composition can be preserved
  while another part is regenerated.

**Exit gate:** generated measures have valid duration totals, pitches and
harmonies meet the selected model's constraints, MIDI round-trips are tested,
native and web agree on the event list, and the standard demo has no required
GPL dependency.

### Text and sequences

- A Unicode-aware 1D token topology with explicit start/end boundaries,
  prefixes, suffixes, masks, and locked spans.
- Character, word, and caller-defined token models with learned n-gram and
  offset constraints.
- Passes for structure/template -> lexical fill -> punctuation or other
  domain-specific refinement.
- Native CLI completion and an interactive pas2js editor that exposes domains
  and explains why a token was selected or rejected.

**Exit gate:** output contains no forbidden learned neighborhoods, boundary and
lock behavior is covered by tests, seeded completions match across native and
web, and documentation describes this accurately as constraint-driven
completion rather than an LLM replacement.

## Phase 5: make the literature

Novel mechanisms will live in reproducible experiments before becoming stable
API. Promising research areas include:

- dependency DAGs rather than only linear pass sequences;
- bounded negotiation or repair between earlier and later passes;
- soft constraints and objective functions alongside hard constraints;
- explanation graphs and minimal contradiction sets;
- streaming and chunk-boundary reconciliation for large or infinite worlds;
- counterfactual regeneration: change one result while preserving everything
  unrelated;
- constraint transfer between representations, such as rhythm influencing a
  visual layout or semantic layers guiding 3D decoration.

Each experiment must state a hypothesis, baseline, fixtures, seed set, metrics,
and stopping rule. Multi-pass models should be compared with equivalent
flattened single-pass models using rule/state count, validation success,
propagations, contradictions, backtracks, runtime, memory, and edit locality.
Raw results and negative findings belong in `docs/research/` beside the prose.

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
- replace the required SoundShop GPL playback path with an MIT-compatible
  project implementation, or keep it as a clearly separate optional GPL
  integration that is not part of standard builds or MIT demo binaries;
- document the exact SDL2 binary source/version or build it reproducibly;
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
