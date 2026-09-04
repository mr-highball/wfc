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
  overlay, and transform modes, named same-coordinate constraints, and atomic
  descendant-only regeneration;
- a versioned portable random source with an explicit pipeline seed,
  independent index-derived pass streams, run-to-run rewind, and matching
  native/pas2js golden fixtures;
- an opt-in reference solver with maintained domains, queue-based fixed-point
  propagation, deterministic fixed-point weighted Shannon entropy, an exact
  unit-weight minimum-domain path, bounded chronological backtracking,
  independent final validation, and structured per-pass reports;
- atomic reference-solver staging across full and selectively regenerated pass
  closures, including locks, named dependencies, definitionless-pass behavior,
  skipped-layer preservation, and rollback on any failed descendant;
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
- an immutable overlapping-pattern 2D model with heterogeneous extraction,
  exact structural compatibility, latent assignment capture, independently
  checked projection, strict canonical `.wfcp` text I/O, and a portable seeded
  demonstration;
- an immutable bounded sequence model with typed BOS history, deterministic
  order-N learning from pretokenized UTF-8 corpora, raw counts, structural
  suffix/prefix recombination, open and derived wrapped graph adaptation,
  latent/public pass projection, and strict canonical `wfcs=1` text I/O;
- a project-owned Music Foundation v1 with an immutable exact score IR,
  reduced rational helpers, strict `wm1`/`wr1`/`wh1` fixed-quantum cells,
  lossless aligned monophonic projection/rebuild, and exact rhythm and
  pitch-class maps across private latent sequence passes;
- strict canonical `wfcmusic=1` score text, a defensive project-owned SMF
  format-0/1 byte codec, deterministic format-0 score export, four focused
  conformance suites, and a native/pas2js three-pass composition example;
- stack-safe iterative traversal of large planes while preserving the legacy
  solver's north/east/south/west depth-first order;
- checked one-command native build gates, an FPM package, a runtime-only
  Lazarus package, and a hosted stable/development CI workflow;
- single-pass and multi-pass console world examples;
- two isolated legacy music experiments, including manually inferred note
  adjacency and optional SoundShop/SDL2 playback;
- building-kit constraints, a console renderer, and a Castle Game Engine
  project skeleton;
- native/pas2js conformance runners covering the core, pass contracts, 2D
  domain semantics, atomic failure, and layer-signature parity.

It is not yet the finished system described above:

- the version-2 reference solver now supports scale-canonical integer weights,
  explicit zero-support adjacency, and caller-owned per-cell domains, but
  deterministic restarts, timing, a stable trace hash, richer explanations,
  and more scalable domain representations remain to be built;
- pass DAGs, named overlay layers, same-coordinate cross-layer requirements,
  selective regeneration, and structured dependency diagnostics are now
  operational; offset and neighborhood reads, cyclic negotiation/repair, and
  projection maps beyond exact same-coordinate token relations remain to be
  designed;
- the examples index now records targets, dependencies, build commands, and
  honest completion status, but full per-example tutorials, invariants,
  expected output, and troubleshooting guides remain to be written;
- the Castle example does not yet render generated building geometry, and the
  building-kit demo currently runs with a depth of one;
- the first interactive pas2js demo and browser test host now exist, but there
  is no complete pass/domain inspector or stable native/pas2js decision-trace
  parity suite (canonical seeded value fixtures do already match);
- model learning now covers pretokenized cardinal radius-one corpora,
  structurally compatible overlapping 2D footprints, and bounded order-N
  sequences with explicit projection; tokenizers, wrapped sequence training,
  3D neighborhoods, provenance metadata, and the planned
  validation/run/inspection tools remain;
- the hosted CI definition is present, but its first remote run still needs to
  be observed before the Phase 0 exit gate is claimed complete;
- the portable music path has no playback or browser UI yet; SoundShop/SDL2
  remains only in two legacy Lazarus experiments behind an optional GPL-3.0
  submodule and is not part of the MIT runtime or standard demo;
- Music Foundation v1 proves quantized single-voice pitch-class composition
  and score-to-SMF export, but polyphonic cell projection, key and chord
  semantics, MIDI-to-score import/learning, phrasing, voicing, ornamentation,
  and the complete music exit gate remain open.

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
  maintain the FPC/pas2js version here. Keep runtime dependencies minimal:
  repository units and the applicable standard RTL only. Third-party engines,
  viewers, and media backends may be isolated optional adapters. Optional
  development tooling must not leak into core/runtime APIs or define canonical
  models, algorithms, artifact formats, validation, or replay behavior.

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
Phase 1 stays open until deterministic restarts, timing, and native/pas2js
stable trace-hash parity satisfy the exit gate below.

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
compatibility default. Version-1 acyclic dependency graphs, named cross-pass
constraints, deterministic topological execution, and selective descendant
regeneration are now implemented; bounded feedback requires a separate
termination and replay contract.

The first packaged domain fixture proves the sequential subset with terrain →
biome → foliage. The selective-settlement fixture expands that proof to
terrain → hydrology/biome → roads → housing → foliage, including named
overlays, non-linear dependency closure, an intentionally illegal descendant
lock, transactional rollback, independent validation, exact recovery, and
matching native/pas2js layer signatures. Pass inspection and richer causal
explanations remain open Phase 2 work.

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

The observation and corpus version-1 primitives now preserve first-seen token
order and raw frequency counts, observe cardinal radius-one relations in
ordered heterogeneous 1D/2D samples without inventing cross-sample seams,
support open/wrapped boundaries and explicit D4 augmentation, checked-merge
compatible models, store ordered source shapes in immutable model data, adapt
representable models to `TGraph`, and round-trip strict canonical `.wfcm`
documents using the `wfcm=1` and `wfcm=2` profiles on native FPC and pas2js.
The graph adapter now maps active zero-support rows to explicit denial instead
of rejecting or widening them. Portable single-sample and corpus fixtures
prove those vertical slices. The
overlapping-pattern version-1 layer additionally extracts deterministic
rectangular 2D payloads, supports open/wrapped heterogeneous sources and
square-footprint D4 augmentation, compiles exact structural overlap, keeps
latent assignments separate from projected tokens, independently validates
both, and round-trips strict standalone `wfcp=1` artifacts.
The sequence version-1 layer learns bounded order-N latent states from ordered
pretokenized UTF-8 corpora, resets typed BOS history at every sample boundary,
retains raw observation/start/end counts, derives exact suffix/prefix
compatibility, adapts open endpoints or BOS-free wrapped cycles, composes
latent and public-token passes in either direction, and round-trips strict
standalone `wfcs=1` artifacts. Corpus seams are absent, while compatible
states may intentionally recombine. The broader tools, tokenizers, wrapped
sequence training, provenance metadata, higher-dimensional extraction, and
complete exit gate remain open.

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
console demo. An interactive browser field instrument now runs the same model,
shows all three layers, supports cell locks, and exposes a deterministic
headless self-test. This is the first vertical slice, not the completed exit
gate.

- Extend the current bounded/wrapped grid with masked and chunked topologies.
- Extend the current terrain/biome/foliage model with coast/hydrology, roads,
  settlements, and richer decoration helpers.
- Image/tile-set learning with rotation and reflection policies.
- Extend the console example and current browser field instrument into native
  and browser inspectors that expose domains, propagation, and contradiction
  traces across the complete multi-pass pipeline.

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

- Music Foundation v1 now provides an immutable event model covering exact
  time, duration, pitch, voice, velocity, meter, tempo, rests, chords, and
  complete timeline boundaries. `wfcmusic=1` preserves it canonically.
- Version-1 fixed-quantum cells model melody attacks/holds/rests, rhythm
  actions, and harmony pitch classes. Exact projection/rebuild is available
  for aligned monophonic voices; chords remain available in scores and export.
- A portable three-pass fixture solves harmony and rhythm before melody,
  composes both constraints through latent sequence projection maps,
  independently validates the public relation, rebuilds a score, and checks
  canonical text and MIDI bytes on native FPC and pas2js/Node.
- The project-owned SMF layer reads and writes format 0 and 1 event streams;
  the score exporter emits deterministic format 0. A semantic score importer
  and event-stream learner remain to be designed.
- Extend the pipeline through meter/phrase -> rhythm -> harmony -> melody ->
  bass/voicing -> dynamics/ornamentation, including polyphonic representations,
  key/scale spelling, voice leading, instrument ranges, and locked motifs.
- Build optional project-owned native playback and a thin WebAudio host from
  pas2js without making a media backend part of the exact runtime foundation.
- Seeded variation and locked motifs so part of a composition can be preserved
  while another part is regenerated.

**Exit gate:** generated measures have valid duration totals, pitches and
harmonies meet the selected model's constraints, semantic score/MIDI
round-trips are tested, native and web agree on the event list, optional native
and WebAudio presentations consume the same validated result, and the standard
demo has no required GPL dependency. Foundation v1 satisfies important parts
of this gate but not the importer, polyphonic/harmonic breadth, browser UI, or
playback requirements.

### Text and sequences

- Version 1 now provides pretokenized UTF-8 bounded learning, typed BOS,
  explicit observed start/end domains, raw counts, order-N latent states,
  structural recombination, derived BOS-free wrapped cycles, projection-aware
  pass helpers, independent path validation, and canonical `wfcs=1` text on
  native FPC and pas2js.
- Extend the Unicode-aware 1D token topology with dedicated prefix, suffix,
  mask, and locked-span helpers and caller-selectable tokenizers.
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

- bounded negotiation or repair across the now-established acyclic pass DAG;
- soft constraints and objective functions alongside hard constraints;
- explanation graphs and minimal contradiction sets;
- streaming and chunk-boundary reconciliation for large or infinite worlds;
- minimal-change counterfactual search beyond dependency-closure regeneration;
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
- replace the required SoundShop GPL playback path with a project-owned,
  MIT-licensed FPC/pas2js implementation; any retained SoundShop integration
  remains a clearly separate optional GPL adapter outside standard builds and
  MIT demo binaries;
- treat SDL2 as an optional media-backend adapter and, when distributing it,
  document the exact binary source/version or build it reproducibly;
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
