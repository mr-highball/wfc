# WFC

WFC is a constraint-driven generation library written in Pascal for Free
Pascal Compiler (FPC) and pas2js. It models user-defined values on a 2D or 3D
graph, applies directional constraints, and supports dependency-planned passes
whose rules and results remain separate.

The pass system is the larger idea: generate terrain first, then foliage,
roads, housing, or any other layer while constraining each stage from the
result before it. The same approach can be specialized for world generation,
modular 3D structures, music, text, and other discrete design problems.

> **Project status:** the original API and greedy traversal solver remain
> available. The opt-in reference solver now provides fixed-point propagation,
> deterministic weighted Shannon-entropy observation, bounded backtracking,
> explicit deny-all adjacency, caller-owned per-cell domains, structured
> contradiction reports, atomic dependency-DAG pipelines, named cross-pass
> constraints, selective descendant regeneration, and separately versioned
> bounded whole-assignment pass negotiation on native FPC and pas2js. The
> ordinary one-way solver retains its replay contract; negotiated solving keeps
> one atomic Trace-v1 report per round and commits only final success. Selective
> Negotiation v1 now restricts that search to canonical requested roots and
> their exact descendant closure while preserving clean layers and random
> streams. The first
> specialized ecosystem now includes independently
> validated terrain → biome → foliage and six-layer settlement worlds with
> portable signatures; the former also has an interactive browser presentation
> compiled from the same Pascal model. Deterministic training primitives learn cardinal
> constraints and raw weights from ordered heterogeneous 1D/2D corpora, merge
> compatible models, apply immutable results to a graph, and round-trip them
> through a strict canonical text format. A second learner extracts weighted
> overlapping 2D footprints, compiles exact structural overlap, captures
> latent assignments, and independently validates projected token grids
> through canonical `wfcp=1` artifacts. Pattern-Projected Pass Composition v1
> now materializes wrapped pattern contributions as an exact public-token pass,
> keeps learned weights solely on the latent layer, and validates the complete
> representation boundary before atomic commit. The sequence foundation learns
> bounded order-N models from pretokenized UTF-8 corpora, derives structural
> suffix/prefix recombination, applies whole/prefix/suffix/fragment or derived
> wrapped graph domains, composes latent and public-token passes in either
> direction, and round-trips strict canonical `wfcs=1` artifacts. The text
> foundation adds project-owned Unicode-scalar tokenization, atomic prefix,
> suffix, mask, and locked-span constraints, exact globally feasible token
> domains, anchored infill, prefix-only continuation, and independent text
> validation on native FPC and pas2js/Node. Text Pass Composition v1 adds a
> persistent structure -> lexical -> punctuation owner, atomic multi-source
> projection maps, exact surface fragments, independent cross-layer
> validation, sanitized public traces, selective regeneration, and one shared
> native/Node/browser fixture. Music Foundation v1 adds an exact
> integer score IR, canonical melody/rhythm/harmony cells, two-source latent
> pass projection, strict `wfcmusic=1` score text, a project-owned SMF
> format-0/1 codec, and a format-0 score exporter. Its portable three-pass
> example runs on native FPC and pas2js/Node without a playback dependency.
> Music Negotiated Variation v1 promotes that path into a reusable persistent
> harmony -> rhythm -> melody owner with public-token constraints and motif
> locks, immutable validated compositions, ordinary and bounded negotiated
> full/selective regeneration, portable public signatures, and strict
> `wfcmusicpass=1` result replay.
> Portable Recipe Foundation v1 adds an immutable rank-1/2/3 authored-rule
> model with explicit denials and required directions, strict `wfcrules=1`
> text, and an immutable `wfcpipeline=1` recipe that owns canonical model,
> rules, pattern, and sequence resources. Recipes validate version pins,
> topology, pass visibility, the complete dependency DAG, typed projection
> bridges, public vocabularies, provenance, and signed-offset token
> requirements before any runtime graph exists.
> Portable Pipeline Execution v1 compiles those recipes into fresh transactional
> graphs, resolves caller locks and domains through transform aliases, and runs
> deterministic one-way or bounded negotiated solves. Strict canonical
> `wfcpipeline-run=1` and `wfcpipeline-result=1` artifacts preserve provenance,
> options, reports, public layers, and failed outcomes across native FPC and
> pas2js/Node. Runtime v2 lowers public Pattern2D and Sequence locks and
> domains back into bounded private source domains while retaining bridge-v1
> forward-only replay. Shared Pascal application units power a recipe validator and a
> headless recipe-plus-run executor; their native and Node hosts contain only
> bounded file, standard-stream, and process plumbing.
> Pipeline v2 adds exact signed-offset and finite any-of-neighborhood reads
> across staged provider passes. Causal Trace v1 adds opt-in chronological
> decision/removal/backtrack evidence, provider-pass cause links, per-pass
> slices, portable versioned hashes, structural validation/query helpers, and
> a shared native/pas2js console inspector. Voxel Foundation v1 adds immutable
> rotation-aware prototypes, explicit six-face socket compatibility, vertical
> support, captured scenes, independent validation, and renderer-neutral
> integer surface meshes. Building 3D v1 now composes a full depth-aware
> footprint -> structure -> envelope/roof -> props DAG through checked public
> prototype maps, validates the result independently, and runs the same
> textual/mesh demonstration on native FPC and pas2js/Node. Its project-owned
> presentation layer adds immutable four-pass face lineage, fixed-integer
> four-yaw isometric commands, stable painter ordering and hit testing,
> deterministic native SVG, and an interactive pas2js/Canvas2D workbench with
> an exact seeded browser fixture.

## Features

- 2D and 3D graph topology with optional wrapped boundaries
- fluent rules over caller-defined string values
- compatibility-preserving wildcard rules plus explicit directional deny-all
- caller-owned, pass-local per-cell domains with canonical value ordering
- required directional rules and selection/invalid-state callbacks
- stable, labeled, zero-based passes with isolated values, rules, and outputs
- deterministic dependency-DAG execution with stable creation-index tie breaks
- explicit legacy, overlay, and transform pass modes
- named same-coordinate, exact-offset, and finite any-of-neighborhood
  cross-pass constraints with selective descendant-only regeneration
- transactional full and selective solves with selected-pass restoration
- compatibility-preserving empty-pass copying and previous-pass constraints
- explicit pipeline seeds with stable, independent per-pass random streams
- an opt-in propagating solver with positive relative weights, deterministic
  fixed-point Shannon entropy, exact unit-weight MRV compatibility, and bounded
  backtracking
- atomic staging, independent validation, and structured execution reports
- opt-in bounded chronological negotiation over exact completed pass
  assignments, with distinct local/pass budgets and atomic rounds
- versioned negotiation attempt transcripts with copied exact exclusions,
  portable hashes, and matching native/pas2js fixtures
- separately versioned selective negotiation over an explicit descendant-closed
  repair horizon, with canonical scope arrays and clean-pass/RNG preservation
- opt-in causal traces covering caller filters, decisions, propagation,
  contradictions, backtracking, pass staging/skipping, and pipeline commit or
  rollback
- versioned portable trace hashes, per-pass event slices, public query and
  validation helpers, and matching native/pas2js trace fixtures
- typed 2D terrain/biome/foliage and selective-settlement libraries with
  independent semantic checkers
- versioned, fixed-token 2D layer signatures shared by native FPC and pas2js
- matching seeded golden fixtures on native FPC and pas2js/Node
- an interactive browser world with synchronized layers, locks, and a seeded
  headless-browser conformance fixture
- immutable, versioned one-layer model data shared by native FPC and pas2js
- deterministic first-seen learning for ordered heterogeneous tokenized 1D/2D
  corpora with independent open or wrapped boundaries and explicit D4
  augmentation
- checked deterministic merging of compatible learned models without inventing
  cross-sample seam relations
- strict canonical `.wfcm` text with UTF-8 percent encoding and byte-exact
  decode/re-encode validation
- immutable rank-1/2/3 hand-authored local rules with positive weights,
  legacy wildcards, explicit deny-all directions, required support, exact
  reciprocal closure, fresh-pass adaptation, and strict `wfcrules=1` text
- immutable `wfcpipeline=1` recipes with owned canonical typed resources,
  closed adapter and bridge kinds, explicit acyclic dependencies, static
  public vocabularies, provenance, version pins, fixed resource/complexity
  limits, and exact decode/re-encode validation
- fresh transactional compilation of portable recipes, including typed
  resource adapters, materializing projection bridges, public requirements,
  independent commit validators, and definition-surface verification
- immutable `wfcpipeline-run=1` invocations and `wfcpipeline-result=1` outcomes
  with strict canonical text, complete provenance, deterministic replay,
  structured failures, public-only layers, and fixed allocation/encoding limits
- transform-alias-aware public locks and domains with exact intersection,
  deterministic bridge-v2 inverse lowering into private Pattern2D and Sequence
  states, pre-publication conflict detection, and project-owned bounded lookup,
  sorting, and intersection shared by native FPC and pas2js
- a committed LearnedPatternWorld recipe/run/result bundle that constrains the
  private learned terrain model exclusively through eight public bridge-v2
  locks and reproduces all three established seed-zero public layer hashes
- dependency-free recipe-validation and headless-execution application units
  with thin native and Node command-line hosts and documented exit contracts
- deterministic overlapping 2D pattern extraction with heterogeneous corpora,
  open/wrapped sources, square-footprint D4 augmentation, structural
  compatibility, explicit latent-to-token projection, and strict `.wfcp`
  replay artifacts
- wrapped same-shape pattern-to-public pass composition with one exact clause
  per footprint coordinate, unit-weight public values, independent commit-time
  validation, and a reusable two-pass owner
- deterministic bounded sequence learning with typed BOS history, raw counts,
  order-N latent states, structural suffix/prefix recombination, explicit
  public-token projection, and strict canonical `wfcs=1` artifacts
- whole, prefix, suffix, fragment, and cycle sequence extents with atomic bulk
  token masks and exact forward/backward public-domain analysis
- project-owned Unicode-scalar tokenization across native UTF-8 and pas2js
  UTF-16, caller-defined tokenizer learning, anchored text infill, deterministic
  prefix continuation, and independent token/text validation
- a reusable three-pass text owner with stable structure, lexical, and
  punctuation layers; direct two-provider surface constraints; exact versioned
  fragments; public-token causal traces; atomic failure/recovery; and an
  interactive pas2js workbench
- atomic N-source sequence projection bundles with complete preflight, OR
  alternatives inside each provider map, and AND semantics across providers
- immutable exact music scores with complete per-voice timelines and exact
  meter-boundary validation
- strict `wm1` melody, `wr1` rhythm, and `wh1` harmony cells with lossless
  aligned monophonic projection and rebuild
- rhythm + harmony -> melody pass composition through public-token maps over
  private latent sequence states
- a reusable persistent music-pass owner with public motif locks, atomic
  ordinary regeneration, bounded full/selective negotiated repair, independent
  validation, immutable score capture, and portable public signatures
- strict canonical `wfcmusic=1` score text, a project-owned SMF format-0/1
  byte codec, and deterministic format-0 score export
- strict canonical `wfcmusicpass=1` public composition results with no latent
  graph keys or external runtime dependency
- [immutable voxel kits](docs/voxel3d.md) with deterministic yaw variants,
  explicit socket relations, support-aware six-direction graph compilation,
  portable scene signatures, independent connectivity validation, and integer
  quad meshes
- [multi-pass Building 3D](docs/building3d.md) with typed 3D massing
  blueprints, private-key-safe voxel projection maps, target-yaw spatial
  clauses, supported structure, facade/roof and prop overlays, independent
  validation, selective regeneration, and matching native/pas2js output
- immutable Building presentation modes with complete public same-cell pass
  lineage and exact structure/prop mesh composition
- project-owned signed fixed-subcell isometric commands with checked integer
  projection, four camera yaws, explicit stable painter sorting, reverse hit
  testing, auto-fitted bounds, and portable view signatures
- deterministic native SVG with public polygon metadata and an interactive
  pas2js Canvas2D Building workbench with seed, selective-regeneration, view,
  Z-clip, picking, and lineage controls
- iterative traversal without a graph-sized call stack
- extension hooks for custom graph and entry behavior
- one Pascal core for native FPC and pas2js

## Basic use

```pascal
uses
  SysUtils,
  wfc;

var
  Graph: TGraph;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
begin
  Graph := TGraph.Create.Reshape(5, 5, 1);
  try
    Graph.AddValue('A')
      .NewRule(AllDirections, ['A', 'B']);

    Graph.AddValue('C')
      .NewRule([gdNorth, gdSouth], ['A', 'B']);

    Options := DefaultGraphSolveOptions;
    if not Graph.TrySolve(Options, Report) then
      raise Exception.CreateFmt('WFC failed in pass %d',
        [Report.FailedPassIndex]);
  finally
    Graph.Free;
  end;
end;
```

For a complete terrain-to-foliage pipeline, including `SwitchToPass`,
`PassGraph`, and `RequirePrevious`, see [pass-system semantics](docs/passes.md).
For dependency roles, pass modes, `RequireFromPassAt`,
`RequireAnyFromPass`, boundary behavior, topological execution, and selective
regeneration, see [pass DAGs](docs/pass-dags.md).
For the reference algorithm, atomicity contract, reports, and exact constraint
semantics, see the [reference solver](docs/solver.md).
For full-pipeline chronological reopening, separate local and pass budgets,
attempt reports, exact assignment exclusions, replay, and current complexity
limits, see [bounded pass negotiation](docs/pass-negotiation.md).
For bounded negotiation inside an explicit descendant closure, canonical root
and active arrays, clean-pass ownership, outer transcript identity, and the
non-minimal repair boundary, see
[selective pass negotiation](docs/selective-negotiation.md).
The self-checking
[negotiated-repair example](examples/2D/04_NegotiatedRepair/README.md)
contrasts a too-narrow leaf horizon with a successful provider-root repair on
native FPC and pas2js/Node.
For `CaptureTrace`, event/cause semantics, per-pass slices, stable hashes,
query/validation helpers, current limits, and the console inspector, see
[causal solve traces](docs/traces.md).
For exact replay behavior, callback requirements, and algorithm versioning,
see [deterministic generation](docs/determinism.md).
For the reusable world model, typed locks, validator, signatures, and console
and browser demonstrations, see the [2D ecosystem](docs/world2d.md).
For learned frequencies and adjacency, immutable model data, graph adaptation,
the `.wfcm` format, and exact replay inputs, see
[model learning and priming](docs/learning.md).
For hand-authored rank-1/2/3 rules, declarative multi-resource pipeline recipes,
run/result artifacts, runtime alias semantics, and the portable command-line
contracts, see [portable pipeline artifacts](docs/pipeline-artifacts.md).
For multi-cell extraction, structural overlap, latent assignment, projection,
independent validation, and the `.wfcp` format, see
[overlapping 2D patterns](docs/patterns.md).
The self-checking
[learned-pattern world](examples/2D/05_LearnedPatternWorld/README.md) connects
that latent model to public terrain, foliage, and structure in one four-pass
native/pas2js pipeline and now includes canonical recipe, run, and result
artifacts driven through public terrain locks; its exact hypothesis and
nonclaims are in the
[Pattern-Projected Pass Composition v1 record](docs/research/pattern-projected-passes-v1.md).
For bounded order-N learning, typed BOS boundaries, open and derived wrapped
generation, pass projection, and canonical `wfcs=1` text, see
[sequence models](docs/sequences.md).
For Unicode-scalar learning, exact token-domain analysis, prefix/suffix and
interior locks, deterministic infill, the three-pass text owner and browser
workbench, validation, and current non-LLM scope, see
[text constraint completion](docs/text.md).
For the exact score model, fixed-quantum cells, music pass projection,
public motif locks and negotiated variation, `wfcmusic=1` and
`wfcmusicpass=1`, Standard MIDI Files, and the optional playback boundary, see the
[music foundation](docs/music.md).
For the reusable footprint-to-props building owner, voxel pass bridge,
validation, capture, signatures, immutable graphical commands, native SVG,
and depth-three browser workbench, see
[Building 3D](docs/building3d.md).

## Build and test

Run the checked native build and conformance suite from the repository root:

```powershell
.\build.ps1
```

```bash
./build.sh
```

Both entry points compile with checked FPC options, keep all output under
`build/`, run the core, 2D ecosystem, selective-settlement, radius-one
learning, overlapping-pattern, pattern-pass composition, sequence,
text-completion, text-pass, authored-rule and portable pipeline recipe,
compile, run, result, runtime, validator, and runner artifacts,
score/cell, music-graph, music-pass, music-result-codec, SMF, score-to-MIDI,
voxel, Building 3D, and
causal-trace, pass-negotiation, and selective-negotiation suites,
smoke-test the portable console examples—including the bounded/wrapped spatial
dependency, causal-trace inspector, bounded pass-negotiation proof, negotiated
2D repair, anchored text infill, three-pass text composition, negotiated music
variation, learned-pattern four-pass world, and depth-three building
proofs—compile and smoke-test the native portable command-line hosts, compile the
fixed-integer isometric, SVG, and Building-view suites, write a deterministic
seed-zero Building SVG, and preserve failure exit codes.
The repository also includes an FPM package, a runtime-only Lazarus package,
and the same conformance sources for pas2js/Node. Separate `build-browser.ps1`
and `build-browser.sh` entry points stage the interactive 2D world;
`build-browser-text.ps1` and `build-browser-text.sh` stage the text pass
workbench; and `build-browser-building3d.ps1` and
`build-browser-building3d.sh` stage the Building workbench. None of the
staging entry points commits generated JavaScript.
See [building and testing](docs/building.md) for compiler overrides, package
commands, output paths, and pas2js setup. The
[Building graphical guide](examples/3D/03_BrowserBuilding/README.md) records
its native command, controls, and seed-zero pipeline `1:F1EF0EB6`, view
`AC7290C0`, `140`-face browser contract.

The core, specialized units, conformance suites, portable examples—including
the pass-composed and negotiated-variation music examples—and building-kit
console need no submodule.
Only the two legacy music playback experiments require Lazarus/LCL, SDL2, and
the optional GPL-3.0 SoundShop submodule; they are isolated from the
dependency-free MIT build path. See the [examples index](examples/README.md)
for exact status and commands.

The portable foundation is project-owned Pascal. Whenever a capability can
reasonably be implemented here instead of adding a library, the project
implements and maintains its own FPC/pas2js version; when that choice is
debatable, project-owned Pascal is the default. Core and runtime units depend
only on repository units and the applicable standard FPC/pas2js RTL. Engines,
native window systems, and media backends may be optional edge adapters, and
development tools may assist builds, tests, conversion, or inspection, but
none may leak into
core/runtime APIs or define canonical algorithms, models, artifacts,
validation, or replay behavior.
The complete admission rules and optional-adapter boundary are recorded in the
[dependency policy](docs/dependencies.md).

## Direction

The [roadmap](ROADMAP.md) covers the remaining reference-solver work, richer
pass composition, conflict-directed and partial-nogood negotiation research,
interactive trace stepping and domain views, richer failed-clause/minimal-core
explanations, trace streaming, higher-dimensional and
cross-pass learning, richer validation and inspection tools,
2D/3D/music/text ecosystems, pas2js
playgrounds, reproducible research, documentation, and release provenance.
Current examples are indexed under [examples](examples/README.md).

## License

WFC is released under the [MIT License](LICENSE).

**Tip jar**

- BTC: `bc1q55qh7xptfgkp087sfr5ppfkqe2jpaa59s8u2lz`
- LTC: `LPbvTsFDZ6EdaLRhsvwbxcSfeUv1eZWGP6`
