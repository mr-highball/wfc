# Overlapping volumes and public constraints v1

Record date: 2026-09-06 UTC. This is a project implementation and conformance
record, not a claim of algorithmic novelty or general musical/spatial quality.
The [API and format guide](../overlapping-3d.md) describes the reusable units.

## Question

Can a learned joint cuboid representation participate in a real public pass
pipeline, while an authored public voxel domain constrains every latent
footprint covering that voxel? Pairwise cardinal observations alone do not
retain a joint cuboid payload. Rotating only directional counts is therefore
not a substitute for rotating these payloads.

The implemented path is:

```text
independent source volumes -> literal transforms -> joint cuboid patterns
                                                       |
public XYZ domains -> all covering latent domains -> pattern pass
                                                       |
                                                public terrain pass
                                                       |
                                                semantic foliage pass
```

The ready-to-use owner contains the first two passes and lowers its public
domains before solving. The separate free bridge supports the third pass and
larger ordinary graph DAGs; it does not automatically discover and lower all
constraints authored elsewhere in such a DAG.

## Invariant and counterexamples

Let `q` be a public voxel, `o` a footprint offset, and `D(q)` its authored
allowed tokens. The covering latent anchor is `a = (q - o) mod extent`.
Before solving, retain at `a` only patterns `p` with `payload(p,o) in D(q)`.
Intersect this condition for **every** constrained voxel and footprint
offset. Alternatives inside one public domain are OR choices; different
observations are AND constraints.

Offsets are not deduplicated by wrapped anchor. On a one-cell axis, offsets
zero and two may address the same anchor but inspect different payload cells.
Pooling their supports accepts contradictions. An assigned empty public
domain is an immediate hard contradiction; clearing a domain removes that
condition and is not the same operation.

Likewise, a token absent at any footprint offset cannot occur anywhere in a
wrapped public output. This does not make the learned model malformed:
another subset of its patterns can still tile the requested extent. The
bridge explicitly excludes those impossible public values. Open source
provenance does not itself require open output topology.

An initial test sample `A,B,A,A` with depth-three footprints did **not** support
an alternating two-cell depth torus: it contained `ABA` but not `BAB`. The
corrected positive fixture is `A,B,A,B,A,A`. Increasing output size or changing
a seed cannot supply a missing footprint. The negative/positive distinction
is a constraint-model property, not a rendering or search-length setting.

## Independent checks executed

All three portable suites were compiled with checked FPC 3.2.2 and 3.3.1,
each for Win32 and Win64. The same Pascal sources also ran as pas2js output
in actual Edge using the included FPC HTTP/capture/check tools.

| Suite | Checks per native run | Checks in browser |
| --- | ---: | ---: |
| `wfc_pattern3d_test` | 181,988 | 182,192 |
| `wfc_pattern3d_text_test` | 61,392 | 61,401 |
| `wfc_pattern3d_passes_test` | 900 | 1,141 |

Additional browser checks exercise malformed JavaScript numeric/token inputs
that normal statically typed Pascal cannot construct. Counts are assertions,
not counts of independent research experiments.

The learner oracle scatters source coordinates forward using independently
enumerated signed permutations. It does not call the production inverse
transform helper. It compares ordered payloads, raw weights and every one of
the six compiled relation directions. Fixtures include rectangular sources,
open/wrapped extraction, larger-than-source wrapped footprints, stabilizers,
isolated corpora, and distinct 24/48 orbits of an eight-label chiral cube.
Deliberate cache and full-hash collisions must still compare actual payloads
and overlap slabs. Hash equality is never a semantic equivalence proof.

The pass suite checks a nonzero-depth `patterns -> terrain -> foliage` graph,
every XYZ footprint contribution, source/root identity, atomic preflight,
public masks and locks, all-axis aliasing, conflicting and cleared domains,
detached outputs, stale-result invalidation, commit-hook rejection and random
stream rollback. A public lock at `(3,1,3)` forces the checker phase for sixteen
seeds with zero local backtracks. The fixed composition signature is
`F7EBCEFF` for the fixture in that suite. Reentrant mutation/output operations
inside the commit hook are rejected before changing owner state.

The strict codec rejects malformed counts, syntax, Unicode and structural
claims. A roughly two-MiB token reused by 1,024 distinct patterns exercises
compact-text-to-key amplification: exact aggregate key length must reject
before materializing a multi-gigabyte derived key collection.

Existing 2D pattern-pass checks (41) and declarative runtime checks (62) also
pass on all four native variants. The installed-package consumer compiles
without `src` on its unit path and passes 31 checks, including the new model,
codec and publicly constrained owner. The three package inventories each
contain all 84 native source units.

## Costs and boundaries

The model interns overlapping slabs with hash routing followed by complete
equality checks. Its final relation matrix performs `6*P*P` class comparisons
and retains a dense matrix. Collision candidate scans, orbit validation,
literal extraction and exact string construction still cost work. This is
not a collision-independent worst-case linear constructor or a compressed
solver-domain result.

Graph-private keys retain exact token dictionaries and payload indices after
a non-authoritative routing hash. Both individual and aggregate ASCII key
lengths must fit `Integer`; actual available memory can be much lower. A
16-MiB encoded artifact limit does not bound expanded model, graph or solver
memory. Public inverse lowering allocates pattern masks only for affected
anchors, plus output-sized lookup arrays; dense public constraints can still
affect every anchor. No throughput or universal speedup claim was measured.

Low-level open projection expands all three dimensions by footprint minus
one. The pass adapter instead has one shared wrapped XYZ extent. Unlike-sized
pass layouts, chunk seams, asynchronous training, full declarative resources,
training/workspace persistence, generic artifact dispatch and a dedicated
interactive volume demo remain outside this milestone. Existing 2D formats
and replay versions are not repurposed. These checks do not demonstrate
unbounded scale, global aesthetic quality, physical validity, or learned
semantics inside individual token labels.

## Reproduction

The normal native build registers the three suites; browser discovery uses
their `*_test.lpr` names. For isolated native checks, create fresh unit/output
directories and compile each source with:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FU<new-units> -FE<new-bin> test/wfc_pattern3d_test.lpr
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FU<new-units> -FE<new-bin> test/wfc_pattern3d_text_test.lpr
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FU<new-units> -FE<new-bin> test/wfc_pattern3d_passes_test.lpr
```

Run each resulting executable and require zero failures. Use separate unit
directories for compiler versions, targets and pas2js. Browser verification
must execute in a browser, not merely compile JavaScript or inspect a static
HTML file. See [development tools](../development-tools.md) for the included
FPC server, browser harness and capture workflow. No new external runtime
dependency is required.
