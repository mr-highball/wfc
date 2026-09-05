# Volume Observation Orbits v1

Status: implemented project method with executable conformance checks. This
note makes no claim of literature priority. Its claim is deliberately narrow:
exact aggregation of transformed six-direction scalar-token observations.

## Representation and derivation

Let `C[d,a,b]` be the raw count of token `a` observing token `b` in direction
`d`, and `W[a]` its occurrence count. A signed-axis transform changes only the
direction of an observed edge; it leaves both token payloads unchanged.
Because coordinate transformation is bijective, this holds for rectangular
volumes, missing open-boundary edges, wrapped seams, and singleton axes.

For a finite transform group `G`, transformed observations are therefore:

```text
W'[a] = |G| * W[a]
C'[e,a,b] = sum over g in G of C[inverse(g)(e),a,b]
```

Horizontal D4 acts transitively on four horizontal directions with stabilizer
size two, while fixing each vertical direction. Thus:

```text
C'[each horizontal,a,b] = 2 * sum(horizontal C[d,a,b])
C'[up,a,b] = 8 * C[up,a,b]
C'[down,a,b] = 8 * C[down,a,b]
```

The 24 proper cube rotations act transitively on six directions, with
stabilizer size four. The full 48 signed permutations have stabilizer size
eight. Their respective formulas are four or eight times the sum of the six
original direction counts, assigned to each destination direction.

`wfc_learn3d` indexes the original corpus once, observes its original edges,
then applies these formulas with checked integer addition/multiplication.
It never allocates 8, 24, or 48 transformed copies. Vocabulary identity stays
first-seen in the original ordered samples, independent of internal hash-table
bucket order. Compared with materialized augmentation, the observation phase
avoids a transform-count multiplier over source cells; both paths still pay
the representation's dense `6*V*V` storage and validation costs. This is an
algorithmic comparison, not a measured wall-clock speed claim.

## An important representational limitation

For these scalar radius-one statistics, cube48 is exactly twice cube24. Both
produce identical allowed relations and relative value weights. They cannot
distinguish reflected chirality. More transform enumeration cannot recover
information absent from the model. Chirality needs a richer footprint,
orientation-aware token identities, or separate authored constraints.

Likewise, D4 preserves the vertical direction but does not infer gravity,
support, enclosure, or connectedness. Those require domain constraints and
independent semantic validation.

## Reproducible checks

Run the normal native `build.ps1` or `build.sh`, then the maintained pas2js
conformance build and the included FPC-hosted browser gate. The focused source
is [wfc_learn3d_test.lpr](../../test/wfc_learn3d_test.lpr).

Its oracle literally materializes transformed coordinates using axis
permutations and endpoint reflections, then independently counts neighbors.
It does not call the production orbit aggregator. Cases cover open and wrapped
2×3×4 volumes, every singleton-axis shape, unique and repeated labels, all
four policies, ordered heterogeneous corpora, separate-model merging,
non-ASCII identity, asymmetric graph direction mapping, and deterministic
solve/reset replay.

For a 2×3×4 volume with one distinct label at each cell, the summed relation
counts provide a compact independent checksum of geometry:

| Policy | Open N/E/S/W/U/D totals | Wrapped total per direction |
| --- | --- | --- |
| none | 16 / 12 / 16 / 12 / 18 / 18 | 24 |
| d4 | 112 / 112 / 112 / 112 / 144 / 144 | 192 |
| cube24 | 368 / 368 / 368 / 368 / 368 / 368 | 576 |
| cube48 | 736 / 736 / 736 / 736 / 736 / 736 | 1,152 |

Full token-pair matrices are compared, not only these totals. Separate suites
exercise strict model/training codecs, depth-sensitive provenance, workspace
locks/domains on nonzero Z, stale-output invalidation, artifact replay, and
aggregate six-plane resource accounting. The Studio real-page self-test adds
DOM slice/coordinate checks and contradiction/recovery before restoring its
original baseline.

## Next experiments

Useful extensions are explicit token-transform maps, overlapping 3D payloads
that retain chirality, and seam summaries for incremental volume solving.
Each needs its own representation, failure cases, and native/browser oracle;
none is implied by the present six-face learner.
