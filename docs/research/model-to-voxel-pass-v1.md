# Model-to-voxel pass experiment v1

This project experiment connects an observed scalar volume to authored
module and placement semantics. It makes no priority or literature-novelty
claim.

## Question

Can a learned first pass remain an independent vocabulary while later
authored constraints select multiple compatible structural interpretations,
place objects at offsets, and negotiate earlier choices when necessary?

## Construction

The [terrace study](../learned-terraces3d.md) learns two independent volumes.
It maps rock to stone or basalt, and soil to grass or flowerbed. The structural
kit separately enforces vertical load/open sockets and support. A foliage pass
requires empty space at the current cell and a particular surface below.

The new bridge resolves a source model on a stable pass index and translates
public source tokens to private target variant requirements only after
complete preflight. It reuses existing pass dependency edges and solver
transactions. There is no second solver or external engine.

## Falsifiable checks

1. Exact source registry, weight, adjacency, denial, boundary, or pass-binding
   tampering must reject configuration without adding dependencies.
2. Invalid/duplicate/unknown maps and invalid spatial terms must not partly
   install a map.
3. The same seed must reproduce the exact native/browser public result.
4. A foliage-only repair must keep earlier terrain and structural assignments.
5. A flower request over an initially unsuitable surface must either fail
   within the requested scope/budget or reopen an authorized earlier choice.
   It must never silently move an ancestor outside selective scope.
6. Learned relations, token projection, support, and foliage offsets must
   pass an independent coordinate-based check before commit.
7. False/exception from final application policy must publish no scene and
   leave a retry identical to an idle control's next attempt, including when
   a valid user edit remains dirty.
8. Native SVG and browser display must agree on the same versioned geometric
   witness and expose no private variant-key strings.

The conformance suites and real-page regression implement these checks.
Seed-zero 6 × 5 × 5 witnesses are scene `1:6D695B99:2D23CF62` and view
`C3D25917`. Different test seeds produce different assignments; this is
not a claim of broad aesthetic diversity.

## Boundaries and follow-up

Cardinal observations capture neighbor support, not joint 3D footprints,
global style, physics, or causality. D4 augmentation preserves gravity but
does not rotate token payload semantics. The fixed authored kit makes the
experiment reproducible, not universally applicable.

Negotiation excludes completed provider assignments; a large provider space
can require many such exclusions. This experiment does not establish a
complexity improvement or minimal-change repair guarantee. Full-volume object
storage and whole-result SVG also do not establish chunked scalability.

Useful next experiments include learned terrain with routed circulation,
local attachment clusters, orientation-bearing token transforms, chunked
boundary contracts, and authored voxel-consumer resources in portable recipes.
Each needs its own independent semantic validator and failure evidence.
