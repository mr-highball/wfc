# voxel and structure 3D foundation

The voxel foundation turns the core six-direction graph into a reusable,
renderer-neutral 3D model. It is portable Pascal for native FPC and pas2js;
generation, capture, validation, signatures, and surface extraction require no
engine, scene format, or media library.

This is a new standard path beside the historical Simple Building Kit. The
legacy experiment remains useful project history, but its depth-one rules,
viewer shell, and unproven binary asset are not inputs to this model.

## model contract

A kit is an immutable ordered collection of module prototypes plus an explicit
unordered socket-compatibility relation. Each prototype declares:

- a stable identifier and renderer-facing material token;
- a positive integer relative weight;
- one socket token for north, east, south, west, up, and down;
- the allowed quarter-turn yaw rotations;
- occupancy, support, walkability, entrance, and required-reachability flags;
  and
- the faces through which a walkable cell connects.

Constructor inputs are deeply copied. Empty or duplicate identity tokens,
invalid weights, empty rotation policies, malformed socket pairs, and
contradictory flag combinations are rejected before a kit exists. Version 1
uses a deliberately small portable ASCII identity grammar so kit fingerprints
and graph keys cannot depend on a host code page or UTF-16 implementation.

Prototype order is semantic. Variants expand in prototype order and then
rotation-enum order, providing one stable value and weight order to the core
solver. Equivalent kits therefore compile to equivalent graph inputs on native
FPC and pas2js.

## rotation

Yaw rotates around the positive Z axis in quarter turns. A positive
quarter-turn maps north to east, east to south, south to west, and west to
north. Up and down never change. Sockets and walk openings rotate together;
prototype identity, material, weight, and flags do not.

Rotation is structural data rather than a suffix interpreted by a renderer.
The kit exposes the prototype and rotation behind each expanded variant, so a
host never needs to parse a graph value to recover orientation.

## sockets and support

Two adjacent variants are locally compatible only when the source face socket
and the opposite target face socket occur in the kit's explicit symmetric
compatibility relation. A token is not assumed to match itself unless that
pair is declared. This permits exact neutral matches as well as differently
named plug/socket or interior/exterior relationships without embedding a
naming convention in the core.

Vertical structural support is an additional exact condition. Above Z=0, a
variant marked as requiring support accepts a lower neighbor only when that
neighbor provides support. The inverse up relation is compiled from the same
condition, so graph adjacency remains symmetric. Z=0 is an explicit implicit
ground plane for this local rule.

Version 1 support is immediate and local. It does not claim cumulative load,
span analysis, stress simulation, or material strength. Higher-level building
libraries may add state-expanded load rules or repair passes, and independent
validators must state which global invariants they check.

## graph adapter

Applying a kit requires an already shaped graph whose active pass has no
registered values. The adapter registers every expanded variant, preserves its
integer weight, and emits exact rules for all six directions. A face with no
compatible target is represented by explicit deny-all support rather than a
legacy wildcard.

Adapter graph keys are private, deterministic identities qualified by kit
semantics and variant index. They must not contain object addresses, random
salts, clocks, or process-global counters. The adapter retains the mapping
needed to constrain cells and capture public scenes without asking callers to
parse those keys.

Capture resolves the internal canonical variant ordinal in bounded key time,
then verifies the complete expected key. It neither trusts a partial prefix nor
linearly scans every variant for every scene cell.

The core treats a missing bounded neighbor as no adjacency arc. Domain-specific
libraries therefore own boundary policy—for example, which modules may touch
the ground, exterior air, or the top of a building. Wrapped topology remains a
caller choice and is not silently disabled by the voxel layer.

## scenes, validation, and meshes

A captured scene is immutable and records dimensions, kit identity, and one
variant index per cell in X-fastest, then Y, then Z order. Capture rejects an
empty cell, unknown private key, mismatched kit application, or incompatible
shape. Its portable signature covers version identifiers, kit identity,
dimensions, and ordered variants.

Independent validation recomputes coordinates instead of trusting mutable
neighbor pointers. It checks all in-bounds socket pairs, local vertical
support, entrance placement, and requested walkable reachability. Global
connectivity is intentionally a validator concern: local sockets can preserve
doorway agreement, but they cannot alone prove that an arbitrary building has
one connected circulation component.

The renderer-neutral mesh is a deterministic list of exposed unit-cube quads.
Vertices and normals use integer lattice coordinates, solid/solid interior
faces are culled, and each face retains its cell, direction, prototype,
rotation, and material identity. The canonical layer contains no floating
point, camera, shader, canvas, engine, or file-format type.

## pass composition

The intended building pipeline is:

```text
footprint -> structure -> envelope/roof -> props
```

Footprint values may classify buildable columns, entrances, circulation, and
voids. Structure modules consume those classifications while satisfying their
six local faces and support below. Envelope and roof passes may then use exact
same-cell or signed-offset reads from committed structure. Props consume the
validated result without making a renderer part of generation.

This separation is deliberate: the general voxel kit knows nothing about
houses, while the building library can use architectural semantics without
placing them in `TGraph`.

## version-one scope

Foundation version 1 is bounded to axis-aligned unit modules, quarter-turn yaw,
finite explicit socket pairs, immediate support, discrete walk openings,
immutable scenes, local/global validation, and exposed cube faces. It does not
perform arbitrary-angle transforms, multi-cell module packing, continuous
geometry booleans, physics, cumulative load analysis, automatic global
connectivity repair, or asset import.

Those omissions are explicit research and ecosystem layers, not behavior
silently delegated to a third-party dependency.
