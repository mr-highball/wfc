# 3D examples

The reusable standard substrate lives in `src/wfc_voxel3d.pas`,
`src/wfc_voxel3d_validate.pas`, `src/wfc_voxel3d_mesh.pas`, and the checked
`src/wfc_voxel3d_passes.pas` bridge. It models rotation-aware six-face sockets,
local vertical support, immutable scenes, entrance/reachability validation,
and renderer-neutral integer cube surfaces. `wfc_voxel3d_isometric` and
`wfc_voxel3d_svg` add a project-owned fixed-integer command projector,
hit-testing, portable view signatures, and canonical SVG without an engine
dependency. See the [voxel foundation contract](../../docs/voxel3d.md).

`02_MultiPassBuilding` is the standard depth-greater-than-one path. One shared
Pascal unit drives a thin native host over the reusable
`wfc_building3d` owner and independent validator. Its `7 x 5 x 3` fixture
solves footprint -> structure -> envelope/roof -> props, proves support and a
reachable rotated entrance, captures a structure scene and integer mesh, and
prints the same public signature and layer slices on both targets without an
engine or third-party runtime dependency. See the
[Building 3D contract](../../docs/building3d.md) and the
[example guide](02_MultiPassBuilding/README.md).

`03_BrowserBuilding` is the standard graphical vertical slice over that same
showcase. `wfc_building3d_view` converts the four public passes into immutable
footprint, structure, envelope/roof, or complete face commands with full
same-cell lineage. A native FPC executable writes deterministic SVG, while a
pas2js browser workbench renders the identical command model through Canvas2D
and adds seeds, selective regeneration, view modes, four-yaw rotation, Z
clipping, picking, and a lineage inspector. Seed zero is pinned to pipeline
`1:F1EF0EB6`, complete view `AC7290C0`, and `140` faces. See the
[graphical example guide](03_BrowserBuilding/README.md).

`01_SimpleBuildingKit` contains two separate starting points:

- `tester.lpr` is a dependency-free native console using the building-kit
  rules, but it currently runs with depth one and renders only a textual slice;
- `castle-demo` is a Castle Game Engine viewer shell that does not yet invoke
  WFC or turn generated values into geometry.

Neither historical entry is presented as the standard 3D demonstration.
The standard graphical path is now generated geometry and project-owned
materials; it does not depend on the shell or its unproven legacy asset.
Interactive native-window adapters remain optional edge integrations rather
than a core dependency. Build commands and exact dependency status are in the
[examples index](../README.md).
