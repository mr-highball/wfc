# 3D examples

The reusable standard substrate now lives in `src/wfc_voxel3d.pas`,
`src/wfc_voxel3d_validate.pas`, `src/wfc_voxel3d_mesh.pas`, and the checked
`src/wfc_voxel3d_passes.pas` bridge. It models
rotation-aware six-face sockets, local vertical support, immutable scenes,
entrance/reachability validation, and renderer-neutral integer cube surfaces
without an engine dependency. Its focused conformance source is
`test/wfc_voxel3d_test.lpr`; see the [voxel foundation contract](../../docs/voxel3d.md).

`02_MultiPassBuilding` is the standard depth-greater-than-one path. One shared
Pascal unit drives thin native and pas2js/Node hosts over the reusable
`wfc_building3d` owner and independent validator. Its `7 x 5 x 3` fixture
solves footprint -> structure -> envelope/roof -> props, proves support and a
reachable rotated entrance, captures a structure scene and integer mesh, and
prints the same public signature and layer slices on both targets without an
engine or third-party runtime dependency. See the
[Building 3D contract](../../docs/building3d.md) and the
[example guide](02_MultiPassBuilding/README.md).

`01_SimpleBuildingKit` contains two separate starting points:

- `tester.lpr` is a dependency-free native console using the building-kit
  rules, but it currently runs with depth one and renders only a textual slice;
- `castle-demo` is a Castle Game Engine viewer shell that does not yet invoke
  WFC or turn generated values into geometry.

Neither historical entry is presented as the standard 3D demonstration.
Connected graphical viewers and legacy asset provenance remain roadmap work;
the canonical generator, validation, mesh data, and portable textual host do
not depend on them. Build commands and exact dependency status are in the
[examples index](../README.md).
