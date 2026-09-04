# 3D examples

The reusable standard substrate now lives in `src/wfc_voxel3d.pas`,
`src/wfc_voxel3d_validate.pas`, and `src/wfc_voxel3d_mesh.pas`. It models
rotation-aware six-face sockets, local vertical support, immutable scenes,
entrance/reachability validation, and renderer-neutral integer cube surfaces
without an engine dependency. Its focused conformance source is
`test/wfc_voxel3d_test.lpr`; see the [voxel foundation contract](../../docs/voxel3d.md).

The standard depth-greater-than-one building wrapper and native/pas2js viewers
will be added as a new example rather than silently redefining the historical
prototype below.

`01_SimpleBuildingKit` contains two separate starting points:

- `tester.lpr` is a dependency-free native console using the building-kit
  rules, but it currently runs with depth one and renders only a textual slice;
- `castle-demo` is a Castle Game Engine viewer shell that does not yet invoke
  WFC or turn generated values into geometry.

Neither is presented as a completed 3D demonstration yet. Vertical sockets,
support rules, depth-greater-than-one fixtures, connected geometry, pas2js
viewing, and asset provenance remain roadmap work. Build commands and exact
dependency status are in the [examples index](../README.md).
