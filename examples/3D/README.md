# 3D examples

`01_SimpleBuildingKit` contains two separate starting points:

- `tester.lpr` is a dependency-free native console using the building-kit
  rules, but it currently runs with depth one and renders only a textual slice;
- `castle-demo` is a Castle Game Engine viewer shell that does not yet invoke
  WFC or turn generated values into geometry.

Neither is presented as a completed 3D demonstration yet. Vertical sockets,
support rules, depth-greater-than-one fixtures, connected geometry, pas2js
viewing, and asset provenance remain roadmap work. Build commands and exact
dependency status are in the [examples index](../README.md).
