# Multi-pass building

This dependency-free example runs one bounded `7 x 5 x 3` building through
four committed passes:

```text
footprint -> structure -> envelope/roof -> props
```

The footprint is a full 3D massing field. Structure consumes its public roles
to choose supported walls and windows, a rotated entrance, walkable interior
air, a reachable feature, lintel, and spanning roof modules. The envelope pass
projects those private voxel variants to facade, trim, and roof-finish
semantics. The prop pass can place a lamp or plant only on the feature cell
after checking all three earlier layers.

The same `building3d_demo.pas` source drives native FPC and pas2js/Node. Both
hosts perform independent semantic and voxel validation, same-seed replay,
renderer-neutral mesh extraction, and print only public roles and prototype
identities.

From the repository root, native FPC can build and run it with:

```powershell
New-Item -ItemType Directory -Force `
  build\examples\building3d\native\units, `
  build\examples\building3d\native\bin | Out-Null
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc `
  -Fuexamples\3D\02_MultiPassBuilding `
  -FUbuild\examples\building3d\native\units `
  -FEbuild\examples\building3d\native\bin `
  examples\3D\02_MultiPassBuilding\MultiPassBuilding.lpr
.\build\examples\building3d\native\bin\MultiPassBuilding.exe 0
```

Compile the Node host with pas2js and its matching RTL:

```bash
mkdir -p build/examples/building3d/pas2js/{units,bin}
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -Fuexamples/3D/02_MultiPassBuilding \
  -FUbuild/examples/building3d/pas2js/units \
  -FEbuild/examples/building3d/pas2js/bin \
  examples/3D/02_MultiPassBuilding/MultiPassBuildingNode.lpr
node build/examples/building3d/pas2js/bin/MultiPassBuildingNode.js 0
```

Omit the seed to use the documented default. The generated JavaScript and all
compiler output remain beneath the ignored `build/` directory.
