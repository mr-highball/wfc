# Building and testing

The dependency-free build covers the core unit, conformance suite, and a
seeded smoke run of the text-rendered tiled-world example. It does not
initialize the optional music submodule or build the unfinished graphical
viewer.

FPC 3.2.2 is the supported stable compiler. The current FPC development
compiler is also exercised as a compatibility canary.

## One-command native gate

From the repository root, use the entry point for your shell:

```powershell
.\build.ps1
```

```bash
./build.sh
```

Both scripts rebuild with assertions and range, overflow, and I/O checks,
run the conformance executable, then compile and smoke-test the dependency-free
tiled-world example with seed `0`. A compiler error, failed check, or example
failure produces a nonzero exit code. Compiler units and binaries are written
beneath `build/native/`; running the gate does not modify tracked source files.

Set `FPC` to select another compiler. Additional compiler arguments may be
passed explicitly:

```powershell
$env:FPC = 'C:\FPC\3.2.2\bin\i386-win32\fpc.exe'
.\build.ps1 -CompilerOptions @('-O2')
```

```bash
FPC=/opt/fpc/bin/fpc ./build.sh -O2
```

## FPM package

`fpmake.pp` describes the runtime `wfc` package and its `rtl-generics`
dependency. Bootstrap FPMake with the compiler, then build the package:

```powershell
New-Item -ItemType Directory -Force `
  build\fpm\bootstrap\units, build\fpm\bootstrap\bin |
  Out-Null
fpc -B -Mobjfpc -Sa -Cr -Co -Ci `
  -FUbuild\fpm\bootstrap\units -FEbuild\fpm\bootstrap\bin fpmake.pp
.\build\fpm\bootstrap\bin\fpmake.exe build
```

```bash
mkdir -p build/fpm/bootstrap/units build/fpm/bootstrap/bin
fpc -B -Mobjfpc -Sa -Cr -Co -Ci \
  -FUbuild/fpm/bootstrap/units -FEbuild/fpm/bootstrap/bin fpmake.pp
./build/fpm/bootstrap/bin/fpmake build
```

Some self-contained compiler layouts do not expose installed package metadata
through their default configuration. If FPMake cannot resolve `rtl` or
`rtl-generics`, append `--globalunitdir=<FPC installation prefix>` and, when
needed, `--compiler=<path to fpc>` to the `fpmake build` command.

FPMake writes a target-specific `wfc-*.fpm` metadata file at the repository
root. That generated file is ignored; compiled units are written beneath
`build/fpm/units/`.

## Lazarus package and project

`wfc.lpk` is a runtime-only package. It does not depend on the LCL. Build the
package and the conformance project without allowing Lazarus to rewrite their
metadata:

```text
lazbuild -B --no-write-project wfc.lpk
lazbuild -B --no-write-project test/wfc_test.lpi
```

The package output is written to `build/lazarus/package/<target>`. The test
executable is written to `build/lazarus/test/bin`, with its units kept in the
adjacent `units/<target>` directory. Run `wfc_test.exe` on Windows or
`wfc_test` on other native targets.

## pas2js and Node.js

The same conformance source can be compiled for Node.js when pas2js and its
matching RTL are configured:

```bash
mkdir -p build/pas2js/units build/pas2js
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/units -FEbuild/pas2js test/wfc_test.lpr
node build/pas2js/wfc_test.js
```

A standalone `pas2js` executable is not enough when its RTL unit paths are
missing. Use the compiler and RTL from the same installation.

The hosted pas2js gate uses exact official upstream pas2js and FPC-source
revisions, verifies both source-archive SHA-256 digests, and caches the resulting
3.3.1 toolchain. It runs the conformance suite and the seeded tiled-world smoke
test with Node.js 22.23.2. A pinned development compiler is used because the
official 3.2.0 binary release cannot resolve the suite's portable overloaded
plain-procedure callback call.

## Continuous integration

The hosted workflow runs the checked native gate with FPC 3.2.2 on Linux,
macOS, and Windows. The Linux lane also builds the FPM and Lazarus packages,
runs the Lazarus project, and verifies that generation leaves the checkout
clean. A separate Linux lane runs the complete pas2js/Node.js gate, while a
canary runs against the current official FPC development image and records the
image digest and compiler revision in the job log. Submodules are deliberately
disabled for every gate.
