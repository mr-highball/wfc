# Building and testing

The dependency-free build covers the core and specialized 2D units, both
conformance suites, and seeded smoke runs of the text-rendered tiled world and
multi-pass 2D ecosystem demo. It does not initialize the optional music
submodule or build the unfinished Castle Game Engine viewer. The browser world
has its own dependency-free pas2js entry point described below.

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

Both scripts rebuild with assertions and range, overflow, and I/O checks, run
`wfc_test` and `wfc_world2d_test`, then compile and smoke-test both portable
examples with seed `0` and the multi-pass world with its default seed as well.
A compiler error, failed check, or example failure produces a nonzero exit
code. Compiler units and binaries are written beneath `build/native/`; running
the gate does not modify tracked source files.

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

The conformance sources can be compiled for Node.js when pas2js and its
matching RTL are configured:

```bash
mkdir -p build/pas2js/units build/pas2js
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/units -FEbuild/pas2js test/wfc_test.lpr
node build/pas2js/wfc_test.js

mkdir -p build/pas2js/world-units build/pas2js/world
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/world-units -FEbuild/pas2js/world \
  test/wfc_world2d_test.lpr
node build/pas2js/world/wfc_world2d_test.js
```

The portable multi-pass host uses the same target:

```bash
mkdir -p build/pas2js/world-example-units build/pas2js/world-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -Fuexamples/2D/common \
  -FUbuild/pas2js/world-example-units -FEbuild/pas2js/world-example \
  examples/2D/01_MultiPassWorld/MultiPassWorld.lpr
node build/pas2js/world-example/MultiPassWorld.js 0
```

A standalone `pas2js` executable is not enough when its RTL unit paths are
missing. Use the compiler and RTL from the same installation.

## pas2js browser world

The browser entry points compile the browser-target Pascal program and stage
its HTML, CSS, and generated `BrowserWorld.js` together:

```powershell
.\build-browser.ps1 -Compiler 'C:\path\to\pas2js.exe'
```

```bash
PAS2JS=/opt/pas2js/bin/pas2js bash ./build-browser.sh
```

Omit the compiler override when `pas2js` is on `PATH`. Both scripts keep units
in `build/browser/world2d/units` and the complete static site in
`build/browser/world2d/www`. The shell entry point converts repository and
output paths for Cygwin, MSYS, and Git Bash before invoking a Windows compiler.
They concatenate the matching pas2js browser RTL into `BrowserWorld.js`, so the
staged page does not depend on an unstaged runtime script.

Serve the staged site from the repository root, for example:

```text
python -m http.server 8080 --directory build/browser/world2d/www
```

Open `http://localhost:8080/` for the interactive viewer. The deterministic
browser fixture at `http://localhost:8080/?selftest=1` succeeds only when the
body finishes with `data-state="solved"`, `data-self-test="passed"`, and the
seed-zero signature
`data-signature="1:5B0DD75D:08022AF1:A40D0955"`. Generated JavaScript and
compiler units remain under the ignored `build/` tree; source distributions do
not commit them.

The hosted pas2js gate uses exact official upstream pas2js and FPC-source
revisions, verifies both source-archive SHA-256 digests, and caches the resulting
3.3.1 toolchain. It runs both conformance suites, the tiled-world seed-zero
smoke test, and the multi-pass world with both seed zero and its default seed
under Node.js 22.23.2. It then builds the browser target, serves the staged
site, and checks its exact body-state contract in headless Chrome. A pinned
development compiler is used because the official 3.2.0 binary release cannot
resolve the suite's portable overloaded plain-procedure callback call.

## Continuous integration

The hosted workflow runs the checked native gate with FPC 3.2.2 on Linux,
macOS, and Windows. The Linux lane also builds the FPM and Lazarus packages,
runs the core Lazarus project, and verifies that generation leaves the checkout
clean. A separate Linux lane runs the complete core and 2D pas2js/Node.js gate,
plus the real browser self-test in headless Chrome, while a canary runs against
the current official FPC development image and records the image digest and
compiler revision in the job log. Submodules are deliberately disabled for
every gate.
