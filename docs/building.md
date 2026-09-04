# Building and testing

The dependency-free build covers the core, specialized 2D and settlement,
radius-one model-learning, and overlapping-pattern units; all five conformance
suites; and seeded smoke runs of the text-rendered tiled world, multi-pass 2D
ecosystem, selective settlement, learned-tiles, learned-corpus, and
overlapping-pattern demos. It does not
initialize the optional music submodule or build the unfinished Castle Game
Engine viewer. The browser world has its own
dependency-free pas2js entry point described below.

FPC 3.2.2 is the supported stable compiler. The current FPC development
compiler is also exercised as a compatibility canary.

## Dependency boundary

Runtime units may use repository units and the applicable standard FPC/pas2js
RTL. When a needed capability can reasonably be implemented and maintained in
portable Pascal, it is project-owned rather than added as a third-party runtime
dependency. Optional tools may build, test, profile, render, convert, or inspect
project artifacts, but tool-specific units and types must not enter core/runtime
`uses` clauses or public APIs. Canonical artifacts, validation, generation, and
replay remain usable without those tools.

## One-command native gate

From the repository root, use the entry point for your shell:

```powershell
.\build.ps1
```

```bash
./build.sh
```

Both scripts rebuild with assertions and range, overflow, and I/O checks, run
`wfc_test`, `wfc_world2d_test`, `wfc_world2d_settlement_test`,
`wfc_learn_test`, and `wfc_pattern2d_test`, then compile and smoke-test all six
portable console examples with seed `0`; the multi-pass and selective-settlement
worlds also run with their default seeds.
A compiler error, failed check,
or example failure produces a nonzero exit code. Compiler units and binaries
are written beneath `build/native/`; running the gate does not modify tracked
source files.

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

`fpmake.pp` describes the runtime `wfc` package. Its only declared dependency,
`rtl-generics`, is part of the standard FPC distribution; no third-party
runtime library is required. Bootstrap FPMake with the compiler, then build
the package:

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

mkdir -p build/pas2js/settlement-units build/pas2js/settlement
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/settlement-units -FEbuild/pas2js/settlement \
  test/wfc_world2d_settlement_test.lpr
node build/pas2js/settlement/wfc_world2d_settlement_test.js

mkdir -p build/pas2js/learning-units build/pas2js/learning
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/learning-units -FEbuild/pas2js/learning \
  test/wfc_learn_test.lpr
node build/pas2js/learning/wfc_learn_test.js

mkdir -p build/pas2js/pattern-units build/pas2js/pattern
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/pattern-units -FEbuild/pas2js/pattern \
  test/wfc_pattern2d_test.lpr
node build/pas2js/pattern/wfc_pattern2d_test.js
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

The dependency-DAG settlement host is portable in the same way:

```bash
mkdir -p build/pas2js/settlement-example-units build/pas2js/settlement-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -Fuexamples/2D/common \
  -FUbuild/pas2js/settlement-example-units \
  -FEbuild/pas2js/settlement-example \
  examples/2D/03_SelectiveSettlement/SelectiveSettlement.lpr
node build/pas2js/settlement-example/SelectiveSettlement.js 0
```

The learned-tiles training and generation host is portable in the same way:

```bash
mkdir -p build/pas2js/learning-example-units build/pas2js/learning-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/learning-example-units -FEbuild/pas2js/learning-example \
  examples/learning/01_LearnTiles/LearnTiles.lpr
node build/pas2js/learning-example/LearnTiles.js 0
```

The heterogeneous corpus host exercises canonical `wfcm=2` on Node.js:

```bash
mkdir -p build/pas2js/corpus-example-units build/pas2js/corpus-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/corpus-example-units -FEbuild/pas2js/corpus-example \
  examples/learning/02_LearnCorpus/LearnCorpus.lpr
node build/pas2js/corpus-example/LearnCorpus.js 0
```

The overlapping-pattern host exercises canonical `wfcp=1`, latent capture,
and independently checked projection:

```bash
mkdir -p build/pas2js/pattern-example-units build/pas2js/pattern-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/pattern-example-units -FEbuild/pas2js/pattern-example \
  examples/learning/03_LearnPatterns/LearnPatterns.lpr
node build/pas2js/pattern-example/LearnPatterns.js 0
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
3.3.1 toolchain. It runs all five conformance suites; the tiled-world,
learned-tiles, learned-corpus, and overlapping-pattern seed-zero smoke tests;
and the multi-pass and selective-settlement worlds with both seed zero and
their default seeds under Node.js 22.23.2. It then
builds the browser target, serves the staged site, and checks its exact
body-state contract in headless Chrome. A pinned development compiler is used
because the official 3.2.0 binary release cannot resolve the suite's portable
overloaded plain-procedure callback call.

## Continuous integration

The hosted workflow runs the checked native gate with FPC 3.2.2 on Linux,
macOS, and Windows. The Linux lane also builds the FPM and Lazarus packages,
runs the core Lazarus project, and verifies that generation leaves the checkout
clean. A separate Linux lane runs the complete core, 2D, and learning
pas2js/Node.js gate, plus the real browser self-test in headless Chrome, while
a canary runs against the current official FPC development image and records
the image digest and compiler revision in the job log. Submodules are
deliberately disabled for every gate.
