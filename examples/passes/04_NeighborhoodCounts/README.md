# Neighborhood Counts

A small three-pass workbench for hard neighborhood count constraints:
terrain supplies land/water, roads respect that terrain, and a fixed market
probe requires an inclusive range of cardinal road neighbors plus a maximum
number of water cells in its surrounding eight-cell ring.

The native FPC and pas2js browser hosts use the same
`neighborhood_count_demo.pas` implementation. Captured output is independently
recounted with a separate stencil sampler. Everything uses repository units,
the applicable compiler RTL, and standard browser APIs under the MIT license.

## Native

The normal `build.ps1` or `build.sh` gate compiles and self-tests the host.
Run the resulting executable (add `.exe` on Windows):

```bash
build/native/bin/NeighborhoodCounts --selftest
build/native/bin/NeighborhoodCounts two
build/native/bin/NeighborhoodCounts repair --repair
```

For an isolated native build:

```bash
mkdir -p build/counts/native/units build/counts/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc \
  -Fuexamples/passes/04_NeighborhoodCounts \
  -FUbuild/counts/native/units -FEbuild/counts/native/bin \
  examples/passes/04_NeighborhoodCounts/NeighborhoodCounts.lpr
build/counts/native/bin/NeighborhoodCounts --selftest
```

Presets are `two`, `lower`, `upper`, `flood`, `alias`, and `repair`. The
deliberately impossible lower/upper/flood probes report a contradiction and
exit with status 2; they do not expose generated layers. Bad arguments exit
with status 1. The successful default output key is:

```text
LLLLLLLLL/.R.R...../....M....
```

Each slash separates terrain, roads, and market in row-major order. `L` is
land, `W` water, `R` road, `M` market, and `.` an empty roads/market cell.

## Browser

From PowerShell:

```powershell
.\build-browser-counts.ps1 -Compiler 'C:/path/to/pas2js.exe'
.\build\native\bin\wfc_serve.exe --root build/browser/counts/www --port 4176
```

From a shell:

```bash
PAS2JS=/opt/pas2js/bin/pas2js bash ./build-browser-counts.sh
build/native/bin/wfc_serve --root build/browser/counts/www --port 4176
```

Build the included FPC server with the normal native gate first. Open
`http://127.0.0.1:4176/`; stop the foreground server with Ctrl+C. The staging
scripts place all generated code and static assets under
`build/browser/counts/www`, with no external services or asset downloads.

Use the presets and input controls to compare ordinary solving with bounded
repair. A pending input edit invalidates the previous displayed output.
Repair may reopen road assignments but preserves the terrain outside its
explicit selective horizon. It cannot override caller-fixed road or terrain
locks, and budget exhaustion is not proof that no solution exists.

Append `?selftest=1` for the event-driven conformance route. Its final body
must contain `data-state="solved"`, `data-self-test="passed"`, the default
`data-output-key`, and `passed` values for `data-lower`, `data-upper`,
`data-alias`, `data-repair`, `data-flood`, and `data-invalidation`. The hosted
gate executes the compiled Pascal in a browser and verifies these markers
using the project-owned FPC checker.

## Explicit count semantics

`gpcmMatchingTerms` counts matching declared offset predicates;
`gpcmDistinctCells` counts each matching resolved provider cell once. In the
wrapped two-cell alias preset, west and east read the same road: that is two
matching terms but one distinct cell. A caller must supply the mode explicitly.
Unresolved bounded offsets and empty provider entries are nonmatches. Exact
duplicate offsets merge their accepted alternatives before validating bounds.

The demo bounds its inspection grids to 1..9 cells per axis and road stencils
to four terms. Those are demo limits, not a core neighborhood-size policy.
This is a local market probe, not a global road quota, connectivity guarantee,
or general city planner. See the [full count contract](../../../../docs/pass-counts.md)
for canonicalization, staging, portable recipe encoding, and failure semantics.
