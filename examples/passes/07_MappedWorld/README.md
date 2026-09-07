# Mapped World

A mixed-resolution terrain → foliage → housing workbench, shared by a native
FPC command-line program and a pas2js browser application. Every layer occupies
the same integer world, but has its own cell size and origin:

| Layer | Cells | Origin | Cell pitch |
| --- | --- | --- | --- |
| terrain | 8 × 6 × 1 | 0,0,0 | 4,4,1 |
| foliage | 32 × 24 × 1 | 0,0,0 | 1,1,1 |
| housing | 3 × 2 × 1 | 4,4,0 | 8,8,1 |

These are fixed showcase dimensions, not a core graph-size cap. The workbench
edits constraints, weights and sampling regions; it does not resize these grids.
The shared owner, independent validator and renderer are project-owned Pascal.
The browser uses ordinary HTML/CSS and the included native FPC server; no
external packages, services, fonts or asset downloads are needed. Everything
remains under the repository's MIT license.

## What to try

The `interior` preset is a deliberately disclosed study: all 48 terrain cells
are zoned land; 767 foliage cells are zoned clear; `(7,7)` permits either tree
or clear. There are initially no caller locks, and every housing site is vacant.
WFC generates the blocker rather than the host writing a convenient tree.
Seed `3` reproduces the documented interior blocker: the house at `(0,0)` spans
world `[4,12) × [4,12)`, its lower corner `(4,4)` is clear, but `(7,7)` is not.

Try three distinct questions:

1. Point sampling can accept that house because it reads only the clear corner.
   The independent physical check still reports an unsafe footprint.
2. Full-cell sampling detects the interior tree. Requiring the house and
   authorizing only housing repair honestly fails; negotiated mode does not
   silently authorize changes to foliage.
3. Explicitly authorize foliage and its housing descendants. Negotiation can
   regenerate the provider and make the requested house possible while leaving
   terrain alone. Every actual scope and search counter appears in the report.

The `sandbox` preset instead starts with unrestricted terrain and foliage
domains, optional housing and editable weights. Paint domains or caller locks, make multiple
housing demands and choose the repair scope. It does not search for a favorable
seed, hide an extra retry, clear blockers outside the solver, or guarantee that
your demands are feasible within the selected search budgets.

## Native build and walkthrough

From the repository root, an isolated checked build on PowerShell is:

```powershell
New-Item -ItemType Directory -Force build/mapped/native/units,build/mapped/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Futools `
  -Fuexamples/passes/07_MappedWorld `
  -FUbuild/mapped/native/units -FEbuild/mapped/native/bin `
  examples/passes/07_MappedWorld/MappedWorld.lpr
.\build\mapped\native\bin\MappedWorld.exe --selftest
```

On a Unix shell:

```bash
mkdir -p build/mapped/native/units build/mapped/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Futools \
  -Fuexamples/passes/07_MappedWorld \
  -FUbuild/mapped/native/units -FEbuild/mapped/native/bin \
  examples/passes/07_MappedWorld/MappedWorld.lpr
build/mapped/native/bin/MappedWorld --selftest
```

Run the resulting executable with these arguments (add `.exe` on Windows).
Choose genuinely new output paths:

```text
MappedWorld --preset interior --seed 3
MappedWorld --demand 0,0=house --repair housing
MappedWorld --demand 0,0=house --repair foliage --trace --svg repaired.svg
MappedWorld --sampling point --demand 0,0=house --repair housing --diagnostic-svg unsafe-study.svg
MappedWorld --demand 0,0=house --repair housing --diagnostic-svg retained-baseline.svg
```

The second command demonstrates an honest failed leaf-only repair. The fourth
creates an explicitly unsafe diagnostic, not a safe house export. The fifth
may export a clearly labeled **NOT CURRENT** retained baseline after the failed
repair; it does not present that old composition as satisfying the new demand.

The CLI workflow is deliberately explicit: generate the initial preset
baseline **without the requested edits**, apply all requested edits, then run
exactly the named `--repair` scope. Every edit therefore requires `--repair`.
Without edits or a repair option, the command generates and inspects a baseline.
If baseline generation fails, it does not apply edits or attempt the repair.
There is no fallback to a wider scope.

## Native options

Run `--help` for the complete surface. `--help`, `--version` and `--selftest`
run alone. Other options accept either `--name value` or `--name=value`.
Numbers are decimal digits only, without whitespace, plus signs, exponents or
hex prefixes; region offsets additionally accept a leading minus sign.

Defaults are interior study, seed `3`, cell coverage, negotiated search,
64 local backtracks, 16 pass backtracks, trace off and inspection site `(0,0)`.
Budget zero is valid. Budgets count search work, not elapsed time, and a limit
result is not proof that no solution exists.

Use `--sampling cell|point|region`. A region is a half-open XYZ box relative to
the lower world corner of each housing cell. The default region
`0,0,0:8,8,1` equals cell coverage; a setback includes neighboring space:

```text
MappedWorld --sampling region --region=-1,-1,0:9,9,1 --demand 0,0=house --repair foliage
```

`--region` requires region sampling. Every house always checks full terrain
coverage; point/region study choices change only the foliage query. The physical
policy always checks the entire house footprint, even when the selected query
does less.

Repeat cell edits only for distinct targets in the same category:

```text
--demand 0,0=house
--demand 1,0=optional
--domain foliage:7,7=clear
--domain terrain:0,0=land|water
--domain foliage:8,7=none
--lock foliage:7,7=tree
```

Quote an entire argument containing `|` in your shell. `none` explicitly sets
an empty allowed domain; it is not a token or a command to clear the restriction.
To relax a study-zoned foliage cell, explicitly set its full vocabulary, for
example `--domain foliage:8,7=clear|tree`. Locks are caller-owned values, separate
from domains. Duplicate edits to the same cell/category reject; different
categories can deliberately constrain the same cell, and the solver must
respect them together. Each CLI invocation starts a new session with no prior
user overrides or locks, so it does not offer no-op clearing/unlocking flags.
The persistent browser session does offer those operations: clearing a user
domain override restores any preset zoning and leaves housing demands intact.

`--repair housing` permits only housing; `--repair foliage` permits foliage and
housing; `--repair all` authorizes all three passes. Terrain edits require the
all-pass scope, and foliage edits require at least the foliage scope. A narrower
scope reports a mismatch instead of using stale providers. `--ordinary` selects
one-way solving; `--negotiated` is the default, and the two cannot be combined.

Sandbox-only positive integer weights are `--land-weight`, `--water-weight`,
`--clear-weight` and `--tree-weight`. Defaults are `12`, `1`, `12`, `1`.
The current coordinator excludes whole provider assignments, not a minimal
set of blocker cells. A free large provider can therefore exhaust its budget
before finding a useful repair. The workbench exposes that limitation.

`--inspect X,Y` is read-only. Its report identifies the snapshot revision,
world-space query, sampled provider cells, ownership/domain state, clear corner
and interior blockers. Selecting a site does not change generation or demands.

## Artifact and exit-status contract

`--svg NEW-FILE` requires a current result that passes both the selected model
and an independent full-footprint policy check. Dirty, failed, retained-baseline
and unsafe study results cannot become safe SVG exports.
`--diagnostic-svg NEW-FILE` is a separate explicit choice; it preserves current
versus retained revision labels and unsafe-study watermarks. The two output
options are mutually exclusive. Both use the included FPC exclusive-new-file
writer; an existing destination is never replaced.

Exit statuses:

- `0`: solved and physically safe; requested publication succeeded.
- `1`: malformed input, rejected configuration, or host/file error.
- `2`: solver contradiction or search limit. A requested diagnostic may still
  have been written, but the requested repair did not succeed.
- `3`: scope/export refusal or a selected-model success that fails physical
  safety. An explicitly requested unsafe diagnostic may still be written.

The detailed status/counters distinguish contradiction, local limit and pass
limit; the CLI does not label all failures “no solution.”

## Browser host

Stage the browser assets with the repository's pas2js script, then host them
with the included native FPC server on an unused local port:

```powershell
.\build-browser-mapped.ps1 -Compiler 'C:/path/to/pas2js.exe'
.\build\native\bin\wfc_serve.exe --root build/browser/mapped/www --port 4192
```

Or from a Unix shell:

```bash
PAS2JS=/opt/pas2js/bin/pas2js bash ./build-browser-mapped.sh
build/native/bin/wfc_serve --root build/browser/mapped/www --port 4192
```

Build the included server with the repository's native build first. Open
`http://127.0.0.1:4192/`; the server stays in the foreground and stops with
Ctrl+C. Staging contains `index.html`, `mappedworld.css` and the freshly compiled
`BrowserMappedWorld.js` with its embedded Pascal runtime. No other runtime is
needed to host it. For an explicit trusted-LAN bind, follow
[development tools](../../../docs/development-tools.md#access-from-a-trusted-local-network).

Definition/seed changes require a new session. Generate establishes the baseline;
edits invalidate the current presentation immediately, and the explicit repair
buttons grant only their named scopes. Selecting a site is read-only. A failed
repair retains its old baseline privately but must not leave a stale safe
download available. The separate diagnostic route is visibly labeled.

## Reuse the model

`mapped_world_workbench.pas` owns graph state, baseline/current publication and
reports. `mapped_world_validation.pas` independently enumerates literal world
intersections; it does not ask the solver whether its own answer was correct.
`mapped_world_svg.pas` renders detached data at actual world coordinates.
Platform adapters own only CLI or DOM/file-download concerns.

This example does not pretend that existing global-shape portable recipe/run
formats can save unlike-sized pass layouts. Genuine versioned per-pass layout
persistence remains separate work. See [mapped passes](../../../docs/mapped-passes.md)
and [integer lattice mapping](../../../docs/lattice.md) for the core contract.
