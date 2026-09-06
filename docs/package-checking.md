# Package completeness

Source-path builds can pass while an installed library is missing a newly
added unit. WFC checks this in two separate ways: a read-only inventory tool,
then real FPM/Lazarus builds and an application compiled against their output.
Both use FPC; no third-party runtime or scripting-language dependency is added.

## Run the inventory check

The normal `build.ps1` / `build.sh` gate builds and tests `wfc_package_check`,
then runs it against the repository. To build only this tool:

```powershell
New-Item -ItemType Directory -Force build/package-check/units, build/package-check/bin | Out-Null
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Futools `
  -FUbuild/package-check/units -FEbuild/package-check/bin tools/wfc_package_check.lpr
./build/package-check/bin/wfc_package_check.exe --root .
```

```bash
mkdir -p build/package-check/units build/package-check/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Futools \
  -FUbuild/package-check/units -FEbuild/package-check/bin tools/wfc_package_check.lpr
./build/package-check/bin/wfc_package_check --root .
```

`--root DIRECTORY` is required exactly once. `--help` and `--version` must
appear alone. Success writes one count summary to standard output. Failure
writes a bounded, printable diagnostic to standard error; it produces no
success output and never edits the repository.

| Exit | Meaning |
| --- | --- |
| 0 | Complete inventory, or successful help/version |
| 1 | Invalid repository, manifest, source declaration, or safety limit |
| 2 | Invalid command-line usage |
| 3 | File/directory/output I/O or unexpected host failure |

## What is checked

Every immediate `src/*.pas` file must have a canonical lowercase Pascal
identifier filename and a matching initial `unit` declaration. Every such
unit must appear exactly once in all three lists:

- `fpmake.pp`: literal `P.Targets.AddUnit('name.pas');` entries and the
  single literal `P.SourcePath.Add('src');` declaration.
- `wfc.lpk`: contiguous `Files/ItemN` records, matching count, canonical
  `src/name.pas` filenames and `UnitName` values.
- `wfc_package.pas`: the interface `uses` list, with no missing, duplicate,
  or unknown unit, in the same order as the Lazarus source-item list.

The Lazarus main-unit record (`wfc_package.pas`) is required but is not counted
as a library source unit. Comments, quoted Pascal strings, and XML comments
do not create declarations. Conditional package entries, include directives,
computed unit paths, and malformed or ambiguous supported syntax are rejected.
Ordinary balanced conditional directives elsewhere, such as the FPMake
`CThreads` platform guard, are allowed.

This is a deliberately restricted static manifest checker, not a Pascal or
XML execution engine. It does not prove what arbitrary FPMake code will do,
validate every source body, evaluate compiler directives, compile a package,
or repair generated files. Matching inventories alone are not installation
proof. FPM entries may have a different order. The Lazarus package and its
generated main unit must agree on source-unit order to avoid regeneration;
the checker rejects that mismatch too.

## Bounds and filesystem scope

The native host opens only the selected root's `src` directory, its immediate
Pascal source files, and the three fixed manifest files. It never follows
paths read from a manifest, invokes another tool, or writes repository files.
It rejects symbolic links/reparse points on inspected paths, non-ordinary
files, noncanonical source names, and Windows device basenames. On Windows,
the root must resolve to an ordinary local-drive path, not a UNC path.

Limits are 4,096 source units, 65,536 directory entries, 1 MiB per source or
manifest text, 32 MiB aggregate source text, 131,072 parser tokens per text,
and 512 diagnostic characters. The portable parser also bounds nesting and
XML attributes. These are tooling safety bounds, not composition-length or
solver-size limits.

Use this on a trusted local checkout. Checking a path and subsequently
opening it is not an atomic filesystem operation: this is not a sandbox
against a concurrently modified hostile filesystem. Native Windows path
handling follows the existing FPC host string conventions; comprehensive
Unicode filesystem support is not claimed.

## Installed-unit proof

The Linux package job builds FPM, installs into an isolated directory under
`build/package-install`, and builds the Lazarus runtime package. It then
compiles `test/wfc_package_consumer.lpr` independently against each output.
Compiler configuration is disabled with `-n`; only standard FPC unit
directories and the selected compiled package directory are supplied with
`-Fu`. There is no repository `src` search path and no source rebuild fallback.

The consumer exercises the public graph API, owned music-form configuration
and phrase validation, portable connectivity recipe/run/result replay, and
owned mixed-pass layout prefixes plus independent mapped-footprint validation.
This catches both missing package entries and unusable exported interfaces.
It is intentionally not named `*_test.lpr`: the source-path conformance runner
must not silently substitute for this package-only check.

For a local equivalent, build/install using [the package instructions](building.md#fpm-package),
create separate consumer unit/bin output directories, then compile with:

```text
fpc -n -Mdelphi -Sa -Cr -Co -Ci -Fu<standard-FPC-units> -Fu<compiled-package-units> -FU<consumer-units> -FE<consumer-bin> test/wfc_package_consumer.lpr
```

Supply the standard unit directories for the same compiler version and CPU/OS
target as the package. Do not add `src` or use `-B` to rebuild installed units.
Run the resulting consumer executable. The hosted job additionally builds
the maintained Lazarus test/music projects and rejects checkout changes.

The portable parser's synthetic tests run natively and through pas2js in a
real browser using the included FPC server/checker. Native process tests
exercise the actual CLI, filesystem failures, and output channels separately.
