# Asset inventory and review

WFC's project-authored source is MIT licensed. This does not establish the
origin or terms of every historical binary included in its source history.

`ASSETS.wfcassets` records the exact size and SHA-256 of nine historical
assets: four notation images, two icons, two compiled resources, and one
Building Kit ZIP. Their seven distinct contents total 550,022 bytes across
the nine paths. All currently have `review=unresolved` and explicit unknown
origin, author, version, license and modification fields. This records a
documentation gap, not a finding that the assets are unlicensed or infringing.
No binary has been removed, rewritten or assigned new license terms.
These retained historical files are not consumed by standard demo execution.

The music READMEs record the images' historical purpose, but not their source
or redistribution terms. The identical icon pair and identical resource pair
do not establish authorship. The ZIP contains `simpleBuildingProtoSet.fbx`
(62,032 bytes, SHA-256
`06E0AF434F1A798A758D765B9159EB522B7D7C23414F9D7F86CF981E1E8C6913`).
Its tool metadata is not an authorship or license declaration. The archive
member is noted here from a separate inventory inspection; version 1 of the
checker hashes the listed ZIP bytes and does not inspect archive contents.

## Included checker

`wfc_asset_check` is a read-only native FPC command. Manifest parsing, inventory
matching, command semantics and the project-owned streaming SHA-256 have no
third-party runtime library dependency. Shared semantics also compile with
pas2js; filesystem access belongs only to the native host. Git is not called
by the checker.

The ordinary `build.ps1` / `build.sh` native gate builds
`build/native/bin/wfc_asset_check[.exe]` and tests the shared implementation and
real native host. To build only the checker from the repository root, with
FPC on `PATH`, use an isolated output directory:

```powershell
$assetBuild = Join-Path 'build' ('asset-tool-' + [guid]::NewGuid().ToString('N'))
$assetUnits = Join-Path $assetBuild 'units'
$assetBin = Join-Path $assetBuild 'bin'
New-Item -ItemType Directory -Force -Path $assetUnits, $assetBin | Out-Null
& fpc -B -Mdelphi -Sa -Cr -Co -Ci -gl -Fusrc -Futools `
  "-FU$assetUnits" "-FE$assetBin" tools/wfc_asset_check.lpr
if ($LASTEXITCODE -ne 0) { throw 'Asset checker compilation failed' }
& (Join-Path $assetBin 'wfc_asset_check.exe') --version
if ($LASTEXITCODE -ne 0) { throw 'Asset checker smoke check failed' }
```

```bash
set -e
mkdir -p build
asset_build=$(mktemp -d "$PWD/build/asset-tool.XXXXXXXX")
mkdir "$asset_build/units" "$asset_build/bin"
fpc -B -Mdelphi -Sa -Cr -Co -Ci -gl -Fusrc -Futools \
  "-FU$asset_build/units" "-FE$asset_build/bin" tools/wfc_asset_check.lpr
"$asset_build/bin/wfc_asset_check" --version
```

Choose your compiler executable explicitly if it is not on `PATH`. These
commands do not need Git, a browser, a server, or an external hash library.
The following examples use the ordinary native-gate output; substitute the
isolated executable path when using this standalone build.

### Capture a complete source inventory

Give the command an explicit inventory of the source distribution being
checked. A producer can list Git-tracked files, or a release builder can list
the files it actually assembled. A one-path-per-line ASCII text file is
converted by the included command:

    wfc_asset_check --inventory-from tracked-paths.txt

This writes canonical `wfcfiles=1` text to stdout. Capture its raw bytes as
`tracked.wfcfiles`; do not add a BOM or translate LF into CRLF. LF and CRLF
are both accepted in the raw input, with an optional final terminator.
Blank rows, duplicates, case collisions and unsafe portable paths are refused.
An empty raw inventory converts to a valid empty canonical inventory.

    wfc_asset_check --root . --manifest ASSETS.wfcassets --files tracked.wfcfiles

The default check reports unresolved metadata but succeeds when the supplied
inventory and all asset bytes match. For an explicitly reviewed release:

    wfc_asset_check --root . --manifest ASSETS.wfcassets --files tracked.wfcfiles --require-reviewed

That second command must refuse the current unresolved records. A
`documented` record requires nonempty descriptive fields, no exact literal
`unknown`, and at least one separately listed readable evidence companion.
This is record completeness, not independent verification of legal permission
or compatibility with MIT. Record actual authorship and terms before changing
status; never use the root MIT notice to fill an unknown asset license.

For a Git checkout on Bash, capture all tracked paths from the repository
root, not a suffix-filtered subset. This creates a fresh evidence directory
and stops on any failed command or nonempty converter stderr:

```bash
set -e
asset_check="$PWD/build/native/bin/wfc_asset_check"
asset_evidence=$(mktemp -d "$PWD/build/asset-check.XXXXXXXX")
git -c core.quotepath=false ls-files --full-name > "$asset_evidence/tracked-paths.txt"
"$asset_check" --inventory-from "$asset_evidence/tracked-paths.txt" \
  > "$asset_evidence/tracked.wfcfiles" 2> "$asset_evidence/inventory.stderr"
test ! -s "$asset_evidence/inventory.stderr"
"$asset_check" --root "$PWD" --manifest "$PWD/ASSETS.wfcassets" \
  --files "$asset_evidence/tracked.wfcfiles"
```

Bash's native stdout redirection preserves the emitted bytes. The converter
sorts and validates; it does not interpret Git quoting or normalize unsafe
filenames. An out-of-profile tracked path must fail. Keep the complete
inventory for the exact tree being checked; do not reuse a list from before
files were added or removed. Failed attempts may remain as evidence but must
not be used as successful canonical input.

For Windows, the complete maintained PowerShell byte-capture recipe is the
Windows **Check tracked asset inventory and bytes** step in
[the CI workflow](../.github/workflows/ci.yml). Copy that step's `run` block,
including its `Invoke-AssetCapture` function, into a PowerShell 7 session at
the repository root after the native build; the YAML wrapper is not a command.
It runs Git and the FPC converter with `ProcessStartInfo`, begins concurrent
`StandardOutput.BaseStream.CopyToAsync` and stderr drains before waiting,
checks the actual exit, and writes captured bytes into fresh files. The
converter's stderr must be empty before its stdout is accepted as the
canonical inventory. Do not replace this capture with `Out-File`,
`Set-Content`, a line pipeline or encoding-dependent text redirection: a BOM,
translated newline or lost byte must not become part of `wfcfiles=1`.
The capture code is shell orchestration of the native tools, not a second
implementation of the asset format or hashing algorithm.

Git is only one inventory producer. For an assembled source distribution,
provide the actual complete one-path-per-line file list to `--inventory-from`
and use the same exact-byte capture. This checker is not a release assembler
and does not automatically include generated demo/RTL outputs in that list.

## Scope and limits

The supplied file list determines scope. Version 1 requires asset declarations
for its published image, model, archive, audio, font and PDF suffix profile;
other suffixes may be declared too. It does not discover omitted files, sniff
unrecognized binary content, inspect archives, or audit source-code licensing.
A complete release still needs a complete distribution inventory, companion
notices and review of archived contents. A passing byte check alone is not a
release-readiness verdict.

All record and canonical-byte allowances are explicit and adjustable:

| CLI option | Default | Applies to |
| --- | ---: | --- |
| `--max-manifest-bytes` | 1048576 | Manifest canonical bytes, check mode |
| `--max-file-list-bytes` | 33554432 | Raw input / canonical file-list bytes, both modes |
| `--max-assets` | 100000 | Manifest entries, check mode |
| `--max-files` | 1000000 | Inventory paths, both modes |
| `--max-evidence-paths` | 1000000 | Total evidence references, check mode |

Values are canonical nonnegative decimal integers up to 2147483647. The
inventory mode accepts only its input and the two applicable limit options;
check-mode flags cannot silently change inventory conversion. Duplicate,
missing, unknown and irrelevant options fail. `--help` and `--version` are
standalone commands. There is no implicit mode, filesystem scan or retry.
These are logical limits, not promises that the host can allocate that memory.
Asset bytes are streamed through a fixed 64 KiB buffer. Exact byte-size text
supports the nonnegative signed 64-bit range, while SHA-256 itself requires
less than 2^64 bits: unsupported actual message sizes are explicitly refused.
See [the SHA-256 implementation and capacity contract](asset-sha256.md).

Paths inside inventories use a documented ASCII relative-path profile, strict
ordinal order and case-insensitive uniqueness. Matching across inventories is
exact-case. Absolute paths, traversal segments, Windows device basenames,
trailing dots/spaces and alternate streams are rejected. In the filesystem,
all selected files and parent directories must be ordinary local paths;
links/reparse points are refused. Run against a trusted, non-concurrently-
mutating release tree, not a hostile filesystem requiring race-proof isolation.
No asset is fetched, extracted, executed or changed by a check.

Exit codes: 0 matching supplied inventory and bytes; 1 semantic mismatch or
unresolved metadata when review is required; 2 command usage; 3 invalid
canonical metadata or policy; 4 I/O or a refused filesystem path. Diagnostics
identify mismatch categories without claiming legal conclusions.

## Canonical formats

Both formats contain printable ASCII and LF only, including one final LF.
No BOM, CR, blank/comment line, unknown field or out-of-order field is accepted.
Counts are canonical nonnegative decimal integers (no leading zero except 0).
String tokens use the existing WFC canonical UTF-8 percent codec: uppercase
hex escapes, no redundant escapes, and no Unicode normalization. For example,
`docs/asset notes.md` is encoded as `docs%2Fasset%20notes.md`.
When using the shared Pascal API directly, native descriptive `String` fields
must contain UTF-8 bytes; browser fields use ordinary Unicode strings. Native
locale-encoded ANSI text is not silently converted into another declaration.

A manifest starts with `wfcassets=1` then `assets=N`. For each zero-based index
`i`, write these fields in this exact order:

| Field after `asset.i.` | Content |
| --- | --- |
| `path` | Encoded portable asset path |
| `bytes` | Exact canonical decimal byte size, up to 9223372036854775807 |
| `sha256` | Exactly 64 uppercase hexadecimal characters |
| `review` | `unresolved` or `documented` |
| `origin` | Encoded nonempty source/origin description |
| `author` | Encoded nonempty authorship declaration |
| `version` | Encoded nonempty source version or revision |
| `license` | Encoded nonempty actual terms or identifier |
| `modifications` | Encoded nonempty modification description |
| `evidence.count` | Number of separately listed evidence paths |
| `evidence.j` | Each zero-based evidence path, encoded, in order |

The manifest and each evidence list are strictly sorted by exact ASCII path
order and reject case-folded duplicates. An asset cannot serve as its own
evidence, including through a different case spelling. All five descriptive
fields may explicitly be `unknown` while unresolved. A documented record
rejects that exact sentinel; the checker does not guess synonyms or make legal
judgments from natural language.

A file inventory starts with `wfcfiles=1` then `files=N`, followed by each
zero-based `file.i=<encoded path>`. It uses the same sorted, unique portable
path rules. Every manifest row and evidence reference must occur with exact
case in this file inventory, even on a case-insensitive host.

The version-1 required suffixes are case-insensitive:
`png jpg jpeg gif bmp ico res zip 7z tar gz fbx obj glb gltf blend stl svg
wav wave mp3 ogg flac mid midi ttf otf woff woff2 pdf`.
Only the final filename suffix is classified; a directory's suffix is not.

Native command paths are separate from inventory-relative paths and may use
ordinary platform path syntax. A filename beginning with `--` must use an
explicit `./` or absolute prefix so it cannot be confused with an option.

Within inventory paths, `/` separates nonempty segments containing only
ASCII letters, digits, spaces, `_`, `-` and `.`. A segment cannot be `.` or
`..` or end in a dot/space. Windows device basenames are rejected regardless
of case or extension, including `CON`, `PRN`, `AUX`, `NUL`, `COM0` through
`COM9` and `LPT0` through `LPT9`; spaces before an extension do not bypass the
device check. Backslashes, colons and non-ASCII names are not normalized into
this profile. Descriptive fields can still contain canonical UTF-8 text.

## Shared Pascal API

The three units remain under `tools/`; using WFC core/runtime packages does
not import them. Manifest/file-list inputs are validated before publication
into immutable owners. Callers own and free returned classes. Returned
records, arrays and reports are detached; mutating a copy does not edit the
owner. The separate SHA context is an explicitly mutable streaming owner.

| Unit | Principal API |
| --- | --- |
| `wfc_asset_manifest` | `TWfcAssetManifest.Create(Entries, Limits)`, `EntryAt`, `CopyLimits`, `Count`, `EncodedBytes`, `DocumentedCount`, `UnresolvedCount` |
| `wfc_asset_manifest` | `TWfcAssetFileList.Create(Paths, Limits)`, `PathAt`, `CopyPaths`, `CopyLimits`, `Count`, `EncodedBytes` |
| `wfc_asset_manifest` | `EncodeWfcAssetManifest`, `DecodeWfcAssetManifest`, `EncodeWfcAssetFileList`, `DecodeWfcAssetFileList` |
| `wfc_asset_manifest` | `ValidateWfcAssetPath`, `WfcAssetPathRequiresManifest`, `CheckWfcAssetInventory` |
| `wfc_asset_check_app` | `ParseWfcAssetCommand`, `ConvertWfcAssetPathLines`, `WfcAssetCommandHelp`, `WfcAssetOneLine` |
| `wfc_sha256` | [Streaming context and byte digest helpers](asset-sha256.md#interface) |

`TWfcAssetManifestLimits` explicitly supplies `Version=1`, `MaxAssets`,
`MaxEvidencePaths` and `MaxEncodedBytes`; `TWfcAssetFileListLimits` supplies
`Version=1`, `MaxFiles` and `MaxEncodedBytes`. Direct API callers choose these
limits themselves. Encode/decode also receives explicit limits and validates
against them. `ByteSizeText` remains exact decimal text, never a JavaScript
floating-point size.

`TWfcAssetInventoryReport` exposes all missing required entries, orphan
manifest rows, and missing evidence references, plus review counts and
`MatchesSuppliedInventory`. This checks declarations against the supplied
list, not filesystem bytes. The native host separately reads selected assets
and checks their size/hash; evidence companions are checked for readability,
not interpreted as proof of permission.

Native descriptive `String` inputs and decoded outputs contain raw UTF-8
bytes; ordinary locale-sensitive `String`/`UTF8String` casts are not a safe
way to preserve those bytes on every host. The shared implementation copies
bytes across its native codec boundary and validates UTF-8. pas2js inputs
instead use ordinary Unicode strings. Raw JavaScript container boundaries
require passive, dense values and refuse accessor/coercion cases; this is
not a sandbox against proxies, forged owners or replaced global intrinsics.

## Conformance and native process fixture

The native build runs `wfc_sha256_test`, `wfc_asset_manifest_test` and
`wfc_asset_check_app_test`. The browser build discovers those same portable
programs, and the maintained FPC-hosted browser runner executes them. The
separate native `wfc_asset_check_process_test` invokes the actual checker
against files and checks command exits, mismatch reporting, exact conversion
bytes, limits, refusal paths and preservation of inputs.

After the normal native gate, invoke that fixture alone with exactly two
arguments (add `.exe` to both program names on Windows):

```text
build/native/bin/wfc_asset_check_process_test build/native/bin/wfc_asset_check build/asset-fixture-new
```

The fixture directory must not exist; its parent must exist. Choose a fresh
name per attempt. The fixture creates and retains its own input/output
evidence there. To compile it alongside the standalone checker above, use
the same FPC flags/output directories and add `-Futest`, with source
`test/wfc_asset_check_process_test.lpr`. It is native-only and intentionally
refuses pas2js compilation; the shared suites are not excluded from browsers.

Symbolic-link cases execute only when the host can create those links.
Unavailable link creation is reported as skipped, not as a successful
link-refusal test; no elevated-permission or alternate-link fallback is used.
The initial four-target Windows proof observed six such skips per target.
Full process assertion totals include repeated byte and directory audits and
must not be presented as that many distinct scenarios or as universal host
coverage. The SHA [verification section](asset-sha256.md#verification) lists
its separate observed native and actual-browser counts.
