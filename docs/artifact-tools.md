# Validate and inspect saved work

`wfc-validate` checks a saved artifact and its required context. `wfc-inspect`
explains that same artifact without training or running a solver. Both use
project-owned Pascal, the existing strict codecs, and thin native FPC hosts;
their application logic and inspection reports also run under pas2js.

This closes a practical workflow gap: a training source, model, recipe, run,
and result can each be checked outside the demo that created it. A valid file
is not automatically a valid composition or a proof that generation will
succeed. The commands explicitly distinguish those claims.

## Build and try

Run `build.ps1` or `bash build.sh` as described in the [build guide](building.md).
The repository executables are `build/native/bin/wfc_validate[.exe]` and
`build/native/bin/wfc_inspect[.exe]`. Help uses the distribution-facing names
`wfc-validate` and `wfc-inspect`; no separate package installation is needed.

From the repository root on Windows:

```powershell
.\build\native\bin\wfc_validate.exe training test/fixtures/training-cli/connectivity.wfclearn
.\build\native\bin\wfc_inspect.exe recipe --limit 100 test/fixtures/training-cli/connectivity.wfcpipeline
.\build\native\bin\wfc_validate.exe result --replay test/fixtures/pipeline-cli/recipe.wfcpipeline test/fixtures/pipeline-cli/solved.wfcrun test/fixtures/pipeline-cli/solved.wfcresult
```

On Linux or macOS use the corresponding `./build/native/bin/wfc_validate`
and `./build/native/bin/wfc_inspect` executables with the same arguments.
The first command checks editable source policy; the second displays the
learned recipe's public pass, resource, and network. The third executes
the recorded invocation and requires the complete result to replay exactly.
These committed fixtures name their project-authored MIT source.

## Command forms

```text
wfc-validate FAMILY [--quiet | --emit-canonical] [--] INPUT
wfc-validate run [--quiet | --emit-canonical] [--] RECIPE RUN
wfc-validate result [--replay] [--quiet | --emit-canonical] [--] RECIPE RUN RESULT
wfc-validate --help
wfc-validate --version

wfc-inspect FAMILY [--limit N] [--] INPUT
wfc-inspect run [--limit N] [--] RECIPE RUN
wfc-inspect result [--limit N] [--] RECIPE RUN RESULT
wfc-inspect --help
wfc-inspect --version
```

For the single-input form, `FAMILY` is one of `rules`, `model`, `pattern2d`, `pattern3d`,
`sequence`, `training`, or `recipe`. Families are explicit, not guessed from
filename extensions or a permissive fallback parser.

| Family | Required input | What acceptance establishes |
| --- | --- | --- |
| `rules` | `wfcrules=1` | Canonical authored local rules, weights, vocabulary, and reciprocal closure. |
| `model` | `wfcm=1,2,3` | Canonical cardinal observations and the immutable model contract, including volumes. |
| `pattern2d` | `wfcp=1` | Canonical overlapping footprints and exact structural model consistency. |
| `pattern3d` | `wfcp=2` | Canonical joint XYZ footprints, exact derived six-direction overlap relations, symmetry and observation counts. |
| `sequence` | `wfcs=1,2` | Canonical latent sequence states, histories, vocabulary, and observation counts; circular models additionally require balanced weighted contexts and no open endpoints. |
| `training` | `wfclearn=1,2,3,4,5,6` | Canonical editable samples, options, provenance, and authored quota/connectivity policy, including circular sequences and overlapping volumes. No training occurs. |
| `recipe` | `wfcpipeline=1,2,3,4` | Canonical embedded resources, static topology, pass DAG, bridges, public requirements, and policy. No graph is compiled. |
| `run` | Recipe, then `wfcpipeline-run=1` | Canonical invocation bound to that recipe, including shape, options, public locks, and domains. Runtime preparation is not performed. |
| `result` | Recipe, run, then `wfcpipeline-result=1` | Canonical bound terminal record, valid repeated fields, public layer shape/vocabulary, and the result decoder's solved public quota/connectivity checks. Not a complete solution proof. |

Exactly one positional file may be `-` for standard input. Context files are
supplied by the caller; metadata and embedded strings never cause additional
files or URLs to be opened. Options precede all positional paths. `--` permits
paths beginning with a hyphen. Extra paths, duplicate options, two stdin
operands, or an unsupported option are usage errors.

The validator's default output is one summary line. `--quiet` suppresses
success output; `--emit-canonical` emits only the exact primary input after
successful checking, not its recipe or run context. It does not repair,
normalize, or convert an invalid document. Existing recipe summaries and
version-1/2/3 recipe bytes are unchanged; the expanded CLI contract is version 2.

## What replay adds

Only `wfc-validate result --replay` executes a solver. It constructs a fresh
runtime using the supplied recipe and recorded run, including the seed,
algorithm/bridge versions, strategy, search budgets, locks, and domains. It
then compares **every byte** of the freshly encoded result with the supplied
canonical result. Comparing only their short signatures would be weaker.

A mismatch in cells, status, counters, failure fields, or evidence identity is
an error. Expected preparation failures are reported as invalid replay
invocations. The command does not overwrite the stored artifact, change its
budgets, relax constraints, or search for a different acceptable result.
Recorded search budgets bound search counts, not elapsed time.

Without replay, the existing result decoder does not independently verify all
local adjacency, locks/domains, transform copies, cross-pass requirements,
private pattern/sequence witnesses, or the occurrence of claimed evidence.
The tests deliberately construct signed, canonical results with altered
in-vocabulary output or counters: ordinary artifact checking accepts them,
while replay rejects them. This boundary is intentional and visible, not a
claim that signatures prove solutions.

Exact replay establishes agreement with the pinned runtime's independently
validated execution path. It is not a new proof system, cryptographic
authentication, aesthetic-quality test, or independent reimplementation of
the entire solver. Replaying a contradiction or exhausted budget can also
succeed; that does not prove every possible assignment is impossible.

## Reading an inspection

Reports are deterministic LF-delimited ASCII text beginning `wfc-inspect=1`.
They always identify the family, summary, validation scope, and
`execution=not-run`. Detail records expose:

- authored vocabularies, weights, local rows, and explicit edge targets;
- learned cardinal observations, source shapes, overlapping payloads, and
  sequence histories/state counts;
- training sample inventory, source metadata, and explicit policy profiles;
- recipe pass labels, visibility, adapters, transforms, dependency edges,
  resource provenance, projection bridges, requirements, quotas, and networks;
- run shape, full unsigned seed, settings, locks, and allowed-token domains;
- result outcomes, failure fields, claimed evidence identity, and public cells
  with zero-based XYZ coordinates.

The inspector does not dump embedded source payloads, infer missing local
edges, expose private result grids, retain live domains, or invent unavailable
trace events. A training inspection lists sample shapes/counts rather than
every corpus token; use the canonical source to inspect that complete corpus.
Pattern compatibility and sequence transitions remain defined by their
displayed structural models, not a fabricated observation table.

Tokens and metadata use the same canonical UTF-8 percent encoding as the
artifacts. For example, a newline becomes `%0A` and an escape character becomes
`%1B`; a corpus cannot inject terminal controls or extra report records.
Vocabulary and authored-policy order are retained. Public result cells follow
the stored row-major X/Y/Z order.

`--limit N` selects the maximum number of detail records across the whole
report; it defaults to 256. `0` requests headers and the footer only. A limit
does not skip validation or alter the artifact. The footer always reports
`details-shown`, `truncated`, and `truncation` (`none`, `record-limit`, or
`byte-limit`). An exact limit that includes the final record is not reported
as truncated.

The report has a separate 16 MiB byte envelope, including headers/footer.
Records are never cut in half to fit it. Increasing the record limit cannot
bypass this output envelope; use `--emit-canonical` when the complete artifact
is needed. This is a bounded inspection view, not a replacement serialization.
Input roles retain their existing format-specific limits: training 8 MiB,
standalone model families 16 MiB, runs 64 MiB, and recipes/results 256 MiB.
Native hosts bound reads before accumulating oversized files or pipes; shared
code preflights each supplied text role before decoding dependencies.

## Exit behavior and embedding

| Exit | Validator and inspector |
| ---: | --- |
| `0` | Accepted artifact, successful requested replay, help, or version. A valid stored non-solved result also returns 0. |
| `1` | Invalid artifact/context, replay preparation failure, or replay mismatch. |
| `2` | Command-line usage error. |
| `3` | Input/output failure. |
| `70` | Unexpected internal failure. |

Exit `4` remains the separate [runner's](pipeline-artifacts.md#headless-command-contracts)
“execution produced a valid non-solved result” outcome. It is not an artifact
validation failure. Invalid input produces no report/canonical stdout;
diagnostics use one bounded, control-safe line on stderr. A downstream write
failure may naturally occur after some bytes have already been delivered.

Portable code can use `TWfcArtifactDocument` in
`tools/wfc_artifact_document.pas`, `WfcInspectArtifact` in
`tools/wfc_artifact_inspect.pas`, or the validator/inspector application units
directly with `-Futools -Fusrc`. The document owns its decoded objects and
contexts; typed properties are borrowed read-only references valid only
while their owner lives. `CanonicalText` and reports are detached strings.
`RequireReplay` is explicit and valid only for a result document.

These tool units do not enlarge the runtime package's public unit inventory.
The native `wfc_artifact_cli_io` adapter handles only bounded file/pipe bytes
and output writes. No shell parser, host serializer, external service,
separate interpreter, or separately fetched library supplies portable behavior.

## Verification and remaining scope

The checked native builds run the shared document, report, and application
suites and an FPC real-process suite against actual validator/inspector
executables. Portable suites also execute in the browser through the included
FPC server, completion capture, and checker. Coverage includes all eight
families, source v4, every context/stdin position, exact legacy summaries,
malformed/binding failures, solved and non-solved replay, canonical forged
results, Unicode/control-safe output, and record/byte truncation.

This is the saved-artifact inspection foundation. Cross-family asset/license
lint, learned unreachable-value analysis, an independent complete public
solution checker, batch execution, interactive stepping/live domains, music
artifact inspection, and compressed trace files remain future work. It does
not silently claim those roadmap exit gates.
