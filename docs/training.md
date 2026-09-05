# Training documents and wfc-learn

The project-owned `wfc_training` and `wfc_training_text` units turn an
editable, pretokenized corpus into a standalone learned model or an executable
[portable pipeline recipe](pipeline-artifacts.md). They use the existing Pascal
learners; no training service, package manager, or external runtime library is
involved. The same code compiles with native FPC and pas2js.

Training is deterministic constraint extraction, not a language model. Samples
teach allowed local relationships and integer frequencies; they do not guarantee
global satisfiability, semantic plausibility, or generalization to every output
size.

## Start with a checked example

[TrainingDocuments](../examples/learning/04_TrainingDocuments/README.md) contains
four source documents and their exact learned models, recipes, run requests, and
solved results. The checked native build creates `build/native/bin/wfc_learn`
(`.exe` on Windows), `wfc_validate`, and `wfc_run`.

```text
wfc_learn [--model | --quiet] [--] INPUT
wfc_learn --help
wfc_learn --version
```

`INPUT` is a file path, or `-` for standard input. Default output is canonical
`wfcpipeline=1` text. `--model` emits `wfcm=1`/`wfcm=2`, `wfcp=1`, or `wfcs=1`
according to the profile. `--quiet` still learns and constructs the recipe,
but emits nothing. Options are mutually exclusive; `--` allows a path
beginning with a hyphen.

Successful data goes only to stdout. Failures write one LF-terminated diagnostic
line to stderr. Exit codes are 0 success, 1 invalid/unsupported training,
2 usage, 3 input/output failure, and 70 an unexpected internal failure.
A valid source can still fail learning when the target model's capacity is
exceeded, or recipe export when the current adapter cannot represent it.

The native entry source is `tools/wfc_learn_cli.lpr`; its distinct basename
avoids an FPC object-file collision with `src/wfc_learn.pas`. Build it with
`-owfc_learn` (`-owfc_learn.exe` on Windows). The host uses
`tools/wfc_learn_app.pas` and contains only bounded file/standard-stream I/O.
The portable units can also be called by a browser program; this milestone
does not add a training browser editor.

## Profiles

| Kind | Source options | Learned output | Recipe |
| --- | --- | --- | --- |
| `adjacency1d` | Height 1; open/wrap; symmetry none; footprint 0,0; order 0 | Radius-one east/west adjacency and counts | Rank 1, public `output` |
| `adjacency2d` | Open/wrap; none/d4; footprint 0,0; order 0 | Cardinal adjacency and counts | Rank 2, public `output` |
| `pattern2d` | Open/wrap; none/d4; positive footprint of at most 64 cells; order 0 | Overlapping footprints and counts | Rank 2, private `patterns` → public `output`; wrapped input only |
| `sequence` | Height 1; open; none; footprint 0,0; order 1–64 | Bounded order-N latent states and counts | Rank 1, private `sequence` → public `output`; whole extent |

D4 means the existing deterministic eight rotation/reflection observations,
including repeated observations when a transform is identical. Pattern D4
requires a square footprint. Open patterns remain supported by `--model`;
default and quiet recipe export reject them explicitly because the current
pattern projection bridge is wrapped-only.

Every sample has independent boundaries. Adjacency and pattern training never
join the last token of one sample to the first token of the next. Sequence
history resets at each sample. Heterogeneous sample dimensions are permitted.
Tokens and samples retain their declared order, including first-seen vocabulary
ordering. See the [cardinal](learning.md), [pattern](patterns.md), and
[sequence](sequences.md) guides for the underlying algorithm contracts.

The exported recipe contains resource `learned`, no authored cross-pass
requirements, and bottom-up traversal. It inherits the declared open/wrapped
topology. Pattern and sequence recipes use current bridge-v2 projection, so
run locks/domains on public `output` constrain private candidates before
solving; private state keys are not user input. Output sizes, seed, locks,
domains, budgets, and strategy belong to the separate recipe-bound run request.

## Editable wfclearn=1 format

All records below are required and ordered. The document is ASCII with
canonical percent-encoded UTF-8 token fields, LF line endings, and exactly one
final LF. It has no comments, blank lines, BOM, or supplied checksum:

```text
wfclearn=1
name=alternating
license=MIT
source=project-authored%20example
kind=adjacency1d
boundary=wrap
symmetry=none
footprint=0,0
order=0
samples=1
sample=0,4,1,first
token=0,0,A
token=0,1,B
token=0,2,A
token=0,3,B
end
```

`sample=index,width,height,name` is followed by exactly `width*height`
`token=sample-index,token-index,value` records. All indices are consecutive
zero-based unsigned canonical decimals. Tokens are row-major
(`index = y*width+x`). Sample names are nonempty and unique. Metadata fields
and token values are nonempty, well-formed Unicode strings. There are no
implicit whitespace splitting, newline splitting, normalization, image
decoding, or musical event imports.

The shared text codec leaves ASCII letters, digits, `-`, `.`, `_`, and
`~` unescaped and encodes other UTF-8 bytes using uppercase `%HH`.
For example `caf%C3%A9` is one token, `%20` is one space token, and
`%F0%9F%8E%B5` is one musical-note token. Literal separators must be encoded.
Lowercase escapes, unnecessary escapes, malformed UTF-8, extra fields,
duplicate/out-of-order records, and noncanonical numbers are rejected.

Editing any source field automatically changes its derived identity. There is
no checksum to repair in a training document. Generated recipes and run/result
artifacts are different: their existing strict signatures are required.
After changing a corpus, create a new run request bound to the new recipe;
do not reuse or manually relabel an old result as a replay.

## Provenance

One declared `license` applies to the entire training corpus. It is retained
verbatim as a label, not interpreted as a license grant or checked against a
license registry. Use only samples you have rights to use, and represent
multi-source licensing explicitly. The tool never fetches a URL named in
`source`, infers ownership, or rewrites source licenses to MIT.

Recipe metadata and its learned resource both retain:

- the declared license;
- the source description plus an ordered sample manifest;
- a `wfclearn-v1/XXXXXXXX` fingerprint of the validated training contents.

The manifest is appended as ` | samples=` followed by comma-separated
`index:encoded-name:widthxheight:XXXXXXXX` entries. Each sample hash covers
its name, dimensions, and ordered token contents. The pipeline codec then
encodes the entire description as one token, including the percent signs in
encoded sample names. The original `.wfclearn` file should be kept alongside
the recipe: the manifest identifies input, but does not embed the entire corpus.

These 32-bit FNV-1a fingerprints are deterministic replay identities, **not**
cryptographic integrity checks, proof of provenance, or collision-resistant
content addresses. Standalone `--model` output uses existing model formats
which do not carry this training provenance; distribute the source document
or the recipe with it.

### Fingerprint contract

FNV-1a starts at 2166136261, XORs each byte, then multiplies by 16777619
modulo 2^32. Every field is prefixed by its ASCII byte length as a four-byte
little-endian unsigned integer. Integer fields below are canonical decimal
strings; token fields use the shared canonical percent encoding.

The training hash consumes, in order: `wfclearn-v1`, training version `1`,
name, license, source, kind, boundary, symmetry, footprint width, footprint
height, order, sample count; then each sample's name, width, height, token
count, and every token in row-major order.

The sample hash uses `wfclearn-sample-v1`, name, width, height, token count,
and every token. Both are rendered as eight uppercase hex digits. Changes to
this extraction/identity contract require a new training version; existing
recipe adapter/solver versions remain independently pinned.

## Limits and failure behavior

Version 1 caps source documents at 8,388,608 encoded ASCII characters and
69,643 lines, with 4,096 samples, 65,536 total source tokens, and dimensions
of 1–65,536. A token field is at most 65,536 characters in canonical encoded
form; metadata, sample names, and every token occurrence together may contain
at most 4,194,304 encoded characters. Pattern footprints contain at most
64 cells and sequence order is at most 64.

A separate 16,777,216 extraction/history-visit bound checks expansion from
footprints, symmetry, and sequence order. It is not a wall-clock timeout or a
bound on every compatibility comparison. The learned model's own vocabulary,
pattern/state/history, relation-table, and encoded-artifact limits still apply.
Recipe provenance must also fit the pipeline token limit. Capacity violations
fail without returning a partial model or recipe.

The text decoder checks declared counts, areas, line count, and encoded size
before allocating declared sample arrays. The immutable document validates
Unicode, option combinations, aggregate budgets, and precomputable model
capacities before dispatch. Unique pattern/payload limits remain enforced by
the pattern learner. Adjacency and sequence learners also reject exhausted target
capacities before growing their value/state tables. This is bounded local
tooling, not a hardened multi-tenant service boundary.

## Pascal integration

```pascal
uses
  wfc_training, wfc_training_text,
  wfc_pipeline_model, wfc_pipeline_text;

var
  Document: TWfcTrainingDocument;
  Recipe: TWfcPipelineModel;
  OutputText: String;
begin
  Document := DecodeWfcTrainingText(InputText); // caller supplies text
  try
    Recipe := LearnWfcTrainingRecipe(Document);
    try
      OutputText := EncodeWfcPipelineModelText(Recipe);
    finally
      Recipe.Free;
    end;
  finally
    Document.Free;
  end;
end;
```

For programmatic input, construct `TWfcTrainingDocument` from
`MakeWfcTrainingMetadata`, `MakeWfcTrainingOptions`, and an ordered
`TWfcTrainingSamples` array. Construction and accessors detach dynamic
arrays; callers own returned document/recipe objects. Use
`LearnWfcTrainingModelText` for standalone output and
`EncodeWfcTrainingText` to save editable input. The core raises
`EWfcTraining` for training-contract errors; decoding normalizes those to
`EConvertError`, while downstream learners retain their model errors.

## Verification and remaining scope

The native build and hosted pas2js gate run immutable-document/learning,
strict-codec, and shared CLI application tests. Separate process harnesses run
25 cases against real learner/validator/runner processes, checking exact LF
stdout, stderr classification, file/stdin parity, all four artifacts, and
replay results. The supplied checkerboards and phrase outputs are also
independently asserted by the core tests and pipeline validators.

The CLI deliberately starts with pretokenized corpora. The complementary
[Training Studio](training-studio.md) now adds explicit Unicode-scalar
raw-text import and browser training/locking/export controls over a shared
Pascal workspace. Raw-text file orchestration in the CLI, image/tile and
voxel imports, musical-event extraction, per-sample structured multi-license
metadata, and general batch orchestration remain separate work.
