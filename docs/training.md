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
five source documents and their exact learned models, recipes, run requests, and
solved results. The checked native build creates `build/native/bin/wfc_learn`
(`.exe` on Windows), `wfc_validate`, and `wfc_run`.

```text
wfc_learn [--model | --quiet] [--] INPUT
wfc_learn --help
wfc_learn --version
```

`INPUT` is a file path, or `-` for standard input. Default output is canonical
`wfcpipeline=1` text, `wfcpipeline=2` with output quotas, or `wfcpipeline=3`
with rooted connectivity (with or without quotas). `--model` emits
`wfcm=1`/`wfcm=2`/`wfcm=3`, `wfcp=1`, or `wfcs=1`/`wfcs=2`
according to the profile; circular sequences use `wfcs=2`. It rejects sources
with quotas or connectivity because standalone models cannot preserve those
policies. `--quiet` still learns and constructs the recipe,
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
The portable units also power the [Training Studio](training-studio.md) browser
editor. The CLI help/version output advertises the supported training formats.

## Profiles

| Kind | Source options | Learned output | Recipe |
| --- | --- | --- | --- |
| `adjacency1d` | Height 1; open/wrap; symmetry none; footprint 0,0; order 0 | Radius-one east/west adjacency and counts | Rank 1, public `output` |
| `adjacency2d` | Open/wrap; none/d4; footprint 0,0; order 0 | Cardinal adjacency and counts | Rank 2, public `output` |
| `adjacency3d` | Explicit positive depth; open/wrap; none/d4/cube24/cube48; footprint 0,0; order 0 | Six-face adjacency and counts | Rank 3, public `output` |
| `pattern2d` | Open/wrap; none/d4; positive footprint of at most 64 cells; order 0 | Overlapping footprints and counts | Rank 2, private `patterns` → public `output`; wrapped input only |
| `sequence` | Height 1; open/wrap; none; footprint 0,0; order 1–64 | Bounded order-N latent states and counts; circular history when wrapped | Rank 1, private `sequence` → public `output`; whole extent for open, wrapped extent for circular |

D4 means the existing deterministic eight rotation/reflection observations,
including repeated observations when a transform is identical. Pattern D4
requires a square footprint. Open patterns remain supported by `--model`;
default and quiet recipe export reject them explicitly because the current
pattern projection bridge is wrapped-only.

Every sample has independent boundaries. Adjacency and pattern training never
join the last token of one sample to the first token of the next. Sequence
history stays within each sample. Open sequences use typed beginning-of-sample
history and observed endpoints; circular sequences wrap within the same sample,
with no BOS or invented endpoint evidence and exactly one observation per
input token. Heterogeneous sample dimensions are permitted.
Tokens and samples retain their declared order, including first-seen vocabulary
ordering. See the [cardinal](learning.md), [pattern](patterns.md), and
[sequence](sequences.md) guides for the underlying algorithm contracts.

The exported recipe contains resource `learned`, no authored cross-pass
requirements, and bottom-up traversal. It inherits the declared open/wrapped
topology. Pattern and sequence recipes use current bridge-v2 projection, so
run locks/domains on public `output` constrain private candidates before
solving; private state keys are not user input. Output sizes, seed, locks,
domains, budgets, and strategy belong to the separate recipe-bound run request.

## Editable training formats

Existing kinds retain byte-identical `wfclearn=1` documents. The additive
`adjacency3d` kind requires `wfclearn=2` and
`sample=index,width,height,depth,name`, followed by exactly `width*height*depth`
tokens in X-fast, then Y, then Z order. No other kind uses version 2. See
[volume learning](volume-learning.md) for a complete example and symmetry policy.

An explicit nonempty output quota registry selects `wfclearn=3` unless a
connectivity or circular-sequence capability requires a later source version.
It retains authored token identities across training and source save/load,
uses explicit depth in every sample record, and exports a quota-bearing recipe.
See [authoring output quotas](training-value-quotas.md) for the complete source,
workspace, CLI, bounds, and compatibility contract.

Nonempty [rooted connectivity](training-connectivity.md) selects `wfclearn=4`
for otherwise non-circular sources, preserving optional quotas. A sequence
source explicitly declaring `boundary=wrap` selects `wfclearn=5`, with or
without either policy registry; all existing open sequence source identities
remain unchanged. Version 5 is rejected for any other kind/boundary pairing.
It uses explicit depth-1 sample records, then both mandatory policy sections:
`value-quota-version=0` / `value-quotas=0` when absent (or version 1 with a
nonempty registry), followed by the analogous `connectivity-version` /
`connectivities` section. Source profile order and coordinates keep their
existing authored/canonical contracts.

The circular recipe uses `wseWrap`, so the requested output's last-to-first
edge is checked. A circle does not have an observed beginning or end; the
generic sequence adapter also permits `wseFragment` when an application wants
a finite extract rather than a closed loop. Public locks and domains still
constrain private sequence candidates through the same projection bridge.
Training circles do not guarantee every circumference can solve and do not
imply musical development. The [Studio](training-studio.md) exposes both an
explicit raw-text boundary choice and a checked circular-text preset.

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
- a versioned `wfclearn-vN/XXXXXXXX` fingerprint of the validated training
  contents: v1 ordinary sources, v2 volumes, v3 authored quotas, v4 rooted
  connectivity, or v5 circular sequences (including their optional policies).

The manifest is appended as ` | samples=` followed by comma-separated
`index:encoded-name:widthxheight:XXXXXXXX` entries. Each sample hash covers
its name, dimensions, and ordered token contents. The pipeline codec then
encodes the entire description as one token, including the percent signs in
encoded sample names. The original `.wfclearn` file should be kept alongside
the recipe: the manifest identifies input, but does not embed the entire corpus.

Volume manifest shapes are `widthxheightxdepth`. Their sample and training
hashes include depth, so reshaping identical flat tokens changes provenance.

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

For `adjacency3d`, the training prefix/version fields become `wfclearn-v2`,
`2`, the sample prefix becomes `wfclearn-sample-v2`, and each sample inserts
depth immediately after height in both hashes. Token order becomes
`x + width*y + width*height*z`. The remaining field order is unchanged;
version-1 kinds never hash an appended depth field.

Nonempty authored quotas select prefix `wfclearn-v3` and version `3`, preserve
the existing sample payload (depth participates for volumes), then append the
`value-quotas` domain, capability version `1`, descriptor count, and each
label/minimum/maximum/value-count/value sequence in authored order. Sample
fingerprints and learned resource payloads do not change solely because quotas
change. See [training quotas](training-value-quotas.md).

Circular sequences select `wfclearn-v5` and version `5`. After the usual
non-volume fields and sample payloads, append `sequence-wrap` and capability
`1`, then only the nonempty quota and connectivity registries in their existing
domain/version/descriptor order. Their sample hashes remain
`wfclearn-sample-v1`; no depth field is added to those non-volume hash payloads.
The whole-source/resource fingerprint distinguishes circular learning even
when the original sample tokens match an open document. Optional policies
retain their own versioned hash extensions.

## Limits and failure behavior

The quota-free version-1/2 resource envelope caps source documents at 8,388,608 encoded ASCII characters and
69,643 lines, with 4,096 samples, 65,536 total source tokens, and dimensions
of 1–65,536. A token field is at most 65,536 characters in canonical encoded
form; metadata, sample names, and every token occurrence together may contain
at most 4,194,304 encoded characters. Pattern footprints contain at most
64 cells and sequence order is at most 64.

Version 3 retains those byte/sample budgets and shares encoded-token capacity
with quota labels and values. Its separately checked 139,277-line envelope
permits 4,096 quota descriptors and 65,536 aggregate quota values, with at most
1,024 values per descriptor. These are authoring limits, not inferred quotas.

Versions 4 and 5 share the checked 274,447-line envelope while retaining the
same 8,388,608-character source and 4,194,304-character aggregate encoded-token
budgets. Connectivity adds at most 4,096 networks, 1,024 profiles per network,
65,536 profiles in total, and 65,536 terminals in total. Circularity does not
increase sample/token/order, model capacity, or interactive Studio limits.

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
30 cases against real learner/validator/runner processes, checking exact LF
stdout, stderr classification, file/stdin parity, all five profile bundles, and
replay results. The supplied checkerboards and phrase outputs are also
independently asserted by the core tests and pipeline validators.

The CLI deliberately starts with pretokenized corpora. The complementary
[Training Studio](training-studio.md) now adds explicit Unicode-scalar
raw-text import and browser training/locking/export controls over a shared
Pascal workspace. Raw-text file orchestration in the CLI, image/tile and
voxel imports, musical-event extraction, per-sample structured multi-license
metadata, and general batch orchestration remain separate work.
