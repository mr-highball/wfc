# Training Studio and editable workspaces

Training Studio connects editable input to training, executable recipes, seeded
runs, public constraints, and inspectable/exportable results. Its state owner is
the project-owned `wfc_training_workspace` unit, shared by native FPC and
pas2js. The browser is a presentation edge over that same code, not another
solver or learner.

The [Studio example](../examples/learning/05_TrainingStudio/README.md) includes a
browser workbench and a native demonstration of six independently checked
presets. For file-oriented automation, use [wfc-learn](training.md) and the
[recipe validator/runner](pipeline-artifacts.md).

## Editing has explicit consequences

A workspace owns one source draft, its trained recipe, one configured run, and
one terminal result. It never exposes an owned graph, recipe, or result object.

| Action | Source | Trained recipe/model | Run request | Result |
| --- | --- | --- | --- | --- |
| Set source, including identical text | Replaced | Discarded | Discarded | Discarded |
| Train | Retained | Rebuilt, or absent on error | Discarded | Discarded |
| Edit run fields / clear run | Retained | Retained | Discarded | Discarded |
| Configure run | Retained | Retained | Rebuilt, or absent on error | Discarded |
| Solve | Retained | Retained | Retained | Replaced by current terminal outcome |

This is intentionally different from the graph pipeline's transactional
regeneration contract. The solver preserves graph state on a failed transaction;
the editor does not label an earlier successful result as output from a new
draft. A malformed source remains visible for correction but cannot export an
old recipe. An invalid run edit retains training but cannot export the old
request or result. Rejected oversized source text clears the draft too, so the
workspace does not retain data beyond its configured source limit.

A valid contradiction or search-budget exhaustion is a real terminal result:
it can be exported with its failure/report identity, but has no public output
cells. Exceptions during solving leave the current request available and no
result. Repeated solving always starts from the same immutable recipe and run;
there is no hidden warm graph or advancing random stream.

## Raw text is an explicit import

`wfc_text_training` adds a bridge from ordered, named raw-text samples to
`TWfcTrainingDocument`:

```pascal
var
  Samples: TWfcTextTrainingSamples;
  Document: TWfcTrainingDocument;
begin
  SetLength(Samples, 2);
  Samples[0] := MakeWfcTextTrainingSample('first', 'a cat.');
  Samples[1] := MakeWfcTextTrainingSample('second', 'a bat.');
  Document := BuildWfcTextTrainingDocument(
    MakeWfcTrainingMetadata('phrases', 'MIT', 'project-authored example'),
    Samples, 3);
  try
    SourceText := EncodeWfcTrainingText(Document);
  finally
    Document.Free;
  end;
end;
```

Use `wfc_model`, `wfc_training`, `wfc_training_text`, and
`wfc_text_training` in the containing program. Variables such as
`SourceText` belong to the caller. The builder uses the existing project-owned
version-1 Unicode-scalar tokenizer, one scalar per token, and produces open
whole-sequence input with the requested order. It is not a word tokenizer or an
LLM. Whitespace, CR, LF, combining marks, and supplementary Unicode scalars
remain exact data. There is no line splitting, trimming, case folding,
normalization, or implied sample boundary.

The browser textarea normalizes line endings to LF before passing its value
to Pascal. That is a host input behavior, not tokenizer normalization. For
exact CR/CRLF sample tokens, use the canonical token editor/import or call
the bridge directly with exact strings.

Each explicit record becomes one independent sequence sample. Empty/malformed
text, duplicate sample names, invalid order, and exceeded corpus/encoding/model
budgets are rejected. Conservative raw-storage checks account for up to four
UTF-8 bytes on native FPC or two UTF-16 units in pas2js per remaining scalar;
exact cumulative scalar counts are checked before final document construction.
The corpus, all sample names, and all three metadata fields must be nonempty.

Metadata is retained verbatim. The caller declares source and license; the
builder does not fetch anything, infer rights, or relabel imported work as MIT.
The output document contains the actual scalar tokens, so its existing
fingerprint covers tokenization output. Keep the original text and declared
metadata with distributed corpora when stronger provenance is needed. The
32-bit replay fingerprints remain noncryptographic.

## Workspace API

Using `SourceText` from the six-scalar raw-text example above:

```pascal
Workspace := TWfcTrainingWorkspace.Create(
  InteractiveWfcTrainingWorkspaceLimits);
try
  Workspace.SetSourceText(SourceText);
  Workspace.Train;

  Options := DefaultWfcTrainingSolveOptions;
  Options.Width := 6;
  Options.Height := 1;
  Options.Seed := 0;
  Workspace.ConfigureRun(Options, nil, nil);
  Workspace.Solve;

  if Workspace.ResultStatus = wprsSolved then
    Tokens := Workspace.OutputTokens;
  ResultText := Workspace.ResultText;
finally
  Workspace.Free;
end;
```

The workspace accepts only training documents; it does not import arbitrary
authored pipeline recipes. Training exports the existing one-resource recipes
with public pass `output`, optionally backed by a private pattern/sequence
pass. `PublicPassIndex` resolves the current public owner for
`MakeWfcPipelineCellLock` and `MakeWfcPipelineCellDomain`. ConfigureRun
accepts both lock and domain arrays and detaches them through the immutable
run constructor. An explicit empty domain remains an intentional contradiction.
Lock/domain records must be strictly ordered by pass, z, y, x; domain tokens
must follow strict public-vocabulary order. One-way solving requires a zero
pass-backtrack budget. See [run artifacts](pipeline-artifacts.md) for the
complete canonical input contract.

Rank-3 training uses `ConfigureVolumeRun(Options, Depth, Locks, Domains)`.
Depth is explicit: the older `ConfigureRun` rejects rank 3 rather than silently
flattening it. Conversely, `ConfigureVolumeRun` rejects non-volume recipes.
Width and height remain in the unchanged solve-options record. XYZ lock/domain
coordinates and X-fast flattened output are preserved through exported run and
result artifacts. Invalid volume edits discard the old invocation and result.

`HasRecipe`, `HasRun`, and `HasResult` distinguish availability; accessors
requiring a missing artifact raise `EWfcTrainingWorkspace`. Underlying
training, codec, and pipeline exceptions keep their existing types.
`PublicVocabulary`, `OutputTokens`, `CopyMetadata`, `CopyPasses`,
`CopyPassOutcomes`, and `CopyFailure` expose detached records/arrays.
`RecipeText`, `ModelText`, `RunText`, and `ResultText` use the existing
canonical artifact codecs. `SourceText` is the editable draft, not an
automatically repaired document.

Source, recipe, and result signatures are available as uppercase eight-digit
strings. Workspace policy is not inserted into solver/artifact identity:
an accepted identical recipe and run retain the same result irrespective of
whether the workspace uses a stricter envelope.

## Explicit resource policy

The default constructor retains the core training/run ceilings. Callers may
choose a lower `TWfcTrainingWorkspaceLimits` at construction; limits cannot
be negative, exceed the public version-1 envelope, or be zero except for
backtrack budgets.

The interactive constructor used by the demo limits source text to 262,144
characters, source tokens to 512, samples to 64, learned model items to 128,
output cells to 512, local backtracks to 4,096, and pass backtracks to 64.
“Model items” means cardinal values, distinct patterns, or sequence states,
not source observations. Output follows the training rank: rank 1/2 uses depth
one, while rank 3 requires an explicit positive depth. The 512-cell interactive
policy bounds the complete `width*height*depth`, not each slice independently.
Rank-1 runs require height one; choose output width/height for the particular
corpus rather than assuming every learned model supports every extent.

Source-token/sample limits are checked after bounded source decoding but before
learning. Learned model-item limits are checked after extraction and before any
runtime graph is allocated. Pattern extraction still observes the underlying
pattern learner's own capacity limits. Output volume and search budgets are
checked before run construction. These are data/search-count bounds, not a
wall-clock guarantee or a multi-tenant sandbox. The browser executes
synchronously and does not claim background workers or mid-solve cancellation.
Use the native tools for larger corpora or output sizes.

## Browser workflow

Load a preset, edit or import canonical `.wfclearn` source, train, then set
shape/seed/search options and solve. Raw-text import is separate and requires
an explicit source and license declaration. The workbench lets you inspect
public tokens, add/remove locks, see pass visibility and terminal counters,
and view/download source, model, recipe, run, and result artifacts.
The token grid uses canonical percent encoding to make spaces, line breaks,
and supplementary Unicode unambiguous; each cell retains its full token label.
Volumes appear as labeled Z slices. A clicked cell copies all three lock
coordinates; locks at the same X/Y on different slices remain separate.

Source edits invalidate training and clear locks; run-field and lock edits
invalidate the invocation/result. A displayed contradiction is not replaced by
the last successful board. Unknown vocabulary, invalid dimensions, malformed
source, and unsupported open-pattern recipe export remain explicit errors.
Accepting a file import clears the old source lineage immediately; cancelled
or obsolete asynchronous reads cannot overwrite a newer edit. Raw-text
conversion initializes rank-1 output width to the imported scalar count.

The whole-output quota editor authors exact or bounded quantities over selected
public tokens. Apply replaces the quota registry in canonical `wfclearn=3`
source and retrains, so source download/import and later retraining preserve
the policy. Unapplied drafts disable solving and derived downloads; discard
does not restore an older result. Source and model identities stay separate:
quota-bearing sources cannot export a standalone model because that format
would lose the constraints. Export the recipe instead. See
[persistent quota authoring](training-value-quotas.md) for the API, failure
invalidation, bounds, and format contracts.

The UI currently exposes token locks, not an editor for arbitrary allowed-token
domain sets; those remain available through ConfigureRun and run artifacts.
It does not turn raw images, voxel sets, or MIDI into corpora. Unicode
word-boundary training, background worker execution, multi-resource recipe
authoring, and large-corpus/batch management remain separate work.

## Verification

The legacy workspace suite pins the original five source/recipe/result identities, validates
public outputs independently, round-trips exported provenance chains, proves
detached arrays, and checks invalidation, contradiction, empty-domain behavior,
recovery, and resource-policy rejection on FPC and pas2js. Text-import tests
cover exact Unicode/whitespace, sample boundaries, metadata retention, and
capacity errors.

The volume suite additionally checks full-depth replay, nonzero-Z locks and
domains, API rank separation, volume-size/budget rejection, and stale-output
invalidation. Preset 5 is checked independently for all three wrapped axes;
the browser self-test checks its pinned artifacts, slice rendering, XYZ clicks,
contradiction, and recovery before restoring the unchanged preset-2 fixture.

The browser's `?selftest=1` path exercises real controls and checks solved,
edited, contradictory, and recovered states before restoring the exact seeded
pattern fixture. Hosted CI checks the resulting body attributes. Browser
appearance is additionally inspected locally; pixel-identical rendering is not
part of the portable solver contract.
