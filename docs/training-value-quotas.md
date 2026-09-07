# Authoring quantities in training documents

Training can now preserve two different kinds of information in one editable
source: observed examples and explicit hard output quotas. Examples teach local
relationships and weights. Quotas express authored requirements such as
“exactly one café token” or “between six and eight tree cells.” They are never
inferred from sample frequencies.

## Try it in Training Studio

Build and open [Training Studio](../examples/learning/05_TrainingStudio/README.md).
Select the whole-token phrase preset and train it. In **Whole-output quotas**,
enter a label, select `café`, set minimum and maximum to `1`, and apply the quota.
Configure and solve: the private sequence must now emit `café fox .` instead of
the seed-zero unconstrained `red fox .`.

Download the source. Reopening that `.wfclearn` document and training again
preserves the quota. Recipe export contains the equivalent `wfcpipeline=2`
constraint, so native execution and browser execution share the same policy.
Save the run too when you need exact shape, seed, locks, and search settings.

For a spatial contradiction, use the 4×4 overlapping checkerboard. Exactly
eight `A` cells is feasible; exactly seven is not. A contradiction clears the
old board. Remove the quota to recover the original seeded result. Volume
quotas count the entire output, including every Z slice—not each slice alone.

Draft quota edits disable solving and derived exports until applied or
discarded. Discarding a draft never resurrects an old result. Source changes,
preset changes, and quota edits invalidate obsolete pending file reads.

## Portable Pascal API

`TWfcTrainingValueQuota` contains `LabelText`, `Values`, `MinimumCount`, and
`MaximumCount`. There is deliberately no pass index: training has one public
`output`, whose index depends on the learner. The compiler resolves that owner
and converts token strings to its actual vocabulary order after learning.

```pascal
{$codepage utf8}
SetLength(Quotas, 1);
Quotas[0] := MakeWfcTrainingValueQuota('one-cafe', ['café'], 1, 1);
Document := TWfcTrainingDocument.Create(Metadata, Options, Samples, Quotas);
Recipe := LearnWfcTrainingRecipe(Document);
```

The original constructor still creates a quota-free source.
`CopyValueQuotas`, `ValueQuotaAt`, `ValueQuotaCount`, and `ValueQuotaVersion`
provide detached inspection; capability version is zero when empty, one when
present. All arrays are copied. Labels are nonempty and unique in the document.
Each set contains unique nonempty public tokens in authored order. That order
is preserved by source text and identity, but never used as a learned index.
Reordering samples cannot redirect a quota to a different token. Unknown
source or learned-public tokens are rejected, never silently omitted.

Bounds are exact finite integers with
`0 <= minimum <= maximum <= High(Integer)`. They are absolute output cell
counts. Source size does not restrict them, and changing output size does not
clamp them. A run smaller than a requested minimum is a normal contradiction;
a maximum above the output size is harmless.

`TWfcTrainingWorkspace.ReplaceValueQuotas(Quotas)` replaces the entire source
registry, updates canonical `SourceText`, and retrains. Invalid records or a
workspace source-size rejection retain the preceding editable source but clear
all derived artifacts. Once a new canonical draft is valid, it becomes the
source before learning; a later learning failure retains that requested draft
for correction, still with no recipe/run/result. Quota editing therefore cannot
leave a successful artifact from an older policy available. The browser keeps
its source editor synchronized with this draft on both success and failure.

## Explicit format version

Nonempty quota registries without connectivity select `wfclearn=3`. All existing quota-free
non-volume documents retain exact version-1 text and identity; quota-free
volumes retain exact version-2 text and identity. Removing the last quota
restores the appropriate old format. No reader silently accepts quota records
inside versions 1 or 2.

[Authored connectivity](training-connectivity.md) selects version 4 and includes
the same quota section alongside named networks. Editing either policy preserves
the other. Removing connectivity returns to version 3 when quotas remain.

Version 3 uses `sample=index,width,height,depth,name` for every training kind;
non-volume samples must have depth `1`. After all samples and their token
records, immediately before `end`, add an ordered section:

```text
value-quota-version=1
value-quotas=1
value-quota=0,one-cafe,1,1,1
quota-token=0,0,caf%C3%A9
```

This is an excerpt, not a standalone source. It uses the existing strict ASCII,
UTF-8 percent escaping, canonical decimal numbers, LF endings, contiguous
indices, and final newline. Source has no supplied checksum. Its immutable
fingerprint includes the version-3 domain, all existing metadata/options/sample
contents, and the ordered quota descriptors. Recipe and resource provenance
use `wfclearn-v3/<fingerprint>`. The learned resource payload is unchanged when
only quotas change; the recipe adds the public policy and new provenance.

## Native tools and lossless export

The ordinary native learner accepts version 3 without an extra switch:

```text
wfc_learn authored.wfclearn
wfc_learn --quiet authored.wfclearn
TrainingStudio --quota-demo
TrainingStudio --quota-selftest
```

Use `.exe` on Windows. The default learner output is the complete canonical
quota-bearing recipe; quiet mode still constructs and validates it. The native
Studio quota demo exercises exact public sequence quantity,
source persistence, contradiction, and recovery through the same workspace.

A standalone adjacency/pattern/sequence model has no place for hard output
quotas. Consequently `--model`, public `LearnWfcTrainingModelText`, and workspace
`ModelText` reject quota-bearing sources with a clear instruction to export the
recipe. The browser disables model-only export for the same reason. Remove
the quotas explicitly if you want an unconstrained standalone model. This
prevents a saved “model” from silently losing the user's policy.

## Limits and remaining scope

Version 3 permits 4,096 quotas, 1,024 values per quota, and 65,536 aggregate
quota values. Quota labels and values share the existing 64 KiB per encoded
token and 4 MiB aggregate encoded-token budget with metadata and samples.
The complete source remains limited to 8 MiB. Version-1/2 line capacity stays
69,643; version 3 adds a separately checked 139,277-line envelope. Counts,
remaining records, and byte budgets are checked before large allocation.

The Studio's existing interactive source/output/search limits remain visible
and unchanged. It runs synchronously; these checks are not a promise of bounded
elapsed solve time. Native and low-level callers retain their larger documented
envelopes. This authoring surface targets the learned recipe's public output;
arbitrary multi-resource pipeline construction, regional counts, soft quotas,
and percentage targets are separate work. See [portable pipeline quotas](pipeline-value-quotas.md)
for private-state lowering, independent commit recount, and result validation.
