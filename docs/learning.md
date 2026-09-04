# model learning and priming

This guide covers radius-one cardinal models. The complementary
[sequence-model guide](sequences.md) documents bounded order-N learning with
typed BOS history, structural suffix/prefix recombination, public-token pass
projection, and canonical `wfcs=1` text.

The learning layer turns one or more tokenized examples into a portable WFC
model. It is deliberately separate from presentation and file I/O: callers
supply tokens, choose boundary and symmetry policies, then either apply the
immutable model to `TGraph` or serialize it in the canonical `.wfcm` text
format.

For multi-cell footprints rather than single-token adjacency, see the
separate [overlapping 2D patterns](patterns.md) layer. It builds on the same
tokens and graph adapter while retaining pattern payloads and projection data
that do not belong in the generic `TWfcModel` IR.

This is the first training primitive for the wider ecosystem. A tile, note,
word, voxel label, or other discrete symbol uses the same frequency and
adjacency representation.

## basic use

```pascal
uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_learn,
  wfc_model_text;

var
  Graph: TGraph;
  Model: TWfcModel;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
  Sample: TWfcModelTokens;
  Text: String;
begin
  SetLength(Sample, 9);
  Sample[0] := '~'; Sample[1] := '~'; Sample[2] := '~';
  Sample[3] := '~'; Sample[4] := '.'; Sample[5] := '~';
  Sample[6] := '~'; Sample[7] := '~'; Sample[8] := '~';

  Model := LearnModel2D(Sample, 3, 3, wmbWrap, wmsD4);
  try
    Text := EncodeWfcModelText(Model);

    Graph := TGraph.Create.Reshape(24, 16, 1);
    try
      Graph.Seed := $DEADBEEF;
      Graph.WrapNeighbors := True;
      ApplyModelToGraph(Model, Graph);

      Options := DefaultGraphSolveOptions;
      if not Graph.TrySolve(Options, Report) then
        raise Exception.CreateFmt('learned model failed in pass %d',
          [Report.FailedPassIndex]);
    finally
      Graph.Free;
    end;
  finally
    Model.Free;
  end;
end;
```

`LearnModel1D` is the corresponding sequence API. It constrains east and west
neighbors; directions outside the learned rank remain wildcards when the model
is applied to a graph.

## deterministic observations

The learner accepts pretokenized input. Tokenization is domain policy and stays
outside the core: a text application may provide characters, words, phonemes,
or parser tokens, while a music application may provide pitches, durations,
chords, or combined events.

`WFC_LEARN_ALGORITHM_VERSION = 1` defines these observations:

- values retain first-seen order from the original row-major sample;
- a value weight is its raw occurrence count;
- 1D samples observe radius-one east and west relations;
- 2D samples observe radius-one north, east, south, and west relations;
- open boundaries skip a missing neighbor;
- wrapped boundaries observe the neighbor across the edge, including a
  length-one self-neighbor; and
- counts are positive `Integer` frequencies with checked overflow.

No floating-point probabilities or unordered dictionaries participate in
training. The raw counts are preserved in `TWfcModel`; the solver later applies
its own pass-wide GCD normalization to value weights.

## ordered corpora and model merging

`LearnModel1DCorpus` and `LearnModel2DCorpus` accept an ordered array of
`TWfcLearnSample` records. `MakeLearnSample1D` and `MakeLearnSample2D` create
validated records and detach their token arrays from caller-owned storage.
Samples may have different positive dimensions.

`WFC_LEARN_CORPUS_ALGORITHM_VERSION = 1` adds these rules to the observation
kernel above:

- token order is first occurrence by sample index, then original row-major
  position;
- every sample is an independent topology, so no relation crosses from the end
  of one sample into the beginning of another;
- open or wrapped boundaries and D4 augmentation are applied independently to
  each sample; and
- duplicate samples and symmetric transforms contribute their full raw counts.

The model retains every source shape in corpus order. `SampleCount` and
`SampleShapeAt` are the corpus-aware accessors; the older `SampleWidth` and
`SampleHeight` properties continue to report shape zero for source
compatibility.

`MergeWfcModels` combines already learned compatible models without source
data. Rank, boundary, symmetry, and active directions must match. Tokens keep
their first appearance across the ordered input models, sample-shape lists are
concatenated, and weights and relation counts are checked-added after
reindexing. The merge is deterministic and associative for a fixed flattened
input order, but deliberately not byte-commutative because value order is part
of model identity. Its replay contract is identified by
`WFC_MODEL_MERGE_ALGORITHM_VERSION`.

## D4 augmentation

For a 2D model, `wmsD4` observes eight explicit orientations in this order:

1. identity;
2. rotation by 90 degrees;
3. rotation by 180 degrees;
4. rotation by 270 degrees;
5. X-coordinate mirror (a left/right reflection);
6. that mirror followed by 90 degrees;
7. that mirror followed by 180 degrees; and
8. that mirror followed by 270 degrees.

Every orientation contributes its complete raw value and relation counts.
Rectangular samples are supported; quarter turns exchange width and height.
This is augmentation, not deduplication, so a symmetric input still contributes
all eight observations. Use `wmsNone` when orientation is meaningful.

## immutable model IR

`TWfcModel` stores:

- rank and ordered original sample shapes;
- boundary and symmetry policies;
- the active direction set;
- UTF-8 tokens in deterministic value order;
- raw positive value weights; and
- a flattened matrix of nonnegative directional relation counts.

Construction validates dimensions, nonempty Unicode-scalar token sequences,
count ranges, direction policy, and reciprocal opposite-direction relations.
Constructor inputs are copied, indexed accessors are read-only, and the
explicit copy methods return detached arrays. A caller can therefore share one
model across graph builders and text encoders without exposing mutable
internal arrays.

## graph application and finite support

`ApplyModelToGraph` registers values in model order, transfers raw value
weights, and converts every positive relation count into a Boolean directional
rule. Relation counts remain in the model for inspection and future weighted
relation work; the current graph solver treats adjacency as allowed or denied.

Model directions describe the target from the source (`east` means the target
is east of the source). The historical `TGraph.NewRule` key instead describes
the rule owner's position relative to the entry being validated. In addition,
the original graph API names increasing row coordinates north while the
row-major learner names them south. The adapter bridges both conventions; this
is observable for asymmetric data and is covered by horizontal and vertical
directed-cycle conformance fixtures.

Application requires an otherwise undefined, non-running target pass. The
root-aware `TGraph.Running` property lets the adapter reject callback-time
imports before inspecting or mutating the public rule registry. It then
preflights token conversion, uniqueness, and the complete finite rule arrays
before mutating the graph. Native `TGraphValue` retains the core's
historical host-string representation; if a UTF-8 model token cannot round-trip
through that representation exactly, the adapter rejects it rather than
substituting a lossy value.

Every active model direction is finite. A source row with positive learned
relations becomes an allow-list; a source row with zero support becomes the
graph's explicit `DenyAll` state. Directions outside the model rank remain
wildcards. Tiny open samples can therefore retain edge-only values without
silently broadening the learned model. The dense installation path preserves
model/value order and public rule ordering while installing zero-support rows
only after the finite arrays. `WFC_MODEL_GRAPH_ADAPTER_VERSION = 1` identifies
this observable conversion contract.

## canonical `.wfcm` text

`EncodeWfcModelText` produces an ASCII, LF-only document with a required final
line feed. Metadata has a fixed order, values are written in model order, and
positive relations are written in direction/source/target order. Tokens are
UTF-8 percent encoded with uppercase hexadecimal; only ASCII letters, digits,
`-`, `.`, `_`, and `~` remain unescaped.

The latest reader/writer version is 2, but encoding deliberately selects the
smallest canonical profile. A one-sample model keeps its byte-identical
`wfcm=1` representation. A model with two or more samples uses `wfcm=2`, which
replaces the singular width and height with ordered shape records:

```text
wfcm=2
rank=1
samples=2
s=0,2,1
s=1,3,1
boundary=open
symmetry=none
directions=E,W
values=3
v=0,1,A
v=1,3,B
v=2,1,C
relations=6
r=E,0,1,1
r=E,1,2,1
r=E,2,1,1
r=W,1,0,1
r=W,1,2,1
r=W,2,1,1
end
```

Version 2 is noncanonical for a singleton corpus and is rejected. Shape
indices must be complete and ordered, dimensions must be positive, and the
declared sample count must match the records.

```text
wfcm=1
rank=1
width=3
height=1
boundary=wrap
symmetry=none
directions=E,W
values=2
v=0,2,A
v=1,1,B
relations=6
r=E,0,0,1
r=E,0,1,1
r=E,1,0,1
r=W,0,0,1
r=W,0,1,1
r=W,1,0,1
end
```

`DecodeWfcModelText` is intentionally strict. It rejects CRLF, reordered or
unknown fields, noncanonical decimals or percent escapes, invalid UTF-8,
duplicate indices, malformed counts, missing final LF, and trailing data. A
successfully decoded document must re-encode byte-for-byte to its input. This
makes the text itself suitable for diffs, fixtures, signatures, and model
identity without a second normalization step.

## replay identity

A saved canonical model fully captures the learned value and relation data.
Recreating that model from training input additionally requires:

- `WFC_LEARN_ALGORITHM_VERSION`;
- `WFC_LEARN_CORPUS_ALGORITHM_VERSION` for multiple samples;
- the exact ordered UTF-8 token sequence and dimensions of every sample;
- boundary and symmetry policies;
- `WFC_MODEL_MERGE_ALGORITHM_VERSION` when models were merged;
- `WFC_MODEL_GRAPH_ADAPTER_VERSION` when applying the model to `TGraph`; and
- the selected model-text profile when comparing serialized bytes.

Solving still uses the replay identity documented in
[deterministic generation](determinism.md), including seed, pass index, graph
topology, solve options, and solver/random algorithm versions.

## current scope

The current learner handles ordered heterogeneous pretokenized corpora of
single-layer 1D or 2D samples with cardinal radius-one relations. The separate
pattern layer extracts overlapping multi-cell 2D footprints, and the sequence
layer learns bounded order-N latent states from pretokenized UTF-8 corpora.
Sequence samples reset a typed BOS history and add no cross-sample seam, while
exact suffix/prefix compatibility intentionally permits recombination. These
generic learners retain their explicit token boundary; the specialized
[text foundation](text.md) now converts raw in-memory documents with a
project-owned Unicode-scalar tokenizer or caller callback. The ecosystem does
not yet stream raw files, learn 3D neighborhoods, smooth unseen relations,
attach provenance or semantic tags, or implement a probabilistic language
model. Those are deliberate extension points built on stable explicit IR
rather than hidden behavior.
