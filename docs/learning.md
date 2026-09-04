# model learning and priming

The learning layer turns a tokenized example into a portable WFC model. It is
deliberately separate from presentation and file I/O: callers supply tokens,
choose boundary and symmetry policies, then either apply the immutable model
to `TGraph` or serialize it in the canonical `.wfcm` text format.

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

- rank and original sample dimensions;
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

## graph application and the empty-support boundary

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

Application requires an otherwise undefined target pass. It also preflights
token conversion and every source value in every active direction. Native
`TGraphValue` retains the core's historical host-string representation; if a
UTF-8 model token cannot round-trip through that representation exactly, the
adapter rejects it before mutation rather than substituting a lossy value. The
current `TGraph` rule contract also treats both an absent rule and an empty
rule list as a wildcard, so a learned row with zero support cannot yet be
represented as “allow nothing.” Such a model is valid IR, but graph application
raises an exception containing `empty-support-not-representable` instead of
silently broadening it.

This commonly matters for tiny open samples whose value appears only on one
edge. Wrapping the training sample, adding representative examples, or waiting
for a future explicit deny-all rule are honest remedies; converting the empty
row to a wildcard is not.

## canonical `.wfcm` text

`EncodeWfcModelText` produces an ASCII, LF-only document with a required final
line feed. Metadata has a fixed order, values are written in model order, and
positive relations are written in direction/source/target order. Tokens are
UTF-8 percent encoded with uppercase hexadecimal; only ASCII letters, digits,
`-`, `.`, `_`, and `~` remain unescaped.

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
- the exact ordered UTF-8 token sequence and original dimensions;
- boundary and symmetry policies; and
- the model-text version when comparing serialized bytes.

Solving still uses the replay identity documented in
[deterministic generation](determinism.md), including seed, pass index, graph
topology, solve options, and solver/random algorithm versions.

## current scope

Version 1 learns one pretokenized, single-layer 1D or 2D sample with cardinal
radius-one relations. It does not yet tokenize raw files, merge corpora,
extract overlapping multi-cell patterns, learn 3D neighborhoods, smooth unseen
relations, attach semantic tags, or train cross-pass predicates. Those are
deliberate extension points built on the stable IR rather than hidden behavior.
