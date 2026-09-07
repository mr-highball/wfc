# Connectivity in training sources

Training documents can carry explicit reachability requirements alongside
examples and [value quotas](training-value-quotas.md). Examples teach local
adjacency and weights. Connectivity says which public tokens participate,
which world-axis directions they connect through, and which output positions
must belong to a root's connected component. It is authored policy, not a
property inferred from sample frequency or appearance.

## Try it

Open [Training Studio](../examples/learning/05_TrainingStudio/README.md) and
load its route corpus demonstration. The corpus admits the local combinations
of road and grass; the authored network requires a path across a 4-by-3 output.
An accompanying quota allows exactly six road cells. Generation must satisfy
both requirements, not merely produce locally plausible tiles.

The connectivity editor supports named networks, a root XYZ position,
fixed terminal XYZ positions, per-token participation and six directional
ports, required-by-value tokens, and an all-participants option. Direction
pairs are north/south, east/west, and up/down. A connection requires reciprocal
ports and mutually allowed local adjacency. An included token with no ports
is an isolated participant; an excluded token does not participate.

Root and fixed terminals must participate and reach the root. Required-by-value
tokens must reach it wherever they occur. All-participants additionally requires
every selected participating token to reach it; otherwise optional disconnected
islands are legal. These are reachability requirements, not shortest-path
optimization. The route demo's six-cell quota supplies a separate quantity rule.

Apply an editor draft before solving or exporting derived artifacts. Quota and
network drafts cannot overwrite one another. Changing a policy invalidates the
old run/result, and an invalid edit never leaves an old success available.
Discarding a draft does not resurrect a previous result. Source changes also
invalidate pending asynchronous file imports.

Download the `.wfclearn` source to preserve examples **and** these policies.
Loading it and training again reconstructs them. Save the recipe and run when
you need executable policy and exact invocation settings as separate artifacts.

## Pascal API

`TWfcTrainingConnectivity` owns `LabelText`, `Root`, `RequiredPositions`,
`Values`, and `RequireAllParticipants`. Each `TWfcTrainingConnectivityValue`
contains a public `Value`, `Openings: TGraphDirections`, and `RequiredByValue`.
There is no pass index: the learner resolves the recipe's public `output` owner.

This excerpt extends an existing training document named `Base`:

```pascal
Root := Default(TGraphPosition);
SetLength(Terminals, 1);
Terminals[0] := Root;
Terminals[0].X := 3;
Terminals[0].Y := 2;
SetLength(Profiles, 1);
Profiles[0] := MakeWfcTrainingConnectivityValue('road',
  [gdNorth, gdEast, gdSouth, gdWest]);
SetLength(Networks, 1);
Networks[0] := MakeWfcTrainingConnectivity('roads', Root,
  Terminals, Profiles, True);
Document := TWfcTrainingDocument.Create(Base.CopyMetadata,
  Base.CopyOptions, Base.CopySamples, Base.CopyValueQuotas, Networks);
Recipe := LearnWfcTrainingRecipe(Document);
```

The original constructors remain unchanged. The new overload appends quotas
and connectivities; an empty connectivity array preserves the earlier behavior.
`ConnectivityCount`, `ConnectivityVersion`, `ConnectivityAt`, and
`CopyConnectivities` provide detached inspection. Capability is zero when empty,
one when nonempty. Makers and owners copy dynamic arrays and directional sets.

Labels are nonempty and unique within the connectivity registry; quota labels
form a separate namespace. Profile tokens are unique and retain authored order
in source text and source identity. They must appear in the samples and the
learned public vocabulary. Lowering resolves the strings and reorders profiles
into the learned vocabulary's actual order; an authored index is never treated
as a learned state ID. Sequence/pattern private states remain implementation
details, using the [portable recipe mapping](pipeline-connectivity.md).

Fixed terminals use strict `(Z,Y,X)` order, without duplicates or a repeated
root. The editor sorts coordinate entries before saving; the low-level API and
canonical decoder reject unsorted records. Coordinates are exact nonnegative
integers through `High(Integer)`. Rank one requires Y/Z zero; rank two requires
Z zero. Anchors describe the future output, not a training sample's dimensions.
An invocation that cannot contain them is rejected; positions never wrap,
clamp, or relocate. All six port directions are accepted at every rank, retaining
the existing reciprocal physical-neighbor and singleton-axis semantics.

Sample D4/cube augmentation transforms sample positions, not authored token
meanings. Ports remain world-axis directions. A directional tile vocabulary
needs explicit token/port semantics; this extension does not infer a rotation
mapping between token names.

## Workspace edits and exports

`TWfcTrainingWorkspace.ReplaceConnectivities(Networks)` updates canonical
`SourceText` and retrains. It preserves the source's quotas; `ReplaceValueQuotas`
symmetrically preserves connectivity. `CopyConnectivities` and
`ConnectivityCount` expose the trained registry without exposing owned objects.

Invalid records or source-size rejection retain the preceding editable source
but clear recipe/run/result. A new valid canonical draft is published before
learning; if learning then fails, that requested source remains editable, still
without derived artifacts. A policy-edit failure therefore cannot silently
revert the source to an older successful policy.

Standalone adjacency, pattern, and sequence models cannot retain these output
requirements. `LearnWfcTrainingModelText`, workspace `ModelText`, and native
`--model` reject sources with connectivity or quotas. Export the complete recipe,
or explicitly remove the policies before requesting an unconstrained model.

Native entry points include:

```text
wfc_learn authored.wfclearn
wfc_learn --quiet authored.wfclearn
TrainingStudio --connectivity-demo
TrainingStudio --connectivity-selftest
```

Add `.exe` on Windows. No extra learner switch is required for the source format.

## Canonical source version 4

Only a nonempty connectivity registry selects `wfclearn=4`. Quota-only sources
remain version 3; unconstrained volumes remain version 2; other unconstrained
kinds remain version 1. Their earlier bytes and fingerprints are unchanged.
Removing the last network restores the appropriate earlier format.

Version 4 uses the depth-bearing sample rows from version 3 for every kind.
After all samples/tokens, it always contains the quota section: capability/count
`0/0` when empty, otherwise capability `1` and a nonempty registry. Connectivity
follows, then the existing `end` marker. For example, this section describes a
network without a quota (it is not a complete document):

```text
value-quota-version=0
value-quotas=0
connectivity-version=1
connectivities=1
connectivity=0,roads,0,0,0,true,1,1
terminal=0,0,3,2,0
profile=0,0,road,15,false
```

A descriptor carries ordinal, escaped label, root XYZ, all-participants,
terminal count, and profile count. Terminal rows carry parent/child ordinals and
XYZ. Profile rows carry parent/child ordinals, escaped public token, opening mask,
and required-by-value. Mask bits are north 1, east 2, south 4, west 8, up 16,
down 32. Booleans are exactly `true` or `false`. Decimal indices, escaped UTF-8
tokens, record ordering, LF endings, and the final newline are canonical.

The immutable source fingerprint uses the `wfclearn-v4` domain and includes
the existing metadata/options/samples, optional quota policy, and every ordered
network field. Generated provenance uses `wfclearn-v4/<fingerprint>`. The learned
resource payload remains unchanged when only authored policy changes. The recipe
adds the equivalent public policy and encodes as `wfcpipeline=3`; run/result
envelopes remain bound to that recipe identity.

## Limits and verification scope

The source permits 4,096 networks, 1,024 profiles per network, 65,536 aggregate
profiles, and 65,536 fixed terminals per network and in aggregate. Network labels
and profile tokens share the existing 64 KiB per encoded token and 4 MiB total
encoded-token budget with metadata, samples, and quotas. Complete source text
remains limited to 8 MiB. Versions 1/2 retain their 69,643-line envelope; version
3 retains 139,277; version 4 permits 274,447. Count/record/byte preflights precede
descriptor-driven allocations. These limits do not promise bounded solver time.

The Studio's visible interactive envelope is unchanged. Native/low-level users
retain their separately documented envelopes. Tests cover all five training
kinds, ownership, profile reordering, old-format compatibility, malformed source,
policy coexistence, workspace failure invalidation, CLI export refusal, native
replay, and real browser editor behavior. The underlying reachability guarantees
remain those of [portable connectivity](pipeline-connectivity.md); no stronger
propagation or speed claim is introduced here.

General multi-resource recipe editing, inferred tile-orientation semantics,
soft connectivity preferences, and chunk-boundary summaries remain separate
work. See the [source-policy research record](research/training-connectivity-v1.md)
for the distinction between learned evidence and authored requirements.
