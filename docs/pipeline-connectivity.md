# Portable rooted connectivity

A saved recipe can require a connected road network or a route from a ground
entrance to upstairs rooms. The constraint survives canonical export, decoding,
and execution in native FPC and pas2js. It needs no callback or external library.

`WFC_PIPELINE_CONNECTIVITY_VERSION = 1` carries the existing
[rooted reciprocal-port semantics](connectivity.md) in an immutable recipe.
This is a reachability constraint, not shortest-path or minimum-road optimization.

## Authoring and ownership

`TWfcPipelineConnectivity` contains a public `PassIndex`, nonempty `LabelText`,
explicit XYZ `Root`, `RequiredPositions`, participating `Values`, and
`RequireAllParticipants`. Each `TWfcPipelineConnectivityValue` contains a
UTF-8 public `Value`, directional `Openings`, and `RequiredByValue`.

Root and fixed terminals must participate and reach the root component.
Selected required-by-value profiles must also reach it. With all-participants
enabled, every selected profile must reach it; otherwise optional disconnected
islands remain legal. Unprofiled values do not participate. An empty opening
set means an isolated participant, not a wildcard. Edges require reciprocal
ports, reciprocal physical neighbors, and mutually permitted local adjacency.

Profiles use strict public-vocabulary order. Terminals use strict `(Z,Y,X)`
order with no duplicates and must not repeat the root. Coordinates are exact
nonnegative integers through `High(Integer)`; rank 1 requires Y/Z zero and
rank 2 requires Z zero. The compiler later checks them against the invocation's
shape. Anchors never wrap or clamp into a smaller grid. All six port directions
are accepted at every rank; singleton wrapped axes cannot connect two distinct
cells through a self-loop.

Each `(PassIndex, LabelText)` is unique within the connectivity registry.
Different descriptors are conjunctive and may describe separate networks on
the same pass. Quota labels are a separate namespace. Exact-copy public aliases
are valid owners. Private state keys are not a portable authoring surface.

The new constructor overload appends quotas and connectivities, with or without
an explicit versions record. Existing constructors remain unchanged. To extend
an existing recipe whose public pass 1 contains `road`:

```pascal
Root := Default(TGraphPosition);
SetLength(Terminals, 1);
Terminals[0] := Root;
Terminals[0].X := 4;
SetLength(Profiles, 1);
Profiles[0] := MakeWfcPipelineConnectivityValue('road', [gdEast, gdWest]);
Connections := Base.CopyConnectivities;
SetLength(Connections, Length(Connections) + 1);
Connections[High(Connections)] := MakeWfcPipelineConnectivity(
  1, 'entrance-to-exit', Root, Terminals, Profiles, True);
Recipe := TWfcPipelineModel.Create(Base.CopyMetadata, Base.CopyVersions,
  Base.Rank, Base.WrapNeighbors, Base.RunMode, Base.CopyResources,
  Base.CopyPasses, Base.CopyDependencies, Base.CopyBridges,
  Base.CopyRequirements, Base.CopyValueQuotas, Connections);
```

This is an authoring excerpt, not a complete program. The
[Connected Routes portable unit](../examples/passes/06_ConnectedRoutes/connected_routes_portable.pas)
is a complete recipe builder. Constructors and inspection accessors deeply
copy arrays and port sets. `ConnectivityAt`, `CopyConnectivities`, and
`ConnectivityCount` expose detached metadata; `ConnectivityVersion` is zero
for an empty registry and one otherwise.

## Private search and public validation

The compiler resolves exact-copy aliases without giving them local definitions.
It installs the descriptor on the materialized public owner. For a projected
owner it also installs an equivalent descriptor on the private source:

- A sequence state's emitted token determines participation, ports, and its
  required-by-value flag. Different histories retain separate solver states.
- A wrapped 2D pattern uses its `(0,0)` public anchor token, not every footprint
  occurrence. Consistent overlaps preserve the public coordinate relation.

This lowering happens before solving and works with both bridge versions.
Bridge-version-1 per-cell locks remain forward-only; connectivity does not
silently upgrade that separate contract. Internal descriptor labels use recipe
ordinals so identically named constraints on different aliases do not collide.

Search retains the core's propagation, backtracking, selective repair horizons,
negotiation, and rollback. A separate public traversal checks the immutable
recipe against committed candidate entries. Clearing the compiled graph's
mutable connectivity registry cannot bypass that publication check.

Solved result construction and decoding perform the public traversal again,
without a live graph or solver reachability cache. The checker reads immutable
rule/model adjacency; public projection passes have neutral local adjacency.
This specifically validates connectivity, not every other adjacency, projection,
or cross-pass constraint in an untrusted result.

Compile failures identify `wpcsConnectivity` and a recipe descriptor index.
Commit diagnostics expose `wpcvkConnectivity`, `ConnectivityIndex`, the declared
public pass, and a failed entry. Core search uses `gckConnectivity`, possibly on
the private source. Canonical result text spells the failure `connectivity`;
its existing failure envelope does not carry descriptor ordinals. Trace
ordinals belong to each compiled pass's local registry, not directly to the
recipe-wide registry.

## Canonical format

Connectivity-bearing recipes encode as `wfcpipeline=3`. The existing quota
section is always present: version/count `0/0` when empty, or version `1` with
a positive count. The connectivity section follows it before the signature:

```text
value-quota-version=0
value-quotas=0
connectivity-version=1
connectivities=1
connectivity=0,1,entrance-to-exit,0,0,0,true,1,1
terminal=0,0,4,0,0
profile=0,0,road,10,false
```

The descriptor fields are ordinal, pass, escaped label, root XYZ,
all-participants, terminal count, and profile count. Terminal rows carry parent
ordinal, child ordinal, XYZ. Profile rows carry parent ordinal, child ordinal,
escaped token, opening mask, and required-by-value. Mask bits are north 1,
east 2, south 4, west 8, up 16, down 32. Booleans are exactly `true` or `false`.
This excerpt needs the full resource/pass context and recomputed signature.

Version 3 requires nonempty connectivity. Recipes with neither extension retain
exact version-1 bytes; quota-only recipes retain exact version-2 bytes. The
base model/version record is unchanged. Only a nonempty connectivity registry
appends a `wfcpipeline-connectivity` semantic-hash domain with portable/core
capabilities and every ordered descriptor field. Run/result envelopes remain
version 1, bound to that recipe signature. Both included CLI tools accept
recipe versions 1, 2, and 3.

## Resource limits and verification

The format allows 4,096 descriptors, 1,024 profiles per descriptor, 65,536
aggregate profiles, and 65,536 terminals both per descriptor and in aggregate.
Labels and profile tokens share the existing 16 MiB outer-token budget and
1 MiB per-token ceiling. The complete text remains bounded to 256 MiB.
Version 3 permits 291,422 lines; older versions retain their original limits.
Version, counts, remaining records, canonical numbers, and encoded budgets
are checked before descriptor-driven allocations or nested decoding.

Compilation/public checking preflights 16,777,216 aggregate descriptor-cell
visits and 16,777,216 aggregate model visits (vocabularies, profiles, terminals,
rule rows, and latent states). These are portable artifact work limits, not a
claim of bounded solver time or infinite storage. No-connectivity invocations
skip the added traversal and work checks.

The normal native and browser gates include model ownership/identity tests,
hostile codec tests, and integration fixtures covering aliases, private
projections, quotas, wrapping, 3D directions, repair, and forged result
rejection. Connected Routes adds native artifact export/replay and an actual
browser-entry proof. See its [commands](../examples/passes/06_ConnectedRoutes/README.md).

Connectivity profiles are explicitly authored. Training Studio does not yet
infer or edit them, and this feature does not export an arbitrary live graph.
The existing graph propagator's finite evidence and limitations still apply;
this format introduces no stronger propagation or relative-speed claim.
The [representation and verification record](research/portable-connectivity-v1.md)
states the mapping argument, fixture scope, and stopping rule.
