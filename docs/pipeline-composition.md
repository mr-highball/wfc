# Composing pipeline fragments

`wfc_pipeline_compose` assembles independently authored or learned pipeline
recipes into one immutable recipe. It is project-owned MIT Pascal for FPC and
pas2js. It does not load files, fetch resources, train models, allocate a graph,
run a solver, or depend on a presentation framework.

Composition is a library operation, not an editable workspace or saved-operation
journal. Use [prepared sessions](pipeline-sessions.md) for in-memory input edits
and revision-bound repair of its output recipe. Its output uses the existing [portable artifact workflow](pipeline-artifacts.md)
and [mapped pass layouts](portable-mapped-passes.md). The unit is included in the
[FPM and Lazarus runtime packages](building.md#fpm-package).

## Explicit assembly

Each `TWfcPipelineFragmentInput` supplies:

- A unique `FragmentId`, retained as the fragment's local namespace.
- Complete canonical `RecipeText`, including its source metadata and versions.
- One final `ResourceIds` entry for every resource, in source order.
- One final `PassLabels` entry for every pass, including private passes and aliases.

Names are explicit, nonempty and case-sensitive. Final resource IDs must be
unique across all fragments, as must final pass labels. A resource ID and pass
label may coincide because they belong to different registries. The composer
never guesses names or deduplicates two resources with equal document bytes.

For example, given two already-owned recipes, the caller can supply complete
name vectors without knowing their internal dependency indices:

```pascal
uses SysUtils, wfc_model, wfc_pipeline_model, wfc_pipeline_text,
  wfc_pipeline_compose;

function Fragment(const Id: TWfcModelToken;
  const Recipe: TWfcPipelineModel): TWfcPipelineFragmentInput;
var I: Integer;
begin
  Result.FragmentId := Id;
  Result.RecipeText := EncodeWfcPipelineModelText(Recipe);
  Result.ResourceIds := nil;
  Result.PassLabels := nil;
  SetLength(Result.ResourceIds, Recipe.ResourceCount);
  SetLength(Result.PassLabels, Recipe.PassCount);
  for I := 0 to Recipe.ResourceCount - 1 do
    Result.ResourceIds[I] := Id + '/resource/' + TWfcModelToken(IntToStr(I));
  for I := 0 to Recipe.PassCount - 1 do
    Result.PassLabels[I] := Id + '/pass/' + TWfcModelToken(IntToStr(I));
end;

{ TerrainRecipe and FoliageRecipe are complete, validated source recipes. }
SetLength(Fragments, 2);
Fragments[0] := Fragment('terrain', TerrainRecipe);
Fragments[1] := Fragment('foliage', FoliageRecipe);
Composition := TWfcPipelineComposition.Create(
  MakeWfcPipelineMetadata('landscape', 'MIT', 'explicit fragment assembly', ''),
  Fragments);
try
  CombinedText := Composition.RecipeText;
  TerrainPass := Composition.ResolvePass('terrain', 'public');
  FoliagePass := Composition.ResolvePass('foliage', 'public');
finally
  Composition.Free;
end;
```

The two `ResolvePass` calls assume each source actually named a pass `public`;
use its real original label. `ResolveResource` likewise accepts an original
resource ID, not the caller's final renamed ID. Unknown names raise
`EWfcPipelineCompose`. No name is inferred from the sample's contents.

`MapIndex(FragmentId, Section, LocalIndex)` provides the same mapping for all
seven stored row families: resources, passes, dependencies, bridges,
requirements, value quotas and connectivity. A section is a row registry,
not a solver execution stage. Local indices are zero-based; a sentinel or an
index into an empty registry is rejected.

## What changes, and what stays identical

Rows are appended in fragment order and source-local order. All resource and
pass references are remapped, including forward transform sources, both ends
of dependencies and bridges, requirement endpoints, and quota/connectivity
owners. A constraint attached to a public alias remains attached to that alias;
assembly does not lower it onto a private or materialized owner.

Internal resource documents, training provenance and license declarations are
preserved byte-for-byte. Tokens, pattern footprints, private latent keys,
sequence extents, mapped offsets, count semantics, connectivity positions and
all per-pass topologies keep their original meaning. Original canonical source
texts remain available through `FragmentAt` even when their metadata does not
become the combined recipe's top-level metadata. An MIT label on the combined
recipe does not relicense an imported resource.

The output is deliberately an explicit spatial recipe5, even when every input
uses legacy uniform topology. Its requested cell extents are still a separate
run decision. Use `TWfcPipelineRun` and the normal compiler/runtime to execute
it; assembly does not allocate a maximum-size grid or pad smaller passes.
Actual bridge/copy layout compatibility is checked when extents are supplied.

## Cross-fragment constraints remain deliberate

Concatenating a terrain fragment and a foliage fragment does not automatically
make foliage depend on terrain. After assembly, resolve their desired public
passes, then construct a new immutable recipe with the explicit dependency
and requirement rows using the existing typed model constructors. Preserve the
other copied rows and versions. Mapped requirements support different grids
in their shared integer world; matching token names alone establishes no
relationship. This component does not yet provide an interactive link editor.

Every original algorithm pin must agree between fragments, including otherwise
unused legacy bridge pins. Pattern3D pins must agree among fragments that
actually use that feature. Global run mode must also agree. There is no silent
version upgrade or chosen compromise between conflicting traversal modes.

An appended fragment cannot have a `gpmLegacy` root. Moving local pass zero to
a later global index can give it a different implicit predecessor. The composer
rejects this case even for a defined root; it does not rewrite its mode or add
an edge. The first fragment may retain a legacy root at global zero. Internal
legacy copies retain their local predecessor, and an explicit transform root
can safely retain a later source within its own fragment.

Identical ordered inputs produce identical complete combined recipe bytes.
This is not a promise that an embedded fragment produces the same generated
output as when run alone with the same seed: stable global pass indices
participate in the core's random-stream identity.

## Ownership and validation

The constructor borrows inputs only for the call. The owner retains detached
name arrays and source texts and owns the combined immutable recipe. Returned
`FragmentAt` arrays are detached. `BorrowRecipe` transfers no ownership: do not
free it separately or retain it after freeing the composition. `RecipeText`
is a Pascal string value and may outlive the owner.

Raw container shape, dense passive string-array slots, names, text lengths and
aggregate input budgets are checked before decoding any source. Each source
then passes the existing closed decoder and exact canonical re-encoding check.
Decoded aggregate relation/policy budgets are checked before concatenated
output rows and final model construction. They do not precede the bounded
decoding needed to discover each source's typed resource size.

Malformed inputs, name collisions, unsupported versions, unsafe relocation and
resource-envelope excesses raise `EWfcPipelineCompose` with context. Allocation
failure retains its original exception type. An unsuccessful replacement
construction cannot change an already-owned composition. JavaScript guards
reject ordinary property getters and sparse arrays; they are not a sandbox
against hostile Proxy traps.

These envelopes protect finite artifact processing. They are not a desired
music duration, fixed map size, or claim of unlimited host memory. Assembly
does not itself implement mutable edits or selective-repair ownership;
[preparation](pipeline-preparation.md) and [sessions](pipeline-sessions.md) supply
those separately. Saved operation journals and resumable workspace execution
remain future work.

## Executable examples and verification

`test/wfc_pipeline_compose_test.lpr` demonstrates explicit naming, all seven
remapped row families, ownership, incompatible versions, unsafe root relocation,
and strict JavaScript input boundaries. `test/wfc_pipeline_compose_resources_test.lpr`
uses actual learned Pattern2D, bounded/wrapped Sequence, and Pattern3D fragments,
including aliases, inverse public constraints, mapped consumers, quotas,
connectivity, two unlike-grid extent sets, and exact result replay.

Both are registered with the maintained native build and discovered by the
portable browser conformance scripts. The [package consumer](package-checking.md)
also executes composition using installed units, without a repository source
fallback. These fixtures establish correctness for their modeled cases, not
universal solvability, minimum-edit repair, or completion of the broader workspace.
