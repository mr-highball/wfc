# 2D world ecosystem

`wfc_world2d` is the first domain library built on the core pass system. It
owns a `TGraph` configured as a bounded depth-one world with three typed passes:

```text
terrain -> biome -> foliage
```

`wfc_world2d_validate` is a separate semantic validator. It does not inspect
the graph's rule tables. Instead, it independently checks every symbol,
same-coordinate layer relationship, and cardinal adjacency in the generated
world. A model-configuration defect therefore cannot validate itself merely
because solving used the same defective table.

Both units use the same source on FPC 3.2.2 and pas2js. Conformance runs under
Node.js, and `BrowserWorld` compiles the same model and validator to a real
interactive browser host with a separate headless-browser fixture.

## standard model

The versioned model is intentionally small enough to understand at a glance
while exercising real cross-pass constraints.

| Layer | Values | Previous-pass relationship |
| --- | --- | --- |
| terrain | `water`, `land`, `mountain` | none |
| biome | `ocean`, `shore`, `plains`, `woodland`, `alpine` | ocean requires water; shore/plains/woodland require land; alpine requires mountain |
| foliage | `none`, `reeds`, `grass`, `tree`, `pine` | none accepts every biome; the other values require shore, plains, woodland, and alpine respectively |

Terrain never places water directly beside mountain. Biome adjacency mirrors
the terrain transitions, so any valid terrain result has at least one valid
biome refinement. Foliage is unconstrained spatially in model version 1, but
its previous-pass filters make trees, reeds, grass, and pine biome-specific.

Foliage only needs to read biome because the biome vocabulary carries forward
the terrain category. This is a useful pattern for a linear pass system: each
layer exposes the information its immediate successor needs without coupling a
late domain directly to every earlier pass.

## basic use

```pascal
uses
  wfc,
  wfc_world2d,
  wfc_world2d_validate;

var
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
  Validation: TWorld2DValidationReport;
  World: TWorld2D;
begin
  World := TWorld2D.Create(32, 14);
  try
    World.Seed := 42;
    World
      .Lock(w2lTerrain, 0, 0, WFC_WORLD2D_TERRAIN_WATER)
      .Lock(w2lBiome, 0, 0, WFC_WORLD2D_BIOME_OCEAN);

    Options := DefaultGraphSolveOptions;
    Options.MaxBacktracks := 4096;
    if not World.TryGenerate(Options, Report) then
      raise EWorld2D.Create('world generation failed');
    if not ValidateWorld2D(World, Validation) then
      raise EWorld2D.Create(
        DescribeWorld2DValidationIssue(Validation.Issue));
  finally
    World.Free;
  end;
end;
```

The two-argument constructor uses `DefaultWorld2DConfig`: seed `0` and bounded
edges. The overload accepting `TWorld2DConfig` applies its seed before pass
materialization. This makes the default useful in examples and tests without
depending on an automatically captured seed.

`TWorld2D` owns its `Graph`; callers must not free that graph. It is exposed for
advanced rule and pass experimentation. The standard validator and signatures
deliberately require exactly the three standard passes, matching shapes, and
depth one. Once an application extends the pipeline, it should also define a
new model version and its own validator/signature contract.

## locks and atomic failure

`Lock` checks coordinates and verifies that the symbol belongs to the selected
layer. It permits a known but contextually impossible lock, such as `tree` on a
cell whose biome is `ocean`. That is model input rather than malformed API use,
so `TryGenerate` returns `False` with the normal structured contradiction
report.

`ClearLock` clears caller-owned state. Calling it on generated output is a
no-op because generated values are already eligible for replacement on the
next solve.

The wrapper delegates to the core atomic `TrySolve` pipeline. If foliage fails,
terrain and biome are not partially regenerated; all pre-call entry states,
the selected pass, and random streams remain intact. The focused conformance
suite locks a tree over an ocean cell, verifies the failure is attributed to
the foliage previous-pass constraint, checks earlier layer signatures, then
clears the lock and reproduces the original complete signature.

## validation reports

`ValidateWorld2D` stops at the first issue and returns its layer, coordinate,
value, related coordinate/value, and issue kind. It distinguishes:

- nonstandard pipeline shape;
- empty or unknown layer values;
- illegal terrain adjacency;
- biome-to-terrain mismatch;
- illegal biome adjacency; and
- foliage-to-biome mismatch.

The validator computes cardinal neighbor coordinates from dimensions and the
wrapping setting instead of following mutable entry links. It checks all three
layers in stable `Y`, then `X`, order and records how many cells and relations
were examined.

## selective-settlement specialization

`wfc_world2d_settlement` builds a second, independent 2D domain on the DAG
coordinator:

```text
terrain ──> hydrology ──┐
    └────> biome ───────┼─> roads ─> housing ─> foliage
```

Roads, housing, and foliage also retain direct dependencies on every earlier
layer whose value they inspect. All six passes use `gpmOverlay`, so each owns a
separate vocabulary rather than copying its input. `RequireFromPass` expresses
same-coordinate context: a bridge requires land, river hydrology, and a
plains/woodland biome; a house requires land, dry hydrology, plains, and a
trail. Alternatives from one source are OR, while requirements from different
sources are AND.

`wfc_world2d_settlement_validate` independently checks the canonical DAG,
shape, symbols, cross-layer relationships, and local adjacency. It does not
consult the configured rule groups. `TSettlement2D.TryRegenerateFrom` exposes
transactional descendant-only regeneration. The demonstration changes one
hydrology lock and executes hydrology, roads, housing, and foliage while
preserving the sibling biome and upstream terrain exactly; clearing the lock
recovers the baseline signature. An intentionally impossible dependent lock
proves that failure restores values, empty state, ownership, selection, and
random streams.

The settlement signature uses project-owned CRC-32 code and six fixed-token
layer streams. It formats:

```text
<signature-version>:<terrain>:<hydrology>:<biome>:<roads>:<housing>:<foliage>
```

See the [selective-settlement example](../examples/2D/03_SelectiveSettlement/README.md)
and the [pass-DAG contract](pass-dags.md).

## portable signatures

`LayerSignature` returns a `Cardinal` CRC-32 checksum, and
`PipelineSignature` formats the three layer checksums as:

```text
<signature-version>:<terrain>:<biome>:<foliage>
```

Each component is eight uppercase hexadecimal digits. Signature version 1
uses the reflected CRC-32 polynomial `$EDB88320`, an initial state of
`$FFFFFFFF`, and a final bitwise complement. Its canonical byte stream is:

1. ASCII `WFC2`;
2. model version, signature version, layer ordinal, width, and height as
   little-endian 32-bit values; and
3. one fixed byte token per cell in `Y`, then `X`, order.

Tokens are assigned by the model, not derived from compiler string memory:
terrain uses `$10..$12`, biome `$20..$24`, and foliage `$30..$34`. Empty cells,
unknown symbols, extra passes, and mismatched shapes are rejected rather than
silently assigned unstable hashes. This is why native FPC and pas2js produce
the same signature even though their internal string representations differ.

The checksum identifies output, not the solver's future decision-trace hash,
and it is not cryptographic. Persist the core random and solver algorithm
versions, seed, options, world model version, and signature version alongside
results intended for long-term replay.

## browser presentation

[`BrowserWorld`](../examples/2D/02_BrowserWorld/README.md) presents terrain,
biome, and foliage as synchronized canvases. Its controls select the seed and
edge policy, generate the next seed, inspect one coordinate across every
layer, and add or clear caller-owned locks. Impossible locks surface as
contradictions through the same atomic pipeline instead of silently replacing
the last valid output.

The browser and console hosts share `world2d_showcase`, including dimensions,
anchors, default seed, and golden signatures. Pascal remains the source of
truth for solving, validation, signatures, rendering behavior, and event
handling; the static HTML and CSS only define the document and presentation.

Run `build-browser.ps1` or `build-browser.sh` from the repository root to stage
the site under `build/browser/world2d/www`. Appending `?selftest=1` runs the
seed-zero browser fixture. A successful run exposes this exact state on the
document body:

```text
data-state="solved"
data-self-test="passed"
data-signature="1:5B0DD75D:08022AF1:A40D0955"
```

The hosted browser gate checks those attributes after executing the generated
program in headless Chrome. This is deliberately distinct from Node.js
conformance: it covers the browser target, document binding, canvas host,
validator, and portable signature together.

## demonstration and conformance

[`MultiPassWorld`](../examples/2D/01_MultiPassWorld/README.md) renders all three
layers side by side, prints solver counters, validates the result, and accepts
an optional seed. The same program is compiled and smoke-tested natively and
with pas2js/Node.

`test/wfc_world2d_test.lpr` freezes the seed `$4D505731` fixture as:

```text
1:F7B994F3:E2E8E15B:21B79A66
```

The suite covers typed guards, complete semantic validation, same-instance and
independent-instance replay, pass-selection restoration, illegal-lock rollback,
recovery, direct corruption detection, incomplete-output rejection, and the
rule that canonical signatures cannot ignore appended passes.
