# SelectiveSettlement

`SelectiveSettlement.lpr` is a dependency-DAG and selective-regeneration
showcase built from the project-owned Pascal core. The same source runs under
native Free Pascal, uses no external library or asset, and
builds a 32 by 14 world with six independently committed overlay layers:

```text
terrain ─────┬─> hydrology ─┬─> roads ─> housing ─> foliage
             │              │      ▲         ▲          ▲
             └─> biome ─────┘      │         │          │
                    └───────────────┘         │          │
terrain ─────────────────────────────────────┘          │
hydrology + biome + roads ──────────────────────────────┘
```

The stable creation and execution order is `terrain`, `hydrology`, `biome`,
`roads`, `housing`, `foliage`. `hydrology` and `biome` are siblings even
though biome has the later index. This detail makes the counterfactual edit a
real DAG demonstration rather than a suffix of a linear pass list.

## layer model

Every layer uses `gpmOverlay`. Named requirements at the same coordinate are
conjunctive across dependencies and alternatives within one dependency are
disjunctive.

| Layer | Values and requirements |
| --- | --- |
| terrain | `water`, `land`, `mountain`; water and mountain cannot touch cardinally |
| hydrology | `sea` requires water; `dry` requires land or mountain; `river` requires land |
| biome | ocean refines water; shore/plains/woodland refine land; alpine refines mountain |
| roads | `trail` requires land + dry + plains/woodland; `bridge` requires land + river + plains/woodland; `tunnel` requires mountain + dry + alpine; `none` is unrestricted |
| housing | house requires a plains trail; cabin requires a woodland trail; lodge requires an alpine tunnel; `none` is unrestricted |
| foliage | reeds require an unoccupied river cell; grass, trees, and pine require an unoccupied matching dry biome; `none` is unrestricted |

Road values describe infrastructure through or serving a parcel. A house and
trail can therefore occupy the same coordinate in their separate overlays.
Foliage is excluded from every nonempty road or housing parcel. Bridge and
tunnel cannot touch, and nonempty housing cells cannot touch cardinally.

The independent `wfc_world2d_settlement_validate` unit recomputes this model
without reading the solver rule tables. It also verifies exact pass labels,
modes, dependency indices, dimensions, wrapping, layer vocabularies, and
cardinal relationships.

## counterfactual regeneration

After generating and validating the baseline, the program finds the first
row-major generated house or cabin on a dry trail. It promotes that cell's
hydrology value from `dry` to a caller-owned `river` lock, then calls:

```pascal
World.TryRegenerateFrom('hydrology', Options, Report);
```

The exact executed closure is:

```text
hydrology, roads, housing, foliage
```

`terrain` is an ancestor and `biome` is an unaffected sibling, so both are
reported as reused and retain their exact value and ownership state. The old
house or cabin becomes impossible on river hydrology and must disappear. The
demo prints changed-cell counts for every layer to make the edit locality
visible.

Clearing the hydrology lock and regenerating the same closure reproduces the
baseline byte-for-byte. The program also attempts to lock a house over the
edited river. That descendant regeneration fails with a named hydrology
dependency contradiction, leaves all committed state intact, and recovers
after the bad lock is cleared.

## replay fixtures

The default seed is `$31474144` (`826753348`; its little-endian bytes spell
`DAG1`). Its row-major edit coordinate is `(5,4)`:

```text
baseline 1:A988ED54:00717522:F7C8FA35:6E039D1A:6A60EB5E:4C620338
edited   1:A988ED54:B43B6363:F7C8FA35:099DA8D9:15265DF0:2324AC9D
```

The automated smoke seed `0` edits `(1,4)`:

```text
baseline 1:DBE324CA:52FB935C:4F4DC112:AE59EBFD:4814172B:8408D520
edited   1:DBE324CA:B34F0FA0:4F4DC112:7423E816:A3FAAAB4:25FE0A34
```

The seven-part signature is:

```text
<version>:<terrain>:<hydrology>:<biome>:<roads>:<housing>:<foliage>
```

Each component is a reflected CRC-32 over the fixed ASCII marker `WFD2`, model
and signature versions, layer ordinal, dimensions, and a fixed layer-specific
byte per row-major cell. It identifies output, not console formatting or
solver counters. The executable checks both known seeds and both states, as
well as exact baseline recovery.

## build and run

From the repository root, compile natively:

```text
mkdir build/settlement/native/units build/settlement/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/settlement/native/units -FEbuild/settlement/native/bin examples/2D/03_SelectiveSettlement/SelectiveSettlement.lpr
build/settlement/native/bin/SelectiveSettlement 0
```

Native output should match exactly for the same seed. The focused
`test/wfc_world2d_settlement_test.lpr` suite additionally covers topology
introspection, wrapped replay, raw-state preservation, failure attribution,
direct corruption, and independent-instance counterfactual parity.

## troubleshooting

- A signature mismatch means the model, solver contract, seed behavior, or
  canonical signature stream changed. Treat it as a versioned compatibility
  decision rather than updating constants automatically.
- `pipeline-topology` means a pass was renamed, reordered, changed from
  overlay mode, or gained/lost a dependency.
- A `road-context`, `housing-context`, or `foliage-context` issue identifies
  the related layer and value that makes the overlay illegal.
- If no edit candidate exists for a custom seed, the generated world simply
  contains no unlocked house/cabin on a dry trail. The two documented seeds
  are permanent fixtures and must always contain their stated candidates.

Overlapping-pattern models deliberately are not used as direct dependencies
here. Their graph values are private latent pattern keys; domain tokens exist
only after checked projection, and open projection can change dimensions. A
future projection-aware committed layer can connect those representations
without leaking latent keys into settlement rules.
