# negotiated music variation

`NegotiatedVariation.lpr` and `NegotiatedVariationNode.lpr` are thin native
FPC and pas2js/Node hosts over one self-checking Pascal fixture. The fixture
uses the project-owned music score, sequence, pass-pipeline, canonical text,
and Standard MIDI File layers. It has no playback or external dependency.

## pass and repair shape

The graph has four fixed-quantum cells and exactly three public music layers:

| Stable index | Pass | Role |
| ---: | --- | --- |
| `0` | `harmony` | Chooses one of two pitch-class progressions. |
| `1` | `rhythm` | Owns the shared attack/hold/attack/hold pattern. |
| `2` | `melody` | Requires both providers through the checked music maps. |

Both order-4 learned melody branches retain the same two-cell C4 opening
motif. Their last two cells differ between E4 and G4, and the two learned
harmony branches supply the corresponding pitch classes. Seed zero first
commits E4. The edit then locks the public opening motif and requests G4.

Ordinary selective regeneration from `harmony` has requested roots `[0]` and
active indices `[0,2]`. It deterministically repeats the original harmony,
then fails at `melody`; the transaction rolls back and the baseline
composition stays current. Bounded selective negotiation uses the same scope,
excludes that one exact completed harmony assignment, and commits the other
harmony with the requested melody. `rhythm` remains outside the active scope,
is reported as reused, and retains its public cells.

The fixture independently captures and validates all three public cell
streams, copies and validates the rebuilt score, round-trips strict
`wfcmusic=1`, canonicalizes the emitted SMF bytes, and prints portable public
composition, score, MIDI, nested-negotiation, and selective-negotiation
signatures. Every claimed seed-zero value and signature is asserted before it
is printed.

## build and run

From the repository root on PowerShell:

```powershell
New-Item -ItemType Directory -Force `
  build\examples\music-negotiated\native\units, `
  build\examples\music-negotiated\native\bin | Out-Null
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc `
  -Fuexamples\music\04_NegotiatedVariation `
  -FUbuild\examples\music-negotiated\native\units `
  -FEbuild\examples\music-negotiated\native\bin `
  examples\music\04_NegotiatedVariation\NegotiatedVariation.lpr
.\build\examples\music-negotiated\native\bin\NegotiatedVariation.exe 0
```

On a POSIX shell:

```bash
mkdir -p build/examples/music-negotiated/native/units \
  build/examples/music-negotiated/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc \
  -Fuexamples/music/04_NegotiatedVariation \
  -FUbuild/examples/music-negotiated/native/units \
  -FEbuild/examples/music-negotiated/native/bin \
  examples/music/04_NegotiatedVariation/NegotiatedVariation.lpr
./build/examples/music-negotiated/native/bin/NegotiatedVariation 0
```

With a configured pas2js compiler and matching RTL:

```bash
mkdir -p build/examples/music-negotiated/pas2js/units \
  build/examples/music-negotiated/pas2js/bin
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -Fuexamples/music/04_NegotiatedVariation \
  -FUbuild/examples/music-negotiated/pas2js/units \
  -FEbuild/examples/music-negotiated/pas2js/bin \
  examples/music/04_NegotiatedVariation/NegotiatedVariationNode.lpr
node build/examples/music-negotiated/pas2js/bin/NegotiatedVariationNode.js 0
```

The seed-zero public goldens are:

| Evidence | Signature |
| --- | --- |
| Baseline composition | `4194643A` |
| Repaired composition | `77B72044` |
| Canonical score text, FNV-1a | `6F92E033` |
| Canonical MIDI bytes, FNV-1a | `4BC35E03` |
| Nested Pass Negotiation v1 transcript | `A91E0706` |
| Selective Negotiation v1 transcript | `38EE8F80` |

The canonical `wfcmusicpass=1` artifact is `685` bytes. The program requires
all of these values before reporting `Self-check: passed`; native and Node
print the same output.

## bounded claim

This is a deterministic feasibility proof, not an optimizer. Pass Negotiation
v1 excludes complete provider assignments and searches them chronologically.
The possible assignment vectors grow exponentially with cells and values;
the local solver-backtrack and outer pass-backtrack budgets are separate hard
limits. Success means only that the first compatible composition within those
limits was found. It does not prove a minimal edit, closest variation, best
voice leading, or better efficiency than a flattened model.

The exact experiment and non-claims are recorded in
[`docs/research/music-negotiated-variation-v1.md`](../../../docs/research/music-negotiated-variation-v1.md).
