# Training Studio

A dependency-free workbench for learning and running your own small corpora.
The browser UI is compiled from Pascal. The native FPC host uses the same
workspace, learners, presets, and independent output checker.

## Run the browser workbench

From the repository root, with pas2js and its matching standard RTL configured:

```powershell
.\build-browser-training.ps1
```

```bash
bash ./build-browser-training.sh
```

The scripts stage `build/browser/training/www` and do not commit generated
JavaScript. Serve that directory with an ordinary static server:

```text
build/native/bin/wfc_serve --root build/browser/training/www --port 4176
```

Build the server with the native gate first; use `wfc_serve.exe` on Windows.
See [development tools](../../../docs/development-tools.md) for its loopback-only serving boundary.

Open `http://127.0.0.1:4176/`. Start with the overlapping checkerboard,
then try editing its token records, changing seed/size, adding two adjacent
same-token locks, and clearing the conflict. Recipe/run/result exports can be
replayed through the existing validator and runner.

Imported canonical source must use LF and exactly one final LF. Raw text is a
separate explicit Unicode-scalar import; it preserves whitespace and treats
the whole provided text as one sample. Source/license fields are declarations
you supply, not inferred permissions. All bundled presets are project-authored
and MIT licensed.
Browser text fields use LF line endings; canonical token records can retain
explicit CR or CRLF tokens without textarea normalization.

## Native demonstrations

The checked native build (`build.ps1` or `build.sh`) also runs all five
presets. Manually:

```text
build/native/bin/TrainingStudio --selftest
build/native/bin/TrainingStudio 2 0
```

Use `TrainingStudio.exe` on Windows. Arguments are preset index 0–4 and
an optional unsigned decimal seed; default is preset 2, seed 0. The native
demonstration is a self-checking console view, not an interactive native window.
For arbitrary file-based training and replay, use `wfc_learn`/`wfc_run`.

The console and browser token grid show percent-encoded values so whitespace and Unicode
remain visible and byte-portable.

## Seed-zero fixtures

These runs use no locks, local budget 1,024, pass budget zero, one-way solving,
and trace disabled:

| Preset | Output | Result signature |
| --- | --- | --- |
| 0 — alternating rows | Eight alternating A/B cells | `15860FEE` |
| 1 — cardinal checkerboard | 4×4 checkerboard | `7E6399E8` |
| 2 — overlapping checkerboard | Same board through private patterns | `947C4AFD` |
| 3 — whole token phrases | `red fox .` | `920A363A` |
| 4 — raw Unicode-scalar text | `a cat.` | `F65D4875` |

The first four presets reproduce the editable source/model/recipe bytes from
[TrainingDocuments](../04_TrainingDocuments/README.md). Their run/result
signatures differ from those earlier locked examples because these defaults
use no locks and a smaller budget.

At `?selftest=1`, the browser returns to preset 2 and publishes:

```text
data-state="solved"
data-self-test="passed"
data-source-signature="0FA2C5EA"
data-recipe-signature="DBCBA621"
data-result-signature="947C4AFD"
data-cell-count="16"
```

The interactive envelope is deliberately finite: 512 source tokens, 64
samples, 128 learned values/patterns/states, 512 output cells, and at most
4,096 local/64 pass backtracks. The UI runs synchronously. Open pattern
training remains model-only in the CLI and cannot export a Studio recipe.

See the [workspace and raw-text guide](../../../docs/training-studio.md) for
API ownership, exact invalidation behavior, provenance, policy limits, and
remaining scope.
