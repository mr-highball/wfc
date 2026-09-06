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
JavaScript. Serve that directory with the included FPC server:

```text
build/native/bin/wfc_serve --root build/browser/training/www --port 4176
```

Build the server with the native gate first; use `wfc_serve.exe` on Windows.
The server binds to loopback by default. See
[development tools](../../../docs/development-tools.md#access-from-a-trusted-local-network)
for opt-in trusted-LAN serving on one explicit address.

Open `http://127.0.0.1:4176/`. Start with the overlapping checkerboard,
then try editing its token records, changing seed/size, adding two adjacent
same-token locks, and clearing the conflict. Recipe/run/result exports can be
replayed through the existing validator and runner.

The **Whole-output quotas** editor adds explicit token-count requirements to
the saved training source. Try the whole-token phrase preset: select `café`,
set minimum and maximum to `1`, apply, then solve. Download/import the source
and retrain to preserve that policy. The 4×4 overlapping checkerboard also
makes a clear failure example: `A=8` solves, `A=7` contradicts, and clearing
quotas restores the original result. Draft edits must be applied or discarded
before solving; model-only export is disabled when it would lose quotas.
See [quota authoring](../../../docs/training-value-quotas.md).

The **Connectivity networks** editor saves explicit root/terminal XYZ positions
and participating public tokens with reciprocal directional ports. Load the
route corpus demonstration to generate a root-to-terminal path on a 4×3 board
with exactly six road cells. Change road ports to east/west only to make the
different-row destination unreachable; restore north/south to recover. This
shows local sample relationships, global quantity, and reachability working
together. The demonstration is separate from the six numbered presets.

Networks and quotas are saved together in version-4 training source. Editors
preserve one another's saved policies, require apply/discard for drafts, and
prevent model-only exports that would drop either requirement. Profiles are
authored, not inferred from the corpus. See
[connectivity authoring](../../../docs/training-connectivity.md).

Select **Volume checkerboard / six neighbors** for a 3D corpus. Width, height,
and depth control the requested volume; output is displayed in labeled Z
slices. Click any slice cell to copy X/Y/Z, add a lock, and solve again. With
this wrapped alternating corpus, even extents solve while odd wrapped cycles
contradict; increasing the search budget cannot repair an impossible cycle.

Imported canonical source must use LF and exactly one final LF. Raw text is a
separate explicit Unicode-scalar import; it preserves whitespace and treats
the whole provided text as one sample. Source/license fields are declarations
you supply, not inferred permissions. All bundled presets are project-authored
and MIT licensed.
Browser text fields use LF line endings; canonical token records can retain
explicit CR or CRLF tokens without textarea normalization.

## Native demonstrations

The checked native build (`build.ps1` or `build.sh`) also runs all six
presets. Manually:

```text
build/native/bin/TrainingStudio --selftest
build/native/bin/TrainingStudio --quota-demo
build/native/bin/TrainingStudio --quota-selftest
build/native/bin/TrainingStudio --connectivity-demo
build/native/bin/TrainingStudio --connectivity-selftest
build/native/bin/TrainingStudio 2 0
build/native/bin/TrainingStudio 5 0
```

Use `TrainingStudio.exe` on Windows. Arguments are preset index 0–5 and
an optional unsigned decimal seed; default is preset 2, seed 0. The native
demonstration is a self-checking console view, not an interactive native window.
For arbitrary file-based training and replay, use `wfc_learn`/`wfc_run`.
The quota demonstration is a separate checked native path; it uses the same
source-owned editing, retraining, contradiction, and replay workspace as the UI.
The connectivity demonstration additionally checks independent road reachability,
quantity, policy persistence, contradiction/recovery, and XYZ participation.

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
| 5 — volume checkerboard | 4×4×4, alternating along X/Y/Z | `CBDC737A` |

The first four presets reproduce the editable source/model/recipe bytes from
[TrainingDocuments](../04_TrainingDocuments/README.md). Their run/result
signatures differ from those earlier locked examples because these defaults
use no locks and a smaller budget.

The fifth training-document bundle (`adjacency3d`) exactly matches Studio
preset 5, including its unlocked 1,024-backtrack run. Its source and recipe
signatures are `C6E52736` and `4B8C29E4`.

At `?selftest=1`, the browser returns to preset 2 and publishes:

```text
data-state="solved"
data-self-test="passed"
data-source-signature="0FA2C5EA"
data-recipe-signature="DBCBA621"
data-result-signature="947C4AFD"
data-cell-count="16"
data-connectivity-edit="passed"
data-connectivity-replay="passed"
data-connectivity-contradiction="passed"
data-connectivity-invalidation="passed"
data-connectivity-volume="passed"
```

The interactive envelope is deliberately finite: 512 source tokens, 64
samples, 128 learned values/patterns/states, 512 output cells, and at most
4,096 local/64 pass backtracks. The UI runs synchronously. Open pattern
training remains model-only in the CLI and cannot export a Studio recipe.

See the [workspace and raw-text guide](../../../docs/training-studio.md) for
API ownership, exact invalidation behavior, provenance, policy limits, and
remaining scope.

See [volume learning](../../../docs/volume-learning.md) for the six-direction
learner, gravity-preserving versus cube symmetries, and format compatibility.
