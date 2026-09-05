# Editable training documents

Five small, project-authored MIT corpora exercise the same portable training
workflow on native FPC. No media, tokenizer, playback, or
third-party library is needed.

| Profile | Samples | Seed-zero public output | Training fingerprint |
| --- | --- | --- | --- |
| `adjacency1d` | Two 4-token alternating rows, independent wraps | `A B A B A B A B` | `6DF80942` |
| `adjacency2d` | 2×2 and 4×2 checkerboards, wrapped D4 | 4×4 checkerboard beginning with A | `6F432E54` |
| `pattern2d` | Same grids, wrapped 2×2 footprints with D4 | Same public 4×4 checkerboard, private pattern pass | `0FA2C5EA` |
| `sequence` | `red fox .` and `café fox .`, open order 2 | `red fox .`, private sequence pass | `62C511AF` |
| `adjacency3d` | 2×2×2 alternating cube, wrapped cube24 | 4×4×4 six-neighbor checkerboard | `C6E52736` |

Each profile includes five files: editable `.wfclearn`, generated
`.wfcpipeline`, standalone `.model` (the existing canonical model format),
recipe-bound `.wfcrun`, and exact solved `.wfcresult`. The generated recipes
retain the source/license and ordered sample-content fingerprints. The
standalone model must travel with its source document or recipe to retain
that provenance.

The original four run files pin seed 0, one-way solving, a 65,536 local backtrack budget,
no trace, and a public `output` lock at the first cell: A for grids and red
for the sequence. The projection examples use bridge v2 to lower that public
lock into the private source pass.

The volume bundle uses Studio preset 5: seed 0, 4×4×4, one-way, local budget
1,024, no locks or trace. Its recipe/result signatures are `4B8C29E4` and
`CBDC737A`. Source depth belongs to `wfclearn=2`; the standalone six-direction
model uses `wfcm=3`. Existing four bundles remain byte-identical.

## Native FPC

Run `./build.sh` or `.\\build.ps1` from the repository root. The examples
are checked by the build. To replay one manually in Bash:

```bash
build/native/bin/wfc_learn examples/learning/04_TrainingDocuments/pattern2d.wfclearn > build/trained.wfcpipeline
cmp build/trained.wfcpipeline examples/learning/04_TrainingDocuments/pattern2d.wfcpipeline
build/native/bin/wfc_validate recipe build/trained.wfcpipeline
build/native/bin/wfc_run build/trained.wfcpipeline examples/learning/04_TrainingDocuments/pattern2d.wfcrun > build/trained.wfcresult
cmp build/trained.wfcresult examples/learning/04_TrainingDocuments/pattern2d.wfcresult
```

Replace `pattern2d` with any profile above. On Windows, use the `.exe`
names. Windows PowerShell's text redirection may alter encoding or line
endings; use a byte-preserving native redirect when saving canonical artifacts:

```powershell
cmd /c "build\\native\\bin\\wfc_learn.exe examples\\learning\\04_TrainingDocuments\\pattern2d.wfclearn > build\\trained.wfcpipeline"
& build/native/bin/wfc_validate.exe recipe build/trained.wfcpipeline
& build/native/bin/wfc_run.exe build/trained.wfcpipeline examples/learning/04_TrainingDocuments/pattern2d.wfcrun
```

These recipes contain strict version pins and signatures. Edit the source
document and retrain instead of hand-editing generated artifacts. Changing the
source changes the recipe identity: the checked run files are for the unchanged
examples. See [pipeline artifacts](../../../docs/pipeline-artifacts.md) for
constructing new recipe-bound run requests.
