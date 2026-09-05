# text pass composition

This example gives text three explicit owners instead of asking one sequence
model to do every job:

| Pass | Public vocabulary | Depends on |
| --- | --- | --- |
| `structure` | `DET`, `ADJ`, `NOUN`, `VERB`, `ADV`, `STOP` | none |
| `lexical` | words and symbolic stop choices | `structure` |
| `punctuation` | versioned visible fragments | `lexical` **and** `structure` |

Each pass is an independent order-2 latent sequence model. Alternatives within
one projection rule are OR choices: the visible `.` fragment may accept either
the lexical `dot` or `period` token. The two named providers of `punctuation`
are separate requirement groups, so lexical and structural compatibility must
both hold at the same position.

The reusable owner is `TWfcTextPassPipeline` in `wfc_text_passes`. It deep-copies
the three complete projection maps, owns one atomic dependency DAG, exposes
public-token constraints and selective descendant regeneration, captures all
three solved sequences, renders the final surface, and validates every latent
path and cross-pass relation independently.

Punctuation tokens use the strict `@wfctf1:` fragment codec. A fragment may
contain leading space, punctuation, Unicode text, or even be empty; rendering
is exact concatenation after canonical decoding. Private `@wfcs` graph keys do
not appear in results or the published causal trace. Trace events expose the
validated public token and numeric latent-state identity instead.

## native FPC

From the repository root, create the output directories and run:

```powershell
New-Item -ItemType Directory -Force build\examples\text-passes\native\units, build\examples\text-passes\native\bin | Out-Null
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/text/03_PassComposition -FUbuild/examples/text-passes/native/units -FEbuild/examples/text-passes/native/bin examples/text/03_PassComposition/TextPassComposition.lpr
build/examples/text-passes/native/bin/TextPassComposition 0
```

Both hosts use the same showcase unit. Seed zero is the checked replay fixture:

```text
Text: A sun rises brightly!
Signature: 1:69ABA6CE
Trace events: 244
Trace hash: 2412171679
structure:   DET NOUN VERB ADV STOP
lexical:     a sun rises brightly bang
punctuation: A |  sun |  rises |  brightly | !
```

## pas2js browser workbench

Stage the static site with either entry point:

```powershell
.\build-browser-text.ps1 -Compiler 'C:\path\to\pas2js.exe'
```

```bash
PAS2JS=/opt/pas2js/bin/pas2js bash ./build-browser-text.sh
```

Serve `build/browser/text-passes/www`, then open the page. The workbench can
replay seeds, inspect same-position lineage, lock or unlock any public token,
re-solve, and load a deliberate cross-pass contradiction. The model, solver,
validator, renderer, and self-test are Pascal; the page has no CDN or runtime
network dependency.

Appending `?selftest=1` runs the deterministic browser contract. Success leaves
the body with `data-state="solved"`, `data-self-test="passed"`,
`data-signature="1:69ABA6CE"`, `data-pass-count="3"`,
`data-output-signature="1:69ABA6CE"`,
`data-trace-hash="2412171679"`, and
`data-output="A sun rises brightly!"`.

## transaction and semantic boundary

This is an atomic, one-way staged cascade through public-result validation. After all
active passes stage a candidate, `TWfcTextPassPipeline` captures the three
latent/public paths, renders the surface, and runs independent validation in
the graph's final commit hook. Capture or validation failure returns `False`
with an empty result and `wtpsCaptureFailed` or `wtpsValidationFailed`. The
nested solve report records `gssContradiction` and `gckFinalValidation`; the
failed public layer and position remain in the owner report.

Such a rejection restores entry values and empty/generated ownership, every
root and pass-local random stream, and the graph's selected-pass/running state.
It does not erase caller-authored rule or domain edits. Public constraint edits
mark their owner dirty, and a later-only regeneration request automatically
widens to the earliest edited provider so stale upstream state is never reused.
The dirty root remains after failure and clears only after successful owner
publication.

The `Graph` property is an advanced surface for inspecting or editing the
owner's existing three layers. A corrupt reused provider discovered during a
later-only selective call raises `EInvalidOperation`: the core rolls back the
active attempt but does not silently widen its scope or attribute the failure
to another layer. The owner also rejects an added fourth pass inside that
rollback boundary. Repair the advanced graph edit and solve from the affected
provider (or run the full transaction) before reuse.

With trace capture enabled, final-validation rejection publishes a sanitized
public chronology ending in rollback. Private graph values are removed from
the nested solve report before trace projection. If an advanced graph edit
prevents public-token projection, the capture/validation diagnosis remains
primary, while the projected trace is empty and its validation report records
the secondary issue.

The [transaction tests](../../../test/wfc_text_pass_transaction_test.lpr) cover
these failures and repaired retries on native FPC and pas2js. The original
successful composition and trace signatures above remain unchanged; see the
[boundary research record](../../../docs/research/text-pass-publication-v1.md).

The v1 solver does not negotiate backward and ask a previously solved provider
to choose a different value during the same ordinary solve. Bounded inter-pass
negotiation is a separate facility, not an implied property of this example.

The showcase corpora and cross-vocabulary semantic maps are author-supplied.
The sequence learner discovers exact local structure inside each token stream;
this fixture does not claim to infer parts of speech or grammar classes from
raw prose.

The complete maintained path uses repository Pascal units and the applicable
standard FPC/pas2js RTL only. See the project
[dependency policy](../../../docs/dependencies.md),
[text foundation](../../../docs/text.md), and
[sequence projection contract](../../../docs/sequences.md).
