# Call-scoped preparation progress

`wfc_preparation` supplies optional synchronous observation for model construction
and independent musical-voice preparation. It owns no graph, clock or UI and
does not change search options, constraints, random draws or replay formats.
Supply `nil` to retain ordinary unobserved behavior. A returned model or graph
does not retain the observer.

## Measured work

`TWfcPreparationObserver` receives `TWfcPreparationProgress`:

| Field | Meaning |
| --- | --- |
| `Stage` | Named operation and unit, such as `model_construct_visits` |
| `Done` | Actual marked visits, rows or cells in that stage invocation |
| `Total` | Known stage total; zero means unknown or empty |
| `PassIndex` | Zero-based model/pass index, or -1 for operation-wide work |

Visits count the marked loops in the primary implementation, including nested
work; they are not elapsed time, bytes, search attempts or an overall percentage.
Visit stages start at 0/0, report each 256 marked visits, and finish with their
actual visit count as both Done and Total. An empty stage finishes at 0/0.
Existing sequence stages retain their own measured units and cadence.

Nested and repeated stage invocations can restart their counters. A consumer
must not sum unrelated stages into a fabricated total or assume that a stage
name is emitted only once. Within independent voices, model indices are harmony
0, rhythm 1 and voices 2 onward; later coverage passes retain their actual graph
indices. Operation-wide visit stages stay -1.

| Primary operation | Stages |
| --- | --- |
| All three `TWfcModel.Create` overloads | `model_construct_visits` |
| `TWfcSequenceModel.CreateGraphModel` | `sequence_graph_model_visits` and nested model construction |
| `ApplyModelToGraph` | `model_apply_visits` |
| `ApplySequenceModelToGraph`, `ApplySequenceModelSegmentToGraph` | `sequence_graph_visits`, `sequence_segment_graph_visits` and nested model work |
| Complete/partial sequence projection validation and requirements | `sequence_projection_preflight_visits`, `sequence_projection_visits`, `sequence_witness_projection_visits` and existing sequence proof stages |
| Ensemble model, rhythm and harmony validators | `ensemble_model_visits`, `ensemble_rhythm_rows`, `ensemble_harmony_rows` |
| Voice configuration copy/validation and coverage classes | `voice_config_copy_visits`, `voice_config_visits`, `voice_coverage_visits` and nested model validation |
| `BuildWfcMusicVoicesSegmentGraph` | `voice_build_visits`, `voice_prepare_visits` and nested model/sequence/projection work |
| Voice capture and independent validation | `voice_capture_visits`, `voice_validate_visits`, nested proof and `voice_capture_ready` |

`TWfcSequencePreparationBridge` forwards the existing typed sequence observer
with `sequence_` stage names and the current model index. Its `SequenceObserver`
and `Observer` accessors return nil when no receiver is installed. It reuses
the primary sequence proof, capture and domain algorithms.

Individual token decoding, history/key/salt access, RTL array/string copies and
allocations remain synchronous between checkpoints. A visit interval promises
no maximum wall-clock latency, browser yielding or background execution. A
caller needing those facilities must arrange them in its host. The voice
stream and other owners do not acquire observation merely because the finite
builder supports it; their call sites must explicitly forward a receiver.

## Cancellation and ownership

The observer is borrowed for one call. It must not mutate borrowed inputs,
reenter the operation or retain private construction state. Raise an exception
to abort. The original observer exception propagates unchanged through the
voice validators' ordinary diagnostic catches.

Model constructors, returned configuration/coverage copies and the finite voice
factory retain private candidates through their final callback. A failed voice
factory disposes its complete candidate graph, including derived pass objects.
`CaptureSolvedWfcMusicVoices` takes its generated record by `var`: capture,
independent proof and the final `voice_capture_ready` callback all finish before
assigning the caller's result. A propagated exception preserves that old result.
An ordinary False still returns an entirely empty generated record and its
validation report. Reports themselves are diagnostic outputs, not an atomic
publication contract.

Direct model/sequence/projection application mutates a caller-supplied
construction graph and keeps its existing basic failure semantics. It may be
partially changed on failure. Use a private graph and dispose it on an aborted
construction; observation does not make these APIs transactional.

## Existing guarded commit

The finite voice builder's final optional `AGraphClass` parameter defaults to
nil, meaning `TGraph`. A caller can pass its subclass and override the existing
`DoValidateCommit` hook to prepare a private captured result before solve commit.
Inherited pass creation preserves the selected class.

`TGraph.ValidatingCommit` is true only during that hook and forwards the root
state through pass graphs. Independent voice proof permits read-only capture
in this exact scope. `Running` remains true, so existing mutation and reentry
guards stay active. Ordinary running capture outside the hook remains rejected.
An exception or rejected validation uses the existing graph rollback; no second
transaction or callback after graph acceptance is introduced.

Strict proof still checks original model weights. An owner applying temporary
search preferences must restore original weights for proof and use `finally`
to reapply preferences while negotiation continues. Observation does not relax
model identity, domains, locks, rhythm, ranges, pair gaps or coverage witnesses.
Publication into an owner's retained state remains that owner's responsibility.

## Conformance and limits of the evidence

`wfc_music_voices_preparation_test` checks actual nested stages, original
observer exceptions, private cleanup, detached capture, ordinary False and
same-graph retry, including guarded proof with preferred search weights. Its
65-state exact-coverage fixture exercises two independent voices. A separate
1,024-state/255-cell case cancels at 256 actual ensemble-validation visits;
it does not claim completion or timing for that large construction.

Checked Win32/Win64 runs of the base matrix pass 533,780 checks and 131 original
abort/retries with zero leaks. Each native large-case check passes separately
with zero leaks. Actual pas2js execution of the full current fixture passes
533,791 checks and 132 abort/retries. Browser-target compilation is distinct
from the current-source CI browser execution gate.

`wfc_music_voices_preparation_replay` uses the shared authored fixture against
both the current source and frozen predecessor. Its three seeded native
results match byte for byte, and actual portable results match after newline
normalization. These are construction, cancellation and exact replay checks;
they establish no musical quality, source learning or portable speed claim.
