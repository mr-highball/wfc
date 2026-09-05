# Independent Voices v1: collective harmony without a joint vocabulary

Status: project-owned representation and conformance experiment. This record
describes a finite constraint encoding and its bounded continuation protocol,
not a priority claim, a new mathematical theorem, a benchmark victory, or a
general theory of counterpoint.

The implementation is portable Pascal under the repository's MIT license.
Existing joint-frame ensemble and monophonic contracts remain unchanged.

## Question

Can independently learned, chord-capable voice vocabularies produce vertical
combinations absent from the complete training frames while preserving each
voice's temporal holds and requiring the exact collective harmony at every
synchronized cell?

Joint-frame vocabulary cannot emit an unobserved complete public frame token.
That restriction is useful when the vertical combination itself is the learned
object. Here the alternative factorization learns one existing singleton
`wme1` sequence model per role, keeps common excerpt provenance, and coordinates
the roles through shared harmony and rhythm providers.

The hypothesis is representational: a small existential supplier layer can
enforce exact collective pitch-class coverage without enumerating the
Cartesian product of role vocabularies. It does not hypothesize independent
musical rhythms, unrestricted improvisation, or improved search efficiency.

## Encoding and proof obligation

At cell `t`, let `H(t)` be the selected harmony set and `S_i(t)` the set of
pitch classes in sounding role `i`, including holds. Each role's local token
must satisfy its action slot in the shared rhythm vector, its pitch range,
its sequence path, and every configured pair constraint.

First impose `S_i(t) ⊆ H(t)` for every role. This is the complete harmonic
condition in **allowed** mode: silence or incomplete coverage is permissible.

For **exact** mode, let `C` be the union of all classes occurring in the finite
harmony vocabulary. Add one witness pass per `c ∈ C`. At each cell its values
are `absent` and one supplier value per role. The rules are:

- `absent` requires `c ∉ H(t)`;
- supplier `i` requires `c ∈ H(t)`, `c ∈ S_i(t)`, and `c ∉ S_j(t)` for
  every earlier role `j < i`.

If a complete assignment satisfies those rules, every selected harmony class
has a sounding supplier. Thus `H(t) ⊆ ⋃S_i(t)`. The per-role subset clauses give
the reverse inclusion, establishing `H(t) = ⋃S_i(t)`.

Conversely, if the selected roles' sounding union equals `H(t)`, choose
`absent` for every omitted class and the lowest-index sounding supplier for
each present class. All witness rules hold. The encoding is therefore satisfiable exactly
when collective coverage holds, assuming the other role/provider constraints.
The proof is finite and local to a synchronized cell; it does not prove that a
bounded solver will discover a satisfying assignment.

Coverage is existential, not exclusive. Multiple roles may sound the same
class. The published version-one encoding chooses the lowest-index supplier
canonically, so a fixed musical assignment has only one coverage proof.
An independent validator rejects a higher-index supplier even if it sounds
the class. Public frames may still have multiple latent sequence explanations;
diagnostic signatures are not unique fingerprints of musical sound.

`RequireSequencePartialProjectionMapsFromPasses` provides the required empty
relation semantics: no allowed provider value means a forbidden consumer
candidate, not an ignored condition. Its existing strict counterpart remains
unchanged. The musical capture validator recomputes the union and checks the
chosen suppliers independently of the mutable compiled requirement registry.

### Negative result: searching equivalent proofs

The initial implementation omitted the earlier-role exclusion. Its coverage
condition was still logically correct, but a class with `k` suppliers admitted
`k` equivalent witness choices at each cell. Ordinary pass negotiation could
spend its budget excluding these proofs without changing any musical role.

A reproduced Voice Studio continuation case used the authored A/B corpus,
seed `1`, five-cell segments, `61` requested seconds, local budget `1024`,
and pass budget `64`. At the failing frontier the noncanonical version spent
all 64 exclusions on coverage assignments: 16 each in `coverage.0`,
`coverage.2`, `coverage.4`, and `coverage.5`. No musical voice was reopened;
the failing consumer was `coverage.7`.

Adding lowest-supplier constraints to the **same frontier, derived seed, and
budgets** solved that local case in five pass backtracks. This is a controlled
witness-symmetry diagnostic, not an across-model benchmark or a guarantee for
every long duration. The former encoding is specified above by omitting only
the earlier-role exclusion; the maintained implementation and independent
validator use the canonical form. This refinement does not prohibit shared
pitch classes and does not change which musical assignments satisfy harmony.

The native export rerun held the authored host budgets at `1024/64`. It
covered seeds `0`, `1`, `55`, and `4294967295` with one-, five-, and seven-cell
segments at 5.25 seconds, plus short-boundary and 61-second cases. All 32 MIDI
and 10 WAVE exports succeeded; the 21 corresponding FPC 3.2.2/development
compiler pairs were byte-identical. Four attempts to replace existing files
were rejected without changing their hashes. These are finite regression
observations, not a duration target or an arbitrary-seed feasibility proof.

For the 61-second, seed-one, five-cell case, independent MIDI decoding found
305 paired notes and end-of-track at tick 58560; WAVE contained 2690100 sample
frames. The stream produced 49 segments with 78 held voice seams and 81
previously unobserved ordered verticals. The small corpus and fixed tempo
make these reproducible correctness checks, not an assessment of musical merit.

## Two distinct fixtures

The following fixtures address different obligations and must not be reported
as the same experiment.

### Minimal graph truth table

`test/wfc_music_voices_graph_test.lpr` uses a tiny two-cell attack/rest setup
with three roles and two alternatives per role. Its eight fixed role choices
exercise the actual graph constraint encoding: six satisfy exact harmonic
coverage, all eight satisfy subset semantics. It also supplies adversarial
graph/result mutations and checks independent validation.

This is a small structural truth table, not a sixteen-cell learned composition
or an audible quality evaluation.

### Authored temporal A/B corpus

`examples/music/07_VoiceStudio/voice_studio_corpus.pas` owns two named
sixteen-cell excerpts. Each repeats the same eight-cell time structure twice:

```text
cell:         0 1 2 3 4 5 6 7
shared set:   C C C C D D D -
bass/chord:   A H H H A H H R
upper:        A H A H A H A R
```

Here `C={0,4,7}`, `D={2,5,9}`, `-` is the empty set, and `A/H/R` mean
attack/hold/rest. Time is exact: TPQ 480, quantum 240, twelve steps per octave,
tempo 500000 microseconds per quarter, 4/4. Each sixteen-cell excerpt lasts
3840 ticks. Excerpt B is selected at tick 3840 in the combined 32-cell score.

| Role | A: C region → D region | B: C region → D region | Velocity | Range |
| --- | --- | --- | --- | --- |
| Bass | `48 → 50` | `55 → 57` | 72 | 48..57 |
| Chord | `[60,64] → [62,65]` | `[64,67] → [65,69]` | 64 | 60..69 |
| Upper | `79 → 81` | `72 → 74` | 96 | 72..81 |

Each chord is one voice with two simultaneously held tones. The upper role
reattacks at cells 2 and 6 while the bass and chord continue holding. Rest at
cell 7 precedes another authored attack at cell 8, providing actual compatible
continuation evidence rather than requiring an artificial seam reset.

Roles use order 2; shared harmony and rhythm use order 8. All observed starts
and structurally compatible role-model edges are independently checked for
valid voice continuity. Shared rhythm is still a **joint action vector**;
this experiment independently recombines pitch content, not arbitrary rhythms.

The independent training test enumerates the eight complete A/B role choices
directly from detached corpus frames, without using the production graph to
judge expected harmonic coverage. Exactly six equal the shared sets. Choices
`[A,A,B]` omit G in the C region and A in the D region; `[B,B,A]` omit C and D
respectively. Both remain legal subsets.

Two exact choices are the original A and B ensembles; four are new exact
vertical combinations. The named witness `[A,B,B]` produces
`[48 | 64,67 | 72]` and `[50 | 65,69 | 74]`, neither of which occurs as an
ordered sounding pitch tuple anywhere in the training corpus. Novelty ignores
action and velocity changes, and does not count the already-observed silence.

All eight choices retain the authored hold/attack sequences, role ranges, and
at-most-two-step sorted-tone movement between consecutive sounding cells.
Those are fixture properties. The general API does not expose arbitrary
voice-leading objectives, and noncrossing requires explicit nonnegative pair
minima or otherwise separating ranges.

## Training provenance and bounded preflight

One `TWfcMusicVoicesTrainingBundle` publishes shared harmony/rhythm documents
and a singleton-frame document per role, with common named excerpts and
stable role/source-voice/track provenance. Learned content remains in existing
training and sequence formats. The source score and metadata are borrowed;
the returned bundle, documents, models, and copied frames are detached.

Common excerpt cuts reject a sounded span crossing either edge in any selected
role. Cropping rests is permitted. There is no attack fabrication or inferred
voice identity. Model consumption checks compatible latent edges separately:
a valid sample containing holds can still produce an unsafe order-one model.

The entire bundle shares aggregate sample, token, encoded-content and visit
budgets. Tests include individually valid documents whose combination exceeds
the aggregate budget, repeated large metadata, oversized chord tokens, and
repeated tone-observation work. This is not a claim that separately checking
each document bounds a multi-document training operation.

## Bounded continuation and publication

`wfc_music_voices_stream` retains exact previous latent states for both shared
providers and every role. Initial segments require observed starts. Continuing
segments require compatibility with those exact previous states, including
valid early BOS-bearing context when a local segment is shorter than order N.
Musical frames alone are not sufficient authentication for that latent context.

Each call builds one local graph, applies future-cell constraints and optional
application restrictions, solves with explicit local/pass budgets, and captures
an independent semantic proof. Only then does it publish a detached result,
advance the frontier, and retire consumed constraints. Allocation and validation
failures leave the last published progress unchanged. The stream never repairs
already emitted history; a later local dead end is terminal for that run.

An intermediate seam does not reset a voice. A held chord crossing a seam
continues as a hold while another role may attack. Finite cutoff and observed
end are distinct policies. `RequireObservedEnd` affects the final segment,
not the beginning of each local chunk.

For fixed model/configuration sizes, live generator storage is independent of
elapsed duration: one local graph, current detached result under construction,
per-model frontier, and explicitly stored future constraints. Vocabulary,
pair-relation, coverage-class, chord, trace/report, and retained-result sizes
remain meaningful costs. A caller can request a very expensive horizon or
retain every result. No global feasibility, constant runtime, infinite integer
capacity, or infinite storage claim follows from bounded timeline memory.

The existing incremental ensemble audio and two-pass MIDI adapters consume
these frames directly. Held phases and note lifetimes survive segment seams.
File hosts drain bounded blocks and publish only after complete validation;
they do not concatenate reset previews or retain a whole-song browser Blob.
Format-specific MIDI and WAV/RF64 capacity checks remain separate from the
generation contract.

## Reproduction and evidence scope

The implementation contracts are version one:

- `WFC_MUSIC_VOICES_TRAINING_VERSION`;
- `WFC_MUSIC_VOICES_GRAPH_VERSION`;
- `WFC_MUSIC_VOICES_STREAM_VERSION`;
- `WFC_MUSIC_VOICES_SEGMENT_SIGNATURE_VERSION`.

The stream uses the existing versioned arrangement section-seed schedule.
The focused stream fixture pins segment signature `10F2F6C8`; that value
belongs to its own two-role sustained-chord fixture, not the A/B corpus or a
universal output. Ordered models, constraints, base seed, segmentation,
search settings, and algorithm versions are needed for replay. These compact
32-bit signatures detect accidental divergence; they are not cryptographic
proofs or complete model identities.

Executable specifications include:

- `test/wfc_music_voices_training_test.lpr`: independent document parity,
  detached provenance, common cuts, aggregate limits, full authored eight-choice
  enumeration, role ordering, and safe/unsafe order-one distinctions;
- `test/wfc_music_voices_graph_test.lpr`: minimal graph truth table, collective
  constraints, exact/allowed separation, and independent result validation;
- `test/wfc_music_voices_stream_test.lpr`: early latent context, held chords,
  frontier rollback, global constraints, cancellation/reentry, deterministic
  signatures, and checked numeric boundaries;
- Voice Studio shared/browser/process tests: application duration planning,
  streamed frame/audio/MIDI consumption and host publication lifecycle.

Run `build.ps1` or `build.sh` for the native gate. Build portable tests with
`build-browser-tests.ps1` / `.sh`, then run the real-browser conformance runner.
The included FPC server and DOM checker verify executed browser evidence;
transpilation alone is not runtime proof. The [build guide](../building.md)
and [Voice Studio guide](../../examples/music/07_VoiceStudio/README.md) contain
the maintained commands. Gate totals are reported by those executions, not
inferred from the existence of source files.

## Limits and next experiments

There is no Cartesian **vocabulary**, but compatible voice tuples and latent
sequence choices can still yield combinatorial search. Canonical suppliers
remove the proof symmetry observed in the initial experiment, not those
musical search choices. The implementation has not
established a speed or memory advantage over every equivalent flattened
model. Model validation and relation construction repeat for local graphs.

The authored corpus is deliberately tiny. Six feasible combinations are a
correctness result, not evidence of broad musical quality. Reproducible next
experiments should compare flattened versus factored models on the same
finite feasible sets, measure canonicalization and search budgets, separate
rhythm-role learning from the current shared vector, and add explicit
temporal voice-leading constraints with independent validators. Longer-form
structure and expressive performance need their own contracts and evidence.
