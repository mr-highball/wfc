# portable music audio v1

`wfc_music_audio` turns an immutable `TWfcMusicScore` into deterministic mono
PCM16 samples and encodes those samples as RIFF/WAVE bytes. The unit is a
preview edge adapter: it performs no device playback, file I/O, browser work,
or sample loading. Its implementation uses only project units and the standard
FPC/pas2js RTL.

The version-1 contract has two independently named versions:

- `WFC_MUSIC_AUDIO_VERSION = 1` covers score timing and synthesis;
- `WFC_MUSIC_WAVE_VERSION = 1` covers the RIFF/WAVE representation.

The implementation is [wfc_music_audio.pas](../src/wfc_music_audio.pas), and
its focused executable specification is
[wfc_music_audio_test.lpr](../test/wfc_music_audio_test.lpr).

## public API and ownership

The complete value and object surface is:

```pascal
type
  EWfcMusicAudio = class(EWfcMusic);

  TWfcMusicPcm16Sample = SmallInt;
  TWfcMusicPcm16Samples = array of TWfcMusicPcm16Sample;
  TWfcMusicAudioBytes = array of Byte;

  TWfcMusicAudioOptions = record
    SampleRate: Integer;
    MasterVolume: Integer;
    AttackMilliseconds: Integer;
    ReleaseMilliseconds: Integer;
  end;

  TWfcMusicPcm16Clip = class
  public
    constructor Create(const ASampleRate: Integer;
      const ASamples: TWfcMusicPcm16Samples);
    function SampleAt(const AIndex: Integer): TWfcMusicPcm16Sample;
    function CopySamples: TWfcMusicPcm16Samples;
    property SampleRate: Integer;
    property FrameCount: Integer;
  end;

function DefaultWfcMusicAudioOptions: TWfcMusicAudioOptions;
function RenderWfcMusicAudio(const AScore: TWfcMusicScore;
  const AOptions: TWfcMusicAudioOptions): TWfcMusicPcm16Clip;
function EncodeWfcMusicWave(
  const AClip: TWfcMusicPcm16Clip): TWfcMusicAudioBytes;
```

`TWfcMusicPcm16Clip` is caller-owned and immutable. Its constructor validates
the sample rate and copies the complete input array. `SampleAt` returns one
sample and raises `ERangeError` for an invalid index; `CopySamples` returns a
detached array that the caller may modify. `RenderWfcMusicAudio` returns a new
caller-owned clip, while `EncodeWfcMusicWave` returns a detached byte array.
Neither operation changes the score or clip.

An explicitly constructed clip may contain zero frames. Encoding it produces
a complete 44-byte WAVE file with an empty `data` chunk. Rendering a score is
stricter: if its entire duration rounds down to zero frames, rendering raises
`EWfcMusicAudio`. Nil scores and clips are rejected as well.

The public format and default constants are:

| Field | Public constant | Default | Accepted range |
| --- | --- | ---: | ---: |
| Channels | `WFC_MUSIC_AUDIO_CHANNEL_COUNT` | 1 | fixed |
| Bits per sample | `WFC_MUSIC_AUDIO_BITS_PER_SAMPLE` | 16 | fixed |
| `SampleRate` | `WFC_MUSIC_AUDIO_DEFAULT_SAMPLE_RATE` | 44,100 Hz | `WFC_MUSIC_AUDIO_MIN_SAMPLE_RATE` = 32,000 through `WFC_MUSIC_AUDIO_MAX_SAMPLE_RATE` = 48,000 Hz |
| `MasterVolume` | `WFC_MUSIC_AUDIO_DEFAULT_MASTER_VOLUME` | 96 | 0 through `WFC_MUSIC_AUDIO_MAX_MASTER_VOLUME` = 127 |
| `AttackMilliseconds` | `WFC_MUSIC_AUDIO_DEFAULT_ATTACK_MILLISECONDS` | 5 ms | 0 through `WFC_MUSIC_AUDIO_MAX_ENVELOPE_MILLISECONDS` = 1,000 ms |
| `ReleaseMilliseconds` | `WFC_MUSIC_AUDIO_DEFAULT_RELEASE_MILLISECONDS` | 20 ms | 0 through `WFC_MUSIC_AUDIO_MAX_ENVELOPE_MILLISECONDS` = 1,000 ms |

## exact musical time to sample frames

Tempo is stored as integer microseconds per quarter note. Let `T` be the
score's ticks per quarter, and let a tempo anchor retain a pair `(U, R)` where
`U` is a whole-microsecond count and `R/T` is the remaining fractional
microsecond. Initially both values are zero. Advancing `D` ticks at tempo `M`
uses only integer arithmetic:

```text
N = D * M + R
U = U + N div T
R = N mod T
```

Every tempo change stores the resulting pair and carries `R` into the next
segment. Splitting a constant tempo into extra equal-tempo changes therefore
cannot alter a later timestamp.

For sample rate `F`, a timestamp maps to this zero-based frame boundary:

```text
frame = floor((U + R / T) * F / 1,000,000)
```

The implementation evaluates the same expression without first discarding
`R` and without forming an unnecessarily large product:

```text
P     = U * F
base  = P div 1,000,000
rem   = P mod 1,000,000
frame = base + (rem * T + R * F) div (T * 1,000,000)
```

Span start and end ticks are mapped independently with this floor rule. A
very narrow span may consequently cover zero frames and emits no sample. A
tempo change inside one held span changes its end time but does not restart
the oscillator.

All wide integer operations have explicit safe bounds shared by native FPC
and pas2js:

- `D * M + R` is at most `8,589,936,735,483,646`, below `2^53`, because
  `D` and `T` fit positive `Integer`, `M <= 4,000,000`, and `R < T`;
- `U * F` is at most `2,880,000,000,000`;
- `rem * T + R * F` is at most `2,250,560,714,524,353`, below `2^51`;
- `T * 1,000,000` is at most `2,147,483,647,000,000`, also below `2^51`.

Native targets use an `Int64` carrier. pas2js uses its exact safe-integer
carrier, with every permitted intermediate below the JavaScript exact-integer
boundary. There is no floating-point clock or accumulated seconds value.

## fixed-point preview synthesis

Version 1 deliberately accepts the narrower playback boundary used by MIDI:
the score must have exactly 12 steps per octave, and every pitch must be in
`0..127`. The general score model remains capable of representing other step
systems and higher pitches; the audio adapter rejects rather than silently
remapping them.

Pitch starts from this pinned twelve-entry Q12 table for pitches `0..11`:

```text
33488 35479 37589 39824 42192 44701
47359 50175 53159 56320 59669 63217
```

Higher octaves use power-of-two shifts. For example, pitch 69 is exactly
`440 * 4096` in the table-derived representation. The phase increment is the
nearest integer to:

```text
frequencyQ12 * 2^24 / (SampleRate * 4096)
```

At pitch 127, the largest permitted phase numerator is
`861,999,936,307,200`, below `2^50`. At the minimum sample rate, that pitch's
table-derived frequency is about 12.54 kHz, below the 16 kHz Nyquist boundary.

Each tone has a 24-bit wrapping phase accumulator and a four-segment integer
triangle wave. Phase begins at zero for every tone in every span. An adjacent
new span therefore retriggers even when its pitch is unchanged; one longer
span remains phase-continuous. Chord tones have independent phase accumulators
and are summed with every simultaneously active voice.

The attack and release envelope uses a fixed unity value of `32767`.
Millisecond values first map to frames with
`SampleRate * milliseconds div 1000`. At frame offset `I` in a span of `L`
frames, the gain is the minimum of unity and any enabled ramps:

```text
attack  = I * 32767 div AttackFrames
release = (L - 1 - I) * 32767 div ReleaseFrames
```

Thus an enabled attack makes the first frame zero, and an enabled release
makes the final frame zero. If the ramps overlap, the lower gain wins.

Amplitude is evaluated in a fixed order, with signed division truncating
toward zero:

```text
triangle * velocity div 127
         * MasterVolume div 127
         * envelope div 32767
         div headroom
```

`headroom` is the sum, over all voices, of each voice's largest audible chord
size. It bounds the maximum simultaneous contribution without a
platform-dependent normalization pass. The integer mix is defensively clamped
to `SmallInt` when the final immutable clip is built.

This is intentionally a small preview synthesizer, not a physical instrument
model. The triangle oscillator is non-bandlimited, so high notes can alias.
Q12 pitch and integer phase increments introduce small tuning quantization.
There is no stereo placement, timbral voice model, sustain stage, filtering,
dither, resampling, dynamics processor, or claim of piano realism. Those
limitations are part of the version-1 sound.

## defensive limits

Validation and render planning finish before the PCM mix and output arrays are
allocated. Spans are inspected one detached tone array at a time rather than
copying the complete score timeline.

| Resource | Public constant | Version-1 maximum |
| --- | --- | ---: |
| Tracks | `WFC_MUSIC_AUDIO_MAX_TRACK_COUNT` | 32 |
| Voices | `WFC_MUSIC_AUDIO_MAX_VOICE_COUNT` | 32 |
| Meter changes | `WFC_MUSIC_AUDIO_MAX_METER_COUNT` | 4,096 |
| Tempo changes | `WFC_MUSIC_AUDIO_MAX_TEMPO_COUNT` | 4,096 |
| Span events | `WFC_MUSIC_AUDIO_MAX_SPAN_COUNT` | 65,536 |
| Tones in one span | `WFC_MUSIC_AUDIO_MAX_TONES_PER_SPAN` | 16 |
| Total tone occurrences | `WFC_MUSIC_AUDIO_MAX_TOTAL_TONE_COUNT` | 65,536 |
| Tempo | `WFC_MUSIC_AUDIO_MAX_TEMPO_MICROSECONDS_PER_QUARTER` | 4,000,000 microseconds per quarter |
| Exact score duration | `WFC_MUSIC_AUDIO_MAX_DURATION_MICROSECONDS` | 60,000,000 microseconds |
| PCM frames | `WFC_MUSIC_AUDIO_MAX_SAMPLE_FRAME_COUNT` | 2,880,000 |
| Tone-frame render visits | `WFC_MUSIC_AUDIO_MAX_RENDER_VISIT_COUNT` | 16,777,216 |
| Encoded WAVE bytes | `WFC_MUSIC_AUDIO_MAX_WAVE_BYTE_COUNT` | 5,760,044 |

The duration limit includes the fractional `R/T`: exactly 60 seconds is
allowed, while 60 seconds plus any fractional microsecond is rejected. Render
visits are the sum of `quantized span frames * tone count` for sounding spans.
The visit budget limits synthesis work independently from the frame and tone
budgets; it is not a wall-time promise.

## canonical PCM16 RIFF/WAVE bytes

`EncodeWfcMusicWave` emits one canonical layout. Integer fields are written
explicitly in little-endian order; no packed Pascal record or host byte order
enters the artifact.

| Offset | Bytes | Meaning |
| ---: | ---: | --- |
| 0 | 4 | `RIFF` |
| 4 | 4 | unsigned file size minus 8 |
| 8 | 4 | `WAVE` |
| 12 | 4 | `fmt `, including the trailing space |
| 16 | 4 | format chunk size `16` |
| 20 | 2 | PCM format tag `1` |
| 22 | 2 | channel count `1` |
| 24 | 4 | sample rate |
| 28 | 4 | byte rate, `SampleRate * 2` |
| 32 | 2 | block alignment `2` |
| 34 | 2 | bits per sample `16` |
| 36 | 4 | `data` |
| 40 | 4 | frame count times 2 |
| 44 | variable | signed PCM16 samples |

Each sample is the two's-complement little-endian representation of
`-32768..32767`; zero is silence. Mono PCM16 data is always even-sized, so this
fixed layout needs no padding byte. The RIFF size is `36 + data bytes`, and the
complete artifact size is `44 + data bytes`.

These fields and sizes follow Microsoft's primary documentation for
[RIFF/WAVE chunk layout](https://learn.microsoft.com/en-us/windows/win32/xaudio2/resource-interchange-file-format--riff-),
[`WAVEFORMATEX`](https://learn.microsoft.com/en-us/windows/win32/api/mmreg/ns-mmreg-waveformatex),
[multimedia data types and PCM ranges](https://learn.microsoft.com/en-us/windows/win32/multimedia/devices-and-data-types),
and [`PCMWAVEFORMAT`](https://learn.microsoft.com/en-us/windows/win32/api/mmeapi/ns-mmeapi-pcmwaveformat).

## conformance evidence

The focused suite contains 35 checks and runs unchanged under FPC 3.2.2, FPC
3.3.1, and pas2js 3.3.1 with Node. It covers immutable ownership, exact empty
and boundary-sample WAVE bytes, multiple voices, chords, rests, tempo changes,
equal-tempo partition equivalence, phase retriggering, a fractional timestamp
that contributes the final frame, a note quantized to zero frames, pitch and
option boundaries, and failure before large render allocation.

Its 1.5-second, 32 kHz multi-voice fixture pins both products:

| Product | FNV-1a hash |
| --- | --- |
| 48,000 PCM16 frames | `D112EACB` |
| 96,044-byte RIFF/WAVE | `8FA328DD` |

The shared Music Studio uses the same renderer for a four-second score at
44,100 Hz, producing exactly 176,400 frames. That is core data-path evidence;
no browser UI behavior is claimed here.
