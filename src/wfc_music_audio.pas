(*
MIT License

Copyright (c) 2021 mr-highball

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)
unit wfc_music_audio;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc_music;

const
  WFC_MUSIC_AUDIO_VERSION = 1;
  WFC_MUSIC_WAVE_VERSION = 1;

  WFC_MUSIC_AUDIO_CHANNEL_COUNT = 1;
  WFC_MUSIC_AUDIO_BITS_PER_SAMPLE = 16;
  WFC_MUSIC_AUDIO_MIN_SAMPLE_RATE = 32000;
  WFC_MUSIC_AUDIO_MAX_SAMPLE_RATE = 48000;
  WFC_MUSIC_AUDIO_DEFAULT_SAMPLE_RATE = 44100;
  WFC_MUSIC_AUDIO_MAX_MASTER_VOLUME = 127;
  WFC_MUSIC_AUDIO_DEFAULT_MASTER_VOLUME = 96;
  WFC_MUSIC_AUDIO_DEFAULT_ATTACK_MILLISECONDS = 5;
  WFC_MUSIC_AUDIO_DEFAULT_RELEASE_MILLISECONDS = 20;
  WFC_MUSIC_AUDIO_MAX_ENVELOPE_MILLISECONDS = 1000;

  WFC_MUSIC_AUDIO_MAX_TRACK_COUNT = 32;
  WFC_MUSIC_AUDIO_MAX_VOICE_COUNT = 32;
  WFC_MUSIC_AUDIO_MAX_METER_COUNT = 4096;
  WFC_MUSIC_AUDIO_MAX_TEMPO_COUNT = 4096;
  WFC_MUSIC_AUDIO_MAX_SPAN_COUNT = 65536;
  WFC_MUSIC_AUDIO_MAX_TONES_PER_SPAN = 16;
  WFC_MUSIC_AUDIO_MAX_TOTAL_TONE_COUNT = 65536;
  WFC_MUSIC_AUDIO_MAX_TEMPO_MICROSECONDS_PER_QUARTER = 4000000;
  WFC_MUSIC_AUDIO_MAX_DURATION_MICROSECONDS = 60000000;
  WFC_MUSIC_AUDIO_MAX_SAMPLE_FRAME_COUNT = 2880000;
  WFC_MUSIC_AUDIO_MAX_RENDER_VISIT_COUNT = 16777216;
  WFC_MUSIC_AUDIO_MAX_WAVE_BYTE_COUNT =
    44 + WFC_MUSIC_AUDIO_MAX_SAMPLE_FRAME_COUNT * 2;

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

  { Immutable, owning, mono PCM16 preview. }
  TWfcMusicPcm16Clip = class
  strict private
    FSampleRate: Integer;
    FSamples: TWfcMusicPcm16Samples;
    function GetFrameCount: Integer;
    procedure ValidateSampleIndex(const AIndex: Integer);
  public
    constructor Create(const ASampleRate: Integer;
      const ASamples: TWfcMusicPcm16Samples);
    function SampleAt(const AIndex: Integer): TWfcMusicPcm16Sample;
    function CopySamples: TWfcMusicPcm16Samples;
    property SampleRate: Integer read FSampleRate;
    property FrameCount: Integer read GetFrameCount;
  end;

function DefaultWfcMusicAudioOptions: TWfcMusicAudioOptions;

{ Renders a deliberately modest, non-bandlimited triangle-wave preview. Each
  span retriggers phase; a held span remains continuous across tempo changes.
  Fixed-point pitch, envelope, timing, and mixing make replay byte-exact. }
function RenderWfcMusicAudio(const AScore: TWfcMusicScore;
  const AOptions: TWfcMusicAudioOptions): TWfcMusicPcm16Clip;

{ Encodes the clip as canonical mono PCM16 RIFF/WAVE bytes. }
function EncodeWfcMusicWave(
  const AClip: TWfcMusicPcm16Clip): TWfcMusicAudioBytes;

implementation

const
  WFC_AUDIO_MICROSECONDS_PER_SECOND = 1000000;
  WFC_AUDIO_PHASE_BITS = 24;
  WFC_AUDIO_PHASE_MODULUS = 1 shl WFC_AUDIO_PHASE_BITS;
  WFC_AUDIO_PHASE_MASK = WFC_AUDIO_PHASE_MODULUS - 1;
  WFC_AUDIO_PHASE_QUARTER = WFC_AUDIO_PHASE_MODULUS div 4;
  WFC_AUDIO_FREQUENCY_FRACTION_BITS = 12;
  WFC_AUDIO_FREQUENCY_SCALE = 1 shl WFC_AUDIO_FREQUENCY_FRACTION_BITS;
  WFC_AUDIO_ENVELOPE_SCALE = 32767;

  { MIDI pitches 0..11 in hertz times 4096, rounded once for this version.
    Higher octaves are exact power-of-two shifts of this pinned table. }
  WFC_AUDIO_BASE_FREQUENCY_Q12: array[0..11] of Integer = (
    33488, 35479, 37589, 39824, 42192, 44701,
    47359, 50175, 53159, 56320, 59669, 63217
  );

type
  { pas2js NativeInt is an exact integer carrier through 2^53; native FPC
    needs Int64 for the same bounded calculations on 32-bit targets. }
  {$IFDEF PAS2JS}
  TWfcAudioWideInteger = NativeInt;
  {$ELSE}
  TWfcAudioWideInteger = Int64;
  {$ENDIF}

  TWfcAudioTime = record
    WholeMicroseconds: Integer;
    FractionNumerator: Integer;
  end;

  TWfcAudioTempoAnchor = record
    Tick: Integer;
    WholeMicroseconds: Integer;
    FractionNumerator: Integer;
    MicrosecondsPerQuarter: Integer;
  end;
  TWfcAudioTempoAnchors = array of TWfcAudioTempoAnchor;
  TWfcAudioIntegerArray = array of Integer;

procedure AudioError(const AMessage: String);
begin
  raise EWfcMusicAudio.Create('cannot render WFC music audio: ' + AMessage);
end;

procedure WaveError(const AMessage: String);
begin
  raise EWfcMusicAudio.Create('cannot encode WFC music WAVE: ' + AMessage);
end;

procedure ValidateSampleRate(const ASampleRate: Integer);
begin
  if (ASampleRate < WFC_MUSIC_AUDIO_MIN_SAMPLE_RATE) or
      (ASampleRate > WFC_MUSIC_AUDIO_MAX_SAMPLE_RATE) then
    AudioError(Format('sample rate must be from %d through %d',
      [WFC_MUSIC_AUDIO_MIN_SAMPLE_RATE,
       WFC_MUSIC_AUDIO_MAX_SAMPLE_RATE]));
end;

function CopyPcm16Samples(
  const AValues: TWfcMusicPcm16Samples): TWfcMusicPcm16Samples;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

{ TWfcMusicPcm16Clip }

constructor TWfcMusicPcm16Clip.Create(const ASampleRate: Integer;
  const ASamples: TWfcMusicPcm16Samples);
begin
  inherited Create;
  ValidateSampleRate(ASampleRate);
  if Length(ASamples) > WFC_MUSIC_AUDIO_MAX_SAMPLE_FRAME_COUNT then
    AudioError('PCM frame count exceeds the preview limit');
  FSampleRate := ASampleRate;
  FSamples := CopyPcm16Samples(ASamples);
end;

function TWfcMusicPcm16Clip.GetFrameCount: Integer;
begin
  Result := Length(FSamples);
end;

procedure TWfcMusicPcm16Clip.ValidateSampleIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Length(FSamples)) then
    raise ERangeError.CreateFmt('PCM sample index is out of bounds [%d]',
      [AIndex]);
end;

function TWfcMusicPcm16Clip.SampleAt(
  const AIndex: Integer): TWfcMusicPcm16Sample;
begin
  ValidateSampleIndex(AIndex);
  Result := FSamples[AIndex];
end;

function TWfcMusicPcm16Clip.CopySamples: TWfcMusicPcm16Samples;
begin
  Result := CopyPcm16Samples(FSamples);
end;

function DefaultWfcMusicAudioOptions: TWfcMusicAudioOptions;
begin
  Result.SampleRate := WFC_MUSIC_AUDIO_DEFAULT_SAMPLE_RATE;
  Result.MasterVolume := WFC_MUSIC_AUDIO_DEFAULT_MASTER_VOLUME;
  Result.AttackMilliseconds :=
    WFC_MUSIC_AUDIO_DEFAULT_ATTACK_MILLISECONDS;
  Result.ReleaseMilliseconds :=
    WFC_MUSIC_AUDIO_DEFAULT_RELEASE_MILLISECONDS;
end;

procedure ValidateOptions(const AOptions: TWfcMusicAudioOptions);
begin
  ValidateSampleRate(AOptions.SampleRate);
  if (AOptions.MasterVolume < 0) or
      (AOptions.MasterVolume > WFC_MUSIC_AUDIO_MAX_MASTER_VOLUME) then
    AudioError(Format('master volume must be from 0 through %d',
      [WFC_MUSIC_AUDIO_MAX_MASTER_VOLUME]));
  if (AOptions.AttackMilliseconds < 0) or
      (AOptions.AttackMilliseconds >
       WFC_MUSIC_AUDIO_MAX_ENVELOPE_MILLISECONDS) then
    AudioError(Format('attack must be from 0 through %d milliseconds',
      [WFC_MUSIC_AUDIO_MAX_ENVELOPE_MILLISECONDS]));
  if (AOptions.ReleaseMilliseconds < 0) or
      (AOptions.ReleaseMilliseconds >
       WFC_MUSIC_AUDIO_MAX_ENVELOPE_MILLISECONDS) then
    AudioError(Format('release must be from 0 through %d milliseconds',
      [WFC_MUSIC_AUDIO_MAX_ENVELOPE_MILLISECONDS]));
end;

procedure AdvanceAudioTime(var ATime: TWfcAudioTime;
  const ADeltaTicks, ATempo, ATicksPerQuarter: Integer);
var
  LAddedMicroseconds: TWfcAudioWideInteger;
  LNumerator: TWfcAudioWideInteger;
begin
  if ADeltaTicks < 0 then
    AudioError('tempo timeline moved backwards');
  { The largest numerator is below 2^53:
      High(Integer) * 4,000,000 + High(Integer) - 1. }
  LNumerator := TWfcAudioWideInteger(ADeltaTicks) *
    TWfcAudioWideInteger(ATempo) +
    TWfcAudioWideInteger(ATime.FractionNumerator);
  LAddedMicroseconds := LNumerator div ATicksPerQuarter;
  if LAddedMicroseconds >
      WFC_MUSIC_AUDIO_MAX_DURATION_MICROSECONDS -
      ATime.WholeMicroseconds then
    AudioError('score duration exceeds the preview limit');
  Inc(ATime.WholeMicroseconds, Integer(LAddedMicroseconds));
  ATime.FractionNumerator := Integer(LNumerator mod ATicksPerQuarter);
  if (ATime.WholeMicroseconds =
      WFC_MUSIC_AUDIO_MAX_DURATION_MICROSECONDS) and
      (ATime.FractionNumerator <> 0) then
    AudioError('score duration exceeds the preview limit');
end;

function BuildTempoAnchors(const AScore: TWfcMusicScore):
  TWfcAudioTempoAnchors;
var
  I: Integer;
  LPrevious: TWfcMusicTempoChange;
  LTempo: TWfcMusicTempoChange;
  LTime: TWfcAudioTime;
begin
  Result := nil;
  SetLength(Result, AScore.TempoCount);
  LTime.WholeMicroseconds := 0;
  LTime.FractionNumerator := 0;
  LPrevious := AScore.TempoAt(0);
  for I := 0 to AScore.TempoCount - 1 do
  begin
    LTempo := AScore.TempoAt(I);
    if LTempo.MicrosecondsPerQuarter >
        WFC_MUSIC_AUDIO_MAX_TEMPO_MICROSECONDS_PER_QUARTER then
      AudioError(Format('tempo exceeds the preview limit [%d]', [I]));
    if I > 0 then
      AdvanceAudioTime(LTime, LTempo.Tick - LPrevious.Tick,
        LPrevious.MicrosecondsPerQuarter, AScore.TicksPerQuarter);
    Result[I].Tick := LTempo.Tick;
    Result[I].WholeMicroseconds := LTime.WholeMicroseconds;
    Result[I].FractionNumerator := LTime.FractionNumerator;
    Result[I].MicrosecondsPerQuarter :=
      LTempo.MicrosecondsPerQuarter;
    LPrevious := LTempo;
  end;
end;

function AudioTimeAtTick(const AScore: TWfcMusicScore;
  const AAnchors: TWfcAudioTempoAnchors;
  const ATick: Integer): TWfcAudioTime;
var
  LHigh: Integer;
  LIndex: Integer;
  LLow: Integer;
  LMiddle: Integer;
begin
  if (ATick < 0) or (ATick > AScore.LengthTicks) then
    AudioError(Format('tick is outside the score [%d]', [ATick]));
  LLow := 0;
  LHigh := Length(AAnchors) - 1;
  LIndex := 0;
  while LLow <= LHigh do
  begin
    LMiddle := LLow + ((LHigh - LLow) div 2);
    if AAnchors[LMiddle].Tick <= ATick then
    begin
      LIndex := LMiddle;
      LLow := LMiddle + 1;
    end
    else
      LHigh := LMiddle - 1;
  end;
  Result.WholeMicroseconds := AAnchors[LIndex].WholeMicroseconds;
  Result.FractionNumerator := AAnchors[LIndex].FractionNumerator;
  AdvanceAudioTime(Result, ATick - AAnchors[LIndex].Tick,
    AAnchors[LIndex].MicrosecondsPerQuarter,
    AScore.TicksPerQuarter);
end;

function AudioTimeToFrame(const ATime: TWfcAudioTime;
  const ATicksPerQuarter, ASampleRate: Integer): Integer;
var
  LBaseFrames: TWfcAudioWideInteger;
  LDenominator: TWfcAudioWideInteger;
  LExtraNumerator: TWfcAudioWideInteger;
  LMicrosecondProduct: TWfcAudioWideInteger;
  LMicrosecondProductRemainder: TWfcAudioWideInteger;
  LResult: TWfcAudioWideInteger;
begin
  { Splitting the whole-microsecond product keeps every pas2js integer exact.
    The largest combined remainder numerator is
      999,999 * High(Integer) + (High(Integer) - 1) * 48,000,
    which is below 2^51 and therefore below the 2^53 exact-integer bound. }
  LMicrosecondProduct :=
    TWfcAudioWideInteger(ATime.WholeMicroseconds) * ASampleRate;
  LBaseFrames := LMicrosecondProduct div
    WFC_AUDIO_MICROSECONDS_PER_SECOND;
  LMicrosecondProductRemainder := LMicrosecondProduct mod
    WFC_AUDIO_MICROSECONDS_PER_SECOND;
  LExtraNumerator :=
    LMicrosecondProductRemainder * ATicksPerQuarter +
    TWfcAudioWideInteger(ATime.FractionNumerator) * ASampleRate;
  LDenominator := TWfcAudioWideInteger(ATicksPerQuarter) *
    WFC_AUDIO_MICROSECONDS_PER_SECOND;
  LResult := LBaseFrames + (LExtraNumerator div LDenominator);
  if (LResult < 0) or
      (LResult > WFC_MUSIC_AUDIO_MAX_SAMPLE_FRAME_COUNT) then
    AudioError('PCM frame count exceeds the preview limit');
  Result := Integer(LResult);
end;

function FrameAtTick(const AScore: TWfcMusicScore;
  const AAnchors: TWfcAudioTempoAnchors;
  const ASampleRate, ATick: Integer): Integer;
begin
  Result := AudioTimeToFrame(AudioTimeAtTick(AScore, AAnchors,
    ATick), AScore.TicksPerQuarter, ASampleRate);
end;

function FrequencyQ12(const APitch: Integer): Integer;
var
  LMultiplier: Integer;
begin
  LMultiplier := 1 shl (APitch div 12);
  Result := WFC_AUDIO_BASE_FREQUENCY_Q12[APitch mod 12] *
    LMultiplier;
end;

function PhaseIncrement(const APitch, ASampleRate: Integer): Integer;
var
  LDenominator: TWfcAudioWideInteger;
  LNumerator: TWfcAudioWideInteger;
begin
  LDenominator := TWfcAudioWideInteger(ASampleRate) *
    WFC_AUDIO_FREQUENCY_SCALE;
  LNumerator := TWfcAudioWideInteger(FrequencyQ12(APitch)) *
    WFC_AUDIO_PHASE_MODULUS;
  Result := Integer((LNumerator + LDenominator div 2) div
    LDenominator);
end;

function TriangleSample(const APhase: Integer): Integer;
var
  LOffset: Integer;
  LValue: Integer;
begin
  LOffset := APhase and (WFC_AUDIO_PHASE_QUARTER - 1);
  LValue := Integer((TWfcAudioWideInteger(LOffset) *
    WFC_AUDIO_ENVELOPE_SCALE) div
    WFC_AUDIO_PHASE_QUARTER);
  case APhase div WFC_AUDIO_PHASE_QUARTER of
    0: Result := LValue;
    1: Result := WFC_AUDIO_ENVELOPE_SCALE - LValue;
    2: Result := -LValue;
  else
    Result := -WFC_AUDIO_ENVELOPE_SCALE + LValue;
  end;
end;

function EnvelopeGain(const AOffset, AFrameCount,
  AAttackFrames, AReleaseFrames: Integer): Integer;
var
  LReleaseOffset: Integer;
  LValue: Integer;
begin
  Result := WFC_AUDIO_ENVELOPE_SCALE;
  if AAttackFrames > 0 then
  begin
    LValue := Integer((TWfcAudioWideInteger(AOffset) *
      WFC_AUDIO_ENVELOPE_SCALE) div
      AAttackFrames);
    if LValue < Result then
      Result := LValue;
  end;
  if AReleaseFrames > 0 then
  begin
    LReleaseOffset := AFrameCount - 1 - AOffset;
    LValue := Integer((TWfcAudioWideInteger(LReleaseOffset) *
      WFC_AUDIO_ENVELOPE_SCALE) div AReleaseFrames);
    if LValue < Result then
      Result := LValue;
  end;
end;

procedure ValidateScoreShape(const AScore: TWfcMusicScore);
begin
  if AScore = nil then
    AudioError('score cannot be nil');
  if AScore.StepsPerOctave <> 12 then
    AudioError('preview synthesis requires exactly 12 steps per octave');
  if AScore.TrackCount > WFC_MUSIC_AUDIO_MAX_TRACK_COUNT then
    AudioError('track count exceeds the preview limit');
  if AScore.VoiceCount > WFC_MUSIC_AUDIO_MAX_VOICE_COUNT then
    AudioError('voice count exceeds the preview limit');
  if AScore.MeterCount > WFC_MUSIC_AUDIO_MAX_METER_COUNT then
    AudioError('meter count exceeds the preview limit');
  if AScore.TempoCount > WFC_MUSIC_AUDIO_MAX_TEMPO_COUNT then
    AudioError('tempo count exceeds the preview limit');
  if AScore.SpanCount > WFC_MUSIC_AUDIO_MAX_SPAN_COUNT then
    AudioError('span count exceeds the preview limit');
end;

procedure ValidateSpansAndPlan(const AScore: TWfcMusicScore;
  const AAnchors: TWfcAudioTempoAnchors; const ASampleRate: Integer;
  out AHeadroom, ATotalTones: Integer;
  out ARenderVisits: TWfcAudioWideInteger);
var
  I: Integer;
  J: Integer;
  LEndFrame: Integer;
  LFrameCount: Integer;
  LMaxVoiceTones: TWfcAudioIntegerArray;
  LSpan: TWfcMusicSpanEvent;
  LStartFrame: Integer;
begin
  AHeadroom := 0;
  ATotalTones := 0;
  ARenderVisits := 0;
  LMaxVoiceTones := nil;
  SetLength(LMaxVoiceTones, AScore.VoiceCount);
  for I := 0 to AScore.SpanCount - 1 do
  begin
    { SpanAt makes at most one detached tone array live at a time. }
    LSpan := AScore.SpanAt(I);
    if Length(LSpan.Tones) > WFC_MUSIC_AUDIO_MAX_TONES_PER_SPAN then
      AudioError(Format('tone count exceeds the per-span limit [%d]', [I]));
    if Length(LSpan.Tones) >
        WFC_MUSIC_AUDIO_MAX_TOTAL_TONE_COUNT - ATotalTones then
      AudioError('total tone count exceeds the preview limit');
    Inc(ATotalTones, Length(LSpan.Tones));
    for J := 0 to Length(LSpan.Tones) - 1 do
      if (LSpan.Tones[J].Pitch < 0) or
          (LSpan.Tones[J].Pitch > 127) then
        AudioError(Format('pitch must be from 0 through 127 [%d, %d]',
          [I, J]));
    if Length(LSpan.Tones) > 0 then
    begin
      LStartFrame := FrameAtTick(AScore, AAnchors, ASampleRate,
        LSpan.StartTick);
      LEndFrame := FrameAtTick(AScore, AAnchors, ASampleRate,
        LSpan.StartTick + LSpan.DurationTicks);
      LFrameCount := LEndFrame - LStartFrame;
      if LFrameCount < 0 then
        AudioError(Format('span frame range moved backwards [%d]', [I]));
      if (LFrameCount > 0) and
          (Length(LSpan.Tones) >
           LMaxVoiceTones[LSpan.VoiceIndex]) then
        LMaxVoiceTones[LSpan.VoiceIndex] := Length(LSpan.Tones);
      ARenderVisits := ARenderVisits +
        TWfcAudioWideInteger(LFrameCount) * Length(LSpan.Tones);
      if ARenderVisits > WFC_MUSIC_AUDIO_MAX_RENDER_VISIT_COUNT then
        AudioError('tone-frame visits exceed the preview limit');
    end;
  end;
  for I := 0 to Length(LMaxVoiceTones) - 1 do
    Inc(AHeadroom, LMaxVoiceTones[I]);
  if AHeadroom < 1 then
    AHeadroom := 1;
end;

procedure RenderTone(AMix: TWfcAudioIntegerArray;
  const AStartFrame, AEndFrame, APitch, AVelocity, AHeadroom: Integer;
  const AOptions: TWfcMusicAudioOptions);
var
  I: Integer;
  LAttackFrames: Integer;
  LEnvelope: Integer;
  LFrameCount: Integer;
  LPhase: Integer;
  LPhaseIncrement: Integer;
  LReleaseFrames: Integer;
  LSample: TWfcAudioWideInteger;
begin
  LFrameCount := AEndFrame - AStartFrame;
  if LFrameCount <= 0 then
    Exit;
  LAttackFrames := (AOptions.SampleRate *
    AOptions.AttackMilliseconds) div 1000;
  LReleaseFrames := (AOptions.SampleRate *
    AOptions.ReleaseMilliseconds) div 1000;
  LPhase := 0;
  LPhaseIncrement := PhaseIncrement(APitch, AOptions.SampleRate);
  for I := 0 to LFrameCount - 1 do
  begin
    LEnvelope := EnvelopeGain(I, LFrameCount,
      LAttackFrames, LReleaseFrames);
    LSample := TWfcAudioWideInteger(TriangleSample(LPhase)) * AVelocity;
    LSample := (LSample div 127) * AOptions.MasterVolume;
    LSample := (LSample div 127) * LEnvelope;
    LSample := (LSample div WFC_AUDIO_ENVELOPE_SCALE) div AHeadroom;
    Inc(AMix[AStartFrame + I], Integer(LSample));
    LPhase := (LPhase + LPhaseIncrement) and WFC_AUDIO_PHASE_MASK;
  end;
end;

function RenderWfcMusicAudio(const AScore: TWfcMusicScore;
  const AOptions: TWfcMusicAudioOptions): TWfcMusicPcm16Clip;
var
  I: Integer;
  J: Integer;
  LAnchors: TWfcAudioTempoAnchors;
  LEndFrame: Integer;
  LFrameCount: Integer;
  LHeadroom: Integer;
  LMix: TWfcAudioIntegerArray;
  LRenderVisits: TWfcAudioWideInteger;
  LSamples: TWfcMusicPcm16Samples;
  LSpan: TWfcMusicSpanEvent;
  LStartFrame: Integer;
  LTotalTones: Integer;
begin
  Result := nil;
  ValidateScoreShape(AScore);
  ValidateOptions(AOptions);
  LAnchors := BuildTempoAnchors(AScore);
  LFrameCount := FrameAtTick(AScore, LAnchors,
    AOptions.SampleRate, AScore.LengthTicks);
  if LFrameCount < 1 then
    AudioError('score duration quantizes to zero PCM frames');
  ValidateSpansAndPlan(AScore, LAnchors, AOptions.SampleRate,
    LHeadroom, LTotalTones, LRenderVisits);

  LMix := nil;
  SetLength(LMix, LFrameCount);
  if AOptions.MasterVolume > 0 then
    for I := 0 to AScore.SpanCount - 1 do
    begin
      LSpan := AScore.SpanAt(I);
      if Length(LSpan.Tones) = 0 then
        Continue;
      LStartFrame := FrameAtTick(AScore, LAnchors,
        AOptions.SampleRate, LSpan.StartTick);
      LEndFrame := FrameAtTick(AScore, LAnchors,
        AOptions.SampleRate,
        LSpan.StartTick + LSpan.DurationTicks);
      for J := 0 to Length(LSpan.Tones) - 1 do
        RenderTone(LMix, LStartFrame, LEndFrame,
          LSpan.Tones[J].Pitch, LSpan.Tones[J].Velocity,
          LHeadroom, AOptions);
    end;

  LSamples := nil;
  SetLength(LSamples, LFrameCount);
  for I := 0 to LFrameCount - 1 do
    if LMix[I] < Low(SmallInt) then
      LSamples[I] := Low(SmallInt)
    else if LMix[I] > High(SmallInt) then
      LSamples[I] := High(SmallInt)
    else
      LSamples[I] := SmallInt(LMix[I]);
  Result := TWfcMusicPcm16Clip.Create(AOptions.SampleRate, LSamples);
end;

procedure PutAscii(var ABytes: TWfcMusicAudioBytes;
  const AOffset: Integer; const AText: String);
var
  I: Integer;
begin
  for I := 1 to Length(AText) do
    ABytes[AOffset + I - 1] := Byte(Ord(AText[I]));
end;

procedure PutU16LE(var ABytes: TWfcMusicAudioBytes;
  const AOffset, AValue: Integer);
begin
  ABytes[AOffset] := Byte(AValue and $FF);
  ABytes[AOffset + 1] := Byte((AValue shr 8) and $FF);
end;

procedure PutU32LE(var ABytes: TWfcMusicAudioBytes;
  const AOffset: Integer; const AValue: Cardinal);
begin
  ABytes[AOffset] := Byte(AValue and $FF);
  ABytes[AOffset + 1] := Byte((AValue shr 8) and $FF);
  ABytes[AOffset + 2] := Byte((AValue shr 16) and $FF);
  ABytes[AOffset + 3] := Byte((AValue shr 24) and $FF);
end;

function EncodeWfcMusicWave(
  const AClip: TWfcMusicPcm16Clip): TWfcMusicAudioBytes;
var
  I: Integer;
  LDataByteCount: Integer;
  LOffset: Integer;
  LSampleValue: Integer;
begin
  Result := nil;
  if AClip = nil then
    WaveError('clip cannot be nil');
  if AClip.FrameCount > WFC_MUSIC_AUDIO_MAX_SAMPLE_FRAME_COUNT then
    WaveError('PCM frame count exceeds the preview limit');
  LDataByteCount := AClip.FrameCount * 2;
  if LDataByteCount > WFC_MUSIC_AUDIO_MAX_WAVE_BYTE_COUNT - 44 then
    WaveError('PCM byte count exceeds the WAVE limit');
  SetLength(Result, 44 + LDataByteCount);
  PutAscii(Result, 0, 'RIFF');
  PutU32LE(Result, 4, Cardinal(36 + LDataByteCount));
  PutAscii(Result, 8, 'WAVE');
  PutAscii(Result, 12, 'fmt ');
  PutU32LE(Result, 16, 16);
  PutU16LE(Result, 20, 1);
  PutU16LE(Result, 22, WFC_MUSIC_AUDIO_CHANNEL_COUNT);
  PutU32LE(Result, 24, Cardinal(AClip.SampleRate));
  PutU32LE(Result, 28, Cardinal(AClip.SampleRate * 2));
  PutU16LE(Result, 32, 2);
  PutU16LE(Result, 34, WFC_MUSIC_AUDIO_BITS_PER_SAMPLE);
  PutAscii(Result, 36, 'data');
  PutU32LE(Result, 40, Cardinal(LDataByteCount));
  LOffset := 44;
  for I := 0 to AClip.FrameCount - 1 do
  begin
    LSampleValue := AClip.SampleAt(I);
    if LSampleValue < 0 then
      Inc(LSampleValue, 65536);
    PutU16LE(Result, LOffset, LSampleValue);
    Inc(LOffset, 2);
  end;
end;

end.
