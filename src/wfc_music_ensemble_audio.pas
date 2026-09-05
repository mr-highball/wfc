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
unit wfc_music_ensemble_audio;

{$mode delphi}{$H+}

interface

uses wfc_music_audio, wfc_music_ensemble;

type
  {$IFDEF PAS2JS}
  TWfcMusicEnsembleAudioCount = NativeInt;
  {$ELSE}
  TWfcMusicEnsembleAudioCount = Int64;
  {$ENDIF}

const
  WFC_MUSIC_ENSEMBLE_AUDIO_VERSION = 1;
  WFC_MUSIC_ENSEMBLE_AUDIO_BLOCK_FRAMES = 2048;
  WFC_MUSIC_ENSEMBLE_AUDIO_MAX_SAFE_INTEGER: TWfcMusicEnsembleAudioCount =
    9007199254740991;

type
  EWfcMusicEnsembleAudio = class(EWfcMusicAudio);
  TWfcMusicEnsembleAudioVoiceCapacities = array of Integer;
  TWfcMusicEnsembleAudioClock = record
    TickCount: TWfcMusicEnsembleAudioCount;
    FrameCount: TWfcMusicEnsembleAudioCount;
    FractionNumerator: TWfcMusicEnsembleAudioCount;
  end;

  { Pull-based, mono PCM16 triangle synthesis for ordered ensemble frames.
    Pitches are 12-step MIDI 0..127; rates/envelopes use AudioOptions' adapter
    bounds. Those synthesis bounds are not limits of the ensemble model.

    Capacities contain one nonnegative maximum chord size per voice, including
    silent slots. Their checked sum (at least one) fixes headroom for the whole
    stream. No voice/chord/song-length policy limit is imposed. Counts remain
    exact through MAX_SAFE_INTEGER; managed array counts fit Integer.

    AdmitFrame deep-copies its input and must wait until NeedsInput. Drain
    ReadSamples until False before admitting the next frame. A zero-sample
    interval still applies its attacks/rests/holds. Hold requires an identical
    predecessor in that voice, including velocities; it never resets phase.
    Segment boundaries have no special meaning to this renderer.

    A ReleaseFrames+1 mix ring (one working slot), one active tone array per
    voice, and one output block are retained. Output is delayed by ReleaseFrames so
    a real note end can shape its last samples without advance knowledge of
    its duration. EndInput closes all notes and drains the existing duration;
    it never appends a release tail. Finish is observed through Finished.

    ReadSamples accepts any positive Integer maximum, returning at most 2048
    samples. Returned arrays belong to the caller and never alias renderer
    storage. False always returns nil; inspect NeedsInput/Finished/Cancelled.
    Invalid arguments/admission state reject before mutation and are retryable.
    Unexpected processing errors set Failed and rethrow; further reads/writes
    reject. Cancel is terminal and discards all not-yet-returned PCM, without
    emitting or closing notes. Destruction neither drains nor calls the host.
    Calls are sequential and not thread-safe; there are no host callbacks. }
  TWfcMusicEnsembleAudioRenderer = class
  private
    type
      TToneState = record
        Pitch, Velocity, Phase, Increment: Integer;
      end;
      TToneStates = array of TToneState;
      TVoiceState = record
        StartFrame: TWfcMusicEnsembleAudioCount;
        Tones: TToneStates;
      end;
      TVoiceStates = array of TVoiceState;
      TIntegerArray = array of Integer;
    var
      FOptions: TWfcMusicAudioOptions;
      FTicksPerQuarter, FHeadroom, FAttackFrames, FReleaseFrames: Integer;
      FCapacities: TWfcMusicEnsembleAudioVoiceCapacities;
      FVoices: TVoiceStates;
      FClock: TWfcMusicEnsembleAudioClock;
      FRemainingFrames, FRenderedFrames, FEmittedFrames:
        TWfcMusicEnsembleAudioCount;
      FRing: TIntegerArray;
      FReadIndex, FWriteIndex, FPending: Integer;
      FInputEnded, FFinished, FCancelled, FFailed: Boolean;
      function GetNeedsInput: Boolean;
      function GetInputTicks: TWfcMusicEnsembleAudioCount;
      function GetFrameCount: TWfcMusicEnsembleAudioCount;
      procedure CheckActive;
      procedure ValidateFrame(const AFrame: TWfcMusicEnsembleFrame);
      procedure CloseVoice(const AIndex: Integer);
      procedure RenderOne;
      function ToneSample(const APhase, AVelocity, AGain: Integer): Integer;
      function AttackGain(const AAge: TWfcMusicEnsembleAudioCount): Integer;
  public
    constructor Create(const AOptions: TWfcMusicAudioOptions;
      const ATicksPerQuarter: Integer;
      const ACapacities: TWfcMusicEnsembleAudioVoiceCapacities);
    procedure AdmitFrame(const AFrame: TWfcMusicEnsembleFrame;
      const ALengthTicks, ATempoMicrosecondsPerQuarter: Integer);
    function ReadSamples(const AMaxFrames: Integer;
      out ASamples: TWfcMusicPcm16Samples): Boolean;
    procedure EndInput;
    procedure Cancel;
    property NeedsInput: Boolean read GetNeedsInput;
    property InputEnded: Boolean read FInputEnded;
    property Finished: Boolean read FFinished;
    property Cancelled: Boolean read FCancelled;
    property Failed: Boolean read FFailed;
    property InputTicks: TWfcMusicEnsembleAudioCount read GetInputTicks;
    property FrameCount: TWfcMusicEnsembleAudioCount read GetFrameCount;
    property RenderedFrames: TWfcMusicEnsembleAudioCount read FRenderedFrames;
    property EmittedFrames: TWfcMusicEnsembleAudioCount read FEmittedFrames;
    property Headroom: Integer read FHeadroom;
    property LatencyFrames: Integer read FReleaseFrames;
  end;

{ Start with Default(TWfcMusicEnsembleAudioClock). Keep TPQ and sample rate
  unchanged throughout one clock; tempo may change on every call. The fraction
  is measured in 1/(TPQ*1000000) sample frames. This is floor-of-absolute-time,
  not a sum of independently rounded intervals. Zero ticks is a validating
  no-op. Invalid input/overflow leaves the complete clock unchanged. }
procedure AdvanceWfcMusicEnsembleAudioClock(
  var AClock: TWfcMusicEnsembleAudioClock;
  const ALengthTicks, ATempoMicrosecondsPerQuarter, ATicksPerQuarter,
  ASampleRate: Integer);

implementation

uses SysUtils, wfc_music_sequence;

const
  MICROSECONDS_PER_SECOND = 1000000;
  PHASE_MODULUS = 16777216;
  PHASE_MASK = PHASE_MODULUS - 1;
  PHASE_QUARTER = PHASE_MODULUS div 4;
  ENVELOPE_SCALE = 32767;
  FREQUENCY_SCALE = 4096;
  { Same pinned Q12 table and integer operation order as audio preview v1.
    Kept local so the established preview implementation remains unchanged. }
  BASE_FREQUENCY_Q12: array[0..11] of Integer = (
    33488, 35479, 37589, 39824, 42192, 44701,
    47359, 50175, 53159, 56320, 59669, 63217);

procedure AudioError(const AMessage: String);
begin
  raise EWfcMusicEnsembleAudio.Create('cannot render ensemble PCM: ' + AMessage);
end;

procedure ValidateCount(const AValue: TWfcMusicEnsembleAudioCount;
  const AName: String);
begin
  if (AValue < 0) or
    (AValue > WFC_MUSIC_ENSEMBLE_AUDIO_MAX_SAFE_INTEGER) then
    AudioError(AName + ' exceeds the nonnegative exact integer envelope');
  {$IFDEF PAS2JS}
  if AValue <> Trunc(AValue) then AudioError(AName + ' must be an exact integer');
  {$ENDIF}
end;

procedure ValidateInteger(const AValue: Integer; const AName: String);
begin
  {$IFDEF PAS2JS}
  if (AValue <> Trunc(AValue)) or (AValue < Low(Integer)) or
    (AValue > High(Integer)) then AudioError(AName + ' must fit Integer');
  {$ENDIF}
end;

procedure ValidateTiming(const ATempo, ATicksPerQuarter, ASampleRate: Integer);
begin
  ValidateInteger(ATempo, 'tempo');
  ValidateInteger(ATicksPerQuarter, 'ticks per quarter');
  ValidateInteger(ASampleRate, 'sample rate');
  if (ATempo < 1) or
    (ATempo > WFC_MUSIC_AUDIO_MAX_TEMPO_MICROSECONDS_PER_QUARTER) then
    AudioError('tempo exceeds the 1..4000000 synthesis range');
  if ATicksPerQuarter < 1 then AudioError('ticks per quarter must be positive');
  if (ASampleRate < WFC_MUSIC_AUDIO_MIN_SAMPLE_RATE) or
    (ASampleRate > WFC_MUSIC_AUDIO_MAX_SAMPLE_RATE) then
    AudioError('sample rate exceeds the 32000..48000 synthesis range');
end;

procedure AdvanceWfcMusicEnsembleAudioClock(
  var AClock: TWfcMusicEnsembleAudioClock;
  const ALengthTicks, ATempoMicrosecondsPerQuarter, ATicksPerQuarter,
  ASampleRate: Integer);
var
  LNumerator, LWholeUs, LTickRemainder, LFrames, LSubFrames,
    LFraction, LDenominator: TWfcMusicEnsembleAudioCount;
  LNext: TWfcMusicEnsembleAudioClock;
begin
  ValidateTiming(ATempoMicrosecondsPerQuarter, ATicksPerQuarter, ASampleRate);
  ValidateInteger(ALengthTicks, 'length ticks');
  if ALengthTicks < 0 then AudioError('length ticks must be nonnegative');
  ValidateCount(AClock.TickCount, 'clock ticks');
  ValidateCount(AClock.FrameCount, 'clock frames');
  ValidateCount(AClock.FractionNumerator, 'clock fraction');
  LDenominator := TWfcMusicEnsembleAudioCount(ATicksPerQuarter) *
    MICROSECONDS_PER_SECOND;
  if AClock.FractionNumerator >= LDenominator then
    AudioError('clock fraction must be normalized');
  if (AClock.TickCount = 0) and
    ((AClock.FrameCount <> 0) or (AClock.FractionNumerator <> 0)) then
    AudioError('zero-tick clock must be zero');
  if ALengthTicks > WFC_MUSIC_ENSEMBLE_AUDIO_MAX_SAFE_INTEGER -
    AClock.TickCount then AudioError('tick total exceeds exact integer range');

  { Avoid totalTicks*tempo*rate and totalMicroseconds*rate. Largest tick/tempo
    product is 8589934588000000 (<2^53). Splitting whole seconds first keeps
    frame products exact; the combined fractional numerator is <2^52. }
  LNumerator := TWfcMusicEnsembleAudioCount(ALengthTicks) *
    ATempoMicrosecondsPerQuarter;
  LWholeUs := LNumerator div ATicksPerQuarter;
  LTickRemainder := LNumerator mod ATicksPerQuarter;
  LFrames := (LWholeUs div MICROSECONDS_PER_SECOND) * ASampleRate;
  LSubFrames := (LWholeUs mod MICROSECONDS_PER_SECOND) * ASampleRate;
  LFrames := LFrames + LSubFrames div MICROSECONDS_PER_SECOND;
  LFraction := AClock.FractionNumerator +
    (LSubFrames mod MICROSECONDS_PER_SECOND) * ATicksPerQuarter +
    LTickRemainder * ASampleRate;
  LFrames := LFrames + LFraction div LDenominator;
  if LFrames > WFC_MUSIC_ENSEMBLE_AUDIO_MAX_SAFE_INTEGER -
    AClock.FrameCount then AudioError('frame total exceeds exact integer range');
  LNext.TickCount := AClock.TickCount + ALengthTicks;
  LNext.FrameCount := AClock.FrameCount + LFrames;
  LNext.FractionNumerator := LFraction mod LDenominator;
  AClock := LNext;
end;

function PhaseIncrement(const APitch, ARate: Integer): Integer;
var LFrequency, LDenominator: TWfcMusicEnsembleAudioCount;
begin
  LFrequency := BASE_FREQUENCY_Q12[APitch mod 12] * (1 shl (APitch div 12));
  LDenominator := TWfcMusicEnsembleAudioCount(ARate) * FREQUENCY_SCALE;
  Result := Integer((LFrequency * PHASE_MODULUS + LDenominator div 2) div
    LDenominator);
end;

function TriangleSample(const APhase: Integer): Integer;
var LValue: Integer;
begin
  LValue := Integer((TWfcMusicEnsembleAudioCount(APhase and
    (PHASE_QUARTER - 1)) * ENVELOPE_SCALE) div PHASE_QUARTER);
  case APhase div PHASE_QUARTER of
    0: Result := LValue;
    1: Result := ENVELOPE_SCALE - LValue;
    2: Result := -LValue;
  else Result := -ENVELOPE_SCALE + LValue;
  end;
end;

constructor TWfcMusicEnsembleAudioRenderer.Create(
  const AOptions: TWfcMusicAudioOptions; const ATicksPerQuarter: Integer;
  const ACapacities: TWfcMusicEnsembleAudioVoiceCapacities);
var I, LSum: Integer;
begin
  inherited Create;
  ValidateTiming(1, ATicksPerQuarter, AOptions.SampleRate);
  ValidateInteger(AOptions.MasterVolume, 'master volume');
  ValidateInteger(AOptions.AttackMilliseconds, 'attack milliseconds');
  ValidateInteger(AOptions.ReleaseMilliseconds, 'release milliseconds');
  if (AOptions.MasterVolume < 0) or (AOptions.MasterVolume > 127) then
    AudioError('master volume must be in 0..127');
  if (AOptions.AttackMilliseconds < 0) or
    (AOptions.AttackMilliseconds > WFC_MUSIC_AUDIO_MAX_ENVELOPE_MILLISECONDS) or
    (AOptions.ReleaseMilliseconds < 0) or
    (AOptions.ReleaseMilliseconds > WFC_MUSIC_AUDIO_MAX_ENVELOPE_MILLISECONDS) then
    AudioError('attack/release milliseconds must be in 0..1000');
  if (Length(ACapacities) < 1) or (Length(ACapacities) > High(Integer)) then
    AudioError('voice capacity count must be positive and fit Integer');
  LSum := 0;
  for I := 0 to High(ACapacities) do
  begin
    ValidateInteger(ACapacities[I], 'voice capacity');
    if ACapacities[I] < 0 then AudioError('voice capacity must be nonnegative');
    if ACapacities[I] > High(Integer) - LSum then
      AudioError('capacity sum must fit Integer');
    Inc(LSum, ACapacities[I]);
  end;
  FOptions := AOptions;
  FTicksPerQuarter := ATicksPerQuarter;
  FHeadroom := LSum;
  if FHeadroom = 0 then FHeadroom := 1;
  FAttackFrames := AOptions.SampleRate * AOptions.AttackMilliseconds div 1000;
  FReleaseFrames := AOptions.SampleRate * AOptions.ReleaseMilliseconds div 1000;
  SetLength(FCapacities, Length(ACapacities));
  SetLength(FVoices, Length(ACapacities));
  for I := 0 to High(ACapacities) do FCapacities[I] := ACapacities[I];
  SetLength(FRing, FReleaseFrames + 1);
end;

function TWfcMusicEnsembleAudioRenderer.GetNeedsInput: Boolean;
begin
  Result := not (FInputEnded or FCancelled or FFailed) and (FRemainingFrames = 0);
end;

function TWfcMusicEnsembleAudioRenderer.GetInputTicks: TWfcMusicEnsembleAudioCount;
begin
  Result := FClock.TickCount;
end;

function TWfcMusicEnsembleAudioRenderer.GetFrameCount: TWfcMusicEnsembleAudioCount;
begin
  Result := FClock.FrameCount;
end;

procedure TWfcMusicEnsembleAudioRenderer.CheckActive;
begin
  if FFailed then AudioError('a previous processing operation failed');
  if FCancelled then AudioError('renderer has been cancelled');
end;

procedure TWfcMusicEnsembleAudioRenderer.ValidateFrame(
  const AFrame: TWfcMusicEnsembleFrame);
var I, J: Integer;
begin
  if Length(AFrame.Voices) <> Length(FVoices) then
    AudioError('frame voice count does not match capacities');
  for I := 0 to High(AFrame.Voices) do
  begin
    case AFrame.Voices[I].Action of
      wmcaRest, wmcaAttack, wmcaHold: ;
    else AudioError('invalid voice action');
    end;
    if Length(AFrame.Voices[I].Tones) > FCapacities[I] then
      AudioError('voice chord exceeds its declared tone capacity');
    if AFrame.Voices[I].Action = wmcaRest then
    begin
      if Length(AFrame.Voices[I].Tones) <> 0 then AudioError('rest contains tones');
      Continue;
    end;
    if Length(AFrame.Voices[I].Tones) = 0 then AudioError('sound has no tones');
    if (AFrame.Voices[I].Action = wmcaHold) and
      (Length(AFrame.Voices[I].Tones) <> Length(FVoices[I].Tones)) then
      AudioError('hold has no identical active predecessor');
    for J := 0 to High(AFrame.Voices[I].Tones) do
    begin
      ValidateInteger(AFrame.Voices[I].Tones[J].Pitch, 'pitch');
      ValidateInteger(AFrame.Voices[I].Tones[J].Velocity, 'velocity');
      if (AFrame.Voices[I].Tones[J].Pitch < 0) or
        (AFrame.Voices[I].Tones[J].Pitch > 127) then
        AudioError('pitch exceeds the 12-step MIDI 0..127 synthesis range');
      if (AFrame.Voices[I].Tones[J].Velocity < 1) or
        (AFrame.Voices[I].Tones[J].Velocity > 127) then
        AudioError('tone velocity must be in 1..127');
      if (J > 0) and (AFrame.Voices[I].Tones[J-1].Pitch >=
        AFrame.Voices[I].Tones[J].Pitch) then
        AudioError('tone pitches must be strictly increasing');
      if AFrame.Voices[I].Action = wmcaHold then
        if (AFrame.Voices[I].Tones[J].Pitch <> FVoices[I].Tones[J].Pitch) or
          (AFrame.Voices[I].Tones[J].Velocity <> FVoices[I].Tones[J].Velocity) then
          AudioError('hold changes an active pitch or velocity');
    end;
  end;
end;

function TWfcMusicEnsembleAudioRenderer.AttackGain(
  const AAge: TWfcMusicEnsembleAudioCount): Integer;
begin
  Result := ENVELOPE_SCALE;
  { Never multiply an arbitrary hold age by a gain: only ramp-local offsets
    below 48000 are multiplied. }
  if (FAttackFrames > 0) and (AAge < FAttackFrames) then
    Result := Integer(AAge * ENVELOPE_SCALE div FAttackFrames);
end;

function TWfcMusicEnsembleAudioRenderer.ToneSample(
  const APhase, AVelocity, AGain: Integer): Integer;
var LSample: TWfcMusicEnsembleAudioCount;
begin
  LSample := TWfcMusicEnsembleAudioCount(TriangleSample(APhase)) * AVelocity;
  LSample := (LSample div 127) * FOptions.MasterVolume;
  LSample := (LSample div 127) * AGain;
  Result := Integer((LSample div ENVELOPE_SCALE) div FHeadroom);
end;

procedure TWfcMusicEnsembleAudioRenderer.CloseVoice(const AIndex: Integer);
var
  LAge: TWfcMusicEnsembleAudioCount;
  LCount, LDistance, LRingIndex, J, LPhase, LAttack, LGain, LRelease: Integer;
begin
  if (Length(FVoices[AIndex].Tones) = 0) or (FReleaseFrames = 0) then Exit;
  LAge := FRenderedFrames - FVoices[AIndex].StartFrame;
  LCount := FReleaseFrames;
  if LAge < LCount then LCount := Integer(LAge);
  LRingIndex := FWriteIndex;
  for LDistance := 1 to LCount do
  begin
    Dec(LRingIndex);
    if LRingIndex < 0 then LRingIndex := High(FRing);
    LAttack := AttackGain(LAge - LDistance);
    LRelease := (LDistance - 1) * ENVELOPE_SCALE div FReleaseFrames;
    LGain := LAttack;
    if LRelease < LGain then LGain := LRelease;
    for J := 0 to High(FVoices[AIndex].Tones) do
    begin
      { Rewind from the next phase by at most the release window. The product
        stays below 2^39; elapsed song length never enters phase arithmetic. }
      LPhase := FVoices[AIndex].Tones[J].Phase - Integer(
        (TWfcMusicEnsembleAudioCount(FVoices[AIndex].Tones[J].Increment) *
        LDistance) mod PHASE_MODULUS);
      if LPhase < 0 then Inc(LPhase, PHASE_MODULUS);
      FRing[LRingIndex] := FRing[LRingIndex] -
        ToneSample(LPhase, FVoices[AIndex].Tones[J].Velocity, LAttack) +
        ToneSample(LPhase, FVoices[AIndex].Tones[J].Velocity, LGain);
    end;
  end;
end;

procedure TWfcMusicEnsembleAudioRenderer.AdmitFrame(
  const AFrame: TWfcMusicEnsembleFrame;
  const ALengthTicks, ATempoMicrosecondsPerQuarter: Integer);
var LNext: TVoiceStates; LClock: TWfcMusicEnsembleAudioClock; I, J: Integer;
begin
  CheckActive;
  if not NeedsInput then AudioError('drain the previous frame before admission');
  if ALengthTicks < 1 then AudioError('admitted frame length must be positive');
  LClock := FClock;
  AdvanceWfcMusicEnsembleAudioClock(LClock, ALengthTicks,
    ATempoMicrosecondsPerQuarter, FTicksPerQuarter, FOptions.SampleRate);
  ValidateFrame(AFrame);
  { Stage every allocation before closing notes or publishing the new clock. }
  SetLength(LNext, Length(FVoices));
  for I := 0 to High(LNext) do
  begin
    LNext[I].StartFrame := FRenderedFrames;
    SetLength(LNext[I].Tones, Length(AFrame.Voices[I].Tones));
    if AFrame.Voices[I].Action = wmcaHold then
      LNext[I].StartFrame := FVoices[I].StartFrame;
    for J := 0 to High(LNext[I].Tones) do
      if AFrame.Voices[I].Action = wmcaHold then
        LNext[I].Tones[J] := FVoices[I].Tones[J]
      else
      begin
        LNext[I].Tones[J].Pitch := AFrame.Voices[I].Tones[J].Pitch;
        LNext[I].Tones[J].Velocity := AFrame.Voices[I].Tones[J].Velocity;
        LNext[I].Tones[J].Increment := PhaseIncrement(
          AFrame.Voices[I].Tones[J].Pitch, FOptions.SampleRate);
      end;
  end;
  try
    for I := 0 to High(FVoices) do
      if AFrame.Voices[I].Action <> wmcaHold then CloseVoice(I);
    FRemainingFrames := LClock.FrameCount - FClock.FrameCount;
    FClock := LClock;
    FVoices := LNext;
  except
    FFailed := True;
    raise;
  end;
end;

procedure TWfcMusicEnsembleAudioRenderer.RenderOne;
var I, J, LGain, LSample: Integer;
begin
  LSample := 0;
  for I := 0 to High(FVoices) do
  begin
    LGain := AttackGain(FRenderedFrames - FVoices[I].StartFrame);
    for J := 0 to High(FVoices[I].Tones) do
    begin
      LSample := LSample + ToneSample(FVoices[I].Tones[J].Phase,
        FVoices[I].Tones[J].Velocity, LGain);
      FVoices[I].Tones[J].Phase := (FVoices[I].Tones[J].Phase +
        FVoices[I].Tones[J].Increment) and PHASE_MASK;
    end;
  end;
  FRing[FWriteIndex] := LSample;
  Inc(FWriteIndex);
  if FWriteIndex = Length(FRing) then FWriteIndex := 0;
  Inc(FPending);
  Inc(FRenderedFrames);
  Dec(FRemainingFrames);
end;

function TWfcMusicEnsembleAudioRenderer.ReadSamples(const AMaxFrames: Integer;
  out ASamples: TWfcMusicPcm16Samples): Boolean;
var LCount, LLimit, LSample: Integer;
begin
  ASamples := nil;
  ValidateInteger(AMaxFrames, 'maximum output frames');
  if AMaxFrames < 1 then AudioError('maximum output frames must be positive');
  if FFailed then AudioError('a previous processing operation failed');
  if FCancelled or FFinished then Exit(False);
  LLimit := AMaxFrames;
  if LLimit > WFC_MUSIC_ENSEMBLE_AUDIO_BLOCK_FRAMES then
    LLimit := WFC_MUSIC_ENSEMBLE_AUDIO_BLOCK_FRAMES;
  SetLength(ASamples, LLimit);
  LCount := 0;
  try
    while LCount < LLimit do
    begin
      if (FPending > FReleaseFrames) or (FInputEnded and (FPending > 0)) then
      begin
        LSample := FRing[FReadIndex];
        if LSample < -32768 then LSample := -32768;
        if LSample > 32767 then LSample := 32767;
        ASamples[LCount] := TWfcMusicPcm16Sample(LSample);
        Inc(LCount);
        Inc(FReadIndex);
        if FReadIndex = Length(FRing) then FReadIndex := 0;
        Dec(FPending);
        Inc(FEmittedFrames);
      end
      else if FRemainingFrames > 0 then RenderOne
      else Break;
    end;
    if FInputEnded and (FPending = 0) then FFinished := True;
    SetLength(ASamples, LCount);
    Result := LCount <> 0;
  except
    ASamples := nil;
    FFailed := True;
    raise;
  end;
end;

procedure TWfcMusicEnsembleAudioRenderer.EndInput;
var I: Integer;
begin
  CheckActive;
  if FInputEnded then Exit;
  if not NeedsInput then AudioError('drain the current frame before ending input');
  try
    for I := 0 to High(FVoices) do CloseVoice(I);
    for I := 0 to High(FVoices) do FVoices[I].Tones := nil;
    FInputEnded := True;
    FFinished := FPending = 0;
  except
    FFailed := True;
    raise;
  end;
end;

procedure TWfcMusicEnsembleAudioRenderer.Cancel;
begin
  if FFinished or FFailed or FCancelled then Exit;
  FCancelled := True;
  FRemainingFrames := 0;
  FPending := 0;
  FVoices := nil;
  FRing := nil;
end;

end.
