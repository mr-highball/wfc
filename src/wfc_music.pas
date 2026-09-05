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
unit wfc_music;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc_model;

const
  WFC_MUSIC_MODEL_VERSION = 1;
  WFC_MUSIC_VALIDATION_VERSION = 1;

type
  EWfcMusic = class(EWfcModel);

  TWfcMusicTick = Integer;
  TWfcMusicPitch = Integer;
  TWfcMusicVelocity = Integer;

  { Nonnegative, reduced rational used for exact musical time. }
  TWfcMusicRational = record
    Numerator: Integer;
    Denominator: Integer;
  end;

  TWfcMusicTrack = record
    Id: TWfcModelToken;
    Name: TWfcModelToken;
  end;
  TWfcMusicTracks = array of TWfcMusicTrack;

  TWfcMusicVoice = record
    TrackIndex: Integer;
    Id: TWfcModelToken;
  end;
  TWfcMusicVoices = array of TWfcMusicVoice;

  TWfcMusicMeterChange = record
    Tick: TWfcMusicTick;
    Numerator: Integer;
    Denominator: Integer;
  end;
  TWfcMusicMeterChanges = array of TWfcMusicMeterChange;

  TWfcMusicTempoChange = record
    Tick: TWfcMusicTick;
    MicrosecondsPerQuarter: Integer;
  end;
  TWfcMusicTempoChanges = array of TWfcMusicTempoChange;

  TWfcMusicTone = record
    Pitch: TWfcMusicPitch;
    Velocity: TWfcMusicVelocity;
  end;
  TWfcMusicTones = array of TWfcMusicTone;

  TWfcMusicSpanKind = (
    wmskRest,
    wmskNote,
    wmskChord
  );

  { Every voice is an exact, nonoverlapping partition of the score timeline.
    Silence is data rather than a pitch sentinel. }
  TWfcMusicSpanEvent = record
    VoiceIndex: Integer;
    StartTick: TWfcMusicTick;
    DurationTicks: TWfcMusicTick;
    Kind: TWfcMusicSpanKind;
    Tones: TWfcMusicTones;
  end;
  TWfcMusicSpanEvents = array of TWfcMusicSpanEvent;

  { TWfcMusicScore }

  TWfcMusicScore = class
  strict private
    FTicksPerQuarter: Integer;
    FStepsPerOctave: Integer;
    FLengthTicks: Integer;
    FTracks: TWfcMusicTracks;
    FVoices: TWfcMusicVoices;
    FMeters: TWfcMusicMeterChanges;
    FTempos: TWfcMusicTempoChanges;
    FSpans: TWfcMusicSpanEvents;

    function GetTrackCount: Integer;
    function GetVoiceCount: Integer;
    function GetMeterCount: Integer;
    function GetTempoCount: Integer;
    function GetSpanCount: Integer;
    procedure ValidateTrackIndex(const AIndex: Integer);
    procedure ValidateVoiceIndex(const AIndex: Integer);
    procedure ValidateMeterIndex(const AIndex: Integer);
    procedure ValidateTempoIndex(const AIndex: Integer);
    procedure ValidateSpanIndex(const AIndex: Integer);
  public
    constructor Create(const ATicksPerQuarter, AStepsPerOctave,
      ALengthTicks: Integer; const ATracks: TWfcMusicTracks;
      const AVoices: TWfcMusicVoices;
      const AMeters: TWfcMusicMeterChanges;
      const ATempos: TWfcMusicTempoChanges;
      const ASpans: TWfcMusicSpanEvents);

    function TrackAt(const AIndex: Integer): TWfcMusicTrack;
    function VoiceAt(const AIndex: Integer): TWfcMusicVoice;
    function MeterAt(const AIndex: Integer): TWfcMusicMeterChange;
    function TempoAt(const AIndex: Integer): TWfcMusicTempoChange;
    function SpanAt(const AIndex: Integer): TWfcMusicSpanEvent;
    function MeterAtTick(const ATick: TWfcMusicTick):
      TWfcMusicMeterChange;
    function TempoAtTick(const ATick: TWfcMusicTick):
      TWfcMusicTempoChange;

    function CopyTracks: TWfcMusicTracks;
    function CopyVoices: TWfcMusicVoices;
    function CopyMeters: TWfcMusicMeterChanges;
    function CopyTempos: TWfcMusicTempoChanges;
    function CopySpans: TWfcMusicSpanEvents;
    function CopyVoiceSpans(const AVoiceIndex: Integer):
      TWfcMusicSpanEvents;

    property TicksPerQuarter: Integer read FTicksPerQuarter;
    property StepsPerOctave: Integer read FStepsPerOctave;
    property LengthTicks: Integer read FLengthTicks;
    property TrackCount: Integer read GetTrackCount;
    property VoiceCount: Integer read GetVoiceCount;
    property MeterCount: Integer read GetMeterCount;
    property TempoCount: Integer read GetTempoCount;
    property SpanCount: Integer read GetSpanCount;
  end;

function MakeWfcMusicRational(const ANumerator,
  ADenominator: Integer): TWfcMusicRational;

function CompareWfcMusicRational(const A,
  B: TWfcMusicRational): Integer;

function WfcMusicTickToQuarter(const ATick,
  ATicksPerQuarter: Integer): TWfcMusicRational;

function WfcMusicQuarterToTickExact(const AValue: TWfcMusicRational;
  const ATicksPerQuarter: Integer): Integer;

function MakeWfcMusicTrack(const AId,
  AName: TWfcModelToken): TWfcMusicTrack;

function MakeWfcMusicVoice(const ATrackIndex: Integer;
  const AId: TWfcModelToken): TWfcMusicVoice;

function MakeWfcMusicMeterChange(const ATick: TWfcMusicTick;
  const ANumerator, ADenominator: Integer): TWfcMusicMeterChange;

function MakeWfcMusicTempoChange(const ATick: TWfcMusicTick;
  const AMicrosecondsPerQuarter: Integer): TWfcMusicTempoChange;

function MakeWfcMusicTone(const APitch: TWfcMusicPitch;
  const AVelocity: TWfcMusicVelocity): TWfcMusicTone;

function MakeWfcMusicRest(const AVoiceIndex: Integer;
  const AStartTick, ADurationTicks: TWfcMusicTick): TWfcMusicSpanEvent;

function MakeWfcMusicSound(const AVoiceIndex: Integer;
  const AStartTick, ADurationTicks: TWfcMusicTick;
  const ATones: TWfcMusicTones): TWfcMusicSpanEvent;

implementation

function CheckedLength(const ALength: SizeInt;
  const ALabel: String): Integer;
begin
  if (ALength < 0) or
      ((ALength and (not SizeInt(High(Integer)))) <> 0) then
    raise EWfcMusic.Create(ALabel + ' exceeds the Integer range');
  Result := Integer(ALength);
end;

function CheckedAddNonnegative(const A, B: Integer;
  const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) then
    raise EWfcMusic.Create(ALabel + ' cannot be negative');
  if A > High(Integer) - B then
    raise EWfcMusic.Create(ALabel + ' exceeds the Integer range');
  Result := A + B;
end;

function CheckedMultiplyNonnegative(const A, B: Integer;
  const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) then
    raise EWfcMusic.Create(ALabel + ' cannot be negative');
  if (A <> 0) and (B > High(Integer) div A) then
    raise EWfcMusic.Create(ALabel + ' exceeds the Integer range');
  Result := A * B;
end;

function GreatestCommonDivisor(A, B: Integer): Integer;
var
  LNext: Integer;
begin
  if (A < 0) or (B < 0) then
    raise EWfcMusic.Create('music GCD operands cannot be negative');
  while B <> 0 do
  begin
    LNext := A mod B;
    A := B;
    B := LNext;
  end;
  Result := A;
end;

function IsPowerOfTwo(const AValue: Integer): Boolean;
begin
  Result := (AValue > 0) and
    ((AValue and (AValue - 1)) = 0);
end;

function MeasureLengthTicks(const ATicksPerQuarter,
  ANumerator, ADenominator: Integer): Integer;
var
  LDenominator: Integer;
  LGcd: Integer;
  LMeterNumerator: Integer;
  LNumerator: Integer;
begin
  if ANumerator < 1 then
    raise EWfcMusic.Create('music meter numerator must be positive');
  if not IsPowerOfTwo(ADenominator) then
    raise EWfcMusic.Create(
      'music meter denominator must be a positive power of two');
  LNumerator := CheckedMultiplyNonnegative(ATicksPerQuarter, 4,
    'music meter quarter-note scale');
  LMeterNumerator := ANumerator;
  LDenominator := ADenominator;
  LGcd := GreatestCommonDivisor(LNumerator, LDenominator);
  LNumerator := LNumerator div LGcd;
  LDenominator := LDenominator div LGcd;
  LGcd := GreatestCommonDivisor(LMeterNumerator, LDenominator);
  LMeterNumerator := LMeterNumerator div LGcd;
  LDenominator := LDenominator div LGcd;
  if LDenominator <> 1 then
    raise EWfcMusic.Create(
      'music meter measure length is not an exact tick count');
  Result := CheckedMultiplyNonnegative(LNumerator, LMeterNumerator,
    'music meter measure length');
  if Result < 1 then
    raise EWfcMusic.Create('music meter measure length must be positive');
end;

function CopyTones(const AValues: TWfcMusicTones): TWfcMusicTones;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function CopySpan(const AValue: TWfcMusicSpanEvent): TWfcMusicSpanEvent;
begin
  Result := AValue;
  Result.Tones := CopyTones(AValue.Tones);
end;

function MakeWfcMusicRational(const ANumerator,
  ADenominator: Integer): TWfcMusicRational;
var
  LGcd: Integer;
begin
  if ANumerator < 0 then
    raise EWfcMusic.Create('music rational numerator cannot be negative');
  if ADenominator < 1 then
    raise EWfcMusic.Create('music rational denominator must be positive');
  if ANumerator = 0 then
  begin
    Result.Numerator := 0;
    Result.Denominator := 1;
    Exit;
  end;
  LGcd := GreatestCommonDivisor(ANumerator, ADenominator);
  Result.Numerator := ANumerator div LGcd;
  Result.Denominator := ADenominator div LGcd;
end;

function CompareWfcMusicRational(const A,
  B: TWfcMusicRational): Integer;
var
  LLeft: Integer;
  LRight: Integer;
  LA: TWfcMusicRational;
  LB: TWfcMusicRational;
begin
  LA := MakeWfcMusicRational(A.Numerator, A.Denominator);
  LB := MakeWfcMusicRational(B.Numerator, B.Denominator);
  LLeft := CheckedMultiplyNonnegative(LA.Numerator, LB.Denominator,
    'music rational comparison');
  LRight := CheckedMultiplyNonnegative(LB.Numerator, LA.Denominator,
    'music rational comparison');
  if LLeft < LRight then
    Result := -1
  else if LLeft > LRight then
    Result := 1
  else
    Result := 0;
end;

function WfcMusicTickToQuarter(const ATick,
  ATicksPerQuarter: Integer): TWfcMusicRational;
begin
  if ATick < 0 then
    raise EWfcMusic.Create('music tick cannot be negative');
  if ATicksPerQuarter < 1 then
    raise EWfcMusic.Create('music ticks per quarter must be positive');
  Result := MakeWfcMusicRational(ATick, ATicksPerQuarter);
end;

function WfcMusicQuarterToTickExact(const AValue: TWfcMusicRational;
  const ATicksPerQuarter: Integer): Integer;
var
  LGcd: Integer;
  LNumerator: Integer;
  LDenominator: Integer;
  LTicks: Integer;
begin
  if ATicksPerQuarter < 1 then
    raise EWfcMusic.Create('music ticks per quarter must be positive');
  LNumerator := MakeWfcMusicRational(AValue.Numerator,
    AValue.Denominator).Numerator;
  LDenominator := MakeWfcMusicRational(AValue.Numerator,
    AValue.Denominator).Denominator;
  LTicks := ATicksPerQuarter;
  LGcd := GreatestCommonDivisor(LTicks, LDenominator);
  LTicks := LTicks div LGcd;
  LDenominator := LDenominator div LGcd;
  LGcd := GreatestCommonDivisor(LNumerator, LDenominator);
  LNumerator := LNumerator div LGcd;
  LDenominator := LDenominator div LGcd;
  if LDenominator <> 1 then
    raise EWfcMusic.Create(
      'music quarter value does not map to an exact tick');
  Result := CheckedMultiplyNonnegative(LNumerator, LTicks,
    'music exact tick conversion');
end;

function MakeWfcMusicTrack(const AId,
  AName: TWfcModelToken): TWfcMusicTrack;
begin
  Result.Id := AId;
  Result.Name := AName;
end;

function MakeWfcMusicVoice(const ATrackIndex: Integer;
  const AId: TWfcModelToken): TWfcMusicVoice;
begin
  Result.TrackIndex := ATrackIndex;
  Result.Id := AId;
end;

function MakeWfcMusicMeterChange(const ATick: TWfcMusicTick;
  const ANumerator, ADenominator: Integer): TWfcMusicMeterChange;
begin
  Result.Tick := ATick;
  Result.Numerator := ANumerator;
  Result.Denominator := ADenominator;
end;

function MakeWfcMusicTempoChange(const ATick: TWfcMusicTick;
  const AMicrosecondsPerQuarter: Integer): TWfcMusicTempoChange;
begin
  Result.Tick := ATick;
  Result.MicrosecondsPerQuarter := AMicrosecondsPerQuarter;
end;

function MakeWfcMusicTone(const APitch: TWfcMusicPitch;
  const AVelocity: TWfcMusicVelocity): TWfcMusicTone;
begin
  Result.Pitch := APitch;
  Result.Velocity := AVelocity;
end;

function MakeWfcMusicRest(const AVoiceIndex: Integer;
  const AStartTick, ADurationTicks: TWfcMusicTick): TWfcMusicSpanEvent;
begin
  Result := Default(TWfcMusicSpanEvent);
  Result.VoiceIndex := AVoiceIndex;
  Result.StartTick := AStartTick;
  Result.DurationTicks := ADurationTicks;
  Result.Kind := wmskRest;
end;

function MakeWfcMusicSound(const AVoiceIndex: Integer;
  const AStartTick, ADurationTicks: TWfcMusicTick;
  const ATones: TWfcMusicTones): TWfcMusicSpanEvent;
begin
  Result := Default(TWfcMusicSpanEvent);
  Result.VoiceIndex := AVoiceIndex;
  Result.StartTick := AStartTick;
  Result.DurationTicks := ADurationTicks;
  Result.Tones := CopyTones(ATones);
  if Length(ATones) = 1 then
    Result.Kind := wmskNote
  else
    Result.Kind := wmskChord;
end;

{ TWfcMusicScore }

constructor TWfcMusicScore.Create(const ATicksPerQuarter,
  AStepsPerOctave, ALengthTicks: Integer;
  const ATracks: TWfcMusicTracks; const AVoices: TWfcMusicVoices;
  const AMeters: TWfcMusicMeterChanges;
  const ATempos: TWfcMusicTempoChanges;
  const ASpans: TWfcMusicSpanEvents);
var
  I: Integer;
  J: Integer;
  LEndTick: Integer;
  LExpectedStart: Integer;
  LExpectedVoice: Integer;
  LMeasureLength: Integer;
  LPreviousKind: TWfcMusicSpanKind;
  LPreviousMeterTick: Integer;
begin
  inherited Create;
  if ATicksPerQuarter < 1 then
    raise EWfcMusic.Create('music ticks per quarter must be positive');
  if AStepsPerOctave < 1 then
    raise EWfcMusic.Create('music steps per octave must be positive');
  if ALengthTicks < 1 then
    raise EWfcMusic.Create('music score length must be positive');
  FTicksPerQuarter := ATicksPerQuarter;
  FStepsPerOctave := AStepsPerOctave;
  FLengthTicks := ALengthTicks;

  if CheckedLength(Length(ATracks), 'music track count') < 1 then
    raise EWfcMusic.Create('a music score must contain a track');
  SetLength(FTracks, Length(ATracks));
  for I := 0 to Length(ATracks) - 1 do
  begin
    if not WfcModelTokenIsValid(ATracks[I].Id) then
      raise EWfcMusic.CreateFmt(
        'music track id must be nonempty, well-formed UTF-8 [%d]', [I]);
    if (ATracks[I].Name <> '') and
        (not WfcModelTokenIsValid(ATracks[I].Name)) then
      raise EWfcMusic.CreateFmt(
        'music track name must be well-formed UTF-8 [%d]', [I]);
    for J := 0 to I - 1 do
      if ATracks[I].Id = ATracks[J].Id then
        raise EWfcMusic.CreateFmt(
          'music track ids must be unique [%d, %d]', [J, I]);
    FTracks[I] := ATracks[I];
  end;

  if CheckedLength(Length(AVoices), 'music voice count') < 1 then
    raise EWfcMusic.Create('a music score must contain a voice');
  SetLength(FVoices, Length(AVoices));
  for I := 0 to Length(AVoices) - 1 do
  begin
    if (AVoices[I].TrackIndex < 0) or
        (AVoices[I].TrackIndex >= Length(ATracks)) then
      raise EWfcMusic.CreateFmt(
        'music voice track index is out of bounds [%d: %d]',
        [I, AVoices[I].TrackIndex]);
    if not WfcModelTokenIsValid(AVoices[I].Id) then
      raise EWfcMusic.CreateFmt(
        'music voice id must be nonempty, well-formed UTF-8 [%d]', [I]);
    for J := 0 to I - 1 do
      if AVoices[I].Id = AVoices[J].Id then
        raise EWfcMusic.CreateFmt(
          'music voice ids must be unique [%d, %d]', [J, I]);
    FVoices[I] := AVoices[I];
  end;

  if CheckedLength(Length(AMeters), 'music meter count') < 1 then
    raise EWfcMusic.Create('a music score must contain a meter');
  SetLength(FMeters, Length(AMeters));
  LPreviousMeterTick := 0;
  LMeasureLength := 0;
  for I := 0 to Length(AMeters) - 1 do
  begin
    if (AMeters[I].Tick < 0) or (AMeters[I].Tick >= ALengthTicks) then
      raise EWfcMusic.CreateFmt(
        'music meter tick is outside the score [%d: %d]',
        [I, AMeters[I].Tick]);
    if (I = 0) and (AMeters[I].Tick <> 0) then
      raise EWfcMusic.Create('the first music meter must begin at tick zero');
    if (I > 0) and (AMeters[I].Tick <= AMeters[I - 1].Tick) then
      raise EWfcMusic.CreateFmt(
        'music meter ticks must be strictly increasing [%d]', [I]);
    if (I > 0) and
        (((AMeters[I].Tick - LPreviousMeterTick) mod
          LMeasureLength) <> 0) then
      raise EWfcMusic.CreateFmt(
        'music meter change must occur at a measure boundary [%d]', [I]);
    LPreviousMeterTick := AMeters[I].Tick;
    LMeasureLength := MeasureLengthTicks(ATicksPerQuarter,
      AMeters[I].Numerator, AMeters[I].Denominator);
    FMeters[I] := AMeters[I];
  end;
  if ((ALengthTicks - LPreviousMeterTick) mod LMeasureLength) <> 0 then
    raise EWfcMusic.Create(
      'music score must end at a complete measure boundary');

  if CheckedLength(Length(ATempos), 'music tempo count') < 1 then
    raise EWfcMusic.Create('a music score must contain a tempo');
  SetLength(FTempos, Length(ATempos));
  for I := 0 to Length(ATempos) - 1 do
  begin
    if (ATempos[I].Tick < 0) or (ATempos[I].Tick >= ALengthTicks) then
      raise EWfcMusic.CreateFmt(
        'music tempo tick is outside the score [%d: %d]',
        [I, ATempos[I].Tick]);
    if (I = 0) and (ATempos[I].Tick <> 0) then
      raise EWfcMusic.Create('the first music tempo must begin at tick zero');
    if (I > 0) and (ATempos[I].Tick <= ATempos[I - 1].Tick) then
      raise EWfcMusic.CreateFmt(
        'music tempo ticks must be strictly increasing [%d]', [I]);
    if ATempos[I].MicrosecondsPerQuarter < 1 then
      raise EWfcMusic.CreateFmt(
        'music tempo must use positive microseconds per quarter [%d]', [I]);
    FTempos[I] := ATempos[I];
  end;

  if CheckedLength(Length(ASpans), 'music span count') < 1 then
    raise EWfcMusic.Create('a music score must contain span events');
  SetLength(FSpans, Length(ASpans));
  LExpectedVoice := 0;
  LExpectedStart := 0;
  LPreviousKind := wmskChord;
  for I := 0 to Length(ASpans) - 1 do
  begin
    if (ASpans[I].VoiceIndex < 0) or
        (ASpans[I].VoiceIndex >= Length(AVoices)) then
      raise EWfcMusic.CreateFmt(
        'music span voice index is out of bounds [%d: %d]',
        [I, ASpans[I].VoiceIndex]);
    if ASpans[I].VoiceIndex <> LExpectedVoice then
    begin
      if (ASpans[I].VoiceIndex <> LExpectedVoice + 1) or
          (LExpectedStart <> ALengthTicks) then
        raise EWfcMusic.CreateFmt(
          'music spans must exactly partition voices in canonical order [%d]',
          [I]);
      Inc(LExpectedVoice);
      LExpectedStart := 0;
      LPreviousKind := wmskChord;
    end;
    if ASpans[I].StartTick <> LExpectedStart then
      raise EWfcMusic.CreateFmt(
        'music voice spans must be contiguous [%d: %d <> %d]',
        [I, ASpans[I].StartTick, LExpectedStart]);
    if ASpans[I].DurationTicks < 1 then
      raise EWfcMusic.CreateFmt(
        'music span duration must be positive [%d]', [I]);
    LEndTick := CheckedAddNonnegative(ASpans[I].StartTick,
      ASpans[I].DurationTicks, 'music span end tick');
    if LEndTick > ALengthTicks then
      raise EWfcMusic.CreateFmt(
        'music span extends beyond the score [%d]', [I]);
    case ASpans[I].Kind of
      wmskRest:
        begin
          if Length(ASpans[I].Tones) <> 0 then
            raise EWfcMusic.CreateFmt(
              'music rest cannot contain tones [%d]', [I]);
          if (I > 0) and (ASpans[I].StartTick > 0) and
              (LPreviousKind = wmskRest) then
            raise EWfcMusic.CreateFmt(
              'adjacent music rests must be merged [%d]', [I]);
        end;
      wmskNote:
        if Length(ASpans[I].Tones) <> 1 then
          raise EWfcMusic.CreateFmt(
            'music note must contain exactly one tone [%d]', [I]);
      wmskChord:
        if Length(ASpans[I].Tones) < 2 then
          raise EWfcMusic.CreateFmt(
            'music chord must contain at least two tones [%d]', [I]);
    else
      raise EWfcMusic.CreateFmt(
        'unknown music span kind [%d: %d]', [I, Ord(ASpans[I].Kind)]);
    end;
    for J := 0 to Length(ASpans[I].Tones) - 1 do
    begin
      if ASpans[I].Tones[J].Pitch < 0 then
        raise EWfcMusic.CreateFmt(
          'music pitch cannot be negative [%d, %d]', [I, J]);
      if (ASpans[I].Tones[J].Velocity < 1) or
          (ASpans[I].Tones[J].Velocity > 127) then
        raise EWfcMusic.CreateFmt(
          'music velocity must be in 1..127 [%d, %d]', [I, J]);
      if (J > 0) and
          (ASpans[I].Tones[J].Pitch <= ASpans[I].Tones[J - 1].Pitch) then
        raise EWfcMusic.CreateFmt(
          'music span pitches must be strictly increasing [%d, %d]',
          [I, J]);
    end;
    FSpans[I] := CopySpan(ASpans[I]);
    LExpectedStart := LEndTick;
    LPreviousKind := ASpans[I].Kind;
  end;
  if (LExpectedVoice <> Length(AVoices) - 1) or
      (LExpectedStart <> ALengthTicks) then
    raise EWfcMusic.Create(
      'music spans must exactly partition every voice');
end;

function TWfcMusicScore.GetTrackCount: Integer;
begin
  Result := Length(FTracks);
end;

function TWfcMusicScore.GetVoiceCount: Integer;
begin
  Result := Length(FVoices);
end;

function TWfcMusicScore.GetMeterCount: Integer;
begin
  Result := Length(FMeters);
end;

function TWfcMusicScore.GetTempoCount: Integer;
begin
  Result := Length(FTempos);
end;

function TWfcMusicScore.GetSpanCount: Integer;
begin
  Result := Length(FSpans);
end;

procedure TWfcMusicScore.ValidateTrackIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Length(FTracks)) then
    raise ERangeError.CreateFmt('music track index is out of bounds [%d]',
      [AIndex]);
end;

procedure TWfcMusicScore.ValidateVoiceIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Length(FVoices)) then
    raise ERangeError.CreateFmt('music voice index is out of bounds [%d]',
      [AIndex]);
end;

procedure TWfcMusicScore.ValidateMeterIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Length(FMeters)) then
    raise ERangeError.CreateFmt('music meter index is out of bounds [%d]',
      [AIndex]);
end;

procedure TWfcMusicScore.ValidateTempoIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Length(FTempos)) then
    raise ERangeError.CreateFmt('music tempo index is out of bounds [%d]',
      [AIndex]);
end;

procedure TWfcMusicScore.ValidateSpanIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Length(FSpans)) then
    raise ERangeError.CreateFmt('music span index is out of bounds [%d]',
      [AIndex]);
end;

function TWfcMusicScore.TrackAt(const AIndex: Integer): TWfcMusicTrack;
begin
  ValidateTrackIndex(AIndex);
  Result := FTracks[AIndex];
end;

function TWfcMusicScore.VoiceAt(const AIndex: Integer): TWfcMusicVoice;
begin
  ValidateVoiceIndex(AIndex);
  Result := FVoices[AIndex];
end;

function TWfcMusicScore.MeterAt(const AIndex: Integer):
  TWfcMusicMeterChange;
begin
  ValidateMeterIndex(AIndex);
  Result := FMeters[AIndex];
end;

function TWfcMusicScore.TempoAt(const AIndex: Integer):
  TWfcMusicTempoChange;
begin
  ValidateTempoIndex(AIndex);
  Result := FTempos[AIndex];
end;

function TWfcMusicScore.SpanAt(const AIndex: Integer): TWfcMusicSpanEvent;
begin
  ValidateSpanIndex(AIndex);
  Result := CopySpan(FSpans[AIndex]);
end;

function TWfcMusicScore.MeterAtTick(const ATick: TWfcMusicTick):
  TWfcMusicMeterChange;
var
  I: Integer;
begin
  if (ATick < 0) or (ATick >= FLengthTicks) then
    raise ERangeError.CreateFmt('music tick is outside the score [%d]',
      [ATick]);
  Result := FMeters[0];
  for I := 1 to Length(FMeters) - 1 do
    if FMeters[I].Tick <= ATick then
      Result := FMeters[I]
    else
      Break;
end;

function TWfcMusicScore.TempoAtTick(const ATick: TWfcMusicTick):
  TWfcMusicTempoChange;
var
  I: Integer;
begin
  if (ATick < 0) or (ATick >= FLengthTicks) then
    raise ERangeError.CreateFmt('music tick is outside the score [%d]',
      [ATick]);
  Result := FTempos[0];
  for I := 1 to Length(FTempos) - 1 do
    if FTempos[I].Tick <= ATick then
      Result := FTempos[I]
    else
      Break;
end;

function TWfcMusicScore.CopyTracks: TWfcMusicTracks;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FTracks));
  for I := 0 to Length(FTracks) - 1 do
    Result[I] := FTracks[I];
end;

function TWfcMusicScore.CopyVoices: TWfcMusicVoices;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FVoices));
  for I := 0 to Length(FVoices) - 1 do
    Result[I] := FVoices[I];
end;

function TWfcMusicScore.CopyMeters: TWfcMusicMeterChanges;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FMeters));
  for I := 0 to Length(FMeters) - 1 do
    Result[I] := FMeters[I];
end;

function TWfcMusicScore.CopyTempos: TWfcMusicTempoChanges;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FTempos));
  for I := 0 to Length(FTempos) - 1 do
    Result[I] := FTempos[I];
end;

function TWfcMusicScore.CopySpans: TWfcMusicSpanEvents;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FSpans));
  for I := 0 to Length(FSpans) - 1 do
    Result[I] := CopySpan(FSpans[I]);
end;

function TWfcMusicScore.CopyVoiceSpans(const AVoiceIndex: Integer):
  TWfcMusicSpanEvents;
var
  I: Integer;
  LCount: Integer;
begin
  ValidateVoiceIndex(AVoiceIndex);
  Result := nil;
  LCount := 0;
  for I := 0 to Length(FSpans) - 1 do
    if FSpans[I].VoiceIndex = AVoiceIndex then
    begin
      SetLength(Result, LCount + 1);
      Result[LCount] := CopySpan(FSpans[I]);
      Inc(LCount);
    end;
end;

end.
