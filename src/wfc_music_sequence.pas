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
unit wfc_music_sequence;

{$mode delphi}{$H+}

interface

uses
  wfc_model,
  wfc_music,
  wfc_sequence;

const
  WFC_MUSIC_SEQUENCE_VERSION = 1;
  WFC_MUSIC_CELL_TOKEN_VERSION = 1;

type
  EWfcMusicSequence = class(EWfcMusic);

  { A fixed quantum is either silent, begins a sound, or continues the sound
    begun by the nearest preceding attack. Pitch and velocity remain present
    on holds so every token is independently meaningful and checkable. }
  TWfcMusicCellAction = (
    wmcaRest,
    wmcaAttack,
    wmcaHold
  );

  TWfcMusicMelodyCell = record
    Action: TWfcMusicCellAction;
    Pitch: TWfcMusicPitch;
    Velocity: TWfcMusicVelocity;
  end;
  TWfcMusicMelodyCells = array of TWfcMusicMelodyCell;

  TWfcMusicRhythmCell = record
    Action: TWfcMusicCellAction;
  end;
  TWfcMusicRhythmCells = array of TWfcMusicRhythmCell;

  TWfcMusicHarmonyCellKind = (
    wmhckRest,
    wmhckPitchClass
  );

  TWfcMusicHarmonyCell = record
    Kind: TWfcMusicHarmonyCellKind;
    StepsPerOctave: Integer;
    PitchClass: Integer;
  end;
  TWfcMusicHarmonyCells = array of TWfcMusicHarmonyCell;

function MakeWfcMusicRestCell: TWfcMusicMelodyCell;
function MakeWfcMusicAttackCell(const APitch,
  AVelocity: Integer): TWfcMusicMelodyCell;
function MakeWfcMusicHoldCell(const APitch,
  AVelocity: Integer): TWfcMusicMelodyCell;
function MakeWfcMusicRhythmCell(
  const AAction: TWfcMusicCellAction): TWfcMusicRhythmCell;
function MakeWfcMusicHarmonyRestCell(
  const AStepsPerOctave: Integer): TWfcMusicHarmonyCell;
function MakeWfcMusicHarmonyPitchClassCell(const AStepsPerOctave,
  APitchClass: Integer): TWfcMusicHarmonyCell;

function EncodeWfcMusicMelodyCell(
  const ACell: TWfcMusicMelodyCell): TWfcModelToken;
function DecodeWfcMusicMelodyCell(
  const AToken: TWfcModelToken): TWfcMusicMelodyCell;
function EncodeWfcMusicRhythmCell(
  const ACell: TWfcMusicRhythmCell): TWfcModelToken;
function DecodeWfcMusicRhythmCell(
  const AToken: TWfcModelToken): TWfcMusicRhythmCell;
function EncodeWfcMusicHarmonyCell(
  const ACell: TWfcMusicHarmonyCell): TWfcModelToken;
function DecodeWfcMusicHarmonyCell(
  const AToken: TWfcModelToken): TWfcMusicHarmonyCell;

function EncodeWfcMusicMelodyCells(
  const ACells: TWfcMusicMelodyCells): TWfcModelTokens;
function DecodeWfcMusicMelodyCells(
  const ATokens: TWfcModelTokens): TWfcMusicMelodyCells;
function EncodeWfcMusicRhythmCells(
  const ACells: TWfcMusicRhythmCells): TWfcModelTokens;
function DecodeWfcMusicRhythmCells(
  const ATokens: TWfcModelTokens): TWfcMusicRhythmCells;
function EncodeWfcMusicHarmonyCells(
  const ACells: TWfcMusicHarmonyCells): TWfcModelTokens;
function DecodeWfcMusicHarmonyCells(
  const ATokens: TWfcModelTokens): TWfcMusicHarmonyCells;

function ProjectWfcMusicVoiceToMelodyCells(const AScore: TWfcMusicScore;
  const AVoiceIndex, AQuantumTicks: Integer): TWfcMusicMelodyCells;
function RebuildWfcMusicVoiceSpans(const ACells: TWfcMusicMelodyCells;
  const AVoiceIndex, AQuantumTicks: Integer): TWfcMusicSpanEvents;

function ProjectWfcMusicMelodyToRhythm(
  const ACells: TWfcMusicMelodyCells): TWfcMusicRhythmCells;
function ProjectWfcMusicMelodyToHarmony(
  const ACells: TWfcMusicMelodyCells;
  const AStepsPerOctave: Integer): TWfcMusicHarmonyCells;

function LearnWfcMusicMelodySequence(const ACells: TWfcMusicMelodyCells;
  const AOrder: Integer): TWfcSequenceModel;

implementation

uses
  SysUtils,
  wfc_sequence_learn;

const
  MUSIC_TOKEN_NAME = 'WFC music cell token';

function AsciiToken(const AText: String): TWfcModelToken;
begin
  Result := TWfcModelToken(AText);
end;

function TokenText(const AToken: TWfcModelToken): String;
begin
  Result := String(AToken);
end;

procedure CellError(const AMessage: String);
begin
  raise EWfcMusicSequence.Create('invalid ' + MUSIC_TOKEN_NAME +
    ': ' + AMessage);
end;

function CanonicalInteger(const AText, AField: String): Integer;
var
  I: Integer;
  LDigit: Integer;
begin
  if AText = '' then
    CellError(AField + ' is empty');
  if (Length(AText) > 1) and (AText[1] = '0') then
    CellError(AField + ' has a leading zero');
  Result := 0;
  for I := 1 to Length(AText) do
  begin
    if not (AText[I] in ['0'..'9']) then
      CellError(AField + ' is not a canonical decimal integer');
    LDigit := Ord(AText[I]) - Ord('0');
    if Result > (High(Integer) - LDigit) div 10 then
      CellError(AField + ' exceeds the Integer range');
    Result := (Result * 10) + LDigit;
  end;
end;

procedure RequireAscii(const AText: String);
var
  I: Integer;
begin
  for I := 1 to Length(AText) do
    if Ord(AText[I]) > 127 then
      CellError('token is not ASCII');
end;

function TakeField(const AText: String; var APosition: Integer;
  const AField: String): String;
var
  LStart: Integer;
begin
  if APosition > Length(AText) then
    CellError(AField + ' is missing');
  LStart := APosition;
  while (APosition <= Length(AText)) and
      (AText[APosition] <> ':') do
    Inc(APosition);
  Result := Copy(AText, LStart, APosition - LStart);
  if Result = '' then
    CellError(AField + ' is empty');
  if APosition <= Length(AText) then
    Inc(APosition);
end;

procedure RequireEnd(const AText: String; const APosition: Integer);
begin
  if (AText <> '') and (AText[Length(AText)] = ':') then
    CellError('token has an empty trailing field');
  if APosition <= Length(AText) then
    CellError('token has trailing fields');
end;

procedure ValidateAction(const AAction: TWfcMusicCellAction);
begin
  case AAction of
    wmcaRest, wmcaAttack, wmcaHold: Exit;
  end;
  CellError('unknown cell action');
end;

procedure ValidateMelodyCell(const ACell: TWfcMusicMelodyCell);
begin
  ValidateAction(ACell.Action);
  if ACell.Action = wmcaRest then
  begin
    if (ACell.Pitch <> 0) or (ACell.Velocity <> 0) then
      CellError('a rest cell must have zero pitch and velocity');
    Exit;
  end;
  if ACell.Pitch < 0 then
    CellError('cell pitch cannot be negative');
  if (ACell.Velocity < 1) or (ACell.Velocity > 127) then
    CellError('cell velocity must be in 1..127');
end;

procedure ValidateHarmonyCell(const ACell: TWfcMusicHarmonyCell);
begin
  if ACell.StepsPerOctave < 1 then
    CellError('harmony steps per octave must be positive');
  case ACell.Kind of
    wmhckRest:
      if ACell.PitchClass <> 0 then
        CellError('a harmony rest must have pitch class zero');
    wmhckPitchClass:
      if (ACell.PitchClass < 0) or
          (ACell.PitchClass >= ACell.StepsPerOctave) then
        CellError('harmony pitch class is out of range');
  else
    CellError('unknown harmony cell kind');
  end;
end;

function MakeWfcMusicRestCell: TWfcMusicMelodyCell;
begin
  Result := Default(TWfcMusicMelodyCell);
  Result.Action := wmcaRest;
end;

function MakeWfcMusicAttackCell(const APitch,
  AVelocity: Integer): TWfcMusicMelodyCell;
begin
  Result := Default(TWfcMusicMelodyCell);
  Result.Action := wmcaAttack;
  Result.Pitch := APitch;
  Result.Velocity := AVelocity;
  ValidateMelodyCell(Result);
end;

function MakeWfcMusicHoldCell(const APitch,
  AVelocity: Integer): TWfcMusicMelodyCell;
begin
  Result := Default(TWfcMusicMelodyCell);
  Result.Action := wmcaHold;
  Result.Pitch := APitch;
  Result.Velocity := AVelocity;
  ValidateMelodyCell(Result);
end;

function MakeWfcMusicRhythmCell(
  const AAction: TWfcMusicCellAction): TWfcMusicRhythmCell;
begin
  ValidateAction(AAction);
  Result.Action := AAction;
end;

function MakeWfcMusicHarmonyRestCell(
  const AStepsPerOctave: Integer): TWfcMusicHarmonyCell;
begin
  Result := Default(TWfcMusicHarmonyCell);
  Result.Kind := wmhckRest;
  Result.StepsPerOctave := AStepsPerOctave;
  ValidateHarmonyCell(Result);
end;

function MakeWfcMusicHarmonyPitchClassCell(const AStepsPerOctave,
  APitchClass: Integer): TWfcMusicHarmonyCell;
begin
  Result := Default(TWfcMusicHarmonyCell);
  Result.Kind := wmhckPitchClass;
  Result.StepsPerOctave := AStepsPerOctave;
  Result.PitchClass := APitchClass;
  ValidateHarmonyCell(Result);
end;

function EncodeWfcMusicMelodyCell(
  const ACell: TWfcMusicMelodyCell): TWfcModelToken;
var
  LAction: Char;
begin
  ValidateMelodyCell(ACell);
  if ACell.Action = wmcaRest then
    Exit(AsciiToken('wm1:r'));
  if ACell.Action = wmcaAttack then
    LAction := 'a'
  else
    LAction := 'h';
  Result := AsciiToken('wm1:' + LAction + ':' +
    IntToStr(ACell.Pitch) + ':' + IntToStr(ACell.Velocity));
end;

function DecodeWfcMusicMelodyCell(
  const AToken: TWfcModelToken): TWfcMusicMelodyCell;
var
  LAction: String;
  LPosition: Integer;
  LText: String;
begin
  LText := TokenText(AToken);
  RequireAscii(LText);
  LPosition := 1;
  if TakeField(LText, LPosition, 'melody token version') <> 'wm1' then
    CellError('unsupported melody token version');
  LAction := TakeField(LText, LPosition, 'melody action');
  if LAction = 'r' then
  begin
    RequireEnd(LText, LPosition);
    Exit(MakeWfcMusicRestCell);
  end;
  if LAction = 'a' then
    Result.Action := wmcaAttack
  else if LAction = 'h' then
    Result.Action := wmcaHold
  else
    CellError('unknown melody action');
  Result.Pitch := CanonicalInteger(
    TakeField(LText, LPosition, 'melody pitch'), 'melody pitch');
  Result.Velocity := CanonicalInteger(
    TakeField(LText, LPosition, 'melody velocity'), 'melody velocity');
  RequireEnd(LText, LPosition);
  ValidateMelodyCell(Result);
end;

function EncodeWfcMusicRhythmCell(
  const ACell: TWfcMusicRhythmCell): TWfcModelToken;
begin
  ValidateAction(ACell.Action);
  case ACell.Action of
    wmcaRest: Result := AsciiToken('wr1:r');
    wmcaAttack: Result := AsciiToken('wr1:a');
    wmcaHold: Result := AsciiToken('wr1:h');
  end;
end;

function DecodeWfcMusicRhythmCell(
  const AToken: TWfcModelToken): TWfcMusicRhythmCell;
var
  LAction: String;
  LPosition: Integer;
  LText: String;
begin
  LText := TokenText(AToken);
  RequireAscii(LText);
  LPosition := 1;
  if TakeField(LText, LPosition, 'rhythm token version') <> 'wr1' then
    CellError('unsupported rhythm token version');
  LAction := TakeField(LText, LPosition, 'rhythm action');
  RequireEnd(LText, LPosition);
  if LAction = 'r' then
    Result.Action := wmcaRest
  else if LAction = 'a' then
    Result.Action := wmcaAttack
  else if LAction = 'h' then
    Result.Action := wmcaHold
  else
    CellError('unknown rhythm action');
end;

function EncodeWfcMusicHarmonyCell(
  const ACell: TWfcMusicHarmonyCell): TWfcModelToken;
var
  LKind: Char;
begin
  ValidateHarmonyCell(ACell);
  if ACell.Kind = wmhckRest then
    LKind := 'r'
  else
    LKind := 'p';
  Result := AsciiToken('wh1:' + LKind + ':' +
    IntToStr(ACell.StepsPerOctave) + ':' +
    IntToStr(ACell.PitchClass));
end;

function DecodeWfcMusicHarmonyCell(
  const AToken: TWfcModelToken): TWfcMusicHarmonyCell;
var
  LKind: String;
  LPosition: Integer;
  LText: String;
begin
  LText := TokenText(AToken);
  RequireAscii(LText);
  LPosition := 1;
  if TakeField(LText, LPosition, 'harmony token version') <> 'wh1' then
    CellError('unsupported harmony token version');
  LKind := TakeField(LText, LPosition, 'harmony kind');
  if LKind = 'r' then
    Result.Kind := wmhckRest
  else if LKind = 'p' then
    Result.Kind := wmhckPitchClass
  else
    CellError('unknown harmony kind');
  Result.StepsPerOctave := CanonicalInteger(
    TakeField(LText, LPosition, 'harmony steps per octave'),
    'harmony steps per octave');
  Result.PitchClass := CanonicalInteger(
    TakeField(LText, LPosition, 'harmony pitch class'),
    'harmony pitch class');
  RequireEnd(LText, LPosition);
  ValidateHarmonyCell(Result);
end;

function EncodeWfcMusicMelodyCells(
  const ACells: TWfcMusicMelodyCells): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ACells));
  for I := 0 to Length(ACells) - 1 do
    Result[I] := EncodeWfcMusicMelodyCell(ACells[I]);
end;

function DecodeWfcMusicMelodyCells(
  const ATokens: TWfcModelTokens): TWfcMusicMelodyCells;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ATokens));
  for I := 0 to Length(ATokens) - 1 do
    Result[I] := DecodeWfcMusicMelodyCell(ATokens[I]);
end;

function EncodeWfcMusicRhythmCells(
  const ACells: TWfcMusicRhythmCells): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ACells));
  for I := 0 to Length(ACells) - 1 do
    Result[I] := EncodeWfcMusicRhythmCell(ACells[I]);
end;

function EncodeWfcMusicHarmonyCells(
  const ACells: TWfcMusicHarmonyCells): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ACells));
  for I := 0 to Length(ACells) - 1 do
    Result[I] := EncodeWfcMusicHarmonyCell(ACells[I]);
end;

function DecodeWfcMusicRhythmCells(
  const ATokens: TWfcModelTokens): TWfcMusicRhythmCells;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ATokens));
  for I := 0 to Length(ATokens) - 1 do
    Result[I] := DecodeWfcMusicRhythmCell(ATokens[I]);
end;

function DecodeWfcMusicHarmonyCells(
  const ATokens: TWfcModelTokens): TWfcMusicHarmonyCells;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ATokens));
  for I := 0 to Length(ATokens) - 1 do
    Result[I] := DecodeWfcMusicHarmonyCell(ATokens[I]);
end;

function CheckedProduct(const A, B: Integer;
  const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) then
    raise EWfcMusicSequence.Create(ALabel + ' cannot be negative');
  if (A <> 0) and (B > High(Integer) div A) then
    raise EWfcMusicSequence.Create(ALabel + ' exceeds the Integer range');
  Result := A * B;
end;

function ProjectWfcMusicVoiceToMelodyCells(const AScore: TWfcMusicScore;
  const AVoiceIndex, AQuantumTicks: Integer): TWfcMusicMelodyCells;
var
  I: Integer;
  J: Integer;
  LCellIndex: Integer;
  LSpan: TWfcMusicSpanEvent;
  LSpans: TWfcMusicSpanEvents;
begin
  Result := nil;
  if not Assigned(AScore) then
    raise EWfcMusicSequence.Create('music score is not assigned');
  if AQuantumTicks < 1 then
    raise EWfcMusicSequence.Create('music quantum must be positive');
  { CopyVoiceSpans performs the public voice-index check. }
  LSpans := AScore.CopyVoiceSpans(AVoiceIndex);
  if (AScore.LengthTicks mod AQuantumTicks) <> 0 then
    raise EWfcMusicSequence.Create(
      'music score length is not divisible by the quantum');
  SetLength(Result, AScore.LengthTicks div AQuantumTicks);
  LCellIndex := 0;
  for I := 0 to Length(LSpans) - 1 do
  begin
    LSpan := LSpans[I];
    if ((LSpan.StartTick mod AQuantumTicks) <> 0) or
        ((LSpan.DurationTicks mod AQuantumTicks) <> 0) then
      raise EWfcMusicSequence.CreateFmt(
        'music span is not aligned to the quantum [%d]', [I]);
    if LSpan.Kind = wmskChord then
      raise EWfcMusicSequence.CreateFmt(
        'a chord cannot be projected as a melody cell [%d]', [I]);
    for J := 0 to (LSpan.DurationTicks div AQuantumTicks) - 1 do
    begin
      if LSpan.Kind = wmskRest then
        Result[LCellIndex] := MakeWfcMusicRestCell
      else if J = 0 then
        Result[LCellIndex] := MakeWfcMusicAttackCell(
          LSpan.Tones[0].Pitch, LSpan.Tones[0].Velocity)
      else
        Result[LCellIndex] := MakeWfcMusicHoldCell(
          LSpan.Tones[0].Pitch, LSpan.Tones[0].Velocity);
      Inc(LCellIndex);
    end;
  end;
  if LCellIndex <> Length(Result) then
    raise EWfcMusicSequence.Create(
      'music voice did not exactly fill the quantum timeline');
end;

procedure AppendSpan(var ASpans: TWfcMusicSpanEvents;
  const ASpan: TWfcMusicSpanEvent);
var
  LCount: Integer;
begin
  LCount := Length(ASpans);
  if LCount = High(Integer) then
    raise EWfcMusicSequence.Create('music span count exceeds the Integer range');
  SetLength(ASpans, LCount + 1);
  ASpans[LCount] := ASpan;
end;

function RebuildWfcMusicVoiceSpans(const ACells: TWfcMusicMelodyCells;
  const AVoiceIndex, AQuantumTicks: Integer): TWfcMusicSpanEvents;
var
  I: Integer;
  LDuration: Integer;
  LStart: Integer;
  LTones: TWfcMusicTones;
begin
  Result := nil;
  if AVoiceIndex < 0 then
    raise EWfcMusicSequence.Create('music voice index cannot be negative');
  if AQuantumTicks < 1 then
    raise EWfcMusicSequence.Create('music quantum must be positive');
  if Length(ACells) < 1 then
    raise EWfcMusicSequence.Create('music melody cells cannot be empty');
  CheckedProduct(Length(ACells), AQuantumTicks,
    'music rebuilt voice length');
  I := 0;
  while I < Length(ACells) do
  begin
    ValidateMelodyCell(ACells[I]);
    LStart := CheckedProduct(I, AQuantumTicks,
      'music rebuilt span start');
    if ACells[I].Action = wmcaRest then
    begin
      LDuration := 1;
      while (I + LDuration < Length(ACells)) and
          (ACells[I + LDuration].Action = wmcaRest) do
      begin
        ValidateMelodyCell(ACells[I + LDuration]);
        Inc(LDuration);
      end;
      AppendSpan(Result, MakeWfcMusicRest(AVoiceIndex, LStart,
        CheckedProduct(LDuration, AQuantumTicks,
          'music rebuilt rest duration')));
      Inc(I, LDuration);
      Continue;
    end;
    if ACells[I].Action <> wmcaAttack then
      raise EWfcMusicSequence.CreateFmt(
        'music hold has no preceding attack [%d]', [I]);
    LDuration := 1;
    while (I + LDuration < Length(ACells)) and
        (ACells[I + LDuration].Action = wmcaHold) do
    begin
      ValidateMelodyCell(ACells[I + LDuration]);
      if (ACells[I + LDuration].Pitch <> ACells[I].Pitch) or
          (ACells[I + LDuration].Velocity <> ACells[I].Velocity) then
        raise EWfcMusicSequence.CreateFmt(
          'music hold does not match its attack [%d]', [I + LDuration]);
      Inc(LDuration);
    end;
    SetLength(LTones, 1);
    LTones[0] := MakeWfcMusicTone(ACells[I].Pitch, ACells[I].Velocity);
    AppendSpan(Result, MakeWfcMusicSound(AVoiceIndex, LStart,
      CheckedProduct(LDuration, AQuantumTicks,
        'music rebuilt sound duration'), LTones));
    Inc(I, LDuration);
  end;
end;

function ProjectWfcMusicMelodyToRhythm(
  const ACells: TWfcMusicMelodyCells): TWfcMusicRhythmCells;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ACells));
  for I := 0 to Length(ACells) - 1 do
  begin
    ValidateMelodyCell(ACells[I]);
    Result[I] := MakeWfcMusicRhythmCell(ACells[I].Action);
  end;
end;

function ProjectWfcMusicMelodyToHarmony(
  const ACells: TWfcMusicMelodyCells;
  const AStepsPerOctave: Integer): TWfcMusicHarmonyCells;
var
  I: Integer;
begin
  if AStepsPerOctave < 1 then
    raise EWfcMusicSequence.Create(
      'music harmony steps per octave must be positive');
  Result := nil;
  SetLength(Result, Length(ACells));
  for I := 0 to Length(ACells) - 1 do
  begin
    ValidateMelodyCell(ACells[I]);
    if ACells[I].Action = wmcaRest then
      Result[I] := MakeWfcMusicHarmonyRestCell(AStepsPerOctave)
    else
      Result[I] := MakeWfcMusicHarmonyPitchClassCell(
        AStepsPerOctave, ACells[I].Pitch mod AStepsPerOctave);
  end;
end;

function LearnWfcMusicMelodySequence(const ACells: TWfcMusicMelodyCells;
  const AOrder: Integer): TWfcSequenceModel;
begin
  Result := LearnSequenceModel(EncodeWfcMusicMelodyCells(ACells), AOrder);
end;

end.
