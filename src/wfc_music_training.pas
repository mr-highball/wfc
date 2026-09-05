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
unit wfc_music_training;

{$mode delphi}{$H+}

interface

uses
  wfc_model,
  wfc_music,
  wfc_training;

const
  WFC_MUSIC_TRAINING_VERSION = 1;

type
  EWfcMusicTraining = class(EWfcMusic);

  TWfcMusicTrainingProjection = (
    wmtpMelody,
    wmtpRhythm,
    wmtpHarmony
  );

  { Every explicit selection becomes one independent sequence sample. }
  TWfcMusicTrainingSelection = record
    Name: TWfcModelToken;
    VoiceIndex: Integer;
    StartTick: Integer;
    LengthTicks: Integer;
  end;
  TWfcMusicTrainingSelections = array of TWfcMusicTrainingSelection;

function MakeWfcMusicTrainingSelection(
  const AName: TWfcModelToken;
  const AVoiceIndex, AStartTick,
  ALengthTicks: Integer): TWfcMusicTrainingSelection;

{ Extracts only the selected score ranges. The returned immutable document is
  owned by the caller; caller metadata and sample order are preserved. }
function BuildWfcMusicTrainingDocument(
  const AScore: TWfcMusicScore;
  const ASelections: TWfcMusicTrainingSelections;
  const AProjection: TWfcMusicTrainingProjection;
  const AQuantumTicks, AOrder: Integer;
  const AMetadata: TWfcTrainingMetadata): TWfcTrainingDocument;

implementation

uses
  SysUtils,
  wfc_music_sequence,
  wfc_text_codec;

procedure MusicTrainingError(const AMessage: String);
begin
  raise EWfcMusicTraining.Create(AMessage);
end;

function MakeWfcMusicTrainingSelection(
  const AName: TWfcModelToken;
  const AVoiceIndex, AStartTick,
  ALengthTicks: Integer): TWfcMusicTrainingSelection;
begin
  Result.Name := AName;
  Result.VoiceIndex := AVoiceIndex;
  Result.StartTick := AStartTick;
  Result.LengthTicks := ALengthTicks;
end;

procedure ValidateProjection(
  const AProjection: TWfcMusicTrainingProjection);
begin
  case AProjection of
    wmtpMelody, wmtpRhythm, wmtpHarmony: Exit;
  end;
  MusicTrainingError('unknown music training projection');
end;

procedure AccumulateEncodedField(const AValue: TWfcModelToken;
  const ALabel: String; var AEncodedTotal: Integer);
var
  LEncoded: String;
  LStorageLength: SizeInt;
begin
  LStorageLength := Length(AValue);
  if LStorageLength = 0 then
    MusicTrainingError(ALabel + ' cannot be empty');
  if (LStorageLength < 0) or
      (LStorageLength > WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH) then
    MusicTrainingError(ALabel +
      ' exceeds the version-1 encoded-token limit');
  if LStorageLength >
      WFC_TRAINING_MAX_TOTAL_ENCODED_TOKEN_LENGTH - AEncodedTotal then
    MusicTrainingError(
      'aggregate encoded music-training content exceeds the version-1 limit');
  try
    LEncoded := WfcTextEncodeToken(AValue, 'WFC music training');
  except
    on E: EConvertError do
      MusicTrainingError(ALabel + ': ' + E.Message);
  end;
  if Length(LEncoded) > WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH then
    MusicTrainingError(ALabel +
      ' exceeds the version-1 encoded-token limit');
  if Length(LEncoded) >
      WFC_TRAINING_MAX_TOTAL_ENCODED_TOKEN_LENGTH - AEncodedTotal then
    MusicTrainingError(
      'aggregate encoded music-training content exceeds the version-1 limit');
  Inc(AEncodedTotal, Length(LEncoded));
end;

function CheckedSelectionEnd(
  const ASelection: TWfcMusicTrainingSelection;
  const ASelectionIndex: Integer): Integer;
begin
  if ASelection.StartTick < 0 then
    MusicTrainingError(Format(
      'music training selection start cannot be negative [%d]',
      [ASelectionIndex]));
  if ASelection.LengthTicks < 1 then
    MusicTrainingError(Format(
      'music training selection length must be positive [%d]',
      [ASelectionIndex]));
  if ASelection.StartTick > High(Integer) - ASelection.LengthTicks then
    MusicTrainingError(Format(
      'music training selection range exceeds the Integer range [%d]',
      [ASelectionIndex]));
  Result := ASelection.StartTick + ASelection.LengthTicks;
end;

function CheckedSpanEnd(const ASpan: TWfcMusicSpanEvent;
  const ASpanIndex: Integer): Integer;
begin
  if (ASpan.StartTick < 0) or (ASpan.DurationTicks < 1) or
      (ASpan.StartTick > High(Integer) - ASpan.DurationTicks) then
    MusicTrainingError(Format(
      'music span range exceeds the Integer range [%d]', [ASpanIndex]));
  Result := ASpan.StartTick + ASpan.DurationTicks;
end;

procedure ValidatePreflight(const AScore: TWfcMusicScore;
  const ASelections: TWfcMusicTrainingSelections;
  const AProjection: TWfcMusicTrainingProjection;
  const AQuantumTicks, AOrder: Integer;
  const AMetadata: TWfcTrainingMetadata);
var
  I: Integer;
  J: Integer;
  LCellCount: Integer;
  LEncodedTotal: Integer;
  LEndTick: Integer;
  LTotalCells: Integer;
begin
  if not Assigned(AScore) then
    MusicTrainingError('music training score is not assigned');
  ValidateProjection(AProjection);
  if AQuantumTicks < 1 then
    MusicTrainingError('music training quantum must be positive');
  if (AOrder < 1) or (AOrder > WFC_TRAINING_MAX_ORDER) then
    MusicTrainingError(
      'music training order is outside the version-1 limit');
  if (Length(ASelections) = 0) or
      (Length(ASelections) > WFC_TRAINING_MAX_SAMPLE_COUNT) then
    MusicTrainingError(
      'music training selection count is outside the version-1 limit');
  if (Length(ASelections) <> 0) and
      (AScore.SpanCount >
      WFC_TRAINING_MAX_VISIT_COUNT div Length(ASelections)) then
    MusicTrainingError(
      'music training span inspection exceeds the version-1 limit');

  LEncodedTotal := 0;
  AccumulateEncodedField(AMetadata.Name,
    'music training name', LEncodedTotal);
  AccumulateEncodedField(AMetadata.LicenseIdentifier,
    'music training license identifier', LEncodedTotal);
  AccumulateEncodedField(AMetadata.SourceDescription,
    'music training source description', LEncodedTotal);

  LTotalCells := 0;
  for I := 0 to Length(ASelections) - 1 do
  begin
    AccumulateEncodedField(ASelections[I].Name,
      Format('music training selection name %d', [I]), LEncodedTotal);
    for J := 0 to I - 1 do
      if ASelections[I].Name = ASelections[J].Name then
        MusicTrainingError(Format(
          'music training selection names must be unique [%d, %d]',
          [J, I]));
    if (ASelections[I].VoiceIndex < 0) or
        (ASelections[I].VoiceIndex >= AScore.VoiceCount) then
      MusicTrainingError(Format(
        'music training selection voice index is out of bounds [%d: %d]',
        [I, ASelections[I].VoiceIndex]));
    LEndTick := CheckedSelectionEnd(ASelections[I], I);
    if LEndTick > AScore.LengthTicks then
      MusicTrainingError(Format(
        'music training selection extends beyond the score [%d]', [I]));
    if ((ASelections[I].StartTick mod AQuantumTicks) <> 0) or
        ((ASelections[I].LengthTicks mod AQuantumTicks) <> 0) then
      MusicTrainingError(Format(
        'music training selection is not aligned to the quantum [%d]', [I]));
    LCellCount := ASelections[I].LengthTicks div AQuantumTicks;
    if LCellCount < AOrder then
      MusicTrainingError(Format(
        'music training selection has fewer cells than the order [%d]', [I]));
    if LCellCount > WFC_TRAINING_MAX_DIMENSION then
      MusicTrainingError(Format(
        'music training selection cell count exceeds the version-1 limit [%d]',
        [I]));
    if LCellCount > WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT - LTotalCells then
      MusicTrainingError(
        'aggregate music training token count exceeds the version-1 limit');
    Inc(LTotalCells, LCellCount);
  end;
end;

function ExtractMelodyCells(const AScore: TWfcMusicScore;
  const ASelection: TWfcMusicTrainingSelection;
  const ASelectionIndex, AQuantumTicks: Integer): TWfcMusicMelodyCells;
var
  I: Integer;
  J: Integer;
  LCellCount: Integer;
  LCellIndex: Integer;
  LDurationCells: Integer;
  LSelectionEnd: Integer;
  LSpan: TWfcMusicSpanEvent;
  LSpanEnd: Integer;
begin
  Result := nil;
  LCellCount := ASelection.LengthTicks div AQuantumTicks;
  SetLength(Result, LCellCount);
  for I := 0 to LCellCount - 1 do
    Result[I] := MakeWfcMusicRestCell;

  LSelectionEnd := ASelection.StartTick + ASelection.LengthTicks;
  for I := 0 to AScore.SpanCount - 1 do
  begin
    LSpan := AScore.SpanAt(I);
    if LSpan.VoiceIndex <> ASelection.VoiceIndex then
      Continue;
    LSpanEnd := CheckedSpanEnd(LSpan, I);
    if (LSpan.StartTick >= LSelectionEnd) or
        (LSpanEnd <= ASelection.StartTick) then
      Continue;

    case LSpan.Kind of
      wmskRest:
        begin
          { Cropping silence at either sample edge is lossless. Every rest
            boundary retained inside a sample must still land on a cell. }
          if (LSpan.StartTick > ASelection.StartTick) and
              (LSpan.StartTick < LSelectionEnd) and
              ((LSpan.StartTick mod AQuantumTicks) <> 0) then
            MusicTrainingError(Format(
              'music rest onset is not aligned to the quantum [%d, %d]',
              [ASelectionIndex, I]));
          if (LSpanEnd > ASelection.StartTick) and
              (LSpanEnd < LSelectionEnd) and
              ((LSpanEnd mod AQuantumTicks) <> 0) then
            MusicTrainingError(Format(
              'music rest end is not aligned to the quantum [%d, %d]',
              [ASelectionIndex, I]));
        end;
      wmskChord:
        MusicTrainingError(Format(
          'a chord cannot be projected for music training [%d, %d]',
          [ASelectionIndex, I]));
      wmskNote:
        begin
          if (LSpan.StartTick < ASelection.StartTick) or
              (LSpanEnd > LSelectionEnd) then
            MusicTrainingError(Format(
              'music training selection cuts a sounded span [%d, %d]',
              [ASelectionIndex, I]));
          if ((LSpan.StartTick mod AQuantumTicks) <> 0) or
              ((LSpan.DurationTicks mod AQuantumTicks) <> 0) then
            MusicTrainingError(Format(
              'music sounded span is not aligned to the quantum [%d, %d]',
              [ASelectionIndex, I]));
          if Length(LSpan.Tones) <> 1 then
            MusicTrainingError(Format(
              'music training sounded span must be monophonic [%d, %d]',
              [ASelectionIndex, I]));
          LCellIndex :=
            (LSpan.StartTick - ASelection.StartTick) div AQuantumTicks;
          LDurationCells := LSpan.DurationTicks div AQuantumTicks;
          Result[LCellIndex] := MakeWfcMusicAttackCell(
            LSpan.Tones[0].Pitch, LSpan.Tones[0].Velocity);
          for J := 1 to LDurationCells - 1 do
            Result[LCellIndex + J] := MakeWfcMusicHoldCell(
              LSpan.Tones[0].Pitch, LSpan.Tones[0].Velocity);
        end;
    else
      MusicTrainingError(Format(
        'unknown music span kind [%d, %d]', [ASelectionIndex, I]));
    end;
  end;
end;

function ProjectTokens(const AScore: TWfcMusicScore;
  const ACells: TWfcMusicMelodyCells;
  const AProjection: TWfcMusicTrainingProjection): TWfcModelTokens;
var
  LHarmony: TWfcMusicHarmonyCells;
  LRhythm: TWfcMusicRhythmCells;
begin
  Result := nil;
  case AProjection of
    wmtpMelody:
      Result := EncodeWfcMusicMelodyCells(ACells);
    wmtpRhythm:
      begin
        LRhythm := ProjectWfcMusicMelodyToRhythm(ACells);
        Result := EncodeWfcMusicRhythmCells(LRhythm);
      end;
    wmtpHarmony:
      begin
        LHarmony := ProjectWfcMusicMelodyToHarmony(
          ACells, AScore.StepsPerOctave);
        Result := EncodeWfcMusicHarmonyCells(LHarmony);
      end;
  else
    MusicTrainingError('unknown music training projection');
  end;
end;

function BuildDocumentCore(const AScore: TWfcMusicScore;
  const ASelections: TWfcMusicTrainingSelections;
  const AProjection: TWfcMusicTrainingProjection;
  const AQuantumTicks, AOrder: Integer;
  const AMetadata: TWfcTrainingMetadata): TWfcTrainingDocument;
var
  I: Integer;
  LCells: TWfcMusicMelodyCells;
  LOptions: TWfcTrainingOptions;
  LSamples: TWfcTrainingSamples;
begin
  Result := nil;
  ValidatePreflight(AScore, ASelections, AProjection, AQuantumTicks,
    AOrder, AMetadata);
  SetLength(LSamples, Length(ASelections));
  for I := 0 to Length(ASelections) - 1 do
  begin
    LCells := ExtractMelodyCells(AScore, ASelections[I], I,
      AQuantumTicks);
    LSamples[I].Name := ASelections[I].Name;
    LSamples[I].Width := Length(LCells);
    LSamples[I].Height := 1;
    LSamples[I].Tokens := ProjectTokens(AScore, LCells, AProjection);
  end;
  LOptions := MakeWfcTrainingOptions(wtkSequence, wmbOpen, wmsNone,
    0, 0, AOrder);
  Result := TWfcTrainingDocument.Create(AMetadata, LOptions, LSamples);
end;

function BuildWfcMusicTrainingDocument(
  const AScore: TWfcMusicScore;
  const ASelections: TWfcMusicTrainingSelections;
  const AProjection: TWfcMusicTrainingProjection;
  const AQuantumTicks, AOrder: Integer;
  const AMetadata: TWfcTrainingMetadata): TWfcTrainingDocument;
begin
  Result := nil;
  try
    Result := BuildDocumentCore(AScore, ASelections, AProjection,
      AQuantumTicks, AOrder, AMetadata);
  except
    on E: EWfcMusicTraining do
      raise;
    on E: EWfcMusic do
      raise EWfcMusicTraining.Create(E.Message);
    on E: EWfcTraining do
      raise EWfcMusicTraining.Create(E.Message);
  end;
end;

end.
