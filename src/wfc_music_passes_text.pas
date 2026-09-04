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
unit wfc_music_passes_text;

{$mode delphi}{$H+}

interface

uses
  wfc_music_passes;

const
  WFC_MUSIC_PASSES_TEXT_VERSION = 1;

function EncodeWfcMusicPassesText(
  const AComposition: TWfcMusicComposition): String;
function DecodeWfcMusicPassesText(
  const AText: String): TWfcMusicComposition;

implementation

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_text_codec,
  wfc_music,
  wfc_music_text,
  wfc_sequence_graph;

const
  MUSIC_PASS_ARTIFACT = 'wfcmusicpass=1';

type
  TStringParts = array of String;

procedure TextError(const AMessage: String);
begin
  WfcTextError(MUSIC_PASS_ARTIFACT, AMessage);
end;

function IntText(const AValue: Integer): String;
begin
  if AValue < 0 then
    raise EWfcMusic.Create(
      'canonical music-pass text cannot encode a negative integer');
  Result := IntToStr(AValue);
end;

procedure AppendLine(var ALines: TWfcTextLines;
  var ACount: Integer; const ALine: String);
begin
  if ACount = High(Integer) then
    raise ERangeError.Create(
      'canonical music-pass text has too many lines');
  SetLength(ALines, ACount + 1);
  ALines[ACount] := ALine;
  Inc(ACount);
end;

function CopyTokens(const AValues: TWfcModelTokens): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function CompositionTokens(const AComposition: TWfcMusicComposition;
  const ALayer: TWfcMusicPassLayer): TWfcModelTokens;
var
  LGenerated: TWfcGeneratedSequence;
begin
  LGenerated := AComposition.CopyGenerated(ALayer);
  Result := CopyTokens(LGenerated.Tokens);
end;

procedure AppendTokenRecords(var ALines: TWfcTextLines;
  var ALineCount: Integer; const APrefix: String;
  const ATokens: TWfcModelTokens);
var
  I: Integer;
begin
  for I := 0 to Length(ATokens) - 1 do
    AppendLine(ALines, ALineCount, APrefix + '=' + IntText(I) + ',' +
      WfcTextEncodeToken(ATokens[I], MUSIC_PASS_ARTIFACT));
end;

function EncodeWfcMusicPassesText(
  const AComposition: TWfcMusicComposition): String;
var
  LCalculatedSignature: TWfcMusicCompositionSignature;
  LCount: Integer;
  LHarmonyTokens: TWfcModelTokens;
  LLines: TWfcTextLines;
  LMelodyTokens: TWfcModelTokens;
  LRhythmTokens: TWfcModelTokens;
  LScore: TWfcMusicScore;
  LScoreText: String;
begin
  if not Assigned(AComposition) then
    raise EArgumentNilException.Create('music composition cannot be nil');

  LHarmonyTokens := CompositionTokens(AComposition, wmplHarmony);
  LRhythmTokens := CompositionTokens(AComposition, wmplRhythm);
  LMelodyTokens := CompositionTokens(AComposition, wmplMelody);
  if (Length(LHarmonyTokens) <> AComposition.CellCount) or
      (Length(LRhythmTokens) <> AComposition.CellCount) or
      (Length(LMelodyTokens) <> AComposition.CellCount) then
    raise EWfcMusic.Create(
      'music composition public token counts do not match its cell count');

  LScore := AComposition.CopyScore;
  try
    if not Assigned(LScore) then
      raise EWfcMusic.Create('music composition score cannot be nil');
    LCalculatedSignature := CalculateWfcMusicCompositionSignature(
      AComposition.Seed, AComposition.QuantumTicks, LHarmonyTokens,
      LRhythmTokens, LMelodyTokens, LScore);
    if LCalculatedSignature <> AComposition.Signature then
      raise EWfcMusic.Create(
        'music composition signature does not match its public data');
    LScoreText := EncodeWfcMusicText(LScore);
  finally
    LScore.Free;
  end;

  LLines := nil;
  LCount := 0;
  AppendLine(LLines, LCount, MUSIC_PASS_ARTIFACT);
  AppendLine(LLines, LCount, 'seed=' + UIntToStr(AComposition.Seed));
  AppendLine(LLines, LCount, 'quantum=' +
    IntText(AComposition.QuantumTicks));
  AppendLine(LLines, LCount, 'cells=' + IntText(AComposition.CellCount));
  AppendTokenRecords(LLines, LCount, 'harmony', LHarmonyTokens);
  AppendTokenRecords(LLines, LCount, 'rhythm', LRhythmTokens);
  AppendTokenRecords(LLines, LCount, 'melody', LMelodyTokens);
  AppendLine(LLines, LCount, 'signature=' +
    WfcMusicCompositionSignatureHex(AComposition.Signature));
  AppendLine(LLines, LCount, 'score=' + WfcTextEncodeToken(
    TWfcModelToken(LScoreText), MUSIC_PASS_ARTIFACT));
  AppendLine(LLines, LCount, 'end');
  Result := WfcTextJoinCanonicalLines(LLines, MUSIC_PASS_ARTIFACT);
end;

function ParseCanonicalSeed(const AText: String): TGraphSeed;
var
  I: Integer;
  LDigit: TGraphSeed;
begin
  if AText = '' then
    TextError('seed is empty');
  if (Length(AText) > 1) and (AText[1] = '0') then
    TextError('seed has a leading zero');
  Result := 0;
  for I := 1 to Length(AText) do
  begin
    if not (AText[I] in ['0'..'9']) then
      TextError('seed is not a canonical unsigned decimal integer');
    LDigit := TGraphSeed(Ord(AText[I]) - Ord('0'));
    if Result > (High(TGraphSeed) - LDigit) div 10 then
      TextError('seed exceeds the unsigned 32-bit range');
    Result := Result * 10 + LDigit;
  end;
end;

function ParseInteger(const AText, AField: String): Integer;
begin
  Result := WfcTextParseCanonicalInteger(AText, AField,
    MUSIC_PASS_ARTIFACT);
end;

function ReadValueLine(const ALines: TWfcTextLines;
  var ALineIndex: Integer; const APrefix, AField: String): String;
begin
  if ALineIndex >= Length(ALines) then
    TextError('document is truncated before ' + AField);
  Result := WfcTextValueAfterPrefix(ALines[ALineIndex], APrefix,
    AField, MUSIC_PASS_ARTIFACT);
  Inc(ALineIndex);
end;

function SplitTokenRecord(const AText: String;
  const AField: String): TStringParts;
var
  LComma: Integer;
begin
  Result := nil;
  LComma := WfcTextFindCharacter(AText, ',', 1);
  if (LComma = 0) or
      (WfcTextFindCharacter(AText, ',', LComma + 1) <> 0) then
    TextError(AField + ' record must contain exactly two fields');
  SetLength(Result, 2);
  Result[0] := Copy(AText, 1, LComma - 1);
  Result[1] := Copy(AText, LComma + 1,
    Length(AText) - LComma);
end;

procedure ReadTokenRecords(const ALines: TWfcTextLines;
  var ALineIndex: Integer; const APrefix, AField: String;
  const ACount: Integer; out ATokens: TWfcModelTokens);
var
  I: Integer;
  LParts: TStringParts;
begin
  ATokens := nil;
  SetLength(ATokens, ACount);
  for I := 0 to ACount - 1 do
  begin
    LParts := SplitTokenRecord(ReadValueLine(ALines, ALineIndex,
      APrefix + '=', AField), AField);
    if ParseInteger(LParts[0], AField + ' index') <> I then
      TextError(AField + ' indices must be canonical and contiguous');
    ATokens[I] := WfcTextDecodeToken(LParts[1], MUSIC_PASS_ARTIFACT);
  end;
end;

procedure RequireRecordCapacity(const ACellCount: Integer;
  const ALines: TWfcTextLines; const ALineIndex: Integer);
var
  LRequired: Integer;
  LRemaining: Integer;
begin
  if ACellCount < 1 then
    TextError('cell count must be positive');
  if ACellCount > (High(Integer) - 3) div 3 then
    TextError('cell count exceeds the supported range');
  LRequired := ACellCount * 3 + 3;
  LRemaining := Length(ALines) - ALineIndex;
  if LRequired > LRemaining then
    TextError('cell records exceed the remaining document lines');
end;

function SignatureTextIsCanonical(const AText: String): Boolean;
const
  HEX_DIGITS = '0123456789ABCDEF';
var
  I: Integer;
begin
  if Length(AText) <> 8 then
    Exit(False);
  for I := 1 to Length(AText) do
    if Pos(AText[I], HEX_DIGITS) = 0 then
      Exit(False);
  Result := True;
end;

function DecodeWfcMusicPassesText(
  const AText: String): TWfcMusicComposition;
var
  LCellCount: Integer;
  LComposition: TWfcMusicComposition;
  LHarmonyTokens: TWfcModelTokens;
  LLineIndex: Integer;
  LLines: TWfcTextLines;
  LMelodyTokens: TWfcModelTokens;
  LQuantumTicks: Integer;
  LRhythmTokens: TWfcModelTokens;
  LScore: TWfcMusicScore;
  LScoreText: TWfcModelToken;
  LSeed: TGraphSeed;
  LSignatureText: String;
begin
  Result := nil;
  WfcTextSplitCanonicalLines(AText, MUSIC_PASS_ARTIFACT, LLines);
  LLineIndex := 0;
  if (Length(LLines) = 0) or (LLines[0] <> MUSIC_PASS_ARTIFACT) then
    TextError('expected wfcmusicpass=1 header');
  Inc(LLineIndex);

  LSeed := ParseCanonicalSeed(ReadValueLine(LLines, LLineIndex,
    'seed=', 'seed'));
  LQuantumTicks := ParseInteger(ReadValueLine(LLines, LLineIndex,
    'quantum=', 'quantum ticks'), 'quantum ticks');
  LCellCount := ParseInteger(ReadValueLine(LLines, LLineIndex,
    'cells=', 'cell count'), 'cell count');
  RequireRecordCapacity(LCellCount, LLines, LLineIndex);

  ReadTokenRecords(LLines, LLineIndex, 'harmony', 'harmony cell',
    LCellCount, LHarmonyTokens);
  ReadTokenRecords(LLines, LLineIndex, 'rhythm', 'rhythm cell',
    LCellCount, LRhythmTokens);
  ReadTokenRecords(LLines, LLineIndex, 'melody', 'melody cell',
    LCellCount, LMelodyTokens);
  LSignatureText := ReadValueLine(LLines, LLineIndex,
    'signature=', 'composition signature');
  if not SignatureTextIsCanonical(LSignatureText) then
    TextError('composition signature must be eight uppercase hexadecimal digits');
  LScoreText := WfcTextDecodeToken(ReadValueLine(LLines, LLineIndex,
    'score=', 'score'), MUSIC_PASS_ARTIFACT);

  if (LLineIndex >= Length(LLines)) or
      (LLines[LLineIndex] <> 'end') then
    TextError('expected end marker');
  Inc(LLineIndex);
  if LLineIndex <> Length(LLines) then
    TextError('trailing data after end marker');

  LScore := DecodeWfcMusicText(String(LScoreText));
  try
    LComposition := CreateWfcMusicComposition(LSeed, LQuantumTicks,
      LHarmonyTokens, LRhythmTokens, LMelodyTokens, LScore);
  finally
    LScore.Free;
  end;
  try
    if WfcMusicCompositionSignatureHex(LComposition.Signature) <>
        LSignatureText then
      TextError('composition signature does not match its public data');
    if EncodeWfcMusicPassesText(LComposition) <> AText then
      TextError('document is not in canonical form');
    Result := LComposition;
    LComposition := nil;
  finally
    LComposition.Free;
  end;
end;

end.
