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
unit wfc_music_text;

{$mode delphi}{$H+}

interface

uses
  wfc_music;

const
  WFC_MUSIC_TEXT_VERSION = 1;

function EncodeWfcMusicText(const AScore: TWfcMusicScore): String;
function DecodeWfcMusicText(const AText: String): TWfcMusicScore;

implementation

uses
  SysUtils,
  wfc_model,
  wfc_text_codec;

const
  MUSIC_ARTIFACT = 'wfcmusic=1';

type
  TStringParts = array of String;

function IntText(const AValue: Integer): String;
begin
  if AValue < 0 then
    raise EWfcMusic.Create(
      'canonical music text cannot encode a negative integer');
  Result := IntToStr(AValue);
end;

procedure AppendLine(var ALines: TWfcTextLines;
  var ACount: Integer; const ALine: String);
begin
  if ACount = High(Integer) then
    raise ERangeError.Create('canonical music text has too many lines');
  SetLength(ALines, ACount + 1);
  ALines[ACount] := ALine;
  Inc(ACount);
end;

function EncodeTone(const ATone: TWfcMusicTone): String;
begin
  Result := IntText(ATone.Pitch) + '@' + IntText(ATone.Velocity);
end;

function EncodeSpan(const AIndex: Integer;
  const ASpan: TWfcMusicSpanEvent): String;
var
  I: Integer;
  LKind: String;
  LTones: String;
begin
  case ASpan.Kind of
    wmskRest: LKind := 'R';
    wmskNote: LKind := 'N';
    wmskChord: LKind := 'C';
  else
    raise EWfcMusic.Create('cannot encode an unknown music span kind');
  end;
  LTones := '';
  for I := 0 to Length(ASpan.Tones) - 1 do
  begin
    if I > 0 then
      LTones := LTones + ';';
    LTones := LTones + EncodeTone(ASpan.Tones[I]);
  end;
  Result := 'span=' + IntText(AIndex) + ',' +
    IntText(ASpan.VoiceIndex) + ',' + IntText(ASpan.StartTick) + ',' +
    IntText(ASpan.DurationTicks) + ',' + LKind + ',' + LTones;
end;

function EncodeWfcMusicText(const AScore: TWfcMusicScore): String;
var
  I: Integer;
  LCount: Integer;
  LLines: TWfcTextLines;
  LMeter: TWfcMusicMeterChange;
  LSpan: TWfcMusicSpanEvent;
  LTempo: TWfcMusicTempoChange;
  LTrack: TWfcMusicTrack;
  LVoice: TWfcMusicVoice;
begin
  if not Assigned(AScore) then
    raise EArgumentNilException.Create('music score cannot be nil');
  LLines := nil;
  LCount := 0;
  AppendLine(LLines, LCount, MUSIC_ARTIFACT);
  AppendLine(LLines, LCount, 'tpq=' + IntText(AScore.TicksPerQuarter));
  AppendLine(LLines, LCount, 'steps=' + IntText(AScore.StepsPerOctave));
  AppendLine(LLines, LCount, 'length=' + IntText(AScore.LengthTicks));

  AppendLine(LLines, LCount, 'tracks=' + IntText(AScore.TrackCount));
  for I := 0 to AScore.TrackCount - 1 do
  begin
    LTrack := AScore.TrackAt(I);
    AppendLine(LLines, LCount, 'track=' + IntText(I) + ',' +
      WfcTextEncodeToken(LTrack.Id, MUSIC_ARTIFACT) + ',' +
      WfcTextEncodeToken(LTrack.Name, MUSIC_ARTIFACT));
  end;

  AppendLine(LLines, LCount, 'voices=' + IntText(AScore.VoiceCount));
  for I := 0 to AScore.VoiceCount - 1 do
  begin
    LVoice := AScore.VoiceAt(I);
    AppendLine(LLines, LCount, 'voice=' + IntText(I) + ',' +
      IntText(LVoice.TrackIndex) + ',' +
      WfcTextEncodeToken(LVoice.Id, MUSIC_ARTIFACT));
  end;

  AppendLine(LLines, LCount, 'meters=' + IntText(AScore.MeterCount));
  for I := 0 to AScore.MeterCount - 1 do
  begin
    LMeter := AScore.MeterAt(I);
    AppendLine(LLines, LCount, 'meter=' + IntText(I) + ',' +
      IntText(LMeter.Tick) + ',' + IntText(LMeter.Numerator) + ',' +
      IntText(LMeter.Denominator));
  end;

  AppendLine(LLines, LCount, 'tempos=' + IntText(AScore.TempoCount));
  for I := 0 to AScore.TempoCount - 1 do
  begin
    LTempo := AScore.TempoAt(I);
    AppendLine(LLines, LCount, 'tempo=' + IntText(I) + ',' +
      IntText(LTempo.Tick) + ',' +
      IntText(LTempo.MicrosecondsPerQuarter));
  end;

  AppendLine(LLines, LCount, 'spans=' + IntText(AScore.SpanCount));
  for I := 0 to AScore.SpanCount - 1 do
  begin
    LSpan := AScore.SpanAt(I);
    AppendLine(LLines, LCount, EncodeSpan(I, LSpan));
  end;
  AppendLine(LLines, LCount, 'end');
  Result := WfcTextJoinCanonicalLines(LLines, MUSIC_ARTIFACT);
end;

function SplitExact(const AText: String; const ASeparator: Char;
  const AExpectedCount: Integer; const AField: String): TStringParts;
var
  I: Integer;
  LCount: Integer;
  LStart: Integer;
begin
  if AExpectedCount < 1 then
    WfcTextError(MUSIC_ARTIFACT, 'internal split count is invalid');
  Result := nil;
  SetLength(Result, AExpectedCount);
  LCount := 0;
  LStart := 1;
  for I := 1 to Length(AText) do
    if AText[I] = ASeparator then
    begin
      if LCount >= AExpectedCount - 1 then
        WfcTextError(MUSIC_ARTIFACT, AField + ' has too many fields');
      Result[LCount] := Copy(AText, LStart, I - LStart);
      Inc(LCount);
      LStart := I + 1;
    end;
  if LCount <> AExpectedCount - 1 then
    WfcTextError(MUSIC_ARTIFACT, AField + ' has the wrong field count');
  Result[LCount] := Copy(AText, LStart, Length(AText) - LStart + 1);
end;

function ParseInteger(const AText, AField: String): Integer;
begin
  Result := WfcTextParseCanonicalInteger(AText, AField,
    MUSIC_ARTIFACT);
end;

procedure RequireIndex(const AText: String; const AExpected: Integer;
  const AField: String);
begin
  if ParseInteger(AText, AField + ' index') <> AExpected then
    WfcTextError(MUSIC_ARTIFACT,
      AField + ' indices must be canonical and contiguous');
end;

function ReadValueLine(const ALines: TWfcTextLines;
  var ALineIndex: Integer; const APrefix, AField: String): String;
begin
  if ALineIndex >= Length(ALines) then
    WfcTextError(MUSIC_ARTIFACT, 'document is truncated before ' + AField);
  Result := WfcTextValueAfterPrefix(ALines[ALineIndex], APrefix,
    AField, MUSIC_ARTIFACT);
  Inc(ALineIndex);
end;

procedure RequireAvailableRecords(const ACount: Integer;
  const ALines: TWfcTextLines; const ALineIndex: Integer;
  const AReservedLines: Integer; const AField: String);
var
  LRemaining: Integer;
begin
  if (ACount < 0) or (ALineIndex < 0) or
      (ALineIndex > Length(ALines)) or (AReservedLines < 0) then
    WfcTextError(MUSIC_ARTIFACT,
      AField + ' count exceeds the remaining document lines');
  LRemaining := Length(ALines) - ALineIndex;
  if (AReservedLines > LRemaining) or
      (ACount > LRemaining - AReservedLines) then
    WfcTextError(MUSIC_ARTIFACT,
      AField + ' count exceeds the remaining document lines');
end;

function ParseTone(const AText: String;
  const AField: String): TWfcMusicTone;
var
  LParts: TStringParts;
begin
  LParts := SplitExact(AText, '@', 2, AField);
  Result := MakeWfcMusicTone(
    ParseInteger(LParts[0], AField + ' pitch'),
    ParseInteger(LParts[1], AField + ' velocity'));
end;

function ParseTones(const AText, AKindText: String;
  const ASpanIndex: Integer): TWfcMusicTones;
var
  I: Integer;
  LCount: Integer;
  LStart: Integer;
  LToneText: String;
begin
  Result := nil;
  if AKindText = 'R' then
  begin
    if AText <> '' then
      WfcTextError(MUSIC_ARTIFACT,
        'rest span tone list must be empty');
    Exit;
  end;
  if AText = '' then
    WfcTextError(MUSIC_ARTIFACT,
      'sounding span tone list cannot be empty');
  LCount := 0;
  LStart := 1;
  for I := 1 to Length(AText) + 1 do
    if (I > Length(AText)) or (AText[I] = ';') then
    begin
      LToneText := Copy(AText, LStart, I - LStart);
      if LToneText = '' then
        WfcTextError(MUSIC_ARTIFACT,
          'span tone list contains an empty item');
      SetLength(Result, LCount + 1);
      Result[LCount] := ParseTone(LToneText,
        'span ' + IntToStr(ASpanIndex) + ' tone ' + IntToStr(LCount));
      Inc(LCount);
      LStart := I + 1;
    end;
  if (AKindText = 'N') and (LCount <> 1) then
    WfcTextError(MUSIC_ARTIFACT,
      'note span must encode exactly one tone');
  if (AKindText = 'C') and (LCount < 2) then
    WfcTextError(MUSIC_ARTIFACT,
      'chord span must encode at least two tones');
end;

function DecodeWfcMusicText(const AText: String): TWfcMusicScore;
var
  I: Integer;
  LExpectedCount: Integer;
  LKind: TWfcMusicSpanKind;
  LLengthTicks: Integer;
  LLineIndex: Integer;
  LLines: TWfcTextLines;
  LMeters: TWfcMusicMeterChanges;
  LParts: TStringParts;
  LScore: TWfcMusicScore;
  LSpans: TWfcMusicSpanEvents;
  LStepsPerOctave: Integer;
  LTempos: TWfcMusicTempoChanges;
  LTicksPerQuarter: Integer;
  LTones: TWfcMusicTones;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  Result := nil;
  WfcTextSplitCanonicalLines(AText, MUSIC_ARTIFACT, LLines);
  LLineIndex := 0;
  if (Length(LLines) = 0) or (LLines[0] <> MUSIC_ARTIFACT) then
    WfcTextError(MUSIC_ARTIFACT, 'expected wfcmusic=1 header');
  Inc(LLineIndex);
  LTicksPerQuarter := ParseInteger(ReadValueLine(LLines, LLineIndex,
    'tpq=', 'ticks per quarter'), 'ticks per quarter');
  LStepsPerOctave := ParseInteger(ReadValueLine(LLines, LLineIndex,
    'steps=', 'steps per octave'), 'steps per octave');
  LLengthTicks := ParseInteger(ReadValueLine(LLines, LLineIndex,
    'length=', 'score length'), 'score length');

  LExpectedCount := ParseInteger(ReadValueLine(LLines, LLineIndex,
    'tracks=', 'track count'), 'track count');
  RequireAvailableRecords(LExpectedCount, LLines, LLineIndex, 5, 'track');
  SetLength(LTracks, LExpectedCount);
  for I := 0 to LExpectedCount - 1 do
  begin
    LParts := SplitExact(ReadValueLine(LLines, LLineIndex,
      'track=', 'track'), ',', 3, 'track');
    RequireIndex(LParts[0], I, 'track');
    LTracks[I] := MakeWfcMusicTrack(
      WfcTextDecodeToken(LParts[1], MUSIC_ARTIFACT),
      WfcTextDecodeToken(LParts[2], MUSIC_ARTIFACT));
  end;

  LExpectedCount := ParseInteger(ReadValueLine(LLines, LLineIndex,
    'voices=', 'voice count'), 'voice count');
  RequireAvailableRecords(LExpectedCount, LLines, LLineIndex, 4, 'voice');
  SetLength(LVoices, LExpectedCount);
  for I := 0 to LExpectedCount - 1 do
  begin
    LParts := SplitExact(ReadValueLine(LLines, LLineIndex,
      'voice=', 'voice'), ',', 3, 'voice');
    RequireIndex(LParts[0], I, 'voice');
    LVoices[I] := MakeWfcMusicVoice(
      ParseInteger(LParts[1], 'voice track index'),
      WfcTextDecodeToken(LParts[2], MUSIC_ARTIFACT));
  end;

  LExpectedCount := ParseInteger(ReadValueLine(LLines, LLineIndex,
    'meters=', 'meter count'), 'meter count');
  RequireAvailableRecords(LExpectedCount, LLines, LLineIndex, 3, 'meter');
  SetLength(LMeters, LExpectedCount);
  for I := 0 to LExpectedCount - 1 do
  begin
    LParts := SplitExact(ReadValueLine(LLines, LLineIndex,
      'meter=', 'meter'), ',', 4, 'meter');
    RequireIndex(LParts[0], I, 'meter');
    LMeters[I] := MakeWfcMusicMeterChange(
      ParseInteger(LParts[1], 'meter tick'),
      ParseInteger(LParts[2], 'meter numerator'),
      ParseInteger(LParts[3], 'meter denominator'));
  end;

  LExpectedCount := ParseInteger(ReadValueLine(LLines, LLineIndex,
    'tempos=', 'tempo count'), 'tempo count');
  RequireAvailableRecords(LExpectedCount, LLines, LLineIndex, 2, 'tempo');
  SetLength(LTempos, LExpectedCount);
  for I := 0 to LExpectedCount - 1 do
  begin
    LParts := SplitExact(ReadValueLine(LLines, LLineIndex,
      'tempo=', 'tempo'), ',', 3, 'tempo');
    RequireIndex(LParts[0], I, 'tempo');
    LTempos[I] := MakeWfcMusicTempoChange(
      ParseInteger(LParts[1], 'tempo tick'),
      ParseInteger(LParts[2], 'tempo microseconds per quarter'));
  end;

  LExpectedCount := ParseInteger(ReadValueLine(LLines, LLineIndex,
    'spans=', 'span count'), 'span count');
  RequireAvailableRecords(LExpectedCount, LLines, LLineIndex, 1, 'span');
  SetLength(LSpans, LExpectedCount);
  for I := 0 to LExpectedCount - 1 do
  begin
    LParts := SplitExact(ReadValueLine(LLines, LLineIndex,
      'span=', 'span'), ',', 6, 'span');
    RequireIndex(LParts[0], I, 'span');
    if LParts[4] = 'R' then
      LKind := wmskRest
    else if LParts[4] = 'N' then
      LKind := wmskNote
    else if LParts[4] = 'C' then
      LKind := wmskChord
    else
      WfcTextError(MUSIC_ARTIFACT, 'unknown span kind');
    LTones := ParseTones(LParts[5], LParts[4], I);
    LSpans[I].VoiceIndex := ParseInteger(LParts[1], 'span voice index');
    LSpans[I].StartTick := ParseInteger(LParts[2], 'span start tick');
    LSpans[I].DurationTicks := ParseInteger(LParts[3],
      'span duration');
    LSpans[I].Kind := LKind;
    LSpans[I].Tones := LTones;
  end;

  if (LLineIndex >= Length(LLines)) or
      (LLines[LLineIndex] <> 'end') then
    WfcTextError(MUSIC_ARTIFACT, 'expected end marker');
  Inc(LLineIndex);
  if LLineIndex <> Length(LLines) then
    WfcTextError(MUSIC_ARTIFACT, 'trailing data after end marker');

  LScore := TWfcMusicScore.Create(LTicksPerQuarter,
    LStepsPerOctave, LLengthTicks, LTracks, LVoices,
    LMeters, LTempos, LSpans);
  try
    if EncodeWfcMusicText(LScore) <> AText then
      WfcTextError(MUSIC_ARTIFACT,
        'document is not in canonical form');
    Result := LScore;
    LScore := nil;
  finally
    LScore.Free;
  end;
end;

end.
