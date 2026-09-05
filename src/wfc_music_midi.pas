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
unit wfc_music_midi;

{$mode delphi}{$H+}

interface

uses
  wfc_music,
  wfc_midi_smf;

const
  WFC_MUSIC_MIDI_VERSION = 1;

type
  EWfcMusicMidi = class(EWfcMusic);

function BuildWfcMusicMidiFile(
  const AScore: TWfcMusicScore): TWfcMidiFile;

function EncodeWfcMusicMidi(
  const AScore: TWfcMusicScore): TWfcMidiBytes;

implementation

uses
  SysUtils;

type
  TWfcMusicMidiEventPriority = (
    wmepMetadata,
    wmepNoteOff,
    wmepNoteOn
  );

  TWfcMusicMidiTimelineEvent = record
    Tick: Integer;
    Priority: TWfcMusicMidiEventPriority;
    Ordinal: Integer;
    EventData: TWfcMidiEvent;
  end;
  TWfcMusicMidiTimeline = array of TWfcMusicMidiTimelineEvent;

procedure MidiMusicError(const AMessage: String);
begin
  raise EWfcMusicMidi.Create('cannot export WFC music to MIDI: ' +
    AMessage);
end;

procedure AppendTimelineEvent(var ATimeline: TWfcMusicMidiTimeline;
  var ACount: Integer; const ATick: Integer;
  const APriority: TWfcMusicMidiEventPriority;
  const AEvent: TWfcMidiEvent);
var
  LNewCapacity: Integer;
begin
  if ATick < 0 then
    MidiMusicError('event tick cannot be negative');
  if ACount = High(Integer) then
    MidiMusicError('event count exceeds the Integer range');
  if ACount >= Length(ATimeline) then
  begin
    LNewCapacity := Length(ATimeline);
    if LNewCapacity < 16 then
      LNewCapacity := 16
    else
    begin
      if LNewCapacity > High(Integer) div 2 then
        MidiMusicError('event capacity exceeds the Integer range');
      LNewCapacity := LNewCapacity * 2;
    end;
    SetLength(ATimeline, LNewCapacity);
  end;
  ATimeline[ACount].Tick := ATick;
  ATimeline[ACount].Priority := APriority;
  ATimeline[ACount].Ordinal := ACount;
  ATimeline[ACount].EventData := AEvent;
  Inc(ACount);
end;

function CompareTimelineEvents(const A,
  B: TWfcMusicMidiTimelineEvent): Integer;
begin
  if A.Tick < B.Tick then
    Exit(-1);
  if A.Tick > B.Tick then
    Exit(1);
  if Ord(A.Priority) < Ord(B.Priority) then
    Exit(-1);
  if Ord(A.Priority) > Ord(B.Priority) then
    Exit(1);
  if A.Ordinal < B.Ordinal then
    Result := -1
  else if A.Ordinal > B.Ordinal then
    Result := 1
  else
    Result := 0;
end;

procedure MergeTimeline(var AValues, AWork: TWfcMusicMidiTimeline;
  const ALow, AMiddle, AHigh: Integer);
var
  I: Integer;
  J: Integer;
  K: Integer;
begin
  I := ALow;
  J := AMiddle + 1;
  K := ALow;
  while (I <= AMiddle) and (J <= AHigh) do
  begin
    if CompareTimelineEvents(AValues[I], AValues[J]) <= 0 then
    begin
      AWork[K] := AValues[I];
      Inc(I);
    end
    else
    begin
      AWork[K] := AValues[J];
      Inc(J);
    end;
    Inc(K);
  end;
  while I <= AMiddle do
  begin
    AWork[K] := AValues[I];
    Inc(I);
    Inc(K);
  end;
  while J <= AHigh do
  begin
    AWork[K] := AValues[J];
    Inc(J);
    Inc(K);
  end;
  for K := ALow to AHigh do
    AValues[K] := AWork[K];
end;

procedure SortTimelineRange(var AValues,
  AWork: TWfcMusicMidiTimeline; const ALow, AHigh: Integer);
var
  LMiddle: Integer;
begin
  if ALow >= AHigh then
    Exit;
  LMiddle := ALow + ((AHigh - ALow) div 2);
  SortTimelineRange(AValues, AWork, ALow, LMiddle);
  SortTimelineRange(AValues, AWork, LMiddle + 1, AHigh);
  MergeTimeline(AValues, AWork, ALow, LMiddle, AHigh);
end;

procedure SortTimeline(var ATimeline: TWfcMusicMidiTimeline);
var
  LWork: TWfcMusicMidiTimeline;
begin
  if Length(ATimeline) < 2 then
    Exit;
  LWork := nil;
  SetLength(LWork, Length(ATimeline));
  SortTimelineRange(ATimeline, LWork, 0, Length(ATimeline) - 1);
end;

function MeterDenominatorPower(const ADenominator: Integer): Byte;
var
  LPower: Integer;
  LValue: Integer;
begin
  if ADenominator < 1 then
    MidiMusicError('meter denominator must be positive');
  LPower := 0;
  LValue := ADenominator;
  while (LValue > 1) and ((LValue and 1) = 0) do
  begin
    LValue := LValue shr 1;
    Inc(LPower);
  end;
  if LValue <> 1 then
    MidiMusicError('meter denominator must be a power of two');
  if LPower > High(Byte) then
    MidiMusicError('meter denominator power exceeds the MIDI byte range');
  Result := Byte(LPower);
end;

procedure ValidateScoreForMidi(const AScore: TWfcMusicScore);
var
  I: Integer;
  J: Integer;
  LMeter: TWfcMusicMeterChange;
  LSpan: TWfcMusicSpanEvent;
  LTempo: TWfcMusicTempoChange;
begin
  if AScore = nil then
    MidiMusicError('score cannot be nil');
  if AScore.StepsPerOctave <> 12 then
    MidiMusicError('MIDI export requires exactly 12 steps per octave');
  if (AScore.TicksPerQuarter < 1) or
      (AScore.TicksPerQuarter > $7FFF) then
    MidiMusicError('ticks per quarter must be from 1 through 32767');
  if AScore.VoiceCount > 16 then
    MidiMusicError('format-0 voice-to-channel mapping supports at most 16 voices');

  for I := 0 to AScore.TempoCount - 1 do
  begin
    LTempo := AScore.TempoAt(I);
    if (LTempo.MicrosecondsPerQuarter < 1) or
        (LTempo.MicrosecondsPerQuarter > $FFFFFF) then
      MidiMusicError(Format(
        'tempo is outside the MIDI three-byte range [%d]', [I]));
  end;
  for I := 0 to AScore.MeterCount - 1 do
  begin
    LMeter := AScore.MeterAt(I);
    if (LMeter.Numerator < 1) or (LMeter.Numerator > High(Byte)) then
      MidiMusicError(Format(
        'meter numerator is outside the MIDI byte range [%d]', [I]));
    MeterDenominatorPower(LMeter.Denominator);
  end;
  for I := 0 to AScore.SpanCount - 1 do
  begin
    LSpan := AScore.SpanAt(I);
    for J := 0 to Length(LSpan.Tones) - 1 do
      if (LSpan.Tones[J].Pitch < 0) or
          (LSpan.Tones[J].Pitch > 127) then
        MidiMusicError(Format(
          'pitch is outside the MIDI note range [%d, %d]', [I, J]));
  end;
end;

procedure AppendMetadata(const AScore: TWfcMusicScore;
  var ATimeline: TWfcMusicMidiTimeline; var ACount: Integer);
var
  I: Integer;
  LMeter: TWfcMusicMeterChange;
  LTempo: TWfcMusicTempoChange;
begin
  { Tempo precedes meter when both change at the same tick. }
  for I := 0 to AScore.TempoCount - 1 do
  begin
    LTempo := AScore.TempoAt(I);
    AppendTimelineEvent(ATimeline, ACount, LTempo.Tick,
      wmepMetadata, MakeWfcMidiTempoEvent(0,
        Cardinal(LTempo.MicrosecondsPerQuarter)));
  end;
  for I := 0 to AScore.MeterCount - 1 do
  begin
    LMeter := AScore.MeterAt(I);
    AppendTimelineEvent(ATimeline, ACount, LMeter.Tick,
      wmepMetadata, MakeWfcMidiTimeSignatureEvent(0,
        Byte(LMeter.Numerator),
        MeterDenominatorPower(LMeter.Denominator), 24, 8));
  end;
end;

procedure AppendSpanEvents(const AScore: TWfcMusicScore;
  var ATimeline: TWfcMusicMidiTimeline; var ACount: Integer);
var
  I: Integer;
  J: Integer;
  LChannel: Byte;
  LEndTick: Integer;
  LSpan: TWfcMusicSpanEvent;
begin
  for I := 0 to AScore.SpanCount - 1 do
  begin
    LSpan := AScore.SpanAt(I);
    if LSpan.Kind = wmskRest then
      Continue;
    if LSpan.StartTick > High(Integer) - LSpan.DurationTicks then
      MidiMusicError(Format('span end tick overflows [%d]', [I]));
    LEndTick := LSpan.StartTick + LSpan.DurationTicks;
    LChannel := Byte(LSpan.VoiceIndex);
    for J := 0 to Length(LSpan.Tones) - 1 do
    begin
      AppendTimelineEvent(ATimeline, ACount, LSpan.StartTick,
        wmepNoteOn, MakeWfcMidiChannelEvent(0,
          Byte($90 or LChannel),
          [Byte(LSpan.Tones[J].Pitch),
           Byte(LSpan.Tones[J].Velocity)]));
      AppendTimelineEvent(ATimeline, ACount, LEndTick,
        wmepNoteOff, MakeWfcMidiChannelEvent(0,
          Byte($80 or LChannel),
          [Byte(LSpan.Tones[J].Pitch), 0]));
    end;
  end;
end;

function BuildWfcMusicMidiFile(
  const AScore: TWfcMusicScore): TWfcMidiFile;
var
  I: Integer;
  LCount: Integer;
  LDelta: Integer;
  LPreviousTick: Integer;
  LTimeline: TWfcMusicMidiTimeline;
begin
  ValidateScoreForMidi(AScore);
  LTimeline := nil;
  LCount := 0;
  AppendMetadata(AScore, LTimeline, LCount);
  AppendSpanEvents(AScore, LTimeline, LCount);
  SetLength(LTimeline, LCount);
  SortTimeline(LTimeline);

  Result.Format := 0;
  Result.TicksPerQuarter := Word(AScore.TicksPerQuarter);
  SetLength(Result.Tracks, 1);
  SetLength(Result.Tracks[0].Events, LCount);
  LPreviousTick := 0;
  for I := 0 to LCount - 1 do
  begin
    LDelta := LTimeline[I].Tick - LPreviousTick;
    if (LDelta < 0) or
        (Cardinal(LDelta) > WFC_MIDI_MAX_VARIABLE_LENGTH) then
      MidiMusicError(Format(
        'event delta exceeds the MIDI variable-length range [%d]', [I]));
    Result.Tracks[0].Events[I] := LTimeline[I].EventData;
    Result.Tracks[0].Events[I].DeltaTicks := Cardinal(LDelta);
    LPreviousTick := LTimeline[I].Tick;
  end;
  LDelta := AScore.LengthTicks - LPreviousTick;
  if (LDelta < 0) or
      (Cardinal(LDelta) > WFC_MIDI_MAX_VARIABLE_LENGTH) then
    MidiMusicError('end-of-track delta exceeds the MIDI variable-length range');
  Result.Tracks[0].EndDeltaTicks := Cardinal(LDelta);
end;

function EncodeWfcMusicMidi(
  const AScore: TWfcMusicScore): TWfcMidiBytes;
begin
  Result := EncodeWfcMidiFile(BuildWfcMusicMidiFile(AScore));
end;

end.
