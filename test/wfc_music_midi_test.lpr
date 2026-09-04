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
program wfc_music_midi_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc_model,
  wfc_music,
  wfc_midi_smf,
  wfc_music_midi;

type
  TTestProcedure = procedure;

const
  GOLDEN_MUSIC_MIDI =
    '4D54686400000006000000010060' +
    '4D54726B00000043' +
    '00FF510307A120' +
    '00FF580402021808' +
    '00903C64' +
    '60803C00' +
    '00913746' +
    '60FF51030927C0' +
    '00FF580406031808' +
    '00813700' +
    '00904050' +
    '0090435A' +
    '60804000' +
    '00804300' +
    '8140FF2F00';

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GCheckCount);
  if ACondition then
    WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailureCount);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try
    ATest;
  except
    on E: Exception do
    begin
      Inc(GFailureCount);
      WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function HexValue(const ACharacter: Char): Integer;
begin
  if ACharacter in ['0'..'9'] then
    Result := Ord(ACharacter) - Ord('0')
  else if ACharacter in ['A'..'F'] then
    Result := Ord(ACharacter) - Ord('A') + 10
  else
    Result := -1;
end;

function HexBytes(const AHex: String): TWfcMidiBytes;
var
  I: Integer;
  LIndex: Integer;
begin
  if (Length(AHex) mod 2) <> 0 then
    raise Exception.Create('test hex has an odd length');
  Result := nil;
  SetLength(Result, Length(AHex) div 2);
  I := 1;
  LIndex := 0;
  while I <= Length(AHex) do
  begin
    if (HexValue(AHex[I]) < 0) or (HexValue(AHex[I + 1]) < 0) then
      raise Exception.Create('test hex has a non-hex character');
    Result[LIndex] := Byte((HexValue(AHex[I]) shl 4) or
      HexValue(AHex[I + 1]));
    Inc(LIndex);
    Inc(I, 2);
  end;
end;

function BytesMatch(const A, B: TWfcMidiBytes): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to Length(A) - 1 do
    if A[I] <> B[I] then
      Exit(False);
  Result := True;
end;

function TonesOf(const AValues: array of TWfcMusicTone): TWfcMusicTones;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function BuildGoldenScore: TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetLength(LTracks, 1);
  LTracks[0] := MakeWfcMusicTrack(TWfcModelToken('score'),
    TWfcModelToken('Golden score'));
  SetLength(LVoices, 2);
  LVoices[0] := MakeWfcMusicVoice(0, TWfcModelToken('lead'));
  LVoices[1] := MakeWfcMusicVoice(0, TWfcModelToken('bass'));
  SetLength(LMeters, 2);
  LMeters[0] := MakeWfcMusicMeterChange(0, 2, 4);
  LMeters[1] := MakeWfcMusicMeterChange(192, 6, 8);
  SetLength(LTempos, 2);
  LTempos[0] := MakeWfcMusicTempoChange(0, 500000);
  LTempos[1] := MakeWfcMusicTempoChange(192, 600000);
  SetLength(LSpans, 7);
  LSpans[0] := MakeWfcMusicSound(0, 0, 96,
    TonesOf([MakeWfcMusicTone(60, 100)]));
  LSpans[1] := MakeWfcMusicRest(0, 96, 96);
  LSpans[2] := MakeWfcMusicSound(0, 192, 96,
    TonesOf([MakeWfcMusicTone(64, 80),
      MakeWfcMusicTone(67, 90)]));
  LSpans[3] := MakeWfcMusicRest(0, 288, 192);
  LSpans[4] := MakeWfcMusicRest(1, 0, 96);
  LSpans[5] := MakeWfcMusicSound(1, 96, 96,
    TonesOf([MakeWfcMusicTone(55, 70)]));
  LSpans[6] := MakeWfcMusicRest(1, 192, 288);
  Result := TWfcMusicScore.Create(96, 12, 480, LTracks, LVoices,
    LMeters, LTempos, LSpans);
end;

function BuildSingleVoiceScore(const ATicksPerQuarter,
  AStepsPerOctave, ALengthTicks, AMeterNumerator,
  AMeterDenominator, ATempo, APitch: Integer): TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetLength(LTracks, 1);
  LTracks[0] := MakeWfcMusicTrack(TWfcModelToken('track'), '');
  SetLength(LVoices, 1);
  LVoices[0] := MakeWfcMusicVoice(0, TWfcModelToken('voice'));
  SetLength(LMeters, 1);
  LMeters[0] := MakeWfcMusicMeterChange(0, AMeterNumerator,
    AMeterDenominator);
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, ATempo);
  SetLength(LSpans, 1);
  if APitch < 0 then
    LSpans[0] := MakeWfcMusicRest(0, 0, ALengthTicks)
  else
    LSpans[0] := MakeWfcMusicSound(0, 0, ALengthTicks,
      TonesOf([MakeWfcMusicTone(APitch, 100)]));
  Result := TWfcMusicScore.Create(ATicksPerQuarter,
    AStepsPerOctave, ALengthTicks, LTracks, LVoices,
    LMeters, LTempos, LSpans);
end;

function BuildManyVoiceScore(const AVoiceCount: Integer;
  const ASoundLastVoice: Boolean): TWfcMusicScore;
var
  I: Integer;
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetLength(LTracks, 1);
  LTracks[0] := MakeWfcMusicTrack(TWfcModelToken('track'), '');
  SetLength(LVoices, AVoiceCount);
  SetLength(LSpans, AVoiceCount);
  for I := 0 to AVoiceCount - 1 do
  begin
    LVoices[I] := MakeWfcMusicVoice(0,
      TWfcModelToken('v' + IntToStr(I)));
    if ASoundLastVoice and (I = AVoiceCount - 1) then
      LSpans[I] := MakeWfcMusicSound(I, 0, 4,
        TonesOf([MakeWfcMusicTone(60, 100)]))
    else
      LSpans[I] := MakeWfcMusicRest(I, 0, 4);
  end;
  SetLength(LMeters, 1);
  LMeters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, 500000);
  Result := TWfcMusicScore.Create(1, 12, 4, LTracks, LVoices,
    LMeters, LTempos, LSpans);
end;

procedure CheckExportRejected(const AScore: TWfcMusicScore;
  const AMessage: String);
var
  LBytes: TWfcMidiBytes;
  LRaised: Boolean;
begin
  LRaised := False;
  try
    LBytes := EncodeWfcMusicMidi(AScore);
    if Length(LBytes) = High(Integer) then
      WriteLn('unreachable');
  except
    on E: EWfcMusicMidi do LRaised := True;
  end;
  Check(LRaised, AMessage);
end;

procedure TestGoldenExport;
var
  LBytes: TWfcMidiBytes;
  LDecoded: TWfcMidiFile;
  LFile: TWfcMidiFile;
  LScore: TWfcMusicScore;
begin
  LScore := BuildGoldenScore;
  try
    LBytes := EncodeWfcMusicMidi(LScore);
    Check(BytesMatch(LBytes, HexBytes(GOLDEN_MUSIC_MIDI)),
      'the musical score matches the exact 89-byte format-0 file');
    LFile := BuildWfcMusicMidiFile(LScore);
    Check((LFile.Format = 0) and (LFile.TicksPerQuarter = 96) and
      (Length(LFile.Tracks) = 1) and
      (Length(LFile.Tracks[0].Events) = 12) and
      (LFile.Tracks[0].EndDeltaTicks = 192),
      'the adapter produces one complete format-0 track');
    Check(BytesMatch(EncodeWfcMidiFile(LFile), LBytes),
      'building and encoding are separate deterministic stages');
    LDecoded := DecodeWfcMidiFile(LBytes);
    Check(BytesMatch(EncodeWfcMidiFile(LDecoded), LBytes),
      'the exported bytes pass the independent SMF reader');
  finally
    LScore.Free;
  end;
end;

procedure TestOrderingAndMapping;
var
  I: Integer;
  LFile: TWfcMidiFile;
  LScore: TWfcMusicScore;
begin
  LScore := BuildGoldenScore;
  try
    LFile := BuildWfcMusicMidiFile(LScore);
    Check((LFile.Tracks[0].Events[0].MetaType = $51) and
      (LFile.Tracks[0].Events[1].MetaType = $58) and
      (LFile.Tracks[0].Events[0].DeltaTicks = 0) and
      (LFile.Tracks[0].Events[1].DeltaTicks = 0),
      'tempo precedes meter at the initial tick');
    Check((LFile.Tracks[0].Events[5].MetaType = $51) and
      (LFile.Tracks[0].Events[6].MetaType = $58) and
      (LFile.Tracks[0].Events[7].Status = $81) and
      (LFile.Tracks[0].Events[8].Status = $90) and
      (LFile.Tracks[0].Events[9].Status = $90),
      'metadata, note-offs, then note-ons order a shared tick');
    Check((LFile.Tracks[0].Events[4].Status = $91) and
      (LFile.Tracks[0].Events[4].Data[0] = 55),
      'voice index one maps directly to MIDI channel one');
    Check((LFile.Tracks[0].Events[8].Data[0] = 64) and
      (LFile.Tracks[0].Events[9].Data[0] = 67),
      'chord note-ons retain canonical ascending pitch order');
    for I := 0 to Length(LFile.Tracks[0].Events) - 1 do
      if (LFile.Tracks[0].Events[I].Status and $F0) = $80 then
        Check(LFile.Tracks[0].Events[I].Data[1] = 0,
          Format('note-off %d has explicit release velocity zero', [I]));
  finally
    LScore.Free;
  end;
end;

procedure TestSixteenChannels;
var
  LFile: TWfcMidiFile;
  LScore: TWfcMusicScore;
begin
  LScore := BuildManyVoiceScore(16, True);
  try
    LFile := BuildWfcMusicMidiFile(LScore);
    Check((Length(LFile.Tracks[0].Events) = 4) and
      (LFile.Tracks[0].Events[2].Status = $9F) and
      (LFile.Tracks[0].Events[3].Status = $8F),
      'voice index fifteen maps to the last MIDI channel');
    Check(LFile.Tracks[0].Events[3].DeltaTicks = 4,
      'a sounding final voice retains its exact duration');
  finally
    LScore.Free;
  end;
end;

procedure TestAdapterGuards;
var
  LScore: TWfcMusicScore;
begin
  CheckExportRejected(nil, 'a nil score is rejected');

  LScore := BuildSingleVoiceScore(96, 19, 384, 4, 4,
    500000, -1);
  try
    CheckExportRejected(LScore,
      'non-twelve-tone pitch systems are not silently remapped');
  finally
    LScore.Free;
  end;

  LScore := BuildSingleVoiceScore(32768, 12, 131072, 4, 4,
    500000, -1);
  try
    CheckExportRejected(LScore,
      'ticks per quarter must fit the positive PPQN field');
  finally
    LScore.Free;
  end;

  LScore := BuildManyVoiceScore(17, False);
  try
    CheckExportRejected(LScore,
      'more than sixteen voices cannot map to MIDI channels');
  finally
    LScore.Free;
  end;

  LScore := BuildSingleVoiceScore(96, 12, 384, 4, 4,
    500000, 128);
  try
    CheckExportRejected(LScore,
      'pitches above 127 are rejected at the MIDI boundary');
  finally
    LScore.Free;
  end;

  LScore := BuildSingleVoiceScore(96, 12, 384, 4, 4,
    $1000000, -1);
  try
    CheckExportRejected(LScore,
      'tempo must fit the three-byte MIDI representation');
  finally
    LScore.Free;
  end;

  LScore := BuildSingleVoiceScore(1, 12, 256, 256, 4,
    500000, -1);
  try
    CheckExportRejected(LScore,
      'meter numerator must fit the MIDI byte representation');
  finally
    LScore.Free;
  end;

  LScore := BuildSingleVoiceScore(32767, 12, 268558332, 4, 4,
    500000, -1);
  try
    CheckExportRejected(LScore,
      'a tail rest cannot overflow a four-byte MIDI delta');
  finally
    LScore.Free;
  end;
end;

procedure TestRepeatability;
var
  LFirst: TWfcMidiBytes;
  LScore: TWfcMusicScore;
begin
  LScore := BuildGoldenScore;
  try
    LFirst := EncodeWfcMusicMidi(LScore);
    Check(BytesMatch(LFirst, EncodeWfcMusicMidi(LScore)),
      'repeated exports are byte-for-byte deterministic');
    Check((WFC_MUSIC_MIDI_VERSION = 1) and
      (WFC_MIDI_SMF_VERSION = 1),
      'music export and its SMF artifact contract are versioned');
  finally
    LScore.Free;
  end;
end;

begin
  WriteLn('WFC music MIDI adapter conformance suite');
  WriteLn('========================================');
  RunTest('exact format-0 export', @TestGoldenExport);
  RunTest('timeline ordering and channel mapping',
    @TestOrderingAndMapping);
  RunTest('sixteen-channel boundary', @TestSixteenChannels);
  RunTest('adapter guards', @TestAdapterGuards);
  RunTest('repeatability and versions', @TestRepeatability);
  WriteLn('========================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d music MIDI checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
