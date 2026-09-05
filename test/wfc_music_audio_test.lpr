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
program wfc_music_audio_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc_model,
  wfc_music,
  wfc_music_audio;

type
  TTestProcedure = procedure;

const
  FNV_OFFSET_BASIS = Cardinal(2166136261);
  EXPECTED_GOLDEN_PCM_HASH = Cardinal($D112EACB);
  EXPECTED_GOLDEN_WAVE_HASH = Cardinal($8FA328DD);

  GOLDEN_BOUNDARY_WAVE =
    '524946462E00000057415645666D74201000000001000100' +
    '44AC00008858010002001000646174610A000000' +
    '0080FFFF00000100FF7F';
  GOLDEN_EMPTY_WAVE =
    '524946462400000057415645666D74201000000001000100' +
    '44AC000088580100020010006461746100000000';

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

procedure HashByte(var AHash: Cardinal; const AValue: Byte);
{$PUSH}
{$Q-}
var
  LValue: Cardinal;
begin
  AHash := AHash xor Cardinal(AValue);
  LValue := AHash;
  AHash := (LValue + (LValue shl 1) + (LValue shl 4) +
    (LValue shl 7) + (LValue shl 8) + (LValue shl 24)) and
    Cardinal($FFFFFFFF);
end;
{$POP}

function BytesHash(const ABytes: TWfcMusicAudioBytes): Cardinal;
var
  I: Integer;
begin
  Result := FNV_OFFSET_BASIS;
  for I := 0 to Length(ABytes) - 1 do
    HashByte(Result, ABytes[I]);
end;

function SamplesHash(const AClip: TWfcMusicPcm16Clip): Cardinal;
var
  I: Integer;
  LValue: Integer;
begin
  Result := FNV_OFFSET_BASIS;
  for I := 0 to AClip.FrameCount - 1 do
  begin
    LValue := AClip.SampleAt(I);
    if LValue < 0 then
      Inc(LValue, 65536);
    HashByte(Result, Byte(LValue and $FF));
    HashByte(Result, Byte((LValue shr 8) and $FF));
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

function HexBytes(const AHex: String): TWfcMusicAudioBytes;
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

function BytesMatch(const A, B: TWfcMusicAudioBytes): Boolean;
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

function ClipsMatch(const A, B: TWfcMusicPcm16Clip): Boolean;
var
  I: Integer;
begin
  if (A.SampleRate <> B.SampleRate) or
      (A.FrameCount <> B.FrameCount) then
    Exit(False);
  for I := 0 to A.FrameCount - 1 do
    if A.SampleAt(I) <> B.SampleAt(I) then
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

procedure SetCommonScoreParts(const AVoiceCount: Integer;
  out ATracks: TWfcMusicTracks; out AVoices: TWfcMusicVoices;
  out AMeters: TWfcMusicMeterChanges);
var
  I: Integer;
begin
  ATracks := nil;
  AVoices := nil;
  AMeters := nil;
  SetLength(ATracks, 1);
  ATracks[0] := MakeWfcMusicTrack(TWfcModelToken('preview'),
    TWfcModelToken('Preview'));
  SetLength(AVoices, AVoiceCount);
  for I := 0 to AVoiceCount - 1 do
    AVoices[I] := MakeWfcMusicVoice(0,
      TWfcModelToken('voice-' + IntToStr(I)));
  SetLength(AMeters, 1);
  AMeters[0] := MakeWfcMusicMeterChange(0, 4, 4);
end;

function BuildSingleSpanScore(const ATicksPerQuarter,
  AStepsPerOctave, ALengthTicks, ATempo: Integer;
  const ATones: TWfcMusicTones): TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetCommonScoreParts(1, LTracks, LVoices, LMeters);
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, ATempo);
  SetLength(LSpans, 1);
  if Length(ATones) = 0 then
    LSpans[0] := MakeWfcMusicRest(0, 0, ALengthTicks)
  else
    LSpans[0] := MakeWfcMusicSound(0, 0, ALengthTicks, ATones);
  Result := TWfcMusicScore.Create(ATicksPerQuarter,
    AStepsPerOctave, ALengthTicks, LTracks, LVoices,
    LMeters, LTempos, LSpans);
end;

function BuildGoldenScore: TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetCommonScoreParts(2, LTracks, LVoices, LMeters);
  SetLength(LTempos, 2);
  LTempos[0] := MakeWfcMusicTempoChange(0, 500000);
  LTempos[1] := MakeWfcMusicTempoChange(960, 250000);
  SetLength(LSpans, 6);
  LSpans[0] := MakeWfcMusicSound(0, 0, 480,
    TonesOf([MakeWfcMusicTone(60, 100),
      MakeWfcMusicTone(64, 80)]));
  LSpans[1] := MakeWfcMusicRest(0, 480, 480);
  LSpans[2] := MakeWfcMusicSound(0, 960, 960,
    TonesOf([MakeWfcMusicTone(67, 90)]));
  LSpans[3] := MakeWfcMusicRest(1, 0, 720);
  LSpans[4] := MakeWfcMusicSound(1, 720, 720,
    TonesOf([MakeWfcMusicTone(48, 70)]));
  LSpans[5] := MakeWfcMusicRest(1, 1440, 480);
  Result := TWfcMusicScore.Create(480, 12, 1920,
    LTracks, LVoices, LMeters, LTempos, LSpans);
end;

function BuildRetriggerScore(const ARetrigger: Boolean): TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetCommonScoreParts(1, LTracks, LVoices, LMeters);
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, 500000);
  if ARetrigger then
  begin
    SetLength(LSpans, 2);
    LSpans[0] := MakeWfcMusicSound(0, 0, 2,
      TonesOf([MakeWfcMusicTone(60, 127)]));
    LSpans[1] := MakeWfcMusicSound(0, 2, 2,
      TonesOf([MakeWfcMusicTone(60, 127)]));
  end
  else
  begin
    SetLength(LSpans, 1);
    LSpans[0] := MakeWfcMusicSound(0, 0, 4,
      TonesOf([MakeWfcMusicTone(60, 127)]));
  end;
  Result := TWfcMusicScore.Create(1, 12, 4,
    LTracks, LVoices, LMeters, LTempos, LSpans);
end;

function BuildTempoPartitionScore(
  const APartitioned: Boolean): TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetCommonScoreParts(1, LTracks, LVoices, LMeters);
  if APartitioned then
  begin
    SetLength(LTempos, 4);
    LTempos[0] := MakeWfcMusicTempoChange(0, 500003);
    LTempos[1] := MakeWfcMusicTempoChange(1, 500003);
    LTempos[2] := MakeWfcMusicTempoChange(3, 500003);
    LTempos[3] := MakeWfcMusicTempoChange(13, 500003);
  end
  else
  begin
    SetLength(LTempos, 1);
    LTempos[0] := MakeWfcMusicTempoChange(0, 500003);
  end;
  SetLength(LSpans, 1);
  LSpans[0] := MakeWfcMusicSound(0, 0, 28,
    TonesOf([MakeWfcMusicTone(57, 100)]));
  Result := TWfcMusicScore.Create(7, 12, 28,
    LTracks, LVoices, LMeters, LTempos, LSpans);
end;

function BuildFractionalFrameScore: TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetCommonScoreParts(1, LTracks, LVoices, LMeters);
  SetLength(LTempos, 2);
  LTempos[0] := MakeWfcMusicTempoChange(0, 6);
  LTempos[1] := MakeWfcMusicTempoChange(1, 8);
  SetLength(LSpans, 1);
  LSpans[0] := MakeWfcMusicSound(0, 0, 12,
    TonesOf([MakeWfcMusicTone(60, 100)]));
  Result := TWfcMusicScore.Create(3, 12, 12,
    LTracks, LVoices, LMeters, LTempos, LSpans);
end;

function BuildFractionalOverLimitScore: TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetCommonScoreParts(1, LTracks, LVoices, LMeters);
  SetLength(LTempos, 2);
  LTempos[0] := MakeWfcMusicTempoChange(0, 500001);
  LTempos[1] := MakeWfcMusicTempoChange(1, 500000);
  SetLength(LSpans, 1);
  LSpans[0] := MakeWfcMusicRest(0, 0, 360);
  Result := TWfcMusicScore.Create(3, 12, 360,
    LTracks, LVoices, LMeters, LTempos, LSpans);
end;

function BuildZeroFrameNoteScore: TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetCommonScoreParts(1, LTracks, LVoices, LMeters);
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, 10000);
  SetLength(LSpans, 2);
  LSpans[0] := MakeWfcMusicSound(0, 0, 1,
    TonesOf([MakeWfcMusicTone(60, 100)]));
  LSpans[1] := MakeWfcMusicRest(0, 1, 399999);
  Result := TWfcMusicScore.Create(100000, 12, 400000,
    LTracks, LVoices, LMeters, LTempos, LSpans);
end;

function BuildManyVoiceScore(const AVoiceCount,
  ALengthTicks, ATempo: Integer; const ASounding: Boolean): TWfcMusicScore;
var
  I: Integer;
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetCommonScoreParts(AVoiceCount, LTracks, LVoices, LMeters);
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, ATempo);
  SetLength(LSpans, AVoiceCount);
  for I := 0 to AVoiceCount - 1 do
    if ASounding then
      LSpans[I] := MakeWfcMusicSound(I, 0, ALengthTicks,
        TonesOf([MakeWfcMusicTone(60 + I, 100)]))
    else
      LSpans[I] := MakeWfcMusicRest(I, 0, ALengthTicks);
  Result := TWfcMusicScore.Create(1, 12, ALengthTicks,
    LTracks, LVoices, LMeters, LTempos, LSpans);
end;

function BuildManyTrackScore(const ATrackCount: Integer): TWfcMusicScore;
var
  I: Integer;
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  LTracks := nil;
  SetLength(LTracks, ATrackCount);
  for I := 0 to ATrackCount - 1 do
    LTracks[I] := MakeWfcMusicTrack(
      TWfcModelToken('track-' + IntToStr(I)), '');
  LVoices := nil;
  SetLength(LVoices, 1);
  LVoices[0] := MakeWfcMusicVoice(0, 'voice');
  LMeters := nil;
  SetLength(LMeters, 1);
  LMeters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  LTempos := nil;
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, 500000);
  LSpans := nil;
  SetLength(LSpans, 1);
  LSpans[0] := MakeWfcMusicRest(0, 0, 4);
  Result := TWfcMusicScore.Create(1, 12, 4,
    LTracks, LVoices, LMeters, LTempos, LSpans);
end;

procedure CheckRenderRejected(const AScore: TWfcMusicScore;
  const AOptions: TWfcMusicAudioOptions; const AMessage: String);
var
  LClip: TWfcMusicPcm16Clip;
  LRaised: Boolean;
begin
  LClip := nil;
  LRaised := False;
  try
    LClip := RenderWfcMusicAudio(AScore, AOptions);
  except
    on E: EWfcMusicAudio do
      LRaised := True;
  end;
  LClip.Free;
  Check(LRaised, AMessage);
end;

procedure TestExactWaveEncoding;
var
  LBytes: TWfcMusicAudioBytes;
  LClip: TWfcMusicPcm16Clip;
  LCopy: TWfcMusicPcm16Samples;
  LSamples: TWfcMusicPcm16Samples;
begin
  LSamples := nil;
  SetLength(LSamples, 5);
  LSamples[0] := Low(SmallInt);
  LSamples[1] := -1;
  LSamples[2] := 0;
  LSamples[3] := 1;
  LSamples[4] := High(SmallInt);
  LClip := TWfcMusicPcm16Clip.Create(44100, LSamples);
  try
    LSamples[0] := 123;
    LCopy := LClip.CopySamples;
    LCopy[4] := 123;
    Check((LClip.FrameCount = 5) and
      (LClip.SampleAt(0) = Low(SmallInt)) and
      (LClip.SampleAt(4) = High(SmallInt)),
      'the PCM clip deeply owns both input and returned samples');
    LBytes := EncodeWfcMusicWave(LClip);
    Check(BytesMatch(LBytes, HexBytes(GOLDEN_BOUNDARY_WAVE)),
      'PCM boundaries encode as the exact canonical 54-byte RIFF/WAVE');
  finally
    LClip.Free;
  end;

  LSamples := nil;
  LClip := TWfcMusicPcm16Clip.Create(44100, LSamples);
  try
    Check(BytesMatch(EncodeWfcMusicWave(LClip),
      HexBytes(GOLDEN_EMPTY_WAVE)),
      'an empty clip encodes as a complete 44-byte RIFF/WAVE');
  finally
    LClip.Free;
  end;
end;

procedure TestGoldenRender;
var
  I: Integer;
  LClip: TWfcMusicPcm16Clip;
  LOptions: TWfcMusicAudioOptions;
  LPcmHash: Cardinal;
  LRepeat: TWfcMusicPcm16Clip;
  LScore: TWfcMusicScore;
  LWaveHash: Cardinal;
  LZeroCount: Integer;
begin
  LScore := BuildGoldenScore;
  LOptions := DefaultWfcMusicAudioOptions;
  LOptions.SampleRate := 32000;
  LClip := nil;
  LRepeat := nil;
  try
    LClip := RenderWfcMusicAudio(LScore, LOptions);
    LRepeat := RenderWfcMusicAudio(LScore, LOptions);
    Check((LClip.SampleRate = 32000) and
      (LClip.FrameCount = 48000),
      'two tempos map the score to exactly 48,000 mono frames');
    Check(ClipsMatch(LClip, LRepeat),
      'repeated fixed-point renders are sample-for-sample identical');
    Check(LClip.SampleAt(20000) = 0,
      'overlapping voice rests preserve their silent interval');
    Check((LClip.SampleAt(0) = 0) and
      (LClip.SampleAt(LClip.FrameCount - 1) = 0),
      'the fixed-point envelope starts and ends the preview at zero');
    LZeroCount := 0;
    for I := 0 to LClip.FrameCount - 1 do
      if LClip.SampleAt(I) = 0 then
        Inc(LZeroCount);
    Check((LZeroCount > 8000) and (LZeroCount < LClip.FrameCount),
      'the fixture contains both synthesized audio and rest silence');
    LPcmHash := SamplesHash(LClip);
    LWaveHash := BytesHash(EncodeWfcMusicWave(LClip));
    WriteLn('  [INFO] golden PCM hash: ', IntToHex(LPcmHash, 8));
    WriteLn('  [INFO] golden WAVE hash: ', IntToHex(LWaveHash, 8));
    Check(LPcmHash = EXPECTED_GOLDEN_PCM_HASH,
      'the exact preview PCM matches its portable golden hash');
    Check(LWaveHash = EXPECTED_GOLDEN_WAVE_HASH,
      'the exact RIFF/WAVE matches its portable golden hash');
  finally
    LRepeat.Free;
    LClip.Free;
    LScore.Free;
  end;
end;

procedure TestTimingAndRetriggerSemantics;
var
  I: Integer;
  LContinuous: TWfcMusicPcm16Clip;
  LOptions: TWfcMusicAudioOptions;
  LPartitioned: TWfcMusicPcm16Clip;
  LRemainderScore: TWfcMusicScore;
  LRemainderClip: TWfcMusicPcm16Clip;
  LRetriggered: TWfcMusicPcm16Clip;
  LScoreA: TWfcMusicScore;
  LScoreB: TWfcMusicScore;
  LZeroClip: TWfcMusicPcm16Clip;
begin
  LOptions := DefaultWfcMusicAudioOptions;
  LOptions.SampleRate := 32000;
  LOptions.MasterVolume := 127;
  LOptions.AttackMilliseconds := 0;
  LOptions.ReleaseMilliseconds := 0;

  LScoreA := BuildTempoPartitionScore(False);
  LScoreB := BuildTempoPartitionScore(True);
  LContinuous := nil;
  LPartitioned := nil;
  try
    LContinuous := RenderWfcMusicAudio(LScoreA, LOptions);
    LPartitioned := RenderWfcMusicAudio(LScoreB, LOptions);
    Check(ClipsMatch(LContinuous, LPartitioned),
      'equal-tempo partitions retain fractional time without seams');
  finally
    LPartitioned.Free;
    LContinuous.Free;
    LScoreB.Free;
    LScoreA.Free;
  end;

  LScoreA := BuildRetriggerScore(False);
  LScoreB := BuildRetriggerScore(True);
  LContinuous := nil;
  LRetriggered := nil;
  try
    LContinuous := RenderWfcMusicAudio(LScoreA, LOptions);
    LRetriggered := RenderWfcMusicAudio(LScoreB, LOptions);
    Check((LRetriggered.SampleAt(32000) = 0) and
      (LContinuous.SampleAt(32000) <> 0),
      'a new span retriggers phase while one held span remains continuous');
  finally
    LRetriggered.Free;
    LContinuous.Free;
    LScoreB.Free;
    LScoreA.Free;
  end;

  LRemainderScore := BuildFractionalFrameScore;
  LRemainderClip := nil;
  try
    LRemainderClip := RenderWfcMusicAudio(LRemainderScore, LOptions);
    Check(LRemainderClip.FrameCount = 1,
      'fractional microseconds survive through final frame quantization');
  finally
    LRemainderClip.Free;
    LRemainderScore.Free;
  end;

  LScoreA := BuildZeroFrameNoteScore;
  LZeroClip := nil;
  try
    LZeroClip := RenderWfcMusicAudio(LScoreA, LOptions);
    Check(LZeroClip.FrameCount = 1280,
      'the containing score retains its exact nonzero duration');
    for I := 0 to LZeroClip.FrameCount - 1 do
      if LZeroClip.SampleAt(I) <> 0 then
      begin
        Check(False, 'a note quantized to zero frames emits no sample');
        Exit;
      end;
    Check(True, 'a note quantized to zero frames emits no sample');
  finally
    LZeroClip.Free;
    LScoreA.Free;
  end;
end;

procedure TestPitchAndOptionBoundaries;
var
  LClip: TWfcMusicPcm16Clip;
  LOptions: TWfcMusicAudioOptions;
  LScore: TWfcMusicScore;
begin
  LOptions := DefaultWfcMusicAudioOptions;
  LOptions.SampleRate := 32000;
  LOptions.AttackMilliseconds := 0;
  LOptions.ReleaseMilliseconds := 0;

  LScore := BuildSingleSpanScore(1, 12, 4, 500000,
    TonesOf([MakeWfcMusicTone(0, 100),
      MakeWfcMusicTone(127, 100)]));
  LClip := nil;
  try
    LClip := RenderWfcMusicAudio(LScore, LOptions);
    Check((LClip.FrameCount = 64000) and
      (SamplesHash(LClip) <> FNV_OFFSET_BASIS),
      'the inclusive MIDI pitch boundary 0..127 renders');
  finally
    LClip.Free;
    LScore.Free;
  end;

  LScore := BuildSingleSpanScore(1, 12, 4, 500000,
    TonesOf([MakeWfcMusicTone(128, 100)]));
  try
    CheckRenderRejected(LScore, LOptions,
      'pitch 128 is rejected at the preview adapter boundary');
  finally
    LScore.Free;
  end;

  LScore := BuildSingleSpanScore(1, 11, 4, 500000, nil);
  try
    CheckRenderRejected(LScore, LOptions,
      'non-twelve-step scores are not silently remapped');
  finally
    LScore.Free;
  end;

  LScore := BuildSingleSpanScore(1, 12, 4, 500000, nil);
  try
    LOptions.SampleRate := 31999;
    CheckRenderRejected(LScore, LOptions,
      'sample rates below the versioned boundary are rejected');
    LOptions := DefaultWfcMusicAudioOptions;
    LOptions.MasterVolume := 128;
    CheckRenderRejected(LScore, LOptions,
      'master volume above 127 is rejected');
    LOptions := DefaultWfcMusicAudioOptions;
    LOptions.AttackMilliseconds := 1001;
    CheckRenderRejected(LScore, LOptions,
      'attack beyond the envelope limit is rejected');
    LOptions := DefaultWfcMusicAudioOptions;
    LOptions.ReleaseMilliseconds := -1;
    CheckRenderRejected(LScore, LOptions,
      'negative release is rejected');
  finally
    LScore.Free;
  end;
end;

procedure TestResourceGuards;
var
  I: Integer;
  LOptions: TWfcMusicAudioOptions;
  LScore: TWfcMusicScore;
  LTones: TWfcMusicTones;
begin
  LOptions := DefaultWfcMusicAudioOptions;
  LOptions.SampleRate := 32000;
  CheckRenderRejected(nil, LOptions, 'a nil score is rejected');

  LScore := BuildManyTrackScore(33);
  try
    CheckRenderRejected(LScore, LOptions,
      'more than 32 source tracks are rejected before rendering');
  finally
    LScore.Free;
  end;

  LScore := BuildManyVoiceScore(33, 4, 500000, False);
  try
    CheckRenderRejected(LScore, LOptions,
      'more than 32 voices are rejected before rendering');
  finally
    LScore.Free;
  end;

  LTones := nil;
  SetLength(LTones, 17);
  for I := 0 to Length(LTones) - 1 do
    LTones[I] := MakeWfcMusicTone(40 + I, 100);
  LScore := BuildSingleSpanScore(1, 12, 4, 500000, LTones);
  try
    CheckRenderRejected(LScore, LOptions,
      'more than 16 simultaneous tones are rejected before PCM allocation');
  finally
    LScore.Free;
  end;

  LScore := BuildSingleSpanScore(1, 12, 4, 4000001, nil);
  try
    CheckRenderRejected(LScore, LOptions,
      'tempo above four seconds per quarter is rejected');
  finally
    LScore.Free;
  end;

  LScore := BuildSingleSpanScore(1, 12, 128, 500000,
    TonesOf([MakeWfcMusicTone(60, 100)]));
  try
    CheckRenderRejected(LScore, LOptions,
      'duration above 60 seconds is rejected before PCM allocation');
  finally
    LScore.Free;
  end;

  LScore := BuildFractionalOverLimitScore;
  try
    CheckRenderRejected(LScore, LOptions,
      'a fractional microsecond beyond 60 seconds is rejected');
  finally
    LScore.Free;
  end;

  LScore := BuildManyVoiceScore(6, 120, 500000, True);
  LOptions.SampleRate := 48000;
  try
    CheckRenderRejected(LScore, LOptions,
      'tone-frame work above the visit budget is rejected before allocation');
  finally
    LScore.Free;
  end;

  LScore := BuildSingleSpanScore(1000000, 12, 4000000,
    1, nil);
  LOptions.SampleRate := 32000;
  try
    CheckRenderRejected(LScore, LOptions,
      'a whole score quantized to zero frames is rejected');
  finally
    LScore.Free;
  end;

  CheckRenderRejected(nil, DefaultWfcMusicAudioOptions,
    'validation remains deterministic after resource failures');
end;

procedure TestWaveAndVersionGuards;
var
  LBytes: TWfcMusicAudioBytes;
  LRaised: Boolean;
begin
  LBytes := nil;
  LRaised := False;
  try
    LBytes := EncodeWfcMusicWave(nil);
  except
    on E: EWfcMusicAudio do
      LRaised := True;
  end;
  if Length(LBytes) = High(Integer) then
    WriteLn('unreachable');
  Check(LRaised, 'a nil PCM clip cannot be encoded');
  Check((WFC_MUSIC_AUDIO_VERSION = 1) and
    (WFC_MUSIC_WAVE_VERSION = 1) and
    (WFC_MUSIC_AUDIO_CHANNEL_COUNT = 1) and
    (WFC_MUSIC_AUDIO_BITS_PER_SAMPLE = 16),
    'preview synthesis and its mono PCM16 artifact are versioned');
  Check(WFC_MUSIC_AUDIO_MAX_WAVE_BYTE_COUNT = 5760044,
    'the encoded artifact limit exactly follows the frame budget');
end;

begin
  WriteLn('WFC music audio conformance suite');
  WriteLn('=================================');
  RunTest('exact PCM16 RIFF/WAVE encoding', @TestExactWaveEncoding);
  RunTest('multi-voice golden rendering', @TestGoldenRender);
  RunTest('timing and retrigger semantics',
    @TestTimingAndRetriggerSemantics);
  RunTest('pitch and option boundaries', @TestPitchAndOptionBoundaries);
  RunTest('source and output resource guards', @TestResourceGuards);
  RunTest('WAVE and version guards', @TestWaveAndVersionGuards);
  WriteLn('=================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d music audio checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
