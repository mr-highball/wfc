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
program wfc_music_ensemble_audio_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc_model, wfc_music, wfc_music_sequence, wfc_music_ensemble,
  wfc_music_audio, wfc_music_audio_stream, wfc_music_ensemble_audio;

type
  TTestProcedure = procedure;
  EExpectedSinkFailure = class(Exception);
  TByteSink = class(TWfcMusicAudioByteSink)
  public
    Bytes: TWfcMusicAudioBytes;
    Calls, MaxBlock, FailOnCall: Integer;
    Reenter: TWfcMusicWaveStream;
    procedure WriteBytes(const ABytes: array of Byte); override;
  end;

var Checks, Failures: Integer;

procedure Check(const ACondition: Boolean; const ALabel: String);
begin
  Inc(Checks);
  if not ACondition then begin Inc(Failures); WriteLn('[FAIL] ', ALabel); end;
end;

procedure Run(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try ATest;
  except on E: Exception do
    begin Inc(Failures); WriteLn('[EXCEPTION] ', E.ClassName, ': ', E.Message); end;
  end;
end;

procedure TByteSink.WriteBytes(const ABytes: array of Byte);
var I, LOffset: Integer;
begin
  Inc(Calls);
  if Length(ABytes) > MaxBlock then MaxBlock := Length(ABytes);
  if Calls = FailOnCall then raise EExpectedSinkFailure.Create('original sink fault');
  if Reenter <> nil then Reenter.AppendSamples([]);
  LOffset := Length(Bytes);
  SetLength(Bytes, LOffset + Length(ABytes));
  for I := 0 to High(ABytes) do Bytes[LOffset + I] := ABytes[I];
end;

function Capacities(const AValues: array of Integer): TWfcMusicEnsembleAudioVoiceCapacities;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

function Voice(const AAction: TWfcMusicCellAction; const APitches,
  AVelocities: array of Integer): TWfcMusicVoiceCell;
var LValues: TWfcMusicTones; I: Integer;
begin
  if Length(APitches) <> Length(AVelocities) then raise Exception.Create('fixture arity');
  SetLength(LValues, Length(APitches));
  for I := 0 to High(APitches) do LValues[I] := MakeWfcMusicTone(APitches[I], AVelocities[I]);
  Result := MakeWfcMusicVoiceCell(AAction, LValues);
end;

function Frame(const AVoices: array of TWfcMusicVoiceCell): TWfcMusicEnsembleFrame;
var LVoices: TWfcMusicVoiceCells; I: Integer;
begin
  SetLength(LVoices, Length(AVoices));
  for I := 0 to High(AVoices) do LVoices[I] := AVoices[I];
  Result := MakeWfcMusicEnsembleFrame(LVoices);
end;

function SameSamples(const A, B: TWfcMusicPcm16Samples): Boolean;
var I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do if A[I] <> B[I] then Exit(False);
  Result := True;
end;

function SameBytes(const A, B: TWfcMusicAudioBytes): Boolean;
var I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do if A[I] <> B[I] then Exit(False);
  Result := True;
end;

procedure Append(var ATarget: TWfcMusicPcm16Samples; const AValues: TWfcMusicPcm16Samples);
var I, LOffset: Integer;
begin
  LOffset := Length(ATarget);
  SetLength(ATarget, LOffset + Length(AValues));
  for I := 0 to High(AValues) do ATarget[LOffset + I] := AValues[I];
end;

procedure Drain(const ARenderer: TWfcMusicEnsembleAudioRenderer;
  const ABlockSize: Integer; var AResult: TWfcMusicPcm16Samples);
var LBlock: TWfcMusicPcm16Samples;
begin
  while ARenderer.ReadSamples(ABlockSize, LBlock) do
  begin
    if (Length(LBlock) < 1) or (Length(LBlock) > ABlockSize) or
      (Length(LBlock) > WFC_MUSIC_ENSEMBLE_AUDIO_BLOCK_FRAMES) then
      raise Exception.Create('invalid output block bound');
    Append(AResult, LBlock);
  end;
  if Length(LBlock) <> 0 then raise Exception.Create('False must return nil');
end;

function RenderFrames(const AFrames: TWfcMusicEnsembleFrames;
  const ALengths, ATempos: array of Integer; const AOptions: TWfcMusicAudioOptions;
  const ATPQ, ABlockSize: Integer;
  const ACapacities: TWfcMusicEnsembleAudioVoiceCapacities): TWfcMusicPcm16Samples;
var LRenderer: TWfcMusicEnsembleAudioRenderer; I: Integer;
begin
  Result := nil;
  LRenderer := TWfcMusicEnsembleAudioRenderer.Create(AOptions, ATPQ, ACapacities);
  try
    for I := 0 to High(AFrames) do
    begin
      if not LRenderer.NeedsInput then raise Exception.Create('drain did not reach input boundary');
      LRenderer.AdmitFrame(AFrames[I], ALengths[I], ATempos[I]);
      Drain(LRenderer, ABlockSize, Result);
    end;
    LRenderer.EndInput;
    Drain(LRenderer, ABlockSize, Result);
    Check(LRenderer.Finished and LRenderer.InputEnded and not LRenderer.NeedsInput,
      'finite rendering reaches a terminal exact end');
    Check((LRenderer.FrameCount = Length(Result)) and
      (LRenderer.EmittedFrames = Length(Result)) and
      (LRenderer.RenderedFrames = Length(Result)), 'all frame counters agree at end');
    LRenderer.EndInput;
  finally LRenderer.Free; end;
end;

function ScoreTemplate(const ATPQ, ALength, AVoices: Integer;
  const ATempos: TWfcMusicTempoChanges): TWfcMusicScore;
var LTracks: TWfcMusicTracks; LVoices: TWfcMusicVoices;
  LMeters: TWfcMusicMeterChanges; LSpans: TWfcMusicSpanEvents; I: Integer;
begin
  SetLength(LTracks, 1); LTracks[0] := MakeWfcMusicTrack('pcm', 'PCM fixture');
  SetLength(LVoices, AVoices); SetLength(LSpans, AVoices);
  for I := 0 to AVoices - 1 do
  begin
    LVoices[I] := MakeWfcMusicVoice(0, TWfcModelToken('voice' + IntToStr(I)));
    LSpans[I] := MakeWfcMusicRest(I, 0, ALength);
  end;
  SetLength(LMeters, 1); LMeters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  Result := TWfcMusicScore.Create(ATPQ, 12, ALength, LTracks, LVoices, LMeters, ATempos, LSpans);
end;

procedure TestPreviewParity;
const RATES: array[0..2] of Integer = (32000, 44100, 48000);
var
  LFrames: TWfcMusicEnsembleFrames; LTempos: TWfcMusicTempoChanges;
  LLengths, LTempoValues: array of Integer; LOptions: TWfcMusicAudioOptions;
  LTemplate, LScore: TWfcMusicScore; LClip: TWfcMusicPcm16Clip;
  LActual, LOther: TWfcMusicPcm16Samples; I, J, K: Integer;
begin
  SetLength(LFrames, 16); SetLength(LLengths, 16); SetLength(LTempoValues, 16);
  for I := 0 to 15 do
  begin
    if I = 0 then
      LFrames[I] := Frame([Voice(wmcaAttack, [0, 43], [127, 81]),
        Voice(wmcaAttack, [60, 64, 127], [70, 91, 17]), MakeWfcMusicRestVoiceCell])
    else
      LFrames[I] := Frame([Voice(wmcaHold, [0, 43], [127, 81]),
        Voice(wmcaAttack, [60, 64, 127], [70, 91, 17]), MakeWfcMusicRestVoiceCell]);
    if I mod 4 = 2 then LFrames[I].Voices[1] := MakeWfcMusicRestVoiceCell;
    if I mod 4 = 1 then LFrames[I].Voices[1].Action := wmcaHold;
    LLengths[I] := 1;
    if I < 7 then LTempoValues[I] := 17003 else LTempoValues[I] := 25009;
  end;
  SetLength(LTempos, 2);
  LTempos[0] := MakeWfcMusicTempoChange(0, LTempoValues[0]);
  LTempos[1] := MakeWfcMusicTempoChange(7, LTempoValues[7]);
  LTemplate := ScoreTemplate(4, 16, 3, LTempos);
  LScore := nil;
  try
    LScore := RebuildWfcMusicEnsembleScore(LFrames, 1, LTemplate);
    for J := 0 to High(RATES) do
      for K := 0 to 3 do
      begin
        LOptions := DefaultWfcMusicAudioOptions;
        LOptions.SampleRate := RATES[J];
        case K of
          0: begin LOptions.AttackMilliseconds := 0; LOptions.ReleaseMilliseconds := 0; end;
          1: begin LOptions.AttackMilliseconds := 5; LOptions.ReleaseMilliseconds := 20; end;
          2: begin LOptions.AttackMilliseconds := 1000; LOptions.ReleaseMilliseconds := 1000; end;
          3: begin LOptions.MasterVolume := 0; LOptions.AttackMilliseconds := 1; LOptions.ReleaseMilliseconds := 1; end;
        end;
        LClip := RenderWfcMusicAudio(LScore, LOptions);
        try
          LActual := RenderFrames(LFrames, LLengths, LTempoValues, LOptions, 4,
            31, Capacities([2, 3, 0]));
          Check(SameSamples(LActual, LClip.CopySamples),
            'preview byte parity for rate/envelope case ' + IntToStr(J) + '/' + IntToStr(K));
          LOther := RenderFrames(LFrames, LLengths, LTempoValues, LOptions, 4,
            High(Integer), Capacities([2, 3, 0]));
          Check(SameSamples(LActual, LOther), 'pull partition does not change PCM');
        finally LClip.Free; end;
      end;
  finally LScore.Free; LTemplate.Free; end;
end;

procedure TestSeamsAndDetachment;
var LOptions: TWfcMusicAudioOptions; LFrames: TWfcMusicEnsembleFrames;
  LWhole, LSplit, LReattack, LDetached: TWfcMusicPcm16Samples;
  LRenderer: TWfcMusicEnsembleAudioRenderer; LFrame: TWfcMusicEnsembleFrame;
  LCaps: TWfcMusicEnsembleAudioVoiceCapacities; LBlock: TWfcMusicPcm16Samples;
begin
  LOptions := DefaultWfcMusicAudioOptions;
  LOptions.SampleRate := 32000;
  SetLength(LFrames, 1);
  LFrames[0] := Frame([Voice(wmcaAttack, [48, 55], [127, 83])]);
  LWhole := RenderFrames(LFrames, [28], [500003], LOptions, 7, 2048, Capacities([2]));
  SetLength(LFrames, 4);
  LFrames[1] := Frame([Voice(wmcaHold, [48, 55], [127, 83])]);
  LFrames[2] := MakeWfcMusicEnsembleFrame(LFrames[1].Voices);
  LFrames[3] := MakeWfcMusicEnsembleFrame(LFrames[1].Voices);
  LSplit := RenderFrames(LFrames, [1, 2, 10, 15], [500003, 500003, 500003, 500003],
    LOptions, 7, 137, Capacities([2]));
  Check(SameSamples(LWhole, LSplit), 'unequal held segments preserve phase, envelopes and fractions');
  LFrames[2].Voices[0].Action := wmcaAttack;
  LReattack := RenderFrames(LFrames, [1, 2, 10, 15], [500003, 500003, 500003, 500003],
    LOptions, 7, 1, Capacities([2]));
  Check(not SameSamples(LWhole, LReattack), 'identical pitches explicitly attacked retrigger');

  LCaps := Capacities([2]);
  LRenderer := TWfcMusicEnsembleAudioRenderer.Create(LOptions, 7, LCaps);
  try
    LCaps[0] := 0;
    LFrame := MakeWfcMusicEnsembleFrame(LFrames[0].Voices);
    LRenderer.AdmitFrame(LFrame, 28, 500003);
    LFrame.Voices[0].Tones[0].Pitch := 127;
    LFrame.Voices[0].Tones[0].Velocity := 1;
    LDetached := nil;
    Drain(LRenderer, 83, LDetached);
    LRenderer.EndInput;
    Drain(LRenderer, 29, LDetached);
    Check(SameSamples(LDetached, LWhole) and (LRenderer.Headroom = 2),
      'admitted tones and constructor capacities are detached from caller arrays');
    SetLength(LBlock, 1); LBlock[0] := 9;
    Check(not LRenderer.ReadSamples(1, LBlock) and (Length(LBlock) = 0),
      'read after completion returns False and nil');
  finally LRenderer.Free; end;
end;

procedure TestExactClock;
var A, B, Saved: TWfcMusicEnsembleAudioClock;
  ExactNumerator: TWfcMusicEnsembleAudioCount; I, N, Tempo: Integer; Rejected: Boolean;
begin
  A := Default(TWfcMusicEnsembleAudioClock);
  AdvanceWfcMusicEnsembleAudioClock(A, 0, 500000, 7, 44100);
  Check((A.TickCount = 0) and (A.FrameCount = 0) and (A.FractionNumerator = 0),
    'zero duration is a normalized validating no-op');
  ExactNumerator := 0;
  for I := 1 to 97 do
  begin
    N := 1 + I mod 5; Tempo := 6 + I * 7919;
    AdvanceWfcMusicEnsembleAudioClock(A, N, Tempo, 7, 44100);
    ExactNumerator := ExactNumerator + TWfcMusicEnsembleAudioCount(N) * Tempo * 44100;
    Check((A.FrameCount = ExactNumerator div 7000000) and
      (A.FractionNumerator = ExactNumerator mod 7000000),
      'independent rational clock reference ' + IntToStr(I));
  end;
  A := Default(TWfcMusicEnsembleAudioClock);
  B := A;
  AdvanceWfcMusicEnsembleAudioClock(A, 999999, 500003, 2147483647, 48000);
  for I := 1 to 3 do AdvanceWfcMusicEnsembleAudioClock(B, 333333, 500003, 2147483647, 48000);
  Check((A.TickCount = B.TickCount) and (A.FrameCount = B.FrameCount) and
    (A.FractionNumerator = B.FractionNumerator), 'large denominator interval partition invariance');
  A := Default(TWfcMusicEnsembleAudioClock);
  AdvanceWfcMusicEnsembleAudioClock(A, High(Integer), 4000000, 1, 48000);
  Check(A.FrameCount = 412316860224000, 'wide duration exceeds both preview and 32-bit frame limits');
  A := Default(TWfcMusicEnsembleAudioClock);
  AdvanceWfcMusicEnsembleAudioClock(A, High(Integer), 4000000, High(Integer), 48000);
  Check((A.FrameCount = 192000) and (A.FractionNumerator = 0),
    'largest tick-tempo intermediate cancels exactly against largest TPQ');
  A.TickCount := 1; A.FrameCount := WFC_MUSIC_ENSEMBLE_AUDIO_MAX_SAFE_INTEGER - 192000;
  A.FractionNumerator := 0;
  AdvanceWfcMusicEnsembleAudioClock(A, 1, 4000000, 1, 48000);
  Check(A.FrameCount = WFC_MUSIC_ENSEMBLE_AUDIO_MAX_SAFE_INTEGER, 'last exact frame is accepted');
  Saved := A; Rejected := False;
  try AdvanceWfcMusicEnsembleAudioClock(A, 1, 4000000, 1, 48000);
  except on EWfcMusicEnsembleAudio do Rejected := True; end;
  Check(Rejected and (A.TickCount = Saved.TickCount) and (A.FrameCount = Saved.FrameCount),
    'frame overflow rejects without partially advancing clock');
  A.TickCount := WFC_MUSIC_ENSEMBLE_AUDIO_MAX_SAFE_INTEGER; A.FrameCount := 0;
  Saved := A; Rejected := False;
  try AdvanceWfcMusicEnsembleAudioClock(A, 1, 1, 1, 32000);
  except on EWfcMusicEnsembleAudio do Rejected := True; end;
  Check(Rejected and (A.TickCount = Saved.TickCount), 'tick overflow rejects before clock mutation');
  for I := 0 to 7 do
  begin
    A := Default(TWfcMusicEnsembleAudioClock); Rejected := False;
    try
      case I of
        0: AdvanceWfcMusicEnsembleAudioClock(A, -1, 500000, 1, 44100);
        1: AdvanceWfcMusicEnsembleAudioClock(A, 1, 0, 1, 44100);
        2: AdvanceWfcMusicEnsembleAudioClock(A, 1, 4000001, 1, 44100);
        3: AdvanceWfcMusicEnsembleAudioClock(A, 1, 500000, 0, 44100);
        4: AdvanceWfcMusicEnsembleAudioClock(A, 1, 500000, 1, 31999);
        5: begin A.FractionNumerator := 1000000; AdvanceWfcMusicEnsembleAudioClock(A, 0, 1, 1, 44100); end;
        6: begin A.FrameCount := 1; AdvanceWfcMusicEnsembleAudioClock(A, 0, 1, 1, 44100); end;
        7: begin A.TickCount := -1; AdvanceWfcMusicEnsembleAudioClock(A, 0, 1, 1, 44100); end;
      end;
    except on EWfcMusicEnsembleAudio do Rejected := True; end;
    Check(Rejected, 'invalid clock/timing field ' + IntToStr(I));
  end;
end;

procedure TestAdmissionAndCancellation;
var LRenderer: TWfcMusicEnsembleAudioRenderer; LOptions: TWfcMusicAudioOptions;
  LFrame, LBad: TWfcMusicEnsembleFrame; LBlock, LOutput: TWfcMusicPcm16Samples;
  I, LInvalidAction: Integer; Rejected: Boolean; BeforeFrames: TWfcMusicEnsembleAudioCount;
begin
  LOptions := DefaultWfcMusicAudioOptions;
  LRenderer := TWfcMusicEnsembleAudioRenderer.Create(LOptions, 480, Capacities([2, 0]));
  try
    LFrame := Frame([Voice(wmcaAttack, [60], [90]), MakeWfcMusicRestVoiceCell]);
    LInvalidAction := 99;
    for I := 0 to 10 do
    begin
      LBad := MakeWfcMusicEnsembleFrame(LFrame.Voices);
      case I of
        0: LBad.Voices[0].Action := wmcaHold;
        1: LBad.Voices[0].Tones[0].Pitch := 128;
        2: LBad.Voices[0].Tones[0].Velocity := 0;
        3: LBad.Voices[0].Action := wmcaRest;
        4: LBad.Voices[0].Tones := nil;
        5: LBad.Voices[0] := Voice(wmcaAttack, [60, 64, 67], [90, 90, 90]);
        6: LBad.Voices[1] := Voice(wmcaAttack, [60], [90]);
        7: SetLength(LBad.Voices, 1);
        8: begin LBad.Voices[0] := Voice(wmcaAttack, [60, 64], [90, 90]); LBad.Voices[0].Tones[1].Pitch := 60; end;
        9: LBad.Voices[0].Action := TWfcMusicCellAction(LInvalidAction);
        10: LBad.Voices[0].Tones[0].Pitch := -1;
      end;
      Rejected := False;
      try LRenderer.AdmitFrame(LBad, 120, 500000);
      except on EWfcMusicEnsembleAudio do Rejected := True; end;
      Check(Rejected and LRenderer.NeedsInput and (LRenderer.FrameCount = 0) and
        not LRenderer.Failed, 'malformed admission is recoverable ' + IntToStr(I));
    end;
    for I := 0 to 2 do
    begin
      Rejected := False;
      try
        case I of
          0: LRenderer.AdmitFrame(LFrame, 0, 500000);
          1: LRenderer.AdmitFrame(LFrame, 120, 4000001);
          2: LRenderer.ReadSamples(0, LBlock);
        end;
      except on EWfcMusicEnsembleAudio do Rejected := True; end;
      Check(Rejected and not LRenderer.Failed, 'invalid timing/read remains retryable ' + IntToStr(I));
    end;
    LRenderer.AdmitFrame(LFrame, 120, 500000);
    BeforeFrames := LRenderer.FrameCount;
    Check(not LRenderer.NeedsInput, 'pending input must be drained');
    Rejected := False;
    try LRenderer.AdmitFrame(LFrame, 120, 500000);
    except on EWfcMusicEnsembleAudio do Rejected := True; end;
    Check(Rejected and (LRenderer.FrameCount = BeforeFrames), 'undrained admission does not advance input');
    Rejected := False;
    try LRenderer.EndInput;
    except on EWfcMusicEnsembleAudio do Rejected := True; end;
    Check(Rejected and not LRenderer.InputEnded, 'premature EndInput is recoverable');
    LOutput := nil; Drain(LRenderer, 2048, LOutput);
    LBad := MakeWfcMusicEnsembleFrame(LFrame.Voices);
    LBad.Voices[0].Action := wmcaHold; LBad.Voices[0].Tones[0].Velocity := 89;
    Rejected := False;
    try LRenderer.AdmitFrame(LBad, 120, 500000);
    except on EWfcMusicEnsembleAudio do Rejected := True; end;
    Check(Rejected and LRenderer.NeedsInput, 'hold velocity change rejects without closing old note');
    LBad.Voices[0].Tones[0].Velocity := 90;
    LRenderer.AdmitFrame(LBad, 120, 500000);
    Check(LRenderer.ReadSamples(17, LBlock) and (Length(LBlock) = 17), 'bounded pull yields partial frame');
    LRenderer.Cancel; LRenderer.Cancel;
    Check(LRenderer.Cancelled and not LRenderer.NeedsInput and not LRenderer.Finished,
      'cancellation is terminal and distinct from successful completion');
    Check(not LRenderer.ReadSamples(100, LBlock) and (Length(LBlock) = 0),
      'cancellation discards pending PCM and never emits later');
    Rejected := False;
    try LRenderer.AdmitFrame(LFrame, 120, 500000);
    except on EWfcMusicEnsembleAudio do Rejected := True; end;
    Check(Rejected, 'cancelled renderer rejects further input');
    Rejected := False;
    try LRenderer.EndInput;
    except on EWfcMusicEnsembleAudio do Rejected := True; end;
    Check(Rejected, 'cancelled renderer cannot be finished as a complete song');
  finally LRenderer.Free; end;
end;

procedure TestZeroFramesAndWideAdmission;
var LOptions: TWfcMusicAudioOptions; LRenderer: TWfcMusicEnsembleAudioRenderer;
  LBlock, LOutput: TWfcMusicPcm16Samples; LFrame: TWfcMusicEnsembleFrame;
  I: Integer; LCaps: TWfcMusicEnsembleAudioVoiceCapacities;
begin
  LOptions := DefaultWfcMusicAudioOptions;
  LRenderer := TWfcMusicEnsembleAudioRenderer.Create(LOptions, 1000000, Capacities([1]));
  try
    LFrame := Frame([Voice(wmcaAttack, [60], [100])]);
    LRenderer.AdmitFrame(LFrame, 1, 1);
    Check(LRenderer.NeedsInput and (LRenderer.FrameCount = 0), 'zero-sample attack is still admitted');
    Check(not LRenderer.ReadSamples(1, LBlock) and (Length(LBlock) = 0), 'zero-sample interval yields nil');
    LFrame.Voices[0].Action := wmcaHold;
    LRenderer.AdmitFrame(LFrame, 1, 1);
    LRenderer.EndInput;
    Check(LRenderer.Finished and (LRenderer.EmittedFrames = 0), 'zero-frame song closes without an artificial sample');
  finally LRenderer.Free; end;
  LRenderer := TWfcMusicEnsembleAudioRenderer.Create(LOptions, 1, Capacities([1]));
  try
    LFrame.Voices[0].Action := wmcaAttack;
    LRenderer.AdmitFrame(LFrame, High(Integer), 4000000);
    Check((LRenderer.FrameCount > High(Integer)) and (LRenderer.RenderedFrames = 0),
      'huge admission is lazy and never allocates its timeline');
    Check(LRenderer.ReadSamples(High(Integer), LBlock) and (Length(LBlock) = 2048),
      'huge admission still returns one bounded block');
    Check(LRenderer.RenderedFrames = 2048 + LRenderer.LatencyFrames,
      'first pull renders only one block plus the fixed release window');
    LRenderer.Cancel;
  finally LRenderer.Free; end;

  SetLength(LCaps, 40); SetLength(LFrame.Voices, 40);
  for I := 0 to 39 do
  begin LCaps[I] := 0; LFrame.Voices[I] := MakeWfcMusicRestVoiceCell; end;
  LRenderer := TWfcMusicEnsembleAudioRenderer.Create(LOptions, 1, LCaps);
  try
    LRenderer.AdmitFrame(LFrame, 4, 1000);
    LOutput := nil; Drain(LRenderer, 2048, LOutput);
    LRenderer.EndInput; Drain(LRenderer, 2048, LOutput);
    Check(LRenderer.Headroom = 1, 'all-silent arbitrary voice count uses minimum unit headroom');
    for I := 0 to High(LOutput) do if LOutput[I] <> 0 then raise Exception.Create('silent output is not zero');
    Check(Length(LOutput) = 176, 'more than preview voice limit is valid for incremental PCM');
  finally LRenderer.Free; end;
  LRenderer := TWfcMusicEnsembleAudioRenderer.Create(LOptions, 1, Capacities([0]));
  try
    LRenderer.EndInput;
    Check(LRenderer.Finished and not LRenderer.ReadSamples(1, LBlock), 'empty input has an exact empty completion');
  finally LRenderer.Free; end;
end;

procedure TestConstructorGuards;
var I: Integer; LRenderer: TWfcMusicEnsembleAudioRenderer;
  LOptions: TWfcMusicAudioOptions; LCaps: TWfcMusicEnsembleAudioVoiceCapacities;
  LTPQ: Integer; Rejected: Boolean;
begin
  for I := 0 to 8 do
  begin
    LOptions := DefaultWfcMusicAudioOptions; LCaps := Capacities([1]); LTPQ := 1;
    case I of
      0: LCaps := nil;
      1: LCaps[0] := -1;
      2: LCaps := Capacities([High(Integer), 1]);
      3: LOptions.SampleRate := 48001;
      4: LOptions.MasterVolume := -1;
      5: LOptions.MasterVolume := 128;
      6: LOptions.AttackMilliseconds := -1;
      7: LOptions.ReleaseMilliseconds := 1001;
      8: LTPQ := 0;
    end;
    LRenderer := nil; Rejected := False;
    try LRenderer := TWfcMusicEnsembleAudioRenderer.Create(LOptions, LTPQ, LCaps);
    except on EWfcMusicEnsembleAudio do Rejected := True; end;
    LRenderer.Free;
    Check(Rejected, 'constructor argument preflight ' + IntToStr(I));
  end;
end;

procedure TestOutputOwnershipAndLargerChords;
var LOptions: TWfcMusicAudioOptions; LRenderer: TWfcMusicEnsembleAudioRenderer;
  LFrame: TWfcMusicEnsembleFrame; LBlock, LSaved, LNext, LDiscard: TWfcMusicPcm16Samples;
  I: Integer;
begin
  LOptions := DefaultWfcMusicAudioOptions;
  LOptions.ReleaseMilliseconds := 0;
  LOptions.AttackMilliseconds := 0;
  SetLength(LFrame.Voices, 1);
  LFrame.Voices[0].Action := wmcaAttack;
  SetLength(LFrame.Voices[0].Tones, 128);
  for I := 0 to 127 do LFrame.Voices[0].Tones[I] := MakeWfcMusicTone(I, 1 + I mod 127);
  LRenderer := TWfcMusicEnsembleAudioRenderer.Create(LOptions, 1, Capacities([128]));
  try
    LRenderer.AdmitFrame(LFrame, 1, 10000);
    Check(LRenderer.ReadSamples(23, LBlock) and (Length(LBlock) = 23),
      '128-tone canonical chord exceeds preview policy without arbitrary new cap');
    LSaved := nil; Append(LSaved, LBlock);
    Check(LRenderer.ReadSamples(23, LNext), 'second output block exists');
    Check(SameSamples(LBlock, LSaved), 'later reads never mutate previously returned samples');
    LBlock[0] := 12345;
    Check(LSaved[0] = 0, 'returned block is distinct from the saved first attack sample');
    LDiscard := nil; Drain(LRenderer, 2048, LDiscard);
    LRenderer.EndInput; Drain(LRenderer, 2048, LDiscard);
    Check(LRenderer.Finished and (LRenderer.EmittedFrames = 441),
      'large chord completes exact declared duration');
  finally LRenderer.Free; end;
end;

{$IFDEF PAS2JS}
function MalformedNumber(const AIndex: Integer): NativeInt;
begin
  asm
    if (AIndex === 0) Result = 0.5;
    else if (AIndex === 1) Result = NaN;
    else if (AIndex === 2) Result = Infinity;
    else Result = 9007199254740992;
  end;
end;

procedure TestHostNumericValidation;
var LClock: TWfcMusicEnsembleAudioClock; LOptions: TWfcMusicAudioOptions;
  LRenderer: TWfcMusicEnsembleAudioRenderer; LFrame: TWfcMusicEnsembleFrame;
  LCaps: TWfcMusicEnsembleAudioVoiceCapacities; LBlock: TWfcMusicPcm16Samples;
  LWriter: TWfcMusicWaveStream; LSink: TByteSink;
  I, J: Integer; Rejected: Boolean;
begin
  LOptions := DefaultWfcMusicAudioOptions;
  for I := 0 to 3 do
  begin
    LClock := Default(TWfcMusicEnsembleAudioClock);
    LClock.TickCount := MalformedNumber(I);
    Rejected := False;
    try AdvanceWfcMusicEnsembleAudioClock(LClock, 1, 500000, 1, 44100);
    except on EWfcMusicEnsembleAudio do Rejected := True; end;
    Check(Rejected, 'host malformed wide clock ' + IntToStr(I));
    LCaps := Capacities([1]); LCaps[0] := MalformedNumber(I);
    LRenderer := nil; Rejected := False;
    try LRenderer := TWfcMusicEnsembleAudioRenderer.Create(LOptions, 1, LCaps);
    except on EWfcMusicEnsembleAudio do Rejected := True; end;
    LRenderer.Free;
    Check(Rejected, 'host malformed declared capacity ' + IntToStr(I));
  end;
  for I := 0 to 3 do
  begin
    LSink := TByteSink.Create; LWriter := nil; Rejected := False;
    try
      try LWriter := TWfcMusicWaveStream.Create(LSink,
        44100 + MalformedNumber(I), 1);
      except on EWfcMusicAudioStream do Rejected := True; end;
      Check(Rejected and (LSink.Calls = 0) and (Length(LSink.Bytes) = 0),
        'host malformed WAVE sample rate rejects before header ' + IntToStr(I));
    finally LWriter.Free; LSink.Free; end;
  end;
  LRenderer := TWfcMusicEnsembleAudioRenderer.Create(LOptions, 1, Capacities([1]));
  try
    for I := 0 to 2 do
      for J := 0 to 3 do
      begin
        LFrame := Frame([Voice(wmcaAttack, [60], [90])]);
        Rejected := False;
        try
          case J of
            0: begin LFrame.Voices[0].Tones[0].Pitch := MalformedNumber(I); LRenderer.AdmitFrame(LFrame, 1, 500000); end;
            1: begin LFrame.Voices[0].Tones[0].Velocity := MalformedNumber(I); LRenderer.AdmitFrame(LFrame, 1, 500000); end;
            2: LRenderer.AdmitFrame(LFrame, MalformedNumber(I), 500000);
            3: LRenderer.ReadSamples(MalformedNumber(I), LBlock);
          end;
        except on EWfcMusicEnsembleAudio do Rejected := True; end;
        Check(Rejected and LRenderer.NeedsInput and not LRenderer.Failed,
          'host malformed frame/read is preflighted ' + IntToStr(I) + '/' + IntToStr(J));
      end;
  finally LRenderer.Free; end;
  LSink := TByteSink.Create; LWriter := nil;
  try
    LWriter := TWfcMusicWaveStream.Create(LSink, 44100, 1);
    SetLength(LBlock, 1);
    for I := 0 to 3 do
    begin
      LBlock[0] := MalformedNumber(I); Rejected := False;
      try LWriter.AppendSamples(LBlock); except on EWfcMusicAudioStream do Rejected := True; end;
      Check(Rejected and not LWriter.Failed and (LSink.Calls = 1),
        'host malformed PCM sample rejects before writes ' + IntToStr(I));
    end;
  finally LWriter.Free; LSink.Free; end;
end;
{$ENDIF}

procedure TestDirectWaveSamples;
var LSink: TByteSink; LWriter: TWfcMusicWaveStream; LClip: TWfcMusicPcm16Clip;
  LSamples: TWfcMusicPcm16Samples; I, LCalls: Integer; Rejected: Boolean;
begin
  SetLength(LSamples, 5000);
  for I := 0 to High(LSamples) do LSamples[I] := TWfcMusicPcm16Sample((I * 37) mod 65536 - 32768);
  LSamples[1] := -1; LSamples[2] := 0; LSamples[3] := 1; LSamples[4] := 32767;
  LSink := TByteSink.Create; LWriter := nil; LClip := nil;
  try
    LWriter := TWfcMusicWaveStream.Create(LSink, 44100, Length(LSamples));
    LWriter.AppendSamples([]);
    LWriter.AppendSamples(LSamples);
    LWriter.Finish;
    LClip := TWfcMusicPcm16Clip.Create(44100, LSamples);
    Check(SameBytes(LSink.Bytes, EncodeWfcMusicWave(LClip)), 'AppendSamples preserves exact PCM16 WAVE bytes');
    Check((LSink.MaxBlock <= 4096) and (LWriter.FrameCount = 5000), 'direct sample writes remain bounded');
    Rejected := False;
    try LWriter.AppendSamples([]); except on EWfcMusicAudioStream do Rejected := True; end;
    Check(Rejected, 'direct append obeys finished stream state');
  finally LClip.Free; LWriter.Free; LSink.Free; end;
  LSink := TByteSink.Create; LWriter := nil;
  try
    LWriter := TWfcMusicWaveStream.Create(LSink, 44100, 4999);
    LCalls := LSink.Calls; Rejected := False;
    try LWriter.AppendSamples(LSamples); except on EWfcMusicAudioStream do Rejected := True; end;
    Check(Rejected and not LWriter.Failed and (LSink.Calls = LCalls), 'oversized direct block rejects before sink writes');
  finally LWriter.Free; LSink.Free; end;
  LSink := TByteSink.Create; LWriter := nil;
  try
    LWriter := TWfcMusicWaveStream.Create(LSink, 44100, 5000);
    LSink.FailOnCall := 3; Rejected := False;
    try LWriter.AppendSamples(LSamples); except on EExpectedSinkFailure do Rejected := True; end;
    Check(Rejected and LWriter.Failed and (LWriter.FrameCount = 2048),
      'direct sample sink fault preserves original exception and completed-block accounting');
    LCalls := LSink.Calls; Rejected := False;
    try LWriter.AppendSamples([]); except on EWfcMusicAudioStream do Rejected := True; end;
    Check(Rejected and (LSink.Calls = LCalls), 'failed direct stream forbids later writes');
  finally LWriter.Free; LSink.Free; end;
  LSink := TByteSink.Create; LWriter := nil;
  try
    LWriter := TWfcMusicWaveStream.Create(LSink, 44100, 5000);
    LSink.Reenter := LWriter; Rejected := False;
    try LWriter.AppendSamples(LSamples); except on EWfcMusicAudioStream do Rejected := True; end;
    Check(Rejected and LWriter.Failed and (LWriter.FrameCount = 0), 'direct sample sink cannot reenter writer');
  finally LWriter.Free; LSink.Free; end;
end;

begin
  WriteLn('WFC incremental ensemble PCM conformance');
  Check(WFC_MUSIC_ENSEMBLE_AUDIO_VERSION = 1, 'renderer algorithm version');
  Run('finite preview parity and bounded block partitions', @TestPreviewParity);
  Run('sustained seams, reattacks and detached input', @TestSeamsAndDetachment);
  Run('exact rational clock and wide arithmetic', @TestExactClock);
  Run('admission validation and cancellation', @TestAdmissionAndCancellation);
  Run('zero-frame, silent and huge lazy input', @TestZeroFramesAndWideAdmission);
  Run('constructor guards', @TestConstructorGuards);
  Run('returned block ownership and larger chords', @TestOutputOwnershipAndLargerChords);
  {$IFDEF PAS2JS}Run('malformed host numeric input', @TestHostNumericValidation);{$ENDIF}
  Run('direct bounded WAVE sample blocks', @TestDirectWaveSamples);
  WriteLn(Checks, ' checks, ', Failures, ' failures');
  if Failures <> 0 then Halt(1);
end.
