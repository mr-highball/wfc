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
program wfc_music_ensemble_midi_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc_model, wfc_music, wfc_music_sequence, wfc_music_ensemble,
  wfc_midi_smf, wfc_midi_stream, wfc_music_midi, wfc_music_ensemble_midi;

type
  TTestProcedure = procedure;
  TWideValues = array of TWfcMidiStreamCount;

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

function Voice(const AAction: TWfcMusicCellAction; const APitches,
  AVelocities: array of Integer): TWfcMusicVoiceCell;
var LTones: TWfcMusicTones; I: Integer;
begin
  if Length(APitches) <> Length(AVelocities) then raise Exception.Create('fixture arity');
  SetLength(LTones, Length(APitches));
  for I := 0 to High(APitches) do LTones[I] := MakeWfcMusicTone(APitches[I], AVelocities[I]);
  Result := MakeWfcMusicVoiceCell(AAction, LTones);
end;

function Frame(const AVoices: array of TWfcMusicVoiceCell): TWfcMusicEnsembleFrame;
var LVoices: TWfcMusicVoiceCells; I: Integer;
begin
  SetLength(LVoices, Length(AVoices));
  for I := 0 to High(AVoices) do LVoices[I] := AVoices[I];
  Result := MakeWfcMusicEnsembleFrame(LVoices);
end;

function Options(const AChannels: array of Integer): TWfcMusicEnsembleMidiOptions;
begin
  Result := DefaultWfcMusicEnsembleMidiOptions(AChannels);
  Result.TicksPerQuarter := 120;
  Result.TempoMicrosecondsPerQuarter := 500000;
  Result.MeterNumerator := 4;
  Result.MeterDenominatorPower := 2;
end;

function SameBytes(const A, B: TWfcMidiBytes): Boolean;
var I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do if A[I] <> B[I] then Exit(False);
  Result := True;
end;

function HexBytes(const AHex: String): TWfcMidiBytes;
var I, N: Integer;
begin
  Result := nil;
  if (Length(AHex) mod 2) <> 0 then raise Exception.Create('odd golden length');
  SetLength(Result, Length(AHex) div 2);
  for I := 0 to High(Result) do
  begin
    N := StrToInt('$' + Copy(AHex, I * 2 + 1, 2));
    Result[I] := Byte(N);
  end;
end;

procedure Append(var ATarget: TWfcMidiBytes; const AValues: TWfcMidiBytes);
var I, LOffset: Integer;
begin
  LOffset := Length(ATarget);
  SetLength(ATarget, LOffset + Length(AValues));
  for I := 0 to High(AValues) do ATarget[LOffset + I] := AValues[I];
end;

procedure Drain(const AStream: TWfcMusicEnsembleMidiStream;
  const ABlockSize: Integer; var ABytes: TWfcMidiBytes);
var LBlock: TWfcMidiBytes;
begin
  while AStream.ReadBytes(ABlockSize, LBlock) do
  begin
    if (Length(LBlock) < 1) or (Length(LBlock) > ABlockSize) or
      (Length(LBlock) > 4096) then raise Exception.Create('unbounded MIDI block');
    Append(ABytes, LBlock);
  end;
  if Length(LBlock) <> 0 then raise Exception.Create('False must return nil');
end;

function PlanFrames(const AFrames: TWfcMusicEnsembleFrames;
  const ALengths: array of TWfcMidiStreamCount;
  const AOptions: TWfcMusicEnsembleMidiOptions): TWfcMusicEnsembleMidiPlan;
var LCounter: TWfcMusicEnsembleMidiCounter; I: Integer;
begin
  LCounter := TWfcMusicEnsembleMidiCounter.Create(AOptions);
  try
    for I := 0 to High(AFrames) do LCounter.AdmitFrame(AFrames[I], ALengths[I]);
    Result := LCounter.Finish;
    Check(LCounter.Finished and not LCounter.Failed and
      (LCounter.TickCount = Result.EndTick), 'counter finishes at exact end');
  finally LCounter.Free; end;
end;

function Replay(const AFrames: TWfcMusicEnsembleFrames;
  const ALengths: array of TWfcMidiStreamCount;
  const APlan: TWfcMusicEnsembleMidiPlan; const ABlockSize: Integer): TWfcMidiBytes;
var LStream: TWfcMusicEnsembleMidiStream; I: Integer;
begin
  Result := nil;
  LStream := TWfcMusicEnsembleMidiStream.Create(APlan);
  try
    Check(not LStream.NeedsInput, 'header and initial metadata require draining');
    Drain(LStream, ABlockSize, Result);
    for I := 0 to High(AFrames) do
    begin
      if not LStream.NeedsInput then raise Exception.Create('missing input boundary');
      LStream.AdmitFrame(AFrames[I], ALengths[I]);
      Drain(LStream, ABlockSize, Result);
    end;
    LStream.EndInput;
    Drain(LStream, ABlockSize, Result);
    Check(LStream.Finished and LStream.InputEnded and not LStream.NeedsInput and
      not LStream.Failed and not LStream.Cancelled, 'replay reaches complete terminal state');
    Check((LStream.EmittedBytes = Length(Result)) and
      (Length(Result) = APlan.ByteCount + 22) and
      (LStream.TickCount = APlan.EndTick), 'file length and tick counters match plan');
    LStream.EndInput;
    Drain(LStream, ABlockSize, Result);
  finally LStream.Free; end;
end;

function FixtureFrames: TWfcMusicEnsembleFrames;
begin
  Result := nil;
  SetLength(Result, 4);
  Result[0] := Frame([Voice(wmcaAttack, [48, 55], [80, 70]), Voice(wmcaAttack, [60], [90])]);
  Result[1] := Frame([Voice(wmcaHold, [48, 55], [80, 70]), Voice(wmcaAttack, [62], [90])]);
  Result[2] := Frame([Voice(wmcaHold, [48, 55], [80, 70]), MakeWfcMusicRestVoiceCell]);
  Result[3] := Frame([Voice(wmcaAttack, [48, 55], [80, 70]), Voice(wmcaAttack, [64], [90])]);
end;

function InvalidAction: TWfcMusicCellAction;
{$IFNDEF PAS2JS}var LValue: Integer;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Result = 99; end;
  {$ELSE}
  LValue := 99;
  Move(LValue, Result, SizeOf(Result));
  {$ENDIF}
end;

function ScoreForFrames(const AFrames: TWfcMusicEnsembleFrames): TWfcMusicScore;
var LTracks: TWfcMusicTracks; LVoices: TWfcMusicVoices;
  LMeters: TWfcMusicMeterChanges; LTempos: TWfcMusicTempoChanges;
  LSpans: TWfcMusicSpanEvents; I: Integer;
begin
  SetLength(LTracks, 1); LTracks[0] := MakeWfcMusicTrack('midi', 'MIDI fixture');
  SetLength(LVoices, Length(AFrames[0].Voices));
  for I := 0 to High(LVoices) do
    LVoices[I] := MakeWfcMusicVoice(0, TWfcModelToken('voice' + IntToStr(I)));
  SetLength(LMeters, 1); LMeters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(LTempos, 1); LTempos[0] := MakeWfcMusicTempoChange(0, 500000);
  LSpans := RebuildWfcMusicEnsembleSpans(AFrames, 120);
  Result := TWfcMusicScore.Create(120, 12, Length(AFrames) * 120,
    LTracks, LVoices, LMeters, LTempos, LSpans);
end;

procedure TestExactBytesAndFiniteParity;
const BLOCKS: array[0..5] of Integer = (1, 2, 3, 7, 4096, 4097);
  { Independent hand-written format-0 fixture: 75 track bytes. The held
    bass chord ends at tick360, not at either intervening frame boundary. }
  GOLDEN = '4D546864000000060000000100784D54726B0000004B' +
    '00FF510307A12000FF580404021808' +
    '009030500090374600913C5A' +
    '78813C0000913E5A' +
    '78813E00' +
    '788030000080370000903050009037460091405A' +
    '78803000008037000081400000FF2F00';
var LFrames: TWfcMusicEnsembleFrames; LPlan: TWfcMusicEnsembleMidiPlan;
  LBytes, LGolden: TWfcMidiBytes; LScore: TWfcMusicScore; I: Integer;
begin
  LFrames := FixtureFrames;
  LPlan := PlanFrames(LFrames, [120, 120, 120, 120], Options([0, 1]));
  LScore := nil;
  try
    Check((LPlan.EndTick = 480) and (LPlan.ByteCount = 75) and
      (LPlan.EventCount = 17) and (LPlan.BridgeCount = 0), 'independent plan event/byte totals');
    LGolden := HexBytes(GOLDEN);
    LScore := ScoreForFrames(LFrames);
    Check(SameBytes(EncodeWfcMusicMidi(LScore), LGolden), 'finite exporter agrees with handwritten bytes');
    for I := 0 to High(BLOCKS) do
    begin
      LBytes := Replay(LFrames, [120, 120, 120, 120], LPlan, BLOCKS[I]);
      Check(SameBytes(LBytes, LGolden), 'exact held-chord/reattack golden, block ' + IntToStr(BLOCKS[I]));
      Check(SameBytes(LBytes, EncodeWfcMusicMidi(LScore)), 'old finite export unchanged, block ' + IntToStr(BLOCKS[I]));
    end;
  finally LScore.Free; LPlan.Free; end;
end;

procedure TestTimingAndMappedOrdering;
var LCounter: TWfcMusicEnsembleMidiCounter; LPlan: TWfcMusicEnsembleMidiPlan;
  LStream: TWfcMusicEnsembleMidiStream; LTiming: TWfcMusicEnsembleMidiTiming;
  LFrames: TWfcMusicEnsembleFrames; LFile: TWfcMidiFile;
  LBytes: TWfcMidiBytes; LTick: TWfcMidiStreamCount; I: Integer;
begin
  SetLength(LFrames, 3);
  LFrames[0] := Frame([Voice(wmcaAttack, [48], [80]), Voice(wmcaAttack, [60], [90])]);
  LFrames[1] := Frame([Voice(wmcaAttack, [50], [81]), Voice(wmcaAttack, [62], [91])]);
  LFrames[2] := Frame([Voice(wmcaHold, [50], [81]), Voice(wmcaHold, [62], [91])]);
  LTiming := Default(TWfcMusicEnsembleMidiTiming);
  LTiming.TempoMicrosecondsPerQuarter := 600000;
  LTiming.MeterNumerator := 3; LTiming.MeterDenominatorPower := 3;
  LCounter := TWfcMusicEnsembleMidiCounter.Create(Options([9, 0]));
  LPlan := nil; LStream := nil; LBytes := nil;
  try
    LCounter.AdmitFrame(LFrames[0], 120);
    LCounter.AdmitFrame(LFrames[1], 120, LTiming);
    LCounter.AdmitFrame(LFrames[2], 120, LTiming);
    LPlan := LCounter.Finish;
    LStream := TWfcMusicEnsembleMidiStream.Create(LPlan);
    Drain(LStream, 5, LBytes);
    LStream.AdmitFrame(LFrames[0], 120); Drain(LStream, 5, LBytes);
    LStream.AdmitFrame(LFrames[1], 120, LTiming); Drain(LStream, 5, LBytes);
    LStream.AdmitFrame(LFrames[2], 120, LTiming); Drain(LStream, 5, LBytes);
    LStream.EndInput; Drain(LStream, 5, LBytes);
    LFile := DecodeWfcMidiFile(LBytes);
    Check((LFile.Format = 0) and (Length(LFile.Tracks) = 1) and
      (LFile.TicksPerQuarter = 120), 'format0 has exactly one metrical track');
    Check((LFile.Tracks[0].Events[2].Status = $99) and
      (LFile.Tracks[0].Events[3].Status = $90), 'explicit mapping preserves voice order, including channel9');
    Check((LFile.Tracks[0].Events[4].MetaType = $51) and
      (LFile.Tracks[0].Events[5].MetaType = $58) and
      (LFile.Tracks[0].Events[6].Status = $89) and
      (LFile.Tracks[0].Events[7].Status = $80) and
      (LFile.Tracks[0].Events[8].Status = $99) and
      (LFile.Tracks[0].Events[9].Status = $90),
      'changed metadata precedes all offs then all ons at shared tick');
    Check((LFile.Tracks[0].Events[10].MetaType = $51) and
      (LFile.Tracks[0].Events[11].MetaType = $58), 'explicit repeated timing remains observable');
    Check(SameBytes(LFile.Tracks[0].Events[4].Data, HexBytes('0927C0')) and
      SameBytes(LFile.Tracks[0].Events[5].Data, HexBytes('03031808')), 'tempo and meter encoded exactly');
    LTick := 0;
    for I := 0 to High(LFile.Tracks[0].Events) do Inc(LTick, LFile.Tracks[0].Events[I].DeltaTicks);
    Inc(LTick, LFile.Tracks[0].EndDeltaTicks);
    Check((LTick = 360) and (Length(LFile.Tracks[0].Events) = 14), 'timing does not reattack held notes or alter end');
  finally LStream.Free; LPlan.Free; LCounter.Free; end;
end;

procedure CheckRejectedOptions(const AOptions: TWfcMusicEnsembleMidiOptions;
  const ALabel: String);
var LCounter: TWfcMusicEnsembleMidiCounter; Rejected: Boolean;
begin
  LCounter := nil; Rejected := False;
  try LCounter := TWfcMusicEnsembleMidiCounter.Create(AOptions);
  except on EWfcMidiStream do Rejected := True; end;
  LCounter.Free;
  Check(Rejected, ALabel);
end;

procedure TestConstructorValidation;
var LOptions: TWfcMusicEnsembleMidiOptions; I: Integer;
  LStream: TWfcMusicEnsembleMidiStream; Rejected: Boolean;
begin
  for I := 0 to 12 do
  begin
    LOptions := Options([0]);
    case I of
      0: LOptions.TicksPerQuarter := 0;
      1: LOptions.TicksPerQuarter := 32768;
      2: LOptions.TempoMicrosecondsPerQuarter := 0;
      3: LOptions.TempoMicrosecondsPerQuarter := $1000000;
      4: LOptions.MeterNumerator := 0;
      5: LOptions.MeterNumerator := 256;
      6: LOptions.MeterDenominatorPower := -1;
      7: LOptions.MeterDenominatorPower := 256;
      8: LOptions.Channels := nil;
      9: LOptions.Channels[0] := -1;
      10: LOptions.Channels[0] := 16;
      11: begin SetLength(LOptions.Channels, 2); LOptions.Channels[1] := 0; end;
      12: SetLength(LOptions.Channels, 17);
    end;
    CheckRejectedOptions(LOptions, 'invalid constructor option ' + IntToStr(I));
  end;
  LStream := nil; Rejected := False;
  try LStream := TWfcMusicEnsembleMidiStream.Create(nil);
  except on EWfcMidiStream do Rejected := True; end;
  LStream.Free;
  Check(Rejected, 'nil replay plan rejects');
end;

procedure TestBoundaryTiming;
var LOptions: TWfcMusicEnsembleMidiOptions; LCounter: TWfcMusicEnsembleMidiCounter;
  LPlan: TWfcMusicEnsembleMidiPlan; LBytes: TWfcMidiBytes; LFile: TWfcMidiFile;
begin
  LOptions := Options([15]);
  LOptions.TicksPerQuarter := 32767;
  LOptions.TempoMicrosecondsPerQuarter := $FFFFFF;
  LOptions.MeterNumerator := 255; LOptions.MeterDenominatorPower := 255;
  LCounter := TWfcMusicEnsembleMidiCounter.Create(LOptions); LPlan := nil;
  try
    LPlan := LCounter.Finish; LBytes := Replay(nil, [], LPlan, 4096);
    LFile := DecodeWfcMidiFile(LBytes);
    Check((LFile.TicksPerQuarter = 32767) and
      SameBytes(LFile.Tracks[0].Events[0].Data, HexBytes('FFFFFF')) and
      SameBytes(LFile.Tracks[0].Events[1].Data, HexBytes('FFFF1808')),
      'wire numeric maxima have no narrower invented musical restriction');
  finally LPlan.Free; LCounter.Free; end;
end;

procedure TestMalformedAdmission;
var LCounter: TWfcMusicEnsembleMidiCounter; LPlan: TWfcMusicEnsembleMidiPlan;
  LStream: TWfcMusicEnsembleMidiStream; LGood, LBad: TWfcMusicEnsembleFrame;
  LTiming: TWfcMusicEnsembleMidiTiming; LLength: TWfcMidiStreamCount;
  LBytes: TWfcMidiBytes; I, J: Integer; Rejected: Boolean;
begin
  LGood := Frame([Voice(wmcaAttack, [60], [90])]);
  LCounter := TWfcMusicEnsembleMidiCounter.Create(Options([0]));
  LPlan := nil; LStream := nil; LBytes := nil;
  try
    LCounter.AdmitFrame(LGood, 1); LPlan := LCounter.Finish;
    LCounter.Free; LCounter := nil;
    LCounter := TWfcMusicEnsembleMidiCounter.Create(Options([0]));
    LStream := TWfcMusicEnsembleMidiStream.Create(LPlan); Drain(LStream, 4096, LBytes);
    for I := 0 to 20 do
    begin
      LBad := Frame([Voice(wmcaAttack, [60], [90])]);
      LLength := 1; LTiming := Default(TWfcMusicEnsembleMidiTiming);
      case I of
        0: LLength := 0;
        1: LLength := -1;
        2: LLength := WFC_MIDI_STREAM_MAX_SAFE_INTEGER + 1;
        3: LBad.Voices := nil;
        4: SetLength(LBad.Voices, 2);
        5: LBad.Voices[0].Action := wmcaHold;
        6: LBad.Voices[0].Action := wmcaRest;
        7: LBad.Voices[0].Tones := nil;
        8: LBad.Voices[0].Tones[0].Pitch := -1;
        9: LBad.Voices[0].Tones[0].Pitch := 128;
        10: LBad.Voices[0].Tones[0].Velocity := 0;
        11: LBad.Voices[0].Tones[0].Velocity := 128;
        12: begin SetLength(LBad.Voices[0].Tones, 2); LBad.Voices[0].Tones[1] := LBad.Voices[0].Tones[0]; end;
        13: begin SetLength(LBad.Voices[0].Tones, 2); LBad.Voices[0].Tones[1] := MakeWfcMusicTone(59, 90); end;
        14: LTiming.TempoMicrosecondsPerQuarter := -1;
        15: LTiming.TempoMicrosecondsPerQuarter := $1000000;
        16: LTiming.MeterNumerator := 256;
        17: LTiming.MeterDenominatorPower := 1;
        18: begin LTiming.MeterNumerator := 4; LTiming.MeterDenominatorPower := 256; end;
        19: LBad.Voices[0].Action := InvalidAction;
        20: LTiming.MeterNumerator := -1;
      end;
      for J := 0 to 1 do
      begin
        Rejected := False;
        try
          if J = 0 then LCounter.AdmitFrame(LBad, LLength, LTiming)
          else LStream.AdmitFrame(LBad, LLength, LTiming);
        except on EWfcMidiStream do Rejected := True; end;
        Check(Rejected, 'malformed admission ' + IntToStr(I) + '/' + IntToStr(J));
      end;
      Check((LCounter.TickCount = 0) and not LCounter.Failed and not LCounter.Finished and
        (LStream.TickCount = 0) and LStream.NeedsInput and not LStream.Failed and
        (LStream.EmittedBytes = Length(LBytes)), 'invalid input preserves both states ' + IntToStr(I));
    end;
    LCounter.AdmitFrame(LGood, 1);
    LStream.AdmitFrame(LGood, 1); Drain(LStream, 4, LBytes);
    LStream.EndInput; Drain(LStream, 4, LBytes);
    Check(LStream.Finished and not LStream.Failed, 'corrected frame succeeds after every preflight rejection');
  finally LStream.Free; LPlan.Free; LCounter.Free; end;
end;

procedure TestStatePreconditionsAndRepartition;
var LFrames: TWfcMusicEnsembleFrames; LPlan: TWfcMusicEnsembleMidiPlan;
  LCounter: TWfcMusicEnsembleMidiCounter; LStream: TWfcMusicEnsembleMidiStream;
  LBytes, LBlock, LExpected: TWfcMidiBytes; LTiming: TWfcMusicEnsembleMidiTiming;
  LAttack, LHold, LRest: TWfcMusicEnsembleFrame; Rejected: Boolean; I: Integer;
  LBefore: TWfcMidiStreamCount;
begin
  LAttack := Frame([Voice(wmcaAttack, [60], [90])]);
  LHold := Frame([Voice(wmcaHold, [60], [90])]);
  LRest := Frame([MakeWfcMusicRestVoiceCell]);
  SetLength(LFrames, 2); LFrames[0] := LAttack; LFrames[1] := LHold;
  LPlan := PlanFrames(LFrames, [1, 5], Options([0]));
  LStream := nil; LCounter := nil;
  try
    LExpected := Replay(LFrames, [1, 5], LPlan, 4);
    LStream := TWfcMusicEnsembleMidiStream.Create(LPlan); LBytes := nil;
    for I := 0 to 1 do
    begin
      Rejected := False;
      try
        if I = 0 then LStream.AdmitFrame(LAttack, 1) else LStream.EndInput;
      except on EWfcMidiStream do Rejected := True; end;
      Check(Rejected and not LStream.Failed and not LStream.InputEnded and
        (LStream.TickCount = 0) and (LStream.EmittedBytes = 0), 'initial pending output rejects premature input ' + IntToStr(I));
    end;
    Check(LStream.ReadBytes(1, LBlock), 'single header byte available');
    Append(LBytes, LBlock); LBlock[0] := 0;
    Drain(LStream, 1, LBytes);
    LStream.AdmitFrame(LAttack, 2);
    for I := 0 to 1 do
    begin
      Rejected := False;
      try
        if I = 0 then LStream.AdmitFrame(LHold, 1) else LStream.EndInput;
      except on EWfcMidiStream do Rejected := True; end;
      Check(Rejected and not LStream.Failed and not LStream.InputEnded and
        (LStream.TickCount = 2), 'pending note output rejects premature input ' + IntToStr(I));
    end;
    Drain(LStream, 1, LBytes);
    LTiming := Default(TWfcMusicEnsembleMidiTiming);
    LStream.AdmitFrame(LHold, 2, LTiming); Drain(LStream, 1, LBytes);
    LStream.AdmitFrame(LHold, 2); Drain(LStream, 1, LBytes);
    LStream.EndInput; LStream.EndInput; Drain(LStream, 1, LBytes);
    Check(SameBytes(LBytes, LExpected), 'logical MIDI replay survives different hold frame partitions and detached blocks');
    LBefore := LStream.EmittedBytes; Rejected := False;
    try LStream.AdmitFrame(LHold, 1); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and LStream.Finished and not LStream.Failed and
      (LStream.EmittedBytes = LBefore), 'finished stream cannot accept more frames');
    LStream.Cancel; LStream.Cancel;
    Check(LStream.Finished and not LStream.Cancelled and not LStream.Failed and
      LStream.InputEnded and not LStream.NeedsInput and
      (LStream.EmittedBytes = LBefore) and not LStream.ReadBytes(1, LBlock) and
      (Length(LBlock) = 0), 'completed cancellation preserves successful completion');
    LCounter := TWfcMusicEnsembleMidiCounter.Create(Options([0]));
    LCounter.AdmitFrame(LAttack, 1); LCounter.AdmitFrame(LRest, 1);
    Rejected := False;
    try LCounter.AdmitFrame(LHold, 1); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and not LCounter.Failed and (LCounter.TickCount = 2), 'rest terminates the active predecessor needed for a hold');
  finally LCounter.Free; LStream.Free; LPlan.Free; end;
end;

procedure TestTimingReplayMismatch;
var LCounter: TWfcMusicEnsembleMidiCounter; LPlan: TWfcMusicEnsembleMidiPlan;
  LStream: TWfcMusicEnsembleMidiStream; LTiming, LChanged: TWfcMusicEnsembleMidiTiming;
  LAttack, LHold: TWfcMusicEnsembleFrame; LBytes: TWfcMidiBytes;
  Rejected, HasEOT: Boolean; I, J: Integer;
begin
  LAttack := Frame([Voice(wmcaAttack, [60], [90])]);
  LHold := Frame([Voice(wmcaHold, [60], [90])]);
  LTiming := Default(TWfcMusicEnsembleMidiTiming);
  LTiming.TempoMicrosecondsPerQuarter := 600000;
  LTiming.MeterNumerator := 3; LTiming.MeterDenominatorPower := 2;
  LCounter := TWfcMusicEnsembleMidiCounter.Create(Options([0])); LPlan := nil;
  try
    LCounter.AdmitFrame(LAttack, 1); LCounter.AdmitFrame(LHold, 1, LTiming);
    LPlan := LCounter.Finish;
    for I := 0 to 3 do
    begin
      LStream := TWfcMusicEnsembleMidiStream.Create(LPlan); LBytes := nil;
      try
        Drain(LStream, 1, LBytes);
        LStream.AdmitFrame(LAttack, 1); Drain(LStream, 1, LBytes);
        LChanged := LTiming;
        case I of
          0: LChanged.TempoMicrosecondsPerQuarter := 600001;
          1: LChanged.MeterNumerator := 4;
          2: LChanged.MeterDenominatorPower := 3;
          3: LChanged := Default(TWfcMusicEnsembleMidiTiming);
        end;
        Rejected := False;
        try
          LStream.AdmitFrame(LHold, 1, LChanged); Drain(LStream, 1, LBytes);
          LStream.EndInput; Drain(LStream, 1, LBytes);
        except on EWfcMidiStream do Rejected := True; end;
        HasEOT := False;
        for J := 0 to Length(LBytes) - 3 do
          if (LBytes[J] = $FF) and (LBytes[J + 1] = $2F) and
            (LBytes[J + 2] = 0) then HasEOT := True;
        Check(Rejected and LStream.Failed and not LStream.Finished and not HasEOT,
          'changed timing/order cannot produce a final EOT ' + IntToStr(I));
      finally LStream.Free; end;
    end;
  finally LPlan.Free; LCounter.Free; end;
end;

procedure TestHoldsAndOwnership;
var LOptions, LCopy: TWfcMusicEnsembleMidiOptions;
  LCounter: TWfcMusicEnsembleMidiCounter; LPlan, LOther: TWfcMusicEnsembleMidiPlan;
  LStream: TWfcMusicEnsembleMidiStream; LAttack, LHold, LBad: TWfcMusicEnsembleFrame;
  LBytes, LBlock: TWfcMidiBytes; LFile: TWfcMidiFile; Rejected: Boolean; I: Integer;
begin
  LOptions := Options([4]);
  LAttack := Frame([Voice(wmcaAttack, [48, 55], [80, 70])]);
  LHold := Frame([Voice(wmcaHold, [48, 55], [80, 70])]);
  LCounter := TWfcMusicEnsembleMidiCounter.Create(LOptions);
  LPlan := nil; LOther := nil; LStream := nil; LBytes := nil;
  try
    LOptions.Channels[0] := 7; LOptions.TicksPerQuarter := 960;
    LCounter.AdmitFrame(LAttack, 1);
    for I := 0 to 1 do
    begin
      LBad := Frame([Voice(wmcaHold, [48, 55], [80, 70])]);
      if I = 0 then LBad.Voices[0].Tones[1].Pitch := 56
      else LBad.Voices[0].Tones[1].Velocity := 71;
      Rejected := False;
      try LCounter.AdmitFrame(LBad, 1); except on EWfcMidiStream do Rejected := True; end;
      Check(Rejected and not LCounter.Failed and (LCounter.TickCount = 1), 'hold requires exact active tones/velocity ' + IntToStr(I));
    end;
    LCounter.AdmitFrame(LHold, 1); LPlan := LCounter.Finish; LOther := LCounter.Finish;
    Check((LPlan <> LOther) and (LPlan.Signature = LOther.Signature) and
      (LPlan.ByteCount = LOther.ByteCount), 'repeated counter Finish returns detached equal plans');
    LCopy := LPlan.CopyOptions;
    Check((LCopy.Channels[0] = 4) and (LCopy.TicksPerQuarter = 120), 'counter constructor deep-copies options');
    LCopy.Channels[0] := 12; LCopy.TempoMicrosecondsPerQuarter := 1;
    LCopy := LOther.CopyOptions;
    Check((LCopy.Channels[0] = 4) and (LCopy.TempoMicrosecondsPerQuarter = 500000), 'plan CopyOptions does not expose mutable internals');
    LStream := TWfcMusicEnsembleMidiStream.Create(LPlan);
    LPlan.Free; LPlan := nil; LOther.Free; LOther := nil; LCounter.Free; LCounter := nil;
    Drain(LStream, 3, LBytes);
    LStream.AdmitFrame(LAttack, 1);
    LAttack.Voices[0].Tones[0].Pitch := 1;
    Drain(LStream, 3, LBytes);
    for I := 0 to 1 do
    begin
      LBad := Frame([Voice(wmcaHold, [48, 55], [80, 70])]);
      if I = 0 then LBad.Voices[0].Tones[1].Pitch := 56
      else LBad.Voices[0].Tones[1].Velocity := 71;
      Rejected := False;
      try LStream.AdmitFrame(LBad, 1); except on EWfcMidiStream do Rejected := True; end;
      Check(Rejected and not LStream.Failed and (LStream.TickCount = 1), 'replay hold preflight is retryable ' + IntToStr(I));
    end;
    LStream.AdmitFrame(LHold, 1); LHold.Voices := nil;
    Drain(LStream, 3, LBytes); LStream.EndInput; Drain(LStream, 3, LBytes);
    LFile := DecodeWfcMidiFile(LBytes);
    Check((Length(LFile.Tracks[0].Events) = 6) and
      (LFile.Tracks[0].Events[2].Status = $94) and
      (LFile.Tracks[0].Events[2].Data[0] = 48) and
      (LFile.Tracks[0].Events[4].DeltaTicks = 2), 'freed plans and mutated admitted frames do not alter held output');
    Rejected := False;
    try LStream.ReadBytes(0, LBlock); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and LStream.Finished, 'invalid read remains invalid after finish');
  finally LStream.Free; LOther.Free; LPlan.Free; LCounter.Free; end;
end;

procedure TestLongGapBridges;
const M: TWfcMidiStreamCount = 268435455;
var LGaps: TWideValues; LFrames: TWfcMusicEnsembleFrames;
  LPlan: TWfcMusicEnsembleMidiPlan; LBytes: TWfcMidiBytes; LFile: TWfcMidiFile;
  I, J, K, LBridges: Integer; LTick, LOnTick, LOffTick: TWfcMidiStreamCount;
begin
  SetLength(LGaps, 7);
  LGaps[0] := 127; LGaps[1] := 128; LGaps[2] := M - 1; LGaps[3] := M;
  LGaps[4] := M + 1; LGaps[5] := M * 2; LGaps[6] := M * 2 + 1;
  SetLength(LFrames, 2);
  for I := 0 to High(LGaps) do
    for J := 0 to 2 do
    begin
      case J of
        0: begin LFrames[0] := Frame([MakeWfcMusicRestVoiceCell]); LFrames[1] := Frame([Voice(wmcaAttack, [60], [90])]); end;
        1: begin LFrames[0] := Frame([Voice(wmcaAttack, [60], [90])]); LFrames[1] := Frame([Voice(wmcaHold, [60], [90])]); end;
        2: begin LFrames[0] := Frame([Voice(wmcaAttack, [60], [90])]); LFrames[1] := Frame([MakeWfcMusicRestVoiceCell]); end;
      end;
      if J = 2 then LPlan := PlanFrames(LFrames, [1, LGaps[I]], Options([0]))
      else LPlan := PlanFrames(LFrames, [LGaps[I], 1], Options([0]));
      try
        if J = 2 then LBytes := Replay(LFrames, [1, LGaps[I]], LPlan, 2)
        else LBytes := Replay(LFrames, [LGaps[I], 1], LPlan, 2);
        LFile := DecodeWfcMidiFile(LBytes); LTick := 0; LOnTick := -1; LOffTick := -1; LBridges := 0;
        for K := 0 to High(LFile.Tracks[0].Events) do
        begin
          Inc(LTick, LFile.Tracks[0].Events[K].DeltaTicks);
          if LFile.Tracks[0].Events[K].Status = $90 then LOnTick := LTick;
          if LFile.Tracks[0].Events[K].Status = $80 then LOffTick := LTick;
          if (LFile.Tracks[0].Events[K].Status = $FF) and
            (LFile.Tracks[0].Events[K].MetaType = 1) then
          begin
            Inc(LBridges);
            Check((Length(LFile.Tracks[0].Events[K].Data) = 0) and
              (LFile.Tracks[0].Events[K].DeltaTicks = M), 'bridge is empty text at max delta');
          end;
        end;
        Inc(LTick, LFile.Tracks[0].EndDeltaTicks);
        Check((LTick = LGaps[I] + 1) and (LPlan.BridgeCount = LBridges), 'bridges preserve exact total and report count');
        if J = 0 then Check((LOnTick = LGaps[I]) and (LOffTick = LGaps[I] + 1), 'initial silence remains exact')
        else if J = 1 then Check((LOnTick = 0) and (LOffTick = LGaps[I] + 1), 'held gap never retriggers the note')
        else Check((LOnTick = 0) and (LOffTick = 1), 'terminal EOT bridging preserves trailing silence');
        Check(Length(LFile.Tracks[0].Events) = 4 + LBridges, 'only metadata and one real note pair accompany bridges');
      finally LPlan.Free; end;
    end;
end;

procedure TestWideCapacityAndEmpty;
var LCounter: TWfcMusicEnsembleMidiCounter; LPlan: TWfcMusicEnsembleMidiPlan;
  LStream: TWfcMusicEnsembleMidiStream; LRest: TWfcMusicEnsembleFrame;
  LBytes, LBlock: TWfcMidiBytes; Rejected: Boolean;
begin
  LRest := Frame([MakeWfcMusicRestVoiceCell]);
  LCounter := TWfcMusicEnsembleMidiCounter.Create(Options([0]));
  LPlan := nil; LStream := nil; LBytes := nil;
  try
    LPlan := LCounter.Finish;
    Check((LPlan.EndTick = 0) and (LPlan.ByteCount = 19) and
      (LPlan.EventCount = 3) and (LPlan.BridgeCount = 0), 'empty stream contains initial timing and EOT');
    LBytes := Replay(nil, [], LPlan, 1);
    Check(SameBytes(LBytes, HexBytes('4D546864000000060000000100784D54726B00000013' +
      '00FF510307A12000FF58040402180800FF2F00')), 'independent empty-stream golden');
    Rejected := False;
    try LCounter.AdmitFrame(LRest, 1); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and LCounter.Finished and not LCounter.Failed, 'finished counter rejects new frames without poisoning');
    LPlan.Free; LPlan := nil; LCounter.Free; LCounter := nil;
    LCounter := TWfcMusicEnsembleMidiCounter.Create(Options([0]));
    LCounter.AdmitFrame(LRest, WFC_MIDI_STREAM_MAX_SAFE_INTEGER);
    Rejected := False;
    try LCounter.AdmitFrame(LRest, 1); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and not LCounter.Failed and
      (LCounter.TickCount = WFC_MIDI_STREAM_MAX_SAFE_INTEGER), 'total tick overflow preflights without arbitrary duration cap');
    LPlan := LCounter.Finish;
    { Independently calculated using integer division, without iterating any
      of the 33,554,432 synthetic bridge events. }
    Check((LPlan.EndTick = WFC_MIDI_STREAM_MAX_SAFE_INTEGER) and
      (LPlan.BridgeCount = 33554432) and (LPlan.ByteCount = 234881046) and
      (LPlan.EventCount = 3), 'maximum exact tick plan remains bounded-memory');
    LStream := TWfcMusicEnsembleMidiStream.Create(LPlan); LBytes := nil;
    Drain(LStream, 4096, LBytes);
    LStream.AdmitFrame(LRest, WFC_MIDI_STREAM_MAX_SAFE_INTEGER);
    Drain(LStream, 4096, LBytes);
    LStream.EndInput;
    Check(LStream.ReadBytes(1, LBlock) and (Length(LBlock) = 1) and
      not LStream.Finished, 'huge pending bridge run is pulled lazily');
    LStream.Cancel;
    Check(LStream.Cancelled and not LStream.Finished and
      not LStream.ReadBytes(1, LBlock) and (Length(LBlock) = 0), 'cancellation discards huge pending bridge run');
  finally LStream.Free; LPlan.Free; LCounter.Free; end;
end;

procedure TestReplayFailureAndCancel;
var LFrames: TWfcMusicEnsembleFrames; LPlan: TWfcMusicEnsembleMidiPlan;
  LStream: TWfcMusicEnsembleMidiStream; LBytes, LBlock: TWfcMidiBytes;
  LBad: TWfcMusicEnsembleFrame; Rejected: Boolean; I: Integer;
  LBefore: TWfcMidiStreamCount;
begin
  SetLength(LFrames, 1); LFrames[0] := Frame([Voice(wmcaAttack, [60], [90])]);
  LPlan := PlanFrames(LFrames, [1], Options([0]));
  try
    for I := 0 to 2 do
    begin
      LStream := TWfcMusicEnsembleMidiStream.Create(LPlan); LBytes := nil;
      try
        Drain(LStream, 4096, LBytes); Rejected := False;
        LBad := Frame([Voice(wmcaAttack, [60], [90])]);
        if I = 0 then LBad.Voices[0].Tones[0].Pitch := 61;
        if I = 1 then LBad.Voices[0].Tones[0].Velocity := 91;
        try
          if I = 2 then LStream.AdmitFrame(LBad, 2) else LStream.AdmitFrame(LBad, 1);
          Drain(LStream, 4096, LBytes);
          LStream.EndInput; Drain(LStream, 4096, LBytes);
        except on EWfcMidiStream do Rejected := True; end;
        Check(Rejected and LStream.Failed and not LStream.Finished,
          'changed replay rejects rather than publishing success ' + IntToStr(I));
        Check(LStream.EmittedBytes = Length(LBytes),
          'discarded mismatch block does not count as returned output ' + IntToStr(I));
        LBefore := LStream.EmittedBytes; Rejected := False;
        try LStream.EndInput; except on EWfcMidiStream do Rejected := True; end;
        Check(Rejected and (LBefore = LStream.EmittedBytes), 'failed replay cannot resume or append EOT');
        LStream.Cancel;
        Check(LStream.Failed and not LStream.Finished, 'cancel cannot convert failure into success');
      finally LStream.Free; end;
    end;
    for I := 0 to 3 do
    begin
      LStream := TWfcMusicEnsembleMidiStream.Create(LPlan); LBytes := nil;
      try
        if I > 0 then Drain(LStream, 4096, LBytes);
        if I > 1 then LStream.AdmitFrame(LFrames[0], 1);
        if I > 2 then begin Drain(LStream, 4096, LBytes); LStream.EndInput; end;
        LBefore := LStream.EmittedBytes;
        LStream.Cancel; LStream.Cancel;
        Check(LStream.Cancelled and not LStream.Finished and not LStream.NeedsInput and
          not LStream.ReadBytes(3, LBlock) and (Length(LBlock) = 0) and
          (LStream.EmittedBytes = LBefore), 'cancel discards pending bytes at lifecycle boundary ' + IntToStr(I));
        Rejected := False;
        try LStream.EndInput; except on EWfcMidiStream do Rejected := True; end;
        Check(Rejected, 'cancelled stream refuses EndInput');
        Rejected := False;
        try LStream.AdmitFrame(LFrames[0], 1); except on EWfcMidiStream do Rejected := True; end;
        Check(Rejected, 'cancelled stream refuses frames');
        Rejected := False;
        try LStream.ReadBytes(0, LBlock); except on EWfcMidiStream do Rejected := True; end;
        Check(Rejected, 'cancelled stream still validates read maximum');
      finally LStream.Free; end;
    end;
  finally LPlan.Free; end;
end;

procedure TestFailedReadAccounting;
var LFrames: TWfcMusicEnsembleFrames; LPlan: TWfcMusicEnsembleMidiPlan;
  LStream: TWfcMusicEnsembleMidiStream; LBad: TWfcMusicEnsembleFrame;
  LBytes, LBlock: TWfcMidiBytes; LBefore: TWfcMidiStreamCount; Rejected: Boolean;
begin
  SetLength(LFrames, 1); LFrames[0] := Frame([Voice(wmcaAttack, [60], [90])]);
  LPlan := PlanFrames(LFrames, [1], Options([0])); LStream := nil;
  try
    LStream := TWfcMusicEnsembleMidiStream.Create(LPlan); LBytes := nil;
    Drain(LStream, 4096, LBytes); LBefore := LStream.EmittedBytes;
    LBad := Frame([Voice(wmcaAttack, [60, 64, 67], [90, 90, 90])]);
    LStream.AdmitFrame(LBad, 1);
    { Two note events fit the planned remaining bytes, but a third exceeds
      the plan while the same coalesced caller block is still unpublished. }
    Rejected := False;
    try LStream.ReadBytes(4096, LBlock); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and LStream.Failed and (Length(LBlock) = 0),
      'mid-block replay overflow returns no partial block');
    Check((LStream.EmittedBytes = LBefore) and (LBefore = Length(LBytes)),
      'mid-block failure counts only bytes actually returned to caller');
  finally LStream.Free; LPlan.Free; end;
end;

procedure TestFullChannelChords;
var LOptions: TWfcMusicEnsembleMidiOptions; LFrames: TWfcMusicEnsembleFrames;
  LPlan: TWfcMusicEnsembleMidiPlan; LBytes: TWfcMidiBytes; LFile: TWfcMidiFile;
  I, J: Integer;
begin
  LOptions := Options([0]); SetLength(LOptions.Channels, 16);
  SetLength(LFrames, 2);
  SetLength(LFrames[0].Voices, 16); SetLength(LFrames[1].Voices, 16);
  for I := 0 to 15 do
  begin
    LOptions.Channels[I] := 15 - I;
    LFrames[0].Voices[I].Action := wmcaAttack;
    LFrames[1].Voices[I].Action := wmcaHold;
    SetLength(LFrames[0].Voices[I].Tones, 128);
    SetLength(LFrames[1].Voices[I].Tones, 128);
    for J := 0 to 127 do
    begin
      LFrames[0].Voices[I].Tones[J] := MakeWfcMusicTone(J, 127);
      LFrames[1].Voices[I].Tones[J] := MakeWfcMusicTone(J, 127);
    end;
  end;
  LPlan := PlanFrames(LFrames, [1, 1], LOptions);
  try
    Check((LPlan.EventCount = 4099) and (LPlan.ByteCount = 16403),
      'all legal channel/pitch pairs fit without an invented chord cap');
    LBytes := Replay(LFrames, [1, 1], LPlan, 4097);
    LFile := DecodeWfcMidiFile(LBytes);
    for I := 0 to 15 do
      Check((LFile.Tracks[0].Events[2 + I * 128].Status = $90 + 15 - I) and
        (LFile.Tracks[0].Events[2 + I * 128].Data[0] = 0) and
        (LFile.Tracks[0].Events[2 + I * 128 + 127].Data[0] = 127),
        'voice-order/chord pitch-order mapping ' + IntToStr(I));
    Check(LFile.Tracks[0].Events[2050].DeltaTicks = 2, 'large held chords have one shared lifetime');
  finally LPlan.Free; end;
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

procedure TestBrowserNumericValidation;
var LOptions: TWfcMusicEnsembleMidiOptions; LCounter: TWfcMusicEnsembleMidiCounter;
  LPlan: TWfcMusicEnsembleMidiPlan; LStream: TWfcMusicEnsembleMidiStream;
  LFrame: TWfcMusicEnsembleFrame; LTiming: TWfcMusicEnsembleMidiTiming;
  LBytes, LBlock: TWfcMidiBytes; I, J, K: Integer; Rejected: Boolean;
begin
  for I := 0 to 3 do
    for J := 0 to 4 do
    begin
      LOptions := Options([0]);
      case J of
        0: LOptions.TicksPerQuarter := MalformedNumber(I);
        1: LOptions.TempoMicrosecondsPerQuarter := MalformedNumber(I);
        2: LOptions.MeterNumerator := MalformedNumber(I);
        3: LOptions.MeterDenominatorPower := MalformedNumber(I);
        4: LOptions.Channels[0] := MalformedNumber(I);
      end;
      CheckRejectedOptions(LOptions, 'browser malformed option ' + IntToStr(I) + '/' + IntToStr(J));
    end;
  LCounter := TWfcMusicEnsembleMidiCounter.Create(Options([0]));
  LPlan := nil; LStream := nil; LBytes := nil;
  try
    LFrame := Frame([Voice(wmcaAttack, [60], [90])]);
    LCounter.AdmitFrame(LFrame, 1); LPlan := LCounter.Finish;
    LCounter.Free; LCounter := nil;
    LCounter := TWfcMusicEnsembleMidiCounter.Create(Options([0]));
    LStream := TWfcMusicEnsembleMidiStream.Create(LPlan); Drain(LStream, 4096, LBytes);
    for I := 0 to 3 do
      for J := 0 to 7 do
      begin
        LFrame := Frame([Voice(wmcaAttack, [60], [90])]);
        LTiming := Default(TWfcMusicEnsembleMidiTiming);
        case J of
          0: LFrame.Voices[0].Tones[0].Pitch := MalformedNumber(I);
          1: LFrame.Voices[0].Tones[0].Velocity := MalformedNumber(I);
          2: LTiming.TempoMicrosecondsPerQuarter := MalformedNumber(I);
          3: LTiming.MeterNumerator := MalformedNumber(I);
          4: begin LTiming.MeterNumerator := 4; LTiming.MeterDenominatorPower := MalformedNumber(I); end;
          7: LFrame.Voices[0].Action := TWfcMusicCellAction(MalformedNumber(I));
        end;
        for K := 0 to 1 do
        begin
          Rejected := False;
          try
            if K = 0 then
            begin
              if J = 5 then LCounter.AdmitFrame(LFrame, MalformedNumber(I))
              else if J = 6 then Continue
              else LCounter.AdmitFrame(LFrame, 1, LTiming);
            end
            else if J = 5 then LStream.AdmitFrame(LFrame, MalformedNumber(I))
            else if J = 6 then LStream.ReadBytes(MalformedNumber(I), LBlock)
            else LStream.AdmitFrame(LFrame, 1, LTiming);
          except on EWfcMidiStream do Rejected := True; end;
          Check(Rejected and (LCounter.TickCount = 0) and not LCounter.Failed and
            (LStream.TickCount = 0) and not LStream.Failed and LStream.NeedsInput,
            'browser malformed input preserves state ' + IntToStr(I) + '/' + IntToStr(J) + '/' + IntToStr(K));
        end;
      end;
  finally LStream.Free; LPlan.Free; LCounter.Free; end;
end;
{$ENDIF}

begin
  WriteLn('WFC incremental ensemble MIDI conformance');
  Run('handwritten bytes and finite exporter parity', @TestExactBytesAndFiniteParity);
  Run('timing changes and explicit channel ordering', @TestTimingAndMappedOrdering);
  Run('constructor preflight', @TestConstructorValidation);
  Run('wire timing capacity boundaries', @TestBoundaryTiming);
  Run('malformed frames and retryable admission', @TestMalformedAdmission);
  Run('state preconditions and logical hold repartition', @TestStatePreconditionsAndRepartition);
  Run('timing replay mismatches never emit final EOT', @TestTimingReplayMismatch);
  Run('exact holds and detached lifetimes', @TestHoldsAndOwnership);
  Run('long silent/held/terminal gap bridges', @TestLongGapBridges);
  Run('empty tracks and maximum exact tick capacity', @TestWideCapacityAndEmpty);
  Run('replay divergence and cancellation', @TestReplayFailureAndCancel);
  Run('coalesced failure output accounting', @TestFailedReadAccounting);
  Run('all channels and full pitch chords', @TestFullChannelChords);
  {$IFDEF PAS2JS}Run('malformed browser numeric values', @TestBrowserNumericValidation);{$ENDIF}
  WriteLn(Checks, ' checks, ', Failures, ' failures');
  if Failures <> 0 then Halt(1);
end.
