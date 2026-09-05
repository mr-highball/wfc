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
program wfc_music_ensemble_midi_stream_demo_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc_midi_smf,
  wfc_midi_stream,
  wfc_music_arrangement,
  wfc_music_ensemble_midi,
  ensemble_studio_stream,
  ensemble_studio_midi_stream;

var
  Checks, Failures: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then
  begin
    Inc(Failures);
    WriteLn('FAIL: ', AMessage);
  end;
end;

function MakePlan(const ASeconds: String; const ASeed: Cardinal;
  const ASegmentCells: Integer): TEnsembleStudioMidiPlan;
var
  LOptions: TEnsembleStudioStreamOptions;
  LPlanner: TEnsembleStudioMidiPlanner;
  LStep: TWfcMusicArrangementStep;
begin
  Result := nil;
  LOptions := DefaultEnsembleStudioStreamOptions;
  LOptions.Seed := ASeed;
  LOptions.SegmentCellCount := ASegmentCells;
  LPlanner := TEnsembleStudioMidiPlanner.Create(
    PlanEnsembleStudioFrames(ASeconds), LOptions);
  try
    repeat
      LStep := LPlanner.Next;
      if LStep = wmaspFailed then
        raise EEnsembleStudioMidiStream.Create(LPlanner.Failure);
      if LStep = wmaspCancelled then
        raise EEnsembleStudioMidiStream.Create('unexpected planning cancellation');
    until LStep = wmaspCompleted;
    Result := LPlanner.DetachPlan;
  finally
    LPlanner.Free;
  end;
end;

procedure AppendBytes(var ADestination: TWfcMidiBytes;
  const ASource: TWfcMidiBytes);
var
  I, LOld: Integer;
begin
  if Length(ASource) > High(Integer) - Length(ADestination) then
    raise EEnsembleStudioMidiStream.Create('test byte fixture exceeds Integer');
  LOld := Length(ADestination);
  SetLength(ADestination, LOld + Length(ASource));
  for I := 0 to High(ASource) do ADestination[LOld + I] := ASource[I];
end;

procedure HashByte(var AHash: Cardinal; const AByte: Byte);
{$PUSH}{$Q-}
var
  LValue: Cardinal;
begin
  LValue := AHash xor Cardinal(AByte);
  AHash := (LValue + (LValue shl 1) + (LValue shl 4) +
    (LValue shl 7) + (LValue shl 8) + (LValue shl 24)) and
    Cardinal($FFFFFFFF);
end;
{$POP}

function ByteSignature(const ABytes: TWfcMidiBytes): Cardinal;
var
  I: Integer;
begin
  Result := Cardinal($811C9DC5);
  for I := 0 to High(ABytes) do HashByte(Result, ABytes[I]);
end;

function BytesEqual(const ALeft, ARight: TWfcMidiBytes): Boolean;
var
  I: Integer;
begin
  if Length(ALeft) <> Length(ARight) then Exit(False);
  for I := 0 to High(ALeft) do
    if ALeft[I] <> ARight[I] then Exit(False);
  Result := True;
end;

function Replay(const APlan: TEnsembleStudioMidiPlan;
  out ABlocks: Integer): TWfcMidiBytes;
var
  LBlock, LSaved: TWfcMidiBytes;
  LStep: TWfcMusicArrangementStep;
  LStream: TEnsembleStudioMidiStream;
begin
  Result := nil;
  ABlocks := 0;
  LSaved := nil;
  LStream := TEnsembleStudioMidiStream.Create(APlan);
  try
    repeat
      LStep := LStream.NextBytes(LBlock);
      case LStep of
        wmaspProduced:
          begin
            Check((Length(LBlock) > 0) and
              (Length(LBlock) <= WFC_MIDI_STREAM_BLOCK_BYTES),
              'MIDI replay returns one bounded byte block');
            if ABlocks = 0 then
            begin
              LSaved := Copy(LBlock, 0, Length(LBlock));
              if Length(LBlock) > 0 then LBlock[0] := LBlock[0] xor $FF;
              Check((Length(LSaved) > 0) and (LSaved[0] = Ord('M')),
                'caller-owned first block begins with an immutable MThd copy');
              LBlock := LSaved;
            end;
            AppendBytes(Result, LBlock);
            Inc(ABlocks);
          end;
        wmaspFailed:
          raise EEnsembleStudioMidiStream.Create(LStream.Failure);
        wmaspCancelled:
          raise EEnsembleStudioMidiStream.Create('unexpected replay cancellation');
      end;
    until LStep = wmaspCompleted;
    Check((LStream.Status = wmasCompleted) and
      (LStream.TickCount = APlan.EndTick) and
      (LStream.EmittedBytes = APlan.FileByteCount) and
      (LStream.FramesProcessed = APlan.FrameCount),
      'replay completes the exact planned tick, byte, and frame counts');
    Check((LStream.NextBytes(LBlock) = wmaspCompleted) and (LBlock = nil),
      'completed MIDI replay is terminal and clears output');
  finally
    LStream.Free;
  end;
end;

procedure CheckDecoded(const ABytes: TWfcMidiBytes;
  const AEndTick: Cardinal);
var
  I: Integer;
  LAbsolute: Cardinal;
  LFile: TWfcMidiFile;
begin
  LFile := DecodeWfcMidiFile(ABytes);
  Check((LFile.Format = 0) and (LFile.TicksPerQuarter = 480) and
    (Length(LFile.Tracks) = 1),
    'independent SMF decoder sees format 0, TPQ 480, and one track');
  Check((Length(LFile.Tracks[0].Events) >= 2) and
    (LFile.Tracks[0].Events[0].MetaType = $51) and
    (LFile.Tracks[0].Events[1].MetaType = $58),
    'tempo 500000 and 4/4 meter metadata lead the track');
  LAbsolute := 0;
  for I := 0 to High(LFile.Tracks[0].Events) do
    Inc(LAbsolute, LFile.Tracks[0].Events[I].DeltaTicks);
  Inc(LAbsolute, LFile.Tracks[0].EndDeltaTicks);
  Check(LAbsolute = AEndTick,
    'decoded event deltas end at the exact planned duration');
end;

procedure TestPlanAndReplay;
var
  LBlocks1, LBlocks2: Integer;
  LBytes1, LBytes2: TWfcMidiBytes;
  LOptions: TWfcMusicEnsembleMidiOptions;
  LPlan1, LPlan2: TEnsembleStudioMidiPlan;
begin
  LPlan1 := MakePlan('5.25', 0, 5);
  LPlan2 := MakePlan('5.25', 0, 5);
  try
    Check((LPlan1.EndTick = 5040) and (LPlan1.FrameCount = 21) and
      (LPlan1.SegmentsProduced = 5) and (LPlan1.SeamHoldCount = 5),
      'counting pass covers the continued five-segment frame stream');
    Check((LPlan1.TrackByteCount = 288) and
      (LPlan1.FileByteCount = 310) and
      (LPlan1.EventCount = 67) and (LPlan1.BridgeCount = 0) and
      (LPlan1.MidiSignature = Cardinal($B4BC447C)) and
      (LPlan1.FrameSignature = Cardinal($297928E9)),
      'counting pass preserves the deterministic MIDI plan golden');
    Check((LPlan1.EndTick = LPlan2.EndTick) and
      (LPlan1.TrackByteCount = LPlan2.TrackByteCount) and
      (LPlan1.EventCount = LPlan2.EventCount) and
      (LPlan1.BridgeCount = LPlan2.BridgeCount) and
      (LPlan1.MidiSignature = LPlan2.MidiSignature) and
      (LPlan1.FrameSignature = LPlan2.FrameSignature),
      'independent planning replay has identical counts and fingerprints');
    LOptions := LPlan1.CopyMidiOptions;
    Check((LOptions.TicksPerQuarter = 480) and
      (LOptions.TempoMicrosecondsPerQuarter = 500000) and
      (LOptions.MeterNumerator = 4) and
      (LOptions.MeterDenominatorPower = 2) and
      (Length(LOptions.Channels) = 3) and
      (LOptions.Channels[0] = 0) and (LOptions.Channels[1] = 1) and
      (LOptions.Channels[2] = 2), 'plan preserves the explicit MIDI policy');
    LOptions.Channels[0] := 15;
    LOptions := LPlan1.CopyMidiOptions;
    Check(LOptions.Channels[0] = 0,
      'copied MIDI options cannot mutate the immutable plan');

    LBytes1 := Replay(LPlan1, LBlocks1);
    LBytes2 := Replay(LPlan2, LBlocks2);
    Check((Length(LBytes1) = LPlan1.FileByteCount) and
      (LPlan1.FileByteCount = LPlan1.TrackByteCount +
        WFC_MIDI_STREAM_HEADER_BYTES),
      'file byte preflight includes the exact format-0 header');
    Check((LBlocks1 = LBlocks2) and (LBlocks1 > 1) and
      BytesEqual(LBytes1, LBytes2),
      'two replay passes produce identical bounded byte sequences');
    Check(ByteSignature(LBytes1) = Cardinal($9A9C3708),
      'replayed format-0 file preserves its deterministic byte golden');
    CheckDecoded(LBytes1, 5040);
    WriteLn('MIDI demo values: bytes=', Length(LBytes1),
      ' events=', LPlan1.EventCount, ' bridges=', LPlan1.BridgeCount,
      ' midi=', IntToHex(LPlan1.MidiSignature, 8),
      ' frames=', IntToHex(LPlan1.FrameSignature, 8),
      ' file=', IntToHex(ByteSignature(LBytes1), 8));
  finally
    LPlan2.Free;
    LPlan1.Free;
  end;
end;

procedure TestCancellationAndOwnership;
var
  LBlock: TWfcMidiBytes;
  LOptions: TEnsembleStudioStreamOptions;
  LPlan: TEnsembleStudioMidiPlan;
  LPlanner: TEnsembleStudioMidiPlanner;
  LRejected: Boolean;
  LStream: TEnsembleStudioMidiStream;
begin
  LOptions := DefaultEnsembleStudioStreamOptions;
  LOptions.SegmentCellCount := 5;
  LPlanner := TEnsembleStudioMidiPlanner.Create(
    PlanEnsembleStudioFrames('1.5'), LOptions);
  try
    Check(LPlanner.Next = wmaspProduced,
      'planning cancellation fixture consumes one frame');
    LPlanner.Cancel;
    Check((LPlanner.Next = wmaspCancelled) and
      (LPlanner.Status = wmasCancelled),
      'planning cancellation is terminal');
    LRejected := False;
    try
      LPlan := LPlanner.DetachPlan;
      LPlan.Free;
    except
      on E: EEnsembleStudioMidiStream do LRejected := True;
    end;
    Check(LRejected, 'cancelled planning exposes no partial plan');
  finally
    LPlanner.Free;
  end;

  LPlan := MakePlan('1.5', 0, 5);
  LStream := TEnsembleStudioMidiStream.Create(LPlan);
  LPlan.Free;
  try
    Check(LStream.NextBytes(LBlock) = wmaspProduced,
      'replay owns a strong copy after caller frees the plan');
    LStream.Cancel;
    Check((LStream.NextBytes(LBlock) = wmaspCancelled) and
      (LBlock = nil) and (LStream.Status = wmasCancelled),
      'replay cancellation discards pending bytes without completion');
  finally
    LStream.Free;
  end;
end;

begin
  try
    TestPlanAndReplay;
    TestCancellationAndOwnership;
    WriteLn('Ensemble MIDI stream demo checks: ', Checks - Failures,
      '/', Checks);
    if Failures <> 0 then Halt(1);
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
