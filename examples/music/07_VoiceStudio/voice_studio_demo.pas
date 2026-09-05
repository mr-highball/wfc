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
unit voice_studio_demo;

{$mode delphi}{$H+}

interface

function VoiceStudioSelfTest: Integer;

implementation

uses
  SysUtils,
  wfc,
  wfc_midi_smf,
  wfc_midi_stream,
  wfc_music_audio,
  wfc_music_arrangement,
  wfc_music_ensemble,
  wfc_music_ensemble_audio,
  wfc_music_sequence,
  voice_studio_corpus,
  voice_studio_stream,
  voice_studio_midi_stream;

procedure Need(const ACondition: Boolean; const AMessage: String;
  var AChecks: Integer);
begin
  Inc(AChecks);
  if not ACondition then
    raise EVoiceStudioStream.Create('self-test: ' + AMessage);
end;

procedure HashByte(var AHash: Cardinal; const AByte: Byte);
{$PUSH}{$Q-}
var V: Cardinal;
begin
  V := AHash xor Cardinal(AByte);
  AHash := (V + (V shl 1) + (V shl 4) + (V shl 7) +
    (V shl 8) + (V shl 24)) and Cardinal($FFFFFFFF);
end;
{$POP}

procedure HashText(var AHash: Cardinal; const AText: String);
var I: Integer;
begin
  for I := 1 to Length(AText) do HashByte(AHash, Byte(Ord(AText[I])));
end;

procedure AppendBytes(var ADestination: TWfcMidiBytes;
  const ASource: TWfcMidiBytes);
var I, N: Integer;
begin
  N := Length(ADestination);
  if Length(ASource) > High(Integer) - N then
    raise EVoiceStudioStream.Create('self-test MIDI fixture is too large');
  SetLength(ADestination, N + Length(ASource));
  for I := 0 to High(ASource) do ADestination[N + I] := ASource[I];
end;

procedure TestDuration(var AChecks: Integer);
var
  P: TVoiceStudioFramePlan;
  W: TVoiceStudioWavePlan;
  Raised: Boolean;
begin
  P := PlanVoiceStudioFrames('5.25');
  Need((P.RequestedTicks = 5040) and (P.ActualTicks = 5040) and
    (P.CellCount = 21), 'exact duration plan', AChecks);
  P := PlanVoiceStudioFrames('0.00001');
  Need((P.RequestedTicks = 1) and (P.ActualTicks = 240) and
    (P.CellCount = 1), 'arbitrary fractional precision rounds to a cell', AChecks);
  P := PlanVoiceStudioFrames('0.333333333333333333333333333333');
  Need((P.RequestedTicks = 320) and (P.ActualTicks = 480),
    'long fraction uses exact bounded decimal multiplication', AChecks);
  W := PlanVoiceStudioWave('5.25');
  Need(W.ExpectedFrames = 231525, 'exact WAVE frame preflight', AChecks);
  Need(VoiceStudioSecondsText(W.ActualTicks) = '5.25',
    'actual duration formatting', AChecks);
  Raised := False;
  try P := PlanVoiceStudioFrames('1e3');
  except on E: EVoiceStudioStream do Raised := True end;
  Need(Raised, 'non-decimal duration rejected', AChecks);
  Raised := False;
  try P := PlanVoiceStudioFrames('0');
  except on E: EVoiceStudioStream do Raised := True end;
  Need(Raised, 'zero duration rejected', AChecks);
end;

function DrainFrameSource(const APlan: TVoiceStudioFramePlan;
  const AOptions: TVoiceStudioStreamOptions; out AHash: Cardinal;
  out ANovel, AShared, ASeams: TWfcMusicArrangementWide;
  out ARoleMask: Cardinal; var AChecks: Integer): Integer;
var
  Cell: TVoiceStudioCell;
  Source: TVoiceStudioFrameStream;
  Step: TWfcMusicArrangementStep;
  I: Integer;
begin
  Result := 0;
  AHash := Cardinal($811C9DC5);
  Source := TVoiceStudioFrameStream.Create(APlan, AOptions);
  try
    repeat
      Step := Source.NextCell(Cell);
      case Step of
        wmaspProduced:
          begin
            Need(Cell.Position = Result, 'monotonic public cell position', AChecks);
            Need(Length(Cell.Frame.Voices) = VOICE_STUDIO_ROLE_COUNT,
              'three independent public voices', AChecks);
            Need((Length(Cell.VoiceTokens) = VOICE_STUDIO_ROLE_COUNT) and
              (Length(Cell.CoverageSuppliers) = VOICE_STUDIO_STEPS),
              'detached layer tokens and witnesses', AChecks);
            for I := 0 to VOICE_STUDIO_ROLE_COUNT - 1 do
              if Cell.Frame.Voices[I].Action <> wmcaRest then
                Need((Cell.Frame.Voices[I].Tones[0].Pitch >=
                    VoiceStudioRoleMinimumPitch(I)) and
                  (Cell.Frame.Voices[I].Tones[
                    High(Cell.Frame.Voices[I].Tones)].Pitch <=
                    VoiceStudioRoleMaximumPitch(I)),
                  'role range ' + IntToStr(I), AChecks);
            HashText(AHash, EncodeWfcMusicEnsembleFrame(Cell.Frame));
            Inc(Result);
          end;
        wmaspCompleted: ;
        wmaspCancelled: raise EVoiceStudioStream.Create('unexpected cancellation');
        wmaspFailed: raise EVoiceStudioStream.Create(Source.Failure);
      end;
    until Step = wmaspCompleted;
    ASeams := Source.SeamHoldCount;
    ANovel := Source.NovelVerticalCount;
    AShared := Source.SharedCoverageCellCount;
    ARoleMask := Source.CoverageRoleMask;
    Need(Source.ProducedTicks = APlan.ActualTicks,
      'source completes exact requested ticks', AChecks);
    Need(Source.SegmentsProduced > 2, 'bounded multi-segment generation', AChecks);
  finally
    Source.Free;
  end;
end;

procedure TestFrames(var AChecks: Integer);
var
  Plan: TVoiceStudioFramePlan;
  Options: TVoiceStudioStreamOptions;
  H1, H2, Mask1, Mask2: Cardinal;
  Novel1, Novel2, Shared1, Shared2, Seams1, Seams2:
    TWfcMusicArrangementWide;
  Count1, Count2: Integer;
  Source: TVoiceStudioFrameStream;
  Cell: TVoiceStudioCell;
begin
  Plan := PlanVoiceStudioFrames('8');
  Options := DefaultVoiceStudioStreamOptions;
  Options.CaptureTrace := True;
  Count1 := DrainFrameSource(Plan, Options, H1, Novel1, Shared1, Seams1,
    Mask1, AChecks);
  Count2 := DrainFrameSource(Plan, Options, H2, Novel2, Shared2, Seams2,
    Mask2, AChecks);
  Need((Count1 = 32) and (Count2 = 32), 'two authored loops streamed', AChecks);
  Need((H1 = H2) and (Novel1 = Novel2) and (Shared1 = Shared2) and
    (Seams1 = Seams2) and (Mask1 = Mask2),
    'same configuration replays deterministically', AChecks);
  Need(Novel1 > 0, 'unseen vertical combinations are generated', AChecks);
  Need(Shared1 > 0, 'harmony witness spans multiple roles', AChecks);
  Need(Mask1 = 7, 'all roles contribute harmony witnesses', AChecks);
  Need(Seams1 > 0, 'held voices continue through five-cell seams', AChecks);

  Source := TVoiceStudioFrameStream.Create(Plan, Options);
  try
    Source.Cancel;
    Need(Source.NextCell(Cell) = wmaspCancelled,
      'frame cancellation is terminal', AChecks);
  finally
    Source.Free;
  end;
end;

procedure TestPcm(var AChecks: Integer);
var
  Plan: TVoiceStudioWavePlan;
  Options: TVoiceStudioStreamOptions;
  Source: TVoiceStudioPcmStream;
  Samples: TWfcMusicPcm16Samples;
  Step: TWfcMusicArrangementStep;
  Count: TWfcMusicEnsembleAudioCount;
  Peak, I: Integer;
  Hash: Cardinal;
  Cell: TVoiceStudioCell;
begin
  Plan := PlanVoiceStudioWave('5.25');
  Options := DefaultVoiceStudioStreamOptions;
  Source := TVoiceStudioPcmStream.Create(Plan, Options);
  Count := 0;
  Peak := 0;
  Hash := Cardinal($811C9DC5);
  try
    repeat
      Step := Source.NextSamples(Samples);
      case Step of
        wmaspProduced:
          begin
            Need((Length(Samples) > 0) and
              (Length(Samples) <= WFC_MUSIC_ENSEMBLE_AUDIO_BLOCK_FRAMES),
              'PCM block is bounded', AChecks);
            Inc(Count, Length(Samples));
            if Length(Samples) > Peak then Peak := Length(Samples);
            for I := 0 to High(Samples) do
            begin
              HashByte(Hash, Byte(Word(Samples[I]) and $FF));
              HashByte(Hash, Byte((Word(Samples[I]) shr 8) and $FF));
            end;
          end;
        wmaspCompleted: ;
        wmaspCancelled: raise EVoiceStudioStream.Create('unexpected PCM cancellation');
        wmaspFailed: raise EVoiceStudioStream.Create(Source.Failure);
      end;
    until Step = wmaspCompleted;
    Need((Count = Plan.ExpectedFrames) and
      (Source.EmittedFrames = Plan.ExpectedFrames),
      'PCM count matches preflight', AChecks);
    Need(Peak <= WFC_MUSIC_ENSEMBLE_AUDIO_BLOCK_FRAMES,
      'only one bounded PCM block is returned', AChecks);
    Need(Source.CopyLatestCell(Cell) and (Cell.Position = Plan.CellCount - 1),
      'latest-cell diagnostics reach final cell', AChecks);
    Need((Source.CoverageRoleMask = 7) and (Hash <> 0),
      'PCM carries independently witnessed voices', AChecks);
  finally
    Source.Free;
  end;
end;

procedure TestMidi(var AChecks: Integer);
var
  Bytes, Block: TWfcMidiBytes;
  FramePlan: TVoiceStudioFramePlan;
  Options: TVoiceStudioStreamOptions;
  Plan: TVoiceStudioMidiPlan;
  Planner: TVoiceStudioMidiPlanner;
  Replay: TVoiceStudioMidiStream;
  Step: TWfcMusicArrangementStep;
  Decoded: TWfcMidiFile;
begin
  FramePlan := PlanVoiceStudioFrames('5.25');
  Options := DefaultVoiceStudioStreamOptions;
  Planner := TVoiceStudioMidiPlanner.Create(FramePlan, Options);
  Plan := nil;
  try
    repeat
      Step := Planner.Next;
      if Step = wmaspFailed then
        raise EVoiceStudioStream.Create(Planner.Failure);
    until Step <> wmaspProduced;
    Need(Step = wmaspCompleted, 'MIDI counting pass completes', AChecks);
    Plan := Planner.DetachPlan;
  finally
    Planner.Free;
  end;
  Replay := TVoiceStudioMidiStream.Create(Plan);
  Plan.Free;
  try
    repeat
      Step := Replay.NextBytes(Block);
      if Step = wmaspProduced then
      begin
        Need(Length(Block) <= WFC_MIDI_STREAM_BLOCK_BYTES,
          'MIDI replay block is bounded', AChecks);
        if Length(Block) > 0 then AppendBytes(Bytes, Block);
      end
      else if Step = wmaspFailed then
        raise EVoiceStudioStream.Create(Replay.Failure);
    until Step <> wmaspProduced;
    Need(Step = wmaspCompleted, 'MIDI replay completes', AChecks);
    Need((Replay.EmittedBytes = Length(Bytes)) and
      (Replay.TickCount = FramePlan.ActualTicks),
      'MIDI byte and tick counts match', AChecks);
    Decoded := DecodeWfcMidiFile(Bytes);
    Need((Decoded.Format = 0) and (Decoded.TicksPerQuarter = VOICE_STUDIO_TPQ),
      'MIDI format and time base decode independently', AChecks);
  finally
    Replay.Free;
  end;
end;

function VoiceStudioSelfTest: Integer;
begin
  Result := 0;
  TestDuration(Result);
  TestFrames(Result);
  TestPcm(Result);
  TestMidi(Result);
end;

end.
