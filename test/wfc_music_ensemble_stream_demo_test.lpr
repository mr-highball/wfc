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
program wfc_music_ensemble_stream_demo_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}
  wfc_browser_test_host,
  browser_ensemble_stream,
  {$ENDIF}
  SysUtils,
  wfc,
  wfc_music_audio,
  wfc_music_audio_stream,
  wfc_music_arrangement,
  wfc_music_ensemble,
  wfc_music_ensemble_audio,
  ensemble_studio_stream;

var
  Checks, Failures: Integer;
  {$IFDEF PAS2JS}
  BrowserStreamController: TBrowserEnsembleStreamController;
  {$ENDIF}

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then
  begin
    Inc(Failures);
    WriteLn('FAIL: ', AMessage);
  end;
end;

procedure ExpectPlanFailure(const AText: String);
var
  LRejected: Boolean;
  LPlan: TEnsembleStudioStreamPlan;
begin
  LRejected := False;
  try
    LPlan := PlanEnsembleStudioStream(AText);
    if LPlan.ExpectedFrames = -1 then WriteLn('unreachable');
  except
    on E: EEnsembleStudioStream do LRejected := True;
  end;
  Check(LRejected, 'invalid duration rejected: ' + AText);
end;

procedure TestPlans;
var
  LPlan: TEnsembleStudioStreamPlan;
begin
  LPlan := PlanEnsembleStudioStream('4');
  Check((LPlan.RequestedText = '4') and (LPlan.RequestedTicks = 3840),
    'whole seconds preserve requested ticks');
  Check((LPlan.ActualTicks = 3840) and (LPlan.CellCount = 16) and
    (LPlan.ExpectedFrames = 176400), 'four-second plan is exact');
  Check(EnsembleStudioStreamSecondsText(LPlan.ActualTicks) = '4',
    'whole actual duration text');

  LPlan := PlanEnsembleStudioStream(' 0.001 ');
  Check((LPlan.RequestedText = '0.001') and
    (LPlan.RequestedTicks = 1), 'millisecond request rounds to one score tick');
  Check((LPlan.ActualTicks = 240) and (LPlan.CellCount = 1) and
    (LPlan.ExpectedFrames = 11025), 'tiny duration rounds to one quantum cell');
  Check(EnsembleStudioStreamSecondsText(LPlan.ActualTicks) = '0.25',
    'quantum-rounded duration is disclosed');

  LPlan := PlanEnsembleStudioStream('1.001');
  Check((LPlan.RequestedTicks = 961) and (LPlan.ActualTicks = 1200) and
    (LPlan.CellCount = 5) and (LPlan.ExpectedFrames = 55125),
    'fractional duration rounds only to the next cell');
  Check(EnsembleStudioStreamSecondsText(LPlan.ActualTicks) = '1.25',
    'fractional actual duration text');

  LPlan := PlanEnsembleStudioStream('0.00001');
  Check((LPlan.RequestedTicks = 1) and (LPlan.ActualTicks = 240),
    'arbitrary fractional precision rounds exactly without a duration cap');
  LPlan := PlanEnsembleStudioStream('0.333333333333333333');
  Check(LPlan.RequestedTicks = 320,
    'long fraction just below one third rounds to exact tick 320');
  LPlan := PlanEnsembleStudioStream('0.333333333333333334');
  Check(LPlan.RequestedTicks = 321,
    'long fraction just above one third rounds to tick 321');
  LPlan := PlanEnsembleStudioStream('1.000000000000000001');
  Check(LPlan.RequestedTicks = 961,
    'long nonzero tail beyond a whole second is retained');

  ExpectPlanFailure('');
  ExpectPlanFailure('0');
  ExpectPlanFailure('.5');
  ExpectPlanFailure('1.');
  ExpectPlanFailure('2 seconds');
  ExpectPlanFailure('99999999999999999999');
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

procedure HashSamples(var AHash: Cardinal;
  const ASamples: TWfcMusicPcm16Samples);
var
  I, LValue: Integer;
begin
  for I := 0 to High(ASamples) do
  begin
    LValue := ASamples[I];
    if LValue < 0 then Inc(LValue, 65536);
    HashByte(AHash, Byte(LValue and $FF));
    HashByte(AHash, Byte(LValue shr 8));
  end;
end;

procedure TestFrameSource;
var
  I: Integer;
  LFirstText: String;
  LFrame, LPrevious: TWfcMusicEnsembleFrame;
  LFramePlan: TEnsembleStudioFramePlan;
  LOptions: TEnsembleStudioStreamOptions;
  LRejected: Boolean;
  LStep: TWfcMusicArrangementStep;
  LStream: TEnsembleStudioFrameStream;
begin
  LFramePlan := PlanEnsembleStudioFrames('1000000000000');
  Check((LFramePlan.RequestedTicks = 960000000000000) and
    (LFramePlan.CellCount = 4000000000000),
    'transport-neutral frame plan is not restricted by the WAVE envelope');
  LRejected := False;
  try
    PlanEnsembleStudioStream('1000000000000');
  except
    on E: EEnsembleStudioStream do LRejected := True;
  end;
  Check(LRejected, 'WAVE planning retains its independent frame envelope');

  LFramePlan := PlanEnsembleStudioFrames('5.25');
  LOptions := DefaultEnsembleStudioStreamOptions;
  LOptions.SegmentCellCount := 5;
  LStream := TEnsembleStudioFrameStream.Create(LFramePlan, LOptions);
  try
    I := 0;
    LPrevious := Default(TWfcMusicEnsembleFrame);
    repeat
      LStep := LStream.NextFrame(LFrame);
      if LStep = wmaspProduced then
      begin
        Check(Length(LFrame.Voices) = 3,
          'detached frame exposes all three ordered voices');
        if I = 0 then
        begin
          Check(WfcMusicEnsembleFrameCanStart(LFrame),
            'first streamed frame is a legal start');
          LFirstText := EncodeWfcMusicEnsembleFrame(LFrame);
          LPrevious := MakeWfcMusicEnsembleFrame(LFrame.Voices);
          if Length(LFrame.Voices[0].Tones) > 0 then
            LFrame.Voices[0].Tones[0].Pitch := 0;
        end
        else
        begin
          Check(WfcMusicEnsembleFrameCanFollow(LPrevious, LFrame),
            'successive streamed frames preserve temporal continuity');
          LPrevious := MakeWfcMusicEnsembleFrame(LFrame.Voices);
        end;
        Inc(I);
      end;
    until LStep <> wmaspProduced;
    Check((LStep = wmaspCompleted) and (I = 21) and
      (LStream.ProducedTicks = 5040),
      'frame source reaches the exact transport-neutral duration');
    Check((LStream.SegmentsProduced = 5) and
      (LStream.SeamHoldCount = 5) and
      (LStream.LastSegmentSignature = Cardinal($433EDED7)),
      'frame source retains segment and held-seam evidence');
    Check((LStream.NextFrame(LFrame) = wmaspCompleted) and
      (LFrame.Voices = nil), 'frame completion is terminal and clears output');
  finally
    LStream.Free;
  end;

  LStream := TEnsembleStudioFrameStream.Create(LFramePlan, LOptions);
  try
    Check((LStream.NextFrame(LFrame) = wmaspProduced) and
      (EncodeWfcMusicEnsembleFrame(LFrame) = LFirstText),
      'caller mutation cannot alter a fresh deterministic frame replay');
    LStream.Cancel;
    Check((LStream.NextFrame(LFrame) = wmaspCancelled) and
      (LFrame.Voices = nil) and (LStream.Status = wmasCancelled),
      'frame-source cancellation is terminal and clears output');
  finally
    LStream.Free;
  end;
end;

procedure Drain(const ASeconds: String; const ASegmentCells: Integer;
  out AHash: Cardinal; out AFrames, ABlocks, ASegments,
  ASeamHolds: TWfcMusicArrangementWide; out ALastSignature: Cardinal);
var
  LOptions: TEnsembleStudioStreamOptions;
  LPlan: TEnsembleStudioStreamPlan;
  LSamples: TWfcMusicPcm16Samples;
  LStep: TWfcMusicArrangementStep;
  LStream: TEnsembleStudioPcmStream;
begin
  LPlan := PlanEnsembleStudioStream(ASeconds);
  LOptions := DefaultEnsembleStudioStreamOptions;
  LOptions.SegmentCellCount := ASegmentCells;
  LStream := TEnsembleStudioPcmStream.Create(LPlan, LOptions);
  AHash := Cardinal(2166136261);
  AFrames := 0;
  ABlocks := 0;
  try
    repeat
      LStep := LStream.NextSamples(LSamples);
      case LStep of
        wmaspProduced:
          begin
            Check((Length(LSamples) > 0) and
              (Length(LSamples) <= WFC_MUSIC_ENSEMBLE_AUDIO_BLOCK_FRAMES),
              'pull block stays within fixed PCM bound');
            HashSamples(AHash, LSamples);
            Inc(AFrames, Length(LSamples));
            Inc(ABlocks);
          end;
        wmaspFailed:
          raise EEnsembleStudioStream.Create(LStream.Failure);
        wmaspCancelled:
          raise EEnsembleStudioStream.Create('unexpected cancellation');
        wmaspCompleted: ;
      end;
    until LStep = wmaspCompleted;
    Check(LSamples = nil, 'completed pull clears output');
    Check((LStream.Status = wmasCompleted) and
      (LStream.EmittedFrames = LPlan.ExpectedFrames) and
      (LStream.RenderedFrames = LPlan.ExpectedFrames),
      'completion accounts for every planned frame');
    Check(LStream.ProducedTicks = LPlan.ActualTicks,
      'generation reaches the exact rounded tick count');
    Check(LStream.LastNegotiationStatus = gnsSolved,
      'last segment has a solved terminal report');
    Check(LStream.NextSamples(LSamples) = wmaspCompleted,
      'completed pull is idempotent');
    Check(LSamples = nil, 'idempotent completion returns no stale block');
    ASegments := LStream.SegmentsProduced;
    ASeamHolds := LStream.SeamHoldCount;
    ALastSignature := LStream.LastSegmentSignature;
  finally
    LStream.Free;
  end;
end;

procedure TestStreaming;
var
  H1, H2, Last1, Last2: Cardinal;
  Frames1, Frames2, Blocks1, Blocks2, Segments1, Segments2,
  Holds1, Holds2: TWfcMusicArrangementWide;
begin
  Drain('4', 5, H1, Frames1, Blocks1, Segments1, Holds1, Last1);
  Drain('4', 5, H2, Frames2, Blocks2, Segments2, Holds2, Last2);
  Check((Frames1 = 176400) and (Frames2 = Frames1),
    'four-second PCM extent is exact');
  Check((Segments1 = 4) and (Segments2 = Segments1),
    'five-cell working segments include one short final segment');
  Check((Holds1 > 0) and (Holds2 = Holds1),
    'held voices genuinely cross non-bar segment seams');
  Check((H1 = H2) and (Last1 = Last2),
    'independent stream replay is byte- and segment-deterministic');
  Check((Blocks1 = Blocks2) and (Blocks1 > 80),
    'stream uses many bounded pulls rather than one song buffer');
  Check((H1 = Cardinal($7230FCDD)) and
    (Last1 = Cardinal($3705EC24)) and (Holds1 = 4),
    'versioned four-second PCM, segment, and seam goldens');

  Drain('0.001', 7, H2, Frames2, Blocks2, Segments2, Holds2, Last2);
  Check((Frames2 = 11025) and (Segments2 = 1) and (Holds2 = 0),
    'one-cell final segment renders without padding');
  Drain('5.25', 5, H2, Frames2, Blocks2, Segments2, Holds2, Last2);
  Check((Frames2 = 231525) and (Segments2 = 5) and (Holds2 > 0),
    'stream continues beyond the finite authored sample');
  Check((H2 = Cardinal($8701A89B)) and
    (Last2 = Cardinal($433EDED7)) and (Holds2 = 5),
    'continued-stream PCM, segment, and seam goldens');
end;

procedure TestCancelAndForgedInput;
var
  LOptions: TEnsembleStudioStreamOptions;
  LPlan: TEnsembleStudioStreamPlan;
  LSamples: TWfcMusicPcm16Samples;
  LStream: TEnsembleStudioPcmStream;
  LRejected: Boolean;
begin
  LPlan := PlanEnsembleStudioStream('4');
  LOptions := DefaultEnsembleStudioStreamOptions;
  LStream := TEnsembleStudioPcmStream.Create(LPlan, LOptions);
  try
    Check(LStream.NextSamples(LSamples) = wmaspProduced,
      'cancellation fixture produces one bounded block');
    LStream.Cancel;
    Check((LStream.Status = wmasCancelled) and
      (LStream.NextSamples(LSamples) = wmaspCancelled) and
      (LSamples = nil), 'cancellation discards pending PCM terminally');
    LStream.Cancel;
    Check(LStream.Status = wmasCancelled, 'cancellation is idempotent');
  finally
    LStream.Free;
  end;

  Inc(LPlan.ExpectedFrames);
  LRejected := False;
  try
    LStream := TEnsembleStudioPcmStream.Create(LPlan, LOptions);
    LStream.Free;
  except
    on E: EEnsembleStudioStream do LRejected := True;
  end;
  Check(LRejected, 'forged frame preflight is rejected before construction');

  LPlan := PlanEnsembleStudioStream('4');
  LPlan.RequestedText := '6';
  LRejected := False;
  try
    LStream := TEnsembleStudioPcmStream.Create(LPlan, LOptions);
    LStream.Free;
  except
    on E: EEnsembleStudioStream do LRejected := True;
  end;
  Check(LRejected, 'forged duration text cannot bless stale numeric fields');

  LPlan := PlanEnsembleStudioStream('4');
  LOptions.SegmentCellCount := 0;
  LRejected := False;
  try
    LStream := TEnsembleStudioPcmStream.Create(LPlan, LOptions);
    LStream.Free;
  except
    on E: EEnsembleStudioStream do LRejected := True;
  end;
  Check(LRejected, 'invalid working segment size rejects');
end;

{$IFDEF PAS2JS}
function MalformedOptionNumber(const AIndex: Integer): NativeInt;
begin
  asm
    if (AIndex === 0) Result = 1.5;
    else if (AIndex === 1) Result = NaN;
    else if (AIndex === 2) Result = Infinity;
    else Result = 2147483648;
  end;
end;

procedure TestBrowserMalformedOptions;
var
  I: Integer;
  LOptions: TEnsembleStudioStreamOptions;
  LPlan: TEnsembleStudioStreamPlan;
  LRejected: Boolean;
  LStream: TEnsembleStudioPcmStream;
begin
  LPlan := PlanEnsembleStudioStream('0.25');
  for I := 0 to 3 do
  begin
    LOptions := DefaultEnsembleStudioStreamOptions;
    LOptions.MaxBacktracks := MalformedOptionNumber(I);
    LRejected := False;
    try
      LStream := TEnsembleStudioPcmStream.Create(LPlan, LOptions);
      LStream.Free;
    except
      on E: EEnsembleStudioStream do LRejected := True;
    end;
    Check(LRejected, 'malformed browser local allowance rejects ' + IntToStr(I));

    LOptions := DefaultEnsembleStudioStreamOptions;
    LOptions.MaxPassBacktracks := MalformedOptionNumber(I);
    LRejected := False;
    try
      LStream := TEnsembleStudioPcmStream.Create(LPlan, LOptions);
      LStream.Free;
    except
      on E: EEnsembleStudioStream do LRejected := True;
    end;
    Check(LRejected, 'malformed browser pass allowance rejects ' + IntToStr(I));
  end;
end;
{$ENDIF}

begin
  try
    TestPlans;
    TestFrameSource;
    TestStreaming;
    TestCancelAndForgedInput;
    {$IFDEF PAS2JS}
    TestBrowserMalformedOptions;
    {$ENDIF}
    WriteLn('Ensemble stream demo checks: ', Checks - Failures, '/', Checks);
    if Failures <> 0 then Halt(1);
    {$IFDEF PAS2JS}
    InstallEnsembleStreamBrowserTestFixture;
    BrowserStreamController := TBrowserEnsembleStreamController.Create;
    BrowserStreamController.Run;
    BrowserStreamController.RunSelfTest;
    WriteLn('Ensemble stream async browser checks: started');
    {$ENDIF}
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
