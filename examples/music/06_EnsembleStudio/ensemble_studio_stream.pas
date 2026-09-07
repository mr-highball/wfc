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
unit ensemble_studio_stream;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  ensemble_studio_profiles,
  wfc_music_audio,
  wfc_music_audio_stream,
  wfc_music_arrangement,
  wfc_music_ensemble,
  wfc_music_ensemble_passes,
  wfc_music_ensemble_audio,
  wfc_music_ensemble_stream;

const
  ENSEMBLE_STUDIO_STREAM_VERSION = 1;
  ENSEMBLE_STUDIO_STREAM_SAMPLE_RATE = 44100;
  ENSEMBLE_STUDIO_STREAM_TEMPO = 500000;
  ENSEMBLE_STUDIO_STREAM_TPQ = 480;
  ENSEMBLE_STUDIO_STREAM_QUANTUM = 240;
  ENSEMBLE_STUDIO_STREAM_DEFAULT_SEGMENT_CELLS = 5;
  ENSEMBLE_STUDIO_STREAM_VOICE_COUNT = 3;

type
  EEnsembleStudioStream = class(Exception);

  TEnsembleStudioStreamOptions = record
    Seed: TGraphSeed;
    Profile: TEnsembleStudioProfile;
    SegmentCellCount: Integer;
    MaxBacktracks: Integer;
    MaxPassBacktracks: Integer;
    CaptureTrace: Boolean;
  end;

  TEnsembleStudioFramePlan = record
    RequestedText: String;
    RequestedTicks: TWfcMusicArrangementWide;
    ActualTicks: TWfcMusicArrangementWide;
    CellCount: TWfcMusicArrangementWide;
  end;

  TEnsembleStudioStreamPlan = record
    RequestedText: String;
    RequestedTicks: TWfcMusicArrangementWide;
    ActualTicks: TWfcMusicArrangementWide;
    CellCount: TWfcMusicArrangementWide;
    ExpectedFrames: TWfcMusicEnsembleAudioCount;
  end;

  { Transport-neutral deterministic frame source. It owns the authored models,
    generator, and at most one current segment. Each produced frame is detached
    for the caller; no frame timeline grows with total duration. }
  TEnsembleStudioFrameStream = class
  strict private
    FPlan: TEnsembleStudioFramePlan;
    FOptions: TEnsembleStudioStreamOptions;
    FModels: TWfcMusicEnsembleModels;
    FGenerator: TWfcMusicEnsembleStream;
    FSegment: TWfcMusicEnsembleSegment;
    FFrames: TWfcMusicEnsembleFrames;
    FFrameIndex: Integer;
    FStatus: TWfcMusicArrangementStatus;
    FFailure: String;
    FSegmentsProduced, FSeamHoldCount: TWfcMusicArrangementWide;
    FLastSegmentSignature: Cardinal;
    FLastNegotiationStatus: TGraphNegotiationStatus;
    FLastTranscriptHash: TGraphTraceSignature;
    procedure ClearSegment;
    procedure Fail(const AMessage: String);
    function GetProducedTicks: TWfcMusicArrangementWide;
  public
    constructor Create(const APlan: TEnsembleStudioFramePlan;
      const AOptions: TEnsembleStudioStreamOptions);
    destructor Destroy; override;
    function NextFrame(out AFrame: TWfcMusicEnsembleFrame):
      TWfcMusicArrangementStep;
    procedure Cancel;
    property Plan: TEnsembleStudioFramePlan read FPlan;
    property Options: TEnsembleStudioStreamOptions read FOptions;
    property Status: TWfcMusicArrangementStatus read FStatus;
    property Failure: String read FFailure;
    property ProducedTicks: TWfcMusicArrangementWide read GetProducedTicks;
    property SegmentsProduced: TWfcMusicArrangementWide read FSegmentsProduced;
    property SeamHoldCount: TWfcMusicArrangementWide read FSeamHoldCount;
    property LastSegmentSignature: Cardinal read FLastSegmentSignature;
    property LastNegotiationStatus: TGraphNegotiationStatus
      read FLastNegotiationStatus;
    property LastTranscriptHash: TGraphTraceSignature
      read FLastTranscriptHash;
  end;

  { Composes the transport-neutral frame source with the stateful PCM renderer.
    Each successful pull returns caller-owned PCM for at most
    WFC_MUSIC_ENSEMBLE_AUDIO_BLOCK_FRAMES frames. No PCM or score storage grows
    with total duration. }
  TEnsembleStudioPcmStream = class
  strict private
    FPlan: TEnsembleStudioStreamPlan;
    FOptions: TEnsembleStudioStreamOptions;
    FFrameStream: TEnsembleStudioFrameStream;
    FRenderer: TWfcMusicEnsembleAudioRenderer;
    FStatus: TWfcMusicArrangementStatus;
    FFailure: String;
    procedure Fail(const AMessage: String);
    function GetProducedTicks: TWfcMusicArrangementWide;
    function GetSegmentsProduced: TWfcMusicArrangementWide;
    function GetSeamHoldCount: TWfcMusicArrangementWide;
    function GetRenderedFrames: TWfcMusicEnsembleAudioCount;
    function GetEmittedFrames: TWfcMusicEnsembleAudioCount;
    function GetLastSegmentSignature: Cardinal;
    function GetLastNegotiationStatus: TGraphNegotiationStatus;
    function GetLastTranscriptHash: TGraphTraceSignature;
  public
    constructor Create(const APlan: TEnsembleStudioStreamPlan;
      const AOptions: TEnsembleStudioStreamOptions);
    destructor Destroy; override;
    function NextSamples(out ASamples: TWfcMusicPcm16Samples):
      TWfcMusicArrangementStep;
    procedure Cancel;
    property Plan: TEnsembleStudioStreamPlan read FPlan;
    property Options: TEnsembleStudioStreamOptions read FOptions;
    property Status: TWfcMusicArrangementStatus read FStatus;
    property Failure: String read FFailure;
    property ProducedTicks: TWfcMusicArrangementWide read GetProducedTicks;
    property SegmentsProduced: TWfcMusicArrangementWide read GetSegmentsProduced;
    property SeamHoldCount: TWfcMusicArrangementWide read GetSeamHoldCount;
    property RenderedFrames: TWfcMusicEnsembleAudioCount read GetRenderedFrames;
    property EmittedFrames: TWfcMusicEnsembleAudioCount read GetEmittedFrames;
    property LastSegmentSignature: Cardinal read GetLastSegmentSignature;
    property LastNegotiationStatus: TGraphNegotiationStatus
      read GetLastNegotiationStatus;
    property LastTranscriptHash: TGraphTraceSignature
      read GetLastTranscriptHash;
  end;

function DefaultEnsembleStudioStreamOptions: TEnsembleStudioStreamOptions;
function PlanEnsembleStudioFrames(
  const ASeconds: String): TEnsembleStudioFramePlan;
function PlanEnsembleStudioStream(
  const ASeconds: String): TEnsembleStudioStreamPlan;
function EnsembleStudioStreamSecondsText(
  const ATicks: TWfcMusicArrangementWide): String;

implementation

uses
  wfc_music_sequence, ensemble_studio_planning;

const
  TICKS_PER_SECOND = 960;
  FRAMES_PER_CELL = 11025;

procedure StreamError(const AMessage: String);
begin
  raise EEnsembleStudioStream.Create('Ensemble Studio stream: ' + AMessage);
end;

function DefaultEnsembleStudioStreamOptions: TEnsembleStudioStreamOptions;
begin
  Result := Default(TEnsembleStudioStreamOptions);
  Result.SegmentCellCount := ENSEMBLE_STUDIO_STREAM_DEFAULT_SEGMENT_CELLS;
  Result.MaxBacktracks := 256;
  Result.MaxPassBacktracks := 16;
end;

function PlanEnsembleStudioFrames(
  const ASeconds: String): TEnsembleStudioFramePlan;
var
  I, D, FractionDigits, FractionStart, ProductDigit, Carry: Integer;
  Whole, Extra: TWfcMusicArrangementWide;
  HasWholeDigit, HasFractionRemainder, InFraction: Boolean;
  LText: String;
begin
  Result := Default(TEnsembleStudioFramePlan);
  LText := Trim(ASeconds);
  if LText = '' then StreamError('duration seconds are required');
  Whole := 0;
  FractionDigits := 0;
  FractionStart := 0;
  HasWholeDigit := False;
  InFraction := False;
  for I := 1 to Length(LText) do
  begin
    if LText[I] = '.' then
    begin
      if InFraction or not HasWholeDigit then
        StreamError('duration must be a positive decimal number');
      InFraction := True;
      FractionStart := I + 1;
      Continue;
    end;
    if not (LText[I] in ['0'..'9']) then
      StreamError('duration must be a positive decimal number');
    D := Ord(LText[I]) - Ord('0');
    if InFraction then
      Inc(FractionDigits)
    else
    begin
      HasWholeDigit := True;
      if Whole > ((WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER div
          TICKS_PER_SECOND) - D) div 10 then
        StreamError('duration exceeds exact tick representation');
      Whole := Whole * 10 + D;
    end;
  end;
  if InFraction and (FractionDigits = 0) then
    StreamError('duration fraction is empty');
  { Multiply the arbitrarily long fractional decimal by 960 from right to
    left. Carry is the whole-tick quotient; any discarded decimal product
    digit means the exact value must be rounded up. All intermediates remain
    below 9600, independent of input length. }
  Carry := 0;
  HasFractionRemainder := False;
  if InFraction then
    for I := Length(LText) downto FractionStart do
    begin
      ProductDigit := (Ord(LText[I]) - Ord('0')) * TICKS_PER_SECOND + Carry;
      if (ProductDigit mod 10) <> 0 then HasFractionRemainder := True;
      Carry := ProductDigit div 10;
    end;
  Extra := Carry;
  if HasFractionRemainder then Inc(Extra);
  Result.RequestedTicks := Whole * TICKS_PER_SECOND;
  if Result.RequestedTicks >
      WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER - Extra then
    StreamError('duration exceeds exact tick representation');
  Result.RequestedTicks := Result.RequestedTicks + Extra;
  if Result.RequestedTicks < 1 then
    StreamError('duration must be greater than zero');
  Result.ActualTicks := ResolveWfcMusicArrangementTicks(
    Result.RequestedTicks, ENSEMBLE_STUDIO_STREAM_QUANTUM, wmarCeilToCell);
  Result.CellCount := Result.ActualTicks div ENSEMBLE_STUDIO_STREAM_QUANTUM;
  Result.RequestedText := LText;
end;

function PlanEnsembleStudioStream(
  const ASeconds: String): TEnsembleStudioStreamPlan;
var
  LFrames: TEnsembleStudioFramePlan;
begin
  LFrames := PlanEnsembleStudioFrames(ASeconds);
  Result.RequestedText := LFrames.RequestedText;
  Result.RequestedTicks := LFrames.RequestedTicks;
  Result.ActualTicks := LFrames.ActualTicks;
  Result.CellCount := LFrames.CellCount;
  if Result.CellCount > WFC_MUSIC_AUDIO_STREAM_MAX_FRAMES div
      FRAMES_PER_CELL then
    StreamError('duration exceeds the exact WAVE/RF64 frame envelope');
  Result.ExpectedFrames :=
    TWfcMusicEnsembleAudioCount(Result.CellCount) * FRAMES_PER_CELL;
  if Result.ExpectedFrames < 1 then
    StreamError('duration quantizes to zero PCM frames');
end;

function EnsembleStudioStreamSecondsText(
  const ATicks: TWfcMusicArrangementWide): String;
var
  LMilliseconds: Integer;
  LWhole, LRemainder: TWfcMusicArrangementWide;
  LFraction: String;
begin
  if (ATicks < 0) or
      (ATicks > WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER) then
    StreamError('tick count exceeds exact representation');
  {$IFDEF PAS2JS}
  if ATicks <> Trunc(ATicks) then
    StreamError('tick count must be an exact integer');
  {$ENDIF}
  LWhole := ATicks div TICKS_PER_SECOND;
  LRemainder := ATicks mod TICKS_PER_SECOND;
  if LRemainder = 0 then Exit(IntToStr(LWhole));
  LMilliseconds := Integer((LRemainder * 1000) div TICKS_PER_SECOND);
  LFraction := Format('%.3d', [LMilliseconds]);
  while (Length(LFraction) > 1) and
      (LFraction[Length(LFraction)] = '0') do
    Delete(LFraction, Length(LFraction), 1);
  Result := IntToStr(LWhole) + '.' + LFraction;
end;

procedure FreeModels(var AModels: TWfcMusicEnsembleModels);
begin
  AModels.Ensemble.Free;
  AModels.Rhythm.Free;
  AModels.Harmony.Free;
  AModels := Default(TWfcMusicEnsembleModels);
end;


procedure ValidateStreamOptions(
  const AOptions: TEnsembleStudioStreamOptions);
begin
  EnsembleStudioProfileName(AOptions.Profile);
  {$IFDEF PAS2JS}
  if (AOptions.Seed <> Trunc(AOptions.Seed)) or
      (AOptions.Seed < 0) or (AOptions.Seed > Cardinal($FFFFFFFF)) then
    StreamError('seed must be an exact unsigned 32-bit integer');
  if (AOptions.SegmentCellCount <> Trunc(AOptions.SegmentCellCount)) or
      (AOptions.MaxBacktracks <> Trunc(AOptions.MaxBacktracks)) or
      (AOptions.MaxPassBacktracks <> Trunc(AOptions.MaxPassBacktracks)) then
    StreamError('stream options must be exact integers');
  if (AOptions.MaxBacktracks > High(Integer)) or
      (AOptions.MaxPassBacktracks > High(Integer)) then
    StreamError('search allowances exceed integer capacity');
  {$ENDIF}
  if (AOptions.SegmentCellCount < 1) or
      (AOptions.SegmentCellCount > High(Integer) div
        ENSEMBLE_STUDIO_STREAM_QUANTUM) then
    StreamError('segment cell count is outside its supported range');
  if (AOptions.MaxBacktracks < 0) or
      (AOptions.MaxPassBacktracks < 0) then
    StreamError('search allowances must be nonnegative');
end;

constructor TEnsembleStudioFrameStream.Create(
  const APlan: TEnsembleStudioFramePlan;
  const AOptions: TEnsembleStudioStreamOptions);
var
  LConfig: TWfcMusicEnsembleStreamConfig;
  LVerifiedPlan: TEnsembleStudioFramePlan;
begin
  inherited Create;
  ValidateStreamOptions(AOptions);
  LVerifiedPlan := PlanEnsembleStudioFrames(APlan.RequestedText);
  if (APlan.RequestedTicks <> LVerifiedPlan.RequestedTicks) or
      (APlan.ActualTicks <> LVerifiedPlan.ActualTicks) or
      (APlan.CellCount <> LVerifiedPlan.CellCount) then
    StreamError('frame plan is inconsistent');
  FPlan := APlan;
  FOptions := AOptions;
  FModels := BuildEnsembleStudioProfileModels(FOptions.Profile);
  try
    LConfig := DefaultWfcMusicEnsembleStreamConfig(FModels,
      ENSEMBLE_STUDIO_STREAM_VOICE_COUNT, 12, ENSEMBLE_STUDIO_STREAM_QUANTUM,
      FPlan.RequestedTicks, FOptions.Seed);
    LConfig.SegmentCellCount := FOptions.SegmentCellCount;
    LConfig.Rounding := wmarCeilToCell;
    LConfig.Search.SolveOptions.MaxBacktracks := FOptions.MaxBacktracks;
    LConfig.Search.MaxPassBacktracks := FOptions.MaxPassBacktracks;
    LConfig.Search.SolveOptions.CaptureTrace := FOptions.CaptureTrace;
    FGenerator := CreateEnsembleStudioStream(LConfig, FOptions.Profile);
    if FGenerator.ActualTicks <> FPlan.ActualTicks then
      StreamError('generation rounding differs from the preflight plan');
  except
    FreeAndNil(FGenerator);
    FreeModels(FModels);
    raise;
  end;
  FStatus := wmasReady;
  FLastNegotiationStatus := gnsContradiction;
end;

destructor TEnsembleStudioFrameStream.Destroy;
begin
  ClearSegment;
  FGenerator.Free;
  FreeModels(FModels);
  inherited Destroy;
end;

procedure TEnsembleStudioFrameStream.ClearSegment;
begin
  FSegment.Free;
  FSegment := nil;
  FFrames := nil;
  FFrameIndex := 0;
end;

procedure TEnsembleStudioFrameStream.Fail(const AMessage: String);
begin
  FFailure := AMessage;
  if FFailure = '' then FFailure := 'stream failed without a diagnostic';
  FStatus := wmasFailed;
  ClearSegment;
  if FGenerator <> nil then FGenerator.Cancel;
end;

function TEnsembleStudioFrameStream.GetProducedTicks:
  TWfcMusicArrangementWide;
begin
  if FGenerator = nil then Result := 0
  else Result := FGenerator.ProducedTicks;
end;

function TEnsembleStudioFrameStream.NextFrame(
  out AFrame: TWfcMusicEnsembleFrame): TWfcMusicArrangementStep;
var
  I: Integer;
  LReport: TGraphNegotiationReport;
  LStep: TWfcMusicArrangementStep;
begin
  AFrame := Default(TWfcMusicEnsembleFrame);
  case FStatus of
    wmasCompleted: Exit(wmaspCompleted);
    wmasCancelled: Exit(wmaspCancelled);
    wmasFailed: Exit(wmaspFailed);
  end;
  FStatus := wmasActive;
  try
    while True do
    begin
      if (FSegment <> nil) and (FFrameIndex < Length(FFrames)) then
      begin
        AFrame := MakeWfcMusicEnsembleFrame(FFrames[FFrameIndex].Voices);
        Inc(FFrameIndex);
        Exit(wmaspProduced);
      end;
      ClearSegment;
      LStep := FGenerator.Next(FSegment, LReport);
      case LStep of
        wmaspProduced:
          begin
            FFrames := FSegment.CopyFrames;
            if Length(FFrames) <> FSegment.CellCount then
            begin
              Fail('generated segment frame count differs');
              Exit(wmaspFailed);
            end;
            if (Length(FFrames) < 1) then
            begin
              Fail('generated segment is empty');
              Exit(wmaspFailed);
            end;
            if FSegmentsProduced > 0 then
              for I := 0 to High(FFrames[0].Voices) do
                if FFrames[0].Voices[I].Action = wmcaHold then
                  Inc(FSeamHoldCount);
            Inc(FSegmentsProduced);
            FLastSegmentSignature := FSegment.Signature;
            FLastNegotiationStatus := LReport.Status;
            FLastTranscriptHash := LReport.TranscriptHash;
          end;
        wmaspCompleted:
          begin
            ClearSegment;
            FStatus := wmasCompleted;
            Exit(wmaspCompleted);
          end;
        wmaspCancelled:
          begin
            FStatus := wmasCancelled;
            Exit(wmaspCancelled);
          end;
        wmaspFailed:
          begin
            FLastNegotiationStatus := LReport.Status;
            FLastTranscriptHash := LReport.TranscriptHash;
            Fail(FGenerator.Failure);
            Exit(wmaspFailed);
          end;
      else
        begin
          Fail('generation returned an unknown step');
          Exit(wmaspFailed);
        end;
      end;
    end;
  except
    on E: EOutOfMemory do
    begin
      Fail('memory allocation failed');
      raise;
    end;
    on E: Exception do
    begin
      Fail(E.Message);
      raise;
    end;
  end;
end;

procedure TEnsembleStudioFrameStream.Cancel;
begin
  if FStatus in [wmasCompleted, wmasCancelled, wmasFailed] then Exit;
  FGenerator.Cancel;
  ClearSegment;
  FStatus := wmasCancelled;
  FFailure := '';
end;

constructor TEnsembleStudioPcmStream.Create(
  const APlan: TEnsembleStudioStreamPlan;
  const AOptions: TEnsembleStudioStreamOptions);
var
  LAudio: TWfcMusicAudioOptions;
  LCapacities: TWfcMusicEnsembleAudioVoiceCapacities;
  LFramePlan: TEnsembleStudioFramePlan;
  LVerifiedPlan: TEnsembleStudioStreamPlan;
begin
  inherited Create;
  LVerifiedPlan := PlanEnsembleStudioStream(APlan.RequestedText);
  if (APlan.RequestedTicks <> LVerifiedPlan.RequestedTicks) or
      (APlan.ActualTicks <> LVerifiedPlan.ActualTicks) or
      (APlan.CellCount <> LVerifiedPlan.CellCount) or
      (APlan.ExpectedFrames <> LVerifiedPlan.ExpectedFrames) then
    StreamError('stream plan is inconsistent');
  FPlan := APlan;
  FOptions := AOptions;
  LFramePlan.RequestedText := APlan.RequestedText;
  LFramePlan.RequestedTicks := APlan.RequestedTicks;
  LFramePlan.ActualTicks := APlan.ActualTicks;
  LFramePlan.CellCount := APlan.CellCount;
  try
    FFrameStream := TEnsembleStudioFrameStream.Create(LFramePlan, AOptions);
    SetLength(LCapacities, ENSEMBLE_STUDIO_STREAM_VOICE_COUNT);
    LCapacities[0] := 1;
    LCapacities[1] := 3;
    LCapacities[2] := 1;
    LAudio := DefaultWfcMusicAudioOptions;
    LAudio.SampleRate := ENSEMBLE_STUDIO_STREAM_SAMPLE_RATE;
    FRenderer := TWfcMusicEnsembleAudioRenderer.Create(LAudio,
      ENSEMBLE_STUDIO_STREAM_TPQ, LCapacities);
  except
    FreeAndNil(FRenderer);
    FreeAndNil(FFrameStream);
    raise;
  end;
  FStatus := wmasReady;
end;

destructor TEnsembleStudioPcmStream.Destroy;
begin
  FRenderer.Free;
  FFrameStream.Free;
  inherited Destroy;
end;

procedure TEnsembleStudioPcmStream.Fail(const AMessage: String);
begin
  FFailure := AMessage;
  if FFailure = '' then FFailure := 'stream failed without a diagnostic';
  FStatus := wmasFailed;
  if FFrameStream <> nil then FFrameStream.Cancel;
  if FRenderer <> nil then FRenderer.Cancel;
end;

function TEnsembleStudioPcmStream.GetProducedTicks:
  TWfcMusicArrangementWide;
begin
  if FFrameStream = nil then Result := 0
  else Result := FFrameStream.ProducedTicks;
end;

function TEnsembleStudioPcmStream.GetSegmentsProduced:
  TWfcMusicArrangementWide;
begin
  if FFrameStream = nil then Result := 0
  else Result := FFrameStream.SegmentsProduced;
end;

function TEnsembleStudioPcmStream.GetSeamHoldCount:
  TWfcMusicArrangementWide;
begin
  if FFrameStream = nil then Result := 0
  else Result := FFrameStream.SeamHoldCount;
end;

function TEnsembleStudioPcmStream.GetRenderedFrames:
  TWfcMusicEnsembleAudioCount;
begin
  if FRenderer = nil then Result := 0
  else Result := FRenderer.RenderedFrames;
end;

function TEnsembleStudioPcmStream.GetEmittedFrames:
  TWfcMusicEnsembleAudioCount;
begin
  if FRenderer = nil then Result := 0
  else Result := FRenderer.EmittedFrames;
end;

function TEnsembleStudioPcmStream.GetLastSegmentSignature: Cardinal;
begin
  if FFrameStream = nil then Result := 0
  else Result := FFrameStream.LastSegmentSignature;
end;

function TEnsembleStudioPcmStream.GetLastNegotiationStatus:
  TGraphNegotiationStatus;
begin
  if FFrameStream = nil then Result := gnsContradiction
  else Result := FFrameStream.LastNegotiationStatus;
end;

function TEnsembleStudioPcmStream.GetLastTranscriptHash:
  TGraphTraceSignature;
begin
  if FFrameStream = nil then Result := 0
  else Result := FFrameStream.LastTranscriptHash;
end;

function TEnsembleStudioPcmStream.NextSamples(
  out ASamples: TWfcMusicPcm16Samples): TWfcMusicArrangementStep;
var
  LFrame: TWfcMusicEnsembleFrame;
  LStep: TWfcMusicArrangementStep;
begin
  ASamples := nil;
  case FStatus of
    wmasCompleted: Exit(wmaspCompleted);
    wmasCancelled: Exit(wmaspCancelled);
    wmasFailed: Exit(wmaspFailed);
  end;
  FStatus := wmasActive;
  try
    while True do
    begin
      if FRenderer.ReadSamples(WFC_MUSIC_ENSEMBLE_AUDIO_BLOCK_FRAMES,
          ASamples) then
        Exit(wmaspProduced);
      if FRenderer.Finished then
      begin
        if FRenderer.EmittedFrames <> FPlan.ExpectedFrames then
        begin
          Fail('emitted frame count differs from the preflight plan');
          Exit(wmaspFailed);
        end;
        FStatus := wmasCompleted;
        Exit(wmaspCompleted);
      end;
      if not FRenderer.NeedsInput then
      begin
        Fail('renderer made no progress and did not request input');
        Exit(wmaspFailed);
      end;
      LStep := FFrameStream.NextFrame(LFrame);
      case LStep of
        wmaspProduced:
          FRenderer.AdmitFrame(LFrame, ENSEMBLE_STUDIO_STREAM_QUANTUM,
            ENSEMBLE_STUDIO_STREAM_TEMPO);
        wmaspCompleted:
          FRenderer.EndInput;
        wmaspCancelled:
          begin
            FStatus := wmasCancelled;
            Exit(wmaspCancelled);
          end;
        wmaspFailed:
          begin
            Fail(FFrameStream.Failure);
            Exit(wmaspFailed);
          end;
      else
        begin
          Fail('frame generation returned an unknown step');
          Exit(wmaspFailed);
        end;
      end;
    end;
  except
    on E: EOutOfMemory do
    begin
      Fail('memory allocation failed');
      raise;
    end;
    on E: Exception do
    begin
      Fail(E.Message);
      raise;
    end;
  end;
end;

procedure TEnsembleStudioPcmStream.Cancel;
begin
  if FStatus in [wmasCompleted, wmasCancelled, wmasFailed] then Exit;
  FFrameStream.Cancel;
  FRenderer.Cancel;
  FStatus := wmasCancelled;
  FFailure := '';
end;

end.
