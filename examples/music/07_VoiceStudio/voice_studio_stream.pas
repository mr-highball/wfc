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
unit voice_studio_stream;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_music_audio,
  wfc_music_audio_stream,
  wfc_music_arrangement,
  wfc_music_ensemble,
  wfc_music_ensemble_audio,
  wfc_music_voices_graph,
  wfc_music_voices_stream;

const
  VOICE_STUDIO_STREAM_VERSION = 1;
  VOICE_STUDIO_STREAM_SAMPLE_RATE = 44100;
  VOICE_STUDIO_STREAM_TEMPO = 500000;
  VOICE_STUDIO_STREAM_DEFAULT_SEGMENT_CELLS = 5;

type
  EVoiceStudioStream = class(Exception);

  TVoiceStudioStreamOptions = record
    Seed: TGraphSeed;
    SegmentCellCount: Integer;
    MaxBacktracks: Integer;
    MaxPassBacktracks: Integer;
    CaptureTrace: Boolean;
  end;

  TVoiceStudioFramePlan = record
    RequestedText: String;
    RequestedTicks: TWfcMusicArrangementWide;
    ActualTicks: TWfcMusicArrangementWide;
    CellCount: TWfcMusicArrangementWide;
  end;

  TVoiceStudioWavePlan = record
    RequestedText: String;
    RequestedTicks: TWfcMusicArrangementWide;
    ActualTicks: TWfcMusicArrangementWide;
    CellCount: TWfcMusicArrangementWide;
    ExpectedFrames: TWfcMusicEnsembleAudioCount;
  end;

  TVoiceStudioCoverageSuppliers = array of Integer;

  { One detached public cell. Coverage is indexed by pitch class and contains
    -1 when that class was absent, otherwise the deterministic lowest matching
    role selected as its proof. It is not sole ownership: another role may
    sound the same pitch class too. }
  TVoiceStudioCell = record
    Position, SegmentIndex: TWfcMusicArrangementWide;
    SegmentOffset: Integer;
    Frame: TWfcMusicEnsembleFrame;
    HarmonyToken, RhythmToken: TWfcModelToken;
    VoiceTokens: TWfcModelTokens;
    CoverageSuppliers: TVoiceStudioCoverageSuppliers;
    ObservedVertical: Boolean;
  end;

  { Owns the corpus models, core stream, and at most one current segment. It
    emits one detached cell at a time and never retains elapsed history. }
  TVoiceStudioFrameStream = class
  strict private
    FPlan: TVoiceStudioFramePlan;
    FOptions: TVoiceStudioStreamOptions;
    FGraphConfig: TWfcMusicVoicesGraphConfig;
    FGenerator: TWfcMusicVoicesStream;
    FSegment: TWfcMusicVoicesSegment;
    FFrames: TWfcMusicEnsembleFrames;
    FHarmony, FRhythm: TWfcModelTokens;
    FVoiceTokens: array of TWfcModelTokens;
    FCoverage: TWfcMusicVoicesCoverageWitnesses;
    FSegmentOffset: Integer;
    FCellsProduced, FSegmentsProduced, FSeamHoldCount,
      FNovelVerticalCount, FSharedCoverageCellCount: TWfcMusicArrangementWide;
    FCoverageRoleMask: Cardinal;
    FLastSegmentSignature: Cardinal;
    FLastNegotiationStatus: TGraphNegotiationStatus;
    FLastTranscriptHash: TGraphTraceSignature;
    FStatus: TWfcMusicArrangementStatus;
    FFailure: String;
    procedure ClearSegment;
    procedure FreeModels;
    procedure Fail(const AMessage: String);
    function LoadSegment: TWfcMusicArrangementStep;
    function GetProducedTicks: TWfcMusicArrangementWide;
  public
    constructor Create(const APlan: TVoiceStudioFramePlan;
      const AOptions: TVoiceStudioStreamOptions);
    destructor Destroy; override;
    function NextCell(out ACell: TVoiceStudioCell): TWfcMusicArrangementStep;
    procedure Cancel;
    property Plan: TVoiceStudioFramePlan read FPlan;
    property Options: TVoiceStudioStreamOptions read FOptions;
    property Status: TWfcMusicArrangementStatus read FStatus;
    property Failure: String read FFailure;
    property ProducedTicks: TWfcMusicArrangementWide read GetProducedTicks;
    property CellsProduced: TWfcMusicArrangementWide read FCellsProduced;
    property SegmentsProduced: TWfcMusicArrangementWide read FSegmentsProduced;
    property SeamHoldCount: TWfcMusicArrangementWide read FSeamHoldCount;
    property NovelVerticalCount: TWfcMusicArrangementWide
      read FNovelVerticalCount;
    property SharedCoverageCellCount: TWfcMusicArrangementWide
      read FSharedCoverageCellCount;
    property CoverageRoleMask: Cardinal read FCoverageRoleMask;
    property LastSegmentSignature: Cardinal read FLastSegmentSignature;
    property LastNegotiationStatus: TGraphNegotiationStatus
      read FLastNegotiationStatus;
    property LastTranscriptHash: TGraphTraceSignature
      read FLastTranscriptHash;
  end;

  { Composes the cell source with the bounded incremental PCM adapter. }
  TVoiceStudioPcmStream = class
  strict private
    FPlan: TVoiceStudioWavePlan;
    FOptions: TVoiceStudioStreamOptions;
    FFrames: TVoiceStudioFrameStream;
    FRenderer: TWfcMusicEnsembleAudioRenderer;
    FLatest: TVoiceStudioCell;
    FHasLatest: Boolean;
    FStatus: TWfcMusicArrangementStatus;
    FFailure: String;
    procedure Fail(const AMessage: String);
    function GetProducedTicks: TWfcMusicArrangementWide;
    function GetSegmentsProduced: TWfcMusicArrangementWide;
    function GetSeamHoldCount: TWfcMusicArrangementWide;
    function GetNovelVerticalCount: TWfcMusicArrangementWide;
    function GetSharedCoverageCellCount: TWfcMusicArrangementWide;
    function GetCoverageRoleMask: Cardinal;
    function GetRenderedFrames: TWfcMusicEnsembleAudioCount;
    function GetEmittedFrames: TWfcMusicEnsembleAudioCount;
  public
    constructor Create(const APlan: TVoiceStudioWavePlan;
      const AOptions: TVoiceStudioStreamOptions);
    destructor Destroy; override;
    function NextSamples(out ASamples: TWfcMusicPcm16Samples):
      TWfcMusicArrangementStep;
    function CopyLatestCell(out ACell: TVoiceStudioCell): Boolean;
    procedure Cancel;
    property Plan: TVoiceStudioWavePlan read FPlan;
    property Options: TVoiceStudioStreamOptions read FOptions;
    property Status: TWfcMusicArrangementStatus read FStatus;
    property Failure: String read FFailure;
    property ProducedTicks: TWfcMusicArrangementWide read GetProducedTicks;
    property SegmentsProduced: TWfcMusicArrangementWide read GetSegmentsProduced;
    property SeamHoldCount: TWfcMusicArrangementWide read GetSeamHoldCount;
    property NovelVerticalCount: TWfcMusicArrangementWide
      read GetNovelVerticalCount;
    property SharedCoverageCellCount: TWfcMusicArrangementWide
      read GetSharedCoverageCellCount;
    property CoverageRoleMask: Cardinal read GetCoverageRoleMask;
    property RenderedFrames: TWfcMusicEnsembleAudioCount read GetRenderedFrames;
    property EmittedFrames: TWfcMusicEnsembleAudioCount read GetEmittedFrames;
  end;

function DefaultVoiceStudioStreamOptions: TVoiceStudioStreamOptions;
function PlanVoiceStudioFrames(const ASeconds: String): TVoiceStudioFramePlan;
function PlanVoiceStudioWave(const ASeconds: String): TVoiceStudioWavePlan;
function VoiceStudioSecondsText(const ATicks: TWfcMusicArrangementWide): String;
function CopyVoiceStudioCell(const ACell: TVoiceStudioCell): TVoiceStudioCell;

implementation

uses
  wfc_sequence,
  wfc_sequence_graph,
  wfc_music_sequence,
  wfc_music_ensemble_graph,
  wfc_music_voices_training,
  voice_studio_corpus;

const
  TICKS_PER_SECOND = 960;
  FRAMES_PER_CELL = 11025;

procedure StreamError(const AMessage: String);
begin
  raise EVoiceStudioStream.Create('Voice Studio stream: ' + AMessage);
end;

function DefaultVoiceStudioStreamOptions: TVoiceStudioStreamOptions;
begin
  Result := Default(TVoiceStudioStreamOptions);
  Result.Seed := 1;
  Result.SegmentCellCount := VOICE_STUDIO_STREAM_DEFAULT_SEGMENT_CELLS;
  Result.MaxBacktracks := 1024;
  Result.MaxPassBacktracks := 64;
end;

function PlanVoiceStudioFrames(const ASeconds: String): TVoiceStudioFramePlan;
var
  I, D, FractionDigits, FractionStart, ProductDigit, Carry: Integer;
  Whole, Extra: TWfcMusicArrangementWide;
  HasWholeDigit, HasFractionRemainder, InFraction: Boolean;
  LText: String;
begin
  Result := Default(TVoiceStudioFramePlan);
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
  if not HasWholeDigit or (InFraction and (FractionDigits = 0)) then
    StreamError('duration must be a positive decimal number');

  Carry := 0;
  HasFractionRemainder := False;
  if InFraction then
    for I := Length(LText) downto FractionStart do
    begin
      ProductDigit := (Ord(LText[I]) - Ord('0')) * TICKS_PER_SECOND + Carry;
      if ProductDigit mod 10 <> 0 then HasFractionRemainder := True;
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
    Result.RequestedTicks, VOICE_STUDIO_QUANTUM, wmarCeilToCell);
  Result.CellCount := Result.ActualTicks div VOICE_STUDIO_QUANTUM;
  Result.RequestedText := LText;
end;

function PlanVoiceStudioWave(const ASeconds: String): TVoiceStudioWavePlan;
var
  LFrames: TVoiceStudioFramePlan;
begin
  LFrames := PlanVoiceStudioFrames(ASeconds);
  Result.RequestedText := LFrames.RequestedText;
  Result.RequestedTicks := LFrames.RequestedTicks;
  Result.ActualTicks := LFrames.ActualTicks;
  Result.CellCount := LFrames.CellCount;
  if Result.CellCount >
      WFC_MUSIC_AUDIO_STREAM_MAX_FRAMES div FRAMES_PER_CELL then
    StreamError('duration exceeds the exact WAVE/RF64 frame envelope');
  Result.ExpectedFrames :=
    TWfcMusicEnsembleAudioCount(Result.CellCount) * FRAMES_PER_CELL;
  if Result.ExpectedFrames < 1 then
    StreamError('duration quantizes to zero PCM frames');
end;

function VoiceStudioSecondsText(
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

function CopyVoiceStudioCell(const ACell: TVoiceStudioCell): TVoiceStudioCell;
var
  I: Integer;
begin
  Result := ACell;
  Result.Frame := MakeWfcMusicEnsembleFrame(ACell.Frame.Voices);
  Result.VoiceTokens := nil;
  SetLength(Result.VoiceTokens, Length(ACell.VoiceTokens));
  for I := 0 to High(Result.VoiceTokens) do
    Result.VoiceTokens[I] := ACell.VoiceTokens[I];
  Result.CoverageSuppliers := nil;
  SetLength(Result.CoverageSuppliers, Length(ACell.CoverageSuppliers));
  for I := 0 to High(Result.CoverageSuppliers) do
    Result.CoverageSuppliers[I] := ACell.CoverageSuppliers[I];
end;

procedure ValidateOptions(const AOptions: TVoiceStudioStreamOptions);
begin
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
      (AOptions.SegmentCellCount > High(Integer) div VOICE_STUDIO_QUANTUM) then
    StreamError('segment cell count is outside its supported range');
  if (AOptions.MaxBacktracks < 0) or
      (AOptions.MaxPassBacktracks < 0) then
    StreamError('search allowances must be nonnegative');
end;

procedure TVoiceStudioFrameStream.FreeModels;
var
  I: Integer;
begin
  for I := 0 to High(FGraphConfig.Voices) do
    FreeAndNil(FGraphConfig.Voices[I].Model);
  FreeAndNil(FGraphConfig.RhythmModel);
  FreeAndNil(FGraphConfig.HarmonyModel);
  FGraphConfig := Default(TWfcMusicVoicesGraphConfig);
end;

constructor TVoiceStudioFrameStream.Create(
  const APlan: TVoiceStudioFramePlan;
  const AOptions: TVoiceStudioStreamOptions);
var
  I: Integer;
  LConfig: TWfcMusicVoicesStreamConfig;
  LPlan: TVoiceStudioFramePlan;
  LVoices: TWfcMusicVoiceSequenceModels;
begin
  inherited Create;
  ValidateOptions(AOptions);
  LPlan := PlanVoiceStudioFrames(APlan.RequestedText);
  if (APlan.RequestedTicks <> LPlan.RequestedTicks) or
      (APlan.ActualTicks <> LPlan.ActualTicks) or
      (APlan.CellCount <> LPlan.CellCount) then
    StreamError('frame plan is inconsistent');
  FPlan := APlan;
  FOptions := AOptions;
  LVoices := nil;
  try
    BuildVoiceStudioModels(FGraphConfig.HarmonyModel,
      FGraphConfig.RhythmModel, LVoices);
    SetLength(FGraphConfig.Voices, Length(LVoices));
    for I := 0 to High(LVoices) do
    begin
      FGraphConfig.Voices[I].Model := LVoices[I];
      LVoices[I] := nil;
      FGraphConfig.Voices[I].MinPitch := VoiceStudioRoleMinimumPitch(I);
      FGraphConfig.Voices[I].MaxPitch := VoiceStudioRoleMaximumPitch(I);
    end;
    FGraphConfig.StepsPerOctave := VOICE_STUDIO_STEPS;
    FGraphConfig.HarmonyMode := wmehmExact;
    SetLength(FGraphConfig.PairConstraints, 2);
    FGraphConfig.PairConstraints[0].LowerVoice := 0;
    FGraphConfig.PairConstraints[0].UpperVoice := 1;
    FGraphConfig.PairConstraints[0].MinGap := 3;
    FGraphConfig.PairConstraints[0].MaxGap := 33;
    FGraphConfig.PairConstraints[0].RestPolicy := wmvprSuspend;
    FGraphConfig.PairConstraints[1].LowerVoice := 1;
    FGraphConfig.PairConstraints[1].UpperVoice := 2;
    FGraphConfig.PairConstraints[1].MinGap := 3;
    FGraphConfig.PairConstraints[1].MaxGap := 33;
    FGraphConfig.PairConstraints[1].RestPolicy := wmvprSuspend;

    LConfig := DefaultWfcMusicVoicesStreamConfig(FGraphConfig,
      VOICE_STUDIO_QUANTUM, FPlan.RequestedTicks, FOptions.Seed);
    LConfig.SegmentCellCount := FOptions.SegmentCellCount;
    LConfig.Rounding := wmarCeilToCell;
    LConfig.Search.SolveOptions.MaxBacktracks := FOptions.MaxBacktracks;
    LConfig.Search.MaxPassBacktracks := FOptions.MaxPassBacktracks;
    LConfig.Search.SolveOptions.CaptureTrace := FOptions.CaptureTrace;
    FGenerator := TWfcMusicVoicesStream.Create(LConfig);
    if FGenerator.ActualTicks <> FPlan.ActualTicks then
      StreamError('generation rounding differs from the preflight plan');
  except
    for I := 0 to High(LVoices) do LVoices[I].Free;
    FreeAndNil(FGenerator);
    FreeModels;
    raise;
  end;
  FStatus := wmasReady;
  FLastNegotiationStatus := gnsContradiction;
end;

destructor TVoiceStudioFrameStream.Destroy;
begin
  ClearSegment;
  FGenerator.Free;
  FreeModels;
  inherited Destroy;
end;

procedure TVoiceStudioFrameStream.ClearSegment;
begin
  FreeAndNil(FSegment);
  FFrames := nil;
  FHarmony := nil;
  FRhythm := nil;
  FVoiceTokens := nil;
  FCoverage := nil;
  FSegmentOffset := 0;
end;

procedure TVoiceStudioFrameStream.Fail(const AMessage: String);
begin
  FFailure := AMessage;
  if FFailure = '' then FFailure := 'stream failed without a diagnostic';
  FStatus := wmasFailed;
  ClearSegment;
  if FGenerator <> nil then FGenerator.Cancel;
end;

function TVoiceStudioFrameStream.GetProducedTicks:
  TWfcMusicArrangementWide;
begin
  Result := FCellsProduced * VOICE_STUDIO_QUANTUM;
end;

function TVoiceStudioFrameStream.LoadSegment: TWfcMusicArrangementStep;
var
  I: Integer;
  LLayer: TWfcGeneratedSequenceSegment;
  LReport: TGraphNegotiationReport;
begin
  ClearSegment;
  Result := FGenerator.Next(FSegment, LReport);
  case Result of
    wmaspProduced:
      begin
        FLastNegotiationStatus := LReport.Status;
        FLastTranscriptHash := LReport.TranscriptHash;
        FFrames := FSegment.CopyFrames;
        LLayer := FSegment.CopyGenerated(0);
        FHarmony := LLayer.Tokens;
        LLayer.Tokens := nil;
        LLayer := FSegment.CopyGenerated(1);
        FRhythm := LLayer.Tokens;
        LLayer.Tokens := nil;
        SetLength(FVoiceTokens, VOICE_STUDIO_ROLE_COUNT);
        for I := 0 to VOICE_STUDIO_ROLE_COUNT - 1 do
        begin
          LLayer := FSegment.CopyGenerated(I + 2);
          FVoiceTokens[I] := LLayer.Tokens;
          LLayer.Tokens := nil;
        end;
        FCoverage := FSegment.CopyCoverage;
        if (Length(FFrames) <> FSegment.CellCount) or
            (Length(FHarmony) <> FSegment.CellCount) or
            (Length(FRhythm) <> FSegment.CellCount) then
        begin
          Fail('generated segment layer lengths differ');
          Exit(wmaspFailed);
        end;
        for I := 0 to High(FVoiceTokens) do
          if Length(FVoiceTokens[I]) <> FSegment.CellCount then
          begin
            Fail('generated voice layer length differs');
            Exit(wmaspFailed);
          end;
        if Length(FFrames) < 1 then
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
      end;
    wmaspCompleted:
      begin
        ClearSegment;
        FStatus := wmasCompleted;
      end;
    wmaspCancelled: FStatus := wmasCancelled;
    wmaspFailed:
      begin
        FLastNegotiationStatus := LReport.Status;
        FLastTranscriptHash := LReport.TranscriptHash;
        Fail(FGenerator.Failure);
      end;
  else
    begin
      Fail('generation returned an unknown step');
      Result := wmaspFailed;
    end;
  end;
end;

function TVoiceStudioFrameStream.NextCell(
  out ACell: TVoiceStudioCell): TWfcMusicArrangementStep;
var
  I, LRoleCount: Integer;
  LMask: Cardinal;
  LVoiceFrame: TWfcMusicEnsembleFrame;
begin
  ACell := Default(TVoiceStudioCell);
  case FStatus of
    wmasCompleted: Exit(wmaspCompleted);
    wmasCancelled: Exit(wmaspCancelled);
    wmasFailed: Exit(wmaspFailed);
  end;
  FStatus := wmasActive;
  try
    while (FSegment = nil) or (FSegmentOffset >= Length(FFrames)) do
    begin
      Result := LoadSegment;
      if Result <> wmaspProduced then Exit;
    end;
    ACell.Position := FCellsProduced;
    ACell.SegmentIndex := FSegment.Index;
    ACell.SegmentOffset := FSegmentOffset;
    ACell.Frame := MakeWfcMusicEnsembleFrame(FFrames[FSegmentOffset].Voices);
    ACell.HarmonyToken := FHarmony[FSegmentOffset];
    ACell.RhythmToken := FRhythm[FSegmentOffset];
    SetLength(ACell.VoiceTokens, Length(FVoiceTokens));
    for I := 0 to High(FVoiceTokens) do
      ACell.VoiceTokens[I] := FVoiceTokens[I][FSegmentOffset];
    SetLength(ACell.CoverageSuppliers, VOICE_STUDIO_STEPS);
    for I := 0 to High(ACell.CoverageSuppliers) do
      ACell.CoverageSuppliers[I] := -1;
    LMask := 0;
    for I := 0 to High(FCoverage) do
      if (FCoverage[I].PitchClass >= 0) and
          (FCoverage[I].PitchClass < Length(ACell.CoverageSuppliers)) then
      begin
        ACell.CoverageSuppliers[FCoverage[I].PitchClass] :=
          FCoverage[I].Suppliers[FSegmentOffset];
        if FCoverage[I].Suppliers[FSegmentOffset] >= 0 then
          LMask := LMask or
            (Cardinal(1) shl FCoverage[I].Suppliers[FSegmentOffset]);
      end;
    LRoleCount := 0;
    for I := 0 to VOICE_STUDIO_ROLE_COUNT - 1 do
      if (LMask and (Cardinal(1) shl I)) <> 0 then Inc(LRoleCount);
    ACell.ObservedVertical := VoiceStudioVerticalWasObserved(ACell.Frame);
    FCoverageRoleMask := FCoverageRoleMask or LMask;
    if LRoleCount > 1 then Inc(FSharedCoverageCellCount);
    if not ACell.ObservedVertical then Inc(FNovelVerticalCount);
    Inc(FSegmentOffset);
    Inc(FCellsProduced);
    Result := wmaspProduced;
  except
    on E: EOutOfMemory do
    begin
      ACell := Default(TVoiceStudioCell);
      Fail('memory allocation failed');
      raise;
    end;
    on E: Exception do
    begin
      ACell := Default(TVoiceStudioCell);
      Fail(E.Message);
      raise;
    end;
  end;
end;

procedure TVoiceStudioFrameStream.Cancel;
begin
  if FStatus in [wmasCompleted, wmasCancelled, wmasFailed] then Exit;
  if FGenerator <> nil then FGenerator.Cancel;
  ClearSegment;
  FStatus := wmasCancelled;
  FFailure := '';
end;

constructor TVoiceStudioPcmStream.Create(
  const APlan: TVoiceStudioWavePlan;
  const AOptions: TVoiceStudioStreamOptions);
var
  I: Integer;
  LAudio: TWfcMusicAudioOptions;
  LCapacities: TWfcMusicEnsembleAudioVoiceCapacities;
  LFramePlan: TVoiceStudioFramePlan;
  LPlan: TVoiceStudioWavePlan;
begin
  inherited Create;
  LPlan := PlanVoiceStudioWave(APlan.RequestedText);
  if (APlan.RequestedTicks <> LPlan.RequestedTicks) or
      (APlan.ActualTicks <> LPlan.ActualTicks) or
      (APlan.CellCount <> LPlan.CellCount) or
      (APlan.ExpectedFrames <> LPlan.ExpectedFrames) then
    StreamError('WAVE plan is inconsistent');
  FPlan := APlan;
  FOptions := AOptions;
  LFramePlan.RequestedText := APlan.RequestedText;
  LFramePlan.RequestedTicks := APlan.RequestedTicks;
  LFramePlan.ActualTicks := APlan.ActualTicks;
  LFramePlan.CellCount := APlan.CellCount;
  try
    FFrames := TVoiceStudioFrameStream.Create(LFramePlan, AOptions);
    SetLength(LCapacities, VOICE_STUDIO_ROLE_COUNT);
    for I := 0 to High(LCapacities) do
      LCapacities[I] := VoiceStudioRoleToneCapacity(I);
    LAudio := DefaultWfcMusicAudioOptions;
    LAudio.SampleRate := VOICE_STUDIO_STREAM_SAMPLE_RATE;
    FRenderer := TWfcMusicEnsembleAudioRenderer.Create(LAudio,
      VOICE_STUDIO_TPQ, LCapacities);
  except
    FreeAndNil(FRenderer);
    FreeAndNil(FFrames);
    raise;
  end;
  FStatus := wmasReady;
end;

destructor TVoiceStudioPcmStream.Destroy;
begin
  FRenderer.Free;
  FFrames.Free;
  inherited Destroy;
end;

procedure TVoiceStudioPcmStream.Fail(const AMessage: String);
begin
  FFailure := AMessage;
  if FFailure = '' then FFailure := 'stream failed without a diagnostic';
  FStatus := wmasFailed;
  if FFrames <> nil then FFrames.Cancel;
  if FRenderer <> nil then FRenderer.Cancel;
end;

function TVoiceStudioPcmStream.GetProducedTicks: TWfcMusicArrangementWide;
begin if FFrames = nil then Result := 0 else Result := FFrames.ProducedTicks end;

function TVoiceStudioPcmStream.GetSegmentsProduced: TWfcMusicArrangementWide;
begin if FFrames = nil then Result := 0 else Result := FFrames.SegmentsProduced end;

function TVoiceStudioPcmStream.GetSeamHoldCount: TWfcMusicArrangementWide;
begin if FFrames = nil then Result := 0 else Result := FFrames.SeamHoldCount end;

function TVoiceStudioPcmStream.GetNovelVerticalCount: TWfcMusicArrangementWide;
begin if FFrames = nil then Result := 0 else Result := FFrames.NovelVerticalCount end;

function TVoiceStudioPcmStream.GetSharedCoverageCellCount:
  TWfcMusicArrangementWide;
begin
  if FFrames = nil then Result := 0
  else Result := FFrames.SharedCoverageCellCount;
end;

function TVoiceStudioPcmStream.GetCoverageRoleMask: Cardinal;
begin if FFrames = nil then Result := 0 else Result := FFrames.CoverageRoleMask end;

function TVoiceStudioPcmStream.GetRenderedFrames:
  TWfcMusicEnsembleAudioCount;
begin if FRenderer = nil then Result := 0 else Result := FRenderer.RenderedFrames end;

function TVoiceStudioPcmStream.GetEmittedFrames:
  TWfcMusicEnsembleAudioCount;
begin if FRenderer = nil then Result := 0 else Result := FRenderer.EmittedFrames end;

function TVoiceStudioPcmStream.CopyLatestCell(
  out ACell: TVoiceStudioCell): Boolean;
begin
  Result := FHasLatest;
  if Result then ACell := CopyVoiceStudioCell(FLatest)
  else ACell := Default(TVoiceStudioCell);
end;

function TVoiceStudioPcmStream.NextSamples(
  out ASamples: TWfcMusicPcm16Samples): TWfcMusicArrangementStep;
var
  LCell: TVoiceStudioCell;
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
          ASamples) then Exit(wmaspProduced);
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
      LStep := FFrames.NextCell(LCell);
      case LStep of
        wmaspProduced:
          begin
            FLatest := CopyVoiceStudioCell(LCell);
            FHasLatest := True;
            FRenderer.AdmitFrame(LCell.Frame, VOICE_STUDIO_QUANTUM,
              VOICE_STUDIO_STREAM_TEMPO);
          end;
        wmaspCompleted: FRenderer.EndInput;
        wmaspCancelled:
          begin
            FStatus := wmasCancelled;
            Exit(wmaspCancelled);
          end;
        wmaspFailed:
          begin
            Fail(FFrames.Failure);
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

procedure TVoiceStudioPcmStream.Cancel;
begin
  if FStatus in [wmasCompleted, wmasCancelled, wmasFailed] then Exit;
  FFrames.Cancel;
  FRenderer.Cancel;
  FStatus := wmasCancelled;
  FFailure := '';
end;

end.
