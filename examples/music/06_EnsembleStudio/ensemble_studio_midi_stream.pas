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
unit ensemble_studio_midi_stream;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_midi_smf,
  wfc_midi_stream,
  wfc_music_arrangement,
  wfc_music_ensemble,
  wfc_music_ensemble_midi,
  ensemble_studio_stream;

const
  ENSEMBLE_STUDIO_MIDI_STREAM_VERSION = 1;

type
  EEnsembleStudioMidiStream = class(EEnsembleStudioStream);

  { Immutable caller-owned result of the counting pass. It retains verified
    scalar configuration, fingerprints, counts, and the bounded core track
    plan; it never retains generated frames or an event timeline. }
  TEnsembleStudioMidiPlan = class
  private
    FFramePlan: TEnsembleStudioFramePlan;
    FOptions: TEnsembleStudioStreamOptions;
    FCorePlan: TWfcMusicEnsembleMidiPlan;
    FFrameCount, FSegmentsProduced, FSeamHoldCount:
      TWfcMusicArrangementWide;
    FFrameSignature, FLastSegmentSignature: Cardinal;
    FLastTranscriptHash: TGraphTraceSignature;
    constructor Create(const AFramePlan: TEnsembleStudioFramePlan;
      const AOptions: TEnsembleStudioStreamOptions;
      const ACorePlan: TWfcMusicEnsembleMidiPlan;
      const AFrameCount, ASegmentsProduced, ASeamHoldCount:
        TWfcMusicArrangementWide;
      const AFrameSignature, ALastSegmentSignature: Cardinal;
      const ALastTranscriptHash: TGraphTraceSignature);
    function GetEndTick: TWfcMidiStreamCount;
    function GetTrackByteCount: TWfcMidiStreamCount;
    function GetFileByteCount: TWfcMidiStreamCount;
    function GetEventCount: TWfcMidiStreamCount;
    function GetBridgeCount: TWfcMidiStreamCount;
    function GetMidiSignature: Cardinal;
  public
    destructor Destroy; override;
    function CopyMidiOptions: TWfcMusicEnsembleMidiOptions;
    property FramePlan: TEnsembleStudioFramePlan read FFramePlan;
    property Options: TEnsembleStudioStreamOptions read FOptions;
    property EndTick: TWfcMidiStreamCount read GetEndTick;
    property TrackByteCount: TWfcMidiStreamCount read GetTrackByteCount;
    property FileByteCount: TWfcMidiStreamCount read GetFileByteCount;
    property EventCount: TWfcMidiStreamCount read GetEventCount;
    property BridgeCount: TWfcMidiStreamCount read GetBridgeCount;
    property MidiSignature: Cardinal read GetMidiSignature;
    property FrameCount: TWfcMusicArrangementWide read FFrameCount;
    property SegmentsProduced: TWfcMusicArrangementWide read FSegmentsProduced;
    property SeamHoldCount: TWfcMusicArrangementWide read FSeamHoldCount;
    property FrameSignature: Cardinal read FFrameSignature;
    property LastSegmentSignature: Cardinal read FLastSegmentSignature;
    property LastTranscriptHash: TGraphTraceSignature read FLastTranscriptHash;
  end;

  { Incremental first pass. Next processes at most one generated frame. A
    completed plan is detached explicitly; cancellation and failure expose no
    partial plan. }
  TEnsembleStudioMidiPlanner = class
  private
    FFrameStream: TEnsembleStudioFrameStream;
    FCounter: TWfcMusicEnsembleMidiCounter;
    FPlan: TEnsembleStudioMidiPlan;
    FStatus: TWfcMusicArrangementStatus;
    FFailure: String;
    FFrameCount: TWfcMusicArrangementWide;
    FFrameSignature: Cardinal;
    procedure Fail(const AMessage: String);
  public
    constructor Create(const AFramePlan: TEnsembleStudioFramePlan;
      const AOptions: TEnsembleStudioStreamOptions);
    destructor Destroy; override;
    function Next: TWfcMusicArrangementStep;
    function DetachPlan: TEnsembleStudioMidiPlan;
    procedure Cancel;
    property Status: TWfcMusicArrangementStatus read FStatus;
    property Failure: String read FFailure;
    property FramesProcessed: TWfcMusicArrangementWide read FFrameCount;
  end;

  { Incremental replay. The constructor copies all required plan data, so the
    caller may free its plan immediately. Each successful pull returns one
    detached block no larger than WFC_MIDI_STREAM_BLOCK_BYTES. Completion is
    published only after core MIDI validation and independent frame
    fingerprint/count validation both match the counting pass. }
  TEnsembleStudioMidiStream = class
  private
    FFramePlan: TEnsembleStudioFramePlan;
    FOptions: TEnsembleStudioStreamOptions;
    FFrameStream: TEnsembleStudioFrameStream;
    FMidi: TWfcMusicEnsembleMidiStream;
    FStatus: TWfcMusicArrangementStatus;
    FFailure: String;
    FExpectedEndTick, FExpectedFileBytes: TWfcMidiStreamCount;
    FExpectedFrames, FExpectedSegments, FExpectedSeamHolds:
      TWfcMusicArrangementWide;
    FExpectedFrameSignature, FExpectedLastSegmentSignature: Cardinal;
    FExpectedLastTranscriptHash: TGraphTraceSignature;
    FFrameCount: TWfcMusicArrangementWide;
    FFrameSignature: Cardinal;
    procedure Fail(const AMessage: String);
    function VerifyCompletion: Boolean;
    function GetTickCount: TWfcMidiStreamCount;
    function GetEmittedBytes: TWfcMidiStreamCount;
  public
    constructor Create(const APlan: TEnsembleStudioMidiPlan);
    destructor Destroy; override;
    function NextBytes(out ABytes: TWfcMidiBytes): TWfcMusicArrangementStep;
    procedure Cancel;
    property Status: TWfcMusicArrangementStatus read FStatus;
    property Failure: String read FFailure;
    property TickCount: TWfcMidiStreamCount read GetTickCount;
    property EmittedBytes: TWfcMidiStreamCount read GetEmittedBytes;
    property FramesProcessed: TWfcMusicArrangementWide read FFrameCount;
  end;

function DefaultEnsembleStudioMidiOptions: TWfcMusicEnsembleMidiOptions;

implementation

procedure MidiDemoError(const AMessage: String);
begin
  raise EEnsembleStudioMidiStream.Create(
    'Ensemble Studio MIDI stream: ' + AMessage);
end;

function DefaultEnsembleStudioMidiOptions: TWfcMusicEnsembleMidiOptions;
const
  CHANNELS: array[0..2] of Integer = (0, 1, 2);
begin
  Result := DefaultWfcMusicEnsembleMidiOptions(CHANNELS);
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

procedure HashWide(var AHash: Cardinal; AValue: TWfcMusicArrangementWide);
var
  I: Integer;
begin
  for I := 0 to 7 do
  begin
    HashByte(AHash, Byte(AValue mod 256));
    AValue := AValue div 256;
  end;
end;

procedure HashFrame(var AHash: Cardinal;
  const AIndex: TWfcMusicArrangementWide;
  const AFrame: TWfcMusicEnsembleFrame);
var
  I: Integer;
  LText: String;
begin
  LText := EncodeWfcMusicEnsembleFrame(AFrame);
  HashWide(AHash, AIndex);
  HashWide(AHash, Length(LText));
  for I := 1 to Length(LText) do
    HashByte(AHash, Byte(Ord(LText[I])));
end;

constructor TEnsembleStudioMidiPlan.Create(
  const AFramePlan: TEnsembleStudioFramePlan;
  const AOptions: TEnsembleStudioStreamOptions;
  const ACorePlan: TWfcMusicEnsembleMidiPlan;
  const AFrameCount, ASegmentsProduced, ASeamHoldCount:
    TWfcMusicArrangementWide;
  const AFrameSignature, ALastSegmentSignature: Cardinal;
  const ALastTranscriptHash: TGraphTraceSignature);
begin
  inherited Create;
  if ACorePlan = nil then MidiDemoError('core plan is required');
  if (AFrameCount <> AFramePlan.CellCount) or
      (ACorePlan.EndTick <> AFramePlan.ActualTicks) then
    MidiDemoError('counting result differs from the duration plan');
  if ACorePlan.ByteCount > WFC_MIDI_STREAM_MAX_TRACK_BYTES then
    MidiDemoError('track byte count exceeds the format-0 envelope');
  FFramePlan := AFramePlan;
  FOptions := AOptions;
  FFrameCount := AFrameCount;
  FSegmentsProduced := ASegmentsProduced;
  FSeamHoldCount := ASeamHoldCount;
  FFrameSignature := AFrameSignature;
  FLastSegmentSignature := ALastSegmentSignature;
  FLastTranscriptHash := ALastTranscriptHash;
  { Ownership transfers only after all potentially raising copies and checks. }
  FCorePlan := ACorePlan;
end;

destructor TEnsembleStudioMidiPlan.Destroy;
begin
  FCorePlan.Free;
  inherited Destroy;
end;

function TEnsembleStudioMidiPlan.CopyMidiOptions:
  TWfcMusicEnsembleMidiOptions;
begin
  Result := FCorePlan.CopyOptions;
end;

function TEnsembleStudioMidiPlan.GetEndTick: TWfcMidiStreamCount;
begin
  Result := FCorePlan.EndTick;
end;

function TEnsembleStudioMidiPlan.GetTrackByteCount: TWfcMidiStreamCount;
begin
  Result := FCorePlan.ByteCount;
end;

function TEnsembleStudioMidiPlan.GetFileByteCount: TWfcMidiStreamCount;
begin
  Result := FCorePlan.ByteCount + WFC_MIDI_STREAM_HEADER_BYTES;
end;

function TEnsembleStudioMidiPlan.GetEventCount: TWfcMidiStreamCount;
begin
  Result := FCorePlan.EventCount;
end;

function TEnsembleStudioMidiPlan.GetBridgeCount: TWfcMidiStreamCount;
begin
  Result := FCorePlan.BridgeCount;
end;

function TEnsembleStudioMidiPlan.GetMidiSignature: Cardinal;
begin
  Result := FCorePlan.Signature;
end;

constructor TEnsembleStudioMidiPlanner.Create(
  const AFramePlan: TEnsembleStudioFramePlan;
  const AOptions: TEnsembleStudioStreamOptions);
begin
  inherited Create;
  FFrameSignature := Cardinal($811C9DC5);
  try
    FFrameStream := TEnsembleStudioFrameStream.Create(AFramePlan, AOptions);
    FCounter := TWfcMusicEnsembleMidiCounter.Create(
      DefaultEnsembleStudioMidiOptions);
  except
    FreeAndNil(FCounter);
    FreeAndNil(FFrameStream);
    raise;
  end;
  FStatus := wmasReady;
end;

destructor TEnsembleStudioMidiPlanner.Destroy;
begin
  FPlan.Free;
  FCounter.Free;
  FFrameStream.Free;
  inherited Destroy;
end;

procedure TEnsembleStudioMidiPlanner.Fail(const AMessage: String);
begin
  FFailure := AMessage;
  if FFailure = '' then FFailure := 'MIDI planning failed without a diagnostic';
  FStatus := wmasFailed;
  if FFrameStream <> nil then FFrameStream.Cancel;
  FreeAndNil(FPlan);
end;

function TEnsembleStudioMidiPlanner.Next: TWfcMusicArrangementStep;
var
  LCorePlan: TWfcMusicEnsembleMidiPlan;
  LFrame: TWfcMusicEnsembleFrame;
  LStep: TWfcMusicArrangementStep;
begin
  case FStatus of
    wmasCompleted: Exit(wmaspCompleted);
    wmasCancelled: Exit(wmaspCancelled);
    wmasFailed: Exit(wmaspFailed);
  end;
  FStatus := wmasActive;
  try
    LStep := FFrameStream.NextFrame(LFrame);
    case LStep of
      wmaspProduced:
        begin
          FCounter.AdmitFrame(LFrame, ENSEMBLE_STUDIO_STREAM_QUANTUM);
          HashFrame(FFrameSignature, FFrameCount, LFrame);
          Inc(FFrameCount);
          Exit(wmaspProduced);
        end;
      wmaspCompleted:
        begin
          LCorePlan := nil;
          try
            LCorePlan := FCounter.Finish;
            FPlan := TEnsembleStudioMidiPlan.Create(FFrameStream.Plan,
              FFrameStream.Options, LCorePlan, FFrameCount,
              FFrameStream.SegmentsProduced, FFrameStream.SeamHoldCount,
              FFrameSignature, FFrameStream.LastSegmentSignature,
              FFrameStream.LastTranscriptHash);
            LCorePlan := nil;
          finally
            LCorePlan.Free;
          end;
          FreeAndNil(FFrameStream);
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
          Fail(FFrameStream.Failure);
          Exit(wmaspFailed);
        end;
    else
      begin
        Fail('frame source returned an unknown planning step');
        Exit(wmaspFailed);
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

function TEnsembleStudioMidiPlanner.DetachPlan: TEnsembleStudioMidiPlan;
begin
  if (FStatus <> wmasCompleted) or (FPlan = nil) then
    MidiDemoError('completed undetached plan is required');
  Result := FPlan;
  FPlan := nil;
end;

procedure TEnsembleStudioMidiPlanner.Cancel;
begin
  if FStatus in [wmasCompleted, wmasCancelled, wmasFailed] then Exit;
  if FFrameStream <> nil then FFrameStream.Cancel;
  FreeAndNil(FPlan);
  FStatus := wmasCancelled;
  FFailure := '';
end;

constructor TEnsembleStudioMidiStream.Create(
  const APlan: TEnsembleStudioMidiPlan);
begin
  inherited Create;
  if APlan = nil then MidiDemoError('plan is required');
  FFramePlan := APlan.FFramePlan;
  FOptions := APlan.FOptions;
  FExpectedEndTick := APlan.EndTick;
  FExpectedFileBytes := APlan.FileByteCount;
  FExpectedFrames := APlan.FrameCount;
  FExpectedSegments := APlan.SegmentsProduced;
  FExpectedSeamHolds := APlan.SeamHoldCount;
  FExpectedFrameSignature := APlan.FrameSignature;
  FExpectedLastSegmentSignature := APlan.LastSegmentSignature;
  FExpectedLastTranscriptHash := APlan.LastTranscriptHash;
  FFrameSignature := Cardinal($811C9DC5);
  try
    FFrameStream := TEnsembleStudioFrameStream.Create(FFramePlan, FOptions);
    FMidi := TWfcMusicEnsembleMidiStream.Create(APlan.FCorePlan);
  except
    FreeAndNil(FMidi);
    FreeAndNil(FFrameStream);
    raise;
  end;
  FStatus := wmasReady;
end;

destructor TEnsembleStudioMidiStream.Destroy;
begin
  FMidi.Free;
  FFrameStream.Free;
  inherited Destroy;
end;

procedure TEnsembleStudioMidiStream.Fail(const AMessage: String);
begin
  FFailure := AMessage;
  if FFailure = '' then FFailure := 'MIDI replay failed without a diagnostic';
  FStatus := wmasFailed;
  if FFrameStream <> nil then FFrameStream.Cancel;
  if FMidi <> nil then FMidi.Cancel;
end;

function TEnsembleStudioMidiStream.VerifyCompletion: Boolean;
begin
  Result := False;
  if (FFrameCount <> FExpectedFrames) or
      (FFrameStream.ProducedTicks <> FFramePlan.ActualTicks) or
      (FMidi.TickCount <> FExpectedEndTick) then
  begin
    Fail('replay frame count or end tick differs from the plan');
    Exit;
  end;
  if (FFrameSignature <> FExpectedFrameSignature) or
      (FFrameStream.SegmentsProduced <> FExpectedSegments) or
      (FFrameStream.SeamHoldCount <> FExpectedSeamHolds) or
      (FFrameStream.LastSegmentSignature <>
        FExpectedLastSegmentSignature) or
      (FFrameStream.LastTranscriptHash <> FExpectedLastTranscriptHash) then
  begin
    Fail('replay generation fingerprint differs from the plan');
    Exit;
  end;
  if FMidi.EmittedBytes <> FExpectedFileBytes then
  begin
    Fail('replay byte count differs from the plan');
    Exit;
  end;
  Result := True;
end;

function TEnsembleStudioMidiStream.GetTickCount: TWfcMidiStreamCount;
begin
  if FMidi = nil then Result := 0 else Result := FMidi.TickCount;
end;

function TEnsembleStudioMidiStream.GetEmittedBytes: TWfcMidiStreamCount;
begin
  if FMidi = nil then Result := 0 else Result := FMidi.EmittedBytes;
end;

function TEnsembleStudioMidiStream.NextBytes(
  out ABytes: TWfcMidiBytes): TWfcMusicArrangementStep;
var
  LFrame: TWfcMusicEnsembleFrame;
  LStep: TWfcMusicArrangementStep;
begin
  ABytes := nil;
  case FStatus of
    wmasCompleted: Exit(wmaspCompleted);
    wmasCancelled: Exit(wmaspCancelled);
    wmasFailed: Exit(wmaspFailed);
  end;
  FStatus := wmasActive;
  try
    while True do
    begin
      if FMidi.ReadBytes(WFC_MIDI_STREAM_BLOCK_BYTES, ABytes) then
        Exit(wmaspProduced);
      if FMidi.Finished then
      begin
        if not VerifyCompletion then Exit(wmaspFailed);
        FStatus := wmasCompleted;
        Exit(wmaspCompleted);
      end;
      if FMidi.Failed then
      begin
        Fail('core MIDI replay failed');
        Exit(wmaspFailed);
      end;
      if not FMidi.NeedsInput then
      begin
        Fail('MIDI replay made no progress and did not request input');
        Exit(wmaspFailed);
      end;
      LStep := FFrameStream.NextFrame(LFrame);
      case LStep of
        wmaspProduced:
          begin
            FMidi.AdmitFrame(LFrame, ENSEMBLE_STUDIO_STREAM_QUANTUM);
            HashFrame(FFrameSignature, FFrameCount, LFrame);
            Inc(FFrameCount);
          end;
        wmaspCompleted:
          FMidi.EndInput;
        wmaspCancelled:
          begin
            FStatus := wmasCancelled;
            FMidi.Cancel;
            Exit(wmaspCancelled);
          end;
        wmaspFailed:
          begin
            Fail(FFrameStream.Failure);
            Exit(wmaspFailed);
          end;
      else
        begin
          Fail('frame source returned an unknown replay step');
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

procedure TEnsembleStudioMidiStream.Cancel;
begin
  if FStatus in [wmasCompleted, wmasCancelled, wmasFailed] then Exit;
  FFrameStream.Cancel;
  FMidi.Cancel;
  FStatus := wmasCancelled;
  FFailure := '';
end;

end.
