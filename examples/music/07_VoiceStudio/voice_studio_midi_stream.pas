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
unit voice_studio_midi_stream;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_midi_smf,
  wfc_midi_stream,
  wfc_music_arrangement,
  wfc_music_ensemble_midi,
  voice_studio_stream;

const
  VOICE_STUDIO_MIDI_STREAM_VERSION = 1;

type
  EVoiceStudioMidiStream = class(EVoiceStudioStream);

  { Detached counting result. It retains only replay configuration, counts,
    diagnostic fingerprints, and the bounded core plan--never cells/frames. }
  TVoiceStudioMidiPlan = class
  private
    FFramePlan: TVoiceStudioFramePlan;
    FOptions: TVoiceStudioStreamOptions;
    FCore: TWfcMusicEnsembleMidiPlan;
    FFrameCount, FSegments, FSeamHolds, FNovelVerticals,
      FSharedCoverageCells: TWfcMusicArrangementWide;
    FCoverageRoleMask, FFrameSignature, FLastSegmentSignature: Cardinal;
    FLastTranscriptHash: TGraphTraceSignature;
    constructor Create(const AFramePlan: TVoiceStudioFramePlan;
      const AOptions: TVoiceStudioStreamOptions;
      const ACore: TWfcMusicEnsembleMidiPlan;
      const AFrameCount, ASegments, ASeamHolds, ANovelVerticals,
        ASharedCoverageCells: TWfcMusicArrangementWide;
      const ACoverageRoleMask, AFrameSignature,
        ALastSegmentSignature: Cardinal;
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
    property FramePlan: TVoiceStudioFramePlan read FFramePlan;
    property Options: TVoiceStudioStreamOptions read FOptions;
    property EndTick: TWfcMidiStreamCount read GetEndTick;
    property TrackByteCount: TWfcMidiStreamCount read GetTrackByteCount;
    property FileByteCount: TWfcMidiStreamCount read GetFileByteCount;
    property EventCount: TWfcMidiStreamCount read GetEventCount;
    property BridgeCount: TWfcMidiStreamCount read GetBridgeCount;
    property MidiSignature: Cardinal read GetMidiSignature;
    property FrameCount: TWfcMusicArrangementWide read FFrameCount;
    property SegmentsProduced: TWfcMusicArrangementWide read FSegments;
    property SeamHoldCount: TWfcMusicArrangementWide read FSeamHolds;
    property NovelVerticalCount: TWfcMusicArrangementWide read FNovelVerticals;
    property SharedCoverageCellCount: TWfcMusicArrangementWide
      read FSharedCoverageCells;
    property CoverageRoleMask: Cardinal read FCoverageRoleMask;
    property FrameSignature: Cardinal read FFrameSignature;
    property LastSegmentSignature: Cardinal read FLastSegmentSignature;
    property LastTranscriptHash: TGraphTraceSignature
      read FLastTranscriptHash;
  end;

  { Counting pass. Next consumes no more than one generated cell. }
  TVoiceStudioMidiPlanner = class
  strict private
    FFrames: TVoiceStudioFrameStream;
    FCounter: TWfcMusicEnsembleMidiCounter;
    FPlan: TVoiceStudioMidiPlan;
    FStatus: TWfcMusicArrangementStatus;
    FFailure: String;
    FFrameCount: TWfcMusicArrangementWide;
    FFrameSignature: Cardinal;
    FLatest: TVoiceStudioCell;
    FHasLatest: Boolean;
    procedure Fail(const AMessage: String);
  public
    constructor Create(const AFramePlan: TVoiceStudioFramePlan;
      const AOptions: TVoiceStudioStreamOptions);
    destructor Destroy; override;
    function Next: TWfcMusicArrangementStep;
    function DetachPlan: TVoiceStudioMidiPlan;
    function CopyLatestCell(out ACell: TVoiceStudioCell): Boolean;
    procedure Cancel;
    property Status: TWfcMusicArrangementStatus read FStatus;
    property Failure: String read FFailure;
    property FramesProcessed: TWfcMusicArrangementWide read FFrameCount;
  end;

  { Replay pass. A produced step may carry an empty byte array when one quiet
    generated cell advanced the source but queued no MIDI event. Thus every
    call has bounded generation work and browser callers can yield reliably. }
  TVoiceStudioMidiStream = class
  strict private
    FFramePlan: TVoiceStudioFramePlan;
    FOptions: TVoiceStudioStreamOptions;
    FFrames: TVoiceStudioFrameStream;
    FMidi: TWfcMusicEnsembleMidiStream;
    FStatus: TWfcMusicArrangementStatus;
    FFailure: String;
    FExpectedEndTick, FExpectedFileBytes: TWfcMidiStreamCount;
    FExpectedFrames, FExpectedSegments, FExpectedSeamHolds,
      FExpectedNovelVerticals, FExpectedSharedCoverageCells:
      TWfcMusicArrangementWide;
    FExpectedCoverageRoleMask, FExpectedFrameSignature,
      FExpectedLastSegmentSignature: Cardinal;
    FExpectedLastTranscriptHash: TGraphTraceSignature;
    FFrameCount: TWfcMusicArrangementWide;
    FFrameSignature: Cardinal;
    FLatest: TVoiceStudioCell;
    FHasLatest: Boolean;
    procedure Fail(const AMessage: String);
    function VerifyCompletion: Boolean;
    function GetTickCount: TWfcMidiStreamCount;
    function GetEmittedBytes: TWfcMidiStreamCount;
  public
    constructor Create(const APlan: TVoiceStudioMidiPlan);
    destructor Destroy; override;
    function NextBytes(out ABytes: TWfcMidiBytes): TWfcMusicArrangementStep;
    function CopyLatestCell(out ACell: TVoiceStudioCell): Boolean;
    procedure Cancel;
    property Status: TWfcMusicArrangementStatus read FStatus;
    property Failure: String read FFailure;
    property TickCount: TWfcMidiStreamCount read GetTickCount;
    property EmittedBytes: TWfcMidiStreamCount read GetEmittedBytes;
    property FramesProcessed: TWfcMusicArrangementWide read FFrameCount;
  end;

function DefaultVoiceStudioMidiOptions: TWfcMusicEnsembleMidiOptions;

implementation

uses
  wfc_music_ensemble,
  voice_studio_corpus;

procedure MidiError(const AMessage: String);
begin
  raise EVoiceStudioMidiStream.Create('Voice Studio MIDI stream: ' + AMessage);
end;

function DefaultVoiceStudioMidiOptions: TWfcMusicEnsembleMidiOptions;
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

procedure HashCell(var AHash: Cardinal; const ACell: TVoiceStudioCell);
var
  I: Integer;
  LText: String;
begin
  LText := EncodeWfcMusicEnsembleFrame(ACell.Frame);
  HashWide(AHash, ACell.Position);
  HashWide(AHash, Length(LText));
  for I := 1 to Length(LText) do HashByte(AHash, Byte(Ord(LText[I])));
  HashWide(AHash, Ord(ACell.ObservedVertical));
  for I := 0 to High(ACell.CoverageSuppliers) do
    HashWide(AHash, ACell.CoverageSuppliers[I] + 1);
end;

constructor TVoiceStudioMidiPlan.Create(
  const AFramePlan: TVoiceStudioFramePlan;
  const AOptions: TVoiceStudioStreamOptions;
  const ACore: TWfcMusicEnsembleMidiPlan;
  const AFrameCount, ASegments, ASeamHolds, ANovelVerticals,
    ASharedCoverageCells: TWfcMusicArrangementWide;
  const ACoverageRoleMask, AFrameSignature,
    ALastSegmentSignature: Cardinal;
  const ALastTranscriptHash: TGraphTraceSignature);
begin
  inherited Create;
  if ACore = nil then MidiError('core plan is required');
  if (AFrameCount <> AFramePlan.CellCount) or
      (ACore.EndTick <> AFramePlan.ActualTicks) then
    MidiError('counting result differs from the duration plan');
  if ACore.ByteCount > WFC_MIDI_STREAM_MAX_TRACK_BYTES then
    MidiError('track byte count exceeds the format-0 envelope');
  FFramePlan := AFramePlan;
  FOptions := AOptions;
  FFrameCount := AFrameCount;
  FSegments := ASegments;
  FSeamHolds := ASeamHolds;
  FNovelVerticals := ANovelVerticals;
  FSharedCoverageCells := ASharedCoverageCells;
  FCoverageRoleMask := ACoverageRoleMask;
  FFrameSignature := AFrameSignature;
  FLastSegmentSignature := ALastSegmentSignature;
  FLastTranscriptHash := ALastTranscriptHash;
  FCore := ACore;
end;

destructor TVoiceStudioMidiPlan.Destroy;
begin
  FCore.Free;
  inherited Destroy;
end;

function TVoiceStudioMidiPlan.CopyMidiOptions:
  TWfcMusicEnsembleMidiOptions;
begin Result := FCore.CopyOptions end;

function TVoiceStudioMidiPlan.GetEndTick: TWfcMidiStreamCount;
begin Result := FCore.EndTick end;

function TVoiceStudioMidiPlan.GetTrackByteCount: TWfcMidiStreamCount;
begin Result := FCore.ByteCount end;

function TVoiceStudioMidiPlan.GetFileByteCount: TWfcMidiStreamCount;
begin Result := FCore.ByteCount + WFC_MIDI_STREAM_HEADER_BYTES end;

function TVoiceStudioMidiPlan.GetEventCount: TWfcMidiStreamCount;
begin Result := FCore.EventCount end;

function TVoiceStudioMidiPlan.GetBridgeCount: TWfcMidiStreamCount;
begin Result := FCore.BridgeCount end;

function TVoiceStudioMidiPlan.GetMidiSignature: Cardinal;
begin Result := FCore.Signature end;

constructor TVoiceStudioMidiPlanner.Create(
  const AFramePlan: TVoiceStudioFramePlan;
  const AOptions: TVoiceStudioStreamOptions);
begin
  inherited Create;
  FFrameSignature := Cardinal($811C9DC5);
  try
    FFrames := TVoiceStudioFrameStream.Create(AFramePlan, AOptions);
    FCounter := TWfcMusicEnsembleMidiCounter.Create(
      DefaultVoiceStudioMidiOptions);
  except
    FreeAndNil(FCounter);
    FreeAndNil(FFrames);
    raise;
  end;
  FStatus := wmasReady;
end;

destructor TVoiceStudioMidiPlanner.Destroy;
begin
  FPlan.Free;
  FCounter.Free;
  FFrames.Free;
  inherited Destroy;
end;

procedure TVoiceStudioMidiPlanner.Fail(const AMessage: String);
begin
  FFailure := AMessage;
  if FFailure = '' then FFailure := 'MIDI planning failed without a diagnostic';
  FStatus := wmasFailed;
  if FFrames <> nil then FFrames.Cancel;
  FreeAndNil(FPlan);
end;

function TVoiceStudioMidiPlanner.CopyLatestCell(
  out ACell: TVoiceStudioCell): Boolean;
begin
  Result := FHasLatest;
  if Result then ACell := CopyVoiceStudioCell(FLatest)
  else ACell := Default(TVoiceStudioCell);
end;

function TVoiceStudioMidiPlanner.Next: TWfcMusicArrangementStep;
var
  LCell: TVoiceStudioCell;
  LCore: TWfcMusicEnsembleMidiPlan;
  LStep: TWfcMusicArrangementStep;
begin
  case FStatus of
    wmasCompleted: Exit(wmaspCompleted);
    wmasCancelled: Exit(wmaspCancelled);
    wmasFailed: Exit(wmaspFailed);
  end;
  FStatus := wmasActive;
  try
    LStep := FFrames.NextCell(LCell);
    case LStep of
      wmaspProduced:
        begin
          FLatest := CopyVoiceStudioCell(LCell);
          FHasLatest := True;
          FCounter.AdmitFrame(LCell.Frame, VOICE_STUDIO_QUANTUM);
          HashCell(FFrameSignature, LCell);
          Inc(FFrameCount);
          Exit(wmaspProduced);
        end;
      wmaspCompleted:
        begin
          LCore := nil;
          try
            LCore := FCounter.Finish;
            FPlan := TVoiceStudioMidiPlan.Create(FFrames.Plan,
              FFrames.Options, LCore, FFrameCount, FFrames.SegmentsProduced,
              FFrames.SeamHoldCount, FFrames.NovelVerticalCount,
              FFrames.SharedCoverageCellCount, FFrames.CoverageRoleMask,
              FFrameSignature, FFrames.LastSegmentSignature,
              FFrames.LastTranscriptHash);
            LCore := nil;
          finally
            LCore.Free;
          end;
          FreeAndNil(FFrames);
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
          Fail(FFrames.Failure);
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

function TVoiceStudioMidiPlanner.DetachPlan: TVoiceStudioMidiPlan;
begin
  if (FStatus <> wmasCompleted) or (FPlan = nil) then
    MidiError('completed undetached plan is required');
  Result := FPlan;
  FPlan := nil;
end;

procedure TVoiceStudioMidiPlanner.Cancel;
begin
  if FStatus in [wmasCompleted, wmasCancelled, wmasFailed] then Exit;
  if FFrames <> nil then FFrames.Cancel;
  FreeAndNil(FPlan);
  FStatus := wmasCancelled;
  FFailure := '';
end;

constructor TVoiceStudioMidiStream.Create(
  const APlan: TVoiceStudioMidiPlan);
begin
  inherited Create;
  if APlan = nil then MidiError('plan is required');
  FFramePlan := APlan.FFramePlan;
  FOptions := APlan.FOptions;
  FExpectedEndTick := APlan.EndTick;
  FExpectedFileBytes := APlan.FileByteCount;
  FExpectedFrames := APlan.FrameCount;
  FExpectedSegments := APlan.SegmentsProduced;
  FExpectedSeamHolds := APlan.SeamHoldCount;
  FExpectedNovelVerticals := APlan.NovelVerticalCount;
  FExpectedSharedCoverageCells := APlan.SharedCoverageCellCount;
  FExpectedCoverageRoleMask := APlan.CoverageRoleMask;
  FExpectedFrameSignature := APlan.FrameSignature;
  FExpectedLastSegmentSignature := APlan.LastSegmentSignature;
  FExpectedLastTranscriptHash := APlan.LastTranscriptHash;
  FFrameSignature := Cardinal($811C9DC5);
  try
    FFrames := TVoiceStudioFrameStream.Create(FFramePlan, FOptions);
    FMidi := TWfcMusicEnsembleMidiStream.Create(APlan.FCore);
  except
    FreeAndNil(FMidi);
    FreeAndNil(FFrames);
    raise;
  end;
  FStatus := wmasReady;
end;

destructor TVoiceStudioMidiStream.Destroy;
begin
  FMidi.Free;
  FFrames.Free;
  inherited Destroy;
end;

procedure TVoiceStudioMidiStream.Fail(const AMessage: String);
begin
  FFailure := AMessage;
  if FFailure = '' then FFailure := 'MIDI replay failed without a diagnostic';
  FStatus := wmasFailed;
  if FFrames <> nil then FFrames.Cancel;
  if FMidi <> nil then FMidi.Cancel;
end;

function TVoiceStudioMidiStream.VerifyCompletion: Boolean;
begin
  Result := False;
  if (FFrameCount <> FExpectedFrames) or
      (FFrames.ProducedTicks <> FFramePlan.ActualTicks) or
      (FMidi.TickCount <> FExpectedEndTick) then
  begin
    Fail('replay frame count or end tick differs from the plan');
    Exit;
  end;
  if (FFrameSignature <> FExpectedFrameSignature) or
      (FFrames.SegmentsProduced <> FExpectedSegments) or
      (FFrames.SeamHoldCount <> FExpectedSeamHolds) or
      (FFrames.NovelVerticalCount <> FExpectedNovelVerticals) or
      (FFrames.SharedCoverageCellCount <> FExpectedSharedCoverageCells) or
      (FFrames.CoverageRoleMask <> FExpectedCoverageRoleMask) or
      (FFrames.LastSegmentSignature <> FExpectedLastSegmentSignature) or
      (FFrames.LastTranscriptHash <> FExpectedLastTranscriptHash) then
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

function TVoiceStudioMidiStream.GetTickCount: TWfcMidiStreamCount;
begin if FMidi = nil then Result := 0 else Result := FMidi.TickCount end;

function TVoiceStudioMidiStream.GetEmittedBytes: TWfcMidiStreamCount;
begin if FMidi = nil then Result := 0 else Result := FMidi.EmittedBytes end;

function TVoiceStudioMidiStream.CopyLatestCell(
  out ACell: TVoiceStudioCell): Boolean;
begin
  Result := FHasLatest;
  if Result then ACell := CopyVoiceStudioCell(FLatest)
  else ACell := Default(TVoiceStudioCell);
end;

function TVoiceStudioMidiStream.NextBytes(
  out ABytes: TWfcMidiBytes): TWfcMusicArrangementStep;
var
  LCell: TVoiceStudioCell;
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

    LStep := FFrames.NextCell(LCell);
    case LStep of
      wmaspProduced:
        begin
          FLatest := CopyVoiceStudioCell(LCell);
          FHasLatest := True;
          FMidi.AdmitFrame(LCell.Frame, VOICE_STUDIO_QUANTUM);
          HashCell(FFrameSignature, LCell);
          Inc(FFrameCount);
          FMidi.ReadBytes(WFC_MIDI_STREAM_BLOCK_BYTES, ABytes);
          Exit(wmaspProduced);
        end;
      wmaspCompleted:
        begin
          FMidi.EndInput;
          if FMidi.ReadBytes(WFC_MIDI_STREAM_BLOCK_BYTES, ABytes) then
            Exit(wmaspProduced);
          if FMidi.Finished and VerifyCompletion then
          begin
            FStatus := wmasCompleted;
            Exit(wmaspCompleted);
          end;
          if FStatus = wmasFailed then Exit(wmaspFailed);
          if FMidi.Failed then
            Fail('core MIDI replay failed after end of input')
          else
            Fail('MIDI end of input made no progress');
          Exit(wmaspFailed);
        end;
      wmaspCancelled:
        begin
          FMidi.Cancel;
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
        Fail('frame source returned an unknown replay step');
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

procedure TVoiceStudioMidiStream.Cancel;
begin
  if FStatus in [wmasCompleted, wmasCancelled, wmasFailed] then Exit;
  FFrames.Cancel;
  FMidi.Cancel;
  FStatus := wmasCancelled;
  FFailure := '';
end;

end.
