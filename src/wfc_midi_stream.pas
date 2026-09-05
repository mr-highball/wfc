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
unit wfc_midi_stream;

{$mode delphi}{$H+}

interface

uses wfc_midi_smf;

type
  {$IFDEF PAS2JS}
  TWfcMidiStreamCount = NativeInt;
  {$ELSE}
  TWfcMidiStreamCount = Int64;
  {$ENDIF}

const
  WFC_MIDI_STREAM_VERSION = 1;
  WFC_MIDI_STREAM_BLOCK_BYTES = 4096;
  WFC_MIDI_STREAM_HEADER_BYTES = 22;
  WFC_MIDI_STREAM_MAX_SAFE_INTEGER: TWfcMidiStreamCount = 9007199254740991;
  WFC_MIDI_STREAM_MAX_TRACK_BYTES: TWfcMidiStreamCount = 4294967295;

type
  EWfcMidiStream = class(EWfcMidiSmf);

  { Immutable caller-owned track summary, made only by a completed counter.
    ByteCount is MTrk data, excluding the 22-byte format-0 file/chunk header.
    EventCount includes the final EOT but excludes generated delay bridges;
    BridgeCount counts those synthetic empty text events separately.

    Signature is a versioned FNV32 logical-event fingerprint, NOT a
    cryptographic commitment or proof of exact replay. Applications requiring
    stronger authentication must supply their own separately trusted proof. }
  TWfcMidiTrackPlan = class
  private
    FEndTick, FByteCount, FEventCount, FBridgeCount: TWfcMidiStreamCount;
    FSignature: Cardinal;
    constructor Create(const AEndTick, AByteCount, AEventCount,
      ABridgeCount: TWfcMidiStreamCount; const ASignature: Cardinal);
  public
    property EndTick: TWfcMidiStreamCount read FEndTick;
    property ByteCount: TWfcMidiStreamCount read FByteCount;
    property EventCount: TWfcMidiStreamCount read FEventCount;
    property BridgeCount: TWfcMidiStreamCount read FBridgeCount;
    property Signature: Cardinal read FSignature;
  end;

  { One pass over nondecreasing absolute ticks. Incoming DeltaTicks must be
    exactly zero; the caller cannot submit EOT. Channel/meta/SysEx validation
    follows the existing canonical SMF adapter without its whole-file memory
    policy caps. Payloads still fit the SMF four-byte VLQ and Integer indexing.
    The counter retains no input array or timeline and does not clone payloads.

    Long gaps are represented by FF 01 00 empty text meta events, each at the
    maximum delta. Bridge count is calculated in O(1), without expanding them:
    for positive gap, bridges=(gap-1) div MAX_VLQ, leaving delta 1..MAX_VLQ.
    Signature hashes logical events rather than iterating generated bridges.
    Payload hashing remains linear in supplied payload bytes.

    Finish owns EOT and freezes the counter. Calling Finish again with the
    same end returns a fresh detached plan; further events/different end reject.
    All validation, overflow and allocation failures preserve counter state. }
  TWfcMidiTrackCounter = class
  private
    FLastTick, FByteCount, FEventCount, FBridgeCount: TWfcMidiStreamCount;
    FSignature: Cardinal;
    FFinished: Boolean;
  public
    constructor Create;
    procedure AppendEvent(const AAbsoluteTick: TWfcMidiStreamCount;
      const AEvent: TWfcMidiEvent);
    function Finish(const AEndTick: TWfcMidiStreamCount): TWfcMidiTrackPlan;
    property LastTick: TWfcMidiStreamCount read FLastTick;
    property ByteCount: TWfcMidiStreamCount read FByteCount;
    property EventCount: TWfcMidiStreamCount read FEventCount;
    property BridgeCount: TWfcMidiStreamCount read FBridgeCount;
    property Finished: Boolean read FFinished;
  end;

  { Format-0, one-track, forward-only SMF with explicit status on every event.
    Create copies plan scalars; the caller may free its plan immediately.
    Drain the initially pending 22-byte header, then AdmitEvent only when
    NeedsInput. Each admission validates first and clones only that event.
    ReadBytes accepts any positive Integer maximum, returns at most 4096
    caller-owned bytes, and returns nil on False. Long payloads and synthetic
    delay bridges are serialized incrementally, never assembled as a track.

    Finish must wait for NeedsInput. It validates the complete replay, including
    EOT, before queuing final bytes. Replay mismatches poison Failed, as do
    unexpected processing errors. Invalid arguments/state or admission staging
    allocation failures are retryable. A replay exceeding a plan budget can
    fail early; any already-returned file prefix must be discarded on failure.

    Cancel is terminal and discards pending bytes, without emitting EOT. It
    cannot turn a failed stream into success. Finish is idempotent after its
    successful admission when the same end tick is supplied. Finished becomes
    True only after every final byte is read. Destruction neither drains nor
    publishes, closes, or calls any host. Sequential, non-reentrant by design. }
  TWfcMidiFileStream = class
  private
    FCounter: TWfcMidiTrackCounter;
    FPlanEndTick, FPlanByteCount, FPlanEventCount, FPlanBridgeCount:
      TWfcMidiStreamCount;
    FPlanSignature: Cardinal;
    FEmittedBytes: TWfcMidiStreamCount;
    FPrefix: array[0..21] of Byte;
    FPrefixCount, FPrefixPosition, FDataPosition: Integer;
    FEvent: TWfcMidiEvent;
    FRemainingBridges: TWfcMidiStreamCount;
    FEventDelta: Cardinal;
    FQueueKind: Integer;
    FEventPrefixReady, FInputEnded, FFinished, FFailed, FCancelled: Boolean;
    function GetNeedsInput: Boolean;
    procedure RequireInput;
    procedure AddPrefixByte(const AByte: Byte);
    procedure AddPrefixVLQ(AValue: Cardinal);
    procedure AddPrefixBigEndian(AValue: TWfcMidiStreamCount; const ACount: Integer);
    procedure PrepareBridgePrefix;
    procedure PrepareEventPrefix;
    procedure CompleteQueue;
    procedure CheckReplayBudget(const ATick, ABytes, AEvents,
      ABridges: TWfcMidiStreamCount);
  public
    constructor Create(const ATicksPerQuarter: Integer; const APlan: TWfcMidiTrackPlan);
    destructor Destroy; override;
    procedure AdmitEvent(const AAbsoluteTick: TWfcMidiStreamCount;
      const AEvent: TWfcMidiEvent);
    function ReadBytes(const AMaxBytes: Integer; out ABytes: TWfcMidiBytes): Boolean;
    procedure Finish(const AEndTick: TWfcMidiStreamCount);
    procedure Cancel;
    property NeedsInput: Boolean read GetNeedsInput;
    property InputEnded: Boolean read FInputEnded;
    property Finished: Boolean read FFinished;
    property Failed: Boolean read FFailed;
    property Cancelled: Boolean read FCancelled;
    property EmittedBytes: TWfcMidiStreamCount read FEmittedBytes;
  end;

implementation

uses SysUtils;

const
  QUEUE_NONE = 0;
  QUEUE_HEADER = 1;
  QUEUE_EVENT = 2;

type
  TPreparedEvent = record
    Tick, ByteCount, EventCount, BridgeCount, AddedBridges: TWfcMidiStreamCount;
    Delta: Cardinal;
    Signature: Cardinal;
  end;

procedure StreamError(const AMessage: String);
begin
  raise EWfcMidiStream.Create('cannot stream Standard MIDI File: ' + AMessage);
end;

procedure CheckCount(const AValue: TWfcMidiStreamCount; const AName: String);
begin
  if (AValue < 0) or (AValue > WFC_MIDI_STREAM_MAX_SAFE_INTEGER) then
    StreamError(AName + ' exceeds the nonnegative exact integer envelope');
  {$IFDEF PAS2JS}
  if AValue <> Trunc(AValue) then StreamError(AName + ' must be an exact integer');
  {$ENDIF}
end;

procedure CheckInteger(const AValue: TWfcMidiStreamCount;
  const AMinimum, AMaximum: TWfcMidiStreamCount; const AName: String);
begin
  CheckCount(AValue, AName);
  if (AValue < AMinimum) or (AValue > AMaximum) then
    StreamError(AName + ' is outside its supported integer range');
end;

procedure HashByte(var AHash: Cardinal; const AByte: Byte);
{$PUSH}{$Q-}
var LValue: Cardinal;
begin
  LValue := AHash xor Cardinal(AByte);
  AHash := (LValue + (LValue shl 1) + (LValue shl 4) +
    (LValue shl 7) + (LValue shl 8) + (LValue shl 24)) and Cardinal($FFFFFFFF);
end;
{$POP}

procedure HashCount(var AHash: Cardinal; AValue: TWfcMidiStreamCount);
var I: Integer;
begin
  for I := 0 to 7 do
  begin HashByte(AHash, Byte(AValue mod 256)); AValue := AValue div 256; end;
end;

function VLQSize(AValue: Cardinal): Integer;
begin
  Result := 1;
  while AValue >= 128 do begin AValue := AValue shr 7; Inc(Result); end;
end;

function ValidateEvent(const AEvent: TWfcMidiEvent; const AOwnedEnd: Boolean): Integer;
var I, LChannelLength: Integer;
begin
  CheckInteger(AEvent.DeltaTicks, 0, 0, 'incoming delta ticks');
  CheckInteger(AEvent.Status, 0, 255, 'event status');
  CheckInteger(AEvent.MetaType, 0, 255, 'meta type');
  CheckInteger(Length(AEvent.Data), 0, WFC_MIDI_MAX_VARIABLE_LENGTH,
    'payload length');
  Result := Length(AEvent.Data);
  for I := 0 to Result - 1 do CheckInteger(AEvent.Data[I], 0, 255, 'payload byte');
  if (AEvent.Status >= $80) and (AEvent.Status <= $EF) then
  begin
    if AEvent.MetaType <> 0 then StreamError('channel event has a meta type');
    if (AEvent.Status and $F0 = $C0) or (AEvent.Status and $F0 = $D0) then
      LChannelLength := 1 else LChannelLength := 2;
    if Result <> LChannelLength then StreamError('channel event has the wrong payload length');
    for I := 0 to Result - 1 do
      if AEvent.Data[I] >= $80 then StreamError('channel data must be seven-bit');
    Exit;
  end;
  case AEvent.Status of
    $F0, $F7:
      if AEvent.MetaType <> 0 then StreamError('SysEx event has a meta type');
    $FF:
      begin
        if AEvent.MetaType >= $80 then StreamError('meta type must be seven-bit');
        case AEvent.MetaType of
          $2F:
            if not AOwnedEnd or (Result <> 0) then StreamError('Finish owns end-of-track');
          $51:
            begin
              if Result <> 3 then StreamError('tempo payload must contain three bytes');
              if (AEvent.Data[0] = 0) and (AEvent.Data[1] = 0) and
                (AEvent.Data[2] = 0) then StreamError('tempo cannot be zero');
            end;
          $58:
            if Result <> 4 then StreamError('time signature payload must contain four bytes');
        end;
      end;
  else StreamError('unsupported event status');
  end;
end;

function PrepareEvent(const ACounter: TWfcMidiTrackCounter;
  const ATick: TWfcMidiStreamCount; const AEvent: TWfcMidiEvent;
  const AOwnedEnd: Boolean): TPreparedEvent;
var LGap, LSize: TWfcMidiStreamCount; LLength, I: Integer;
begin
  CheckCount(ATick, 'absolute tick');
  if ATick < ACounter.FLastTick then StreamError('event ticks must not decrease');
  LLength := ValidateEvent(AEvent, AOwnedEnd);
  LGap := ATick - ACounter.FLastTick;
  Result.AddedBridges := 0;
  if LGap > 0 then
    Result.AddedBridges := (LGap - 1) div WFC_MIDI_MAX_VARIABLE_LENGTH;
  Result.Delta := Cardinal(LGap - Result.AddedBridges * WFC_MIDI_MAX_VARIABLE_LENGTH);
  LSize := Result.AddedBridges * 7 + VLQSize(Result.Delta) + 1 + LLength;
  if AEvent.Status = $FF then Inc(LSize);
  if AEvent.Status >= $F0 then Inc(LSize, VLQSize(Cardinal(LLength)));
  if LSize > WFC_MIDI_STREAM_MAX_TRACK_BYTES - ACounter.FByteCount then
    StreamError('MTrk byte count exceeds its unsigned 32-bit length');
  if ACounter.FEventCount = WFC_MIDI_STREAM_MAX_SAFE_INTEGER then
    StreamError('logical event count exceeds exact integer range');
  if Result.AddedBridges > WFC_MIDI_STREAM_MAX_SAFE_INTEGER - ACounter.FBridgeCount then
    StreamError('bridge count exceeds exact integer range');
  Result.Tick := ATick;
  Result.ByteCount := ACounter.FByteCount + LSize;
  Result.EventCount := ACounter.FEventCount + 1;
  Result.BridgeCount := ACounter.FBridgeCount + Result.AddedBridges;
  Result.Signature := ACounter.FSignature;
  HashCount(Result.Signature, ATick);
  HashByte(Result.Signature, AEvent.Status);
  HashByte(Result.Signature, AEvent.MetaType);
  HashCount(Result.Signature, LLength);
  for I := 0 to LLength - 1 do HashByte(Result.Signature, AEvent.Data[I]);
  if AOwnedEnd then
  begin
    HashCount(Result.Signature, Result.ByteCount);
    HashCount(Result.Signature, Result.EventCount);
    HashCount(Result.Signature, Result.BridgeCount);
  end;
end;

procedure CommitPrepared(const ACounter: TWfcMidiTrackCounter; const AValue: TPreparedEvent);
begin
  ACounter.FLastTick := AValue.Tick;
  ACounter.FByteCount := AValue.ByteCount;
  ACounter.FEventCount := AValue.EventCount;
  ACounter.FBridgeCount := AValue.BridgeCount;
  ACounter.FSignature := AValue.Signature;
end;

function EndEvent: TWfcMidiEvent;
begin
  Result := Default(TWfcMidiEvent);
  Result.Status := $FF; Result.MetaType := $2F;
end;

constructor TWfcMidiTrackPlan.Create(const AEndTick, AByteCount, AEventCount,
  ABridgeCount: TWfcMidiStreamCount; const ASignature: Cardinal);
begin
  inherited Create;
  FEndTick := AEndTick; FByteCount := AByteCount; FEventCount := AEventCount;
  FBridgeCount := ABridgeCount; FSignature := ASignature;
end;

constructor TWfcMidiTrackCounter.Create;
begin
  inherited Create;
  FSignature := Cardinal(2166136261);
  HashCount(FSignature, WFC_MIDI_STREAM_VERSION);
end;

procedure TWfcMidiTrackCounter.AppendEvent(const AAbsoluteTick: TWfcMidiStreamCount;
  const AEvent: TWfcMidiEvent);
var LPrepared: TPreparedEvent;
begin
  if FFinished then StreamError('counter is already finished');
  LPrepared := PrepareEvent(Self, AAbsoluteTick, AEvent, False);
  CommitPrepared(Self, LPrepared);
end;

function TWfcMidiTrackCounter.Finish(const AEndTick: TWfcMidiStreamCount): TWfcMidiTrackPlan;
var LPrepared: TPreparedEvent;
begin
  Result := nil;
  CheckCount(AEndTick, 'end tick');
  if FFinished then
  begin
    if AEndTick <> FLastTick then StreamError('finished counter end tick cannot change');
    Exit(TWfcMidiTrackPlan.Create(FLastTick, FByteCount, FEventCount, FBridgeCount, FSignature));
  end;
  LPrepared := PrepareEvent(Self, AEndTick, EndEvent, True);
  Result := TWfcMidiTrackPlan.Create(LPrepared.Tick, LPrepared.ByteCount,
    LPrepared.EventCount, LPrepared.BridgeCount, LPrepared.Signature);
  CommitPrepared(Self, LPrepared);
  FFinished := True;
end;

constructor TWfcMidiFileStream.Create(const ATicksPerQuarter: Integer;
  const APlan: TWfcMidiTrackPlan);
begin
  inherited Create;
  CheckInteger(ATicksPerQuarter, 1, $7FFF, 'ticks per quarter');
  if APlan = nil then StreamError('track plan cannot be nil');
  FPlanEndTick := APlan.EndTick; FPlanByteCount := APlan.ByteCount;
  FPlanEventCount := APlan.EventCount; FPlanBridgeCount := APlan.BridgeCount;
  FPlanSignature := APlan.Signature;
  FCounter := TWfcMidiTrackCounter.Create;
  AddPrefixByte(Ord('M')); AddPrefixByte(Ord('T')); AddPrefixByte(Ord('h')); AddPrefixByte(Ord('d'));
  AddPrefixBigEndian(6, 4);
  AddPrefixBigEndian(0, 2); AddPrefixBigEndian(1, 2);
  AddPrefixBigEndian(ATicksPerQuarter, 2);
  AddPrefixByte(Ord('M')); AddPrefixByte(Ord('T')); AddPrefixByte(Ord('r')); AddPrefixByte(Ord('k'));
  AddPrefixBigEndian(FPlanByteCount, 4);
  FQueueKind := QUEUE_HEADER;
end;

destructor TWfcMidiFileStream.Destroy;
begin
  FCounter.Free;
  inherited Destroy;
end;

function TWfcMidiFileStream.GetNeedsInput: Boolean;
begin
  Result := not (FInputEnded or FFailed or FCancelled) and (FQueueKind = QUEUE_NONE);
end;

procedure TWfcMidiFileStream.RequireInput;
begin
  if FFailed then StreamError('a previous replay or processing operation failed');
  if FCancelled then StreamError('stream is cancelled');
  if not NeedsInput then StreamError('drain pending bytes before admitting input');
end;

procedure TWfcMidiFileStream.AddPrefixByte(const AByte: Byte);
begin
  FPrefix[FPrefixCount] := AByte; Inc(FPrefixCount);
end;

procedure TWfcMidiFileStream.AddPrefixVLQ(AValue: Cardinal);
var LBytes: array[0..3] of Byte; LCount, I: Integer;
begin
  LCount := 1; LBytes[0] := Byte(AValue and $7F);
  while AValue >= 128 do
  begin
    AValue := AValue shr 7;
    LBytes[LCount] := Byte(AValue and $7F) or $80; Inc(LCount);
  end;
  for I := LCount - 1 downto 0 do AddPrefixByte(LBytes[I]);
end;

procedure TWfcMidiFileStream.AddPrefixBigEndian(AValue: TWfcMidiStreamCount;
  const ACount: Integer);
var I: Integer;
begin
  for I := ACount - 1 downto 0 do
  begin
    FPrefix[FPrefixCount + I] := Byte(AValue mod 256);
    AValue := AValue div 256;
  end;
  Inc(FPrefixCount, ACount);
end;

procedure TWfcMidiFileStream.PrepareBridgePrefix;
begin
  FPrefixCount := 0; FPrefixPosition := 0;
  AddPrefixVLQ(WFC_MIDI_MAX_VARIABLE_LENGTH);
  AddPrefixByte($FF); AddPrefixByte($01); AddPrefixByte(0);
  Dec(FRemainingBridges);
end;

procedure TWfcMidiFileStream.PrepareEventPrefix;
begin
  FPrefixCount := 0; FPrefixPosition := 0;
  AddPrefixVLQ(FEventDelta); AddPrefixByte(FEvent.Status);
  if FEvent.Status = $FF then AddPrefixByte(FEvent.MetaType);
  if FEvent.Status >= $F0 then AddPrefixVLQ(Cardinal(Length(FEvent.Data)));
  FEventPrefixReady := True;
end;

procedure TWfcMidiFileStream.CompleteQueue;
begin
  if FPrefixPosition < FPrefixCount then Exit;
  if FQueueKind = QUEUE_HEADER then FQueueKind := QUEUE_NONE
  else if (FQueueKind = QUEUE_EVENT) and (FRemainingBridges = 0) and
    FEventPrefixReady and (FDataPosition = Length(FEvent.Data)) then
  begin
    FEvent.Data := nil;
    FQueueKind := QUEUE_NONE;
    if FInputEnded then
    begin
      if FEmittedBytes <> WFC_MIDI_STREAM_HEADER_BYTES + FPlanByteCount then
        StreamError('serialized byte count differs from the track plan');
      FFinished := True;
    end;
  end;
end;

procedure TWfcMidiFileStream.CheckReplayBudget(const ATick, ABytes, AEvents,
  ABridges: TWfcMidiStreamCount);
begin
  if (ATick > FPlanEndTick) or (ABytes > FPlanByteCount - 4) or
    (AEvents >= FPlanEventCount) or (ABridges > FPlanBridgeCount) then
  begin
    FFailed := True;
    StreamError('event replay exceeds the declared track plan');
  end;
end;

procedure TWfcMidiFileStream.AdmitEvent(const AAbsoluteTick: TWfcMidiStreamCount;
  const AEvent: TWfcMidiEvent);
var LPrepared: TPreparedEvent; LCopy: TWfcMidiEvent; I: Integer;
begin
  RequireInput;
  LPrepared := PrepareEvent(FCounter, AAbsoluteTick, AEvent, False);
  CheckReplayBudget(LPrepared.Tick, LPrepared.ByteCount, LPrepared.EventCount,
    LPrepared.BridgeCount);
  LCopy := Default(TWfcMidiEvent);
  LCopy.Status := AEvent.Status; LCopy.MetaType := AEvent.MetaType;
  SetLength(LCopy.Data, Length(AEvent.Data));
  for I := 0 to High(LCopy.Data) do LCopy.Data[I] := AEvent.Data[I];
  FEvent := LCopy;
  FRemainingBridges := LPrepared.AddedBridges;
  FEventDelta := LPrepared.Delta;
  FPrefixCount := 0; FPrefixPosition := 0; FDataPosition := 0;
  FEventPrefixReady := False; FQueueKind := QUEUE_EVENT;
  CommitPrepared(FCounter, LPrepared);
end;

function TWfcMidiFileStream.ReadBytes(const AMaxBytes: Integer;
  out ABytes: TWfcMidiBytes): Boolean;
var LLimit, LCount, LAvailable, I: Integer;
begin
  ABytes := nil;
  CheckInteger(AMaxBytes, 1, High(Integer), 'maximum output bytes');
  if FFailed then StreamError('a previous replay or processing operation failed');
  if FCancelled or FFinished or (FQueueKind = QUEUE_NONE) then Exit(False);
  LLimit := AMaxBytes;
  if LLimit > WFC_MIDI_STREAM_BLOCK_BYTES then LLimit := WFC_MIDI_STREAM_BLOCK_BYTES;
  SetLength(ABytes, LLimit);
  LCount := 0;
  try
    while (LCount < LLimit) and (FQueueKind <> QUEUE_NONE) do
    begin
      if FPrefixPosition < FPrefixCount then
      begin
        LAvailable := FPrefixCount - FPrefixPosition;
        if LAvailable > LLimit - LCount then LAvailable := LLimit - LCount;
        for I := 0 to LAvailable - 1 do ABytes[LCount + I] := FPrefix[FPrefixPosition + I];
        Inc(FPrefixPosition, LAvailable); Inc(LCount, LAvailable);
        Inc(FEmittedBytes, LAvailable);
      end
      else if FRemainingBridges > 0 then PrepareBridgePrefix
      else if (FQueueKind = QUEUE_EVENT) and not FEventPrefixReady then PrepareEventPrefix
      else if FDataPosition < Length(FEvent.Data) then
      begin
        LAvailable := Length(FEvent.Data) - FDataPosition;
        if LAvailable > LLimit - LCount then LAvailable := LLimit - LCount;
        for I := 0 to LAvailable - 1 do ABytes[LCount + I] := FEvent.Data[FDataPosition + I];
        Inc(FDataPosition, LAvailable); Inc(LCount, LAvailable);
        Inc(FEmittedBytes, LAvailable);
      end;
      CompleteQueue;
    end;
    SetLength(ABytes, LCount);
    Result := LCount <> 0;
  except
    ABytes := nil;
    FFailed := True;
    raise;
  end;
end;

procedure TWfcMidiFileStream.Finish(const AEndTick: TWfcMidiStreamCount);
var LPrepared: TPreparedEvent;
begin
  CheckCount(AEndTick, 'end tick');
  if FFailed then StreamError('a previous replay or processing operation failed');
  if FCancelled then StreamError('stream is cancelled');
  if FInputEnded then
  begin
    if AEndTick <> FPlanEndTick then StreamError('finished stream end tick cannot change');
    Exit;
  end;
  RequireInput;
  LPrepared := PrepareEvent(FCounter, AEndTick, EndEvent, True);
  if (LPrepared.Tick <> FPlanEndTick) or (LPrepared.ByteCount <> FPlanByteCount) or
    (LPrepared.EventCount <> FPlanEventCount) or (LPrepared.BridgeCount <> FPlanBridgeCount) or
    (LPrepared.Signature <> FPlanSignature) then
  begin
    FFailed := True;
    StreamError('final replay fingerprint or counts differ from the track plan');
  end;
  FEvent := EndEvent;
  FRemainingBridges := LPrepared.AddedBridges;
  FEventDelta := LPrepared.Delta;
  FPrefixCount := 0; FPrefixPosition := 0; FDataPosition := 0;
  FEventPrefixReady := False; FQueueKind := QUEUE_EVENT;
  CommitPrepared(FCounter, LPrepared);
  FCounter.FFinished := True;
  FInputEnded := True;
end;

procedure TWfcMidiFileStream.Cancel;
begin
  if FFinished or FCancelled then Exit;
  FCancelled := True;
  FQueueKind := QUEUE_NONE;
  FEvent.Data := nil;
  FRemainingBridges := 0;
  FPrefixCount := 0; FPrefixPosition := 0;
end;

end.
