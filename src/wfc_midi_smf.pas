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
unit wfc_midi_smf;

{$mode delphi}{$H+}

interface

uses
  SysUtils;

const
  WFC_MIDI_SMF_VERSION = 1;
  WFC_MIDI_MAX_VARIABLE_LENGTH = Cardinal($0FFFFFFF);

type
  EWfcMidiSmf = class(Exception);

  TWfcMidiBytes = array of Byte;

  TWfcMidiEvent = record
    DeltaTicks: Cardinal;
    Status: Byte;
    MetaType: Byte;
    Data: TWfcMidiBytes;
  end;
  TWfcMidiEvents = array of TWfcMidiEvent;

  TWfcMidiTrack = record
    Events: TWfcMidiEvents;
    EndDeltaTicks: Cardinal;
  end;
  TWfcMidiTracks = array of TWfcMidiTrack;

  TWfcMidiFile = record
    Format: Word;
    TicksPerQuarter: Word;
    Tracks: TWfcMidiTracks;
  end;

  TWfcMidiReadLimits = record
    MaxFileBytes: Integer;
    MaxTrackBytes: Integer;
    MaxTracks: Integer;
    MaxEvents: Integer;
    MaxEventDataBytes: Integer;
  end;

function DefaultWfcMidiReadLimits: TWfcMidiReadLimits;

function MakeWfcMidiChannelEvent(const ADeltaTicks: Cardinal;
  const AStatus: Byte; const AData: array of Byte): TWfcMidiEvent;
function MakeWfcMidiMetaEvent(const ADeltaTicks: Cardinal;
  const AMetaType: Byte; const AData: array of Byte): TWfcMidiEvent;
function MakeWfcMidiSystemExclusiveEvent(const ADeltaTicks: Cardinal;
  const AStatus: Byte; const AData: array of Byte): TWfcMidiEvent;
function MakeWfcMidiTempoEvent(const ADeltaTicks,
  AMicrosecondsPerQuarter: Cardinal): TWfcMidiEvent;
function MakeWfcMidiTimeSignatureEvent(const ADeltaTicks: Cardinal;
  const ANumerator, ADenominatorPower, AMidiClocksPerClick,
  ANotatedThirtySecondsPerQuarter: Byte): TWfcMidiEvent;

function EncodeWfcMidiVariableLength(
  const AValue: Cardinal): TWfcMidiBytes;
function DecodeWfcMidiVariableLength(
  const ABytes: TWfcMidiBytes): Cardinal;

function EncodeWfcMidiFile(const AFile: TWfcMidiFile): TWfcMidiBytes;
function DecodeWfcMidiFile(const ABytes: TWfcMidiBytes): TWfcMidiFile;
function DecodeWfcMidiFileWithLimits(const ABytes: TWfcMidiBytes;
  const ALimits: TWfcMidiReadLimits): TWfcMidiFile;

implementation

const
  WFC_MIDI_DEFAULT_MAX_FILE_BYTES = 64 * 1024 * 1024;
  WFC_MIDI_DEFAULT_MAX_TRACK_BYTES = 16 * 1024 * 1024;
  WFC_MIDI_DEFAULT_MAX_TRACKS = 256;
  WFC_MIDI_DEFAULT_MAX_EVENTS = 1000000;
  WFC_MIDI_DEFAULT_MAX_EVENT_DATA_BYTES = 16 * 1024 * 1024;

type
  TByteWriter = record
    Bytes: TWfcMidiBytes;
    Count: Integer;
    Limit: Integer;
  end;

procedure MidiError(const AMessage: String);
begin
  raise EWfcMidiSmf.Create('invalid Standard MIDI File: ' + AMessage);
end;

function CheckedLength(const ALength: SizeInt;
  const ALabel: String): Integer;
begin
  if (ALength < 0) or
      ((ALength and (not SizeInt(High(Integer)))) <> 0) then
    MidiError(ALabel + ' exceeds the supported integer range');
  Result := Integer(ALength);
end;

procedure ValidateLimits(const ALimits: TWfcMidiReadLimits);
begin
  if ALimits.MaxFileBytes < 14 then
    MidiError('maximum file size must be at least 14 bytes');
  if ALimits.MaxTrackBytes < 4 then
    MidiError('maximum track size must be at least 4 bytes');
  if (ALimits.MaxTracks < 1) or (ALimits.MaxTracks > High(Word)) then
    MidiError('maximum track count must be from 1 through 65535');
  if ALimits.MaxEvents < 1 then
    MidiError('maximum event count must be positive');
  if ALimits.MaxEventDataBytes < 0 then
    MidiError('maximum event-data size cannot be negative');
end;

function DefaultWfcMidiReadLimits: TWfcMidiReadLimits;
begin
  Result.MaxFileBytes := WFC_MIDI_DEFAULT_MAX_FILE_BYTES;
  Result.MaxTrackBytes := WFC_MIDI_DEFAULT_MAX_TRACK_BYTES;
  Result.MaxTracks := WFC_MIDI_DEFAULT_MAX_TRACKS;
  Result.MaxEvents := WFC_MIDI_DEFAULT_MAX_EVENTS;
  Result.MaxEventDataBytes := WFC_MIDI_DEFAULT_MAX_EVENT_DATA_BYTES;
end;

function CopyBytes(const AData: array of Byte): TWfcMidiBytes;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AData));
  for I := 0 to Length(AData) - 1 do
    Result[I] := AData[I];
end;

function ChannelDataLength(const AStatus: Byte): Integer;
begin
  if (AStatus < $80) or (AStatus > $EF) then
    Exit(-1);
  case AStatus and $F0 of
    $C0, $D0: Result := 1;
  else
    Result := 2;
  end;
end;

procedure ValidateDelta(const ADeltaTicks: Cardinal);
begin
  if ADeltaTicks > WFC_MIDI_MAX_VARIABLE_LENGTH then
    MidiError('event delta exceeds the four-byte variable-length range');
end;

procedure ValidateEvent(const AEvent: TWfcMidiEvent;
  const AMaxEventDataBytes: Integer);
var
  I: Integer;
  LDataLength: Integer;
  LExpectedLength: Integer;
  LTempo: Cardinal;
begin
  ValidateDelta(AEvent.DeltaTicks);
  LDataLength := CheckedLength(Length(AEvent.Data),
    'event-data length');
  if LDataLength > AMaxEventDataBytes then
    MidiError('event data exceeds the configured limit');
  if Cardinal(LDataLength) > WFC_MIDI_MAX_VARIABLE_LENGTH then
    MidiError('event data exceeds the variable-length range');

  LExpectedLength := ChannelDataLength(AEvent.Status);
  if LExpectedLength >= 0 then
  begin
    if AEvent.MetaType <> 0 then
      MidiError('a channel event cannot have a meta type');
    if LDataLength <> LExpectedLength then
      MidiError('channel event has the wrong data length');
    for I := 0 to LDataLength - 1 do
      if AEvent.Data[I] >= $80 then
        MidiError('channel event data bytes must be seven-bit values');
    Exit;
  end;

  case AEvent.Status of
    $F0, $F7:
      begin
        if AEvent.MetaType <> 0 then
          MidiError('a system-exclusive event cannot have a meta type');
      end;
    $FF:
      begin
        if AEvent.MetaType >= $80 then
          MidiError('meta-event types must be seven-bit values');
        case AEvent.MetaType of
          $2F: MidiError('end-of-track is owned by the track structure');
          $51:
            begin
              if LDataLength <> 3 then
                MidiError('a tempo event must contain three data bytes');
              LTempo := (Cardinal(AEvent.Data[0]) shl 16) or
                (Cardinal(AEvent.Data[1]) shl 8) or
                Cardinal(AEvent.Data[2]);
              if LTempo = 0 then
                MidiError('tempo microseconds per quarter cannot be zero');
            end;
          $58:
            if LDataLength <> 4 then
              MidiError('a time-signature event must contain four data bytes');
        end;
      end;
  else
    MidiError('unsupported event status');
  end;
end;

function MakeWfcMidiChannelEvent(const ADeltaTicks: Cardinal;
  const AStatus: Byte; const AData: array of Byte): TWfcMidiEvent;
begin
  Result.DeltaTicks := ADeltaTicks;
  Result.Status := AStatus;
  Result.MetaType := 0;
  Result.Data := CopyBytes(AData);
  ValidateEvent(Result, High(Integer));
end;

function MakeWfcMidiMetaEvent(const ADeltaTicks: Cardinal;
  const AMetaType: Byte; const AData: array of Byte): TWfcMidiEvent;
begin
  Result.DeltaTicks := ADeltaTicks;
  Result.Status := $FF;
  Result.MetaType := AMetaType;
  Result.Data := CopyBytes(AData);
  ValidateEvent(Result, High(Integer));
end;

function MakeWfcMidiSystemExclusiveEvent(const ADeltaTicks: Cardinal;
  const AStatus: Byte; const AData: array of Byte): TWfcMidiEvent;
begin
  Result.DeltaTicks := ADeltaTicks;
  Result.Status := AStatus;
  Result.MetaType := 0;
  Result.Data := CopyBytes(AData);
  ValidateEvent(Result, High(Integer));
end;

function MakeWfcMidiTempoEvent(const ADeltaTicks,
  AMicrosecondsPerQuarter: Cardinal): TWfcMidiEvent;
begin
  if (AMicrosecondsPerQuarter = 0) or
      (AMicrosecondsPerQuarter > $FFFFFF) then
    MidiError('tempo must be from 1 through 16777215 microseconds per quarter');
  Result := MakeWfcMidiMetaEvent(ADeltaTicks, $51,
    [Byte(AMicrosecondsPerQuarter shr 16),
     Byte(AMicrosecondsPerQuarter shr 8),
     Byte(AMicrosecondsPerQuarter)]);
end;

function MakeWfcMidiTimeSignatureEvent(const ADeltaTicks: Cardinal;
  const ANumerator, ADenominatorPower, AMidiClocksPerClick,
  ANotatedThirtySecondsPerQuarter: Byte): TWfcMidiEvent;
begin
  Result := MakeWfcMidiMetaEvent(ADeltaTicks, $58,
    [ANumerator, ADenominatorPower, AMidiClocksPerClick,
     ANotatedThirtySecondsPerQuarter]);
end;

procedure InitializeWriter(out AWriter: TByteWriter;
  const ALimit: Integer);
begin
  AWriter.Bytes := nil;
  AWriter.Count := 0;
  AWriter.Limit := ALimit;
end;

procedure EnsureWriterCapacity(var AWriter: TByteWriter;
  const AAdditional: Integer);
var
  LNeeded: Integer;
  LNewCapacity: Integer;
begin
  if AAdditional < 0 then
    MidiError('negative byte append length');
  if AWriter.Count > AWriter.Limit - AAdditional then
    MidiError('encoded data exceeds the configured size limit');
  LNeeded := AWriter.Count + AAdditional;
  if LNeeded <= Length(AWriter.Bytes) then
    Exit;
  LNewCapacity := Length(AWriter.Bytes);
  if LNewCapacity < 64 then
    LNewCapacity := 64;
  while LNewCapacity < LNeeded do
  begin
    if LNewCapacity > AWriter.Limit div 2 then
    begin
      LNewCapacity := AWriter.Limit;
      Break;
    end;
    LNewCapacity := LNewCapacity * 2;
  end;
  if LNewCapacity < LNeeded then
    MidiError('encoded data exceeds the configured size limit');
  SetLength(AWriter.Bytes, LNewCapacity);
end;

procedure WriteByte(var AWriter: TByteWriter; const AValue: Byte);
begin
  EnsureWriterCapacity(AWriter, 1);
  AWriter.Bytes[AWriter.Count] := AValue;
  Inc(AWriter.Count);
end;

procedure WriteBytes(var AWriter: TByteWriter;
  const AValues: TWfcMidiBytes);
var
  I: Integer;
begin
  EnsureWriterCapacity(AWriter, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
  begin
    AWriter.Bytes[AWriter.Count] := AValues[I];
    Inc(AWriter.Count);
  end;
end;

procedure WriteAscii4(var AWriter: TByteWriter;
  const AText: String);
var
  I: Integer;
begin
  if Length(AText) <> 4 then
    MidiError('internal chunk identifier is not four bytes');
  for I := 1 to 4 do
    WriteByte(AWriter, Byte(Ord(AText[I])));
end;

procedure WriteBigEndianWord(var AWriter: TByteWriter;
  const AValue: Word);
begin
  WriteByte(AWriter, Byte(AValue shr 8));
  WriteByte(AWriter, Byte(AValue));
end;

procedure WriteBigEndianCardinal(var AWriter: TByteWriter;
  const AValue: Cardinal);
begin
  WriteByte(AWriter, Byte(AValue shr 24));
  WriteByte(AWriter, Byte(AValue shr 16));
  WriteByte(AWriter, Byte(AValue shr 8));
  WriteByte(AWriter, Byte(AValue));
end;

procedure WriteVariableLength(var AWriter: TByteWriter;
  const AValue: Cardinal);
var
  I: Integer;
  LBytes: array[0..3] of Byte;
  LCount: Integer;
  LValue: Cardinal;
begin
  if AValue > WFC_MIDI_MAX_VARIABLE_LENGTH then
    MidiError('value exceeds the four-byte variable-length range');
  LValue := AValue;
  LCount := 1;
  LBytes[3] := Byte(LValue and $7F);
  LValue := LValue shr 7;
  while LValue <> 0 do
  begin
    Inc(LCount);
    LBytes[4 - LCount] := Byte((LValue and $7F) or $80);
    LValue := LValue shr 7;
  end;
  for I := 4 - LCount to 3 do
    WriteByte(AWriter, LBytes[I]);
end;

function FinishWriter(var AWriter: TByteWriter): TWfcMidiBytes;
begin
  SetLength(AWriter.Bytes, AWriter.Count);
  Result := AWriter.Bytes;
  AWriter.Bytes := nil;
  AWriter.Count := 0;
end;

function EncodeWfcMidiVariableLength(
  const AValue: Cardinal): TWfcMidiBytes;
var
  LWriter: TByteWriter;
begin
  InitializeWriter(LWriter, 4);
  WriteVariableLength(LWriter, AValue);
  Result := FinishWriter(LWriter);
end;

function ReadByte(const ABytes: TWfcMidiBytes;
  var APosition: Integer; const AEndPosition: Integer): Byte;
begin
  if (APosition < 0) or (APosition >= AEndPosition) or
      (APosition >= Length(ABytes)) then
    MidiError('unexpected end of data');
  Result := ABytes[APosition];
  Inc(APosition);
end;

function ReadVariableLength(const ABytes: TWfcMidiBytes;
  var APosition: Integer; const AEndPosition: Integer): Cardinal;
var
  B: Byte;
  I: Integer;
  LFirst: Byte;
begin
  Result := 0;
  LFirst := 0;
  for I := 1 to 4 do
  begin
    B := ReadByte(ABytes, APosition, AEndPosition);
    if I = 1 then
      LFirst := B;
    Result := (Result shl 7) or Cardinal(B and $7F);
    if (B and $80) = 0 then
    begin
      if (I > 1) and ((LFirst and $7F) = 0) then
        MidiError('variable-length value is not minimally encoded');
      Exit;
    end;
  end;
  MidiError('variable-length value exceeds four bytes');
end;

function DecodeWfcMidiVariableLength(
  const ABytes: TWfcMidiBytes): Cardinal;
var
  LPosition: Integer;
begin
  if Length(ABytes) = 0 then
    MidiError('variable-length value is empty');
  LPosition := 0;
  Result := ReadVariableLength(ABytes, LPosition, Length(ABytes));
  if LPosition <> Length(ABytes) then
    MidiError('variable-length value has trailing bytes');
end;

procedure WriteEvent(var AWriter: TByteWriter;
  const AEvent: TWfcMidiEvent; const AMaxEventDataBytes: Integer);
begin
  ValidateEvent(AEvent, AMaxEventDataBytes);
  WriteVariableLength(AWriter, AEvent.DeltaTicks);
  WriteByte(AWriter, AEvent.Status);
  if AEvent.Status = $FF then
    WriteByte(AWriter, AEvent.MetaType);
  if AEvent.Status >= $F0 then
    WriteVariableLength(AWriter, Length(AEvent.Data));
  WriteBytes(AWriter, AEvent.Data);
end;

function EncodeTrack(const ATrack: TWfcMidiTrack;
  const ALimits: TWfcMidiReadLimits;
  var AEventCount: Integer): TWfcMidiBytes;
var
  I: Integer;
  LWriter: TByteWriter;
begin
  InitializeWriter(LWriter, ALimits.MaxTrackBytes);
  for I := 0 to Length(ATrack.Events) - 1 do
  begin
    if AEventCount = ALimits.MaxEvents then
      MidiError('event count exceeds the configured limit');
    Inc(AEventCount);
    WriteEvent(LWriter, ATrack.Events[I],
      ALimits.MaxEventDataBytes);
  end;
  if AEventCount = ALimits.MaxEvents then
    MidiError('event count exceeds the configured limit');
  Inc(AEventCount);
  ValidateDelta(ATrack.EndDeltaTicks);
  WriteVariableLength(LWriter, ATrack.EndDeltaTicks);
  WriteByte(LWriter, $FF);
  WriteByte(LWriter, $2F);
  WriteByte(LWriter, 0);
  Result := FinishWriter(LWriter);
end;

function EncodeWfcMidiFile(const AFile: TWfcMidiFile): TWfcMidiBytes;
var
  I: Integer;
  LEventCount: Integer;
  LLimits: TWfcMidiReadLimits;
  LTrackBytes: TWfcMidiBytes;
  LTrackCount: Integer;
  LWriter: TByteWriter;
begin
  LLimits := DefaultWfcMidiReadLimits;
  ValidateLimits(LLimits);
  LTrackCount := CheckedLength(Length(AFile.Tracks), 'track count');
  if (AFile.Format <> 0) and (AFile.Format <> 1) then
    MidiError('only format 0 and format 1 are supported');
  if ((AFile.Format = 0) and (LTrackCount <> 1)) or
      ((AFile.Format = 1) and (LTrackCount < 1)) then
    MidiError('track count does not match the file format');
  if LTrackCount > LLimits.MaxTracks then
    MidiError('track count exceeds the configured limit');
  if (AFile.TicksPerQuarter = 0) or
      (AFile.TicksPerQuarter > $7FFF) then
    MidiError('ticks per quarter must be from 1 through 32767');

  InitializeWriter(LWriter, LLimits.MaxFileBytes);
  WriteAscii4(LWriter, 'MThd');
  WriteBigEndianCardinal(LWriter, 6);
  WriteBigEndianWord(LWriter, AFile.Format);
  WriteBigEndianWord(LWriter, Word(LTrackCount));
  WriteBigEndianWord(LWriter, AFile.TicksPerQuarter);
  LEventCount := 0;
  for I := 0 to LTrackCount - 1 do
  begin
    LTrackBytes := EncodeTrack(AFile.Tracks[I], LLimits,
      LEventCount);
    WriteAscii4(LWriter, 'MTrk');
    WriteBigEndianCardinal(LWriter, Cardinal(Length(LTrackBytes)));
    WriteBytes(LWriter, LTrackBytes);
  end;
  Result := FinishWriter(LWriter);
end;

procedure RequireAscii4(const ABytes: TWfcMidiBytes;
  var APosition: Integer; const AExpected: String);
var
  I: Integer;
begin
  for I := 1 to 4 do
    if ReadByte(ABytes, APosition, Length(ABytes)) <>
        Byte(Ord(AExpected[I])) then
      MidiError('expected ' + AExpected + ' chunk');
end;

function ReadBigEndianWord(const ABytes: TWfcMidiBytes;
  var APosition: Integer): Word;
begin
  Result := (Word(ReadByte(ABytes, APosition, Length(ABytes))) shl 8) or
    Word(ReadByte(ABytes, APosition, Length(ABytes)));
end;

function ReadBigEndianCardinal(const ABytes: TWfcMidiBytes;
  var APosition: Integer): Cardinal;
begin
  Result := (Cardinal(ReadByte(ABytes, APosition, Length(ABytes))) shl 24) or
    (Cardinal(ReadByte(ABytes, APosition, Length(ABytes))) shl 16) or
    (Cardinal(ReadByte(ABytes, APosition, Length(ABytes))) shl 8) or
    Cardinal(ReadByte(ABytes, APosition, Length(ABytes)));
end;

function ReadEventData(const ABytes: TWfcMidiBytes;
  var APosition: Integer; const AEndPosition, ALength,
  AMaxEventDataBytes: Integer): TWfcMidiBytes;
var
  I: Integer;
begin
  if ALength < 0 then
    MidiError('negative event-data length');
  if ALength > AMaxEventDataBytes then
    MidiError('event data exceeds the configured limit');
  if ALength > AEndPosition - APosition then
    MidiError('event data exceeds its track chunk');
  Result := nil;
  SetLength(Result, ALength);
  for I := 0 to ALength - 1 do
    Result[I] := ReadByte(ABytes, APosition, AEndPosition);
end;

procedure AppendEvent(var AEvents: TWfcMidiEvents;
  const AEvent: TWfcMidiEvent);
var
  I: Integer;
  LIndex: Integer;
begin
  LIndex := CheckedLength(Length(AEvents), 'decoded event count');
  if LIndex = High(Integer) then
    MidiError('decoded event count exceeds the supported integer range');
  SetLength(AEvents, LIndex + 1);
  AEvents[LIndex].DeltaTicks := AEvent.DeltaTicks;
  AEvents[LIndex].Status := AEvent.Status;
  AEvents[LIndex].MetaType := AEvent.MetaType;
  SetLength(AEvents[LIndex].Data, Length(AEvent.Data));
  for I := 0 to Length(AEvent.Data) - 1 do
    AEvents[LIndex].Data[I] := AEvent.Data[I];
end;

function CardinalLengthToInteger(const ALength: Cardinal;
  const ALabel: String): Integer;
begin
  if ALength > Cardinal(High(Integer)) then
    MidiError(ALabel + ' exceeds the supported integer range');
  Result := Integer(ALength);
end;

procedure DecodeTrack(const ABytes: TWfcMidiBytes;
  var APosition: Integer; const ATrackLength: Integer;
  const ALimits: TWfcMidiReadLimits; var AEventCount: Integer;
  out ATrack: TWfcMidiTrack);
var
  B: Byte;
  I: Integer;
  LDataLength: Integer;
  LDeltaTicks: Cardinal;
  LEndPosition: Integer;
  LEvent: TWfcMidiEvent;
  LExpectedDataLength: Integer;
  LHasFirstData: Boolean;
  LLengthValue: Cardinal;
  LRunningStatus: Byte;
begin
  if ATrackLength > ALimits.MaxTrackBytes then
    MidiError('track data exceeds the configured limit');
  if ATrackLength > Length(ABytes) - APosition then
    MidiError('track chunk exceeds the file data');
  LEndPosition := APosition + ATrackLength;
  ATrack.Events := nil;
  ATrack.EndDeltaTicks := 0;
  LRunningStatus := 0;

  while APosition < LEndPosition do
  begin
    if AEventCount = ALimits.MaxEvents then
      MidiError('event count exceeds the configured limit');
    Inc(AEventCount);
    LDeltaTicks := ReadVariableLength(ABytes, APosition,
      LEndPosition);
    B := ReadByte(ABytes, APosition, LEndPosition);
    LHasFirstData := B < $80;
    if LHasFirstData then
    begin
      if LRunningStatus = 0 then
        MidiError('running-status data has no channel status');
      LEvent.Status := LRunningStatus;
    end
    else
    begin
      LEvent.Status := B;
      if ChannelDataLength(B) >= 0 then
        LRunningStatus := B
      else
        LRunningStatus := 0;
    end;
    LEvent.DeltaTicks := LDeltaTicks;
    LEvent.MetaType := 0;
    LEvent.Data := nil;

    LExpectedDataLength := ChannelDataLength(LEvent.Status);
    if LExpectedDataLength >= 0 then
    begin
      SetLength(LEvent.Data, LExpectedDataLength);
      I := 0;
      if LHasFirstData then
      begin
        LEvent.Data[0] := B;
        I := 1;
      end;
      while I < LExpectedDataLength do
      begin
        LEvent.Data[I] := ReadByte(ABytes, APosition,
          LEndPosition);
        if LEvent.Data[I] >= $80 then
          MidiError('channel event data bytes must be seven-bit values');
        Inc(I);
      end;
      ValidateEvent(LEvent, ALimits.MaxEventDataBytes);
      AppendEvent(ATrack.Events, LEvent);
      Continue;
    end;

    if LHasFirstData then
      MidiError('invalid running status');
    case LEvent.Status of
      $F0, $F7:
        begin
          LLengthValue := ReadVariableLength(ABytes, APosition,
            LEndPosition);
          LDataLength := CardinalLengthToInteger(LLengthValue,
            'system-exclusive data length');
          LEvent.Data := ReadEventData(ABytes, APosition,
            LEndPosition, LDataLength,
            ALimits.MaxEventDataBytes);
          ValidateEvent(LEvent, ALimits.MaxEventDataBytes);
          AppendEvent(ATrack.Events, LEvent);
        end;
      $FF:
        begin
          LEvent.MetaType := ReadByte(ABytes, APosition,
            LEndPosition);
          if LEvent.MetaType >= $80 then
            MidiError('meta-event types must be seven-bit values');
          LLengthValue := ReadVariableLength(ABytes, APosition,
            LEndPosition);
          LDataLength := CardinalLengthToInteger(LLengthValue,
            'meta-event data length');
          LEvent.Data := ReadEventData(ABytes, APosition,
            LEndPosition, LDataLength,
            ALimits.MaxEventDataBytes);
          if LEvent.MetaType = $2F then
          begin
            if LDataLength <> 0 then
              MidiError('end-of-track must have zero data length');
            if APosition <> LEndPosition then
              MidiError('data follows the end-of-track event');
            ATrack.EndDeltaTicks := LDeltaTicks;
            Exit;
          end;
          ValidateEvent(LEvent, ALimits.MaxEventDataBytes);
          AppendEvent(ATrack.Events, LEvent);
        end;
    else
      MidiError('unsupported event status');
    end;
  end;
  MidiError('track is missing its end-of-track event');
end;

function DecodeWfcMidiFileWithLimits(const ABytes: TWfcMidiBytes;
  const ALimits: TWfcMidiReadLimits): TWfcMidiFile;
var
  I: Integer;
  LEventCount: Integer;
  LHeaderLength: Cardinal;
  LPosition: Integer;
  LTrackCount: Word;
  LTrackLength: Cardinal;
begin
  ValidateLimits(ALimits);
  if Length(ABytes) > ALimits.MaxFileBytes then
    MidiError('file data exceeds the configured limit');
  if Length(ABytes) < 14 then
    MidiError('file is shorter than its header');
  LPosition := 0;
  RequireAscii4(ABytes, LPosition, 'MThd');
  LHeaderLength := ReadBigEndianCardinal(ABytes, LPosition);
  if LHeaderLength <> 6 then
    MidiError('header chunk length must be six');
  Result.Format := ReadBigEndianWord(ABytes, LPosition);
  LTrackCount := ReadBigEndianWord(ABytes, LPosition);
  Result.TicksPerQuarter := ReadBigEndianWord(ABytes, LPosition);
  if (Result.Format <> 0) and (Result.Format <> 1) then
    MidiError('only format 0 and format 1 are supported');
  if ((Result.Format = 0) and (LTrackCount <> 1)) or
      ((Result.Format = 1) and (LTrackCount < 1)) then
    MidiError('track count does not match the file format');
  if LTrackCount > ALimits.MaxTracks then
    MidiError('track count exceeds the configured limit');
  if (Result.TicksPerQuarter = 0) or
      ((Result.TicksPerQuarter and $8000) <> 0) then
    MidiError('only positive PPQN time division is supported');
  Result.Tracks := nil;
  SetLength(Result.Tracks, LTrackCount);
  LEventCount := 0;
  for I := 0 to LTrackCount - 1 do
  begin
    RequireAscii4(ABytes, LPosition, 'MTrk');
    LTrackLength := ReadBigEndianCardinal(ABytes, LPosition);
    DecodeTrack(ABytes, LPosition,
      CardinalLengthToInteger(LTrackLength, 'track length'),
      ALimits, LEventCount, Result.Tracks[I]);
  end;
  if LPosition <> Length(ABytes) then
    MidiError('file has trailing data');
end;

function DecodeWfcMidiFile(const ABytes: TWfcMidiBytes): TWfcMidiFile;
begin
  Result := DecodeWfcMidiFileWithLimits(ABytes,
    DefaultWfcMidiReadLimits);
end;

end.
