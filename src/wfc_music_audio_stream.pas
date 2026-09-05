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
unit wfc_music_audio_stream;

{$mode delphi}{$H+}

interface

uses
  wfc_music_audio;

type
  {$IFDEF PAS2JS}
  TWfcMusicAudioStreamCount = NativeInt;
  {$ELSE}
  TWfcMusicAudioStreamCount = Int64;
  {$ENDIF}

const
  WFC_MUSIC_AUDIO_STREAM_VERSION = 1;
  WFC_MUSIC_AUDIO_STREAM_BLOCK_BYTES = 4096;
  WFC_MUSIC_AUDIO_STREAM_MAX_SAFE_INTEGER: TWfcMusicAudioStreamCount =
    9007199254740991;
  { Reserve the complete RF64 header within the exact integer envelope. }
  WFC_MUSIC_AUDIO_STREAM_MAX_FRAMES: TWfcMusicAudioStreamCount =
    (9007199254740991 - 80) div 2;

type
  EWfcMusicAudioStream = class(EWfcMusicAudio);

  { The caller owns the sink. WriteBytes synchronously consumes the whole
    borrowed block or raises; it must not retain a reference to that block.
    A raising sink may already have written a prefix: rollback is impossible.
    Implementations may write to a file, browser stream, device, or memory.
    This class itself introduces no host or filesystem dependency. }
  TWfcMusicAudioByteSink = class
  public
    procedure WriteBytes(const ABytes: array of Byte); virtual; abstract;
  end;

  { Sequential mono PCM16 encoder with a known final length and no seeking.
    Construction writes a complete header only after argument validation.
    RIFF/WAVE uses 44 bytes; larger files use an 80-byte RF64 header with ds64.
    At most BLOCK_BYTES PCM bytes are buffered, regardless of total duration.

    Nil/rate/length errors reject an append before writing and do not poison
    the stream. Short Finish is similarly recoverable. A sink exception sets
    Failed, rethrows the original exception, and forbids any later write.
    FrameCount includes only blocks whose sink call returned successfully;
    a failed sink call may have written an unknown partial block physically.

    Finish is idempotent after exact completion and writes no extra bytes.
    The destructor neither finishes the stream nor frees/closes the sink.
    The caller must retain the sink until this object has been released.
    Calls are sequential and non-reentrant, not thread-safe. }
  TWfcMusicWaveStream = class
  strict private
    FSink: TWfcMusicAudioByteSink;
    FSampleRate: Integer;
    FExpectedFrames: TWfcMusicAudioStreamCount;
    FFrameCount: TWfcMusicAudioStreamCount;
    FFinished: Boolean;
    FFailed: Boolean;
    FWriting: Boolean;
    FIsRF64: Boolean;
    FBuffer: TWfcMusicAudioBytes;
    procedure CheckWritable;
    procedure WriteBuffer;
    procedure WriteHeader;
  public
    constructor Create(const ASink: TWfcMusicAudioByteSink;
      const ASampleRate: Integer;
      const AExpectedFrames: TWfcMusicAudioStreamCount);
    procedure AppendClip(const AClip: TWfcMusicPcm16Clip);
    { Borrowed samples are consumed synchronously with the same validation,
      bounded writes and failure accounting as AppendClip. No clip allocation
      or preview-duration limit applies to this direct PCM entry point. }
    procedure AppendSamples(const ASamples: array of TWfcMusicPcm16Sample);
    procedure Finish;
    property SampleRate: Integer read FSampleRate;
    property FrameCount: TWfcMusicAudioStreamCount read FFrameCount;
    property ExpectedFrames: TWfcMusicAudioStreamCount read FExpectedFrames;
    property Finished: Boolean read FFinished;
    property Failed: Boolean read FFailed;
    property IsRF64: Boolean read FIsRF64;
  end;

implementation

uses
  SysUtils;

const
  WAVE_DWORD_MAX: TWfcMusicAudioStreamCount = 4294967295;
  RIFF_HEADER_BYTES = 44;
  RF64_HEADER_BYTES = 80;

procedure StreamError(const AMessage: String);
begin
  raise EWfcMusicAudioStream.Create('cannot stream WFC music WAVE: ' +
    AMessage);
end;

procedure PutAscii(var ABytes: TWfcMusicAudioBytes;
  const AOffset: Integer; const AText: String);
var
  I: Integer;
begin
  for I := 1 to Length(AText) do
    ABytes[AOffset + I - 1] := Byte(Ord(AText[I]));
end;

procedure PutUnsignedLE(var ABytes: TWfcMusicAudioBytes;
  const AOffset, AByteCount: Integer;
  AValue: TWfcMusicAudioStreamCount);
var
  I: Integer;
begin
  { Division by a power of two is exact throughout the shared safe range.
    Do not route these counts through 32-bit bitwise operators. }
  for I := 0 to AByteCount - 1 do
  begin
    ABytes[AOffset + I] := Byte(AValue mod 256);
    AValue := AValue div 256;
  end;
end;

constructor TWfcMusicWaveStream.Create(const ASink: TWfcMusicAudioByteSink;
  const ASampleRate: Integer;
  const AExpectedFrames: TWfcMusicAudioStreamCount);
begin
  inherited Create;
  if ASink = nil then
    StreamError('sink cannot be nil');
  {$IFDEF PAS2JS}
  if ASampleRate <> Trunc(ASampleRate) then
    StreamError('sample rate must be an exact integer');
  {$ENDIF}
  if (ASampleRate < WFC_MUSIC_AUDIO_MIN_SAMPLE_RATE) or
    (ASampleRate > WFC_MUSIC_AUDIO_MAX_SAMPLE_RATE) then
    StreamError(Format('sample rate must be from %d through %d',
      [WFC_MUSIC_AUDIO_MIN_SAMPLE_RATE, WFC_MUSIC_AUDIO_MAX_SAMPLE_RATE]));
  if (AExpectedFrames < 0) or
    (AExpectedFrames > WFC_MUSIC_AUDIO_STREAM_MAX_FRAMES) then
    StreamError('expected frames exceed the exact file-size envelope');
  {$IFDEF PAS2JS}
  if AExpectedFrames <> Trunc(AExpectedFrames) then
    StreamError('expected frames must be an exact integer');
  {$ENDIF}
  FSink := ASink;
  FSampleRate := ASampleRate;
  FExpectedFrames := AExpectedFrames;
  FIsRF64 := AExpectedFrames > ((WAVE_DWORD_MAX - 36) div 2);
  WriteHeader;
end;

procedure TWfcMusicWaveStream.CheckWritable;
begin
  if FWriting then StreamError('sink callbacks cannot reenter the stream');
  if FFailed then StreamError('a previous sink write failed');
  if FFinished then StreamError('stream has already finished');
end;

procedure TWfcMusicWaveStream.WriteBuffer;
begin
  FWriting := True;
  try
    try
      FSink.WriteBytes(FBuffer);
    except
      FFailed := True;
      raise;
    end;
  finally
    FWriting := False;
  end;
end;

procedure TWfcMusicWaveStream.WriteHeader;
var
  LDataBytes: TWfcMusicAudioStreamCount;
  LFormatOffset: Integer;
  LDataOffset: Integer;
begin
  LDataBytes := FExpectedFrames * 2;
  if FIsRF64 then
  begin
    { RF64 compatibility layout: EBU Tech 3306 v1.1 section 3.4 / Annex A.2.
      https://tech.ebu.ch/files/live/sites/tech/files/shared/tech/tech3306v1_1.pdf
      This is not a BWF/MBWF/BW64 metadata implementation. No fact chunk is
      needed for integer PCM; ds64 still carries the exact frame count. }
    SetLength(FBuffer, RF64_HEADER_BYTES);
    PutAscii(FBuffer, 0, 'RF64');
    PutUnsignedLE(FBuffer, 4, 4, WAVE_DWORD_MAX);
    PutAscii(FBuffer, 8, 'WAVE');
    PutAscii(FBuffer, 12, 'ds64');
    PutUnsignedLE(FBuffer, 16, 4, 28);
    PutUnsignedLE(FBuffer, 20, 8, LDataBytes + RF64_HEADER_BYTES - 8);
    PutUnsignedLE(FBuffer, 28, 8, LDataBytes);
    PutUnsignedLE(FBuffer, 36, 8, FExpectedFrames);
    PutUnsignedLE(FBuffer, 44, 4, 0);
    LFormatOffset := 48;
    LDataOffset := 72;
  end
  else
  begin
    SetLength(FBuffer, RIFF_HEADER_BYTES);
    PutAscii(FBuffer, 0, 'RIFF');
    PutUnsignedLE(FBuffer, 4, 4, LDataBytes + RIFF_HEADER_BYTES - 8);
    PutAscii(FBuffer, 8, 'WAVE');
    LFormatOffset := 12;
    LDataOffset := 36;
  end;
  PutAscii(FBuffer, LFormatOffset, 'fmt ');
  PutUnsignedLE(FBuffer, LFormatOffset + 4, 4, 16);
  PutUnsignedLE(FBuffer, LFormatOffset + 8, 2, 1);
  PutUnsignedLE(FBuffer, LFormatOffset + 10, 2, 1);
  PutUnsignedLE(FBuffer, LFormatOffset + 12, 4, FSampleRate);
  PutUnsignedLE(FBuffer, LFormatOffset + 16, 4, FSampleRate * 2);
  PutUnsignedLE(FBuffer, LFormatOffset + 20, 2, 2);
  PutUnsignedLE(FBuffer, LFormatOffset + 22, 2, 16);
  PutAscii(FBuffer, LDataOffset, 'data');
  if FIsRF64 then
    PutUnsignedLE(FBuffer, LDataOffset + 4, 4, WAVE_DWORD_MAX)
  else
    PutUnsignedLE(FBuffer, LDataOffset + 4, 4, LDataBytes);
  WriteBuffer;
end;

procedure TWfcMusicWaveStream.AppendClip(const AClip: TWfcMusicPcm16Clip);
var
  I: Integer;
  LOffset: Integer;
  LFrames: Integer;
  LSample: Integer;
begin
  CheckWritable;
  if AClip = nil then StreamError('clip cannot be nil');
  if AClip.SampleRate <> FSampleRate then
    StreamError('clip sample rate does not match the stream');
  if AClip.FrameCount > FExpectedFrames - FFrameCount then
    StreamError('clip exceeds the remaining declared frame count');

  LOffset := 0;
  while LOffset < AClip.FrameCount do
  begin
    LFrames := AClip.FrameCount - LOffset;
    if LFrames > WFC_MUSIC_AUDIO_STREAM_BLOCK_BYTES div 2 then
      LFrames := WFC_MUSIC_AUDIO_STREAM_BLOCK_BYTES div 2;
    SetLength(FBuffer, LFrames * 2);
    for I := 0 to LFrames - 1 do
    begin
      LSample := AClip.SampleAt(LOffset + I);
      if LSample < 0 then Inc(LSample, 65536);
      FBuffer[I * 2] := Byte(LSample and $FF);
      FBuffer[I * 2 + 1] := Byte(LSample shr 8);
    end;
    WriteBuffer;
    FFrameCount := FFrameCount + LFrames;
    Inc(LOffset, LFrames);
  end;
end;

procedure TWfcMusicWaveStream.AppendSamples(
  const ASamples: array of TWfcMusicPcm16Sample);
var
  I, LOffset, LFrames, LSample: Integer;
begin
  CheckWritable;
  if Length(ASamples) > High(Integer) then
    StreamError('sample block count exceeds Integer');
  if Length(ASamples) > FExpectedFrames - FFrameCount then
    StreamError('sample block exceeds the remaining declared frame count');
  {$IFDEF PAS2JS}
  { Typed arrays supplied by a host can still contain malformed numeric data.
    Validate the complete borrowed block before the first sink side effect. }
  for I := 0 to High(ASamples) do
    if (ASamples[I] <> Trunc(ASamples[I])) or (ASamples[I] < -32768) or
      (ASamples[I] > 32767) then StreamError('sample must fit signed PCM16');
  {$ENDIF}
  LOffset := 0;
  while LOffset < Length(ASamples) do
  begin
    LFrames := Length(ASamples) - LOffset;
    if LFrames > WFC_MUSIC_AUDIO_STREAM_BLOCK_BYTES div 2 then
      LFrames := WFC_MUSIC_AUDIO_STREAM_BLOCK_BYTES div 2;
    SetLength(FBuffer, LFrames * 2);
    for I := 0 to LFrames - 1 do
    begin
      LSample := ASamples[LOffset + I];
      if LSample < 0 then Inc(LSample, 65536);
      FBuffer[I * 2] := Byte(LSample and $FF);
      FBuffer[I * 2 + 1] := Byte(LSample shr 8);
    end;
    WriteBuffer;
    FFrameCount := FFrameCount + LFrames;
    Inc(LOffset, LFrames);
  end;
end;

procedure TWfcMusicWaveStream.Finish;
begin
  if FWriting then StreamError('sink callbacks cannot reenter the stream');
  if FFailed then StreamError('a previous sink write failed');
  if FFinished then Exit;
  if FFrameCount <> FExpectedFrames then
    StreamError('stream is short of its declared frame count');
  FFinished := True;
end;

end.
