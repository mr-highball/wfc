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
program wfc_music_audio_stream_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc_music_audio,
  wfc_music_audio_stream;

type
  TTestProcedure = procedure;
  ESinkFailure = class(Exception);
  TRecordingSink = class(TWfcMusicAudioByteSink)
  public
    Bytes: TWfcMusicAudioBytes;
    Calls: Integer;
    MaxBlock: Integer;
    TotalBytes: TWfcMusicAudioStreamCount;
    FailOnCall: Integer;
    PartialFailure: Boolean;
    Reenter: TWfcMusicWaveStream;
    destructor Destroy; override;
    procedure WriteBytes(const ABytes: array of Byte); override;
  end;

var
  GChecks: Integer = 0;
  GFailures: Integer = 0;
  GDestroyedSinks: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GChecks);
  if ACondition then WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailures);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try
    ATest;
  except
    on E: Exception do
    begin
      Inc(GFailures);
      WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

destructor TRecordingSink.Destroy;
begin
  Inc(GDestroyedSinks);
  inherited Destroy;
end;

procedure TRecordingSink.WriteBytes(const ABytes: array of Byte);
var
  I: Integer;
  LCount: Integer;
  LOffset: Integer;
begin
  Inc(Calls);
  if Length(ABytes) > MaxBlock then MaxBlock := Length(ABytes);
  if (FailOnCall = Calls) and (not PartialFailure) then
    raise ESinkFailure.Create('original sink failure');
  LCount := Length(ABytes);
  if (FailOnCall = Calls) and (LCount > 1) then LCount := 1;
  LOffset := Length(Bytes);
  SetLength(Bytes, LOffset + LCount);
  for I := 0 to LCount - 1 do Bytes[LOffset + I] := ABytes[I];
  TotalBytes := TotalBytes + LCount;
  if Reenter <> nil then Reenter.Finish;
  if FailOnCall = Calls then
    raise ESinkFailure.Create('original sink failure');
end;

function SameBytes(const A, B: TWfcMusicAudioBytes): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to Length(A) - 1 do
    if A[I] <> B[I] then Exit(False);
  Result := True;
end;

function HexNibble(const AValue: Char): Integer;
begin
  if AValue in ['0'..'9'] then Exit(Ord(AValue) - Ord('0'));
  if AValue in ['A'..'F'] then Exit(Ord(AValue) - Ord('A') + 10);
  raise Exception.Create('invalid test hex');
end;

function HexBytes(const AValue: String): TWfcMusicAudioBytes;
var
  I: Integer;
begin
  if Length(AValue) mod 2 <> 0 then
    raise Exception.Create('odd test hex');
  Result := nil;
  SetLength(Result, Length(AValue) div 2);
  for I := 0 to Length(Result) - 1 do
    Result[I] := Byte(HexNibble(AValue[I * 2 + 1]) * 16 +
      HexNibble(AValue[I * 2 + 2]));
end;

function U32At(const ABytes: TWfcMusicAudioBytes;
  const AOffset: Integer): TWfcMusicAudioStreamCount;
begin
  Result := TWfcMusicAudioStreamCount(ABytes[AOffset]) +
    TWfcMusicAudioStreamCount(ABytes[AOffset + 1]) * 256 +
    TWfcMusicAudioStreamCount(ABytes[AOffset + 2]) * 65536 +
    TWfcMusicAudioStreamCount(ABytes[AOffset + 3]) * 16777216;
end;

function U64At(const ABytes: TWfcMusicAudioBytes;
  const AOffset: Integer): TWfcMusicAudioStreamCount;
begin
  Result := U32At(ABytes, AOffset) +
    U32At(ABytes, AOffset + 4) * TWfcMusicAudioStreamCount(4294967296);
end;

function PatternClip(const AStart, ACount, ASampleRate: Integer):
  TWfcMusicPcm16Clip;
const
  SAMPLES: array[0..4] of SmallInt = (-32768, -1, 0, 1, 32767);
var
  I: Integer;
  LSamples: TWfcMusicPcm16Samples;
begin
  SetLength(LSamples, ACount);
  for I := 0 to ACount - 1 do LSamples[I] := SAMPLES[(AStart + I) mod 5];
  Result := TWfcMusicPcm16Clip.Create(ASampleRate, LSamples);
end;

procedure ExpectAppendRejected(const AWriter: TWfcMusicWaveStream;
  const AClip: TWfcMusicPcm16Clip; const ALabel: String);
var
  LRaised: Boolean;
begin
  LRaised := False;
  try
    AWriter.AppendClip(AClip);
  except
    on E: EWfcMusicAudioStream do LRaised := True;
  end;
  Check(LRaised, ALabel);
end;

procedure ExpectFinishRejected(const AWriter: TWfcMusicWaveStream;
  const ALabel: String);
var
  LRaised: Boolean;
begin
  LRaised := False;
  try
    AWriter.Finish;
  except
    on E: EWfcMusicAudioStream do LRaised := True;
  end;
  Check(LRaised, ALabel);
end;

procedure TestClassicBytes;
const
  EMPTY_WAVE =
    '524946462400000057415645666D74201000000001000100' +
    '44AC000088580100020010006461746100000000';
var
  I: Integer;
  LSink: TRecordingSink;
  LWriter: TWfcMusicWaveStream;
  LClip: TWfcMusicPcm16Clip;
begin
  Check((WFC_MUSIC_AUDIO_STREAM_VERSION = 1) and
    (WFC_MUSIC_AUDIO_STREAM_BLOCK_BYTES = 4096),
    'stream version and bounded block size are explicit');
  for I := 0 to 1 do
  begin
    LSink := TRecordingSink.Create;
    LClip := PatternClip(0, I * 5, 44100);
    LWriter := nil;
    try
      LWriter := TWfcMusicWaveStream.Create(LSink, 44100, LClip.FrameCount);
      Check((LSink.Calls = 1) and (Length(LSink.Bytes) = 44) and
        (LWriter.FrameCount = 0) and (not LWriter.Finished) and
        (not LWriter.Failed) and (not LWriter.IsRF64),
        'constructor emits exactly one complete classic header');
      LWriter.AppendClip(LClip);
      LWriter.Finish;
      Check(SameBytes(LSink.Bytes, EncodeWfcMusicWave(LClip)),
        'stream equals the complete in-memory WAVE codec byte for byte');
      Check((LWriter.FrameCount = LClip.FrameCount) and
        (LWriter.ExpectedFrames = LClip.FrameCount) and LWriter.Finished and
        (LWriter.SampleRate = 44100),
        'exact appended count is required for completion');
      if I = 0 then
        Check(SameBytes(LSink.Bytes, HexBytes(EMPTY_WAVE)),
          'empty clip emits the independently pinned classic header')
      else
        Check(SameBytes(Copy(LSink.Bytes, 44, 10),
          HexBytes('0080FFFF00000100FF7F')),
          'signed PCM extrema are little-endian two-byte samples');
      LWriter.Finish;
      Check(LSink.Calls = 1 + I, 'idempotent Finish writes no trailer or duplicate header');
      ExpectAppendRejected(LWriter, LClip, 'append after Finish is rejected');
      Check(LSink.Calls = 1 + I, 'append after Finish cannot write bytes');
    finally
      LWriter.Free;
      LClip.Free;
      LSink.Free;
    end;
  end;
end;

procedure TestBlockAndClipBoundaries;
const
  PARTS: array[0..3] of Integer = (1000, 0, 2048, 1955);
var
  I: Integer;
  LOffset: Integer;
  LSink, LWholeSink: TRecordingSink;
  LWriter, LWholeWriter: TWfcMusicWaveStream;
  LClip, LWhole: TWfcMusicPcm16Clip;
begin
  LWhole := PatternClip(0, 5003, 32000);
  LSink := TRecordingSink.Create;
  LWholeSink := TRecordingSink.Create;
  LWriter := nil;
  LWholeWriter := nil;
  try
    LWriter := TWfcMusicWaveStream.Create(LSink, 32000, 5003);
    LWholeWriter := TWfcMusicWaveStream.Create(LWholeSink, 32000, 5003);
    LWholeWriter.AppendClip(LWhole);
    LWholeWriter.Finish;
    Check((LWholeSink.Calls = 4) and (LWholeSink.MaxBlock = 4096),
      'large clip is written in bounded 2048-frame blocks and a final tail');
    LOffset := 0;
    for I := Low(PARTS) to High(PARTS) do
    begin
      LClip := PatternClip(LOffset, PARTS[I], 32000);
      try
        LWriter.AppendClip(LClip);
      finally
        LClip.Free;
      end;
      Inc(LOffset, PARTS[I]);
      Check(LWriter.FrameCount = LOffset,
        'each independent clip advances only its exact appended frames');
    end;
    LWriter.Finish;
    Check(SameBytes(LSink.Bytes, LWholeSink.Bytes),
      'different clip and block partitioning yields identical PCM and header');
    Check(SameBytes(LSink.Bytes, EncodeWfcMusicWave(LWhole)),
      'chunked output equals the immutable whole-clip encoder');
    Check((LSink.MaxBlock <= WFC_MUSIC_AUDIO_STREAM_BLOCK_BYTES) and
      (LSink.TotalBytes = 44 + 5003 * 2),
      'total length is exact while individual writes remain bounded');
  finally
    LWholeWriter.Free;
    LWriter.Free;
    LWhole.Free;
    LWholeSink.Free;
    LSink.Free;
  end;
end;

procedure TestRecoverableInputErrors;
var
  LSink: TRecordingSink;
  LWriter: TWfcMusicWaveStream;
  LClip, LWrongRate, LExtra: TWfcMusicPcm16Clip;
begin
  LSink := TRecordingSink.Create;
  LWriter := nil;
  LClip := PatternClip(0, 4, 48000);
  LWrongRate := PatternClip(0, 4, 44100);
  LExtra := PatternClip(0, 4097, 48000);
  try
    LWriter := TWfcMusicWaveStream.Create(LSink, 48000, 4);
    ExpectAppendRejected(LWriter, nil, 'nil clip rejects before writing');
    ExpectAppendRejected(LWriter, LWrongRate, 'sample-rate mismatch rejects before writing');
    ExpectAppendRejected(LWriter, LExtra, 'oversized clip preflights its entire extent');
    ExpectFinishRejected(LWriter, 'short Finish rejects without changing state');
    Check((LSink.Calls = 1) and (LWriter.FrameCount = 0) and
      (not LWriter.Failed) and (not LWriter.Finished),
      'caller input errors are recoverable and write no partial prefix');
    LWriter.AppendClip(LClip);
    ExpectAppendRejected(LWriter, LClip, 'extra frames reject even at exact expected count');
    Check((LSink.Calls = 2) and (not LWriter.Failed) and
      (LWriter.FrameCount = 4),
      'overrun rejection keeps the fully accepted frame count');
    LWriter.Finish;
    Check(LWriter.Finished and SameBytes(LSink.Bytes, EncodeWfcMusicWave(LClip)),
      'valid retry after input errors can complete normally');
  finally
    LWriter.Free;
    LExtra.Free;
    LWrongRate.Free;
    LClip.Free;
    LSink.Free;
  end;
end;

procedure TestConstructorValidationAndOwnership;
var
  I: Integer;
  LDestroyed: Integer;
  LRate: Integer;
  LExpected: TWfcMusicAudioStreamCount;
  LSink: TRecordingSink;
  LWriter: TWfcMusicWaveStream;
  LRaised: Boolean;
begin
  LSink := TRecordingSink.Create;
  LDestroyed := GDestroyedSinks;
  try
    for I := 0 to 4 do
    begin
      LRate := 44100;
      LExpected := 0;
      case I of
        0: LRate := WFC_MUSIC_AUDIO_MIN_SAMPLE_RATE - 1;
        1: LRate := WFC_MUSIC_AUDIO_MAX_SAMPLE_RATE + 1;
        2: LExpected := -1;
        3: LExpected := WFC_MUSIC_AUDIO_STREAM_MAX_FRAMES + 1;
        4: LExpected := WFC_MUSIC_AUDIO_STREAM_MAX_SAFE_INTEGER;
      end;
      LWriter := nil;
      LRaised := False;
      try
        LWriter := TWfcMusicWaveStream.Create(LSink, LRate, LExpected);
      except
        on E: EWfcMusicAudioStream do LRaised := True;
      end;
      LWriter.Free;
      Check(LRaised and (LSink.Calls = 0),
        'invalid constructor argument rejects before the first sink write');
    end;
    LWriter := nil;
    LRaised := False;
    try
      LWriter := TWfcMusicWaveStream.Create(nil, 44100, 0);
    except
      on E: EWfcMusicAudioStream do LRaised := True;
    end;
    LWriter.Free;
    Check(LRaised, 'nil sink rejects construction');
    LWriter := TWfcMusicWaveStream.Create(LSink, 44100, 5);
    LWriter.Free;
    Check((LSink.Calls = 1) and (Length(LSink.Bytes) = 44),
      'destroying an incomplete writer neither finishes nor invents PCM');
    Check(GDestroyedSinks = LDestroyed,
      'constructor failures and writer destruction never own the caller sink');
    LSink.WriteBytes([7]);
    Check(LSink.Calls = 2, 'caller sink remains usable after writer destruction');
  finally
    LSink.Free;
  end;
  Check(GDestroyedSinks = LDestroyed + 1, 'caller frees its sink exactly once');
end;

procedure TestRF64Headers;
const
  FIRST_RF64 =
    '52463634FFFFFFFF57415645647336341C000000' +
    '2400000001000000DCFFFFFF00000000EEFFFF7F00000000' +
    '00000000666D7420100000000100010044AC000088580100' +
    '0200100064617461FFFFFFFF';
var
  LSink: TRecordingSink;
  LWriter: TWfcMusicWaveStream;
  LExpected: TWfcMusicAudioStreamCount;
  I: Integer;
begin
  for I := 0 to 4 do
  begin
    case I of
      0: LExpected := 2147483629;
      1: LExpected := 2147483630;
      2: LExpected := 2147483648;
      3: LExpected := 4294967296;
    else
      LExpected := WFC_MUSIC_AUDIO_STREAM_MAX_FRAMES;
    end;
    LSink := TRecordingSink.Create;
    LWriter := nil;
    try
      LWriter := TWfcMusicWaveStream.Create(LSink, 44100, LExpected);
      Check((LSink.Calls = 1) and (LWriter.FrameCount = 0) and
        (LWriter.ExpectedFrames = LExpected),
        'huge declared output allocates only its header');
      if I = 0 then
      begin
        Check((not LWriter.IsRF64) and (Length(LSink.Bytes) = 44) and
          (U32At(LSink.Bytes, 4) = 4294967294) and
          (U32At(LSink.Bytes, 40) = 4294967258),
          'last complete classic RIFF size before overflow stays RIFF');
      end
      else
      begin
        Check(LWriter.IsRF64 and (Length(LSink.Bytes) = 80) and
          (U32At(LSink.Bytes, 4) = 4294967295) and
          (U32At(LSink.Bytes, 76) = 4294967295),
          'RF64 replaces both legacy sizes with sentinel DWORDs');
        Check((U32At(LSink.Bytes, 16) = 28) and
          (U64At(LSink.Bytes, 20) = LExpected * 2 + 72) and
          (U64At(LSink.Bytes, 28) = LExpected * 2) and
          (U64At(LSink.Bytes, 36) = LExpected) and
          (U32At(LSink.Bytes, 44) = 0),
          'ds64 stores exact RIFF, PCM, sample counts and an empty size table');
        if I = 1 then
          Check(SameBytes(LSink.Bytes, HexBytes(FIRST_RF64)),
            'first RF64 header matches independently calculated golden bytes');
      end;
      ExpectFinishRejected(LWriter, 'header alone never counts as completed PCM');
      Check((not LWriter.Finished) and (not LWriter.Failed) and
        (LSink.Calls = 1), 'short huge output stays unfinished without further writes');
    finally
      LWriter.Free;
      LSink.Free;
    end;
  end;
end;

procedure TestSinkFailures;
var
  I: Integer;
  LDestroyed: Integer;
  LSink: TRecordingSink;
  LWriter: TWfcMusicWaveStream;
  LClip: TWfcMusicPcm16Clip;
  LRaised: Boolean;
  LCalls: Integer;
begin
  LClip := PatternClip(0, 5000, 44100);
  try
    for I := 0 to 1 do
    begin
      LSink := TRecordingSink.Create;
      LWriter := nil;
      try
        LSink.FailOnCall := 3;
        LSink.PartialFailure := I = 1;
        LWriter := TWfcMusicWaveStream.Create(LSink, 44100, 5000);
        LRaised := False;
        try
          LWriter.AppendClip(LClip);
        except
          on E: ESinkFailure do
            LRaised := E.Message = 'original sink failure';
        end;
        Check(LRaised, 'original sink exception type and message propagate');
        Check(LWriter.Failed and (not LWriter.Finished) and
          (LWriter.FrameCount = 2048),
          'failed block poisons the stream and counts only fully accepted blocks');
        Check(LSink.TotalBytes = 44 + 4096 + I,
          'throwing sink may physically accept an uncounted partial block');
        LCalls := LSink.Calls;
        LSink.FailOnCall := 0;
        ExpectAppendRejected(LWriter, LClip, 'append after sink failure rejects');
        ExpectFinishRejected(LWriter, 'Finish after sink failure rejects');
        Check((LSink.Calls = LCalls) and (not LWriter.Finished),
          'a poisoned stream never writes again or claims completion');
      finally
        LWriter.Free;
        LSink.Free;
      end;
    end;

    LSink := TRecordingSink.Create;
    LDestroyed := GDestroyedSinks;
    LWriter := nil;
    try
      LSink.FailOnCall := 1;
      LSink.PartialFailure := True;
      LRaised := False;
      try
        LWriter := TWfcMusicWaveStream.Create(LSink, 44100, 5000);
      except
        on E: ESinkFailure do LRaised := True;
      end;
      Check(LRaised and (LWriter = nil) and (LSink.TotalBytes = 1),
        'header write failure does not return a partially constructed writer');
      Check(GDestroyedSinks = LDestroyed,
        'managed constructor cleanup leaves the external sink caller-owned');
    finally
      LWriter.Free;
      LSink.Free;
    end;
  finally
    LClip.Free;
  end;
end;

procedure TestNonReentrantSink;
var
  LSink: TRecordingSink;
  LWriter: TWfcMusicWaveStream;
  LClip: TWfcMusicPcm16Clip;
begin
  LSink := TRecordingSink.Create;
  LWriter := nil;
  LClip := PatternClip(0, 5, 44100);
  try
    LWriter := TWfcMusicWaveStream.Create(LSink, 44100, 5);
    LSink.Reenter := LWriter;
    ExpectAppendRejected(LWriter, LClip, 'sink cannot call Finish during an active write');
    Check(LWriter.Failed and (not LWriter.Finished) and
      (LWriter.FrameCount = 0),
      'uncaught reentrant sink failure cannot publish finished state');
    LSink.Reenter := nil;
  finally
    LWriter.Free;
    LClip.Free;
    LSink.Free;
  end;
end;

begin
  WriteLn('WFC bounded-memory WAVE/RF64 stream conformance suite');
  WriteLn('===================================================');
  RunTest('classic WAVE byte parity', @TestClassicBytes);
  RunTest('bounded blocks and independent clip partitions', @TestBlockAndClipBoundaries);
  RunTest('recoverable preflight errors', @TestRecoverableInputErrors);
  RunTest('constructor validation and caller ownership', @TestConstructorValidationAndOwnership);
  RunTest('RIFF and RF64 size boundaries', @TestRF64Headers);
  RunTest('sink failure poisoning and constructor cleanup', @TestSinkFailures);
  RunTest('non-reentrant sink contract', @TestNonReentrantSink);
  WriteLn('===================================================');
  WriteLn(Format('%d checks, %d failures', [GChecks, GFailures]));
  if GFailures > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d WAVE stream checks failed', [GFailures]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
