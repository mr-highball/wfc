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
program wfc_midi_smf_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc_midi_smf;

type
  TTestProcedure = procedure;

const
  GOLDEN_FORMAT_0 =
    '4D546864000000060000000101E0' +
    '4D54726B0000001C' +
    '00FF510307A120' +
    '00FF580404021808' +
    '00903C64' +
    '8360803C00' +
    '00FF2F00';

  GOLDEN_FORMAT_1 =
    '4D54686400000006000100020060' +
    '4D54726B00000013' +
    '00FF510307A120' +
    '00FF580404021808' +
    '00FF2F00' +
    '4D54726B0000000F' +
    '00C000' +
    '00903C40' +
    '60803C00' +
    '00FF2F00';

  RUNNING_STATUS_INPUT =
    '4D54686400000006000000010060' +
    '4D54726B0000000B' +
    '00903C40' +
    '603C00' +
    '00FF2F00';

  RUNNING_STATUS_CANONICAL =
    '4D54686400000006000000010060' +
    '4D54726B0000000C' +
    '00903C40' +
    '60903C00' +
    '00FF2F00';

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GCheckCount);
  if ACondition then
    WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailureCount);
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
      Inc(GFailureCount);
      WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function HexValue(const ACharacter: Char): Integer;
begin
  if ACharacter in ['0'..'9'] then
    Result := Ord(ACharacter) - Ord('0')
  else if ACharacter in ['A'..'F'] then
    Result := Ord(ACharacter) - Ord('A') + 10
  else if ACharacter in ['a'..'f'] then
    Result := Ord(ACharacter) - Ord('a') + 10
  else
    Result := -1;
end;

function HexBytes(const AHex: String): TWfcMidiBytes;
var
  I: Integer;
  LHigh: Integer;
  LIndex: Integer;
  LLow: Integer;
begin
  if (Length(AHex) mod 2) <> 0 then
    raise Exception.Create('test hex has an odd length');
  Result := nil;
  SetLength(Result, Length(AHex) div 2);
  LIndex := 0;
  I := 1;
  while I <= Length(AHex) do
  begin
    LHigh := HexValue(AHex[I]);
    LLow := HexValue(AHex[I + 1]);
    if (LHigh < 0) or (LLow < 0) then
      raise Exception.Create('test hex has a non-hex character');
    Result[LIndex] := Byte((LHigh shl 4) or LLow);
    Inc(LIndex);
    Inc(I, 2);
  end;
end;

function BytesMatch(const A, B: TWfcMidiBytes): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to Length(A) - 1 do
    if A[I] <> B[I] then
      Exit(False);
  Result := True;
end;

procedure CheckDecodeRejected(const ABytes: TWfcMidiBytes;
  const AMessage: String);
var
  LFile: TWfcMidiFile;
  LRaised: Boolean;
begin
  LRaised := False;
  try
    try
      LFile := DecodeWfcMidiFile(ABytes);
      if LFile.Format = High(Word) then
        WriteLn('unreachable');
    except
      on E: EWfcMidiSmf do LRaised := True;
    end;
  finally
    LFile.Tracks := nil;
  end;
  Check(LRaised, AMessage);
end;

procedure CheckDecodeWithLimitsRejected(const ABytes: TWfcMidiBytes;
  const ALimits: TWfcMidiReadLimits; const AMessage: String);
var
  LFile: TWfcMidiFile;
  LRaised: Boolean;
begin
  LRaised := False;
  try
    try
      LFile := DecodeWfcMidiFileWithLimits(ABytes, ALimits);
      if LFile.Format = High(Word) then
        WriteLn('unreachable');
    except
      on E: EWfcMidiSmf do LRaised := True;
    end;
  finally
    LFile.Tracks := nil;
  end;
  Check(LRaised, AMessage);
end;

function BuildFormat0: TWfcMidiFile;
begin
  Result.Format := 0;
  Result.TicksPerQuarter := 480;
  SetLength(Result.Tracks, 1);
  SetLength(Result.Tracks[0].Events, 4);
  Result.Tracks[0].Events[0] :=
    MakeWfcMidiTempoEvent(0, 500000);
  Result.Tracks[0].Events[1] :=
    MakeWfcMidiTimeSignatureEvent(0, 4, 2, 24, 8);
  Result.Tracks[0].Events[2] :=
    MakeWfcMidiChannelEvent(0, $90, [$3C, $64]);
  Result.Tracks[0].Events[3] :=
    MakeWfcMidiChannelEvent(480, $80, [$3C, 0]);
  Result.Tracks[0].EndDeltaTicks := 0;
end;

function BuildFormat1: TWfcMidiFile;
begin
  Result.Format := 1;
  Result.TicksPerQuarter := 96;
  SetLength(Result.Tracks, 2);
  SetLength(Result.Tracks[0].Events, 2);
  Result.Tracks[0].Events[0] :=
    MakeWfcMidiTempoEvent(0, 500000);
  Result.Tracks[0].Events[1] :=
    MakeWfcMidiTimeSignatureEvent(0, 4, 2, 24, 8);
  Result.Tracks[0].EndDeltaTicks := 0;
  SetLength(Result.Tracks[1].Events, 3);
  Result.Tracks[1].Events[0] :=
    MakeWfcMidiChannelEvent(0, $C0, [0]);
  Result.Tracks[1].Events[1] :=
    MakeWfcMidiChannelEvent(0, $90, [$3C, $40]);
  Result.Tracks[1].Events[2] :=
    MakeWfcMidiChannelEvent(96, $80, [$3C, 0]);
  Result.Tracks[1].EndDeltaTicks := 0;
end;

procedure TestFormat0Golden;
var
  LDecoded: TWfcMidiFile;
  LEncoded: TWfcMidiBytes;
  LExpected: TWfcMidiBytes;
begin
  LExpected := HexBytes(GOLDEN_FORMAT_0);
  LEncoded := EncodeWfcMidiFile(BuildFormat0);
  Check(BytesMatch(LEncoded, LExpected),
    'format-0 writer matches the 50-byte golden file');
  LDecoded := DecodeWfcMidiFile(LExpected);
  Check((LDecoded.Format = 0) and
    (LDecoded.TicksPerQuarter = 480) and
    (Length(LDecoded.Tracks) = 1) and
    (Length(LDecoded.Tracks[0].Events) = 4),
    'format-0 reader preserves header and events');
  Check((LDecoded.Tracks[0].Events[0].MetaType = $51) and
    BytesMatch(LDecoded.Tracks[0].Events[0].Data,
      HexBytes('07A120')),
    'tempo is represented as exact microseconds-per-quarter bytes');
  Check(BytesMatch(EncodeWfcMidiFile(LDecoded), LExpected),
    'canonical format-0 bytes round-trip exactly');
end;

procedure TestFormat1Golden;
var
  LDecoded: TWfcMidiFile;
  LExpected: TWfcMidiBytes;
begin
  LExpected := HexBytes(GOLDEN_FORMAT_1);
  Check(BytesMatch(EncodeWfcMidiFile(BuildFormat1), LExpected),
    'format-1 writer matches the conductor and note-track golden file');
  LDecoded := DecodeWfcMidiFile(LExpected);
  Check((LDecoded.Format = 1) and
    (LDecoded.TicksPerQuarter = 96) and
    (Length(LDecoded.Tracks) = 2) and
    (Length(LDecoded.Tracks[0].Events) = 2) and
    (Length(LDecoded.Tracks[1].Events) = 3),
    'format-1 reader preserves both ordered tracks');
  Check((LDecoded.Tracks[1].Events[0].Status = $C0) and
    (Length(LDecoded.Tracks[1].Events[0].Data) = 1),
    'one-data-byte channel messages retain their arity');
  Check(BytesMatch(EncodeWfcMidiFile(LDecoded), LExpected),
    'canonical format-1 bytes round-trip exactly');
end;

procedure TestVariableLengths;
const
  VALUES: array[0..7] of Cardinal =
    (0, 127, 128, 480, 8192, 16383, 16384, $0FFFFFFF);
  HEXES: array[0..7] of String =
    ('00', '7F', '8100', '8360', 'C000', 'FF7F',
     '818000', 'FFFFFF7F');
var
  I: Integer;
  LRaised: Boolean;
begin
  for I := Low(VALUES) to High(VALUES) do
  begin
    Check(BytesMatch(EncodeWfcMidiVariableLength(VALUES[I]),
      HexBytes(HEXES[I])),
      Format('variable-length writer matches vector %d', [I]));
    Check(DecodeWfcMidiVariableLength(HexBytes(HEXES[I])) = VALUES[I],
      Format('variable-length reader matches vector %d', [I]));
  end;
  CheckDecodeRejected(HexBytes(
    '4D54686400000006000000010060' +
    '4D54726B00000005' +
    '8000FF2F00'),
    'the file reader rejects a nonminimal variable-length delta');
  LRaised := False;
  try
    DecodeWfcMidiVariableLength(HexBytes('8180808000'));
  except
    on E: EWfcMidiSmf do LRaised := True;
  end;
  Check(LRaised, 'the variable-length reader rejects five bytes');
  LRaised := False;
  try
    EncodeWfcMidiVariableLength($10000000);
  except
    on E: EWfcMidiSmf do LRaised := True;
  end;
  Check(LRaised, 'the variable-length writer rejects overflow');
end;

procedure TestRunningStatus;
var
  LDecoded: TWfcMidiFile;
begin
  LDecoded := DecodeWfcMidiFile(HexBytes(RUNNING_STATUS_INPUT));
  Check((Length(LDecoded.Tracks[0].Events) = 2) and
    (LDecoded.Tracks[0].Events[0].Status = $90) and
    (LDecoded.Tracks[0].Events[1].Status = $90),
    'running-status input expands to explicit in-memory statuses');
  Check(BytesMatch(EncodeWfcMidiFile(LDecoded),
    HexBytes(RUNNING_STATUS_CANONICAL)),
    'canonical output never emits running status');
  CheckDecodeRejected(HexBytes(
    '4D54686400000006000000010060' +
    '4D54726B00000007' +
    '003C4000FF2F00'),
    'data cannot establish running status');
  CheckDecodeRejected(HexBytes(
    '4D54686400000006000000010060' +
    '4D54726B0000000F' +
    '00903C40' +
    '00FF0100' +
    '003C00' +
    '00FF2F00'),
    'a meta event clears channel running status');
end;

procedure TestOpaqueEvents;
var
  LDecoded: TWfcMidiFile;
  LEncoded: TWfcMidiBytes;
  LFile: TWfcMidiFile;
begin
  LFile.Format := 0;
  LFile.TicksPerQuarter := 240;
  SetLength(LFile.Tracks, 1);
  SetLength(LFile.Tracks[0].Events, 3);
  LFile.Tracks[0].Events[0] :=
    MakeWfcMidiSystemExclusiveEvent(0, $F0, [$7D, 1, $F7]);
  LFile.Tracks[0].Events[1] :=
    MakeWfcMidiSystemExclusiveEvent(3, $F7, [$F1, $7F]);
  LFile.Tracks[0].Events[2] :=
    MakeWfcMidiMetaEvent(4, $7F, [0, $FF]);
  LFile.Tracks[0].EndDeltaTicks := 5;
  LEncoded := EncodeWfcMidiFile(LFile);
  LDecoded := DecodeWfcMidiFile(LEncoded);
  Check((Length(LDecoded.Tracks[0].Events) = 3) and
    (LDecoded.Tracks[0].Events[0].Status = $F0) and
    (LDecoded.Tracks[0].Events[1].Status = $F7) and
    (LDecoded.Tracks[0].Events[2].MetaType = $7F),
    'SysEx, escaped, and unknown meta events are preserved');
  Check((LDecoded.Tracks[0].EndDeltaTicks = 5) and
    BytesMatch(EncodeWfcMidiFile(LDecoded), LEncoded),
    'track-tail timing and opaque bytes round-trip exactly');
end;

procedure TestMalformedFilesAndLimits;
var
  LBytes: TWfcMidiBytes;
  LLimits: TWfcMidiReadLimits;
begin
  LBytes := HexBytes(GOLDEN_FORMAT_0);
  LBytes[9] := 2;
  CheckDecodeRejected(LBytes, 'format 2 is outside version-one scope');
  LBytes := HexBytes(GOLDEN_FORMAT_0);
  LBytes[11] := 2;
  CheckDecodeRejected(LBytes,
    'format 0 requires exactly one declared track');
  LBytes := HexBytes(GOLDEN_FORMAT_0);
  LBytes[12] := $E7;
  CheckDecodeRejected(LBytes,
    'SMPTE time division is outside version-one scope');
  LBytes := HexBytes(GOLDEN_FORMAT_0);
  LBytes[7] := 7;
  CheckDecodeRejected(LBytes,
    'the header chunk length is exactly six');
  CheckDecodeRejected(HexBytes(
    '4D54686400000006000000010060' +
    '4D54726B00000000'),
    'every track requires an end-of-track event');
  CheckDecodeRejected(HexBytes(
    '4D54686400000006000000010060' +
    '4D54726B00000005' +
    '00FF2F01AA'),
    'end-of-track has an exact zero data length');
  CheckDecodeRejected(HexBytes(
    '4D54686400000006000000010060' +
    '4D54726B00000005' +
    '00FF2F0000'),
    'bytes cannot follow end-of-track inside its chunk');
  CheckDecodeRejected(HexBytes(GOLDEN_FORMAT_0 + '00'),
    'bytes cannot follow the declared track chunks');
  CheckDecodeRejected(HexBytes(
    '4D54686400000006000000010060' +
    '4D54726BFFFFFFFF'),
    'a declared track length cannot exceed available bytes');
  CheckDecodeRejected(HexBytes(
    '4D54686400000006000000010060' +
    '4D54726B00000005' +
    '00F1000000'),
    'unsupported system statuses are rejected');
  CheckDecodeRejected(HexBytes(
    '4D54686400000006000000010060' +
    '4D54726B00000008' +
    '00903C8000FF2F00'),
    'channel data bytes cannot have their high bit set');

  LLimits := DefaultWfcMidiReadLimits;
  LLimits.MaxFileBytes := 14;
  CheckDecodeWithLimitsRejected(HexBytes(GOLDEN_FORMAT_0), LLimits,
    'the configured file-byte limit is enforced before parsing');
  LLimits := DefaultWfcMidiReadLimits;
  LLimits.MaxEvents := 1;
  CheckDecodeWithLimitsRejected(HexBytes(GOLDEN_FORMAT_0), LLimits,
    'the configured aggregate event limit includes end-of-track');
  LLimits := DefaultWfcMidiReadLimits;
  LLimits.MaxEventDataBytes := 2;
  CheckDecodeWithLimitsRejected(HexBytes(GOLDEN_FORMAT_0), LLimits,
    'declared event data is bounded before allocation');
end;

procedure TestConstructorGuards;
var
  LEvent: TWfcMidiEvent;
  LRaised: Boolean;
begin
  LEvent := MakeWfcMidiTempoEvent(0, 500000);
  Check((LEvent.Status = $FF) and (LEvent.MetaType = $51) and
    BytesMatch(LEvent.Data, HexBytes('07A120')),
    'the tempo helper uses the canonical three-byte payload');
  LEvent := MakeWfcMidiTimeSignatureEvent(0, 6, 3, 24, 8);
  Check(BytesMatch(LEvent.Data, HexBytes('06031808')),
    'the time-signature helper preserves all four fields');
  LRaised := False;
  try
    MakeWfcMidiTempoEvent(0, 0);
  except
    on E: EWfcMidiSmf do LRaised := True;
  end;
  Check(LRaised, 'zero microseconds per quarter is rejected');
  LRaised := False;
  try
    MakeWfcMidiChannelEvent(0, $C0, [0, 1]);
  except
    on E: EWfcMidiSmf do LRaised := True;
  end;
  Check(LRaised, 'channel constructors enforce status arity');
  LRaised := False;
  try
    MakeWfcMidiChannelEvent(0, $90, [$3C, $80]);
  except
    on E: EWfcMidiSmf do LRaised := True;
  end;
  Check(LRaised, 'channel constructors enforce seven-bit data');
  LRaised := False;
  try
    MakeWfcMidiMetaEvent(0, $2F, []);
  except
    on E: EWfcMidiSmf do LRaised := True;
  end;
  Check(LRaised, 'callers cannot inject structural end-of-track events');
  Check(WFC_MIDI_SMF_VERSION = 1,
    'the Standard MIDI File contract is explicitly versioned');
end;

begin
  WriteLn('WFC Standard MIDI File conformance suite');
  WriteLn('========================================');
  RunTest('format-0 golden bytes', @TestFormat0Golden);
  RunTest('format-1 golden bytes', @TestFormat1Golden);
  RunTest('variable-length quantities', @TestVariableLengths);
  RunTest('running status', @TestRunningStatus);
  RunTest('opaque SMF events', @TestOpaqueEvents);
  RunTest('malformed files and limits', @TestMalformedFilesAndLimits);
  RunTest('typed constructor guards', @TestConstructorGuards);
  WriteLn('========================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d MIDI checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
