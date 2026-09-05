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
program wfc_midi_stream_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc_midi_smf, wfc_midi_stream;

type TTest = procedure;
var Checks, Failures: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then begin Inc(Failures); WriteLn('[FAIL] ', AMessage); end;
end;

procedure Run(const AName: String; const ATest: TTest);
begin
  WriteLn('[TEST] ', AName);
  try ATest;
  except on E: Exception do
    begin Inc(Failures); WriteLn('[EXCEPTION] ', E.ClassName, ': ', E.Message); end;
  end;
end;

function Bytes(const AValues: array of Byte): TWfcMidiBytes;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

function SameBytes(const A, B: TWfcMidiBytes): Boolean;
var I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do if A[I] <> B[I] then Exit(False);
  Result := True;
end;

procedure Append(var ATarget: TWfcMidiBytes; const AValues: TWfcMidiBytes);
var I, LOffset: Integer;
begin
  LOffset := Length(ATarget); SetLength(ATarget, LOffset + Length(AValues));
  for I := 0 to High(AValues) do ATarget[LOffset + I] := AValues[I];
end;

procedure Drain(const AWriter: TWfcMidiFileStream; const ABlockSize: Integer;
  var AOutput: TWfcMidiBytes);
var LBytes: TWfcMidiBytes;
begin
  while AWriter.ReadBytes(ABlockSize, LBytes) do
  begin
    if (Length(LBytes) < 1) or (Length(LBytes) > ABlockSize) or
      (Length(LBytes) > WFC_MIDI_STREAM_BLOCK_BYTES) then
      raise Exception.Create('output block violates requested/internal bounds');
    Append(AOutput, LBytes);
  end;
  if Length(LBytes) <> 0 then raise Exception.Create('False must return nil bytes');
end;

function PlanFile(const AFile: TWfcMidiFile): TWfcMidiTrackPlan;
var LCounter: TWfcMidiTrackCounter; LTick: TWfcMidiStreamCount;
  LEvent: TWfcMidiEvent; I: Integer;
begin
  LCounter := TWfcMidiTrackCounter.Create;
  try
    LTick := 0;
    for I := 0 to High(AFile.Tracks[0].Events) do
    begin
      LEvent := AFile.Tracks[0].Events[I]; Inc(LTick, LEvent.DeltaTicks);
      LEvent.DeltaTicks := 0; LCounter.AppendEvent(LTick, LEvent);
    end;
    Inc(LTick, AFile.Tracks[0].EndDeltaTicks);
    Result := LCounter.Finish(LTick);
  finally LCounter.Free; end;
end;

function RenderFile(const AFile: TWfcMidiFile; const ABlockSize: Integer): TWfcMidiBytes;
var LPlan: TWfcMidiTrackPlan; LWriter: TWfcMidiFileStream;
  LEvent: TWfcMidiEvent; LTick: TWfcMidiStreamCount; I: Integer;
begin
  Result := nil; LPlan := PlanFile(AFile); LWriter := nil;
  try
    LWriter := TWfcMidiFileStream.Create(AFile.TicksPerQuarter, LPlan);
    Check(not LWriter.NeedsInput, 'header begins pending');
    Drain(LWriter, ABlockSize, Result);
    Check(LWriter.NeedsInput and (LWriter.EmittedBytes = 22), 'header drains independently of events');
    LTick := 0;
    for I := 0 to High(AFile.Tracks[0].Events) do
    begin
      LEvent := AFile.Tracks[0].Events[I]; Inc(LTick, LEvent.DeltaTicks);
      LEvent.DeltaTicks := 0; LWriter.AdmitEvent(LTick, LEvent);
      Drain(LWriter, ABlockSize, Result);
    end;
    Inc(LTick, AFile.Tracks[0].EndDeltaTicks);
    LWriter.Finish(LTick); LWriter.Finish(LTick);
    Check(LWriter.InputEnded and not LWriter.Finished, 'Finish queues final bytes without publishing completion early');
    Drain(LWriter, ABlockSize, Result);
    Check(LWriter.Finished and not LWriter.NeedsInput and
      (LWriter.EmittedBytes = LPlan.ByteCount + 22), 'complete output matches exact plan extent');
  finally LWriter.Free; LPlan.Free; end;
end;

function BasicFile: TWfcMidiFile;
begin
  Result := Default(TWfcMidiFile);
  Result.TicksPerQuarter := 480;
  SetLength(Result.Tracks, 1);
end;

procedure FinishAndFree(const ACounter: TWfcMidiTrackCounter;
  const AEndTick: TWfcMidiStreamCount);
var LPlan: TWfcMidiTrackPlan;
begin
  LPlan := ACounter.Finish(AEndTick);
  LPlan.Free;
end;

procedure TestCanonicalParity;
const SIZES: array[0..6] of Integer = (0, 1, 127, 128, 16383, 16384, 32769);
var LFile, LDecoded: TWfcMidiFile; LExpected, LActual: TWfcMidiBytes;
  I, J, K: Integer; LData: TWfcMidiBytes;
begin
  LFile := BasicFile;
  LExpected := EncodeWfcMidiFile(LFile);
  LActual := RenderFile(LFile, 1);
  Check(SameBytes(LActual, LExpected), 'empty format-0 file is canonical EOT-only SMF');
  SetLength(LFile.Tracks[0].Events, 12);
  LFile.Tracks[0].Events[0] := MakeWfcMidiTempoEvent(0, 500000);
  LFile.Tracks[0].Events[1] := MakeWfcMidiTimeSignatureEvent(0, 4, 2, 24, 8);
  LFile.Tracks[0].Events[2] := MakeWfcMidiChannelEvent(0, $90, [60, 100]);
  LFile.Tracks[0].Events[3] := MakeWfcMidiChannelEvent(127, $A9, [60, 50]);
  LFile.Tracks[0].Events[4] := MakeWfcMidiChannelEvent(128, $BF, [127, 127]);
  LFile.Tracks[0].Events[5] := MakeWfcMidiChannelEvent(0, $C0, [7]);
  LFile.Tracks[0].Events[6] := MakeWfcMidiChannelEvent(16383, $D1, [12]);
  LFile.Tracks[0].Events[7] := MakeWfcMidiChannelEvent(16384, $E9, [0, 64]);
  LFile.Tracks[0].Events[8] := MakeWfcMidiChannelEvent(0, $80, [60, 32]);
  LFile.Tracks[0].Events[9] := MakeWfcMidiSystemExclusiveEvent(0, $F0, [$7D, 1, 2, $F7]);
  LFile.Tracks[0].Events[10] := MakeWfcMidiSystemExclusiveEvent(0, $F7, [0, $80, $FF]);
  LFile.Tracks[0].EndDeltaTicks := 128;
  for I := 0 to High(SIZES) do
  begin
    SetLength(LData, SIZES[I]);
    for J := 0 to High(LData) do LData[J] := Byte(J mod 256);
    LFile.Tracks[0].Events[11] := MakeWfcMidiMetaEvent(0, $7F, LData);
    LExpected := EncodeWfcMidiFile(LFile);
    for K := 0 to 2 do
    begin
      case K of
        0: LActual := RenderFile(LFile, 1);
        1: LActual := RenderFile(LFile, 137);
        2: LActual := RenderFile(LFile, High(Integer));
      end;
      Check(SameBytes(LActual, LExpected), 'legacy SMF byte parity payload/block ' + IntToStr(I) + '/' + IntToStr(K));
    end;
    LDecoded := DecodeWfcMidiFile(LActual);
    Check((LDecoded.Format = 0) and (LDecoded.TicksPerQuarter = 480) and
      (Length(LDecoded.Tracks[0].Events) = 12), 'stream output decodes with existing SMF reader');
  end;
end;

procedure TestPlansAndSignatures;
var C: TWfcMidiTrackCounter; A, B: TWfcMidiTrackPlan; W: TWfcMidiFileStream;
  LBytes: TWfcMidiBytes; Rejected: Boolean;
begin
  C := TWfcMidiTrackCounter.Create; A := nil; B := nil;
  try
    A := C.Finish(0); B := C.Finish(0);
    Check((A <> B) and (A.ByteCount = 4) and (A.EventCount = 1) and
      (A.BridgeCount = 0) and (A.EndTick = 0), 'counter returns detached immutable EOT-inclusive plans');
    Check(A.Signature = Cardinal($AF8CB30F), 'independently calculated empty-plan FNV vector');
    Rejected := False;
    try C.AppendEvent(0, MakeWfcMidiChannelEvent(0, $90, [60, 100]));
    except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and C.Finished and (C.EventCount = 1), 'finished counter rejects new events');
    Rejected := False;
    try FinishAndFree(C, 1); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and (C.LastTick = 0), 'finished counter rejects changed end');
  finally B.Free; A.Free; C.Free; end;
  C := TWfcMidiTrackCounter.Create; A := nil;
  try
    C.AppendEvent(0, MakeWfcMidiChannelEvent(0, $90, [60, 100]));
    C.AppendEvent(480, MakeWfcMidiChannelEvent(0, $80, [60, 0]));
    A := C.Finish(960);
    Check((A.ByteCount = 14) and (A.EventCount = 3), 'canonical note/rest/EOT count');
    Check(A.Signature = Cardinal($7C723795), 'independently calculated logical note-plan FNV vector');
    W := TWfcMidiFileStream.Create(480, A);
  finally A.Free; C.Free; end;
  try
    LBytes := nil; Drain(W, 22, LBytes);
    W.AdmitEvent(0, MakeWfcMidiChannelEvent(0, $90, [60, 100])); Drain(W, 3, LBytes);
    W.AdmitEvent(480, MakeWfcMidiChannelEvent(0, $80, [60, 0])); Drain(W, 3, LBytes);
    W.Finish(960); Drain(W, 3, LBytes);
    Check(W.Finished and (Length(LBytes) = 36), 'writer retains no dependency on freed plan or counter');
  finally W.Free; end;
end;

procedure TestBridgeBoundaries;
var I, J, K: Integer; LGap, LBridgeCount, LTick: TWfcMidiStreamCount;
  C: TWfcMidiTrackCounter; P: TWfcMidiTrackPlan; W: TWfcMidiFileStream;
  LBytes: TWfcMidiBytes; F: TWfcMidiFile; E: TWfcMidiEvent;
begin
  E := MakeWfcMidiChannelEvent(0, $90, [60, 100]);
  for I := 0 to 5 do
  begin
    case I of
      0: LGap := 0;
      1: LGap := WFC_MIDI_MAX_VARIABLE_LENGTH;
      2: LGap := TWfcMidiStreamCount(WFC_MIDI_MAX_VARIABLE_LENGTH) + 1;
      3: LGap := TWfcMidiStreamCount(WFC_MIDI_MAX_VARIABLE_LENGTH) * 2;
      4: LGap := TWfcMidiStreamCount(WFC_MIDI_MAX_VARIABLE_LENGTH) * 2 + 1;
    else LGap := TWfcMidiStreamCount(WFC_MIDI_MAX_VARIABLE_LENGTH) - 1;
    end;
    LBridgeCount := 0;
    if LGap > 0 then LBridgeCount := (LGap - 1) div WFC_MIDI_MAX_VARIABLE_LENGTH;
    for J := 0 to 1 do
    begin
      C := TWfcMidiTrackCounter.Create; P := nil; W := nil;
      try
        if J = 0 then C.AppendEvent(LGap, E);
        P := C.Finish(LGap);
        Check(P.BridgeCount = LBridgeCount, 'bridge count boundary ' + IntToStr(I) + '/' + IntToStr(J));
        Check(P.EventCount = 2-J, 'synthetic text bridges are counted separately from logical events');
        W := TWfcMidiFileStream.Create(32767, P); LBytes := nil;
        Drain(W, 1, LBytes);
        if J = 0 then begin W.AdmitEvent(LGap, E); Drain(W, 2, LBytes); end;
        W.Finish(LGap); Drain(W, 3, LBytes);
        F := DecodeWfcMidiFile(LBytes);
        Check(Length(F.Tracks[0].Events) = LBridgeCount + 1-J, 'physical bridge events remain valid SMF');
        LTick := F.Tracks[0].EndDeltaTicks;
        for K := 0 to High(F.Tracks[0].Events) do Inc(LTick, F.Tracks[0].Events[K].DeltaTicks);
        Check(LTick = LGap, 'bridges preserve exact absolute event/end time');
        for K := 0 to Integer(LBridgeCount) - 1 do
          Check((F.Tracks[0].Events[K].DeltaTicks = WFC_MIDI_MAX_VARIABLE_LENGTH) and
            (F.Tracks[0].Events[K].Status = $FF) and (F.Tracks[0].Events[K].MetaType = $01) and
            (Length(F.Tracks[0].Events[K].Data) = 0), 'each bridge is canonical maximum-delta empty text');
        if J = 0 then
          Check((F.Tracks[0].Events[Integer(LBridgeCount)].Status = $90) and
            SameBytes(F.Tracks[0].Events[Integer(LBridgeCount)].Data, E.Data),
            'channel status and payload are explicit after bridge meta events');
        Check(Length(LBytes) = 22 + P.ByteCount, 'bridge-inclusive byte count exactly matches output');
      finally W.Free; P.Free; C.Free; end;
    end;
  end;
end;

procedure TestCounterValidation;
var C: TWfcMidiTrackCounter; P: TWfcMidiTrackPlan; E, Bad: TWfcMidiEvent;
  I: Integer; Rejected: Boolean; BeforeBytes, BeforeEvents: TWfcMidiStreamCount;
begin
  C := TWfcMidiTrackCounter.Create;
  E := MakeWfcMidiChannelEvent(0, $90, [60, 100]);
  try
    C.AppendEvent(10, E); BeforeBytes := C.ByteCount; BeforeEvents := C.EventCount;
    for I := 0 to 15 do
    begin
      Bad := E; Bad.Data := Bytes(E.Data);
      case I of
        0: Bad.DeltaTicks := 1;
        1: Bad.Status := $70;
        2: Bad.Status := $F1;
        3: Bad.MetaType := 1;
        4: Bad.Data := nil;
        5: Bad.Data[0] := 128;
        6: begin Bad.Status := $F0; Bad.MetaType := 1; end;
        7: begin Bad.Status := $F7; Bad.MetaType := 1; end;
        8: begin Bad.Status := $FF; Bad.MetaType := $80; end;
        9: begin Bad.Status := $FF; Bad.MetaType := $2F; Bad.Data := nil; end;
        10: begin Bad.Status := $FF; Bad.MetaType := $51; Bad.Data := Bytes([0, 0, 0]); end;
        11: begin Bad.Status := $FF; Bad.MetaType := $51; Bad.Data := Bytes([1, 2]); end;
        12: begin Bad.Status := $FF; Bad.MetaType := $58; Bad.Data := Bytes([4, 2, 24]); end;
        13: begin Bad.Status := $C0; Bad.Data := Bytes([0, 1]); end;
        14: begin Bad.Status := $D0; Bad.Data := nil; end;
        15: Bad.Status := 0;
      end;
      Rejected := False;
      try C.AppendEvent(11, Bad); except on EWfcMidiStream do Rejected := True; end;
      Check(Rejected and not C.Finished and (C.LastTick = 10) and
        (C.ByteCount = BeforeBytes) and (C.EventCount = BeforeEvents),
        'counter malformed typed event rejects atomically ' + IntToStr(I));
    end;
    Rejected := False;
    try C.AppendEvent(9, E); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and (C.LastTick = 10), 'counter rejects decreasing absolute tick');
    Rejected := False;
    try FinishAndFree(C, 9); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and not C.Finished, 'invalid earlier end leaves counter retryable');
    Rejected := False;
    try C.AppendEvent(-1, E); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected, 'negative absolute ticks reject');
    Rejected := False;
    try C.AppendEvent(WFC_MIDI_STREAM_MAX_SAFE_INTEGER + 1, E);
    except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and (C.LastTick = 10), 'unsafe absolute tick rejects before any state change');
    C.AppendEvent(10, E); P := C.Finish(10);
    try Check(P.EventCount = 3, 'same-tick logical event order and recovery remain available');
    finally P.Free; end;
  finally C.Free; end;
end;

procedure TestWriterOwnershipAndStates;
var C: TWfcMidiTrackCounter; P: TWfcMidiTrackPlan; W: TWfcMidiFileStream;
  E, Bad: TWfcMidiEvent; Original, Header, Saved, OutputBytes, Block: TWfcMidiBytes;
  I: Integer; Rejected: Boolean; F: TWfcMidiFile;
begin
  E := Default(TWfcMidiEvent); E.Status := $FF; E.MetaType := $01;
  SetLength(E.Data, 10001);
  for I := 0 to High(E.Data) do E.Data[I] := Byte(I mod 256);
  Original := Bytes(E.Data);
  C := TWfcMidiTrackCounter.Create; P := nil; W := nil;
  try
    C.AppendEvent(0, E); P := C.Finish(1);
    E.Data[0] := 99;
    W := TWfcMidiFileStream.Create(1, P);
    Rejected := False;
    try W.AdmitEvent(0, E); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and not W.Failed and not W.NeedsInput, 'header must drain before event admission');
    Rejected := False;
    try W.Finish(1); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and not W.Failed, 'Finish cannot bypass pending header');
    Rejected := False;
    try W.ReadBytes(0, Block); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and (Length(Block) = 0) and (W.EmittedBytes = 0), 'invalid pull maximum rejects with nil output');
    Check(W.ReadBytes(22, Header) and W.NeedsInput, 'exact header-sized pull immediately exposes NeedsInput');
    Saved := Bytes(Header); OutputBytes := Bytes(Header);
    Bad := E; Bad.DeltaTicks := 1;
    Rejected := False;
    try W.AdmitEvent(0, Bad); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and not W.Failed and W.NeedsInput, 'malformed writer event remains retryable');
    E.Data := Bytes(Original); W.AdmitEvent(0, E); E.Data[0] := 99;
    Rejected := False;
    try W.AdmitEvent(0, E); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and not W.Failed, 'undrained event cannot be overwritten by another admission');
    Rejected := False;
    try W.Finish(1); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and not W.InputEnded and not W.Failed, 'undrained event prevents premature Finish');
    Drain(W, 4096, OutputBytes);
    Check(SameBytes(Header, Saved), 'later reads never mutate a previously returned block');
    Header[0] := 0;
    W.Finish(1); Drain(W, 4096, OutputBytes);
    F := DecodeWfcMidiFile(OutputBytes);
    Check(SameBytes(F.Tracks[0].Events[0].Data, Original),
      'counter observes values immediately and writer clones one admitted payload');
    Check(W.Finished and not W.ReadBytes(1, Block) and (Length(Block) = 0),
      'finished reader returns False and nil');
    W.Cancel; Check(W.Finished and not W.Cancelled, 'cancel does not alter complete success');
    Rejected := False;
    try W.AdmitEvent(0, E); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected, 'finished writer rejects new logical events');
  finally W.Free; P.Free; C.Free; end;
end;

procedure TestReplayAndCancellation;
var C: TWfcMidiTrackCounter; P: TWfcMidiTrackPlan; W: TWfcMidiFileStream;
  E, Changed: TWfcMidiEvent; OutBytes, Block: TWfcMidiBytes;
  I: Integer; Rejected: Boolean; BeforeBytes: TWfcMidiStreamCount;
begin
  E := MakeWfcMidiChannelEvent(0, $90, [60, 100]);
  C := TWfcMidiTrackCounter.Create; P := nil;
  try
    C.AppendEvent(0, E); P := C.Finish(480);
    for I := 0 to 3 do
    begin
      W := TWfcMidiFileStream.Create(480, P);
      try
        OutBytes := nil; Drain(W, 4096, OutBytes);
        if I <> 1 then
        begin
          Changed := E; Changed.Data := Bytes(E.Data);
          if I = 0 then Changed.Data[1] := 99;
          W.AdmitEvent(0, Changed); Drain(W, 4096, OutBytes);
        end;
        BeforeBytes := W.EmittedBytes; Rejected := False;
        try
          case I of
            0,1: W.Finish(480);
            2: W.Finish(481);
            3: W.AdmitEvent(0, E);
          end;
        except on EWfcMidiStream do Rejected := True; end;
        Check(Rejected and W.Failed and not W.Finished and not W.NeedsInput,
          'valid-but-different replay is terminal ' + IntToStr(I));
        Check(W.EmittedBytes = BeforeBytes, 'replay mismatch queues no extra event or EOT bytes');
        Rejected := False;
        try W.ReadBytes(4096, Block); except on EWfcMidiStream do Rejected := True; end;
        Check(Rejected and (Length(Block) = 0), 'failed replay cannot emit additional bytes');
        W.Cancel; Check(W.Failed and not W.Finished, 'cancellation cannot erase replay failure');
      finally W.Free; end;
    end;
    W := TWfcMidiFileStream.Create(480, P);
    try
      W.Cancel; W.Cancel;
      Check(W.Cancelled and not W.NeedsInput and not W.Finished,
        'cancellation is terminal before any header output');
      Check(not W.ReadBytes(1, Block) and (Length(Block) = 0) and (W.EmittedBytes = 0),
        'cancelled writer discards pending header');
      Rejected := False;
      try W.Finish(480); except on EWfcMidiStream do Rejected := True; end;
      Check(Rejected, 'cancelled writer cannot queue EOT');
      Rejected := False;
      try W.AdmitEvent(0, E); except on EWfcMidiStream do Rejected := True; end;
      Check(Rejected, 'cancelled writer cannot accept events');
    finally W.Free; end;
  finally P.Free; C.Free; end;
end;

procedure TestHugeLazyGap;
var C: TWfcMidiTrackCounter; P: TWfcMidiTrackPlan; W: TWfcMidiFileStream;
  Block: TWfcMidiBytes; I: Integer;
begin
  C := TWfcMidiTrackCounter.Create; P := nil; W := nil;
  try
    P := C.Finish(WFC_MIDI_STREAM_MAX_SAFE_INTEGER);
    Check((P.EndTick = WFC_MIDI_STREAM_MAX_SAFE_INTEGER) and
      (P.BridgeCount = 33554432) and (P.ByteCount = 234881031) and
      (P.EventCount = 1), 'largest exact end counts tens of millions of bridges without expansion');
    W := TWfcMidiFileStream.Create(32767, P);
    Check(W.ReadBytes(22, Block) and W.NeedsInput, 'huge logical plan still exposes a fixed header');
    W.Finish(WFC_MIDI_STREAM_MAX_SAFE_INTEGER);
    Check(W.ReadBytes(High(Integer), Block) and (Length(Block) = 4096) and
      not W.Finished and not W.NeedsInput, 'huge final gap emits only the requested bounded block');
    for I := 0 to 583 do
      if not ((Block[I*7] = $FF) and (Block[I*7+1] = $FF) and
        (Block[I*7+2] = $FF) and (Block[I*7+3] = $7F) and
        (Block[I*7+4] = $FF) and (Block[I*7+5] = $01) and
        (Block[I*7+6] = 0)) then raise Exception.Create('huge bridge encoding differs');
    Check(True, 'each lazily generated bridge is maximum VLQ followed by empty text');
    W.Cancel;
    Check(not W.ReadBytes(4096, Block) and (Length(Block) = 0) and
      (W.EmittedBytes = 4118), 'huge queued gap cancels without further expansion');
  finally W.Free; P.Free; C.Free; end;
end;

procedure TestConstructorGuards;
var C: TWfcMidiTrackCounter; P: TWfcMidiTrackPlan; W: TWfcMidiFileStream;
  I: Integer; Rejected: Boolean;
begin
  C := TWfcMidiTrackCounter.Create; P := C.Finish(0); C.Free;
  try
    for I := 0 to 3 do
    begin
      W := nil; Rejected := False;
      try
        case I of
          0: W := TWfcMidiFileStream.Create(0, P);
          1: W := TWfcMidiFileStream.Create(32768, P);
          2: W := TWfcMidiFileStream.Create(-1, P);
          3: W := TWfcMidiFileStream.Create(480, nil);
        end;
      except on EWfcMidiStream do Rejected := True; end;
      W.Free;
      Check(Rejected, 'invalid file constructor argument ' + IntToStr(I));
    end;
  finally P.Free; end;
end;

{$IFDEF PAS2JS}
function BadNumber(const AIndex: Integer): NativeInt;
begin
  asm
    if (AIndex === 0) Result = 0.5;
    else if (AIndex === 1) Result = NaN;
    else if (AIndex === 2) Result = Infinity;
    else Result = 9007199254740992;
  end;
end;

procedure TestHostNumerics;
var C: TWfcMidiTrackCounter; P: TWfcMidiTrackPlan; W: TWfcMidiFileStream;
  E: TWfcMidiEvent; Block: TWfcMidiBytes; I,J: Integer; Rejected: Boolean;
  LBadLength: NativeInt;
begin
  C := TWfcMidiTrackCounter.Create;
  try
    for I := 0 to 3 do
      for J := 0 to 4 do
      begin
        E := MakeWfcMidiChannelEvent(0, $90, [60, 100]);
        Rejected := False;
        try
          case J of
            0: C.AppendEvent(BadNumber(I), E);
            1: begin E.DeltaTicks := BadNumber(I); C.AppendEvent(0, E); end;
            2: begin E.Status := BadNumber(I); C.AppendEvent(0, E); end;
            3: begin E.MetaType := BadNumber(I); C.AppendEvent(0, E); end;
            4: begin E.Data[0] := BadNumber(I); C.AppendEvent(0, E); end;
          end;
        except on EWfcMidiStream do Rejected := True; end;
        Check(Rejected and (C.EventCount = 0) and (C.ByteCount = 0),
          'malformed host logical event is rejected before counting ' + IntToStr(I) + '/' + IntToStr(J));
      end;
    E := Default(TWfcMidiEvent); E.Status := $FF; E.MetaType := 1;
    for I := 0 to 3 do
    begin
      LBadLength := BadNumber(I);
      asm E.Data = {length: LBadLength}; end;
      Rejected := False;
      try C.AppendEvent(0, E); except on EWfcMidiStream do Rejected := True; end;
      Check(Rejected and (C.ByteCount = 0), 'host payload length must be a bounded exact integer');
    end;
    asm E.Data = {length: 268435456}; end;
    Rejected := False;
    try C.AppendEvent(0, E); except on EWfcMidiStream do Rejected := True; end;
    Check(Rejected and (C.ByteCount = 0), 'oversized host payload length rejects before payload access/allocation');
    E.Data := nil;
    P := C.Finish(0);
  finally C.Free; end;
  try
    for I := 0 to 3 do
    begin
      W := nil; Rejected := False;
      try W := TWfcMidiFileStream.Create(480 + BadNumber(I), P);
      except on EWfcMidiStream do Rejected := True; end;
      W.Free;
      Check(Rejected, 'malformed host TPQ rejects ' + IntToStr(I));
    end;
    W := TWfcMidiFileStream.Create(480, P);
    try
      for I := 0 to 3 do
      begin
        Rejected := False;
        try W.ReadBytes(BadNumber(I), Block); except on EWfcMidiStream do Rejected := True; end;
        Check(Rejected and not W.Failed and (W.EmittedBytes = 0), 'malformed host pull size preserves pending header');
      end;
      W.ReadBytes(22, Block);
      for I := 0 to 3 do
      begin
        Rejected := False;
        try W.Finish(BadNumber(I)); except on EWfcMidiStream do Rejected := True; end;
        Check(Rejected and not W.Failed and W.NeedsInput, 'malformed host end tick is retryable');
      end;
      W.Finish(0); W.ReadBytes(4, Block);
      Check(W.Finished, 'valid finish remains possible after host numeric rejections');
    finally W.Free; end;
  finally P.Free; end;
end;
{$ENDIF}

begin
  Check(WFC_MIDI_STREAM_VERSION = 1, 'stream version');
  Run('canonical format-0 byte parity', @TestCanonicalParity);
  Run('immutable plans and logical fingerprint', @TestPlansAndSignatures);
  Run('long-gap bridge boundaries', @TestBridgeBoundaries);
  Run('counter validation and atomic recovery', @TestCounterValidation);
  Run('writer ownership and admission states', @TestWriterOwnershipAndStates);
  Run('replay mismatch and cancellation', @TestReplayAndCancellation);
  Run('huge gap counted and emitted lazily', @TestHugeLazyGap);
  Run('file constructor guards', @TestConstructorGuards);
  {$IFDEF PAS2JS}Run('malformed host numeric input', @TestHostNumerics);{$ENDIF}
  WriteLn('MIDI stream checks: ', Checks, ', failures: ', Failures);
  if Failures <> 0 then Halt(1);
end.
