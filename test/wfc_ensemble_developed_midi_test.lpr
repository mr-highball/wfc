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
program wfc_ensemble_developed_midi_test;
{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc_midi_smf, wfc_midi_stream, wfc_music, wfc_music_sequence, wfc_music_ensemble,
  wfc_music_arrangement, ensemble_studio_profiles, ensemble_studio_workbench,
  ensemble_studio_stream, ensemble_studio_midi_stream;

type
  TExpectedNote = record Tick: Cardinal; Status, Pitch, Velocity: Byte; end;
  TExpectedNotes = array of TExpectedNote;

var Checks: Integer;

procedure Check(const Condition: Boolean; const Text: String);
begin
  Inc(Checks);
  if not Condition then raise Exception.Create(Text);
end;

procedure HashByte(var Hash: Cardinal; const Value: Byte);
{$PUSH}{$Q-}
var H: Cardinal;
begin
  H := Hash xor Cardinal(Value);
  Hash := (H + (H shl 1) + (H shl 4) + (H shl 7) +
    (H shl 8) + (H shl 24)) and Cardinal($FFFFFFFF);
end;
{$POP}

procedure HashInteger(var Hash: Cardinal; Value: Integer);
var I: Integer;
begin
  for I := 0 to 7 do begin HashByte(Hash, Byte(Value mod 256)); Value := Value div 256; end;
end;

function FrameHash(const Frames: TWfcMusicEnsembleFrames): Cardinal;
var I, J: Integer; Token: String;
begin
  Result := Cardinal($811C9DC5);
  for I := 0 to High(Frames) do
  begin
    Token := EncodeWfcMusicEnsembleFrame(Frames[I]);
    HashInteger(Result, I); HashInteger(Result, Length(Token));
    for J := 1 to Length(Token) do HashByte(Result, Byte(Ord(Token[J])));
  end;
end;

procedure AddNote(var Notes: TExpectedNotes; const Tick: Cardinal;
  const Status, Pitch, Velocity: Byte);
var I: Integer;
begin
  I := Length(Notes); SetLength(Notes, I + 1);
  Notes[I].Tick := Tick; Notes[I].Status := Status;
  Notes[I].Pitch := Pitch; Notes[I].Velocity := Velocity;
end;

//Reconstruct expected performance directly from finite frames, not from the
//MIDI counting/replay implementation or its hashes. Holds have no seam events;
//every new attack closes old tones before any new voice attack at that tick.
function ExpectedNotes(const Frames: TWfcMusicEnsembleFrames): TExpectedNotes;
var Active: array of TWfcMusicTones; I, V, T: Integer;
begin
  Result := nil; SetLength(Active, 3);
  for I := 0 to High(Frames) do
  begin
    Check(Length(Frames[I].Voices) = 3, 'finite reference keeps three ordered voices');
    for V := 0 to 2 do
      if Frames[I].Voices[V].Action <> wmcaHold then
      begin
        for T := 0 to High(Active[V]) do
          AddNote(Result, Cardinal(I * 240), $80 + V, Active[V][T].Pitch, 0);
        Active[V] := nil;
      end;
    for V := 0 to 2 do
      if Frames[I].Voices[V].Action = wmcaAttack then
      begin
        Active[V] := Frames[I].Voices[V].Tones;
        for T := 0 to High(Active[V]) do
          AddNote(Result, Cardinal(I * 240), $90 + V, Active[V][T].Pitch, Active[V][T].Velocity);
      end;
  end;
  for V := 0 to 2 do for T := 0 to High(Active[V]) do
    AddNote(Result, Cardinal(Length(Frames) * 240), $80 + V, Active[V][T].Pitch, 0);
end;

procedure CheckPerformance(const Bytes: TWfcMidiBytes; const Notes: TExpectedNotes);
var FileData: TWfcMidiFile; I, N: Integer; Tick: Cardinal; E: TWfcMidiEvent;
begin
  FileData := DecodeWfcMidiFile(Bytes);
  Check((FileData.Format = 0) and (FileData.TicksPerQuarter = 480) and
    (Length(FileData.Tracks) = 1), 'developed replay retains format zero and exact timing policy');
  Tick := 0; N := 0;
  for I := 0 to High(FileData.Tracks[0].Events) do
  begin
    E := FileData.Tracks[0].Events[I]; Inc(Tick, E.DeltaTicks);
    if E.Status >= $F0 then Continue;
    Check((N < Length(Notes)) and (Length(E.Data) = 2), 'decoded performance has exactly shaped expected note events');
    Check((Tick = Notes[N].Tick) and (E.Status = Notes[N].Status) and
      (E.Data[0] = Notes[N].Pitch) and (E.Data[1] = Notes[N].Velocity),
      'decoded tick, ordered voice, attack/off, pitch and velocity match finite frames');
    Inc(N);
  end;
  Inc(Tick, FileData.Tracks[0].EndDeltaTicks);
  Check((N = Length(Notes)) and (Tick = 30720), 'all finite performance events and final closing tick are represented');
end;

function MakePlan(const Horizon: Integer): TEnsembleStudioMidiPlan;
var Options: TEnsembleStudioStreamOptions; Planner: TEnsembleStudioMidiPlanner;
  Step: TWfcMusicArrangementStep; Iterations: Integer;
begin
  Result := nil; Options := DefaultEnsembleStudioStreamOptions;
  Check(Options.Profile = espStructuralV1, 'structural remains the default stream profile');
  Options.Profile := espDevelopedPeriodV1; Options.Seed := 4;
  Options.SegmentCellCount := Horizon;
  Planner := TEnsembleStudioMidiPlanner.Create(PlanEnsembleStudioFrames('32'), Options);
  try
    //The planner owns a scalar snapshot, not the caller's later profile choice.
    Options.Profile := espStructuralV1; Iterations := 0;
    repeat
      Inc(Iterations); Check(Iterations <= 130, 'counting pass has a bounded finite frame loop');
      Step := Planner.Next;
      Check(Step in [wmaspProduced, wmaspCompleted], 'developed counting pass: ' + Planner.Failure);
    until Step = wmaspCompleted;
    Result := Planner.DetachPlan;
  finally Planner.Free; end;
end;

function ReplayAndRelease(var Plan: TEnsembleStudioMidiPlan): TWfcMidiBytes;
var Stream: TEnsembleStudioMidiStream; Block: TWfcMidiBytes;
  Step: TWfcMusicArrangementStep; ExpectedBytes, I, Old, Iterations: Integer;
begin
  Result := nil; ExpectedBytes := Integer(Plan.FileByteCount);
  Stream := TEnsembleStudioMidiStream.Create(Plan); FreeAndNil(Plan);
  try
    Iterations := 0;
    repeat
      Inc(Iterations); Check(Iterations < 10000, 'replay cannot spin beyond this finite fixture');
      Step := Stream.NextBytes(Block);
      Check(Step in [wmaspProduced, wmaspCompleted], 'developed replay: ' + Stream.Failure);
      if Step = wmaspProduced then
      begin
        Check((Length(Block) > 0) and (Length(Block) <= WFC_MIDI_STREAM_BLOCK_BYTES), 'replay exposes detached bounded blocks');
        Old := Length(Result); SetLength(Result, Old + Length(Block));
        for I := 0 to High(Block) do Result[Old + I] := Block[I];
      end;
    until Step = wmaspCompleted;
    Check((Stream.FramesProcessed = 128) and (Stream.TickCount = 30720) and
      (Stream.EmittedBytes = ExpectedBytes) and (Length(Result) = ExpectedBytes),
      'replay retains developed configuration after caller frees its plan');
    Check((Stream.NextBytes(Block) = wmaspCompleted) and (Block = nil), 'successful replay remains terminal');
  finally Stream.Free; end;
end;

procedure TestDevelopedMidi;
var Studio: TEnsembleStudio; Frames: TWfcMusicEnsembleFrames;
  Notes: TExpectedNotes; P: TEnsembleStudioMidiPlan; Options: TEnsembleStudioStreamOptions;
  Bytes, FirstBytes: TWfcMidiBytes; H, I: Integer; ExpectedHash, FirstMidiHash: Cardinal;
begin
  Studio := TEnsembleStudio.Create(4, 16, espDevelopedPeriodV1);
  try
    Check(Studio.Run(esaGenerate, DefaultEnsembleStudioOptions), 'finite developed seed4 sixteen-bar fixture solves');
    Check(Studio.CurrentIsValid, 'finite expected composition is independently valid');
    Frames := Studio.EnsembleFrames;
  finally Studio.Free; end;
  Check(Length(Frames) = 128, 'finite expected duration is exactly32seconds');
  Notes := ExpectedNotes(Frames); ExpectedHash := FrameHash(Frames); FirstBytes := nil; FirstMidiHash := 0;
  for H := 0 to 1 do
  begin
    if H = 0 then P := MakePlan(5) else P := MakePlan(32);
    try
      Check((P.Options.Profile = espDevelopedPeriodV1) and (P.Options.Seed = 4), 'immutable counting plan retains exact developed profile and seed');
      Options := P.Options; Options.Profile := espStructuralV1;
      Check((Options.Profile = espStructuralV1) and (P.Options.Profile = espDevelopedPeriodV1),
        'copied Options cannot reset the stored profile');
      Check((P.FrameCount = 128) and (P.EndTick = 30720) and (P.FrameSignature = ExpectedHash),
        'planned frame fingerprint is independently recounted from finite developed frames');
      if H = 0 then FirstMidiHash := P.MidiSignature
      else Check(P.MidiSignature = FirstMidiHash, 'segment horizon cannot change developed MIDI events');
      Bytes := ReplayAndRelease(P); CheckPerformance(Bytes, Notes);
      if H = 0 then FirstBytes := Bytes
      else
      begin
        Check(Length(Bytes) = Length(FirstBytes), 'five-cell and bar-aligned replay sizes match');
        for I := 0 to High(Bytes) do Check(Bytes[I] = FirstBytes[I], 'profile replay bytes are independent of segment horizon');
      end;
    finally P.Free; end;
  end;
  WriteLn('Developed MIDI values: seed=4 seconds=32 frames=128 frame-hash=',
    IntToHex(ExpectedHash, 8), ' midi=', IntToHex(FirstMidiHash, 8), ' bytes=', Length(FirstBytes));
end;

begin
  try
    TestDevelopedMidi;
    WriteLn('Developed Ensemble MIDI checks: ', Checks);
  except on E: Exception do begin WriteLn('FAIL: ', E.Message); Halt(1); end; end;
end.
