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
program wfc_music_studies_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_music,
  wfc_music_audio,
  wfc_music_audio_stream,
  wfc_midi_smf,
  wfc_music_midi,
  main,
  simpleriff;

type
  TTestProcedure = procedure;

  TSimpleMusicProbe = class(TSimpleMusic)
  public
    function PitchOf(const ANote: String): Integer;
  end;

  TCountingPrefixSink = class(TWfcMusicAudioByteSink)
  public
    TotalBytes: TWfcMusicAudioStreamCount;
    Calls: Integer;
    MaxBlock: Integer;
    Prefix: TWfcMusicAudioBytes;
    procedure WriteBytes(const ABytes: array of Byte); override;
  end;

const
  PREFIX_LIMIT = 80;

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

function TSimpleMusicProbe.PitchOf(const ANote: String): Integer;
begin
  Result := NotePitch(ANote);
end;

procedure TCountingPrefixSink.WriteBytes(const ABytes: array of Byte);
var
  I: Integer;
  LCopyCount: Integer;
  LOffset: Integer;
begin
  Inc(Calls);
  if Length(ABytes) > MaxBlock then MaxBlock := Length(ABytes);
  TotalBytes := TotalBytes + Length(ABytes);
  LOffset := Length(Prefix);
  LCopyCount := Length(ABytes);
  if LCopyCount > PREFIX_LIMIT - LOffset then
    LCopyCount := PREFIX_LIMIT - LOffset;
  if LCopyCount <= 0 then Exit;
  SetLength(Prefix, LOffset + LCopyCount);
  for I := 0 to LCopyCount - 1 do
    Prefix[LOffset + I] := ABytes[I];
end;

function SameMidiBytes(const A, B: TWfcMidiBytes): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to Length(A) - 1 do
    if A[I] <> B[I] then Exit(False);
  Result := True;
end;

function SameScores(const A, B: TWfcMusicScore): Boolean;
var
  I: Integer;
  J: Integer;
  LMeterA: TWfcMusicMeterChange;
  LMeterB: TWfcMusicMeterChange;
  LSpanA: TWfcMusicSpanEvent;
  LSpanB: TWfcMusicSpanEvent;
  LTempoA: TWfcMusicTempoChange;
  LTempoB: TWfcMusicTempoChange;
  LTrackA: TWfcMusicTrack;
  LTrackB: TWfcMusicTrack;
  LVoiceA: TWfcMusicVoice;
  LVoiceB: TWfcMusicVoice;
begin
  if (A = nil) or (B = nil) then Exit(A = B);
  if (A.TicksPerQuarter <> B.TicksPerQuarter) or
      (A.StepsPerOctave <> B.StepsPerOctave) or
      (A.LengthTicks <> B.LengthTicks) or
      (A.TrackCount <> B.TrackCount) or
      (A.VoiceCount <> B.VoiceCount) or
      (A.MeterCount <> B.MeterCount) or
      (A.TempoCount <> B.TempoCount) or
      (A.SpanCount <> B.SpanCount) then Exit(False);
  for I := 0 to A.TrackCount - 1 do
  begin
    LTrackA := A.TrackAt(I); LTrackB := B.TrackAt(I);
    if (LTrackA.Id <> LTrackB.Id) or (LTrackA.Name <> LTrackB.Name) then
      Exit(False);
  end;
  for I := 0 to A.VoiceCount - 1 do
  begin
    LVoiceA := A.VoiceAt(I); LVoiceB := B.VoiceAt(I);
    if (LVoiceA.TrackIndex <> LVoiceB.TrackIndex) or
        (LVoiceA.Id <> LVoiceB.Id) then Exit(False);
  end;
  for I := 0 to A.MeterCount - 1 do
  begin
    LMeterA := A.MeterAt(I); LMeterB := B.MeterAt(I);
    if (LMeterA.Tick <> LMeterB.Tick) or
        (LMeterA.Numerator <> LMeterB.Numerator) or
        (LMeterA.Denominator <> LMeterB.Denominator) then Exit(False);
  end;
  for I := 0 to A.TempoCount - 1 do
  begin
    LTempoA := A.TempoAt(I); LTempoB := B.TempoAt(I);
    if (LTempoA.Tick <> LTempoB.Tick) or
        (LTempoA.MicrosecondsPerQuarter <>
          LTempoB.MicrosecondsPerQuarter) then Exit(False);
  end;
  for I := 0 to A.SpanCount - 1 do
  begin
    LSpanA := A.SpanAt(I); LSpanB := B.SpanAt(I);
    if (LSpanA.VoiceIndex <> LSpanB.VoiceIndex) or
        (LSpanA.StartTick <> LSpanB.StartTick) or
        (LSpanA.DurationTicks <> LSpanB.DurationTicks) or
        (LSpanA.Kind <> LSpanB.Kind) or
        (Length(LSpanA.Tones) <> Length(LSpanB.Tones)) then Exit(False);
    for J := 0 to Length(LSpanA.Tones) - 1 do
      if (LSpanA.Tones[J].Pitch <> LSpanB.Tones[J].Pitch) or
          (LSpanA.Tones[J].Velocity <> LSpanB.Tones[J].Velocity) then
        Exit(False);
  end;
  Result := True;
end;

function PitchAt(const AScore: TWfcMusicScore;
  const AIndex: Integer): Integer;
var
  LSpan: TWfcMusicSpanEvent;
begin
  LSpan := AScore.SpanAt(AIndex);
  if (LSpan.Kind <> wmskNote) or (Length(LSpan.Tones) <> 1) then
    raise Exception.Create('study score does not contain one-note spans');
  Result := LSpan.Tones[0].Pitch;
end;

procedure CheckStudyScore(const AScore: TWfcMusicScore;
  const ACount, ATempo: Integer; const ALabel: String);
var
  I: Integer;
  LMeter: TWfcMusicMeterChange;
  LOk: Boolean;
  LSpan: TWfcMusicSpanEvent;
  LTempo: TWfcMusicTempoChange;
begin
  LOk := AScore <> nil;
  if LOk then
    LOk := (AScore.TicksPerQuarter = 1) and
      (AScore.StepsPerOctave = 12) and
      (AScore.LengthTicks = ACount) and
      (AScore.TrackCount = 1) and
      (AScore.VoiceCount = 1) and
      (AScore.MeterCount = 1) and
      (AScore.TempoCount = 1) and
      (AScore.SpanCount = ACount);
  if LOk then
  begin
    LMeter := AScore.MeterAt(0);
    LTempo := AScore.TempoAt(0);
    LOk := (LMeter.Tick = 0) and (LMeter.Numerator = 1) and
      (LMeter.Denominator = 4) and (LTempo.Tick = 0) and
      (LTempo.MicrosecondsPerQuarter = ATempo);
  end;
  I := 0;
  while LOk and (I < ACount) do
  begin
    LSpan := AScore.SpanAt(I);
    LOk := (LSpan.VoiceIndex = 0) and (LSpan.StartTick = I) and
      (LSpan.DurationTicks = 1) and (LSpan.Kind = wmskNote) and
      (Length(LSpan.Tones) = 1);
    if LOk then
      LOk := (LSpan.Tones[0].Pitch >= 0) and
        (LSpan.Tones[0].Pitch <= 127) and
        (LSpan.Tones[0].Velocity = 96);
    Inc(I);
  end;
  Check(LOk, ALabel + ' has the canonical one-quarter note timeline');
end;

function PreviousScalePitch(const APitch: Integer): Integer;
begin
  case APitch of
    57: Result := 69;
    59: Result := 57;
    61: Result := 59;
    62: Result := 61;
    64: Result := 62;
    66: Result := 64;
    68: Result := 66;
    69: Result := 68;
  else Result := -1;
  end;
end;

function SongsForMask(const AMask: Integer): TSimpleRiffSongs;
begin
  Result := [];
  if (AMask and 1) <> 0 then Include(Result, srsMary);
  if (AMask and 2) <> 0 then Include(Result, srsBridge);
  if (AMask and 4) <> 0 then Include(Result, srsHotCross);
end;

function MaryPitch(const APitch: Integer): Boolean;
begin
  Result := (APitch = 60) or (APitch = 62) or
    (APitch = 64) or (APitch = 67);
end;

function BridgePitch(const APitch: Integer): Boolean;
begin
  Result := (APitch = 57) or (APitch = 59) or
    (APitch = 60) or (APitch = 62) or (APitch = 64);
end;

function HotCrossPitch(const APitch: Integer): Boolean;
begin
  Result := (APitch = 60) or (APitch = 62) or (APitch = 64);
end;

function RiffPitchAllowed(const AMask, APitch: Integer): Boolean;
begin
  Result := (((AMask and 1) <> 0) and MaryPitch(APitch)) or
    (((AMask and 2) <> 0) and BridgePitch(APitch)) or
    (((AMask and 4) <> 0) and HotCrossPitch(APitch));
end;

function MaryPair(const ALeft, ARight: Integer): Boolean;
begin
  Result :=
    ((ALeft = 64) and ((ARight = 62) or (ARight = 64) or
      (ARight = 67))) or
    ((ALeft = 62) and ((ARight = 64) or (ARight = 60))) or
    ((ALeft = 60) and (ARight = 62)) or
    ((ALeft = 67) and ((ARight = 64) or (ARight = 67)));
end;

function BridgePair(const ALeft, ARight: Integer): Boolean;
begin
  { Index-order playback traverses the stored east relation from target back
    to source. Symmetric declarations are unchanged; the two deliberately
    asymmetric Bridge edges therefore appear as B-to-D and B-to-A. }
  Result :=
    ((ALeft = 62) and ((ARight = 64) or (ARight = 60) or
      (ARight = 57))) or
    ((ALeft = 64) and (ARight = 62)) or
    ((ALeft = 60) and ((ARight = 62) or (ARight = 59))) or
    ((ALeft = 57) and (ARight = 62)) or
    ((ALeft = 59) and ((ARight = 60) or (ARight = 62) or
      (ARight = 57)));
end;

function HotCrossPair(const ALeft, ARight: Integer): Boolean;
begin
  Result :=
    ((ALeft = 64) and ((ARight = 62) or (ARight = 60))) or
    ((ALeft = 62) and ((ARight = 64) or (ARight = 60))) or
    ((ALeft = 60) and ((ARight = 64) or (ARight = 62)));
end;

function RiffPairAllowed(const AMask, ALeft, ARight: Integer): Boolean;
begin
  Result := (((AMask and 1) <> 0) and MaryPair(ALeft, ARight)) or
    (((AMask and 2) <> 0) and BridgePair(ALeft, ARight)) or
    (((AMask and 4) <> 0) and HotCrossPair(ALeft, ARight));
end;

procedure TestScaleGrammarAndReplay;
const
  NAMES: array[0..7] of String =
    ('A', 'B', 'C#', 'D', 'E', 'F#', 'G#', 'A+');
  PITCHES: array[0..7] of Integer = (57, 59, 61, 62, 64, 66, 68, 69);
  SEED_GOLDEN: array[0..15] of Integer =
    (59, 57, 69, 68, 66, 64, 62, 61,
     59, 57, 69, 68, 66, 64, 62, 61);
var
  I: Integer;
  LAllSeen: Boolean;
  LFirst: TWfcMusicScore;
  LFresh: TWfcMusicScore;
  LFreshOwner: TSimpleMusic;
  LOwner: TSimpleMusic;
  LProbe: TSimpleMusicProbe;
  LReplay: TWfcMusicScore;
  LSeedGoldenOk: Boolean;
  LScaleOk: Boolean;
  LSeen: array[0..127] of Boolean;
  LThrowaway: TWfcMusicScore;
begin
  LProbe := TSimpleMusicProbe.Create;
  try
    for I := 0 to High(NAMES) do
      Check(LProbe.PitchOf(NAMES[I]) = PITCHES[I],
        NAMES[I] + ' preserves its original MIDI pitch');
  finally
    LProbe.Free;
  end;

  LOwner := TSimpleMusic.Create;
  LFreshOwner := TSimpleMusic.Create;
  LFirst := nil; LThrowaway := nil; LReplay := nil; LFresh := nil;
  try
    LFirst := LOwner.GenerateMusic(16, 500000, TGraphSeed($DEADBEEF));
    CheckStudyScore(LFirst, 16, 500000, 'A-major generation');
    for I := Low(LSeen) to High(LSeen) do LSeen[I] := False;
    LScaleOk := True;
    LSeedGoldenOk := True;
    for I := 0 to LFirst.SpanCount - 1 do
    begin
      if (PitchAt(LFirst, I) < Low(LSeen)) or
          (PitchAt(LFirst, I) > High(LSeen)) then
        LScaleOk := False
      else
        LSeen[PitchAt(LFirst, I)] := True;
      if (I > 0) and
          (PitchAt(LFirst, I) <> PreviousScalePitch(PitchAt(LFirst, I - 1))) then
      begin
        WriteLn('  scale edge ', PitchAt(LFirst, I - 1), ' -> ',
          PitchAt(LFirst, I));
        LScaleOk := False;
      end;
      if PitchAt(LFirst, I) <> SEED_GOLDEN[I] then
        LSeedGoldenOk := False;
    end;
    Check(LScaleOk,
      'A-major playback preserves the authored directed adjacency cycle');
    Check(LSeedGoldenOk,
      'seed DEADBEEF preserves the shared sixteen-pitch golden sequence');
    LAllSeen := True;
    for I := 0 to High(PITCHES) do
      if not LSeen[PITCHES[I]] then LAllSeen := False;
    Check(LAllSeen, 'a sixteen-note A-major run visits all eight pitches');

    LThrowaway := LOwner.GenerateMusic(3, 1, 19);
    CheckStudyScore(LThrowaway, 3, 1,
      'an intervening arbitrary positive request');
    LReplay := LOwner.GenerateMusic(16, 500000, TGraphSeed($DEADBEEF));
    LFresh := LFreshOwner.GenerateMusic(16, 500000,
      TGraphSeed($DEADBEEF));
    Check(SameScores(LFirst, LReplay) and
      SameMidiBytes(EncodeWfcMusicMidi(LFirst),
        EncodeWfcMusicMidi(LReplay)),
      'resetting and replaying a seed is byte-exact on one owner');
    Check(SameScores(LFirst, LFresh) and
      SameMidiBytes(EncodeWfcMusicMidi(LFirst),
        EncodeWfcMusicMidi(LFresh)),
      'the same seed is byte-exact on a fresh owner');
  finally
    LFresh.Free;
    LReplay.Free;
    LThrowaway.Free;
    LFirst.Free;
    LFreshOwner.Free;
    LOwner.Free;
  end;
end;

procedure TestEveryRiffSelection;
var
  I: Integer;
  LAdjacencyOk: Boolean;
  LBytes: TWfcMidiBytes;
  LDomainOk: Boolean;
  LMask: Integer;
  LOwner: TSimpleRiff;
  LReplay: TWfcMusicScore;
  LScore: TWfcMusicScore;
begin
  LOwner := TSimpleRiff.Create;
  try
    for LMask := 1 to 7 do
    begin
      LOwner.Songs := SongsForMask(LMask);
      LScore := LOwner.GenerateMusic(24, 400000,
        TGraphSeed(1000 + LMask));
      LReplay := nil;
      try
        CheckStudyScore(LScore, 24, 400000,
          Format('riff selection %d', [LMask]));
        LDomainOk := True;
        LAdjacencyOk := True;
        for I := 0 to LScore.SpanCount - 1 do
        begin
          if not RiffPitchAllowed(LMask, PitchAt(LScore, I)) then
            LDomainOk := False;
          if (I > 0) and not RiffPairAllowed(LMask,
              PitchAt(LScore, I - 1), PitchAt(LScore, I)) then
          begin
            WriteLn('  riff edge ', LMask, ': ', PitchAt(LScore, I - 1),
              ' -> ', PitchAt(LScore, I));
            LAdjacencyOk := False;
          end;
        end;
        Check(LDomainOk, Format(
          'riff selection %d stays within its selected pitch union',
          [LMask]));
        Check(LAdjacencyOk, Format(
          'riff selection %d follows only its selected adjacency union',
          [LMask]));
        LBytes := EncodeWfcMusicMidi(LScore);
        LReplay := LOwner.GenerateMusic(24, 400000,
          TGraphSeed(1000 + LMask));
        Check(SameScores(LScore, LReplay) and
          SameMidiBytes(LBytes, EncodeWfcMusicMidi(LReplay)),
          Format('riff selection %d replays exactly after reset', [LMask]));
      finally
        LReplay.Free;
        LScore.Free;
      end;
    end;
  finally
    LOwner.Free;
  end;
end;

procedure ExpectRangeRejected(const AOwner: TSimpleMusic;
  const ACount, ATempo: Integer; const ALabel: String);
var
  LRaised: Boolean;
  LScore: TWfcMusicScore;
begin
  LRaised := False;
  LScore := nil;
  try
    try
      LScore := AOwner.GenerateMusic(ACount, ATempo, 7);
    except
      on E: ERangeError do LRaised := True;
    end;
  finally
    LScore.Free;
  end;
  Check(LRaised, ALabel);
end;

procedure TestInvalidRequestsAndReuse;
var
  LOwner: TSimpleMusic;
  LRaised: Boolean;
  LRiff: TSimpleRiff;
  LScore: TWfcMusicScore;
begin
  LOwner := TSimpleMusic.Create;
  try
    ExpectRangeRejected(LOwner, 0, 500000,
      'zero notes are rejected');
    ExpectRangeRejected(LOwner, -1, 500000,
      'negative notes are rejected');
    ExpectRangeRejected(LOwner, 1, 0,
      'zero tempo is rejected');
    ExpectRangeRejected(LOwner, 1, -1,
      'negative tempo is rejected');
    LScore := LOwner.GenerateMusic(1, 1, High(TGraphSeed));
    try
      CheckStudyScore(LScore, 1, 1,
        'minimum positive request after rejected calls');
    finally
      LScore.Free;
    end;
  finally
    LOwner.Free;
  end;

  LRiff := TSimpleRiff.Create;
  try
    LRiff.Songs := [];
    LRaised := False;
    LScore := nil;
    try
      try
        LScore := LRiff.GenerateMusic(4, 500000, 8);
      except
        on E: EArgumentException do LRaised := True;
      end;
    finally
      LScore.Free;
    end;
    Check(LRaised, 'an empty riff selection is rejected');
    LRiff.Songs := [srsHotCross];
    LScore := LRiff.GenerateMusic(3, 333333, 8);
    try
      CheckStudyScore(LScore, 3, 333333,
        'riff owner reuse after an empty-selection failure');
    finally
      LScore.Free;
    end;
  finally
    LRiff.Free;
  end;
end;

procedure TestMidiRoundTrip;
var
  I: Integer;
  LBytes: TWfcMidiBytes;
  LDecoded: TWfcMidiFile;
  LEventOk: Boolean;
  LHeaderOk: Boolean;
  LNoteOffs: Integer;
  LNoteOns: Integer;
  LOwner: TSimpleMusic;
  LScore: TWfcMusicScore;
  LTick: Cardinal;
begin
  LOwner := TSimpleMusic.Create;
  LScore := nil;
  try
    LScore := LOwner.GenerateMusic(13, 500000, 55);
    LBytes := EncodeWfcMusicMidi(LScore);
    LHeaderOk := Length(LBytes) >= 14;
    if LHeaderOk then
      LHeaderOk := (LBytes[0] = Ord('M')) and
        (LBytes[1] = Ord('T')) and (LBytes[2] = Ord('h')) and
        (LBytes[3] = Ord('d'));
    Check(LHeaderOk, 'the owned MIDI encoder writes an SMF header');
    LDecoded := DecodeWfcMidiFile(LBytes);
    Check((LDecoded.Format = 0) and (LDecoded.TicksPerQuarter = 1) and
      (Length(LDecoded.Tracks) = 1),
      'the owned MIDI reader recovers format 0 and the study time base');
    Check(Length(LDecoded.Tracks[0].Events) = 2 * LScore.SpanCount + 2,
      'decoded MIDI contains tempo, meter, and paired note events');
    LTick := 0;
    LNoteOns := 0;
    LNoteOffs := 0;
    LEventOk := True;
    for I := 0 to High(LDecoded.Tracks[0].Events) do
    begin
      Inc(LTick, LDecoded.Tracks[0].Events[I].DeltaTicks);
      case LDecoded.Tracks[0].Events[I].Status and $F0 of
        $80:
          begin
            Inc(LNoteOffs);
            if Length(LDecoded.Tracks[0].Events[I].Data) <> 2 then
              LEventOk := False
            else if LDecoded.Tracks[0].Events[I].Data[1] <> 0 then
              LEventOk := False;
          end;
        $90:
          begin
            if (Length(LDecoded.Tracks[0].Events[I].Data) <> 2) then
              LEventOk := False
            else
            begin
              if (LNoteOns >= LScore.SpanCount) or
                  (LDecoded.Tracks[0].Events[I].Data[0] <>
                    PitchAt(LScore, LNoteOns)) or
                  (LDecoded.Tracks[0].Events[I].Data[1] <> 96) then
                LEventOk := False;
              Inc(LNoteOns);
            end;
          end;
      end;
    end;
    Inc(LTick, LDecoded.Tracks[0].EndDeltaTicks);
    Check((LNoteOns = 13) and (LNoteOffs = 13) and LEventOk,
      'decoded note pairs preserve every authored pitch and velocity');
    Check(LTick = 13, 'decoded MIDI reaches the exact score end tick');
    Check(SameMidiBytes(EncodeWfcMidiFile(LDecoded), LBytes),
      'owned MIDI decode and re-encode are byte-exact');
    Check(SameMidiBytes(EncodeWfcMusicMidi(LScore), LBytes),
      'repeated owned MIDI encoding is deterministic');
  finally
    LScore.Free;
    LOwner.Free;
  end;
end;

function TagAt(const ABytes: TWfcMusicAudioBytes;
  const AOffset: Integer): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to 3 do Result := Result + Chr(ABytes[AOffset + I]);
end;

function U16At(const ABytes: TWfcMusicAudioBytes;
  const AOffset: Integer): TWfcMusicAudioStreamCount;
begin
  Result := TWfcMusicAudioStreamCount(ABytes[AOffset]) +
    TWfcMusicAudioStreamCount(ABytes[AOffset + 1]) * 256;
end;

function U32At(const ABytes: TWfcMusicAudioBytes;
  const AOffset: Integer): TWfcMusicAudioStreamCount;
begin
  Result := TWfcMusicAudioStreamCount(ABytes[AOffset]) +
    TWfcMusicAudioStreamCount(ABytes[AOffset + 1]) * 256 +
    TWfcMusicAudioStreamCount(ABytes[AOffset + 2]) * 65536 +
    TWfcMusicAudioStreamCount(ABytes[AOffset + 3]) * 16777216;
end;

procedure CheckWave(const ASink: TCountingPrefixSink;
  const ASampleRate: Integer; const AFrames: TWfcMusicAudioStreamCount;
  const ALabel: String);
var
  LDataBytes: TWfcMusicAudioStreamCount;
  LHeaderOk: Boolean;
begin
  LDataBytes := AFrames * 2;
  Check(ASink.TotalBytes = 44 + LDataBytes,
    ALabel + ' has the exact physical extent');
  LHeaderOk := Length(ASink.Prefix) >= 44;
  if LHeaderOk then
    LHeaderOk := (TagAt(ASink.Prefix, 0) = 'RIFF') and
      (TagAt(ASink.Prefix, 8) = 'WAVE') and
      (TagAt(ASink.Prefix, 12) = 'fmt ') and
      (TagAt(ASink.Prefix, 36) = 'data') and
      (U32At(ASink.Prefix, 4) = LDataBytes + 36) and
      (U32At(ASink.Prefix, 16) = 16) and
      (U16At(ASink.Prefix, 20) = 1) and
      (U16At(ASink.Prefix, 22) = 1) and
      (U32At(ASink.Prefix, 24) = ASampleRate) and
      (U32At(ASink.Prefix, 28) = ASampleRate * 2) and
      (U16At(ASink.Prefix, 32) = 2) and
      (U16At(ASink.Prefix, 34) = 16) and
      (U32At(ASink.Prefix, 40) = LDataBytes);
  Check(LHeaderOk, ALabel + ' has an exact mono PCM16 RIFF/WAVE header');
  Check((ASink.Calls > 1) and
    (ASink.MaxBlock <= WFC_MUSIC_AUDIO_STREAM_BLOCK_BYTES),
    ALabel + ' is emitted in bounded streaming blocks');
  Check(Length(ASink.Prefix) <= PREFIX_LIMIT,
    ALabel + ' counting sink retains only a bounded prefix');
end;

procedure TestStreamedWave;
var
  LOptions: TWfcMusicAudioOptions;
  LOwner: TSimpleMusic;
  LScore: TWfcMusicScore;
  LSink: TCountingPrefixSink;
begin
  LOwner := TSimpleMusic.Create;
  try
    LOptions := DefaultWfcMusicAudioOptions;
    LOptions.SampleRate := WFC_MUSIC_AUDIO_MIN_SAMPLE_RATE;
    LScore := LOwner.GenerateMusic(4, 500000, 55);
    LSink := TCountingPrefixSink.Create;
    try
      RenderSimpleMusicWave(LScore, LSink, LOptions);
      CheckWave(LSink, 32000, 64000, 'short streamed WAVE');
    finally
      LSink.Free;
      LScore.Free;
    end;

    LScore := LOwner.GenerateMusic(16, 4000000, 55);
    LSink := TCountingPrefixSink.Create;
    try
      RenderSimpleMusicWave(LScore, LSink, LOptions);
      CheckWave(LSink, 32000, 2048000,
        'sixty-four-second streamed WAVE');
      Check(LSink.TotalBytes = 4096044,
        'streaming exceeds sixty seconds without a preview-size cap');
    finally
      LSink.Free;
      LScore.Free;
    end;
  finally
    LOwner.Free;
  end;
end;

begin
  WriteLn('WFC music study conformance suite');
  WriteLn('=================================');
  RunTest('A-major grammar and deterministic reset',
    @TestScaleGrammarAndReplay);
  RunTest('all seven nonempty riff selections', @TestEveryRiffSelection);
  RunTest('invalid requests and owner reuse', @TestInvalidRequestsAndReuse);
  RunTest('owned MIDI encode and decode', @TestMidiRoundTrip);
  RunTest('bounded streamed WAVE output', @TestStreamedWave);
  WriteLn('=================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d music study checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
