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
program wfc_music_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc_model,
  wfc_music,
  wfc_music_sequence,
  wfc_music_text,
  wfc_sequence;

type
  TTestProcedure = procedure;

const
  GOLDEN_MUSIC_TEXT =
    'wfcmusic=1'#10 +
    'tpq=480'#10 +
    'steps=12'#10 +
    'length=4800'#10 +
    'tracks=2'#10 +
    'track=0,lead%E2%99%AB,Lead%20%E2%99%AB'#10 +
    'track=1,bass,Bass%E2%99%AB'#10 +
    'voices=2'#10 +
    'voice=0,0,melody%E2%99%AB'#10 +
    'voice=1,1,bass-line'#10 +
    'meters=2'#10 +
    'meter=0,0,4,4'#10 +
    'meter=1,1920,3,4'#10 +
    'tempos=2'#10 +
    'tempo=0,0,500000'#10 +
    'tempo=1,2400,400000'#10 +
    'spans=6'#10 +
    'span=0,0,0,960,N,60@100'#10 +
    'span=1,0,960,960,R,'#10 +
    'span=2,0,1920,1440,C,64@90;67@88'#10 +
    'span=3,0,3360,1440,N,72@96'#10 +
    'span=4,1,0,1920,R,'#10 +
    'span=5,1,1920,2880,C,36@80;43@75'#10 +
    'end'#10;

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

function TonesOf(const AValues: array of TWfcMusicTone): TWfcMusicTones;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function SpansOf(const AValues: array of TWfcMusicSpanEvent):
  TWfcMusicSpanEvents;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function UnicodeLeadId: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken('lead' + Chr($266B));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(
    UnicodeString('lead') + WideChar($266B)));
  {$ENDIF}
end;

function UnicodeLeadName: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken('Lead ' + Chr($266B));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(
    UnicodeString('Lead ') + WideChar($266B)));
  {$ENDIF}
end;

function UnicodeMelodyId: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken('melody' + Chr($266B));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(
    UnicodeString('melody') + WideChar($266B)));
  {$ENDIF}
end;

function UnicodeBassName: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken('Bass' + Chr($266B));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(
    UnicodeString('Bass') + WideChar($266B)));
  {$ENDIF}
end;

function ReplaceOnce(const AText, AOld, ANew: String): String;
var
  LPosition: Integer;
begin
  LPosition := Pos(AOld, AText);
  if LPosition = 0 then
    raise Exception.Create('music text test replacement was not found');
  Result := Copy(AText, 1, LPosition - 1) + ANew +
    Copy(AText, LPosition + Length(AOld),
      Length(AText) - LPosition - Length(AOld) + 1);
end;

function NewMusicTextFixtureScore: TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetLength(LTracks, 2);
  LTracks[0] := MakeWfcMusicTrack(UnicodeLeadId, UnicodeLeadName);
  LTracks[1] := MakeWfcMusicTrack('bass', UnicodeBassName);
  SetLength(LVoices, 2);
  LVoices[0] := MakeWfcMusicVoice(0, UnicodeMelodyId);
  LVoices[1] := MakeWfcMusicVoice(1, 'bass-line');
  SetLength(LMeters, 2);
  LMeters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  LMeters[1] := MakeWfcMusicMeterChange(1920, 3, 4);
  SetLength(LTempos, 2);
  LTempos[0] := MakeWfcMusicTempoChange(0, 500000);
  LTempos[1] := MakeWfcMusicTempoChange(2400, 400000);
  LSpans := SpansOf([
    MakeWfcMusicSound(0, 0, 960,
      TonesOf([MakeWfcMusicTone(60, 100)])),
    MakeWfcMusicRest(0, 960, 960),
    MakeWfcMusicSound(0, 1920, 1440,
      TonesOf([MakeWfcMusicTone(64, 90),
        MakeWfcMusicTone(67, 88)])),
    MakeWfcMusicSound(0, 3360, 1440,
      TonesOf([MakeWfcMusicTone(72, 96)])),
    MakeWfcMusicRest(1, 0, 1920),
    MakeWfcMusicSound(1, 1920, 2880,
      TonesOf([MakeWfcMusicTone(36, 80),
        MakeWfcMusicTone(43, 75)]))]);
  Result := TWfcMusicScore.Create(480, 12, 4800, LTracks,
    LVoices, LMeters, LTempos, LSpans);
end;

function CellsOf(const AValues: array of TWfcMusicMelodyCell):
  TWfcMusicMelodyCells;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function NewSingleVoiceScore(const ALengthTicks: Integer;
  const ASpans: TWfcMusicSpanEvents; const AMeterNumerator: Integer = 4;
  const AMeterDenominator: Integer = 4): TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetLength(LTracks, 1);
  LTracks[0] := MakeWfcMusicTrack('lead', 'Lead');
  SetLength(LVoices, 1);
  LVoices[0] := MakeWfcMusicVoice(0, 'melody');
  SetLength(LMeters, 1);
  LMeters[0] := MakeWfcMusicMeterChange(0, AMeterNumerator,
    AMeterDenominator);
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, 500000);
  Result := TWfcMusicScore.Create(480, 12, ALengthTicks, LTracks,
    LVoices, LMeters, LTempos, ASpans);
end;

function SpanMatches(const A, B: TWfcMusicSpanEvent): Boolean;
var
  I: Integer;
begin
  if (A.VoiceIndex <> B.VoiceIndex) or
      (A.StartTick <> B.StartTick) or
      (A.DurationTicks <> B.DurationTicks) or
      (A.Kind <> B.Kind) or (Length(A.Tones) <> Length(B.Tones)) then
    Exit(False);
  for I := 0 to Length(A.Tones) - 1 do
    if (A.Tones[I].Pitch <> B.Tones[I].Pitch) or
        (A.Tones[I].Velocity <> B.Tones[I].Velocity) then
      Exit(False);
  Result := True;
end;

function SpanArraysMatch(const A, B: TWfcMusicSpanEvents): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to Length(A) - 1 do
    if not SpanMatches(A[I], B[I]) then
      Exit(False);
  Result := True;
end;

function TokensMatch(const A: TWfcModelTokens;
  const B: array of TWfcModelToken): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to Length(B) - 1 do
    if A[I] <> B[I] then
      Exit(False);
  Result := True;
end;

function RationalRejected(const ANumerator, ADenominator: Integer): Boolean;
begin
  Result := False;
  try
    MakeWfcMusicRational(ANumerator, ADenominator);
  except
    on E: EWfcMusic do Result := True;
  end;
end;

function QuarterRejected(const ANumerator, ADenominator,
  ATicksPerQuarter: Integer): Boolean;
var
  LValue: TWfcMusicRational;
begin
  Result := False;
  try
    LValue.Numerator := ANumerator;
    LValue.Denominator := ADenominator;
    WfcMusicQuarterToTickExact(LValue, ATicksPerQuarter);
  except
    on E: EWfcMusic do Result := True;
  end;
end;

procedure TestRationalTime;
var
  LHalf: TWfcMusicRational;
  LOther: TWfcMusicRational;
begin
  LHalf := MakeWfcMusicRational(2, 4);
  Check((LHalf.Numerator = 1) and (LHalf.Denominator = 2),
    'rational construction reduces exact musical time');
  LOther.Numerator := 3;
  LOther.Denominator := 6;
  Check(CompareWfcMusicRational(LHalf, LOther) = 0,
    'rational comparison normalizes both operands');
  LOther := MakeWfcMusicRational(3, 4);
  Check(CompareWfcMusicRational(LHalf, LOther) < 0,
    'rational comparison retains exact ordering');
  LHalf := WfcMusicTickToQuarter(240, 480);
  Check((LHalf.Numerator = 1) and (LHalf.Denominator = 2),
    'tick conversion yields a reduced quarter-note value');
  LOther := MakeWfcMusicRational(3, 2);
  Check(WfcMusicQuarterToTickExact(LOther, 480) = 720,
    'exact quarter-note conversion returns integer ticks');
  Check(QuarterRejected(1, 7, 480),
    'inexact quarter-note conversion is rejected');
  Check(RationalRejected(-1, 2) and RationalRejected(1, 0),
    'negative and zero-denominator rationals are rejected');
end;

function InvalidScoreRejected(const ALength: Integer;
  const ASpans: TWfcMusicSpanEvents; const ANumerator,
  ADenominator: Integer): Boolean;
var
  LScore: TWfcMusicScore;
begin
  Result := False;
  LScore := nil;
  try
    try
      LScore := NewSingleVoiceScore(ALength, ASpans, ANumerator,
        ADenominator);
    except
      on E: EWfcMusic do Result := True;
    end;
  finally
    LScore.Free;
  end;
end;

function CoreScoreGuardRejected(const AKind: Integer): Boolean;
var
  LMeters: TWfcMusicMeterChanges;
  LScore: TWfcMusicScore;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTicksPerQuarter: Integer;
  LTones: TWfcMusicTones;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  Result := False;
  LScore := nil;
  LTicksPerQuarter := 480;
  SetLength(LTracks, 1);
  LTracks[0] := MakeWfcMusicTrack('track', 'Track');
  SetLength(LVoices, 1);
  LVoices[0] := MakeWfcMusicVoice(0, 'voice');
  SetLength(LMeters, 1);
  LMeters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, 500000);
  LSpans := SpansOf([MakeWfcMusicRest(0, 0, 1920)]);
  case AKind of
    0: LTicksPerQuarter := 0;
    1:
      begin
        SetLength(LTracks, 2);
        LTracks[1] := MakeWfcMusicTrack('track', 'Duplicate');
      end;
    2: LVoices[0] := MakeWfcMusicVoice(1, 'voice');
    3: LMeters[0] := MakeWfcMusicMeterChange(480, 4, 4);
    4: LTempos[0] := MakeWfcMusicTempoChange(0, 0);
    5: LSpans := SpansOf([MakeWfcMusicRest(0, 1, 1919)]);
    6:
      begin
        LTones := TonesOf([MakeWfcMusicTone(60, 0)]);
        LSpans := SpansOf([MakeWfcMusicSound(0, 0, 1920, LTones)]);
      end;
  end;
  try
    try
      LScore := TWfcMusicScore.Create(LTicksPerQuarter, 12, 1920,
        LTracks, LVoices, LMeters, LTempos, LSpans);
    except
      on E: EWfcMusic do Result := True;
    end;
  finally
    LScore.Free;
  end;
end;

procedure TestScoreInvariants;
var
  LScore: TWfcMusicScore;
  LSpans: TWfcMusicSpanEvents;
  LTones: TWfcMusicTones;
begin
  LSpans := SpansOf([
    MakeWfcMusicSound(0, 0, 480,
      TonesOf([MakeWfcMusicTone(69, 100)])),
    MakeWfcMusicRest(0, 480, 480),
    MakeWfcMusicSound(0, 960, 960,
      TonesOf([MakeWfcMusicTone(73, 90)]))]);
  LScore := NewSingleVoiceScore(1920, LSpans);
  try
    Check((LScore.TrackCount = 1) and (LScore.VoiceCount = 1) and
      (LScore.SpanCount = 3) and (LScore.LengthTicks = 1920),
      'a canonical voice exactly partitions a complete measure');
    Check((LScore.MeterAtTick(1919).Numerator = 4) and
      (LScore.TempoAtTick(1919).MicrosecondsPerQuarter = 500000),
      'meter and tempo lookup cover the final score tick');
    LSpans := LScore.CopyVoiceSpans(0);
    LSpans[0].Tones[0].Pitch := 1;
    Check(LScore.SpanAt(0).Tones[0].Pitch = 69,
      'score span copies detach nested tone arrays');
  finally
    LScore.Free;
  end;

  Check(InvalidScoreRejected(1900,
    SpansOf([MakeWfcMusicRest(0, 0, 1900)]), 4, 4),
    'a score ending within a measure is rejected');
  Check(InvalidScoreRejected(1920,
    SpansOf([MakeWfcMusicRest(0, 0, 1920)]), 4, 3),
    'a non-power-of-two meter denominator is rejected');
  Check(InvalidScoreRejected(1920,
    SpansOf([MakeWfcMusicRest(0, 0, 480),
      MakeWfcMusicRest(0, 480, 1440)]), 4, 4),
    'adjacent rest spans must be canonicalized into one span');
  LTones := TonesOf([MakeWfcMusicTone(72, 90),
    MakeWfcMusicTone(60, 90)]);
  Check(InvalidScoreRejected(1920,
    SpansOf([MakeWfcMusicSound(0, 0, 1920, LTones)]), 4, 4),
    'chord tones must use strictly increasing pitch order');
  Check(CoreScoreGuardRejected(0) and CoreScoreGuardRejected(1) and
    CoreScoreGuardRejected(2),
    'score identity requires positive timing, unique tracks and valid voices');
  Check(CoreScoreGuardRejected(3) and CoreScoreGuardRejected(4),
    'score meter and tempo timelines must begin validly at tick zero');
  Check(CoreScoreGuardRejected(5) and CoreScoreGuardRejected(6),
    'score voices reject timeline gaps and invalid tone velocity');
end;

function MusicTextDecodeRejected(const AText: String): Boolean;
var
  LScore: TWfcMusicScore;
begin
  Result := False;
  LScore := nil;
  try
    try
      LScore := DecodeWfcMusicText(AText);
    except
      on E: Exception do Result := True;
    end;
  finally
    LScore.Free;
  end;
end;

procedure TestCanonicalMusicText;
var
  LDecoded: TWfcMusicScore;
  LDetachedSpans: TWfcMusicSpanEvents;
  LEncoded: String;
  LScore: TWfcMusicScore;
begin
  LScore := NewMusicTextFixtureScore;
  try
    LEncoded := EncodeWfcMusicText(LScore);
    Check(LEncoded = GOLDEN_MUSIC_TEXT,
      'tracks, voices, metadata, rests, notes and chords have one exact artifact');
  finally
    LScore.Free;
  end;

  LDecoded := DecodeWfcMusicText(GOLDEN_MUSIC_TEXT);
  try
    Check(EncodeWfcMusicText(LDecoded) = GOLDEN_MUSIC_TEXT,
      'canonical music text decodes and re-encodes byte for byte');
    Check((LDecoded.TrackAt(0).Id = UnicodeLeadId) and
      (LDecoded.TrackAt(0).Name = UnicodeLeadName) and
      (LDecoded.TrackAt(1).Name = UnicodeBassName) and
      (LDecoded.VoiceAt(0).Id = UnicodeMelodyId),
      'percent-encoded Unicode track and voice identity round-trips');
    Check((LDecoded.MeterCount = 2) and
      (LDecoded.MeterAt(1).Numerator = 3) and
      (LDecoded.TempoCount = 2) and
      (LDecoded.TempoAt(1).MicrosecondsPerQuarter = 400000) and
      (LDecoded.SpanAt(0).Kind = wmskNote) and
      (LDecoded.SpanAt(1).Kind = wmskRest) and
      (LDecoded.SpanAt(2).Kind = wmskChord),
      'decoded public score semantics retain every encoded section');
    LDetachedSpans := LDecoded.CopySpans;
    LDetachedSpans[2].Tones[0].Pitch := 1;
    LDetachedSpans[5].Tones[1].Velocity := 1;
    Check((LDecoded.SpanAt(2).Tones[0].Pitch = 64) and
      (LDecoded.SpanAt(5).Tones[1].Velocity = 75),
      'decoded span copies deeply detach nested chord-tone arrays');
  finally
    LDecoded.Free;
  end;

  Check(MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
    #10, #13#10)), 'music text rejects CRLF input');
  Check(MusicTextDecodeRejected(Copy(GOLDEN_MUSIC_TEXT, 1,
    Length(GOLDEN_MUSIC_TEXT) - 1)),
    'music text requires a final LF');
  Check(MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
    'tracks=2'#10 +
      'track=0,lead%E2%99%AB,Lead%20%E2%99%AB'#10,
    'track=0,lead%E2%99%AB,Lead%20%E2%99%AB'#10 +
      'tracks=2'#10)),
    'music text rejects reordered sections');
  Check(MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
    'spans=6', 'spans=7')),
    'music text rejects a declared count that exceeds its records');
  Check(MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
    'tracks=2', 'tracks=2147483647')) and
    MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
      'voices=2', 'voices=2147483647')) and
    MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
      'meters=2', 'meters=2147483647')) and
    MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
      'tempos=2', 'tempos=2147483647')) and
    MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
      'spans=6', 'spans=2147483647')),
    'music text bounds every declared count by remaining input lines');
  Check(MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
    'span=0,0,0,960,N,60@100', 'span=1,0,0,960,N,60@100')),
    'music text rejects a noncontiguous record index');
  Check(MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
    'tpq=480', 'tpq=0480')),
    'music text rejects leading-zero integers');
  Check(MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
    'lead%E2%99%AB', '%6Cead%E2%99%AB')) and
    MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
      '%E2%99%AB', '%e2%99%AB')),
    'music text rejects unnecessary and lowercase percent escapes');
  Check(MusicTextDecodeRejected(GOLDEN_MUSIC_TEXT + 'extra'#10),
    'music text rejects trailing data');

  Check(MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
    'track=1,bass,Bass%E2%99%AB',
    'track=1,lead%E2%99%AB,Bass%E2%99%AB')),
    'decoded text is rejected by the constructor for duplicate track identity');
  Check(MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
    'meter=1,1920,3,4', 'meter=1,960,3,4')),
    'decoded text is rejected for a non-boundary meter change');
  Check(MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
    'span=0,0,0,960,N,60@100', 'span=0,0,0,959,N,60@100')),
    'decoded text is rejected when spans do not exactly partition a voice');
  Check(MusicTextDecodeRejected(ReplaceOnce(GOLDEN_MUSIC_TEXT,
    '64@90;67@88', '64@90;67@0')),
    'decoded text is rejected for a constructor-invalid chord tone');
end;

function MelodyDecodeRejected(const AToken: TWfcModelToken): Boolean;
begin
  Result := False;
  try
    DecodeWfcMusicMelodyCell(AToken);
  except
    on E: EWfcMusicSequence do Result := True;
  end;
end;

function RhythmDecodeRejected(const AToken: TWfcModelToken): Boolean;
begin
  Result := False;
  try
    DecodeWfcMusicRhythmCell(AToken);
  except
    on E: EWfcMusicSequence do Result := True;
  end;
end;

function HarmonyDecodeRejected(const AToken: TWfcModelToken): Boolean;
begin
  Result := False;
  try
    DecodeWfcMusicHarmonyCell(AToken);
  except
    on E: EWfcMusicSequence do Result := True;
  end;
end;

function InvalidCellEnumsRejected: Boolean;
var
  LHarmony: TWfcMusicHarmonyCell;
  LInvalid: Integer;
  LMelody: TWfcMusicMelodyCell;
  LRhythm: TWfcMusicRhythmCell;
begin
  Result := False;
  LInvalid := Ord(High(TWfcMusicCellAction)) + 1;
  LMelody := Default(TWfcMusicMelodyCell);
  LMelody.Pitch := 60;
  LMelody.Velocity := 100;
  LRhythm := Default(TWfcMusicRhythmCell);
  LHarmony := Default(TWfcMusicHarmonyCell);
  LHarmony.StepsPerOctave := 12;
  try
    {$R-}
    LMelody.Action := TWfcMusicCellAction(LInvalid);
    LRhythm.Action := TWfcMusicCellAction(LInvalid);
    LInvalid := Ord(High(TWfcMusicHarmonyCellKind)) + 1;
    LHarmony.Kind := TWfcMusicHarmonyCellKind(LInvalid);
    {$R+}
  except
    on E: ERangeError do
      Exit(True);
  end;
  try
    EncodeWfcMusicMelodyCell(LMelody);
    Exit;
  except
    on E: EWfcMusicSequence do;
  end;
  try
    EncodeWfcMusicRhythmCell(LRhythm);
    Exit;
  except
    on E: EWfcMusicSequence do;
  end;
  try
    EncodeWfcMusicHarmonyCell(LHarmony);
  except
    on E: EWfcMusicSequence do
      Result := True;
  end;
end;

procedure TestCanonicalCellCodecs;
var
  LHarmony: TWfcMusicHarmonyCell;
  LMelody: TWfcMusicMelodyCell;
  LRhythm: TWfcMusicRhythmCell;
begin
  Check(EncodeWfcMusicMelodyCell(MakeWfcMusicRestCell) = 'wm1:r',
    'melody rests have one canonical token');
  Check(EncodeWfcMusicMelodyCell(
    MakeWfcMusicAttackCell(69, 100)) = 'wm1:a:69:100',
    'melody attacks encode pitch and velocity canonically');
  Check(EncodeWfcMusicMelodyCell(
    MakeWfcMusicHoldCell(69, 100)) = 'wm1:h:69:100',
    'melody holds remain independently meaningful');
  LMelody := DecodeWfcMusicMelodyCell('wm1:a:69:100');
  Check((LMelody.Action = wmcaAttack) and (LMelody.Pitch = 69) and
    (LMelody.Velocity = 100), 'melody token decoding is exact');

  LRhythm := DecodeWfcMusicRhythmCell('wr1:h');
  Check((LRhythm.Action = wmcaHold) and
    (EncodeWfcMusicRhythmCell(LRhythm) = 'wr1:h'),
    'rhythm tokens round-trip their articulation');
  LHarmony := DecodeWfcMusicHarmonyCell('wh1:p:12:9');
  Check((LHarmony.Kind = wmhckPitchClass) and
    (LHarmony.StepsPerOctave = 12) and (LHarmony.PitchClass = 9) and
    (EncodeWfcMusicHarmonyCell(LHarmony) = 'wh1:p:12:9'),
    'harmony tokens carry their pitch system and class');
  LHarmony := DecodeWfcMusicHarmonyCell('wh1:r:12:0');
  Check((LHarmony.Kind = wmhckRest) and
    (EncodeWfcMusicHarmonyCell(LHarmony) = 'wh1:r:12:0'),
    'harmony silence is explicit rather than a pitch sentinel');

  Check(MelodyDecodeRejected('wm2:r') and
    MelodyDecodeRejected('wm1:a:069:100') and
    MelodyDecodeRejected('wm1:a:-1:100') and
    MelodyDecodeRejected('wm1:r:0') and
    MelodyDecodeRejected('wm1:a:69:0') and
    MelodyDecodeRejected('wm1:r:'),
    'melody decoder rejects versions, noncanonical fields and invalid data');
  Check(RhythmDecodeRejected('wr1:x') and
    RhythmDecodeRejected('wr1:a:extra') and RhythmDecodeRejected('wr1:a:'),
    'rhythm decoder rejects unknown actions and trailing fields');
  Check(HarmonyDecodeRejected('wh1:p:12:12') and
    HarmonyDecodeRejected('wh1:r:12:1') and
    HarmonyDecodeRejected('wh1:p:0:0') and
    HarmonyDecodeRejected('wh1:p:12:9:'),
    'harmony decoder rejects invalid pitch systems and rest payloads');
  Check(InvalidCellEnumsRejected,
    'cell encoders reject invalid enum bit patterns');
end;

function ProjectionRejected(const AScore: TWfcMusicScore;
  const AQuantum: Integer): Boolean;
begin
  Result := False;
  try
    ProjectWfcMusicVoiceToMelodyCells(AScore, 0, AQuantum);
  except
    on E: EWfcMusicSequence do Result := True;
  end;
end;

function RebuildRejected(const ACells: TWfcMusicMelodyCells): Boolean;
begin
  Result := False;
  try
    RebuildWfcMusicVoiceSpans(ACells, 0, 240);
  except
    on E: EWfcMusicSequence do Result := True;
  end;
end;

procedure TestQuantumProjectionAndRebuild;
var
  LCells: TWfcMusicMelodyCells;
  LExpected: TWfcMusicSpanEvents;
  LRebuilt: TWfcMusicSpanEvents;
  LScore: TWfcMusicScore;
begin
  LExpected := SpansOf([
    MakeWfcMusicSound(0, 0, 480,
      TonesOf([MakeWfcMusicTone(60, 100)])),
    MakeWfcMusicRest(0, 480, 240),
    MakeWfcMusicSound(0, 720, 480,
      TonesOf([MakeWfcMusicTone(62, 90)])),
    MakeWfcMusicRest(0, 1200, 720)]);
  LScore := NewSingleVoiceScore(1920, LExpected);
  try
    LCells := ProjectWfcMusicVoiceToMelodyCells(LScore, 0, 240);
    Check((Length(LCells) = 8) and
      (LCells[0].Action = wmcaAttack) and
      (LCells[1].Action = wmcaHold) and
      (LCells[2].Action = wmcaRest) and
      (LCells[3].Action = wmcaAttack) and
      (LCells[4].Action = wmcaHold) and
      (LCells[5].Action = wmcaRest) and
      (LCells[7].Action = wmcaRest),
      'one score voice projects to exact attack, hold and rest quanta');
    LRebuilt := RebuildWfcMusicVoiceSpans(LCells, 0, 240);
    Check(SpanArraysMatch(LRebuilt, LExpected),
      'quantum cells rebuild the original canonical voice exactly');
    Check(TokensMatch(EncodeWfcMusicMelodyCells(LCells),
      ['wm1:a:60:100', 'wm1:h:60:100', 'wm1:r',
       'wm1:a:62:90', 'wm1:h:62:90', 'wm1:r', 'wm1:r', 'wm1:r']),
      'projected melody cells have stable public sequence tokens');
  finally
    LScore.Free;
  end;

  LScore := NewSingleVoiceScore(1920, SpansOf([
    MakeWfcMusicSound(0, 0, 360,
      TonesOf([MakeWfcMusicTone(60, 100)])),
    MakeWfcMusicRest(0, 360, 1560)]));
  try
    Check(ProjectionRejected(LScore, 240),
      'projection rejects spans not aligned to the selected quantum');
  finally
    LScore.Free;
  end;
  LScore := NewSingleVoiceScore(1920, SpansOf([
    MakeWfcMusicSound(0, 0, 1920,
      TonesOf([MakeWfcMusicTone(60, 100),
        MakeWfcMusicTone(64, 100)]))]));
  try
    Check(ProjectionRejected(LScore, 240),
      'melody projection rejects chords instead of losing tones');
  finally
    LScore.Free;
  end;

  Check(RebuildRejected(CellsOf([MakeWfcMusicHoldCell(60, 100)])),
    'rebuild rejects a hold without an attack');
  Check(RebuildRejected(CellsOf([MakeWfcMusicAttackCell(60, 100),
    MakeWfcMusicHoldCell(61, 100)])),
    'rebuild rejects a hold whose pitch differs from its attack');
  Check(RebuildRejected(nil),
    'rebuild rejects an empty generated melody');
end;

procedure TestRhythmAndHarmonyProjection;
var
  LCells: TWfcMusicMelodyCells;
  LHarmony: TWfcMusicHarmonyCells;
  LRhythm: TWfcMusicRhythmCells;
begin
  LCells := CellsOf([MakeWfcMusicAttackCell(69, 100),
    MakeWfcMusicHoldCell(69, 100), MakeWfcMusicRestCell,
    MakeWfcMusicAttackCell(72, 90)]);
  LRhythm := ProjectWfcMusicMelodyToRhythm(LCells);
  LHarmony := ProjectWfcMusicMelodyToHarmony(LCells, 12);
  Check(TokensMatch(EncodeWfcMusicRhythmCells(LRhythm),
    ['wr1:a', 'wr1:h', 'wr1:r', 'wr1:a']),
    'rhythm projection removes pitch while retaining articulation');
  Check(TokensMatch(EncodeWfcMusicHarmonyCells(LHarmony),
    ['wh1:p:12:9', 'wh1:p:12:9', 'wh1:r:12:0', 'wh1:p:12:0']),
    'harmony projection folds sounding pitches into exact pitch classes');
end;

function ContainsDirectedEdge(const AEdges: array of Integer;
  const ASource, ATarget: Integer): Boolean;
var
  I: Integer;
begin
  I := 0;
  while I + 1 < Length(AEdges) do
  begin
    if (AEdges[I] = ASource) and (AEdges[I + 1] = ATarget) then
      Exit(True);
    Inc(I, 2);
  end;
  Result := False;
end;

procedure TestLegacyDirectedFixtures;
const
  { Relative chromatic pitches retain the 2021 A=0 convention without
    misrepresenting those values as MIDI note numbers. }
  A_MAJOR: array[0..15] of Integer =
    (0, 2, 2, 4, 4, 5, 5, 7, 7, 9, 9, 11, 11, 12, 12, 0);
  A_SCALE: array[0..8] of Integer = (0, 2, 4, 5, 7, 9, 11, 12, 0);
  MARY: array[0..15] of Integer =
    (4, 2, 4, 4, 4, 7, 2, 4, 2, 0, 0, 2, 7, 4, 7, 7);
  BRIDGE: array[0..19] of Integer =
    (2, 4, 2, 0, 2, 9, 2, 11, 4, 2, 0, 2, 0, 11, 9, 2,
     9, 11, 11, 0);
  HOT_CROSS: array[0..11] of Integer =
    (4, 2, 4, 0, 2, 4, 2, 0, 0, 4, 0, 2);
var
  I: Integer;
begin
  for I := 0 to Length(A_SCALE) - 2 do
    Check(ContainsDirectedEdge(A_MAJOR, A_SCALE[I], A_SCALE[I + 1]),
      'A-major retains directed scale edge ' + IntToStr(I));
  Check(ContainsDirectedEdge(A_MAJOR, 12, 0) and
    not ContainsDirectedEdge(A_MAJOR, 0, 12),
    'A-major retains the intentional upper-A octave drop');

  Check(ContainsDirectedEdge(MARY, 4, 7) and
    ContainsDirectedEdge(MARY, 7, 4) and
    ContainsDirectedEdge(MARY, 0, 2),
    'Mary fixture expands bidirectional declarations exactly');
  Check(ContainsDirectedEdge(BRIDGE, 2, 11) and
    not ContainsDirectedEdge(BRIDGE, 11, 2) and
    ContainsDirectedEdge(BRIDGE, 9, 11),
    'London Bridge preserves asymmetric east/west declarations');
  Check(ContainsDirectedEdge(HOT_CROSS, 4, 0) and
    ContainsDirectedEdge(HOT_CROSS, 0, 4) and
    not ContainsDirectedEdge(HOT_CROSS, 4, 4),
    'Hot Cross Buns preserves its exact directed relation set');
end;

procedure TestLegacyScaleSequence;
var
  I: Integer;
  LCells: TWfcMusicMelodyCells;
  LHarmony: TWfcMusicHarmonyCells;
  LModel: TWfcSequenceModel;
const
  PITCHES: array[0..8] of Integer = (0, 2, 4, 5, 7, 9, 11, 12, 0);
begin
  SetLength(LCells, Length(PITCHES));
  for I := 0 to Length(PITCHES) - 1 do
    LCells[I] := MakeWfcMusicAttackCell(PITCHES[I], 100);
  LHarmony := ProjectWfcMusicMelodyToHarmony(LCells, 12);
  Check((LHarmony[0].PitchClass = 0) and
    (LHarmony[7].PitchClass = 0) and (LHarmony[8].PitchClass = 0),
    'relative A, upper A and returned A share one pitch class');
  LModel := LearnWfcMusicMelodySequence(LCells, 2);
  try
    Check((LModel.Order = 2) and (LModel.StateCount = 9) and
      (LModel.StartCountAt(0) = 1) and (LModel.EndCountAt(8) = 1),
      'legacy A-major cells learn as a bounded order-two melody');
    for I := 0 to LModel.StateCount - 2 do
      Check(LModel.StatesCompatible(I, I + 1),
        'learned A-major preserves adjacent state ' + IntToStr(I));
    Check(LModel.StatesCompatible(8, 1),
      'returned A can structurally begin another scale traversal');
  finally
    LModel.Free;
  end;
end;

procedure TestVersions;
begin
  Check((WFC_MUSIC_MODEL_VERSION = 1) and
    (WFC_MUSIC_VALIDATION_VERSION = 1) and
    (WFC_MUSIC_SEQUENCE_VERSION = 1) and
    (WFC_MUSIC_CELL_TOKEN_VERSION = 1) and
    (WFC_MUSIC_TEXT_VERSION = 1),
    'music model, validation, sequence, token and text surfaces are version one');
end;

begin
  WriteLn('WFC music foundation conformance suite');
  WriteLn('======================================');
  RunTest('rational musical time', @TestRationalTime);
  RunTest('score invariants', @TestScoreInvariants);
  RunTest('canonical music text', @TestCanonicalMusicText);
  RunTest('canonical cell codecs', @TestCanonicalCellCodecs);
  RunTest('quantum projection and rebuild',
    @TestQuantumProjectionAndRebuild);
  RunTest('rhythm and harmony projection',
    @TestRhythmAndHarmonyProjection);
  RunTest('legacy directed fixtures', @TestLegacyDirectedFixtures);
  RunTest('legacy scale sequence', @TestLegacyScaleSequence);
  RunTest('version surfaces', @TestVersions);
  WriteLn('======================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d music checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
