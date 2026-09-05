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
program wfc_music_midi_import_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc_music,
  wfc_music_text,
  wfc_music_midi,
  wfc_midi_smf,
  wfc_music_midi_import;

type
  TTestProcedure = procedure;

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

function EmptyFile(const AFormat, ATrackCount, ATpq,
  AEndTick: Integer): TWfcMidiFile;
var
  I: Integer;
begin
  Result := Default(TWfcMidiFile);
  Result.Format := AFormat;
  Result.TicksPerQuarter := ATpq;
  SetLength(Result.Tracks, ATrackCount);
  for I := 0 to ATrackCount - 1 do
    Result.Tracks[I].EndDeltaTicks := AEndTick;
end;

procedure AddEvent(var AFile: TWfcMidiFile; const ATrack: Integer;
  const AEvent: TWfcMidiEvent);
var
  LCount: Integer;
begin
  LCount := Length(AFile.Tracks[ATrack].Events);
  SetLength(AFile.Tracks[ATrack].Events, LCount + 1);
  AFile.Tracks[ATrack].Events[LCount] := AEvent;
end;

procedure AddOn(var AFile: TWfcMidiFile; const ATrack: Integer;
  const ADelta: Cardinal; const AChannel, APitch, AVelocity: Byte);
begin
  AddEvent(AFile, ATrack, MakeWfcMidiChannelEvent(ADelta,
    $90 or AChannel, [APitch, AVelocity]));
end;

procedure AddOff(var AFile: TWfcMidiFile; const ATrack: Integer;
  const ADelta: Cardinal; const AChannel, APitch, AVelocity: Byte);
begin
  AddEvent(AFile, ATrack, MakeWfcMidiChannelEvent(ADelta,
    $80 or AChannel, [APitch, AVelocity]));
end;

function SingleNote: TWfcMidiFile;
begin
  Result := EmptyFile(0, 1, 120, 360);
  AddOn(Result, 0, 0, 0, 60, 100);
  AddOff(Result, 0, 120, 0, 60, 0);
end;

function PolyphonicFile: TWfcMidiFile;
begin
  Result := EmptyFile(0, 1, 120, 120);
  AddOn(Result, 0, 0, 0, 60, 100);
  AddOn(Result, 0, 60, 0, 64, 80);
  AddOff(Result, 0, 60, 0, 64, 0);
  AddOn(Result, 0, 0, 0, 67, 90);
  AddOff(Result, 0, 60, 0, 67, 0);
  AddOff(Result, 0, 180, 0, 60, 0);
end;

procedure SeedReport(var AReport: TWfcMusicMidiImportReport);
begin
  AReport := Default(TWfcMusicMidiImportReport);
  AReport.SourceFormat := 7;
  AReport.SourceTrackCount := 7;
  AReport.SourceEventCount := 7;
  AReport.NoteCount := 7;
  AReport.SourceLengthTicks := 7;
  AReport.ScoreLengthTicks := 7;
  AReport.PaddingTicks := 7;
  AReport.IgnoredChannelEvents := 7;
  AReport.IgnoredSystemEvents := 7;
  AReport.IgnoredMetaEvents := 7;
  AReport.OmittedTrackNames := 7;
  AReport.DiscardedReleaseVelocities := 7;
  AReport.TerminalTimingEvents := 7;
  AReport.RedundantTimingEvents := 7;
  AReport.UsedDefaultTempo := True;
  AReport.UsedDefaultMeter := True;
  AReport.AddedSilentVoice := True;
  SetLength(AReport.Voices, 1);
  AReport.Voices[0].SourceTrack := 7;
  AReport.Voices[0].Channel := 7;
  AReport.Voices[0].Lane := 7;
end;

function ReportIsEmpty(const AReport: TWfcMusicMidiImportReport): Boolean;
begin
  Result := (AReport.SourceFormat = 0) and
    (AReport.SourceTrackCount = 0) and
    (AReport.SourceEventCount = 0) and (AReport.NoteCount = 0) and
    (AReport.SourceLengthTicks = 0) and (AReport.ScoreLengthTicks = 0) and
    (AReport.PaddingTicks = 0) and (AReport.IgnoredChannelEvents = 0) and
    (AReport.IgnoredSystemEvents = 0) and (AReport.IgnoredMetaEvents = 0) and
    (AReport.OmittedTrackNames = 0) and
    (AReport.DiscardedReleaseVelocities = 0) and
    (AReport.TerminalTimingEvents = 0) and
    (AReport.RedundantTimingEvents = 0) and
    (not AReport.UsedDefaultTempo) and (not AReport.UsedDefaultMeter) and
    (not AReport.AddedSilentVoice) and (Length(AReport.Voices) = 0);
end;

procedure ExpectRejected(const AFile: TWfcMidiFile;
  const AOptions: TWfcMusicMidiImportOptions; const ALabel: String);
var
  LReport: TWfcMusicMidiImportReport;
  LScore: TWfcMusicScore;
  LRaised: Boolean;
begin
  SeedReport(LReport);
  LScore := nil;
  LRaised := False;
  try
    LScore := ImportWfcMusicMidi(AFile, AOptions, LReport);
  except
    on E: EWfcMusicMidiImport do LRaised := True;
  end;
  LScore.Free;
  Check(LRaised, ALabel + ' is rejected');
  Check(ReportIsEmpty(LReport), ALabel + ' clears the entire report');
end;

procedure ExpectDefaultRejected(const AFile: TWfcMidiFile;
  const ALabel: String);
begin
  ExpectRejected(AFile, DefaultWfcMusicMidiImportOptions, ALabel);
end;

procedure CheckNote(const AScore: TWfcMusicScore; const AIndex,
  AVoice, AStart, ADuration, APitch, AVelocity: Integer;
  const ALabel: String);
var
  LSpan: TWfcMusicSpanEvent;
begin
  LSpan := AScore.SpanAt(AIndex);
  Check((LSpan.Kind = wmskNote) and (LSpan.VoiceIndex = AVoice) and
    (LSpan.StartTick = AStart) and (LSpan.DurationTicks = ADuration) and
    (Length(LSpan.Tones) = 1), ALabel + ' exact interval');
  if Length(LSpan.Tones) = 1 then
    Check((LSpan.Tones[0].Pitch = APitch) and
      (LSpan.Tones[0].Velocity = AVelocity), ALabel + ' pitch and velocity');
end;

procedure CheckRest(const AScore: TWfcMusicScore; const AIndex,
  AVoice, AStart, ADuration: Integer; const ALabel: String);
var
  LSpan: TWfcMusicSpanEvent;
begin
  LSpan := AScore.SpanAt(AIndex);
  Check((LSpan.Kind = wmskRest) and (LSpan.VoiceIndex = AVoice) and
    (LSpan.StartTick = AStart) and (LSpan.DurationTicks = ADuration) and
    (Length(LSpan.Tones) = 0), ALabel);
end;

function FindVoice(const AReport: TWfcMusicMidiImportReport;
  const ATrack, AChannel, ALane: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(AReport.Voices) - 1 do
    if (AReport.Voices[I].SourceTrack = ATrack) and
      (AReport.Voices[I].Channel = AChannel) and
      (AReport.Voices[I].Lane = ALane) then
      Exit(I);
end;

procedure TestDefaultsAndExactImport;
var
  LOptions: TWfcMusicMidiImportOptions;
  LReport: TWfcMusicMidiImportReport;
  LScore: TWfcMusicScore;
begin
  LOptions := DefaultWfcMusicMidiImportOptions;
  Check((WFC_MUSIC_MIDI_IMPORT_VERSION = 1) and
    (LOptions.UnsupportedEvents = wmmupReject) and
    (LOptions.EndPolicy = wmmepPadMeasure),
    'version and conservative projection defaults');
  LScore := ImportWfcMusicMidi(SingleNote, LOptions, LReport);
  try
    Check((LScore.TicksPerQuarter = 120) and
      (LScore.StepsPerOctave = 12) and (LScore.LengthTicks = 480),
      'PPQ ticks and twelve-step pitches stay exact');
    Check((LScore.TrackCount = 1) and (LScore.VoiceCount = 1) and
      (LScore.SpanCount = 2), 'one source lane and its tail rest');
    CheckNote(LScore, 0, 0, 0, 120, 60, 100, 'source note');
    CheckRest(LScore, 1, 0, 120, 360, 'end-of-track silence is preserved');
    Check((LReport.SourceFormat = 0) and
      (LReport.SourceTrackCount = 1) and (LReport.SourceEventCount = 3) and
      (LReport.NoteCount = 1), 'source accounting includes end-of-track');
    Check((LReport.SourceLengthTicks = 480) and
      (LReport.ScoreLengthTicks = 480) and (LReport.PaddingTicks = 0),
      'already complete measure adds no padding');
    Check(LReport.UsedDefaultMeter and LReport.UsedDefaultTempo and
      (not LReport.AddedSilentVoice), 'implicit timing defaults are reported');
    Check((LScore.MeterAt(0).Numerator = 4) and
      (LScore.MeterAt(0).Denominator = 4) and
      (LScore.TempoAt(0).MicrosecondsPerQuarter = 500000),
      'absent timing becomes initial 4/4 and 500000 microseconds');
    Check(FindVoice(LReport, 0, 0, 0) = 0,
      'first lane exposes source track and channel');
  finally
    LScore.Free;
  end;
end;

procedure TestAttackPreservingLanes;
var
  LFile: TWfcMidiFile;
  LReport: TWfcMusicMidiImportReport;
  LScore: TWfcMusicScore;
begin
  LFile := PolyphonicFile;
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    Check((LScore.VoiceCount = 2) and (LScore.SpanCount = 6) and
      (LReport.NoteCount = 3), 'overlapping intervals produce two exact lanes');
    CheckNote(LScore, 0, 0, 0, 360, 60, 100,
      'sustained C is not retriggered at unrelated onsets');
    CheckRest(LScore, 1, 0, 360, 120, 'first lane tail rest');
    CheckRest(LScore, 2, 1, 0, 60, 'second lane leading rest');
    CheckNote(LScore, 3, 1, 60, 60, 64, 80, 'second lane E');
    CheckNote(LScore, 4, 1, 120, 60, 67, 90, 'first-free lane reuses released E');
    CheckRest(LScore, 5, 1, 180, 300, 'second lane merged tail rest');
    Check((FindVoice(LReport, 0, 0, 0) = 0) and
      (FindVoice(LReport, 0, 0, 1) = 1), 'lane allocation is deterministic');
  finally
    LScore.Free;
  end;

  LFile := EmptyFile(0, 1, 120, 240);
  AddOn(LFile, 0, 0, 0, 60, 100);
  AddOff(LFile, 0, 120, 0, 60, 0);
  AddOn(LFile, 0, 0, 0, 60, 70);
  AddOn(LFile, 0, 120, 0, 60, 0);
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    Check((LScore.VoiceCount = 1) and (LScore.SpanCount = 3),
      'contiguous same-pitch reattack stays a separate span');
    CheckNote(LScore, 0, 0, 0, 120, 60, 100, 'first same-pitch attack');
    CheckNote(LScore, 1, 0, 120, 120, 60, 70,
      'second same-pitch attack and zero-velocity release');
  finally
    LScore.Free;
  end;
end;

procedure TestMergedTrackOwnership;
var
  LFile: TWfcMidiFile;
  LReport: TWfcMusicMidiImportReport;
  LScore: TWfcMusicScore;
  LSpans: TWfcMusicSpanEvents;
  LVoice: Integer;
begin
  LFile := EmptyFile(1, 3, 120, 0);
  LFile.Tracks[0].EndDeltaTicks := 960;
  AddEvent(LFile, 0, MakeWfcMidiTempoEvent(0, 400000));
  AddOn(LFile, 1, 0, 2, 60, 99);
  AddOn(LFile, 1, 120, 3, 60, 77);
  AddOff(LFile, 1, 120, 3, 60, 0);
  AddOff(LFile, 2, 360, 2, 60, 0);
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    Check((LReport.SourceFormat = 1) and (LReport.SourceTrackCount = 3) and
      (LReport.SourceEventCount = 8) and (LReport.SourceLengthTicks = 960),
      'merged format-one source retains conductor extent and every EOT');
    Check((LScore.VoiceCount = 2) and (LScore.LengthTicks = 960),
      'same pitch on different channels is valid polyphony');
    LVoice := FindVoice(LReport, 1, 2, 0);
    Check(LVoice >= 0, 'cross-track release retains NoteOn track ownership');
    if LVoice >= 0 then
    begin
      LSpans := LScore.CopyVoiceSpans(LVoice);
      Check((Length(LSpans) = 2) and (LSpans[0].StartTick = 0) and
        (LSpans[0].DurationTicks = 360),
        'global channel-pitch pairing crosses track boundaries');
    end;
    Check(FindVoice(LReport, 1, 3, 0) >= 0,
      'another source channel has its own lane');
    Check(FindVoice(LReport, 2, 2, 0) = -1,
      'release-only track cannot steal note provenance');
    Check((not LReport.UsedDefaultTempo) and
      (LScore.TempoAt(0).MicrosecondsPerQuarter = 400000),
      'conductor tempo applies across every lane');
  finally
    LScore.Free;
  end;
end;

procedure TestPairingRejections;
var
  LFile: TWfcMidiFile;
begin
  LFile := EmptyFile(0, 1, 120, 480);
  AddOff(LFile, 0, 0, 0, 60, 0);
  ExpectDefaultRejected(LFile, 'unmatched explicit release');
  LFile := EmptyFile(0, 1, 120, 480);
  AddOn(LFile, 0, 0, 0, 60, 0);
  ExpectDefaultRejected(LFile, 'unmatched velocity-zero release');
  LFile := EmptyFile(0, 1, 120, 480);
  AddOn(LFile, 0, 0, 0, 60, 90);
  ExpectDefaultRejected(LFile, 'unclosed note at end-of-track');
  LFile := EmptyFile(0, 1, 120, 480);
  AddOn(LFile, 0, 0, 0, 60, 90);
  AddOff(LFile, 0, 0, 0, 60, 0);
  ExpectDefaultRejected(LFile, 'zero-duration note');
  LFile := EmptyFile(0, 1, 120, 360);
  AddOn(LFile, 0, 0, 0, 60, 90);
  AddOn(LFile, 0, 120, 0, 60, 80);
  AddOff(LFile, 0, 0, 0, 60, 0);
  ExpectDefaultRejected(LFile, 'same-tick attack before old release');
  LFile := EmptyFile(1, 2, 120, 360);
  AddOn(LFile, 0, 0, 2, 60, 90);
  AddOff(LFile, 0, 120, 2, 60, 0);
  AddOn(LFile, 1, 0, 2, 60, 80);
  AddOff(LFile, 1, 120, 2, 60, 0);
  ExpectDefaultRejected(LFile, 'same-key ambiguity across source tracks');
  LFile := SingleNote;
  LFile.Tracks[0].Events[1].Status := $81;
  ExpectDefaultRejected(LFile, 'release on another channel');

  LFile := EmptyFile(1, 2, 120, 480);
  AddOff(LFile, 0, 0, 0, 60, 0);
  AddOn(LFile, 1, 0, 0, 60, 100);
  ExpectDefaultRejected(LFile,
    'same-tick track ordering cannot move an early release after an attack');
end;

procedure TestTimingAndPadding;
var
  LFile: TWfcMidiFile;
  LOptions: TWfcMusicMidiImportOptions;
  LReport: TWfcMusicMidiImportReport;
  LScore: TWfcMusicScore;
begin
  LOptions := DefaultWfcMusicMidiImportOptions;
  LFile := SingleNote;
  LFile.Tracks[0].EndDeltaTicks := 10;
  LScore := ImportWfcMusicMidi(LFile, LOptions, LReport);
  try
    Check((LReport.SourceLengthTicks = 130) and
      (LReport.ScoreLengthTicks = 480) and (LReport.PaddingTicks = 350),
      'padding exposes both source and complete-measure lengths');
    CheckNote(LScore, 0, 0, 0, 120, 60, 100,
      'padding does not alter the original note');
  finally
    LScore.Free;
  end;
  LOptions.EndPolicy := wmmepRequireMeasure;
  ExpectRejected(LFile, LOptions, 'partial measure under exact-end policy');

  LFile := EmptyFile(0, 1, 120, 480);
  AddEvent(LFile, 0, MakeWfcMidiTempoEvent(120, 600000));
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    Check((LScore.TempoCount = 2) and LReport.UsedDefaultTempo and
      (LScore.TempoAt(0).Tick = 0) and
      (LScore.TempoAt(1).Tick = 120) and
      (LScore.TempoAt(1).MicrosecondsPerQuarter = 600000),
      'late first tempo retains the initial default and exact change tick');
  finally
    LScore.Free;
  end;

  LFile := EmptyFile(0, 1, 120, 360);
  AddEvent(LFile, 0, MakeWfcMidiTimeSignatureEvent(0, 4, 2, 24, 8));
  AddEvent(LFile, 0, MakeWfcMidiTempoEvent(0, 500000));
  AddEvent(LFile, 0, MakeWfcMidiTimeSignatureEvent(480, 3, 2, 24, 8));
  AddEvent(LFile, 0, MakeWfcMidiTempoEvent(0, 700000));
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    Check((LScore.LengthTicks = 840) and (LScore.MeterCount = 2) and
      (LScore.MeterAt(1).Tick = 480) and
      (LScore.MeterAt(1).Numerator = 3),
      'meter change starts on the preceding complete measure');
    Check((not LReport.UsedDefaultMeter) and
      (not LReport.UsedDefaultTempo) and (LReport.PaddingTicks = 0),
      'explicit timing and new-meter exact end need no defaults or padding');
    Check((LScore.TempoCount = 2) and
      (LScore.TempoAt(1).Tick = 480),
      'tempo and meter may change at the same tick');
  finally
    LScore.Free;
  end;

  LFile := EmptyFile(0, 1, 120, 480);
  AddEvent(LFile, 0, MakeWfcMidiTimeSignatureEvent(120, 3, 2, 24, 8));
  ExpectDefaultRejected(LFile, 'off-bar meter change');
  LFile := EmptyFile(0, 1, 1, 4);
  AddEvent(LFile, 0, MakeWfcMidiTimeSignatureEvent(0, 1, 3, 24, 8));
  ExpectDefaultRejected(LFile, 'nonintegral meter measure length');
  LFile := EmptyFile(0, 1, 120, 480);
  AddEvent(LFile, 0, MakeWfcMidiTimeSignatureEvent(0, 0, 2, 24, 8));
  ExpectDefaultRejected(LFile, 'zero meter numerator');
  LFile.Tracks[0].Events[0] :=
    MakeWfcMidiTimeSignatureEvent(0, 4, 31, 24, 8);
  ExpectDefaultRejected(LFile, 'overflowing denominator exponent');
  LFile.Tracks[0].Events[0] :=
    MakeWfcMidiTimeSignatureEvent(0, 4, 2, 24, 16);
  ExpectDefaultRejected(LFile, 'unsupported notated-quarter meaning');
end;

procedure TestTimingCanonicalization;
var
  LFile: TWfcMidiFile;
  LReport: TWfcMusicMidiImportReport;
  LScore: TWfcMusicScore;
begin
  LFile := EmptyFile(1, 2, 120, 480);
  AddEvent(LFile, 0, MakeWfcMidiTempoEvent(0, 600000));
  AddEvent(LFile, 1, MakeWfcMidiTempoEvent(0, 600000));
  AddEvent(LFile, 1, MakeWfcMidiTimeSignatureEvent(0, 4, 2, 24, 8));
  AddEvent(LFile, 1, MakeWfcMidiTimeSignatureEvent(60, 4, 2, 24, 8));
  LFile.Tracks[1].EndDeltaTicks := 420;
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    Check((LScore.TempoCount = 1) and (LScore.MeterCount = 1) and
      (LReport.RedundantTimingEvents = 2),
      'duplicate tempo and off-bar unchanged meter coalesce');
  finally
    LScore.Free;
  end;
  LFile.Tracks[1].Events[0] := MakeWfcMidiTempoEvent(0, 700000);
  ExpectDefaultRejected(LFile, 'conflicting simultaneous tempo across tracks');

  LFile := EmptyFile(1, 2, 120, 480);
  AddEvent(LFile, 0, MakeWfcMidiTimeSignatureEvent(0, 4, 2, 24, 8));
  AddEvent(LFile, 1, MakeWfcMidiTimeSignatureEvent(0, 3, 2, 24, 8));
  ExpectDefaultRejected(LFile, 'conflicting simultaneous meter across tracks');

  LFile := EmptyFile(0, 1, 120, 0);
  AddEvent(LFile, 0, MakeWfcMidiTempoEvent(480, 700000));
  AddEvent(LFile, 0, MakeWfcMidiTimeSignatureEvent(0, 3, 2, 24, 8));
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    Check((LScore.LengthTicks = 480) and (LScore.TempoCount = 1) and
      (LScore.MeterCount = 1) and (LReport.TerminalTimingEvents = 2) and
      (LReport.PaddingTicks = 0),
      'terminal timing is reported without inventing another measure');
  finally
    LScore.Free;
  end;
  AddEvent(LFile, 0, MakeWfcMidiTempoEvent(0, 800000));
  ExpectDefaultRejected(LFile, 'conflicting terminal tempo is still ambiguous');

  LFile := EmptyFile(0, 1, 120, 480);
  AddEvent(LFile, 0, MakeWfcMidiTimeSignatureEvent(0, 4, 2, 36, 8));
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    Check((LScore.MeterAt(0).Numerator = 4) and
      (LReport.IgnoredMetaEvents = 1),
      'nonstandard metronome click is omitted with a report');
  finally
    LScore.Free;
  end;
end;

procedure TestSilentInputs;
var
  LFile: TWfcMidiFile;
  LReport: TWfcMusicMidiImportReport;
  LScore: TWfcMusicScore;
begin
  LFile := EmptyFile(1, 2, 120, 0);
  LFile.Tracks[1].EndDeltaTicks := 480;
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    Check((LReport.NoteCount = 0) and LReport.AddedSilentVoice and
      (LScore.VoiceCount = 1) and (LScore.SpanCount = 1),
      'positive silent file gets one explicit rest-only voice');
    CheckRest(LScore, 0, 0, 0, 480, 'silent source extent is preserved');
    Check(LReport.SourceEventCount = 2,
      'empty tracks still contribute their end-of-track events');
  finally
    LScore.Free;
  end;
  LFile := EmptyFile(0, 1, 120, 0);
  ExpectDefaultRejected(LFile, 'zero-length source');
end;

procedure TestUnsupportedPolicies;
var
  LFile: TWfcMidiFile;
  LOptions: TWfcMusicMidiImportOptions;
  LReport: TWfcMusicMidiImportReport;
  LScore: TWfcMusicScore;
  I: Integer;
  LMeta: Byte;
begin
  LOptions := DefaultWfcMusicMidiImportOptions;
  for I := 0 to 4 do
  begin
    LFile := SingleNote;
    case I of
      0: AddEvent(LFile, 0, MakeWfcMidiChannelEvent(0, $A0, [60, 40]));
      1: AddEvent(LFile, 0, MakeWfcMidiChannelEvent(0, $B0, [64, 127]));
      2: AddEvent(LFile, 0, MakeWfcMidiChannelEvent(0, $C0, [5]));
      3: AddEvent(LFile, 0, MakeWfcMidiChannelEvent(0, $D0, [80]));
      4: AddEvent(LFile, 0, MakeWfcMidiChannelEvent(0, $E0, [0, 64]));
    end;
    ExpectRejected(LFile, LOptions, 'unsupported channel family ' + IntToStr(I));
    LOptions.UnsupportedEvents := wmmupIgnoreAndReport;
    LScore := ImportWfcMusicMidi(LFile, LOptions, LReport);
    try
      Check((LReport.IgnoredChannelEvents = 1) and
        (LReport.NoteCount = 1), 'explicit channel omission is counted');
    finally
      LScore.Free;
    end;
    LOptions.UnsupportedEvents := wmmupReject;
  end;

  for I := 0 to 1 do
  begin
    LFile := SingleNote;
    if I = 0 then LMeta := $F0 else LMeta := $F7;
    AddEvent(LFile, 0, MakeWfcMidiSystemExclusiveEvent(0, LMeta, [$7D, $F7]));
    ExpectRejected(LFile, LOptions, 'opaque system-exclusive record ' + IntToStr(I));
    LOptions.UnsupportedEvents := wmmupIgnoreAndReport;
    LScore := ImportWfcMusicMidi(LFile, LOptions, LReport);
    try
      Check(LReport.IgnoredSystemEvents = 1, 'explicit system omission is counted');
    finally
      LScore.Free;
    end;
    LOptions.UnsupportedEvents := wmmupReject;
  end;

  for I := 0 to 1 do
  begin
    LFile := SingleNote;
    if I = 0 then LMeta := $7E else LMeta := $7F;
    AddEvent(LFile, 0, MakeWfcMidiMetaEvent(0, LMeta, [1, 2]));
    ExpectRejected(LFile, LOptions, 'opaque meta record ' + IntToStr(I));
    LOptions.UnsupportedEvents := wmmupIgnoreAndReport;
    LScore := ImportWfcMusicMidi(LFile, LOptions, LReport);
    try
      Check(LReport.IgnoredMetaEvents = 1, 'explicit opaque meta omission is counted');
    finally
      LScore.Free;
    end;
    LOptions.UnsupportedEvents := wmmupReject;
  end;

  LOptions.UnsupportedEvents := wmmupIgnoreAndReport;
  for I := 0 to 3 do
  begin
    LFile := SingleNote;
    case I of
      0: AddEvent(LFile, 0, MakeWfcMidiMetaEvent(0, $20, [0]));
      1: AddEvent(LFile, 0, MakeWfcMidiMetaEvent(0, $21, [0]));
      2: AddEvent(LFile, 0, MakeWfcMidiMetaEvent(0, $09, [65]));
      3: AddEvent(LFile, 0, MakeWfcMidiMetaEvent(0, $54, [0, 0, 0, 0, 0]));
    end;
    ExpectRejected(LFile, LOptions,
      'routing or absolute-time metadata cannot be ignored ' + IntToStr(I));
  end;
end;

procedure TestHarmlessOmissions;
var
  LFile: TWfcMidiFile;
  LReport: TWfcMusicMidiImportReport;
  LScore: TWfcMusicScore;
begin
  LFile := SingleNote;
  AddEvent(LFile, 0, MakeWfcMidiMetaEvent(0, $01, [65, 66, 67]));
  AddEvent(LFile, 0, MakeWfcMidiMetaEvent(0, $03, [$FF, $FE, 65]));
  LFile.Tracks[0].Events[1].Data[1] := 99;
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    Check((LReport.OmittedTrackNames = 1) and
      (LReport.DiscardedReleaseVelocities = 1),
      'uninterpreted track name and release velocity are reported');
    Check(LReport.IgnoredMetaEvents = 2,
      'text and omitted track-name events are both counted');
    CheckNote(LScore, 0, 0, 0, 120, 60, 100,
      'omitted release velocity cannot change attack velocity');
    Check(Pos('65', EncodeWfcMusicText(LScore)) = 0,
      'raw non-UTF8 names are not guessed into score labels');
  finally
    LScore.Free;
  end;
end;

procedure TestTypedInputValidation;
var
  LFile: TWfcMidiFile;
begin
  LFile := SingleNote;
  LFile.Format := 2;
  ExpectDefaultRejected(LFile, 'unsupported format two');
  LFile := EmptyFile(0, 2, 120, 480);
  ExpectDefaultRejected(LFile, 'format zero with multiple tracks');
  LFile := EmptyFile(1, 0, 120, 480);
  ExpectDefaultRejected(LFile, 'format one without tracks');
  LFile := SingleNote;
  LFile.TicksPerQuarter := 0;
  ExpectDefaultRejected(LFile, 'zero PPQ');
  LFile.TicksPerQuarter := $8001;
  ExpectDefaultRejected(LFile, 'SMPTE division');
  LFile := SingleNote;
  LFile.Tracks[0].Events[0].DeltaTicks := $10000000;
  ExpectDefaultRejected(LFile, 'event delta beyond VLQ range');
  LFile := SingleNote;
  LFile.Tracks[0].EndDeltaTicks := $10000000;
  ExpectDefaultRejected(LFile, 'end delta beyond VLQ range');
  LFile := SingleNote;
  SetLength(LFile.Tracks[0].Events[0].Data, 1);
  ExpectDefaultRejected(LFile, 'short typed NoteOn payload');
  LFile := SingleNote;
  LFile.Tracks[0].Events[0].Data[0] := $80;
  ExpectDefaultRejected(LFile, 'eight-bit channel data');
  LFile := SingleNote;
  LFile.Tracks[0].Events[0].MetaType := 1;
  ExpectDefaultRejected(LFile, 'channel event with a meta type');
  LFile := SingleNote;
  LFile.Tracks[0].Events[0].Status := $F1;
  ExpectDefaultRejected(LFile, 'unsupported raw status');
  LFile := SingleNote;
  LFile.Tracks[0].Events[0].Status := $FF;
  LFile.Tracks[0].Events[0].MetaType := $2F;
  LFile.Tracks[0].Events[0].Data := nil;
  ExpectDefaultRejected(LFile, 'typed embedded end-of-track');
  LFile := SingleNote;
  LFile.Tracks[0].Events[0] := MakeWfcMidiMetaEvent(0, $01, [0]);
  LFile.Tracks[0].Events[0].MetaType := $80;
  ExpectDefaultRejected(LFile, 'eight-bit meta type');
  LFile := EmptyFile(0, 1, 120, 480);
  AddEvent(LFile, 0, MakeWfcMidiTempoEvent(0, 1));
  LFile.Tracks[0].Events[0].Data := nil;
  ExpectDefaultRejected(LFile, 'short typed tempo');
  LFile.Tracks[0].Events[0].Data := nil;
  SetLength(LFile.Tracks[0].Events[0].Data, 3);
  ExpectDefaultRejected(LFile, 'zero typed tempo');
  LFile := EmptyFile(0, 1, 120, 480);
  AddEvent(LFile, 0, MakeWfcMidiTimeSignatureEvent(0, 4, 2, 24, 8));
  SetLength(LFile.Tracks[0].Events[0].Data, 3);
  ExpectDefaultRejected(LFile, 'short typed meter');
end;

procedure TestResourceBoundaries;
var
  I: Integer;
  LCount: Integer;
  LFile: TWfcMidiFile;
  LReport: TWfcMusicMidiImportReport;
  LScore: TWfcMusicScore;
begin
  LFile := EmptyFile(1, WFC_MUSIC_MIDI_IMPORT_MAX_TRACKS, 1, 4);
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    Check((LReport.SourceTrackCount = 256) and
      (LReport.SourceEventCount = 256),
      'maximum track count and structural EOT accounting are accepted');
  finally
    LScore.Free;
  end;
  SetLength(LFile.Tracks, WFC_MUSIC_MIDI_IMPORT_MAX_TRACKS + 1);
  ExpectDefaultRejected(LFile, 'track count above the resource cap');

  LFile := EmptyFile(0, 1, 120, 240);
  for I := 0 to WFC_MUSIC_MIDI_IMPORT_MAX_VOICES - 1 do
    AddOn(LFile, 0, 0, Byte(I div 128), Byte(I mod 128), 80);
  for I := 0 to WFC_MUSIC_MIDI_IMPORT_MAX_VOICES - 1 do
    if I = 0 then
      AddOff(LFile, 0, 240, Byte(I div 128), Byte(I mod 128), 0)
    else
      AddOff(LFile, 0, 0, Byte(I div 128), Byte(I mod 128), 0);
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    Check((LScore.VoiceCount = WFC_MUSIC_MIDI_IMPORT_MAX_VOICES) and
      (LReport.NoteCount = 256) and (LScore.SpanCount = 512),
      'maximum lane count accepts distinct simultaneous channel-pitch keys');
  finally
    LScore.Free;
  end;
  LFile := EmptyFile(0, 1, 120, 240);
  for I := 0 to WFC_MUSIC_MIDI_IMPORT_MAX_VOICES do
    AddOn(LFile, 0, 0, Byte(I div 128), Byte(I mod 128), 80);
  for I := 0 to WFC_MUSIC_MIDI_IMPORT_MAX_VOICES do
    if I = 0 then
      AddOff(LFile, 0, 240, Byte(I div 128), Byte(I mod 128), 0)
    else
      AddOff(LFile, 0, 0, Byte(I div 128), Byte(I mod 128), 0);
  ExpectDefaultRejected(LFile, 'lane count above the resource cap');

  LFile := EmptyFile(0, 1, 1, 4);
  LCount := WFC_MUSIC_MIDI_IMPORT_MAX_EVENTS - 1;
  SetLength(LFile.Tracks[0].Events, LCount);
  for I := 0 to LCount - 1 do
    LFile.Tracks[0].Events[I] := MakeWfcMidiMetaEvent(0, $01, []);
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    Check((LReport.SourceEventCount = WFC_MUSIC_MIDI_IMPORT_MAX_EVENTS) and
      (LReport.IgnoredMetaEvents = LCount),
      'maximum event count includes the final structural event');
  finally
    LScore.Free;
  end;
  SetLength(LFile.Tracks[0].Events, LCount + 1);
  LFile.Tracks[0].Events[LCount] := MakeWfcMidiMetaEvent(0, $01, []);
  ExpectDefaultRejected(LFile, 'event count above the EOT-inclusive cap');

  LFile := EmptyFile(0, 1, 1, 1);
  SetLength(LFile.Tracks[0].Events, WFC_MUSIC_MIDI_IMPORT_MAX_TIMING_EVENTS);
  for I := 0 to Length(LFile.Tracks[0].Events) - 1 do
    if I = 0 then
      LFile.Tracks[0].Events[I] := MakeWfcMidiTempoEvent(0, 500000)
    else
      LFile.Tracks[0].Events[I] := MakeWfcMidiTempoEvent(1, 500000 + (I mod 2));
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    Check(LScore.TempoCount = WFC_MUSIC_MIDI_IMPORT_MAX_TIMING_EVENTS,
      'maximum explicit canonical tempo anchors are accepted');
  finally
    LScore.Free;
  end;
  LFile.Tracks[0].Events[0] := MakeWfcMidiTempoEvent(1, 600000);
  ExpectDefaultRejected(LFile, 'default tempo cannot exceed canonical anchor cap');
  LFile := EmptyFile(0, 1, 120, 480);
  SetLength(LFile.Tracks[0].Events, WFC_MUSIC_MIDI_IMPORT_MAX_TIMING_EVENTS + 1);
  for I := 0 to Length(LFile.Tracks[0].Events) - 1 do
    LFile.Tracks[0].Events[I] := MakeWfcMidiTempoEvent(0, 500000);
  ExpectDefaultRejected(LFile, 'redundant raw tempo events still consume input cap');
  for I := 0 to Length(LFile.Tracks[0].Events) - 1 do
    LFile.Tracks[0].Events[I] :=
      MakeWfcMidiTimeSignatureEvent(0, 4, 2, 24, 8);
  ExpectDefaultRejected(LFile, 'redundant raw meter events still consume input cap');

  LFile := EmptyFile(0, 1, 1, 4);
  for I := 0 to 7 do
    AddEvent(LFile, 0,
      MakeWfcMidiMetaEvent(WFC_MIDI_MAX_VARIABLE_LENGTH, $01, []));
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    Check(LScore.LengthTicks = High(Integer) - 3,
      'largest complete four-tick measure below Integer limit stays exact');
  finally
    LScore.Free;
  end;
  LFile.Tracks[0].EndDeltaTicks := 7;
  ExpectDefaultRejected(LFile, 'padding beyond Integer range');
  LFile.Tracks[0].EndDeltaTicks := 8;
  ExpectDefaultRejected(LFile, 'accumulated source ticks beyond Integer range');

  LFile := EmptyFile(0, 1, 1, 4);
  AddEvent(LFile, 0, MakeWfcMidiMetaEvent(0, $01, []));
  SetLength(LFile.Tracks[0].Events[0].Data,
    WFC_MUSIC_MIDI_IMPORT_MAX_BYTES);
  ExpectDefaultRejected(LFile, 'canonical byte envelope above the input cap');
end;

procedure TestRawByteFailures;
var
  I: Integer;
  LBytes: TWfcMidiBytes;
  LReport: TWfcMusicMidiImportReport;
  LScore: TWfcMusicScore;
  LRaised: Boolean;
begin
  for I := 0 to 3 do
  begin
    LBytes := EncodeWfcMidiFile(SingleNote);
    case I of
      0: LBytes := nil;
      1: SetLength(LBytes, Length(LBytes) - 1);
      2: LBytes[0] := 0;
      3: SetLength(LBytes, WFC_MUSIC_MIDI_IMPORT_MAX_BYTES + 1);
    end;
    SeedReport(LReport);
    LScore := nil;
    LRaised := False;
    try
      LScore := DecodeWfcMusicMidi(LBytes,
        DefaultWfcMusicMidiImportOptions, LReport);
    except
      on E: EWfcMusicMidiImport do LRaised := True;
    end;
    LScore.Free;
    Check(LRaised, 'raw-byte failure is wrapped at importer boundary ' +
      IntToStr(I));
    Check(ReportIsEmpty(LReport),
      'raw-byte rejection resets every report field ' + IntToStr(I));
  end;
end;

procedure TestPortableRoundTrips;
const
  RUNNING_NOTE: array[0..32] of Byte =
    ($4D,$54,$68,$64,0,0,0,6,0,0,0,1,0,$60,
     $4D,$54,$72,$6B,0,0,0,$0B,
     0,$90,$3C,$40,$60,$3C,0,0,$FF,$2F,0);
var
  I: Integer;
  LFile: TWfcMidiFile;
  LBytes: TWfcMidiBytes;
  LReport, LSecondReport: TWfcMusicMidiImportReport;
  LScore, LSecond: TWfcMusicScore;
  LText: String;
begin
  LFile := PolyphonicFile;
  LScore := ImportWfcMusicMidi(LFile,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    LText := EncodeWfcMusicText(LScore);
    LBytes := EncodeWfcMidiFile(LFile);
    LSecond := DecodeWfcMusicMidi(LBytes,
      DefaultWfcMusicMidiImportOptions, LSecondReport);
    try
      Check(LText = EncodeWfcMusicText(LSecond),
        'typed and encoded source import produce identical exact score text');
      Check((LReport.SourceEventCount = LSecondReport.SourceEventCount) and
        (LReport.NoteCount = LSecondReport.NoteCount),
        'typed and raw-byte source accounting agrees');
    finally
      LSecond.Free;
    end;
    LFile.Tracks[0].Events[0].Data[0] := 72;
    LReport.Voices[0].SourceTrack := 99;
    Check(LText = EncodeWfcMusicText(LScore),
      'imported score owns its data independently of source and report');
    LSecond := DecodeWfcMusicText(LText);
    try
      Check(LText = EncodeWfcMusicText(LSecond),
        'exact imported score survives the canonical text codec');
    finally
      LSecond.Free;
    end;
    LSecond := DecodeWfcMusicMidi(EncodeWfcMusicMidi(LScore),
      DefaultWfcMusicMidiImportOptions, LSecondReport);
    try
      Check((LSecond.VoiceCount = LScore.VoiceCount) and
        (LSecond.SpanCount = LScore.SpanCount) and
        (LSecond.LengthTicks = LScore.LengthTicks),
        'score-to-MIDI-to-score preserves lane intervals in supported export range');
      CheckNote(LSecond, 0, 0, 0, 360, 60, 100,
        'round-trip sustained interval stays unsplit');
    finally
      LSecond.Free;
    end;
    LSecond := ImportWfcMusicMidi(PolyphonicFile,
      DefaultWfcMusicMidiImportOptions, LSecondReport);
    try
      Check(LText = EncodeWfcMusicText(LSecond),
        'repeated independent imports are canonical and deterministic');
    finally
      LSecond.Free;
    end;
  finally
    LScore.Free;
  end;
  SetLength(LBytes, Length(RUNNING_NOTE));
  for I := 0 to High(RUNNING_NOTE) do LBytes[I] := RUNNING_NOTE[I];
  LScore := DecodeWfcMusicMidi(LBytes,
    DefaultWfcMusicMidiImportOptions, LReport);
  try
    CheckNote(LScore, 0, 0, 0, 96, 60, 64,
      'running status and velocity-zero release pass through owned codec');
    Check((LReport.SourceLengthTicks = 96) and
      (LReport.ScoreLengthTicks = 384) and (LReport.PaddingTicks = 288),
      'raw running-status fixture exposes exact rest padding');
  finally
    LScore.Free;
  end;
end;

begin
  WriteLn('WFC music MIDI semantic importer conformance suite');
  WriteLn('=================================================');
  RunTest('defaults and exact import', @TestDefaultsAndExactImport);
  RunTest('attack-preserving lane allocation', @TestAttackPreservingLanes);
  RunTest('merged-track note ownership', @TestMergedTrackOwnership);
  RunTest('unambiguous note-pair requirements', @TestPairingRejections);
  RunTest('exact timing and explicit padding', @TestTimingAndPadding);
  RunTest('timing canonicalization', @TestTimingCanonicalization);
  RunTest('explicit silent score handling', @TestSilentInputs);
  RunTest('strict and explicit omission policies', @TestUnsupportedPolicies);
  RunTest('harmless metadata and release omission', @TestHarmlessOmissions);
  RunTest('typed input validation', @TestTypedInputValidation);
  RunTest('bounded allocation and exact overflow guards', @TestResourceBoundaries);
  RunTest('raw-byte failure boundaries', @TestRawByteFailures);
  RunTest('portable codec and ownership round trips', @TestPortableRoundTrips);
  WriteLn('=================================================');
  WriteLn(Format('%d checks, %d failures', [GCheckCount, GFailureCount]));
  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d music MIDI import checks failed', [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
