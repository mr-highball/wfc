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
program wfc_music_training_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}
  wfc_browser_test_host,
  {$ENDIF}
  SysUtils,
  wfc_model,
  wfc_music,
  wfc_music_sequence,
  wfc_music_training,
  wfc_sequence,
  wfc_sequence_text,
  wfc_training;

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

procedure BeginTest(const AName: String);
begin
  WriteLn('[TEST] ', AName);
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

function SelectionsOf(const AValues: array of TWfcMusicTrainingSelection):
  TWfcMusicTrainingSelections;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function BasicMetadata: TWfcTrainingMetadata;
begin
  Result := MakeWfcTrainingMetadata('phrases', 'MIT',
    'project-authored score phrases');
end;

function NewSingleVoiceScore(const ALengthTicks: Integer;
  const ASpans: TWfcMusicSpanEvents;
  const ATicksPerQuarter: Integer = 240): TWfcMusicScore;
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
  LMeters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, 500000);
  Result := TWfcMusicScore.Create(ATicksPerQuarter, 12, ALengthTicks,
    LTracks, LVoices, LMeters, LTempos, ASpans);
end;

function BasicScore: TWfcMusicScore;
begin
  Result := NewSingleVoiceScore(1920, SpansOf([
    MakeWfcMusicRest(0, 0, 240),
    MakeWfcMusicSound(0, 240, 480,
      TonesOf([MakeWfcMusicTone(60, 100)])),
    MakeWfcMusicSound(0, 720, 240,
      TonesOf([MakeWfcMusicTone(60, 100)])),
    MakeWfcMusicRest(0, 960, 960)]));
end;

function TwoVoicePhraseScore: TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetLength(LTracks, 1);
  LTracks[0] := MakeWfcMusicTrack('ensemble', 'Ensemble');
  SetLength(LVoices, 2);
  LVoices[0] := MakeWfcMusicVoice(0, 'upper');
  LVoices[1] := MakeWfcMusicVoice(0, 'lower');
  SetLength(LMeters, 1);
  LMeters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, 500000);
  Result := TWfcMusicScore.Create(240, 12, 1920, LTracks, LVoices,
    LMeters, LTempos, SpansOf([
      MakeWfcMusicSound(0, 0, 240,
        TonesOf([MakeWfcMusicTone(72, 100)])),
      MakeWfcMusicRest(0, 240, 1680),
      MakeWfcMusicSound(1, 0, 240,
        TonesOf([MakeWfcMusicTone(48, 90)])),
      MakeWfcMusicRest(1, 240, 1680)]));
end;

function BuildRejected(const AScore: TWfcMusicScore;
  const ASelections: TWfcMusicTrainingSelections;
  const AProjection: TWfcMusicTrainingProjection;
  const AQuantumTicks, AOrder: Integer;
  const AExpected: String): Boolean;
var
  LDocument: TWfcTrainingDocument;
begin
  Result := False;
  LDocument := nil;
  try
    try
      LDocument := BuildWfcMusicTrainingDocument(AScore, ASelections,
        AProjection, AQuantumTicks, AOrder, BasicMetadata);
    except
      on E: EWfcMusicTraining do
        Result := (AExpected = '') or (Pos(AExpected, E.Message) > 0);
    end;
  finally
    LDocument.Free;
  end;
end;

{$PUSH}{$R-}
function InvalidProjection: TWfcMusicTrainingProjection;
var
  LValue: Integer;
begin
  LValue := Ord(High(TWfcMusicTrainingProjection)) + 1;
  Result := TWfcMusicTrainingProjection(LValue);
end;
{$POP}

procedure TestProjectionDocuments;
var
  LDocument: TWfcTrainingDocument;
  LMetadata: TWfcTrainingMetadata;
  LOptions: TWfcTrainingOptions;
  LSample: TWfcTrainingSample;
  LSelections: TWfcMusicTrainingSelections;
  LScore: TWfcMusicScore;
begin
  BeginTest('exact melody, rhythm and harmony projections');
  LScore := BasicScore;
  try
    LSelections := SelectionsOf([
      MakeWfcMusicTrainingSelection('whole', 0, 0, 1920)]);

    LDocument := BuildWfcMusicTrainingDocument(LScore, LSelections,
      wmtpMelody, 240, 2, BasicMetadata);
    try
      LOptions := LDocument.CopyOptions;
      Check((LOptions.Kind = wtkSequence) and
        (LOptions.Boundary = wmbOpen) and
        (LOptions.Symmetry = wmsNone) and
        (LOptions.PatternWidth = 0) and
        (LOptions.PatternHeight = 0) and (LOptions.Order = 2),
        'bridge emits the exact open sequence-training profile');
      LMetadata := LDocument.CopyMetadata;
      Check((LMetadata.Name = 'phrases') and
        (LMetadata.LicenseIdentifier = 'MIT') and
        (LMetadata.SourceDescription = 'project-authored score phrases'),
        'caller metadata is preserved without inference');
      LSample := LDocument.SampleAt(0);
      Check((LSample.Name = 'whole') and (LSample.Width = 8) and
        (LSample.Height = 1) and (Length(LSample.Tokens) = 8),
        'selection becomes one ordered height-one sample');
      Check((LSample.Tokens[0] = 'wm1:r') and
        (LSample.Tokens[1] = 'wm1:a:60:100') and
        (LSample.Tokens[2] = 'wm1:h:60:100') and
        (LSample.Tokens[3] = 'wm1:a:60:100') and
        (LSample.Tokens[4] = 'wm1:r'),
        'melody preserves rests, holds, and adjacent-note reattacks');
    finally
      LDocument.Free;
    end;

    LDocument := BuildWfcMusicTrainingDocument(LScore, LSelections,
      wmtpRhythm, 240, 2, BasicMetadata);
    try
      LSample := LDocument.SampleAt(0);
      Check((LSample.Tokens[0] = 'wr1:r') and
        (LSample.Tokens[1] = 'wr1:a') and
        (LSample.Tokens[2] = 'wr1:h') and
        (LSample.Tokens[3] = 'wr1:a'),
        'rhythm projection retains articulation and reattacks');
    finally
      LDocument.Free;
    end;

    LDocument := BuildWfcMusicTrainingDocument(LScore, LSelections,
      wmtpHarmony, 240, 2, BasicMetadata);
    try
      LSample := LDocument.SampleAt(0);
      Check((LSample.Tokens[0] = 'wh1:r:12:0') and
        (LSample.Tokens[1] = 'wh1:p:12:0') and
        (LSample.Tokens[2] = 'wh1:p:12:0') and
        (LSample.Tokens[3] = 'wh1:p:12:0'),
        'harmony projection uses the score pitch system exactly');
    finally
      LDocument.Free;
    end;
  finally
    LScore.Free;
  end;
end;

procedure TestDetachment;
var
  LDocument: TWfcTrainingDocument;
  LMetadata: TWfcTrainingMetadata;
  LSample: TWfcTrainingSample;
  LSelections: TWfcMusicTrainingSelections;
  LScore: TWfcMusicScore;
begin
  BeginTest('input and accessors remain deeply detached');
  LScore := BasicScore;
  try
    LMetadata := BasicMetadata;
    LSelections := SelectionsOf([
      MakeWfcMusicTrainingSelection('original', 0, 0, 960)]);
    LDocument := BuildWfcMusicTrainingDocument(LScore, LSelections,
      wmtpMelody, 240, 1, LMetadata);
    try
      LMetadata.Name := 'changed';
      LSelections[0].Name := 'changed';
      LSelections[0].StartTick := 960;
      LSample := LDocument.SampleAt(0);
      LSample.Name := 'changed-again';
      LSample.Tokens[0] := 'changed-token';
      LSample := LDocument.SampleAt(0);
      Check((LDocument.CopyMetadata.Name = 'phrases') and
        (LSample.Name = 'original') and (LSample.Tokens[0] = 'wm1:r'),
        'document detaches caller records and accessor token arrays');
    finally
      LDocument.Free;
    end;
  finally
    LScore.Free;
  end;
end;

procedure TestBoundariesAndCuts;
var
  LDocument: TWfcTrainingDocument;
  LSample: TWfcTrainingSample;
  LScore: TWfcMusicScore;
  LSelections: TWfcMusicTrainingSelections;
begin
  BeginTest('selection boundaries, cropped rests and sounded-span cuts');
  { The selected range is wholly inside a rest. The unaligned chord outside
    it must not be inspected as selected musical content. }
  LScore := NewSingleVoiceScore(1920, SpansOf([
    MakeWfcMusicRest(0, 0, 1000),
    MakeWfcMusicSound(0, 1000, 200,
      TonesOf([MakeWfcMusicTone(60, 90),
        MakeWfcMusicTone(64, 90)])),
    MakeWfcMusicRest(0, 1200, 720)]));
  try
    LSelections := SelectionsOf([
      MakeWfcMusicTrainingSelection('cropped-rest', 0, 240, 480)]);
    LDocument := BuildWfcMusicTrainingDocument(LScore, LSelections,
      wmtpMelody, 240, 1, BasicMetadata);
    try
      LSample := LDocument.SampleAt(0);
      Check((Length(LSample.Tokens) = 2) and
        (LSample.Tokens[0] = 'wm1:r') and
        (LSample.Tokens[1] = 'wm1:r'),
        'rests may be cropped at both sample edges');
    finally
      LDocument.Free;
    end;
  finally
    LScore.Free;
  end;

  LScore := NewSingleVoiceScore(1920, SpansOf([
    MakeWfcMusicSound(0, 0, 480,
      TonesOf([MakeWfcMusicTone(62, 88)])),
    MakeWfcMusicRest(0, 480, 1440)]));
  try
    Check(BuildRejected(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('cut-start', 0, 240, 240)]),
      wmtpMelody, 240, 1, 'cuts a sounded span'),
      'a selection cannot begin inside a held note');
    Check(BuildRejected(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('cut-end', 0, 0, 240)]),
      wmtpMelody, 240, 1, 'cuts a sounded span'),
      'a selection cannot end inside a held note');
  finally
    LScore.Free;
  end;

  LScore := NewSingleVoiceScore(1920, SpansOf([
    MakeWfcMusicRest(0, 0, 300),
    MakeWfcMusicSound(0, 300, 240,
      TonesOf([MakeWfcMusicTone(65, 90)])),
    MakeWfcMusicRest(0, 540, 1380)]));
  try
    Check(BuildRejected(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('unaligned-rest', 0, 0, 480)]),
      wmtpMelody, 240, 1, 'rest end'),
      'an internal cropped-rest boundary must align to the quantum');
  finally
    LScore.Free;
  end;
end;

procedure TestChordRejection;
var
  LProjection: TWfcMusicTrainingProjection;
  LScore: TWfcMusicScore;
  LSelections: TWfcMusicTrainingSelections;
begin
  BeginTest('chords are rejected by every monophonic projection');
  LScore := NewSingleVoiceScore(1920, SpansOf([
    MakeWfcMusicSound(0, 0, 240,
      TonesOf([MakeWfcMusicTone(60, 90),
        MakeWfcMusicTone(64, 80)])),
    MakeWfcMusicRest(0, 240, 1680)]));
  try
    LSelections := SelectionsOf([
      MakeWfcMusicTrainingSelection('chord', 0, 0, 240)]);
    for LProjection := Low(TWfcMusicTrainingProjection) to
        High(TWfcMusicTrainingProjection) do
      Check(BuildRejected(LScore, LSelections, LProjection, 240, 1,
        'chord'), 'selected chord rejection is projection-independent');
  finally
    LScore.Free;
  end;
end;

procedure TestAlignmentAndArguments;
var
  LScore: TWfcMusicScore;
  LSelections: TWfcMusicTrainingSelections;
begin
  BeginTest('argument, range and alignment validation');
  LScore := BasicScore;
  try
    LSelections := SelectionsOf([
      MakeWfcMusicTrainingSelection('valid', 0, 0, 240)]);
    Check(BuildRejected(nil, LSelections, wmtpMelody, 240, 1,
      'not assigned'), 'nil scores use the bridge exception type');
    Check(BuildRejected(LScore, LSelections, InvalidProjection, 240, 1,
      'unknown'), 'invalid projection enum bits are rejected');
    Check(BuildRejected(LScore, LSelections, wmtpMelody, 0, 1,
      'quantum'), 'nonpositive quantum is rejected');
    Check(BuildRejected(LScore, LSelections, wmtpMelody, 240, 0,
      'order'), 'zero order is rejected');
    Check(BuildRejected(LScore, LSelections, wmtpMelody, 240,
      WFC_TRAINING_MAX_ORDER + 1, 'order'),
      'order above the training limit is rejected');
    Check(BuildRejected(LScore, nil, wmtpMelody, 240, 1,
      'selection count'), 'an empty selection corpus is rejected');
    Check(BuildRejected(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('voice', 1, 0, 240)]),
      wmtpMelody, 240, 1, 'voice index'),
      'voice indices are checked by the bridge');
    Check(BuildRejected(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('negative', 0, -1, 240)]),
      wmtpMelody, 240, 1, 'negative'),
      'negative selection starts are rejected');
    Check(BuildRejected(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('empty', 0, 0, 0)]),
      wmtpMelody, 240, 1, 'positive'),
      'zero selection lengths are rejected');
    Check(BuildRejected(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('overflow', 0,
        High(Integer) - 1, 10)]), wmtpMelody, 240, 1, 'Integer range'),
      'selection end arithmetic is overflow-safe');
    Check(BuildRejected(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('outside', 0, 1920, 240)]),
      wmtpMelody, 240, 1, 'beyond'),
      'selection ranges must stay within the score');
    Check(BuildRejected(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('start-align', 0, 1, 240)]),
      wmtpMelody, 240, 1, 'aligned'),
      'selection starts must align to the quantum');
    Check(BuildRejected(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('length-align', 0, 0, 241)]),
      wmtpMelody, 240, 1, 'aligned'),
      'selection lengths must align to the quantum');
    Check(BuildRejected(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('short', 0, 0, 240)]),
      wmtpMelody, 240, 2, 'fewer cells'),
      'each sample must contain at least its sequence order');
    Check(BuildRejected(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('', 0, 0, 240)]),
      wmtpMelody, 240, 1, 'cannot be empty'),
      'sample names must be explicit');
    Check(BuildRejected(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('same', 0, 0, 240),
      MakeWfcMusicTrainingSelection('same', 0, 240, 240)]),
      wmtpMelody, 240, 1, 'unique'),
      'sample names must be unique');
  finally
    LScore.Free;
  end;
end;

procedure TestSoundAlignment;
var
  LScore: TWfcMusicScore;
begin
  BeginTest('intersecting sounded spans require exact alignment');
  LScore := NewSingleVoiceScore(1920, SpansOf([
    MakeWfcMusicRest(0, 0, 120),
    MakeWfcMusicSound(0, 120, 240,
      TonesOf([MakeWfcMusicTone(60, 100)])),
    MakeWfcMusicRest(0, 360, 1560)]));
  try
    Check(BuildRejected(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('onset', 0, 0, 480)]),
      wmtpMelody, 240, 1, 'not aligned'),
      'unaligned selected note onset is rejected');
  finally
    LScore.Free;
  end;

  LScore := NewSingleVoiceScore(1920, SpansOf([
    MakeWfcMusicSound(0, 0, 360,
      TonesOf([MakeWfcMusicTone(60, 100)])),
    MakeWfcMusicRest(0, 360, 1560)]));
  try
    Check(BuildRejected(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('duration', 0, 0, 480)]),
      wmtpMelody, 240, 1, 'not aligned'),
      'unaligned selected note duration is rejected');
  finally
    LScore.Free;
  end;
end;

procedure TestHugeSparseExtraction;
const
  HUGE_LENGTH = 16777216;
var
  LDocument: TWfcTrainingDocument;
  LSample: TWfcTrainingSample;
  LScore: TWfcMusicScore;
begin
  BeginTest('tiny ranges are extracted from huge sparse timelines');
  LScore := NewSingleVoiceScore(HUGE_LENGTH,
    SpansOf([MakeWfcMusicRest(0, 0, HUGE_LENGTH)]), 1);
  try
    LDocument := BuildWfcMusicTrainingDocument(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('tiny', 0, HUGE_LENGTH div 2, 4)]),
      wmtpRhythm, 1, 1, BasicMetadata);
    try
      LSample := LDocument.SampleAt(0);
      Check((LSample.Width = 4) and (Length(LSample.Tokens) = 4) and
        (LSample.Tokens[0] = 'wr1:r') and
        (LSample.Tokens[3] = 'wr1:r'),
        'allocation follows selection size rather than score length');
    finally
      LDocument.Free;
    end;
  finally
    LScore.Free;
  end;
end;

function NewManySpanScore(const ASpanCount: Integer): TWfcMusicScore;
var
  I: Integer;
  LLength: Integer;
  LSpans: TWfcMusicSpanEvents;
begin
  LLength := ASpanCount + 3;
  SetLength(LSpans, ASpanCount);
  for I := 0 to ASpanCount - 2 do
    if (I and 1) = 0 then
      LSpans[I] := MakeWfcMusicSound(0, I, 1,
        TonesOf([MakeWfcMusicTone(60, 100)]))
    else
      LSpans[I] := MakeWfcMusicRest(0, I, 1);
  I := ASpanCount - 1;
  if (I and 1) = 0 then
    LSpans[I] := MakeWfcMusicSound(0, I, 4,
      TonesOf([MakeWfcMusicTone(60, 100)]))
  else
    LSpans[I] := MakeWfcMusicRest(0, I, 4);
  Result := NewSingleVoiceScore(LLength, LSpans, 1);
end;

procedure TestCapacityPreflight;
var
  I: Integer;
  LManySelections: TWfcMusicTrainingSelections;
  LScore: TWfcMusicScore;
begin
  BeginTest('sample, token and inspection capacities are preflighted');
  LScore := NewSingleVoiceScore(32772,
    SpansOf([MakeWfcMusicRest(0, 0, 32772)]), 1);
  try
    SetLength(LManySelections, WFC_TRAINING_MAX_SAMPLE_COUNT + 1);
    Check(BuildRejected(LScore, LManySelections, wmtpMelody, 1, 1,
      'selection count'), 'selection-count cap is enforced before projection');

    LManySelections := SelectionsOf([
      MakeWfcMusicTrainingSelection('first', 0, 0, 32769),
      MakeWfcMusicTrainingSelection('second', 0, 0, 32769)]);
    Check(BuildRejected(LScore, LManySelections, wmtpMelody, 1, 1,
      'aggregate'), 'aggregate output-token cap is preflighted');
  finally
    LScore.Free;
  end;

  LScore := NewManySpanScore(4097);
  try
    SetLength(LManySelections, WFC_TRAINING_MAX_SAMPLE_COUNT);
    for I := 0 to Length(LManySelections) - 1 do
      LManySelections[I].VoiceIndex := 0;
    Check(BuildRejected(LScore, LManySelections, wmtpMelody, 1, 1,
      'inspection'),
      'span-by-selection inspection work is overflow-safe and bounded');
  finally
    LScore.Free;
  end;
end;

procedure TestIndependentLearningSamples;
var
  I: Integer;
  LDocument: TWfcTrainingDocument;
  LModel: TWfcSequenceModel;
  LScore: TWfcMusicScore;
  LStartTotal: Integer;
begin
  BeginTest('ordered phrase and voice selections remain independent');
  LScore := TwoVoicePhraseScore;
  try
    LDocument := BuildWfcMusicTrainingDocument(LScore, SelectionsOf([
      MakeWfcMusicTrainingSelection('first', 0, 0, 240),
      MakeWfcMusicTrainingSelection('second', 1, 0, 240)]),
      wmtpMelody, 240, 1, BasicMetadata);
    try
      Check((LDocument.SampleAt(0).Tokens[0] = 'wm1:a:72:100') and
        (LDocument.SampleAt(1).Tokens[0] = 'wm1:a:48:90'),
        'each sample projects only its explicitly selected voice');
      LModel := DecodeWfcSequenceText(LearnWfcTrainingModelText(LDocument));
      try
        Check((LModel.SampleCount = 2) and
          (LModel.SampleLengthAt(0) = 1) and
          (LModel.SampleLengthAt(1) = 1),
          'learning retains two explicit phrase boundaries');
        LStartTotal := 0;
        for I := 0 to LModel.StateCount - 1 do
          Inc(LStartTotal, LModel.StartCountAt(I));
        Check(LStartTotal = 2,
          'both selections begin independently rather than forming a seam');
      finally
        LModel.Free;
      end;
    finally
      LDocument.Free;
    end;
  finally
    LScore.Free;
  end;
end;

procedure TestMetadataFailureWrapping;
var
  LDocument: TWfcTrainingDocument;
  LMetadata: TWfcTrainingMetadata;
  LRaised: Boolean;
  LScore: TWfcMusicScore;
begin
  BeginTest('training validation failures use the bridge exception type');
  LScore := BasicScore;
  try
    LMetadata := BasicMetadata;
    LMetadata.LicenseIdentifier := '';
    LDocument := nil;
    LRaised := False;
    try
      try
        LDocument := BuildWfcMusicTrainingDocument(LScore, SelectionsOf([
          MakeWfcMusicTrainingSelection('sample', 0, 0, 240)]),
          wmtpMelody, 240, 1, LMetadata);
      except
        on E: EWfcMusicTraining do
          LRaised := Pos('license identifier', E.Message) > 0;
      end;
    finally
      LDocument.Free;
    end;
    Check(LRaised, 'metadata errors preserve their useful message');
  finally
    LScore.Free;
  end;
end;

begin
  WriteLn('WFC music-training bridge tests');
  WriteLn('version=', WFC_MUSIC_TRAINING_VERSION);
  TestProjectionDocuments;
  TestDetachment;
  TestBoundariesAndCuts;
  TestChordRejection;
  TestAlignmentAndArguments;
  TestSoundAlignment;
  TestHugeSparseExtraction;
  TestCapacityPreflight;
  TestIndependentLearningSamples;
  TestMetadataFailureWrapping;
  WriteLn('[SUMMARY] checks=', GCheckCount, ' failures=', GFailureCount);
  if GFailureCount <> 0 then
    Halt(1);
end.
