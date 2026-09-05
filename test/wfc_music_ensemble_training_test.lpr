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
program wfc_music_ensemble_training_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc_model, wfc_music, wfc_music_text, wfc_music_sequence,
  wfc_music_ensemble, wfc_music_ensemble_training, wfc_sequence,
  wfc_sequence_text, wfc_training, wfc_training_text;

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GCheckCount);
  if ACondition then WriteLn('  [PASS] ', AMessage)
  else begin Inc(GFailureCount); WriteLn('  [FAIL] ', AMessage); end;
end;

function TonesOf(const AValues: array of TWfcMusicTone): TWfcMusicTones;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

function SpansOf(const AValues: array of TWfcMusicSpanEvent): TWfcMusicSpanEvents;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

function VoicesOf(const AValues: array of Integer): TWfcMusicEnsembleTrainingVoiceIndices;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

function SelectionsOf(const AValues: array of TWfcMusicEnsembleTrainingSelection):
  TWfcMusicEnsembleTrainingSelections;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

function OneSelection(const AStart, ALength: Integer):
  TWfcMusicEnsembleTrainingSelections;
begin
  Result := SelectionsOf([
    MakeWfcMusicEnsembleTrainingSelection('excerpt', AStart, ALength)]);
end;

function BasicMetadata: TWfcTrainingMetadata;
begin
  Result := MakeWfcTrainingMetadata('ensemble phrases', 'MIT',
    'project-authored independent common excerpts');
end;

function NewScore(const AVoiceCount, ALength, AQuarter, ASteps: Integer;
  const ASpans: TWfcMusicSpanEvents; const ASecondTempo: Boolean = False): TWfcMusicScore;
var Tracks: TWfcMusicTracks; Voices: TWfcMusicVoices;
  Meters: TWfcMusicMeterChanges; Tempos: TWfcMusicTempoChanges; I: Integer;
begin
  SetLength(Tracks, 1);
  Tracks[0] := MakeWfcMusicTrack('ensemble', 'Ensemble');
  SetLength(Voices, AVoiceCount);
  for I := 0 to AVoiceCount - 1 do
    Voices[I] := MakeWfcMusicVoice(0, 'voice-' + IntToStr(I));
  SetLength(Meters, 1);
  Meters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  if ASecondTempo then SetLength(Tempos, 2) else SetLength(Tempos, 1);
  Tempos[0] := MakeWfcMusicTempoChange(0, 500000);
  if ASecondTempo then Tempos[1] := MakeWfcMusicTempoChange(123, 430000);
  Result := TWfcMusicScore.Create(AQuarter, ASteps, ALength,
    Tracks, Voices, Meters, Tempos, ASpans);
end;

function BasicScore: TWfcMusicScore;
begin
  Result := NewScore(4, 1920, 240, 12, SpansOf([
    MakeWfcMusicSound(0, 0, 960, TonesOf([MakeWfcMusicTone(48, 80)])),
    MakeWfcMusicRest(0, 960, 960),
    MakeWfcMusicSound(1, 0, 960, TonesOf([MakeWfcMusicTone(60, 90),
      MakeWfcMusicTone(64, 75), MakeWfcMusicTone(67, 85)])),
    MakeWfcMusicRest(1, 960, 960),
    MakeWfcMusicSound(2, 0, 240, TonesOf([MakeWfcMusicTone(72, 100)])),
    MakeWfcMusicSound(2, 240, 240, TonesOf([MakeWfcMusicTone(74, 101)])),
    MakeWfcMusicSound(2, 480, 240, TonesOf([MakeWfcMusicTone(74, 101)])),
    MakeWfcMusicRest(2, 720, 240),
    MakeWfcMusicSound(2, 960, 240, TonesOf([MakeWfcMusicTone(76, 90)])),
    MakeWfcMusicRest(2, 1200, 720),
    MakeWfcMusicRest(3, 0, 1920)]), True);
end;

function RestScore(const AVoiceCount, ALength: Integer): TWfcMusicScore;
var Spans: TWfcMusicSpanEvents; I: Integer;
begin
  SetLength(Spans, AVoiceCount);
  for I := 0 to AVoiceCount - 1 do Spans[I] := MakeWfcMusicRest(I, 0, ALength);
  Result := NewScore(AVoiceCount, ALength, 1, 12, Spans);
end;

function AllVoices(const ACount: Integer): TWfcMusicEnsembleTrainingVoiceIndices;
var I: Integer;
begin
  Result := nil; SetLength(Result, ACount);
  for I := 0 to ACount - 1 do Result[I] := I;
end;

function BuildRejected(const AScore: TWfcMusicScore;
  const AVoices: TWfcMusicEnsembleTrainingVoiceIndices;
  const ASelections: TWfcMusicEnsembleTrainingSelections;
  const AProjection: TWfcMusicEnsembleTrainingProjection;
  const AQuantum, AOrder: Integer; const AExpected: String;
  const AMetadata: TWfcTrainingMetadata): Boolean;
var Document: TWfcTrainingDocument;
begin
  Document := nil; Result := False;
  try
    try
      Document := BuildWfcMusicEnsembleTrainingDocument(AScore, AVoices,
        ASelections, AProjection, AQuantum, AOrder, AMetadata);
    except
      on E: EWfcMusicEnsembleTraining do
        Result := (Document = nil) and (Pos('ensemble training adapter:', E.Message) = 1)
          and ((AExpected = '') or (Pos(AExpected, E.Message) > 0));
    end;
  finally Document.Free; end;
end;

function Rejected(const AScore: TWfcMusicScore;
  const AVoices: TWfcMusicEnsembleTrainingVoiceIndices;
  const ASelections: TWfcMusicEnsembleTrainingSelections;
  const AProjection: TWfcMusicEnsembleTrainingProjection;
  const AQuantum, AOrder: Integer; const AExpected: String): Boolean;
begin
  Result := BuildRejected(AScore, AVoices, ASelections, AProjection,
    AQuantum, AOrder, AExpected, BasicMetadata);
end;

{$PUSH}{$R-}
function InvalidProjection: TWfcMusicEnsembleTrainingProjection;
var Value: Integer;
begin
  Value := Ord(High(TWfcMusicEnsembleTrainingProjection)) + 1;
  Result := TWfcMusicEnsembleTrainingProjection(Value);
end;
{$POP}

procedure TestExactProjectionAndRoundtrip;
const
  FIRST = 'wme1:4:a:1:48:80:a:3:60:90:64:75:67:85:a:1:72:100:r';
  NEXT = 'wme1:4:h:1:48:80:h:3:60:90:64:75:67:85:a:1:74:101:r';
var Score, Rebuilt: TWfcMusicScore; Document: TWfcTrainingDocument;
  Selections: TWfcMusicEnsembleTrainingSelections; A, B: TWfcTrainingSample;
  Tokens: TWfcModelTokens; Frames: TWfcMusicEnsembleFrames;
  Spans: TWfcMusicSpanEvents; Options: TWfcTrainingOptions;
  Metadata: TWfcTrainingMetadata; I: Integer;
begin
  WriteLn('[TEST] synchronized exact voices, independent attacks, complete roundtrip');
  Score := BasicScore;
  try
    Selections := SelectionsOf([
      MakeWfcMusicEnsembleTrainingSelection('opening', 0, 960),
      MakeWfcMusicEnsembleTrainingSelection('ending', 960, 960)]);
    Document := BuildWfcMusicEnsembleTrainingDocument(Score, VoicesOf([0, 1, 2, 3]),
      Selections, wmetpFrame, 240, 2, BasicMetadata);
    try
      Options := Document.CopyOptions;
      Check((Options.Kind = wtkSequence) and (Options.Boundary = wmbOpen) and
        (Options.Symmetry = wmsNone) and (Options.PatternWidth = 0) and
        (Options.PatternHeight = 0) and (Options.Order = 2),
        'common excerpts use the unchanged open sequence profile');
      Metadata := Document.CopyMetadata;
      Check((Metadata.Name = BasicMetadata.Name) and
        (Metadata.LicenseIdentifier = 'MIT') and
        (Metadata.SourceDescription = BasicMetadata.SourceDescription),
        'all caller provenance metadata is retained');
      Check((Document.SampleCount = 2) and (Document.TotalTokenCount = 8),
        'each simultaneous frame is one token, not one token per voice');
      A := Document.SampleAt(0); B := Document.SampleAt(1);
      Check((A.Name = 'opening') and (B.Name = 'ending') and
        (A.Width = 4) and (A.Height = 1) and (Length(A.Tokens) = 4),
        'explicit sample order and excerpt dimensions are retained');
      Check(A.Tokens[0] = FIRST, 'first frame preserves all three chord velocities and silent slot');
      Check(A.Tokens[1] = NEXT, 'upper attack does not retrigger held bass or held chord');
      Check(A.Tokens[2] = NEXT, 'identical adjacent upper notes remain separate attacks');
      Check(A.Tokens[3] = 'wme1:4:h:1:48:80:h:3:60:90:64:75:67:85:r:r',
        'upper rest leaves the other voices sounding');
      Check((B.Tokens[0] = 'wme1:4:r:r:a:1:76:90:r') and
        (B.Tokens[1] = 'wme1:4:r:r:r:r'), 'later excerpt retains all silent voice slots');
      SetLength(Tokens, 8);
      for I := 0 to 3 do begin Tokens[I] := A.Tokens[I]; Tokens[I + 4] := B.Tokens[I]; end;
      Frames := DecodeWfcMusicEnsembleFrames(Tokens);
      ValidateWfcMusicEnsembleFrames(Frames);
      for I := 0 to High(Tokens) do
        Check(EncodeWfcMusicEnsembleFrame(Frames[I]) = Tokens[I],
          'frame token is canonical at ' + IntToStr(I));
      Rebuilt := RebuildWfcMusicEnsembleScore(Frames, 240, Score);
      try
        Check(EncodeWfcMusicText(Rebuilt) = EncodeWfcMusicText(Score),
          'complete selected frames rebuild every score span and timing metadata exactly');
      finally Rebuilt.Free; end;
      Spans := RebuildWfcMusicEnsembleSpans(DecodeWfcMusicEnsembleFrames(A.Tokens), 240);
      Check((Spans[0].VoiceIndex = 0) and (Spans[0].DurationTicks = 960) and
        (Spans[1].VoiceIndex = 1) and (Spans[1].DurationTicks = 960) and
        (Length(Spans[1].Tones) = 3), 'excerpt reconstruction does not slice sustained bass or chords');
      Check((Spans[3].StartTick = 240) and (Spans[4].StartTick = 480),
        'reconstruction retains both repeated-note attack boundaries');
    finally Document.Free; end;
  finally Score.Free; end;
end;

procedure TestOtherProjectionsAndVoiceOrder;
var Score: TWfcMusicScore; Document: TWfcTrainingDocument;
  Sample: TWfcTrainingSample; Frame: TWfcMusicEnsembleFrame; I: Integer;
begin
  WriteLn('[TEST] exact sounding sets, rhythm vectors, caller voice order');
  Score := BasicScore;
  try
    Document := BuildWfcMusicEnsembleTrainingDocument(Score, AllVoices(4),
      OneSelection(0, 1920), wmetpRhythmVector, 240, 2, BasicMetadata);
    try
      Sample := Document.SampleAt(0);
      Check((Sample.Tokens[0] = 'wmer1:4:a:a:a:r') and
        (Sample.Tokens[1] = 'wmer1:4:h:h:a:r') and
        (Sample.Tokens[2] = 'wmer1:4:h:h:a:r') and
        (Sample.Tokens[3] = 'wmer1:4:h:h:r:r') and
        (Sample.Tokens[4] = 'wmer1:4:r:r:a:r'),
        'rhythm is per-voice attack/hold/rest, not a flattened global action');
      for I := 0 to High(Sample.Tokens) do
        Check(EncodeWfcMusicRhythmFrame(DecodeWfcMusicRhythmFrame(Sample.Tokens[I])) =
          Sample.Tokens[I], 'rhythm token is canonical at ' + IntToStr(I));
    finally Document.Free; end;
    Document := BuildWfcMusicEnsembleTrainingDocument(Score, AllVoices(4),
      OneSelection(0, 1920), wmetpExactPitchClassSet, 240, 2, BasicMetadata);
    try
      Sample := Document.SampleAt(0);
      Check((Sample.Tokens[0] = 'wmhs1:12:3:0:4:7') and
        (Sample.Tokens[1] = 'wmhs1:12:4:0:2:4:7') and
        (Sample.Tokens[2] = 'wmhs1:12:4:0:2:4:7') and
        (Sample.Tokens[3] = 'wmhs1:12:3:0:4:7') and
        (Sample.Tokens[4] = 'wmhs1:12:1:4') and
        (Sample.Tokens[5] = 'wmhs1:12:0'),
        'exact sets include held tones, deduplicate octaves and represent silence explicitly');
      for I := 0 to High(Sample.Tokens) do
        Check(EncodeWfcMusicPitchClassSet(DecodeWfcMusicPitchClassSet(Sample.Tokens[I])) =
          Sample.Tokens[I], 'pitch-class token is canonical at ' + IntToStr(I));
    finally Document.Free; end;
    Document := BuildWfcMusicEnsembleTrainingDocument(Score, VoicesOf([2, 0, 3, 1]),
      OneSelection(0, 960), wmetpFrame, 240, 1, BasicMetadata);
    try
      Frame := DecodeWfcMusicEnsembleFrame(Document.SampleAt(0).Tokens[1]);
      Check((Length(Frame.Voices) = 4) and
        (Frame.Voices[0].Tones[0].Pitch = 74) and
        (Frame.Voices[1].Tones[0].Pitch = 48) and
        (Frame.Voices[2].Action = wmcaRest) and
        (Length(Frame.Voices[3].Tones) = 3),
        'source indexing optimization never changes caller slot order');
    finally Document.Free; end;
    Document := BuildWfcMusicEnsembleTrainingDocument(Score, VoicesOf([3]),
      OneSelection(240, 240), wmetpFrame, 240, 1, BasicMetadata);
    try
      Check(Document.SampleAt(0).Tokens[0] = 'wme1:1:r',
        'one rest-only voice and a sub-measure excerpt are valid');
    finally Document.Free; end;
  finally Score.Free; end;
  Score := NewScore(1, 4, 1, 19, SpansOf([
    MakeWfcMusicSound(0, 0, 4, TonesOf([MakeWfcMusicTone(20, 70),
      MakeWfcMusicTone(38, 127), MakeWfcMusicTone(39, 1)]))]));
  try
    Document := BuildWfcMusicEnsembleTrainingDocument(Score, VoicesOf([0]),
      OneSelection(0, 4), wmetpExactPitchClassSet, 4, 1, BasicMetadata);
    try Check(Document.SampleAt(0).Tokens[0] = 'wmhs1:19:2:0:1',
      'projection uses the score tuning, not an implicit twelve-step tuning');
    finally Document.Free; end;
  finally Score.Free; end;
end;

procedure TestCorpusAndDetachment;
var Score: TWfcMusicScore; Document, Decoded: TWfcTrainingDocument;
  Model: TWfcSequenceModel; Voices: TWfcMusicEnsembleTrainingVoiceIndices;
  Selections: TWfcMusicEnsembleTrainingSelections; Metadata: TWfcTrainingMetadata;
  Sample: TWfcTrainingSample; Samples: TWfcTrainingSamples;
  Before, Learned: String; I, Starts, Ends: Integer; Signature: Cardinal;
begin
  WriteLn('[TEST] independent corpus boundaries, metadata, immutable source detachment');
  Score := BasicScore;
  Voices := AllVoices(4); Metadata := BasicMetadata;
  Selections := SelectionsOf([
    MakeWfcMusicEnsembleTrainingSelection('later first', 960, 960),
    MakeWfcMusicEnsembleTrainingSelection('opening second', 0, 960)]);
  Document := BuildWfcMusicEnsembleTrainingDocument(Score, Voices, Selections,
    wmetpFrame, 240, 3, Metadata);
  try
    Before := EncodeWfcTrainingText(Document); Signature := Document.Signature;
    Learned := LearnWfcTrainingModelText(Document);
    Model := DecodeWfcSequenceText(Learned);
    try
      Check((Model.SampleCount = 2) and (Model.SampleLengthAt(0) = 4) and
        (Model.SampleLengthAt(1) = 4) and (Model.ObservationCount = 8),
        'learner consumes two independent common excerpts');
      Starts := 0; Ends := 0;
      for I := 0 to Model.StateCount - 1 do
      begin
        Inc(Starts, Model.StartCountAt(I)); Inc(Ends, Model.EndCountAt(I));
        if Model.StartCountAt(I) > 0 then
          Check(Model.StateLeadingBosCountAt(I) = 2,
            'each common excerpt resets both history positions to BOS');
      end;
      Check((Starts = 2) and (Ends = 2),
        'no synthetic seam joins the two common excerpts');
      Check(Model.PublicTokenAt(0) = 'wme1:4:r:r:a:1:76:90:r',
        'explicit corpus order controls stable first-observation vocabulary order');
    finally Model.Free; end;
    Voices[0] := 3; Selections[0].Name := 'changed'; Selections[0].StartTick := 0;
    Metadata.Name := 'changed'; Sample := Document.SampleAt(0);
    Sample.Tokens[0] := 'changed'; Samples := Document.CopySamples;
    Samples[1].Tokens[0] := 'changed'; Score.Free; Score := nil;
    Check((EncodeWfcTrainingText(Document) = Before) and (Document.Signature = Signature),
      'mutated inputs and accessor arrays cannot change the detached document');
    Check(LearnWfcTrainingModelText(Document) = Learned,
      'source can be freed before deterministic learning or persistence');
    Decoded := DecodeWfcTrainingText(Before);
    try Check(EncodeWfcTrainingText(Decoded) = Before,
      'new projected tokens roundtrip through the unchanged training text protocol');
    finally Decoded.Free; end;
  finally Document.Free; Score.Free; end;
end;

procedure TestExcerptBoundariesAndAlignment;
var Score: TWfcMusicScore; Document: TWfcTrainingDocument;
  Projection: TWfcMusicEnsembleTrainingProjection;
begin
  WriteLn('[TEST] sounded cuts are rejected across every selected voice');
  Score := BasicScore;
  try
    for Projection := Low(Projection) to High(Projection) do
    begin
      Check(Rejected(Score, AllVoices(4), OneSelection(240, 720), Projection, 240, 1, 'cuts'),
        'all projections reject an initial cut through bass or chord ' + IntToStr(Ord(Projection)));
      Check(Rejected(Score, VoicesOf([2, 1]), OneSelection(0, 720), Projection, 240, 1, 'cuts'),
        'all projections reject a terminal cut in a non-first selected voice ' + IntToStr(Ord(Projection)));
      Document := BuildWfcMusicEnsembleTrainingDocument(Score, VoicesOf([2, 3]),
        OneSelection(240, 480), Projection, 240, 1, BasicMetadata);
      try Check(Document.TotalTokenCount = 2,
        'unselected sounding cuts do not reject an otherwise exact excerpt ' + IntToStr(Ord(Projection)));
      finally Document.Free; end;
    end;
    Document := BuildWfcMusicEnsembleTrainingDocument(Score, AllVoices(4),
      OneSelection(1440, 240), wmetpFrame, 240, 1, BasicMetadata);
    try Check(Document.SampleAt(0).Tokens[0] = 'wme1:4:r:r:r:r',
      'silence may be cropped at both common excerpt boundaries');
    finally Document.Free; end;
  finally Score.Free; end;
  Score := NewScore(2, 16, 1, 12, SpansOf([
    MakeWfcMusicRest(0, 0, 1),
    MakeWfcMusicSound(0, 1, 2, TonesOf([MakeWfcMusicTone(60, 80), MakeWfcMusicTone(64, 90)])),
    MakeWfcMusicRest(0, 3, 5),
    MakeWfcMusicSound(0, 8, 4, TonesOf([MakeWfcMusicTone(67, 90)])),
    MakeWfcMusicRest(0, 12, 4),
    MakeWfcMusicRest(1, 0, 16)]));
  try
    Check(Rejected(Score, VoicesOf([0]), OneSelection(0, 4), wmetpFrame, 2, 1, 'aligned'),
      'retained non-grid sound or silence boundary rejects before token expansion');
    Document := BuildWfcMusicEnsembleTrainingDocument(Score, VoicesOf([0]),
      OneSelection(4, 8), wmetpFrame, 4, 1, BasicMetadata);
    try Check((Document.SampleAt(0).Tokens[0] = 'wme1:1:r') and
      (Document.SampleAt(0).Tokens[1] = 'wme1:1:a:1:67:90'),
      'unaligned earlier chord is never expanded and a cropped earlier rest is valid');
    finally Document.Free; end;
    Document := BuildWfcMusicEnsembleTrainingDocument(Score, VoicesOf([1]),
      OneSelection(0, 16), wmetpFrame, 4, 1, BasicMetadata);
    try Check(Document.TotalTokenCount = 4,
      'unaligned spans in entirely unselected voices do not invalidate training');
    finally Document.Free; end;
  finally Score.Free; end;
  Score := RestScore(1, High(Integer) - 3);
  try
    Document := BuildWfcMusicEnsembleTrainingDocument(Score, VoicesOf([0]),
      OneSelection(High(Integer) - 7, 4), wmetpFrame, 4, 1, BasicMetadata);
    try Check((Document.TotalTokenCount = 1) and
      (Document.SampleAt(0).Tokens[0] = 'wme1:1:r'),
      'tiny late excerpt from a huge sparse score does not allocate its full timeline');
    finally Document.Free; end;
  finally Score.Free; end;
end;

procedure TestArguments;
var Score: TWfcMusicScore; Selections: TWfcMusicEnsembleTrainingSelections;
  Metadata: TWfcTrainingMetadata; Voices: TWfcMusicEnsembleTrainingVoiceIndices;
  Document: TWfcTrainingDocument;
begin
  WriteLn('[TEST] argument validation and typed failure boundary');
  Score := BasicScore; Selections := OneSelection(0, 960);
  try
    Check(Rejected(nil, AllVoices(4), Selections, wmetpFrame, 240, 1, 'nil'), 'nil score rejects');
    Check(Rejected(Score, nil, Selections, wmetpFrame, 240, 1, 'voice count'), 'empty voice vector rejects');
    Check(Rejected(Score, VoicesOf([0, 0]), Selections, wmetpFrame, 240, 1, 'distinct'), 'duplicate source voices reject');
    Check(Rejected(Score, VoicesOf([-1]), Selections, wmetpFrame, 240, 1, 'bounds'), 'negative source voice rejects');
    Check(Rejected(Score, VoicesOf([4]), Selections, wmetpFrame, 240, 1, 'bounds'), 'out-of-range source voice rejects');
    SetLength(Voices, WFC_TRAINING_MAX_DIMENSION + 1);
    Check(Rejected(Score, Voices, Selections, wmetpFrame, 240, 1, 'dimension'),
      'excess selected voice count rejects before copying the selection vector');
    Check(Rejected(Score, AllVoices(4), nil, wmetpFrame, 240, 1, 'selection count'), 'empty corpus rejects');
    Check(Rejected(Score, AllVoices(4), Selections, InvalidProjection, 240, 1, 'projection'), 'unknown projection rejects');
    Check(Rejected(Score, AllVoices(4), Selections, wmetpFrame, 0, 1, 'quantum'), 'nonpositive quantum rejects');
    Check(Rejected(Score, AllVoices(4), Selections, wmetpFrame, 240, 0, 'order'), 'zero order rejects');
    Check(Rejected(Score, AllVoices(4), Selections, wmetpFrame, 240, 65, 'order'), 'order above existing adapter limit rejects');
    Check(Rejected(Score, AllVoices(4), Selections, wmetpFrame, 240, 5, 'fewer'), 'excerpt shorter than order rejects');
    Check(Rejected(Score, AllVoices(4), OneSelection(-1, 960), wmetpFrame, 240, 1, 'start'), 'negative start rejects');
    Check(Rejected(Score, AllVoices(4), OneSelection(0, 0), wmetpFrame, 240, 1, 'length'), 'zero length rejects');
    Check(Rejected(Score, AllVoices(4), OneSelection(High(Integer), 1), wmetpFrame, 240, 1, 'Integer'), 'end arithmetic cannot wrap');
    Check(Rejected(Score, AllVoices(4), OneSelection(960, 1200), wmetpFrame, 240, 1, 'beyond'), 'excerpt past score rejects');
    Check(Rejected(Score, AllVoices(4), OneSelection(1, 960), wmetpFrame, 240, 1, 'aligned'), 'unaligned excerpt start rejects');
    Check(Rejected(Score, AllVoices(4), OneSelection(0, 959), wmetpFrame, 240, 1, 'aligned'), 'unaligned excerpt length rejects');
    Selections[0].Name := '';
    Check(Rejected(Score, AllVoices(4), Selections, wmetpFrame, 240, 1, 'selection name'), 'empty excerpt name rejects');
    Selections := SelectionsOf([
      MakeWfcMusicEnsembleTrainingSelection('same', 0, 960),
      MakeWfcMusicEnsembleTrainingSelection('same', 960, 960)]);
    Check(Rejected(Score, AllVoices(4), Selections, wmetpFrame, 240, 1, 'unique'), 'duplicate excerpt names reject');
    SetLength(Selections, WFC_TRAINING_MAX_SAMPLE_COUNT + 1);
    Check(Rejected(Score, AllVoices(4), Selections, wmetpFrame, 240, 1, 'selection count'),
      'excess corpus count rejects before allocating sample output');
    Metadata := BasicMetadata; Metadata.LicenseIdentifier := '';
    Check(BuildRejected(Score, AllVoices(4), OneSelection(0, 960), wmetpFrame,
      240, 1, 'metadata license', Metadata), 'metadata failures retain the adapter exception and useful context');
    Metadata := BasicMetadata;
    Metadata.Name := StringOfChar(' ', WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH div 3 + 1);
    Check(BuildRejected(Score, AllVoices(4), OneSelection(0, 960), wmetpFrame,
      240, 1, 'encoded-token', Metadata), 'percent-encoded metadata size, not only raw length, is bounded');
    Metadata.Name := StringOfChar(' ', WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH div 3);
    Document := BuildWfcMusicEnsembleTrainingDocument(Score, AllVoices(4),
      OneSelection(0, 960), wmetpFrame, 240, 1, Metadata);
    try Check(Document.CopyMetadata.Name = Metadata.Name,
      'metadata immediately below the escaped-token cap remains valid');
    finally Document.Free; end;
  finally Score.Free; end;
end;

procedure TestCapacityPreflight;
var Score: TWfcMusicScore; Document: TWfcTrainingDocument; Tones: TWfcMusicTones;
  Spans: TWfcMusicSpanEvents; Selections: TWfcMusicEnsembleTrainingSelections;
  Classes: TWfcMusicPitchClassSet; I: Integer;
begin
  WriteLn('[TEST] existing training expansion, encoded-content and model limits');
  Score := RestScore(257, 65536);
  try
    Check(Rejected(Score, AllVoices(257), OneSelection(0, 65536), wmetpFrame, 1, 1,
      'expanded voice cells'), 'expanded voice-cell budget is checked before allocating a frame matrix');
  finally Score.Free; end;
  SetLength(Tones, 257);
  for I := 0 to High(Tones) do Tones[I] := MakeWfcMusicTone(I, 80);
  Score := NewScore(1, 65536, 1, 12, SpansOf([MakeWfcMusicSound(0, 0, 65536, Tones)]));
  try
    Check(Rejected(Score, VoicesOf([0]), OneSelection(0, 65536), wmetpFrame, 1, 1,
      'expanded tone copies'), 'a held chord cannot bypass repeated-tone work preflight');
  finally Score.Free; end;
  SetLength(Tones, 6000);
  for I := 0 to High(Tones) do Tones[I] := MakeWfcMusicTone(I, 80);
  Score := NewScore(1, 4, 1, 12, SpansOf([MakeWfcMusicSound(0, 0, 4, Tones)]));
  try
    Check(Rejected(Score, VoicesOf([0]), OneSelection(0, 4), wmetpFrame, 4, 1,
      'projected encoded token'), 'oversized chord token rejects before canonical token concatenation');
    Document := BuildWfcMusicEnsembleTrainingDocument(Score, VoicesOf([0]),
      OneSelection(0, 4), wmetpRhythmVector, 4, 1, BasicMetadata);
    try Check(Document.SampleAt(0).Tokens[0] = 'wmer1:1:a',
      'large exact chord does not impose its token size on a small rhythm projection');
    finally Document.Free; end;
    Document := BuildWfcMusicEnsembleTrainingDocument(Score, VoicesOf([0]),
      OneSelection(0, 4), wmetpExactPitchClassSet, 4, 1, BasicMetadata);
    try Check(Document.SampleAt(0).Tokens[0] = 'wmhs1:12:12:0:1:2:3:4:5:6:7:8:9:10:11',
      'large chord can still project to its small exact pitch-class set');
    finally Document.Free; end;
  finally Score.Free; end;
  SetLength(Tones, 4096);
  for I := 0 to High(Tones) do Tones[I] := MakeWfcMusicTone(I * 2, 80);
  Score := NewScore(1, 4, 1, 8192, SpansOf([MakeWfcMusicSound(0, 0, 4, Tones)]));
  try
    Document := BuildWfcMusicEnsembleTrainingDocument(Score, VoicesOf([0]),
      OneSelection(0, 4), wmetpExactPitchClassSet, 2, 1, BasicMetadata);
    try
      Classes := DecodeWfcMusicPitchClassSet(Document.SampleAt(0).Tokens[0]);
      Check((Classes.StepsPerOctave = 8192) and
        (Length(Classes.PitchClasses) = 4096) and
        (Classes.PitchClasses[0] = 0) and (Classes.PitchClasses[4095] = 8190),
        'large non-twelve-step set remains exact within the existing encoded-token limit');
      Check(Document.SampleAt(0).Tokens[0] = Document.SampleAt(0).Tokens[1],
        'large-set projection includes the same tones on attack and sustain');
    finally Document.Free; end;
  finally Score.Free; end;
  SetLength(Tones, 10000);
  for I := 0 to High(Tones) do Tones[I] := MakeWfcMusicTone(I, 80);
  Score := NewScore(1, 4, 1, High(Integer),
    SpansOf([MakeWfcMusicSound(0, 0, 4, Tones)]));
  try
    Check(Rejected(Score, VoicesOf([0]), OneSelection(0, 4), wmetpExactPitchClassSet,
      4, 1, 'projected encoded token'),
      'oversized exact set rejects without a tuning-sized mask or token concatenation');
  finally Score.Free; end;
  Score := RestScore(256, 4096);
  try
    Check(Rejected(Score, AllVoices(256), OneSelection(0, 4096), wmetpFrame, 1, 1,
      'aggregate encoded content'), 'aggregate escaped content rejects before complete sample-token allocation');
  finally Score.Free; end;
  Score := RestScore(1, 65540);
  try
    Check(Rejected(Score, VoicesOf([0]), OneSelection(0, 65537), wmetpFrame, 1, 1,
      'dimension'), 'per-sample frame dimension preserves the existing training cap');
    Selections := SelectionsOf([
      MakeWfcMusicEnsembleTrainingSelection('one', 0, 40000),
      MakeWfcMusicEnsembleTrainingSelection('two', 0, 40000)]);
    Check(Rejected(Score, VoicesOf([0]), Selections, wmetpFrame, 1, 1,
      'aggregate frame'), 'overlapping excerpts count independently against aggregate frame cap');
    Document := BuildWfcMusicEnsembleTrainingDocument(Score, VoicesOf([0]),
      OneSelection(0, 65536), wmetpFrame, 1, 1, BasicMetadata);
    try Check(Document.TotalTokenCount = 65536,
      'the exact existing 65,536-token document limit is still accepted');
    finally Document.Free; end;
  finally Score.Free; end;
  SetLength(Spans, 1028);
  for I := 0 to High(Spans) do
    Spans[I] := MakeWfcMusicSound(0, I, 1, TonesOf([MakeWfcMusicTone(I, 80)]));
  Score := NewScore(1, 1028, 1, 12, Spans);
  try
    Check(Rejected(Score, VoicesOf([0]), OneSelection(0, 1028), wmetpFrame, 1, 1,
      ''), 'downstream sequence vocabulary preflight failures use the ensemble adapter boundary');
  finally Score.Free; end;
  SetLength(Spans, 2048);
  for I := 0 to High(Spans) do
    Spans[I] := MakeWfcMusicSound(0, I, 1, TonesOf([MakeWfcMusicTone(60, 80)]));
  Score := NewScore(1, 2048, 1, 12, Spans);
  try
    SetLength(Selections, WFC_TRAINING_MAX_SAMPLE_COUNT);
    Check(Rejected(Score, VoicesOf([0]), Selections, wmetpFrame, 1, 1,
      'span inspection'), 'source span by excerpt work is checked before repeated scans');
  finally Score.Free; end;
end;

begin
  WriteLn('WFC ensemble-training bridge tests');
  WriteLn('version=', WFC_MUSIC_ENSEMBLE_TRAINING_VERSION);
  TestExactProjectionAndRoundtrip;
  TestOtherProjectionsAndVoiceOrder;
  TestCorpusAndDetachment;
  TestExcerptBoundariesAndAlignment;
  TestArguments;
  TestCapacityPreflight;
  WriteLn('[SUMMARY] checks=', GCheckCount, ' failures=', GFailureCount);
  if GFailureCount <> 0 then Halt(1);
end.
