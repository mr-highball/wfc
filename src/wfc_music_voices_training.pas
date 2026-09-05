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
unit wfc_music_voices_training;

{$mode delphi}{$H+}

interface

uses wfc_model, wfc_music, wfc_music_ensemble_training, wfc_training,
  wfc_sequence;

const WFC_MUSIC_VOICES_TRAINING_VERSION = 1;

type
  EWfcMusicVoicesTraining = class(EWfcMusicEnsembleTraining);
  TWfcMusicVoiceTrainingRole = record
    Id: TWfcModelToken;
    SourceVoiceIndex, Order: Integer;
  end;
  TWfcMusicVoiceTrainingRoles = array of TWfcMusicVoiceTrainingRole;
  TWfcMusicVoiceTrainingProvenance = record
    Role: TWfcMusicVoiceTrainingRole;
    SourceVoiceId, SourceTrackId: TWfcModelToken;
    SourceTrackIndex: Integer;
  end;
  TWfcMusicVoiceSequenceModels = array of TWfcSequenceModel;

  { An immutable, detached group of ordinary training documents. Each voice
    document emits singleton wme1 frames, including complete chords. Harmony
    emits the union of selected sounding pitch classes; rhythm preserves the
    ordered action vector. Provenance is deliberately outside learned tokens.
    Every CopyDocument result is independently caller-owned. }
  TWfcMusicVoicesTrainingBundle = class
  private
    FProvenance: array of TWfcMusicVoiceTrainingProvenance;
    FDocuments: array of TWfcTrainingDocument;
    FSelections: TWfcMusicEnsembleTrainingSelections;
    FMetadata: TWfcTrainingMetadata;
    FQuantumTicks, FTicksPerQuarter, FStepsPerOctave: Integer;
    function GetRoleCount: Integer;
    procedure CheckRole(const AIndex: Integer);
    function CopyDocument(const AIndex: Integer): TWfcTrainingDocument;
  public
    destructor Destroy; override;
    function RoleAt(const AIndex: Integer): TWfcMusicVoiceTrainingProvenance;
    function CopySelections: TWfcMusicEnsembleTrainingSelections;
    function CopyMetadata: TWfcTrainingMetadata;
    function CopyHarmonyDocument: TWfcTrainingDocument;
    function CopyRhythmDocument: TWfcTrainingDocument;
    function CopyVoiceDocument(const AIndex: Integer): TWfcTrainingDocument;
    property RoleCount: Integer read GetRoleCount;
    property QuantumTicks: Integer read FQuantumTicks;
    property TicksPerQuarter: Integer read FTicksPerQuarter;
    property StepsPerOctave: Integer read FStepsPerOctave;
  end;

{ Score, metadata, roles and selections are borrowed only during this call.
  Common excerpt edges must not cut a sounded span in ANY selected voice.
  Rest cropping is permitted. No hold is rewritten as an attack.

  Limits apply to the whole bundle, not separately to each document: total
  samples, tokens, encoded metadata/sample names/tokens and provenance use the
  existing training limits. Source/excerpt inspection, expanded voice/tone
  observations and sequence-history work share one aggregate visit budget.
  This conservative preflight includes the reused document adapters' walks.
  Complete retained documents are allocated only after aggregate preflight;
  exceptions publish no partial bundle. SpanAt may temporarily copy a chord
  from the caller's already validated immutable score. }
function BuildWfcMusicVoicesTrainingBundle(const AScore: TWfcMusicScore;
  const ARoles: TWfcMusicVoiceTrainingRoles;
  const ASelections: TWfcMusicEnsembleTrainingSelections;
  const AQuantumTicks, AHarmonyOrder, ARhythmOrder: Integer;
  const AMetadata: TWfcTrainingMetadata): TWfcMusicVoicesTrainingBundle;

implementation

uses SysUtils, wfc_music_sequence, wfc_music_ensemble, wfc_text_codec;

type
  TIntegers = array of Integer;
  TBundleBuilder = class
  private
    FScore: TWfcMusicScore;
    FRoles: TWfcMusicVoiceTrainingRoles;
    FSelections: TWfcMusicEnsembleTrainingSelections;
    FFirst, FLast: TIntegers;
    FQuantum, FHOrder, FROrder, FEncoded, FWork: Integer;
    FMetadata: TWfcTrainingMetadata;
    procedure Preflight;
    procedure CheckProjectedContent;
  public
    function Build(const AScore: TWfcMusicScore;
      const ARoles: TWfcMusicVoiceTrainingRoles;
      const ASelections: TWfcMusicEnsembleTrainingSelections;
      const AQuantumTicks, AHarmonyOrder, ARhythmOrder: Integer;
      const AMetadata: TWfcTrainingMetadata): TWfcMusicVoicesTrainingBundle;
  end;

procedure TrainingError(const AMessage: String);
begin
  raise EWfcMusicVoicesTraining.Create('independent voice training: ' + AMessage);
end;

procedure CheckInteger(const AValue, AMinimum, AMaximum: Integer;
  const ALabel: String);
begin
  {$IFDEF PAS2JS}
  if (AValue <> AValue) or (AValue < AMinimum) or (AValue > AMaximum) then
    TrainingError(ALabel + ' is outside its integer range');
  if AValue <> Trunc(AValue) then TrainingError(ALabel + ' must be an integer');
  {$ELSE}
  if (AValue < AMinimum) or (AValue > AMaximum) then
    TrainingError(ALabel + ' is outside its integer range');
  {$ENDIF}
end;

function Add(const A, B, AMax: Integer; const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) or (A > AMax) or (B > AMax - A) then
    TrainingError('aggregate ' + ALabel + ' exceeds the training limit');
  Result := A + B;
end;

function Product(const A, B, AMax: Integer; const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) or ((A <> 0) and (B > AMax div A)) then
    TrainingError('aggregate ' + ALabel + ' exceeds the training limit');
  Result := A * B;
end;

procedure Accumulate(const AToken: TWfcModelToken; var ATotal: Integer);
var Encoded: String;
begin
  if (Length(AToken) = 0) or
    (Length(AToken) > WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH) or
    not WfcModelTokenIsValid(AToken) then TrainingError('invalid training token');
  Encoded := WfcTextEncodeToken(AToken, 'WFC independent voice training');
  if Length(Encoded) > WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH then
    TrainingError('encoded token exceeds the training limit');
  ATotal := Add(ATotal, Length(Encoded),
    WFC_TRAINING_MAX_TOTAL_ENCODED_TOKEN_LENGTH, 'encoded content');
end;

function Digits(const AValue: Integer): Integer;
var N: Integer;
begin
  N := AValue; Result := 1;
  while N >= 10 do begin Inc(Result); N := N div 10; end;
end;

procedure TBundleBuilder.Preflight;
var I, J, K, N, Docs, BaseEncoded, Orders, Frames, Count, Finish,
  SpanEnd, SelectedSpans, Tones: Integer;
  Span: TWfcMusicSpanEvent; Voice: TWfcMusicVoice; Track: TWfcMusicTrack;
begin
  N := Length(FRoles); Docs := N + 2;
  Product(Docs, Length(FSelections), WFC_TRAINING_MAX_SAMPLE_COUNT, 'samples');
  FEncoded := 0;
  Accumulate(FMetadata.Name, FEncoded);
  Accumulate(FMetadata.LicenseIdentifier, FEncoded);
  Accumulate(FMetadata.SourceDescription, FEncoded);
  Orders := FHOrder + FROrder;
  for I := 0 to N - 1 do
  begin
    CheckInteger(FRoles[I].SourceVoiceIndex, 0, FScore.VoiceCount - 1, 'source voice');
    CheckInteger(FRoles[I].Order, 1, WFC_TRAINING_MAX_ORDER, 'voice order');
    Orders := Add(Orders, FRoles[I].Order, WFC_TRAINING_MAX_VISIT_COUNT, 'orders');
    for J := 0 to I - 1 do
      if (FRoles[I].Id = FRoles[J].Id) or
        (FRoles[I].SourceVoiceIndex = FRoles[J].SourceVoiceIndex) then
        TrainingError('role ids and source voice indices must be distinct');
  end;
  Frames := 0;
  for I := 0 to High(FSelections) do
  begin
    CheckInteger(FSelections[I].StartTick, 0, High(Integer), 'excerpt start');
    CheckInteger(FSelections[I].LengthTicks, 1, High(Integer), 'excerpt length');
    Finish := Add(FSelections[I].StartTick, FSelections[I].LengthTicks,
      High(Integer), 'excerpt end');
    if Finish > FScore.LengthTicks then TrainingError('excerpt extends beyond score');
    if (FSelections[I].StartTick mod FQuantum <> 0) or
      (FSelections[I].LengthTicks mod FQuantum <> 0) then
      TrainingError('excerpt is not quantum aligned');
    Count := FSelections[I].LengthTicks div FQuantum;
    CheckInteger(Count, 1, WFC_TRAINING_MAX_DIMENSION, 'excerpt cells');
    if (Count < FHOrder) or (Count < FROrder) then
      TrainingError('excerpt is shorter than a provider order');
    for J := 0 to N - 1 do
      if Count < FRoles[J].Order then TrainingError('excerpt is shorter than a voice order');
    for J := 0 to I - 1 do
      if FSelections[I].Name = FSelections[J].Name then TrainingError('duplicate excerpt name');
    Accumulate(FSelections[I].Name, FEncoded);
    Frames := Add(Frames, Count, WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT, 'frames');
  end;
  Product(Frames, Docs, WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT, 'tokens');
  BaseEncoded := FEncoded;
  FEncoded := Product(BaseEncoded, Docs,
    WFC_TRAINING_MAX_TOTAL_ENCODED_TOKEN_LENGTH, 'encoded document metadata');
  for I := 0 to N - 1 do
  begin
    Accumulate(FRoles[I].Id, FEncoded);
    Voice := FScore.VoiceAt(FRoles[I].SourceVoiceIndex);
    Track := FScore.TrackAt(Voice.TrackIndex);
    Accumulate(Voice.Id, FEncoded); Accumulate(Track.Id, FEncoded);
  end;

  { N source walks here; N+2 inside the retained document adapters. }
  FWork := Product(FScore.SpanCount, 2 * N + 2,
    WFC_TRAINING_MAX_VISIT_COUNT, 'source inspection');
  FWork := Add(FWork, Product(N, N, WFC_TRAINING_MAX_VISIT_COUNT,
    'role identity comparisons'), WFC_TRAINING_MAX_VISIT_COUNT, 'work');
  FWork := Add(FWork, Product(Length(FSelections), Length(FSelections),
    WFC_TRAINING_MAX_VISIT_COUNT, 'excerpt identity comparisons'),
    WFC_TRAINING_MAX_VISIT_COUNT, 'work');
  FWork := Add(FWork, Product(Frames, Orders, WFC_TRAINING_MAX_VISIT_COUNT,
    'sequence histories'), WFC_TRAINING_MAX_VISIT_COUNT, 'work');
  FWork := Add(FWork, Product(Product(Frames, N,
    WFC_TRAINING_MAX_VISIT_COUNT, 'voice cells'), 7,
    WFC_TRAINING_MAX_VISIT_COUNT, 'voice observations'),
    WFC_TRAINING_MAX_VISIT_COUNT, 'work');
  SetLength(FFirst, N); SetLength(FLast, N);
  SelectedSpans := 0;
  for I := 0 to N - 1 do
  begin
    FFirst[I] := -1; FLast[I] := -1;
    for J := 0 to FScore.SpanCount - 1 do
    begin
      Span := FScore.SpanAt(J);
      if Span.VoiceIndex <> FRoles[I].SourceVoiceIndex then Continue;
      if FFirst[I] < 0 then FFirst[I] := J;
      FLast[I] := J;
    end;
    if FFirst[I] < 0 then TrainingError('source voice has no span partition');
    SelectedSpans := Add(SelectedSpans, FLast[I] - FFirst[I] + 1,
      WFC_TRAINING_MAX_VISIT_COUNT, 'selected spans');
  end;
  { One edge scan and one content walk here, plus three walks for each shared
    document and each singleton document: eleven per selected role. }
  FWork := Add(FWork, Product(Product(SelectedSpans, Length(FSelections),
    WFC_TRAINING_MAX_VISIT_COUNT, 'excerpt inspection'), 11,
    WFC_TRAINING_MAX_VISIT_COUNT, 'excerpt walks'),
    WFC_TRAINING_MAX_VISIT_COUNT, 'work');
  Tones := 0;
  for I := 0 to High(FSelections) do
  begin
    Finish := FSelections[I].StartTick + FSelections[I].LengthTicks;
    for J := 0 to N - 1 do
      for K := FFirst[J] to FLast[J] do
      begin
        Span := FScore.SpanAt(K); SpanEnd := Span.StartTick + Span.DurationTicks;
        if (Span.StartTick >= Finish) or (SpanEnd <= FSelections[I].StartTick) then Continue;
        if Span.Kind = wmskRest then
        begin
          if ((Span.StartTick > FSelections[I].StartTick) and
            (Span.StartTick mod FQuantum <> 0)) or
            ((SpanEnd < Finish) and (SpanEnd mod FQuantum <> 0)) then
            TrainingError('retained rest edge is not quantum aligned');
        end
        else
        begin
          if (Span.StartTick < FSelections[I].StartTick) or (SpanEnd > Finish) then
            TrainingError('common excerpt cuts a sounded span');
          if (Span.StartTick mod FQuantum <> 0) or
            (Span.DurationTicks mod FQuantum <> 0) then
            TrainingError('sound is not quantum aligned');
          Tones := Add(Tones, Product(Span.DurationTicks div FQuantum,
            Length(Span.Tones), WFC_TRAINING_MAX_VISIT_COUNT, 'tone observations'),
            WFC_TRAINING_MAX_VISIT_COUNT, 'tone observations');
        end;
      end;
  end;
  FWork := Add(FWork, Product(Tones, 7, WFC_TRAINING_MAX_VISIT_COUNT,
    'tone walks'), WFC_TRAINING_MAX_VISIT_COUNT, 'work');
end;

procedure TBundleBuilder.CheckProjectedContent;
var I, V, T, Cell, Tick, Count, EncodedLength: Integer;
  Cursors: TIntegers; Spans: TWfcMusicSpanEvents;
  Frame, Single: TWfcMusicEnsembleFrame; PitchSet: TWfcMusicPitchClassSet;
begin
  SetLength(Cursors, Length(FRoles)); SetLength(Spans, Length(FRoles));
  SetLength(Frame.Voices, Length(FRoles)); SetLength(Single.Voices, 1);
  for I := 0 to High(FSelections) do
  begin
    for V := 0 to High(FRoles) do
    begin
      Cursors[V] := FFirst[V]; Spans[V] := FScore.SpanAt(Cursors[V]);
      while Spans[V].StartTick + Spans[V].DurationTicks <= FSelections[I].StartTick do
      begin Inc(Cursors[V]); Spans[V] := FScore.SpanAt(Cursors[V]); end;
    end;
    Count := FSelections[I].LengthTicks div FQuantum;
    for Cell := 0 to Count - 1 do
    begin
      Tick := FSelections[I].StartTick + Cell * FQuantum;
      for V := 0 to High(FRoles) do
      begin
        if Tick >= Spans[V].StartTick + Spans[V].DurationTicks then
        begin Inc(Cursors[V]); Spans[V] := FScore.SpanAt(Cursors[V]); end;
        if Spans[V].Kind = wmskRest then Frame.Voices[V].Action := wmcaRest
        else if Tick = Spans[V].StartTick then Frame.Voices[V].Action := wmcaAttack
        else Frame.Voices[V].Action := wmcaHold;
        Frame.Voices[V].Tones := Spans[V].Tones;
        EncodedLength := 12; { wme1:1:r, after escaping colons }
        if Frame.Voices[V].Action <> wmcaRest then
        begin
          EncodedLength := Add(EncodedLength, 3 + Digits(Length(Spans[V].Tones)),
            WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH, 'single voice token');
          for T := 0 to High(Spans[V].Tones) do
            EncodedLength := Add(EncodedLength, 6 + Digits(Spans[V].Tones[T].Pitch) +
              Digits(Spans[V].Tones[T].Velocity), WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH,
              'single voice token');
        end;
        Single.Voices[0] := Frame.Voices[V];
        Accumulate(EncodeWfcMusicEnsembleFrame(Single), FEncoded);
      end;
      EncodedLength := Add(8 + Digits(Length(FRoles)), Length(FRoles) * 4,
        WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH, 'rhythm token');
      Accumulate(EncodeWfcMusicRhythmFrame(ProjectWfcMusicEnsembleFrameToRhythm(Frame)), FEncoded);
      PitchSet := ProjectWfcMusicEnsembleFrameToPitchClassSet(Frame, FScore.StepsPerOctave);
      EncodedLength := 11 + Digits(PitchSet.StepsPerOctave) + Digits(Length(PitchSet.PitchClasses));
      for T := 0 to High(PitchSet.PitchClasses) do
        EncodedLength := Add(EncodedLength, 3 + Digits(PitchSet.PitchClasses[T]),
          WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH, 'harmony token');
      Accumulate(EncodeWfcMusicPitchClassSet(PitchSet), FEncoded);
    end;
  end;
end;

function TBundleBuilder.Build(const AScore: TWfcMusicScore;
  const ARoles: TWfcMusicVoiceTrainingRoles;
  const ASelections: TWfcMusicEnsembleTrainingSelections;
  const AQuantumTicks, AHarmonyOrder, ARhythmOrder: Integer;
  const AMetadata: TWfcTrainingMetadata): TWfcMusicVoicesTrainingBundle;
var B: TWfcMusicVoicesTrainingBundle; All, One: TWfcMusicEnsembleTrainingVoiceIndices;
  I: Integer; Voice: TWfcMusicVoice;
begin
  if AScore = nil then TrainingError('score cannot be nil');
  CheckInteger(AQuantumTicks, 1, High(Integer), 'quantum');
  CheckInteger(AHarmonyOrder, 1, WFC_TRAINING_MAX_ORDER, 'harmony order');
  CheckInteger(ARhythmOrder, 1, WFC_TRAINING_MAX_ORDER, 'rhythm order');
  CheckInteger(Length(ARoles), 1, WFC_TRAINING_MAX_SAMPLE_COUNT - 2, 'role count');
  CheckInteger(Length(ASelections), 1, WFC_TRAINING_MAX_SAMPLE_COUNT, 'selection count');
  FScore := AScore; FRoles := ARoles; FSelections := ASelections;
  FQuantum := AQuantumTicks; FHOrder := AHarmonyOrder; FROrder := ARhythmOrder;
  FMetadata := AMetadata;
  Preflight;
  CheckProjectedContent;
  B := TWfcMusicVoicesTrainingBundle.Create;
  try
    B.FQuantumTicks := FQuantum; B.FTicksPerQuarter := FScore.TicksPerQuarter;
    B.FStepsPerOctave := FScore.StepsPerOctave; B.FMetadata := FMetadata;
    SetLength(B.FSelections, Length(FSelections));
    for I := 0 to High(FSelections) do B.FSelections[I] := FSelections[I];
    SetLength(B.FProvenance, Length(FRoles));
    SetLength(All, Length(FRoles)); SetLength(One, 1);
    for I := 0 to High(FRoles) do
    begin
      All[I] := FRoles[I].SourceVoiceIndex;
      B.FProvenance[I].Role := FRoles[I]; Voice := FScore.VoiceAt(All[I]);
      B.FProvenance[I].SourceVoiceId := Voice.Id;
      B.FProvenance[I].SourceTrackIndex := Voice.TrackIndex;
      B.FProvenance[I].SourceTrackId := FScore.TrackAt(Voice.TrackIndex).Id;
    end;
    SetLength(B.FDocuments, Length(FRoles) + 2);
    B.FDocuments[0] := BuildWfcMusicEnsembleTrainingDocument(FScore, All,
      FSelections, wmetpExactPitchClassSet, FQuantum, FHOrder, FMetadata);
    B.FDocuments[1] := BuildWfcMusicEnsembleTrainingDocument(FScore, All,
      FSelections, wmetpRhythmVector, FQuantum, FROrder, FMetadata);
    for I := 0 to High(FRoles) do
    begin
      One[0] := All[I];
      B.FDocuments[I + 2] := BuildWfcMusicEnsembleTrainingDocument(FScore, One,
        FSelections, wmetpFrame, FQuantum, FRoles[I].Order, FMetadata);
    end;
    Result := B; B := nil;
  finally B.Free; end;
end;

destructor TWfcMusicVoicesTrainingBundle.Destroy;
var I: Integer;
begin
  for I := 0 to High(FDocuments) do FDocuments[I].Free;
  inherited Destroy;
end;

function TWfcMusicVoicesTrainingBundle.GetRoleCount: Integer;
begin Result := Length(FProvenance); end;

procedure TWfcMusicVoicesTrainingBundle.CheckRole(const AIndex: Integer);
begin CheckInteger(AIndex, 0, RoleCount - 1, 'role index'); end;

function TWfcMusicVoicesTrainingBundle.RoleAt(const AIndex: Integer): TWfcMusicVoiceTrainingProvenance;
begin CheckRole(AIndex); Result := FProvenance[AIndex]; end;

function TWfcMusicVoicesTrainingBundle.CopySelections: TWfcMusicEnsembleTrainingSelections;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(FSelections));
  for I := 0 to High(FSelections) do Result[I] := FSelections[I];
end;

function TWfcMusicVoicesTrainingBundle.CopyMetadata: TWfcTrainingMetadata;
begin Result := FMetadata; end;

function TWfcMusicVoicesTrainingBundle.CopyDocument(const AIndex: Integer): TWfcTrainingDocument;
begin
  if AIndex >= Length(FDocuments) then TrainingError('bundle is not initialized');
  Result := TWfcTrainingDocument.Create(FDocuments[AIndex].CopyMetadata,
    FDocuments[AIndex].CopyOptions, FDocuments[AIndex].CopySamples);
end;

function TWfcMusicVoicesTrainingBundle.CopyHarmonyDocument: TWfcTrainingDocument;
begin Result := CopyDocument(0); end;

function TWfcMusicVoicesTrainingBundle.CopyRhythmDocument: TWfcTrainingDocument;
begin Result := CopyDocument(1); end;

function TWfcMusicVoicesTrainingBundle.CopyVoiceDocument(const AIndex: Integer): TWfcTrainingDocument;
begin CheckRole(AIndex); Result := CopyDocument(AIndex + 2); end;

function BuildWfcMusicVoicesTrainingBundle(const AScore: TWfcMusicScore;
  const ARoles: TWfcMusicVoiceTrainingRoles;
  const ASelections: TWfcMusicEnsembleTrainingSelections;
  const AQuantumTicks, AHarmonyOrder, ARhythmOrder: Integer;
  const AMetadata: TWfcTrainingMetadata): TWfcMusicVoicesTrainingBundle;
var Builder: TBundleBuilder;
begin
  Result := nil; Builder := TBundleBuilder.Create;
  try
    try
      Result := Builder.Build(AScore, ARoles, ASelections, AQuantumTicks,
        AHarmonyOrder, ARhythmOrder, AMetadata);
    except
      on E: EWfcMusicVoicesTraining do raise;
      on E: EWfcMusic do TrainingError(E.Message);
      on E: EWfcTraining do TrainingError(E.Message);
      on E: EConvertError do TrainingError(E.Message);
      on E: ERangeError do TrainingError(E.Message);
    end;
  finally Builder.Free; end;
end;

end.
