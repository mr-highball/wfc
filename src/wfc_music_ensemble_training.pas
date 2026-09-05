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
unit wfc_music_ensemble_training;

{$mode delphi}{$H+}

interface

uses
  wfc_model, wfc_music, wfc_music_ensemble, wfc_training;

const
  WFC_MUSIC_ENSEMBLE_TRAINING_VERSION = 1;

type
  EWfcMusicEnsembleTraining = class(EWfcMusicEnsemble);

  TWfcMusicEnsembleTrainingVoiceIndices = array of Integer;
  TWfcMusicEnsembleTrainingProjection = (
    wmetpFrame,
    wmetpRhythmVector,
    wmetpExactPitchClassSet
  );
  TWfcMusicEnsembleTrainingSelection = record
    Name: TWfcModelToken;
    StartTick: Integer;
    LengthTicks: Integer;
  end;
  TWfcMusicEnsembleTrainingSelections =
    array of TWfcMusicEnsembleTrainingSelection;

function MakeWfcMusicEnsembleTrainingSelection(const AName: TWfcModelToken;
  const AStartTick, ALengthTicks: Integer): TWfcMusicEnsembleTrainingSelection;

{ One ordered, nonempty, distinct voice vector applies to every independently
  named common excerpt. Frame slot I always means AVoiceIndices[I], including
  rest-only voices; original score ids are not inserted into learned tokens.
  The caller retains the selection vector and import report as provenance.

  Excerpts need quantum alignment, not complete measures. Sounded spans may
  not cross either excerpt edge; only silence can be cropped. Each sample
  contains at least AOrder frames. Metadata and sample order are preserved.
  The immutable returned document is caller-owned and detached from inputs.

  This adapter uses the existing training document and work limits, including
  expanded voice cells and repeated tone observations. It checks expansion and
  encoded content before allocating complete sample-token arrays. Score is an
  already validated immutable object, not an untrusted parser input: SpanAt
  necessarily makes a temporary deep copy before its tones can be inspected.
  Only selected excerpts are expanded; no intermediate excerpt score or
  full-score frame matrix is constructed. }
function BuildWfcMusicEnsembleTrainingDocument(const AScore: TWfcMusicScore;
  const AVoiceIndices: TWfcMusicEnsembleTrainingVoiceIndices;
  const ASelections: TWfcMusicEnsembleTrainingSelections;
  const AProjection: TWfcMusicEnsembleTrainingProjection;
  const AQuantumTicks, AOrder: Integer;
  const AMetadata: TWfcTrainingMetadata): TWfcTrainingDocument;

implementation

uses
  SysUtils, wfc_music_sequence, wfc_text_codec;

type
  TVoiceInfo = record
    SourceIndex, Slot, FirstSpan, LastSpan: Integer;
  end;
  TVoiceInfos = array of TVoiceInfo;
  TIntegers = array of Integer;

  TEnsembleTrainingBuilder = class
  private
    FScore: TWfcMusicScore;
    FSelections: TWfcMusicEnsembleTrainingSelections;
    FProjection: TWfcMusicEnsembleTrainingProjection;
    FQuantum, FOrder, FInitialEncodedLength: Integer;
    FMetadata: TWfcTrainingMetadata;
    FVoices: TVoiceInfos;
    FSamples: TWfcTrainingSamples;
    procedure PrepareVoices(const AIndices: TWfcMusicEnsembleTrainingVoiceIndices);
    procedure Preflight;
    procedure WalkSamples(const AEmit: Boolean);
    function FrameToken(const AFrame: TWfcMusicEnsembleFrame): TWfcModelToken;
  public
    function Build(const AScore: TWfcMusicScore;
      const AVoiceIndices: TWfcMusicEnsembleTrainingVoiceIndices;
      const ASelections: TWfcMusicEnsembleTrainingSelections;
      const AProjection: TWfcMusicEnsembleTrainingProjection;
      const AQuantumTicks, AOrder: Integer;
      const AMetadata: TWfcTrainingMetadata): TWfcTrainingDocument;
  end;

procedure TrainingError(const AMessage: String);
begin
  raise EWfcMusicEnsembleTraining.Create('ensemble training adapter: ' + AMessage);
end;

function CheckedAdd(const A, B, AMaximum: Integer;
  const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) or (A > AMaximum) or (B > AMaximum - A) then
    TrainingError(ALabel + ' exceeds the existing training limit');
  Result := A + B;
end;

function CheckedProduct(const A, B, AMaximum: Integer;
  const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) or ((A <> 0) and (B > AMaximum div A)) then
    TrainingError(ALabel + ' exceeds the existing training limit');
  Result := A * B;
end;

procedure AccumulateToken(const AToken: TWfcModelToken; const ALabel: String;
  var ATotal: Integer);
var Encoded: String;
begin
  if (Length(AToken) = 0) or
    (Length(AToken) > WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH) or
    not WfcModelTokenIsValid(AToken) then
    TrainingError(ALabel + ' must be a nonempty valid token within the training limit');
  Encoded := WfcTextEncodeToken(AToken, 'WFC ensemble training');
  if Length(Encoded) > WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH then
    TrainingError(ALabel + ' exceeds the encoded-token training limit');
  ATotal := CheckedAdd(ATotal, Length(Encoded),
    WFC_TRAINING_MAX_TOTAL_ENCODED_TOKEN_LENGTH, 'aggregate encoded content');
end;

function MakeWfcMusicEnsembleTrainingSelection(const AName: TWfcModelToken;
  const AStartTick, ALengthTicks: Integer): TWfcMusicEnsembleTrainingSelection;
begin
  Result.Name := AName;
  Result.StartTick := AStartTick;
  Result.LengthTicks := ALengthTicks;
end;

function SelectionEnd(const ASelection: TWfcMusicEnsembleTrainingSelection): Integer;
begin
  if (ASelection.StartTick < 0) or (ASelection.LengthTicks < 1) then
    TrainingError('selection start must be nonnegative and length positive');
  if ASelection.StartTick > High(Integer) - ASelection.LengthTicks then
    TrainingError('selection end exceeds the Integer range');
  Result := ASelection.StartTick + ASelection.LengthTicks;
end;

function Digits(const AValue: Integer): Integer;
var N: Integer;
begin
  N := AValue;
  Result := 1;
  while N >= 10 do begin Inc(Result); N := N div 10; end;
end;

procedure AddEncodedLength(var ALength: Integer; const AExtra: Integer);
begin
  ALength := CheckedAdd(ALength, AExtra,
    WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH, 'projected encoded token');
end;

procedure SortVoices(var AVoices: TVoiceInfos);
var Buffer: TVoiceInfos; Width, Base, Middle, Finish, L, R, OutIndex: Integer;
begin
  SetLength(Buffer, Length(AVoices));
  Width := 1;
  while Width < Length(AVoices) do
  begin
    Base := 0;
    while Base < Length(AVoices) do
    begin
      Middle := Base + Width;
      if Middle > Length(AVoices) then Middle := Length(AVoices);
      Finish := Middle + Width;
      if Finish > Length(AVoices) then Finish := Length(AVoices);
      L := Base; R := Middle;
      for OutIndex := Base to Finish - 1 do
        if (L < Middle) and ((R >= Finish) or
          (AVoices[L].SourceIndex <= AVoices[R].SourceIndex)) then
        begin Buffer[OutIndex] := AVoices[L]; Inc(L); end
        else begin Buffer[OutIndex] := AVoices[R]; Inc(R); end;
      Base := Finish;
    end;
    for OutIndex := 0 to High(AVoices) do AVoices[OutIndex] := Buffer[OutIndex];
    Width := Width * 2;
  end;
end;

procedure TEnsembleTrainingBuilder.PrepareVoices(
  const AIndices: TWfcMusicEnsembleTrainingVoiceIndices);
var I, V: Integer; Span: TWfcMusicSpanEvent;
begin
  if (Length(AIndices) = 0) or
    (Length(AIndices) > WFC_TRAINING_MAX_DIMENSION) then
    TrainingError('selected voice count is outside the training dimension limit');
  for I := 0 to High(AIndices) do
    if (AIndices[I] < 0) or (AIndices[I] >= FScore.VoiceCount) then
      TrainingError('selected voice index is out of bounds');
  if FScore.SpanCount > WFC_TRAINING_MAX_VISIT_COUNT then
    TrainingError('source span inspection exceeds the training visit limit');
  SetLength(FVoices, Length(AIndices));
  for I := 0 to High(AIndices) do
  begin
    FVoices[I].SourceIndex := AIndices[I];
    FVoices[I].Slot := I;
    FVoices[I].FirstSpan := -1;
    FVoices[I].LastSpan := -1;
  end;
  SortVoices(FVoices);
  for I := 1 to High(FVoices) do
    if FVoices[I].SourceIndex = FVoices[I - 1].SourceIndex then
      TrainingError('selected voice indices must be distinct');
  V := 0;
  for I := 0 to FScore.SpanCount - 1 do
  begin
    Span := FScore.SpanAt(I);
    while (V < Length(FVoices)) and
      (FVoices[V].SourceIndex < Span.VoiceIndex) do Inc(V);
    if V = Length(FVoices) then Break;
    if FVoices[V].SourceIndex = Span.VoiceIndex then
    begin
      if FVoices[V].FirstSpan < 0 then FVoices[V].FirstSpan := I;
      FVoices[V].LastSpan := I;
    end;
  end;
  for V := 0 to High(FVoices) do
    if FVoices[V].FirstSpan < 0 then
      TrainingError('selected voice does not have a canonical span partition');
end;

procedure TEnsembleTrainingBuilder.Preflight;
var I, J, V, K, Finish, SpanEnd, Count, TotalFrames, VoiceCells,
  ToneCopies, SelectedSpans, Visits: Integer; Span: TWfcMusicSpanEvent;
begin
  FInitialEncodedLength := 0;
  AccumulateToken(FMetadata.Name, 'metadata name', FInitialEncodedLength);
  AccumulateToken(FMetadata.LicenseIdentifier, 'metadata license', FInitialEncodedLength);
  AccumulateToken(FMetadata.SourceDescription, 'metadata source', FInitialEncodedLength);
  SelectedSpans := 0;
  for V := 0 to High(FVoices) do
    SelectedSpans := CheckedAdd(SelectedSpans,
      FVoices[V].LastSpan - FVoices[V].FirstSpan + 1,
      WFC_TRAINING_MAX_VISIT_COUNT, 'selected span inspection');
  Visits := CheckedProduct(SelectedSpans, Length(FSelections),
    WFC_TRAINING_MAX_VISIT_COUNT, 'excerpt span inspection');
  Visits := CheckedProduct(Visits, 3, WFC_TRAINING_MAX_VISIT_COUNT,
    'preflight and extraction span inspection');
  CheckedAdd(Visits, FScore.SpanCount, WFC_TRAINING_MAX_VISIT_COUNT,
    'source and excerpt span inspection');
  TotalFrames := 0; VoiceCells := 0; ToneCopies := 0;
  for I := 0 to High(FSelections) do
  begin
    AccumulateToken(FSelections[I].Name, 'selection name', FInitialEncodedLength);
    for J := 0 to I - 1 do
      if FSelections[I].Name = FSelections[J].Name then
        TrainingError('selection names must be unique');
    Finish := SelectionEnd(FSelections[I]);
    if Finish > FScore.LengthTicks then TrainingError('selection extends beyond the score');
    if ((FSelections[I].StartTick mod FQuantum) <> 0) or
      ((FSelections[I].LengthTicks mod FQuantum) <> 0) then
      TrainingError('selection is not aligned to the quantum');
    Count := FSelections[I].LengthTicks div FQuantum;
    if Count < FOrder then TrainingError('selection has fewer frames than the order');
    if Count > WFC_TRAINING_MAX_DIMENSION then
      TrainingError('selection frame count exceeds the training dimension limit');
    TotalFrames := CheckedAdd(TotalFrames, Count,
      WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT, 'aggregate frame count');
    VoiceCells := CheckedAdd(VoiceCells, CheckedProduct(Count, Length(FVoices),
      WFC_TRAINING_MAX_VISIT_COUNT, 'expanded voice cells'),
      WFC_TRAINING_MAX_VISIT_COUNT, 'aggregate expanded voice cells');
    for V := 0 to High(FVoices) do
      for K := FVoices[V].FirstSpan to FVoices[V].LastSpan do
      begin
        Span := FScore.SpanAt(K);
        SpanEnd := Span.StartTick + Span.DurationTicks;
        if (Span.StartTick >= Finish) or
          (SpanEnd <= FSelections[I].StartTick) then Continue;
        if Span.Kind = wmskRest then
        begin
          if ((Span.StartTick > FSelections[I].StartTick) and
            ((Span.StartTick mod FQuantum) <> 0)) or
            ((SpanEnd < Finish) and ((SpanEnd mod FQuantum) <> 0)) then
            TrainingError('retained rest boundary is not aligned to the quantum');
        end
        else
        begin
          if (Span.StartTick < FSelections[I].StartTick) or (SpanEnd > Finish) then
            TrainingError(Format('selection %d cuts a sounded span in source voice %d',
              [I, FVoices[V].SourceIndex]));
          if ((Span.StartTick mod FQuantum) <> 0) or
            ((Span.DurationTicks mod FQuantum) <> 0) then
            TrainingError('selected sounded span is not aligned to the quantum');
          ToneCopies := CheckedAdd(ToneCopies,
            CheckedProduct(Span.DurationTicks div FQuantum, Length(Span.Tones),
              WFC_TRAINING_MAX_VISIT_COUNT, 'expanded tone copies'),
            WFC_TRAINING_MAX_VISIT_COUNT, 'aggregate expanded tone copies');
        end;
      end;
  end;
  CheckedProduct(TotalFrames, FOrder, WFC_TRAINING_MAX_VISIT_COUNT,
    'sequence history visits');
end;

function TEnsembleTrainingBuilder.FrameToken(
  const AFrame: TWfcMusicEnsembleFrame): TWfcModelToken;
var V, T, EncodedLength: Integer; PitchSet: TWfcMusicPitchClassSet;
begin
  { Colons encode as three bytes in wfclearn. Count before concatenating
    canonical frame/set tokens, so oversized typed chords cannot build an
    oversized intermediate token. The exact shared codec is checked again. }
  case FProjection of
    wmetpFrame:
      begin
        EncodedLength := 7 + Digits(Length(AFrame.Voices));
        for V := 0 to High(AFrame.Voices) do
        begin
          AddEncodedLength(EncodedLength, 4);
          if AFrame.Voices[V].Action = wmcaRest then Continue;
          AddEncodedLength(EncodedLength, 3 + Digits(Length(AFrame.Voices[V].Tones)));
          for T := 0 to High(AFrame.Voices[V].Tones) do
            AddEncodedLength(EncodedLength, 6 +
              Digits(AFrame.Voices[V].Tones[T].Pitch) +
              Digits(AFrame.Voices[V].Tones[T].Velocity));
        end;
        Result := EncodeWfcMusicEnsembleFrame(AFrame);
      end;
    wmetpRhythmVector:
      begin
        EncodedLength := 8 + Digits(Length(AFrame.Voices));
        AddEncodedLength(EncodedLength, Length(AFrame.Voices) * 4);
        Result := EncodeWfcMusicRhythmFrame(ProjectWfcMusicEnsembleFrameToRhythm(AFrame));
      end;
    wmetpExactPitchClassSet:
      begin
        PitchSet := ProjectWfcMusicEnsembleFrameToPitchClassSet(AFrame, FScore.StepsPerOctave);
        EncodedLength := 11 + Digits(PitchSet.StepsPerOctave) +
          Digits(Length(PitchSet.PitchClasses));
        for T := 0 to High(PitchSet.PitchClasses) do
          AddEncodedLength(EncodedLength, 3 + Digits(PitchSet.PitchClasses[T]));
        Result := EncodeWfcMusicPitchClassSet(PitchSet);
      end;
  else
    TrainingError('unknown projection');
    Result := '';
  end;
end;

procedure TEnsembleTrainingBuilder.WalkSamples(const AEmit: Boolean);
var I, V, Slot, Cell, Tick, Count, EncodedTotal: Integer;
  Cursors: TIntegers; Spans: TWfcMusicSpanEvents;
  Frame: TWfcMusicEnsembleFrame; Token: TWfcModelToken;
begin
  EncodedTotal := FInitialEncodedLength;
  SetLength(Cursors, Length(FVoices));
  SetLength(Spans, Length(FVoices));
  SetLength(Frame.Voices, Length(FVoices));
  for I := 0 to High(FSelections) do
  begin
    for V := 0 to High(FVoices) do
    begin
      Slot := FVoices[V].Slot;
      Cursors[Slot] := FVoices[V].FirstSpan;
      Spans[Slot] := FScore.SpanAt(Cursors[Slot]);
      while Spans[Slot].StartTick + Spans[Slot].DurationTicks <= FSelections[I].StartTick do
      begin
        Inc(Cursors[Slot]);
        Spans[Slot] := FScore.SpanAt(Cursors[Slot]);
      end;
    end;
    Count := FSelections[I].LengthTicks div FQuantum;
    for Cell := 0 to Count - 1 do
    begin
      Tick := FSelections[I].StartTick + Cell * FQuantum;
      for Slot := 0 to High(Spans) do
      begin
        if Tick >= Spans[Slot].StartTick + Spans[Slot].DurationTicks then
        begin
          Inc(Cursors[Slot]);
          Spans[Slot] := FScore.SpanAt(Cursors[Slot]);
        end;
        if Spans[Slot].Kind = wmskRest then Frame.Voices[Slot].Action := wmcaRest
        else if Tick = Spans[Slot].StartTick then Frame.Voices[Slot].Action := wmcaAttack
        else Frame.Voices[Slot].Action := wmcaHold;
        { These private, read-only aliases point only to detached SpanAt
          snapshots. No frame arrays escape; output is canonical text. }
        Frame.Voices[Slot].Tones := Spans[Slot].Tones;
      end;
      Token := FrameToken(Frame);
      if AEmit then FSamples[I].Tokens[Cell] := Token
      else AccumulateToken(Token, 'projected frame', EncodedTotal);
    end;
  end;
end;

function TEnsembleTrainingBuilder.Build(const AScore: TWfcMusicScore;
  const AVoiceIndices: TWfcMusicEnsembleTrainingVoiceIndices;
  const ASelections: TWfcMusicEnsembleTrainingSelections;
  const AProjection: TWfcMusicEnsembleTrainingProjection;
  const AQuantumTicks, AOrder: Integer;
  const AMetadata: TWfcTrainingMetadata): TWfcTrainingDocument;
var I: Integer;
begin
  if not Assigned(AScore) then TrainingError('score cannot be nil');
  if AQuantumTicks < 1 then TrainingError('quantum must be positive');
  if (AOrder < 1) or (AOrder > WFC_TRAINING_MAX_ORDER) then
    TrainingError('order is outside the training limit');
  if (Length(ASelections) = 0) or
    (Length(ASelections) > WFC_TRAINING_MAX_SAMPLE_COUNT) then
    TrainingError('selection count is outside the training limit');
  case AProjection of
    wmetpFrame, wmetpRhythmVector, wmetpExactPitchClassSet: ;
  else TrainingError('unknown projection'); end;
  FScore := AScore;
  FSelections := ASelections;
  FProjection := AProjection;
  FQuantum := AQuantumTicks;
  FOrder := AOrder;
  FMetadata := AMetadata;
  PrepareVoices(AVoiceIndices);
  Preflight;
  WalkSamples(False);
  SetLength(FSamples, Length(FSelections));
  for I := 0 to High(FSamples) do
  begin
    FSamples[I].Name := FSelections[I].Name;
    FSamples[I].Width := FSelections[I].LengthTicks div FQuantum;
    FSamples[I].Height := 1;
    SetLength(FSamples[I].Tokens, FSamples[I].Width);
  end;
  WalkSamples(True);
  Result := TWfcTrainingDocument.Create(FMetadata,
    MakeWfcTrainingOptions(wtkSequence, wmbOpen, wmsNone, 0, 0, FOrder), FSamples);
end;

function BuildWfcMusicEnsembleTrainingDocument(const AScore: TWfcMusicScore;
  const AVoiceIndices: TWfcMusicEnsembleTrainingVoiceIndices;
  const ASelections: TWfcMusicEnsembleTrainingSelections;
  const AProjection: TWfcMusicEnsembleTrainingProjection;
  const AQuantumTicks, AOrder: Integer;
  const AMetadata: TWfcTrainingMetadata): TWfcTrainingDocument;
var Builder: TEnsembleTrainingBuilder;
begin
  Result := nil;
  Builder := TEnsembleTrainingBuilder.Create;
  try
    try
      Result := Builder.Build(AScore, AVoiceIndices, ASelections, AProjection,
        AQuantumTicks, AOrder, AMetadata);
    except
      on E: EWfcMusicEnsembleTraining do raise;
      on E: EWfcMusic do TrainingError(E.Message);
      on E: EWfcTraining do TrainingError(E.Message);
      on E: EConvertError do TrainingError(E.Message);
      on E: ERangeError do TrainingError(E.Message);
    end;
  finally Builder.Free; end;
end;

end.
