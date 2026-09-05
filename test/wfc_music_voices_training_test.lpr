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
program wfc_music_voices_training_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc_model, wfc_music, wfc_music_sequence, wfc_music_ensemble,
  wfc_music_ensemble_training, wfc_music_voices_training, wfc_training,
  wfc_training_text, wfc_sequence, wfc_sequence_text, voice_studio_corpus;

type TTest = procedure;
var Checks, Failures: Integer;

procedure Check(const AValue: Boolean; const ALabel: String);
begin
  Inc(Checks);
  if AValue then WriteLn('  [PASS] ', ALabel)
  else begin Inc(Failures); WriteLn('  [FAIL] ', ALabel); end;
end;

procedure Run(const ALabel: String; ATest: TTest);
begin
  WriteLn(ALabel);
  try ATest; except on E: Exception do Check(False, E.ClassName + ': ' + E.Message); end;
end;

function Metadata: TWfcTrainingMetadata;
begin Result := MakeWfcTrainingMetadata('independent voices', 'MIT', 'project-owned exact fixtures'); end;

function Selection(const AStart, ALength: Integer): TWfcMusicEnsembleTrainingSelections;
begin
  Result := nil; SetLength(Result, 1);
  Result[0] := MakeWfcMusicEnsembleTrainingSelection('common', AStart, ALength);
end;

function Roles(const ACount, AOrder: Integer): TWfcMusicVoiceTrainingRoles;
var I: Integer;
begin
  Result := nil; SetLength(Result, ACount);
  for I := 0 to High(Result) do
  begin Result[I].Id := 'role-' + IntToStr(I); Result[I].SourceVoiceIndex := I;
    Result[I].Order := AOrder; end;
end;

function ScoreOf(const AVoiceCount, ALength, ASteps: Integer;
  const ASpans: TWfcMusicSpanEvents): TWfcMusicScore;
var Tracks: TWfcMusicTracks; Voices: TWfcMusicVoices;
  Meters: TWfcMusicMeterChanges; Tempos: TWfcMusicTempoChanges; I: Integer;
begin
  SetLength(Tracks, 1); Tracks[0] := MakeWfcMusicTrack('source', 'Source');
  SetLength(Voices, AVoiceCount);
  for I := 0 to High(Voices) do Voices[I] := MakeWfcMusicVoice(0, 'voice-' + IntToStr(I));
  SetLength(Meters, 1); Meters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(Tempos, 1); Tempos[0] := MakeWfcMusicTempoChange(0, 500000);
  Result := TWfcMusicScore.Create(1, ASteps, ALength, Tracks, Voices, Meters, Tempos, ASpans);
end;

function RestScore(const AVoiceCount, ALength: Integer): TWfcMusicScore;
var Spans: TWfcMusicSpanEvents; I: Integer;
begin
  SetLength(Spans, AVoiceCount);
  for I := 0 to High(Spans) do Spans[I] := MakeWfcMusicRest(I, 0, ALength);
  Result := ScoreOf(AVoiceCount, ALength, 12, Spans);
end;

function ChordScore(const ALength, AToneCount: Integer): TWfcMusicScore;
var Spans: TWfcMusicSpanEvents; Tones: TWfcMusicTones; I: Integer;
begin
  SetLength(Tones, AToneCount);
  for I := 0 to High(Tones) do Tones[I] := MakeWfcMusicTone(I, 80);
  SetLength(Spans, 1); Spans[0] := MakeWfcMusicSound(0, 0, ALength, Tones);
  Result := ScoreOf(1, ALength, AToneCount + 1, Spans);
end;

function Rejected(const AScore: TWfcMusicScore; const ARoles: TWfcMusicVoiceTrainingRoles;
  const ASelections: TWfcMusicEnsembleTrainingSelections;
  const AQuantum, AHOrder, AROrder: Integer; const AMetadata: TWfcTrainingMetadata;
  const AMessagePart: String = ''): Boolean;
var B: TWfcMusicVoicesTrainingBundle;
begin
  B := nil; Result := False;
  try
    try B := BuildWfcMusicVoicesTrainingBundle(AScore, ARoles, ASelections,
      AQuantum, AHOrder, AROrder, AMetadata);
    except on E: EWfcMusicVoicesTraining do
      Result := (B = nil) and ((AMessagePart = '') or (Pos(AMessagePart, E.Message) > 0)); end;
  finally B.Free; end;
end;

procedure TestDocumentsAndDetachment;
var Score: TWfcMusicScore; B: TWfcMusicVoicesTrainingBundle;
  R: TWfcMusicVoiceTrainingRoles; S, CopyS: TWfcMusicEnsembleTrainingSelections;
  M: TWfcTrainingMetadata; P: TWfcMusicVoiceTrainingProvenance;
  D, Expected, Again: TWfcTrainingDocument; Indices: TWfcMusicEnsembleTrainingVoiceIndices;
  I, J, K, Total: Integer; Samples: TWfcTrainingSamples; Frame: TWfcMusicEnsembleFrame;
  Text: String;
begin
  Score := BuildVoiceStudioScore; B := nil; D := nil; Expected := nil; Again := nil;
  try
    R := VoiceStudioRoles; S := VoiceStudioSelections; M := Metadata;
    B := BuildWfcMusicVoicesTrainingBundle(Score, R, S, 240, 8, 8, M);
    Check((B.RoleCount = 3) and (B.QuantumTicks = 240) and
      (B.TicksPerQuarter = 480) and (B.StepsPerOctave = 12), 'bundle timing and stable role count');
    Total := 0;
    for I := 0 to 4 do
    begin
      if I < 2 then
      begin
        SetLength(Indices, 3); for J := 0 to 2 do Indices[J] := J;
        if I = 0 then
        begin D := B.CopyHarmonyDocument; Expected := BuildWfcMusicEnsembleTrainingDocument(
          Score, Indices, S, wmetpExactPitchClassSet, 240, 8, M); end
        else begin D := B.CopyRhythmDocument; Expected := BuildWfcMusicEnsembleTrainingDocument(
          Score, Indices, S, wmetpRhythmVector, 240, 8, M); end;
      end
      else
      begin
        SetLength(Indices, 1); Indices[0] := I - 2; D := B.CopyVoiceDocument(I - 2);
        Expected := BuildWfcMusicEnsembleTrainingDocument(Score, Indices, S, wmetpFrame, 240, 2, M);
      end;
      Check(EncodeWfcTrainingText(D) = EncodeWfcTrainingText(Expected),
        'document matches independent existing projection ' + IntToStr(I));
      Check((D.SampleCount = 2) and (D.TotalTokenCount = 32), 'common two-excerpt cardinality ' + IntToStr(I));
      Inc(Total, D.TotalTokenCount);
      if I >= 2 then
        for J := 0 to 1 do
        begin
          Samples := D.CopySamples;
          for K := 0 to 15 do
          begin
            Frame := DecodeWfcMusicEnsembleFrame(Samples[J].Tokens[K]);
            Check(Length(Frame.Voices) = 1, 'role emits singleton frame');
            if (I = 3) and ((K mod 8) <> 7) then
              Check(Length(Frame.Voices[0].Tones) = 2, 'chord role preserves full tone set');
          end;
        end;
      WriteLn('    document ', I, ' signature ', IntToHex(D.Signature, 8));
      FreeAndNil(D); FreeAndNil(Expected);
    end;
    Check(Total = 160, 'five document token counts aggregate to160');
    P := B.RoleAt(1);
    Check((P.Role.Id = 'chord') and (P.Role.SourceVoiceIndex = 1) and
      (P.Role.Order = 2) and (P.SourceVoiceId = 'chord') and
      (P.SourceTrackId = 'voice-studio') and (P.SourceTrackIndex = 0), 'source identity stays detached from token content');
    P.Role.Id := 'changed'; R[1].Id := 'changed'; R[1].SourceVoiceIndex := 0;
    S[0].Name := 'changed'; S[0].LengthTicks := 240; M.Name := 'changed';
    FreeAndNil(Score);
    CopyS := B.CopySelections; CopyS[0].Name := 'also changed';
    Check((B.RoleAt(1).Role.Id = 'chord') and (B.CopySelections[0].Name = 'excerpt-a') and
      (B.CopyMetadata.Name = 'independent voices'), 'all borrowed inputs and accessor records detached after score freed');
    D := B.CopyVoiceDocument(1); Text := EncodeWfcTrainingText(D);
    Samples := D.CopySamples; Samples[0].Tokens[0] := 'changed'; FreeAndNil(D);
    Again := B.CopyVoiceDocument(1);
    Check(EncodeWfcTrainingText(Again) = Text, 'copied documents are independently owned and immutable');
    FreeAndNil(B);
    Check(Again.TotalTokenCount = 32, 'document outlives bundle');
  finally Again.Free; Expected.Free; D.Free; B.Free; Score.Free; end;
end;

procedure TestMalformedAndCommonCuts;
var Score: TWfcMusicScore; R: TWfcMusicVoiceTrainingRoles;
  S: TWfcMusicEnsembleTrainingSelections; M: TWfcTrainingMetadata; I: Integer;
  B: TWfcMusicVoicesTrainingBundle; D: TWfcTrainingDocument; F: TWfcMusicEnsembleFrame;
begin
  Score := BuildVoiceStudioScore;
  try
    R := VoiceStudioRoles; S := VoiceStudioSelections;
    Check(Rejected(nil, R, S, 240, 8, 8, Metadata), 'nil source rejects without publication');
    Check(Rejected(Score, nil, S, 240, 8, 8, Metadata), 'empty roles reject');
    Check(Rejected(Score, R, nil, 240, 8, 8, Metadata), 'empty common excerpts reject');
    for I := 0 to 7 do
    begin
      R := VoiceStudioRoles;
      case I of
        0: R[1].Id := R[0].Id;
        1: R[1].Id := '';
        2: R[1].SourceVoiceIndex := 0;
        3: R[1].SourceVoiceIndex := -1;
        4: R[1].SourceVoiceIndex := 3;
        5: R[1].Order := 0;
        6: R[1].Order := 65;
        7: R[1].Order := 17;
      end;
      Check(Rejected(Score, R, S, 240, 8, 8, Metadata), 'malformed role ' + IntToStr(I));
    end;
    R := VoiceStudioRoles;
    Check(Rejected(Score, R, S, 0, 8, 8, Metadata), 'zero quantum rejects');
    Check(Rejected(Score, R, S, 240, 0, 8, Metadata), 'zero harmony order rejects');
    Check(Rejected(Score, R, S, 240, 8, 65, Metadata), 'oversized rhythm order rejects');
    for I := 0 to 6 do
    begin
      S := VoiceStudioSelections;
      case I of
        0: S[1].Name := S[0].Name;
        1: S[0].Name := '';
        2: S[0].StartTick := -1;
        3: S[0].LengthTicks := 0;
        4: begin S[0].StartTick := High(Integer); S[0].LengthTicks := 1; end;
        5: S[0].LengthTicks := 7920;
        6: S[0].StartTick := 1;
      end;
      Check(Rejected(Score, R, S, 240, 8, 8, Metadata), 'malformed excerpt ' + IntToStr(I));
    end;
    Check(Rejected(Score, R, Selection(480, 1440), 240, 1, 1, Metadata,
      'cuts a sounded span'), 'common start rejects held bass/chord even when upper reattacks');
    Check(Rejected(Score, R, Selection(0, 480), 240, 1, 1, Metadata,
      'cuts a sounded span'), 'common end cannot shorten a sustained chord');
    M := Metadata; M.LicenseIdentifier := '';
    Check(Rejected(Score, R, VoiceStudioSelections, 240, 8, 8, M), 'invalid metadata preflight');
  finally Score.Free; end;
  Score := RestScore(2, 16); B := nil; D := nil;
  try
    B := BuildWfcMusicVoicesTrainingBundle(Score, Roles(2, 1), Selection(2, 5), 1, 1, 1, Metadata);
    D := B.CopyVoiceDocument(1); F := DecodeWfcMusicEnsembleFrame(D.SampleAt(0).Tokens[0]);
    Check((D.TotalTokenCount = 5) and (F.Voices[0].Action = wmcaRest),
      'common rest-only cropping is exact and need not be a complete measure');
  finally D.Free; B.Free; Score.Free; end;
end;

procedure TestAggregateGuards;
var Score: TWfcMusicScore; R: TWfcMusicVoiceTrainingRoles;
  M: TWfcTrainingMetadata; Indices: TWfcMusicEnsembleTrainingVoiceIndices;
  D: TWfcTrainingDocument; S: TWfcMusicEnsembleTrainingSelections; I: Integer;
begin
  Score := RestScore(1, 22000); D := nil;
  try
    R := Roles(1, 1); SetLength(Indices, 1); Indices[0] := 0;
    D := BuildWfcMusicEnsembleTrainingDocument(Score, Indices, Selection(0, 22000),
      wmetpFrame, 1, 1, Metadata);
    Check(D.TotalTokenCount = 22000, 'one22000-token document is individually valid');
    Check(Rejected(Score, R, Selection(0, 22000), 1, 1, 1, Metadata, 'tokens'),
      'three individually valid documents exceed aggregate65536-token budget');
  finally D.Free; Score.Free; end;
  Score := RestScore(22, 4);
  try
    M.Name := StringOfChar('n', 60000); M.LicenseIdentifier := StringOfChar('l', 60000);
    M.SourceDescription := StringOfChar('s', 60000);
    Check(Rejected(Score, Roles(22, 1), Selection(0, 4), 1, 1, 1, M, 'encoded'),
      'repeated document metadata participates in aggregate encoded guard');
  finally Score.Free; end;
  Score := ChordScore(256, 1200); D := nil;
  try
    D := BuildWfcMusicEnsembleTrainingDocument(Score, Indices, Selection(0, 256),
      wmetpFrame, 1, 1, Metadata);
    Check(D.TotalTokenCount = 256, 'large singleton chord document fits its own encoded bound');
    FreeAndNil(D);
    D := BuildWfcMusicEnsembleTrainingDocument(Score, Indices, Selection(0, 256),
      wmetpExactPitchClassSet, 1, 1, Metadata);
    Check(D.TotalTokenCount = 256, 'large pitchclass document independently fits');
    Check(Rejected(Score, Roles(1, 1), Selection(0, 256), 1, 1, 1, Metadata, 'encoded'),
      'combined chord and harmony content exceeds aggregate bound before publication');
  finally D.Free; Score.Free; end;
  Score := ChordScore(4096, 600);
  try
    Check(Rejected(Score, Roles(1, 1), Selection(0, 4096), 1, 1, 1, Metadata, 'tone walks'),
      'repeated chord observations have a genuine aggregate work guard');
  finally Score.Free; end;
  Score := ChordScore(4, 7000);
  try
    Check(Rejected(Score, Roles(1, 1), Selection(0, 4), 1, 1, 1, Metadata, 'single voice token'),
      'oversized chord token is rejected before canonical concatenation');
  finally Score.Free; end;
  Score := RestScore(1, 4);
  try
    SetLength(S, 1366);
    for I := 0 to High(S) do
      S[I] := MakeWfcMusicEnsembleTrainingSelection('sample-' + IntToStr(I), 0, 4);
    Check(Rejected(Score, Roles(1, 1), S, 1, 1, 1, Metadata, 'samples'),
      'shared excerpts count once per document against aggregate sample bound');
  finally Score.Free; end;
end;

procedure TestNovelRecombinations;
var Mask, Cell, V, T, ValidExact, NovelExact: Integer; Choices: array[0..2] of Integer;
  Frames, A, B: TWfcMusicEnsembleFrames; Actual, Expected: TWfcMusicPitchClassSet;
  Exact, Allowed, InRange, Smooth, Novel: Boolean; Original: TWfcModelToken;
begin
  A := VoiceStudioCorpus(0); B := VoiceStudioCorpus(1);
  Check((Length(A) = 16) and (Length(B) = 16), 'two independently authored sixteen-cell excerpts');
  ValidExact := 0; NovelExact := 0;
  for Mask := 0 to 7 do
  begin
    for V := 0 to 2 do Choices[V] := (Mask shr V) and 1;
    Frames := VoiceStudioRecombination(Choices);
    ValidateWfcMusicEnsembleFrames(Frames);
    Exact := True; Allowed := True; InRange := True; Smooth := True;
    for Cell := 0 to 15 do
    begin
      Actual := ProjectWfcMusicEnsembleFrameToPitchClassSet(Frames[Cell], 12);
      Expected := ProjectWfcMusicEnsembleFrameToPitchClassSet(A[Cell], 12);
      Exact := Exact and WfcMusicPitchClassSetsEqual(Actual, Expected);
      Allowed := Allowed and WfcMusicPitchClassSetIsSubset(Actual, Expected);
      for V := 0 to 2 do
        for T := 0 to High(Frames[Cell].Voices[V].Tones) do
        begin
          InRange := InRange and (Frames[Cell].Voices[V].Tones[T].Pitch >= VoiceStudioRoleMinimumPitch(V)) and
            (Frames[Cell].Voices[V].Tones[T].Pitch <= VoiceStudioRoleMaximumPitch(V));
          if (Cell > 0) and (Length(Frames[Cell - 1].Voices[V].Tones) > T) then
            Smooth := Smooth and (Abs(Frames[Cell].Voices[V].Tones[T].Pitch -
              Frames[Cell - 1].Voices[V].Tones[T].Pitch) <= 2);
        end;
    end;
    Novel := not VoiceStudioVerticalWasObserved(Frames[0]);
    if Exact then begin Inc(ValidExact); if Novel then Inc(NovelExact); end;
    Check(Exact = not ((Mask = 4) or (Mask = 3)), 'independent exact coverage truth table mask' + IntToStr(Mask));
    Check(Allowed, 'allowed harmony admits every partial-coverage mixture mask' + IntToStr(Mask));
    Check(InRange and Smooth, 'role ranges and bounded sorted-tone motion mask' + IntToStr(Mask));
    Check((Frames[2].Voices[0].Action = wmcaHold) and
      (Frames[2].Voices[1].Action = wmcaHold) and (Frames[2].Voices[2].Action = wmcaAttack),
      'held bass and complete chord coexist with upper reattack mask' + IntToStr(Mask));
  end;
  Check((ValidExact = 6) and (NovelExact = 4), 'six exact combinations include four truly novel verticals');
  Frames := VoiceStudioRecombination([0, 1, 1]);
  Check(not VoiceStudioVerticalWasObserved(Frames[0]) and
    not VoiceStudioVerticalWasObserved(Frames[4]) and VoiceStudioVerticalWasObserved(Frames[7]),
    'named witness novel in both sounding harmonies; silence is not novel');
  Original := EncodeWfcMusicEnsembleFrame(A[0]);
  Frames := VoiceStudioCorpus(0); Frames[0].Voices[0].Tones[0].Pitch := 0;
  Check(EncodeWfcMusicEnsembleFrame(VoiceStudioCorpus(0)[0]) = Original,
    'corpus arrays are fresh across calls');
  Frames := VoiceStudioCorpus(0); Frames[0].Voices[0].Action := wmcaHold;
  Frames[0].Voices[0].Tones[0].Velocity := 1;
  Check(VoiceStudioVerticalWasObserved(Frames[0]), 'action/velocity changes cannot manufacture novelty');
end;

function AllEdgesValid(const AModel: TWfcSequenceModel): Boolean;
var I, J: Integer; A, B: TWfcMusicEnsembleFrame;
begin
  Result := True;
  for I := 0 to AModel.StateCount - 1 do
  begin
    A := DecodeWfcMusicEnsembleFrame(AModel.PublicTokenAt(AModel.StateEmittedTokenIndexAt(I)));
    if (AModel.StartCountAt(I) > 0) and not WfcMusicEnsembleFrameCanStart(A) then Exit(False);
    for J := 0 to AModel.StateCount - 1 do
      if AModel.StatesCompatible(I, J) then
      begin
        B := DecodeWfcMusicEnsembleFrame(AModel.PublicTokenAt(AModel.StateEmittedTokenIndexAt(J)));
        if not WfcMusicEnsembleFrameCanFollow(A, B) then Exit(False);
      end;
  end;
end;

procedure TestModelSemantics;
var H, R, Model: TWfcSequenceModel; Voices: TWfcMusicVoiceSequenceModels;
  B: TWfcMusicVoicesTrainingBundle; D: TWfcTrainingDocument; Score: TWfcMusicScore;
  Roles1: TWfcMusicVoiceTrainingRoles; I: Integer; Text: String;
  Spans: TWfcMusicSpanEvents; Tones: TWfcMusicTones;
begin
  H := nil; R := nil; Voices := nil;
  try
    BuildVoiceStudioModels(H, R, Voices);
    Check((H <> nil) and (R <> nil) and (Length(Voices) = 3), 'model helper transfers all five owned models');
    for I := 0 to 2 do
      Check(AllEdgesValid(Voices[I]), 'authored role model has valid observed starts and every latent edge ' + IntToStr(I));
    Text := EncodeWfcSequenceText(Voices[1]);
    Model := DecodeWfcSequenceText(Text);
    try Check(EncodeWfcSequenceText(Model) = Text, 'role model roundtrip uses existing format unchanged');
    finally Model.Free; end;
  finally H.Free; R.Free; for I := 0 to High(Voices) do Voices[I].Free; end;
  Score := RestScore(1, 4); B := nil; D := nil; Model := nil;
  try
    B := BuildWfcMusicVoicesTrainingBundle(Score, Roles(1, 1), Selection(0, 4), 1, 1, 1, Metadata);
    D := B.CopyVoiceDocument(0); Model := DecodeWfcSequenceText(LearnWfcTrainingModelText(D));
    Check(AllEdgesValid(Model), 'order1 rest-only training is valid, not blanket-banned');
  finally Model.Free; D.Free; B.Free; Score.Free; end;
  SetLength(Tones, 1); Tones[0] := MakeWfcMusicTone(60, 90);
  SetLength(Spans, 4);
  Spans[0] := MakeWfcMusicSound(0, 0, 1, Tones);
  Spans[1] := MakeWfcMusicRest(0, 1, 1);
  Tones[0].Pitch := 62;
  Spans[2] := MakeWfcMusicSound(0, 2, 1, Tones);
  Spans[3] := MakeWfcMusicRest(0, 3, 1);
  Score := ScoreOf(1, 4, 12, Spans); B := nil; D := nil; Model := nil;
  try
    B := BuildWfcMusicVoicesTrainingBundle(Score, Roles(1, 1), Selection(0, 4), 1, 1, 1, Metadata);
    D := B.CopyVoiceDocument(0); Model := DecodeWfcSequenceText(LearnWfcTrainingModelText(D));
    Check(AllEdgesValid(Model) and (Model.PublicTokenCount = 3),
      'order1 rest/attack-only vocabulary remains valid with multiple pitches');
  finally Model.Free; D.Free; B.Free; Score.Free; end;
  Score := BuildVoiceStudioScore; B := nil; D := nil; Model := nil;
  try
    Roles1 := VoiceStudioRoles; Roles1[0].Order := 1;
    B := BuildWfcMusicVoicesTrainingBundle(Score, Roles1, VoiceStudioSelections, 240, 8, 8, Metadata);
    D := B.CopyVoiceDocument(0); Model := DecodeWfcSequenceText(LearnWfcTrainingModelText(D));
    Check(not AllEdgesValid(Model), 'training is honest: order1 hold vocabulary needs consumer edge validation');
  finally Model.Free; D.Free; B.Free; Score.Free; end;
end;

procedure TestRoleOrderingAndAccessors;
var Score: TWfcMusicScore; B: TWfcMusicVoicesTrainingBundle; D: TWfcTrainingDocument;
  R: TWfcMusicVoiceTrainingRoles; Temp: TWfcMusicVoiceTrainingRole;
  F: TWfcMusicEnsembleFrame; Rhythm: TWfcMusicRhythmFrame; Rej: Boolean; I: Integer;
begin
  Score := BuildVoiceStudioScore; B := nil; D := nil;
  try
    R := VoiceStudioRoles; Temp := R[0]; R[0] := R[2]; R[2] := Temp;
    B := BuildWfcMusicVoicesTrainingBundle(Score, R, VoiceStudioSelections, 240, 8, 8, Metadata);
    D := B.CopyVoiceDocument(0);
    F := DecodeWfcMusicEnsembleFrame(D.SampleAt(0).Tokens[2]);
    Check((B.RoleAt(0).Role.Id = 'upper') and (B.RoleAt(0).Role.SourceVoiceIndex = 2) and
      (F.Voices[0].Action = wmcaAttack) and (F.Voices[0].Tones[0].Pitch = 79),
      'declared role order, not numeric source order, identifies singleton documents');
    FreeAndNil(D); D := B.CopyRhythmDocument;
    Rhythm := DecodeWfcMusicRhythmFrame(D.SampleAt(0).Tokens[2]);
    Check((Rhythm.Actions[0] = wmcaAttack) and (Rhythm.Actions[1] = wmcaHold) and
      (Rhythm.Actions[2] = wmcaHold), 'rhythm slots follow the same explicit role order');
    for I := -1 to 3 do
      if (I = -1) or (I = 3) then
      begin
        Rej := False; try B.RoleAt(I); except on EWfcMusicVoicesTraining do Rej := True; end;
        Check(Rej, 'provenance accessor rejects out-of-range role');
        Rej := False; try B.CopyVoiceDocument(I); except on EWfcMusicVoicesTraining do Rej := True; end;
        Check(Rej, 'document accessor rejects out-of-range role');
      end;
    Rej := False; try VoiceStudioCorpus(-1); except on EWfcMusicVoicesTraining do Rej := True; end;
    Check(Rej, 'negative corpus index rejects');
    Rej := False; try VoiceStudioCorpus(2); except on EWfcMusicVoicesTraining do Rej := True; end;
    Check(Rej, 'unknown corpus index rejects');
    Rej := False; try VoiceStudioRecombination([0, 1]); except on EWfcMusicVoicesTraining do Rej := True; end;
    Check(Rej, 'recombination requires exactly three role choices');
    Rej := False; try VoiceStudioRecombination([0, 2, 0]); except on EWfcMusicVoicesTraining do Rej := True; end;
    Check(Rej, 'recombination rejects unknown excerpt choices');
  finally D.Free; B.Free; Score.Free; end;
end;

{$IFDEF PAS2JS}
function BadNumber(const AIndex: Integer): Integer;
begin
  asm
    if (AIndex === 0) Result = NaN;
    else if (AIndex === 1) Result = Infinity;
    else if (AIndex === 2) Result = 0.5;
    else Result = 2147483648;
  end;
end;

procedure TestBrowserNumbers;
var Score: TWfcMusicScore; R: TWfcMusicVoiceTrainingRoles;
  S: TWfcMusicEnsembleTrainingSelections; I, J, Q, H, Rhythm: Integer;
  B: TWfcMusicVoicesTrainingBundle; Rej: Boolean;
begin
  Score := BuildVoiceStudioScore; B := nil;
  try
    for I := 0 to 3 do for J := 0 to 6 do
    begin
      R := VoiceStudioRoles; S := VoiceStudioSelections; Q := 240; H := 8; Rhythm := 8;
      case J of
        0: Q := BadNumber(I); 1: H := BadNumber(I); 2: Rhythm := BadNumber(I);
        3: R[0].Order := BadNumber(I); 4: R[0].SourceVoiceIndex := BadNumber(I);
        5: S[0].StartTick := BadNumber(I); 6: S[0].LengthTicks := BadNumber(I);
      end;
      Check(Rejected(Score, R, S, Q, H, Rhythm, Metadata), 'browser exact integer preflight ' + IntToStr(I) + '/' + IntToStr(J));
    end;
    B := BuildVoiceStudioTrainingBundle;
    for I := 0 to 3 do
    begin
      Rej := False; try B.RoleAt(BadNumber(I)); except on EWfcMusicVoicesTraining do Rej := True; end;
      Check(Rej, 'browser malformed provenance index');
      Rej := False; try VoiceStudioCorpus(BadNumber(I)); except on EWfcMusicVoicesTraining do Rej := True; end;
      Check(Rej, 'browser malformed corpus index');
    end;
  finally B.Free; Score.Free; end;
end;
{$ENDIF}

begin
  WriteLn('WFC independent voice training conformance');
  Run('shared document parity and detached provenance', @TestDocumentsAndDetachment);
  Run('role validation and exact common excerpts', @TestMalformedAndCommonCuts);
  Run('whole-bundle resource preflight', @TestAggregateGuards);
  Run('independent six-of-eight recombination proof', @TestNovelRecombinations);
  Run('model temporal semantics and order1 distinction', @TestModelSemantics);
  Run('declared role order and accessor boundaries', @TestRoleOrderingAndAccessors);
  {$IFDEF PAS2JS}Run('browser malformed numeric boundaries', @TestBrowserNumbers);{$ENDIF}
  WriteLn(Checks, ' checks, ', Failures, ' failures');
  if Failures <> 0 then Halt(1);
end.
