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
program wfc_ensemble_profiles_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_sequence, wfc_music,
  wfc_music_sequence, wfc_music_ensemble, wfc_music_ensemble_graph,
  wfc_music_ensemble_passes, wfc_music_arrangement, wfc_music_form,
  ensemble_studio_profiles;

var Checks, Failures: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then begin Inc(Failures); WriteLn('FAIL: ', AMessage); end;
end;

function CorpusSignature(const AProfile: TEnsembleStudioProfile): String;
{$PUSH}{$Q-}
var I, J, K: Integer; H, V: Cardinal; F: TWfcMusicEnsembleFrames; S: String;
begin
  H := Cardinal(2166136261);
  for I := 0 to EnsembleStudioProfileCorpusCount(AProfile) - 1 do
  begin
    F := EnsembleStudioProfileCorpus(AProfile,I);
    for J := 0 to High(F) do
    begin
      S := EncodeWfcMusicEnsembleFrame(F[J]) + #10;
      for K := 1 to Length(S) do
      begin
        V := H xor Cardinal(Ord(S[K]));
        H := (V + (V shl 1) + (V shl 4) + (V shl 7) +
          (V shl 8) + (V shl 24)) and Cardinal($FFFFFFFF);
      end;
    end;
  end;
  Result := UpperCase(IntToHex(H,8));
end;
{$POP}

function BarFor(const C: TWfcMusicFormConfig; const ARealization: Integer): TWfcMusicFormBar;
var G: Integer;
begin
  Result := Default(TWfcMusicFormBar);
  Result.CellCount := 8;
  Result.RealizationIndex := ARealization;
  Result.HarmonyIndex := C.Realizations[ARealization].HarmonyIndex;
  G := C.Realizations[ARealization].GestureIndex;
  Result.GestureIndex := G;
  Result.HarmonicFunction := C.Harmonies[Result.HarmonyIndex].HarmonicFunction;
  Result.MotifIndex := C.Gestures[G].MotifIndex;
  if Result.MotifIndex = 0 then Result.Role := wmfrAnswer else Result.Role := wmfrContrast;
  if wmfcHalf in C.Gestures[G].Cadences then Result.Cadence := wmfcHalf
  else if wmfcAuthentic in C.Gestures[G].Cadences then Result.Cadence := wmfcAuthentic
  else Result.Cadence := wmfcNone;
end;

procedure TestCatalog;
var C: TWfcMusicFormConfig; B: TWfcMusicFormBar; F, Other: TWfcMusicEnsembleFrames;
  T, R, H: TWfcModelTokens; I, J, K, Attacks, Entry, Last: Integer;
  Why: String; Rejected: Boolean;
begin
  Check(EnsembleStudioProfileName(espStructuralV1) = 'structural-v1', 'stable structural name');
  Check(EnsembleStudioProfileName(espDevelopedPeriodV1) = 'developed-period-v1', 'stable developed name');
  Check(ParseEnsembleStudioProfile('structural-v1') = espStructuralV1, 'parse structural');
  Check(ParseEnsembleStudioProfile('developed-period-v1') = espDevelopedPeriodV1, 'parse developed');
  Rejected := False;
  try ParseEnsembleStudioProfile('developed'); except on E: Exception do Rejected := True; end;
  Check(Rejected, 'unversioned aliases are not silently accepted');
  Check(EnsembleStudioProfileOrder(espStructuralV1) = 8, 'legacy order eight');
  Check(EnsembleStudioProfileOrder(espDevelopedPeriodV1) = 2, 'developed one-predecessor history');
  Check(EnsembleStudioProfileCorpusCount(espStructuralV1) = 16, 'legacy sixteen samples');
  { Independently obtained from the pre-extraction ba9f3e0 implementation;
    every canonical frame token is followed by one LF, in corpus order. }
  Check(CorpusSignature(espStructuralV1) = 'B7F17A91', 'all original corpus bytes preserved');
  Check(CorpusSignature(espDevelopedPeriodV1) = '590DF2E5', 'developed version-one corpus fingerprint');
  for I := 0 to 15 do
  begin
    F := EnsembleStudioProfileCorpus(espStructuralV1, I);
    Check(Length(F) = 16, 'legacy two bars');
    ValidateWfcMusicEnsembleFrames(F);
    for J := 0 to 7 do
      Check(EncodeWfcMusicEnsembleFrame(F[J]) = EncodeWfcMusicEnsembleFrame(F[J+8]),
        'legacy repeated bar retained');
  end;
  C := EnsembleStudioDevelopedFormConfig(128, 4);
  Check(Length(C.Harmonies) = 5, 'five harmonic intents');
  Check(Length(C.Gestures) = 8, 'two related motif families and cadences');
  Check(Length(C.Realizations) = 24, 'bounded actual realizations');
  for I := 0 to High(C.Realizations) do
  begin
    B := BarFor(C, I);
    F := EnsembleStudioPlannedFrames(B);
    Check(Length(F) = 8, 'full authored bar');
    Check(ValidateEnsembleStudioPlannedFrames(B, F, Why), 'independent authored realization check');
    T := EnsembleStudioPlannedTokens(B, wmelEnsemble);
    R := EnsembleStudioPlannedTokens(B, wmelRhythm);
    H := EnsembleStudioPlannedTokens(B, wmelHarmony);
    Attacks := 0; Entry := -1; Last := -1;
    for J := 0 to 7 do
    begin
      Check(T[J] = EncodeWfcMusicEnsembleFrame(F[J]), 'exact ensemble token');
      Check(R[J] = EncodeWfcMusicRhythmFrame(ProjectWfcMusicEnsembleFrameToRhythm(F[J])),
        'exact rhythm projection');
      Check(H[J] = EncodeWfcMusicPitchClassSet(ProjectWfcMusicEnsembleFrameToPitchClassSet(F[J],12)),
        'exact SOUNDING pitch-class projection');
      if F[J].Voices[2].Action = wmcaAttack then
      begin
        Inc(Attacks); Last := F[J].Voices[2].Tones[0].Pitch;
        if Entry = -1 then Entry := Last;
      end;
    end;
    Check(Attacks = C.Gestures[B.GestureIndex].AttackCount, 'metadata upper attack count is recounted');
    Check(Entry = C.Realizations[I].EntryPitch, 'entry anchor is first actual attack');
    Check(Last = C.Realizations[I].ExitPitch, 'exit anchor survives terminal rest');
    Check(Length(DecodeWfcMusicPitchClassSet(H[7]).PitchClasses) = 0,
      'terminal silence does not claim harmonic-intent triad sounds');
    for J := 1 to 8 do
    begin
      B.CellCount := J;
      Other := EnsembleStudioPlannedFrames(B);
      Check(Length(Other) = J, 'partial bar never padded');
      Check(ValidateEnsembleStudioPlannedFrames(B, Other, Why), 'partial bar exact validation');
      for K := 0 to J - 1 do
        Check(EncodeWfcMusicEnsembleFrame(Other[K]) = T[K], 'partial bar is exact prefix');
    end;
    B.CellCount := 8;
    Other := EnsembleStudioPlannedFrames(B);
    Other[0].Voices[2].Tones[0].Velocity := 95;
    Check(not ValidateEnsembleStudioPlannedFrames(B, Other, Why), 'changed velocity rejected');
    Other := EnsembleStudioPlannedFrames(B);
    Other[0].Voices[2].Tones[0].Pitch := Other[0].Voices[2].Tones[0].Pitch + 1;
    Check(not ValidateEnsembleStudioPlannedFrames(B, Other, Why), 'unplanned pitch rejected');
    Other := EnsembleStudioPlannedFrames(B);
    for J := 0 to 6 do
      Other[J].Voices[2].Tones[0].Pitch := Other[J].Voices[2].Tones[0].Pitch + 1;
    ValidateWfcMusicEnsembleFrames(Other);
    Check(not ValidateEnsembleStudioPlannedFrames(B, Other, Why),
      'physically valid but unplanned melody rejected independently');
    B.MotifIndex := 1 - B.MotifIndex;
    Check(not ValidateEnsembleStudioPlannedFrames(B, F, Why), 'forged motif metadata rejected');
  end;
end;

procedure CheckPath(const M: TWfcSequenceModel; const T: TWfcModelTokens);
var Reach, Next: array of Boolean; I, J, K, Token: Integer; Found: Boolean;
begin
  SetLength(Reach, M.StateCount); SetLength(Next, M.StateCount);
  for I := 0 to High(T) do
  begin
    Token := M.FindPublicToken(T[I]);
    Check(Token >= 0, 'planned token was observed');
    Found := False;
    for J := 0 to M.StateCount - 1 do
    begin
      Next[J] := False;
      if M.StateEmittedTokenIndexAt(J) = Token then
      begin
        if I = 0 then Next[J] := M.StartCountAt(J) > 0
        else
          for K := 0 to M.StateCount - 1 do
            if Reach[K] and M.StatesCompatible(K,J) then begin Next[J] := True; Break; end;
      end;
      if Next[J] then Found := True;
    end;
    Check(Found, 'planned sequence has a genuine observed latent path');
    for J := 0 to High(Reach) do Reach[J] := Next[J];
  end;
end;

procedure TestModels;
const TOKEN_COUNTS: array[TWfcMusicEnsembleLayer] of Integer = (21,7,95);
  STATE_COUNTS: array[TWfcMusicEnsembleLayer] of Integer = (64,19,162);
var M: TWfcMusicEnsembleModels; C: TWfcMusicFormConfig; B: TWfcMusicFormBar;
  I, J, K, N, Previous, Current, PairCount: Integer;
  F, G: TWfcMusicEnsembleFrames; T: TWfcModelTokens; ObservedPairs: array of Boolean;
  Model: TWfcSequenceModel; Layer: TWfcMusicEnsembleLayer;
begin
  M := BuildEnsembleStudioProfileModels(espDevelopedPeriodV1);
  try
    ValidateWfcMusicEnsembleModel(M.Ensemble, 3);
    ValidateWfcMusicEnsembleRhythmModel(M.Rhythm, 3);
    ValidateWfcMusicEnsembleHarmonyModel(M.Harmony, 12);
    C := EnsembleStudioDevelopedFormConfig(128, 4);
    for Layer := Low(Layer) to High(Layer) do
    begin
      case Layer of wmelHarmony: Model := M.Harmony;
        wmelRhythm: Model := M.Rhythm; else Model := M.Ensemble; end;
      Check(Model.Order = 2, 'all developed layers use one predecessor');
      Check(Model.StateCount <= 1024, 'actual model fits existing state ceiling');
      Check(Model.PublicTokenCount = TOKEN_COUNTS[Layer], 'version-one public catalog count');
      Check(Model.StateCount = STATE_COUNTS[Layer], 'version-one latent catalog count');
      Check(Model.ObservationCount = 408, 'version-one observation count');
      Check(Model.SampleCount = 48, 'version-one sample count');
      WriteLn('Layer ', Ord(Layer), ': tokens=', Model.PublicTokenCount,
        ' states=', Model.StateCount, ' observations=', Model.ObservationCount,
        ' rank-one relation integers=', 4 * Model.StateCount * Model.StateCount);
      for I := 0 to High(C.Realizations) do
      begin
        B := BarFor(C, I); T := EnsembleStudioPlannedTokens(B, Layer);
        CheckPath(Model, T);
      end;
      SetLength(ObservedPairs, Model.PublicTokenCount * Model.PublicTokenCount);
      for I := 0 to High(ObservedPairs) do ObservedPairs[I] := False;
      PairCount := 0;
      for I := 0 to 47 do
      begin
        F := EnsembleStudioProfileCorpus(espDevelopedPeriodV1,I);
        case Layer of
          wmelHarmony: T := EncodeWfcMusicPitchClassSets(ProjectWfcMusicEnsembleFramesToPitchClassSets(F,12));
          wmelRhythm: T := EncodeWfcMusicRhythmFrames(ProjectWfcMusicEnsembleFramesToRhythm(F));
        else T := EncodeWfcMusicEnsembleFrames(F); end;
        for J := 1 to High(T) do
        begin
          Previous := Model.FindPublicToken(T[J-1]); Current := Model.FindPublicToken(T[J]);
          K := Previous * Model.PublicTokenCount + Current;
          if not ObservedPairs[K] then begin ObservedPairs[K] := True; Inc(PairCount); end;
        end;
      end;
      Check(PairCount < Length(ObservedPairs), 'training does not grant Cartesian public pairs');
      for I := 0 to Model.StateCount - 1 do
        if Model.StateLeadingBosCountAt(I) = 0 then
        begin
          Previous := Model.HistoryItemAt(I,0).TokenIndex;
          Current := Model.StateEmittedTokenIndexAt(I);
          Check(ObservedPairs[Previous * Model.PublicTokenCount + Current],
            'each non-BOS state witnesses an actually observed pair');
        end;
    end;
    { Independent all-pairs seam check; no artificial all-pairs training.
      Planner further restricts these physically legal transitions. }
    for I := 0 to High(C.Realizations) do
    begin
      F := EnsembleStudioPlannedFrames(BarFor(C,I));
      for J := 0 to High(C.Realizations) do
      begin
        G := EnsembleStudioPlannedFrames(BarFor(C,J));
        Check(WfcMusicEnsembleFrameCanFollow(F[7], G[0]), 'every bar seam preserves hold validity');
        SetLength(T, 16);
        for K := 0 to 7 do
        begin T[K] := EncodeWfcMusicEnsembleFrame(F[K]); T[K+8] := EncodeWfcMusicEnsembleFrame(G[K]); end;
        CheckPath(M.Ensemble, T);
      end;
    end;
    N := EnsembleStudioProfileCorpusCount(espDevelopedPeriodV1);
    Check(N = 48, 'bounded observed-fragment corpus');
    for I := 0 to N - 1 do
    begin
      F := EnsembleStudioProfileCorpus(espDevelopedPeriodV1, I);
      Check(Length(F) = 8 + I div 24, 'standalone or release-prefix fragment only');
    end;
  finally FreeEnsembleStudioProfileModels(M); end;
  Check((M.Harmony = nil) and (M.Rhythm = nil) and (M.Ensemble = nil), 'model free clears ownership');
end;

procedure TestForm;
var C: TWfcMusicFormConfig; Cursor: TWfcMusicFormCursor; Plan: TWfcMusicFormPhrasePlan;
  Report: TGraphNegotiationReport; B: TWfcMusicFormBar;
  F, Prior: TWfcMusicEnsembleFrames; I, Seed, Total, Bars, Changes, Half, Authentic: Integer;
  Step: TWfcMusicArrangementStep; Why: String; Seen: array[0..4] of Boolean;
begin
  for Seed := 1 to 8 do
    for Total := 1 to 17 do
    begin
      C := EnsembleStudioDevelopedFormConfig(Total * 8 - (Seed mod 8), Seed);
      Cursor := TWfcMusicFormCursor.Create(C);
      Plan := nil; Prior := nil; Bars := 0; Changes := 0; Half := 0; Authentic := 0;
      for I := 0 to 4 do Seen[I] := False;
      try
        repeat
          Step := Cursor.Next(Plan, Report);
          if Step = wmaspProduced then
          begin
            Check(Plan.BarCount <= 4, 'one bounded phrase retained');
            for I := 0 to Plan.BarCount - 1 do
            begin
              B := Plan.BarAt(I); Inc(Bars); Seen[B.HarmonyIndex] := True;
              F := EnsembleStudioPlannedFrames(B);
              Check(ValidateEnsembleStudioPlannedFrames(B,F,Why), 'planned sounding realization validates');
              if Length(Prior) > 0 then
              begin
                Check(WfcMusicEnsembleFrameCanFollow(Prior[High(Prior)],F[0]), 'planned bar boundary valid');
                if EncodeWfcMusicEnsembleFrame(Prior[0]) <> EncodeWfcMusicEnsembleFrame(F[0]) then Inc(Changes);
              end;
              Prior := F;
              if B.Cadence = wmfcHalf then begin Inc(Half); Check(B.HarmonicFunction = wmffDominant, 'half ends dominant'); end;
              if B.Cadence = wmfcAuthentic then begin Inc(Authentic); Check(B.HarmonicFunction = wmffTonic, 'authentic ends tonic'); end;
            end;
            FreeAndNil(Plan);
          end;
        until Step <> wmaspProduced;
        Check(Step = wmaspCompleted, 'developed form completes: ' + Cursor.Failure);
        Check(Bars = Total, 'exact requested bars including truncated last bar');
        Check(Authentic >= 1, 'finite form has authentic terminal intent');
        if Total >= 4 then Check(Changes >= 2, 'harmonic/gesture development is not a repeated bar');
        if Total >= 4 then Check(Seen[0] and Seen[4], 'longer form visits both tonic and dominant');
        if Total >= 8 then Check(Half >= 1, 'question phrase has half cadence');
      finally Plan.Free; Cursor.Free; end;
    end;
end;

{$IFDEF PAS2JS}
procedure TestBrowserNumbers;
var C: TWfcMusicFormConfig; B: TWfcMusicFormBar; F: TWfcMusicEnsembleFrames;
  I, X: Integer; Why: String; P: TEnsembleStudioProfile; Rejected: Boolean;
begin
  C := EnsembleStudioDevelopedFormConfig(8, 0);
  B := BarFor(C, 0);
  F := EnsembleStudioPlannedFrames(B);
  for I := 0 to 6 do
  begin
    case I of
      0: asm X = NaN; end;
      1: asm X = Infinity; end;
      2: asm X = -Infinity; end;
      3: asm X = 0.5; end;
      4: asm X = undefined; end;
      5: asm X = "0"; end;
      6: asm X = null; end;
    end;
    B := BarFor(C, 0); B.RealizationIndex := X;
    Check(not ValidateEnsembleStudioPlannedFrames(B,F,Why), 'browser malformed realization rejected');
    B := BarFor(C, 0); B.CellCount := X;
    Check(not ValidateEnsembleStudioPlannedFrames(B,F,Why), 'browser malformed cell count rejected');
    Rejected := False;
    try EnsembleStudioProfileCorpus(espDevelopedPeriodV1,X);
    except on E: Exception do Rejected := True; end;
    Check(Rejected, 'browser malformed corpus index rejected');
    P := TEnsembleStudioProfile(X);
    Rejected := False;
    try EnsembleStudioProfileName(P); except on E: Exception do Rejected := True; end;
    Check(Rejected, 'browser malformed profile rejected');
  end;
end;
{$ENDIF}

begin
  try
    TestCatalog;
    TestModels;
    TestForm;
    {$IFDEF PAS2JS}TestBrowserNumbers;{$ENDIF}
  except on E: Exception do begin Inc(Failures); WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message); end; end;
  WriteLn('Ensemble profile checks: ', Checks - Failures, '/', Checks);
  if Failures <> 0 then Halt(1);
end.
