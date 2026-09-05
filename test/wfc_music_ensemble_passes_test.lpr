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
program wfc_music_ensemble_passes_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_sequence, wfc_sequence_learn,
  wfc_sequence_graph, wfc_music, wfc_music_text, wfc_music_sequence,
  wfc_music_ensemble, wfc_music_ensemble_graph, wfc_music_ensemble_passes;

const Q = 240;
type
  TFixture = record
    A, B: TWfcMusicEnsembleFrames;
    HA, HB, RT, EA, EB: TWfcModelTokens;
    H, R, E: TWfcSequenceModel;
    Template: TWfcMusicScore;
  end;
  TTest = procedure;
var Checks, Failures: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then
  begin Inc(Failures); WriteLn('FAIL: ', AMessage); end;
end;

procedure Run(const AName: String; const ATest: TTest);
begin
  WriteLn('Test: ', AName);
  try ATest;
  except on E: Exception do
  begin Inc(Failures); WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message); end;
  end;
end;

function Samples(const A, B: TWfcModelTokens): TWfcSequenceSamples;
begin
  Result := nil; SetLength(Result, 2);
  Result[0] := MakeWfcSequenceSample(A);
  Result[1] := MakeWfcSequenceSample(B);
end;

function TokensEqual(const A, B: TWfcModelTokens): Boolean;
var I: Integer;
begin
  Result := False;
  if Length(A) <> Length(B) then Exit;
  for I := 0 to High(A) do if A[I] <> B[I] then Exit;
  Result := True;
end;

function IndicesEqual(const A: TGraphPassIndices;
  const B: array of Integer): Boolean;
var I: Integer;
begin
  Result := False;
  if Length(A) <> Length(B) then Exit;
  for I := 0 to High(A) do if A[I] <> B[I] then Exit;
  Result := True;
end;

function VoiceCells(const A: array of TWfcMusicVoiceCell): TWfcMusicVoiceCells;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(A));
  for I := 0 to High(A) do Result[I] := A[I];
end;

function Phrase(const AUpper: Integer): TWfcMusicEnsembleFrames;
var Bass, Lead: TWfcMusicTones; V: TWfcMusicVoiceCells;
begin
  Result := nil; SetLength(Result, 4);
  SetLength(Bass, 2);
  Bass[0] := MakeWfcMusicTone(48, 80); Bass[1] := MakeWfcMusicTone(55, 70);
  SetLength(Lead, 1); SetLength(V, 2);
  Lead[0] := MakeWfcMusicTone(AUpper, 90);
  V[0] := MakeWfcMusicVoiceCell(wmcaAttack, Bass);
  V[1] := MakeWfcMusicVoiceCell(wmcaAttack, Lead);
  Result[0] := MakeWfcMusicEnsembleFrame(V);
  Lead[0] := MakeWfcMusicTone(AUpper + 1, 91);
  V[0] := MakeWfcMusicVoiceCell(wmcaHold, Bass);
  V[1] := MakeWfcMusicVoiceCell(wmcaAttack, Lead);
  Result[1] := MakeWfcMusicEnsembleFrame(V);
  V[0] := MakeWfcMusicRestVoiceCell; V[1] := MakeWfcMusicRestVoiceCell;
  Result[2] := MakeWfcMusicEnsembleFrame(V);
  Lead[0] := MakeWfcMusicTone(AUpper, 90);
  V[0] := MakeWfcMusicVoiceCell(wmcaAttack, Bass);
  V[1] := MakeWfcMusicVoiceCell(wmcaAttack, Lead);
  Result[3] := MakeWfcMusicEnsembleFrame(V);
end;

function Template(const ACells: Integer): TWfcMusicScore;
var T: TWfcMusicTracks; V: TWfcMusicVoices;
  M: TWfcMusicMeterChanges; P: TWfcMusicTempoChanges;
  S: TWfcMusicSpanEvents;
begin
  SetLength(T, 2); T[0] := MakeWfcMusicTrack('bass', 'Held chord');
  T[1] := MakeWfcMusicTrack('lead', 'Independent attacks');
  SetLength(V, 2); V[0] := MakeWfcMusicVoice(0, 'bass');
  V[1] := MakeWfcMusicVoice(1, 'lead');
  SetLength(M, 1); M[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(P, 2); P[0] := MakeWfcMusicTempoChange(0, 500000);
  P[1] := MakeWfcMusicTempoChange(Q, 600000);
  SetLength(S, 2); S[0] := MakeWfcMusicRest(0, 0, ACells * Q);
  S[1] := MakeWfcMusicRest(1, 0, ACells * Q);
  Result := TWfcMusicScore.Create(60, 12, ACells * Q, T, V, M, P, S);
end;

procedure Init(out F: TFixture);
begin
  F := Default(TFixture);
  F.A := Phrase(64); F.B := Phrase(67);
  F.EA := EncodeWfcMusicEnsembleFrames(F.A);
  F.EB := EncodeWfcMusicEnsembleFrames(F.B);
  F.HA := EncodeWfcMusicPitchClassSets(
    ProjectWfcMusicEnsembleFramesToPitchClassSets(F.A, 12));
  F.HB := EncodeWfcMusicPitchClassSets(
    ProjectWfcMusicEnsembleFramesToPitchClassSets(F.B, 12));
  F.RT := EncodeWfcMusicRhythmFrames(ProjectWfcMusicEnsembleFramesToRhythm(F.A));
  F.E := LearnSequenceModelCorpus(Samples(F.EA, F.EB), 2);
  F.H := LearnSequenceModelCorpus(Samples(F.HA, F.HB), 4);
  F.R := LearnSequenceModel(F.RT, 4);
  F.Template := Template(4);
end;

procedure Done(var F: TFixture);
begin
  F.Template.Free; F.E.Free; F.H.Free; F.R.Free;
  F := Default(TFixture);
end;

function Config(const F: TFixture): TWfcMusicEnsembleConfig;
begin
  Result := DefaultWfcMusicEnsembleConfig(F.Template, Q, 0);
  Result.Models.Harmony := F.H; Result.Models.Rhythm := F.R;
  Result.Models.Ensemble := F.E;
end;

function Owner(const F: TFixture): TWfcMusicEnsemblePipeline;
begin Result := TWfcMusicEnsemblePipeline.Create(Config(F)); end;

procedure LockA(const P: TWfcMusicEnsemblePipeline; const F: TFixture);
begin
  P.IntersectLockedSpan(wmelHarmony, 0, F.HA);
  P.IntersectLockedSpan(wmelRhythm, 0, F.RT);
end;

function Fingerprint(const P: TWfcMusicEnsemblePipeline): String;
var L: TWfcMusicEnsembleLayer; G: TWfcGeneratedSequence;
  R: TWfcSequenceGraphValidationReport; I: Integer;
begin
  Result := '';
  for L := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
    if P.TryCopyCommittedLayer(L, G, R) then
    begin
      Result := Result + '|' + IntToStr(Ord(L));
      for I := 0 to High(G.Tokens) do
        Result := Result + ':' + IntToStr(G.StateIndices[I]) + '=' + G.Tokens[I];
    end
    else Result := Result + '!' + IntToStr(Ord(R.Issue.Kind));
end;

procedure TestGenerationAndCopies;
var F: TFixture; P: TWfcMusicEnsemblePipeline;
  C, C2: TWfcMusicEnsembleComposition; R: TWfcMusicEnsembleReport;
  V: TWfcMusicEnsembleValidationReport; S, S2: TWfcMusicScore;
  Frames, Fresh: TWfcMusicEnsembleFrames; Spans: TWfcMusicSpanEvents;
  G, G2: TWfcGeneratedSequence; H: TWfcMusicPitchClassSets;
  Rhythm: TWfcMusicRhythmFrames; TextBefore: String;
  Signature: TWfcMusicEnsembleCompositionSignature;
begin
  Init(F); P := Owner(F); C := nil; C2 := nil; S := nil; S2 := nil;
  try
    Check((P.CellCount = 4) and (P.QuantumTicks = Q) and
      (P.Extent = wseWhole) and (P.HarmonyMode = wmehmExact), 'explicit configuration');
    LockA(P, F);
    Check(P.TryGenerate(C, R), 'ordinary generation');
    if C = nil then Exit;
    Check((R.Status = wmesCompleted) and R.Validation.Valid, 'commit semantic validation');
    Check(C.HasLatentCapture and (C.CellCount = 4), 'complete latent capture');
    Check(P.Validate(C, V) and (V.CheckedLayers = 3), 'independent post-commit validation');
    Check(TokensEqual(C.CopyGenerated(wmelEnsemble).Tokens, F.EA), 'exact output tokens');
    S := C.CopyScore;
    Check((S.VoiceCount = 2) and (S.TrackCount = 2) and
      (S.TempoCount = 2) and (S.TempoAt(1).MicrosecondsPerQuarter = 600000),
      'all voice track and timing metadata preserved');
    Spans := S.CopyVoiceSpans(0);
    Check((Length(Spans) = 3) and (Spans[0].StartTick = 0) and
      (Spans[0].DurationTicks = 2 * Q) and (Length(Spans[0].Tones) = 2),
      'held chord is not sliced by another voice attack');
    Check((Spans[1].Kind = wmskRest) and (Spans[2].StartTick = 3 * Q) and
      (Spans[2].DurationTicks = Q), 'rest and identical chord reattack preserved');
    Spans := S.CopyVoiceSpans(1);
    Check((Length(Spans) = 4) and (Spans[1].Tones[0].Pitch = 65) and
      (Spans[1].StartTick = Q), 'independent upper voice attacks');
    TextBefore := EncodeWfcMusicText(S); Signature := C.Signature;
    Frames := C.CopyEnsembleFrames;
    Frames[0].Voices[0].Tones[0].Pitch := 1;
    Fresh := C.CopyEnsembleFrames;
    Check(Fresh[0].Voices[0].Tones[0].Pitch = 48, 'frame tones deeply detached');
    G := C.CopyGenerated(wmelEnsemble); G.Tokens[0] := 'changed'; G.StateIndices[0] := -1;
    G2 := C.CopyGenerated(wmelEnsemble);
    Check((G2.Tokens[0] = F.EA[0]) and (G2.StateIndices[0] >= 0), 'latent arrays detached');
    H := C.CopyHarmonyCells; H[0].PitchClasses[0] := 11;
    H := C.CopyHarmonyCells;
    Check(H[0].PitchClasses[0] = 0, 'harmony sets detached');
    Rhythm := C.CopyRhythmCells; Rhythm[0].Actions[0] := wmcaRest;
    Rhythm := C.CopyRhythmCells;
    Check(Rhythm[0].Actions[0] = wmcaAttack, 'rhythm vectors detached');
    S.Free; S := nil; S2 := C.CopyScore;
    Check(EncodeWfcMusicText(S2) = TextBefore, 'copied score lifetime independent');
    Check(C.Signature = Signature, 'detached copy edits do not change signature');
    Check(CalculateWfcMusicEnsembleCompositionSignature(C) = Signature, 'signature recomputes');
    Check(Signature = Cardinal($1261D0B1), 'portable baseline signature golden');
    S := P.CopyScoreTemplate;
    Check((S <> F.Template) and (EncodeWfcMusicText(S) = EncodeWfcMusicText(F.Template)),
      'template is deep cloned');
    Check(P.TryGenerate(C2, R) and (C2.Signature = Signature), 'repeated seeded full solve replay');
    WriteLn('Baseline signature: ', WfcMusicEnsembleCompositionSignatureHex(Signature));
    P.Free; P := nil;
    Check(C.CopyEnsembleFrames[0].Voices[0].Tones[0].Pitch = 48,
      'composition outlives owner');
  finally S2.Free; S.Free; C2.Free; C.Free; P.Free; Done(F); end;
end;

procedure TestSelectiveAndRollback;
var F: TFixture; P: TWfcMusicEnsemblePipeline;
  C, Next: TWfcMusicEnsembleComposition; R: TWfcMusicEnsembleReport;
  NR: TWfcMusicEnsembleSelectiveNegotiationReport;
  N: TGraphNegotiationOptions; O: TGraphSolveOptions;
  V: TWfcMusicEnsembleValidationReport; Before: String;
  Locks: TWfcMusicVoiceCells; Failed: Boolean; I: Integer;
  Roots: TWfcMusicEnsembleLayers;
begin
  Init(F); P := Owner(F); C := nil; Next := nil;
  try
    LockA(P, F); O := DefaultGraphSolveOptions;
    Check(P.TryRegenerateFrom(wmelEnsemble, O, C, R), 'first selective call expands all dirty layers');
    Check(IndicesEqual(R.Solve.ExecutionOrder, [0, 1, 2]), 'initial selective full closure');
    if C = nil then Exit;
    Locks := VoiceCells([F.A[1].Voices[0]]);
    Check(P.LockVoiceCells(0, 1, Locks) = P, 'voice-only fluent lock');
    Check(P.TryRegenerateFrom(wmelEnsemble, O, Next, R), 'voice-only selective repair');
    Check((R.Solve.Passes[0].Disposition = gpdReused) and
      (R.Solve.Passes[1].Disposition = gpdReused) and
      (R.Solve.Passes[2].Disposition = gpdSolved), 'both clean providers reused');
    Check(TokensEqual(Next.CopyGenerated(wmelEnsemble).Tokens, F.EA),
      'voice lock preserves other independent attack');
    Next.Free; Next := nil;
    P.IntersectAllowedTokens(wmelHarmony, 0, F.HA[0]);
    Check(P.TryRegenerateFrom(wmelEnsemble, O, Next, R), 'dirty provider included despite consumer-only root');
    Check((R.Solve.Passes[0].Disposition = gpdSolved) and
      (R.Solve.Passes[1].Disposition = gpdReused), 'dirty provider closure exact');
    Next.Free; Next := nil;
    P.Seed := 99;
    Check(P.TryRegenerateFrom(wmelEnsemble, O, Next, R), 'seed invalidation reparses all passes');
    for I := 0 to 2 do Check(R.Solve.Passes[I].Disposition = gpdSolved,
      'seed invalidates layer ' + IntToStr(I));
    Check((Next.Seed = 99) and (P.Seed = 99), 'new seed captured');
    Next.Free; Next := nil;
    Before := Fingerprint(P);
    Locks := VoiceCells([F.B[0].Voices[1], F.B[1].Voices[1]]);
    Locks[1].Tones[0].Velocity := 0;
    Failed := False;
    try P.LockVoiceCells(1, 0, Locks); except on E: Exception do Failed := True; end;
    Check(Failed, 'late invalid voice cell rejected');
    Check(Fingerprint(P) = Before, 'invalid voice lock does not mutate committed layers');
    Check(P.TryRegenerateFrom(wmelEnsemble, O, Next, R), 'late invalid voice lock leaves no partial constraint');
    Next.Free; Next := nil;
    Failed := False;
    try P.LockVoiceCells(2, 0, VoiceCells([F.A[0].Voices[0]]));
    except on E: Exception do Failed := True; end;
    Check(Failed, 'voice index checked');
    Failed := False;
    try P.LockVoiceCells(0, 4, VoiceCells([F.A[0].Voices[0]]));
    except on E: Exception do Failed := True; end;
    Check(Failed, 'voice lock range checked');
    Before := Fingerprint(P);
    P.IntersectAllowedTokens(wmelEnsemble, 0, F.EB[0]);
    Check(not P.TryGenerate(Next, R), 'incompatible exact provider constraint fails');
    Check((Next = nil) and (R.Status = wmesSolveFailed), 'failed solve publishes no composition');
    Check(Fingerprint(P) = Before, 'failed solve rolls all layers back');
    Check(not P.Validate(C, V), 'old artifact checked against current caller constraints');
    Check(V.Issue.Kind = wmevikCallerConstraint, 'constraint issue diagnostic');
    P.ClearAllowedTokens(wmelEnsemble, 0);
    Check(P.TryRegenerateFrom(wmelEnsemble, O, Next, R), 'clearing restores baseline endpoint domain');
    Next.Free; Next := nil;
    N := DefaultGraphNegotiationOptions;
    SetLength(Roots, 2); Roots[0] := wmelEnsemble; Roots[1] := wmelEnsemble;
    Check(P.TryRegenerateNegotiatedFrom(Roots, N, Next, NR), 'selective negotiated duplicate roots');
    Check(NR.Status = wmesCompleted, 'selective negotiated semantic commit');
    Next.Free; Next := nil;
    Locks := VoiceCells([MakeWfcMusicVoiceCell(wmcaAttack,
      F.A[0].Voices[1].Tones)]);
    Locks[0].Tones[0].Pitch := 120;
    P.LockVoiceCells(1, 0, Locks);
    Check(not P.TryGenerate(Next, R), 'valid unrepresented voice cell becomes contradiction');
    Check(Next = nil, 'empty voice domain publishes nothing');
  finally Next.Free; C.Free; P.Free; Done(F); end;
end;

procedure TestExtentsAndConstruction;
var F: TFixture; P: TWfcMusicEnsemblePipeline; C: TWfcMusicEnsembleComposition;
  Conf: TWfcMusicEnsembleConfig; R: TWfcMusicEnsembleReport;
  N: TWfcMusicEnsembleNegotiationReport;
  V: TWfcMusicEnsembleValidationReport; Small: TWfcMusicScore;
  O: TGraphSolveOptions; Failed: Boolean; Bad: TWfcSequenceModel;
begin
  Init(F); P := nil; C := nil; Small := Template(2); Bad := nil;
  try
    Conf := Config(F); Conf.ScoreTemplate := Small; Conf.Extent := wsePrefix;
    P := TWfcMusicEnsemblePipeline.Create(Conf);
    P.IntersectAllowedTokens(wmelHarmony, 0, F.HA[0]);
    Check(P.TryGenerate(C, R), 'finite prefix accepts truncated observed phrase');
    Check((C <> nil) and (C.CellCount = 2) and (C.Extent = wsePrefix),
      'prefix extent retained');
    Check(P.Validate(C, V), 'prefix independent validation');
    Check(C.CopyGenerated(wmelEnsemble).Extent = wsePrefix, 'latent capture uses prefix');
    C.Free; C := nil; P.Free; P := nil;
    Conf.Extent := wseWhole; P := TWfcMusicEnsemblePipeline.Create(Conf);
    O := DefaultGraphSolveOptions;
    Check(not P.TryGenerate(O, C, R), 'whole extent rejects same unfinished provider phrase');
    P.Free; P := nil;
    Conf := Config(F); Conf.Extent := wseSuffix; Failed := False;
    try P := TWfcMusicEnsemblePipeline.Create(Conf);
    except on E: Exception do Failed := True; end;
    Check(Failed, 'suffix policy rejected');
    Conf := Config(F); Conf.QuantumTicks := 7; Failed := False;
    try P := TWfcMusicEnsemblePipeline.Create(Conf);
    except on E: Exception do Failed := True; end;
    Check(Failed, 'inexact quantum rejected');
    Conf := Config(F); Conf.ScoreTemplate := nil; Failed := False;
    try P := TWfcMusicEnsemblePipeline.Create(Conf);
    except on E: Exception do Failed := True; end;
    Check(Failed, 'nil template rejected');
    Conf := Config(F); Conf.Models.Rhythm := nil; Failed := False;
    try P := TWfcMusicEnsemblePipeline.Create(Conf);
    except on E: Exception do Failed := True; end;
    Check(Failed, 'nil model rejected');
    Bad := LearnSequenceModel(F.EA, 1);
    Conf := Config(F); Conf.Models.Ensemble := Bad; Failed := False;
    try P := TWfcMusicEnsemblePipeline.Create(Conf);
    except on E: Exception do Failed := True; end;
    Check(Failed, 'unsafe structural hold edges rejected before owner solve');
    Bad.Free; Bad := nil;
    Conf := Config(F); P := TWfcMusicEnsemblePipeline.Create(Conf);
    F.Template.Free; F.Template := nil;
    Check(P.TryGenerateNegotiated(C, N),
      'copied template remains valid after caller frees original');
  finally Bad.Free; C.Free; P.Free; Small.Free; Done(F); end;
end;

procedure TestArtifactsAndAllowedHarmony;
var F: TFixture; P: TWfcMusicEnsemblePipeline; C, Artifact: TWfcMusicEnsembleComposition;
  Conf: TWfcMusicEnsembleConfig; R: TWfcMusicEnsembleReport;
  V: TWfcMusicEnsembleValidationReport; S: TWfcMusicScore;
  H, Wrong, Ensemble, Rhythm: TWfcModelTokens; Sets: TWfcMusicPitchClassSets;
  HM: TWfcSequenceModel; Failed: Boolean; I: Integer;
  ExactSig, AllowedSig, PrefixSig: TWfcMusicEnsembleCompositionSignature;
  WrongScore: TWfcMusicScore;
  OriginalSpans, SlicedSpans: TWfcMusicSpanEvents;
  SlicedFrames: TWfcMusicEnsembleFrames;
begin
  Init(F); P := nil; C := nil; Artifact := nil; S := nil; HM := nil;
  WrongScore := nil;
  try
    S := RebuildWfcMusicEnsembleScore(F.A, Q, F.Template);
    ExactSig := CalculateWfcMusicEnsembleCompositionSignature(0, Q, wseWhole,
      wmehmExact, F.HA, F.RT, F.EA, S);
    AllowedSig := CalculateWfcMusicEnsembleCompositionSignature(0, Q, wseWhole,
      wmehmAllowed, F.HA, F.RT, F.EA, S);
    PrefixSig := CalculateWfcMusicEnsembleCompositionSignature(0, Q, wsePrefix,
      wmehmExact, F.HA, F.RT, F.EA, S);
    Check((ExactSig <> AllowedSig) and (ExactSig <> PrefixSig), 'signature binds explicit semantics');
    Artifact := CreateWfcMusicEnsembleComposition(0, Q, wseWhole,
      wmehmExact, F.HA, F.RT, F.EA, S);
    Check(not Artifact.HasLatentCapture, 'public artifact invents no latent witness');
    P := Owner(F);
    Check(not P.Validate(Artifact, V) and (V.Issue.Kind = wmevikLatentCapture),
      'owner requires real latent witness');
    Artifact.Free; Artifact := nil; P.Free; P := nil;
    SetLength(Sets, 4);
    for I := 0 to 3 do Sets[I] := MakeWfcMusicPitchClassSet(12, [0, 4, 5, 7, 8, 11]);
    H := EncodeWfcMusicPitchClassSets(Sets);
    Failed := False;
    try Artifact := CreateWfcMusicEnsembleComposition(0, Q, wseWhole,
      wmehmExact, H, F.RT, F.EA, S);
    except on E: Exception do Failed := True; end;
    Check(Failed, 'exact projection rejects merely containing palette');
    Artifact := CreateWfcMusicEnsembleComposition(0, Q, wseWhole,
      wmehmAllowed, H, F.RT, F.EA, S);
    Check(Artifact <> nil, 'allowed palette accepts strict subsets and silence');
    Artifact.Free; Artifact := nil;
    HM := LearnSequenceModel(H, 4); Conf := Config(F);
    Conf.Models.Harmony := HM; Conf.HarmonyMode := wmehmAllowed;
    P := TWfcMusicEnsemblePipeline.Create(Conf); P.LockEnsembleFrames(0, F.A);
    Check(P.TryGenerate(C, R), 'allowed-set pipeline generates exact score');
    Check(P.Validate(C, V), 'allowed-set independent commit validator');
    Check(TokensEqual(C.CopyGenerated(wmelHarmony).Tokens, H), 'full allowed provider retained');
    Wrong := Copy(H); Wrong[1] := EncodeWfcMusicPitchClassSet(MakeWfcMusicPitchClassSet(12, [0, 5]));
    Failed := False;
    try Artifact := CreateWfcMusicEnsembleComposition(0, Q, wseWhole,
      wmehmAllowed, Wrong, F.RT, F.EA, S);
    except on E: Exception do Failed := True; end;
    Check(Failed, 'allowed set must include held bass classes too');
    Rhythm := Copy(F.RT); Rhythm[1] := F.RT[0]; Failed := False;
    try Artifact := CreateWfcMusicEnsembleComposition(0, Q, wseWhole,
      wmehmExact, F.HA, Rhythm, F.EA, S);
    except on E: Exception do Failed := True; end;
    Check(Failed, 'independent rhythm vector mismatch rejected');
    Ensemble := Copy(F.EA); Ensemble[0] := F.EA[1]; Failed := False;
    try Artifact := CreateWfcMusicEnsembleComposition(0, Q, wseWhole,
      wmehmAllowed, H, F.RT, Ensemble, S);
    except on E: Exception do Failed := True; end;
    Check(Failed, 'leading held ensemble cannot become artifact');
    WrongScore := RebuildWfcMusicEnsembleScore(F.B, Q, F.Template);
    Failed := False;
    try Artifact := CreateWfcMusicEnsembleComposition(0, Q, wseWhole,
      wmehmExact, F.HA, F.RT, F.EA, WrongScore);
    except on E: Exception do Failed := True; end;
    Check(Failed, 'correct tokens cannot publish another exact score');
    WrongScore.Free; WrongScore := nil;
    OriginalSpans := S.CopySpans;
    SetLength(SlicedSpans, Length(OriginalSpans) + 1);
    SlicedSpans[0] := MakeWfcMusicSound(0, 0, Q, OriginalSpans[0].Tones);
    SlicedSpans[1] := MakeWfcMusicSound(0, Q, Q, OriginalSpans[0].Tones);
    for I := 1 to High(OriginalSpans) do SlicedSpans[I + 1] := OriginalSpans[I];
    WrongScore := TWfcMusicScore.Create(S.TicksPerQuarter, S.StepsPerOctave,
      S.LengthTicks, S.CopyTracks, S.CopyVoices, S.CopyMeters, S.CopyTempos, SlicedSpans);
    Failed := False;
    try Artifact := CreateWfcMusicEnsembleComposition(0, Q, wseWhole,
      wmehmExact, F.HA, F.RT, F.EA, WrongScore);
    except on E: Exception do Failed := True; end;
    Check(Failed, 'independent interval proof rejects held chord sliced into another attack');
    SlicedFrames := DecodeWfcMusicEnsembleFrames(F.EA);
    SlicedFrames[1].Voices[0].Action := wmcaAttack;
    Ensemble := EncodeWfcMusicEnsembleFrames(SlicedFrames);
    Rhythm := EncodeWfcMusicRhythmFrames(ProjectWfcMusicEnsembleFramesToRhythm(SlicedFrames));
    Artifact := CreateWfcMusicEnsembleComposition(0, Q, wseWhole,
      wmehmExact, F.HA, Rhythm, Ensemble, WrongScore);
    Check(Artifact <> nil, 'identical adjacent chord attacks remain legitimate when explicitly requested');
    Artifact.Free; Artifact := nil;
    Failed := False;
    try Artifact := CreateWfcMusicEnsembleComposition(0, Q, wseWhole,
      wmehmExact, F.HA, F.RT, F.EA, nil);
    except on E: Exception do Failed := True; end;
    Check(Failed, 'nil artifact score rejected');
  finally WrongScore.Free; Artifact.Free; C.Free; P.Free; HM.Free; S.Free; Done(F); end;
end;

procedure TestSafeOrderOneAndAtomicConstraints;
var F: TFixture; P: TWfcMusicEnsemblePipeline;
  C, Next: TWfcMusicEnsembleComposition; R: TWfcMusicEnsembleReport;
  Conf: TWfcMusicEnsembleConfig; Frames: TWfcMusicEnsembleFrames;
  EModel, HModel, RModel: TWfcSequenceModel;
  E, H, Rhythm: TWfcModelTokens; Constraints: TWfcSequenceTokenConstraints;
  Failed: Boolean; Before: String; Roots: TWfcMusicEnsembleLayers;
  O: TGraphSolveOptions;
begin
  Init(F); P := nil; C := nil; Next := nil;
  EModel := nil; HModel := nil; RModel := nil;
  try
    SetLength(Frames, 4);
    Frames[0] := F.A[0]; Frames[1] := F.A[2];
    Frames[2] := F.A[0]; Frames[3] := F.A[2];
    E := EncodeWfcMusicEnsembleFrames(Frames);
    H := EncodeWfcMusicPitchClassSets(ProjectWfcMusicEnsembleFramesToPitchClassSets(Frames, 12));
    Rhythm := EncodeWfcMusicRhythmFrames(ProjectWfcMusicEnsembleFramesToRhythm(Frames));
    EModel := LearnSequenceModel(E, 1); HModel := LearnSequenceModel(H, 1);
    RModel := LearnSequenceModel(Rhythm, 1);
    Conf := Config(F); Conf.Models.Ensemble := EModel;
    Conf.Models.Harmony := HModel; Conf.Models.Rhythm := RModel;
    P := TWfcMusicEnsemblePipeline.Create(Conf);
    P.IntersectLockedSpan(wmelHarmony, 0, H);
    P.IntersectLockedSpan(wmelRhythm, 0, Rhythm);
    Check(P.TryGenerate(C, R), 'safe order-one rest attack owner solves');
    Check(TokensEqual(C.CopyGenerated(wmelEnsemble).Tokens, E), 'safe order-one output exact');
    Before := Fingerprint(P);
    SetLength(Constraints, 2);
    Constraints[0].Position := 0;
    SetLength(Constraints[0].AllowedTokens, 1);
    Constraints[0].AllowedTokens[0] := E[1];
    Constraints[1].Position := 1;
    SetLength(Constraints[1].AllowedTokens, 1);
    Constraints[1].AllowedTokens[0] := 'not-an-ensemble-token';
    Failed := False;
    try P.IntersectTokenConstraints(wmelEnsemble, Constraints);
    except on X: Exception do Failed := True; end;
    Check(Failed, 'late unknown public token rejected');
    Check(Fingerprint(P) = Before, 'public constraint batch preflight atomic');
    O := DefaultGraphSolveOptions; SetLength(Roots, 2);
    Roots[0] := wmelEnsemble; Roots[1] := wmelEnsemble;
    Check(P.TryRegenerateFrom(Roots, O, Next, R), 'ordinary duplicate roots preserve valid solve');
    Check(TokensEqual(Next.CopyGenerated(wmelEnsemble).Tokens, E), 'invalid batch leaves no earlier restriction');
    Next.Free; Next := nil;
    Roots := nil; Failed := False;
    try P.TryRegenerateFrom(Roots, O, Next, R);
    except on X: Exception do Failed := True; end;
    Check(Failed and (Next = nil) and (R.Status = wmesNotRun), 'empty roots clear output before rejection');
    O.MaxBacktracks := -1; Failed := False;
    try P.TryGenerate(O, Next, R);
    except on X: Exception do Failed := True; end;
    Check(Failed and (Next = nil), 'invalid solver budget publishes no stale artifact');
    Check(Fingerprint(P) = Before, 'invalid calls leave committed content intact');
  finally Next.Free; C.Free; P.Free; RModel.Free; HModel.Free; EModel.Free; Done(F); end;
end;

procedure TestNegotiation;
var F: TFixture; P: TWfcMusicEnsemblePipeline; C: TWfcMusicEnsembleComposition;
  R: TWfcMusicEnsembleReport; N: TWfcMusicEnsembleNegotiationReport;
  O: TGraphNegotiationOptions; Before: String;
  SN: TWfcMusicEnsembleSelectiveNegotiationReport;
begin
  Init(F); P := nil; C := nil;
  try
    O := DefaultGraphNegotiationOptions; O.MaxPassBacktracks := 64;
    P := Owner(F);
    P.LockVoiceCells(1, 0, VoiceCells([F.B[0].Voices[1]]));
    Check(not P.TryGenerate(C, R), 'seed-zero one-way witness rejects conflicting provider');
    Check(C = nil, 'one-way rejection leaves no composition');
    O.MaxPassBacktracks := 0;
    Check(not P.TryGenerateNegotiated(O, C, N), 'zero provider retry cap preserves failure');
    Check((C = nil) and (N.Search.PassBacktracks = 0), 'bounded failure publishes nothing');
    O.MaxPassBacktracks := 64;
    Check(P.TryGenerateNegotiated(O, C, N), 'negotiation reopens conflicting harmony provider');
    if C = nil then Exit;
    Check(N.Search.PassBacktracks > 0, 'negotiation actually revisited provider');
    Check(TokensEqual(C.CopyGenerated(wmelEnsemble).Tokens, F.EB), 'negotiated coherent alternate phrase');
    Check(N.Search.TranscriptHash = TGraphTraceSignature($ED81126E), 'portable negotiation transcript golden');
    C.Free; C := nil; P.Free; P := Owner(F);
    Check(P.TryGenerate(C, R), 'selective negotiation baseline');
    Check(TokensEqual(C.CopyGenerated(wmelEnsemble).Tokens, F.EA), 'baseline chooses conflicting phrase A');
    C.Free; C := nil;
    P.LockVoiceCells(1, 0, VoiceCells([F.B[0].Voices[1]]));
    Before := Fingerprint(P);
    Check(not P.TryRegenerateNegotiatedFrom(wmelEnsemble, O, C, SN),
      'consumer-only negotiation cannot reopen clean provider outside closure');
    Check((C = nil) and (Fingerprint(P) = Before), 'failed selective negotiation rolls back');
    Check(P.TryRegenerateNegotiatedFrom(wmelHarmony, O, C, SN),
      'provider-root selective negotiation repairs ensemble');
    Check((SN.Search.Search.PassBacktracks > 0) and
      (SN.Search.Search.FinalReport.Passes[1].Disposition = gpdReused),
      'selective provider reopened while rhythm reused');
    Check(TokensEqual(C.CopyGenerated(wmelEnsemble).Tokens, F.EB), 'selective negotiated alternate output');
  finally C.Free; P.Free; Done(F); end;
end;

begin
  Checks := 0; Failures := 0;
  Run('generation and detached snapshots', TestGenerationAndCopies);
  Run('selective closure and rollback', TestSelectiveAndRollback);
  Run('extent and constructor preflight', TestExtentsAndConstruction);
  Run('artifacts and allowed harmony', TestArtifactsAndAllowedHarmony);
  Run('safe order one and atomic public constraints', TestSafeOrderOneAndAtomicConstraints);
  Run('provider negotiation', TestNegotiation);
  WriteLn('Checks: ', Checks, ', Failures: ', Failures);
  if Failures <> 0 then Halt(1);
end.
