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
program wfc_music_voices_graph_test;
{$mode delphi}{$H+}
uses SysUtils, wfc, wfc_model, wfc_sequence, wfc_sequence_learn,
  wfc_sequence_graph, wfc_music, wfc_music_sequence, wfc_music_ensemble,
  wfc_music_ensemble_graph, wfc_music_voices_graph
  {$IFDEF PAS2JS}, wfc_browser_test_host{$ENDIF};

var Checks, Failures: Integer;
procedure Check(const B: Boolean; const S: String);
begin
  Inc(Checks);
  if not B then begin Inc(Failures); WriteLn('FAIL: ', S); end;
end;

function Cell(const Action: TWfcMusicCellAction; const Pitches: array of Integer;
  const Velocity: Integer = 80): TWfcMusicVoiceCell;
var T: TWfcMusicTones; I: Integer;
begin
  SetLength(T, Length(Pitches));
  for I := 0 to High(T) do T[I] := MakeWfcMusicTone(Pitches[I], Velocity);
  Result := MakeWfcMusicVoiceCell(Action, T);
end;

function Single(const V: TWfcMusicVoiceCell): TWfcModelToken;
var A: TWfcMusicVoiceCells;
begin SetLength(A, 1); A[0] := V; Result := EncodeWfcMusicEnsembleFrame(MakeWfcMusicEnsembleFrame(A)); end;

function Choice(const Voice, Variant: Integer): TWfcMusicVoiceCell;
begin
  case Voice of
    0: if Variant = 0 then Result := Cell(wmcaAttack, [48], 72)
       else Result := Cell(wmcaAttack, [55], 72);
    1: if Variant = 0 then Result := Cell(wmcaAttack, [60, 64], 64)
       else Result := Cell(wmcaAttack, [64, 67], 64);
  else
    if Variant = 0 then Result := Cell(wmcaAttack, [79], 96)
    else Result := Cell(wmcaAttack, [72], 96);
  end;
end;

function OriginalFrame(const Variant: Integer): TWfcMusicEnsembleFrame;
var V: TWfcMusicVoiceCells; I: Integer;
begin
  SetLength(V, 3); for I := 0 to 2 do V[I] := Choice(I, Variant);
  Result := MakeWfcMusicEnsembleFrame(V);
end;

function Fixture: TWfcMusicVoicesGraphConfig;
var Samples: TWfcSequenceSamples; Tokens: TWfcModelTokens;
  Sets: TWfcMusicPitchClassSets; Classes: TWfcMusicPitchClasses;
  Rhythm: TWfcMusicRhythmFrames; Actions: TWfcMusicCellActions; I, J: Integer;
begin
  Result := Default(TWfcMusicVoicesGraphConfig);
  Result.StepsPerOctave := 12; Result.HarmonyMode := wmehmExact;
  SetLength(Result.Voices, 3);
  try
    SetLength(Samples, 2); SetLength(Tokens, 2);
    for I := 0 to 2 do
    begin
      for J := 0 to 1 do
      begin
        Tokens[0] := Single(Choice(I, J)); Tokens[1] := Single(MakeWfcMusicRestVoiceCell);
        Samples[J] := MakeWfcSequenceSample(Tokens);
      end;
      Result.Voices[I].Model := LearnSequenceModelCorpus(Samples, 2);
      Result.Voices[I].MinPitch := 0; Result.Voices[I].MaxPitch := 127;
    end;
    SetLength(Sets, 2); SetLength(Classes, 3);
    Classes[0] := 0; Classes[1] := 4; Classes[2] := 7;
    Sets[0] := MakeWfcMusicPitchClassSet(12, Classes); Classes := nil;
    Sets[1] := MakeWfcMusicPitchClassSet(12, Classes);
    Result.HarmonyModel := LearnSequenceModel(EncodeWfcMusicPitchClassSets(Sets), 2);
    SetLength(Rhythm, 2); SetLength(Actions, 3);
    for I := 0 to 2 do Actions[I] := wmcaAttack;
    Rhythm[0] := MakeWfcMusicRhythmFrame(Actions);
    for I := 0 to 2 do Actions[I] := wmcaRest;
    Rhythm[1] := MakeWfcMusicRhythmFrame(Actions);
    Result.RhythmModel := LearnSequenceModel(EncodeWfcMusicRhythmFrames(Rhythm), 2);
    SetLength(Result.PairConstraints, 2);
    for I := 0 to 1 do
    begin
      Result.PairConstraints[I].LowerVoice := I;
      Result.PairConstraints[I].UpperVoice := I + 1;
      Result.PairConstraints[I].MinGap := 3; Result.PairConstraints[I].MaxGap := 33;
      Result.PairConstraints[I].RestPolicy := wmvprSuspend;
    end;
  except
    Result.HarmonyModel.Free; Result.RhythmModel.Free;
    for I := 0 to High(Result.Voices) do Result.Voices[I].Model.Free;
    raise;
  end;
end;

procedure FreeConfig(var C: TWfcMusicVoicesGraphConfig);
var I: Integer;
begin
  C.HarmonyModel.Free; C.RhythmModel.Free;
  for I := 0 to High(C.Voices) do C.Voices[I].Model.Free;
  C := Default(TWfcMusicVoicesGraphConfig);
end;

function Initial(const C: TWfcMusicVoicesGraphConfig; const Ending: Boolean = True): TWfcMusicVoicesBoundaries;
var I: Integer;
begin
  Result := nil; SetLength(Result, WfcMusicVoicesModelCount(C));
  for I := 0 to High(Result) do Result[I] := MakeWfcSequenceInitialSegmentBoundary(Ending);
end;

procedure SelectChoices(const C: TWfcMusicVoicesGraphConfig; const G: TGraph;
  const Choices: array of Integer);
var I: Integer;
begin
  for I := 0 to High(Choices) do
    IntersectSequenceAllowedTokens(C.Voices[I].Model, G.PassGraph[I + 2], 0,
      Single(Choice(I, Choices[I])));
end;

procedure TestIndependentCombinations;
var C: TWfcMusicVoicesGraphConfig; B: TWfcMusicVoicesBoundaries;
  G: TGraph; Report: TGraphSolveReport; Generated: TWfcMusicVoicesGenerated;
  Proof: TWfcMusicVoicesValidationReport; I, J, K, Mode, Winners, P, X: Integer;
  Expected, Solved: Boolean;
begin
  C := Fixture;
  try
    B := Initial(C);
    Check(WfcMusicVoicesModelCount(C) = 5, 'H/R plus three independent models');
    Check((WfcMusicVoicesPassLabel(2) = 'voice.0') and
      (WfcMusicVoicesCoveragePassLabel(7) = 'coverage.7'), 'stable public pass labels');
    for Mode := 0 to 1 do
    begin
      if Mode = 0 then C.HarmonyMode := wmehmExact else C.HarmonyMode := wmehmAllowed;
      Winners := 0;
      for I := 0 to 1 do for J := 0 to 1 do for K := 0 to 1 do
      begin
        G := BuildWfcMusicVoicesSegmentGraph(C, 2, 0, B);
        try
          SelectChoices(C, G, [I, J, K]);
          Expected := (Mode = 1) or not (((I = 0) and (J = 0) and (K = 1)) or
            ((I = 1) and (J = 1) and (K = 0)));
          Solved := G.TrySolve(DefaultGraphSolveOptions, Report);
          Check(Solved = Expected, 'mixture coverage differs from membership ' + IntToStr(Mode) + '/' +
            IntToStr(I) + IntToStr(J) + IntToStr(K));
          if Solved then
          begin
            Inc(Winners);
            Check(CaptureSolvedWfcMusicVoices(C, G, B, Generated, Proof), 'independent capture proves solved mixture');
            Check((Proof.CheckedModels = 5) and (Proof.CheckedCells = 2), 'proof accounts for models and cells');
            if Mode = 0 then
            begin
              Check((G.TotalPassCount = 8) and (Length(Generated.Coverage) = 3), 'exact mode uses three small witness passes');
              for P := 0 to 2 do Check(Generated.Coverage[P].Suppliers[1] = -1, 'terminal silence has explicit absence witness');
            end
            else Check((G.TotalPassCount = 5) and (Generated.Coverage = nil), 'allowed mode has no exact-coverage witnesses');
            if (I = 0) and (J = 1) and (K = 1) then
              Check((EncodeWfcMusicEnsembleFrame(Generated.Frames[0]) <>
                EncodeWfcMusicEnsembleFrame(OriginalFrame(0))) and
                (EncodeWfcMusicEnsembleFrame(Generated.Frames[0]) <>
                EncodeWfcMusicEnsembleFrame(OriginalFrame(1))),
                'novel vertical is not either authored full-frame choice');
          end
          else
          begin
            Check(not CaptureSolvedWfcMusicVoices(C, G, B, Generated, Proof), 'failed graph exposes no composition');
            Check((Generated.Layers = nil) and (Generated.Frames = nil) and (Generated.Coverage = nil), 'failure resets every output vector');
            for P := 0 to G.TotalPassCount - 1 do for X := 0 to 1 do
              Check(G.PassGraph[P].Entry[X, 0, 0].Empty and not G.PassGraph[P].Entry[X, 0, 0].Generated,
                'failed whole transaction restores empty generated state');
          end;
        finally G.Free; end;
      end;
      if Mode = 0 then Check(Winners = 6, 'exact coverage admits six mixtures')
      else Check(Winners = 8, 'allowed palette admits all eight mixtures');
    end;
  finally FreeConfig(C); end;
end;

procedure TestRangesPairsAndProof;
var C, Changed: TWfcMusicVoicesGraphConfig; B: TWfcMusicVoicesBoundaries;
  G: TGraph; R: TGraphSolveReport; D: TWfcMusicVoicesGenerated;
  Proof: TWfcMusicVoicesValidationReport; P, Saved: Integer;
begin
  C := Fixture;
  try
    B := Initial(C);
    G := BuildWfcMusicVoicesSegmentGraph(C, 2, 0, B);
    try
      SelectChoices(C, G, [0, 1, 1]);
      Check(G.TrySolve(DefaultGraphSolveOptions, R), 'proof fixture solves');
      Check(CaptureSolvedWfcMusicVoices(C, G, B, D, Proof), 'proof fixture captures');
      Check(D.Coverage[0].Suppliers[0] = 0, 'shared sounding class selects its lowest supplying voice');
      D.Coverage[0].Suppliers[0] := 2;
      G.PassGraph[5].Entry[0, 0, 0].Value := 'voice.2';
      Check(not ValidateWfcMusicVoicesGenerated(C, G, B, D, Proof) and
        (Proof.Issue.Kind = wmvikWitness), 'matching higher supplier is independently rejected as noncanonical');
      D.Coverage[0].Suppliers[0] := 0;
      G.PassGraph[5].Entry[0, 0, 0].Value := 'voice.0';
      Saved := D.Coverage[0].Suppliers[0]; D.Coverage[0].Suppliers[0] := -1;
      Check(not ValidateWfcMusicVoicesGenerated(C, G, B, D, Proof) and
        (Proof.Issue.Kind = wmvikWitness), 'forged absence witness cannot bless sounding class');
      D.Coverage[0].Suppliers[0] := Saved;
      G.PassGraph[5].Entry[0, 0, 0].Value := 'absent';
      Check(not ValidateWfcMusicVoicesGenerated(C, G, B, D, Proof), 'graph witness tampering is independently rejected');
      G.PassGraph[5].Entry[0, 0, 0].Value := 'voice.' + IntToStr(Saved);
      G.PassGraph[5].SetAllowedValues(0, 0, 0, ['absent']);
      Check(not ValidateWfcMusicVoicesGenerated(C, G, B, D, Proof) and
        (Proof.Issue.Kind = wmvikEntryConstraint), 'witness caller domain is checked independently');
      G.PassGraph[5].ClearAllowedValues(0, 0, 0);
      Changed := CopyWfcMusicVoicesGraphConfig(C); Changed.Voices[0].MinPitch := 49;
      Check(C.Voices[0].MinPitch = 0, 'configuration voice records are detached');
      Check(not ValidateWfcMusicVoicesGenerated(Changed, G, B, D, Proof) and
        (Proof.Issue.Kind = wmvikRange), 'independent range check does not trust installed rules');
      Changed := CopyWfcMusicVoicesGraphConfig(C); Changed.PairConstraints[0].MinGap := 30;
      Check(C.PairConstraints[0].MinGap = 3, 'configuration pair records are detached');
      Check(not ValidateWfcMusicVoicesGenerated(Changed, G, B, D, Proof) and
        (Proof.Issue.Kind = wmvikPair), 'independent pair check does not trust installed rules');
      Inc(D.Frames[0].Voices[0].Tones[0].Velocity);
      Check(not ValidateWfcMusicVoicesGenerated(C, G, B, D, Proof), 'altered output voice velocity is rejected');
      Dec(D.Frames[0].Voices[0].Tones[0].Velocity);
      Check(ValidateWfcMusicVoicesGenerated(C, G, B, D, Proof), 'restored proof remains valid');
      G.PassGraph[2].ClearAllowedValues(0, 0, 0);
      G.PassGraph[2].SetAllowedValues(0, 0, 0, []);
      Check(not ValidateWfcMusicVoicesGenerated(C, G, B, D, Proof) and
        (Proof.Issue.Kind = wmvikEntryConstraint), 'latent caller empty domain cannot be erased from proof');
    finally G.Free; end;
    for P := 0 to 2 do
    begin
      Changed := CopyWfcMusicVoicesGraphConfig(C);
      case P of
        0: Changed.Voices[0].MinPitch := 56;
        1: Changed.PairConstraints[0].MinGap := 33;
        2: Changed.PairConstraints[0].RestPolicy := wmvprReject;
      end;
      G := BuildWfcMusicVoicesSegmentGraph(Changed, 2, 0, B);
      try Check(not G.TrySolve(DefaultGraphSolveOptions, R), 'range/pair unsupported alternatives are contradictions ' + IntToStr(P));
      finally G.Free; end;
    end;
  finally FreeConfig(C); end;
end;

procedure TestNegotiation;
var C: TWfcMusicVoicesGraphConfig; B: TWfcMusicVoicesBoundaries;
  G: TGraph; R: TGraphSolveReport; NR: TGraphNegotiationReport;
  O: TGraphNegotiationOptions; D: TWfcMusicVoicesGenerated;
  Proof: TWfcMusicVoicesValidationReport; Seed: Integer; Found: Boolean;
begin
  C := Fixture;
  try
    B := Initial(C); Found := False;
    for Seed := 0 to 63 do
    begin
      G := BuildWfcMusicVoicesSegmentGraph(C, 2, Seed, B);
      try
        if not G.TrySolve(DefaultGraphSolveOptions, R) then
        begin
          O := DefaultGraphNegotiationOptions; O.MaxPassBacktracks := 64;
          Check(G.TrySolveNegotiated(O, NR), 'negotiation repairs an initially uncovered vertical');
          Check(CaptureSolvedWfcMusicVoices(C, G, B, D, Proof), 'negotiated result satisfies independent music proof');
          Found := True;
          WriteLn('Negotiation escape seed: ', Seed);
          Break;
        end;
      finally G.Free; end;
    end;
    Check(Found, 'deterministic seed search finds a genuine one-way failure');
  finally FreeConfig(C); end;
end;

procedure TestIndependentHarmonyProof;
var C, Allowed: TWfcMusicVoicesGraphConfig; B: TWfcMusicVoicesBoundaries;
  Source, Exact: TGraph; R: TGraphSolveReport; D: TWfcMusicVoicesGenerated;
  Proof: TWfcMusicVoicesValidationReport; P, X: Integer;
  Classes: TWfcMusicPitchClasses;
begin
  C := Fixture; Source := nil; Exact := nil;
  try
    B := Initial(C); Allowed := CopyWfcMusicVoicesGraphConfig(C);
    Allowed.HarmonyMode := wmehmAllowed;
    Source := BuildWfcMusicVoicesSegmentGraph(Allowed, 2, 0, B);
    SelectChoices(C, Source, [0, 0, 1]);
    Check(Source.TrySolve(DefaultGraphSolveOptions, R), 'missing-class fixture is a valid allowed-palette result');
    Check(CaptureSolvedWfcMusicVoices(Allowed, Source, B, D, Proof), 'missing-class fixture has valid independent model paths');
    Exact := BuildWfcMusicVoicesSegmentGraph(C, 2, 0, B);
    for P := 0 to WfcMusicVoicesModelCount(C) - 1 do for X := 0 to 1 do
      Exact.PassGraph[P].Entry[X, 0, 0].Value := Source.PassGraph[P].Entry[X, 0, 0].Value;
    Classes := WfcMusicVoicesCoverageClasses(C); SetLength(D.Coverage, Length(Classes));
    for P := 0 to High(Classes) do
    begin
      D.Coverage[P].PitchClass := Classes[P]; SetLength(D.Coverage[P].Suppliers, 2);
      for X := 0 to 1 do
      begin
        D.Coverage[P].Suppliers[X] := -1;
        Exact.PassGraph[WfcMusicVoicesModelCount(C) + P].Entry[X, 0, 0].Value := 'absent';
      end;
      Exact.PassGraph[WfcMusicVoicesModelCount(C) + P].RuleGroups.Clear;
    end;
    Check(not ValidateWfcMusicVoicesGenerated(C, Exact, B, D, Proof) and
      (Proof.Issue.Kind = wmvikHarmony), 'independent exact union rejects a missing class despite removed witness rules');
  finally Exact.Free; Source.Free; FreeConfig(C); end;
end;

procedure TestCanonicalWitnessSearch;
var C: TWfcMusicVoicesGraphConfig; Samples: TWfcSequenceSamples;
  Tokens: TWfcModelTokens; Classes: TWfcMusicPitchClasses;
  Actions: TWfcMusicCellActions; B: TWfcMusicVoicesBoundaries;
  G: TGraph; Plain: TGraphSolveReport; R: TGraphNegotiationReport;
  O: TGraphNegotiationOptions; D: TWfcMusicVoicesGenerated;
  Proof: TWfcMusicVoicesValidationReport;
  I, J, X, Seed, K: Integer; Found, VoiceReopened: Boolean;
begin
  C := Fixture;
  try
    SetLength(Samples, 2); SetLength(Tokens, 5);
    for I := 0 to 2 do
    begin
      for J := 0 to 1 do
      begin
        for X := 0 to 3 do Tokens[X] := Single(Choice(I, J));
        Tokens[4] := Single(MakeWfcMusicRestVoiceCell);
        Samples[J] := MakeWfcSequenceSample(Tokens);
      end;
      FreeAndNil(C.Voices[I].Model);
      C.Voices[I].Model := LearnSequenceModelCorpus(Samples, 2);
    end;
    for J := 0 to 1 do
    begin
      SetLength(Classes, 3);
      if J = 0 then begin Classes[0] := 0; Classes[1] := 4; Classes[2] := 7; end
      else begin Classes[0] := 2; Classes[1] := 5; Classes[2] := 9; end;
      for X := 0 to 3 do Tokens[X] := EncodeWfcMusicPitchClassSet(MakeWfcMusicPitchClassSet(12, Classes));
      Classes := nil; Tokens[4] := EncodeWfcMusicPitchClassSet(MakeWfcMusicPitchClassSet(12, Classes));
      Samples[J] := MakeWfcSequenceSample(Tokens);
    end;
    FreeAndNil(C.HarmonyModel); C.HarmonyModel := LearnSequenceModelCorpus(Samples, 5);
    SetLength(Actions, 3);
    for I := 0 to 2 do Actions[I] := wmcaAttack;
    for X := 0 to 3 do Tokens[X] := EncodeWfcMusicRhythmFrame(MakeWfcMusicRhythmFrame(Actions));
    for I := 0 to 2 do Actions[I] := wmcaRest;
    Tokens[4] := EncodeWfcMusicRhythmFrame(MakeWfcMusicRhythmFrame(Actions));
    FreeAndNil(C.RhythmModel); C.RhythmModel := LearnSequenceModel(Tokens, 5);
    B := Initial(C); Found := False;
    O := DefaultGraphNegotiationOptions;
    O.SolveOptions.MaxBacktracks := 1024; O.MaxPassBacktracks := 64;
    for Seed := 0 to 63 do
    begin
      G := BuildWfcMusicVoicesSegmentGraph(C, 5, Seed, B);
      try
        SetLength(Classes, 3); Classes[0] := 0; Classes[1] := 4; Classes[2] := 7;
        IntersectSequenceAllowedTokens(C.HarmonyModel, G.PassGraph[0], 0,
          EncodeWfcMusicPitchClassSet(MakeWfcMusicPitchClassSet(12, Classes)));
        SelectChoices(C, G, [0, 0]);
        if G.TrySolve(DefaultGraphSolveOptions, Plain) then Continue;
        Found := True;
        Check(G.TrySolveNegotiated(O, R), 'canonical witnesses repair a four-cell shared-class ambiguity within unchanged64 retries');
        VoiceReopened := False;
        for I := 0 to High(R.Attempts) do
          if R.Attempts[I].BacktrackedPassIndex = 4 then VoiceReopened := True;
        Check(VoiceReopened, 'negotiation reopens the musical upper voice instead of exhausting equivalent proof assignments');
        Check((R.PassBacktracks = 5) and (Length(R.Attempts) = 5), 'regression fixture needs four proof rollbacks then one voice rollback');
        Check(CaptureSolvedWfcMusicVoices(C, G, B, D, Proof), 'canonical search result has independent complete music proof');
        for X := 0 to 3 do Check(D.Coverage[0].Suppliers[X] = 0,
          'repeated shared-class cells all choose the same canonical supplier');
        WriteLn('Canonical witness repair seed: ', Seed, '; backtracks: ', R.PassBacktracks);
        Break;
      finally G.Free; end;
    end;
    Check(Found, 'regression fixture contains a genuine initial missing-class choice');
    { Here voice zero always supplies class zero, so the negative relation for
      a later supplier is empty: it must forbid that witness, not weaken it. }
    for I := 0 to High(C.Voices) do
    begin
      SetLength(Tokens, 1); Tokens[0] := Single(Cell(wmcaAttack, [48 + I * 12]));
      FreeAndNil(C.Voices[I].Model); C.Voices[I].Model := LearnSequenceModel(Tokens, 1);
    end;
    SetLength(Classes, 1); Classes[0] := 0;
    Tokens[0] := EncodeWfcMusicPitchClassSet(MakeWfcMusicPitchClassSet(12, Classes));
    FreeAndNil(C.HarmonyModel); C.HarmonyModel := LearnSequenceModel(Tokens, 1);
    for I := 0 to 2 do Actions[I] := wmcaAttack;
    Tokens[0] := EncodeWfcMusicRhythmFrame(MakeWfcMusicRhythmFrame(Actions));
    FreeAndNil(C.RhythmModel); C.RhythmModel := LearnSequenceModel(Tokens, 1);
    B := Initial(C);
    for K := 0 to 1 do
    begin
      G := BuildWfcMusicVoicesSegmentGraph(C, 1, 0, B);
      try
        if K = 1 then G.PassGraph[5].SetAllowedValues(0, 0, 0, ['voice.1']);
        Check(G.TrySolve(DefaultGraphSolveOptions, Plain) = (K = 0),
          'empty earlier-voice omission relation cannot admit a higher supplier');
        if K = 0 then
        begin
          Check(CaptureSolvedWfcMusicVoices(C, G, B, D, Proof), 'all-supplying singleton models capture');
          Check(D.Coverage[0].Suppliers[0] = 0, 'one canonical witness remains when every voice supplies the class');
        end;
      finally G.Free; end;
    end;
  finally FreeConfig(C); end;
end;

procedure TestSilenceAndLargeTuning;
var C: TWfcMusicVoicesGraphConfig; Tokens: TWfcModelTokens; SetValue: TWfcMusicPitchClassSet;
  Classes: TWfcMusicPitchClasses; Actions: TWfcMusicCellActions; B: TWfcMusicVoicesBoundaries;
  G: TGraph; R: TGraphSolveReport; D: TWfcMusicVoicesGenerated;
  Proof: TWfcMusicVoicesValidationReport; Silent: Integer;
begin
  for Silent := 0 to 1 do
  begin
    C := Default(TWfcMusicVoicesGraphConfig); C.StepsPerOctave := 1000003; C.HarmonyMode := wmehmExact;
    SetLength(C.Voices, 1); C.Voices[0].MaxPitch := High(Integer);
    SetLength(Tokens, 1); SetLength(Actions, 1);
    if Silent = 0 then
    begin Tokens[0] := Single(Cell(wmcaAttack, [1000004])); Actions[0] := wmcaAttack;
      SetLength(Classes, 1); Classes[0] := 1; end
    else begin Tokens[0] := Single(MakeWfcMusicRestVoiceCell); Actions[0] := wmcaRest; Classes := nil; end;
    try
      C.Voices[0].Model := LearnSequenceModel(Tokens, 1);
      SetValue := MakeWfcMusicPitchClassSet(C.StepsPerOctave, Classes);
      Tokens[0] := EncodeWfcMusicPitchClassSet(SetValue); C.HarmonyModel := LearnSequenceModel(Tokens, 1);
      Tokens[0] := EncodeWfcMusicRhythmFrame(MakeWfcMusicRhythmFrame(Actions)); C.RhythmModel := LearnSequenceModel(Tokens, 1);
      B := Initial(C); G := BuildWfcMusicVoicesSegmentGraph(C, 1, 0, B);
      try
        Check(G.TrySolve(DefaultGraphSolveOptions, R), 'large non-twelve-step tuning or silence solves');
        Check(CaptureSolvedWfcMusicVoices(C, G, B, D, Proof), 'large tuning/silence independently proves');
        Check(G.TotalPassCount = 4 - Silent, 'only observed harmony classes allocate witness passes');
        if Silent = 1 then Check((D.Coverage = nil) and (Length(D.Frames[0].Voices[0].Tones) = 0),
          'exact empty harmony has zero witnesses and still requires true silence');
      finally G.Free; end;
    finally FreeConfig(C); end;
  end;
end;

procedure TestHeldSegments;
var C: TWfcMusicVoicesGraphConfig; F: TWfcMusicEnsembleFrames; V: TWfcMusicVoiceCells;
  Tokens: TWfcModelTokens; B, NextB: TWfcMusicVoicesBoundaries; G: TGraph;
  R: TGraphSolveReport; D: TWfcMusicVoicesGenerated; Proof: TWfcMusicVoicesValidationReport; I: Integer;
begin
  C := Default(TWfcMusicVoicesGraphConfig); C.StepsPerOctave := 12; C.HarmonyMode := wmehmExact;
  SetLength(C.Voices, 1); C.Voices[0].MaxPitch := 127;
  SetLength(F, 3); SetLength(V, 1);
  V[0] := Cell(wmcaAttack, [48, 52], 75); F[0] := MakeWfcMusicEnsembleFrame(V);
  V[0] := Cell(wmcaHold, [48, 52], 75); F[1] := MakeWfcMusicEnsembleFrame(V);
  V[0] := MakeWfcMusicRestVoiceCell; F[2] := MakeWfcMusicEnsembleFrame(V);
  try
    Tokens := EncodeWfcMusicEnsembleFrames(F); C.Voices[0].Model := LearnSequenceModel(Tokens, 2);
    C.HarmonyModel := LearnSequenceModel(EncodeWfcMusicPitchClassSets(
      ProjectWfcMusicEnsembleFramesToPitchClassSets(F, 12)), 2);
    C.RhythmModel := LearnSequenceModel(EncodeWfcMusicRhythmFrames(
      ProjectWfcMusicEnsembleFramesToRhythm(F)), 2);
    B := Initial(C, False); G := BuildWfcMusicVoicesSegmentGraph(C, 1, 0, B);
    try
      Check(G.TrySolve(DefaultGraphSolveOptions, R), 'one-cell initial attack segment solves');
      Check(CaptureSolvedWfcMusicVoices(C, G, B, D, Proof), 'initial segment captures');
      SetLength(NextB, Length(B));
      for I := 0 to High(B) do NextB[I] := MakeWfcSequenceContinuingSegmentBoundary(D.Layers[I].StateIndices[0], True);
    finally G.Free; end;
    G := BuildWfcMusicVoicesSegmentGraph(C, 2, 0, NextB);
    try
      Check(G.TrySolve(DefaultGraphSolveOptions, R), 'continued hold plus rest segment solves');
      Check(CaptureSolvedWfcMusicVoices(C, G, NextB, D, Proof), 'continued segment independently proves');
      Check((D.Frames[0].Voices[0].Action = wmcaHold) and
        (D.Frames[0].Voices[0].Tones[1].Velocity = 75), 'held chord and velocities cross the seam intact');
      Check(not ValidateWfcMusicVoicesGenerated(C, G, Initial(C), D, Proof), 'original incoming boundary cannot be replaced by an initial boundary');
    finally G.Free; end;
  finally FreeConfig(C); end;
end;

procedure TestConfigurationGuards;
var C, Bad: TWfcMusicVoicesGraphConfig; B: TWfcMusicVoicesBoundaries;
  I: Integer; Rejected: Boolean; G: TGraph;
begin
  C := Fixture;
  try
    B := Initial(C);
    for I := 0 to 7 do
    begin
      Bad := CopyWfcMusicVoicesGraphConfig(C); Rejected := False; G := nil;
      case I of
        0: Bad.StepsPerOctave := 0;
        1: Bad.Voices[0].MinPitch := -1;
        2: Bad.Voices[0].MaxPitch := -1;
        3: Bad.PairConstraints[0].UpperVoice := 0;
        4: Bad.PairConstraints[0].MaxGap := 2;
        5: Bad.PairConstraints[1] := Bad.PairConstraints[0];
        6: Bad.RhythmModel := nil;
        7: Bad.StepsPerOctave := 19;
      end;
      try G := BuildWfcMusicVoicesSegmentGraph(Bad, 2, 0, B);
      except on Exception do Rejected := True; end;
      G.Free; Check(Rejected, 'malformed config rejects before returning graph ' + IntToStr(I));
    end;
    Rejected := False;
    try G := BuildWfcMusicVoicesSegmentGraph(C, 0, 0, B); G.Free;
    except on Exception do Rejected := True; end;
    Check(Rejected, 'zero graph size rejects');
    B[2].PreviousState := 0; Rejected := False;
    try G := BuildWfcMusicVoicesSegmentGraph(C, 2, 0, B); G.Free;
    except on Exception do Rejected := True; end;
    Check(Rejected, 'initial boundary requires minus-one predecessor');
  finally FreeConfig(C); end;
end;

procedure TestFiveVoicesNineteenSteps;
const Pitches: array[0..4] of Integer = (38, 42, 56, 57, 61);
var C: TWfcMusicVoicesGraphConfig; Tokens: TWfcModelTokens; V: TWfcMusicVoiceCells;
  F: TWfcMusicEnsembleFrame; B: TWfcMusicVoicesBoundaries; I: Integer;
  G: TGraph; R: TGraphSolveReport; D: TWfcMusicVoicesGenerated;
  Proof: TWfcMusicVoicesValidationReport;
begin
  C := Default(TWfcMusicVoicesGraphConfig); C.StepsPerOctave := 19;
  C.HarmonyMode := wmehmExact; SetLength(C.Voices, 5); SetLength(V, 5); SetLength(Tokens, 1);
  try
    for I := 0 to 4 do
    begin
      V[I] := Cell(wmcaAttack, [Pitches[I]], 70 + I);
      Tokens[0] := Single(V[I]); C.Voices[I].Model := LearnSequenceModel(Tokens, 1);
      C.Voices[I].MinPitch := Pitches[I]; C.Voices[I].MaxPitch := Pitches[I];
    end;
    F := MakeWfcMusicEnsembleFrame(V);
    Tokens[0] := EncodeWfcMusicPitchClassSet(ProjectWfcMusicEnsembleFrameToPitchClassSet(F, 19));
    C.HarmonyModel := LearnSequenceModel(Tokens, 1);
    Tokens[0] := EncodeWfcMusicRhythmFrame(ProjectWfcMusicEnsembleFrameToRhythm(F));
    C.RhythmModel := LearnSequenceModel(Tokens, 1);
    B := Initial(C); G := BuildWfcMusicVoicesSegmentGraph(C, 1, Cardinal($FFFFFFFF), B);
    try
      Check(G.TrySolve(DefaultGraphSolveOptions, R), 'five independent voices solve in nineteen-step tuning');
      Check(CaptureSolvedWfcMusicVoices(C, G, B, D, Proof), 'dynamic five-voice output independently validates');
      Check((Length(D.Layers) = 7) and (Length(D.Frames[0].Voices) = 5) and
        (G.TotalPassCount = 10), 'model/output/pass vectors are not fixed to three voices');
      Check((D.Coverage[2].PitchClass = 18) and (D.Coverage[2].Suppliers[0] = 2),
        'non-twelve-step edge class chooses its actual supplying voice');
      for I := 0 to 4 do Check(D.Frames[0].Voices[I].Tones[0].Velocity = 70 + I,
        'per-voice velocity remains independent');
      G.PassGraph[0].CurrentPass := 'changed-harmony';
      Check(not CaptureSolvedWfcMusicVoices(C, G, B, D, Proof) and
        (Proof.Issue.Kind = wmvikGraphShape), 'pass-label tampering cannot retain a trusted layout');
    finally G.Free; end;
  finally FreeConfig(C); end;
end;

{$IFDEF PAS2JS}
function BadNumber(const I: Integer): NativeInt;
begin
  asm
    if (I === 0) Result = 1.5;
    else if (I === 1) Result = NaN;
    else if (I === 2) Result = Infinity;
    else Result = 9007199254740992;
  end;
end;

procedure TestBrowserNumbers;
var C, Bad: TWfcMusicVoicesGraphConfig; B: TWfcMusicVoicesBoundaries;
  G: TGraph; I, J: Integer; Rejected: Boolean;
begin
  C := Fixture;
  try
    for I := 0 to 3 do for J := 0 to 7 do
    begin
      Bad := CopyWfcMusicVoicesGraphConfig(C); B := Initial(C); G := nil; Rejected := False;
      try
        case J of
          0: Bad.StepsPerOctave := BadNumber(I);
          1: Bad.Voices[0].MinPitch := BadNumber(I);
          2: Bad.PairConstraints[0].LowerVoice := BadNumber(I);
          3: Bad.PairConstraints[0].MinGap := BadNumber(I);
          4: Bad.HarmonyMode := TWfcMusicEnsembleHarmonyMode(BadNumber(I));
          5: B[2].PreviousState := BadNumber(I);
        end;
        if J = 6 then G := BuildWfcMusicVoicesSegmentGraph(Bad, BadNumber(I), 0, B)
        else if J = 7 then G := BuildWfcMusicVoicesSegmentGraph(Bad, 2, BadNumber(I), B)
        else G := BuildWfcMusicVoicesSegmentGraph(Bad, 2, 0, B);
      except on Exception do Rejected := True; end;
      G.Free; Check(Rejected, 'browser malformed graph number rejects ' + IntToStr(I) + '/' + IntToStr(J));
    end;
  finally FreeConfig(C); end;
end;
{$ENDIF}

begin
  try
    TestIndependentCombinations;
    TestRangesPairsAndProof;
    TestNegotiation;
    TestIndependentHarmonyProof;
    TestCanonicalWitnessSearch;
    TestSilenceAndLargeTuning;
    TestHeldSegments;
    TestConfigurationGuards;
    TestFiveVoicesNineteenSteps;
    {$IFDEF PAS2JS}TestBrowserNumbers;{$ENDIF}
  except on E: Exception do begin Inc(Failures); WriteLn(E.ClassName, ': ', E.Message); end; end;
  WriteLn('Independent voice graph checks: ', Checks, ', failures: ', Failures);
  if Failures <> 0 then Halt(1);
end.
