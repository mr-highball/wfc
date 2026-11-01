(*
MIT License

Copyright (c) 2021, 2026 mr-highball

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
program wfc_music_voices_composition_test;
{$mode delphi}{$H+}
uses SysUtils, wfc, wfc_model, wfc_sequence, wfc_sequence_learn,
  wfc_sequence_graph, wfc_preparation, wfc_music, wfc_music_sequence,
  wfc_music_ensemble, wfc_music_ensemble_graph, wfc_music_voices_graph,
  wfc_music_voices_stream, wfc_music_arrangement
  {$IFDEF PAS2JS}{$IFNDEF WFC_NODE_COMPOSITION_TEST}, wfc_browser_test_host{$ENDIF}{$ENDIF};

type
  TAbortObserver = class
    Stage: String;
    procedure Observe(const P: TWfcPreparationProgress);
  end;
  TCancelStream = class(TWfcMusicVoicesStream)
  protected
    procedure ConfigureSegment(const AIndex, AStartTick: TWfcMusicArrangementWide;
      const ACellCount: Integer; const AGraph: TGraph); override;
  end;
var Checks: Integer;

procedure Check(const B: Boolean; const S: String);
begin Inc(Checks); if not B then raise Exception.Create(S); end;

function ChoiceModel(const A, B: String): TWfcSequenceModel;
var Samples: TWfcSequenceSamples;
begin
  SetLength(Samples, 2);
  Samples[0] := MakeWfcSequenceSample([A, A, A, A]);
  Samples[1] := MakeWfcSequenceSample([B, B, B, B]);
  { Order two encodes the authored self transitions; order one intentionally
    has no history and would permit arbitrary switching between a and b. }
  Result := LearnSequenceModelCorpus(Samples, 2);
end;

function Fixture: TWfcMusicVoicesGraphConfig;
var F: TWfcMusicEnsembleFrame; H0, H1, R, V: String;
begin
  Result := Default(TWfcMusicVoicesGraphConfig);
  SetLength(F.Voices, 1);
  F.Voices[0] := MakeWfcMusicVoiceCell(wmcaAttack, [MakeWfcMusicTone(60, 80)]);
  V := EncodeWfcMusicEnsembleFrame(F);
  H0 := EncodeWfcMusicPitchClassSet(ProjectWfcMusicEnsembleFrameToPitchClassSet(F, 12));
  R := EncodeWfcMusicRhythmFrame(ProjectWfcMusicEnsembleFrameToRhythm(F));
  F.Voices[0] := MakeWfcMusicVoiceCell(wmcaAttack, [MakeWfcMusicTone(67, 80)]);
  H1 := EncodeWfcMusicPitchClassSet(ProjectWfcMusicEnsembleFrameToPitchClassSet(F, 12));
  Result.HarmonyModel := ChoiceModel(H0, H1);
  Result.RhythmModel := ChoiceModel(R, R);
  SetLength(Result.Voices, 1);
  Result.Voices[0].Model := ChoiceModel(V, V);
  Result.Voices[0].MinPitch := 60; Result.Voices[0].MaxPitch := 60;
  Result.StepsPerOctave := 12; Result.HarmonyMode := wmehmExact;
  SetLength(Result.Auxiliary, 2);
  Result.Auxiliary[0].Name := 'caller-context';
  Result.Auxiliary[0].Model := ChoiceModel('a', 'b');
  Result.Auxiliary[1].Name := 'caller-pattern';
  Result.Auxiliary[1].Model := ChoiceModel('u', 'v');
  SetLength(Result.Projections, 2);
  Result.Projections[0].SourceModelIndex := 3;
  Result.Projections[0].TargetModelIndex := 0;
  SetLength(Result.Projections[0].Rules, 2);
  Result.Projections[0].Rules[0] := MakeWfcSequenceProjectionRule(H0, ['a']);
  Result.Projections[0].Rules[1] := MakeWfcSequenceProjectionRule(H1, ['b']);
  Result.Projections[1].SourceModelIndex := 1;
  Result.Projections[1].TargetModelIndex := 4;
  SetLength(Result.Projections[1].Rules, 2);
  Result.Projections[1].Rules[0] := MakeWfcSequenceProjectionRule('u', [R]);
  Result.Projections[1].Rules[1] := MakeWfcSequenceProjectionRule('v', nil);
end;

procedure Dispose(var C: TWfcMusicVoicesGraphConfig);
var I: Integer;
begin
  C.HarmonyModel.Free; C.RhythmModel.Free;
  for I := 0 to High(C.Voices) do C.Voices[I].Model.Free;
  for I := 0 to High(C.Auxiliary) do C.Auxiliary[I].Model.Free;
  C := Default(TWfcMusicVoicesGraphConfig);
end;

function Initial(const C: TWfcMusicVoicesGraphConfig): TWfcMusicVoicesBoundaries;
var I: Integer;
begin
  Result := nil; SetLength(Result, WfcMusicVoicesModelCount(C));
  for I := 0 to High(Result) do Result[I] := MakeWfcSequenceInitialSegmentBoundary(False);
end;

procedure TAbortObserver.Observe(const P: TWfcPreparationProgress);
begin if P.Stage = Stage then raise Exception.Create('composition observer abort'); end;

procedure TCancelStream.ConfigureSegment(const AIndex, AStartTick: TWfcMusicArrangementWide;
  const ACellCount: Integer; const AGraph: TGraph);
begin Cancel; end;

procedure TestFinite;
var C, Changed: TWfcMusicVoicesGraphConfig; B: TWfcMusicVoicesBoundaries;
  G: TGraph; R: TGraphSolveReport; N: TGraphNegotiationReport;
  D: TWfcMusicVoicesGenerated; V: TWfcMusicVoicesValidationReport;
  Seed, I: Integer; Escaped: Boolean;
begin
  C := Fixture;
  try
    B := Initial(C); Escaped := False;
    Check(WfcMusicVoicesModelCount(C) = 5, 'declared models counted');
    Check(WfcMusicVoicesPassLabel(C, 3) = 'caller-context', 'declared source label');
    for Seed := 0 to 63 do
    begin
      G := BuildWfcMusicVoicesSegmentGraph(C, 2, Seed, B);
      try
        if not G.TrySolve(DefaultGraphSolveOptions, R) then
        begin
          Check(G.TrySolveNegotiated(DefaultGraphNegotiationOptions, N),
            'context rejection must renegotiate with conditional music');
          Check(CaptureSolvedWfcMusicVoices(C, G, B, D, V),
            'declared composition captures: ' + V.Issue.Detail);
          Check((V.CheckedModels = 5) and (V.CheckedCells = 2), 'proof includes auxiliary paths');
          for I := 0 to 1 do
            Check((D.Layers[3].Tokens[I] = 'a') and (D.Layers[4].Tokens[I] = 'u'),
              'conditional upstream and downstream relations hold');
          Check(Length(D.Frames[0].Voices) = 1, 'auxiliary is not another pitched voice');
          Changed := CopyWfcMusicVoicesGraphConfig(C);
          Changed.Projections[0].Rules[0].SourceTokens[0] := 'b';
          Check(C.Projections[0].Rules[0].SourceTokens[0] = 'a', 'nested config is detached');
          Check(not ValidateWfcMusicVoicesGenerated(Changed, G, B, D, V) and
            (V.Issue.Kind = wmvikProjection), 'independent proof rejects changed declared relation');
          Inc(D.Layers[3].Boundary.MinimumFollowingCells);
          Check(not ValidateWfcMusicVoicesGenerated(C, G, B, D, V) and
            (V.Issue.Kind = wmvikBoundary), 'source following boundary is authenticated');
          Dec(D.Layers[3].Boundary.MinimumFollowingCells);
          Check(ValidateWfcMusicVoicesGenerated(C, G, B, D, V), 'restored proof passes');
          G.PassGraph[3].SetAllowedValues(0, 0, 0, []);
          Check(not ValidateWfcMusicVoicesGenerated(C, G, B, D, V) and
            (V.Issue.Kind = wmvikEntryConstraint), 'source caller domain is independently checked');
          G.PassGraph[3].ClearAllowedValues(0, 0, 0);
          Changed := CopyWfcMusicVoicesGraphConfig(C);
          Changed.Auxiliary[0].Model := ChoiceModel('a', 'c');
          try Check(not ValidateWfcMusicVoicesGenerated(Changed, G, B, D, V),
            'stale source model identity is rejected'); finally Changed.Auxiliary[0].Model.Free; end;
          G.SwitchToPass('undeclared');
          Check(not ValidateWfcMusicVoicesGenerated(C, G, B, D, V) and
            (V.Issue.Kind = wmvikGraphShape), 'undeclared pass remains rejected');
          WriteLn('Composition negotiation escape seed: ', Seed);
          Escaped := True; Break;
        end;
      finally G.Free; end;
    end;
    Check(Escaped, 'fixture exercises genuine upstream conditional rejection');
    Changed := CopyWfcMusicVoicesGraphConfig(C); Changed.Projections := nil;
    G := BuildWfcMusicVoicesSegmentGraph(Changed, 2, 0, B);
    try
      IntersectSequenceAllowedTokens(C.Auxiliary[0].Model, G.PassGraph[3], 0, 'b');
      Check(G.TrySolveNegotiated(DefaultGraphNegotiationOptions, N), 'graph without relation solves independently');
      Check(CaptureSolvedWfcMusicVoices(Changed, G, B, D, V), 'independent paths themselves are valid');
      Check(not ValidateWfcMusicVoicesGenerated(C, G, B, D, V) and
        (V.Issue.Kind = wmvikProjection), 'missing live requirements cannot bless an invalid relation');
      Check(not CaptureSolvedWfcMusicVoices(C, G, B, D, V) and
        (D.Layers = nil) and (D.Frames = nil), 'failed capture publishes no partial result');
    finally G.Free; end;
  finally Dispose(C); end;
end;

procedure TestMalformedAndAbort;
var C, Bad: TWfcMusicVoicesGraphConfig; B: TWfcMusicVoicesBoundaries;
  G: TGraph; O: TAbortObserver; I: Integer; Raised: Boolean;
  D: TWfcMusicVoicesGenerated; V: TWfcMusicVoicesValidationReport;
  N: TGraphNegotiationReport;
begin
  C := Fixture; O := TAbortObserver.Create;
  try
    B := Initial(C);
    for I := 0 to 7 do
    begin
      Bad := CopyWfcMusicVoicesGraphConfig(C);
      case I of
        0: Bad.Auxiliary[0].Name := 'harmony';
        1: Bad.Auxiliary[0].Name := Bad.Auxiliary[1].Name;
        2: Bad.Projections[0].SourceModelIndex := 99;
        3: Bad.Projections[0].TargetModelIndex := 3;
        4: Bad.Projections[0].Rules[0].SourceTokens[0] := 'unknown';
        5: Bad.Projections[0].Rules[0].TargetToken := 'unknown';
        6: begin
          SetLength(Bad.Projections, 3); Bad.Projections[2] := Bad.Projections[0];
        end;
        7: begin
          SetLength(Bad.Projections, 3);
          Bad.Projections[2].SourceModelIndex := 0;
          Bad.Projections[2].TargetModelIndex := 3;
          SetLength(Bad.Projections[2].Rules, 2);
          Bad.Projections[2].Rules[0] := MakeWfcSequenceProjectionRule('a',
            [C.HarmonyModel.PublicTokenAt(0), C.HarmonyModel.PublicTokenAt(1)]);
          Bad.Projections[2].Rules[1] := MakeWfcSequenceProjectionRule('b',
            [C.HarmonyModel.PublicTokenAt(0), C.HarmonyModel.PublicTokenAt(1)]);
        end;
      end;
      Raised := False; G := nil;
      try G := BuildWfcMusicVoicesSegmentGraph(Bad, 2, 0, B);
      except on E: Exception do Raised := True; end;
      G.Free; Check(Raised, 'malformed composition rejected ' + IntToStr(I));
    end;
    O.Stage := 'sequence_projection_visits'; G := nil; Raised := False;
    try G := BuildWfcMusicVoicesSegmentGraph(C, 2, 0, B, O.Observe);
    except on E: Exception do Raised := E.Message = 'composition observer abort'; end;
    Check(Raised and (G = nil), 'build abort disposes private graph');
    G := BuildWfcMusicVoicesSegmentGraph(C, 2, 0, B);
    try
      Check(G.TrySolveNegotiated(DefaultGraphNegotiationOptions, N), 'abort proof fixture solves');
      Check(CaptureSolvedWfcMusicVoices(C, G, B, D, V), 'abort proof fixture captures');
      O.Stage := 'sequence_projection_proof_visits'; Raised := False;
      try CaptureSolvedWfcMusicVoices(C, G, B, D, V, O.Observe);
      except on E: Exception do Raised := E.Message = 'composition observer abort'; end;
      Check(Raised and (Length(D.Layers) = 5) and (D.Layers[3].Tokens[0] = 'a'),
        'observer exception preserves existing caller result');
    finally G.Free; end;
  finally O.Free; Dispose(C); end;
end;

procedure TestStream;
var C: TWfcMusicVoicesGraphConfig; Config: TWfcMusicVoicesStreamConfig;
  P, Q: TWfcMusicVoicesStream; Cancelled: TCancelStream;
  S, T: TWfcMusicVoicesSegment; R: TGraphNegotiationReport;
  Before, After: TWfcMusicVoicesStreamFrontier; Layer: TWfcGeneratedSequenceSegment;
  Frames: TWfcMusicEnsembleFrames;
  I, J: Integer; Signature: Cardinal; Step: TWfcMusicArrangementStep;
begin
  C := Fixture;
  try
    Config := DefaultWfcMusicVoicesStreamConfig(C, 1, 8, 1704297163);
    Config.SegmentCellCount := 2; Config.LookaheadCellCount := 4;
    P := TWfcMusicVoicesStream.Create(Config); Q := TWfcMusicVoicesStream.Create(Config);
    try
      Config.Graph.Auxiliary[0].Name := 'mutated';
      Config.Graph.Projections[0].Rules[0].SourceTokens[0] := 'b';
      Check(C.Projections[0].Rules[0].SourceTokens[0] = 'a', 'caller model config remains detached');
      for I := 0 to 3 do
      begin
        Before := P.CopyFrontier;
        Step := P.Next(S, R);
        if Step <> wmaspProduced then
        begin
          for J := 0 to High(Before.Tokens) do
            WriteLn('Previous ', J, ': ', Before.StateIndices[J], ' / ', Before.Tokens[J]);
          for J := 0 to High(R.Attempts) do
            WriteLn('Attempt ', J, ': fail=', R.Attempts[J].SolveReport.FailedPassIndex,
              ' backtracked=', R.Attempts[J].BacktrackedPassIndex,
              ' kind=', Ord(R.Attempts[J].SolveReport.Contradiction.Kind));
        end;
        Check(Step = wmaspProduced, 'shared stream prefix ' + IntToStr(I) + ': ' + P.Failure);
        try
          Check(Q.Next(T, R) = wmaspProduced, 'replay produces same prefix');
          try Check(S.Signature = T.Signature, 'composition replay is deterministic'); finally T.Free; end;
          Check((S.ModelCount = 5) and (S.CellCount = 2), 'prefix contains all declared models');
          Frames := S.CopyFrames;
          Check((Length(Frames) = 2) and (Length(Frames[0].Voices) = 1),
            'frame copy decodes only pitched voices, never auxiliary tokens');
          Frames[0].Voices[0].Tones[0].Pitch := 61;
          Check(S.CopyFrames[0].Voices[0].Tones[0].Pitch = 60,
            'frame copy stays detached with auxiliary passes');
          if I < 3 then Check(S.PlanningCellCount = 4, 'full future window is retained for checking');
          After := P.CopyFrontier;
          for J := 0 to S.ModelCount - 1 do
          begin
            Layer := S.CopyGenerated(J);
            Check(Length(Layer.Tokens) = 2, 'provisional suffix is not published');
            Check(After.StateIndices[J] = Layer.StateIndices[1], 'all frontiers commit at prefix end');
            if I > 0 then Check(Layer.Boundary.PreviousState = Before.StateIndices[J],
              'each model continues from its actual published predecessor');
          end;
          Layer := S.CopyGenerated(3); Check(Layer.Tokens[0] = 'a', 'caller config mutation cannot change stream');
        finally S.Free; end;
      end;
      Check(P.Next(S, R) = wmaspCompleted, 'stream completes exactly');
    finally Q.Free; P.Free; end;
    Config := DefaultWfcMusicVoicesStreamConfig(C, 1, 8, 0);
    Config.SegmentCellCount := 2; Config.LookaheadCellCount := 4;
    P := TWfcMusicVoicesStream.Create(Config);
    try
      P.IntersectAllowedTokens(3, 3, ['b']);
      Check((P.Next(S, R) = wmaspFailed) and (S = nil) and (P.ProducedTicks = 0),
        'source contradiction in provisional suffix blocks prefix publication');
      Check(not P.CopyFrontier.HasPrevious, 'failed negotiation advances no frontier');
    finally P.Free; end;
    P := TWfcMusicVoicesStream.Create(Config);
    try
      Check(P.Next(S, R) = wmaspProduced, 'valid shared prefix before later failure');
      try
        Before := P.CopyFrontier; Signature := S.Signature;
        P.IntersectAllowedTokens(3, 5, ['b']);
        Check((P.Next(T, R) = wmaspFailed) and (T = nil), 'later incompatible source fails');
        After := P.CopyFrontier;
        Check((P.ProducedTicks = 2) and (After.EndTick = Before.EndTick),
          'later failure retains the previous published clock');
        for J := 0 to High(Before.StateIndices) do
          Check(After.StateIndices[J] = Before.StateIndices[J], 'later failure retains every frontier');
        Check(S.Signature = Signature, 'already published segment remains immutable');
      finally S.Free; end;
    finally P.Free; end;
    Cancelled := TCancelStream.Create(Config);
    try Check((Cancelled.Next(S, R) = wmaspCancelled) and (S = nil) and
      (Cancelled.ProducedTicks = 0) and not Cancelled.CopyFrontier.HasPrevious,
      'cancellation cannot partly commit context'); finally Cancelled.Free; end;
    P := TWfcMusicVoicesStream.Create(Config);
    try
      Check(P.Next(S, R) = wmaspProduced, 'signature baseline');
      Signature := S.Signature; S.Free;
    finally P.Free; end;
    Config.Graph.Auxiliary[0].Name := 'renamed-context';
    P := TWfcMusicVoicesStream.Create(Config);
    try
      Check(P.Next(S, R) = wmaspProduced, 'renamed declaration still solves');
      Check(S.Signature <> Signature, 'signature binds declared auxiliary identity'); S.Free;
    finally P.Free; end;
  finally Dispose(C); end;
end;

procedure TestNoCompositionReplay;
const Expected: array[0..3] of Cardinal =
  (3981094241, 3176484686, 3860877355, 2740477165);
var C: TWfcMusicVoicesGraphConfig; F: TWfcMusicEnsembleFrame;
  Config: TWfcMusicVoicesStreamConfig; P: TWfcMusicVoicesStream;
  S: TWfcMusicVoicesSegment; R: TGraphNegotiationReport;
  H, T, V: String; I: Integer;
begin
  { Exact signatures from the same standalone fixture built against unchanged
    448b2a3. This checks original behavior, not two runs of the new code. }
  C := Default(TWfcMusicVoicesGraphConfig); P := nil;
  try
    SetLength(F.Voices, 1);
    F.Voices[0] := MakeWfcMusicVoiceCell(wmcaAttack, [MakeWfcMusicTone(60, 80)]);
    H := EncodeWfcMusicPitchClassSet(ProjectWfcMusicEnsembleFrameToPitchClassSet(F, 12));
    T := EncodeWfcMusicRhythmFrame(ProjectWfcMusicEnsembleFrameToRhythm(F));
    V := EncodeWfcMusicEnsembleFrame(F);
    C.HarmonyModel := LearnSequenceModel([H, H, H, H], 2);
    C.RhythmModel := LearnSequenceModel([T, T, T, T], 2);
    SetLength(C.Voices, 1);
    C.Voices[0].Model := LearnSequenceModel([V, V, V, V], 2);
    C.Voices[0].MinPitch := 60; C.Voices[0].MaxPitch := 60;
    C.StepsPerOctave := 12; C.HarmonyMode := wmehmExact;
    Config := DefaultWfcMusicVoicesStreamConfig(C, 1, 8, 1704297163);
    Config.SegmentCellCount := 2; Config.LookaheadCellCount := 4;
    P := TWfcMusicVoicesStream.Create(Config);
    for I := 0 to High(Expected) do
    begin
      Check(P.Next(S, R) = wmaspProduced, 'original default prefix produces');
      try Check(S.Signature = Expected[I], 'unchanged no-composition signature ' + IntToStr(I));
      finally S.Free; end;
    end;
    Check(P.Next(S, R) = wmaspCompleted, 'original default clock ends exactly');
  finally P.Free; Dispose(C); end;
end;

begin
  TestFinite; TestMalformedAndAbort; TestStream; TestNoCompositionReplay;
  WriteLn('Voice composition checks: ', Checks, ', failures: 0');
end.
