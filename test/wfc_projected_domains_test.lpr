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
program wfc_projected_domains_test;
{$mode delphi}{$H+}
uses SysUtils, Classes, wfc, wfc_solve_progress, wfc_model, wfc_sequence,
  wfc_sequence_learn, wfc_sequence_graph, wfc_music, wfc_music_sequence,
  wfc_music_ensemble, wfc_music_ensemble_graph, wfc_music_voices_graph,
  wfc_music_voices_stream, wfc_music_arrangement
  {$IFDEF PAS2JS}{$IFNDEF WFC_NODE_DOMAIN_TEST}, wfc_browser_test_host{$ENDIF}{$ENDIF};

type
  TAbortObserver = class
    Calls: Integer;
    Graph: TGraph;
    procedure Observe(const P: TWfcSolveProgress);
  end;
var Checks: Integer;

procedure Check(const B: Boolean; const S: String);
begin Inc(Checks); if not B then raise Exception.Create(S) end;

procedure TAbortObserver.Observe(const P: TWfcSolveProgress);
var Blocked: Boolean;
begin
  Inc(Calls);
  if P.Done = 0 then Exit;
  Blocked := False;
  try Graph.SetAllowedValues(0, 0, 0, 'a');
  except on E: EInvalidOperation do Blocked := True end;
  Check(Blocked, 'preparation observer cannot mutate graph domains');
  raise EAbort.Create('projected preparation cancelled');
end;

function Simple: TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Reshape(3, 1, 1); Result.WrapNeighbors := False;
    Result.CurrentPass := 'source'; Result.PassMode := gpmOverlay;
    Result.AddValue('a').NewRule([gdEast, gdWest], 'a');
    Result.AddValue('b').NewRule([gdEast, gdWest], 'b');
    Result.SwitchToPass('middle'); Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('x').RequireFromPass('source', 'a');
    Result.AddValue('y').RequireFromPass('source', 'b');
    Result.SwitchToPass('target'); Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('u').RequireFromPass('middle', 'x');
    Result.AddValue('v').RequireFromPass('middle', 'y');
    Result.Seed := 1704297163;
  except Result.Free; raise end;
end;

procedure TestDomains;
var G: TGraph; D: TGraphPreparedDomains; R: TGraphDomainPreparationReport;
  I, P: Integer; O: TAbortObserver; Raised: Boolean; Selection: String;
begin
  G := Simple; O := nil;
  try
    Selection := G.CurrentPass;
    Check(G.PrepareProjectedDomains(D, R), 'unrestricted chain remains locally consistent');
    for P := 0 to 2 do for I := 0 to 2 do
      Check(Length(D[P].Cells[I]) = 2, 'both independently feasible alternatives survive');
    Check((G.CurrentPass = Selection) and (G.Seed = 1704297163),
      'preparation preserves selected pass and seed');
    Check(G.PassGraph[0].Entry[0, 0, 0].Empty and not G.PassGraph[0].HasAllowedValues(0, 0, 0),
      'detached preparation chooses no values and changes no masks');
    D[0].Cells[0][0] := 'detached';
    G.SetAllowedValues(2, 0, 0, 'u');
    Check(G.PrepareProjectedDomains(D, R), 'future target restriction propagates upstream');
    Check((R.Iterations > 1) and (R.RemovedCandidates = 8),
      'closure removes eight further candidates beyond the caller restriction');
    for I := 0 to 2 do
    begin
      Check((Length(D[0].Cells[I]) = 1) and (D[0].Cells[I][0] = 'a'),
        'future restriction reaches all temporal source cells');
      Check((Length(D[1].Cells[I]) = 1) and (D[1].Cells[I][0] = 'x'),
        'temporal source support propagates forward again');
      Check((Length(D[2].Cells[I]) = 1) and (D[2].Cells[I][0] = 'u'),
        'all target cells reach same fixed point');
    end;
    Check(not G.PassGraph[0].HasAllowedValues(0, 0, 0), 'original source masks remain absent');
    G.PassGraph[0].Entry[1, 0, 0].Value := 'b';
    Check(not G.PrepareProjectedDomains(D, R) and not R.Consistent and (Length(D) = 0),
      'contradictory caller lock is rejected and output cleared');
    G.PassGraph[0].Entry[1, 0, 0].ClearValue;
    Check(G.PrepareProjectedDomains(D, R), 'fresh preparation recovers without stale domains');
    G.Rules['u'].RequireFromPassAt('source', MakeGraphOffset(1, 0, 0), ['a']);
    Raised := False;
    try G.PrepareProjectedDomains(D, R);
    except on E: EInvalidOperation do Raised := True end;
    Check(Raised and (Length(D) = 3) and not G.Running,
      'unsupported offset preserves old output and releases running guard');
  finally G.Free end;
  G := Simple;
  try
    SetLength(D, 1); D[0].LabelText := 'old';
    O := TAbortObserver.Create; O.Graph := G; Raised := False;
    try G.PrepareProjectedDomains(D, R, O.Observe);
    except on E: EAbort do Raised := E.Message = 'projected preparation cancelled' end;
    Check(Raised and (O.Calls > 1) and (Length(D) = 1) and (D[0].LabelText = 'old'),
      'interior cancellation preserves caller output');
    Check(not G.Running and (G.CurrentPass = 'target') and
      G.PassGraph[0].Entry[0, 0, 0].Empty, 'cancellation preserves graph ownership');
  finally O.Free; G.Free end;
end;

function Fixture: TWfcMusicVoicesGraphConfig;
var F: TWfcMusicEnsembleFrame; H, R, V: String;
begin
  Result := Default(TWfcMusicVoicesGraphConfig);
  SetLength(F.Voices, 1);
  F.Voices[0] := MakeWfcMusicVoiceCell(wmcaAttack, [MakeWfcMusicTone(60, 80)]);
  H := EncodeWfcMusicPitchClassSet(ProjectWfcMusicEnsembleFrameToPitchClassSet(F, 12));
  R := EncodeWfcMusicRhythmFrame(ProjectWfcMusicEnsembleFrameToRhythm(F));
  V := EncodeWfcMusicEnsembleFrame(F);
  Result.HarmonyModel := LearnSequenceModel([H, H, H, H], 1);
  Result.RhythmModel := LearnSequenceModel([R, R, R, R], 1);
  SetLength(Result.Voices, 1);
  Result.Voices[0].Model := LearnSequenceModel([V, V, V, V], 1);
  Result.Voices[0].MinPitch := 60; Result.Voices[0].MaxPitch := 60;
  Result.StepsPerOctave := 12; Result.HarmonyMode := wmehmExact;
  SetLength(Result.Auxiliary, 1); Result.Auxiliary[0].Name := 'caller-source';
  Result.Auxiliary[0].Model := LearnSequenceModel(['a', 'b', 'a', 'b'], 1);
  SetLength(Result.Projections, 1);
  Result.Projections[0].SourceModelIndex := 3; Result.Projections[0].TargetModelIndex := 0;
  SetLength(Result.Projections[0].Rules, 1);
  Result.Projections[0].Rules[0] := MakeWfcSequenceProjectionRule(H, ['a']);
end;

procedure Dispose(var C: TWfcMusicVoicesGraphConfig);
begin
  C.HarmonyModel.Free; C.RhythmModel.Free; C.Voices[0].Model.Free;
  C.Auxiliary[0].Model.Free;
end;

procedure TestOriginal;
var C: TWfcMusicVoicesGraphConfig; B: TWfcMusicVoicesBoundaries;
  G: TGraph; D: TGraphPreparedDomains; P: TGraphDomainPreparationReport;
  O: TGraphNegotiationOptions; N: TGraphNegotiationReport;
  Generated: TWfcMusicVoicesGenerated; Proof: TWfcMusicVoicesValidationReport;
  I, J: Integer;
begin
  C := Fixture; G := nil;
  try
    SetLength(B, 4);
    for I := 0 to 3 do B[I] := MakeWfcSequenceInitialSegmentBoundary(False);
    O := DefaultGraphNegotiationOptions;
    O.SolveOptions.MaxBacktracks := 256; O.MaxPassBacktracks := 16;
    G := BuildWfcMusicVoicesSegmentGraph(C, 64, 1704297163, B);
    Check(not G.TrySolveNegotiated(O, N) and (N.Status = gnsPassBacktrackLimit) and
      (N.PassBacktracks = 16), 'retained original free case reaches its unchanged search limit');
    FreeAndNil(G);
    G := BuildWfcMusicVoicesSegmentGraph(C, 64, 1704297163, B);
    Check(G.PrepareProjectedDomains(D, P), 'known target domains prepare without source choices');
    for I := 0 to High(D) do for J := 0 to High(D[I].Cells) do
      G.PassGraph[I].SetAllowedValues(J, 0, 0, D[I].Cells[J]);
    Check(G.TrySolveNegotiated(O, N) and (N.PassBacktracks = 0),
      'same models seed and budgets now solve with zero retries');
    Check(CaptureSolvedWfcMusicVoices(C, G, B, Generated, Proof) and
      (Proof.CheckedCells = 64), 'independent original model and latent-path proof passes');
    for I := 0 to 63 do
      Check(Generated.Layers[3].Tokens[I] = 'a', 'source respects declared target support');
    FreeAndNil(G);
    G := BuildWfcMusicVoicesSegmentGraph(C, 64, 1704297163, B);
    for I := 0 to 63 do
      IntersectSequenceAllowedTokens(C.Auxiliary[0].Model, G.PassGraph[3], I, 'a');
    Check(G.TrySolveNegotiated(O, N) and CaptureSolvedWfcMusicVoices(C, G, B,
      Generated, Proof), 'explicit original feasible witness remains valid');
    WriteLn('Original64: unprepared16 retries; prepared0; same seed1704297163 and256/16 limits.');
  finally G.Free; Dispose(C) end;
end;

procedure TestStream;
var C: TWfcMusicVoicesGraphConfig; Config: TWfcMusicVoicesStreamConfig;
  Stream: TWfcMusicVoicesStream; Segment: TWfcMusicVoicesSegment;
  N: TGraphNegotiationReport; I, J, Count: Integer; L: TWfcGeneratedSequenceSegment;
begin
  C := Fixture; Stream := nil; Segment := nil;
  try
    Config := DefaultWfcMusicVoicesStreamConfig(C, 1, 64, 1704297163);
    Config.SegmentCellCount := 8; Config.LookaheadCellCount := 64;
    Config.PrepareProjectedDomains := True;
    Stream := TWfcMusicVoicesStream.Create(Config);
    Stream.IntersectAllowedTokens(3, 63, ['a']);
    Count := 0;
    for I := 0 to 7 do
    begin
      Check(Stream.Next(Segment, N) = wmaspProduced, 'prepared bounded prefix produces');
      try
        Check((Segment.CellCount = 8) and (Segment.PlanningCellCount = 64 - I * 8),
          'only published prefix commits from full remaining lookahead');
        Check((N.PassBacktracks = 0) and Stream.LastPreparation.Consistent,
          'prepared stream avoids known-domain rejection retries');
        L := Segment.CopyGenerated(3);
        for J := 0 to High(L.Tokens) do
          Check(L.Tokens[J] = 'a', 'shared source frontier retains supported decisions');
        Inc(Count, Segment.CellCount);
      finally FreeAndNil(Segment) end;
    end;
    Check((Count = 64) and (Stream.ProducedTicks = 64) and
      (Stream.Next(Segment, N) = wmaspCompleted), 'prepared stream ends exactly');
  finally Segment.Free; Stream.Free; Dispose(C) end;
end;

begin
  TestDomains; TestOriginal; TestStream;
  WriteLn('Projected domain checks: ', Checks);
end.
