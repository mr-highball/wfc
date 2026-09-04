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
program wfc_music_graph_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_music_sequence,
  wfc_music_graph,
  wfc_sequence,
  wfc_sequence_learn,
  wfc_sequence_graph;

type
  TTestProcedure = procedure;

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

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try
    ATest;
  except
    on E: Exception do
    begin
      Inc(GFailureCount);
      WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function MelodyCellsOf(const AValues: array of TWfcMusicMelodyCell):
  TWfcMusicMelodyCells;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function MakeRhythmCell(
  const AAction: TWfcMusicCellAction): TWfcMusicRhythmCell;
begin
  Result := Default(TWfcMusicRhythmCell);
  Result.Action := AAction;
end;

function RhythmCellsOf(const AValues: array of TWfcMusicRhythmCell):
  TWfcMusicRhythmCells;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function MakeHarmonyCell(const APitchClass,
  AStepsPerOctave: Integer): TWfcMusicHarmonyCell;
begin
  Result := Default(TWfcMusicHarmonyCell);
  Result.Kind := wmhckPitchClass;
  Result.StepsPerOctave := AStepsPerOctave;
  Result.PitchClass := APitchClass;
end;

function HarmonyCellsOf(const AValues: array of TWfcMusicHarmonyCell):
  TWfcMusicHarmonyCells;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function SamplesOf(const AValues: array of TWfcSequenceSample):
  TWfcSequenceSamples;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function TokensMatch(const AActual,
  AExpected: TWfcModelTokens): Boolean;
var
  I: Integer;
begin
  if Length(AActual) <> Length(AExpected) then
    Exit(False);
  for I := 0 to Length(AExpected) - 1 do
    if AActual[I] <> AExpected[I] then
      Exit(False);
  Result := True;
end;

function ProjectionRuleForAction(
  const ARules: TWfcSequenceProjectionRules;
  const AAction: TWfcMusicCellAction;
  out ARule: TWfcSequenceProjectionRule): Boolean;
var
  I: Integer;
begin
  ARule := Default(TWfcSequenceProjectionRule);
  for I := 0 to Length(ARules) - 1 do
    if DecodeWfcMusicMelodyCell(ARules[I].TargetToken).Action =
        AAction then
    begin
      ARule := ARules[I];
      Exit(True);
    end;
  Result := False;
end;

function CallRejectedWithoutDependencyMutation(
  const AMelodyModel, ARhythmModel,
  AHarmonyModel: TWfcSequenceModel; const AGraph: TGraph;
  const ARhythmPass, AHarmonyPass: String;
  const AStepsPerOctave: Integer; out AMessage: String): Boolean;
var
  LDependencyCountBefore: Integer;
begin
  AMessage := '';
  LDependencyCountBefore := AGraph.DependencyCount;
  Result := False;
  try
    RequireWfcMusicMelodyFromPasses(AMelodyModel,
      ARhythmModel, AHarmonyModel, AGraph, ARhythmPass, AHarmonyPass,
      AStepsPerOctave);
  except
    on E: Exception do
    begin
      Result := True;
      AMessage := E.Message;
    end;
  end;
  Result := Result and
    (AGraph.DependencyCount = LDependencyCountBefore) and
    (Pos('@wfcs', AMessage) = 0);
end;

procedure TestThreePassMusicComposition;
var
  I: Integer;
  LAlternativeHarmony: TWfcMusicHarmonyCells;
  LAlternativeSolved: Boolean;
  LCapturedHarmony: TWfcGeneratedSequence;
  LCapturedMelody: TWfcGeneratedSequence;
  LCapturedRhythm: TWfcGeneratedSequence;
  LDesiredHarmony: TWfcMusicHarmonyCells;
  LDesiredMelody: TWfcMusicMelodyCells;
  LDesiredRhythm: TWfcMusicRhythmCells;
  LErrorMessage: String;
  LGraph: TGraph;
  LHarmonyModel: TWfcSequenceModel;
  LHarmonyMismatchModel: TWfcSequenceModel;
  LHarmonyMissingModel: TWfcSequenceModel;
  LHarmonyRules: TWfcSequenceProjectionRules;
  LMelodyCa: TWfcMusicMelodyCell;
  LMelodyCh: TWfcMusicMelodyCell;
  LMelodyGa: TWfcMusicMelodyCell;
  LMelodyModel: TWfcSequenceModel;
  LMelodyRest: TWfcMusicMelodyCell;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRestRule: TWfcSequenceProjectionRule;
  LRhythmModel: TWfcSequenceModel;
  LRhythmMissingModel: TWfcSequenceModel;
  LValidation: TWfcSequenceGraphValidationReport;
  LValue: TGraphValue;
begin
  LGraph := nil;
  LHarmonyModel := nil;
  LHarmonyMismatchModel := nil;
  LHarmonyMissingModel := nil;
  LMelodyModel := nil;
  LRhythmModel := nil;
  LRhythmMissingModel := nil;
  LMelodyCa := MakeWfcMusicAttackCell(60, 90);
  LMelodyCh := MakeWfcMusicHoldCell(60, 90);
  LMelodyGa := MakeWfcMusicAttackCell(67, 100);
  LMelodyRest := MakeWfcMusicRestCell;
  LDesiredMelody := MelodyCellsOf([
    LMelodyCa, LMelodyCa, LMelodyRest, LMelodyGa]);
  LDesiredRhythm := RhythmCellsOf([
    MakeRhythmCell(wmcaAttack), MakeRhythmCell(wmcaAttack),
    MakeRhythmCell(wmcaRest), MakeRhythmCell(wmcaAttack)]);
  LDesiredHarmony := HarmonyCellsOf([
    MakeHarmonyCell(0, 12), MakeHarmonyCell(0, 12),
    MakeHarmonyCell(7, 12), MakeHarmonyCell(7, 12)]);
  LAlternativeHarmony := HarmonyCellsOf([
    MakeHarmonyCell(0, 12), MakeHarmonyCell(0, 12),
    MakeHarmonyCell(0, 12), MakeHarmonyCell(7, 12)]);
  try
    LMelodyModel := LearnSequenceModelCorpus(SamplesOf([
      MakeWfcSequenceSample(EncodeWfcMusicMelodyCells(
        LDesiredMelody)),
      MakeWfcSequenceSample(EncodeWfcMusicMelodyCells(MelodyCellsOf([
        LMelodyCa, LMelodyCh, LMelodyRest, LMelodyGa]))),
      MakeWfcSequenceSample(EncodeWfcMusicMelodyCells(MelodyCellsOf([
        LMelodyCa, LMelodyGa, LMelodyRest, LMelodyGa])))
      ]), 2);
    LRhythmModel := LearnSequenceModelCorpus(SamplesOf([
      MakeWfcSequenceSample(EncodeWfcMusicRhythmCells(
        LDesiredRhythm)),
      MakeWfcSequenceSample(EncodeWfcMusicRhythmCells(RhythmCellsOf([
        MakeRhythmCell(wmcaAttack), MakeRhythmCell(wmcaHold),
        MakeRhythmCell(wmcaRest), MakeRhythmCell(wmcaAttack)])))
      ]), 2);
    LHarmonyModel := LearnSequenceModelCorpus(SamplesOf([
      MakeWfcSequenceSample(EncodeWfcMusicHarmonyCells(
        LDesiredHarmony)),
      MakeWfcSequenceSample(EncodeWfcMusicHarmonyCells(
        LAlternativeHarmony))
      ]), 2);
    LRhythmMissingModel := LearnSequenceModel(
      EncodeWfcMusicRhythmCells(RhythmCellsOf([
        MakeRhythmCell(wmcaAttack), MakeRhythmCell(wmcaRest),
        MakeRhythmCell(wmcaAttack)])), 2);
    LHarmonyMissingModel := LearnSequenceModel(
      EncodeWfcMusicHarmonyCells(HarmonyCellsOf([
        MakeHarmonyCell(0, 12), MakeHarmonyCell(0, 12)])), 2);
    LHarmonyMismatchModel := LearnSequenceModel(
      EncodeWfcMusicHarmonyCells(HarmonyCellsOf([
        MakeHarmonyCell(0, 24), MakeHarmonyCell(7, 24)])), 2);

    Check((LMelodyModel.Order = 2) and
      (LRhythmModel.Order = 2) and (LHarmonyModel.Order = 2),
      'harmony, rhythm, and melody use order-two latent fixtures');
    LHarmonyRules := BuildWfcMusicHarmonyProjectionRules(
      LMelodyModel, LHarmonyModel, 12);
    Check(ProjectionRuleForAction(LHarmonyRules, wmcaRest,
      LRestRule) and (Length(LRestRule.SourceTokens) = 2),
      'a melody rest accepts both learned harmony source alternatives');

    LGraph := TGraph.Create;
    LGraph.Reshape(4, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.Seed := 0;
    LGraph.CurrentPass := 'harmony';
    ApplySequenceModelToGraph(LHarmonyModel, LGraph);
    for I := 0 to Length(LDesiredHarmony) - 1 do
      IntersectSequenceAllowedTokens(LHarmonyModel, LGraph, I,
        EncodeWfcMusicHarmonyCell(LDesiredHarmony[I]));
    LGraph.SwitchToPass('rhythm');
    ApplySequenceModelToGraph(LRhythmModel, LGraph);
    for I := 0 to Length(LDesiredRhythm) - 1 do
      IntersectSequenceAllowedTokens(LRhythmModel, LGraph, I,
        EncodeWfcMusicRhythmCell(LDesiredRhythm[I]));
    LGraph.SwitchToPass('melody');
    ApplySequenceModelToGraph(LMelodyModel, LGraph);

    Check(CallRejectedWithoutDependencyMutation(LMelodyModel,
      LRhythmMissingModel, LHarmonyModel, LGraph,
      'rhythm', 'harmony', 12,
      LErrorMessage),
      'a missing rhythm token is rejected before dependency mutation');
    Check(CallRejectedWithoutDependencyMutation(LMelodyModel,
      LRhythmModel, LHarmonyMissingModel, LGraph,
      'rhythm', 'harmony', 12,
      LErrorMessage),
      'a missing harmony token is rejected before dependency mutation');
    Check(CallRejectedWithoutDependencyMutation(LMelodyModel,
      LRhythmModel, LHarmonyMismatchModel, LGraph,
      'rhythm', 'harmony', 12,
      LErrorMessage),
      'a steps-per-octave mismatch is rejected atomically');
    Check(CallRejectedWithoutDependencyMutation(LMelodyModel,
      LRhythmModel, LHarmonyModel, LGraph,
      'rhythm', 'missing-harmony', 12,
      LErrorMessage),
      'a missing second source pass is rejected before either map mutates');

    RequireWfcMusicMelodyFromPasses(LMelodyModel,
      LRhythmModel, LHarmonyModel, LGraph, 'rhythm', 'harmony', 12);
    Check(LGraph.DependencyCount = 2,
      'melody declares distinct rhythm and harmony dependencies');
    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport),
      'the constrained three-pass music pipeline solves');
    Check(CaptureSolvedSequence(LHarmonyModel, LGraph.PassGraph[0],
      LCapturedHarmony, LValidation) and
      TokensMatch(LCapturedHarmony.Tokens,
        EncodeWfcMusicHarmonyCells(LDesiredHarmony)),
      'the harmony source captures its exact public progression');
    Check(CaptureSolvedSequence(LRhythmModel, LGraph.PassGraph[1],
      LCapturedRhythm, LValidation) and
      TokensMatch(LCapturedRhythm.Tokens,
        EncodeWfcMusicRhythmCells(LDesiredRhythm)),
      'the rhythm source captures its exact public progression');
    Check(CaptureSolvedSequence(LMelodyModel, LGraph.PassGraph[2],
      LCapturedMelody, LValidation) and
      TokensMatch(LCapturedMelody.Tokens,
        EncodeWfcMusicMelodyCells(LDesiredMelody)),
      'melody is simultaneously narrowed by rhythm action and harmony pitch');
    Check((Pos('@wfcs', String(LCapturedMelody.Tokens[0])) = 0) and
      (Pos('@wfcs', String(LCapturedHarmony.Tokens[0])) = 0) and
      (Pos('@wfcs', String(LCapturedRhythm.Tokens[0])) = 0),
      'captured music exposes no private sequence key');

    LGraph.SwitchToPass('harmony');
    LGraph.ClearAllowedValues(2, 0, 0);
    IntersectSequenceAllowedTokens(LHarmonyModel, LGraph, 2,
      EncodeWfcMusicHarmonyCell(LAlternativeHarmony[2]));
    LGraph.SwitchToPass('melody');
    LAlternativeSolved := LGraph.TrySolve(LOptions, LReport);
    Check(LAlternativeSolved,
      'the alternate harmony source path remains satisfiable');
    Check(LAlternativeSolved and
      CaptureSolvedSequence(LHarmonyModel, LGraph.PassGraph[0],
        LCapturedHarmony, LValidation) and
      TokensMatch(LCapturedHarmony.Tokens,
        EncodeWfcMusicHarmonyCells(LAlternativeHarmony)),
      'the alternate public harmony is selected at the rest');
    Check(LAlternativeSolved and
      CaptureSolvedSequence(LMelodyModel, LGraph.PassGraph[2],
        LCapturedMelody, LValidation) and
      TokensMatch(LCapturedMelody.Tokens,
        EncodeWfcMusicMelodyCells(LDesiredMelody)),
      'either harmony alternative supports the same public melody rest');

    for I := 0 to 3 do
    begin
      LValue := LGraph.PassGraph[0].Entry[I, 0, 0].Value;
      LGraph.PassGraph[0].Entry[I, 0, 0].Value := LValue;
      LGraph.PassGraph[0].ClearAllowedValues(I, 0, 0);
    end;
    Check((not LGraph.PassGraph[0].Entry[0, 0, 0].Generated) and
      (not LGraph.PassGraph[0].Entry[3, 0, 0].Generated),
      'the solved harmony progression can be promoted to caller locks');
    LGraph.Seed := $DEADBEEF;
    Check(LGraph.TrySolve(LOptions, LReport) and
      CaptureSolvedSequence(LHarmonyModel, LGraph.PassGraph[0],
        LCapturedHarmony, LValidation) and
      CaptureSolvedSequence(LMelodyModel, LGraph.PassGraph[2],
        LCapturedMelody, LValidation) and
      TokensMatch(LCapturedHarmony.Tokens,
        EncodeWfcMusicHarmonyCells(LAlternativeHarmony)) and
      TokensMatch(LCapturedMelody.Tokens,
        EncodeWfcMusicMelodyCells(LDesiredMelody)),
      'locked harmony and deterministic public melody survive a new seed');
  finally
    LGraph.Free;
    LHarmonyMismatchModel.Free;
    LHarmonyMissingModel.Free;
    LRhythmMissingModel.Free;
    LHarmonyModel.Free;
    LRhythmModel.Free;
    LMelodyModel.Free;
  end;
end;

begin
  WriteLn('WFC music graph conformance suite');
  WriteLn('=================================');
  RunTest('three-pass music composition',
    @TestThreePassMusicComposition);
  WriteLn('=================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d music graph checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
