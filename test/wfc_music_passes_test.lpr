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
program wfc_music_passes_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_model,
  wfc_sequence,
  wfc_sequence_learn,
  wfc_sequence_graph,
  wfc_music,
  wfc_music_text,
  wfc_music_sequence,
  wfc_music_passes;

const
  FIXTURE_QUANTUM = 120;
  FIXTURE_CELL_COUNT = 4;
  FIXTURE_SEED = TGraphSeed(0);
  EXPECTED_BASELINE_SIGNATURE =
    TWfcMusicCompositionSignature($1E6C0029);
  EXPECTED_REPAIRED_SIGNATURE =
    TWfcMusicCompositionSignature($FB2AD681);
  EXPECTED_FULL_NEGOTIATION_TRANSCRIPT =
    TGraphTraceSignature($2CDCEE10);

type
  TTestProcedure = procedure;

  TFixture = record
    HarmonyA: TWfcMusicHarmonyCells;
    HarmonyB: TWfcMusicHarmonyCells;
    Rhythm: TWfcMusicRhythmCells;
    RhythmB: TWfcMusicRhythmCells;
    MelodyA: TWfcMusicMelodyCells;
    MelodyB: TWfcMusicMelodyCells;
    HarmonyModel: TWfcSequenceModel;
    RhythmModel: TWfcSequenceModel;
    MelodyModel: TWfcSequenceModel;
    ScoreTemplate: TWfcMusicScore;
  end;

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

function MelodyCellsOf(const AValues: array of TWfcMusicMelodyCell):
  TWfcMusicMelodyCells; forward;
function MakeRhythmCell(
  const AAction: TWfcMusicCellAction): TWfcMusicRhythmCell; forward;
function RhythmCellsOf(const AValues: array of TWfcMusicRhythmCell):
  TWfcMusicRhythmCells; forward;
function MakeHarmonyCell(const APitchClass: Integer):
  TWfcMusicHarmonyCell; forward;
function HarmonyCellsOf(const AValues: array of TWfcMusicHarmonyCell):
  TWfcMusicHarmonyCells; forward;
function SamplesOf(const AValues: array of TWfcSequenceSample):
  TWfcSequenceSamples; forward;
function BuildScore(const AMelody: TWfcMusicMelodyCells):
  TWfcMusicScore; forward;

procedure InitializeInvalidContinuationFixture(out AFixture: TFixture);
begin
  AFixture := Default(TFixture);
  AFixture.MelodyA := MelodyCellsOf([
    MakeWfcMusicAttackCell(60, 90),
    MakeWfcMusicHoldCell(60, 90),
    MakeWfcMusicAttackCell(64, 96),
    MakeWfcMusicHoldCell(64, 96)
  ]);
  AFixture.MelodyB := MelodyCellsOf([
    MakeWfcMusicHoldCell(60, 90),
    MakeWfcMusicHoldCell(60, 90),
    MakeWfcMusicAttackCell(64, 96),
    MakeWfcMusicHoldCell(64, 96)
  ]);
  AFixture.Rhythm := RhythmCellsOf([
    MakeRhythmCell(wmcaAttack), MakeRhythmCell(wmcaHold),
    MakeRhythmCell(wmcaAttack), MakeRhythmCell(wmcaHold)
  ]);
  AFixture.RhythmB := RhythmCellsOf([
    MakeRhythmCell(wmcaHold), MakeRhythmCell(wmcaHold),
    MakeRhythmCell(wmcaAttack), MakeRhythmCell(wmcaHold)
  ]);
  AFixture.HarmonyA := HarmonyCellsOf([
    MakeHarmonyCell(0), MakeHarmonyCell(0),
    MakeHarmonyCell(4), MakeHarmonyCell(4)
  ]);
  AFixture.HarmonyB := AFixture.HarmonyA;
  AFixture.HarmonyModel := LearnSequenceModel(
    EncodeWfcMusicHarmonyCells(AFixture.HarmonyA), 4);
  AFixture.RhythmModel := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(EncodeWfcMusicRhythmCells(AFixture.RhythmB)),
    MakeWfcSequenceSample(EncodeWfcMusicRhythmCells(AFixture.Rhythm))
  ]), 4);
  AFixture.MelodyModel := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(EncodeWfcMusicMelodyCells(AFixture.MelodyB)),
    MakeWfcSequenceSample(EncodeWfcMusicMelodyCells(AFixture.MelodyA))
  ]), 4);
  AFixture.ScoreTemplate := BuildScore(AFixture.MelodyA);
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
  for I := 0 to High(AValues) do
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
  for I := 0 to High(AValues) do
    Result[I] := AValues[I];
end;

function MakeHarmonyCell(const APitchClass: Integer):
  TWfcMusicHarmonyCell;
begin
  Result := Default(TWfcMusicHarmonyCell);
  Result.Kind := wmhckPitchClass;
  Result.StepsPerOctave := 12;
  Result.PitchClass := APitchClass;
end;

function HarmonyCellsOf(const AValues: array of TWfcMusicHarmonyCell):
  TWfcMusicHarmonyCells;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do
    Result[I] := AValues[I];
end;

function SamplesOf(const AValues: array of TWfcSequenceSample):
  TWfcSequenceSamples;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do
    Result[I] := AValues[I];
end;

function TokensMatch(const A, B: TWfcModelTokens): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to High(A) do
    if A[I] <> B[I] then
      Exit(False);
  Result := True;
end;

function MelodyCellsMatch(const A, B: TWfcMusicMelodyCells): Boolean;
begin
  Result := TokensMatch(EncodeWfcMusicMelodyCells(A),
    EncodeWfcMusicMelodyCells(B));
end;

function IndicesEqual(const AValues: TGraphPassIndices;
  const AExpected: array of Integer): Boolean;
var
  I: Integer;
begin
  if Length(AValues) <> Length(AExpected) then
    Exit(False);
  for I := 0 to High(AValues) do
    if AValues[I] <> AExpected[I] then
      Exit(False);
  Result := True;
end;

function BuildScore(const AMelody: TWfcMusicMelodyCells): TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetLength(LTracks, 1);
  LTracks[0] := MakeWfcMusicTrack('lead', 'Lead');
  SetLength(LVoices, 1);
  LVoices[0] := MakeWfcMusicVoice(0, 'voice');
  SetLength(LMeters, 1);
  LMeters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, 500000);
  LSpans := RebuildWfcMusicVoiceSpans(AMelody, 0, FIXTURE_QUANTUM);
  Result := TWfcMusicScore.Create(FIXTURE_QUANTUM, 12,
    FIXTURE_CELL_COUNT * FIXTURE_QUANTUM, LTracks, LVoices,
    LMeters, LTempos, LSpans);
end;

procedure InitializeFixture(out AFixture: TFixture);
begin
  AFixture := Default(TFixture);
  AFixture.MelodyA := MelodyCellsOf([
    MakeWfcMusicAttackCell(60, 90),
    MakeWfcMusicHoldCell(60, 90),
    MakeWfcMusicAttackCell(64, 96),
    MakeWfcMusicHoldCell(64, 96)
  ]);
  AFixture.MelodyB := MelodyCellsOf([
    MakeWfcMusicAttackCell(60, 90),
    MakeWfcMusicHoldCell(60, 90),
    MakeWfcMusicAttackCell(67, 104),
    MakeWfcMusicHoldCell(67, 104)
  ]);
  AFixture.Rhythm := RhythmCellsOf([
    MakeRhythmCell(wmcaAttack), MakeRhythmCell(wmcaHold),
    MakeRhythmCell(wmcaAttack), MakeRhythmCell(wmcaHold)
  ]);
  AFixture.HarmonyA := HarmonyCellsOf([
    MakeHarmonyCell(0), MakeHarmonyCell(0),
    MakeHarmonyCell(4), MakeHarmonyCell(4)
  ]);
  AFixture.HarmonyB := HarmonyCellsOf([
    MakeHarmonyCell(0), MakeHarmonyCell(0),
    MakeHarmonyCell(7), MakeHarmonyCell(7)
  ]);
  AFixture.HarmonyModel := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(EncodeWfcMusicHarmonyCells(AFixture.HarmonyA)),
    MakeWfcSequenceSample(EncodeWfcMusicHarmonyCells(AFixture.HarmonyB))
  ]), 2);
  AFixture.RhythmModel := LearnSequenceModel(
    EncodeWfcMusicRhythmCells(AFixture.Rhythm), 2);
  AFixture.MelodyModel := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(EncodeWfcMusicMelodyCells(AFixture.MelodyA)),
    MakeWfcSequenceSample(EncodeWfcMusicMelodyCells(AFixture.MelodyB))
  ]), 2);
  AFixture.ScoreTemplate := BuildScore(AFixture.MelodyA);
end;

procedure FinalizeFixture(var AFixture: TFixture);
begin
  AFixture.ScoreTemplate.Free;
  AFixture.MelodyModel.Free;
  AFixture.RhythmModel.Free;
  AFixture.HarmonyModel.Free;
  AFixture := Default(TFixture);
end;

function CreatePipeline(const AFixture: TFixture): TWfcMusicPassPipeline;
var
  LConfig: TWfcMusicPassConfig;
begin
  LConfig := DefaultWfcMusicPassConfig(AFixture.ScoreTemplate,
    FIXTURE_QUANTUM, FIXTURE_SEED);
  LConfig.Models.Harmony := AFixture.HarmonyModel;
  LConfig.Models.Rhythm := AFixture.RhythmModel;
  LConfig.Models.Melody := AFixture.MelodyModel;
  Result := TWfcMusicPassPipeline.Create(LConfig);
end;

function CommittedFingerprint(
  const APipeline: TWfcMusicPassPipeline): String;
var
  I: Integer;
  LGenerated: TWfcGeneratedSequence;
  LLayer: TWfcMusicPassLayer;
  LReport: TWfcSequenceGraphValidationReport;
begin
  Result := '';
  for LLayer := Low(TWfcMusicPassLayer) to High(TWfcMusicPassLayer) do
  begin
    if not APipeline.TryCopyCommittedLayer(LLayer, LGenerated,
        LReport) then
    begin
      Result := Result + '|!' + IntToStr(Ord(LLayer)) + ':' +
        IntToStr(Ord(LReport.Issue.Kind));
      Continue;
    end;
    Result := Result + '|L' + IntToStr(Ord(LLayer)) + ':' +
      IntToStr(Ord(LGenerated.Boundary)) + ':' +
      IntToStr(Ord(LGenerated.Extent));
    for I := 0 to High(LGenerated.Tokens) do
      Result := Result + '|' + IntToStr(LGenerated.StateIndices[I]) + ':' +
        String(LGenerated.Tokens[I]);
  end;
end;

procedure ExpectFactoryRejected(const ASeed: TGraphSeed;
  const AQuantum: Integer; const AHarmony, ARhythm, AMelody: TWfcModelTokens;
  const AScore: TWfcMusicScore; const AMessage: String);
var
  LComposition: TWfcMusicComposition;
  LRejected: Boolean;
begin
  LComposition := nil;
  LRejected := False;
  try
    try
      LComposition := CreateWfcMusicComposition(ASeed, AQuantum,
        AHarmony, ARhythm, AMelody, AScore);
    except
      on E: Exception do
        LRejected := True;
    end;
  finally
    LComposition.Free;
  end;
  Check(LRejected, AMessage);
end;

procedure TestPublicFactoryAndSignature;
var
  LComposition: TWfcMusicComposition;
  LFixture: TFixture;
  LGenerated: TWfcGeneratedSequence;
  LHarmony, LMutatedHarmony: TWfcModelTokens;
  LMelody: TWfcModelTokens;
  LRhythm, LMutatedRhythm: TWfcModelTokens;
  LScore: TWfcMusicScore;
  LSignature: TWfcMusicCompositionSignature;
begin
  InitializeFixture(LFixture);
  LComposition := nil;
  try
    LHarmony := EncodeWfcMusicHarmonyCells(LFixture.HarmonyA);
    LRhythm := EncodeWfcMusicRhythmCells(LFixture.Rhythm);
    LMelody := EncodeWfcMusicMelodyCells(LFixture.MelodyA);
    LScore := BuildScore(LFixture.MelodyA);
    try
      LSignature := CalculateWfcMusicCompositionSignature(FIXTURE_SEED,
        FIXTURE_QUANTUM, LHarmony, LRhythm, LMelody, LScore);
      LComposition := CreateWfcMusicComposition(FIXTURE_SEED,
        FIXTURE_QUANTUM, LHarmony, LRhythm, LMelody, LScore);
      Check((LComposition.CellCount = FIXTURE_CELL_COUNT) and
        (LComposition.Seed = FIXTURE_SEED) and
        (LComposition.QuantumTicks = FIXTURE_QUANTUM),
        'checked construction preserves public configuration');
      Check((LComposition.Signature = LSignature) and
        (WfcMusicCompositionSignatureHex(LSignature) =
          IntToHex(LSignature, 8)),
        'public signature overload and stable uppercase hex agree');
      Check(not LComposition.HasLatentCapture,
        'checked public construction does not invent latent states');

      LGenerated := LComposition.CopyGenerated(wmplHarmony);
      LGenerated.Tokens[0] := EncodeWfcMusicHarmonyCell(
        LFixture.HarmonyB[0]);
      Check(TokensMatch(LComposition.CopyGenerated(wmplHarmony).Tokens,
        LHarmony), 'composition token copies are detached');
      LScore.Free;
      LScore := nil;
      LScore := LComposition.CopyScore;
      Check(EncodeWfcMusicText(LScore) =
        EncodeWfcMusicText(LFixture.ScoreTemplate),
        'composition owns an independent canonical score copy');

      LMutatedRhythm := EncodeWfcMusicRhythmCells(LFixture.Rhythm);
      LMutatedRhythm[2] := EncodeWfcMusicRhythmCell(
        MakeRhythmCell(wmcaRest));
      ExpectFactoryRejected(FIXTURE_SEED, FIXTURE_QUANTUM,
        LHarmony, LMutatedRhythm, LMelody, LScore,
        'checked construction rejects a rhythm projection mismatch');
      LMutatedHarmony := EncodeWfcMusicHarmonyCells(LFixture.HarmonyA);
      LMutatedHarmony[2] := EncodeWfcMusicHarmonyCell(
        LFixture.HarmonyB[2]);
      ExpectFactoryRejected(FIXTURE_SEED, FIXTURE_QUANTUM,
        LMutatedHarmony, LRhythm, LMelody, LScore,
        'checked construction rejects a harmony projection mismatch');
    finally
      LScore.Free;
    end;
  finally
    LComposition.Free;
    FinalizeFixture(LFixture);
  end;
end;

procedure TestVariationAndNegotiatedRepair;
var
  LAfterFailure: String;
  LBaseline, LFailed, LRepaired: TWfcMusicComposition;
  LBaselineFingerprint, LLockedFingerprint: String;
  LBaselineSignature, LRepairedSignature: TWfcMusicCompositionSignature;
  LFixture: TFixture;
  LGenerated: TWfcGeneratedSequence;
  LNegotiationOptions: TGraphNegotiationOptions;
  LOrdinaryReport: TWfcMusicPassReport;
  LPipeline: TWfcMusicPassPipeline;
  LRepairReport: TWfcMusicPassSelectiveNegotiationReport;
  LValidation: TWfcMusicPassValidationReport;
  LVariation: TWfcMusicMelodyCells;
begin
  InitializeFixture(LFixture);
  LPipeline := nil;
  LBaseline := nil;
  LFailed := nil;
  LRepaired := nil;
  try
    LPipeline := CreatePipeline(LFixture);
    LFixture.ScoreTemplate.Free;
    LFixture.ScoreTemplate := nil;
    LPipeline.IntersectAllowedTokens(wmplHarmony, 2,
      EncodeWfcMusicHarmonyCell(LFixture.HarmonyA[2]));
    Check(LPipeline.TryGenerate(LBaseline, LOrdinaryReport),
      'seeded initial harmony-rhythm-melody generation succeeds');
    Check(Assigned(LBaseline) and (LOrdinaryReport.Status = wmpsCompleted)
      and LBaseline.HasLatentCapture,
      'initial generation returns a checked latent capture');
    Check(LPipeline.Validate(LBaseline, LValidation) and LValidation.Valid
      and (LValidation.CheckedLayers = 3)
      and (LValidation.CheckedCells = 12),
      'independent pipeline validation checks all three layer captures');
    Check(MelodyCellsMatch(LBaseline.CopyMelodyCells, LFixture.MelodyA),
      'initial harmony constraint selects the expected A branch');
    LGenerated := LBaseline.CopyGenerated(wmplHarmony);
    Check(TokensMatch(LGenerated.Tokens,
      EncodeWfcMusicHarmonyCells(LFixture.HarmonyA)) and
      (Length(LGenerated.StateIndices) = FIXTURE_CELL_COUNT),
      'capture exposes public harmony and detached latent state indices');
    LBaselineSignature := LBaseline.Signature;
    LBaselineFingerprint := CommittedFingerprint(LPipeline);

    LPipeline.ClearAllowedTokens(wmplHarmony, 2);
    SetLength(LVariation, 2);
    LVariation[0] := LFixture.MelodyB[2];
    LVariation[1] := LFixture.MelodyB[3];
    LPipeline.LockMelodyCells(2, LVariation);
    LLockedFingerprint := CommittedFingerprint(LPipeline);
    Check(LLockedFingerprint = LBaselineFingerprint,
      'public token constraints do not mutate committed entry values');
    Check(not LPipeline.TryRegenerateFrom(wmplHarmony,
      DefaultGraphSolveOptions, LFailed, LOrdinaryReport),
      'ordinary one-way regeneration cannot reopen rejected harmony');
    Check((LFailed = nil) and (LOrdinaryReport.Status = wmpsSolveFailed),
      'ordinary failure publishes no partial composition');
    LAfterFailure := CommittedFingerprint(LPipeline);
    Check(LAfterFailure = LLockedFingerprint,
      'ordinary regeneration failure rolls back every committed entry');
    Check((LBaseline.Signature = LBaselineSignature) and
      MelodyCellsMatch(LBaseline.CopyMelodyCells, LFixture.MelodyA),
      'previous composition remains immutable after failed regeneration');

    LNegotiationOptions := DefaultGraphNegotiationOptions;
    LNegotiationOptions.MaxPassBacktracks := 4;
    Check(LPipeline.TryRegenerateNegotiatedFrom(wmplHarmony,
      LNegotiationOptions, LRepaired, LRepairReport),
      'bounded selective negotiation reopens the harmony provider');
    Check(Assigned(LRepaired) and (LRepairReport.Status = wmpsCompleted)
      and LRepaired.HasLatentCapture,
      'negotiated repair commits one complete checked composition');
    Check(IndicesEqual(LRepairReport.Search.RequestedRootIndices, [0]) and
      IndicesEqual(LRepairReport.Search.ActivePassIndices, [0, 2]),
      'selective scope retains harmony as the root and reuses rhythm');
    WriteLn('  [INFO] repair pass backtracks ',
      LRepairReport.Search.Search.PassBacktracks, ', attempts ',
      Length(LRepairReport.Search.Search.Attempts));
    if Length(LRepairReport.Search.Search.Attempts) > 0 then
      WriteLn('  [INFO] first rejected pass ',
        LRepairReport.Search.Search.Attempts[0].BacktrackedPassIndex);
    if Length(LRepairReport.Search.Search.Attempts) > 1 then
      WriteLn('  [INFO] second rejected pass ',
        LRepairReport.Search.Search.Attempts[1].BacktrackedPassIndex);
    Check((LRepairReport.Search.Search.PassBacktracks = 2) and
      (Length(LRepairReport.Search.Search.Attempts) = 2) and
      (LRepairReport.Search.Search.Attempts[0].BacktrackedPassIndex = 0) and
      (LRepairReport.Search.Search.Attempts[1].BacktrackedPassIndex = 0),
      'repair records both deterministic rejected harmony assignments');
    Check(LRepairReport.Search.Search.FinalReport.Passes[1].Disposition =
      gpdReused, 'selective repair reports the rhythm pass as reused');
    Check(MelodyCellsMatch(LRepaired.CopyMelodyCells, LFixture.MelodyB),
      'negotiation preserves the lock and selects the compatible B melody');
    Check(LPipeline.Validate(LRepaired, LValidation) and LValidation.Valid,
      'negotiated composition independently validates');
    LRepairedSignature := LRepaired.Signature;
    Check((LRepairedSignature <> LBaselineSignature) and
      (CalculateWfcMusicCompositionSignature(LRepaired) =
        LRepairedSignature),
      'public signature changes with the variation and recomputes exactly');
    Check((LBaselineSignature = EXPECTED_BASELINE_SIGNATURE) and
      (LRepairedSignature = EXPECTED_REPAIRED_SIGNATURE),
      'baseline and repaired public signatures match their v1 goldens');
    WriteLn('  [INFO] baseline signature ',
      WfcMusicCompositionSignatureHex(LBaselineSignature));
    WriteLn('  [INFO] repaired signature ',
      WfcMusicCompositionSignatureHex(LRepairedSignature));

    LPipeline.IntersectAllowedTokens(wmplMelody, 2,
      EncodeWfcMusicMelodyCell(LFixture.MelodyA[2]));
    LLockedFingerprint := CommittedFingerprint(LPipeline);
    LFailed.Free;
    LFailed := nil;
    Check(not LPipeline.TryRegenerateNegotiatedFrom(wmplHarmony,
      LNegotiationOptions, LFailed, LRepairReport),
      'contradictory public locks make bounded repair fail');
    Check((LFailed = nil) and
      (CommittedFingerprint(LPipeline) = LLockedFingerprint),
      'failed negotiated repair is atomic and returns no composition');
    Check((LRepaired.Signature = LRepairedSignature) and
      MelodyCellsMatch(LRepaired.CopyMelodyCells, LFixture.MelodyB),
      'successful capture remains detached from later pipeline failures');
  finally
    LRepaired.Free;
    LFailed.Free;
    LBaseline.Free;
    LPipeline.Free;
    FinalizeFixture(LFixture);
  end;
end;

procedure TestFullNegotiatedGeneration;
var
  I: Integer;
  LComposition: TWfcMusicComposition;
  LConstraints: TWfcSequenceTokenConstraints;
  LFixture: TFixture;
  LOptions: TGraphNegotiationOptions;
  LPipeline: TWfcMusicPassPipeline;
  LReport: TWfcMusicPassNegotiationReport;
  LTokens: TWfcModelTokens;
  LValidation: TWfcMusicPassValidationReport;
begin
  InitializeFixture(LFixture);
  LPipeline := nil;
  LComposition := nil;
  try
    LPipeline := CreatePipeline(LFixture);
    LFixture.ScoreTemplate.Free;
    LFixture.ScoreTemplate := nil;
    SetLength(LConstraints, 2);
    SetLength(LTokens, 1);
    LTokens[0] := EncodeWfcMusicMelodyCell(LFixture.MelodyB[2]);
    LConstraints[0] := MakeWfcSequenceTokenConstraint(2, LTokens);
    LTokens[0] := EncodeWfcMusicMelodyCell(LFixture.MelodyB[3]);
    LConstraints[1] := MakeWfcSequenceTokenConstraint(3, LTokens);
    LPipeline.IntersectTokenConstraints(wmplMelody, LConstraints);
    LOptions := DefaultGraphNegotiationOptions;
    LOptions.MaxPassBacktracks := 4;
    Check(LPipeline.TryGenerateNegotiated(LOptions, LComposition, LReport),
      'bounded full negotiation generates across all three layers');
    Check(Assigned(LComposition) and (LReport.Status = wmpsCompleted) and
      (LReport.Search.Status = gnsSolved),
      'full negotiation returns a solved report and immutable capture');
    WriteLn('  [INFO] full pass backtracks ', LReport.Search.PassBacktracks,
      ', attempts ', Length(LReport.Search.Attempts));
    for I := 0 to High(LReport.Search.Attempts) do
      WriteLn('  [INFO] full rejected pass ', I, ': ',
        LReport.Search.Attempts[I].BacktrackedPassIndex);
    WriteLn('  [INFO] full transcript ',
      IntToHex(LReport.Search.TranscriptHash, 8));
    Check((LReport.Search.PassBacktracks = 4) and
      (Length(LReport.Search.Attempts) = 4) and
      (LReport.Search.Attempts[0].BacktrackedPassIndex = 1) and
      (LReport.Search.Attempts[1].BacktrackedPassIndex = 0) and
      (LReport.Search.Attempts[2].BacktrackedPassIndex = 1) and
      (LReport.Search.Attempts[3].BacktrackedPassIndex = 0) and
      (LReport.Search.TranscriptHash =
        EXPECTED_FULL_NEGOTIATION_TRANSCRIPT),
      'full negotiation preserves its exact portable bounded transcript');
    Check(MelodyCellsMatch(LComposition.CopyMelodyCells,
      LFixture.MelodyB),
      'full negotiation satisfies ordered public token constraints');
    Check(LPipeline.Validate(LComposition, LValidation) and
      LValidation.Valid,
      'full negotiated generation independently validates');
  finally
    LComposition.Free;
    LPipeline.Free;
    FinalizeFixture(LFixture);
  end;
end;

procedure TestFinalValidationRollback;
var
  LBaseline, LControlBaseline: TWfcMusicComposition;
  LBefore, LControlBefore: String;
  LControlFailed, LFailed, LRetryFailed: TWfcMusicComposition;
  LControlPipeline: TWfcMusicPassPipeline;
  LControlReport, LFirstReport,
    LRetryReport: TWfcMusicPassReport;
  LFixture: TFixture;
  LOptions: TGraphSolveOptions;
  LPipeline: TWfcMusicPassPipeline;
begin
  InitializeInvalidContinuationFixture(LFixture);
  LPipeline := nil;
  LControlPipeline := nil;
  LBaseline := nil;
  LControlBaseline := nil;
  LFailed := nil;
  LRetryFailed := nil;
  LControlFailed := nil;
  try
    LPipeline := CreatePipeline(LFixture);
    LControlPipeline := CreatePipeline(LFixture);
    LFixture.ScoreTemplate.Free;
    LFixture.ScoreTemplate := nil;

    LPipeline.IntersectAllowedTokens(wmplRhythm, 0,
      EncodeWfcMusicRhythmCell(LFixture.Rhythm[0]));
    LPipeline.IntersectAllowedTokens(wmplMelody, 0,
      EncodeWfcMusicMelodyCell(LFixture.MelodyA[0]));
    LControlPipeline.IntersectAllowedTokens(wmplRhythm, 0,
      EncodeWfcMusicRhythmCell(LFixture.Rhythm[0]));
    LControlPipeline.IntersectAllowedTokens(wmplMelody, 0,
      EncodeWfcMusicMelodyCell(LFixture.MelodyA[0]));
    Check(LPipeline.TryGenerate(LBaseline, LFirstReport) and
      LControlPipeline.TryGenerate(LControlBaseline, LControlReport),
      'control pipelines first commit a valid melody chronology');
    LBefore := CommittedFingerprint(LPipeline);
    LControlBefore := CommittedFingerprint(LControlPipeline);
    Check((LBefore = LControlBefore) and
      TokensMatch(LPipeline.CopyCommittedTokens(wmplMelody),
        EncodeWfcMusicMelodyCells(LFixture.MelodyA)),
      'typed committed-layer inspection captures the exact valid baseline');

    LPipeline.ClearAllowedTokens(wmplRhythm, 0);
    LPipeline.ClearAllowedTokens(wmplMelody, 0);
    LControlPipeline.ClearAllowedTokens(wmplRhythm, 0);
    LControlPipeline.ClearAllowedTokens(wmplMelody, 0);
    LPipeline.IntersectLockedSpan(wmplRhythm, 0,
      EncodeWfcMusicRhythmCells(LFixture.RhythmB));
    LPipeline.LockMelodyCells(0, LFixture.MelodyB);
    LControlPipeline.IntersectLockedSpan(wmplRhythm, 0,
      EncodeWfcMusicRhythmCells(LFixture.RhythmB));
    LControlPipeline.LockMelodyCells(0, LFixture.MelodyB);

    LOptions := DefaultGraphSolveOptions;
    LOptions.CaptureTrace := True;
    Check(not LPipeline.TryRegenerateFrom(wmplRhythm, LOptions,
      LFailed, LFirstReport),
      'reachable leading hold is rejected by generic final validation');
    Check((LFailed = nil) and
      (LFirstReport.Status = wmpsValidationFailed) and
      (LFirstReport.Solve.Contradiction.Kind = gckFinalValidation) and
      (LFirstReport.Solve.FailedPassIndex = Ord(wmplMelody)) and
      (LFirstReport.Solve.Contradiction.EntryIndex = 0),
      'final-validation failure returns nil with exact pass and cell');
    Check((LFirstReport.Validation.Issue.Kind =
        wmpvikMelodyContinuation) and
      (LFirstReport.Validation.Issue.Layer = wmplMelody) and
      (LFirstReport.Validation.Issue.Position = 0),
      'structured diagnostics identify melody continuation, not score');
    Check((LFirstReport.Validation.Issue.Detail = '') and
      (Pos('@wfcs', DescribeWfcMusicPassValidationIssue(
        LFirstReport.Validation.Issue)) = 0),
      'public diagnostics contain no private latent key or raw exception');
    Check(CommittedFingerprint(LPipeline) = LBefore,
      'final validation restores every committed latent value exactly');

    Check(not LControlPipeline.TryRegenerateFrom(wmplRhythm, LOptions,
      LControlFailed, LControlReport),
      'independent control reaches the same final-validation rejection');
    Check(not LPipeline.TryRegenerateFrom(wmplRhythm, LOptions,
      LRetryFailed, LRetryReport),
      'the rejected transaction can be retried without stale pending state');
    Check((LControlFailed = nil) and (LRetryFailed = nil) and
      (LFirstReport.Solve.TraceHash = LControlReport.Solve.TraceHash) and
      (LFirstReport.Solve.TraceHash = LRetryReport.Solve.TraceHash) and
      (LFirstReport.Solve.TraceHash <> 0),
      'rollback restores RNG state for an exact cross-instance replay');
    Check((CommittedFingerprint(LPipeline) = LBefore) and
      (CommittedFingerprint(LControlPipeline) = LControlBefore),
      'repeated and control failures preserve both committed snapshots');

  finally
    LControlFailed.Free;
    LRetryFailed.Free;
    LFailed.Free;
    LControlBaseline.Free;
    LBaseline.Free;
    LControlPipeline.Free;
    LPipeline.Free;
    FinalizeFixture(LFixture);
  end;
end;

procedure TestConfigurationContract;
var
  LConfig: TWfcMusicPassConfig;
  LFixture: TFixture;
  LPipeline: TWfcMusicPassPipeline;
  LRejected: Boolean;
begin
  Check((WFC_MUSIC_PASS_PIPELINE_VERSION = 1) and
    (WFC_MUSIC_PASS_VALIDATION_VERSION = 1) and
    (WFC_MUSIC_COMPOSITION_SIGNATURE_VERSION = 1),
    'pipeline, validation, and signature formats are explicitly versioned');
  Check((WfcMusicPassLayerName(wmplHarmony) = 'harmony') and
    (WfcMusicPassLayerName(wmplRhythm) = 'rhythm') and
    (WfcMusicPassLayerName(wmplMelody) = 'melody'),
    'typed layers map to stable public pass labels');
  InitializeFixture(LFixture);
  LPipeline := nil;
  try
    LConfig := DefaultWfcMusicPassConfig(LFixture.ScoreTemplate, 0,
      FIXTURE_SEED);
    LConfig.Models.Harmony := LFixture.HarmonyModel;
    LConfig.Models.Rhythm := LFixture.RhythmModel;
    LConfig.Models.Melody := LFixture.MelodyModel;
    LRejected := False;
    try
      LPipeline := TWfcMusicPassPipeline.Create(LConfig);
    except
      on E: Exception do
        LRejected := True;
    end;
    Check(LRejected, 'pipeline rejects a non-positive fixed quantum');
  finally
    LPipeline.Free;
    FinalizeFixture(LFixture);
  end;
end;

begin
  RunTest('public factory and signature', @TestPublicFactoryAndSignature);
  RunTest('variation and negotiated repair',
    @TestVariationAndNegotiatedRepair);
  RunTest('full negotiated generation', @TestFullNegotiatedGeneration);
  RunTest('final-validation rollback', @TestFinalValidationRollback);
  RunTest('configuration contract', @TestConfigurationContract);
  WriteLn;
  WriteLn(GCheckCount, ' checks, ', GFailureCount, ' failures');
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d checks failed', [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
