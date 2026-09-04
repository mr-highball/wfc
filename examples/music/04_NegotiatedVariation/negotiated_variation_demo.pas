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
unit negotiated_variation_demo;

{$mode delphi}{$H+}

interface

procedure RunNegotiatedVariationDemo;

implementation

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_sequence,
  wfc_sequence_learn,
  wfc_sequence_graph,
  wfc_trace,
  wfc_music,
  wfc_music_sequence,
  wfc_music_passes,
  wfc_music_passes_text,
  wfc_music_text,
  wfc_midi_smf,
  wfc_music_midi;

const
  CELL_COUNT = 4;
  QUANTUM_TICKS = 480;
  STEPS_PER_OCTAVE = 12;
  DEFAULT_SEED = TGraphSeed(0);
  MOTIF_CELL_COUNT = 2;

  EXPECTED_BASELINE_COMPOSITION =
    TWfcMusicCompositionSignature($4194643A);
  EXPECTED_REPAIRED_COMPOSITION =
    TWfcMusicCompositionSignature($77B72044);
  EXPECTED_SCORE_SIGNATURE = Cardinal($6F92E033);
  EXPECTED_MIDI_SIGNATURE = Cardinal($4BC35E03);
  EXPECTED_NESTED_TRANSCRIPT = TGraphTraceSignature($A91E0706);
  EXPECTED_SELECTIVE_TRANSCRIPT = TGraphTraceSignature($38EE8F80);
  EXPECTED_ARTIFACT_BYTES = 685;

type
  ENegotiatedVariationDemo = class(Exception);

procedure Require(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise ENegotiatedVariationDemo.Create(AMessage);
end;

function ParseSeed: TGraphSeed;
var
  I: Integer;
  LDigit: TGraphSeed;
  LParsed: TGraphSeed;
  LText: String;
begin
  if ParamCount = 0 then
    Exit(DEFAULT_SEED);
  if ParamCount <> 1 then
    raise EConvertError.Create(
      'usage: NegotiatedVariation [unsigned-32-bit-seed]');
  LText := ParamStr(1);
  if LText = '' then
    raise EConvertError.Create('seed cannot be empty');
  LParsed := 0;
  for I := 1 to Length(LText) do
  begin
    if not (LText[I] in ['0'..'9']) then
      raise EConvertError.Create('seed must be an unsigned 32-bit integer');
    LDigit := TGraphSeed(Ord(LText[I]) - Ord('0'));
    if LParsed > (High(TGraphSeed) - LDigit) div 10 then
      raise EConvertError.Create('seed exceeds 4294967295');
    LParsed := LParsed * 10 + LDigit;
  end;
  Result := LParsed;
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

function MelodyE: TWfcMusicMelodyCells;
begin
  Result := MelodyCellsOf([
    MakeWfcMusicAttackCell(60, 90),
    MakeWfcMusicHoldCell(60, 90),
    MakeWfcMusicAttackCell(64, 96),
    MakeWfcMusicHoldCell(64, 96)]);
end;

function MelodyG: TWfcMusicMelodyCells;
begin
  Result := MelodyCellsOf([
    MakeWfcMusicAttackCell(60, 90),
    MakeWfcMusicHoldCell(60, 90),
    MakeWfcMusicAttackCell(67, 100),
    MakeWfcMusicHoldCell(67, 100)]);
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

function BuildScoreTemplate(const ACells: TWfcMusicMelodyCells):
  TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetLength(LTracks, 1);
  LTracks[0] := MakeWfcMusicTrack('lead', 'Negotiated Variation');
  SetLength(LVoices, 1);
  LVoices[0] := MakeWfcMusicVoice(0, 'melody');
  SetLength(LMeters, 1);
  LMeters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, 500000);
  LSpans := RebuildWfcMusicVoiceSpans(ACells, 0, QUANTUM_TICKS);
  Result := TWfcMusicScore.Create(480, STEPS_PER_OCTAVE,
    CELL_COUNT * QUANTUM_TICKS, LTracks, LVoices,
    LMeters, LTempos, LSpans);
end;

function CapturedSolveOptions: TGraphSolveOptions;
begin
  Result := DefaultGraphSolveOptions;
  Result.MaxBacktracks := 64;
  Result.CaptureTrace := True;
end;

function NegotiationOptions: TGraphNegotiationOptions;
begin
  Result := DefaultGraphNegotiationOptions;
  Result.SolveOptions := CapturedSolveOptions;
  Result.MaxPassBacktracks := 1;
end;

function MelodyCellsEqual(const A, B: TWfcMusicMelodyCells): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to Length(A) - 1 do
    if (A[I].Action <> B[I].Action) or
        (A[I].Pitch <> B[I].Pitch) or
        (A[I].Velocity <> B[I].Velocity) then
      Exit(False);
  Result := True;
end;

function RhythmCellsEqual(const A, B: TWfcMusicRhythmCells): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to Length(A) - 1 do
    if A[I].Action <> B[I].Action then
      Exit(False);
  Result := True;
end;

function TokensEqual(const A, B: TWfcModelTokens): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to Length(A) - 1 do
    if A[I] <> B[I] then
      Exit(False);
  Result := True;
end;

function IndicesEqual(const A: TGraphPassIndices;
  const B: array of Integer): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to Length(A) - 1 do
    if A[I] <> B[I] then
      Exit(False);
  Result := True;
end;

function ExecutionOrderEqual(const AReport: TGraphSolveReport;
  const AExpected: array of Integer): Boolean;
begin
  Result := IndicesEqual(AReport.ExecutionOrder, AExpected);
end;

function BytesEqual(const A, B: TWfcMidiBytes): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to Length(A) - 1 do
    if A[I] <> B[I] then
      Exit(False);
  Result := True;
end;

function FnvByte(var AHash: Cardinal; const AByte: Byte): Cardinal;
{$PUSH}
{$Q-}
var
  LValue: Cardinal;
begin
  AHash := AHash xor Cardinal(AByte);
  LValue := AHash;
  AHash := (LValue + (LValue shl 1) + (LValue shl 4) +
    (LValue shl 7) + (LValue shl 8) + (LValue shl 24)) and
    Cardinal($FFFFFFFF);
  Result := AHash;
end;
{$POP}

function ChecksumBytes(const ABytes: TWfcMidiBytes): Cardinal;
var
  I: Integer;
begin
  Result := Cardinal(2166136261);
  for I := 0 to Length(ABytes) - 1 do
    FnvByte(Result, ABytes[I]);
end;

function ChecksumAscii(const AText: String): Cardinal;
var
  I: Integer;
begin
  Result := Cardinal(2166136261);
  for I := 1 to Length(AText) do
  begin
    Require(Ord(AText[I]) <= 127,
      'signature input is not canonical ASCII');
    FnvByte(Result, Byte(Ord(AText[I])));
  end;
end;

function CardinalHex(const AValue: Cardinal): String;
begin
  Result := UpperCase(IntToHex(AValue, 8));
end;

procedure ValidatePublicComposition(const AComposition: TWfcMusicComposition);
var
  I: Integer;
  LHarmony: TWfcMusicHarmonyCells;
  LMelody: TWfcMusicMelodyCells;
  LRhythm: TWfcMusicRhythmCells;
begin
  Require(Assigned(AComposition), 'public composition is nil');
  LHarmony := AComposition.CopyHarmonyCells;
  LRhythm := AComposition.CopyRhythmCells;
  LMelody := AComposition.CopyMelodyCells;
  Require((Length(LHarmony) = CELL_COUNT) and
      (Length(LRhythm) = CELL_COUNT) and
      (Length(LMelody) = CELL_COUNT),
    'public composition changed the fixture width');
  for I := 0 to CELL_COUNT - 1 do
  begin
    Require(LMelody[I].Action = LRhythm[I].Action,
      'public rhythm relation failed at cell ' + IntToStr(I));
    if LMelody[I].Action <> wmcaRest then
      Require((LHarmony[I].Kind = wmhckPitchClass) and
          (LHarmony[I].StepsPerOctave = STEPS_PER_OCTAVE) and
          (LHarmony[I].PitchClass =
            (LMelody[I].Pitch mod STEPS_PER_OCTAVE)),
        'public harmony relation failed at cell ' + IntToStr(I));
  end;
end;

procedure RequireCommittedTokens(const APipeline: TWfcMusicPassPipeline;
  const ABaseline: TWfcMusicComposition);
var
  LActual: TWfcGeneratedSequence;
  LExpected: TWfcGeneratedSequence;
  LLayer: TWfcMusicPassLayer;
  LReport: TWfcSequenceGraphValidationReport;
begin
  for LLayer := Low(TWfcMusicPassLayer) to High(TWfcMusicPassLayer) do
  begin
    Require(APipeline.TryCopyCommittedLayer(LLayer, LActual, LReport),
      'cannot recapture committed ' + WfcMusicPassLayerName(LLayer));
    LExpected := ABaseline.CopyGenerated(LLayer);
    Require(TokensEqual(LActual.Tokens, LExpected.Tokens),
      'failed ordinary regeneration changed committed ' +
        WfcMusicPassLayerName(LLayer));
  end;
end;

procedure RequireTraceHash(const AReport: TGraphSolveReport;
  const AContext: String);
begin
  Require(AReport.TraceCaptured, AContext + ' did not capture a trace');
  Require(AReport.TraceHash = CalculateGraphTraceHash(AReport),
    AContext + ' trace hash did not recompute');
end;

procedure RequireSeedZeroGoldens(const ASeed: TGraphSeed;
  const ABaseline, ARepaired: TWfcMusicComposition;
  const AScoreSignature, AMidiSignature: Cardinal;
  const AReport: TWfcMusicPassSelectiveNegotiationReport;
  const AArtifactLength: Integer);
begin
  if ASeed <> DEFAULT_SEED then
    Exit;
  Require((ABaseline.Signature = EXPECTED_BASELINE_COMPOSITION) and
      (ARepaired.Signature = EXPECTED_REPAIRED_COMPOSITION),
    'seed-zero public composition signatures changed');
  Require(AScoreSignature = EXPECTED_SCORE_SIGNATURE,
    'seed-zero score signature changed');
  Require(AMidiSignature = EXPECTED_MIDI_SIGNATURE,
    'seed-zero MIDI signature changed');
  Require(AReport.Search.Search.TranscriptHash =
      EXPECTED_NESTED_TRANSCRIPT,
    'seed-zero nested negotiation transcript changed: ' +
      GraphTraceSignatureHex(AReport.Search.Search.TranscriptHash));
  Require(AReport.Search.TranscriptHash = EXPECTED_SELECTIVE_TRANSCRIPT,
    'seed-zero selective negotiation transcript changed: ' +
      GraphTraceSignatureHex(AReport.Search.TranscriptHash));
  Require(AArtifactLength = EXPECTED_ARTIFACT_BYTES,
    'seed-zero wfcmusicpass byte count changed');
end;

procedure RunFixture(const ASeed: TGraphSeed);
var
  LArtifact: String;
  LArtifactReplay: TWfcMusicComposition;
  LBaseline: TWfcMusicComposition;
  LBaselineCells: TWfcMusicMelodyCells;
  LBaselineRhythm: TWfcMusicRhythmCells;
  LConfig: TWfcMusicPassConfig;
  LControlBaseline: TWfcMusicComposition;
  LControlPipeline: TWfcMusicPassPipeline;
  LControlReport: TWfcMusicPassReport;
  LEnding: TWfcMusicMelodyCells;
  LHarmonyE: TWfcMusicHarmonyCells;
  LHarmonyG: TWfcMusicHarmonyCells;
  LHarmonyModel: TWfcSequenceModel;
  LMelodyE: TWfcMusicMelodyCells;
  LMelodyG: TWfcMusicMelodyCells;
  LMelodyModel: TWfcSequenceModel;
  LMidi: TWfcMidiBytes;
  LMidiCanonical: TWfcMidiBytes;
  LMidiFile: TWfcMidiFile;
  LMidiSignature: Cardinal;
  LMotif: TWfcMusicMelodyCells;
  LNegotiated: TWfcMusicComposition;
  LNegotiatedReport: TWfcMusicPassSelectiveNegotiationReport;
  LNegotiatedSolved: Boolean;
  LNegotiationOptions: TGraphNegotiationOptions;
  LOrdinary: TWfcMusicComposition;
  LOrdinaryReport: TWfcMusicPassReport;
  LPipeline: TWfcMusicPassPipeline;
  LReplayScore: TWfcMusicScore;
  LRepairedCells: TWfcMusicMelodyCells;
  LRepairedRhythm: TWfcMusicRhythmCells;
  LRhythmCells: TWfcMusicRhythmCells;
  LRhythmModel: TWfcSequenceModel;
  LScore: TWfcMusicScore;
  LScoreSignature: Cardinal;
  LScoreTemplate: TWfcMusicScore;
  LScoreText: String;
  LSolveOptions: TGraphSolveOptions;
  LTarget: TWfcMusicMelodyCells;
  LValidation: TWfcMusicPassValidationReport;
begin
  LBaseline := nil;
  LControlBaseline := nil;
  LControlPipeline := nil;
  LHarmonyModel := nil;
  LMelodyModel := nil;
  LNegotiated := nil;
  LOrdinary := nil;
  LPipeline := nil;
  LRhythmModel := nil;
  LScoreTemplate := nil;
  LArtifactReplay := nil;
  LScore := nil;
  LReplayScore := nil;
  try
    LMelodyE := MelodyE;
    LMelodyG := MelodyG;
    LRhythmCells := ProjectWfcMusicMelodyToRhythm(LMelodyE);
    LHarmonyE := ProjectWfcMusicMelodyToHarmony(LMelodyE,
      STEPS_PER_OCTAVE);
    LHarmonyG := ProjectWfcMusicMelodyToHarmony(LMelodyG,
      STEPS_PER_OCTAVE);

    LHarmonyModel := LearnSequenceModelCorpus(SamplesOf([
      MakeWfcSequenceSample(EncodeWfcMusicHarmonyCells(LHarmonyE)),
      MakeWfcSequenceSample(EncodeWfcMusicHarmonyCells(LHarmonyG))]), 4);
    LRhythmModel := LearnSequenceModel(
      EncodeWfcMusicRhythmCells(LRhythmCells), 4);
    LMelodyModel := LearnSequenceModelCorpus(SamplesOf([
      MakeWfcSequenceSample(EncodeWfcMusicMelodyCells(LMelodyE)),
      MakeWfcSequenceSample(EncodeWfcMusicMelodyCells(LMelodyG))]), 4);
    LScoreTemplate := BuildScoreTemplate(LMelodyE);

    LConfig := DefaultWfcMusicPassConfig(LScoreTemplate,
      QUANTUM_TICKS, ASeed);
    LConfig.Models.Harmony := LHarmonyModel;
    LConfig.Models.Rhythm := LRhythmModel;
    LConfig.Models.Melody := LMelodyModel;
    LPipeline := TWfcMusicPassPipeline.Create(LConfig);
    LControlPipeline := TWfcMusicPassPipeline.Create(LConfig);
    Require((Ord(wmplHarmony) = 0) and
        (Ord(wmplRhythm) = 1) and
        (Ord(wmplMelody) = 2) and
        (WfcMusicPassLayerName(wmplHarmony) =
          WFC_MUSIC_PASS_HARMONY) and
        (WfcMusicPassLayerName(wmplRhythm) =
          WFC_MUSIC_PASS_RHYTHM) and
        (WfcMusicPassLayerName(wmplMelody) =
          WFC_MUSIC_PASS_MELODY),
      'music pass labels or stable indices changed');

    LSolveOptions := CapturedSolveOptions;
    Require(LPipeline.TryGenerate(LSolveOptions, LBaseline,
        LOrdinaryReport),
      'seeded baseline did not solve');
    Require(LControlPipeline.TryGenerate(LSolveOptions, LControlBaseline,
        LControlReport) and
        (LControlBaseline.Signature = LBaseline.Signature),
      'independent control did not replay the seeded baseline');
    Require(LOrdinaryReport.Status = wmpsCompleted,
      'baseline owner report did not complete');
    RequireTraceHash(LOrdinaryReport.Solve, 'baseline generation');
    Require(LPipeline.Validate(LBaseline, LValidation),
      'baseline validation failed: ' +
        DescribeWfcMusicPassValidationIssue(LValidation.Issue));
    ValidatePublicComposition(LBaseline);

    LBaselineCells := LBaseline.CopyMelodyCells;
    LBaselineRhythm := LBaseline.CopyRhythmCells;
    Require(MelodyCellsEqual(LBaselineCells, LMelodyE) or
        MelodyCellsEqual(LBaselineCells, LMelodyG),
      'seeded baseline is not one complete learned branch');
    if MelodyCellsEqual(LBaselineCells, LMelodyE) then
      LTarget := LMelodyG
    else
      LTarget := LMelodyE;

    SetLength(LMotif, MOTIF_CELL_COUNT);
    LMotif[0] := LBaselineCells[0];
    LMotif[1] := LBaselineCells[1];
    SetLength(LEnding, CELL_COUNT - MOTIF_CELL_COUNT);
    LEnding[0] := LTarget[2];
    LEnding[1] := LTarget[3];
    LPipeline.LockMelodyCells(0, LMotif);
    LPipeline.LockMelodyCells(MOTIF_CELL_COUNT, LEnding);

    LOrdinary.Free;
    LOrdinary := nil;
    Require(not LPipeline.TryRegenerateFrom(wmplHarmony,
        LSolveOptions, LOrdinary, LOrdinaryReport),
      'ordinary selective regeneration unexpectedly solved');
    Require(not Assigned(LOrdinary),
      'failed ordinary regeneration returned a composition');
    Require((LOrdinaryReport.Status = wmpsSolveFailed) and
        (LOrdinaryReport.FailedLayer = wmplMelody) and
        (LOrdinaryReport.Solve.Status = gssContradiction) and
        (LOrdinaryReport.Solve.FailedPassIndex = Ord(wmplMelody)) and
        (LOrdinaryReport.Solve.Contradiction.Kind = gckPassDependency) and
        (LOrdinaryReport.Solve.Contradiction.DependencyPassIndex =
          Ord(wmplHarmony)),
      'ordinary failure is not the melody-from-harmony contradiction');
    Require(ExecutionOrderEqual(LOrdinaryReport.Solve, [0, 2]),
      'ordinary repair did not execute harmony and melody only');
    Require(LOrdinaryReport.Solve.Passes[Ord(wmplRhythm)].Disposition =
        gpdReused,
      'ordinary repair did not report rhythm as reused');
    RequireTraceHash(LOrdinaryReport.Solve, 'ordinary failed repair');
    RequireCommittedTokens(LPipeline, LBaseline);

    LNegotiationOptions := NegotiationOptions;
    LNegotiatedSolved := LPipeline.TryRegenerateNegotiatedFrom(wmplHarmony,
      LNegotiationOptions, LNegotiated, LNegotiatedReport);
    Require(LNegotiatedSolved,
      Format('bounded selective negotiation did not repair the variation ' +
        '(owner=%d search=%d pass-backtracks=%d attempts=%d failed-pass=%d kind=%d provider=%d)',
        [Ord(LNegotiatedReport.Status),
         Ord(LNegotiatedReport.Search.Search.Status),
         LNegotiatedReport.Search.Search.PassBacktracks,
         Length(LNegotiatedReport.Search.Search.Attempts),
         LNegotiatedReport.Search.Search.FinalReport.FailedPassIndex,
         Ord(LNegotiatedReport.Search.Search.FinalReport.Contradiction.Kind),
         LNegotiatedReport.Search.Search.FinalReport.Contradiction.
           DependencyPassIndex]));
    Require((LNegotiatedReport.Status = wmpsCompleted) and
        (LNegotiatedReport.Search.Search.Status = gnsSolved),
      'negotiated repair did not finish with solved owner metadata');
    Require((LNegotiatedReport.Search.Search.PassBacktracks = 1) and
        (Length(LNegotiatedReport.Search.Search.Attempts) = 1),
      'negotiated repair did not retain exactly one rejected round');
    Require(IndicesEqual(LNegotiatedReport.Search.RequestedRootIndices,
        [Ord(wmplHarmony)]) and
        IndicesEqual(LNegotiatedReport.Search.ActivePassIndices,
          [Ord(wmplHarmony), Ord(wmplMelody)]),
      'selective repair roots or active closure changed');
    Require((LNegotiatedReport.Search.Search.Attempts[0].
          BacktrackedPassIndex = Ord(wmplHarmony)) and
        (LNegotiatedReport.Search.Search.Attempts[0].
          BacktrackedExecutionOrdinal = 0) and
        (Length(LNegotiatedReport.Search.Search.Attempts[0].
          ExcludedAssignment) = CELL_COUNT),
      'negotiation did not exclude the complete harmony assignment');
    Require((LNegotiatedReport.Search.Search.Attempts[0].SolveReport.
          FailedPassIndex = Ord(wmplMelody)) and
        (LNegotiatedReport.Search.Search.Attempts[0].SolveReport.
          Contradiction.Kind = gckPassDependency) and
        (LNegotiatedReport.Search.Search.Attempts[0].SolveReport.
          Contradiction.DependencyPassIndex = Ord(wmplHarmony)),
      'rejected round did not retain the melody contradiction');
    Require(ExecutionOrderEqual(
        LNegotiatedReport.Search.Search.FinalReport, [0, 2]),
      'negotiated terminal round changed the active execution order');
    Require(LNegotiatedReport.Search.Search.FinalReport.Passes[
        Ord(wmplRhythm)].Disposition = gpdReused,
      'negotiated terminal round did not reuse rhythm');
    RequireTraceHash(
      LNegotiatedReport.Search.Search.Attempts[0].SolveReport,
      'rejected negotiated round');
    Require(LNegotiatedReport.Search.Search.Attempts[0].SolveReport.
        TraceHash = LOrdinaryReport.Solve.TraceHash,
      'rejected negotiated round is not the ordinary failed repair');
    RequireTraceHash(LNegotiatedReport.Search.Search.FinalReport,
      'terminal negotiated round');
    Require(LNegotiatedReport.Search.Search.TranscriptHash =
        CalculateGraphNegotiationTranscriptHash(LNegotiationOptions,
          LNegotiatedReport.Search.Search),
      'nested negotiation transcript hash did not recompute');
    Require(LNegotiatedReport.Search.TranscriptHash =
        CalculateGraphSelectiveNegotiationTranscriptHash(
          LNegotiationOptions, LNegotiatedReport.Search),
      'selective negotiation transcript hash did not recompute');

    Require(LPipeline.Validate(LNegotiated, LValidation),
      'repaired composition validation failed: ' +
        DescribeWfcMusicPassValidationIssue(LValidation.Issue));
    ValidatePublicComposition(LNegotiated);
    LRepairedCells := LNegotiated.CopyMelodyCells;
    LRepairedRhythm := LNegotiated.CopyRhythmCells;
    Require(MelodyCellsEqual(LRepairedCells, LTarget),
      'negotiated repair did not commit the requested ending');
    Require((LRepairedCells[0].Action = LMotif[0].Action) and
        (LRepairedCells[0].Pitch = LMotif[0].Pitch) and
        (LRepairedCells[0].Velocity = LMotif[0].Velocity) and
        (LRepairedCells[1].Action = LMotif[1].Action) and
        (LRepairedCells[1].Pitch = LMotif[1].Pitch) and
        (LRepairedCells[1].Velocity = LMotif[1].Velocity),
      'negotiated repair changed the locked public motif');
    Require(RhythmCellsEqual(LRepairedRhythm, LBaselineRhythm),
      'selective repair changed the reused rhythm provider');

    LScore := LNegotiated.CopyScore;
    LScoreText := EncodeWfcMusicText(LScore);
    LReplayScore := DecodeWfcMusicText(LScoreText);
    Require(EncodeWfcMusicText(LReplayScore) = LScoreText,
      'repaired score text did not round-trip canonically');
    LScoreSignature := ChecksumAscii(LScoreText);

    LArtifact := EncodeWfcMusicPassesText(LNegotiated);
    LArtifactReplay := DecodeWfcMusicPassesText(LArtifact);
    Require((EncodeWfcMusicPassesText(LArtifactReplay) = LArtifact) and
        (LArtifactReplay.Signature = LNegotiated.Signature),
      'wfcmusicpass artifact did not round-trip canonically');

    LMidi := EncodeWfcMusicMidi(LScore);
    LMidiFile := DecodeWfcMidiFile(LMidi);
    LMidiCanonical := EncodeWfcMidiFile(LMidiFile);
    Require(BytesEqual(LMidi, LMidiCanonical),
      'repaired MIDI bytes did not round-trip canonically');
    LMidiSignature := ChecksumBytes(LMidi);
    RequireSeedZeroGoldens(ASeed, LBaseline, LNegotiated,
      LScoreSignature, LMidiSignature, LNegotiatedReport,
      Length(LArtifact));

    WriteLn('NegotiatedVariation: harmony + rhythm -> melody variation');
    WriteLn('Seed: ', ASeed);
    WriteLn('Passes: harmony=0 rhythm=1 melody=2');
    WriteLn('Baseline composition: ',
      WfcMusicCompositionSignatureHex(LBaseline.Signature),
      ' ending=', LBaselineCells[2].Pitch);
    WriteLn('Motif lock: cells=[0,1] pitches=', LMotif[0].Pitch,
      ',', LMotif[1].Pitch);
    WriteLn('Requested ending: ', LTarget[2].Pitch, ',', LTarget[3].Pitch);
    WriteLn('Ordinary repair: rollback failed-pass=',
      LOrdinaryReport.Solve.FailedPassIndex, ' provider=',
      LOrdinaryReport.Solve.Contradiction.DependencyPassIndex,
      ' rhythm=reused');
    WriteLn('Selective scope: requested=[0] active=[0,2]');
    WriteLn('Negotiated rounds: ',
      Length(LNegotiatedReport.Search.Search.Attempts) + 1,
      ' pass-backtracks=',
      LNegotiatedReport.Search.Search.PassBacktracks);
    WriteLn('Repaired composition: ',
      WfcMusicCompositionSignatureHex(LNegotiated.Signature));
    WriteLn('Score FNV-1a: ', CardinalHex(LScoreSignature));
    WriteLn('MIDI FNV-1a: ', CardinalHex(LMidiSignature));
    WriteLn('Nested negotiation hash: ', GraphTraceSignatureHex(
      LNegotiatedReport.Search.Search.TranscriptHash));
    WriteLn('Selective transcript hash: ', GraphTraceSignatureHex(
      LNegotiatedReport.Search.TranscriptHash));
    WriteLn('Canonical wfcmusicpass bytes: ', Length(LArtifact));
    WriteLn('Reused rhythm: public-cells-and-report=preserved');
    WriteLn('Independent validation: motif, providers, score, artifact, MIDI');
    WriteLn('Self-check: passed');
  finally
    LReplayScore.Free;
    LScore.Free;
    LArtifactReplay.Free;
    LOrdinary.Free;
    LNegotiated.Free;
    LBaseline.Free;
    LControlBaseline.Free;
    LPipeline.Free;
    LControlPipeline.Free;
    LScoreTemplate.Free;
    LMelodyModel.Free;
    LRhythmModel.Free;
    LHarmonyModel.Free;
  end;
end;

procedure RunNegotiatedVariationDemo;
begin
  Require((WFC_MUSIC_PASS_PIPELINE_VERSION = 1) and
      (WFC_MUSIC_PASS_VALIDATION_VERSION = 1) and
      (WFC_MUSIC_COMPOSITION_SIGNATURE_VERSION = 1) and
      (WFC_MUSIC_PASSES_TEXT_VERSION = 1) and
      (WFC_PASS_NEGOTIATION_ALGORITHM_VERSION = 1) and
      (WFC_PASS_NEGOTIATION_HASH_VERSION = 1) and
      (WFC_SELECTIVE_NEGOTIATION_ALGORITHM_VERSION = 1) and
      (WFC_SELECTIVE_NEGOTIATION_HASH_VERSION = 1),
    'fixture requires the version-one music and negotiation contracts');
  RunFixture(ParseSeed);
end;

end.
