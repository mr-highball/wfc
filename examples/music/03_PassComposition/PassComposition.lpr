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
program PassComposition;

{$mode delphi}{$H+}

uses
  SysUtils,
  {$IFDEF PAS2JS}
  NodeJSApp,
  NodeJS,
  {$ENDIF}
  wfc,
  wfc_model,
  wfc_sequence,
  wfc_sequence_learn,
  wfc_sequence_graph,
  wfc_music,
  wfc_music_sequence,
  wfc_music_graph,
  wfc_music_text,
  wfc_midi_smf,
  wfc_music_midi;

const
  CELL_COUNT = 8;
  QUANTUM_TICKS = 480;
  DEFAULT_SEED = TGraphSeed(0);

type
  EPassComposition = class(Exception);

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
      'usage: PassComposition [unsigned-32-bit-seed]');
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
    LParsed := (LParsed * 10) + LDigit;
  end;
  Result := LParsed;
end;

function MelodyA: TWfcMusicMelodyCells;
begin
  Result := nil;
  SetLength(Result, CELL_COUNT);
  Result[0] := MakeWfcMusicAttackCell(60, 96);
  Result[1] := MakeWfcMusicHoldCell(60, 96);
  Result[2] := MakeWfcMusicAttackCell(64, 96);
  Result[3] := MakeWfcMusicHoldCell(64, 96);
  Result[4] := MakeWfcMusicAttackCell(67, 96);
  Result[5] := MakeWfcMusicHoldCell(67, 96);
  Result[6] := MakeWfcMusicRestCell;
  Result[7] := MakeWfcMusicRestCell;
end;

function MelodyB: TWfcMusicMelodyCells;
begin
  Result := nil;
  SetLength(Result, CELL_COUNT);
  Result[0] := MakeWfcMusicAttackCell(65, 88);
  Result[1] := MakeWfcMusicHoldCell(65, 88);
  Result[2] := MakeWfcMusicAttackCell(69, 92);
  Result[3] := MakeWfcMusicHoldCell(69, 92);
  Result[4] := MakeWfcMusicAttackCell(72, 100);
  Result[5] := MakeWfcMusicHoldCell(72, 100);
  Result[6] := MakeWfcMusicRestCell;
  Result[7] := MakeWfcMusicRestCell;
end;

function MelodyBHigh: TWfcMusicMelodyCells;
begin
  Result := nil;
  SetLength(Result, CELL_COUNT);
  Result[0] := MakeWfcMusicAttackCell(77, 84);
  Result[1] := MakeWfcMusicHoldCell(77, 84);
  Result[2] := MakeWfcMusicAttackCell(81, 90);
  Result[3] := MakeWfcMusicHoldCell(81, 90);
  Result[4] := MakeWfcMusicAttackCell(84, 104);
  Result[5] := MakeWfcMusicHoldCell(84, 104);
  Result[6] := MakeWfcMusicRestCell;
  Result[7] := MakeWfcMusicRestCell;
end;

function SequenceCorpus(const AFirst,
  ASecond: TWfcModelTokens): TWfcSequenceSamples;
begin
  Result := nil;
  SetLength(Result, 2);
  Result[0] := MakeWfcSequenceSample(AFirst);
  Result[1] := MakeWfcSequenceSample(ASecond);
end;

function LearnCorpus(const AFirst,
  ASecond: TWfcModelTokens): TWfcSequenceModel;
var
  LCorpus: TWfcSequenceSamples;
begin
  LCorpus := SequenceCorpus(AFirst, ASecond);
  Result := LearnSequenceModelCorpus(LCorpus, 2);
end;

procedure RequireCapture(const AModel: TWfcSequenceModel;
  const AGraph: TGraph; out AGenerated: TWfcGeneratedSequence);
var
  LValidation: TWfcSequenceGraphValidationReport;
begin
  if not CaptureSolvedSequence(AModel, AGraph, AGenerated,
      LValidation) then
    raise EPassComposition.Create('sequence capture failed: ' +
      DescribeSequenceGraphIssue(LValidation.Issue));
end;

function SolveComposition(const ASeed: TGraphSeed;
  const AMelodyModel, ARhythmModel,
  AHarmonyModel: TWfcSequenceModel; const ARhythmPlan,
  AHarmonyPlan: TWfcModelTokens;
  out AMelody, ARhythm,
  AHarmony: TWfcGeneratedSequence): TGraphSolveReport;
var
  I: Integer;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
begin
  Result := Default(TGraphSolveReport);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(CELL_COUNT, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.Seed := ASeed;

    LGraph.CurrentPass := 'harmony';
    ApplySequenceModelToGraph(AHarmonyModel, LGraph);
    for I := 0 to CELL_COUNT - 1 do
      IntersectSequenceAllowedTokens(AHarmonyModel, LGraph, I,
        AHarmonyPlan[I]);

    LGraph.SwitchToPass('rhythm');
    ApplySequenceModelToGraph(ARhythmModel, LGraph);
    for I := 0 to CELL_COUNT - 1 do
      IntersectSequenceAllowedTokens(ARhythmModel, LGraph, I,
        ARhythmPlan[I]);

    LGraph.SwitchToPass('melody');
    ApplySequenceModelToGraph(AMelodyModel, LGraph);
    RequireWfcMusicMelodyFromPasses(AMelodyModel, ARhythmModel,
      AHarmonyModel, LGraph, 'rhythm', 'harmony', 12);

    LOptions := DefaultGraphSolveOptions;
    if not LGraph.TrySolve(LOptions, Result) then
      raise EPassComposition.CreateFmt(
        'music pass pipeline did not solve (status=%d pass=%d kind=%d entry=%d dependency=%d)',
        [Ord(Result.Status), Result.FailedPassIndex,
         Ord(Result.Contradiction.Kind), Result.Contradiction.EntryIndex,
         Result.Contradiction.DependencyPassIndex]);
    RequireCapture(AHarmonyModel, LGraph.PassGraph[0], AHarmony);
    RequireCapture(ARhythmModel, LGraph.PassGraph[1], ARhythm);
    RequireCapture(AMelodyModel, LGraph.PassGraph[2], AMelody);
  finally
    LGraph.Free;
  end;
end;

procedure ValidatePublicComposition(const AMelodyTokens,
  ARhythmTokens, AHarmonyTokens: TWfcModelTokens);
var
  I: Integer;
  LHarmony: TWfcMusicHarmonyCell;
  LMelody: TWfcMusicMelodyCell;
  LRhythm: TWfcMusicRhythmCell;
begin
  if (Length(AMelodyTokens) <> CELL_COUNT) or
      (Length(ARhythmTokens) <> CELL_COUNT) or
      (Length(AHarmonyTokens) <> CELL_COUNT) then
    raise EPassComposition.Create('generated pass width changed');
  for I := 0 to CELL_COUNT - 1 do
  begin
    LMelody := DecodeWfcMusicMelodyCell(AMelodyTokens[I]);
    LRhythm := DecodeWfcMusicRhythmCell(ARhythmTokens[I]);
    LHarmony := DecodeWfcMusicHarmonyCell(AHarmonyTokens[I]);
    if LMelody.Action <> LRhythm.Action then
      raise EPassComposition.CreateFmt(
        'rhythm projection failed at cell %d', [I]);
    if (LMelody.Action <> wmcaRest) and
        ((LHarmony.Kind <> wmhckPitchClass) or
         (LHarmony.StepsPerOctave <> 12) or
         (LHarmony.PitchClass <> (LMelody.Pitch mod 12))) then
      raise EPassComposition.CreateFmt(
        'harmony projection failed at cell %d', [I]);
  end;
end;

function BuildScore(const ACells: TWfcMusicMelodyCells): TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetLength(LTracks, 1);
  LTracks[0] := MakeWfcMusicTrack('lead', 'Pass Composition');
  SetLength(LVoices, 1);
  LVoices[0] := MakeWfcMusicVoice(0, 'melody');
  SetLength(LMeters, 1);
  LMeters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, 500000);
  LSpans := RebuildWfcMusicVoiceSpans(ACells, 0, QUANTUM_TICKS);
  Result := TWfcMusicScore.Create(480, 12,
    CELL_COUNT * QUANTUM_TICKS, LTracks, LVoices,
    LMeters, LTempos, LSpans);
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

function ChecksumBytes(const ABytes: TWfcMidiBytes): LongWord;
{$PUSH}
{$Q-}
var
  I: Integer;
  LValue: LongWord;
begin
  Result := LongWord(2166136261);
  for I := 0 to Length(ABytes) - 1 do
  begin
    Result := Result xor LongWord(ABytes[I]);
    LValue := Result;
    Result := (LValue + (LValue shl 1) + (LValue shl 4) +
      (LValue shl 7) + (LValue shl 8) + (LValue shl 24)) and
      LongWord($FFFFFFFF);
  end;
end;
{$POP}

procedure WriteScore(const AScore: TWfcMusicScore);
var
  I: Integer;
  J: Integer;
  LSpan: TWfcMusicSpanEvent;
begin
  for I := 0 to AScore.SpanCount - 1 do
  begin
    LSpan := AScore.SpanAt(I);
    Write('span ', I, ': start=', LSpan.StartTick,
      ' duration=', LSpan.DurationTicks);
    if LSpan.Kind = wmskRest then
      Write(' rest')
    else
      for J := 0 to Length(LSpan.Tones) - 1 do
        Write(' pitch=', LSpan.Tones[J].Pitch,
          '@', LSpan.Tones[J].Velocity);
    WriteLn;
  end;
end;

procedure Run;
var
  LHarmony: TWfcGeneratedSequence;
  LHarmonyA: TWfcMusicHarmonyCells;
  LHarmonyB: TWfcMusicHarmonyCells;
  LHarmonyPlan: TWfcModelTokens;
  LHarmonyModel: TWfcSequenceModel;
  LMelody: TWfcGeneratedSequence;
  LMelodyA: TWfcMusicMelodyCells;
  LMelodyB: TWfcMusicMelodyCells;
  LMelodyHigh: TWfcMusicMelodyCells;
  LMelodyCells: TWfcMusicMelodyCells;
  LMelodyModel: TWfcSequenceModel;
  LMidi: TWfcMidiBytes;
  LMidiCanonical: TWfcMidiBytes;
  LMidiFile: TWfcMidiFile;
  LReplay: TWfcMusicScore;
  LReport: TGraphSolveReport;
  LRhythm: TWfcGeneratedSequence;
  LRhythmA: TWfcMusicRhythmCells;
  LRhythmB: TWfcMusicRhythmCells;
  LRhythmPlan: TWfcModelTokens;
  LRhythmModel: TWfcSequenceModel;
  LScore: TWfcMusicScore;
  LSeed: TGraphSeed;
  LText: String;
begin
  LSeed := ParseSeed;
  LMelodyA := MelodyA;
  LMelodyB := MelodyB;
  LMelodyHigh := MelodyBHigh;
  LRhythmA := ProjectWfcMusicMelodyToRhythm(LMelodyB);
  LRhythmB := ProjectWfcMusicMelodyToRhythm(LMelodyHigh);
  LHarmonyA := ProjectWfcMusicMelodyToHarmony(LMelodyA, 12);
  LHarmonyB := ProjectWfcMusicMelodyToHarmony(LMelodyB, 12);
  LRhythmPlan := EncodeWfcMusicRhythmCells(LRhythmA);
  LHarmonyPlan := EncodeWfcMusicHarmonyCells(LHarmonyB);

  LMelodyModel := nil;
  LRhythmModel := nil;
  LHarmonyModel := nil;
  try
    LMelodyModel := LearnCorpus(EncodeWfcMusicMelodyCells(LMelodyB),
      EncodeWfcMusicMelodyCells(LMelodyHigh));
    LRhythmModel := LearnCorpus(EncodeWfcMusicRhythmCells(LRhythmA),
      EncodeWfcMusicRhythmCells(LRhythmB));
    LHarmonyModel := LearnCorpus(EncodeWfcMusicHarmonyCells(LHarmonyA),
      EncodeWfcMusicHarmonyCells(LHarmonyB));
    LReport := SolveComposition(LSeed, LMelodyModel,
      LRhythmModel, LHarmonyModel, LRhythmPlan, LHarmonyPlan,
      LMelody, LRhythm, LHarmony);
    ValidatePublicComposition(LMelody.Tokens, LRhythm.Tokens,
      LHarmony.Tokens);
    LMelodyCells := DecodeWfcMusicMelodyCells(LMelody.Tokens);
    LScore := BuildScore(LMelodyCells);
    try
      LText := EncodeWfcMusicText(LScore);
      LReplay := DecodeWfcMusicText(LText);
      try
        if EncodeWfcMusicText(LReplay) <> LText then
          raise EPassComposition.Create('music text replay changed bytes');
      finally
        LReplay.Free;
      end;

      LMidi := EncodeWfcMusicMidi(LScore);
      LMidiFile := DecodeWfcMidiFile(LMidi);
      LMidiCanonical := EncodeWfcMidiFile(LMidiFile);
      if not BytesEqual(LMidi, LMidiCanonical) then
        raise EPassComposition.Create('MIDI replay changed bytes');

      WriteLn('PassComposition: harmony + rhythm -> melody -> score -> MIDI');
      WriteLn('Seed: ', LSeed);
      WriteLn('Cells: ', CELL_COUNT, ' at ', QUANTUM_TICKS, ' ticks');
      WriteLn('Pipeline passes: ', Length(LReport.Passes));
      WriteLn('Upstream plan: F-A-C pitch classes with attack/hold/rest rhythm');
      WriteLn('Public projection validation: verified');
      WriteLn('Canonical wfcmusic bytes: ', Length(LText));
      WriteLn('Canonical MIDI bytes: ', Length(LMidi));
      WriteLn('MIDI FNV-1a: ', ChecksumBytes(LMidi));
      WriteScore(LScore);
      WriteLn('Versions: music=', WFC_MUSIC_MODEL_VERSION,
        ' cells=', WFC_MUSIC_CELL_TOKEN_VERSION,
        ' music-graph=', WFC_MUSIC_GRAPH_ADAPTER_VERSION,
        ' music-text=', WFC_MUSIC_TEXT_VERSION,
        ' midi=', WFC_MIDI_SMF_VERSION,
        ' music-midi=', WFC_MUSIC_MIDI_VERSION);
    finally
      LScore.Free;
    end;
  finally
    LHarmonyModel.Free;
    LRhythmModel.Free;
    LMelodyModel.Free;
  end;
end;

begin
  try
    Run;
  except
    on E: Exception do
    begin
      WriteLn('PassComposition error: ', E.Message);
      {$IFDEF PAS2JS}
      TNJSProcess.exitCode := 1;
      {$ELSE}
      Halt(1);
      {$ENDIF}
    end;
  end;
end.
