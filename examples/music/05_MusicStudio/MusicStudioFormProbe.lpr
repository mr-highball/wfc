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
program MusicStudioFormProbe;

{$mode delphi}{$H+}

uses
  SysUtils, wfc, wfc_sequence, wfc_sequence_learn, wfc_music,
  wfc_music_sequence, wfc_music_passes, music_studio_workbench;

{ Reproducible modeling experiment, not an optimizer benchmark. The only
  independent variable is rhythm context order. No retries or seed search. }

function ScoreTemplate: TWfcMusicScore;
var T: TWfcMusicTracks; V: TWfcMusicVoices;
  M: TWfcMusicMeterChanges; P: TWfcMusicTempoChanges;
begin
  SetLength(T, 1); T[0] := MakeWfcMusicTrack('lead', 'Music Studio');
  SetLength(V, 1); V[0] := MakeWfcMusicVoice(0, 'melody');
  SetLength(M, 1); M[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(P, 1); P[0] := MakeWfcMusicTempoChange(0, MUSIC_STUDIO_TEMPO);
  Result := TWfcMusicScore.Create(MUSIC_STUDIO_TPQ, 12,
    MUSIC_STUDIO_CELL_COUNT * MUSIC_STUDIO_QUANTUM, T, V, M, P,
    RebuildWfcMusicVoiceSpans(MusicStudioCorpus(0), 0, MUSIC_STUDIO_QUANTUM));
end;

procedure Measure(const ARhythmOrder: Integer);
var
  H, R, M: TWfcSequenceSamples;
  Models: TWfcMusicPassModels;
  C: TWfcMusicMelodyCells;
  Template: TWfcMusicScore;
  Config: TWfcMusicPassConfig;
  Pipeline: TWfcMusicPassPipeline;
  Composition: TWfcMusicComposition;
  Report: TWfcMusicPassNegotiationReport;
  Options: TGraphNegotiationOptions;
  I, Seed, Decisions, Propagations, Contradictions, Backtracks: Integer;
  Solved, Expected: Boolean;
  Status, Signature: String;

  procedure AddCounters(const AReport: TGraphSolveReport);
  var J: Integer;
  begin
    for J := 0 to High(AReport.Passes) do
    begin
      Inc(Decisions, AReport.Passes[J].Decisions);
      Inc(Propagations, AReport.Passes[J].Propagations);
      Inc(Contradictions, AReport.Passes[J].Contradictions);
      Inc(Backtracks, AReport.Passes[J].Backtracks);
    end;
  end;

begin
  Models := Default(TWfcMusicPassModels);
  Template := nil;
  try
    SetLength(H, MUSIC_STUDIO_CORPUS_COUNT);
    SetLength(R, MUSIC_STUDIO_CORPUS_COUNT);
    SetLength(M, MUSIC_STUDIO_CORPUS_COUNT);
    for I := 0 to MUSIC_STUDIO_CORPUS_COUNT - 1 do
    begin
      C := MusicStudioCorpus(I);
      H[I] := MakeWfcSequenceSample(EncodeWfcMusicHarmonyCells(
        ProjectWfcMusicMelodyToHarmony(C, 12)));
      R[I] := MakeWfcSequenceSample(EncodeWfcMusicRhythmCells(
        ProjectWfcMusicMelodyToRhythm(C)));
      M[I] := MakeWfcSequenceSample(EncodeWfcMusicMelodyCells(C));
    end;
    Models.Harmony := LearnSequenceModelCorpus(H, 3);
    Models.Rhythm := LearnSequenceModelCorpus(R, ARhythmOrder);
    Models.Melody := LearnSequenceModelCorpus(M, 3);
    Template := ScoreTemplate;
    Options := DefaultGraphNegotiationOptions;
    Options.SolveOptions.MaxBacktracks := 256;
    Options.SolveOptions.CaptureTrace := False;
    Options.MaxPassBacktracks := 16;
    for Seed := 0 to 15 do
    begin
      Config := DefaultWfcMusicPassConfig(Template, MUSIC_STUDIO_QUANTUM, Seed);
      Config.Models := Models;
      Pipeline := TWfcMusicPassPipeline.Create(Config);
      Composition := nil;
      try
        Solved := Pipeline.TryGenerateNegotiated(Options, Composition, Report);
        if ARhythmOrder = 3 then Expected := Seed = 10
        else Expected := Seed in [0,4,6,7,13,14,15];
        if Solved <> Expected then
          raise Exception.Create('form experiment solved-seed matrix changed');
        Signature := '';
        if Solved then
        begin
          if not MusicStudioCompositionIsValid(Composition) then
            raise Exception.Create('form experiment independent validation failed');
          Status := 'solved';
          Signature := UpperCase(IntToHex(Composition.Signature, 8));
        end
        else
        begin
          if Assigned(Composition) or
              (Report.Search.Status <> gnsPassBacktrackLimit) then
            raise Exception.Create('form experiment expected a clean pass limit');
          Status := 'pass-limit';
        end;
        Decisions := 0; Propagations := 0;
        Contradictions := 0; Backtracks := 0;
        for I := 0 to High(Report.Search.Attempts) do
          AddCounters(Report.Search.Attempts[I].SolveReport);
        AddCounters(Report.Search.FinalReport);
        WriteLn(ARhythmOrder, ',', Seed, ',', Status, ',',
          Length(Report.Search.Attempts) + 1, ',', Report.Search.PassBacktracks,
          ',', Decisions, ',', Propagations, ',', Contradictions, ',', Backtracks,
          ',', Signature, ',', UpperCase(IntToHex(Report.Search.TranscriptHash, 8)));
      finally Composition.Free; Pipeline.Free; end;
    end;
  finally
    Template.Free; Models.Melody.Free; Models.Rhythm.Free; Models.Harmony.Free;
  end;
end;

begin
  WriteLn('rhythm_order,seed,status,rounds,pass_backtracks,decisions,propagations,contradictions,local_backtracks,composition,transcript');
  Measure(3);
  Measure(MUSIC_STUDIO_CELL_COUNT);
end.
