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
program wfc_music_voices_preparation_replay;
{$mode delphi}{$H+}
uses SysUtils, wfc, wfc_sequence_graph, wfc_music_voices_graph,
  wfc_voice_preparation_fixture;
const
  Seeds: array[0..2] of Integer = (731, 1731, 2731);
var
  C: TWfcMusicVoicesGraphConfig;
  B: TWfcMusicVoicesBoundaries;
  G: TGraph;
  V: TWfcMusicVoicesGenerated;
  R: TGraphSolveReport;
  P: TWfcMusicVoicesValidationReport;
  Seed: Integer;
begin
  C := PreparationFixture;
  try
    B := PreparationBoundaries;
    for Seed in Seeds do
    begin
      G := BuildWfcMusicVoicesSegmentGraph(C, 65, Seed, B);
      try
        if not G.TrySolve(DefaultGraphSolveOptions, R) then
          raise Exception.Create('Replay solve failed');
        if not CaptureSolvedWfcMusicVoices(C, G, B, V, P) then
          raise Exception.Create('Replay capture failed: ' + P.Issue.Detail);
        WriteLn(Seed, ':', PreparationEvidence(V));
      finally
        G.Free;
      end;
    end;
  finally
    FreePreparationFixture(C);
  end;
end.
