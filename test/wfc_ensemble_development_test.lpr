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
program wfc_ensemble_development_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_music, wfc_music_ensemble,
  wfc_music_ensemble_passes, wfc_music_audio, wfc_music_arrangement,
  ensemble_studio_profiles, ensemble_studio_workbench, ensemble_studio_stream;

var Checks: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;

function SameTokens(const A, B: TWfcModelTokens): Boolean;
var I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do if A[I] <> B[I] then Exit(False);
  Result := True;
end;

function SameRange(const T: TWfcModelTokens; A, B, Count: Integer): Boolean;
var I: Integer;
begin
  for I := 0 to Count - 1 do if T[A+I] <> T[B+I] then Exit(False);
  Result := True;
end;

function StreamTokens(const ASeed: TGraphSeed; const ACells, AHorizon: Integer):
  TWfcModelTokens;
var
  O: TEnsembleStudioStreamOptions;
  P: TEnsembleStudioFramePlan;
  S: TEnsembleStudioFrameStream;
  F: TWfcMusicEnsembleFrame;
  Step: TWfcMusicArrangementStep;
  I: Integer;
begin
  O := DefaultEnsembleStudioStreamOptions;
  O.Profile := espDevelopedPeriodV1;
  O.Seed := ASeed;
  O.SegmentCellCount := AHorizon;
  P := PlanEnsembleStudioFrames(EnsembleStudioStreamSecondsText(ACells * 240));
  S := TEnsembleStudioFrameStream.Create(P, O);
  Result := nil;
  SetLength(Result, ACells);
  I := 0;
  try
    repeat
      Step := S.NextFrame(F);
      if Step = wmaspCompleted then Break;
      Check(Step = wmaspProduced, 'developed stream: ' + S.Failure);
      Check(I < ACells, 'stream cannot pad a closing phrase');
      Result[I] := EncodeWfcMusicEnsembleFrame(F);
      Inc(I);
    until False;
    Check(I = ACells, 'all requested developed cells produced');
    Check(S.ProducedTicks = P.ActualTicks, 'stream retains exact requested extent');
  finally S.Free; end;
end;

procedure TestFiniteAndRepair;
var
  S, Replay: TEnsembleStudio;
  T, V: TWfcModelTokens;
  O: TEnsembleStudioOptions;
  Wave: TWfcMusicAudioBytes;
  Score: TWfcMusicScore;
  I, J, UniqueBars, Frames: Integer;
  Seen, Raised: Boolean;
  Failure, PlanText, Signature: String;
begin
  O := DefaultEnsembleStudioOptions;
  S := TEnsembleStudio.Create(4, 16);
  try
    Check(S.Run(esaGenerate, O), 'legacy seed4 sixteen bars still solve');
    T := S.CellTokens(wmelEnsemble);
    for I := 1 to 15 do
      Check(SameRange(T, 0, I*8, 8), 'legacy repeating-bar reproduction remains explicit');
    Check(S.TryWavePreview(Wave, Frames, Failure), 'legacy full-score WAV: ' + Failure);
    Check(Frames = 32*44100, 'reported repetition was not WAV clipping');
    Wave := nil;

    S.Reset(4, 16, espDevelopedPeriodV1);
    Check(not S.HasBaseline and not S.HasCurrent, 'profile switch clears stale artifacts');
    Check(S.Run(esaGenerate, O), 'developed seed4 sixteen bars solve: ' + S.RunReportText);
    Check(S.CurrentIsValid, 'developed composition independently valid');
    T := S.CellTokens(wmelEnsemble);
    Check(Length(T) = 128, 'developed sixteen bars contain128 actual cells');
    Score := S.CopyScore;
    try Check(Score.LengthTicks = 30720, 'developed score is full32seconds');
    finally Score.Free; end;
    UniqueBars := 0;
    for I := 0 to 15 do
    begin
      Seen := False;
      for J := 0 to I - 1 do if SameRange(T, J*8, I*8, 8) then Seen := True;
      if not Seen then Inc(UniqueBars);
    end;
    Check(UniqueBars >= 6, 'developed material contains at least six distinct bars');
    for I := 1 to 3 do
      Check(not SameRange(T, (I-1)*32, I*32, 32), 'adjacent phrases are not a four-bar loop');
    PlanText := S.FormReportText;
    Check((Pos('question', PlanText) > 0) and (Pos('answer', PlanText) > 0) and
      (Pos('contrast', PlanText) > 0) and (Pos('return', PlanText) > 0),
      'actual form report exposes all four phrase roles');
    Signature := S.SignatureText;
    Replay := TEnsembleStudio.Create(4, 16, espDevelopedPeriodV1);
    try
      Check(Replay.Run(esaGenerate, O), 'same-profile replay solves');
      Check((Replay.SignatureText = Signature) and (Replay.FormReportText = PlanText),
        'same seed, extent, and profile reproduce frames and form');
    finally Replay.Free; end;

    S.SetLock(wmelEnsemble, 0, T[0]);
    Check(S.Run(esaEnsemble, O), 'compatible lock permits selective repair');
    S.ClearLock(wmelEnsemble, 0);
    Check(S.Run(esaGenerate, O), 'clear compatible lock retains plan baseline');
    Check(SameTokens(T, S.CellTokens(wmelEnsemble)), 'clearing lock cannot erase the plan');
    V := S.PublicTokens(wmelEnsemble);
    I := 0;
    while (I < Length(V)) and (V[I] = T[0]) do Inc(I);
    Check(I < Length(V), 'catalog contains a conflicting public token');
    S.SetLock(wmelEnsemble, 0, V[I]);
    Check(not S.Run(esaGenerate, O), 'incompatible lock fails instead of rewriting phrase');
    Check(not S.HasCurrent, 'failed lock hides current artifacts');
    Check(S.FormReportText = PlanText, 'failed repair preserves immutable form');
    S.ClearLocks;
    Check(S.Run(esaGenerate, O), 'cleared contradiction recovers');
    Check(SameTokens(T, S.CellTokens(wmelEnsemble)), 'recovery restores planned cells');
    Raised := False;
    try S.Reset(4, 0, espStructuralV1);
    except on E: Exception do Raised := True; end;
    Check(Raised and S.HasCurrent and (S.Profile = espDevelopedPeriodV1),
      'rejected profile reset preserves current session');
    Check(SameTokens(T, StreamTokens(4, 128, 5)), 'finite and five-cell stream have same developed score');
    Check(SameTokens(T, StreamTokens(4, 128, 32)), 'bar-aligned segment size cannot change form or score');
    Check(SameTokens(T, StreamTokens(4, 128, 41)), 'segments spanning phrases preserve developed score');
    Check(S.TryWavePreview(Wave, Frames, Failure), 'developed full WAV: ' + Failure);
    Check(Frames = 32*44100, 'developed WAV renders all requested bars');
    Wave := nil;
  finally S.Free; end;
end;

procedure TestExtentAndWideStream;
const CELLS: array[0..8] of Integer = (1,3,8,9,31,32,33,127,129);
var
  I: Integer;
  A, B: TWfcModelTokens;
  O: TEnsembleStudioStreamOptions;
  P: TEnsembleStudioFramePlan;
  S: TEnsembleStudioFrameStream;
  F: TWfcMusicEnsembleFrame;
begin
  for I := 0 to High(CELLS) do
  begin
    A := StreamTokens(55, CELLS[I], 5);
    B := StreamTokens(55, CELLS[I], 7);
    Check(SameTokens(A, B), 'partial closing extent is independent of acoustic segmentation');
  end;
  O := DefaultEnsembleStudioStreamOptions;
  O.Profile := espDevelopedPeriodV1;
  P := PlanEnsembleStudioFrames('536870912');
  Check(P.CellCount > High(Integer), 'wide test exceeds a signed32cell timeline');
  S := TEnsembleStudioFrameStream.Create(P, O);
  try
    for I := 1 to 3 do
      Check(S.NextFrame(F) = wmaspProduced, 'wide duration begins without whole-score allocation');
    S.Cancel;
    Check(S.NextFrame(F) = wmaspCancelled, 'wide stream cancels without exhausting duration');
  finally S.Free; end;
end;

begin
  try
    TestFiniteAndRepair;
    TestExtentAndWideStream;
    WriteLn('Ensemble development checks: ', Checks);
  except
    on E: Exception do begin WriteLn('FAIL: ', E.Message); Halt(1); end;
  end;
end.
