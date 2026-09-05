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
program wfc_training_workspace_test;

{$mode delphi}{$H+}

uses
  SysUtils, wfc, wfc_model, wfc_training_workspace,
  wfc_pipeline_model, wfc_pipeline_text, wfc_pipeline_run,
  wfc_pipeline_run_text, wfc_pipeline_result, wfc_pipeline_result_text,
  training_studio_presets, training_studio_demo;

const
  SOURCE_SIGNATURES: array[0..4] of String =
    ('6DF80942', '6F432E54', '0FA2C5EA', '62C511AF', 'FBE11615');
  RECIPE_SIGNATURES: array[0..4] of String =
    ('7ED0F825', '678E6527', 'DBCBA621', '4D1533DF', '6854AA6C');
  RESULT_SIGNATURES: array[0..4] of String =
    ('15860FEE', '7E6399E8', '947C4AFD', '920A363A', 'F65D4875');

var
  GChecks: Integer = 0;
  GFailures: Integer = 0;

procedure Check(const AValue: Boolean; const AMessage: String);
begin
  Inc(GChecks);
  if not AValue then
  begin
    Inc(GFailures);
    WriteLn('[FAIL] ', AMessage);
  end;
end;

procedure TestPresets;
var
  W: TWfcTrainingWorkspace;
  O: TWfcTrainingSolveOptions;
  T: TWfcModelTokens;
  P: TWfcPipelineModel;
  R: TWfcPipelineRun;
  V: TWfcPipelineResult;
  S: String;
  I: Integer;
begin
  W := TWfcTrainingWorkspace.Create(InteractiveWfcTrainingWorkspaceLimits);
  try
    Check(not W.HasRecipe and not W.HasRun and not W.HasResult,
      'new workspace has no derived artifacts');
    for I := 0 to TRAINING_STUDIO_PRESET_COUNT - 1 do
    begin
      W.SetSourceText(TrainingStudioPresetText(I));
      Check(not W.HasRecipe and not W.HasRun and not W.HasResult,
        'setting source invalidates every derived artifact');
      W.Train;
      Check(W.HasRecipe and not W.HasRun and not W.HasResult,
        'training publishes only its current recipe');
      O := TrainingStudioPresetOptions(I);
      W.ConfigureRun(O, nil, nil);
      Check(W.HasRun and not W.HasResult, 'configure publishes an unexecuted run');
      W.Solve;
      T := W.OutputTokens;
      Check(W.TrainingSignatureText = SOURCE_SIGNATURES[I],
        'preset source identity is pinned');
      Check(W.RecipeSignatureText = RECIPE_SIGNATURES[I],
        'preset recipe identity is pinned');
      Check(W.ResultSignatureText = RESULT_SIGNATURES[I],
        'preset result identity is pinned');
      Check((W.ResultStatus = wprsSolved) and
        TrainingStudioOutputIsValid(I, O.Width, O.Height, T),
        'preset obeys independently checked public constraints');
      S := W.ResultText;
      W.Solve;
      Check(W.ResultText = S, 'repeating solve preserves exact replay');
      T[0] := 'mutated';
      T := W.OutputTokens;
      Check(T[0] <> 'mutated', 'output token access is detached');
      T := W.PublicVocabulary;
      T[0] := 'mutated';
      T := W.PublicVocabulary;
      Check(T[0] <> 'mutated', 'vocabulary access is detached');
      P := DecodeWfcPipelineModelText(W.RecipeText);
      try
        R := DecodeWfcPipelineRunText(W.RunText, P);
        try
          V := DecodeWfcPipelineResultText(W.ResultText, P, R);
          try
            Check(EncodeWfcPipelineResultText(V) = S,
              'exported recipe/run/result forms a canonical provenance chain');
          finally V.Free end;
        finally R.Free end;
      finally P.Free end;
    end;
  finally W.Free end;
end;

procedure TestFailureLifecycle;
var
  W: TWfcTrainingWorkspace;
  O: TWfcTrainingSolveOptions;
  L: TWfcPipelineCellLocks;
  D: TWfcPipelineCellDomains;
  S: String;
  RaisedError: Boolean;
begin
  W := TWfcTrainingWorkspace.Create(InteractiveWfcTrainingWorkspaceLimits);
  try
    W.SetSourceText(TrainingStudioPresetText(2));
    W.Train;
    O := TrainingStudioPresetOptions(2);
    SetLength(L, 1);
    L[0] := MakeWfcPipelineCellLock(W.PublicPassIndex, 0, 0, 0, 'A');
    W.ConfigureRun(O, L, nil);
    S := W.RunText;
    L[0].Token := 'B';
    Check(W.RunText = S, 'configured run owns its lock inputs');
    L[0].Token := 'A';
    W.Solve;
    Check(W.ResultStatus = wprsSolved, 'public lock solves private patterns');
    S := W.ResultText;
    SetLength(L, 2);
    L[1] := MakeWfcPipelineCellLock(W.PublicPassIndex, 1, 0, 0, 'A');
    W.ConfigureRun(O, L, nil);
    Check(not W.HasResult, 'changing constraints discards the old result');
    W.Solve;
    Check(W.HasResult and (W.ResultStatus = wprsContradiction) and
      (Length(W.OutputTokens) = 0), 'contradiction is exportable but has no old cells');
    Check(Pos('status=contradiction', W.ResultText) > 0,
      'failed result export retains its real terminal status');
    SetLength(L, 1);
    W.ConfigureRun(O, L, nil);
    W.Solve;
    Check(W.ResultText = S, 'removing the conflicting lock restores exact replay');

    SetLength(D, 1);
    D[0] := MakeWfcPipelineCellDomain(W.PublicPassIndex, 0, 0, 0, nil);
    W.ConfigureRun(O, nil, D);
    W.Solve;
    Check((W.ResultStatus = wprsContradiction) and
      (Length(W.OutputTokens) = 0), 'explicit empty public domain is not ignored');

    L[0].Token := 'not-in-vocabulary';
    RaisedError := False;
    try W.ConfigureRun(O, L, nil) except on E: Exception do RaisedError := True end;
    Check(RaisedError and W.HasRecipe and not W.HasRun and not W.HasResult,
      'invalid run edit never exposes the prior request or result');
    RaisedError := False;
    try S := W.ResultText except on E: EWfcTrainingWorkspace do RaisedError := True end;
    Check(RaisedError, 'stale result cannot be exported');

    W.SetSourceText('invalid');
    RaisedError := False;
    try W.Train except on E: Exception do RaisedError := True end;
    Check(RaisedError and (W.SourceText = 'invalid') and
      not W.HasRecipe and not W.HasRun and not W.HasResult,
      'invalid source retains only the draft');
    RaisedError := False;
    try S := W.RecipeText except on E: EWfcTrainingWorkspace do RaisedError := True end;
    Check(RaisedError, 'stale recipe cannot be exported');
    W.SetSourceText(TrainingStudioPresetText(2));
    W.Train;
    W.ConfigureRun(O, nil, nil);
    W.Solve;
    W.ClearRun;
    Check(W.HasRecipe and not W.HasRun and not W.HasResult,
      'unparsed run-field edits preserve training but invalidate invocation');
  finally W.Free end;
end;

procedure TestPolicy;
var
  W: TWfcTrainingWorkspace;
  L: TWfcTrainingWorkspaceLimits;
  O: TWfcTrainingSolveOptions;
  RaisedError: Boolean;
begin
  L := InteractiveWfcTrainingWorkspaceLimits;
  Check((L.MaxSourceTokens = 512) and (L.MaxModelItems = 128) and
    (L.MaxOutputCells = 512) and (L.MaxBacktracks = 4096),
    'interactive envelope is explicit');
  L.MaxModelItems := 1;
  W := TWfcTrainingWorkspace.Create(L);
  try
    W.SetSourceText(TrainingStudioPresetText(2));
    RaisedError := False;
    try W.Train except on E: EWfcTrainingWorkspace do RaisedError := True end;
    Check(RaisedError and not W.HasRecipe, 'model policy rejects before graph creation');
  finally W.Free end;
  L := InteractiveWfcTrainingWorkspaceLimits;
  L.MaxSourceTokens := 1;
  W := TWfcTrainingWorkspace.Create(L);
  try
    W.SetSourceText(TrainingStudioPresetText(2));
    RaisedError := False;
    try W.Train except on E: EWfcTrainingWorkspace do RaisedError := True end;
    Check(RaisedError and not W.HasRecipe, 'source token policy rejects before learning');
  finally W.Free end;
  W := TWfcTrainingWorkspace.Create(InteractiveWfcTrainingWorkspaceLimits);
  try
    W.SetSourceText(TrainingStudioPresetText(2));
    W.Train;
    O := TrainingStudioPresetOptions(2);
    O.Width := 513;
    RaisedError := False;
    try W.ConfigureRun(O, nil, nil) except on E: EWfcTrainingWorkspace do RaisedError := True end;
    Check(RaisedError and not W.HasRun, 'shape exceeds interactive cell envelope');
    O.Width := 32;
    O.Height := 32;
    RaisedError := False;
    try W.ConfigureRun(O, nil, nil) except on E: EWfcTrainingWorkspace do RaisedError := True end;
    Check(RaisedError and not W.HasRun, 'checked shape product exceeds envelope');
    O := TrainingStudioPresetOptions(2);
    O.MaxBacktracks := 4097;
    RaisedError := False;
    try W.ConfigureRun(O, nil, nil) except on E: EWfcTrainingWorkspace do RaisedError := True end;
    Check(RaisedError and not W.HasRun, 'search budget exceeds interactive envelope');
    RaisedError := False;
    try W.SetSourceText(StringOfChar('x', 262145))
    except on E: EWfcTrainingWorkspace do RaisedError := True end;
    Check(RaisedError and not W.HasRecipe and (W.SourceText = ''),
      'oversized source clears old artifacts without retaining oversized draft');
  finally W.Free end;
end;

begin
  TestPresets;
  TestFailureLifecycle;
  TestPolicy;
  WriteLn('Checks: ', GChecks, ', Failures: ', GFailures);
  if GFailures <> 0 then
  begin
    {$IFDEF PAS2JS}raise Exception.Create('workspace conformance failed');
    {$ELSE}Halt(1);{$ENDIF}
  end;
end.
