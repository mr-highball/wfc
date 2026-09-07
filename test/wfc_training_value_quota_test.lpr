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
program wfc_training_value_quota_test;
{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host, Web,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_text_codec, wfc_training, wfc_training_text,
  wfc_training_workspace, wfc_pipeline_model, wfc_pipeline_text,
  wfc_pipeline_run, wfc_pipeline_run_text, wfc_pipeline_result,
  wfc_pipeline_result_text, wfc_pipeline_runtime, wfc_learn_app,
  training_studio_presets, training_studio_demo;

var Checks: Integer;

procedure Check(const Condition: Boolean; const TextValue: String);
begin
  Inc(Checks);
  if not Condition then
  begin
    {$IFDEF PAS2JS}
    document.body.setAttribute('data-self-test-message', TextValue);
    {$ENDIF}
    raise Exception.Create(TextValue);
  end;
end;

function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(Values));
  for I := 0 to High(Values) do Result[I] := Values[I];
end;

function Quotas(const Token: TWfcModelToken; const Minimum, Maximum: Integer):
  TWfcTrainingValueQuotas;
begin
  Result := nil; SetLength(Result, 1);
  Result[0] := MakeWfcTrainingValueQuota('authored-selection', Tokens([Token]),
    Minimum, Maximum);
end;

function Count(const Values: TWfcModelTokens; const Token: TWfcModelToken): Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to High(Values) do if Values[I] = Token then Inc(Result);
end;

function SourceWithQuotas(const Preset: Integer; const Q: TWfcTrainingValueQuotas;
  const ReverseSamples: Boolean = False): String;
var Original, Edited: TWfcTrainingDocument; Samples: TWfcTrainingSamples;
  Temporary: TWfcTrainingSample;
begin
  Original := DecodeWfcTrainingText(TrainingStudioPresetText(Preset));
  try
    Samples := Original.CopySamples;
    if ReverseSamples then
    begin
      Check(Length(Samples) >= 2, 'reorder fixture contains independent samples');
      Temporary := Samples[0]; Samples[0] := Samples[1]; Samples[1] := Temporary;
    end;
    Edited := TWfcTrainingDocument.Create(Original.CopyMetadata,
      Original.CopyOptions, Samples, Q);
    try Result := EncodeWfcTrainingText(Edited); finally Edited.Free; end;
  finally Original.Free; end;
end;

procedure Configure(const W: TWfcTrainingWorkspace;
  const O: TWfcTrainingSolveOptions; const Depth: Integer;
  const Preset: Integer = -1);
var Locks: TWfcPipelineCellLocks;
begin
  Locks := nil;
  if Preset >= 0 then Locks := TrainingStudioPresetLocks(Preset, W.PublicPassIndex);
  if W.Rank = 3 then W.ConfigureVolumeRun(O, Depth, Locks, nil)
  else W.ConfigureRun(O, Locks, nil);
end;

procedure CheckNoDerived(const W: TWfcTrainingWorkspace; const Draft: String);
var Raised: Boolean; Ignored: String;
begin
  Check((not W.HasRecipe) and (not W.HasRun) and (not W.HasResult) and
    (W.SourceText = Draft), 'failed quota mutation retains only the old editable draft');
  Raised := False;
  try Ignored := W.RecipeText; except on E: EWfcTrainingWorkspace do Raised := True; end;
  Check(Raised, 'invalid quota draft cannot export a stale recipe');
  Raised := False;
  try Ignored := W.RunText; except on E: EWfcTrainingWorkspace do Raised := True; end;
  Check(Raised, 'invalid quota draft cannot export a stale run');
  Raised := False;
  try Ignored := W.ResultText; except on E: EWfcTrainingWorkspace do Raised := True; end;
  Check(Raised, 'invalid quota draft cannot export a stale result');
end;

procedure CheckFullReplay(const W: TWfcTrainingWorkspace;
  const O: TWfcTrainingSolveOptions; const Depth, Preset: Integer);
var Fresh: TWfcTrainingWorkspace; Model: TWfcPipelineModel;
  Run: TWfcPipelineRun; Output, Decoded: TWfcPipelineResult;
  RecipeText, RunText, ResultText: String;
begin
  RecipeText := W.RecipeText; RunText := W.RunText; ResultText := W.ResultText;
  Fresh := TWfcTrainingWorkspace.Create(InteractiveWfcTrainingWorkspaceLimits);
  try
    Fresh.SetSourceText(W.SourceText); Fresh.Train;
    Check((Fresh.RecipeText = RecipeText) and (Fresh.ValueQuotaCount = W.ValueQuotaCount),
      'source import into an independent workspace retains authored quotas and recipe identity');
    Configure(Fresh, O, Depth, Preset); Fresh.Solve;
    Check((Fresh.RunText = RunText) and (Fresh.ResultText = ResultText),
      'persisted source replays exact invocation and terminal output');
  finally Fresh.Free; end;
  Model := DecodeWfcPipelineModelText(RecipeText);
  try
    Run := DecodeWfcPipelineRunText(RunText, Model);
    try
      Output := ExecuteWfcPipeline(Model, Run);
      try Check(EncodeWfcPipelineResultText(Output) = ResultText,
        'exported authored recipe and run replay independently of workspace state');
      finally Output.Free; end;
      Decoded := DecodeWfcPipelineResultText(ResultText, Model, Run);
      try Check(EncodeWfcPipelineResultText(Decoded) = ResultText,
        'exported result remains canonical under independent quota recount');
      finally Decoded.Free; end;
    finally Run.Free; end;
  finally Model.Free; end;
end;

procedure CheckCli(const Source, ExpectedRecipe: String);
var Command: TWfcLearnCommand; Output, ErrorText: String; Code: Integer;
begin
  Command := Default(TWfcLearnCommand); Command.Kind := wlckLearn;
  Command.OutputMode := wlomRecipe;
  Code := WfcLearnExecuteText(Command, Source, Output, ErrorText);
  Check((Code = WFC_LEARN_EXIT_SUCCESS) and (ErrorText = '') and
    (Output = ExpectedRecipe), 'native/shared CLI default retains all persisted quotas');
  Command.OutputMode := wlomQuiet;
  Code := WfcLearnExecuteText(Command, Source, Output, ErrorText);
  Check((Code = WFC_LEARN_EXIT_SUCCESS) and (Output = '') and (ErrorText = ''),
    'quiet training accepts a valid quota-bearing source without stdout');
  Command.OutputMode := wlomModel;
  Code := WfcLearnExecuteText(Command, Source, Output, ErrorText);
  Check((Code = WFC_LEARN_EXIT_INVALID_TRAINING) and (Output = '') and
    (Pos('standalone model', ErrorText) > 0) and (Pos('quota', ErrorText) > 0),
    'model-only export rejects rather than silently stripping authored quotas');
end;

procedure TestEveryOutputKind;
var W: TWfcTrainingWorkspace; O: TWfcTrainingSolveOptions;
  Q, CopyQ: TWfcTrainingValueQuotas; Output: TWfcModelTokens;
  Model: TWfcPipelineModel; P, N, Depth: Integer; Token: TWfcModelToken;
  OldSource, OldRecipe, OldModel, OldTraining, Source, Recipe, ResultText: String;
  Rejected: Boolean;
begin
  W := TWfcTrainingWorkspace.Create(InteractiveWfcTrainingWorkspaceLimits);
  try
    for P := 0 to TRAINING_STUDIO_PRESET_COUNT - 1 do
    begin
      W.SetSourceText(TrainingStudioPresetText(P)); W.Train;
      OldSource := W.SourceText; OldRecipe := W.RecipeText;
      OldModel := W.ModelText; OldTraining := W.TrainingSignatureText;
      O := TrainingStudioPresetOptions(P); Depth := TrainingStudioPresetDepth(P);
      Configure(W, O, Depth, P); W.Solve;
      Check(W.ResultStatus = wprsSolved, 'unmodified preset provides a feasible reference');
      Output := W.OutputTokens; Token := Output[0]; N := Count(Output, Token);
      Q := Quotas(Token, N, N); W.ReplaceValueQuotas(Q);
      Check(W.HasRecipe and (not W.HasRun) and (not W.HasResult),
        'quota edit retrains current recipe and invalidates old run/result');
      //Circular sequence semantics require source v5 even when quotas are
      //present; the six original presets retain their quota-only v3 format.
      if P = TRAINING_STUDIO_CIRCULAR_PRESET then
        Check(Pos('wfclearn=5'#10, W.SourceText) = 1,
          'circular preset quota preserves wrapped source v5')
      else if P=TRAINING_STUDIO_PATTERN3D_PRESET then
        Check(Pos('wfclearn=6'#10,W.SourceText)=1,'overlapping volume quota retains source v6')
      else Check(Pos('wfclearn=3'#10, W.SourceText) = 1,
        'original preset quota retains source v3: ' + IntToStr(P));
      if P=TRAINING_STUDIO_PATTERN3D_PRESET then
        Check(Pos('wfcpipeline=4'#10,W.RecipeText)=1,'overlapping volume quota retains recipe v4')
      else Check(Pos('wfcpipeline=2'#10, W.RecipeText) = 1,
        'legacy preset quota selects pipeline v2: ' + IntToStr(P));
      Check((W.ValueQuotaCount = 1) and
        (W.TrainingSignatureText <> OldTraining),
        'authoring changes source identity and retains one descriptor');
      Rejected := False;
      try Recipe := W.ModelText; except on E: EWfcTrainingWorkspace do Rejected := True; end;
      Check(Rejected, 'workspace refuses standalone model export that would strip quotas');
      Source := W.SourceText; Recipe := W.RecipeText;
      Q[0].Values[0] := 'caller-mutated';
      CopyQ := W.CopyValueQuotas; CopyQ[0].Values[0] := 'accessor-mutated';
      Check((W.SourceText = Source) and (W.CopyValueQuotas[0].Values[0] = Token),
        'workspace owns detached authoring descriptors');
      Model := DecodeWfcPipelineModelText(Recipe);
      try
        Check(Model.ResourceAt(0).Document = OldModel,
          'authored policy leaves the underlying learned model bytes unchanged');
        Check((Model.ValueQuotaCount = 1) and
          (Model.ValueQuotaAt(0).PassIndex = W.PublicPassIndex) and
          (Model.PassAt(W.PublicPassIndex).Visibility = wppvPublic),
          'learning binds quota to current public output, never a private state pass');
        if P in [2, 3, 4, TRAINING_STUDIO_CIRCULAR_PRESET,TRAINING_STUDIO_PATTERN3D_PRESET] then
          Check(W.PublicPassIndex = 1, 'projection authoring resolves public pass one')
        else Check(W.PublicPassIndex = 0, 'direct authoring resolves public pass zero');
      finally Model.Free; end;
      Configure(W, O, Depth, P); W.Solve; Output := W.OutputTokens;
      Check((W.ResultStatus = wprsSolved) and (Count(Output, Token) = N),
        'hard authored count independently matches every output cell');
      if P=TRAINING_STUDIO_PATTERN3D_PRESET then
        Check(TrainingStudioLatticeOutputIsValid(O.Width,O.Height,Depth,Output),
          'authored volume quota preserves supported planting semantics')
      else if P = 5 then
        Check(TrainingStudioVolumeOutputIsValid(O.Width, O.Height, Depth, Output) and
          (Length(Output) = O.Width * O.Height * Depth), 'volume quota counts all Z slices')
      else Check(TrainingStudioOutputIsValid(P, O.Width, O.Height, Output),
        'authored quota composes with independently validated learned structure');
      ResultText := W.ResultText; W.Solve;
      Check(W.ResultText = ResultText, 'repeating authored solve retains exact output');
      CheckFullReplay(W, O, Depth, P); CheckCli(Source, Recipe);
      W.Train;
      Check((W.SourceText = Source) and (W.RecipeText = Recipe) and
        (not W.HasRun) and (not W.HasResult), 'retraining cannot drop persisted constraints');
      Configure(W, O, Depth, P); W.Solve;
      Check(W.ResultText = ResultText, 'retraining identical source preserves exact seeded replay');
      W.ReplaceValueQuotas(nil);
      Check((W.SourceText = OldSource) and (W.RecipeText = OldRecipe) and
        (W.TrainingSignatureText = OldTraining) and (W.ModelText = OldModel),
        'removing last quota restores exact quota-free source, recipe and model identities');
    end;
  finally W.Free; end;
end;

procedure TestAuthoredOrderAndShape;
var W: TWfcTrainingWorkspace; O: TWfcTrainingSolveOptions;
  Q: TWfcTrainingValueQuotas; Model: TWfcPipelineModel;
  Doc: TWfcTrainingDocument; Vocabulary: TWfcModelTokens;
  Cafe: TWfcModelToken; I, V, Previous: Integer; OldRecipe, Source: String;
begin
  Cafe := WfcTextDecodeToken('caf%C3%A9', 'quota test');
  Q := Quotas('red', 1, 1); Q[0].Values := Tokens(['red', Cafe]);
  W := TWfcTrainingWorkspace.Create(InteractiveWfcTrainingWorkspaceLimits);
  try
    for I := 0 to 1 do
    begin
      Source := SourceWithQuotas(3, Q, I = 1);
      W.SetSourceText(Source); W.Train;
      Check((W.CopyValueQuotas[0].Values[0] = 'red') and
        (W.CopyValueQuotas[0].Values[1] = Cafe), 'author order persists across corpus reorder');
      Doc := DecodeWfcTrainingText(W.SourceText);
      try Check((Doc.ValueQuotaAt(0).Values[0] = 'red') and
        (EncodeWfcTrainingText(Doc) = Source), 'source codec preserves authored set order');
      finally Doc.Free; end;
      Vocabulary := W.PublicVocabulary;
      if I = 0 then Check(Vocabulary[0] = 'red', 'original corpus defines original token ordering')
      else Check(Vocabulary[0] = Cafe, 'reordered corpus really changes numeric vocabulary indices');
      Model := DecodeWfcPipelineModelText(W.RecipeText);
      try
        Previous := -1;
        for V := 0 to High(Model.ValueQuotaAt(0).Values) do
        begin
          Check((Model.ValueQuotaAt(0).Values[V] = 'red') or
            (Model.ValueQuotaAt(0).Values[V] = Cafe), 'compiled quota retains exact intended token identities');
          while (Previous + 1 < Length(Vocabulary)) and
            (Vocabulary[Previous + 1] <> Model.ValueQuotaAt(0).Values[V]) do Inc(Previous);
          Inc(Previous);
          Check(Previous < Length(Vocabulary), 'lowered accepted set follows fresh public vocabulary order');
        end;
      finally Model.Free; end;
      O := TrainingStudioPresetOptions(3); Configure(W, O, 1); W.Solve;
      Check((W.ResultStatus = wprsSolved) and
        (Count(W.OutputTokens, 'red') + Count(W.OutputTokens, Cafe) = 1),
        'reordered vocabulary cannot redirect an authored quota');
    end;
    W.SetSourceText(TrainingStudioPresetText(0)); W.Train;
    W.ReplaceValueQuotas(Quotas('A', 5, 5));
    O := TrainingStudioPresetOptions(0); O.Width := 10;
    Configure(W, O, 1); W.Solve;
    Check((W.ResultStatus = wprsSolved) and (Count(W.OutputTokens, 'A') = 5),
      'absolute quota is feasible at its authored output shape');
    OldRecipe := W.RecipeText; O.Width := 4;
    Configure(W, O, 1);
    Check((not W.HasResult) and (W.RecipeText = OldRecipe), 'shape edit invalidates output but not quota intent');
    W.Solve;
    Check((W.ResultStatus = wprsContradiction) and (Length(W.OutputTokens) = 0) and
      (W.CopyValueQuotas[0].MinimumCount = 5), 'shrinking below minimum yields contradiction, not silent clamping');
    O.Width := 10; Configure(W, O, 1); W.Solve;
    Check(W.ResultStatus = wprsSolved, 'expanding shape restores feasible authored intent');
    W.ReplaceValueQuotas(Quotas('A', 0, 100)); O.Width := 4;
    Configure(W, O, 1); W.Solve;
    Check((W.ResultStatus = wprsSolved) and (W.CopyValueQuotas[0].MaximumCount = 100),
      'upper quota bound larger than output size remains legal and unchanged');
  finally W.Free; end;
end;

procedure TestInvalidDraftLifecycle;
var W: TWfcTrainingWorkspace; Q: TWfcTrainingValueQuotas;
  O: TWfcTrainingSolveOptions; Source: String; I: Integer; Raised: Boolean;
begin
  W := TWfcTrainingWorkspace.Create(InteractiveWfcTrainingWorkspaceLimits);
  try
    Source := SourceWithQuotas(3, Quotas('red', 1, 1));
    for I := 0 to 7 do
    begin
      W.SetSourceText(Source); W.Train;
      O := TrainingStudioPresetOptions(3); Configure(W, O, 1); W.Solve;
      Q := Quotas('red', 1, 1);
      case I of
        0: Q[0].LabelText := '';
        1: Q[0].Values := nil;
        2: Q[0].Values[0] := 'missing-token';
        3: Q[0].MinimumCount := -1;
        4: Q[0].MaximumCount := 0;
        5: Q[0].Values := Tokens(['red', 'red']);
        6: begin SetLength(Q, 2); Q[1] := Q[0]; end;
        7: Q[0].LabelText := TWfcModelToken(StringOfChar('x',
          WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH + 1));
      end;
      Raised := False;
      try W.ReplaceValueQuotas(Q); except on E: Exception do Raised := True; end;
      Check(Raised, 'invalid authored quota edit is rejected'); CheckNoDerived(W, Source);
      W.Train;
      Check(W.ValueQuotaCount = 1, 'retained source recovers the last authored quota after correction');
    end;
    W.SetSourceText(Source); W.Train;
    Source := StringReplace(Source, 'token=0,0,red', 'token=0,0,blue', []);
    W.SetSourceText(Source); Raised := False;
    try W.Train; except on E: Exception do Raised := True; end;
    Check(Raised, 'removing a selected token from corpus rejects the stale author intent');
    CheckNoDerived(W, Source);
  finally W.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestBrowserBounds;
var W: TWfcTrainingWorkspace; Q: TWfcTrainingValueQuotas;
  I, Field, X: Integer; Source: String; Raised: Boolean;
begin
  W := TWfcTrainingWorkspace.Create(InteractiveWfcTrainingWorkspaceLimits);
  try
    Source := SourceWithQuotas(3, Quotas('red', 1, 1));
    for I := 0 to 7 do
    begin
      case I of
        0: asm X = NaN; end;
        1: asm X = Infinity; end;
        2: asm X = -Infinity; end;
        3: asm X = 0.5; end;
        4: asm X = undefined; end;
        5: asm X = "0"; end;
        6: asm X = null; end;
        7: asm X = 4294967296; end;
      end;
      for Field := 0 to 1 do
      begin
        W.SetSourceText(Source); W.Train; Q := Quotas('red', 1, 1);
        if Field = 0 then Q[0].MinimumCount := X else Q[0].MaximumCount := X;
        Raised := False;
        try W.ReplaceValueQuotas(Q); except on E: Exception do Raised := True; end;
        Check(Raised, 'malformed JavaScript numeric quota field is rejected');
        CheckNoDerived(W, Source);
      end;
    end;
  finally W.Free; end;
end;
{$ENDIF}

begin
  try
    Check((Pos('wfclearn=1,2,3,4,5', WfcLearnVersionText) > 0) and
      (Pos('rejects sources with value quotas or connectivity', WfcLearnHelpText) > 0),
      'CLI help advertises persisted policy sources and model-only rejection');
    TestEveryOutputKind;
    TestAuthoredOrderAndShape;
    TestInvalidDraftLifecycle;
    {$IFDEF PAS2JS}TestBrowserBounds;{$ENDIF}
    WriteLn('Persisted training value-quota checks: ', Checks);
  except on E: Exception do begin WriteLn('FAIL: ', E.ClassName, ': ', E.Message); Halt(1); end; end;
end.
