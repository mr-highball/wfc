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
program wfc_artifact_cli_app_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host, JS,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_rule_model, wfc_rule_text, wfc_text_codec,
  wfc_training, wfc_training_text, wfc_pipeline_model, wfc_pipeline_text,
  wfc_pipeline_run, wfc_pipeline_run_text, wfc_pipeline_result,
  wfc_pipeline_result_text, wfc_pipeline_runtime,
  wfc_artifact_document, wfc_validate_app, wfc_inspect_app;

var Checks: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;

function Args(const AValues: array of String): TWfcValidateArguments;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

function Tokens(const AValues: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

procedure RejectValidate(const AValues: array of String);
var C: TWfcValidateCommand; E: String;
begin
  Check(not WfcValidateParseCommand(Args(AValues), C, E) and (E <> ''),
    'reject validator arguments');
end;

procedure RejectInspect(const AValues: array of String);
var C: TWfcInspectCommand; E: String;
begin
  Check(not WfcInspectParseCommand(Args(AValues), C, E) and (E <> ''),
    'reject inspector arguments');
end;

procedure TestParsers;
var K: TWfcArtifactKind; C: TWfcValidateCommand; P: TWfcInspectCommand;
  A: TWfcValidateArguments; S, E: String; I, N: Integer;
begin
  for K := Low(TWfcArtifactKind) to High(TWfcArtifactKind) do
  begin
    S := WfcArtifactKindName(K);
    N := 1;
    if K = wakRun then N := 2 else if K = wakResult then N := 3;
    SetLength(A, N + 1); A[0] := S;
    for I := 1 to N do A[I] := 'path' + IntToStr(I);
    Check(WfcValidateParseCommand(A, C, E) and (E = '') and
      (WfcValidateCommandArtifactKind(C.Kind) = K) and
      (C.InputPath = A[N]) and not C.ReplayResult, 'family positional primary');
    if N >= 2 then Check(C.RecipePath = A[1], 'recipe context first');
    if N = 3 then Check(C.RunPath = A[2], 'run context second');
    Check(WfcInspectParseCommand(A, P, E) and
      (P.Kind = C.Kind) and (P.InputPath = C.InputPath) and
      (P.RecipePath = C.RecipePath) and (P.RunPath = C.RunPath) and
      (P.DetailLimit = 256), 'inspection uses same explicit context order');
    for I := 1 to N do
    begin
      S := A[I]; A[I] := '-';
      Check(WfcValidateParseCommand(A, C, E), 'one stdin at each position');
      Check(WfcInspectParseCommand(A, P, E), 'inspection stdin at each position');
      A[I] := S;
    end;
  end;
  Check(WfcValidateParseCommand(Args(['result', '--quiet', '--replay', 'r', 'u', '-']), C, E) and
    C.ReplayResult and (C.OutputMode = wvomQuiet), 'quiet replay parses');
  Check(WfcValidateParseCommand(Args(['result', '--replay', '--emit-canonical', '--', '-r', '-u', '-s']), C, E) and
    C.ReplayResult and (C.InputPath = '-s'), 'canonical replay escaped paths');
  Check(WfcValidateParseCommand(Args(['training', '--', '--quiet']), C, E) and
    (C.InputPath = '--quiet') and (C.OutputMode = wvomSummary), 'escaped option is a path');
  RejectValidate(['run', '-', '-']);
  RejectValidate(['result', '-', 'run', '-']);
  RejectValidate(['result', 'recipe', '-', '-']);
  RejectValidate(['result', 'recipe', 'run']);
  RejectValidate(['run', 'recipe', 'run', 'extra']);
  RejectValidate(['model', '']);
  RejectValidate(['result', '', 'run', 'result']);
  RejectValidate(['result', 'recipe', '', 'result']);
  RejectValidate(['result', 'recipe', 'run', '']);
  RejectValidate(['run', '--replay', 'recipe', 'run']);
  RejectValidate(['training', '--replay', 'input']);
  RejectValidate(['result', '--replay', '--replay', 'r', 'u', 's']);
  RejectValidate(['result', '--quiet', '--emit-canonical', 'r', 'u', 's']);
  RejectValidate(['result', '--emit-canonical', '--quiet', 'r', 'u', 's']);
  RejectValidate(['result', '--quiet', '--quiet', 'r', 'u', 's']);
  RejectValidate(['result', 'r', '--replay', 'u', 's']);
  RejectValidate(['run', 'r', '--', 'u']);
  RejectValidate(['rules', '--limit', '0', 'input']);
  Check(WfcInspectParseCommand(Args(['result', '--limit', '0', 'r', 'u', 's']), P, E) and
    (P.DetailLimit = 0), 'zero details');
  Check(WfcInspectParseCommand(Args(['training', '--limit', '2147483647', 's']), P, E) and
    (P.DetailLimit = High(Integer)), 'exact integer maximum detail limit');
  Check(WfcInspectParseCommand(Args(['recipe', '--', '--limit']), P, E) and
    (P.InputPath = '--limit'), 'inspector escaped option path');
  RejectInspect(['recipe', '--limit']);
  RejectInspect(['recipe', '--limit', '', 'r']);
  RejectInspect(['recipe', '--limit', '-1', 'r']);
  RejectInspect(['recipe', '--limit', '01', 'r']);
  RejectInspect(['recipe', '--limit', '+1', 'r']);
  RejectInspect(['recipe', '--limit', '1.0', 'r']);
  RejectInspect(['recipe', '--limit', 'NaN', 'r']);
  RejectInspect(['recipe', '--limit', 'Infinity', 'r']);
  RejectInspect(['recipe', '--limit', '2147483648', 'r']);
  RejectInspect(['recipe', '--limit', '1', '--limit', '2', 'r']);
  RejectInspect(['recipe', 'r', '--limit', '1']);
  RejectInspect(['recipe', '--quiet', 'r']);
  RejectInspect(['recipe', '--emit-canonical', 'r']);
  RejectInspect(['result', '--replay', 'r', 'u', 's']);
  RejectInspect(['run', '-', '-']);
  RejectInspect(['--help', '--limit', '1']);
  RejectInspect(['--version', 'extra']);
  RejectInspect([]);
  RejectInspect(['unknown', 'path']);
end;

function Training(const AKind: TWfcTrainingKind; const APolicy: Boolean): TWfcTrainingDocument;
var S: TWfcTrainingSamples; O: TWfcTrainingOptions;
  C: TWfcTrainingConnectivities; V: TWfcTrainingConnectivityValues;
  Q: TWfcTrainingValueQuotas; R: TGraphPosition;
begin
  SetLength(S, 1);
  O := MakeWfcTrainingOptions(AKind, wmbWrap, wmsNone, 0, 0, 0);
  if AKind = wtkPattern2D then
  begin
    O.PatternWidth := 1; O.PatternHeight := 1;
    S[0] := MakeWfcTrainingSample('grid', 2, 2, Tokens(['A', 'B', 'B', 'A']));
  end
  else S[0] := MakeWfcTrainingSample('strip', 4, 1, Tokens(['A', 'B', 'A', 'B']));
  if AKind = wtkSequence then begin O.Order := 2; O.Boundary := wmbOpen; end;
  if APolicy then
  begin
    R := Default(TGraphPosition);
    SetLength(V, 2);
    V[0] := MakeWfcTrainingConnectivityValue('B', [gdEast, gdWest]);
    V[1] := MakeWfcTrainingConnectivityValue('A', [gdEast, gdWest]);
    SetLength(C, 1);
    C[0] := MakeWfcTrainingConnectivity('route', R, nil, V, True);
    SetLength(Q, 1);
    Q[0] := MakeWfcTrainingValueQuota('some-A', Tokens(['A']), 0, 99);
  end;
  Result := TWfcTrainingDocument.Create(MakeWfcTrainingMetadata('cli fixture', 'MIT',
    'project-authored'), O, S, Q, C);
end;

procedure CheckArtifact(const AFamily, AInput, ARecipe, ARun: String);
var A: TWfcValidateArguments; C: TWfcValidateCommand; P: TWfcInspectCommand;
  E, O: String; Code: Integer;
begin
  if AFamily = 'result' then A := Args([AFamily, 'recipe', 'run', 'input'])
  else if AFamily = 'run' then A := Args([AFamily, 'recipe', 'input'])
  else A := Args([AFamily, 'input']);
  Check(WfcValidateParseCommand(A, C, E), 'execution parser');
  Code := WfcValidateExecuteText(C, AInput, ARecipe, ARun, O, E);
  Check((Code = 0) and (E = '') and (Pos('valid canonical ', O) = 1), 'summary: ' + AFamily + ' ' + E);
  C.OutputMode := wvomQuiet;
  Code := WfcValidateExecuteText(C, AInput, ARecipe, ARun, O, E);
  Check((Code = 0) and (O = '') and (E = ''), 'quiet: ' + AFamily);
  C.OutputMode := wvomCanonical;
  Code := WfcValidateExecuteText(C, AInput, ARecipe, ARun, O, E);
  Check((Code = 0) and (O = AInput) and (E = ''), 'canonical: ' + AFamily);
  Code := WfcValidateExecuteText(C, AInput + 'end'#10, ARecipe, ARun, O, E);
  Check((Code = 1) and (O = '') and
    (Pos('wfc-validate: invalid ' + AFamily + ': ', E) = 1), 'invalid family: ' + AFamily);
  Check(WfcInspectParseCommand(A, P, E), 'inspect execution parser');
  P.DetailLimit := 0;
  Code := WfcInspectExecuteText(P, AInput, ARecipe, ARun, O, E);
  Check((Code = 0) and (O <> '') and (E = ''), 'inspect summary: ' + AFamily + ' ' + E);
  P.DetailLimit := 1;
  Code := WfcInspectExecuteText(P, AInput, ARecipe, ARun, O, E);
  Check((Code = 0) and (O <> '') and (E = ''), 'inspect bounded details: ' + AFamily);
  Code := WfcInspectExecuteText(P, AInput + 'end'#10, ARecipe, ARun, O, E);
  Check((Code = 1) and (O = '') and
    (Pos('wfc-inspect: invalid ' + AFamily + ': ', E) = 1), 'inspect invalid family: ' + AFamily);
end;

procedure TestExecution;
var D: TWfcTrainingDocument; Recipe: TWfcPipelineModel;
  Run: TWfcPipelineRun; Stored: TWfcPipelineResult; Rule: TWfcRuleModel;
  Weights: TWfcModelIntegerArray; Domains: TWfcPipelineCellDomains;
  SourceText, RecipeText, RunText, ResultText, E, O: String;
  C: TWfcValidateCommand; P: TWfcInspectCommand; Code, I: Integer;
begin
  SetLength(Weights, 1); Weights[0] := 1;
  Rule := TWfcRuleModel.Create(1, Tokens(['A']), Weights, nil);
  try CheckArtifact('rules', EncodeWfcRuleText(Rule), '', ''); finally Rule.Free; end;
  D := Training(wtkAdjacency1D, False);
  try CheckArtifact('model', LearnWfcTrainingModelText(D), '', ''); finally D.Free; end;
  D := Training(wtkPattern2D, False);
  try CheckArtifact('pattern2d', LearnWfcTrainingModelText(D), '', ''); finally D.Free; end;
  D := Training(wtkSequence, False);
  try CheckArtifact('sequence', LearnWfcTrainingModelText(D), '', ''); finally D.Free; end;
  D := Training(wtkAdjacency1D, True);
  try
    SourceText := EncodeWfcTrainingText(D);
    Check(Pos('wfclearn=4'#10, SourceText) = 1, 'source4 fixture');
    CheckArtifact('training', SourceText, '', '');
    Recipe := LearnWfcTrainingRecipe(D);
  finally D.Free; end;
  try
    RecipeText := EncodeWfcPipelineModelText(Recipe);
    CheckArtifact('recipe', RecipeText, '', '');
    for I := 0 to 1 do
    begin
      Domains := nil;
      if I = 1 then
      begin
        SetLength(Domains, 1);
        Domains[0] := MakeWfcPipelineCellDomain(0, 0, 0, 0, nil);
      end;
      Run := TWfcPipelineRun.Create(Recipe, 4, 1, 1, 7, wpssNegotiated,
        256, 16, True, nil, Domains);
      try
        RunText := EncodeWfcPipelineRunText(Run);
        CheckArtifact('run', RunText, RecipeText, '');
        Stored := ExecuteWfcPipeline(Recipe, Run);
        try
          Check((I = 1) = (Stored.Status <> wprsSolved), 'solved/non-solved fixture');
          ResultText := EncodeWfcPipelineResultText(Stored);
        finally Stored.Free; end;
        CheckArtifact('result', ResultText, RecipeText, RunText);
        Check(WfcValidateParseCommand(Args(['result', '--replay', 'r', 'u', 's']), C, E), 'replay parser');
        Code := WfcValidateExecuteText(C, ResultText, RecipeText, RunText, O, E);
        Check((Code = 0) and (E = '') and (O <> ''), 'exact replay including non-solved: ' + E);
        C.OutputMode := wvomQuiet;
        Code := WfcValidateExecuteText(C, ResultText, RecipeText, RunText, O, E);
        Check((Code = 0) and (O = '') and (E = ''), 'quiet replay including non-solved');
        C.OutputMode := wvomCanonical;
        Code := WfcValidateExecuteText(C, ResultText, RecipeText, RunText, O, E);
        Check((Code = 0) and (O = ResultText) and (E = ''), 'canonical replay including non-solved');
        Code := WfcValidateExecuteText(C, ResultText, '', RunText, O, E);
        Check((Code = 1) and (O = '') and (E <> ''), 'missing recipe context');
        Code := WfcValidateExecuteText(C, ResultText, RecipeText, '', O, E);
        Check((Code = 1) and (O = '') and (E <> ''), 'missing run context');
        Code := WfcValidateExecuteText(C, ResultText, O, E);
        Check((Code = 1) and (O = '') and (E <> ''), 'single-text API cannot invent contexts');
      finally Run.Free; end;
    end;
  finally Recipe.Free; end;
  Check(WfcValidateParseCommand(Args(['training', 'source']), C, E), 'source parser');
  Code := WfcValidateExecuteText(C, SourceText + #$80, O, E);
  Check((Code = 1) and (O = '') and (Pos('non-ASCII byte', E) > 0), 'raw non-ASCII source');
  {$IFDEF PAS2JS}
  asm C.OutputMode = '0'; end;
  Code := WfcValidateExecuteText(C, SourceText, O, E);
  Check((Code = 2) and (O = '') and (E <> ''), 'string output enum rejected');
  asm C.OutputMode = NaN; end;
  Code := WfcValidateExecuteText(C, SourceText, O, E);
  Check((Code = 2) and (O = ''), 'NaN output enum rejected');
  asm C.OutputMode = 0.5; end;
  Code := WfcValidateExecuteText(C, SourceText, O, E);
  Check((Code = 2) and (O = ''), 'fractional output enum rejected');
  C.Kind := wvckResult; C.OutputMode := wvomSummary;
  asm C.ReplayResult = 'false'; end;
  Code := WfcValidateExecuteText(C, '', '', '', O, E);
  Check((Code = 2) and (O = '') and (Pos('must be Boolean', E) > 0), 'string replay flag rejected before decoding');
  asm C.ReplayResult = 0; end;
  Code := WfcValidateExecuteText(C, '', '', '', O, E);
  Check((Code = 2) and (O = ''), 'numeric replay flag rejected');
  asm C.ReplayResult = null; end;
  Code := WfcValidateExecuteText(C, '', '', '', O, E);
  Check((Code = 2) and (O = ''), 'null replay flag rejected');
  {$ENDIF}
  Check(WfcInspectParseCommand(Args(['training', 'source']), P, E), 'source inspection parser');
  P.DetailLimit := -1;
  Code := WfcInspectExecuteText(P, SourceText, O, E);
  Check((Code = 2) and (O = '') and (E <> ''), 'negative public detail limit rejected');
  {$IFDEF PAS2JS}
  asm P.DetailLimit = NaN; end;
  Code := WfcInspectExecuteText(P, SourceText, O, E);
  Check((Code = 2) and (O = ''), 'NaN public detail limit rejected');
  asm P.DetailLimit = Infinity; end;
  Code := WfcInspectExecuteText(P, SourceText, O, E);
  Check((Code = 2) and (O = ''), 'infinite public detail limit rejected');
  asm P.DetailLimit = 0.5; end;
  Code := WfcInspectExecuteText(P, SourceText, O, E);
  Check((Code = 2) and (O = ''), 'fractional public detail limit rejected');
  {$ENDIF}
end;

procedure TestMessages;
var P: TWfcInspectCommand; C: TWfcValidateCommand; E, O: String; Code: Integer;
begin
  Check(WfcInspectParseCommand(Args(['--help']), P, E), 'help parses');
  Code := WfcInspectExecuteText(P, '', O, E);
  Check((Code = 0) and (E = '') and (O = WfcInspectHelpText) and
    (Pos('never executes or replays', O) > 0), 'inspect help scope');
  Check(WfcInspectParseCommand(Args(['--version']), P, E), 'version parses');
  Code := WfcInspectExecuteText(P, '', O, E);
  Check((Code = 0) and (E = '') and (O = 'wfc-inspect 1'#10), 'inspect version');
  Check(WfcValidateParseCommand(Args(['--help']), C, E), 'validate help parses');
  Code := WfcValidateExecuteText(C, '', O, E);
  Check((Code = 0) and (E = '') and (Pos('Only result --replay executes', O) > 0) and
    (Pos('non-solved results succeed with exit code 0', O) > 0), 'validate help limits');
  Check(WfcValidateOneLineMessage('bad'#0#9#10#13#27#127'message') = 'bad      message',
    'diagnostic controls sanitized');
  Check(Length(WfcValidateOneLineMessage(StringOfChar('x', 5000))) = 4096, 'diagnostic length bound');
  Check(WfcValidateOneLineMessage('') = 'unspecified failure', 'empty diagnostic');
end;

begin
  TestParsers;
  TestExecution;
  TestMessages;
  WriteLn('[SUMMARY] checks=', Checks, ' failures=0');
end.
