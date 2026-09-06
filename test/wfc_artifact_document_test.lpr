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
program wfc_artifact_document_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_rule_model, wfc_rule_text,
  wfc_model_text, wfc_pattern2d_text, wfc_sequence, wfc_sequence_text,
  wfc_training, wfc_training_text, wfc_pipeline_model, wfc_pipeline_text,
  wfc_pipeline_run, wfc_pipeline_run_text, wfc_pipeline_result,
  wfc_pipeline_result_text, wfc_pipeline_runtime, wfc_artifact_document;

type TTestProcedure = procedure;

const
  LEGACY_RECIPE =
    'wfcpipeline=1'#10 + 'name=codec'#10 + 'license=MIT'#10 +
    'source='#10 + 'fingerprint='#10 + 'graph-model-version=1'#10 +
    'random-algorithm-version=1'#10 + 'solver-algorithm-version=2'#10 +
    'pipeline-algorithm-version=2'#10 + 'bundle-graph-adapter-version=1'#10 +
    'model-graph-adapter-version=1'#10 + 'rules-graph-adapter-version=1'#10 +
    'pattern2d-graph-adapter-version=1'#10 + 'sequence-graph-adapter-version=1'#10 +
    'pattern2d-bridge-version=1'#10 + 'sequence-bridge-version=1'#10 +
    'rank=1'#10 + 'wrap=false'#10 + 'traversal=bottom-up'#10 + 'resources=1'#10 +
    'resource=0,basic-rules,rules,' +
      'wfcrules%3D1%0Arank%3D1%0Avalues%3D1%0Av%3D0%2C1%2Conly%0A' +
      'rules%3D0%0Asignature%3DD83BEE6A%0Aend%0A,pipeline%20codec%20test,MIT,'#10 +
    'passes=1'#10 + 'pass=0,layer,public,legacy,-1,rules,0,false,whole'#10 +
    'dependencies=0'#10 + 'bridges=0'#10 + 'requirements=0'#10 +
    'signature=9B25EECF'#10 + 'end'#10;
  LEGACY_SUMMARY = 'valid canonical wfcpipeline=1 signature=9B25EECF' +
    ' resources=1 passes=1 dependencies=0 bridges=0 requirements=0'#10;

var
  Checks, Failures: Integer;
  Texts: array[TWfcArtifactKind] of String;

procedure Check(const Condition: Boolean; const MessageText: String);
begin
  Inc(Checks);
  if Condition then Exit;
  Inc(Failures); WriteLn('[FAIL] ', MessageText);
end;

procedure RunTest(const Name: String; const Test: TTestProcedure);
begin
  WriteLn('[TEST] ', Name);
  try Test;
  except on E: Exception do
    begin Inc(Failures); WriteLn('[EXCEPTION] ', E.ClassName, ': ', E.Message); end;
  end;
end;

function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(Values));
  for I := 0 to High(Values) do Result[I] := Values[I];
end;

function Point(const X: Integer): TGraphPosition;
begin Result := Default(TGraphPosition); Result.X := X; end;

function Training(const Kind: TWfcTrainingKind; const Policies: Boolean = False;
  const OpenPattern: Boolean = False): TWfcTrainingDocument;
var
  O: TWfcTrainingOptions; S: TWfcTrainingSamples;
  Q: TWfcTrainingValueQuotas; C: TWfcTrainingConnectivities;
  V: TWfcTrainingConnectivityValues;
begin
  Q := nil; C := nil;
  O := MakeWfcTrainingOptions(Kind, wmbWrap, wmsNone, 0, 0, 0);
  SetLength(S, 1);
  if Kind = wtkPattern2D then
  begin
    O.PatternWidth := 2; O.PatternHeight := 2;
    if OpenPattern then O.Boundary := wmbOpen;
    S[0] := MakeWfcTrainingSample('square', 2, 2, Tokens(['A', 'B', 'B', 'A']));
  end
  else if Kind in [wtkAdjacency3D,wtkPattern3D] then
  begin
    if Kind=wtkPattern3D then
    begin O.PatternWidth:=2; O.PatternHeight:=1; O.PatternDepth:=2; end;
    S[0] := MakeWfcTrainingSample('volume', 2, 1, 2, Tokens(['A', 'B', 'B', 'A']))
  end
  else
    S[0] := MakeWfcTrainingSample('strip', 4, 1, Tokens(['A', 'B', 'A', 'B']));
  if Kind = wtkSequence then
  begin O.Order := 2; O.Boundary := wmbOpen; end;
  if Policies then
  begin
    SetLength(Q, 1); Q[0] := MakeWfcTrainingValueQuota('presence', Tokens(['A']), 1, 5);
    SetLength(V, 1); V[0] := MakeWfcTrainingConnectivityValue('A', [gdEast, gdWest], True);
    SetLength(C, 1); C[0] := MakeWfcTrainingConnectivity('route', Point(0), nil, V, True);
  end;
  Result := TWfcTrainingDocument.Create(
    MakeWfcTrainingMetadata('artifact', 'MIT', 'authored fixture'), O, S, Q, C);
end;

function Recipe(const AliasPass: Boolean = False): TWfcPipelineModel;
var P: TWfcPipelinePasses; R: TWfcPipelineResources; D: TWfcPipelineDependencies;
begin
  SetLength(R, 1);
  R[0] := MakeWfcPipelineResource('rules', wprkRules, Texts[wakRules], 'artifact rules', 'MIT', '');
  SetLength(P, 1); D := nil;
  P[0] := MakeWfcPipelinePass('output', wppvPublic, gpmOverlay,
    -1, wpakRules, 0, False, wseWhole);
  if AliasPass then
  begin
    SetLength(P, 2);
    P[1] := MakeWfcPipelinePass('copy', wppvPublic, gpmTransform,
      0, wpakEmpty, -1, False, wseWhole);
    SetLength(D, 1); D[0] := MakeWfcPipelineDependency(1, 0);
  end;
  Result := TWfcPipelineModel.Create(
    MakeWfcPipelineMetadata('artifact', 'MIT', '', ''),
    1, False, rmBottomUp, R, P, D, nil, nil);
end;

function Run(const R: TWfcPipelineModel; const EmptyDomain: Boolean = False;
  const Seed: TGraphSeed = 4): TWfcPipelineRun;
var L: TWfcPipelineCellLocks; D: TWfcPipelineCellDomains;
begin
  L := nil; D := nil;
  if EmptyDomain then
  begin SetLength(D, 1); D[0] := MakeWfcPipelineCellDomain(0, 0, 0, 0, nil); end
  else
  begin SetLength(L, 1); L[0] := MakeWfcPipelineCellLock(0, 0, 0, 0, 'A'); end;
  Result := TWfcPipelineRun.Create(R, 2, 1, 1, Seed,
    wpssOneWay, 32, 0, True, L, D);
end;

procedure MakeFixtures;
var
  R: TWfcRuleModel; Weights: TWfcModelIntegerArray; D: TWfcTrainingDocument;
  P: TWfcPipelineModel; U: TWfcPipelineRun; S: TWfcPipelineResult;
begin
  SetLength(Weights, 2); Weights[0] := 1; Weights[1] := 1;
  R := TWfcRuleModel.Create(1, Tokens(['A', 'B']), Weights, nil);
  try Texts[wakRules] := EncodeWfcRuleText(R); finally R.Free; end;
  D := Training(wtkAdjacency1D);
  try Texts[wakModel] := LearnWfcTrainingModelText(D); finally D.Free; end;
  D := Training(wtkPattern2D);
  try Texts[wakPattern2D] := LearnWfcTrainingModelText(D); finally D.Free; end;
  D := Training(wtkPattern3D);
  try Texts[wakPattern3D] := LearnWfcTrainingModelText(D); finally D.Free; end;
  D := Training(wtkSequence);
  try Texts[wakSequence] := LearnWfcTrainingModelText(D); finally D.Free; end;
  D := Training(wtkAdjacency1D, True);
  try Texts[wakTraining] := EncodeWfcTrainingText(D); finally D.Free; end;
  P := Recipe;
  try
    Texts[wakRecipe] := EncodeWfcPipelineModelText(P);
    U := Run(P);
    try
      Texts[wakRun] := EncodeWfcPipelineRunText(U);
      S := ExecuteWfcPipeline(P, U);
      try
        Check(S.Status = wprsSolved, 'canonical replay fixture solves');
        Texts[wakResult] := EncodeWfcPipelineResultText(S);
      finally S.Free; end;
    finally U.Free; end;
  finally P.Free; end;
end;

function OpenArtifact(const K: TWfcArtifactKind; const Text: String): TWfcArtifactDocument;
var R, U: String;
begin
  R := ''; U := '';
  if K in [wakRun, wakResult] then R := Texts[wakRecipe];
  if K = wakResult then U := Texts[wakRun];
  Result := TWfcArtifactDocument.Create(K, Text, R, U);
end;

function Invalid(const K: TWfcArtifactKind; const Text: String): Boolean;
var D: TWfcArtifactDocument;
begin
  Result := False; D := nil;
  try
    try D := OpenArtifact(K, Text);
    except on E: EWfcArtifactInvalid do Result := True; end;
  finally D.Free; end;
end;

function ContextInvalid(const K: TWfcArtifactKind; const Text, R, U: String): Boolean;
var D: TWfcArtifactDocument;
begin
  Result := False; D := nil;
  try
    try D := TWfcArtifactDocument.Create(K, Text, R, U);
    except on E: EWfcArtifactInvalid do Result := True; end;
  finally D.Free; end;
end;

procedure TestFamilies;
var K: TWfcArtifactKind; D: TWfcArtifactDocument; N: Integer; T: String;
begin
  for K := Low(TWfcArtifactKind) to High(TWfcArtifactKind) do
  begin
    D := OpenArtifact(K, Texts[K]);
    try
      Check(D.Kind = K, WfcArtifactKindName(K) + ' preserves declared kind');
      Check(D.CanonicalText = Texts[K], 'canonical bytes retained unchanged');
      Check((D.Summary <> '') and (D.Summary[Length(D.Summary)] = #10), 'summary has final LF');
      N := Ord(D.Rules <> nil) + Ord(D.Model <> nil) + Ord(D.Pattern2D <> nil) +
        Ord(D.Sequence <> nil) + Ord(D.Training <> nil) + Ord(D.Recipe <> nil) +
        Ord(D.Run <> nil) + Ord(D.StoredResult <> nil) + Ord(D.Pattern3D <> nil);
      if K = wakResult then Check(N = 3, 'result owns exactly its binding chain')
      else if K = wakRun then Check(N = 2, 'run owns exactly its recipe and run')
      else Check(N = 1, 'standalone family has one typed owned document');
      case K of
        wakRules: Check(D.Rules <> nil, 'typed rules view');
        wakModel: Check(D.Model <> nil, 'typed model view');
        wakPattern2D: Check(D.Pattern2D <> nil, 'typed pattern view');
        wakPattern3D: Check(D.Pattern3D <> nil,'typed volume footprint view');
        wakSequence: Check(D.Sequence <> nil, 'typed sequence view');
        wakTraining: Check((D.Training.ValueQuotaCount = 1) and
          (D.Training.ConnectivityCount = 1) and (Pos('wfclearn=4'#10, D.CanonicalText) = 1),
          'source4 retains both authored policies without learning');
        wakRecipe: Check(D.Recipe <> nil, 'typed recipe view');
        wakRun: Check(D.Run <> nil, 'typed run view');
        wakResult: Check(D.StoredResult <> nil, 'typed result view');
      end;
      if K <> wakResult then
      begin
        T := '';
        try D.RequireReplay; except on E: EWfcArtifactReplayInvocation do T := E.Message; end;
        Check(T = 'replay requires a result artifact', 'only result supports replay');
      end;
    finally D.Free; end;
  end;
  Check(WfcArtifactKindName(wakRules) = 'rules', 'rules command name');
  Check(WfcArtifactKindName(wakModel) = 'model', 'model command name');
  Check(WfcArtifactKindName(wakPattern2D) = 'pattern2d', 'pattern2d command name');
  Check(WfcArtifactKindName(wakSequence) = 'sequence', 'sequence command name');
  Check(WfcArtifactKindName(wakTraining) = 'training', 'training command name');
  Check(WfcArtifactKindName(wakRecipe) = 'recipe', 'recipe command name');
  Check(WfcArtifactKindName(wakRun) = 'run', 'run command name');
  Check(WfcArtifactKindName(wakResult) = 'result', 'result command name');
end;

procedure TestStrictnessAndContexts;
var K: TWfcArtifactKind; D: TWfcArtifactDocument; T, DirectError, WrapperError: String;
  I: Integer; P: TWfcPipelineModel; U: TWfcPipelineRun;
begin
  for K := Low(TWfcArtifactKind) to High(TWfcArtifactKind) do
  begin
    Check(Invalid(K, ''), 'empty ' + WfcArtifactKindName(K) + ' rejected');
    Check(Invalid(K, Copy(Texts[K], 1, Length(Texts[K]) - 1)), 'missing final LF rejected');
    Check(Invalid(K, Texts[K] + #10), 'trailing empty record rejected');
    Check(Invalid(K, StringReplace(Texts[K], #10, #13#10, [rfReplaceAll])), 'CRLF rejected');
    I := Pos(#10, Texts[K]);
    Check(Invalid(K, Copy(Texts[K], 1, Pos('=', Texts[K])) + '999' +
      Copy(Texts[K], I, Length(Texts[K]))), 'unsupported family version rejected');
    Check(Invalid(K, #$FF + Texts[K]), 'non-ASCII input rejected');
    if not (K in [wakRun, wakResult]) then
    begin
      Check(ContextInvalid(K, Texts[K], Texts[wakRecipe], ''), 'standalone rejects recipe context');
      Check(ContextInvalid(K, Texts[K], '', Texts[wakRun]), 'standalone rejects run context');
    end;
  end;
  Check(ContextInvalid(wakRun, Texts[wakRun], '', ''), 'run requires recipe');
  Check(ContextInvalid(wakRun, Texts[wakRun], Texts[wakRecipe], Texts[wakRun]), 'run rejects extraneous run');
  Check(ContextInvalid(wakResult, Texts[wakResult], '', Texts[wakRun]), 'result requires recipe');
  Check(ContextInvalid(wakResult, Texts[wakResult], Texts[wakRecipe], ''), 'result requires run');
  Check(ContextInvalid(wakResult, Texts[wakResult], 'bad', Texts[wakRun]), 'malformed recipe context rejected');
  Check(ContextInvalid(wakResult, Texts[wakResult], Texts[wakRecipe], 'bad'), 'malformed run context rejected');
  Check(ContextInvalid(wakResult, Texts[wakResult], #$FF, Texts[wakRun]), 'non-ASCII recipe context rejected');
  Check(ContextInvalid(wakResult, Texts[wakResult], Texts[wakRecipe], #$FF), 'non-ASCII run context rejected');
  Check(ContextInvalid(wakRun, Texts[wakRun], LEGACY_RECIPE, ''), 'wrong recipe identity rejected');
  P := Recipe;
  try
    U := Run(P, False, 5);
    try Check(ContextInvalid(wakResult, Texts[wakResult], Texts[wakRecipe],
      EncodeWfcPipelineRunText(U)), 'wrong run identity rejected'); finally U.Free; end;
  finally P.Free; end;
  DirectError := ''; P := nil;
  try
    try P := DecodeWfcPipelineModelText('bad');
    except on E: EConvertError do DirectError := E.Message; end;
  finally P.Free; end;
  WrapperError := ''; D := nil;
  try
    try D := TWfcArtifactDocument.Create(wakRecipe, 'bad', '', '');
    except on E: EWfcArtifactInvalid do WrapperError := E.Message; end;
  finally D.Free; end;
  Check((DirectError <> '') and (WrapperError = DirectError), 'legacy decoder diagnostic bytes unchanged');
  D := TWfcArtifactDocument.Create(wakRecipe, LEGACY_RECIPE, '', '');
  try
    Check(D.CanonicalText = LEGACY_RECIPE, 'literal legacy recipe golden preserved');
    Check(D.Summary = LEGACY_SUMMARY, 'literal legacy recipe summary golden preserved');
  finally D.Free; end;
  T := StringOfChar('x', WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH + 1);
  Check(Invalid(wakTraining, T), 'over-limit source rejected before decoder allocation');
end;

procedure TestLimits;
begin
  Check(WfcArtifactInputLimit(wakRules) = WFC_RULE_MAX_ENCODED_TEXT_LENGTH, 'rules cap');
  Check(WfcArtifactInputLimit(wakModel) = WFC_MODEL_MAX_ENCODED_TEXT_LENGTH, 'model cap');
  Check(WfcArtifactInputLimit(wakPattern2D) = WFC_PATTERN_2D_MAX_ENCODED_TEXT_LENGTH, 'pattern cap');
  Check(WfcArtifactInputLimit(wakSequence) = WFC_SEQUENCE_MAX_ENCODED_TEXT_LENGTH, 'sequence cap');
  Check(WfcArtifactInputLimit(wakTraining) = WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH, 'training cap');
  Check(WfcArtifactInputLimit(wakRecipe) = WFC_PIPELINE_MAX_ENCODED_TEXT_LENGTH, 'recipe cap');
  Check(WfcArtifactInputLimit(wakRun) = WFC_PIPELINE_RUN_MAX_ENCODED_TEXT_LENGTH, 'run cap');
  Check(WfcArtifactInputLimit(wakResult) = WFC_PIPELINE_RESULT_MAX_ENCODED_TEXT_LENGTH, 'result cap');
end;

procedure TestReplay;
var
  P: TWfcPipelineModel; U: TWfcPipelineRun; S, Forged: TWfcPipelineResult;
  D: TWfcArtifactDocument; Layers: TWfcPipelineResultLayers;
  Outcomes: TWfcPipelinePassOutcomes; RText, UText, SText, SummaryText: String;
  Failed: Boolean; I: Integer;
begin
  RText := Texts[wakRecipe]; UText := Texts[wakRun]; SText := Texts[wakResult];
  D := TWfcArtifactDocument.Create(wakResult, SText, RText, UText);
  try
    SummaryText := D.Summary;
    D.RequireReplay; D.RequireReplay;
    Check(D.CanonicalText = SText, 'repeated fresh replay leaves canonical input unchanged');
    Check(D.Summary = SummaryText, 'replay does not relabel schema validation as an independent proof');
    Check((RText = Texts[wakRecipe]) and (UText = Texts[wakRun]) and
      (SText = Texts[wakResult]), 'caller input strings remain unchanged');
  finally D.Free; end;
  P := Recipe;
  try
    U := Run(P);
    try
      S := ExecuteWfcPipeline(P, U);
      try
        for I := 0 to 1 do
        begin
          Layers := S.CopyLayers; Outcomes := S.CopyPassOutcomes;
          if I = 0 then Layers[0].Tokens[0] := 'B'
          else Inc(Outcomes[0].Propagations);
          Forged := TWfcPipelineResult.Create(P, U, S.CopyVersions,
            S.Status, S.PassBacktracks, S.EvidenceKind, S.EvidenceSignature,
            S.CopyFailure, Outcomes, Layers);
          try SText := EncodeWfcPipelineResultText(Forged); finally Forged.Free; end;
          D := TWfcArtifactDocument.Create(wakResult, SText, Texts[wakRecipe], Texts[wakRun]);
          try
            Check(D.StoredResult.Status = wprsSolved, 'canonical forged output/counter passes existing result schema');
            Failed := False;
            try D.RequireReplay; except on E: EWfcArtifactReplayMismatch do Failed := True; end;
            Check(Failed, 'full replay rejects changed output/counter, despite a valid recomputed signature');
            Check(D.CanonicalText = SText, 'failed replay preserves the stored document');
          finally D.Free; end;
        end;
      finally S.Free; end;
    finally U.Free; end;
    U := Run(P, True);
    try
      S := ExecuteWfcPipeline(P, U);
      try
        Check(S.Status = wprsContradiction, 'empty public domain produces valid terminal contradiction');
        SText := EncodeWfcPipelineResultText(S);
      finally S.Free; end;
      D := TWfcArtifactDocument.Create(wakResult, SText,
        Texts[wakRecipe], EncodeWfcPipelineRunText(U));
      try
        D.RequireReplay;
        Check(Pos('status=contradiction', D.Summary) > 0, 'exact non-solved replay succeeds and reports its status');
        Check(D.StoredResult.LayerCount = 0, 'non-solved replay does not invent output');
      finally D.Free; end;
    finally U.Free; end;
  finally P.Free; end;
end;

procedure TestReplayInvocation;
var
  P: TWfcPipelineModel; U: TWfcPipelineRun; S: TWfcPipelineResult;
  L: TWfcPipelineCellLocks; O: TWfcPipelinePassOutcomes;
  Layers: TWfcPipelineResultLayers; D: TWfcArtifactDocument; I: Integer; Failed: Boolean;
begin
  P := Recipe(True);
  try
    SetLength(L, 2);
    L[0] := MakeWfcPipelineCellLock(0, 0, 0, 0, 'A');
    L[1] := MakeWfcPipelineCellLock(1, 0, 0, 0, 'B');
    U := TWfcPipelineRun.Create(P, 1, 1, 1, 0, wpssOneWay, 32, 0, False, L, nil);
    try
      SetLength(O, 2); SetLength(Layers, 2);
      for I := 0 to 1 do
      begin
        O[I] := Default(TWfcPipelinePassOutcome); O[I].PassIndex := I;
        O[I].Executed := True; O[I].ExecutionOrdinal := I;
        if I = 0 then O[I].Disposition := gpdSolved else O[I].Disposition := gpdCopied;
        if I = 0 then Layers[I] := MakeWfcPipelineResultLayer(I, 'output', Tokens(['A']))
        else Layers[I] := MakeWfcPipelineResultLayer(I, 'copy', Tokens(['B']));
      end;
      S := TWfcPipelineResult.Create(P, U, CurrentWfcPipelineResultVersions,
        wprsSolved, 0, wpekNone, 0, EmptyWfcPipelineFailure, O, Layers);
      try
        D := TWfcArtifactDocument.Create(wakResult, EncodeWfcPipelineResultText(S),
          EncodeWfcPipelineModelText(P), EncodeWfcPipelineRunText(U));
        try
          Check(D.Run.LockCount = 2, 'schema binding accepts separately legal exact-copy alias locks');
          Failed := False;
          try D.RequireReplay;
          except on E: EWfcArtifactReplayInvocation do Failed := Pos('conflict', E.Message) > 0; end;
          Check(Failed, 'replay classifies incompatible alias locks as invocation failure, not mismatch');
        finally D.Free; end;
      finally S.Free; end;
    finally U.Free; end;
  finally P.Free; end;
end;

procedure TestSchemaNotLearning;
var T: TWfcTrainingDocument; P: TWfcPipelineModel; D: TWfcArtifactDocument;
  Failed: Boolean; Text: String;
begin
  T := Training(wtkPattern2D, False, True);
  try
    Text := EncodeWfcTrainingText(T);
    D := TWfcArtifactDocument.Create(wakTraining, Text, '', '');
    try
      Check(D.CanonicalText = Text, 'valid open-pattern source is accepted without recipe learning');
      Check(Pos('scope=authoring-schema', D.Summary) > 0, 'training summary states authoring validation scope');
      P := nil; Failed := False;
      try
        try P := LearnWfcTrainingRecipe(D.Training);
        except on E: EWfcTraining do Failed := True; end;
      finally P.Free; end;
      Check(Failed, 'the same valid authoring document has unsupported recipe lowering');
    finally D.Free; end;
  finally T.Free; end;
  T := Training(wtkAdjacency3D);
  try
    Text := EncodeWfcTrainingText(T);
    D := TWfcArtifactDocument.Create(wakTraining, Text, '', '');
    try Check(Pos('wfclearn=2'#10, D.CanonicalText) = 1, 'quota-free volume source retains original version2'); finally D.Free; end;
  finally T.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestBrowserKinds;
var K: TWfcArtifactKind; I, N: Integer; Text: String; D: TWfcArtifactDocument; Failed: Boolean;
begin
  for I := 0 to 8 do
  begin
    case I of
      0: asm K = NaN; end; 1: asm K = Infinity; end; 2: asm K = -1; end;
      3: asm K = 9; end; 4: asm K = 0.5; end; 5: asm K = '0'; end;
      6: asm K = undefined; end; 7: asm K = null; end; 8: asm K = 4294967296; end;
    end;
    Failed := False; Text := '';
    try Text := WfcArtifactKindName(K); except on E: EWfcArtifactInvalid do Failed := True; end;
    Check(Failed and (Text = ''), 'browser malformed kind name rejected');
    Failed := False; N := 0;
    try N := WfcArtifactInputLimit(K); except on E: EWfcArtifactInvalid do Failed := True; end;
    Check(Failed and (N = 0), 'browser malformed kind cap rejected');
    Failed := False; D := nil;
    try
      try D := TWfcArtifactDocument.Create(K, Texts[wakRules], '', '');
      except on E: EWfcArtifactInvalid do Failed := True; end;
    finally D.Free; end;
    Check(Failed, 'browser malformed constructor kind rejected before allocation');
  end;
end;
{$ENDIF}

begin
  RunTest('small canonical fixtures', @MakeFixtures);
  RunTest('nine closed families and borrowed typed views', @TestFamilies);
  RunTest('strict canonical input and mandatory binding', @TestStrictnessAndContexts);
  RunTest('existing per-role codec caps', @TestLimits);
  RunTest('complete solved/non-solved replay and canonical forgeries', @TestReplay);
  RunTest('schema binding versus executable alias-lock preparation', @TestReplayInvocation);
  RunTest('authoring schema does not execute learning', @TestSchemaNotLearning);
  {$IFDEF PAS2JS}RunTest('browser malformed family ordinals', @TestBrowserKinds);{$ENDIF}
  WriteLn('[SUMMARY] checks=', Checks, ' failures=', Failures);
  if Failures <> 0 then Halt(1);
end.
