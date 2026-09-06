{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_artifact_inspect_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host, JS,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_rule_model, wfc_rule_text,
  wfc_training, wfc_training_text, wfc_pipeline_model, wfc_pipeline_text,
  wfc_pipeline_run, wfc_pipeline_run_text, wfc_pipeline_result,
  wfc_pipeline_result_text, wfc_pipeline_runtime, wfc_text_codec,
  wfc_artifact_document, wfc_artifact_inspect;

var Checks: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;

function Tokens(const AValues: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

procedure CheckSafeReport(const S: String);
var I: Integer;
begin
  Check((Length(S) > 0) and (S[Length(S)] = #10), 'final LF');
  Check(Length(S) <= WFC_ARTIFACT_INSPECT_MAX_REPORT_LENGTH, 'bounded bytes');
  for I := 1 to Length(S) do
    if not ((S[I] = #10) or ((Ord(S[I]) >= 32) and (Ord(S[I]) <= 126))) then
      raise Exception.Create('report contains unsafe byte');
  Check(Pos('execution=not-run'#10, S) > 0, 'no execution claim');
end;

procedure TestRulesAndBudgets;
var R: TWfcRuleModel; D: TWfcArtifactDocument; W: TWfcModelIntegerArray;
  S, T: String; I: Integer; Failed: Boolean;
begin
  SetLength(W, 1); W[0] := 1;
  R := TWfcRuleModel.Create(1, Tokens(['only']), W, nil);
  try S := EncodeWfcRuleText(R); finally R.Free; end;
  D := TWfcArtifactDocument.Create(wakRules, S, '', '');
  try
    for I := 0 to 3 do
    begin
      T := WfcInspectArtifact(D, I);
      CheckSafeReport(T);
      if I < 2 then
        Check(Pos('details-shown=' + IntToStr(I) + #10 +
          'truncated=true'#10 + 'truncation=record-limit'#10, T) > 0, 'exact limited prefix')
      else
        Check(Pos('details-shown=2'#10 + 'truncated=false'#10 +
          'truncation=none'#10, T) > 0, 'exact boundary is not falsely truncated');
    end;
    Check(Pos('rules rank=1 values=1 rows=0 absent-row=wildcard'#10 +
      'value index=0 token=only weight=1'#10, T) > 0, 'rule semantics and ordered vocabulary');
    Check(WfcInspectArtifact(D, 3) = T, 'repeated report exact');
    T[1] := 'X';
    Check(D.CanonicalText = S, 'report is detached from artifact');
    Failed := False;
    try T := WfcInspectArtifact(D, -1); except on EWfcArtifactInspect do Failed := True; end;
    Check(Failed, 'negative API limit rejected');
    CheckSafeReport(WfcInspectArtifact(D, High(Integer)));
    {$IFDEF PAS2JS}
    asm I = NaN; end;
    Failed := False;
    try T := WfcInspectArtifact(D, I); except on EWfcArtifactInspect do Failed := True; end;
    Check(Failed, 'NaN API limit rejected');
    asm I = Infinity; end;
    Failed := False;
    try T := WfcInspectArtifact(D, I); except on EWfcArtifactInspect do Failed := True; end;
    Check(Failed, 'infinite API limit rejected');
    asm I = 0.5; end;
    Failed := False;
    try T := WfcInspectArtifact(D, I); except on EWfcArtifactInspect do Failed := True; end;
    Check(Failed, 'fractional API limit rejected');
    {$ENDIF}
  finally D.Free; end;
  Failed := False;
  try T := WfcInspectArtifact(nil, 0); except on EWfcArtifactInspect do Failed := True; end;
  Check(Failed, 'nil API document rejected');
end;

function Source(const AKind: TWfcTrainingKind; const APolicy: Boolean): TWfcTrainingDocument;
var S: TWfcTrainingSamples; O: TWfcTrainingOptions; Q: TWfcTrainingValueQuotas;
  C: TWfcTrainingConnectivities; V: TWfcTrainingConnectivityValues;
  P: TGraphPosition; LToken: TWfcModelToken;
begin
  LToken := WfcTextDecodeToken('road%20%1B%0A%3D%F0%9F%8C%B1', 'fixture');
  { Inspection accepts portable Unicode even when the native graph adapter
    cannot represent it in the host code page. The separately executed
    policy fixture uses an ASCII vocabulary on every target. }
  if APolicy then LToken := 'road';
  SetLength(S, 1);
  S[0] := MakeWfcTrainingSample('sample', 1, 1, 1, Tokens([LToken]));
  O := MakeWfcTrainingOptions(AKind, wmbWrap, wmsNone, 0, 0, 0);
  if AKind = wtkPattern2D then begin O.PatternWidth := 1; O.PatternHeight := 1; end;
  if AKind = wtkSequence then begin O.Order := 1; O.Boundary := wmbOpen; end;
  if APolicy then
  begin
    SetLength(Q, 1);
    Q[0] := MakeWfcTrainingValueQuota('quantity', Tokens([LToken]), 1, 1);
    SetLength(C, 1); SetLength(V, 1);
    V[0] := MakeWfcTrainingConnectivityValue(LToken, [gdEast, gdWest], True);
    P := Default(TGraphPosition);
    C[0] := MakeWfcTrainingConnectivity('rooted', P, nil, V, True);
  end;
  Result := TWfcTrainingDocument.Create(MakeWfcTrainingMetadata('inspect fixture', 'MIT',
    WfcTextDecodeToken('name%0Aforged%3Drecord', 'fixture')), O, S, Q, C);
end;

procedure TestFamilies;
var K: TWfcTrainingKind; A: TWfcArtifactKind; T: TWfcTrainingDocument;
  D: TWfcArtifactDocument; P: TWfcPipelineModel; R: TWfcPipelineRun;
  V: TWfcPipelineResult; S, RecipeText, RunText, ResultText: String;
begin
  for K := Low(TWfcTrainingKind) to High(TWfcTrainingKind) do
  begin
    T := Source(K, False);
    try
      case K of wtkPattern2D: A := wakPattern2D; wtkSequence: A := wakSequence;
      else A := wakModel; end;
      D := TWfcArtifactDocument.Create(A, LearnWfcTrainingModelText(T), '', '');
      try
        S := WfcInspectArtifact(D, 1000); CheckSafeReport(S);
        Check(Pos('road%20%1B%0A%3D%F0%9F%8C%B1', S) > 0, 'portable escaped Unicode token');
        Check(Pos('truncated=false'#10, S) > 0, 'complete small learned artifact');
        if K = wtkPattern2D then Check(Pos('pattern-cell pattern=0 xy=0,0 palette=0'#10, S) > 0, 'latent footprint exposed')
        else if K = wtkSequence then Check(Pos('state index=0 emitted=0 count=1 starts=1 ends=1'#10, S) > 0, 'sequence state counts exposed')
        else Check(Pos('observation direction=E source=0 target=0 count=1'#10, S) > 0, 'actual observed edge count');
      finally D.Free; end;
    finally T.Free; end;
  end;
  T := Source(wtkAdjacency1D, True);
  try
    D := TWfcArtifactDocument.Create(wakTraining, EncodeWfcTrainingText(T), '', '');
    try
      S := WfcInspectArtifact(D, 1000); CheckSafeReport(S);
      Check(Pos('source=name%0Aforged%3Drecord', S) > 0, 'metadata cannot inject report records');
      Check(Pos('profile network=0 index=0 token=road ports=EW required=true'#10, S) > 0, 'training policy ports preserved');
      Check(Pos('quota index=0 label=quantity minimum=1 maximum=1 tokens=1'#10, S) > 0, 'source quantities exposed');
    finally D.Free; end;
    P := LearnWfcTrainingRecipe(T);
    try
      RecipeText := EncodeWfcPipelineModelText(P);
      D := TWfcArtifactDocument.Create(wakRecipe, RecipeText, '', '');
      try
        S := WfcInspectArtifact(D, 1000); CheckSafeReport(S);
        Check(Pos('network index=0 pass=0 label=rooted root=0,0,0', S) > 0, 'recipe networks identify public owner');
        Check(Pos('resource index=0', S) > 0, 'recipe resource inventory');
      finally D.Free; end;
      R := TWfcPipelineRun.Create(P, 1, 1, 1, 4294967295, wpssOneWay, 16, 0, False, nil, nil);
      try
        RunText := EncodeWfcPipelineRunText(R);
        D := TWfcArtifactDocument.Create(wakRun, RunText, RecipeText, '');
        try
          S := WfcInspectArtifact(D, 1000); CheckSafeReport(S);
          Check(Pos('seed=4294967295', S) > 0, 'full unsigned seed portable');
          Check(Pos('runtime-preflight=not-run', S) > 0, 'run scope stated');
        finally D.Free; end;
        V := ExecuteWfcPipeline(P, R);
        try ResultText := EncodeWfcPipelineResultText(V); finally V.Free; end;
        D := TWfcArtifactDocument.Create(wakResult, ResultText, RecipeText, RunText);
        try
          S := WfcInspectArtifact(D, 1000); CheckSafeReport(S);
          Check(Pos('cell layer=0 index=0 xyz=0,0,0 token=road'#10, S) > 0, 'public output cells at exact coordinate');
          Check(Pos('full-solution=not-proven', S) > 0, 'result is not a false solver proof');
          Check(Pos('evidence=claimed-not-replayed', S) > 0, 'evidence identity not authenticated');
          Check(D.CanonicalText = ResultText, 'inspection preserves source bytes');
        finally D.Free; end;
      finally R.Free; end;
    finally P.Free; end;
  finally T.Free; end;
end;

procedure TestByteBudget;
var T: TWfcTrainingDocument; S: TWfcTrainingSamples; P: TWfcPipelineModel;
  R: TWfcPipelineRun; V: TWfcPipelineResult; O: TWfcPipelinePassOutcomes;
  L: TWfcPipelineResultLayers; C: TWfcModelTokens; D: TWfcArtifactDocument;
  LargeToken: TWfcModelToken; I: Integer; Text: String;
begin
  LargeToken := StringOfChar('x', 2048);
  SetLength(S, 1); S[0] := MakeWfcTrainingSample('large', 1, 1, Tokens([LargeToken]));
  T := TWfcTrainingDocument.Create(MakeWfcTrainingMetadata('budget', 'MIT', 'authored'),
    MakeWfcTrainingOptions(wtkAdjacency1D, wmbWrap, wmsNone, 0, 0, 0), S);
  try P := LearnWfcTrainingRecipe(T); finally T.Free; end;
  try
    R := TWfcPipelineRun.Create(P, 9000, 1, 1, 0, wpssOneWay, 0, 0, False, nil, nil);
    try
      SetLength(O, 1); O[0].PassIndex := 0; O[0].Executed := True;
      O[0].ExecutionOrdinal := 0; O[0].Disposition := gpdSolved;
      SetLength(C, 9000); for I := 0 to High(C) do C[I] := LargeToken;
      SetLength(L, 1); L[0] := MakeWfcPipelineResultLayer(0, P.PassAt(0).LabelName, C);
      V := TWfcPipelineResult.Create(P, R, CurrentWfcPipelineResultVersions,
        wprsSolved, 0, wpekNone, 0, EmptyWfcPipelineFailure, O, L);
      try Text := EncodeWfcPipelineResultText(V); finally V.Free; end;
      D := TWfcArtifactDocument.Create(wakResult, Text,
        EncodeWfcPipelineModelText(P), EncodeWfcPipelineRunText(R));
      try
        Text := WfcInspectArtifact(D, High(Integer));
        CheckSafeReport(Text);
        Check(Pos('truncated=true'#10 + 'truncation=byte-limit'#10, Text) > 0, 'explicit byte budget truncation');
        Check(Pos('cell layer=0 index=8999 ', Text) = 0, 'omitted cells are not fabricated');
      finally D.Free; end;
    finally R.Free; end;
  finally P.Free; end;
end;

procedure TestCoordinatesAndDirections;
var Samples: TWfcTrainingSamples; T: TWfcTrainingDocument;
  P: TWfcPipelineModel; R: TWfcPipelineRun; V: TWfcPipelineResult;
  O: TWfcPipelinePassOutcomes; L: TWfcPipelineResultLayers;
  D: TWfcArtifactDocument; Domains: TWfcPipelineCellDomains;
  Text, RecipeText, RunText: String;
begin
  SetLength(Samples, 1);
  Samples[0] := MakeWfcTrainingSample('volume', 2, 2, 2,
    Tokens(['T0','T1','T2','T3','T4','T5','T6','T7']));
  T := TWfcTrainingDocument.Create(MakeWfcTrainingMetadata('axes', 'MIT', 'authored'),
    MakeWfcTrainingOptions(wtkAdjacency3D, wmbOpen, wmsNone, 0, 0, 0), Samples);
  try
    D := TWfcArtifactDocument.Create(wakModel, LearnWfcTrainingModelText(T), '', '');
    try
      Text := WfcInspectArtifact(D, 1000);
      Check(Pos('observation direction=E source=0 target=1 count=1'#10, Text) > 0,
        'positive X observation is east');
      Check(Pos('observation direction=W source=1 target=0 count=1'#10, Text) > 0,
        'reciprocal negative X observation is west');
      Check(Pos('observation direction=W source=0 target=1 ', Text) = 0,
        'asymmetric source/target relation not invented');
    finally D.Free; end;
    P := LearnWfcTrainingRecipe(T);
    try
      RecipeText := EncodeWfcPipelineModelText(P);
      R := TWfcPipelineRun.Create(P, 2, 2, 2, 1, wpssOneWay, 0, 0, False, nil, nil);
      try
        RunText := EncodeWfcPipelineRunText(R);
        SetLength(O, 1); O[0].PassIndex := 0; O[0].Executed := True;
        O[0].ExecutionOrdinal := 0; O[0].Disposition := gpdSolved;
        SetLength(L, 1);
        L[0] := MakeWfcPipelineResultLayer(0, P.PassAt(0).LabelName, Samples[0].Tokens);
        V := TWfcPipelineResult.Create(P, R, CurrentWfcPipelineResultVersions,
          wprsSolved, 0, wpekNone, 0, EmptyWfcPipelineFailure, O, L);
        try Text := EncodeWfcPipelineResultText(V); finally V.Free; end;
        D := TWfcArtifactDocument.Create(wakResult, Text, RecipeText, RunText);
        try
          Text := WfcInspectArtifact(D, 1000);
          Check(Pos('cell layer=0 index=1 xyz=1,0,0 token=T1'#10, Text) > 0, 'X fastest');
          Check(Pos('cell layer=0 index=2 xyz=0,1,0 token=T2'#10, Text) > 0, 'Y next');
          Check(Pos('cell layer=0 index=4 xyz=0,0,1 token=T4'#10, Text) > 0, 'Z plane offset');
          Check(Pos('cell layer=0 index=7 xyz=1,1,1 token=T7'#10, Text) > 0, 'non-origin XYZ exact');
        finally D.Free; end;
      finally R.Free; end;
      SetLength(Domains, 1);
      Domains[0] := MakeWfcPipelineCellDomain(0, 0, 0, 0, nil);
      R := TWfcPipelineRun.Create(P, 2, 2, 2, 1, wpssOneWay, 0, 0, False, nil, Domains);
      try
        RunText := EncodeWfcPipelineRunText(R);
        V := ExecuteWfcPipeline(P, R);
        try
          Check(V.Status <> wprsSolved, 'contradictory empty domain fixture');
          Text := EncodeWfcPipelineResultText(V);
        finally V.Free; end;
        D := TWfcArtifactDocument.Create(wakResult, Text, RecipeText, RunText);
        try
          Text := WfcInspectArtifact(D, 1000); CheckSafeReport(Text);
          Check(Pos('solved-public-quotas-and-connectivity=not-applicable;', Text) > 0,
            'non-solved record does not claim public output checks');
          Check(Pos('cell layer=', Text) = 0, 'non-solved inspection has no invented cells');
        finally D.Free; end;
      finally R.Free; end;
    finally P.Free; end;
  finally T.Free; end;
end;

begin
  try
    TestRulesAndBudgets;
    TestFamilies;
    TestCoordinatesAndDirections;
    TestByteBudget;
    WriteLn('Artifact inspection checks: ', Checks);
  except
    on E: Exception do begin WriteLn('FAIL: ', E.ClassName, ': ', E.Message); Halt(1); end;
  end;
end.
