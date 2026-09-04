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
program wfc_text_passes_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_sequence,
  wfc_sequence_learn,
  wfc_sequence_graph,
  wfc_text_passes,
  wfc_trace;

type
  TTestProcedure = procedure;

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GCheckCount);
  if ACondition then
    WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailureCount);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try
    ATest;
  except
    on E: Exception do
    begin
      Inc(GFailureCount);
      WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function TokensOf(const AValues: array of TWfcModelToken): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
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

function TokensMatch(const A, B: TWfcModelTokens): Boolean;
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

function StatesMatch(const A, B: TWfcSequenceStateIndices): Boolean;
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

function OneToken(const AToken: TWfcModelToken): TWfcModelTokens;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0] := AToken;
end;

function FragmentTokensOf(
  const AValues: array of TWfcModelToken): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := EncodeWfcTextPassFragment(AValues[I]);
end;

function StructureSourcesForLexical(
  const AToken: TWfcModelToken): TWfcModelTokens;
begin
  if (AToken = 'the') or (AToken = 'a') then
    Result := OneToken('DET')
  else if (AToken = 'quiet') or (AToken = 'quick') or
      (AToken = 'calm') then
    Result := OneToken('ADJ')
  else if (AToken = 'owl') or (AToken = 'fox') or
      (AToken = 'sun') or (AToken = 'sea') then
    Result := OneToken('NOUN')
  else if (AToken = 'rests') or (AToken = 'rises') or
      (AToken = 'settles') then
    Result := OneToken('VERB')
  else if (AToken = 'quietly') or (AToken = 'brightly') then
    Result := OneToken('ADV')
  else if (AToken = 'dot') or (AToken = 'period') or
      (AToken = 'bang') then
    Result := OneToken('STOP')
  else
    raise Exception.Create('unmapped lexical fixture token');
end;

function LexicalSourcesForPunctuation(
  const AToken: TWfcModelToken): TWfcModelTokens;
var
  LFragment: TWfcModelToken;
begin
  LFragment := DecodeWfcTextPassFragment(AToken);
  if LFragment = 'The' then
    Result := OneToken('the')
  else if LFragment = 'A' then
    Result := OneToken('a')
  else if LFragment = ' quiet' then
    Result := OneToken('quiet')
  else if LFragment = ' quick' then
    Result := OneToken('quick')
  else if LFragment = ' calm' then
    Result := OneToken('calm')
  else if LFragment = ' owl' then
    Result := OneToken('owl')
  else if LFragment = ' fox' then
    Result := OneToken('fox')
  else if LFragment = ' sun' then
    Result := OneToken('sun')
  else if LFragment = ' sea' then
    Result := OneToken('sea')
  else if LFragment = ' rests' then
    Result := OneToken('rests')
  else if LFragment = ' rises' then
    Result := OneToken('rises')
  else if LFragment = ' settles' then
    Result := OneToken('settles')
  else if LFragment = ' quietly' then
    Result := OneToken('quietly')
  else if LFragment = ' brightly' then
    Result := OneToken('brightly')
  else if LFragment = '.' then
    Result := TokensOf(['dot', 'period'])
  else if LFragment = '!' then
    Result := OneToken('bang')
  else
    raise Exception.Create('unmapped punctuation fixture token');
end;

function StructureSourcesForPunctuation(
  const AToken: TWfcModelToken): TWfcModelTokens;
var
  LFragment: TWfcModelToken;
begin
  LFragment := DecodeWfcTextPassFragment(AToken);
  if (LFragment = 'The') or (LFragment = 'A') then
    Result := OneToken('DET')
  else if (LFragment = ' quiet') or (LFragment = ' quick') or
      (LFragment = ' calm') then
    Result := OneToken('ADJ')
  else if (LFragment = ' owl') or (LFragment = ' fox') or
      (LFragment = ' sun') or (LFragment = ' sea') then
    Result := OneToken('NOUN')
  else if (LFragment = ' rests') or (LFragment = ' rises') or
      (LFragment = ' settles') then
    Result := OneToken('VERB')
  else if (LFragment = ' quietly') or (LFragment = ' brightly') then
    Result := OneToken('ADV')
  else if (LFragment = '.') or (LFragment = '!') then
    Result := OneToken('STOP')
  else
    raise Exception.Create('unmapped punctuation structure token');
end;

function BuildLexicalStructureRules(const AModel: TWfcSequenceModel):
  TWfcSequenceProjectionRules;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, AModel.PublicTokenCount);
  for I := 0 to AModel.PublicTokenCount - 1 do
    Result[I] := MakeWfcSequenceProjectionRule(AModel.PublicTokenAt(I),
      StructureSourcesForLexical(AModel.PublicTokenAt(I)));
end;

function BuildPunctuationLexicalRules(const AModel: TWfcSequenceModel):
  TWfcSequenceProjectionRules;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, AModel.PublicTokenCount);
  for I := 0 to AModel.PublicTokenCount - 1 do
    Result[I] := MakeWfcSequenceProjectionRule(AModel.PublicTokenAt(I),
      LexicalSourcesForPunctuation(AModel.PublicTokenAt(I)));
end;

function BuildPunctuationStructureRules(const AModel: TWfcSequenceModel):
  TWfcSequenceProjectionRules;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, AModel.PublicTokenCount);
  for I := 0 to AModel.PublicTokenCount - 1 do
    Result[I] := MakeWfcSequenceProjectionRule(AModel.PublicTokenAt(I),
      StructureSourcesForPunctuation(AModel.PublicTokenAt(I)));
end;

procedure BuildFixtureModels(out AStructure, ALexical,
  APunctuation: TWfcSequenceModel);
begin
  AStructure := nil;
  ALexical := nil;
  APunctuation := nil;
  try
    AStructure := LearnSequenceModelCorpus(SamplesOf([
      MakeWfcSequenceSample(TokensOf([
        'DET', 'ADJ', 'NOUN', 'VERB', 'STOP'])),
      MakeWfcSequenceSample(TokensOf([
        'DET', 'NOUN', 'VERB', 'ADV', 'STOP']))
      ]), 2);
    ALexical := LearnSequenceModelCorpus(SamplesOf([
      MakeWfcSequenceSample(TokensOf([
        'the', 'quiet', 'owl', 'rests', 'dot'])),
      MakeWfcSequenceSample(TokensOf([
        'the', 'quick', 'fox', 'rests', 'bang'])),
      MakeWfcSequenceSample(TokensOf([
        'the', 'fox', 'rests', 'quietly', 'dot'])),
      MakeWfcSequenceSample(TokensOf([
        'a', 'sun', 'rises', 'brightly', 'bang'])),
      MakeWfcSequenceSample(TokensOf([
        'a', 'calm', 'sea', 'settles', 'period']))
      ]), 2);
    APunctuation := LearnSequenceModelCorpus(SamplesOf([
      MakeWfcSequenceSample(FragmentTokensOf([
        'The', ' quiet', ' owl', ' rests', '.'])),
      MakeWfcSequenceSample(FragmentTokensOf([
        'The', ' quick', ' fox', ' rests', '!'])),
      MakeWfcSequenceSample(FragmentTokensOf([
        'The', ' fox', ' rests', ' quietly', '.'])),
      MakeWfcSequenceSample(FragmentTokensOf([
        'A', ' sun', ' rises', ' brightly', '!'])),
      MakeWfcSequenceSample(FragmentTokensOf([
        'A', ' calm', ' sea', ' settles', '.']))
      ]), 2);
  except
    APunctuation.Free;
    ALexical.Free;
    AStructure.Free;
    APunctuation := nil;
    ALexical := nil;
    AStructure := nil;
    raise;
  end;
end;

function BuildConfig(const AStructure, ALexical,
  APunctuation: TWfcSequenceModel): TWfcTextPassConfig;
begin
  Result := DefaultWfcTextPassConfig(5, wseWhole, 0);
  Result.Models.Structure := AStructure;
  Result.Models.Lexical := ALexical;
  Result.Models.Punctuation := APunctuation;
  Result.Maps.LexicalFromStructure :=
    BuildLexicalStructureRules(ALexical);
  Result.Maps.PunctuationFromLexical :=
    BuildPunctuationLexicalRules(APunctuation);
  Result.Maps.PunctuationFromStructure :=
    BuildPunctuationStructureRules(APunctuation);
end;

function IsPublicResult(const AResult: TWfcTextPassResult): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(AResult.Structure.Tokens) - 1 do
    if (Pos('@wfcs', String(AResult.Structure.Tokens[I])) > 0) or
        (Pos('@wfcs', String(AResult.Lexical.Tokens[I])) > 0) or
        (Pos('@wfcs', String(AResult.Punctuation.Tokens[I])) > 0) then
      Exit(False);
  Result := True;
end;

function IsPublicTrace(const ATrace: TWfcTextPassTraceEvents): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(ATrace) - 1 do
    if Pos('@wfcs', String(ATrace[I].Token)) > 0 then
      Exit(False);
  Result := True;
end;

procedure TestThreeOwnedPassesAndReplay;
var
  I: Integer;
  LBadPunctuation: TWfcSequenceModel;
  LConfig: TWfcTextPassConfig;
  LErrorMessage: String;
  LLexical: TWfcSequenceModel;
  LLayer: TWfcTextPassLayer;
  LOptions: TGraphSolveOptions;
  LPipeline: TWfcTextPassPipeline;
  LPunctuation: TWfcSequenceModel;
  LReplay: TWfcTextPassResult;
  LReport: TWfcTextPassReport;
  LResult: TWfcTextPassResult;
  LStructure: TWfcSequenceModel;
  LStrippedTraceValidation: TGraphTraceValidationReport;
  LTraceHash: TGraphTraceSignature;
  LValidation: TWfcTextPassValidationReport;
begin
  BuildFixtureModels(LStructure, LLexical, LPunctuation);
  LBadPunctuation := nil;
  LPipeline := nil;
  try
    LBadPunctuation := LearnSequenceModel(TokensOf(['plain']), 1);
    LConfig := BuildConfig(LStructure, LLexical, LPunctuation);
    LConfig.TokenLength := 1;
    LConfig.Models.Punctuation := LBadPunctuation;
    SetLength(LConfig.Maps.PunctuationFromLexical, 1);
    LConfig.Maps.PunctuationFromLexical[0] :=
      MakeWfcSequenceProjectionRule('plain', OneToken('the'));
    SetLength(LConfig.Maps.PunctuationFromStructure, 1);
    LConfig.Maps.PunctuationFromStructure[0] :=
      MakeWfcSequenceProjectionRule('plain', OneToken('DET'));
    LErrorMessage := '';
    try
      LPipeline := TWfcTextPassPipeline.Create(LConfig);
    except
      on E: Exception do
        LErrorMessage := E.Message;
    end;
    Check((not Assigned(LPipeline)) and
      (Pos('canonical text fragment', LErrorMessage) > 0),
      'the owner rejects an unversioned punctuation vocabulary up front');
    if Assigned(LPipeline) then
      FreeAndNil(LPipeline);
    LBadPunctuation.Free;
    LBadPunctuation := nil;

    LConfig := BuildConfig(LStructure, LLexical, LPunctuation);
    Check(Length(LConfig.Maps.PunctuationFromLexical[
      LPunctuation.FindPublicToken(
        EncodeWfcTextPassFragment('.'))].SourceTokens) = 2,
      'one punctuation target retains two lexical OR alternatives');
    LPipeline := TWfcTextPassPipeline.Create(LConfig);

    Check((LPipeline.Graph.TotalPassCount = 3) and
      (LPipeline.LayerGraph[wtplStructure].CurrentPass = 'structure') and
      (LPipeline.LayerGraph[wtplLexical].CurrentPass = 'lexical') and
      (LPipeline.LayerGraph[wtplPunctuation].CurrentPass = 'punctuation'),
      'the owner creates three stable, named pass owners');
    Check((LPipeline.LayerGraph[wtplStructure].DependencyCount = 0) and
      (LPipeline.LayerGraph[wtplLexical].DependencyCount = 1) and
      (LPipeline.LayerGraph[wtplPunctuation].DependencyCount = 2),
      'lexical depends on structure and punctuation depends on both priors');

    LPipeline.IntersectAllowedTokens(wtplStructure, 1, 'ADJ')
      .IntersectAllowedTokens(wtplLexical, 2, 'fox')
      .IntersectAllowedTokens(wtplPunctuation, 4,
        EncodeWfcTextPassFragment('!'));
    LOptions := DefaultGraphSolveOptions;
    LOptions.CaptureTrace := True;
    Check(LPipeline.TryGenerate(LOptions, LResult, LReport),
      'the constrained three-pass text pipeline solves');
    Check(LResult.Text = 'The quick fox rests!',
      'structure, word choice, and punctuation render the expected sentence');
    Check(TokensMatch(LResult.Structure.Tokens,
        TokensOf(['DET', 'ADJ', 'NOUN', 'VERB', 'STOP'])) and
      TokensMatch(LResult.Lexical.Tokens,
        TokensOf(['the', 'quick', 'fox', 'rests', 'bang'])) and
      TokensMatch(LResult.Punctuation.Tokens,
        FragmentTokensOf(['The', ' quick', ' fox', ' rests', '!'])),
      'each pass exposes its own public sequence');
    Check(IsPublicResult(LResult),
      'captured pass results contain no private latent graph keys');
    Check(LReport.Validation.Valid and
      (LReport.Validation.CheckedLayers = 3) and
      (LReport.Validation.CheckedTokens = 15) and
      (LReport.Validation.CheckedRelations = 15),
      'independent validation checks every path, projection, and relation');
    Check(LReport.TraceCaptured and LReport.TraceValidation.Valid and
      (Length(LReport.Trace) > 0) and IsPublicTrace(LReport.Trace) and
      (not LReport.Solve.TraceCaptured) and
      (Length(LReport.Solve.Trace) = 0),
      'the owner validates and projects causal events without private keys');
    Check(ValidateGraphTrace(LPipeline.Graph, LReport.Solve,
        LStrippedTraceValidation),
      'the stripped generic solve report retains capture-disabled invariants');
    LTraceHash := LReport.TraceHash;

    Check(LPipeline.TryGenerate(LOptions, LReplay, LReport) and
      (LReplay.Text = LResult.Text) and
      StatesMatch(LReplay.Structure.StateIndices,
        LResult.Structure.StateIndices) and
      StatesMatch(LReplay.Lexical.StateIndices,
        LResult.Lexical.StateIndices) and
      StatesMatch(LReplay.Punctuation.StateIndices,
        LResult.Punctuation.StateIndices) and
      (LReport.TraceHash = LTraceHash),
      'the same seed and constraints replay all three latent paths exactly');

    LPipeline.Seed := $DEADBEEF;
    Check(LPipeline.TryRegenerateFrom(wtplLexical, LOptions,
        LReplay, LReport) and
      (LReport.Solve.Passes[Ord(wtplStructure)].Disposition = gpdReused) and
      LReport.Solve.Passes[Ord(wtplLexical)].Executed and
      LReport.Solve.Passes[Ord(wtplPunctuation)].Executed and
      (LReplay.Text = LResult.Text),
      'selective lexical regeneration reuses structure and refreshes descendants');

    Check(LPipeline.TryRegenerateFrom(wtplPunctuation, LOptions,
        LReplay, LReport) and
      (LReport.Solve.Passes[Ord(wtplStructure)].Disposition = gpdReused) and
      (LReport.Solve.Passes[Ord(wtplLexical)].Disposition = gpdReused) and
      LReport.Solve.Passes[Ord(wtplPunctuation)].Executed,
      'selective punctuation regeneration reuses both providers');
    Check(LPipeline.TryRegenerateFrom(wtplStructure, LOptions,
        LReplay, LReport) and
      LReport.Solve.Passes[Ord(wtplStructure)].Executed and
      LReport.Solve.Passes[Ord(wtplLexical)].Executed and
      LReport.Solve.Passes[Ord(wtplPunctuation)].Executed,
      'selective structure regeneration refreshes the complete closure');

    for LLayer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
      for I := 0 to 4 do
        LPipeline.ClearAllowedTokens(LLayer, I);
    LPipeline.Seed := 0;
    Check(LPipeline.TryGenerate(LOptions, LReplay, LReport) and
      (LReplay.Text = 'A sun rises brightly!'),
      'clearing every public constraint restores sequence endpoint domains');
    LPipeline.IntersectAllowedTokens(wtplStructure, 1, 'ADJ');
    Check(LPipeline.TryRegenerateFrom(wtplPunctuation, LOptions,
        LReplay, LReport) and
      LReport.Solve.Passes[Ord(wtplStructure)].Executed and
      LReport.Solve.Passes[Ord(wtplLexical)].Executed and
      LReport.Solve.Passes[Ord(wtplPunctuation)].Executed and
      (LReplay.Structure.Tokens[1] = 'ADJ'),
      'an upstream constraint edit widens downstream regeneration to its dirty root');
    LPipeline.ClearAllowedTokens(wtplStructure, 1);
    Check(LPipeline.TryRegenerateFrom(wtplPunctuation, LOptions,
        LReplay, LReport) and
      LReport.Solve.Passes[Ord(wtplStructure)].Executed and
      LReport.Solve.Passes[Ord(wtplLexical)].Executed and
      LReport.Solve.Passes[Ord(wtplPunctuation)].Executed,
      'clearing an upstream constraint also refreshes its complete closure');
    LPipeline.IntersectAllowedTokens(wtplLexical, 1, 'calm');
    Check((not LPipeline.Validate(LReplay, LValidation)) and
      (LValidation.Issue.Kind = wtpvikCallerConstraint) and
      (LValidation.Issue.Layer = wtplLexical) and
      (LValidation.Issue.Position = 1),
      'independent validation rejects a coherent result after constraints change');
    LPipeline.ClearAllowedTokens(wtplLexical, 1);
  finally
    LPipeline.Free;
    LBadPunctuation.Free;
    LPunctuation.Free;
    LLexical.Free;
    LStructure.Free;
  end;
end;

procedure TestContradictionIsTransactional;
var
  LBaseline: TWfcTextPassResult;
  LCaptured: TWfcGeneratedSequence;
  LCaptureReport: TWfcSequenceGraphValidationReport;
  LConfig: TWfcTextPassConfig;
  LFailed: TWfcTextPassResult;
  LLexical: TWfcSequenceModel;
  LPipeline: TWfcTextPassPipeline;
  LPunctuation: TWfcSequenceModel;
  LRecovered: TWfcTextPassResult;
  LReport: TWfcTextPassReport;
  LStructure: TWfcSequenceModel;
begin
  BuildFixtureModels(LStructure, LLexical, LPunctuation);
  LPipeline := nil;
  try
    LConfig := BuildConfig(LStructure, LLexical, LPunctuation);
    LPipeline := TWfcTextPassPipeline.Create(LConfig);
    Check(LPipeline.TryGenerate(LBaseline, LReport),
      'an unconstrained baseline solves before the conflict');

    LPipeline.IntersectAllowedTokens(wtplStructure, 1, 'ADJ');
    LPipeline.IntersectAllowedTokens(wtplLexical, 1, 'fox');
    Check((not LPipeline.TryRegenerateFrom(wtplPunctuation,
        LFailed, LReport)) and
      (LReport.Status = wtpsSolveFailed) and
      (LReport.Solve.FailedPassIndex = Ord(wtplLexical)) and
      (LReport.FailedLayer = wtplLexical) and
      LReport.Solve.Passes[Ord(wtplStructure)].Executed and
      (Length(LFailed.Structure.Tokens) = 0) and
      (LFailed.Text = ''),
      'a downstream request widens to the dirty structure root before failing');
    Check(CaptureSolvedSequence(LPunctuation,
        LPipeline.LayerGraph[wtplPunctuation], wseWhole,
        LCaptured, LCaptureReport) and
      TokensMatch(LCaptured.Tokens, LBaseline.Punctuation.Tokens),
      'a downstream contradiction leaves the committed surface untouched');

    LPipeline.ClearAllowedTokens(wtplLexical, 1);
    Check(LPipeline.TryRegenerateFrom(wtplPunctuation,
        LRecovered, LReport) and
      LReport.Solve.Passes[Ord(wtplStructure)].Executed and
      LReport.Solve.Passes[Ord(wtplLexical)].Executed and
      LReport.Solve.Passes[Ord(wtplPunctuation)].Executed and
      (LRecovered.Structure.Tokens[1] = 'ADJ'),
      'a failed solve retains the earliest dirty root through a later edit');

    LPipeline.ClearAllowedTokens(wtplStructure, 1);
    Check(LPipeline.TryGenerate(LRecovered, LReport) and
      (LRecovered.Text = LBaseline.Text) and
      StatesMatch(LRecovered.Structure.StateIndices,
        LBaseline.Structure.StateIndices) and
      StatesMatch(LRecovered.Lexical.StateIndices,
        LBaseline.Lexical.StateIndices) and
      StatesMatch(LRecovered.Punctuation.StateIndices,
        LBaseline.Punctuation.StateIndices),
      'clearing the conflict restores the exact same-seed composition');

    LPipeline.IntersectAllowedTokens(wtplLexical, 4, 'bang');
    LPipeline.IntersectAllowedTokens(wtplPunctuation, 4,
      EncodeWfcTextPassFragment('.'));
    Check((not LPipeline.TryGenerate(LFailed, LReport)) and
      (LReport.Status = wtpsSolveFailed) and
      (LReport.Solve.FailedPassIndex = Ord(wtplPunctuation)) and
      (LReport.FailedLayer = wtplPunctuation),
      'a surface conflict reports punctuation as the failed owner');
    LPipeline.ClearAllowedTokens(wtplLexical, 4);
    LPipeline.ClearAllowedTokens(wtplPunctuation, 4);
    Check(LPipeline.TryGenerate(LRecovered, LReport) and
      (LRecovered.Text = LBaseline.Text),
      'clearing a punctuation conflict recovers the baseline transaction');
  finally
    LPipeline.Free;
    LPunctuation.Free;
    LLexical.Free;
    LStructure.Free;
  end;
end;

procedure TestAtomicConfigurationAndIndependentTamperChecks;
var
  I: Integer;
  LConfig: TWfcTextPassConfig;
  LErrorMessage: String;
  LLexical: TWfcSequenceModel;
  LPipeline: TWfcTextPassPipeline;
  LPunctuation: TWfcSequenceModel;
  LReport: TWfcTextPassReport;
  LResult: TWfcTextPassResult;
  LStructure: TWfcSequenceModel;
  LTampered: TWfcTextPassResult;
  LValidation: TWfcTextPassValidationReport;
begin
  BuildFixtureModels(LStructure, LLexical, LPunctuation);
  LPipeline := nil;
  try
    LConfig := BuildConfig(LStructure, LLexical, LPunctuation);
    LConfig.Maps.PunctuationFromStructure[0].SourceTokens[0] := 'missing';
    LErrorMessage := '';
    try
      LPipeline := TWfcTextPassPipeline.Create(LConfig);
    except
      on E: Exception do
        LErrorMessage := E.Message;
    end;
    Check((not Assigned(LPipeline)) and (LErrorMessage <> '') and
      (Pos('@wfcs', LErrorMessage) = 0),
      'a malformed final map rejects the whole owner without latent leakage');

    LConfig := BuildConfig(LStructure, LLexical, LPunctuation);
    LPipeline := TWfcTextPassPipeline.Create(LConfig);
    LPipeline.IntersectAllowedTokens(wtplStructure, 1, 'ADJ')
      .IntersectAllowedTokens(wtplLexical, 2, 'fox')
      .IntersectAllowedTokens(wtplPunctuation, 4,
        EncodeWfcTextPassFragment('!'));
    Check(LPipeline.TryGenerate(LResult, LReport),
      'the valid owner solves for tamper validation');

    LTampered := LResult;
    LTampered.Text := LTampered.Text + '?';
    Check((not LPipeline.Validate(LTampered, LValidation)) and
      (LValidation.Issue.Kind = wtpvikRendering),
      'independent validation rejects changed rendered text');

    LPipeline.ClearAllowedTokens(wtplPunctuation, 4);
    LTampered := LResult;
    for I := 0 to LPunctuation.StateCount - 1 do
      if LPunctuation.ProjectStateToken(I) =
          EncodeWfcTextPassFragment('.') then
        if (LPunctuation.HistorySize = 1) and
            (LPunctuation.HistoryItemAt(I, 0).Kind = wshToken) and
            (LPunctuation.PublicTokenAt(
              LPunctuation.HistoryItemAt(I, 0).TokenIndex) =
                EncodeWfcTextPassFragment(' rests')) then
        begin
          LTampered.Punctuation.StateIndices[4] := I;
          LTampered.Punctuation.Tokens[4] :=
            EncodeWfcTextPassFragment('.');
          Break;
        end;
    LTampered.Text := 'The quick fox rests.';
    Check((not LPipeline.Validate(LTampered, LValidation)) and
      (LValidation.Issue.Kind = wtpvikPassProjection) and
      (LValidation.Issue.Relation = wtprPunctuationFromLexical) and
      (LValidation.Issue.Position = 4),
      'a locally valid surface path cannot violate its lexical dependency');
  finally
    LPipeline.Free;
    LPunctuation.Free;
    LLexical.Free;
    LStructure.Free;
  end;
end;

procedure TestFragmentCodec;
var
  LErrorMessage: String;
  LFragments: TWfcModelTokens;
  LToken: TWfcModelToken;
begin
  LToken := EncodeWfcTextPassFragment('');
  Check((WFC_TEXT_PASS_FRAGMENT_VERSION = 1) and
    WfcModelTokenIsValid(LToken) and
    (DecodeWfcTextPassFragment(LToken) = ''),
    'the versioned fragment codec represents an empty visible slot');
  LFragments := TokensOf([
    EncodeWfcTextPassFragment(''),
    EncodeWfcTextPassFragment(' leading'),
    EncodeWfcTextPassFragment('.'),
    EncodeWfcTextPassFragment('')]);
  Check(RenderWfcTextPassFragments(LFragments) = ' leading.',
    'fragment rendering preserves empty slots, spaces, and punctuation');

  LErrorMessage := '';
  try
    DecodeWfcTextPassFragment('@wfctf1:%2f');
  except
    on E: Exception do
      LErrorMessage := E.Message;
  end;
  Check(LErrorMessage <> '',
    'the fragment decoder rejects noncanonical lowercase escapes');

  LErrorMessage := '';
  try
    DecodeWfcTextPassFragment('plain text');
  except
    on E: Exception do
      LErrorMessage := E.Message;
  end;
  Check(LErrorMessage <> '',
    'the fragment decoder rejects unversioned surface tokens');
end;

begin
  WriteLn('WFC text pass conformance suite');
  WriteLn('===============================');
  RunTest('three owned passes and replay', @TestThreeOwnedPassesAndReplay);
  RunTest('transactional contradiction', @TestContradictionIsTransactional);
  RunTest('configuration and tamper validation',
    @TestAtomicConfigurationAndIndependentTamperChecks);
  RunTest('versioned fragment codec', @TestFragmentCodec);
  WriteLn('===============================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d text pass checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
