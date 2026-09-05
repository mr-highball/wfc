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
program wfc_text_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_model,
  wfc_sequence,
  wfc_sequence_analyze,
  wfc_sequence_graph,
  wfc_text_complete,
  wfc_text_tokenize;

type
  TTestProcedure = procedure;
  TGraphDomains = array of TGraphValues;

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

function TokensOf(const AValues: array of TWfcModelToken):
  TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function DocumentsOf(const AValues: array of TWfcModelToken):
  TWfcTextDocuments;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function IndicesOf(const AValues: array of Integer):
  TWfcSequenceStateIndices;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function TokensMatch(const AActual: TWfcModelTokens;
  const AExpected: array of TWfcModelToken): Boolean;
var
  I: Integer;
begin
  if Length(AActual) <> Length(AExpected) then
    Exit(False);
  for I := 0 to Length(AExpected) - 1 do
    if AActual[I] <> AExpected[I] then
      Exit(False);
  Result := True;
end;

function TokenArraysMatch(const A, B: TWfcModelTokens): Boolean;
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

function GraphValuesMatch(const A, B: TGraphValues): Boolean;
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

function GraphDomainsMatch(const AGraph: TGraph;
  const AExpected: TGraphDomains): Boolean;
var
  I: Integer;
begin
  if Integer(AGraph.Dimension.Width) <> Length(AExpected) then
    Exit(False);
  for I := 0 to Length(AExpected) - 1 do
    if not GraphValuesMatch(AGraph.CopyAllowedValues(
        TGraphCoordinate(I), 0, 0), AExpected[I]) then
      Exit(False);
  Result := True;
end;

function CaptureGraphDomains(const AGraph: TGraph): TGraphDomains;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Integer(AGraph.Dimension.Width));
  for I := 0 to Length(Result) - 1 do
    Result[I] := AGraph.CopyAllowedValues(TGraphCoordinate(I), 0, 0);
end;

function ScalarTokens(const AText: TWfcModelToken): TWfcModelTokens;
begin
  Result := TokenizeWfcText(AText, wttkUnicodeScalar);
end;

function BmpToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($266B));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(
    UnicodeString(WideChar($266B))));
  {$ENDIF}
end;

function NonBmpToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($D83D) + Chr($DE42));
  {$ELSE}
  SetLength(Result, 4);
  Result[1] := AnsiChar($F0);
  Result[2] := AnsiChar($9F);
  Result[3] := AnsiChar($99);
  Result[4] := AnsiChar($82);
  {$ENDIF}
end;

function MalformedText: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($D800));
  {$ELSE}
  SetLength(Result, 1);
  Result[1] := AnsiChar($FF);
  {$ENDIF}
end;

function TokenizeAsciiWords(const AText: TWfcModelToken):
  TWfcModelTokens;
var
  I: Integer;
  LStart: Integer;
begin
  Result := nil;
  I := 1;
  while I <= Length(AText) do
  begin
    while (I <= Length(AText)) and (AText[I] = ' ') do
      Inc(I);
    if I > Length(AText) then
      Break;
    LStart := I;
    while (I <= Length(AText)) and (AText[I] <> ' ') do
      Inc(I);
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Copy(AText, LStart, I - LStart);
  end;
end;

procedure TestScalarTokenizer;
var
  LBmp: TWfcModelToken;
  LMalformedRejected: Boolean;
  LNonBmp: TWfcModelToken;
  LText: TWfcModelToken;
  LTokens: TWfcModelTokens;
begin
  Check((WFC_TEXT_TOKENIZER_VERSION = 1) and
    (WfcTextTokenizerName(wttkUnicodeScalar) = 'unicode-scalar'),
    'the Unicode-scalar tokenizer has a stable version and name');

  LText := 'A b,!'+#9+#10;
  LTokens := ScalarTokens(LText);
  Check(TokensMatch(LTokens, ['A', ' ', 'b', ',', '!', #9, #10]),
    'ASCII letters, whitespace, and punctuation remain separate scalars');
  Check(DetokenizeWfcText(LTokens, wttkUnicodeScalar) = LText,
    'ASCII scalar tokenization round-trips byte-for-byte');

  LBmp := BmpToken;
  LNonBmp := NonBmpToken;
  LText := 'x' + LBmp + LNonBmp + '!';
  LTokens := ScalarTokens(LText);
  Check(TokensMatch(LTokens, ['x', LBmp, LNonBmp, '!']),
    'BMP and non-BMP Unicode values each occupy one public token');
  Check(DetokenizeWfcText(LTokens, wttkUnicodeScalar) = LText,
    'BMP and non-BMP Unicode scalars round-trip on this host');

  LMalformedRejected := False;
  try
    LTokens := ScalarTokens(MalformedText);
  except
    on E: EWfcTextTokenize do LMalformedRejected := True;
  end;
  Check(LMalformedRejected,
    'malformed UTF-8 or an unpaired UTF-16 surrogate is rejected');
end;

procedure TestCallerTokenizerLearning;
var
  LCompletion: TWfcTextCompletion;
  LModel: TWfcSequenceModel;
  LRejected: Boolean;
  LReport: TWfcTextCompletionReport;
  LRequest: TWfcTextCompletionRequest;
begin
  LModel := LearnWfcTextModel(DocumentsOf([
    'red blue', 'red green']), 2, @TokenizeAsciiWords);
  try
    Check(TokensMatch(LModel.CopyPublicTokens,
      ['red', 'blue', 'green']),
      'caller tokenizers preserve deterministic first-seen vocabulary order');
    Check((LModel.SampleCount = 2) and
      (LModel.ObservationCount = 4) and
      (LModel.StateCount = 3),
      'caller-tokenized documents enter the ordinary sequence learner');
    Check((LModel.ProjectStateToken(0) = 'red') and
      (LModel.StateObservationCountAt(0) = 2),
      'caller-defined multi-scalar tokens retain raw counts');
    LRequest := DefaultWfcTextCompletionRequest(2, wseWhole, 0);
    LRejected := False;
    try
      TryCompleteWfcText(LModel, LRequest, LCompletion, LReport);
    except
      on E: EWfcTextComplete do LRejected := True;
    end;
    Check(LRejected,
      'scalar completion rejects a model built from multi-scalar tokens');
  finally
    LModel.Free;
  end;
end;

procedure CheckExtentSolve(const AModel: TWfcSequenceModel;
  const AExtent: TWfcSequenceExtent; const AExpected: TWfcModelTokens;
  const AMessage: String);
var
  LGenerated: TWfcGeneratedSequence;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LValidation: TWfcSequenceGraphValidationReport;
begin
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(TGraphCoordinate(Length(AExpected)), 1, 1);
    LGraph.WrapNeighbors := AExtent = wseWrap;
    LGraph.Seed := 0;
    ApplySequenceModelToGraph(AModel, LGraph, AExtent);
    IntersectSequenceLockedSpan(AModel, LGraph, 0, AExpected);
    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport) and
      CaptureSolvedSequence(AModel, LGraph, AExtent,
        LGenerated, LValidation) and
      TokenArraysMatch(LGenerated.Tokens, AExpected) and
      ValidateSequenceStatePath(AModel, LGenerated.StateIndices,
        AExtent, LValidation), AMessage);
  finally
    LGraph.Free;
  end;
end;

procedure TestExtentSemantics;
var
  LModel: TWfcSequenceModel;
  LReport: TWfcSequenceGraphValidationReport;
begin
  LModel := LearnWfcTextModel(DocumentsOf(['ABA']), 2,
    wttkUnicodeScalar);
  try
    Check(ValidateSequenceStatePath(LModel, IndicesOf([0, 1, 2]),
      wseWhole, LReport),
      'whole paths require both observed corpus boundaries');
    Check(ValidateSequenceStatePath(LModel, IndicesOf([0, 1]),
      wsePrefix, LReport) and
      (not ValidateSequenceStatePath(LModel, IndicesOf([0, 1]),
        wseWhole, LReport)),
      'prefix paths require a start but may stop before an observed end');
    Check(ValidateSequenceStatePath(LModel, IndicesOf([1, 2]),
      wseSuffix, LReport) and
      (not ValidateSequenceStatePath(LModel, IndicesOf([1, 2]),
        wseWhole, LReport)),
      'suffix paths may start inside a sample but require an observed end');
    Check(ValidateSequenceStatePath(LModel, IndicesOf([1]),
      wseFragment, LReport) and
      (not ValidateSequenceStatePath(LModel, IndicesOf([0]),
        wseFragment, LReport)) and
      (LReport.Issue.Kind = wsgikBoundaryState),
      'fragments require neither endpoint and reject BOS-bearing starts');
    Check(ValidateSequenceStatePath(LModel, IndicesOf([1, 2]),
      wseWrap, LReport),
      'wrapped paths require a BOS-free structural closing transition');

    CheckExtentSolve(LModel, wseWhole, ScalarTokens('ABA'),
      'whole graph adaptation and capture preserve extent semantics');
    CheckExtentSolve(LModel, wsePrefix, ScalarTokens('AB'),
      'prefix graph adaptation and capture preserve extent semantics');
    CheckExtentSolve(LModel, wseSuffix, ScalarTokens('BA'),
      'suffix graph adaptation and capture preserve extent semantics');
    CheckExtentSolve(LModel, wseFragment, ScalarTokens('BA'),
      'fragment graph adaptation and capture preserve extent semantics');
    CheckExtentSolve(LModel, wseWrap, ScalarTokens('BA'),
      'wrapped graph adaptation and capture preserve extent semantics');
  finally
    LModel.Free;
  end;
end;

function LearnCompletionModel: TWfcSequenceModel;
begin
  Result := LearnWfcTextModel(DocumentsOf([
    'the quick fox rests.',
    'the quiet owl rests.']), 3, wttkUnicodeScalar);
end;

function MainCompletionRequest: TWfcTextCompletionRequest;
begin
  Result := DefaultWfcTextCompletionRequest(20, wseWhole, 0);
  Result.Prefix := ScalarTokens('the qu');
  Result.Suffix := ScalarTokens(' rests.');
end;

procedure TestDomainAnalysis;
var
  LAnalysis: TWfcSequenceDomainAnalysis;
  LConstraints: TWfcSequenceTokenConstraints;
  LModel: TWfcSequenceModel;
  LRequest: TWfcTextCompletionRequest;
  LStart: Integer;
begin
  Check(WFC_SEQUENCE_ANALYZE_VERSION = 1,
    'sequence-domain analysis has an explicit version');
  LModel := LearnCompletionModel;
  try
    LRequest := MainCompletionRequest;
    LConstraints := BuildWfcTextCompletionConstraints(LRequest);
    Check(AnalyzeSequenceTokenDomains(LModel, LRequest.TokenLength,
      LRequest.Extent, LConstraints, LAnalysis) and
      LAnalysis.Satisfiable and
      (Length(LAnalysis.Positions) = 20),
      'the constrained completion has an exact feasible-domain analysis');
    Check(TokensMatch(LAnalysis.Positions[0].Tokens, ['t']) and
      (Length(LAnalysis.Positions[0].Weights) = 1) and
      (LAnalysis.Positions[0].Weights[0] = 2),
      'analysis aggregates raw weight two for the shared corpus start');
    Check(TokensMatch(LAnalysis.Positions[7].Tokens, ['e', 'c']) and
      (Length(LAnalysis.Positions[7].StateIndices) = 2) and
      (Length(LAnalysis.Positions[7].Weights) = 2) and
      (LAnalysis.Positions[7].Weights[0] = 1) and
      (LAnalysis.Positions[7].Weights[1] = 1),
      'position seven reports exact c/e candidates and raw weights');

    LStart := Length(LConstraints);
    SetLength(LConstraints, LStart + 2);
    LConstraints[LStart] := MakeWfcSequenceTokenConstraint(7,
      TokensOf(['c', 'e']));
    LConstraints[LStart + 1] := MakeWfcSequenceTokenConstraint(7,
      TokensOf(['e']));
    Check(AnalyzeSequenceTokenDomains(LModel, LRequest.TokenLength,
      LRequest.Extent, LConstraints, LAnalysis) and
      TokensMatch(LAnalysis.Positions[7].Tokens, ['e']) and
      (LAnalysis.Positions[7].Weights[0] = 1),
      'duplicate position domains intersect instead of replacing each other');
  finally
    LModel.Free;
  end;
end;

procedure TestBulkConstraintHelpers;
var
  LBefore: TGraphDomains;
  LConstraints: TWfcSequenceTokenConstraints;
  LGenerated: TWfcGeneratedSequence;
  LGraph: TGraph;
  LModel: TWfcSequenceModel;
  LOptions: TGraphSolveOptions;
  LRaised: Boolean;
  LReport: TGraphSolveReport;
  LValidation: TWfcSequenceGraphValidationReport;
begin
  LModel := LearnCompletionModel;
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(20, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.Seed := 0;
    ApplySequenceModelToGraph(LModel, LGraph, wseWhole);
    IntersectSequencePrefix(LModel, LGraph, ScalarTokens('the qu'));
    IntersectSequenceSuffix(LModel, LGraph, ScalarTokens(' rests.'));
    IntersectSequenceLockedSpan(LModel, LGraph, 10,
      ScalarTokens('fox'));
    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport) and
      CaptureSolvedSequence(LModel, LGraph, wseWhole,
        LGenerated, LValidation) and
      (DetokenizeWfcText(LGenerated.Tokens,
        wttkUnicodeScalar) = 'the quick fox rests.'),
      'prefix, suffix, and locked-span helpers compose exactly');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(20, 1, 1);
    LGraph.WrapNeighbors := False;
    ApplySequenceModelToGraph(LModel, LGraph, wseWhole);
    LBefore := CaptureGraphDomains(LGraph);
    SetLength(LConstraints, 2);
    LConstraints[0] := MakeWfcSequenceTokenConstraint(7,
      TokensOf(['c']));
    LConstraints[1] := MakeWfcSequenceTokenConstraint(8,
      TokensOf(['not-in-the-model']));
    LRaised := False;
    try
      IntersectSequenceTokenConstraints(LModel, LGraph, LConstraints);
    except
      on E: EArgumentException do LRaised := True;
    end;
    Check(LRaised and GraphDomainsMatch(LGraph, LBefore),
      'malformed bulk token constraints fail atomically during preflight');

    LConstraints[1] := MakeWfcSequenceTokenConstraint(20,
      TokensOf(['c']));
    LRaised := False;
    try
      IntersectSequenceTokenConstraints(LModel, LGraph, LConstraints);
    except
      on E: ERangeError do LRaised := True;
    end;
    Check(LRaised and GraphDomainsMatch(LGraph, LBefore),
      'out-of-range bulk constraints also leave every domain unchanged');
  finally
    LGraph.Free;
    LModel.Free;
  end;
end;

procedure TestCompletionAndValidation;
var
  LCompletion: TWfcTextCompletion;
  LInvalidRequest: TWfcTextCompletionRequest;
  LLockedCompletion: TWfcTextCompletion;
  LModel: TWfcSequenceModel;
  LReport: TWfcTextCompletionReport;
  LRequest: TWfcTextCompletionRequest;
  LRaisedByCompletion: Boolean;
  LRaisedByValidation: Boolean;
  LTampered: TWfcTextCompletion;
  LValidation: TWfcTextValidationReport;
begin
  Check(WFC_TEXT_COMPLETION_VERSION = 1,
    'text completion has an explicit version');
  LModel := LearnCompletionModel;
  try
    LRequest := MainCompletionRequest;
    Check(TryCompleteWfcText(LModel, LRequest, LCompletion, LReport) and
      (LReport.Status = wctcsCompleted) and
      (LCompletion.Text = 'the quick fox rests.') and
      LReport.Validation.Valid,
      'seed zero replays the quick/fox completion and validates it');

    SetLength(LRequest.LockedSpans, 1);
    LRequest.LockedSpans[0] := MakeWfcTextLockedSpan(10,
      ScalarTokens('owl'));
    Check(TryCompleteWfcText(LModel, LRequest, LLockedCompletion,
      LReport) and
      (LLockedCompletion.Text = 'the quiet owl rests.') and
      TokensMatch(Copy(LLockedCompletion.Generated.Tokens, 10, 3),
        ['o', 'w', 'l']),
      'an interior locked span is preserved and propagates both directions');

    LTampered := LCompletion;
    LTampered.Generated.Tokens := Copy(
      LCompletion.Generated.Tokens, 0,
      Length(LCompletion.Generated.Tokens));
    LTampered.Generated.Tokens[7] := 'e';
    Check((not ValidateWfcTextCompletion(LModel,
      MainCompletionRequest, LTampered, LValidation)) and
      (LValidation.Issue.Kind = wctvikProjection),
      'independent validation rejects a tampered public projection');

    LInvalidRequest := MainCompletionRequest;
    SetLength(LInvalidRequest.Domains, 1);
    LInvalidRequest.Domains[0] := MakeWfcSequenceTokenConstraint(7,
      TokensOf(['c', 'not-in-the-model']));
    LRaisedByValidation := False;
    try
      ValidateWfcTextCompletion(LModel, LInvalidRequest,
        LCompletion, LValidation);
    except
      on E: EArgumentException do LRaisedByValidation := True;
    end;
    Check(LRaisedByValidation,
      'independent validation rejects every unknown domain member');

    LInvalidRequest := MainCompletionRequest;
    LInvalidRequest.SolveOptions.MaxBacktracks := -1;
    LRaisedByValidation := False;
    LRaisedByCompletion := False;
    try
      ValidateWfcTextCompletion(LModel, LInvalidRequest,
        LCompletion, LValidation);
    except
      on E: ERangeError do LRaisedByValidation := True;
    end;
    try
      TryCompleteWfcText(LModel, LInvalidRequest,
        LLockedCompletion, LReport);
    except
      on E: ERangeError do LRaisedByCompletion := True;
    end;
    Check(LRaisedByValidation and LRaisedByCompletion,
      'negative solve limits are rejected by shared request preflight');
  finally
    LModel.Free;
  end;
end;

procedure TestCompletionFailuresAndPrefixExtent;
var
  LCompletion: TWfcTextCompletion;
  LModel: TWfcSequenceModel;
  LReport: TWfcTextCompletionReport;
  LRequest: TWfcTextCompletionRequest;
begin
  LModel := LearnWfcTextModel(DocumentsOf(['abcdef']), 3,
    wttkUnicodeScalar);
  try
    LRequest := DefaultWfcTextCompletionRequest(3, wsePrefix, 0);
    LRequest.Prefix := ScalarTokens('ab');
    Check(TryCompleteWfcText(LModel, LRequest, LCompletion, LReport) and
      (LCompletion.Text = 'abc'),
      'a prefix continuation may stop before the learned source ends');
    LRequest.Extent := wseWhole;
    Check((not TryCompleteWfcText(LModel, LRequest,
      LCompletion, LReport)) and
      (LReport.Status = wctcsUnsatisfiable) and
      (not LReport.Analysis.Satisfiable),
      'the same short request is unsatisfiable as a whole sample');
  finally
    LModel.Free;
  end;

  LModel := LearnCompletionModel;
  try
    LRequest := MainCompletionRequest;
    LRequest.Prefix := ScalarTokens('the quick ');
    SetLength(LRequest.LockedSpans, 1);
    LRequest.LockedSpans[0] := MakeWfcTextLockedSpan(10,
      ScalarTokens('owl'));
    Check((not TryCompleteWfcText(LModel, LRequest,
      LCompletion, LReport)) and
      (LReport.Status = wctcsUnsatisfiable) and
      (LReport.Analysis.Issue.Kind = wsaikNoPath),
      'a forbidden quick/owl mixed completion is reported unsatisfiable');
  finally
    LModel.Free;
  end;
end;

begin
  WriteLn('WFC text foundation conformance suite');
  WriteLn('====================================');
  RunTest('Unicode-scalar tokenization', @TestScalarTokenizer);
  RunTest('caller tokenizer learning', @TestCallerTokenizerLearning);
  RunTest('sequence extent semantics', @TestExtentSemantics);
  RunTest('exact domain analysis', @TestDomainAnalysis);
  RunTest('bulk constraint helpers', @TestBulkConstraintHelpers);
  RunTest('completion and independent validation',
    @TestCompletionAndValidation);
  RunTest('completion failure and prefix extent',
    @TestCompletionFailuresAndPrefixExtent);
  WriteLn('====================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d text checks failed', [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
