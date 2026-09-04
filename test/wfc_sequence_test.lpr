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
program wfc_sequence_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_sequence,
  wfc_sequence_learn,
  wfc_sequence_graph,
  wfc_sequence_text;

type
  TTestProcedure = procedure;

const
  GOLDEN_BRANCHING_ORDER_2 =
    'wfcs=1'#10 +
    'order=2'#10 +
    'samples=2'#10 +
    's=0,3'#10 +
    's=1,3'#10 +
    'tokens=3'#10 +
    't=0,A'#10 +
    't=1,B'#10 +
    't=2,C'#10 +
    'states=5'#10 +
    'q=0,2,2,0,B,E0'#10 +
    'q=1,1,0,0,T0,E1'#10 +
    'q=2,1,0,1,T1,E0'#10 +
    'q=3,1,0,0,T0,E2'#10 +
    'q=4,1,0,1,T2,E0'#10 +
    'end'#10;

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

function MusicalNoteToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($266B));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(
    UnicodeString(WideChar($266B))));
  {$ENDIF}
end;

function ReplaceOnce(const AText, AOld, ANew: String): String;
var
  LPosition: Integer;
begin
  LPosition := Pos(AOld, AText);
  if LPosition = 0 then
    raise Exception.Create('test replacement text was not found');
  Result := Copy(AText, 1, LPosition - 1) + ANew +
    Copy(AText, LPosition + Length(AOld),
      Length(AText) - LPosition - Length(AOld) + 1);
end;

procedure CheckLearnRejected(const ATokens: TWfcModelTokens;
  const AOrder: Integer; const AMessage: String);
var
  LModel: TWfcSequenceModel;
  LRaised: Boolean;
begin
  LModel := nil;
  LRaised := False;
  try
    try
      LModel := LearnSequenceModel(ATokens, AOrder);
    except
      on E: EWfcSequence do LRaised := True;
    end;
  finally
    LModel.Free;
  end;
  Check(LRaised, AMessage);
end;

procedure CheckDecodeRejected(const AText, AMessage: String);
var
  LModel: TWfcSequenceModel;
  LRaised: Boolean;
begin
  LModel := nil;
  LRaised := False;
  try
    try
      LModel := DecodeWfcSequenceText(AText);
    except
      on E: EConvertError do LRaised := True;
    end;
  finally
    LModel.Free;
  end;
  Check(LRaised, AMessage);
end;

procedure TestOrderOne;
var
  I: Integer;
  J: Integer;
  LModel: TWfcSequenceModel;
begin
  LModel := LearnSequenceModel(TokensOf(['A', 'B', 'A']), 1);
  try
    Check((LModel.Order = 1) and (LModel.HistorySize = 0),
      'order one has an empty latent history');
    Check((LModel.SampleCount = 1) and
      (LModel.SampleLengthAt(0) = 3) and
      (LModel.ObservationCount = 3),
      'sample and observation counts retain raw data');
    Check(TokensMatch(LModel.CopyPublicTokens, ['A', 'B']),
      'public vocabulary uses deterministic first-seen order');
    Check((LModel.StateCount = 2) and
      (LModel.StateObservationCountAt(0) = 2) and
      (LModel.StartCountAt(0) = 1) and
      (LModel.EndCountAt(0) = 1) and
      (LModel.StateObservationCountAt(1) = 1) and
      (LModel.StartCountAt(1) = 0) and
      (LModel.EndCountAt(1) = 0),
      'order-one state counts retain starts and ends');
    for I := 0 to LModel.StateCount - 1 do
      for J := 0 to LModel.StateCount - 1 do
        Check(LModel.StatesCompatible(I, J),
          'empty histories structurally allow every order-one pair');
  finally
    LModel.Free;
  end;
end;

procedure TestOrderTwoAndRepetition;
var
  LHistory: TWfcSequenceHistoryItem;
  LModel: TWfcSequenceModel;
begin
  LModel := LearnSequenceModel(TokensOf(['A', 'B', 'A']), 2);
  try
    Check((LModel.StateCount = 3) and (LModel.HistorySize = 1),
      'order two stores the expected three latent states');
    LHistory := LModel.HistoryItemAt(0, 0);
    Check((LHistory.Kind = wshBos) and (LHistory.TokenIndex = -1) and
      (LModel.StateEmittedTokenIndexAt(0) = 0),
      'the first state uses typed canonical BOS');
    Check(LModel.StatesCompatible(0, 1) and
      LModel.StatesCompatible(1, 2) and
      LModel.StatesCompatible(2, 1),
      'order-two structural overlap admits observed continuation and recombination');
    Check(not LModel.StatesCompatible(0, 2) and
      not LModel.StatesCompatible(1, 1),
      'order-two structural overlap rejects mismatched histories');
  finally
    LModel.Free;
  end;

  LModel := LearnSequenceModel(TokensOf(['A', 'A', 'A']), 2);
  try
    Check((LModel.StateCount = 2) and
      (LModel.StateObservationCountAt(0) = 1) and
      (LModel.StartCountAt(0) = 1) and
      (LModel.StateObservationCountAt(1) = 2) and
      (LModel.EndCountAt(1) = 1),
      'repetition aggregates structurally identical states');
    Check(LModel.StatesCompatible(0, 1) and
      LModel.StatesCompatible(1, 1),
      'repetition preserves entry and self-loop compatibility');
  finally
    LModel.Free;
  end;
end;

procedure TestOrderThreeAndSingleton;
var
  LItem: TWfcSequenceHistoryItem;
  LModel: TWfcSequenceModel;
begin
  LModel := LearnSequenceModel(TokensOf(['A', 'B', 'A', 'B']), 3);
  try
    Check(LModel.StateCount = 4,
      'order three retains four distinct bounded states');
    Check(LModel.StatesCompatible(0, 1) and
      LModel.StatesCompatible(1, 2) and
      LModel.StatesCompatible(2, 3) and
      LModel.StatesCompatible(3, 2),
      'order-three suffix/prefix overlap is exact');
    Check(not LModel.StatesCompatible(0, 2) and
      not LModel.StatesCompatible(2, 2),
      'order-three overlap rejects one-token-only matches');
  finally
    LModel.Free;
  end;

  LModel := LearnSequenceModel(TokensOf(['note']), 3);
  try
    Check((LModel.StateCount = 1) and
      (LModel.StateLeadingBosCountAt(0) = 2) and
      (LModel.StartCountAt(0) = 1) and
      (LModel.EndCountAt(0) = 1),
      'a singleton sample is valid at an order larger than its length');
    LItem := LModel.HistoryItemAt(0, 1);
    Check((LItem.Kind = wshBos) and (LItem.TokenIndex = -1),
      'all singleton history padding remains typed BOS');
  finally
    LModel.Free;
  end;
end;

procedure TestCorpusBoundaries;
var
  LModel: TWfcSequenceModel;
  LSamples: TWfcSequenceSamples;
begin
  LSamples := SamplesOf([
    MakeWfcSequenceSample(TokensOf(['A', 'B'])),
    MakeWfcSequenceSample(TokensOf(['C', 'A']))]);
  LModel := LearnSequenceModelCorpus(LSamples, 2);
  try
    Check((LModel.SampleCount = 2) and
      (LModel.SampleLengthAt(0) = 2) and
      (LModel.SampleLengthAt(1) = 2) and
      (LModel.ObservationCount = 4),
      'corpus sample boundaries and lengths are retained');
    Check(TokensMatch(LModel.CopyPublicTokens, ['A', 'B', 'C']),
      'corpus vocabulary remains first-seen across samples');
    Check((LModel.StateCount = 4) and
      (LModel.StateLeadingBosCountAt(0) = 1) and
      (LModel.StateLeadingBosCountAt(2) = 1),
      'each corpus sample restarts with BOS rather than forming a seam');
    Check(LModel.StartCountAt(0) = 1,
      'the first sample contributes one start observation');
    Check(LModel.StartCountAt(2) = 1,
      'the second sample contributes an independent start observation');
  finally
    LModel.Free;
  end;
end;

procedure TestImmutabilityAndGuards;
var
  LCounts: TWfcModelIntegerArray;
  LModel: TWfcSequenceModel;
  LReplacement: TWfcSequenceModel;
  LSamples: TWfcSequenceSamples;
  LStates: TWfcSequenceStates;
  LTokens: TWfcModelTokens;
  LRaised: Boolean;
begin
  CheckLearnRejected(TokensOf(['A']), 0,
    'learning rejects order zero');
  CheckLearnRejected(TokensOf([]), 1,
    'learning rejects an empty sample');
  CheckLearnRejected(TokensOf(['']), 1,
    'learning rejects an empty public token');
  SetLength(LSamples, 0);
  LRaised := False;
  try
    LReplacement := LearnSequenceModelCorpus(LSamples, 1);
    LReplacement.Free;
  except
    on E: EWfcSequence do LRaised := True;
  end;
  Check(LRaised, 'learning rejects an empty corpus');

  LModel := LearnSequenceModel(TokensOf(['A', 'B', 'A']), 2);
  try
    LTokens := LModel.CopyPublicTokens;
    LTokens[0] := 'changed';
    LStates := LModel.CopyStates;
    LStates[0].EmittedTokenIndex := 1;
    LStates[1].History[0] := MakeWfcSequenceBosHistoryItem;
    LCounts := LModel.CopyStateCounts;
    LCounts[0] := 99;
    Check((LModel.PublicTokenAt(0) = 'A') and
      (LModel.StateEmittedTokenIndexAt(0) = 0) and
      (LModel.HistoryItemAt(1, 0).Kind = wshToken) and
      (LModel.StateObservationCountAt(0) = 1),
      'all model copies are deeply isolated from immutable storage');

    LRaised := False;
    try
      LReplacement := TWfcSequenceModel.Create(2,
        LModel.CopySampleLengths, LModel.CopyPublicTokens,
        LModel.CopyStates, LCounts, LModel.CopyStartCounts,
        LModel.CopyEndCounts);
      LReplacement.Free;
    except
      on E: EWfcSequence do LRaised := True;
    end;
    Check(LRaised,
      'constructor rejects counts inconsistent with retained samples');

    LStates := LModel.CopyStates;
    LStates[1].History[0] := MakeWfcSequenceBosHistoryItem;
    LRaised := False;
    try
      LReplacement := TWfcSequenceModel.Create(2,
        LModel.CopySampleLengths, LModel.CopyPublicTokens, LStates,
        LModel.CopyStateCounts, LModel.CopyStartCounts,
        LModel.CopyEndCounts);
      LReplacement.Free;
    except
      on E: EWfcSequence do LRaised := True;
    end;
    Check(LRaised,
      'constructor rejects duplicate or boundary-inconsistent states');

    LRaised := False;
    try
      LModel.PublicTokenAt(-1);
    except
      on E: ERangeError do LRaised := True;
    end;
    Check(LRaised, 'public accessors reject invalid indices');
  finally
    LModel.Free;
  end;
end;

procedure TestGraphOpenAndDomains;
var
  LAllowed: TGraphValues;
  LFalsePosition: Integer;
  LGenerated: TWfcGeneratedSequence;
  LGraph: TGraph;
  LModel: TWfcSequenceModel;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LValidation: TWfcSequenceGraphValidationReport;
begin
  LModel := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(TokensOf(['A', 'B', 'A'])),
    MakeWfcSequenceSample(TokensOf(['A', 'C', 'A']))]), 2);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(3, 1, 1);
    LGraph.WrapNeighbors := False;
    ApplySequenceModelToGraph(LModel, LGraph);
    Check(LGraph.RuleGroups.Count = LModel.StateCount,
      'graph adaptation registers one private value per latent state');
    LAllowed := LGraph.CopyAllowedValues(0, 0, 0);
    Check(Length(LAllowed) = 1,
      'open graph start domain contains only observed starts');
    LAllowed := LGraph.CopyAllowedValues(2, 0, 0);
    Check(Length(LAllowed) = 2,
      'open graph end domain contains all observed ending states');

    IntersectSequenceAllowedTokens(LModel, LGraph, 1, 'B');
    LAllowed := LGraph.CopyAllowedValues(1, 0, 0);
    Check(Length(LAllowed) = 1,
      'public-token masking narrows a latent cell domain');
    IntersectSequenceAllowedTokens(LModel, LGraph, 1,
      TokensOf(['B', 'C']));
    Check(Length(LGraph.CopyAllowedValues(1, 0, 0)) = 1,
      'token masking intersects rather than replacing an existing domain');

    LGraph.Seed := 0;
    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport),
      'the masked open sequence graph solves');
    Check(CaptureSolvedSequence(LModel, LGraph, LGenerated,
      LValidation), 'a solved latent graph captures successfully');
    Check((LGenerated.Boundary = wmbOpen) and
      TokensMatch(LGenerated.Tokens, ['A', 'B', 'A']),
      'capture exposes only the expected public open sequence');
    Check(LValidation.Valid and (LValidation.CheckedStates = 3) and
      (LValidation.CheckedTransitions = 2),
      'capture returns complete public validation evidence');
    Check(SequenceStatesSatisfyEntryConstraints(LModel, LGraph,
        LGenerated.StateIndices, LFalsePosition) and
      (LFalsePosition = -1),
      'bulk caller-domain validation accepts the solved state path');

    LGraph.Entry[1, 0, 0].Value := LGraph.Entry[2, 0, 0].Value;
    LGraph.ClearAllowedValues(1, 0, 0);
    Check((not SequenceStateSatisfiesEntryConstraints(LModel, LGraph,
        1, LGenerated.StateIndices[1])) and
      (not SequenceStatesSatisfyEntryConstraints(LModel, LGraph,
        LGenerated.StateIndices, LFalsePosition)) and
      (LFalsePosition = 1),
      'single and bulk validation reject a conflicting caller lock');
  finally
    LGraph.Free;
    LModel.Free;
  end;
end;

procedure TestWrappedGraphAndValidation;
var
  LGenerated: TWfcGeneratedSequence;
  LGraph: TGraph;
  LModel: TWfcSequenceModel;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LValidation: TWfcSequenceGraphValidationReport;
begin
  LModel := LearnSequenceModel(TokensOf(['A', 'B', 'A']), 2);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.WrapNeighbors := True;
    ApplySequenceModelToGraph(LModel, LGraph);
    Check((Length(LGraph.CopyAllowedValues(0, 0, 0)) = 2) and
      (Length(LGraph.CopyAllowedValues(1, 0, 0)) = 2),
      'wrapped domains exclude every BOS-bearing state');
    LGraph.Seed := 0;
    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport),
      'an even alternating wrapped sequence solves');
    Check(CaptureSolvedSequence(LModel, LGraph, LGenerated,
      LValidation) and (LGenerated.Boundary = wmbWrap),
      'wrapped capture validates the closing transition');
    Check((LValidation.CheckedStates = 2) and
      (LValidation.CheckedTransitions = 2),
      'wrapped validation counts the closing arc');
  finally
    LGraph.Free;
  end;

  Check(not ValidateSequenceStatePath(LModel, IndicesOf([0, 1, 2]),
    wmbWrap, LValidation) and
    (LValidation.Issue.Kind = wsgikBoundaryState),
    'wrapped validation reports a BOS-bearing state without leaking keys');
  Check(not ValidateSequenceStatePath(LModel, IndicesOf([0]),
    wseFragment, LValidation) and
    (LValidation.Issue.Kind = wsgikBoundaryState) and
    (Pos('wrapped', DescribeSequenceGraphIssue(
      LValidation.Issue)) = 0),
    'BOS diagnostics remain accurate for non-wrapped extents');
  Check(not ValidateSequenceStatePath(LModel, IndicesOf([1, 1]),
    wmbWrap, LValidation) and
    (LValidation.Issue.Kind = wsgikTransition),
    'wrapped validation reports an incompatible transition');
  Check(not ValidateSequenceStatePath(LModel, IndicesOf([99]),
    wmbOpen, LValidation) and
    (LValidation.Issue.Kind = wsgikStateIndex),
    'validation reports invalid public state indices');
  Check(Pos('@wfcs', DescribeSequenceGraphIssue(
    LValidation.Issue)) = 0,
    'validation descriptions never reveal private graph keys');
  LModel.Free;

  LModel := LearnSequenceModel(TokensOf(['note']), 3);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := True;
    ApplySequenceModelToGraph(LModel, LGraph);
    Check(LGraph.HasAllowedValues(0, 0, 0) and
      (Length(LGraph.CopyAllowedValues(0, 0, 0)) = 0),
      'a wrapped singleton with only BOS states receives an explicit empty domain');
    LOptions := DefaultGraphSolveOptions;
    Check(not LGraph.TrySolve(LOptions, LReport) and
      (LReport.Contradiction.Kind = gckEntryDomain),
      'the impossible wrapped singleton fails as an entry-domain contradiction');
  finally
    LGraph.Free;
    LModel.Free;
  end;
end;

procedure TestCaptureFailures;
var
  LGenerated: TWfcGeneratedSequence;
  LGraph: TGraph;
  LModel: TWfcSequenceModel;
  LValidation: TWfcSequenceGraphValidationReport;
begin
  LModel := LearnSequenceModel(TokensOf(['A']), 1);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    ApplySequenceModelToGraph(LModel, LGraph);
    Check(not CaptureSolvedSequence(LModel, LGraph, LGenerated,
      LValidation) and (LValidation.Issue.Kind = wsgikEmptyCell),
      'capture rejects an unsolved cell');
    Check((Length(LGenerated.StateIndices) = 0) and
      (Length(LGenerated.Tokens) = 0),
      'failed empty-cell capture leaves output at Default');
    LGraph.Entry[0, 0, 0].Value := 'public-not-latent';
    Check(not CaptureSolvedSequence(LModel, LGraph, LGenerated,
      LValidation) and
      (LValidation.Issue.Kind = wsgikUnknownStateKey),
      'capture rejects an unknown value without returning it');
    Check((Length(LGenerated.StateIndices) = 0) and
      (Length(LGenerated.Tokens) = 0),
      'failed unknown-key capture leaves output at Default');
  finally
    LGraph.Free;
    LModel.Free;
  end;
end;

procedure TestPassProjectionHelpers;
var
  LGenerated: TWfcGeneratedSequence;
  LGraph: TGraph;
  LModel: TWfcSequenceModel;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LValidation: TWfcSequenceGraphValidationReport;
begin
  LModel := LearnSequenceModel(TokensOf(['A', 'B', 'A']), 2);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(3, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'tokens';
    LGraph.AddValue('A');
    LGraph.AddValue('B');
    LGraph.SetAllowedValues(0, 0, 0, 'A');
    LGraph.SetAllowedValues(1, 0, 0, 'B');
    LGraph.SetAllowedValues(2, 0, 0, 'A');
    LGraph.SwitchToPass('latent');
    ApplySequenceModelToGraph(LModel, LGraph);
    RequireSequenceProjectionFromTokenPass(LModel, LGraph, 'tokens');
    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport),
      'a latent sequence pass can require matching public provider tokens');
    Check(CaptureSolvedSequence(LModel, LGraph.PassGraph[1],
      LGenerated, LValidation) and
      TokensMatch(LGenerated.Tokens, ['A', 'B', 'A']),
      'provider requirements retain latent context and capture public tokens');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(3, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'latent';
    ApplySequenceModelToGraph(LModel, LGraph);
    IntersectSequenceAllowedTokens(LModel, LGraph, 1, 'B');
    LGraph.SwitchToPass('classes');
    LGraph.AddValue('vowel');
    LGraph.AddValue('other');
    RequireProjectedSequenceFromPass(LModel, LGraph, 'vowel',
      'latent', TokensOf(['A']));
    RequireProjectedSequenceFromPass(LModel, LGraph, 'other',
      'latent', TokensOf(['B']));
    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport),
      'a downstream pass can consume projected public-token classes');
    Check((LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'vowel') and
      (LGraph.PassGraph[1].Entry[1, 0, 0].Value = 'other') and
      (LGraph.PassGraph[1].Entry[2, 0, 0].Value = 'vowel'),
      'downstream OR requirements classify latent states by projection');
  finally
    LGraph.Free;
    LModel.Free;
  end;
end;

procedure TestProjectionMapCompositionAndValidation;
var
  LErrorMessage: String;
  LGenerated: TWfcGeneratedSequence;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRules: TWfcSequenceProjectionRules;
  LSourceModel: TWfcSequenceModel;
  LSourceModelReordered: TWfcSequenceModel;
  LSourceModelTwo: TWfcSequenceModel;
  LTargetModel: TWfcSequenceModel;
  LTargetModelReordered: TWfcSequenceModel;
  LValidation: TWfcSequenceGraphValidationReport;

  function ProjectionMapRejected(
    const ATargetModel, ASourceModel: TWfcSequenceModel;
    const ARules: TWfcSequenceProjectionRules): Boolean;
  var
    LDependencyCountBefore: Integer;
  begin
    Result := False;
    LErrorMessage := '';
    LDependencyCountBefore := LGraph.DependencyCount;
    try
      RequireSequenceProjectionMapFromPass(ATargetModel,
        ASourceModel, LGraph, 'source-one', ARules);
    except
      on E: Exception do
      begin
        Result := True;
        LErrorMessage := E.Message;
      end;
    end;
    Result := Result and
      (LGraph.DependencyCount = LDependencyCountBefore) and
      (Pos('@wfcs', LErrorMessage) = 0);
  end;

begin
  LSourceModel := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(TokensOf(['A'])),
    MakeWfcSequenceSample(TokensOf(['B'])),
    MakeWfcSequenceSample(TokensOf(['C']))]), 1);
  LSourceModelReordered := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(TokensOf(['A'])),
    MakeWfcSequenceSample(TokensOf(['C'])),
    MakeWfcSequenceSample(TokensOf(['B']))]), 1);
  LSourceModelTwo := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(TokensOf(['hot'])),
    MakeWfcSequenceSample(TokensOf(['cold']))]), 1);
  LTargetModel := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(TokensOf(['X'])),
    MakeWfcSequenceSample(TokensOf(['Y']))]), 1);
  LTargetModelReordered := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(TokensOf(['Y'])),
    MakeWfcSequenceSample(TokensOf(['X']))]), 1);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'source-one';
    ApplySequenceModelToGraph(LSourceModel, LGraph);
    IntersectSequenceAllowedTokens(LSourceModel, LGraph, 0, 'B');
    LGraph.SwitchToPass('source-two');
    ApplySequenceModelToGraph(LSourceModelTwo, LGraph);
    IntersectSequenceAllowedTokens(LSourceModelTwo, LGraph, 0, 'cold');
    LGraph.SwitchToPass('target');
    ApplySequenceModelToGraph(LTargetModel, LGraph);

    SetLength(LRules, 1);
    LRules[0] := MakeWfcSequenceProjectionRule('X', TokensOf(['A']));
    Check(ProjectionMapRejected(LTargetModel, LSourceModel, LRules),
      'a missing target projection is rejected atomically');

    SetLength(LRules, 2);
    LRules[0] := MakeWfcSequenceProjectionRule('X', TokensOf(['A']));
    LRules[1] := MakeWfcSequenceProjectionRule('X', TokensOf(['B']));
    Check(ProjectionMapRejected(LTargetModel, LSourceModel, LRules),
      'a duplicate target projection is rejected atomically');

    LRules[0] := MakeWfcSequenceProjectionRule('X', TokensOf(['A']));
    LRules[1] := MakeWfcSequenceProjectionRule('missing',
      TokensOf(['B']));
    Check(ProjectionMapRejected(LTargetModel, LSourceModel, LRules),
      'an unknown target projection is rejected atomically');

    LRules[0] := MakeWfcSequenceProjectionRule('X', TokensOf([]));
    LRules[1] := MakeWfcSequenceProjectionRule('Y', TokensOf(['C']));
    Check(ProjectionMapRejected(LTargetModel, LSourceModel, LRules),
      'an empty source-alternative list is rejected atomically');

    LRules[0] := MakeWfcSequenceProjectionRule('X',
      TokensOf(['A', 'missing']));
    LRules[1] := MakeWfcSequenceProjectionRule('Y', TokensOf(['C']));
    Check(ProjectionMapRejected(LTargetModel, LSourceModel, LRules),
      'an unknown source projection is rejected atomically');

    LRules[0] := MakeWfcSequenceProjectionRule('X',
      TokensOf(['A', 'A']));
    LRules[1] := MakeWfcSequenceProjectionRule('Y', TokensOf(['C']));
    Check(ProjectionMapRejected(LTargetModel, LSourceModel, LRules),
      'a duplicate source alternative is rejected atomically');

    LRules[0] := MakeWfcSequenceProjectionRule('X',
      TokensOf(['A', 'B']));
    LRules[1] := MakeWfcSequenceProjectionRule('Y', TokensOf(['C']));
    Check(ProjectionMapRejected(LTargetModel, LSourceModelReordered,
      LRules),
      'a projection map rejects the wrong applied source identity');
    Check(ProjectionMapRejected(LTargetModelReordered, LSourceModel,
      LRules),
      'a projection map rejects the wrong applied target identity');

    LGraph.SwitchToPass('target');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.SwitchToPass('source-one');
    LGraph.DependsOn('target');
    LGraph.SwitchToPass('target');
    Check(ProjectionMapRejected(LTargetModel, LSourceModel, LRules),
      'a projection map rejects a dependency cycle before mutation');
    LGraph.SwitchToPass('source-one');
    LGraph.RemoveDependency('target');
    LGraph.SwitchToPass('target');
    LGraph.PassMode := gpmLegacy;

    RequireSequenceProjectionMapFromPass(LTargetModel,
      LSourceModel, LGraph, 'source-one', LRules);
    Check(LGraph.DependencyCount = 2,
      'a complete projection map declares its additional source dependency');

    LRules[0] := MakeWfcSequenceProjectionRule('X', TokensOf(['hot']));
    LRules[1] := MakeWfcSequenceProjectionRule('Y', TokensOf(['cold']));
    RequireSequenceProjectionMapFromPass(LTargetModel,
      LSourceModelTwo, LGraph, 'source-two', LRules);
    Check(LGraph.DependencyCount = 2,
      'projection maps from distinct passes compose conjunctively');

    LOptions := DefaultGraphSolveOptions;
    Check(not LGraph.TrySolve(LOptions, LReport),
      'disagreeing source-pass projections create an exact contradiction');

    LGraph.SwitchToPass('source-two');
    LGraph.ClearAllowedValues(0, 0, 0);
    IntersectSequenceAllowedTokens(LSourceModelTwo, LGraph, 0, 'hot');
    LGraph.SwitchToPass('target');
    Check(LGraph.TrySolve(LOptions, LReport),
      'agreeing source-pass projections solve after the provider changes');
    Check(CaptureSolvedSequence(LTargetModel, LGraph.PassGraph[2],
      LGenerated, LValidation) and
      TokensMatch(LGenerated.Tokens, ['X']) and
      (Pos('@wfcs', String(LGenerated.Tokens[0])) = 0),
      'two source alternatives select public output without private-key leakage');
  finally
    LGraph.Free;
    LTargetModelReordered.Free;
    LTargetModel.Free;
    LSourceModelTwo.Free;
    LSourceModelReordered.Free;
    LSourceModel.Free;
  end;
end;

procedure TestProjectionBundleAtomicity;
var
  LBindings: TWfcSequenceProjectionBindings;
  LErrorMessage: String;
  LGenerated: TWfcGeneratedSequence;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRulesOne: TWfcSequenceProjectionRules;
  LRulesTwo: TWfcSequenceProjectionRules;
  LSourceModelOne: TWfcSequenceModel;
  LSourceModelTwo: TWfcSequenceModel;
  LTargetModel: TWfcSequenceModel;
  LValidation: TWfcSequenceGraphValidationReport;

  function BundleRejected(
    const ABindings: TWfcSequenceProjectionBindings): Boolean;
  var
    LDependencyCount: Integer;
  begin
    LDependencyCount := LGraph.DependencyCount;
    LErrorMessage := '';
    Result := False;
    try
      RequireSequenceProjectionMapsFromPasses(LTargetModel,
        LGraph, ABindings);
    except
      on E: Exception do
      begin
        Result := True;
        LErrorMessage := E.Message;
      end;
    end;
    Result := Result and
      (LGraph.DependencyCount = LDependencyCount) and
      (Pos('@wfcs', LErrorMessage) = 0);
  end;

begin
  LSourceModelOne := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(TokensOf(['A'])),
    MakeWfcSequenceSample(TokensOf(['B']))]), 1);
  LSourceModelTwo := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(TokensOf(['hot'])),
    MakeWfcSequenceSample(TokensOf(['cold']))]), 1);
  LTargetModel := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(TokensOf(['X'])),
    MakeWfcSequenceSample(TokensOf(['Y']))]), 1);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'source-one';
    ApplySequenceModelToGraph(LSourceModelOne, LGraph);
    IntersectSequenceAllowedTokens(LSourceModelOne, LGraph, 0, 'B');
    LGraph.SwitchToPass('source-two');
    ApplySequenceModelToGraph(LSourceModelTwo, LGraph);
    IntersectSequenceAllowedTokens(LSourceModelTwo, LGraph, 0, 'hot');
    LGraph.SwitchToPass('target');
    ApplySequenceModelToGraph(LTargetModel, LGraph);
    IntersectSequenceAllowedTokens(LTargetModel, LGraph, 0, 'X');

    SetLength(LRulesOne, 2);
    LRulesOne[0] := MakeWfcSequenceProjectionRule('X', TokensOf(['A']));
    LRulesOne[1] := MakeWfcSequenceProjectionRule('Y', TokensOf(['B']));
    SetLength(LRulesTwo, 2);
    LRulesTwo[0] := MakeWfcSequenceProjectionRule('X', TokensOf(['hot']));
    LRulesTwo[1] := MakeWfcSequenceProjectionRule('Y', TokensOf(['missing']));
    SetLength(LBindings, 2);
    LBindings[0] := MakeWfcSequenceProjectionBinding(LSourceModelOne,
      'source-one', LRulesOne);
    LBindings[1] := MakeWfcSequenceProjectionBinding(LSourceModelTwo,
      'source-two', LRulesTwo);
    Check(BundleRejected(LBindings),
      'a malformed second binding rejects the bundle before mutation');

    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport) and
      CaptureSolvedSequence(LTargetModel, LGraph.PassGraph[2],
        LGenerated, LValidation) and TokensMatch(LGenerated.Tokens, ['X']),
      'a rejected bundle leaves no first-binding semantic requirement');

    LBindings[1] := MakeWfcSequenceProjectionBinding(LSourceModelOne,
      'source-one', LRulesOne);
    Check(BundleRejected(LBindings),
      'a bundle rejects a repeated source pass instead of merging it as OR');

    SetLength(LBindings, 0);
    Check(BundleRejected(LBindings),
      'an empty projection bundle is rejected without mutation');

    LRulesOne[0] := MakeWfcSequenceProjectionRule('X', TokensOf(['B']));
    LRulesOne[1] := MakeWfcSequenceProjectionRule('Y', TokensOf(['A']));
    LRulesTwo[0] := MakeWfcSequenceProjectionRule('X', TokensOf(['hot']));
    LRulesTwo[1] := MakeWfcSequenceProjectionRule('Y', TokensOf(['cold']));
    SetLength(LBindings, 2);
    LBindings[0] := MakeWfcSequenceProjectionBinding(LSourceModelOne,
      'source-one', LRulesOne);
    LBindings[1] := MakeWfcSequenceProjectionBinding(LSourceModelTwo,
      'source-two', LRulesTwo);
    ValidateSequenceProjectionMapsFromPasses(LTargetModel,
      LGraph, LBindings);
    Check((LGraph.DependencyCount = 1) and
      (LGraph.DependencyIndex[0] = 1),
      'bundle validation preserves the legacy predecessor dependency');
    RequireSequenceProjectionMapsFromPasses(LTargetModel,
      LGraph, LBindings);
    Check((LGraph.DependencyCount = 2) and
      (LGraph.DependencyIndex[0] = 0) and
      (LGraph.DependencyIndex[1] = 1),
      'a valid bundle declares both distinct providers');
    Check(LGraph.TrySolve(LOptions, LReport) and
      CaptureSolvedSequence(LTargetModel, LGraph.PassGraph[2],
        LGenerated, LValidation) and TokensMatch(LGenerated.Tokens, ['X']),
      'a target must satisfy both valid bundle bindings');
  finally
    LGraph.Free;
    LTargetModel.Free;
    LSourceModelTwo.Free;
    LSourceModelOne.Free;
  end;
end;

procedure TestDeterministicBranchingSeeds;
var
  LGenerated: TWfcGeneratedSequence;
  LGraph: TGraph;
  LModel: TWfcSequenceModel;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LValidation: TWfcSequenceGraphValidationReport;

  function SolveTokens(const ASeed: TGraphSeed): TWfcModelTokens;
  begin
    LGraph := TGraph.Create;
    try
      LGraph.Reshape(3, 1, 1);
      LGraph.WrapNeighbors := False;
      LGraph.Seed := ASeed;
      ApplySequenceModelToGraph(LModel, LGraph);
      if not LGraph.TrySolve(LOptions, LReport) then
        raise Exception.Create('branching seed fixture did not solve');
      if not CaptureSolvedSequence(LModel, LGraph, LGenerated,
          LValidation) then
        raise Exception.Create('branching seed fixture did not capture');
      Result := LGenerated.Tokens;
    finally
      LGraph.Free;
    end;
  end;

begin
  LOptions := DefaultGraphSolveOptions;
  LModel := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(TokensOf(['A', 'B', 'A'])),
    MakeWfcSequenceSample(TokensOf(['A', 'C', 'A']))]), 2);
  try
    Check(TokensMatch(SolveTokens(0), ['A', 'B', 'A']),
      'seed zero deterministically selects the B branch');
    Check(TokensMatch(SolveTokens($DEADBEEF), ['A', 'C', 'A']),
      'seed DEADBEEF deterministically selects the C branch');
  finally
    LModel.Free;
  end;
end;

procedure TestAdapterIdentityAndAtomicity;
var
  LAllowedBefore: TGraphValues;
  LGenerated: TWfcGeneratedSequence;
  LGraph: TGraph;
  LModelAB: TWfcSequenceModel;
  LModelBA: TWfcSequenceModel;
  LModelX: TWfcSequenceModel;
  LModelY: TWfcSequenceModel;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRaised: Boolean;
  LValidation: TWfcSequenceGraphValidationReport;
begin
  LModelX := LearnSequenceModel(TokensOf(['X']), 1);
  LModelY := LearnSequenceModel(TokensOf(['Y']), 1);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    ApplySequenceModelToGraph(LModelX, LGraph);
    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport),
      'identity fixture solves with its applied model');
    Check(not CaptureSolvedSequence(LModelY, LGraph, LGenerated,
      LValidation),
      'capture rejects a distinct same-shape model identity');
    Check((Length(LGenerated.StateIndices) = 0) and
      (Length(LGenerated.Tokens) = 0),
      'identity-mismatch capture leaves output at Default');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    ApplySequenceModelToGraph(LModelX, LGraph);
    LRaised := False;
    try
      IntersectSequenceAllowedTokens(LModelY, LGraph, 0, 'Y');
    except
      on E: EWfcSequenceGraph do LRaised := True;
    end;
    Check(LRaised,
      'domain intersection rejects a distinct same-shape model identity');

    LRaised := False;
    try
      RequireSequenceProjectionFromTokenPass(LModelY, LGraph,
        'provider');
    except
      on E: EWfcSequenceGraph do LRaised := True;
    end;
    Check(LRaised,
      'provider projection rejects a distinct same-shape model identity');
  finally
    LGraph.Free;
    LModelY.Free;
    LModelX.Free;
  end;

  LModelAB := LearnSequenceModel(TokensOf(['A', 'B']), 1);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'source';
    ApplySequenceModelToGraph(LModelAB, LGraph);
    LAllowedBefore := LGraph.CopyAllowedValues(0, 0, 0);
    LRaised := False;
    try
      IntersectSequenceAllowedTokens(LModelAB, LGraph, 0,
        TokensOf(['A', 'missing']));
    except
      on E: EArgumentException do LRaised := True;
    end;
    Check(LRaised and
      (Length(LGraph.CopyAllowedValues(0, 0, 0)) =
        Length(LAllowedBefore)),
      'an unknown token array is rejected before mutating the domain');

    LGraph.SwitchToPass('classes');
    LGraph.AddValue('is-a');
    LModelBA := LearnSequenceModel(TokensOf(['B', 'A']), 1);
    try
      LRaised := False;
      try
        RequireProjectedSequenceFromPass(LModelBA, LGraph,
          'is-a', 'source', TokensOf(['A']));
      except
        on E: EWfcSequenceGraph do LRaised := True;
      end;
      Check(LRaised,
        'downstream projection rejects a reordered same-shape source model');
    finally
      LModelBA.Free;
    end;
  finally
    LGraph.Free;
    LModelAB.Free;
  end;
end;

procedure TestUnicodeProviderProjection;
var
  LDependencyCountBefore: Integer;
  LGenerated: TWfcGeneratedSequence;
  LGraph: TGraph;
  LModel: TWfcSequenceModel;
  LNote: TWfcModelToken;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRaised: Boolean;
  LValidation: TWfcSequenceGraphValidationReport;
begin
  LNote := MusicalNoteToken;
  LModel := LearnSequenceModel(TokensOf([LNote]), 1);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'public';
    {$IFDEF PAS2JS}
    LGraph.AddValue(TGraphValue(LNote));
    LGraph.SetAllowedValues(0, 0, 0, TGraphValue(LNote));
    {$ELSE}
    LGraph.AddValue(TGraphValue(UTF8Decode(LNote)));
    LGraph.SetAllowedValues(0, 0, 0,
      TGraphValue(UTF8Decode(LNote)));
    {$ENDIF}
    LGraph.SwitchToPass('latent');
    ApplySequenceModelToGraph(LModel, LGraph);
    LDependencyCountBefore := LGraph.DependencyCount;
    LRaised := False;
    try
      RequireSequenceProjectionFromTokenPass(LModel, LGraph, 'public');
    except
      on E: EWfcSequenceGraph do LRaised := True;
    end;
    if LRaised then
      Check(LGraph.DependencyCount = LDependencyCountBefore,
        'unrepresentable Unicode projection is rejected atomically')
    else
    begin
      LOptions := DefaultGraphSolveOptions;
      Check(LGraph.TrySolve(LOptions, LReport) and
        CaptureSolvedSequence(LModel, LGraph.PassGraph[1],
          LGenerated, LValidation) and
        TokensMatch(LGenerated.Tokens, [LNote]),
        'representable Unicode projection round-trips publicly');
    end;
  finally
    LGraph.Free;
    LModel.Free;
  end;
end;

procedure TestCanonicalTextCodec;
var
  LDecoded: TWfcSequenceModel;
  LModel: TWfcSequenceModel;
begin
  LModel := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(TokensOf(['A', 'B', 'A'])),
    MakeWfcSequenceSample(TokensOf(['A', 'C', 'A']))]), 2);
  try
    Check(EncodeWfcSequenceText(LModel) = GOLDEN_BRANCHING_ORDER_2,
      'the branching fixture has an exact canonical artifact');
    LDecoded := DecodeWfcSequenceText(GOLDEN_BRANCHING_ORDER_2);
    try
      Check(EncodeWfcSequenceText(LDecoded) = GOLDEN_BRANCHING_ORDER_2,
        'canonical decode and re-encode are byte stable');
      Check((LDecoded.Order = 2) and
        TokensMatch(LDecoded.CopyPublicTokens, ['A', 'B', 'C']) and
        (LDecoded.StateCount = 5),
        'codec round-trip preserves public model semantics');
    finally
      LDecoded.Free;
    end;
  finally
    LModel.Free;
  end;

  CheckDecodeRejected(ReplaceOnce(GOLDEN_BRANCHING_ORDER_2,
    #10, #13#10), 'the decoder rejects CRLF input');
  CheckDecodeRejected(Copy(GOLDEN_BRANCHING_ORDER_2, 1,
    Length(GOLDEN_BRANCHING_ORDER_2) - 1),
    'the decoder requires a final LF');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_BRANCHING_ORDER_2,
    'order=2', 'order=02'),
    'the decoder rejects leading-zero integers');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_BRANCHING_ORDER_2,
    't=0,A', 't=0,%41'),
    'the decoder rejects unnecessary token escapes');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_BRANCHING_ORDER_2,
    'q=0,2,2,0,B,E0', 'q=0,2,2,0,B,T0'),
    'the decoder rejects a history atom in the emitted field');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_BRANCHING_ORDER_2,
    'q=0,2,2,0,B,E0', 'q=0,2,2,0,T0,E0'),
    'the decoder rejects a non-BOS start state');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_BRANCHING_ORDER_2,
    'end'#10, 'end.'#10),
    'the decoder rejects a noncanonical end marker');
  CheckDecodeRejected(GOLDEN_BRANCHING_ORDER_2 + 'extra'#10,
    'the decoder rejects trailing data');
end;

procedure TestVersionsAndPrivateKeyCollision;
var
  LGraph: TGraph;
  LModel: TWfcSequenceModel;
  LTokens: TWfcModelTokens;
  LGenerated: TWfcGeneratedSequence;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LValidation: TWfcSequenceGraphValidationReport;
begin
  Check((WFC_SEQUENCE_MODEL_VERSION = 1) and
    (WFC_SEQUENCE_GRAPH_MODEL_VERSION = 1) and
    (WFC_SEQUENCE_EXTENT_VERSION = 1) and
    (WFC_SEQUENCE_LEARN_ALGORITHM_VERSION = 1) and
    (WFC_SEQUENCE_GRAPH_ADAPTER_VERSION = 1) and
    (WFC_SEQUENCE_TEXT_VERSION = 1),
    'every sequence artifact and algorithm surface has version one');

  LTokens := TokensOf(['@wfcs1:0:0', '@wfcs1:00:0', 'A']);
  LModel := LearnSequenceModel(LTokens, 1);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(3, 1, 1);
    LGraph.WrapNeighbors := False;
    ApplySequenceModelToGraph(LModel, LGraph);
    IntersectSequenceAllowedTokens(LModel, LGraph, 1,
      '@wfcs1:00:0');
    Check(LGraph.RuleGroups.Count = LModel.StateCount,
      'collision avoidance retains every latent graph state');
    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport) and
      CaptureSolvedSequence(LModel, LGraph, LGenerated,
        LValidation) and
      TokensMatch(LGenerated.Tokens,
        ['@wfcs1:0:0', '@wfcs1:00:0', 'A']),
      'canonical and noncanonical private-key lookalikes remain ordinary data');
  finally
    LGraph.Free;
    LModel.Free;
  end;
end;

begin
  WriteLn('WFC sequence foundation conformance suite');
  WriteLn('========================================');
  RunTest('order-one learning', @TestOrderOne);
  RunTest('order-two overlap and repetition',
    @TestOrderTwoAndRepetition);
  RunTest('order-three overlap and singleton',
    @TestOrderThreeAndSingleton);
  RunTest('corpus boundaries', @TestCorpusBoundaries);
  RunTest('immutability and constructor guards',
    @TestImmutabilityAndGuards);
  RunTest('open graph and public domains',
    @TestGraphOpenAndDomains);
  RunTest('wrapped graph and validation',
    @TestWrappedGraphAndValidation);
  RunTest('capture failure evidence', @TestCaptureFailures);
  RunTest('pass projection helpers', @TestPassProjectionHelpers);
  RunTest('projection map composition and validation',
    @TestProjectionMapCompositionAndValidation);
  RunTest('projection bundle atomicity',
    @TestProjectionBundleAtomicity);
  RunTest('deterministic branching seeds',
    @TestDeterministicBranchingSeeds);
  RunTest('adapter identity and atomicity',
    @TestAdapterIdentityAndAtomicity);
  RunTest('Unicode provider projection',
    @TestUnicodeProviderProjection);
  RunTest('canonical sequence text codec',
    @TestCanonicalTextCodec);
  RunTest('versions and private-key collision',
    @TestVersionsAndPrivateKeyCollision);
  WriteLn('========================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d sequence checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
