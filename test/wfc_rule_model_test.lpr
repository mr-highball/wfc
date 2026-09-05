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
program wfc_rule_model_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_model,
  wfc_rule_model;

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

function IntegersOf(const AValues: array of Integer):
  TWfcModelIntegerArray;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function RowsOf(const AValues: array of TWfcRuleRow): TWfcRuleRows;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function RankOneRows: TWfcRuleRows;
begin
  Result := RowsOf([
    MakeWfcAllowRuleRow(0, gdEast, True, IntegersOf([1])),
    MakeWfcDenyRuleRow(0, gdWest),
    MakeWfcDenyRuleRow(1, gdEast),
    MakeWfcAllowRuleRow(1, gdWest, True, IntegersOf([0]))
  ]);
end;

function NewRankOneModel: TWfcRuleModel;
begin
  Result := TWfcRuleModel.Create(1, TokensOf(['A', 'B']),
    IntegersOf([2, 3]), RankOneRows);
end;

procedure ExpectModelError(const ARank: Integer;
  const ATokens: TWfcModelTokens;
  const AWeights: TWfcModelIntegerArray; const ARows: TWfcRuleRows;
  const AExpected, ALabel: String);
var
  LMessage: String;
  LModel: TWfcRuleModel;
  LRaised: Boolean;
begin
  LMessage := '';
  LModel := nil;
  LRaised := False;
  try
    try
      LModel := TWfcRuleModel.Create(ARank, ATokens, AWeights, ARows);
    except
      on E: EWfcRuleModel do
      begin
        LRaised := True;
        LMessage := E.Message;
      end;
    end;
  finally
    LModel.Free;
  end;
  Check(LRaised and (Pos(AExpected, LMessage) > 0), ALabel);
  if not LRaised then
    WriteLn('    expected EWfcRuleModel containing: ', AExpected)
  else if Pos(AExpected, LMessage) = 0 then
    WriteLn('    actual: ', LMessage);
end;

procedure ExpectAnyError(const AOperation: Integer;
  const AModel: TWfcRuleModel; const AGraph: TGraph;
  const AExpected, ALabel: String);
var
  LMessage: String;
  LRaised: Boolean;
begin
  LMessage := '';
  LRaised := False;
  try
    case AOperation of
      0:
        ApplyRuleModelToGraph(AModel, AGraph);
      1:
        AModel.TokenAt(-1);
      2:
        AModel.RuleTargetAt(0, 99);
    end;
  except
    on E: Exception do
    begin
      LRaised := True;
      LMessage := E.Message;
    end;
  end;
  Check(LRaised and (Pos(AExpected, LMessage) > 0), ALabel);
end;

procedure TestImmutableModel;
var
  LModel: TWfcRuleModel;
  LRows: TWfcRuleRows;
  LSignature: TWfcRuleModelSignature;
  LTargets: TWfcModelIntegerArray;
  LTokens: TWfcModelTokens;
  LWeights: TWfcModelIntegerArray;
begin
  LTokens := TokensOf(['A', 'B']);
  LWeights := IntegersOf([2, 3]);
  LRows := RankOneRows;
  LModel := TWfcRuleModel.Create(1, LTokens, LWeights, LRows);
  try
    LSignature := LModel.Signature;
    Check((LModel.Rank = 1) and (LModel.ValueCount = 2) and
      (LModel.RuleCount = 4),
      'the immutable model exposes rank, value, and rule counts');
    Check((LModel.TokenAt(0) = 'A') and (LModel.FindToken('B') = 1) and
      (LModel.FindToken('missing') = -1),
      'token lookup uses stable value order');
    Check((LModel.WeightAt(0) = 2) and (LModel.WeightAt(1) = 3),
      'positive weights retain their exact values');
    Check((LModel.RuleOwnerAt(0) = 0) and
      (LModel.RuleDirectionAt(0) = gdEast) and
      (LModel.RuleStateAt(0) = wrsAllow) and
      LModel.RuleRequiredAt(0) and
      (LModel.RuleTargetAt(0, 0) = 1),
      'finite required rule metadata is available through scalar accessors');
    Check((LModel.RuleStateAt(1) = wrsDeny) and
      (LModel.RuleTargetCountAt(1) = 0) and
      (not LModel.RuleRequiredAt(1)),
      'explicit deny remains distinct from an absent wildcard row');
    Check((Length(WfcRuleModelSignatureHex(LSignature)) = 8) and
      (CalculateWfcRuleModelSignature(LModel) = LSignature),
      'the model signature is complete and reproducible');

    LTokens[0] := 'changed';
    LWeights[0] := 99;
    LRows[0].OwnerIndex := 1;
    LRows[0].TargetIndices[0] := 0;
    Check((LModel.TokenAt(0) = 'A') and (LModel.WeightAt(0) = 2) and
      (LModel.RuleOwnerAt(0) = 0) and
      (LModel.RuleTargetAt(0, 0) = 1) and
      (LModel.Signature = LSignature),
      'constructor inputs are deeply detached from the model');

    LTokens := LModel.CopyTokens;
    LWeights := LModel.CopyWeights;
    LTargets := LModel.CopyRuleTargets(0);
    LTokens[0] := 'copy';
    LWeights[0] := 101;
    LTargets[0] := 0;
    Check((LModel.TokenAt(0) = 'A') and (LModel.WeightAt(0) = 2) and
      (LModel.RuleTargetAt(0, 0) = 1) and
      (LModel.Signature = LSignature),
      'all public array copies are deeply detached');

    ExpectAnyError(1, LModel, nil, 'value index out of range',
      'value access rejects a negative index');
    ExpectAnyError(2, LModel, nil, 'target ordinal out of range',
      'target access rejects an invalid ordinal');
  finally
    LModel.Free;
  end;
end;

procedure TestRanksAndMultiTargetClosure;
var
  LModel: TWfcRuleModel;
  LRows: TWfcRuleRows;
begin
  LRows := RowsOf([
    MakeWfcAllowRuleRow(0, gdNorth, False, IntegersOf([0])),
    MakeWfcDenyRuleRow(0, gdEast),
    MakeWfcAllowRuleRow(0, gdSouth, False, IntegersOf([0])),
    MakeWfcDenyRuleRow(0, gdWest)
  ]);
  LModel := TWfcRuleModel.Create(2, TokensOf(['tile']),
    IntegersOf([1]), LRows);
  try
    Check((LModel.Rank = 2) and (LModel.RuleCount = 4),
      'rank two accepts the four planar directions');
  finally
    LModel.Free;
  end;

  LRows := RowsOf([
    MakeWfcAllowRuleRow(0, gdUp, True, IntegersOf([1, 2])),
    MakeWfcAllowRuleRow(1, gdDown, True, IntegersOf([0])),
    MakeWfcAllowRuleRow(2, gdDown, True, IntegersOf([0]))
  ]);
  LModel := TWfcRuleModel.Create(3, TokensOf(['floor', 'wall', 'air']),
    IntegersOf([4, 2, 8]), LRows);
  try
    Check((LModel.Rank = 3) and (LModel.RuleTargetCountAt(0) = 2) and
      (LModel.RuleTargetAt(0, 0) = 1) and
      (LModel.RuleTargetAt(0, 1) = 2),
      'rank three retains ordered multi-target vertical closure');
  finally
    LModel.Free;
  end;
end;

procedure TestStructuralRejections;
var
  LRow: TWfcRuleRow;
  LRows: TWfcRuleRows;
begin
  ExpectModelError(0, TokensOf(['A']), IntegersOf([1]), nil,
    'rank must be 1, 2, or 3', 'rank zero is rejected');
  ExpectModelError(4, TokensOf(['A']), IntegersOf([1]), nil,
    'rank must be 1, 2, or 3', 'rank four is rejected');
  ExpectModelError(1, nil, nil, nil, 'at least one value',
    'an empty value vocabulary is rejected');
  ExpectModelError(1, TokensOf(['A']), nil, nil,
    'weight count must match', 'weight arity is validated');
  ExpectModelError(1, TokensOf(['']), IntegersOf([1]), nil,
    'token must be nonempty', 'an empty token is rejected');
  ExpectModelError(1, TokensOf(['A', 'A']), IntegersOf([1, 1]), nil,
    'tokens must be unique', 'duplicate tokens are rejected');
  ExpectModelError(1, TokensOf(['A']), IntegersOf([0]), nil,
    'weight must be positive', 'zero weight is rejected');
  ExpectModelError(1, TokensOf(['A']), IntegersOf([-1]), nil,
    'weight must be positive', 'negative weight is rejected');

  LRows := RowsOf([
    MakeWfcDenyRuleRow(0, gdWest),
    MakeWfcDenyRuleRow(0, gdEast)
  ]);
  ExpectModelError(1, TokensOf(['A']), IntegersOf([1]), LRows,
    'strictly ordered', 'rule rows cannot be out of owner/direction order');
  LRows := RowsOf([
    MakeWfcDenyRuleRow(0, gdEast),
    MakeWfcDenyRuleRow(0, gdEast)
  ]);
  ExpectModelError(1, TokensOf(['A']), IntegersOf([1]), LRows,
    'strictly ordered', 'duplicate owner/direction rows are rejected');
  LRows := RowsOf([MakeWfcDenyRuleRow(0, gdNorth)]);
  ExpectModelError(1, TokensOf(['A']), IntegersOf([1]), LRows,
    'inactive for rank 1', 'rank one rejects north/south rows');
  LRows := RowsOf([MakeWfcDenyRuleRow(2, gdEast)]);
  ExpectModelError(1, TokensOf(['A']), IntegersOf([1]), LRows,
    'owner index is out of range', 'owner indices are range checked');

  LRows := RowsOf([
    MakeWfcAllowRuleRow(0, gdEast, False, nil)
  ]);
  ExpectModelError(1, TokensOf(['A']), IntegersOf([1]), LRows,
    'at least one target', 'finite allow rows cannot be empty');
  LRow := MakeWfcDenyRuleRow(0, gdEast);
  LRow.Required := True;
  ExpectModelError(1, TokensOf(['A']), IntegersOf([1]), RowsOf([LRow]),
    'deny rule cannot be required', 'deny rows reject required metadata');
  LRow := MakeWfcDenyRuleRow(0, gdEast);
  LRow.TargetIndices := IntegersOf([0]);
  ExpectModelError(1, TokensOf(['A']), IntegersOf([1]), RowsOf([LRow]),
    'deny rule cannot contain targets', 'deny rows reject target indices');
  LRows := RowsOf([
    MakeWfcAllowRuleRow(0, gdEast, False, IntegersOf([1]))
  ]);
  ExpectModelError(1, TokensOf(['A']), IntegersOf([1]), LRows,
    'target index is out of range', 'target indices are range checked');
  LRows := RowsOf([
    MakeWfcAllowRuleRow(0, gdEast, False, IntegersOf([1, 0]))
  ]);
  ExpectModelError(1, TokensOf(['A', 'B']), IntegersOf([1, 1]), LRows,
    'strictly ordered', 'target indices must be strictly increasing');

  LRows := RowsOf([
    MakeWfcAllowRuleRow(0, gdEast, False, IntegersOf([1]))
  ]);
  ExpectModelError(1, TokensOf(['A', 'B']), IntegersOf([1, 1]), LRows,
    'no reciprocal row', 'a finite edge requires an inverse row');
  LRows := RowsOf([
    MakeWfcAllowRuleRow(0, gdEast, False, IntegersOf([1])),
    MakeWfcDenyRuleRow(1, gdWest)
  ]);
  ExpectModelError(1, TokensOf(['A', 'B']), IntegersOf([1, 1]), LRows,
    'no reciprocal target', 'an inverse deny cannot close a finite edge');
  LRows := RowsOf([
    MakeWfcAllowRuleRow(0, gdEast, False, IntegersOf([1])),
    MakeWfcAllowRuleRow(1, gdWest, True, IntegersOf([0]))
  ]);
  ExpectModelError(1, TokensOf(['A', 'B']), IntegersOf([1, 1]), LRows,
    'required metadata does not match',
    'required direction metadata must already be at its reciprocal fixed point');
end;

procedure TestVersionedCardinalityLimits;
var
  LRows: TWfcRuleRows;
  LTokens: TWfcModelTokens;
  LWeights: TWfcModelIntegerArray;
begin
  Check((WFC_RULE_LIMITS_VERSION = 1) and
    (WFC_RULE_MAX_VALUE_COUNT = 1024) and
    (WFC_RULE_MAX_RULE_COUNT = 6144) and
    (WFC_RULE_MAX_TOTAL_TARGET_COUNT = 65536),
    'version-one rule cardinality limits are exact public constants');

  SetLength(LTokens, WFC_RULE_MAX_VALUE_COUNT + 1);
  SetLength(LWeights, WFC_RULE_MAX_VALUE_COUNT + 1);
  ExpectModelError(1, LTokens, LWeights, nil,
    'value count exceeds the version-1 limit',
    'value cardinality is rejected before quadratic uniqueness work');

  SetLength(LRows, WFC_RULE_MAX_RULE_COUNT + 1);
  ExpectModelError(1, TokensOf(['A']), IntegersOf([1]), LRows,
    'row count exceeds the version-1 limit',
    'rule-row cardinality is rejected before table construction');

  SetLength(LRows, 1);
  LRows[0] := MakeWfcAllowRuleRow(0, gdEast, False, nil);
  SetLength(LRows[0].TargetIndices,
    WFC_RULE_MAX_TOTAL_TARGET_COUNT + 1);
  ExpectModelError(1, TokensOf(['A']), IntegersOf([1]), LRows,
    'target count exceeds the version-1 aggregate limit',
    'aggregate targets are rejected before reciprocal-closure work');
end;

function GraphValuesEqual(const ALeft, ARight: TGraphValues): Boolean;
var
  I: Integer;
begin
  if Length(ALeft) <> Length(ARight) then
    Exit(False);
  for I := 0 to Length(ALeft) - 1 do
    if ALeft[I] <> ARight[I] then
      Exit(False);
  Result := True;
end;

function RuleGroupsMatch(const ALeft, ARight: TGraphRuleGroup): Boolean;
var
  D: TGraphDirection;
begin
  if (ALeft.Weight <> ARight.Weight) or
      (ALeft.DeniedDirections <> ARight.DeniedDirections) then
    Exit(False);
  for D := Low(TGraphDirection) to High(TGraphDirection) do
  begin
    if ALeft.Exists[D] <> ARight.Exists[D] then
      Exit(False);
    if ALeft.Exists[D] and
        ((ALeft.Rule[D].Info <> ARight.Rule[D].Info) or
         (not GraphValuesEqual(ALeft.Rule[D].Value,
           ARight.Rule[D].Value))) then
      Exit(False);
  end;
  Result := True;
end;

procedure TestGraphAdapter;
var
  LGraph: TGraph;
  LManual: TGraph;
  LModel: TWfcRuleModel;
  LValues: TGraphValues;
begin
  LModel := NewRankOneModel;
  LGraph := TGraph.Create;
  LManual := TGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LManual.Reshape(2, 1, 1);
    ApplyRuleModelToGraph(LModel, LGraph);

    LValues := LGraph.CopyRegisteredValues;
    Check((Length(LValues) = 2) and (LValues[0] = 'A') and
      (LValues[1] = 'B'),
      'the adapter preserves canonical value registration order');
    Check((LGraph.Rules['A'].Weight = 2) and
      (LGraph.Rules['B'].Weight = 3),
      'the adapter preserves exact positive weights');
    Check(LGraph.Rules['A'].Exists[gdEast] and
      LGraph.Rules['A'].Rule[gdEast].Info and
      GraphValuesEqual(LGraph.Rules['A'].Rule[gdEast].Value,
        TGraphValues.Create('B')),
      'the adapter installs finite required support');
    Check(LGraph.Rules['A'].Denied[gdWest] and
      (not LGraph.Rules['A'].Exists[gdWest]) and
      (not LGraph.Rules['A'].Exists[gdNorth]) and
      (not LGraph.Rules['A'].Denied[gdNorth]),
      'explicit deny remains distinct from an absent wildcard direction');

    LManual.AddValue('A', 2);
    LManual.AddValue('B', 3);
    LManual.Rules['A'].NewRule([gdEast], 'B', True);
    LManual.Rules['A'].DenyAll([gdWest]);
    LManual.Rules['B'].DenyAll([gdEast]);
    Check(RuleGroupsMatch(LGraph.Rules['A'], LManual.Rules['A']) and
      RuleGroupsMatch(LGraph.Rules['B'], LManual.Rules['B']),
      'prepared model rows equal TGraph fluent reciprocal closure exactly');
  finally
    LManual.Free;
    LGraph.Free;
    LModel.Free;
  end;
end;

procedure TestAdapterPreflight;
var
  LGraph: TGraph;
  LModel: TWfcRuleModel;
  LValues: TGraphValues;
begin
  LModel := NewRankOneModel;
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('existing', 7).DenyAll([gdEast]);
    LValues := LGraph.CopyRegisteredValues;
    ExpectAnyError(0, LModel, LGraph, 'must be empty',
      'the adapter rejects a pass with an existing definition');
    Check((Length(LGraph.CopyRegisteredValues) = 1) and
      (LGraph.CopyRegisteredValues[0] = LValues[0]) and
      (LGraph.Rules['existing'].Weight = 7) and
      LGraph.Rules['existing'].Denied[gdEast],
      'failed empty-pass preflight leaves the target definition unchanged');
    ExpectAnyError(0, nil, LGraph, 'model must be assigned',
      'the adapter rejects a nil model before mutation');
    ExpectAnyError(0, LModel, nil, 'graph must be assigned',
      'the adapter rejects a nil graph before mutation');
  finally
    LGraph.Free;
    LModel.Free;
  end;
end;

procedure TestSignatureSensitivity;
var
  LBase: TWfcRuleModel;
  LChangedRank: TWfcRuleModel;
  LChangedRule: TWfcRuleModel;
  LChangedToken: TWfcRuleModel;
  LChangedWeight: TWfcRuleModel;
begin
  LBase := NewRankOneModel;
  LChangedRank := TWfcRuleModel.Create(2, TokensOf(['A', 'B']),
    IntegersOf([2, 3]), RankOneRows);
  LChangedToken := TWfcRuleModel.Create(1, TokensOf(['A', 'C']),
    IntegersOf([2, 3]), RankOneRows);
  LChangedWeight := TWfcRuleModel.Create(1, TokensOf(['A', 'B']),
    IntegersOf([2, 4]), RankOneRows);
  LChangedRule := TWfcRuleModel.Create(1, TokensOf(['A', 'B']),
    IntegersOf([2, 3]), RowsOf([
      MakeWfcAllowRuleRow(0, gdEast, False, IntegersOf([1])),
      MakeWfcDenyRuleRow(0, gdWest),
      MakeWfcDenyRuleRow(1, gdEast),
      MakeWfcAllowRuleRow(1, gdWest, False, IntegersOf([0]))
    ]));
  try
    Check((LBase.Signature <> LChangedRank.Signature) and
      (LBase.Signature <> LChangedToken.Signature) and
      (LBase.Signature <> LChangedWeight.Signature) and
      (LBase.Signature <> LChangedRule.Signature),
      'rank, token, weight, and rule metadata all affect model identity');
    WriteLn('  [INFO] rank-one signature: ',
      WfcRuleModelSignatureHex(LBase.Signature));
  finally
    LChangedRule.Free;
    LChangedWeight.Free;
    LChangedToken.Free;
    LChangedRank.Free;
    LBase.Free;
  end;
end;

begin
  WriteLn('WFC generic rule-model conformance suite');
  WriteLn('========================================');
  RunTest('immutable model and detached ownership', @TestImmutableModel);
  RunTest('rank and multi-target closure', @TestRanksAndMultiTargetClosure);
  RunTest('structural rejection matrix', @TestStructuralRejections);
  RunTest('versioned cardinality limits', @TestVersionedCardinalityLimits);
  RunTest('fresh-pass graph adapter', @TestGraphAdapter);
  RunTest('adapter complete preflight', @TestAdapterPreflight);
  RunTest('semantic signature sensitivity', @TestSignatureSensitivity);
  WriteLn('========================================');
  WriteLn('Checks: ', GCheckCount, '  Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d rule-model checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
