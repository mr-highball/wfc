program wfc_test;

{$Mode delphi}{$H+}
{$IFNDEF PAS2JS}
{$ModeSwitch nestedprocvars}
{$ENDIF}

uses
  SysUtils,
  Generics.Collections,
  wfc;

type
  TTestProcedure = procedure;

  TTestGraph = class(TGraph)
  strict private
    FPassInitialized: Boolean;
  strict protected
    function DoCreateEntry: TGraphEntry; override;
    function DoCreatePass(const APassIndex: Integer): TGraph; override;
    procedure DoInitializePass; override;
  public
    property PassInitialized: Boolean read FPassInitialized;
    procedure ValidateForTest(const AEntry: TGraphEntry;
      out AValues: TGraphValues);
  end;

  TConfiguredGraph = class(TGraph)
  strict private
    FConfiguration: String;
    FInitializedConfiguration: String;
    FRegisteredDuringInitialization: Boolean;
  strict protected
    function DoCreatePass(const APassIndex: Integer): TGraph; override;
    procedure DoInitializePass; override;
  public
    constructor Create(const AConfiguration: String); reintroduce;
    property InitializedConfiguration: String
      read FInitializedConfiguration;
    property RegisteredDuringInitialization: Boolean
      read FRegisteredDuringInitialization;
  end;

const
  MAX_CAPTURED_PASSES = 16;

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;
  GCapturedPassCount: Integer = 0;
  GCapturedPassNames: array[0..Pred(MAX_CAPTURED_PASSES)] of String;
  GCapturedPassIndices: array[0..Pred(MAX_CAPTURED_PASSES)] of Integer;
  GCapturedPassGraphs: array[0..Pred(MAX_CAPTURED_PASSES)] of TGraph;
  GSelectionSwitchCount: Integer = 0;
  GObservedCallbackPasses: array[0..Pred(MAX_CAPTURED_PASSES)] of Integer;
  GInvalidRecoveryCount: Integer = 0;
  GPassFactoryCount: Integer = 0;
  GPassInitializeCount: Integer = 0;
  GFailPassInitializeAt: Integer = 0;
  GInitializeNestedPass: Boolean = False;
  GInitializeRenamePass: Boolean = False;
  GInitializeSetMode: Boolean = False;
  GInitializeSetWrap: Boolean = False;
  GEntryCreateCount: Integer = 0;
  GFailEntryCreateAt: Integer = 0;

function TTestGraph.DoCreateEntry: TGraphEntry;
begin
  Inc(GEntryCreateCount);
  if (GFailEntryCreateAt > 0)
    and (GEntryCreateCount = GFailEntryCreateAt) then
    raise Exception.Create('intentional entry factory failure');
  Result := inherited DoCreateEntry;
end;

function TTestGraph.DoCreatePass(const APassIndex: Integer): TGraph;
begin
  Inc(GPassFactoryCount);
  Result := inherited DoCreatePass(APassIndex);
end;

procedure TTestGraph.DoInitializePass;
begin
  inherited DoInitializePass;
  Inc(GPassInitializeCount);
  if GInitializeRenamePass then
    CurrentPass := 'renamed-by-initializer';
  if GInitializeNestedPass then
    SwitchToPass('nested-by-initializer');
  if GInitializeSetMode then
    Mode := rmTopDown;
  if GInitializeSetWrap then
    WrapNeighbors := False;
  if (GFailPassInitializeAt > 0)
    and (GPassInitializeCount = GFailPassInitializeAt) then
    raise Exception.Create('intentional pass initialization failure');
  FPassInitialized := True;
end;

procedure TTestGraph.ValidateForTest(const AEntry: TGraphEntry;
  out AValues: TGraphValues);
begin
  DoValidate(AEntry, 0, 0, AValues);
end;

constructor TConfiguredGraph.Create(const AConfiguration: String);
begin
  inherited Create;
  FConfiguration := AConfiguration;
end;

function TConfiguredGraph.DoCreatePass(
  const APassIndex: Integer): TGraph;
begin
  Result := inherited DoCreatePass(APassIndex);
  TConfiguredGraph(Result).FConfiguration := FConfiguration;
end;

procedure TConfiguredGraph.DoInitializePass;
begin
  inherited DoInitializePass;
  FInitializedConfiguration := FConfiguration;
  FRegisteredDuringInitialization :=
    PassGraph[CurrentPassIndex] = Self;
end;

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

function SelectFirstValid(const {%H-}AGraph: TGraph;
  const AEntry: TGraphEntry; const AValid: TGraphValues): TGraphValue;
begin
  if Length(AValid) = 0 then
    Result := AEntry.Value
  else
    Result := AValid[0];
end;

function SelectLastValid(const {%H-}AGraph: TGraph;
  const AEntry: TGraphEntry; const AValid: TGraphValues): TGraphValue;
begin
  if Length(AValid) = 0 then
    Result := AEntry.Value
  else
    Result := AValid[High(AValid)];
end;

function SelectAndSwitchPass(const AGraph: TGraph;
  const {%H-}AEntry: TGraphEntry;
  const AValid: TGraphValues): TGraphValue;
begin
  if GSelectionSwitchCount < MAX_CAPTURED_PASSES then
    GObservedCallbackPasses[GSelectionSwitchCount] :=
      AGraph.CurrentPassIndex;
  Inc(GSelectionSwitchCount);
  if AGraph.CurrentPassIndex = 0 then
    AGraph.SwitchToPass(1)
  else
    AGraph.SwitchToPass(0);
  Result := AValid[0];
end;

function SelectAndRenamePass(const AGraph: TGraph;
  const {%H-}AEntry: TGraphEntry;
  const AValid: TGraphValues): TGraphValue;
begin
  if AGraph.CurrentPass = 'before' then
    AGraph.CurrentPass := 'after';
  Result := AValid[0];
end;

function SelectAndCreatePass(const AGraph: TGraph;
  const {%H-}AEntry: TGraphEntry;
  const AValid: TGraphValues): TGraphValue;
begin
  AGraph.SwitchToPass('created-during-run');
  Result := AValid[0];
end;

function SelectOutsideDomain(const {%H-}AGraph: TGraph;
  const {%H-}AEntry: TGraphEntry;
  const {%H-}AValid: TGraphValues): TGraphValue;
begin
  Result := 'outside-domain';
end;

procedure ReplaceInvalidWithNone(const {%H-}AGraph: TGraph;
  const {%H-}AEntry: TGraphEntry; var AValue: TGraphValue);
begin
  Inc(GInvalidRecoveryCount);
  AValue := 'none';
end;

procedure ReplaceInvalidWithAllowed(const {%H-}AGraph: TGraph;
  const {%H-}AEntry: TGraphEntry; var AValue: TGraphValue);
begin
  AValue := 'allowed';
end;

procedure ConfigureDeterministicSelection(const AGraph: TGraph);
var
  I: Integer;
begin
  for I := 0 to Pred(AGraph.TotalPassCount) do
    AGraph.PassGraph[I].SelectionCallback := SelectFirstValid;
end;

procedure ResetCapturedPasses;
var
  I: Integer;
begin
  GCapturedPassCount := 0;
  for I := 0 to Pred(MAX_CAPTURED_PASSES) do
  begin
    GCapturedPassNames[I] := '';
    GCapturedPassIndices[I] := -1;
    GCapturedPassGraphs[I] := nil;
  end;
end;

procedure CapturePass(const AGraph: TGraph; const APass: String;
  const APassIndex: Integer);
begin
  if GCapturedPassCount < MAX_CAPTURED_PASSES then
  begin
    GCapturedPassNames[GCapturedPassCount] := APass;
    GCapturedPassIndices[GCapturedPassCount] := APassIndex;
    GCapturedPassGraphs[GCapturedPassCount] := AGraph;
  end;
  Inc(GCapturedPassCount);
end;

procedure CapturePassAndSwitch(const AGraph: TGraph; const APass: String;
  const APassIndex: Integer);
begin
  CapturePass(AGraph, APass, APassIndex);
  AGraph.SwitchToPass(0);
end;

procedure TestGraphEntry;
var
  LEntry: TGraphEntry;
  LNeighbor: TGraphEntry;
begin
  LEntry := TGraphEntry.Create;
  LNeighbor := TGraphEntry.Create;
  try
    Check(LEntry.Empty, 'a new entry is empty');
    Check(not Assigned(LEntry[gdNorth]), 'north neighbor starts nil');
    Check(not Assigned(LEntry[gdEast]), 'east neighbor starts nil');
    Check(not Assigned(LEntry[gdSouth]), 'south neighbor starts nil');
    Check(not Assigned(LEntry[gdWest]), 'west neighbor starts nil');
    Check(not Assigned(LEntry[gdUp]), 'up neighbor starts nil');
    Check(not Assigned(LEntry[gdDown]), 'down neighbor starts nil');

    LEntry[gdNorth] := LNeighbor;
    Check(LEntry[gdNorth] = LNeighbor, 'a neighbor can be assigned and read');

    LEntry.Value := 'test';
    Check(LEntry.Value = 'test', 'an entry value can be assigned and read');
    Check(not LEntry.Empty, 'assigning a value collapses the entry');

    LEntry.Reset;
    Check(LEntry.Empty, 'reset makes the entry empty');
    Check(LEntry.Value = '', 'reset clears the entry value');
    Check(not Assigned(LEntry[gdNorth]), 'reset clears neighbors');
  finally
    LNeighbor.Free;
    LEntry.Free;
  end;
end;

procedure TestRuleGroup;
var
  LGroup: TGraphRuleGroup;
begin
  LGroup := TGraphRuleGroup.Create;
  try
    LGroup
      .NewRule([gdNorth], 'north-value')
      .NewRule([gdEast], 'east-value');

    Check(LGroup.Exists[gdNorth], 'a north rule can be added');
    Check(LGroup.Exists[gdEast], 'an east rule can be added fluently');
    Check(ContainsGraphValue(LGroup[gdNorth].Value, 'north-value'),
      'the north rule retains its allowed value');
    Check(ContainsGraphValue(LGroup[gdEast].Value, 'east-value'),
      'the east rule retains its allowed value');

    LGroup.NewRule([gdNorth], 'second-north-value', True);
    Check(Length(LGroup.Rules) = 2,
      'adding the same direction updates rather than duplicates it');
    Check(ContainsGraphValue(LGroup[gdNorth].Value,
      'second-north-value'), 'a directional upsert retains the new value');
    Check(LGroup[gdNorth].Info,
      'a directional upsert promotes the required flag');

    LGroup.NewRule([gdUp], 'required-value', True);
    Check(LGroup.HasRequired, 'required rules are reported by the group');
    Check(LGroup[gdUp].Info, 'the required flag is retained');
  finally
    LGroup.Free;
  end;
end;

procedure TestCompatibilityTypes;
var
  LList: TGraph.TPlanesList;
  LRule: TGraphRule;
  LValues: TGraphValues;
  {$IFNDEF PAS2JS}
  LOriginalList: TObjectList<TGraph.TPlanes>;
  LOriginalPlanes: TDictionary<TGraph.Z, TGraph.TPlane>;
  LOriginalRule: TPair<TGraphDirection, TGraphValues, TRequireRule>;
  LGraph: TGraph;
  {$ENDIF}
begin
  SetLength(LValues, 1);
  LValues[0] := 'allowed';
  {$IFDEF PAS2JS}
  LRule := TGraphRule.Create(gdWest, LValues, True);
  {$ELSE}
  LRule.Key := gdWest;
  LRule.Value := LValues;
  LRule.Info := True;
  {$ENDIF}
  Check(LRule.Key = gdWest,
    'TGraphRule preserves directional metadata');
  Check(ContainsGraphValue(LRule.Value, 'allowed'),
    'TGraphRule stores values');
  Check(LRule.Info, 'TGraphRule stores required metadata');

  LList := TGraph.TPlanesList.Create(False);
  try
    Check(LList.Count = 0,
      'the original TPlanesList public type remains constructible');
  finally
    LList.Free;
  end;

  {$IFNDEF PAS2JS}
  LOriginalRule := LRule;
  LRule := LOriginalRule;
  Check(LRule.Info,
    'native TGraphRule retains its original three-parameter TPair identity');

  LGraph := TGraph.Create;
  try
    LOriginalPlanes := LGraph.Planes;
    Check(Assigned(LOriginalPlanes),
      'native Planes retains its original TDictionary identity');
  finally
    LGraph.Free;
  end;

  LOriginalList := TObjectList<TGraph.TPlanes>.Create(False);
  LList := LOriginalList;
  try
    Check(LList.Count = 0,
      'native TPlanesList retains its original TObjectList identity');
  finally
    LList.Free;
  end;
  {$ENDIF}
end;

procedure TestInverseRules;
var
  LGraph: TGraph;
  LGroup: TGraphRuleGroup;
  LRule: TGraphRule;
begin
  LGraph := TGraph.Create;
  try
    LGraph.AddValue('A').NewRule([gdEast], 'B');
    LGraph.Rules['A'].NewRule([gdNorth], 'B');

    LGroup := LGraph.Rules['B'];
    Check(LGroup.Exists[gdWest],
      'adding a value creates its inverse directional rule');
    LRule := LGroup.Rule[gdWest];
    Check(ContainsGraphValue(LRule.Value, 'A'),
      'the first inverse rule points back to its source value');
    Check(LGroup.Exists[gdSouth],
      'adding a direction to an existing value also creates its inverse');
    LRule := LGroup.Rule[gdSouth];
    Check(ContainsGraphValue(LRule.Value, 'A'),
      'the later inverse rule points back to its source value');

    LGraph.AddValue('D').NewRule([gdEast], 'B');
    LGraph.Rules['A']
      .NewRule([gdEast], 'C', True);
    LRule := LGraph.Rules['A'].Rule[gdEast];
    Check(LRule.Info,
      'a required upsert promotes the complete directional rule');
    LRule := LGraph.Rules['B'].Rule[gdWest];
    Check(LRule.Info,
      'promoting a direction also promotes its existing inverse edge');
    LRule := LGraph.Rules['C'].Rule[gdWest];
    Check(LRule.Info,
      'a new inverse edge receives the promoted required flag');
    LRule := LGraph.Rules['D'].Rule[gdEast];
    Check(LRule.Info,
      'required inverse promotion reaches the full symmetric closure');
  finally
    LGraph.Free;
  end;
end;

procedure TestRequiredRules;
var
  LGraph: TGraph;
  LSeed: TGraphEntry;
begin
  LGraph := TGraph.Create.Reshape(2, 2, 2);
  try
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('0')
      .NewRule([gdDown], 'U', True)
      .NewRule([gdWest], 'E', True);
    LGraph.AddValue('fallback');
    LSeed := LGraph.Entry[0, 0, 0];
    LSeed.Value := '0';
    LGraph.SelectionCallback := SelectFirstValid;

    LGraph.Run;

    Check(LSeed[gdEast].Value = 'E',
      'a required west rule forces the east neighbor');
    Check(LSeed[gdUp].Value = 'U',
      'a required down rule forces the upper neighbor');
  finally
    LGraph.Free;
  end;
end;

procedure TestConjunctiveRequiredRules;
var
  LCenter: TGraphEntry;
  LGraph: TTestGraph;
  LPass: TTestGraph;
  LValues: TGraphValues;
begin
  GFailEntryCreateAt := 0;
  LGraph := TTestGraph.Create;
  try
    LGraph.WrapNeighbors := False;
    LGraph.Reshape(3, 3, 1);
    LGraph.AddValue('N').NewRule([gdNorth], 'A', True);
    LGraph.AddValue('E').NewRule([gdEast], 'B', True);
    LGraph.AddValue('fallback');

    LPass := TTestGraph(LGraph.PassGraph[0]);
    LCenter := LPass.Entry[1, 1, 0];
    LPass.Entry[1, 2, 0].Value := 'N';
    LPass.Entry[2, 1, 0].Value := 'E';

    LPass.ValidateForTest(LCenter, LValues);
    Check(Length(LValues) = 0,
      'conflicting required neighbors leave no generated candidate');

    LCenter.Value := 'A';
    LPass.ValidateForTest(LCenter, LValues);
    Check(Length(LValues) = 0,
      'a locked value cannot bypass a conflicting required neighbor');

    LCenter.ClearValue;
    LPass.Rules['N'].NewRule([gdNorth], 'B', True);
    LPass.Rules['E'].NewRule([gdEast], 'C', True);
    LPass.ValidateForTest(LCenter, LValues);
    Check((Length(LValues) = 1) and (LValues[0] = 'B'),
      'overlapping required neighbors retain only their intersection');
  finally
    LGraph.Free;
  end;
end;

procedure TestPreviousConstraintNeedsEarlierPass;
var
  LGraph: TGraph;
  LGroup: TGraphRuleGroup;
  LRaised: Boolean;
begin
  LGraph := TGraph.Create;
  try
    LGroup := LGraph.AddValue('tree');
    LRaised := False;
    try
      LGroup.RequirePrevious('land');
    except
      on E: Exception do
        LRaised := True;
    end;

    Check(LRaised,
      'RequirePrevious rejects pass zero because no earlier pass exists');
    Check(Length(LGroup.PreviousValues) = 0,
      'a rejected pass-zero previous constraint is not stored');

    LGraph.CurrentPass := 'terrain';
    LGraph.SwitchToPass('foliage');
    LGraph.AddValue('tree').RequirePrevious('land');
    Check(ContainsGraphValue(LGraph.Rules['tree'].PreviousValues, 'land'),
      'RequirePrevious remains available on later passes');
  finally
    LGraph.Free;
  end;
end;

procedure TestGraphShapeAndNeighbors;
var
  LGraph: TGraph;
  LEntry: TGraphEntry;
begin
  LGraph := TGraph.Create;
  try
    LGraph.WrapNeighbors := False;
    LGraph.Reshape({width} 2, {height} 2, {depth} 2);

    Check(LGraph.Dimension.Width = 2, 'reshape sets width');
    Check(LGraph.Dimension.Height = 2, 'reshape sets height');
    Check(LGraph.Dimension.Depth = 2, 'reshape sets depth');
    Check(LGraph.Planes.Count = 2, 'reshape creates one plane per Z value');
    Check(LGraph.Planes[0].Count = 4, 'each plane contains width * height entries');

    LEntry := LGraph[0, 0, 0];
    Check(LEntry[gdEast] = LGraph[1, 0, 0], 'east neighbor is wired correctly');
    Check(LEntry[gdNorth] = LGraph[0, 1, 0], 'north neighbor is wired correctly');
    Check(LEntry[gdUp] = LGraph[0, 0, 1], 'up neighbor is wired correctly');
    Check(not Assigned(LEntry[gdWest]), 'west boundary is nil without wrapping');
    Check(not Assigned(LEntry[gdSouth]), 'south boundary is nil without wrapping');
    Check(not Assigned(LEntry[gdDown]), 'down boundary is nil without wrapping');

    LGraph.WrapNeighbors := True;
    Check(LEntry[gdWest] = LGraph[1, 0, 0],
      'enabling wrapping relinks the west boundary');
    Check(LEntry[gdSouth] = LGraph[0, 1, 0],
      'enabling wrapping relinks the south boundary');
    Check(LEntry[gdDown] = LGraph[0, 0, 1],
      'enabling wrapping relinks the lower boundary');

    LGraph.WrapNeighbors := False;
    Check(not Assigned(LEntry[gdWest]),
      'disabling wrapping relinks the boundary back to nil');
  finally
    LGraph.Free;
  end;
end;

procedure TestStablePassSelection;
var
  LGraph: TGraph;
  LIndex: Integer;
begin
  LGraph := TGraph.Create.Reshape(2, 1, 1);
  try
    LGraph.CurrentPass := 'first';
    Check(LGraph.TotalPassCount = 1, 'the initial graph is pass zero');
    Check(LGraph.CurrentPass = 'first', 'the initial pass can be labeled');
    Check(LGraph.CurrentPassIndex = 0, 'the initial pass index is zero');

    LGraph.SwitchToPass('second', LIndex);
    Check(LIndex = 1, 'the second pass receives index one');
    Check(LGraph.CurrentPassIndex = 1, 'switching updates CurrentPassIndex');
    Check(LGraph.TotalPassCount = 2, 'adding a pass updates TotalPassCount');

    LGraph.SwitchToPass('first', LIndex);
    Check(LIndex = 0, 'switching back returns the stable first-pass index');
    Check(LGraph.CurrentPassIndex = 0, 'switching back selects pass zero');

    LGraph.SwitchToPass('second', LIndex);
    Check(LIndex = 1, 'revisiting a pass preserves its index');
    Check(LGraph.CurrentPassIndex = 1, 'revisiting restores index one');
    Check(LGraph.TotalPassCount = 2, 'revisiting does not create a duplicate pass');
    Check(Assigned(LGraph.PassGraph[0]), 'PassGraph exposes pass zero');
    Check(Assigned(LGraph.PassGraph[1]), 'PassGraph exposes pass one');
    Check(LGraph.PassGraph[0] <> LGraph.PassGraph[1],
      'each pass has distinct graph state');
    Check((LGraph.PassGraph[0].CurrentPass = 'first')
      and (LGraph.PassGraph[0].CurrentPassIndex = 0),
      'pass zero reports its own stable identity');
    Check((LGraph.PassGraph[1].CurrentPass = 'second')
      and (LGraph.PassGraph[1].CurrentPassIndex = 1),
      'pass one reports its own stable identity');
  finally
    LGraph.Free;
  end;
end;

procedure TestForEachPass;
var
  LGraph: TGraph;
begin
  LGraph := TGraph.Create.Reshape(2, 1, 1);
  try
    LGraph.CurrentPass := 'terrain';
    LGraph.SwitchToPass('foliage');
    ResetCapturedPasses;

    LGraph.ForEachPass(CapturePassAndSwitch);

    Check(GCapturedPassCount = 2, 'ForEachPass visits every pass');
    Check(GCapturedPassNames[0] = 'terrain', 'ForEachPass starts with terrain');
    Check(GCapturedPassNames[1] = 'foliage', 'ForEachPass then visits foliage');
    Check(GCapturedPassIndices[0] = 0, 'ForEachPass reports index zero first');
    Check(GCapturedPassIndices[1] = 1, 'ForEachPass reports index one second');
    Check(GCapturedPassGraphs[0] = LGraph.PassGraph[0],
      'ForEachPass provides the first pass graph');
    Check(GCapturedPassGraphs[1] = LGraph.PassGraph[1],
      'ForEachPass provides the second pass graph');
    Check((GCapturedPassGraphs[0].CurrentPass = 'terrain')
      and (GCapturedPassGraphs[0].CurrentPassIndex = 0),
      'the first callback graph reports the first pass identity');
    Check((GCapturedPassGraphs[1].CurrentPass = 'foliage')
      and (GCapturedPassGraphs[1].CurrentPassIndex = 1),
      'the second callback graph reports the second pass identity');
    Check((LGraph.CurrentPass = 'foliage') and (LGraph.CurrentPassIndex = 1),
      'ForEachPass restores selection after a callback switches passes');
  finally
    LGraph.Free;
  end;
end;

{$IFNDEF PAS2JS}
procedure TestNativeNestedPassCallback;
var
  LGraph: TGraph;
  LVisited: Integer;

  procedure Visit(const {%H-}AGraph: TGraph; const {%H-}APass: String;
    const {%H-}APassIndex: Integer);
  begin
    Inc(LVisited);
  end;

begin
  LGraph := TGraph.Create;
  try
    LGraph.CurrentPass := 'one';
    LGraph.SwitchToPass('two');
    LVisited := 0;
    //Do not use @ here: the native nested overload carries the captured frame.
    LGraph.ForEachPass(Visit);
    Check(LVisited = 2,
      'the legacy native nested pass callback retains its captured frame');
  finally
    LGraph.Free;
  end;
end;
{$ENDIF}

procedure TestPassScopedState;
var
  LGraph: TGraph;
begin
  LGraph := TGraph.Create.Reshape(2, 1, 1);
  try
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('land');
    LGraph[0, 0, 0].Value := 'land';

    LGraph.SwitchToPass('foliage');
    LGraph.AddValue('tree');

    Check(LGraph.RuleGroups.ContainsKey('tree'),
      'RuleGroups reads from the active pass');
    Check(not LGraph.RuleGroups.ContainsKey('land'),
      'the active pass does not expose another pass rule group');
    Check(LGraph[0, 0, 0].Empty, 'Entry reads from the active pass');
    Check(LGraph.Planes = LGraph.PassGraph[1].Planes,
      'Planes reads from the active pass');

    Check(LGraph.PassGraph[0].RuleGroups.ContainsKey('land'),
      'pass zero retains its own rule groups');
    Check(not LGraph.PassGraph[0].RuleGroups.ContainsKey('tree'),
      'pass zero is isolated from pass one rules');
    Check(LGraph.PassGraph[1].RuleGroups.ContainsKey('tree'),
      'pass one retains its own rule groups');
    Check(not LGraph.PassGraph[1].RuleGroups.ContainsKey('land'),
      'pass one is isolated from pass zero rules');
    Check(LGraph.PassGraph[0].Entry[0, 0, 0].Value = 'land',
      'pass zero retains its own entry values');
    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Empty,
      'pass one retains its own entry values');
  finally
    LGraph.Free;
  end;
end;

procedure TestPassShapeAndWrapPropagation;
var
  LGraph: TGraph;
  I: Integer;
begin
  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.CurrentPass := 'first';
    LGraph.SwitchToPass('second');
    //global topology settings remain global even when called through a
    //specific pass graph
    LGraph.PassGraph[0].WrapNeighbors := False;
    LGraph.PassGraph[1].Reshape(3, 2, 2);
    LGraph.PassGraph[1].Mode := rmTopDown;

    for I := 0 to Pred(LGraph.TotalPassCount) do
    begin
      Check(LGraph.PassGraph[I].Dimension.Width = 3,
        Format('reshape propagates width to pass %d', [I]));
      Check(LGraph.PassGraph[I].Dimension.Height = 2,
        Format('reshape propagates height to pass %d', [I]));
      Check(LGraph.PassGraph[I].Dimension.Depth = 2,
        Format('reshape propagates depth to pass %d', [I]));
      Check(LGraph.PassGraph[I].Planes.Count = 2,
        Format('reshape rebuilds planes for pass %d', [I]));
      Check(not LGraph.PassGraph[I].WrapNeighbors,
        Format('disabled wrapping propagates to pass %d', [I]));
      Check(LGraph.PassGraph[I].Mode = rmTopDown,
        Format('run mode propagates to pass %d', [I]));
    end;

    LGraph.PassGraph[0].WrapNeighbors := True;
    for I := 0 to Pred(LGraph.TotalPassCount) do
      Check(LGraph.PassGraph[I].WrapNeighbors,
        Format('enabled wrapping propagates to pass %d', [I]));
  finally
    LGraph.Free;
  end;
end;

procedure TestSequentialRunAndPassRestore;
var
  LGraph: TGraph;
begin
  LGraph := TGraph.Create.Reshape(2, 1, 1);
  try
    LGraph.CurrentPass := 'base';
    LGraph.AddValue('base-value');

    LGraph.SwitchToPass('derived');
    LGraph.AddValue('derived-value').RequirePrevious(['base-value']);
    GSelectionSwitchCount := 0;
    LGraph.PassGraph[0].SelectionCallback := SelectAndSwitchPass;
    LGraph.PassGraph[1].SelectionCallback := SelectAndSwitchPass;

    LGraph.Run;

    Check(LGraph.PassGraph[0].Entry[0, 0, 0].Value = 'base-value',
      'Run collapses the first pass');
    Check(LGraph.PassGraph[0].Entry[1, 0, 0].Value = 'base-value',
      'Run keeps solving the first pass after a callback switches away');
    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'derived-value',
      'Run executes the dependent pass after the first pass');
    Check(LGraph.PassGraph[1].Entry[1, 0, 0].Value = 'derived-value',
      'Run keeps solving the dependent pass after a callback switches away');
    Check(GSelectionSwitchCount = 4,
      'selection callbacks may switch passes during a run');
    Check((GObservedCallbackPasses[0] = 0)
      and (GObservedCallbackPasses[1] = 0)
      and (GObservedCallbackPasses[2] = 1)
      and (GObservedCallbackPasses[3] = 1),
      'every callback observes the pass currently being solved');
    Check((LGraph.CurrentPass = 'derived') and (LGraph.CurrentPassIndex = 1),
      'Run restores the pass that was selected before execution');
  finally
    LGraph.Free;
  end;
end;

procedure TestRunRestoresRenamedPass;
var
  LGraph: TGraph;
begin
  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.CurrentPass := 'before';
    LGraph.AddValue('value');
    LGraph.SelectionCallback := SelectAndRenamePass;

    LGraph.Run;

    Check((LGraph.CurrentPass = 'after')
      and (LGraph.CurrentPassIndex = 0),
      'Run restores a selected pass by stable index after it is renamed');
    LGraph.SwitchToPass('after');
    Check(LGraph.CurrentPassIndex = 0,
      'a callback rename keeps the pass lookup coherent');
  finally
    LGraph.Free;
  end;
end;

procedure TestRunRejectsPassCreation;
var
  LGraph: TGraph;
  LRaised: Boolean;
begin
  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.CurrentPass := 'existing';
    LGraph.AddValue('value');
    LGraph.SelectionCallback := SelectAndCreatePass;
    LRaised := False;
    try
      LGraph.Run;
    except
      on E: Exception do
        LRaised := True;
    end;

    Check(LRaised,
      'a selection callback cannot add a pass during pipeline execution');
    Check(LGraph.TotalPassCount = 1,
      'rejected pass creation leaves the pass registry unchanged');
    Check((LGraph.CurrentPass = 'existing')
      and (LGraph.CurrentPassIndex = 0),
      'rejected pass creation preserves the selected pass');
  finally
    LGraph.Free;
  end;
end;

procedure TestEmptyPassCopiesPrevious;
var
  LGraph: TGraph;
begin
  LGraph := TGraph.Create.Reshape(2, 1, 1);
  try
    LGraph.CurrentPass := 'source';
    LGraph.AddValue('left');
    LGraph.AddValue('right');
    LGraph[0, 0, 0].Value := 'left';
    LGraph[1, 0, 0].Value := 'right';

    LGraph.SwitchToPass('copy');
    ConfigureDeterministicSelection(LGraph);
    LGraph.Run;

    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'left',
      'an empty pass copies the previous left value');
    Check(LGraph.PassGraph[1].Entry[1, 0, 0].Value = 'right',
      'an empty pass copies the previous right value');
    Check(LGraph.PassGraph[0].Entry[0, 0, 0].Value = 'left',
      'copying does not modify the source pass');
    Check(LGraph.PassGraph[0].Entry[1, 0, 0].Value = 'right',
      'all source values remain unchanged');
    Check((LGraph.CurrentPass = 'copy') and (LGraph.CurrentPassIndex = 1),
      'copying also preserves the selected pass');
    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Generated,
      'copied output is tracked as generated rather than caller-locked');

    LGraph.PassGraph[1].Entry[1, 0, 0].Value := 'local-lock';
    LGraph.PassGraph[0].Entry[0, 0, 0].Value := 'updated';
    LGraph.PassGraph[0].Entry[1, 0, 0].Value := 'updated-right';
    LGraph.Run;
    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'updated',
      'rerunning an empty pass refreshes its copied snapshot');
    Check(LGraph.PassGraph[1].Entry[1, 0, 0].Value = 'local-lock',
      'an empty pass preserves a caller-locked destination value');
    Check(not LGraph.PassGraph[1].Entry[1, 0, 0].Generated,
      'a destination lock remains caller-controlled after copying');
  finally
    LGraph.Free;
  end;
end;

procedure TestTerrainToFoliage;
var
  LGraph: TGraph;
  X: Integer;
  LExpectedTerrain: String;
  LRaised: Boolean;
begin
  LGraph := TGraph.Create.Reshape(4, 1, 1);
  try
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('land');
    LGraph.AddValue('water');

    for X := 0 to 3 do
      if (X mod 2) = 0 then
        LGraph[X, 0, 0].Value := 'land'
      else
        LGraph[X, 0, 0].Value := 'water';

    LGraph.SwitchToPass('foliage');
    LGraph.AddValue('tree').RequirePrevious(['land']);
    LGraph.AddValue('none');
    ConfigureDeterministicSelection(LGraph);

    LGraph.Run;

    for X := 0 to 3 do
    begin
      if (X mod 2) = 0 then
        LExpectedTerrain := 'land'
      else
        LExpectedTerrain := 'water';

      Check(LGraph.PassGraph[0].Entry[X, 0, 0].Value = LExpectedTerrain,
        Format('terrain cell %d remains unchanged', [X]));

      if LExpectedTerrain = 'land' then
        Check(LGraph.PassGraph[1].Entry[X, 0, 0].Value = 'tree',
          Format('land cell %d accepts a tree', [X]))
      else
      begin
        Check(LGraph.PassGraph[1].Entry[X, 0, 0].Value <> 'tree',
          Format('water cell %d rejects a tree', [X]));
        Check(LGraph.PassGraph[1].Entry[X, 0, 0].Value = 'none',
          Format('water cell %d uses the unconstrained fallback', [X]));
      end;
    end;

    Check((LGraph.CurrentPass = 'foliage') and (LGraph.CurrentPassIndex = 1),
      'terrain-to-foliage generation restores the foliage selection');
    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Generated,
      'solver output is marked as generated');

    LGraph.PassGraph[0].Entry[0, 0, 0].Value := 'water';
    LGraph.Run;
    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'none',
      'rerunning clears stale downstream output after terrain changes');

    LGraph.PassGraph[0].Entry[0, 0, 0].Value := 'land';
    LGraph.Run;
    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'tree',
      'rerunning regenerates downstream output when terrain permits it');

    //Assigning the same generated value explicitly promotes it to a lock.
    LGraph.PassGraph[1].Entry[0, 0, 0].Value :=
      LGraph.PassGraph[1].Entry[0, 0, 0].Value;
    Check(not LGraph.PassGraph[1].Entry[0, 0, 0].Generated,
      'an explicit same-value assignment locks generated output');
    LGraph.PassGraph[0].Entry[0, 0, 0].Value := 'water';
    LRaised := False;
    try
      LGraph.Run;
    except
      on E: Exception do
        LRaised := True;
    end;
    Check(LRaised,
      'a locked downstream value cannot silently violate RequirePrevious');

    LGraph.PassGraph[1].Entry[0, 0, 0].ClearValue;
    LGraph.Run;
    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'none',
      'clearing a lock allows the downstream value to regenerate');
  finally
    LGraph.Free;
  end;
end;

procedure TestInvalidSeedRecovery;
var
  LGraph: TGraph;
begin
  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('water');
    LGraph.Entry[0, 0, 0].Value := 'water';

    LGraph.SwitchToPass('foliage');
    LGraph.AddValue('tree').RequirePrevious('land');
    LGraph.AddValue('none');
    LGraph.Entry[0, 0, 0].Value := 'tree';
    LGraph.InvalidStateCallback := ReplaceInvalidWithNone;
    GInvalidRecoveryCount := 0;

    LGraph.Run;

    Check(GInvalidRecoveryCount = 1,
      'an invalid locked value invokes the invalid-state callback');
    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'none',
      'the invalid-state callback can repair a locked pass value');
    Check(not LGraph.PassGraph[1].Entry[0, 0, 0].Generated,
      'a repaired locked value remains caller-controlled');
  finally
    LGraph.Free;
  end;
end;

procedure TestSelectionMustUseValidDomain;
var
  LGraph: TGraph;
  LRaised: Boolean;
begin
  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.AddValue('allowed');
    LGraph.SelectionCallback := SelectOutsideDomain;
    LRaised := False;
    try
      LGraph.Run;
    except
      on E: Exception do
        LRaised := True;
    end;

    Check(LRaised,
      'a selection callback cannot emit a value outside the valid domain');
    Check(LGraph.Entry[0, 0, 0].Empty,
      'an invalid selection is not committed to the graph');

    LGraph.InvalidStateCallback := ReplaceInvalidWithAllowed;
    LGraph.Run;
    Check(LGraph.Entry[0, 0, 0].Value = 'allowed',
      'the invalid-state callback may repair an invalid selection');
  finally
    LGraph.Free;
  end;
end;

procedure TestTransactionalReshape;
var
  LGraph: TTestGraph;
  LRaised: Boolean;
begin
  GFailEntryCreateAt := 0;
  GEntryCreateCount := 0;
  LGraph := TTestGraph.Create;
  LGraph.Reshape(2, 1, 1);
  try
    LGraph.CurrentPass := 'one';
    LGraph.AddValue('A');
    LGraph.Entry[0, 0, 0].Value := 'A';
    LGraph.SwitchToPass('two').AddValue('B');
    LGraph.Entry[0, 0, 0].Value := 'B';

    LRaised := False;
    try
      LGraph.Reshape(High(TGraphCoordinate), 2, 1);
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised, 'an oversized reshape is rejected');
    Check((LGraph.PassGraph[0].Dimension.Width = 2)
      and (LGraph.PassGraph[1].Dimension.Width = 2),
      'a rejected reshape preserves every pass dimension');
    Check((LGraph.PassGraph[0].Entry[0, 0, 0].Value = 'A')
      and (LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'B'),
      'a rejected reshape preserves every pass value');

    GEntryCreateCount := 0;
    GFailEntryCreateAt := 4;
    LRaised := False;
    try
      LGraph.Reshape(3, 1, 1);
    except
      on E: Exception do
        LRaised := True;
    end;
    GFailEntryCreateAt := 0;
    Check(LRaised, 'an entry-factory failure aborts reshape');
    Check((LGraph.PassGraph[0].Dimension.Width = 2)
      and (LGraph.PassGraph[1].Dimension.Width = 2),
      'a factory failure cannot split pass dimensions');
    Check((LGraph.PassGraph[0].Entry[0, 0, 0].Value = 'A')
      and (LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'B'),
      'a factory failure leaves committed values untouched');
  finally
    GFailEntryCreateAt := 0;
    LGraph.Free;
  end;
end;

procedure TestSubclassPassFactory;
var
  LGraph: TTestGraph;
begin
  GPassFactoryCount := 0;
  GPassInitializeCount := 0;
  GFailPassInitializeAt := 0;
  GFailEntryCreateAt := 0;
  LGraph := TTestGraph.Create;
  try
    Check(GPassFactoryCount = 0,
      'pass-zero creation is deferred until the graph is first used');
    Check(LGraph.TotalPassCount = 1,
      'first use materializes the default pass');
    Check(GPassFactoryCount = 1,
      'pass zero is created through the virtual pass factory');
    Check(TTestGraph(LGraph.PassGraph[0]).PassInitialized,
      'pass zero runs the subclass pass-initialization hook');

    LGraph.CurrentPass := 'one';
    LGraph.SwitchToPass('two');
    Check(GPassFactoryCount = 2,
      'later passes use the same virtual pass factory');
    Check(TTestGraph(LGraph.PassGraph[1]).PassInitialized,
      'later passes run the subclass pass-initialization hook');

    LGraph.Reset;
    Check(GPassFactoryCount = 3,
      'reset uses the same virtual pass factory');
    Check(TTestGraph(LGraph.PassGraph[0]).PassInitialized,
      'the reset pass runs the subclass pass-initialization hook');
  finally
    GFailPassInitializeAt := 0;
    LGraph.Free;
  end;
end;

procedure TestConfiguredSubclassLifecycle;
var
  LGraph: TConfiguredGraph;
  LPass: TConfiguredGraph;
begin
  LGraph := TConfiguredGraph.Create('configured');
  try
    LPass := TConfiguredGraph(LGraph.PassGraph[0]);
    Check(LPass.InitializedConfiguration = 'configured',
      'pass-zero creation sees parameterized subclass configuration');
    Check(LPass.RegisteredDuringInitialization,
      'pass zero is registered before its initialization hook runs');

    LGraph.CurrentPass := 'one';
    LGraph.SwitchToPass('two');
    LPass := TConfiguredGraph(LGraph.PassGraph[1]);
    Check(LPass.InitializedConfiguration = 'configured',
      'later passes receive the same subclass configuration');
    Check(LPass.RegisteredDuringInitialization,
      'later passes are registered before their initialization hook runs');
  finally
    LGraph.Free;
  end;
end;

procedure TestResetClearsPipeline;
var
  LGraph: TGraph;
  LPass: TGraph;
  LRaised: Boolean;
begin
  LGraph := TGraph.Create.Reshape(2, 2, 1);
  try
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('land');
    LGraph.SwitchToPass('foliage').AddValue('tree');
    LGraph.PassGraph[0].SelectionCallback := SelectLastValid;
    LGraph.PassGraph[0].InvalidStateCallback := ReplaceInvalidWithNone;

    LPass := LGraph.PassGraph[1];
    LRaised := False;
    try
      LPass.Reset;
    except
      on E: Exception do
        LRaised := True;
    end;
    Check(LRaised, 'Reset rejects a pass-local receiver');
    Check((LGraph.TotalPassCount = 2)
      and (LGraph.Dimension.Width = 2),
      'a rejected child Reset leaves the pipeline intact');

    LGraph.Reset;

    Check(LGraph.TotalPassCount = 1,
      'reset removes every additional pass');
    Check((LGraph.CurrentPass = '') and (LGraph.CurrentPassIndex = 0),
      'reset restores the unnamed first pass');
    Check((LGraph.Dimension.Width = 0)
      and (LGraph.Dimension.Height = 0)
      and (LGraph.Dimension.Depth = 0),
      'reset clears shared dimensions');
    Check(LGraph.RuleGroups.Count = 0,
      'reset clears first-pass values and rules');
    Check(LGraph.Planes.Count = 0,
      'reset clears first-pass planes');
    Check(Assigned(LGraph.SelectionCallback),
      'reset preserves the first-pass selection callback');
    Check(Assigned(LGraph.InvalidStateCallback),
      'reset preserves the first-pass invalid-state callback');

    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('first');
    LGraph.AddValue('last');
    LGraph.Run;
    Check(LGraph.Entry[0, 0, 0].Value = 'last',
      'the preserved selection callback remains active after reset');
  finally
    LGraph.Free;
  end;
end;

procedure TestTransactionalResetFailure;
var
  LGraph: TTestGraph;
  LRaised: Boolean;
begin
  GPassInitializeCount := 0;
  GFailPassInitializeAt := 0;
  LGraph := TTestGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('land');
    LGraph.Entry[0, 0, 0].Value := 'land';
    LGraph.SwitchToPass('foliage').AddValue('tree');
    LGraph.Entry[0, 0, 0].Value := 'tree';

    GFailPassInitializeAt := Succ(GPassInitializeCount);
    LRaised := False;
    try
      LGraph.Reset;
    except
      on E: Exception do
        LRaised := True;
    end;
    GFailPassInitializeAt := 0;

    Check(LRaised, 'a failing reset initialization hook is reported');
    Check(LGraph.TotalPassCount = 2,
      'a failed reset preserves the complete old pass registry');
    Check((LGraph.CurrentPass = 'foliage')
      and (LGraph.CurrentPassIndex = 1),
      'a failed reset restores the prior pass selection');
    Check((LGraph.Dimension.Width = 2)
      and (LGraph.Dimension.Height = 1)
      and (LGraph.Dimension.Depth = 1),
      'a failed reset restores the prior dimensions');
    Check((LGraph.PassGraph[0].Entry[0, 0, 0].Value = 'land')
      and (LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'tree'),
      'a failed reset preserves values in every old pass');
  finally
    GFailPassInitializeAt := 0;
    LGraph.Free;
  end;
end;

procedure TestInitializationMutationGuards;
var
  LGraph: TTestGraph;
  LRaised: Boolean;
begin
  GInitializeNestedPass := False;
  GInitializeRenamePass := False;
  GInitializeSetMode := False;
  GInitializeSetWrap := False;
  GFailPassInitializeAt := 0;
  LGraph := TTestGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.CurrentPass := 'base';
    LGraph.AddValue('base-value');

    GInitializeRenamePass := True;
    LRaised := False;
    try
      LGraph.SwitchToPass('candidate');
    except
      on E: Exception do
        LRaised := True;
    end;
    GInitializeRenamePass := False;
    Check(LRaised,
      'a pass initializer cannot rename its registered pass');
    Check((LGraph.TotalPassCount = 1)
      and (LGraph.CurrentPass = 'base'),
      'a rejected initializer rename leaves no stale pass identity');

    GInitializeNestedPass := True;
    LRaised := False;
    try
      LGraph.SwitchToPass('candidate');
    except
      on E: Exception do
        LRaised := True;
    end;
    GInitializeNestedPass := False;
    Check(LRaised,
      'a pass initializer cannot create a nested pass');
    Check(LGraph.TotalPassCount = 1,
      'rejected nested creation rolls back the candidate pass');

    LGraph.SwitchToPass('candidate');
    Check((LGraph.TotalPassCount = 2)
      and (LGraph.CurrentPassIndex = 1),
      'a rolled-back pass label can be created normally afterward');

    LGraph.Mode := rmBottomUp;
    LGraph.WrapNeighbors := True;
    GInitializeSetMode := True;
    LRaised := False;
    try
      LGraph.Reset;
    except
      on E: Exception do
        LRaised := True;
    end;
    GInitializeSetMode := False;
    Check(LRaised,
      'a reset initializer cannot mutate the pipeline run mode');
    Check((LGraph.Mode = rmBottomUp)
      and (LGraph.PassGraph[0].Mode = rmBottomUp)
      and (LGraph.PassGraph[1].Mode = rmBottomUp),
      'failed reset preserves the shared run mode on root and passes');

    GInitializeSetWrap := True;
    LRaised := False;
    try
      LGraph.Reset;
    except
      on E: Exception do
        LRaised := True;
    end;
    GInitializeSetWrap := False;
    Check(LRaised,
      'a reset initializer cannot mutate neighbor wrapping');
    Check(LGraph.WrapNeighbors
      and LGraph.PassGraph[0].WrapNeighbors
      and LGraph.PassGraph[1].WrapNeighbors,
      'failed reset preserves wrapping and the old pass topology');
  finally
    GInitializeNestedPass := False;
    GInitializeRenamePass := False;
    GInitializeSetMode := False;
    GInitializeSetWrap := False;
    GFailPassInitializeAt := 0;
    LGraph.Free;
  end;
end;

begin
  WriteLn('WFC conformance suite');
  WriteLn('=====================');

  RunTest('graph entry lifecycle', @TestGraphEntry);
  RunTest('rule group basics', @TestRuleGroup);
  RunTest('public compatibility types', @TestCompatibilityTypes);
  RunTest('inverse rule generation', @TestInverseRules);
  RunTest('required rule enforcement', @TestRequiredRules);
  RunTest('conjunctive required constraints', @TestConjunctiveRequiredRules);
  RunTest('previous-pass constraint placement',
    @TestPreviousConstraintNeedsEarlierPass);
  RunTest('graph shape and neighbors', @TestGraphShapeAndNeighbors);
  RunTest('stable pass selection', @TestStablePassSelection);
  RunTest('portable pass iteration', @TestForEachPass);
  {$IFNDEF PAS2JS}
  RunTest('native nested pass callback', @TestNativeNestedPassCallback);
  {$ENDIF}
  RunTest('pass-scoped state', @TestPassScopedState);
  RunTest('pass shape and wrapping propagation', @TestPassShapeAndWrapPropagation);
  RunTest('sequential run and pass restoration', @TestSequentialRunAndPassRestore);
  RunTest('run restoration after pass rename', @TestRunRestoresRenamedPass);
  RunTest('run rejects pass creation', @TestRunRejectsPassCreation);
  RunTest('empty pass copies previous output', @TestEmptyPassCopiesPrevious);
  RunTest('terrain to foliage constraint', @TestTerrainToFoliage);
  RunTest('invalid locked pass recovery', @TestInvalidSeedRecovery);
  RunTest('selection callback domain validation',
    @TestSelectionMustUseValidDomain);
  RunTest('transactional reshape', @TestTransactionalReshape);
  RunTest('subclass pass factory', @TestSubclassPassFactory);
  RunTest('configured subclass lifecycle', @TestConfiguredSubclassLifecycle);
  RunTest('reset clears the pass pipeline', @TestResetClearsPipeline);
  RunTest('transactional reset failure', @TestTransactionalResetFailure);
  RunTest('pass initialization mutation guards',
    @TestInitializationMutationGuards);

  WriteLn('=====================');
  WriteLn(Format('%d checks, %d failures', [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d WFC checks failed', [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
