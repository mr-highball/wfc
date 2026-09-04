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
program wfc_test;

{$Mode delphi}{$H+}
{$IFNDEF PAS2JS}
{$ModeSwitch nestedprocvars}
{$ENDIF}

uses
  Classes,
  SysUtils,
  Generics.Collections,
  wfc_solver_reference,
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

  TInvalidStartGraph = class(TGraph)
  strict protected
    procedure DoGetStartCoord(out X, Y: TGraphCoordinate); override;
  end;

  TFixedStartGraph = class(TGraph)
  strict protected
    procedure DoGetStartCoord(out X, Y: TGraphCoordinate); override;
  end;

  TCommitFailEntry = class(TGraphEntry)
  strict protected
    procedure DoBeforeSetValue(const AValue: TGraphValue); override;
  end;

  TCommitFailGraph = class(TGraph)
  strict protected
    function DoCreateEntry: TGraphEntry; override;
  end;

  TCommitMutateEntry = class(TGraphEntry)
  strict protected
    procedure DoAfterSetValue(const AValue: TGraphValue); override;
  end;

  TCommitMutateGraph = class(TGraph)
  strict protected
    function DoCreateEntry: TGraphEntry; override;
  end;

  TCommitIdentityEntry = class(TGraphEntry)
  strict protected
    procedure DoAfterSetValue(const AValue: TGraphValue); override;
  end;

  TCommitIdentityGraph = class(TGraph)
  strict protected
    function DoCreateEntry: TGraphEntry; override;
  end;

  TReferenceRandomProbe = class
  strict private
    FCallCount: Integer;
    FCounts: array[0..15] of Integer;
    function GetCount(const AIndex: Integer): Integer;
  public
    property CallCount: Integer read FCallCount;
    property Count[const AIndex: Integer]: Integer read GetCount;
    function RandomIndex(const ACount: Integer): Integer;
    procedure Reset;
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
  GInitializeSetSeed: Boolean = False;
  GInitializeSetWrap: Boolean = False;
  GEntryCreateCount: Integer = 0;
  GFailEntryCreateAt: Integer = 0;
  GTraversalCount: Integer = 0;
  GTraversalIndices: array[0..Pred(MAX_CAPTURED_PASSES)] of Integer;
  GPassExecutionCount: Integer = 0;
  GPassExecutionIndices: array[0..Pred(MAX_CAPTURED_PASSES)] of Integer;
  GCommitSetCount: Integer = 0;
  GFailCommitSetAt: Integer = 0;
  GCommitMutationSourceIndex: Integer = -1;
  GCommitMutationTarget: TGraphEntry = nil;
  GCommitIdentityGraph: TGraph = nil;
  GCommitIdentityCount: Integer = 0;
  GCommitIdentitySwitchSelection: Boolean = False;
  GCommitIdentityPasses: array[0..Pred(MAX_CAPTURED_PASSES)] of Integer;
  GCommitRandomGraph: TGraph = nil;
  GCommitRandomPassIndex: Integer = -1;
  GCommitRandomDrawCount: Integer = 0;
  GCommitDomainGraph: TGraph = nil;
  GCommitDomainMutationCount: Integer = 0;
  GCommitResetEntryEnabled: Boolean = False;
  GCommitResetEntryCount: Integer = 0;
  GInvalidResetEntryCount: Integer = 0;
  GCapturedWeightCount: Integer = 0;
  GCapturedWeights: array[0..Pred(MAX_CAPTURED_PASSES)] of TGraphWeight;
  GSpatialExactMutationRejected: Boolean = False;
  GSpatialAnyMutationRejected: Boolean = False;

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
  if GInitializeSetSeed then
    Seed := $12345678;
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

procedure TInvalidStartGraph.DoGetStartCoord(out X, Y: TGraphCoordinate);
begin
  X := Dimension.Width;
  Y := 0;
end;

procedure TFixedStartGraph.DoGetStartCoord(out X, Y: TGraphCoordinate);
begin
  X := 0;
  Y := 0;
end;

procedure TCommitFailEntry.DoBeforeSetValue(const AValue: TGraphValue);
begin
  inherited DoBeforeSetValue(AValue);
  Inc(GCommitSetCount);
  if (GFailCommitSetAt > 0)
    and (GCommitSetCount = GFailCommitSetAt) then
    raise Exception.Create('intentional reference commit failure');
end;

function TCommitFailGraph.DoCreateEntry: TGraphEntry;
begin
  Result := TCommitFailEntry.Create;
end;

procedure TCommitMutateEntry.DoAfterSetValue(const AValue: TGraphValue);
begin
  inherited DoAfterSetValue(AValue);
  if (Index = GCommitMutationSourceIndex)
    and Assigned(GCommitMutationTarget) then
    GCommitMutationTarget.Value := 'outside';
end;

function TCommitMutateGraph.DoCreateEntry: TGraphEntry;
begin
  Result := TCommitMutateEntry.Create;
end;

procedure TCommitIdentityEntry.DoAfterSetValue(const AValue: TGraphValue);
begin
  inherited DoAfterSetValue(AValue);
  if GCommitResetEntryEnabled then
  begin
    Inc(GCommitResetEntryCount);
    Reset;
  end;
  if Assigned(GCommitDomainGraph) then
  begin
    Inc(GCommitDomainMutationCount);
    GCommitDomainGraph.SetAllowedValues(0, 0, 0, 'B');
  end;
  if Assigned(GCommitRandomGraph) and (GCommitRandomPassIndex >= 0) then
  begin
    GCommitRandomGraph.PassGraph[GCommitRandomPassIndex].RandomIndex(1000);
    Inc(GCommitRandomDrawCount);
  end;
  if Assigned(GCommitIdentityGraph)
    and (GCommitIdentityCount < MAX_CAPTURED_PASSES) then
  begin
    GCommitIdentityPasses[GCommitIdentityCount] :=
      GCommitIdentityGraph.CurrentPassIndex;
    Inc(GCommitIdentityCount);
    if GCommitIdentitySwitchSelection
      and (GCommitIdentityGraph.TotalPassCount > 1) then
      if GCommitIdentityGraph.CurrentPassIndex = 0 then
        GCommitIdentityGraph.SwitchToPass(1)
      else
        GCommitIdentityGraph.SwitchToPass(0);
  end;
end;

function TCommitIdentityGraph.DoCreateEntry: TGraphEntry;
begin
  Result := TCommitIdentityEntry.Create;
end;

function TReferenceRandomProbe.GetCount(const AIndex: Integer): Integer;
begin
  if (AIndex < 0) or (AIndex >= FCallCount)
    or (AIndex > High(FCounts)) then
    raise ERangeError.CreateFmt(
      'random probe index is out of bounds [%d]', [AIndex]);
  Result := FCounts[AIndex];
end;

function TReferenceRandomProbe.RandomIndex(const ACount: Integer): Integer;
begin
  if FCallCount <= High(FCounts) then
    FCounts[FCallCount] := ACount;
  Inc(FCallCount);
  Result := Pred(ACount);
end;

procedure TReferenceRandomProbe.Reset;
var
  I: Integer;
begin
  FCallCount := 0;
  for I := 0 to High(FCounts) do
    FCounts[I] := -1;
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

function GraphValuesAre(const AActual: TGraphValues;
  const AExpected: array of TGraphValue): Boolean;
var
  I: Integer;
begin
  if Length(AActual) <> Length(AExpected) then
    Exit(False);
  for I := 0 to High(AActual) do
    if AActual[I] <> AExpected[I] then
      Exit(False);
  Result := True;
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

function SelectFirstAndCaptureWeights(const AGraph: TGraph;
  const AEntry: TGraphEntry; const AValid: TGraphValues): TGraphValue;
var
  I: Integer;
begin
  GCapturedWeightCount := Length(AValid);
  for I := 0 to High(AValid) do
    if I <= High(GCapturedWeights) then
      GCapturedWeights[I] := AGraph.Rules[AValid[I]].Weight;
  if Length(AValid) = 0 then
    Result := AEntry.Value
  else
    Result := AValid[0];
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

function SelectRandomAfterSwitch(const AGraph: TGraph;
  const {%H-}AEntry: TGraphEntry;
  const AValid: TGraphValues): TGraphValue;
begin
  if AGraph.CurrentPassIndex = 0 then
    AGraph.SwitchToPass(1)
  else
    AGraph.SwitchToPass(0);
  Result := AValid[AGraph.RandomIndex(Length(AValid))];
end;

function SelectAndMutateSeed(const AGraph: TGraph;
  const {%H-}AEntry: TGraphEntry;
  const AValid: TGraphValues): TGraphValue;
begin
  AGraph.Seed := $12345678;
  Result := AValid[0];
end;

function SelectAndMutateAllowedValues(const AGraph: TGraph;
  const {%H-}AEntry: TGraphEntry;
  const AValid: TGraphValues): TGraphValue;
begin
  AGraph.SetAllowedValues(0, 0, 0, 'B');
  Result := AValid[0];
end;

function SelectAndAttemptSpatialMutation(const AGraph: TGraph;
  const {%H-}AEntry: TGraphEntry;
  const AValid: TGraphValues): TGraphValue;
begin
  try
    AGraph.Rules['candidate'].RequireFromPassAt('source',
      MakeGraphOffset(0, 0, 0), 'A');
  except
    on E: EInvalidOperation do
      GSpatialExactMutationRejected := True;
  end;
  try
    AGraph.Rules['candidate'].RequireAnyFromPass('source', [
      MakeGraphPassMatchTerm(MakeGraphOffset(0, 0, 0), 'A')]);
  except
    on E: EInvalidOperation do
      GSpatialAnyMutationRejected := True;
  end;
  Result := AValid[0];
end;

function SelectAndCaptureTraversal(const {%H-}AGraph: TGraph;
  const AEntry: TGraphEntry;
  const AValid: TGraphValues): TGraphValue;
begin
  if GTraversalCount < MAX_CAPTURED_PASSES then
    GTraversalIndices[GTraversalCount] := AEntry.Index;
  Inc(GTraversalCount);
  Result := AValid[0];
end;

function SelectAndCapturePassExecution(const AGraph: TGraph;
  const {%H-}AEntry: TGraphEntry;
  const AValid: TGraphValues): TGraphValue;
begin
  if GPassExecutionCount < MAX_CAPTURED_PASSES then
    GPassExecutionIndices[GPassExecutionCount] :=
      AGraph.CurrentPassIndex;
  Inc(GPassExecutionCount);
  if Length(AValid) = 0 then
    Result := TGraphValue.Empty
  else
    Result := AValid[0];
end;

function SelectAndMutateNeighbor(const AGraph: TGraph;
  const AEntry: TGraphEntry;
  const AValid: TGraphValues): TGraphValue;
var
  LRootEntry: TGraphEntry;
begin
  if AEntry.Index = 2 then
  begin
    LRootEntry := AGraph.Entry[0, 0, 0];
    LRootEntry[gdEast] := nil;
  end;
  Result := AValid[0];
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

procedure MutateInvalidEntryWithInvented(const {%H-}AGraph: TGraph;
  const AEntry: TGraphEntry; var AValue: TGraphValue);
begin
  AEntry.Value := 'invented';
  AValue := 'invented';
end;

procedure ResetInvalidEntryAndProposeAllowed(const {%H-}AGraph: TGraph;
  const AEntry: TGraphEntry; var AValue: TGraphValue);
begin
  Inc(GInvalidResetEntryCount);
  AEntry.Reset;
  AValue := 'A';
end;

procedure RepairEmptyDomainWithNone(const AGraph: TGraph;
  const {%H-}AEntry: TGraphEntry; var AValue: TGraphValue);
begin
  AGraph.AddValue('none');
  AValue := 'none';
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

function SnapshotPass(const AGraph: TGraph;
  const APassIndex: Integer): String;
var
  X, Y, Z: Integer;
  LPass: TGraph;
begin
  Result := '';
  LPass := AGraph.PassGraph[APassIndex];
  for Z := 0 to Integer(LPass.Dimension.Depth) - 1 do
  begin
    for Y := 0 to Integer(LPass.Dimension.Height) - 1 do
    begin
      for X := 0 to Integer(LPass.Dimension.Width) - 1 do
        Result := Result + LPass.Entry[X, Y, Z].Value;
      Result := Result + '/';
    end;
    Result := Result + '|';
  end;
end;

function SnapshotPipeline(const AGraph: TGraph): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Pred(AGraph.TotalPassCount) do
    Result := Result + IntToStr(I) + ':' + SnapshotPass(AGraph, I) + '#';
end;

function SnapshotPassState(const AGraph: TGraph;
  const APassIndex: Integer): String;
var
  I: Integer;
  LEntry: TGraphEntry;
  LPass: TGraph;
begin
  Result := '';
  LPass := AGraph.PassGraph[APassIndex];
  for I := 0 to (Integer(LPass.Dimension.Width)
    * Integer(LPass.Dimension.Height)
    * Integer(LPass.Dimension.Depth)) - 1 do
  begin
    LEntry := LPass.Entry[
      I mod Integer(LPass.Dimension.Width),
      (I div Integer(LPass.Dimension.Width))
        mod Integer(LPass.Dimension.Height),
      I div (Integer(LPass.Dimension.Width)
        * Integer(LPass.Dimension.Height))];
    Result := Result + IntToStr(Length(LEntry.Value)) + ':'
      + LEntry.Value + ':' + IntToStr(Ord(LEntry.Empty)) + ':'
      + IntToStr(Ord(LEntry.Generated)) + ';';
  end;
end;

function SnapshotPipelineState(const AGraph: TGraph): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Pred(AGraph.TotalPassCount) do
    Result := Result + IntToStr(I) + '={'
      + SnapshotPassState(AGraph, I) + '};';
end;

function DependencySnapshot(const AGraph: TGraph;
  const APassIndex: Integer): String;
var
  I: Integer;
  LPass: TGraph;
begin
  Result := '';
  LPass := AGraph.PassGraph[APassIndex];
  for I := 0 to Pred(LPass.DependencyCount) do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + IntToStr(LPass.DependencyIndex[I]);
  end;
end;

function ExecutionOrderSnapshot(const AReport: TGraphSolveReport): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AReport.ExecutionOrder) do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + IntToStr(AReport.ExecutionOrder[I]);
  end;
end;

function NewSeededFixture(const ASeed: TGraphSeed): TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := ASeed;
  Result.Reshape(5, 3, 2);
  Result.WrapNeighbors := False;
  Result.CurrentPass := 'random-grid';
  Result.AddValue('Q');
  Result.AddValue('A');
  Result.AddValue('Z');
end;

function NewSeededPassFixture(const ASeed: TGraphSeed): TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := ASeed;
  Result.Reshape(6, 2, 1);
  Result.WrapNeighbors := False;
  Result.CurrentPass := 'terrain';
  Result.AddValue('L');
  Result.AddValue('W');
  Result.SwitchToPass('foliage');
  Result.AddValue('T').RequirePrevious('L');
  Result.AddValue('.');
  Result.SwitchToPass('copy');
end;

function NewTwoPassRandomFixture(const ASeed: TGraphSeed): TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := ASeed;
  Result.Reshape(4, 2, 1);
  Result.WrapNeighbors := False;
  Result.CurrentPass := 'first';
  Result.AddValue('A');
  Result.AddValue('B');
  Result.SwitchToPass('second');
  Result.AddValue('X');
  Result.AddValue('Y');
end;

function NewReferenceEqualityFixture: TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := 0;
  Result.Reshape(3, 1, 1);
  Result.WrapNeighbors := False;
  Result.AddValue('A').NewRule([gdEast, gdWest], 'A');
  Result.AddValue('B').NewRule([gdEast, gdWest], 'B');
  Result.Entry[0, 0, 0].Value := 'A';
end;

function NewReferenceEscapeRing: TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := 0;
  Result.Reshape(3, 1, 1);
  Result.AddValue('A').NewRule([gdEast, gdWest], 'B');
  Result.AddValue('C').NewRule([gdEast, gdWest], 'C');
end;

function NewReferenceOddRing: TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := 0;
  Result.Reshape(3, 1, 1);
  Result.AddValue('A').NewRule([gdEast, gdWest], 'B');
end;

function NewWeightedScaleFixture(const AFirstWeight,
  ASecondWeight: TGraphWeight): TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := 1;
  Result.Reshape(1, 1, 1);
  Result.WrapNeighbors := False;
  Result.AddValue('A', AFirstWeight);
  Result.AddValue('B', ASecondWeight);
end;

function NewWeightedEntropyFixture: TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := 0;
  Result.Reshape(2, 1, 1);
  Result.WrapNeighbors := False;
  Result.Entry[0, 0, 0].Value := '2';
  Result.Entry[1, 0, 0].Value := '3';
  Result.SwitchToPass('choices');
  Result.AddValue('A', 1).RequirePrevious(['2', '3']);
  Result.AddValue('B', 1).RequirePrevious(['2', '3']);
  Result.AddValue('C', 100).RequirePrevious('3');
end;

function NewEqualWeightMrvFixture(const AWeight: TGraphWeight): TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := 0;
  Result.Reshape(2, 1, 1);
  Result.WrapNeighbors := False;
  Result.Entry[0, 0, 0].Value := '3';
  Result.Entry[1, 0, 0].Value := '2';
  Result.SwitchToPass('choices');
  Result.AddValue('A', AWeight).RequirePrevious(['3', '2']);
  Result.AddValue('B', AWeight).RequirePrevious(['3', '2']);
  Result.AddValue('C', AWeight).RequirePrevious('3');
end;

function NewWeightedEscapeRing(const AWeight, CWeight: TGraphWeight): TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := 0;
  Result.Reshape(3, 1, 1);
  Result.AddValue('A', AWeight).NewRule([gdEast, gdWest], 'B');
  Result.AddValue('C', CWeight).NewRule([gdEast, gdWest], 'C');
end;

function NewWeightedPassFixture: TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := 0;
  Result.Reshape(1, 1, 1);
  Result.WrapNeighbors := False;
  Result.CurrentPass := 'first';
  Result.AddValue('A', 1);
  Result.AddValue('B', 3);
  Result.SwitchToPass('second');
  Result.AddValue('X', 3);
  Result.AddValue('Y', 1);
  Result.SwitchToPass('copy');
end;

function NewTopologicalDagFixture: TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := 0;
  Result.Reshape(1, 1, 1);
  Result.WrapNeighbors := False;

  Result.CurrentPass := 'root';
  Result.PassMode := gpmOverlay;
  Result.AddValue('R');

  Result.SwitchToPass('late-low');
  Result.PassMode := gpmOverlay;
  Result.ClearDependencies;
  Result.AddValue('L');

  Result.SwitchToPass('early-ready');
  Result.PassMode := gpmOverlay;
  Result.ClearDependencies;
  Result.AddValue('E');

  Result.SwitchToPass('producer');
  Result.PassMode := gpmOverlay;
  Result.ClearDependencies;
  Result.AddValue('P');

  Result.SwitchToPass('later-ready');
  Result.PassMode := gpmOverlay;
  Result.ClearDependencies;
  Result.AddValue('A');

  Result.SwitchToPass('late-low').DependsOn('producer');
  Result.SwitchToPass('early-ready').DependsOn('root');
  Result.SwitchToPass('producer').DependsOn('root');
  Result.SwitchToPass('later-ready').DependsOn('root');
end;

function NewNamedPassFixture: TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := 0;
  Result.Reshape(4, 1, 1);
  Result.WrapNeighbors := False;

  Result.CurrentPass := 'terrain';
  Result.PassMode := gpmOverlay;
  Result.AddValue('land');
  Result.AddValue('sand');
  Result.AddValue('water');
  Result.Entry[0, 0, 0].Value := 'land';
  Result.Entry[1, 0, 0].Value := 'sand';
  Result.Entry[2, 0, 0].Value := 'land';
  Result.Entry[3, 0, 0].Value := 'water';

  Result.SwitchToPass('target');
  Result.PassMode := gpmOverlay;
  Result.ClearDependencies;

  Result.SwitchToPass('climate');
  Result.PassMode := gpmOverlay;
  Result.ClearDependencies;
  Result.AddValue('warm');
  Result.AddValue('cold');
  Result.Entry[0, 0, 0].Value := 'warm';
  Result.Entry[1, 0, 0].Value := 'warm';
  Result.Entry[2, 0, 0].Value := 'cold';
  Result.Entry[3, 0, 0].Value := 'warm';

  Result.SwitchToPass('target');
  Result.AddValue('A')
    .RequireFromPass('terrain', ['land', 'sand'])
    .RequireFromPass('terrain', 'sand')
    .RequireFromPass('climate', 'warm');
  Result.AddValue('B')
    .RequireFromPass('terrain', 'land')
    .RequireFromPass('climate', 'cold');
  Result.AddValue('C')
    .RequireFromPass('terrain', 'water')
    .RequireFromPass('climate', 'warm');
end;

function NewSelectiveDagFixture: TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := $12345678;
  Result.Reshape(1, 1, 1);
  Result.WrapNeighbors := False;

  Result.CurrentPass := 'terrain';
  Result.PassMode := gpmOverlay;
  Result.AddValue('land');
  Result.Entry[0, 0, 0].Value := 'land';

  Result.SwitchToPass('hydrology');
  Result.PassMode := gpmOverlay;
  Result.ClearDependencies;
  Result.AddValue('dry').RequireFromPass('terrain', 'land');
  Result.AddValue('river').RequireFromPass('terrain', 'land');
  Result.Entry[0, 0, 0].Value := 'dry';

  Result.SwitchToPass('biome');
  Result.PassMode := gpmOverlay;
  Result.ClearDependencies;
  Result.AddValue('plains').RequireFromPass('terrain', 'land');

  Result.SwitchToPass('roads');
  Result.PassMode := gpmOverlay;
  Result.ClearDependencies;
  Result.AddValue('trail')
    .RequireFromPass('terrain', 'land')
    .RequireFromPass('hydrology', 'dry')
    .RequireFromPass('biome', 'plains');
  Result.AddValue('bridge')
    .RequireFromPass('terrain', 'land')
    .RequireFromPass('hydrology', 'river')
    .RequireFromPass('biome', 'plains');

  Result.SwitchToPass('housing');
  Result.PassMode := gpmOverlay;
  Result.ClearDependencies;
  Result.AddValue('house')
    .RequireFromPass('terrain', 'land')
    .RequireFromPass('hydrology', 'dry')
    .RequireFromPass('biome', 'plains')
    .RequireFromPass('roads', 'trail');
  Result.AddValue('none')
    .RequireFromPass('terrain', 'land')
    .RequireFromPass('hydrology', 'river')
    .RequireFromPass('biome', 'plains')
    .RequireFromPass('roads', 'bridge');

  Result.SwitchToPass('foliage');
  Result.PassMode := gpmOverlay;
  Result.ClearDependencies;
  Result.AddValue('garden')
    .RequireFromPass('terrain', 'land')
    .RequireFromPass('hydrology', 'dry')
    .RequireFromPass('biome', 'plains')
    .RequireFromPass('roads', 'trail')
    .RequireFromPass('housing', 'house');
  Result.AddValue('reeds')
    .RequireFromPass('terrain', 'land')
    .RequireFromPass('hydrology', 'river')
    .RequireFromPass('biome', 'plains')
    .RequireFromPass('roads', 'bridge')
    .RequireFromPass('housing', 'none');
end;

function NewSpatialParityFixture: TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := $2468ACE0;
  Result.Reshape(3, 1, 1);
  Result.WrapNeighbors := False;

  Result.CurrentPass := 'source';
  Result.PassMode := gpmOverlay;
  Result.AddValue('A');
  Result.AddValue('B');
  Result.AddValue('C');
  Result.Entry[0, 0, 0].Value := 'A';
  Result.Entry[1, 0, 0].Value := 'B';
  Result.Entry[2, 0, 0].Value := 'C';

  Result.SwitchToPass('consumer');
  Result.PassMode := gpmOverlay;
  Result.ClearDependencies;
  Result.AddValue('next-A').RequireFromPassAt('source',
    MakeGraphOffset(1, 0, 0), 'A');
  Result.AddValue('next-B').RequireFromPassAt('source',
    MakeGraphOffset(1, 0, 0), 'B');
  Result.AddValue('next-C').RequireFromPassAt('source',
    MakeGraphOffset(1, 0, 0), 'C');
  Result.AddValue('edge').RequireFromPassAt('source',
    MakeGraphOffset(0, 0, 0), 'C');
end;

function NewSpatialAnyParityFixture: TGraph;
begin
  Result := TGraph.Create.Reshape(3, 1, 1);
  Result.WrapNeighbors := False;
  Result.CurrentPass := 'source';
  Result.PassMode := gpmOverlay;
  Result.AddValue('left');
  Result.AddValue('middle');
  Result.AddValue('right');
  Result.Entry[0, 0, 0].Value := 'left';
  Result.Entry[1, 0, 0].Value := 'middle';
  Result.Entry[2, 0, 0].Value := 'right';
  Result.SwitchToPass('consumer');
  Result.PassMode := gpmOverlay;
  Result.ClearDependencies;
  Result.AddValue('choice').RequireAnyFromPass('source', [
    MakeGraphPassMatchTerm(MakeGraphOffset(-1, 0, 0), 'left'),
    MakeGraphPassMatchTerm(MakeGraphOffset(1, 0, 0), 'right')]);
  Result.AddValue('none');
  Result.Entry[1, 0, 0].Value := 'choice';
end;

function NewSpatialAnyExtremeFailureFixture(
  const AWrap: Boolean): TGraph;
var
  LRequired: TGraphValue;
  Y, Z: Integer;
begin
  Result := TGraph.Create.Reshape(1, 4, 4);
  Result.WrapNeighbors := AWrap;
  Result.CurrentPass := 'source';
  Result.PassMode := gpmOverlay;
  Result.AddValue('hit');
  Result.AddValue('miss');
  for Z := 0 to 3 do
    for Y := 0 to 3 do
      Result.Entry[0, Y, Z].Value := 'hit';
  if AWrap then
    LRequired := 'miss'
  else
    LRequired := 'hit';
  Result.SwitchToPass('consumer');
  Result.PassMode := gpmOverlay;
  Result.ClearDependencies;
  Result.AddValue('choice').RequireAnyFromPass('source', [
    MakeGraphPassMatchTerm(MakeGraphOffset(0, Low(Integer), 0),
      LRequired),
    MakeGraphPassMatchTerm(MakeGraphOffset(0, High(Integer), 0),
      LRequired),
    MakeGraphPassMatchTerm(MakeGraphOffset(0, 0, Low(Integer)),
      LRequired),
    MakeGraphPassMatchTerm(MakeGraphOffset(0, 0, High(Integer)),
      LRequired)]);
  Result.AddValue('none');
  Result.Entry[0, 1, 1].Value := 'choice';
end;

function NewSpatialZeroMergeFixture(const AReverse: Boolean): TGraph;
var
  LGroup: TGraphRuleGroup;
begin
  Result := TGraph.Create.Reshape(1, 1, 1);
  Result.WrapNeighbors := False;
  Result.CurrentPass := 'source';
  Result.AddValue('previous');
  Result.AddValue('named');
  Result.AddValue('exact');
  Result.Entry[0, 0, 0].Value := 'exact';
  Result.SwitchToPass('consumer');
  LGroup := Result.AddValue('choice');
  if AReverse then
    LGroup.RequireFromPassAt('source', MakeGraphOffset(0, 0, 0),
      'exact').RequireFromPass('source', 'named')
      .RequirePrevious('previous')
  else
    LGroup.RequirePrevious('previous')
      .RequireFromPass('source', 'named')
      .RequireFromPassAt('source', MakeGraphOffset(0, 0, 0), 'exact');
  Result.AddValue('none');
  Result.Entry[0, 0, 0].Value := 'choice';
end;

function SamePassSolveReport(const A, B: TGraphPassSolveReport): Boolean;
begin
  Result := (A.Decisions = B.Decisions)
    and (A.Propagations = B.Propagations)
    and (A.Contradictions = B.Contradictions)
    and (A.Backtracks = B.Backtracks)
    and (A.Executed = B.Executed)
    and (A.ExecutionOrdinal = B.ExecutionOrdinal)
    and (A.Disposition = B.Disposition);
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

procedure TestPortableRandomSource;
const
  PASS_ZERO_GOLDEN: array[0..4] of Integer = (
    1207838001, 2068637377, 804576980, 1434489320, 682523572);
  PASS_ONE_GOLDEN: array[0..4] of Integer = (
    1831489428, 1235882772, 1527468314, 1521803337, 23774112);
  PASS_TWO_GOLDEN: array[0..4] of Integer = (
    1897658175, 1111398514, 1235936229, 1708237457, 1148126430);
  SEED_ZERO_GOLDEN: array[0..4] of Integer = (
    590474650, 1492349913, 534184656, 1965128666, 1242937530);
  SEED_ONE_GOLDEN: array[0..4] of Integer = (
    49361508, 1256767595, 249573063, 662742859, 1621684536);
  SEED_MAX_GOLDEN: array[0..4] of Integer = (
    798449634, 608112268, 2016217309, 1385297694, 693014667);
var
  I: Integer;
  LAutomatic: TGraph;
  LAutomaticSeed: TGraphSeed;
  LGraph: TGraph;
  LRaised: Boolean;
begin
  Check(WFC_RANDOM_ALGORITHM_VERSION = 1,
    'the portable random algorithm has an explicit replay version');
  LGraph := TGraph.Create;
  try
    LGraph.SwitchToPass('second');
    LGraph.SwitchToPass('third');
    LGraph.Seed := $DEADBEEF;
    Check((LGraph.Seed = $DEADBEEF)
      and (LGraph.PassGraph[0].Seed = $DEADBEEF)
      and (LGraph.PassGraph[1].Seed = $DEADBEEF)
      and (LGraph.PassGraph[2].Seed = $DEADBEEF),
      'the public seed is shared by the complete pipeline');

    Check(LGraph.PassGraph[0].RandomIndex(1) = 0,
      'a singleton bound returns zero without needing a choice');
    for I := 0 to High(PASS_ZERO_GOLDEN) do
      Check(LGraph.PassGraph[0].RandomIndex(High(Integer))
        = PASS_ZERO_GOLDEN[I],
        Format('pass zero matches portable random vector %d', [I]));
    for I := 0 to High(PASS_ONE_GOLDEN) do
      Check(LGraph.PassGraph[1].RandomIndex(High(Integer))
        = PASS_ONE_GOLDEN[I],
        Format('pass one matches jumped random vector %d', [I]));
    for I := 0 to High(PASS_TWO_GOLDEN) do
      Check(LGraph.PassGraph[2].RandomIndex(High(Integer))
        = PASS_TWO_GOLDEN[I],
        Format('pass two matches twice-jumped random vector %d', [I]));

    LGraph.Seed := 0;
    for I := 0 to High(SEED_ZERO_GOLDEN) do
      Check(LGraph.PassGraph[0].RandomIndex(High(Integer))
        = SEED_ZERO_GOLDEN[I],
        Format('seed zero matches portable random vector %d', [I]));
    LGraph.Seed := 1;
    for I := 0 to High(SEED_ONE_GOLDEN) do
      Check(LGraph.PassGraph[0].RandomIndex(High(Integer))
        = SEED_ONE_GOLDEN[I],
        Format('seed one matches portable random vector %d', [I]));
    LGraph.Seed := High(TGraphSeed);
    for I := 0 to High(SEED_MAX_GOLDEN) do
      Check(LGraph.PassGraph[0].RandomIndex(High(Integer))
        = SEED_MAX_GOLDEN[I],
        Format('maximum seed matches portable random vector %d', [I]));

    LGraph.Seed := $DEADBEEF;
    Check(LGraph.PassGraph[0].RandomIndex(1500000000) = 568637377,
      'bounded sampling rejects a biased draw before returning a value');

    LGraph.Seed := $DEADBEEF;
    Check(LGraph.PassGraph[0].RandomIndex(High(Integer))
      = PASS_ZERO_GOLDEN[0],
      'assigning a seed rewinds existing pass streams');
    Check(LGraph.PassGraph[1].RandomIndex(High(Integer))
      = PASS_ONE_GOLDEN[0],
      'rewinding one pipeline restores every independent pass stream');

    LGraph.Seed := $DEADBEEF;
    for I := 1 to 32 do
      LGraph.PassGraph[0].RandomIndex(High(Integer));
    Check(LGraph.PassGraph[1].RandomIndex(High(Integer))
      = PASS_ONE_GOLDEN[0],
      'draws from one pass cannot perturb another pass stream');

    LGraph.PassGraph[1].CurrentPass := 'renamed-second';
    LGraph.Seed := $DEADBEEF;
    Check(LGraph.PassGraph[1].RandomIndex(High(Integer))
      = PASS_ONE_GOLDEN[0],
      'renaming a pass does not change its index-derived stream');

    LRaised := False;
    try
      LGraph.RandomIndex(0);
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised, 'RandomIndex rejects a zero bound');
    LRaised := False;
    try
      LGraph.RandomIndex(-1);
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised, 'RandomIndex rejects a negative bound');

    LGraph.Reset;
    Check(LGraph.Seed = $DEADBEEF,
      'Reset preserves the pipeline seed');
    Check(LGraph.RandomIndex(High(Integer)) = PASS_ZERO_GOLDEN[0],
      'the reset pass starts from the same pass-zero stream');
  finally
    LGraph.Free;
  end;

  LAutomatic := TGraph.Create;
  try
    LAutomaticSeed := LAutomatic.Seed;
    for I := 1 to 32 do
      System.Random(1000);
    Check(LAutomatic.Seed = LAutomaticSeed,
      'an automatic seed is captured once on first read');
    LAutomatic.SwitchToPass('second');
    Check((LAutomatic.Seed = LAutomaticSeed)
      and (LAutomatic.PassGraph[0].Seed = LAutomaticSeed)
      and (LAutomatic.PassGraph[1].Seed = LAutomaticSeed),
      'materializing passes preserves the captured automatic seed');
  finally
    LAutomatic.Free;
  end;
end;

{$IFNDEF PAS2JS}
procedure TestExplicitSeedHostRandomIsolation;
var
  LActual: LongInt;
  LExpected: LongInt;
  LGraph: TGraph;
  LSavedRandSeed: Cardinal;
begin
  LGraph := nil;
  LSavedRandSeed := System.RandSeed;
  try
    System.RandSeed := 123456789;
    LExpected := System.Random(1000000);

    System.RandSeed := 123456789;
    LGraph := TGraph.Create;
    LGraph.Seed := $DEADBEEF;
    LActual := System.Random(1000000);

    Check(LActual = LExpected,
      'assigning an explicit seed does not consume the host random stream');
  finally
    LGraph.Free;
    System.RandSeed := LSavedRandSeed;
  end;
end;
{$ENDIF}

procedure TestSeededReplay;
var
  I: Integer;
  LFirst: TGraph;
  LFirstSnapshot: String;
  LOtherSeedSnapshot: String;
  LSecond: TGraph;
begin
  LFirst := NewSeededFixture($DEADBEEF);
  LSecond := NewSeededFixture($DEADBEEF);
  try
    LFirst.Run;
    LFirstSnapshot := SnapshotPipeline(LFirst);

    //Neither unrelated RTL draws nor random calls made outside Run are part
    //of the graph-owned replay stream.
    for I := 1 to 64 do
      System.Random(1000);
    LSecond.RandomIndex(High(Integer));
    LSecond.Run;
    Check(SnapshotPipeline(LSecond) = LFirstSnapshot,
      'same seed and model replay across independent graph instances');

    LFirst.Run;
    Check(SnapshotPipeline(LFirst) = LFirstSnapshot,
      'every Run rewinds its graph-owned random streams');

    LFirst.Seed := $CAFEBABE;
    LFirst.Run;
    LOtherSeedSnapshot := SnapshotPipeline(LFirst);
    Check(LOtherSeedSnapshot <> LFirstSnapshot,
      'a different explicit seed changes the generated fixture');

    LFirst.Seed := $DEADBEEF;
    LFirst.Run;
    Check(SnapshotPipeline(LFirst) = LFirstSnapshot,
      'restoring the original seed restores the exact fixture');

    LFirst.SwitchToPass('later-copy');
    LFirst.Run;
    Check(SnapshotPass(LFirst, 0) = SnapshotPass(LSecond, 0),
      'appending a later pass cannot perturb earlier pass output');
    Check(SnapshotPass(LFirst, 1) = SnapshotPass(LFirst, 0),
      'the appended definitionless pass copies the replayed output');

    Check(LFirstSnapshot =
      '0:ZAZZA/QQZQA/ZZAQZ/|ZAZAA/ZZQQA/ZQZQQ/|#',
      'the seeded 3D fixture matches its canonical replay vector');
  finally
    LSecond.Free;
    LFirst.Free;
  end;
end;

procedure TestSeededMultiPassReplay;
var
  LGraph: TGraph;
  LSnapshot: String;
begin
  LGraph := NewSeededPassFixture($DEADBEEF);
  try
    LGraph.Run;
    LSnapshot := SnapshotPipeline(LGraph);
    Check(SnapshotPass(LGraph, 2) = SnapshotPass(LGraph, 1),
      'a seeded definitionless pass copies the preceding result');
    LGraph.Run;
    Check(SnapshotPipeline(LGraph) = LSnapshot,
      'terrain, constrained foliage, and copy passes replay together');
    Check(LSnapshot =
      '0:LWLWLL/WWLWLL/|#1:.....T/.....T/|#2:.....T/.....T/|#',
      'the seeded multi-pass fixture matches its canonical replay vector');
  finally
    LGraph.Free;
  end;

end;

procedure TestSeededWrappedTopDownReplay;
var
  LGraph: TGraph;
  LSnapshot: String;
begin
  LGraph := NewSeededFixture($DEADBEEF);
  try
    LGraph.WrapNeighbors := True;
    LGraph.Mode := rmTopDown;
    LGraph.Run;
    LSnapshot := SnapshotPipeline(LGraph);
    LGraph.Run;
    Check(SnapshotPipeline(LGraph) = LSnapshot,
      'wrapped top-down generation replays on consecutive runs');
    Check(LSnapshot =
      '0:QZAZA/ZQAQZ/ZQAQZ/|ZAQZA/QQZQZ/ZZAAZ/|#',
      'wrapped top-down generation matches its canonical replay vector');
  finally
    LGraph.Free;
  end;

end;

procedure TestRandomCallbackPassIdentity;
var
  I: Integer;
  LBaseline: TGraph;
  LSwitching: TGraph;
begin
  LBaseline := NewTwoPassRandomFixture($DEADBEEF);
  LSwitching := NewTwoPassRandomFixture($DEADBEEF);
  try
    for I := 0 to Pred(LSwitching.TotalPassCount) do
      LSwitching.PassGraph[I].SelectionCallback := SelectRandomAfterSwitch;
    LBaseline.Run;
    LSwitching.Run;
    Check(SnapshotPipeline(LSwitching) = SnapshotPipeline(LBaseline),
      'root RandomIndex stays bound to the pass being solved after a callback switch');
  finally
    LSwitching.Free;
    LBaseline.Free;
  end;
end;

procedure TestSeedMutationDuringRun;
var
  LGraph: TGraph;
  LRaised: Boolean;
begin
  LGraph := NewSeededFixture($DEADBEEF);
  try
    LGraph.SelectionCallback := SelectAndMutateSeed;
    LRaised := False;
    try
      LGraph.Run;
    except
      on E: Exception do
        LRaised := True;
    end;
    Check(LRaised, 'callbacks cannot change the seed during Run');
    Check((LGraph.Seed = $DEADBEEF)
      and (LGraph.TotalPassCount = 1)
      and (LGraph.CurrentPassIndex = 0),
      'a rejected seed mutation leaves pipeline identity and seed intact');
  finally
    LGraph.Free;
  end;
end;

procedure TestLargeIterativeTraversal;
var
  LGenerated: Integer;
  LGraph: TGraph;
  X, Y: Integer;
begin
  LGraph := TGraph.Create;
  try
    LGraph.Seed := 1;
    LGraph.Reshape(128, 128, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    LGraph.Run;

    LGenerated := 0;
    for Y := 0 to 127 do
      for X := 0 to 127 do
        if LGraph.Entry[X, Y, 0].Generated
          and (LGraph.Entry[X, Y, 0].Value = 'A') then
          Inc(LGenerated);
    Check(LGenerated = 128 * 128,
      'iterative traversal visits a large plane without recursion limits');
  finally
    LGraph.Free;
  end;
end;

procedure TestInvalidStartCoordinate;
var
  LGraph: TGraph;
  LRaised: Boolean;
begin
  LGraph := TInvalidStartGraph.Create;
  try
    LGraph.Reshape(2, 2, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    LRaised := False;
    try
      LGraph.Run;
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised,
      'an out-of-range start coordinate cannot alias another graph row');
    Check(LGraph.Entry[0, 0, 0].Empty
      and LGraph.Entry[1, 0, 0].Empty
      and LGraph.Entry[0, 1, 0].Empty
      and LGraph.Entry[1, 1, 0].Empty,
      'an invalid start coordinate is rejected before generation begins');
  finally
    LGraph.Free;
  end;
end;

procedure TestIterativeTraversalCompatibility;
var
  LDirection: TGraphDirection;
  LEntry: TGraphEntry;
  LExternal: TGraphEntry;
  LGraph: TGraph;
  X, Y: Integer;
begin
  LGraph := TFixedStartGraph.Create;
  try
    LGraph.Reshape(2, 2, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    LGraph.SelectionCallback := SelectAndCaptureTraversal;
    GTraversalCount := 0;
    LGraph.Run;
    Check((GTraversalCount = 4)
      and (GTraversalIndices[0] = 0)
      and (GTraversalIndices[1] = 2)
      and (GTraversalIndices[2] = 3)
      and (GTraversalIndices[3] = 1),
      'iterative traversal preserves the legacy north/east/south/west DFS order');
  finally
    LGraph.Free;
  end;

  LGraph := TFixedStartGraph.Create;
  try
    LGraph.Reshape(2, 2, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    for Y := 0 to 1 do
      for X := 0 to 1 do
      begin
        LEntry := LGraph.Entry[X, Y, 0];
        for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
          LEntry[LDirection] := nil;
      end;
    LEntry := LGraph.Entry[0, 0, 0];
    LEntry[gdNorth] := LGraph.Entry[0, 1, 0];
    LEntry[gdEast] := LGraph.Entry[1, 0, 0];
    LGraph.SelectionCallback := SelectAndMutateNeighbor;
    LGraph.Run;
    Check(LGraph.Entry[0, 0, 0].Generated
      and LGraph.Entry[0, 1, 0].Generated
      and LGraph.Entry[1, 0, 0].Empty,
      'later neighbors are read after earlier DFS subtrees and callback mutations');
  finally
    LGraph.Free;
  end;

  LExternal := TGraphEntry.Create;
  LGraph := TFixedStartGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    LGraph.SelectionCallback := SelectFirstValid;
    LEntry := LGraph.Entry[0, 0, 0];
    LEntry[gdNorth] := LExternal;
    LGraph.Run;
    Check(LExternal.Generated and (LExternal.Value = 'A'),
      'iterative traversal retains public links to entries outside graph storage');
  finally
    LGraph.Free;
    LExternal.Free;
  end;
end;

procedure TestRuleGroup;
var
  LDirection: TGraphDirection;
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

    LGroup.NewRule(AllDirections, 'all-directions-value');
    for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
      if LGroup.Exists[LDirection] then
        Check(ContainsGraphValue(LGroup[LDirection].Value,
          'all-directions-value'),
          Format('one call retains its value for direction %d',
            [Ord(LDirection)]))
      else
        Check(False,
          Format('one call creates a distinct rule for direction %d',
            [Ord(LDirection)]));
  finally
    LGroup.Free;
  end;
end;

procedure TestExplicitDenyModel;
var
  D: TGraphDirection;
  LGraph: TGraph;
  LGroup: TGraphRuleGroup;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRaised: Boolean;
  LRules: TGraphRules;
begin
  Check((Ord(gckNone) = 0)
    and (Ord(gckEmptyDomain) = 1)
    and (Ord(gckInvalidLock) = 2)
    and (Ord(gckAdjacency) = 3)
    and (Ord(gckPreviousPass) = 4)
    and (Ord(gckRequiredSupport) = 5)
    and (Ord(gckFinalValidation) = 6)
    and (Ord(gckPassDependency) = 7),
    'existing public contradiction-kind ordinals remain replay compatible');
  Check(Ord(gckEntryDomain) > Ord(gckPassDependency),
    'the additive entry-domain diagnostic does not renumber prior kinds');
  Check(WFC_GRAPH_MODEL_VERSION > 0,
    'the graph model publishes a positive compatibility version');

  LGroup := TGraphRuleGroup.Create('A');
  try
    Check(LGroup.DeniedDirections = [],
      'a new rule group has no explicitly denied direction');
    LGroup.NewRule([gdEast], 'B', True);
    Check(LGroup.Exists[gdEast] and (not LGroup.Denied[gdEast])
      and LGroup.HasRequired,
      'a finite required rule begins in allow-list mode');

    LGroup.DenyAll([gdNorth, gdEast]);
    Check(LGroup.DeniedDirections = [gdNorth, gdEast],
      'DenyAll records its complete canonical direction set');
    Check(LGroup.Denied[gdNorth] and LGroup.Denied[gdEast]
      and (not LGroup.Denied[gdSouth]),
      'the indexed denied view agrees with DeniedDirections');
    Check(not LGroup.HasRequired,
      'transitioning the only required rule to deny-all removes required support');

    LGroup.NewRule([gdEast], 'C');
    Check((not LGroup.Denied[gdEast]) and LGroup.Denied[gdNorth]
      and LGroup.Exists[gdEast]
      and GraphValuesAre(LGroup[gdEast].Value, ['C']),
      'a later finite rule replaces deny-all without reviving stale values');
    LGroup.DenyAll([gdEast]).NewRule([gdEast], 'D', True);
    Check((not LGroup.Denied[gdEast])
      and GraphValuesAre(LGroup[gdEast].Value, ['D'])
      and LGroup[gdEast].Info,
      'finite to deny to finite remains fluent and preserves the new required flag');
  finally
    LGroup.Free;
  end;

  LOptions := DefaultGraphSolveOptions;
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    Check(LGraph.TrySolve(LOptions, LReport),
      'a missing directional rule remains a wildcard');
    Check(LReport.GraphModelVersion = WFC_GRAPH_MODEL_VERSION,
      'successful reports identify the graph-model semantics');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    SetLength(LRules, Ord(High(TGraphDirection)) + 1);
    for D := Low(TGraphDirection) to High(TGraphDirection) do
    begin
      LRules[Ord(D)].Key := D;
      SetLength(LRules[Ord(D)].Value, 0);
      LRules[Ord(D)].Info := False;
    end;
    LGraph.Rules['A'].Rules := LRules;
    Check(LGraph.Rules['A'].DeniedDirections = [],
      'legacy present-empty rules do not become explicit denials');
    Check(LGraph.TrySolve(LOptions, LReport),
      'the reference solver preserves present-empty wildcard compatibility');
    LGraph.Entry[0, 0, 0].ClearValue;
    LGraph.Entry[1, 0, 0].ClearValue;
    LGraph.SelectionCallback := SelectFirstValid;
    LGraph.Run;
    Check((LGraph.Entry[0, 0, 0].Value = 'A')
      and (LGraph.Entry[1, 0, 0].Value = 'A'),
      'legacy Run preserves present-empty wildcard compatibility');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A').DenyAll(AllDirections);
    LGraph.SelectionCallback := SelectFirstValid;
    LRaised := False;
    try
      LGraph.Run;
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised,
      'legacy Run enforces explicit deny-all on a real neighbor arc');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A').DenyAll(AllDirections);
    Check(not LGraph.TrySolve(LOptions, LReport),
      'the reference solver enforces explicit deny-all on a real neighbor arc');
    Check((LReport.Contradiction.Kind = gckAdjacency)
      and (LReport.GraphModelVersion = WFC_GRAPH_MODEL_VERSION),
      'explicit denial reports adjacency under the published model version');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A').DenyAll(AllDirections);
    LGraph.SelectionCallback := SelectFirstValid;
    LGraph.Run;
    Check(LGraph.Entry[0, 0, 0].Value = 'A',
      'deny-all does not reject a direction whose boundary neighbor is nil');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A').DenyAll(AllDirections);
    Check(LGraph.TrySolve(LOptions, LReport)
      and (LGraph.Entry[0, 0, 0].Value = 'A'),
      'the reference solver also ignores denied arcs beyond an open boundary');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A').DenyAll([gdNorth]);
    LGraph.SelectionCallback := SelectFirstValid;
    LRaised := False;
    try
      LGraph.Run;
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised,
      'legacy Run applies explicit denial to a wrapped singleton self-arc');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A').DenyAll([gdNorth]);
    Check(not LGraph.TrySolve(LOptions, LReport),
      'the reference solver applies explicit denial to a wrapped singleton');
    Check(LReport.Contradiction.Kind = gckAdjacency,
      'wrapped singleton denial receives adjacency evidence');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.AddValue('A').NewRule([gdEast], 'B');
    LGraph.RuleGroups.Add('nil-public-group', nil);
    LRaised := False;
    try
      LGraph.Rules['A'].DenyAll([gdEast]);
    except
      on E: EInvalidOperation do
        LRaised := True;
      on E: Exception do
        ;
    end;
    Check(LRaised,
      'DenyAll preflight rejects a nil public rule group');
    Check((not LGraph.Rules['A'].Denied[gdEast])
      and LGraph.Rules['A'].Exists[gdEast]
      and GraphValuesAre(LGraph.Rules['A'].Rule[gdEast].Value, ['B'])
      and (not LGraph.Rules['B'].Denied[gdWest])
      and GraphValuesAre(LGraph.Rules['B'].Rule[gdWest].Value, ['A']),
      'nil-group rejection precedes every source and reciprocal denial change');
    LGraph.RuleGroups.Remove('nil-public-group');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.AddValue('A').NewRule([gdEast], 'B');
    LGraph.Rules['B'].Value := 'identity-corrupt';
    LRaised := False;
    try
      LGraph.Rules['A'].DenyAll([gdEast]);
    except
      on E: EInvalidOperation do
        LRaised := True;
      on E: Exception do
        ;
    end;
    Check(LRaised,
      'DenyAll preflight rejects a key/value identity mismatch');
    Check((not LGraph.Rules['A'].Denied[gdEast])
      and LGraph.Rules['A'].Exists[gdEast]
      and GraphValuesAre(LGraph.Rules['A'].Rule[gdEast].Value, ['B'])
      and (not LGraph.Rules['B'].Denied[gdWest])
      and GraphValuesAre(LGraph.Rules['B'].Rule[gdWest].Value, ['A']),
      'identity rejection precedes every source and reciprocal denial change');
    LGraph.Rules['B'].Value := 'B';
  finally
    LGraph.Free;
  end;
end;

procedure TestAllowedValueStorage;
var
  LCopy: TGraphValues;
  LGraph: TGraph;
  LInput: TGraphValues;
  LPassIndex: Integer;
  LRaised: Boolean;
begin
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.AddValue('A');
    LGraph.AddValue('B');
    LGraph.AddValue('C');

    SetLength(LInput, 3);
    LInput[0] := 'C';
    LInput[1] := 'A';
    LInput[2] := 'C';
    Check(LGraph.SetAllowedValues(0, 0, 0, LInput) = LGraph,
      'SetAllowedValues is fluent');
    Check(LGraph.HasAllowedValues(0, 0, 0),
      'an assigned entry domain is distinguishable from no domain');
    LCopy := LGraph.CopyAllowedValues(0, 0, 0);
    Check(GraphValuesAre(LCopy, ['A', 'C']),
      'entry domains are deduplicated into canonical value-registry order');

    LInput[0] := 'B';
    LInput[1] := 'B';
    LCopy := LGraph.CopyAllowedValues(0, 0, 0);
    Check(GraphValuesAre(LCopy, ['A', 'C']),
      'SetAllowedValues detaches its storage from the caller array');
    LCopy[0] := 'B';
    LCopy := LGraph.CopyAllowedValues(0, 0, 0);
    Check(GraphValuesAre(LCopy, ['A', 'C']),
      'CopyAllowedValues returns detached storage');

    Check(LGraph.SetAllowedValues(0, 0, 0, 'B') = LGraph,
      'the single-value domain overload is fluent');
    Check(GraphValuesAre(LGraph.CopyAllowedValues(0, 0, 0), ['B']),
      'the single-value overload creates a singleton domain');

    LGraph.SetAllowedValues(0, 0, 0, ['A', 'C']);
    LRaised := False;
    try
      LGraph.SetAllowedValues(0, 0, 0, ['B', 'outside']);
    except
      on E: Exception do
        LRaised := True;
    end;
    Check(LRaised,
      'an entry domain rejects a value outside the selected pass registry');
    Check(GraphValuesAre(LGraph.CopyAllowedValues(0, 0, 0), ['A', 'C']),
      'unknown-value rejection is atomic');

    SetLength(LInput, 0);
    LGraph.SetAllowedValues(1, 0, 0, LInput);
    Check(LGraph.HasAllowedValues(1, 0, 0)
      and (Length(LGraph.CopyAllowedValues(1, 0, 0)) = 0),
      'an explicit empty domain remains distinct from a cleared domain');
    Check(LGraph.ClearAllowedValues(1, 0, 0) = LGraph,
      'ClearAllowedValues is fluent');
    Check((not LGraph.HasAllowedValues(1, 0, 0))
      and (Length(LGraph.CopyAllowedValues(1, 0, 0)) = 0),
      'clearing removes domain presence as well as its values');

    LGraph.SwitchToPass('second', LPassIndex);
    LGraph.AddValue('X');
    LGraph.AddValue('Y');
    LGraph.SetAllowedValues(0, 0, 0, 'Y');
    Check(GraphValuesAre(LGraph.CopyAllowedValues(0, 0, 0), ['Y'])
      and GraphValuesAre(
        LGraph.PassGraph[0].CopyAllowedValues(0, 0, 0), ['A', 'C']),
      'entry domains are pass-local when addressed through root or pass graph');
    Check(not LGraph.PassGraph[0].HasAllowedValues(1, 0, 0)
      and (LPassIndex = 1),
      'clearing one pass cannot create domain state in another pass');
  finally
    LGraph.Free;
  end;
end;

procedure TestAllowedValueSolving;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    LGraph.AddValue('B');
    LGraph.SetAllowedValues(0, 0, 0, 'B');
    LGraph.SelectionCallback := SelectFirstValid;
    LGraph.Run;
    Check((LGraph.Entry[0, 0, 0].Value = 'B')
      and LGraph.Entry[0, 0, 0].Generated,
      'legacy Run intersects generation with an entry domain');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    LGraph.AddValue('B');
    LGraph.SetAllowedValues(0, 0, 0, 'B');
    Check(LGraph.TrySolve(LOptions, LReport)
      and (LGraph.Entry[0, 0, 0].Value = 'B'),
      'the reference solver intersects generation with an entry domain');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A');
    LGraph.SetAllowedValues(0, 0, 0, []);
    Check(not LGraph.TrySolve(LOptions, LReport),
      'an explicit empty entry domain is a reported contradiction');
    Check((LReport.Contradiction.Kind = gckEntryDomain)
      and (LReport.Contradiction.EntryIndex = 0)
      and (LReport.GraphModelVersion = WFC_GRAPH_MODEL_VERSION),
      'empty-domain evidence identifies the entry and model semantics');
    Check(LGraph.Entry[0, 0, 0].Empty,
      'an empty entry domain cannot commit an invented value');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A');
    LGraph.AddValue('B');
    LGraph.SetAllowedValues(0, 0, 0, 'A');
    LGraph.Entry[0, 0, 0].Value := 'B';
    Check(not LGraph.TrySolve(LOptions, LReport),
      'a caller lock cannot bypass a disjoint entry domain');
    Check((LReport.Contradiction.Kind = gckEntryDomain)
      and (LReport.Contradiction.EntryIndex = 0),
      'lock-domain disjointness is distinguished from an unknown lock');
    Check((LGraph.Entry[0, 0, 0].Value = 'B')
      and (not LGraph.Entry[0, 0, 0].Generated)
      and GraphValuesAre(LGraph.CopyAllowedValues(0, 0, 0), ['A']),
      'failed lock-domain intersection preserves both caller inputs');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A').NewRule([gdEast, gdWest], 'A');
    LGraph.AddValue('B').NewRule([gdEast, gdWest], 'B');
    LGraph.SetAllowedValues(0, 0, 0, ['A', 'B']);
    LGraph.SetAllowedValues(1, 0, 0, 'A');
    LGraph.Entry[0, 0, 0].Value := 'A';
    Check(LGraph.TrySolve(LOptions, LReport),
      'lock, mask, and adjacency constraints intersect when compatible');
    Check((LGraph.Entry[0, 0, 0].Value = 'A')
      and (LGraph.Entry[1, 0, 0].Value = 'A'),
      'the compatible intersection commits only its common value');

    LGraph.Entry[1, 0, 0].ClearValue;
    LGraph.SetAllowedValues(1, 0, 0, 'B');
    Check(not LGraph.TrySolve(LOptions, LReport),
      'a nonempty mask can still contradict adjacency and a caller lock');
    Check(LReport.Contradiction.Kind = gckAdjacency,
      'propagated mask incompatibility remains an adjacency contradiction');
    Check((LGraph.Entry[0, 0, 0].Value = 'A')
      and LGraph.Entry[1, 0, 0].Empty,
      'failed adjacency preserves the caller lock and unassigned destination');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A');
    LGraph.AddValue('B');
    LGraph.SetAllowedValues(0, 0, 0, 'A');
    Check(LGraph.TrySolve(LOptions, LReport),
      'the persistence fixture solves its masked entry');
    LGraph.Entry[0, 0, 0].ClearValue;
    Check(LGraph.Entry[0, 0, 0].Empty
      and LGraph.HasAllowedValues(0, 0, 0)
      and GraphValuesAre(LGraph.CopyAllowedValues(0, 0, 0), ['A']),
      'ClearValue removes assignment ownership without clearing its domain');
    Check(LGraph.TrySolve(LOptions, LReport)
      and (LGraph.Entry[0, 0, 0].Value = 'A'),
      'entry domains persist through repeated reference solves');
  finally
    LGraph.Free;
  end;
end;

procedure TestAllowedValueLifecycle;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRaised: Boolean;
begin
  LOptions := DefaultGraphSolveOptions;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A');
    LGraph.AddValue('B');
    LGraph.SetAllowedValues(0, 0, 0, 'A');
    LGraph.Entry[0, 0, 0].Reset;
    Check(LGraph.Entry[0, 0, 0].Empty
      and LGraph.HasAllowedValues(0, 0, 0)
      and GraphValuesAre(LGraph.CopyAllowedValues(0, 0, 0), ['A']),
      'public Entry.Reset clears entry state without discarding its caller domain');
  finally
    LGraph.Free;
  end;

  GInvalidResetEntryCount := 0;
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    LGraph.SetAllowedValues(0, 0, 0, []);
    LGraph.InvalidStateCallback := ResetInvalidEntryAndProposeAllowed;
    LRaised := False;
    try
      LGraph.Run;
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (GInvalidResetEntryCount = 1),
      'a legacy invalid-state hook cannot bypass an empty caller domain through Entry.Reset');
    Check(LGraph.Entry[0, 0, 0].Empty
      and LGraph.HasAllowedValues(0, 0, 0)
      and (Length(LGraph.CopyAllowedValues(0, 0, 0)) = 0),
      'legacy hook reset leaves the explicit empty domain intact');
  finally
    GInvalidResetEntryCount := 0;
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A');
    LGraph.AddValue('B');
    LGraph.SetAllowedValues(0, 0, 0, 'A');
    LGraph.SwitchToPass('second');
    LGraph.AddValue('X');
    LGraph.SetAllowedValues(0, 0, 0, 'X');
    LGraph.Reshape(1, 1, 1);
    Check((not LGraph.PassGraph[0].HasAllowedValues(0, 0, 0))
      and (not LGraph.PassGraph[1].HasAllowedValues(0, 0, 0)),
      'Reshape clears entry domains from every pass');

    LGraph.SetAllowedValues(0, 0, 0, 'X');
    LGraph.Reset;
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('R');
    Check((LGraph.TotalPassCount = 1)
      and (not LGraph.HasAllowedValues(0, 0, 0)),
      'Reset discards masked pass storage before later reshaping');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Seed := $10203040;
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'clean';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('A');
    LGraph.AddValue('B');
    LGraph.SetAllowedValues(0, 0, 0, 'A');
    LGraph.SwitchToPass('dirty');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('X');
    LGraph.AddValue('Y');
    LGraph.SetAllowedValues(0, 0, 0, 'X');
    Check(LGraph.TrySolve(LOptions, LReport),
      'the selective-domain fixture establishes committed output');

    LGraph.SetAllowedValues(0, 0, 0, []);
    Check(not LGraph.TryRegenerateFrom('dirty', LOptions, LReport),
      'an empty dirty-pass domain aborts selective regeneration');
    Check((not LReport.Passes[0].Executed)
      and (LReport.Passes[0].Disposition = gpdReused)
      and (LGraph.PassGraph[0].Entry[0, 0, 0].Value = 'A')
      and LGraph.PassGraph[0].Entry[0, 0, 0].Generated
      and (LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'X')
      and LGraph.PassGraph[1].Entry[0, 0, 0].Generated,
      'failed selective solving rolls entries back and reuses the clean pass');
    Check(GraphValuesAre(
      LGraph.PassGraph[0].CopyAllowedValues(0, 0, 0), ['A'])
      and LGraph.PassGraph[1].HasAllowedValues(0, 0, 0)
      and (Length(LGraph.PassGraph[1].CopyAllowedValues(0, 0, 0)) = 0),
      'selective rollback preserves every caller-owned pass domain');

    LGraph.SetAllowedValues(0, 0, 0, 'X');
    LGraph.Entry[0, 0, 0].ClearValue;
    Check(LGraph.TryRegenerateFrom('dirty', LOptions, LReport),
      'restoring the dirty-pass domain permits selective regeneration');
    Check((not LReport.Passes[0].Executed)
      and (LReport.Passes[0].Disposition = gpdReused)
      and GraphValuesAre(
        LGraph.PassGraph[0].CopyAllowedValues(0, 0, 0), ['A'])
      and GraphValuesAre(
        LGraph.PassGraph[1].CopyAllowedValues(0, 0, 0), ['X']),
      'successful selective solving leaves skipped and dirty domains unchanged');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A');
    LGraph.AddValue('B');
    LGraph.SetAllowedValues(0, 0, 0, 'A');
    LGraph.SelectionCallback := SelectAndMutateAllowedValues;
    LRaised := False;
    try
      LGraph.Run;
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised,
      'a legacy selection callback cannot mutate entry domains while running');
    Check(LGraph.Entry[0, 0, 0].Empty
      and GraphValuesAre(LGraph.CopyAllowedValues(0, 0, 0), ['A']),
      'rejected callback mutation preserves assignment and domain state');
  finally
    LGraph.Free;
  end;

  GCommitDomainGraph := nil;
  GCommitDomainMutationCount := 0;
  LGraph := TCommitIdentityGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A');
    LGraph.AddValue('B');
    LGraph.SetAllowedValues(0, 0, 0, 'A');
    GCommitDomainGraph := LGraph;
    LRaised := False;
    try
      LGraph.TrySolve(LOptions, LReport);
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (GCommitDomainMutationCount = 1),
      'a reference commit hook cannot mutate entry domains');
    Check(LGraph.Entry[0, 0, 0].Empty
      and GraphValuesAre(LGraph.CopyAllowedValues(0, 0, 0), ['A']),
      'commit-hook domain mutation rolls entries back without changing masks');
  finally
    GCommitDomainGraph := nil;
    GCommitDomainMutationCount := 0;
    LGraph.Free;
  end;

  GCommitResetEntryEnabled := False;
  GCommitResetEntryCount := 0;
  LGraph := TCommitIdentityGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    LGraph.SetAllowedValues(0, 0, 0, 'A');
    GCommitResetEntryEnabled := True;
    LRaised := False;
    try
      LGraph.TrySolve(LOptions, LReport);
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (GCommitResetEntryCount = 1),
      'a reference commit hook reset is detected before success');
    Check(LGraph.Entry[0, 0, 0].Empty
      and LGraph.HasAllowedValues(0, 0, 0)
      and GraphValuesAre(LGraph.CopyAllowedValues(0, 0, 0), ['A']),
      'commit-hook reset rollback preserves the caller domain');
  finally
    GCommitResetEntryEnabled := False;
    GCommitResetEntryCount := 0;
    LGraph.Free;
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
  LOriginalDirections: TGraphDirections;
  LOriginalValues: TArray<TGraphValue>;
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
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('compat').DenyAll([gdUp]);
    LGraph.SetAllowedValues(0, 0, 0, 'compat');
    LOriginalDirections := LGraph.Rules['compat'].DeniedDirections;
    LOriginalValues := LGraph.CopyAllowedValues(0, 0, 0);
    Check((LOriginalDirections = [gdUp])
      and GraphValuesAre(LOriginalValues, ['compat']),
      'native denial and domain APIs retain original public set and TArray identities');
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

procedure TestDependencyDagConfiguration;
var
  LGraph: TGraph;
  LBefore: String;
  LInvalidModeOrdinal: Integer;
  LReplacementGroup: TGraphRuleGroup;
  LRaised: Boolean;
begin
  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.CurrentPass := 'first';
    Check((LGraph.PassMode = gpmLegacy)
      and (LGraph.DependencyCount = 0),
      'pass zero starts in legacy mode without a dependency');

    LGraph.SwitchToPass('second');
    Check((LGraph.PassMode = gpmLegacy)
      and (DependencySnapshot(LGraph, 1) = '0'),
      'a new legacy pass starts with its stable predecessor dependency');
    LGraph.DependsOn('first').DependsOn('first');
    Check(DependencySnapshot(LGraph, 1) = '0',
      'duplicate dependency declarations are idempotent');

    LRaised := False;
    try
      LGraph.RemoveDependency('first');
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (DependencySnapshot(LGraph, 1) = '0'),
      'the active legacy predecessor role cannot be removed');

    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    Check((LGraph.PassMode = gpmOverlay)
      and (LGraph.DependencyCount = 0),
      'an explicit overlay may clear the converted legacy edge');
    LGraph.DependsOn('first').DependsOn('first');

    LGraph.SwitchToPass('third');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.SwitchToPass('second');
    LGraph.RemoveDependency('third');
    Check(DependencySnapshot(LGraph, 1) = '0',
      'removing an absent dependency is idempotent');

    LBefore := DependencySnapshot(LGraph, 1);
    LRaised := False;
    try
      LGraph.DependsOn('missing');
    except
      on E: EArgumentException do
        LRaised := True;
    end;
    Check(LRaised and (DependencySnapshot(LGraph, 1) = LBefore),
      'a missing dependency label fails without mutating the plan');

    LRaised := False;
    try
      LGraph.DependsOn('second');
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (DependencySnapshot(LGraph, 1) = LBefore),
      'a self dependency fails atomically');

    LGraph.SwitchToPass('first');
    LRaised := False;
    try
      LGraph.DependsOn('second');
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.DependencyCount = 0),
      'a dependency that would close a cycle fails atomically');

    LGraph.PassGraph[0].CurrentPass := 'renamed-first';
    Check(DependencySnapshot(LGraph, 1) = '0',
      'dependency bindings survive source-pass renaming by stable index');

    LGraph.SwitchToPass('second');
    LGraph.TransformFrom('renamed-first');
    Check((LGraph.PassMode = gpmTransform)
      and (LGraph.TransformSourceIndex = 0)
      and (DependencySnapshot(LGraph, 1) = '0'),
      'TransformFrom binds one explicit stable source');
    LRaised := False;
    try
      LGraph.ClearDependencies;
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (DependencySnapshot(LGraph, 1) = '0'),
      'a transform source role protects its dependency');

    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LRaised := False;
    try
      LGraph.PassMode := gpmTransform;
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.PassMode = gpmOverlay)
      and (LGraph.DependencyCount = 0),
      'an ambiguous source-free transform mode change is atomic');

    LGraph.DependsOn('renamed-first').DependsOn('third');
    LBefore := DependencySnapshot(LGraph, 1);
    LRaised := False;
    try
      LGraph.PassMode := gpmTransform;
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.PassMode = gpmOverlay)
      and (DependencySnapshot(LGraph, 1) = LBefore),
      'a multi-source direct transform mode change is atomic');

    LGraph.ClearDependencies;
    LGraph.AddValue('candidate');
    LRaised := False;
    try
      LGraph.Rules['candidate'].RequireFromPass('missing', 'value');
    except
      on E: EArgumentException do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.DependencyCount = 0),
      'a missing named requirement adds neither requirement nor dependency');
    LRaised := False;
    try
      LGraph.Rules['candidate'].RequireFromPass('second', 'value');
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.DependencyCount = 0),
      'a self named requirement fails atomically');
    Check((LGraph.CurrentPass = 'second')
      and (LGraph.CurrentPassIndex = 1),
      'dependency errors preserve the caller-selected pass');

    LBefore := Format('%d|%d|%s', [Ord(LGraph.PassMode),
      LGraph.TransformSourceIndex, DependencySnapshot(LGraph, 1)]);
    LInvalidModeOrdinal := Ord(High(TGraphPassMode)) + 1;
    LRaised := False;
    try
      LGraph.PassMode := TGraphPassMode(LInvalidModeOrdinal);
    except
      on E: Exception do
        LRaised := True;
    end;
    Check(LRaised and (Format('%d|%d|%s', [Ord(LGraph.PassMode),
      LGraph.TransformSourceIndex, DependencySnapshot(LGraph, 1)]) =
      LBefore),
      'an invalid pass mode is rejected before mutating dependency state');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.CurrentPass := 'source';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('A');
    LGraph.SwitchToPass('consumer');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('X').RequireFromPass('source', 'A');
    Check(DependencySnapshot(LGraph, 1) = '0',
      'a live named requirement protects its source edge');
    LGraph.RuleGroups.Remove('X');
    LReplacementGroup := TGraphRuleGroup.Create('X');
    LGraph.RuleGroups.Add('X', LReplacementGroup);
    Check(LGraph.DependencyCount = 0,
      'replacing the last constrained group removes its stale requirement edge');
    LGraph.SwitchToPass('source');
    LRaised := False;
    try
      LGraph.DependsOn('consumer');
    except
      on E: Exception do
        LRaised := True;
    end;
    Check((not LRaised) and (DependencySnapshot(LGraph, 0) = '1'),
      'removed requirement roles cannot create a false reverse-edge cycle');
  finally
    LGraph.Free;
  end;
end;

procedure TestDeterministicDagExecution;
const
  EXPECTED_ORDINALS: array[0..4] of Integer = (0, 3, 1, 2, 4);
  EXPECTED_ORDER: array[0..4] of Integer = (0, 2, 3, 1, 4);
var
  I: Integer;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := NewTopologicalDagFixture;
  try
    Check(LGraph.TrySolve(LOptions, LReport),
      'the non-linear dependency fixture solves');
    Check(ExecutionOrderSnapshot(LReport) = '0,2,3,1,4',
      'TrySolve uses stable index-priority topological order');
    Check(LReport.PipelineAlgorithmVersion = WFC_PIPELINE_ALGORITHM_VERSION,
      'the solve report identifies the dependency-pipeline algorithm');
    for I := 0 to High(EXPECTED_ORDINALS) do
      Check(LReport.Passes[I].Executed
        and (LReport.Passes[I].ExecutionOrdinal = EXPECTED_ORDINALS[I])
        and (LReport.Passes[I].Disposition = gpdSolved),
        Format('pass %d reports its stable topological execution slot', [I]));
    Check(LGraph.CurrentPassIndex = 4,
      'topological reference solving restores caller selection');

    for I := 0 to Pred(LGraph.TotalPassCount) do
      LGraph.PassGraph[I].SelectionCallback :=
        SelectAndCapturePassExecution;
    GPassExecutionCount := 0;
    LGraph.Run;
    Check(GPassExecutionCount = Length(EXPECTED_ORDER),
      'legacy Run executes every defined DAG pass once');
    for I := 0 to High(EXPECTED_ORDER) do
      Check(GPassExecutionIndices[I] = EXPECTED_ORDER[I],
        Format('legacy Run uses topological slot %d', [I]));
    Check(LGraph.CurrentPassIndex = 4,
      'topological legacy Run restores caller selection');
  finally
    LGraph.Free;
  end;
end;

procedure TestNamedPassRequirements;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRaised: Boolean;
  LRun: TGraph;
  LSnapshot: String;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := NewNamedPassFixture;
  try
    Check(DependencySnapshot(LGraph, 1) = '0,2',
      'named requirements add canonical direct dependency edges');
    LGraph.PassGraph[0].CurrentPass := 'ground';
    Check(LGraph.TrySolve(LOptions, LReport),
      'renaming a named source does not break its bound requirements');
    Check(ExecutionOrderSnapshot(LReport) = '0,2,1',
      'a later-created named source executes before its consumer');
    Check(SnapshotPass(LGraph, 1) = 'AABC/|',
      'same-source alternatives are OR while distinct sources are AND');
    Check((not LGraph.PassGraph[0].Entry[0, 0, 0].Generated)
      and (not LGraph.PassGraph[2].Entry[0, 0, 0].Generated),
      'named source locks retain caller ownership');
    Check(LGraph.CurrentPassIndex = 1,
      'named dependency solving restores consumer selection');

    LGraph.PassGraph[1].Entry[2, 0, 0].Value := 'A';
    LSnapshot := SnapshotPipelineState(LGraph);
    Check(not LGraph.TryRegenerateFrom('target', LOptions, LReport),
      'a lock violating one named source fails selective solving');
    Check((LReport.FailedPassIndex = 1)
      and (LReport.Contradiction.Kind = gckPassDependency)
      and (LReport.Contradiction.PassIndex = 1)
      and (LReport.Contradiction.DependencyPassIndex = 2)
      and (LReport.Contradiction.EntryIndex = 2),
      'named failure identifies consumer, source, and coordinate');
    Check(SnapshotPipelineState(LGraph) = LSnapshot,
      'named dependency failure is transactionally inert');
  finally
    LGraph.Free;
  end;

  LRun := NewNamedPassFixture;
  try
    ConfigureDeterministicSelection(LRun);
    LRaised := False;
    try
      LRun.Run;
    except
      on E: Exception do
        LRaised := True;
    end;
    Check((not LRaised) and (SnapshotPass(LRun, 1) = 'AABC/|'),
      'legacy Run honors named requirements in topological order');
  finally
    LRun.Free;
  end;
end;

procedure TestSpatialPassExactOffsets;
var
  LCenterIndex: Integer;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LSnapshot: String;
  X, Y, Z: Integer;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := TGraph.Create.Reshape(3, 3, 3);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'structure';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('empty');
    LGraph.AddValue('west');
    LGraph.AddValue('east');
    LGraph.AddValue('south');
    LGraph.AddValue('north');
    LGraph.AddValue('below');
    LGraph.AddValue('above');
    for Z := 0 to 2 do
      for Y := 0 to 2 do
        for X := 0 to 2 do
          LGraph.Entry[X, Y, Z].Value := 'empty';
    LGraph.Entry[0, 1, 1].Value := 'west';
    LGraph.Entry[2, 1, 1].Value := 'east';
    LGraph.Entry[1, 0, 1].Value := 'south';
    LGraph.Entry[1, 2, 1].Value := 'north';
    LGraph.Entry[1, 1, 0].Value := 'below';
    LGraph.Entry[1, 1, 2].Value := 'above';

    LGraph.SwitchToPass('supported');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('center')
      .RequireFromPassAt('structure', MakeGraphOffset(-1, 0, 0), 'west')
      .RequireFromPassAt('structure', MakeGraphOffset(1, 0, 0),
        ['west', 'east'])
      .RequireFromPassAt('structure', MakeGraphOffset(0, -1, 0), 'south')
      .RequireFromPassAt('structure', MakeGraphOffset(0, 1, 0), 'north')
      .RequireFromPassAt('structure', MakeGraphOffset(0, 0, -1), 'below')
      .RequireFromPassAt('structure', MakeGraphOffset(0, 0, 1), 'above');
    LGraph.AddValue('none');
    LGraph.Entry[1, 1, 1].Value := 'center';
    LCenterIndex := LGraph.Entry[1, 1, 1].Index;

    Check(DependencySnapshot(LGraph, 1) = '0',
      'six spatial clauses retain one canonical dependency edge');
    Check(LGraph.TrySolve(LOptions, LReport)
      and (LGraph.PassGraph[1].Entry[1, 1, 1].Value = 'center'),
      'signed X, Y, and Z offsets match their exact source coordinates');
    Check((WFC_PIPELINE_ALGORITHM_VERSION = 2)
      and (LReport.PipelineAlgorithmVersion = 2),
      'spatial pass solving reports pipeline algorithm version 2');

    LGraph.PassGraph[0].Entry[1, 1, 2].Value := 'empty';
    LSnapshot := SnapshotPipelineState(LGraph);
    Check(not LGraph.TryRegenerateFrom('structure', LOptions, LReport),
      'distinct exact-offset clauses remain conjunctive');
    Check((LReport.FailedPassIndex = 1)
      and (LReport.Contradiction.Kind = gckPassDependency)
      and (LReport.Contradiction.PassIndex = 1)
      and (LReport.Contradiction.DependencyPassIndex = 0)
      and (LReport.Contradiction.EntryIndex = LCenterIndex),
      'a spatial failure attributes the consumer, source pass, and entry');
    Check(ExecutionOrderSnapshot(LReport) = '0,1',
      'selective spatial regeneration executes its dependency closure');
    Check(SnapshotPipelineState(LGraph) = LSnapshot,
      'failed spatial regeneration rolls every pass back atomically');

    LGraph.PassGraph[0].Entry[1, 1, 2].Value := 'above';
    Check(LGraph.TryRegenerateFrom('structure', LOptions, LReport)
      and (LGraph.PassGraph[1].Entry[1, 1, 1].Value = 'center'),
      'repairing one failed spatial clause permits selective retry');
  finally
    LGraph.Free;
  end;
end;

procedure TestSpatialPassAnyClauses;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;

  LGraph := TGraph.Create.Reshape(3, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'source';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('left');
    LGraph.AddValue('anchor');
    LGraph.AddValue('right-a');
    LGraph.AddValue('right-b');
    LGraph.Entry[0, 0, 0].Value := 'left';
    LGraph.Entry[1, 0, 0].Value := 'anchor';
    LGraph.Entry[2, 0, 0].Value := 'right-b';
    LGraph.SwitchToPass('consumer');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('choice')
      .RequireAnyFromPass('source', [
        MakeGraphPassMatchTerm(MakeGraphOffset(-1, 0, 0), 'left'),
        MakeGraphPassMatchTerm(MakeGraphOffset(1, 0, 0),
          ['right-a', 'right-b'])])
      .RequireFromPassAt('source', MakeGraphOffset(0, 0, 0), 'anchor');
    LGraph.AddValue('none');
    LGraph.Entry[1, 0, 0].Value := 'choice';
    Check(LGraph.TrySolve(LOptions, LReport)
      and (LGraph.PassGraph[1].Entry[1, 0, 0].Value = 'choice'),
      'an existential clause accepts either offset with term-specific values');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create.Reshape(3, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'source';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('left');
    LGraph.AddValue('anchor');
    LGraph.AddValue('right-a');
    LGraph.Entry[0, 0, 0].Value := 'right-a';
    LGraph.Entry[1, 0, 0].Value := 'anchor';
    LGraph.Entry[2, 0, 0].Value := 'left';
    LGraph.SwitchToPass('consumer');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('choice')
      .RequireAnyFromPass('source', [
        MakeGraphPassMatchTerm(MakeGraphOffset(-1, 0, 0), 'left'),
        MakeGraphPassMatchTerm(MakeGraphOffset(1, 0, 0), 'right-a')])
      .RequireFromPassAt('source', MakeGraphOffset(0, 0, 0), 'anchor');
    LGraph.AddValue('none');
    LGraph.Entry[1, 0, 0].Value := 'choice';
    Check(not LGraph.TrySolve(LOptions, LReport),
      'existential terms do not pool values across different offsets');
    Check((LReport.Contradiction.Kind = gckPassDependency)
      and (LReport.Contradiction.DependencyPassIndex = 0)
      and (LReport.Contradiction.EntryIndex =
        LGraph.PassGraph[1].Entry[1, 0, 0].Index),
      'a failed existential clause retains dependency attribution');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create.Reshape(2, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'source';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('blank');
    LGraph.AddValue('hit');
    LGraph.Entry[0, 0, 0].Value := 'blank';
    LGraph.Entry[1, 0, 0].Value := 'hit';
    LGraph.SwitchToPass('consumer');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('choice').RequireAnyFromPass('source', [
      MakeGraphPassMatchTerm(MakeGraphOffset(-1, 0, 0), 'hit'),
      MakeGraphPassMatchTerm(MakeGraphOffset(1, 0, 0), 'hit')]);
    LGraph.AddValue('none');
    LGraph.Entry[0, 0, 0].Value := 'choice';
    Check(LGraph.TrySolve(LOptions, LReport),
      'an out-of-bounds term does not poison an in-bounds existential match');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'source';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('hit');
    LGraph.Entry[0, 0, 0].Value := 'hit';
    LGraph.SwitchToPass('consumer');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('choice').RequireAnyFromPass('source', [
      MakeGraphPassMatchTerm(MakeGraphOffset(-1, 0, 0), 'hit'),
      MakeGraphPassMatchTerm(MakeGraphOffset(1, 0, 0), 'hit')]);
    LGraph.Entry[0, 0, 0].Value := 'choice';
    Check(not LGraph.TrySolve(LOptions, LReport),
      'an existential clause fails when every bounded term is out of bounds');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'source';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('hit');
    LGraph.Entry[0, 0, 0].Value := 'hit';
    LGraph.SwitchToPass('consumer');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('choice').RequireFromPassAt('source',
      MakeGraphOffset(-1, 0, 0), 'hit');
    LGraph.Entry[0, 0, 0].Value := 'choice';
    Check(not LGraph.TrySolve(LOptions, LReport),
      'an exact clause fails when its bounded coordinate is out of bounds');
  finally
    LGraph.Free;
  end;
end;

procedure TestSpatialPassClauseComposition;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LSnapshot: String;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := TGraph.Create.Reshape(3, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'geometry';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('left');
    LGraph.AddValue('anchor');
    LGraph.AddValue('right');
    LGraph.AddValue('wrong');
    LGraph.Entry[0, 0, 0].Value := 'left';
    LGraph.Entry[1, 0, 0].Value := 'anchor';
    LGraph.Entry[2, 0, 0].Value := 'right';

    LGraph.SwitchToPass('climate');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('warm');
    LGraph.AddValue('cold');
    LGraph.Entry[0, 0, 0].Value := 'warm';
    LGraph.Entry[1, 0, 0].Value := 'warm';
    LGraph.Entry[2, 0, 0].Value := 'warm';

    LGraph.SwitchToPass('consumer');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('choice')
      .RequireAnyFromPass('geometry', [
        MakeGraphPassMatchTerm(MakeGraphOffset(-1, 0, 0), 'left'),
        MakeGraphPassMatchTerm(MakeGraphOffset(1, 0, 0), 'right')])
      .RequireAnyFromPass('geometry', [
        MakeGraphPassMatchTerm(MakeGraphOffset(0, 0, 0), 'anchor')])
      .RequireAnyFromPass('climate', [
        MakeGraphPassMatchTerm(MakeGraphOffset(0, 0, 0), 'warm')]);
    LGraph.AddValue('none');
    LGraph.Entry[1, 0, 0].Value := 'choice';

    Check((DependencySnapshot(LGraph, 2) = '0,1')
      and LGraph.TrySolve(LOptions, LReport),
      'separate any clauses and providers solve as one AND composition');

    LGraph.PassGraph[0].Entry[1, 0, 0].Value := 'wrong';
    LSnapshot := SnapshotPipelineState(LGraph);
    Check(not LGraph.TryRegenerateFrom('geometry', LOptions, LReport),
      'a second any clause from one provider remains independently required');
    Check((LReport.Contradiction.Kind = gckPassDependency)
      and (LReport.Contradiction.DependencyPassIndex = 0),
      'same-provider any-clause failure identifies that provider');
    Check(SnapshotPipelineState(LGraph) = LSnapshot,
      'same-provider any-clause failure rolls back atomically');

    LGraph.PassGraph[0].Entry[1, 0, 0].Value := 'anchor';
    LGraph.PassGraph[1].Entry[1, 0, 0].Value := 'cold';
    LSnapshot := SnapshotPipelineState(LGraph);
    Check(not LGraph.TryRegenerateFrom('climate', LOptions, LReport),
      'an any clause from a distinct provider is also conjunctive');
    Check((LReport.Contradiction.Kind = gckPassDependency)
      and (LReport.Contradiction.DependencyPassIndex = 1)
      and (SnapshotPipelineState(LGraph) = LSnapshot),
      'multi-provider failure attributes its source and preserves state');
  finally
    LGraph.Free;
  end;
end;

procedure TestSpatialPassWrapping;
var
  LGraph: TGraph;
  LOffset: TGraphOffset;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRaised: Boolean;
  LRun: TGraph;
  Y, Z: Integer;
begin
  LOffset := MakeGraphOffset(-7, 11, -13);
  Check((LOffset.DeltaX = -7) and (LOffset.DeltaY = 11)
    and (LOffset.DeltaZ = -13),
    'the portable offset constructor preserves all signed components');

  LOptions := DefaultGraphSolveOptions;
  LGraph := TGraph.Create.Reshape(4, 1, 1);
  try
    LGraph.WrapNeighbors := True;
    LGraph.CurrentPass := 'source';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('low');
    LGraph.AddValue('high');
    LGraph.AddValue('large');
    LGraph.AddValue('negative');
    LGraph.Entry[0, 0, 0].Value := 'high';
    LGraph.Entry[1, 0, 0].Value := 'low';
    LGraph.Entry[2, 0, 0].Value := 'large';
    LGraph.Entry[3, 0, 0].Value := 'negative';

    LGraph.SwitchToPass('consumer');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('wrapped')
      .RequireFromPassAt('source',
        MakeGraphOffset(Low(Integer), 0, 0), 'low')
      .RequireFromPassAt('source',
        MakeGraphOffset(High(Integer), 0, 0), 'high')
      .RequireFromPassAt('source', MakeGraphOffset(5, 0, 0), 'large')
      .RequireFromPassAt('source', MakeGraphOffset(-2, 0, 0), 'negative');
    LGraph.AddValue('none');
    LGraph.Entry[1, 0, 0].Value := 'wrapped';

    Check(LGraph.TrySolve(LOptions, LReport)
      and (LGraph.PassGraph[1].Entry[1, 0, 0].Value = 'wrapped'),
      'wrapped offsets use overflow-safe modulo for large and negative deltas');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create.Reshape(1, 4, 4);
  try
    LGraph.WrapNeighbors := True;
    LGraph.CurrentPass := 'source';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('origin');
    LGraph.AddValue('y-high');
    LGraph.AddValue('z-high');
    for Z := 0 to 3 do
      for Y := 0 to 3 do
        LGraph.Entry[0, Y, Z].Value := 'origin';
    LGraph.Entry[0, 0, 1].Value := 'y-high';
    LGraph.Entry[0, 1, 0].Value := 'z-high';
    LGraph.SwitchToPass('consumer');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('wrapped')
      .RequireFromPassAt('source',
        MakeGraphOffset(0, Low(Integer), 0), 'origin')
      .RequireFromPassAt('source',
        MakeGraphOffset(0, High(Integer), 0), 'y-high')
      .RequireFromPassAt('source',
        MakeGraphOffset(0, 0, Low(Integer)), 'origin')
      .RequireFromPassAt('source',
        MakeGraphOffset(0, 0, High(Integer)), 'z-high');
    LGraph.AddValue('none');
    LGraph.Entry[0, 1, 1].Value := 'wrapped';
    Check(LGraph.TrySolve(LOptions, LReport),
      'Y and Z Low/High(Integer) offsets wrap without overflow');
  finally
    LGraph.Free;
  end;

  LGraph := NewSpatialAnyExtremeFailureFixture(False);
  LRun := NewSpatialAnyExtremeFailureFixture(False);
  try
    Check(not LGraph.TrySolve(LOptions, LReport),
      'bounded Y/Z Low/High(Integer) any terms fail in TrySolve');
    LRaised := False;
    try
      LRun.Run;
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised,
      'legacy Run agrees on bounded extreme-offset failure');
  finally
    LRun.Free;
    LGraph.Free;
  end;

  LGraph := NewSpatialAnyExtremeFailureFixture(True);
  LRun := NewSpatialAnyExtremeFailureFixture(True);
  try
    Check(not LGraph.TrySolve(LOptions, LReport),
      'wrapped Y/Z extreme any terms still fail when values mismatch');
    LRaised := False;
    try
      LRun.Run;
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised,
      'legacy Run agrees on wrapped extreme-offset value failure');
  finally
    LRun.Free;
    LGraph.Free;
  end;
end;

procedure TestSpatialPassCompatibilityAndParity;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReference: TGraph;
  LReport: TGraphSolveReport;
  LReportTwin: TGraphSolveReport;
  LRaised: Boolean;
  LRun: TGraph;
  LSnapshot: String;
begin
  LOptions := DefaultGraphSolveOptions;
  LReference := NewSpatialParityFixture;
  LRun := NewSpatialParityFixture;
  try
    ConfigureDeterministicSelection(LReference);
    ConfigureDeterministicSelection(LRun);
    Check(LReference.TrySolve(LOptions, LReport),
      'the reference solver accepts the exact-offset parity fixture');
    LRaised := False;
    try
      LRun.Run;
    except
      on E: Exception do
        LRaised := True;
    end;
    Check(not LRaised,
      'legacy Run accepts the exact-offset parity fixture');
    Check((SnapshotPass(LReference, 1) = 'next-Bnext-Cedge/|')
      and (SnapshotPass(LRun, 1) = SnapshotPass(LReference, 1)),
      'legacy Run and TrySolve produce identical spatial filtering');
    Check((LReport.PipelineAlgorithmVersion = 2)
      and (WFC_PIPELINE_ALGORITHM_VERSION = 2),
      'the parity solve uses the public pipeline v2 contract');
  finally
    LRun.Free;
    LReference.Free;
  end;

  LReference := NewSpatialAnyParityFixture;
  LRun := NewSpatialAnyParityFixture;
  try
    ConfigureDeterministicSelection(LReference);
    ConfigureDeterministicSelection(LRun);
    Check(LReference.TrySolve(LOptions, LReport),
      'the reference solver accepts the any-clause parity fixture');
    LRaised := False;
    try
      LRun.Run;
    except
      on E: Exception do
        LRaised := True;
    end;
    Check(not LRaised,
      'legacy Run accepts the any-clause parity fixture');
    Check(SnapshotPipelineState(LRun) = SnapshotPipelineState(LReference),
      'legacy Run and TrySolve agree on successful any clauses');
  finally
    LRun.Free;
    LReference.Free;
  end;

  LReference := NewSpatialZeroMergeFixture(False);
  LRun := NewSpatialZeroMergeFixture(True);
  try
    Check(LReference.TrySolve(LOptions, LReport)
      and LRun.TrySolve(LOptions, LReportTwin),
      'all three zero-offset APIs merge as OR in both call orders');
    Check((SnapshotPass(LReference, 1) = 'choice/|')
      and (SnapshotPipelineState(LReference) = SnapshotPipelineState(LRun)),
      'zero-offset OR merging is call-order independent');
  finally
    LRun.Free;
    LReference.Free;
  end;

  LGraph := TGraph.Create.Reshape(2, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'terrain';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('land');
    LGraph.AddValue('water');
    LGraph.AddValue('rock');
    LGraph.AddValue('sand');
    LGraph.Entry[0, 0, 0].Value := 'water';
    LGraph.Entry[1, 0, 0].Value := 'rock';

    LGraph.SwitchToPass('foliage');
    LGraph.AddValue('tree')
      .RequirePrevious('land')
      .RequireFromPass('terrain', 'water')
      .RequireFromPassAt('terrain', MakeGraphOffset(1, 0, 0), 'rock');
    LGraph.AddValue('none');
    LGraph.Entry[0, 0, 0].Value := 'tree';
    Check(LGraph.TrySolve(LOptions, LReport)
      and (LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'tree'),
      'previous and named zero-offset alternatives remain OR-compatible');

    LGraph.PassGraph[0].Entry[1, 0, 0].Value := 'sand';
    LSnapshot := SnapshotPipelineState(LGraph);
    Check(not LGraph.TryRegenerateFrom('terrain', LOptions, LReport),
      'a nonzero clause from the predecessor remains a separate AND');
    Check((LReport.Contradiction.Kind = gckPassDependency)
      and (LReport.Contradiction.DependencyPassIndex = 0)
      and (LReport.Contradiction.EntryIndex =
        LGraph.PassGraph[1].Entry[0, 0, 0].Index),
      'the nonzero predecessor failure is attributed to its source');
    Check(SnapshotPipelineState(LGraph) = LSnapshot,
      'a failed predecessor-offset retry preserves the prior pipeline state');
  finally
    LGraph.Free;
  end;
end;

procedure TestSpatialPassOwnershipAndValidation;
var
  I: Integer;
  LExactValues: TGraphValues;
  LGraph: TGraph;
  LGroup: TGraphRuleGroup;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LReportTwin: TGraphSolveReport;
  LRaised: Boolean;
  LTerms: TGraphPassMatchTerms;
  LTwin: TGraph;
  LValues: TGraphValues;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := TGraph.Create.Reshape(3, 1, 1);
  LTwin := TGraph.Create.Reshape(3, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'source';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('left');
    LGraph.AddValue('middle');
    LGraph.AddValue('right');
    LGraph.Entry[0, 0, 0].Value := 'left';
    LGraph.Entry[1, 0, 0].Value := 'middle';
    LGraph.Entry[2, 0, 0].Value := 'right';
    LGraph.SwitchToPass('consumer');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    SetLength(LValues, 3);
    LValues[0] := 'left';
    LValues[1] := 'left';
    LValues[2] := 'left';
    SetLength(LTerms, 3);
    LTerms[0] := MakeGraphPassMatchTerm(
      MakeGraphOffset(-1, 0, 0), LValues);
    LTerms[1] := MakeGraphPassMatchTerm(
      MakeGraphOffset(-1, 0, 0), LValues);
    LTerms[2] := MakeGraphPassMatchTerm(
      MakeGraphOffset(1, 0, 0), 'right');
    SetLength(LExactValues, 2);
    LExactValues[0] := 'middle';
    LExactValues[1] := 'middle';
    LGroup := LGraph.AddValue('choice');
    LGroup.RequireAnyFromPass('source', LTerms)
      .RequireFromPassAt('source', MakeGraphOffset(0, 0, 0),
        LExactValues);
    LGraph.AddValue('none');
    LGraph.Entry[1, 0, 0].Value := 'choice';

    LValues[0] := 'mutated';
    LValues[1] := 'mutated';
    LValues[2] := 'mutated';
    LExactValues[0] := 'mutated';
    LExactValues[1] := 'mutated';
    LTerms[0].Values[0] := 'mutated';
    LTerms[1].Values[0] := 'mutated';
    LTerms[2].Values[0] := 'mutated';
    for I := 0 to High(LTerms) do
      LTerms[I] := MakeGraphPassMatchTerm(
        MakeGraphOffset(0, 0, 0), 'mutated');

    LTwin.WrapNeighbors := False;
    LTwin.CurrentPass := 'source';
    LTwin.PassMode := gpmOverlay;
    LTwin.AddValue('left');
    LTwin.AddValue('middle');
    LTwin.AddValue('right');
    LTwin.Entry[0, 0, 0].Value := 'left';
    LTwin.Entry[1, 0, 0].Value := 'middle';
    LTwin.Entry[2, 0, 0].Value := 'right';
    LTwin.SwitchToPass('consumer');
    LTwin.PassMode := gpmOverlay;
    LTwin.ClearDependencies;
    LTwin.AddValue('choice').RequireAnyFromPass('source', [
      MakeGraphPassMatchTerm(MakeGraphOffset(1, 0, 0), 'right'),
      MakeGraphPassMatchTerm(MakeGraphOffset(-1, 0, 0), 'left'),
      MakeGraphPassMatchTerm(MakeGraphOffset(-1, 0, 0), 'left')])
      .RequireFromPassAt('source', MakeGraphOffset(0, 0, 0), 'middle');
    LTwin.AddValue('none');
    LTwin.Entry[1, 0, 0].Value := 'choice';

    Check(LGraph.TrySolve(LOptions, LReport)
      and LTwin.TrySolve(LOptions, LReportTwin),
      'spatial clauses deep-copy terms, nested values, and exact arrays');
    Check(SnapshotPipelineState(LGraph) = SnapshotPipelineState(LTwin),
      'duplicate first-term values and reordered terms stay canonical');
    for I := 0 to Pred(LGraph.TotalPassCount) do
      Check(SamePassSolveReport(LReport.Passes[I], LReportTwin.Passes[I]),
        Format('canonical spatial terms reproduce pass report %d', [I]));
  finally
    LTwin.Free;
    LGraph.Free;
  end;

  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'source';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('A');
    LGraph.Entry[0, 0, 0].Value := 'A';
    LGraph.SwitchToPass('consumer');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGroup := LGraph.AddValue('candidate');
    LGraph.AddValue('none');

    SetLength(LTerms, 0);
    LRaised := False;
    try
      LGroup.RequireAnyFromPass('source', LTerms);
    except
      on E: EArgumentException do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.DependencyCount = 0),
      'an empty existential clause is rejected without adding an edge');

    SetLength(LTerms, 2);
    LTerms[0] := MakeGraphPassMatchTerm(
      MakeGraphOffset(1, 0, 0), 'never');
    LRaised := False;
    try
      LGroup.RequireAnyFromPass('source', LTerms);
    except
      on E: EArgumentException do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.DependencyCount = 0),
      'a clause containing an empty term is rejected atomically');

    LRaised := False;
    try
      LGroup.RequireFromPassAt('source', MakeGraphOffset(0, 0, 0),
        TGraphValue.Empty);
    except
      on E: EArgumentException do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.DependencyCount = 0),
      'an empty exact scalar value is rejected without mutation');

    SetLength(LValues, 0);
    LRaised := False;
    try
      LGroup.RequireFromPassAt('source',
        MakeGraphOffset(0, 0, 0), LValues);
    except
      on E: EArgumentException do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.DependencyCount = 0),
      'an empty exact value array is rejected without mutation');

    LRaised := False;
    try
      LTerms[0] := MakeGraphPassMatchTerm(
        MakeGraphOffset(0, 0, 0), LValues);
    except
      on E: EArgumentException do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.DependencyCount = 0),
      'the public term constructor rejects an empty value array');

    LGroup.RequireFromPassAt('source',
      MakeGraphOffset(0, 0, 0), 'A');
    LGraph.Entry[0, 0, 0].Value := 'candidate';
    Check((DependencySnapshot(LGraph, 1) = '0')
      and LGraph.TrySolve(LOptions, LReport),
      'valid configuration succeeds after atomic validation failures');
  finally
    LGraph.Free;
  end;
end;

procedure TestSpatialPassConfigurationGuards;
var
  LGraph: TGraph;
  LGroup: TGraphRuleGroup;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRaised: Boolean;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'source';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('A');
    LGraph.Entry[0, 0, 0].Value := 'A';
    LGraph.SwitchToPass('consumer');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGroup := LGraph.AddValue('candidate');
    LGraph.AddValue('none');
    LGraph.Entry[0, 0, 0].Value := 'candidate';

    LRaised := False;
    try
      LGroup.RequireFromPassAt('missing', MakeGraphOffset(0, 0, 0), 'A');
    except
      on E: EArgumentException do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.DependencyCount = 0),
      'exact clauses reject an unknown provider atomically');

    LRaised := False;
    try
      LGroup.RequireAnyFromPass('missing', [
        MakeGraphPassMatchTerm(MakeGraphOffset(0, 0, 0), 'A')]);
    except
      on E: EArgumentException do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.DependencyCount = 0),
      'any clauses reject an unknown provider atomically');

    LRaised := False;
    try
      LGroup.RequireFromPassAt('consumer',
        MakeGraphOffset(0, 0, 0), 'candidate');
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.DependencyCount = 0),
      'exact clauses reject their own pass atomically');

    LRaised := False;
    try
      LGroup.RequireAnyFromPass('consumer', [
        MakeGraphPassMatchTerm(MakeGraphOffset(0, 0, 0), 'candidate')]);
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.DependencyCount = 0),
      'any clauses reject their own pass atomically');

    LGraph.DependsOn('source');
    LGraph.SwitchToPass('source');
    LGroup := LGraph.Rules['A'];
    LRaised := False;
    try
      LGroup.RequireFromPassAt('consumer',
        MakeGraphOffset(0, 0, 0), 'candidate');
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.DependencyCount = 0)
      and (DependencySnapshot(LGraph, 1) = '0'),
      'exact clauses reject a dependency cycle atomically');

    LRaised := False;
    try
      LGroup.RequireAnyFromPass('consumer', [
        MakeGraphPassMatchTerm(MakeGraphOffset(0, 0, 0), 'candidate')]);
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.DependencyCount = 0)
      and (DependencySnapshot(LGraph, 1) = '0'),
      'any clauses reject a dependency cycle atomically');
    Check(LGraph.TrySolve(LOptions, LReport),
      'configuration recovers after unknown, self, and cycle failures');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'source';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('A');
    LGraph.Entry[0, 0, 0].Value := 'A';
    LGraph.SwitchToPass('consumer');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('candidate');
    LGraph.AddValue('none');
    LGraph.SelectionCallback := SelectAndAttemptSpatialMutation;
    GSpatialExactMutationRejected := False;
    GSpatialAnyMutationRejected := False;
    LGraph.Run;
    Check(GSpatialExactMutationRejected
      and (LGraph.DependencyCount = 0),
      'exact clauses cannot mutate dependency state during Run');
    Check(GSpatialAnyMutationRejected
      and (LGraph.DependencyCount = 0)
      and (LGraph.Entry[0, 0, 0].Value = 'candidate'),
      'any clauses cannot mutate requirements during Run');
  finally
    LGraph.Free;
  end;
end;

procedure TestDefinitionlessPassModes;
var
  LBefore: String;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := TGraph.Create.Reshape(2, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'source';
    LGraph.Entry[0, 0, 0].Value := 'A';
    LGraph.Entry[1, 0, 0].Value := 'B';

    LGraph.SwitchToPass('legacy-copy');

    LGraph.SwitchToPass('overlay');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.DependsOn('source');
    LGraph.Entry[1, 0, 0].Value := 'O';

    LGraph.SwitchToPass('transform');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.TransformFrom('source');
    LGraph.Entry[1, 0, 0].Value := 'Z';

    Check(LGraph.TrySolve(LOptions, LReport),
      'definitionless legacy, overlay, and transform modes stage together');
    Check((LReport.Passes[0].Disposition = gpdReused)
      and (LReport.Passes[1].Disposition = gpdCopied)
      and (LReport.Passes[2].Disposition = gpdCleared)
      and (LReport.Passes[3].Disposition = gpdCopied),
      'the report distinguishes preserved, cleared, and copied passes');
    Check((SnapshotPass(LGraph, 0) = 'AB/|')
      and (SnapshotPass(LGraph, 1) = 'AB/|')
      and (SnapshotPass(LGraph, 2) = 'O/|')
      and (SnapshotPass(LGraph, 3) = 'AZ/|'),
      'each definitionless mode applies its documented value source');
    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Generated
      and LGraph.PassGraph[3].Entry[0, 0, 0].Generated
      and LGraph.PassGraph[2].Entry[0, 0, 0].Empty
      and (not LGraph.PassGraph[2].Entry[1, 0, 0].Generated)
      and (not LGraph.PassGraph[3].Entry[1, 0, 0].Generated),
      'copy ownership is generated while overlay and destination locks persist');

    LBefore := SnapshotPipelineState(LGraph);
    LGraph.Run;
    Check(SnapshotPipelineState(LGraph) = LBefore,
      'legacy Run applies the same definitionless mode semantics');

    LGraph.SwitchToPass('transform');
    LGraph.PassMode := gpmOverlay;
    Check(LGraph.TryRegenerateFrom('transform', LOptions, LReport),
      'a former transform can selectively regenerate as an overlay');
    Check((LReport.Passes[3].Disposition = gpdCleared)
      and LGraph.PassGraph[3].Entry[0, 0, 0].Empty
      and (LGraph.PassGraph[3].Entry[1, 0, 0].Value = 'Z')
      and (not LGraph.PassGraph[3].Entry[1, 0, 0].Generated),
      'overlay regeneration clears stale generated copy cells but keeps locks');

    LGraph.SwitchToPass('overlay').TransformFrom('source');
    Check(LGraph.TryRegenerateFrom('overlay', LOptions, LReport),
      'an overlay can become an explicit transform source');
    Check((LReport.Passes[2].Disposition = gpdCopied)
      and (LGraph.PassGraph[2].Entry[0, 0, 0].Value = 'A')
      and LGraph.PassGraph[2].Entry[0, 0, 0].Generated
      and (LGraph.PassGraph[2].Entry[1, 0, 0].Value = 'O')
      and (not LGraph.PassGraph[2].Entry[1, 0, 0].Generated),
      'transform regeneration copies only unlocked cells from its named source');
  finally
    LGraph.Free;
  end;
end;

procedure TestSelectiveDagRegeneration;
var
  I: Integer;
  LBiomeEntry: TGraphEntry;
  LBiomeSnapshot: String;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LReportTwin: TGraphSolveReport;
  LRaised: Boolean;
  LRoots: TGraphPassLabels;
  LSnapshot: String;
  LTerrainEntry: TGraphEntry;
  LTerrainSnapshot: String;
  LTwin: TGraph;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := NewSelectiveDagFixture;
  LTwin := NewSelectiveDagFixture;
  try
    Check(LGraph.TrySolve(LOptions, LReport)
      and LTwin.TrySolve(LOptions, LReportTwin),
      'selective DAG twins establish identical committed layers');
    Check(SnapshotPipeline(LGraph) =
      '0:land/|#1:dry/|#2:plains/|#3:trail/|#4:house/|#5:garden/|#',
      'the dry branch establishes the expected complete DAG output');

    LTerrainEntry := LGraph.PassGraph[0].Entry[0, 0, 0];
    LBiomeEntry := LGraph.PassGraph[2].Entry[0, 0, 0];
    LTerrainSnapshot := SnapshotPassState(LGraph, 0);
    LBiomeSnapshot := SnapshotPassState(LGraph, 2);
    LGraph.PassGraph[1].Entry[0, 0, 0].Value := 'river';
    LTwin.PassGraph[1].Entry[0, 0, 0].Value := 'river';
    LGraph.SwitchToPass('foliage');

    Check(LGraph.PassGraph[4].TryRegenerateFrom(
      'hydrology', LOptions, LReport),
      'selective solving coordinates through a child pass receiver');
    Check(ExecutionOrderSnapshot(LReport) = '1,3,4,5',
      'hydrology regeneration executes its exact transitive consumer closure');
    Check((not LReport.Passes[0].Executed)
      and (LReport.Passes[0].ExecutionOrdinal = -1)
      and (LReport.Passes[0].Disposition = gpdReused)
      and (not LReport.Passes[2].Executed)
      and (LReport.Passes[2].ExecutionOrdinal = -1)
      and (LReport.Passes[2].Disposition = gpdReused),
      'terrain and the biome sibling are explicitly reported as reused');
    Check(LReport.Passes[1].Executed
      and (LReport.Passes[1].ExecutionOrdinal = 0)
      and LReport.Passes[3].Executed
      and (LReport.Passes[3].ExecutionOrdinal = 1)
      and LReport.Passes[4].Executed
      and (LReport.Passes[4].ExecutionOrdinal = 2)
      and LReport.Passes[5].Executed
      and (LReport.Passes[5].ExecutionOrdinal = 3),
      'dirty descendants receive contiguous topological ordinals');
    Check(SnapshotPipeline(LGraph) =
      '0:land/|#1:river/|#2:plains/|#3:bridge/|#4:none/|#5:reeds/|#',
      'selective regeneration replaces only the dependent semantic branch');
    Check((SnapshotPassState(LGraph, 0) = LTerrainSnapshot)
      and (SnapshotPassState(LGraph, 2) = LBiomeSnapshot)
      and (LGraph.PassGraph[0].Entry[0, 0, 0] = LTerrainEntry)
      and (LGraph.PassGraph[2].Entry[0, 0, 0] = LBiomeEntry),
      'unaffected layers retain values, ownership, and entry identity');
    Check(LGraph.CurrentPassIndex = 5,
      'selective solving restores the caller-selected pass');
    Check(LGraph.PassGraph[0].RandomIndex(1000)
      = LTwin.PassGraph[0].RandomIndex(1000),
      'selective solving leaves the reused terrain random stream untouched');
    Check(LGraph.PassGraph[2].RandomIndex(1000)
      = LTwin.PassGraph[2].RandomIndex(1000),
      'selective solving leaves the reused biome random stream untouched');

    Check(LTwin.TryRegenerateFrom('hydrology', LOptions, LReportTwin),
      'an equivalent selective twin solves independently');
    Check((SnapshotPipelineState(LTwin) = SnapshotPipelineState(LGraph))
      and (ExecutionOrderSnapshot(LReportTwin)
        = ExecutionOrderSnapshot(LReport)),
      'selective regeneration replays exact state and execution order');
    for I := 0 to Pred(LGraph.TotalPassCount) do
      Check(SamePassSolveReport(LReport.Passes[I],
        LReportTwin.Passes[I]),
        Format('selective replay reproduces pass report %d', [I]));

    SetLength(LRoots, 3);
    LRoots[0] := 'hydrology';
    LRoots[1] := 'hydrology';
    LRoots[2] := 'biome';
    Check(LGraph.TryRegenerateFrom(LRoots, LOptions, LReport),
      'multiple selective roots form one deduplicated dirty union');
    Check(ExecutionOrderSnapshot(LReport) = '1,2,3,4,5',
      'multi-root regeneration executes the canonical union closure');
    LSnapshot := SnapshotPipelineState(LGraph);
    SetLength(LRoots, 0);
    LRaised := False;
    try
      LGraph.TryRegenerateFrom(LRoots, LOptions, LReport);
    except
      on E: EArgumentException do
        LRaised := True;
    end;
    Check(LRaised and (SnapshotPipelineState(LGraph) = LSnapshot),
      'an empty selective root set is rejected without mutation');
  finally
    LTwin.Free;
    LGraph.Free;
  end;
end;

procedure TestSelectiveDagRollback;
var
  I: Integer;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LReportTwin: TGraphSolveReport;
  LSnapshot: String;
  LTwin: TGraph;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := NewSelectiveDagFixture;
  LTwin := NewSelectiveDagFixture;
  try
    Check(LGraph.TrySolve(LOptions, LReport)
      and LTwin.TrySolve(LOptions, LReportTwin),
      'rollback DAG twins establish matching prior output');
    LGraph.PassGraph[1].Entry[0, 0, 0].Value := 'river';
    LTwin.PassGraph[1].Entry[0, 0, 0].Value := 'river';
    LGraph.PassGraph[4].Entry[0, 0, 0].Value := 'house';
    LTwin.PassGraph[4].Entry[0, 0, 0].Value := 'house';
    LGraph.SwitchToPass('biome');
    LSnapshot := SnapshotPipelineState(LGraph);

    Check(not LGraph.TryRegenerateFrom('hydrology', LOptions, LReport),
      'an invalid downstream caller lock aborts selective regeneration');
    Check((LReport.FailedPassIndex = 4)
      and (LReport.Contradiction.Kind = gckPassDependency)
      and (LReport.Contradiction.PassIndex = 4)
      and (LReport.Contradiction.DependencyPassIndex = 1)
      and (LReport.Contradiction.EntryIndex = 0),
      'selective failure identifies the housing-to-hydrology dependency');
    Check(ExecutionOrderSnapshot(LReport) = '1,3,4',
      'a failed report records only passes actually attempted');
    Check(LReport.Passes[1].Executed
      and LReport.Passes[3].Executed
      and LReport.Passes[4].Executed
      and (not LReport.Passes[5].Executed)
      and (LReport.Passes[5].Disposition = gpdNotRun),
      'a descendant after the failed pass is not reported as executed');
    Check((SnapshotPipelineState(LGraph) = LSnapshot)
      and (LGraph.CurrentPassIndex = 2),
      'selective contradiction restores every entry and caller selection');
    for I := 0 to Pred(LGraph.TotalPassCount) do
      Check(LGraph.PassGraph[I].RandomIndex(1000)
        = LTwin.PassGraph[I].RandomIndex(1000),
        Format('selective failure restores random stream %d', [I]));

    LGraph.PassGraph[4].Entry[0, 0, 0].ClearValue;
    LTwin.PassGraph[4].Entry[0, 0, 0].ClearValue;
    Check(LGraph.TryRegenerateFrom('hydrology', LOptions, LReport)
      and LTwin.TryRegenerateFrom('hydrology', LOptions, LReportTwin),
      'repairing the invalid lock permits an exact selective retry');
    Check(SnapshotPipelineState(LGraph) = SnapshotPipelineState(LTwin),
      'a retry after failure matches a twin that never attempted it');
    for I := 0 to Pred(LGraph.TotalPassCount) do
      Check(SamePassSolveReport(LReport.Passes[I],
        LReportTwin.Passes[I]),
        Format('selective retry reproduces pass report %d', [I]));
  finally
    LTwin.Free;
    LGraph.Free;
  end;
end;

procedure TestPreviousConstraintNeedsEarlierPass;
var
  LBaseGroup: TGraphRuleGroup;
  LGraph: TGraph;
  LGroup: TGraphRuleGroup;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
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

  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('land');
    LGraph.AddValue('water');
    LGraph.Entry[0, 0, 0].Value := 'water';
    LGraph.SwitchToPass('foliage');
    LGraph.AddValue('tree');
    LGraph.AddValue('none');
    LGraph.RuleGroups.Remove('tree');
    LBaseGroup := TGraphRuleGroup.Create('tree');
    LBaseGroup.RequirePrevious('land');
    LGraph.RuleGroups.Add('tree', LBaseGroup);
    LGraph.Run;
    Check(LGraph.Entry[0, 0, 0].Value = 'none',
      'legacy Run honors PreviousValues on a public base rule group');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('land');
    LGraph.AddValue('water');
    LGraph.Entry[0, 0, 0].Value := 'water';
    LGraph.SwitchToPass('foliage');
    LGraph.AddValue('tree');
    LGraph.AddValue('none');
    LGraph.RuleGroups.Remove('tree');
    LBaseGroup := TGraphRuleGroup.Create('tree');
    LBaseGroup.RequirePrevious('land');
    LGraph.RuleGroups.Add('tree', LBaseGroup);
    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport)
      and (LGraph.Entry[0, 0, 0].Value = 'none'),
      'TrySolve honors PreviousValues on a public base rule group');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('water');
    LGraph.Entry[0, 0, 0].Value := 'water';
    LGraph.SwitchToPass('foliage');
    LGraph.AddValue('tree')
      .RequirePrevious('land')
      .RequireFromPass('terrain', 'water');
    LGraph.Entry[0, 0, 0].Value := 'tree';
    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport),
      'previous and named alternatives from one source remain OR');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'terrain';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('land');
    LGraph.AddValue('water');
    LGraph.Entry[0, 0, 0].Value := 'land';
    LGraph.SwitchToPass('foliage');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('tree');
    LGraph.AddValue('none');
    LGraph.RuleGroups.Remove('tree');
    LBaseGroup := TGraphRuleGroup.Create('tree');
    LBaseGroup.RequirePrevious('land');
    LGraph.RuleGroups.Add('tree', LBaseGroup);
    LGraph.RuleGroups.Remove('none');
    LBaseGroup := TGraphRuleGroup.Create('none');
    LBaseGroup.RequirePrevious('water');
    LGraph.RuleGroups.Add('none', LBaseGroup);
    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport)
      and (LGraph.Entry[0, 0, 0].Value = 'tree'),
      'base PreviousValues supply an implicit overlay predecessor edge');
    LGraph.PassGraph[0].Entry[0, 0, 0].Value := 'water';
    Check(LGraph.TryRegenerateFrom('terrain', LOptions, LReport)
      and LReport.Passes[1].Executed
      and (LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'none'),
      'the implicit previous-value edge participates in selective closure');
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
  LValues: TGraphValues;
begin
  LGraph := TGraph.Create.Reshape(2, 1, 1);
  try
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('land');
    LGraph[0, 0, 0].Value := 'land';

    LGraph.SwitchToPass('foliage');
    Check(not LGraph.HasDefinition,
      'definition inspection follows the newly selected empty pass');
    LGraph.AddValue('tree');

    Check(LGraph.HasDefinition,
      'definition inspection sees active-pass registered values');
    LValues := LGraph.CopyRegisteredValues;
    Check((Length(LValues) = 1) and (LValues[0] = 'tree'),
      'registered value copies preserve active-pass AddValue order');
    LValues[0] := 'changed';
    Check(LGraph.CopyRegisteredValues[0] = 'tree',
      'registered value inspection returns an independent array');

    Check(LGraph.RuleGroups.ContainsKey('tree'),
      'RuleGroups reads from the active pass');
    Check(not LGraph.RuleGroups.ContainsKey('land'),
      'the active pass does not expose another pass rule group');
    Check(LGraph[0, 0, 0].Empty, 'Entry reads from the active pass');
    Check(LGraph.Planes = LGraph.PassGraph[1].Planes,
      'Planes reads from the active pass');

    Check(LGraph.PassGraph[0].RuleGroups.ContainsKey('land'),
      'pass zero retains its own rule groups');
    Check(LGraph.PassGraph[0].CopyRegisteredValues[0] = 'land',
      'pass graph inspection addresses its own value registry');
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

procedure TestUnassignedContradiction;
var
  LGraph: TGraph;
  LRaised: Boolean;
begin
  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('water');
    LGraph.Entry[0, 0, 0].Value := 'water';

    LGraph.SwitchToPass('foliage');
    LGraph.AddValue('tree').RequirePrevious('land');

    LRaised := False;
    try
      LGraph.Run;
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised,
      'an unassigned cell with no valid value reports a contradiction');
    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Empty,
      'a contradiction cannot commit an empty or invented value');

    GInvalidRecoveryCount := 0;
    LGraph.InvalidStateCallback := ReplaceInvalidWithNone;
    LRaised := False;
    try
      LGraph.Run;
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check((GInvalidRecoveryCount = 1) and LRaised,
      'an invalid-state callback cannot invent a value outside the model');
    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Empty,
      'failed contradiction recovery leaves the destination unassigned');

    LGraph.InvalidStateCallback := MutateInvalidEntryWithInvented;
    LRaised := False;
    try
      LGraph.Run;
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised,
      'direct callback mutation cannot bypass contradiction validation');
    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Empty,
      'failed callback mutation is rolled back to an unassigned entry');

    LGraph.InvalidStateCallback := RepairEmptyDomainWithNone;
    LGraph.Run;
    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'none',
      'the invalid-state callback may repair model state with a valid value');
    Check(LGraph.PassGraph[1].Entry[0, 0, 0].Generated,
      'successful empty-domain recovery commits solver-owned output');
  finally
    LGraph.Free;
  end;
end;

procedure TestWrappedSelfConstraint;
var
  LGraph: TGraph;
  LRaised: Boolean;
begin
  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.AddValue('A').NewRule([gdNorth], 'B');
    LGraph.AddValue('B');
    LGraph.SelectionCallback := SelectFirstValid;

    LRaised := False;
    try
      LGraph.Run;
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised,
      'an incompatible wrapped self-arc reports a contradiction');
    Check(LGraph.Entry[0, 0, 0].Empty,
      'an incompatible wrapped self-arc cannot commit a value');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.AddValue('A').NewRule([gdNorth, gdSouth], 'A');
    LGraph.SelectionCallback := SelectFirstValid;
    LGraph.Run;
    Check(LGraph.Entry[0, 0, 0].Value = 'A',
      'a compatible wrapped self-arc remains solvable');
  finally
    LGraph.Free;
  end;
end;

procedure TestSelfRequiredSupportIsolation;
var
  LGraph: TTestGraph;
  LPass: TTestGraph;
  LRunGraph: TFixedStartGraph;
  LValues: TGraphValues;
begin
  GFailEntryCreateAt := 0;
  LGraph := TTestGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    //B and its inverse X are required-only values whose possible supporter is
    //an unassigned external neighbor. A alone supports itself on the wrapped
    //north/south arcs.
    LGraph.AddValue('B').NewRule([gdEast], 'X', True);
    LGraph.AddValue('A').NewRule([gdNorth], 'A', True);
    LPass := TTestGraph(LGraph.PassGraph[0]);
    LPass.ValidateForTest(LPass.Entry[0, 0, 0], LValues);
    Check((Length(LValues) = 1) and (LValues[0] = 'A'),
      'required self-support is isolated to its own candidate');
  finally
    LGraph.Free;
  end;

  LRunGraph := TFixedStartGraph.Create;
  try
    LRunGraph.Reshape(2, 1, 1);
    LRunGraph.AddValue('B').NewRule([gdEast], 'X', True);
    LRunGraph.AddValue('A').NewRule([gdNorth], 'A', True);
    LRunGraph.SelectionCallback := SelectFirstValid;
    LRunGraph.Run;
    Check(LRunGraph.Entry[0, 0, 0].Value = 'A',
      'Run cannot borrow required self-support from another candidate');
  finally
    LRunGraph.Free;
  end;
end;

procedure TestWeightRegistryAndValidation;
var
  LGraph: TGraph;
  LGroup: TGraph.TParentedGraphRuleGroup;
  LOriginal: TGraph.TParentedGraphRuleGroup;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRaised: Boolean;
begin
  Check(WFC_DEFAULT_VALUE_WEIGHT = 1,
    'the public default value weight is one');

  LGraph := TGraph.Create;
  try
    LOriginal := LGraph.AddValue('A');
    Check(LOriginal.Weight = WFC_DEFAULT_VALUE_WEIGHT,
      'the one-argument AddValue overload installs the default weight');
    LGroup := LGraph.AddValue('A', 5);
    Check((LGroup = LOriginal) and (LGroup.Weight = 5)
      and (LGraph.RuleGroups.Count = 1),
      'the weighted overload updates an existing value without reordering it');
    Check((LGraph.AddValue('A') = LOriginal)
      and (LOriginal.Weight = 5),
      'the compatibility overload never resets an explicit weight');

    LOriginal.NewRule([gdEast], 'B');
    Check(LGraph.Rules['B'].Weight = WFC_DEFAULT_VALUE_WEIGHT,
      'a value introduced by a rule receives the default weight');
    LGraph.AddValue('B', 4);
    Check(LGraph.Rules['B'].Weight = 4,
      'an auto-registered rule target can be weighted explicitly');

    LGraph.CurrentPass := 'first';
    LGraph.SwitchToPass('second');
    LGraph.AddValue('A', 2);
    Check((LGraph.Rules['A'].Weight = 2)
      and (LGraph.PassGraph[0].Rules['A'].Weight = 5),
      'weights belong to their pass instead of leaking across the pipeline');
    LGraph.SwitchToPass(0);

    LRaised := False;
    try
      LGraph.Rules['A'].Weight := 0;
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.Rules['A'].Weight = 5),
      'assigning zero raises and preserves the previous weight');

    LRaised := False;
    try
      LGraph.AddValue('A', -1);
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised and (LGraph.Rules['A'].Weight = 5),
      'an invalid duplicate AddValue call cannot mutate its value');

    LRaised := False;
    try
      LGraph.AddValue('invalid', 0);
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised and (not LGraph.RuleGroups.ContainsKey('invalid')),
      'an invalid new value is rejected before registry mutation');

    LGraph.Reset;
    Check(LGraph.AddValue('fresh').Weight = WFC_DEFAULT_VALUE_WEIGHT,
      'Reset discards weighted model state with the rest of the pass model');
  finally
    LGraph.Free;
  end;

  LOptions := DefaultGraphSolveOptions;
  LGraph := TGraph.Create;
  try
    LGraph.Seed := 1;
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A', High(Integer));
    LGraph.AddValue('B', High(Integer));
    Check(LGraph.TrySolve(LOptions, LReport),
      'equal maximum raw weights normalize to a valid unit model');
    Check((SnapshotPass(LGraph, 0) = 'B/|')
      and (LReport.Passes[0].Decisions = 1),
      'maximum equal weights retain the unit-model seeded choice');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Seed := 0;
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A', High(Integer) - 1);
    LGraph.AddValue('B', 1);
    Check(LGraph.TrySolve(LOptions, LReport),
      'a normalized total exactly equal to High(Integer) is valid');
    Check(SnapshotPass(LGraph, 0) = 'A/|',
      'the maximum legal ticket range is portable under checked arithmetic');
  finally
    LGraph.Free;
  end;
end;

procedure TestWeightScaleAndLegacySelection;
var
  LFirst: TGraph;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LReportScaled: TGraphSolveReport;
  LScaled: TGraph;
  LTwin: TGraph;
begin
  LOptions := DefaultGraphSolveOptions;
  LFirst := NewWeightedScaleFixture(1, 2);
  LScaled := NewWeightedScaleFixture(2, 4);
  try
    Check(LFirst.TrySolve(LOptions, LReport)
      and LScaled.TrySolve(LOptions, LReportScaled),
      'GCD-equivalent weighted models both solve');
    Check((SnapshotPass(LFirst, 0) = 'B/|')
      and (SnapshotPass(LScaled, 0) = 'B/|'),
      'scaled weights reproduce the same exact seeded ticket choice');
    Check(SamePassSolveReport(LReport.Passes[0],
      LReportScaled.Passes[0]),
      'scaled weights reproduce the same solver counters');
    Check(LFirst.RandomIndex(1000) = LScaled.RandomIndex(1000),
      'scaled models leave their random streams in the same state');
    Check(LFirst.TrySolve(LOptions, LReport)
      and (SnapshotPass(LFirst, 0) = 'B/|'),
      'weighted solving rewinds and replays on the same graph');
  finally
    LScaled.Free;
    LFirst.Free;
  end;

  LGraph := NewEqualWeightMrvFixture(1);
  LTwin := NewEqualWeightMrvFixture(7);
  try
    Check(LGraph.TrySolve(LOptions, LReport)
      and LTwin.TrySolve(LOptions, LReportScaled),
      'unit and scaled-equal MRV fixtures both solve');
    Check((SnapshotPass(LGraph, 1) = 'AB/|')
      and (SnapshotPass(LTwin, 1) = SnapshotPass(LGraph, 1)),
      'scaled-equal weights take the exact unit-model observation order');
    Check(SamePassSolveReport(LReport.Passes[1],
      LReportScaled.Passes[1]),
      'scaled-equal weights preserve unit-model solver counters');
    Check(LGraph.PassGraph[1].RandomIndex(1000)
        = LTwin.PassGraph[1].RandomIndex(1000),
      'scaled-equal weights preserve the following unit-model stream state');
  finally
    LTwin.Free;
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  LTwin := TGraph.Create;
  try
    LGraph.Seed := 0;
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A', 1);
    LGraph.AddValue('B', 3);

    LTwin.Seed := 0;
    LTwin.Reshape(1, 1, 1);
    LTwin.WrapNeighbors := False;
    LTwin.AddValue('A');
    LTwin.AddValue('B');

    LGraph.Run;
    LTwin.Run;
    Check((SnapshotPass(LGraph, 0) = 'A/|')
      and (SnapshotPass(LTwin, 0) = 'A/|'),
      'legacy Run keeps its uniform compatibility selection');
    Check(LGraph.RandomIndex(1000) = LTwin.RandomIndex(1000),
      'legacy Run ignores weights without changing random consumption');
  finally
    LTwin.Free;
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  LTwin := TGraph.Create;
  try
    LGraph.Seed := 0;
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A', 1);
    LGraph.AddValue('B', 100);
    LGraph.SelectionCallback := SelectFirstAndCaptureWeights;

    LTwin.Seed := 0;
    LTwin.Reshape(1, 1, 1);
    LTwin.WrapNeighbors := False;
    LTwin.AddValue('A', 1);
    LTwin.AddValue('B', 100);

    GCapturedWeightCount := 0;
    LGraph.Run;
    Check((SnapshotPass(LGraph, 0) = 'A/|')
      and (GCapturedWeightCount = 2)
      and (GCapturedWeights[0] = 1)
      and (GCapturedWeights[1] = 100),
      'a custom legacy callback remains authoritative and can inspect weights');
    Check(LGraph.RandomIndex(1000) = LTwin.RandomIndex(1000),
      'a deterministic custom callback need not consume the graph stream');
  finally
    LTwin.Free;
    LGraph.Free;
  end;
end;

procedure TestWeightedEntropyObservation;
var
  LBottom: TGraph;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LTop: TGraph;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := NewWeightedEntropyFixture;
  try
    GTraversalCount := 0;
    GInvalidRecoveryCount := 0;
    LGraph.SelectionCallback := SelectAndCaptureTraversal;
    LGraph.InvalidStateCallback := ReplaceInvalidWithNone;
    Check(LGraph.TrySolve(LOptions, LReport),
      'the weighted entropy discriminator solves');
    Check(SnapshotPass(LGraph, 1) = 'AC/|',
      'entropy observes the skewed three-value domain before the uniform pair');
    Check((LReport.Passes[0].Decisions = 0)
      and (LReport.Passes[1].Decisions = 2)
      and (LReport.Passes[1].Propagations = 0)
      and (LReport.Passes[1].Contradictions = 0)
      and (LReport.Passes[1].Backtracks = 0),
      'the entropy fixture has stable decision counters');
    Check((GTraversalCount = 0) and (GInvalidRecoveryCount = 0),
      'weighted reference solving still ignores legacy callbacks');
  finally
    LGraph.Free;
  end;

  LBottom := TGraph.Create;
  LTop := TGraph.Create;
  try
    LBottom.Seed := 2;
    LBottom.Reshape(1, 1, 2);
    LBottom.WrapNeighbors := False;
    LBottom.AddValue('A', 1);
    LBottom.AddValue('B', 3);
    Check(LBottom.TrySolve(LOptions, LReport),
      'the bottom-up weighted entropy tie fixture solves');
    Check((SnapshotPass(LBottom, 0) = 'A/|B/|')
      and (LReport.Passes[0].Decisions = 2),
      'weighted entropy ties use bottom-up cell order');

    LTop.Seed := 2;
    LTop.Reshape(1, 1, 2);
    LTop.WrapNeighbors := False;
    LTop.Mode := rmTopDown;
    LTop.AddValue('A', 1);
    LTop.AddValue('B', 3);
    Check(LTop.TrySolve(LOptions, LReport),
      'the top-down weighted entropy tie fixture solves');
    Check(SnapshotPass(LTop, 0) = 'B/|A/|',
      'weighted entropy ties use top-down cell order');
  finally
    LTop.Free;
    LBottom.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Seed := 0;
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.Entry[0, 0, 0].Value := 'allow';
    LGraph.SwitchToPass('choices');
    LGraph.AddValue('A', 1).RequirePrevious('allow');
    LGraph.AddValue('B', 100).RequirePrevious('deny');
    LGraph.AddValue('C', 4).RequirePrevious('allow');
    Check(LGraph.TrySolve(LOptions, LReport),
      'the filtered weighted-domain fixture solves');
    Check((SnapshotPass(LGraph, 1) = 'C/|')
      and (LReport.Passes[1].Decisions = 1),
      'weighted tickets sum only candidates active in the current domain');
  finally
    LGraph.Free;
  end;
end;

procedure TestWeightedBacktracking;
var
  LDiscard: Integer;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LTwin: TGraph;
begin
  LOptions := DefaultGraphSolveOptions;
  LOptions.MaxBacktracks := 2;
  LGraph := NewWeightedEscapeRing(100, 1);
  LTwin := NewWeightedEscapeRing(100, 1);
  try
    Check(LGraph.TrySolve(LOptions, LReport),
      'weighted recovery reaches the viable cyclic alternative');
    Check(SnapshotPass(LGraph, 0) = 'CCC/|',
      'weighted recovery commits the self-compatible ring');
    Check((LReport.Passes[0].Decisions = 3)
      and (LReport.Passes[0].Contradictions = 2)
      and (LReport.Passes[0].Backtracks = 2),
      'weighted alternatives are frozen in cyclic registration order');
    LDiscard := LTwin.RandomIndex(102);
    Check((LDiscard >= 0)
      and (LGraph.RandomIndex(1000) = LTwin.RandomIndex(1000)),
      'successful cyclic recovery consumes exactly one weighted ticket');
  finally
    LTwin.Free;
    LGraph.Free;
  end;

  LOptions.MaxBacktracks := 1;
  LGraph := NewWeightedEscapeRing(100, 1);
  LTwin := NewWeightedEscapeRing(100, 1);
  try
    Check(not LGraph.TrySolve(LOptions, LReport),
      'the weighted ring observes its backtrack limit');
    Check((LReport.Status = gssBacktrackLimit)
      and (LReport.Passes[0].Decisions = 2)
      and (LReport.Passes[0].Contradictions = 2)
      and (LReport.Passes[0].Backtracks = 1),
      'weighted limit counters include both failed cyclic alternatives');
    Check(SnapshotPass(LGraph, 0) = '/|',
      'weighted limit exhaustion commits no partial ring');
    Check(LGraph.RandomIndex(1000) = LTwin.RandomIndex(1000),
      'weighted limit exhaustion restores the pre-call random stream');
  finally
    LTwin.Free;
    LGraph.Free;
  end;

  LOptions.MaxBacktracks := 0;
  LGraph := NewWeightedEscapeRing(1, 100);
  try
    Check(LGraph.TrySolve(LOptions, LReport),
      'a dominant viable value solves without recovery');
    Check((SnapshotPass(LGraph, 0) = 'CCC/|')
      and (LReport.Passes[0].Decisions = 1)
      and (LReport.Passes[0].Contradictions = 0)
      and (LReport.Passes[0].Backtracks = 0),
      'the weighted ticket selects the dominant viable branch directly');
  finally
    LGraph.Free;
  end;
end;

procedure TestWeightedPassesAndAtomicity;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LReportTwin: TGraphSolveReport;
  LRaised: Boolean;
  LSnapshot: String;
  LTwin: TGraph;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := NewWeightedPassFixture;
  try
    Check(LGraph.TrySolve(LOptions, LReport),
      'the weighted multi-pass fixture solves');
    Check(SnapshotPipeline(LGraph) = '0:B/|#1:Y/|#2:Y/|#',
      'each weighted pass uses its independent seeded stream and copy stage');
    Check((Length(LReport.Passes) = 3)
      and (LReport.Passes[0].Decisions = 1)
      and (LReport.Passes[1].Decisions = 1)
      and (LReport.Passes[2].Decisions = 0)
      and (LReport.Passes[0].Contradictions = 0)
      and (LReport.Passes[1].Contradictions = 0),
      'weighted pass reports remain isolated by pass');
    Check(LGraph.CurrentPassIndex = 2,
      'weighted pipeline solving restores the caller-selected pass');
    LSnapshot := SnapshotPass(LGraph, 0);
    LGraph.PassGraph[1].CurrentPass := 'renamed-second';
    LGraph.SwitchToPass('later-copy');
    Check(LGraph.TrySolve(LOptions, LReport)
      and (SnapshotPass(LGraph, 0) = LSnapshot),
      'renaming and appending later passes cannot perturb an earlier weighted stream');
  finally
    LGraph.Free;
  end;

  LGraph := NewWeightedScaleFixture(1, 2);
  LTwin := NewWeightedScaleFixture(1, 2);
  try
    Check(LGraph.TrySolve(LOptions, LReport)
      and LTwin.TrySolve(LOptions, LReportTwin),
      'the weighted rollback twins establish matching prior output');
    LGraph.SwitchToPass('dependent');
    LGraph.AddValue('X').RequirePrevious('never');
    LGraph.Entry[0, 0, 0].Value := 'X';
    LTwin.SwitchToPass('dependent');
    LTwin.AddValue('X').RequirePrevious('never');
    LTwin.Entry[0, 0, 0].Value := 'X';
    LSnapshot := SnapshotPipeline(LGraph);

    Check(not LGraph.TrySolve(LOptions, LReport),
      'a later weighted-pipeline contradiction fails atomically');
    Check((LReport.FailedPassIndex = 1)
      and (LReport.Contradiction.Kind = gckPreviousPass),
      'the weighted rollback report identifies the dependent pass');
    Check((SnapshotPipeline(LGraph) = LSnapshot)
      and LGraph.PassGraph[0].Entry[0, 0, 0].Generated
      and (not LGraph.PassGraph[1].Entry[0, 0, 0].Generated)
      and (LGraph.CurrentPassIndex = 1),
      'later failure preserves weighted output, locks, ownership, and selection');
    Check(LGraph.PassGraph[0].RandomIndex(1000)
        = LTwin.PassGraph[0].RandomIndex(1000),
      'later failure restores the first weighted pass stream');
    Check(LGraph.PassGraph[1].RandomIndex(1000)
        = LTwin.PassGraph[1].RandomIndex(1000),
      'later failure restores the dependent weighted pass stream');

    LGraph.Rules['X'].RequirePrevious('B');
    LTwin.Rules['X'].RequirePrevious('B');
    Check(LGraph.TrySolve(LOptions, LReport)
      and LTwin.TrySolve(LOptions, LReportTwin),
      'repairing the dependent constraint permits an exact retry');
    Check((SnapshotPipeline(LGraph) = '0:B/|#1:X/|#')
      and (SnapshotPipeline(LTwin) = SnapshotPipeline(LGraph))
      and SamePassSolveReport(LReport.Passes[0],
        LReportTwin.Passes[0])
      and SamePassSolveReport(LReport.Passes[1],
        LReportTwin.Passes[1]),
      'a repaired retry matches a twin that never attempted the failure');
  finally
    LTwin.Free;
    LGraph.Free;
  end;

  LGraph := NewWeightedScaleFixture(1, 2);
  LTwin := NewWeightedScaleFixture(1, 2);
  try
    Check(LGraph.TrySolve(LOptions, LReport)
      and LTwin.TrySolve(LOptions, LReportTwin),
      'the overflow twins establish matching weighted output');
    LGraph.SwitchToPass('overflow');
    LGraph.AddValue('X', High(Integer));
    LGraph.AddValue('Y', High(Integer) - 1);
    LTwin.SwitchToPass('overflow');
    LTwin.AddValue('X', High(Integer));
    LTwin.AddValue('Y', High(Integer) - 1);

    LRaised := False;
    try
      LGraph.TrySolve(LOptions, LReport);
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised,
      'normalized pass-weight overflow raises during model preparation');
    Check((SnapshotPipeline(LGraph) = '0:B/|#1:/|#')
      and (LGraph.CurrentPassIndex = 1),
      'overflow preparation commits nothing and restores pass selection');
    Check(LGraph.PassGraph[0].RandomIndex(1000)
        = LTwin.PassGraph[0].RandomIndex(1000),
      'overflow preparation restores the preceding pass stream');
    Check(LGraph.PassGraph[1].RandomIndex(1000)
        = LTwin.PassGraph[1].RandomIndex(1000),
      'overflow preparation restores its malformed pass stream');

    LGraph.Rules['Y'].Weight := High(Integer);
    LTwin.Rules['Y'].Weight := High(Integer);
    Check(LGraph.TrySolve(LOptions, LReport)
      and LTwin.TrySolve(LOptions, LReportTwin),
      'repairing an overflowing ratio permits deterministic retry');
    Check((SnapshotPipeline(LGraph) = '0:B/|#1:Y/|#')
      and (SnapshotPipeline(LTwin) = SnapshotPipeline(LGraph))
      and SamePassSolveReport(LReport.Passes[0],
        LReportTwin.Passes[0])
      and SamePassSolveReport(LReport.Passes[1],
        LReportTwin.Passes[1]),
      'overflow repair replays exactly against the untouched twin');
  finally
    LTwin.Free;
    LGraph.Free;
  end;
end;

procedure TestReferenceWeightedKernel;
var
  LAssignment: TReferenceIntegerArray;
  I: Integer;
  LModel: TReferenceModel;
  LProbe: TReferenceRandomProbe;
  LReport: TReferenceSolveReport;
  LRaised: Boolean;

  procedure InitializeUnconstrainedModel(const ACellCount,
    AValueCount: Integer; out AModel: TReferenceModel);
  var
    LCell: Integer;
    LIndex: Integer;
  begin
    AModel := Default(TReferenceModel);
    AModel.CellCount := ACellCount;
    AModel.ValueCount := AValueCount;
    SetLength(AModel.Neighbors,
      ACellCount * WFC_REFERENCE_DIRECTION_COUNT);
    for LIndex := 0 to High(AModel.Neighbors) do
      AModel.Neighbors[LIndex] := -1;
    SetLength(AModel.Compatibility,
      WFC_REFERENCE_DIRECTION_COUNT * AValueCount * AValueCount);
    SetLength(AModel.RequiredValues, AValueCount);
    SetLength(AModel.RequiredSupport, Length(AModel.Compatibility));
    SetLength(AModel.InitialAllowed, ACellCount * AValueCount);
    SetLength(AModel.InitialFailureKinds, ACellCount);
    SetLength(AModel.LockedValues, ACellCount);
    SetLength(AModel.CellOrder, ACellCount);
    for LCell := 0 to Pred(ACellCount) do
    begin
      AModel.InitialFailureKinds[LCell] := rckEmptyDomain;
      AModel.LockedValues[LCell] := -1;
      AModel.CellOrder[LCell] := LCell;
    end;
  end;

begin
  LProbe := TReferenceRandomProbe.Create;
  try
    InitializeUnconstrainedModel(2, 3, LModel);
    LModel.InitialAllowed[0] := 1;
    LModel.InitialAllowed[1] := 1;
    LModel.InitialAllowed[3] := 1;
    LModel.InitialAllowed[4] := 1;
    LModel.InitialAllowed[5] := 1;
    SetLength(LModel.ValueWeights, 3);
    LModel.ValueWeights[0] := 1;
    LModel.ValueWeights[1] := 1;
    LModel.ValueWeights[2] := 100;
    LProbe.Reset;
    Check(SolveReferenceModel(LModel, 0, LProbe.RandomIndex,
      LAssignment, LReport),
      'the flat kernel solves a weighted entropy model');
    Check((Length(LAssignment) = 2)
      and (LAssignment[0] = 1) and (LAssignment[1] = 2),
      'the flat weighted kernel maps tickets in value order');
    Check((LProbe.CallCount = 2)
      and (LProbe.Count[0] = 102) and (LProbe.Count[1] = 2),
      'the flat kernel observes entropy before drawing active-domain tickets');

    SetLength(LModel.ValueWeights, 0);
    LProbe.Reset;
    Check(SolveReferenceModel(LModel, 0, LProbe.RandomIndex,
      LAssignment, LReport),
      'an omitted kernel weight vector retains implicit unit weights');
    Check((LProbe.CallCount = 2)
      and (LProbe.Count[0] = 2) and (LProbe.Count[1] = 3),
      'implicit unit weights retain the version-1 MRV decision order');

    InitializeUnconstrainedModel(1, 2, LModel);
    LModel.InitialAllowed[0] := 1;
    LModel.InitialAllowed[1] := 1;
    SetLength(LModel.ValueWeights, 2);
    LModel.ValueWeights[0] := 1;
    LModel.ValueWeights[1] := 2;
    LProbe.Reset;
    Check(SolveReferenceModel(LModel, 0, LProbe.RandomIndex,
      LAssignment, LReport)
      and (LAssignment[0] = 1)
      and (LProbe.CallCount = 1) and (LProbe.Count[0] = 3),
      'the kernel exposes the canonical weighted ticket bound');
    LModel.ValueWeights[0] := 2;
    LModel.ValueWeights[1] := 4;
    LProbe.Reset;
    Check(SolveReferenceModel(LModel, 0, LProbe.RandomIndex,
      LAssignment, LReport)
      and (LAssignment[0] = 1)
      and (LProbe.CallCount = 1) and (LProbe.Count[0] = 3),
      'the kernel normalizes scale-equivalent ratios by their GCD');

    InitializeUnconstrainedModel(1, 3, LModel);
    for I := 0 to 2 do
      LModel.InitialAllowed[I] := 1;
    SetLength(LModel.ValueWeights, 2);
    LModel.ValueWeights[0] := 1;
    LModel.ValueWeights[1] := 1;
    LRaised := False;
    try
      SolveReferenceModel(LModel, 0, LProbe.RandomIndex,
        LAssignment, LReport);
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised,
      'the flat kernel rejects a nonempty weight vector of the wrong length');

    SetLength(LModel.ValueWeights, 3);
    LModel.ValueWeights[0] := 1;
    LModel.ValueWeights[1] := 0;
    LModel.ValueWeights[2] := 1;
    LRaised := False;
    try
      SolveReferenceModel(LModel, 0, LProbe.RandomIndex,
        LAssignment, LReport);
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised, 'the flat kernel rejects a zero weight');

    LModel.ValueWeights[1] := -1;
    LRaised := False;
    try
      SolveReferenceModel(LModel, 0, LProbe.RandomIndex,
        LAssignment, LReport);
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised, 'the flat kernel rejects a negative weight');

    LModel.ValueWeights[0] := High(Integer);
    LModel.ValueWeights[1] := High(Integer) - 1;
    LModel.ValueWeights[2] := 1;
    LProbe.Reset;
    LRaised := False;
    try
      SolveReferenceModel(LModel, 0, LProbe.RandomIndex,
        LAssignment, LReport);
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised and (LProbe.CallCount = 0),
      'normalized kernel-weight overflow fails before random observation');
  finally
    LProbe.Free;
  end;
end;

procedure TestReferenceAssignmentExclusions;
var
  I: Integer;
  LAssignment: TReferenceIntegerArray;
  LBaselineAssignment: TReferenceIntegerArray;
  LBaselineReport: TReferenceSolveReport;
  LFoundExclusionTrace: Boolean;
  LModel: TReferenceModel;
  LProbe: TReferenceRandomProbe;
  LReport: TReferenceSolveReport;
  LRaised: Boolean;

  procedure InitializeUnconstrainedModel(const ACellCount,
    AValueCount: Integer; out AModel: TReferenceModel);
  var
    LCell: Integer;
    LIndex: Integer;
  begin
    AModel := Default(TReferenceModel);
    AModel.CellCount := ACellCount;
    AModel.ValueCount := AValueCount;
    SetLength(AModel.Neighbors,
      ACellCount * WFC_REFERENCE_DIRECTION_COUNT);
    for LIndex := 0 to High(AModel.Neighbors) do
      AModel.Neighbors[LIndex] := -1;
    SetLength(AModel.Compatibility,
      WFC_REFERENCE_DIRECTION_COUNT * AValueCount * AValueCount);
    SetLength(AModel.RequiredValues, AValueCount);
    SetLength(AModel.RequiredSupport, Length(AModel.Compatibility));
    SetLength(AModel.InitialAllowed, ACellCount * AValueCount);
    for LIndex := 0 to High(AModel.InitialAllowed) do
      AModel.InitialAllowed[LIndex] := 1;
    SetLength(AModel.InitialFailureKinds, ACellCount);
    SetLength(AModel.LockedValues, ACellCount);
    SetLength(AModel.CellOrder, ACellCount);
    for LCell := 0 to Pred(ACellCount) do
    begin
      AModel.InitialFailureKinds[LCell] := rckEmptyDomain;
      AModel.LockedValues[LCell] := -1;
      AModel.CellOrder[LCell] := LCell;
    end;
  end;

  function SameTraceEvent(const A, B: TReferenceTraceEvent): Boolean;
  begin
    Result := (A.EventId = B.EventId)
      and (A.CauseEventId = B.CauseEventId)
      and (A.Kind = B.Kind)
      and (A.CauseKind = B.CauseKind)
      and (A.EntryIndex = B.EntryIndex)
      and (A.ValueIndex = B.ValueIndex)
      and (A.NeighborIndex = B.NeighborIndex)
      and (A.Direction = B.Direction)
      and (A.DecisionDepth = B.DecisionDepth)
      and (A.DomainCountBefore = B.DomainCountBefore)
      and (A.DomainCountAfter = B.DomainCountAfter);
  end;

  function SameReferenceRun(const AAssignment,
    BAssignment: TReferenceIntegerArray; const AReport,
    BReport: TReferenceSolveReport): Boolean;
  var
    LIndex: Integer;
  begin
    Result := (Length(AAssignment) = Length(BAssignment))
      and (AReport.Status = BReport.Status)
      and (AReport.Decisions = BReport.Decisions)
      and (AReport.Propagations = BReport.Propagations)
      and (AReport.Contradictions = BReport.Contradictions)
      and (AReport.Backtracks = BReport.Backtracks)
      and (AReport.ExcludedAssignments = BReport.ExcludedAssignments)
      and (AReport.Contradiction.Kind = BReport.Contradiction.Kind)
      and (AReport.Contradiction.EntryIndex =
        BReport.Contradiction.EntryIndex)
      and (AReport.Contradiction.NeighborIndex =
        BReport.Contradiction.NeighborIndex)
      and (AReport.Contradiction.Direction =
        BReport.Contradiction.Direction)
      and (Length(AReport.Trace) = Length(BReport.Trace));
    if not Result then
      Exit;
    for LIndex := 0 to High(AAssignment) do
      if AAssignment[LIndex] <> BAssignment[LIndex] then
        Exit(False);
    for LIndex := 0 to High(AReport.Trace) do
      if not SameTraceEvent(AReport.Trace[LIndex],
          BReport.Trace[LIndex]) then
        Exit(False);
  end;

begin
  LProbe := TReferenceRandomProbe.Create;
  try
    InitializeUnconstrainedModel(1, 3, LModel);
    LProbe.Reset;
    Check(SolveReferenceModel(LModel, 3, True, LProbe.RandomIndex,
      LBaselineAssignment, LBaselineReport),
      'an empty exact-exclusion set retains the ordinary kernel solve');
    Check((Length(LBaselineAssignment) = 1)
      and (LBaselineAssignment[0] = 2)
      and (LBaselineReport.ExcludedAssignments = 0),
      'the empty exclusion path retains its first assignment and report');
    LProbe.Reset;
    Check(SolveReferenceModel(LModel, 3, True, LProbe.RandomIndex,
      LAssignment, LReport)
      and SameReferenceRun(LBaselineAssignment, LAssignment,
        LBaselineReport, LReport),
      'the empty exclusion path replays assignment, counters, and trace');

    SetLength(LModel.ExcludedAssignments, 1);
    SetLength(LModel.ExcludedAssignments[0], 1);
    LModel.ExcludedAssignments[0][0] := 2;
    LProbe.Reset;
    Check(SolveReferenceModel(LModel, 1, True, LProbe.RandomIndex,
      LAssignment, LReport)
      and (Length(LAssignment) = 1) and (LAssignment[0] = 0),
      'excluding the first complete assignment resumes at its next alternative');
    Check((LReport.ExcludedAssignments = 1)
      and (LReport.Contradictions = 1)
      and (LReport.Backtracks = 1)
      and (LReport.Decisions = 2)
      and (LProbe.CallCount = 1),
      'exact exclusion recovery is counted and reuses the frozen candidate order');
    LFoundExclusionTrace := False;
    for I := 0 to High(LReport.Trace) do
      if (LReport.Trace[I].Kind = rtekContradiction)
        and (LReport.Trace[I].CauseKind = rtckExcludedAssignment)
        and (LReport.Trace[I].EntryIndex = -1) then
        LFoundExclusionTrace := True;
    Check(LFoundExclusionTrace,
      'trace capture distinguishes an entryless full-assignment exclusion');

    InitializeUnconstrainedModel(2, 2, LModel);
    SetLength(LModel.ExcludedAssignments, 1);
    SetLength(LModel.ExcludedAssignments[0], 2);
    LModel.ExcludedAssignments[0][0] := 1;
    LModel.ExcludedAssignments[0][1] := 1;
    LProbe.Reset;
    Check(SolveReferenceModel(LModel, 1, LProbe.RandomIndex,
      LAssignment, LReport)
      and (Length(LAssignment) = 2)
      and (LAssignment[0] = 1) and (LAssignment[1] = 0),
      'a full-assignment exclusion rejects only the exact multi-cell tuple');
    Check((LReport.ExcludedAssignments = 1)
      and (LReport.Backtracks = 1)
      and (LProbe.CallCount = 2),
      'multi-cell exclusion resumes the deepest frozen decision first');

    InitializeUnconstrainedModel(1, 3, LModel);
    SetLength(LModel.ExcludedAssignments, 3);
    SetLength(LModel.ExcludedAssignments[0], 1);
    SetLength(LModel.ExcludedAssignments[1], 1);
    SetLength(LModel.ExcludedAssignments[2], 1);
    LModel.ExcludedAssignments[0][0] := 2;
    LModel.ExcludedAssignments[1][0] := 0;
    LModel.ExcludedAssignments[2][0] := 1;
    LProbe.Reset;
    Check(not SolveReferenceModel(LModel, 3, LProbe.RandomIndex,
      LAssignment, LReport),
      'excluding every complete assignment exhausts the choice frame');
    Check((LReport.Status = rssContradiction)
      and (LReport.Contradiction.Kind = rckExcludedAssignment)
      and (LReport.Contradiction.EntryIndex = -1)
      and (LReport.ExcludedAssignments = 3)
      and (LReport.Contradictions = 3)
      and (LReport.Backtracks = 3)
      and (Length(LAssignment) = 0),
      'exhaustion retains dedicated exact-exclusion evidence');

    InitializeUnconstrainedModel(1, 2, LModel);
    SetLength(LModel.ExcludedAssignments, 1);
    SetLength(LModel.ExcludedAssignments[0], 0);
    LProbe.Reset;
    LRaised := False;
    try
      SolveReferenceModel(LModel, 1, LProbe.RandomIndex,
        LAssignment, LReport);
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (LProbe.CallCount = 0),
      'an exclusion with the wrong cell count fails before observation');

    SetLength(LModel.ExcludedAssignments[0], 1);
    LModel.ExcludedAssignments[0][0] := 2;
    LProbe.Reset;
    LRaised := False;
    try
      SolveReferenceModel(LModel, 1, LProbe.RandomIndex,
        LAssignment, LReport);
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised and (LProbe.CallCount = 0),
      'an out-of-range excluded value fails before observation');

    LModel.ExcludedAssignments[0][0] := -1;
    LProbe.Reset;
    LRaised := False;
    try
      SolveReferenceModel(LModel, 1, LProbe.RandomIndex,
        LAssignment, LReport);
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised and (LProbe.CallCount = 0),
      'a negative excluded value fails before observation');

    SetLength(LModel.ExcludedAssignments, 2);
    SetLength(LModel.ExcludedAssignments[0], 1);
    SetLength(LModel.ExcludedAssignments[1], 1);
    LModel.ExcludedAssignments[0][0] := 0;
    LModel.ExcludedAssignments[1][0] := 0;
    LProbe.Reset;
    LRaised := False;
    try
      SolveReferenceModel(LModel, 1, LProbe.RandomIndex,
        LAssignment, LReport);
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised and (LProbe.CallCount = 0),
      'duplicate exact exclusions are rejected before observation');

    InitializeUnconstrainedModel(0, 0, LModel);
    SetLength(LModel.ExcludedAssignments, 1);
    SetLength(LModel.ExcludedAssignments[0], 0);
    Check(not SolveReferenceModel(LModel, 0, True, nil,
      LAssignment, LReport),
      'the exact empty assignment can be excluded from a zero-cell model');
    Check((LReport.Status = rssContradiction)
      and (LReport.Contradiction.Kind = rckExcludedAssignment)
      and (LReport.ExcludedAssignments = 1)
      and (Length(LReport.Trace) = 1)
      and (LReport.Trace[0].CauseKind = rtckExcludedAssignment),
      'zero-cell exclusion evidence is bounded and entryless');
  finally
    LProbe.Free;
  end;
end;

procedure TestReferencePropagation;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRaised: Boolean;
begin
  Check(WFC_SOLVER_ALGORITHM_VERSION = 2,
    'the reference solver has an explicit replay version');
  LOptions := DefaultGraphSolveOptions;
  Check(LOptions.MaxBacktracks = 256,
    'the reference solver exposes a stable default backtrack limit');

  LGraph := NewReferenceEqualityFixture;
  try
    Check(LGraph.TrySolve(LOptions, LReport),
      'fixed-point propagation solves a locked equality chain');
    Check(SnapshotPass(LGraph, 0) = 'AAA/|',
      'propagation reaches the end of the chain before observation');
    Check((LReport.Status = gssSolved)
      and (LReport.FailedPassIndex = -1)
      and (LReport.Contradiction.Kind = gckNone),
      'a solved report has no terminal contradiction');
    Check((LReport.Passes[0].Decisions = 0)
      and (LReport.Passes[0].Propagations = 2)
      and (LReport.Passes[0].Contradictions = 0)
      and (LReport.Passes[0].Backtracks = 0),
      'the equality-chain report freezes propagation counters');
    Check((not LGraph.Entry[0, 0, 0].Generated)
      and LGraph.Entry[1, 0, 0].Generated
      and LGraph.Entry[2, 0, 0].Generated,
      'reference solving preserves locks and owns generated cells');

    LOptions.MaxBacktracks := -1;
    LRaised := False;
    try
      LGraph.TrySolve(LOptions, LReport);
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised, 'a negative reference backtrack limit is rejected');
    Check(SnapshotPass(LGraph, 0) = 'AAA/|',
      'an invalid option cannot mutate solved output');
  finally
    LGraph.Free;
  end;

  LOptions := DefaultGraphSolveOptions;
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(0, 1, High(TGraphCoordinate));
    LGraph.AddValue('A');
    Check(LGraph.TrySolve(LOptions, LReport),
      'a defined zero-cell graph solves without traversing empty dimensions');
    Check((LReport.Status = gssSolved)
      and (Length(LReport.Passes) = 1),
      'zero-cell reference solving returns a complete report');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A');
    Check(LGraph.PassGraph[0].TrySolve(LOptions, LReport),
      'calling TrySolve through a pass graph coordinates the root pipeline');
    Check((LGraph.Entry[0, 0, 0].Value = 'A')
      and LGraph.Entry[0, 0, 0].Generated,
      'pass-graph reference solving commits root-owned output');
  finally
    LGraph.Free;
  end;
end;

procedure TestReferenceModeTieBreak;
var
  LBottom: TGraph;
  LFailure: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LTop: TGraph;
begin
  LOptions := DefaultGraphSolveOptions;
  LBottom := TGraph.Create;
  LTop := TGraph.Create;
  try
    LBottom.Seed := 0;
    LBottom.Reshape(1, 1, 2);
    LBottom.WrapNeighbors := False;
    LBottom.AddValue('A');
    LBottom.AddValue('B');
    Check(LBottom.TrySolve(LOptions, LReport),
      'bottom-up reference tie fixture solves');
    Check(SnapshotPass(LBottom, 0) = 'A/|B/|',
      'bottom-up MRV ties visit lower Z first');
    Check((LReport.Passes[0].Decisions = 2)
      and (LReport.Passes[0].Propagations = 0),
      'unconstrained tie decisions do not count as propagation');

    LTop.Seed := 0;
    LTop.Reshape(1, 1, 2);
    LTop.WrapNeighbors := False;
    LTop.Mode := rmTopDown;
    LTop.AddValue('A');
    LTop.AddValue('B');
    Check(LTop.TrySolve(LOptions, LReport),
      'top-down reference tie fixture solves');
    Check(SnapshotPass(LTop, 0) = 'B/|A/|',
      'top-down MRV ties visit upper Z first');
  finally
    LTop.Free;
    LBottom.Free;
  end;

  LFailure := TGraph.Create;
  try
    LFailure.Reshape(1, 1, 2);
    LFailure.WrapNeighbors := False;
    LFailure.Mode := rmTopDown;
    LFailure.AddValue('R').NewRule([gdNorth], 'R', True);
    Check(not LFailure.TrySolve(LOptions, LReport),
      'top-down required propagation reports an unsatisfied domain');
    Check((LReport.Contradiction.Kind = gckRequiredSupport)
      and (LReport.Contradiction.EntryIndex = 1),
      'initial contradiction evidence follows the documented cell order');
  finally
    LFailure.Free;
  end;
end;

procedure TestReferenceMrvSelection;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := TGraph.Create;
  try
    LGraph.Seed := 0;
    LGraph.Reshape(2, 1, 1);
    LGraph.WrapNeighbors := False;
    //A definitionless input pass supplies domains of size three at entry 0
    //and size two at entry 1 in the choices pass.
    LGraph.Entry[0, 0, 0].Value := '3';
    LGraph.Entry[1, 0, 0].Value := '2';
    LGraph.SwitchToPass('choices');
    LGraph.AddValue('A').RequirePrevious(['3', '2']);
    LGraph.AddValue('B').RequirePrevious(['3', '2']);
    LGraph.AddValue('C').RequirePrevious('3');

    Check(LGraph.TrySolve(LOptions, LReport),
      'the unequal-domain MRV fixture solves');
    Check(SnapshotPass(LGraph, 1) = 'AB/|',
      'MRV observes the size-two entry before the lower-index size-three entry');
    Check((LReport.Passes[1].Decisions = 2)
      and (LReport.Passes[1].Contradictions = 0),
      'the MRV fixture records two stable observations');
  finally
    LGraph.Free;
  end;
end;

procedure TestReferenceWrappedArcs;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := TGraph.Create;
  try
    LGraph.Seed := 0;
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A').NewRule([gdEast, gdWest], 'B');
    Check(not LGraph.TrySolve(LOptions, LReport),
      'an alternating wrapped self-arc is contradictory');
    Check((LReport.Status = gssContradiction)
      and (LReport.Contradiction.Kind = gckAdjacency),
      'a wrapped self failure reports adjacency evidence');
    Check(LGraph.Entry[0, 0, 0].Empty,
      'a wrapped self contradiction commits no output');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A').NewRule(AllDirections, 'A');
    Check(LGraph.TrySolve(LOptions, LReport),
      'a compatible wrapped self-arc solves');
    Check(LGraph.Entry[0, 0, 0].Value = 'A',
      'the compatible wrapped singleton commits its value');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.AddValue('K')
      .NewRule([gdWest], 'A')
      .NewRule([gdEast], 'B');
    LGraph.Entry[0, 0, 0].Value := 'K';
    Check(not LGraph.TrySolve(LOptions, LReport),
      'duplicate directions to one wrapped neighbor stay conjunctive');
    Check((LReport.Contradiction.Kind = gckAdjacency)
      and (LReport.Contradiction.NeighborIndex >= 0),
      'duplicate-arc failure identifies the conflicting neighbor');
    Check(LGraph.Entry[1, 0, 0].Empty,
      'duplicate-arc contradiction leaves the unlocked cell empty');
  finally
    LGraph.Free;
  end;
end;

procedure TestReferenceBacktracking;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LTwin: TGraph;
begin
  LOptions := DefaultGraphSolveOptions;
  LOptions.MaxBacktracks := 0;
  LGraph := NewReferenceEscapeRing;
  LTwin := NewReferenceEscapeRing;
  try
    Check(not LGraph.TrySolve(LOptions, LReport),
      'zero backtracks stops after the first failed branch');
    Check((LReport.Status = gssBacktrackLimit)
      and (LReport.Passes[0].Backtracks = 0)
      and (LReport.Passes[0].Contradictions = 1),
      'the zero-limit report distinguishes exhaustion from contradiction');
    Check(SnapshotPass(LGraph, 0) = '/|',
      'backtrack-limit exhaustion is atomic');
    Check(LGraph.RandomIndex(1000) = LTwin.RandomIndex(1000),
      'failed reference solving restores the pre-call random stream');
  finally
    LTwin.Free;
    LGraph.Free;
  end;

  LOptions.MaxBacktracks := 1;
  LGraph := NewReferenceEscapeRing;
  try
    Check(LGraph.TrySolve(LOptions, LReport),
      'one backtrack escapes an initially failing odd-ring branch');
    Check(SnapshotPass(LGraph, 0) = 'CCC/|',
      'bounded recovery commits the self-compatible ring state');
    Check((LReport.Passes[0].Decisions = 2)
      and (LReport.Passes[0].Contradictions = 1)
      and (LReport.Passes[0].Backtracks = 1),
      'successful recovery freezes decision and backtrack counters');
  finally
    LGraph.Free;
  end;

  LOptions.MaxBacktracks := 16;
  LGraph := NewReferenceOddRing;
  try
    Check(not LGraph.TrySolve(LOptions, LReport),
      'a two-value alternating odd ring is proven unsatisfiable');
    Check((LReport.Status = gssContradiction)
      and (LReport.Passes[0].Decisions = 2)
      and (LReport.Passes[0].Contradictions = 2)
      and (LReport.Passes[0].Backtracks = 2),
      'exhausting every odd-ring branch reports a contradiction');
    Check(SnapshotPass(LGraph, 0) = '/|',
      'an exhausted unsatisfiable search commits nothing');
  finally
    LGraph.Free;
  end;
end;

procedure TestReferenceRequiredSupport;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A').NewRule([gdWest], 'R', True);
    LGraph.AddValue('.');
    LGraph.Entry[0, 0, 0].Value := 'A';
    Check(LGraph.TrySolve(LOptions, LReport),
      'a locked required source forces a supported neighbor');
    Check(SnapshotPass(LGraph, 0) = 'AR/|',
      'required support commits the forced neighbor value');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A').NewRule([gdWest], 'R', True);
    LGraph.AddValue('.');
    Check(LGraph.TrySolve(LOptions, LReport),
      'orphan required-only values are removed without contradiction');
    Check(LGraph.Entry[0, 0, 0].Value = '.',
      'the nonrequired fallback survives orphan pruning');
    Check(LReport.Passes[0].Propagations = 2,
      'orphan required candidates are reported as propagations');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A').NewRule([gdWest], 'R', True);
    LGraph.Entry[0, 0, 0].Value := 'R';
    Check(LGraph.TrySolve(LOptions, LReport),
      'a caller lock retains the legacy required-support exemption');
    Check((LGraph.Entry[0, 0, 0].Value = 'R')
      and (not LGraph.Entry[0, 0, 0].Generated),
      'a required-only lock remains caller-owned');
  finally
    LGraph.Free;
  end;
end;

procedure TestReferenceKernelContract;
var
  I: Integer;
  LAssignment: TReferenceIntegerArray;
  LModel: TReferenceModel;
  LReport: TReferenceSolveReport;

  function KernelRelationIndex(const ADirection, ACurrentValue,
    ANeighborValue: Integer): Integer;
  begin
    Result := ((ADirection * LModel.ValueCount + ACurrentValue)
      * LModel.ValueCount) + ANeighborValue;
  end;
begin
  LModel := Default(TReferenceModel);
  LModel.CellCount := 1;
  LModel.ValueCount := 2;
  SetLength(LModel.Neighbors, WFC_REFERENCE_DIRECTION_COUNT);
  for I := 0 to High(LModel.Neighbors) do
    LModel.Neighbors[I] := -1;
  SetLength(LModel.Compatibility,
    WFC_REFERENCE_DIRECTION_COUNT * 2 * 2);
  SetLength(LModel.RequiredValues, 2);
  SetLength(LModel.RequiredSupport, Length(LModel.Compatibility));
  SetLength(LModel.InitialAllowed, 2);
  SetLength(LModel.InitialFailureKinds, 1);
  SetLength(LModel.LockedValues, 1);
  SetLength(LModel.CellOrder, 1);
  LModel.InitialAllowed[0] := 1;
  LModel.InitialAllowed[1] := 1;
  LModel.RequiredValues[1] := 1;
  LModel.InitialFailureKinds[0] := rckEmptyDomain;
  LModel.LockedValues[0] := 1;
  LModel.CellOrder[0] := 0;

  Check(SolveReferenceModel(LModel, 0, nil, LAssignment, LReport),
    'a flat-kernel lock is enforced and exempt from required support');
  Check((Length(LAssignment) = 1) and (LAssignment[0] = 1)
    and (LReport.Decisions = 0),
    'kernel lock filtering is deterministic and does not count as propagation');

  LModel := Default(TReferenceModel);
  LModel.CellCount := 2;
  LModel.ValueCount := 4;
  SetLength(LModel.Neighbors,
    LModel.CellCount * WFC_REFERENCE_DIRECTION_COUNT);
  for I := 0 to High(LModel.Neighbors) do
    LModel.Neighbors[I] := -1;
  LModel.Neighbors[Ord(gdEast)] := 1;
  SetLength(LModel.Compatibility,
    WFC_REFERENCE_DIRECTION_COUNT * LModel.ValueCount * LModel.ValueCount);
  SetLength(LModel.RequiredValues, LModel.ValueCount);
  SetLength(LModel.RequiredSupport, Length(LModel.Compatibility));
  SetLength(LModel.InitialAllowed,
    LModel.CellCount * LModel.ValueCount);
  SetLength(LModel.InitialFailureKinds, LModel.CellCount);
  SetLength(LModel.LockedValues, LModel.CellCount);
  SetLength(LModel.CellOrder, LModel.CellCount);
  for I := 0 to Pred(LModel.CellCount) do
  begin
    LModel.InitialFailureKinds[I] := rckEmptyDomain;
    LModel.LockedValues[I] := -1;
    LModel.CellOrder[I] := I;
  end;

  //Cell zero can be required C=0 or fallback F=1. Cell one can be Good=2
  //or Bad=3. C is adjacent only to Good, while an intentionally malformed
  //support matrix marks only the incompatible Bad pair as required support.
  LModel.InitialAllowed[0] := 1;
  LModel.InitialAllowed[1] := 1;
  LModel.InitialAllowed[6] := 1;
  LModel.InitialAllowed[7] := 1;
  LModel.RequiredValues[0] := 1;
  LModel.Compatibility[KernelRelationIndex(Ord(gdEast), 0, 2)] := 1;
  LModel.Compatibility[KernelRelationIndex(Ord(gdEast), 1, 2)] := 1;
  LModel.Compatibility[KernelRelationIndex(Ord(gdEast), 1, 3)] := 1;
  LModel.RequiredSupport[KernelRelationIndex(Ord(gdEast), 0, 3)] := 1;

  Check(SolveReferenceModel(LModel, 0, nil, LAssignment, LReport),
    'required support is intersected with compatibility inside the kernel');
  Check((Length(LAssignment) = 2) and (LAssignment[0] = 1)
    and (LAssignment[1] = 2),
    'incompatible required support cannot preserve a required candidate');
end;

procedure TestReferenceLocksAndAtomicPasses;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LTwin: TGraph;
begin
  LOptions := DefaultGraphSolveOptions;
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A');
    LGraph.Entry[0, 0, 0].Value := 'outside';
    Check(not LGraph.TrySolve(LOptions, LReport),
      'an unknown lock is rejected by the reference solver');
    Check((LReport.Contradiction.Kind = gckInvalidLock)
      and (LReport.Contradiction.EntryIndex = 0),
      'an unknown lock receives structured contradiction evidence');
    Check((LGraph.Entry[0, 0, 0].Value = 'outside')
      and (not LGraph.Entry[0, 0, 0].Generated),
      'invalid-lock rejection preserves caller state');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A').NewRule([gdEast, gdWest], 'A');
    LGraph.AddValue('B').NewRule([gdEast, gdWest], 'B');
    LGraph.Entry[0, 0, 0].Value := 'A';
    LGraph.Entry[1, 0, 0].Value := 'B';
    Check(not LGraph.TrySolve(LOptions, LReport),
      'incompatible registered locks report a contradiction');
    Check((LReport.Contradiction.Kind = gckAdjacency)
      and (LGraph.Entry[0, 0, 0].Value = 'A')
      and (LGraph.Entry[1, 0, 0].Value = 'B'),
      'adjacency failure preserves both caller locks');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.AddValue('A');
    LGraph.SwitchToPass('copy');
    LGraph.Entry[1, 0, 0].Value := 'locked';
    Check(LGraph.TrySolve(LOptions, LReport),
      'a later definitionless pass copies staged reference output');
    Check((LGraph.PassGraph[0].Entry[0, 0, 0].Value = 'A')
      and (LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'A')
      and LGraph.PassGraph[1].Entry[0, 0, 0].Generated,
      'reference copy output remains solver-owned');
    Check((LGraph.PassGraph[1].Entry[1, 0, 0].Value = 'locked')
      and (not LGraph.PassGraph[1].Entry[1, 0, 0].Generated),
      'a definitionless reference pass preserves its caller lock');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Seed := $12345678;
    LGraph.Reshape(1, 1, 1);
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('A');
    Check(LGraph.TrySolve(LOptions, LReport),
      'the initial pass establishes generated output');
    LGraph.SwitchToPass('dependent');
    LGraph.AddValue('X').RequirePrevious('B');
    LGraph.Entry[0, 0, 0].Value := 'X';

    Check(not LGraph.TrySolve(LOptions, LReport),
      'a later previous-pass contradiction fails the whole pipeline');
    Check((LReport.FailedPassIndex = 1)
      and (LReport.Contradiction.Kind = gckPreviousPass),
      'the report identifies the failed dependent pass');
    Check((LGraph.PassGraph[0].Entry[0, 0, 0].Value = 'A')
      and LGraph.PassGraph[0].Entry[0, 0, 0].Generated,
      'later-pass failure preserves earlier generated output');
    Check((LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'X')
      and (not LGraph.PassGraph[1].Entry[0, 0, 0].Generated),
      'later-pass failure preserves its caller lock');
    Check(LGraph.CurrentPassIndex = 1,
      'failed reference solving restores pass selection');
  finally
    LGraph.Free;
  end;

  GCommitSetCount := 0;
  GFailCommitSetAt := 0;
  LGraph := TCommitFailGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('A');
    LGraph.SwitchToPass('dependent');
    LGraph.AddValue('X').RequirePrevious('B');
    Check(not LGraph.TrySolve(LOptions, LReport),
      'a later contradiction aborts a subclassed pipeline');
    Check(GCommitSetCount = 0,
      'no entry setter runs before the complete pipeline succeeds');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  LTwin := TGraph.Create;
  try
    LGraph.Seed := $A5A5A5A5;
    LGraph.Reshape(1, 1, 1);
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('A');
    LGraph.SwitchToPass('dependent');
    LGraph.AddValue('X').RequirePrevious('B');
    LGraph.Entry[0, 0, 0].Value := 'X';

    LTwin.Seed := $A5A5A5A5;
    LTwin.Reshape(1, 1, 1);
    LTwin.CurrentPass := 'terrain';
    LTwin.AddValue('A');
    LTwin.SwitchToPass('dependent');
    LTwin.AddValue('X').RequirePrevious('B');
    LTwin.Entry[0, 0, 0].Value := 'X';

    Check(LGraph.PassGraph[0].RandomIndex(1000)
      = LTwin.PassGraph[0].RandomIndex(1000),
      'the first pass random streams start in the same advanced state');
    Check(LGraph.PassGraph[1].RandomIndex(1000)
      = LTwin.PassGraph[1].RandomIndex(1000),
      'the dependent pass random streams start in the same advanced state');
    Check(not LGraph.TrySolve(LOptions, LReport),
      'the random-state fixture fails in its later pass');
    Check(LGraph.PassGraph[0].RandomIndex(1000)
      = LTwin.PassGraph[0].RandomIndex(1000),
      'later failure restores the first pass random stream');
    Check(LGraph.PassGraph[1].RandomIndex(1000)
      = LTwin.PassGraph[1].RandomIndex(1000),
      'later failure restores the dependent pass random stream');
  finally
    LTwin.Free;
    LGraph.Free;
  end;
end;

procedure TestReferenceCommitRollbackAndTopology;
var
  LEntry: TGraphEntry;
  LExternal: TGraphEntry;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRaised: Boolean;
  LRules: TGraphRules;
  LTwin: TGraph;
begin
  LOptions := DefaultGraphSolveOptions;
  GCommitSetCount := 0;
  GFailCommitSetAt := 2;
  LGraph := TCommitFailGraph.Create;
  LTwin := TGraph.Create;
  try
    LGraph.Seed := $13579BDF;
    LGraph.Reshape(2, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    LGraph.AddValue('B');

    LTwin.Seed := $13579BDF;
    LTwin.Reshape(2, 1, 1);
    LTwin.WrapNeighbors := False;
    LTwin.AddValue('A');
    LTwin.AddValue('B');
    Check(LGraph.PassGraph[0].RandomIndex(1000)
      = LTwin.PassGraph[0].RandomIndex(1000),
      'the commit-failure random streams start in the same advanced state');

    LRaised := False;
    try
      LGraph.TrySolve(LOptions, LReport);
    except
      on E: Exception do
        LRaised := True;
    end;
    Check(LRaised, 'a commit hook failure is re-raised');
    Check(LGraph.Entry[0, 0, 0].Empty
      and LGraph.Entry[1, 0, 0].Empty
      and (not LGraph.Entry[0, 0, 0].Generated)
      and (not LGraph.Entry[1, 0, 0].Generated),
      'a commit hook failure rolls every entry back atomically');
    Check(LGraph.PassGraph[0].RandomIndex(1000)
      = LTwin.PassGraph[0].RandomIndex(1000),
      'a commit hook failure restores the pre-call random stream');
  finally
    GFailCommitSetAt := 0;
    LTwin.Free;
    LGraph.Free;
  end;

  LGraph := TCommitMutateGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    GCommitMutationSourceIndex := 0;
    GCommitMutationTarget := LGraph.Entry[1, 0, 0];
    Check(LGraph.TrySolve(LOptions, LReport),
      'a hook-created live value cannot become a new commit-time lock');
    Check((LGraph.Entry[0, 0, 0].Value = 'A')
      and LGraph.Entry[0, 0, 0].Generated
      and (LGraph.Entry[1, 0, 0].Value = 'A')
      and LGraph.Entry[1, 0, 0].Generated,
      'frozen ownership overwrites a hook-created later value');
  finally
    GCommitMutationSourceIndex := -1;
    GCommitMutationTarget := nil;
    LGraph.Free;
  end;

  LGraph := TCommitMutateGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    LEntry := LGraph.Entry[1, 0, 0];
    LEntry.Value := 'A';
    GCommitMutationSourceIndex := 0;
    GCommitMutationTarget := LEntry;
    LRaised := False;
    try
      LGraph.TrySolve(LOptions, LReport);
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised,
      'a commit hook cannot rewrite a later caller lock');
    Check(LGraph.Entry[0, 0, 0].Empty
      and (LGraph.Entry[1, 0, 0].Value = 'A')
      and (not LGraph.Entry[1, 0, 0].Generated),
      'later-lock hook corruption rolls the complete commit back');
  finally
    GCommitMutationSourceIndex := -1;
    GCommitMutationTarget := nil;
    LGraph.Free;
  end;

  LGraph := TCommitMutateGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    GCommitMutationSourceIndex := 1;
    GCommitMutationTarget := LGraph.Entry[0, 0, 0];
    LRaised := False;
    try
      LGraph.TrySolve(LOptions, LReport);
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised,
      'a commit hook cannot rewrite an already committed entry');
    Check(LGraph.Entry[0, 0, 0].Empty
      and LGraph.Entry[1, 0, 0].Empty,
      'earlier-entry hook corruption rolls the complete commit back');
  finally
    GCommitMutationSourceIndex := -1;
    GCommitMutationTarget := nil;
    LGraph.Free;
  end;

  LGraph := TCommitIdentityGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    LGraph.SwitchToPass('second');
    LGraph.AddValue('A');
    GCommitIdentityGraph := LGraph;
    GCommitIdentityCount := 0;
    GCommitIdentitySwitchSelection := True;
    Check(LGraph.TrySolve(LOptions, LReport),
      'entry hooks may inspect and switch pass selection during commit');
    Check((GCommitIdentityCount = 4)
      and (GCommitIdentityPasses[0] = 0)
      and (GCommitIdentityPasses[1] = 0)
      and (GCommitIdentityPasses[2] = 1)
      and (GCommitIdentityPasses[3] = 1),
      'every commit hook observes the pass that owns its entry');
    Check(LGraph.CurrentPassIndex = 1,
      'commit-hook pass switches cannot change caller selection');
  finally
    GCommitIdentitySwitchSelection := False;
    GCommitIdentityCount := 0;
    GCommitIdentityGraph := nil;
    LGraph.Free;
  end;

  LGraph := TCommitIdentityGraph.Create;
  LTwin := TCommitIdentityGraph.Create;
  try
    LGraph.Seed := $2468ACE0;
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'clean';
    LGraph.PassMode := gpmOverlay;
    LGraph.AddValue('A');
    LGraph.AddValue('B');
    LGraph.SwitchToPass('dirty');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('X');
    LGraph.AddValue('Y');

    LTwin.Seed := $2468ACE0;
    LTwin.Reshape(1, 1, 1);
    LTwin.WrapNeighbors := False;
    LTwin.CurrentPass := 'clean';
    LTwin.PassMode := gpmOverlay;
    LTwin.AddValue('A');
    LTwin.AddValue('B');
    LTwin.SwitchToPass('dirty');
    LTwin.PassMode := gpmOverlay;
    LTwin.ClearDependencies;
    LTwin.AddValue('X');
    LTwin.AddValue('Y');

    Check(LGraph.TrySolve(LOptions, LReport)
      and LTwin.TrySolve(LOptions, LReport),
      'selective commit random-stream twins establish matching state');
    LGraph.PassGraph[1].Entry[0, 0, 0].ClearValue;
    GCommitRandomGraph := LGraph;
    GCommitRandomPassIndex := 0;
    GCommitRandomDrawCount := 0;
    Check(LGraph.TryRegenerateFrom('dirty', LOptions, LReport)
      and (GCommitRandomDrawCount > 0)
      and (not LReport.Passes[0].Executed)
      and (LReport.Passes[0].Disposition = gpdReused),
      'a selective commit hook can probe a reused pass');
    GCommitRandomGraph := nil;
    GCommitRandomPassIndex := -1;
    Check(LGraph.PassGraph[0].RandomIndex(1000)
      = LTwin.PassGraph[0].RandomIndex(1000),
      'successful selective commit restores a reused pass random stream');
  finally
    GCommitRandomGraph := nil;
    GCommitRandomPassIndex := -1;
    GCommitRandomDrawCount := 0;
    LTwin.Free;
    LGraph.Free;
  end;

  LExternal := TGraphEntry.Create;
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.AddValue('A');
    LEntry := LGraph.Entry[0, 0, 0];
    LEntry[gdEast] := LExternal;
    LRaised := False;
    try
      LGraph.TrySolve(LOptions, LReport);
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised,
      'the reference solver rejects topology outside graph storage');
    Check(LGraph.Entry[0, 0, 0].Empty,
      'malformed external topology cannot commit output');
    LEntry[gdEast] := nil;
  finally
    LGraph.Free;
    LExternal.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A');
    LGraph.RuleGroups['A'].Value := 'mismatch';
    LRaised := False;
    try
      LGraph.TrySolve(LOptions, LReport);
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised,
      'the reference solver rejects a mismatched rule-group identity');
    Check(LGraph.Entry[0, 0, 0].Empty,
      'mismatched model identity cannot commit output');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A').NewRule([gdNorth], ['A']);
    LRules := LGraph.Rules['A'].Rules;
    SetLength(LRules, Succ(Length(LRules)));
    LRules[High(LRules)] := LRules[0];
    LGraph.Rules['A'].Rules := LRules;
    LRaised := False;
    try
      LGraph.TrySolve(LOptions, LReport);
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised,
      'the reference solver rejects duplicate directional rules');
    Check(LGraph.Entry[0, 0, 0].Empty,
      'duplicate directional rules cannot commit output');
  finally
    LGraph.Free;
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.AddValue('A');
    LGraph.RuleGroups.Remove('A');
    LGraph.RuleGroups.Add('A', nil);
    LRaised := False;
    try
      LGraph.TrySolve(LOptions, LReport);
    except
      on E: EInvalidOperation do
        LRaised := True;
    end;
    Check(LRaised, 'the reference solver rejects a nil rule group');
    Check(LGraph.Entry[0, 0, 0].Empty,
      'a nil rule group cannot commit output');
  finally
    LGraph.Free;
  end;
end;

procedure TestReferenceReplayAndLegacyIsolation;
var
  LFresh: TGraph;
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LReport1: TGraphSolveReport;
  LReport2: TGraphSolveReport;
  LSnapshot: String;
begin
  LOptions := DefaultGraphSolveOptions;
  LOptions.MaxBacktracks := 1;
  LGraph := NewReferenceEscapeRing;
  LFresh := NewReferenceEscapeRing;
  try
    GTraversalCount := 0;
    GInvalidRecoveryCount := 0;
    LGraph.SelectionCallback := SelectAndCaptureTraversal;
    LGraph.InvalidStateCallback := ReplaceInvalidWithNone;
    Check(LGraph.TrySolve(LOptions, LReport1),
      'reference replay fixture solves with legacy callbacks installed');
    LSnapshot := SnapshotPipeline(LGraph);
    Check((GTraversalCount = 0) and (GInvalidRecoveryCount = 0),
      'reference solving does not invoke legacy callbacks');

    Check(LGraph.TrySolve(LOptions, LReport2),
      'reference solving replays on an existing graph');
    Check((SnapshotPipeline(LGraph) = LSnapshot)
      and SamePassSolveReport(LReport1.Passes[0], LReport2.Passes[0]),
      'repeated reference solving reproduces output and counters');
    Check((LReport2.Seed = 0)
      and (LReport2.RandomAlgorithmVersion = WFC_RANDOM_ALGORITHM_VERSION)
      and (LReport2.SolverAlgorithmVersion = WFC_SOLVER_ALGORITHM_VERSION),
      'the replay report captures both algorithm identities');

    Check(LFresh.TrySolve(LOptions, LReport2),
      'a fresh graph reproduces the reference solution');
    Check(SnapshotPipeline(LFresh) = LSnapshot,
      'reference replay is stable across graph instances');
  finally
    LFresh.Free;
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
    Check((LGraph.PassMode = gpmLegacy)
      and (LGraph.DependencyCount = 0)
      and (LGraph.TransformSourceIndex = -1),
      'reset clears pass modes, dependencies, and transform bindings');
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
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.DependsOn('terrain');

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
    Check((LGraph.PassGraph[1].PassMode = gpmOverlay)
      and (DependencySnapshot(LGraph, 1) = '0'),
      'a failed reset preserves the old dependency plan');
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
  GInitializeSetSeed := False;
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

    LGraph.Seed := $DEADBEEF;
    GInitializeSetSeed := True;
    LRaised := False;
    try
      LGraph.Reset;
    except
      on E: Exception do
        LRaised := True;
    end;
    GInitializeSetSeed := False;
    Check(LRaised,
      'a reset initializer cannot mutate the pipeline seed');
    Check((LGraph.Seed = $DEADBEEF)
      and (LGraph.TotalPassCount = 2)
      and (LGraph.CurrentPass = 'candidate'),
      'failed seed mutation preserves the old pipeline and seed');

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
    GInitializeSetSeed := False;
    GInitializeSetWrap := False;
    GFailPassInitializeAt := 0;
    LGraph.Free;
  end;
end;

begin
  WriteLn('WFC conformance suite');
  WriteLn('=====================');

  RunTest('graph entry lifecycle', @TestGraphEntry);
  RunTest('portable seeded random source', @TestPortableRandomSource);
  {$IFNDEF PAS2JS}
  RunTest('explicit seed host random isolation',
    @TestExplicitSeedHostRandomIsolation);
  {$ENDIF}
  RunTest('seeded replay', @TestSeededReplay);
  RunTest('seeded multi-pass replay', @TestSeededMultiPassReplay);
  RunTest('seeded wrapped top-down replay',
    @TestSeededWrappedTopDownReplay);
  RunTest('random callback pass identity', @TestRandomCallbackPassIdentity);
  RunTest('seed mutation during run', @TestSeedMutationDuringRun);
  RunTest('large iterative traversal', @TestLargeIterativeTraversal);
  RunTest('invalid start coordinate', @TestInvalidStartCoordinate);
  RunTest('iterative traversal compatibility',
    @TestIterativeTraversalCompatibility);
  RunTest('rule group basics', @TestRuleGroup);
  RunTest('explicit directional denial model', @TestExplicitDenyModel);
  RunTest('entry-domain storage', @TestAllowedValueStorage);
  RunTest('entry-domain solving', @TestAllowedValueSolving);
  RunTest('entry-domain lifecycle', @TestAllowedValueLifecycle);
  RunTest('public compatibility types', @TestCompatibilityTypes);
  RunTest('inverse rule generation', @TestInverseRules);
  RunTest('required rule enforcement', @TestRequiredRules);
  RunTest('conjunctive required constraints', @TestConjunctiveRequiredRules);
  RunTest('pass dependency configuration', @TestDependencyDagConfiguration);
  RunTest('previous-pass constraint placement',
    @TestPreviousConstraintNeedsEarlierPass);
  RunTest('graph shape and neighbors', @TestGraphShapeAndNeighbors);
  RunTest('stable pass selection', @TestStablePassSelection);
  RunTest('deterministic DAG execution', @TestDeterministicDagExecution);
  RunTest('named pass requirements', @TestNamedPassRequirements);
  RunTest('spatial pass exact offsets', @TestSpatialPassExactOffsets);
  RunTest('spatial pass existential clauses', @TestSpatialPassAnyClauses);
  RunTest('spatial pass clause composition',
    @TestSpatialPassClauseComposition);
  RunTest('spatial pass wrapping arithmetic', @TestSpatialPassWrapping);
  RunTest('spatial pass compatibility and parity',
    @TestSpatialPassCompatibilityAndParity);
  RunTest('spatial pass ownership and validation',
    @TestSpatialPassOwnershipAndValidation);
  RunTest('spatial pass configuration guards',
    @TestSpatialPassConfigurationGuards);
  RunTest('definitionless pass modes', @TestDefinitionlessPassModes);
  RunTest('selective DAG regeneration', @TestSelectiveDagRegeneration);
  RunTest('selective DAG rollback', @TestSelectiveDagRollback);
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
  RunTest('unassigned contradiction handling',
    @TestUnassignedContradiction);
  RunTest('wrapped self constraints', @TestWrappedSelfConstraint);
  RunTest('self required-support isolation',
    @TestSelfRequiredSupportIsolation);
  RunTest('weighted registry and validation',
    @TestWeightRegistryAndValidation);
  RunTest('weight scale and legacy selection',
    @TestWeightScaleAndLegacySelection);
  RunTest('weighted entropy observation',
    @TestWeightedEntropyObservation);
  RunTest('weighted bounded backtracking',
    @TestWeightedBacktracking);
  RunTest('weighted passes and atomicity',
    @TestWeightedPassesAndAtomicity);
  RunTest('reference weighted-kernel contract',
    @TestReferenceWeightedKernel);
  RunTest('reference exact-assignment exclusions',
    @TestReferenceAssignmentExclusions);
  RunTest('reference fixed-point propagation', @TestReferencePropagation);
  RunTest('reference mode tie-breaking', @TestReferenceModeTieBreak);
  RunTest('reference MRV selection', @TestReferenceMrvSelection);
  RunTest('reference wrapped arcs', @TestReferenceWrappedArcs);
  RunTest('reference bounded backtracking', @TestReferenceBacktracking);
  RunTest('reference required support', @TestReferenceRequiredSupport);
  RunTest('reference flat-kernel contract', @TestReferenceKernelContract);
  RunTest('reference locks and atomic passes',
    @TestReferenceLocksAndAtomicPasses);
  RunTest('reference commit rollback and topology',
    @TestReferenceCommitRollbackAndTopology);
  RunTest('reference replay and legacy isolation',
    @TestReferenceReplayAndLegacyIsolation);
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
