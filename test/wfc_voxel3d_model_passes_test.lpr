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
program wfc_voxel3d_model_passes_test;

{$mode delphi}{$H+}
{$R+}
{$Q+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_model,
  wfc_voxel3d,
  wfc_voxel3d_passes;

type
  TTestProcedure = procedure;

const
  SOURCE_A = 'A';
  SOURCE_B = 'B';
  TARGET_FACING = 'facing';
  TARGET_NONE = 'none';

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

function TokensOf(const AValues: array of String): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do
    Result[I] := TWfcModelToken(AValues[I]);
end;

function ModelTermsOf(
  const ATerms: array of TVoxel3DModelPassSpatialTerm):
  TVoxel3DModelPassSpatialTerms;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ATerms));
  for I := 0 to High(ATerms) do
    Result[I] := ATerms[I];
end;

function Selector(const APrototypeId: String;
  const ARotations: TVoxel3DRotations): TVoxel3DPassVariantSelector;
begin
  Result := MakeVoxel3DPassVariantSelector(APrototypeId, ARotations);
end;

function AllSockets(const AToken: String): TVoxel3DSockets;
begin
  Result := MakeVoxel3DSockets(AToken, AToken, AToken, AToken,
    AToken, AToken);
end;

function NewSourceModel(const ARank: Integer;
  const ABoundary: TWfcModelBoundary;
  const AFirstWeight: Integer): TWfcModel;
var
  D: TWfcModelDirection;
  Directions: TWfcModelDirections;
  I, J, Slot: Integer;
  Relations, Weights: TWfcModelIntegerArray;
  Shapes: TWfcModelSampleShapes;
  Tokens: TWfcModelTokens;
begin
  case ARank of
    1: Directions := [wmdEast, wmdWest];
    2: Directions := [wmdNorth, wmdEast, wmdSouth, wmdWest];
    3: Directions := [wmdNorth, wmdEast, wmdSouth, wmdWest,
      wmdUp, wmdDown];
  else
    raise ERangeError.Create('test model rank is out of bounds');
  end;
  Tokens := TokensOf([SOURCE_A, SOURCE_B]);
  SetLength(Weights, 2);
  Weights[0] := AFirstWeight;
  Weights[1] := 1;
  SetLength(Relations, WfcModelStoredDirectionCount(ARank) * 4);
  for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    if D in Directions then
      for I := 0 to 1 do
        for J := 0 to 1 do
        begin
          Slot := ((Ord(D) * 2) + I) * 2 + J;
          Relations[Slot] := 1;
        end;
  SetLength(Shapes, 1);
  case ARank of
    1: Shapes[0] := MakeWfcModelSampleShape(2, 1);
    2: Shapes[0] := MakeWfcModelSampleShape(2, 2);
    3: Shapes[0] := MakeWfcModelSampleShape(2, 1, 2);
  end;
  Result := TWfcModel.Create(ARank, Shapes, ABoundary, wmsNone,
    Directions, Tokens, Weights, Relations);
end;

function NewTargetKit: TVoxel3DKit;
var
  Pairs: TVoxel3DSocketPairs;
  Prototypes: TVoxel3DPrototypes;
begin
  SetLength(Prototypes, 2);
  Prototypes[0] := MakeVoxel3DPrototype(TARGET_FACING, 'target', 1,
    AllSockets('open'), ALL_VOXEL3D_ROTATIONS, [v3pfEmpty], []);
  Prototypes[1] := MakeVoxel3DPrototype(TARGET_NONE, 'target', 1,
    AllSockets('open'), [v3r0], [v3pfEmpty], []);
  SetLength(Pairs, 1);
  Pairs[0] := MakeVoxel3DSocketPair('open', 'open');
  Result := TVoxel3DKit.Create('model-pass-target', Prototypes, Pairs);
end;

function NewSingleTargetKit: TVoxel3DKit;
var
  Pairs: TVoxel3DSocketPairs;
  Prototypes: TVoxel3DPrototypes;
begin
  SetLength(Prototypes, 1);
  Prototypes[0] := MakeVoxel3DPrototype(TARGET_NONE, 'target', 1,
    AllSockets('open'), [v3r0], [v3pfEmpty], []);
  SetLength(Pairs, 1);
  Pairs[0] := MakeVoxel3DSocketPair('open', 'open');
  Result := TVoxel3DKit.Create('model-pass-single-target',
    Prototypes, Pairs);
end;

function FindVariant(const AAdapter: TVoxel3DGraphAdapter;
  const APrototypeId: String; const ARotation: TVoxel3DRotation;
  out AIndex: Integer): Boolean;
var
  I: Integer;
  V: TVoxel3DVariant;
begin
  for I := 0 to AAdapter.VariantCount - 1 do
  begin
    V := AAdapter.VariantAt(I);
    if (V.PrototypeId = APrototypeId) and (V.Rotation = ARotation) then
    begin
      AIndex := I;
      Exit(True);
    end;
  end;
  AIndex := -1;
  Result := False;
end;

function CompleteProjectionRules: TVoxel3DModelPassProjectionRules;
begin
  Result := nil;
  SetLength(Result, 2);
  Result[0] := MakeVoxel3DModelPassProjectionRule(
    Selector(TARGET_FACING, ALL_VOXEL3D_ROTATIONS),
    TokensOf([SOURCE_A, SOURCE_B]));
  Result[1] := MakeVoxel3DModelPassProjectionRule(
    Selector(TARGET_NONE, [v3r0]), TokensOf([SOURCE_B]));
end;

procedure CreateFixture(const AWidth, AHeight, ADepth: Integer;
  const AWrap: Boolean; out AGraph: TGraph; out AModel: TWfcModel;
  out AKit: TVoxel3DKit; out AAdapter: TVoxel3DGraphAdapter);
var
  Boundary: TWfcModelBoundary;
begin
  AGraph := nil;
  AModel := nil;
  AKit := nil;
  AAdapter := nil;
  try
    if AWrap then Boundary := wmbWrap else Boundary := wmbOpen;
    AModel := NewSourceModel(3, Boundary, 3);
    AKit := NewTargetKit;
    AGraph := TGraph.Create;
    AGraph.Reshape(AWidth, AHeight, ADepth);
    AGraph.WrapNeighbors := AWrap;
    AGraph.CurrentPass := 'source-model';
    AGraph.PassMode := gpmOverlay;
    ApplyModelToGraph(AModel, AGraph);
    AGraph.SwitchToPass('target-kit');
    AGraph.PassMode := gpmOverlay;
    AAdapter := AKit.ApplyToGraph(AGraph);
  except
    AAdapter.Free;
    AGraph.Free;
    AKit.Free;
    AModel.Free;
    raise;
  end;
end;

procedure FreeFixture(var AGraph: TGraph; var AModel: TWfcModel;
  var AKit: TVoxel3DKit; var AAdapter: TVoxel3DGraphAdapter);
begin
  AAdapter.Free;
  AGraph.Free;
  AKit.Free;
  AModel.Free;
  AAdapter := nil;
  AGraph := nil;
  AKit := nil;
  AModel := nil;
end;

function SolveOptions: TGraphSolveOptions;
begin
  Result := DefaultGraphSolveOptions;
  Result.MaxBacktracks := 4096;
end;

procedure TestModelDefinitionWitness;
var
  G: TGraph;
  M: TWfcModel;

  procedure CheckShape(const ARank, AWidth, AHeight, ADepth: Integer;
    const AExpected: Boolean; const ALabel: String);
  var
    LocalGraph: TGraph;
    LocalModel: TWfcModel;
  begin
    LocalGraph := TGraph.Create;
    LocalModel := NewSourceModel(ARank, wmbOpen, 3);
    try
      LocalGraph.Reshape(AWidth, AHeight, ADepth);
      LocalGraph.WrapNeighbors := False;
      LocalGraph.CurrentPass := 'model';
      ApplyModelToGraph(LocalModel, LocalGraph);
      Check(WfcModelDefinitionMatchesGraph(LocalModel,
        LocalGraph.PassGraph[0]) = AExpected, ALabel);
    finally
      LocalGraph.Free;
      LocalModel.Free;
    end;
  end;

begin
  CheckShape(1, 3, 1, 1, True,
    'rank-1 witnesses accept a one-dimensional graph shape');
  CheckShape(1, 3, 2, 1, False,
    'rank-1 witnesses reject an active height axis');
  CheckShape(2, 3, 2, 1, True,
    'rank-2 witnesses accept a planar graph shape');
  CheckShape(2, 3, 2, 2, False,
    'rank-2 witnesses reject an active depth axis');
  CheckShape(3, 3, 2, 2, True,
    'rank-3 witnesses accept a volume graph shape');

  G := TGraph.Create;
  M := NewSourceModel(3, wmbOpen, 3);
  try
    G.Reshape(2, 1, 2);
    G.WrapNeighbors := True;
    G.CurrentPass := 'model';
    ApplyModelToGraph(M, G);
    Check(not WfcModelDefinitionMatchesGraph(M, G.PassGraph[0]),
      'the witness rejects a boundary-policy mismatch');
  finally
    G.Free;
    M.Free;
  end;

  G := TGraph.Create;
  M := NewSourceModel(3, wmbOpen, 3);
  try
    G.Reshape(2, 1, 2);
    G.WrapNeighbors := False;
    G.CurrentPass := 'model';
    ApplyModelToGraph(M, G);
    G.PassGraph[0].SetAllowedValues(0, 0, 0, SOURCE_A);
    Check(WfcModelDefinitionMatchesGraph(M, G.PassGraph[0]),
      'entry domains are additive to the exact model definition');
    G.PassGraph[0].Rules[SOURCE_A].Weight := 4;
    Check(not WfcModelDefinitionMatchesGraph(M, G.PassGraph[0]),
      'the witness rejects a changed source weight');
    G.PassGraph[0].Rules[SOURCE_A].Weight := 3;
    G.PassGraph[0].Rules[SOURCE_A].DenyAll([gdNorth]);
    Check(not WfcModelDefinitionMatchesGraph(M, G.PassGraph[0]),
      'the witness rejects changed finite local support');
  finally
    G.Free;
    M.Free;
  end;
end;

procedure TestProjectionAndGuards;
var
  Adapter: TVoxel3DGraphAdapter;
  Allowed: TWfcModelTokens;
  G: TGraph;
  Kit: TVoxel3DKit;
  M, WrongBoundary: TWfcModel;
  NoneIndex, Facing90Index: Integer;
  InitialDependencies: Integer;
  Raised: Boolean;
  Report: TGraphSolveReport;
  Rules: TVoxel3DModelPassProjectionRules;
  Scene: TVoxel3DScene;
begin
  Allowed := TokensOf([SOURCE_A]);
  SetLength(Rules, 1);
  Rules[0] := MakeVoxel3DModelPassProjectionRule(
    Selector(TARGET_NONE, [v3r0]), Allowed);
  Allowed[0] := SOURCE_B;
  Check(Rules[0].AllowedSourceTokens[0] = SOURCE_A,
    'model projection construction deep-copies source tokens');

  CreateFixture(2, 1, 1, False, G, M, Kit, Adapter);
  WrongBoundary := nil;
  Scene := nil;
  try
    InitialDependencies := Adapter.AppliedGraph.DependencyCount;
    Rules := CompleteProjectionRules;
    ValidateVoxel3DProjectionFromModelPass(Kit, Adapter, M, 0, Rules);
    Check(Adapter.AppliedGraph.DependencyCount = InitialDependencies,
      'model projection validation does not mutate dependencies');

    SetLength(Rules, 1);
    Rules[0] := MakeVoxel3DModelPassProjectionRule(
      Selector(TARGET_FACING, ALL_VOXEL3D_ROTATIONS),
      TokensOf([SOURCE_A]));
    Raised := False;
    try
      RequireVoxel3DProjectionFromModelPass(Kit, Adapter, M, 0, Rules);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and (Adapter.AppliedGraph.DependencyCount =
      InitialDependencies),
      'an incomplete target map is rejected before dependency mutation');

    Rules := CompleteProjectionRules;
    Rules[0].AllowedSourceTokens := TokensOf([SOURCE_A, SOURCE_A]);
    Raised := False;
    try
      RequireVoxel3DProjectionFromModelPass(Kit, Adapter, M, 0, Rules);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and (Adapter.AppliedGraph.DependencyCount =
      InitialDependencies),
      'duplicate model tokens are rejected atomically');

    Rules := CompleteProjectionRules;
    Rules[0].AllowedSourceTokens := TokensOf(['unknown']);
    Raised := False;
    try
      RequireVoxel3DProjectionFromModelPass(Kit, Adapter, M, 0, Rules);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and (Adapter.AppliedGraph.DependencyCount =
      InitialDependencies),
      'tokens outside the model vocabulary are rejected atomically');

    WrongBoundary := NewSourceModel(3, wmbWrap, 3);
    Rules := CompleteProjectionRules;
    Raised := False;
    try
      ValidateVoxel3DProjectionFromModelPass(Kit, Adapter,
        WrongBoundary, 0, Rules);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and (Adapter.AppliedGraph.DependencyCount =
      InitialDependencies),
      'a model that does not witness the source pass is rejected atomically');

    Raised := False;
    try
      ValidateVoxel3DProjectionFromModelPass(Kit, Adapter, nil, 0, Rules);
    except
      on E: EArgumentNilException do Raised := True;
    end;
    Check(Raised, 'a nil source model is rejected');
    Raised := False;
    try
      ValidateVoxel3DProjectionFromModelPass(Kit, Adapter, M,
        Adapter.PassIndex, Rules);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised, 'a target pass cannot also be its model source');
    Raised := False;
    try
      ValidateVoxel3DProjectionFromModelPass(Kit, Adapter, M, -1, Rules);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and (Adapter.AppliedGraph.DependencyCount =
      InitialDependencies),
      'an out-of-range source pass is rejected atomically');

    Rules := CompleteProjectionRules;
    RequireVoxel3DProjectionFromModelPass(Kit, Adapter, M, 0, Rules);
    Check((Adapter.AppliedGraph.DependencyCount = 1) and
      (Adapter.AppliedGraph.DependencyIndex[0] = 0),
      'a complete model projection installs its source dependency');
    RequireVoxel3DProjectionFromModelPass(Kit, Adapter, M, 0, Rules);
    Check(Adapter.AppliedGraph.DependencyCount = 1,
      'reinstalling an identical projection keeps one dependency edge');

    Check(FindVariant(Adapter, TARGET_FACING, v3r90, Facing90Index) and
      FindVariant(Adapter, TARGET_NONE, v3r0, NoneIndex),
      'projection fixture resolves typed target variants');
    G.PassGraph[0].SetAllowedValues(0, 0, 0, SOURCE_A);
    G.PassGraph[0].SetAllowedValues(1, 0, 0, SOURCE_B);
    Adapter.AppliedGraph.SetAllowedValues(0, 0, 0,
      Adapter.VariantGraphKeyAt(Facing90Index));
    Adapter.AppliedGraph.SetAllowedValues(1, 0, 0,
      Adapter.VariantGraphKeyAt(NoneIndex));
    Check(G.TrySolve(SolveOptions, Report),
      'a many-to-many public-token projection solves');
    Scene := Kit.CaptureScene(Adapter);
    Check((G.PassGraph[0].Entry[0, 0, 0].Value = SOURCE_A) and
      (G.PassGraph[0].Entry[1, 0, 0].Value = SOURCE_B) and
      (Scene.VariantAtCell(0, 0, 0).PrototypeId = TARGET_FACING) and
      (Scene.VariantAtCell(0, 0, 0).Rotation = v3r90) and
      (Scene.VariantAtCell(1, 0, 0).PrototypeId = TARGET_NONE),
      'source output stays public while capture returns typed target variants');
  finally
    Scene.Free;
    WrongBoundary.Free;
    FreeFixture(G, M, Kit, Adapter);
  end;
end;

procedure TestCycleGuard;
var
  Adapter: TVoxel3DGraphAdapter;
  G: TGraph;
  Kit: TVoxel3DKit;
  M: TWfcModel;
  Raised: Boolean;
  Rules: TVoxel3DModelPassProjectionRules;
begin
  Adapter := nil;
  G := nil;
  Kit := nil;
  M := nil;
  try
    Kit := NewTargetKit;
    M := NewSourceModel(3, wmbOpen, 3);
    G := TGraph.Create;
    G.Reshape(2, 1, 1);
    G.WrapNeighbors := False;
    G.CurrentPass := 'target-kit';
    G.PassMode := gpmOverlay;
    Adapter := Kit.ApplyToGraph(G);
    G.SwitchToPass('source-model');
    G.PassMode := gpmOverlay;
    ApplyModelToGraph(M, G);
    G.PassGraph[1].Rules[SOURCE_A].RequireFromPass('target-kit',
      Adapter.VariantGraphKeyAt(0));
    Check(WfcModelDefinitionMatchesGraph(M, G.PassGraph[1]),
      'additive source-pass requirements preserve its model witness');
    Rules := CompleteProjectionRules;
    Raised := False;
    try
      RequireVoxel3DProjectionFromModelPass(Kit, Adapter, M, 1, Rules);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and (Adapter.AppliedGraph.DependencyCount = 0),
      'a reverse dependency cycle is rejected before target mutation');
  finally
    Adapter.Free;
    G.Free;
    Kit.Free;
    M.Free;
  end;
end;

procedure TestSpatialOffsets;
var
  Adapter: TVoxel3DGraphAdapter;
  Clauses: TVoxel3DModelPassSpatialClauses;
  G: TGraph;
  Kit: TVoxel3DKit;
  M: TWfcModel;
  NoneIndex, Facing90Index, X, Y, Z: Integer;
  Report: TGraphSolveReport;
  Scene: TVoxel3DScene;
  Terms: TVoxel3DModelPassSpatialTerms;
begin
  Terms := ModelTermsOf([MakeVoxel3DModelPassSpatialTerm(
    MakeGraphOffset(1, 0, 1), TokensOf([SOURCE_B]))]);
  SetLength(Clauses, 1);
  Clauses[0] := MakeVoxel3DModelPassSpatialClause(
    Selector(TARGET_FACING, ALL_VOXEL3D_ROTATIONS),
    v3pofTargetYaw, Terms);
  Terms[0].AllowedSourceTokens[0] := SOURCE_A;
  Check(Clauses[0].Terms[0].AllowedSourceTokens[0] = SOURCE_B,
    'model spatial construction deep-copies nested source tokens');

  CreateFixture(3, 3, 2, False, G, M, Kit, Adapter);
  Scene := nil;
  try
    Check(FindVariant(Adapter, TARGET_FACING, v3r90, Facing90Index) and
      FindVariant(Adapter, TARGET_NONE, v3r0, NoneIndex),
      'spatial fixture resolves typed target variants');
    for Z := 0 to 1 do
      for Y := 0 to 2 do
        for X := 0 to 2 do
        begin
          G.PassGraph[0].SetAllowedValues(X, Y, Z, SOURCE_A);
          Adapter.AppliedGraph.SetAllowedValues(X, Y, Z,
            Adapter.VariantGraphKeyAt(NoneIndex));
        end;
    G.PassGraph[0].SetAllowedValues(1, 0, 1, SOURCE_B);
    Adapter.AppliedGraph.SetAllowedValues(1, 1, 0,
      Adapter.VariantGraphKeyAt(Facing90Index));
    RequireVoxel3DSpatialClausesFromModelPass(Kit, Adapter, M, 0, Clauses);
    Check(G.TrySolve(SolveOptions, Report),
      'a target-yaw model clause solves with a nonzero Z offset');
    Scene := Kit.CaptureScene(Adapter);
    Check((Scene.VariantAtCell(1, 1, 0).Rotation = v3r90) and
      (G.PassGraph[0].Entry[1, 0, 1].Value = SOURCE_B),
      'yaw rotates XY while preserving the authored Z offset');
  finally
    Scene.Free;
    FreeFixture(G, M, Kit, Adapter);
  end;

  CreateFixture(1, 1, 1, False, G, M, Kit, Adapter);
  try
    FindVariant(Adapter, TARGET_FACING, v3r0, Facing90Index);
    G.PassGraph[0].SetAllowedValues(0, 0, 0, SOURCE_B);
    Adapter.AppliedGraph.SetAllowedValues(0, 0, 0,
      Adapter.VariantGraphKeyAt(Facing90Index));
    Terms := ModelTermsOf([MakeVoxel3DModelPassSpatialTerm(
      MakeGraphOffset(0, 0, 1), TokensOf([SOURCE_B]))]);
    SetLength(Clauses, 1);
    Clauses[0] := MakeVoxel3DModelPassSpatialClause(
      Selector(TARGET_FACING, ALL_VOXEL3D_ROTATIONS), v3pofWorld, Terms);
    RequireVoxel3DSpatialClausesFromModelPass(Kit, Adapter, M, 0, Clauses);
    Check(not G.TrySolve(SolveOptions, Report),
      'an open-boundary spatial term does not match a missing source cell');
  finally
    FreeFixture(G, M, Kit, Adapter);
  end;

  CreateFixture(1, 1, 1, True, G, M, Kit, Adapter);
  try
    FindVariant(Adapter, TARGET_FACING, v3r0, Facing90Index);
    G.PassGraph[0].SetAllowedValues(0, 0, 0, SOURCE_B);
    Adapter.AppliedGraph.SetAllowedValues(0, 0, 0,
      Adapter.VariantGraphKeyAt(Facing90Index));
    RequireVoxel3DSpatialClausesFromModelPass(Kit, Adapter, M, 0, Clauses);
    Check(G.TrySolve(SolveOptions, Report),
      'the same spatial term matches across a wrapped seam');
  finally
    FreeFixture(G, M, Kit, Adapter);
  end;
end;

procedure TestSelectiveNegotiationScope;
var
  Adapter: TVoxel3DGraphAdapter;
  G: TGraph;
  Kit: TVoxel3DKit;
  M: TWfcModel;
  NOptions: TGraphNegotiationOptions;
  NReport: TGraphSelectiveNegotiationReport;
  Report: TGraphSolveReport;
  Rules: TVoxel3DModelPassProjectionRules;
begin
  Adapter := nil;
  G := nil;
  Kit := nil;
  M := nil;
  try
    M := NewSourceModel(3, wmbOpen, 1000000);
    Kit := NewSingleTargetKit;
    G := TGraph.Create;
    G.Reshape(1, 1, 1);
    G.WrapNeighbors := False;
    G.Seed := 0;
    G.CurrentPass := 'source-model';
    G.PassMode := gpmOverlay;
    ApplyModelToGraph(M, G);
    G.PassGraph[0].SetAllowedValues(0, 0, 0, SOURCE_A);
    G.SwitchToPass('target-kit');
    G.PassMode := gpmOverlay;
    Adapter := Kit.ApplyToGraph(G);
    Check(G.TrySolve(SolveOptions, Report) and
      (G.PassGraph[0].Entry[0, 0, 0].Value = SOURCE_A),
      'selective fixture starts from a committed provider assignment');
    G.PassGraph[0].ClearAllowedValues(0, 0, 0);

    SetLength(Rules, 1);
    Rules[0] := MakeVoxel3DModelPassProjectionRule(
      Selector(TARGET_NONE, [v3r0]), TokensOf([SOURCE_B]));
    RequireVoxel3DProjectionFromModelPass(Kit, Adapter, M, 0, Rules);
    NOptions := DefaultGraphNegotiationOptions;
    NOptions.SolveOptions.MaxBacktracks := 64;
    NOptions.MaxPassBacktracks := 2;
    Check(not G.TryRegenerateNegotiatedFrom('target-kit',
      NOptions, NReport),
      'target-only selective negotiation does not reopen its provider');
    Check((Length(NReport.ActivePassIndices) = 1) and
      (NReport.ActivePassIndices[0] = 1) and
      (G.PassGraph[0].Entry[0, 0, 0].Value = SOURCE_A),
      'the failed target-only attempt preserves the provider and scope');

    Check(G.TryRegenerateNegotiatedFrom('source-model',
      NOptions, NReport),
      'provider-root selective negotiation can reopen the public source');
    Check((NReport.Search.PassBacktracks = 1) and
      (G.PassGraph[0].Entry[0, 0, 0].Value = SOURCE_B),
      'one rejected provider assignment is excluded before success');
  finally
    Adapter.Free;
    G.Free;
    Kit.Free;
    M.Free;
  end;
end;

{$IFDEF PAS2JS}
procedure TestMalformedBrowserNumbers;
var
  Adapter: TVoxel3DGraphAdapter;
  Clauses: TVoxel3DModelPassSpatialClauses;
  G: TGraph;
  InitialDependencies: Integer;
  Kit: TVoxel3DKit;
  M: TWfcModel;
  PassIndex: Integer;
  Raised: Boolean;
  Rules: TVoxel3DModelPassProjectionRules;
  Terms: TVoxel3DModelPassSpatialTerms;
begin
  CreateFixture(2, 1, 1, False, G, M, Kit, Adapter);
  try
    InitialDependencies := Adapter.AppliedGraph.DependencyCount;
    Rules := CompleteProjectionRules;
    PassIndex := 0;
    asm PassIndex = 0.5; end;
    Raised := False;
    try
      ValidateVoxel3DProjectionFromModelPass(Kit, Adapter, M,
        PassIndex, Rules);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and
      (Adapter.AppliedGraph.DependencyCount = InitialDependencies),
      'browser source pass indices must be exact integers');

    Terms := ModelTermsOf([MakeVoxel3DModelPassSpatialTerm(
      MakeGraphOffset(0, 0, 0), TokensOf([SOURCE_A]))]);
    SetLength(Clauses, 1);
    Clauses[0] := MakeVoxel3DModelPassSpatialClause(
      Selector(TARGET_FACING, ALL_VOXEL3D_ROTATIONS), v3pofWorld, Terms);
    asm Clauses[0].Terms[0].Offset.DeltaX = 0.5; end;
    Raised := False;
    try
      RequireVoxel3DSpatialClausesFromModelPass(Kit, Adapter, M,
        0, Clauses);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and
      (Adapter.AppliedGraph.DependencyCount = InitialDependencies),
      'browser spatial offsets must use exact integer coordinates');

    Terms := ModelTermsOf([MakeVoxel3DModelPassSpatialTerm(
      MakeGraphOffset(0, 0, 0), TokensOf([SOURCE_A]))]);
    Clauses[0] := MakeVoxel3DModelPassSpatialClause(
      Selector(TARGET_FACING, ALL_VOXEL3D_ROTATIONS), v3pofWorld, Terms);
    asm Clauses[0].Terms[0].Offset.DeltaX = 2147483648; end;
    Raised := False;
    try
      RequireVoxel3DSpatialClausesFromModelPass(Kit, Adapter, M,
        0, Clauses);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and
      (Adapter.AppliedGraph.DependencyCount = InitialDependencies),
      'browser spatial X offsets reject positive Integer overflow');

    Terms := ModelTermsOf([MakeVoxel3DModelPassSpatialTerm(
      MakeGraphOffset(0, 0, 0), TokensOf([SOURCE_A]))]);
    Clauses[0] := MakeVoxel3DModelPassSpatialClause(
      Selector(TARGET_FACING, ALL_VOXEL3D_ROTATIONS), v3pofWorld, Terms);
    asm Clauses[0].Terms[0].Offset.DeltaY = -2147483649; end;
    Raised := False;
    try
      RequireVoxel3DSpatialClausesFromModelPass(Kit, Adapter, M,
        0, Clauses);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and
      (Adapter.AppliedGraph.DependencyCount = InitialDependencies),
      'browser spatial Y offsets reject negative Integer overflow');

    Terms := ModelTermsOf([MakeVoxel3DModelPassSpatialTerm(
      MakeGraphOffset(0, 0, 0), TokensOf([SOURCE_A]))]);
    Clauses[0] := MakeVoxel3DModelPassSpatialClause(
      Selector(TARGET_FACING, ALL_VOXEL3D_ROTATIONS), v3pofWorld, Terms);
    asm Clauses[0].Terms[0].Offset.DeltaZ = Infinity; end;
    Raised := False;
    try
      RequireVoxel3DSpatialClausesFromModelPass(Kit, Adapter, M,
        0, Clauses);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and
      (Adapter.AppliedGraph.DependencyCount = InitialDependencies),
      'browser spatial Z offsets reject infinity');

    Terms := ModelTermsOf([MakeVoxel3DModelPassSpatialTerm(
      MakeGraphOffset(0, 0, 0), TokensOf([SOURCE_A]))]);
    Clauses[0] := MakeVoxel3DModelPassSpatialClause(
      Selector(TARGET_FACING, ALL_VOXEL3D_ROTATIONS), v3pofWorld, Terms);
    asm Clauses[0].OffsetFrame = 0.5; end;
    Raised := False;
    try
      RequireVoxel3DSpatialClausesFromModelPass(Kit, Adapter, M,
        0, Clauses);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and
      (Adapter.AppliedGraph.DependencyCount = InitialDependencies),
      'browser spatial frame values must be exact enum ordinals');
  finally
    FreeFixture(G, M, Kit, Adapter);
  end;
end;
{$ENDIF}

begin
  WriteLn('WFC voxel/model pass bridge conformance suite');
  WriteLn('==============================================');
  Check(WFC_VOXEL3D_PASS_BRIDGE_VERSION = 1,
    'the existing kit-pass bridge version remains unchanged');
  Check(WFC_VOXEL3D_MODEL_PASS_BRIDGE_VERSION = 1,
    'the model-pass bridge starts at version one');
  RunTest('model definition witness', @TestModelDefinitionWitness);
  RunTest('projection and guards', @TestProjectionAndGuards);
  RunTest('cycle guard', @TestCycleGuard);
  RunTest('spatial offsets', @TestSpatialOffsets);
  RunTest('selective negotiation scope', @TestSelectiveNegotiationScope);
  {$IFDEF PAS2JS}
  RunTest('browser numeric injection guards', @TestMalformedBrowserNumbers);
  {$ENDIF}
  WriteLn('==============================================');
  WriteLn('Checks: ', GCheckCount, '  Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d voxel/model pass bridge checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
