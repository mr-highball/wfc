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
program wfc_building3d_test;

{$mode delphi}{$H+}
{$R+}
{$Q+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_voxel3d,
  wfc_voxel3d_passes,
  wfc_building3d,
  wfc_building3d_validate;

const
  TEST_WIDTH = 7;
  TEST_HEIGHT = 5;
  TEST_DEPTH = 3;
  TEST_CELL_COUNT = TEST_WIDTH * TEST_HEIGHT * TEST_DEPTH;
  TEST_SEED = TGraphSeed($13579BDF);
  EXPECTED_PIPELINE_SIGNATURE = '1:F1EF0EB6';
  EXPECTED_STRUCTURE_SCENE_SIGNATURE = '15787695';
  EXPECTED_ENVELOPE_SCENE_SIGNATURE = '99DE5A7F';
  EXPECTED_PROPS_SCENE_SIGNATURE = '9732AB01';

  BRIDGE_SOURCE_A = 'source-a';
  BRIDGE_SOURCE_B = 'source-b';
  BRIDGE_TARGET_FACING = 'target-facing';
  BRIDGE_TARGET_NONE = 'target-none';

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

function TestOptions: TGraphSolveOptions;
begin
  Result := DefaultGraphSolveOptions;
  Result.MaxBacktracks := 8192;
end;

function AllSockets(const AToken: String): TVoxel3DSockets;
begin
  Result := MakeVoxel3DSockets(AToken, AToken, AToken, AToken,
    AToken, AToken);
end;

function SelectorsOf(
  const ASelectors: array of TVoxel3DPassVariantSelector):
  TVoxel3DPassVariantSelectors;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ASelectors));
  for I := 0 to High(ASelectors) do
    Result[I] := ASelectors[I];
end;

function TermsOf(const ATerms: array of TVoxel3DPassSpatialTerm):
  TVoxel3DPassSpatialTerms;
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

function ExpectedShowcaseRole(const AX, AY, AZ: Integer):
  TBuilding3DFootprintRole;
begin
  if (AX < 1) or (AX > 5) then
    Exit(b3frVoid);
  case AZ of
    0:
      begin
        if (AX = 3) and (AY = 0) then
          Exit(b3frEntranceSouth);
        if (AX = 1) or (AX = 5) or (AY = 0) or (AY = 4) then
          Exit(b3frGroundShell);
        if (AX = 3) and (AY = 2) then
          Exit(b3frFeature);
        Result := b3frInterior;
      end;
    1:
      begin
        if (AX = 3) and (AY = 0) then
          Exit(b3frLintel);
        if (AX = 1) or (AX = 5) or (AY = 0) or (AY = 4) then
          Exit(b3frShell);
        Result := b3frInterior;
      end;
    2:
      Result := b3frRoof;
  else
    Result := b3frVoid;
  end;
end;

function NewShowcaseBlueprint: TBuilding3DBlueprint;
var
  X, Y, Z: Integer;
begin
  Result := TBuilding3DBlueprint.Create(TEST_WIDTH, TEST_HEIGHT, TEST_DEPTH);
  try
    for Z := 0 to TEST_DEPTH - 1 do
      for Y := 0 to TEST_HEIGHT - 1 do
        for X := 0 to TEST_WIDTH - 1 do
          Result.SetRole(X, Y, Z, ExpectedShowcaseRole(X, Y, Z));
  except
    Result.Free;
    raise;
  end;
end;

function NewShowcase(const ASeed: TGraphSeed): TBuilding3D;
var
  B: TBuilding3DBlueprint;
  C: TBuilding3DConfig;
begin
  B := NewShowcaseBlueprint;
  try
    C := DefaultBuilding3DConfig;
    C.Seed := ASeed;
    Result := TBuilding3D.Create(B, C);
  finally
    B.Free;
  end;
end;

function StageSnapshot(const ABuilding: TBuilding3D;
  const AStage: TBuilding3DStage): String;
var
  Token: String;
  X, Y, Z: Integer;
begin
  Result := '';
  for Z := 0 to Integer(ABuilding.Depth) - 1 do
    for Y := 0 to Integer(ABuilding.Height) - 1 do
      for X := 0 to Integer(ABuilding.Width) - 1 do
      begin
        Token := ABuilding.StageTokenAt(AStage, X, Y, Z);
        Result := Result + IntToStr(Length(Token)) + ':' + Token;
        if AStage <> b3sFootprint then
          Result := Result + '@' + IntToStr(Voxel3DRotationDegrees(
            ABuilding.StageRotationAt(AStage, X, Y, Z)));
        Result := Result + ';';
      end;
end;

function PublicSnapshot(const ABuilding: TBuilding3D): String;
var
  S: TBuilding3DStage;
begin
  Result := '';
  for S := Low(TBuilding3DStage) to High(TBuilding3DStage) do
    Result := Result + IntToStr(Ord(S)) + '[' +
      StageSnapshot(ABuilding, S) + ']';
end;

function RawSnapshot(const ABuilding: TBuilding3D): String;
var
  E: TGraphEntry;
  S: TBuilding3DStage;
  X, Y, Z: Integer;
begin
  Result := '';
  for S := Low(TBuilding3DStage) to High(TBuilding3DStage) do
  begin
    Result := Result + IntToStr(Ord(S)) + ':';
    for Z := 0 to Integer(ABuilding.Depth) - 1 do
      for Y := 0 to Integer(ABuilding.Height) - 1 do
        for X := 0 to Integer(ABuilding.Width) - 1 do
        begin
          E := ABuilding.StageGraph(S).Entry[X, Y, Z];
          if E.Empty then Result := Result + 'E'
          else if E.Generated then Result := Result + 'G'
          else Result := Result + 'L';
          Result := Result + IntToStr(Length(E.Value)) + ':' + E.Value + ';';
        end;
    Result := Result + '|';
  end;
end;

function FindVariant(const AKit: TVoxel3DKit;
  const APrototypeId: String; const ARotation: TVoxel3DRotation;
  out AIndex: Integer): Boolean;
var
  I: Integer;
  V: TVoxel3DVariant;
begin
  if Assigned(AKit) then
    for I := 0 to AKit.VariantCount - 1 do
    begin
      V := AKit.VariantAt(I);
      if (V.PrototypeId = APrototypeId) and
          (V.Rotation = ARotation) then
      begin
        AIndex := I;
        Exit(True);
      end;
    end;
  AIndex := -1;
  Result := False;
end;

function OffsetEquals(const AOffset: TGraphOffset;
  const AX, AY, AZ: Integer): Boolean;
begin
  Result := (AOffset.DeltaX = AX) and (AOffset.DeltaY = AY) and
    (AOffset.DeltaZ = AZ);
end;

function NewBridgeSourceKit(const AId: String): TVoxel3DKit;
var
  P: TVoxel3DPrototypes;
  Pairs: TVoxel3DSocketPairs;
begin
  SetLength(P, 2);
  P[0] := MakeVoxel3DPrototype(BRIDGE_SOURCE_A, 'source', 1,
    AllSockets('open'), [v3r0], [v3pfEmpty], []);
  P[1] := MakeVoxel3DPrototype(BRIDGE_SOURCE_B, 'source', 1,
    AllSockets('open'), [v3r0], [v3pfEmpty], []);
  SetLength(Pairs, 1);
  Pairs[0] := MakeVoxel3DSocketPair('open', 'open');
  Result := TVoxel3DKit.Create(AId, P, Pairs);
end;

function NewBridgeTargetKit: TVoxel3DKit;
var
  P: TVoxel3DPrototypes;
  Pairs: TVoxel3DSocketPairs;
begin
  SetLength(P, 2);
  P[0] := MakeVoxel3DPrototype(BRIDGE_TARGET_FACING, 'target', 1,
    AllSockets('open'), ALL_VOXEL3D_ROTATIONS, [v3pfEmpty], []);
  P[1] := MakeVoxel3DPrototype(BRIDGE_TARGET_NONE, 'none', 1,
    AllSockets('open'), [v3r0], [v3pfEmpty], []);
  SetLength(Pairs, 1);
  Pairs[0] := MakeVoxel3DSocketPair('open', 'open');
  Result := TVoxel3DKit.Create('bridge-target-kit', P, Pairs);
end;

procedure CreateBridgeFixture(out AGraph: TGraph;
  out ASourceKit, ATargetKit: TVoxel3DKit;
  out ASourceAdapter, ATargetAdapter: TVoxel3DGraphAdapter);
begin
  AGraph := nil;
  ASourceKit := nil;
  ATargetKit := nil;
  ASourceAdapter := nil;
  ATargetAdapter := nil;
  try
    ASourceKit := NewBridgeSourceKit('bridge-source-kit');
    ATargetKit := NewBridgeTargetKit;
    AGraph := TGraph.Create;
    AGraph.Reshape(3, 3, 1);
    AGraph.WrapNeighbors := False;
    AGraph.CurrentPass := 'source';
    AGraph.PassMode := gpmOverlay;
    ASourceAdapter := ASourceKit.ApplyToGraph(AGraph);
    AGraph.SwitchToPass('target');
    AGraph.PassMode := gpmOverlay;
    ATargetAdapter := ATargetKit.ApplyToGraph(AGraph);
  except
    ATargetAdapter.Free;
    ASourceAdapter.Free;
    AGraph.Free;
    ATargetKit.Free;
    ASourceKit.Free;
    raise;
  end;
end;

procedure FreeBridgeFixture(var AGraph: TGraph;
  var ASourceKit, ATargetKit: TVoxel3DKit;
  var ASourceAdapter, ATargetAdapter: TVoxel3DGraphAdapter);
begin
  ATargetAdapter.Free;
  ASourceAdapter.Free;
  AGraph.Free;
  ATargetKit.Free;
  ASourceKit.Free;
  ATargetAdapter := nil;
  ASourceAdapter := nil;
  AGraph := nil;
  ATargetKit := nil;
  ASourceKit := nil;
end;

function CompleteProjectionRules: TVoxel3DPassProjectionRules;
begin
  Result := nil;
  SetLength(Result, 2);
  Result[0] := MakeVoxel3DPassProjectionRule(
    Selector(BRIDGE_TARGET_FACING, ALL_VOXEL3D_ROTATIONS),
    SelectorsOf([Selector(BRIDGE_SOURCE_A, [v3r0])]));
  Result[1] := MakeVoxel3DPassProjectionRule(
    Selector(BRIDGE_TARGET_NONE, [v3r0]),
    SelectorsOf([Selector(BRIDGE_SOURCE_B, [v3r0])]));
end;

function ReverseProjectionRules: TVoxel3DPassProjectionRules;
begin
  Result := nil;
  SetLength(Result, 2);
  Result[0] := MakeVoxel3DPassProjectionRule(
    Selector(BRIDGE_SOURCE_A, [v3r0]),
    SelectorsOf([Selector(BRIDGE_TARGET_FACING,
      ALL_VOXEL3D_ROTATIONS)]));
  Result[1] := MakeVoxel3DPassProjectionRule(
    Selector(BRIDGE_SOURCE_B, [v3r0]),
    SelectorsOf([Selector(BRIDGE_TARGET_NONE, [v3r0])]));
end;

function IsExecution(const AReport: TGraphSolveReport;
  const AExpected: array of Integer): Boolean;
var
  I, J: Integer;
  ExpectedExecution: Boolean;
begin
  if (Length(AReport.ExecutionOrder) <> Length(AExpected)) or
      (Length(AReport.Passes) <> 4) then
    Exit(False);
  for I := 0 to High(AExpected) do
    if (AReport.ExecutionOrder[I] <> AExpected[I]) or
        (not AReport.Passes[AExpected[I]].Executed) or
        (AReport.Passes[AExpected[I]].ExecutionOrdinal <> I) or
        (AReport.Passes[AExpected[I]].Disposition <> gpdSolved) then
      Exit(False);
  for I := 0 to High(AReport.Passes) do
  begin
    ExpectedExecution := False;
    for J := 0 to High(AExpected) do
      if AExpected[J] = I then
        ExpectedExecution := True;
    if (not ExpectedExecution) and
        (AReport.Passes[I].Executed or
        (AReport.Passes[I].Disposition <> gpdReused)) then
      Exit(False);
  end;
  Result := True;
end;

procedure TestVersionsTokensAndBlueprint;
var
  B, Wrong: TBuilding3DBlueprint;
  Building: TBuilding3D;
  C: TBuilding3DConfig;
  EKind: TBuilding3DEnvelopeKind;
  PKind: TBuilding3DPropKind;
  ParsedEKind: TBuilding3DEnvelopeKind;
  ParsedPKind: TBuilding3DPropKind;
  ParsedRole: TBuilding3DFootprintRole;
  ParsedSKind: TBuilding3DStructureKind;
  Raised: Boolean;
  Role: TBuilding3DFootprintRole;
  Roles: TBuilding3DFootprintRoles;
  S: TBuilding3DStage;
  SKind: TBuilding3DStructureKind;
  VReport: TBuilding3DValidationReport;
begin
  Check((WFC_BUILDING3D_MODEL_VERSION = 1) and
    (WFC_BUILDING3D_BLUEPRINT_VERSION = 1) and
    (WFC_BUILDING3D_SIGNATURE_VERSION = 1) and
    (WFC_BUILDING3D_VALIDATION_VERSION = 1) and
    (WFC_VOXEL3D_PASS_BRIDGE_VERSION = 1),
    'building, blueprint, validation, signature, and bridge versions are one');
  Check((Building3DStageName(b3sFootprint) = WFC_BUILDING3D_PASS_FOOTPRINT)
    and (Building3DStageName(b3sStructure) = WFC_BUILDING3D_PASS_STRUCTURE)
    and (Building3DStageName(b3sEnvelopeRoof) = WFC_BUILDING3D_PASS_ENVELOPE)
    and (Building3DStageName(b3sProps) = WFC_BUILDING3D_PASS_PROPS),
    'the four stage labels are stable public tokens');
  for Role := Low(TBuilding3DFootprintRole) to
      High(TBuilding3DFootprintRole) do
    Check(TryBuilding3DFootprintRole(Building3DFootprintRoleToken(Role),
      ParsedRole) and (ParsedRole = Role),
      'footprint role token round-trips at ordinal ' +
      IntToStr(Ord(Role)));
  for SKind := Low(TBuilding3DStructureKind) to
      High(TBuilding3DStructureKind) do
    Check(TryBuilding3DStructureKind(Building3DStructureKindToken(SKind),
      ParsedSKind) and (ParsedSKind = SKind),
      'structure token round-trips at ordinal ' +
      IntToStr(Ord(SKind)));
  for EKind := Low(TBuilding3DEnvelopeKind) to
      High(TBuilding3DEnvelopeKind) do
    Check(TryBuilding3DEnvelopeKind(Building3DEnvelopeKindToken(EKind),
      ParsedEKind) and (ParsedEKind = EKind),
      'envelope token round-trips at ordinal ' +
      IntToStr(Ord(EKind)));
  for PKind := Low(TBuilding3DPropKind) to
      High(TBuilding3DPropKind) do
    Check(TryBuilding3DPropKind(Building3DPropKindToken(PKind),
      ParsedPKind) and (ParsedPKind = PKind),
      'prop token round-trips at ordinal ' + IntToStr(Ord(PKind)));
  Check((not TryBuilding3DFootprintRole('unknown-role', ParsedRole)) and
    (not TryBuilding3DStructureKind('unknown-kind', ParsedSKind)) and
    (not TryBuilding3DEnvelopeKind('unknown-envelope', ParsedEKind)) and
    (not TryBuilding3DPropKind('unknown-prop', ParsedPKind)),
    'unknown public tokens are rejected');
  Check(Building3DSignatureHex(Cardinal($12AB34CD)) = '12AB34CD',
    'building signatures use stable uppercase eight-digit hex');

  C := DefaultBuilding3DConfig;
  Check((C.Seed = 0) and (not C.WrapNeighbors),
    'the default building configuration is deterministic and bounded');
  B := TBuilding3DBlueprint.Create(2, 2, 2);
  try
    Check((B.Width = 2) and (B.Height = 2) and (B.Depth = 2) and
      (B.CellCount = 8) and (B[1, 1, 1] = b3frVoid),
      'a blueprint has checked dimensions and defaults to void');
    B.Fill(b3frInterior).SetRole(1, 1, 1, b3frFeature);
    Roles := B.CopyRoles;
    Roles[0] := b3frVoid;
    Check((B[0, 0, 0] = b3frInterior) and
      (B[1, 1, 1] = b3frFeature),
      'blueprint role copies and fluent mutations have independent ownership');
    Raised := False;
    try
      Role := B[2, 0, 0];
    except
      on E: ERangeError do Raised := True;
    end;
    Check(Raised, 'blueprint access rejects an out-of-range coordinate');
  finally
    B.Free;
  end;

  Raised := False;
  B := nil;
  try
    try
      B := TBuilding3DBlueprint.Create(0, 1, 1);
    except
      on E: ERangeError do Raised := True;
    end;
  finally
    B.Free;
  end;
  Check(Raised, 'blueprints reject zero dimensions');

  Raised := False;
  Building := nil;
  try
    try
      Building := TBuilding3D.Create(TBuilding3DBlueprint(nil));
    except
      on E: EArgumentNilException do Raised := True;
    end;
  finally
    Building.Free;
  end;
  Check(Raised, 'building construction rejects a nil blueprint');

  Building := TBuilding3D.Create(2, 2, 2);
  Wrong := TBuilding3DBlueprint.Create(2, 2, 1);
  try
    Check((not Building.HasSolution) and
      (not ValidateBuilding3D(Building, VReport)) and
      (VReport.Issue.Kind = b3vikUnsolved),
      'unsolved building validation returns a typed issue');
    Check(DescribeBuilding3DValidationIssue(VReport.Issue) =
      'unsolved: expected solved, found unsolved',
      'unsolved validation descriptions are stable');
    Raised := False;
    try
      S := b3sFootprint;
      Building.StageTokenAt(S, 0, 0, 0);
    except
      on E: EBuilding3DScene do Raised := True;
    end;
    Check(Raised, 'typed stage output is unavailable before generation');
    Raised := False;
    try
      Building.ApplyBlueprint(Wrong);
    except
      on E: EBuilding3DBlueprint do Raised := True;
    end;
    Check(Raised, 'blueprints cannot be applied to a different shape');
  finally
    Wrong.Free;
    Building.Free;
  end;
end;

procedure TestPassBridgeGuardsAndRotation;
var
  Clauses: TVoxel3DPassSpatialClauses;
  FakeAdapter: TVoxel3DGraphAdapter;
  FakeGraph: TGraph;
  G: TGraph;
  InitialDependencyCount, SourceAIndex, SourceBIndex, TargetFacing90Index,
    TargetNoneIndex, X, Y: Integer;
  Offset: TGraphOffset;
  Options: TGraphSolveOptions;
  Raised: Boolean;
  Report: TGraphSolveReport;
  Rules, ReverseRules: TVoxel3DPassProjectionRules;
  SourceAdapter, TargetAdapter: TVoxel3DGraphAdapter;
  Sources: TVoxel3DPassVariantSelectors;
  SourceKit, TargetKit, WrongKit: TVoxel3DKit;
  Terms: TVoxel3DPassSpatialTerms;
begin
  Offset := MakeGraphOffset(1, 2, 3);
  Check(OffsetEquals(RotateVoxel3DPassOffset(Offset, v3r0), 1, 2, 3),
    'target-yaw offset leaves yaw zero unchanged');
  Check(OffsetEquals(RotateVoxel3DPassOffset(Offset, v3r90), 2, -1, 3),
    'target-yaw offset rotates clockwise through 90 degrees');
  Check(OffsetEquals(RotateVoxel3DPassOffset(Offset, v3r180), -1, -2, 3),
    'target-yaw offset rotates through 180 degrees');
  Check(OffsetEquals(RotateVoxel3DPassOffset(Offset, v3r270), -2, 1, 3),
    'target-yaw offset rotates through 270 degrees');
  Raised := False;
  try
    RotateVoxel3DPassOffset(MakeGraphOffset(Low(Integer), 0, 0), v3r90);
  except
    on E: EVoxel3DPassMap do Raised := True;
  end;
  Check(Raised, 'target-yaw offset rejects signed negation overflow');

  Sources := SelectorsOf([Selector(BRIDGE_SOURCE_A, [v3r0])]);
  Rules := nil;
  SetLength(Rules, 1);
  Rules[0] := MakeVoxel3DPassProjectionRule(
    Selector(BRIDGE_TARGET_NONE, [v3r0]), Sources);
  Sources[0].PrototypeId := BRIDGE_SOURCE_B;
  Check(Rules[0].AllowedSources[0].PrototypeId = BRIDGE_SOURCE_A,
    'projection rule construction deep-copies source selectors');
  Terms := TermsOf([MakeVoxel3DPassSpatialTerm(MakeGraphOffset(1, 0, 0),
    SelectorsOf([Selector(BRIDGE_SOURCE_A, [v3r0])]))]);
  SetLength(Clauses, 1);
  Clauses[0] := MakeVoxel3DPassSpatialClause(
    Selector(BRIDGE_TARGET_FACING, ALL_VOXEL3D_ROTATIONS),
    v3pofTargetYaw, Terms);
  Terms[0].AllowedSources[0].PrototypeId := BRIDGE_SOURCE_B;
  Check(Clauses[0].Terms[0].AllowedSources[0].PrototypeId = BRIDGE_SOURCE_A,
    'spatial clause construction deep-copies nested source selectors');

  CreateBridgeFixture(G, SourceKit, TargetKit, SourceAdapter, TargetAdapter);
  WrongKit := nil;
  FakeAdapter := nil;
  FakeGraph := nil;
  try
    InitialDependencyCount := TargetAdapter.AppliedGraph.DependencyCount;
    SetLength(Rules, 1);
    Rules[0] := MakeVoxel3DPassProjectionRule(
      Selector(BRIDGE_TARGET_FACING, ALL_VOXEL3D_ROTATIONS),
      SelectorsOf([Selector(BRIDGE_SOURCE_A, [v3r0])]));
    Raised := False;
    try
      RequireVoxel3DProjectionFromPass(TargetKit, TargetAdapter,
        SourceKit, SourceAdapter, Rules);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and (TargetAdapter.AppliedGraph.DependencyCount =
      InitialDependencyCount),
      'an incomplete projection map is rejected before graph mutation');

    Rules := CompleteProjectionRules;
    Rules[0].AllowedSources := SelectorsOf([
      Selector(BRIDGE_SOURCE_A, [v3r0]),
      Selector(BRIDGE_SOURCE_A, [v3r0])]);
    Raised := False;
    try
      RequireVoxel3DProjectionFromPass(TargetKit, TargetAdapter,
        SourceKit, SourceAdapter, Rules);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and (TargetAdapter.AppliedGraph.DependencyCount =
      InitialDependencyCount),
      'duplicate source selection is rejected atomically');

    Rules := CompleteProjectionRules;
    Rules[1].Target := Selector(BRIDGE_TARGET_FACING, [v3r0]);
    Raised := False;
    try
      ValidateVoxel3DProjectionFromPass(TargetKit, TargetAdapter,
        SourceKit, SourceAdapter, Rules);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and (TargetAdapter.AppliedGraph.DependencyCount =
      InitialDependencyCount),
      'overlapping target selectors are rejected without mutation');

    Rules := CompleteProjectionRules;
    Rules[1].Target := Selector(BRIDGE_TARGET_NONE, []);
    Raised := False;
    try
      ValidateVoxel3DProjectionFromPass(TargetKit, TargetAdapter,
        SourceKit, SourceAdapter, Rules);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised, 'empty selector rotations are rejected');

    WrongKit := NewBridgeSourceKit('wrong-bridge-kit');
    Rules := CompleteProjectionRules;
    Raised := False;
    try
      ValidateVoxel3DProjectionFromPass(TargetKit, TargetAdapter,
        WrongKit, SourceAdapter, Rules);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised, 'bridge validation rejects a kit/adapter identity mismatch');

    Raised := False;
    try
      ValidateVoxel3DProjectionFromPass(TargetKit, TargetAdapter,
        SourceKit, nil, Rules);
    except
      on E: EArgumentNilException do Raised := True;
    end;
    Check(Raised, 'bridge validation rejects a nil adapter');

    FakeGraph := TGraph.Create;
    FakeGraph.Reshape(3, 3, 1);
    FakeGraph.WrapNeighbors := False;
    FakeGraph.CurrentPass := 'unapplied-source';
    FakeGraph.PassMode := gpmOverlay;
    FakeAdapter := TVoxel3DGraphAdapter.Create(SourceKit, FakeGraph);
    Raised := False;
    try
      ValidateVoxel3DProjectionFromPass(TargetKit, TargetAdapter,
        SourceKit, FakeAdapter, Rules);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised,
      'a shaped but unapplied adapter identity fails bridge preflight');

    SetLength(Clauses, 1);
    SetLength(Terms, 0);
    Clauses[0] := MakeVoxel3DPassSpatialClause(
      Selector(BRIDGE_TARGET_FACING, ALL_VOXEL3D_ROTATIONS),
      v3pofWorld, Terms);
    Raised := False;
    try
      RequireVoxel3DSpatialClausesFromPass(TargetKit, TargetAdapter,
        SourceKit, SourceAdapter, Clauses);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised and (TargetAdapter.AppliedGraph.DependencyCount =
      InitialDependencyCount),
      'an empty spatial clause is rejected before dependency mutation');

    ValidateVoxel3DProjectionFromPass(TargetKit, TargetAdapter,
      SourceKit, SourceAdapter, Rules);
    Check(TargetAdapter.AppliedGraph.DependencyCount = InitialDependencyCount,
      'projection preflight preserves the existing dependency topology');
    RequireVoxel3DProjectionFromPass(TargetKit, TargetAdapter,
      SourceKit, SourceAdapter, Rules);
    Check((TargetAdapter.AppliedGraph.DependencyCount = 1) and
      (TargetAdapter.AppliedGraph.DependencyIndex[0] = 0),
      'a complete projection installs one canonical source dependency');

    ReverseRules := ReverseProjectionRules;
    Raised := False;
    try
      ValidateVoxel3DProjectionFromPass(SourceKit, SourceAdapter,
        TargetKit, TargetAdapter, ReverseRules);
    except
      on E: EVoxel3DPassMap do Raised := True;
    end;
    Check(Raised, 'a reverse projection that would create a cycle is rejected');
  finally
    FakeAdapter.Free;
    FakeGraph.Free;
    WrongKit.Free;
    FreeBridgeFixture(G, SourceKit, TargetKit,
      SourceAdapter, TargetAdapter);
  end;

  CreateBridgeFixture(G, SourceKit, TargetKit, SourceAdapter, TargetAdapter);
  try
    Check(FindVariant(SourceKit, BRIDGE_SOURCE_A, v3r0, SourceAIndex) and
      FindVariant(SourceKit, BRIDGE_SOURCE_B, v3r0, SourceBIndex) and
      FindVariant(TargetKit, BRIDGE_TARGET_FACING, v3r90,
        TargetFacing90Index) and
      FindVariant(TargetKit, BRIDGE_TARGET_NONE, v3r0, TargetNoneIndex),
      'the spatial bridge fixture resolves all typed variants');
    for Y := 0 to 2 do
      for X := 0 to 2 do
      begin
        SourceAdapter.AppliedGraph.SetAllowedValues(X, Y, 0,
          SourceAdapter.VariantGraphKeyAt(SourceAIndex));
        TargetAdapter.AppliedGraph.SetAllowedValues(X, Y, 0,
          TargetAdapter.VariantGraphKeyAt(TargetNoneIndex));
      end;
    SourceAdapter.AppliedGraph.SetAllowedValues(1, 0, 0,
      SourceAdapter.VariantGraphKeyAt(SourceBIndex));
    TargetAdapter.AppliedGraph.SetAllowedValues(1, 1, 0,
      TargetAdapter.VariantGraphKeyAt(TargetFacing90Index));
    SetLength(Terms, 1);
    Terms[0] := MakeVoxel3DPassSpatialTerm(MakeGraphOffset(1, 0, 0),
      SelectorsOf([Selector(BRIDGE_SOURCE_B, [v3r0])]));
    SetLength(Clauses, 1);
    Clauses[0] := MakeVoxel3DPassSpatialClause(
      Selector(BRIDGE_TARGET_FACING, ALL_VOXEL3D_ROTATIONS),
      v3pofTargetYaw, Terms);
    RequireVoxel3DSpatialClausesFromPass(TargetKit, TargetAdapter,
      SourceKit, SourceAdapter, Clauses);
    Options := TestOptions;
    Check(G.TrySolve(Options, Report),
      'a target-yaw spatial clause solves on the expected rotated source');
    Check((TargetAdapter.VariantAt(TargetFacing90Index).Rotation = v3r90) and
      (TargetAdapter.AppliedGraph.Entry[1, 1, 0].Value =
        TargetAdapter.VariantGraphKeyAt(TargetFacing90Index)) and
      (SourceAdapter.AppliedGraph.Entry[1, 0, 0].Value =
        SourceAdapter.VariantGraphKeyAt(SourceBIndex)),
      'local +X at yaw 90 samples world north while preserving Z');
  finally
    FreeBridgeFixture(G, SourceKit, TargetKit,
      SourceAdapter, TargetAdapter);
  end;
end;

procedure TestFourPassShowcaseAndReplay;
var
  AllEnvelopesMatch, AllPropsMatch, AllRolesMatch: Boolean;
  B: TBuilding3DBlueprint;
  Building, Twin: TBuilding3D;
  ECounts: array[TBuilding3DEnvelopeKind] of Integer;
  EKind, ExpectedEnvelope: TBuilding3DEnvelopeKind;
  I, X, Y, Z: Integer;
  Options: TGraphSolveOptions;
  PCounts: array[TBuilding3DPropKind] of Integer;
  PKind: TBuilding3DPropKind;
  Report: TGraphSolveReport;
  Role: TBuilding3DFootprintRole;
  RoleCounts: array[TBuilding3DFootprintRole] of Integer;
  SceneEnvelope, SceneProps, SceneStructure: TVoxel3DScene;
  Signature, Snapshot: String;
  S: TBuilding3DStage;
  SCounts: array[TBuilding3DStructureKind] of Integer;
  SKind: TBuilding3DStructureKind;
  VOptions: TBuilding3DValidationOptions;
  VReport: TBuilding3DValidationReport;
  TwinScene: TVoxel3DScene;
begin
  for Role := Low(TBuilding3DFootprintRole) to
      High(TBuilding3DFootprintRole) do RoleCounts[Role] := 0;
  for SKind := Low(TBuilding3DStructureKind) to
      High(TBuilding3DStructureKind) do SCounts[SKind] := 0;
  for EKind := Low(TBuilding3DEnvelopeKind) to
      High(TBuilding3DEnvelopeKind) do ECounts[EKind] := 0;
  for PKind := Low(TBuilding3DPropKind) to
      High(TBuilding3DPropKind) do PCounts[PKind] := 0;
  AllEnvelopesMatch := True;
  AllPropsMatch := True;
  AllRolesMatch := True;
  SceneEnvelope := nil;
  SceneProps := nil;
  SceneStructure := nil;
  TwinScene := nil;
  Options := TestOptions;
  B := NewShowcaseBlueprint;
  Building := TBuilding3D.Create(B, DefaultBuilding3DConfig);
  Twin := NewShowcase(0);
  try
    B.SetRole(3, 2, 0, b3frVoid);
    Check((Building.Width = TEST_WIDTH) and
      (Building.Height = TEST_HEIGHT) and (Building.Depth = TEST_DEPTH) and
      (not Building.WrapNeighbors) and (Building.Graph.TotalPassCount = 4),
      'the showcase owns a bounded 7x5x3 four-pass graph');
    for S := Low(TBuilding3DStage) to High(TBuilding3DStage) do
    begin
      Check((Building.StageGraph(S).CurrentPassIndex = Ord(S)) and
        (Building.StageGraph(S).CurrentPass = Building3DStageName(S)) and
        (Building.StageGraph(S).PassMode = gpmOverlay),
        Building3DStageName(S) + ' has stable overlay identity');
    end;
    Check(Building.StageGraph(b3sFootprint).DependencyCount = 0,
      'footprint is the DAG root');
    Check((Building.StageGraph(b3sStructure).DependencyCount = 1) and
      (Building.StageGraph(b3sStructure).DependencyIndex[0] = 0),
      'structure depends exactly on footprint');
    Check((Building.StageGraph(b3sEnvelopeRoof).DependencyCount = 1) and
      (Building.StageGraph(b3sEnvelopeRoof).DependencyIndex[0] = 1),
      'envelope-roof depends exactly on structure');
    Check((Building.StageGraph(b3sProps).DependencyCount = 3) and
      (Building.StageGraph(b3sProps).DependencyIndex[0] = 0) and
      (Building.StageGraph(b3sProps).DependencyIndex[1] = 1) and
      (Building.StageGraph(b3sProps).DependencyIndex[2] = 2),
      'props declares the canonical footprint, structure, envelope DAG');

    Check(Building.TryGenerate(Options, Report),
      'the depth-three showcase solves');
    Check((Report.Status = gssSolved) and
      IsExecution(Report, [0, 1, 2, 3]),
      'the initial report executes all four stages in DAG order');
    Check(Building.FootprintRoleAt(3, 2, 0) = b3frFeature,
      'the model deep-copies blueprint domains before caller mutation');

    for Z := 0 to TEST_DEPTH - 1 do
      for Y := 0 to TEST_HEIGHT - 1 do
        for X := 0 to TEST_WIDTH - 1 do
        begin
          Role := Building.FootprintRoleAt(X, Y, Z);
          SKind := Building.StructureKindAt(X, Y, Z);
          EKind := Building.EnvelopeKindAt(X, Y, Z);
          PKind := Building.PropKindAt(X, Y, Z);
          RoleCounts[Role] := RoleCounts[Role] + 1;
          SCounts[SKind] := SCounts[SKind] + 1;
          ECounts[EKind] := ECounts[EKind] + 1;
          PCounts[PKind] := PCounts[PKind] + 1;
          AllRolesMatch := AllRolesMatch and
            (Role = ExpectedShowcaseRole(X, Y, Z));
          case SKind of
            b3skVoidAir, b3skInteriorAir, b3skFeatureAir:
              ExpectedEnvelope := b3ekNone;
            b3skFoundation, b3skWall, b3skLintel:
              ExpectedEnvelope := b3ekFacade;
            b3skWindow: ExpectedEnvelope := b3ekWindowTrim;
            b3skDoor: ExpectedEnvelope := b3ekDoorTrim;
            b3skRoofSpan: ExpectedEnvelope := b3ekRoofFinish;
          end;
          AllEnvelopesMatch := AllEnvelopesMatch and
            (EKind = ExpectedEnvelope);
          if Role = b3frFeature then
            AllPropsMatch := AllPropsMatch and
              (PKind in [b3pkLamp, b3pkPlant])
          else
            AllPropsMatch := AllPropsMatch and (PKind = b3pkNone);
        end;

    Check(AllRolesMatch,
      'all 105 footprint outputs match the caller-owned blueprint');
    Check(AllEnvelopesMatch,
      'all 105 envelope outputs follow their structure kinds');
    Check(AllPropsMatch,
      'all 105 prop outputs are confined to the feature role');

    Check((RoleCounts[b3frVoid] = 30) and
      (RoleCounts[b3frGroundShell] = 15) and
      (RoleCounts[b3frShell] = 15) and
      (RoleCounts[b3frInterior] = 17) and
      (RoleCounts[b3frFeature] = 1) and
      (RoleCounts[b3frLintel] = 1) and
      (RoleCounts[b3frRoof] = 25) and
      (RoleCounts[b3frEntranceSouth] = 1),
      'showcase footprint counts encode void, shell, entrance, rooms, and roof');
    Check((SCounts[b3skVoidAir] = 30) and
      (SCounts[b3skInteriorAir] = 17) and
      (SCounts[b3skFeatureAir] = 1) and
      (SCounts[b3skFoundation] = 15) and
      (SCounts[b3skWall] + SCounts[b3skWindow] = 15) and
      (SCounts[b3skDoor] = 1) and (SCounts[b3skLintel] = 1) and
      (SCounts[b3skRoofSpan] = 25),
      'structure output preserves the complete blueprint partition');
    Check((ECounts[b3ekNone] = 48) and
      (ECounts[b3ekFacade] + ECounts[b3ekWindowTrim] = 31) and
      (ECounts[b3ekDoorTrim] = 1) and
      (ECounts[b3ekRoofFinish] = 25),
      'envelope output maps air, facade/window, door, and roof exactly');
    Check((PCounts[b3pkNone] = TEST_CELL_COUNT - 1) and
      (PCounts[b3pkLamp] + PCounts[b3pkPlant] = 1),
      'props place one object only at the feature cell');
    Check((Building.StructureKindAt(3, 0, 0) = b3skDoor) and
      (Building.StageRotationAt(b3sStructure, 3, 0, 0) = v3r0) and
      (Building.StructureKindAt(3, 0, 1) = b3skLintel),
      'south entrance orientation and upper lintel are explicit');

    VOptions := DefaultBuilding3DValidationOptions;
    VOptions.RequireFeature := True;
    Check(ValidateBuilding3D(Building, VOptions, VReport),
      'the independent building and voxel validators accept the showcase');
    Check((VReport.CheckedCells = TEST_CELL_COUNT) and
      (VReport.FeatureCount = 1) and (VReport.PropCount = 1) and
      (VReport.Structure.CheckedCells = TEST_CELL_COUNT) and
      (VReport.Structure.CheckedRelations = 540) and
      (VReport.Structure.EntranceCount = 1) and
      (VReport.Structure.ReachableCount = 10),
      'validation reports exact depth-three cell, relation, entrance, and reach counts');

    Signature := Building.PipelineSignature;
    Snapshot := PublicSnapshot(Building);
    WriteLn('  [INFO] canonical building signature: ', Signature);
    Check(Signature = EXPECTED_PIPELINE_SIGNATURE,
      'seed-zero pipeline output matches its portable golden signature');
    SceneStructure := Building.CaptureStructureScene;
    SceneEnvelope := Building.CaptureEnvelopeScene;
    SceneProps := Building.CapturePropsScene;
    WriteLn('  [INFO] scene signatures: structure=',
      Voxel3DSignatureHex(SceneStructure.Signature), ' envelope=',
      Voxel3DSignatureHex(SceneEnvelope.Signature), ' props=',
      Voxel3DSignatureHex(SceneProps.Signature));
    Check((Voxel3DSignatureHex(SceneStructure.Signature) =
        EXPECTED_STRUCTURE_SCENE_SIGNATURE) and
      (Voxel3DSignatureHex(SceneEnvelope.Signature) =
        EXPECTED_ENVELOPE_SCENE_SIGNATURE) and
      (Voxel3DSignatureHex(SceneProps.Signature) =
        EXPECTED_PROPS_SCENE_SIGNATURE),
      'all three captured stages match their portable scene goldens');
    Check((SceneStructure.CellCount = TEST_CELL_COUNT) and
      (SceneEnvelope.CellCount = TEST_CELL_COUNT) and
      (SceneProps.CellCount = TEST_CELL_COUNT) and
      (not SceneStructure.WrapNeighbors),
      'all three typed voxel scenes capture the same bounded shape');

    Check(Building.TryGenerate(Options, Report) and
      (Building.PipelineSignature = Signature) and
      (PublicSnapshot(Building) = Snapshot),
      'same-instance full generation replays exactly');
    Check(Twin.TryGenerate(Options, Report) and
      (Twin.PipelineSignature = Signature) and
      (PublicSnapshot(Twin) = Snapshot),
      'an independently constructed building replays exactly');
    TwinScene := Twin.CaptureStructureScene;
    Check(TwinScene.Signature = SceneStructure.Signature,
      'structure scene signatures replay across independent models');
    I := Length(Snapshot);
    Check(I > TEST_CELL_COUNT,
      'the public snapshot contains all typed stage outputs');
  finally
    TwinScene.Free;
    SceneProps.Free;
    SceneEnvelope.Free;
    SceneStructure.Free;
    Twin.Free;
    Building.Free;
    B.Free;
  end;
end;

procedure TestSelectiveRegenerationAndAtomicity;
var
  Baseline, BeforeFailure, Raw: String;
  Building: TBuilding3D;
  FoundationIndex: Integer;
  Options: TGraphSolveOptions;
  Raised: Boolean;
  Report: TGraphSolveReport;
begin
  Options := TestOptions;
  Building := NewShowcase(TEST_SEED);
  try
    Check(Building.TryGenerate(Options, Report),
      'the selective fixture establishes a solved baseline');
    Baseline := Building.PipelineSignature;
    Check(Building.TryRegenerateFrom(b3sEnvelopeRoof, Options, Report) and
      IsExecution(Report, [Ord(b3sEnvelopeRoof), Ord(b3sProps)]) and
      (Building.PipelineSignature = Baseline),
      'envelope regeneration executes only envelope and props');
    Check(Building.TryRegenerateFrom(b3sStructure, Options, Report) and
      IsExecution(Report, [Ord(b3sStructure), Ord(b3sEnvelopeRoof),
        Ord(b3sProps)]) and (Building.PipelineSignature = Baseline),
      'structure regeneration executes its exact transitive closure');
    Check(Building.TryRegenerateFrom(b3sProps, Options, Report) and
      IsExecution(Report, [Ord(b3sProps)]) and
      (Building.PipelineSignature = Baseline),
      'props regeneration reuses all three providers');
    Check(Building.TryRegenerateFrom(b3sFootprint, Options, Report) and
      IsExecution(Report, [0, 1, 2, 3]) and
      (Building.PipelineSignature = Baseline),
      'footprint regeneration executes the complete four-pass DAG');

    Building.SetFootprintRole(3, 2, 0, b3frInterior);
    Check(not Building.HasSolution,
      'a typed footprint edit marks public output dirty');
    Raised := False;
    try
      Building.TryRegenerateFrom(b3sStructure, Options, Report);
    except
      on E: EBuilding3DScene do Raised := True;
    end;
    Check(Raised,
      'a dirty footprint cannot be bypassed by later-stage regeneration');
    Check(Building.TryRegenerateFrom(b3sFootprint, Options, Report) and
      IsExecution(Report, [0, 1, 2, 3]) and Building.HasSolution and
      (Building.FootprintRoleAt(3, 2, 0) = b3frInterior) and
      (Building.PropKindAt(3, 2, 0) = b3pkNone),
      'typed footprint edits regenerate from the DAG root');
    Building.SetFootprintRole(3, 2, 0, b3frFeature);
    Check(Building.TryRegenerateFrom(b3sFootprint, Options, Report) and
      (Building.PipelineSignature = Baseline),
      'restoring the typed role recovers the exact baseline');

    Check(FindVariant(Building.StructureKit,
      WFC_BUILDING3D_STRUCTURE_FOUNDATION, v3r0, FoundationIndex),
      'the contradiction fixture resolves the foundation variant');
    BeforeFailure := Building.PipelineSignature;
    Raw := RawSnapshot(Building);
    Building.StageGraph(b3sStructure).SetAllowedValues(3, 2, 0,
      Building.StructureKit.VariantGraphKeyAt(FoundationIndex));
    Check(not Building.TryRegenerateFrom(b3sStructure, Options, Report),
      'a foundation forced onto a feature role is contradictory');
    Check((Report.FailedPassIndex = Ord(b3sStructure)) and
      (Report.Contradiction.Kind = gckPassDependency) and
      (Report.Contradiction.DependencyPassIndex = Ord(b3sFootprint)) and
      (Length(Report.ExecutionOrder) = 1) and
      (Report.ExecutionOrder[0] = Ord(b3sStructure)) and
      (Report.Passes[Ord(b3sStructure)].Disposition = gpdFailed),
      'the failure report identifies the structure/footprint dependency');
    Check((RawSnapshot(Building) = Raw) and
      (Building.PipelineSignature = BeforeFailure),
      'failed selective generation leaves every committed stage atomic');
    Building.StageGraph(b3sStructure).ClearAllowedValues(3, 2, 0);
    Check(Building.TryRegenerateFrom(b3sStructure, Options, Report) and
      (Building.PipelineSignature = Baseline),
      'clearing the invalid domain recovers the exact baseline');
  finally
    Building.Free;
  end;
end;

procedure TestValidationTamperAndMissingFeature;
var
  B: TBuilding3DBlueprint;
  Building: TBuilding3D;
  FoundationIndex: Integer;
  Key: String;
  OldWeight: TGraphWeight;
  Options: TGraphSolveOptions;
  Raised: Boolean;
  Report: TGraphSolveReport;
  Scene: TVoxel3DScene;
  VOptions: TBuilding3DValidationOptions;
  VReport: TBuilding3DValidationReport;
begin
  Options := TestOptions;
  Building := NewShowcase(TEST_SEED);
  try
    Check(Building.TryGenerate(Options, Report) and
      FindVariant(Building.StructureKit,
        WFC_BUILDING3D_STRUCTURE_FOUNDATION, v3r0, FoundationIndex),
      'the output-tamper fixture establishes a valid model');
    Building.StageGraph(b3sStructure).Entry[3, 2, 0].Value :=
      Building.StructureKit.VariantGraphKeyAt(FoundationIndex);
    Check((not ValidateBuilding3D(Building, VReport)) and
      (VReport.Issue.Kind = b3vikFootprintStructure) and
      (VReport.Issue.Stage = b3sStructure) and
      (VReport.Issue.X = 3) and (VReport.Issue.Y = 2) and
      (VReport.Issue.Z = 0),
      'independent validation detects a valid voxel in the wrong public role');
    Check(Pos('footprint-structure at (3,2,0)',
      DescribeBuilding3DValidationIssue(VReport.Issue)) = 1,
      'cross-stage tamper descriptions name the exact coordinate');
  finally
    Building.Free;
  end;

  Building := NewShowcase(TEST_SEED);
  try
    Check(Building.TryGenerate(Options, Report) and
      FindVariant(Building.StructureKit,
        WFC_BUILDING3D_STRUCTURE_FOUNDATION, v3r0, FoundationIndex),
      'the definition-tamper fixture establishes a valid model');
    Key := Building.StructureKit.VariantGraphKeyAt(FoundationIndex);
    OldWeight := Building.StageGraph(b3sStructure).Rules[Key].Weight;
    Building.StageGraph(b3sStructure).Rules[Key].Weight := OldWeight + 1;
    Raised := False;
    Scene := nil;
    try
      try
        Scene := Building.CaptureStructureScene;
      except
        on E: EVoxel3DScene do Raised := True;
        on E: EBuilding3DScene do Raised := True;
      end;
    finally
      Scene.Free;
    end;
    Check(Raised,
      'scene capture rejects a tampered applied voxel definition');
  finally
    Building.Free;
  end;

  Building := NewShowcase(TEST_SEED);
  try
    Check(Building.TryGenerate(Options, Report),
      'the stage-attribution fixture establishes a valid model');
    Building.StageGraph(b3sFootprint).Entry[0, 0, 0].Value :=
      'unknown-footprint-value';
    Check((not ValidateBuilding3D(Building, VReport)) and
      (VReport.Issue.Kind = b3vikStageValue) and
      (VReport.Issue.Stage = b3sFootprint) and
      (VReport.Issue.X = 0) and (VReport.Issue.Y = 0) and
      (VReport.Issue.Z = 0),
      'unknown public footprint output is attributed to its exact stage');
    Raised := False;
    try
      Building.StageTokenAt(b3sFootprint, 0, 0, 0);
    except
      on E: EBuilding3DScene do Raised := True;
    end;
    Check(Raised,
      'public stage access rejects an unknown footprint output');
    Raised := False;
    try
      Building.PipelineSignature;
    except
      on E: EBuilding3DScene do Raised := True;
    end;
    Check(Raised,
      'portable signatures reject an unknown footprint output');
  finally
    Building.Free;
  end;

  Building := NewShowcase(TEST_SEED);
  try
    Check(Building.TryGenerate(Options, Report) and
      Building.DefinitionMatchesPipeline,
      'the pipeline-tamper fixture starts with the canonical definition');
    Building.Graph.SwitchToPass('unexpected-fifth-pass');
    Building.Graph.PassMode := gpmOverlay;
    Check((not Building.DefinitionMatchesPipeline) and
      (not ValidateBuilding3D(Building, VReport)) and
      (VReport.Issue.Kind = b3vikPipelineDefinition),
      'validation rejects an extra pass outside the canonical four-stage DAG');
    Check(DescribeBuilding3DValidationIssue(VReport.Issue) =
      'pipeline-definition: expected canonical-four-pass-pipeline, found modified-pipeline',
      'pipeline-definition descriptions are stable');
    Raised := False;
    try
      Building.PipelineSignature;
    except
      on E: EBuilding3DScene do Raised := True;
    end;
    Check(Raised, 'signatures reject a modified pipeline definition');
  finally
    Building.Free;
  end;

  B := NewShowcaseBlueprint;
  B.SetRole(3, 2, 0, b3frInterior);
  Building := TBuilding3D.Create(B);
  try
    Check(Building.TryGenerate(Options, Report),
      'the no-feature variant still solves structurally');
    VOptions := DefaultBuilding3DValidationOptions;
    VOptions.RequireFeature := True;
    Check((not ValidateBuilding3D(Building, VOptions, VReport)) and
      (VReport.Issue.Kind = b3vikMissingFeature) and
      (VReport.FeatureCount = 0) and (VReport.PropCount = 0),
      'feature-required validation rejects a building with no feature or prop');
    Check(DescribeBuilding3DValidationIssue(VReport.Issue) =
      'missing-feature: expected at-least-one-feature, found none',
      'missing-feature descriptions are stable');
  finally
    Building.Free;
    B.Free;
  end;
end;

procedure TestSceneLifetime;
var
  Building: TBuilding3D;
  CopyIndices: TVoxel3DVariantIndices;
  EnvelopeId, PropId, StructureId: String;
  EnvelopeScene, PropsScene, StructureScene: TVoxel3DScene;
  EnvelopeSignature, PropsSignature,
    StructureSignature: TVoxel3DSignature;
  OriginalIndex: Integer;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
begin
  Options := TestOptions;
  Building := NewShowcase(TEST_SEED);
  StructureScene := nil;
  EnvelopeScene := nil;
  PropsScene := nil;
  try
    Check(Building.TryGenerate(Options, Report),
      'the lifetime fixture generates all stages');
    StructureScene := Building.CaptureStructureScene;
    EnvelopeScene := Building.CaptureEnvelopeScene;
    PropsScene := Building.CapturePropsScene;
    StructureSignature := StructureScene.Signature;
    EnvelopeSignature := EnvelopeScene.Signature;
    PropsSignature := PropsScene.Signature;
    StructureId := StructureScene.VariantAtCell(3, 0, 0).PrototypeId;
    EnvelopeId := EnvelopeScene.VariantAtCell(3, 0, 2).PrototypeId;
    PropId := PropsScene.VariantAtCell(3, 2, 0).PrototypeId;
    CopyIndices := StructureScene.CopyVariantIndices;
    OriginalIndex := StructureScene.VariantIndexAt(0, 0, 0);
    CopyIndices[0] := (OriginalIndex + 1) mod StructureScene.VariantCount;
    Check(StructureScene.VariantIndexAt(0, 0, 0) = OriginalIndex,
      'scene index copies cannot mutate captured ownership');
  finally
    Building.Free;
  end;

  try
    Check((StructureScene.Width = TEST_WIDTH) and
      (StructureScene.Height = TEST_HEIGHT) and
      (StructureScene.Depth = TEST_DEPTH) and
      (StructureScene.CellCount = TEST_CELL_COUNT) and
      (StructureScene.Signature = StructureSignature) and
      (StructureScene.VariantAtCell(3, 0, 0).PrototypeId = StructureId) and
      (StructureId = WFC_BUILDING3D_STRUCTURE_DOOR),
      'structure scene remains fully usable after its building is freed');
    Check((EnvelopeScene.Signature = EnvelopeSignature) and
      (EnvelopeScene.VariantAtCell(3, 0, 2).PrototypeId = EnvelopeId) and
      (EnvelopeId = WFC_BUILDING3D_ENVELOPE_ROOF),
      'envelope scene independently owns roof output for its lifetime');
    Check((PropsScene.Signature = PropsSignature) and
      (PropsScene.VariantAtCell(3, 2, 0).PrototypeId = PropId) and
      ((PropId = WFC_BUILDING3D_PROP_LAMP) or
       (PropId = WFC_BUILDING3D_PROP_PLANT)),
      'props scene independently owns the generated feature prop');
  finally
    PropsScene.Free;
    EnvelopeScene.Free;
    StructureScene.Free;
  end;
end;

begin
  WriteLn('WFC Building 3D tests');
  RunTest('versions, tokens, and blueprint ownership',
    @TestVersionsTokensAndBlueprint);
  RunTest('voxel pass bridge guards and target-yaw offsets',
    @TestPassBridgeGuardsAndRotation);
  RunTest('four-pass depth-three showcase and replay',
    @TestFourPassShowcaseAndReplay);
  RunTest('selective regeneration and atomic rollback',
    @TestSelectiveRegenerationAndAtomicity);
  RunTest('validation, tamper detection, and feature policy',
    @TestValidationTamperAndMissingFeature);
  RunTest('captured scene lifetime and deep ownership',
    @TestSceneLifetime);
  WriteLn('[SUMMARY] ', GCheckCount, ' checks, ', GFailureCount, ' failures');
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.Create(IntToStr(GFailureCount) +
      ' building 3D test failures');
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
