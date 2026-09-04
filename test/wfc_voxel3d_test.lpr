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
program wfc_voxel3d_test;

{$mode delphi}{$H+}
{$R+}
{$Q+}

uses
  SysUtils,
  wfc,
  wfc_voxel3d,
  wfc_voxel3d_validate,
  wfc_voxel3d_mesh;

type
  TTestProcedure = procedure;

const
  BASIC_SOLID = 0;
  BASIC_AIR = 1;
  BASIC_SUPPORT = 2;
  BASIC_NEEDS_SUPPORT = 3;
  BASIC_ENTRANCE = 4;
  BASIC_PATH = 5;

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;
  GRunningGuardKit: TVoxel3DKit = nil;
  GRunningGuardRaised: Boolean = False;

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

function IndicesOf(const AValues: array of Integer): TVoxel3DVariantIndices;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do
    Result[I] := AValues[I];
end;

function ValuesContain(const AValues: TGraphValues;
  const AValue: TGraphValue): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AValues) do
    if AValues[I] = AValue then
      Exit(True);
  Result := False;
end;

function AllSockets(const AToken: String): TVoxel3DSockets;
begin
  Result := MakeVoxel3DSockets(AToken, AToken, AToken, AToken,
    AToken, AToken);
end;

function NewBasicKit(const AId: String): TVoxel3DKit;
var
  P: TVoxel3DPrototypes;
  Pairs: TVoxel3DSocketPairs;
begin
  SetLength(P, 6);
  P[BASIC_SOLID] := MakeVoxel3DPrototype('solid', 'stone', 3,
    AllSockets('open'), [v3r0], [v3pfSolid, v3pfProvidesSupport], []);
  P[BASIC_AIR] := MakeVoxel3DPrototype('air', 'none', 1,
    AllSockets('open'), [v3r0], [v3pfEmpty], []);
  P[BASIC_SUPPORT] := MakeVoxel3DPrototype('support', 'wood', 2,
    AllSockets('open'), [v3r0], [v3pfSolid, v3pfProvidesSupport], []);
  P[BASIC_NEEDS_SUPPORT] := MakeVoxel3DPrototype('supported', 'brick', 2,
    AllSockets('open'), [v3r0], [v3pfSolid, v3pfRequiresSupport], []);
  P[BASIC_ENTRANCE] := MakeVoxel3DPrototype('entrance', 'floor', 1,
    AllSockets('open'), [v3r0],
    [v3pfEmpty, v3pfWalkable, v3pfEntrance], [gdEast, gdWest]);
  P[BASIC_PATH] := MakeVoxel3DPrototype('path', 'floor', 1,
    AllSockets('open'), [v3r0],
    [v3pfEmpty, v3pfWalkable, v3pfRequiredReachable], [gdEast, gdWest]);
  SetLength(Pairs, 1);
  Pairs[0] := MakeVoxel3DSocketPair('open', 'open');
  Result := TVoxel3DKit.Create(AId, P, Pairs);
end;

function NewRotationKit(const AId: String;
  const AReversePairOrder: Boolean): TVoxel3DKit;
const
  TOKENS: array[0..5] of String = ('n', 'e', 's', 'w', 'u', 'd');
var
  I: Integer;
  P: TVoxel3DPrototypes;
  Pairs: TVoxel3DSocketPairs;
begin
  SetLength(P, 2);
  P[0] := MakeVoxel3DPrototype('turn', 'copper', 7,
    MakeVoxel3DSockets('n', 'e', 's', 'w', 'u', 'd'),
    ALL_VOXEL3D_ROTATIONS, [v3pfEmpty, v3pfWalkable],
    [gdNorth, gdUp]);
  P[1] := MakeVoxel3DPrototype('cap', 'glass', 2,
    AllSockets('n'), [v3r0, v3r180], [v3pfSolid], []);
  SetLength(Pairs, Length(TOKENS));
  for I := 0 to High(TOKENS) do
    if AReversePairOrder then
      Pairs[High(TOKENS) - I] := MakeVoxel3DSocketPair(TOKENS[I], TOKENS[I])
    else
      Pairs[I] := MakeVoxel3DSocketPair(TOKENS[I], TOKENS[I]);
  Result := TVoxel3DKit.Create(AId, P, Pairs);
end;

function NewLookupScaleKit: TVoxel3DKit;
const
  PROTOTYPE_COUNT = 32;
var
  I: Integer;
  P: TVoxel3DPrototypes;
  Pairs: TVoxel3DSocketPairs;
begin
  SetLength(P, PROTOTYPE_COUNT);
  for I := 0 to High(P) do
    P[I] := MakeVoxel3DPrototype('part-' + IntToStr(I), 'test', 1,
      AllSockets('open'), ALL_VOXEL3D_ROTATIONS, [v3pfEmpty], []);
  SetLength(Pairs, 1);
  Pairs[0] := MakeVoxel3DSocketPair('open', 'open');
  Result := TVoxel3DKit.Create('lookup-scale-kit', P, Pairs);
end;

function NewMismatchKit(const AId: String): TVoxel3DKit;
var
  P: TVoxel3DPrototypes;
  Pairs: TVoxel3DSocketPairs;
begin
  SetLength(P, 2);
  P[0] := MakeVoxel3DPrototype('a', 'a-mat', 1, AllSockets('a'),
    [v3r0], [v3pfEmpty], []);
  P[1] := MakeVoxel3DPrototype('b', 'b-mat', 1, AllSockets('b'),
    [v3r0], [v3pfEmpty], []);
  SetLength(Pairs, 2);
  Pairs[0] := MakeVoxel3DSocketPair('a', 'a');
  Pairs[1] := MakeVoxel3DSocketPair('b', 'b');
  Result := TVoxel3DKit.Create(AId, P, Pairs);
end;

function NewDenyKit: TVoxel3DKit;
var
  P: TVoxel3DPrototypes;
  Pairs: TVoxel3DSocketPairs;
begin
  SetLength(P, 1);
  P[0] := MakeVoxel3DPrototype('deny', 'test', 1,
    MakeVoxel3DSockets('blocked', 'open', 'blocked', 'open',
      'open', 'open'), [v3r0], [v3pfEmpty], []);
  SetLength(Pairs, 1);
  Pairs[0] := MakeVoxel3DSocketPair('open', 'open');
  Result := TVoxel3DKit.Create('deny-kit', P, Pairs);
end;

function NewSeamKit: TVoxel3DKit;
var
  P: TVoxel3DPrototypes;
  Pairs: TVoxel3DSocketPairs;
begin
  SetLength(P, 2);
  P[0] := MakeVoxel3DPrototype('left', 'test', 1,
    MakeVoxel3DSockets('open', 'open', 'open', 'wa', 'open', 'open'),
    [v3r0], [v3pfEmpty], []);
  P[1] := MakeVoxel3DPrototype('right', 'test', 1,
    MakeVoxel3DSockets('open', 'eb', 'open', 'open', 'open', 'open'),
    [v3r0], [v3pfEmpty], []);
  SetLength(Pairs, 3);
  Pairs[0] := MakeVoxel3DSocketPair('open', 'open');
  Pairs[1] := MakeVoxel3DSocketPair('wa', 'wa');
  Pairs[2] := MakeVoxel3DSocketPair('eb', 'eb');
  Result := TVoxel3DKit.Create('seam-kit', P, Pairs);
end;

function KitCreationRejected(const AId: String;
  const APrototypes: TVoxel3DPrototypes;
  const APairs: TVoxel3DSocketPairs): Boolean;
var
  K: TVoxel3DKit;
begin
  Result := False;
  K := nil;
  try
    try
      K := TVoxel3DKit.Create(AId, APrototypes, APairs);
    except
      on EVoxel3D do
        Result := True;
      on ERangeError do
        Result := True;
      on EArgumentException do
        Result := True;
    end;
  finally
    K.Free;
  end;
end;

function NewScene(const AKit: TVoxel3DKit;
  const AWidth, AHeight, ADepth: Integer; const AWrap: Boolean;
  const AIndices: TVoxel3DVariantIndices): TVoxel3DScene;
var
  Adapter: TVoxel3DGraphAdapter;
  Graph: TGraph;
begin
  Result := nil;
  Graph := TGraph.Create;
  Adapter := nil;
  try
    Graph.Reshape(AWidth, AHeight, ADepth);
    Graph.WrapNeighbors := AWrap;
    Adapter := AKit.ApplyToGraph(Graph);
    Result := TVoxel3DScene.Create(Adapter, AIndices);
  finally
    Adapter.Free;
    Graph.Free;
  end;
end;

function NewSolvedScene(const AKit: TVoxel3DKit;
  const AWidth, AHeight, ADepth: Integer; const AWrap: Boolean;
  const AIndices: TVoxel3DVariantIndices): TVoxel3DScene;
var
  Adapter: TVoxel3DGraphAdapter;
  Graph: TGraph;
  I, X, Y, Z, Plane: Integer;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
begin
  Result := nil;
  if Length(AIndices) <> AWidth * AHeight * ADepth then
    raise Exception.Create('test fixture cell-count mismatch');
  Graph := TGraph.Create;
  Adapter := nil;
  try
    Graph.Reshape(AWidth, AHeight, ADepth);
    Graph.WrapNeighbors := AWrap;
    Graph.Seed := 123456;
    Adapter := AKit.ApplyToGraph(Graph);
    Plane := AWidth * AHeight;
    for I := 0 to High(AIndices) do
    begin
      Z := I div Plane;
      Y := (I mod Plane) div AWidth;
      X := I mod AWidth;
      Graph.SetAllowedValues(X, Y, Z,
        Adapter.VariantGraphKeyAt(AIndices[I]));
    end;
    Options := DefaultGraphSolveOptions;
    Options.MaxBacktracks := 64;
    if not Graph.TrySolve(Options, Report) then
      raise Exception.Create('test fixture could not be solved');
    Result := AKit.CaptureScene(Adapter);
  finally
    Adapter.Free;
    Graph.Free;
  end;
end;

function RunningGuardSelection(const AGraph: TGraph;
  const AEntry: TGraphEntry; const AValid: TGraphValues): TGraphValue;
var
  Adapter: TVoxel3DGraphAdapter;
begin
  Adapter := nil;
  try
    try
      Adapter := GRunningGuardKit.ApplyToGraph(AGraph);
    except
      on EVoxel3DAdapter do
        GRunningGuardRaised := True;
    end;
  finally
    Adapter.Free;
  end;
  if Length(AValid) = 0 then
    Result := ''
  else
    Result := AValid[0];
end;

procedure TestVersionsAndTokens;
var
  LongToken: String;
begin
  Check(WFC_VOXEL3D_KIT_VERSION = 1, 'kit format version is one');
  Check(WFC_VOXEL3D_ADAPTER_VERSION = 1, 'adapter format version is one');
  Check(WFC_VOXEL3D_SCENE_VERSION = 1, 'scene format version is one');
  Check(WFC_VOXEL3D_SIGNATURE_VERSION = 1,
    'signature format version is one');
  Check(WFC_VOXEL3D_VALIDATION_VERSION = 1,
    'validation format version is one');
  Check(WFC_VOXEL3D_MESH_VERSION = 1, 'mesh format version is one');
  Check(IsVoxel3DToken('A0-z._~'), 'portable punctuation token is accepted');
  Check(not IsVoxel3DToken(''), 'empty token is rejected');
  Check(not IsVoxel3DToken('-starts-wrong'),
    'token must begin with an ASCII alphanumeric');
  Check(not IsVoxel3DToken('has space'), 'space is rejected');
  Check(not IsVoxel3DToken('has:colon'), 'colon is rejected');
  Check(not IsVoxel3DToken(String(Chr(233))), 'non-ASCII token is rejected');
  LongToken := StringOfChar('a', WFC_VOXEL3D_MAX_TOKEN_LENGTH);
  Check(IsVoxel3DToken(LongToken), 'maximum token length is accepted');
  LongToken := LongToken + 'a';
  Check(not IsVoxel3DToken(LongToken), 'overlong token is rejected');
  Check(Length(Voxel3DSignatureHex(Cardinal($0123ABCD))) = 8,
    'signature hex has fixed width');
  Check(Voxel3DSignatureHex(Cardinal($0123ABCD)) = '0123ABCD',
    'signature hex is canonical uppercase');
end;

procedure TestConstructorGuardsAndOwnership;
var
  P: TVoxel3DPrototypes;
  Pairs: TVoxel3DSocketPairs;
  K: TVoxel3DKit;
  CopyP: TVoxel3DPrototype;
  CopyPair: TVoxel3DSocketPair;
begin
  P := nil;
  Pairs := nil;
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects an empty prototype model');

  SetLength(P, 1);
  P[0] := MakeVoxel3DPrototype('p', 'm', 1, AllSockets('x'),
    [v3r0], [v3pfEmpty], []);
  SetLength(Pairs, 1);
  Pairs[0] := MakeVoxel3DSocketPair('x', 'x');
  Check(KitCreationRejected('', P, Pairs), 'kit rejects an empty kit id');

  P[0].Id := '-bad';
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects invalid prototype ids');
  P[0].Id := 'p';
  P[0].Material := 'bad material';
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects invalid material tokens');
  P[0].Material := 'm';
  P[0].Sockets[gdUp] := '';
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects empty socket tokens');
  P[0].Sockets[gdUp] := 'x';
  P[0].Weight := 0;
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects zero prototype weight');
  P[0].Weight := 1;
  P[0].AllowedRotations := [];
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects an empty allowed-rotation set');
  P[0].AllowedRotations := [v3r0];
  P[0].Flags := [v3pfEmpty, v3pfSolid];
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects prototypes that are both empty and solid');
  P[0].Flags := [v3pfEmpty, v3pfRequiresSupport];
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects empty prototypes requiring support');
  P[0].Flags := [v3pfEmpty, v3pfProvidesSupport];
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects empty prototypes providing support');
  P[0].Flags := [v3pfSolid, v3pfWalkable];
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects solid walkable prototypes');
  P[0].Flags := [v3pfEmpty, v3pfEntrance];
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects a non-walkable entrance');
  P[0].Flags := [v3pfEmpty, v3pfRequiredReachable];
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects a non-walkable required-reachable cell');
  P[0].Flags := [v3pfEmpty, v3pfWalkable, v3pfEntrance];
  P[0].WalkOpenings := [];
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects an entrance without a walk opening');
  P[0].Flags := [v3pfEmpty, v3pfWalkable];
  P[0].WalkOpenings := [gdNorth];
  P[0].Flags := [v3pfEmpty];
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects walk openings on a non-walkable prototype');

  P[0] := MakeVoxel3DPrototype('p', 'm', 1, AllSockets('x'),
    [v3r0], [v3pfEmpty], []);
  Pairs[0].First := '';
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects invalid socket-pair tokens');
  Pairs[0] := MakeVoxel3DSocketPair('x', 'x');

  SetLength(P, 2);
  P[1] := P[0];
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects duplicate prototype ids');
  SetLength(P, 1);
  SetLength(Pairs, 2);
  Pairs[0] := MakeVoxel3DSocketPair('x', 'y');
  Pairs[1] := MakeVoxel3DSocketPair('y', 'x');
  Check(KitCreationRejected('kit', P, Pairs),
    'kit rejects reversed duplicate compatibility pairs');

  SetLength(Pairs, 2);
  Pairs[0] := MakeVoxel3DSocketPair('z', 'x');
  Pairs[1] := MakeVoxel3DSocketPair('y', 'x');
  K := TVoxel3DKit.Create('owned-kit', P, Pairs);
  try
    P[0].Id := 'mutated';
    P[0].Sockets[gdNorth] := 'mutated';
    Pairs[0].First := 'mutated';
    Check(K.PrototypeAt(0).Id = 'p',
      'kit deep-copies caller prototype records');
    Check(K.PrototypeAt(0).Sockets[gdNorth] = 'x',
      'kit deep-copies caller socket strings');
    CopyP := K.PrototypeAt(0);
    CopyP.Id := 'changed-copy';
    CopyP.Sockets[gdSouth] := 'changed-copy';
    Check((CopyP.Id = 'changed-copy') and (K.PrototypeAt(0).Id = 'p'),
      'prototype accessor returns an independent record');
    Check((CopyP.Sockets[gdSouth] = 'changed-copy') and
      (K.PrototypeAt(0).Sockets[gdSouth] = 'x'),
      'prototype accessor deep-copies socket strings');
    CopyPair := K.SocketPairAt(0);
    CopyPair.First := 'changed-copy';
    Check((CopyPair.First = 'changed-copy') and
      (K.SocketPairAt(0).First <> 'changed-copy'),
      'socket-pair accessor returns an independent record');
    Check((K.SocketPairAt(0).First = 'x') and
      (K.SocketPairAt(0).Second = 'y'),
      'socket pairs are canonically ordered and sorted');
    Check((K.SocketPairAt(1).First = 'x') and
      (K.SocketPairAt(1).Second = 'z'),
      'canonical pair sort is deterministic');
    Check(K.SocketsCompatible('z', 'x'),
      'socket compatibility is unordered');
    Check(not K.SocketsCompatible('x', 'x'),
      'undeclared socket pair is incompatible');
  finally
    K.Free;
  end;
end;

procedure TestNilAndAccessorGuards;
var
  Adapter, TemporaryAdapter: TVoxel3DGraphAdapter;
  Graph: TGraph;
  K: TVoxel3DKit;
  Mesh, TemporaryMesh: TVoxel3DMesh;
  Rejected: Boolean;
  Report: TVoxel3DValidationReport;
  Scene, TemporaryScene: TVoxel3DScene;
begin
  K := NewBasicKit('nil-guard-kit');
  Graph := TGraph.Create;
  Adapter := nil;
  Scene := nil;
  Mesh := nil;
  TemporaryAdapter := nil;
  TemporaryScene := nil;
  TemporaryMesh := nil;
  try
    Rejected := False;
    try
      TemporaryAdapter := K.ApplyToGraph(nil);
    except
      on EVoxel3DAdapter do Rejected := True;
    end;
    TemporaryAdapter.Free;
    TemporaryAdapter := nil;
    Check(Rejected, 'kit application rejects a nil graph');

    Rejected := False;
    try
      TemporaryAdapter := TVoxel3DGraphAdapter.Create(nil, Graph);
    except
      on EVoxel3DAdapter do Rejected := True;
    end;
    TemporaryAdapter.Free;
    TemporaryAdapter := nil;
    Check(Rejected, 'adapter constructor rejects a nil kit');

    Rejected := False;
    try
      TemporaryAdapter := TVoxel3DGraphAdapter.Create(K, nil);
    except
      on EVoxel3DAdapter do Rejected := True;
    end;
    TemporaryAdapter.Free;
    TemporaryAdapter := nil;
    Check(Rejected, 'adapter constructor rejects a nil graph');

    Rejected := False;
    try
      TemporaryScene := TVoxel3DScene.Create(nil, nil);
    except
      on EVoxel3DScene do Rejected := True;
    end;
    TemporaryScene.Free;
    TemporaryScene := nil;
    Check(Rejected, 'scene constructor rejects a nil adapter');

    Rejected := False;
    try
      TemporaryScene := K.CaptureScene(nil);
    except
      on EVoxel3DScene do Rejected := True;
    end;
    TemporaryScene.Free;
    TemporaryScene := nil;
    Check(Rejected, 'scene capture rejects a nil adapter');

    Graph.Reshape(1, 1, 1);
    Graph.WrapNeighbors := False;
    Adapter := K.ApplyToGraph(Graph);
    Scene := TVoxel3DScene.Create(Adapter, IndicesOf([BASIC_AIR]));
    Mesh := BuildVoxel3DMesh(K, Scene);

    Rejected := False;
    try
      ValidateVoxel3DScene(nil, Scene, Report);
    except
      on EArgumentNilException do Rejected := True;
    end;
    Check(Rejected, 'validator rejects a nil kit');
    Rejected := False;
    try
      ValidateVoxel3DScene(K, nil, Report);
    except
      on EArgumentNilException do Rejected := True;
    end;
    Check(Rejected, 'validator rejects a nil scene');

    Rejected := False;
    try
      TemporaryMesh := BuildVoxel3DMesh(nil, Scene);
    except
      on EVoxel3DMesh do Rejected := True;
    end;
    TemporaryMesh.Free;
    TemporaryMesh := nil;
    Check(Rejected, 'mesh builder rejects a nil kit');
    Rejected := False;
    try
      TemporaryMesh := BuildVoxel3DMesh(K, nil);
    except
      on EVoxel3DMesh do Rejected := True;
    end;
    TemporaryMesh.Free;
    TemporaryMesh := nil;
    Check(Rejected, 'mesh builder rejects a nil scene');
    Check(not K.MatchesScene(nil), 'semantic match rejects a nil scene');

    Rejected := False;
    try
      K.PrototypeAt(-1);
    except
      on ERangeError do Rejected := True;
    end;
    Check(Rejected, 'prototype accessor rejects a negative index');
    Rejected := False;
    try
      K.SocketPairAt(K.SocketPairCount);
    except
      on ERangeError do Rejected := True;
    end;
    Check(Rejected, 'socket-pair accessor rejects its upper bound');
    Rejected := False;
    try
      K.VariantAt(K.VariantCount);
    except
      on ERangeError do Rejected := True;
    end;
    Check(Rejected, 'variant accessor rejects its upper bound');
    Rejected := False;
    try
      Adapter.VariantGraphKeyAt(-1);
    except
      on ERangeError do Rejected := True;
    end;
    Check(Rejected, 'adapter key accessor rejects a negative index');
    Rejected := False;
    try
      Scene.VariantAt(Scene.VariantCount);
    except
      on ERangeError do Rejected := True;
    end;
    Check(Rejected, 'scene variant accessor rejects its upper bound');
    Rejected := False;
    try
      Mesh.QuadAt(Mesh.QuadCount);
    except
      on ERangeError do Rejected := True;
    end;
    Check(Rejected, 'mesh quad accessor rejects its upper bound');
  finally
    TemporaryMesh.Free;
    TemporaryScene.Free;
    TemporaryAdapter.Free;
    Mesh.Free;
    Scene.Free;
    Adapter.Free;
    Graph.Free;
    K.Free;
  end;
end;

procedure TestRotationsAndVariants;
var
  D: TGraphDirection;
  R: TVoxel3DRotation;
  S, Rotated: TVoxel3DSockets;
  V: TVoxel3DVariant;
  K: TVoxel3DKit;
begin
  Check(Voxel3DRotationDegrees(v3r0) = 0, 'zero rotation is zero degrees');
  Check(Voxel3DRotationDegrees(v3r90) = 90, 'quarter rotation is 90 degrees');
  Check(Voxel3DRotationDegrees(v3r180) = 180, 'half rotation is 180 degrees');
  Check(Voxel3DRotationDegrees(v3r270) = 270,
    'three-quarter rotation is 270 degrees');
  Check(RotateVoxel3DDirection(gdNorth, v3r90) = gdEast,
    'clockwise yaw rotates north to east');
  Check(RotateVoxel3DDirection(gdEast, v3r90) = gdSouth,
    'clockwise yaw rotates east to south');
  Check(RotateVoxel3DDirection(gdUp, v3r270) = gdUp,
    'yaw leaves up invariant');
  Check(RotateVoxel3DDirection(gdDown, v3r90) = gdDown,
    'yaw leaves down invariant');
  for D := Low(TGraphDirection) to High(TGraphDirection) do
  begin
    Check(RotateVoxel3DDirection(
      RotateVoxel3DDirection(
        RotateVoxel3DDirection(
          RotateVoxel3DDirection(D, v3r90), v3r90), v3r90), v3r90) = D,
      'four quarter-turns restore direction ' + IntToStr(Ord(D)));
    Check(OppositeVoxel3DDirection(OppositeVoxel3DDirection(D)) = D,
      'direction opposite is an involution ' + IntToStr(Ord(D)));
  end;

  S := MakeVoxel3DSockets('n', 'e', 's', 'w', 'u', 'd');
  Rotated := RotateVoxel3DSockets(S, v3r90);
  Check(Rotated[gdEast] = 'n', 'rotated north socket appears on east');
  Check(Rotated[gdSouth] = 'e', 'rotated east socket appears on south');
  Check(Rotated[gdWest] = 's', 'rotated south socket appears on west');
  Check(Rotated[gdNorth] = 'w', 'rotated west socket appears on north');
  Check((Rotated[gdUp] = 'u') and (Rotated[gdDown] = 'd'),
    'rotated vertical sockets are invariant');
  Check(RotateVoxel3DDirections([gdNorth, gdUp], v3r90) =
    [gdEast, gdUp], 'walk openings rotate with yaw');

  K := NewRotationKit('rotation-kit', False);
  try
    Check(K.PrototypeCount = 2, 'rotation fixture has two prototypes');
    Check(K.VariantCount = 6, 'variants expand every allowed rotation');
    for R := Low(TVoxel3DRotation) to High(TVoxel3DRotation) do
    begin
      V := K.VariantAt(Ord(R));
      Check((V.PrototypeIndex = 0) and (V.PrototypeId = 'turn'),
        'variant order retains prototype order at rotation ' +
        IntToStr(Ord(R)));
      Check(V.Rotation = R,
        'variant order uses ascending rotation ordinal ' + IntToStr(Ord(R)));
    end;
    Check(K.VariantAt(4).Rotation = v3r0,
      'second prototype first variant begins at rotation zero');
    Check(K.VariantAt(5).Rotation = v3r180,
      'sparse rotation set retains ordinal order');
    V := K.VariantAt(1);
    Check((V.Sockets[gdEast] = 'n') and
      (V.Sockets[gdUp] = 'u'), 'variant exposes already rotated sockets');
    Check(V.WalkOpenings = [gdEast, gdUp],
      'variant exposes already rotated openings');
    V.PrototypeId := 'changed';
    V.Sockets[gdEast] := 'changed';
    Check((K.VariantAt(1).PrototypeId = 'turn') and
      (K.VariantAt(1).Sockets[gdEast] = 'n'),
      'variant accessor is deeply independent');
    Check(Voxel3DPrototypeHasFlag(K.PrototypeAt(0), v3pfEmpty),
      'prototype flag helper finds present flags');
    Check(Voxel3DVariantHasFlag(K.VariantAt(0), v3pfWalkable),
      'variant flag helper finds present flags');
  finally
    K.Free;
  end;
end;

procedure TestDeterministicIdentityAndKeys;
var
  I, Found: Integer;
  K1, K2: TVoxel3DKit;
begin
  K1 := NewRotationKit('same-kit', False);
  K2 := NewRotationKit('same-kit', True);
  try
    Check(K1.Signature = K2.Signature,
      'equivalent kit declarations have the same signature');
    Check(K1.Identity = K2.Identity,
      'equivalent kit declarations have the same identity');
    Check(K1.SocketPairCount = K2.SocketPairCount,
      'pair canonicalization preserves model size');
    Check(K1.VariantCount = K2.VariantCount,
      'equivalent kits expand the same variant count');
    for I := 0 to K1.VariantCount - 1 do
    begin
      Check(K1.VariantGraphKeyAt(I) = K2.VariantGraphKeyAt(I),
        'variant graph key is replay-stable at ' + IntToStr(I));
      Check(K1.FindVariantGraphKey(K1.VariantGraphKeyAt(I), Found) and
        (Found = I), 'variant key round-trips at ' + IntToStr(I));
      Check(K1.VariantGraphKeyAt(I) <> K1.Id,
        'graph key is qualified beyond the public kit id');
    end;
    Check(not K1.FindVariantGraphKey('unknown-key', Found),
      'unknown graph key is not accepted');
    Check(not K1.FindVariantGraphKey(K1.Identity +
      ':variant:00:turn:0', Found) and (Found = -1),
      'graph-key lookup rejects a noncanonical leading-zero index');
    Check(not K1.FindVariantGraphKey(K1.Identity +
      ':variant:999999999999999999999:turn:0', Found) and
      (Found = -1), 'graph-key lookup rejects index overflow');
    Check(not K1.FindVariantGraphKey(K1.VariantGraphKeyAt(0) +
      '-tampered', Found) and (Found = -1),
      'graph-key lookup verifies the complete canonical key');
    Check(Pos(K1.Id, K1.VariantGraphKeyAt(0)) > 0,
      'graph key is visibly qualified by kit identity');
  finally
    K2.Free;
    K1.Free;
  end;
end;

procedure TestGraphKeyLookupScale;
const
  WIDTH = 8;
  HEIGHT = 4;
  DEPTH = 4;
var
  Adapter: TVoxel3DGraphAdapter;
  AllMatched: Boolean;
  Found, I, X, Y, Z: Integer;
  Graph: TGraph;
  Indices: TVoxel3DVariantIndices;
  K: TVoxel3DKit;
  Scene: TVoxel3DScene;
begin
  K := NewLookupScaleKit;
  Graph := TGraph.Create;
  Adapter := nil;
  Scene := nil;
  try
    Check(K.VariantCount = 128,
      'lookup-scale fixture expands three-digit variant indices');
    AllMatched := True;
    for I := 0 to K.VariantCount - 1 do
      if (not K.FindVariantGraphKey(K.VariantGraphKeyAt(I), Found)) or
          (Found <> I) then
        AllMatched := False;
    Check(AllMatched,
      'kit lookup exactly decodes every multi-digit variant key');

    Graph.Reshape(WIDTH, HEIGHT, DEPTH);
    Adapter := K.ApplyToGraph(Graph);
    AllMatched := True;
    for I := 0 to Adapter.VariantCount - 1 do
      if (not Adapter.FindVariantGraphKey(
          Adapter.VariantGraphKeyAt(I), Found)) or (Found <> I) then
        AllMatched := False;
    Check(AllMatched,
      'adapter lookup exactly decodes every multi-digit variant key');

    I := 0;
    for Z := 0 to DEPTH - 1 do
      for Y := 0 to HEIGHT - 1 do
        for X := 0 to WIDTH - 1 do
        begin
          Graph.Entry[X, Y, Z].Value :=
            Adapter.VariantGraphKeyAt(I mod Adapter.VariantCount);
          Inc(I);
        end;
    Scene := K.CaptureScene(Adapter);
    Indices := Scene.CopyVariantIndices;
    AllMatched := Length(Indices) = WIDTH * HEIGHT * DEPTH;
    for I := 0 to High(Indices) do
      if Indices[I] <> I mod Adapter.VariantCount then
        AllMatched := False;
    Check(AllMatched,
      'capture resolves a volume spanning every three-digit variant key');
  finally
    Scene.Free;
    Adapter.Free;
    Graph.Free;
    K.Free;
  end;
end;

procedure TestCompatibilityAndGraphRules;
var
  Adapter: TVoxel3DGraphAdapter;
  D, CoreDirection: TGraphDirection;
  Found: Integer;
  Graph: TGraph;
  I, J: Integer;
  K, DenyKit: TVoxel3DKit;
  RuleValues: TGraphValues;
  SourceKey, TargetKey: TGraphValue;
begin
  K := NewBasicKit('rules-kit');
  Graph := TGraph.Create;
  Adapter := nil;
  try
    Check(K.VariantsCompatible(BASIC_NEEDS_SUPPORT, gdDown,
      BASIC_SUPPORT), 'requiring variant accepts a provider below');
    Check(not K.VariantsCompatible(BASIC_NEEDS_SUPPORT, gdDown,
      BASIC_AIR), 'requiring variant rejects non-provider below');
    Check(K.VariantsCompatible(BASIC_SUPPORT, gdUp,
      BASIC_NEEDS_SUPPORT), 'support relation is symmetric from below');
    Check(not K.VariantsCompatible(BASIC_AIR, gdUp,
      BASIC_NEEDS_SUPPORT), 'reverse support relation rejects non-provider');
    Check(K.VariantsCompatible(BASIC_NEEDS_SUPPORT, gdEast,
      BASIC_AIR), 'support constraint only applies vertically');

    Graph.Reshape(2, 2, 2);
    Graph.WrapNeighbors := False;
    Adapter := K.ApplyToGraph(Graph);
    Check(Adapter.AppliedGraph = Graph.PassGraph[0],
      'adapter binds the exact active graph pass');
    Check((Adapter.Width = 2) and (Adapter.Height = 2) and
      (Adapter.Depth = 2), 'adapter captures graph dimensions');
    Check(not Adapter.WrapNeighbors, 'adapter captures bounded topology');
    Check(Adapter.VariantCount = K.VariantCount,
      'adapter exposes all kit variants');
    Check(Adapter.ApplicationIdentity <> Adapter.KitIdentity,
      'application identity includes topology beyond kit identity');
    for I := 0 to K.VariantCount - 1 do
    begin
      SourceKey := Adapter.VariantGraphKeyAt(I);
      Check(Adapter.FindVariantGraphKey(SourceKey, Found) and
        (Found = I), 'adapter graph-key lookup round-trips variant ' +
        IntToStr(I));
      Check(Graph.RuleGroups.ContainsKey(SourceKey),
        'graph registers variant rule group ' + IntToStr(I));
      Check(Graph.Rules[SourceKey].Weight = K.VariantAt(I).Weight,
        'graph imports variant weight ' + IntToStr(I));
      for D := Low(TGraphDirection) to High(TGraphDirection) do
      begin
        CoreDirection := OppositeVoxel3DDirection(D);
        RuleValues := Graph.Rules[SourceKey].Rule[CoreDirection].Value;
        for J := 0 to K.VariantCount - 1 do
        begin
          TargetKey := Adapter.VariantGraphKeyAt(J);
          Check(ValuesContain(RuleValues, TargetKey) =
            K.VariantsCompatible(I, D, J),
            'graph rule exactly matches natural direction ' +
            IntToStr(Ord(D)) + ', variants ' + IntToStr(I) + '/' +
            IntToStr(J));
        end;
      end;
    end;
    Check(not Adapter.FindVariantGraphKey('unknown-key', Found) and
      (Found = -1), 'adapter graph-key lookup rejects an unknown value');
  finally
    Adapter.Free;
    Graph.Free;
    K.Free;
  end;

  DenyKit := NewDenyKit;
  Graph := TGraph.Create;
  Adapter := nil;
  try
    Graph.Reshape(1, 1, 1);
    Graph.WrapNeighbors := False;
    Adapter := DenyKit.ApplyToGraph(Graph);
    SourceKey := Adapter.VariantGraphKeyAt(0);
    Check(Graph.Rules[SourceKey].Denied[gdSouth],
      'zero north neighbors becomes explicit inverse-direction deny-all');
    Check(Graph.Rules[SourceKey].Denied[gdNorth],
      'zero south neighbors becomes explicit inverse-direction deny-all');
    Check(not Graph.Rules[SourceKey].Denied[gdWest],
      'compatible east face is not denied');
    Check(Graph.Rules[SourceKey].Exists[gdWest],
      'compatible east face has a concrete inverse-direction rule');
  finally
    Adapter.Free;
    Graph.Free;
    DenyKit.Free;
  end;
end;

procedure TestSolvesAndSupport;
var
  Adapter: TVoxel3DGraphAdapter;
  Graph: TGraph;
  K: TVoxel3DKit;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
begin
  Options := DefaultGraphSolveOptions;
  Options.MaxBacktracks := 64;
  K := NewBasicKit('solve-kit');
  Graph := TGraph.Create;
  Adapter := nil;
  try
    Graph.Reshape(1, 1, 2);
    Graph.WrapNeighbors := False;
    Adapter := K.ApplyToGraph(Graph);
    Graph.SetAllowedValues(0, 0, 0,
      Adapter.VariantGraphKeyAt(BASIC_SUPPORT));
    Graph.SetAllowedValues(0, 0, 1,
      Adapter.VariantGraphKeyAt(BASIC_NEEDS_SUPPORT));
    Check(Graph.TrySolve(Options, Report),
      'bounded stack solves when below cell provides support');
    Check(not Graph[0, 0, 0].Empty and not Graph[0, 0, 1].Empty,
      'successful checked solve commits both cells');
  finally
    Adapter.Free;
    Graph.Free;
  end;

  Graph := TGraph.Create;
  Adapter := nil;
  try
    Graph.Reshape(1, 1, 2);
    Graph.WrapNeighbors := False;
    Adapter := K.ApplyToGraph(Graph);
    Graph.SetAllowedValues(0, 0, 0,
      Adapter.VariantGraphKeyAt(BASIC_AIR));
    Graph.SetAllowedValues(0, 0, 1,
      Adapter.VariantGraphKeyAt(BASIC_NEEDS_SUPPORT));
    Check(not Graph.TrySolve(Options, Report),
      'bounded stack rejects unsupported upper cell');
    Check(Graph[0, 0, 0].Empty and Graph[0, 0, 1].Empty,
      'failed checked solve commits no partial cells');
  finally
    Adapter.Free;
    Graph.Free;
  end;

  Graph := TGraph.Create;
  Adapter := nil;
  try
    Graph.Reshape(1, 1, 1);
    Graph.WrapNeighbors := False;
    Adapter := K.ApplyToGraph(Graph);
    Graph.SetAllowedValues(0, 0, 0,
      Adapter.VariantGraphKeyAt(BASIC_NEEDS_SUPPORT));
    Check(Graph.TrySolve(Options, Report),
      'bounded z=0 cell receives implicit ground support');
  finally
    Adapter.Free;
    Graph.Free;
  end;

  Graph := TGraph.Create;
  Adapter := nil;
  try
    Graph.Reshape(1, 1, 1);
    Graph.WrapNeighbors := True;
    Adapter := K.ApplyToGraph(Graph);
    Graph.SetAllowedValues(0, 0, 0,
      Adapter.VariantGraphKeyAt(BASIC_NEEDS_SUPPORT));
    Check(not Graph.TrySolve(Options, Report),
      'wrapped singleton has no implicit ground exception');
  finally
    Adapter.Free;
    Graph.Free;
  end;

  Graph := TGraph.Create;
  Adapter := nil;
  try
    Graph.Reshape(1, 1, 1);
    Graph.WrapNeighbors := True;
    Adapter := K.ApplyToGraph(Graph);
    Graph.SetAllowedValues(0, 0, 0,
      Adapter.VariantGraphKeyAt(BASIC_AIR));
    Check(Graph.TrySolve(Options, Report),
      'wrapped singleton solves when all six self-relations are compatible');
  finally
    Adapter.Free;
    Graph.Free;
  end;

  K.Free;

  K := NewMismatchKit('bounded-wrap-kit');
  Graph := TGraph.Create;
  Adapter := nil;
  try
    Graph.Reshape(2, 1, 1);
    Graph.WrapNeighbors := False;
    Adapter := K.ApplyToGraph(Graph);
    Graph.SetAllowedValues(0, 0, 0, Adapter.VariantGraphKeyAt(0));
    Graph.SetAllowedValues(1, 0, 0, Adapter.VariantGraphKeyAt(0));
    Check(Graph.TrySolve(Options, Report),
      'bounded graph solves a compatible forced pair');
  finally
    Adapter.Free;
    Graph.Free;
  end;
  Graph := TGraph.Create;
  Adapter := nil;
  try
    Graph.Reshape(2, 1, 1);
    Graph.WrapNeighbors := True;
    Adapter := K.ApplyToGraph(Graph);
    Graph.SetAllowedValues(0, 0, 0, Adapter.VariantGraphKeyAt(0));
    Graph.SetAllowedValues(1, 0, 0, Adapter.VariantGraphKeyAt(1));
    Check(not Graph.TrySolve(Options, Report),
      'wrapped graph rejects incompatible forced seam');
  finally
    Adapter.Free;
    Graph.Free;
  end;
  K.Free;
end;

procedure TestApplicationPreflightAndCaptureGuards;
var
  Adapter: TVoxel3DGraphAdapter;
  Captured: TVoxel3DScene;
  EmptyValues: TGraphValues;
  Graph: TGraph;
  K, WrongKit: TVoxel3DKit;
  Rejected: Boolean;
  RuleCount: Integer;
  RuleRows: TGraphRules;
begin
  K := NewBasicKit('guard-kit');
  Graph := TGraph.Create;
  Adapter := nil;
  try
    Rejected := False;
    try
      Adapter := K.ApplyToGraph(Graph);
    except
      on EVoxel3DAdapter do Rejected := True;
    end;
    Check(Rejected, 'application rejects an unshaped graph');
    Check(Graph.RuleGroups.Count = 0,
      'unshaped preflight rejection is atomic');
  finally
    Adapter.Free;
    Graph.Free;
  end;

  Graph := TGraph.Create;
  Adapter := nil;
  try
    Graph.Reshape(1, 1, 1).AddValue('caller-value');
    RuleCount := Graph.RuleGroups.Count;
    Rejected := False;
    try
      Adapter := K.ApplyToGraph(Graph);
    except
      on EVoxel3DAdapter do Rejected := True;
    end;
    Check(Rejected, 'application rejects a nonempty model');
    Check(Graph.RuleGroups.Count = RuleCount,
      'nonempty model rejection imports no variant keys');
  finally
    Adapter.Free;
    Graph.Free;
  end;

  Graph := TGraph.Create;
  Adapter := nil;
  try
    Graph.Reshape(1, 1, 1).AddValue('stale-value');
    Graph.RuleGroups.Clear;
    Check(Graph.RuleGroups.Count = 0,
      'stale-value fixture exposes no public rule groups');
    Rejected := False;
    try
      Adapter := K.ApplyToGraph(Graph);
    except
      on EVoxel3DAdapter do Rejected := True;
    end;
    Check(Rejected,
      'application detects hidden stale values after public group clearing');
    Check(Graph.RuleGroups.Count = 0,
      'stale-value rejection imports no variant groups');
  finally
    Adapter.Free;
    Graph.Free;
  end;

  Graph := TGraph.Create;
  Adapter := nil;
  try
    Graph.Reshape(1, 1, 1);
    Graph[0, 0, 0].Value := 'caller-lock';
    Rejected := False;
    try
      Adapter := K.ApplyToGraph(Graph);
    except
      on EVoxel3DAdapter do Rejected := True;
    end;
    Check(Rejected, 'application rejects a caller entry lock');
    Check(Graph.RuleGroups.Count = 0,
      'entry-lock rejection imports no variant keys');
  finally
    Adapter.Free;
    Graph.Free;
  end;

  Graph := TGraph.Create;
  Adapter := nil;
  try
    Graph.Reshape(1, 1, 1);
    EmptyValues := nil;
    Graph.SetAllowedValues(0, 0, 0, EmptyValues);
    Rejected := False;
    try
      Adapter := K.ApplyToGraph(Graph);
    except
      on EVoxel3DAdapter do Rejected := True;
    end;
    Check(Rejected, 'application rejects an explicit caller domain');
    Check(Graph.RuleGroups.Count = 0,
      'domain rejection imports no variant keys');
  finally
    Adapter.Free;
    Graph.Free;
  end;

  Graph := TGraph.Create;
  try
    Graph.Reshape(1, 1, 1).AddValue('manual').NewRule(
      [gdNorth, gdEast, gdSouth, gdWest, gdUp, gdDown], 'manual');
    GRunningGuardKit := K;
    GRunningGuardRaised := False;
    Graph.SelectionCallback := @RunningGuardSelection;
    Graph.Run;
    Check(GRunningGuardRaised,
      'application rejects mutation while the graph is running');
    Check(Graph.RuleGroups.Count = 1,
      'running rejection leaves the live model unchanged');
  finally
    GRunningGuardKit := nil;
    Graph.Free;
  end;

  Graph := TGraph.Create;
  Adapter := nil;
  Captured := nil;
  WrongKit := NewBasicKit('wrong-kit');
  try
    Graph.Reshape(1, 1, 1);
    Graph.WrapNeighbors := False;
    Adapter := K.ApplyToGraph(Graph);
    Rejected := False;
    try
      Captured := WrongKit.CaptureScene(Adapter);
    except
      on EVoxel3DScene do Rejected := True;
    end;
    Captured.Free;
    Captured := nil;
    Check(Rejected, 'capture rejects an adapter from another kit');

    Graph.Rules[Adapter.VariantGraphKeyAt(0)].Weight := 99;
    Rejected := False;
    try
      Captured := K.CaptureScene(Adapter);
    except
      on EVoxel3DScene do Rejected := True;
    end;
    Captured.Free;
    Check(Rejected, 'capture rejects mutated adapter graph definition');
  finally
    Adapter.Free;
    Graph.Free;
    WrongKit.Free;
  end;

  Graph := TGraph.Create;
  Adapter := nil;
  Captured := nil;
  try
    Graph.Reshape(1, 1, 1);
    Graph.WrapNeighbors := False;
    Adapter := K.ApplyToGraph(Graph);
    RuleCount := Graph.RuleGroups.Count;
    Graph.AddValue('junk');
    Graph.RuleGroups.Remove('junk');
    Check(Graph.RuleGroups.Count = RuleCount,
      'hidden-registry fixture restores the public group count');
    Rejected := False;
    try
      Captured := K.CaptureScene(Adapter);
    except
      on EVoxel3DScene do Rejected := True;
    end;
    Captured.Free;
    Check(Rejected,
      'capture detects a hidden extra registered graph value');
  finally
    Adapter.Free;
    Graph.Free;
  end;

  Graph := TGraph.Create;
  Adapter := nil;
  Captured := nil;
  try
    Graph.Reshape(1, 1, 1);
    Graph.WrapNeighbors := False;
    Adapter := K.ApplyToGraph(Graph);
    RuleRows := Graph.Rules[Adapter.VariantGraphKeyAt(0)].Rules;
    SetLength(RuleRows, Length(RuleRows) + 1);
    RuleRows[High(RuleRows)] := RuleRows[0];
    Graph.Rules[Adapter.VariantGraphKeyAt(0)].Rules := RuleRows;
    Rejected := False;
    try
      Captured := K.CaptureScene(Adapter);
    except
      on EVoxel3DScene do Rejected := True;
    end;
    Captured.Free;
    Check(Rejected, 'capture rejects duplicate direction rule records');
  finally
    Adapter.Free;
    Graph.Free;
  end;

  Graph := TGraph.Create;
  Adapter := nil;
  Captured := nil;
  try
    Graph.Reshape(1, 1, 1);
    Graph.WrapNeighbors := False;
    Adapter := K.ApplyToGraph(Graph);
    RuleRows := Graph.Rules[Adapter.VariantGraphKeyAt(0)].Rules;
    RuleRows[0].Info := True;
    Graph.Rules[Adapter.VariantGraphKeyAt(0)].Rules := RuleRows;
    Rejected := False;
    try
      Captured := K.CaptureScene(Adapter);
    except
      on EVoxel3DScene do Rejected := True;
    end;
    Captured.Free;
    Check(Rejected, 'capture rejects an invalid required rule record');
  finally
    Adapter.Free;
    Graph.Free;
  end;

  Graph := TGraph.Create;
  Adapter := nil;
  Captured := nil;
  try
    Graph.Reshape(1, 1, 1);
    Graph.WrapNeighbors := False;
    Adapter := K.ApplyToGraph(Graph);
    Rejected := False;
    try
      Captured := K.CaptureScene(Adapter);
    except
      on EVoxel3DScene do Rejected := True;
    end;
    Captured.Free;
    Captured := nil;
    Check(Rejected, 'capture rejects an empty graph cell');

    Graph[0, 0, 0].Value := 'unknown-cell-value';
    Rejected := False;
    try
      Captured := K.CaptureScene(Adapter);
    except
      on EVoxel3DScene do Rejected := True;
    end;
    Captured.Free;
    Check(Rejected, 'capture rejects an unknown graph cell value');
  finally
    Adapter.Free;
    Graph.Free;
    K.Free;
  end;
end;

procedure TestSceneCaptureAndReplay;
var
  Adapter: TVoxel3DGraphAdapter;
  CopyIndices: TVoxel3DVariantIndices;
  Graph: TGraph;
  I: Integer;
  K1, K2: TVoxel3DKit;
  Rejected: Boolean;
  RejectedScene: TVoxel3DScene;
  S1, S2, S3, Wrapped: TVoxel3DScene;
  V: TVoxel3DVariant;
begin
  K1 := NewBasicKit('scene-kit');
  K2 := NewBasicKit('scene-kit');
  S1 := NewSolvedScene(K1, 2, 1, 1, False,
    IndicesOf([BASIC_SOLID, BASIC_AIR]));
  S2 := NewSolvedScene(K2, 2, 1, 1, False,
    IndicesOf([BASIC_SOLID, BASIC_AIR]));
  S3 := NewSolvedScene(K1, 2, 1, 1, False,
    IndicesOf([BASIC_AIR, BASIC_SOLID]));
  Wrapped := NewScene(K1, 2, 1, 1, True,
    IndicesOf([BASIC_SOLID, BASIC_AIR]));
  try
    Check((S1.Width = 2) and (S1.Height = 1) and (S1.Depth = 1),
      'captured scene retains shape');
    Check(S1.CellCount = 2, 'captured scene reports cell count');
    Check(not S1.WrapNeighbors, 'captured scene retains bounded topology');
    Check(S1.KitId = K1.Id, 'captured scene retains public kit id');
    Check(S1.KitIdentity = K1.Identity,
      'captured scene retains semantic kit identity');
    Check(S1.KitSignature = K1.Signature,
      'captured scene retains kit signature');
    Check(K1.MatchesScene(S1), 'kit exactly matches its captured scene');
    Check(K2.MatchesScene(S1),
      'independent equivalent kit exactly matches captured scene');
    Check(S1.Signature = S2.Signature,
      'equivalent solve replay has stable scene signature');
    Check(S1.ApplicationIdentity = S2.ApplicationIdentity,
      'equivalent graph application has stable identity');
    Check(S1.Signature <> S3.Signature,
      'scene signature covers ordered cell variants');
    Check(S1.Signature <> Wrapped.Signature,
      'scene signature covers wrapped topology');
    Check(S1.ApplicationIdentity <> Wrapped.ApplicationIdentity,
      'application identity covers wrapped topology');
    Check(S1.VariantIndexAt(0, 0, 0) = BASIC_SOLID,
      'scene coordinate accessor uses X-fast layout');
    Check(S1.VariantIndexAt(1, 0, 0) = BASIC_AIR,
      'scene coordinate accessor returns second X cell');
    Check(S1.VariantAtCell(0, 0, 0).PrototypeId = 'solid',
      'scene typed cell accessor exposes prototype metadata');
    CopyIndices := S1.CopyVariantIndices;
    CopyIndices[0] := BASIC_PATH;
    Check(S1.VariantIndexAt(0, 0, 0) = BASIC_SOLID,
      'scene index copy cannot mutate the snapshot');
    V := S1.VariantAt(BASIC_SOLID);
    V.PrototypeId := 'changed';
    V.Sockets[gdNorth] := 'changed';
    Check((V.PrototypeId = 'changed') and
      (V.Sockets[gdNorth] = 'changed') and
      (S1.VariantAt(BASIC_SOLID).PrototypeId = 'solid') and
      (S1.VariantAt(BASIC_SOLID).Sockets[gdNorth] = 'open'),
      'scene variant accessor is deeply independent');
    for I := 0 to S1.CellCount - 1 do
      Check(S1.CopyVariantIndices[I] = S2.CopyVariantIndices[I],
        'replayed scene index matches at ' + IntToStr(I));

    Rejected := False;
    try
      S1.VariantIndexAt(2, 0, 0);
    except
      on ERangeError do Rejected := True;
    end;
    Check(Rejected, 'scene rejects out-of-bounds coordinates');
  finally
    Wrapped.Free;
    S3.Free;
    S2.Free;
    S1.Free;
  end;

  Graph := TGraph.Create;
  Adapter := nil;
  RejectedScene := nil;
  try
    Graph.Reshape(2, 1, 1);
    Graph.WrapNeighbors := False;
    Adapter := K1.ApplyToGraph(Graph);
    Rejected := False;
    try
      RejectedScene := TVoxel3DScene.Create(Adapter,
        IndicesOf([BASIC_SOLID]));
    except
      on EVoxel3DScene do Rejected := True;
    end;
    RejectedScene.Free;
    RejectedScene := nil;
    Check(Rejected, 'scene constructor rejects shape/index length mismatch');
    Rejected := False;
    try
      RejectedScene := TVoxel3DScene.Create(Adapter,
        IndicesOf([BASIC_SOLID, 999]));
    except
      on EVoxel3DScene do Rejected := True;
    end;
    RejectedScene.Free;
    Check(Rejected, 'scene constructor rejects unknown variant indices');
  finally
    Adapter.Free;
    Graph.Free;
    K2.Free;
    K1.Free;
  end;
end;

procedure DisableEntranceChecks(var AOptions: TVoxel3DValidationOptions);
begin
  AOptions.RequireEntrance := False;
  AOptions.RequireBoundaryFacingEntrance := False;
  AOptions.CheckRequiredReachability := False;
end;

procedure TestSnapshotLifetimes;
var
  Adapter: TVoxel3DGraphAdapter;
  Graph: TGraph;
  K: TVoxel3DKit;
  Mesh: TVoxel3DMesh;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
  Scene: TVoxel3DScene;
  SceneIdentity: String;
  SceneSignature: TVoxel3DSignature;
begin
  K := NewBasicKit('lifetime-kit');
  Graph := TGraph.Create;
  Adapter := nil;
  Scene := nil;
  Mesh := nil;
  try
    Graph.Reshape(1, 1, 1);
    Graph.WrapNeighbors := False;
    Adapter := K.ApplyToGraph(Graph);
    Graph.SetAllowedValues(0, 0, 0,
      Adapter.VariantGraphKeyAt(BASIC_SOLID));
    Options := DefaultGraphSolveOptions;
    Options.MaxBacktracks := 16;
    Check(Graph.TrySolve(Options, Report),
      'lifetime fixture is produced by the checked solver');
    Scene := K.CaptureScene(Adapter);
    Mesh := BuildVoxel3DMesh(K, Scene);
    SceneIdentity := Scene.KitIdentity;
    SceneSignature := Scene.Signature;

    Adapter.Free;
    Adapter := nil;
    Graph.Free;
    Graph := nil;
    K.Free;
    K := nil;

    Check(Scene.VariantIndexAt(0, 0, 0) = BASIC_SOLID,
      'captured scene indices survive adapter, graph, and kit destruction');
    Check((Scene.VariantAtCell(0, 0, 0).PrototypeId = 'solid') and
      (Scene.VariantAtCell(0, 0, 0).Material = 'stone'),
      'captured scene metadata owns its strings after source destruction');
    Check((Scene.KitIdentity = SceneIdentity) and
      (Scene.Signature = SceneSignature),
      'captured scene identities survive source destruction');

    Scene.Free;
    Scene := nil;
    Check((Mesh.QuadCount = 6) and
      (Mesh.QuadAt(0).PrototypeId = 'solid') and
      (Mesh.QuadAt(0).Material = 'stone'),
      'mesh snapshot survives kit, graph, adapter, and scene destruction');
    Check((Mesh.KitIdentity = SceneIdentity) and
      (Mesh.SceneSignature = SceneSignature),
      'mesh retains independent identity metadata for its lifetime');
  finally
    Mesh.Free;
    Scene.Free;
    Adapter.Free;
    Graph.Free;
    K.Free;
  end;
end;

procedure TestValidator;
var
  Description: String;
  I: Integer;
  Indices: TVoxel3DVariantIndices;
  K, Other, Mismatch, Seam: TVoxel3DKit;
  Options: TVoxel3DValidationOptions;
  Report: TVoxel3DValidationReport;
  Scene: TVoxel3DScene;
begin
  K := NewBasicKit('validation-kit');
  Scene := NewScene(K, 3, 1, 1, False,
    IndicesOf([BASIC_ENTRANCE, BASIC_PATH, BASIC_PATH]));
  try
    Check(ValidateVoxel3DScene(K, Scene, Report),
      'validator accepts a connected, supported, socket-valid scene');
    Check(Report.Valid, 'valid report mirrors result');
    Check(Report.CheckedCells = 3, 'validator reports checked cell count');
    Check(Report.EntranceCount = 1, 'validator counts entrances');
    Check(Report.ReachableCount = 3,
      'reachability includes entrance and connected walkable cells');
    Check(Report.Issue.Kind = v3vikNone, 'valid scene has no issue');
  finally
    Scene.Free;
  end;

  Scene := NewScene(K, 1, 1, 1, False, IndicesOf([BASIC_AIR]));
  try
    Check(not ValidateVoxel3DScene(K, Scene, Report),
      'validator rejects a missing required entrance');
    Check(Report.Issue.Kind = v3vikMissingEntrance,
      'missing entrance has precise issue kind');
    Description := DescribeVoxel3DValidationIssue(K, Report.Issue);
    Check(Pos('missing-entrance', Description) > 0,
      'missing entrance description names the issue');
  finally
    Scene.Free;
  end;

  SetLength(Indices, 9);
  for I := 0 to High(Indices) do
    Indices[I] := BASIC_AIR;
  Indices[4] := BASIC_ENTRANCE;
  Scene := NewScene(K, 3, 3, 1, False, Indices);
  try
    Check(not ValidateVoxel3DScene(K, Scene, Report),
      'validator rejects an entrance with no boundary-facing opening');
    Check(Report.Issue.Kind = v3vikEntranceNotBoundaryFacing,
      'interior entrance has precise issue kind');
    Check((Report.Issue.X = 1) and (Report.Issue.Y = 1),
      'interior entrance issue reports coordinates');
  finally
    Scene.Free;
  end;

  Scene := NewScene(K, 3, 1, 1, False,
    IndicesOf([BASIC_ENTRANCE, BASIC_AIR, BASIC_PATH]));
  try
    Check(not ValidateVoxel3DScene(K, Scene, Report),
      'validator rejects a required cell unreachable through openings');
    Check(Report.Issue.Kind = v3vikUnreachable,
      'unreachable cell has precise issue kind');
    Check(Report.Issue.X = 2, 'unreachable issue identifies required cell');
    Description := DescribeVoxel3DValidationIssue(K, Report.Issue);
    Check((Pos('unreachable', Description) > 0) and
      (Pos('(2,0,0)', Description) > 0),
      'unreachable description includes kind and coordinates');
  finally
    Scene.Free;
  end;

  Scene := NewScene(K, 1, 1, 2, False,
    IndicesOf([BASIC_AIR, BASIC_NEEDS_SUPPORT]));
  Options := DefaultVoxel3DValidationOptions;
  Options.RequireEntrance := False;
  Options.RequireBoundaryFacingEntrance := False;
  Options.CheckRequiredReachability := False;
  try
    Check(not ValidateVoxel3DScene(K, Scene, Options, Report),
      'validator rejects unsupported elevated cell');
    Check(Report.Issue.Kind = v3vikUnsupported,
      'unsupported cell has precise issue kind');
    Check(Report.Issue.HasDirection and (Report.Issue.Direction = gdDown),
      'unsupported issue identifies below relation');
  finally
    Scene.Free;
  end;

  Mismatch := NewMismatchKit('mismatch-validation');
  Scene := NewScene(Mismatch, 2, 1, 1, False, IndicesOf([0, 1]));
  Options := DefaultVoxel3DValidationOptions;
  DisableEntranceChecks(Options);
  try
    Check(not ValidateVoxel3DScene(Mismatch, Scene, Options, Report),
      'validator independently detects socket mismatch');
    Check(Report.Issue.Kind = v3vikSocketMismatch,
      'socket mismatch has precise issue kind');
    Check(Report.Issue.HasDirection and
      (Report.Issue.NeighborVariantIndex >= 0),
      'socket mismatch records its neighbor relation');
    Description := DescribeVoxel3DValidationIssue(Mismatch, Report.Issue);
    Check((Pos('socket-mismatch', Description) > 0) and
      (Pos('toward', Description) > 0),
      'socket mismatch description includes relation context');
  finally
    Scene.Free;
    Mismatch.Free;
  end;

  Seam := NewSeamKit;
  Options := DefaultVoxel3DValidationOptions;
  DisableEntranceChecks(Options);
  Scene := NewScene(Seam, 2, 1, 1, False, IndicesOf([0, 1]));
  try
    Check(ValidateVoxel3DScene(Seam, Scene, Options, Report),
      'bounded validator ignores absent external seam');
  finally
    Scene.Free;
  end;
  Scene := NewScene(Seam, 2, 1, 1, True, IndicesOf([0, 1]));
  try
    Check(not ValidateVoxel3DScene(Seam, Scene, Options, Report),
      'wrapped validator checks cross-seam sockets');
    Check(Report.Issue.Kind = v3vikSocketMismatch,
      'wrapped seam failure is a socket mismatch');
  finally
    Scene.Free;
    Seam.Free;
  end;

  Other := NewBasicKit('other-validation-kit');
  Scene := NewScene(K, 1, 1, 1, False, IndicesOf([BASIC_AIR]));
  try
    Check(not ValidateVoxel3DScene(Other, Scene, Report),
      'validator rejects wrong kit witness');
    Check(Report.Issue.Kind = v3vikKitIdentity,
      'wrong kit has precise identity issue');
  finally
    Scene.Free;
    Other.Free;
  end;

  Check(Voxel3DValidationIssueName(v3vikNone) = 'none',
    'none issue has stable name');
  Check(Voxel3DValidationIssueName(v3vikKitIdentity) = 'kit-identity',
    'identity issue has stable name');
  Check(Voxel3DValidationIssueName(v3vikSocketMismatch) = 'socket-mismatch',
    'socket issue has stable name');
  Check(Voxel3DValidationIssueName(v3vikUnsupported) = 'unsupported',
    'support issue has stable name');
  Check(Voxel3DValidationIssueName(v3vikMissingEntrance) = 'missing-entrance',
    'missing entrance issue has stable name');
  Check(Voxel3DValidationIssueName(v3vikEntranceNotBoundaryFacing) =
    'entrance-not-boundary-facing', 'boundary issue has stable name');
  Check(Voxel3DValidationIssueName(v3vikUnreachable) = 'unreachable',
    'reachability issue has stable name');
  K.Free;
end;

function VectorEquals(const AVector: TVoxel3DMeshVector;
  const AX, AY, AZ: Integer): Boolean;
begin
  Result := (AVector.X = AX) and (AVector.Y = AY) and
    (AVector.Z = AZ);
end;

procedure TestMesh;
const
  EXPECTED_TWO_SOLID_DIRECTIONS: array[0..9] of TGraphDirection = (
    gdNorth, gdSouth, gdWest, gdUp, gdDown,
    gdNorth, gdEast, gdSouth, gdUp, gdDown);
var
  CopyQuads: TVoxel3DMeshQuads;
  I: Integer;
  K, WrongKit: TVoxel3DKit;
  Mesh: TVoxel3DMesh;
  Q: TVoxel3DMeshQuad;
  Rejected: Boolean;
  Scene: TVoxel3DScene;
  WrongMesh: TVoxel3DMesh;
begin
  K := NewBasicKit('mesh-kit');
  Scene := NewScene(K, 1, 1, 1, False, IndicesOf([BASIC_SOLID]));
  Mesh := BuildVoxel3DMesh(K, Scene);
  try
    Check(Mesh.QuadCount = 6, 'one bounded solid cube emits six quads');
    Check((Mesh.Width = 1) and (Mesh.Height = 1) and (Mesh.Depth = 1),
      'mesh retains scene shape');
    Check(not Mesh.WrapNeighbors, 'mesh retains bounded topology');
    Check(Mesh.KitIdentity = K.Identity, 'mesh retains kit identity');
    Check(Mesh.SceneSignature = Scene.Signature,
      'mesh retains source scene signature');
    for I := 0 to 5 do
      Check(Mesh.QuadAt(I).Direction = TGraphDirection(I),
        'one-cube faces follow direction ordinal ' + IntToStr(I));
    Q := Mesh.QuadAt(0);
    Check(VectorEquals(Q.Normal, 0, 1, 0),
      'north face has stable outward normal');
    Check(VectorEquals(Q.Vertices[0], 0, 1, 0) and
      VectorEquals(Q.Vertices[1], 0, 1, 1) and
      VectorEquals(Q.Vertices[2], 1, 1, 1) and
      VectorEquals(Q.Vertices[3], 1, 1, 0),
      'north face has stable counter-clockwise vertices');
    Q := Mesh.QuadAt(1);
    Check(VectorEquals(Q.Normal, 1, 0, 0) and
      VectorEquals(Q.Vertices[0], 1, 0, 0) and
      VectorEquals(Q.Vertices[1], 1, 1, 0) and
      VectorEquals(Q.Vertices[2], 1, 1, 1) and
      VectorEquals(Q.Vertices[3], 1, 0, 1),
      'east face has stable normal and counter-clockwise vertices');
    Q := Mesh.QuadAt(2);
    Check(VectorEquals(Q.Normal, 0, -1, 0) and
      VectorEquals(Q.Vertices[0], 0, 0, 0) and
      VectorEquals(Q.Vertices[1], 1, 0, 0) and
      VectorEquals(Q.Vertices[2], 1, 0, 1) and
      VectorEquals(Q.Vertices[3], 0, 0, 1),
      'south face has stable normal and counter-clockwise vertices');
    Q := Mesh.QuadAt(3);
    Check(VectorEquals(Q.Normal, -1, 0, 0) and
      VectorEquals(Q.Vertices[0], 0, 0, 0) and
      VectorEquals(Q.Vertices[1], 0, 0, 1) and
      VectorEquals(Q.Vertices[2], 0, 1, 1) and
      VectorEquals(Q.Vertices[3], 0, 1, 0),
      'west face has stable normal and counter-clockwise vertices');
    Q := Mesh.QuadAt(4);
    Check(VectorEquals(Q.Normal, 0, 0, 1) and
      VectorEquals(Q.Vertices[0], 0, 0, 1) and
      VectorEquals(Q.Vertices[1], 1, 0, 1) and
      VectorEquals(Q.Vertices[2], 1, 1, 1) and
      VectorEquals(Q.Vertices[3], 0, 1, 1),
      'up face has stable normal and counter-clockwise vertices');
    Q := Mesh.QuadAt(5);
    Check(VectorEquals(Q.Normal, 0, 0, -1) and
      VectorEquals(Q.Vertices[0], 0, 0, 0) and
      VectorEquals(Q.Vertices[1], 0, 1, 0) and
      VectorEquals(Q.Vertices[2], 1, 1, 0) and
      VectorEquals(Q.Vertices[3], 1, 0, 0),
      'down face has stable normal and counter-clockwise vertices');
    Q := Mesh.QuadAt(0);
    Check((Q.CellX = 0) and (Q.CellY = 0) and (Q.CellZ = 0),
      'quad retains source cell coordinates');
    Check((Q.VariantIndex = BASIC_SOLID) and (Q.PrototypeIndex = 0) and
      (Q.PrototypeId = 'solid') and (Q.Material = 'stone') and
      (Q.Rotation = v3r0), 'quad retains renderer-neutral metadata');

    CopyQuads := Mesh.CopyQuads;
    CopyQuads[0].Material := 'changed';
    CopyQuads[0].Vertices[0].X := 99;
    Check((Mesh.QuadAt(0).Material = 'stone') and
      (Mesh.QuadAt(0).Vertices[0].X = 0),
      'mesh quad array copy cannot mutate snapshot');
    Q := Mesh.QuadAt(0);
    Q.PrototypeId := 'changed';
    Check(Mesh.QuadAt(0).PrototypeId = 'solid',
      'quad accessor returns independent string metadata');
  finally
    Mesh.Free;
    Scene.Free;
  end;

  Scene := NewScene(K, 2, 1, 1, False,
    IndicesOf([BASIC_SOLID, BASIC_SOLID]));
  Mesh := BuildVoxel3DMesh(K, Scene);
  try
    Check(Mesh.QuadCount = 10,
      'two adjacent solid cubes cull their shared faces');
    for I := 0 to High(EXPECTED_TWO_SOLID_DIRECTIONS) do
      Check(Mesh.QuadAt(I).Direction = EXPECTED_TWO_SOLID_DIRECTIONS[I],
        'two-cube face order is stable at ' + IntToStr(I));
    Check((Mesh.QuadAt(0).CellX = 0) and
      (Mesh.QuadAt(5).CellX = 1),
      'mesh uses X-fast cell order');
  finally
    Mesh.Free;
    Scene.Free;
  end;

  Scene := NewScene(K, 2, 1, 1, False,
    IndicesOf([BASIC_SOLID, BASIC_AIR]));
  Mesh := BuildVoxel3DMesh(K, Scene);
  try
    Check(Mesh.QuadCount = 6,
      'non-solid neighbor does not cull a solid face');
  finally
    Mesh.Free;
    Scene.Free;
  end;

  Scene := NewScene(K, 1, 1, 1, True, IndicesOf([BASIC_SOLID]));
  Mesh := BuildVoxel3DMesh(K, Scene);
  try
    Check(Mesh.QuadCount = 0,
      'wrapped singleton culls every face against itself');
  finally
    Mesh.Free;
    Scene.Free;
  end;

  WrongKit := NewBasicKit('wrong-mesh-kit');
  Scene := NewScene(K, 1, 1, 1, False, IndicesOf([BASIC_SOLID]));
  Rejected := False;
  WrongMesh := nil;
  try
    try
      WrongMesh := BuildVoxel3DMesh(WrongKit, Scene);
    except
      on EVoxel3DMesh do Rejected := True;
    end;
    WrongMesh.Free;
    Check(Rejected, 'mesh build rejects wrong kit witness');
  finally
    Scene.Free;
    WrongKit.Free;
    K.Free;
  end;
end;

begin
  WriteLn('WFC Voxel 3D Foundation tests');
  RunTest('versions and portable tokens', @TestVersionsAndTokens);
  RunTest('constructor guards and deep ownership',
    @TestConstructorGuardsAndOwnership);
  RunTest('nil and accessor guards', @TestNilAndAccessorGuards);
  RunTest('yaw rotations and deterministic variants', @TestRotationsAndVariants);
  RunTest('semantic identity and graph-key replay',
    @TestDeterministicIdentityAndKeys);
  RunTest('constant-time graph-key capture lookup',
    @TestGraphKeyLookupScale);
  RunTest('six-direction compatibility and graph rules',
    @TestCompatibilityAndGraphRules);
  RunTest('bounded, wrapped, and support solves', @TestSolvesAndSupport);
  RunTest('atomic application and capture guards',
    @TestApplicationPreflightAndCaptureGuards);
  RunTest('scene capture, ownership, and replay', @TestSceneCaptureAndReplay);
  RunTest('scene and mesh snapshot lifetimes', @TestSnapshotLifetimes);
  RunTest('independent semantic validation', @TestValidator);
  RunTest('renderer-neutral mesh extraction', @TestMesh);
  WriteLn('[SUMMARY] ', GCheckCount, ' checks, ', GFailureCount, ' failures');
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.Create(IntToStr(GFailureCount) +
      ' voxel 3D test failures');
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
