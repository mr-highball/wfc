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
unit wfc_voxel3d_mesh;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_voxel3d;

const
  WFC_VOXEL3D_MESH_VERSION = 1;

type
  EVoxel3DMesh = class(EVoxel3D);

  //Mesh coordinates are signed Integer lattice values on both native FPC and
  //pas2js. A checked build rejects scene dimensions whose positive boundary
  //cannot be represented after adding the unit-cube endpoint.
  TVoxel3DMeshVector = record
    X: Integer;
    Y: Integer;
    Z: Integer;
  end;
  TVoxel3DMeshVertices = array[0..3] of TVoxel3DMeshVector;

  //Vertices are counter-clockwise when viewed from outside the solid cell.
  //PrototypeId and Material are copied public tokens; no private graph value
  //or renderer object crosses this boundary.
  TVoxel3DMeshQuad = record
    Vertices: TVoxel3DMeshVertices;
    Normal: TVoxel3DMeshVector;
    CellX: Integer;
    CellY: Integer;
    CellZ: Integer;
    Direction: TGraphDirection;
    VariantIndex: Integer;
    PrototypeIndex: Integer;
    PrototypeId: String;
    Material: String;
    Rotation: TVoxel3DRotation;
  end;
  TVoxel3DMeshQuads = array of TVoxel3DMeshQuad;

  { TVoxel3DMesh }

  //An immutable, renderer-neutral snapshot. Quad order is cell Z/Y/X with X
  //fastest, followed by TGraphDirection ordinal within each solid cell.
  TVoxel3DMesh = class
  private
    FApplicationIdentity: String;
    FDepth: Integer;
    FHeight: Integer;
    FKitId: String;
    FKitIdentity: String;
    FKitSignature: TVoxel3DSignature;
    FQuads: TVoxel3DMeshQuads;
    FSceneSignature: TVoxel3DSignature;
    FWidth: Integer;
    FWrapNeighbors: Boolean;

    function GetQuadCount: Integer;
  public
    constructor Create(const AKit: TVoxel3DKit;
      const AScene: TVoxel3DScene);
    function QuadAt(const AIndex: Integer): TVoxel3DMeshQuad;
    function CopyQuads: TVoxel3DMeshQuads;

    property ApplicationIdentity: String read FApplicationIdentity;
    property Depth: Integer read FDepth;
    property Height: Integer read FHeight;
    property KitId: String read FKitId;
    property KitIdentity: String read FKitIdentity;
    property KitSignature: TVoxel3DSignature read FKitSignature;
    property QuadCount: Integer read GetQuadCount;
    property SceneSignature: TVoxel3DSignature read FSceneSignature;
    property Width: Integer read FWidth;
    property WrapNeighbors: Boolean read FWrapNeighbors;
  end;

//The kit is required as an independent identity witness. The builder compares
//the full ordered variant definition as well as public identities before
//reading scene cells. The returned mesh is caller-owned.
function BuildVoxel3DMesh(const AKit: TVoxel3DKit;
  const AScene: TVoxel3DScene): TVoxel3DMesh;

implementation

type
  TMeshBooleanArray = array of Boolean;

function MeshVector(const AX, AY, AZ: Integer): TVoxel3DMeshVector;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
end;

function CopyMeshQuad(const AQuad: TVoxel3DMeshQuad): TVoxel3DMeshQuad;
var
  I: Integer;
begin
  for I := Low(Result.Vertices) to High(Result.Vertices) do
    Result.Vertices[I] := AQuad.Vertices[I];
  Result.Normal := AQuad.Normal;
  Result.CellX := AQuad.CellX;
  Result.CellY := AQuad.CellY;
  Result.CellZ := AQuad.CellZ;
  Result.Direction := AQuad.Direction;
  Result.VariantIndex := AQuad.VariantIndex;
  Result.PrototypeIndex := AQuad.PrototypeIndex;
  Result.PrototypeId := AQuad.PrototypeId;
  Result.Material := AQuad.Material;
  Result.Rotation := AQuad.Rotation;
end;

function CheckedMeshDimension(const AValue: TGraphCoordinate;
  const AName: String): Integer;
begin
  if AValue = 0 then
    raise EVoxel3DMesh.Create('voxel mesh ' + AName +
      ' must be positive');
  if AValue > TGraphCoordinate(High(Integer) - 1) then
    raise ERangeError.Create('voxel mesh ' + AName +
      ' exceeds the integer lattice');
  Result := Integer(AValue);
end;

function CheckedMeshCellCount(const AWidth, AHeight,
  ADepth: Integer): Integer;
var
  LPlane: Integer;
begin
  if AWidth > High(Integer) div AHeight then
    raise ERangeError.Create('voxel mesh plane size exceeds Integer');
  LPlane := AWidth * AHeight;
  if LPlane > High(Integer) div ADepth then
    raise ERangeError.Create('voxel mesh cell count exceeds Integer');
  Result := LPlane * ADepth;
end;

function CellIndex(const AX, AY, AZ, AWidth,
  AHeight: Integer): Integer;
begin
  Result := (AZ * AWidth * AHeight) + (AY * AWidth) + AX;
end;

function ResolveNeighborIndex(const AX, AY, AZ, AWidth, AHeight,
  ADepth: Integer; const AWrap: Boolean;
  const ADirection: TGraphDirection; out AIndex: Integer): Boolean;
var
  LX, LY, LZ: Integer;
begin
  LX := AX;
  LY := AY;
  LZ := AZ;
  case ADirection of
    gdNorth:
      if LY < AHeight - 1 then
        Inc(LY)
      else if AWrap then
        LY := 0
      else
        Exit(False);
    gdEast:
      if LX < AWidth - 1 then
        Inc(LX)
      else if AWrap then
        LX := 0
      else
        Exit(False);
    gdSouth:
      if LY > 0 then
        Dec(LY)
      else if AWrap then
        LY := AHeight - 1
      else
        Exit(False);
    gdWest:
      if LX > 0 then
        Dec(LX)
      else if AWrap then
        LX := AWidth - 1
      else
        Exit(False);
    gdUp:
      if LZ < ADepth - 1 then
        Inc(LZ)
      else if AWrap then
        LZ := 0
      else
        Exit(False);
    gdDown:
      if LZ > 0 then
        Dec(LZ)
      else if AWrap then
        LZ := ADepth - 1
      else
        Exit(False);
  end;
  AIndex := CellIndex(LX, LY, LZ, AWidth, AHeight);
  Result := True;
end;

function FaceIsExposed(const AX, AY, AZ, AWidth, AHeight,
  ADepth: Integer; const AWrap: Boolean;
  const ADirection: TGraphDirection;
  const AVariantIndices: TVoxel3DVariantIndices;
  const ASolidVariants: TMeshBooleanArray): Boolean;
var
  LNeighborIndex: Integer;
begin
  if not ResolveNeighborIndex(AX, AY, AZ, AWidth, AHeight,
      ADepth, AWrap, ADirection, LNeighborIndex) then
    Exit(True);
  Result := not ASolidVariants[AVariantIndices[LNeighborIndex]];
end;

procedure SetQuadGeometry(var AQuad: TVoxel3DMeshQuad;
  const AX, AY, AZ: Integer; const ADirection: TGraphDirection);
var
  LX1, LY1, LZ1: Integer;
begin
  //The checked dimension bound makes these additions safe.
  LX1 := AX + 1;
  LY1 := AY + 1;
  LZ1 := AZ + 1;
  case ADirection of
    gdNorth:
      begin
        AQuad.Normal := MeshVector(0, 1, 0);
        AQuad.Vertices[0] := MeshVector(AX, LY1, AZ);
        AQuad.Vertices[1] := MeshVector(AX, LY1, LZ1);
        AQuad.Vertices[2] := MeshVector(LX1, LY1, LZ1);
        AQuad.Vertices[3] := MeshVector(LX1, LY1, AZ);
      end;
    gdEast:
      begin
        AQuad.Normal := MeshVector(1, 0, 0);
        AQuad.Vertices[0] := MeshVector(LX1, AY, AZ);
        AQuad.Vertices[1] := MeshVector(LX1, LY1, AZ);
        AQuad.Vertices[2] := MeshVector(LX1, LY1, LZ1);
        AQuad.Vertices[3] := MeshVector(LX1, AY, LZ1);
      end;
    gdSouth:
      begin
        AQuad.Normal := MeshVector(0, -1, 0);
        AQuad.Vertices[0] := MeshVector(AX, AY, AZ);
        AQuad.Vertices[1] := MeshVector(LX1, AY, AZ);
        AQuad.Vertices[2] := MeshVector(LX1, AY, LZ1);
        AQuad.Vertices[3] := MeshVector(AX, AY, LZ1);
      end;
    gdWest:
      begin
        AQuad.Normal := MeshVector(-1, 0, 0);
        AQuad.Vertices[0] := MeshVector(AX, AY, AZ);
        AQuad.Vertices[1] := MeshVector(AX, AY, LZ1);
        AQuad.Vertices[2] := MeshVector(AX, LY1, LZ1);
        AQuad.Vertices[3] := MeshVector(AX, LY1, AZ);
      end;
    gdUp:
      begin
        AQuad.Normal := MeshVector(0, 0, 1);
        AQuad.Vertices[0] := MeshVector(AX, AY, LZ1);
        AQuad.Vertices[1] := MeshVector(LX1, AY, LZ1);
        AQuad.Vertices[2] := MeshVector(LX1, LY1, LZ1);
        AQuad.Vertices[3] := MeshVector(AX, LY1, LZ1);
      end;
    gdDown:
      begin
        AQuad.Normal := MeshVector(0, 0, -1);
        AQuad.Vertices[0] := MeshVector(AX, AY, AZ);
        AQuad.Vertices[1] := MeshVector(AX, LY1, AZ);
        AQuad.Vertices[2] := MeshVector(LX1, LY1, AZ);
        AQuad.Vertices[3] := MeshVector(LX1, AY, AZ);
      end;
  end;
end;

procedure SetQuadMetadata(var AQuad: TVoxel3DMeshQuad;
  const AX, AY, AZ: Integer; const ADirection: TGraphDirection;
  const AVariantIndex: Integer; const AVariant: TVoxel3DVariant);
begin
  AQuad.CellX := AX;
  AQuad.CellY := AY;
  AQuad.CellZ := AZ;
  AQuad.Direction := ADirection;
  AQuad.VariantIndex := AVariantIndex;
  AQuad.PrototypeIndex := AVariant.PrototypeIndex;
  AQuad.PrototypeId := AVariant.PrototypeId;
  AQuad.Material := AVariant.Material;
  AQuad.Rotation := AVariant.Rotation;
end;

procedure ValidateKitSceneIdentity(const AKit: TVoxel3DKit;
  const AScene: TVoxel3DScene);
begin
  if not Assigned(AKit) then
    raise EVoxel3DMesh.Create('voxel mesh kit cannot be nil');
  if not Assigned(AScene) then
    raise EVoxel3DMesh.Create('voxel mesh scene cannot be nil');
  if (AKit.Id <> AScene.KitId) or
      (AKit.Identity <> AScene.KitIdentity) or
      (AKit.Signature <> AScene.KitSignature) or
      (AKit.VariantCount <> AScene.VariantCount) then
    raise EVoxel3DMesh.Create(
      'voxel mesh kit does not match the captured scene');
  if not AKit.MatchesScene(AScene) then
    raise EVoxel3DMesh.Create(
      'voxel mesh kit semantics do not match the captured scene');
end;

{ TVoxel3DMesh }

function TVoxel3DMesh.GetQuadCount: Integer;
begin
  Result := Length(FQuads);
end;

function TVoxel3DMesh.QuadAt(
  const AIndex: Integer): TVoxel3DMeshQuad;
begin
  if (AIndex < 0) or (AIndex >= Length(FQuads)) then
    raise ERangeError.CreateFmt(
      'voxel mesh quad index out of bounds [%d]', [AIndex]);
  Result := CopyMeshQuad(FQuads[AIndex]);
end;

function TVoxel3DMesh.CopyQuads: TVoxel3DMeshQuads;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FQuads));
  for I := 0 to High(FQuads) do
    Result[I] := CopyMeshQuad(FQuads[I]);
end;

constructor TVoxel3DMesh.Create(const AKit: TVoxel3DKit;
  const AScene: TVoxel3DScene);
var
  I: Integer;
  LCellCount: Integer;
  LCellIndex: Integer;
  LDepth: Integer;
  LDirection: TGraphDirection;
  LHeight: Integer;
  LQuadCount: Integer;
  LQuadIndex: Integer;
  LQuads: TVoxel3DMeshQuads;
  LSolidVariants: TMeshBooleanArray;
  LVariant: TVoxel3DVariant;
  LVariantIndex: Integer;
  LVariantIndices: TVoxel3DVariantIndices;
  LWidth: Integer;
  X, Y, Z: Integer;
begin
  inherited Create;
  ValidateKitSceneIdentity(AKit, AScene);
  LWidth := CheckedMeshDimension(AScene.Width, 'width');
  LHeight := CheckedMeshDimension(AScene.Height, 'height');
  LDepth := CheckedMeshDimension(AScene.Depth, 'depth');
  LCellCount := CheckedMeshCellCount(LWidth, LHeight, LDepth);
  if AScene.CellCount <> LCellCount then
    raise EVoxel3DMesh.Create(
      'voxel mesh scene cell count does not match its dimensions');

  SetLength(LSolidVariants, AScene.VariantCount);
  for I := 0 to AScene.VariantCount - 1 do
    LSolidVariants[I] := Voxel3DVariantHasFlag(
      AScene.VariantAt(I), v3pfSolid);
  LVariantIndices := AScene.CopyVariantIndices;
  if Length(LVariantIndices) <> LCellCount then
    raise EVoxel3DMesh.Create(
      'voxel mesh scene variant storage does not match its dimensions');
  for I := 0 to High(LVariantIndices) do
    if (LVariantIndices[I] < 0) or
        (LVariantIndices[I] >= Length(LSolidVariants)) then
      raise EVoxel3DMesh.CreateFmt(
        'voxel mesh scene variant index is out of bounds at cell %d', [I]);

  LQuadCount := 0;
  LCellIndex := 0;
  for Z := 0 to LDepth - 1 do
    for Y := 0 to LHeight - 1 do
      for X := 0 to LWidth - 1 do
      begin
        LVariantIndex := LVariantIndices[LCellIndex];
        if LSolidVariants[LVariantIndex] then
          for LDirection := Low(TGraphDirection) to
              High(TGraphDirection) do
            if FaceIsExposed(X, Y, Z, LWidth, LHeight, LDepth,
                AScene.WrapNeighbors, LDirection, LVariantIndices,
                LSolidVariants) then
            begin
              if LQuadCount = High(Integer) then
                raise ERangeError.Create(
                  'voxel mesh quad count exceeds Integer');
              Inc(LQuadCount);
            end;
        Inc(LCellIndex);
      end;

  SetLength(LQuads, LQuadCount);
  LQuadIndex := 0;
  LCellIndex := 0;
  for Z := 0 to LDepth - 1 do
    for Y := 0 to LHeight - 1 do
      for X := 0 to LWidth - 1 do
      begin
        LVariantIndex := LVariantIndices[LCellIndex];
        if LSolidVariants[LVariantIndex] then
        begin
          LVariant := AScene.VariantAt(LVariantIndex);
          for LDirection := Low(TGraphDirection) to
              High(TGraphDirection) do
            if FaceIsExposed(X, Y, Z, LWidth, LHeight, LDepth,
                AScene.WrapNeighbors, LDirection, LVariantIndices,
                LSolidVariants) then
            begin
              SetQuadGeometry(LQuads[LQuadIndex], X, Y, Z,
                LDirection);
              SetQuadMetadata(LQuads[LQuadIndex], X, Y, Z,
                LDirection, LVariantIndex, LVariant);
              Inc(LQuadIndex);
            end;
        end;
        Inc(LCellIndex);
      end;
  if LQuadIndex <> LQuadCount then
    raise EVoxel3DMesh.Create(
      'voxel mesh extraction produced an inconsistent quad count');
  FApplicationIdentity := AScene.ApplicationIdentity;
  FWidth := LWidth;
  FHeight := LHeight;
  FDepth := LDepth;
  FKitId := AScene.KitId;
  FKitIdentity := AScene.KitIdentity;
  FKitSignature := AScene.KitSignature;
  FSceneSignature := AScene.Signature;
  FWrapNeighbors := AScene.WrapNeighbors;
  SetLength(FQuads, Length(LQuads));
  for I := 0 to High(LQuads) do
    FQuads[I] := CopyMeshQuad(LQuads[I]);
end;

function BuildVoxel3DMesh(const AKit: TVoxel3DKit;
  const AScene: TVoxel3DScene): TVoxel3DMesh;
begin
  Result := TVoxel3DMesh.Create(AKit, AScene);
end;

end.
