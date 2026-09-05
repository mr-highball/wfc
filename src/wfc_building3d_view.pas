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
unit wfc_building3d_view;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_voxel3d,
  wfc_voxel3d_isometric,
  wfc_building3d;

const
  WFC_BUILDING3D_VIEW_VERSION = 1;
  WFC_BUILDING3D_VIEW_GEOMETRY_VERSION = 1;
  WFC_BUILDING3D_VIEW_STYLE_VERSION = 1;
  WFC_BUILDING3D_VIEW_SUBCELL_SCALE = WFC_VOXEL3D_SUBCELL_SCALE;

type
  EBuilding3DView = class(EBuilding3D);

  TBuilding3DViewMode = (
    b3vmFootprint,
    b3vmStructure,
    b3vmEnvelopeRoof,
    b3vmComplete
  );

  //Envelope/roof is deliberately not a geometry source. It styles the exact
  //matching structure face instead of pretending that a semantic overlay is
  //a second solid voxel mesh.
  TBuilding3DViewGeometryKind = (
    b3vgFootprint,
    b3vgStructure,
    b3vgProp
  );

  TBuilding3DViewStyle = record
    FillColor: TVoxel3DColor;
    EdgeColor: TVoxel3DColor;
  end;

  //Every emitted face carries a complete, public same-cell account of all
  //four passes. Prototype identifiers, materials, and rotations come from
  //the immutable public voxel variants; graph adapter keys never enter this
  //record.
  TBuilding3DViewLineage = record
    FootprintRole: TBuilding3DFootprintRole;
    FootprintToken: String;

    StructureKind: TBuilding3DStructureKind;
    StructurePrototypeId: String;
    StructureMaterial: String;
    StructureRotation: TVoxel3DRotation;

    EnvelopeKind: TBuilding3DEnvelopeKind;
    EnvelopePrototypeId: String;
    EnvelopeMaterial: String;
    EnvelopeRotation: TVoxel3DRotation;

    PropKind: TBuilding3DPropKind;
    PropPrototypeId: String;
    PropMaterial: String;
    PropRotation: TVoxel3DRotation;
  end;

  TBuilding3DViewFace = record
    Quad: TVoxel3DViewQuad;
    GeometryKind: TBuilding3DViewGeometryKind;
    Lineage: TBuilding3DViewLineage;
  end;
  TBuilding3DViewFaces = array of TBuilding3DViewFace;

  { TBuilding3DView }

  //An immutable renderer-neutral snapshot. World vertices are signed Integer
  //fixed-subcell coordinates: WFC_BUILDING3D_VIEW_SUBCELL_SCALE units equal
  //one building cell. Face order is deterministic and Copy* arrays detach.
  TBuilding3DView = class
  private
    FBuildingSignature: TBuilding3DSignature;
    FDepth: Integer;
    FFaces: TBuilding3DViewFaces;
    FHeight: Integer;
    FMode: TBuilding3DViewMode;
    FPipelineSignature: String;
    FSeed: TGraphSeed;
    FWidth: Integer;
    function GetFaceCount: Integer;
  public
    constructor Create(const ABuilding: TBuilding3D;
      const AMode: TBuilding3DViewMode);
    function FaceAt(const AIndex: Integer): TBuilding3DViewFace;
    function CopyFaces: TBuilding3DViewFaces;
    function CopyQuads: TVoxel3DViewQuads;

    property BuildingSignature: TBuilding3DSignature
      read FBuildingSignature;
    property Depth: Integer read FDepth;
    property FaceCount: Integer read GetFaceCount;
    property Height: Integer read FHeight;
    property Mode: TBuilding3DViewMode read FMode;
    property PipelineSignature: String read FPipelineSignature;
    property Seed: TGraphSeed read FSeed;
    property Width: Integer read FWidth;
  end;

function Building3DViewModeName(const AMode: TBuilding3DViewMode): String;

function Building3DFootprintViewStyle(
  const ARole: TBuilding3DFootprintRole): TBuilding3DViewStyle;
function Building3DStructureViewStyle(
  const AKind: TBuilding3DStructureKind): TBuilding3DViewStyle;
function Building3DEnvelopeViewStyle(
  const AKind: TBuilding3DEnvelopeKind): TBuilding3DViewStyle;
function Building3DPropViewStyle(
  const AKind: TBuilding3DPropKind): TBuilding3DViewStyle;

//The returned immutable view is caller-owned. The overload without a mode
//builds the complete envelope-decorated structure plus prop presentation.
function BuildBuilding3DView(const ABuilding: TBuilding3D;
  const AMode: TBuilding3DViewMode): TBuilding3DView; overload;
function BuildBuilding3DView(
  const ABuilding: TBuilding3D): TBuilding3DView; overload;

implementation

uses
  wfc_voxel3d_mesh;

const
  VIEW_LAYER_FOOTPRINT = 'footprint';
  VIEW_LAYER_STRUCTURE = 'structure';
  VIEW_LAYER_ENVELOPE = 'envelope-roof';
  VIEW_LAYER_PROPS = 'props';

function ViewColor(const AR, AG, AB, AA: Byte): TVoxel3DColor;
begin
  Result := MakeVoxel3DColor(AR, AG, AB, AA);
end;

function ViewStyle(const AR, AG, AB, AA,
  AEdgeR, AEdgeG, AEdgeB, AEdgeA: Byte): TBuilding3DViewStyle;
begin
  Result.FillColor := ViewColor(AR, AG, AB, AA);
  Result.EdgeColor := ViewColor(AEdgeR, AEdgeG, AEdgeB, AEdgeA);
end;

function Building3DViewModeName(const AMode: TBuilding3DViewMode): String;
begin
  case AMode of
    b3vmFootprint: Result := VIEW_LAYER_FOOTPRINT;
    b3vmStructure: Result := VIEW_LAYER_STRUCTURE;
    b3vmEnvelopeRoof: Result := VIEW_LAYER_ENVELOPE;
    b3vmComplete: Result := 'complete';
  else
    raise ERangeError.CreateFmt(
      'Building 3D view mode is out of bounds [%d]', [Ord(AMode)]);
  end;
end;

function Building3DFootprintViewStyle(
  const ARole: TBuilding3DFootprintRole): TBuilding3DViewStyle;
begin
  case ARole of
    b3frVoid:
      Result := ViewStyle(0, 0, 0, 0, 0, 0, 0, 0);
    b3frGroundShell:
      Result := ViewStyle(116, 103, 88, 255, 42, 38, 34, 255);
    b3frShell:
      Result := ViewStyle(164, 156, 145, 255, 48, 44, 40, 255);
    b3frInterior:
      Result := ViewStyle(220, 214, 198, 192, 80, 74, 66, 255);
    b3frFeature:
      Result := ViewStyle(181, 112, 191, 255, 76, 36, 84, 255);
    b3frLintel:
      Result := ViewStyle(136, 119, 99, 255, 48, 40, 32, 255);
    b3frRoof:
      Result := ViewStyle(151, 78, 70, 255, 62, 29, 27, 255);
    b3frEntranceNorth, b3frEntranceEast,
    b3frEntranceSouth, b3frEntranceWest:
      Result := ViewStyle(91, 139, 161, 224, 30, 56, 68, 255);
  else
    raise ERangeError.CreateFmt(
      'Building 3D footprint role is out of bounds [%d]', [Ord(ARole)]);
  end;
end;

function Building3DStructureViewStyle(
  const AKind: TBuilding3DStructureKind): TBuilding3DViewStyle;
begin
  case AKind of
    b3skVoidAir, b3skInteriorAir, b3skFeatureAir:
      Result := ViewStyle(0, 0, 0, 0, 0, 0, 0, 0);
    b3skFoundation:
      Result := ViewStyle(103, 107, 111, 255, 36, 39, 42, 255);
    b3skWall:
      Result := ViewStyle(198, 181, 154, 255, 61, 51, 41, 255);
    b3skWindow:
      Result := ViewStyle(94, 166, 196, 208, 29, 67, 84, 255);
    b3skDoor:
      Result := ViewStyle(119, 79, 54, 255, 47, 29, 19, 255);
    b3skLintel:
      Result := ViewStyle(174, 151, 119, 255, 58, 45, 33, 255);
    b3skRoofSpan:
      Result := ViewStyle(135, 67, 62, 255, 57, 25, 23, 255);
  else
    raise ERangeError.CreateFmt(
      'Building 3D structure kind is out of bounds [%d]', [Ord(AKind)]);
  end;
end;

function Building3DEnvelopeViewStyle(
  const AKind: TBuilding3DEnvelopeKind): TBuilding3DViewStyle;
begin
  case AKind of
    b3ekNone:
      Result := ViewStyle(0, 0, 0, 0, 0, 0, 0, 0);
    b3ekFacade:
      Result := ViewStyle(213, 177, 125, 255, 73, 53, 34, 255);
    b3ekWindowTrim:
      Result := ViewStyle(65, 151, 192, 224, 21, 61, 82, 255);
    b3ekDoorTrim:
      Result := ViewStyle(133, 82, 48, 255, 49, 27, 14, 255);
    b3ekRoofFinish:
      Result := ViewStyle(168, 70, 65, 255, 67, 23, 22, 255);
  else
    raise ERangeError.CreateFmt(
      'Building 3D envelope kind is out of bounds [%d]', [Ord(AKind)]);
  end;
end;

function Building3DPropViewStyle(
  const AKind: TBuilding3DPropKind): TBuilding3DViewStyle;
begin
  case AKind of
    b3pkNone:
      Result := ViewStyle(0, 0, 0, 0, 0, 0, 0, 0);
    b3pkLamp:
      Result := ViewStyle(231, 190, 72, 255, 86, 63, 15, 255);
    b3pkPlant:
      Result := ViewStyle(82, 151, 79, 255, 27, 61, 27, 255);
  else
    raise ERangeError.CreateFmt(
      'Building 3D prop kind is out of bounds [%d]', [Ord(AKind)]);
  end;
end;

function CopyLineage(
  const ALineage: TBuilding3DViewLineage): TBuilding3DViewLineage;
begin
  Result.FootprintRole := ALineage.FootprintRole;
  Result.FootprintToken := ALineage.FootprintToken;
  Result.StructureKind := ALineage.StructureKind;
  Result.StructurePrototypeId := ALineage.StructurePrototypeId;
  Result.StructureMaterial := ALineage.StructureMaterial;
  Result.StructureRotation := ALineage.StructureRotation;
  Result.EnvelopeKind := ALineage.EnvelopeKind;
  Result.EnvelopePrototypeId := ALineage.EnvelopePrototypeId;
  Result.EnvelopeMaterial := ALineage.EnvelopeMaterial;
  Result.EnvelopeRotation := ALineage.EnvelopeRotation;
  Result.PropKind := ALineage.PropKind;
  Result.PropPrototypeId := ALineage.PropPrototypeId;
  Result.PropMaterial := ALineage.PropMaterial;
  Result.PropRotation := ALineage.PropRotation;
end;

function CopyViewQuad(const AQuad: TVoxel3DViewQuad): TVoxel3DViewQuad;
var
  I: Integer;
begin
  for I := Low(Result.Vertices) to High(Result.Vertices) do
    Result.Vertices[I] := AQuad.Vertices[I];
  Result.CellX := AQuad.CellX;
  Result.CellY := AQuad.CellY;
  Result.CellZ := AQuad.CellZ;
  Result.Direction := AQuad.Direction;
  Result.Rotation := AQuad.Rotation;
  Result.LayerOrder := AQuad.LayerOrder;
  Result.LayerId := AQuad.LayerId;
  Result.PrototypeId := AQuad.PrototypeId;
  Result.Material := AQuad.Material;
  Result.Semantic := AQuad.Semantic;
  Result.FillColor := AQuad.FillColor;
  Result.EdgeColor := AQuad.EdgeColor;
end;

function CopyViewFace(
  const AFace: TBuilding3DViewFace): TBuilding3DViewFace;
begin
  Result.Quad := CopyViewQuad(AFace.Quad);
  Result.GeometryKind := AFace.GeometryKind;
  Result.Lineage := CopyLineage(AFace.Lineage);
end;

procedure CheckBuildingDimensions(const ABuilding: TBuilding3D;
  out AWidth, AHeight, ADepth: Integer);
const
  MAX_VIEW_DIMENSION = High(Integer) div WFC_BUILDING3D_VIEW_SUBCELL_SCALE;
begin
  if ABuilding.Width > TGraphCoordinate(MAX_VIEW_DIMENSION) then
    raise ERangeError.Create('Building 3D view width exceeds fixed-subcell coordinates');
  if ABuilding.Height > TGraphCoordinate(MAX_VIEW_DIMENSION) then
    raise ERangeError.Create('Building 3D view height exceeds fixed-subcell coordinates');
  if ABuilding.Depth > TGraphCoordinate(MAX_VIEW_DIMENSION) then
    raise ERangeError.Create('Building 3D view depth exceeds fixed-subcell coordinates');
  AWidth := Integer(ABuilding.Width);
  AHeight := Integer(ABuilding.Height);
  ADepth := Integer(ABuilding.Depth);
end;

function ViewPoint(const AX, AY, AZ: Integer): TVoxel3DViewPoint3;
begin
  Result := MakeVoxel3DViewPoint3(AX, AY, AZ);
end;

procedure SetCellFaceVertices(var AVertices: TVoxel3DViewVertices;
  const AX, AY, AZ: Integer; const ADirection: TGraphDirection);
var
  LX0, LX1, LY0, LY1, LZ0, LZ1: Integer;
begin
  LX0 := AX * WFC_BUILDING3D_VIEW_SUBCELL_SCALE;
  LX1 := (AX + 1) * WFC_BUILDING3D_VIEW_SUBCELL_SCALE;
  LY0 := AY * WFC_BUILDING3D_VIEW_SUBCELL_SCALE;
  LY1 := (AY + 1) * WFC_BUILDING3D_VIEW_SUBCELL_SCALE;
  LZ0 := AZ * WFC_BUILDING3D_VIEW_SUBCELL_SCALE;
  LZ1 := (AZ + 1) * WFC_BUILDING3D_VIEW_SUBCELL_SCALE;
  case ADirection of
    gdNorth:
      begin
        AVertices[0] := ViewPoint(LX0, LY1, LZ0);
        AVertices[1] := ViewPoint(LX0, LY1, LZ1);
        AVertices[2] := ViewPoint(LX1, LY1, LZ1);
        AVertices[3] := ViewPoint(LX1, LY1, LZ0);
      end;
    gdEast:
      begin
        AVertices[0] := ViewPoint(LX1, LY0, LZ0);
        AVertices[1] := ViewPoint(LX1, LY1, LZ0);
        AVertices[2] := ViewPoint(LX1, LY1, LZ1);
        AVertices[3] := ViewPoint(LX1, LY0, LZ1);
      end;
    gdSouth:
      begin
        AVertices[0] := ViewPoint(LX0, LY0, LZ0);
        AVertices[1] := ViewPoint(LX1, LY0, LZ0);
        AVertices[2] := ViewPoint(LX1, LY0, LZ1);
        AVertices[3] := ViewPoint(LX0, LY0, LZ1);
      end;
    gdWest:
      begin
        AVertices[0] := ViewPoint(LX0, LY0, LZ0);
        AVertices[1] := ViewPoint(LX0, LY0, LZ1);
        AVertices[2] := ViewPoint(LX0, LY1, LZ1);
        AVertices[3] := ViewPoint(LX0, LY1, LZ0);
      end;
    gdUp:
      begin
        AVertices[0] := ViewPoint(LX0, LY0, LZ1);
        AVertices[1] := ViewPoint(LX1, LY0, LZ1);
        AVertices[2] := ViewPoint(LX1, LY1, LZ1);
        AVertices[3] := ViewPoint(LX0, LY1, LZ1);
      end;
    gdDown:
      begin
        AVertices[0] := ViewPoint(LX0, LY0, LZ0);
        AVertices[1] := ViewPoint(LX0, LY1, LZ0);
        AVertices[2] := ViewPoint(LX1, LY1, LZ0);
        AVertices[3] := ViewPoint(LX1, LY0, LZ0);
      end;
  else
    raise ERangeError.CreateFmt(
      'Building 3D view direction is out of bounds [%d]',
      [Ord(ADirection)]);
  end;
end;

function BuildLineage(const ABuilding: TBuilding3D;
  const AX, AY, AZ: Integer): TBuilding3DViewLineage;
var
  LEnvelope: TVoxel3DVariant;
  LProp: TVoxel3DVariant;
  LStructure: TVoxel3DVariant;
begin
  Result.FootprintRole := ABuilding.FootprintRoleAt(AX, AY, AZ);
  Result.FootprintToken := Building3DFootprintRoleToken(
    Result.FootprintRole);

  Result.StructureKind := ABuilding.StructureKindAt(AX, AY, AZ);
  LStructure := ABuilding.StageVariantAt(b3sStructure, AX, AY, AZ);
  Result.StructurePrototypeId := LStructure.PrototypeId;
  Result.StructureMaterial := LStructure.Material;
  Result.StructureRotation := LStructure.Rotation;

  Result.EnvelopeKind := ABuilding.EnvelopeKindAt(AX, AY, AZ);
  LEnvelope := ABuilding.StageVariantAt(b3sEnvelopeRoof, AX, AY, AZ);
  Result.EnvelopePrototypeId := LEnvelope.PrototypeId;
  Result.EnvelopeMaterial := LEnvelope.Material;
  Result.EnvelopeRotation := LEnvelope.Rotation;

  Result.PropKind := ABuilding.PropKindAt(AX, AY, AZ);
  LProp := ABuilding.StageVariantAt(b3sProps, AX, AY, AZ);
  Result.PropPrototypeId := LProp.PrototypeId;
  Result.PropMaterial := LProp.Material;
  Result.PropRotation := LProp.Rotation;
end;

procedure InitializeQuad(var AQuad: TVoxel3DViewQuad;
  const AX, AY, AZ: Integer; const ADirection: TGraphDirection;
  const ARotation: TVoxel3DRotation; const ALayerOrder: Integer;
  const ALayerId, APrototypeId, AMaterial, ASemantic: String;
  const AStyle: TBuilding3DViewStyle);
begin
  if (not IsVoxel3DToken(ALayerId)) or
      (not IsVoxel3DToken(APrototypeId)) or
      (not IsVoxel3DToken(AMaterial)) or
      (not IsVoxel3DToken(ASemantic)) then
    raise EBuilding3DView.Create(
      'Building 3D view metadata must use portable public tokens');
  AQuad.CellX := AX;
  AQuad.CellY := AY;
  AQuad.CellZ := AZ;
  AQuad.Direction := ADirection;
  AQuad.Rotation := ARotation;
  AQuad.LayerOrder := ALayerOrder;
  AQuad.LayerId := ALayerId;
  AQuad.PrototypeId := APrototypeId;
  AQuad.Material := AMaterial;
  AQuad.Semantic := ASemantic;
  AQuad.FillColor := AStyle.FillColor;
  AQuad.EdgeColor := AStyle.EdgeColor;
end;

procedure AppendFace(var AFaces: TBuilding3DViewFaces;
  const AFace: TBuilding3DViewFace);
var
  LCount: Integer;
begin
  LCount := Length(AFaces);
  if LCount = High(Integer) then
    raise ERangeError.Create('Building 3D view face count exceeds Integer');
  SetLength(AFaces, LCount + 1);
  AFaces[LCount] := CopyViewFace(AFace);
end;

function ResolveNeighbor(const AX, AY, AZ, AWidth, AHeight,
  ADepth: Integer; const AWrap: Boolean;
  const ADirection: TGraphDirection;
  out ANeighborX, ANeighborY, ANeighborZ: Integer): Boolean;
begin
  ANeighborX := AX;
  ANeighborY := AY;
  ANeighborZ := AZ;
  case ADirection of
    gdNorth:
      if ANeighborY < AHeight - 1 then Inc(ANeighborY)
      else if AWrap then ANeighborY := 0 else Exit(False);
    gdEast:
      if ANeighborX < AWidth - 1 then Inc(ANeighborX)
      else if AWrap then ANeighborX := 0 else Exit(False);
    gdSouth:
      if ANeighborY > 0 then Dec(ANeighborY)
      else if AWrap then ANeighborY := AHeight - 1 else Exit(False);
    gdWest:
      if ANeighborX > 0 then Dec(ANeighborX)
      else if AWrap then ANeighborX := AWidth - 1 else Exit(False);
    gdUp:
      if ANeighborZ < ADepth - 1 then Inc(ANeighborZ)
      else if AWrap then ANeighborZ := 0 else Exit(False);
    gdDown:
      if ANeighborZ > 0 then Dec(ANeighborZ)
      else if AWrap then ANeighborZ := ADepth - 1 else Exit(False);
  else
    raise ERangeError.CreateFmt(
      'Building 3D view direction is out of bounds [%d]',
      [Ord(ADirection)]);
  end;
  Result := True;
end;

function FootprintFaceExposed(const ABuilding: TBuilding3D;
  const AX, AY, AZ, AWidth, AHeight, ADepth: Integer;
  const ADirection: TGraphDirection): Boolean;
var
  LX, LY, LZ: Integer;
begin
  if not ResolveNeighbor(AX, AY, AZ, AWidth, AHeight, ADepth,
      ABuilding.WrapNeighbors, ADirection, LX, LY, LZ) then
    Exit(True);
  Result := ABuilding.FootprintRoleAt(LX, LY, LZ) = b3frVoid;
end;

procedure AppendFootprintFaces(const ABuilding: TBuilding3D;
  const AWidth, AHeight, ADepth: Integer;
  var AFaces: TBuilding3DViewFaces);
var
  LDirection: TGraphDirection;
  LFace: TBuilding3DViewFace;
  LRole: TBuilding3DFootprintRole;
  LStyle: TBuilding3DViewStyle;
  LToken: String;
  X, Y, Z: Integer;
begin
  for Z := 0 to ADepth - 1 do
    for Y := 0 to AHeight - 1 do
      for X := 0 to AWidth - 1 do
      begin
        LRole := ABuilding.FootprintRoleAt(X, Y, Z);
        if LRole = b3frVoid then
          Continue;
        LToken := Building3DFootprintRoleToken(LRole);
        LStyle := Building3DFootprintViewStyle(LRole);
        for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
          if FootprintFaceExposed(ABuilding, X, Y, Z,
              AWidth, AHeight, ADepth, LDirection) then
          begin
            LFace := Default(TBuilding3DViewFace);
            LFace.GeometryKind := b3vgFootprint;
            LFace.Lineage := BuildLineage(ABuilding, X, Y, Z);
            SetCellFaceVertices(LFace.Quad.Vertices, X, Y, Z,
              LDirection);
            InitializeQuad(LFace.Quad, X, Y, Z, LDirection, v3r0,
              Ord(b3sFootprint), VIEW_LAYER_FOOTPRINT, LToken,
              LToken, LToken, LStyle);
            AppendFace(AFaces, LFace);
          end;
      end;
end;

procedure ScaleMeshVertices(const AQuad: TVoxel3DMeshQuad;
  var AVertices: TVoxel3DViewVertices);
var
  I: Integer;
begin
  for I := Low(AVertices) to High(AVertices) do
    AVertices[I] := ViewPoint(
      AQuad.Vertices[I].X * WFC_BUILDING3D_VIEW_SUBCELL_SCALE,
      AQuad.Vertices[I].Y * WFC_BUILDING3D_VIEW_SUBCELL_SCALE,
      AQuad.Vertices[I].Z * WFC_BUILDING3D_VIEW_SUBCELL_SCALE);
end;

procedure AppendMeshFaces(const ABuilding: TBuilding3D;
  const AMesh: TVoxel3DMesh;
  const AGeometryKind: TBuilding3DViewGeometryKind;
  const ADecorateEnvelope: Boolean;
  var AFaces: TBuilding3DViewFaces);
var
  I: Integer;
  LFace: TBuilding3DViewFace;
  LLayerId: String;
  LLayerOrder: Integer;
  LMaterial: String;
  LMeshQuad: TVoxel3DMeshQuad;
  LPrototypeId: String;
  LSemantic: String;
  LStyle: TBuilding3DViewStyle;
begin
  for I := 0 to AMesh.QuadCount - 1 do
  begin
    LMeshQuad := AMesh.QuadAt(I);
    LFace := Default(TBuilding3DViewFace);
    LFace.GeometryKind := AGeometryKind;
    LFace.Lineage := BuildLineage(ABuilding, LMeshQuad.CellX,
      LMeshQuad.CellY, LMeshQuad.CellZ);
    ScaleMeshVertices(LMeshQuad, LFace.Quad.Vertices);

    case AGeometryKind of
      b3vgStructure:
        if ADecorateEnvelope and
            (LFace.Lineage.EnvelopeKind <> b3ekNone) then
        begin
          LLayerId := VIEW_LAYER_ENVELOPE;
          LLayerOrder := Ord(b3sEnvelopeRoof);
          LPrototypeId := LFace.Lineage.StructurePrototypeId;
          LMaterial := LFace.Lineage.EnvelopeMaterial;
          LSemantic := LFace.Lineage.EnvelopePrototypeId;
          LStyle := Building3DEnvelopeViewStyle(
            LFace.Lineage.EnvelopeKind);
        end
        else
        begin
          LLayerId := VIEW_LAYER_STRUCTURE;
          LLayerOrder := Ord(b3sStructure);
          LPrototypeId := LFace.Lineage.StructurePrototypeId;
          LMaterial := LFace.Lineage.StructureMaterial;
          LSemantic := LFace.Lineage.StructurePrototypeId;
          LStyle := Building3DStructureViewStyle(
            LFace.Lineage.StructureKind);
        end;
      b3vgProp:
        begin
          LLayerId := VIEW_LAYER_PROPS;
          LLayerOrder := Ord(b3sProps);
          LPrototypeId := LFace.Lineage.PropPrototypeId;
          LMaterial := LFace.Lineage.PropMaterial;
          LSemantic := LFace.Lineage.PropPrototypeId;
          LStyle := Building3DPropViewStyle(LFace.Lineage.PropKind);
        end;
    else
      raise EBuilding3DView.Create(
        'footprint geometry cannot be appended from a voxel mesh');
    end;

    InitializeQuad(LFace.Quad, LMeshQuad.CellX, LMeshQuad.CellY,
      LMeshQuad.CellZ, LMeshQuad.Direction, LMeshQuad.Rotation,
      LLayerOrder, LLayerId, LPrototypeId, LMaterial, LSemantic,
      LStyle);
    AppendFace(AFaces, LFace);
  end;
end;

{ TBuilding3DView }

function TBuilding3DView.GetFaceCount: Integer;
begin
  Result := Length(FFaces);
end;

constructor TBuilding3DView.Create(const ABuilding: TBuilding3D;
  const AMode: TBuilding3DViewMode);
var
  LPropMesh: TVoxel3DMesh;
  LPropScene: TVoxel3DScene;
  LStructureMesh: TVoxel3DMesh;
  LStructureScene: TVoxel3DScene;
begin
  inherited Create;
  FFaces := nil;
  LPropMesh := nil;
  LPropScene := nil;
  LStructureMesh := nil;
  LStructureScene := nil;
  if not Assigned(ABuilding) then
    raise EArgumentNilException.Create(
      'Building 3D view requires a building');
  Building3DViewModeName(AMode);
  if not ABuilding.HasSolution then
    raise EBuilding3DView.Create(
      'Building 3D view requires a solved building');
  if not ABuilding.DefinitionMatchesPipeline then
    raise EBuilding3DView.Create(
      'Building 3D view requires the canonical pipeline definition');
  CheckBuildingDimensions(ABuilding, FWidth, FHeight, FDepth);
  FMode := AMode;
  FSeed := ABuilding.Seed;
  FBuildingSignature := ABuilding.Signature;
  FPipelineSignature := ABuilding.PipelineSignature;

  try
    case FMode of
      b3vmFootprint:
        AppendFootprintFaces(ABuilding, FWidth, FHeight, FDepth,
          FFaces);
      b3vmStructure, b3vmEnvelopeRoof, b3vmComplete:
        begin
          LStructureScene := ABuilding.CaptureStructureScene;
          LStructureMesh := BuildVoxel3DMesh(ABuilding.StructureKit,
            LStructureScene);
          AppendMeshFaces(ABuilding, LStructureMesh, b3vgStructure,
            FMode <> b3vmStructure, FFaces);
          if FMode = b3vmComplete then
          begin
            LPropScene := ABuilding.CapturePropsScene;
            LPropMesh := BuildVoxel3DMesh(ABuilding.PropsKit,
              LPropScene);
            AppendMeshFaces(ABuilding, LPropMesh, b3vgProp, False,
              FFaces);
          end;
        end;
    end;
  finally
    LPropMesh.Free;
    LPropScene.Free;
    LStructureMesh.Free;
    LStructureScene.Free;
  end;
end;

function TBuilding3DView.FaceAt(
  const AIndex: Integer): TBuilding3DViewFace;
begin
  if (AIndex < 0) or (AIndex >= Length(FFaces)) then
    raise ERangeError.CreateFmt(
      'Building 3D view face index is out of bounds [%d]', [AIndex]);
  Result := CopyViewFace(FFaces[AIndex]);
end;

function TBuilding3DView.CopyFaces: TBuilding3DViewFaces;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FFaces));
  for I := 0 to High(FFaces) do
    Result[I] := CopyViewFace(FFaces[I]);
end;

function TBuilding3DView.CopyQuads: TVoxel3DViewQuads;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FFaces));
  for I := 0 to High(FFaces) do
    Result[I] := CopyViewQuad(FFaces[I].Quad);
end;

function BuildBuilding3DView(const ABuilding: TBuilding3D;
  const AMode: TBuilding3DViewMode): TBuilding3DView;
begin
  Result := TBuilding3DView.Create(ABuilding, AMode);
end;

function BuildBuilding3DView(
  const ABuilding: TBuilding3D): TBuilding3DView;
begin
  Result := BuildBuilding3DView(ABuilding, b3vmComplete);
end;

end.
