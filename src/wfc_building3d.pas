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
unit wfc_building3d;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_voxel3d,
  wfc_voxel3d_passes;

const
  WFC_BUILDING3D_MODEL_VERSION = 1;
  WFC_BUILDING3D_BLUEPRINT_VERSION = 1;
  WFC_BUILDING3D_SIGNATURE_VERSION = 1;

  WFC_BUILDING3D_PASS_FOOTPRINT = 'footprint';
  WFC_BUILDING3D_PASS_STRUCTURE = 'structure';
  WFC_BUILDING3D_PASS_ENVELOPE = 'envelope-roof';
  WFC_BUILDING3D_PASS_PROPS = 'props';

  WFC_BUILDING3D_ROLE_VOID = 'void';
  WFC_BUILDING3D_ROLE_GROUND = 'ground-shell';
  WFC_BUILDING3D_ROLE_SHELL = 'shell';
  WFC_BUILDING3D_ROLE_INTERIOR = 'interior';
  WFC_BUILDING3D_ROLE_FEATURE = 'feature';
  WFC_BUILDING3D_ROLE_LINTEL = 'lintel';
  WFC_BUILDING3D_ROLE_ROOF = 'roof';
  WFC_BUILDING3D_ROLE_ENTRANCE_NORTH = 'entrance-north';
  WFC_BUILDING3D_ROLE_ENTRANCE_EAST = 'entrance-east';
  WFC_BUILDING3D_ROLE_ENTRANCE_SOUTH = 'entrance-south';
  WFC_BUILDING3D_ROLE_ENTRANCE_WEST = 'entrance-west';

  WFC_BUILDING3D_STRUCTURE_VOID = 'void-air';
  WFC_BUILDING3D_STRUCTURE_INTERIOR = 'interior-air';
  WFC_BUILDING3D_STRUCTURE_FEATURE = 'feature-air';
  WFC_BUILDING3D_STRUCTURE_FOUNDATION = 'foundation';
  WFC_BUILDING3D_STRUCTURE_WALL = 'wall';
  WFC_BUILDING3D_STRUCTURE_WINDOW = 'window';
  WFC_BUILDING3D_STRUCTURE_DOOR = 'door';
  WFC_BUILDING3D_STRUCTURE_LINTEL = 'lintel';
  WFC_BUILDING3D_STRUCTURE_ROOF = 'roof-span';

  WFC_BUILDING3D_ENVELOPE_NONE = 'none';
  WFC_BUILDING3D_ENVELOPE_FACADE = 'facade';
  WFC_BUILDING3D_ENVELOPE_WINDOW = 'window-trim';
  WFC_BUILDING3D_ENVELOPE_DOOR = 'door-trim';
  WFC_BUILDING3D_ENVELOPE_ROOF = 'roof-finish';

  WFC_BUILDING3D_PROP_NONE = 'none';
  WFC_BUILDING3D_PROP_LAMP = 'lamp';
  WFC_BUILDING3D_PROP_PLANT = 'plant';

type
  EBuilding3D = class(Exception);
  EBuilding3DBlueprint = class(EBuilding3D);
  EBuilding3DModel = class(EBuilding3D);
  EBuilding3DScene = class(EBuilding3D);

  TBuilding3DSignature = Cardinal;

  TBuilding3DStage = (
    b3sFootprint,
    b3sStructure,
    b3sEnvelopeRoof,
    b3sProps
  );

  TBuilding3DFootprintRole = (
    b3frVoid,
    b3frGroundShell,
    b3frShell,
    b3frInterior,
    b3frFeature,
    b3frLintel,
    b3frRoof,
    b3frEntranceNorth,
    b3frEntranceEast,
    b3frEntranceSouth,
    b3frEntranceWest
  );
  TBuilding3DFootprintRoles = array of TBuilding3DFootprintRole;

  TBuilding3DStructureKind = (
    b3skVoidAir,
    b3skInteriorAir,
    b3skFeatureAir,
    b3skFoundation,
    b3skWall,
    b3skWindow,
    b3skDoor,
    b3skLintel,
    b3skRoofSpan
  );

  TBuilding3DEnvelopeKind = (
    b3ekNone,
    b3ekFacade,
    b3ekWindowTrim,
    b3ekDoorTrim,
    b3ekRoofFinish
  );

  TBuilding3DPropKind = (
    b3pkNone,
    b3pkLamp,
    b3pkPlant
  );

  TBuilding3DConfig = record
    Seed: TGraphSeed;
    WrapNeighbors: Boolean;
  end;

  { TBuilding3DBlueprint }

  //A blueprint is an engine-neutral 3D massing field. It defaults to void and
  //is copied into the footprint pass as one exact caller domain per cell.
  TBuilding3DBlueprint = class
  private
    FWidth: TGraphCoordinate;
    FHeight: TGraphCoordinate;
    FDepth: TGraphCoordinate;
    FRoles: TBuilding3DFootprintRoles;
    function CoordToIndex(const AX, AY,
      AZ: TGraphCoordinate): Integer;
    function GetCellCount: Integer;
    function GetRole(const AX, AY,
      AZ: TGraphCoordinate): TBuilding3DFootprintRole;
  public
    constructor Create(const AWidth, AHeight,
      ADepth: TGraphCoordinate);
    function Fill(const ARole: TBuilding3DFootprintRole): TBuilding3DBlueprint;
    function SetRole(const AX, AY, AZ: TGraphCoordinate;
      const ARole: TBuilding3DFootprintRole): TBuilding3DBlueprint;
    function CopyRoles: TBuilding3DFootprintRoles;

    property CellCount: Integer read GetCellCount;
    property Width: TGraphCoordinate read FWidth;
    property Height: TGraphCoordinate read FHeight;
    property Depth: TGraphCoordinate read FDepth;
    property Role[const AX, AY, AZ: TGraphCoordinate]:
      TBuilding3DFootprintRole read GetRole; default;
  end;

  { TBuilding3D }

  //The four passes share one TGraph transaction. Voxel graph keys never
  //escape this adapter: callers see footprint roles, prototype kinds, and
  //rotations, while renderers may capture independent voxel scenes.
  TBuilding3D = class
  private
    FGraph: TGraph;
    FStructureKit: TVoxel3DKit;
    FEnvelopeKit: TVoxel3DKit;
    FPropsKit: TVoxel3DKit;
    FStructureAdapter: TVoxel3DGraphAdapter;
    FEnvelopeAdapter: TVoxel3DGraphAdapter;
    FPropsAdapter: TVoxel3DGraphAdapter;
    FHasSolution: Boolean;
    FHasCommittedSolution: Boolean;
    FFootprintDirty: Boolean;

    procedure Initialize(const AWidth, AHeight,
      ADepth: TGraphCoordinate; const AConfig: TBuilding3DConfig);
    procedure ConfigureFootprint;
    procedure ConfigureStructureDependencies;
    procedure ConfigureEnvelopeDependencies;
    procedure ConfigurePropsDependencies;
    procedure MarkFootprintDirty;
    procedure RequireCanonicalPipeline(const AOperation: String);
    procedure RequireSolution(const AOperation: String);
    function AdapterForStage(
      const AStage: TBuilding3DStage): TVoxel3DGraphAdapter;
    function GetDepth: TGraphCoordinate;
    function GetHeight: TGraphCoordinate;
    function GetSeed: TGraphSeed;
    function GetWidth: TGraphCoordinate;
    function GetWrapNeighbors: Boolean;
    procedure SetSeed(const AValue: TGraphSeed);
  public
    constructor Create(const AWidth, AHeight,
      ADepth: TGraphCoordinate); overload;
    constructor Create(const AWidth, AHeight, ADepth: TGraphCoordinate;
      const AConfig: TBuilding3DConfig); overload;
    constructor Create(const ABlueprint: TBuilding3DBlueprint); overload;
    constructor Create(const ABlueprint: TBuilding3DBlueprint;
      const AConfig: TBuilding3DConfig); overload;
    destructor Destroy; override;

    function ApplyBlueprint(
      const ABlueprint: TBuilding3DBlueprint): TBuilding3D;
    function SetFootprintRole(const AX, AY, AZ: TGraphCoordinate;
      const ARole: TBuilding3DFootprintRole): TBuilding3D;
    function AllowFootprintRoles(const AX, AY, AZ: TGraphCoordinate;
      const ARoles: TBuilding3DFootprintRoles): TBuilding3D;
    function ClearFootprintDomain(const AX, AY,
      AZ: TGraphCoordinate): TBuilding3D;

    function TryGenerate(const AOptions: TGraphSolveOptions;
      out AReport: TGraphSolveReport): Boolean; overload;
    function TryGenerate(out AReport: TGraphSolveReport): Boolean; overload;
    function TryRegenerateFrom(const AStage: TBuilding3DStage;
      const AOptions: TGraphSolveOptions;
      out AReport: TGraphSolveReport): Boolean; overload;
    function TryRegenerateFrom(const AStage: TBuilding3DStage;
      out AReport: TGraphSolveReport): Boolean; overload;

    function StageGraph(const AStage: TBuilding3DStage): TGraph;
    function StageTokenAt(const AStage: TBuilding3DStage;
      const AX, AY, AZ: TGraphCoordinate): String;
    function StageRotationAt(const AStage: TBuilding3DStage;
      const AX, AY, AZ: TGraphCoordinate): TVoxel3DRotation;
    function StageVariantAt(const AStage: TBuilding3DStage;
      const AX, AY, AZ: TGraphCoordinate): TVoxel3DVariant;
    function FootprintRoleAt(const AX, AY,
      AZ: TGraphCoordinate): TBuilding3DFootprintRole;
    function StructureKindAt(const AX, AY,
      AZ: TGraphCoordinate): TBuilding3DStructureKind;
    function EnvelopeKindAt(const AX, AY,
      AZ: TGraphCoordinate): TBuilding3DEnvelopeKind;
    function PropKindAt(const AX, AY,
      AZ: TGraphCoordinate): TBuilding3DPropKind;

    function CaptureStructureScene: TVoxel3DScene;
    function CaptureEnvelopeScene: TVoxel3DScene;
    function CapturePropsScene: TVoxel3DScene;
    function DefinitionMatchesPipeline: Boolean;
    function Signature: TBuilding3DSignature;
    function PipelineSignature: String;

    property Graph: TGraph read FGraph;
    property StructureKit: TVoxel3DKit read FStructureKit;
    property EnvelopeKit: TVoxel3DKit read FEnvelopeKit;
    property PropsKit: TVoxel3DKit read FPropsKit;
    property HasSolution: Boolean read FHasSolution;
    property Width: TGraphCoordinate read GetWidth;
    property Height: TGraphCoordinate read GetHeight;
    property Depth: TGraphCoordinate read GetDepth;
    property Seed: TGraphSeed read GetSeed write SetSeed;
    property WrapNeighbors: Boolean read GetWrapNeighbors;
  end;

function DefaultBuilding3DConfig: TBuilding3DConfig;
function Building3DStageName(const AStage: TBuilding3DStage): String;
function Building3DFootprintRoleToken(
  const ARole: TBuilding3DFootprintRole): String;
function TryBuilding3DFootprintRole(const AToken: String;
  out ARole: TBuilding3DFootprintRole): Boolean;
function Building3DStructureKindToken(
  const AKind: TBuilding3DStructureKind): String;
function TryBuilding3DStructureKind(const AToken: String;
  out AKind: TBuilding3DStructureKind): Boolean;
function Building3DEnvelopeKindToken(
  const AKind: TBuilding3DEnvelopeKind): String;
function TryBuilding3DEnvelopeKind(const AToken: String;
  out AKind: TBuilding3DEnvelopeKind): Boolean;
function Building3DPropKindToken(
  const AKind: TBuilding3DPropKind): String;
function TryBuilding3DPropKind(const AToken: String;
  out AKind: TBuilding3DPropKind): Boolean;
function Building3DSignatureHex(
  const ASignature: TBuilding3DSignature): String;

implementation

const
  UNIVERSAL_SOCKET = 'building-any';
  STRUCTURE_KIT_ID = 'wfc-building-structure-v1';
  ENVELOPE_KIT_ID = 'wfc-building-envelope-v1';
  PROPS_KIT_ID = 'wfc-building-props-v1';
  HEX_DIGITS = '0123456789ABCDEF';

function CheckedCellCount(const AWidth, AHeight,
  ADepth: TGraphCoordinate; const AOperation: String): Integer;
var
  LWidth, LHeight, LDepth, LPlane: Integer;
begin
  if (AWidth = 0) or (AHeight = 0) or (ADepth = 0) then
    raise ERangeError.Create(AOperation + ' dimensions must be positive');
  if (AWidth > TGraphCoordinate(High(Integer))) or
      (AHeight > TGraphCoordinate(High(Integer))) or
      (ADepth > TGraphCoordinate(High(Integer))) then
    raise ERangeError.Create(AOperation + ' dimensions exceed Integer');
  LWidth := Integer(AWidth);
  LHeight := Integer(AHeight);
  LDepth := Integer(ADepth);
  if LWidth > High(Integer) div LHeight then
    raise ERangeError.Create(AOperation + ' plane size exceeds Integer');
  LPlane := LWidth * LHeight;
  if LPlane > High(Integer) div LDepth then
    raise ERangeError.Create(AOperation + ' cell count exceeds Integer');
  Result := LPlane * LDepth;
end;

function GraphValuesOf(const AValues: array of TGraphValue): TGraphValues;
var
  I: Integer;
begin
  Result := Default(TGraphValues);
  SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do
    Result[I] := AValues[I];
end;

function PassSelectorsOf(
  const ASelectors: array of TVoxel3DPassVariantSelector):
  TVoxel3DPassVariantSelectors;
var
  I: Integer;
begin
  Result := Default(TVoxel3DPassVariantSelectors);
  SetLength(Result, Length(ASelectors));
  for I := 0 to High(ASelectors) do
    Result[I] := ASelectors[I];
end;

function PassSelector(const APrototypeId: String):
  TVoxel3DPassVariantSelector;
begin
  Result := MakeVoxel3DPassVariantSelector(APrototypeId,
    ALL_VOXEL3D_ROTATIONS);
end;

function DefaultBuilding3DConfig: TBuilding3DConfig;
begin
  Result.Seed := 0;
  Result.WrapNeighbors := False;
end;

function Building3DStageName(const AStage: TBuilding3DStage): String;
begin
  case AStage of
    b3sFootprint: Result := WFC_BUILDING3D_PASS_FOOTPRINT;
    b3sStructure: Result := WFC_BUILDING3D_PASS_STRUCTURE;
    b3sEnvelopeRoof: Result := WFC_BUILDING3D_PASS_ENVELOPE;
    b3sProps: Result := WFC_BUILDING3D_PASS_PROPS;
  else
    raise ERangeError.CreateFmt('building stage is out of bounds [%d]',
      [Ord(AStage)]);
  end;
end;

function Building3DFootprintRoleToken(
  const ARole: TBuilding3DFootprintRole): String;
begin
  case ARole of
    b3frVoid: Result := WFC_BUILDING3D_ROLE_VOID;
    b3frGroundShell: Result := WFC_BUILDING3D_ROLE_GROUND;
    b3frShell: Result := WFC_BUILDING3D_ROLE_SHELL;
    b3frInterior: Result := WFC_BUILDING3D_ROLE_INTERIOR;
    b3frFeature: Result := WFC_BUILDING3D_ROLE_FEATURE;
    b3frLintel: Result := WFC_BUILDING3D_ROLE_LINTEL;
    b3frRoof: Result := WFC_BUILDING3D_ROLE_ROOF;
    b3frEntranceNorth: Result := WFC_BUILDING3D_ROLE_ENTRANCE_NORTH;
    b3frEntranceEast: Result := WFC_BUILDING3D_ROLE_ENTRANCE_EAST;
    b3frEntranceSouth: Result := WFC_BUILDING3D_ROLE_ENTRANCE_SOUTH;
    b3frEntranceWest: Result := WFC_BUILDING3D_ROLE_ENTRANCE_WEST;
  else
    raise ERangeError.CreateFmt(
      'building footprint role is out of bounds [%d]', [Ord(ARole)]);
  end;
end;

function TryBuilding3DFootprintRole(const AToken: String;
  out ARole: TBuilding3DFootprintRole): Boolean;
begin
  Result := True;
  if AToken = WFC_BUILDING3D_ROLE_VOID then ARole := b3frVoid
  else if AToken = WFC_BUILDING3D_ROLE_GROUND then ARole := b3frGroundShell
  else if AToken = WFC_BUILDING3D_ROLE_SHELL then ARole := b3frShell
  else if AToken = WFC_BUILDING3D_ROLE_INTERIOR then ARole := b3frInterior
  else if AToken = WFC_BUILDING3D_ROLE_FEATURE then ARole := b3frFeature
  else if AToken = WFC_BUILDING3D_ROLE_LINTEL then ARole := b3frLintel
  else if AToken = WFC_BUILDING3D_ROLE_ROOF then ARole := b3frRoof
  else if AToken = WFC_BUILDING3D_ROLE_ENTRANCE_NORTH then
    ARole := b3frEntranceNorth
  else if AToken = WFC_BUILDING3D_ROLE_ENTRANCE_EAST then
    ARole := b3frEntranceEast
  else if AToken = WFC_BUILDING3D_ROLE_ENTRANCE_SOUTH then
    ARole := b3frEntranceSouth
  else if AToken = WFC_BUILDING3D_ROLE_ENTRANCE_WEST then
    ARole := b3frEntranceWest
  else
  begin
    ARole := b3frVoid;
    Result := False;
  end;
end;

function Building3DStructureKindToken(
  const AKind: TBuilding3DStructureKind): String;
begin
  case AKind of
    b3skVoidAir: Result := WFC_BUILDING3D_STRUCTURE_VOID;
    b3skInteriorAir: Result := WFC_BUILDING3D_STRUCTURE_INTERIOR;
    b3skFeatureAir: Result := WFC_BUILDING3D_STRUCTURE_FEATURE;
    b3skFoundation: Result := WFC_BUILDING3D_STRUCTURE_FOUNDATION;
    b3skWall: Result := WFC_BUILDING3D_STRUCTURE_WALL;
    b3skWindow: Result := WFC_BUILDING3D_STRUCTURE_WINDOW;
    b3skDoor: Result := WFC_BUILDING3D_STRUCTURE_DOOR;
    b3skLintel: Result := WFC_BUILDING3D_STRUCTURE_LINTEL;
    b3skRoofSpan: Result := WFC_BUILDING3D_STRUCTURE_ROOF;
  else
    raise ERangeError.CreateFmt(
      'building structure kind is out of bounds [%d]', [Ord(AKind)]);
  end;
end;

function TryBuilding3DStructureKind(const AToken: String;
  out AKind: TBuilding3DStructureKind): Boolean;
begin
  Result := True;
  if AToken = WFC_BUILDING3D_STRUCTURE_VOID then AKind := b3skVoidAir
  else if AToken = WFC_BUILDING3D_STRUCTURE_INTERIOR then
    AKind := b3skInteriorAir
  else if AToken = WFC_BUILDING3D_STRUCTURE_FEATURE then
    AKind := b3skFeatureAir
  else if AToken = WFC_BUILDING3D_STRUCTURE_FOUNDATION then
    AKind := b3skFoundation
  else if AToken = WFC_BUILDING3D_STRUCTURE_WALL then AKind := b3skWall
  else if AToken = WFC_BUILDING3D_STRUCTURE_WINDOW then AKind := b3skWindow
  else if AToken = WFC_BUILDING3D_STRUCTURE_DOOR then AKind := b3skDoor
  else if AToken = WFC_BUILDING3D_STRUCTURE_LINTEL then AKind := b3skLintel
  else if AToken = WFC_BUILDING3D_STRUCTURE_ROOF then AKind := b3skRoofSpan
  else
  begin
    AKind := b3skVoidAir;
    Result := False;
  end;
end;

function Building3DEnvelopeKindToken(
  const AKind: TBuilding3DEnvelopeKind): String;
begin
  case AKind of
    b3ekNone: Result := WFC_BUILDING3D_ENVELOPE_NONE;
    b3ekFacade: Result := WFC_BUILDING3D_ENVELOPE_FACADE;
    b3ekWindowTrim: Result := WFC_BUILDING3D_ENVELOPE_WINDOW;
    b3ekDoorTrim: Result := WFC_BUILDING3D_ENVELOPE_DOOR;
    b3ekRoofFinish: Result := WFC_BUILDING3D_ENVELOPE_ROOF;
  else
    raise ERangeError.CreateFmt(
      'building envelope kind is out of bounds [%d]', [Ord(AKind)]);
  end;
end;

function TryBuilding3DEnvelopeKind(const AToken: String;
  out AKind: TBuilding3DEnvelopeKind): Boolean;
begin
  Result := True;
  if AToken = WFC_BUILDING3D_ENVELOPE_NONE then AKind := b3ekNone
  else if AToken = WFC_BUILDING3D_ENVELOPE_FACADE then AKind := b3ekFacade
  else if AToken = WFC_BUILDING3D_ENVELOPE_WINDOW then AKind := b3ekWindowTrim
  else if AToken = WFC_BUILDING3D_ENVELOPE_DOOR then AKind := b3ekDoorTrim
  else if AToken = WFC_BUILDING3D_ENVELOPE_ROOF then AKind := b3ekRoofFinish
  else
  begin
    AKind := b3ekNone;
    Result := False;
  end;
end;

function Building3DPropKindToken(const AKind: TBuilding3DPropKind): String;
begin
  case AKind of
    b3pkNone: Result := WFC_BUILDING3D_PROP_NONE;
    b3pkLamp: Result := WFC_BUILDING3D_PROP_LAMP;
    b3pkPlant: Result := WFC_BUILDING3D_PROP_PLANT;
  else
    raise ERangeError.CreateFmt('building prop kind is out of bounds [%d]',
      [Ord(AKind)]);
  end;
end;

function TryBuilding3DPropKind(const AToken: String;
  out AKind: TBuilding3DPropKind): Boolean;
begin
  Result := True;
  if AToken = WFC_BUILDING3D_PROP_NONE then AKind := b3pkNone
  else if AToken = WFC_BUILDING3D_PROP_LAMP then AKind := b3pkLamp
  else if AToken = WFC_BUILDING3D_PROP_PLANT then AKind := b3pkPlant
  else
  begin
    AKind := b3pkNone;
    Result := False;
  end;
end;

function Building3DSignatureHex(
  const ASignature: TBuilding3DSignature): String;
var
  I: Integer;
  LValue: Cardinal;
begin
  SetLength(Result, 8);
  LValue := ASignature;
  for I := 8 downto 1 do
  begin
    Result[I] := HEX_DIGITS[Integer(LValue and Cardinal($F)) + 1];
    LValue := LValue shr 4;
  end;
end;

function NewStructureKit: TVoxel3DKit;
var
  P: TVoxel3DPrototypes;
  Pairs: TVoxel3DSocketPairs;
  S: TVoxel3DSockets;
begin
  S := MakeVoxel3DSockets(UNIVERSAL_SOCKET, UNIVERSAL_SOCKET,
    UNIVERSAL_SOCKET, UNIVERSAL_SOCKET, UNIVERSAL_SOCKET,
    UNIVERSAL_SOCKET);
  SetLength(P, 9);
  P[0] := MakeVoxel3DPrototype(WFC_BUILDING3D_STRUCTURE_VOID, 'none', 1,
    S, [v3r0], [v3pfEmpty], []);
  P[1] := MakeVoxel3DPrototype(WFC_BUILDING3D_STRUCTURE_INTERIOR,
    'interior', 1, S, [v3r0], [v3pfEmpty, v3pfWalkable],
    [gdNorth, gdEast, gdSouth, gdWest]);
  P[2] := MakeVoxel3DPrototype(WFC_BUILDING3D_STRUCTURE_FEATURE,
    'interior-feature', 1, S, [v3r0],
    [v3pfEmpty, v3pfWalkable, v3pfRequiredReachable],
    [gdNorth, gdEast, gdSouth, gdWest]);
  P[3] := MakeVoxel3DPrototype(WFC_BUILDING3D_STRUCTURE_FOUNDATION,
    'stone', 1, S, [v3r0], [v3pfSolid, v3pfProvidesSupport], []);
  P[4] := MakeVoxel3DPrototype(WFC_BUILDING3D_STRUCTURE_WALL,
    'wall', 3, S, [v3r0],
    [v3pfSolid, v3pfRequiresSupport, v3pfProvidesSupport], []);
  P[5] := MakeVoxel3DPrototype(WFC_BUILDING3D_STRUCTURE_WINDOW,
    'window', 2, S, [v3r0],
    [v3pfSolid, v3pfRequiresSupport, v3pfProvidesSupport], []);
  P[6] := MakeVoxel3DPrototype(WFC_BUILDING3D_STRUCTURE_DOOR,
    'door', 1, S, [v3r0, v3r90],
    [v3pfEmpty, v3pfWalkable, v3pfEntrance], [gdNorth, gdSouth]);
  P[7] := MakeVoxel3DPrototype(WFC_BUILDING3D_STRUCTURE_LINTEL,
    'wall', 1, S, [v3r0], [v3pfSolid, v3pfProvidesSupport], []);
  //Version 1 treats a roof cell as a spanning module. State-expanded load and
  //span analysis belong to later structural libraries, not this local rule.
  P[8] := MakeVoxel3DPrototype(WFC_BUILDING3D_STRUCTURE_ROOF,
    'roof', 1, S, [v3r0], [v3pfSolid, v3pfProvidesSupport], []);
  SetLength(Pairs, 1);
  Pairs[0] := MakeVoxel3DSocketPair(UNIVERSAL_SOCKET, UNIVERSAL_SOCKET);
  Result := TVoxel3DKit.Create(STRUCTURE_KIT_ID, P, Pairs);
end;

function NewEnvelopeKit: TVoxel3DKit;
var
  P: TVoxel3DPrototypes;
  Pairs: TVoxel3DSocketPairs;
  S: TVoxel3DSockets;
begin
  S := MakeVoxel3DSockets(UNIVERSAL_SOCKET, UNIVERSAL_SOCKET,
    UNIVERSAL_SOCKET, UNIVERSAL_SOCKET, UNIVERSAL_SOCKET,
    UNIVERSAL_SOCKET);
  SetLength(P, 5);
  P[0] := MakeVoxel3DPrototype(WFC_BUILDING3D_ENVELOPE_NONE, 'none', 1,
    S, [v3r0], [v3pfEmpty], []);
  P[1] := MakeVoxel3DPrototype(WFC_BUILDING3D_ENVELOPE_FACADE,
    'facade', 1, S, [v3r0], [], []);
  P[2] := MakeVoxel3DPrototype(WFC_BUILDING3D_ENVELOPE_WINDOW,
    'window-trim', 1, S, [v3r0], [], []);
  P[3] := MakeVoxel3DPrototype(WFC_BUILDING3D_ENVELOPE_DOOR,
    'door-trim', 1, S, [v3r0], [], []);
  P[4] := MakeVoxel3DPrototype(WFC_BUILDING3D_ENVELOPE_ROOF,
    'roof-finish', 1, S, [v3r0], [], []);
  SetLength(Pairs, 1);
  Pairs[0] := MakeVoxel3DSocketPair(UNIVERSAL_SOCKET, UNIVERSAL_SOCKET);
  Result := TVoxel3DKit.Create(ENVELOPE_KIT_ID, P, Pairs);
end;

function NewPropsKit: TVoxel3DKit;
var
  P: TVoxel3DPrototypes;
  Pairs: TVoxel3DSocketPairs;
  S: TVoxel3DSockets;
begin
  S := MakeVoxel3DSockets(UNIVERSAL_SOCKET, UNIVERSAL_SOCKET,
    UNIVERSAL_SOCKET, UNIVERSAL_SOCKET, UNIVERSAL_SOCKET,
    UNIVERSAL_SOCKET);
  SetLength(P, 3);
  P[0] := MakeVoxel3DPrototype(WFC_BUILDING3D_PROP_NONE, 'none', 1,
    S, [v3r0], [v3pfEmpty], []);
  P[1] := MakeVoxel3DPrototype(WFC_BUILDING3D_PROP_LAMP, 'brass', 1,
    S, [v3r0], [v3pfSolid], []);
  P[2] := MakeVoxel3DPrototype(WFC_BUILDING3D_PROP_PLANT, 'foliage', 1,
    S, [v3r0], [v3pfSolid], []);
  SetLength(Pairs, 1);
  Pairs[0] := MakeVoxel3DSocketPair(UNIVERSAL_SOCKET, UNIVERSAL_SOCKET);
  Result := TVoxel3DKit.Create(PROPS_KIT_ID, P, Pairs);
end;

{ TBuilding3DBlueprint }

constructor TBuilding3DBlueprint.Create(const AWidth, AHeight,
  ADepth: TGraphCoordinate);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FDepth := ADepth;
  SetLength(FRoles, CheckedCellCount(AWidth, AHeight, ADepth,
    'building blueprint'));
  Fill(b3frVoid);
end;

function TBuilding3DBlueprint.CoordToIndex(const AX, AY,
  AZ: TGraphCoordinate): Integer;
begin
  if (AX >= FWidth) or (AY >= FHeight) or (AZ >= FDepth) then
    raise ERangeError.CreateFmt(
      'building blueprint coordinate out of bounds [%d,%d,%d]',
      [AX, AY, AZ]);
  Result := (Integer(AZ) * Integer(FWidth) * Integer(FHeight)) +
    (Integer(AY) * Integer(FWidth)) + Integer(AX);
end;

function TBuilding3DBlueprint.GetCellCount: Integer;
begin
  Result := Length(FRoles);
end;

function TBuilding3DBlueprint.GetRole(const AX, AY,
  AZ: TGraphCoordinate): TBuilding3DFootprintRole;
begin
  Result := FRoles[CoordToIndex(AX, AY, AZ)];
end;

function TBuilding3DBlueprint.Fill(
  const ARole: TBuilding3DFootprintRole): TBuilding3DBlueprint;
var
  I: Integer;
begin
  Building3DFootprintRoleToken(ARole);
  for I := 0 to High(FRoles) do
    FRoles[I] := ARole;
  Result := Self;
end;

function TBuilding3DBlueprint.SetRole(const AX, AY,
  AZ: TGraphCoordinate; const ARole: TBuilding3DFootprintRole):
  TBuilding3DBlueprint;
begin
  Building3DFootprintRoleToken(ARole);
  FRoles[CoordToIndex(AX, AY, AZ)] := ARole;
  Result := Self;
end;

function TBuilding3DBlueprint.CopyRoles: TBuilding3DFootprintRoles;
var
  I: Integer;
begin
  Result := Default(TBuilding3DFootprintRoles);
  SetLength(Result, Length(FRoles));
  for I := 0 to High(FRoles) do
    Result[I] := FRoles[I];
end;

{ TBuilding3D }

constructor TBuilding3D.Create(const AWidth, AHeight,
  ADepth: TGraphCoordinate);
begin
  inherited Create;
  Initialize(AWidth, AHeight, ADepth, DefaultBuilding3DConfig);
end;

constructor TBuilding3D.Create(const AWidth, AHeight,
  ADepth: TGraphCoordinate; const AConfig: TBuilding3DConfig);
begin
  inherited Create;
  Initialize(AWidth, AHeight, ADepth, AConfig);
end;

constructor TBuilding3D.Create(const ABlueprint: TBuilding3DBlueprint);
begin
  inherited Create;
  if not Assigned(ABlueprint) then
    raise EArgumentNilException.Create('building blueprint cannot be nil');
  Initialize(ABlueprint.Width, ABlueprint.Height, ABlueprint.Depth,
    DefaultBuilding3DConfig);
  ApplyBlueprint(ABlueprint);
end;

constructor TBuilding3D.Create(const ABlueprint: TBuilding3DBlueprint;
  const AConfig: TBuilding3DConfig);
begin
  inherited Create;
  if not Assigned(ABlueprint) then
    raise EArgumentNilException.Create('building blueprint cannot be nil');
  Initialize(ABlueprint.Width, ABlueprint.Height, ABlueprint.Depth, AConfig);
  ApplyBlueprint(ABlueprint);
end;

destructor TBuilding3D.Destroy;
begin
  FPropsAdapter.Free;
  FEnvelopeAdapter.Free;
  FStructureAdapter.Free;
  FGraph.Free;
  FPropsKit.Free;
  FEnvelopeKit.Free;
  FStructureKit.Free;
  inherited Destroy;
end;

procedure TBuilding3D.Initialize(const AWidth, AHeight,
  ADepth: TGraphCoordinate; const AConfig: TBuilding3DConfig);
begin
  CheckedCellCount(AWidth, AHeight, ADepth, 'building model');
  FStructureKit := NewStructureKit;
  FEnvelopeKit := NewEnvelopeKit;
  FPropsKit := NewPropsKit;
  FGraph := TGraph.Create;
  FGraph.Seed := AConfig.Seed;
  FGraph.Reshape(AWidth, AHeight, ADepth);
  FGraph.WrapNeighbors := AConfig.WrapNeighbors;

  FGraph.CurrentPass := WFC_BUILDING3D_PASS_FOOTPRINT;
  FGraph.PassMode := gpmOverlay;
  ConfigureFootprint;

  FGraph.SwitchToPass(WFC_BUILDING3D_PASS_STRUCTURE);
  FGraph.PassMode := gpmOverlay;
  FStructureAdapter := FStructureKit.ApplyToGraph(FGraph);
  ConfigureStructureDependencies;

  FGraph.SwitchToPass(WFC_BUILDING3D_PASS_ENVELOPE);
  FGraph.PassMode := gpmOverlay;
  FEnvelopeAdapter := FEnvelopeKit.ApplyToGraph(FGraph);
  ConfigureEnvelopeDependencies;

  FGraph.SwitchToPass(WFC_BUILDING3D_PASS_PROPS);
  FGraph.PassMode := gpmOverlay;
  FPropsAdapter := FPropsKit.ApplyToGraph(FGraph);
  ConfigurePropsDependencies;
  FHasSolution := False;
  FHasCommittedSolution := False;
  FFootprintDirty := False;
end;

procedure TBuilding3D.ConfigureFootprint;
var
  R: TBuilding3DFootprintRole;
begin
  for R := Low(TBuilding3DFootprintRole) to
      High(TBuilding3DFootprintRole) do
    FGraph.AddValue(Building3DFootprintRoleToken(R));
end;

procedure TBuilding3D.ConfigureStructureDependencies;
var
  I: Integer;
  G: TGraphRuleGroup;
  V: TVoxel3DVariant;
begin
  for I := 0 to FStructureAdapter.VariantCount - 1 do
  begin
    V := FStructureAdapter.VariantAt(I);
    G := FGraph.PassGraph[Ord(b3sStructure)].Rules[
      FStructureAdapter.VariantGraphKeyAt(I)];
    if V.PrototypeId = WFC_BUILDING3D_STRUCTURE_VOID then
      G.RequireFromPass(WFC_BUILDING3D_PASS_FOOTPRINT,
        WFC_BUILDING3D_ROLE_VOID)
    else if V.PrototypeId = WFC_BUILDING3D_STRUCTURE_INTERIOR then
      G.RequireFromPass(WFC_BUILDING3D_PASS_FOOTPRINT,
        WFC_BUILDING3D_ROLE_INTERIOR)
    else if V.PrototypeId = WFC_BUILDING3D_STRUCTURE_FEATURE then
      G.RequireFromPass(WFC_BUILDING3D_PASS_FOOTPRINT,
        WFC_BUILDING3D_ROLE_FEATURE)
    else if V.PrototypeId = WFC_BUILDING3D_STRUCTURE_FOUNDATION then
      G.RequireFromPass(WFC_BUILDING3D_PASS_FOOTPRINT,
        WFC_BUILDING3D_ROLE_GROUND)
    else if (V.PrototypeId = WFC_BUILDING3D_STRUCTURE_WALL) or
        (V.PrototypeId = WFC_BUILDING3D_STRUCTURE_WINDOW) then
      G.RequireFromPass(WFC_BUILDING3D_PASS_FOOTPRINT,
        WFC_BUILDING3D_ROLE_SHELL)
    else if V.PrototypeId = WFC_BUILDING3D_STRUCTURE_DOOR then
    begin
      if V.Rotation = v3r0 then
        G.RequireFromPass(WFC_BUILDING3D_PASS_FOOTPRINT,
          GraphValuesOf([WFC_BUILDING3D_ROLE_ENTRANCE_NORTH,
            WFC_BUILDING3D_ROLE_ENTRANCE_SOUTH]))
      else if V.Rotation = v3r90 then
        G.RequireFromPass(WFC_BUILDING3D_PASS_FOOTPRINT,
          GraphValuesOf([WFC_BUILDING3D_ROLE_ENTRANCE_EAST,
            WFC_BUILDING3D_ROLE_ENTRANCE_WEST]))
      else
        raise EBuilding3DModel.Create('unexpected door rotation');
    end
    else if V.PrototypeId = WFC_BUILDING3D_STRUCTURE_LINTEL then
      G.RequireFromPass(WFC_BUILDING3D_PASS_FOOTPRINT,
        WFC_BUILDING3D_ROLE_LINTEL)
    else if V.PrototypeId = WFC_BUILDING3D_STRUCTURE_ROOF then
      G.RequireFromPass(WFC_BUILDING3D_PASS_FOOTPRINT,
        WFC_BUILDING3D_ROLE_ROOF)
    else
      raise EBuilding3DModel.CreateFmt(
        'unknown structure prototype "%s"', [V.PrototypeId]);
  end;
end;

procedure TBuilding3D.ConfigureEnvelopeDependencies;
var
  R: TVoxel3DPassProjectionRules;
begin
  SetLength(R, 5);
  R[0] := MakeVoxel3DPassProjectionRule(
    PassSelector(WFC_BUILDING3D_ENVELOPE_NONE),
    PassSelectorsOf([
      PassSelector(WFC_BUILDING3D_STRUCTURE_VOID),
      PassSelector(WFC_BUILDING3D_STRUCTURE_INTERIOR),
      PassSelector(WFC_BUILDING3D_STRUCTURE_FEATURE)]));
  R[1] := MakeVoxel3DPassProjectionRule(
    PassSelector(WFC_BUILDING3D_ENVELOPE_FACADE),
    PassSelectorsOf([
      PassSelector(WFC_BUILDING3D_STRUCTURE_FOUNDATION),
      PassSelector(WFC_BUILDING3D_STRUCTURE_WALL),
      PassSelector(WFC_BUILDING3D_STRUCTURE_LINTEL)]));
  R[2] := MakeVoxel3DPassProjectionRule(
    PassSelector(WFC_BUILDING3D_ENVELOPE_WINDOW),
    PassSelectorsOf([PassSelector(WFC_BUILDING3D_STRUCTURE_WINDOW)]));
  R[3] := MakeVoxel3DPassProjectionRule(
    PassSelector(WFC_BUILDING3D_ENVELOPE_DOOR),
    PassSelectorsOf([PassSelector(WFC_BUILDING3D_STRUCTURE_DOOR)]));
  R[4] := MakeVoxel3DPassProjectionRule(
    PassSelector(WFC_BUILDING3D_ENVELOPE_ROOF),
    PassSelectorsOf([PassSelector(WFC_BUILDING3D_STRUCTURE_ROOF)]));
  RequireVoxel3DProjectionFromPass(FEnvelopeKit, FEnvelopeAdapter,
    FStructureKit, FStructureAdapter, R);
end;

procedure TBuilding3D.ConfigurePropsDependencies;
var
  I: Integer;
  G: TGraphRuleGroup;
  EnvelopeRules: TVoxel3DPassProjectionRules;
  StructureRules: TVoxel3DPassProjectionRules;
  V: TVoxel3DVariant;
begin
  for I := 0 to FPropsAdapter.VariantCount - 1 do
  begin
    V := FPropsAdapter.VariantAt(I);
    G := FGraph.PassGraph[Ord(b3sProps)].Rules[
      FPropsAdapter.VariantGraphKeyAt(I)];
    if V.PrototypeId = WFC_BUILDING3D_PROP_NONE then
      G.RequireFromPass(WFC_BUILDING3D_PASS_FOOTPRINT,
        GraphValuesOf([
          WFC_BUILDING3D_ROLE_VOID,
          WFC_BUILDING3D_ROLE_GROUND,
          WFC_BUILDING3D_ROLE_SHELL,
          WFC_BUILDING3D_ROLE_INTERIOR,
          WFC_BUILDING3D_ROLE_LINTEL,
          WFC_BUILDING3D_ROLE_ROOF,
          WFC_BUILDING3D_ROLE_ENTRANCE_NORTH,
          WFC_BUILDING3D_ROLE_ENTRANCE_EAST,
          WFC_BUILDING3D_ROLE_ENTRANCE_SOUTH,
          WFC_BUILDING3D_ROLE_ENTRANCE_WEST]))
    else if (V.PrototypeId = WFC_BUILDING3D_PROP_LAMP) or
        (V.PrototypeId = WFC_BUILDING3D_PROP_PLANT) then
      G.RequireFromPass(WFC_BUILDING3D_PASS_FOOTPRINT,
        WFC_BUILDING3D_ROLE_FEATURE)
    else
      raise EBuilding3DModel.CreateFmt(
        'unknown prop prototype "%s"', [V.PrototypeId]);
  end;

  SetLength(StructureRules, 3);
  StructureRules[0] := MakeVoxel3DPassProjectionRule(
    PassSelector(WFC_BUILDING3D_PROP_NONE),
    PassSelectorsOf([
      PassSelector(WFC_BUILDING3D_STRUCTURE_VOID),
      PassSelector(WFC_BUILDING3D_STRUCTURE_INTERIOR),
      PassSelector(WFC_BUILDING3D_STRUCTURE_FOUNDATION),
      PassSelector(WFC_BUILDING3D_STRUCTURE_WALL),
      PassSelector(WFC_BUILDING3D_STRUCTURE_WINDOW),
      PassSelector(WFC_BUILDING3D_STRUCTURE_DOOR),
      PassSelector(WFC_BUILDING3D_STRUCTURE_LINTEL),
      PassSelector(WFC_BUILDING3D_STRUCTURE_ROOF)]));
  StructureRules[1] := MakeVoxel3DPassProjectionRule(
    PassSelector(WFC_BUILDING3D_PROP_LAMP),
    PassSelectorsOf([PassSelector(WFC_BUILDING3D_STRUCTURE_FEATURE)]));
  StructureRules[2] := MakeVoxel3DPassProjectionRule(
    PassSelector(WFC_BUILDING3D_PROP_PLANT),
    PassSelectorsOf([PassSelector(WFC_BUILDING3D_STRUCTURE_FEATURE)]));

  SetLength(EnvelopeRules, 3);
  EnvelopeRules[0] := MakeVoxel3DPassProjectionRule(
    PassSelector(WFC_BUILDING3D_PROP_NONE),
    PassSelectorsOf([
      PassSelector(WFC_BUILDING3D_ENVELOPE_NONE),
      PassSelector(WFC_BUILDING3D_ENVELOPE_FACADE),
      PassSelector(WFC_BUILDING3D_ENVELOPE_WINDOW),
      PassSelector(WFC_BUILDING3D_ENVELOPE_DOOR),
      PassSelector(WFC_BUILDING3D_ENVELOPE_ROOF)]));
  EnvelopeRules[1] := MakeVoxel3DPassProjectionRule(
    PassSelector(WFC_BUILDING3D_PROP_LAMP),
    PassSelectorsOf([PassSelector(WFC_BUILDING3D_ENVELOPE_NONE)]));
  EnvelopeRules[2] := MakeVoxel3DPassProjectionRule(
    PassSelector(WFC_BUILDING3D_PROP_PLANT),
    PassSelectorsOf([PassSelector(WFC_BUILDING3D_ENVELOPE_NONE)]));

  //Validate both complete maps before the first private-key requirement is
  //installed. This keeps a malformed built-in definition mutation-free.
  ValidateVoxel3DProjectionFromPass(FPropsKit, FPropsAdapter,
    FStructureKit, FStructureAdapter, StructureRules);
  ValidateVoxel3DProjectionFromPass(FPropsKit, FPropsAdapter,
    FEnvelopeKit, FEnvelopeAdapter, EnvelopeRules);
  RequireVoxel3DProjectionFromPass(FPropsKit, FPropsAdapter,
    FStructureKit, FStructureAdapter, StructureRules);
  RequireVoxel3DProjectionFromPass(FPropsKit, FPropsAdapter,
    FEnvelopeKit, FEnvelopeAdapter, EnvelopeRules);
end;

function TBuilding3D.ApplyBlueprint(
  const ABlueprint: TBuilding3DBlueprint): TBuilding3D;
var
  X, Y, Z: Integer;
begin
  if not Assigned(ABlueprint) then
    raise EArgumentNilException.Create('building blueprint cannot be nil');
  if (ABlueprint.Width <> Width) or (ABlueprint.Height <> Height) or
      (ABlueprint.Depth <> Depth) then
    raise EBuilding3DBlueprint.Create(
      'building blueprint dimensions do not match the model');
  for Z := 0 to Integer(Depth) - 1 do
    for Y := 0 to Integer(Height) - 1 do
      for X := 0 to Integer(Width) - 1 do
        FGraph.PassGraph[Ord(b3sFootprint)].SetAllowedValues(X, Y, Z,
          Building3DFootprintRoleToken(ABlueprint[X, Y, Z]));
  MarkFootprintDirty;
  Result := Self;
end;

function TBuilding3D.SetFootprintRole(const AX, AY,
  AZ: TGraphCoordinate; const ARole: TBuilding3DFootprintRole): TBuilding3D;
begin
  FGraph.PassGraph[Ord(b3sFootprint)].SetAllowedValues(AX, AY, AZ,
    Building3DFootprintRoleToken(ARole));
  MarkFootprintDirty;
  Result := Self;
end;

function TBuilding3D.AllowFootprintRoles(const AX, AY,
  AZ: TGraphCoordinate; const ARoles: TBuilding3DFootprintRoles): TBuilding3D;
var
  I: Integer;
  V: TGraphValues;
begin
  SetLength(V, Length(ARoles));
  for I := 0 to High(ARoles) do
    V[I] := Building3DFootprintRoleToken(ARoles[I]);
  FGraph.PassGraph[Ord(b3sFootprint)].SetAllowedValues(AX, AY, AZ, V);
  MarkFootprintDirty;
  Result := Self;
end;

function TBuilding3D.ClearFootprintDomain(const AX, AY,
  AZ: TGraphCoordinate): TBuilding3D;
begin
  FGraph.PassGraph[Ord(b3sFootprint)].ClearAllowedValues(AX, AY, AZ);
  MarkFootprintDirty;
  Result := Self;
end;

procedure TBuilding3D.MarkFootprintDirty;
begin
  FFootprintDirty := True;
  FHasSolution := False;
end;

function TBuilding3D.TryGenerate(const AOptions: TGraphSolveOptions;
  out AReport: TGraphSolveReport): Boolean;
begin
  Result := FGraph.TrySolve(AOptions, AReport);
  if Result then
  begin
    FHasSolution := True;
    FHasCommittedSolution := True;
    FFootprintDirty := False;
  end;
end;

function TBuilding3D.TryGenerate(out AReport: TGraphSolveReport): Boolean;
begin
  Result := TryGenerate(DefaultGraphSolveOptions, AReport);
end;

function TBuilding3D.TryRegenerateFrom(const AStage: TBuilding3DStage;
  const AOptions: TGraphSolveOptions;
  out AReport: TGraphSolveReport): Boolean;
begin
  if not FHasCommittedSolution then
    raise EBuilding3DScene.Create(
      'building selective regeneration requires a committed solution');
  if FFootprintDirty and (AStage <> b3sFootprint) then
    raise EBuilding3DScene.Create(
      'a changed footprint must be regenerated from the footprint pass');
  Result := FGraph.TryRegenerateFrom(Building3DStageName(AStage),
    AOptions, AReport);
  if Result then
  begin
    FHasSolution := True;
    FHasCommittedSolution := True;
    FFootprintDirty := False;
  end;
end;

function TBuilding3D.TryRegenerateFrom(const AStage: TBuilding3DStage;
  out AReport: TGraphSolveReport): Boolean;
begin
  Result := TryRegenerateFrom(AStage, DefaultGraphSolveOptions, AReport);
end;

procedure TBuilding3D.RequireSolution(const AOperation: String);
begin
  if not FHasSolution then
    raise EBuilding3DScene.Create(AOperation + ' requires a solved building');
end;

procedure TBuilding3D.RequireCanonicalPipeline(const AOperation: String);
begin
  RequireSolution(AOperation);
  if not DefinitionMatchesPipeline then
    raise EBuilding3DScene.Create(AOperation +
      ' requires the canonical Building 3D pipeline definition');
end;

function TBuilding3D.AdapterForStage(
  const AStage: TBuilding3DStage): TVoxel3DGraphAdapter;
begin
  case AStage of
    b3sStructure: Result := FStructureAdapter;
    b3sEnvelopeRoof: Result := FEnvelopeAdapter;
    b3sProps: Result := FPropsAdapter;
    b3sFootprint:
      raise EBuilding3DScene.Create(
        'the footprint pass has public roles, not voxel variants');
  else
    raise ERangeError.CreateFmt('building stage is out of bounds [%d]',
      [Ord(AStage)]);
  end;
end;

function TBuilding3D.StageGraph(const AStage: TBuilding3DStage): TGraph;
begin
  Building3DStageName(AStage);
  Result := FGraph.PassGraph[Ord(AStage)];
end;

function TBuilding3D.StageVariantAt(const AStage: TBuilding3DStage;
  const AX, AY, AZ: TGraphCoordinate): TVoxel3DVariant;
var
  A: TVoxel3DGraphAdapter;
  I: Integer;
  V: TGraphValue;
begin
  RequireSolution('building variant access');
  A := AdapterForStage(AStage);
  V := StageGraph(AStage).Entry[AX, AY, AZ].Value;
  if not A.FindVariantGraphKey(V, I) then
    raise EBuilding3DScene.CreateFmt(
      'building stage %s has an unknown value at [%d,%d,%d]',
      [Building3DStageName(AStage), AX, AY, AZ]);
  Result := A.VariantAt(I);
end;

function TBuilding3D.StageTokenAt(const AStage: TBuilding3DStage;
  const AX, AY, AZ: TGraphCoordinate): String;
begin
  RequireSolution('building stage access');
  if AStage = b3sFootprint then
    Result := Building3DFootprintRoleToken(
      FootprintRoleAt(AX, AY, AZ))
  else
    Result := StageVariantAt(AStage, AX, AY, AZ).PrototypeId;
end;

function TBuilding3D.StageRotationAt(const AStage: TBuilding3DStage;
  const AX, AY, AZ: TGraphCoordinate): TVoxel3DRotation;
begin
  Result := StageVariantAt(AStage, AX, AY, AZ).Rotation;
end;

function TBuilding3D.FootprintRoleAt(const AX, AY,
  AZ: TGraphCoordinate): TBuilding3DFootprintRole;
var
  V: String;
begin
  RequireSolution('building footprint access');
  V := StageGraph(b3sFootprint).Entry[AX, AY, AZ].Value;
  if not TryBuilding3DFootprintRole(V, Result) then
    raise EBuilding3DScene.CreateFmt(
      'building footprint has an unknown value at [%d,%d,%d]',
      [AX, AY, AZ]);
end;

function TBuilding3D.StructureKindAt(const AX, AY,
  AZ: TGraphCoordinate): TBuilding3DStructureKind;
var
  V: String;
begin
  V := StageVariantAt(b3sStructure, AX, AY, AZ).PrototypeId;
  if not TryBuilding3DStructureKind(V, Result) then
    raise EBuilding3DScene.CreateFmt(
      'building structure has an unknown prototype at [%d,%d,%d]',
      [AX, AY, AZ]);
end;

function TBuilding3D.EnvelopeKindAt(const AX, AY,
  AZ: TGraphCoordinate): TBuilding3DEnvelopeKind;
var
  V: String;
begin
  V := StageVariantAt(b3sEnvelopeRoof, AX, AY, AZ).PrototypeId;
  if not TryBuilding3DEnvelopeKind(V, Result) then
    raise EBuilding3DScene.CreateFmt(
      'building envelope has an unknown prototype at [%d,%d,%d]',
      [AX, AY, AZ]);
end;

function TBuilding3D.PropKindAt(const AX, AY,
  AZ: TGraphCoordinate): TBuilding3DPropKind;
var
  V: String;
begin
  V := StageVariantAt(b3sProps, AX, AY, AZ).PrototypeId;
  if not TryBuilding3DPropKind(V, Result) then
    raise EBuilding3DScene.CreateFmt(
      'building props have an unknown prototype at [%d,%d,%d]',
      [AX, AY, AZ]);
end;

function PassDependenciesMatch(const AGraph: TGraph;
  const AExpected: array of Integer): Boolean;
var
  I: Integer;
begin
  Result := AGraph.DependencyCount = Length(AExpected);
  if not Result then
    Exit;
  for I := 0 to High(AExpected) do
    if AGraph.DependencyIndex[I] <> AExpected[I] then
      Exit(False);
end;

function TBuilding3D.DefinitionMatchesPipeline: Boolean;
var
  I: Integer;
  P: TGraph;
  R: TBuilding3DFootprintRole;
  Registered: TGraphValues;
  Group: TGraphRuleGroup;
begin
  Result := False;
  try
    if (not Assigned(FGraph)) or FGraph.Running or
        (FGraph.TotalPassCount <> 4) or
        (not Assigned(FStructureKit)) or
        (not Assigned(FEnvelopeKit)) or
        (not Assigned(FPropsKit)) or
        (not Assigned(FStructureAdapter)) or
        (not Assigned(FEnvelopeAdapter)) or
        (not Assigned(FPropsAdapter)) then
      Exit;

    for I := 0 to 3 do
    begin
      P := FGraph.PassGraph[I];
      if (P.CurrentPass <> Building3DStageName(TBuilding3DStage(I))) or
          (P.CurrentPassIndex <> I) or (P.PassMode <> gpmOverlay) or
          (P.Dimension.Width <> Width) or
          (P.Dimension.Height <> Height) or
          (P.Dimension.Depth <> Depth) or
          (P.WrapNeighbors <> WrapNeighbors) then
        Exit;
    end;

    if not PassDependenciesMatch(FGraph.PassGraph[0], []) or
        not PassDependenciesMatch(FGraph.PassGraph[1], [0]) or
        not PassDependenciesMatch(FGraph.PassGraph[2], [1]) or
        not PassDependenciesMatch(FGraph.PassGraph[3], [0, 1, 2]) then
      Exit;

    Registered := FGraph.PassGraph[0].CopyRegisteredValues;
    if (Length(Registered) <> Ord(High(TBuilding3DFootprintRole)) + 1) or
        (FGraph.PassGraph[0].RuleGroups.Count <> Length(Registered)) then
      Exit;
    for R := Low(TBuilding3DFootprintRole) to
        High(TBuilding3DFootprintRole) do
    begin
      I := Ord(R);
      if Registered[I] <> Building3DFootprintRoleToken(R) then
        Exit;
      if not FGraph.PassGraph[0].RuleGroups.ContainsKey(Registered[I]) then
        Exit;
      Group := FGraph.PassGraph[0].RuleGroups[Registered[I]];
      if (not Assigned(Group)) or (Group.Value <> Registered[I]) or
          (Group.Weight <> WFC_DEFAULT_VALUE_WEIGHT) or
          (Length(Group.Rules) <> 0) or
          (Group.DeniedDirections <> []) or
          (Length(Group.PreviousValues) <> 0) then
        Exit;
    end;

    if (FStructureAdapter.AppliedGraph <> FGraph.PassGraph[1]) or
        (FEnvelopeAdapter.AppliedGraph <> FGraph.PassGraph[2]) or
        (FPropsAdapter.AppliedGraph <> FGraph.PassGraph[3]) or
        not FStructureKit.MatchesAdapter(FStructureAdapter) or
        not FEnvelopeKit.MatchesAdapter(FEnvelopeAdapter) or
        not FPropsKit.MatchesAdapter(FPropsAdapter) or
        not FStructureAdapter.DefinitionMatchesGraph or
        not FEnvelopeAdapter.DefinitionMatchesGraph or
        not FPropsAdapter.DefinitionMatchesGraph then
      Exit;

    Result := True;
  except
    on Exception do
      Result := False;
  end;
end;

function TBuilding3D.CaptureStructureScene: TVoxel3DScene;
begin
  RequireCanonicalPipeline('building structure capture');
  Result := FStructureKit.CaptureScene(FStructureAdapter);
end;

function TBuilding3D.CaptureEnvelopeScene: TVoxel3DScene;
begin
  RequireCanonicalPipeline('building envelope capture');
  Result := FEnvelopeKit.CaptureScene(FEnvelopeAdapter);
end;

function TBuilding3D.CapturePropsScene: TVoxel3DScene;
begin
  RequireCanonicalPipeline('building props capture');
  Result := FPropsKit.CaptureScene(FPropsAdapter);
end;

procedure HashByte(var AHash: Cardinal; const AValue: Byte);
{$PUSH}
{$Q-}
var
  LValue: Cardinal;
begin
  AHash := AHash xor Cardinal(AValue);
  LValue := AHash;
  AHash := (LValue + (LValue shl 1) + (LValue shl 4) +
    (LValue shl 7) + (LValue shl 8) + (LValue shl 24)) and
    Cardinal($FFFFFFFF);
end;
{$POP}

procedure HashCardinal(var AHash: Cardinal; const AValue: Cardinal);
begin
  HashByte(AHash, Byte(AValue and $FF));
  HashByte(AHash, Byte((AValue shr 8) and $FF));
  HashByte(AHash, Byte((AValue shr 16) and $FF));
  HashByte(AHash, Byte((AValue shr 24) and $FF));
end;

procedure HashText(var AHash: Cardinal; const AValue: String);
var
  I: Integer;
begin
  HashCardinal(AHash, Cardinal(Length(AValue)));
  for I := 1 to Length(AValue) do
    HashByte(AHash, Byte(Ord(AValue[I])));
end;

function TBuilding3D.Signature: TBuilding3DSignature;
var
  S: TBuilding3DStage;
  X, Y, Z: Integer;
begin
  RequireCanonicalPipeline('building signature');
  Result := Cardinal(2166136261);
  HashText(Result, 'wfc-building3d');
  HashCardinal(Result, WFC_BUILDING3D_MODEL_VERSION);
  HashCardinal(Result, WFC_BUILDING3D_BLUEPRINT_VERSION);
  HashCardinal(Result, WFC_BUILDING3D_SIGNATURE_VERSION);
  HashCardinal(Result, WFC_VOXEL3D_PASS_BRIDGE_VERSION);
  HashCardinal(Result, WFC_RANDOM_ALGORITHM_VERSION);
  HashCardinal(Result, WFC_SOLVER_ALGORITHM_VERSION);
  HashCardinal(Result, WFC_GRAPH_MODEL_VERSION);
  HashCardinal(Result, WFC_PIPELINE_ALGORITHM_VERSION);
  HashCardinal(Result, Cardinal(Width));
  HashCardinal(Result, Cardinal(Height));
  HashCardinal(Result, Cardinal(Depth));
  HashCardinal(Result, FStructureKit.Signature);
  HashCardinal(Result, FEnvelopeKit.Signature);
  HashCardinal(Result, FPropsKit.Signature);
  if WrapNeighbors then HashByte(Result, 1) else HashByte(Result, 0);
  for S := Low(TBuilding3DStage) to High(TBuilding3DStage) do
  begin
    HashCardinal(Result, Cardinal(Ord(S)));
    for Z := 0 to Integer(Depth) - 1 do
      for Y := 0 to Integer(Height) - 1 do
        for X := 0 to Integer(Width) - 1 do
        begin
          HashText(Result, StageTokenAt(S, X, Y, Z));
          if S <> b3sFootprint then
            HashByte(Result, Byte(Ord(StageRotationAt(S, X, Y, Z))));
        end;
  end;
end;

function TBuilding3D.PipelineSignature: String;
begin
  Result := IntToStr(WFC_BUILDING3D_SIGNATURE_VERSION) + ':' +
    Building3DSignatureHex(Signature);
end;

function TBuilding3D.GetWidth: TGraphCoordinate;
begin
  Result := FGraph.Dimension.Width;
end;

function TBuilding3D.GetHeight: TGraphCoordinate;
begin
  Result := FGraph.Dimension.Height;
end;

function TBuilding3D.GetDepth: TGraphCoordinate;
begin
  Result := FGraph.Dimension.Depth;
end;

function TBuilding3D.GetSeed: TGraphSeed;
begin
  Result := FGraph.Seed;
end;

procedure TBuilding3D.SetSeed(const AValue: TGraphSeed);
begin
  FGraph.Seed := AValue;
end;

function TBuilding3D.GetWrapNeighbors: Boolean;
begin
  Result := FGraph.WrapNeighbors;
end;

end.
