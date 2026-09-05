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
unit wfc_world2d_settlement;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_world2d;

const
  WFC_SETTLEMENT2D_MODEL_VERSION = 1;
  WFC_SETTLEMENT2D_SIGNATURE_VERSION = 1;

  WFC_SETTLEMENT2D_PASS_TERRAIN = 'terrain';
  WFC_SETTLEMENT2D_PASS_HYDROLOGY = 'hydrology';
  WFC_SETTLEMENT2D_PASS_BIOME = 'biome';
  WFC_SETTLEMENT2D_PASS_ROADS = 'roads';
  WFC_SETTLEMENT2D_PASS_HOUSING = 'housing';
  WFC_SETTLEMENT2D_PASS_FOLIAGE = 'foliage';

  WFC_SETTLEMENT2D_HYDROLOGY_SEA = 'sea';
  WFC_SETTLEMENT2D_HYDROLOGY_DRY = 'dry';
  WFC_SETTLEMENT2D_HYDROLOGY_RIVER = 'river';

  WFC_SETTLEMENT2D_ROADS_NONE = 'none';
  WFC_SETTLEMENT2D_ROADS_TRAIL = 'trail';
  WFC_SETTLEMENT2D_ROADS_BRIDGE = 'bridge';
  WFC_SETTLEMENT2D_ROADS_TUNNEL = 'tunnel';

  WFC_SETTLEMENT2D_HOUSING_NONE = 'none';
  WFC_SETTLEMENT2D_HOUSING_HOUSE = 'house';
  WFC_SETTLEMENT2D_HOUSING_CABIN = 'cabin';
  WFC_SETTLEMENT2D_HOUSING_LODGE = 'lodge';

  WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS: TGraphDirections =
    [gdNorth, gdEast, gdSouth, gdWest];

type
  ESettlement2D = class(Exception);

  TSettlement2DLayer = (
    s2lTerrain,
    s2lHydrology,
    s2lBiome,
    s2lRoads,
    s2lHousing,
    s2lFoliage
  );

  TSettlement2DSignature = Cardinal;
  TSettlement2DLayerSignatures =
    array[TSettlement2DLayer] of TSettlement2DSignature;

  TSettlement2DConfig = record
    Seed: TGraphSeed;
    WrapNeighbors: Boolean;
  end;

  { TSettlement2D }

  (*
    A six-layer dependency-DAG world. Every layer is an overlay with its own
    value domain. Dependency indices are stable creation indices, while solve
    and selective-regeneration order comes from the declared DAG.
  *)
  TSettlement2D = class
  strict private
    FGraph: TGraph;

    function GetHeight: TGraphCoordinate;
    function GetLayerGraph(const ALayer: TSettlement2DLayer): TGraph;
    function GetSeed: TGraphSeed;
    function GetValue(const ALayer: TSettlement2DLayer;
      const AX, AY: TGraphCoordinate): TGraphValue;
    function GetWidth: TGraphCoordinate;
    function GetWrapNeighbors: Boolean;
    procedure SetSeed(const AValue: TGraphSeed);
    procedure SetWrapNeighbors(const AValue: Boolean);
    procedure Initialize(const AWidth, AHeight: TGraphCoordinate;
      const AConfig: TSettlement2DConfig);
    procedure ValidateCoordinate(const AX, AY: TGraphCoordinate);
    procedure ValidateSignatureShape;

    procedure ConfigureTerrain;
    procedure ConfigureHydrology;
    procedure ConfigureBiomes;
    procedure ConfigureRoads;
    procedure ConfigureHousing;
    procedure ConfigureFoliage;
  public
    constructor Create(const AWidth, AHeight: TGraphCoordinate); overload;
    constructor Create(const AWidth, AHeight: TGraphCoordinate;
      const AConfig: TSettlement2DConfig); overload;
    destructor Destroy; override;

    function Lock(const ALayer: TSettlement2DLayer;
      const AX, AY: TGraphCoordinate;
      const AValue: TGraphValue): TSettlement2D;
    function ClearLock(const ALayer: TSettlement2DLayer;
      const AX, AY: TGraphCoordinate): TSettlement2D;
    function TryGenerate(const AOptions: TGraphSolveOptions;
      out AReport: TGraphSolveReport): Boolean; overload;
    function TryGenerate(out AReport: TGraphSolveReport): Boolean; overload;
    function TryRegenerateFrom(const APass: String;
      const AOptions: TGraphSolveOptions;
      out AReport: TGraphSolveReport): Boolean; overload;
    function TryRegenerateFrom(const APass: String;
      out AReport: TGraphSolveReport): Boolean; overload;
    function TryRegenerateFrom(const APasses: TGraphPassLabels;
      const AOptions: TGraphSolveOptions;
      out AReport: TGraphSolveReport): Boolean; overload;

    function LayerSignature(
      const ALayer: TSettlement2DLayer): TSettlement2DSignature;
    function PipelineSignature: String;

    property Graph: TGraph read FGraph;
    property LayerGraph[const ALayer: TSettlement2DLayer]: TGraph
      read GetLayerGraph;
    property Value[const ALayer: TSettlement2DLayer;
      const AX, AY: TGraphCoordinate]: TGraphValue read GetValue;
    property Width: TGraphCoordinate read GetWidth;
    property Height: TGraphCoordinate read GetHeight;
    property Seed: TGraphSeed read GetSeed write SetSeed;
    property WrapNeighbors: Boolean read GetWrapNeighbors
      write SetWrapNeighbors;
  end;

function Settlement2DLayerName(const ALayer: TSettlement2DLayer): String;
function Settlement2DSignatureHex(
  const ASignature: TSettlement2DSignature): String;
function DefaultSettlement2DConfig: TSettlement2DConfig;
function IsSettlement2DLayerValue(const ALayer: TSettlement2DLayer;
  const AValue: TGraphValue): Boolean;

implementation

const
  CRC32_POLYNOMIAL = Cardinal($EDB88320);
  HEX_DIGITS = '0123456789ABCDEF';

function Settlement2DLayerName(const ALayer: TSettlement2DLayer): String;
begin
  case ALayer of
    s2lTerrain:
      Result := WFC_SETTLEMENT2D_PASS_TERRAIN;
    s2lHydrology:
      Result := WFC_SETTLEMENT2D_PASS_HYDROLOGY;
    s2lBiome:
      Result := WFC_SETTLEMENT2D_PASS_BIOME;
    s2lRoads:
      Result := WFC_SETTLEMENT2D_PASS_ROADS;
    s2lHousing:
      Result := WFC_SETTLEMENT2D_PASS_HOUSING;
    s2lFoliage:
      Result := WFC_SETTLEMENT2D_PASS_FOLIAGE;
  end;
end;

function DefaultSettlement2DConfig: TSettlement2DConfig;
begin
  Result.Seed := 0;
  Result.WrapNeighbors := False;
end;

function TrySettlement2DValueToken(const ALayer: TSettlement2DLayer;
  const AValue: TGraphValue; out AToken: Byte): Boolean;
begin
  Result := True;
  case ALayer of
    s2lTerrain:
      if AValue = WFC_WORLD2D_TERRAIN_WATER then
        AToken := $10
      else if AValue = WFC_WORLD2D_TERRAIN_LAND then
        AToken := $11
      else if AValue = WFC_WORLD2D_TERRAIN_MOUNTAIN then
        AToken := $12
      else
        Result := False;
    s2lHydrology:
      if AValue = WFC_SETTLEMENT2D_HYDROLOGY_SEA then
        AToken := $20
      else if AValue = WFC_SETTLEMENT2D_HYDROLOGY_DRY then
        AToken := $21
      else if AValue = WFC_SETTLEMENT2D_HYDROLOGY_RIVER then
        AToken := $22
      else
        Result := False;
    s2lBiome:
      if AValue = WFC_WORLD2D_BIOME_OCEAN then
        AToken := $30
      else if AValue = WFC_WORLD2D_BIOME_SHORE then
        AToken := $31
      else if AValue = WFC_WORLD2D_BIOME_PLAINS then
        AToken := $32
      else if AValue = WFC_WORLD2D_BIOME_WOODLAND then
        AToken := $33
      else if AValue = WFC_WORLD2D_BIOME_ALPINE then
        AToken := $34
      else
        Result := False;
    s2lRoads:
      if AValue = WFC_SETTLEMENT2D_ROADS_NONE then
        AToken := $40
      else if AValue = WFC_SETTLEMENT2D_ROADS_TRAIL then
        AToken := $41
      else if AValue = WFC_SETTLEMENT2D_ROADS_BRIDGE then
        AToken := $42
      else if AValue = WFC_SETTLEMENT2D_ROADS_TUNNEL then
        AToken := $43
      else
        Result := False;
    s2lHousing:
      if AValue = WFC_SETTLEMENT2D_HOUSING_NONE then
        AToken := $50
      else if AValue = WFC_SETTLEMENT2D_HOUSING_HOUSE then
        AToken := $51
      else if AValue = WFC_SETTLEMENT2D_HOUSING_CABIN then
        AToken := $52
      else if AValue = WFC_SETTLEMENT2D_HOUSING_LODGE then
        AToken := $53
      else
        Result := False;
    s2lFoliage:
      if AValue = WFC_WORLD2D_FOLIAGE_NONE then
        AToken := $60
      else if AValue = WFC_WORLD2D_FOLIAGE_REEDS then
        AToken := $61
      else if AValue = WFC_WORLD2D_FOLIAGE_GRASS then
        AToken := $62
      else if AValue = WFC_WORLD2D_FOLIAGE_TREE then
        AToken := $63
      else if AValue = WFC_WORLD2D_FOLIAGE_PINE then
        AToken := $64
      else
        Result := False;
  end;
end;

function IsSettlement2DLayerValue(const ALayer: TSettlement2DLayer;
  const AValue: TGraphValue): Boolean;
var
  LToken: Byte;
begin
  Result := TrySettlement2DValueToken(ALayer, AValue, LToken);
end;

function Settlement2DSignatureHex(
  const ASignature: TSettlement2DSignature): String;
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

function UpdateCrc32(const ACrc, AByte: Cardinal): Cardinal;
var
  I: Integer;
begin
  Result := ACrc xor (AByte and Cardinal($FF));
  for I := 0 to 7 do
    if (Result and Cardinal(1)) <> 0 then
      Result := (Result shr 1) xor CRC32_POLYNOMIAL
    else
      Result := Result shr 1;
end;

procedure MixCardinal(var ACrc: Cardinal; const AValue: Cardinal);
begin
  ACrc := UpdateCrc32(ACrc, AValue);
  ACrc := UpdateCrc32(ACrc, AValue shr 8);
  ACrc := UpdateCrc32(ACrc, AValue shr 16);
  ACrc := UpdateCrc32(ACrc, AValue shr 24);
end;

constructor TSettlement2D.Create(
  const AWidth, AHeight: TGraphCoordinate);
begin
  inherited Create;
  Initialize(AWidth, AHeight, DefaultSettlement2DConfig);
end;

constructor TSettlement2D.Create(const AWidth, AHeight: TGraphCoordinate;
  const AConfig: TSettlement2DConfig);
begin
  inherited Create;
  Initialize(AWidth, AHeight, AConfig);
end;

procedure TSettlement2D.Initialize(
  const AWidth, AHeight: TGraphCoordinate;
  const AConfig: TSettlement2DConfig);
begin
  if (AWidth = 0) or (AHeight = 0) then
    raise ERangeError.Create(
      'a settlement world needs positive width and height');

  FGraph := TGraph.Create;
  try
    FGraph.Seed := AConfig.Seed;
    FGraph.Reshape(AWidth, AHeight, 1);
    FGraph.WrapNeighbors := AConfig.WrapNeighbors;

    FGraph.CurrentPass := WFC_SETTLEMENT2D_PASS_TERRAIN;
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies;
    ConfigureTerrain;

    FGraph.SwitchToPass(WFC_SETTLEMENT2D_PASS_HYDROLOGY);
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies
      .DependsOn(WFC_SETTLEMENT2D_PASS_TERRAIN);
    ConfigureHydrology;

    FGraph.SwitchToPass(WFC_SETTLEMENT2D_PASS_BIOME);
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies
      .DependsOn(WFC_SETTLEMENT2D_PASS_TERRAIN);
    ConfigureBiomes;

    FGraph.SwitchToPass(WFC_SETTLEMENT2D_PASS_ROADS);
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies
      .DependsOn(WFC_SETTLEMENT2D_PASS_TERRAIN)
      .DependsOn(WFC_SETTLEMENT2D_PASS_HYDROLOGY)
      .DependsOn(WFC_SETTLEMENT2D_PASS_BIOME);
    ConfigureRoads;

    FGraph.SwitchToPass(WFC_SETTLEMENT2D_PASS_HOUSING);
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies
      .DependsOn(WFC_SETTLEMENT2D_PASS_TERRAIN)
      .DependsOn(WFC_SETTLEMENT2D_PASS_HYDROLOGY)
      .DependsOn(WFC_SETTLEMENT2D_PASS_BIOME)
      .DependsOn(WFC_SETTLEMENT2D_PASS_ROADS);
    ConfigureHousing;

    FGraph.SwitchToPass(WFC_SETTLEMENT2D_PASS_FOLIAGE);
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies
      .DependsOn(WFC_SETTLEMENT2D_PASS_TERRAIN)
      .DependsOn(WFC_SETTLEMENT2D_PASS_HYDROLOGY)
      .DependsOn(WFC_SETTLEMENT2D_PASS_BIOME)
      .DependsOn(WFC_SETTLEMENT2D_PASS_ROADS)
      .DependsOn(WFC_SETTLEMENT2D_PASS_HOUSING);
    ConfigureFoliage;

    FGraph.SwitchToPass(WFC_SETTLEMENT2D_PASS_TERRAIN);
  except
    FGraph.Free;
    FGraph := nil;
    raise;
  end;
end;

destructor TSettlement2D.Destroy;
begin
  FGraph.Free;
  inherited Destroy;
end;

procedure TSettlement2D.ConfigureTerrain;
begin
  FGraph.AddValue(WFC_WORLD2D_TERRAIN_WATER)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_TERRAIN_WATER,
      WFC_WORLD2D_TERRAIN_LAND]);
  FGraph.AddValue(WFC_WORLD2D_TERRAIN_LAND)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_TERRAIN_WATER,
      WFC_WORLD2D_TERRAIN_LAND,
      WFC_WORLD2D_TERRAIN_MOUNTAIN]);
  FGraph.AddValue(WFC_WORLD2D_TERRAIN_MOUNTAIN)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_TERRAIN_LAND,
      WFC_WORLD2D_TERRAIN_MOUNTAIN]);
end;

procedure TSettlement2D.ConfigureHydrology;
begin
  FGraph.AddValue(WFC_SETTLEMENT2D_HYDROLOGY_SEA, 1)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_WATER);
  FGraph.AddValue(WFC_SETTLEMENT2D_HYDROLOGY_DRY, 5)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN, [
      WFC_WORLD2D_TERRAIN_LAND,
      WFC_WORLD2D_TERRAIN_MOUNTAIN]);
  FGraph.AddValue(WFC_SETTLEMENT2D_HYDROLOGY_RIVER, 1)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_LAND);
end;

procedure TSettlement2D.ConfigureBiomes;
begin
  FGraph.AddValue(WFC_WORLD2D_BIOME_OCEAN)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_BIOME_OCEAN,
      WFC_WORLD2D_BIOME_SHORE,
      WFC_WORLD2D_BIOME_PLAINS,
      WFC_WORLD2D_BIOME_WOODLAND])
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_WATER);
  FGraph.AddValue(WFC_WORLD2D_BIOME_SHORE)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_BIOME_OCEAN,
      WFC_WORLD2D_BIOME_SHORE,
      WFC_WORLD2D_BIOME_PLAINS,
      WFC_WORLD2D_BIOME_WOODLAND,
      WFC_WORLD2D_BIOME_ALPINE])
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_LAND);
  FGraph.AddValue(WFC_WORLD2D_BIOME_PLAINS)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_BIOME_OCEAN,
      WFC_WORLD2D_BIOME_SHORE,
      WFC_WORLD2D_BIOME_PLAINS,
      WFC_WORLD2D_BIOME_WOODLAND,
      WFC_WORLD2D_BIOME_ALPINE])
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_LAND);
  FGraph.AddValue(WFC_WORLD2D_BIOME_WOODLAND)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_BIOME_OCEAN,
      WFC_WORLD2D_BIOME_SHORE,
      WFC_WORLD2D_BIOME_PLAINS,
      WFC_WORLD2D_BIOME_WOODLAND,
      WFC_WORLD2D_BIOME_ALPINE])
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_LAND);
  FGraph.AddValue(WFC_WORLD2D_BIOME_ALPINE)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_BIOME_SHORE,
      WFC_WORLD2D_BIOME_PLAINS,
      WFC_WORLD2D_BIOME_WOODLAND,
      WFC_WORLD2D_BIOME_ALPINE])
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_MOUNTAIN);
end;

procedure TSettlement2D.ConfigureRoads;
begin
  FGraph.AddValue(WFC_SETTLEMENT2D_ROADS_NONE, 8)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS, [
      WFC_SETTLEMENT2D_ROADS_NONE,
      WFC_SETTLEMENT2D_ROADS_TRAIL,
      WFC_SETTLEMENT2D_ROADS_BRIDGE,
      WFC_SETTLEMENT2D_ROADS_TUNNEL]);
  FGraph.AddValue(WFC_SETTLEMENT2D_ROADS_TRAIL, 3)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS, [
      WFC_SETTLEMENT2D_ROADS_NONE,
      WFC_SETTLEMENT2D_ROADS_TRAIL,
      WFC_SETTLEMENT2D_ROADS_BRIDGE,
      WFC_SETTLEMENT2D_ROADS_TUNNEL])
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_LAND)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_BIOME, [
      WFC_WORLD2D_BIOME_PLAINS,
      WFC_WORLD2D_BIOME_WOODLAND]);
  FGraph.AddValue(WFC_SETTLEMENT2D_ROADS_BRIDGE, 2)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS, [
      WFC_SETTLEMENT2D_ROADS_NONE,
      WFC_SETTLEMENT2D_ROADS_TRAIL,
      WFC_SETTLEMENT2D_ROADS_BRIDGE])
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_LAND)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      WFC_SETTLEMENT2D_HYDROLOGY_RIVER)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_BIOME, [
      WFC_WORLD2D_BIOME_PLAINS,
      WFC_WORLD2D_BIOME_WOODLAND]);
  FGraph.AddValue(WFC_SETTLEMENT2D_ROADS_TUNNEL, 1)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS, [
      WFC_SETTLEMENT2D_ROADS_NONE,
      WFC_SETTLEMENT2D_ROADS_TRAIL,
      WFC_SETTLEMENT2D_ROADS_TUNNEL])
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_MOUNTAIN)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_BIOME,
      WFC_WORLD2D_BIOME_ALPINE);
end;

procedure TSettlement2D.ConfigureHousing;
begin
  FGraph.AddValue(WFC_SETTLEMENT2D_HOUSING_NONE, 12)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS, [
      WFC_SETTLEMENT2D_HOUSING_NONE,
      WFC_SETTLEMENT2D_HOUSING_HOUSE,
      WFC_SETTLEMENT2D_HOUSING_CABIN,
      WFC_SETTLEMENT2D_HOUSING_LODGE]);
  FGraph.AddValue(WFC_SETTLEMENT2D_HOUSING_HOUSE, 2)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS,
      WFC_SETTLEMENT2D_HOUSING_NONE)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_LAND)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_BIOME,
      WFC_WORLD2D_BIOME_PLAINS)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_ROADS,
      WFC_SETTLEMENT2D_ROADS_TRAIL);
  FGraph.AddValue(WFC_SETTLEMENT2D_HOUSING_CABIN, 2)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS,
      WFC_SETTLEMENT2D_HOUSING_NONE)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_LAND)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_BIOME,
      WFC_WORLD2D_BIOME_WOODLAND)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_ROADS,
      WFC_SETTLEMENT2D_ROADS_TRAIL);
  FGraph.AddValue(WFC_SETTLEMENT2D_HOUSING_LODGE, 1)
    .NewRule(WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS,
      WFC_SETTLEMENT2D_HOUSING_NONE)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_MOUNTAIN)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_BIOME,
      WFC_WORLD2D_BIOME_ALPINE)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_ROADS,
      WFC_SETTLEMENT2D_ROADS_TUNNEL);
end;

procedure TSettlement2D.ConfigureFoliage;
begin
  FGraph.AddValue(WFC_WORLD2D_FOLIAGE_NONE, 8);
  FGraph.AddValue(WFC_WORLD2D_FOLIAGE_REEDS, 2)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_LAND)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      WFC_SETTLEMENT2D_HYDROLOGY_RIVER)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_BIOME, [
      WFC_WORLD2D_BIOME_PLAINS,
      WFC_WORLD2D_BIOME_WOODLAND])
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_ROADS,
      WFC_SETTLEMENT2D_ROADS_NONE)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_HOUSING,
      WFC_SETTLEMENT2D_HOUSING_NONE);
  FGraph.AddValue(WFC_WORLD2D_FOLIAGE_GRASS, 3)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_LAND)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_BIOME,
      WFC_WORLD2D_BIOME_PLAINS)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_ROADS,
      WFC_SETTLEMENT2D_ROADS_NONE)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_HOUSING,
      WFC_SETTLEMENT2D_HOUSING_NONE);
  FGraph.AddValue(WFC_WORLD2D_FOLIAGE_TREE, 2)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_LAND)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_BIOME,
      WFC_WORLD2D_BIOME_WOODLAND)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_ROADS,
      WFC_SETTLEMENT2D_ROADS_NONE)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_HOUSING,
      WFC_SETTLEMENT2D_HOUSING_NONE);
  FGraph.AddValue(WFC_WORLD2D_FOLIAGE_PINE, 2)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_TERRAIN,
      WFC_WORLD2D_TERRAIN_MOUNTAIN)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_BIOME,
      WFC_WORLD2D_BIOME_ALPINE)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_ROADS,
      WFC_SETTLEMENT2D_ROADS_NONE)
    .RequireFromPass(WFC_SETTLEMENT2D_PASS_HOUSING,
      WFC_SETTLEMENT2D_HOUSING_NONE);
end;

function TSettlement2D.GetWidth: TGraphCoordinate;
begin
  Result := FGraph.Dimension.Width;
end;

function TSettlement2D.GetHeight: TGraphCoordinate;
begin
  Result := FGraph.Dimension.Height;
end;

function TSettlement2D.GetSeed: TGraphSeed;
begin
  Result := FGraph.Seed;
end;

procedure TSettlement2D.SetSeed(const AValue: TGraphSeed);
begin
  FGraph.Seed := AValue;
end;

function TSettlement2D.GetWrapNeighbors: Boolean;
begin
  Result := FGraph.WrapNeighbors;
end;

procedure TSettlement2D.SetWrapNeighbors(const AValue: Boolean);
begin
  FGraph.WrapNeighbors := AValue;
end;

function TSettlement2D.GetLayerGraph(
  const ALayer: TSettlement2DLayer): TGraph;
begin
  Result := FGraph.PassGraph[Ord(ALayer)];
end;

procedure TSettlement2D.ValidateCoordinate(
  const AX, AY: TGraphCoordinate);
begin
  if (AX >= Width) or (AY >= Height) then
    raise ERangeError.Create(
      'settlement coordinate is outside its bounds');
end;

function TSettlement2D.GetValue(const ALayer: TSettlement2DLayer;
  const AX, AY: TGraphCoordinate): TGraphValue;
begin
  ValidateCoordinate(AX, AY);
  Result := LayerGraph[ALayer].Entry[AX, AY, 0].Value;
end;

function TSettlement2D.Lock(const ALayer: TSettlement2DLayer;
  const AX, AY: TGraphCoordinate;
  const AValue: TGraphValue): TSettlement2D;
begin
  ValidateCoordinate(AX, AY);
  if not IsSettlement2DLayerValue(ALayer, AValue) then
    raise ESettlement2D.Create(
      'value is not registered for this settlement layer');
  LayerGraph[ALayer].Entry[AX, AY, 0].Value := AValue;
  Result := Self;
end;

function TSettlement2D.ClearLock(const ALayer: TSettlement2DLayer;
  const AX, AY: TGraphCoordinate): TSettlement2D;
begin
  ValidateCoordinate(AX, AY);
  if not LayerGraph[ALayer].Entry[AX, AY, 0].Generated then
    LayerGraph[ALayer].Entry[AX, AY, 0].ClearValue;
  Result := Self;
end;

function TSettlement2D.TryGenerate(const AOptions: TGraphSolveOptions;
  out AReport: TGraphSolveReport): Boolean;
begin
  Result := FGraph.TrySolve(AOptions, AReport);
end;

function TSettlement2D.TryGenerate(
  out AReport: TGraphSolveReport): Boolean;
begin
  Result := TryGenerate(DefaultGraphSolveOptions, AReport);
end;

function TSettlement2D.TryRegenerateFrom(const APass: String;
  const AOptions: TGraphSolveOptions;
  out AReport: TGraphSolveReport): Boolean;
begin
  Result := FGraph.TryRegenerateFrom(APass, AOptions, AReport);
end;

function TSettlement2D.TryRegenerateFrom(const APass: String;
  out AReport: TGraphSolveReport): Boolean;
begin
  Result := TryRegenerateFrom(APass, DefaultGraphSolveOptions, AReport);
end;

function TSettlement2D.TryRegenerateFrom(
  const APasses: TGraphPassLabels;
  const AOptions: TGraphSolveOptions;
  out AReport: TGraphSolveReport): Boolean;
begin
  Result := FGraph.TryRegenerateFrom(APasses, AOptions, AReport);
end;

function ExpectedDependencyCount(
  const ALayer: TSettlement2DLayer): Integer;
begin
  case ALayer of
    s2lTerrain:
      Result := 0;
    s2lHydrology, s2lBiome:
      Result := 1;
    s2lRoads:
      Result := 3;
    s2lHousing:
      Result := 4;
    s2lFoliage:
      Result := 5;
  end;
end;

function ExpectedDependencyIndex(const ALayer: TSettlement2DLayer;
  const AOrdinal: Integer): Integer;
begin
  Result := -1;
  case ALayer of
    s2lTerrain:
      Result := -1;
    s2lHydrology, s2lBiome:
      if AOrdinal = 0 then
        Result := Ord(s2lTerrain);
    s2lRoads:
      case AOrdinal of
        0: Result := Ord(s2lTerrain);
        1: Result := Ord(s2lHydrology);
        2: Result := Ord(s2lBiome);
      end;
    s2lHousing:
      case AOrdinal of
        0: Result := Ord(s2lTerrain);
        1: Result := Ord(s2lHydrology);
        2: Result := Ord(s2lBiome);
        3: Result := Ord(s2lRoads);
      end;
    s2lFoliage:
      case AOrdinal of
        0: Result := Ord(s2lTerrain);
        1: Result := Ord(s2lHydrology);
        2: Result := Ord(s2lBiome);
        3: Result := Ord(s2lRoads);
        4: Result := Ord(s2lHousing);
      end;
  end;
  if Result < 0 then
    raise ERangeError.Create('unknown settlement dependency ordinal');
end;

procedure TSettlement2D.ValidateSignatureShape;
var
  I: Integer;
  LLayer: TSettlement2DLayer;
  LLayerGraph: TGraph;
begin
  if (Width = 0) or (Height = 0) or (FGraph.TotalPassCount <> 6) then
    raise ESettlement2D.Create(
      'the settlement signature requires the complete six-layer DAG');
  for LLayer := Low(TSettlement2DLayer) to High(TSettlement2DLayer) do
  begin
    LLayerGraph := LayerGraph[LLayer];
    if (LLayerGraph.Dimension.Width <> Width)
      or (LLayerGraph.Dimension.Height <> Height)
      or (LLayerGraph.Dimension.Depth <> 1)
      or (LLayerGraph.CurrentPassIndex <> Ord(LLayer))
      or (LLayerGraph.CurrentPass <> Settlement2DLayerName(LLayer))
      or (LLayerGraph.PassMode <> gpmOverlay)
      or (LLayerGraph.DependencyCount <>
        ExpectedDependencyCount(LLayer)) then
      raise ESettlement2D.Create(
        'the settlement signature requires its canonical layer topology');
    for I := 0 to LLayerGraph.DependencyCount - 1 do
      if LLayerGraph.DependencyIndex[I] <>
          ExpectedDependencyIndex(LLayer, I) then
        raise ESettlement2D.Create(
          'the settlement signature requires its canonical dependency DAG');
  end;
end;

function TSettlement2D.LayerSignature(
  const ALayer: TSettlement2DLayer): TSettlement2DSignature;
var
  LEntry: TGraphEntry;
  LHash: Cardinal;
  LHeight: Integer;
  LLayerGraph: TGraph;
  LToken: Byte;
  LWidth: Integer;
  X, Y: Integer;
begin
  ValidateSignatureShape;
  LHash := Cardinal($FFFFFFFF);
  // Little-endian bytes spell the fixed ASCII marker "WFD2".
  MixCardinal(LHash, Cardinal($32444657));
  MixCardinal(LHash, WFC_SETTLEMENT2D_MODEL_VERSION);
  MixCardinal(LHash, WFC_SETTLEMENT2D_SIGNATURE_VERSION);
  MixCardinal(LHash, Cardinal(Ord(ALayer)));
  LWidth := Integer(Width);
  LHeight := Integer(Height);
  MixCardinal(LHash, Cardinal(LWidth));
  MixCardinal(LHash, Cardinal(LHeight));
  LLayerGraph := LayerGraph[ALayer];
  for Y := 0 to Pred(LHeight) do
    for X := 0 to Pred(LWidth) do
    begin
      LEntry := LLayerGraph.Entry[X, Y, 0];
      if LEntry.Empty then
        raise ESettlement2D.Create(
          'cannot sign a settlement layer containing an empty cell');
      if not TrySettlement2DValueToken(ALayer, LEntry.Value, LToken) then
        raise ESettlement2D.Create(
          'cannot sign a settlement layer containing an unknown value');
      LHash := UpdateCrc32(LHash, Cardinal(LToken));
    end;
  Result := not LHash;
end;

function TSettlement2D.PipelineSignature: String;
var
  LLayer: TSettlement2DLayer;
begin
  Result := IntToStr(WFC_SETTLEMENT2D_SIGNATURE_VERSION);
  for LLayer := Low(TSettlement2DLayer) to High(TSettlement2DLayer) do
    Result := Result + ':' + Settlement2DSignatureHex(
      LayerSignature(LLayer));
end;

end.
