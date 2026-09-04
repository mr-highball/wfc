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
unit wfc_world2d;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc;

const
  WFC_WORLD2D_MODEL_VERSION = 1;
  WFC_WORLD2D_SIGNATURE_VERSION = 1;

  WFC_WORLD2D_PASS_TERRAIN = 'terrain';
  WFC_WORLD2D_PASS_BIOME = 'biome';
  WFC_WORLD2D_PASS_FOLIAGE = 'foliage';

  WFC_WORLD2D_TERRAIN_WATER = 'water';
  WFC_WORLD2D_TERRAIN_LAND = 'land';
  WFC_WORLD2D_TERRAIN_MOUNTAIN = 'mountain';

  WFC_WORLD2D_BIOME_OCEAN = 'ocean';
  WFC_WORLD2D_BIOME_SHORE = 'shore';
  WFC_WORLD2D_BIOME_PLAINS = 'plains';
  WFC_WORLD2D_BIOME_WOODLAND = 'woodland';
  WFC_WORLD2D_BIOME_ALPINE = 'alpine';

  WFC_WORLD2D_FOLIAGE_NONE = 'none';
  WFC_WORLD2D_FOLIAGE_REEDS = 'reeds';
  WFC_WORLD2D_FOLIAGE_GRASS = 'grass';
  WFC_WORLD2D_FOLIAGE_TREE = 'tree';
  WFC_WORLD2D_FOLIAGE_PINE = 'pine';

  WFC_WORLD2D_CARDINAL_DIRECTIONS: TGraphDirections =
    [gdNorth, gdEast, gdSouth, gdWest];

type
  EWorld2D = class(Exception);

  TWorld2DLayer = (
    w2lTerrain,
    w2lBiome,
    w2lFoliage
  );

  TWorld2DSignature = Cardinal;
  TWorld2DLayerSignatures = array[TWorld2DLayer] of TWorld2DSignature;

  TWorld2DConfig = record
    Seed: TGraphSeed;
    WrapNeighbors: Boolean;
  end;

  (*
    A ready-to-use three-pass ecology world. The owned TGraph remains exposed
    for advanced extension, while the typed layer API keeps ordinary 2D use
    independent of pass selection.
  *)
  TWorld2D = class
  strict private
    FGraph: TGraph;

    function GetHeight: TGraphCoordinate;
    function GetLayerGraph(const ALayer: TWorld2DLayer): TGraph;
    function GetSeed: TGraphSeed;
    function GetValue(const ALayer: TWorld2DLayer;
      const AX, AY: TGraphCoordinate): TGraphValue;
    function GetWidth: TGraphCoordinate;
    function GetWrapNeighbors: Boolean;
    procedure SetSeed(const AValue: TGraphSeed);
    procedure SetWrapNeighbors(const AValue: Boolean);
    procedure Initialize(const AWidth, AHeight: TGraphCoordinate;
      const AConfig: TWorld2DConfig);
    procedure ValidateCoordinate(const AX, AY: TGraphCoordinate);
    procedure ValidateSignatureShape;

    procedure ConfigureTerrain;
    procedure ConfigureBiomes;
    procedure ConfigureFoliage;
  public
    constructor Create(const AWidth, AHeight: TGraphCoordinate); overload;
    constructor Create(const AWidth, AHeight: TGraphCoordinate;
      const AConfig: TWorld2DConfig); overload;
    destructor Destroy; override;

    function Lock(const ALayer: TWorld2DLayer;
      const AX, AY: TGraphCoordinate;
      const AValue: TGraphValue): TWorld2D;
    function ClearLock(const ALayer: TWorld2DLayer;
      const AX, AY: TGraphCoordinate): TWorld2D;
    function TryGenerate(const AOptions: TGraphSolveOptions;
      out AReport: TGraphSolveReport): Boolean; overload;
    function TryGenerate(out AReport: TGraphSolveReport): Boolean; overload;

    function LayerSignature(const ALayer: TWorld2DLayer): TWorld2DSignature;
    function PipelineSignature: String;

    property Graph: TGraph read FGraph;
    property LayerGraph[const ALayer: TWorld2DLayer]: TGraph
      read GetLayerGraph;
    property Value[const ALayer: TWorld2DLayer;
      const AX, AY: TGraphCoordinate]: TGraphValue read GetValue;
    property Width: TGraphCoordinate read GetWidth;
    property Height: TGraphCoordinate read GetHeight;
    property Seed: TGraphSeed read GetSeed write SetSeed;
    property WrapNeighbors: Boolean read GetWrapNeighbors
      write SetWrapNeighbors;
  end;

function World2DLayerName(const ALayer: TWorld2DLayer): String;
function World2DSignatureHex(const ASignature: TWorld2DSignature): String;
function DefaultWorld2DConfig: TWorld2DConfig;
function IsWorld2DLayerValue(const ALayer: TWorld2DLayer;
  const AValue: TGraphValue): Boolean;

implementation

const
  CRC32_POLYNOMIAL = Cardinal($EDB88320);
  HEX_DIGITS = '0123456789ABCDEF';

function World2DLayerName(const ALayer: TWorld2DLayer): String;
begin
  case ALayer of
    w2lTerrain:
      Result := WFC_WORLD2D_PASS_TERRAIN;
    w2lBiome:
      Result := WFC_WORLD2D_PASS_BIOME;
    w2lFoliage:
      Result := WFC_WORLD2D_PASS_FOLIAGE;
  else
    raise ERangeError.Create('unknown 2D world layer');
  end;
end;

function DefaultWorld2DConfig: TWorld2DConfig;
begin
  Result.Seed := 0;
  Result.WrapNeighbors := False;
end;

function TryWorld2DValueToken(const ALayer: TWorld2DLayer;
  const AValue: TGraphValue; out AToken: Byte): Boolean;
begin
  Result := True;
  case ALayer of
    w2lTerrain:
      if AValue = WFC_WORLD2D_TERRAIN_WATER then
        AToken := $10
      else if AValue = WFC_WORLD2D_TERRAIN_LAND then
        AToken := $11
      else if AValue = WFC_WORLD2D_TERRAIN_MOUNTAIN then
        AToken := $12
      else
        Result := False;
    w2lBiome:
      if AValue = WFC_WORLD2D_BIOME_OCEAN then
        AToken := $20
      else if AValue = WFC_WORLD2D_BIOME_SHORE then
        AToken := $21
      else if AValue = WFC_WORLD2D_BIOME_PLAINS then
        AToken := $22
      else if AValue = WFC_WORLD2D_BIOME_WOODLAND then
        AToken := $23
      else if AValue = WFC_WORLD2D_BIOME_ALPINE then
        AToken := $24
      else
        Result := False;
    w2lFoliage:
      if AValue = WFC_WORLD2D_FOLIAGE_NONE then
        AToken := $30
      else if AValue = WFC_WORLD2D_FOLIAGE_REEDS then
        AToken := $31
      else if AValue = WFC_WORLD2D_FOLIAGE_GRASS then
        AToken := $32
      else if AValue = WFC_WORLD2D_FOLIAGE_TREE then
        AToken := $33
      else if AValue = WFC_WORLD2D_FOLIAGE_PINE then
        AToken := $34
      else
        Result := False;
  else
    Result := False;
  end;
end;

function IsWorld2DLayerValue(const ALayer: TWorld2DLayer;
  const AValue: TGraphValue): Boolean;
var
  LToken: Byte;
begin
  Result := TryWorld2DValueToken(ALayer, AValue, LToken);
end;

function World2DSignatureHex(const ASignature: TWorld2DSignature): String;
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

constructor TWorld2D.Create(const AWidth, AHeight: TGraphCoordinate);
begin
  inherited Create;
  Initialize(AWidth, AHeight, DefaultWorld2DConfig);
end;

constructor TWorld2D.Create(const AWidth, AHeight: TGraphCoordinate;
  const AConfig: TWorld2DConfig);
begin
  inherited Create;
  Initialize(AWidth, AHeight, AConfig);
end;

procedure TWorld2D.Initialize(const AWidth, AHeight: TGraphCoordinate;
  const AConfig: TWorld2DConfig);
begin
  if (AWidth = 0) or (AHeight = 0) then
    raise ERangeError.Create('a 2D world needs positive width and height');

  FGraph := TGraph.Create;
  try
    FGraph.Seed := AConfig.Seed;
    FGraph.Reshape(AWidth, AHeight, 1);
    FGraph.WrapNeighbors := AConfig.WrapNeighbors;
    FGraph.CurrentPass := WFC_WORLD2D_PASS_TERRAIN;
    ConfigureTerrain;
    FGraph.SwitchToPass(WFC_WORLD2D_PASS_BIOME);
    ConfigureBiomes;
    FGraph.SwitchToPass(WFC_WORLD2D_PASS_FOLIAGE);
    ConfigureFoliage;
    FGraph.SwitchToPass(WFC_WORLD2D_PASS_TERRAIN);
  except
    FGraph.Free;
    FGraph := nil;
    raise;
  end;
end;

destructor TWorld2D.Destroy;
begin
  FGraph.Free;
  inherited Destroy;
end;

procedure TWorld2D.ConfigureTerrain;
begin
  FGraph.AddValue(WFC_WORLD2D_TERRAIN_WATER)
    .NewRule(WFC_WORLD2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_TERRAIN_WATER,
      WFC_WORLD2D_TERRAIN_LAND]);
  FGraph.AddValue(WFC_WORLD2D_TERRAIN_LAND)
    .NewRule(WFC_WORLD2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_TERRAIN_WATER,
      WFC_WORLD2D_TERRAIN_LAND,
      WFC_WORLD2D_TERRAIN_MOUNTAIN]);
  FGraph.AddValue(WFC_WORLD2D_TERRAIN_MOUNTAIN)
    .NewRule(WFC_WORLD2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_TERRAIN_LAND,
      WFC_WORLD2D_TERRAIN_MOUNTAIN]);
end;

procedure TWorld2D.ConfigureBiomes;
begin
  FGraph.AddValue(WFC_WORLD2D_BIOME_OCEAN)
    .NewRule(WFC_WORLD2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_BIOME_OCEAN,
      WFC_WORLD2D_BIOME_SHORE,
      WFC_WORLD2D_BIOME_PLAINS,
      WFC_WORLD2D_BIOME_WOODLAND])
    .RequirePrevious(WFC_WORLD2D_TERRAIN_WATER);
  FGraph.AddValue(WFC_WORLD2D_BIOME_SHORE)
    .NewRule(WFC_WORLD2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_BIOME_OCEAN,
      WFC_WORLD2D_BIOME_SHORE,
      WFC_WORLD2D_BIOME_PLAINS,
      WFC_WORLD2D_BIOME_WOODLAND,
      WFC_WORLD2D_BIOME_ALPINE])
    .RequirePrevious(WFC_WORLD2D_TERRAIN_LAND);
  FGraph.AddValue(WFC_WORLD2D_BIOME_PLAINS)
    .NewRule(WFC_WORLD2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_BIOME_OCEAN,
      WFC_WORLD2D_BIOME_SHORE,
      WFC_WORLD2D_BIOME_PLAINS,
      WFC_WORLD2D_BIOME_WOODLAND,
      WFC_WORLD2D_BIOME_ALPINE])
    .RequirePrevious(WFC_WORLD2D_TERRAIN_LAND);
  FGraph.AddValue(WFC_WORLD2D_BIOME_WOODLAND)
    .NewRule(WFC_WORLD2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_BIOME_OCEAN,
      WFC_WORLD2D_BIOME_SHORE,
      WFC_WORLD2D_BIOME_PLAINS,
      WFC_WORLD2D_BIOME_WOODLAND,
      WFC_WORLD2D_BIOME_ALPINE])
    .RequirePrevious(WFC_WORLD2D_TERRAIN_LAND);
  FGraph.AddValue(WFC_WORLD2D_BIOME_ALPINE)
    .NewRule(WFC_WORLD2D_CARDINAL_DIRECTIONS, [
      WFC_WORLD2D_BIOME_SHORE,
      WFC_WORLD2D_BIOME_PLAINS,
      WFC_WORLD2D_BIOME_WOODLAND,
      WFC_WORLD2D_BIOME_ALPINE])
    .RequirePrevious(WFC_WORLD2D_TERRAIN_MOUNTAIN);
end;

procedure TWorld2D.ConfigureFoliage;
begin
  FGraph.AddValue(WFC_WORLD2D_FOLIAGE_NONE)
    .RequirePrevious([
      WFC_WORLD2D_BIOME_OCEAN,
      WFC_WORLD2D_BIOME_SHORE,
      WFC_WORLD2D_BIOME_PLAINS,
      WFC_WORLD2D_BIOME_WOODLAND,
      WFC_WORLD2D_BIOME_ALPINE]);
  FGraph.AddValue(WFC_WORLD2D_FOLIAGE_REEDS)
    .RequirePrevious(WFC_WORLD2D_BIOME_SHORE);
  FGraph.AddValue(WFC_WORLD2D_FOLIAGE_GRASS)
    .RequirePrevious(WFC_WORLD2D_BIOME_PLAINS);
  FGraph.AddValue(WFC_WORLD2D_FOLIAGE_TREE)
    .RequirePrevious(WFC_WORLD2D_BIOME_WOODLAND);
  FGraph.AddValue(WFC_WORLD2D_FOLIAGE_PINE)
    .RequirePrevious(WFC_WORLD2D_BIOME_ALPINE);
end;

function TWorld2D.GetWidth: TGraphCoordinate;
begin
  Result := FGraph.Dimension.Width;
end;

function TWorld2D.GetHeight: TGraphCoordinate;
begin
  Result := FGraph.Dimension.Height;
end;

function TWorld2D.GetSeed: TGraphSeed;
begin
  Result := FGraph.Seed;
end;

procedure TWorld2D.SetSeed(const AValue: TGraphSeed);
begin
  FGraph.Seed := AValue;
end;

function TWorld2D.GetWrapNeighbors: Boolean;
begin
  Result := FGraph.WrapNeighbors;
end;

procedure TWorld2D.SetWrapNeighbors(const AValue: Boolean);
begin
  FGraph.WrapNeighbors := AValue;
end;

function TWorld2D.GetLayerGraph(const ALayer: TWorld2DLayer): TGraph;
begin
  Result := FGraph.PassGraph[Ord(ALayer)];
end;

procedure TWorld2D.ValidateCoordinate(const AX, AY: TGraphCoordinate);
begin
  if (AX >= Width) or (AY >= Height) then
    raise ERangeError.Create('2D world coordinate is outside its bounds');
end;

function TWorld2D.GetValue(const ALayer: TWorld2DLayer;
  const AX, AY: TGraphCoordinate): TGraphValue;
begin
  ValidateCoordinate(AX, AY);
  Result := LayerGraph[ALayer].Entry[AX, AY, 0].Value;
end;

function TWorld2D.Lock(const ALayer: TWorld2DLayer;
  const AX, AY: TGraphCoordinate;
  const AValue: TGraphValue): TWorld2D;
begin
  ValidateCoordinate(AX, AY);
  if not IsWorld2DLayerValue(ALayer, AValue) then
    raise EWorld2D.Create('value is not registered for this 2D layer');
  LayerGraph[ALayer].Entry[AX, AY, 0].Value := AValue;
  Result := Self;
end;

function TWorld2D.ClearLock(const ALayer: TWorld2DLayer;
  const AX, AY: TGraphCoordinate): TWorld2D;
begin
  ValidateCoordinate(AX, AY);
  if not LayerGraph[ALayer].Entry[AX, AY, 0].Generated then
    LayerGraph[ALayer].Entry[AX, AY, 0].ClearValue;
  Result := Self;
end;

function TWorld2D.TryGenerate(const AOptions: TGraphSolveOptions;
  out AReport: TGraphSolveReport): Boolean;
begin
  Result := FGraph.TrySolve(AOptions, AReport);
end;

function TWorld2D.TryGenerate(out AReport: TGraphSolveReport): Boolean;
begin
  Result := TryGenerate(DefaultGraphSolveOptions, AReport);
end;

procedure TWorld2D.ValidateSignatureShape;
var
  LLayer: TWorld2DLayer;
  LLayerGraph: TGraph;
begin
  if (Width = 0) or (Height = 0) or (FGraph.TotalPassCount <> 3) then
    raise EWorld2D.Create(
      'the 2D signature requires the complete standard three-pass pipeline');
  for LLayer := Low(TWorld2DLayer) to High(TWorld2DLayer) do
  begin
    LLayerGraph := LayerGraph[LLayer];
    if (LLayerGraph.Dimension.Width <> Width)
      or (LLayerGraph.Dimension.Height <> Height)
      or (LLayerGraph.Dimension.Depth <> 1) then
      raise EWorld2D.Create(
        'the 2D signature requires matching depth-one layer shapes');
  end;
end;

function TWorld2D.LayerSignature(
  const ALayer: TWorld2DLayer): TWorld2DSignature;
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
  //Little-endian bytes spell the fixed ASCII marker "WFC2".
  MixCardinal(LHash, Cardinal($32434657));
  MixCardinal(LHash, WFC_WORLD2D_MODEL_VERSION);
  MixCardinal(LHash, WFC_WORLD2D_SIGNATURE_VERSION);
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
        raise EWorld2D.Create(
          'cannot sign a 2D layer that contains an empty cell');
      if not TryWorld2DValueToken(ALayer, LEntry.Value, LToken) then
        raise EWorld2D.Create(
          'cannot sign a 2D layer that contains an unknown value');
      LHash := UpdateCrc32(LHash, Cardinal(LToken));
    end;
  Result := not LHash;
end;

function TWorld2D.PipelineSignature: String;
begin
  Result := IntToStr(WFC_WORLD2D_SIGNATURE_VERSION)
    + ':' + World2DSignatureHex(LayerSignature(w2lTerrain))
    + ':' + World2DSignatureHex(LayerSignature(w2lBiome))
    + ':' + World2DSignatureHex(LayerSignature(w2lFoliage));
end;

end.
