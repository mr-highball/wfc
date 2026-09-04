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
unit wfc_world2d_settlement_validate;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_world2d,
  wfc_world2d_settlement;

type
  TSettlement2DValidationIssueKind = (
    s2vikNone,
    s2vikPipelineTopology,
    s2vikPipelineShape,
    s2vikEmptyCell,
    s2vikUnknownValue,
    s2vikTerrainAdjacency,
    s2vikHydrologyTerrain,
    s2vikBiomeTerrain,
    s2vikBiomeAdjacency,
    s2vikRoadContext,
    s2vikRoadAdjacency,
    s2vikHousingContext,
    s2vikHousingAdjacency,
    s2vikFoliageContext
  );

  TSettlement2DValidationIssue = record
    Kind: TSettlement2DValidationIssueKind;
    Layer: TSettlement2DLayer;
    X: Integer;
    Y: Integer;
    NeighborX: Integer;
    NeighborY: Integer;
    HasDirection: Boolean;
    Direction: TGraphDirection;
    HasRelatedLayer: Boolean;
    RelatedLayer: TSettlement2DLayer;
    Value: TGraphValue;
    RelatedValue: TGraphValue;
  end;

  TSettlement2DValidationReport = record
    Valid: Boolean;
    CheckedCells: Integer;
    CheckedRelations: Integer;
    Issue: TSettlement2DValidationIssue;
  end;

function ValidateSettlement2D(const AWorld: TSettlement2D;
  out AReport: TSettlement2DValidationReport): Boolean;
function Settlement2DValidationIssueName(
  const AKind: TSettlement2DValidationIssueKind): String;
function DescribeSettlement2DValidationIssue(
  const AIssue: TSettlement2DValidationIssue): String;

implementation

function IsTerrainValue(const AValue: TGraphValue): Boolean;
begin
  Result := (AValue = WFC_WORLD2D_TERRAIN_WATER)
    or (AValue = WFC_WORLD2D_TERRAIN_LAND)
    or (AValue = WFC_WORLD2D_TERRAIN_MOUNTAIN);
end;

function TerrainAllows(const ACurrent, ANeighbor: TGraphValue): Boolean;
begin
  if ACurrent = WFC_WORLD2D_TERRAIN_WATER then
    Result := (ANeighbor = WFC_WORLD2D_TERRAIN_WATER)
      or (ANeighbor = WFC_WORLD2D_TERRAIN_LAND)
  else if ACurrent = WFC_WORLD2D_TERRAIN_LAND then
    Result := IsTerrainValue(ANeighbor)
  else if ACurrent = WFC_WORLD2D_TERRAIN_MOUNTAIN then
    Result := (ANeighbor = WFC_WORLD2D_TERRAIN_LAND)
      or (ANeighbor = WFC_WORLD2D_TERRAIN_MOUNTAIN)
  else
    Result := False;
end;

function HydrologyMatchesTerrain(const AHydrology,
  ATerrain: TGraphValue): Boolean;
begin
  if AHydrology = WFC_SETTLEMENT2D_HYDROLOGY_SEA then
    Result := ATerrain = WFC_WORLD2D_TERRAIN_WATER
  else if AHydrology = WFC_SETTLEMENT2D_HYDROLOGY_DRY then
    Result := (ATerrain = WFC_WORLD2D_TERRAIN_LAND)
      or (ATerrain = WFC_WORLD2D_TERRAIN_MOUNTAIN)
  else if AHydrology = WFC_SETTLEMENT2D_HYDROLOGY_RIVER then
    Result := ATerrain = WFC_WORLD2D_TERRAIN_LAND
  else
    Result := False;
end;

function IsBiomeValue(const AValue: TGraphValue): Boolean;
begin
  Result := (AValue = WFC_WORLD2D_BIOME_OCEAN)
    or (AValue = WFC_WORLD2D_BIOME_SHORE)
    or (AValue = WFC_WORLD2D_BIOME_PLAINS)
    or (AValue = WFC_WORLD2D_BIOME_WOODLAND)
    or (AValue = WFC_WORLD2D_BIOME_ALPINE);
end;

function BiomeMatchesTerrain(const ABiome,
  ATerrain: TGraphValue): Boolean;
begin
  if ABiome = WFC_WORLD2D_BIOME_OCEAN then
    Result := ATerrain = WFC_WORLD2D_TERRAIN_WATER
  else if (ABiome = WFC_WORLD2D_BIOME_SHORE)
    or (ABiome = WFC_WORLD2D_BIOME_PLAINS)
    or (ABiome = WFC_WORLD2D_BIOME_WOODLAND) then
    Result := ATerrain = WFC_WORLD2D_TERRAIN_LAND
  else if ABiome = WFC_WORLD2D_BIOME_ALPINE then
    Result := ATerrain = WFC_WORLD2D_TERRAIN_MOUNTAIN
  else
    Result := False;
end;

function BiomeAllows(const ACurrent, ANeighbor: TGraphValue): Boolean;
begin
  if ACurrent = WFC_WORLD2D_BIOME_OCEAN then
    Result := (ANeighbor = WFC_WORLD2D_BIOME_OCEAN)
      or (ANeighbor = WFC_WORLD2D_BIOME_SHORE)
      or (ANeighbor = WFC_WORLD2D_BIOME_PLAINS)
      or (ANeighbor = WFC_WORLD2D_BIOME_WOODLAND)
  else if ACurrent = WFC_WORLD2D_BIOME_SHORE then
    Result := IsBiomeValue(ANeighbor)
  else if ACurrent = WFC_WORLD2D_BIOME_PLAINS then
    Result := IsBiomeValue(ANeighbor)
  else if ACurrent = WFC_WORLD2D_BIOME_WOODLAND then
    Result := IsBiomeValue(ANeighbor)
  else if ACurrent = WFC_WORLD2D_BIOME_ALPINE then
    Result := (ANeighbor = WFC_WORLD2D_BIOME_SHORE)
      or (ANeighbor = WFC_WORLD2D_BIOME_PLAINS)
      or (ANeighbor = WFC_WORLD2D_BIOME_WOODLAND)
      or (ANeighbor = WFC_WORLD2D_BIOME_ALPINE)
  else
    Result := False;
end;

function IsRoadValue(const AValue: TGraphValue): Boolean;
begin
  Result := (AValue = WFC_SETTLEMENT2D_ROADS_NONE)
    or (AValue = WFC_SETTLEMENT2D_ROADS_TRAIL)
    or (AValue = WFC_SETTLEMENT2D_ROADS_BRIDGE)
    or (AValue = WFC_SETTLEMENT2D_ROADS_TUNNEL);
end;

function RoadMatchesContext(const ARoad, ATerrain, AHydrology,
  ABiome: TGraphValue; out ARelatedLayer: TSettlement2DLayer): Boolean;
begin
  ARelatedLayer := s2lTerrain;
  if ARoad = WFC_SETTLEMENT2D_ROADS_NONE then
    Exit(True);

  if ARoad = WFC_SETTLEMENT2D_ROADS_TRAIL then
  begin
    if ATerrain <> WFC_WORLD2D_TERRAIN_LAND then
      Exit(False);
    ARelatedLayer := s2lHydrology;
    if AHydrology <> WFC_SETTLEMENT2D_HYDROLOGY_DRY then
      Exit(False);
    ARelatedLayer := s2lBiome;
    Exit((ABiome = WFC_WORLD2D_BIOME_PLAINS)
      or (ABiome = WFC_WORLD2D_BIOME_WOODLAND));
  end;

  if ARoad = WFC_SETTLEMENT2D_ROADS_BRIDGE then
  begin
    if ATerrain <> WFC_WORLD2D_TERRAIN_LAND then
      Exit(False);
    ARelatedLayer := s2lHydrology;
    if AHydrology <> WFC_SETTLEMENT2D_HYDROLOGY_RIVER then
      Exit(False);
    ARelatedLayer := s2lBiome;
    Exit((ABiome = WFC_WORLD2D_BIOME_PLAINS)
      or (ABiome = WFC_WORLD2D_BIOME_WOODLAND));
  end;

  if ARoad = WFC_SETTLEMENT2D_ROADS_TUNNEL then
  begin
    if ATerrain <> WFC_WORLD2D_TERRAIN_MOUNTAIN then
      Exit(False);
    ARelatedLayer := s2lHydrology;
    if AHydrology <> WFC_SETTLEMENT2D_HYDROLOGY_DRY then
      Exit(False);
    ARelatedLayer := s2lBiome;
    Exit(ABiome = WFC_WORLD2D_BIOME_ALPINE);
  end;
  Result := False;
end;

function RoadAllows(const ACurrent, ANeighbor: TGraphValue): Boolean;
begin
  Result := IsRoadValue(ACurrent) and IsRoadValue(ANeighbor)
    and not (((ACurrent = WFC_SETTLEMENT2D_ROADS_BRIDGE)
      and (ANeighbor = WFC_SETTLEMENT2D_ROADS_TUNNEL))
      or ((ACurrent = WFC_SETTLEMENT2D_ROADS_TUNNEL)
      and (ANeighbor = WFC_SETTLEMENT2D_ROADS_BRIDGE)));
end;

function IsHousingValue(const AValue: TGraphValue): Boolean;
begin
  Result := (AValue = WFC_SETTLEMENT2D_HOUSING_NONE)
    or (AValue = WFC_SETTLEMENT2D_HOUSING_HOUSE)
    or (AValue = WFC_SETTLEMENT2D_HOUSING_CABIN)
    or (AValue = WFC_SETTLEMENT2D_HOUSING_LODGE);
end;

function HousingMatchesContext(const AHousing, ATerrain, AHydrology,
  ABiome, ARoad: TGraphValue;
  out ARelatedLayer: TSettlement2DLayer): Boolean;
begin
  ARelatedLayer := s2lTerrain;
  if AHousing = WFC_SETTLEMENT2D_HOUSING_NONE then
    Exit(True);

  if AHousing = WFC_SETTLEMENT2D_HOUSING_LODGE then
  begin
    if ATerrain <> WFC_WORLD2D_TERRAIN_MOUNTAIN then
      Exit(False);
    ARelatedLayer := s2lHydrology;
    if AHydrology <> WFC_SETTLEMENT2D_HYDROLOGY_DRY then
      Exit(False);
    ARelatedLayer := s2lBiome;
    if ABiome <> WFC_WORLD2D_BIOME_ALPINE then
      Exit(False);
    ARelatedLayer := s2lRoads;
    Exit(ARoad = WFC_SETTLEMENT2D_ROADS_TUNNEL);
  end;

  if ATerrain <> WFC_WORLD2D_TERRAIN_LAND then
    Exit(False);
  ARelatedLayer := s2lHydrology;
  if AHydrology <> WFC_SETTLEMENT2D_HYDROLOGY_DRY then
    Exit(False);
  ARelatedLayer := s2lBiome;
  if (AHousing = WFC_SETTLEMENT2D_HOUSING_HOUSE)
    and (ABiome <> WFC_WORLD2D_BIOME_PLAINS) then
    Exit(False);
  if (AHousing = WFC_SETTLEMENT2D_HOUSING_CABIN)
    and (ABiome <> WFC_WORLD2D_BIOME_WOODLAND) then
    Exit(False);
  if (AHousing <> WFC_SETTLEMENT2D_HOUSING_HOUSE)
    and (AHousing <> WFC_SETTLEMENT2D_HOUSING_CABIN) then
    Exit(False);
  ARelatedLayer := s2lRoads;
  Result := ARoad = WFC_SETTLEMENT2D_ROADS_TRAIL;
end;

function HousingAllows(const ACurrent, ANeighbor: TGraphValue): Boolean;
begin
  Result := IsHousingValue(ACurrent) and IsHousingValue(ANeighbor)
    and ((ACurrent = WFC_SETTLEMENT2D_HOUSING_NONE)
      or (ANeighbor = WFC_SETTLEMENT2D_HOUSING_NONE));
end;

function FoliageMatchesContext(const AFoliage, ATerrain, AHydrology,
  ABiome, ARoad, AHousing: TGraphValue;
  out ARelatedLayer: TSettlement2DLayer): Boolean;
begin
  ARelatedLayer := s2lTerrain;
  if AFoliage = WFC_WORLD2D_FOLIAGE_NONE then
    Exit(True);

  if AFoliage = WFC_WORLD2D_FOLIAGE_PINE then
  begin
    if ATerrain <> WFC_WORLD2D_TERRAIN_MOUNTAIN then
      Exit(False);
  end
  else if ATerrain <> WFC_WORLD2D_TERRAIN_LAND then
    Exit(False);

  ARelatedLayer := s2lHydrology;
  if AFoliage = WFC_WORLD2D_FOLIAGE_REEDS then
  begin
    if AHydrology <> WFC_SETTLEMENT2D_HYDROLOGY_RIVER then
      Exit(False);
  end
  else if AHydrology <> WFC_SETTLEMENT2D_HYDROLOGY_DRY then
    Exit(False);

  ARelatedLayer := s2lBiome;
  if (AFoliage = WFC_WORLD2D_FOLIAGE_REEDS)
    and (ABiome <> WFC_WORLD2D_BIOME_PLAINS)
    and (ABiome <> WFC_WORLD2D_BIOME_WOODLAND) then
    Exit(False)
  else if (AFoliage = WFC_WORLD2D_FOLIAGE_GRASS)
    and (ABiome <> WFC_WORLD2D_BIOME_PLAINS) then
    Exit(False)
  else if (AFoliage = WFC_WORLD2D_FOLIAGE_TREE)
    and (ABiome <> WFC_WORLD2D_BIOME_WOODLAND) then
    Exit(False)
  else if (AFoliage = WFC_WORLD2D_FOLIAGE_PINE)
    and (ABiome <> WFC_WORLD2D_BIOME_ALPINE) then
    Exit(False)
  else if (AFoliage <> WFC_WORLD2D_FOLIAGE_REEDS)
    and (AFoliage <> WFC_WORLD2D_FOLIAGE_GRASS)
    and (AFoliage <> WFC_WORLD2D_FOLIAGE_TREE)
    and (AFoliage <> WFC_WORLD2D_FOLIAGE_PINE) then
    Exit(False);

  ARelatedLayer := s2lRoads;
  if ARoad <> WFC_SETTLEMENT2D_ROADS_NONE then
    Exit(False);
  ARelatedLayer := s2lHousing;
  Result := AHousing = WFC_SETTLEMENT2D_HOUSING_NONE;
end;

procedure InitializeReport(out AReport: TSettlement2DValidationReport);
begin
  AReport := Default(TSettlement2DValidationReport);
  AReport.Valid := False;
  AReport.Issue.Kind := s2vikNone;
  AReport.Issue.Layer := s2lTerrain;
  AReport.Issue.X := -1;
  AReport.Issue.Y := -1;
  AReport.Issue.NeighborX := -1;
  AReport.Issue.NeighborY := -1;
  AReport.Issue.Direction := gdNorth;
  AReport.Issue.RelatedLayer := s2lTerrain;
end;

procedure SetIssue(var AReport: TSettlement2DValidationReport;
  const AKind: TSettlement2DValidationIssueKind;
  const ALayer: TSettlement2DLayer; const AEntry: TGraphEntry;
  const AValue: TGraphValue);
begin
  AReport.Valid := False;
  AReport.Issue.Kind := AKind;
  AReport.Issue.Layer := ALayer;
  AReport.Issue.Value := AValue;
  if Assigned(AEntry) then
  begin
    AReport.Issue.X := Integer(AEntry.Position.X);
    AReport.Issue.Y := Integer(AEntry.Position.Y);
  end;
end;

procedure SetRelatedIssue(var AReport: TSettlement2DValidationReport;
  const AKind: TSettlement2DValidationIssueKind;
  const ALayer, ARelatedLayer: TSettlement2DLayer;
  const AEntry, ARelatedEntry: TGraphEntry;
  const AValue, ARelatedValue: TGraphValue);
begin
  SetIssue(AReport, AKind, ALayer, AEntry, AValue);
  AReport.Issue.HasRelatedLayer := True;
  AReport.Issue.RelatedLayer := ARelatedLayer;
  AReport.Issue.RelatedValue := ARelatedValue;
  if Assigned(ARelatedEntry) then
  begin
    AReport.Issue.NeighborX := Integer(ARelatedEntry.Position.X);
    AReport.Issue.NeighborY := Integer(ARelatedEntry.Position.Y);
  end;
end;

function ExpectedDependencyCount(
  const ALayer: TSettlement2DLayer): Integer;
begin
  Result := -1;
  case ALayer of
    s2lTerrain: Result := 0;
    s2lHydrology, s2lBiome: Result := 1;
    s2lRoads: Result := 3;
    s2lHousing: Result := 4;
    s2lFoliage: Result := 5;
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
      if AOrdinal = 0 then Result := Ord(s2lTerrain);
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
end;

function ValidatePipeline(const AWorld: TSettlement2D;
  out AKind: TSettlement2DValidationIssueKind;
  out ALayer: TSettlement2DLayer): Boolean;
var
  I: Integer;
  LGraph: TGraph;
  LLayer: TSettlement2DLayer;
begin
  Result := False;
  AKind := s2vikPipelineTopology;
  ALayer := s2lTerrain;
  if not Assigned(AWorld) then
    Exit;
  if AWorld.Graph.TotalPassCount <> 6 then
    Exit;
  if (AWorld.Width = 0) or (AWorld.Height = 0) then
  begin
    AKind := s2vikPipelineShape;
    Exit;
  end;
  for LLayer := Low(TSettlement2DLayer) to High(TSettlement2DLayer) do
  begin
    ALayer := LLayer;
    LGraph := AWorld.LayerGraph[LLayer];
    if (LGraph.Dimension.Width <> AWorld.Width)
      or (LGraph.Dimension.Height <> AWorld.Height)
      or (LGraph.Dimension.Depth <> 1) then
    begin
      AKind := s2vikPipelineShape;
      Exit;
    end;
    if (LGraph.CurrentPassIndex <> Ord(LLayer))
      or (LGraph.CurrentPass <> Settlement2DLayerName(LLayer))
      or (LGraph.PassMode <> gpmOverlay)
      or (LGraph.DependencyCount <> ExpectedDependencyCount(LLayer)) then
      Exit;
    for I := 0 to LGraph.DependencyCount - 1 do
      if LGraph.DependencyIndex[I] <>
          ExpectedDependencyIndex(LLayer, I) then
        Exit;
  end;
  Result := True;
end;

function TryGetNeighborCoordinate(const AWorld: TSettlement2D;
  const AX, AY: TGraphCoordinate; const ADirection: TGraphDirection;
  out ANeighborX, ANeighborY: TGraphCoordinate): Boolean;
begin
  ANeighborX := AX;
  ANeighborY := AY;
  Result := True;
  case ADirection of
    gdNorth:
      if AY + 1 < AWorld.Height then
        ANeighborY := AY + 1
      else if AWorld.WrapNeighbors then
        ANeighborY := 0
      else
        Result := False;
    gdEast:
      if AX + 1 < AWorld.Width then
        ANeighborX := AX + 1
      else if AWorld.WrapNeighbors then
        ANeighborX := 0
      else
        Result := False;
    gdSouth:
      if AY > 0 then
        ANeighborY := AY - 1
      else if AWorld.WrapNeighbors then
        ANeighborY := AWorld.Height - 1
      else
        Result := False;
    gdWest:
      if AX > 0 then
        ANeighborX := AX - 1
      else if AWorld.WrapNeighbors then
        ANeighborX := AWorld.Width - 1
      else
        Result := False;
  else
    Result := False;
  end;
end;

function RelatedValueAt(const AWorld: TSettlement2D;
  const ALayer: TSettlement2DLayer;
  const AX, AY: TGraphCoordinate): TGraphValue;
begin
  Result := AWorld.Value[ALayer, AX, AY];
end;

function ValidateSettlement2D(const AWorld: TSettlement2D;
  out AReport: TSettlement2DValidationReport): Boolean;
var
  LBiome: TGraphEntry;
  LDirection: TGraphDirection;
  LEntry: TGraphEntry;
  LFoliage: TGraphEntry;
  LHousing: TGraphEntry;
  LHydrology: TGraphEntry;
  LIssueKind: TSettlement2DValidationIssueKind;
  LLayer: TSettlement2DLayer;
  LNeighbor: TGraphEntry;
  LNeighborX: TGraphCoordinate;
  LNeighborY: TGraphCoordinate;
  LRelatedLayer: TSettlement2DLayer;
  LRoad: TGraphEntry;
  LTerrain: TGraphEntry;
  X, Y: Integer;
begin
  InitializeReport(AReport);
  Result := False;
  if not ValidatePipeline(AWorld, LIssueKind, LLayer) then
  begin
    SetIssue(AReport, LIssueKind, LLayer, nil, '');
    Exit;
  end;

  for Y := 0 to Integer(AWorld.Height) - 1 do
    for X := 0 to Integer(AWorld.Width) - 1 do
    begin
      LTerrain := AWorld.LayerGraph[s2lTerrain].Entry[X, Y, 0];
      LHydrology := AWorld.LayerGraph[s2lHydrology].Entry[X, Y, 0];
      LBiome := AWorld.LayerGraph[s2lBiome].Entry[X, Y, 0];
      LRoad := AWorld.LayerGraph[s2lRoads].Entry[X, Y, 0];
      LHousing := AWorld.LayerGraph[s2lHousing].Entry[X, Y, 0];
      LFoliage := AWorld.LayerGraph[s2lFoliage].Entry[X, Y, 0];

      for LLayer := Low(TSettlement2DLayer) to High(TSettlement2DLayer) do
      begin
        LEntry := AWorld.LayerGraph[LLayer].Entry[X, Y, 0];
        Inc(AReport.CheckedCells);
        if LEntry.Empty then
        begin
          SetIssue(AReport, s2vikEmptyCell, LLayer, LEntry, '');
          Exit;
        end;
        if not IsSettlement2DLayerValue(LLayer, LEntry.Value) then
        begin
          SetIssue(AReport, s2vikUnknownValue, LLayer,
            LEntry, LEntry.Value);
          Exit;
        end;
      end;

      Inc(AReport.CheckedRelations);
      if not HydrologyMatchesTerrain(LHydrology.Value, LTerrain.Value) then
      begin
        SetRelatedIssue(AReport, s2vikHydrologyTerrain,
          s2lHydrology, s2lTerrain, LHydrology, LTerrain,
          LHydrology.Value, LTerrain.Value);
        Exit;
      end;

      Inc(AReport.CheckedRelations);
      if not BiomeMatchesTerrain(LBiome.Value, LTerrain.Value) then
      begin
        SetRelatedIssue(AReport, s2vikBiomeTerrain,
          s2lBiome, s2lTerrain, LBiome, LTerrain,
          LBiome.Value, LTerrain.Value);
        Exit;
      end;

      Inc(AReport.CheckedRelations);
      if not RoadMatchesContext(LRoad.Value, LTerrain.Value,
          LHydrology.Value, LBiome.Value, LRelatedLayer) then
      begin
        SetRelatedIssue(AReport, s2vikRoadContext,
          s2lRoads, LRelatedLayer, LRoad,
          AWorld.LayerGraph[LRelatedLayer].Entry[X, Y, 0],
          LRoad.Value, RelatedValueAt(AWorld, LRelatedLayer, X, Y));
        Exit;
      end;

      Inc(AReport.CheckedRelations);
      if not HousingMatchesContext(LHousing.Value, LTerrain.Value,
          LHydrology.Value, LBiome.Value, LRoad.Value,
          LRelatedLayer) then
      begin
        SetRelatedIssue(AReport, s2vikHousingContext,
          s2lHousing, LRelatedLayer, LHousing,
          AWorld.LayerGraph[LRelatedLayer].Entry[X, Y, 0],
          LHousing.Value, RelatedValueAt(AWorld, LRelatedLayer, X, Y));
        Exit;
      end;

      Inc(AReport.CheckedRelations);
      if not FoliageMatchesContext(LFoliage.Value, LTerrain.Value,
          LHydrology.Value, LBiome.Value, LRoad.Value,
          LHousing.Value, LRelatedLayer) then
      begin
        SetRelatedIssue(AReport, s2vikFoliageContext,
          s2lFoliage, LRelatedLayer, LFoliage,
          AWorld.LayerGraph[LRelatedLayer].Entry[X, Y, 0],
          LFoliage.Value, RelatedValueAt(AWorld, LRelatedLayer, X, Y));
        Exit;
      end;
    end;

  for LLayer in [s2lTerrain, s2lBiome, s2lRoads, s2lHousing] do
    for Y := 0 to Integer(AWorld.Height) - 1 do
      for X := 0 to Integer(AWorld.Width) - 1 do
      begin
        LEntry := AWorld.LayerGraph[LLayer].Entry[X, Y, 0];
        for LDirection in WFC_SETTLEMENT2D_CARDINAL_DIRECTIONS do
        begin
          if not TryGetNeighborCoordinate(AWorld, X, Y, LDirection,
            LNeighborX, LNeighborY) then
            Continue;
          LNeighbor := AWorld.LayerGraph[LLayer].Entry[
            LNeighborX, LNeighborY, 0];
          Inc(AReport.CheckedRelations);
          if ((LLayer = s2lTerrain)
              and not TerrainAllows(LEntry.Value, LNeighbor.Value))
            or ((LLayer = s2lBiome)
              and not BiomeAllows(LEntry.Value, LNeighbor.Value))
            or ((LLayer = s2lRoads)
              and not RoadAllows(LEntry.Value, LNeighbor.Value))
            or ((LLayer = s2lHousing)
              and not HousingAllows(LEntry.Value, LNeighbor.Value)) then
          begin
            case LLayer of
              s2lTerrain: LIssueKind := s2vikTerrainAdjacency;
              s2lBiome: LIssueKind := s2vikBiomeAdjacency;
              s2lRoads: LIssueKind := s2vikRoadAdjacency;
              s2lHousing: LIssueKind := s2vikHousingAdjacency;
            else
              LIssueKind := s2vikPipelineTopology;
            end;
            SetRelatedIssue(AReport, LIssueKind, LLayer, LLayer,
              LEntry, LNeighbor, LEntry.Value, LNeighbor.Value);
            AReport.Issue.HasDirection := True;
            AReport.Issue.Direction := LDirection;
            Exit;
          end;
        end;
      end;

  AReport.Valid := True;
  AReport.Issue.Kind := s2vikNone;
  Result := True;
end;

function Settlement2DValidationIssueName(
  const AKind: TSettlement2DValidationIssueKind): String;
begin
  Result := 'unknown';
  case AKind of
    s2vikNone: Result := 'none';
    s2vikPipelineTopology: Result := 'pipeline-topology';
    s2vikPipelineShape: Result := 'pipeline-shape';
    s2vikEmptyCell: Result := 'empty-cell';
    s2vikUnknownValue: Result := 'unknown-value';
    s2vikTerrainAdjacency: Result := 'terrain-adjacency';
    s2vikHydrologyTerrain: Result := 'hydrology-terrain';
    s2vikBiomeTerrain: Result := 'biome-terrain';
    s2vikBiomeAdjacency: Result := 'biome-adjacency';
    s2vikRoadContext: Result := 'road-context';
    s2vikRoadAdjacency: Result := 'road-adjacency';
    s2vikHousingContext: Result := 'housing-context';
    s2vikHousingAdjacency: Result := 'housing-adjacency';
    s2vikFoliageContext: Result := 'foliage-context';
  end;
end;

function DescribeSettlement2DValidationIssue(
  const AIssue: TSettlement2DValidationIssue): String;
const
  DIRECTION_NAMES: array[TGraphDirection] of String = (
    'north', 'east', 'south', 'west', 'up', 'down'
  );
begin
  Result := Settlement2DValidationIssueName(AIssue.Kind)
    + ' in ' + Settlement2DLayerName(AIssue.Layer);
  if AIssue.X >= 0 then
    Result := Result + ' at (' + IntToStr(AIssue.X)
      + ',' + IntToStr(AIssue.Y) + ')';
  if AIssue.Value <> '' then
    Result := Result + ': ' + AIssue.Value;
  if AIssue.HasRelatedLayer then
    Result := Result + ' versus '
      + Settlement2DLayerName(AIssue.RelatedLayer);
  if AIssue.RelatedValue <> '' then
    Result := Result + '=' + AIssue.RelatedValue;
  if AIssue.HasDirection then
    Result := Result + ' toward ' + DIRECTION_NAMES[AIssue.Direction];
end;

end.
