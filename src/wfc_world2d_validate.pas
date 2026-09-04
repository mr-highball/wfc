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
unit wfc_world2d_validate;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_world2d;

type
  TWorld2DValidationIssueKind = (
    w2vikNone,
    w2vikPipelineShape,
    w2vikEmptyCell,
    w2vikUnknownTerrain,
    w2vikTerrainAdjacency,
    w2vikUnknownBiome,
    w2vikBiomeTerrain,
    w2vikBiomeAdjacency,
    w2vikUnknownFoliage,
    w2vikFoliageBiome
  );

  TWorld2DValidationIssue = record
    Kind: TWorld2DValidationIssueKind;
    Layer: TWorld2DLayer;
    X: Integer;
    Y: Integer;
    NeighborX: Integer;
    NeighborY: Integer;
    HasDirection: Boolean;
    Direction: TGraphDirection;
    Value: TGraphValue;
    RelatedValue: TGraphValue;
  end;

  TWorld2DValidationReport = record
    Valid: Boolean;
    CheckedCells: Integer;
    CheckedRelations: Integer;
    Issue: TWorld2DValidationIssue;
  end;

function ValidateWorld2D(const AWorld: TWorld2D;
  out AReport: TWorld2DValidationReport): Boolean;
function World2DValidationIssueName(
  const AKind: TWorld2DValidationIssueKind): String;
function DescribeWorld2DValidationIssue(
  const AIssue: TWorld2DValidationIssue): String;

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
    Result := (ANeighbor = WFC_WORLD2D_BIOME_OCEAN)
      or (ANeighbor = WFC_WORLD2D_BIOME_SHORE)
      or (ANeighbor = WFC_WORLD2D_BIOME_PLAINS)
      or (ANeighbor = WFC_WORLD2D_BIOME_WOODLAND)
      or (ANeighbor = WFC_WORLD2D_BIOME_ALPINE)
  else if (ACurrent = WFC_WORLD2D_BIOME_PLAINS)
    or (ACurrent = WFC_WORLD2D_BIOME_WOODLAND) then
    Result := (ANeighbor = WFC_WORLD2D_BIOME_OCEAN)
      or (ANeighbor = WFC_WORLD2D_BIOME_SHORE)
      or (ANeighbor = WFC_WORLD2D_BIOME_PLAINS)
      or (ANeighbor = WFC_WORLD2D_BIOME_WOODLAND)
      or (ANeighbor = WFC_WORLD2D_BIOME_ALPINE)
  else if ACurrent = WFC_WORLD2D_BIOME_ALPINE then
    Result := (ANeighbor = WFC_WORLD2D_BIOME_SHORE)
      or (ANeighbor = WFC_WORLD2D_BIOME_PLAINS)
      or (ANeighbor = WFC_WORLD2D_BIOME_WOODLAND)
      or (ANeighbor = WFC_WORLD2D_BIOME_ALPINE)
  else
    Result := False;
end;

function IsFoliageValue(const AValue: TGraphValue): Boolean;
begin
  Result := (AValue = WFC_WORLD2D_FOLIAGE_NONE)
    or (AValue = WFC_WORLD2D_FOLIAGE_REEDS)
    or (AValue = WFC_WORLD2D_FOLIAGE_GRASS)
    or (AValue = WFC_WORLD2D_FOLIAGE_TREE)
    or (AValue = WFC_WORLD2D_FOLIAGE_PINE);
end;

function FoliageMatchesBiome(const AFoliage,
  ABiome: TGraphValue): Boolean;
begin
  if AFoliage = WFC_WORLD2D_FOLIAGE_NONE then
    Result := IsBiomeValue(ABiome)
  else if AFoliage = WFC_WORLD2D_FOLIAGE_REEDS then
    Result := ABiome = WFC_WORLD2D_BIOME_SHORE
  else if AFoliage = WFC_WORLD2D_FOLIAGE_GRASS then
    Result := ABiome = WFC_WORLD2D_BIOME_PLAINS
  else if AFoliage = WFC_WORLD2D_FOLIAGE_TREE then
    Result := ABiome = WFC_WORLD2D_BIOME_WOODLAND
  else if AFoliage = WFC_WORLD2D_FOLIAGE_PINE then
    Result := ABiome = WFC_WORLD2D_BIOME_ALPINE
  else
    Result := False;
end;

procedure InitializeReport(out AReport: TWorld2DValidationReport);
begin
  AReport.Valid := False;
  AReport.CheckedCells := 0;
  AReport.CheckedRelations := 0;
  AReport.Issue.Kind := w2vikNone;
  AReport.Issue.Layer := w2lTerrain;
  AReport.Issue.X := -1;
  AReport.Issue.Y := -1;
  AReport.Issue.NeighborX := -1;
  AReport.Issue.NeighborY := -1;
  AReport.Issue.HasDirection := False;
  AReport.Issue.Direction := gdNorth;
  AReport.Issue.Value := '';
  AReport.Issue.RelatedValue := '';
end;

procedure SetIssue(var AReport: TWorld2DValidationReport;
  const AKind: TWorld2DValidationIssueKind;
  const ALayer: TWorld2DLayer; const AEntry, ANeighbor: TGraphEntry;
  const AValue, ARelatedValue: TGraphValue);
begin
  AReport.Valid := False;
  AReport.Issue.Kind := AKind;
  AReport.Issue.Layer := ALayer;
  AReport.Issue.Value := AValue;
  AReport.Issue.RelatedValue := ARelatedValue;
  if Assigned(AEntry) then
  begin
    AReport.Issue.X := Integer(AEntry.Position.X);
    AReport.Issue.Y := Integer(AEntry.Position.Y);
  end;
  if Assigned(ANeighbor) then
  begin
    AReport.Issue.NeighborX := Integer(ANeighbor.Position.X);
    AReport.Issue.NeighborY := Integer(ANeighbor.Position.Y);
  end;
end;

function ValidateLayerShape(const AWorld: TWorld2D;
  const ALayer: TWorld2DLayer): Boolean;
var
  LGraph: TGraph;
begin
  LGraph := AWorld.LayerGraph[ALayer];
  Result := (LGraph.Dimension.Width = AWorld.Width)
    and (LGraph.Dimension.Height = AWorld.Height)
    and (LGraph.Dimension.Depth = 1);
end;

function TryGetNeighborCoordinate(const AWorld: TWorld2D;
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

function ValidateWorld2D(const AWorld: TWorld2D;
  out AReport: TWorld2DValidationReport): Boolean;
var
  LBiome: TGraphEntry;
  LBiomeGraph: TGraph;
  LDirection: TGraphDirection;
  LEntry: TGraphEntry;
  LFoliage: TGraphEntry;
  LFoliageGraph: TGraph;
  LLayer: TWorld2DLayer;
  LHeight: Integer;
  LNeighbor: TGraphEntry;
  LNeighborX: TGraphCoordinate;
  LNeighborY: TGraphCoordinate;
  LTerrain: TGraphEntry;
  LTerrainGraph: TGraph;
  LWidth: Integer;
  X, Y: Integer;
begin
  InitializeReport(AReport);
  Result := False;
  if not Assigned(AWorld) then
  begin
    SetIssue(AReport, w2vikPipelineShape, w2lTerrain, nil, nil, '', '');
    Exit;
  end;
  if AWorld.Graph.TotalPassCount <> 3 then
  begin
    SetIssue(AReport, w2vikPipelineShape, w2lTerrain, nil, nil, '', '');
    Exit;
  end;
  if (AWorld.Width = 0) or (AWorld.Height = 0) then
  begin
    SetIssue(AReport, w2vikPipelineShape, w2lTerrain, nil, nil, '', '');
    Exit;
  end;
  for LLayer := Low(TWorld2DLayer) to High(TWorld2DLayer) do
    if not ValidateLayerShape(AWorld, LLayer) then
    begin
      SetIssue(AReport, w2vikPipelineShape, LLayer, nil, nil, '', '');
      Exit;
    end;

  LTerrainGraph := AWorld.LayerGraph[w2lTerrain];
  LBiomeGraph := AWorld.LayerGraph[w2lBiome];
  LFoliageGraph := AWorld.LayerGraph[w2lFoliage];
  LWidth := Integer(AWorld.Width);
  LHeight := Integer(AWorld.Height);

  for Y := 0 to Pred(LHeight) do
    for X := 0 to Pred(LWidth) do
    begin
      LTerrain := LTerrainGraph.Entry[X, Y, 0];
      Inc(AReport.CheckedCells);
      if LTerrain.Empty then
      begin
        SetIssue(AReport, w2vikEmptyCell, w2lTerrain,
          LTerrain, nil, '', '');
        Exit;
      end;
      if not IsTerrainValue(LTerrain.Value) then
      begin
        SetIssue(AReport, w2vikUnknownTerrain, w2lTerrain,
          LTerrain, nil, LTerrain.Value, '');
        Exit;
      end;

      LBiome := LBiomeGraph.Entry[X, Y, 0];
      Inc(AReport.CheckedCells);
      if LBiome.Empty then
      begin
        SetIssue(AReport, w2vikEmptyCell, w2lBiome,
          LBiome, nil, '', '');
        Exit;
      end;
      if not IsBiomeValue(LBiome.Value) then
      begin
        SetIssue(AReport, w2vikUnknownBiome, w2lBiome,
          LBiome, nil, LBiome.Value, LTerrain.Value);
        Exit;
      end;
      Inc(AReport.CheckedRelations);
      if not BiomeMatchesTerrain(LBiome.Value, LTerrain.Value) then
      begin
        SetIssue(AReport, w2vikBiomeTerrain, w2lBiome,
          LBiome, LTerrain, LBiome.Value, LTerrain.Value);
        Exit;
      end;

      LFoliage := LFoliageGraph.Entry[X, Y, 0];
      Inc(AReport.CheckedCells);
      if LFoliage.Empty then
      begin
        SetIssue(AReport, w2vikEmptyCell, w2lFoliage,
          LFoliage, nil, '', '');
        Exit;
      end;
      if not IsFoliageValue(LFoliage.Value) then
      begin
        SetIssue(AReport, w2vikUnknownFoliage, w2lFoliage,
          LFoliage, nil, LFoliage.Value, LBiome.Value);
        Exit;
      end;
      Inc(AReport.CheckedRelations);
      if not FoliageMatchesBiome(LFoliage.Value, LBiome.Value) then
      begin
        SetIssue(AReport, w2vikFoliageBiome, w2lFoliage,
          LFoliage, LBiome, LFoliage.Value, LBiome.Value);
        Exit;
      end;
    end;

  for LLayer := w2lTerrain to w2lBiome do
    for Y := 0 to Pred(LHeight) do
      for X := 0 to Pred(LWidth) do
      begin
        LEntry := AWorld.LayerGraph[LLayer].Entry[X, Y, 0];
        for LDirection in WFC_WORLD2D_CARDINAL_DIRECTIONS do
        begin
          if not TryGetNeighborCoordinate(AWorld, X, Y, LDirection,
            LNeighborX, LNeighborY) then
            Continue;
          LNeighbor := AWorld.LayerGraph[LLayer].Entry[
            LNeighborX, LNeighborY, 0];
          Inc(AReport.CheckedRelations);
          if ((LLayer = w2lTerrain)
              and (not TerrainAllows(LEntry.Value, LNeighbor.Value)))
            or ((LLayer = w2lBiome)
              and (not BiomeAllows(LEntry.Value, LNeighbor.Value))) then
          begin
            if LLayer = w2lTerrain then
              SetIssue(AReport, w2vikTerrainAdjacency, LLayer,
                LEntry, LNeighbor, LEntry.Value, LNeighbor.Value)
            else
              SetIssue(AReport, w2vikBiomeAdjacency, LLayer,
                LEntry, LNeighbor, LEntry.Value, LNeighbor.Value);
            AReport.Issue.HasDirection := True;
            AReport.Issue.Direction := LDirection;
            Exit;
          end;
        end;
      end;

  AReport.Valid := True;
  AReport.Issue.Kind := w2vikNone;
  Result := True;
end;

function World2DValidationIssueName(
  const AKind: TWorld2DValidationIssueKind): String;
begin
  case AKind of
    w2vikNone:
      Result := 'none';
    w2vikPipelineShape:
      Result := 'pipeline-shape';
    w2vikEmptyCell:
      Result := 'empty-cell';
    w2vikUnknownTerrain:
      Result := 'unknown-terrain';
    w2vikTerrainAdjacency:
      Result := 'terrain-adjacency';
    w2vikUnknownBiome:
      Result := 'unknown-biome';
    w2vikBiomeTerrain:
      Result := 'biome-terrain';
    w2vikBiomeAdjacency:
      Result := 'biome-adjacency';
    w2vikUnknownFoliage:
      Result := 'unknown-foliage';
    w2vikFoliageBiome:
      Result := 'foliage-biome';
  else
    Result := 'unknown';
  end;
end;

function DescribeWorld2DValidationIssue(
  const AIssue: TWorld2DValidationIssue): String;
const
  DIRECTION_NAMES: array[TGraphDirection] of String = (
    'north', 'east', 'south', 'west', 'up', 'down'
  );
begin
  Result := World2DValidationIssueName(AIssue.Kind)
    + ' in ' + World2DLayerName(AIssue.Layer);
  if AIssue.X >= 0 then
    Result := Result + ' at (' + IntToStr(AIssue.X)
      + ',' + IntToStr(AIssue.Y) + ')';
  if AIssue.Value <> '' then
    Result := Result + ': ' + AIssue.Value;
  if AIssue.RelatedValue <> '' then
    Result := Result + ' versus ' + AIssue.RelatedValue;
  if AIssue.HasDirection then
    Result := Result + ' toward ' + DIRECTION_NAMES[AIssue.Direction];
end;

end.
