{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
unit mapped_world_types;
{$mode delphi}{$H+}
interface
uses SysUtils, wfc, wfc_lattice;
const
  MAPPED_WORLD_MODEL_VERSION = 1;
  MAPPED_WORLD_SITE_COUNT = 6;
type
  EMappedWorld = class(Exception);
  TMappedWorldLayer = (mwlTerrain, mwlFoliage, mwlHousing);
  TMappedWorldSampling = (mwsCell, mwsPointStudy, mwsRegion);
  TMappedWorldPreset = (mwpInteriorStudy, mwpLandscapeSandbox);
  TMappedWorldDemand = (mwdVacant, mwdOptional, mwdRequired);
  TMappedWorldAction = (mwaGenerate, mwaHousingOnly, mwaFoliageAndHousing, mwaAllPasses);
  TMappedWorldStatus = (mwstIdle, mwstDirty, mwstSolving, mwstSolved,
    mwstContradiction, mwstLocalLimit, mwstPassLimit, mwstInvalidConfiguration,
    mwstScopeMismatch, mwstUnexpectedError);
  TMappedWorldConfig = record
    Seed: TGraphSeed;
    Preset: TMappedWorldPreset;
    Sampling: TMappedWorldSampling;
    RegionMinimum, RegionMaximum: TGraphOffset;
    LandWeight, WaterWeight, ClearWeight, TreeWeight: TGraphWeight;
  end;
  TMappedWorldSearchOptions = record
    Negotiated: Boolean;
    MaxBacktracks, MaxPassBacktracks: Integer;
    CaptureTrace: Boolean;
  end;
  TMappedWorldCell = record
    Value: TGraphValue;
    Generated, Locked: Boolean;
    LockValue: TGraphValue;
    HasDomain: Boolean;
    Domain: TGraphValues;
  end;
  TMappedWorldCells = array of TMappedWorldCell;
  TMappedWorldLayerResult = record
    Layout: TWfcLatticeLayout;
    Cells: TMappedWorldCells;
  end;
  TMappedWorldLayers = array[TMappedWorldLayer] of TMappedWorldLayerResult;
  TMappedWorldDemands = array[0..MAPPED_WORLD_SITE_COUNT-1] of TMappedWorldDemand;
  TMappedWorldResult = record
    Config: TMappedWorldConfig;
    Revision, ModelVersion, MappingVersion: Integer;
    Layers: TMappedWorldLayers;
    Demands: TMappedWorldDemands;
    ModelValid, PhysicalSafe: Boolean;
    Signature: Cardinal;
    TraceSignature, TranscriptSignature: TGraphTraceSignature;
  end;
  TMappedWorldSample = record
    Layer: TMappedWorldLayer;
    CellIndex: Integer;
    Position: TWfcLatticeVector;
    Bounds: TWfcLatticeBox;
    Cell: TMappedWorldCell;
    Accepted, IsCorner: Boolean;
  end;
  TMappedWorldSamples = array of TMappedWorldSample;
  TMappedWorldInspection = record
    SiteX, SiteY, SiteIndex, Revision: Integer;
    Sampling: TMappedWorldSampling;
    IsCurrent: Boolean;
    Banner: String;
    HouseBounds, QueryBounds: TWfcLatticeBox;
    TerrainQueryInBounds, FoliageQueryInBounds: Boolean;
    SelectedModelClear, PhysicalClear: Boolean;
    TerrainSamples, FoliageSamples, PhysicalBlockers: TMappedWorldSamples;
  end;
  TMappedWorldIssueKind = (mwikMalformed, mwikToken, mwikOwnership,
    mwikDomain, mwikDemand, mwikFoliageTerrain, mwikHousingTerrain,
    mwikHousingQuery, mwikPhysicalTerrain, mwikPhysicalFoliage);
  TMappedWorldIssue = record
    Kind: TMappedWorldIssueKind;
    Layer: TMappedWorldLayer;
    CellIndex: Integer;
    MessageText: String;
  end;
  TMappedWorldIssues = array of TMappedWorldIssue;
  TMappedWorldValidation = record
    ModelValid, PhysicalSafe: Boolean;
    Issues: TMappedWorldIssues;
  end;
  TMappedWorldSvgOptions = record
    ShowTerrain, ShowFoliage, ShowHousing, ShowInspection: Boolean;
    Diagnostic: Boolean;
  end;

function DefaultMappedWorldConfig: TMappedWorldConfig;
function DefaultMappedWorldSearchOptions: TMappedWorldSearchOptions;
function DefaultMappedWorldSvgOptions: TMappedWorldSvgOptions;
function MappedWorldLayout(const ALayer: TMappedWorldLayer): TWfcLatticeLayout;
function MappedWorldLayerName(const ALayer: TMappedWorldLayer): String;
function MappedWorldTokens(const ALayer: TMappedWorldLayer): TGraphValues;
function MappedWorldTokenValid(const ALayer: TMappedWorldLayer; const AValue: TGraphValue): Boolean;
function MappedWorldCellIndex(const ALayer: TMappedWorldLayer; const AX, AY: Integer): Integer;
procedure RequireMappedWorldInteger(const AValue, AMinimum, AMaximum: Integer; const AName: String);
procedure RequireMappedWorldBoolean(const AValue: Boolean; const AName: String);
procedure ValidateMappedWorldConfig(const AConfig: TMappedWorldConfig);
procedure ValidateMappedWorldSearchOptions(const AOptions: TMappedWorldSearchOptions);
procedure ValidateMappedWorldSnapshotShape(const AResult: TMappedWorldResult);
function CopyMappedWorldCell(const ACell: TMappedWorldCell): TMappedWorldCell;
function CopyMappedWorldResult(const AResult: TMappedWorldResult): TMappedWorldResult;
function CopyMappedWorldInspection(const AValue: TMappedWorldInspection): TMappedWorldInspection;
function CalculateMappedWorldSignature(const AResult: TMappedWorldResult): Cardinal;
implementation

procedure RequireMappedWorldInteger(const AValue, AMinimum, AMaximum: Integer; const AName: String);
{$IFDEF PAS2JS}var Valid:Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid=typeof AValue === 'number' && Number.isInteger(AValue); end;
  if not Valid then raise EMappedWorld.Create(AName+' must be an exact integer');
  {$ENDIF}
  if not ((AValue>=AMinimum) and (AValue<=AMaximum)) then
    raise EMappedWorld.Create(AName+' is outside its integer range');
  {$IFDEF PAS2JS}
  if AValue<>Trunc(AValue) then raise EMappedWorld.Create(AName+' must be an exact integer');
  {$ENDIF}
end;

procedure RequireMappedWorldBoolean(const AValue: Boolean; const AName: String);
{$IFDEF PAS2JS}var Valid:Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid=typeof AValue === 'boolean'; end;
  if not Valid then raise EMappedWorld.Create(AName+' must be a Boolean');
  {$ENDIF}
end;

function DefaultMappedWorldConfig: TMappedWorldConfig;
begin
  Result:=Default(TMappedWorldConfig); Result.Seed:=3;
  Result.Preset:=mwpInteriorStudy; Result.Sampling:=mwsCell;
  Result.RegionMinimum:=MakeGraphOffset(0,0,0);
  Result.RegionMaximum:=MakeGraphOffset(8,8,1);
  Result.LandWeight:=12; Result.WaterWeight:=1;
  Result.ClearWeight:=12; Result.TreeWeight:=1;
end;

function DefaultMappedWorldSearchOptions: TMappedWorldSearchOptions;
begin
  Result:=Default(TMappedWorldSearchOptions); Result.Negotiated:=True;
  Result.MaxBacktracks:=64; Result.MaxPassBacktracks:=16;
end;

function DefaultMappedWorldSvgOptions: TMappedWorldSvgOptions;
begin
  Result:=Default(TMappedWorldSvgOptions);
  Result.ShowTerrain:=True; Result.ShowFoliage:=True;
  Result.ShowHousing:=True; Result.ShowInspection:=True;
end;

function MappedWorldLayout(const ALayer: TMappedWorldLayer): TWfcLatticeLayout;
begin
  RequireMappedWorldInteger(Ord(ALayer),0,2,'layer');
  case ALayer of
    mwlTerrain: Result:=MakeWfcLatticeLayout(8,6,1,MakeWfcLatticeVector(0,0,0),MakeWfcLatticeVector(4,4,1),False);
    mwlFoliage: Result:=MakeWfcLatticeLayout(32,24,1,False);
    mwlHousing: Result:=MakeWfcLatticeLayout(3,2,1,MakeWfcLatticeVector(4,4,0),MakeWfcLatticeVector(8,8,1),False);
  end;
end;

function MappedWorldLayerName(const ALayer: TMappedWorldLayer): String;
begin
  RequireMappedWorldInteger(Ord(ALayer),0,2,'layer');
  case ALayer of mwlTerrain:Result:='terrain'; mwlFoliage:Result:='foliage'; mwlHousing:Result:='housing'; end;
end;

function MappedWorldTokens(const ALayer: TMappedWorldLayer): TGraphValues;
begin
  RequireMappedWorldInteger(Ord(ALayer),0,2,'layer');
  Result:=nil; SetLength(Result,2);
  case ALayer of
    mwlTerrain:begin Result[0]:='land'; Result[1]:='water'; end;
    mwlFoliage:begin Result[0]:='clear'; Result[1]:='tree'; end;
    mwlHousing:begin Result[0]:='vacant'; Result[1]:='house'; end;
  end;
end;

function MappedWorldTokenValid(const ALayer: TMappedWorldLayer; const AValue: TGraphValue): Boolean;
begin
  RequireMappedWorldInteger(Ord(ALayer),0,2,'layer');
  {$IFDEF PAS2JS}
  asm if (typeof AValue !== 'string') return false; end;
  {$ENDIF}
  case ALayer of
    mwlTerrain:Result:=(AValue='land') or (AValue='water');
    mwlFoliage:Result:=(AValue='clear') or (AValue='tree');
    mwlHousing:Result:=(AValue='vacant') or (AValue='house');
  end;
end;

function MappedWorldCellIndex(const ALayer: TMappedWorldLayer; const AX, AY: Integer): Integer;
var L:TWfcLatticeLayout;
begin
  L:=MappedWorldLayout(ALayer);
  RequireMappedWorldInteger(AX,0,L.Cells.X-1,'cell X');
  RequireMappedWorldInteger(AY,0,L.Cells.Y-1,'cell Y');
  Result:=AY*L.Cells.X+AX;
end;

procedure ValidateMappedWorldConfig(const AConfig: TMappedWorldConfig);
var L:TWfcLatticeLayout; V:TGraphOffset;
  {$IFDEF PAS2JS}Valid:Boolean;{$ENDIF}
  procedure Weight(const W:TGraphWeight; const Name:String);
  {$IFDEF PAS2JS}var Valid:Boolean;{$ENDIF}
  begin
    {$IFDEF PAS2JS}
    asm Valid=typeof W === 'number' && Number.isInteger(W); end;
    if not Valid then raise EMappedWorld.Create(Name+' must be an exact integer');
    {$ENDIF}
    if not ((W>=1) and (W<=High(Integer))) then raise EMappedWorld.Create(Name+' must be a positive portable weight');
    {$IFDEF PAS2JS}if W<>Trunc(W) then raise EMappedWorld.Create(Name+' must be an exact integer');{$ENDIF}
  end;
  procedure Offset(const O:TGraphOffset);
  {$IFDEF PAS2JS}var Valid:Boolean;{$ENDIF}
  begin
    {$IFDEF PAS2JS}
    asm Valid=O !== null && typeof O === 'object' && !Array.isArray(O); end;
    if not Valid then raise EMappedWorld.Create('region offset must be a record');
    {$ENDIF}
    MakeWfcLatticeVector(O.DeltaX,O.DeltaY,O.DeltaZ);
  end;
  procedure EndPoint(const Anchor,Delta:Integer);
  var D:Double;
  begin
    D:=Anchor; D:=D+Delta;
    if (D<Low(Integer)) or (D>High(Integer)) then raise EMappedWorld.Create('region exceeds signed world coordinates');
  end;
begin
  {$IFDEF PAS2JS}
  asm
    Valid=AConfig !== null && typeof AConfig === 'object' && !Array.isArray(AConfig) &&
      typeof AConfig.Seed === 'number' && Number.isInteger(AConfig.Seed);
  end;
  if not Valid then raise EMappedWorld.Create('config must be a record with an exact Cardinal seed');
  {$ENDIF}
  RequireMappedWorldInteger(Ord(AConfig.Preset),0,1,'preset');
  RequireMappedWorldInteger(Ord(AConfig.Sampling),0,2,'sampling');
  if not ((AConfig.Seed>=0) and (AConfig.Seed<=High(Cardinal))) then raise EMappedWorld.Create('seed must be a Cardinal');
  {$IFDEF PAS2JS}if AConfig.Seed<>Trunc(AConfig.Seed) then raise EMappedWorld.Create('seed must be an exact integer');{$ENDIF}
  Weight(AConfig.LandWeight,'land weight'); Weight(AConfig.WaterWeight,'water weight');
  Weight(AConfig.ClearWeight,'clear weight'); Weight(AConfig.TreeWeight,'tree weight');
  Offset(AConfig.RegionMinimum); Offset(AConfig.RegionMaximum);
  if (AConfig.RegionMinimum.DeltaX>=AConfig.RegionMaximum.DeltaX) or
    (AConfig.RegionMinimum.DeltaY>=AConfig.RegionMaximum.DeltaY) or
    (AConfig.RegionMinimum.DeltaZ>=AConfig.RegionMaximum.DeltaZ) then
    raise EMappedWorld.Create('region must have positive XYZ extent');
  L:=MappedWorldLayout(mwlHousing);
  V:=AConfig.RegionMinimum;
  EndPoint(L.Origin.X,V.DeltaX); EndPoint(L.Origin.Y,V.DeltaY); EndPoint(L.Origin.Z,V.DeltaZ);
  V:=AConfig.RegionMaximum;
  EndPoint(L.Origin.X+(L.Cells.X-1)*L.Pitch.X,V.DeltaX);
  EndPoint(L.Origin.Y+(L.Cells.Y-1)*L.Pitch.Y,V.DeltaY);
  EndPoint(L.Origin.Z,V.DeltaZ);
end;

procedure ValidateMappedWorldSearchOptions(const AOptions: TMappedWorldSearchOptions);
{$IFDEF PAS2JS}var Valid:Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid=AOptions !== null && typeof AOptions === 'object' && !Array.isArray(AOptions); end;
  if not Valid then raise EMappedWorld.Create('search options must be a record');
  {$ENDIF}
  RequireMappedWorldBoolean(AOptions.Negotiated,'negotiated');
  RequireMappedWorldBoolean(AOptions.CaptureTrace,'capture trace');
  RequireMappedWorldInteger(AOptions.MaxBacktracks,0,High(Integer),'local backtracks');
  RequireMappedWorldInteger(AOptions.MaxPassBacktracks,0,High(Integer),'pass backtracks');
end;

procedure ValidateMappedWorldSnapshotShape(const AResult: TMappedWorldResult);
var L:TMappedWorldLayer; I,J:Integer; C:TMappedWorldCell;
  {$IFDEF PAS2JS}Valid:Boolean;{$ENDIF}
  procedure CardinalValue(const V:Cardinal; const Name:String);
  {$IFDEF PAS2JS}var Valid:Boolean;{$ENDIF}
  begin
    {$IFDEF PAS2JS}
    asm
      Valid=typeof V === 'number' && Number.isInteger(V) && V >= 0 && V <= 4294967295;
    end;
    if not Valid then raise EMappedWorld.Create(Name+' must be an exact Cardinal');
    {$ENDIF}
  end;
begin
  {$IFDEF PAS2JS}
  asm
    Valid=AResult !== null && typeof AResult === 'object' && !Array.isArray(AResult) &&
      Array.isArray(AResult.Layers) && AResult.Layers.length === 3 &&
      Array.isArray(AResult.Demands) && AResult.Demands.length === 6;
  end;
  if not Valid then raise EMappedWorld.Create('snapshot needs three layers and six demands');
  {$ENDIF}
  ValidateMappedWorldConfig(AResult.Config);
  RequireMappedWorldInteger(AResult.Revision,0,High(Integer),'revision');
  RequireMappedWorldInteger(AResult.ModelVersion,MAPPED_WORLD_MODEL_VERSION,MAPPED_WORLD_MODEL_VERSION,'model version');
  RequireMappedWorldInteger(AResult.MappingVersion,WFC_PASS_MAPPING_VERSION,WFC_PASS_MAPPING_VERSION,'mapping version');
  RequireMappedWorldBoolean(AResult.ModelValid,'model valid');
  RequireMappedWorldBoolean(AResult.PhysicalSafe,'physical safe');
  CardinalValue(AResult.Signature,'signature');
  CardinalValue(AResult.TraceSignature,'trace signature');
  CardinalValue(AResult.TranscriptSignature,'transcript signature');
  for I:=0 to MAPPED_WORLD_SITE_COUNT-1 do RequireMappedWorldInteger(Ord(AResult.Demands[I]),0,2,'demand');
  for L:=Low(TMappedWorldLayer) to High(TMappedWorldLayer) do
  begin
    {$IFDEF PAS2JS}
    asm Valid=AResult.Layers[L] !== null && typeof AResult.Layers[L] === 'object' && !Array.isArray(AResult.Layers[L]); end;
    if not Valid then raise EMappedWorld.Create('snapshot layer must be a record');
    {$ENDIF}
    ValidateWfcLatticeLayout(AResult.Layers[L].Layout);
    if not SameWfcLatticeLayout(AResult.Layers[L].Layout,MappedWorldLayout(L)) then
      raise EMappedWorld.Create('snapshot '+MappedWorldLayerName(L)+' layout differs from this showcase model');
    {$IFDEF PAS2JS}
    asm Valid=Array.isArray(AResult.Layers[L].Cells); end;
    if not Valid then raise EMappedWorld.Create('snapshot cells must be an array');
    {$ENDIF}
    if Length(AResult.Layers[L].Cells)<>WfcLatticeCellCount(AResult.Layers[L].Layout) then raise EMappedWorld.Create('snapshot cell count differs from its layer');
    for I:=0 to High(AResult.Layers[L].Cells) do
    begin
      {$IFDEF PAS2JS}
      asm
        var rawCell=AResult.Layers[L].Cells[I];
        Valid=rawCell !== null && typeof rawCell === 'object' && !Array.isArray(rawCell) &&
          typeof rawCell.Value === 'string' && typeof rawCell.LockValue === 'string' && Array.isArray(rawCell.Domain);
      end;
      if not Valid then raise EMappedWorld.Create('snapshot cell record/strings/domain are malformed');
      {$ENDIF}
      C:=AResult.Layers[L].Cells[I];
      RequireMappedWorldBoolean(C.Generated,'generated'); RequireMappedWorldBoolean(C.Locked,'locked');
      RequireMappedWorldBoolean(C.HasDomain,'has domain');
      if (not C.HasDomain) and (Length(C.Domain)<>0) then raise EMappedWorld.Create('absent domain must have no tokens');
      if Length(C.Domain)>2 then raise EMappedWorld.Create('domain exceeds layer vocabulary');
      for J:=0 to High(C.Domain) do
      begin
        if not MappedWorldTokenValid(L,C.Domain[J]) then raise EMappedWorld.Create('domain token is outside layer vocabulary');
        if (J>0) and (C.Domain[J]=C.Domain[0]) then raise EMappedWorld.Create('domain tokens must be distinct');
      end;
    end;
  end;
end;

function CopyMappedWorldCell(const ACell: TMappedWorldCell): TMappedWorldCell;
begin Result:=ACell; Result.Domain:=Copy(ACell.Domain,0,Length(ACell.Domain)); end;

function CopyMappedWorldResult(const AResult: TMappedWorldResult): TMappedWorldResult;
var L:TMappedWorldLayer; I:Integer;
begin
  Result:=AResult;
  for L:=Low(TMappedWorldLayer) to High(TMappedWorldLayer) do
  begin
    Result.Layers[L].Cells:=nil; SetLength(Result.Layers[L].Cells,Length(AResult.Layers[L].Cells));
    for I:=0 to High(Result.Layers[L].Cells) do Result.Layers[L].Cells[I]:=CopyMappedWorldCell(AResult.Layers[L].Cells[I]);
  end;
end;

function CopySamples(const A:TMappedWorldSamples):TMappedWorldSamples;
var I:Integer;
begin
  Result:=nil; SetLength(Result,Length(A));
  for I:=0 to High(A) do begin Result[I]:=A[I]; Result[I].Cell:=CopyMappedWorldCell(A[I].Cell); end;
end;

function CopyMappedWorldInspection(const AValue: TMappedWorldInspection): TMappedWorldInspection;
begin
  Result:=AValue; Result.TerrainSamples:=CopySamples(AValue.TerrainSamples);
  Result.FoliageSamples:=CopySamples(AValue.FoliageSamples);
  Result.PhysicalBlockers:=CopySamples(AValue.PhysicalBlockers);
end;

function CalculateMappedWorldSignature(const AResult: TMappedWorldResult): Cardinal;
var H:Cardinal; L:TMappedWorldLayer; I,J:Integer; C:TMappedWorldCell; B:TWfcLatticeLayout;
  procedure ByteHash(const V:Byte);
  var LoPart,HiPart:Cardinal;
  begin
    LoPart:=(H and $FFFF) xor V;
    HiPart:=(H shr 16)*403+LoPart*256;
    LoPart:=LoPart*403; HiPart:=HiPart+(LoPart shr 16);
    H:=((HiPart and $FFFF) shl 16) or (LoPart and $FFFF);
  end;
  procedure U(const V:Cardinal);
  begin ByteHash(V and $FF); ByteHash((V shr 8) and $FF); ByteHash((V shr 16) and $FF); ByteHash((V shr 24) and $FF); end;
  procedure N(const V:Integer);
  begin if V<0 then U(High(Cardinal)-Cardinal(-(V+1))) else U(Cardinal(V)); end;
  procedure S(const V:String);
  var K:Integer;
  begin N(Length(V)); for K:=1 to Length(V) do begin N(Ord(V[K])); end; end;
  procedure O(const V:TGraphOffset);
  begin N(V.DeltaX); N(V.DeltaY); N(V.DeltaZ); end;
  procedure Vec(const V:TWfcLatticeVector);
  begin N(V.X); N(V.Y); N(V.Z); end;
begin
  ValidateMappedWorldSnapshotShape(AResult);
  H:=2166136261; S('mapped-world-result'); N(AResult.ModelVersion); N(AResult.MappingVersion);
  N(AResult.Revision); U(AResult.Config.Seed); N(Ord(AResult.Config.Preset)); N(Ord(AResult.Config.Sampling));
  O(AResult.Config.RegionMinimum); O(AResult.Config.RegionMaximum);
  N(AResult.Config.LandWeight); N(AResult.Config.WaterWeight); N(AResult.Config.ClearWeight); N(AResult.Config.TreeWeight);
  for L:=Low(TMappedWorldLayer) to High(TMappedWorldLayer) do
  begin
    B:=AResult.Layers[L].Layout; Vec(B.Cells); Vec(B.Origin); Vec(B.Pitch); N(Ord(B.Wrap));
    for I:=0 to High(AResult.Layers[L].Cells) do
    begin
      C:=AResult.Layers[L].Cells[I]; S(C.Value); N(Ord(C.Generated)); N(Ord(C.Locked));
      S(C.LockValue); N(Ord(C.HasDomain)); N(Length(C.Domain)); for J:=0 to High(C.Domain) do S(C.Domain[J]);
    end;
  end;
  for I:=0 to MAPPED_WORLD_SITE_COUNT-1 do N(Ord(AResult.Demands[I]));
  N(Ord(AResult.ModelValid)); N(Ord(AResult.PhysicalSafe)); U(AResult.TraceSignature); U(AResult.TranscriptSignature);
  Result:=H;
end;
end.
