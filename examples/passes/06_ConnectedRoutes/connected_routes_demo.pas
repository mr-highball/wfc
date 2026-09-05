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
unit connected_routes_demo;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc;

const
  CONNECTED_ROUTES_DEMO_VERSION = 1;
  CONNECTED_ROUTES_TOWN_WIDTH = 7;
  CONNECTED_ROUTES_TOWN_HEIGHT = 5;
  CONNECTED_ROUTES_CIRCULATION_WIDTH = 5;
  CONNECTED_ROUTES_CIRCULATION_HEIGHT = 4;
  CONNECTED_ROUTES_CIRCULATION_DEPTH = 2;

type
  EConnectedRoutesDemo = class(Exception);

  TConnectedRoutesCase = (
    crcTown2D,
    crcCirculation3D
  );

  TConnectedRoutesPortal = (
    crpFirst,
    crpSecond,
    crpBoth,
    crpNone
  );

  TConnectedRoutesConfig = record
    CaseKind: TConnectedRoutesCase;
    Portal: TConnectedRoutesPortal;
    Seed: TGraphSeed;
    RequireAllParticipants: Boolean;
    MaxBacktracks: Integer;
    MaxPassBacktracks: Integer;
    CaptureTrace: Boolean;
  end;

  TConnectedRoutesCell = record
    BaseValue: TGraphValue;
    RouteValue: TGraphValue;
    FeatureValue: TGraphValue;
    Openings: TGraphDirections;
    Participant: Boolean;
    OnWitness: Boolean;
  end;
  TConnectedRoutesCells = array of TConnectedRoutesCell;

  TConnectedRoutesResult = record
    Solved: Boolean;
    WasRepair: Boolean;
    ProviderReused: Boolean;
    IndependentlyVerified: Boolean;
    CaseKind: TConnectedRoutesCase;
    Portal: TConnectedRoutesPortal;
    RequireAllParticipants: Boolean;
    Width, Height, Depth: Integer;
    Root: TGraphPosition;
    RequiredPositions: TGraphPositions;
    Cells: TConnectedRoutesCells;
    ParticipantCount: Integer;
    WitnessCellCount: Integer;
    DisconnectedParticipantCount: Integer;
    WitnessVerticalEdgeCount: Integer;
    PassBacktracks: Integer;
    FailedPassIndex: Integer;
    ConstraintIndex: Integer;
    Status: String;
    Detail: String;
    Signature: String;
    OutputKey: String;
    SvgText: String;
  end;

  TConnectedRoutesSession = class
  private
    FConfig: TConnectedRoutesConfig;
    FGraph: TGraph;
    FHasBaseline: Boolean;
    FHasCurrent: Boolean;
    FCurrent: TConnectedRoutesResult;
    FProviderBaseline: TGraphValues;
    procedure BuildGraph;
    procedure ApplyRouteDomains;
    procedure ApplyConnectivity;
    procedure InvalidateCurrent;
    function RoutePassLabel: String;
    function RoutePassIndex: Integer;
    function ProviderPassIndex: Integer;
    function CaptureResult(const AWasRepair, AProviderReused: Boolean;
      const APassBacktracks: Integer): TConnectedRoutesResult;
  public
    constructor Create(const AConfig: TConnectedRoutesConfig);
    destructor Destroy; override;
    procedure Reset(const AConfig: TConnectedRoutesConfig);
    procedure SetPortal(const APortal: TConnectedRoutesPortal);
    procedure SetRequireAllParticipants(const AValue: Boolean);
    procedure SetSearchLimits(const AMaxBacktracks,
      AMaxPassBacktracks: Integer; const ACaptureTrace: Boolean = False);
    function Generate(out AResult: TConnectedRoutesResult): Boolean;
    function Repair(out AResult: TConnectedRoutesResult): Boolean;
    function CopyCurrent(out AResult: TConnectedRoutesResult): Boolean;
    property Config: TConnectedRoutesConfig read FConfig;
    property HasBaseline: Boolean read FHasBaseline;
    property HasCurrent: Boolean read FHasCurrent;
  end;

function DefaultConnectedRoutesConfig(
  const ACase: TConnectedRoutesCase): TConnectedRoutesConfig;
function ConnectedRoutesCaseName(const ACase: TConnectedRoutesCase): String;
function ConnectedRoutesPortalName(
  const APortal: TConnectedRoutesPortal): String;
function CopyConnectedRoutesResult(
  const ASource: TConnectedRoutesResult): TConnectedRoutesResult;
function ConnectedRoutesSelfTest: Integer;

implementation

const
  PASS_TERRAIN = 'terrain';
  PASS_ROADS = 'roads';
  PASS_HOUSING = 'housing';
  PASS_STRUCTURE = 'structure';
  PASS_CIRCULATION = 'circulation';
  PASS_FEATURES = 'features';
  CONNECTIVITY_LABEL = 'rooted-route';

  TOWN_FIRST_SIGNATURE = '61943F3F';
  TOWN_SECOND_SIGNATURE = '5C735E7A';
  CIRCULATION_FIRST_SIGNATURE = '9F2CC7A4';
  CIRCULATION_SECOND_SIGNATURE = '47E47400';

  BASE_LAND = 'land';
  BASE_WATER = 'water';
  BASE_BRIDGE = 'bridge';
  BASE_FLOOR = 'floor';
  BASE_WALL = 'wall';
  BASE_SHAFT = 'shaft';

  FEATURE_NONE = 'none';
  FEATURE_TOWN_ROOT = 'town-root';
  FEATURE_TOWN_TERMINAL = 'town-terminal';
  FEATURE_ENTRANCE = 'entrance';
  FEATURE_GALLERY = 'gallery';

  ROUTE_VALUE_COUNT = 18;
  ROUTE_EMPTY_WEIGHT = 32;
  LF = #10;

  { Every 2D cardinal subset plus one lower-shaft and one upper-shaft
    profile. The shaft values expose all horizontal ports; only reciprocal
    selected neighbors become edges. }
  ROUTE_MASKS: array[0..ROUTE_VALUE_COUNT - 1] of Integer = (
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 31, 47);

type
  TIntegerArray = array of Integer;
  TBooleanArray = array of Boolean;

procedure Require(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise EConnectedRoutesDemo.Create(AMessage);
end;

procedure ValidateEnum(const AValue, AMinimum, AMaximum: Integer;
  const AName: String);
begin
  {$IFDEF PAS2JS}
  if AValue <> Trunc(AValue) then
    raise EConnectedRoutesDemo.Create(AName + ' is invalid');
  {$ENDIF}
  if (AValue < AMinimum) or (AValue > AMaximum) then
    raise EConnectedRoutesDemo.Create(AName + ' is invalid');
end;

function Position(const AX, AY, AZ: Integer): TGraphPosition;
begin
  Result.X := TGraphCoordinate(AX);
  Result.Y := TGraphCoordinate(AY);
  Result.Z := TGraphCoordinate(AZ);
end;

function PositionEqual(const ALeft, ARight: TGraphPosition): Boolean;
begin
  Result := (ALeft.X = ARight.X) and (ALeft.Y = ARight.Y)
    and (ALeft.Z = ARight.Z);
end;

function ConnectedRoutesCaseName(const ACase: TConnectedRoutesCase): String;
begin
  case ACase of
    crcTown2D: Result := 'town';
    crcCirculation3D: Result := 'circulation';
  else
    raise EConnectedRoutesDemo.Create('connected-routes case is invalid');
  end;
end;

function ConnectedRoutesPortalName(
  const APortal: TConnectedRoutesPortal): String;
begin
  case APortal of
    crpFirst: Result := 'first';
    crpSecond: Result := 'second';
    crpBoth: Result := 'both';
    crpNone: Result := 'none';
  else
    raise EConnectedRoutesDemo.Create('connected-routes portal is invalid');
  end;
end;

function DefaultConnectedRoutesConfig(
  const ACase: TConnectedRoutesCase): TConnectedRoutesConfig;
begin
  ValidateEnum(Ord(ACase), Ord(Low(TConnectedRoutesCase)),
    Ord(High(TConnectedRoutesCase)), 'connected-routes case');
  Result := Default(TConnectedRoutesConfig);
  Result.CaseKind := ACase;
  Result.Portal := crpFirst;
  Result.Seed := 0;
  Result.RequireAllParticipants := True;
  Result.MaxBacktracks := 4096;
  Result.MaxPassBacktracks := 32;
  Result.CaptureTrace := False;
end;

function RouteValue(const AMask: Integer): TGraphValue;
var
  I: Integer;
begin
  for I := 0 to ROUTE_VALUE_COUNT - 1 do
    if ROUTE_MASKS[I] = AMask then
      Exit('route-' + LowerCase(IntToHex(AMask, 2)));
  raise EConnectedRoutesDemo.Create('route mask is outside the demo palette');
end;

function RouteMask(const AValue: TGraphValue): Integer;
var
  I: Integer;
begin
  for I := 0 to ROUTE_VALUE_COUNT - 1 do
    if AValue = RouteValue(ROUTE_MASKS[I]) then
      Exit(ROUTE_MASKS[I]);
  raise EConnectedRoutesDemo.Create('captured route has an unknown value');
end;

function DirectionBit(const ADirection: TGraphDirection): Integer;
begin
  Result := 1 shl Ord(ADirection);
end;

function MaskHasDirection(const AMask: Integer;
  const ADirection: TGraphDirection): Boolean;
begin
  Result := (AMask and DirectionBit(ADirection)) <> 0;
end;

function OpeningsForMask(const AMask: Integer): TGraphDirections;
var
  D: TGraphDirection;
begin
  Result := [];
  for D := Low(TGraphDirection) to High(TGraphDirection) do
    if MaskHasDirection(AMask, D) then
      Include(Result, D);
end;

function CoordinateIndex(const AX, AY, AZ, AWidth,
  AHeight: Integer): Integer;
begin
  Result := ((AZ * AHeight) + AY) * AWidth + AX;
end;

procedure IndexCoordinate(const AIndex, AWidth, AHeight: Integer;
  out AX, AY, AZ: Integer);
var
  LPlane: Integer;
begin
  LPlane := AWidth * AHeight;
  AZ := AIndex div LPlane;
  AY := (AIndex mod LPlane) div AWidth;
  AX := AIndex mod AWidth;
end;

function PositionIndex(const APosition: TGraphPosition;
  const AWidth, AHeight, ADepth: Integer): Integer;
begin
  if (APosition.X >= TGraphCoordinate(AWidth))
    or (APosition.Y >= TGraphCoordinate(AHeight))
    or (APosition.Z >= TGraphCoordinate(ADepth)) then
    raise EConnectedRoutesDemo.Create('position is outside the demo shape');
  Result := CoordinateIndex(Integer(APosition.X), Integer(APosition.Y),
    Integer(APosition.Z), AWidth, AHeight);
end;

function TryNeighbor(const AIndex, AWidth, AHeight, ADepth: Integer;
  const ADirection: TGraphDirection; out ANeighbor: Integer): Boolean;
var
  X, Y, Z: Integer;
begin
  IndexCoordinate(AIndex, AWidth, AHeight, X, Y, Z);
  case ADirection of
    gdNorth: Inc(Y);
    gdEast: Inc(X);
    gdSouth: Dec(Y);
    gdWest: Dec(X);
    gdUp: Inc(Z);
    gdDown: Dec(Z);
  else
    raise EConnectedRoutesDemo.Create('direction is invalid');
  end;
  Result := (X >= 0) and (X < AWidth) and (Y >= 0) and (Y < AHeight)
    and (Z >= 0) and (Z < ADepth);
  if Result then
    ANeighbor := CoordinateIndex(X, Y, Z, AWidth, AHeight)
  else
    ANeighbor := -1;
end;

function CopyPositions(const ASource: TGraphPositions): TGraphPositions;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ASource));
  for I := 0 to High(ASource) do
    Result[I] := ASource[I];
end;

function CopyCells(const ASource: TConnectedRoutesCells): TConnectedRoutesCells;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ASource));
  for I := 0 to High(ASource) do
    Result[I] := ASource[I];
end;

function CopyConnectedRoutesResult(
  const ASource: TConnectedRoutesResult): TConnectedRoutesResult;
begin
  Result := ASource;
  Result.RequiredPositions := CopyPositions(ASource.RequiredPositions);
  Result.Cells := CopyCells(ASource.Cells);
end;

procedure ValidateConfig(const AConfig: TConnectedRoutesConfig);
begin
  ValidateEnum(Ord(AConfig.CaseKind), Ord(Low(TConnectedRoutesCase)),
    Ord(High(TConnectedRoutesCase)), 'connected-routes case');
  ValidateEnum(Ord(AConfig.Portal), Ord(Low(TConnectedRoutesPortal)),
    Ord(High(TConnectedRoutesPortal)), 'connected-routes portal');
  if AConfig.MaxBacktracks < 0 then
    raise EConnectedRoutesDemo.Create('local backtracks cannot be negative');
  if AConfig.MaxPassBacktracks < 0 then
    raise EConnectedRoutesDemo.Create('pass backtracks cannot be negative');
  {$IFDEF PAS2JS}
  if (AConfig.Seed < 0) or (AConfig.Seed > Cardinal($FFFFFFFF))
    or (AConfig.Seed <> Trunc(AConfig.Seed)) then
    raise EConnectedRoutesDemo.Create(
      'seed must fit an exact unsigned 32-bit integer');
  if (AConfig.MaxBacktracks <> Trunc(AConfig.MaxBacktracks))
    or (AConfig.MaxBacktracks > High(Integer)) then
    raise EConnectedRoutesDemo.Create(
      'local backtracks must fit an exact nonnegative Integer');
  if (AConfig.MaxPassBacktracks <> Trunc(AConfig.MaxPassBacktracks))
    or (AConfig.MaxPassBacktracks > High(Integer)) then
    raise EConnectedRoutesDemo.Create(
      'pass backtracks must fit an exact nonnegative Integer');
  if ((AConfig.RequireAllParticipants <> False)
      and (AConfig.RequireAllParticipants <> True))
    or ((AConfig.CaptureTrace <> False)
      and (AConfig.CaptureTrace <> True)) then
    raise EConnectedRoutesDemo.Create(
      'all-participants and trace flags must be Boolean');
  {$ENDIF}
end;

procedure ScenarioShape(const ACase: TConnectedRoutesCase;
  out AWidth, AHeight, ADepth: Integer);
begin
  case ACase of
    crcTown2D:
      begin
        AWidth := CONNECTED_ROUTES_TOWN_WIDTH;
        AHeight := CONNECTED_ROUTES_TOWN_HEIGHT;
        ADepth := 1;
      end;
    crcCirculation3D:
      begin
        AWidth := CONNECTED_ROUTES_CIRCULATION_WIDTH;
        AHeight := CONNECTED_ROUTES_CIRCULATION_HEIGHT;
        ADepth := CONNECTED_ROUTES_CIRCULATION_DEPTH;
      end;
  else
    raise EConnectedRoutesDemo.Create('connected-routes case is invalid');
  end;
end;

function ScenarioRoot(const ACase: TConnectedRoutesCase): TGraphPosition;
begin
  case ACase of
    crcTown2D: Result := Position(0, 2, 0);
    crcCirculation3D: Result := Position(0, 1, 0);
  else
    raise EConnectedRoutesDemo.Create('connected-routes case is invalid');
  end;
end;

function ScenarioRequired(
  const ACase: TConnectedRoutesCase): TGraphPositions;
begin
  Result := nil;
  SetLength(Result, 2);
  case ACase of
    crcTown2D:
      begin
        Result[0] := Position(6, 0, 0);
        Result[1] := Position(6, 4, 0);
      end;
    crcCirculation3D:
      begin
        Result[0] := Position(4, 1, 1);
        Result[1] := Position(4, 3, 1);
      end;
  else
    raise EConnectedRoutesDemo.Create('connected-routes case is invalid');
  end;
end;

function IsFirstPortal(const ACase: TConnectedRoutesCase;
  const AX, AY: Integer): Boolean;
begin
  case ACase of
    crcTown2D: Result := (AX = 3) and (AY = 1);
    crcCirculation3D: Result := (AX = 1) and (AY = 1);
  else
    raise EConnectedRoutesDemo.Create('connected-routes case is invalid');
  end;
end;

function IsSecondPortal(const ACase: TConnectedRoutesCase;
  const AX, AY: Integer): Boolean;
begin
  case ACase of
    crcTown2D: Result := (AX = 3) and (AY = 3);
    crcCirculation3D: Result := (AX = 3) and (AY = 2);
  else
    raise EConnectedRoutesDemo.Create('connected-routes case is invalid');
  end;
end;

function PortalEnabled(const APortal: TConnectedRoutesPortal;
  const AFirst: Boolean): Boolean;
begin
  case APortal of
    crpFirst: Result := AFirst;
    crpSecond: Result := not AFirst;
    crpBoth: Result := True;
    crpNone: Result := False;
  else
    raise EConnectedRoutesDemo.Create('connected-routes portal is invalid');
  end;
end;

function BaseValueAt(const ACase: TConnectedRoutesCase;
  const AX, AY, AZ: Integer): TGraphValue;
begin
  case ACase of
    crcTown2D:
      begin
        if AX <> 3 then
          Exit(BASE_LAND);
        if IsFirstPortal(ACase, AX, AY)
          or IsSecondPortal(ACase, AX, AY) then
          Exit(BASE_BRIDGE);
        Result := BASE_WATER;
      end;
    crcCirculation3D:
      begin
        if IsFirstPortal(ACase, AX, AY)
          or IsSecondPortal(ACase, AX, AY) then
          Exit(BASE_SHAFT);
        if ((AX = 2) and (AY = 1))
          or ((AZ = 1) and (AX = 2) and (AY = 3)) then
          Exit(BASE_WALL);
        Result := BASE_FLOOR;
      end;
  else
    raise EConnectedRoutesDemo.Create('connected-routes case is invalid');
  end;
end;

function FeatureValueAt(const ACase: TConnectedRoutesCase;
  const APosition: TGraphPosition): TGraphValue;
var
  I: Integer;
  LRequired: TGraphPositions;
begin
  if PositionEqual(APosition, ScenarioRoot(ACase)) then
  begin
    if ACase = crcTown2D then
      Exit(FEATURE_TOWN_ROOT)
    else
      Exit(FEATURE_ENTRANCE);
  end;
  LRequired := ScenarioRequired(ACase);
  for I := 0 to High(LRequired) do
    if PositionEqual(APosition, LRequired[I]) then
    begin
      if ACase = crcTown2D then
        Exit(FEATURE_TOWN_TERMINAL)
      else
        Exit(FEATURE_GALLERY);
    end;
  Result := FEATURE_NONE;
end;

function AllRouteValues(const AIncludeEmpty: Boolean): TGraphValues;
var
  I, LStart: Integer;
begin
  Result := nil;
  if AIncludeEmpty then
    LStart := 0
  else
    LStart := 1;
  SetLength(Result, ROUTE_VALUE_COUNT - LStart);
  for I := LStart to ROUTE_VALUE_COUNT - 1 do
    Result[I - LStart] := RouteValue(ROUTE_MASKS[I]);
end;

function IsFixedRoutePosition(const ACase: TConnectedRoutesCase;
  const AX, AY, AZ: Integer): Boolean;
var
  I: Integer;
  LPosition: TGraphPosition;
  LRequired: TGraphPositions;
begin
  LPosition := Position(AX, AY, AZ);
  if PositionEqual(LPosition, ScenarioRoot(ACase)) then
    Exit(True);
  LRequired := ScenarioRequired(ACase);
  for I := 0 to High(LRequired) do
    if PositionEqual(LPosition, LRequired[I]) then
      Exit(True);
  Result := False;
end;

function MaskStaysInBounds(const AMask, AX, AY, AZ,
  AWidth, AHeight, ADepth: Integer): Boolean;
begin
  Result := not (((AX = 0) and MaskHasDirection(AMask, gdWest))
    or ((AX = AWidth - 1) and MaskHasDirection(AMask, gdEast))
    or ((AY = 0) and MaskHasDirection(AMask, gdSouth))
    or ((AY = AHeight - 1) and MaskHasDirection(AMask, gdNorth))
    or ((AZ = 0) and MaskHasDirection(AMask, gdDown))
    or ((AZ = ADepth - 1) and MaskHasDirection(AMask, gdUp)));
end;

function MaskAllowedAt(const AConfig: TConnectedRoutesConfig;
  const AMask, AX, AY, AZ, AWidth, AHeight, ADepth: Integer): Boolean;
var
  I: Integer;
  LBase: TGraphValue;
  LFirst, LPortalOpen: Boolean;
begin
  Result := False;
  LPortalOpen := False;
  for I := 0 to ROUTE_VALUE_COUNT - 1 do
    if ROUTE_MASKS[I] = AMask then
    begin
      LPortalOpen := True;
      Break;
    end;
  if not LPortalOpen then Exit;
  LBase := BaseValueAt(AConfig.CaseKind, AX, AY, AZ);
  if AMask = 0 then
    Exit(not IsFixedRoutePosition(AConfig.CaseKind, AX, AY, AZ));
  if not MaskStaysInBounds(AMask, AX, AY, AZ,
    AWidth, AHeight, ADepth) then Exit;

  if AConfig.CaseKind = crcTown2D then
  begin
    if (AMask and not $0F) <> 0 then Exit;
    if LBase = BASE_WATER then Exit;
    if LBase = BASE_BRIDGE then
    begin
      LFirst := IsFirstPortal(AConfig.CaseKind, AX, AY);
      LPortalOpen := PortalEnabled(AConfig.Portal, LFirst);
      if not LPortalOpen then Exit;
    end;
    Exit(True);
  end;

  if LBase = BASE_WALL then Exit;
  if LBase = BASE_SHAFT then
  begin
    LFirst := IsFirstPortal(AConfig.CaseKind, AX, AY);
    LPortalOpen := PortalEnabled(AConfig.Portal, LFirst);
    if not LPortalOpen then Exit;
    if AZ = 0 then Result := AMask = 31
    else Result := AMask = 47;
    Exit;
  end;
  Result := (AMask and (DirectionBit(gdUp) or
    DirectionBit(gdDown))) = 0;
end;

function AllowedRouteValuesAt(const AConfig: TConnectedRoutesConfig;
  const AX, AY, AZ, AWidth, AHeight, ADepth: Integer): TGraphValues;
var
  I, LCount: Integer;
begin
  Result := nil;
  SetLength(Result, ROUTE_VALUE_COUNT);
  LCount := 0;
  for I := 0 to ROUTE_VALUE_COUNT - 1 do
    if MaskAllowedAt(AConfig, ROUTE_MASKS[I], AX, AY, AZ,
      AWidth, AHeight, ADepth) then
    begin
      Result[LCount] := RouteValue(ROUTE_MASKS[I]);
      Inc(LCount);
    end;
  SetLength(Result, LCount);
  if LCount = 0 then
  begin
    { A blocked nonterminal always retains the explicit nonparticipant. }
    if not IsFixedRoutePosition(AConfig.CaseKind, AX, AY, AZ) then
    begin
      SetLength(Result, 1);
      Result[0] := RouteValue(0);
    end;
  end;
end;

procedure ConfigureBasePass(const AGraph: TGraph;
  const ACase: TConnectedRoutesCase);
var
  X, Y, Z, W, H, D: Integer;
  LValue: TGraphValue;
begin
  ScenarioShape(ACase, W, H, D);
  AGraph.CurrentPass := PASS_TERRAIN;
  if ACase = crcCirculation3D then
    AGraph.CurrentPass := PASS_STRUCTURE;
  AGraph.PassMode := gpmOverlay;
  if ACase = crcTown2D then
  begin
    AGraph.AddValue(BASE_LAND);
    AGraph.AddValue(BASE_WATER);
    AGraph.AddValue(BASE_BRIDGE);
  end
  else
  begin
    AGraph.AddValue(BASE_FLOOR);
    AGraph.AddValue(BASE_WALL);
    AGraph.AddValue(BASE_SHAFT);
  end;
  for Z := 0 to D - 1 do
    for Y := 0 to H - 1 do
      for X := 0 to W - 1 do
      begin
        LValue := BaseValueAt(ACase, X, Y, Z);
        AGraph.SetAllowedValues(X, Y, Z, LValue);
      end;
end;

procedure ConfigureRoutePass(const AGraph: TGraph;
  const ACase: TConnectedRoutesCase);
var
  I: Integer;
  LBasePass: String;
  LWalkable: TGraphValues;
begin
  if ACase = crcTown2D then
  begin
    AGraph.SwitchToPass(PASS_ROADS);
    LBasePass := PASS_TERRAIN;
    SetLength(LWalkable, 2);
    LWalkable[0] := BASE_LAND;
    LWalkable[1] := BASE_BRIDGE;
  end
  else
  begin
    AGraph.SwitchToPass(PASS_CIRCULATION);
    LBasePass := PASS_STRUCTURE;
    SetLength(LWalkable, 2);
    LWalkable[0] := BASE_FLOOR;
    LWalkable[1] := BASE_SHAFT;
  end;
  AGraph.PassMode := gpmOverlay;
  AGraph.ClearDependencies;
  AGraph.AddValue(RouteValue(0), ROUTE_EMPTY_WEIGHT);
  for I := 1 to ROUTE_VALUE_COUNT - 1 do
    AGraph.AddValue(RouteValue(ROUTE_MASKS[I]), 1)
      .RequireFromPass(LBasePass, LWalkable);
end;

procedure ConfigureFeaturePass(const AGraph: TGraph;
  const ACase: TConnectedRoutesCase);
var
  X, Y, Z, W, H, D: Integer;
  LPosition: TGraphPosition;
  LRoutePass: String;
  LRouteValues: TGraphValues;
begin
  ScenarioShape(ACase, W, H, D);
  LRouteValues := AllRouteValues(False);
  if ACase = crcTown2D then
  begin
    AGraph.SwitchToPass(PASS_HOUSING);
    LRoutePass := PASS_ROADS;
    AGraph.AddValue(FEATURE_NONE);
    AGraph.AddValue(FEATURE_TOWN_ROOT)
      .RequireFromPass(LRoutePass, LRouteValues);
    AGraph.AddValue(FEATURE_TOWN_TERMINAL)
      .RequireFromPass(LRoutePass, LRouteValues);
  end
  else
  begin
    AGraph.SwitchToPass(PASS_FEATURES);
    LRoutePass := PASS_CIRCULATION;
    AGraph.AddValue(FEATURE_NONE);
    AGraph.AddValue(FEATURE_ENTRANCE)
      .RequireFromPass(LRoutePass, LRouteValues);
    AGraph.AddValue(FEATURE_GALLERY)
      .RequireFromPass(LRoutePass, LRouteValues);
  end;
  AGraph.PassMode := gpmOverlay;
  for Z := 0 to D - 1 do
    for Y := 0 to H - 1 do
      for X := 0 to W - 1 do
      begin
        LPosition := Position(X, Y, Z);
        AGraph.SetAllowedValues(X, Y, Z,
          FeatureValueAt(ACase, LPosition));
      end;
end;

function TConnectedRoutesSession.RoutePassLabel: String;
begin
  if FConfig.CaseKind = crcTown2D then
    Result := PASS_ROADS
  else
    Result := PASS_CIRCULATION;
end;

function TConnectedRoutesSession.RoutePassIndex: Integer;
begin
  Result := 1;
end;

function TConnectedRoutesSession.ProviderPassIndex: Integer;
begin
  Result := 0;
end;

procedure TConnectedRoutesSession.InvalidateCurrent;
begin
  FHasCurrent := False;
  FCurrent := Default(TConnectedRoutesResult);
end;

procedure TConnectedRoutesSession.ApplyRouteDomains;
var
  X, Y, Z, W, H, D: Integer;
  LRoute: TGraph;
  LValues: TGraphValues;
begin
  if FGraph = nil then
    raise EConnectedRoutesDemo.Create('connected-routes graph is unavailable');
  ScenarioShape(FConfig.CaseKind, W, H, D);
  LRoute := FGraph.PassGraph[RoutePassIndex()];
  for Z := 0 to D - 1 do
    for Y := 0 to H - 1 do
      for X := 0 to W - 1 do
      begin
        LValues := AllowedRouteValuesAt(FConfig, X, Y, Z, W, H, D);
        if Length(LValues) = 0 then
          LRoute.SetAllowedValues(X, Y, Z, [])
        else
          LRoute.SetAllowedValues(X, Y, Z, LValues);
      end;
end;

procedure TConnectedRoutesSession.ApplyConnectivity;
var
  I, LMaximumOrdinal: Integer;
  LConstraint: TGraphConnectivityConstraint;
  LProfiles: TGraphConnectivityValues;
  LRoute: TGraph;
begin
  if FGraph = nil then
    raise EConnectedRoutesDemo.Create('connected-routes graph is unavailable');
  if FConfig.CaseKind = crcTown2D then
    LMaximumOrdinal := 15
  else
    LMaximumOrdinal := ROUTE_VALUE_COUNT - 1;
  SetLength(LProfiles, LMaximumOrdinal);
  for I := 1 to LMaximumOrdinal do
    LProfiles[I - 1] := MakeGraphConnectivityValue(
      RouteValue(ROUTE_MASKS[I]), OpeningsForMask(ROUTE_MASKS[I]), False);
  LConstraint := MakeGraphConnectivityConstraint(CONNECTIVITY_LABEL,
    ScenarioRoot(FConfig.CaseKind), ScenarioRequired(FConfig.CaseKind),
    LProfiles, FConfig.RequireAllParticipants);
  LRoute := FGraph.PassGraph[RoutePassIndex()];
  LRoute.RemoveConnectivity(CONNECTIVITY_LABEL);
  LRoute.RequireConnectivity(LConstraint);
end;

procedure TConnectedRoutesSession.BuildGraph;
var
  W, H, D: Integer;
begin
  FGraph.Free;
  FGraph := nil;
  ScenarioShape(FConfig.CaseKind, W, H, D);
  FGraph := TGraph.Create;
  try
    FGraph.Seed := FConfig.Seed;
    FGraph.Reshape(W, H, D);
    FGraph.WrapNeighbors := False;
    ConfigureBasePass(FGraph, FConfig.CaseKind);
    ConfigureRoutePass(FGraph, FConfig.CaseKind);
    ConfigureFeaturePass(FGraph, FConfig.CaseKind);
    ApplyRouteDomains;
    ApplyConnectivity;
  except
    FGraph.Free;
    FGraph := nil;
    raise;
  end;
end;

constructor TConnectedRoutesSession.Create(
  const AConfig: TConnectedRoutesConfig);
begin
  inherited Create;
  Reset(AConfig);
end;

destructor TConnectedRoutesSession.Destroy;
begin
  FGraph.Free;
  inherited Destroy;
end;

procedure TConnectedRoutesSession.Reset(
  const AConfig: TConnectedRoutesConfig);
var
  LPreviousConfig: TConnectedRoutesConfig;
  LPreviousCurrent: TConnectedRoutesResult;
  LPreviousGraph: TGraph;
  LPreviousHasBaseline, LPreviousHasCurrent: Boolean;
  LPreviousProvider: TGraphValues;
begin
  ValidateConfig(AConfig);
  LPreviousConfig := FConfig;
  LPreviousGraph := FGraph;
  LPreviousHasBaseline := FHasBaseline;
  LPreviousHasCurrent := FHasCurrent;
  LPreviousCurrent := FCurrent;
  LPreviousProvider := FProviderBaseline;
  FGraph := nil;
  FConfig := AConfig;
  try
    BuildGraph;
  except
    FGraph.Free;
    FGraph := LPreviousGraph;
    FConfig := LPreviousConfig;
    FHasBaseline := LPreviousHasBaseline;
    FHasCurrent := LPreviousHasCurrent;
    FCurrent := LPreviousCurrent;
    FProviderBaseline := LPreviousProvider;
    raise;
  end;
  LPreviousGraph.Free;
  FHasBaseline := False;
  FProviderBaseline := nil;
  InvalidateCurrent;
end;

procedure TConnectedRoutesSession.SetPortal(
  const APortal: TConnectedRoutesPortal);
begin
  ValidateEnum(Ord(APortal), Ord(Low(TConnectedRoutesPortal)),
    Ord(High(TConnectedRoutesPortal)), 'connected-routes portal');
  if FConfig.Portal = APortal then Exit;
  InvalidateCurrent;
  FConfig.Portal := APortal;
  ApplyRouteDomains;
end;

procedure TConnectedRoutesSession.SetRequireAllParticipants(
  const AValue: Boolean);
begin
  {$IFDEF PAS2JS}
  if (AValue <> False) and (AValue <> True) then
    raise EConnectedRoutesDemo.Create(
      'all-participants flag must be Boolean');
  {$ENDIF}
  if FConfig.RequireAllParticipants = AValue then Exit;
  InvalidateCurrent;
  FConfig.RequireAllParticipants := AValue;
  ApplyConnectivity;
end;

procedure TConnectedRoutesSession.SetSearchLimits(
  const AMaxBacktracks, AMaxPassBacktracks: Integer;
  const ACaptureTrace: Boolean);
var
  LConfig: TConnectedRoutesConfig;
begin
  {$IFDEF PAS2JS}
  if (ACaptureTrace <> False) and (ACaptureTrace <> True) then
    raise EConnectedRoutesDemo.Create('trace flag must be Boolean');
  {$ENDIF}
  LConfig := FConfig;
  LConfig.MaxBacktracks := AMaxBacktracks;
  LConfig.MaxPassBacktracks := AMaxPassBacktracks;
  LConfig.CaptureTrace := ACaptureTrace;
  ValidateConfig(LConfig);
  if (FConfig.MaxBacktracks = AMaxBacktracks)
    and (FConfig.MaxPassBacktracks = AMaxPassBacktracks)
    and (FConfig.CaptureTrace = ACaptureTrace) then Exit;
  InvalidateCurrent;
  FConfig := LConfig;
end;

function SolveStatusName(const AStatus: TGraphSolveStatus): String;
begin
  case AStatus of
    gssSolved: Result := 'solved';
    gssContradiction: Result := 'contradiction';
    gssBacktrackLimit: Result := 'local-limit';
  else
    Result := 'unknown';
  end;
end;

function NegotiationStatusName(const AStatus: TGraphNegotiationStatus): String;
begin
  case AStatus of
    gnsSolved: Result := 'solved';
    gnsContradiction: Result := 'contradiction';
    gnsSolverBacktrackLimit: Result := 'local-limit';
    gnsPassBacktrackLimit: Result := 'pass-limit';
  else
    Result := 'unknown';
  end;
end;

function FailureResult(const AConfig: TConnectedRoutesConfig;
  const AWasRepair: Boolean; const AStatus: String;
  const AReport: TGraphSolveReport;
  const APassBacktracks: Integer): TConnectedRoutesResult;
var
  W, H, D: Integer;
begin
  Result := Default(TConnectedRoutesResult);
  ScenarioShape(AConfig.CaseKind, W, H, D);
  Result.CaseKind := AConfig.CaseKind;
  Result.Portal := AConfig.Portal;
  Result.RequireAllParticipants := AConfig.RequireAllParticipants;
  Result.WasRepair := AWasRepair;
  Result.Width := W;
  Result.Height := H;
  Result.Depth := D;
  Result.Root := ScenarioRoot(AConfig.CaseKind);
  Result.RequiredPositions := ScenarioRequired(AConfig.CaseKind);
  Result.Status := AStatus;
  Result.PassBacktracks := APassBacktracks;
  Result.FailedPassIndex := AReport.FailedPassIndex;
  Result.ConstraintIndex := AReport.Contradiction.ConstraintIndex;
  Result.Detail := 'No generated route is public. Terminal status=' +
    AStatus + '; failed pass=' + IntToStr(Result.FailedPassIndex);
  if AReport.Contradiction.Kind = gckConnectivity then
    Result.Detail := Result.Detail + '; connectivity clause=' +
      IntToStr(Result.ConstraintIndex);
  Result.Detail := Result.Detail + '.';
end;

function TConnectedRoutesSession.Generate(
  out AResult: TConnectedRoutesResult): Boolean;
var
  I, W, H, D: Integer;
  LCaptured: TConnectedRoutesResult;
  LOptions: TGraphSolveOptions;
  LProvider: TGraphValues;
  LReport: TGraphSolveReport;
begin
  ValidateConfig(FConfig);
  InvalidateCurrent;
  FHasBaseline := False;
  FProviderBaseline := nil;
  BuildGraph;
  LOptions := DefaultGraphSolveOptions;
  LOptions.MaxBacktracks := FConfig.MaxBacktracks;
  LOptions.CaptureTrace := FConfig.CaptureTrace;
  Result := FGraph.TrySolve(LOptions, LReport);
  if not Result then
  begin
    AResult := FailureResult(FConfig, False,
      SolveStatusName(LReport.Status), LReport, 0);
    Exit;
  end;
  ScenarioShape(FConfig.CaseKind, W, H, D);
  SetLength(LProvider, W * H * D);
  for I := 0 to High(LProvider) do
    LProvider[I] := FGraph.PassGraph[ProviderPassIndex()].Entry[
      I mod W, (I div W) mod H, I div (W * H)].Value;
  try
    LCaptured := CaptureResult(False, False, 0);
  except
    FProviderBaseline := nil;
    FHasBaseline := False;
    raise;
  end;
  FProviderBaseline := LProvider;
  FHasBaseline := True;
  FCurrent := LCaptured;
  FHasCurrent := True;
  AResult := CopyConnectedRoutesResult(FCurrent);
end;

function TConnectedRoutesSession.Repair(
  out AResult: TConnectedRoutesResult): Boolean;
var
  LCaptured: TConnectedRoutesResult;
  LNegotiation: TGraphNegotiationOptions;
  LProviderIndex: Integer;
  LReport: TGraphSelectiveNegotiationReport;
  LReused: Boolean;
begin
  ValidateConfig(FConfig);
  if not FHasBaseline then
    raise EConnectedRoutesDemo.Create(
      'selective repair needs a successful baseline generation');
  InvalidateCurrent;
  LNegotiation := DefaultGraphNegotiationOptions;
  LNegotiation.SolveOptions.MaxBacktracks := FConfig.MaxBacktracks;
  LNegotiation.SolveOptions.CaptureTrace := FConfig.CaptureTrace;
  LNegotiation.MaxPassBacktracks := FConfig.MaxPassBacktracks;
  Result := FGraph.TryRegenerateNegotiatedFrom(RoutePassLabel,
    LNegotiation, LReport);
  if not Result then
  begin
    AResult := FailureResult(FConfig, True,
      NegotiationStatusName(LReport.Search.Status),
      LReport.Search.FinalReport, LReport.Search.PassBacktracks);
    Exit;
  end;
  LProviderIndex := ProviderPassIndex();
  LReused := False;
  if Length(LReport.Search.FinalReport.Passes) = 3 then
    LReused := LReport.Search.FinalReport.Passes[
      LProviderIndex].Disposition = gpdReused;
  try
    LCaptured := CaptureResult(True, LReused,
      LReport.Search.PassBacktracks);
  except
    FProviderBaseline := nil;
    FHasBaseline := False;
    raise;
  end;
  FCurrent := LCaptured;
  FHasCurrent := True;
  AResult := CopyConnectedRoutesResult(FCurrent);
end;

function TConnectedRoutesSession.CopyCurrent(
  out AResult: TConnectedRoutesResult): Boolean;
begin
  Result := FHasCurrent;
  if Result then
    AResult := CopyConnectedRoutesResult(FCurrent)
  else
    AResult := Default(TConnectedRoutesResult);
end;

function BaseGlyph(const AValue: TGraphValue): Char;
begin
  if (AValue = BASE_LAND) or (AValue = BASE_FLOOR) then Result := '.'
  else if AValue = BASE_WATER then Result := '~'
  else if AValue = BASE_BRIDGE then Result := '='
  else if AValue = BASE_WALL then Result := '#'
  else if AValue = BASE_SHAFT then Result := 'S'
  else raise EConnectedRoutesDemo.Create('captured base has an unknown value');
end;

function FeatureGlyph(const AValue: TGraphValue): Char;
begin
  if AValue = FEATURE_NONE then Result := '.'
  else if (AValue = FEATURE_TOWN_ROOT) or (AValue = FEATURE_ENTRANCE) then
    Result := 'R'
  else if (AValue = FEATURE_TOWN_TERMINAL) or
      (AValue = FEATURE_GALLERY) then Result := 'T'
  else raise EConnectedRoutesDemo.Create(
    'captured feature has an unknown value');
end;

function IsRequiredPosition(const APosition: TGraphPosition;
  const ARequired: TGraphPositions): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(ARequired) do
    if PositionEqual(APosition, ARequired[I]) then Exit(True);
  Result := False;
end;

procedure IndependentlyValidate(var AResult: TConnectedRoutesResult);
var
  D: TGraphDirection;
  I, J, LCurrent, LHead, LNeighbor, LRootIndex, LTail: Integer;
  LMask, LNeighborMask: Integer;
  LParents, LQueue: TIntegerArray;
  LReachable: TBooleanArray;
  LPosition: TGraphPosition;
  X, Y, Z: Integer;
begin
  Require(AResult.Solved, 'cannot validate an unsolved result');
  Require(Length(AResult.Cells) = AResult.Width * AResult.Height * AResult.Depth,
    'captured result shape differs from its dimensions');
  Require(Length(AResult.RequiredPositions) > 0,
    'connected-routes fixture needs at least one terminal');
  SetLength(LReachable, Length(AResult.Cells));
  SetLength(LParents, Length(AResult.Cells));
  SetLength(LQueue, Length(AResult.Cells));
  for I := 0 to High(LParents) do LParents[I] := -1;

  AResult.ParticipantCount := 0;
  for I := 0 to High(AResult.Cells) do
  begin
    IndexCoordinate(I, AResult.Width, AResult.Height, X, Y, Z);
    LPosition := Position(X, Y, Z);
    Require(AResult.Cells[I].BaseValue =
      BaseValueAt(AResult.CaseKind, X, Y, Z),
      'captured provider differs from the fixed scenario');
    Require(AResult.Cells[I].FeatureValue =
      FeatureValueAt(AResult.CaseKind, LPosition),
      'captured feature differs from the fixed scenario');
    LMask := RouteMask(AResult.Cells[I].RouteValue);
    AResult.Cells[I].Openings := OpeningsForMask(LMask);
    AResult.Cells[I].Participant := LMask <> 0;
    AResult.Cells[I].OnWitness := False;
    if LMask <> 0 then Inc(AResult.ParticipantCount);
    if (FeatureValueAt(AResult.CaseKind, LPosition) <> FEATURE_NONE) then
      Require(LMask <> 0, 'a fixed terminal does not participate');

    if (AResult.CaseKind = crcTown2D) and (LMask <> 0) then
      Require((AResult.Cells[I].BaseValue = BASE_LAND)
        or (AResult.Cells[I].BaseValue = BASE_BRIDGE),
        'a road occupies water')
    else if (AResult.CaseKind = crcCirculation3D) and (LMask <> 0) then
      Require(AResult.Cells[I].BaseValue <> BASE_WALL,
        'circulation occupies a wall');

    { Route values deliberately have wildcard directional adjacency. The
      independent connectivity traversal below still requires reciprocal
      physical ports at both selected endpoints. A dangling opening is not an
      edge and does not become part of the witness. }
  end;

  LRootIndex := PositionIndex(AResult.Root, AResult.Width,
    AResult.Height, AResult.Depth);
  Require(AResult.Cells[LRootIndex].Participant,
    'connectivity root does not participate');
  LHead := 0;
  LTail := 1;
  LQueue[0] := LRootIndex;
  LReachable[LRootIndex] := True;
  while LHead < LTail do
  begin
    LCurrent := LQueue[LHead];
    Inc(LHead);
    LMask := RouteMask(AResult.Cells[LCurrent].RouteValue);
    for D := Low(TGraphDirection) to High(TGraphDirection) do
      if MaskHasDirection(LMask, D) and
          TryNeighbor(LCurrent, AResult.Width, AResult.Height,
            AResult.Depth, D, LNeighbor) and
          (not LReachable[LNeighbor]) then
      begin
        LNeighborMask := RouteMask(AResult.Cells[LNeighbor].RouteValue);
        if MaskHasDirection(LNeighborMask, InverseOfDir(D)) then
        begin
          LReachable[LNeighbor] := True;
          LParents[LNeighbor] := LCurrent;
          LQueue[LTail] := LNeighbor;
          Inc(LTail);
        end;
      end;
  end;

  for I := 0 to High(AResult.RequiredPositions) do
  begin
    J := PositionIndex(AResult.RequiredPositions[I], AResult.Width,
      AResult.Height, AResult.Depth);
    Require(LReachable[J], 'a fixed terminal is not reachable from the root');
    while J >= 0 do
    begin
      AResult.Cells[J].OnWitness := True;
      if J = LRootIndex then Break;
      J := LParents[J];
      Require(J >= 0, 'BFS predecessor chain did not reach the root');
    end;
  end;

  AResult.WitnessCellCount := 0;
  AResult.DisconnectedParticipantCount := 0;
  AResult.WitnessVerticalEdgeCount := 0;
  for I := 0 to High(AResult.Cells) do
  begin
    if AResult.Cells[I].OnWitness then Inc(AResult.WitnessCellCount);
    if AResult.Cells[I].Participant and not LReachable[I] then
      Inc(AResult.DisconnectedParticipantCount);
    if AResult.Cells[I].OnWitness
      and MaskHasDirection(RouteMask(AResult.Cells[I].RouteValue), gdUp)
      and TryNeighbor(I, AResult.Width, AResult.Height, AResult.Depth,
        gdUp, LNeighbor)
      and AResult.Cells[LNeighbor].OnWitness then
      Inc(AResult.WitnessVerticalEdgeCount);
  end;
  if AResult.RequireAllParticipants then
    Require(AResult.DisconnectedParticipantCount = 0,
      'all-participants mode retained a disconnected route component');

  if AResult.CaseKind = crcTown2D then
  begin
    J := CoordinateIndex(3, 1, 0, AResult.Width, AResult.Height);
    if PortalEnabled(AResult.Portal, True) and
        not PortalEnabled(AResult.Portal, False) then
      Require(AResult.Cells[J].OnWitness,
        'town witness did not use its only available bridge');
    J := CoordinateIndex(3, 3, 0, AResult.Width, AResult.Height);
    if PortalEnabled(AResult.Portal, False) and
        not PortalEnabled(AResult.Portal, True) then
      Require(AResult.Cells[J].OnWitness,
        'town witness did not use its only available bridge');
  end
  else
    Require(AResult.WitnessVerticalEdgeCount > 0,
      'circulation witness never crosses between floors');
  AResult.IndependentlyVerified := True;
end;

procedure HashByte(var AHash: Cardinal; const AValue: Byte);
{$PUSH}{$Q-}
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
  HashByte(AHash, Byte(AValue));
  HashByte(AHash, Byte(AValue shr 8));
  HashByte(AHash, Byte(AValue shr 16));
  HashByte(AHash, Byte(AValue shr 24));
end;

procedure HashText(var AHash: Cardinal; const AText: String);
var
  I: Integer;
begin
  HashCardinal(AHash, Cardinal(Length(AText)));
  for I := 1 to Length(AText) do
    HashByte(AHash, Byte(Ord(AText[I]) and $FF));
end;

function HexCardinal(const AValue: Cardinal): String;
begin
  Result := UpperCase(IntToHex(AValue, 8));
end;

function ResultOutputKey(const AResult: TConnectedRoutesResult): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AResult.Cells) do
    Result := Result + BaseGlyph(AResult.Cells[I].BaseValue);
  Result := Result + '/';
  for I := 0 to High(AResult.Cells) do
  begin
    if I > 0 then Result := Result + ',';
    Result := Result + LowerCase(IntToHex(
      RouteMask(AResult.Cells[I].RouteValue), 2));
  end;
  Result := Result + '/';
  for I := 0 to High(AResult.Cells) do
    Result := Result + FeatureGlyph(AResult.Cells[I].FeatureValue);
end;

function ResultSignature(const AResult: TConnectedRoutesResult): String;
var
  I: Integer;
  LHash: Cardinal;
begin
  LHash := Cardinal(2166136261);
  HashText(LHash, 'WFC connected routes');
  HashCardinal(LHash, CONNECTED_ROUTES_DEMO_VERSION);
  HashCardinal(LHash, WFC_GRAPH_CONNECTIVITY_VERSION);
  HashCardinal(LHash, Cardinal(Ord(AResult.CaseKind)));
  HashCardinal(LHash, Cardinal(Ord(AResult.Portal)));
  HashCardinal(LHash, Cardinal(Ord(AResult.RequireAllParticipants)));
  HashCardinal(LHash, Cardinal(AResult.Width));
  HashCardinal(LHash, Cardinal(AResult.Height));
  HashCardinal(LHash, Cardinal(AResult.Depth));
  for I := 0 to High(AResult.Cells) do
  begin
    HashText(LHash, AResult.Cells[I].BaseValue);
    HashText(LHash, AResult.Cells[I].RouteValue);
    HashText(LHash, AResult.Cells[I].FeatureValue);
    HashCardinal(LHash, Cardinal(Ord(AResult.Cells[I].OnWitness)));
  end;
  Result := HexCardinal(LHash);
end;

function XmlEscape(const AText: String): String;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AText) do
    case AText[I] of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&apos;';
    else
      if Ord(AText[I]) < 32 then
        raise EConnectedRoutesDemo.Create(
          'SVG text contains an unsupported control character')
      else
        Result := Result + AText[I];
    end;
end;

function BaseFill(const AValue: TGraphValue): String;
begin
  if (AValue = BASE_WATER) then Result := '#9bd7e8'
  else if (AValue = BASE_BRIDGE) then Result := '#e8c77b'
  else if (AValue = BASE_WALL) then Result := '#4a515b'
  else if (AValue = BASE_SHAFT) then Result := '#c5b3e6'
  else Result := '#edf0dc';
end;

function SvgForResult(const AResult: TConnectedRoutesResult): String;
const
  CELL = 48;
  PAD = 28;
  GAP = 46;
var
  D: TGraphDirection;
  I, LCenterX, LCenterY, LDisplayY, LNeighbor, LPanelX, LSvgHeight,
    LSvgWidth, X, Y, Z: Integer;
  LCell: TConnectedRoutesCell;
  LFeature, LStroke: String;
begin
  if not AResult.Solved or not AResult.IndependentlyVerified then
    raise EConnectedRoutesDemo.Create(
      'SVG requires a solved independently verified result');
  LSvgWidth := (PAD * 2) + (AResult.Width * CELL * AResult.Depth)
    + (GAP * (AResult.Depth - 1));
  LSvgHeight := (PAD * 2) + (AResult.Height * CELL) + 24;
  Result := '<?xml version="1.0" encoding="UTF-8"?>' + LF +
    '<svg xmlns="http://www.w3.org/2000/svg" width="' +
    IntToStr(LSvgWidth) + '" height="' + IntToStr(LSvgHeight) +
    '" viewBox="0 0 ' + IntToStr(LSvgWidth) + ' ' +
    IntToStr(LSvgHeight) + '" role="img" data-case="' +
    ConnectedRoutesCaseName(AResult.CaseKind) + '" data-signature="' +
    AResult.Signature + '">' + LF +
    '  <title>' + XmlEscape('Connected routes ' +
      ConnectedRoutesCaseName(AResult.CaseKind) + ' ' +
      AResult.Signature) + '</title>' + LF +
    '  <rect width="100%" height="100%" fill="#f8f5ed"/>' + LF;
  for Z := 0 to AResult.Depth - 1 do
  begin
    LPanelX := PAD + Z * ((AResult.Width * CELL) + GAP);
    Result := Result + '  <text x="' + IntToStr(LPanelX) + '" y="20" ' +
      'font-family="sans-serif" font-size="13" fill="#30343b">floor z=' +
      IntToStr(Z) + '</text>' + LF;
    for Y := 0 to AResult.Height - 1 do
      for X := 0 to AResult.Width - 1 do
      begin
        I := CoordinateIndex(X, Y, Z, AResult.Width, AResult.Height);
        LCell := AResult.Cells[I];
        LDisplayY := AResult.Height - 1 - Y;
        LCenterX := LPanelX + X * CELL + CELL div 2;
        LCenterY := PAD + LDisplayY * CELL + CELL div 2;
        Result := Result + '  <g data-x="' + IntToStr(X) + '" data-y="' +
          IntToStr(Y) + '" data-z="' + IntToStr(Z) + '" data-route="' +
          IntToStr(RouteMask(LCell.RouteValue)) + '">' + LF +
          '    <rect x="' + IntToStr(LPanelX + X * CELL) + '" y="' +
          IntToStr(PAD + LDisplayY * CELL) + '" width="' + IntToStr(CELL) +
          '" height="' + IntToStr(CELL) + '" fill="' +
          BaseFill(LCell.BaseValue) + '" stroke="#c2bcae"/>' + LF;
        if LCell.OnWitness then LStroke := '#d54f31'
        else LStroke := '#66717d';
        if LCell.Participant then
        begin
          for D := gdNorth to gdWest do
            if (D in LCell.Openings)
              and TryNeighbor(I, AResult.Width, AResult.Height,
                AResult.Depth, D, LNeighbor)
              and (InverseOfDir(D) in
                AResult.Cells[LNeighbor].Openings) then
            begin
              case D of
                gdNorth: Result := Result + '    <line x1="' +
                  IntToStr(LCenterX) + '" y1="' + IntToStr(LCenterY) +
                  '" x2="' + IntToStr(LCenterX) + '" y2="' +
                  IntToStr(LCenterY - CELL div 2) + '"';
                gdEast: Result := Result + '    <line x1="' +
                  IntToStr(LCenterX) + '" y1="' + IntToStr(LCenterY) +
                  '" x2="' + IntToStr(LCenterX + CELL div 2) + '" y2="' +
                  IntToStr(LCenterY) + '"';
                gdSouth: Result := Result + '    <line x1="' +
                  IntToStr(LCenterX) + '" y1="' + IntToStr(LCenterY) +
                  '" x2="' + IntToStr(LCenterX) + '" y2="' +
                  IntToStr(LCenterY + CELL div 2) + '"';
                gdWest: Result := Result + '    <line x1="' +
                  IntToStr(LCenterX) + '" y1="' + IntToStr(LCenterY) +
                  '" x2="' + IntToStr(LCenterX - CELL div 2) + '" y2="' +
                  IntToStr(LCenterY) + '"';
              end;
              Result := Result + ' stroke="' + LStroke +
                '" stroke-width="6" stroke-linecap="round"/>' + LF;
            end;
          Result := Result + '    <circle cx="' + IntToStr(LCenterX) +
            '" cy="' + IntToStr(LCenterY) + '" r="5" fill="' +
            LStroke + '"/>' + LF;
          if ((gdUp in LCell.Openings)
              and TryNeighbor(I, AResult.Width, AResult.Height,
                AResult.Depth, gdUp, LNeighbor)
              and (gdDown in AResult.Cells[LNeighbor].Openings))
            or ((gdDown in LCell.Openings)
              and TryNeighbor(I, AResult.Width, AResult.Height,
                AResult.Depth, gdDown, LNeighbor)
              and (gdUp in AResult.Cells[LNeighbor].Openings)) then
            Result := Result + '    <rect x="' + IntToStr(LCenterX - 8) +
              '" y="' + IntToStr(LCenterY - 8) +
              '" width="16" height="16" fill="none" stroke="#5c3e91" ' +
              'stroke-width="3"/>' + LF;
        end;
        LFeature := FeatureGlyph(LCell.FeatureValue);
        if LFeature <> '.' then
          Result := Result + '    <text x="' + IntToStr(LCenterX) +
            '" y="' + IntToStr(LCenterY + 5) +
            '" text-anchor="middle" font-family="sans-serif" ' +
            'font-size="14" font-weight="700" fill="#17191c">' +
            LFeature + '</text>' + LF;
        Result := Result + '  </g>' + LF;
      end;
  end;
  Result := Result + '</svg>' + LF;
end;

function TConnectedRoutesSession.CaptureResult(const AWasRepair,
  AProviderReused: Boolean;
  const APassBacktracks: Integer): TConnectedRoutesResult;
var
  I, W, H, D, X, Y, Z: Integer;
  LBase, LFeature, LRoute: TGraph;
begin
  Result := Default(TConnectedRoutesResult);
  ScenarioShape(FConfig.CaseKind, W, H, D);
  Result.Solved := True;
  Result.WasRepair := AWasRepair;
  Result.ProviderReused := AProviderReused;
  Result.CaseKind := FConfig.CaseKind;
  Result.Portal := FConfig.Portal;
  Result.RequireAllParticipants := FConfig.RequireAllParticipants;
  Result.Width := W;
  Result.Height := H;
  Result.Depth := D;
  Result.Root := ScenarioRoot(FConfig.CaseKind);
  Result.RequiredPositions := ScenarioRequired(FConfig.CaseKind);
  Result.PassBacktracks := APassBacktracks;
  Result.FailedPassIndex := -1;
  Result.ConstraintIndex := -1;
  SetLength(Result.Cells, W * H * D);
  LBase := FGraph.PassGraph[ProviderPassIndex()];
  LRoute := FGraph.PassGraph[RoutePassIndex()];
  LFeature := FGraph.PassGraph[2];
  for I := 0 to High(Result.Cells) do
  begin
    IndexCoordinate(I, W, H, X, Y, Z);
    Require((not LBase.Entry[X, Y, Z].Empty)
      and (not LRoute.Entry[X, Y, Z].Empty)
      and (not LFeature.Entry[X, Y, Z].Empty),
      'successful pipeline contains an empty captured cell');
    Result.Cells[I].BaseValue := LBase.Entry[X, Y, Z].Value;
    Result.Cells[I].RouteValue := LRoute.Entry[X, Y, Z].Value;
    Result.Cells[I].FeatureValue := LFeature.Entry[X, Y, Z].Value;
    if AWasRepair then
      Require((I <= High(FProviderBaseline)) and
        (Result.Cells[I].BaseValue = FProviderBaseline[I]),
        'selective repair changed its immutable provider');
  end;
  if AWasRepair then
    Require(AProviderReused,
      'selective report did not mark the upstream provider reused');
  IndependentlyValidate(Result);
  Result.OutputKey := ResultOutputKey(Result);
  Result.Signature := ResultSignature(Result);
  Result.Status := 'solved';
  Result.Detail := 'Root and ' + IntToStr(Length(Result.RequiredPositions)) +
    ' terminals connected; witness cells=' +
    IntToStr(Result.WitnessCellCount) + '; participants=' +
    IntToStr(Result.ParticipantCount) + '; disconnected optional=' +
    IntToStr(Result.DisconnectedParticipantCount) + '; pass backtracks=' +
    IntToStr(Result.PassBacktracks) + '.';
  Result.SvgText := SvgForResult(Result);
end;

function SameProvider(const ALeft,
  ARight: TConnectedRoutesResult): Boolean;
var
  I: Integer;
begin
  Result := Length(ALeft.Cells) = Length(ARight.Cells);
  if not Result then Exit;
  for I := 0 to High(ALeft.Cells) do
    if ALeft.Cells[I].BaseValue <> ARight.Cells[I].BaseValue then
      Exit(False);
end;

function SameRoute(const ALeft,
  ARight: TConnectedRoutesResult): Boolean;
var
  I: Integer;
begin
  Result := Length(ALeft.Cells) = Length(ARight.Cells);
  if not Result then Exit;
  for I := 0 to High(ALeft.Cells) do
    if ALeft.Cells[I].RouteValue <> ARight.Cells[I].RouteValue then
      Exit(False);
end;

function ConnectedRoutesSelfTest: Integer;
var
  C: TConnectedRoutesConfig;
  CopyResult, First, Failed, Replay, Second: TConnectedRoutesResult;
  Session: TConnectedRoutesSession;

  procedure Check(const ACondition: Boolean; const AMessage: String);
  begin
    Inc(Result);
    Require(ACondition, 'connected routes self-test: ' + AMessage);
  end;

  procedure CheckSvg(const AValue: TConnectedRoutesResult;
    const ACaseText: String);
  begin
    Check(Pos('<?xml version="1.0" encoding="UTF-8"?>' + LF,
      AValue.SvgText) = 1, ACaseText + ' SVG declaration');
    Check(Pos(#13, AValue.SvgText) = 0,
      ACaseText + ' SVG is canonical LF-only text');
    Check(Pos('data-case="' + ACaseText + '"', AValue.SvgText) > 0,
      ACaseText + ' SVG case metadata');
    Check(Pos('data-signature="' + AValue.Signature + '"',
      AValue.SvgText) > 0, ACaseText + ' SVG signature metadata');
    Check(Pos('data-z="' + IntToStr(AValue.Depth - 1) + '"',
      AValue.SvgText) > 0, ACaseText + ' SVG depth coordinate');
  end;

begin
  Result := 0;
  Check(CONNECTED_ROUTES_DEMO_VERSION = 1, 'demo version changed');
  Check(WFC_GRAPH_CONNECTIVITY_VERSION = 1,
    'graph connectivity version changed');

  C := DefaultConnectedRoutesConfig(crcTown2D);
  Session := TConnectedRoutesSession.Create(C);
  try
    Check(Session.Generate(First), 'town first crossing did not solve');
    Check(First.Solved and First.IndependentlyVerified,
      'town result was not independently verified');
    Check((First.Width = 7) and (First.Height = 5) and (First.Depth = 1),
      'town dimensions changed');
    Check((First.WitnessCellCount > 0) and
      (First.DisconnectedParticipantCount = 0),
      'town witness or all-participant result changed');
    Check(First.Signature = TOWN_FIRST_SIGNATURE,
      'town first-crossing golden changed: expected ' +
      TOWN_FIRST_SIGNATURE + ', actual ' + First.Signature +
      ', output ' + First.OutputKey);
    CheckSvg(First, 'town');

    Check(Session.CopyCurrent(CopyResult), 'town current result unavailable');
    CopyResult.Cells[0].BaseValue := 'changed';
    CopyResult.RequiredPositions[0] := Position(0, 0, 0);
    Check(Session.CopyCurrent(CopyResult) and
      (CopyResult.Cells[0].BaseValue = First.Cells[0].BaseValue) and
      PositionEqual(CopyResult.RequiredPositions[0],
        First.RequiredPositions[0]), 'result copies retain no caller aliases');

    Session.SetPortal(crpSecond);
    Check(not Session.HasCurrent, 'crossing edit retained stale public output');
    Check(Session.Repair(Second), 'town alternate crossing did not repair');
    Check(Second.ProviderReused and SameProvider(First, Second),
      'town repair changed terrain');
    Check(not SameRoute(First, Second),
      'town crossing edit did not reroute roads');
    Check(Second.Signature = TOWN_SECOND_SIGNATURE,
      'town second-crossing golden changed: expected ' +
      TOWN_SECOND_SIGNATURE + ', actual ' + Second.Signature +
      ', output ' + Second.OutputKey);
    Check(Second.WasRepair and Second.IndependentlyVerified,
      'town repair result was not verified');

    Session.SetPortal(crpNone);
    Check(not Session.Repair(Failed),
      'town with both crossings closed unexpectedly solved');
    Check((not Failed.Solved) and (Length(Failed.Cells) = 0)
      and (Failed.SvgText = '') and (not Session.HasCurrent),
      'town failure exposed stale output');
    Check(Session.HasBaseline,
      'town failed repair discarded the rollback baseline');
    Session.SetPortal(crpFirst);
    Check(Session.Repair(Replay), 'town rollback recovery did not solve');
    Check(SameRoute(First, Replay) and SameProvider(First, Replay),
      'town rollback recovery did not restore the first crossing result');
  finally
    Session.Free;
  end;

  Session := TConnectedRoutesSession.Create(
    DefaultConnectedRoutesConfig(crcTown2D));
  try
    Check(Session.Generate(Replay) and (Replay.Signature = First.Signature)
      and SameRoute(Replay, First), 'town same-seed replay changed');
  finally
    Session.Free;
  end;

  C := DefaultConnectedRoutesConfig(crcCirculation3D);
  Session := TConnectedRoutesSession.Create(C);
  try
    Check(Session.Generate(First), 'circulation first shaft did not solve');
    Check(First.Solved and First.IndependentlyVerified and
      (First.WitnessVerticalEdgeCount > 0),
      'circulation did not prove a cross-floor witness');
    Check((First.Width = 5) and (First.Height = 4) and (First.Depth = 2),
      'circulation dimensions changed');
    Check(First.Signature = CIRCULATION_FIRST_SIGNATURE,
      'circulation first-shaft golden changed: expected ' +
      CIRCULATION_FIRST_SIGNATURE + ', actual ' + First.Signature +
      ', output ' + First.OutputKey);
    CheckSvg(First, 'circulation');

    Session.SetPortal(crpSecond);
    Check(not Session.HasCurrent, 'shaft edit retained stale public output');
    Check(Session.Repair(Second), 'alternate shaft did not repair');
    Check(Second.ProviderReused and SameProvider(First, Second),
      'circulation repair changed structure');
    Check((Second.WitnessVerticalEdgeCount > 0) and
      (not SameRoute(First, Second)),
      'shaft edit did not reroute cross-floor circulation');
    Check(Second.Signature = CIRCULATION_SECOND_SIGNATURE,
      'circulation second-shaft golden changed: expected ' +
      CIRCULATION_SECOND_SIGNATURE + ', actual ' + Second.Signature +
      ', output ' + Second.OutputKey);

    Session.SetPortal(crpNone);
    Check(not Session.Repair(Failed),
      'circulation without a shaft unexpectedly solved');
    Check((Length(Failed.Cells) = 0) and (Failed.SvgText = '')
      and (not Session.HasCurrent),
      'circulation failure exposed stale output');
    Session.SetPortal(crpFirst);
    Check(Session.Repair(Replay) and SameRoute(First, Replay),
      'circulation rollback recovery changed the first-shaft result');
  finally
    Session.Free;
  end;

  C := DefaultConnectedRoutesConfig(crcTown2D);
  C.RequireAllParticipants := False;
  Session := TConnectedRoutesSession.Create(C);
  try
    Check(Session.Generate(Replay) and Replay.IndependentlyVerified,
      'required-terminals-only mode did not solve');
  finally
    Session.Free;
  end;

  C := DefaultConnectedRoutesConfig(crcTown2D);
  C.MaxBacktracks := -1;
  try
    Session := TConnectedRoutesSession.Create(C);
    Session.Free;
    Check(False, 'negative search allowance was accepted');
  except
    on E: EConnectedRoutesDemo do
      Check(Pos('cannot be negative', E.Message) > 0,
        'negative search allowance diagnostic changed');
  end;
end;

end.
