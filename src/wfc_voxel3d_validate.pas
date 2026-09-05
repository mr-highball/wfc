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
unit wfc_voxel3d_validate;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_voxel3d;

const
  WFC_VOXEL3D_VALIDATION_VERSION = 1;

type
  TVoxel3DValidationIssueKind = (
    v3vikNone,
    v3vikKitIdentity,
    v3vikSocketMismatch,
    v3vikUnsupported,
    v3vikMissingEntrance,
    v3vikEntranceNotBoundaryFacing,
    v3vikUnreachable
  );

  TVoxel3DValidationIssue = record
    Kind: TVoxel3DValidationIssueKind;
    X: Integer;
    Y: Integer;
    Z: Integer;
    NeighborX: Integer;
    NeighborY: Integer;
    NeighborZ: Integer;
    HasDirection: Boolean;
    Direction: TGraphDirection;
    VariantIndex: Integer;
    NeighborVariantIndex: Integer;
  end;

  TVoxel3DValidationOptions = record
    CheckSockets: Boolean;
    CheckSupport: Boolean;
    RequireEntrance: Boolean;
    RequireBoundaryFacingEntrance: Boolean;
    CheckRequiredReachability: Boolean;
  end;

  TVoxel3DValidationReport = record
    Valid: Boolean;
    { Diagnostic counters saturate at High(Integer) rather than wrapping. }
    CheckedCells: Integer;
    CheckedRelations: Integer;
    EntranceCount: Integer;
    ReachableCount: Integer;
    Issue: TVoxel3DValidationIssue;
  end;

function DefaultVoxel3DValidationOptions: TVoxel3DValidationOptions;

function ValidateVoxel3DScene(const AKit: TVoxel3DKit;
  const AScene: TVoxel3DScene;
  const AOptions: TVoxel3DValidationOptions;
  out AReport: TVoxel3DValidationReport): Boolean; overload;

function ValidateVoxel3DScene(const AKit: TVoxel3DKit;
  const AScene: TVoxel3DScene;
  out AReport: TVoxel3DValidationReport): Boolean; overload;

function Voxel3DValidationIssueName(
  const AKind: TVoxel3DValidationIssueKind): String;

function DescribeVoxel3DValidationIssue(const AKit: TVoxel3DKit;
  const AIssue: TVoxel3DValidationIssue): String;

implementation

type
  TVoxel3DBytes = array of Byte;
  TVoxel3DIntegers = array of Integer;

function DefaultVoxel3DValidationOptions: TVoxel3DValidationOptions;
begin
  Result.CheckSockets := True;
  Result.CheckSupport := True;
  Result.RequireEntrance := True;
  Result.RequireBoundaryFacingEntrance := True;
  Result.CheckRequiredReachability := True;
end;

procedure InitializeReport(out AReport: TVoxel3DValidationReport);
begin
  AReport := Default(TVoxel3DValidationReport);
  AReport.Valid := False;
  AReport.Issue.Kind := v3vikNone;
  AReport.Issue.X := -1;
  AReport.Issue.Y := -1;
  AReport.Issue.Z := -1;
  AReport.Issue.NeighborX := -1;
  AReport.Issue.NeighborY := -1;
  AReport.Issue.NeighborZ := -1;
  AReport.Issue.HasDirection := False;
  AReport.Issue.Direction := gdNorth;
  AReport.Issue.VariantIndex := -1;
  AReport.Issue.NeighborVariantIndex := -1;
end;

procedure IncrementReportCounter(var AValue: Integer);
begin
  if AValue < High(Integer) then
    Inc(AValue);
end;

procedure SetIssue(var AReport: TVoxel3DValidationReport;
  const AKind: TVoxel3DValidationIssueKind;
  const AX, AY, AZ, AVariantIndex: Integer);
begin
  AReport.Valid := False;
  AReport.Issue.Kind := AKind;
  AReport.Issue.X := AX;
  AReport.Issue.Y := AY;
  AReport.Issue.Z := AZ;
  AReport.Issue.NeighborX := -1;
  AReport.Issue.NeighborY := -1;
  AReport.Issue.NeighborZ := -1;
  AReport.Issue.HasDirection := False;
  AReport.Issue.Direction := gdNorth;
  AReport.Issue.VariantIndex := AVariantIndex;
  AReport.Issue.NeighborVariantIndex := -1;
end;

procedure SetRelationIssue(var AReport: TVoxel3DValidationReport;
  const AKind: TVoxel3DValidationIssueKind;
  const AX, AY, AZ, AVariantIndex: Integer;
  const ADirection: TGraphDirection;
  const ANeighborX, ANeighborY, ANeighborZ,
  ANeighborVariantIndex: Integer);
begin
  SetIssue(AReport, AKind, AX, AY, AZ, AVariantIndex);
  AReport.Issue.HasDirection := True;
  AReport.Issue.Direction := ADirection;
  AReport.Issue.NeighborX := ANeighborX;
  AReport.Issue.NeighborY := ANeighborY;
  AReport.Issue.NeighborZ := ANeighborZ;
  AReport.Issue.NeighborVariantIndex := ANeighborVariantIndex;
end;

function CoordToIndex(const AX, AY, AZ,
  AWidth, AHeight: Integer): Integer;
begin
  Result := (AZ * AWidth * AHeight) + (AY * AWidth) + AX;
end;

procedure IndexToCoord(const AIndex, AWidth, AHeight: Integer;
  out AX, AY, AZ: Integer);
var
  LPlane: Integer;
begin
  LPlane := AWidth * AHeight;
  AZ := AIndex div LPlane;
  AY := (AIndex mod LPlane) div AWidth;
  AX := AIndex mod AWidth;
end;

function TryResolveNeighbor(const AX, AY, AZ,
  AWidth, AHeight, ADepth: Integer;
  const AWrap: Boolean; const ADirection: TGraphDirection;
  out ANeighborX, ANeighborY, ANeighborZ: Integer): Boolean;
begin
  ANeighborX := AX;
  ANeighborY := AY;
  ANeighborZ := AZ;
  if (Ord(ADirection) < Ord(Low(TGraphDirection))) or
      (Ord(ADirection) > Ord(High(TGraphDirection))) then
    raise ERangeError.Create(
      'voxel validation direction is out of bounds');
  case ADirection of
    gdNorth:
      Inc(ANeighborY);
    gdEast:
      Inc(ANeighborX);
    gdSouth:
      Dec(ANeighborY);
    gdWest:
      Dec(ANeighborX);
    gdUp:
      Inc(ANeighborZ);
    gdDown:
      Dec(ANeighborZ);
  end;

  if AWrap then
  begin
    if ANeighborX < 0 then
      ANeighborX := AWidth - 1
    else if ANeighborX >= AWidth then
      ANeighborX := 0;
    if ANeighborY < 0 then
      ANeighborY := AHeight - 1
    else if ANeighborY >= AHeight then
      ANeighborY := 0;
    if ANeighborZ < 0 then
      ANeighborZ := ADepth - 1
    else if ANeighborZ >= ADepth then
      ANeighborZ := 0;
    Exit(True);
  end;

  Result := (ANeighborX >= 0) and (ANeighborX < AWidth) and
    (ANeighborY >= 0) and (ANeighborY < AHeight) and
    (ANeighborZ >= 0) and (ANeighborZ < ADepth);
end;

function IsBoundaryFacingEntrance(const AScene: TVoxel3DScene;
  const AX, AY, AZ: Integer; const AVariant: TVoxel3DVariant): Boolean;
var
  D: TGraphDirection;
  LNeighborX, LNeighborY, LNeighborZ: Integer;
begin
  Result := False;
  if AScene.WrapNeighbors then
    Exit;
  for D := Low(TGraphDirection) to High(TGraphDirection) do
    if (D in AVariant.WalkOpenings) and
        (not TryResolveNeighbor(AX, AY, AZ,
          Integer(AScene.Width), Integer(AScene.Height),
          Integer(AScene.Depth), False, D,
          LNeighborX, LNeighborY, LNeighborZ)) then
      Exit(True);
end;

function ValidateVoxel3DScene(const AKit: TVoxel3DKit;
  const AScene: TVoxel3DScene;
  const AOptions: TVoxel3DValidationOptions;
  out AReport: TVoxel3DValidationReport): Boolean;
var
  D: TGraphDirection;
  I: Integer;
  LCurrentIndex: Integer;
  LCurrentVariant: TVoxel3DVariant;
  LNeighborIndex: Integer;
  LNeighborVariantIndex: Integer;
  LNeighborVariant: TVoxel3DVariant;
  LQueue: TVoxel3DIntegers;
  LQueueHead, LQueueTail: Integer;
  LVisited: TVoxel3DBytes;
  LWidth, LHeight, LDepth: Integer;
  X, Y, Z: Integer;
  LNeighborX, LNeighborY, LNeighborZ: Integer;
begin
  if not Assigned(AKit) then
    raise EArgumentNilException.Create('voxel validation kit cannot be nil');
  if not Assigned(AScene) then
    raise EArgumentNilException.Create('voxel validation scene cannot be nil');

  InitializeReport(AReport);
  if not AKit.MatchesScene(AScene) then
  begin
    SetIssue(AReport, v3vikKitIdentity, -1, -1, -1, -1);
    Exit(False);
  end;

  LWidth := Integer(AScene.Width);
  LHeight := Integer(AScene.Height);
  LDepth := Integer(AScene.Depth);

  for Z := 0 to LDepth - 1 do
    for Y := 0 to LHeight - 1 do
      for X := 0 to LWidth - 1 do
      begin
        LCurrentIndex := AScene.VariantIndexAt(X, Y, Z);
        LCurrentVariant := AScene.VariantAt(LCurrentIndex);
        IncrementReportCounter(AReport.CheckedCells);

        if v3pfEntrance in LCurrentVariant.Flags then
        begin
          IncrementReportCounter(AReport.EntranceCount);
          if AOptions.RequireBoundaryFacingEntrance and
              (not IsBoundaryFacingEntrance(AScene, X, Y, Z,
                LCurrentVariant)) then
          begin
            SetIssue(AReport, v3vikEntranceNotBoundaryFacing,
              X, Y, Z, LCurrentIndex);
            Exit(False);
          end;
        end;

        if AOptions.CheckSockets then
          for D := Low(TGraphDirection) to High(TGraphDirection) do
          begin
            if not TryResolveNeighbor(X, Y, Z, LWidth, LHeight,
                LDepth, AScene.WrapNeighbors, D,
                LNeighborX, LNeighborY, LNeighborZ) then
              Continue;
            LNeighborVariantIndex := AScene.VariantIndexAt(
              LNeighborX, LNeighborY, LNeighborZ);
            LNeighborVariant := AScene.VariantAt(LNeighborVariantIndex);
            IncrementReportCounter(AReport.CheckedRelations);
            if not AKit.SocketsCompatible(
                LCurrentVariant.Sockets[D],
                LNeighborVariant.Sockets[
                  OppositeVoxel3DDirection(D)]) then
            begin
              SetRelationIssue(AReport, v3vikSocketMismatch,
                X, Y, Z, LCurrentIndex, D,
                LNeighborX, LNeighborY, LNeighborZ,
                LNeighborVariantIndex);
              Exit(False);
            end;
          end;

        if AOptions.CheckSupport and
            (v3pfRequiresSupport in LCurrentVariant.Flags) and
            TryResolveNeighbor(X, Y, Z, LWidth, LHeight, LDepth,
              AScene.WrapNeighbors, gdDown,
              LNeighborX, LNeighborY, LNeighborZ) then
        begin
          LNeighborVariantIndex := AScene.VariantIndexAt(
            LNeighborX, LNeighborY, LNeighborZ);
          LNeighborVariant := AScene.VariantAt(LNeighborVariantIndex);
          IncrementReportCounter(AReport.CheckedRelations);
          if not (v3pfProvidesSupport in LNeighborVariant.Flags) then
          begin
            SetRelationIssue(AReport, v3vikUnsupported,
              X, Y, Z, LCurrentIndex, gdDown,
              LNeighborX, LNeighborY, LNeighborZ,
              LNeighborVariantIndex);
            Exit(False);
          end;
        end;
      end;

  if AOptions.RequireEntrance and (AReport.EntranceCount = 0) then
  begin
    SetIssue(AReport, v3vikMissingEntrance, -1, -1, -1, -1);
    Exit(False);
  end;

  if AOptions.CheckRequiredReachability then
  begin
    SetLength(LVisited, AScene.CellCount);
    SetLength(LQueue, AScene.CellCount);
    LQueueHead := 0;
    LQueueTail := 0;

    for Z := 0 to LDepth - 1 do
      for Y := 0 to LHeight - 1 do
        for X := 0 to LWidth - 1 do
        begin
          LCurrentIndex := AScene.VariantIndexAt(X, Y, Z);
          LCurrentVariant := AScene.VariantAt(LCurrentIndex);
          if v3pfEntrance in LCurrentVariant.Flags then
          begin
            I := CoordToIndex(X, Y, Z, LWidth, LHeight);
            if LVisited[I] = 0 then
            begin
              LVisited[I] := 1;
              LQueue[LQueueTail] := I;
              Inc(LQueueTail);
            end;
          end;
        end;

    while LQueueHead < LQueueTail do
    begin
      I := LQueue[LQueueHead];
      Inc(LQueueHead);
      IncrementReportCounter(AReport.ReachableCount);
      IndexToCoord(I, LWidth, LHeight, X, Y, Z);
      LCurrentIndex := AScene.VariantIndexAt(X, Y, Z);
      LCurrentVariant := AScene.VariantAt(LCurrentIndex);
      for D := Low(TGraphDirection) to High(TGraphDirection) do
      begin
        if not (D in LCurrentVariant.WalkOpenings) then
          Continue;
        if not TryResolveNeighbor(X, Y, Z, LWidth, LHeight,
            LDepth, AScene.WrapNeighbors, D,
            LNeighborX, LNeighborY, LNeighborZ) then
          Continue;
        LNeighborVariantIndex := AScene.VariantIndexAt(
          LNeighborX, LNeighborY, LNeighborZ);
        LNeighborVariant := AScene.VariantAt(LNeighborVariantIndex);
        IncrementReportCounter(AReport.CheckedRelations);
        if not (v3pfWalkable in LNeighborVariant.Flags) or
            not (OppositeVoxel3DDirection(D) in
              LNeighborVariant.WalkOpenings) then
          Continue;
        LNeighborIndex := CoordToIndex(LNeighborX, LNeighborY,
          LNeighborZ, LWidth, LHeight);
        if LVisited[LNeighborIndex] = 0 then
        begin
          LVisited[LNeighborIndex] := 1;
          LQueue[LQueueTail] := LNeighborIndex;
          Inc(LQueueTail);
        end;
      end;
    end;

    for I := 0 to AScene.CellCount - 1 do
    begin
      IndexToCoord(I, LWidth, LHeight, X, Y, Z);
      LCurrentIndex := AScene.VariantIndexAt(X, Y, Z);
      LCurrentVariant := AScene.VariantAt(LCurrentIndex);
      if (v3pfRequiredReachable in LCurrentVariant.Flags) and
          (LVisited[I] = 0) then
      begin
        SetIssue(AReport, v3vikUnreachable,
          X, Y, Z, LCurrentIndex);
        Exit(False);
      end;
    end;
  end;

  AReport.Valid := True;
  AReport.Issue.Kind := v3vikNone;
  Result := True;
end;

function ValidateVoxel3DScene(const AKit: TVoxel3DKit;
  const AScene: TVoxel3DScene;
  out AReport: TVoxel3DValidationReport): Boolean;
begin
  Result := ValidateVoxel3DScene(AKit, AScene,
    DefaultVoxel3DValidationOptions, AReport);
end;

function Voxel3DValidationIssueName(
  const AKind: TVoxel3DValidationIssueKind): String;
begin
  if (Ord(AKind) < Ord(Low(TVoxel3DValidationIssueKind))) or
      (Ord(AKind) > Ord(High(TVoxel3DValidationIssueKind))) then
    Exit('unknown');
  case AKind of
    v3vikNone:
      Result := 'none';
    v3vikKitIdentity:
      Result := 'kit-identity';
    v3vikSocketMismatch:
      Result := 'socket-mismatch';
    v3vikUnsupported:
      Result := 'unsupported';
    v3vikMissingEntrance:
      Result := 'missing-entrance';
    v3vikEntranceNotBoundaryFacing:
      Result := 'entrance-not-boundary-facing';
    v3vikUnreachable:
      Result := 'unreachable';
  end;
end;

function DescribeVoxel3DValidationIssue(const AKit: TVoxel3DKit;
  const AIssue: TVoxel3DValidationIssue): String;
const
  DIRECTION_NAMES: array[TGraphDirection] of String = (
    'north', 'east', 'south', 'west', 'up', 'down'
  );
var
  LVariant: TVoxel3DVariant;
begin
  Result := Voxel3DValidationIssueName(AIssue.Kind);
  if (AIssue.X >= 0) and (AIssue.Y >= 0) and (AIssue.Z >= 0) then
    Result := Result + ' at (' + IntToStr(AIssue.X) + ',' +
      IntToStr(AIssue.Y) + ',' + IntToStr(AIssue.Z) + ')';
  if Assigned(AKit) and (AIssue.VariantIndex >= 0) and
      (AIssue.VariantIndex < AKit.VariantCount) then
  begin
    LVariant := AKit.VariantAt(AIssue.VariantIndex);
    Result := Result + ' [' + LVariant.PrototypeId + '@' +
      IntToStr(Voxel3DRotationDegrees(LVariant.Rotation)) + ']';
  end;
  if AIssue.HasDirection then
    Result := Result + ' toward ' + DIRECTION_NAMES[AIssue.Direction];
  if (AIssue.NeighborX >= 0) and (AIssue.NeighborY >= 0) and
      (AIssue.NeighborZ >= 0) then
    Result := Result + ' (' + IntToStr(AIssue.NeighborX) + ',' +
      IntToStr(AIssue.NeighborY) + ',' +
      IntToStr(AIssue.NeighborZ) + ')';
  if Assigned(AKit) and (AIssue.NeighborVariantIndex >= 0) and
      (AIssue.NeighborVariantIndex < AKit.VariantCount) then
  begin
    LVariant := AKit.VariantAt(AIssue.NeighborVariantIndex);
    Result := Result + ' [' + LVariant.PrototypeId + '@' +
      IntToStr(Voxel3DRotationDegrees(LVariant.Rotation)) + ']';
  end;
end;

end.
