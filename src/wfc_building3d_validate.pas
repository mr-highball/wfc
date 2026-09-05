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
unit wfc_building3d_validate;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_voxel3d,
  wfc_voxel3d_validate,
  wfc_building3d;

const
  WFC_BUILDING3D_VALIDATION_VERSION = 1;

type
  TBuilding3DValidationIssueKind = (
    b3vikNone,
    b3vikUnsolved,
    b3vikPipelineDefinition,
    b3vikStageValue,
    b3vikFootprintStructure,
    b3vikStructureEnvelope,
    b3vikPropPlacement,
    b3vikMissingFeature,
    b3vikStructureVoxel
  );

  TBuilding3DValidationIssue = record
    Kind: TBuilding3DValidationIssueKind;
    Stage: TBuilding3DStage;
    X: Integer;
    Y: Integer;
    Z: Integer;
    Expected: String;
    Actual: String;
    VoxelIssue: TVoxel3DValidationIssue;
  end;

  TBuilding3DValidationOptions = record
    Structure: TVoxel3DValidationOptions;
    RequireFeature: Boolean;
  end;

  TBuilding3DValidationReport = record
    Valid: Boolean;
    CheckedCells: Integer;
    FeatureCount: Integer;
    PropCount: Integer;
    Structure: TVoxel3DValidationReport;
    Issue: TBuilding3DValidationIssue;
  end;

function DefaultBuilding3DValidationOptions:
  TBuilding3DValidationOptions;

function ValidateBuilding3D(const ABuilding: TBuilding3D;
  const AOptions: TBuilding3DValidationOptions;
  out AReport: TBuilding3DValidationReport): Boolean; overload;

function ValidateBuilding3D(const ABuilding: TBuilding3D;
  out AReport: TBuilding3DValidationReport): Boolean; overload;

function Building3DValidationIssueName(
  const AKind: TBuilding3DValidationIssueKind): String;
function DescribeBuilding3DValidationIssue(
  const AIssue: TBuilding3DValidationIssue): String;

implementation

function DefaultBuilding3DValidationOptions:
  TBuilding3DValidationOptions;
begin
  Result.Structure := DefaultVoxel3DValidationOptions;
  Result.RequireFeature := False;
end;

procedure InitializeReport(out AReport: TBuilding3DValidationReport);
begin
  AReport := Default(TBuilding3DValidationReport);
  AReport.Valid := False;
  AReport.Issue.Kind := b3vikNone;
  AReport.Issue.Stage := b3sFootprint;
  AReport.Issue.X := -1;
  AReport.Issue.Y := -1;
  AReport.Issue.Z := -1;
  AReport.Issue.VoxelIssue.X := -1;
  AReport.Issue.VoxelIssue.Y := -1;
  AReport.Issue.VoxelIssue.Z := -1;
end;

procedure IncrementCounter(var AValue: Integer);
begin
  if AValue < High(Integer) then
    Inc(AValue);
end;

procedure SetIssue(var AReport: TBuilding3DValidationReport;
  const AKind: TBuilding3DValidationIssueKind;
  const AStage: TBuilding3DStage;
  const AX, AY, AZ: Integer;
  const AExpected, AActual: String);
begin
  AReport.Valid := False;
  AReport.Issue.Kind := AKind;
  AReport.Issue.Stage := AStage;
  AReport.Issue.X := AX;
  AReport.Issue.Y := AY;
  AReport.Issue.Z := AZ;
  AReport.Issue.Expected := AExpected;
  AReport.Issue.Actual := AActual;
end;

function StructureMatchesFootprint(
  const ARole: TBuilding3DFootprintRole;
  const AKind: TBuilding3DStructureKind;
  const ARotation: TVoxel3DRotation): Boolean;
begin
  Result := False;
  case ARole of
    b3frVoid:
      Result := AKind = b3skVoidAir;
    b3frGroundShell:
      Result := AKind = b3skFoundation;
    b3frShell:
      Result := AKind in [b3skWall, b3skWindow];
    b3frInterior:
      Result := AKind = b3skInteriorAir;
    b3frFeature:
      Result := AKind = b3skFeatureAir;
    b3frLintel:
      Result := AKind = b3skLintel;
    b3frRoof:
      Result := AKind = b3skRoofSpan;
    b3frEntranceNorth, b3frEntranceSouth:
      Result := (AKind = b3skDoor) and (ARotation = v3r0);
    b3frEntranceEast, b3frEntranceWest:
      Result := (AKind = b3skDoor) and (ARotation = v3r90);
  end;
end;

function ExpectedStructure(const ARole: TBuilding3DFootprintRole): String;
begin
  case ARole of
    b3frVoid: Result := WFC_BUILDING3D_STRUCTURE_VOID;
    b3frGroundShell: Result := WFC_BUILDING3D_STRUCTURE_FOUNDATION;
    b3frShell: Result := WFC_BUILDING3D_STRUCTURE_WALL + '|' +
      WFC_BUILDING3D_STRUCTURE_WINDOW;
    b3frInterior: Result := WFC_BUILDING3D_STRUCTURE_INTERIOR;
    b3frFeature: Result := WFC_BUILDING3D_STRUCTURE_FEATURE;
    b3frLintel: Result := WFC_BUILDING3D_STRUCTURE_LINTEL;
    b3frRoof: Result := WFC_BUILDING3D_STRUCTURE_ROOF;
    b3frEntranceNorth, b3frEntranceSouth:
      Result := WFC_BUILDING3D_STRUCTURE_DOOR + '@0';
    b3frEntranceEast, b3frEntranceWest:
      Result := WFC_BUILDING3D_STRUCTURE_DOOR + '@90';
  else
    Result := 'known-structure';
  end;
end;

function ExpectedEnvelope(
  const AStructure: TBuilding3DStructureKind): TBuilding3DEnvelopeKind;
begin
  case AStructure of
    b3skVoidAir, b3skInteriorAir, b3skFeatureAir:
      Result := b3ekNone;
    b3skFoundation, b3skWall, b3skLintel:
      Result := b3ekFacade;
    b3skWindow:
      Result := b3ekWindowTrim;
    b3skDoor:
      Result := b3ekDoorTrim;
    b3skRoofSpan:
      Result := b3ekRoofFinish;
  else
    Result := b3ekNone;
  end;
end;

function PropMatches(const ARole: TBuilding3DFootprintRole;
  const AStructure: TBuilding3DStructureKind;
  const AEnvelope: TBuilding3DEnvelopeKind;
  const AProp: TBuilding3DPropKind): Boolean;
begin
  if ARole = b3frFeature then
    Result := (AStructure = b3skFeatureAir) and
      (AEnvelope = b3ekNone) and (AProp in [b3pkLamp, b3pkPlant])
  else
    Result := AProp = b3pkNone;
end;

function ValidateBuilding3D(const ABuilding: TBuilding3D;
  const AOptions: TBuilding3DValidationOptions;
  out AReport: TBuilding3DValidationReport): Boolean;
var
  Envelope, ExpectedEnvelopeKind: TBuilding3DEnvelopeKind;
  Prop: TBuilding3DPropKind;
  Role: TBuilding3DFootprintRole;
  Rotation: TVoxel3DRotation;
  Scene: TVoxel3DScene;
  Structure: TBuilding3DStructureKind;
  X, Y, Z: Integer;
begin
  if not Assigned(ABuilding) then
    raise EArgumentNilException.Create('building validation model cannot be nil');
  InitializeReport(AReport);
  if not ABuilding.HasSolution then
  begin
    SetIssue(AReport, b3vikUnsolved, b3sFootprint,
      -1, -1, -1, 'solved', 'unsolved');
    Exit(False);
  end;
  if not ABuilding.DefinitionMatchesPipeline then
  begin
    SetIssue(AReport, b3vikPipelineDefinition, b3sFootprint,
      -1, -1, -1, 'canonical-four-pass-pipeline', 'modified-pipeline');
    Exit(False);
  end;

  Scene := nil;
  try
    Scene := ABuilding.CaptureStructureScene;
    if not ValidateVoxel3DScene(ABuilding.StructureKit, Scene,
        AOptions.Structure, AReport.Structure) then
    begin
      SetIssue(AReport, b3vikStructureVoxel, b3sStructure,
        AReport.Structure.Issue.X, AReport.Structure.Issue.Y,
        AReport.Structure.Issue.Z, 'valid-voxel-scene',
        Voxel3DValidationIssueName(AReport.Structure.Issue.Kind));
      AReport.Issue.VoxelIssue := AReport.Structure.Issue;
      Exit(False);
    end;
  finally
    Scene.Free;
  end;

  for Z := 0 to Integer(ABuilding.Depth) - 1 do
    for Y := 0 to Integer(ABuilding.Height) - 1 do
      for X := 0 to Integer(ABuilding.Width) - 1 do
      begin
        try
          Role := ABuilding.FootprintRoleAt(X, Y, Z);
        except
          on E: EBuilding3DScene do
          begin
            SetIssue(AReport, b3vikStageValue, b3sFootprint,
              X, Y, Z, 'known-public-value', E.Message);
            Exit(False);
          end;
        end;
        try
          Structure := ABuilding.StructureKindAt(X, Y, Z);
          Rotation := ABuilding.StageRotationAt(b3sStructure, X, Y, Z);
        except
          on E: EBuilding3DScene do
          begin
            SetIssue(AReport, b3vikStageValue, b3sStructure,
              X, Y, Z, 'known-public-value', E.Message);
            Exit(False);
          end;
        end;
        try
          Envelope := ABuilding.EnvelopeKindAt(X, Y, Z);
        except
          on E: EBuilding3DScene do
          begin
            SetIssue(AReport, b3vikStageValue, b3sEnvelopeRoof,
              X, Y, Z, 'known-public-value', E.Message);
            Exit(False);
          end;
        end;
        try
          Prop := ABuilding.PropKindAt(X, Y, Z);
        except
          on E: EBuilding3DScene do
          begin
            SetIssue(AReport, b3vikStageValue, b3sProps,
              X, Y, Z, 'known-public-value', E.Message);
            Exit(False);
          end;
        end;

        if not StructureMatchesFootprint(Role, Structure, Rotation) then
        begin
          SetIssue(AReport, b3vikFootprintStructure, b3sStructure,
            X, Y, Z, ExpectedStructure(Role),
            Building3DStructureKindToken(Structure) + '@' +
            IntToStr(Voxel3DRotationDegrees(Rotation)));
          Exit(False);
        end;

        ExpectedEnvelopeKind := ExpectedEnvelope(Structure);
        if Envelope <> ExpectedEnvelopeKind then
        begin
          SetIssue(AReport, b3vikStructureEnvelope, b3sEnvelopeRoof,
            X, Y, Z, Building3DEnvelopeKindToken(ExpectedEnvelopeKind),
            Building3DEnvelopeKindToken(Envelope));
          Exit(False);
        end;

        if not PropMatches(Role, Structure, Envelope, Prop) then
        begin
          if Role = b3frFeature then
            SetIssue(AReport, b3vikPropPlacement, b3sProps,
              X, Y, Z, WFC_BUILDING3D_PROP_LAMP + '|' +
              WFC_BUILDING3D_PROP_PLANT,
              Building3DPropKindToken(Prop))
          else
            SetIssue(AReport, b3vikPropPlacement, b3sProps,
              X, Y, Z, WFC_BUILDING3D_PROP_NONE,
              Building3DPropKindToken(Prop));
          Exit(False);
        end;

        if Role = b3frFeature then
          IncrementCounter(AReport.FeatureCount);
        if Prop <> b3pkNone then
          IncrementCounter(AReport.PropCount);
        IncrementCounter(AReport.CheckedCells);
      end;

  if AOptions.RequireFeature and (AReport.FeatureCount = 0) then
  begin
    SetIssue(AReport, b3vikMissingFeature, b3sFootprint,
      -1, -1, -1, 'at-least-one-feature', 'none');
    Exit(False);
  end;

  AReport.Valid := True;
  AReport.Issue.Kind := b3vikNone;
  Result := True;
end;

function ValidateBuilding3D(const ABuilding: TBuilding3D;
  out AReport: TBuilding3DValidationReport): Boolean;
begin
  Result := ValidateBuilding3D(ABuilding,
    DefaultBuilding3DValidationOptions, AReport);
end;

function Building3DValidationIssueName(
  const AKind: TBuilding3DValidationIssueKind): String;
begin
  case AKind of
    b3vikNone: Result := 'none';
    b3vikUnsolved: Result := 'unsolved';
    b3vikPipelineDefinition: Result := 'pipeline-definition';
    b3vikStageValue: Result := 'stage-value';
    b3vikFootprintStructure: Result := 'footprint-structure';
    b3vikStructureEnvelope: Result := 'structure-envelope';
    b3vikPropPlacement: Result := 'prop-placement';
    b3vikMissingFeature: Result := 'missing-feature';
    b3vikStructureVoxel: Result := 'structure-voxel';
  else
    Result := 'unknown';
  end;
end;

function DescribeBuilding3DValidationIssue(
  const AIssue: TBuilding3DValidationIssue): String;
begin
  Result := Building3DValidationIssueName(AIssue.Kind);
  if AIssue.X >= 0 then
    Result := Result + ' at (' + IntToStr(AIssue.X) + ',' +
      IntToStr(AIssue.Y) + ',' + IntToStr(AIssue.Z) + ')';
  if (AIssue.Expected <> '') or (AIssue.Actual <> '') then
    Result := Result + ': expected ' + AIssue.Expected +
      ', found ' + AIssue.Actual;
end;

end.
