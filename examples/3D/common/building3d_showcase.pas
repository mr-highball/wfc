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
unit building3d_showcase;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_voxel3d,
  wfc_building3d,
  wfc_building3d_validate;

const
  BUILDING3D_SHOWCASE_VERSION = 1;

  BUILDING3D_SHOWCASE_WIDTH = 7;
  BUILDING3D_SHOWCASE_HEIGHT = 5;
  BUILDING3D_SHOWCASE_DEPTH = 3;
  BUILDING3D_SHOWCASE_CELL_COUNT = 105;
  BUILDING3D_SHOWCASE_FEATURE_COUNT = 1;
  BUILDING3D_SHOWCASE_PROP_COUNT = 1;
  BUILDING3D_SHOWCASE_REACHABLE_COUNT = 10;
  BUILDING3D_SHOWCASE_STRUCTURE_QUAD_COUNT = 134;

  BUILDING3D_SHOWCASE_DEFAULT_SEED = TGraphSeed(20210914);
  BUILDING3D_SHOWCASE_SEED_ZERO = TGraphSeed(0);

  BUILDING3D_SHOWCASE_SEED_ZERO_PIPELINE_SIGNATURE = '1:F1EF0EB6';
  BUILDING3D_SHOWCASE_SEED_ZERO_STRUCTURE_SIGNATURE = '15787695';
  BUILDING3D_SHOWCASE_DEFAULT_PIPELINE_SIGNATURE = '1:911E9106';
  BUILDING3D_SHOWCASE_DEFAULT_STRUCTURE_SIGNATURE = 'AA2076F8';

  BUILDING3D_SHOWCASE_ENTRANCE_X = 3;
  BUILDING3D_SHOWCASE_ENTRANCE_Y = 0;
  BUILDING3D_SHOWCASE_ENTRANCE_Z = 0;
  BUILDING3D_SHOWCASE_FEATURE_X = 3;
  BUILDING3D_SHOWCASE_FEATURE_Y = 2;
  BUILDING3D_SHOWCASE_FEATURE_Z = 0;

type
  EBuilding3DShowcase = class(Exception);

  TBuilding3DShowcaseExpected = record
    PipelineSignature: String;
    StructureSceneSignature: String;
  end;

function NewBuilding3DShowcaseBlueprint: TBuilding3DBlueprint;

function NewSolvedBuilding3DShowcase(const ASeed: TGraphSeed;
  out AReport: TGraphSolveReport): TBuilding3D;

function TryBuilding3DShowcaseExpected(const ASeed: TGraphSeed;
  out AExpected: TBuilding3DShowcaseExpected): Boolean;

procedure VerifyBuilding3DShowcase(const ABuilding: TBuilding3D;
  const AReport: TGraphSolveReport;
  out AValidation: TBuilding3DValidationReport);

implementation

uses
  wfc_voxel3d_mesh;

procedure RequireShowcase(const ACondition: Boolean;
  const AMessage: String);
begin
  if not ACondition then
    raise EBuilding3DShowcase.Create(AMessage);
end;

function NewBuilding3DShowcaseBlueprint: TBuilding3DBlueprint;
var
  X, Y: Integer;
begin
  Result := TBuilding3DBlueprint.Create(
    BUILDING3D_SHOWCASE_WIDTH,
    BUILDING3D_SHOWCASE_HEIGHT,
    BUILDING3D_SHOWCASE_DEPTH);
  try
    for Y := 0 to BUILDING3D_SHOWCASE_HEIGHT - 1 do
      for X := 1 to 5 do
      begin
        if (X = 1) or (X = 5) or (Y = 0) or (Y = 4) then
          Result.SetRole(X, Y, 0, b3frGroundShell)
        else
          Result.SetRole(X, Y, 0, b3frInterior);

        if (X = 1) or (X = 5) or (Y = 0) or (Y = 4) then
          Result.SetRole(X, Y, 1, b3frShell)
        else
          Result.SetRole(X, Y, 1, b3frInterior);

        Result.SetRole(X, Y, 2, b3frRoof);
      end;
    Result.SetRole(BUILDING3D_SHOWCASE_ENTRANCE_X,
      BUILDING3D_SHOWCASE_ENTRANCE_Y,
      BUILDING3D_SHOWCASE_ENTRANCE_Z, b3frEntranceSouth);
    Result.SetRole(BUILDING3D_SHOWCASE_ENTRANCE_X,
      BUILDING3D_SHOWCASE_ENTRANCE_Y, 1, b3frLintel);
    Result.SetRole(BUILDING3D_SHOWCASE_FEATURE_X,
      BUILDING3D_SHOWCASE_FEATURE_Y,
      BUILDING3D_SHOWCASE_FEATURE_Z, b3frFeature);
  except
    Result.Free;
    raise;
  end;
end;

function NewSolvedBuilding3DShowcase(const ASeed: TGraphSeed;
  out AReport: TGraphSolveReport): TBuilding3D;
var
  LBlueprint: TBuilding3DBlueprint;
  LConfig: TBuilding3DConfig;
  LOptions: TGraphSolveOptions;
begin
  Result := nil;
  LBlueprint := NewBuilding3DShowcaseBlueprint;
  try
    LConfig := DefaultBuilding3DConfig;
    LConfig.Seed := ASeed;
    LConfig.WrapNeighbors := False;
    Result := TBuilding3D.Create(LBlueprint, LConfig);
    try
      LOptions := DefaultGraphSolveOptions;
      LOptions.MaxBacktracks := 4096;
      if not Result.TryGenerate(LOptions, AReport) then
        raise EBuilding3DShowcase.CreateFmt(
          'pipeline failed in pass %d with contradiction %d',
          [AReport.FailedPassIndex, Ord(AReport.Contradiction.Kind)]);
    except
      Result.Free;
      Result := nil;
      raise;
    end;
  finally
    LBlueprint.Free;
  end;
end;

function TryBuilding3DShowcaseExpected(const ASeed: TGraphSeed;
  out AExpected: TBuilding3DShowcaseExpected): Boolean;
begin
  AExpected := Default(TBuilding3DShowcaseExpected);
  if ASeed = BUILDING3D_SHOWCASE_SEED_ZERO then
  begin
    AExpected.PipelineSignature :=
      BUILDING3D_SHOWCASE_SEED_ZERO_PIPELINE_SIGNATURE;
    AExpected.StructureSceneSignature :=
      BUILDING3D_SHOWCASE_SEED_ZERO_STRUCTURE_SIGNATURE;
    Exit(True);
  end;
  if ASeed = BUILDING3D_SHOWCASE_DEFAULT_SEED then
  begin
    AExpected.PipelineSignature :=
      BUILDING3D_SHOWCASE_DEFAULT_PIPELINE_SIGNATURE;
    AExpected.StructureSceneSignature :=
      BUILDING3D_SHOWCASE_DEFAULT_STRUCTURE_SIGNATURE;
    Exit(True);
  end;
  Result := False;
end;

procedure VerifyBuilding3DShowcase(const ABuilding: TBuilding3D;
  const AReport: TGraphSolveReport;
  out AValidation: TBuilding3DValidationReport);
var
  I: Integer;
  LExpected: TBuilding3DShowcaseExpected;
  LMesh: TVoxel3DMesh;
  LScene: TVoxel3DScene;
  LValidationOptions: TBuilding3DValidationOptions;
begin
  AValidation := Default(TBuilding3DValidationReport);
  if not Assigned(ABuilding) then
    raise EArgumentNilException.Create(
      'Building 3D showcase verification requires a building');
  RequireShowcase(ABuilding.HasSolution,
    'showcase building is not solved');
  RequireShowcase(ABuilding.DefinitionMatchesPipeline,
    'showcase pipeline definition changed');
  RequireShowcase((ABuilding.Width = BUILDING3D_SHOWCASE_WIDTH) and
    (ABuilding.Height = BUILDING3D_SHOWCASE_HEIGHT) and
    (ABuilding.Depth = BUILDING3D_SHOWCASE_DEPTH),
    'showcase dimensions changed');
  RequireShowcase(not ABuilding.WrapNeighbors,
    'showcase topology must remain bounded');

  RequireShowcase(AReport.Status = gssSolved,
    'solve report is not solved');
  RequireShowcase((AReport.Seed = ABuilding.Seed) and
    (AReport.RandomAlgorithmVersion = WFC_RANDOM_ALGORITHM_VERSION) and
    (AReport.SolverAlgorithmVersion = WFC_SOLVER_ALGORITHM_VERSION) and
    (AReport.GraphModelVersion = WFC_GRAPH_MODEL_VERSION) and
    (AReport.PipelineAlgorithmVersion = WFC_PIPELINE_ALGORITHM_VERSION) and
    (AReport.FailedPassIndex = -1) and
    (AReport.Contradiction.Kind = gckNone),
    'solve report identity or success evidence changed');
  RequireShowcase(Length(AReport.Passes) = 4,
    'solve report does not contain four pass reports');
  RequireShowcase(Length(AReport.ExecutionOrder) = 4,
    'solve report does not contain four passes');
  for I := 0 to 3 do
  begin
    RequireShowcase(AReport.ExecutionOrder[I] = I,
      'solve report execution order changed');
    RequireShowcase(AReport.Passes[I].Executed and
      (AReport.Passes[I].ExecutionOrdinal = I) and
      (AReport.Passes[I].Disposition = gpdSolved),
      'solve report pass evidence changed');
  end;

  LValidationOptions := DefaultBuilding3DValidationOptions;
  LValidationOptions.RequireFeature := True;
  RequireShowcase(ValidateBuilding3D(ABuilding, LValidationOptions,
    AValidation), 'independent validation failed: ' +
    DescribeBuilding3DValidationIssue(AValidation.Issue));
  RequireShowcase(AValidation.CheckedCells =
    BUILDING3D_SHOWCASE_CELL_COUNT,
    'showcase validated-cell count changed');
  RequireShowcase(AValidation.Structure.ReachableCount =
    BUILDING3D_SHOWCASE_REACHABLE_COUNT,
    'showcase reachable-cell count changed');
  RequireShowcase((AValidation.Structure.CheckedCells =
      BUILDING3D_SHOWCASE_CELL_COUNT) and
    (AValidation.Structure.EntranceCount = 1),
    'showcase structure validation evidence changed');
  RequireShowcase((AValidation.FeatureCount =
      BUILDING3D_SHOWCASE_FEATURE_COUNT) and
    (AValidation.PropCount = BUILDING3D_SHOWCASE_PROP_COUNT),
    'showcase feature or prop count changed');
  RequireShowcase(ABuilding.StageRotationAt(b3sStructure,
    BUILDING3D_SHOWCASE_ENTRANCE_X,
    BUILDING3D_SHOWCASE_ENTRANCE_Y,
    BUILDING3D_SHOWCASE_ENTRANCE_Z) = v3r0,
    'south entrance did not select the north/south door rotation');

  LScene := ABuilding.CaptureStructureScene;
  LMesh := nil;
  try
    LMesh := BuildVoxel3DMesh(ABuilding.StructureKit, LScene);
    RequireShowcase(LMesh.QuadCount =
      BUILDING3D_SHOWCASE_STRUCTURE_QUAD_COUNT,
      'showcase structure mesh quad count changed');
    if TryBuilding3DShowcaseExpected(ABuilding.Seed, LExpected) then
    begin
      RequireShowcase(ABuilding.PipelineSignature =
        LExpected.PipelineSignature,
        'showcase pipeline signature changed');
      RequireShowcase(Voxel3DSignatureHex(LScene.Signature) =
        LExpected.StructureSceneSignature,
        'showcase structure scene signature changed');
    end;
  finally
    LMesh.Free;
    LScene.Free;
  end;
end;

end.
