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
unit building3d_demo;

{$mode delphi}{$H+}

interface

procedure RunBuilding3DDemo;

implementation

uses
  SysUtils,
  wfc,
  wfc_voxel3d,
  wfc_voxel3d_mesh,
  wfc_building3d,
  wfc_building3d_validate,
  building3d_showcase;

const
  DEFAULT_SEED = BUILDING3D_SHOWCASE_DEFAULT_SEED;

type
  EBuilding3DDemo = class(Exception);

procedure Require(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise EBuilding3DDemo.Create(AMessage);
end;

function ParseSeed: TGraphSeed;
var
  I: Integer;
  LDigit, LParsed: TGraphSeed;
  LText: String;
begin
  if ParamCount = 0 then
    Exit(DEFAULT_SEED);
  if ParamCount <> 1 then
    raise EConvertError.Create(
      'usage: MultiPassBuilding [unsigned-32-bit-seed]');
  LText := ParamStr(1);
  if LText = '' then
    raise EConvertError.Create('seed cannot be empty');
  LParsed := 0;
  for I := 1 to Length(LText) do
  begin
    if not (LText[I] in ['0'..'9']) then
      raise EConvertError.Create(
        'seed must be an unsigned 32-bit integer');
    LDigit := TGraphSeed(Ord(LText[I]) - Ord('0'));
    if LParsed > (High(TGraphSeed) - LDigit) div 10 then
      raise EConvertError.Create('seed exceeds 4294967295');
    LParsed := (LParsed * 10) + LDigit;
  end;
  Result := LParsed;
end;

function FootprintGlyph(const AValue: TBuilding3DFootprintRole): Char;
begin
  case AValue of
    b3frVoid: Result := ' ';
    b3frGroundShell: Result := 'G';
    b3frShell: Result := 'S';
    b3frInterior: Result := '.';
    b3frFeature: Result := '*';
    b3frLintel: Result := '=';
    b3frRoof: Result := '^';
    b3frEntranceNorth, b3frEntranceEast,
    b3frEntranceSouth, b3frEntranceWest: Result := 'E';
  else
    Result := '?';
  end;
end;

function StructureGlyph(const AValue: TBuilding3DStructureKind): Char;
begin
  case AValue of
    b3skVoidAir: Result := ' ';
    b3skInteriorAir: Result := '.';
    b3skFeatureAir: Result := '*';
    b3skFoundation: Result := '#';
    b3skWall: Result := 'W';
    b3skWindow: Result := 'o';
    b3skDoor: Result := 'D';
    b3skLintel: Result := '=';
    b3skRoofSpan: Result := '^';
  else
    Result := '?';
  end;
end;

function EnvelopeGlyph(const AValue: TBuilding3DEnvelopeKind): Char;
begin
  case AValue of
    b3ekNone: Result := ' ';
    b3ekFacade: Result := 'F';
    b3ekWindowTrim: Result := 'w';
    b3ekDoorTrim: Result := 'd';
    b3ekRoofFinish: Result := 'R';
  else
    Result := '?';
  end;
end;

function PropGlyph(const AValue: TBuilding3DPropKind): Char;
begin
  case AValue of
    b3pkNone: Result := ' ';
    b3pkLamp: Result := 'L';
    b3pkPlant: Result := 'P';
  else
    Result := '?';
  end;
end;

function CellGlyph(const ABuilding: TBuilding3D;
  const AStage: TBuilding3DStage;
  const AX, AY, AZ: Integer): Char;
begin
  case AStage of
    b3sFootprint:
      Result := FootprintGlyph(ABuilding.FootprintRoleAt(AX, AY, AZ));
    b3sStructure:
      Result := StructureGlyph(ABuilding.StructureKindAt(AX, AY, AZ));
    b3sEnvelopeRoof:
      Result := EnvelopeGlyph(ABuilding.EnvelopeKindAt(AX, AY, AZ));
    b3sProps:
      Result := PropGlyph(ABuilding.PropKindAt(AX, AY, AZ));
  else
    Result := '?';
  end;
end;

procedure PrintStage(const ABuilding: TBuilding3D;
  const AStage: TBuilding3DStage);
var
  X, Y, Z: Integer;
begin
  WriteLn(Building3DStageName(AStage));
  for Z := Integer(ABuilding.Depth) - 1 downto 0 do
  begin
    WriteLn('  z=', Z);
    for Y := Integer(ABuilding.Height) - 1 downto 0 do
    begin
      Write('  |');
      for X := 0 to Integer(ABuilding.Width) - 1 do
        Write(CellGlyph(ABuilding, AStage, X, Y, Z));
      WriteLn('|');
    end;
  end;
end;

procedure RunBuilding3DDemo;
var
  B, Replay: TBuilding3D;
  Mesh: TVoxel3DMesh;
  Report, ReplayReport: TGraphSolveReport;
  Scene: TVoxel3DScene;
  Seed: TGraphSeed;
  Signature: String;
  Stage: TBuilding3DStage;
  Validation: TBuilding3DValidationReport;
begin
  Seed := ParseSeed;
  B := NewSolvedBuilding3DShowcase(Seed, Report);
  Replay := nil;
  Scene := nil;
  Mesh := nil;
  try
    VerifyBuilding3DShowcase(B, Report, Validation);

    Signature := B.PipelineSignature;
    Replay := NewSolvedBuilding3DShowcase(Seed, ReplayReport);
    VerifyBuilding3DShowcase(Replay, ReplayReport, Validation);
    Require(Replay.PipelineSignature = Signature,
      'same-seed replay changed the public pipeline signature');

    Scene := B.CaptureStructureScene;
    Mesh := BuildVoxel3DMesh(B.StructureKit, Scene);

    WriteLn('WFC Multi-Pass Building 3D');
    WriteLn('Model version: ', WFC_BUILDING3D_MODEL_VERSION);
    WriteLn('Seed: ', Seed);
    WriteLn('Dimensions: ', B.Width, 'x', B.Height, 'x', B.Depth);
    WriteLn('Pipeline: footprint -> structure -> envelope-roof -> props');
    WriteLn('Signature: ', Signature);
    WriteLn('Structure scene: ', Voxel3DSignatureHex(Scene.Signature));
    WriteLn('Structure mesh quads: ', Mesh.QuadCount);
    WriteLn('Validated cells: ', Validation.CheckedCells);
    WriteLn('Reachable cells: ', Validation.Structure.ReachableCount);
    WriteLn('Features/props: ', Validation.FeatureCount, '/',
      Validation.PropCount);
    WriteLn('Door yaw: ', Voxel3DRotationDegrees(
      B.StageRotationAt(b3sStructure, 3, 0, 0)));
    for Stage := Low(TBuilding3DStage) to High(TBuilding3DStage) do
      PrintStage(B, Stage);
    WriteLn('Self-check: passed');
  finally
    Mesh.Free;
    Scene.Free;
    Replay.Free;
    B.Free;
  end;
end;

end.
