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
program Building3DSvg;

{$mode delphi}{$H+}

uses
  Classes,
  SysUtils,
  wfc,
  wfc_voxel3d,
  wfc_voxel3d_isometric,
  wfc_voxel3d_svg,
  wfc_building3d,
  wfc_building3d_validate,
  wfc_building3d_view,
  building3d_showcase;

function ParseSeed(const AText: String): TGraphSeed;
var
  Digit: TGraphSeed;
  I: Integer;
begin
  if AText = '' then
    raise EConvertError.Create('seed cannot be empty');
  Result := 0;
  for I := 1 to Length(AText) do
  begin
    if not (AText[I] in ['0'..'9']) then
      raise EConvertError.Create('seed must be an unsigned 32-bit integer');
    Digit := TGraphSeed(Ord(AText[I]) - Ord('0'));
    if Result > (High(TGraphSeed) - Digit) div 10 then
      raise EConvertError.Create('seed exceeds 4294967295');
    Result := Result * 10 + Digit;
  end;
end;

procedure SaveExact(const APath, AContent: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(APath, fmCreate);
  try
    if Length(AContent) > 0 then
      Stream.WriteBuffer(AContent[1], Length(AContent));
  finally
    Stream.Free;
  end;
end;

procedure Run;
var
  Building: TBuilding3D;
  Options: TVoxel3DIsometricOptions;
  OutputPath: String;
  Projection: TVoxel3DProjectedScene;
  Report: TGraphSolveReport;
  Seed: TGraphSeed;
  Svg: String;
  SvgOptions: TVoxel3DSvgOptions;
  Validation: TBuilding3DValidationReport;
  View: TBuilding3DView;
begin
  if ParamCount > 2 then
    raise EConvertError.Create(
      'usage: Building3DSvg [unsigned-32-bit-seed] [output.svg]');
  Seed := BUILDING3D_SHOWCASE_DEFAULT_SEED;
  if ParamCount >= 1 then Seed := ParseSeed(ParamStr(1));
  OutputPath := 'building3d.svg';
  if ParamCount >= 2 then OutputPath := ParamStr(2);
  if OutputPath = '' then raise EConvertError.Create('output path cannot be empty');

  Building := NewSolvedBuilding3DShowcase(Seed, Report);
  View := nil;
  Projection := nil;
  try
    VerifyBuilding3DShowcase(Building, Report, Validation);
    View := BuildBuilding3DView(Building, b3vmComplete);
    Options := DefaultVoxel3DIsometricOptions;
    Projection := ProjectVoxel3DIsometric(View.CopyQuads, Options);
    SvgOptions := DefaultVoxel3DSvgOptions;
    SvgOptions.Title := 'WFC four-pass Building 3D, seed ' + UIntToStr(Seed);
    Svg := EncodeVoxel3DProjectedSceneSvg(Projection, SvgOptions);
    SaveExact(OutputPath, Svg);

    WriteLn('WFC Building 3D SVG');
    WriteLn('Seed: ', Seed);
    WriteLn('Pipeline: ', Building.PipelineSignature);
    WriteLn('View: ', Voxel3DSignatureHex(Projection.Signature));
    WriteLn('Faces: ', Projection.QuadCount);
    WriteLn('Output: ', OutputPath);
    WriteLn('Self-check: passed');
  finally
    Projection.Free;
    View.Free;
    Building.Free;
  end;
end;

begin
  try
    Run;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
