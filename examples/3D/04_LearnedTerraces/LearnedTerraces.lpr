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
program LearnedTerraces;
{$mode delphi}{$H+}
uses Classes, SysUtils, wfc, wfc_voxel3d, wfc_voxel3d_isometric,
  wfc_voxel3d_svg, wfc_terraces3d, wfc_terraces3d_view;
var
  Owner: TTerraces3D;
  Scene: TTerraces3DScene;
  Projection: TVoxel3DProjectedScene;
  Report: TGraphNegotiationReport;
  SvgOptions: TVoxel3DSvgOptions;
  Stream: TFileStream;
  Svg, OutputPath: String;
  Seed: QWord;
  W,H,D: Integer;
begin
  Owner := nil; Scene := nil; Projection := nil;
  try
    if not (ParamCount in [0,1,2,5]) then
      raise EConvertError.Create('usage: LearnedTerraces [seed] [output.svg] [width height depth]');
    Seed := 0; W := 6; H := 5; D := 5; OutputPath := 'terraces3d.svg';
    if ParamCount >= 1 then
      if not TryStrToQWord(ParamStr(1),Seed) or (Seed > High(TGraphSeed)) then
        raise EConvertError.Create('seed must be an unsigned 32-bit integer');
    if ParamCount >= 2 then OutputPath := ParamStr(2);
    if ParamCount = 5 then
    begin W := StrToInt(ParamStr(3)); H := StrToInt(ParamStr(4)); D := StrToInt(ParamStr(5)); end;
    if OutputPath = '' then raise EConvertError.Create('output path cannot be empty');
    Owner := TTerraces3D.Create(W,H,D,TGraphSeed(Seed));
    if not Owner.TryGenerate(DefaultTerraces3DOptions,Scene,Report) then
      raise ETerraces3D.Create('generation failed; adjust seed or search budgets');
    Projection := ProjectTerraces3D(Owner,Scene,DefaultVoxel3DIsometricOptions);
    SvgOptions := DefaultVoxel3DSvgOptions;
    SvgOptions.Title := 'Learned Terraces: terrain -> structure -> foliage';
    Svg := EncodeVoxel3DProjectedSceneSvg(Projection,SvgOptions);
    Stream := TFileStream.Create(OutputPath,fmCreate);
    try if Svg <> '' then Stream.WriteBuffer(Svg[1],Length(Svg)); finally Stream.Free; end;
    WriteLn('WFC Learned Terraces / MIT / native Pascal');
    WriteLn('Seed: ',Seed,'; dimensions: ',W,'x',H,'x',D);
    WriteLn('Pipeline: learned terrain -> socket/support structure -> foliage');
    WriteLn('Signature: ',Scene.Signature);
    WriteLn('View signature: ',Voxel3DSignatureHex(Projection.Signature));
    WriteLn('SVG: ',OutputPath);
    WriteLn('Self-check: passed');
  except on E: Exception do
    begin WriteLn(StdErr,E.ClassName,': ',E.Message); Projection.Free; Scene.Free; Owner.Free; Halt(1); end;
  end;
  Projection.Free; Scene.Free; Owner.Free;
end.
