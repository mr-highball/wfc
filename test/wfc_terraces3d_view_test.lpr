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
program wfc_terraces3d_view_test;
{$mode delphi}{$H+}
uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_voxel3d, wfc_voxel3d_isometric, wfc_voxel3d_svg,
  wfc_terraces3d, wfc_terraces3d_view;
var
  Owner: TTerraces3D; Scene: TTerraces3DScene;
  Projection, Replay: TVoxel3DProjectedScene;
  Report: TGraphNegotiationReport;
  Options: TVoxel3DIsometricOptions;
  Quad: TVoxel3DProjectedQuad;
  Svg, Before: String;
  I,J,Yaw,PlantFaces,Checks,Failures: Integer;
procedure Check(const OK: Boolean; const Message: String);
begin
  Inc(Checks);
  if not OK then begin Inc(Failures); WriteLn('FAIL: ',Message); end;
end;
begin
  Owner := nil; Scene := nil; Projection := nil; Replay := nil;
  try
    Owner := TTerraces3D.Create(6,5,5,0);
    Check(Owner.TryGenerate(DefaultTerraces3DOptions,Scene,Report),'view baseline');
    if Assigned(Scene) then
    begin
      Before := Scene.Signature;
      Check(Before = '1:6D695B99:2D23CF62','native/browser composition golden');
      for Yaw := 0 to 3 do
      begin
        Options := DefaultVoxel3DIsometricOptions; Options.Yaw := TVoxel3DViewYaw(Yaw);
        Projection := ProjectTerraces3D(Owner,Scene,Options);
        Replay := ProjectTerraces3D(Owner,Scene,Options);
        Check(Projection.Signature = Replay.Signature,'repeated projection exact');
        if Yaw = 0 then Check(Voxel3DSignatureHex(Projection.Signature) = 'C3D25917','default view golden');
        Check(Projection.QuadCount > 0,'nonempty scene geometry');
        PlantFaces := 0;
        for I := 0 to Projection.QuadCount-1 do
        begin
          Quad := Projection.QuadAt(I);
          Check((Quad.LayerId = 'structure') or (Quad.LayerId = 'foliage'),'public layer');
          Check(Pos('@',Quad.PrototypeId) = 0,'no private key in mesh metadata');
          Check(Quad.Semantic = String(Scene.TerrainAt(Quad.CellX,Quad.CellY,Quad.CellZ)),'exact terrain lineage');
          if Quad.LayerId = 'foliage' then
          begin
            Inc(PlantFaces);
            for J := 0 to 3 do
              Check((Quad.WorldVertices[J].X >= Quad.CellX*1024+256) and
                (Quad.WorldVertices[J].X <= Quad.CellX*1024+768) and
                (Quad.WorldVertices[J].Y >= Quad.CellY*1024+256) and
                (Quad.WorldVertices[J].Y <= Quad.CellY*1024+768) and
                (Quad.WorldVertices[J].Z >= Quad.CellZ*1024) and
                (Quad.WorldVertices[J].Z <= Quad.CellZ*1024+512),'plant glyph stays inside its assigned cell');
          end;
        end;
        Check(PlantFaces > 0,'default composition contains plants');
        Svg := EncodeVoxel3DProjectedSceneSvg(Projection,DefaultVoxel3DSvgOptions);
        Check(Pos('<svg ',Svg) > 0,'owned SVG encoding');
        Check(Pos('data-layer="foliage"',Svg) > 0,'SVG retains foliage metadata');
        Check(Pos('data-semantic="air"',Svg) > 0,'SVG retains public terrain token');
        Check(Pos('data-wfc-view-signature="'+Voxel3DSignatureHex(Projection.Signature)+'"',Svg) > 0,'SVG carries view witness');
        Check(Scene.Signature = Before,'presentation never mutates composition');
        FreeAndNil(Replay); FreeAndNil(Projection);
      end;
    end;
  except on E: Exception do begin Inc(Failures); WriteLn('UNEXPECTED: ',E.ClassName,': ',E.Message); end;
  end;
  Replay.Free; Projection.Free; Scene.Free; Owner.Free;
  WriteLn('checks=',Checks,' failures=',Failures);
  if Failures <> 0 then Halt(1);
end.
