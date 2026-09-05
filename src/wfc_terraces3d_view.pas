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
unit wfc_terraces3d_view;
{$mode delphi}{$H+}
interface
uses SysUtils, wfc, wfc_voxel3d, wfc_voxel3d_isometric, wfc_terraces3d;
{ Caller owns the projection. Plants are inset half-height glyphs, not mesh
  imports or botanical geometry. Cell metadata retains exact pass lineage. }
function ProjectTerraces3D(const AOwner: TTerraces3D;
  const AScene: TTerraces3DScene;
  const AOptions: TVoxel3DIsometricOptions): TVoxel3DProjectedScene;
implementation
uses wfc_voxel3d_mesh;

function Color(const P: String; const D: TGraphDirection): TVoxel3DColor;
var R,G,B,Shade: Integer;
begin
  R := 96; G := 91; B := 87;
  if P = 'bedrock' then begin R := 75; G := 72; B := 78; end
  else if P = 'stone' then begin R := 164; G := 156; B := 134; end
  else if P = 'basalt' then begin R := 100; G := 117; B := 120; end
  else if P = 'grass' then
    if D = gdUp then begin R := 153; G := 184; B := 104; end
    else begin R := 140; G := 124; B := 83; end
  else if P = 'flowerbed' then begin R := 157; G := 109; B := 74; end
  else if P = 'fern' then begin R := 78; G := 147; B := 110; end
  else if P = 'flowers' then begin R := 226; G := 165; B := 116; end;
  Shade := 100;
  if D in [gdNorth,gdSouth] then Shade := 84
  else if D in [gdEast,gdWest] then Shade := 70;
  Result := MakeVoxel3DColor(R*Shade div 100,G*Shade div 100,B*Shade div 100,255);
end;

function ProjectTerraces3D(const AOwner: TTerraces3D;
  const AScene: TTerraces3DScene;
  const AOptions: TVoxel3DIsometricOptions): TVoxel3DProjectedScene;
var
  Quads: TVoxel3DViewQuads;
  Mesh: TVoxel3DMesh;
  Q: TVoxel3DMeshQuad;
  I,J,L,N,Start: Integer;
  Message: String;
begin
  Result := nil;
  if not Assigned(AOwner) then raise EArgumentNilException.Create('terrace view owner is nil');
  if not AOwner.Validate(AScene,Message) then raise ETerraces3D.Create(Message);
  if (AScene.Width > High(Integer) div WFC_VOXEL3D_SUBCELL_SCALE) or
    (AScene.Height > High(Integer) div WFC_VOXEL3D_SUBCELL_SCALE) or
    (AScene.Depth > High(Integer) div WFC_VOXEL3D_SUBCELL_SCALE) then
    raise ERangeError.Create('terrace view exceeds fixed-subcell coordinates');
  for L := 0 to 1 do
  begin
    if L = 0 then Mesh := BuildVoxel3DMesh(AOwner.StructureKit,AScene.Structure)
    else Mesh := BuildVoxel3DMesh(AOwner.FoliageKit,AScene.Foliage);
    try
      Start := Length(Quads);
      if Mesh.QuadCount > High(Integer)-Start then
        raise ERangeError.Create('terrace view quad count exceeds Integer');
      SetLength(Quads,Start+Mesh.QuadCount);
      for I := 0 to Mesh.QuadCount-1 do
      begin
        Q := Mesh.QuadAt(I); N := Start+I;
        Quads[N].CellX := Q.CellX; Quads[N].CellY := Q.CellY; Quads[N].CellZ := Q.CellZ;
        Quads[N].Direction := Q.Direction; Quads[N].Rotation := Q.Rotation;
        Quads[N].LayerOrder := L;
        if L = 0 then Quads[N].LayerId := 'structure' else Quads[N].LayerId := 'foliage';
        Quads[N].PrototypeId := Q.PrototypeId; Quads[N].Material := Q.Material;
        Quads[N].Semantic := String(AScene.TerrainAt(Q.CellX,Q.CellY,Q.CellZ));
        Quads[N].FillColor := Color(Q.PrototypeId,Q.Direction);
        Quads[N].EdgeColor := MakeVoxel3DColor(47,55,50,255);
        for J := 0 to 3 do
          if L = 0 then
            Quads[N].Vertices[J] := MakeVoxel3DViewPoint3(
              Q.Vertices[J].X*1024,Q.Vertices[J].Y*1024,Q.Vertices[J].Z*1024)
          else
            Quads[N].Vertices[J] := MakeVoxel3DViewPoint3(
              Q.CellX*1024+(Q.Vertices[J].X-Q.CellX)*512+256,
              Q.CellY*1024+(Q.Vertices[J].Y-Q.CellY)*512+256,
              Q.CellZ*1024+(Q.Vertices[J].Z-Q.CellZ)*512);
      end;
    finally Mesh.Free; end;
  end;
  Result := ProjectVoxel3DIsometric(Quads,AOptions);
end;
end.
