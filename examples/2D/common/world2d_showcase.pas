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
unit world2d_showcase;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_world2d;

const
  WFC_WORLD2D_SHOWCASE_WIDTH = 32;
  WFC_WORLD2D_SHOWCASE_HEIGHT = 14;
  WFC_WORLD2D_SHOWCASE_DEFAULT_SEED = TGraphSeed($4D505731);
  WFC_WORLD2D_SHOWCASE_DEFAULT_SIGNATURE =
    '1:81F03F86:9069C5F9:41619106';
  WFC_WORLD2D_SHOWCASE_SEED_ZERO_SIGNATURE =
    '1:5B0DD75D:08022AF1:A40D0955';

procedure ApplyWorld2DShowcaseAnchors(const AWorld: TWorld2D);
function ExpectedWorld2DShowcaseSignature(
  const ASeed: TGraphSeed): String;
procedure VerifyWorld2DShowcaseSignature(const ASeed: TGraphSeed;
  const ASignature: String);

implementation

procedure ApplyWorld2DShowcaseAnchors(const AWorld: TWorld2D);
var
  LCenterX: TGraphCoordinate;
  LCenterY: TGraphCoordinate;
begin
  if not Assigned(AWorld) then
    raise EWorld2D.Create('cannot anchor an unassigned 2D world');
  if (AWorld.Width <> WFC_WORLD2D_SHOWCASE_WIDTH)
    or (AWorld.Height <> WFC_WORLD2D_SHOWCASE_HEIGHT) then
    raise EWorld2D.CreateFmt(
      'showcase anchors require a %d by %d world',
      [WFC_WORLD2D_SHOWCASE_WIDTH, WFC_WORLD2D_SHOWCASE_HEIGHT]);

  LCenterX := AWorld.Width div 2;
  LCenterY := AWorld.Height div 2;
  AWorld
    .Lock(w2lTerrain, 0, 0, WFC_WORLD2D_TERRAIN_WATER)
    .Lock(w2lTerrain, 1, 0, WFC_WORLD2D_TERRAIN_LAND)
    .Lock(w2lTerrain, LCenterX, LCenterY, WFC_WORLD2D_TERRAIN_LAND)
    .Lock(w2lTerrain, LCenterX - 1, LCenterY,
      WFC_WORLD2D_TERRAIN_LAND)
    .Lock(w2lTerrain, LCenterX + 1, LCenterY,
      WFC_WORLD2D_TERRAIN_LAND)
    .Lock(w2lTerrain, LCenterX, LCenterY - 1,
      WFC_WORLD2D_TERRAIN_LAND)
    .Lock(w2lTerrain, LCenterX, LCenterY + 1,
      WFC_WORLD2D_TERRAIN_LAND)
    .Lock(w2lTerrain, AWorld.Width - 1, AWorld.Height - 1,
      WFC_WORLD2D_TERRAIN_MOUNTAIN)
    .Lock(w2lBiome, 0, 0, WFC_WORLD2D_BIOME_OCEAN)
    .Lock(w2lBiome, 1, 0, WFC_WORLD2D_BIOME_SHORE)
    .Lock(w2lBiome, LCenterX, LCenterY, WFC_WORLD2D_BIOME_WOODLAND)
    .Lock(w2lBiome, AWorld.Width - 1, AWorld.Height - 1,
      WFC_WORLD2D_BIOME_ALPINE)
    .Lock(w2lFoliage, 1, 0, WFC_WORLD2D_FOLIAGE_REEDS)
    .Lock(w2lFoliage, LCenterX, LCenterY, WFC_WORLD2D_FOLIAGE_TREE)
    .Lock(w2lFoliage, AWorld.Width - 1, AWorld.Height - 1,
      WFC_WORLD2D_FOLIAGE_PINE);
end;

function ExpectedWorld2DShowcaseSignature(
  const ASeed: TGraphSeed): String;
begin
  if ASeed = WFC_WORLD2D_SHOWCASE_DEFAULT_SEED then
    Result := WFC_WORLD2D_SHOWCASE_DEFAULT_SIGNATURE
  else if ASeed = 0 then
    Result := WFC_WORLD2D_SHOWCASE_SEED_ZERO_SIGNATURE
  else
    Result := '';
end;

procedure VerifyWorld2DShowcaseSignature(const ASeed: TGraphSeed;
  const ASignature: String);
var
  LExpected: String;
begin
  LExpected := ExpectedWorld2DShowcaseSignature(ASeed);
  if (LExpected <> '') and (ASignature <> LExpected) then
    raise EWorld2D.CreateFmt(
      'showcase signature mismatch for seed %s: expected %s, got %s',
      [UIntToStr(ASeed), LExpected, ASignature]);
end;

end.
