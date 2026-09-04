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
unit wfc_voxel3d_svg;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc_voxel3d,
  wfc_voxel3d_isometric;

const
  WFC_VOXEL3D_SVG_VERSION = 1;

type
  EVoxel3DSvg = class(EVoxel3D);

  TVoxel3DSvgOptions = record
    Title: String;
    BackgroundColor: TVoxel3DColor;
    IncludeMetadata: Boolean;
    StrokeWidth: Integer;
  end;

function DefaultVoxel3DSvgOptions: TVoxel3DSvgOptions;
function Voxel3DColorCss(const AColor: TVoxel3DColor): String;
function EncodeVoxel3DProjectedSceneSvg(
  const AScene: TVoxel3DProjectedScene;
  const AOptions: TVoxel3DSvgOptions): String;

implementation

const
  HEX_DIGITS = '0123456789ABCDEF';
  LF = #10;

function HexByte(const AValue: Byte): String;
begin
  SetLength(Result, 2);
  Result[1] := HEX_DIGITS[(AValue shr 4) + 1];
  Result[2] := HEX_DIGITS[(AValue and $0F) + 1];
end;

function EscapeXml(const AValue: String): String;
var
  C: Char;
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AValue) do
  begin
    C := AValue[I];
    case C of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&apos;';
      #0..#8, #11, #12, #14..#31:
        raise EVoxel3DSvg.CreateFmt(
          'SVG text contains an unsupported control character at %d', [I]);
    else
      Result := Result + C;
    end;
  end;
end;

function OpacityText(const AAlpha: Byte): String;
var
  LThousandths: Integer;
begin
  if AAlpha = 0 then
    Exit('0');
  if AAlpha = 255 then
    Exit('1');
  LThousandths := (Integer(AAlpha) * 1000 + 127) div 255;
  Result := '0.' + Chr(Ord('0') + (LThousandths div 100)) +
    Chr(Ord('0') + ((LThousandths div 10) mod 10)) +
    Chr(Ord('0') + (LThousandths mod 10));
end;

function PointList(const AVertices: TVoxel3DScreenVertices): String;
var
  I: Integer;
begin
  Result := '';
  for I := Low(AVertices) to High(AVertices) do
  begin
    if I > Low(AVertices) then
      Result := Result + ' ';
    Result := Result + IntToStr(AVertices[I].X) + ',' +
      IntToStr(AVertices[I].Y);
  end;
end;

function DefaultVoxel3DSvgOptions: TVoxel3DSvgOptions;
begin
  Result.Title := 'WFC voxel projection';
  Result.BackgroundColor := MakeVoxel3DColor(7, 20, 22, 255);
  Result.IncludeMetadata := True;
  Result.StrokeWidth := 1;
end;

function Voxel3DColorCss(const AColor: TVoxel3DColor): String;
begin
  Result := '#' + HexByte(AColor.R) + HexByte(AColor.G) +
    HexByte(AColor.B);
end;

function EncodeVoxel3DProjectedSceneSvg(
  const AScene: TVoxel3DProjectedScene;
  const AOptions: TVoxel3DSvgOptions): String;
var
  I: Integer;
  LBounds: TVoxel3DScreenBounds;
  LMetadata: String;
  LQuad: TVoxel3DProjectedQuad;
begin
  if not Assigned(AScene) then
    raise EVoxel3DSvg.Create('SVG projected scene cannot be nil');
  if AOptions.StrokeWidth < 0 then
    raise ERangeError.Create('SVG stroke width cannot be negative');

  LBounds := AScene.Bounds;
  Result := '<?xml version="1.0" encoding="UTF-8"?>' + LF +
    '<svg xmlns="http://www.w3.org/2000/svg" width="' +
    IntToStr(LBounds.Width) + '" height="' + IntToStr(LBounds.Height) +
    '" viewBox="0 0 ' + IntToStr(LBounds.Width) + ' ' +
    IntToStr(LBounds.Height) + '" role="img" aria-labelledby="title">' + LF +
    '  <title id="title">' + EscapeXml(AOptions.Title) + '</title>' + LF +
    '  <rect width="100%" height="100%" fill="' +
    Voxel3DColorCss(AOptions.BackgroundColor) + '" fill-opacity="' +
    OpacityText(AOptions.BackgroundColor.A) + '"/>' + LF +
    '  <g data-wfc-view-signature="' +
    Voxel3DSignatureHex(AScene.Signature) + '">' + LF;

  for I := 0 to AScene.QuadCount - 1 do
  begin
    LQuad := AScene.QuadAt(I);
    LMetadata := '';
    if AOptions.IncludeMetadata then
      LMetadata := ' data-index="' + IntToStr(I) + '" data-cell="' +
        IntToStr(LQuad.CellX) + ',' + IntToStr(LQuad.CellY) + ',' +
        IntToStr(LQuad.CellZ) + '" data-layer="' +
        EscapeXml(LQuad.LayerId) + '" data-prototype="' +
        EscapeXml(LQuad.PrototypeId) + '" data-material="' +
        EscapeXml(LQuad.Material) + '" data-semantic="' +
        EscapeXml(LQuad.Semantic) + '"';
    Result := Result + '    <polygon points="' +
      PointList(LQuad.ScreenVertices) + '" fill="' +
      Voxel3DColorCss(LQuad.FillColor) + '" fill-opacity="' +
      OpacityText(LQuad.FillColor.A) + '" stroke="' +
      Voxel3DColorCss(LQuad.EdgeColor) + '" stroke-opacity="' +
      OpacityText(LQuad.EdgeColor.A) + '" stroke-width="' +
      IntToStr(AOptions.StrokeWidth) + '" stroke-linejoin="round"' +
      LMetadata + '/>' + LF;
  end;
  Result := Result + '  </g>' + LF + '</svg>' + LF;
end;

end.
