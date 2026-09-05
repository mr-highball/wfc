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
program wfc_voxel3d_svg_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_voxel3d,
  wfc_voxel3d_isometric,
  wfc_voxel3d_svg;

var
  Checks: Integer = 0;
  Failures: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then
  begin
    Inc(Failures);
    WriteLn('FAIL: ', AMessage);
  end;
end;

function OneTopFace: TVoxel3DViewQuads;
const
  S = WFC_VOXEL3D_SUBCELL_SCALE;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0].Vertices[0] := MakeVoxel3DViewPoint3(0, 0, S);
  Result[0].Vertices[1] := MakeVoxel3DViewPoint3(S, 0, S);
  Result[0].Vertices[2] := MakeVoxel3DViewPoint3(S, S, S);
  Result[0].Vertices[3] := MakeVoxel3DViewPoint3(0, S, S);
  Result[0].CellX := 0;
  Result[0].CellY := 0;
  Result[0].CellZ := 0;
  Result[0].Direction := gdUp;
  Result[0].Rotation := v3r0;
  Result[0].LayerOrder := 2;
  Result[0].LayerId := 'structure';
  Result[0].PrototypeId := 'roof-span';
  Result[0].Material := 'roof';
  Result[0].Semantic := 'roof-finish';
  Result[0].FillColor := MakeVoxel3DColor($12, $AB, $34, 128);
  Result[0].EdgeColor := MakeVoxel3DColor($01, $02, $03, 255);
end;

procedure Run;
var
  Options: TVoxel3DSvgOptions;
  Projection: TVoxel3DProjectedScene;
  Quads: TVoxel3DViewQuads;
  Raised: Boolean;
  Svg, Twin: String;
begin
  Check(WFC_VOXEL3D_SVG_VERSION = 1, 'SVG format version changed');
  Check(Voxel3DColorCss(MakeVoxel3DColor($0A, $B0, $FF, 1)) = '#0AB0FF',
    'color encoding is not exact uppercase hexadecimal');

  Quads := OneTopFace;
  Projection := ProjectVoxel3DIsometric(Quads,
    DefaultVoxel3DIsometricOptions);
  try
    Options := DefaultVoxel3DSvgOptions;
    Options.Title := 'Roof & <trim> "proof"';
    Svg := EncodeVoxel3DProjectedSceneSvg(Projection, Options);
    Twin := EncodeVoxel3DProjectedSceneSvg(Projection, Options);
    Check(Svg = Twin, 'same projected scene did not encode identically');
    Check(Pos(#13, Svg) = 0, 'SVG output is not canonical LF-only text');
    Check(Pos('<?xml version="1.0" encoding="UTF-8"?>'#10, Svg) = 1,
      'SVG declaration is missing or unstable');
    Check(Pos('Roof &amp; &lt;trim&gt; &quot;proof&quot;', Svg) > 0,
      'SVG title escaping is incomplete');
    Check(Pos('fill="#12AB34" fill-opacity="0.502"', Svg) > 0,
      'RGBA fill was not encoded deterministically');
    Check(Pos('stroke="#010203" stroke-opacity="1"', Svg) > 0,
      'RGBA edge was not encoded deterministically');
    Check(Pos('data-cell="0,0,0" data-layer="structure"', Svg) > 0,
      'public cell/layer metadata is absent');
    Check(Pos('data-prototype="roof-span" data-material="roof"', Svg) > 0,
      'public prototype/material metadata is absent');
    Check(Pos('data-semantic="roof-finish"', Svg) > 0,
      'public semantic metadata is absent');
    Check(Pos(Voxel3DSignatureHex(Projection.Signature), Svg) > 0,
      'projected command signature is absent');

    Options.IncludeMetadata := False;
    Svg := EncodeVoxel3DProjectedSceneSvg(Projection, Options);
    Check(Pos('data-cell=', Svg) = 0,
      'metadata opt-out still emitted cell metadata');
    Check(Pos('<polygon points=', Svg) > 0,
      'metadata opt-out removed the graphical polygon');

    Options.StrokeWidth := -1;
    Raised := False;
    try
      Svg := EncodeVoxel3DProjectedSceneSvg(Projection, Options);
    except
      on E: ERangeError do Raised := True;
    end;
    Check(Raised, 'negative SVG stroke width was accepted');
  finally
    Projection.Free;
  end;

  Options := DefaultVoxel3DSvgOptions;
  Raised := False;
  try
    Svg := EncodeVoxel3DProjectedSceneSvg(nil, Options);
  except
    on E: EVoxel3DSvg do Raised := True;
  end;
  Check(Raised, 'nil projected scene was accepted');

  Options.Title := 'bad' + #1;
  Projection := ProjectVoxel3DIsometric(nil,
    DefaultVoxel3DIsometricOptions);
  try
    Raised := False;
    try
      Svg := EncodeVoxel3DProjectedSceneSvg(Projection, Options);
    except
      on E: EVoxel3DSvg do Raised := True;
    end;
    Check(Raised, 'invalid XML control character was accepted');
  finally
    Projection.Free;
  end;
end;

begin
  try
    Run;
  except
    on E: Exception do
    begin
      Inc(Failures);
      WriteLn('UNHANDLED: ', E.ClassName, ': ', E.Message);
    end;
  end;
  WriteLn('Checks: ', Checks);
  WriteLn('Failures: ', Failures);
  if Failures <> 0 then
  {$IFDEF PAS2JS}
    raise Exception.Create(IntToStr(Failures) + ' voxel SVG test failures');
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
