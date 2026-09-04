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
program wfc_voxel3d_isometric_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_voxel3d,
  wfc_voxel3d_isometric;

const
  GOLDEN_SINGLE_FACE_SIGNATURE = 'CFFE81B3';
  GOLDEN_LAYERED_SIGNATURE = '8AEE1A32';

type
  TTestProcedure = procedure;

var
  CheckCount: Integer = 0;
  FailureCount: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(CheckCount);
  if not ACondition then
    raise Exception.Create(AMessage);
end;

procedure CheckInteger(const AExpected, AActual: Integer;
  const AMessage: String);
begin
  Check(AExpected = AActual, AMessage + ': expected ' +
    IntToStr(AExpected) + ', got ' + IntToStr(AActual));
end;

procedure CheckText(const AExpected, AActual, AMessage: String);
begin
  Check(AExpected = AActual, AMessage + ': expected "' +
    AExpected + '", got "' + AActual + '"');
end;

procedure CheckPoint(const APoint: TVoxel3DScreenPoint;
  const AX, AY: Integer; const AMessage: String);
begin
  Check((APoint.X = AX) and (APoint.Y = AY),
    AMessage + ': expected [' + IntToStr(AX) + ',' + IntToStr(AY) +
    '], got [' + IntToStr(APoint.X) + ',' + IntToStr(APoint.Y) + ']');
end;

function ColorEquals(const ALeft,
  ARight: TVoxel3DColor): Boolean;
begin
  Result := (ALeft.R = ARight.R) and (ALeft.G = ARight.G) and
    (ALeft.B = ARight.B) and (ALeft.A = ARight.A);
end;

function NewTopQuad(const AX0, AY0, AX1, AY1, AZ: Integer;
  const ALayerOrder: Integer; const ALayerId, ASemantic: String):
  TVoxel3DViewQuad;
begin
  Result.Vertices[0] := MakeVoxel3DViewPoint3(AX0, AY0, AZ);
  Result.Vertices[1] := MakeVoxel3DViewPoint3(AX1, AY0, AZ);
  Result.Vertices[2] := MakeVoxel3DViewPoint3(AX1, AY1, AZ);
  Result.Vertices[3] := MakeVoxel3DViewPoint3(AX0, AY1, AZ);
  Result.CellX := 0;
  Result.CellY := 0;
  Result.CellZ := 0;
  Result.Direction := gdUp;
  Result.Rotation := v3r0;
  Result.LayerOrder := ALayerOrder;
  Result.LayerId := ALayerId;
  Result.PrototypeId := 'solid';
  Result.Material := 'stone';
  Result.Semantic := ASemantic;
  Result.FillColor := MakeVoxel3DColor(17, 34, 51, 255);
  Result.EdgeColor := MakeVoxel3DColor(3, 4, 5, 255);
end;

function NewEastQuad: TVoxel3DViewQuad;
const
  S = WFC_VOXEL3D_SUBCELL_SCALE;
begin
  Result.Vertices[0] := MakeVoxel3DViewPoint3(S, 0, 0);
  Result.Vertices[1] := MakeVoxel3DViewPoint3(S, S, 0);
  Result.Vertices[2] := MakeVoxel3DViewPoint3(S, S, S);
  Result.Vertices[3] := MakeVoxel3DViewPoint3(S, 0, S);
  Result.CellX := 0;
  Result.CellY := 0;
  Result.CellZ := 0;
  Result.Direction := gdEast;
  Result.Rotation := v3r90;
  Result.LayerOrder := 2;
  Result.LayerId := 'structure';
  Result.PrototypeId := 'window';
  Result.Material := 'glass';
  Result.Semantic := 'window-trim';
  Result.FillColor := MakeVoxel3DColor(80, 160, 220, 240);
  Result.EdgeColor := MakeVoxel3DColor(8, 16, 22, 255);
end;

function OneQuad(const AQuad: TVoxel3DViewQuad): TVoxel3DViewQuads;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0] := AQuad;
end;

function InvalidYaw: TVoxel3DViewYaw;
{$PUSH}
{$R-}
var
  LOrdinal: Integer;
begin
  LOrdinal := 99;
  Result := TVoxel3DViewYaw(LOrdinal);
end;
{$POP}

function InvalidDirection: TGraphDirection;
{$PUSH}
{$R-}
var
  LOrdinal: Integer;
begin
  LOrdinal := 99;
  Result := TGraphDirection(LOrdinal);
end;
{$POP}

function InvalidRotation: TVoxel3DRotation;
{$PUSH}
{$R-}
var
  LOrdinal: Integer;
begin
  LOrdinal := 99;
  Result := TVoxel3DRotation(LOrdinal);
end;
{$POP}

procedure TestDefaultsAndEmpty;
var
  Bounds: TVoxel3DScreenBounds;
  C: TVoxel3DColor;
  HitIndex: Integer;
  O: TVoxel3DIsometricOptions;
  P: TVoxel3DViewPoint3;
  Raised: Boolean;
  Scene: TVoxel3DProjectedScene;
begin
  O := DefaultVoxel3DIsometricOptions;
  Check(O.Yaw = v3vy0, 'default yaw is zero');
  CheckInteger(32, O.HorizontalStep, 'default horizontal step');
  CheckInteger(16, O.PlanVerticalStep, 'default plan vertical step');
  CheckInteger(32, O.ElevationStep, 'default elevation step');
  CheckInteger(16, O.Margin, 'default margin');
  C := MakeVoxel3DColor(1, 2, 3, 4);
  Check((C.R = 1) and (C.G = 2) and (C.B = 3) and (C.A = 4),
    'color factory retains channels');
  P := MakeVoxel3DViewPoint3(-7, 8, 9);
  Check((P.X = -7) and (P.Y = 8) and (P.Z = 9),
    'view-point factory retains signed subcells');

  Scene := ProjectVoxel3DIsometric(nil, O);
  try
    CheckInteger(0, Scene.QuadCount, 'empty projection has no quads');
    Bounds := Scene.Bounds;
    Check((Bounds.Left = 0) and (Bounds.Top = 0) and
      (Bounds.Right = 0) and (Bounds.Bottom = 0) and
      (Bounds.Width = 0) and (Bounds.Height = 0),
      'empty projection has zero bounds');
    HitIndex := 42;
    Check(not Scene.HitTest(0, 0, HitIndex),
      'empty projection cannot be hit');
    CheckInteger(-1, HitIndex, 'empty hit clears the output index');
    Raised := False;
    try
      Scene.QuadAt(0);
    except
      on ERangeError do Raised := True;
    end;
    Check(Raised, 'empty projection rejects quad access');
  finally
    Scene.Free;
  end;
end;

procedure TestExactFourYawProjection;
const
  EXPECTED_POINTS: array[TVoxel3DViewYaw, 0..3, 0..1] of Integer = (
    ((48, 48), (16, 64), (16, 32), (48, 16)),
    ((16, 48), (48, 64), (48, 32), (16, 16)),
    ((16, 64), (48, 48), (48, 16), (16, 32)),
    ((48, 64), (16, 48), (16, 16), (48, 32))
  );
  EXPECTED_DEPTH: array[TVoxel3DViewYaw] of Integer =
    (2048, 0, -1024, 1024);
var
  I: Integer;
  O: TVoxel3DIsometricOptions;
  Q: TVoxel3DProjectedQuad;
  Scene: TVoxel3DProjectedScene;
  Yaw: TVoxel3DViewYaw;
begin
  O := DefaultVoxel3DIsometricOptions;
  for Yaw := Low(TVoxel3DViewYaw) to High(TVoxel3DViewYaw) do
  begin
    O.Yaw := Yaw;
    Scene := ProjectVoxel3DIsometric(OneQuad(NewEastQuad), O);
    try
      CheckInteger(1, Scene.QuadCount, 'one face remains one projected quad');
      Check((Scene.Bounds.Left = 16) and (Scene.Bounds.Top = 16) and
        (Scene.Bounds.Right = 48) and (Scene.Bounds.Bottom = 64) and
        (Scene.Bounds.Width = 65) and (Scene.Bounds.Height = 81),
        'each quarter turn preserves exact fitted bounds');
      Q := Scene.QuadAt(0);
      for I := 0 to 3 do
        CheckPoint(Q.ScreenVertices[I], EXPECTED_POINTS[Yaw, I, 0],
          EXPECTED_POINTS[Yaw, I, 1], 'quarter-turn projected vertex');
      CheckInteger(EXPECTED_DEPTH[Yaw], Q.DepthKey,
        'quarter-turn depth key');
      Check((Q.Direction = gdEast) and (Q.Rotation = v3r90),
        'projection preserves public direction and module rotation');
      CheckText('structure', Q.LayerId, 'projection preserves layer id');
      CheckText('window', Q.PrototypeId,
        'projection preserves prototype id');
      CheckText('glass', Q.Material, 'projection preserves material');
      CheckText('window-trim', Q.Semantic,
        'projection preserves semantic token');
    finally
      Scene.Free;
    end;
  end;
end;

procedure TestBoundsHitAndGoldenSignature;
const
  S = WFC_VOXEL3D_SUBCELL_SCALE;
var
  Bounds: TVoxel3DScreenBounds;
  HitIndex: Integer;
  O: TVoxel3DIsometricOptions;
  Q: TVoxel3DProjectedQuad;
  Quads: TVoxel3DViewQuads;
  Replay: TVoxel3DProjectedScene;
  Scene: TVoxel3DProjectedScene;
begin
  O := DefaultVoxel3DIsometricOptions;
  Quads := OneQuad(NewTopQuad(0, 0, S, S, S, 0,
    'structure', 'roof-finish'));
  Scene := ProjectVoxel3DIsometric(Quads, O);
  Replay := ProjectVoxel3DIsometric(Quads, O);
  try
    Bounds := Scene.Bounds;
    Check((Bounds.Left = 16) and (Bounds.Top = 16) and
      (Bounds.Right = 80) and (Bounds.Bottom = 48) and
      (Bounds.Width = 97) and (Bounds.Height = 65),
      'top face has exact auto-fitted bounds');
    Q := Scene.QuadAt(0);
    CheckPoint(Q.ScreenVertices[0], 48, 16, 'top north vertex');
    CheckPoint(Q.ScreenVertices[1], 80, 32, 'top east vertex');
    CheckPoint(Q.ScreenVertices[2], 48, 48, 'top south vertex');
    CheckPoint(Q.ScreenVertices[3], 16, 32, 'top west vertex');
    HitIndex := -1;
    Check(Scene.HitTest(48, 32, HitIndex), 'top center is hittable');
    CheckInteger(0, HitIndex, 'top center resolves the only quad');
    Check(Scene.HitTest(48, 16, HitIndex),
      'projected polygon edges are hittable');
    Check(not Scene.HitTest(0, 0, HitIndex),
      'outside content does not hit');
    CheckInteger(-1, HitIndex, 'miss clears output index');
    Check(Scene.Signature = Replay.Signature,
      'same view input replays exactly');
    CheckText(GOLDEN_SINGLE_FACE_SIGNATURE,
      Voxel3DSignatureHex(Scene.Signature),
      'single-face projection signature');
  finally
    Replay.Free;
    Scene.Free;
  end;
end;

procedure TestStablePainterOrderAndHit;
const
  S = WFC_VOXEL3D_SUBCELL_SCALE;
var
  HitIndex: Integer;
  O: TVoxel3DIsometricOptions;
  Q: TVoxel3DProjectedQuad;
  Quads: TVoxel3DViewQuads;
  Scene: TVoxel3DProjectedScene;
begin
  SetLength(Quads, 3);
  Quads[0] := NewTopQuad(0, 0, S, S, S, 10,
    'structure', 'source-zero');
  Quads[1] := NewTopQuad(0, 0, S, S, S, 0,
    'footprint', 'source-one');
  Quads[2] := NewTopQuad(0, 0, S, S, S, 10,
    'structure', 'source-two');
  O := DefaultVoxel3DIsometricOptions;
  Scene := ProjectVoxel3DIsometric(Quads, O);
  try
    CheckInteger(3, Scene.QuadCount, 'layered projection retains all faces');
    Q := Scene.QuadAt(0);
    Check((Q.LayerOrder = 0) and (Q.SourceIndex = 1),
      'lower layer sorts first for equal depth');
    Q := Scene.QuadAt(1);
    Check((Q.LayerOrder = 10) and (Q.SourceIndex = 0),
      'equal high layer retains first source before second');
    Q := Scene.QuadAt(2);
    Check((Q.LayerOrder = 10) and (Q.SourceIndex = 2),
      'stable tie order retains second source last');
    Check(Scene.HitTest(48, 32, HitIndex),
      'overlapping painter quads are hittable');
    CheckInteger(2, HitIndex, 'hit test scans painter order in reverse');
    Q := Scene.QuadAt(HitIndex);
    CheckText('source-two', Q.Semantic,
      'hit test returns topmost public semantic');
    CheckText(GOLDEN_LAYERED_SIGNATURE,
      Voxel3DSignatureHex(Scene.Signature),
      'layered projection signature');
  finally
    Scene.Free;
  end;
end;

procedure TestDepthPainterOrder;
const
  S = WFC_VOXEL3D_SUBCELL_SCALE;
var
  HitIndex: Integer;
  O: TVoxel3DIsometricOptions;
  Q: TVoxel3DProjectedQuad;
  Quads: TVoxel3DViewQuads;
  Scene: TVoxel3DProjectedScene;
begin
  SetLength(Quads, 2);
  //Translation by +1 voxel on X, Y, and Z follows the isometric view ray:
  //screen points stay exact while depth increases by three voxels.
  Quads[0] := NewTopQuad(S, S, 2 * S, 2 * S, 2 * S, 0,
    'structure', 'near-face');
  Quads[0].CellX := 1;
  Quads[0].CellY := 1;
  Quads[0].CellZ := 1;
  Quads[1] := NewTopQuad(0, 0, S, S, S, 0,
    'structure', 'far-face');
  O := DefaultVoxel3DIsometricOptions;
  Scene := ProjectVoxel3DIsometric(Quads, O);
  try
    Q := Scene.QuadAt(0);
    Check((Q.SourceIndex = 1) and (Q.Semantic = 'far-face'),
      'smaller camera depth draws first despite reverse source order');
    Q := Scene.QuadAt(1);
    Check((Q.SourceIndex = 0) and (Q.Semantic = 'near-face'),
      'larger camera depth draws last');
    Check(Scene.QuadAt(0).DepthKey < Scene.QuadAt(1).DepthKey,
      'painter list depth keys increase far-to-near');
    Check(Scene.HitTest(48, 32, HitIndex),
      'view-ray-overlapping faces are hittable');
    CheckInteger(1, HitIndex, 'reverse hit chooses the nearer depth face');
    CheckText('near-face', Scene.QuadAt(HitIndex).Semantic,
      'depth hit exposes nearer public semantic');
  finally
    Scene.Free;
  end;
end;

procedure TestNonPowerOfTwoStableSort;
const
  QUAD_COUNT = 257;
  S = WFC_VOXEL3D_SUBCELL_SCALE;
var
  ExpectedIndex: Integer;
  HitIndex: Integer;
  I: Integer;
  Layer: Integer;
  O: TVoxel3DIsometricOptions;
  Q: TVoxel3DProjectedQuad;
  Quads: TVoxel3DViewQuads;
  Scene: TVoxel3DProjectedScene;
begin
  SetLength(Quads, QUAD_COUNT);
  for I := 0 to High(Quads) do
  begin
    Layer := (I * 3) mod 7;
    Quads[I] := NewTopQuad(0, 0, S, S, S, Layer,
      'layer-' + IntToStr(Layer), 'face-' + IntToStr(I));
  end;
  O := DefaultVoxel3DIsometricOptions;
  Scene := ProjectVoxel3DIsometric(Quads, O);
  try
    CheckInteger(QUAD_COUNT, Scene.QuadCount,
      'non-power-of-two painter list retains every face');
    ExpectedIndex := 0;
    for Layer := 0 to 6 do
      for I := 0 to High(Quads) do
        if ((I * 3) mod 7) = Layer then
        begin
          Q := Scene.QuadAt(ExpectedIndex);
          Check((Q.LayerOrder = Layer) and (Q.SourceIndex = I),
            'iterative merge sort preserves layer and stable source order');
          Inc(ExpectedIndex);
        end;
    CheckInteger(QUAD_COUNT, ExpectedIndex,
      'non-power-of-two expected order covers every face');
    Check(Scene.HitTest(48, 32, HitIndex),
      'large overlapping painter list remains hittable');
    CheckInteger(QUAD_COUNT - 1, HitIndex,
      'large reverse hit starts at the final painter entry');
    CheckInteger(254, Scene.QuadAt(HitIndex).SourceIndex,
      'large reverse hit resolves the final stable source in top layer');
  finally
    Scene.Free;
  end;
end;

procedure TestDeepCopyAndLifetime;
const
  S = WFC_VOXEL3D_SUBCELL_SCALE;
var
  Copy: TVoxel3DProjectedQuads;
  OriginalColor: TVoxel3DColor;
  O: TVoxel3DIsometricOptions;
  Q: TVoxel3DProjectedQuad;
  Quads: TVoxel3DViewQuads;
  Scene: TVoxel3DProjectedScene;
  Signature: TVoxel3DSignature;
begin
  Quads := OneQuad(NewTopQuad(0, 0, S, S, S, 0,
    'structure', 'roof-finish'));
  OriginalColor := Quads[0].FillColor;
  O := DefaultVoxel3DIsometricOptions;
  Scene := ProjectVoxel3DIsometric(Quads, O);
  try
    Signature := Scene.Signature;
    Quads[0].Vertices[0].X := 123456;
    Quads[0].LayerId := 'changed-input';
    Quads[0].FillColor := MakeVoxel3DColor(0, 0, 0, 0);
    SetLength(Quads, 0);
    Q := Scene.QuadAt(0);
    Check((Q.WorldVertices[0].X = 0) and
      (Q.LayerId = 'structure') and
      ColorEquals(Q.FillColor, OriginalColor),
      'scene owns a deep copy independent of input lifetime');

    Copy := Scene.CopyQuads;
    Copy[0].WorldVertices[0].X := -1;
    Copy[0].LayerId := 'changed-copy';
    Copy[0].FillColor.R := 255;
    Q := Scene.QuadAt(0);
    Check((Q.WorldVertices[0].X = 0) and
      (Q.LayerId = 'structure') and
      ColorEquals(Q.FillColor, OriginalColor),
      'copied painter list cannot mutate the scene');

    Q.LayerId := 'changed-record';
    Q.ScreenVertices[0].X := -99;
    Q := Scene.QuadAt(0);
    Check((Q.LayerId = 'structure') and
      (Q.ScreenVertices[0].X = 48),
      'QuadAt returns an independent value copy');
    Check(Scene.Signature = Signature,
      'caller mutations do not change the immutable signature');
  finally
    Scene.Free;
  end;
end;

procedure TestMetadataAndOptionRejection;
const
  S = WFC_VOXEL3D_SUBCELL_SCALE;
var
  O: TVoxel3DIsometricOptions;
  Q: TVoxel3DViewQuad;
  Raised: Boolean;
  Scene: TVoxel3DProjectedScene;
begin
  O := DefaultVoxel3DIsometricOptions;
  Q := NewTopQuad(0, 0, S, S, S, 0, 'structure', 'roof-finish');

  O.HorizontalStep := 0;
  Raised := False;
  Scene := nil;
  try
    try
      Scene := ProjectVoxel3DIsometric(OneQuad(Q), O);
    except
      on EVoxel3DIsometric do Raised := True;
    end;
  finally
    Scene.Free;
  end;
  Check(Raised, 'zero horizontal step is rejected');

  O := DefaultVoxel3DIsometricOptions;
  O.PlanVerticalStep := -1;
  Raised := False;
  Scene := nil;
  try
    try
      Scene := ProjectVoxel3DIsometric(OneQuad(Q), O);
    except
      on EVoxel3DIsometric do Raised := True;
    end;
  finally
    Scene.Free;
  end;
  Check(Raised, 'negative plan step is rejected');

  O := DefaultVoxel3DIsometricOptions;
  O.ElevationStep := 0;
  Raised := False;
  Scene := nil;
  try
    try
      Scene := ProjectVoxel3DIsometric(OneQuad(Q), O);
    except
      on EVoxel3DIsometric do Raised := True;
    end;
  finally
    Scene.Free;
  end;
  Check(Raised, 'zero elevation step is rejected');

  O := DefaultVoxel3DIsometricOptions;
  O.Margin := -1;
  Raised := False;
  Scene := nil;
  try
    try
      Scene := ProjectVoxel3DIsometric(OneQuad(Q), O);
    except
      on EVoxel3DIsometric do Raised := True;
    end;
  finally
    Scene.Free;
  end;
  Check(Raised, 'negative margin is rejected');

  O := DefaultVoxel3DIsometricOptions;
  O.Yaw := InvalidYaw;
  Raised := False;
  Scene := nil;
  try
    try
      Scene := ProjectVoxel3DIsometric(OneQuad(Q), O);
    except
      on ERangeError do Raised := True;
    end;
  finally
    Scene.Free;
  end;
  Check(Raised, 'invalid yaw is rejected');

  O := DefaultVoxel3DIsometricOptions;
  Q.LayerId := 'private:key';
  Raised := False;
  Scene := nil;
  try
    try
      Scene := ProjectVoxel3DIsometric(OneQuad(Q), O);
    except
      on EVoxel3DIsometric do Raised := True;
    end;
  finally
    Scene.Free;
  end;
  Check(Raised, 'reserved graph-key punctuation is rejected from metadata');

  Q := NewTopQuad(0, 0, S, S, S, 0, 'structure', 'roof-finish');
  Q.PrototypeId := '';
  Raised := False;
  Scene := nil;
  try
    try
      Scene := ProjectVoxel3DIsometric(OneQuad(Q), O);
    except
      on EVoxel3DIsometric do Raised := True;
    end;
  finally
    Scene.Free;
  end;
  Check(Raised, 'empty public metadata token is rejected');

  Q := NewTopQuad(0, 0, S, S, S, 0, 'structure', 'roof-finish');
  Q.CellX := -1;
  Raised := False;
  Scene := nil;
  try
    try
      Scene := ProjectVoxel3DIsometric(OneQuad(Q), O);
    except
      on EVoxel3DIsometric do Raised := True;
    end;
  finally
    Scene.Free;
  end;
  Check(Raised, 'negative source cell coordinate is rejected');

  Q := NewTopQuad(0, 0, S, S, S, 0, 'structure', 'roof-finish');
  Q.Direction := InvalidDirection;
  Raised := False;
  Scene := nil;
  try
    try
      Scene := ProjectVoxel3DIsometric(OneQuad(Q), O);
    except
      on ERangeError do Raised := True;
    end;
  finally
    Scene.Free;
  end;
  Check(Raised, 'invalid public direction is rejected');

  Q := NewTopQuad(0, 0, S, S, S, 0, 'structure', 'roof-finish');
  Q.Rotation := InvalidRotation;
  Raised := False;
  Scene := nil;
  try
    try
      Scene := ProjectVoxel3DIsometric(OneQuad(Q), O);
    except
      on ERangeError do Raised := True;
    end;
  finally
    Scene.Free;
  end;
  Check(Raised, 'invalid public rotation is rejected');
end;

procedure TestDegenerateAndOverflowRejection;
const
  S = WFC_VOXEL3D_SUBCELL_SCALE;
var
  O: TVoxel3DIsometricOptions;
  Q: TVoxel3DViewQuad;
  Raised: Boolean;
  Scene: TVoxel3DProjectedScene;
begin
  O := DefaultVoxel3DIsometricOptions;
  Q := NewTopQuad(0, 0, S, S, S, 0, 'structure', 'roof-finish');
  Q.Vertices[1] := Q.Vertices[0];
  Raised := False;
  Scene := nil;
  try
    try
      Scene := ProjectVoxel3DIsometric(OneQuad(Q), O);
    except
      on EVoxel3DIsometric do Raised := True;
    end;
  finally
    Scene.Free;
  end;
  Check(Raised, 'degenerate projected quad is rejected');

  Q := NewEastQuad;
  Q.Vertices[0].X := Low(Integer);
  O.Yaw := v3vy90;
  Raised := False;
  Scene := nil;
  try
    try
      Scene := ProjectVoxel3DIsometric(OneQuad(Q), O);
    except
      on ERangeError do Raised := True;
    end;
  finally
    Scene.Free;
  end;
  Check(Raised, 'unrepresentable yaw negation is rejected');

  Q := NewTopQuad(0, 0, S, S, S, 0, 'structure', 'roof-finish');
  Q.Vertices[1].X := High(Integer);
  O := DefaultVoxel3DIsometricOptions;
  O.HorizontalStep := 2;
  Raised := False;
  Scene := nil;
  try
    try
      Scene := ProjectVoxel3DIsometric(OneQuad(Q), O);
    except
      on ERangeError do Raised := True;
    end;
  finally
    Scene.Free;
  end;
  Check(Raised, 'projection multiplication overflow is rejected');

  Q := NewTopQuad(0, 0, 1049600, S, S, 0,
    'structure', 'roof-finish');
  O := DefaultVoxel3DIsometricOptions;
  Raised := False;
  Scene := nil;
  try
    try
      Scene := ProjectVoxel3DIsometric(OneQuad(Q), O);
    except
      on ERangeError do Raised := True;
    end;
  finally
    Scene.Free;
  end;
  Check(Raised, 'unsafe hit-test screen span is rejected');

  Q := NewTopQuad(0, 0, S, S, S, 0, 'structure', 'roof-finish');
  O := DefaultVoxel3DIsometricOptions;
  O.Margin := High(Integer);
  Raised := False;
  Scene := nil;
  try
    try
      Scene := ProjectVoxel3DIsometric(OneQuad(Q), O);
    except
      on ERangeError do Raised := True;
    end;
  finally
    Scene.Free;
  end;
  Check(Raised, 'canvas margin overflow is rejected');
end;

procedure TestSignatureSensitivity;
const
  S = WFC_VOXEL3D_SUBCELL_SCALE;
var
  Base: TVoxel3DProjectedScene;
  Changed: TVoxel3DProjectedScene;
  O: TVoxel3DIsometricOptions;
  Q: TVoxel3DViewQuad;
begin
  O := DefaultVoxel3DIsometricOptions;
  Q := NewTopQuad(0, 0, S, S, S, 0, 'structure', 'roof-finish');
  Base := ProjectVoxel3DIsometric(OneQuad(Q), O);
  Changed := nil;
  try
    O.Yaw := v3vy90;
    Changed := ProjectVoxel3DIsometric(OneQuad(Q), O);
    Check(Base.Signature <> Changed.Signature,
      'camera yaw participates in view signature');
    FreeAndNil(Changed);

    O := DefaultVoxel3DIsometricOptions;
    Q.FillColor.R := Q.FillColor.R + 1;
    Changed := ProjectVoxel3DIsometric(OneQuad(Q), O);
    Check(Base.Signature <> Changed.Signature,
      'style participates in view signature');
    FreeAndNil(Changed);

    Q := NewTopQuad(0, 0, S, S, S, 0, 'structure', 'changed');
    Changed := ProjectVoxel3DIsometric(OneQuad(Q), O);
    Check(Base.Signature <> Changed.Signature,
      'public semantic participates in view signature');
  finally
    Changed.Free;
    Base.Free;
  end;
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  try
    ATest;
    WriteLn('ok - ', AName);
  except
    on E: Exception do
    begin
      Inc(FailureCount);
      WriteLn('not ok - ', AName, ': ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

begin
  RunTest('defaults and empty projection', @TestDefaultsAndEmpty);
  RunTest('exact four-yaw projection', @TestExactFourYawProjection);
  RunTest('bounds, hit testing, and golden signature',
    @TestBoundsHitAndGoldenSignature);
  RunTest('stable painter order and reverse hit testing',
    @TestStablePainterOrderAndHit);
  RunTest('far-to-near depth painter order', @TestDepthPainterOrder);
  RunTest('non-power-of-two stable painter sort',
    @TestNonPowerOfTwoStableSort);
  RunTest('deep copy and independent lifetime', @TestDeepCopyAndLifetime);
  RunTest('metadata and option rejection', @TestMetadataAndOptionRejection);
  RunTest('degenerate and overflow rejection',
    @TestDegenerateAndOverflowRejection);
  RunTest('signature sensitivity', @TestSignatureSensitivity);
  WriteLn('Checks: ', CheckCount, ', failures: ', FailureCount);
  if FailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.Create(IntToStr(FailureCount) +
      ' voxel isometric test failures');
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
