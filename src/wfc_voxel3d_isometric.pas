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
unit wfc_voxel3d_isometric;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_voxel3d;

const
  WFC_VOXEL3D_ISOMETRIC_VERSION = 1;

  //View-space geometry uses signed fixed subcells. This lets domain layers
  //describe inset trim and smaller props without introducing floating point.
  WFC_VOXEL3D_SUBCELL_SCALE = 1024;

  //Convex hit testing uses signed 32-bit cross products. Restricting the
  //auto-fitted content span makes the largest difference of two products fit
  //without Int64 on both native FPC and pas2js.
  WFC_VOXEL3D_MAX_SCREEN_SPAN = 32767;

type
  EVoxel3DIsometric = class(EVoxel3D);

  TVoxel3DColor = record
    R: Byte;
    G: Byte;
    B: Byte;
    A: Byte;
  end;

  TVoxel3DViewPoint3 = record
    X: Integer;
    Y: Integer;
    Z: Integer;
  end;
  TVoxel3DViewVertices = array[0..3] of TVoxel3DViewPoint3;

  TVoxel3DViewYaw = (
    v3vy0,
    v3vy90,
    v3vy180,
    v3vy270
  );

  //The view quad contains only public, renderer-facing metadata. Every text
  //field is a portable voxel token, so private graph keys (which contain
  //reserved punctuation) cannot cross this boundary.
  TVoxel3DViewQuad = record
    Vertices: TVoxel3DViewVertices;
    CellX: Integer;
    CellY: Integer;
    CellZ: Integer;
    Direction: TGraphDirection;
    Rotation: TVoxel3DRotation;
    LayerOrder: Integer;
    LayerId: String;
    PrototypeId: String;
    Material: String;
    Semantic: String;
    FillColor: TVoxel3DColor;
    EdgeColor: TVoxel3DColor;
  end;
  TVoxel3DViewQuads = array of TVoxel3DViewQuad;

  //HorizontalStep is the X displacement of one voxel along either plan axis.
  //PlanVerticalStep is its downward screen displacement. ElevationStep is the
  //upward displacement of one voxel along +Z. All values are integer pixels.
  TVoxel3DIsometricOptions = record
    Yaw: TVoxel3DViewYaw;
    HorizontalStep: Integer;
    PlanVerticalStep: Integer;
    ElevationStep: Integer;
    Margin: Integer;
  end;

  TVoxel3DScreenPoint = record
    X: Integer;
    Y: Integer;
  end;
  TVoxel3DScreenVertices = array[0..3] of TVoxel3DScreenPoint;

  //Left/Top/Right/Bottom are inclusive content bounds after auto-fitting.
  //Width/Height are the complete canvas size, including the requested margin.
  //An empty scene has every bounds field set to zero.
  TVoxel3DScreenBounds = record
    Left: Integer;
    Top: Integer;
    Right: Integer;
    Bottom: Integer;
    Width: Integer;
    Height: Integer;
  end;

  TVoxel3DProjectedQuad = record
    WorldVertices: TVoxel3DViewVertices;
    ScreenVertices: TVoxel3DScreenVertices;
    CellX: Integer;
    CellY: Integer;
    CellZ: Integer;
    Direction: TGraphDirection;
    Rotation: TVoxel3DRotation;
    LayerOrder: Integer;
    LayerId: String;
    PrototypeId: String;
    Material: String;
    Semantic: String;
    FillColor: TVoxel3DColor;
    EdgeColor: TVoxel3DColor;
    DepthKey: Integer;
    SourceIndex: Integer;
  end;
  TVoxel3DProjectedQuads = array of TVoxel3DProjectedQuad;

  { TVoxel3DProjectedScene }

  //An immutable, auto-fitted painter list. Quads are ordered far-to-near by
  //an explicit stable merge sort whose complete tie break is host-independent.
  TVoxel3DProjectedScene = class
  private
    FBounds: TVoxel3DScreenBounds;
    FOptions: TVoxel3DIsometricOptions;
    FQuads: TVoxel3DProjectedQuads;
    FSignature: TVoxel3DSignature;
    function GetQuadCount: Integer;
  public
    constructor Create(const AQuads: TVoxel3DViewQuads;
      const AOptions: TVoxel3DIsometricOptions);
    function QuadAt(const AIndex: Integer): TVoxel3DProjectedQuad;
    function CopyQuads: TVoxel3DProjectedQuads;

    //The returned index addresses this scene's sorted painter list. Edges are
    //inside. Overlap resolves to the last (nearest/topmost) drawn quad.
    function HitTest(const AX, AY: Integer;
      out AQuadIndex: Integer): Boolean;

    property Bounds: TVoxel3DScreenBounds read FBounds;
    property Options: TVoxel3DIsometricOptions read FOptions;
    property QuadCount: Integer read GetQuadCount;
    property Signature: TVoxel3DSignature read FSignature;
  end;

function MakeVoxel3DColor(const AR, AG, AB,
  AA: Byte): TVoxel3DColor;
function MakeVoxel3DViewPoint3(const AX, AY,
  AZ: Integer): TVoxel3DViewPoint3;
function DefaultVoxel3DIsometricOptions: TVoxel3DIsometricOptions;

//The returned scene is caller-owned.
function ProjectVoxel3DIsometric(const AQuads: TVoxel3DViewQuads;
  const AOptions: TVoxel3DIsometricOptions): TVoxel3DProjectedScene;

implementation

const
  FNV_OFFSET_BASIS = Cardinal(2166136261);

function MakeVoxel3DColor(const AR, AG, AB,
  AA: Byte): TVoxel3DColor;
begin
  Result.R := AR;
  Result.G := AG;
  Result.B := AB;
  Result.A := AA;
end;

function MakeVoxel3DViewPoint3(const AX, AY,
  AZ: Integer): TVoxel3DViewPoint3;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
end;

function DefaultVoxel3DIsometricOptions: TVoxel3DIsometricOptions;
begin
  Result.Yaw := v3vy0;
  Result.HorizontalStep := 32;
  Result.PlanVerticalStep := 16;
  Result.ElevationStep := 32;
  Result.Margin := 16;
end;

function CheckedAdd(const ALeft, ARight: Integer;
  const AOperation: String): Integer;
begin
  if ((ARight > 0) and (ALeft > High(Integer) - ARight)) or
      ((ARight < 0) and (ALeft < Low(Integer) - ARight)) then
    raise ERangeError.Create(AOperation + ' exceeds Integer');
  Result := ALeft + ARight;
end;

function CheckedSubtract(const ALeft, ARight: Integer;
  const AOperation: String): Integer;
begin
  if ((ARight > 0) and (ALeft < Low(Integer) + ARight)) or
      ((ARight < 0) and (ALeft > High(Integer) + ARight)) then
    raise ERangeError.Create(AOperation + ' exceeds Integer');
  Result := ALeft - ARight;
end;

function CheckedNegate(const AValue: Integer;
  const AOperation: String): Integer;
begin
  if AValue = Low(Integer) then
    raise ERangeError.Create(AOperation + ' exceeds Integer');
  Result := -AValue;
end;

function CheckedMultiply(const ALeft, ARight: Integer;
  const AOperation: String): Integer;
begin
  if (ALeft = 0) or (ARight = 0) then
    Exit(0);
  if ALeft > 0 then
  begin
    if ARight > 0 then
    begin
      if ALeft > High(Integer) div ARight then
        raise ERangeError.Create(AOperation + ' exceeds Integer');
    end
    else if ARight < Low(Integer) div ALeft then
      raise ERangeError.Create(AOperation + ' exceeds Integer');
  end
  else if ARight > 0 then
  begin
    if ALeft < Low(Integer) div ARight then
      raise ERangeError.Create(AOperation + ' exceeds Integer');
  end
  else if ALeft < High(Integer) div ARight then
    raise ERangeError.Create(AOperation + ' exceeds Integer');
  Result := ALeft * ARight;
end;

function IsValidYaw(const AYaw: TVoxel3DViewYaw): Boolean;
begin
  Result := Ord(AYaw) <= Ord(High(TVoxel3DViewYaw));
end;

function IsValidDirection(const ADirection: TGraphDirection): Boolean;
begin
  Result := (Ord(ADirection) >= Ord(Low(TGraphDirection))) and
    (Ord(ADirection) <= Ord(High(TGraphDirection)));
end;

function IsValidRotation(const ARotation: TVoxel3DRotation): Boolean;
begin
  Result := (Ord(ARotation) >= Ord(Low(TVoxel3DRotation))) and
    (Ord(ARotation) <= Ord(High(TVoxel3DRotation)));
end;

procedure ValidateOptions(const AOptions: TVoxel3DIsometricOptions);
begin
  if not IsValidYaw(AOptions.Yaw) then
    raise ERangeError.CreateFmt(
      'voxel isometric yaw is out of bounds [%d]', [Ord(AOptions.Yaw)]);
  if AOptions.HorizontalStep <= 0 then
    raise EVoxel3DIsometric.Create(
      'voxel isometric horizontal step must be positive');
  if AOptions.PlanVerticalStep <= 0 then
    raise EVoxel3DIsometric.Create(
      'voxel isometric plan vertical step must be positive');
  if AOptions.ElevationStep <= 0 then
    raise EVoxel3DIsometric.Create(
      'voxel isometric elevation step must be positive');
  if AOptions.Margin < 0 then
    raise EVoxel3DIsometric.Create(
      'voxel isometric margin cannot be negative');
end;

procedure ValidateViewQuad(const AQuad: TVoxel3DViewQuad;
  const AIndex: Integer);
begin
  if (AQuad.CellX < 0) or (AQuad.CellY < 0) or
      (AQuad.CellZ < 0) then
    raise EVoxel3DIsometric.CreateFmt(
      'voxel view quad %d has a negative cell coordinate', [AIndex]);
  if not IsValidDirection(AQuad.Direction) then
    raise ERangeError.CreateFmt(
      'voxel view quad %d direction is out of bounds [%d]',
      [AIndex, Ord(AQuad.Direction)]);
  if not IsValidRotation(AQuad.Rotation) then
    raise ERangeError.CreateFmt(
      'voxel view quad %d rotation is out of bounds [%d]',
      [AIndex, Ord(AQuad.Rotation)]);
  if not IsVoxel3DToken(AQuad.LayerId) then
    raise EVoxel3DIsometric.CreateFmt(
      'voxel view quad %d layer id is not a portable token', [AIndex]);
  if not IsVoxel3DToken(AQuad.PrototypeId) then
    raise EVoxel3DIsometric.CreateFmt(
      'voxel view quad %d prototype id is not a portable token', [AIndex]);
  if not IsVoxel3DToken(AQuad.Material) then
    raise EVoxel3DIsometric.CreateFmt(
      'voxel view quad %d material is not a portable token', [AIndex]);
  if not IsVoxel3DToken(AQuad.Semantic) then
    raise EVoxel3DIsometric.CreateFmt(
      'voxel view quad %d semantic is not a portable token', [AIndex]);
end;

procedure RotatePoint(const APoint: TVoxel3DViewPoint3;
  const AYaw: TVoxel3DViewYaw; out ARotatedX,
  ARotatedY: Integer);
begin
  case AYaw of
    v3vy0:
      begin
        ARotatedX := APoint.X;
        ARotatedY := APoint.Y;
      end;
    v3vy90:
      begin
        ARotatedX := APoint.Y;
        ARotatedY := CheckedNegate(APoint.X,
          'voxel isometric yaw rotation');
      end;
    v3vy180:
      begin
        ARotatedX := CheckedNegate(APoint.X,
          'voxel isometric yaw rotation');
        ARotatedY := CheckedNegate(APoint.Y,
          'voxel isometric yaw rotation');
      end;
    v3vy270:
      begin
        ARotatedX := CheckedNegate(APoint.Y,
          'voxel isometric yaw rotation');
        ARotatedY := APoint.X;
      end;
  else
    raise ERangeError.Create('voxel isometric yaw is out of bounds');
  end;
end;

function ScaleSubcells(const AValue, AStep: Integer;
  const AOperation: String): Integer;
begin
  Result := CheckedMultiply(AValue, AStep, AOperation) div
    WFC_VOXEL3D_SUBCELL_SCALE;
end;

procedure ProjectPoint(const APoint: TVoxel3DViewPoint3;
  const AOptions: TVoxel3DIsometricOptions;
  out AScreenPoint: TVoxel3DScreenPoint;
  out ADepth: Integer);
var
  LPlanSum: Integer;
  LPlanDifference: Integer;
  LRotatedX: Integer;
  LRotatedY: Integer;
  LScreenPlanY: Integer;
  LScreenZ: Integer;
begin
  RotatePoint(APoint, AOptions.Yaw, LRotatedX, LRotatedY);
  LPlanDifference := CheckedSubtract(LRotatedX, LRotatedY,
    'voxel isometric horizontal projection');
  LPlanSum := CheckedAdd(LRotatedX, LRotatedY,
    'voxel isometric plan projection');
  AScreenPoint.X := ScaleSubcells(LPlanDifference,
    AOptions.HorizontalStep, 'voxel isometric horizontal projection');
  LScreenPlanY := ScaleSubcells(LPlanSum,
    AOptions.PlanVerticalStep, 'voxel isometric plan projection');
  LScreenZ := ScaleSubcells(APoint.Z, AOptions.ElevationStep,
    'voxel isometric elevation projection');
  AScreenPoint.Y := CheckedSubtract(LScreenPlanY, LScreenZ,
    'voxel isometric vertical projection');
  ADepth := CheckedAdd(LPlanSum, APoint.Z,
    'voxel isometric depth');
end;

function CopyProjectedQuad(
  const AQuad: TVoxel3DProjectedQuad): TVoxel3DProjectedQuad;
var
  I: Integer;
begin
  for I := Low(Result.WorldVertices) to High(Result.WorldVertices) do
  begin
    Result.WorldVertices[I] := AQuad.WorldVertices[I];
    Result.ScreenVertices[I] := AQuad.ScreenVertices[I];
  end;
  Result.CellX := AQuad.CellX;
  Result.CellY := AQuad.CellY;
  Result.CellZ := AQuad.CellZ;
  Result.Direction := AQuad.Direction;
  Result.Rotation := AQuad.Rotation;
  Result.LayerOrder := AQuad.LayerOrder;
  Result.LayerId := AQuad.LayerId;
  Result.PrototypeId := AQuad.PrototypeId;
  Result.Material := AQuad.Material;
  Result.Semantic := AQuad.Semantic;
  Result.FillColor := AQuad.FillColor;
  Result.EdgeColor := AQuad.EdgeColor;
  Result.DepthKey := AQuad.DepthKey;
  Result.SourceIndex := AQuad.SourceIndex;
end;

procedure CopyViewMetadata(const ASource: TVoxel3DViewQuad;
  const ASourceIndex: Integer; var ADestination: TVoxel3DProjectedQuad);
var
  I: Integer;
begin
  for I := Low(ASource.Vertices) to High(ASource.Vertices) do
    ADestination.WorldVertices[I] := ASource.Vertices[I];
  ADestination.CellX := ASource.CellX;
  ADestination.CellY := ASource.CellY;
  ADestination.CellZ := ASource.CellZ;
  ADestination.Direction := ASource.Direction;
  ADestination.Rotation := ASource.Rotation;
  ADestination.LayerOrder := ASource.LayerOrder;
  ADestination.LayerId := ASource.LayerId;
  ADestination.PrototypeId := ASource.PrototypeId;
  ADestination.Material := ASource.Material;
  ADestination.Semantic := ASource.Semantic;
  ADestination.FillColor := ASource.FillColor;
  ADestination.EdgeColor := ASource.EdgeColor;
  ADestination.SourceIndex := ASourceIndex;
end;

function CompareProjectedQuads(const ALeft,
  ARight: TVoxel3DProjectedQuad): Integer;
begin
  if ALeft.DepthKey < ARight.DepthKey then Exit(-1);
  if ALeft.DepthKey > ARight.DepthKey then Exit(1);
  if ALeft.LayerOrder < ARight.LayerOrder then Exit(-1);
  if ALeft.LayerOrder > ARight.LayerOrder then Exit(1);
  if ALeft.CellZ < ARight.CellZ then Exit(-1);
  if ALeft.CellZ > ARight.CellZ then Exit(1);
  if ALeft.CellY < ARight.CellY then Exit(-1);
  if ALeft.CellY > ARight.CellY then Exit(1);
  if ALeft.CellX < ARight.CellX then Exit(-1);
  if ALeft.CellX > ARight.CellX then Exit(1);
  if Ord(ALeft.Direction) < Ord(ARight.Direction) then Exit(-1);
  if Ord(ALeft.Direction) > Ord(ARight.Direction) then Exit(1);
  if ALeft.SourceIndex < ARight.SourceIndex then Exit(-1);
  if ALeft.SourceIndex > ARight.SourceIndex then Exit(1);
  Result := 0;
end;

procedure StableSortProjectedQuads(var AQuads: TVoxel3DProjectedQuads);
var
  I: Integer;
  LCount: Integer;
  LLeft: Integer;
  LLeftCursor: Integer;
  LMiddle: Integer;
  LOutput: Integer;
  LRight: Integer;
  LRightCursor: Integer;
  LWidth: Integer;
  LTemporary: TVoxel3DProjectedQuads;
begin
  LCount := Length(AQuads);
  if LCount < 2 then
    Exit;
  SetLength(LTemporary, LCount);
  LWidth := 1;
  while LWidth < LCount do
  begin
    LLeft := 0;
    while LLeft < LCount do
    begin
      if LLeft > LCount - LWidth then
        LMiddle := LCount
      else
        LMiddle := LLeft + LWidth;
      if LMiddle > LCount - LWidth then
        LRight := LCount
      else
        LRight := LMiddle + LWidth;
      LLeftCursor := LLeft;
      LRightCursor := LMiddle;
      LOutput := LLeft;
      while (LLeftCursor < LMiddle) and (LRightCursor < LRight) do
      begin
        //Choosing the left item on equality is the stability guarantee.
        if CompareProjectedQuads(AQuads[LLeftCursor],
            AQuads[LRightCursor]) <= 0 then
        begin
          LTemporary[LOutput] := CopyProjectedQuad(AQuads[LLeftCursor]);
          Inc(LLeftCursor);
        end
        else
        begin
          LTemporary[LOutput] := CopyProjectedQuad(AQuads[LRightCursor]);
          Inc(LRightCursor);
        end;
        Inc(LOutput);
      end;
      while LLeftCursor < LMiddle do
      begin
        LTemporary[LOutput] := CopyProjectedQuad(AQuads[LLeftCursor]);
        Inc(LLeftCursor);
        Inc(LOutput);
      end;
      while LRightCursor < LRight do
      begin
        LTemporary[LOutput] := CopyProjectedQuad(AQuads[LRightCursor]);
        Inc(LRightCursor);
        Inc(LOutput);
      end;
      LLeft := LRight;
    end;
    for I := 0 to LCount - 1 do
      AQuads[I] := CopyProjectedQuad(LTemporary[I]);
    if LWidth > LCount div 2 then
      LWidth := LCount
    else
      LWidth := LWidth * 2;
  end;
end;

function CrossProduct(const AFirst, ASecond,
  APoint: TVoxel3DScreenPoint): Integer;
var
  LEdgeX: Integer;
  LEdgeY: Integer;
  LPointX: Integer;
  LPointY: Integer;
  LProductA: Integer;
  LProductB: Integer;
begin
  //Every difference is bounded by WFC_VOXEL3D_MAX_SCREEN_SPAN before
  //this routine is called. Two products and their difference therefore fit.
  LEdgeX := ASecond.X - AFirst.X;
  LEdgeY := ASecond.Y - AFirst.Y;
  LPointX := APoint.X - AFirst.X;
  LPointY := APoint.Y - AFirst.Y;
  LProductA := LEdgeX * LPointY;
  LProductB := LEdgeY * LPointX;
  Result := LProductA - LProductB;
end;

procedure ValidateProjectedConvexQuad(
  const AQuad: TVoxel3DProjectedQuad);
var
  I: Integer;
  LCross: Integer;
  LHasNegative: Boolean;
  LHasPositive: Boolean;
begin
  LHasNegative := False;
  LHasPositive := False;
  for I := Low(AQuad.ScreenVertices) to High(AQuad.ScreenVertices) do
  begin
    LCross := CrossProduct(AQuad.ScreenVertices[I],
      AQuad.ScreenVertices[(I + 1) mod 4],
      AQuad.ScreenVertices[(I + 2) mod 4]);
    if LCross < 0 then
      LHasNegative := True
    else if LCross > 0 then
      LHasPositive := True
    else
      raise EVoxel3DIsometric.CreateFmt(
        'voxel view quad %d projects to a degenerate polygon',
        [AQuad.SourceIndex]);
    if LHasNegative and LHasPositive then
      raise EVoxel3DIsometric.CreateFmt(
        'voxel view quad %d does not project to a convex polygon',
        [AQuad.SourceIndex]);
  end;
end;

function PointInProjectedQuad(const AX, AY: Integer;
  const AQuad: TVoxel3DProjectedQuad): Boolean;
var
  I: Integer;
  LCross: Integer;
  LHasNegative: Boolean;
  LHasPositive: Boolean;
  LPoint: TVoxel3DScreenPoint;
begin
  LPoint.X := AX;
  LPoint.Y := AY;
  LHasNegative := False;
  LHasPositive := False;
  for I := Low(AQuad.ScreenVertices) to High(AQuad.ScreenVertices) do
  begin
    LCross := CrossProduct(AQuad.ScreenVertices[I],
      AQuad.ScreenVertices[(I + 1) mod 4], LPoint);
    if LCross < 0 then
      LHasNegative := True
    else if LCross > 0 then
      LHasPositive := True;
    if LHasNegative and LHasPositive then
      Exit(False);
  end;
  Result := True;
end;

procedure HashByte(var AHash: Cardinal; const AValue: Byte);
{$PUSH}
{$Q-}
var
  LValue: Cardinal;
begin
  AHash := AHash xor Cardinal(AValue);
  LValue := AHash;
  AHash := (LValue + (LValue shl 1) + (LValue shl 4) +
    (LValue shl 7) + (LValue shl 8) + (LValue shl 24)) and
    Cardinal($FFFFFFFF);
end;
{$POP}

procedure HashCardinal(var AHash: Cardinal; const AValue: Cardinal);
begin
  HashByte(AHash, Byte(AValue and $FF));
  HashByte(AHash, Byte((AValue shr 8) and $FF));
  HashByte(AHash, Byte((AValue shr 16) and $FF));
  HashByte(AHash, Byte((AValue shr 24) and $FF));
end;

procedure HashInteger(var AHash: Cardinal; const AValue: Integer);
var
  LMagnitude: Cardinal;
begin
  if AValue < 0 then
  begin
    HashByte(AHash, 1);
    //This form represents Low(Integer) without attempting to negate it.
    LMagnitude := Cardinal(-(AValue + 1));
    Inc(LMagnitude);
  end
  else
  begin
    HashByte(AHash, 0);
    LMagnitude := Cardinal(AValue);
  end;
  HashCardinal(AHash, LMagnitude);
end;

procedure HashText(var AHash: Cardinal; const AValue: String);
var
  I: Integer;
begin
  HashCardinal(AHash, Cardinal(Length(AValue)));
  for I := 1 to Length(AValue) do
    HashByte(AHash, Byte(Ord(AValue[I])));
end;

procedure HashColor(var AHash: Cardinal; const AColor: TVoxel3DColor);
begin
  HashByte(AHash, AColor.R);
  HashByte(AHash, AColor.G);
  HashByte(AHash, AColor.B);
  HashByte(AHash, AColor.A);
end;

function CalculateSignature(const AOptions: TVoxel3DIsometricOptions;
  const ABounds: TVoxel3DScreenBounds;
  const AQuads: TVoxel3DProjectedQuads): TVoxel3DSignature;
var
  I: Integer;
  J: Integer;
begin
  Result := FNV_OFFSET_BASIS;
  HashText(Result, 'wfc-voxel3d-isometric');
  HashCardinal(Result, WFC_VOXEL3D_ISOMETRIC_VERSION);
  HashCardinal(Result, WFC_VOXEL3D_SUBCELL_SCALE);
  HashCardinal(Result, Cardinal(Ord(AOptions.Yaw)));
  HashInteger(Result, AOptions.HorizontalStep);
  HashInteger(Result, AOptions.PlanVerticalStep);
  HashInteger(Result, AOptions.ElevationStep);
  HashInteger(Result, AOptions.Margin);
  HashInteger(Result, ABounds.Left);
  HashInteger(Result, ABounds.Top);
  HashInteger(Result, ABounds.Right);
  HashInteger(Result, ABounds.Bottom);
  HashInteger(Result, ABounds.Width);
  HashInteger(Result, ABounds.Height);
  HashCardinal(Result, Cardinal(Length(AQuads)));
  for I := 0 to High(AQuads) do
  begin
    HashInteger(Result, AQuads[I].SourceIndex);
    HashInteger(Result, AQuads[I].DepthKey);
    HashInteger(Result, AQuads[I].LayerOrder);
    HashInteger(Result, AQuads[I].CellX);
    HashInteger(Result, AQuads[I].CellY);
    HashInteger(Result, AQuads[I].CellZ);
    HashCardinal(Result, Cardinal(Ord(AQuads[I].Direction)));
    HashCardinal(Result, Cardinal(Ord(AQuads[I].Rotation)));
    HashText(Result, AQuads[I].LayerId);
    HashText(Result, AQuads[I].PrototypeId);
    HashText(Result, AQuads[I].Material);
    HashText(Result, AQuads[I].Semantic);
    HashColor(Result, AQuads[I].FillColor);
    HashColor(Result, AQuads[I].EdgeColor);
    for J := Low(AQuads[I].WorldVertices) to
        High(AQuads[I].WorldVertices) do
    begin
      HashInteger(Result, AQuads[I].WorldVertices[J].X);
      HashInteger(Result, AQuads[I].WorldVertices[J].Y);
      HashInteger(Result, AQuads[I].WorldVertices[J].Z);
      HashInteger(Result, AQuads[I].ScreenVertices[J].X);
      HashInteger(Result, AQuads[I].ScreenVertices[J].Y);
    end;
  end;
end;

{ TVoxel3DProjectedScene }

constructor TVoxel3DProjectedScene.Create(
  const AQuads: TVoxel3DViewQuads;
  const AOptions: TVoxel3DIsometricOptions);
var
  I: Integer;
  J: Integer;
  LDepth: Integer;
  LDepthMaximum: Integer;
  LDepthMinimum: Integer;
  LMaximumX: Integer;
  LMaximumY: Integer;
  LMinimumX: Integer;
  LMinimumY: Integer;
  LSpanX: Integer;
  LSpanY: Integer;
begin
  inherited Create;
  ValidateOptions(AOptions);
  FOptions := AOptions;
  SetLength(FQuads, Length(AQuads));

  if Length(AQuads) = 0 then
  begin
    FBounds.Left := 0;
    FBounds.Top := 0;
    FBounds.Right := 0;
    FBounds.Bottom := 0;
    FBounds.Width := 0;
    FBounds.Height := 0;
    FSignature := CalculateSignature(FOptions, FBounds, FQuads);
    Exit;
  end;

  LMinimumX := High(Integer);
  LMinimumY := High(Integer);
  LMaximumX := Low(Integer);
  LMaximumY := Low(Integer);
  for I := 0 to High(AQuads) do
  begin
    ValidateViewQuad(AQuads[I], I);
    CopyViewMetadata(AQuads[I], I, FQuads[I]);
    LDepthMinimum := High(Integer);
    LDepthMaximum := Low(Integer);
    for J := Low(AQuads[I].Vertices) to High(AQuads[I].Vertices) do
    begin
      ProjectPoint(AQuads[I].Vertices[J], FOptions,
        FQuads[I].ScreenVertices[J], LDepth);
      if FQuads[I].ScreenVertices[J].X < LMinimumX then
        LMinimumX := FQuads[I].ScreenVertices[J].X;
      if FQuads[I].ScreenVertices[J].X > LMaximumX then
        LMaximumX := FQuads[I].ScreenVertices[J].X;
      if FQuads[I].ScreenVertices[J].Y < LMinimumY then
        LMinimumY := FQuads[I].ScreenVertices[J].Y;
      if FQuads[I].ScreenVertices[J].Y > LMaximumY then
        LMaximumY := FQuads[I].ScreenVertices[J].Y;
      if LDepth < LDepthMinimum then
        LDepthMinimum := LDepth;
      if LDepth > LDepthMaximum then
        LDepthMaximum := LDepth;
    end;
    //The midpoint of the depth interval is a stable face-center key without
    //summing four potentially large vertex depths.
    FQuads[I].DepthKey := CheckedAdd(LDepthMinimum,
      CheckedSubtract(LDepthMaximum, LDepthMinimum,
        'voxel isometric depth interval') div 2,
      'voxel isometric depth midpoint');
  end;

  LSpanX := CheckedSubtract(LMaximumX, LMinimumX,
    'voxel isometric horizontal span');
  LSpanY := CheckedSubtract(LMaximumY, LMinimumY,
    'voxel isometric vertical span');
  if (LSpanX > WFC_VOXEL3D_MAX_SCREEN_SPAN) or
      (LSpanY > WFC_VOXEL3D_MAX_SCREEN_SPAN) then
    raise ERangeError.CreateFmt(
      'voxel isometric content span exceeds %d pixels',
      [WFC_VOXEL3D_MAX_SCREEN_SPAN]);

  for I := 0 to High(FQuads) do
    for J := Low(FQuads[I].ScreenVertices) to
        High(FQuads[I].ScreenVertices) do
    begin
      FQuads[I].ScreenVertices[J].X := CheckedAdd(
        CheckedSubtract(FQuads[I].ScreenVertices[J].X, LMinimumX,
          'voxel isometric horizontal fitting'), FOptions.Margin,
        'voxel isometric horizontal margin');
      FQuads[I].ScreenVertices[J].Y := CheckedAdd(
        CheckedSubtract(FQuads[I].ScreenVertices[J].Y, LMinimumY,
          'voxel isometric vertical fitting'), FOptions.Margin,
        'voxel isometric vertical margin');
    end;

  FBounds.Left := FOptions.Margin;
  FBounds.Top := FOptions.Margin;
  FBounds.Right := CheckedAdd(FOptions.Margin, LSpanX,
    'voxel isometric right bound');
  FBounds.Bottom := CheckedAdd(FOptions.Margin, LSpanY,
    'voxel isometric bottom bound');
  FBounds.Width := CheckedAdd(
    CheckedAdd(FBounds.Right, FOptions.Margin,
      'voxel isometric canvas width'), 1,
    'voxel isometric canvas width');
  FBounds.Height := CheckedAdd(
    CheckedAdd(FBounds.Bottom, FOptions.Margin,
      'voxel isometric canvas height'), 1,
    'voxel isometric canvas height');

  for I := 0 to High(FQuads) do
    ValidateProjectedConvexQuad(FQuads[I]);
  StableSortProjectedQuads(FQuads);
  FSignature := CalculateSignature(FOptions, FBounds, FQuads);
end;

function TVoxel3DProjectedScene.GetQuadCount: Integer;
begin
  Result := Length(FQuads);
end;

function TVoxel3DProjectedScene.QuadAt(
  const AIndex: Integer): TVoxel3DProjectedQuad;
begin
  if (AIndex < 0) or (AIndex >= Length(FQuads)) then
    raise ERangeError.CreateFmt(
      'voxel projected quad index out of bounds [%d]', [AIndex]);
  Result := CopyProjectedQuad(FQuads[AIndex]);
end;

function TVoxel3DProjectedScene.CopyQuads: TVoxel3DProjectedQuads;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FQuads));
  for I := 0 to High(FQuads) do
    Result[I] := CopyProjectedQuad(FQuads[I]);
end;

function TVoxel3DProjectedScene.HitTest(const AX, AY: Integer;
  out AQuadIndex: Integer): Boolean;
var
  I: Integer;
begin
  AQuadIndex := -1;
  if Length(FQuads) = 0 then
    Exit(False);
  if (AX < FBounds.Left) or (AX > FBounds.Right) or
      (AY < FBounds.Top) or (AY > FBounds.Bottom) then
    Exit(False);
  for I := High(FQuads) downto 0 do
    if PointInProjectedQuad(AX, AY, FQuads[I]) then
    begin
      AQuadIndex := I;
      Exit(True);
    end;
  Result := False;
end;

function ProjectVoxel3DIsometric(const AQuads: TVoxel3DViewQuads;
  const AOptions: TVoxel3DIsometricOptions): TVoxel3DProjectedScene;
begin
  Result := TVoxel3DProjectedScene.Create(AQuads, AOptions);
end;

end.
