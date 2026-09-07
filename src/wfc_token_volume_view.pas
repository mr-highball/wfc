{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
unit wfc_token_volume_view;
{$mode delphi}{$H+}
interface
uses SysUtils,wfc_model,wfc_voxel3d_isometric;
const
  WFC_TOKEN_VOLUME_VIEW_VERSION = 1;
  WFC_TOKEN_VOLUME_VIEW_DEFAULT_MAX_QUADS = 131072;
type
  EWfcTokenVolumeView = class(Exception);
  TWfcTokenVolumeViewOptions = record
    Projection: TVoxel3DIsometricOptions;
    HiddenTokenIndex: Integer;
    VisibleDepth: Integer;
    MaxQuads: Integer;
  end;

function DefaultWfcTokenVolumeViewOptions(const ADepth:Integer):TWfcTokenVolumeViewOptions;
{ Tokens are X-fast, then Y, then Z. This is a finite visual cutout, not a
  periodic mesh or a physical/material model. Visible tokens are opaque unit
  cubes; all internal faces are culled, including unlike-token neighbors.
  Hide/cut options never alter the input volume. The caller owns the scene.
  Metadata uses token-N palette indices, never raw tokens or graph keys.
  MaxQuads is an explicit caller budget: an excess rejects, never truncates. }
function ProjectWfcTokenVolume3D(const ATokens:TWfcModelTokens;
  const AWidth,AHeight,ADepth:Integer; const APalette:TWfcModelTokens;
  const AOptions:TWfcTokenVolumeViewOptions):TVoxel3DProjectedScene;

implementation
uses wfc,wfc_voxel3d;
type TIndices=array of Integer;

procedure RequireInteger(const V,Minimum,Maximum:Integer; const Name:String);
{$IFDEF PAS2JS}var Valid:Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid=typeof V==='number' && Number.isFinite(V) && Math.floor(V)===V; end;
  if not Valid then raise EWfcTokenVolumeView.Create(Name+' must be a finite Integer');
  {$ENDIF}
  if (V<Minimum) or(V>Maximum) then raise EWfcTokenVolumeView.Create(Name+' is out of range');
end;

procedure RequireToken(const T:TWfcModelToken; const Name:String);
{$IFDEF PAS2JS}var Valid:Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid=typeof T==='string'; end;
  if not Valid then raise EWfcTokenVolumeView.Create(Name+' must be a string');
  {$ENDIF}
  if not WfcModelTokenIsValid(T) or(Length(T)=0) then
    raise EWfcTokenVolumeView.Create(Name+' must contain nonempty Unicode scalar values');
end;

function Multiply(const A,B:Integer; const Name:String):Integer;
begin
  if A>High(Integer) div B then raise EWfcTokenVolumeView.Create(Name+' exceeds Integer capacity');
  Result:=A*B;
end;

function Add(const A,B:Integer; const Name:String):Integer;
begin
  if A>High(Integer)-B then raise EWfcTokenVolumeView.Create(Name+' exceeds Integer capacity');
  Result:=A+B;
end;

function DefaultWfcTokenVolumeViewOptions(const ADepth:Integer):TWfcTokenVolumeViewOptions;
begin
  RequireInteger(ADepth,1,High(Integer),'view depth');
  Result.Projection:=DefaultVoxel3DIsometricOptions;
  Result.HiddenTokenIndex:=-1; Result.VisibleDepth:=ADepth;
  Result.MaxQuads:=WFC_TOKEN_VOLUME_VIEW_DEFAULT_MAX_QUADS;
end;

function HashToken(const T:TWfcModelToken):Cardinal;
{$PUSH}{$Q-}
var I:Integer;
begin
  Result:=5381;
  for I:=1 to Length(T) do Result:=((Result shl 5)+Result+Cardinal(Ord(T[I]))) and Cardinal($FFFFFFFF);
end;
{$POP}

function PaletteSlot(const T:TWfcModelToken; const Palette:TWfcModelTokens;
  const Slots:TIndices):Integer;
begin
  Result:=Integer(HashToken(T) and Cardinal(Length(Slots)-1));
  while Slots[Result]<>0 do
  begin
    if Palette[Slots[Result]-1]=T then Exit;
    Result:=(Result+1) and(Length(Slots)-1);
  end;
end;

function TokenColor(const Index:Integer; const Token:TWfcModelToken;
  const Direction:TGraphDirection):TVoxel3DColor;
var R,G,B,Shade:Integer;
begin
  { Stable index colors are deliberately presentation-only. Named examples
    have familiar colors without assigning any solver/material semantics. }
  case Index mod 6 of
    0:begin R:=177; G:=157; B:=117; end;
    1:begin R:=97; G:=170; B:=152; end;
    2:begin R:=189; G:=135; B:=151; end;
    3:begin R:=115; G:=155; B:=203; end;
    4:begin R:=197; G:=178; B:=102; end;
    else begin R:=147; G:=133; B:=185; end;
  end;
  if Token='stone' then begin R:=164; G:=156; B:=134; end
  else if Token='leaf' then begin R:=99; G:=167; B:=107; end
  else if Token='air' then begin R:=154; G:=195; B:=208; end;
  Shade:=100;
  if Direction in [gdNorth,gdSouth] then Shade:=84
  else if Direction in [gdEast,gdWest] then Shade:=70
  else if Direction=gdDown then Shade:=60;
  Result:=MakeVoxel3DColor(R*Shade div 100,G*Shade div 100,B*Shade div 100,255);
end;

procedure SetFace(var Q:TVoxel3DViewQuad; const X,Y,Z:Integer;
  const D:TGraphDirection);
var X0,Y0,Z0,X1,Y1,Z1:Integer;
  procedure V(const I,A,B,C:Integer);
  begin Q.Vertices[I]:=MakeVoxel3DViewPoint3(A,B,C); end;
begin
  X0:=X*WFC_VOXEL3D_SUBCELL_SCALE; X1:=X0+WFC_VOXEL3D_SUBCELL_SCALE;
  Y0:=Y*WFC_VOXEL3D_SUBCELL_SCALE; Y1:=Y0+WFC_VOXEL3D_SUBCELL_SCALE;
  Z0:=Z*WFC_VOXEL3D_SUBCELL_SCALE; Z1:=Z0+WFC_VOXEL3D_SUBCELL_SCALE;
  { Same outward cyclic vertex orientation as wfc_voxel3d_mesh. }
  case D of
    gdNorth:begin V(0,X0,Y1,Z0); V(1,X0,Y1,Z1); V(2,X1,Y1,Z1); V(3,X1,Y1,Z0); end;
    gdEast: begin V(0,X1,Y0,Z0); V(1,X1,Y1,Z0); V(2,X1,Y1,Z1); V(3,X1,Y0,Z1); end;
    gdSouth:begin V(0,X0,Y0,Z0); V(1,X1,Y0,Z0); V(2,X1,Y0,Z1); V(3,X0,Y0,Z1); end;
    gdWest: begin V(0,X0,Y0,Z0); V(1,X0,Y0,Z1); V(2,X0,Y1,Z1); V(3,X0,Y1,Z0); end;
    gdUp:   begin V(0,X0,Y0,Z1); V(1,X1,Y0,Z1); V(2,X1,Y1,Z1); V(3,X0,Y1,Z1); end;
    gdDown: begin V(0,X0,Y0,Z0); V(1,X0,Y1,Z0); V(2,X1,Y1,Z0); V(3,X1,Y0,Z0); end;
  end;
end;

function ProjectWfcTokenVolume3D(const ATokens:TWfcModelTokens;
  const AWidth,AHeight,ADepth:Integer; const APalette:TWfcModelTokens;
  const AOptions:TWfcTokenVolumeViewOptions):TVoxel3DProjectedScene;
var Cells,PaletteCount,Capacity,I,J,X,Y,Z,Index,Count,N,Plan,Elevation:Integer;
  Slots,Indices:TIndices; Quads:TVoxel3DViewQuads; D:TGraphDirection;

  function Visible(const AX,AY,AZ:Integer):Boolean;
  begin
    if (AX<0) or(AY<0) or(AZ<0) or(AX>=AWidth) or(AY>=AHeight) or
      (AZ>=AOptions.VisibleDepth) then Exit(False);
    Result:=Indices[(AZ*AHeight+AY)*AWidth+AX]<>AOptions.HiddenTokenIndex;
  end;

  function Exposed(const Direction:TGraphDirection):Boolean;
  begin
    case Direction of
      gdNorth:Result:=not Visible(X,Y+1,Z);
      gdEast:Result:=not Visible(X+1,Y,Z);
      gdSouth:Result:=not Visible(X,Y-1,Z);
      gdWest:Result:=not Visible(X-1,Y,Z);
      gdUp:Result:=not Visible(X,Y,Z+1);
      gdDown:Result:=not Visible(X,Y,Z-1);
      else Result:=False;
    end;
  end;

begin
  Result:=nil;
  RequireInteger(AWidth,1,High(Integer),'width');
  RequireInteger(AHeight,1,High(Integer),'height');
  RequireInteger(ADepth,1,High(Integer),'depth');
  Cells:=Multiply(Multiply(AWidth,AHeight,'volume cells'),ADepth,'volume cells');
  if Length(ATokens)<>Cells then raise EWfcTokenVolumeView.Create('volume token count does not match XYZ shape');
  if (Length(APalette)=0) or(Length(APalette)>High(Integer) div 2) then
    raise EWfcTokenVolumeView.Create('palette size is outside the index-table capacity');
  PaletteCount:=Length(APalette);
  RequireInteger(AOptions.HiddenTokenIndex,-1,PaletteCount-1,'hidden token index');
  RequireInteger(AOptions.VisibleDepth,1,ADepth,'visible depth');
  RequireInteger(AOptions.MaxQuads,0,High(Integer),'quad budget');
  RequireInteger(Ord(AOptions.Projection.Yaw),0,3,'view yaw');
  RequireInteger(AOptions.Projection.HorizontalStep,1,High(Integer),'horizontal step');
  RequireInteger(AOptions.Projection.PlanVerticalStep,1,High(Integer),'plan vertical step');
  RequireInteger(AOptions.Projection.ElevationStep,1,High(Integer),'elevation step');
  RequireInteger(AOptions.Projection.Margin,0,(High(Integer)-1) div 2,'view margin');
  { Conservative enclosing-cutout arithmetic preflight. The existing
    projector independently checks actual pixel spans, convexity and fitting.
    These fixed-subcell products are numeric limits, not solver/grid limits. }
  Plan:=Multiply(Add(AWidth,AHeight,'view plan'),WFC_VOXEL3D_SUBCELL_SCALE,'view plan');
  Elevation:=Multiply(AOptions.VisibleDepth,WFC_VOXEL3D_SUBCELL_SCALE,'view elevation');
  Add(Plan,Elevation,'view depth key');
  Multiply(Plan,AOptions.Projection.HorizontalStep,'horizontal projection');
  Add(Multiply(Plan,AOptions.Projection.PlanVerticalStep,'plan projection') div WFC_VOXEL3D_SUBCELL_SCALE,
    Multiply(Elevation,AOptions.Projection.ElevationStep,'vertical projection') div WFC_VOXEL3D_SUBCELL_SCALE,
    'vertical span');
  for I:=0 to PaletteCount-1 do RequireToken(APalette[I],'palette token');
  Capacity:=2;
  while Capacity div 2<PaletteCount do
  begin
    if Capacity>High(Integer) div 2 then raise EWfcTokenVolumeView.Create('palette index table exceeds Integer capacity');
    Capacity:=Capacity*2;
  end;
  SetLength(Slots,Capacity);
  for I:=0 to PaletteCount-1 do
  begin
    J:=PaletteSlot(APalette[I],APalette,Slots);
    if Slots[J]<>0 then raise EWfcTokenVolumeView.Create('palette tokens must be unique');
    Slots[J]:=I+1;
  end;
  SetLength(Indices,Cells);
  for I:=0 to Cells-1 do
  begin
    RequireToken(ATokens[I],'volume token');
    J:=PaletteSlot(ATokens[I],APalette,Slots);
    if Slots[J]=0 then raise EWfcTokenVolumeView.Create('volume token is outside the palette');
    Indices[I]:=Slots[J]-1;
  end;
  Count:=0;
  for Z:=0 to AOptions.VisibleDepth-1 do for Y:=0 to AHeight-1 do for X:=0 to AWidth-1 do
    if Visible(X,Y,Z) then for D:=Low(TGraphDirection) to High(TGraphDirection) do
      if Exposed(D) then
      begin
        if Count=AOptions.MaxQuads then raise EWfcTokenVolumeView.Create('exposed face count exceeds the quad budget');
        Inc(Count);
      end;
  { No quad, projected-quad or sort arrays exist before the complete count
    and membership preflight succeeds. Indexed tokens/slots are temporary. }
  SetLength(Quads,Count); N:=0;
  for Z:=0 to AOptions.VisibleDepth-1 do for Y:=0 to AHeight-1 do for X:=0 to AWidth-1 do
    if Visible(X,Y,Z) then for D:=Low(TGraphDirection) to High(TGraphDirection) do
      if Exposed(D) then
      begin
        Index:=Indices[(Z*AHeight+Y)*AWidth+X];
        Quads[N].CellX:=X; Quads[N].CellY:=Y; Quads[N].CellZ:=Z;
        Quads[N].Direction:=D; Quads[N].Rotation:=v3r0; Quads[N].LayerOrder:=0;
        Quads[N].LayerId:='tokens'; Quads[N].PrototypeId:='token-'+IntToStr(Index);
        Quads[N].Material:=Quads[N].PrototypeId; Quads[N].Semantic:=Quads[N].PrototypeId;
        Quads[N].FillColor:=TokenColor(Index,APalette[Index],D);
        Quads[N].EdgeColor:=MakeVoxel3DColor(42,54,56,255);
        SetFace(Quads[N],X,Y,Z,D); Inc(N);
      end;
  Result:=ProjectVoxel3DIsometric(Quads,AOptions.Projection);
end;
end.
