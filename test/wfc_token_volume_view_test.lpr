{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_token_volume_view_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,wfc,wfc_model,wfc_voxel3d,wfc_voxel3d_isometric,wfc_voxel3d_svg,wfc_token_volume_view;
type TTest=procedure;
const
  SCENE_GOLDENS:array[0..3] of String=('2B1B2B3D','1A0B2F3C','7E73B576','54BA37CF');
  SVG_GOLDENS:array[0..3] of String=('9F3233CF','1059E0E5','76F149B3','2446CA66');
var Checks,Failures:Integer;
procedure Check(const OK:Boolean; const Msg:String);
begin Inc(Checks); if not OK then begin Inc(Failures); WriteLn('[FAIL] ',Msg); end; end;
procedure Run(const Name:String; const Test:TTest);
begin WriteLn('[TEST] ',Name); try Test; except on E:Exception do
  begin Inc(Failures); WriteLn('[EXCEPTION] ',E.Message); end; end; end;
function Tokens(const A:array of TWfcModelToken):TWfcModelTokens;
var I:Integer;
begin Result:=nil; SetLength(Result,Length(A)); for I:=0 to High(A) do Result[I]:=A[I]; end;
function Note:TWfcModelToken;
begin {$IFDEF PAS2JS}Result:=Chr($266B);{$ELSE}Result:=UTF8Encode(UnicodeString(WideChar($266B)));{$ENDIF} end;
function HashAscii(const S:String):Cardinal;
{$PUSH}{$Q-}
var I:Integer; V:Cardinal;
begin
  Result:=2166136261;
  for I:=1 to Length(S) do begin V:=Result xor Cardinal(Ord(S[I]));
    Result:=(V+(V shl 1)+(V shl 4)+(V shl 7)+(V shl 8)+(V shl 24)) and Cardinal($FFFFFFFF); end;
end;
{$POP}

procedure Reject(const T:TWfcModelTokens; const W,H,D:Integer;
  const P:TWfcModelTokens; const O:TWfcTokenVolumeViewOptions; const Fragment:String);
var S:TVoxel3DProjectedScene; Raised:Boolean;
begin
  S:=nil; Raised:=False;
  try
    try S:=ProjectWfcTokenVolume3D(T,W,H,D,P,O);
    except on E:Exception do begin Raised:=(Fragment='') or(Pos(Fragment,E.Message)>0);
      if not Raised then WriteLn('[DETAIL] ',E.Message); end; end;
    Check(Raised,'reject '+Fragment);
  finally S.Free; end;
end;

procedure TestCubeAndCut;
var T,P:TWfcModelTokens; O:TWfcTokenVolumeViewOptions; S:TVoxel3DProjectedScene;
  I:Integer; Q:TVoxel3DProjectedQuads; Sig:Cardinal;
begin
  P:=Tokens(['stone']); T:=Tokens(['stone']); O:=DefaultWfcTokenVolumeViewOptions(1);
  Check((O.HiddenTokenIndex=-1) and(O.VisibleDepth=1) and(O.MaxQuads=131072),'explicit default full cutout and budget');
  S:=ProjectWfcTokenVolume3D(T,1,1,1,P,O);
  try
    Check(S.QuadCount=6,'one cube has all six boundary faces, no wrapped culling');
    Sig:=S.Signature; Q:=S.CopyQuads; Q[0].Semantic:='changed'; Q[0].WorldVertices[0].X:=-1;
    T[0]:='changed'; P[0]:='changed';
    Check((S.Signature=Sig) and(S.QuadAt(0).Semantic='token-0') and
      (S.QuadAt(0).WorldVertices[0].X>=0),'projected geometry and metadata fully detached');
  finally S.Free; end;
  P:=Tokens(['stone']); SetLength(T,12); for I:=0 to 11 do T[I]:='stone';
  O:=DefaultWfcTokenVolumeViewOptions(2); S:=ProjectWfcTokenVolume3D(T,2,3,2,P,O);
  try Check(S.QuadCount=32,'rectangular solid culls every internal neighbor face'); finally S.Free; end;
  O.VisibleDepth:=1; O.MaxQuads:=22; S:=ProjectWfcTokenVolume3D(T,2,3,2,P,O);
  try
    Check(S.QuadCount=22,'Z cutoff exposes exactly the newly cut surface');
    for I:=0 to S.QuadCount-1 do Check(S.QuadAt(I).CellZ=0,'cut geometry has no highZ metadata');
  finally S.Free; end;
  O.MaxQuads:=21; Reject(T,2,3,2,P,O,'quad budget');
  O.HiddenTokenIndex:=0; O.MaxQuads:=0; S:=ProjectWfcTokenVolume3D(T,2,3,2,P,O);
  try Check((S.QuadCount=0) and(S.Bounds.Width=0) and(S.Bounds.Height=0),'empty hidden volume fits zero budget'); finally S.Free; end;
  Check((Length(T)=12) and(T[11]='stone'),'cut and hidden options never edit the score');
end;

procedure TestFacesAndYaw;
var T,P:TWfcModelTokens; O:TWfcTokenVolumeViewOptions; S:TVoxel3DProjectedScene;
  Q:TVoxel3DProjectedQuad; Expected,Seen:array of Boolean;
  X,Y,Z,I,J,K,Yaw,Total,NX,NY,NZ,Hit:Integer; D:TGraphDirection;
  SVG:String;
begin
  P:=Tokens(['stone','air','leaf']); SetLength(T,18);
  for I:=0 to 17 do T[I]:=P[(I+(I div 6)) mod 3];
  SetLength(Expected,18*6); Total:=0;
  for Z:=0 to 2 do for Y:=0 to 1 do for X:=0 to 2 do
  begin
    I:=(Z*2+Y)*3+X;
    if T[I]='air' then Continue;
    for D:=Low(TGraphDirection) to High(TGraphDirection) do
    begin
      NX:=X; NY:=Y; NZ:=Z;
      case D of gdNorth:Inc(NY); gdEast:Inc(NX); gdSouth:Dec(NY);
        gdWest:Dec(NX); gdUp:Inc(NZ); gdDown:Dec(NZ); end;
      K:=I*6+Ord(D);
      if (NX<0) or(NX>=3) or(NY<0) or(NY>=2) or(NZ<0) or(NZ>=3) then Expected[K]:=True
      else Expected[K]:=T[(NZ*2+NY)*3+NX]='air';
      if Expected[K] then Inc(Total);
    end;
  end;
  for Yaw:=0 to 3 do
  begin
    O:=DefaultWfcTokenVolumeViewOptions(3); O.HiddenTokenIndex:=1;
    O.Projection.Yaw:=TVoxel3DViewYaw(Yaw); O.MaxQuads:=Total;
    S:=ProjectWfcTokenVolume3D(T,3,2,3,P,O);
    try
      Seen:=nil; SetLength(Seen,Length(Expected));
      Check(S.QuadCount=Total,'all yaw modes retain the exact exposed unit-face set');
      for I:=0 to S.QuadCount-1 do
      begin
        Q:=S.QuadAt(I); K:=((Q.CellZ*2+Q.CellY)*3+Q.CellX)*6+Ord(Q.Direction);
        Check(Expected[K] and not Seen[K],'independent fullXYZ face oracle, no duplicates'); Seen[K]:=True;
        Check(Q.Semantic='token-'+IntToStr((((Q.CellZ*2+Q.CellY)*3+Q.CellX)+Q.CellZ) mod 3),
          'palette index metadata resolves exact source token');
        for J:=0 to 3 do
        begin
          Check((Q.WorldVertices[J].X>=Q.CellX*1024) and(Q.WorldVertices[J].X<=(Q.CellX+1)*1024) and
            (Q.WorldVertices[J].Y>=Q.CellY*1024) and(Q.WorldVertices[J].Y<=(Q.CellY+1)*1024) and
            (Q.WorldVertices[J].Z>=Q.CellZ*1024) and(Q.WorldVertices[J].Z<=(Q.CellZ+1)*1024),
            'world vertices remain the exact public cell cube');
          case Q.Direction of
            gdNorth:Check(Q.WorldVertices[J].Y=(Q.CellY+1)*1024,'north positiveY plane');
            gdEast:Check(Q.WorldVertices[J].X=(Q.CellX+1)*1024,'east positiveX plane');
            gdSouth:Check(Q.WorldVertices[J].Y=Q.CellY*1024,'south negativeY plane');
            gdWest:Check(Q.WorldVertices[J].X=Q.CellX*1024,'west negativeX plane');
            gdUp:Check(Q.WorldVertices[J].Z=(Q.CellZ+1)*1024,'up positiveZ plane');
            gdDown:Check(Q.WorldVertices[J].Z=Q.CellZ*1024,'down negativeZ plane');
          end;
        end;
      end;
      for I:=0 to High(Expected) do Check(Expected[I]=Seen[I],'complete literal face set retained');
      Q:=S.QuadAt(S.QuadCount-1); X:=0; Y:=0;
      for J:=0 to 3 do begin Inc(X,Q.ScreenVertices[J].X); Inc(Y,Q.ScreenVertices[J].Y); end;
      Check(S.HitTest(X div 4,Y div 4,Hit) and(Hit=S.QuadCount-1),'existing painter-aware hit testing returns public XYZ face');
      SVG:=EncodeVoxel3DProjectedSceneSvg(S,DefaultVoxel3DSvgOptions);
      Check((Pos('data-index=',SVG)>0) and(Pos('token-2',SVG)>0),'existing SVG exporter retains selectable metadata');
      WriteLn('[INFO] yaw=',Yaw,' scene=',IntToHex(S.Signature,8),' svg=',IntToHex(HashAscii(SVG),8));
      Check(IntToHex(S.Signature,8)=SCENE_GOLDENS[Yaw],'portable scene signature golden');
      Check(IntToHex(HashAscii(SVG),8)=SVG_GOLDENS[Yaw],'byte-exact portable SVG golden');
    finally S.Free; end;
  end;
end;

procedure TestTokensAndGuards;
var T,P:TWfcModelTokens; O:TWfcTokenVolumeViewOptions; S:TVoxel3DProjectedScene;
  SVG:String; I:Integer;
begin
  P:=Tokens(['<script>&"',Note,'@p3v1;public-text']); T:=Copy(P,0,Length(P));
  O:=DefaultWfcTokenVolumeViewOptions(1); S:=ProjectWfcTokenVolume3D(T,3,1,1,P,O);
  try
    Check(S.QuadCount=14,'unlike visible neighbors also cull internal faces');
    SVG:=EncodeVoxel3DProjectedSceneSvg(S,DefaultVoxel3DSvgOptions);
    Check((Pos('<script>',SVG)=0) and(Pos('@p3v1',SVG)=0) and(Pos('token-1',SVG)>0),
      'Unicode and punctuation palette uses safe index metadata only');
  finally S.Free; end;
  O:=DefaultWfcTokenVolumeViewOptions(1);
  Reject(T,0,1,1,P,O,'width'); Reject(T,High(Integer),2,1,P,O,'capacity');
  Reject(T,2,1,1,P,O,'count'); Reject(T,3,1,1,nil,O,'palette size');
  P[1]:=P[0]; Reject(T,3,1,1,P,O,'unique');
  P:=Tokens(['A']); T:=Tokens(['A','unknown']);
  O:=DefaultWfcTokenVolumeViewOptions(2); O.VisibleDepth:=1; O.HiddenTokenIndex:=0;
  Reject(T,1,1,2,P,O,'outside the palette');
  T:=Tokens(['A']); O:=DefaultWfcTokenVolumeViewOptions(1);
  for I:=0 to 8 do
  begin
    O:=DefaultWfcTokenVolumeViewOptions(1);
    case I of
      0:O.HiddenTokenIndex:=1; 1:O.HiddenTokenIndex:=-2; 2:O.VisibleDepth:=0;
      3:O.VisibleDepth:=2; 4:O.MaxQuads:=-1; 5:O.Projection.HorizontalStep:=0;
      6:O.Projection.PlanVerticalStep:=High(Integer); 7:O.Projection.ElevationStep:=0;
      8:O.Projection.Margin:=High(Integer);
    end;
    Reject(T,1,1,1,P,O,'');
  end;
end;

{$IFDEF PAS2JS}
procedure TestHostileJS;
var T,P:TWfcModelTokens; O:TWfcTokenVolumeViewOptions; I,J,W:Integer; Bad:Integer;
begin
  for I:=0 to 9 do
  begin
    asm Bad=[NaN,Infinity,-Infinity,0.5,'1',null,undefined,true,{},[]][I]; end;
    for J:=0 to 10 do
    begin
      T:=Tokens(['A']); P:=Tokens(['A']); W:=1; O:=DefaultWfcTokenVolumeViewOptions(1);
      case J of
        0:W:=Bad; 1:O.VisibleDepth:=Bad; 2:O.HiddenTokenIndex:=Bad; 3:O.MaxQuads:=Bad;
        4:O.Projection.HorizontalStep:=Bad; 5:O.Projection.PlanVerticalStep:=Bad;
        6:O.Projection.ElevationStep:=Bad; 7:O.Projection.Margin:=Bad;
        8:asm O.Projection.Yaw=Bad; end;
        9:asm T[0]=Bad; end;
        10:asm P[0]=Bad; end;
      end;
      Reject(T,W,1,1,P,O,'');
    end;
  end;
end;
{$ENDIF}

begin
  Run('cubes, bounded cutout and ownership',TestCubeAndCut);
  Run('independent XYZ faces and four yaws',TestFacesAndYaw);
  Run('public tokens, metadata and preflight',TestTokensAndGuards);
  {$IFDEF PAS2JS}Run('typed-JS hostile values',TestHostileJS);{$ENDIF}
  WriteLn('Checks: ',Checks,'  Failures: ',Failures);
  if Failures<>0 then begin
    {$IFDEF PAS2JS}raise Exception.Create('token volume view checks failed');{$ELSE}Halt(1);{$ENDIF}
  end;
end.
