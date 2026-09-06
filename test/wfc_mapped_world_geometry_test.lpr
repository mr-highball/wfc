{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_mapped_world_geometry_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF} SysUtils, wfc, wfc_lattice,
  mapped_world_types, mapped_world_validation, mapped_world_svg;
var Checks,Failures:Integer;
procedure Check(const OK:Boolean; const Name:String);
begin Inc(Checks); if not OK then begin Inc(Failures); WriteLn('[FAIL] ',Name); end; end;

function Fixture:TMappedWorldResult;
var L:TMappedWorldLayer; I:Integer;
begin
  Result:=Default(TMappedWorldResult); Result.Config:=DefaultMappedWorldConfig;
  Result.Revision:=1; Result.ModelVersion:=MAPPED_WORLD_MODEL_VERSION;
  Result.MappingVersion:=WFC_PASS_MAPPING_VERSION;
  for L:=Low(TMappedWorldLayer) to High(TMappedWorldLayer) do
  begin
    Result.Layers[L].Layout:=MappedWorldLayout(L);
    SetLength(Result.Layers[L].Cells,WfcLatticeCellCount(Result.Layers[L].Layout));
    for I:=0 to High(Result.Layers[L].Cells) do
    begin
      Result.Layers[L].Cells[I].Generated:=True;
      Result.Layers[L].Cells[I].Value:=MappedWorldTokens(L)[0];
    end;
  end;
  for I:=0 to 5 do Result.Demands[I]:=mwdOptional;
end;

procedure Seal(var R:TMappedWorldResult);
var V:TMappedWorldValidation;
begin AnalyzeMappedWorldResult(R,V); R.ModelValid:=V.ModelValid; R.PhysicalSafe:=V.PhysicalSafe; R.Signature:=CalculateMappedWorldSignature(R); end;

function CountText(const Needle,Haystack:String):Integer;
var I:Integer;
begin Result:=0; for I:=1 to Length(Haystack)-Length(Needle)+1 do if Copy(Haystack,I,Length(Needle))=Needle then Inc(Result); end;

function TextWitness(const S:String):Cardinal;
var I:Integer; H,L,U:Cardinal;
begin
  H:=2166136261;
  for I:=1 to Length(S) do
  begin L:=(H and $FFFF) xor Ord(S[I]); U:=(H shr 16)*403+L*256;
    L:=L*403; U:=U+(L shr 16); H:=((U and $FFFF) shl 16) or (L and $FFFF); end;
  Result:=H;
end;

procedure TestInspection;
var R:TMappedWorldResult; I:TMappedWorldInspection; V:TMappedWorldValidation;
  X,Y,J:Integer; B,C:TWfcLatticeBox;
begin
  R:=Fixture;
  Check(AnalyzeMappedWorldResult(R,V) and V.PhysicalSafe,'valid detached fixture');
  for Y:=0 to 1 do for X:=0 to 2 do
  begin
    I:=InspectMappedWorldSite(R,X,Y,True);
    Check((I.SiteIndex=Y*3+X) and (I.HouseBounds.Minimum.X=4+8*X) and
      (I.HouseBounds.Maximum.X=12+8*X) and (I.HouseBounds.Minimum.Y=4+8*Y), 'actual housing world extent');
    Check((Length(I.TerrainSamples)=4) and (Length(I.FoliageSamples)=64),'all site sample counts');
    Check(I.SelectedModelClear and I.PhysicalClear and I.IsCurrent,'all sites clear');
    for J:=0 to High(I.FoliageSamples) do
    begin
      Check((I.FoliageSamples[J].Position.X>=4+8*X) and (I.FoliageSamples[J].Position.X<12+8*X) and
        (I.FoliageSamples[J].Position.Y>=4+8*Y) and (I.FoliageSamples[J].Position.Y<12+8*Y),'literal footprint membership');
      Check(I.FoliageSamples[J].Cell.Generated and (not I.FoliageSamples[J].Cell.Locked),'sample ownership detached');
    end;
  end;
  R.Layers[mwlHousing].Cells[0].Value:='house';
  R.Layers[mwlFoliage].Cells[7*32+7].Value:='tree';
  R.Config.Sampling:=mwsPointStudy;
  Check(AnalyzeMappedWorldResult(R,V) and (not V.PhysicalSafe),'point model valid but physical house unsafe');
  I:=InspectMappedWorldSite(R,0,0,True);
  Check((Length(I.FoliageSamples)=1) and (I.FoliageSamples[0].CellIndex=4*32+4) and I.FoliageSamples[0].IsCorner,'point selects exact lower corner');
  Check((Length(I.PhysicalBlockers)=1) and (I.PhysicalBlockers[0].CellIndex=7*32+7) and
    (not I.PhysicalBlockers[0].IsCorner),'interior blocker independently visible');
  R.Config.Sampling:=mwsCell;
  Check(not AnalyzeMappedWorldResult(R,V) and (not V.PhysicalSafe),'cell model rejects interior tree');
  R.Config.Sampling:=mwsRegion;
  Check(not AnalyzeMappedWorldResult(R,V),'equal region rejects interior tree');
  R.Config.RegionMaximum:=MakeGraphOffset(1,1,1);
  Check(AnalyzeMappedWorldResult(R,V) and (not V.PhysicalSafe),'undersized region is an unsafe counterexample');
  R:=Fixture; R.Layers[mwlHousing].Cells[0].Value:='house'; R.Config.Sampling:=mwsRegion;
  R.Config.RegionMinimum:=MakeGraphOffset(-1,-1,0); R.Config.RegionMaximum:=MakeGraphOffset(9,9,1);
  I:=InspectMappedWorldSite(R,0,0,True);
  Check((Length(I.FoliageSamples)=100) and I.FoliageQueryInBounds,'setback samples actual 10x10 extent');
  R.Layers[mwlFoliage].Cells[3*32+3].Value:='tree';
  Check(not AnalyzeMappedWorldResult(R,V) and V.PhysicalSafe,'setback blocker is outside physical house');
  R.Config.RegionMinimum:=MakeGraphOffset(-5,0,0); R.Config.RegionMaximum:=MakeGraphOffset(8,8,1);
  I:=InspectMappedWorldSite(R,0,0,True);
  Check((not I.FoliageQueryInBounds) and (not I.SelectedModelClear),'partially outside region never clips to success');
  R:=Fixture; R.Layers[mwlHousing].Cells[0].Value:='house';
  R.Layers[mwlFoliage].Cells[12*32+12].Value:='tree';
  Check(AnalyzeMappedWorldResult(R,V) and V.PhysicalSafe,'half-open upper edge excluded');
  B:=MappedWorldLiteralCellBox(R.Layers[mwlFoliage].Layout,4*32+4);
  C:=B; C.Minimum.X:=B.Maximum.X; C.Maximum.X:=C.Minimum.X+1;
  Check(not MappedWorldBoxesIntersect(B,C),'touching intervals do not overlap');
  C:=B; C.Maximum.X:=C.Minimum.X;
  Check(not MappedWorldBoxesIntersect(B,C),'empty interval does not overlap');
  Check(not MappedWorldBoxContainsPoint(B,B.Maximum),'upper corner is excluded');
end;

procedure TestTampering;
var R,Q:TMappedWorldResult; V:TMappedWorldValidation; I,J:TMappedWorldInspection;
  K:Integer; Rejected:Boolean;
begin
  for K:=0 to 12 do
  begin
    R:=Fixture;
    case K of
      0:R.Layers[mwlTerrain].Cells[0].Value:='lava';
      1:R.Layers[mwlTerrain].Cells[0].Generated:=False;
      2:R.Layers[mwlTerrain].Cells[0].Locked:=True;
      3:R.Layers[mwlTerrain].Cells[0].LockValue:='land';
      4:begin R.Layers[mwlTerrain].Cells[0].HasDomain:=True; SetLength(R.Layers[mwlTerrain].Cells[0].Domain,1); R.Layers[mwlTerrain].Cells[0].Domain[0]:='water'; end;
      5:R.Layers[mwlTerrain].Cells[0].HasDomain:=True;
      6:R.Demands[5]:=mwdRequired;
      7:begin R.Layers[mwlFoliage].Cells[0].Value:='tree'; R.Layers[mwlTerrain].Cells[0].Value:='water'; end;
      8:Inc(R.Layers[mwlFoliage].Layout.Origin.X);
      9:SetLength(R.Layers[mwlFoliage].Cells,767);
      10:R.ModelVersion:=99;
      11:R.MappingVersion:=99;
      12:R.Layers[mwlHousing].Cells[5].Value:='';
    end;
    R.ModelValid:=True; R.PhysicalSafe:=True;
    Check(not AnalyzeMappedWorldResult(R,V),'arbitrary capture tampering rejected '+IntToStr(K));
    Check(Length(V.Issues)>0,'tampering has explanatory issue');
  end;
  R:=Fixture;
  R.Layers[mwlTerrain].Cells[0].Generated:=False; R.Layers[mwlTerrain].Cells[0].Locked:=True;
  R.Layers[mwlTerrain].Cells[0].LockValue:='land';
  R.Layers[mwlFoliage].Cells[4*32+4].HasDomain:=True;
  SetLength(R.Layers[mwlFoliage].Cells[4*32+4].Domain,1); R.Layers[mwlFoliage].Cells[4*32+4].Domain[0]:='clear';
  Check(AnalyzeMappedWorldResult(R,V),'consistent caller lock and domain accepted');
  Q:=CopyMappedWorldResult(R); Q.Layers[mwlFoliage].Cells[4*32+4].Domain[0]:='tree';
  Check(R.Layers[mwlFoliage].Cells[4*32+4].Domain[0]='clear','result nested domains detached');
  I:=InspectMappedWorldSite(R,0,0,False); J:=CopyMappedWorldInspection(I);
  J.FoliageSamples[0].Cell.Domain[0]:='tree';
  Check((I.FoliageSamples[0].Cell.Domain[0]='clear') and (R.Layers[mwlFoliage].Cells[4*32+4].Domain[0]='clear'),'inspection sample domains detached');
  Check((not I.IsCurrent) and (Pos('NOT CURRENT',I.Banner)>0),'baseline banner explicit');
  Rejected:=False; try InspectMappedWorldSite(R,3,0,True); except on E:Exception do Rejected:=True; end;
  Check(Rejected,'invalid site rejected');
end;

procedure TestSvg;
var R:TMappedWorldResult; I:TMappedWorldInspection; O:TMappedWorldSvgOptions;
  S,T:String; Rejected:Boolean;
begin
  R:=Fixture; R.Layers[mwlHousing].Cells[0].Value:='house'; Seal(R);
  I:=InspectMappedWorldSite(R,0,0,True); O:=DefaultMappedWorldSvgOptions;
  S:=RenderMappedWorldSvg(R,I,O); T:=RenderMappedWorldSvg(CopyMappedWorldResult(R),CopyMappedWorldInspection(I),O);
  Check(S=T,'detached deterministic SVG bytes');
  Check(CountText('<g data-layer="terrain" data-cell-index=',S)=48,'terrain renderer count');
  Check(CountText('<g data-layer="foliage" data-cell-index=',S)=768,'foliage renderer count');
  Check(CountText('data-house-index=',S)=6,'six keyboard/clickable housing targets');
  Check(CountText('pointer-events="all"',S)=6,'vacant and occupied sites have complete hit targets');
  Check((Pos('id="terrain-layer"',S)>0) and (Pos('viewBox="0 0 32 24"',S)>0),'world-aligned scene and layer IDs');
  Check((Pos('CURRENT / MODEL VALID / PHYSICAL POLICY SAFE',S)>0) and (Pos('<script',S)=0),'safe SVG label and no scripts');
  WriteLn('[GOLDEN] result=',IntToHex(R.Signature,8),' svg=',IntToHex(TextWitness(S),8),' bytes=',Length(S));
  Check(R.Signature=$50D1C15C,'portable snapshot signature golden');
  Check((TextWitness(S)=$3B5D754A) and (Length(S)=159429),'portable SVG exact-byte witness golden');
  I.Banner:='<script a="x">&''</script>'; T:=RenderMappedWorldSvg(R,I,O);
  Check(T=S,'forged caller banner cannot replace independently computed wording');
  Check(EscapeMappedWorldSvg('<script a="x">&''</script>')='&lt;script a=&quot;x&quot;&gt;&amp;&apos;&lt;/script&gt;',
    'SVG text escapes every XML delimiter');
  I.IsCurrent:=False; Rejected:=False;
  try RenderMappedWorldSvg(R,I,O); except on E:Exception do Rejected:=True; end;
  Check(Rejected,'safe SVG denies retained baseline');
  O.Diagnostic:=True; T:=RenderMappedWorldSvg(R,I,O);
  Check(Pos('DIAGNOSTIC / NOT CURRENT / RETAINED BASELINE',T)>0,'retained diagnostic watermark');
  I.IsCurrent:=True; O.Diagnostic:=False; Inc(R.Signature); Rejected:=False;
  try RenderMappedWorldSvg(R,I,O); except on E:Exception do Rejected:=True; end;
  Check(Rejected,'safe SVG rejects tampered signature');
  R.Config.Sampling:=mwsPointStudy; R.Layers[mwlFoliage].Cells[7*32+7].Value:='tree'; Seal(R);
  I:=InspectMappedWorldSite(R,0,0,True); Rejected:=False;
  try RenderMappedWorldSvg(R,I,O); except on E:Exception do Rejected:=True; end;
  Check(Rejected,'safe SVG denies physically unsafe current study');
  O.Diagnostic:=True; S:=RenderMappedWorldSvg(R,I,O);
  Check((Pos('DIAGNOSTIC / UNSAFE STUDY',S)>0) and (CountText('data-blocker="physical"',S)=1),'unsafe study diagnostic and blocker marker');
  I.PhysicalBlockers:=nil; I.QueryBounds:=Default(TWfcLatticeBox);
  Check(RenderMappedWorldSvg(R,I,O)=S,'renderer independently recomputes forged inspector geometry');
  Inc(I.Revision); Rejected:=False;
  try RenderMappedWorldSvg(R,I,O); except on E:Exception do Rejected:=True; end;
  Check(Rejected,'diagnostic rejects mixed snapshot/inspection revisions');
end;

procedure TestConfiguration;
var C:TMappedWorldConfig; O:TMappedWorldSearchOptions; K:Integer; Rejected:Boolean;
begin
  for K:=0 to 6 do
  begin
    C:=DefaultMappedWorldConfig;
    case K of
      0:C.RegionMaximum:=MakeGraphOffset(0,8,1);
      1:C.RegionMinimum:=MakeGraphOffset(0,9,0);
      2:C.RegionMaximum:=MakeGraphOffset(8,8,0);
      3:C.LandWeight:=0;
      4:C.ClearWeight:=-1;
      5:C.RegionMaximum.DeltaX:=High(Integer);
      6:C.RegionMaximum.DeltaY:=High(Integer);
    end;
    Rejected:=False; try ValidateMappedWorldConfig(C); except on E:Exception do Rejected:=True; end;
    Check(Rejected,'invalid configuration rejected '+IntToStr(K));
  end;
  C:=DefaultMappedWorldConfig; C.RegionMinimum.DeltaX:=Low(Integer); ValidateMappedWorldConfig(C);
  Check(True,'representable negative region remains available');
  O:=DefaultMappedWorldSearchOptions; O.MaxBacktracks:=0; O.MaxPassBacktracks:=0; ValidateMappedWorldSearchOptions(O);
  Check(True,'zero search budgets accepted');
  O.MaxBacktracks:=High(Integer); O.MaxPassBacktracks:=High(Integer); ValidateMappedWorldSearchOptions(O);
  Check(True,'search budget maximum is portable Integer not a showcase cap');
end;

{$IFDEF PAS2JS}
procedure TestHostileJavaScript;
var R:TMappedWorldResult; C:TMappedWorldConfig; O:TMappedWorldSearchOptions;
  V:TMappedWorldValidation; K:Integer; Rejected:Boolean;
  SvgOptions:TMappedWorldSvgOptions; Inspection:TMappedWorldInspection;
begin
  for K:=0 to 12 do
  begin
    { Each raw-JavaScript case needs a fresh record: a previous null nested
      record deliberately cannot be reused by Pascal's in-place assignment. }
    asm C=pas.mapped_world_types.TMappedWorldConfig.$new(); end;
    C:=DefaultMappedWorldConfig;
    asm
      switch(K) {
      case 0:C.Seed='3';break; case 1:C.LandWeight='12';break;
      case 2:C.Preset=true;break;case 3:C.Sampling=NaN;break;
      case 4:C.RegionMaximum.DeltaX=Infinity;break;
      case 5:C.RegionMinimum=null;break;case 6:C.TreeWeight=0.5;break;
      case 7:C.Seed=4294967296;break;case 8:C.Sampling=0.5;break;
      case 9:C=null;break;case 10:C.Seed=-1;break;
      case 11:C.RegionMaximum.DeltaZ='1';break;case 12:C=[];break;
      }
    end;
    Rejected:=False; try ValidateMappedWorldConfig(C); except on E:Exception do Rejected:=True; end;
    Check(Rejected,'hostile JavaScript config '+IntToStr(K));
  end;
  for K:=0 to 19 do
  begin
    asm R=pas.mapped_world_types.TMappedWorldResult.$new(); end;
    R:=Fixture;
    asm
      switch(K) {
      case 0:R.Layers=null;break;case 1:R.Layers[1].Cells[0]=null;break;
      case 2:R.Layers[0].Cells[0].Generated=1;break;case 3:R.Demands[0]='1';break;
      case 4:R.Layers[0].Cells[0].Domain={};break;case 5:R.Layers[0].Cells[0].Value=1;break;
      case 6:R.Layers[1].Layout.Pitch.X='1';break;case 7:R.Revision=Infinity;break;
      case 8:R.Signature='0';break;case 9:R.TraceSignature=NaN;break;
      case 10:R.TranscriptSignature=-1;break;
      case 11:R.Config=null;break;case 12:R.Layers[0].Layout=null;break;
      case 13:R.Layers[1].Cells=undefined;break;case 14:R.Demands.length=5;break;
      case 15:R.Layers[0].Cells[0].Domain=null;break;case 16:R=null;break;
      case 17:R.Layers[0]=[];break;case 18:R.Layers[0].Cells[0].LockValue=false;break;
      case 19:R.Layers[0].Cells[0].Domain='land';break;
      }
    end;
    Check(not AnalyzeMappedWorldResult(R,V),'hostile JavaScript snapshot '+IntToStr(K));
  end;
  O:=DefaultMappedWorldSearchOptions; asm O.MaxBacktracks='64'; end;
  Rejected:=False; try ValidateMappedWorldSearchOptions(O); except on E:Exception do Rejected:=True; end;
  Check(Rejected,'hostile JavaScript budget');
  asm O=null; end;
  Rejected:=False; try ValidateMappedWorldSearchOptions(O); except on E:Exception do Rejected:=True; end;
  Check(Rejected,'null JavaScript search options produce a typed error');
  asm R=pas.mapped_world_types.TMappedWorldResult.$new(); end;
  R:=Fixture; Seal(R);
  for K:=0 to 4 do
  begin
    asm
      SvgOptions=pas.mapped_world_types.TMappedWorldSvgOptions.$new();
      Inspection=pas.mapped_world_types.TMappedWorldInspection.$new();
    end;
    SvgOptions:=DefaultMappedWorldSvgOptions; Inspection:=InspectMappedWorldSite(R,0,0,True);
    asm
      switch(K) {
      case 0:SvgOptions=null;break;case 1:Inspection=null;break;
      case 2:SvgOptions.Diagnostic=0;break;case 3:Inspection.Revision='1';break;
      case 4:Inspection.IsCurrent=1;break;
      }
    end;
    Rejected:=False;
    try RenderMappedWorldSvg(R,Inspection,SvgOptions); except on E:Exception do Rejected:=True; end;
    Check(Rejected,'hostile JavaScript SVG input '+IntToStr(K));
  end;
end;
{$ENDIF}
begin
  try TestInspection; TestTampering; TestSvg; TestConfiguration;
    {$IFDEF PAS2JS}TestHostileJavaScript;{$ENDIF}
  except on E:Exception do begin Inc(Failures); WriteLn('[EXCEPTION] ',E.ClassName,': ',E.Message); end; end;
  WriteLn('Mapped world geometry: ',Checks,' checks, ',Failures,' failures');
  if Failures<>0 then Halt(1);
end.
