{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
unit mapped_world_svg;
{$mode delphi}{$H+}
interface
uses mapped_world_types;
{ Safe output requires a current, correctly signed, independently valid and
  physically clear result. Diagnostics are always visibly watermarked. }
function RenderMappedWorldSvg(const AResult:TMappedWorldResult;
  const AInspection:TMappedWorldInspection; const AOptions:TMappedWorldSvgOptions):String;
function EscapeMappedWorldSvg(const AText:String):String;
implementation
uses SysUtils, wfc_lattice, mapped_world_validation;

function EscapeMappedWorldSvg(const AText:String):String;
var I:Integer;
begin
  Result:='';
  for I:=1 to Length(AText) do
    case AText[I] of
      '&':Result:=Result+'&amp;'; '<':Result:=Result+'&lt;'; '>':Result:=Result+'&gt;';
      '"':Result:=Result+'&quot;'; '''':Result:=Result+'&apos;';
      #0..#8,#11,#12,#14..#31:Result:=Result+'?';
    else Result:=Result+AText[I]; end;
end;

function RenderMappedWorldSvg(const AResult:TMappedWorldResult;
  const AInspection:TMappedWorldInspection; const AOptions:TMappedWorldSvgOptions):String;
var R:TMappedWorldValidation; I:TMappedWorldInspection; L:TMappedWorldLayer;
  J,K:Integer; C:TMappedWorldCell; B:TWfcLatticeBox; Caption,Color,Attrs,Tag:String;
  Show:Boolean;
  {$IFDEF PAS2JS}Valid:Boolean;{$ENDIF}
  function N(const V:Integer):String; begin Result:=IntToStr(V); end;
  function Size(const LowValue,HighValue:Integer):String;
  begin Result:=IntToStr(Int64(HighValue)-Int64(LowValue)); end;
  procedure Add(const S:String); begin Result:=Result+S+#10; end;
  procedure TextLine(const Y:Integer; const S:String; const Fill:String='#dae6df');
  begin Add('<text x="16" y="'+N(Y)+'" fill="'+Fill+'">'+EscapeMappedWorldSvg(S)+'</text>'); end;
  function Rect(const Box:TWfcLatticeBox; const Extra:String):String;
  begin
    Result:='<rect x="'+N(Box.Minimum.X)+'" y="'+N(Box.Minimum.Y)+'" width="'+
      Size(Box.Minimum.X,Box.Maximum.X)+'" height="'+Size(Box.Minimum.Y,Box.Maximum.Y)+'" '+Extra+'/>';
  end;
begin
  ValidateMappedWorldSnapshotShape(AResult);
  {$IFDEF PAS2JS}
  asm Valid=AOptions !== null && typeof AOptions === 'object' && !Array.isArray(AOptions) &&
    AInspection !== null && typeof AInspection === 'object' && !Array.isArray(AInspection); end;
  if not Valid then raise EMappedWorld.Create('SVG options and inspection must be records');
  {$ENDIF}
  RequireMappedWorldBoolean(AOptions.Diagnostic,'diagnostic SVG');
  RequireMappedWorldBoolean(AOptions.ShowTerrain,'show terrain');
  RequireMappedWorldBoolean(AOptions.ShowFoliage,'show foliage');
  RequireMappedWorldBoolean(AOptions.ShowHousing,'show housing');
  RequireMappedWorldBoolean(AOptions.ShowInspection,'show inspection');
  RequireMappedWorldBoolean(AInspection.IsCurrent,'inspection current');
  RequireMappedWorldInteger(AInspection.Revision,0,High(Integer),'inspection revision');
  if AInspection.Revision<>AResult.Revision then raise EMappedWorld.Create('inspection revision does not match the snapshot');
  AnalyzeMappedWorldResult(AResult,R);
  if not AOptions.Diagnostic then
  begin
    if not AInspection.IsCurrent then raise EMappedWorld.Create('current SVG is unavailable for a retained baseline');
    if (not R.ModelValid) or (not R.PhysicalSafe) or (not AResult.ModelValid) or (not AResult.PhysicalSafe) then
      raise EMappedWorld.Create('safe SVG requires selected-model validity and full physical clearance');
    if AResult.Signature<>CalculateMappedWorldSignature(AResult) then raise EMappedWorld.Create('safe SVG snapshot signature does not match');
  end;
  { Recompute all markers from the capture, not caller-provided sample lists. }
  I:=InspectMappedWorldSite(AResult,AInspection.SiteX,AInspection.SiteY,AInspection.IsCurrent);
  if not AOptions.Diagnostic then Caption:='CURRENT / MODEL VALID / PHYSICAL POLICY SAFE'
  else if not AInspection.IsCurrent then Caption:='DIAGNOSTIC / NOT CURRENT / RETAINED BASELINE'
  else if not R.ModelValid then Caption:='DIAGNOSTIC / INVALID STUDY / NOT SAFE OUTPUT'
  else if not R.PhysicalSafe then Caption:='DIAGNOSTIC / UNSAFE STUDY / NOT SAFE OUTPUT'
  else Caption:='DIAGNOSTIC / CURRENT STUDY / NOT PRODUCTION EXPORT';
  Result:='';
  Add('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 800 650" role="img" aria-labelledby="world-title world-description">');
  Add('<title id="world-title">Mapped World - '+EscapeMappedWorldSvg(Caption)+'</title>');
  Add('<desc id="world-description">Terrain, foliage and housing share actual world coordinates. X increases right and Y down in this diagram. Blocker crosses are physical obstacles, independent of the selected query.</desc>');
  Add('<rect width="800" height="650" fill="#101b19"/>');
  Add('<g font-family="monospace" font-size="13">');
  TextLine(25,'WFC / MAPPED WORLD / REVISION '+N(AResult.Revision));
  TextLine(48,Caption,'#f5ce75');
  Add('<svg x="16" y="64" width="640" height="480" viewBox="0 0 32 24" overflow="hidden">');
  for L:=Low(TMappedWorldLayer) to High(TMappedWorldLayer) do
  begin
    case L of mwlTerrain:Show:=AOptions.ShowTerrain; mwlFoliage:Show:=AOptions.ShowFoliage; mwlHousing:Show:=AOptions.ShowHousing; end;
    if not Show then Continue;
    Tag:=MappedWorldLayerName(L); Add('<g id="'+Tag+'-layer" data-layer="'+Tag+'">');
    for J:=0 to High(AResult.Layers[L].Cells) do
    begin
      C:=AResult.Layers[L].Cells[J]; B:=MappedWorldLiteralCellBox(AResult.Layers[L].Layout,J);
      Attrs:='data-layer="'+Tag+'" data-cell-index="'+N(J)+'"';
      if L=mwlHousing then Attrs:=Attrs+' data-house-index="'+N(J)+'"';
      Add('<g '+Attrs+'>');
      Add('<title>'+EscapeMappedWorldSvg(Tag+' '+N(J)+': '+C.Value)+
        ' generated='+N(Ord(C.Generated))+' locked='+N(Ord(C.Locked))+' zoned='+N(Ord(C.HasDomain))+'</title>');
      case L of
        mwlTerrain:
          begin if C.Value='land' then Color:='#295444' else if C.Value='water' then Color:='#246a8d' else Color:='#b52651';
            Add(Rect(B,'fill="'+Color+'" stroke="#101b19" stroke-width=".08"')); end;
        mwlFoliage:
          begin
            Add(Rect(B,'fill="transparent" stroke="none"'));
            if C.Value='tree' then Add('<circle cx="'+N(B.Minimum.X)+'.5" cy="'+N(B.Minimum.Y)+'.5" r=".32" fill="#9bd25c"/>')
            else if C.Value<>'clear' then Add(Rect(B,'fill="#b52651"'));
          end;
        mwlHousing:
          begin
            if C.Value='house' then Color:='#f1be6a' else Color:='none';
            Add(Rect(B,'fill="'+Color+'" fill-opacity=".32" stroke="#e9c999" stroke-width=".14" pointer-events="all"'));
            Add('<text x="'+N(B.Minimum.X+1)+'" y="'+N(B.Minimum.Y+1)+'" font-size=".65" fill="#fff4d7">site '+N(J)+'</text>');
          end;
      end;
      if C.Locked then Add(Rect(B,'fill="none" stroke="#f492c2" stroke-width=".1"'));
      Add('</g>');
    end;
    Add('</g>');
  end;
  if AOptions.ShowInspection then
  begin
    Add('<g id="inspection-layer" pointer-events="none">');
    Add(Rect(I.HouseBounds,'data-footprint="selected" fill="none" stroke="#ffffff" stroke-width=".19"'));
    if I.Sampling<>mwsPointStudy then
      Add(Rect(I.QueryBounds,'data-query="region" fill="none" stroke="#77e3de" stroke-dasharray=".3 .2" stroke-width=".12"'));
    Add('<circle data-corner="selected" cx="'+N(I.HouseBounds.Minimum.X)+'" cy="'+N(I.HouseBounds.Minimum.Y)+'" r=".24" fill="#77e3de"/>');
    for K:=0 to High(I.PhysicalBlockers) do
    begin
      B:=I.PhysicalBlockers[K].Bounds;
      Add('<path data-blocker="physical" data-layer="'+MappedWorldLayerName(I.PhysicalBlockers[K].Layer)+
        '" data-cell-index="'+N(I.PhysicalBlockers[K].CellIndex)+'" d="M '+N(B.Minimum.X)+' '+N(B.Minimum.Y)+
        ' L '+N(B.Maximum.X)+' '+N(B.Maximum.Y)+' M '+N(B.Maximum.X)+' '+N(B.Minimum.Y)+
        ' L '+N(B.Minimum.X)+' '+N(B.Maximum.Y)+'" stroke="#ff668b" stroke-width=".16"/>');
    end;
    Add('</g>');
  end;
  Add('</svg>');
  TextLine(566,'terrain: 8x6 @ pitch4 / foliage: 32x24 @ pitch1 / housing: 3x2 @ pitch8');
  TextLine(586,'site '+N(I.SiteIndex)+' sampled terrain='+N(Length(I.TerrainSamples))+' foliage='+N(Length(I.FoliageSamples))+
    ' physical blockers='+N(Length(I.PhysicalBlockers)));
  TextLine(606,I.Banner);
  TextLine(626,'snapshot '+IntToHex(AResult.Signature,8)+' / scene is world-aligned; study geometry is not a core size cap');
  Add('</g></svg>');
end;
end.
