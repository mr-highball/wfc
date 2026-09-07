{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Portable local-grid diagnostic, not a physical safety export. }
unit pipeline_workspace_view;
{$mode delphi}{$H+}
interface
uses SysUtils, pipeline_workspace_workbench;
type
  EWfcPipelineWorkspaceView = class(Exception);
  TWfcPipelineWorkspaceSlice = record
    Version, PassIndex, StartX, StartY, SliceZ, Width, Height: Integer;
    CellPixels, MaxRenderedCells, MaxSvgBytes: Integer;
    UseBaseline: Boolean;
  end;

{ No mutation or solver call. Renders exactly the requested local XY window at
  one local Z, rejecting an out-of-bounds window rather than silently clipping.
  World origin/pitch and each half-open cell footprint remain explicit metadata.
  All token/label text uses canonical percent encoding, so arbitrary Unicode or
  XML control/markup characters never become active SVG markup. Palette colours
  are presentation-only. Retained baselines are ALWAYS labelled historical.
  MaxRenderedCells/MaxSvgBytes bound this visual output, not composition extent,
  core solver memory or detached state copies made before rendering. Typed live
  owners and ordinary synchronous access only; not a hostile owner/Proxy sandbox. }
function PipelineWorkspaceSliceSvg(const Workbench: TWfcPipelineWorkspaceWorkbench;
  const Options: TWfcPipelineWorkspaceSlice): String;

implementation
uses wfc_model, wfc_lattice, wfc_pipeline_model, wfc_pipeline_run,
  wfc_pipeline_session, wfc_text_codec;

procedure ViewError(const Detail: String);
begin raise EWfcPipelineWorkspaceView.Create('workspace view: '+Detail); end;

procedure RequireOptions(const O: TWfcPipelineWorkspaceSlice);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    Valid=O!==null && typeof O==='object' && !Array.isArray(O);
    if(Valid) {
      for(const k of ['Version','PassIndex','StartX','StartY','SliceZ','Width',
        'Height','CellPixels','MaxRenderedCells','MaxSvgBytes','UseBaseline']) {
        let p=O,d;
        while(p!==null && !(d=Object.getOwnPropertyDescriptor(p,k))) p=Object.getPrototypeOf(p);
        if(!d || !Object.prototype.hasOwnProperty.call(d,'value') ||
          (k==='UseBaseline' ? typeof d.value!=='boolean' :
          !Number.isInteger(d.value) || d.value<0 || d.value>2147483647)) { Valid=false;break; }
      }
    }
  end;
  if not Valid then ViewError('options require passive, exact scalar fields');
  {$ENDIF}
  if O.Version<>1 then ViewError('unsupported options version');
  if (O.PassIndex<0) or (O.StartX<0) or (O.StartY<0) or (O.SliceZ<0) then
    ViewError('pass and local coordinates must be nonnegative');
  if (O.Width<1) or (O.Height<1) or (O.CellPixels<1) or
    (O.MaxRenderedCells<1) or (O.MaxSvgBytes<1) then
    ViewError('window and presentation allowances must be positive');
  if O.Width>O.MaxRenderedCells div O.Height then ViewError('rendered cell allowance exceeded');
  if O.Width> (High(Integer)-16) div O.CellPixels then ViewError('SVG width exceeds Integer capacity');
  if O.Height> (High(Integer)-60) div O.CellPixels then ViewError('SVG height exceeds Integer capacity');
end;

function VectorText(const V: TWfcLatticeVector): String;
begin Result:=IntToStr(V.X)+','+IntToStr(V.Y)+','+IntToStr(V.Z); end;

function Digit(const V: Boolean): String;
begin if V then Result:='1' else Result:='0'; end;

function CanonicalToken(const T: TWfcModelToken): String;
begin Result:=WfcTextEncodeToken(T,'workspace SVG token'); end;

function PaletteColour(const T: TWfcModelToken; const Palette: TWfcModelTokens): String;
const Colours: array[0..7] of String = ('#64b6a2','#c1a36c','#929cc9','#ca8c9c',
  '#8cbb78','#7ca9c2','#b09ac4','#bbac8a');
var I: Integer;
begin
  for I:=0 to High(Palette) do if Palette[I]=T then Exit(Colours[I mod 8]);
  ViewError('captured token is outside the recipe vocabulary');
  Result:='';
end;

function PipelineWorkspaceSliceSvg(const Workbench: TWfcPipelineWorkspaceWorkbench;
  const Options: TWfcPipelineWorkspaceSlice): String;
var Recipe: TWfcPipelineModel; Run: TWfcPipelineRun;
  State: TWfcPipelineSessionPublicState; Layer: TWfcPipelineSessionLayer;
  Cell: TWfcPipelineSessionCell; Layout: TWfcLatticeLayout;
  Palette: TWfcModelTokens; Box: TWfcLatticeBox;
  Lines: TWfcTextLines; ByteCount,LineCount,CellCount,PassPosition: Integer;
  I,X,Y,CX,CY,Index,PixelWidth,PixelHeight: Integer;
  LabelText,Status,Description,Colour,Stroke,TokenText: String;
  procedure AddLine(const Value: String);
  begin
    if Length(Value)>Options.MaxSvgBytes-ByteCount then ViewError('SVG byte allowance exceeded');
    Inc(ByteCount,Length(Value));
    if ByteCount=Options.MaxSvgBytes then ViewError('SVG final LF exceeds byte allowance');
    Inc(ByteCount);
    Lines[LineCount]:=Value; Inc(LineCount);
  end;
begin
  RequireOptions(Options);
  if Workbench=nil then ViewError('workbench is required');
  if not Workbench.HasExecution then ViewError('no workspace execution');
  Recipe:=nil; Run:=nil; State:=nil;
  try
    Recipe:=Workbench.CopyCurrentRecipe; Run:=Workbench.CopyAppliedRun;
    if Options.PassIndex>=Recipe.PassCount then ViewError('pass index is outside the recipe');
    if Recipe.PassAt(Options.PassIndex).Visibility<>wppvPublic then ViewError('only public passes have display output');
    Layout:=Run.PassLayoutAt(Options.PassIndex);
    if (Options.StartX>=Layout.Cells.X) or (Options.Width>Layout.Cells.X-Options.StartX) or
      (Options.StartY>=Layout.Cells.Y) or (Options.Height>Layout.Cells.Y-Options.StartY) or
      (Options.SliceZ>=Layout.Cells.Z) then ViewError('requested local window is outside the pass');
    CellCount:=Options.Width*Options.Height;
    if CellCount>High(Integer)-5 then ViewError('SVG line inventory exceeds Integer capacity');
    if (Options.MaxSvgBytes<5) or (CellCount>Options.MaxSvgBytes-5) then
      ViewError('SVG byte allowance cannot hold the minimum line inventory');
    PixelWidth:=Options.Width*Options.CellPixels+16;
    if PixelWidth<320 then PixelWidth:=320;
    PixelHeight:=Options.Height*Options.CellPixels+60;
    if Options.UseBaseline then
    begin
      if not Workbench.HasSuccessfulBaseline then ViewError('no retained successful baseline');
      State:=Workbench.CopyLastSuccessfulState;
      Status:='historical-baseline';
      Description:='HISTORICAL BASELINE - not a claim about current inputs';
    end
    else
    begin
      State:=Workbench.CopyPublicState;
      if Workbench.HasCurrentOutput then
      begin Status:='current'; Description:='CURRENT - satisfies the executed recipe, not an independent physical policy'; end
      else begin Status:='not-current'; Description:='NOT CURRENT - pending or unsuccessful workspace output'; end;
    end;
    if State=nil then ViewError('requested public state is absent');
    PassPosition:=-1;
    for I:=0 to State.LayerCount-1 do
    begin
      Layer:=State.LayerAt(I);
      if Layer.PassIndex=Options.PassIndex then begin PassPosition:=I; Break; end;
    end;
    if PassPosition<0 then ViewError('requested public layer is absent');
    if not SameWfcLatticeLayout(Layout,Layer.Layout) then ViewError('state layout differs from applied invocation');
    if Length(Layer.Cells)<>WfcLatticeCellCount(Layout) then ViewError('public layer has incomplete cell inventory');
    Palette:=Recipe.CopyPublicVocabulary(Options.PassIndex);
    LabelText:=CanonicalToken(Layer.LabelName);
    SetLength(Lines,CellCount+5); ByteCount:=0; LineCount:=0;
    AddLine('<svg xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Workspace local-grid diagnostic" viewBox="0 0 '+
      IntToStr(PixelWidth)+' '+IntToStr(PixelHeight)+'" data-workspace-view="1" data-status="'+Status+
      '" data-publication-revision="'+IntToStr(Workbench.PublicationRevision)+'" data-session-revision="'+
      IntToStr(Workbench.SessionRevision)+'" data-pass="'+IntToStr(Options.PassIndex)+'" data-rank="'+
      IntToStr(Layer.Rank)+'" data-local-window="'+IntToStr(Options.StartX)+','+IntToStr(Options.StartY)+','+
      IntToStr(Options.SliceZ)+','+IntToStr(Options.Width)+','+IntToStr(Options.Height)+
      '" data-cells="'+VectorText(Layout.Cells)+'" data-origin="'+VectorText(Layout.Origin)+
      '" data-pitch="'+VectorText(Layout.Pitch)+'" data-wrap="'+Digit(Layout.Wrap)+'">');
    AddLine('<title>'+LabelText+' - '+Description+'</title>');
    AddLine('<desc>Local XY grid at Z='+IntToStr(Options.SliceZ)+
      '. This is a window, not a resized composition. World footprints are half-open. Tokens and labels are canonical percent text.</desc>');
    AddLine('<rect width="100%" height="100%" fill="#111b21"/><text x="8" y="18" fill="#e3e9e5" font-family="monospace" font-size="12">'+
      'pass '+IntToStr(Options.PassIndex)+' / '+Status+'</text><text x="8" y="36" fill="#adbdb7" font-family="monospace" font-size="10">local Z='+
      IntToStr(Options.SliceZ)+'; world footprint in each cell tooltip</text>');
    for Y:=0 to Options.Height-1 do for X:=0 to Options.Width-1 do
    begin
      CX:=Options.StartX+X; CY:=Options.StartY+Y;
      Index:=(Options.SliceZ*Layout.Cells.Y+CY)*Layout.Cells.X+CX;
      Cell:=Layer.Cells[Index]; TokenText:=CanonicalToken(Cell.Token);
      if Cell.Empty then Colour:='#29343c' else Colour:=PaletteColour(Cell.Token,Palette);
      if not Cell.Empty and not Cell.Generated then Stroke:='#f4ca74' else Stroke:='#111b21';
      Box:=WfcLatticeCellBox(Layout,MakeWfcLatticeVector(CX,CY,Options.SliceZ));
      AddLine('<g data-cell-index="'+IntToStr(Index)+'" data-cell="'+IntToStr(CX)+','+IntToStr(CY)+','+
        IntToStr(Options.SliceZ)+'" data-token="'+TokenText+'" data-empty="'+Digit(Cell.Empty)+
        '" data-generated="'+Digit(Cell.Generated)+'" data-world-min="'+VectorText(Box.Minimum)+
        '" data-world-max="'+VectorText(Box.Maximum)+'"><title>cell '+IntToStr(CX)+','+IntToStr(CY)+','+
        IntToStr(Options.SliceZ)+'; token '+TokenText+'; empty '+Digit(Cell.Empty)+'; generated '+Digit(Cell.Generated)+
        '; world minimum '+VectorText(Box.Minimum)+'; exclusive maximum '+VectorText(Box.Maximum)+'</title><rect x="'+
        IntToStr(8+X*Options.CellPixels)+'" y="'+IntToStr(48+Y*Options.CellPixels)+'" width="'+
        IntToStr(Options.CellPixels)+'" height="'+IntToStr(Options.CellPixels)+'" fill="'+Colour+'" stroke="'+Stroke+'"/></g>');
    end;
    AddLine('</svg>');
    if LineCount<>Length(Lines) then ViewError('internal line inventory mismatch');
    Result:=WfcTextJoinCanonicalLines(Lines,'workspace SVG');
    if Length(Result)<>ByteCount then ViewError('internal SVG byte inventory mismatch');
  finally State.Free; Run.Free; Recipe.Free; end;
end;
end.
