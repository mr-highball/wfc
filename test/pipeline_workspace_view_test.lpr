{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Independently authored slice-view conformance. }
program pipeline_workspace_view_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,Web,{$ENDIF}
  SysUtils,wfc,wfc_model,wfc_rule_model,wfc_rule_text,wfc_sequence,wfc_lattice,
  wfc_pipeline_layout,wfc_pipeline_model,wfc_pipeline_run,wfc_pipeline_text,
  wfc_pipeline_run_text,wfc_pipeline_workspace_replay,wfc_text_codec,
  wfc_workspace_replay_fixture,pipeline_workspace_workbench,pipeline_workspace_view;
var Checks,Documents: Integer;
procedure Verify(const OK: Boolean; const Detail: String);
begin Inc(Checks); if not OK then raise Exception.Create('workspace view test: '+Detail); end;

function Policy: TWfcPipelineWorkspacePolicy;
begin
  Result.Version:=1; Result.Journal:=JournalLimits; Result.Replacement:=ReplacementLimits;
  Result.Outcome:=OutcomeLimits; Result.Evidence:=EvidenceLimits;
  Result.Replay.Version:=1; Result.Replay.MaxEpochs:=8; Result.Replay.MaxSolveActions:=32;
  Result.Replay.MaxInstantiatedCellRecords:=1024; Result.Replay.MaxEvidenceTextBytes:=8000000;
end;

function Window: TWfcPipelineWorkspaceSlice;
begin
  Result.Version:=1; Result.PassIndex:=0; Result.StartX:=0; Result.StartY:=0;
  Result.SliceZ:=1; Result.Width:=2; Result.Height:=2; Result.CellPixels:=36;
  Result.MaxRenderedCells:=4; Result.MaxSvgBytes:=1048576; Result.UseBaseline:=False;
end;

function Occurrences(const Needle,Haystack: String): Integer;
var I: Integer;
begin
  Result:=0;
  for I:=1 to Length(Haystack)-Length(Needle)+1 do
    if Copy(Haystack,I,Length(Needle))=Needle then Inc(Result);
end;

procedure ExportSvg(const Value: String);
{$IFDEF PAS2JS}var N: TJSElement;{$ENDIF}
begin
  {$IFDEF WORKSPACE_VIEW_EXPORT}
  Inc(Documents);
  {$IFDEF PAS2JS}
  N:=document.createElement('pre'); N.id:='workspace-svg-parity-'+IntToStr(Documents);
  N.setAttribute('class','complete-workspace-svg'); N.textContent:=Value;
  document.body.appendChild(N);
  {$ELSE}
  WriteLn('workspace-svg-parity-',Documents,'=',WfcTextEncodeToken(TWfcModelToken(Value),'SVG parity'));
  {$ENDIF}
  {$ENDIF}
end;

function MakeWorkbench: TWfcPipelineWorkspaceWorkbench;
var Tokens: TWfcModelTokens; Weights: TWfcModelIntegerArray; Rules: TWfcRuleModel;
  Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Topologies: TWfcPipelinePassTopologies; Extents: TWfcPipelinePassExtents;
  Recipe: TWfcPipelineModel; Run: TWfcPipelineRun; Locks: TWfcPipelineCellLocks;
  Receipt: TWfcPipelineWorkspaceReceipt;
begin
  Result:=nil; Rules:=nil; Recipe:=nil; Run:=nil; Receipt:=nil;
  try
    try
    SetLength(Tokens,2); Tokens[0]:='<script>&"'#10;
    { Executed graph tokens must round-trip through the host graph string.
      Keep this token ASCII on native and browser; its percent spelling is
      literal data, not a decoded supplementary Unicode graph value. }
    Tokens[1]:='leaf-%F0%9F%8D%83';
    SetLength(Weights,2); Weights[0]:=1; Weights[1]:=1;
    Rules:=TWfcRuleModel.Create(3,Tokens,Weights,nil);
    SetLength(Resources,1);
    Resources[0]:=MakeWfcPipelineResource('display-rules',wprkRules,EncodeWfcRuleText(Rules),
      'project-authored XML and literal percent tokens','MIT','slice-view-fixture');
    SetLength(Passes,2);
    Passes[0]:=MakeWfcPipelinePass('<layer&>',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
    Passes[1]:=MakeWfcPipelinePass('private',wppvPrivate,gpmOverlay,-1,wpakRules,0,False,wseWhole);
    SetLength(Topologies,2); SetLength(Extents,2);
    Topologies[0]:=MakeWfcPipelinePassTopology(3,MakeWfcLatticeVector(-10,5,-3),MakeWfcLatticeVector(2,3,4),False);
    Topologies[1]:=LegacyWfcPipelinePassTopology(3,False);
    Extents[0]:=MakeWfcLatticeVector(2,2,2); Extents[1]:=MakeWfcLatticeVector(1,1,1);
    Recipe:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('view','MIT','literal','view-v1'),
      CurrentWfcPipelineVersions,3,False,rmBottomUp,Resources,Passes,nil,nil,nil,nil,nil,
      WFC_PIPELINE_PASS_MAPPING_VERSION,Topologies);
    SetLength(Locks,2);
    Locks[0]:=MakeWfcPipelineCellLock(0,0,0,1,Tokens[1]);
    Locks[1]:=MakeWfcPipelineCellLock(0,1,1,1,Tokens[0]);
    Run:=TWfcPipelineRun.Create(Recipe,Extents,11,wpssOneWay,64,0,False,Locks,nil);
    Result:=TWfcPipelineWorkspaceWorkbench.Create;
    Receipt:=Result.BeginEpoch(EncodeWfcPipelineModelText(Recipe),EncodeWfcPipelineRunText(Run),Policy,0);
    except FreeAndNil(Result); raise;
    end;
  finally Receipt.Free; Run.Free; Recipe.Free; Rules.Free;
  end;
end;

procedure Refuses(const W: TWfcPipelineWorkspaceWorkbench; const O: TWfcPipelineWorkspaceSlice);
var Saved,Ignored: String; Revision: Integer; Rejected,HadExecution: Boolean;
begin
  HadExecution:=W.HasExecution; Saved:='';
  if HadExecution then Saved:=W.CopyCanonicalJournal;
  Revision:=W.PublicationRevision; Rejected:=False;
  try Ignored:=PipelineWorkspaceSliceSvg(W,O);
  except on E: EWfcPipelineWorkspaceView do Rejected:=True; end;
  Verify(Rejected,'malformed or excessive visual request refuses');
  Verify((W.HasExecution=HadExecution) and (W.PublicationRevision=Revision),
    'refused presentation never changes live owner/revision');
  if HadExecution then Verify(W.CopyCanonicalJournal=Saved,
    'refused presentation never changes complete live history');
end;

{$IFDEF PAS2JS}
procedure RawOptions(const W: TWfcPipelineWorkspaceWorkbench; const CaseIndex: Integer);
var O: TWfcPipelineWorkspaceSlice; Touches: Integer;
begin
  O:=Window; Touches:=0;
  asm
    switch(CaseIndex) {
      case 0:Object.defineProperty(O,'Version',{get:function(){Touches++;return 1;}});break;
      case 1:Object.defineProperty(O,'Width',{get:function(){Touches++;return 2;}});break;
      case 2:Object.defineProperty(O,'UseBaseline',{get:function(){Touches++;return false;}});break;
      case 3:O.Width=NaN;break;
      case 4:O.StartX=0.5;break;
      case 5:O.UseBaseline=0;break;
      case 6:O=null;break;
      case 7:O=[];break;
    }
  end;
  Refuses(W,O); Verify(Touches=0,'raw option accessors never executed');
end;

procedure FrozenOptions(const W: TWfcPipelineWorkspaceWorkbench; const Expected: String);
var O: TWfcPipelineWorkspaceSlice;
begin
  O:=Window;
  asm Object.freeze(O); end;
  Verify(PipelineWorkspaceSliceSvg(W,O)=Expected,'passive frozen options remain supported');
  { O is never reused as a record assignment destination after freezing. }
end;
{$ENDIF}

procedure TestView;
var W: TWfcPipelineWorkspaceWorkbench; O: TWfcPipelineWorkspaceSlice;
  P: TWfcPipelineWorkspacePolicy; Receipt: TWfcPipelineWorkspaceReceipt;
  Run: TWfcPipelineRun; Roots: TGraphPassIndices;
  Svg,Other,Saved: String; I: Integer;
begin
  W:=nil; Receipt:=nil; Run:=nil;
  try
    Verify(WfcTextEncodeToken(WfcTextDecodeToken('leaf-%F0%9F%8D%83','view fixture'),
      'view fixture')='leaf-%F0%9F%8D%83',
      'Unicode codec round-trip is independent of host graph token representability');
    P:=Policy; W:=TWfcPipelineWorkspaceWorkbench.Create; O:=Window;
    Refuses(W,O); FreeAndNil(W); W:=MakeWorkbench;
    Svg:=PipelineWorkspaceSliceSvg(W,O);
    Verify(Pos('data-status="not-current"',Svg)>0,'initial ungenerated state is never current');
    Verify(Pos('data-empty="1"',Svg)>0,'unset cells remain visibly empty');
    O.UseBaseline:=True; Refuses(W,O); O:=Window;
    Receipt:=W.ExecuteInitial(P,W.PublicationRevision);
    Verify(Receipt.HasCurrentOutput,'actual 3D fixture solved'); FreeAndNil(Receipt);
    Saved:=W.CopyCanonicalJournal;
    Svg:=PipelineWorkspaceSliceSvg(W,O); ExportSvg(Svg);
    Verify(Pos('data-status="current"',Svg)>0,'actual currentness is labelled');
    Verify(Pos('data-rank="3"',Svg)>0,'rank3 slice is explicit');
    Verify(Pos('data-cells="2,2,2"',Svg)>0,'full composition extent is not the slice extent');
    Verify(Pos('data-origin="-10,5,-3" data-pitch="2,3,4"',Svg)>0,'world geometry retained');
    Verify(Pos('data-cell-index="7" data-cell="1,1,1"',Svg)>0,'X-fast XYZ index independently matches');
    Verify(Pos('data-world-min="-8,8,1" data-world-max="-6,11,5"',Svg)>0,'exact half-open world footprint');
    Verify(Occurrences('<g data-cell-index=',Svg)=4,'exactly requested four cells, no hidden truncation');
    Verify(Pos('data-generated="0"',Svg)>0,'caller locks remain identified');
    Verify(Pos('data-generated="1"',Svg)>0,'solver-owned cells remain identified');
    Verify((Pos('<script>',Svg)=0) and (Pos('<layer&>',Svg)=0),'source tokens and labels never become markup');
    Verify(Pos('%3Cscript%3E%26%22%0A',Svg)>0,'complete special/control token remains encoded');
    Verify(Pos('leaf-%25F0%259F%258D%2583',Svg)>0,
      'literal percent token spelling is encoded as data, never decoded as Unicode');
    for I:=1 to Length(Svg) do Verify((Svg[I]=#10) or ((Ord(Svg[I])>=32) and (Ord(Svg[I])<=126)),
      'complete SVG is canonical ASCII plus LF');
    O.MaxSvgBytes:=Length(Svg); Verify(PipelineWorkspaceSliceSvg(W,O)=Svg,'exact SVG byte limit succeeds');
    Dec(O.MaxSvgBytes); Refuses(W,O); O:=Window;
    O.MaxRenderedCells:=3; Refuses(W,O); O:=Window;
    O.StartX:=1; Refuses(W,O); O:=Window;
    O.SliceZ:=2; Refuses(W,O); O:=Window;
    O.PassIndex:=1; Refuses(W,O); O:=Window;
    O.PassIndex:=2; Refuses(W,O); O:=Window;
    O.CellPixels:=High(Integer); Refuses(W,O); O:=Window;
    O.MaxSvgBytes:=4; Refuses(W,O); O:=Window;
    O.Width:=1; O.Height:=1; O.StartX:=1; O.StartY:=1;
    Other:=PipelineWorkspaceSliceSvg(W,O);
    Verify((Occurrences('<g data-cell-index=',Other)=1) and (Pos('data-cell-index="7"',Other)>0),
      'window selection changes only displayed inventory');
    Verify(W.CopyCanonicalJournal=Saved,'all read-only views leave complete saved history unchanged');
    {$IFDEF PAS2JS}
    for I:=0 to 7 do RawOptions(W,I);
    FrozenOptions(W,Svg);
    {$ENDIF}
    Receipt:=W.SetCellDomain(0,1,0,1,nil,P,W.PublicationRevision); FreeAndNil(Receipt);
    O:=Window; Other:=PipelineWorkspaceSliceSvg(W,O); ExportSvg(Other);
    Verify(Pos('data-status="not-current"',Other)>0,'dirty state cannot retain current label');
    O.UseBaseline:=True; Other:=PipelineWorkspaceSliceSvg(W,O); ExportSvg(Other);
    Verify(Pos('data-status="historical-baseline"',Other)>0,'retained baseline remains explicitly historical');
    Verify(Pos('not a claim about current inputs',Other)>0,'baseline has explicit current-input warning');
    Run:=W.CopyAppliedRun; SetLength(Roots,1); Roots[0]:=0;
    Receipt:=W.ExecuteRepair(EncodeWfcPipelineRunText(Run),Roots,P,W.PublicationRevision);
    Verify(not Receipt.HasCurrentOutput and Receipt.HasSuccessfulBaseline,'real unsuccessful repair retained truthful baseline');
    FreeAndNil(Receipt); O:=Window; Other:=PipelineWorkspaceSliceSvg(W,O);
    Verify(Pos('data-status="not-current"',Other)>0,'failed repair display is not labelled current');
  finally Run.Free; Receipt.Free; W.Free; end;
end;
begin
  TestView;
  WriteLn('Workspace slice view checks: ',Checks);
  if Documents>0 then WriteLn('Complete workspace SVG documents: ',Documents);
end.
