{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_pipeline_mapped_run_test;
{$mode delphi}{$H+}
uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_rule_model, wfc_rule_text, wfc_sequence,
  wfc_lattice, wfc_pipeline_layout, wfc_pipeline_model,
  wfc_pipeline_run, wfc_pipeline_run_text;

var Checks: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;

function Tokens: TWfcModelTokens;
begin
  SetLength(Result,2); Result[0]:='A'; Result[1]:='B';
end;

function Recipe(const ASpatial: Boolean; const ATransform: Boolean=False): TWfcPipelineModel;
var Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Topologies: TWfcPipelinePassTopologies; Dependencies: TWfcPipelineDependencies;
  Rules: TWfcRuleModel; Weights: TWfcModelIntegerArray;
  Metadata: TWfcPipelineMetadata; I: Integer;
begin
  SetLength(Weights,2); Weights[0]:=1; Weights[1]:=1;
  Rules:=TWfcRuleModel.Create(2,Tokens,Weights,nil);
  try
    SetLength(Resources,1);
    Resources[0]:=MakeWfcPipelineResource('rules',wprkRules,
      EncodeWfcRuleText(Rules),'mapped run fixture','MIT','');
  finally Rules.Free; end;
  SetLength(Passes,3); SetLength(Topologies,3);
  for I:=0 to 2 do
  begin
    Passes[I]:=MakeWfcPipelinePass('pass-'+IntToStr(I),wppvPublic,
      gpmOverlay,WFC_PIPELINE_NO_INDEX,wpakRules,0,False,wseWhole);
    Topologies[I]:=MakeWfcPipelinePassTopology(2,
      MakeWfcLatticeVector(-10+I,4-I,0),MakeWfcLatticeVector(I+1,2,1),I=1);
  end;
  SetLength(Dependencies,1);
  Dependencies[0]:=MakeWfcPipelineDependency(2,1);
  if ATransform then
  begin
    Passes[2]:=MakeWfcPipelinePass('pass-2',wppvPublic,gpmTransform,1,
      wpakEmpty,WFC_PIPELINE_NO_INDEX,False,wseWhole);
    Topologies[2]:=Topologies[1];
  end;
  Metadata:=MakeWfcPipelineMetadata('mapped run','MIT','','');
  if ASpatial then
    Result:=TWfcPipelineModel.Create(Metadata,CurrentWfcPipelineVersions,
      2,False,rmBottomUp,Resources,Passes,Dependencies,nil,nil,nil,nil,
      WFC_PIPELINE_PASS_MAPPING_VERSION,Topologies)
  else
    Result:=TWfcPipelineModel.Create(Metadata,2,False,rmBottomUp,
      Resources,Passes,Dependencies,nil,nil);
end;

function Extents: TWfcPipelinePassExtents;
begin
  SetLength(Result,3);
  Result[0]:=MakeWfcLatticeVector(2,1,1);
  Result[1]:=MakeWfcLatticeVector(5,1,1);
  Result[2]:=MakeWfcLatticeVector(3,1,1);
end;

function NewRun(const ARecipe: TWfcPipelineModel;
  const AExtents: TWfcPipelinePassExtents;
  const ALocks: TWfcPipelineCellLocks=nil;
  const ADomains: TWfcPipelineCellDomains=nil): TWfcPipelineRun;
begin
  Result:=TWfcPipelineRun.Create(ARecipe,AExtents,High(Cardinal),
    wpssOneWay,10,0,False,ALocks,ADomains);
end;

procedure RejectRun(const ARecipe: TWfcPipelineModel;
  const AExtents: TWfcPipelinePassExtents;
  const ALocks: TWfcPipelineCellLocks; const ADomains: TWfcPipelineCellDomains);
var Value: TWfcPipelineRun; Rejected: Boolean;
begin
  Value:=nil; Rejected:=False;
  try
    try Value:=NewRun(ARecipe,AExtents,ALocks,ADomains);
    except on E: Exception do Rejected:=True; end;
  finally Value.Free; end;
  Check(Rejected,'invalid mapped run must reject');
end;

procedure RejectText(const ARecipe: TWfcPipelineModel; const AText: String);
var Value: TWfcPipelineRun; Rejected: Boolean;
begin
  Value:=nil; Rejected:=False;
  try
    try Value:=DecodeWfcPipelineRunText(AText,ARecipe);
    except on E: EConvertError do Rejected:=True; end;
  finally Value.Free; end;
  Check(Rejected,'malformed mapped run text must reject');
end;

function Change(const AText,AOld,ANew: String): String;
begin
  Check(Pos(AOld,AText)>0,'mutation anchor exists: '+AOld);
  Result:=StringReplace(AText,AOld,ANew,[]);
end;

procedure TestRuns;
var Model, Spatial, CopyModel: TWfcPipelineModel;
  Value, Again, Legacy, ExplicitUniform: TWfcPipelineRun;
  Cells, Detached: TWfcPipelinePassExtents; Layouts: TWfcLatticeLayouts;
  Locks: TWfcPipelineCellLocks; Domains: TWfcPipelineCellDomains;
  Text, OwnedText: String; BeforeSignature: Cardinal; I: Integer;
begin
  Model:=Recipe(False); Spatial:=Recipe(True); CopyModel:=Recipe(False,True);
  Value:=nil; Again:=nil; Legacy:=nil; ExplicitUniform:=nil;
  try
    Cells:=Extents;
    SetLength(Locks,2);
    Locks[0]:=MakeWfcPipelineCellLock(1,4,0,0,'A');
    Locks[1]:=MakeWfcPipelineCellLock(2,2,0,0,'B');
    SetLength(Domains,1);
    Domains[0]:=MakeWfcPipelineCellDomain(1,4,0,0,Tokens);
    Value:=NewRun(Model,Cells,Locks,Domains);
    Check(Value.FormatVersion=2,'explicit extents use run2 on legacy recipes');
    Check((Value.PassCount=3) and (Value.TotalCellCount=10),'actual aggregate pass cells');
    Check((Value.PassOffsetAt(0)=0) and (Value.PassOffsetAt(1)=2) and
      (Value.PassOffsetAt(2)=7),'checked prefixes include every pass');
    Check(Value.PassOffsetAt(1)+4<>Value.PassOffsetAt(2)+2,'late-pass local cells do not alias');
    Check((Value.PassCellCount(1)=5) and (Value.PassCellCount(2)=3),'pass-local extents');
    Check((Value.Width=2) and (Value.Height=1) and (Value.Depth=1),'legacy view is pass zero only');
    Check(Value.LockAt(0).X=4,'late-pass lock beyond root extent remains valid');
    Text:=EncodeWfcPipelineRunText(Value);
    Check(Pos('layout-version=1'+#10+'mapping-version=1'+#10+'extents=3'+#10,Text)>0,'capability pins and complete extent table');
    Again:=DecodeWfcPipelineRunText(Text,Model);
    Check((Again.Signature=Value.Signature) and (EncodeWfcPipelineRunText(Again)=Text),'run2 canonical byte round trip');
    FreeAndNil(Again);
    Cells[1].X:=1; Locks[0].X:=0; Domains[0].AllowedTokens[0]:='changed';
    Detached:=Value.CopyPassExtents; Detached[2].X:=99;
    Layouts:=Value.CopyPassLayouts; Layouts[1].Origin.X:=999;
    Check((Value.PassCellCount(1)=5) and (Value.PassCellCount(2)=3) and
      (Value.DomainAt(0).AllowedTokens[0]='A'),'run owns detached caller arrays');
    Check(Value.PassLayoutAt(1).Origin.X=0,'returned layouts detached');
    BeforeSignature:=Value.Signature;
    Check(EncodeWfcPipelineRunText(Value)=Text,'copy mutation cannot change signed text');
    FreeAndNil(Value);
    Cells:=Extents;
    for I:=0 to High(Cells) do Cells[I]:=MakeWfcLatticeVector(2,1,1);
    Legacy:=TWfcPipelineRun.Create(Model,2,1,1,High(Cardinal),wpssOneWay,10,0,False,nil,nil);
    ExplicitUniform:=NewRun(Model,Cells);
    Check((Legacy.FormatVersion=1) and (ExplicitUniform.FormatVersion=2),'constructor controls explicit capability; no compact downcast');
    Check(Legacy.Signature<>ExplicitUniform.Signature,'format capability binds run identity');
    Check(Pos('wfcpipeline-run=1'+#10,EncodeWfcPipelineRunText(Legacy))=1,'legacy constructor stays byte-format1');
    FreeAndNil(Legacy);
    Legacy:=TWfcPipelineRun.Create(Spatial,2,1,1,High(Cardinal),wpssOneWay,10,0,False,nil,nil);
    Check(Legacy.FormatVersion=2,'numeric constructor on spatial recipe uses run2');
    Check((Legacy.PassTopologyAt(1).Rank=2) and Legacy.PassLayoutAt(1).Wrap,
      'run carries independent topology rank and wrapping');
    Cells:=Extents;
    Value:=NewRun(Spatial,Cells);
    OwnedText:=EncodeWfcPipelineRunText(Value);
    Again:=DecodeWfcPipelineRunText(OwnedText,Spatial);
    Check(EncodeWfcPipelineRunText(Again)=OwnedText,'nonunit signed-origin spatial round trip');
    FreeAndNil(Again);
    Check(Value.PassLayoutAt(2).Pitch.X=3,'late pass nonunit pitch retained');
    FreeAndNil(Spatial);
    Check(EncodeWfcPipelineRunText(Value)=OwnedText,'recipe may be freed before run serialization');
    FreeAndNil(Value);
    Cells:=Extents; Cells[1]:=MakeWfcLatticeVector(2,3,1);
    Value:=NewRun(Model,Cells); BeforeSignature:=Value.Signature; FreeAndNil(Value);
    Cells[1]:=MakeWfcLatticeVector(3,2,1); Value:=NewRun(Model,Cells);
    Check(Value.Signature<>BeforeSignature,'same-count different shape changes run signature');
    FreeAndNil(Value);

    Cells:=Extents; RejectRun(CopyModel,Cells,nil,nil);
    Cells[2]:=Cells[1]; Value:=NewRun(CopyModel,Cells);
    Check(Value.TotalCellCount=12,'matching transform alias owns its own prefix slots');
    FreeAndNil(Value);
    Cells:=Extents;
    SetLength(Locks,1); Locks[0]:=MakeWfcPipelineCellLock(1,5,0,0,'A');
    RejectRun(Model,Cells,Locks,nil);
    Locks[0]:=MakeWfcPipelineCellLock(2,3,0,0,'A'); RejectRun(Model,Cells,Locks,nil);
    SetLength(Domains,1); Domains[0]:=MakeWfcPipelineCellDomain(1,5,0,0,Tokens);
    RejectRun(Model,Cells,nil,Domains);
    Cells[2].X:=0; RejectRun(Model,Cells,nil,nil);
    Cells:=Extents; Cells[2].X:=WFC_PIPELINE_RUN_MAX_DIMENSION+1; RejectRun(Model,Cells,nil,nil);
    Cells:=Extents; Cells[1]:=MakeWfcLatticeVector(2049,2048,1); RejectRun(Model,Cells,nil,nil);
    Cells:=Extents; Cells[1].Z:=2; RejectRun(Model,Cells,nil,nil);
    SetLength(Cells,2); RejectRun(Model,Cells,nil,nil);
    Cells:=Extents; SetLength(Cells,4); Cells[3]:=MakeWfcLatticeVector(1,1,1); RejectRun(Model,Cells,nil,nil);

    RejectText(Model,Change(Text,'layout-version=1','layout-version=2'));
    RejectText(Model,Change(Text,'mapping-version=1','mapping-version=0'));
    RejectText(Model,Change(Text,'extents=3','extents=2'));
    RejectText(Model,Change(Text,'extents=3','extents=257'));
    RejectText(Model,Change(Text,'extent=1,5,1,1','extent=0,5,1,1'));
    RejectText(Model,Change(Text,'extent=1,5,1,1'+#10,''));
    RejectText(Model,Change(Text,'extent=1,5,1,1','extent=1,05,1,1'));
    RejectText(Model,Change(Text,'extent=1,5,1,1','extent=1,-0,1,1'));
    RejectText(Model,Change(Text,'extent=1,5,1,1','extent=1,5.0,1,1'));
    RejectText(Model,Change(Text,'extent=1,5,1,1','extent=1,2147483648,1,1'));
    RejectText(Model,Change(Text,'extent=1,5,1,1','extent=1,4194305,1,1'));
    RejectText(Model,Change(Text,'extent=1,5,1,1','extent=1,5,1,1,0'));
    RejectText(Model,Change(Text,'width=2','width=3'));
    RejectText(Model,Change(Text,'extent=2,3,1,1','extent=2,4,1,1'));
    RejectText(Model,Change(Text,'wfcpipeline-run=2','wfcpipeline-run=1'));
  finally
    Value.Free; Again.Free; Legacy.Free; ExplicitUniform.Free;
    Model.Free; Spatial.Free; CopyModel.Free;
  end;
end;

procedure TestMixedRanks;
var Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Topologies: TWfcPipelinePassTopologies; Cells: TWfcPipelinePassExtents;
  Rules: TWfcRuleModel; Weights: TWfcModelIntegerArray;
  Model: TWfcPipelineModel; Value,Again: TWfcPipelineRun;
  I: Integer; Text: String;
begin
  SetLength(Resources,3); SetLength(Passes,3); SetLength(Topologies,3);
  SetLength(Cells,3); SetLength(Weights,2); Weights[0]:=1; Weights[1]:=1;
  for I:=0 to 2 do
  begin
    Rules:=TWfcRuleModel.Create(I+1,Tokens,Weights,nil);
    try Resources[I]:=MakeWfcPipelineResource('rules-'+IntToStr(I),wprkRules,
      EncodeWfcRuleText(Rules),'rank fixture','MIT','');
    finally Rules.Free; end;
    Passes[I]:=MakeWfcPipelinePass('rank-'+IntToStr(I+1),wppvPublic,
      gpmOverlay,WFC_PIPELINE_NO_INDEX,wpakRules,I,False,wseWhole);
    Topologies[I]:=MakeWfcPipelinePassTopology(I+1,MakeWfcLatticeVector(-5,7,-9),
      MakeWfcLatticeVector(2,3,4),I=2);
  end;
  Cells[0]:=MakeWfcLatticeVector(3,1,1); Cells[1]:=MakeWfcLatticeVector(2,2,1);
  Cells[2]:=MakeWfcLatticeVector(1,2,3);
  Model:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('mixed ranks','MIT','',''),
    CurrentWfcPipelineVersions,1,False,rmBottomUp,Resources,Passes,nil,nil,nil,nil,nil,
    WFC_PIPELINE_PASS_MAPPING_VERSION,Topologies);
  Value:=nil; Again:=nil;
  try
    Value:=NewRun(Model,Cells);
    Check(Value.TotalCellCount=13,'mixed ranks count actual local extents');
    for I:=0 to 2 do Check(Value.PassTopologyAt(I).Rank=I+1,'each run pass retains its own rank');
    Check(Value.PassLayoutAt(0).Pitch.Z=4,'inactive local axes retain physical thickness');
    Text:=EncodeWfcPipelineRunText(Value); Again:=DecodeWfcPipelineRunText(Text,Model);
    Check(EncodeWfcPipelineRunText(Again)=Text,'mixed-rank exact codec round trip');
    Cells[0].Y:=2; RejectRun(Model,Cells,nil,nil);
    Cells[0].Y:=1; Cells[1].Z:=2; RejectRun(Model,Cells,nil,nil);
  finally Value.Free; Again.Free; Model.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestHostileTypedInputs;
var Model: TWfcPipelineModel; Cells: TWfcPipelinePassExtents;
  Locks: TWfcPipelineCellLocks; Value: TWfcPipelineRun;
  Seed: TGraphSeed; Budget: Integer; Trace: Boolean; Rejected: Boolean;
begin
  Model:=Recipe(False); Value:=nil;
  try
    Cells:=Extents; asm Cells[1].X = NaN; end; RejectRun(Model,Cells,nil,nil);
    Cells:=Extents; asm Cells[1].X = Infinity; end; RejectRun(Model,Cells,nil,nil);
    Cells:=Extents; asm Cells[1].X = '5'; end; RejectRun(Model,Cells,nil,nil);
    Cells:=Extents; asm Cells[1].X = 1.5; end; RejectRun(Model,Cells,nil,nil);
    Cells:=Extents; asm delete Cells[1]; end; RejectRun(Model,Cells,nil,nil);
    Cells:=Extents; asm Cells[1] = null; end; RejectRun(Model,Cells,nil,nil);
    Cells:=Extents; asm Object.defineProperty(Cells[1], 'X', {get: function(){throw new Error('getter invoked');}}); end;
    RejectRun(Model,Cells,nil,nil);
    Cells:=Extents; SetLength(Locks,1); Locks[0]:=MakeWfcPipelineCellLock(1,1,0,0,'A');
    asm Locks[0].X = 0.5; end; RejectRun(Model,Cells,Locks,nil);
    asm Locks[0].X = NaN; end; RejectRun(Model,Cells,Locks,nil);
    Seed:=3; asm Seed=NaN; end; Rejected:=False;
    try Value:=TWfcPipelineRun.Create(Model,Cells,Seed,wpssOneWay,10,0,False,nil,nil);
    except on E: Exception do Rejected:=True; end;
    FreeAndNil(Value); Check(Rejected,'typed NaN seed rejects');
    Budget:=10; asm Budget='10'; end; Rejected:=False;
    try Value:=TWfcPipelineRun.Create(Model,Cells,3,wpssOneWay,Budget,0,False,nil,nil);
    except on E: Exception do Rejected:=True; end;
    FreeAndNil(Value); Check(Rejected,'typed numeric-string budget rejects');
    Trace:=False; asm Trace=0; end; Rejected:=False;
    try Value:=TWfcPipelineRun.Create(Model,Cells,3,wpssOneWay,10,0,Trace,nil,nil);
    except on E: Exception do Rejected:=True; end;
    FreeAndNil(Value); Check(Rejected,'typed non-Boolean trace rejects');
  finally Value.Free; Model.Free; end;
end;
{$ENDIF}

begin
  try
    TestRuns;
    TestMixedRanks;
    {$IFDEF PAS2JS}TestHostileTypedInputs;{$ENDIF}
    WriteLn('Mapped pipeline run checks: ',Checks,'/',Checks);
  except
    on E: Exception do begin WriteLn('FAIL: ',E.Message); Halt(1); end;
  end;
end.
