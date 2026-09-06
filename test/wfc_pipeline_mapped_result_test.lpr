{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_pipeline_mapped_result_test;
{$mode delphi}{$H+}
uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,wfc,wfc_model,wfc_rule_model,wfc_rule_text,wfc_sequence,
  wfc_lattice,wfc_pipeline_layout,wfc_pipeline_model,wfc_pipeline_run,
  wfc_pipeline_result,wfc_pipeline_result_text;

var Checks: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin Inc(Checks); if not ACondition then raise Exception.Create(AMessage); end;

function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result:=nil; SetLength(Result,Length(Values));
  for I:=0 to High(Values) do Result[I]:=Values[I];
end;

function Recipe: TWfcPipelineModel;
var Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Topologies: TWfcPipelinePassTopologies; Dependencies: TWfcPipelineDependencies;
  Requirements: TWfcPipelineRequirements; Query: TWfcPipelineMappedQuery;
  Rules: TWfcRuleModel; Weights: TWfcModelIntegerArray; I: Integer;
begin
  SetLength(Weights,2); Weights[0]:=1; Weights[1]:=1;
  Rules:=TWfcRuleModel.Create(1,Tokens(['A','B']),Weights,nil);
  try
    SetLength(Resources,1); Resources[0]:=MakeWfcPipelineResource('rules',
      wprkRules,EncodeWfcRuleText(Rules),'mapped result fixture','MIT','');
  finally Rules.Free; end;
  SetLength(Passes,3); SetLength(Topologies,3);
  for I:=0 to 2 do
  begin
    Passes[I]:=MakeWfcPipelinePass('pass-'+IntToStr(I),wppvPublic,
      gpmOverlay,WFC_PIPELINE_NO_INDEX,wpakRules,0,False,wseWhole);
    Topologies[I]:=MakeWfcPipelinePassTopology(1,
      MakeWfcLatticeVector(-4,0,0),MakeWfcLatticeVector(2,1,1),False);
  end;
  Passes[1].Visibility:=wppvPrivate;
  Topologies[0].Pitch.X:=4;
  SetLength(Dependencies,1); Dependencies[0]:=MakeWfcPipelineDependency(2,0);
  Query:=Default(TWfcPipelineMappedQuery);
  Query.Kind:=gpmkCellCoverage; Query.Match:=gpmmAll;
  Query.AllowedProviderTokens:=Tokens(['A']);
  SetLength(Requirements,1);
  Requirements[0]:=MakeWfcPipelineMappedRequirement(2,'A',0,Query);
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('mapped result','MIT','',''),
    CurrentWfcPipelineVersions,1,False,rmBottomUp,Resources,Passes,
    Dependencies,nil,Requirements,nil,nil,WFC_PIPELINE_PASS_MAPPING_VERSION,Topologies);
end;

function NewRun(const Model: TWfcPipelineModel): TWfcPipelineRun;
var Cells: TWfcPipelinePassExtents;
begin
  SetLength(Cells,3); Cells[0]:=MakeWfcLatticeVector(2,1,1);
  Cells[1]:=MakeWfcLatticeVector(5,1,1); Cells[2]:=MakeWfcLatticeVector(3,1,1);
  Result:=TWfcPipelineRun.Create(Model,Cells,3,wpssOneWay,10,0,False,nil,nil);
end;

function Outcomes(const AFailed: Boolean): TWfcPipelinePassOutcomes;
var I: Integer;
begin
  Result:=nil; SetLength(Result,3);
  for I:=0 to 2 do
  begin
    Result[I]:=Default(TWfcPipelinePassOutcome); Result[I].PassIndex:=I;
    Result[I].Executed:=True; Result[I].ExecutionOrdinal:=I;
    Result[I].Disposition:=gpdSolved;
  end;
  if AFailed then
  begin
    Result[1].Disposition:=gpdFailed; Result[1].Contradictions:=1;
    Result[2].Executed:=False; Result[2].ExecutionOrdinal:=-1;
    Result[2].Disposition:=gpdNotRun;
  end;
end;

function Layers: TWfcPipelineResultLayers;
begin
  Result:=nil; SetLength(Result,2);
  Result[0]:=MakeWfcPipelineResultLayer(0,'pass-0',Tokens(['A','A']));
  Result[1]:=MakeWfcPipelineResultLayer(2,'pass-2',Tokens(['A','A','A']));
end;

function NewResult(const Model: TWfcPipelineModel; const Run: TWfcPipelineRun;
  const Values: TWfcPipelineResultLayers): TWfcPipelineResult;
begin
  Result:=TWfcPipelineResult.Create(Model,Run,CurrentWfcPipelineResultVersions,
    wprsSolved,0,wpekNone,0,EmptyWfcPipelineFailure,Outcomes(False),Values);
end;

procedure RejectText(const Model: TWfcPipelineModel; const Run: TWfcPipelineRun;
  const Text: String);
var Value: TWfcPipelineResult; Rejected: Boolean;
begin
  Value:=nil; Rejected:=False;
  try
    try Value:=DecodeWfcPipelineResultText(Text,Model,Run);
    except on E: EConvertError do Rejected:=True; end;
  finally Value.Free; end;
  Check(Rejected,'malformed mapped result text must reject');
end;

function Change(const Text,OldValue,NewValue: String): String;
begin
  Check(Pos(OldValue,Text)>0,'mutation anchor exists: '+OldValue);
  Result:=StringReplace(Text,OldValue,NewValue,[]);
end;

procedure TestResults;
var Model: TWfcPipelineModel; Run: TWfcPipelineRun;
  Value, Again: TWfcPipelineResult; Values, Captured: TWfcPipelineResultLayers;
  Layouts: TWfcLatticeLayouts; Failure: TWfcPipelineFailure;
  Graph: TGraph; I,J: Integer; Text,FailedText: String; Rejected: Boolean;
begin
  Model:=Recipe; Run:=NewRun(Model); Value:=nil; Again:=nil; Graph:=nil;
  try
    Values:=Layers; Value:=NewResult(Model,Run,Values);
    Check((Value.FormatVersion=2) and (Value.PassCount=3) and (Value.LayerCount=2),
      'result2 owns all-pass layouts but only public values');
    Check((Value.TotalCellCount=10) and (Value.CellCount=2),
      'legacy cell count is pass-zero view; total is actual aggregate');
    Check((Value.PassCellCount(1)=5) and (Value.PassOffsetAt(2)=7),
      'private and public pass layout slots both retained');
    Check(Value.LayerLayoutAt(1).Cells.X=3,'layer layout follows public pass index not layer index');
    Check(Value.PassTopologyAt(1).Rank=1,'detached private rank available');
    Text:=EncodeWfcPipelineResultText(Value);
    Check(Pos('layouts=3'+#10,Text)>0,'complete resolved layout table serialized');
    Check(Pos('layout=1,1,-4,0,0,2,1,1,5,1,1,false'+#10,Text)>0,
      'private layout includes signed origin/nonunit pitch without private values');
    Again:=DecodeWfcPipelineResultText(Text,Model,Run);
    Check((Again.Signature=Value.Signature) and (EncodeWfcPipelineResultText(Again)=Text),
      'mapped result canonical byte round trip');
    FreeAndNil(Again);
    Values[0].Tokens[0]:='B'; Layouts:=Value.CopyPassLayouts; Layouts[2].Cells.X:=17;
    Captured:=Value.CopyLayers; Captured[1].Tokens[2]:='B';
    Check(EncodeWfcPipelineResultText(Value)=Text,'caller/copy edits cannot change result');
    FreeAndNil(Value);
    Values:=Layers; Values[0].Tokens[1]:='B';
    Rejected:=False;
    try Value:=NewResult(Model,Run,Values);
    except on E: EWfcPipelineResult do Rejected:=Pos('mapped clause',E.Message)>0; end;
    FreeAndNil(Value);
    Check(Rejected,'in-vocabulary far-cell mapped violation rejects before signature publication');
    Values[1].Tokens[2]:='B'; Value:=NewResult(Model,Run,Values);
    Check(Value.LayerAt(1).Tokens[2]='B','only matching consumer tokens activate mapped clauses');
    FreeAndNil(Value);

    RejectText(Model,Run,Change(Text,'layout-version=1','layout-version=2'));
    RejectText(Model,Run,Change(Text,'mapping-version=1','mapping-version=0'));
    RejectText(Model,Run,Change(Text,'layouts=3','layouts=2'));
    RejectText(Model,Run,Change(Text,'layout=1,1,-4,0,0,2,1,1,5,1,1,false'+#10,''));
    RejectText(Model,Run,Change(Text,'layout=1,1,','layout=0,1,'));
    RejectText(Model,Run,Change(Text,'layout=1,1,-4','layout=1,2,-4'));
    RejectText(Model,Run,Change(Text,'layout=1,1,-4','layout=1,1,-0'));
    RejectText(Model,Run,Change(Text,'layout=1,1,-4','layout=1,1,-2147483649'));
    RejectText(Model,Run,Change(Text,'layout=1,1,-4','layout=1,1,2147483647'));
    RejectText(Model,Run,Change(Text,',2,1,1,5,1,1,false',',0,1,1,5,1,1,false'));
    RejectText(Model,Run,Change(Text,',2,1,1,5,1,1,false',',2,1,1,0,1,1,false'));
    RejectText(Model,Run,Change(Text,',2,1,1,5,1,1,false',',3,1,1,5,1,1,false'));
    RejectText(Model,Run,Change(Text,',2,1,1,5,1,1,false',',2,1,1,6,1,1,false'));
    RejectText(Model,Run,Change(Text,',2,1,1,5,1,1,false',',2,1,1,5,1,1,true'));
    RejectText(Model,Run,Change(Text,'width=2','width=3'));
    RejectText(Model,Run,Change(Text,'layer=1,2,pass-2,3','layer=1,2,pass-2,2'));
    RejectText(Model,Run,Change(Text,'layer=1,2,pass-2,3','layer=1,1,pass-2,3'));
    RejectText(Model,Run,Change(Text,'wfcpipeline-result=2','wfcpipeline-result=1'));

    Failure:=EmptyWfcPipelineFailure; Failure.Kind:=gckPreviousPass;
    Failure.PassIndex:=1; Failure.EntryIndex:=4; Failure.NeighborIndex:=4;
    Failure.DependencyPassIndex:=0;
    Value:=TWfcPipelineResult.Create(Model,Run,CurrentWfcPipelineResultVersions,
      wprsContradiction,0,wpekNone,0,Failure,Outcomes(True),nil);
    Check((Value.LayerCount=0) and (Value.PassCount=3) and
      (Value.CopyFailure.NeighborIndex=4),'failure owns local neighbor index beyond provider/root count');
    FailedText:=EncodeWfcPipelineResultText(Value);
    Again:=DecodeWfcPipelineResultText(FailedText,Model,Run);
    Check(EncodeWfcPipelineResultText(Again)=FailedText,'failed zero-layer result retains complete layout byte round trip');
    FreeAndNil(Again); FreeAndNil(Value);
    Failure.NeighborIndex:=5; Rejected:=False;
    try
      Value:=TWfcPipelineResult.Create(Model,Run,CurrentWfcPipelineResultVersions,
        wprsContradiction,0,wpekNone,0,Failure,Outcomes(True),nil);
    except on E: EWfcPipelineResult do Rejected:=Pos('neighbor',E.Message)>0; end;
    FreeAndNil(Value); Check(Rejected,'failure neighbor is bounded by its own pass');
    RejectText(Model,Run,Change(FailedText,'failure-entry=4','failure-entry=5'));
    RejectText(Model,Run,Change(FailedText,'failure-neighbor=4','failure-neighbor=5'));
    RejectText(Model,Run,Change(FailedText,'layouts=3','layouts=0'));

    Graph:=TGraph.Create; Graph.CurrentPass:='pass-0'; Graph.PassMode:=gpmOverlay;
    for I:=1 to 2 do
    begin Graph.SwitchToPass('pass-'+IntToStr(I)); Graph.PassMode:=gpmOverlay; Graph.ClearDependencies; end;
    Graph.ConfigurePassLayouts(Run.CopyPassLayouts); Graph.Seed:=Run.Seed;
    for I:=0 to 2 do
      if I<>1 then
        for J:=0 to Run.PassCellCount(I)-1 do Graph.PassGraph[I].Entry[J,0,0].Value:='A';
    Captured:=CaptureWfcPipelinePublicLayers(Model,Run,Graph);
    Check((Length(Captured)=2) and (Length(Captured[0].Tokens)=2) and
      (Length(Captured[1].Tokens)=3),'capture loops each public pass extent independently');
    Value:=NewResult(Model,Run,Captured); Check(Value.LayerAt(1).Tokens[2]='A','capture includes late far-edge cell');
    FreeAndNil(Value);
    Graph.PassGraph[2].Entry[2,0,0].ClearValue; Rejected:=False;
    try Captured:=CaptureWfcPipelinePublicLayers(Model,Run,Graph);
    except on E: EWfcPipelineResult do Rejected:=Pos('empty',E.Message)>0; end;
    Check(Rejected,'capture cannot omit empty cells beyond root size');
    for I:=0 to 2 do
    begin
      Layouts:=Run.CopyPassLayouts;
      case I of
        0: Layouts[1].Origin.X:=0;
        1: Layouts[1].Pitch.X:=3;
        2: Layouts[1].Wrap:=True;
      end;
      Graph.ConfigurePassLayouts(Layouts); Rejected:=False;
      try Captured:=CaptureWfcPipelinePublicLayers(Model,Run,Graph);
      except on E: EWfcPipelineResult do Rejected:=Pos('layout',E.Message)>0; end;
      Check(Rejected,'format2 rejects changed private origin/pitch/wrap with identical local dimensions');
    end;

    Value:=DecodeWfcPipelineResultText(Text,Model,Run);
    FreeAndNil(Graph); FreeAndNil(Run); FreeAndNil(Model);
    Check((Value.PassCellCount(1)=5) and (EncodeWfcPipelineResultText(Value)=Text),
      'result owns complete topology/extent data after recipe and run are freed');
  finally Value.Free; Again.Free; Graph.Free; Run.Free; Model.Free; end;
end;

begin
  try TestResults; WriteLn('Mapped pipeline result checks: ',Checks,'/',Checks);
  except on E: Exception do begin WriteLn('FAIL: ',E.Message); Halt(1); end; end;
end.
