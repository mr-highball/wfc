{ SPDX-License-Identifier: MIT }
program wfc_pipeline_mapped_artifact_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_rule_model, wfc_rule_text, wfc_sequence,
  wfc_lattice, wfc_pipeline_layout, wfc_pipeline_model, wfc_pipeline_text,
  wfc_pipeline_run, wfc_pipeline_run_text, wfc_pipeline_result,
  wfc_pipeline_result_text, wfc_pipeline_runtime, wfc_text_codec,
  wfc_artifact_document, wfc_artifact_inspect;

var Checks:Integer;

procedure Check(const Condition:Boolean; const Detail:String);
begin Inc(Checks); if not Condition then raise Exception.Create(Detail); end;

function V(const X,Y,Z:Integer):TWfcLatticeVector;
begin Result:=MakeWfcLatticeVector(X,Y,Z); end;

function OneToken(const Token:TWfcModelToken):TWfcModelTokens;
begin Result:=nil; SetLength(Result,1); Result[0]:=Token; end;

function RuleDocument(const Token:TWfcModelToken):String;
var Model:TWfcRuleModel; Weights:TWfcModelIntegerArray;
begin
  SetLength(Weights,1); Weights[0]:=1;
  Model:=TWfcRuleModel.Create(2,OneToken(Token),Weights,nil);
  try Result:=EncodeWfcRuleText(Model); finally Model.Free; end;
end;

function NewRecipe:TWfcPipelineModel;
const Names:array[0..2] of String=('terrain','foliage','housing');
  Values:array[0..2] of String=('land','clear','house');
  Pitches:array[0..2] of Integer=(8,1,4);
var Resources:TWfcPipelineResources; Passes:TWfcPipelinePasses;
  Dependencies:TWfcPipelineDependencies; Requirements:TWfcPipelineRequirements;
  Topologies:TWfcPipelinePassTopologies; Query:TWfcPipelineMappedQuery; I:Integer;
begin
  SetLength(Resources,3); SetLength(Passes,3); SetLength(Topologies,3);
  for I:=0 to 2 do
  begin
    Resources[I]:=MakeWfcPipelineResource(TWfcModelToken(Names[I]),wprkRules,
      RuleDocument(TWfcModelToken(Values[I])),'project-authored portable spatial fixture','MIT','mapped-artifacts-v1');
    Passes[I]:=MakeWfcPipelinePass(TWfcModelToken(Names[I]),wppvPublic,gpmOverlay,-1,wpakRules,I,False,wseWhole);
    Topologies[I]:=MakeWfcPipelinePassTopology(2,V(0,0,0),V(Pitches[I],Pitches[I],1),False);
  end;
  SetLength(Dependencies,2); SetLength(Requirements,2);
  for I:=0 to 1 do
  begin
    Dependencies[I]:=MakeWfcPipelineDependency(I+1,I);
    Query:=Default(TWfcPipelineMappedQuery); Query.Kind:=gpmkCellCoverage; Query.Match:=gpmmAll;
    Query.AllowedProviderTokens:=OneToken(TWfcModelToken(Values[I]));
    Requirements[I]:=MakeWfcPipelineMappedRequirement(I+1,TWfcModelToken(Values[I+1]),I,Query);
  end;
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('Portable terrain and housing','MIT',
    'fixed recipe, two independently supplied invocation extents','mapped-artifacts-v1'),
    CurrentWfcPipelineVersions,2,False,rmBottomUp,Resources,Passes,Dependencies,nil,
    Requirements,nil,nil,1,Topologies);
end;

procedure CheckReport(const Text:String);
var I:Integer;
begin
  Check((Length(Text)>0) and (Text[Length(Text)]=#10),'inspection is LF terminated');
  Check(Pos('execution=not-run'#10,Text)>0,'inspection never implies replay');
  for I:=1 to Length(Text) do
    if not ((Text[I]=#10) or ((Ord(Text[I])>=32) and (Ord(Text[I])<=126))) then
      raise Exception.Create('inspection contains an unsafe byte');
end;

procedure RoundTrip(const Recipe:TWfcPipelineModel; const WidthFactor:Integer;
  out RecipeText,RunText,ResultText:String);
var Extents:TWfcPipelinePassExtents; Run:TWfcPipelineRun; ResultValue:TWfcPipelineResult;
  Artifact:TWfcArtifactDocument; Report,Payload:String; I:Integer;
begin
  SetLength(Extents,3);
  Extents[0]:=V(WidthFactor,1,1); Extents[1]:=V(8*WidthFactor,8,1);
  Extents[2]:=V(2*WidthFactor,2,1);
  Run:=TWfcPipelineRun.Create(Recipe,Extents,4,wpssOneWay,256,0,True,nil,nil);
  ResultValue:=nil;
  try
    ResultValue:=ExecuteWfcPipeline(Recipe,Run);
    Check(ResultValue.Status=wprsSolved,'real mapped pipeline solved');
    Check(ResultValue.LayerCount=3,'every public layer captured');
    Check(Run.TotalCellCount=69*WidthFactor,'actual pass-cell sum, not a maximum padded grid');
    Check(Run.PassOffsetAt(2)=65*WidthFactor,'housing prefix follows terrain plus foliage');
    for I:=0 to 2 do
      Check(Length(ResultValue.LayerAt(I).Tokens)=Run.PassCellCount(I),'each layer has its own extent');
    RecipeText:=EncodeWfcPipelineModelText(Recipe);
    RunText:=EncodeWfcPipelineRunText(Run);
    ResultText:=EncodeWfcPipelineResultText(ResultValue);
    Check(Pos('wfcpipeline=5'#10,RecipeText)=1,'spatial recipe format5');
    Check(Pos('wfcpipeline-run=2'#10,RunText)=1,'explicit extents run2');
    Check(Pos('wfcpipeline-result=2'#10,ResultText)=1,'all-pass layout result2');
  finally ResultValue.Free; Run.Free; end;

  Artifact:=TWfcArtifactDocument.Create(wakRecipe,RecipeText,'','');
  try
    Check(Artifact.CanonicalText=RecipeText,'real recipe artifact binding preserves full bytes');
    Report:=WfcInspectArtifact(Artifact,1000); CheckReport(Report);
    Check(Pos('pass-topology pass=0 rank=2 origin=0,0,0 pitch=8,8,1 wrap=false',Report)>0,
      'recipe inspection shows coarse terrain geometry');
    Check(Pos('pass-topology pass=1 rank=2 origin=0,0,0 pitch=1,1,1 wrap=false',Report)>0,
      'recipe inspection shows fine foliage geometry');
    Check(Pos('mapped-requirement index=1 kind=cell consumer=2 token=house provider=1 match=all',Report)>0,
      'mapped query appears as world geometry, not legacy count terms');
    Check(Pos('mapped-allowed requirement=1 index=0 token=clear',Report)>0,'public provider token visible');
  finally Artifact.Free; end;
  Artifact:=TWfcArtifactDocument.Create(wakRun,RunText,RecipeText,'');
  try
    Check(Artifact.CanonicalText=RunText,'bound run full bytes preserved');
    Report:=WfcInspectArtifact(Artifact,1000); CheckReport(Report);
    Check(Pos('run-layouts count=3 total-cells='+IntToStr(69*WidthFactor),Report)>0,
      'inspector exposes actual sum');
    Check(Pos('run-layout pass=1 rank=2 cells='+IntToStr(8*WidthFactor)+',8,1',Report)>0,
      'run inspector does not substitute root extent');
    Report:=WfcInspectArtifact(Artifact,1); CheckReport(Report);
    Check(Pos('details-shown=1'#10'truncated=true'#10,Report)>0,'spatial diagnostics respect record budget');
  finally Artifact.Free; end;
  Artifact:=TWfcArtifactDocument.Create(wakResult,ResultText,RecipeText,RunText);
  try
    Check(Artifact.CanonicalText=ResultText,'bound result full bytes preserved');
    Report:=WfcInspectArtifact(Artifact,1000); CheckReport(Report);
    Check(Pos('solved-public-mapped-policies=checked',Report)>0,'public policy validation scope disclosed');
    Check(Pos('full-solution=not-proven',Report)>0,'static inspection does not claim full hidden solve proof');
    Check(Pos('result-layout pass=1 rank=2 cells='+IntToStr(8*WidthFactor)+',8,1',Report)>0,
      'result includes non-root geometry');
    Check(Pos('cell layer=2 index='+IntToStr(4*WidthFactor-1)+' xyz='+IntToStr(2*WidthFactor-1)+
      ',1,0 token=house world-min='+IntToStr(8*WidthFactor-4)+',4,0 world-max-exclusive='+
      IntToStr(8*WidthFactor)+',8,1',Report)>0,'last house is decoded in its own local/world coordinates');
    Artifact.RequireReplay;
    Check(Artifact.CanonicalText=ResultText,'fresh complete replay leaves imported result unchanged');
    Payload:=WfcTextEncodeToken(TWfcModelToken(RecipeText+#0+RunText+#0+ResultText),'mapped artifact parity');
    WriteLn('mapped-artifact-parity-',WidthFactor,'=',Payload);
  finally Artifact.Free; end;
end;

var Model:TWfcPipelineModel; RecipeA,RecipeB,RunA,RunB,ResultA,ResultB:String;
  Failed:Boolean; Artifact:TWfcArtifactDocument;
begin
  Model:=NewRecipe;
  try
    RoundTrip(Model,1,RecipeA,RunA,ResultA);
    RoundTrip(Model,2,RecipeB,RunB,ResultB);
    Check(RecipeA=RecipeB,'one immutable recipe supports two independently requested extents');
    Check((RunA<>RunB) and (ResultA<>ResultB),'run and result identities bind changed extent tables');
    Failed:=False; Artifact:=nil;
    try Artifact:=TWfcArtifactDocument.Create(wakResult,ResultA,RecipeA,RunB);
    except on EWfcArtifactInvalid do Failed:=True; end;
    Artifact.Free; Check(Failed,'wrong extent invocation cannot bind a saved result');
  finally Model.Free; end;
  WriteLn('Mapped artifact inspect and replay: ',Checks,' checks passed.');
end.
