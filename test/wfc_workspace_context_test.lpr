{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Static-context regression; no graph/solver/session construction. }
program wfc_workspace_context_test;
{$mode delphi}{$H+}
uses SysUtils,wfc,wfc_model,wfc_sequence,wfc_rule_model,wfc_rule_text,
  wfc_pipeline_model,wfc_pipeline_run,wfc_pipeline_text,wfc_pipeline_run_text,
  wfc_pipeline_workspace_context;
var Checks: Integer;
procedure Check(const Condition: Boolean; const Detail: String);
begin Inc(Checks); if not Condition then raise Exception.Create(Detail); end;
function Fixture(const Name,Value: String): TWfcPipelineModel;
var Rules: TWfcRuleModel; Values: TWfcModelTokens; Weights: TWfcModelIntegerArray;
  Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
begin
  SetLength(Values,1); Values[0]:=TWfcModelToken(Value); SetLength(Weights,1); Weights[0]:=1;
  Rules:=TWfcRuleModel.Create(1,Values,Weights,nil);
  try
    SetLength(Resources,1); Resources[0]:=MakeWfcPipelineResource('vocabulary',wprkRules,
      EncodeWfcRuleText(Rules),'literal source corpus','MIT','source-proof');
  finally Rules.Free; end;
  SetLength(Passes,1); Passes[0]:=MakeWfcPipelinePass('public',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata(Name,'MIT','retained source','fingerprint'),
    1,False,rmBottomUp,Resources,Passes,nil,nil,nil);
end;
procedure Reject(const Recipes: TWfcPipelineWorkspaceRecipeTexts;
  const Runs: TWfcPipelineWorkspaceRunTexts; const Limits: TWfcPipelineWorkspaceContextLimits;
  const RequireContextError: Boolean);
var C: TWfcPipelineWorkspaceContexts; Rejected: Boolean;
begin
  C:=nil; Rejected:=False;
  try
    try C:=TWfcPipelineWorkspaceContexts.Create(Recipes,Runs,Limits);
    except
      on E: EWfcPipelineWorkspaceContext do Rejected:=True;
      on E: Exception do begin if RequireContextError then raise; Rejected:=True; end;
    end;
    Check(Rejected,'invalid context must reject');
  finally C.Free; end;
end;
procedure TestContexts;
var A,B,CopyModel: TWfcPipelineModel; Run,CopyRun: TWfcPipelineRun;
  Recipes,BadRecipes: TWfcPipelineWorkspaceRecipeTexts;
  Runs,BadRuns: TWfcPipelineWorkspaceRunTexts; Limits,BadLimits: TWfcPipelineWorkspaceContextLimits;
  C: TWfcPipelineWorkspaceContexts; I,Bytes: Integer; Rejected: Boolean; SavedA,SavedRun: String;
begin
  A:=Fixture('first','A'); B:=Fixture('second','B'); C:=nil; CopyModel:=nil; CopyRun:=nil;
  SetLength(Recipes,3); SetLength(Runs,3);
  try
    Recipes[0]:=EncodeWfcPipelineModelText(A); Recipes[1]:=EncodeWfcPipelineModelText(B); Recipes[2]:=Recipes[0];
    Run:=TWfcPipelineRun.Create(A,2,1,1,7,wpssOneWay,4,0,True,nil,nil);
    try Runs[0].RecipeIndex:=0; Runs[0].Text:=EncodeWfcPipelineRunText(Run); finally Run.Free; end;
    Run:=TWfcPipelineRun.Create(B,3,1,1,9,wpssNegotiated,8,4,False,nil,nil);
    try Runs[1].RecipeIndex:=1; Runs[1].Text:=EncodeWfcPipelineRunText(Run); finally Run.Free; end;
    Runs[2].RecipeIndex:=2; Runs[2].Text:=Runs[0].Text;
    Bytes:=0; for I:=0 to 2 do Inc(Bytes,Length(Recipes[I])+Length(Runs[I].Text));
    Limits.Version:=1; Limits.MaxRecipes:=3; Limits.MaxRuns:=3; Limits.MaxTextBytes:=Bytes;
    C:=TWfcPipelineWorkspaceContexts.Create(Recipes,Runs,Limits);
    Check((C.RecipeCount=3) and (C.RunCount=3) and (C.TextBytes=Bytes),'exact aggregate limits accepted');
    Check(C.BorrowRecipe(0).Signature=C.BorrowRecipe(2).Signature,'duplicate fixture really shares signature');
    Check(C.BorrowRecipe(0)<>C.BorrowRecipe(2),'duplicate contexts are separate ordered owners, no hash deduplication');
    Check(C.BorrowRun(0)<>C.BorrowRun(2),'duplicate runs remain distinct context rows');
    for I:=0 to 2 do
    begin
      Check(C.RecipeTextAt(I)=Recipes[I],'full canonical recipe retained');
      Check(C.RunTextAt(I).Text=Runs[I].Text,'full canonical run retained');
      Check(C.RunTextAt(I).RecipeIndex=I,'exact ordered run recipe reference retained');
      Check(EncodeWfcPipelineModelText(C.BorrowRecipe(I))=Recipes[I],'typed recipe still complete');
      Check(EncodeWfcPipelineRunText(C.BorrowRun(I))=Runs[I].Text,'typed run still complete');
      Check(C.BorrowRecipe(I).ResourceAt(0).SourceDescription='literal source corpus','resource source provenance retained');
      Check(C.BorrowRecipe(I).ResourceAt(0).SourceLicenseIdentifier='MIT','resource license retained');
    end;
    SavedA:=Recipes[0]; SavedRun:=Runs[0].Text;
    Recipes[0]:='caller-mutated'; Runs[0].Text:='caller-mutated'; Runs[0].RecipeIndex:=1;
    Check(C.RecipeTextAt(0)=SavedA,'recipe text detached from caller');
    Check((C.RunTextAt(0).Text=SavedRun) and (C.RunTextAt(0).RecipeIndex=0),'run record detached from caller');
    Recipes[0]:=SavedA; Runs[0].Text:=SavedRun; Runs[0].RecipeIndex:=0;
    CopyModel:=C.CopyRecipe(0); CopyRun:=C.CopyRun(0);
    Check(CopyModel<>C.BorrowRecipe(0),'CopyRecipe is independently owned');
    Check(CopyRun<>C.BorrowRun(0),'CopyRun is independently owned');
    for I:=-1 to 3 do if (I=-1) or (I=3) then
    begin
      Rejected:=False; try C.RecipeTextAt(I); except on E: EWfcPipelineWorkspaceContext do Rejected:=True; end;
      Check(Rejected,'recipe index rejects');
      Rejected:=False; try C.RunTextAt(I); except on E: EWfcPipelineWorkspaceContext do Rejected:=True; end;
      Check(Rejected,'run index rejects');
    end;
    BadLimits:=Limits; Dec(BadLimits.MaxTextBytes); Reject(Recipes,Runs,BadLimits,True);
    BadLimits:=Limits; Dec(BadLimits.MaxRecipes); Reject(Recipes,Runs,BadLimits,True);
    BadLimits:=Limits; Dec(BadLimits.MaxRuns); Reject(Recipes,Runs,BadLimits,True);
    BadLimits:=Limits; BadLimits.Version:=2; Reject(Recipes,Runs,BadLimits,True);
    BadLimits:=Limits; BadLimits.MaxTextBytes:=0; Reject(Recipes,Runs,BadLimits,True);
    Reject(nil,Runs,Limits,True); Reject(Recipes,nil,Limits,True);
    BadRuns:=nil; SetLength(BadRuns,3);
    for I:=0 to 2 do begin BadRuns[I].RecipeIndex:=Runs[I].RecipeIndex; BadRuns[I].Text:=Runs[I].Text; end;
    BadRuns[0].RecipeIndex:=3; Reject(Recipes,BadRuns,Limits,True);
    BadRuns[0].RecipeIndex:=1; Reject(Recipes,BadRuns,Limits,False);
    BadRecipes:=nil; SetLength(BadRecipes,3); for I:=0 to 2 do BadRecipes[I]:=Recipes[I];
    BadRecipes[0]:='not-a-recipe'#10; BadLimits:=Limits; BadLimits.MaxTextBytes:=1;
    Reject(BadRecipes,Runs,BadLimits,True); { Budget rejection before codec dispatch. }
    Reject(BadRecipes,Runs,Limits,False);
    BadRecipes[0]:=SavedA+#13; Reject(BadRecipes,Runs,Limits,True);
    Check(C.RecipeTextAt(0)=SavedA,'failed independent candidates do not alter retained owner');
    FreeAndNil(C); FreeAndNil(A); FreeAndNil(B);
    Check(EncodeWfcPipelineModelText(CopyModel)=SavedA,'copied recipe outlives producing contexts');
    Check(EncodeWfcPipelineRunText(CopyRun)=SavedRun,'copied run outlives producing contexts');
  finally CopyRun.Free; CopyModel.Free; C.Free; B.Free; A.Free; end;
end;
begin TestContexts; WriteLn('Workspace complete context checks: ',Checks); end.
