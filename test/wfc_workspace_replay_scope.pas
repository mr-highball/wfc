{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Actual nonnumeric DAG scope and multi-root replay historian. }
unit wfc_workspace_replay_scope;
{$mode delphi}{$H+}
interface
procedure TestNonNumericReplayScope;
implementation
uses SysUtils,wfc,wfc_model,wfc_sequence,wfc_rule_model,wfc_rule_text,wfc_pipeline_model,
  wfc_pipeline_text,wfc_pipeline_run,wfc_pipeline_run_text,wfc_pipeline_prepare,
  wfc_pipeline_session,wfc_pipeline_session_evidence,wfc_pipeline_workspace_context,
  wfc_pipeline_workspace_journal,wfc_pipeline_workspace_replay,wfc_workspace_replay_fixture;

procedure TestNonNumericReplayScope;
var Recipe: TWfcPipelineModel; Rules: TWfcRuleModel; BaseRun,LockedRun: TWfcPipelineRun;
  Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies; Weights: TWfcModelIntegerArray;
  Vocabulary: TWfcModelTokens; Locks: TWfcPipelineCellLocks;
  Session: TWfcPipelinePreparedSession; Outcome: TWfcPipelineSessionOutcome;
  Edit: TWfcPipelineSessionEditOutcome; Plan: TWfcPipelineSessionRepairPlan;
  State,Expected,Actual,Baseline: TWfcPipelineSessionPublicState;
  Scope: TWfcPipelineSessionScope; Labels: TGraphPassLabels;
  Recipes: TWfcPipelineWorkspaceRecipeTexts; Runs: TWfcPipelineWorkspaceRunTexts;
  Actions: TWfcPipelineWorkspaceActions; Journal: TWfcPipelineWorkspaceJournal;
  Replay: TWfcPipelineWorkspaceExecution; Limits: TWfcPipelineWorkspaceReplayLimits;
  Report: TGraphSolveReport; I: Integer; Token: TWfcModelToken;
begin
  Recipe:=nil; Rules:=nil; BaseRun:=nil; LockedRun:=nil; Session:=nil;
  Outcome:=nil; Edit:=nil; Plan:=nil; State:=nil; Expected:=nil; Actual:=nil;
  Baseline:=nil; Journal:=nil; Replay:=nil;
  try
    SetLength(Vocabulary,2); Vocabulary[0]:='G'; Vocabulary[1]:='W';
    SetLength(Weights,2); Weights[0]:=1; Weights[1]:=1;
    Rules:=TWfcRuleModel.Create(1,Vocabulary,Weights,nil);
    SetLength(Resources,1); Resources[0]:=MakeWfcPipelineResource('tiles',wprkRules,
      EncodeWfcRuleText(Rules),'two project-authored tokens','MIT','gw');
    SetLength(Passes,3);
    Passes[0]:=MakeWfcPipelinePass('consumer',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
    Passes[1]:=MakeWfcPipelinePass('unrelated',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
    Passes[2]:=MakeWfcPipelinePass('provider',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
    SetLength(Dependencies,1); Dependencies[0]:=MakeWfcPipelineDependency(0,2);
    Recipe:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('nonnumeric DAG','MIT','literal topology','scope-fixture'),
      CurrentWfcPipelineVersions,1,False,rmBottomUp,Resources,Passes,Dependencies,nil,nil);
    BaseRun:=TWfcPipelineRun.Create(Recipe,2,1,1,91,wpssOneWay,256,0,True,nil,nil);
    Session:=TWfcPipelinePreparedSession.Create(Recipe,BaseRun,ReplacementLimits,OutcomeLimits);
    SetLength(Recipes,1); Recipes[0]:=EncodeWfcPipelineModelText(Recipe);
    SetLength(Runs,2); Runs[0].RecipeIndex:=0; Runs[0].Text:=EncodeWfcPipelineRunText(BaseRun);
    SetLength(Actions,5); Actions[0].Kind:=wpwakBeginEpoch; Actions[0].RunIndex:=0;
    Outcome:=Session.ExecuteInitial; Check(Outcome.Solved,'nonnumeric direct initial solves');
    Actions[1].Kind:=wpwakInitial; Actions[1].RunIndex:=0;
    Actions[1].EvidenceText:=EncodeWfcPipelineSessionOutcomeEvidence(Outcome,EvidenceLimits);
    FreeAndNil(Outcome); State:=Session.CopyPublicState;
    Token:=State.LayerAt(2).Cells[0].Token;
    SetLength(Locks,1); Locks[0]:=MakeWfcPipelineCellLock(2,0,0,0,Token);
    LockedRun:=TWfcPipelineRun.Create(Recipe,2,1,1,91,wpssOneWay,256,0,True,Locks,nil);
    Runs[1].RecipeIndex:=0; Runs[1].Text:=EncodeWfcPipelineRunText(LockedRun);
    Edit:=Session.ApplyInputs(LockedRun); Actions[2].Kind:=wpwakEdit; Actions[2].RunIndex:=1;
    Actions[2].EvidenceText:=EncodeWfcPipelineSessionEditEvidence(Edit,EvidenceLimits); FreeAndNil(Edit);
    Edit:=Session.ApplyInputs(BaseRun); Actions[3].Kind:=wpwakEdit; Actions[3].RunIndex:=0;
    Actions[3].EvidenceText:=EncodeWfcPipelineSessionEditEvidence(Edit,EvidenceLimits); FreeAndNil(Edit);
    SetLength(Labels,1); Labels[0]:='consumer'; Plan:=Session.PlanRepair(BaseRun,Labels);
    Scope:=Plan.CopyScope;
    Check(not Plan.CanExecute,'numeric early consumer cannot authorize later provider');
    Check((Length(Scope.MissingPassIndices)=1) and (Scope.MissingPassIndices[0]=2),'explicit missing provider index is retained');
    FreeAndNil(Plan);
    SetLength(Labels,3); Labels[0]:='provider'; Labels[1]:='consumer'; Labels[2]:='provider';
    Plan:=Session.PlanRepair(BaseRun,Labels); Scope:=Plan.CopyScope;
    Check(Plan.CanExecute,'explicit multi-root repair is authorized');
    Check((Length(Scope.RequestedRootIndices)=2) and (Scope.RequestedRootIndices[0]=0) and
      (Scope.RequestedRootIndices[1]=2),'duplicate reordered user roots become canonical0,2');
    Check((Length(Scope.ActivePassIndices)=2) and (Scope.ActivePassIndices[0]=2) and
      (Scope.ActivePassIndices[1]=0),'actual dependency execution order is2,0 not numeric suffix');
    Outcome:=Session.ExecuteRepair(Plan); Check(Outcome.Solved,'nonnumeric multi-root repair solves');
    Report:=Outcome.CopySolveReport;
    Check((not Report.Passes[1].Executed) and (Report.Passes[1].Disposition=gpdReused),'middle independent pass is truly reused');
    Actions[4].Kind:=wpwakRepair; Actions[4].RunIndex:=0;
    Actions[4].RequestedRootIndices:=Scope.RequestedRootIndices;
    Actions[4].EvidenceText:=EncodeWfcPipelineSessionOutcomeEvidence(Outcome,EvidenceLimits);
    Expected:=Session.CopyPublicState; Baseline:=Session.CopyLastSuccessfulState;
    for I:=0 to High(State.LayerAt(1).Cells) do
      Check((State.LayerAt(1).Cells[I].Token=Expected.LayerAt(1).Cells[I].Token) and
        (State.LayerAt(1).Cells[I].Generated=Expected.LayerAt(1).Cells[I].Generated),'unrelated retained cell ownership');
    Journal:=TWfcPipelineWorkspaceJournal.Create(Recipes,Runs,Actions,JournalLimits);
    Limits.Version:=1; Limits.MaxEpochs:=1; Limits.MaxSolveActions:=2;
    Limits.MaxInstantiatedCellRecords:=BaseRun.TotalCellCount; Limits.MaxEvidenceTextBytes:=Journal.EvidenceTextBytes;
    Replay:=ReplayWfcPipelineWorkspace(Journal,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,Limits);
    Check(Replay.VerifiedActions=5,'all nonnumeric history actions replayed');
    Actual:=Replay.CopyPublicState; EqualPublicState(Expected,Actual); FreeAndNil(Actual);
    Actual:=Replay.CopyLastSuccessfulState; EqualPublicState(Baseline,Actual);
    ExportReplayJournal(Replay.CopyCanonicalJournal);
  finally
    Actual.Free; Replay.Free; Journal.Free; Baseline.Free; Expected.Free; State.Free;
    Outcome.Free; Plan.Free; Edit.Free; Session.Free; LockedRun.Free; BaseRun.Free; Recipe.Free; Rules.Free;
  end;
end;
end.
