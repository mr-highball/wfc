{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Actual captured histories + graph-free journal model/codec checks.
  Capturing this fixture executes sessions; constructing/decoding journals does not. }
program wfc_workspace_journal_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,wfc,wfc_model,wfc_sequence,wfc_rule_model,wfc_rule_text,wfc_lattice,
  wfc_pipeline_model,wfc_pipeline_layout,wfc_pipeline_run,wfc_pipeline_text,
  wfc_pipeline_run_text,wfc_pipeline_prepare,wfc_pipeline_session,
  wfc_pipeline_session_evidence,wfc_pipeline_workspace_context,
  wfc_pipeline_workspace_journal,wfc_pipeline_workspace_journal_text
  {$IFDEF WFC_JOURNAL_EXPORT},wfc_text_codec{$IFDEF PAS2JS},Web{$ENDIF}{$ENDIF};
var Checks: Integer;
{$IFDEF WFC_JOURNAL_EXPORT}{$IFDEF PAS2JS}
procedure PublishJournalDocument(const Text: String);
var E: TJSElement;
begin
  E:=document.createElement('pre'); E.id:='journal-parity-1';
  E.className:='complete-workspace-journal'; E.textContent:=Text;
  document.body.appendChild(E);
end;
{$ENDIF}{$ENDIF}
procedure Check(const Condition: Boolean; const Detail: String);
begin Inc(Checks); if not Condition then raise Exception.Create(Detail); end;
function Fixture(const Spatial: Boolean): TWfcPipelineModel;
var Rules: TWfcRuleModel; Values: TWfcModelTokens; Weights: TWfcModelIntegerArray;
  Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies; Topologies: TWfcPipelinePassTopologies;
begin
  SetLength(Values,1); Values[0]:='land'; SetLength(Weights,1); Weights[0]:=1;
  Rules:=TWfcRuleModel.Create(1,Values,Weights,nil);
  try
    SetLength(Resources,1); Resources[0]:=MakeWfcPipelineResource('vocabulary',wprkRules,
      EncodeWfcRuleText(Rules),'journal literal source corpus','MIT','journal-source-proof');
  finally Rules.Free; end;
  SetLength(Passes,2);
  Passes[0]:=MakeWfcPipelinePass('public',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  if Spatial then
  begin
    Passes[1]:=MakeWfcPipelinePass('other',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
    SetLength(Topologies,2);
    Topologies[0]:=MakeWfcPipelinePassTopology(1,MakeWfcLatticeVector(-8,4,3),MakeWfcLatticeVector(2,1,1),False);
    Topologies[1]:=MakeWfcPipelinePassTopology(1,MakeWfcLatticeVector(1,-2,7),MakeWfcLatticeVector(1,3,4),True);
    Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('mapped journal epoch','MIT','complete source','proof'),
      CurrentWfcPipelineVersions,1,False,rmBottomUp,Resources,Passes,nil,nil,nil,nil,nil,1,Topologies);
  end
  else
  begin
    Passes[1]:=MakeWfcPipelinePass('alias',wppvPublic,gpmTransform,0,wpakEmpty,-1,False,wseWhole);
    SetLength(Dependencies,1); Dependencies[0]:=MakeWfcPipelineDependency(1,0);
    Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('legacy journal epoch','MIT','complete source','proof'),
      1,False,rmBottomUp,Resources,Passes,Dependencies,nil,nil);
  end;
end;
function GenerousLimits: TWfcPipelineWorkspaceJournalLimits;
begin
  Result.Version:=1; Result.MaxRecipes:=16; Result.MaxRuns:=32; Result.MaxContextTextBytes:=1000000;
  Result.MaxActions:=128; Result.MaxRootReferences:=128; Result.MaxEvidenceTextBytes:=4000000; Result.MaxEncodedTextBytes:=8000000;
end;
function ReplacementLimits: TWfcPipelineReplacementLimits;
begin Result.Version:=1; Result.MaxRetainedCellRecords:=10000; Result.MaxRetainedValueItems:=10000; Result.MaxCandidateVisits:=1000000; end;
function CaptureLimits: TWfcPipelineSessionOutcomeLimits;
begin
  Result.Version:=1; Result.MaxPublicCellRecords:=10000; Result.MaxEncodedTokenBytes:=1000000;
  Result.MaxReportPassRecords:=10000; Result.MaxTraceEvents:=100000; Result.MaxExcludedAssignmentItems:=100000;
end;
function EvidenceLimits: TWfcPipelineSessionEvidenceLimits;
begin Result.Version:=1; Result.MaxTextBytes:=2000000; Result.MaxLines:=100000; end;
function CloneActions(const A: TWfcPipelineWorkspaceActions): TWfcPipelineWorkspaceActions;
var I: Integer;
begin
  Result:=nil; SetLength(Result,Length(A));
  for I:=0 to High(A) do begin Result[I]:=A[I]; Result[I].RequestedRootIndices:=Copy(A[I].RequestedRootIndices,0,Length(A[I].RequestedRootIndices)); end;
end;
procedure AddAction(var A: TWfcPipelineWorkspaceActions; const Kind: TWfcPipelineWorkspaceActionKind;
  const RunIndex: Integer; const Roots: TGraphPassIndices; const Evidence: String);
var N: Integer;
begin
  N:=Length(A); SetLength(A,N+1); A[N].Kind:=Kind; A[N].RunIndex:=RunIndex;
  A[N].RequestedRootIndices:=Copy(Roots,0,Length(Roots)); A[N].EvidenceText:=Evidence;
end;
procedure RejectModel(const Recipes: TWfcPipelineWorkspaceRecipeTexts;
  const Runs: TWfcPipelineWorkspaceRunTexts; const Actions: TWfcPipelineWorkspaceActions;
  const Limits: TWfcPipelineWorkspaceJournalLimits; const LabelText: String);
var J: TWfcPipelineWorkspaceJournal; Rejected: Boolean;
begin
  J:=nil; Rejected:=False;
  try
    try J:=TWfcPipelineWorkspaceJournal.Create(Recipes,Runs,Actions,Limits);
    except on E:EWfcPipelineWorkspaceJournal do begin Rejected:=True; Check(Length(E.Message)>0,LabelText+' typed message'); end; end;
    Check(Rejected,LabelText+' rejects');
  finally J.Free; end;
end;
procedure RejectText(const Text: String; const Limits: TWfcPipelineWorkspaceJournalLimits; const LabelText: String);
var J: TWfcPipelineWorkspaceJournal; Rejected: Boolean;
begin
  J:=nil; Rejected:=False;
  try
    try J:=DecodeWfcPipelineWorkspaceJournalText(Text,Limits);
    except on E:EConvertError do begin Rejected:=True; Check(Pos('invalid WFC workspace journal text:',E.Message)=1,LabelText+' codec prefix'); end; end;
    Check(Rejected,LabelText+' rejects');
  finally J.Free; end;
end;
procedure BuildHistory(out Recipes: TWfcPipelineWorkspaceRecipeTexts;
  out Runs: TWfcPipelineWorkspaceRunTexts; out Actions: TWfcPipelineWorkspaceActions;
  out FailedInitialEvidence: String);
var A,B: TWfcPipelineModel; R: array of TWfcPipelineRun; Locks: TWfcPipelineCellLocks;
  Domains: TWfcPipelineCellDomains; Extents: TWfcPipelinePassExtents;
  S: TWfcPipelinePreparedSession; O: TWfcPipelineSessionOutcome; E: TWfcPipelineSessionEditOutcome;
  P: TWfcPipelineSessionRepairPlan; Roots: TGraphPassIndices; Labels: TGraphPassLabels;
  State: TWfcPipelineSessionPublicState; I: Integer; Rejected: Boolean;
begin
  Recipes:=nil; Runs:=nil; Actions:=nil; FailedInitialEvidence:='';
  A:=nil; B:=nil; S:=nil; O:=nil; E:=nil; P:=nil; State:=nil; SetLength(R,15);
  try
    A:=Fixture(False); B:=Fixture(True); SetLength(Recipes,3);
    Recipes[0]:=EncodeWfcPipelineModelText(A); Recipes[1]:=EncodeWfcPipelineModelText(B); Recipes[2]:=Recipes[0];
    R[0]:=TWfcPipelineRun.Create(A,3,1,1,7,wpssOneWay,64,0,True,nil,nil);
    SetLength(Locks,1); Locks[0]:=MakeWfcPipelineCellLock(1,0,0,0,'land');
    R[1]:=TWfcPipelineRun.Create(A,3,1,1,7,wpssOneWay,64,0,True,Locks,nil);
    R[2]:=TWfcPipelineRun.Create(A,3,1,1,7,wpssNegotiated,48,8,False,nil,nil);
    R[3]:=TWfcPipelineRun.Create(A,3,1,1,8,wpssOneWay,64,0,True,nil,nil);
    R[4]:=TWfcPipelineRun.Create(A,4,1,1,7,wpssOneWay,64,0,True,nil,nil);
    SetLength(Extents,2); Extents[0]:=MakeWfcLatticeVector(2,1,1); Extents[1]:=MakeWfcLatticeVector(3,1,1);
    R[5]:=TWfcPipelineRun.Create(B,Extents,11,wpssNegotiated,64,8,True,nil,nil);
    Extents[1].X:=4; R[6]:=TWfcPipelineRun.Create(B,Extents,11,wpssNegotiated,64,8,True,nil,nil);
    R[7]:=TWfcPipelineRun.Create(A,3,1,1,7,wpssOneWay,64,0,True,nil,nil);
    Extents[0].X:=3; Extents[1].X:=3;
    R[8]:=TWfcPipelineRun.Create(A,Extents,7,wpssOneWay,64,0,True,nil,nil);
    SetLength(Domains,1); Domains[0]:=MakeWfcPipelineCellDomain(0,0,0,0,nil);
    R[9]:=TWfcPipelineRun.Create(A,3,1,1,7,wpssOneWay,64,0,True,nil,Domains);
    R[10]:=TWfcPipelineRun.Create(A,3,1,1,7,wpssOneWay,64,0,True,nil,nil);
    SetLength(Locks,2); Locks[0]:=MakeWfcPipelineCellLock(0,0,0,0,'land'); Locks[1]:=MakeWfcPipelineCellLock(0,1,0,0,'land');
    R[11]:=TWfcPipelineRun.Create(A,3,1,1,7,wpssOneWay,64,0,True,Locks,nil);
    Locks[0]:=MakeWfcPipelineCellLock(0,1,0,0,'land'); Locks[1]:=MakeWfcPipelineCellLock(0,0,0,0,'land');
    Rejected:=False;
    try R[12]:=TWfcPipelineRun.Create(A,3,1,1,7,wpssOneWay,64,0,True,Locks,nil);
    except on E:EWfcPipelineRun do Rejected:=True; end;
    Check(Rejected,'typed run rejects reversed authored lock order before journal construction');
    Locks[0]:=MakeWfcPipelineCellLock(0,0,0,0,'land'); Locks[1]:=MakeWfcPipelineCellLock(0,2,0,0,'land');
    R[12]:=TWfcPipelineRun.Create(A,3,1,1,7,wpssOneWay,64,0,True,Locks,nil);
    SetLength(Domains,2); Domains[0]:=MakeWfcPipelineCellDomain(0,0,0,0,nil); Domains[1]:=MakeWfcPipelineCellDomain(0,1,0,0,nil);
    R[13]:=TWfcPipelineRun.Create(A,3,1,1,7,wpssOneWay,64,0,True,nil,Domains);
    Domains[0]:=MakeWfcPipelineCellDomain(0,1,0,0,nil); Domains[1]:=MakeWfcPipelineCellDomain(0,0,0,0,nil);
    Rejected:=False;
    try R[14]:=TWfcPipelineRun.Create(A,3,1,1,7,wpssOneWay,64,0,True,nil,Domains);
    except on E:EWfcPipelineRun do Rejected:=True; end;
    Check(Rejected,'typed run rejects reversed authored domain order before journal construction');
    Domains[0]:=MakeWfcPipelineCellDomain(0,0,0,0,nil); Domains[1]:=MakeWfcPipelineCellDomain(0,2,0,0,nil);
    R[14]:=TWfcPipelineRun.Create(A,3,1,1,7,wpssOneWay,64,0,True,nil,Domains);
    SetLength(Runs,Length(R));
    for I:=0 to High(R) do begin Runs[I].RecipeIndex:=0; Runs[I].Text:=EncodeWfcPipelineRunText(R[I]); end;
    Runs[5].RecipeIndex:=1; Runs[6].RecipeIndex:=1; Runs[7].RecipeIndex:=2;
    SetLength(Roots,1); Roots[0]:=0; SetLength(Labels,1); Labels[0]:='public';
    S:=TWfcPipelinePreparedSession.Create(A,R[0],ReplacementLimits,CaptureLimits);
    AddAction(Actions,wpwakBeginEpoch,0,nil,'');
    O:=S.ExecuteInitial; Check(O.Solved,'fixture baseline really solved');
    AddAction(Actions,wpwakInitial,0,nil,EncodeWfcPipelineSessionOutcomeEvidence(O,EvidenceLimits)); FreeAndNil(O);
    E:=S.ApplyInputs(R[1]); State:=E.CopyPublicState;
    Check(not State.LayerAt(0).Cells[0].Generated,'fixture same-token lock really becomes caller-owned'); FreeAndNil(State);
    AddAction(Actions,wpwakEdit,1,nil,EncodeWfcPipelineSessionEditEvidence(E,EvidenceLimits)); FreeAndNil(E);
    E:=S.ApplyInputs(R[0]); State:=E.CopyPublicState;
    Check(State.LayerAt(0).Cells[0].Empty,'fixture clear really empties prior generated provider'); FreeAndNil(State);
    AddAction(Actions,wpwakEdit,0,nil,EncodeWfcPipelineSessionEditEvidence(E,EvidenceLimits)); FreeAndNil(E);
    P:=S.PlanRepair(R[0],Labels); O:=S.ExecuteRepair(P); Check(O.Solved,'fixture explicit provider repair solves');
    AddAction(Actions,wpwakRepair,0,Roots,EncodeWfcPipelineSessionOutcomeEvidence(O,EvidenceLimits)); FreeAndNil(O); FreeAndNil(P);
    E:=S.ApplyInputs(R[2]); AddAction(Actions,wpwakEdit,2,nil,EncodeWfcPipelineSessionEditEvidence(E,EvidenceLimits)); FreeAndNil(E);
    P:=S.PlanRepair(R[2],Labels); O:=S.ExecuteRepair(P); Check(O.Solved,'fixture negotiated selective repair solves');
    AddAction(Actions,wpwakRepair,2,Roots,EncodeWfcPipelineSessionOutcomeEvidence(O,EvidenceLimits)); FreeAndNil(O); FreeAndNil(P); FreeAndNil(S);
    S:=TWfcPipelinePreparedSession.Create(B,R[5],ReplacementLimits,CaptureLimits);
    AddAction(Actions,wpwakBeginEpoch,5,nil,''); O:=S.ExecuteInitial; Check(O.Solved,'fixture unlike-layout second epoch solves');
    AddAction(Actions,wpwakInitial,5,nil,EncodeWfcPipelineSessionOutcomeEvidence(O,EvidenceLimits)); FreeAndNil(O); FreeAndNil(S);
    S:=TWfcPipelinePreparedSession.Create(A,R[3],ReplacementLimits,CaptureLimits);
    AddAction(Actions,wpwakBeginEpoch,3,nil,''); O:=S.ExecuteInitial; Check(O.Solved,'fixture explicit new-seed epoch solves');
    AddAction(Actions,wpwakInitial,3,nil,EncodeWfcPipelineSessionOutcomeEvidence(O,EvidenceLimits)); FreeAndNil(O); FreeAndNil(S);
    S:=TWfcPipelinePreparedSession.Create(A,R[0],ReplacementLimits,CaptureLimits);
    AddAction(Actions,wpwakBeginEpoch,0,nil,''); E:=S.ApplyInputs(R[1]);
    AddAction(Actions,wpwakEdit,1,nil,EncodeWfcPipelineSessionEditEvidence(E,EvidenceLimits)); FreeAndNil(E);
    O:=S.ExecuteInitial; Check(O.Solved,'fixture pre-initial edit then then-applied initial solves');
    AddAction(Actions,wpwakInitial,1,nil,EncodeWfcPipelineSessionOutcomeEvidence(O,EvidenceLimits)); FreeAndNil(O); FreeAndNil(S);
    S:=TWfcPipelinePreparedSession.Create(A,R[9],ReplacementLimits,CaptureLimits); O:=S.ExecuteInitial;
    Check(not O.Solved,'fixture contradictory initial is an actual failed outcome');
    FailedInitialEvidence:=EncodeWfcPipelineSessionOutcomeEvidence(O,EvidenceLimits);
  finally State.Free; P.Free; E.Free; O.Free; S.Free; for I:=0 to High(R) do R[I].Free; B.Free; A.Free; end;
end;
{$IFDEF PAS2JS}
procedure TestHostile(const Recipes: TWfcPipelineWorkspaceRecipeTexts;
  const Runs: TWfcPipelineWorkspaceRunTexts; const Actions: TWfcPipelineWorkspaceActions;
  const Limits: TWfcPipelineWorkspaceJournalLimits; const Text: String); forward;
{$ENDIF}
procedure TestJournal;
var Recipes,BadRecipes: TWfcPipelineWorkspaceRecipeTexts; Runs,BadRuns: TWfcPipelineWorkspaceRunTexts;
  Actions,Bad: TWfcPipelineWorkspaceActions; Limits,Exact,Small: TWfcPipelineWorkspaceJournalLimits;
  J,D: TWfcPipelineWorkspaceJournal; ModelCopy: TWfcPipelineModel; RunCopy: TWfcPipelineRun;
  Action: TWfcPipelineWorkspaceAction; Saved,Changed,FailedEvidence: String; I: Integer; Raised: Boolean;
begin
  J:=nil; D:=nil; ModelCopy:=nil; RunCopy:=nil; BuildHistory(Recipes,Runs,Actions,FailedEvidence); Limits:=GenerousLimits;
  try
    J:=TWfcPipelineWorkspaceJournal.Create(Recipes,Runs,Actions,Limits); Saved:=EncodeWfcPipelineWorkspaceJournalText(J);
    {$IFDEF WFC_JOURNAL_EXPORT}
    WriteLn('journal-parity-1=',WfcTextEncodeToken(TWfcModelToken(Saved),'private journal parity'));
    {$IFDEF PAS2JS}PublishJournalDocument(Saved);{$ENDIF}
    {$ENDIF}
    {$IFDEF PAS2JS}TestHostile(Recipes,Runs,Actions,Limits,Saved);{$ENDIF}
    Check(J.Verification=wpwvUnverifiedClaims,'actual captured inputs do not make static journal verified');
    Check((J.RecipeCount=3) and (J.RunCount=15) and (J.ActionCount=14),'all contexts and all epochs retained');
    Check((J.ActionAt(2).Kind=wpwakEdit) and (J.ActionAt(3).Kind=wpwakEdit),'lock and clear remain distinct accepted edits');
    Check(J.BorrowRecipe(0)<>J.BorrowRecipe(2),'identical recipe contexts are not deduplicated');
    Check(J.BorrowRun(0)<>J.BorrowRun(7),'identical run text in another recipe row remains distinct');
    Check(J.BorrowRun(0)<>J.BorrowRun(10),'duplicate runs bound to the same recipe row remain distinct');
    Check(J.BorrowRecipe(0).ResourceAt(0).SourceLicenseIdentifier='MIT','full resource license retained');
    Check(J.BorrowRun(5).PassLayoutAt(1).Cells.X=3,'unlike mapped local extent retained');
    for I:=0 to High(Actions) do Check(J.ActionAt(I).EvidenceText=Actions[I].EvidenceText,'complete captured evidence retained');
    D:=DecodeWfcPipelineWorkspaceJournalText(Saved,Limits);
    Check(EncodeWfcPipelineWorkspaceJournalText(D)=Saved,'full canonical journal round trip');
    Check(D.Verification=wpwvUnverifiedClaims,'decoded evidence remains explicitly unverified'); FreeAndNil(D);
    Exact:=Limits; Exact.MaxRecipes:=J.RecipeCount; Exact.MaxRuns:=J.RunCount;
    Exact.MaxContextTextBytes:=J.ContextTextBytes; Exact.MaxActions:=J.ActionCount;
    Exact.MaxRootReferences:=J.RootReferenceCount; Exact.MaxEvidenceTextBytes:=J.EvidenceTextBytes; Exact.MaxEncodedTextBytes:=J.EncodedTextBytes;
    D:=TWfcPipelineWorkspaceJournal.Create(Recipes,Runs,Actions,Exact); Check(D.CopyCanonicalText=Saved,'all exact logical limits accepted'); FreeAndNil(D);
    D:=DecodeWfcPipelineWorkspaceJournalText(Saved,Exact); Check(D.EncodedTextBytes=Length(Saved),'exact decoder envelopes accepted'); FreeAndNil(D);
    for I:=0 to 6 do
    begin
      Small:=Exact; case I of
        0:Dec(Small.MaxRecipes); 1:Dec(Small.MaxRuns); 2:Dec(Small.MaxContextTextBytes);
        3:Dec(Small.MaxActions); 4:Dec(Small.MaxRootReferences); 5:Dec(Small.MaxEvidenceTextBytes); 6:Dec(Small.MaxEncodedTextBytes);
      end;
      RejectModel(Recipes,Runs,Actions,Small,'one-less model envelope '+IntToStr(I)); RejectText(Saved,Small,'one-less codec envelope '+IntToStr(I));
    end;
    for I:=0 to 19 do
    begin
      Bad:=CloneActions(Actions);
      case I of
        0:Bad[0].Kind:=wpwakEdit;
        1:Bad[1].RunIndex:=3; { Changed seed without epoch. }
        2:Bad[2].RunIndex:=4; { Changed extent without epoch. }
        3:Bad[2].RunIndex:=7; { Identical recipe in different explicit row. }
        4:Bad[2].RunIndex:=8; { Same layout but different run format. }
        5:Bad[1].RunIndex:=10; { Even identical run text must use exact applied index. }
        6:Bad[4].RunIndex:=1; { Repair cannot introduce locks. }
        7:Bad[2].Kind:=wpwakInitial;
        8:begin Bad[1].Kind:=wpwakRepair; SetLength(Bad[1].RequestedRootIndices,1); Bad[1].RequestedRootIndices[0]:=0; end;
        9:Bad[4].RequestedRootIndices:=nil;
        10:begin SetLength(Bad[4].RequestedRootIndices,2); Bad[4].RequestedRootIndices[0]:=1; Bad[4].RequestedRootIndices[1]:=0; end;
        11:begin SetLength(Bad[4].RequestedRootIndices,2); Bad[4].RequestedRootIndices[0]:=0; Bad[4].RequestedRootIndices[1]:=0; end;
        12:Bad[4].RequestedRootIndices[0]:=2;
        13:begin SetLength(Bad[2].RequestedRootIndices,1); Bad[2].RequestedRootIndices[0]:=0; end;
        14:Bad[0].EvidenceText:=Actions[1].EvidenceText;
        15:Bad[1].EvidenceText:='';
        16:Bad[1].EvidenceText:=StringReplace(Bad[1].EvidenceText,'wfc-session-evidence=1','wfc-session-evidence=2',[]);
        17:Bad[6].EvidenceText:=Actions[4].EvidenceText;
        18:Bad[2].EvidenceText:=Bad[2].EvidenceText+#9#10;
        19:Bad[1].RunIndex:=Length(Runs);
      end;
      RejectModel(Recipes,Runs,Bad,Limits,'invalid transition '+IntToStr(I));
    end;
    Bad:=CloneActions(Actions); SetLength(Bad,2); Bad[1].Kind:=wpwakEdit; Bad[1].RunIndex:=5; Bad[1].EvidenceText:=Actions[2].EvidenceText;
    RejectModel(Recipes,Runs,Bad,Limits,'cross-recipe edit');
    Bad:=CloneActions(Actions); SetLength(Bad,2); Bad[0].RunIndex:=5; Bad[1].Kind:=wpwakEdit; Bad[1].RunIndex:=6; Bad[1].EvidenceText:=Actions[2].EvidenceText;
    RejectModel(Recipes,Runs,Bad,Limits,'nonroot mapped extent drift');
    Bad:=CloneActions(Actions); SetLength(Bad,3); Bad[0].RunIndex:=11; Bad[1].RunIndex:=11;
    Bad[2]:=Actions[4]; Bad[2].RunIndex:=12;
    RejectModel(Recipes,Runs,Bad,Limits,'repair cannot change coordinates within the full authored locks');
    Bad[0].RunIndex:=13; Bad[1].RunIndex:=13; Bad[2].RunIndex:=14;
    RejectModel(Recipes,Runs,Bad,Limits,'repair cannot change coordinates within the full authored domains');
    { Header-valid claims are not interpreted as trusted successful reports. }
    Bad:=CloneActions(Actions); Bad[1].EvidenceText:='wfc-session-evidence=1'#10'kind=ordinary-full'#10;
    D:=TWfcPipelineWorkspaceJournal.Create(Recipes,Runs,Bad,Limits);
    Check(D.Verification=wpwvUnverifiedClaims,'LF-boundary inner truncation is an explicitly unverified claim');
    Changed:=D.CopyCanonicalText; FreeAndNil(D); D:=DecodeWfcPipelineWorkspaceJournalText(Changed,Limits);
    Check(D.ActionAt(1).EvidenceText=Bad[1].EvidenceText,'opaque truncated claim retained exactly without pretending replay'); FreeAndNil(D);
    Bad:=CloneActions(Actions); SetLength(Bad,3); Bad[0].RunIndex:=9;
    Bad[1].RunIndex:=9; Bad[1].EvidenceText:=FailedEvidence;
    Bad[2]:=Actions[4]; Bad[2].RunIndex:=9; { Intentionally unverified repair claim after failed initial. }
    D:=TWfcPipelineWorkspaceJournal.Create(Recipes,Runs,Bad,Limits);
    Check(D.ActionCount=3,'successful baseline is a replay obligation, not inferred statically'); FreeAndNil(D);
    Bad:=nil; AddAction(Bad,wpwakBeginEpoch,0,nil,'');
    D:=TWfcPipelineWorkspaceJournal.Create(Recipes,Runs,Bad,Limits);
    Check((D.RootReferenceCount=0) and (D.EvidenceTextBytes=0),'begin-only history has valid zero actual roots/evidence'); FreeAndNil(D);
    for I:=0 to 13 do
    begin
      case I of
        0:Changed:=Copy(Saved,1,Length(Saved)-1);
        1:Changed:=Copy(Saved,1,Length(Saved)-6);
        2:Changed:=Saved+'unknown=1'#10;
        3:Changed:=Saved+#10;
        4:Changed:=StringReplace(Saved,'wfc-workspace-journal=1','wfc-workspace-journal=2',[]);
        5:Changed:=StringReplace(Saved,'verification=unverified-claims','verification=verified',[]);
        6:Changed:=StringReplace(Saved,'recipe-count=3','recipe-count=03',[]);
        7:Changed:=StringReplace(Saved,'action-count=14','action-count=15',[]);
        8:Changed:=StringReplace(Saved,'root-reference-count=2','root-reference-count=3',[]);
        9:Changed:=StringReplace(Saved,'recipe.0.bytes=','recipe.1.bytes=',[]);
        10:Changed:=StringReplace(Saved,'%0A','%0a',[]);
        11:Changed:=StringReplace(Saved,'end=1','end=0',[]);
        12:Changed:=StringReplace(Saved,'run.0.recipe=0','run.0.recipe=3',[]);
        13:Changed:=StringReplace(Saved,'action.4.root.0=0','action.4.root.0=2',[]);
      end;
      RejectText(Changed,Limits,'malformed outer document '+IntToStr(I));
    end;
    BadRecipes:=Copy(Recipes,0,Length(Recipes)); BadRecipes[0]:='not-a-recipe'#10;
    Small:=Limits; Small.MaxEncodedTextBytes:=1; RejectModel(BadRecipes,Runs,Actions,Small,'outer preflight precedes typed nested decode');
    RejectModel(BadRecipes,Runs,Actions,Limits,'invalid full embedded context');
    BadRuns:=Copy(Runs,0,Length(Runs)); BadRuns[0].Text:=Copy(BadRuns[0].Text,1,Length(BadRuns[0].Text)-1);
    RejectModel(Recipes,BadRuns,Actions,Limits,'truncated embedded run');
    Action:=J.ActionAt(4); Action.RequestedRootIndices[0]:=1; Action.EvidenceText:='changed';
    Check((J.ActionAt(4).RequestedRootIndices[0]=0) and (J.CopyCanonicalText=Saved),'detached action copies cannot mutate owner');
    Bad:=J.CopyActions; Bad[4].RequestedRootIndices[0]:=1; Check(J.ActionAt(4).RequestedRootIndices[0]=0,'nested CopyActions is detached');
    Recipes[0]:='caller-mutated'; Runs[0].Text:='caller-mutated'; Actions[4].RequestedRootIndices[0]:=1;
    Check(J.CopyCanonicalText=Saved,'caller input arrays are detached');
    ModelCopy:=J.CopyRecipe(0); RunCopy:=J.CopyRun(0); Action:=J.ActionAt(1);
    for I:=-1 to J.ActionCount do if (I=-1) or (I=J.ActionCount) then
    begin Raised:=False; try J.ActionAt(I); except on E:EWfcPipelineWorkspaceJournal do Raised:=True; end; Check(Raised,'action index typed boundary'); end;
    FreeAndNil(J);
    Check(Pos('wfcpipeline=',EncodeWfcPipelineModelText(ModelCopy))=1,'copied recipe survives journal');
    Check(Pos('wfcpipeline-run=',EncodeWfcPipelineRunText(RunCopy))=1,'copied run survives journal');
    Check(Pos('wfc-session-evidence=1',Action.EvidenceText)=1,'detached evidence survives journal');
  finally RunCopy.Free; ModelCopy.Free; D.Free; J.Free; end;
end;
{$IFDEF PAS2JS}
procedure TestHostile(const Recipes: TWfcPipelineWorkspaceRecipeTexts;
  const Runs: TWfcPipelineWorkspaceRunTexts; const Actions: TWfcPipelineWorkspaceActions;
  const Limits: TWfcPipelineWorkspaceJournalLimits; const Text: String);
var A: TWfcPipelineWorkspaceActions; R: TWfcPipelineWorkspaceRecipeTexts;
  U: TWfcPipelineWorkspaceRunTexts; L: TWfcPipelineWorkspaceJournalLimits;
  I: Integer; J: TWfcPipelineWorkspaceJournal;
  procedure ModelCase(const Index: Integer);
  var A: TWfcPipelineWorkspaceActions; R: TWfcPipelineWorkspaceRecipeTexts;
    U: TWfcPipelineWorkspaceRunTexts; L: TWfcPipelineWorkspaceJournalLimits;
    Hits: Integer;
  begin
    A:=CloneActions(Actions); R:=Copy(Recipes,0,Length(Recipes)); U:=Copy(Runs,0,Length(Runs)); L:=Limits; Hits:=0;
    asm
      const getter=()=>{Hits++;return 0;};
      switch(Index) {
        case 0:A=null;break;
        case 1:A='invalid';break;
        case 2:delete A[0];break;
        case 3:Object.defineProperty(A,'0',{get:getter});break;
        case 4:Object.defineProperty(A[0],'Kind',{get:getter});break;
        case 5:Object.defineProperty(A[4].RequestedRootIndices,'0',{get:getter});break;
        case 6:A[4].RunIndex=0.5;break;
        case 7:A[4].Kind=NaN;break;
        case 8:A[4].EvidenceText=new String(A[4].EvidenceText);break;
        case 9:A[4].RequestedRootIndices=new Array(1);Object.setPrototypeOf(A[4].RequestedRootIndices,{'0':0});break;
        case 10:A[4].RequestedRootIndices[0]=Infinity;break;
        case 11:Object.defineProperty(R,'0',{get:getter});break;
        case 12:Object.defineProperty(U[0],'RecipeIndex',{get:getter});break;
        case 13:Object.defineProperty(L,'MaxEncodedTextBytes',{get:getter});break;
        case 14:L.MaxActions=NaN;break;
        case 15:L=null;break;
        case 16:delete U[0];break;
        case 17:Object.defineProperty(A[4],'RequestedRootIndices',{get:getter});break;
      }
    end;
    RejectModel(R,U,A,L,'hostile raw input '+IntToStr(Index)); Check(Hits=0,'hostile getter was not executed');
  end;
  procedure TextCase(const Index: Integer);
  var Raw: String; L: TWfcPipelineWorkspaceJournalLimits; Hits: Integer;
  begin
    Raw:=Text; L:=Limits; Hits:=0;
    asm
      switch(Index) {
        case 0:Raw=null;break;
        case 1:Raw=123;break;
        case 2:Raw=new String(Raw);break;
        case 3:L.MaxEncodedTextBytes=Infinity;break;
        case 4:Object.defineProperty(L,'Version',{get:()=>{Hits++;return 1;}});break;
      }
    end;
    RejectText(Raw,L,'hostile codec input '+IntToStr(Index)); Check(Hits=0,'codec getter was not executed');
  end;
  procedure SliceOverrideCase(const Index: Integer);
  var A: TWfcPipelineWorkspaceActions; J: TWfcPipelineWorkspaceJournal;
    Hits: Integer; Detached: TWfcPipelineWorkspaceAction;
  begin
    A:=CloneActions(Actions); J:=nil; Hits:=0;
    asm
      const roots=A[4].RequestedRootIndices;
      switch(Index) {
        case 0:Object.defineProperty(roots,'slice',{get:()=>{Hits++;return function(){return this;};}});break;
        case 1:roots.slice=function(){Hits++;return this;};break;
        case 2:roots.slice=null;break;
      }
    end;
    try
      J:=TWfcPipelineWorkspaceJournal.Create(Recipes,Runs,A,Limits);
      Check(Hits=0,'constructor never dispatches a caller root vector method/getter');
      Check(J.CopyCanonicalText=Text,'non-data slice shadow does not alter canonical root values');
      A[4].RequestedRootIndices[0]:=1;
      Detached:=J.ActionAt(4);
      Check(Detached.RequestedRootIndices[0]=0,'caller slice override cannot retain an alias into journal');
      Detached.RequestedRootIndices[0]:=1;
      Check(J.ActionAt(4).RequestedRootIndices[0]=0,'returned sanitized vector is independently detached');
      Check(Hits=0,'later action access never dispatches original root vector methods');
    finally J.Free; end;
  end;
begin
  J:=nil;
  { Each hostile case gets a fresh typed local record. Reassigning the next
    case into a prior raw null/getter record could fail in test setup itself. }
  for I:=0 to 17 do ModelCase(I);
  for I:=0 to 4 do TextCase(I);
  for I:=0 to 2 do SliceOverrideCase(I);
  A:=CloneActions(Actions); R:=Copy(Recipes,0,Length(Recipes)); U:=Copy(Runs,0,Length(Runs)); L:=Limits;
  asm
    for(const a of A){Object.freeze(a.RequestedRootIndices);Object.freeze(a);}Object.freeze(A);
    for(const u of U)Object.freeze(u);Object.freeze(U);Object.freeze(R);Object.freeze(L);
  end;
  try
    J:=TWfcPipelineWorkspaceJournal.Create(R,U,A,L);
    Check(J.CopyCanonicalText=Text,'valid fully frozen passive input is accepted without bookkeeping writes');
    Check(J.Verification=wpwvUnverifiedClaims,'frozen inputs do not create verification authority');
  finally J.Free; end;
end;
{$ENDIF}
begin
  TestJournal;
  WriteLn('Workspace journal checks: ',Checks);
end.
