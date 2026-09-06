{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Actual-history replay and publication-failure regression. }
program wfc_workspace_replay_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,wfc,wfc_pipeline_run,wfc_pipeline_prepare,wfc_pipeline_session,
  wfc_pipeline_session_evidence,wfc_pipeline_workspace_journal,
  wfc_pipeline_workspace_journal_text,wfc_pipeline_workspace_replay,
  wfc_workspace_replay_fixture,wfc_workspace_replay_extra,wfc_workspace_replay_scope,
  wfc_workspace_authoring_test;
function ExactReplayLimits(const J: TWfcPipelineWorkspaceJournal): TWfcPipelineWorkspaceReplayLimits;
var I: Integer; A: TWfcPipelineWorkspaceAction;
begin
  Result:=Default(TWfcPipelineWorkspaceReplayLimits); Result.Version:=1;
  Result.MaxEvidenceTextBytes:=J.EvidenceTextBytes;
  for I:=0 to J.ActionCount-1 do
  begin
    A:=J.ActionAt(I);
    if A.Kind=wpwakBeginEpoch then
    begin Inc(Result.MaxEpochs); Inc(Result.MaxInstantiatedCellRecords,J.BorrowRun(A.RunIndex).TotalCellCount); end;
    if A.Kind in [wpwakInitial,wpwakRepair] then Inc(Result.MaxSolveActions);
  end;
end;
procedure VerifyRestoration(const EndWithFailure: Boolean);
var J,Decoded: TWfcPipelineWorkspaceJournal; Execution: TWfcPipelineWorkspaceExecution;
  Expected,Baseline,Actual: TWfcPipelineSessionPublicState; Revision: Integer;
  Text: String; Applied: TWfcPipelineRun; Limits: TWfcPipelineWorkspaceReplayLimits;
begin
  J:=nil; Decoded:=nil; Execution:=nil; Expected:=nil; Baseline:=nil; Actual:=nil; Applied:=nil;
  try
    J:=FixtureJournal(EndWithFailure,Expected,Baseline,Revision); Text:=J.CopyCanonicalText;
    Check(J.Verification=wpwvUnverifiedClaims,'captured document is still unverified claims');
    Decoded:=DecodeWfcPipelineWorkspaceJournalText(Text,JournalLimits);
    Check(Decoded.Verification=wpwvUnverifiedClaims,'decoding is not execution verification');
    Limits:=ExactReplayLimits(Decoded);
    Execution:=ReplayWfcPipelineWorkspace(Decoded,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,Limits);
    Check(Execution.VerifiedActions=J.ActionCount,'every accepted action actually replayed');
    Check(Execution.EpochCount=Limits.MaxEpochs,'all explicit epochs retained');
    Check(Execution.Revision=Revision,'actual final session revision');
    Check(Execution.HasCurrentOutput=not EndWithFailure,'actual final currentness, including normal failed history');
    Check(Execution.HasSuccessfulBaseline,'actual retained successful baseline');
    Check(Execution.CopyCanonicalJournal=Text,'complete input journal retained unchanged');
    ExportReplayJournal(Execution.CopyCanonicalJournal);
    Actual:=Execution.CopyPublicState; EqualPublicState(Expected,Actual); FreeAndNil(Actual);
    Actual:=Execution.CopyLastSuccessfulState; EqualPublicState(Baseline,Actual); FreeAndNil(Actual);
    Applied:=Execution.CopyAppliedRun;
    Check(Applied.LockCount=0,'restored output is not injected as caller locks');
    if not EndWithFailure then
      Check((Applied.Seed=72) and (Applied.Strategy=wpssNegotiated),'final epoch seed and negotiated strategy retained');
    FreeAndNil(Applied); FreeAndNil(J); FreeAndNil(Decoded);
    Actual:=Execution.CopyPublicState; EqualPublicState(Expected,Actual);
    FreeAndNil(Execution); EqualPublicState(Expected,Actual);
    Check(Length(Text)>0,'complete history remains available after producing owners are gone');
  finally Applied.Free; Actual.Free; Execution.Free; Decoded.Free; J.Free; Baseline.Free; Expected.Free; end;
end;
procedure ExpectRestoreFailure(const Slot: TWfcPipelineWorkspaceSlot;
  const J: TWfcPipelineWorkspaceJournal; const JL: TWfcPipelineWorkspaceJournalLimits;
  const RL: TWfcPipelineReplacementLimits; const OL: TWfcPipelineSessionOutcomeLimits;
  const EL: TWfcPipelineSessionEvidenceLimits; const PL: TWfcPipelineWorkspaceReplayLimits;
  const ExpectedKind: Integer);
var BeforeState,AfterState,BeforeBaseline,AfterBaseline: TWfcPipelineSessionPublicState; Text: String;
  Revision: Integer; Rejected: Boolean;
begin
  BeforeState:=Slot.CopyPublicState; AfterState:=nil; BeforeBaseline:=nil; AfterBaseline:=nil;
  Text:=Slot.CopyCanonicalJournal;
  Revision:=Slot.PublicationRevision; Rejected:=False;
  try
    BeforeBaseline:=Slot.CopyLastSuccessfulState;
    try Slot.Restore(J,JL,RL,OL,EL,PL,Revision);
    except
      on E: EWfcPipelineWorkspaceReplay do
      begin
        Rejected:=True;
        if ExpectedKind>=0 then Check(Ord(E.Kind)=ExpectedKind,'precise replay failure kind');
        if E.Kind=wpwrfEvidenceMismatch then Check(E.MismatchOffset>0,'exact text mismatch offset retained');
      end;
      on E: Exception do begin if ExpectedKind>=0 then raise; Rejected:=True; end;
    end;
    Check(Rejected,'candidate failure actually occurred');
    Check(Slot.HasExecution and (Slot.PublicationRevision=Revision),'failed restoration never publishes a revision');
    Check(Slot.CopyCanonicalJournal=Text,'failed restoration retains every prior journal byte');
    AfterState:=Slot.CopyPublicState; EqualPublicState(BeforeState,AfterState);
    AfterBaseline:=Slot.CopyLastSuccessfulState; EqualPublicState(BeforeBaseline,AfterBaseline);
  finally AfterBaseline.Free; BeforeBaseline.Free; AfterState.Free; BeforeState.Free; end;
end;
procedure TestAtomicFailures;
var J,Bad: TWfcPipelineWorkspaceJournal; Slot: TWfcPipelineWorkspaceSlot;
  Expected,Baseline,Actual: TWfcPipelineSessionPublicState; Revision,I,N,Position: Integer;
  Actions,Reduced: TWfcPipelineWorkspaceActions; PL,ShortPL: TWfcPipelineWorkspaceReplayLimits;
  OL: TWfcPipelineSessionOutcomeLimits; EL: TWfcPipelineSessionEvidenceLimits;
  JL: TWfcPipelineWorkspaceJournalLimits; Text,Prefix: String; Rejected: Boolean;
begin
  J:=nil; Bad:=nil; Slot:=nil; Expected:=nil; Baseline:=nil; Actual:=nil;
  try
    J:=FixtureJournal(False,Expected,Baseline,Revision); PL:=ExactReplayLimits(J); Slot:=TWfcPipelineWorkspaceSlot.Create;
    Check(not Slot.HasExecution and (Slot.PublicationRevision=0),'new publication slot empty');
    Slot.Restore(J,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,PL,0);
    Check(Slot.HasExecution and (Slot.PublicationRevision=1),'complete verified candidate published once');
    Actual:=Slot.CopyPublicState; EqualPublicState(Expected,Actual); FreeAndNil(Actual);
    Text:=J.CopyCanonicalText;

    { Preserve every reported hash and change one valid output token. }
    Actions:=J.CopyActions; Prefix:='state.layer.0.cell.0=0,1,';
    Position:=Pos(Prefix,Actions[1].EvidenceText); Check(Position>0,'first ordinary outcome has an actual generated root token');
    Inc(Position,Length(Prefix));
    if Actions[1].EvidenceText[Position]='U' then Actions[1].EvidenceText[Position]:='V'
    else begin Check(Actions[1].EvidenceText[Position]='V','root token belongs to U/V fixture'); Actions[1].EvidenceText[Position]:='U'; end;
    Bad:=CopyWithActions(J,Actions);
    Check(Bad.Verification=wpwvUnverifiedClaims,'changed valid output with original hashes remains only a claim');
    ExpectRestoreFailure(Slot,Bad,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,PL,Ord(wpwrfEvidenceMismatch)); FreeAndNil(Bad);

    { A well-enveloped inner truncation is syntactically an opaque claim, never
      a verified history; exact replay must reject it without publishing. }
    Actions:=J.CopyActions; Position:=Pos('invocation.format=',Actions[1].EvidenceText);
    Check(Position>1,'fixture has invocation boundary'); Actions[1].EvidenceText:=Copy(Actions[1].EvidenceText,1,Position-1);
    Bad:=CopyWithActions(J,Actions);
    ExpectRestoreFailure(Slot,Bad,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,PL,Ord(wpwrfEvidenceMismatch)); FreeAndNil(Bad);

    { Valid label indices do not authorize a changed backing provider. }
    Actions:=J.CopyActions; Check(Actions[4].Kind=wpwakRepair,'fixture repair follows lock and clear');
    SetLength(Actions[4].RequestedRootIndices,1); Actions[4].RequestedRootIndices[0]:=3;
    Bad:=CopyWithActions(J,Actions);
    ExpectRestoreFailure(Slot,Bad,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,PL,Ord(wpwrfScope)); FreeAndNil(Bad);

    { Coalescing lock+clear loses real accepted ownership/revision transitions. }
    Actions:=J.CopyActions; Reduced:=nil; SetLength(Reduced,Length(Actions)-2); N:=0;
    for I:=0 to High(Actions) do if (I<>2) and (I<>3) then begin Reduced[N]:=Actions[I]; Inc(N); end;
    Bad:=CopyWithActions(J,Reduced);
    ExpectRestoreFailure(Slot,Bad,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,PL,Ord(wpwrfEvidenceMismatch)); FreeAndNil(Bad);

    ShortPL:=PL; Dec(ShortPL.MaxEpochs);
    ExpectRestoreFailure(Slot,J,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,ShortPL,Ord(wpwrfBudget));
    ShortPL:=PL; Dec(ShortPL.MaxSolveActions);
    ExpectRestoreFailure(Slot,J,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,ShortPL,Ord(wpwrfBudget));
    ShortPL:=PL; Dec(ShortPL.MaxInstantiatedCellRecords);
    ExpectRestoreFailure(Slot,J,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,ShortPL,Ord(wpwrfBudget));
    ShortPL:=PL; Dec(ShortPL.MaxEvidenceTextBytes);
    ExpectRestoreFailure(Slot,J,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,ShortPL,Ord(wpwrfBudget));
    JL:=JournalLimits; JL.MaxEncodedTextBytes:=1;
    ExpectRestoreFailure(Slot,J,JL,ReplacementLimits,OutcomeLimits,EvidenceLimits,PL,-1);
    OL:=OutcomeLimits; OL.MaxPublicCellRecords:=1;
    ExpectRestoreFailure(Slot,J,JournalLimits,ReplacementLimits,OL,EvidenceLimits,PL,-1);
    EL:=EvidenceLimits; EL.MaxTextBytes:=1;
    ExpectRestoreFailure(Slot,J,JournalLimits,ReplacementLimits,OutcomeLimits,EL,PL,-1);

    Rejected:=False;
    try Slot.Restore(nil,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,PL,0);
    except on E: EWfcPipelineWorkspaceReplay do begin Rejected:=True; Check(E.Kind=wpwrfStalePublication,'stale revision rejects before nil input dispatch'); end; end;
    Check(Rejected and (Slot.PublicationRevision=1),'stale publication cannot replace a newer owner');
    Check(Slot.CopyCanonicalJournal=Text,'all failure paths preserve original complete journal');
    FreeAndNil(J); Actual:=Slot.CopyPublicState; EqualPublicState(Expected,Actual);
    FreeAndNil(Slot); EqualPublicState(Expected,Actual);
  finally Actual.Free; Slot.Free; Bad.Free; J.Free; Baseline.Free; Expected.Free; end;
end;
begin
  {$IFDEF WFC_REPLAY_EXPORT}ExportReplayDocuments:=True;{$ENDIF}
  VerifyRestoration(False); VerifyRestoration(True); TestAtomicFailures;
  TestAdditionalReplayHistories; TestNonNumericReplayScope;
  TestWorkspaceAuthoring;
  WriteLn('Workspace replay and atomic restoration checks: ',ReplayChecks);
  if ExportReplayDocuments then WriteLn('Complete workspace journals: ',ReplayDocumentCount);
end.
