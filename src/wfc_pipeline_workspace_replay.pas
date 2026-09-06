{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Exact journal replay into a retained execution and atomic restore.
  Typed in-process owners only; not a concurrent or hostile-owner sandbox. }
unit wfc_pipeline_workspace_replay;
{$mode delphi}{$H+}
interface
uses SysUtils,wfc,wfc_pipeline_model,wfc_pipeline_run,wfc_pipeline_prepare,wfc_pipeline_session,
  wfc_pipeline_session_evidence,wfc_pipeline_workspace_journal;
type
  TWfcPipelineWorkspaceReplayFailureKind = (wpwrfInputs,wpwrfBudget,
    wpwrfScope,wpwrfEvidenceMismatch,wpwrfUnverifiedOwner,wpwrfStalePublication);
  EWfcPipelineWorkspaceReplay = class(Exception)
  private
    FKind: TWfcPipelineWorkspaceReplayFailureKind;
    FActionIndex,FMismatchOffset: Integer;
  public
    constructor CreateFailure(const Kind: TWfcPipelineWorkspaceReplayFailureKind;
      const ActionIndex,MismatchOffset: Integer; const Detail: String);
    property Kind: TWfcPipelineWorkspaceReplayFailureKind read FKind;
    property ActionIndex: Integer read FActionIndex;
    { One-based canonical ASCII offset;0 when no text comparison failed. }
    property MismatchOffset: Integer read FMismatchOffset;
  end;
  TWfcPipelineWorkspaceReplayLimits = record
    Version: Integer;
    MaxEpochs,MaxSolveActions,MaxInstantiatedCellRecords,MaxEvidenceTextBytes: Integer;
  end;
  { Explicit per-operation logical budgets; not peak heap or wall-clock limits.
    Raw JS policy/roots/text are checked, but owner instances remain typed-only.
    No callbacks, proxies, concurrent publication or poisoned global RTL. }
  TWfcPipelineWorkspacePolicy = record
    Version: Integer;
    Journal: TWfcPipelineWorkspaceJournalLimits;
    Replacement: TWfcPipelineReplacementLimits;
    Outcome: TWfcPipelineSessionOutcomeLimits;
    Evidence: TWfcPipelineSessionEvidenceLimits;
    Replay: TWfcPipelineWorkspaceReplayLimits;
  end;
  TWfcPipelineWorkspaceReceipt = class
  private
    FKind: TWfcPipelineWorkspaceActionKind;
    FActionIndex,FRecipeIndex,FRunIndex,FEpochCount: Integer;
    FPublicationRevision,FSessionRevision: Integer;
    FHasCurrentOutput,FHasSuccessfulBaseline: Boolean;
    FEvidenceText: String;
    FState: TWfcPipelineSessionPublicState;
    FEdit: TWfcPipelineSessionEditOutcome;
    FOutcome: TWfcPipelineSessionOutcome;
    constructor CreateOwned;
  public
    destructor Destroy; override;
    { Immutable borrows owned by this receipt, never Free them separately.
      They and their detached copies survive subsequent slot publications. }
    function BorrowPublicState: TWfcPipelineSessionPublicState;
    function BorrowEditOutcome: TWfcPipelineSessionEditOutcome;
    function BorrowSolveOutcome: TWfcPipelineSessionOutcome;
    property Kind: TWfcPipelineWorkspaceActionKind read FKind;
    property ActionIndex: Integer read FActionIndex;
    property RecipeIndex: Integer read FRecipeIndex;
    property RunIndex: Integer read FRunIndex;
    property EpochCount: Integer read FEpochCount;
    property PublicationRevision: Integer read FPublicationRevision;
    property SessionRevision: Integer read FSessionRevision;
    property HasCurrentOutput: Boolean read FHasCurrentOutput;
    property HasSuccessfulBaseline: Boolean read FHasSuccessfulBaseline;
    property EvidenceText: String read FEvidenceText;
  end;
  TWfcPipelineWorkspaceRepairPreview = class
  private
    FScope: TWfcPipelineSessionScope;
    FRunText: String;
    FPublicationRevision,FSessionRevision: Integer;
    FCanExecute,FMissingBaseline: Boolean;
    constructor CreateOwned;
  public
    function CopyScope: TWfcPipelineSessionScope;
    property RunText: String read FRunText;
    property PublicationRevision: Integer read FPublicationRevision;
    property SessionRevision: Integer read FSessionRevision;
    property CanExecute: Boolean read FCanExecute;
    property MissingBaseline: Boolean read FMissingBaseline;
  end;
  TWfcPipelineWorkspaceExecution = class
  private
    FJournal: TWfcPipelineWorkspaceJournal;
    FSession: TWfcPipelinePreparedSession;
    { Session borrows this independent epoch lease, never FJournal. }
    FSessionRecipe: TWfcPipelineModel;
    FVerified: Boolean;
    FVerifiedActions,FEpochCount,FRecipeIndex,FRunIndex: Integer;
    constructor CreateOwned;
    procedure RequireVerified;
    function GetHasCurrentOutput: Boolean;
    function GetHasSuccessfulBaseline: Boolean;
    function GetRevision: Integer;
  public
    destructor Destroy; override;
    function CopyCanonicalJournal: String;
    function CopyPublicState: TWfcPipelineSessionPublicState;
    function CopyLastSuccessfulState: TWfcPipelineSessionPublicState;
    function CopyAppliedRun: TWfcPipelineRun;
    property HasCurrentOutput: Boolean read GetHasCurrentOutput;
    property HasSuccessfulBaseline: Boolean read GetHasSuccessfulBaseline;
    property Revision: Integer read GetRevision;
    property VerifiedActions: Integer read FVerifiedActions;
    property EpochCount: Integer read FEpochCount;
    property CurrentRecipeIndex: Integer read FRecipeIndex;
    property CurrentRunIndex: Integer read FRunIndex;
  end;
  { Single-threaded/synchronous publication owner. Failed restore never writes
    Current or PublicationRevision. No borrowed mutable execution escapes. }
  TWfcPipelineWorkspaceSlot = class
  strict private
    FCurrent: TWfcPipelineWorkspaceExecution;
    FPublicationRevision: Integer;
    function GetHasExecution: Boolean;
    procedure RequirePublication(const Expected: Integer; const WillPublish: Boolean);
    function Author(const Kind: TWfcPipelineWorkspaceActionKind;
      const RecipeText,RunText: String; const Roots: TGraphPassIndices;
      const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedPublicationRevision: Integer): TWfcPipelineWorkspaceReceipt;
    function GetHasCurrentOutput: Boolean;
    function GetHasSuccessfulBaseline: Boolean;
    function GetSessionRevision: Integer;
    function GetCurrentRecipeIndex: Integer;
    function GetCurrentRunIndex: Integer;
  public
    destructor Destroy; override;
    procedure Restore(const Journal: TWfcPipelineWorkspaceJournal;
      const JournalLimits: TWfcPipelineWorkspaceJournalLimits;
      const ReplacementLimits: TWfcPipelineReplacementLimits;
      const OutcomeLimits: TWfcPipelineSessionOutcomeLimits;
      const EvidenceLimits: TWfcPipelineSessionEvidenceLimits;
      const ReplayLimits: TWfcPipelineWorkspaceReplayLimits;
      const ExpectedPublicationRevision: Integer);
    function BeginEpoch(const RecipeText,InitialRunText: String;
      const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedPublicationRevision: Integer): TWfcPipelineWorkspaceReceipt;
    function ApplyInputs(const CompleteRunText: String;
      const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedPublicationRevision: Integer): TWfcPipelineWorkspaceReceipt;
    function ExecuteInitial(const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedPublicationRevision: Integer): TWfcPipelineWorkspaceReceipt;
    function ExecuteRepair(const CompleteRunText: String;
      const RootIndices: TGraphPassIndices; const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedPublicationRevision: Integer): TWfcPipelineWorkspaceReceipt;
    function PreviewRepair(const CompleteRunText: String;
      const RootIndices: TGraphPassIndices; const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedPublicationRevision: Integer): TWfcPipelineWorkspaceRepairPreview;
    function CopyCanonicalJournal: String;
    function CopyPublicState: TWfcPipelineSessionPublicState;
    function CopyLastSuccessfulState: TWfcPipelineSessionPublicState;
    function CopyAppliedRun: TWfcPipelineRun;
    function CopyCurrentRecipe: TWfcPipelineModel;
    property HasExecution: Boolean read GetHasExecution;
    property HasCurrentOutput: Boolean read GetHasCurrentOutput;
    property HasSuccessfulBaseline: Boolean read GetHasSuccessfulBaseline;
    property SessionRevision: Integer read GetSessionRevision;
    property CurrentRecipeIndex: Integer read GetCurrentRecipeIndex;
    property CurrentRunIndex: Integer read GetCurrentRunIndex;
    property PublicationRevision: Integer read FPublicationRevision;
  end;
function ReplayWfcPipelineWorkspace(const Journal: TWfcPipelineWorkspaceJournal;
  const JournalLimits: TWfcPipelineWorkspaceJournalLimits;
  const ReplacementLimits: TWfcPipelineReplacementLimits;
  const OutcomeLimits: TWfcPipelineSessionOutcomeLimits;
  const EvidenceLimits: TWfcPipelineSessionEvidenceLimits;
  const ReplayLimits: TWfcPipelineWorkspaceReplayLimits): TWfcPipelineWorkspaceExecution;
implementation
uses wfc_model,wfc_pipeline_text,wfc_pipeline_run_text,
  wfc_pipeline_workspace_context,wfc_pipeline_workspace_journal_text;
constructor EWfcPipelineWorkspaceReplay.CreateFailure(const Kind: TWfcPipelineWorkspaceReplayFailureKind;
  const ActionIndex,MismatchOffset: Integer; const Detail: String);
begin
  inherited Create('workspace replay: '+Detail); FKind:=Kind;
  FActionIndex:=ActionIndex; FMismatchOffset:=MismatchOffset;
end;
procedure ReplayError(const Kind: TWfcPipelineWorkspaceReplayFailureKind;
  const ActionIndex: Integer; const Detail: String);
begin raise EWfcPipelineWorkspaceReplay.CreateFailure(Kind,ActionIndex,0,Detail); end;
procedure RequireLimits(const R: TWfcPipelineReplacementLimits;
  const O: TWfcPipelineSessionOutcomeLimits; const E: TWfcPipelineSessionEvidenceLimits;
  const L: TWfcPipelineWorkspaceReplayLimits);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    function fields(o,names) {
      if (o===null || typeof o!=='object' || Array.isArray(o)) return false;
      for (const name of names) {
        let p=o,d;
        while (p!==null && !(d=Object.getOwnPropertyDescriptor(p,name))) p=Object.getPrototypeOf(p);
        if (!d || !('value' in d) || !Number.isInteger(d.value) || d.value<1 || d.value>2147483647) return false;
        if (name==='Version' && d.value!==1) return false;
      }
      return true;
    }
    Valid=fields(R,['Version','MaxRetainedCellRecords','MaxRetainedValueItems','MaxCandidateVisits']) &&
      fields(O,['Version','MaxPublicCellRecords','MaxEncodedTokenBytes','MaxReportPassRecords','MaxTraceEvents','MaxExcludedAssignmentItems']) &&
      fields(E,['Version','MaxTextBytes','MaxLines']) &&
      fields(L,['Version','MaxEpochs','MaxSolveActions','MaxInstantiatedCellRecords','MaxEvidenceTextBytes']);
  end;
  if not Valid then ReplayError(wpwrfInputs,-1,'complete passive positive version1 limits required');
  {$ENDIF}
  if (R.Version<>1) or (R.MaxRetainedCellRecords<1) or (R.MaxRetainedValueItems<1) or
    (R.MaxCandidateVisits<1) or (O.Version<>1) or (O.MaxPublicCellRecords<1) or
    (O.MaxEncodedTokenBytes<1) or (O.MaxReportPassRecords<1) or (O.MaxTraceEvents<1) or
    (O.MaxExcludedAssignmentItems<1) or (E.Version<>1) or (E.MaxTextBytes<1) or
    (E.MaxLines<1) or (L.Version<>1) or (L.MaxEpochs<1) or (L.MaxSolveActions<1) or
    (L.MaxInstantiatedCellRecords<1) or (L.MaxEvidenceTextBytes<1) then
      ReplayError(wpwrfInputs,-1,'unsupported or nonpositive execution limits');
end;
procedure Charge(const Amount,Maximum,ActionIndex: Integer; var Used: Integer;
  const Detail: String);
begin
  if (Amount<0) or (Amount>Maximum-Used) then ReplayError(wpwrfBudget,ActionIndex,Detail);
  Inc(Used,Amount);
end;
procedure CompareEvidence(const Expected,Actual: String; const ActionIndex: Integer);
var I,Count,Offset: Integer;
begin
  Count:=Length(Expected); if Length(Actual)<Count then Count:=Length(Actual);
  Offset:=0;
  for I:=1 to Count do if Expected[I]<>Actual[I] then begin Offset:=I; Break; end;
  if (Offset=0) and (Length(Expected)<>Length(Actual)) then Offset:=Count+1;
  if Offset<>0 then raise EWfcPipelineWorkspaceReplay.CreateFailure(wpwrfEvidenceMismatch,
    ActionIndex,Offset,'complete outcome differs at byte '+IntToStr(Offset)+
    ' (expected '+IntToStr(Length(Expected))+', actual '+IntToStr(Length(Actual))+' bytes)');
end;
function RootLabels(const Recipe: TWfcPipelineModel; const Indices: TGraphPassIndices): TGraphPassLabels;
var I: Integer; Token: TWfcModelToken;
begin
  Result:=nil; SetLength(Result,Length(Indices));
  for I:=0 to High(Indices) do
  begin
    Token:=Recipe.PassAt(Indices[I]).LabelName;
    {$IFDEF PAS2JS}Result[I]:=String(Token);
    {$ELSE}
    Result[I]:=String(UTF8Decode(Token));
    if UTF8Encode(UnicodeString(Result[I]))<>Token then ReplayError(wpwrfInputs,-1,'root label is not representable by graph strings');
    {$ENDIF}
  end;
end;
procedure RequirePolicy(const P: TWfcPipelineWorkspacePolicy);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    function passive(o,key) {
      if(o===null || typeof o!=='object' || Array.isArray(o)) return undefined;
      let d;
      while(o!==null && !(d=Object.getOwnPropertyDescriptor(o,key))) o=Object.getPrototypeOf(o);
      return d && Object.prototype.hasOwnProperty.call(d,'value') ? d : undefined;
    }
    const v=passive(P,'Version');
    Valid=!!v && v.value===1;
    if(Valid) for(const name of ['Journal','Replacement','Outcome','Evidence','Replay']) {
      const d=passive(P,name);
      if(!d || d.value===null || typeof d.value!=='object' || Array.isArray(d.value)) {Valid=false;break;}
    }
  end;
  if not Valid then ReplayError(wpwrfInputs,-1,'complete passive version1 workspace policy required');
  {$ENDIF}
  if P.Version<>1 then ReplayError(wpwrfInputs,-1,'unsupported workspace policy');
  ValidateWfcPipelineWorkspaceJournalLimits(P.Journal);
  RequireLimits(P.Replacement,P.Outcome,P.Evidence,P.Replay);
end;
procedure RequireText(const Text: String; const Maximum: Integer);
var I: Integer;
{$IFDEF PAS2JS}Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}asm Valid=typeof Text==='string'; end;
  if not Valid then ReplayError(wpwrfInputs,-1,'primitive canonical document string required');{$ENDIF}
  if (Length(Text)=0) or (Length(Text)>Maximum) then ReplayError(wpwrfInputs,-1,'empty or oversized context text');
  if Text[Length(Text)]<>#10 then ReplayError(wpwrfInputs,-1,'complete context text with final LF required');
  for I:=1 to Length(Text) do
    if (Text[I]<>#10) and ((Ord(Text[I])<32) or (Ord(Text[I])>126)) then
      ReplayError(wpwrfInputs,-1,'canonical ASCII context text required');
end;
function OwnedRoots(const Roots: TGraphPassIndices; const PassCount,Maximum: Integer): TGraphPassIndices;
var I: Integer;
{$IFDEF PAS2JS}Valid: Boolean;{$ENDIF}
begin
  Result:=nil;
  {$IFDEF PAS2JS}
  asm
    Valid=Array.isArray(Roots) && Roots.length>0 && Roots.length<=Maximum;
    if(Valid) for(let i=0;i<Roots.length;i++) {
      const d=Object.getOwnPropertyDescriptor(Roots,String(i));
      if(!d || !Object.prototype.hasOwnProperty.call(d,'value') ||
        typeof d.value!=='number' || !Number.isInteger(d.value) || d.value<0 || d.value>=PassCount) {Valid=false;break;}
    }
  end;
  if not Valid then ReplayError(wpwrfInputs,-1,'dense passive exact in-range repair roots required');
  {$ENDIF}
  if (Length(Roots)=0) or (Length(Roots)>Maximum) then ReplayError(wpwrfInputs,-1,'explicit repair root count out of range');
  for I:=0 to High(Roots) do
  begin
    if (Roots[I]<0) or (Roots[I]>=PassCount) then ReplayError(wpwrfInputs,-1,'repair root out of range');
    if I>0 then if Roots[I]<=Roots[I-1] then ReplayError(wpwrfInputs,-1,'repair roots must be strictly ascending and unique');
  end;
  { Do not dispatch caller-controlled slice/arrayRef/species behavior. }
  SetLength(Result,Length(Roots)); for I:=0 to High(Roots) do Result[I]:=Roots[I];
end;
function CloneIndices(const Values: TGraphPassIndices): TGraphPassIndices;
var I: Integer;
begin Result:=nil; SetLength(Result,Length(Values)); for I:=0 to High(Values) do Result[I]:=Values[I]; end;
procedure CheckJournalEnvelope(const J: TWfcPipelineWorkspaceJournal;
  const L: TWfcPipelineWorkspaceJournalLimits);
begin
  if J=nil then Exit;
  if (J.RecipeCount>L.MaxRecipes) or (J.RunCount>L.MaxRuns) or
    (J.ActionCount>L.MaxActions) or (J.ContextTextBytes>L.MaxContextTextBytes) or
    (J.RootReferenceCount>L.MaxRootReferences) or (J.EvidenceTextBytes>L.MaxEvidenceTextBytes) or
    (J.EncodedTextBytes>L.MaxEncodedTextBytes) then ReplayError(wpwrfBudget,-1,'retained journal exceeds operation policy');
end;
procedure PreflightReplayWork(const J: TWfcPipelineWorkspaceJournal;
  const Limits: TWfcPipelineWorkspaceReplayLimits);
var I,Epochs,Solves,Cells,EvidenceBytes: Integer; A: TWfcPipelineWorkspaceAction;
begin
  Epochs:=0; Solves:=0; Cells:=0; EvidenceBytes:=0;
  for I:=0 to J.ActionCount-1 do
  begin
    A:=J.ActionAt(I);
    if A.Kind=wpwakBeginEpoch then
    begin
      Charge(1,Limits.MaxEpochs,I,Epochs,'epoch replay budget exceeded');
      Charge(J.BorrowRun(A.RunIndex).TotalCellCount,Limits.MaxInstantiatedCellRecords,I,Cells,'cumulative epoch cell-instance budget exceeded');
    end;
    if A.Kind in [wpwakInitial,wpwakRepair] then Charge(1,Limits.MaxSolveActions,I,Solves,'solve action budget exceeded');
    Charge(Length(A.EvidenceText),Limits.MaxEvidenceTextBytes,I,EvidenceBytes,'claimed evidence byte budget exceeded');
  end;
end;
function MinimumEvidence(const Kind: TWfcPipelineWorkspaceActionKind;
  const Run: TWfcPipelineRun): String;
var Name: String;
begin
  if Kind=wpwakBeginEpoch then begin Result:=''; Exit; end;
  if Kind=wpwakEdit then Name:='edit'
  else
  begin
    if Run.Strategy=wpssOneWay then Name:='ordinary-' else Name:='negotiated-';
    if Kind=wpwakInitial then Name:=Name+'full' else Name:=Name+'selective';
  end;
  Result:='wfc-session-evidence=1'#10'kind='+Name+#10;
end;
constructor TWfcPipelineWorkspaceReceipt.CreateOwned;
begin inherited Create; end;
destructor TWfcPipelineWorkspaceReceipt.Destroy;
begin FState.Free; FEdit.Free; FOutcome.Free; inherited Destroy; end;
function TWfcPipelineWorkspaceReceipt.BorrowPublicState: TWfcPipelineSessionPublicState;
begin Result:=FState; end;
function TWfcPipelineWorkspaceReceipt.BorrowEditOutcome: TWfcPipelineSessionEditOutcome;
begin Result:=FEdit; end;
function TWfcPipelineWorkspaceReceipt.BorrowSolveOutcome: TWfcPipelineSessionOutcome;
begin Result:=FOutcome; end;
constructor TWfcPipelineWorkspaceRepairPreview.CreateOwned;
begin inherited Create; end;
function TWfcPipelineWorkspaceRepairPreview.CopyScope: TWfcPipelineSessionScope;
begin
  Result:=Default(TWfcPipelineSessionScope); Result.ScopeAlgorithmVersion:=FScope.ScopeAlgorithmVersion;
  Result.RequestedRootIndices:=CloneIndices(FScope.RequestedRootIndices);
  Result.ActivePassIndices:=CloneIndices(FScope.ActivePassIndices);
  Result.AuthoredPassIndices:=CloneIndices(FScope.AuthoredPassIndices);
  Result.RequiredPassIndices:=CloneIndices(FScope.RequiredPassIndices);
  Result.MissingPassIndices:=CloneIndices(FScope.MissingPassIndices);
end;
constructor TWfcPipelineWorkspaceExecution.CreateOwned;
begin inherited Create; FRecipeIndex:=-1; FRunIndex:=-1; end;
destructor TWfcPipelineWorkspaceExecution.Destroy;
begin FSession.Free; FSessionRecipe.Free; FJournal.Free; inherited Destroy; end;
procedure TWfcPipelineWorkspaceExecution.RequireVerified;
begin
  if not FVerified or (FSession=nil) or (FJournal=nil) then
    ReplayError(wpwrfUnverifiedOwner,-1,'no verified retained execution');
end;
function TWfcPipelineWorkspaceExecution.GetHasCurrentOutput: Boolean;
begin RequireVerified; Result:=FSession.HasCurrentOutput; end;
function TWfcPipelineWorkspaceExecution.GetHasSuccessfulBaseline: Boolean;
begin RequireVerified; Result:=FSession.HasSuccessfulBaseline; end;
function TWfcPipelineWorkspaceExecution.GetRevision: Integer;
begin RequireVerified; Result:=FSession.Revision; end;
function TWfcPipelineWorkspaceExecution.CopyCanonicalJournal: String;
begin RequireVerified; Result:=FJournal.CopyCanonicalText; end;
function TWfcPipelineWorkspaceExecution.CopyPublicState: TWfcPipelineSessionPublicState;
begin RequireVerified; Result:=FSession.CopyPublicState; end;
function TWfcPipelineWorkspaceExecution.CopyLastSuccessfulState: TWfcPipelineSessionPublicState;
begin RequireVerified; Result:=FSession.CopyLastSuccessfulState; end;
function TWfcPipelineWorkspaceExecution.CopyAppliedRun: TWfcPipelineRun;
begin RequireVerified; Result:=FSession.CopyAppliedRun; end;
function ReplayWfcPipelineWorkspace(const Journal: TWfcPipelineWorkspaceJournal;
  const JournalLimits: TWfcPipelineWorkspaceJournalLimits;
  const ReplacementLimits: TWfcPipelineReplacementLimits;
  const OutcomeLimits: TWfcPipelineSessionOutcomeLimits;
  const EvidenceLimits: TWfcPipelineSessionEvidenceLimits;
  const ReplayLimits: TWfcPipelineWorkspaceReplayLimits): TWfcPipelineWorkspaceExecution;
var Candidate: TWfcPipelineWorkspaceExecution; Action: TWfcPipelineWorkspaceAction;
  I,Epochs,Solves,Cells,EvidenceBytes: Integer; Run: TWfcPipelineRun;
  O: TWfcPipelineSessionOutcome; Edit: TWfcPipelineSessionEditOutcome;
  Plan: TWfcPipelineSessionRepairPlan; LocalEvidenceLimits: TWfcPipelineSessionEvidenceLimits;
  Actual: String; Labels: TGraphPassLabels;
begin
  Result:=nil; RequireLimits(ReplacementLimits,OutcomeLimits,EvidenceLimits,ReplayLimits);
  if Journal=nil then ReplayError(wpwrfInputs,-1,'journal is required');
  Candidate:=TWfcPipelineWorkspaceExecution.CreateOwned;
  try
    { The returned execution owns independently decoded COMPLETE contexts. The
      input journal is borrowed for this call only, never used by signature. }
    Candidate.FJournal:=DecodeWfcPipelineWorkspaceJournalText(Journal.CopyCanonicalText,JournalLimits);
    if Candidate.FJournal.ActionCount<1 then ReplayError(wpwrfInputs,-1,'journal has no epoch');
    Epochs:=0; Solves:=0; Cells:=0; EvidenceBytes:=0;
    for I:=0 to Candidate.FJournal.ActionCount-1 do
    begin
      Action:=Candidate.FJournal.ActionAt(I);
      case Action.Kind of
        wpwakBeginEpoch:begin
          Charge(1,ReplayLimits.MaxEpochs,I,Epochs,'epoch replay budget exceeded');
          Charge(Candidate.FJournal.BorrowRun(Action.RunIndex).TotalCellCount,
            ReplayLimits.MaxInstantiatedCellRecords,I,Cells,'cumulative epoch cell-instance budget exceeded');
        end;
        wpwakInitial,wpwakRepair:Charge(1,ReplayLimits.MaxSolveActions,I,Solves,'solve action budget exceeded');
      end;
      Charge(Length(Action.EvidenceText),ReplayLimits.MaxEvidenceTextBytes,I,EvidenceBytes,'claimed evidence byte budget exceeded');
    end;
    EvidenceBytes:=0;
    for I:=0 to Candidate.FJournal.ActionCount-1 do
    begin
      Action:=Candidate.FJournal.ActionAt(I); Run:=Candidate.FJournal.BorrowRun(Action.RunIndex);
      O:=nil; Edit:=nil; Plan:=nil;
      try
        if Action.Kind=wpwakBeginEpoch then
        begin
          FreeAndNil(Candidate.FSession);
          FreeAndNil(Candidate.FSessionRecipe);
          Candidate.FRecipeIndex:=Candidate.FJournal.RunTextAt(Action.RunIndex).RecipeIndex;
          Candidate.FSessionRecipe:=Candidate.FJournal.CopyRecipe(Candidate.FRecipeIndex);
          Candidate.FSession:=TWfcPipelinePreparedSession.Create(
            Candidate.FSessionRecipe,Run,ReplacementLimits,OutcomeLimits);
          Inc(Candidate.FEpochCount); Actual:='';
        end
        else
        begin
          LocalEvidenceLimits:=EvidenceLimits;
          if LocalEvidenceLimits.MaxTextBytes>ReplayLimits.MaxEvidenceTextBytes-EvidenceBytes then
            LocalEvidenceLimits.MaxTextBytes:=ReplayLimits.MaxEvidenceTextBytes-EvidenceBytes;
          if LocalEvidenceLimits.MaxTextBytes<1 then ReplayError(wpwrfBudget,I,'no remaining evidence byte budget');
          case Action.Kind of
            wpwakEdit:begin
              Edit:=Candidate.FSession.ApplyInputs(Run);
              Actual:=EncodeWfcPipelineSessionEditEvidence(Edit,LocalEvidenceLimits);
            end;
            wpwakInitial:begin
              O:=Candidate.FSession.ExecuteInitial;
              Actual:=EncodeWfcPipelineSessionOutcomeEvidence(O,LocalEvidenceLimits);
            end;
            wpwakRepair:begin
              Labels:=RootLabels(Candidate.FJournal.BorrowRecipe(Candidate.FRecipeIndex),Action.RequestedRootIndices);
              Plan:=Candidate.FSession.PlanRepair(Run,Labels);
              if not Plan.CanExecute then ReplayError(wpwrfScope,I,'recorded repair lacks a successful baseline or sufficient explicit scope');
              O:=Candidate.FSession.ExecuteRepair(Plan);
              Actual:=EncodeWfcPipelineSessionOutcomeEvidence(O,LocalEvidenceLimits);
            end;
          else ReplayError(wpwrfInputs,I,'unknown journal action'); end;
        end;
        Charge(Length(Actual),ReplayLimits.MaxEvidenceTextBytes,I,EvidenceBytes,'actual evidence byte budget exceeded');
        CompareEvidence(Action.EvidenceText,Actual,I);
        Candidate.FRunIndex:=Action.RunIndex; Inc(Candidate.FVerifiedActions);
      finally Plan.Free; Edit.Free; O.Free; end;
    end;
    if (Candidate.FSession=nil) or not Candidate.FSession.Usable then
      ReplayError(wpwrfUnverifiedOwner,-1,'replay did not retain a usable session');
    Candidate.FVerified:=True;
    Result:=Candidate; Candidate:=nil;
  finally Candidate.Free; end;
end;
destructor TWfcPipelineWorkspaceSlot.Destroy;
begin FCurrent.Free; inherited Destroy; end;
function TWfcPipelineWorkspaceSlot.GetHasExecution: Boolean;
begin Result:=FCurrent<>nil; end;
procedure TWfcPipelineWorkspaceSlot.RequirePublication(const Expected: Integer;
  const WillPublish: Boolean);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}asm Valid=typeof Expected==='number' && Number.isInteger(Expected); end;
  if not Valid then ReplayError(wpwrfStalePublication,-1,'publication revision must be an Integer');{$ENDIF}
  if (Expected<0) or (Expected<>FPublicationRevision) then ReplayError(wpwrfStalePublication,-1,'stale publication revision');
  if WillPublish and (FPublicationRevision=High(Integer)) then ReplayError(wpwrfBudget,-1,'publication revision exhausted');
end;
procedure TWfcPipelineWorkspaceSlot.Restore(const Journal: TWfcPipelineWorkspaceJournal;
  const JournalLimits: TWfcPipelineWorkspaceJournalLimits;
  const ReplacementLimits: TWfcPipelineReplacementLimits;
  const OutcomeLimits: TWfcPipelineSessionOutcomeLimits;
  const EvidenceLimits: TWfcPipelineSessionEvidenceLimits;
  const ReplayLimits: TWfcPipelineWorkspaceReplayLimits;
  const ExpectedPublicationRevision: Integer);
var Candidate,Old: TWfcPipelineWorkspaceExecution;
begin
  RequirePublication(ExpectedPublicationRevision,True);
  Candidate:=ReplayWfcPipelineWorkspace(Journal,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,ReplayLimits);
  { No user callbacks or concurrent mutation are supported. No published owner
    was touched by decoding, replay, capture, budget failure or comparison. }
  Old:=FCurrent; FCurrent:=Candidate; Inc(FPublicationRevision); Old.Free;
end;
function TWfcPipelineWorkspaceSlot.Author(const Kind: TWfcPipelineWorkspaceActionKind;
  const RecipeText,RunText: String; const Roots: TGraphPassIndices;
  const Policy: TWfcPipelineWorkspacePolicy;
  const ExpectedPublicationRevision: Integer): TWfcPipelineWorkspaceReceipt;
var OldJournal,Draft,Finished: TWfcPipelineWorkspaceJournal;
  Recipes: TWfcPipelineWorkspaceRecipeTexts; Runs: TWfcPipelineWorkspaceRunTexts;
  Actions: TWfcPipelineWorkspaceActions; CleanRoots: TGraphPassIndices;
  Candidate,Old: TWfcPipelineWorkspaceExecution;
  NewRecipe: TWfcPipelineModel; NewRun: TWfcPipelineRun;
  Receipt: TWfcPipelineWorkspaceReceipt; Edit: TWfcPipelineSessionEditOutcome;
  Outcome: TWfcPipelineSessionOutcome; Plan: TWfcPipelineSessionRepairPlan;
  LocalEvidence: TWfcPipelineSessionEvidenceLimits;
  RC,UC,AC,ContextBytes,RootCount,EvidenceBytes,I,RecipeIndex,RunIndex: Integer;
  Text: String;
begin
  Result:=nil; RequirePublication(ExpectedPublicationRevision,True); RequirePolicy(Policy);
  if (Kind<>wpwakBeginEpoch) and (FCurrent=nil) then ReplayError(wpwrfUnverifiedOwner,-1,'workspace slot is empty');
  OldJournal:=nil; if FCurrent<>nil then begin FCurrent.RequireVerified; OldJournal:=FCurrent.FJournal; end;
  CheckJournalEnvelope(OldJournal,Policy.Journal);
  RC:=0; UC:=0; AC:=0; ContextBytes:=0; RootCount:=0; EvidenceBytes:=0;
  if OldJournal<>nil then
  begin
    RC:=OldJournal.RecipeCount; UC:=OldJournal.RunCount; AC:=OldJournal.ActionCount;
    ContextBytes:=OldJournal.ContextTextBytes; RootCount:=OldJournal.RootReferenceCount;
    EvidenceBytes:=OldJournal.EvidenceTextBytes;
  end;
  Charge(1,Policy.Journal.MaxActions,-1,AC,'extended action count budget exceeded');
  if Kind=wpwakBeginEpoch then
  begin
    RequireText(RecipeText,Policy.Journal.MaxContextTextBytes);
    Charge(Length(RecipeText),Policy.Journal.MaxContextTextBytes,-1,ContextBytes,'extended recipe text budget exceeded');
    Charge(1,Policy.Journal.MaxRecipes,-1,RC,'extended recipe count budget exceeded');
  end;
  if Kind<>wpwakInitial then
  begin
    RequireText(RunText,Policy.Journal.MaxContextTextBytes);
    Charge(Length(RunText),Policy.Journal.MaxContextTextBytes,-1,ContextBytes,'extended run text budget exceeded');
    Charge(1,Policy.Journal.MaxRuns,-1,UC,'extended run count budget exceeded');
  end;
  CleanRoots:=nil;
  if Kind=wpwakRepair then
  begin
    CleanRoots:=OwnedRoots(Roots,FCurrent.FSessionRecipe.PassCount,Policy.Journal.MaxRootReferences);
    Charge(Length(CleanRoots),Policy.Journal.MaxRootReferences,-1,RootCount,'extended root reference budget exceeded');
  end;
  Candidate:=nil; Draft:=nil; Finished:=nil; NewRecipe:=nil; NewRun:=nil;
  Receipt:=nil; Edit:=nil; Outcome:=nil; Plan:=nil;
  try
    if Kind=wpwakBeginEpoch then
    begin
      NewRecipe:=DecodeWfcPipelineModelText(RecipeText);
      if EncodeWfcPipelineModelText(NewRecipe)<>RecipeText then ReplayError(wpwrfInputs,-1,'recipe must be exact canonical text');
      RecipeIndex:=RC-1; NewRun:=DecodeWfcPipelineRunText(RunText,NewRecipe);
    end
    else
    begin
      RecipeIndex:=FCurrent.FRecipeIndex;
      if Kind=wpwakInitial then NewRun:=FCurrent.CopyAppliedRun
      else NewRun:=DecodeWfcPipelineRunText(RunText,FCurrent.FSessionRecipe);
    end;
    if Kind<>wpwakInitial then
    begin
      if EncodeWfcPipelineRunText(NewRun)<>RunText then ReplayError(wpwrfInputs,-1,'run must be exact canonical text');
      RunIndex:=UC-1;
    end
    else RunIndex:=FCurrent.FRunIndex;
    SetLength(Recipes,RC); SetLength(Runs,UC); SetLength(Actions,AC);
    if OldJournal<>nil then
    begin
      for I:=0 to OldJournal.RecipeCount-1 do Recipes[I]:=OldJournal.RecipeTextAt(I);
      for I:=0 to OldJournal.RunCount-1 do Runs[I]:=OldJournal.RunTextAt(I);
      for I:=0 to OldJournal.ActionCount-1 do Actions[I]:=OldJournal.ActionAt(I);
    end;
    if Kind=wpwakBeginEpoch then Recipes[RecipeIndex]:=RecipeText;
    if Kind<>wpwakInitial then begin Runs[RunIndex].RecipeIndex:=RecipeIndex; Runs[RunIndex].Text:=RunText; end;
    Actions[AC-1].Kind:=Kind; Actions[AC-1].RunIndex:=RunIndex;
    Actions[AC-1].RequestedRootIndices:=CleanRoots;
    { INTERNAL unverified lower bound only. This draft never escapes, is never
      replayed, and never substitutes for the complete actual evidence below.
      Reusing the static journal validator enforces exact epoch/ordered-input
      transitions and the known full outer envelope before any graph executes. }
    Actions[AC-1].EvidenceText:=MinimumEvidence(Kind,NewRun);
    Draft:=TWfcPipelineWorkspaceJournal.Create(Recipes,Runs,Actions,Policy.Journal);
    PreflightReplayWork(Draft,Policy.Replay);
    LocalEvidence:=Policy.Evidence;
    if Kind<>wpwakBeginEpoch then
    begin
      if LocalEvidence.MaxTextBytes>Policy.Journal.MaxEvidenceTextBytes-EvidenceBytes then
        LocalEvidence.MaxTextBytes:=Policy.Journal.MaxEvidenceTextBytes-EvidenceBytes;
      if LocalEvidence.MaxTextBytes>Policy.Replay.MaxEvidenceTextBytes-EvidenceBytes then
        LocalEvidence.MaxTextBytes:=Policy.Replay.MaxEvidenceTextBytes-EvidenceBytes;
      if LocalEvidence.MaxTextBytes<Length(Actions[AC-1].EvidenceText) then
        ReplayError(wpwrfBudget,AC-1,'no remaining complete evidence envelope');
    end;
    if OldJournal=nil then Candidate:=TWfcPipelineWorkspaceExecution.CreateOwned
    else Candidate:=ReplayWfcPipelineWorkspace(OldJournal,Policy.Journal,
      Policy.Replacement,Policy.Outcome,Policy.Evidence,Policy.Replay);
    Candidate.FVerified:=False;
    case Kind of
      wpwakBeginEpoch:begin
        FreeAndNil(Candidate.FSession); FreeAndNil(Candidate.FSessionRecipe);
        Candidate.FSessionRecipe:=NewRecipe; NewRecipe:=nil;
        Candidate.FSession:=TWfcPipelinePreparedSession.Create(Candidate.FSessionRecipe,
          NewRun,Policy.Replacement,Policy.Outcome);
        Inc(Candidate.FEpochCount); Text:='';
      end;
      wpwakEdit:begin
        Edit:=Candidate.FSession.ApplyInputs(NewRun);
        Text:=EncodeWfcPipelineSessionEditEvidence(Edit,LocalEvidence);
      end;
      wpwakInitial:begin
        Outcome:=Candidate.FSession.ExecuteInitial;
        Text:=EncodeWfcPipelineSessionOutcomeEvidence(Outcome,LocalEvidence);
      end;
      wpwakRepair:begin
        Plan:=Candidate.FSession.PlanRepair(NewRun,RootLabels(Candidate.FSessionRecipe,CleanRoots));
        if not Plan.CanExecute then ReplayError(wpwrfScope,AC-1,'repair lacks a successful baseline or sufficient explicit scope');
        Outcome:=Candidate.FSession.ExecuteRepair(Plan);
        Text:=EncodeWfcPipelineSessionOutcomeEvidence(Outcome,LocalEvidence);
      end;
    end;
    if not Candidate.FSession.Usable then ReplayError(wpwrfUnverifiedOwner,AC-1,'action did not retain a usable session');
    Actions[AC-1].EvidenceText:=Text;
    Finished:=TWfcPipelineWorkspaceJournal.Create(Recipes,Runs,Actions,Policy.Journal);
    PreflightReplayWork(Finished,Policy.Replay);
    { The independent recipe lease makes this safe: no session reference points
      into either journal. Retain THIS actual action-producing session. }
    Candidate.FJournal.Free; Candidate.FJournal:=Finished; Finished:=nil;
    Candidate.FRecipeIndex:=RecipeIndex; Candidate.FRunIndex:=RunIndex;
    Candidate.FVerifiedActions:=AC; Candidate.FVerified:=True;
    Receipt:=TWfcPipelineWorkspaceReceipt.CreateOwned;
    Receipt.FKind:=Kind; Receipt.FActionIndex:=AC-1; Receipt.FRecipeIndex:=RecipeIndex;
    Receipt.FRunIndex:=RunIndex; Receipt.FEpochCount:=Candidate.FEpochCount;
    Receipt.FPublicationRevision:=FPublicationRevision+1;
    Receipt.FSessionRevision:=Candidate.Revision; Receipt.FEvidenceText:=Text;
    Receipt.FHasCurrentOutput:=Candidate.HasCurrentOutput;
    Receipt.FHasSuccessfulBaseline:=Candidate.HasSuccessfulBaseline;
    Receipt.FState:=Candidate.CopyPublicState;
    Receipt.FEdit:=Edit; Edit:=nil; Receipt.FOutcome:=Outcome; Outcome:=nil;
    RequirePublication(ExpectedPublicationRevision,True);
    Old:=FCurrent; FCurrent:=Candidate; Candidate:=nil; Inc(FPublicationRevision);
    Old.Free; Result:=Receipt; Receipt:=nil;
  finally
    Plan.Free; Receipt.Free; Outcome.Free; Edit.Free; NewRun.Free; NewRecipe.Free;
    Finished.Free; Draft.Free; Candidate.Free;
  end;
end;
function TWfcPipelineWorkspaceSlot.BeginEpoch(const RecipeText,InitialRunText: String;
  const Policy: TWfcPipelineWorkspacePolicy;
  const ExpectedPublicationRevision: Integer): TWfcPipelineWorkspaceReceipt;
begin Result:=Author(wpwakBeginEpoch,RecipeText,InitialRunText,nil,Policy,ExpectedPublicationRevision); end;
function TWfcPipelineWorkspaceSlot.ApplyInputs(const CompleteRunText: String;
  const Policy: TWfcPipelineWorkspacePolicy;
  const ExpectedPublicationRevision: Integer): TWfcPipelineWorkspaceReceipt;
begin Result:=Author(wpwakEdit,'',CompleteRunText,nil,Policy,ExpectedPublicationRevision); end;
function TWfcPipelineWorkspaceSlot.ExecuteInitial(const Policy: TWfcPipelineWorkspacePolicy;
  const ExpectedPublicationRevision: Integer): TWfcPipelineWorkspaceReceipt;
begin Result:=Author(wpwakInitial,'','',nil,Policy,ExpectedPublicationRevision); end;
function TWfcPipelineWorkspaceSlot.ExecuteRepair(const CompleteRunText: String;
  const RootIndices: TGraphPassIndices; const Policy: TWfcPipelineWorkspacePolicy;
  const ExpectedPublicationRevision: Integer): TWfcPipelineWorkspaceReceipt;
begin Result:=Author(wpwakRepair,'',CompleteRunText,RootIndices,Policy,ExpectedPublicationRevision); end;
function TWfcPipelineWorkspaceSlot.PreviewRepair(const CompleteRunText: String;
  const RootIndices: TGraphPassIndices; const Policy: TWfcPipelineWorkspacePolicy;
  const ExpectedPublicationRevision: Integer): TWfcPipelineWorkspaceRepairPreview;
var Candidate: TWfcPipelineWorkspaceExecution; Run: TWfcPipelineRun;
  Plan: TWfcPipelineSessionRepairPlan; Preview: TWfcPipelineWorkspaceRepairPreview;
  Roots: TGraphPassIndices;
begin
  Result:=nil; RequirePublication(ExpectedPublicationRevision,False); RequirePolicy(Policy);
  if FCurrent=nil then ReplayError(wpwrfUnverifiedOwner,-1,'workspace slot is empty');
  FCurrent.RequireVerified; CheckJournalEnvelope(FCurrent.FJournal,Policy.Journal);
  PreflightReplayWork(FCurrent.FJournal,Policy.Replay);
  RequireText(CompleteRunText,Policy.Journal.MaxContextTextBytes);
  Roots:=OwnedRoots(RootIndices,FCurrent.FSessionRecipe.PassCount,Policy.Journal.MaxRootReferences);
  Candidate:=nil; Run:=nil; Plan:=nil; Preview:=nil;
  try
    Run:=DecodeWfcPipelineRunText(CompleteRunText,FCurrent.FSessionRecipe);
    if EncodeWfcPipelineRunText(Run)<>CompleteRunText then ReplayError(wpwrfInputs,-1,'run must be exact canonical text');
    Candidate:=ReplayWfcPipelineWorkspace(FCurrent.FJournal,Policy.Journal,
      Policy.Replacement,Policy.Outcome,Policy.Evidence,Policy.Replay);
    Plan:=Candidate.FSession.PlanRepair(Run,RootLabels(Candidate.FSessionRecipe,Roots));
    Preview:=TWfcPipelineWorkspaceRepairPreview.CreateOwned;
    Preview.FScope:=Plan.CopyScope; Preview.FCanExecute:=Plan.CanExecute;
    Preview.FMissingBaseline:=Plan.MissingBaseline; Preview.FRunText:=CompleteRunText;
    Preview.FPublicationRevision:=FPublicationRevision; Preview.FSessionRevision:=Candidate.Revision;
    RequirePublication(ExpectedPublicationRevision,False);
    Result:=Preview; Preview:=nil;
  finally Preview.Free; Plan.Free; Run.Free; Candidate.Free; end;
end;
function TWfcPipelineWorkspaceSlot.CopyCanonicalJournal: String;
begin
  if FCurrent=nil then ReplayError(wpwrfUnverifiedOwner,-1,'workspace slot is empty');
  Result:=FCurrent.CopyCanonicalJournal;
end;
function TWfcPipelineWorkspaceSlot.CopyPublicState: TWfcPipelineSessionPublicState;
begin
  if FCurrent=nil then ReplayError(wpwrfUnverifiedOwner,-1,'workspace slot is empty');
  Result:=FCurrent.CopyPublicState;
end;
function TWfcPipelineWorkspaceSlot.CopyLastSuccessfulState: TWfcPipelineSessionPublicState;
begin
  if FCurrent=nil then ReplayError(wpwrfUnverifiedOwner,-1,'workspace slot is empty');
  Result:=FCurrent.CopyLastSuccessfulState;
end;
function TWfcPipelineWorkspaceSlot.CopyAppliedRun: TWfcPipelineRun;
begin
  if FCurrent=nil then ReplayError(wpwrfUnverifiedOwner,-1,'workspace slot is empty');
  Result:=FCurrent.CopyAppliedRun;
end;
function TWfcPipelineWorkspaceSlot.CopyCurrentRecipe: TWfcPipelineModel;
begin
  if FCurrent=nil then ReplayError(wpwrfUnverifiedOwner,-1,'workspace slot is empty');
  FCurrent.RequireVerified; Result:=FCurrent.FJournal.CopyRecipe(FCurrent.FRecipeIndex);
end;
function TWfcPipelineWorkspaceSlot.GetHasCurrentOutput: Boolean;
begin Result:=(FCurrent<>nil) and FCurrent.HasCurrentOutput; end;
function TWfcPipelineWorkspaceSlot.GetHasSuccessfulBaseline: Boolean;
begin Result:=(FCurrent<>nil) and FCurrent.HasSuccessfulBaseline; end;
function TWfcPipelineWorkspaceSlot.GetSessionRevision: Integer;
begin
  if FCurrent=nil then ReplayError(wpwrfUnverifiedOwner,-1,'workspace slot is empty');
  Result:=FCurrent.Revision;
end;
function TWfcPipelineWorkspaceSlot.GetCurrentRecipeIndex: Integer;
begin
  if FCurrent=nil then Result:=-1 else Result:=FCurrent.FRecipeIndex;
end;
function TWfcPipelineWorkspaceSlot.GetCurrentRunIndex: Integer;
begin
  if FCurrent=nil then Result:=-1 else Result:=FCurrent.FRunIndex;
end;
end.
