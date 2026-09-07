{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Immutable, graph-free journal with explicitly unverified claims. }
unit wfc_pipeline_workspace_journal;
{$mode delphi}{$H+}
interface
uses SysUtils,wfc,wfc_pipeline_model,wfc_pipeline_run,
  wfc_pipeline_workspace_context;
const WFC_PIPELINE_WORKSPACE_JOURNAL_VERSION = 1;
type
  EWfcPipelineWorkspaceJournal = class(Exception);
  TWfcPipelineWorkspaceActionKind = (wpwakBeginEpoch,wpwakEdit,wpwakInitial,wpwakRepair);
  TWfcPipelineWorkspaceVerification = (wpwvUnverifiedClaims);
  TWfcPipelineWorkspaceAction = record
    Kind: TWfcPipelineWorkspaceActionKind;
    RunIndex: Integer;
    RequestedRootIndices: TGraphPassIndices;
    EvidenceText: String;
  end;
  TWfcPipelineWorkspaceActions = array of TWfcPipelineWorkspaceAction;
  TWfcPipelineWorkspaceJournalLimits = record
    Version: Integer;
    MaxRecipes,MaxRuns,MaxContextTextBytes,MaxActions: Integer;
    MaxRootReferences,MaxEvidenceTextBytes,MaxEncodedTextBytes: Integer;
  end;
  TWfcPipelineWorkspaceJournal = class
  strict private
    FContexts: TWfcPipelineWorkspaceContexts;
    FActions: TWfcPipelineWorkspaceActions;
    FLimits: TWfcPipelineWorkspaceJournalLimits;
    FCanonicalText: String;
    FRootReferences,FEvidenceTextBytes: Integer;
    FInitialized: Boolean;
    procedure RequireInitialized;
    procedure CheckActionIndex(const Index: Integer);
    function GetRecipeCount: Integer;
    function GetRunCount: Integer;
    function GetActionCount: Integer;
    function GetContextTextBytes: Integer;
    function GetRootReferenceCount: Integer;
    function GetEvidenceTextBytes: Integer;
    function GetEncodedTextBytes: Integer;
    function GetVerification: TWfcPipelineWorkspaceVerification;
  public
    constructor Create(const RecipeTexts: TWfcPipelineWorkspaceRecipeTexts;
      const RunTexts: TWfcPipelineWorkspaceRunTexts;
      const Actions: TWfcPipelineWorkspaceActions;
      const Limits: TWfcPipelineWorkspaceJournalLimits);
    destructor Destroy; override;
    function RecipeTextAt(const Index: Integer): String;
    function RunTextAt(const Index: Integer): TWfcPipelineWorkspaceRunText;
    { Immutable borrows: keep this journal alive and never Free the returned
      objects. CopyRecipe/CopyRun instead transfer independent ownership. }
    function BorrowRecipe(const Index: Integer): TWfcPipelineModel;
    function BorrowRun(const Index: Integer): TWfcPipelineRun;
    function CopyRecipe(const Index: Integer): TWfcPipelineModel;
    function CopyRun(const Index: Integer): TWfcPipelineRun;
    function ActionAt(const Index: Integer): TWfcPipelineWorkspaceAction;
    function CopyActions: TWfcPipelineWorkspaceActions;
    function CopyLimits: TWfcPipelineWorkspaceJournalLimits;
    function CopyCanonicalText: String;
    property RecipeCount: Integer read GetRecipeCount;
    property RunCount: Integer read GetRunCount;
    property ActionCount: Integer read GetActionCount;
    property ContextTextBytes: Integer read GetContextTextBytes;
    property RootReferenceCount: Integer read GetRootReferenceCount;
    property EvidenceTextBytes: Integer read GetEvidenceTextBytes;
    property EncodedTextBytes: Integer read GetEncodedTextBytes;
    property Verification: TWfcPipelineWorkspaceVerification read GetVerification;
  end;
procedure ValidateWfcPipelineWorkspaceJournalLimits(
  const Limits: TWfcPipelineWorkspaceJournalLimits);
function WfcPipelineWorkspaceActionName(const Kind: TWfcPipelineWorkspaceActionKind): String;
implementation
uses wfc_lattice,wfc_model,wfc_text_codec;
const Artifact='WFC workspace journal';
type
  TJournalWriter = class
  private
    FMaximum,FBytes,FCount: Integer;
    FWriting: Boolean;
    FLines: TWfcTextLines;
    procedure ChargeLine(const Count: Integer);
  public
    constructor Create(const Maximum: Integer);
    procedure Line(const Value: String);
    procedure Document(const Prefix,Text: String);
    procedure Number(const Prefix: String; const Value: Integer);
    procedure BeginWriting;
    function Finish: String;
  end;
procedure JournalError(const Detail: String);
begin raise EWfcPipelineWorkspaceJournal.Create('workspace journal: '+Detail); end;
procedure RequireInteger(const Value,Minimum,Maximum: Integer; const Name: String);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}asm Valid=typeof Value==='number' && Number.isInteger(Value); end;
  if not Valid then JournalError(Name+' must be an exact finite Integer');{$ENDIF}
  if (Value<Minimum) or (Value>Maximum) then JournalError(Name+' is out of range');
end;
procedure Charge(var Used: Integer; const Count,Maximum: Integer; const Name: String);
begin
  if (Count<0) or (Count>Maximum) or (Used>Maximum-Count) then JournalError(Name+' budget exceeded');
  Inc(Used,Count);
end;
procedure ValidateWfcPipelineWorkspaceJournalLimits(const Limits: TWfcPipelineWorkspaceJournalLimits);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    Valid=Limits!==null && typeof Limits==='object' && !Array.isArray(Limits);
    if(Valid) for(const key of ['Version','MaxRecipes','MaxRuns','MaxContextTextBytes',
      'MaxActions','MaxRootReferences','MaxEvidenceTextBytes','MaxEncodedTextBytes']) {
      let p=Limits,d;
      while(p!==null && !(d=Object.getOwnPropertyDescriptor(p,key))) p=Object.getPrototypeOf(p);
      if(!d || !Object.prototype.hasOwnProperty.call(d,'value') ||
        typeof d.value!=='number' || !Number.isInteger(d.value) ||
        d.value<1 || d.value>2147483647) {Valid=false;break;}
    }
  end;
  if not Valid then JournalError('limits require passive positive Integer fields');
  {$ENDIF}
  RequireInteger(Limits.Version,1,1,'limits version');
  RequireInteger(Limits.MaxRecipes,1,High(Integer),'recipe limit');
  RequireInteger(Limits.MaxRuns,1,High(Integer),'run limit');
  RequireInteger(Limits.MaxContextTextBytes,1,High(Integer),'context text limit');
  RequireInteger(Limits.MaxActions,1,High(Integer),'action limit');
  RequireInteger(Limits.MaxRootReferences,1,High(Integer),'root reference limit');
  RequireInteger(Limits.MaxEvidenceTextBytes,1,High(Integer),'evidence text limit');
  RequireInteger(Limits.MaxEncodedTextBytes,1,High(Integer),'encoded text limit');
end;
procedure GuardInputs(const Recipes: TWfcPipelineWorkspaceRecipeTexts;
  const Runs: TWfcPipelineWorkspaceRunTexts; const Actions: TWfcPipelineWorkspaceActions;
  const Limits: TWfcPipelineWorkspaceJournalLimits);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  ValidateWfcPipelineWorkspaceJournalLimits(Limits);
  {$IFDEF PAS2JS}
  asm
    function passive(o,key) {
      if(o===null || typeof o!=='object' || Array.isArray(o)) return undefined;
      let d;
      while(o!==null && !(d=Object.getOwnPropertyDescriptor(o,key))) o=Object.getPrototypeOf(o);
      return d && Object.prototype.hasOwnProperty.call(d,'value') ? d : undefined;
    }
    function slot(a,i) {
      const d=Object.getOwnPropertyDescriptor(a,String(i));
      return d && Object.prototype.hasOwnProperty.call(d,'value') ? d : undefined;
    }
    function index(d,max) {return d && typeof d.value==='number' && Number.isInteger(d.value) && d.value>=0 && d.value<max;}
    Valid=Array.isArray(Recipes) && Recipes.length>0 && Recipes.length<=Limits.MaxRecipes &&
      Array.isArray(Runs) && Runs.length>0 && Runs.length<=Limits.MaxRuns &&
      Array.isArray(Actions) && Actions.length>0 && Actions.length<=Limits.MaxActions;
    if(Valid) for(let i=0;i<Recipes.length;i++) {
      const d=slot(Recipes,i); if(!d || typeof d.value!=='string') {Valid=false;break;}
    }
    if(Valid) for(let i=0;i<Runs.length;i++) {
      const d=slot(Runs,i), r=d && passive(d.value,'RecipeIndex'), t=d && passive(d.value,'Text');
      if(!index(r,Recipes.length) || !t || typeof t.value!=='string') {Valid=false;break;}
    }
    let roots=0;
    if(Valid) for(let i=0;i<Actions.length;i++) {
      const d=slot(Actions,i), k=d && passive(d.value,'Kind'), r=d && passive(d.value,'RunIndex'),
        q=d && passive(d.value,'RequestedRootIndices'), e=d && passive(d.value,'EvidenceText');
      if(!index(k,4) || !index(r,Runs.length) || !q || !Array.isArray(q.value) ||
        q.value.length>Limits.MaxRootReferences-roots || !e || typeof e.value!=='string') {Valid=false;break;}
      roots+=q.value.length;
      for(let j=0;j<q.value.length;j++) if(!index(slot(q.value,j),2147483648)) {Valid=false;break;}
      if(!Valid) break;
    }
  end;
  if not Valid then JournalError('inputs require complete passive dense typed arrays');
  {$ENDIF}
  RequireInteger(Length(Recipes),1,Limits.MaxRecipes,'recipe count');
  RequireInteger(Length(Runs),1,Limits.MaxRuns,'run count');
  RequireInteger(Length(Actions),1,Limits.MaxActions,'action count');
end;
procedure CheckDocument(const Text: String; const AllowEmpty: Boolean; const Name: String);
var I: Integer;
begin
  if Text='' then begin if not AllowEmpty then JournalError(Name+' is empty'); Exit; end;
  if Text[Length(Text)]<>#10 then JournalError(Name+' requires a final LF');
  for I:=1 to Length(Text) do
  begin
    if (Text[I]<>#10) and ((Ord(Text[I])<32) or (Ord(Text[I])>126)) then
      JournalError(Name+' requires printable ASCII fields and LF');
    if (Text[I]=#10) and ((I=1) or (Text[I-1]=#10)) then JournalError(Name+' has a blank line');
  end;
end;
function WfcPipelineWorkspaceActionName(const Kind: TWfcPipelineWorkspaceActionKind): String;
begin
  RequireInteger(Ord(Kind),0,3,'action kind');
  case Kind of
    wpwakBeginEpoch:Result:='begin-epoch'; wpwakEdit:Result:='edit';
    wpwakInitial:Result:='initial'; wpwakRepair:Result:='repair';
  end;
end;
function Unreserved(const Ch: Char): Boolean;
begin Result:=(Ch in ['A'..'Z','a'..'z','0'..'9','-','.','_','~']); end;
constructor TJournalWriter.Create(const Maximum: Integer);
begin inherited Create; FMaximum:=Maximum; end;
procedure TJournalWriter.ChargeLine(const Count: Integer);
begin
  Charge(FBytes,Count,FMaximum,'encoded text'); Charge(FBytes,1,FMaximum,'encoded text');
  if FCount=High(Integer) then JournalError('encoded line count exceeds Integer');
  if FWriting and (FCount>=Length(FLines)) then JournalError('encoded line preflight changed');
  Inc(FCount);
end;
procedure TJournalWriter.Line(const Value: String);
begin ChargeLine(Length(Value)); if FWriting then FLines[FCount-1]:=Value; end;
procedure TJournalWriter.Document(const Prefix,Text: String);
var I,Bytes: Integer; Encoded: String;
begin
  { Exact ASCII RFC3986 byte measurement, matching the maintained token codec.
    No encoded token or complete line is allocated in this counting phase. }
  Bytes:=Length(Prefix);
  for I:=1 to Length(Text) do
    if Unreserved(Text[I]) then Charge(Bytes,1,FMaximum,'encoded document')
    else Charge(Bytes,3,FMaximum,'encoded document');
  ChargeLine(Bytes);
  if FWriting then
  begin
    Encoded:=WfcTextEncodeToken(TWfcModelToken(Text),Artifact);
    if Length(Encoded)<>Bytes-Length(Prefix) then JournalError('token measurement differs from maintained codec');
    FLines[FCount-1]:=Prefix+Encoded;
  end;
end;
procedure TJournalWriter.Number(const Prefix: String; const Value: Integer);
begin Line(Prefix+IntToStr(Value)); end;
procedure TJournalWriter.BeginWriting;
begin
  if FWriting then JournalError('writer phase already entered');
  SetLength(FLines,FCount); FCount:=0; FBytes:=0; FWriting:=True;
end;
function TJournalWriter.Finish: String;
begin
  if not FWriting or (FCount<>Length(FLines)) then JournalError('writer count changed');
  Result:=WfcTextJoinCanonicalLines(FLines,Artifact);
  if Length(Result)<>FBytes then JournalError('encoded byte count changed');
end;
procedure WriteDocument(const W: TJournalWriter; const Recipes: TWfcPipelineWorkspaceRecipeTexts;
  const Runs: TWfcPipelineWorkspaceRunTexts; const Actions: TWfcPipelineWorkspaceActions;
  const ContextBytes,RootReferences,EvidenceBytes: Integer);
var I,J: Integer; Prefix: String;
begin
  W.Line('wfc-workspace-journal=1'); W.Line('verification=unverified-claims');
  W.Number('recipe-count=',Length(Recipes)); W.Number('run-count=',Length(Runs));
  W.Number('action-count=',Length(Actions)); W.Number('root-reference-count=',RootReferences);
  W.Number('context-text-bytes=',ContextBytes); W.Number('evidence-text-bytes=',EvidenceBytes);
  for I:=0 to High(Recipes) do
  begin Prefix:='recipe.'+IntToStr(I); W.Number(Prefix+'.bytes=',Length(Recipes[I])); W.Document(Prefix+'.text=',Recipes[I]); end;
  for I:=0 to High(Runs) do
  begin
    Prefix:='run.'+IntToStr(I); W.Number(Prefix+'.recipe=',Runs[I].RecipeIndex);
    W.Number(Prefix+'.bytes=',Length(Runs[I].Text)); W.Document(Prefix+'.text=',Runs[I].Text);
  end;
  for I:=0 to High(Actions) do
  begin
    Prefix:='action.'+IntToStr(I); W.Line(Prefix+'.kind='+WfcPipelineWorkspaceActionName(Actions[I].Kind));
    W.Number(Prefix+'.run=',Actions[I].RunIndex); W.Number(Prefix+'.root-count=',Length(Actions[I].RequestedRootIndices));
    for J:=0 to High(Actions[I].RequestedRootIndices) do W.Number(Prefix+'.root.'+IntToStr(J)+'=',Actions[I].RequestedRootIndices[J]);
    W.Number(Prefix+'.evidence-bytes=',Length(Actions[I].EvidenceText)); W.Document(Prefix+'.evidence=',Actions[I].EvidenceText);
  end;
  W.Line('end=1');
end;
function SameInputs(const A,B: TWfcPipelineRun): Boolean;
var I,J: Integer; LA,LB: TWfcPipelineCellLock; DA,DB: TWfcPipelineCellDomain;
begin
  Result:=False; if (A.LockCount<>B.LockCount) or (A.DomainCount<>B.DomainCount) then Exit;
  for I:=0 to A.LockCount-1 do
  begin
    LA:=A.LockAt(I); LB:=B.LockAt(I);
    if (LA.PassIndex<>LB.PassIndex) or (LA.X<>LB.X) or (LA.Y<>LB.Y) or (LA.Z<>LB.Z) or (LA.Token<>LB.Token) then Exit;
  end;
  for I:=0 to A.DomainCount-1 do
  begin
    DA:=A.DomainAt(I); DB:=B.DomainAt(I);
    if (DA.PassIndex<>DB.PassIndex) or (DA.X<>DB.X) or (DA.Y<>DB.Y) or (DA.Z<>DB.Z) or
      (Length(DA.AllowedTokens)<>Length(DB.AllowedTokens)) then Exit;
    for J:=0 to High(DA.AllowedTokens) do if DA.AllowedTokens[J]<>DB.AllowedTokens[J] then Exit;
  end;
  Result:=True;
end;
procedure RequireEpoch(const A,Epoch: TWfcPipelineRun);
var I: Integer;
begin
  if (A.Seed<>Epoch.Seed) or (A.FormatVersion<>Epoch.FormatVersion) or (A.PassCount<>Epoch.PassCount) then
    JournalError('seed, run format and pass count require an explicit epoch');
  for I:=0 to A.PassCount-1 do
    if (A.PassTopologyAt(I).Rank<>Epoch.PassTopologyAt(I).Rank) or
      not SameWfcLatticeLayout(A.PassLayoutAt(I),Epoch.PassLayoutAt(I)) then
      JournalError('every pass rank/layout must remain fixed within an epoch');
end;
procedure RequireEvidenceKind(const Action: TWfcPipelineWorkspaceAction; const Run: TWfcPipelineRun);
var Kind,Prefix: String;
begin
  if Action.Kind=wpwakBeginEpoch then
  begin if Action.EvidenceText<>'' then JournalError('begin-epoch cannot claim an outcome'); Exit; end;
  if Action.Kind=wpwakEdit then Kind:='edit'
  else
  begin
    if Run.Strategy=wpssOneWay then Kind:='ordinary-' else Kind:='negotiated-';
    if Action.Kind=wpwakInitial then Kind:=Kind+'full' else Kind:=Kind+'selective';
  end;
  Prefix:='wfc-session-evidence=1'#10'kind='+Kind+#10;
  if Copy(Action.EvidenceText,1,Length(Prefix))<>Prefix then JournalError('unverified evidence version/kind does not match action strategy');
end;
procedure ValidateTransitions(const Contexts: TWfcPipelineWorkspaceContexts; const Actions: TWfcPipelineWorkspaceActions);
var I,J,EpochIndex,AppliedIndex,RecipeIndex: Integer; Attempted: Boolean; Run: TWfcPipelineRun;
begin
  if Actions[0].Kind<>wpwakBeginEpoch then JournalError('first action must begin an epoch');
  EpochIndex:=-1; AppliedIndex:=-1; RecipeIndex:=-1; Attempted:=False;
  for I:=0 to High(Actions) do
  begin
    Run:=Contexts.BorrowRun(Actions[I].RunIndex);
    if Actions[I].Kind=wpwakBeginEpoch then
    begin
      EpochIndex:=Actions[I].RunIndex; AppliedIndex:=EpochIndex;
      RecipeIndex:=Contexts.RunTextAt(EpochIndex).RecipeIndex; Attempted:=False;
    end
    else
    begin
      if Contexts.RunTextAt(Actions[I].RunIndex).RecipeIndex<>RecipeIndex then
        JournalError('recipe context index requires an explicit epoch');
      RequireEpoch(Run,Contexts.BorrowRun(EpochIndex));
      case Actions[I].Kind of
        wpwakEdit:AppliedIndex:=Actions[I].RunIndex;
        wpwakInitial:begin
          if Attempted then JournalError('initial is allowed once per epoch');
          if Actions[I].RunIndex<>AppliedIndex then JournalError('initial must use the then-applied run index');
          Attempted:=True;
        end;
        wpwakRepair:begin
          if not Attempted then JournalError('repair requires a prior initial attempt');
          if not SameInputs(Run,Contexts.BorrowRun(AppliedIndex)) then JournalError('repair requires exactly the applied ordered inputs');
          AppliedIndex:=Actions[I].RunIndex;
        end;
      end;
    end;
    if Actions[I].Kind=wpwakRepair then
    begin
      if Length(Actions[I].RequestedRootIndices)=0 then JournalError('repair requires explicit nonempty roots');
      for J:=0 to High(Actions[I].RequestedRootIndices) do
      begin
        RequireInteger(Actions[I].RequestedRootIndices[J],0,Run.PassCount-1,'repair root');
        if (J>0) and (Actions[I].RequestedRootIndices[J]<=Actions[I].RequestedRootIndices[J-1]) then
          JournalError('stored roots must be strictly ascending and unique');
      end;
    end
    else if Length(Actions[I].RequestedRootIndices)<>0 then JournalError('only repair carries roots');
    RequireEvidenceKind(Actions[I],Run);
  end;
end;
constructor TWfcPipelineWorkspaceJournal.Create(const RecipeTexts: TWfcPipelineWorkspaceRecipeTexts;
  const RunTexts: TWfcPipelineWorkspaceRunTexts; const Actions: TWfcPipelineWorkspaceActions;
  const Limits: TWfcPipelineWorkspaceJournalLimits);
var I,J,ContextBytes: Integer; ContextLimits: TWfcPipelineWorkspaceContextLimits; W: TJournalWriter;
begin
  inherited Create; GuardInputs(RecipeTexts,RunTexts,Actions,Limits); W:=nil; ContextBytes:=0;
  for I:=0 to High(RecipeTexts) do
  begin Charge(ContextBytes,Length(RecipeTexts[I]),Limits.MaxContextTextBytes,'context text'); CheckDocument(RecipeTexts[I],False,'recipe document'); end;
  for I:=0 to High(RunTexts) do
  begin
    RequireInteger(RunTexts[I].RecipeIndex,0,Length(RecipeTexts)-1,'run recipe index');
    Charge(ContextBytes,Length(RunTexts[I].Text),Limits.MaxContextTextBytes,'context text'); CheckDocument(RunTexts[I].Text,False,'run document');
  end;
  for I:=0 to High(Actions) do
  begin
    RequireInteger(Ord(Actions[I].Kind),0,3,'action kind'); RequireInteger(Actions[I].RunIndex,0,Length(RunTexts)-1,'action run index');
    Charge(FRootReferences,Length(Actions[I].RequestedRootIndices),Limits.MaxRootReferences,'root reference');
    for J:=0 to High(Actions[I].RequestedRootIndices) do RequireInteger(Actions[I].RequestedRootIndices[J],0,High(Integer),'root index');
    Charge(FEvidenceTextBytes,Length(Actions[I].EvidenceText),Limits.MaxEvidenceTextBytes,'evidence text');
    CheckDocument(Actions[I].EvidenceText,Actions[I].Kind=wpwakBeginEpoch,'evidence document');
  end;
  try
    W:=TJournalWriter.Create(Limits.MaxEncodedTextBytes);
    WriteDocument(W,RecipeTexts,RunTexts,Actions,ContextBytes,FRootReferences,FEvidenceTextBytes);
    { All raw container/count/document/encoded-byte envelopes are checked before
      typed context decoders, owned action/root clones or encoded-line storage. }
    ContextLimits.Version:=1; ContextLimits.MaxRecipes:=Limits.MaxRecipes;
    ContextLimits.MaxRuns:=Limits.MaxRuns; ContextLimits.MaxTextBytes:=Limits.MaxContextTextBytes;
    try FContexts:=TWfcPipelineWorkspaceContexts.Create(RecipeTexts,RunTexts,ContextLimits);
    except on E:EOutOfMemory do raise; on E:Exception do JournalError('contexts: '+E.Message); end;
    ValidateTransitions(FContexts,Actions);
    SetLength(FActions,Length(Actions));
    for I:=0 to High(Actions) do
    begin
      { Do not record-assign a borrowed root array or use Copy here: pas2js
        arrayRef mutates frozen inputs, and scalar arrayCopy calls the caller's
        slice method. Only validated scalar indices enter the owned vector. }
      FActions[I].Kind:=Actions[I].Kind; FActions[I].RunIndex:=Actions[I].RunIndex;
      FActions[I].EvidenceText:=Actions[I].EvidenceText;
      SetLength(FActions[I].RequestedRootIndices,Length(Actions[I].RequestedRootIndices));
      for J:=0 to High(Actions[I].RequestedRootIndices) do
        FActions[I].RequestedRootIndices[J]:=Actions[I].RequestedRootIndices[J];
    end;
    W.BeginWriting; WriteDocument(W,RecipeTexts,RunTexts,Actions,ContextBytes,FRootReferences,FEvidenceTextBytes);
    FCanonicalText:=W.Finish; FLimits:=Limits; FInitialized:=True;
  finally W.Free; end;
end;
destructor TWfcPipelineWorkspaceJournal.Destroy;
begin FContexts.Free; inherited Destroy; end;
procedure TWfcPipelineWorkspaceJournal.RequireInitialized;
begin if not FInitialized then JournalError('journal is not initialized'); end;
procedure TWfcPipelineWorkspaceJournal.CheckActionIndex(const Index: Integer);
begin RequireInitialized; RequireInteger(Index,0,Length(FActions)-1,'action index'); end;
function TWfcPipelineWorkspaceJournal.GetRecipeCount: Integer;
begin RequireInitialized; Result:=FContexts.RecipeCount; end;
function TWfcPipelineWorkspaceJournal.GetRunCount: Integer;
begin RequireInitialized; Result:=FContexts.RunCount; end;
function TWfcPipelineWorkspaceJournal.GetActionCount: Integer;
begin RequireInitialized; Result:=Length(FActions); end;
function TWfcPipelineWorkspaceJournal.GetContextTextBytes: Integer;
begin RequireInitialized; Result:=FContexts.TextBytes; end;
function TWfcPipelineWorkspaceJournal.GetRootReferenceCount: Integer;
begin RequireInitialized; Result:=FRootReferences; end;
function TWfcPipelineWorkspaceJournal.GetEvidenceTextBytes: Integer;
begin RequireInitialized; Result:=FEvidenceTextBytes; end;
function TWfcPipelineWorkspaceJournal.GetEncodedTextBytes: Integer;
begin RequireInitialized; Result:=Length(FCanonicalText); end;
function TWfcPipelineWorkspaceJournal.GetVerification: TWfcPipelineWorkspaceVerification;
begin RequireInitialized; Result:=wpwvUnverifiedClaims; end;
function TWfcPipelineWorkspaceJournal.RecipeTextAt(const Index: Integer): String;
begin RequireInitialized; RequireInteger(Index,0,FContexts.RecipeCount-1,'recipe index'); Result:=FContexts.RecipeTextAt(Index); end;
function TWfcPipelineWorkspaceJournal.RunTextAt(const Index: Integer): TWfcPipelineWorkspaceRunText;
begin RequireInitialized; RequireInteger(Index,0,FContexts.RunCount-1,'run index'); Result:=FContexts.RunTextAt(Index); end;
function TWfcPipelineWorkspaceJournal.BorrowRecipe(const Index: Integer): TWfcPipelineModel;
begin RequireInitialized; RequireInteger(Index,0,FContexts.RecipeCount-1,'recipe index'); Result:=FContexts.BorrowRecipe(Index); end;
function TWfcPipelineWorkspaceJournal.BorrowRun(const Index: Integer): TWfcPipelineRun;
begin RequireInitialized; RequireInteger(Index,0,FContexts.RunCount-1,'run index'); Result:=FContexts.BorrowRun(Index); end;
function TWfcPipelineWorkspaceJournal.CopyRecipe(const Index: Integer): TWfcPipelineModel;
begin RequireInitialized; RequireInteger(Index,0,FContexts.RecipeCount-1,'recipe index'); Result:=FContexts.CopyRecipe(Index); end;
function TWfcPipelineWorkspaceJournal.CopyRun(const Index: Integer): TWfcPipelineRun;
begin RequireInitialized; RequireInteger(Index,0,FContexts.RunCount-1,'run index'); Result:=FContexts.CopyRun(Index); end;
function TWfcPipelineWorkspaceJournal.ActionAt(const Index: Integer): TWfcPipelineWorkspaceAction;
begin CheckActionIndex(Index); Result:=FActions[Index]; Result.RequestedRootIndices:=Copy(FActions[Index].RequestedRootIndices,0,Length(FActions[Index].RequestedRootIndices)); end;
function TWfcPipelineWorkspaceJournal.CopyActions: TWfcPipelineWorkspaceActions;
var I: Integer;
begin RequireInitialized; Result:=nil; SetLength(Result,Length(FActions)); for I:=0 to High(Result) do Result[I]:=ActionAt(I); end;
function TWfcPipelineWorkspaceJournal.CopyLimits: TWfcPipelineWorkspaceJournalLimits;
begin RequireInitialized; Result:=FLimits; end;
function TWfcPipelineWorkspaceJournal.CopyCanonicalText: String;
begin RequireInitialized; Result:=FCanonicalText; end;
end.
