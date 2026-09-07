{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Prepared session; no journal codec or mutable graph escape. }
unit wfc_pipeline_session;
{$mode delphi}{$H+}
interface

uses SysUtils, wfc, wfc_model, wfc_lattice, wfc_pipeline_model,
  wfc_pipeline_run, wfc_pipeline_layout, wfc_pipeline_compile,
  wfc_pipeline_prepare;

const WFC_PIPELINE_SESSION_VERSION = 1;

type
  EWfcPipelineSession = class(Exception);
  { Limits charge one detached payload: actual public cells; encoded public
    labels/cell tokens, invocation lock/domain tokens and every actual trace
    Value/delivery-message string; every nested solve Passes row; every nested
    trace event; every rejected assignment item. Negotiated reports are stored
    once, not again as a separate terminal solve. Scope/index arrays are bounded
    by the recipe pass limit; invocation arrays retain existing run bounds.
    These are capture limits, not limits on earlier core-report allocation,
    aggregate caller copies, retained successful-state copies or peak heap. }
  TWfcPipelineSessionOutcomeLimits = record
    Version: Integer;
    MaxPublicCellRecords: Integer;
    MaxEncodedTokenBytes: Integer;
    MaxReportPassRecords: Integer;
    MaxTraceEvents: Integer;
    MaxExcludedAssignmentItems: Integer;
  end;
  TWfcPipelineSessionOutcomeKind = (wpsokOrdinaryFull, wpsokNegotiatedFull,
    wpsokOrdinarySelective, wpsokNegotiatedSelective);
  TWfcPipelineSessionCell = record
    Token: TWfcModelToken;
    Empty, Generated: Boolean;
  end;
  TWfcPipelineSessionCells = array of TWfcPipelineSessionCell;
  TWfcPipelineSessionLayer = record
    PassIndex: Integer;
    LabelName: TWfcModelToken;
    Rank: Integer;
    Layout: TWfcLatticeLayout;
    Cells: TWfcPipelineSessionCells;
  end;
  TWfcPipelineSessionLayers = array of TWfcPipelineSessionLayer;
  { Detached invocation evidence, not a new run artifact. No recipe borrow is
    retained by outcomes/plans; these values survive session/recipe disposal. }
  TWfcPipelineSessionInvocation = record
    FormatVersion: Integer;
    RecipeSignature: TWfcPipelineSignature;
    Seed: TGraphSeed;
    Strategy: TWfcPipelineSolveStrategy;
    MaxBacktracks, MaxPassBacktracks: Integer;
    CaptureTrace: Boolean;
    Topologies: TWfcPipelinePassTopologies;
    Extents: TWfcPipelinePassExtents;
    Locks: TWfcPipelineCellLocks;
    Domains: TWfcPipelineCellDomains;
  end;
  TWfcPipelineSessionScope = record
    ScopeAlgorithmVersion: Integer;
    RequestedRootIndices, ActivePassIndices: TGraphPassIndices;
    AuthoredPassIndices, RequiredPassIndices, MissingPassIndices: TGraphPassIndices;
  end;

  TWfcPipelineSessionPublicState = class
  private
    FData: TObject;
    constructor CreateOwned(const AData: TObject);
    function GetLayerCount: Integer;
  public
    destructor Destroy; override;
    function LayerAt(const AIndex: Integer): TWfcPipelineSessionLayer;
    function CopyLayers: TWfcPipelineSessionLayers;
    property LayerCount: Integer read GetLayerCount;
  end;

  TWfcPipelineSessionRepairPlan = class
  private
    FData: TObject;
    constructor CreateOwned(const AData: TObject);
    function GetBaseRevision: Integer;
    function GetCanExecute: Boolean;
    function GetMissingBaseline: Boolean;
  public
    destructor Destroy; override;
    function CopyScope: TWfcPipelineSessionScope;
    function CopyInvocation: TWfcPipelineSessionInvocation;
    property BaseRevision: Integer read GetBaseRevision;
    property CanExecute: Boolean read GetCanExecute;
    property MissingBaseline: Boolean read GetMissingBaseline;
  end;

  TWfcPipelineSessionEditOutcome = class
  private
    FData: TObject;
    constructor CreateOwned(const AData: TObject);
    function GetRevision: Integer;
    function GetHasCurrentOutput: Boolean;
    function GetHasSuccessfulBaseline: Boolean;
  public
    destructor Destroy; override;
    function CopyImpact: TWfcPipelineInputImpact;
    function CopyPendingPassIndices: TGraphPassIndices;
    function CopyAuthoredPassIndices: TGraphPassIndices;
    function CopyPublicState: TWfcPipelineSessionPublicState;
    function CopyInvocation: TWfcPipelineSessionInvocation;
    property Revision: Integer read GetRevision;
    property HasCurrentOutput: Boolean read GetHasCurrentOutput;
    property HasSuccessfulBaseline: Boolean read GetHasSuccessfulBaseline;
  end;

  TWfcPipelineSessionOutcome = class
  private
    FData: TObject;
    constructor CreateOwned(const AData: TObject);
    function GetKind: TWfcPipelineSessionOutcomeKind;
    function GetRevision: Integer;
    function GetSolved: Boolean;
    function GetHasCurrentOutput: Boolean;
    function GetHasSuccessfulBaseline: Boolean;
    function GetHasNegotiation: Boolean;
    function GetHasSelectiveNegotiation: Boolean;
    function GetLastValidation: TWfcPipelineCommitValidation;
  public
    destructor Destroy; override;
    function CopyScope: TWfcPipelineSessionScope;
    function CopyPendingPassIndices: TGraphPassIndices;
    function CopyAuthoredPassIndices: TGraphPassIndices;
    function CopyPublicState: TWfcPipelineSessionPublicState;
    function CopyInvocation: TWfcPipelineSessionInvocation;
    function CopySolveReport: TGraphSolveReport;
    function CopyNegotiationReport: TGraphNegotiationReport;
    function CopySelectiveNegotiationReport: TGraphSelectiveNegotiationReport;
    property Kind: TWfcPipelineSessionOutcomeKind read GetKind;
    property Revision: Integer read GetRevision;
    property Solved: Boolean read GetSolved;
    property HasCurrentOutput: Boolean read GetHasCurrentOutput;
    property HasSuccessfulBaseline: Boolean read GetHasSuccessfulBaseline;
    property HasNegotiation: Boolean read GetHasNegotiation;
    property HasSelectiveNegotiation: Boolean read GetHasSelectiveNegotiation;
    property LastValidation: TWfcPipelineCommitValidation read GetLastValidation;
  end;

  TWfcPipelinePreparedSession = class
  private
    FData: TObject;
    function GetRevision: Integer;
    function GetUsable: Boolean;
    function GetHasSuccessfulBaseline: Boolean;
    function GetHasCurrentOutput: Boolean;
  public
    { Recipe is borrowed and must outlive this session; InitialRun is needed
      only during construction. Every returned class is caller-owned. }
    constructor Create(const ARecipe: TWfcPipelineModel;
      const AInitialRun: TWfcPipelineRun;
      const AReplacementLimits: TWfcPipelineReplacementLimits;
      const AOutcomeLimits: TWfcPipelineSessionOutcomeLimits);
    destructor Destroy; override;
    function ExecuteInitial: TWfcPipelineSessionOutcome;
    function ApplyInputs(const ADesiredRun: TWfcPipelineRun): TWfcPipelineSessionEditOutcome;
    function PlanRepair(const ADesiredRun: TWfcPipelineRun;
      const ARequestedRoots: TGraphPassLabels): TWfcPipelineSessionRepairPlan;
    function ExecuteRepair(const APlan: TWfcPipelineSessionRepairPlan): TWfcPipelineSessionOutcome;
    function CopyPublicState: TWfcPipelineSessionPublicState;
    { Returns nil when no successful baseline exists. }
    function CopyLastSuccessfulState: TWfcPipelineSessionPublicState;
    function CopyPendingPassIndices: TGraphPassIndices;
    function CopyAppliedRun: TWfcPipelineRun;
    property Revision: Integer read GetRevision;
    property Usable: Boolean read GetUsable;
    property HasSuccessfulBaseline: Boolean read GetHasSuccessfulBaseline;
    property HasCurrentOutput: Boolean read GetHasCurrentOutput;
  end;

implementation

uses wfc_text_codec;

type
  TWrapperKind = (wkState, wkPlan, wkEdit, wkOutcome, wkSession);
  TSessionIdentity = class
    References: Integer;
    constructor Create;
    procedure Retain;
    procedure Release;
  end;
  TStateData = class
    Layers: TWfcPipelineSessionLayers;
  end;
  TPlanData = class
    Identity: TSessionIdentity;
    Revision: Integer;
    MissingBaseline: Boolean;
    Scope: TWfcPipelineSessionScope;
    Invocation: TWfcPipelineSessionInvocation;
    destructor Destroy; override;
  end;
  TSnapshotData = class
    Revision: Integer;
    Current, Baseline: Boolean;
    Pending, Authored: TGraphPassIndices;
    Layers: TWfcPipelineSessionLayers;
    Invocation: TWfcPipelineSessionInvocation;
  end;
  TEditData = class(TSnapshotData)
    Impact: TWfcPipelineInputImpact;
  end;
  TOutcomeData = class(TSnapshotData)
    Kind: TWfcPipelineSessionOutcomeKind;
    Solved: Boolean;
    Scope: TWfcPipelineSessionScope;
    Validation: TWfcPipelineCommitValidation;
    Ordinary: TGraphSolveReport;
    Negotiated: TGraphNegotiationReport;
    Selective: TGraphSelectiveNegotiationReport;
  end;
  TSessionData = class
    Recipe: TWfcPipelineModel;
    InitialRun, AppliedRun: TWfcPipelineRun;
    Preparation: TWfcPipelinePreparation;
    Binding: TWfcPipelineInputBinding;
    Identity: TSessionIdentity;
    Limits: TWfcPipelineSessionOutcomeLimits;
    Revision: Integer;
    Usable, InitialAttempted, Baseline, Current: Boolean;
    Pending, Authored: TGraphPassIndices;
    LastSuccessful: TWfcPipelineSessionLayers;
    destructor Destroy; override;
  end;
  TCaptureBudget = record
    Cells, Tokens, Passes, Events, Assignments: Integer;
  end;
{$IFDEF PAS2JS}
  TWrapper = record Owner: TObject; Kind: TWrapperKind; end;
  TWrappers = array of TWrapper;
var Wrappers: TWrappers;
{$ENDIF}

procedure Fail(const Detail: String);
begin raise EWfcPipelineSession.Create('pipeline session: '+Detail); end;

procedure RegisterWrapper(const Owner: TObject; const Kind: TWrapperKind);
{$IFDEF PAS2JS}var N: Integer;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  N:=Length(Wrappers); if N=High(Integer) then Fail('live wrapper count exceeds Integer');
  SetLength(Wrappers,N+1); Wrappers[N].Owner:=Owner; Wrappers[N].Kind:=Kind;
  {$ENDIF}
end;

procedure UnregisterWrapper(const Owner: TObject);
{$IFDEF PAS2JS}var I,J: Integer;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  for I:=0 to High(Wrappers) do if Wrappers[I].Owner=Owner then
  begin
    for J:=I+1 to High(Wrappers) do Wrappers[J-1]:=Wrappers[J];
    SetLength(Wrappers,Length(Wrappers)-1); Exit;
  end;
  {$ENDIF}
end;

function CheckedData(const Owner: TObject; const Kind: TWrapperKind): TObject;
var Valid: Boolean; {$IFDEF PAS2JS}I: Integer;{$ENDIF}
begin
  Valid:=False;
  {$IFDEF PAS2JS}
  { Reference comparison reads no property from arbitrary JavaScript input. }
  for I:=0 to High(Wrappers) do
    if (Wrappers[I].Owner=Owner) and (Wrappers[I].Kind=Kind) then
    begin Valid:=True; Break; end;
  {$ELSE}
  if Owner<>nil then case Kind of
    wkState: Valid:=Owner is TWfcPipelineSessionPublicState;
    wkPlan: Valid:=Owner is TWfcPipelineSessionRepairPlan;
    wkEdit: Valid:=Owner is TWfcPipelineSessionEditOutcome;
    wkOutcome: Valid:=Owner is TWfcPipelineSessionOutcome;
    wkSession: Valid:=Owner is TWfcPipelinePreparedSession;
  end;
  {$ENDIF}
  if not Valid then Fail('receiver or argument is not a live initialized owner');
  Result:=nil;
  case Kind of
    wkState: Result:=TWfcPipelineSessionPublicState(Owner).FData;
    wkPlan: Result:=TWfcPipelineSessionRepairPlan(Owner).FData;
    wkEdit: Result:=TWfcPipelineSessionEditOutcome(Owner).FData;
    wkOutcome: Result:=TWfcPipelineSessionOutcome(Owner).FData;
    wkSession: Result:=TWfcPipelinePreparedSession(Owner).FData;
  end;
  if Result=nil then Fail('receiver or argument is not initialized');
end;

function SessionData(const Owner: TWfcPipelinePreparedSession;
  const RequireUsable: Boolean=True): TSessionData;
begin
  Result:=TSessionData(CheckedData(Owner,wkSession));
  if RequireUsable and not Result.Usable then Fail('session candidate is unusable');
end;

constructor TSessionIdentity.Create;
begin inherited Create; References:=1; end;
procedure TSessionIdentity.Retain;
{$IFNDEF PAS2JS}var Previous: Integer;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  if References=High(Integer) then Fail('identity lease count exceeds Integer');
  Inc(References);
  {$ELSE}
  repeat
    Previous:=System.InterlockedCompareExchange(References,0,0);
    if (Previous<=0) or (Previous=High(Integer)) then Fail('identity lease count exceeds Integer');
  until System.InterlockedCompareExchange(References,Previous+1,Previous)=Previous;
  {$ENDIF}
end;
procedure TSessionIdentity.Release;
begin
  {$IFDEF PAS2JS}Dec(References); if References=0 then Free;
  {$ELSE}if System.InterlockedDecrement(References)=0 then Free;{$ENDIF}
end;
destructor TPlanData.Destroy;
begin if Identity<>nil then Identity.Release; inherited Destroy; end;
destructor TSessionData.Destroy;
begin
  Binding.Free; Preparation.Free; AppliedRun.Free; InitialRun.Free;
  if Identity<>nil then Identity.Release;
  inherited Destroy;
end;

procedure RequireInteger(const Value,Minimum,Maximum: Integer; const Name: String);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid=typeof Value==='number' && Number.isFinite(Value) && Number.isInteger(Value); end;
  if not Valid then Fail(Name+' must be an exact finite Integer');
  {$ENDIF}
  if (Value<Minimum) or (Value>Maximum) then Fail(Name+' is out of range');
end;

procedure RequireLimits(const Limits: TWfcPipelineSessionOutcomeLimits);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    Valid=Limits!==null && typeof Limits==='object' && !Array.isArray(Limits);
    if(Valid) for(const key of ['Version','MaxPublicCellRecords','MaxEncodedTokenBytes',
      'MaxReportPassRecords','MaxTraceEvents','MaxExcludedAssignmentItems']) {
      let p=Limits,d;
      while(p!==null) {d=Object.getOwnPropertyDescriptor(p,key);if(d)break;p=Object.getPrototypeOf(p);}
      if(!d || !Object.prototype.hasOwnProperty.call(d,'value') || typeof d.value!=='number' ||
        !Number.isFinite(d.value) || !Number.isInteger(d.value)) {Valid=false;break;}
    }
  end;
  if not Valid then Fail('outcome limits require passive finite Integer fields');
  {$ENDIF}
  RequireInteger(Limits.Version,1,1,'outcome limits version');
  RequireInteger(Limits.MaxPublicCellRecords,1,High(Integer),'public cell limit');
  RequireInteger(Limits.MaxEncodedTokenBytes,1,High(Integer),'encoded token limit');
  RequireInteger(Limits.MaxReportPassRecords,1,High(Integer),'report pass limit');
  RequireInteger(Limits.MaxTraceEvents,1,High(Integer),'trace event limit');
  RequireInteger(Limits.MaxExcludedAssignmentItems,1,High(Integer),'excluded assignment limit');
end;

procedure Charge(var Total: Integer; const Count: SizeInt;
  const Maximum: Integer; const Name: String);
begin
  if (Count<0) or (Count>Maximum) or (Total>Maximum-Count) then Fail(Name+' capture limit exceeded');
  Inc(Total,Integer(Count));
end;

function GraphToken(const Value: TGraphValue): TWfcModelToken;
begin {$IFDEF PAS2JS}Result:=TWfcModelToken(Value);{$ELSE}Result:=UTF8Encode(UnicodeString(Value));{$ENDIF} end;
function GraphLabel(const Value: TWfcModelToken): String;
begin {$IFDEF PAS2JS}Result:=String(Value);{$ELSE}Result:=String(UTF8Decode(Value));{$ENDIF} end;
procedure ChargeToken(var Budget: TCaptureBudget; const Token: TWfcModelToken;
  const Limits: TWfcPipelineSessionOutcomeLimits);
var Encoded: String;
begin
  Encoded:=WfcTextEncodeToken(Token,'pipeline session capture');
  Charge(Budget.Tokens,Length(Encoded),Limits.MaxEncodedTokenBytes,'encoded token');
end;

function CopyIndices(const Values: TGraphPassIndices): TGraphPassIndices;
begin Result:=Copy(Values,0,Length(Values)); end;
function CopyTokens(const Values: TWfcModelTokens): TWfcModelTokens;
begin Result:=Copy(Values,0,Length(Values)); end;
function CopyDomains(const Values: TWfcPipelineCellDomains): TWfcPipelineCellDomains;
var I: Integer;
begin
  Result:=Copy(Values,0,Length(Values));
  for I:=0 to High(Result) do Result[I].AllowedTokens:=CopyTokens(Values[I].AllowedTokens);
end;
function CopyInvocation(const Value: TWfcPipelineSessionInvocation): TWfcPipelineSessionInvocation;
begin
  Result:=Value; Result.Topologies:=Copy(Value.Topologies,0,Length(Value.Topologies));
  Result.Extents:=Copy(Value.Extents,0,Length(Value.Extents));
  Result.Locks:=Copy(Value.Locks,0,Length(Value.Locks)); Result.Domains:=CopyDomains(Value.Domains);
end;
function Invocation(const Run: TWfcPipelineRun): TWfcPipelineSessionInvocation;
var I: Integer;
begin
  Result:=Default(TWfcPipelineSessionInvocation);
  Result.FormatVersion:=Run.FormatVersion; Result.RecipeSignature:=Run.RecipeSignature;
  Result.Seed:=Run.Seed; Result.Strategy:=Run.Strategy; Result.MaxBacktracks:=Run.MaxBacktracks;
  Result.MaxPassBacktracks:=Run.MaxPassBacktracks; Result.CaptureTrace:=Run.CaptureTrace;
  SetLength(Result.Topologies,Run.PassCount);
  for I:=0 to Run.PassCount-1 do Result.Topologies[I]:=Run.PassTopologyAt(I);
  Result.Extents:=Run.CopyPassExtents; Result.Locks:=Run.CopyLocks; Result.Domains:=Run.CopyDomains;
end;
function CopyRun(const Recipe: TWfcPipelineModel; const Run: TWfcPipelineRun): TWfcPipelineRun;
begin
  if Run.FormatVersion=WFC_PIPELINE_RUN_VERSION then
    Result:=TWfcPipelineRun.Create(Recipe,Run.Width,Run.Height,Run.Depth,Run.Seed,
      Run.Strategy,Run.MaxBacktracks,Run.MaxPassBacktracks,Run.CaptureTrace,Run.CopyLocks,Run.CopyDomains)
  else Result:=TWfcPipelineRun.Create(Recipe,Run.CopyPassExtents,Run.Seed,
      Run.Strategy,Run.MaxBacktracks,Run.MaxPassBacktracks,Run.CaptureTrace,Run.CopyLocks,Run.CopyDomains);
end;
function RunFromInvocation(const Recipe: TWfcPipelineModel;
  const Value: TWfcPipelineSessionInvocation): TWfcPipelineRun;
begin
  if Value.FormatVersion=WFC_PIPELINE_RUN_VERSION then
    Result:=TWfcPipelineRun.Create(Recipe,Value.Extents[0].X,Value.Extents[0].Y,
      Value.Extents[0].Z,Value.Seed,Value.Strategy,Value.MaxBacktracks,
      Value.MaxPassBacktracks,Value.CaptureTrace,Value.Locks,Value.Domains)
  else Result:=TWfcPipelineRun.Create(Recipe,Value.Extents,Value.Seed,Value.Strategy,
      Value.MaxBacktracks,Value.MaxPassBacktracks,Value.CaptureTrace,Value.Locks,Value.Domains);
end;
procedure ChargeInvocation(var Budget: TCaptureBudget; const Run: TWfcPipelineRun;
  const Limits: TWfcPipelineSessionOutcomeLimits);
var I,J: Integer; Domain: TWfcPipelineCellDomain;
begin
  for I:=0 to Run.LockCount-1 do ChargeToken(Budget,Run.LockAt(I).Token,Limits);
  for I:=0 to Run.DomainCount-1 do
  begin
    Domain:=Run.DomainAt(I);
    for J:=0 to High(Domain.AllowedTokens) do ChargeToken(Budget,Domain.AllowedTokens[J],Limits);
  end;
end;
function SameInputs(const A,B: TWfcPipelineRun): Boolean;
var I,J: Integer; LA,LB: TWfcPipelineCellLock; DA,DB: TWfcPipelineCellDomain;
begin
  Result:=False;
  if (A.LockCount<>B.LockCount) or (A.DomainCount<>B.DomainCount) then Exit;
  for I:=0 to A.LockCount-1 do
  begin
    LA:=A.LockAt(I); LB:=B.LockAt(I);
    if (LA.PassIndex<>LB.PassIndex) or (LA.X<>LB.X) or (LA.Y<>LB.Y) or
      (LA.Z<>LB.Z) or (LA.Token<>LB.Token) then Exit;
  end;
  for I:=0 to A.DomainCount-1 do
  begin
    DA:=A.DomainAt(I); DB:=B.DomainAt(I);
    if (DA.PassIndex<>DB.PassIndex) or (DA.X<>DB.X) or (DA.Y<>DB.Y) or
      (DA.Z<>DB.Z) or (Length(DA.AllowedTokens)<>Length(DB.AllowedTokens)) then Exit;
    for J:=0 to High(DA.AllowedTokens) do if DA.AllowedTokens[J]<>DB.AllowedTokens[J] then Exit;
  end;
  Result:=True;
end;

function CloneScope(const Value: TWfcPipelineSessionScope): TWfcPipelineSessionScope;
begin
  Result:=Value; Result.RequestedRootIndices:=CopyIndices(Value.RequestedRootIndices);
  Result.ActivePassIndices:=CopyIndices(Value.ActivePassIndices);
  Result.AuthoredPassIndices:=CopyIndices(Value.AuthoredPassIndices);
  Result.RequiredPassIndices:=CopyIndices(Value.RequiredPassIndices);
  Result.MissingPassIndices:=CopyIndices(Value.MissingPassIndices);
end;
function CloneImpact(const Value: TWfcPipelineInputImpact): TWfcPipelineInputImpact;
begin
  Result:=Value; Result.AuthoredPassIndices:=CopyIndices(Value.AuthoredPassIndices);
  Result.ChangedPassIndices:=CopyIndices(Value.ChangedPassIndices);
end;
function CloneLayers(const Values: TWfcPipelineSessionLayers): TWfcPipelineSessionLayers;
var I: Integer;
begin
  Result:=Copy(Values,0,Length(Values));
  for I:=0 to High(Result) do Result[I].Cells:=Copy(Values[I].Cells,0,Length(Values[I].Cells));
end;
function CloneSolve(const Value: TGraphSolveReport): TGraphSolveReport;
begin
  Result:=Value; Result.Passes:=Copy(Value.Passes,0,Length(Value.Passes));
  Result.ExecutionOrder:=CopyIndices(Value.ExecutionOrder); Result.Trace:=Copy(Value.Trace,0,Length(Value.Trace));
end;
function CloneNegotiation(const Value: TGraphNegotiationReport): TGraphNegotiationReport;
var I: Integer;
begin
  Result:=Value; Result.Attempts:=Copy(Value.Attempts,0,Length(Value.Attempts));
  for I:=0 to High(Result.Attempts) do
  begin
    Result.Attempts[I].SolveReport:=CloneSolve(Value.Attempts[I].SolveReport);
    Result.Attempts[I].ExcludedAssignment:=Copy(Value.Attempts[I].ExcludedAssignment,0,Length(Value.Attempts[I].ExcludedAssignment));
  end;
  Result.FinalReport:=CloneSolve(Value.FinalReport);
end;
function CloneSelective(const Value: TGraphSelectiveNegotiationReport): TGraphSelectiveNegotiationReport;
begin
  Result:=Value; Result.RequestedRootIndices:=CopyIndices(Value.RequestedRootIndices);
  Result.ActivePassIndices:=CopyIndices(Value.ActivePassIndices); Result.Search:=CloneNegotiation(Value.Search);
end;
procedure ChargeSolve(var Budget: TCaptureBudget; const Report: TGraphSolveReport;
  const Limits: TWfcPipelineSessionOutcomeLimits);
var I: Integer;
begin
  Charge(Budget.Passes,Length(Report.Passes),Limits.MaxReportPassRecords,'report pass');
  Charge(Budget.Events,Length(Report.Trace),Limits.MaxTraceEvents,'trace event');
  for I:=0 to High(Report.Trace) do ChargeToken(Budget,GraphToken(Report.Trace[I].Value),Limits);
  ChargeToken(Budget,GraphToken(Report.TraceDelivery.FailureMessage),Limits);
end;
procedure ChargeNegotiation(var Budget: TCaptureBudget; const Report: TGraphNegotiationReport;
  const Limits: TWfcPipelineSessionOutcomeLimits);
var I: Integer;
begin
  for I:=0 to High(Report.Attempts) do
  begin
    ChargeSolve(Budget,Report.Attempts[I].SolveReport,Limits);
    Charge(Budget.Assignments,Length(Report.Attempts[I].ExcludedAssignment),Limits.MaxExcludedAssignmentItems,'excluded assignment');
  end;
  ChargeSolve(Budget,Report.FinalReport,Limits);
end;

function Contains(const Values: TGraphPassIndices; const Value: Integer): Boolean;
var I: Integer;
begin Result:=False; for I:=0 to High(Values) do if Values[I]=Value then Exit(True); end;
function UnionIndices(const A,B: TGraphPassIndices; const PassCount: Integer): TGraphPassIndices;
var I,N: Integer;
begin
  N:=0; for I:=0 to PassCount-1 do if Contains(A,I) or Contains(B,I) then Inc(N);
  Result:=nil; SetLength(Result,N); N:=0;
  for I:=0 to PassCount-1 do if Contains(A,I) or Contains(B,I) then begin Result[N]:=I; Inc(N); end;
end;
function Difference(const A,B: TGraphPassIndices): TGraphPassIndices;
var I,N: Integer;
begin
  N:=0; for I:=0 to High(A) do if not Contains(B,A[I]) then Inc(N);
  Result:=nil; SetLength(Result,N); N:=0;
  for I:=0 to High(A) do if not Contains(B,A[I]) then begin Result[N]:=A[I]; Inc(N); end;
end;
function SameIndices(const A,B: TGraphPassIndices): Boolean;
var I: Integer;
begin
  Result:=False; if Length(A)<>Length(B) then Exit;
  for I:=0 to High(A) do if A[I]<>B[I] then Exit;
  Result:=True;
end;
function RootLabels(const Data: TSessionData; const Roots: TGraphPassIndices): TGraphPassLabels;
var I: Integer;
begin
  Result:=nil; SetLength(Result,Length(Roots));
  for I:=0 to High(Roots) do Result[I]:=GraphLabel(Data.Recipe.PassAt(Roots[I]).LabelName);
end;
function MakeScope(const Data: TSessionData; const Roots: TGraphPassLabels;
  const Full: Boolean): TWfcPipelineSessionScope;
var Labels: TGraphPassLabels; I: Integer;
begin
  Result:=Default(TWfcPipelineSessionScope);
  Result.ScopeAlgorithmVersion:=WFC_SELECTIVE_NEGOTIATION_ALGORITHM_VERSION;
  { Do not arrayRef/copy raw roots before the core's passive input guard.
    pas2js arrayRef writes bookkeeping even for a const array assignment. }
  if Full then
  begin
    SetLength(Labels,Data.Recipe.PassCount);
    for I:=0 to High(Labels) do Labels[I]:=GraphLabel(Data.Recipe.PassAt(I).LabelName);
  end;
  try
    if Full then
      Data.Binding.BorrowCompiled.Graph.ResolveRegenerationScope(Labels,Result.RequestedRootIndices,Result.ActivePassIndices)
    else
      Data.Binding.BorrowCompiled.Graph.ResolveRegenerationScope(Roots,Result.RequestedRootIndices,Result.ActivePassIndices);
  except
    on E:EOutOfMemory do raise;
    on E:Exception do Fail('repair roots: '+E.Message);
  end;
  if Full then Result.RequestedRootIndices:=nil;
  Result.AuthoredPassIndices:=CopyIndices(Data.Authored);
  Result.RequiredPassIndices:=CopyIndices(Data.Pending);
  Result.MissingPassIndices:=Difference(Data.Pending,Result.ActivePassIndices);
end;

procedure InspectState(const Data: TSessionData; var Budget: TCaptureBudget);
var G,P: TGraph; Saved,I,X,Y,Z,J: Integer; Entry: TGraphEntry;
  Layout: TWfcLatticeLayout; Token: TWfcModelToken; V: TWfcModelTokens; Found: Boolean;
begin
  G:=Data.Binding.BorrowCompiled.Graph; Saved:=G.CurrentPassIndex; G.SwitchToPass(0);
  try
    for I:=0 to Data.Recipe.PassCount-1 do
    begin
      P:=G.PassGraph[I]; Layout:=Data.AppliedRun.PassLayoutAt(I);
      if not SameWfcLatticeLayout(P.PassLayout,Layout) then Fail('actual public layout changed');
      if GraphToken(P.CurrentPass)<>Data.Recipe.PassAt(I).LabelName then Fail('actual pass identity changed');
      if Data.Recipe.PassAt(I).Visibility<>wppvPublic then Continue;
      Charge(Budget.Cells,Data.AppliedRun.PassCellCount(I),Data.Limits.MaxPublicCellRecords,'public cell');
      ChargeToken(Budget,Data.Recipe.PassAt(I).LabelName,Data.Limits);
      V:=Data.Recipe.CopyPublicVocabulary(I);
      for Z:=0 to Layout.Cells.Z-1 do for Y:=0 to Layout.Cells.Y-1 do for X:=0 to Layout.Cells.X-1 do
      begin
        Entry:=P.Entry[X,Y,Z]; if Entry.Empty then Continue;
        Token:=GraphToken(Entry.Value); Found:=False;
        for J:=0 to High(V) do if V[J]=Token then begin Found:=True; Break; end;
        if not Found then Fail('actual public token is outside declared vocabulary');
        ChargeToken(Budget,Token,Data.Limits);
      end;
    end;
  finally G.SwitchToPass(Saved); end;
end;
function CaptureState(const Data: TSessionData): TWfcPipelineSessionLayers;
var G,P: TGraph; Saved,I,N,J,X,Y,Z: Integer; Entry: TGraphEntry; Layout: TWfcLatticeLayout;
begin
  N:=0; for I:=0 to Data.Recipe.PassCount-1 do if Data.Recipe.PassAt(I).Visibility=wppvPublic then Inc(N);
  Result:=nil; SetLength(Result,N); N:=0;
  G:=Data.Binding.BorrowCompiled.Graph; Saved:=G.CurrentPassIndex; G.SwitchToPass(0);
  try
    for I:=0 to Data.Recipe.PassCount-1 do
    begin
      if Data.Recipe.PassAt(I).Visibility<>wppvPublic then Continue;
      P:=G.PassGraph[I]; Layout:=Data.AppliedRun.PassLayoutAt(I);
      Result[N].PassIndex:=I; Result[N].LabelName:=Data.Recipe.PassAt(I).LabelName;
      Result[N].Rank:=Data.AppliedRun.PassTopologyAt(I).Rank; Result[N].Layout:=Layout;
      SetLength(Result[N].Cells,Data.AppliedRun.PassCellCount(I)); J:=0;
      for Z:=0 to Layout.Cells.Z-1 do for Y:=0 to Layout.Cells.Y-1 do for X:=0 to Layout.Cells.X-1 do
      begin
        Entry:=P.Entry[X,Y,Z]; Result[N].Cells[J].Empty:=Entry.Empty;
        Result[N].Cells[J].Generated:=Entry.Generated;
        if not Entry.Empty then Result[N].Cells[J].Token:=GraphToken(Entry.Value)
        else Result[N].Cells[J].Token:='';
        Inc(J);
      end;
      Inc(N);
    end;
  finally G.SwitchToPass(Saved); end;
end;
function StateOwner(const Layers: TWfcPipelineSessionLayers): TWfcPipelineSessionPublicState;
var Data: TStateData;
begin
  Data:=TStateData.Create;
  try Data.Layers:=CloneLayers(Layers); Result:=TWfcPipelineSessionPublicState.CreateOwned(Data); Data:=nil;
  finally Data.Free; end;
end;

constructor TWfcPipelineSessionPublicState.CreateOwned(const AData: TObject);
begin inherited Create; RegisterWrapper(Self,wkState); FData:=AData; end;
destructor TWfcPipelineSessionPublicState.Destroy;
begin UnregisterWrapper(Self); FData.Free; inherited Destroy; end;
function TWfcPipelineSessionPublicState.GetLayerCount: Integer;
begin Result:=Length(TStateData(CheckedData(Self,wkState)).Layers); end;
function TWfcPipelineSessionPublicState.LayerAt(const AIndex: Integer): TWfcPipelineSessionLayer;
var Data: TStateData;
begin
  Data:=TStateData(CheckedData(Self,wkState)); RequireInteger(AIndex,0,Length(Data.Layers)-1,'layer index');
  Result:=Data.Layers[AIndex]; Result.Cells:=Copy(Data.Layers[AIndex].Cells,0,Length(Data.Layers[AIndex].Cells));
end;
function TWfcPipelineSessionPublicState.CopyLayers: TWfcPipelineSessionLayers;
begin Result:=CloneLayers(TStateData(CheckedData(Self,wkState)).Layers); end;

constructor TWfcPipelineSessionRepairPlan.CreateOwned(const AData: TObject);
begin inherited Create; RegisterWrapper(Self,wkPlan); FData:=AData; end;
destructor TWfcPipelineSessionRepairPlan.Destroy;
begin UnregisterWrapper(Self); FData.Free; inherited Destroy; end;
function TWfcPipelineSessionRepairPlan.GetBaseRevision: Integer;
begin Result:=TPlanData(CheckedData(Self,wkPlan)).Revision; end;
function TWfcPipelineSessionRepairPlan.GetMissingBaseline: Boolean;
begin Result:=TPlanData(CheckedData(Self,wkPlan)).MissingBaseline; end;
function TWfcPipelineSessionRepairPlan.GetCanExecute: Boolean;
var Data: TPlanData;
begin Data:=TPlanData(CheckedData(Self,wkPlan)); Result:=not Data.MissingBaseline and (Length(Data.Scope.MissingPassIndices)=0); end;
function TWfcPipelineSessionRepairPlan.CopyScope: TWfcPipelineSessionScope;
begin Result:=CloneScope(TPlanData(CheckedData(Self,wkPlan)).Scope); end;
function TWfcPipelineSessionRepairPlan.CopyInvocation: TWfcPipelineSessionInvocation;
begin Result:=wfc_pipeline_session.CopyInvocation(TPlanData(CheckedData(Self,wkPlan)).Invocation); end;

constructor TWfcPipelineSessionEditOutcome.CreateOwned(const AData: TObject);
begin inherited Create; RegisterWrapper(Self,wkEdit); FData:=AData; end;
destructor TWfcPipelineSessionEditOutcome.Destroy;
begin UnregisterWrapper(Self); FData.Free; inherited Destroy; end;
function TWfcPipelineSessionEditOutcome.GetRevision: Integer;
begin Result:=TEditData(CheckedData(Self,wkEdit)).Revision; end;
function TWfcPipelineSessionEditOutcome.GetHasCurrentOutput: Boolean;
begin Result:=TEditData(CheckedData(Self,wkEdit)).Current; end;
function TWfcPipelineSessionEditOutcome.GetHasSuccessfulBaseline: Boolean;
begin Result:=TEditData(CheckedData(Self,wkEdit)).Baseline; end;
function TWfcPipelineSessionEditOutcome.CopyImpact: TWfcPipelineInputImpact;
begin Result:=CloneImpact(TEditData(CheckedData(Self,wkEdit)).Impact); end;
function TWfcPipelineSessionEditOutcome.CopyPendingPassIndices: TGraphPassIndices;
begin Result:=CopyIndices(TEditData(CheckedData(Self,wkEdit)).Pending); end;
function TWfcPipelineSessionEditOutcome.CopyAuthoredPassIndices: TGraphPassIndices;
begin Result:=CopyIndices(TEditData(CheckedData(Self,wkEdit)).Authored); end;
function TWfcPipelineSessionEditOutcome.CopyPublicState: TWfcPipelineSessionPublicState;
begin Result:=StateOwner(TEditData(CheckedData(Self,wkEdit)).Layers); end;
function TWfcPipelineSessionEditOutcome.CopyInvocation: TWfcPipelineSessionInvocation;
begin Result:=wfc_pipeline_session.CopyInvocation(TEditData(CheckedData(Self,wkEdit)).Invocation); end;

constructor TWfcPipelineSessionOutcome.CreateOwned(const AData: TObject);
begin inherited Create; RegisterWrapper(Self,wkOutcome); FData:=AData; end;
destructor TWfcPipelineSessionOutcome.Destroy;
begin UnregisterWrapper(Self); FData.Free; inherited Destroy; end;
function TWfcPipelineSessionOutcome.GetKind: TWfcPipelineSessionOutcomeKind;
begin Result:=TOutcomeData(CheckedData(Self,wkOutcome)).Kind; end;
function TWfcPipelineSessionOutcome.GetRevision: Integer;
begin Result:=TOutcomeData(CheckedData(Self,wkOutcome)).Revision; end;
function TWfcPipelineSessionOutcome.GetSolved: Boolean;
begin Result:=TOutcomeData(CheckedData(Self,wkOutcome)).Solved; end;
function TWfcPipelineSessionOutcome.GetHasCurrentOutput: Boolean;
begin Result:=TOutcomeData(CheckedData(Self,wkOutcome)).Current; end;
function TWfcPipelineSessionOutcome.GetHasSuccessfulBaseline: Boolean;
begin Result:=TOutcomeData(CheckedData(Self,wkOutcome)).Baseline; end;
function TWfcPipelineSessionOutcome.GetHasNegotiation: Boolean;
begin Result:=TOutcomeData(CheckedData(Self,wkOutcome)).Kind in [wpsokNegotiatedFull,wpsokNegotiatedSelective]; end;
function TWfcPipelineSessionOutcome.GetHasSelectiveNegotiation: Boolean;
begin Result:=TOutcomeData(CheckedData(Self,wkOutcome)).Kind=wpsokNegotiatedSelective; end;
function TWfcPipelineSessionOutcome.GetLastValidation: TWfcPipelineCommitValidation;
begin Result:=TOutcomeData(CheckedData(Self,wkOutcome)).Validation; end;
function TWfcPipelineSessionOutcome.CopyScope: TWfcPipelineSessionScope;
begin Result:=CloneScope(TOutcomeData(CheckedData(Self,wkOutcome)).Scope); end;
function TWfcPipelineSessionOutcome.CopyPendingPassIndices: TGraphPassIndices;
begin Result:=CopyIndices(TOutcomeData(CheckedData(Self,wkOutcome)).Pending); end;
function TWfcPipelineSessionOutcome.CopyAuthoredPassIndices: TGraphPassIndices;
begin Result:=CopyIndices(TOutcomeData(CheckedData(Self,wkOutcome)).Authored); end;
function TWfcPipelineSessionOutcome.CopyPublicState: TWfcPipelineSessionPublicState;
begin Result:=StateOwner(TOutcomeData(CheckedData(Self,wkOutcome)).Layers); end;
function TWfcPipelineSessionOutcome.CopyInvocation: TWfcPipelineSessionInvocation;
begin Result:=wfc_pipeline_session.CopyInvocation(TOutcomeData(CheckedData(Self,wkOutcome)).Invocation); end;
function TWfcPipelineSessionOutcome.CopySolveReport: TGraphSolveReport;
var Data: TOutcomeData;
begin
  Data:=TOutcomeData(CheckedData(Self,wkOutcome));
  case Data.Kind of
    wpsokNegotiatedFull: Result:=CloneSolve(Data.Negotiated.FinalReport);
    wpsokNegotiatedSelective: Result:=CloneSolve(Data.Selective.Search.FinalReport);
  else Result:=CloneSolve(Data.Ordinary); end;
end;
function TWfcPipelineSessionOutcome.CopyNegotiationReport: TGraphNegotiationReport;
var Data: TOutcomeData;
begin
  Data:=TOutcomeData(CheckedData(Self,wkOutcome));
  case Data.Kind of
    wpsokNegotiatedFull: Result:=CloneNegotiation(Data.Negotiated);
    wpsokNegotiatedSelective: Result:=CloneNegotiation(Data.Selective.Search);
  else Fail('outcome has no negotiation report'); end;
end;
function TWfcPipelineSessionOutcome.CopySelectiveNegotiationReport: TGraphSelectiveNegotiationReport;
var Data: TOutcomeData;
begin
  Data:=TOutcomeData(CheckedData(Self,wkOutcome));
  if Data.Kind<>wpsokNegotiatedSelective then Fail('outcome has no selective negotiation report');
  Result:=CloneSelective(Data.Selective);
end;

procedure NextRevision(const Data: TSessionData);
begin if Data.Revision=High(Integer) then Fail('session revision exhausted'); end;
function PreparedPlan(const Data: TSessionData; const Run: TWfcPipelineRun): TWfcPipelineInputPlan;
begin
  try Result:=Data.Preparation.PrepareInputs(Run);
  except on E:EOutOfMemory do raise; on E:Exception do Fail('input preflight: '+E.Message); end;
end;

constructor TWfcPipelinePreparedSession.Create(const ARecipe: TWfcPipelineModel;
  const AInitialRun: TWfcPipelineRun; const AReplacementLimits: TWfcPipelineReplacementLimits;
  const AOutcomeLimits: TWfcPipelineSessionOutcomeLimits);
var Data: TSessionData; Plan: TWfcPipelineInputPlan; Budget: TCaptureBudget;
begin
  inherited Create; RequireLimits(AOutcomeLimits);
  Data:=TSessionData.Create; FData:=Data; Data.Limits:=AOutcomeLimits;
  Data.Recipe:=ARecipe; Data.Identity:=TSessionIdentity.Create; Plan:=nil;
  try
    Data.Preparation:=TWfcPipelinePreparation.Create(ARecipe,AInitialRun);
    Plan:=PreparedPlan(Data,AInitialRun);
    Data.InitialRun:=CopyRun(ARecipe,AInitialRun); Data.AppliedRun:=CopyRun(ARecipe,AInitialRun);
    Budget:=Default(TCaptureBudget); ChargeInvocation(Budget,Data.AppliedRun,Data.Limits);
    Data.Binding:=TWfcPipelineInputBinding.CreateEditable(Data.Preparation,Plan,AReplacementLimits);
    InspectState(Data,Budget); Data.Usable:=True; RegisterWrapper(Self,wkSession);
  finally Plan.Free; end;
end;
destructor TWfcPipelinePreparedSession.Destroy;
begin UnregisterWrapper(Self); FData.Free; inherited Destroy; end;
function TWfcPipelinePreparedSession.GetRevision: Integer;
begin Result:=SessionData(Self,False).Revision; end;
function TWfcPipelinePreparedSession.GetUsable: Boolean;
begin Result:=SessionData(Self,False).Usable; end;
function TWfcPipelinePreparedSession.GetHasSuccessfulBaseline: Boolean;
begin Result:=SessionData(Self,False).Baseline; end;
function TWfcPipelinePreparedSession.GetHasCurrentOutput: Boolean;
var Data: TSessionData;
begin Data:=SessionData(Self,False); Result:=Data.Usable and Data.Current; end;
function TWfcPipelinePreparedSession.CopyPublicState: TWfcPipelineSessionPublicState;
var Data: TSessionData; Budget: TCaptureBudget; State: TStateData;
begin
  Data:=SessionData(Self); Budget:=Default(TCaptureBudget); InspectState(Data,Budget);
  State:=TStateData.Create;
  try State.Layers:=CaptureState(Data); Result:=TWfcPipelineSessionPublicState.CreateOwned(State); State:=nil;
  finally State.Free; end;
end;
function TWfcPipelinePreparedSession.CopyLastSuccessfulState: TWfcPipelineSessionPublicState;
var Data: TSessionData;
begin
  Data:=SessionData(Self,False); Result:=nil;
  if Data.Baseline then Result:=StateOwner(Data.LastSuccessful);
end;
function TWfcPipelinePreparedSession.CopyPendingPassIndices: TGraphPassIndices;
begin Result:=CopyIndices(SessionData(Self,False).Pending); end;
function TWfcPipelinePreparedSession.CopyAppliedRun: TWfcPipelineRun;
var Data: TSessionData;
begin Data:=SessionData(Self,False); Result:=CopyRun(Data.Recipe,Data.AppliedRun); end;

function TWfcPipelinePreparedSession.ApplyInputs(const ADesiredRun: TWfcPipelineRun): TWfcPipelineSessionEditOutcome;
var Data: TSessionData; Plan: TWfcPipelineInputPlan; NewRun: TWfcPipelineRun;
  Edit: TEditData; Impact: TWfcPipelineInputImpact; Budget: TCaptureBudget;
begin
  Result:=nil; Data:=SessionData(Self); NextRevision(Data);
  Plan:=nil; NewRun:=nil; Edit:=nil;
  try
    Plan:=PreparedPlan(Data,ADesiredRun); NewRun:=CopyRun(Data.Recipe,ADesiredRun);
    Budget:=Default(TCaptureBudget); ChargeInvocation(Budget,NewRun,Data.Limits);
    Edit:=TEditData.Create;
    { ReplaceInputs distinguishes preflight rejection from a poisoned write. }
    try Impact:=Data.Binding.ReplaceInputs(Plan);
    except
      on E:Exception do
      begin if not Data.Binding.Usable then Data.Usable:=False; raise; end;
    end;
    try
      Data.AppliedRun.Free; Data.AppliedRun:=NewRun; NewRun:=nil;
      Inc(Data.Revision);
      Data.Pending:=UnionIndices(Data.Pending,Impact.ChangedPassIndices,Data.Recipe.PassCount);
      Data.Authored:=UnionIndices(Data.Authored,Impact.AuthoredPassIndices,Data.Recipe.PassCount);
      if Impact.AuthoredInputsChanged or Impact.GraphInputsChanged then Data.Current:=False;
      InspectState(Data,Budget);
      Edit.Revision:=Data.Revision; Edit.Current:=Data.Current; Edit.Baseline:=Data.Baseline;
      Edit.Impact:=CloneImpact(Impact); Edit.Pending:=CopyIndices(Data.Pending);
      Edit.Authored:=CopyIndices(Data.Authored); Edit.Invocation:=Invocation(Data.AppliedRun);
      Edit.Layers:=CaptureState(Data);
      Result:=TWfcPipelineSessionEditOutcome.CreateOwned(Edit); Edit:=nil;
    except Data.Usable:=False; raise; end;
  finally Edit.Free; NewRun.Free; Plan.Free; end;
end;

function TWfcPipelinePreparedSession.PlanRepair(const ADesiredRun: TWfcPipelineRun;
  const ARequestedRoots: TGraphPassLabels): TWfcPipelineSessionRepairPlan;
var Data: TSessionData; Input: TWfcPipelineInputPlan; Plan: TPlanData;
  Budget: TCaptureBudget;
begin
  Result:=nil; Data:=SessionData(Self); Input:=nil; Plan:=nil;
  try
    Input:=PreparedPlan(Data,ADesiredRun);
    if not SameInputs(ADesiredRun,Data.AppliedRun) then Fail('repair inputs differ from applied inputs; ApplyInputs first');
    Budget:=Default(TCaptureBudget); ChargeInvocation(Budget,ADesiredRun,Data.Limits);
    Plan:=TPlanData.Create; Plan.Scope:=MakeScope(Data,ARequestedRoots,False);
    Plan.Revision:=Data.Revision; Plan.MissingBaseline:=not Data.Baseline;
    Plan.Invocation:=Invocation(ADesiredRun); Data.Identity.Retain; Plan.Identity:=Data.Identity;
    Result:=TWfcPipelineSessionRepairPlan.CreateOwned(Plan); Plan:=nil;
  finally Plan.Free; Input.Free; end;
end;

function ExecuteAction(const Data: TSessionData; const Scope: TWfcPipelineSessionScope;
  const Selective: Boolean): TWfcPipelineSessionOutcome;
var Output: TOutcomeData; Budget: TCaptureBudget; SolveOptions: TGraphSolveOptions;
  NegotiationOptions: TGraphNegotiationOptions; Solve: TGraphSolveReport;
  Negotiated: TGraphNegotiationReport; Selection: TGraphSelectiveNegotiationReport;
  Labels: TGraphPassLabels; Solved: Boolean;
begin
  Result:=nil; Output:=nil;
  Solve:=Default(TGraphSolveReport); Negotiated:=Default(TGraphNegotiationReport);
  Selection:=Default(TGraphSelectiveNegotiationReport);
  SolveOptions:=Default(TGraphSolveOptions); SolveOptions.MaxBacktracks:=Data.AppliedRun.MaxBacktracks;
  SolveOptions.CaptureTrace:=Data.AppliedRun.CaptureTrace;
  NegotiationOptions:=Default(TGraphNegotiationOptions); NegotiationOptions.SolveOptions:=SolveOptions;
  NegotiationOptions.MaxPassBacktracks:=Data.AppliedRun.MaxPassBacktracks;
  Labels:=RootLabels(Data,Scope.RequestedRootIndices);
  try
    try
      if Data.AppliedRun.Strategy=wpssOneWay then
      begin
        if Selective then Solved:=Data.Binding.BorrowCompiled.Graph.TryRegenerateFrom(Labels,SolveOptions,Solve)
        else Solved:=Data.Binding.BorrowCompiled.Graph.TrySolve(SolveOptions,Solve);
      end
      else
      begin
        if Selective then
        begin
          Solved:=Data.Binding.BorrowCompiled.Graph.TryRegenerateNegotiatedFrom(Labels,NegotiationOptions,Selection);
          if not SameIndices(Selection.RequestedRootIndices,Scope.RequestedRootIndices) or
            not SameIndices(Selection.ActivePassIndices,Scope.ActivePassIndices) then Fail('actual selective scope differs from authorization');
        end
        else Solved:=Data.Binding.BorrowCompiled.Graph.TrySolveNegotiated(NegotiationOptions,Negotiated);
      end;
      Inc(Data.Revision);
      if Solved then
      begin
        Data.Pending:=Difference(Data.Pending,Scope.ActivePassIndices);
        if Length(Data.Pending)=0 then Data.Authored:=nil;
        Data.Current:=Length(Data.Pending)=0;
      end
      else
      begin
        Data.Pending:=UnionIndices(Data.Pending,Scope.ActivePassIndices,Data.Recipe.PassCount);
        Data.Current:=False;
      end;
      Budget:=Default(TCaptureBudget); ChargeInvocation(Budget,Data.AppliedRun,Data.Limits);
      InspectState(Data,Budget);
      if Data.AppliedRun.Strategy=wpssOneWay then ChargeSolve(Budget,Solve,Data.Limits)
      else if Selective then ChargeNegotiation(Budget,Selection.Search,Data.Limits)
      else ChargeNegotiation(Budget,Negotiated,Data.Limits);
      Output:=TOutcomeData.Create; Output.Solved:=Solved;
      Output.Revision:=Data.Revision; Output.Current:=Data.Current; Output.Baseline:=Data.Baseline or Solved;
      Output.Pending:=CopyIndices(Data.Pending); Output.Authored:=CopyIndices(Data.Authored);
      Output.Invocation:=Invocation(Data.AppliedRun); Output.Scope:=CloneScope(Scope);
      Output.Layers:=CaptureState(Data); Output.Validation:=Data.Binding.BorrowCompiled.LastValidation;
      if Data.AppliedRun.Strategy=wpssOneWay then
      begin
        if Selective then Output.Kind:=wpsokOrdinarySelective else Output.Kind:=wpsokOrdinaryFull;
        Output.Ordinary:=CloneSolve(Solve);
      end
      else if Selective then begin Output.Kind:=wpsokNegotiatedSelective; Output.Selective:=CloneSelective(Selection); end
      else begin Output.Kind:=wpsokNegotiatedFull; Output.Negotiated:=CloneNegotiation(Negotiated); end;
      if Solved then
      begin
        Data.LastSuccessful:=CloneLayers(Output.Layers);
        Data.Baseline:=True;
      end;
      Result:=TWfcPipelineSessionOutcome.CreateOwned(Output); Output:=nil;
    except Data.Usable:=False; raise; end;
  finally Output.Free; end;
end;

function TWfcPipelinePreparedSession.ExecuteInitial: TWfcPipelineSessionOutcome;
var Data: TSessionData; Scope: TWfcPipelineSessionScope;
begin
  Data:=SessionData(Self); NextRevision(Data);
  if Data.InitialAttempted then Fail('initial execution has already been attempted');
  Scope:=MakeScope(Data,nil,True); Data.InitialAttempted:=True;
  try Result:=ExecuteAction(Data,Scope,False);
  except Data.Usable:=False; raise; end;
end;

function TWfcPipelinePreparedSession.ExecuteRepair(const APlan: TWfcPipelineSessionRepairPlan): TWfcPipelineSessionOutcome;
var Data: TSessionData; Plan: TPlanData; Scope: TWfcPipelineSessionScope;
  Run: TWfcPipelineRun; Input: TWfcPipelineInputPlan;
begin
  Data:=SessionData(Self); NextRevision(Data); Plan:=TPlanData(CheckedData(APlan,wkPlan));
  if Plan.Identity<>Data.Identity then Fail('repair plan belongs to a different session lifetime');
  if Plan.Revision<>Data.Revision then Fail('repair plan revision is stale');
  if Plan.MissingBaseline or not Data.Baseline then Fail('repair requires a successful baseline');
  if Length(Plan.Scope.MissingPassIndices)<>0 then Fail('repair scope does not cover pending passes');
  Run:=nil; Input:=nil;
  try
    Run:=RunFromInvocation(Data.Recipe,Plan.Invocation); Input:=PreparedPlan(Data,Run);
    if not SameInputs(Run,Data.AppliedRun) then Fail('repair plan inputs no longer match applied inputs');
    Scope:=MakeScope(Data,RootLabels(Data,Plan.Scope.RequestedRootIndices),False);
    if not SameIndices(Scope.RequestedRootIndices,Plan.Scope.RequestedRootIndices) or
      not SameIndices(Scope.ActivePassIndices,Plan.Scope.ActivePassIndices) or
      not SameIndices(Scope.RequiredPassIndices,Plan.Scope.RequiredPassIndices) or
      not SameIndices(Scope.AuthoredPassIndices,Plan.Scope.AuthoredPassIndices) or
      not SameIndices(Scope.MissingPassIndices,Plan.Scope.MissingPassIndices) or
      (Scope.ScopeAlgorithmVersion<>Plan.Scope.ScopeAlgorithmVersion) then Fail('repair plan context changed');
    Data.AppliedRun.Free; Data.AppliedRun:=Run; Run:=nil;
    try Result:=ExecuteAction(Data,Scope,True);
    except Data.Usable:=False; raise; end;
  finally Input.Free; Run.Free; end;
end;

end.
