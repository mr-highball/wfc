{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
unit mapped_world_workbench;
{$mode delphi}{$H+}
interface
uses SysUtils, wfc, mapped_world_types;
type
  TMappedWorldLayerSet = set of TMappedWorldLayer;
  TMappedWorldReport = record
    Status: TMappedWorldStatus;
    Action: TMappedWorldAction;
    Options: TMappedWorldSearchOptions;
    Seed: TGraphSeed;
    Revision: Integer;
    Detail: String;
    RequestedRootIndices, ActivePassIndices: TGraphPassIndices;
    ScopeAlgorithmVersion: Integer;
    Rounds, PassBacktracks, Decisions, Propagations, Contradictions,
      Backtracks, ExcludedAssignments: Integer;
    FailedPass, FailedCell, DependencyPass: Integer;
    FailureKind: TGraphContradictionKind;
    //Counters aggregate all rounds; detailed trace slices live in SolveReport
    //and each NegotiationReport attempt, not this aggregate array.
    Passes: TGraphPassSolveReports;
    //Selective TranscriptHash binds the requested roots and active closure.
    //The nested NegotiationReport retains its inner chronological hash.
    TraceHash, TranscriptHash: TGraphTraceSignature;
    Validation: TMappedWorldValidation;
    SolveReport: TGraphSolveReport;
    NegotiationReport: TGraphNegotiationReport;
  end;

  { Owns the only mutable graph. Current artifacts and the retained repair
    baseline are deliberately separate; neither graph nor mutable captures
    are exposed. Independent model validation runs inside graph commit. }
  TMappedWorldSession = class
  private
    FGraph: TGraph;
    FConfig: TMappedWorldConfig;
    FEdits: TMappedWorldLayers;
    FDemands: TMappedWorldDemands;
    FDirtyRoots: TMappedWorldLayerSet;
    FHasBaseline, FHasCurrent: Boolean;
    FBaseline, FCandidate: TMappedWorldResult;
    FStatus: TMappedWorldStatus;
    FRevision, FAttemptRevision: Integer;
    FReport: TMappedWorldReport;
    FCommitValidation: TMappedWorldValidation;
    procedure RequireIdle;
    procedure PrepareEdit;
    procedure Changed(const ALayer: TMappedWorldLayer);
    function BuildGraph(const AConfig: TMappedWorldConfig;
      out AEdits: TMappedWorldLayers; out ADemands: TMappedWorldDemands): TGraph;
    procedure ApplyDomain(const AGraph: TGraph; const AConfig: TMappedWorldConfig;
      const ALayer: TMappedWorldLayer; const AX, AY: Integer;
      const AEdit: TMappedWorldCell; const ADemand: TMappedWorldDemand);
    function CaptureGraph: TMappedWorldResult;
    function ValidateCommit(out AFailedPass, AFailedCell: Integer): Boolean;
    procedure CollectSolveReport(const AReport: TGraphSolveReport);
    procedure SetFinalReport(const AReport: TGraphSolveReport);
  public
    constructor Create(const AConfig: TMappedWorldConfig);
    destructor Destroy; override;
    procedure Reset(const AConfig: TMappedWorldConfig);
    procedure SetDemand(const AX, AY: Integer; const AValue: TMappedWorldDemand);
    procedure SetDomain(const ALayer: TMappedWorldLayer; const AX, AY: Integer;
      const AValues: TGraphValues);
    procedure ClearDomain(const ALayer: TMappedWorldLayer; const AX, AY: Integer);
    procedure SetLock(const ALayer: TMappedWorldLayer; const AX, AY: Integer;
      const AValue: TGraphValue);
    procedure ClearLock(const ALayer: TMappedWorldLayer; const AX, AY: Integer);
    function GetDemand(const AX, AY: Integer): TMappedWorldDemand;
    function CopyDemands: TMappedWorldDemands;
    function Run(const AAction: TMappedWorldAction;
      const AOptions: TMappedWorldSearchOptions): Boolean;
    function CopyCurrent(out AResult: TMappedWorldResult): Boolean;
    function CopyInspection(const AX, AY: Integer;
      out AResult: TMappedWorldInspection): Boolean;
    function CopyReport: TMappedWorldReport;
    function RunReportText: String;
    function TryCurrentSvg(out AText: String): Boolean; overload;
    function TryCurrentSvg(const AX, AY: Integer; out AText: String): Boolean; overload;
    function TryDiagnosticSvg(out AText: String): Boolean; overload;
    function TryDiagnosticSvg(const AX, AY: Integer; out AText: String): Boolean; overload;
    property Config: TMappedWorldConfig read FConfig;
    property HasBaseline: Boolean read FHasBaseline;
    property HasCurrent: Boolean read FHasCurrent;
    property Status: TMappedWorldStatus read FStatus;
  end;

function MappedWorldStatusName(const AValue: TMappedWorldStatus): String;
function MappedWorldActionName(const AValue: TMappedWorldAction): String;
function MappedWorldPresetName(const AValue: TMappedWorldPreset): String;
function MappedWorldSamplingName(const AValue: TMappedWorldSampling): String;
function MappedWorldDemandName(const AValue: TMappedWorldDemand): String;
function MappedWorldReportText(const AReport: TMappedWorldReport): String;
function MappedWorldSelfTest: Integer;
implementation
uses wfc_lattice, mapped_world_validation, mapped_world_svg;
type
  TMappedWorldCommitCheck = function(out AFailedPass, AFailedCell: Integer): Boolean of object;
  TCommitCheckedGraph = class(TGraph)
  public
    CommitCheck: TMappedWorldCommitCheck;
  protected
    function DoValidateCommit(out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean; override;
  end;

function TCommitCheckedGraph.DoValidateCommit(out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
begin
  Result:=inherited DoValidateCommit(AFailedPassIndex,AFailedEntryIndex);
  if Result and Assigned(CommitCheck) then Result:=CommitCheck(AFailedPassIndex,AFailedEntryIndex);
end;

function List(const A: array of String): TGraphValues;
var I: Integer;
begin Result:=nil; SetLength(Result,Length(A)); for I:=0 to High(A) do Result[I]:=A[I]; end;

function Contains(const A: TGraphValues; const V: String): Boolean;
var I: Integer;
begin Result:=False; for I:=0 to High(A) do if A[I]=V then Exit(True); end;

function CanonicalDomain(const ALayer: TMappedWorldLayer; const AValues: TGraphValues): TGraphValues;
var I,N: Integer; Tokens: TGraphValues;
begin
  Tokens:=MappedWorldTokens(ALayer);
  {$IFDEF PAS2JS}
  asm
    if (!Array.isArray(AValues)) throw pas.mapped_world_types.EMappedWorld.$create('Create',['domain must be an array']);
  end;
  {$ENDIF}
  for I:=0 to High(AValues) do
    if not MappedWorldTokenValid(ALayer,AValues[I]) then raise EMappedWorld.Create('domain token is outside the layer vocabulary');
  Result:=nil;
  for I:=0 to High(Tokens) do if Contains(AValues,Tokens[I]) then begin
    N:=Length(Result); SetLength(Result,N+1); Result[N]:=Tokens[I];
  end;
end;

function MappedWorldStatusName(const AValue: TMappedWorldStatus): String;
begin
  RequireMappedWorldInteger(Ord(AValue),Ord(Low(TMappedWorldStatus)),Ord(High(TMappedWorldStatus)),'status');
  case AValue of
    mwstIdle:Result:='idle'; mwstDirty:Result:='dirty'; mwstSolving:Result:='solving';
    mwstSolved:Result:='solved'; mwstContradiction:Result:='contradiction';
    mwstLocalLimit:Result:='local-limit'; mwstPassLimit:Result:='pass-limit';
    mwstInvalidConfiguration:Result:='invalid-configuration'; mwstScopeMismatch:Result:='scope-mismatch';
    mwstUnexpectedError:Result:='unexpected-error';
  end;
end;

function MappedWorldActionName(const AValue: TMappedWorldAction): String;
begin
  RequireMappedWorldInteger(Ord(AValue),0,3,'action');
  case AValue of mwaGenerate:Result:='generate'; mwaHousingOnly:Result:='housing-only';
    mwaFoliageAndHousing:Result:='foliage-and-housing'; mwaAllPasses:Result:='all-passes'; end;
end;

function MappedWorldPresetName(const AValue: TMappedWorldPreset): String;
begin
  RequireMappedWorldInteger(Ord(AValue),0,1,'preset');
  if AValue=mwpInteriorStudy then Result:='interior-study' else Result:='landscape-sandbox';
end;

function MappedWorldSamplingName(const AValue: TMappedWorldSampling): String;
begin
  RequireMappedWorldInteger(Ord(AValue),0,2,'sampling');
  case AValue of mwsCell:Result:='cell'; mwsPointStudy:Result:='point-study'; mwsRegion:Result:='region'; end;
end;

function MappedWorldDemandName(const AValue: TMappedWorldDemand): String;
begin
  RequireMappedWorldInteger(Ord(AValue),0,2,'demand');
  case AValue of mwdVacant:Result:='vacant'; mwdOptional:Result:='optional'; mwdRequired:Result:='required'; end;
end;

function CopySolve(const A: TGraphSolveReport): TGraphSolveReport;
begin
  Result:=A; Result.Passes:=Copy(A.Passes,0,Length(A.Passes));
  Result.ExecutionOrder:=Copy(A.ExecutionOrder,0,Length(A.ExecutionOrder));
  Result.Trace:=Copy(A.Trace,0,Length(A.Trace));
end;

function CopyNegotiation(const A: TGraphNegotiationReport): TGraphNegotiationReport;
var I: Integer;
begin
  Result:=A; Result.FinalReport:=CopySolve(A.FinalReport);
  Result.Attempts:=nil; SetLength(Result.Attempts,Length(A.Attempts));
  for I:=0 to High(A.Attempts) do begin
    Result.Attempts[I]:=A.Attempts[I];
    Result.Attempts[I].SolveReport:=CopySolve(A.Attempts[I].SolveReport);
    Result.Attempts[I].ExcludedAssignment:=Copy(A.Attempts[I].ExcludedAssignment,0,Length(A.Attempts[I].ExcludedAssignment));
  end;
end;

function RootNames(const A: TGraphPassIndices): String;
var I: Integer;
begin
  Result:=''; for I:=0 to High(A) do begin
    if I>0 then Result:=Result+',';
    if (A[I]>=0) and (A[I]<=2) then Result:=Result+MappedWorldLayerName(TMappedWorldLayer(A[I]))
    else Result:=Result+IntToStr(A[I]);
  end;
  if Result='' then Result:='none';
end;

function MappedWorldReportText(const AReport: TMappedWorldReport): String;
var I: Integer;
begin
  Result:='status='+MappedWorldStatusName(AReport.Status)+' action='+MappedWorldActionName(AReport.Action)+#10+
    'seed='+UIntToStr(AReport.Seed)+' revision='+IntToStr(AReport.Revision)+#10+
    'negotiated='+BoolToStr(AReport.Options.Negotiated,True)+
    ' local-limit-per-pass='+IntToStr(AReport.Options.MaxBacktracks)+
    ' pass-limit='+IntToStr(AReport.Options.MaxPassBacktracks)+
    ' trace='+BoolToStr(AReport.Options.CaptureTrace,True)+#10+
    'requested='+RootNames(AReport.RequestedRootIndices)+' active='+RootNames(AReport.ActivePassIndices)+#10+
    'rounds='+IntToStr(AReport.Rounds)+' pass-backtracks='+IntToStr(AReport.PassBacktracks)+
    '/'+IntToStr(AReport.Options.MaxPassBacktracks)+' local-backtracks='+IntToStr(AReport.Backtracks)+#10+
    'decisions='+IntToStr(AReport.Decisions)+' propagations='+IntToStr(AReport.Propagations)+
    ' contradictions='+IntToStr(AReport.Contradictions)+' exclusions='+IntToStr(AReport.ExcludedAssignments)+#10+
    'failed-pass='+IntToStr(AReport.FailedPass)+' failed-cell='+IntToStr(AReport.FailedCell)+
    ' dependency-pass='+IntToStr(AReport.DependencyPass)+' failure-kind='+IntToStr(Ord(AReport.FailureKind))+#10+
    'trace='+IntToHex(AReport.TraceHash,8)+' transcript='+IntToHex(AReport.TranscriptHash,8)+#10;
  for I:=0 to High(AReport.Passes) do
    Result:=Result+'pass '+IntToStr(I)+' decisions='+IntToStr(AReport.Passes[I].Decisions)+
      ' backtracks='+IntToStr(AReport.Passes[I].Backtracks)+' exclusions='+IntToStr(AReport.Passes[I].ExcludedAssignments)+#10;
  Result:=Result+AReport.Detail;
end;

constructor TMappedWorldSession.Create(const AConfig: TMappedWorldConfig);
begin inherited Create; Reset(AConfig); end;

destructor TMappedWorldSession.Destroy;
begin FGraph.Free; inherited Destroy; end;

procedure TMappedWorldSession.RequireIdle;
begin if FStatus=mwstSolving then raise EMappedWorld.Create('session operation is already running'); end;

procedure TMappedWorldSession.PrepareEdit;
begin
  RequireIdle;
  if FRevision=High(Integer) then raise EMappedWorld.Create('session revision exhausted; create a new session');
end;

procedure TMappedWorldSession.Changed(const ALayer: TMappedWorldLayer);
begin
  Inc(FRevision); Include(FDirtyRoots,ALayer); FHasCurrent:=False; FStatus:=mwstDirty;
  FReport:=Default(TMappedWorldReport); FReport.Status:=FStatus; FReport.Seed:=FConfig.Seed;
  FReport.Revision:=FRevision; FReport.FailedPass:=-1; FReport.FailedCell:=-1; FReport.DependencyPass:=-1;
  FReport.Detail:='Caller edits invalidate current artifacts; choose a scope covering every edited layer.';
end;

procedure TMappedWorldSession.ApplyDomain(const AGraph: TGraph; const AConfig: TMappedWorldConfig;
  const ALayer: TMappedWorldLayer; const AX, AY: Integer; const AEdit: TMappedWorldCell;
  const ADemand: TMappedWorldDemand);
var Domain: TGraphValues; Present: Boolean; Pass: TGraph; Wanted: String;
begin
  Present:=AEdit.HasDomain; Domain:=AEdit.Domain;
  if (not Present) and (AConfig.Preset=mwpInteriorStudy) then begin
    if ALayer=mwlTerrain then begin Present:=True; Domain:=List(['land']); end
    else if (ALayer=mwlFoliage) and ((AX<>7) or (AY<>7)) then begin Present:=True; Domain:=List(['clear']); end;
  end;
  if (ALayer=mwlHousing) and (ADemand<>mwdOptional) then begin
    if ADemand=mwdRequired then Wanted:='house' else Wanted:='vacant';
    if (not Present) or Contains(Domain,Wanted) then Domain:=List([Wanted]) else Domain:=nil;
    Present:=True;
  end;
  Pass:=AGraph.PassGraph[Ord(ALayer)];
  if Present then Pass.SetAllowedValues(AX,AY,0,Domain) else Pass.ClearAllowedValues(AX,AY,0);
end;

function TMappedWorldSession.BuildGraph(const AConfig: TMappedWorldConfig;
  out AEdits: TMappedWorldLayers; out ADemands: TMappedWorldDemands): TGraph;
var L: TMappedWorldLayer; Layouts: TWfcLatticeLayouts; Shape: TWfcLatticeLayout;
  X,Y,I: Integer; Query: TGraphPassMapQuery;
begin
  ValidateMappedWorldConfig(AConfig); AEdits:=Default(TMappedWorldLayers);
  if AConfig.Preset=mwpInteriorStudy then for I:=0 to 5 do ADemands[I]:=mwdVacant
  else for I:=0 to 5 do ADemands[I]:=mwdOptional;
  Result:=TCommitCheckedGraph.Create;
  try
    Result.Seed:=AConfig.Seed; Result.Reshape(1,1,1);
    Result.CurrentPass:='terrain'; Result.PassMode:=gpmOverlay;
    if AConfig.Preset=mwpInteriorStudy then begin Result.AddValue('land'); Result.AddValue('water'); end
    else begin Result.AddValue('land',AConfig.LandWeight); Result.AddValue('water',AConfig.WaterWeight); end;
    Result.SwitchToPass('foliage'); Result.PassMode:=gpmOverlay; Result.ClearDependencies;
    if AConfig.Preset=mwpInteriorStudy then begin Result.AddValue('tree'); Result.AddValue('clear'); end
    else begin Result.AddValue('tree',AConfig.TreeWeight); Result.AddValue('clear',AConfig.ClearWeight); end;
    Result.SwitchToPass('housing'); Result.PassMode:=gpmOverlay; Result.ClearDependencies;
    Result.AddValue('house'); Result.AddValue('vacant');
    SetLength(Layouts,3);
    for L:=Low(TMappedWorldLayer) to High(TMappedWorldLayer) do begin
      Shape:=MappedWorldLayout(L); Layouts[Ord(L)]:=Shape; AEdits[L].Layout:=Shape;
      SetLength(AEdits[L].Cells,Shape.Cells.X*Shape.Cells.Y);
    end;
    Result.ConfigurePassLayouts(Layouts);
    Result.PassGraph[1].Rules['tree'].RequireMappedFromPass('terrain',
      MakeGraphPassPointQuery(MakeGraphOffset(0,0,0),List(['land'])));
    Result.Rules['house'].RequireMappedFromPass('terrain',MakeGraphPassCellQuery(List(['land'])));
    case AConfig.Sampling of
      mwsCell: Query:=MakeGraphPassCellQuery(List(['clear']));
      mwsPointStudy: Query:=MakeGraphPassPointQuery(MakeGraphOffset(0,0,0),List(['clear']));
      mwsRegion: Query:=MakeGraphPassRegionQuery(AConfig.RegionMinimum,AConfig.RegionMaximum,List(['clear']));
    end;
    Result.Rules['house'].RequireMappedFromPass('foliage',Query);
    for L:=Low(TMappedWorldLayer) to High(TMappedWorldLayer) do begin
      Shape:=Layouts[Ord(L)];
      for Y:=0 to Shape.Cells.Y-1 do for X:=0 to Shape.Cells.X-1 do begin
        I:=Y*Shape.Cells.X+X;
        if L=mwlHousing then ApplyDomain(Result,AConfig,L,X,Y,AEdits[L].Cells[I],ADemands[I])
        else ApplyDomain(Result,AConfig,L,X,Y,AEdits[L].Cells[I],mwdOptional);
      end;
    end;
    TCommitCheckedGraph(Result).CommitCheck:=ValidateCommit;
  except Result.Free; raise; end;
end;

procedure TMappedWorldSession.Reset(const AConfig: TMappedWorldConfig);
var NewGraph,OldGraph: TGraph; Edits: TMappedWorldLayers; Demands: TMappedWorldDemands;
begin
  RequireIdle;
  NewGraph:=BuildGraph(AConfig,Edits,Demands);
  OldGraph:=FGraph; FGraph:=NewGraph; FConfig:=AConfig; FEdits:=Edits; FDemands:=Demands;
  FRevision:=0; FAttemptRevision:=0; FHasBaseline:=False; FHasCurrent:=False;
  FBaseline:=Default(TMappedWorldResult); FCandidate:=Default(TMappedWorldResult);
  FDirtyRoots:=[]; FStatus:=mwstIdle; FCommitValidation:=Default(TMappedWorldValidation);
  FReport:=Default(TMappedWorldReport); FReport.Status:=FStatus; FReport.Seed:=FConfig.Seed;
  FReport.FailedPass:=-1; FReport.FailedCell:=-1; FReport.DependencyPass:=-1;
  FReport.Detail:='Generate a baseline before applying a selective repair.';
  OldGraph.Free;
end;

procedure TMappedWorldSession.SetDemand(const AX, AY: Integer; const AValue: TMappedWorldDemand);
var I: Integer;
begin
  RequireMappedWorldInteger(Ord(AValue),0,2,'demand'); I:=MappedWorldCellIndex(mwlHousing,AX,AY);
  PrepareEdit; if FDemands[I]=AValue then Exit;
  ApplyDomain(FGraph,FConfig,mwlHousing,AX,AY,FEdits[mwlHousing].Cells[I],AValue);
  FDemands[I]:=AValue; Changed(mwlHousing);
end;

procedure TMappedWorldSession.SetDomain(const ALayer: TMappedWorldLayer; const AX, AY: Integer;
  const AValues: TGraphValues);
var I,J: Integer; Edit: TMappedWorldCell; Same: Boolean; Demand: TMappedWorldDemand;
begin
  I:=MappedWorldCellIndex(ALayer,AX,AY); PrepareEdit;
  Edit:=FEdits[ALayer].Cells[I]; Edit.Domain:=CanonicalDomain(ALayer,AValues); Edit.HasDomain:=True;
  Same:=FEdits[ALayer].Cells[I].HasDomain and (Length(Edit.Domain)=Length(FEdits[ALayer].Cells[I].Domain));
  if Same then for J:=0 to High(Edit.Domain) do Same:=Same and (Edit.Domain[J]=FEdits[ALayer].Cells[I].Domain[J]);
  if Same then Exit;
  Demand:=mwdOptional; if ALayer=mwlHousing then Demand:=FDemands[I];
  ApplyDomain(FGraph,FConfig,ALayer,AX,AY,Edit,Demand);
  FEdits[ALayer].Cells[I]:=Edit; Changed(ALayer);
end;

procedure TMappedWorldSession.ClearDomain(const ALayer: TMappedWorldLayer; const AX, AY: Integer);
var I: Integer; Edit: TMappedWorldCell; Demand: TMappedWorldDemand;
begin
  I:=MappedWorldCellIndex(ALayer,AX,AY); PrepareEdit; Edit:=FEdits[ALayer].Cells[I];
  if not Edit.HasDomain then Exit;
  Edit.HasDomain:=False; Edit.Domain:=nil;
  Demand:=mwdOptional; if ALayer=mwlHousing then Demand:=FDemands[I];
  ApplyDomain(FGraph,FConfig,ALayer,AX,AY,Edit,Demand);
  FEdits[ALayer].Cells[I]:=Edit; Changed(ALayer);
end;

procedure TMappedWorldSession.SetLock(const ALayer: TMappedWorldLayer; const AX, AY: Integer;
  const AValue: TGraphValue);
var I: Integer;
begin
  I:=MappedWorldCellIndex(ALayer,AX,AY);
  if not MappedWorldTokenValid(ALayer,AValue) then raise EMappedWorld.Create('lock token is outside the layer vocabulary');
  PrepareEdit;
  if FEdits[ALayer].Cells[I].Locked and (FEdits[ALayer].Cells[I].LockValue=AValue) then Exit;
  //Only explicit caller lock requests assign entry values. Generated results
  //are never reconstructed or rewritten by this owner.
  FGraph.PassGraph[Ord(ALayer)].Entry[AX,AY,0].Value:=AValue;
  FEdits[ALayer].Cells[I].Locked:=True; FEdits[ALayer].Cells[I].LockValue:=AValue;
  Changed(ALayer);
end;

procedure TMappedWorldSession.ClearLock(const ALayer: TMappedWorldLayer; const AX, AY: Integer);
var I: Integer;
begin
  I:=MappedWorldCellIndex(ALayer,AX,AY); PrepareEdit;
  if not FEdits[ALayer].Cells[I].Locked then Exit;
  FGraph.PassGraph[Ord(ALayer)].Entry[AX,AY,0].ClearValue;
  FEdits[ALayer].Cells[I].Locked:=False; FEdits[ALayer].Cells[I].LockValue:='';
  Changed(ALayer);
end;

function TMappedWorldSession.GetDemand(const AX, AY: Integer): TMappedWorldDemand;
begin Result:=FDemands[MappedWorldCellIndex(mwlHousing,AX,AY)]; end;

function TMappedWorldSession.CopyDemands: TMappedWorldDemands;
begin Result:=FDemands; end;

function TMappedWorldSession.CaptureGraph: TMappedWorldResult;
var L: TMappedWorldLayer; X,Y,I: Integer; Shape: TWfcLatticeLayout; Pass: TGraph; E: TGraphEntry;
begin
  Result:=Default(TMappedWorldResult); Result.Config:=FConfig;
  Result.ModelVersion:=MAPPED_WORLD_MODEL_VERSION; Result.MappingVersion:=WFC_PASS_MAPPING_VERSION;
  Result.Revision:=FAttemptRevision; Result.Demands:=FDemands;
  for L:=Low(TMappedWorldLayer) to High(TMappedWorldLayer) do begin
    Pass:=FGraph.PassGraph[Ord(L)]; Shape:=Pass.PassLayout; Result.Layers[L].Layout:=Shape;
    SetLength(Result.Layers[L].Cells,Shape.Cells.X*Shape.Cells.Y);
    for Y:=0 to Shape.Cells.Y-1 do for X:=0 to Shape.Cells.X-1 do begin
      I:=Y*Shape.Cells.X+X; E:=Pass.Entry[X,Y,0];
      with Result.Layers[L].Cells[I] do begin
        Value:=E.Value; Generated:=E.Generated; Locked:=FEdits[L].Cells[I].Locked;
        LockValue:=FEdits[L].Cells[I].LockValue;
        HasDomain:=Pass.HasAllowedValues(X,Y,0);
        if HasDomain then Domain:=Pass.CopyAllowedValues(X,Y,0);
      end;
    end;
  end;
end;

function TMappedWorldSession.ValidateCommit(out AFailedPass, AFailedCell: Integer): Boolean;
var Candidate: TMappedWorldResult; I: Integer;
begin
  Candidate:=CaptureGraph; Result:=AnalyzeMappedWorldResult(Candidate,FCommitValidation);
  AFailedPass:=-1; AFailedCell:=-1;
  if Result then begin
    Candidate.ModelValid:=FCommitValidation.ModelValid; Candidate.PhysicalSafe:=FCommitValidation.PhysicalSafe;
    FCandidate:=Candidate;
  end else for I:=0 to High(FCommitValidation.Issues) do
    if not (FCommitValidation.Issues[I].Kind in [mwikPhysicalTerrain,mwikPhysicalFoliage]) then begin
      AFailedPass:=Ord(FCommitValidation.Issues[I].Layer); AFailedCell:=FCommitValidation.Issues[I].CellIndex; Break;
    end;
end;

procedure TMappedWorldSession.CollectSolveReport(const AReport: TGraphSolveReport);
var I: Integer;
begin
  if Length(FReport.Passes)<Length(AReport.Passes) then SetLength(FReport.Passes,Length(AReport.Passes));
  for I:=0 to High(AReport.Passes) do begin
    Inc(FReport.Decisions,AReport.Passes[I].Decisions); Inc(FReport.Propagations,AReport.Passes[I].Propagations);
    Inc(FReport.Contradictions,AReport.Passes[I].Contradictions); Inc(FReport.Backtracks,AReport.Passes[I].Backtracks);
    Inc(FReport.ExcludedAssignments,AReport.Passes[I].ExcludedAssignments);
    Inc(FReport.Passes[I].Decisions,AReport.Passes[I].Decisions);
    Inc(FReport.Passes[I].Propagations,AReport.Passes[I].Propagations);
    Inc(FReport.Passes[I].Contradictions,AReport.Passes[I].Contradictions);
    Inc(FReport.Passes[I].Backtracks,AReport.Passes[I].Backtracks);
    Inc(FReport.Passes[I].ExcludedAssignments,AReport.Passes[I].ExcludedAssignments);
    FReport.Passes[I].Executed:=FReport.Passes[I].Executed or AReport.Passes[I].Executed;
    FReport.Passes[I].ExecutionOrdinal:=AReport.Passes[I].ExecutionOrdinal;
    FReport.Passes[I].Disposition:=AReport.Passes[I].Disposition;
  end;
end;

procedure TMappedWorldSession.SetFinalReport(const AReport: TGraphSolveReport);
begin
  FReport.SolveReport:=AReport; FReport.FailedPass:=AReport.FailedPassIndex;
  FReport.FailedCell:=AReport.Contradiction.EntryIndex;
  FReport.DependencyPass:=AReport.Contradiction.DependencyPassIndex;
  FReport.FailureKind:=AReport.Contradiction.Kind; FReport.TraceHash:=AReport.TraceHash;
  CollectSolveReport(AReport);
end;

function TMappedWorldSession.Run(const AAction: TMappedWorldAction;
  const AOptions: TMappedWorldSearchOptions): Boolean;
var I,Root: Integer; L: TMappedWorldLayer; Solve: TGraphSolveReport;
  Search: TGraphNegotiationReport; Selective: TGraphSelectiveNegotiationReport;
  Options: TGraphSolveOptions; Negotiation: TGraphNegotiationOptions;
  Committed: Boolean;
begin
  RequireIdle; RequireMappedWorldInteger(Ord(AAction),0,3,'action');
  ValidateMappedWorldSearchOptions(AOptions); PrepareEdit;
  Result:=False; Committed:=False; FHasCurrent:=False; FCandidate:=Default(TMappedWorldResult);
  FReport:=Default(TMappedWorldReport); FReport.Action:=AAction; FReport.Options:=AOptions;
  FReport.Seed:=FConfig.Seed; FReport.Revision:=FRevision;
  FReport.FailedPass:=-1; FReport.FailedCell:=-1; FReport.DependencyPass:=-1;
  Root:=0; if AAction=mwaHousingOnly then Root:=2 else if AAction=mwaFoliageAndHousing then Root:=1;
  SetLength(FReport.RequestedRootIndices,1); FReport.RequestedRootIndices[0]:=Root;
  SetLength(FReport.ActivePassIndices,3-Root);
  for I:=Root to 2 do FReport.ActivePassIndices[I-Root]:=I;
  if (AAction<>mwaGenerate) and (not FHasBaseline) then begin
    FStatus:=mwstInvalidConfiguration; FReport.Status:=FStatus;
    FReport.Detail:='Generate a baseline before selective repair.'; Exit;
  end;
  for L:=Low(TMappedWorldLayer) to High(TMappedWorldLayer) do
    if (L in FDirtyRoots) and (Ord(L)<Root) then begin
      FStatus:=mwstScopeMismatch; FReport.Status:=FStatus;
      FReport.Detail:='Repair scope excludes edited '+MappedWorldLayerName(L)+'; choose an explicitly broader scope.'; Exit;
    end;
  FStatus:=mwstSolving; FReport.Status:=FStatus; FAttemptRevision:=FRevision+1;
  FCommitValidation:=Default(TMappedWorldValidation);
  Options:=DefaultGraphSolveOptions; Options.MaxBacktracks:=AOptions.MaxBacktracks;
  Options.CaptureTrace:=AOptions.CaptureTrace;
  Negotiation:=DefaultGraphNegotiationOptions; Negotiation.SolveOptions:=Options;
  Negotiation.MaxPassBacktracks:=AOptions.MaxPassBacktracks;
  try
    if AOptions.Negotiated then begin
      if AAction=mwaGenerate then Result:=FGraph.TrySolveNegotiated(Negotiation,Search)
      else begin
        Result:=FGraph.TryRegenerateNegotiatedFrom(MappedWorldLayerName(TMappedWorldLayer(Root)),Negotiation,Selective);
        Search:=Selective.Search; FReport.RequestedRootIndices:=Selective.RequestedRootIndices;
        FReport.ActivePassIndices:=Selective.ActivePassIndices;
        FReport.ScopeAlgorithmVersion:=Selective.ScopeAlgorithmVersion;
      end;
      Committed:=Result;
      FReport.NegotiationReport:=Search; FReport.Rounds:=Length(Search.Attempts)+1;
      FReport.PassBacktracks:=Search.PassBacktracks;
      if AAction=mwaGenerate then FReport.TranscriptHash:=Search.TranscriptHash
      else FReport.TranscriptHash:=Selective.TranscriptHash;
      for I:=0 to High(Search.Attempts) do CollectSolveReport(Search.Attempts[I].SolveReport);
      Solve:=Search.FinalReport;
      case Search.Status of
        gnsSolved:FStatus:=mwstSolved; gnsContradiction:FStatus:=mwstContradiction;
        gnsSolverBacktrackLimit:FStatus:=mwstLocalLimit; gnsPassBacktrackLimit:FStatus:=mwstPassLimit;
      end;
    end else begin
      if AAction=mwaGenerate then Result:=FGraph.TrySolve(Options,Solve)
      else Result:=FGraph.TryRegenerateFrom(MappedWorldLayerName(TMappedWorldLayer(Root)),Options,Solve);
      Committed:=Result;
      FReport.Rounds:=1;
      case Solve.Status of gssSolved:FStatus:=mwstSolved;
        gssContradiction:FStatus:=mwstContradiction; gssBacktrackLimit:FStatus:=mwstLocalLimit; end;
    end;
    SetFinalReport(Solve); FReport.Validation:=FCommitValidation;
    if Result then begin
      if not FCandidate.ModelValid then raise EMappedWorld.Create('solver returned without an independently validated committed capture');
      FCandidate.TraceSignature:=FReport.TraceHash; FCandidate.TranscriptSignature:=FReport.TranscriptHash;
      FCandidate.Signature:=CalculateMappedWorldSignature(FCandidate);
      FBaseline:=FCandidate; FRevision:=FAttemptRevision; FReport.Revision:=FRevision;
      FHasBaseline:=True; FHasCurrent:=True; FDirtyRoots:=[];
      if FCandidate.PhysicalSafe then FReport.Detail:='Selected model satisfied; every generated house has a physically clear full footprint.'
      else FReport.Detail:='Selected weak model satisfied, but a house intersects a physical blocker. Only explicitly marked diagnostic SVG is available.';
    end else begin
      FReport.Detail:='Current output is unavailable. The last committed baseline is retained only for inspection and an authorized repair.';
      if FStatus in [mwstLocalLimit,mwstPassLimit] then
        FReport.Detail:=FReport.Detail+' Search budget exhausted; this is not proof of infeasibility.';
    end;
  except on E: Exception do begin
    Result:=False; FHasCurrent:=False; FStatus:=mwstUnexpectedError;
    FReport.Detail:='Operation failed: '+E.Message;
    if Committed then begin
      //A resource/error after the graph's successful commit cannot honestly
      //advertise the older capture as that graph's repair baseline. Require
      //a new session instead of silently repairing a different composition.
      FHasBaseline:=False; FBaseline:=Default(TMappedWorldResult);
      FReport.Detail:=FReport.Detail+' Publication failed after solving; create a new session before repair.';
    end;
  end; end;
  FReport.Status:=FStatus; FCandidate:=Default(TMappedWorldResult);
end;

function TMappedWorldSession.CopyCurrent(out AResult: TMappedWorldResult): Boolean;
begin
  AResult:=Default(TMappedWorldResult); Result:=FHasCurrent;
  if Result then AResult:=CopyMappedWorldResult(FBaseline);
end;

function TMappedWorldSession.CopyInspection(const AX, AY: Integer; out AResult: TMappedWorldInspection): Boolean;
begin
  MappedWorldCellIndex(mwlHousing,AX,AY); AResult:=Default(TMappedWorldInspection);
  Result:=FHasBaseline;
  if Result then AResult:=InspectMappedWorldSite(FBaseline,AX,AY,FHasCurrent);
end;

function TMappedWorldSession.CopyReport: TMappedWorldReport;
begin
  Result:=FReport;
  Result.RequestedRootIndices:=Copy(FReport.RequestedRootIndices,0,Length(FReport.RequestedRootIndices));
  Result.ActivePassIndices:=Copy(FReport.ActivePassIndices,0,Length(FReport.ActivePassIndices));
  Result.Passes:=Copy(FReport.Passes,0,Length(FReport.Passes));
  Result.Validation.Issues:=Copy(FReport.Validation.Issues,0,Length(FReport.Validation.Issues));
  Result.SolveReport:=CopySolve(FReport.SolveReport); Result.NegotiationReport:=CopyNegotiation(FReport.NegotiationReport);
end;

function TMappedWorldSession.RunReportText: String;
begin Result:=MappedWorldReportText(FReport); end;

function TMappedWorldSession.TryCurrentSvg(out AText: String): Boolean;
begin Result:=TryCurrentSvg(0,0,AText); end;

function TMappedWorldSession.TryCurrentSvg(const AX, AY: Integer; out AText: String): Boolean;
var I: TMappedWorldInspection;
begin
  MappedWorldCellIndex(mwlHousing,AX,AY); AText:='';
  Result:=FHasCurrent and FBaseline.ModelValid and FBaseline.PhysicalSafe;
  if not Result then Exit;
  I:=InspectMappedWorldSite(FBaseline,AX,AY,True);
  AText:=RenderMappedWorldSvg(FBaseline,I,DefaultMappedWorldSvgOptions);
end;

function TMappedWorldSession.TryDiagnosticSvg(out AText: String): Boolean;
begin Result:=TryDiagnosticSvg(0,0,AText); end;

function TMappedWorldSession.TryDiagnosticSvg(const AX, AY: Integer; out AText: String): Boolean;
var I: TMappedWorldInspection; Options: TMappedWorldSvgOptions;
begin
  MappedWorldCellIndex(mwlHousing,AX,AY); AText:=''; Result:=FHasBaseline;
  if not Result then Exit;
  I:=InspectMappedWorldSite(FBaseline,AX,AY,FHasCurrent); Options:=DefaultMappedWorldSvgOptions;
  Options.Diagnostic:=True; AText:=RenderMappedWorldSvg(FBaseline,I,Options);
end;

function MappedWorldSelfTest: Integer;
var Session: TMappedWorldSession; Config: TMappedWorldConfig; Options: TMappedWorldSearchOptions;
  Capture: TMappedWorldResult; Inspection: TMappedWorldInspection; Text: String; Checks: Integer;
  procedure Require(const OK: Boolean; const MessageText: String);
  begin Inc(Checks); if not OK then raise EMappedWorld.Create('Mapped World self-test: '+MessageText); end;
begin
  Checks:=0; Config:=DefaultMappedWorldConfig; Options:=DefaultMappedWorldSearchOptions;
  Session:=TMappedWorldSession.Create(Config);
  try
    Require(not Session.HasBaseline and not Session.HasCurrent,'new session has no output');
    Require(Session.Run(mwaGenerate,Options),'baseline generation');
    Require(Session.CopyCurrent(Capture) and Capture.ModelValid,'independent baseline capture');
    Require(Capture.Layers[mwlFoliage].Cells[7*32+7].Generated
      and (Capture.Layers[mwlFoliage].Cells[7*32+7].Value='tree'),'seed three generates the interior tree');
    Require(Session.CopyInspection(0,0,Inspection) and (Length(Inspection.PhysicalBlockers)=1),'interior blocker inspection');
    Session.SetDemand(0,0,mwdRequired);
    Require(not Session.TryCurrentSvg(Text) and (Text=''),'demand edit removes current export');
    Options.Negotiated:=False;
    Require(not Session.Run(mwaHousingOnly,Options),'housing-only fails');
    Require(Session.HasBaseline and not Session.HasCurrent,'failure preserves hidden baseline only');
    Options.Negotiated:=True;
    Require(not Session.Run(mwaHousingOnly,Options),'negotiated leaf cannot reopen foliage');
    Require(Session.Run(mwaFoliageAndHousing,Options),'authorized upstream repair');
    Require(Session.CopyCurrent(Capture) and Capture.PhysicalSafe
      and (Capture.Layers[mwlHousing].Cells[0].Value='house'),'required house is physically safe');
    Require(Session.TryCurrentSvg(Text) and (Pos('<svg ',Text)>0),'safe shared SVG export');
    Config.Sampling:=mwsPointStudy; Session.Reset(Config);
    Require(Session.Run(mwaGenerate,Options),'point-study baseline'); Session.SetDemand(0,0,mwdRequired);
    Require(Session.Run(mwaHousingOnly,Options),'point study accepts the clear corner');
    Require(Session.CopyCurrent(Capture) and Capture.ModelValid and not Capture.PhysicalSafe,'weak model is not physical safety');
    Require(not Session.TryCurrentSvg(Text) and Session.TryDiagnosticSvg(Text)
      and (Pos('UNSAFE STUDY',Text)>0),'unsafe study export is explicitly diagnostic');
  finally Session.Free; end;
  Result:=Checks;
end;
end.
