{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Complete canonical outcome evidence for workspace journals.
  This writer accepts actual typed session owners, not imported report records.
  It is not an imported-report decoder or a replay implementation. }
unit wfc_pipeline_session_evidence;
{$mode delphi}{$H+}
interface
uses SysUtils, wfc_pipeline_session;
type
  EWfcPipelineSessionEvidence = class(Exception);
  TWfcPipelineSessionEvidenceLimits = record
    Version: Integer;
    MaxTextBytes: Integer;
    MaxLines: Integer;
  end;
function EncodeWfcPipelineSessionOutcomeEvidence(const Outcome: TWfcPipelineSessionOutcome;
  const Limits: TWfcPipelineSessionEvidenceLimits): String;
function EncodeWfcPipelineSessionEditEvidence(const Edit: TWfcPipelineSessionEditOutcome;
  const Limits: TWfcPipelineSessionEvidenceLimits): String;
implementation
uses wfc,wfc_model,wfc_lattice,wfc_pipeline_compile,wfc_pipeline_prepare,
  wfc_pipeline_run,wfc_pipeline_layout,wfc_text_codec;
const Artifact = 'private WFC session evidence';
type
  TEvidenceWriter = class
  private
    FLimits: TWfcPipelineSessionEvidenceLimits;
    FLines: TWfcTextLines;
    FCount,FBytes: Integer;
    FWriting: Boolean;
  public
    constructor Create(const Limits: TWfcPipelineSessionEvidenceLimits);
    procedure Line(const Value: String);
    procedure IntegerField(const Key: String; const Value: Integer);
    procedure CardinalField(const Key: String; const Value: Cardinal);
    procedure BooleanField(const Key: String; const Value: Boolean);
    procedure Indices(const Key: String; const Values: TGraphPassIndices);
    procedure BeginWriting;
    function Finish: String;
  end;
procedure Fail(const Detail: String);
begin raise EWfcPipelineSessionEvidence.Create(Detail); end;
procedure RequireLimits(const Limits: TWfcPipelineSessionEvidenceLimits);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    Valid=Limits!==null && typeof Limits==='object' && !Array.isArray(Limits);
    if (Valid) {
      const names=['Version','MaxTextBytes','MaxLines'];
      for (const name of names) {
        let p=Limits,d;
        while (p!==null && !(d=Object.getOwnPropertyDescriptor(p,name))) p=Object.getPrototypeOf(p);
        if (!d || !('value' in d) || typeof d.value!=='number' ||
          !Number.isInteger(d.value) || d.value<1 || d.value>2147483647) { Valid=false; break; }
      }
    }
  end;
  if not Valid then Fail('evidence limits require complete passive positive Integer fields');
  {$ENDIF}
  if (Limits.Version<>1) or (Limits.MaxTextBytes<1) or (Limits.MaxLines<1) then
    Fail('unsupported evidence limits or nonpositive envelope');
end;
constructor TEvidenceWriter.Create(const Limits: TWfcPipelineSessionEvidenceLimits);
begin inherited Create; RequireLimits(Limits); FLimits:=Limits; end;
procedure TEvidenceWriter.Line(const Value: String);
begin
  if FCount=FLimits.MaxLines then Fail('evidence line budget exceeded');
  if Length(Value)>=FLimits.MaxTextBytes-FBytes then Fail('evidence text budget exceeded');
  if FWriting then
  begin
    if FCount>=Length(FLines) then Fail('evidence preflight line count changed');
    FLines[FCount]:=Value;
  end;
  Inc(FCount); Inc(FBytes,Length(Value)+1);
end;
procedure TEvidenceWriter.IntegerField(const Key: String; const Value: Integer);
begin Line(Key+'='+IntToStr(Value)); end;
procedure TEvidenceWriter.CardinalField(const Key: String; const Value: Cardinal);
begin Line(Key+'='+UIntToStr(Value)); end;
procedure TEvidenceWriter.BooleanField(const Key: String; const Value: Boolean);
begin IntegerField(Key,Ord(Value)); end;
procedure TEvidenceWriter.Indices(const Key: String; const Values: TGraphPassIndices);
var I: Integer;
begin
  IntegerField(Key+'.count',Length(Values));
  for I:=0 to High(Values) do IntegerField(Key+'.'+IntToStr(I),Values[I]);
end;
procedure TEvidenceWriter.BeginWriting;
begin
  if FWriting then Fail('evidence writer already entered output phase');
  SetLength(FLines,FCount); FCount:=0; FBytes:=0; FWriting:=True;
end;
function TEvidenceWriter.Finish: String;
begin
  if not FWriting or (FCount<>Length(FLines)) then Fail('evidence output count differs from preflight');
  Result:=WfcTextJoinCanonicalLines(FLines,Artifact);
  if Length(Result)<>FBytes then Fail('evidence joined byte count differs from preflight');
end;
function Token(const Value: TWfcModelToken): String;
begin Result:=WfcTextEncodeToken(Value,Artifact); end;
function GraphToken(const Value: TGraphValue): String;
begin
  {$IFDEF PAS2JS}Result:=Token(TWfcModelToken(Value));
  {$ELSE}Result:=Token(UTF8Encode(UnicodeString(Value)));{$ENDIF}
end;
function HostToken(const Value: String): String;
begin
  {$IFDEF PAS2JS}Result:=Token(TWfcModelToken(Value));
  {$ELSE}Result:=Token(UTF8Encode(UnicodeString(Value)));{$ENDIF}
end;
function Integers(const Values: array of Integer): String;
var I: Integer;
begin
  Result:=''; for I:=0 to High(Values) do
  begin if I<>0 then Result:=Result+','; Result:=Result+IntToStr(Values[I]); end;
end;
function Vector(const V: TWfcLatticeVector): String;
begin Result:=Integers([V.X,V.Y,V.Z]); end;
procedure Header(const W: TEvidenceWriter; const Kind: String);
begin
  W.Line('wfc-session-evidence=1'); W.Line('kind='+Kind);
  W.IntegerField('session-version',WFC_PIPELINE_SESSION_VERSION);
  W.IntegerField('trace-version',WFC_TRACE_VERSION);
  W.IntegerField('trace-hash-version',WFC_TRACE_HASH_VERSION);
  W.IntegerField('negotiation-hash-version',WFC_PASS_NEGOTIATION_HASH_VERSION);
  W.IntegerField('selective-hash-version',WFC_SELECTIVE_NEGOTIATION_HASH_VERSION);
end;
procedure WriteInvocation(const W: TEvidenceWriter; const V: TWfcPipelineSessionInvocation);
var I,J: Integer; Prefix: String; L: TWfcPipelineCellLock; D: TWfcPipelineCellDomain;
begin
  W.IntegerField('invocation.format',V.FormatVersion); W.CardinalField('invocation.recipe-signature',V.RecipeSignature);
  W.CardinalField('invocation.seed',V.Seed); W.IntegerField('invocation.strategy',Ord(V.Strategy));
  W.IntegerField('invocation.local-budget',V.MaxBacktracks); W.IntegerField('invocation.pass-budget',V.MaxPassBacktracks);
  W.BooleanField('invocation.capture',V.CaptureTrace);
  W.IntegerField('invocation.topology-count',Length(V.Topologies));
  for I:=0 to High(V.Topologies) do
    W.Line('invocation.topology.'+IntToStr(I)+'='+Integers([V.Topologies[I].Rank,Ord(V.Topologies[I].Wrap)])+','+
      Vector(V.Topologies[I].Origin)+','+Vector(V.Topologies[I].Pitch));
  W.IntegerField('invocation.extent-count',Length(V.Extents));
  for I:=0 to High(V.Extents) do W.Line('invocation.extent.'+IntToStr(I)+'='+Vector(V.Extents[I]));
  W.IntegerField('invocation.lock-count',Length(V.Locks));
  for I:=0 to High(V.Locks) do
  begin
    L:=V.Locks[I]; W.Line('invocation.lock.'+IntToStr(I)+'='+Integers([L.PassIndex,L.X,L.Y,L.Z])+','+Token(L.Token));
  end;
  W.IntegerField('invocation.domain-count',Length(V.Domains));
  for I:=0 to High(V.Domains) do
  begin
    Prefix:='invocation.domain.'+IntToStr(I); D:=V.Domains[I];
    W.Line(Prefix+'='+Integers([D.PassIndex,D.X,D.Y,D.Z,Length(D.AllowedTokens)]));
    for J:=0 to High(D.AllowedTokens) do W.Line(Prefix+'.token.'+IntToStr(J)+'='+Token(D.AllowedTokens[J]));
  end;
end;
procedure WriteScope(const W: TEvidenceWriter; const S: TWfcPipelineSessionScope);
begin
  W.IntegerField('scope.version',S.ScopeAlgorithmVersion);
  W.Indices('scope.roots',S.RequestedRootIndices); W.Indices('scope.active',S.ActivePassIndices);
  W.Indices('scope.authored',S.AuthoredPassIndices); W.Indices('scope.required',S.RequiredPassIndices);
  W.Indices('scope.missing',S.MissingPassIndices);
end;
procedure WriteState(const W: TEvidenceWriter; const Layers: TWfcPipelineSessionLayers);
var I,J: Integer; Prefix: String; L: TWfcPipelineSessionLayer;
begin
  W.IntegerField('state.layer-count',Length(Layers));
  for I:=0 to High(Layers) do
  begin
    L:=Layers[I]; Prefix:='state.layer.'+IntToStr(I);
    W.Line(Prefix+'='+Integers([L.PassIndex,L.Rank])+','+Token(L.LabelName));
    W.Line(Prefix+'.layout='+Vector(L.Layout.Cells)+','+Vector(L.Layout.Origin)+','+
      Vector(L.Layout.Pitch)+','+IntToStr(Ord(L.Layout.Wrap)));
    W.IntegerField(Prefix+'.cell-count',Length(L.Cells));
    for J:=0 to High(L.Cells) do
      W.Line(Prefix+'.cell.'+IntToStr(J)+'='+Integers([Ord(L.Cells[J].Empty),Ord(L.Cells[J].Generated)])+','+Token(L.Cells[J].Token));
  end;
end;
procedure WriteSolve(const W: TEvidenceWriter; const Prefix: String; const R: TGraphSolveReport);
var I: Integer; P: TGraphPassSolveReport; E: TGraphTraceEvent; D: TGraphContradiction;
begin
  W.IntegerField(Prefix+'.status',Ord(R.Status)); W.CardinalField(Prefix+'.seed',R.Seed);
  W.Line(Prefix+'.versions='+Integers([R.RandomAlgorithmVersion,R.SolverAlgorithmVersion,R.GraphModelVersion,R.PipelineAlgorithmVersion]));
  W.IntegerField(Prefix+'.failed-pass',R.FailedPassIndex); D:=R.Contradiction;
  W.Line(Prefix+'.contradiction='+Integers([Ord(D.Kind),D.PassIndex,D.EntryIndex,D.NeighborIndex,
    Ord(D.HasDirection),Ord(D.Direction),D.DependencyPassIndex,D.ConstraintIndex]));
  W.IntegerField(Prefix+'.pass-count',Length(R.Passes));
  for I:=0 to High(R.Passes) do
  begin
    P:=R.Passes[I]; W.Line(Prefix+'.pass.'+IntToStr(I)+'='+Integers([P.Decisions,P.Propagations,
      P.Contradictions,P.Backtracks,P.ExcludedAssignments,Ord(P.Executed),P.ExecutionOrdinal,
      Ord(P.Disposition),P.TraceStart,P.TraceCount]));
  end;
  W.Indices(Prefix+'.order',R.ExecutionOrder); W.BooleanField(Prefix+'.trace-captured',R.TraceCaptured);
  W.CardinalField(Prefix+'.trace-hash',R.TraceHash); W.IntegerField(Prefix+'.trace-count',Length(R.Trace));
  for I:=0 to High(R.Trace) do
  begin
    E:=R.Trace[I]; W.Line(Prefix+'.event.'+IntToStr(I)+'='+Integers([E.EventId,E.CauseEventId,
      Ord(E.Kind),Ord(E.CauseKind),E.PassIndex,E.EntryIndex,E.ValueIndex])+','+GraphToken(E.Value)+','+
      Integers([E.NeighborIndex,Ord(E.HasDirection),Ord(E.Direction),E.DependencyPassIndex,
        E.DecisionDepth,E.DomainCountBefore,E.DomainCountAfter,E.ConstraintIndex]));
  end;
  W.Line(Prefix+'.delivery='+Integers([R.TraceDelivery.Version,Ord(R.TraceDelivery.Status),
    R.TraceDelivery.ProducedEventCount,R.TraceDelivery.DeliveredEventCount])+','+UIntToStr(R.TraceDelivery.TraceHash)+','+
    Integers([Ord(R.TraceDelivery.FailurePhase),R.TraceDelivery.FailureEventId])+','+HostToken(R.TraceDelivery.FailureMessage));
end;
procedure WriteNegotiation(const W: TEvidenceWriter; const Prefix: String; const R: TGraphNegotiationReport);
var I,J: Integer; Path: String;
begin
  W.IntegerField(Prefix+'.status',Ord(R.Status)); W.CardinalField(Prefix+'.seed',R.Seed);
  W.IntegerField(Prefix+'.version',R.NegotiationAlgorithmVersion); W.IntegerField(Prefix+'.pass-backtracks',R.PassBacktracks);
  W.CardinalField(Prefix+'.transcript-hash',R.TranscriptHash); W.IntegerField(Prefix+'.attempt-count',Length(R.Attempts));
  for I:=0 to High(R.Attempts) do
  begin
    Path:=Prefix+'.attempt.'+IntToStr(I);
    W.Line(Path+'.backtracked='+Integers([R.Attempts[I].BacktrackedPassIndex,R.Attempts[I].BacktrackedExecutionOrdinal]));
    W.IntegerField(Path+'.assignment-count',Length(R.Attempts[I].ExcludedAssignment));
    for J:=0 to High(R.Attempts[I].ExcludedAssignment) do
      W.IntegerField(Path+'.assignment.'+IntToStr(J),R.Attempts[I].ExcludedAssignment[J]);
    WriteSolve(W,Path+'.solve',R.Attempts[I].SolveReport);
  end;
  WriteSolve(W,Prefix+'.final',R.FinalReport);
end;
function OutcomeName(const Kind: TWfcPipelineSessionOutcomeKind): String;
begin
  case Kind of
    wpsokOrdinaryFull:Result:='ordinary-full'; wpsokNegotiatedFull:Result:='negotiated-full';
    wpsokOrdinarySelective:Result:='ordinary-selective'; wpsokNegotiatedSelective:Result:='negotiated-selective';
  else Fail('unknown captured outcome kind'); end;
end;
function EncodeWfcPipelineSessionOutcomeEvidence(const Outcome: TWfcPipelineSessionOutcome;
  const Limits: TWfcPipelineSessionEvidenceLimits): String;
var W: TEvidenceWriter; V: TWfcPipelineSessionInvocation; Scope: TWfcPipelineSessionScope;
  State: TWfcPipelineSessionPublicState; Layers: TWfcPipelineSessionLayers;
  Pending,Authored: TGraphPassIndices; S: TGraphSolveReport; N: TGraphNegotiationReport;
  Q: TGraphSelectiveNegotiationReport; Validation: TWfcPipelineCommitValidation;
  Kind: TWfcPipelineSessionOutcomeKind; Revision,Phase: Integer; Solved,Current,Baseline: Boolean;
begin
  Result:=''; RequireLimits(Limits);
  if Outcome=nil then Fail('captured outcome is required');
  V:=Outcome.CopyInvocation; Scope:=Outcome.CopyScope; Pending:=Outcome.CopyPendingPassIndices;
  Authored:=Outcome.CopyAuthoredPassIndices; Validation:=Outcome.LastValidation;
  Kind:=Outcome.Kind; Revision:=Outcome.Revision; Solved:=Outcome.Solved;
  Current:=Outcome.HasCurrentOutput; Baseline:=Outcome.HasSuccessfulBaseline;
  State:=Outcome.CopyPublicState; try Layers:=State.CopyLayers; finally State.Free; end;
  S:=Default(TGraphSolveReport); N:=Default(TGraphNegotiationReport); Q:=Default(TGraphSelectiveNegotiationReport);
  case Kind of
    wpsokOrdinaryFull,wpsokOrdinarySelective:S:=Outcome.CopySolveReport;
    wpsokNegotiatedFull:N:=Outcome.CopyNegotiationReport;
    wpsokNegotiatedSelective:Q:=Outcome.CopySelectiveNegotiationReport;
  end;
  W:=TEvidenceWriter.Create(Limits);
  try
    for Phase:=0 to 1 do
    begin
      Header(W,OutcomeName(Kind)); W.IntegerField('revision',Revision); W.BooleanField('solved',Solved);
      W.BooleanField('current',Current); W.BooleanField('baseline',Baseline);
      WriteInvocation(W,V); WriteScope(W,Scope); W.Indices('pending',Pending); W.Indices('authored',Authored);
      WriteState(W,Layers);
      W.Line('validation='+Integers([Ord(Validation.Kind),Validation.PassIndex,Validation.BridgeIndex,
        Validation.RequirementIndex,Validation.ValueQuotaIndex,Validation.ConnectivityIndex,Validation.EntryIndex]));
      case Kind of
        wpsokOrdinaryFull,wpsokOrdinarySelective:WriteSolve(W,'solve',S);
        wpsokNegotiatedFull:WriteNegotiation(W,'search',N);
        wpsokNegotiatedSelective:begin
          W.IntegerField('selection.version',Q.ScopeAlgorithmVersion);
          W.Indices('selection.roots',Q.RequestedRootIndices); W.Indices('selection.active',Q.ActivePassIndices);
          W.CardinalField('selection.transcript-hash',Q.TranscriptHash); WriteNegotiation(W,'selection.search',Q.Search);
        end;
      end;
      if Phase=0 then W.BeginWriting;
    end;
    Result:=W.Finish;
  finally W.Free; end;
end;
function EncodeWfcPipelineSessionEditEvidence(const Edit: TWfcPipelineSessionEditOutcome;
  const Limits: TWfcPipelineSessionEvidenceLimits): String;
var W: TEvidenceWriter; V: TWfcPipelineSessionInvocation; Impact: TWfcPipelineInputImpact;
  State: TWfcPipelineSessionPublicState; Layers: TWfcPipelineSessionLayers;
  Pending,Authored: TGraphPassIndices; Revision,Phase: Integer; Current,Baseline: Boolean;
begin
  Result:=''; RequireLimits(Limits);
  if Edit=nil then Fail('captured edit is required');
  V:=Edit.CopyInvocation; Impact:=Edit.CopyImpact; Pending:=Edit.CopyPendingPassIndices;
  Authored:=Edit.CopyAuthoredPassIndices; Revision:=Edit.Revision;
  Current:=Edit.HasCurrentOutput; Baseline:=Edit.HasSuccessfulBaseline;
  State:=Edit.CopyPublicState; try Layers:=State.CopyLayers; finally State.Free; end;
  W:=TEvidenceWriter.Create(Limits);
  try
    for Phase:=0 to 1 do
    begin
      Header(W,'edit'); W.IntegerField('revision',Revision);
      W.BooleanField('current',Current); W.BooleanField('baseline',Baseline);
      WriteInvocation(W,V); W.Indices('pending',Pending); W.Indices('authored',Authored);
      W.BooleanField('impact.authored-changed',Impact.AuthoredInputsChanged);
      W.BooleanField('impact.graph-changed',Impact.GraphInputsChanged);
      W.Indices('impact.authored',Impact.AuthoredPassIndices); W.Indices('impact.changed',Impact.ChangedPassIndices);
      WriteState(W,Layers); if Phase=0 then W.BeginWriting;
    end;
    Result:=W.Finish;
  finally W.Free; end;
end;
end.
