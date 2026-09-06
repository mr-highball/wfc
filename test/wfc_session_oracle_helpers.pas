{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Independent integration oracle; no production capture/clone helpers used. }
unit wfc_session_oracle_helpers;
{$mode delphi}{$H+}
interface
uses SysUtils, wfc, wfc_model, wfc_lattice, wfc_pipeline_model, wfc_pipeline_layout,
  wfc_pipeline_run, wfc_pipeline_compile, wfc_pipeline_session;
var OracleChecks: Integer;
procedure Check(const OK: Boolean; const Detail: String);
procedure EqualIndices(const A,B: TGraphPassIndices; const Detail: String);
procedure EqualSolve(const A,B: TGraphSolveReport);
procedure EqualNegotiation(const A,B: TGraphNegotiationReport);
procedure EqualSelective(const A,B: TGraphSelectiveNegotiationReport);
procedure EqualValidation(const A,B: TWfcPipelineCommitValidation);
procedure EqualState(const State: TWfcPipelineSessionPublicState;
  const Recipe: TWfcPipelineModel; const Run: TWfcPipelineRun; const Graph: TGraph);
procedure EqualInvocation(const Value: TWfcPipelineSessionInvocation;
  const Run: TWfcPipelineRun);
implementation
procedure Check(const OK: Boolean; const Detail: String);
begin
  Inc(OracleChecks);
  if not OK then raise Exception.Create('session oracle: '+Detail);
end;
procedure EqualIndices(const A,B: TGraphPassIndices; const Detail: String);
var I: Integer;
begin
  Check(Length(A)=Length(B),Detail+' count');
  for I:=0 to High(A) do Check(A[I]=B[I],Detail+' index '+IntToStr(I));
end;
procedure EqualContradiction(const A,B: TGraphContradiction);
begin
  Check(A.Kind=B.Kind,'contradiction kind');
  Check(A.PassIndex=B.PassIndex,'contradiction pass');
  Check(A.EntryIndex=B.EntryIndex,'contradiction entry');
  Check(A.NeighborIndex=B.NeighborIndex,'contradiction neighbor');
  Check(A.HasDirection=B.HasDirection,'contradiction has direction');
  Check(A.Direction=B.Direction,'contradiction direction');
  Check(A.DependencyPassIndex=B.DependencyPassIndex,'contradiction dependency');
  Check(A.ConstraintIndex=B.ConstraintIndex,'complete contradiction constraint ordinal');
end;
procedure EqualEvent(const A,B: TGraphTraceEvent);
begin
  Check(A.EventId=B.EventId,'event ID');
  Check(A.CauseEventId=B.CauseEventId,'event cause ID');
  Check(A.Kind=B.Kind,'event kind');
  Check(A.CauseKind=B.CauseKind,'event cause');
  Check(A.PassIndex=B.PassIndex,'event pass');
  Check(A.EntryIndex=B.EntryIndex,'event entry');
  Check(A.ValueIndex=B.ValueIndex,'event value index');
  Check(A.Value=B.Value,'event inspection text');
  Check(A.NeighborIndex=B.NeighborIndex,'event neighbor');
  Check(A.HasDirection=B.HasDirection,'event has direction');
  Check(A.Direction=B.Direction,'event direction');
  Check(A.DependencyPassIndex=B.DependencyPassIndex,'event dependency');
  Check(A.DecisionDepth=B.DecisionDepth,'event decision depth');
  Check(A.DomainCountBefore=B.DomainCountBefore,'event domain before');
  Check(A.DomainCountAfter=B.DomainCountAfter,'event domain after');
  Check(A.ConstraintIndex=B.ConstraintIndex,'event constraint ordinal');
end;
procedure EqualSolve(const A,B: TGraphSolveReport);
var I: Integer; P,Q: TGraphPassSolveReport;
begin
  Check(A.Status=B.Status,'solve status');
  Check(A.Seed=B.Seed,'effective seed');
  Check(A.RandomAlgorithmVersion=B.RandomAlgorithmVersion,'random pin');
  Check(A.SolverAlgorithmVersion=B.SolverAlgorithmVersion,'solver pin');
  Check(A.GraphModelVersion=B.GraphModelVersion,'model pin');
  Check(A.PipelineAlgorithmVersion=B.PipelineAlgorithmVersion,'pipeline pin');
  Check(A.FailedPassIndex=B.FailedPassIndex,'failed pass');
  EqualContradiction(A.Contradiction,B.Contradiction);
  Check(Length(A.Passes)=Length(B.Passes),'report pass count');
  for I:=0 to High(A.Passes) do
  begin
    P:=A.Passes[I]; Q:=B.Passes[I];
    Check(P.Decisions=Q.Decisions,'pass decisions');
    Check(P.Propagations=Q.Propagations,'pass propagations');
    Check(P.Contradictions=Q.Contradictions,'pass contradictions');
    Check(P.Backtracks=Q.Backtracks,'pass backtracks');
    Check(P.ExcludedAssignments=Q.ExcludedAssignments,'pass exclusions');
    Check(P.Executed=Q.Executed,'actual executed flag');
    Check(P.ExecutionOrdinal=Q.ExecutionOrdinal,'actual ordinal');
    Check(P.Disposition=Q.Disposition,'actual disposition');
    Check(P.TraceStart=Q.TraceStart,'trace start');
    Check(P.TraceCount=Q.TraceCount,'trace count');
  end;
  EqualIndices(A.ExecutionOrder,B.ExecutionOrder,'actual execution order');
  Check(A.TraceCaptured=B.TraceCaptured,'capture choice');
  Check(A.TraceHash=B.TraceHash,'trace hash');
  Check(Length(A.Trace)=Length(B.Trace),'complete trace length');
  for I:=0 to High(A.Trace) do EqualEvent(A.Trace[I],B.Trace[I]);
  Check(A.TraceDelivery.Version=B.TraceDelivery.Version,'delivery version');
  Check(A.TraceDelivery.Status=B.TraceDelivery.Status,'delivery status');
  Check(A.TraceDelivery.ProducedEventCount=B.TraceDelivery.ProducedEventCount,'produced count');
  Check(A.TraceDelivery.DeliveredEventCount=B.TraceDelivery.DeliveredEventCount,'delivered count');
  Check(A.TraceDelivery.TraceHash=B.TraceDelivery.TraceHash,'delivery hash');
  Check(A.TraceDelivery.FailurePhase=B.TraceDelivery.FailurePhase,'delivery failure phase');
  Check(A.TraceDelivery.FailureEventId=B.TraceDelivery.FailureEventId,'delivery failure ID');
  Check(A.TraceDelivery.FailureMessage=B.TraceDelivery.FailureMessage,'delivery failure text');
end;
procedure EqualNegotiation(const A,B: TGraphNegotiationReport);
var I,J: Integer;
begin
  Check(A.Status=B.Status,'negotiation status');
  Check(A.Seed=B.Seed,'negotiation seed');
  Check(A.NegotiationAlgorithmVersion=B.NegotiationAlgorithmVersion,'negotiation pin');
  Check(A.PassBacktracks=B.PassBacktracks,'pass backtracks');
  Check(A.TranscriptHash=B.TranscriptHash,'transcript hash');
  Check(Length(A.Attempts)=Length(B.Attempts),'all rejected attempts');
  for I:=0 to High(A.Attempts) do
  begin
    EqualSolve(A.Attempts[I].SolveReport,B.Attempts[I].SolveReport);
    Check(A.Attempts[I].BacktrackedPassIndex=B.Attempts[I].BacktrackedPassIndex,'excluded pass');
    Check(A.Attempts[I].BacktrackedExecutionOrdinal=B.Attempts[I].BacktrackedExecutionOrdinal,'excluded ordinal');
    Check(Length(A.Attempts[I].ExcludedAssignment)=Length(B.Attempts[I].ExcludedAssignment),'complete excluded assignment length');
    for J:=0 to High(A.Attempts[I].ExcludedAssignment) do
      Check(A.Attempts[I].ExcludedAssignment[J]=B.Attempts[I].ExcludedAssignment[J],'excluded assignment value');
  end;
  EqualSolve(A.FinalReport,B.FinalReport);
end;
procedure EqualSelective(const A,B: TGraphSelectiveNegotiationReport);
begin
  Check(A.ScopeAlgorithmVersion=B.ScopeAlgorithmVersion,'selective scope pin');
  EqualIndices(A.RequestedRootIndices,B.RequestedRootIndices,'canonical roots');
  EqualIndices(A.ActivePassIndices,B.ActivePassIndices,'complete authorized closure');
  EqualNegotiation(A.Search,B.Search);
  Check(A.TranscriptHash=B.TranscriptHash,'selective transcript hash');
end;
procedure EqualValidation(const A,B: TWfcPipelineCommitValidation);
begin
  Check(A.Kind=B.Kind,'compiler validation kind');
  Check(A.PassIndex=B.PassIndex,'compiler validation pass');
  Check(A.BridgeIndex=B.BridgeIndex,'compiler validation bridge');
  Check(A.RequirementIndex=B.RequirementIndex,'compiler validation requirement');
  Check(A.ValueQuotaIndex=B.ValueQuotaIndex,'compiler validation quota');
  Check(A.ConnectivityIndex=B.ConnectivityIndex,'compiler validation connectivity');
  Check(A.EntryIndex=B.EntryIndex,'compiler validation entry');
end;
function AsToken(const V: TGraphValue): TWfcModelToken;
begin
  {$IFDEF PAS2JS}Result:=TWfcModelToken(V);
  {$ELSE}Result:=UTF8Encode(UnicodeString(V));{$ENDIF}
end;
procedure EqualState(const State: TWfcPipelineSessionPublicState;
  const Recipe: TWfcPipelineModel; const Run: TWfcPipelineRun; const Graph: TGraph);
var Saved,I,J,X,Y,Z,N: Integer; L: TWfcPipelineSessionLayer;
  G: TGraph; E: TGraphEntry; Layout: TWfcLatticeLayout;
begin
  Check(State<>nil,'public state owner exists');
  Saved:=Graph.CurrentPassIndex; Graph.SwitchToPass(0); J:=0;
  try
    for I:=0 to Recipe.PassCount-1 do
      if Recipe.PassAt(I).Visibility=wppvPublic then
      begin
        L:=State.LayerAt(J); Inc(J); G:=Graph.PassGraph[I];
        Check(L.PassIndex=I,'public pass identity');
        Check(L.LabelName=Recipe.PassAt(I).LabelName,'public label');
        Check(L.Rank=Run.PassTopologyAt(I).Rank,'public rank');
        Layout:=Run.PassLayoutAt(I);
        Check(SameWfcLatticeLayout(L.Layout,Layout),'public complete layout');
        Check(Length(L.Cells)=Run.PassCellCount(I),'public actual cell count');
        N:=0;
        for Z:=0 to Layout.Cells.Z-1 do
          for Y:=0 to Layout.Cells.Y-1 do
            for X:=0 to Layout.Cells.X-1 do
            begin
              E:=G.Entry[X,Y,Z];
              Check(L.Cells[N].Empty=E.Empty,'public actual empty');
              Check(L.Cells[N].Generated=E.Generated,'public actual generated ownership');
              if E.Empty then Check(L.Cells[N].Token='','empty has no invented token')
              else Check(L.Cells[N].Token=AsToken(E.Value),'public actual token');
              Inc(N);
            end;
      end;
    Check(J=State.LayerCount,'only all public layers captured');
  finally Graph.SwitchToPass(Saved); end;
end;
procedure EqualInvocation(const Value: TWfcPipelineSessionInvocation;
  const Run: TWfcPipelineRun);
var I,J: Integer; L: TWfcPipelineCellLock; D: TWfcPipelineCellDomain;
  T: TWfcPipelinePassTopology; E: TWfcLatticeVector;
begin
  Check(Value.FormatVersion=Run.FormatVersion,'invocation format');
  Check(Value.RecipeSignature=Run.RecipeSignature,'invocation recipe claim');
  Check(Value.Seed=Run.Seed,'invocation seed');
  Check(Value.Strategy=Run.Strategy,'invocation strategy');
  Check(Value.MaxBacktracks=Run.MaxBacktracks,'invocation local budget');
  Check(Value.MaxPassBacktracks=Run.MaxPassBacktracks,'invocation pass budget');
  Check(Value.CaptureTrace=Run.CaptureTrace,'invocation capture');
  Check(Length(Value.Topologies)=Run.PassCount,'invocation topologies');
  Check(Length(Value.Extents)=Run.PassCount,'invocation extents');
  for I:=0 to Run.PassCount-1 do
  begin
    T:=Run.PassTopologyAt(I); E:=Run.PassLayoutAt(I).Cells;
    Check(Value.Topologies[I].Rank=T.Rank,'topology rank');
    Check(Value.Topologies[I].Wrap=T.Wrap,'topology wrap');
    Check((Value.Topologies[I].Origin.X=T.Origin.X) and
      (Value.Topologies[I].Origin.Y=T.Origin.Y) and (Value.Topologies[I].Origin.Z=T.Origin.Z),'topology origin');
    Check((Value.Topologies[I].Pitch.X=T.Pitch.X) and
      (Value.Topologies[I].Pitch.Y=T.Pitch.Y) and (Value.Topologies[I].Pitch.Z=T.Pitch.Z),'topology pitch');
    Check((Value.Extents[I].X=E.X) and (Value.Extents[I].Y=E.Y) and (Value.Extents[I].Z=E.Z),'exact pass extent');
  end;
  Check(Length(Value.Locks)=Run.LockCount,'invocation lock count');
  for I:=0 to Run.LockCount-1 do
  begin
    L:=Run.LockAt(I);
    Check((Value.Locks[I].PassIndex=L.PassIndex) and (Value.Locks[I].X=L.X) and
      (Value.Locks[I].Y=L.Y) and (Value.Locks[I].Z=L.Z) and (Value.Locks[I].Token=L.Token),'exact authored lock');
  end;
  Check(Length(Value.Domains)=Run.DomainCount,'invocation domain count');
  for I:=0 to Run.DomainCount-1 do
  begin
    D:=Run.DomainAt(I);
    Check((Value.Domains[I].PassIndex=D.PassIndex) and (Value.Domains[I].X=D.X) and
      (Value.Domains[I].Y=D.Y) and (Value.Domains[I].Z=D.Z),'exact authored domain cell');
    Check(Length(Value.Domains[I].AllowedTokens)=Length(D.AllowedTokens),'complete authored domain');
    for J:=0 to High(D.AllowedTokens) do
      Check(Value.Domains[I].AllowedTokens[J]=D.AllowedTokens[J],'authored domain token');
  end;
end;
end.
