(*
MIT License

Copyright (c) 2021 mr-highball

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)
program wfc_connectivity_trace_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  Classes, SysUtils, wfc, wfc_trace;

type
  TTopologyHookGraph = class(TGraph)
  public
    HookMode: Integer;
    RewriteRules: Boolean;
  protected
    function DoValidateCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean; override;
  end;

function TTopologyHookGraph.DoValidateCommit(out AFailedPassIndex,
  AFailedEntryIndex: Integer): Boolean;
var
  Rules: TGraphRules;
begin
  Result := inherited DoValidateCommit(AFailedPassIndex, AFailedEntryIndex);
  if RewriteRules then
  begin
    SetLength(Rules, 2);
    Rules[0].Key := gdEast;
    Rules[0].Value := ['none'];
    Rules[0].Info := False;
    Rules[1].Key := gdWest;
    Rules[1].Value := ['none'];
    Rules[1].Info := False;
    PassGraph[0].Rules['a'].Rules := Rules;
    PassGraph[0].Rules['b'].Rules := Rules;
  end;
  if HookMode = 0 then Exit;
  PassGraph[0].Entry[0, 0, 0].Neighbor[gdEast] := nil;
  PassGraph[0].RandomIndex(1000);
  if HookMode = 2 then
  begin
    AFailedPassIndex := CurrentPassIndex;
    AFailedEntryIndex := -1;
    Result := False;
  end;
  if HookMode = 3 then raise Exception.Create('test hook exception');
end;

var
  Checks, Failures: Integer;
  Golden: TGraphTraceSignature;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if ACondition then Exit;
  Inc(Failures);
  WriteLn('FAIL: ', AMessage);
end;

function Position(const AX: Integer): TGraphPosition;
begin
  Result.X := AX; Result.Y := 0; Result.Z := 0;
end;

function Constraint(const ALabel: String;
  const AOpenings: TGraphDirections): TGraphConnectivityConstraint;
var
  Terminals: TGraphPositions;
  Profiles: TGraphConnectivityValues;
begin
  SetLength(Terminals, 1);
  Terminals[0] := Position(2);
  SetLength(Profiles, 2);
  Profiles[0] := MakeGraphConnectivityValue('a', AOpenings);
  Profiles[1] := MakeGraphConnectivityValue('b', AOpenings);
  Result := MakeGraphConnectivityConstraint(ALabel, Position(0),
    Terminals, Profiles);
end;

function Fixture: TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := 55;
  Result.Reshape(3, 1, 1);
  Result.WrapNeighbors := False;
  Result.CurrentPass := 'routes';
  Result.AddValue('none');
  Result.AddValue('a');
  Result.AddValue('b');
  Result.RequireConnectivity(Constraint('route', [gdEast, gdWest]));
end;

function Values(const AGraph: TGraph): String;
var I: Integer;
begin
  Result := '';
  for I := 0 to 2 do Result := Result + AGraph.PassGraph[0].Entry[I, 0, 0].Value;
end;

procedure CheckTrace(const AGraph: TGraph; const AReport: TGraphSolveReport);
var
  Validation: TGraphTraceValidationReport;
  I: Integer;
  Valid: Boolean;
begin
  Check(AReport.TraceHash = CalculateGraphTraceHash(AReport), 'trace hash recomputes');
  Valid := ValidateGraphTrace(AGraph, AReport, Validation);
  Check(Valid,
    'independent trace validator: ' + DescribeGraphTraceValidationIssue(Validation.Issue));
  for I := 0 to High(AReport.Trace) do
    if AReport.Trace[I].CauseKind = gtckConnectivity then
    begin
      Check(AReport.Trace[I].ConstraintIndex >= 0, 'connectivity cause has an ordinal');
      Check((AReport.Trace[I].NeighborIndex = -1)
        and not AReport.Trace[I].HasDirection
        and (AReport.Trace[I].DependencyPassIndex = -1),
        'global cause does not pretend to be an adjacency or provider');
    end
    else Check(AReport.Trace[I].ConstraintIndex = -1, 'ordinary event keeps sentinel');
end;

procedure TestPropagationTrace;
var
  Graph, Plain: TGraph;
  Options: TGraphSolveOptions;
  Report, Again, Damaged: TGraphSolveReport;
  Validation: TGraphTraceValidationReport;
  I, Removal, Decision, RemovedCount: Integer;
  Before: String;
begin
  Graph := Fixture;
  Plain := Fixture;
  try
    //The second equivalent clause has a distinct label and stable ordinal.
    Graph.RequireConnectivity(Constraint('second-route', [gdEast, gdWest]));
    Plain.RequireConnectivity(Constraint('second-route', [gdEast, gdWest]));
    Options := DefaultGraphSolveOptions;
    Options.CaptureTrace := True;
    Check(Graph.TrySolve(Options, Report), 'corridor with alternative road values solves');
    CheckTrace(Graph, Report);
    Golden := Report.TraceHash;
    Removal := -1; Decision := -1; RemovedCount := 0;
    for I := 0 to High(Report.Trace) do
    begin
      if (Report.Trace[I].Kind = gtekCandidateRemoved)
        and (Report.Trace[I].CauseKind = gtckConnectivity) then
      begin
        Removal := I;
        Inc(RemovedCount);
        Check(Report.Trace[I].ConstraintIndex = 0, 'first clause owns its pruning');
        Check(Report.Trace[I].Value = 'none', 'mandatory cells lose nonparticipants');
        Check(Report.Trace[I].DecisionDepth = 0, 'bottleneck pruning precedes decisions');
      end;
      if (Report.Trace[I].Kind = gtekDecision)
        and (Report.Trace[I].CauseKind = gtckConnectivity) then
      begin
        Decision := I;
        Check(Report.Trace[I].ConstraintIndex =
          Report.Trace[Report.Trace[I].CauseEventId].ConstraintIndex,
          'decision inherits the exact connectivity ordinal');
      end;
    end;
    Check(RemovedCount = 3, 'root, articulation, and terminal are forced participants');
    Check(Decision >= 0, 'decision retains a connectivity causal link');
    Check(Removal >= 0, 'a connectivity removal is captured');
    if Removal >= 0 then
    begin
      Check(Pos('connectivity=0', FormatGraphTraceEvent(Report.Trace[Removal])) > 0,
        'text formatter exposes the constraint ordinal');
      Damaged := Report;
      Damaged.Trace := CopyGraphTraceEvents(Report.Trace);
      Damaged.Trace[Removal].ConstraintIndex := -1;
      Check(CalculateGraphTraceHash(Damaged) <> Report.TraceHash,
        'ordinal contributes to connectivity event hash');
      Damaged.TraceHash := CalculateGraphTraceHash(Damaged);
      Check(not ValidateGraphTrace(Graph, Damaged, Validation)
        and (Validation.Issue.Kind = gtvikConstraintIndex),
        'rehashed missing ordinal is rejected');
      Damaged.Trace[Removal].ConstraintIndex := 2;
      Damaged.TraceHash := CalculateGraphTraceHash(Damaged);
      Check(not ValidateGraphTrace(Graph, Damaged, Validation)
        and (Validation.Issue.Kind = gtvikConstraintIndex),
        'out-of-range ordinal is rejected');
    end;
    if Decision >= 0 then
    begin
      Damaged := Report;
      Damaged.Trace := CopyGraphTraceEvents(Report.Trace);
      Damaged.Trace[Decision].ConstraintIndex := 1;
      Damaged.TraceHash := CalculateGraphTraceHash(Damaged);
      Check(not ValidateGraphTrace(Graph, Damaged, Validation)
        and (Validation.Issue.Kind = gtvikCausalLink),
        'a valid but different ordinal cannot replace the decision cause');
    end;
    Damaged := Report;
    Damaged.Trace := CopyGraphTraceEvents(Report.Trace);
    Damaged.Trace[0].ConstraintIndex := 0;
    Check(not ValidateGraphTrace(Graph, Damaged, Validation)
      and (Validation.Issue.Kind = gtvikConstraintIndex),
      'ordinary events cannot carry a connectivity ordinal');
    Before := Values(Graph);
    Check(Graph.TrySolve(Options, Again), 'same-seed repeat solves');
    Check((Values(Graph) = Before) and (Again.TraceHash = Report.TraceHash),
      'same model replays assignment and connectivity trace');
    Options.CaptureTrace := False;
    Check(Plain.TrySolve(Options, Again), 'capture-disabled equivalent solves');
    Check((Values(Plain) = Before) and (Again.TraceHash = 0)
      and (Length(Again.Trace) = 0), 'capture has no generation effect');
    Check(Again.Contradiction.ConstraintIndex = -1, 'success clears terminal ordinal');
  finally
    Plain.Free;
    Graph.Free;
  end;
end;

procedure TestFailureAndTranscript;
var
  Graph: TGraph;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
  Negotiation, Changed: TGraphNegotiationReport;
  NegotiationOptions: TGraphNegotiationOptions;
  Before: String;
  I, Contradictions: Integer;
begin
  Graph := Fixture;
  try
    Options := DefaultGraphSolveOptions;
    Check(Graph.TrySolve(Options, Report), 'failure fixture has a baseline');
    Before := Values(Graph);
    Graph.RequireConnectivity(Constraint('isolated', []));
    Options.CaptureTrace := True;
    Check(not Graph.TrySolve(Options, Report), 'disconnected second clause fails');
    Check(Values(Graph) = Before, 'connectivity failure rolls back the prior result');
    Check((Report.Contradiction.Kind = gckConnectivity)
      and (Report.Contradiction.ConstraintIndex = 1),
      'terminal failure identifies the second AND clause');
    Check(Report.Passes[0].Decisions = 0, 'impossible possible-graph cut fails before choice');
    CheckTrace(Graph, Report);
    Contradictions := 0;
    for I := 0 to High(Report.Trace) do
      if Report.Trace[I].Kind = gtekContradiction then
      begin
        Inc(Contradictions);
        Check((Report.Trace[I].CauseKind = gtckConnectivity)
          and (Report.Trace[I].ConstraintIndex = 1), 'failure event has the same ordinal');
      end;
    Check(Contradictions = 1, 'one structural cut produces one contradiction');
    NegotiationOptions := DefaultGraphNegotiationOptions;
    NegotiationOptions.SolveOptions.CaptureTrace := False;
    Check(not Graph.TrySolveNegotiated(NegotiationOptions, Negotiation),
      'negotiated capture-disabled search reports infeasibility');
    Check(Negotiation.TranscriptHash =
      CalculateGraphNegotiationTranscriptHash(NegotiationOptions, Negotiation),
      'negotiation transcript recomputes');
    Changed := Negotiation;
    Changed.FinalReport.Contradiction.ConstraintIndex := 0;
    Check(CalculateGraphNegotiationTranscriptHash(NegotiationOptions, Changed)
      <> Negotiation.TranscriptHash, 'terminal ordinal is hashed without trace capture');
    Graph.RemoveConnectivity('isolated');
    Check(Graph.TrySolve(Options, Report), 'removing impossible clause allows recovery');
    CheckTrace(Graph, Report);
  finally Graph.Free; end;
end;

procedure TestReusedConstraintEdit;
var
  Graph: TGraph;
  ForeignEntry: TGraphEntry;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
  Before: String;
  Raised: Boolean;
begin
  Graph := Fixture;
  ForeignEntry := TGraphEntry.Create;
  try
    Graph.SwitchToPass('copy');
    Options := DefaultGraphSolveOptions;
    Check(Graph.TrySolve(Options, Report), 'selective fixture has baseline');
    Before := Values(Graph);
    Graph.PassGraph[0].RequireConnectivity(Constraint('isolated', []));
    Raised := False;
    try Graph.TryRegenerateFrom('copy', Options, Report);
    except on E: EInvalidOperation do Raised := Pos('include it in regeneration', E.Message) > 0; end;
    Check(Raised, 'invalid reused connectivity requires including its owning pass');
    Check((Graph.CurrentPass = 'copy') and not Graph.Running
      and (Values(Graph) = Before), 'reused preflight is nonmutating');
    Graph.PassGraph[0].RemoveConnectivity('isolated');
    Options.CaptureTrace := True;
    Check(Graph.TryRegenerateFrom('copy', Options, Report), 'valid reused pass is accepted');
    Check(Report.Passes[0].Disposition = gpdReused, 'upstream routing is explicitly reused');
    CheckTrace(Graph, Report);
    Graph.PassGraph[0].Entry[0, 0, 0].Neighbor[gdNorth] := ForeignEntry;
    Raised := False;
    try Graph.TryRegenerateFrom('copy', Options, Report);
    except on E: EInvalidOperation do Raised := Pos('external neighbor', E.Message) > 0; end;
    Check(Raised, 'reused topology rejects foreign objects even behind a closed port');
    Check((Values(Graph) = Before) and not Graph.Running,
      'malformed reused topology preflight leaves entries untouched');
    Graph.PassGraph[0].Entry[0, 0, 0].Neighbor[gdNorth] := nil;
  finally Graph.Free; ForeignEntry.Free; end;
end;

procedure TestTopologyHookRollback;
var
  Graph: TTopologyHookGraph;
  Twin: TGraph;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
  Before: String;
  Scope, Mode, I: Integer;
  Raised, Solved: Boolean;
begin
  Graph := TTopologyHookGraph.Create;
  Twin := Fixture;
  try
    Graph.Seed := 55;
    Graph.Reshape(3, 1, 1);
    Graph.WrapNeighbors := False;
    Graph.CurrentPass := 'routes';
    Graph.AddValue('none'); Graph.AddValue('a'); Graph.AddValue('b');
    Graph.RequireConnectivity(Constraint('route', [gdEast, gdWest]));
    Graph.SwitchToPass('copy');
    Twin.SwitchToPass('copy');
    Options := DefaultGraphSolveOptions;
    Options.CaptureTrace := True;
    for Scope := 0 to 1 do for Mode := 1 to 3 do
    begin
      Graph.HookMode := 0;
      Check(Graph.TrySolve(Options, Report), 'hook fixture prepares baseline');
      Check(Twin.TrySolve(Options, Report), 'hook twin prepares exact random baseline');
      Before := Values(Graph);
      Graph.HookMode := Mode;
      Raised := False; Solved := False;
      try
        if Scope = 0 then Solved := Graph.TrySolve(Options, Report)
        else Solved := Graph.TryRegenerateFrom('copy', Options, Report);
      except on E: Exception do Raised := True; end;
      Check(not Solved, 'topology-changing hook cannot publish a connected result');
      Check(Raised = (Mode <> 2), 'returning-false and exceptional hook contracts are retained');
      Check(Graph.PassGraph[0].Entry[0, 0, 0][gdEast] =
        Graph.PassGraph[0].Entry[1, 0, 0], 'neighbor topology is restored silently');
      Check((Values(Graph) = Before) and not Graph.Running
        and (Graph.CurrentPass = 'copy'), 'hook rollback restores entries and selection');
      for I := 0 to 7 do
        Check(Graph.PassGraph[0].RandomIndex(1000) = Twin.PassGraph[0].RandomIndex(1000),
          'topology rejection also restores the constrained pass random stream');
      if Mode = 2 then
      begin
        Check(Report.Contradiction.ConstraintIndex = -1,
          'independent semantic validator failure has no connectivity clause ordinal');
        CheckTrace(Graph, Report);
      end;
    end;
  finally Twin.Free; Graph.Free; end;
end;

procedure TestRuleHookRevalidation;
var
  Graph: TTopologyHookGraph;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
  Scope: Integer;
  Raised, Solved: Boolean;
  Before: String;
begin
  Graph := TTopologyHookGraph.Create;
  try
    Graph.Seed := 55;
    Graph.Reshape(3, 1, 1);
    Graph.WrapNeighbors := False;
    Graph.CurrentPass := 'routes';
    Graph.AddValue('none'); Graph.AddValue('a'); Graph.AddValue('b');
    Graph.RequireConnectivity(Constraint('route', [gdEast, gdWest]));
    Graph.SwitchToPass('copy');
    Options := DefaultGraphSolveOptions;
    Options.CaptureTrace := True;
    for Scope := 0 to 1 do
    begin
      Graph.RewriteRules := False;
      Check(Graph.TrySolve(Options, Report), 'rule hook fixture prepares baseline');
      Before := Values(Graph);
      Graph.RewriteRules := True;
      Raised := False; Solved := False;
      try
        if Scope = 0 then Solved := Graph.TrySolve(Options, Report)
        else Solved := Graph.TryRegenerateFrom('copy', Options, Report);
      except on E: EInvalidOperation do Raised := True; end;
      Check(not Solved and Raised,
        'live compatibility cannot invalidate a published active or reused route');
      Check((Values(Graph) = Before) and not Graph.Running,
        'rule hook rejection restores entries and running state');
      Check(Graph.PassGraph[0].Rules['a'].Exists[gdEast],
        'legacy model mutation is a caller side effect, not an entry snapshot');
      //Explicitly restore caller-owned model edits before retrying.
      Graph.PassGraph[0].Rules['a'].Rules := nil;
      Graph.PassGraph[0].Rules['b'].Rules := nil;
    end;
  finally Graph.Free; end;
end;

begin
  Check(WFC_GRAPH_CONNECTIVITY_VERSION = 1, 'opt-in model version is explicit');
  Check(GraphTraceCauseKindName(gtckConnectivity) = 'connectivity', 'stable cause name');
  TestPropagationTrace;
  TestFailureAndTranscript;
  TestReusedConstraintEdit;
  TestTopologyHookRollback;
  TestRuleHookRevalidation;
  Check(Golden = TGraphTraceSignature($71BEEAB4), 'portable connectivity trace golden');
  WriteLn('CONNECTIVITY_TRACE_HASH=', IntToHex(Golden, 8));
  WriteLn('checks=', Checks, ' failures=', Failures);
  if Failures <> 0 then
    {$IFDEF PAS2JS}raise Exception.Create('connectivity trace checks failed');
    {$ELSE}Halt(1);{$ENDIF}
end.
