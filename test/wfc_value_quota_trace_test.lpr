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
program wfc_value_quota_trace_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_trace, wfc_trace_stream;

var
  Checks, Failures: Integer;
  Golden: TGraphTraceSignature;

procedure Check(const ACondition: Boolean; const ALabel: String);
begin
  Inc(Checks);
  if ACondition then Exit;
  Inc(Failures);
  WriteLn('FAIL: ', ALabel);
end;

function Fixture: TGraph;
begin
  Result := TGraph.Create;
  Result.Seed := 55;
  Result.Reshape(3, 1, 1);
  Result.WrapNeighbors := False;
  Result.CurrentPass := 'housing';
  Result.AddValue('empty'); Result.AddValue('house'); Result.AddValue('shop');
  Result.RequireValueQuota(MakeGraphValueQuotaConstraint(
    'occupied', ['house', 'shop'], 3, 3));
  Result.RequireValueQuota(MakeGraphValueQuotaConstraint(
    'same-extent', ['shop', 'house'], 3, 5));
end;

function Values(const AGraph: TGraph): String;
var I: Integer;
begin
  Result := '';
  for I := 0 to 2 do
    Result := Result + '/' + AGraph.PassGraph[0].Entry[I, 0, 0].Value;
end;

procedure CheckTrace(const AGraph: TGraph; const AReport: TGraphSolveReport);
var
  Validation: TGraphTraceValidationReport;
  Layout: TGraphTraceLayout;
  I: Integer;
  Valid: Boolean;
begin
  Check(AReport.TraceHash = CalculateGraphTraceHash(AReport), 'trace rehash');
  Valid := ValidateGraphTrace(AGraph, AReport, Validation);
  Check(Valid, 'legacy trace validation: ' +
    DescribeGraphTraceValidationIssue(Validation.Issue));
  Valid := TryBuildGraphTraceLayout(AGraph, AReport, Layout, Validation);
  Check(Valid, 'chronological trace validation: ' +
    DescribeGraphTraceValidationIssue(Validation.Issue));
  if Valid then
    Check(ValidateGraphTraceLayout(AGraph, AReport, Layout, Validation),
      'derived layout validates');
  for I := 0 to High(AReport.Trace) do
    if AReport.Trace[I].CauseKind = gtckValueQuota then
      Check((AReport.Trace[I].ConstraintIndex >= 0) and
        (AReport.Trace[I].NeighborIndex = -1) and
        (AReport.Trace[I].DependencyPassIndex = -1) and
        not AReport.Trace[I].HasDirection,
        'quota cause identifies a clause without fabricated adjacency');
end;

procedure TestTraceAndForgery;
var
  Graph: TGraph;
  Options: TGraphSolveOptions;
  Report, Damaged: TGraphSolveReport;
  Validation: TGraphTraceValidationReport;
  I, Removal, Decision, RemovedCount: Integer;
begin
  Graph := Fixture;
  try
    Options := DefaultGraphSolveOptions;
    Options.CaptureTrace := True;
    Check(Graph.TrySolve(Options, Report), 'occupied alternatives solve');
    CheckTrace(Graph, Report);
    Golden := Report.TraceHash;
    Removal := -1; Decision := -1; RemovedCount := 0;
    for I := 0 to High(Report.Trace) do
    begin
      if (Report.Trace[I].Kind = gtekCandidateRemoved) and
        (Report.Trace[I].CauseKind = gtckValueQuota) then
      begin
        Removal := I;
        Inc(RemovedCount);
        Check((Report.Trace[I].ConstraintIndex = 0) and
          (Report.Trace[I].Value = 'empty') and
          (Report.Trace[I].DecisionDepth = 0), 'minimum prunes before decisions');
      end;
      if (Report.Trace[I].Kind = gtekDecision) and
        (Report.Trace[I].CauseKind = gtckValueQuota) then Decision := I;
    end;
    Check(RemovedCount = 3, 'all three mixed domains lose nonmembers');
    Check(Decision >= 0, 'choice inherits the last quota removal cause');
    if Removal >= 0 then
    begin
      Check(Pos('value-quota=0', FormatGraphTraceEvent(Report.Trace[Removal])) > 0,
        'formatted event exposes quota identity');
      Damaged := Report;
      Damaged.Trace := CopyGraphTraceEvents(Report.Trace);
      Damaged.Trace[Removal].ConstraintIndex := 1;
      Check(CalculateGraphTraceHash(Damaged) <> Report.TraceHash,
        'quota ordinal contributes to hash');
      Damaged.Trace[Removal].ConstraintIndex := -1;
      Damaged.TraceHash := CalculateGraphTraceHash(Damaged);
      Check(not ValidateGraphTrace(Graph, Damaged, Validation) and
        (Validation.Issue.Kind = gtvikConstraintIndex), 'missing ordinal rejected');
      Damaged.Trace[Removal].ConstraintIndex := 2;
      Damaged.TraceHash := CalculateGraphTraceHash(Damaged);
      Check(not ValidateGraphTrace(Graph, Damaged, Validation) and
        (Validation.Issue.Kind = gtvikConstraintIndex), 'out-of-registry ordinal rejected');
      Damaged.Trace[Removal] := Report.Trace[Removal];
      Damaged.Trace[Removal].HasDirection := True;
      Damaged.TraceHash := CalculateGraphTraceHash(Damaged);
      Check(not ValidateGraphTrace(Graph, Damaged, Validation),
        'quota cannot fabricate an adjacency direction');
      Damaged.Trace[Removal] := Report.Trace[Removal];
      Damaged.Trace[Removal].CauseKind := gtckConnectivity;
      Damaged.TraceHash := CalculateGraphTraceHash(Damaged);
      Check(not ValidateGraphTrace(Graph, Damaged, Validation) and
        (Validation.Issue.Kind = gtvikConstraintIndex),
        'quota registry cannot supply a connectivity ordinal');
    end;
    if Decision >= 0 then
    begin
      Damaged := Report;
      Damaged.Trace := CopyGraphTraceEvents(Report.Trace);
      Damaged.Trace[Decision].ConstraintIndex := 1;
      Damaged.TraceHash := CalculateGraphTraceHash(Damaged);
      Check(not ValidateGraphTrace(Graph, Damaged, Validation) and
        (Validation.Issue.Kind = gtvikCausalLink),
        'decision must inherit its actual cause ordinal');
    end;
    Damaged := Report;
    Damaged.Trace := CopyGraphTraceEvents(Report.Trace);
    Damaged.Trace[0].ConstraintIndex := 0;
    Damaged.TraceHash := CalculateGraphTraceHash(Damaged);
    Check(not ValidateGraphTrace(Graph, Damaged, Validation) and
      (Validation.Issue.Kind = gtvikConstraintIndex), 'ordinary metadata has no quota ordinal');
  finally Graph.Free; end;
end;

procedure TestFailureAndTranscript;
var
  Graph: TGraph;
  Options: TGraphSolveOptions;
  Report, Damaged: TGraphSolveReport;
  Validation: TGraphTraceValidationReport;
  Layout: TGraphTraceLayout;
  Negotiation, Changed: TGraphNegotiationReport;
  NegotiationOptions: TGraphNegotiationOptions;
  Root: TGraphPosition;
  Profiles: TGraphConnectivityValues;
  Before: String;
  I: Integer;
begin
  Graph := Fixture;
  try
    Options := DefaultGraphSolveOptions;
    Check(Graph.TrySolve(Options, Report), 'failure fixture baseline');
    Before := Values(Graph);
    Graph.RemoveValueQuota('same-extent');
    Graph.RequireValueQuota(MakeGraphValueQuotaConstraint(
      'impossible', ['house', 'shop'], 4, 4));
    //One connectivity clause and two quotas: their ordinal spaces differ.
    Root.X := 0; Root.Y := 0; Root.Z := 0;
    SetLength(Profiles, 2);
    Profiles[0] := MakeGraphConnectivityValue('house', [gdEast, gdWest]);
    Profiles[1] := MakeGraphConnectivityValue('shop', [gdEast, gdWest]);
    Graph.RequireConnectivity(MakeGraphConnectivityConstraint('road', Root, nil, Profiles));
    Options.CaptureTrace := True;
    Check(not Graph.TrySolve(Options, Report), 'impossible whole-pass minimum fails');
    Check((Report.Contradiction.Kind = gckValueQuota) and
      (Report.Contradiction.ConstraintIndex = 1) and
      (Report.Contradiction.EntryIndex = -1), 'global failure names the second quota');
    Check(Values(Graph) = Before, 'failure restores the old public composition');
    CheckTrace(Graph, Report);
    for I := 0 to High(Report.Trace) do
      if (Report.Trace[I].Kind = gtekContradiction) and
        (Report.Trace[I].CauseKind = gtckValueQuota) then
      begin
        Damaged := Report;
        Damaged.Trace := CopyGraphTraceEvents(Report.Trace);
        Damaged.Trace[I].EntryIndex := 0;
        Damaged.Contradiction.EntryIndex := 0;
        Damaged.TraceHash := CalculateGraphTraceHash(Damaged);
        Check(not ValidateGraphTrace(Graph, Damaged, Validation) and
          (Validation.Issue.Kind = gtvikEventFields),
          'global quota failure cannot fabricate a concrete cell');
        Check(not TryBuildGraphTraceLayout(Graph, Damaged, Layout, Validation),
          'chronological validator rejects a matching forged cell summary');
        Damaged := Report;
        Damaged.Trace := CopyGraphTraceEvents(Report.Trace);
        Damaged.Trace[I].DomainCountBefore := 7;
        Damaged.Trace[I].DomainCountAfter := 7;
        Damaged.TraceHash := CalculateGraphTraceHash(Damaged);
        Check(not ValidateGraphTrace(Graph, Damaged, Validation) and
          (Validation.Issue.Kind = gtvikEventFields),
          'global quota failure cannot invent a cell domain size');
        Check(not TryBuildGraphTraceLayout(Graph, Damaged, Layout, Validation),
          'chronological validator rejects forged global domain counts');
      end;
    NegotiationOptions := DefaultGraphNegotiationOptions;
    NegotiationOptions.SolveOptions.CaptureTrace := False;
    Check(not Graph.TrySolveNegotiated(NegotiationOptions, Negotiation),
      'negotiation retains the impossible quota');
    Check(Negotiation.TranscriptHash =
      CalculateGraphNegotiationTranscriptHash(NegotiationOptions, Negotiation),
      'quota negotiation rehashes without trace capture');
    Changed := Negotiation;
    Changed.FinalReport.Contradiction.ConstraintIndex := 0;
    Check(CalculateGraphNegotiationTranscriptHash(NegotiationOptions, Changed) <>
      Negotiation.TranscriptHash, 'capture-disabled transcript includes quota ordinal');
    Graph.RemoveValueQuota('impossible');
    Check(Graph.TrySolve(Options, Report), 'removing the impossible quota recovers');
    CheckTrace(Graph, Report);
  finally Graph.Free; end;
end;

procedure TestStreamWindows;
var
  Oracle, Graph: TGraph;
  Sink: TGraphTraceWindowSink;
  Options: TGraphSolveOptions;
  Expected, Actual, Rebuilt: TGraphSolveReport;
  Events: TGraphTraceEvents;
  I, Capacity, Offset: Integer;
begin
  Oracle := Fixture;
  try
    Options := DefaultGraphSolveOptions; Options.CaptureTrace := True;
    Check(Oracle.TrySolve(Options, Expected), 'captured window oracle');
    for Capacity := 0 to Length(Expected.Trace) do
    begin
      Graph := Fixture;
      Sink := TGraphTraceWindowSink.Create(Capacity);
      try
        Graph.TraceSink := Sink;
        Options.CaptureTrace := False;
        Check(Graph.TrySolve(Options, Actual), 'sink-only quota solve');
        Check((Values(Graph) = Values(Oracle)) and
          (Actual.TraceHash = 0) and (Length(Actual.Trace) = 0),
          'trace storage cannot alter the composition');
        Check((Actual.TraceDelivery.Status = gtdsComplete) and
          (Actual.TraceDelivery.TraceHash = Expected.TraceHash) and
          (Sink.Delivery.TraceHash = Expected.TraceHash),
          'every bounded window preserves the whole-stream hash');
        Events := Sink.CopyEvents;
        Offset := Length(Expected.Trace) - Length(Events);
        for I := 0 to High(Events) do
          Check(FormatGraphTraceEvent(Events[I]) =
            FormatGraphTraceEvent(Expected.Trace[Offset + I]),
            'window retains exact original events and quota ordinals');
        if Sink.Complete then
        begin
          Rebuilt := Actual;
          Rebuilt.TraceCaptured := True;
          Rebuilt.Trace := Sink.CopyEvents;
          Rebuilt.TraceHash := Sink.Delivery.TraceHash;
          Rebuilt.Passes := Copy(Actual.Passes);
          for I := 0 to High(Rebuilt.Passes) do
          begin
            Rebuilt.Passes[I].TraceStart := Expected.Passes[I].TraceStart;
            Rebuilt.Passes[I].TraceCount := Expected.Passes[I].TraceCount;
          end;
          CheckTrace(Graph, Rebuilt);
        end;
      finally
        Graph.TraceSink := nil;
        Sink.Free;
        Graph.Free;
      end;
    end;
  finally Oracle.Free; end;
end;

begin
  Check(WFC_GRAPH_VALUE_QUOTA_VERSION = 1, 'explicit opt-in version');
  Check(GraphTraceCauseKindName(gtckValueQuota) = 'value-quota', 'stable cause name');
  TestTraceAndForgery;
  TestFailureAndTranscript;
  TestStreamWindows;
  Check(Golden = TGraphTraceSignature($19EBB6BA), 'portable quota trace golden');
  WriteLn('VALUE_QUOTA_TRACE_HASH=', IntToHex(Golden, 8));
  WriteLn('checks=', Checks, ' failures=', Failures);
  if Failures <> 0 then
    {$IFDEF PAS2JS}raise Exception.Create('value quota trace checks failed');
    {$ELSE}Halt(1);{$ENDIF}
end.
