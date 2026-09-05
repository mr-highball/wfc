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
program wfc_trace_utility_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_trace;

type
  TTestProcedure = procedure;

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GCheckCount);
  if ACondition then
    WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailureCount);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  ATest;
end;

function MakeEvent(const AEventId: Integer;
  const AKind: TGraphTraceEventKind;
  const APassIndex: Integer): TGraphTraceEvent;
begin
  Result := Default(TGraphTraceEvent);
  Result.EventId := AEventId;
  Result.CauseEventId := -1;
  Result.Kind := AKind;
  Result.CauseKind := gtckNone;
  Result.PassIndex := APassIndex;
  Result.EntryIndex := -1;
  Result.ValueIndex := -1;
  Result.Value := '';
  Result.NeighborIndex := -1;
  Result.HasDirection := False;
  Result.Direction := gdNorth;
  Result.DependencyPassIndex := -1;
  Result.DecisionDepth := 0;
  Result.DomainCountBefore := 0;
  Result.DomainCountAfter := 0;
  Result.ConstraintIndex := -1;
end;

procedure SetValueEvent(var AEvent: TGraphTraceEvent;
  const AEntryIndex, AValueIndex: Integer;
  const AValue: TGraphValue;
  const ABefore, AAfter: Integer);
begin
  AEvent.EntryIndex := AEntryIndex;
  AEvent.ValueIndex := AValueIndex;
  AEvent.Value := AValue;
  AEvent.DomainCountBefore := ABefore;
  AEvent.DomainCountAfter := AAfter;
end;

function BuildGraph: TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Reshape(2, 2, 1);
    Result.CurrentPass := 'tokens';
    Result.AddValue('A');
    Result.AddValue('B');
    Result.SwitchToPass('classes');
    Result.AddValue('X');
    Result.AddValue('Y');
    Result.DependsOn('tokens');
    Result.SwitchToPass(0);
  except
    Result.Free;
    raise;
  end;
end;

function BuildValidReport: TGraphSolveReport;
begin
  Result := Default(TGraphSolveReport);
  Result.Status := gssSolved;
  Result.FailedPassIndex := -1;
  Result.TraceCaptured := True;
  SetLength(Result.Passes, 2);
  Result.Passes[0].TraceStart := 0;
  Result.Passes[0].TraceCount := 5;
  Result.Passes[1].TraceStart := 5;
  Result.Passes[1].TraceCount := 3;

  SetLength(Result.Trace, 9);
  Result.Trace[0] := MakeEvent(0, gtekPassBegin, 0);
  Result.Trace[0].CauseKind := gtckTransaction;

  Result.Trace[1] := MakeEvent(1,
    gtekInitialCandidateRemoved, 0);
  Result.Trace[1].CauseKind := gtckCallerDomain;
  SetValueEvent(Result.Trace[1], 0, 1, 'B', 2, 1);

  Result.Trace[2] := MakeEvent(2, gtekDecision, 0);
  Result.Trace[2].DecisionDepth := 1;
  SetValueEvent(Result.Trace[2], 1, 0, 'A', 2, 2);

  Result.Trace[3] := MakeEvent(3, gtekCandidateRemoved, 0);
  Result.Trace[3].CauseEventId := 2;
  Result.Trace[3].CauseKind := gtckDecision;
  Result.Trace[3].DecisionDepth := 1;
  SetValueEvent(Result.Trace[3], 1, 1, 'B', 2, 1);

  Result.Trace[4] := MakeEvent(4, gtekPassStaged, 0);
  Result.Trace[4].CauseEventId := 3;
  Result.Trace[4].CauseKind := gtckTransaction;

  Result.Trace[5] := MakeEvent(5, gtekPassBegin, 1);
  Result.Trace[5].CauseKind := gtckTransaction;

  Result.Trace[6] := MakeEvent(6,
    gtekInitialCandidateRemoved, 1);
  Result.Trace[6].CauseEventId := 4;
  Result.Trace[6].CauseKind := gtckPassDependency;
  Result.Trace[6].DependencyPassIndex := 0;
  SetValueEvent(Result.Trace[6], 2, 1, 'Y', 2, 1);

  Result.Trace[7] := MakeEvent(7, gtekPassStaged, 1);
  Result.Trace[7].CauseEventId := 6;
  Result.Trace[7].CauseKind := gtckTransaction;

  Result.Trace[8] := MakeEvent(8, gtekPipelineCommit, -1);
  Result.Trace[8].CauseEventId := 7;
  Result.Trace[8].CauseKind := gtckTransaction;
  Result.TraceHash := CalculateGraphTraceHash(Result);
end;

function CloneReport(const AReport: TGraphSolveReport): TGraphSolveReport;
var
  I: Integer;
begin
  Result := AReport;
  SetLength(Result.Passes, Length(AReport.Passes));
  for I := 0 to Length(AReport.Passes) - 1 do
    Result.Passes[I] := AReport.Passes[I];
  SetLength(Result.ExecutionOrder, Length(AReport.ExecutionOrder));
  for I := 0 to Length(AReport.ExecutionOrder) - 1 do
    Result.ExecutionOrder[I] := AReport.ExecutionOrder[I];
  Result.Trace := CopyGraphTraceEvents(AReport.Trace);
end;

function IsRejectedAs(const AGraph: TGraph;
  const AReport: TGraphSolveReport;
  const AKind: TGraphTraceValidationIssueKind): Boolean;
var
  LValidation: TGraphTraceValidationReport;
begin
  Result := (not ValidateGraphTrace(AGraph, AReport, LValidation)) and
    (LValidation.Issue.Kind = AKind);
end;

procedure TestStableNamesAndSignature;
begin
  Check(GraphTraceEventKindName(gtekPassBegin) = 'pass-begin',
    'event names use stable lowercase words');
  Check(GraphTraceEventKindName(gtekInitialCandidateRemoved) =
    'initial-candidate-removed',
    'multiword event names are unambiguous');
  Check(GraphTraceEventKindName(gtekPipelineRollback) =
    'pipeline-rollback', 'terminal rollback has a stable name');
  Check(GraphTraceCauseKindName(gtckCallerDomain) = 'caller-domain',
    'cause names use stable lowercase words');
  Check(GraphTraceCauseKindName(gtckRequiredSupport) =
    'required-support', 'required support has a stable cause name');
  Check(GraphTraceDirectionName(gdUp) = 'up',
    'direction names are stable');
  Check(GraphTraceValidationIssueKindName(gtvikPassSlice) =
    'pass-slice', 'validation issue names are stable');
  Check(GraphTraceValidationIssueKindName(gtvikTraceHash) =
    'trace-hash', 'signature validation has a stable issue name');
  Check(GraphTraceValidationIssueKindName(gtvikCausalLink) =
    'causal-link', 'causal validation has a stable issue name');
  Check(GraphTraceSignatureHex(TGraphTraceSignature($0123ABCD)) =
    '0123ABCD', 'trace signatures use eight uppercase hex digits');
  Check(GraphTraceSignatureHex(TGraphTraceSignature(0)) = '00000000',
    'zero trace signatures retain fixed width');
  Check((WFC_TRACE_VERSION = 1) and
      (WFC_TRACE_HASH_VERSION = 1) and
      (WFC_TRACE_UTILITY_VERSION = 1),
    'trace schema, hash, and utility versions are explicit');
end;

procedure TestLookupAndCopies;
var
  LCopy: TGraphTraceEvents;
  LEvent: TGraphTraceEvent;
  LReport: TGraphSolveReport;
  LRaised: Boolean;
begin
  LReport := BuildValidReport;
  Check(TryFindGraphTraceEvent(LReport.Trace, 6, LEvent) and
      (LEvent.Kind = gtekInitialCandidateRemoved) and
      (LEvent.PassIndex = 1),
    'event lookup uses public event ids');
  Check(not TryFindGraphTraceEvent(LReport.Trace, 99, LEvent),
    'missing event lookup returns false');

  LRaised := False;
  try
    FindGraphTraceEvent(LReport.Trace, 99);
  except
    on E: EWfcTrace do LRaised := True;
  end;
  Check(LRaised, 'strict event lookup rejects a missing id');

  LCopy := CopyGraphTraceEvents(LReport.Trace);
  LCopy[1].Value := 'changed';
  Check((Length(LCopy) = 9) and (LReport.Trace[1].Value = 'B'),
    'full trace copies are detached from the source array');

  LCopy := CopyGraphTraceEventsForPass(LReport.Trace, 0);
  Check((Length(LCopy) = 5) and (LCopy[0].EventId = 0) and
      (LCopy[4].EventId = 4),
    'pass filtering preserves chronological order');
  LCopy := CopyGraphTraceEventsForPass(LReport.Trace, 99);
  Check(Length(LCopy) = 0,
    'pass filtering returns an empty array when absent');

  LCopy := CopyGraphTraceEventsForEntry(LReport.Trace, 0, 1);
  Check((Length(LCopy) = 2) and (LCopy[0].EventId = 2) and
      (LCopy[1].EventId = 3),
    'entry filtering is pass-qualified and chronological');
end;

procedure TestCoordinatesAndFormatting;
var
  LDimension: TGraph.TDimension;
  LEvent: TGraphTraceEvent;
  LPosition: TGraphPosition;
  LRaised: Boolean;
begin
  LDimension.Width := 2;
  LDimension.Height := 3;
  LDimension.Depth := 2;
  Check(TryGraphEntryIndexToPosition(LDimension, 11, LPosition) and
      (LPosition.X = 1) and (LPosition.Y = 2) and
      (LPosition.Z = 1),
    'entry indices map to x-fastest graph coordinates');
  Check(not TryGraphEntryIndexToPosition(LDimension, 12, LPosition),
    'coordinate mapping rejects an index after the graph');
  Check(not TryGraphEntryIndexToPosition(LDimension, -1, LPosition),
    'coordinate mapping rejects a negative index');
  LDimension.Depth := 0;
  Check(not TryGraphEntryIndexToPosition(LDimension, 0, LPosition),
    'coordinate mapping rejects empty dimensions');

  LRaised := False;
  try
    GraphEntryIndexToPosition(LDimension, 0);
  except
    on E: EWfcTrace do LRaised := True;
  end;
  Check(LRaised, 'strict coordinate mapping rejects invalid dimensions');

  LEvent := MakeEvent(3, gtekCandidateRemoved, 0);
  LEvent.CauseEventId := 2;
  LEvent.CauseKind := gtckDecision;
  LEvent.DecisionDepth := 1;
  SetValueEvent(LEvent, 1, 1, 'B', 2, 1);
  Check(FormatGraphTraceEvent(LEvent) =
    'event=3 kind=candidate-removed cause=decision cause-event=2' +
    ' pass=0 entry=1 value-index=1 value=B neighbor=-1' +
    ' direction=none dependency-pass=-1 depth=1 domain=2->1',
    'one-line formatting has a fixed field order');
  LEvent.Value := 'A B'#10'%';
  Check(Pos('value=A%20B%0A%25 ', FormatGraphTraceEvent(LEvent)) > 0,
    'one-line formatting escapes whitespace and control characters');
  Check(Pos(#10, FormatGraphTraceEvent(LEvent)) = 0,
    'formatted events cannot inject another line');
end;

procedure TestStructuralValidation;
var
  LGraph: TGraph;
  LIssue: TGraphTraceValidationIssue;
  LReport: TGraphSolveReport;
  LSource: TGraphSolveReport;
  LValidation: TGraphTraceValidationReport;
begin
  LGraph := BuildGraph;
  try
    LSource := BuildValidReport;
    Check(ValidateGraphTrace(LGraph, LSource, LValidation) and
        LValidation.Valid and (LValidation.CheckedEvents = 9) and
        (LValidation.Issue.Kind = gtvikNone),
      'a complete committed trace is structurally valid');
    Check(DescribeGraphTraceValidationIssue(LValidation.Issue) =
      'trace is valid',
      'valid trace descriptions are explicit');

    LReport := CloneReport(LSource);
    Inc(LReport.TraceHash);
    Check(IsRejectedAs(LGraph, LReport, gtvikTraceHash),
      'a structurally valid trace with a changed signature is rejected');

    LReport := Default(TGraphSolveReport);
    LReport.Status := gssSolved;
    SetLength(LReport.Passes, 2);
    LReport.Passes[0].TraceStart := -1;
    LReport.Passes[1].TraceStart := -1;
    Check(ValidateGraphTrace(LGraph, LReport, LValidation),
      'disabled capture requires empty trace state');
    LReport.TraceHash := 1;
    Check(IsRejectedAs(LGraph, LReport, gtvikDisabledCapture),
      'disabled capture rejects a retained hash');

    LReport := CloneReport(LSource);
    LReport.Trace[2].EventId := 20;
    Check(IsRejectedAs(LGraph, LReport, gtvikEventId),
      'event ids must equal chronological indices');

    LReport := CloneReport(LSource);
    LReport.Trace[2].CauseEventId := 2;
    Check(IsRejectedAs(LGraph, LReport, gtvikCauseEventId),
      'cause ids must be absent or refer to earlier events');

    LReport := CloneReport(LSource);
    LReport.Trace[3].CauseEventId := 1;
    Check(IsRejectedAs(LGraph, LReport, gtvikCausalLink),
      'decision removals must link directly to their decision');

    LReport := CloneReport(LSource);
    LReport.Trace[6].CauseEventId := 3;
    Check(IsRejectedAs(LGraph, LReport, gtvikCausalLink),
      'dependency removals must link to provider completion');

    LReport := CloneReport(LSource);
    LReport.Trace[1].PassIndex := 2;
    Check(IsRejectedAs(LGraph, LReport, gtvikPassIndex),
      'pass-scoped events require a valid pass');

    LReport := CloneReport(LSource);
    LReport.Trace[1].EntryIndex := 4;
    Check(IsRejectedAs(LGraph, LReport, gtvikEntryIndex),
      'entry indices must fit graph dimensions');

    LReport := CloneReport(LSource);
    LReport.Trace[1].ValueIndex := 2;
    Check(IsRejectedAs(LGraph, LReport, gtvikValueIndex),
      'value indices must fit the event pass registry');

    LReport := CloneReport(LSource);
    LReport.Trace[1].Value := 'A';
    Check(IsRejectedAs(LGraph, LReport, gtvikValue),
      'inspection values must agree with stable value indices');

    LReport := CloneReport(LSource);
    LReport.Trace[1].Direction := gdEast;
    Check(IsRejectedAs(LGraph, LReport, gtvikDirection),
      'absent directions retain the canonical north sentinel');

    LReport := CloneReport(LSource);
    LReport.Trace[1].DependencyPassIndex := 1;
    Check(IsRejectedAs(LGraph, LReport,
      gtvikDependencyPassIndex),
      'dependency references must name a declared provider pass');

    LReport := CloneReport(LSource);
    LReport.Trace[1].DecisionDepth := -1;
    Check(IsRejectedAs(LGraph, LReport, gtvikDecisionDepth),
      'decision depths cannot be negative');

    LReport := CloneReport(LSource);
    LReport.Trace[1].DomainCountBefore := 1;
    LReport.Trace[1].DomainCountAfter := 2;
    Check(IsRejectedAs(LGraph, LReport, gtvikDomainCount),
      'candidate removal cannot grow a domain');

    LReport := CloneReport(LSource);
    LReport.Trace[1].DomainCountBefore := 3;
    LReport.Trace[1].DomainCountAfter := 1;
    Check(IsRejectedAs(LGraph, LReport, gtvikDomainCount),
      'candidate removal changes a domain by exactly one');

    LReport := CloneReport(LSource);
    LReport.Trace[3].Kind := gtekCandidateRestored;
    Check(IsRejectedAs(LGraph, LReport, gtvikDomainCount),
      'candidate restoration cannot shrink a domain');

    LReport := CloneReport(LSource);
    LReport.Trace[3].Kind := gtekCandidateRestored;
    LReport.Trace[3].DomainCountBefore := 1;
    LReport.Trace[3].DomainCountAfter := 3;
    Check(IsRejectedAs(LGraph, LReport, gtvikDomainCount),
      'candidate restoration changes a domain by exactly one');

    LReport := CloneReport(LSource);
    LReport.Passes[0].TraceCount := 4;
    Check(IsRejectedAs(LGraph, LReport, gtvikPassSlice),
      'every pass-scoped event belongs to its pass slice');

    LReport := CloneReport(LSource);
    LReport.Status := gssContradiction;
    Check(IsRejectedAs(LGraph, LReport, gtvikTerminalEvent),
      'a failed solve must terminate with pipeline rollback');

    LReport := CloneReport(LSource);
    LReport.Trace[8].PassIndex := 0;
    Check(IsRejectedAs(LGraph, LReport, gtvikPassIndex),
      'pipeline terminal events use the pass-minus-one sentinel');

    LReport := CloneReport(LSource);
    LReport.Trace[4].EntryIndex := 0;
    Check(IsRejectedAs(LGraph, LReport, gtvikEventFields),
      'pass metadata rejects cell-specific fields');

    LReport := CloneReport(LSource);
    LReport.Trace[1].ValueIndex := -1;
    LReport.Trace[1].Value := '';
    Check(IsRejectedAs(LGraph, LReport, gtvikEventFields),
      'candidate events require their removed value');

    LReport := CloneReport(LSource);
    LReport.Passes[0].TraceStart := 1;
    Check(IsRejectedAs(LGraph, LReport, gtvikPassSlice),
      'overlapping or mislabeled pass slices are rejected');

    LReport := CloneReport(LSource);
    LReport.TraceCaptured := False;
    Check(IsRejectedAs(LGraph, LReport, gtvikDisabledCapture),
      'capture false cannot retain events');

    LIssue.Kind := gtvikValue;
    LIssue.EventIndex := 1;
    LIssue.PassIndex := 0;
    Check(Pos('event 1', DescribeGraphTraceValidationIssue(
      LIssue)) > 0,
      'invalid trace descriptions locate event and pass');
  finally
    LGraph.Free;
  end;
end;

begin
  WriteLn('WFC trace utility conformance suite');
  WriteLn('===================================');
  RunTest('stable names and signatures', @TestStableNamesAndSignature);
  RunTest('lookup and filtering', @TestLookupAndCopies);
  RunTest('coordinates and formatting', @TestCoordinatesAndFormatting);
  RunTest('structural validation', @TestStructuralValidation);
  WriteLn('===================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));
  if GFailureCount <> 0 then
    raise Exception.CreateFmt('%d trace utility checks failed',
      [GFailureCount]);
end.
