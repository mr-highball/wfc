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
program wfc_trace_layout_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_trace;

type
  TRejectingGraph = class(TGraph)
  protected
    function DoValidateCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean; override;
  public
    RejectPass, RejectEntry: Integer;
    RaiseInHook: Boolean;
    HookCalls: Integer;
  end;

var
  Checks, Failures: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if ACondition then Exit;
  Inc(Failures);
  WriteLn('FAIL: ', AMessage);
end;

function TRejectingGraph.DoValidateCommit(out AFailedPassIndex,
  AFailedEntryIndex: Integer): Boolean;
begin
  Inc(HookCalls);
  if RaiseInHook then raise Exception.Create('test commit hook exception');
  Result := RejectPass < 0;
  AFailedPassIndex := RejectPass;
  if Result then AFailedEntryIndex := -1 else AFailedEntryIndex := RejectEntry;
end;

function NewGraph(const APassCount: Integer;
  const AWidth: Integer = 1): TRejectingGraph;
const
  Names: array[0..3] of String = ('first', 'second', 'third', 'fourth');
var
  I: Integer;
begin
  Result := TRejectingGraph.Create;
  try
    Result.RejectPass := -1;
    Result.RejectEntry := 0;
    Result.Seed := 55;
    Result.Reshape(AWidth, 1, 1);
    Result.WrapNeighbors := False;
    for I := 0 to APassCount - 1 do
    begin
      if I = 0 then Result.CurrentPass := Names[I]
      else
      begin
        Result.SwitchToPass(Names[I]);
        Result.PassMode := gpmOverlay;
      end;
      Result.AddValue(Chr(Ord('A') + I));
    end;
  except
    Result.Free;
    raise;
  end;
end;

function CaptureOptions: TGraphSolveOptions;
begin
  Result := DefaultGraphSolveOptions;
  Result.CaptureTrace := True;
end;

function CloneReport(const AReport: TGraphSolveReport): TGraphSolveReport;
var
  I: Integer;
begin
  Result := AReport;
  Result.Passes := nil;
  SetLength(Result.Passes, Length(AReport.Passes));
  for I := 0 to High(AReport.Passes) do Result.Passes[I] := AReport.Passes[I];
  Result.ExecutionOrder := nil;
  SetLength(Result.ExecutionOrder, Length(AReport.ExecutionOrder));
  for I := 0 to High(AReport.ExecutionOrder) do
    Result.ExecutionOrder[I] := AReport.ExecutionOrder[I];
  Result.Trace := CopyGraphTraceEvents(AReport.Trace);
end;

procedure CheckUnchanged(const ABefore, AAfter: TGraphSolveReport;
  const AContext: String);
var
  I: Integer;
begin
  Check((ABefore.TraceHash = AAfter.TraceHash)
    and (ABefore.Status = AAfter.Status)
    and (ABefore.FailedPassIndex = AAfter.FailedPassIndex)
    and (Length(ABefore.Trace) = Length(AAfter.Trace)), AContext + ' report identity');
  for I := 0 to High(ABefore.Trace) do
    Check(FormatGraphTraceEvent(ABefore.Trace[I]) =
      FormatGraphTraceEvent(AAfter.Trace[I]), AContext + ' original event ' + IntToStr(I));
  for I := 0 to High(ABefore.Passes) do
    Check((ABefore.Passes[I].TraceStart = AAfter.Passes[I].TraceStart)
      and (ABefore.Passes[I].TraceCount = AAfter.Passes[I].TraceCount),
      AContext + ' legacy slice ' + IntToStr(I));
end;

function CheckedLayout(const AGraph: TGraph; const AReport: TGraphSolveReport;
  const AContext: String): TGraphTraceLayout;
var
  Validation: TGraphTraceValidationReport;
  Before: TGraphSolveReport;
  I, J, K, Count, RangeCount, RangeStart, Last: Integer;
begin
  Before := CloneReport(AReport);
  if not TryBuildGraphTraceLayout(AGraph, AReport, Result, Validation) then
    raise Exception.Create(AContext + ': unexpected build rejection: ' +
      DescribeGraphTraceValidationIssue(Validation.Issue));
  Check(Validation.Valid and (Validation.Issue.Kind = gtvikNone),
    AContext + ' valid diagnostic');
  Check(ValidateGraphTraceLayout(AGraph, AReport, Result, Validation),
    AContext + ' view independently revalidates');
  Check((Result.Version = WFC_TRACE_LAYOUT_VERSION)
    and (Result.TraceCaptured = AReport.TraceCaptured)
    and (Result.TraceHash = AReport.TraceHash)
    and (Result.EventCount = Length(AReport.Trace))
    and (Length(Result.Passes) = Length(AReport.Passes)), AContext + ' header');
  Check(Result.TerminalEventIndex = Length(AReport.Trace) - 1,
    AContext + ' terminal index');
  for I := 0 to High(Result.Passes) do
  begin
    Count := 0; RangeCount := 0; Last := -2; RangeStart := -1;
    for J := 0 to High(AReport.Trace) do
      if AReport.Trace[J].PassIndex = I then
      begin
        Inc(Count);
        if J <> Last + 1 then
        begin
          Inc(RangeCount);
          RangeStart := J;
          Check(Result.Passes[I].Ranges[RangeCount - 1].Start = J,
            AContext + ' independently found maximal range start');
        end;
        K := J - RangeStart + 1;
        if (J = High(AReport.Trace)) or
          (AReport.Trace[J + 1].PassIndex <> I) then
          Check(Result.Passes[I].Ranges[RangeCount - 1].Count = K,
            AContext + ' independently found maximal range count');
        Last := J;
      end;
    Check((Result.Passes[I].EventCount = Count)
      and (Length(Result.Passes[I].Ranges) = RangeCount),
      AContext + ' independent pass coverage ' + IntToStr(I));
  end;
  CheckUnchanged(Before, AReport, AContext);
end;

procedure ExpectReportRejected(const AGraph: TGraph;
  const AReport: TGraphSolveReport; const AContext: String);
var
  Layout: TGraphTraceLayout;
  Validation: TGraphTraceValidationReport;
  Accepted: Boolean;
begin
  Layout := Default(TGraphTraceLayout);
  Layout.Version := 99; Layout.TraceCaptured := True; Layout.TraceHash := 123;
  Layout.EventCount := 4; Layout.TerminalEventIndex := 3;
  SetLength(Layout.Passes, 1); SetLength(Layout.Passes[0].Ranges, 1);
  Layout.Passes[0].EventCount := 3;
  try
    Accepted := TryBuildGraphTraceLayout(AGraph, AReport, Layout, Validation);
  except
    on E: Exception do
    begin
      Check(False, AContext + ' returns a diagnostic, not ' + E.ClassName);
      Exit;
    end;
  end;
  Check(not Accepted and not Validation.Valid and
    (Validation.Issue.Kind <> gtvikNone), AContext + ' rejected');
  Check((Layout.Version = 0) and not Layout.TraceCaptured
    and (Layout.TraceHash = 0) and (Layout.EventCount = 0)
    and (Layout.TerminalEventIndex = 0) and (Length(Layout.Passes) = 0),
    AContext + ' failed out layout fully reset');
end;

procedure ExpectLayoutRejected(const AGraph: TGraph;
  const AReport: TGraphSolveReport; const ALayout: TGraphTraceLayout;
  const AContext: String);
var
  Validation: TGraphTraceValidationReport;
  Accepted: Boolean;
begin
  try
    Accepted := ValidateGraphTraceLayout(AGraph, AReport, ALayout, Validation);
  except
    on E: Exception do
    begin
      Check(False, AContext + ' returns a diagnostic, not ' + E.ClassName);
      Exit;
    end;
  end;
  Check(not Accepted and not Validation.Valid
    and (Validation.Issue.Kind <> gtvikNone), AContext + ' view rejected');
end;

procedure TestActualHooks;
var
  G: TRejectingGraph;
  R: TGraphSolveReport;
  L: TGraphTraceLayout;
  V: TGraphTraceValidationReport;
  PassCount, RejectPass, I: Integer;
  Solved: Boolean;
begin
  for PassCount := 1 to 3 do
    for RejectPass := -1 to PassCount - 1 do
    begin
      G := NewGraph(PassCount);
      try
        G.RejectPass := RejectPass;
        Solved := G.TrySolve(CaptureOptions, R);
        Check(Solved = (RejectPass < 0), 'actual hook outcome');
        Check(G.HookCalls = 1, 'one actual commit hook call');
        L := CheckedLayout(G, R, 'actual hook ' + IntToStr(PassCount) + '/' + IntToStr(RejectPass));
        Check(ValidateGraphTrace(G, R, V) =
          ((RejectPass < 0) or (RejectPass = PassCount - 1)),
          'strict v1 validator retains its contiguous boundary');
        if RejectPass >= 0 then
        begin
          Check((R.Status = gssContradiction)
            and (R.Contradiction.Kind = gckFinalValidation)
            and (R.FailedPassIndex = RejectPass), 'actual failure classification');
          for I := 0 to PassCount - 1 do
            Check(G.PassGraph[I].Entry[0, 0, 0].Empty
              and not G.PassGraph[I].Entry[0, 0, 0].Generated,
              'rejected candidate rolls back every pass');
          if RejectPass < PassCount - 1 then
            Check(Length(L.Passes[RejectPass].Ranges) = 2,
              'earlier pass has exactly two derived ranges')
          else Check(Length(L.Passes[RejectPass].Ranges) = 1,
            'latest pass retains one maximal range');
        end;
        if PassCount = 2 then
          case RejectPass of
            -1: Check(R.TraceHash = TGraphTraceSignature($709F3450), 'original success hash');
             0: Check(R.TraceHash = TGraphTraceSignature($B27D0AE0), 'original earlier-pass failure hash');
             1: Check(R.TraceHash = TGraphTraceSignature($460B78DC), 'original last-pass failure hash');
          end;
      finally G.Free; end;
    end;

  G := NewGraph(2, 0);
  try
    G.RejectPass := 0; G.RejectEntry := -1;
    Check(not G.TrySolve(CaptureOptions, R), 'zero-cell whole-pass rejection');
    L := CheckedLayout(G, R, 'zero-cell entryless');
    Check((R.Contradiction.EntryIndex = -1)
      and (Length(L.Passes[0].Ranges) = 2), 'entryless suffix represented');
  finally G.Free; end;

  G := NewGraph(2);
  try
    G.RejectPass := 0; G.RejectEntry := -1;
    Check(not G.TrySolve(CaptureOptions, R), 'nonempty entryless rejection');
    L := CheckedLayout(G, R, 'nonempty entryless');
    Check(L.EventCount = 7, 'nonempty entryless event count');
  finally G.Free; end;
end;

procedure TestExecutionOrderAndReuse;
var
  G: TRejectingGraph;
  R, Baseline: TGraphSolveReport;
  L: TGraphTraceLayout;
  I: Integer;
begin
  G := NewGraph(3);
  try
    G.SwitchToPass('third'); G.ClearDependencies; G.DependsOn('first');
    G.SwitchToPass('second'); G.ClearDependencies; G.DependsOn('third');
    G.RejectPass := 2;
    Check(not G.TrySolve(CaptureOptions, R), 'topological middle pass rejects');
    Check((R.ExecutionOrder[0] = 0) and (R.ExecutionOrder[1] = 2)
      and (R.ExecutionOrder[2] = 1), 'execution order is not numeric');
    L := CheckedLayout(G, R, 'topological order');
    Check(Length(L.Passes[2].Ranges) = 2, 'nonnumeric middle pass split');
  finally G.Free; end;

  for I := 0 to 1 do
  begin
    G := NewGraph(3);
    try
      G.SwitchToPass('second'); G.ClearDependencies;
      G.SwitchToPass('third'); G.ClearDependencies;
      if I = 0 then G.DependsOn('second') else G.DependsOn('first');
      Check(G.TrySolve(CaptureOptions, Baseline), 'selective baseline commits');
      if I = 0 then
      begin
        G.RejectPass := 1;
        Check(not G.TryRegenerateFrom('second', CaptureOptions, R),
          'later two active passes reject with earlier reused input');
      end
      else
      begin
        G.RejectPass := 0;
        Check(not G.TryRegenerateFrom('first', CaptureOptions, R),
          'pass-one reused prefix precedes active pass zero');
      end;
      L := CheckedLayout(G, R, 'selective reuse ' + IntToStr(I));
      Check((R.Trace[0].Kind = gtekPassSkipped)
        and (R.Trace[0].PassIndex = I), 'reused pass emitted first');
      Check((Length(L.Passes[I].Ranges) = 1)
        and (L.Passes[I].EventCount = 1), 'reused pass has singleton range');
      Check((G.PassGraph[0].Entry[0, 0, 0].Value = 'A')
        and (G.PassGraph[1].Entry[0, 0, 0].Value = 'B')
        and (G.PassGraph[2].Entry[0, 0, 0].Value = 'C'),
        'selective rejection preserves previously committed values');
    finally G.Free; end;
  end;
end;

procedure TestDisabledAndCopies;
var
  G: TRejectingGraph;
  R: TGraphSolveReport;
  O: TGraphSolveOptions;
  L, C: TGraphTraceLayout;
  V: TGraphTraceValidationReport;
  I: Integer;
begin
  G := NewGraph(2);
  try
    O := DefaultGraphSolveOptions;
    Check(G.TrySolve(O, R), 'disabled capture solves normally');
    L := CheckedLayout(G, R, 'disabled capture');
    Check(not L.TraceCaptured and (L.TraceHash = 0) and (L.EventCount = 0)
      and (L.TerminalEventIndex = -1), 'disabled layout canonical empty header');
    for I := 0 to High(L.Passes) do
      Check((L.Passes[I].EventCount = 0) and (Length(L.Passes[I].Ranges) = 0),
        'disabled layout retains empty pass records');
    C := CopyGraphTraceLayout(L);
    C.Passes[0].EventCount := 3;
    Check(L.Passes[0].EventCount = 0, 'disabled pass arrays deeply detached');
    ExpectLayoutRejected(G, R, C, 'disabled forged pass count');

    G.RejectPass := 0;
    Check(not G.TrySolve(CaptureOptions, R), 'copy fixture rejects');
    L := CheckedLayout(G, R, 'copy source');
    C := CopyGraphTraceLayout(L);
    Check(ValidateGraphTraceLayout(G, R, C, V), 'deep copy retains valid content');
    C.Passes[0].Ranges[0].Start := 99;
    C.Passes[0].EventCount := 99;
    Check((L.Passes[0].Ranges[0].Start = 0) and (L.Passes[0].EventCount = 4),
      'nested ranges and pass counters are detached');
    Check(R.Passes[0].TraceStart = 0, 'layout mutation cannot alter source slices');
    ExpectLayoutRejected(G, R, C, 'mutated detached copy');
    C := CopyGraphTraceLayout(Default(TGraphTraceLayout));
    Check((C.Version = 0) and (Length(C.Passes) = 0), 'copy accepts default empty layout');
    ExpectReportRejected(nil, R, 'nil graph');
    R.TraceCaptured := False;
    ExpectReportRejected(G, R, 'disabled report cannot retain events');
  finally G.Free; end;
end;

procedure TestLocalSearchAndHookBoundaries;
var
  G: TRejectingGraph;
  O: TGraphSolveOptions;
  R: TGraphSolveReport;
  L: TGraphTraceLayout;
  Limit, I: Integer;
  Raised: Boolean;
begin
  for Limit := 0 to 1 do
  begin
    G := TRejectingGraph.Create;
    try
      G.RejectPass := -1;
      G.Seed := 0; G.Reshape(3, 1, 1);
      G.AddValue('A').NewRule([gdEast, gdWest], 'B');
      G.AddValue('C').NewRule([gdEast, gdWest], 'C');
      G.SwitchToPass('later'); G.PassMode := gpmOverlay;
      G.AddValue('X');
      O := CaptureOptions;
      if Limit = 0 then O.MaxBacktracks := 0 else O.MaxBacktracks := 64;
      Check(G.TrySolve(O, R) = (Limit = 1), 'local recovery/limit fixture outcome');
      L := CheckedLayout(G, R, 'ordinary local search ' + IntToStr(Limit));
      if Limit = 0 then
        Check((R.Status = gssBacktrackLimit) and (G.HookCalls = 0)
          and (L.Passes[1].EventCount = 0)
          and (Length(L.Passes[1].Ranges) = 0),
          'failed search keeps unattempted downstream pass empty')
      else
        Check((R.Passes[0].Backtracks > 0) and (G.HookCalls = 1),
          'ordinary local branch restoration remains representable');
    finally G.Free; end;
  end;

  G := NewGraph(3);
  try
    G.SwitchToPass('first'); G.Entry[0, 0, 0].Value := 'unknown-lock';
    Check(not G.TrySolve(CaptureOptions, R), 'invalid caller lock contradicts');
    L := CheckedLayout(G, R, 'ordinary lock contradiction');
    Check((G.HookCalls = 0) and (L.Passes[1].EventCount = 0)
      and (L.Passes[2].EventCount = 0), 'early contradiction has no fabricated later events');
  finally G.Free; end;

  G := TRejectingGraph.Create;
  try
    G.RejectPass := 0; G.RejectEntry := -1;
    G.Reshape(1, 1, 1); G.CurrentPass := 'unconfigured';
    G.SwitchToPass('configured'); G.AddValue('A');
    Check(not G.TrySolve(CaptureOptions, R), 'active definitionless first pass can be rejected');
    L := CheckedLayout(G, R, 'active definitionless pass');
    Check(R.Passes[0].Executed and (Length(L.Passes[0].Ranges) = 2),
      'executed definitionless pass has staged suffix eligibility');
  finally G.Free; end;

  for I := 0 to 2 do
  begin
    G := NewGraph(2);
    try
      Check(G.TrySolve(CaptureOptions, R), 'hook-boundary baseline');
      case I of
        0: begin G.RejectPass := 0; end;
        1: begin G.RejectPass := 2; end;
        2: begin G.RaiseInHook := True; end;
      end;
      Raised := False;
      try
        if I = 0 then G.TryRegenerateFrom('second', CaptureOptions, R)
        else G.TrySolve(CaptureOptions, R);
      except on E: Exception do Raised := True; end;
      Check(Raised, 'invalid scope/index or hook exception is not a new ordinary failure');
      Check(not G.Running and (G.PassGraph[0].Entry[0, 0, 0].Value = 'A')
        and (G.PassGraph[1].Entry[0, 0, 0].Value = 'B'),
        'hook boundary exception retains original committed values');
    finally G.Free; end;
  end;
end;

procedure TestMalformedLayouts;
var
  G: TRejectingGraph;
  R: TGraphSolveReport;
  Source, L: TGraphTraceLayout;
  I: Integer;
begin
  G := NewGraph(2);
  try
    G.RejectPass := 0; G.TrySolve(CaptureOptions, R);
    Source := CheckedLayout(G, R, 'view mutation baseline');
    for I := 0 to 17 do
    begin
      L := CopyGraphTraceLayout(Source);
      case I of
        0: L.Version := 0;
        1: L.Version := 2;
        2: L.TraceCaptured := False;
        3: L.TraceHash := L.TraceHash xor 1;
        4: L.EventCount := L.EventCount - 1;
        5: L.TerminalEventIndex := 0;
        6: SetLength(L.Passes, 1);
        7: L.Passes[0].EventCount := 3;
        8: L.Passes[0].Ranges[0].Start := -1;
        9: L.Passes[0].Ranges[0].Count := 0;
        10: L.Passes[0].Ranges[0].Count := High(Integer);
        11: L.Passes[0].Ranges[1].Start := 2;
        12: L.Passes[0].Ranges[1].Count := 3;
        13: SetLength(L.Passes[0].Ranges, 1);
        14: begin
          SetLength(L.Passes[0].Ranges, 3);
          L.Passes[0].Ranges[2] := L.Passes[0].Ranges[1];
        end;
        15: begin
          L.Passes[0].Ranges[0].Count := 1;
          SetLength(L.Passes[0].Ranges, 3);
          L.Passes[0].Ranges[2] := L.Passes[0].Ranges[1];
          L.Passes[0].Ranges[1].Start := 1;
          L.Passes[0].Ranges[1].Count := 1;
        end;
        16: begin
          L.Passes[0].Ranges[0].Start := 4;
          L.Passes[0].Ranges[1].Start := 0;
        end;
        17: L.Passes[1].Ranges[0].Start := 0;
      end;
      ExpectLayoutRejected(G, R, L, 'forged layout ' + IntToStr(I));
    end;
  finally G.Free; end;
end;

procedure CheckNegotiationLayouts(const AGraph: TGraph;
  const AOptions: TGraphNegotiationOptions;
  const AReport: TGraphNegotiationReport; const AContext: String);
var
  I: Integer;
  Before: TGraphTraceSignature;
  L: TGraphTraceLayout;
begin
  Before := AReport.TranscriptHash;
  Check(CalculateGraphNegotiationTranscriptHash(AOptions, AReport) = Before,
    AContext + ' original negotiation transcript recomputes');
  for I := 0 to High(AReport.Attempts) do
  begin
    L := CheckedLayout(AGraph, AReport.Attempts[I].SolveReport,
      AContext + ' attempt ' + IntToStr(I));
    Check(L.TraceCaptured = AOptions.SolveOptions.CaptureTrace,
      AContext + ' rejected round capture follows options');
  end;
  L := CheckedLayout(AGraph, AReport.FinalReport, AContext + ' final report');
  Check(L.TraceCaptured = AOptions.SolveOptions.CaptureTrace,
    AContext + ' final round capture follows options');
  Check((AReport.TranscriptHash = Before)
    and (CalculateGraphNegotiationTranscriptHash(AOptions, AReport) = Before),
    AContext + ' layouts leave nested transcript and legacy slices unchanged');
end;

procedure TestNestedTransactions;
var
  G: TRejectingGraph;
  N: TGraphNegotiationOptions;
  NR: TGraphNegotiationReport;
  SR: TGraphSelectiveNegotiationReport;
  RestartOptions: TGraphRestartOptions;
  RR: TGraphRestartReport;
  Baseline: TGraphSolveReport;
  L: TGraphTraceLayout;
  Capture, Budget, Strategy, I: Integer;
  Before: TGraphTraceSignature;
begin
  for Capture := 0 to 1 do
  begin
    N := DefaultGraphNegotiationOptions;
    N.SolveOptions.CaptureTrace := Capture = 1;
    for Budget := 0 to 1 do
    begin
      N.MaxPassBacktracks := Budget;
      G := NewGraph(2);
      try
        G.RejectPass := 0;
        Check(not G.TrySolveNegotiated(N, NR), 'negotiated early hook rejection fails');
        Check(Length(NR.Attempts) = Budget,
          'bounded negotiation retains the expected rejected late round');
        CheckNegotiationLayouts(G, N, NR,
          'negotiation ' + IntToStr(Capture) + '/' + IntToStr(Budget));
      finally G.Free; end;

      G := NewGraph(3);
      try
        Check(G.TrySolve(N.SolveOptions, Baseline), 'selective negotiation baseline');
        G.RejectPass := 1;
        Check(not G.TryRegenerateNegotiatedFrom('second', N, SR),
          'selective negotiation rejects earlier active pass');
        Before := SR.TranscriptHash;
        Check(CalculateGraphSelectiveNegotiationTranscriptHash(N, SR) = Before,
          'original selective transcript recomputes');
        Check(Length(SR.Search.Attempts) = Budget,
          'selective rejected round is actually retained when authorized');
        CheckNegotiationLayouts(G, N, SR.Search,
          'selective negotiation ' + IntToStr(Capture) + '/' + IntToStr(Budget));
        Check((SR.TranscriptHash = Before)
          and (CalculateGraphSelectiveNegotiationTranscriptHash(N, SR) = Before),
          'derived layouts leave selective scope digest unchanged');
        Check(G.PassGraph[0].Entry[0, 0, 0].Value = 'A',
          'selective nested failure preserves reused provider');
      finally G.Free; end;
    end;

    N.MaxPassBacktracks := 0;
    RestartOptions := DefaultGraphRestartOptions;
    RestartOptions.MaxRestarts := 2;
    for Strategy := 0 to 1 do
    begin
      G := NewGraph(2);
      try
        G.RejectPass := 0;
        if Strategy = 0 then
          Check(not G.TrySolveRestarted(N.SolveOptions, RestartOptions, RR),
            'one-way restart policy stops on commit rejection')
        else
          Check(not G.TrySolveNegotiatedRestarted(N, RestartOptions, RR),
            'negotiated restart policy stops on terminal pass budget');
        Check((Length(RR.Attempts) = 1) and (RR.Restarts = 0),
          'declared restart allowance does not retry semantic rejection');
        Before := RR.TranscriptHash;
        for I := 0 to High(RR.Attempts) do
        begin
          L := CheckedLayout(G, RR.Attempts[I].SolveReport,
            'restart attempt ' + IntToStr(Capture) + '/' + IntToStr(Strategy));
          Check(L.TraceCaptured = (Capture = 1), 'restart attempt retains capture setting');
          if Strategy = 1 then
            CheckNegotiationLayouts(G, N, RR.Attempts[I].NegotiationReport,
              'restart nested negotiation');
        end;
        L := CheckedLayout(G, RR.FinalReport, 'restart final report');
        Check(L.TraceCaptured = (Capture = 1), 'restart final capture setting');
        if Strategy = 0 then
          Check(CalculateGraphRestartTranscriptHash(N.SolveOptions,
            RestartOptions, RR) = Before, 'one-way restart digest unchanged')
        else
          Check(CalculateGraphRestartTranscriptHash(N,
            RestartOptions, RR) = Before, 'negotiated restart digest unchanged');
        Check(RR.TranscriptHash = Before, 'stored restart transcript unchanged');
      finally G.Free; end;
    end;
  end;
end;

procedure TestRehashedReports;
var
  G: TRejectingGraph;
  Source, R: TGraphSolveReport;
  L: TGraphTraceLayout;
  I: Integer;
begin
  G := NewGraph(2);
  try
    G.RejectPass := 0; G.TrySolve(CaptureOptions, Source);
    L := CheckedLayout(G, Source, 'forged report baseline');
    for I := 0 to 33 do
    begin
      R := CloneReport(Source);
      case I of
        0: R.Trace[4].CauseKind := gtckAdjacency;
        1: R.Trace[4].Kind := gtekDecision;
        2: R.Trace[4].CauseEventId := -1;
        3: R.Trace[4].CauseEventId := 3;
        4: R.Trace[4].EntryIndex := 1;
        5: R.Trace[4].PassIndex := 1;
        6: R.Trace[5].CauseEventId := -1;
        7: R.Trace[5].PassIndex := 1;
        8: R.Trace[6].CauseEventId := 4;
        9: R.Trace[6].Kind := gtekPipelineCommit;
        10: R.Status := gssBacktrackLimit;
        11: R.FailedPassIndex := 1;
        12: R.Contradiction.Kind := gckEntryDomain;
        13: R.Contradiction.PassIndex := 1;
        14: R.Contradiction.EntryIndex := -1;
        15: R.Contradiction.NeighborIndex := 0;
        16: R.Contradiction.HasDirection := True;
        17: R.Contradiction.DependencyPassIndex := 0;
        18: R.Contradiction.ConstraintIndex := 0;
        19: R.Passes[0].Executed := False;
        20: R.Passes[0].Disposition := gpdReused;
        21: R.ExecutionOrder[0] := 1;
        22: R.Passes[0].ExecutionOrdinal := 42;
        23: R.Trace[0].Kind := gtekPassSkipped;
        24: R.Trace[1].Kind := gtekPassBegin;
        25: R.Trace[3].Kind := gtekPassFailed;
        26: begin
          SetLength(R.Trace, 9);
          R.Trace[6] := Source.Trace[4]; R.Trace[6].EventId := 6;
          R.Trace[7] := Source.Trace[5]; R.Trace[7].EventId := 7;
          R.Trace[7].CauseEventId := 6;
          R.Trace[8] := Source.Trace[6]; R.Trace[8].EventId := 8;
          R.Trace[8].CauseEventId := 7;
          Inc(R.Passes[0].TraceCount, 2);
        end;
        27: R.Trace[4].DomainCountBefore := 1;
        28: R.Passes[0].TraceStart := 1;
        29: R.Passes[0].TraceCount := 2;
        30: R.Passes[0].TraceCount := High(Integer);
        31: SetLength(R.Passes, 1);
        32: R.Contradiction.Direction := gdEast;
        33: R.Trace[4].HasDirection := True;
      end;
      R.TraceHash := CalculateGraphTraceHash(R);
      ExpectReportRejected(G, R, 'rehashed report mutation ' + IntToStr(I));
      ExpectLayoutRejected(G, R, L, 'old view over forged report ' + IntToStr(I));
    end;
    R := CloneReport(Source); R.TraceHash := R.TraceHash xor 1;
    ExpectReportRejected(G, R, 'unchanged events with bad hash');
  finally G.Free; end;
end;

procedure TestOrdinarySummaryBinding;
const
  ExpectedKinds: array[0..7] of TGraphContradictionKind =
    (gckInvalidLock, gckAdjacency, gckAdjacency, gckRequiredSupport,
     gckPassDependency, gckPreviousPass, gckConnectivity, gckEntryDomain);
var
  G: TRejectingGraph;
  Source, R: TGraphSolveReport;
  O: TGraphSolveOptions;
  L: TGraphTraceLayout;
  RootPosition, TerminalPosition: TGraphPosition;
  Fixture, Field: Integer;
begin
  for Fixture := 0 to 7 do
  begin
    O := CaptureOptions;
    case Fixture of
      0, 4, 5: G := NewGraph(2);
      1, 6: G := NewGraph(1, 2);
      2: begin
        G := TRejectingGraph.Create; G.RejectPass := -1;
        G.Seed := 0; G.Reshape(3, 1, 1);
      end;
    else G := NewGraph(1);
    end;
    try
      case Fixture of
        0: G.PassGraph[0].Entry[0, 0, 0].Value := 'unknown-lock';
        1: begin
          G.Rules['A'].NewRule([gdEast, gdWest], 'A');
          G.AddValue('B').NewRule([gdEast, gdWest], 'B');
          G.Entry[0, 0, 0].Value := 'A'; G.Entry[1, 0, 0].Value := 'B';
        end;
        2: begin
          G.AddValue('A').NewRule([gdEast, gdWest], 'B');
          G.AddValue('C').NewRule([gdEast, gdWest], 'C');
          O.MaxBacktracks := 0;
        end;
        3: G.Rules['A'].NewRule([gdNorth], 'A', True);
        4: G.Rules['B'].RequireFromPass('first', 'missing');
        5: G.Rules['B'].RequirePrevious('missing');
        6: begin
          RootPosition := Default(TGraphPosition);
          TerminalPosition := RootPosition; TerminalPosition.X := 1;
          G.RequireConnectivity(MakeGraphConnectivityConstraint('isolated',
            RootPosition, [TerminalPosition],
            [MakeGraphConnectivityValue('A', [])]));
        end;
        7: G.SetAllowedValues(0, 0, 0, []);
      end;
      Check(not G.TrySolve(O, Source), 'ordinary summary fixture fails ' + IntToStr(Fixture));
      Check(Source.Contradiction.Kind = ExpectedKinds[Fixture],
        'ordinary fixture has intended cause ' + IntToStr(Fixture));
      if Fixture = 2 then Check(Source.Status = gssBacktrackLimit,
        'ordinary summary fixture includes local backtrack exhaustion');
      L := CheckedLayout(G, Source, 'ordinary summary baseline ' + IntToStr(Fixture));
      for Field := 0 to 9 do
      begin
        R := CloneReport(Source);
        case Field of
          0: R.Contradiction.Kind := gckNone;
          1: R.Contradiction.PassIndex := -1;
          2: if R.Contradiction.EntryIndex = -1 then R.Contradiction.EntryIndex := 0
             else R.Contradiction.EntryIndex := -1;
          3: if R.Contradiction.NeighborIndex = -1 then R.Contradiction.NeighborIndex := 0
             else R.Contradiction.NeighborIndex := -1;
          4: R.Contradiction.HasDirection := not R.Contradiction.HasDirection;
          5: if R.Contradiction.Direction = gdEast then R.Contradiction.Direction := gdWest
             else R.Contradiction.Direction := gdEast;
          6: if R.Contradiction.DependencyPassIndex = -1 then R.Contradiction.DependencyPassIndex := 0
             else R.Contradiction.DependencyPassIndex := -1;
          7: if R.Contradiction.ConstraintIndex = -1 then R.Contradiction.ConstraintIndex := 0
             else R.Contradiction.ConstraintIndex := -1;
          8: R.FailedPassIndex := -1;
          9: R.Status := gssSolved;
        end;
        R.TraceHash := CalculateGraphTraceHash(R);
        ExpectReportRejected(G, R, 'ordinary summary field ' +
          IntToStr(Fixture) + '/' + IntToStr(Field));
        ExpectLayoutRejected(G, R, L, 'ordinary old view field ' +
          IntToStr(Fixture) + '/' + IntToStr(Field));
      end;
    finally G.Free; end;
  end;

  G := NewGraph(2);
  try
    Check(G.TrySolve(CaptureOptions, Source), 'canonical solved-summary baseline');
    L := CheckedLayout(G, Source, 'solved summary baseline');
    for Field := 0 to 8 do
    begin
      R := CloneReport(Source);
      case Field of
        0: R.Contradiction.PassIndex := 0;
        1: R.Contradiction.EntryIndex := 0;
        2: R.Contradiction.NeighborIndex := 0;
        3: R.Contradiction.HasDirection := True;
        4: R.Contradiction.Direction := gdEast;
        5: R.Contradiction.DependencyPassIndex := 0;
        6: R.Contradiction.ConstraintIndex := 0;
        7: R.FailedPassIndex := 0;
        8: R.Contradiction.Kind := gckAdjacency;
      end;
      R.TraceHash := CalculateGraphTraceHash(R);
      ExpectReportRejected(G, R, 'solved noncanonical summary field ' + IntToStr(Field));
      ExpectLayoutRejected(G, R, L, 'solved old view field ' + IntToStr(Field));
    end;
  finally G.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestMalformedBrowserFields;
var
  G: TRejectingGraph;
  Source, R: TGraphSolveReport;
  Original, L: TGraphTraceLayout;
  X, I, J: Integer;
  B: Boolean;
begin
  G := NewGraph(2);
  try
    G.RejectPass := 0; G.TrySolve(CaptureOptions, Source);
    Original := CheckedLayout(G, Source, 'browser malformed baseline');
    for I := 0 to 6 do
    begin
      case I of
        0: asm X = NaN; end;
        1: asm X = Infinity; end;
        2: asm X = -Infinity; end;
        3: asm X = 0.5; end;
        4: asm X = undefined; end;
        5: asm X = "0"; end;
        6: asm X = 2147483648; end;
      end;
      for J := 0 to 7 do
      begin
        L := CopyGraphTraceLayout(Original);
        case J of
          0: L.Version := X;
          1: L.EventCount := X;
          2: L.TerminalEventIndex := X;
          3: L.Passes[0].EventCount := X;
          4: L.Passes[0].Ranges[0].Start := X;
          5: L.Passes[0].Ranges[0].Count := X;
          6: L.Passes[0].Ranges[1].Start := X;
          7: L.Passes[1].Ranges[0].Count := X;
        end;
        ExpectLayoutRejected(G, Source, L, 'browser view number ' + IntToStr(I) + '/' + IntToStr(J));
      end;
      for J := 0 to 21 do
      begin
        R := CloneReport(Source);
        case J of
          0: R.Trace[4].CauseEventId := X;
          1: R.Trace[4].PassIndex := X;
          2: R.Trace[4].EntryIndex := X;
          3: R.Trace[4].ValueIndex := X;
          4: R.Trace[4].NeighborIndex := X;
          5: R.Trace[4].DependencyPassIndex := X;
          6: R.Trace[4].DecisionDepth := X;
          7: R.Trace[4].DomainCountBefore := X;
          8: R.Trace[4].DomainCountAfter := X;
          9: R.Trace[4].ConstraintIndex := X;
          10: R.Trace[4].Kind := TGraphTraceEventKind(X);
          11: R.Trace[4].CauseKind := TGraphTraceCauseKind(X);
          12: R.Trace[4].Direction := TGraphDirection(X);
          13: R.Passes[0].TraceStart := X;
          14: R.Passes[0].TraceCount := X;
          15: R.Passes[0].ExecutionOrdinal := X;
          16: R.ExecutionOrder[0] := X;
          17: R.FailedPassIndex := X;
          18: R.Status := TGraphSolveStatus(X);
          19: R.Contradiction.PassIndex := X;
          20: R.Contradiction.EntryIndex := X;
          21: R.Passes[0].Disposition := TGraphPassDisposition(X);
        end;
        ExpectReportRejected(G, R, 'browser report number ' + IntToStr(I) + '/' + IntToStr(J));
      end;
    end;
    for I := 0 to 2 do
    begin
      case I of
        0: asm B = 1; end;
        1: asm B = "true"; end;
        2: asm B = undefined; end;
      end;
      L := CopyGraphTraceLayout(Original); L.TraceCaptured := B;
      ExpectLayoutRejected(G, Source, L, 'browser layout Boolean ' + IntToStr(I));
      for J := 0 to 3 do
      begin
        R := CloneReport(Source);
        case J of
          0: R.TraceCaptured := B;
          1: R.Passes[0].Executed := B;
          2: R.Trace[4].HasDirection := B;
          3: R.Contradiction.HasDirection := B;
        end;
        ExpectReportRejected(G, R, 'browser report Boolean ' + IntToStr(I) + '/' + IntToStr(J));
      end;
    end;
    R := CloneReport(Source);
    //Changing only length creates sparse slots without allocating billions
    //of records. The validator must reject before visiting any absent slot.
    asm R.Trace.length = 2147483648; end;
    ExpectReportRejected(G, R, 'sparse browser trace exceeds Integer length');
    ExpectLayoutRejected(G, R, Original, 'view over oversized sparse trace');
  finally G.Free; end;
end;
{$ENDIF}

begin
  try
    Check((WFC_TRACE_VERSION = 1) and (WFC_TRACE_HASH_VERSION = 1)
      and (WFC_TRACE_UTILITY_VERSION = 1) and (WFC_TRACE_LAYOUT_VERSION = 1),
      'independent layout version leaves existing contracts unchanged');
    TestActualHooks;
    TestExecutionOrderAndReuse;
    TestDisabledAndCopies;
    TestLocalSearchAndHookBoundaries;
    TestMalformedLayouts;
    TestNestedTransactions;
    TestRehashedReports;
    TestOrdinarySummaryBinding;
    {$IFDEF PAS2JS}TestMalformedBrowserFields;{$ENDIF}
  except
    on E: Exception do
    begin
      Inc(Failures);
      WriteLn('UNEXPECTED: ', E.ClassName, ': ', E.Message);
    end;
  end;
  WriteLn('TRACE_LAYOUT_GOLDENS=709F3450/460B78DC/B27D0AE0');
  WriteLn('checks=', Checks, ' failures=', Failures);
  if Failures <> 0 then Halt(1);
end.
