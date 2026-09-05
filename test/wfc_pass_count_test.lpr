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
program wfc_pass_count_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  Classes, SysUtils, wfc;

type
  TTestProcedure = procedure;

var
  GChecks, GFailures: Integer;
  GMutationRejected: Boolean;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GChecks);
  if ACondition then WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailures);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try
    ATest;
  except
    on E: Exception do
    begin
      Inc(GFailures);
      WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function FirstValue(const AGraph: TGraph; const AEntry: TGraphEntry;
  const AValid: TGraphValues): TGraphValue;
begin
  if Length(AValid) = 0 then Result := AEntry.Value
  else Result := AValid[0];
end;

function Term(const X, Y, Z: Integer; const AValue: TGraphValue):
  TGraphPassMatchTerm;
begin
  Result := MakeGraphPassMatchTerm(MakeGraphOffset(X, Y, Z), AValue);
end;

function NewFixture(const W, H, D: Integer; const AWrap: Boolean;
  const AFillSource: Boolean = True): TGraph;
var X, Y, Z: Integer;
begin
  Result := TGraph.Create.Reshape(W, H, D);
  Result.WrapNeighbors := AWrap;
  Result.CurrentPass := 'source';
  Result.PassMode := gpmOverlay;
  Result.AddValue('A');
  Result.AddValue('B');
  Result.SelectionCallback := FirstValue;
  if AFillSource then
    for Z := 0 to D - 1 do
      for Y := 0 to H - 1 do
        for X := 0 to W - 1 do Result.Entry[X, Y, Z].Value := 'B';
  Result.SwitchToPass('consumer');
  Result.PassMode := gpmOverlay;
  Result.ClearDependencies;
  Result.AddValue('choice');
  Result.AddValue('none');
  Result.SelectionCallback := FirstValue;
  for Z := 0 to D - 1 do
    for Y := 0 to H - 1 do
      for X := 0 to W - 1 do Result.Entry[X, Y, Z].Value := 'none';
end;

function Snapshot(const AGraph: TGraph): String;
var P, X, Y, Z: Integer; E: TGraphEntry;
begin
  Result := '';
  for P := 0 to AGraph.TotalPassCount - 1 do
    for Z := 0 to Integer(AGraph.Dimension.Depth) - 1 do
      for Y := 0 to Integer(AGraph.Dimension.Height) - 1 do
        for X := 0 to Integer(AGraph.Dimension.Width) - 1 do
        begin
          E := AGraph.PassGraph[P].Entry[X, Y, Z];
          Result := Result + IntToStr(P) + ':' + IntToStr(Length(E.Value)) +
            ':' + E.Value + ':' + IntToStr(Ord(E.Generated)) + ';';
        end;
end;

function BothEngines(const AGraph: TGraph; const AExpected: Boolean): Boolean;
var LReport: TGraphSolveReport; LReference, LLegacy: Boolean;
begin
  LReference := AGraph.TrySolve(DefaultGraphSolveOptions, LReport);
  LLegacy := True;
  try AGraph.Run;
  except on E: Exception do LLegacy := False; end;
  Result := (LReference = AExpected) and (LLegacy = AExpected);
end;

function BitCount(const AValue: Integer): Integer;
var N: Integer;
begin
  N := AValue;
  Result := 0;
  while N > 0 do begin Inc(Result, N mod 2); N := N div 2; end;
end;

procedure TestAllFourPositionRanges;
const Positions: array[0..3] of Integer = (0, 1, 3, 4);
var
  LGraph: TGraph;
  LMode: TGraphPassCountMode;
  LMin, LMax, LMask, I, LExpectedCount: Integer;
  LGood: Boolean;
begin
  for LMode := Low(TGraphPassCountMode) to High(TGraphPassCountMode) do
    for LMin := 0 to 4 do
      for LMax := LMin to 4 do
      begin
        LGood := True;
        for LMask := 0 to 15 do
        begin
          LGraph := NewFixture(5, 1, 1, False);
          try
            for I := 0 to 3 do
              if (LMask and (1 shl I)) <> 0 then
                LGraph.PassGraph[0].Entry[Positions[I], 0, 0].Value := 'A';
            LGraph.Entry[2, 0, 0].Value := 'choice';
            LGraph.Rules['choice'].RequireCountFromPass('source',
              [Term(-2, 0, 0, 'A'), Term(-1, 0, 0, 'A'),
               Term(1, 0, 0, 'A'), Term(2, 0, 0, 'A')],
              LMin, LMax, LMode);
            LExpectedCount := BitCount(LMask);
            if not BothEngines(LGraph,
              (LExpectedCount >= LMin) and (LExpectedCount <= LMax)) then
            begin
              LGood := False;
              WriteLn('    mismatch mask=', LMask, ' expected count=', LExpectedCount);
            end;
          finally LGraph.Free; end;
        end;
        Check(LGood, Format('all 16 provider assignments, both engines, mode %d range %d..%d',
          [Ord(LMode), LMin, LMax]));
      end;
end;

procedure TestWrappedAliases;
const Ranges: array[0..7, 0..1] of Integer =
  ((0, 0), (1, 1), (2, 2), (3, 3), (5, 5), (0, 6), (2, 6), (0, 2));
var
  LGraph: TGraph;
  LMode: TGraphPassCountMode;
  LMask, I, LCount: Integer;
  LGood: Boolean;
begin
  for LMode := Low(TGraphPassCountMode) to High(TGraphPassCountMode) do
    for LMask := 0 to 3 do
    begin
      { Four odd offsets alias provider cell 1: one allows B, three allow A.
        Two even offsets alias cell 0 and both allow A. This expected count is
        calculated from that truth table, independently of the core resolver. }
      if LMode = gpcmMatchingTerms then
      begin
        if (LMask and 2) <> 0 then LCount := 3 else LCount := 1;
        if (LMask and 1) <> 0 then Inc(LCount, 2);
      end
      else
      begin
        LCount := 1;
        if (LMask and 1) <> 0 then Inc(LCount);
      end;
      LGood := True;
      for I := 0 to High(Ranges) do
      begin
        LGraph := NewFixture(2, 1, 1, True);
        try
          if (LMask and 1) <> 0 then LGraph.PassGraph[0].Entry[0, 0, 0].Value := 'A';
          if (LMask and 2) <> 0 then LGraph.PassGraph[0].Entry[1, 0, 0].Value := 'A';
          LGraph.Entry[0, 0, 0].Value := 'choice';
          LGraph.Rules['choice'].RequireCountFromPass('source',
            [Term(3, 0, 0, 'A'), Term(2, 0, 0, 'A'), Term(1, 0, 0, 'A'),
             Term(0, 0, 0, 'A'), Term(-1, 0, 0, 'A'), Term(-3, 0, 0, 'B')],
            Ranges[I, 0], Ranges[I, 1], LMode);
          if not BothEngines(LGraph, (LCount >= Ranges[I, 0]) and
            (LCount <= Ranges[I, 1])) then LGood := False;
        finally LGraph.Free; end;
      end;
      Check(LGood, Format('wrapped aliases and mismatching-first alias, mode %d mask %d',
        [Ord(LMode), LMask]));
    end;
end;

procedure TestDimensionsAndBoundaries;
var
  LGraph: TGraph;
  LTerms: TGraphPassMatchTerms;
  LMode: TGraphPassCountMode;
  W, H, D, X, Y, Z, LCase, LCount: Integer;
  LWrap: Boolean;
begin
  LTerms := [Term(-1, 0, 0, 'A'), Term(1, 0, 0, 'A'),
    Term(0, -1, 0, 'A'), Term(0, 1, 0, 'A'),
    Term(0, 0, -1, 'A'), Term(0, 0, 1, 'A')];
  for LMode := Low(TGraphPassCountMode) to High(TGraphPassCountMode) do
    for LCase := 0 to 3 do
    begin
      W := 3; H := 3; D := 3; LWrap := False;
      case LCase of
        0: LCount := 3;
        1: begin D := 1; LCount := 2; end;
        2: begin LWrap := True; LCount := 6; end;
      else
        W := 1; H := 1; D := 1; LWrap := True;
        if LMode = gpcmMatchingTerms then LCount := 6 else LCount := 1;
      end;
      LGraph := NewFixture(W, H, D, LWrap);
      try
        for Z := 0 to D - 1 do
          for Y := 0 to H - 1 do
            for X := 0 to W - 1 do LGraph.PassGraph[0].Entry[X, Y, Z].Value := 'A';
        LGraph.Entry[0, 0, 0].Value := 'choice';
        LGraph.Rules['choice'].RequireCountFromPass('source', LTerms,
          LCount, LCount, LMode);
        Check(BothEngines(LGraph, True), Format('2D/3D boundary case %d mode %d',
          [LCase, Ord(LMode)]));
      finally LGraph.Free; end;
    end;

  for LMode := Low(TGraphPassCountMode) to High(TGraphPassCountMode) do
    for LCase := 0 to 1 do
    begin
      LGraph := NewFixture(1, 1, 1, LCase = 1);
      try
        LGraph.PassGraph[0].Entry[0, 0, 0].Value := 'A';
        LGraph.Entry[0, 0, 0].Value := 'choice';
        if LCase = 0 then LCount := 0
        else if LMode = gpcmMatchingTerms then LCount := 3 else LCount := 1;
        LGraph.Rules['choice'].RequireCountFromPass('source',
          [Term(Low(Integer), 0, 0, 'A'), Term(0, High(Integer), 0, 'A'),
           Term(0, 0, Low(Integer), 'A')], LCount, LCount, LMode);
        Check(BothEngines(LGraph, True), Format('extreme signed offsets case %d mode %d',
          [LCase, Ord(LMode)]));
      finally LGraph.Free; end;
    end;

  LGraph := TGraph.Create.Reshape(1, 1, 1);
  try
    LGraph.WrapNeighbors := False;
    LGraph.CurrentPass := 'source';
    LGraph.PassMode := gpmOverlay;
    LGraph.SwitchToPass('consumer');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('choice').RequireCountFromPass('source',
      [Term(0, 0, 0, 'A')], 0, 0, gpcmDistinctCells);
    LGraph.Entry[0, 0, 0].Value := 'choice';
    Check(BothEngines(LGraph, True), 'empty provider is a non-match and permits zero count');
  finally LGraph.Free; end;
end;

procedure TestCopiesAndClauseAlgebra;
var
  LGraph: TGraph;
  LTerms: TGraphPassMatchTerms;
  LValues: TGraphValues;
  LCase: Integer;
begin
  LGraph := NewFixture(3, 1, 1, False);
  try
    LGraph.PassGraph[0].Entry[0, 0, 0].Value := 'A';
    LGraph.Entry[1, 0, 0].Value := 'choice';
    LValues := ['A', 'A'];
    LTerms := [MakeGraphPassMatchTerm(MakeGraphOffset(-1, 0, 0), LValues),
      Term(-1, 0, 0, 'B'), Term(1, 0, 0, 'A')];
    LGraph.Rules['choice'].RequireCountFromPass('source', LTerms, 1, 1,
      gpcmMatchingTerms);
    LValues[0] := 'B';
    LTerms[0].Values[0] := 'B';
    LTerms[1].Offset.DeltaX := 1;
    LTerms[2].Values[0] := 'B';
    LTerms := nil;
    Check(BothEngines(LGraph, True), 'exact duplicate offsets union once and input arrays are detached');
  finally LGraph.Free; end;

  for LCase := 0 to 3 do
  begin
    LGraph := NewFixture(1, 1, 1, True);
    try
      LGraph.PassGraph[0].Entry[0, 0, 0].Value := 'A';
      LGraph.Entry[0, 0, 0].Value := 'choice';
      LTerms := [Term(-1, 0, 0, 'A'), Term(1, 0, 0, 'A')];
      case LCase of
        0:
          begin
            LGraph.Rules['choice'].RequireCountFromPass('source', LTerms,
              2, 2, gpcmMatchingTerms).RequireCountFromPass('source', LTerms,
              1, 1, gpcmDistinctCells);
          end;
        1:
          begin
            LGraph.Rules['choice'].RequireCountFromPass('source', LTerms,
              1, 1, gpcmMatchingTerms).RequireCountFromPass('source', LTerms,
              2, 2, gpcmMatchingTerms);
          end;
        2:
          begin
            LGraph.Rules['choice'].RequirePrevious('A')
              .RequireCountFromPass('source', [Term(0, 0, 0, 'A')],
                0, 0, gpcmMatchingTerms);
          end;
        3:
          begin
            LGraph.Rules['choice'].RequireCountFromPass('source',
              [Term(0, 0, 0, 'A')], 0, 0, gpcmDistinctCells)
              .RequireFromPass('source', 'A');
          end;
      end;
      Check(BothEngines(LGraph, LCase = 0), Format('independent AND clause algebra case %d', [LCase]));
    finally LGraph.Free; end;
  end;

  LGraph := NewFixture(1, 1, 1, True);
  try
    LGraph.PassGraph[0].Entry[0, 0, 0].Value := 'A';
    LGraph.Entry[0, 0, 0].Value := 'choice';
    LGraph.Rules['choice'].RequireCountFromPass('source',
      [MakeGraphPassMatchTerm(MakeGraphOffset(0, 0, 0), ['B', 'A'])],
      1, 1, gpcmMatchingTerms).RequireCountFromPass('source',
      [MakeGraphPassMatchTerm(MakeGraphOffset(0, 0, 0), ['A', 'B', 'A'])],
      1, 1, gpcmMatchingTerms);
    Check(BothEngines(LGraph, True), 'reordered accepted-value sets and repeated equivalent calls retain meaning');
  finally LGraph.Free; end;
end;

function InvalidMode: TGraphPassCountMode;
{$PUSH}{$R-}
var N: Integer;
begin
  N := Ord(High(TGraphPassCountMode)) + 1;
  Result := TGraphPassCountMode(N);
end;
{$POP}

procedure TestRejectedDeclarations;
var
  LGraph: TGraph;
  LTerms: TGraphPassMatchTerms;
  LMode: TGraphPassCountMode;
  LMin, LMax, LCase: Integer;
  LExpected, LActual: String;
  LGroup: TGraphRuleGroup;
  LReport: TGraphSolveReport;
begin
  for LCase := 0 to 10 do
  begin
    LGraph := NewFixture(1, 1, 1, False);
    try
      LTerms := [Term(0, 0, 0, 'A')];
      LMin := 0; LMax := 1; LMode := gpcmMatchingTerms;
      LExpected := 'range';
      case LCase of
        0: begin LTerms := nil; LExpected := 'argument'; end;
        1: begin LTerms[0].Values := nil; LExpected := 'argument'; end;
        2: begin LTerms[0].Values := ['']; LExpected := 'argument'; end;
        3: LMin := -1;
        4: LMax := -1;
        5: begin LMin := 1; LMax := 0; end;
        6: LMax := 2;
        7: begin LMin := 2; LMax := 2; end;
        8: begin LTerms := [Term(0, 0, 0, 'A'), Term(0, 0, 0, 'B')];
             LMin := 2; LMax := 2; end;
        9: LMode := InvalidMode;
        10: begin LTerms := [Term(-1, 0, 0, 'A'), Term(1, 0, 0, 'A')];
              LTerms[1].Values := nil; LExpected := 'argument'; end;
      end;
      LActual := '';
      try
        LGraph.Rules['choice'].RequireCountFromPass('source', LTerms, LMin, LMax, LMode);
      except
        on E: EArgumentException do LActual := 'argument';
        on E: ERangeError do LActual := 'range';
      end;
      Check((LActual = LExpected) and (LGraph.DependencyCount = 0),
        Format('invalid declaration %d rejects atomically with its public exception', [LCase]));
      Check(LGraph.TrySolve(DefaultGraphSolveOptions, LReport),
        Format('invalid declaration %d leaves the existing graph usable', [LCase]));
    finally LGraph.Free; end;
  end;

  LGraph := NewFixture(1, 1, 1, False);
  try
    LActual := '';
    try LGraph.Rules['choice'].RequireCountFromPass('missing',
      [Term(0, 0, 0, 'A')], 0, 1, gpcmDistinctCells);
    except on E: EArgumentException do LActual := 'argument'; end;
    Check((LActual = 'argument') and (LGraph.DependencyCount = 0), 'unknown provider adds no edge');
    LActual := '';
    try LGraph.Rules['choice'].RequireCountFromPass('consumer',
      [Term(0, 0, 0, 'choice')], 0, 1, gpcmDistinctCells);
    except on E: EInvalidOperation do LActual := 'invalid'; end;
    Check((LActual = 'invalid') and (LGraph.DependencyCount = 0), 'self provider adds no edge');
    LGraph.DependsOn('source');
    LGraph.SwitchToPass('source');
    LActual := '';
    try LGraph.Rules['A'].RequireCountFromPass('consumer',
      [Term(0, 0, 0, 'choice')], 0, 1, gpcmMatchingTerms);
    except on E: EInvalidOperation do LActual := 'invalid'; end;
    Check((LActual = 'invalid') and (LGraph.DependencyCount = 0), 'count cannot close a dependency cycle');
  finally LGraph.Free; end;

  LGroup := TGraphRuleGroup.Create('choice');
  try
    LActual := '';
    try LGroup.RequireCountFromPass('source', [Term(0, 0, 0, 'A')],
      0, 1, gpcmMatchingTerms);
    except on E: EInvalidOperation do LActual := 'invalid'; end;
    Check(LActual = 'invalid', 'unowned rule group rejects count binding');
  finally LGroup.Free; end;
end;

function MutateDuringSelection(const AGraph: TGraph; const AEntry: TGraphEntry;
  const AValid: TGraphValues): TGraphValue;
begin
  try
    AGraph.Rules['choice'].RequireCountFromPass('source',
      [Term(0, 0, 0, 'A')], 0, 1, gpcmMatchingTerms);
  except on E: EInvalidOperation do GMutationRejected := True; end;
  Result := FirstValue(AGraph, AEntry, AValid);
end;

procedure TestDependenciesAndRunningMutation;
var LGraph: TGraph; LRaised: Boolean;
begin
  LGraph := NewFixture(1, 1, 1, False);
  try
    LGraph.Rules['choice'].RequireCountFromPass('source',
      [Term(0, 0, 0, 'A')], 0, 1, gpcmMatchingTerms);
    Check((LGraph.DependencyCount = 1) and (LGraph.DependencyIndex[0] = 0),
      'a tautological count still owns its provider dependency');
    LRaised := False;
    try LGraph.RemoveDependency('source');
    except on E: EInvalidOperation do LRaised := True; end;
    Check(LRaised and (LGraph.DependencyCount = 1), 'count-protected dependency cannot be removed');
    LRaised := False;
    try LGraph.ClearDependencies;
    except on E: EInvalidOperation do LRaised := True; end;
    Check(LRaised and (LGraph.DependencyCount = 1), 'count-protected dependency cannot be cleared');
  finally LGraph.Free; end;

  LGraph := NewFixture(1, 1, 1, False);
  try
    LGraph.Entry[0, 0, 0].ClearValue;
    LGraph.SelectionCallback := MutateDuringSelection;
    GMutationRejected := False;
    LGraph.Run;
    Check(GMutationRejected and (LGraph.DependencyCount = 0),
      'running callback cannot install a count or mutate dependencies');
  finally LGraph.Free; end;
end;

procedure TestRollbackAndTrace;
var
  LGraph: TGraph;
  LBefore: String;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  I: Integer;
  LFound: Boolean;
begin
  LGraph := NewFixture(2, 1, 1, False, False);
  try
    LGraph.PassGraph[0].SetAllowedValues(0, 0, 0, ['A']);
    LGraph.PassGraph[0].SetAllowedValues(1, 0, 0, ['A']);
    LGraph.Entry[0, 0, 0].Value := 'choice';
    LGraph.Rules['choice'].RequireCountFromPass('source',
      [Term(0, 0, 0, 'A'), Term(1, 0, 0, 'A')], 0, 1, gpcmDistinctCells);
    LOptions := DefaultGraphSolveOptions;
    LOptions.CaptureTrace := True;
    LBefore := Snapshot(LGraph);
    Check(not LGraph.TrySolve(LOptions, LReport), 'upper bound rejects the freshly staged provider output');
    Check(Snapshot(LGraph) = LBefore, 'count failure rolls back every staged cell and generated flag');
    Check((LReport.FailedPassIndex = 1) and
      (LReport.Contradiction.Kind = gckPassDependency) and
      (LReport.Contradiction.DependencyPassIndex = 0),
      'count contradiction retains its provider pass cause');
    LFound := False;
    for I := 0 to High(LReport.Trace) do
      if (LReport.Trace[I].Kind = gtekInitialCandidateRemoved) and
        (LReport.Trace[I].CauseKind = gtckPassDependency) and
        (LReport.Trace[I].DependencyPassIndex = 0) and
        (LReport.Trace[I].Value = 'choice') then LFound := True;
    Check(LFound, 'candidate-removal trace names the count provider');
    Check((LReport.PipelineAlgorithmVersion = 2) and
      (WFC_PASS_COUNT_VERSION = 1), 'count capability has its own version without rewriting pipeline v2');
  finally LGraph.Free; end;
end;

procedure TestSelectiveReuse;
var LGraph: TGraph; LReport: TGraphSolveReport; LBefore: String;
begin
  LGraph := NewFixture(1, 1, 1, False);
  try
    LGraph.PassGraph[0].Entry[0, 0, 0].Value := 'A';
    LGraph.Entry[0, 0, 0].Value := 'choice';
    LGraph.Rules['choice'].RequireCountFromPass('source', [Term(0, 0, 0, 'A')],
      1, 1, gpcmDistinctCells);
    LGraph.SwitchToPass('side');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('S');
    LGraph.SwitchToPass('decoration');
    LGraph.PassMode := gpmOverlay;
    LGraph.ClearDependencies;
    LGraph.AddValue('D').RequireFromPass('consumer', 'choice');
    Check(LGraph.TrySolve(DefaultGraphSolveOptions, LReport), 'counted branch and independent side branch initially solve');
    LBefore := Snapshot(LGraph);
    Check(LGraph.TryRegenerateFrom('consumer', DefaultGraphSolveOptions, LReport),
      'selective consumer regeneration can read a reused count provider');
    Check((Snapshot(LGraph) = LBefore) and
      (LReport.Passes[0].Disposition = gpdReused) and
      (LReport.Passes[2].Disposition = gpdReused) and LReport.Passes[3].Executed,
      'count regeneration preserves clean provider/side and regenerates descendants');
    LGraph.PassGraph[0].Entry[0, 0, 0].Value := 'B';
    LBefore := Snapshot(LGraph);
    Check(not LGraph.TryRegenerateFrom('consumer', DefaultGraphSolveOptions, LReport),
      'selective count evaluates the current reused provider value');
    Check(Snapshot(LGraph) = LBefore, 'failed selective count preserves prior branch output and clean state');
    LGraph.PassGraph[0].Entry[0, 0, 0].Value := 'A';
    Check(LGraph.TryRegenerateFrom('source', DefaultGraphSolveOptions, LReport) and
      LReport.Passes[1].Executed and LReport.Passes[3].Executed and
      (LReport.Passes[2].Disposition = gpdReused),
      'inferred count edge participates in exact descendant closure');
  finally LGraph.Free; end;
end;

procedure TestNegotiatedProviderRecovery;
var
  LGraph: TGraph;
  LReport: TGraphSolveReport;
  LNegotiated: TGraphNegotiationReport;
  LOptions: TGraphNegotiationOptions;
  LSeed: Integer;
  LFound, LRecovered: Boolean;
begin
  LFound := False;
  LRecovered := False;
  for LSeed := 0 to 31 do
  begin
    LGraph := NewFixture(1, 1, 1, False, False);
    try
      LGraph.Seed := LSeed;
      LGraph.Entry[0, 0, 0].Value := 'choice';
      LGraph.Rules['choice'].RequireCountFromPass('source', [Term(0, 0, 0, 'A')],
        1, 1, gpcmDistinctCells);
      if not LGraph.TrySolve(DefaultGraphSolveOptions, LReport) then
      begin
        LFound := True;
        LOptions := DefaultGraphNegotiationOptions;
        LOptions.MaxPassBacktracks := 1;
        LRecovered := LGraph.TrySolveNegotiated(LOptions, LNegotiated) and
          (LNegotiated.PassBacktracks = 1) and
          (LGraph.PassGraph[0].Entry[0, 0, 0].Value = 'A');
      end;
    finally LGraph.Free; end;
    if LFound then Break;
  end;
  Check(LFound, 'finite seed fixture reaches a one-way count contradiction');
  Check(LRecovered, 'bounded negotiation reopens the provider after a count contradiction');
end;

begin
  WriteLn('WFC pass count-range conformance suite');
  RunTest('exhaustive four-position interval truth tables', @TestAllFourPositionRanges);
  RunTest('wrapped term versus distinct-cell counts', @TestWrappedAliases);
  RunTest('dimensions, boundaries, empty providers, and extreme offsets', @TestDimensionsAndBoundaries);
  RunTest('detached terms and conjunctive count algebra', @TestCopiesAndClauseAlgebra);
  RunTest('atomic invalid declarations', @TestRejectedDeclarations);
  RunTest('protected dependencies and running mutation', @TestDependenciesAndRunningMutation);
  RunTest('staged rollback and causal trace', @TestRollbackAndTrace);
  RunTest('selective provider reuse and descendant closure', @TestSelectiveReuse);
  RunTest('negotiated provider recovery', @TestNegotiatedProviderRecovery);
  WriteLn(Format('%d checks, %d failures', [GChecks, GFailures]));
  if GFailures <> 0 then Halt(1);
end.
