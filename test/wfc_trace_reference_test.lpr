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
program wfc_trace_reference_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc_solver_reference;

var
  GChecks: Integer = 0;
  GFailures: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GChecks);
  if ACondition then
    WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailures);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

procedure InitializeModel(const ACellCount, AValueCount: Integer;
  out AModel: TReferenceModel);
var
  I: Integer;
begin
  AModel := Default(TReferenceModel);
  AModel.CellCount := ACellCount;
  AModel.ValueCount := AValueCount;
  SetLength(AModel.Neighbors,
    ACellCount * WFC_REFERENCE_DIRECTION_COUNT);
  for I := 0 to High(AModel.Neighbors) do
    AModel.Neighbors[I] := -1;
  SetLength(AModel.Compatibility,
    WFC_REFERENCE_DIRECTION_COUNT * AValueCount * AValueCount);
  SetLength(AModel.RequiredValues, AValueCount);
  SetLength(AModel.RequiredSupport, Length(AModel.Compatibility));
  SetLength(AModel.InitialAllowed, ACellCount * AValueCount);
  SetLength(AModel.InitialFailureKinds, ACellCount);
  SetLength(AModel.LockedValues, ACellCount);
  SetLength(AModel.CellOrder, ACellCount);
  for I := 0 to Pred(ACellCount) do
  begin
    AModel.InitialFailureKinds[I] := rckEmptyDomain;
    AModel.LockedValues[I] := -1;
    AModel.CellOrder[I] := I;
  end;
end;

procedure AllowAll(var AModel: TReferenceModel);
var
  I: Integer;
begin
  for I := 0 to High(AModel.InitialAllowed) do
    AModel.InitialAllowed[I] := 1;
end;

function RelationIndex(const AValueCount, ADirection, ACurrentValue,
  ANeighborValue: Integer): Integer;
begin
  Result := ((ADirection * AValueCount + ACurrentValue)
    * AValueCount) + ANeighborValue;
end;

procedure AllowPair(var AModel: TReferenceModel; const ADirection,
  ACurrentValue, ANeighborValue: Integer);
begin
  AModel.Compatibility[RelationIndex(AModel.ValueCount, ADirection,
    ACurrentValue, ANeighborValue)] := 1;
end;

function SameAssignment(const ALeft,
  ARight: TReferenceIntegerArray): Boolean;
var
  I: Integer;
begin
  if Length(ALeft) <> Length(ARight) then
    Exit(False);
  for I := 0 to High(ALeft) do
    if ALeft[I] <> ARight[I] then
      Exit(False);
  Result := True;
end;

function SameReportCore(const ALeft,
  ARight: TReferenceSolveReport): Boolean;
begin
  Result := (ALeft.Status = ARight.Status)
    and (ALeft.Decisions = ARight.Decisions)
    and (ALeft.Propagations = ARight.Propagations)
    and (ALeft.Contradictions = ARight.Contradictions)
    and (ALeft.Backtracks = ARight.Backtracks)
    and (ALeft.Contradiction.Kind = ARight.Contradiction.Kind)
    and (ALeft.Contradiction.EntryIndex =
      ARight.Contradiction.EntryIndex)
    and (ALeft.Contradiction.NeighborIndex =
      ARight.Contradiction.NeighborIndex)
    and (ALeft.Contradiction.Direction =
      ARight.Contradiction.Direction);
end;

function SameTrace(const ALeft,
  ARight: TReferenceTraceEvents): Boolean;
var
  I: Integer;
begin
  if Length(ALeft) <> Length(ARight) then
    Exit(False);
  for I := 0 to High(ALeft) do
    if (ALeft[I].EventId <> ARight[I].EventId)
      or (ALeft[I].CauseEventId <> ARight[I].CauseEventId)
      or (ALeft[I].Kind <> ARight[I].Kind)
      or (ALeft[I].CauseKind <> ARight[I].CauseKind)
      or (ALeft[I].EntryIndex <> ARight[I].EntryIndex)
      or (ALeft[I].ValueIndex <> ARight[I].ValueIndex)
      or (ALeft[I].NeighborIndex <> ARight[I].NeighborIndex)
      or (ALeft[I].Direction <> ARight[I].Direction)
      or (ALeft[I].DecisionDepth <> ARight[I].DecisionDepth)
      or (ALeft[I].DomainCountBefore <>
        ARight[I].DomainCountBefore)
      or (ALeft[I].DomainCountAfter <>
        ARight[I].DomainCountAfter) then
      Exit(False);
  Result := True;
end;

procedure TestNoTraceCompatibility;
var
  LAssignmentExplicit: TReferenceIntegerArray;
  LAssignmentLegacy: TReferenceIntegerArray;
  LExplicit: TReferenceSolveReport;
  LLegacy: TReferenceSolveReport;
  LModel: TReferenceModel;
begin
  WriteLn('[TEST] disabled trace compatibility');
  InitializeModel(1, 2, LModel);
  AllowAll(LModel);
  Check(SolveReferenceModel(LModel, 0, nil,
      LAssignmentLegacy, LLegacy),
    'the existing overload still solves');
  Check(SolveReferenceModel(LModel, 0, False, nil,
      LAssignmentExplicit, LExplicit),
    'the explicit disabled-trace overload still solves');
  Check(SameAssignment(LAssignmentLegacy, LAssignmentExplicit)
      and SameReportCore(LLegacy, LExplicit),
    'disabled tracing preserves assignments and counters exactly');
  Check((Length(LLegacy.Trace) = 0)
      and (Length(LExplicit.Trace) = 0),
    'both disabled trace paths return an empty event array');
end;

procedure TestPropagationTrace;
var
  LAssignment: TReferenceIntegerArray;
  LModel: TReferenceModel;
  LReport: TReferenceSolveReport;
begin
  WriteLn('[TEST] initial and adjacency trace');
  InitializeModel(2, 2, LModel);
  AllowAll(LModel);
  LModel.InitialAllowed[3] := 0;
  LModel.Neighbors[0] := 1;
  AllowPair(LModel, 0, 0, 0);
  AllowPair(LModel, 0, 1, 1);

  Check(SolveReferenceModel(LModel, 0, True, nil,
      LAssignment, LReport),
    'the traced equality fixture solves by propagation');
  Check((Length(LAssignment) = 2)
      and (LAssignment[0] = 0) and (LAssignment[1] = 0),
    'propagation commits the supported pair');
  Check(Length(LReport.Trace) = 3,
    'the fixture emits one initial removal, one propagation, and solved');
  if Length(LReport.Trace) = 3 then
  begin
    Check((LReport.Trace[0].EventId = 0)
        and (LReport.Trace[0].Kind = rtekInitialCandidateRemoved)
        and (LReport.Trace[0].CauseKind = rtckInitialDomain)
        and (LReport.Trace[0].CauseEventId = -1)
        and (LReport.Trace[0].EntryIndex = 1)
        and (LReport.Trace[0].ValueIndex = 1)
        and (LReport.Trace[0].DomainCountBefore = 2)
        and (LReport.Trace[0].DomainCountAfter = 1),
      'initial filtering records its exact candidate and domain transition');
    Check((LReport.Trace[1].EventId = 1)
        and (LReport.Trace[1].Kind = rtekCandidateRemoved)
        and (LReport.Trace[1].CauseKind = rtckAdjacency)
        and (LReport.Trace[1].CauseEventId = 0)
        and (LReport.Trace[1].EntryIndex = 0)
        and (LReport.Trace[1].ValueIndex = 1)
        and (LReport.Trace[1].NeighborIndex = 1)
        and (LReport.Trace[1].Direction = 0)
        and (LReport.Trace[1].DomainCountBefore = 2)
        and (LReport.Trace[1].DomainCountAfter = 1),
      'adjacency removal links to the latest neighbor change');
    Check((LReport.Trace[2].Kind = rtekSolved)
        and (LReport.Trace[2].CauseEventId = 1),
      'the solved event links to the latest domain change');
  end;
end;

procedure TestLockAndDecisionTrace;
var
  LAssignment: TReferenceIntegerArray;
  LModel: TReferenceModel;
  LReport: TReferenceSolveReport;
begin
  WriteLn('[TEST] lock and decision trace');
  InitializeModel(1, 2, LModel);
  AllowAll(LModel);
  LModel.LockedValues[0] := 0;
  Check(SolveReferenceModel(LModel, 0, True, nil,
      LAssignment, LReport),
    'the traced lock fixture solves');
  Check((Length(LReport.Trace) = 2)
      and (LReport.Trace[0].Kind = rtekInitialCandidateRemoved)
      and (LReport.Trace[0].CauseKind = rtckLock)
      and (LReport.Trace[0].ValueIndex = 1),
    'lock filtering has a distinct causal kind');

  InitializeModel(1, 2, LModel);
  AllowAll(LModel);
  Check(SolveReferenceModel(LModel, 0, True, nil,
      LAssignment, LReport),
    'the traced decision fixture solves');
  Check((Length(LReport.Trace) = 3)
      and (LReport.Trace[0].Kind = rtekDecision)
      and (LReport.Trace[0].ValueIndex = 0)
      and (LReport.Trace[0].DecisionDepth = 0)
      and (LReport.Trace[1].Kind = rtekCandidateRemoved)
      and (LReport.Trace[1].CauseKind = rtckDecision)
      and (LReport.Trace[1].CauseEventId = 0)
      and (LReport.Trace[2].Kind = rtekSolved)
      and (LReport.Trace[2].CauseEventId = 1),
    'decision pruning and completion retain their causal chain');
end;

procedure TestRequiredSupportTrace;
var
  LAssignment: TReferenceIntegerArray;
  LModel: TReferenceModel;
  LReport: TReferenceSolveReport;
begin
  WriteLn('[TEST] required-support trace');
  InitializeModel(2, 2, LModel);
  AllowAll(LModel);
  LModel.InitialAllowed[2] := 0;
  LModel.Neighbors[0] := 1;
  LModel.RequiredValues[0] := 1;
  LModel.RequiredSupport[RelationIndex(2, 0, 0, 0)] := 1;
  AllowPair(LModel, 0, 0, 0);
  AllowPair(LModel, 0, 0, 1);
  AllowPair(LModel, 0, 1, 0);
  AllowPair(LModel, 0, 1, 1);

  Check(SolveReferenceModel(LModel, 0, True, nil,
      LAssignment, LReport),
    'the orphan-required fixture retains its fallback');
  Check((Length(LReport.Trace) >= 3)
      and (LReport.Trace[0].Kind = rtekInitialCandidateRemoved)
      and (LReport.Trace[1].Kind = rtekCandidateRemoved)
      and (LReport.Trace[1].CauseKind = rtckRequiredSupport)
      and (LReport.Trace[1].CauseEventId = 0)
      and (LReport.Trace[1].EntryIndex = 0)
      and (LReport.Trace[1].ValueIndex = 0),
    'required-support pruning links to the latest supporting-neighbor change');
end;

procedure ConfigureEscapeRing(out AModel: TReferenceModel);
var
  LCell: Integer;
begin
  InitializeModel(3, 3, AModel);
  AllowAll(AModel);
  for LCell := 0 to 2 do
  begin
    AModel.Neighbors[(LCell * WFC_REFERENCE_DIRECTION_COUNT) + 0] :=
      (LCell + 1) mod 3;
    AModel.Neighbors[(LCell * WFC_REFERENCE_DIRECTION_COUNT) + 1] :=
      (LCell + 2) mod 3;
  end;
  AllowPair(AModel, 0, 0, 1);
  AllowPair(AModel, 0, 1, 0);
  AllowPair(AModel, 0, 2, 2);
  AllowPair(AModel, 1, 0, 1);
  AllowPair(AModel, 1, 1, 0);
  AllowPair(AModel, 1, 2, 2);
end;

procedure TestBacktrackTraceAndReplay;
var
  I: Integer;
  LAssignment: TReferenceIntegerArray;
  LAssignmentTwin: TReferenceIntegerArray;
  LBacktracks: Integer;
  LContradictions: Integer;
  LDecisionRemovals: Integer;
  LModel: TReferenceModel;
  LReport: TReferenceSolveReport;
  LReportTwin: TReferenceSolveReport;
  LRestores: Integer;
  LRetryDecisions: Integer;
  LSolved: Integer;
begin
  WriteLn('[TEST] contradiction, backtrack, restore, and replay trace');
  ConfigureEscapeRing(LModel);
  Check(SolveReferenceModel(LModel, 2, True, nil,
      LAssignment, LReport),
    'bounded traced recovery reaches the viable ring branch');
  Check((Length(LAssignment) = 3)
      and (LAssignment[0] = 2) and (LAssignment[1] = 2)
      and (LAssignment[2] = 2),
    'the recovered assignment is the self-compatible branch');

  LBacktracks := 0;
  LContradictions := 0;
  LDecisionRemovals := 0;
  LRestores := 0;
  LRetryDecisions := 0;
  LSolved := 0;
  for I := 0 to High(LReport.Trace) do
  begin
    Check(LReport.Trace[I].EventId = I,
      Format('trace event %d has its stable append-order id', [I]));
    Check((LReport.Trace[I].CauseEventId = -1)
        or ((LReport.Trace[I].CauseEventId >= 0)
          and (LReport.Trace[I].CauseEventId < I)),
      Format('trace event %d has only a prior causal parent', [I]));
    case LReport.Trace[I].Kind of
      rtekDecision:
        if LReport.Trace[I].CauseKind = rtckBacktrack then
        begin
          Inc(LRetryDecisions);
          Check((LReport.Trace[I].CauseEventId >= 0)
              and (LReport.Trace[
                LReport.Trace[I].CauseEventId].Kind = rtekBacktrack),
            'retry decision links to the backtrack that enabled it');
        end;
      rtekCandidateRemoved:
        if LReport.Trace[I].CauseKind = rtckDecision then
        begin
          Inc(LDecisionRemovals);
          Check(LReport.Trace[LReport.Trace[I].CauseEventId].Kind
              = rtekDecision,
            'decision removal links directly to its decision event');
        end;
      rtekContradiction:
        begin
          Inc(LContradictions);
          Check((LReport.Trace[I].CauseEventId >= 0)
              and (LReport.Trace[
                LReport.Trace[I].CauseEventId].EntryIndex
                = LReport.Trace[I].EntryIndex),
            'contradiction links to the latest change of its empty entry');
        end;
      rtekBacktrack:
        begin
          Inc(LBacktracks);
          Check((LReport.Trace[I].CauseEventId >= 0)
              and (LReport.Trace[
                LReport.Trace[I].CauseEventId].Kind
                = rtekContradiction),
            'backtrack links directly to the triggering contradiction');
        end;
      rtekCandidateRestored:
        begin
          Inc(LRestores);
          Check((LReport.Trace[I].CauseEventId >= 0)
              and (LReport.Trace[
                LReport.Trace[I].CauseEventId].Kind
                = rtekBacktrack),
            'restoration links directly to its backtrack event');
        end;
      rtekSolved:
        begin
          Inc(LSolved);
          Check(LReport.Trace[I].CauseEventId >= 0,
            'solved links to the latest retained domain change');
        end;
    else
      ;
    end;
  end;
  Check((LDecisionRemovals > 0) and (LContradictions = 2)
      and (LBacktracks = 2) and (LRestores > 0)
      and (LRetryDecisions = 2) and (LSolved = 1),
    'the trace contains the complete two-branch recovery lifecycle');
  Check((LReport.Contradictions = 2) and (LReport.Backtracks = 2),
    'trace capture does not reinterpret solver counters');

  Check(SolveReferenceModel(LModel, 2, True, nil,
      LAssignmentTwin, LReportTwin),
    'an identical traced solve replays');
  Check(SameAssignment(LAssignment, LAssignmentTwin)
      and SameReportCore(LReport, LReportTwin)
      and SameTrace(LReport.Trace, LReportTwin.Trace),
    'the complete causal event stream is deterministic');
end;

begin
  WriteLn('WFC reference trace conformance suite');
  WriteLn('====================================');
  TestNoTraceCompatibility;
  TestPropagationTrace;
  TestLockAndDecisionTrace;
  TestRequiredSupportTrace;
  TestBacktrackTraceAndReplay;
  WriteLn('====================================');
  WriteLn(GChecks, ' checks, ', GFailures, ' failures');
  if GFailures <> 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d reference trace checks failed',
      [GFailures]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
