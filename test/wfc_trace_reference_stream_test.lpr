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
program wfc_trace_reference_stream_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc_solver_reference;

type
  TTraceReceiver = class
  public
    Expected: TReferenceTraceEvents;
    Compare: Boolean;
    Count, RandomCalls, FailAt: Integer;
    constructor Create;
    procedure Receive(const AEvent: TReferenceTraceEvent);
    function Choose(const ACount: Integer): Integer;
  end;

var
  Checks, Failures: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then
  begin
    Inc(Failures);
    WriteLn('FAIL: ', AMessage);
  end;
end;

function SameEvent(const A, B: TReferenceTraceEvent): Boolean;
begin
  Result := (A.EventId = B.EventId) and (A.CauseEventId = B.CauseEventId)
    and (A.Kind = B.Kind) and (A.CauseKind = B.CauseKind)
    and (A.EntryIndex = B.EntryIndex) and (A.ValueIndex = B.ValueIndex)
    and (A.NeighborIndex = B.NeighborIndex) and (A.Direction = B.Direction)
    and (A.DecisionDepth = B.DecisionDepth)
    and (A.DomainCountBefore = B.DomainCountBefore)
    and (A.DomainCountAfter = B.DomainCountAfter)
    and (A.ConstraintIndex = B.ConstraintIndex);
end;

constructor TTraceReceiver.Create;
begin
  inherited Create;
  FailAt := -1;
end;

procedure TTraceReceiver.Receive(const AEvent: TReferenceTraceEvent);
begin
  Check(AEvent.EventId = Count, 'stream IDs remain consecutive');
  Check((AEvent.CauseEventId >= -1) and (AEvent.CauseEventId < Count),
    'stream causes only reference earlier events');
  if Compare then
  begin
    Check(Count < Length(Expected), 'stream does not exceed buffered event count');
    if Count < Length(Expected) then
      Check(SameEvent(AEvent, Expected[Count]),
        'every streamed numeric field equals buffered capture');
  end;
  if Count = FailAt then
    raise Exception.Create('intentional reference sink failure');
  Inc(Count);
end;

function TTraceReceiver.Choose(const ACount: Integer): Integer;
begin
  Check(ACount > 0, 'random callback receives a positive bound');
  Inc(RandomCalls);
  Result := 0;
end;

procedure InitializeModel(const ACells, AValues: Integer;
  out AModel: TReferenceModel);
var I: Integer;
begin
  AModel := Default(TReferenceModel);
  AModel.CellCount := ACells;
  AModel.ValueCount := AValues;
  SetLength(AModel.Neighbors, ACells * WFC_REFERENCE_DIRECTION_COUNT);
  for I := 0 to High(AModel.Neighbors) do AModel.Neighbors[I] := -1;
  SetLength(AModel.Compatibility,
    WFC_REFERENCE_DIRECTION_COUNT * AValues * AValues);
  for I := 0 to High(AModel.Compatibility) do AModel.Compatibility[I] := 1;
  SetLength(AModel.RequiredValues, AValues);
  SetLength(AModel.RequiredSupport, Length(AModel.Compatibility));
  SetLength(AModel.InitialAllowed, ACells * AValues);
  for I := 0 to High(AModel.InitialAllowed) do AModel.InitialAllowed[I] := 1;
  SetLength(AModel.InitialFailureKinds, ACells);
  SetLength(AModel.LockedValues, ACells);
  SetLength(AModel.CellOrder, ACells);
  for I := 0 to ACells - 1 do
  begin
    AModel.InitialFailureKinds[I] := rckEmptyDomain;
    AModel.LockedValues[I] := -1;
    AModel.CellOrder[I] := I;
  end;
end;

function SameAssignment(const A, B: TReferenceIntegerArray): Boolean;
var I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do
    if A[I] <> B[I] then Exit(False);
  Result := True;
end;

function SameCore(const A, B: TReferenceSolveReport): Boolean;
begin
  Result := (A.Status = B.Status) and (A.Decisions = B.Decisions)
    and (A.Propagations = B.Propagations) and (A.Backtracks = B.Backtracks)
    and (A.Contradictions = B.Contradictions)
    and (A.ExcludedAssignments = B.ExcludedAssignments)
    and (A.Contradiction.Kind = B.Contradiction.Kind)
    and (A.Contradiction.EntryIndex = B.Contradiction.EntryIndex)
    and (A.Contradiction.NeighborIndex = B.Contradiction.NeighborIndex)
    and (A.Contradiction.Direction = B.Contradiction.Direction)
    and (A.Contradiction.ConstraintIndex = B.Contradiction.ConstraintIndex);
end;

procedure CompareModes(const AModel: TReferenceModel;
  const ABacktracks: Integer; const AContext: String);
var
  Receiver: TTraceReceiver;
  Expected, Actual: TReferenceSolveReport;
  ExpectedAssignment, ActualAssignment: TReferenceIntegerArray;
  ExpectedResult, ActualResult: Boolean;
  ExpectedRandomCalls, Mode, I: Integer;
begin
  Receiver := TTraceReceiver.Create;
  try
    ExpectedResult := SolveReferenceModel(AModel, ABacktracks, True,
      Receiver.Choose, ExpectedAssignment, Expected);
    ExpectedRandomCalls := Receiver.RandomCalls;
    Receiver.Expected := Expected.Trace;
    for Mode := 0 to 2 do
    begin
      Receiver.Count := 0;
      Receiver.RandomCalls := 0;
      Receiver.Compare := True;
      if Mode = 2 then
        ActualResult := SolveReferenceModel(AModel, ABacktracks, False, nil,
          Receiver.Choose, ActualAssignment, Actual)
      else
        ActualResult := SolveReferenceModel(AModel, ABacktracks, Mode = 1,
          Receiver.Receive, Receiver.Choose, ActualAssignment, Actual);
      Check(ActualResult = ExpectedResult, AContext + ': same solve outcome');
      Check(SameCore(Expected, Actual), AContext + ': same complete report core');
      Check(SameAssignment(ExpectedAssignment, ActualAssignment),
        AContext + ': same assignments');
      Check(Receiver.RandomCalls = ExpectedRandomCalls,
        AContext + ': same random callback count');
      if Mode = 2 then
        Check(Receiver.Count = 0, AContext + ': nil sink stays disabled')
      else
        Check(Receiver.Count = Length(Expected.Trace),
          AContext + ': whole event stream delivered');
      if Mode = 1 then
      begin
        Check(Length(Actual.Trace) = Length(Expected.Trace),
          AContext + ': explicit buffered capture retained');
        for I := 0 to High(Actual.Trace) do
          Check(SameEvent(Actual.Trace[I], Expected.Trace[I]),
            AContext + ': buffered capture unchanged with sink');
      end
      else
        Check(Length(Actual.Trace) = 0,
          AContext + ': nonretaining mode publishes no event array');
    end;
  finally
    Receiver.Free;
  end;
end;

procedure TestModes;
var Model: TReferenceModel; I, D: Integer;
begin
  InitializeModel(0, 2, Model);
  CompareModes(Model, 0, 'zero cells');
  InitializeModel(3, 3, Model);
  CompareModes(Model, 0, 'ordinary decisions');
  Model.InitialAllowed[0] := 0;
  Model.LockedValues[1] := 2;
  CompareModes(Model, 0, 'initial domain and lock causes');
  Model.InitialAllowed[1] := 0;
  Model.InitialAllowed[2] := 0;
  CompareModes(Model, 0, 'initial contradiction');

  InitializeModel(3, 3, Model);
  for I := 0 to High(Model.Compatibility) do Model.Compatibility[I] := 0;
  for I := 0 to 2 do
  begin
    Model.Neighbors[I * 6] := (I + 1) mod 3;
    Model.Neighbors[I * 6 + 1] := (I + 2) mod 3;
  end;
  for D := 0 to 1 do
  begin
    Model.Compatibility[D * 9 + 1] := 1;
    Model.Compatibility[D * 9 + 3] := 1;
    Model.Compatibility[D * 9 + 8] := 1;
  end;
  CompareModes(Model, 0, 'backtrack limit');
  CompareModes(Model, 2, 'abandoned branches and restored candidates');

  InitializeModel(2, 2, Model);
  SetLength(Model.ExcludedAssignments, 1);
  SetLength(Model.ExcludedAssignments[0], 2);
  CompareModes(Model, 4, 'exact assignment exclusion');

  InitializeModel(3, 3, Model);
  Model.Neighbors[1] := 1; Model.Neighbors[9] := 0;
  Model.Neighbors[7] := 2; Model.Neighbors[15] := 1;
  SetLength(Model.Connectivity, 1);
  Model.Connectivity[0].RootCell := 0;
  SetLength(Model.Connectivity[0].RequiredCells, 1);
  Model.Connectivity[0].RequiredCells[0] := 2;
  SetLength(Model.Connectivity[0].Profiles, 2);
  for I := 0 to 1 do
  begin
    Model.Connectivity[0].Profiles[I].ValueIndex := I + 1;
    Model.Connectivity[0].Profiles[I].Ports := 10;
  end;
  CompareModes(Model, 4, 'connectivity-caused decisions');
end;

procedure TestNonretainingAndFailure;
var
  Model: TReferenceModel;
  Receiver: TTraceReceiver;
  Assignment: TReferenceIntegerArray;
  Report: TReferenceSolveReport;
  I: Integer;
  Raised: Boolean;
begin
  InitializeModel(4096, 2, Model);
  for I := 0 to Model.CellCount - 1 do Model.InitialAllowed[I * 2 + 1] := 0;
  Receiver := TTraceReceiver.Create;
  try
    Check(SolveReferenceModel(Model, 0, False, Receiver.Receive, nil,
      Assignment, Report), 'large nonretaining solve succeeds');
    Check((Receiver.Count = 4097) and (Length(Report.Trace) = 0),
      '4097 events delivered to a count-only receiver without report retention');
    Receiver.Count := 0;
    Receiver.FailAt := 2;
    Raised := False;
    try
      SolveReferenceModel(Model, 0, False, Receiver.Receive, nil,
        Assignment, Report);
    except
      on E: Exception do
        Raised := E.Message = 'intentional reference sink failure';
    end;
    Check(Raised, 'low-level sink failure propagates to its adapter');
    Check(Receiver.Count = 2, 'no later callback follows a low-level failure');
  finally
    Receiver.Free;
  end;
end;

begin
  TestModes;
  TestNonretainingAndFailure;
  WriteLn(Checks, ' reference stream checks, ', Failures, ' failures');
  if Failures <> 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d reference stream checks failed', [Failures]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
