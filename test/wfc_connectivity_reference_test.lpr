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
program wfc_connectivity_reference_test;

{$mode delphi}{$H+}

uses Classes, SysUtils, wfc_solver_reference, wfc_connectivity_reference
  {$IFDEF PAS2JS}, wfc_browser_test_host{$ENDIF};

var Checks, Failures, OracleCases: Integer;

procedure Check(const B: Boolean; const S: String);
begin
  Inc(Checks);
  if not B then begin Inc(Failures); WriteLn('FAIL: ', S); end;
end;

procedure Model(const Cells, Values: Integer; out M: TReferenceModel);
var I: Integer;
begin
  M := Default(TReferenceModel);
  M.CellCount := Cells; M.ValueCount := Values;
  SetLength(M.Neighbors, Cells * 6);
  for I := 0 to High(M.Neighbors) do M.Neighbors[I] := -1;
  SetLength(M.Compatibility, 6 * Values * Values);
  for I := 0 to High(M.Compatibility) do M.Compatibility[I] := 1;
  SetLength(M.RequiredValues, Values);
  SetLength(M.RequiredSupport, Length(M.Compatibility));
  SetLength(M.InitialAllowed, Cells * Values);
  for I := 0 to High(M.InitialAllowed) do M.InitialAllowed[I] := 1;
  SetLength(M.InitialFailureKinds, Cells);
  SetLength(M.LockedValues, Cells); SetLength(M.CellOrder, Cells);
  for I := 0 to Cells - 1 do
  begin
    M.InitialFailureKinds[I] := rckEmptyDomain;
    M.LockedValues[I] := -1; M.CellOrder[I] := I;
  end;
end;

function Opposite(const D: Integer): Integer;
begin
  case D of
    0: Result := 2; 1: Result := 3; 2: Result := 0;
    3: Result := 1; 4: Result := 5; 5: Result := 4;
    else raise Exception.Create('bad test direction');
  end;
end;

procedure Edge(var M: TReferenceModel; const A, B, D: Integer);
begin M.Neighbors[A * 6 + D] := B; M.Neighbors[B * 6 + Opposite(D)] := A; end;

procedure Mask(var M: TReferenceModel; const Cell, Bits: Integer);
var V: Integer;
begin
  for V := 0 to M.ValueCount - 1 do
    M.InitialAllowed[Cell * M.ValueCount + V] := Ord((Bits and (1 shl V)) <> 0);
end;

procedure Constraint(var M: TReferenceModel; const Root: Integer;
  const Required: array of Integer; const All: Boolean = False);
var I, N: Integer;
begin
  N := Length(M.Connectivity); SetLength(M.Connectivity, N + 1);
  M.Connectivity[N].RootCell := Root;
  M.Connectivity[N].RequireAllParticipants := All;
  SetLength(M.Connectivity[N].RequiredCells, Length(Required));
  for I := 0 to High(Required) do M.Connectivity[N].RequiredCells[I] := Required[I];
end;

procedure Profile(var M: TReferenceModel; const Value, Ports: Integer;
  const Required: Boolean = False; const Index: Integer = 0);
var N: Integer;
begin
  N := Length(M.Connectivity[Index].Profiles);
  SetLength(M.Connectivity[Index].Profiles, N + 1);
  M.Connectivity[Index].Profiles[N].ValueIndex := Value;
  M.Connectivity[Index].Profiles[N].Ports := Ports;
  M.Connectivity[Index].Profiles[N].RequiredByValue := Required;
end;

function PortsOf(const C: TReferenceConnectivityConstraint;
  const V: Integer): Integer;
var I: Integer;
begin
  Result := -1;
  for I := 0 to High(C.Profiles) do
    if C.Profiles[I].ValueIndex = V then Exit(C.Profiles[I].Ports);
end;

function MustReach(const C: TReferenceConnectivityConstraint;
  const Cell, V: Integer): Boolean;
var I: Integer;
begin
  if Cell = C.RootCell then Exit(True);
  for I := 0 to High(C.RequiredCells) do if C.RequiredCells[I] = Cell then Exit(True);
  for I := 0 to High(C.Profiles) do
    if C.Profiles[I].ValueIndex = V then
      Exit(C.RequireAllParticipants or C.Profiles[I].RequiredByValue);
  Result := False;
end;

//Independent oracle: enumerate assignments and flood selected cells directly.
//It does not call the connectivity analyzer, possible-edge builder, lowlinks,
//or complete-validator implementation and does not inspect solver domains.
function Valid(const M: TReferenceModel; const A: TReferenceIntegerArray): Boolean;
var C, D, N, I, J, P, Q, Head, Tail: Integer; Seen: array of Boolean;
  Queue: array of Integer; Equal, Supported: Boolean;
begin
  Result := False;
  if Length(A) <> M.CellCount then Exit;
  for C := 0 to M.CellCount - 1 do
  begin
    if (A[C] < 0) or (A[C] >= M.ValueCount) then Exit;
    if M.InitialAllowed[C * M.ValueCount + A[C]] = 0 then Exit;
    if (M.LockedValues[C] >= 0) and (A[C] <> M.LockedValues[C]) then Exit;
    Supported := M.RequiredValues[A[C]] = 0;
    for D := 0 to 5 do
    begin
      N := M.Neighbors[C * 6 + D]; if N < 0 then Continue;
      I := (D * M.ValueCount + A[C]) * M.ValueCount + A[N];
      if M.Compatibility[I] = 0 then Exit;
      if M.RequiredSupport[I] <> 0 then Supported := True;
    end;
    if not Supported and (M.LockedValues[C] < 0) then Exit;
  end;
  for I := 0 to High(M.ExcludedAssignments) do
  begin
    Equal := True;
    for C := 0 to M.CellCount - 1 do
      if A[C] <> M.ExcludedAssignments[I][C] then Equal := False;
    if Equal then Exit;
  end;
  SetLength(Seen, M.CellCount); SetLength(Queue, M.CellCount);
  for I := 0 to High(M.Connectivity) do
  begin
    for C := 0 to M.CellCount - 1 do Seen[C] := False;
    C := M.Connectivity[I].RootCell;
    if PortsOf(M.Connectivity[I], A[C]) < 0 then Exit;
    Seen[C] := True; Queue[0] := C; Head := 0; Tail := 1;
    while Head < Tail do
    begin
      C := Queue[Head]; Inc(Head); P := PortsOf(M.Connectivity[I], A[C]);
      for D := 0 to 5 do
      begin
        N := M.Neighbors[C * 6 + D];
        if (N < 0) or Seen[N] then Continue;
        Q := PortsOf(M.Connectivity[I], A[N]);
        if (Q < 0) or ((P and (1 shl D)) = 0) or
          ((Q and (1 shl Opposite(D))) = 0) then Continue;
        if M.Neighbors[N * 6 + Opposite(D)] <> C then Continue;
        J := (Opposite(D) * M.ValueCount + A[N]) * M.ValueCount + A[C];
        if M.Compatibility[J] = 0 then Continue;
        Seen[N] := True; Queue[Tail] := N; Inc(Tail);
      end;
    end;
    for C := 0 to M.CellCount - 1 do
      if MustReach(M.Connectivity[I], C, A[C]) and
        ((PortsOf(M.Connectivity[I], A[C]) < 0) or not Seen[C]) then Exit;
  end;
  Result := True;
end;

function Exists(const M: TReferenceModel): Boolean;
var A: TReferenceIntegerArray; C: Integer;
begin
  SetLength(A, M.CellCount);
  repeat
    if Valid(M, A) then Exit(True);
    C := 0;
    while (C < M.CellCount) and (A[C] = M.ValueCount - 1) do
    begin A[C] := 0; Inc(C); end;
    if C = M.CellCount then Exit(False);
    Inc(A[C]);
  until False;
end;

procedure CompareOracle(const M: TReferenceModel; const LabelText: String);
var A: TReferenceIntegerArray; R: TReferenceSolveReport; Expected, Actual: Boolean;
begin
  Inc(OracleCases);
  Expected := Exists(M);
  Actual := SolveReferenceModel(M, High(Integer), nil, A, R);
  Check(Actual = Expected, 'oracle existence ' + LabelText);
  if Actual then Check(Valid(M, A), 'independent solved assignment ' + LabelText)
  else Check((Length(A) = 0) and (R.Status = rssContradiction),
    'exhaustion yields no assignment ' + LabelText);
end;

procedure TestBottlenecks;
var M: TReferenceModel; A: TReferenceIntegerArray; R: TReferenceSolveReport;
  I, Count: Integer;
begin
  Model(5, 2, M);
  for I := 0 to 3 do Edge(M, I, I + 1, 1);
  Constraint(M, 0, [4, 4]); Profile(M, 1, 10);
  Check(SolveReferenceModel(M, 0, True, nil, A, R), 'corridor solves without search');
  Check((R.Decisions = 0) and (R.Propagations = 5), 'root, terminal and all bottlenecks forced');
  Count := 0;
  for I := 0 to High(R.Trace) do
    if R.Trace[I].CauseKind = rtckConnectivity then
    begin
      Inc(Count); Check(R.Trace[I].ConstraintIndex = 0, 'pruning trace descriptor index');
    end
    else Check(R.Trace[I].ConstraintIndex = -1, 'ordinary trace descriptor sentinel');
  Check(Count = 5, 'five connectivity deletions are observable');
  Check(Valid(M, A), 'corridor independently valid');
  Check(R.Contradiction.ConstraintIndex = -1, 'success clears contradiction descriptor');
  M.LockedValues[2] := 0;
  Check(not SolveReferenceModel(M, 100, True, nil, A, R), 'locked separator contradicts');
  Check((R.Decisions = 0) and (R.Contradiction.Kind = rckConnectivity) and
    (R.Contradiction.ConstraintIndex = 0), 'unreachable terminal diagnosed before decisions');
  Check(Length(A) = 0, 'locked-cut failure has no partial assignment');
  M.LockedValues[2] := -1;
  Check(SolveReferenceModel(M, 0, nil, A, R), 'failed solve did not mutate source model');
  for I := 0 to High(M.InitialAllowed) do Check(M.InitialAllowed[I] = 1, 'source domains remain detached');
end;

procedure TestPoliciesAndPorts;
var M: TReferenceModel; A: TReferenceIntegerArray; R: TReferenceSolveReport;
  D, I: Integer;
begin
  Model(3, 3, M); Edge(M, 0, 1, 1);
  Constraint(M, 0, []); Profile(M, 1, 10); Profile(M, 2, 10, True);
  M.LockedValues[0] := 1; M.LockedValues[1] := 1;
  Mask(M, 2, 6);
  Check(SolveReferenceModel(M, 0, True, nil, A, R), 'optional disconnected participant allowed');
  Check((A[2] = 1) and (R.Decisions = 0), 'unreachable required-by-value candidate pruned');
  M.Connectivity[0].RequireAllParticipants := True;
  Check(not SolveReferenceModel(M, 10, nil, A, R), 'all-participants rejects disconnected island');
  Mask(M, 2, 7);
  Check(SolveReferenceModel(M, 0, nil, A, R) and (A[2] = 0), 'all-participants island may choose absence');
  M.LockedValues[2] := 2;
  Check(not SolveReferenceModel(M, 10, nil, A, R), 'locks do not exempt global required values');

  for D := 0 to 5 do
  begin
    Model(2, 3, M); Edge(M, 0, 1, D);
    Constraint(M, 0, [1]); Profile(M, 1, 1 shl D);
    Profile(M, 2, 1 shl Opposite(D)); M.LockedValues[0] := 1; M.LockedValues[1] := 2;
    Check(SolveReferenceModel(M, 0, nil, A, R), 'reciprocal port direction ' + IntToStr(D));
    M.Connectivity[0].Profiles[1].Ports := 0;
    Check(not SolveReferenceModel(M, 0, nil, A, R), 'one-sided port cannot connect');
    M.Connectivity[0].Profiles[1].Ports := 1 shl Opposite(D);
    I := (Opposite(D) * 3 + 2) * 3 + 1; M.Compatibility[I] := 0;
    Check(not SolveReferenceModel(M, 0, nil, A, R), 'reciprocal compatibility remains mandatory');
  end;

  Model(1, 2, M); for D := 0 to 5 do M.Neighbors[D] := 0;
  Constraint(M, 0, [0, 0], True); Profile(M, 1, 0);
  Check(SolveReferenceModel(M, 0, nil, A, R) and (A[0] = 1), 'zero-port self-root is one physical participant');
  M.Compatibility[(1 * 2 + 1) * 2 + 1] := 0;
  Check(not SolveReferenceModel(M, 0, nil, A, R), 'self alias still obeys ordinary same-value adjacency');

  Model(2, 2, M); Edge(M, 0, 1, 1); Edge(M, 1, 0, 1);
  Constraint(M, 0, [1], True); Profile(M, 1, 10);
  Check(SolveReferenceModel(M, 0, nil, A, R), 'wrapped parallel east/west aliases connect physical cells');
  Model(2, 2, M); M.Neighbors[1] := 1;
  Constraint(M, 0, [1]); Profile(M, 1, 10);
  Check(not SolveReferenceModel(M, 0, nil, A, R), 'nonreciprocal numeric neighbor is not an edge');

  Model(1, 3, M); Constraint(M, 0, []); Profile(M, 1, 0);
  Constraint(M, 0, []); Profile(M, 2, 0, False, 1);
  Check(not SolveReferenceModel(M, 0, True, nil, A, R), 'multiple descriptors are AND, not union');
  Check((R.Contradiction.Kind = rckConnectivity) and (R.Contradiction.ConstraintIndex = 1),
    'independent second descriptor identified');
end;

procedure TestPortCorrelationAndTrail;
var M: TReferenceModel; A: TReferenceIntegerArray; R, R2: TReferenceSolveReport;
  I: Integer; Removed, Restored: Boolean;
begin
  Model(3, 6, M); Edge(M, 0, 1, 1); Edge(M, 0, 2, 0);
  Constraint(M, 0, [1, 2]);
  Profile(M, 1, 2); Profile(M, 2, 1); Profile(M, 3, 3);
  Profile(M, 4, 8); Profile(M, 5, 4);
  Mask(M, 0, 14); Mask(M, 1, 16); Mask(M, 2, 32);
  Check(not SolveReferenceModel(M, 0, True, nil, A, R), 'unrelated possible-edge witnesses need search');
  Check((R.Status = rssBacktrackLimit) and (R.Contradiction.Kind = rckConnectivity),
    'port correlation failure retains connectivity cause at budget limit');
  Check(SolveReferenceModel(M, 2, True, nil, A, R), 'finite search finds common port witness');
  Check((A[0] = 3) and (R.Backtracks = 2) and Valid(M, A), 'two failed orientations restore and retry');
  Mask(M, 0, 6); CompareOracle(M, 'unsatisfiable pairwise witnesses');

  Model(4, 2, M); Edge(M, 0, 1, 1); Edge(M, 0, 2, 2);
  Edge(M, 1, 3, 2); Edge(M, 2, 3, 1);
  Constraint(M, 0, [3]); Profile(M, 1, 15);
  M.LockedValues[0] := 1; M.LockedValues[3] := 1;
  SetLength(M.ExcludedAssignments, 1); SetLength(M.ExcludedAssignments[0], 4);
  M.ExcludedAssignments[0][0] := 1; M.ExcludedAssignments[0][1] := 0;
  M.ExcludedAssignments[0][2] := 1; M.ExcludedAssignments[0][3] := 1;
  Check(SolveReferenceModel(M, 20, True, nil, A, R), 'backtrack around an excluded valid route');
  Check((A[1] = 1) and (A[2] = 0) and Valid(M, A), 'alternative route survives restored bottleneck domain');
  Removed := False; Restored := False;
  for I := 0 to High(R.Trace) do
  begin
    if (R.Trace[I].EntryIndex = 2) and (R.Trace[I].ValueIndex = 0) and
      (R.Trace[I].CauseKind = rtckConnectivity) then Removed := True;
    if (R.Trace[I].EntryIndex = 2) and (R.Trace[I].ValueIndex = 0) and
      (R.Trace[I].Kind = rtekCandidateRestored) then Restored := True;
    if R.Trace[I].CauseKind <> rtckConnectivity then
      Check(R.Trace[I].ConstraintIndex = -1, 'restoration/exclusion sentinel stays -1');
  end;
  Check(Removed and Restored, 'connectivity deletions use the ordinary reversible trail');
  Check(SolveReferenceModel(M, 20, True, nil, A, R2), 'same model replay succeeds');
  Check((R.Backtracks = R2.Backtracks) and (Length(R.Trace) = Length(R2.Trace)),
    'deterministic replay counters and trace length');
  for I := 0 to High(R.Trace) do
    Check((R.Trace[I].Kind = R2.Trace[I].Kind) and
      (R.Trace[I].EntryIndex = R2.Trace[I].EntryIndex) and
      (R.Trace[I].ValueIndex = R2.Trace[I].ValueIndex) and
      (R.Trace[I].CauseKind = R2.Trace[I].CauseKind) and
      (R.Trace[I].ConstraintIndex = R2.Trace[I].ConstraintIndex), 'deterministic numeric event');
end;

procedure TestExhaustive;
const AXES: array[0..2] of Integer = (1, 2, 4);
var M: TReferenceModel; A, B, C, D, Mode, Dir, I: Integer;
begin
  //Every nonempty entry domain of three values in a three-cell corridor,
  //in all three spatial axes. Vary fixed/conditional/all requirements and
  //one-sided opening profiles without borrowing solver reachability code.
  for Dir := 0 to 2 do
    for Mode := 0 to 3 do
    begin
      Model(3, 3, M); Edge(M, 0, 1, AXES[Dir]); Edge(M, 1, 2, AXES[Dir]);
      if (Mode and 1) = 0 then Constraint(M, 0, [2], (Mode and 2) <> 0)
      else Constraint(M, 0, [], (Mode and 2) <> 0);
      Profile(M, 1, (1 shl AXES[Dir]) or (1 shl Opposite(AXES[Dir])));
      Profile(M, 2, 1 shl Opposite(AXES[Dir]), True);
      for A := 1 to 7 do for B := 1 to 7 do for C := 1 to 7 do
      begin
        Mask(M, 0, A); Mask(M, 1, B); Mask(M, 2, C);
        CompareOracle(M, 'axis/domain ' + IntToStr(OracleCases));
      end;
    end;
  for Mode := 0 to 7 do
  begin
    Model(4, 2, M); Edge(M, 0, 1, 1); Edge(M, 0, 2, 2);
    Edge(M, 1, 3, 2); Edge(M, 2, 3, 1);
    if (Mode and 4) <> 0 then
    begin Edge(M, 1, 0, 1); Edge(M, 3, 2, 1); end;
    Constraint(M, 0, [3], (Mode and 1) <> 0); Profile(M, 1, 15);
    if (Mode and 2) <> 0 then M.LockedValues[2] := 1;
    for A := 1 to 3 do for B := 1 to 3 do for C := 1 to 3 do for D := 1 to 3 do
    begin
      Mask(M, 0, A); Mask(M, 1, B); Mask(M, 2, C); Mask(M, 3, D);
      CompareOracle(M, 'square/wrap/lock ' + IntToStr(OracleCases));
    end;
  end;
  //All 512 directional compatibility matrices for a two-cell, three-value
  //edge, with the reverse deliberately transposed so ordinary AC participates.
  for Mode := 0 to 511 do
  begin
    Model(2, 3, M); Edge(M, 0, 1, 1); Constraint(M, 0, [1]);
    Profile(M, 1, 10); Profile(M, 2, 10, True);
    for I := 0 to 8 do
    begin
      M.Compatibility[9 + I] := Ord((Mode and (1 shl I)) <> 0);
      M.Compatibility[27 + (I mod 3) * 3 + I div 3] := M.Compatibility[9 + I];
    end;
    CompareOracle(M, 'compatibility matrix ' + IntToStr(Mode));
  end;
end;

procedure TestLargeIterative;
const COUNT = 12000;
var M: TReferenceModel; A: TReferenceIntegerArray; R: TReferenceSolveReport; I: Integer;
begin
  Model(COUNT, 2, M);
  for I := 0 to COUNT - 2 do Edge(M, I, I + 1, 4);
  Constraint(M, 0, [COUNT - 1]); Profile(M, 1, 48);
  Check(SolveReferenceModel(M, 0, nil, A, R), 'deep vertical corridor uses iterative traversal');
  Check((Length(A) = COUNT) and (R.Decisions = 0) and (R.Propagations = COUNT),
    'long bottleneck chain has no recursion or authored route vocabulary');
end;

procedure ExpectInvalid(const M: TReferenceModel; const S: String);
var A: TReferenceIntegerArray; R: TReferenceSolveReport; Rejected: Boolean;
begin
  Rejected := False;
  try SolveReferenceModel(M, 0, nil, A, R);
  except on E: ERangeError do Rejected := True;
    on E: EInvalidOperation do Rejected := True;
  end;
  Check(Rejected, S);
end;

procedure TestMalformedAndOldPath;
var M: TReferenceModel; A: TReferenceIntegerArray; R: TReferenceSolveReport; I: Integer;
  {$IFNDEF PAS2JS}RawFlag: Byte;{$ENDIF}
begin
  Model(1, 2, M); Constraint(M, 0, []); Profile(M, 1, 0);
  M.Connectivity[0].RootCell := -1; ExpectInvalid(M, 'negative root rejected');
  M.Connectivity[0].RootCell := 1; ExpectInvalid(M, 'root extent rejected');
  M.Connectivity[0].RootCell := 0;
  SetLength(M.Connectivity[0].RequiredCells, 1);
  M.Connectivity[0].RequiredCells[0] := 1; ExpectInvalid(M, 'required extent rejected');
  M.Connectivity[0].RequiredCells := nil;
  M.Connectivity[0].Profiles[0].ValueIndex := 2; ExpectInvalid(M, 'profile value extent rejected');
  M.Connectivity[0].Profiles[0].ValueIndex := 1;
  M.Connectivity[0].Profiles[0].Ports := 64; ExpectInvalid(M, 'undefined port bit rejected');
  M.Connectivity[0].Profiles[0].Ports := 0;
  Profile(M, 1, 0); ExpectInvalid(M, 'duplicate profile rejected');
  M.Connectivity[0].Profiles := nil; ExpectInvalid(M, 'empty profiles rejected');
  Profile(M, 1, 0);
  M.CellCount := High(Integer); ExpectInvalid(M, 'oversized flattened matrices reject before allocation');
  M.CellCount := 1; M.ValueCount := High(Integer);
  ExpectInvalid(M, 'oversized value matrices reject before allocation');

  Model(1, 2, M); Mask(M, 0, 0);
  M.InitialFailureKinds[0] := rckConnectivity;
  ExpectInvalid(M, 'caller cannot forge descriptor-free connectivity classification');
  Constraint(M, 0, []); Profile(M, 1, 0);
  ExpectInvalid(M, 'only analyzer may publish descriptor-bearing connectivity failures');

  {$IFNDEF PAS2JS}
  for I := 0 to 1 do
  begin
    if I = 0 then RawFlag := 2 else RawFlag := 255;
    Model(1, 2, M); Constraint(M, 0, []); Profile(M, 1, 0);
    Move(RawFlag, M.Connectivity[0].RequireAllParticipants, SizeOf(Boolean));
    ExpectInvalid(M, 'malformed native all-participants Boolean');
    M.Connectivity[0].RequireAllParticipants := False;
    Move(RawFlag, M.Connectivity[0].Profiles[0].RequiredByValue, SizeOf(Boolean));
    ExpectInvalid(M, 'malformed native required-by-value Boolean');
  end;
  {$ENDIF}

  Model(1, 2, M);
  Check(SolveReferenceModel(M, 0, True, nil, A, R), 'empty-descriptor historical path solves');
  Check((A[0] = 0) and (R.Decisions = 1) and (R.Propagations = 0), 'old candidate order unchanged');
  Check(R.Contradiction.ConstraintIndex = -1, 'old success sentinel');
  for I := 0 to High(R.Trace) do Check(R.Trace[I].ConstraintIndex = -1, 'all old trace fields initialized');
  Mask(M, 0, 0);
  Check(not SolveReferenceModel(M, 0, True, nil, A, R), 'old empty domain fails');
  Check((R.Contradiction.Kind = rckEmptyDomain) and (R.Contradiction.ConstraintIndex = -1),
    'old contradiction kind/sentinel preserved');
end;

procedure TestDecisionCause;
var M: TReferenceModel; A: TReferenceIntegerArray; R: TReferenceSolveReport;
  I: Integer; Found: Boolean;
begin
  Model(1, 3, M); Constraint(M, 0, []); Profile(M, 1, 0); Profile(M, 2, 0);
  Check(SolveReferenceModel(M, 0, True, nil, A, R), 'root with two participating choices solves');
  Found := False;
  for I := 0 to High(R.Trace) do
  begin
    if R.Trace[I].CauseKind = rtckConnectivity then
      Check(R.Trace[I].ConstraintIndex = 0, 'connectivity-derived event retains descriptor')
    else Check(R.Trace[I].ConstraintIndex = -1, 'other cause has no descriptor');
    if R.Trace[I].Kind = rtekDecision then
    begin
      Found := True;
      Check((R.Trace[I].CauseKind = rtckConnectivity) and
        (R.Trace[I].ConstraintIndex = 0), 'decision inherits connectivity cause and ordinal');
    end;
  end;
  Check(Found, 'connectivity-derived decision actually exercised');
end;

{$IFDEF PAS2JS}
procedure TestMalformedBrowserNumbers;
var M: TReferenceModel; X: Integer; B: Boolean; I, J: Integer;
begin
  for I := 0 to 5 do
  begin
    case I of
      0: asm X = NaN; end;
      1: asm X = Infinity; end;
      2: asm X = -Infinity; end;
      3: asm X = 0.5; end;
      4: asm X = undefined; end;
      5: asm X = "0"; end;
    end;
    for J := 0 to 7 do
    begin
      Model(1, 2, M); Constraint(M, 0, [0]); Profile(M, 1, 0);
      case J of
        0: M.Connectivity[0].RootCell := X;
        1: M.Connectivity[0].RequiredCells[0] := X;
        2: M.Connectivity[0].Profiles[0].ValueIndex := X;
        3: M.Connectivity[0].Profiles[0].Ports := X;
        4: M.Neighbors[0] := X;
        5: M.CellCount := X;
        6: M.InitialAllowed[0] := X;
        7: M.Compatibility[0] := X;
      end;
      ExpectInvalid(M, 'malformed browser numeric field ' + IntToStr(I) + '/' + IntToStr(J));
    end;
  end;
  for I := 0 to 2 do
  begin
    case I of
      0: asm B = 1; end;
      1: asm B = "true"; end;
      2: asm B = undefined; end;
    end;
    Model(1, 2, M); Constraint(M, 0, []); Profile(M, 1, 0);
    M.Connectivity[0].RequireAllParticipants := B; ExpectInvalid(M, 'all flag strictly Boolean');
    M.Connectivity[0].RequireAllParticipants := False;
    M.Connectivity[0].Profiles[0].RequiredByValue := B; ExpectInvalid(M, 'required flag strictly Boolean');
  end;
end;
{$ENDIF}

begin
  try
    TestBottlenecks; TestPoliciesAndPorts; TestPortCorrelationAndTrail;
    TestExhaustive; TestLargeIterative; TestMalformedAndOldPath; TestDecisionCause;
    {$IFDEF PAS2JS}TestMalformedBrowserNumbers;{$ENDIF}
  except on E: Exception do begin Inc(Failures); WriteLn('UNEXPECTED: ', E.ClassName, ': ', E.Message); end; end;
  WriteLn('Connectivity oracle models: ', OracleCases);
  WriteLn('Passed: ', Checks - Failures, '/', Checks);
  if Failures <> 0 then Halt(1);
end.
