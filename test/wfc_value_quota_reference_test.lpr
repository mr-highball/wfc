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
program wfc_value_quota_reference_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  Classes, SysUtils, wfc_solver_reference;

var
  Checks, Failures, OracleCases, CheckedRemovals: Integer;

procedure Check(const ACondition: Boolean; const ALabel: String);
begin
  Inc(Checks);
  if ACondition then Exit;
  Inc(Failures);
  if Failures <= 20 then WriteLn('[FAIL] ', ALabel);
end;

procedure Model(const ACells, AValues: Integer; out M: TReferenceModel);
var I: Integer;
begin
  M := Default(TReferenceModel);
  M.CellCount := ACells; M.ValueCount := AValues;
  SetLength(M.Neighbors, ACells * 6);
  for I := 0 to High(M.Neighbors) do M.Neighbors[I] := -1;
  SetLength(M.Compatibility, 6 * AValues * AValues);
  for I := 0 to High(M.Compatibility) do M.Compatibility[I] := 1;
  SetLength(M.RequiredValues, AValues);
  SetLength(M.RequiredSupport, Length(M.Compatibility));
  SetLength(M.InitialAllowed, ACells * AValues);
  for I := 0 to High(M.InitialAllowed) do M.InitialAllowed[I] := 1;
  SetLength(M.InitialFailureKinds, ACells);
  SetLength(M.LockedValues, ACells); SetLength(M.CellOrder, ACells);
  for I := 0 to ACells - 1 do
  begin
    M.InitialFailureKinds[I] := rckEmptyDomain;
    M.LockedValues[I] := -1; M.CellOrder[I] := I;
  end;
end;

procedure Domain(var M: TReferenceModel; const C, Mask: Integer);
var V: Integer;
begin
  for V := 0 to M.ValueCount - 1 do
    M.InitialAllowed[C * M.ValueCount + V] := Ord((Mask and (1 shl V)) <> 0);
end;

procedure Quota(var M: TReferenceModel; const Mask, AMin, AMax: Integer);
var N, V, K: Integer;
begin
  N := Length(M.ValueQuotas); SetLength(M.ValueQuotas, N + 1);
  M.ValueQuotas[N].MinimumCount := AMin; M.ValueQuotas[N].MaximumCount := AMax;
  for V := 0 to M.ValueCount - 1 do
    if (Mask and (1 shl V)) <> 0 then
    begin
      K := Length(M.ValueQuotas[N].Values);
      SetLength(M.ValueQuotas[N].Values, K + 1);
      M.ValueQuotas[N].Values[K] := V;
    end;
end;

function Member(const Values: TReferenceIntegerArray; const V: Integer): Boolean;
var I: Integer;
begin
  for I := 0 to High(Values) do if Values[I] = V then Exit(True);
  Result := False;
end;

//Independent complete-assignment oracle. No quota analyzer, domain-count
//shortcut, or solver validation routine participates in this recount.
function Valid(const M: TReferenceModel; const A: TReferenceIntegerArray;
  const Domains: TReferenceByteArray): Boolean;
var C, D, N, I, J, Count: Integer; Equal, Supported: Boolean;
begin
  Result := False;
  if Length(A) <> M.CellCount then Exit;
  for C := 0 to M.CellCount - 1 do
  begin
    if (A[C] < 0) or (A[C] >= M.ValueCount) then Exit;
    I := C * M.ValueCount + A[C];
    if M.InitialAllowed[I] = 0 then Exit;
    if (Length(Domains) <> 0) and (Domains[I] = 0) then Exit;
    if (M.LockedValues[C] >= 0) and (M.LockedValues[C] <> A[C]) then Exit;
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
  for J := 0 to High(M.ValueQuotas) do
  begin
    Count := 0;
    for C := 0 to M.CellCount - 1 do
      if Member(M.ValueQuotas[J].Values, A[C]) then Inc(Count);
    if (Count < M.ValueQuotas[J].MinimumCount) or
      (Count > M.ValueQuotas[J].MaximumCount) then Exit;
  end;
  Result := True;
end;

function Exists(const M: TReferenceModel; const Domains: TReferenceByteArray;
  const Cell: Integer = -1; const Value: Integer = -1): Boolean;
var A: TReferenceIntegerArray; C: Integer;
begin
  SetLength(A, M.CellCount);
  repeat
    if ((Cell < 0) or (A[Cell] = Value)) and Valid(M, A, Domains) then Exit(True);
    C := 0;
    while (C < M.CellCount) and (A[C] = M.ValueCount - 1) do
    begin A[C] := 0; Inc(C); end;
    if C = M.CellCount then Exit(False);
    Inc(A[C]);
  until False;
end;

function Fingerprint(const M: TReferenceModel): String;
var I, J: Integer;
begin
  Result := '';
  for I := 0 to High(M.InitialAllowed) do Result := Result + IntToStr(M.InitialAllowed[I]);
  for I := 0 to High(M.LockedValues) do Result := Result + ',' + IntToStr(M.LockedValues[I]);
  for I := 0 to High(M.ValueQuotas) do
  begin
    Result := Result + ';' + IntToStr(M.ValueQuotas[I].MinimumCount) + ':' +
      IntToStr(M.ValueQuotas[I].MaximumCount);
    for J := 0 to High(M.ValueQuotas[I].Values) do
      Result := Result + ',' + IntToStr(M.ValueQuotas[I].Values[J]);
  end;
end;

function SoundTrace(const M: TReferenceModel; const T: TReferenceTraceEvents): Boolean;
var Domains: TReferenceByteArray; I, K: Integer;
begin
  SetLength(Domains, M.CellCount * M.ValueCount);
  for I := 0 to High(Domains) do Domains[I] := 1;
  for I := 0 to High(T) do
    if T[I].Kind in [rtekInitialCandidateRemoved, rtekCandidateRemoved,
      rtekCandidateRestored] then
    begin
      if (T[I].EntryIndex < 0) or (T[I].EntryIndex >= M.CellCount) or
        (T[I].ValueIndex < 0) or (T[I].ValueIndex >= M.ValueCount) then Exit(False);
      K := T[I].EntryIndex * M.ValueCount + T[I].ValueIndex;
      if T[I].Kind = rtekCandidateRestored then Domains[K] := 1
      else
      begin
        if T[I].CauseKind = rtckValueQuota then
        begin
          Inc(CheckedRemovals);
          if Exists(M, Domains, T[I].EntryIndex, T[I].ValueIndex) then Exit(False);
        end;
        Domains[K] := 0;
      end;
    end;
  Result := True;
end;

function SameTrace(const A, B: TReferenceTraceEvents): Boolean;
var I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do
    if (A[I].EventId <> B[I].EventId) or
      (A[I].CauseEventId <> B[I].CauseEventId) or
      (A[I].Kind <> B[I].Kind) or (A[I].CauseKind <> B[I].CauseKind) or
      (A[I].EntryIndex <> B[I].EntryIndex) or (A[I].ValueIndex <> B[I].ValueIndex) or
      (A[I].NeighborIndex <> B[I].NeighborIndex) or (A[I].Direction <> B[I].Direction) or
      (A[I].DecisionDepth <> B[I].DecisionDepth) or
      (A[I].DomainCountBefore <> B[I].DomainCountBefore) or
      (A[I].DomainCountAfter <> B[I].DomainCountAfter) or
      (A[I].ConstraintIndex <> B[I].ConstraintIndex) then Exit(False);
  Result := True;
end;

procedure CompareOracle(const M: TReferenceModel);
var A: TReferenceIntegerArray; R: TReferenceSolveReport;
  Expected, Actual: Boolean; Before, LabelText: String;
begin
  Inc(OracleCases); LabelText := 'oracle model ' + IntToStr(OracleCases);
  Expected := Exists(M, nil); Before := Fingerprint(M);
  Actual := SolveReferenceModel(M, High(Integer), True, nil, A, R);
  Check(Expected = Actual, LabelText + ' feasibility');
  if Actual then Check(Valid(M, A, nil), LabelText + ' independent recount')
  else Check((Length(A) = 0) and (R.Status = rssContradiction), LabelText + ' exhausted result');
  Check(SoundTrace(M, R.Trace), LabelText + ' sound pruning in every replayed branch');
  Check(Fingerprint(M) = Before, LabelText + ' caller model unchanged');
end;

procedure TestExhaustive;
const DomainMasks: array[0..3] of Integer = (3, 5, 6, 7);
var M: TReferenceModel; Cells, Values, Limit, Total, Code, N, C,
  SetMask, Lo, Hi, A, B, Matrix, I: Integer;
begin
  //Every domain (including empty), every nonempty value set, and every
  //inclusive in-range quota: 4608 independently enumerated models.
  for Cells := 2 to 3 do
  begin
    Values := 5 - Cells; Limit := 1 shl Values; Total := 1;
    for C := 1 to Cells do Total := Total * Limit;
    Model(Cells, Values, M);
    for Code := 0 to Total - 1 do
    begin
      N := Code;
      for C := 0 to Cells - 1 do begin Domain(M, C, N mod Limit); N := N div Limit; end;
      for SetMask := 1 to Limit - 1 do
        for Lo := 0 to Cells do for Hi := Lo to Cells do
        begin M.ValueQuotas := nil; Quota(M, SetMask, Lo, Hi); CompareOracle(M); end;
    end;
  end;
  //All exact bounds for two overlapping sets over every combination of
  //the four multi-valued three-value domains: neither set is a partition.
  Model(3, 3, M);
  for Code := 0 to 63 do
  begin
    N := Code;
    for C := 0 to 2 do begin Domain(M, C, DomainMasks[N mod 4]); N := N div 4; end;
    for A := 0 to 3 do for B := 0 to 3 do
    begin
      M.ValueQuotas := nil; Quota(M, 3, A, A); Quota(M, 6, B, B); CompareOracle(M);
    end;
  end;
  //Every directional relation for a two-cell, three-value edge, with the
  //reverse transposed, independently intersected with an exact set quota.
  for Matrix := 0 to 511 do
  begin
    Model(2, 3, M); M.Neighbors[1] := 1; M.Neighbors[9] := 0;
    for I := 0 to 8 do
    begin
      M.Compatibility[9 + I] := Ord((Matrix and (1 shl I)) <> 0);
      M.Compatibility[27 + (I mod 3) * 3 + I div 3] := M.Compatibility[9 + I];
    end;
    Quota(M, 3, 1, 1); CompareOracle(M);
  end;
  //All caller-lock combinations, including locks incompatible with quotas.
  for Code := 0 to 26 do
  begin
    Model(3, 2, M); N := Code;
    for C := 0 to 2 do begin M.LockedValues[C] := N mod 3 - 1; N := N div 3; end;
    for A := 0 to 3 do
    begin M.ValueQuotas := nil; Quota(M, 1, A, A); CompareOracle(M); end;
  end;
end;

procedure TestBoundsAndRestoration;
var M: TReferenceModel; A, Again: TReferenceIntegerArray; R, R2: TReferenceSolveReport;
  I, C, V: Integer; Removed, Restored: Boolean; QuotaRemoved: TReferenceByteArray;
begin
  Model(0, 2, M); Quota(M, 1, 0, 5); CompareOracle(M);
  M.ValueQuotas[0].MinimumCount := 1; CompareOracle(M);
  Check(not SolveReferenceModel(M, 0, nil, A, R) and
    (R.Contradiction.Kind = rckValueQuota) and (R.Contradiction.EntryIndex = -1),
    'empty pass unmet minimum is a quota contradiction');
  Model(2, 2, M); Quota(M, 1, 3, 4); CompareOracle(M);
  M.ValueQuotas[0].MinimumCount := 0; M.ValueQuotas[0].MaximumCount := High(Integer);
  CompareOracle(M);
  Model(3, 2, M); Quota(M, 1, 0, 0);
  Check(SolveReferenceModel(M, 0, True, nil, A, R) and (R.Decisions = 0),
    'zero maximum eliminates the counted set without decisions');
  Check(Valid(M, A, nil) and SoundTrace(M, R.Trace), 'zero quota recount and sound pruning');
  Model(3, 2, M); Quota(M, 1, 3, 3);
  Check(SolveReferenceModel(M, 0, True, nil, A, R) and (R.Decisions = 0),
    'tight minimum forces every eligible cell');
  Check(Valid(M, A, nil) and SoundTrace(M, R.Trace), 'minimum quota recount and sound pruning');
  Model(2, 3, M); M.Neighbors[1] := 1; M.Neighbors[9] := 0;
  for C := 0 to 2 do for V := 0 to 2 do
  begin
    M.Compatibility[9 + C * 3 + V] := Ord(C = V);
    M.Compatibility[27 + C * 3 + V] := Ord(C = V);
  end;
  Quota(M, 1, 0, 1);
  Check(not SolveReferenceModel(M, 0, True, nil, A, R) and
    (R.Status = rssBacktrackLimit), 'quota failure honors zero backtrack budget');
  Check(SolveReferenceModel(M, 10, True, nil, A, R) and (R.Backtracks > 0),
    'quota contradiction restores the trail and searches an alternative');
  Check(Valid(M, A, nil) and SoundTrace(M, R.Trace), 'backtracked assignment and pruning match oracle');
  Model(2, 2, M); Quota(M, 1, 1, 1);
  SetLength(M.ExcludedAssignments, 1); M.ExcludedAssignments[0] := [0, 1];
  Check(SolveReferenceModel(M, 10, True, nil, A, R) and (R.Backtracks > 0),
    'exact exclusion forces recovery after quota propagation');
  SetLength(QuotaRemoved, M.CellCount * M.ValueCount);
  Removed := False; Restored := False;
  for I := 0 to High(R.Trace) do
  begin
    if (R.Trace[I].Kind = rtekCandidateRemoved) and
      (R.Trace[I].CauseKind = rtckValueQuota) then
    begin
      Removed := True;
      QuotaRemoved[R.Trace[I].EntryIndex * M.ValueCount + R.Trace[I].ValueIndex] := 1;
    end;
    if R.Trace[I].Kind = rtekCandidateRestored then
      if QuotaRemoved[R.Trace[I].EntryIndex * M.ValueCount + R.Trace[I].ValueIndex] <> 0 then
        Restored := True;
  end;
  Check(Removed and Restored and Valid(M, A, nil) and SoundTrace(M, R.Trace),
    'quota-pruned candidates are restored and independently revalidated');
  Check(SolveReferenceModel(M, 10, True, nil, Again, R2) and
    (R.Backtracks = R2.Backtracks) and SameTrace(R.Trace, R2.Trace),
    'quota backtracking replays deterministically');
  Model(2, 3, M); Quota(M, 3, 2, 2);
  SolveReferenceModel(M, 10, True, nil, A, R);
  M.ValueQuotas[0].Values := [1, 0];
  SolveReferenceModel(M, 10, True, nil, Again, R2);
  Check(SameTrace(R.Trace, R2.Trace) and (A[0] = Again[0]) and
    (A[1] = Again[1]), 'numeric input set order does not change search');
end;

procedure ExpectInvalid(const M: TReferenceModel; const LabelText: String);
var A: TReferenceIntegerArray; R: TReferenceSolveReport; Rejected: Boolean;
begin
  Rejected := False;
  try SolveReferenceModel(M, 0, nil, A, R);
  except on E: ERangeError do Rejected := True;
    on E: EInvalidOperation do Rejected := True; end;
  Check(Rejected, LabelText);
end;

procedure TestInvalid;
var M: TReferenceModel;
begin
  Model(2, 2, M); Quota(M, 1, 0, 2);
  M.ValueQuotas[0].MinimumCount := -1; ExpectInvalid(M, 'negative quota minimum rejected');
  M.ValueQuotas[0].MinimumCount := 2; M.ValueQuotas[0].MaximumCount := 1;
  ExpectInvalid(M, 'inverted bounds rejected');
  M.ValueQuotas[0].MinimumCount := 0; M.ValueQuotas[0].MaximumCount := 2;
  M.ValueQuotas[0].Values := nil; ExpectInvalid(M, 'empty numeric set rejected');
  M.ValueQuotas[0].Values := [-1]; ExpectInvalid(M, 'negative value index rejected');
  M.ValueQuotas[0].Values := [2]; ExpectInvalid(M, 'out-of-range value index rejected');
  M.ValueQuotas[0].Values := [0, 0]; ExpectInvalid(M, 'duplicate numeric indices rejected');
  M.ValueQuotas := nil; M.InitialFailureKinds[0] := rckValueQuota;
  ExpectInvalid(M, 'quota initial-failure kind is reserved even without descriptors');
end;

{$IFDEF PAS2JS}
procedure TestBrowserNumbers;
var M: TReferenceModel; X, I, J: Integer;
begin
  for I := 0 to 7 do
  begin
    case I of
      0: asm X = NaN; end;
      1: asm X = Infinity; end;
      2: asm X = -Infinity; end;
      3: asm X = 0.5; end;
      4: asm X = undefined; end;
      5: asm X = "0"; end;
      6: asm X = null; end;
      7: asm X = 4294967296; end;
    end;
    for J := 0 to 5 do
    begin
      Model(2, 2, M); Quota(M, 1, 0, 2);
      case J of
        0: M.ValueQuotas[0].MinimumCount := X;
        1: M.ValueQuotas[0].MaximumCount := X;
        2: M.ValueQuotas[0].Values[0] := X;
        3: M.CellCount := X;
        4: M.ValueCount := X;
        5: M.LockedValues[0] := X;
      end;
      ExpectInvalid(M, 'malformed browser quota/model integer ' + IntToStr(I) + '/' + IntToStr(J));
    end;
  end;
end;
{$ENDIF}

begin
  try
    TestExhaustive; TestBoundsAndRestoration; TestInvalid;
    {$IFDEF PAS2JS}TestBrowserNumbers;{$ENDIF}
    Check(CheckedRemovals > 0, 'exhaustive oracle exercised real quota candidate removals');
  except on E: Exception do begin Inc(Failures); WriteLn('UNEXPECTED: ', E.ClassName, ': ', E.Message); end; end;
  WriteLn('Value-quota oracle models: ', OracleCases, '; checked removals: ', CheckedRemovals);
  WriteLn('Value-quota reference checks: ', Checks - Failures, '/', Checks);
  if Failures <> 0 then Halt(1);
end.
