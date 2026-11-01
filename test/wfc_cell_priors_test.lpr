(*
MIT License

Copyright (c) 2026 mr-highball

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
program wfc_cell_priors_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}{$IFNDEF WFC_NODE_TEST}wfc_browser_test_host,{$ENDIF}{$ENDIF}
  Classes, SysUtils, wfc, wfc_solver_reference, wfc_solve_progress;
type
  TMiddle = class
    Counts: TReferenceIntegerArray;
    function Pick(const ACount: Integer): Integer;
  end;
  TGuard = class
    Graph: TGraph;
    Rejected: Integer;
    Cancel: Boolean;
    procedure Observe(const AWork: TWfcSolveProgress);
  end;
var Checks: Integer;
procedure Check(const AValue: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not AValue then raise Exception.Create(AMessage);
end;
function TMiddle.Pick(const ACount: Integer): Integer;
begin
  SetLength(Counts, Length(Counts) + 1);
  Counts[High(Counts)] := ACount;
  Result := ACount div 2;
end;
function Model(const ACells: Integer = 2; const AValues: Integer = 2): TReferenceModel;
var I: Integer;
begin
  Result := Default(TReferenceModel);
  Result.CellCount := ACells;
  Result.ValueCount := AValues;
  SetLength(Result.Neighbors, ACells * WFC_REFERENCE_DIRECTION_COUNT);
  for I := 0 to High(Result.Neighbors) do Result.Neighbors[I] := -1;
  SetLength(Result.Compatibility, AValues * AValues * WFC_REFERENCE_DIRECTION_COUNT);
  for I := 0 to High(Result.Compatibility) do Result.Compatibility[I] := 1;
  SetLength(Result.RequiredValues, AValues);
  SetLength(Result.RequiredSupport, Length(Result.Compatibility));
  SetLength(Result.InitialAllowed, ACells * AValues);
  for I := 0 to High(Result.InitialAllowed) do Result.InitialAllowed[I] := 1;
  SetLength(Result.InitialFailureKinds, ACells);
  SetLength(Result.LockedValues, ACells);
  SetLength(Result.CellOrder, ACells);
  for I := 0 to ACells - 1 do
  begin
    Result.LockedValues[I] := -1;
    Result.CellOrder[I] := I;
  end;
end;
procedure TGuard.Observe(const AWork: TWfcSolveProgress);
var LWeights: TGraphValueWeights;
begin
  Check(Graph.Running, 'The observer runs inside the guarded transaction');
  if Rejected = 0 then
  begin
    try Graph.SetCellValueWeights(0, 0, 0, [MakeGraphValueWeight('A', 2)]);
    except on E: EInvalidOperation do Inc(Rejected) end;
    try Graph.ClearCellValueWeights(0, 0, 0);
    except on E: EInvalidOperation do Inc(Rejected) end;
    LWeights := Graph.CopyCellValueWeights(0, 0, 0);
    LWeights[0].Weight := 99;
    Check(Graph.CopyCellValueWeights(0, 0, 0)[0].Weight <> 99,
      'Read-only observation receives detached prior inputs');
  end;
  if Cancel and (AWork.Done >= 256) then raise EAbort.Create('Operator cancelled');
end;
procedure Kernel;
var
  M: TReferenceModel;
  R, R0: TReferenceSolveReport;
  A, A0: TReferenceIntegerArray;
  P: TMiddle;
  I: Integer;
  LRejected: Boolean;
begin
  P := TMiddle.Create;
  try
    M := Model;
    SetLength(M.CellValueWeights, 4);
    M.CellValueWeights[0] := 31;
    M.CellValueWeights[1] := 1;
    M.CellValueWeights[2] := 1;
    M.CellValueWeights[3] := 31;
    Check(SolveReferenceModel(M, 16, P.Pick, A, R) and (A[0] = 0) and (A[1] = 1),
      'Opposing positive priors select different values inside one actual solve');
    Check((P.Counts[0] = 32) and (P.Counts[1] = 32), 'Each position supplies its own ticket distribution');
    Check((M.CellValueWeights[0] = 31) and (M.CellValueWeights[3] = 31),
      'Kernel normalization never mutates caller priors');
    M.LockedValues[0] := 1;
    M.InitialAllowed[3] := 0;
    Check(SolveReferenceModel(M, 16, P.Pick, A, R) and (A[0] = 1) and (A[1] = 0),
      'Positive preference never overrides a lock or allowed domain');

    M := Model(2, 4);
    SetLength(M.CellValueWeights, 8);
    M.CellValueWeights[4] := 100;
    P.Counts := nil;
    Check(SolveReferenceModel(M, 16, True, P.Pick, A, R), 'Skewed entropy model solves');
    Check((R.Trace[0].EntryIndex = 1) and (P.Counts[0] = 103),
      'Weighted entropy prioritizes the actual lower-entropy position');

    M := Model;
    P.Counts := nil;
    Check(SolveReferenceModel(M, 16, True, P.Pick, A0, R0), 'Default baseline solves');
    SetLength(M.CellValueWeights, 4);
    for I := 0 to 3 do M.CellValueWeights[I] := 2;
    Check(SolveReferenceModel(M, 16, True, P.Pick, A, R), 'Relative-neutral explicit priors solve');
    Check((A[0] = A0[0]) and (A[1] = A0[1]) and (Length(R.Trace) = Length(R0.Trace)),
      'Neutral priors preserve default assignment and causal extent');
    for I := 0 to High(R.Trace) do
      Check((R.Trace[I].Kind = R0.Trace[I].Kind) and
        (R.Trace[I].EntryIndex = R0.Trace[I].EntryIndex) and
        (R.Trace[I].ValueIndex = R0.Trace[I].ValueIndex), 'Neutral causal decisions stay exact');

    M := Model;
    SetLength(M.CellValueWeights, 4);
    M.CellValueWeights[0] := 1;
    M.CellValueWeights[1] := 31;
    M.CellValueWeights[2] := 31;
    M.CellValueWeights[3] := 1;
    SetLength(M.ExcludedAssignments, 2);
    for I := 0 to 1 do
    begin
      SetLength(M.ExcludedAssignments[I], 2);
      M.ExcludedAssignments[I][0] := 1;
      M.ExcludedAssignments[I][1] := I;
    end;
    Check(SolveReferenceModel(M, 16, True, P.Pick, A, R) and (A[0] = 0) and (A[1] = 0),
      'Backtracking restores the correct per-position distributions after exhausted branches');
    Check(R.Backtracks >= 2, 'The regression exercises real restoration, not only forward choice');

    M := Model;
    M.ValueWeights := [10, 20];
    SetLength(M.CellValueWeights, 4);
    M.CellValueWeights[1] := 10;
    P.Counts := nil;
    Check(SolveReferenceModel(M, 16, P.Pick, A, R), 'Partial absolute prior row solves');
    Check((P.Counts[0] = 3) and (P.Counts[1] = 2),
      'Omitted values inherit raw global weights before each row is normalized');
    Check((M.ValueWeights[0] = 10) and (M.ValueWeights[1] = 20) and
      (M.CellValueWeights[1] = 10), 'Per-row normalization leaves both caller vectors untouched');

    for I := 0 to 2 do
    begin
      M := Model;
      SetLength(M.CellValueWeights, 4);
      case I of
        0: SetLength(M.CellValueWeights, 3);
        1: M.CellValueWeights[0] := -1;
        2: begin M.CellValueWeights[0] := High(Integer); M.CellValueWeights[1] := High(Integer) - 1 end;
      end;
      P.Counts := nil;
      LRejected := False;
      try SolveReferenceModel(M, 16, P.Pick, A, R);
      except on Exception do LRejected := True end;
      Check(LRejected and (Length(P.Counts) = 0), 'Malformed or overflowing priors reject before randomness');
    end;
  finally
    P.Free;
  end;
end;
procedure PublicGraph;
var
  G: TGraph;
  R, R0: TGraphSolveReport;
  N: TGraphNegotiationReport;
  W: TGraphValueWeights;
  S: TGraphAssignmentSnapshot;
  O: TGraphSolveOptions;
  Guard: TGuard;
  Baseline: TGraphValues;
  I, J: Integer;
  LRejected: Boolean;
begin
  G := TGraph.Create;
  Guard := TGuard.Create;
  try
    O := DefaultGraphSolveOptions;
    O.CaptureTrace := True;
    G.Reshape(64, 1, 1);
    G.WrapNeighbors := False;
    G.Seed := 42;
    G.AddValue('A');
    G.AddValue('B');
    W := [MakeGraphValueWeight('B', 1), MakeGraphValueWeight('A', 31)];
    G.SetCellValueWeights(0, 0, 0, W);
    W[0].Weight := 99;
    W := G.CopyCellValueWeights(0, 0, 0);
    Check((W[0].Value = 'A') and (W[0].Weight = 31) and (W[1].Weight = 1),
      'Pass priors detach input arrays and canonicalize value order');
    W[0].Weight := 2;
    Check(G.CopyCellValueWeights(0, 0, 0)[0].Weight = 31, 'Prior getters detach their arrays');
    for I := 0 to 3 do
    begin
      W := [MakeGraphValueWeight('A', 3)];
      case I of
        0: W[0].Weight := 0;
        1: W[0].Value := 'unknown';
        2: W := [W[0], W[0]];
      end;
      LRejected := False;
      try
        if I = 3 then G.SetCellValueWeights(64, 0, 0, W)
        else G.SetCellValueWeights(0, 0, 0, W);
      except on Exception do LRejected := True end;
      Check(LRejected and (G.CopyCellValueWeights(0, 0, 0)[0].Weight = 31),
        'Invalid prior replacement preserves original inputs');
    end;
    G.ClearCellValueWeights(0, 0, 0);
    Check(not G.HasCellValueWeights(0, 0, 0), 'Clear returns to ordinary value weights');
    Check(G.TrySolve(O, R0), 'Default graph baseline solves');
    SetLength(Baseline, 64);
    for I := 0 to 63 do Baseline[I] := G[I, 0, 0].Value;
    for I := 0 to 63 do G.SetCellValueWeights(I, 0, 0,
      [MakeGraphValueWeight('A', 2), MakeGraphValueWeight('B', 2)]);
    Check(G.TrySolve(O, R), 'Neutral graph priors solve');
    Check((R.TraceHash <> 0) and (R.TraceHash = R0.TraceHash),
      'Relative-neutral priors preserve actual captured default trace hash');
    for I := 0 to 63 do Check(G[I, 0, 0].Value = Baseline[I], 'Neutral seeded public output stays exact');
    Guard.Graph := G;
    Guard.Cancel := True;
    LRejected := False;
    try G.TrySolve(DefaultGraphSolveOptions, R, Guard.Observe);
    except on EAbort do LRejected := True end;
    Check(LRejected and (Guard.Rejected = 2), 'Running mutations reject and interior preparation can cancel');
    for I := 0 to 63 do
      Check((G[I, 0, 0].Value = Baseline[I]) and G[I, 0, 0].Generated and
        (G.CopyCellValueWeights(I, 0, 0)[0].Weight = 2), 'Cancellation preserves accepted values, ownership and priors');
    S := G.CaptureAssignments;
    for I := 0 to 63 do G[I, 0, 0].ClearValue;
    Check(G.HasCellValueWeights(0, 0, 0), 'Clearing assignments retains caller priors');
    G.RestoreAssignments(S);
    for I := 0 to 63 do
      Check((G[I, 0, 0].Value = Baseline[I]) and G[I, 0, 0].Generated and
        (G.CopyCellValueWeights(I, 0, 0)[0].Weight = 2),
        'Assignment restoration preserves configured priors and generated ownership');
    G.Reshape(2, 1, 1);
    Check(not G.HasCellValueWeights(0, 0, 0), 'Reshape clears old per-cell inputs');
    G.SetCellValueWeights(0, 0, 0, [MakeGraphValueWeight('A', 31)]);
    G[0, 0, 0].Value := 'B';
    G.SetAllowedValues(1, 0, 0, 'A');
    G.SetCellValueWeights(1, 0, 0, [MakeGraphValueWeight('B', 31)]);
    Check(G.TrySolve(DefaultGraphSolveOptions, R) and (G[0, 0, 0].Value = 'B') and
      (G[1, 0, 0].Value = 'A'), 'Public hard locks/domains prevail over positive priors');
    G.Reset;
    G.Reshape(2, 1, 1);
    G.Seed := 42;
    G.AddValue('A');
    G.AddValue('B');
    G.SetCellValueWeights(0, 0, 0, [MakeGraphValueWeight('A', 1000000)]);
    G.SetCellValueWeights(1, 0, 0, [MakeGraphValueWeight('B', 31)]);
    G.SwitchToPass('consumer');
    G.PassMode := gpmOverlay;
    G.AddValue('X').RequireFromPass('', 'A');
    G.AddValue('Y').RequireFromPass('', 'B');
    G.SetAllowedValues(0, 0, 0, 'Y');
    Check(not G.HasCellValueWeights(0, 0, 0), 'Root-selected inputs are pass scoped');
    Check(G.PassGraph[0].HasCellValueWeights(0, 0, 0), 'Direct child-pass access retains its prior');
    Check(G.TrySolveNegotiated(DefaultGraphNegotiationOptions, N), 'Negotiation honors priors while repairing a provider');
    Check((G.PassGraph[0].Entry[0, 0, 0].Value = 'B') and (G.PassGraph[1].Entry[0, 0, 0].Value = 'Y'),
      'Provider preference never relaxes a downstream hard requirement');
    for J := 0 to 1 do
      Check((G.PassGraph[0].Entry[J, 0, 0].Value = 'A') = (G.PassGraph[1].Entry[J, 0, 0].Value = 'X'),
        'All dependent cells follow the actual selected provider');
    G.Reset;
    G.Reshape(1, 1, 1);
    Check(not G.HasCellValueWeights(0, 0, 0), 'Full reset clears the prior pipeline');
  finally
    Guard.Free;
    G.Free;
  end;
end;
{$IFDEF PAS2JS}
procedure HostNumbers;
var
  G: TGraph;
  M: TReferenceModel;
  R: TReferenceSolveReport;
  A: TReferenceIntegerArray;
  W: TGraphValueWeights;
  P: TMiddle;
  X, I, J: Integer;
  LRejected: Boolean;
begin
  G := TGraph.Create.Reshape(2, 1, 1);
  P := TMiddle.Create;
  try
    G.AddValue('A');
    G.AddValue('B');
    G.SetCellValueWeights(0, 0, 0, [MakeGraphValueWeight('A', 3)]);
    for I := 0 to 8 do
    begin
      case I of
        0: asm X = NaN; end;
        1: asm X = Infinity; end;
        2: asm X = -Infinity; end;
        3: asm X = 0.5; end;
        4: asm X = undefined; end;
        5: asm X = "1"; end;
        6: asm X = null; end;
        7: asm X = 4294967296; end;
        8: asm X = true; end;
      end;
      W := [MakeGraphValueWeight('A', 2)];
      W[0].Weight := X;
      LRejected := False;
      try G.SetCellValueWeights(0, 0, 0, W);
      except on E: Exception do LRejected := True end;
      Check(LRejected and (G.CopyCellValueWeights(0, 0, 0)[0].Weight = 3),
        'Malformed host weight rejects without replacing the prior');
      LRejected := False;
      try G.SetCellValueWeights(X, 0, 0, [MakeGraphValueWeight('A', 2)]);
      except on E: Exception do LRejected := True end;
      Check(LRejected, 'Malformed host coordinate rejects');
      for J := 0 to 1 do
      begin
        M := Model;
        SetLength(M.CellValueWeights, 4);
        if J = 0 then M.CellValueWeights[0] := X
        else M.ValueWeights := [X, 1];
        P.Counts := nil;
        LRejected := False;
        try SolveReferenceModel(M, 16, P.Pick, A, R);
        except on E: Exception do LRejected := True end;
        Check(LRejected and (Length(P.Counts) = 0),
          'Malformed numeric-kernel prior rejects before random draws');
      end;
    end;
  finally
    P.Free;
    G.Free;
  end;
end;
{$ENDIF}
begin
  try
    Kernel;
    PublicGraph;
    {$IFDEF PAS2JS}HostNumbers;{$ENDIF}
    WriteLn('PASS cell priors: ', Checks, ' checks');
  except on E: Exception do
  begin WriteLn('FAIL: ', E.ClassName, ': ', E.Message); ExitCode := 1 end end;
end.
