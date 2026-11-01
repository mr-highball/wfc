(*
MIT License

Copyright (c) 2021, 2026 mr-highball

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
program wfc_domain_prepare_test;
{$mode delphi}{$H+}
uses SysUtils, wfc_solver_reference, wfc_solve_progress
  {$IFDEF PAS2JS}{$IFNDEF WFC_NODE_DOMAIN_TEST}, wfc_browser_test_host{$ENDIF}{$ENDIF};
type
  TObserver = class
    Calls: Integer;
    procedure Observe(const P: TWfcSolveProgress);
  end;
var Checks: Integer;

procedure Check(const B: Boolean; const S: String);
begin Inc(Checks); if not B then raise Exception.Create(S); end;

procedure TObserver.Observe(const P: TWfcSolveProgress);
begin
  Inc(Calls);
  if P.Done > 0 then raise EAbort.Create('domain cancellation');
end;

function Fixture(const Cells: Integer = 2): TReferenceModel;
var I, J, V, N: Integer;
begin
  Result := Default(TReferenceModel);
  Result.CellCount := Cells; Result.ValueCount := 2;
  SetLength(Result.Neighbors, Cells * WFC_REFERENCE_DIRECTION_COUNT);
  for I := 0 to High(Result.Neighbors) do Result.Neighbors[I] := -1;
  for I := 0 to Cells - 1 do
  begin
    if I + 1 < Cells then Result.Neighbors[I * WFC_REFERENCE_DIRECTION_COUNT] := I + 1;
    if I > 0 then Result.Neighbors[I * WFC_REFERENCE_DIRECTION_COUNT + 1] := I - 1;
  end;
  SetLength(Result.Compatibility, WFC_REFERENCE_DIRECTION_COUNT * 4);
  SetLength(Result.RequiredSupport, Length(Result.Compatibility));
  for I := 0 to WFC_REFERENCE_DIRECTION_COUNT - 1 do
    for V := 0 to 1 do for N := 0 to 1 do
      if V = N then Result.Compatibility[(I * 2 + V) * 2 + N] := 1;
  SetLength(Result.RequiredValues, 2);
  SetLength(Result.InitialAllowed, Cells * 2);
  for I := 0 to High(Result.InitialAllowed) do Result.InitialAllowed[I] := 1;
  Result.InitialAllowed[Cells * 2 - 1] := 0;
  SetLength(Result.InitialFailureKinds, Cells);
  SetLength(Result.LockedValues, Cells);
  SetLength(Result.CellOrder, Cells);
  for J := 0 to Cells - 1 do
  begin
    Result.LockedValues[J] := -1; Result.CellOrder[J] := J;
    Result.InitialFailureKinds[J] := rckEntryDomain;
  end;
end;

procedure Run;
var M: TReferenceModel; D, Again: TReferenceByteArray;
  P: TReferenceDomainReport; Search: TReferenceSolveReport;
  Assignment: TReferenceIntegerArray;
  O: TObserver; Work: TWfcSolveWork; Raised: Boolean; I: Integer;
begin
  M := Fixture;
  SetLength(M.ValueWeights, 2); M.ValueWeights[0] := 1; M.ValueWeights[1] := 100;
  Check(PropagateReferenceModel(M, D, P), 'primary propagation reaches a consistent fixed point');
  Check(P.Consistent and (P.Candidates = 2) and (P.Propagations > 0), 'domain report describes candidates, not decisions');
  Check((Length(D) = 4) and (D[0] = 1) and (D[1] = 0) and (D[2] = 1) and (D[3] = 0),
    'known future domain propagates through the original temporal arcs');
  Check((M.InitialAllowed[1] = 1) and (M.ValueWeights[1] = 100), 'borrowed masks and preferences remain unchanged');
  D[0] := 0;
  Check(PropagateReferenceModel(M, Again, P) and (Again[0] = 1), 'returned domains are detached');
  M.InitialAllowed[3] := 1;
  Check(PropagateReferenceModel(M, D, P) and (P.Candidates = 4), 'preparation makes no random choice');
  for I := 0 to 3 do Check(D[I] = 1, 'every supported choice survives');
  M.InitialAllowed[3] := 0; M.LockedValues[0] := 1;
  Check(not PropagateReferenceModel(M, D, P) and (D = nil) and not P.Consistent,
    'contradiction clears ordinary output without fabricating an assignment');
  Check(P.Contradiction.Kind <> rckNone, 'primary contradiction classification is retained');
  M := Fixture; M.InitialAllowed[3] := 1;
  SetLength(M.ValueQuotas, 1); SetLength(M.ValueQuotas[0].Values, 1);
  M.ValueQuotas[0].Values[0] := 0;
  M.ValueQuotas[0].MinimumCount := 2; M.ValueQuotas[0].MaximumCount := 2;
  Check(PropagateReferenceModel(M, D, P) and (P.Candidates = 2), 'primary quota propagation is shared');
  Check((D[1] = 0) and (D[3] = 0), 'quota removes unsupported candidates');
  M := Fixture;
  SetLength(M.ExcludedAssignments, 1); SetLength(M.ExcludedAssignments[0], 2);
  M.ExcludedAssignments[0][0] := 0; M.ExcludedAssignments[0][1] := 0;
  Check(PropagateReferenceModel(M, D, P), 'local consistency is not a global feasibility promise');
  Check(not SolveReferenceModel(M, 0, nil, Assignment, Search) and
    (Search.ExcludedAssignments = 1), 'full search still owns complete assignment exclusions');
  SetLength(D, 1); D[0] := 9;
  M := Fixture; M.InitialAllowed := nil; Raised := False;
  try PropagateReferenceModel(M, D, P);
  except on E: Exception do Raised := True; end;
  Check(Raised and (Length(D) = 1) and (D[0] = 9), 'malformed input preserves prior caller output');
  M := Fixture(128); O := TObserver.Create; Work := TWfcSolveWork.Create(O.Observe);
  try
    Work.BeginPhase(wspPrepare, 0); Raised := False;
    try PropagateReferenceModel(M, D, P, Work);
    except on E: EAbort do Raised := E.Message = 'domain cancellation'; end;
    Check(Raised and (O.Calls > 1) and (Length(D) = 1) and (D[0] = 9),
      'interior cancellation preserves prior caller output');
  finally Work.Free; O.Free; end;
end;

begin
  Run;
  WriteLn('Primary domain preparation checks: ', Checks);
end.
