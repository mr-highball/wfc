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
program wfc_assignments_test;
{$mode delphi}{$H+}
uses SysUtils, Classes, wfc, wfc_solve_progress, wfc_lattice;

type
  TObserver = class
    Graph: TGraph;
    Cancel: Boolean;
    CancelPhase: TWfcSolvePhase;
    Reenter: Boolean;
    Rejected: Boolean;
    Calls: Integer;
    procedure Observe(const AWork: TWfcSolveProgress);
  end;

var Checks: Integer;

procedure Check(const ACondition: Boolean; const ADetail: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(ADetail);
end;

procedure TObserver.Observe(const AWork: TWfcSolveProgress);
var LSnapshot: TGraphAssignmentSnapshot;
begin
  Inc(Calls);
  Check(AWork.Phase <> wspSearch, 'Assignments must not invoke solver search');
  if Reenter then
  begin
    Reenter := False;
    try LSnapshot := Graph.CaptureAssignments;
    except on EInvalidOperation do Rejected := True end;
  end;
  if Cancel and (AWork.Phase = CancelPhase) then raise EAbort.Create('Expected observer abort');
end;

function NewGraph: TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Reshape(2, 1, 1);
    Result.WrapNeighbors := False;
    Result.Seed := 731;
    Result.CurrentPass := 'source';
    Result.PassMode := gpmOverlay;
    Result.AddValue('a');
    Result.AddValue('b');
    Result.SwitchToPass('part');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('a').RequireFromPass('source', 'a');
    Result.AddValue('b').RequireFromPass('source', 'b');
    Result.SwitchToPass('other');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('a');
    Result.AddValue('b');
  except
    Result.Free;
    raise;
  end;
end;

function AllEmpty(const AGraph: TGraph): Boolean;
var P, C: Integer;
begin
  Result := True;
  for P := 0 to AGraph.TotalPassCount - 1 do
    for C := 0 to 1 do
      Result := Result and AGraph.PassGraph[P].Entry[C, 0, 0].Empty;
end;

procedure SameAssignments(const ALeft, ARight: TGraph);
var P, C: Integer;
begin
  for P := 0 to ALeft.TotalPassCount - 1 do
    for C := 0 to 1 do
    begin
      Check(ALeft.PassGraph[P].Entry[C, 0, 0].Value = ARight.PassGraph[P].Entry[C, 0, 0].Value,
        'Exact restored value');
      Check(ALeft.PassGraph[P].Entry[C, 0, 0].Empty = ARight.PassGraph[P].Entry[C, 0, 0].Empty,
        'Exact empty ownership');
      Check(ALeft.PassGraph[P].Entry[C, 0, 0].Generated = ARight.PassGraph[P].Entry[C, 0, 0].Generated,
        'Exact generated versus caller ownership');
    end;
end;

procedure Run;
var
  LOriginal: TGraph;
  LRestored: TGraph;
  LFresh: TGraph;
  LSnapshot: TGraphAssignmentSnapshot;
  LBad: TGraphAssignmentSnapshot;
  LReport: TGraphNegotiationReport;
  LSelective: TGraphSelectiveNegotiationReport;
  LObserver: TObserver;
  LRejected: Boolean;
  LOther: String;
  LUnrelated: array[0..1] of String;
  I: Integer;
begin
  LOriginal := NewGraph;
  LRestored := nil;
  LFresh := nil;
  LObserver := TObserver.Create;
  try
    LOriginal.PassGraph[0].Entry[0, 0, 0].Value := 'a';
    LSnapshot := LOriginal.CaptureAssignments;
    LRestored := NewGraph;
    LRestored.RestoreAssignments(LSnapshot);
    SameAssignments(LOriginal, LRestored);
    Check(LRestored.PassGraph[0].Entry[0, 0, 0].Value = 'a', 'Partial snapshot retains a real caller lock');
    FreeAndNil(LRestored);
    Check(LOriginal.TrySolveNegotiated(DefaultGraphNegotiationOptions, LReport), 'Baseline generation');
    Check(not LOriginal.PassGraph[0].Entry[0, 0, 0].Generated, 'Caller lock is not reclassified by solving');
    Check(LOriginal.PassGraph[0].Entry[1, 0, 0].Generated, 'Actual solver output is generated');
    LObserver.Graph := LOriginal;
    LObserver.Reenter := True;
    LSnapshot := LOriginal.CaptureAssignments(100, 10000, LObserver.Observe);
    Check(LObserver.Rejected and (LObserver.Calls > 0), 'Capture rejects observer reentry');
    Check(LOriginal.CurrentPass = 'other', 'Capture preserves selected pass');
    LRestored := NewGraph;
    LObserver.Graph := LRestored;
    LObserver.Reenter := True;
    LObserver.Rejected := False;
    LRestored.PassGraph[1].RestoreAssignments(LSnapshot, 100, 10000, LObserver.Observe);
    Check(LObserver.Rejected, 'Restore rejects observer reentry through the root');
    Check(LRestored.CurrentPass = 'other', 'Restore preserves selected pass');
    SameAssignments(LOriginal, LRestored);
    LSnapshot.Passes[0].Cells[1].Value := 'detached';
    Check(LRestored.PassGraph[0].Entry[1, 0, 0].Value <> 'detached', 'Restored cells do not borrow snapshot records');
    if LOriginal.PassGraph[0].Entry[1, 0, 0].Value = 'a' then LOther := 'b' else LOther := 'a';
    for I := 0 to 1 do LUnrelated[I] := LRestored.PassGraph[2].Entry[I, 0, 0].Value;
    LOriginal.PassGraph[0].SetAllowedValues(1, 0, 0, [LOther]);
    LRestored.PassGraph[0].SetAllowedValues(1, 0, 0, [LOther]);
    Check(LOriginal.TryRegenerateNegotiatedFrom('source', DefaultGraphNegotiationOptions, LSelective),
      'Original generated output remains negotiable');
    Check(LRestored.TryRegenerateNegotiatedFrom('source', DefaultGraphNegotiationOptions, LSelective),
      'Restored output remains negotiable instead of becoming caller locks');
    SameAssignments(LOriginal, LRestored);
    for I := 0 to 1 do
      Check(LRestored.PassGraph[2].Entry[I, 0, 0].Value = LUnrelated[I],
        'Regeneration preserves unrelated accepted output');
    Check(LRestored.PassGraph[0].Entry[0, 0, 0].Value = 'a', 'Regeneration preserves the genuine caller lock');

    for I := 0 to 8 do
    begin
      LBad := LOriginal.CaptureAssignments;
      case I of
        0: LBad.Version := 2;
        1: LBad.Passes[0].LabelText := 'wrong';
        2: LBad.Passes[0].Layout.Origin.X := 1;
        3: SetLength(LBad.Passes[0].Cells, 1);
        4: LBad.Passes[0].Cells[0].Value := 'unknown';
        5: LBad.Passes[0].Cells[0].Empty := True;
        6: begin
          LBad.Passes[0].Cells[0].Empty := True;
          LBad.Passes[0].Cells[0].Value := TGraphValue.Empty;
          LBad.Passes[0].Cells[0].Generated := True;
        end;
      end;
      LFresh := NewGraph;
      LRejected := False;
      try
        case I of
          7: LFresh.RestoreAssignments(LBad, 5, 10000);
          8: LFresh.RestoreAssignments(LBad, 100, 1);
          else LFresh.RestoreAssignments(LBad);
        end;
      except on Exception do LRejected := True end;
      Check(LRejected and AllEmpty(LFresh), 'Malformed or overbudget snapshot leaves the target empty');
      FreeAndNil(LFresh);
    end;
    LSnapshot := LOriginal.CaptureAssignments;
    LFresh := NewGraph;
    if LSnapshot.Passes[0].Cells[1].Value = 'a' then LOther := 'b' else LOther := 'a';
    LFresh.PassGraph[0].SetAllowedValues(1, 0, 0, [LOther]);
    LRejected := False;
    try LFresh.RestoreAssignments(LSnapshot);
    except on EArgumentException do LRejected := True end;
    Check(LRejected and AllEmpty(LFresh), 'Target domain conflict is not silently relaxed');
    FreeAndNil(LFresh);
    for I := 0 to 1 do
    begin
      LFresh := NewGraph;
      LObserver.Graph := LFresh;
      LObserver.Cancel := True;
      if I = 0 then LObserver.CancelPhase := wspPrepare else LObserver.CancelPhase := wspCommit;
      LRejected := False;
      try LFresh.RestoreAssignments(LSnapshot, 100, 10000, LObserver.Observe);
      except on EAbort do LRejected := True end;
      Check(LRejected and AllEmpty(LFresh) and not LFresh.Running,
        'Observer cancellation leaves a fresh target and releases the running guard');
      LFresh.RestoreAssignments(LSnapshot);
      SameAssignments(LOriginal, LFresh);
      FreeAndNil(LFresh);
    end;
    LObserver.Cancel := False;
    LRejected := False;
    try LRestored.RestoreAssignments(LSnapshot);
    except on EInvalidOperation do LRejected := True end;
    Check(LRejected, 'Nonempty target is rejected without overwriting accepted output');
    LRejected := False;
    try LSnapshot := LOriginal.CaptureAssignments(0);
    except on EArgumentException do LRejected := True end;
    Check(LRejected and not LOriginal.Running, 'Invalid capture budget releases the guard');
  finally
    LObserver.Free;
    LFresh.Free;
    LRestored.Free;
    LOriginal.Free;
  end;
end;

procedure LatticeRoundTrip;
var
  LSource: TGraph;
  LTarget: TGraph;
  LLayouts: TWfcLatticeLayouts;
  LBefore: TGraphAssignmentSnapshot;
  LAfter: TGraphAssignmentSnapshot;
  LReport: TGraphNegotiationReport;
  P, C: Integer;
begin
  LSource := TGraph.Create;
  LTarget := TGraph.Create;
  try
    for P := 0 to 1 do
    begin
      if P = 0 then
      begin
        LSource.CurrentPass := 'coarse';
        LTarget.CurrentPass := 'coarse';
      end
      else
      begin
        LSource.SwitchToPass('fine');
        LTarget.SwitchToPass('fine');
      end;
      LSource.PassMode := gpmOverlay;
      LTarget.PassMode := gpmOverlay;
      LSource.ClearDependencies;
      LTarget.ClearDependencies;
      LSource.AddValue('a'); LSource.AddValue('b');
      LTarget.AddValue('a'); LTarget.AddValue('b');
    end;
    SetLength(LLayouts, 2);
    LLayouts[0] := MakeWfcLatticeLayout(2, 1, 1, MakeWfcLatticeVector(0, 0, 0),
      MakeWfcLatticeVector(2, 2, 1), False);
    LLayouts[1] := MakeWfcLatticeLayout(4, 2, 1, MakeWfcLatticeVector(-1, 3, 0),
      MakeWfcLatticeVector(1, 1, 1), True);
    LSource.ConfigurePassLayouts(LLayouts);
    LTarget.ConfigurePassLayouts(LLayouts);
    LSource.Seed := 731;
    LTarget.Seed := 731;
    Check(LSource.TrySolveNegotiated(DefaultGraphNegotiationOptions, LReport), 'Unequal configured grids solve');
    LBefore := LSource.CaptureAssignments;
    LTarget.RestoreAssignments(LBefore);
    LAfter := LTarget.CaptureAssignments;
    for P := 0 to High(LBefore.Passes) do
    begin
      Check(SameWfcLatticeLayout(LBefore.Passes[P].Layout, LAfter.Passes[P].Layout),
        'Origins, pitches, wrapping and unequal extents are preserved');
      for C := 0 to High(LBefore.Passes[P].Cells) do
        Check((LBefore.Passes[P].Cells[C].Value = LAfter.Passes[P].Cells[C].Value) and
          (LBefore.Passes[P].Cells[C].Generated = LAfter.Passes[P].Cells[C].Generated),
          'Every unequal-grid assignment is retained');
    end;
  finally
    LTarget.Free;
    LSource.Free;
  end;
end;

begin
  Run;
  LatticeRoundTrip;
  WriteLn('Assignment snapshots: ', Checks, ' checks passed.');
end.
