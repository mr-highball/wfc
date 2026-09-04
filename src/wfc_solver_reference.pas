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
unit wfc_solver_reference;

{$mode delphi}{$H+}

interface

(*
  This is the string-free implementation kernel used by wfc.TGraph.TrySolve.
  Its declarations are installed so the wfc unit can be built by package
  managers, but they are not a stable application API. Use the wfc unit unless
  deliberately working on the solver implementation.
*)

const
  WFC_REFERENCE_DIRECTION_COUNT = 6;

type
  TReferenceIntegerArray = array of Integer;
  TReferenceByteArray = array of Byte;

  TReferenceContradictionKind = (
    rckNone,
    rckEmptyDomain,
    rckAdjacency,
    rckPreviousPass,
    rckRequiredSupport,
    rckFinalValidation
  );

  TReferenceContradictionKindArray =
    array of TReferenceContradictionKind;

  TReferenceSolveStatus = (
    rssSolved,
    rssContradiction,
    rssBacktrackLimit
  );

  TReferenceContradiction = record
    Kind: TReferenceContradictionKind;
    EntryIndex: Integer;
    NeighborIndex: Integer;
    Direction: Integer;
  end;

  TReferenceSolveReport = record
    Status: TReferenceSolveStatus;
    Decisions: Integer;
    Propagations: Integer;
    Contradictions: Integer;
    Backtracks: Integer;
    Contradiction: TReferenceContradiction;
  end;

  TReferenceRandomIndex = function(const ACount: Integer): Integer of object;

  (*
    A compact, string-free constraint model. Every matrix is flattened to
    keep native FPC and pas2js ownership and copy behavior identical.

    Compatibility is indexed by direction, current value, neighbor value.
    RequiredSupport uses the same layout and marks pairs that can activate a
    required-only current value. Bits for incompatible pairs are ignored.
  *)
  TReferenceModel = record
    CellCount: Integer;
    ValueCount: Integer;
    Neighbors: TReferenceIntegerArray;
    Compatibility: TReferenceByteArray;
    RequiredValues: TReferenceByteArray;
    RequiredSupport: TReferenceByteArray;
    InitialAllowed: TReferenceByteArray;
    InitialFailureKinds: TReferenceContradictionKindArray;
    LockedValues: TReferenceIntegerArray;
    CellOrder: TReferenceIntegerArray;
  end;

function SolveReferenceModel(const AModel: TReferenceModel;
  const AMaxBacktracks: Integer; const ARandomIndex: TReferenceRandomIndex;
  out AAssignment: TReferenceIntegerArray;
  out AReport: TReferenceSolveReport): Boolean;

implementation

uses
  Classes,
  SysUtils;

type
  TReferenceDecisionFrame = record
    CellIndex: Integer;
    TrailMark: Integer;
    NextAlternative: Integer;
    Alternatives: TReferenceIntegerArray;
  end;

  TReferenceDecisionFrames = array of TReferenceDecisionFrame;

  TReferenceRecoveryResult = (
    rrRetry,
    rrExhausted,
    rrLimit
  );

  TReferenceSolver = class
  strict private
    FModel: TReferenceModel;
    FMaxBacktracks: Integer;
    FRandomIndex: TReferenceRandomIndex;
    FDomains: TReferenceByteArray;
    FDomainCounts: TReferenceIntegerArray;
    FIncomingStarts: TReferenceIntegerArray;
    FIncomingArcs: TReferenceIntegerArray;
    FQueue: TReferenceIntegerArray;
    FInQueue: TReferenceByteArray;
    FQueueHead: Integer;
    FQueueTail: Integer;
    FQueueCount: Integer;
    FTrail: TReferenceIntegerArray;
    FTrailCount: Integer;
    FFrames: TReferenceDecisionFrames;
    FFrameCount: Integer;
    FReport: TReferenceSolveReport;

    function DomainIndex(const ACell, AValue: Integer): Integer; inline;
    function RelationIndex(const ADirection, ACurrentValue,
      ANeighborValue: Integer): Integer; inline;
    procedure RecordContradiction(const AKind: TReferenceContradictionKind;
      const AEntryIndex, ANeighborIndex, ADirection: Integer);
    procedure EnsureTrailCapacity;
    function RemoveCandidate(const ACell, AValue: Integer;
      const APropagation: Boolean): Boolean;
    procedure RestoreTrail(const AMark: Integer);
    procedure ResetQueue;
    procedure Enqueue(const ACell: Integer);
    function Dequeue(out ACell: Integer): Boolean;
    procedure BuildIncomingArcs;
    function InitializeDomains: Boolean;
    function ReviseArc(const ACell, ADirection: Integer): Boolean;
    function ReviseRequired(const ACell: Integer): Boolean;
    function Propagate: Boolean;
    function FindDecisionCell: Integer;
    procedure EnsureFrameCapacity;
    procedure TryFrameAlternative(const AFrameIndex: Integer);
    procedure PushDecision(const ACell: Integer);
    function Recover: TReferenceRecoveryResult;
    procedure ExtractAssignment(out AAssignment: TReferenceIntegerArray);
    function ValidateAssignment(const AAssignment: TReferenceIntegerArray;
      out AContradiction: TReferenceContradiction): Boolean;
  public
    constructor Create(const AModel: TReferenceModel;
      const AMaxBacktracks: Integer;
      const ARandomIndex: TReferenceRandomIndex);
    function Execute(out AAssignment: TReferenceIntegerArray;
      out AReport: TReferenceSolveReport): Boolean;
  end;

procedure IncrementCounter(var AValue: Integer); inline;
begin
  if AValue < High(Integer) then
    Inc(AValue);
end;

function SafeProduct(const A, B: Integer; const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) then
    raise ERangeError.Create(ALabel + ' cannot be negative');
  if (A <> 0) and (B > High(Integer) div A) then
    raise ERangeError.Create(ALabel + ' is too large');
  Result := A * B;
end;

procedure RequireLength(const AActual, AExpected: Integer;
  const ALabel: String);
begin
  if AActual <> AExpected then
    raise EInvalidOperation.CreateFmt(
      '%s has length %d; expected %d', [ALabel, AActual, AExpected]);
end;

procedure ValidateModel(const AModel: TReferenceModel);
var
  I: Integer;
  LCellValueCount: Integer;
  LRelationCount: Integer;
  LSeen: TReferenceByteArray;
begin
  if AModel.CellCount < 0 then
    raise ERangeError.Create('reference model cell count cannot be negative');
  if AModel.ValueCount < 0 then
    raise ERangeError.Create('reference model value count cannot be negative');
  if (AModel.CellCount > 0) and (AModel.ValueCount = 0) then
    raise EInvalidOperation.Create(
      'a nonempty reference model needs at least one value');

  LCellValueCount := SafeProduct(AModel.CellCount, AModel.ValueCount,
    'reference domain matrix');
  LRelationCount := SafeProduct(
    SafeProduct(AModel.ValueCount, AModel.ValueCount,
      'reference relation matrix'),
    WFC_REFERENCE_DIRECTION_COUNT, 'reference relation matrix');

  RequireLength(Length(AModel.Neighbors),
    SafeProduct(AModel.CellCount, WFC_REFERENCE_DIRECTION_COUNT,
      'reference neighbor matrix'), 'Neighbors');
  RequireLength(Length(AModel.Compatibility), LRelationCount,
    'Compatibility');
  RequireLength(Length(AModel.RequiredValues), AModel.ValueCount,
    'RequiredValues');
  RequireLength(Length(AModel.RequiredSupport), LRelationCount,
    'RequiredSupport');
  RequireLength(Length(AModel.InitialAllowed), LCellValueCount,
    'InitialAllowed');
  RequireLength(Length(AModel.InitialFailureKinds), AModel.CellCount,
    'InitialFailureKinds');
  RequireLength(Length(AModel.LockedValues), AModel.CellCount,
    'LockedValues');
  RequireLength(Length(AModel.CellOrder), AModel.CellCount,
    'CellOrder');

  for I := 0 to High(AModel.Neighbors) do
    if (AModel.Neighbors[I] < -1)
      or (AModel.Neighbors[I] >= AModel.CellCount) then
      raise ERangeError.CreateFmt(
        'reference neighbor index is out of bounds [%d]',
        [AModel.Neighbors[I]]);
  for I := 0 to High(AModel.LockedValues) do
    if (AModel.LockedValues[I] < -1)
      or (AModel.LockedValues[I] >= AModel.ValueCount) then
      raise ERangeError.CreateFmt(
        'reference lock value is out of bounds [%d]',
        [AModel.LockedValues[I]]);

  SetLength(LSeen, AModel.CellCount);
  for I := 0 to High(AModel.CellOrder) do
  begin
    if (AModel.CellOrder[I] < 0)
      or (AModel.CellOrder[I] >= AModel.CellCount) then
      raise ERangeError.CreateFmt(
        'reference cell order is out of bounds [%d]',
        [AModel.CellOrder[I]]);
    if LSeen[AModel.CellOrder[I]] <> 0 then
      raise EInvalidOperation.CreateFmt(
        'reference cell order repeats entry %d', [AModel.CellOrder[I]]);
    LSeen[AModel.CellOrder[I]] := 1;
  end;
end;

constructor TReferenceSolver.Create(const AModel: TReferenceModel;
  const AMaxBacktracks: Integer;
  const ARandomIndex: TReferenceRandomIndex);
begin
  inherited Create;
  FModel := AModel;
  FMaxBacktracks := AMaxBacktracks;
  FRandomIndex := ARandomIndex;
end;

function TReferenceSolver.DomainIndex(const ACell,
  AValue: Integer): Integer;
begin
  Result := (ACell * FModel.ValueCount) + AValue;
end;

function TReferenceSolver.RelationIndex(const ADirection,
  ACurrentValue, ANeighborValue: Integer): Integer;
begin
  Result := ((ADirection * FModel.ValueCount + ACurrentValue)
    * FModel.ValueCount) + ANeighborValue;
end;

procedure TReferenceSolver.RecordContradiction(
  const AKind: TReferenceContradictionKind; const AEntryIndex,
  ANeighborIndex, ADirection: Integer);
begin
  IncrementCounter(FReport.Contradictions);
  FReport.Contradiction.Kind := AKind;
  FReport.Contradiction.EntryIndex := AEntryIndex;
  FReport.Contradiction.NeighborIndex := ANeighborIndex;
  FReport.Contradiction.Direction := ADirection;
end;

procedure TReferenceSolver.EnsureTrailCapacity;
var
  LCapacity: Integer;
begin
  if FTrailCount < Length(FTrail) then
    Exit;
  LCapacity := Length(FTrail);
  if LCapacity < 64 then
    LCapacity := 64
  else if LCapacity > High(Integer) div 2 then
    raise ERangeError.Create('reference solver trail is too large')
  else
    LCapacity := LCapacity * 2;
  SetLength(FTrail, LCapacity);
end;

function TReferenceSolver.RemoveCandidate(const ACell, AValue: Integer;
  const APropagation: Boolean): Boolean;
var
  LIndex: Integer;
begin
  LIndex := DomainIndex(ACell, AValue);
  if FDomains[LIndex] = 0 then
    Exit(False);

  EnsureTrailCapacity;
  FTrail[FTrailCount] := LIndex;
  Inc(FTrailCount);
  FDomains[LIndex] := 0;
  Dec(FDomainCounts[ACell]);
  if APropagation then
    IncrementCounter(FReport.Propagations);
  Enqueue(ACell);
  Result := True;
end;

procedure TReferenceSolver.RestoreTrail(const AMark: Integer);
var
  LCell: Integer;
  LIndex: Integer;
begin
  while FTrailCount > AMark do
  begin
    Dec(FTrailCount);
    LIndex := FTrail[FTrailCount];
    if FDomains[LIndex] = 0 then
    begin
      FDomains[LIndex] := 1;
      LCell := LIndex div FModel.ValueCount;
      Inc(FDomainCounts[LCell]);
    end;
  end;
end;

procedure TReferenceSolver.ResetQueue;
var
  I: Integer;
begin
  FQueueHead := 0;
  FQueueTail := 0;
  FQueueCount := 0;
  for I := 0 to High(FInQueue) do
    FInQueue[I] := 0;
end;

procedure TReferenceSolver.Enqueue(const ACell: Integer);
begin
  if (ACell < 0) or (FModel.CellCount = 0)
    or (FInQueue[ACell] <> 0) then
    Exit;
  FQueue[FQueueTail] := ACell;
  Inc(FQueueTail);
  if FQueueTail = FModel.CellCount then
    FQueueTail := 0;
  Inc(FQueueCount);
  FInQueue[ACell] := 1;
end;

function TReferenceSolver.Dequeue(out ACell: Integer): Boolean;
begin
  if FQueueCount = 0 then
    Exit(False);
  ACell := FQueue[FQueueHead];
  Inc(FQueueHead);
  if FQueueHead = FModel.CellCount then
    FQueueHead := 0;
  Dec(FQueueCount);
  FInQueue[ACell] := 0;
  Result := True;
end;

procedure TReferenceSolver.BuildIncomingArcs;
var
  I: Integer;
  LArc: Integer;
  LCounts: TReferenceIntegerArray;
  LCursor: TReferenceIntegerArray;
  LNeighbor: Integer;
  LTotal: Integer;
begin
  SetLength(LCounts, FModel.CellCount);
  LTotal := 0;
  for LArc := 0 to High(FModel.Neighbors) do
  begin
    LNeighbor := FModel.Neighbors[LArc];
    if LNeighbor >= 0 then
    begin
      Inc(LCounts[LNeighbor]);
      Inc(LTotal);
    end;
  end;

  SetLength(FIncomingStarts, FModel.CellCount + 1);
  for I := 0 to Pred(FModel.CellCount) do
    FIncomingStarts[I + 1] := FIncomingStarts[I] + LCounts[I];
  SetLength(FIncomingArcs, LTotal);
  SetLength(LCursor, FModel.CellCount);
  for I := 0 to Pred(FModel.CellCount) do
    LCursor[I] := FIncomingStarts[I];

  for LArc := 0 to High(FModel.Neighbors) do
  begin
    LNeighbor := FModel.Neighbors[LArc];
    if LNeighbor >= 0 then
    begin
      FIncomingArcs[LCursor[LNeighbor]] := LArc;
      Inc(LCursor[LNeighbor]);
    end;
  end;
end;

function TReferenceSolver.InitializeDomains: Boolean;
var
  LCell: Integer;
  LIndex: Integer;
  LKind: TReferenceContradictionKind;
  LValue: Integer;
begin
  SetLength(FDomains, SafeProduct(FModel.CellCount, FModel.ValueCount,
    'reference domains'));
  SetLength(FDomainCounts, FModel.CellCount);
  SetLength(FQueue, FModel.CellCount);
  SetLength(FInQueue, FModel.CellCount);
  SetLength(FTrail, 0);
  FTrailCount := 0;
  SetLength(FFrames, 0);
  FFrameCount := 0;
  ResetQueue;

  for LCell := 0 to Pred(FModel.CellCount) do
  begin
    for LValue := 0 to Pred(FModel.ValueCount) do
    begin
      LIndex := DomainIndex(LCell, LValue);
      if (FModel.InitialAllowed[LIndex] <> 0)
        and ((FModel.LockedValues[LCell] < 0)
          or (FModel.LockedValues[LCell] = LValue)) then
      begin
        FDomains[LIndex] := 1;
        Inc(FDomainCounts[LCell]);
      end;
    end;

    if FDomainCounts[LCell] = 0 then
    begin
      LKind := FModel.InitialFailureKinds[LCell];
      if LKind = rckNone then
        LKind := rckEmptyDomain;
      RecordContradiction(LKind, LCell, -1, -1);
      Exit(False);
    end;
  end;
  Result := True;
end;

function TReferenceSolver.ReviseArc(const ACell,
  ADirection: Integer): Boolean;
var
  LNeighbor: Integer;
  LNeighborValue: Integer;
  LSupported: Boolean;
  LValue: Integer;
begin
  LNeighbor := FModel.Neighbors[
    (ACell * WFC_REFERENCE_DIRECTION_COUNT) + ADirection];
  if LNeighbor < 0 then
    Exit(True);

  for LValue := 0 to Pred(FModel.ValueCount) do
  begin
    if FDomains[DomainIndex(ACell, LValue)] = 0 then
      Continue;

    LSupported := False;
    if LNeighbor = ACell then
      LSupported := FModel.Compatibility[
        RelationIndex(ADirection, LValue, LValue)] <> 0
    else
      for LNeighborValue := 0 to Pred(FModel.ValueCount) do
        if (FDomains[DomainIndex(LNeighbor, LNeighborValue)] <> 0)
          and (FModel.Compatibility[
            RelationIndex(ADirection, LValue, LNeighborValue)] <> 0) then
        begin
          LSupported := True;
          Break;
        end;

    if not LSupported then
    begin
      RemoveCandidate(ACell, LValue, True);
      if FDomainCounts[ACell] = 0 then
      begin
        RecordContradiction(rckAdjacency, ACell, LNeighbor, ADirection);
        Exit(False);
      end;
    end;
  end;
  Result := True;
end;

function TReferenceSolver.ReviseRequired(const ACell: Integer): Boolean;
var
  LDirection: Integer;
  LNeighbor: Integer;
  LNeighborValue: Integer;
  LSupported: Boolean;
  LValue: Integer;
begin
  if FModel.LockedValues[ACell] >= 0 then
    Exit(True);

  for LValue := 0 to Pred(FModel.ValueCount) do
  begin
    if (FDomains[DomainIndex(ACell, LValue)] = 0)
      or (FModel.RequiredValues[LValue] = 0) then
      Continue;

    LSupported := False;
    for LDirection := 0 to Pred(WFC_REFERENCE_DIRECTION_COUNT) do
    begin
      LNeighbor := FModel.Neighbors[
        (ACell * WFC_REFERENCE_DIRECTION_COUNT) + LDirection];
      if LNeighbor < 0 then
        Continue;

      if LNeighbor = ACell then
        LSupported := (FModel.RequiredSupport[
          RelationIndex(LDirection, LValue, LValue)] <> 0)
          and (FModel.Compatibility[
            RelationIndex(LDirection, LValue, LValue)] <> 0)
      else
        for LNeighborValue := 0 to Pred(FModel.ValueCount) do
          if (FDomains[DomainIndex(LNeighbor, LNeighborValue)] <> 0)
            and (FModel.RequiredSupport[
              RelationIndex(LDirection, LValue, LNeighborValue)] <> 0)
            and (FModel.Compatibility[
              RelationIndex(LDirection, LValue, LNeighborValue)] <> 0) then
          begin
            LSupported := True;
            Break;
          end;
      if LSupported then
        Break;
    end;

    if not LSupported then
    begin
      RemoveCandidate(ACell, LValue, True);
      if FDomainCounts[ACell] = 0 then
      begin
        RecordContradiction(rckRequiredSupport, ACell, -1, -1);
        Exit(False);
      end;
    end;
  end;
  Result := True;
end;

function TReferenceSolver.Propagate: Boolean;
var
  LArc: Integer;
  LChangedCell: Integer;
  LDirection: Integer;
  LIncomingIndex: Integer;
  LSourceCell: Integer;
begin
  while Dequeue(LChangedCell) do
    for LIncomingIndex := FIncomingStarts[LChangedCell]
      to Pred(FIncomingStarts[LChangedCell + 1]) do
    begin
      LArc := FIncomingArcs[LIncomingIndex];
      LSourceCell := LArc div WFC_REFERENCE_DIRECTION_COUNT;
      LDirection := LArc mod WFC_REFERENCE_DIRECTION_COUNT;
      if not ReviseArc(LSourceCell, LDirection) then
        Exit(False);
      if not ReviseRequired(LSourceCell) then
        Exit(False);
    end;
  Result := True;
end;

function TReferenceSolver.FindDecisionCell: Integer;
var
  I: Integer;
  LCell: Integer;
  LCount: Integer;
  LMinimum: Integer;
begin
  Result := -1;
  LMinimum := High(Integer);
  for I := 0 to Pred(FModel.CellCount) do
  begin
    LCell := FModel.CellOrder[I];
    LCount := FDomainCounts[LCell];
    if (LCount > 1) and (LCount < LMinimum) then
    begin
      Result := LCell;
      LMinimum := LCount;
    end;
  end;
end;

procedure TReferenceSolver.EnsureFrameCapacity;
var
  LCapacity: Integer;
begin
  if FFrameCount < Length(FFrames) then
    Exit;
  LCapacity := Length(FFrames);
  if LCapacity < 16 then
    LCapacity := 16
  else if LCapacity > High(Integer) div 2 then
    raise ERangeError.Create('reference solver decision stack is too large')
  else
    LCapacity := LCapacity * 2;
  SetLength(FFrames, LCapacity);
end;

procedure TReferenceSolver.TryFrameAlternative(const AFrameIndex: Integer);
var
  LChosen: Integer;
  LFrame: TReferenceDecisionFrame;
  LValue: Integer;
begin
  LFrame := FFrames[AFrameIndex];
  LChosen := LFrame.Alternatives[LFrame.NextAlternative];
  Inc(LFrame.NextAlternative);
  FFrames[AFrameIndex] := LFrame;
  IncrementCounter(FReport.Decisions);

  for LValue := 0 to Pred(FModel.ValueCount) do
    if (LValue <> LChosen)
      and (FDomains[DomainIndex(LFrame.CellIndex, LValue)] <> 0) then
      RemoveCandidate(LFrame.CellIndex, LValue, False);
end;

procedure TReferenceSolver.PushDecision(const ACell: Integer);
var
  I: Integer;
  LCount: Integer;
  LFrame: TReferenceDecisionFrame;
  LStart: Integer;
  LValues: TReferenceIntegerArray;
  LValue: Integer;
begin
  LCount := FDomainCounts[ACell];
  SetLength(LValues, LCount);
  I := 0;
  for LValue := 0 to Pred(FModel.ValueCount) do
    if FDomains[DomainIndex(ACell, LValue)] <> 0 then
    begin
      LValues[I] := LValue;
      Inc(I);
    end;

  if Assigned(FRandomIndex) then
    LStart := FRandomIndex(LCount)
  else
    LStart := 0;
  if (LStart < 0) or (LStart >= LCount) then
    raise ERangeError.CreateFmt(
      'reference random index is out of bounds [%d of %d]',
      [LStart, LCount]);

  LFrame.CellIndex := ACell;
  LFrame.TrailMark := FTrailCount;
  LFrame.NextAlternative := 0;
  SetLength(LFrame.Alternatives, LCount);
  for I := 0 to Pred(LCount) do
    LFrame.Alternatives[I] := LValues[(LStart + I) mod LCount];

  EnsureFrameCapacity;
  FFrames[FFrameCount] := LFrame;
  Inc(FFrameCount);
  TryFrameAlternative(Pred(FFrameCount));
end;

function TReferenceSolver.Recover: TReferenceRecoveryResult;
var
  LFrameIndex: Integer;
begin
  while FFrameCount > 0 do
  begin
    if FReport.Backtracks >= FMaxBacktracks then
      Exit(rrLimit);
    IncrementCounter(FReport.Backtracks);

    LFrameIndex := Pred(FFrameCount);
    RestoreTrail(FFrames[LFrameIndex].TrailMark);
    ResetQueue;
    if FFrames[LFrameIndex].NextAlternative
      < Length(FFrames[LFrameIndex].Alternatives) then
    begin
      TryFrameAlternative(LFrameIndex);
      Exit(rrRetry);
    end;

    FFrames[LFrameIndex].Alternatives := nil;
    Dec(FFrameCount);
  end;
  Result := rrExhausted;
end;

procedure TReferenceSolver.ExtractAssignment(
  out AAssignment: TReferenceIntegerArray);
var
  LCell: Integer;
  LValue: Integer;
begin
  SetLength(AAssignment, FModel.CellCount);
  for LCell := 0 to Pred(FModel.CellCount) do
  begin
    AAssignment[LCell] := -1;
    for LValue := 0 to Pred(FModel.ValueCount) do
      if FDomains[DomainIndex(LCell, LValue)] <> 0 then
      begin
        AAssignment[LCell] := LValue;
        Break;
      end;
  end;
end;

function TReferenceSolver.ValidateAssignment(
  const AAssignment: TReferenceIntegerArray;
  out AContradiction: TReferenceContradiction): Boolean;
var
  LCell: Integer;
  LDirection: Integer;
  LNeighbor: Integer;
  LNeighborValue: Integer;
  LOrderIndex: Integer;
  LSupported: Boolean;
  LValue: Integer;
begin
  AContradiction.Kind := rckNone;
  AContradiction.EntryIndex := -1;
  AContradiction.NeighborIndex := -1;
  AContradiction.Direction := -1;
  if Length(AAssignment) <> FModel.CellCount then
    Exit(False);

  for LOrderIndex := 0 to Pred(FModel.CellCount) do
  begin
    LCell := FModel.CellOrder[LOrderIndex];
    LValue := AAssignment[LCell];
    if (LValue < 0) or (LValue >= FModel.ValueCount)
      or (FDomainCounts[LCell] <> 1)
      or (FModel.InitialAllowed[DomainIndex(LCell, LValue)] = 0)
      or ((FModel.LockedValues[LCell] >= 0)
        and (FModel.LockedValues[LCell] <> LValue)) then
    begin
      AContradiction.Kind := rckFinalValidation;
      AContradiction.EntryIndex := LCell;
      Exit(False);
    end;

    for LDirection := 0 to Pred(WFC_REFERENCE_DIRECTION_COUNT) do
    begin
      LNeighbor := FModel.Neighbors[
        (LCell * WFC_REFERENCE_DIRECTION_COUNT) + LDirection];
      if LNeighbor < 0 then
        Continue;
      LNeighborValue := AAssignment[LNeighbor];
      if (LNeighborValue < 0)
        or (FModel.Compatibility[
          RelationIndex(LDirection, LValue, LNeighborValue)] = 0) then
      begin
        AContradiction.Kind := rckFinalValidation;
        AContradiction.EntryIndex := LCell;
        AContradiction.NeighborIndex := LNeighbor;
        AContradiction.Direction := LDirection;
        Exit(False);
      end;
    end;

    if (FModel.RequiredValues[LValue] <> 0)
      and (FModel.LockedValues[LCell] < 0) then
    begin
      LSupported := False;
      for LDirection := 0 to Pred(WFC_REFERENCE_DIRECTION_COUNT) do
      begin
        LNeighbor := FModel.Neighbors[
          (LCell * WFC_REFERENCE_DIRECTION_COUNT) + LDirection];
        if LNeighbor < 0 then
          Continue;
        LNeighborValue := AAssignment[LNeighbor];
        if (LNeighborValue >= 0)
          and (FModel.RequiredSupport[
            RelationIndex(LDirection, LValue, LNeighborValue)] <> 0)
          and (FModel.Compatibility[
            RelationIndex(LDirection, LValue, LNeighborValue)] <> 0) then
        begin
          LSupported := True;
          Break;
        end;
      end;
      if not LSupported then
      begin
        AContradiction.Kind := rckFinalValidation;
        AContradiction.EntryIndex := LCell;
        Exit(False);
      end;
    end;
  end;
  Result := True;
end;

function TReferenceSolver.Execute(out AAssignment: TReferenceIntegerArray;
  out AReport: TReferenceSolveReport): Boolean;
var
  I: Integer;
  LCell: Integer;
  LFinalContradiction: TReferenceContradiction;
  LRecovery: TReferenceRecoveryResult;
begin
  AAssignment := nil;
  FReport.Status := rssContradiction;
  FReport.Decisions := 0;
  FReport.Propagations := 0;
  FReport.Contradictions := 0;
  FReport.Backtracks := 0;
  FReport.Contradiction.Kind := rckNone;
  FReport.Contradiction.EntryIndex := -1;
  FReport.Contradiction.NeighborIndex := -1;
  FReport.Contradiction.Direction := -1;

  if not InitializeDomains then
  begin
    AReport := FReport;
    Exit(False);
  end;
  BuildIncomingArcs;

  //Required-only values with no possible supporter must disappear even in a
  //graph with no incoming adjacency arcs.
  for I := 0 to Pred(FModel.CellCount) do
  begin
    LCell := FModel.CellOrder[I];
    if not ReviseRequired(LCell) then
    begin
      AReport := FReport;
      Exit(False);
    end;
  end;
  for I := 0 to Pred(FModel.CellCount) do
    Enqueue(FModel.CellOrder[I]);

  while True do
  begin
    if not Propagate then
    begin
      LRecovery := Recover;
      case LRecovery of
        rrRetry:
          Continue;
        rrLimit:
          begin
            FReport.Status := rssBacktrackLimit;
            AReport := FReport;
            Exit(False);
          end;
        rrExhausted:
          begin
            FReport.Status := rssContradiction;
            AReport := FReport;
            Exit(False);
          end;
      end;
    end;

    LCell := FindDecisionCell;
    if LCell >= 0 then
    begin
      PushDecision(LCell);
      Continue;
    end;

    ExtractAssignment(AAssignment);
    if not ValidateAssignment(AAssignment, LFinalContradiction) then
    begin
      RecordContradiction(rckFinalValidation,
        LFinalContradiction.EntryIndex,
        LFinalContradiction.NeighborIndex,
        LFinalContradiction.Direction);
      AAssignment := nil;
      FReport.Status := rssContradiction;
      AReport := FReport;
      Exit(False);
    end;

    FReport.Status := rssSolved;
    FReport.Contradiction.Kind := rckNone;
    FReport.Contradiction.EntryIndex := -1;
    FReport.Contradiction.NeighborIndex := -1;
    FReport.Contradiction.Direction := -1;
    AReport := FReport;
    Exit(True);
  end;
end;

function SolveReferenceModel(const AModel: TReferenceModel;
  const AMaxBacktracks: Integer; const ARandomIndex: TReferenceRandomIndex;
  out AAssignment: TReferenceIntegerArray;
  out AReport: TReferenceSolveReport): Boolean;
var
  LSolver: TReferenceSolver;
begin
  if AMaxBacktracks < 0 then
    raise ERangeError.CreateFmt(
      'maximum backtracks cannot be negative [%d]', [AMaxBacktracks]);
  ValidateModel(AModel);
  LSolver := TReferenceSolver.Create(AModel, AMaxBacktracks, ARandomIndex);
  try
    Result := LSolver.Execute(AAssignment, AReport);
  finally
    LSolver.Free;
  end;
end;

end.
