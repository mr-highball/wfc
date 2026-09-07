{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
unit wfc_lattice;
{$mode delphi}{$H+}

interface

uses SysUtils;

const WFC_LATTICE_VERSION = 1;

type
  EWfcLattice = class(Exception);
  TWfcLatticeVector = record
    X, Y, Z: Integer;
  end;
  TWfcLatticeLayout = record
    Cells, Origin, Pitch: TWfcLatticeVector;
    Wrap: Boolean;
  end;
  TWfcLatticeLayouts = array of TWfcLatticeLayout;
  TWfcLatticeBox = record
    Minimum, Maximum: TWfcLatticeVector;
  end;
  TWfcLatticeInterval = record
    First, Last: Integer;
  end;
  TWfcLatticeAxisCoverage = record
    IntervalCount: Integer;
    Intervals: array[0..1] of TWfcLatticeInterval;
    Count: Integer;
  end;
  { Sorted, disjoint, nonadjacent inclusive intervals on each axis. Unused
    interval slots are zero. Default(TWfcLatticeCoverage) is canonical empty
    coverage; every other valid coverage has all three axes nonempty.
    This record owns no buffers, even when it covers billions of cells. }
  TWfcLatticeCoverage = record
    X, Y, Z: TWfcLatticeAxisCoverage;
  end;

function MakeWfcLatticeVector(const AX, AY, AZ: Integer): TWfcLatticeVector;
function MakeWfcLatticeLayout(const AWidth, AHeight, ADepth: Integer;
  const AOrigin, APitch: TWfcLatticeVector; const AWrap: Boolean):
  TWfcLatticeLayout; overload;
function MakeWfcLatticeLayout(const AWidth, AHeight, ADepth: Integer;
  const AWrap: Boolean): TWfcLatticeLayout; overload;
procedure ValidateWfcLatticeLayout(const ALayout: TWfcLatticeLayout);
function WfcLatticeCellCount(const ALayout: TWfcLatticeLayout): Integer;
function SameWfcLatticeLayout(const A, B: TWfcLatticeLayout): Boolean;
function WfcLatticeCellBox(const ALayout: TWfcLatticeLayout;
  const ACell: TWfcLatticeVector): TWfcLatticeBox;
{ Bounded points outside the half-open extent return False. Wrapped points
  resolve periodically. A false result clears ACell to the zero vector. }
function TryWfcLatticePoint(const ALayout: TWfcLatticeLayout;
  const AWorld: TWfcLatticeVector; out ACell: TWfcLatticeVector): Boolean;
{ ABox must have positive span on all axes. A bounded query must fit in its
  entirety; otherwise False and canonical empty coverage are returned.
  Wrapped coverage visits each intersected provider cell exactly once, even
  across many periods. World endpoints are signed 32-bit Integers. }
function TryWfcLatticeCoverage(const ALayout: TWfcLatticeLayout;
  const ABox: TWfcLatticeBox; out ACoverage: TWfcLatticeCoverage): Boolean;
function WfcLatticeCoverageCellCount(const ACoverage: TWfcLatticeCoverage): Integer;
{ Zero-based ordinal in canonical XYZ flat order: X fastest, then Y, then Z.
  No footprint-sized array is allocated or enumerated to resolve an ordinal. }
function WfcLatticeCoverageCell(const ACoverage: TWfcLatticeCoverage;
  const AOrdinal: Integer): TWfcLatticeVector;

implementation

procedure RequireInteger(const AValue, AMinimum, AMaximum: Integer;
  const AName: String);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid = typeof AValue === 'number' && Number.isFinite(AValue) && Math.floor(AValue) === AValue; end;
  if not Valid then raise EWfcLattice.Create(AName + ' must be a finite Integer');
  {$ENDIF}
  if (AValue < AMinimum) or (AValue > AMaximum) then
    raise EWfcLattice.Create(AName + ' is out of range');
end;

procedure ValidateVector(const A: TWfcLatticeVector; const AMinimum: Integer;
  const AName: String);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid = A !== null && typeof A === 'object' && !Array.isArray(A); end;
  if not Valid then raise EWfcLattice.Create(AName + ' must be a vector');
  {$ENDIF}
  RequireInteger(A.X, AMinimum, High(Integer), AName + '.X');
  RequireInteger(A.Y, AMinimum, High(Integer), AName + '.Y');
  RequireInteger(A.Z, AMinimum, High(Integer), AName + '.Z');
end;

function CheckedProduct(const A, B: Integer): Integer;
begin
  { Callers have already validated positive operands. }
  if A > High(Integer) div B then
    raise EWfcLattice.Create('lattice cell count exceeds Integer capacity');
  Result := A * B;
end;

function Wide(const A: Integer): Double; inline;
begin Result := A; end;

function AxisEnd(const ACells, AOrigin, APitch: Integer): Double;
var Capacity: Double;
begin
  { Never form Cells*Pitch before this preflight: two valid Integer operands
    could otherwise produce an inexact 62-bit JavaScript product. The allowed
    span is at most 2^32-1, not High(Integer): negative origins are useful. }
  Capacity := Wide(High(Integer)) - Wide(AOrigin);
  if Wide(ACells) > Capacity / Wide(APitch) then
    raise EWfcLattice.Create('lattice world endpoint exceeds Integer capacity');
  Result := Wide(AOrigin) + Wide(ACells) * Wide(APitch);
end;

procedure ValidateWfcLatticeLayout(const ALayout: TWfcLatticeLayout);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid = ALayout !== null && typeof ALayout === 'object' && !Array.isArray(ALayout); end;
  if not Valid then raise EWfcLattice.Create('layout must be a record');
  asm Valid = typeof ALayout.Wrap === 'boolean'; end;
  if not Valid then raise EWfcLattice.Create('layout wrap must be Boolean');
  {$ENDIF}
  ValidateVector(ALayout.Cells, 1, 'cells');
  ValidateVector(ALayout.Origin, Low(Integer), 'origin');
  ValidateVector(ALayout.Pitch, 1, 'pitch');
  CheckedProduct(CheckedProduct(ALayout.Cells.X, ALayout.Cells.Y), ALayout.Cells.Z);
  AxisEnd(ALayout.Cells.X, ALayout.Origin.X, ALayout.Pitch.X);
  AxisEnd(ALayout.Cells.Y, ALayout.Origin.Y, ALayout.Pitch.Y);
  AxisEnd(ALayout.Cells.Z, ALayout.Origin.Z, ALayout.Pitch.Z);
end;

function MakeWfcLatticeVector(const AX, AY, AZ: Integer): TWfcLatticeVector;
begin
  Result.X := AX; Result.Y := AY; Result.Z := AZ;
  ValidateVector(Result, Low(Integer), 'vector');
end;

function MakeWfcLatticeLayout(const AWidth, AHeight, ADepth: Integer;
  const AOrigin, APitch: TWfcLatticeVector; const AWrap: Boolean): TWfcLatticeLayout;
begin
  ValidateVector(AOrigin, Low(Integer), 'origin');
  ValidateVector(APitch, 1, 'pitch');
  Result.Cells := MakeWfcLatticeVector(AWidth, AHeight, ADepth);
  Result.Origin := AOrigin; Result.Pitch := APitch; Result.Wrap := AWrap;
  ValidateWfcLatticeLayout(Result);
end;

function MakeWfcLatticeLayout(const AWidth, AHeight, ADepth: Integer;
  const AWrap: Boolean): TWfcLatticeLayout;
begin
  Result := MakeWfcLatticeLayout(AWidth, AHeight, ADepth,
    MakeWfcLatticeVector(0,0,0), MakeWfcLatticeVector(1,1,1), AWrap);
end;

function WfcLatticeCellCount(const ALayout: TWfcLatticeLayout): Integer;
begin
  ValidateWfcLatticeLayout(ALayout);
  Result := ALayout.Cells.X * ALayout.Cells.Y * ALayout.Cells.Z;
end;

function EqualVector(const A, B: TWfcLatticeVector): Boolean;
begin Result := (A.X=B.X) and (A.Y=B.Y) and (A.Z=B.Z); end;

function SameWfcLatticeLayout(const A, B: TWfcLatticeLayout): Boolean;
begin
  ValidateWfcLatticeLayout(A); ValidateWfcLatticeLayout(B);
  Result := EqualVector(A.Cells,B.Cells) and EqualVector(A.Origin,B.Origin)
    and EqualVector(A.Pitch,B.Pitch) and (A.Wrap=B.Wrap);
end;

function WfcLatticeCellBox(const ALayout: TWfcLatticeLayout;
  const ACell: TWfcLatticeVector): TWfcLatticeBox;
begin
  ValidateWfcLatticeLayout(ALayout);
  ValidateVector(ACell, 0, 'cell');
  if (ACell.X>=ALayout.Cells.X) or (ACell.Y>=ALayout.Cells.Y) or
    (ACell.Z>=ALayout.Cells.Z) then raise EWfcLattice.Create('cell is outside the lattice');
  { The layout's endpoint preflight proves these exact products fit in at
    most 32 unsigned bits. The sums and final casts fit signed Integer. }
  Result.Minimum.X := Integer(Trunc(Wide(ALayout.Origin.X)+Wide(ACell.X)*Wide(ALayout.Pitch.X)));
  Result.Minimum.Y := Integer(Trunc(Wide(ALayout.Origin.Y)+Wide(ACell.Y)*Wide(ALayout.Pitch.Y)));
  Result.Minimum.Z := Integer(Trunc(Wide(ALayout.Origin.Z)+Wide(ACell.Z)*Wide(ALayout.Pitch.Z)));
  Result.Maximum.X := Integer(Trunc(Wide(Result.Minimum.X)+Wide(ALayout.Pitch.X)));
  Result.Maximum.Y := Integer(Trunc(Wide(Result.Minimum.Y)+Wide(ALayout.Pitch.Y)));
  Result.Maximum.Z := Integer(Trunc(Wide(Result.Minimum.Z)+Wide(ALayout.Pitch.Z)));
end;

function FloorQuotient(const A, B: Double): Double;
begin
  { A is an exact signed integer with magnitude <=2^32; B is positive.
    Trunc gives the toward-zero quotient. Correct negative nonmultiples
    explicitly without narrowing an intermediate quotient to Integer. }
  Result := Trunc(A / B);
  if Result * B > A then Result := Result - 1;
end;

function PositiveRemainder(const A, B: Double): Double;
begin Result := A - FloorQuotient(A,B)*B; end;

function PointAxis(const ACells, AOrigin, APitch, AWorld: Integer;
  const AWrap: Boolean; out ACell: Integer): Boolean;
var Index: Double;
begin
  Index := FloorQuotient(Wide(AWorld)-Wide(AOrigin),Wide(APitch));
  if AWrap then Index := PositiveRemainder(Index,Wide(ACells))
  else if (Index<0) or (Index>=ACells) then Exit(False);
  ACell := Integer(Trunc(Index)); Result := True;
end;

function TryWfcLatticePoint(const ALayout: TWfcLatticeLayout;
  const AWorld: TWfcLatticeVector; out ACell: TWfcLatticeVector): Boolean;
var Cell, World: TWfcLatticeVector; Layout: TWfcLatticeLayout;
begin
  ValidateWfcLatticeLayout(ALayout);
  ValidateVector(AWorld, Low(Integer), 'world point');
  { Const input and out output may name the same caller record. Snapshot
    before clearing the output, including when it aliases a layout vector. }
  Layout := ALayout; World := AWorld;
  ACell := Default(TWfcLatticeVector);
  Cell := Default(TWfcLatticeVector);
  Result := PointAxis(Layout.Cells.X,Layout.Origin.X,Layout.Pitch.X,World.X,Layout.Wrap,Cell.X)
    and PointAxis(Layout.Cells.Y,Layout.Origin.Y,Layout.Pitch.Y,World.Y,Layout.Wrap,Cell.Y)
    and PointAxis(Layout.Cells.Z,Layout.Origin.Z,Layout.Pitch.Z,World.Z,Layout.Wrap,Cell.Z);
  if Result then ACell := Cell;
end;

procedure ValidateBox(const ABox: TWfcLatticeBox);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid = ABox !== null && typeof ABox === 'object' && !Array.isArray(ABox); end;
  if not Valid then raise EWfcLattice.Create('box must be a record');
  {$ENDIF}
  ValidateVector(ABox.Minimum, Low(Integer), 'box minimum');
  ValidateVector(ABox.Maximum, Low(Integer), 'box maximum');
  if (ABox.Minimum.X>=ABox.Maximum.X) or (ABox.Minimum.Y>=ABox.Maximum.Y) or
    (ABox.Minimum.Z>=ABox.Maximum.Z) then raise EWfcLattice.Create('box spans must be positive');
end;

function CoverageAxis(const ACells, AOrigin, APitch, AMinimum, AMaximum: Integer;
  const AWrap: Boolean; out A: TWfcLatticeAxisCoverage): Boolean;
var First, Last, Count, Start, Finish: Double;
begin
  A := Default(TWfcLatticeAxisCoverage);
  if not AWrap then
    if (AMinimum<AOrigin) or (Wide(AMaximum)>AxisEnd(ACells,AOrigin,APitch)) then Exit(False);
  First := FloorQuotient(Wide(AMinimum)-Wide(AOrigin),Wide(APitch));
  { Integer half-open endpoints: Maximum-1 is the final world tick whose
    containing cell intersects the box. Subtract in Double before narrowing. }
  Last := FloorQuotient(Wide(AMaximum)-1-Wide(AOrigin),Wide(APitch));
  Count := Last-First+1;
  A.IntervalCount := 1;
  if AWrap and (Count>=ACells) then
  begin A.Count:=ACells; A.Intervals[0].Last:=ACells-1; Exit(True); end;
  A.Count := Integer(Trunc(Count));
  if AWrap then Start := PositiveRemainder(First,Wide(ACells)) else Start := First;
  Finish := Start+Count-1;
  if Finish<ACells then
  begin
    A.Intervals[0].First := Integer(Trunc(Start));
    A.Intervals[0].Last := Integer(Trunc(Finish));
  end
  else
  begin
    A.IntervalCount := 2;
    A.Intervals[0].Last := Integer(Trunc(Finish-ACells));
    A.Intervals[1].First := Integer(Trunc(Start));
    A.Intervals[1].Last := ACells-1;
  end;
  Result := True;
end;

function TryWfcLatticeCoverage(const ALayout: TWfcLatticeLayout;
  const ABox: TWfcLatticeBox; out ACoverage: TWfcLatticeCoverage): Boolean;
var Coverage: TWfcLatticeCoverage;
begin
  ACoverage := Default(TWfcLatticeCoverage);
  ValidateWfcLatticeLayout(ALayout); ValidateBox(ABox);
  Coverage := Default(TWfcLatticeCoverage);
  Result := CoverageAxis(ALayout.Cells.X,ALayout.Origin.X,ALayout.Pitch.X,ABox.Minimum.X,ABox.Maximum.X,ALayout.Wrap,Coverage.X)
    and CoverageAxis(ALayout.Cells.Y,ALayout.Origin.Y,ALayout.Pitch.Y,ABox.Minimum.Y,ABox.Maximum.Y,ALayout.Wrap,Coverage.Y)
    and CoverageAxis(ALayout.Cells.Z,ALayout.Origin.Z,ALayout.Pitch.Z,ABox.Minimum.Z,ABox.Maximum.Z,ALayout.Wrap,Coverage.Z);
  if Result then ACoverage := Coverage;
end;

function ValidateCoverageAxis(const A: TWfcLatticeAxisCoverage): Integer;
var I, Part, Count: Integer;
{$IFDEF PAS2JS}Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid = A !== null && typeof A === 'object' && !Array.isArray(A) &&
    Array.isArray(A.Intervals) && A.Intervals.length === 2 &&
    [0,1].every(function(i) { var v=A.Intervals[i];
      return v !== null && typeof v === 'object' && !Array.isArray(v); }); end;
  if not Valid then raise EWfcLattice.Create('coverage axis must have two interval records');
  {$ENDIF}
  RequireInteger(A.IntervalCount,0,2,'coverage interval count');
  RequireInteger(A.Count,0,High(Integer),'coverage axis count');
  Count := 0;
  for I:=0 to 1 do
  begin
    RequireInteger(A.Intervals[I].First,0,High(Integer)-1,'coverage interval first');
    RequireInteger(A.Intervals[I].Last,0,High(Integer)-1,'coverage interval last');
    if I>=A.IntervalCount then
    begin
      if (A.Intervals[I].First<>0) or (A.Intervals[I].Last<>0) then
        raise EWfcLattice.Create('unused coverage interval must be zero');
    end
    else
    begin
      if A.Intervals[I].First>A.Intervals[I].Last then
        raise EWfcLattice.Create('coverage interval is reversed');
      if (I=1) and (A.Intervals[I].First<=A.Intervals[0].Last+1) then
        raise EWfcLattice.Create('coverage intervals must be sorted, disjoint and nonadjacent');
      Part:=A.Intervals[I].Last-A.Intervals[I].First+1;
      if Part>High(Integer)-Count then raise EWfcLattice.Create('coverage axis count exceeds Integer capacity');
      Inc(Count,Part);
    end;
  end;
  if Count<>A.Count then raise EWfcLattice.Create('coverage axis count is inconsistent');
  Result:=Count;
end;

function WfcLatticeCoverageCellCount(const ACoverage: TWfcLatticeCoverage): Integer;
var X,Y,Z: Integer;
{$IFDEF PAS2JS}Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid = ACoverage !== null && typeof ACoverage === 'object' && !Array.isArray(ACoverage); end;
  if not Valid then raise EWfcLattice.Create('coverage must be a record');
  {$ENDIF}
  X:=ValidateCoverageAxis(ACoverage.X);
  Y:=ValidateCoverageAxis(ACoverage.Y);
  Z:=ValidateCoverageAxis(ACoverage.Z);
  if (X=0) and (Y=0) and (Z=0) then Exit(0);
  if (X=0) or (Y=0) or (Z=0) then raise EWfcLattice.Create('coverage cannot have a partially empty axis');
  Result:=CheckedProduct(CheckedProduct(X,Y),Z);
end;

function AxisCell(const A: TWfcLatticeAxisCoverage; const AOrdinal: Integer): Integer;
var FirstCount: Integer;
begin
  FirstCount:=A.Intervals[0].Last-A.Intervals[0].First+1;
  if AOrdinal<FirstCount then Result:=A.Intervals[0].First+AOrdinal
  else Result:=A.Intervals[1].First+(AOrdinal-FirstCount);
end;

function WfcLatticeCoverageCell(const ACoverage: TWfcLatticeCoverage;
  const AOrdinal: Integer): TWfcLatticeVector;
var Count, Q: Integer;
begin
  Count:=WfcLatticeCoverageCellCount(ACoverage);
  RequireInteger(AOrdinal,0,High(Integer),'coverage ordinal');
  if AOrdinal>=Count then raise EWfcLattice.Create('coverage ordinal is outside the coverage');
  Result.X:=AxisCell(ACoverage.X,AOrdinal mod ACoverage.X.Count);
  Q:=AOrdinal div ACoverage.X.Count;
  Result.Y:=AxisCell(ACoverage.Y,Q mod ACoverage.Y.Count);
  Result.Z:=AxisCell(ACoverage.Z,Q div ACoverage.Y.Count);
end;

end.
