{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_lattice_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF} SysUtils, wfc_lattice;

type TTest = procedure;
var Checks, Failures, OracleCases: Integer;

procedure Check(const OK: Boolean; const Msg: String);
begin
  Inc(Checks);
  if not OK then begin Inc(Failures); WriteLn('[FAIL] ',Msg); end;
end;

procedure Run(const Name: String; const Test: TTest);
begin
  WriteLn('[TEST] ',Name);
  try Test; except on E: Exception do
    begin Inc(Failures); WriteLn('[EXCEPTION] ',E.ClassName,': ',E.Message); end; end;
end;

function V(const X,Y,Z: Integer): TWfcLatticeVector;
begin Result:=MakeWfcLatticeVector(X,Y,Z); end;

function Box(const X0,Y0,Z0,X1,Y1,Z1: Integer): TWfcLatticeBox;
begin Result.Minimum:=V(X0,Y0,Z0); Result.Maximum:=V(X1,Y1,Z1); end;

function Equal(const A,B: TWfcLatticeVector): Boolean;
begin Result:=(A.X=B.X) and (A.Y=B.Y) and (A.Z=B.Z); end;

procedure RejectLayout(const L: TWfcLatticeLayout; const Msg: String);
var Raised: Boolean;
begin
  Raised:=False;
  try ValidateWfcLatticeLayout(L); except on E:EWfcLattice do Raised:=True; end;
  Check(Raised,Msg);
end;

procedure RejectBox(const L: TWfcLatticeLayout; const B: TWfcLatticeBox;
  const Msg: String);
var C:TWfcLatticeCoverage; Raised:Boolean;
begin
  Raised:=False;
  try TryWfcLatticeCoverage(L,B,C); except on E:EWfcLattice do Raised:=True; end;
  Check(Raised,Msg);
end;

procedure RejectCoverage(const C: TWfcLatticeCoverage; const Msg:String);
var Raised:Boolean;
begin
  Raised:=False;
  try WfcLatticeCoverageCellCount(C); except on E:EWfcLattice do Raised:=True; end;
  Check(Raised,Msg);
end;

procedure TestBasic;
var L,M:TWfcLatticeLayout; B:TWfcLatticeBox; C:TWfcLatticeCoverage;
  P:TWfcLatticeVector; Raised:Boolean;
begin
  L:=MakeWfcLatticeLayout(3,4,2,False);
  Check((WFC_LATTICE_VERSION=1) and (WfcLatticeCellCount(L)=24),'version and exact cell count');
  Check(Equal(L.Origin,V(0,0,0)) and Equal(L.Pitch,V(1,1,1)) and not L.Wrap,'default world frame');
  M:=L; Check(SameWfcLatticeLayout(L,M),'same complete layout');
  M.Wrap:=True; Check(not SameWfcLatticeLayout(L,M),'wrap participates in identity');
  M:=L; M.Origin.X:=1; Check(not SameWfcLatticeLayout(L,M),'origin participates in identity');
  M:=L; M.Pitch.Y:=2; Check(not SameWfcLatticeLayout(L,M),'pitch participates in identity');
  M:=L; M.Cells.Z:=1; Check(not SameWfcLatticeLayout(L,M),'all dimensions participate in identity');
  Check(TryWfcLatticePoint(L,V(2,3,1),P) and Equal(P,V(2,3,1)),'in-bounds corner point');
  P:=V(99,99,99);
  Check(not TryWfcLatticePoint(L,V(3,0,0),P) and Equal(P,V(0,0,0)),'upper endpoint excluded and failed output clear');
  Check(not TryWfcLatticePoint(L,V(-1,0,0),P),'bounded negative point rejects');
  B:=WfcLatticeCellBox(L,V(2,3,1));
  Check(Equal(B.Minimum,V(2,3,1)) and Equal(B.Maximum,V(3,4,2)),'exact half-open cell box');
  Check(TryWfcLatticeCoverage(L,Box(0,0,0,3,4,2),C) and (WfcLatticeCoverageCellCount(C)=24),'complete bounded extent');
  Check(Equal(WfcLatticeCoverageCell(C,23),V(2,3,1)),'last canonical XYZ ordinal');
  Check(TryWfcLatticeCoverage(L,Box(1,1,0,2,3,1),C) and (WfcLatticeCoverageCellCount(C)=2),'bounded subbox');
  Check(Equal(WfcLatticeCoverageCell(C,0),V(1,1,0)) and Equal(WfcLatticeCoverageCell(C,1),V(1,2,0)),'subbox ordinals are provider coordinates');
  Check(not TryWfcLatticeCoverage(L,Box(-1,0,0,1,1,1),C) and (WfcLatticeCoverageCellCount(C)=0),'partial bounded overlap rejects, never clips');
  Check(not TryWfcLatticeCoverage(L,Box(2,0,0,4,1,1),C) and (C.X.IntervalCount=0) and (C.Y.IntervalCount=0) and (C.Z.IntervalCount=0),'failed coverage has no partially published axes');
  Raised:=False; try WfcLatticeCoverageCell(C,0); except on E:EWfcLattice do Raised:=True; end;
  Check(Raised,'empty coverage has no valid ordinal');
  L:=MakeWfcLatticeLayout(5,4,3,V(-7,-11,-8),V(3,4,5),True);
  Check(TryWfcLatticeCoverage(L,Box(-10,-15,-13,-4,-7,-3),C),'three-axis seam coverage');
  Check((C.X.IntervalCount=2) and (C.Y.IntervalCount=2) and (C.Z.IntervalCount=2) and
    (WfcLatticeCoverageCellCount(C)=8),'at most two canonical intervals per seam axis');
  Check((C.X.Intervals[0].First=0) and (C.X.Intervals[0].Last=0) and
    (C.X.Intervals[1].First=4) and (C.X.Intervals[1].Last=4),'seam order canonical, not request order');
  Check(Equal(WfcLatticeCoverageCell(C,0),V(0,0,0)) and Equal(WfcLatticeCoverageCell(C,7),V(4,3,2)),'all-axis seam ordinal bounds');
  Check(TryWfcLatticePoint(L,V(-8,-12,-9),P) and Equal(P,V(4,3,2)),'negative fractions use floor, not truncation');
  Check(TryWfcLatticePoint(L,V(-7,-11,-8),P) and Equal(P,V(0,0,0)),'exact negative origin');
  P:=V(-8,-12,-9);
  Check(TryWfcLatticePoint(L,P,P) and Equal(P,V(4,3,2)),'point input/output aliasing is supported');
  M:=L;
  Check(TryWfcLatticePoint(M,V(-8,-12,-9),M.Cells) and Equal(M.Cells,V(4,3,2)),'output may alias a layout vector after snapshot');
  Check(TryWfcLatticeCoverage(L,Box(-6,-10,-7,8,5,7),C) and (WfcLatticeCoverageCellCount(C)=60),'nonaligned almost-period query can intersect every cell');
  Check(TryWfcLatticeCoverage(L,Box(-100,-100,-100,100,100,100),C) and
    (WfcLatticeCoverageCellCount(C)=60) and (C.X.IntervalCount=1),'multiple periods visit each cell once');
  Check(TryWfcLatticeCoverage(L,Box(-7,-11,-8,-4,-7,-3),C) and
    (WfcLatticeCoverageCellCount(C)=1),'upper boundary touching a neighboring cell does not include it');
end;

function OracleAxis(const Cell,N,Origin,Pitch,Lo,Hi:Integer; const Wrap:Boolean):Boolean;
var K,FirstK,LastK,Left:Integer;
begin
  { Independent forward interval intersection. No production mapping,
    floor/modulo, cell-box or coverage helper is consulted. The exhaustive
    small-case query construction stays within five image periods. }
  if Wrap then begin FirstK:=-8; LastK:=8; end
  else begin FirstK:=0; LastK:=0; end;
  Result:=False;
  for K:=FirstK to LastK do
  begin
    Left:=Origin+Cell*Pitch+K*N*Pitch;
    if (Left<Hi) and (Left+Pitch>Lo) then Exit(True);
  end;
end;

procedure OracleCase(const L:TWfcLatticeLayout; const B:TWfcLatticeBox);
var C:TWfcLatticeCoverage; P,Q:TWfcLatticeVector;
  X,Y,Z,Count,N:Integer; Expected,Actual,Hit,PointExpected:Boolean;
begin
  Inc(OracleCases);
  Expected:=L.Wrap or ((B.Minimum.X>=L.Origin.X) and (B.Minimum.Y>=L.Origin.Y) and
    (B.Minimum.Z>=L.Origin.Z) and (B.Maximum.X<=L.Origin.X+L.Cells.X*L.Pitch.X) and
    (B.Maximum.Y<=L.Origin.Y+L.Cells.Y*L.Pitch.Y) and (B.Maximum.Z<=L.Origin.Z+L.Cells.Z*L.Pitch.Z));
  Actual:=TryWfcLatticeCoverage(L,B,C);
  Check(Actual=Expected,'oracle complete bounded/wrapped acceptance');
  Count:=0; Q:=V(0,0,0); PointExpected:=False;
  for Z:=0 to L.Cells.Z-1 do for Y:=0 to L.Cells.Y-1 do for X:=0 to L.Cells.X-1 do
  begin
    Hit:=OracleAxis(X,L.Cells.X,L.Origin.X,L.Pitch.X,B.Minimum.X,B.Maximum.X,L.Wrap) and
      OracleAxis(Y,L.Cells.Y,L.Origin.Y,L.Pitch.Y,B.Minimum.Y,B.Maximum.Y,L.Wrap) and
      OracleAxis(Z,L.Cells.Z,L.Origin.Z,L.Pitch.Z,B.Minimum.Z,B.Maximum.Z,L.Wrap);
    if Expected and Hit then
    begin
      if Actual then
      begin
        P:=WfcLatticeCoverageCell(C,Count);
        Check((P.X=X) and (P.Y=Y) and (P.Z=Z),'oracle unique complete XYZ enumeration');
      end;
      Inc(Count);
    end;
    if OracleAxis(X,L.Cells.X,L.Origin.X,L.Pitch.X,B.Minimum.X,B.Minimum.X+1,L.Wrap) and
      OracleAxis(Y,L.Cells.Y,L.Origin.Y,L.Pitch.Y,B.Minimum.Y,B.Minimum.Y+1,L.Wrap) and
      OracleAxis(Z,L.Cells.Z,L.Origin.Z,L.Pitch.Z,B.Minimum.Z,B.Minimum.Z+1,L.Wrap) then
    begin
      Check(not PointExpected,'oracle periodic point has exactly one provider');
      PointExpected:=True; Q:=V(X,Y,Z);
    end;
  end;
  N:=WfcLatticeCoverageCellCount(C);
  Check(N=Count,'oracle Cartesian count, no duplicated periodic images');
  P:=V(99,99,99);
  Check(TryWfcLatticePoint(L,B.Minimum,P)=PointExpected,'oracle point acceptance');
  Check(Equal(P,Q),'oracle point exact coordinates or cleared failure');
end;

procedure TestSmallOracle;
var W,H,D,PV,OV,R,I,SX,SY,SZ,X,Y,Z:Integer;
  L:TWfcLatticeLayout; B:TWfcLatticeBox;
begin
  for W:=1 to 4 do for H:=1 to 3 do for D:=1 to 3 do
    for PV:=0 to 2 do for OV:=0 to 2 do for R:=0 to 1 do
  begin
    L:=MakeWfcLatticeLayout(W,H,D,V(OV*3-5,2-OV*4,OV-3),V(PV+1,3-PV,1+(PV mod 2)),R=1);
    SX:=W*L.Pitch.X; SY:=H*L.Pitch.Y; SZ:=D*L.Pitch.Z;
    for I:=0 to 23 do
    begin
      X:=L.Origin.X+(I*7 mod (4*SX+7))-2*SX-3;
      Y:=L.Origin.Y+(I*5 mod (4*SY+7))-2*SY-3;
      Z:=L.Origin.Z+(I*11 mod (4*SZ+7))-2*SZ-3;
      B:=Box(X,Y,Z,X+1+(I*3 mod (3*SX+2)),Y+1+(I*7 mod (3*SY+2)),Z+1+(I*2 mod (3*SZ+2)));
      OracleCase(L,B);
    end;
    for Z:=0 to D-1 do for Y:=0 to H-1 do for X:=0 to W-1 do
    begin
      B.Minimum:=V(L.Origin.X+X*L.Pitch.X,L.Origin.Y+Y*L.Pitch.Y,L.Origin.Z+Z*L.Pitch.Z);
      B.Maximum:=V(B.Minimum.X+L.Pitch.X,B.Minimum.Y+L.Pitch.Y,B.Minimum.Z+L.Pitch.Z);
      OracleCase(L,B);
    end;
  end;
  Check(OracleCases=22032,'fixed exhaustive oracle case count');
end;

procedure TestNumericExtremes;
var L,M:TWfcLatticeLayout; B:TWfcLatticeBox; C:TWfcLatticeCoverage;
  P:TWfcLatticeVector; Raised:Boolean; I:Integer;
begin
  L:=MakeWfcLatticeLayout(3,1,1,V(Low(Integer),0,0),V(1431655765,1,1),False);
  Check(WfcLatticeCellCount(L)=3,'legal world span 2^32-1 is not capped at signed Integer');
  B:=WfcLatticeCellBox(L,V(2,0,0));
  Check((B.Minimum.X=715827882) and (B.Maximum.X=High(Integer)),'large exact multiplication after endpoint preflight');
  Check(TryWfcLatticeCoverage(L,Box(Low(Integer),0,0,High(Integer),1,1),C) and
    (WfcLatticeCoverageCellCount(C)=3),'full signed-world coverage');
  Check(TryWfcLatticePoint(L,V(High(Integer)-1,0,0),P) and (P.X=2),'large last included tick');
  Check(not TryWfcLatticePoint(L,V(High(Integer),0,0),P),'signed-high endpoint is excluded');
  L.Wrap:=True;
  Check(TryWfcLatticePoint(L,V(High(Integer),0,0),P) and (P.X=0),'signed-high endpoint wraps exactly');
  L:=MakeWfcLatticeLayout(7,1,1,V(High(Integer)-7,0,0),V(1,1,1),True);
  Check(TryWfcLatticePoint(L,V(Low(Integer),0,0),P) and (P.X=4),'negative 32-bit-wide difference exact modulo');
  L:=MakeWfcLatticeLayout(7,1,1,V(Low(Integer),0,0),V(1,1,1),True);
  Check(TryWfcLatticePoint(L,V(High(Integer),0,0),P) and (P.X=3),'positive 32-bit-wide difference exact modulo');
  L:=MakeWfcLatticeLayout(High(Integer),1,1,True);
  Check(WfcLatticeCellCount(L)=High(Integer),'numeric maximum cell count needs no storage');
  Check(TryWfcLatticeCoverage(L,Box(Low(Integer),0,0,High(Integer),1,1),C) and
    (WfcLatticeCoverageCellCount(C)=High(Integer)),'huge lazy coverage does not enumerate or allocate');
  Check(Equal(WfcLatticeCoverageCell(C,High(Integer)-1),V(High(Integer)-1,0,0)),'huge lazy ordinal exact');
  Check(TryWfcLatticeCoverage(L,Box(-2,0,0,2,1,1),C) and (C.X.IntervalCount=2) and
    (WfcLatticeCoverageCellCount(C)=4),'large seam uses wide intermediate finish');
  Check(Equal(WfcLatticeCoverageCell(C,3),V(High(Integer)-1,0,0)),'large seam canonical tail');
  for I:=0 to 2 do
  begin
    M:=MakeWfcLatticeLayout(1,1,1,False);
    case I of
      0:begin M.Cells.X:=2; M.Pitch.X:=High(Integer); M.Origin.X:=Low(Integer); end;
      1:begin M.Cells.Y:=2; M.Pitch.Y:=High(Integer); M.Origin.Y:=Low(Integer); end;
      2:begin M.Cells.Z:=2; M.Pitch.Z:=High(Integer); M.Origin.Z:=Low(Integer); end;
    end;
    Check(WfcLatticeCellCount(M)=2,'wide legal endpoints work independently on all XYZ axes');
  end;
  M:=MakeWfcLatticeLayout(1,1,1,False); M.Cells.X:=High(Integer); M.Pitch.X:=High(Integer); M.Origin.X:=Low(Integer);
  RejectLayout(M,'reject a potential 62-bit product before multiplication');
  M:=MakeWfcLatticeLayout(1,1,1,False); M.Cells.X:=46341; M.Cells.Y:=46341;
  RejectLayout(M,'cell-plane overflow');
  M:=MakeWfcLatticeLayout(1,1,1,False); M.Cells:=V(1291,1291,1291);
  RejectLayout(M,'cell-volume overflow');
  M:=MakeWfcLatticeLayout(1,1,1,False); M.Origin.Z:=High(Integer);
  RejectLayout(M,'world endpoint overflow');
  M:=MakeWfcLatticeLayout(1,1,1,False); M.Pitch.Y:=0;
  RejectLayout(M,'zero pitch');
  M:=MakeWfcLatticeLayout(1,1,1,False); M.Cells.Z:=0;
  RejectLayout(M,'zero extent');
  M:=MakeWfcLatticeLayout(1,1,1,False); M.Cells.Y:=-1;
  RejectLayout(M,'negative extent');
  Raised:=False; try SameWfcLatticeLayout(L,M); except on E:EWfcLattice do Raised:=True; end;
  Check(Raised,'same-layout validates even an unequal second argument');
  L:=MakeWfcLatticeLayout(1,1,1,False);
  for I:=0 to 2 do
  begin
    B:=Box(0,0,0,1,1,1);
    case I of 0:B.Maximum.X:=0; 1:B.Maximum.Y:=0; 2:B.Maximum.Z:=0; end;
    RejectBox(L,B,'zero box span');
  end;
  RejectBox(L,Box(1,0,0,0,1,1),'reversed box');
  Raised:=False; try WfcLatticeCellBox(L,V(-1,0,0)); except on E:EWfcLattice do Raised:=True; end;
  Check(Raised,'negative cell index');
  Raised:=False; try WfcLatticeCellBox(L,V(0,0,1)); except on E:EWfcLattice do Raised:=True; end;
  Check(Raised,'upper cell index');
end;

procedure TestCoverageRecords;
var L:TWfcLatticeLayout; C,Bad:TWfcLatticeCoverage; Raised:Boolean;
begin
  C:=Default(TWfcLatticeCoverage);
  Check(WfcLatticeCoverageCellCount(C)=0,'canonical default empty coverage');
  L:=MakeWfcLatticeLayout(5,2,1,True);
  Check(TryWfcLatticeCoverage(L,Box(-1,0,0,1,2,1),C),'fixture two intervals');
  Bad:=C; Bad.X.Intervals[0].First:=-1; RejectCoverage(Bad,'negative interval');
  Bad:=C; Bad.X.Intervals[0].First:=1; RejectCoverage(Bad,'reversed interval');
  Bad:=C; Bad.X.Intervals[1].First:=0; RejectCoverage(Bad,'overlapping intervals');
  Bad:=C; Bad.X.Intervals[1].First:=1; RejectCoverage(Bad,'adjacent intervals must merge');
  Bad:=C; Bad.X.Count:=1; RejectCoverage(Bad,'forged axis count');
  Bad:=C; Bad.X.IntervalCount:=3; RejectCoverage(Bad,'too many intervals');
  Bad:=C; Bad.X.Intervals[1].Last:=High(Integer); RejectCoverage(Bad,'no layout can contain cell High(Integer)');
  Bad:=C; Bad.Y.Intervals[1].Last:=1; RejectCoverage(Bad,'unused interval nonzero');
  Bad:=C; Bad.Z:=Default(TWfcLatticeAxisCoverage); RejectCoverage(Bad,'partially empty coverage');
  Bad:=C; Bad.X.IntervalCount:=1; Bad.X.Intervals[0].First:=0;
  Bad.X.Intervals[0].Last:=High(Integer)-1; Bad.X.Intervals[1]:=Default(TWfcLatticeInterval);
  Bad.X.Count:=High(Integer); RejectCoverage(Bad,'coverage Cartesian product overflow');
  Raised:=False; try WfcLatticeCoverageCell(C,-1); except on E:EWfcLattice do Raised:=True; end;
  Check(Raised,'negative coverage ordinal');
  Raised:=False; try WfcLatticeCoverageCell(C,4); except on E:EWfcLattice do Raised:=True; end;
  Check(Raised,'upper coverage ordinal');
  Check((C.X.Intervals[1].First=4) and (C.X.Count=2),'record copies are detached');
end;

{$IFDEF PAS2JS}
function HostileInteger(const I:Integer):Integer;
begin
  asm return [0.5,NaN,Infinity,-Infinity,2147483648,-2147483649,'0',null,undefined,true][I]; end;
end;

procedure TestHostileJavaScript;
var L:TWfcLatticeLayout; B:TWfcLatticeBox; C,Bad:TWfcLatticeCoverage;
  P:TWfcLatticeVector; I,J,Value:Integer; Raised:Boolean;
begin
  for I:=0 to 9 do
  begin
    Value:=HostileInteger(I);
    Raised:=False; try MakeWfcLatticeVector(Value,0,0); except on E:EWfcLattice do Raised:=True; end;
    Check(Raised,'JS constructor typed finite Integer');
    for J:=0 to 8 do
    begin
      L:=MakeWfcLatticeLayout(2,2,2,False);
      case J of
        0:L.Cells.X:=Value; 1:L.Cells.Y:=Value; 2:L.Cells.Z:=Value;
        3:L.Origin.X:=Value; 4:L.Origin.Y:=Value; 5:L.Origin.Z:=Value;
        6:L.Pitch.X:=Value; 7:L.Pitch.Y:=Value; 8:L.Pitch.Z:=Value;
      end;
      RejectLayout(L,'JS every layout scalar is validated');
    end;
    L:=MakeWfcLatticeLayout(2,2,2,False);
    P:=V(0,0,0); P.Z:=Value; Raised:=False;
    try TryWfcLatticePoint(L,P,P); except on E:EWfcLattice do Raised:=True; end;
    Check(Raised,'JS world point scalar');
    P:=V(0,0,0); P.Y:=Value; Raised:=False;
    try WfcLatticeCellBox(L,P); except on E:EWfcLattice do Raised:=True; end;
    Check(Raised,'JS cell coordinate scalar');
    B:=Box(0,0,0,1,1,1); B.Maximum.Z:=Value;
    RejectBox(L,B,'JS box scalar');
    Check(TryWfcLatticeCoverage(L,Box(0,0,0,2,2,2),C),'JS fixture');
    for J:=0 to 3 do
    begin
      Bad:=C;
      case J of 0:Bad.X.Count:=Value; 1:Bad.Y.IntervalCount:=Value;
        2:Bad.Z.Intervals[0].First:=Value; 3:Bad.Z.Intervals[0].Last:=Value; end;
      RejectCoverage(Bad,'JS coverage scalar');
    end;
    Raised:=False; try WfcLatticeCoverageCell(C,Value); except on E:EWfcLattice do Raised:=True; end;
    Check(Raised,'JS ordinal scalar');
  end;
  for I:=0 to 5 do
  begin
    L:=MakeWfcLatticeLayout(1,1,1,False);
    asm L.Wrap = [0,1,'false','true',null,undefined][I]; end;
    RejectLayout(L,'JS wrap must be a real Boolean');
  end;
  L:=MakeWfcLatticeLayout(1,1,1,False);
  asm L.Cells = null; end;
  RejectLayout(L,'JS malformed vector record');
  C:=Default(TWfcLatticeCoverage);
  asm C.X.Intervals = []; end;
  RejectCoverage(C,'JS malformed interval array');
  for I:=0 to 1 do
  begin
    C:=Default(TWfcLatticeCoverage);
    asm delete C.X.Intervals[I]; end;
    RejectCoverage(C,'JS sparse interval array');
  end;
end;
{$ENDIF}

begin
  Run('exact defaults, seams, nonaligned cells and aliasing',@TestBasic);
  Run('independent small forward-intersection oracle',@TestSmallOracle);
  Run('signed-world numeric extremes and preflight',@TestNumericExtremes);
  Run('canonical public coverage records',@TestCoverageRecords);
  {$IFDEF PAS2JS}Run('hostile JavaScript values',@TestHostileJavaScript);{$ENDIF}
  WriteLn('Lattice checks: ',Checks-Failures,'/',Checks,'; oracle cases: ',OracleCases);
  if Failures<>0 then Halt(1);
end.
