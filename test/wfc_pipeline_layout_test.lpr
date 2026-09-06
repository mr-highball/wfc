{ SPDX-License-Identifier: MIT
  Checked native and browser conformance for owned pass-layout tables. }
program wfc_pipeline_layout_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc_lattice, wfc_pipeline_layout;
type TTest=procedure;
var Checks,Failures:Integer;

procedure Check(const OK:Boolean; const Text:String);
begin
  Inc(Checks);
  if not OK then begin Inc(Failures); WriteLn('[FAIL] ',Text); end;
end;

procedure Run(const Name:String; const Test:TTest);
begin
  WriteLn('[TEST] ',Name);
  try Test; except on E:Exception do
    begin Inc(Failures); WriteLn('[EXCEPTION] ',E.ClassName,': ',E.Message); end; end;
end;

function V(const X,Y,Z:Integer):TWfcLatticeVector;
begin Result:=MakeWfcLatticeVector(X,Y,Z); end;

function Equal(const A,B:TWfcLatticeVector):Boolean;
begin Result:=(A.X=B.X) and (A.Y=B.Y) and (A.Z=B.Z); end;

procedure Inputs(const Count:Integer; out T:TWfcPipelinePassTopologies;
  out E:TWfcPipelinePassExtents);
var I:Integer;
begin
  T:=nil; E:=nil;
  SetLength(T,Count); SetLength(E,Count);
  for I:=0 to Count-1 do begin T[I]:=LegacyWfcPipelinePassTopology(1,False); E[I]:=V(1,1,1); end;
end;

procedure RejectTable(const T:TWfcPipelinePassTopologies; const E:TWfcPipelinePassExtents;
  const Text:String);
var Table:TWfcPipelineLayoutTable; Raised:Boolean;
begin
  Raised:=False; Table:=nil;
  try
    try Table:=TWfcPipelineLayoutTable.Create(T,E); except on EWfcLattice do Raised:=True; end;
  finally Table.Free; end;
  Check(Raised,Text);
end;

procedure RejectCell(const Table:TWfcPipelineLayoutTable; const Pass:Integer;
  const Cell:TWfcLatticeVector; const Text:String);
var Raised:Boolean;
begin
  Raised:=False;
  try Table.FlatCellIndex(Pass,Cell); except on EWfcLattice do Raised:=True; end;
  Check(Raised,Text);
end;

procedure PrefixesAndIsolation;
var T,TC:TWfcPipelinePassTopologies; E,EC:TWfcPipelinePassExtents;
  Table:TWfcPipelineLayoutTable; L:TWfcLatticeLayout; LS:TWfcLatticeLayouts;
  Top:TWfcPipelinePassTopology; Seen:array[0..9] of Boolean;
  I,X,K:Integer;
begin
  Inputs(3,T,E); E[0]:=V(2,1,1); E[1]:=V(5,1,1); E[2]:=V(3,1,1);
  Table:=TWfcPipelineLayoutTable.Create(T,E);
  try
    Check((Table.PassCount=3) and (Table.TotalCellCount=10),'sum actual pass cells');
    Check((Table.PassCellCount(0)=2) and (Table.PassCellCount(1)=5)
      and (Table.PassCellCount(2)=3),'unlike pass counts');
    Check((Table.PassOffsetAt(0)=0) and (Table.PassOffsetAt(1)=2)
      and (Table.PassOffsetAt(2)=7),'checked prefixes are 0,2,7');
    Check((1*2+4=6) and (2*2+2=6),'former uniform stride example collides');
    Check((Table.FlatCellIndex(1,V(4,0,0))=6) and
      (Table.FlatCellIndex(2,V(2,0,0))=9),'actual prefix keys are distinct 6 and 9');
    for I:=0 to 9 do Seen[I]:=False;
    for I:=0 to 2 do for X:=0 to E[I].X-1 do
    begin
      K:=Table.FlatCellIndex(I,V(X,0,0));
      Check(not Seen[K],'small full flat-key table has no alias'); Seen[K]:=True;
      Check(Table.LocalCellIndex(I,V(X,0,0))=X,'local X fastest');
    end;
    for I:=0 to 9 do Check(Seen[I],'prefix keys have no padding or gaps');
    T[0].Origin.X:=99; T[0].Pitch.Y:=99; T[0].Rank:=3; T[0].Wrap:=True; E[1].X:=99;
    SetLength(T,0); SetLength(E,0);
    L:=Table.PassLayoutAt(0);
    Check(Equal(L.Origin,V(0,0,0)) and Equal(L.Pitch,V(1,1,1)) and not L.Wrap,
      'input vectors/topology were detached');
    Check(Table.PassCellCount(1)=5,'input extents and array lengths were detached');
    L.Origin.X:=88; L.Cells.X:=88;
    L:=Table.PassLayoutAt(0); Check((L.Cells.X=2) and (L.Origin.X=0),'single layout result is detached');
    Top:=Table.PassTopologyAt(0); Top.Origin.X:=77; Top.Pitch.Z:=77; Top.Rank:=2;
    Top:=Table.PassTopologyAt(0);
    Check((Top.Rank=1) and (Top.Origin.X=0) and (Top.Pitch.Z=1),'single topology result is detached');
    LS:=Table.CopyLayouts; LS[0].Origin.X:=66; LS[1].Cells.X:=66; SetLength(LS,1);
    LS:=Table.CopyLayouts;
    Check((Length(LS)=3) and (LS[0].Origin.X=0) and (LS[1].Cells.X=5),'layout array and nested vectors detached');
    EC:=Table.CopyExtents; EC[2].X:=55; SetLength(EC,0); EC:=Table.CopyExtents;
    Check((Length(EC)=3) and (EC[2].X=3),'extent array and vectors detached');
    TC:=Table.CopyTopologies; TC[2].Origin.Z:=44; TC[0].Wrap:=True; SetLength(TC,0);
    TC:=Table.CopyTopologies;
    Check((Length(TC)=3) and (TC[2].Origin.Z=0) and not TC[0].Wrap,'topology copy detached');
    RejectCell(Table,-1,V(0,0,0),'negative pass rejected');
    RejectCell(Table,3,V(0,0,0),'upper pass rejected');
    RejectCell(Table,1,V(5,0,0),'upper local X rejected');
    RejectCell(Table,1,V(-1,0,0),'negative local X rejected');
    RejectCell(Table,1,V(0,1,0),'local Y dimension enforced');
    RejectCell(Table,1,V(0,0,1),'local Z dimension enforced');
  finally Table.Free; end;
end;

procedure RankAndShape;
var T:TWfcPipelinePassTopologies; E:TWfcPipelinePassExtents; Table:TWfcPipelineLayoutTable;
  L,M:TWfcLatticeLayout; B:TWfcLatticeBox; Rank:Integer; Wrap:Boolean;
  Top:TWfcPipelinePassTopology;
begin
  Top:=MakeWfcPipelinePassTopology(1,Default(TWfcLatticeVector),V(1,1,1),False);
  Check(Equal(Top.Origin,V(0,0,0)),'direct compiler-default origin is accepted');
  T:=nil; E:=nil; SetLength(T,1); SetLength(E,1);
  T[0].Rank:=1; T[0].Pitch:=V(1,1,1); E[0]:=V(1,1,1);
  Table:=TWfcPipelineLayoutTable.Create(T,E);
  try
    L:=Table.PassLayoutAt(0);
    Check(Equal(L.Origin,V(0,0,0)) and not L.Wrap,'untouched compiler-default fields are accepted');
  finally Table.Free; end;
  for Rank:=1 to 3 do for Wrap:=False to True do
  begin
    Inputs(1,T,E); T[0]:=LegacyWfcPipelinePassTopology(Rank,Wrap);
    Table:=TWfcPipelineLayoutTable.Create(T,E);
    try
      L:=Table.PassLayoutAt(0);
      Check((Table.PassTopologyAt(0).Rank=Rank) and (L.Wrap=Wrap)
        and Equal(L.Origin,V(0,0,0)) and Equal(L.Pitch,V(1,1,1)),'legacy topology helper');
    finally Table.Free; end;
  end;
  Inputs(3,T,E);
  T[0]:=MakeWfcPipelinePassTopology(1,V(-4,9,-5),V(2,7,9),True); E[0]:=V(2,1,1);
  T[1]:=MakeWfcPipelinePassTopology(2,V(5,-9,13),V(3,2,4),False); E[1]:=V(2,3,1);
  T[2]:=MakeWfcPipelinePassTopology(3,V(-3,-5,-7),V(3,5,7),True); E[2]:=V(2,2,2);
  Table:=TWfcPipelineLayoutTable.Create(T,E);
  try
    Check((Table.TotalCellCount=16) and (Table.PassOffsetAt(2)=8),'mixed ranks sum actual storage');
    L:=Table.PassLayoutAt(0); B:=WfcLatticeCellBox(L,V(1,0,0));
    Check(Equal(B.Minimum,V(-2,9,-5)) and Equal(B.Maximum,V(0,16,4)),
      'rank-one unused local axes retain meaningful world origins and pitches');
    L:=Table.PassLayoutAt(1); B:=WfcLatticeCellBox(L,V(1,2,0));
    Check(Equal(B.Minimum,V(8,-5,13)) and Equal(B.Maximum,V(11,-3,17)),
      'rank-two world Z remains meaningful');
    Check(Table.LocalCellIndex(2,V(1,1,1))=7,'three-dimensional local order');
    Check(Table.FlatCellIndex(2,V(1,1,1))=15,'three-dimensional global key');
    RejectCell(Table,0,V(2,0,0),'wrapped world does not wrap local storage indices');
  finally Table.Free; end;
  Inputs(2,T,E); T[0]:=LegacyWfcPipelinePassTopology(2,False); T[1]:=T[0];
  E[0]:=V(2,3,1); E[1]:=V(3,2,1);
  Table:=TWfcPipelineLayoutTable.Create(T,E);
  try
    L:=Table.PassLayoutAt(0); M:=Table.PassLayoutAt(1);
    Check((Table.PassCellCount(0)=6) and (Table.PassCellCount(1)=6)
      and not SameWfcLatticeLayout(L,M),'same cell count does not imply same shape');
    Check((Table.LocalCellIndex(0,V(1,1,0))=3) and
      (Table.LocalCellIndex(1,V(1,1,0))=4),'local indexing uses actual shape');
  finally Table.Free; end;
  E[1]:=E[0]; Table:=TWfcPipelineLayoutTable.Create(T,E);
  try Check((Table.TotalCellCount=12) and (Table.PassOffsetAt(1)=6),
    'identical layouts remain distinct stored passes, never deduplicated');
  finally Table.Free; end;
end;

procedure ExtremeAndRejected;
var T:TWfcPipelinePassTopologies; E:TWfcPipelinePassExtents; Table:TWfcPipelineLayoutTable;
  B:TWfcLatticeBox; I:Integer;
begin
  Inputs(1,T,E); T[0]:=MakeWfcPipelinePassTopology(1,V(Low(Integer),0,0),V(2,1,1),False);
  E[0]:=V(High(Integer),1,1); Table:=TWfcPipelineLayoutTable.Create(T,E);
  try
    Check((Table.PassCount=1) and (Table.TotalCellCount=High(Integer)),
      'billions of conceptual cells require only one table record');
    Check(Table.FlatCellIndex(0,V(High(Integer)-1,0,0))=High(Integer)-1,'last Integer-capacity key');
    B:=WfcLatticeCellBox(Table.PassLayoutAt(0),V(High(Integer)-1,0,0));
    Check((B.Minimum.X=High(Integer)-3) and (B.Maximum.X=High(Integer)-1),
      'signed extreme span beyond High(Integer) uses exact lattice endpoints');
  finally Table.Free; end;
  Inputs(3,T,E); E[0]:=V(High(Integer)-2,1,1);
  Table:=TWfcPipelineLayoutTable.Create(T,E);
  try Check((Table.TotalCellCount=High(Integer)) and
    (Table.PassOffsetAt(2)=High(Integer)-1) and
    (Table.FlatCellIndex(2,V(0,0,0))=High(Integer)-1),'three tiny records fill total capacity exactly');
  finally Table.Free; end;
  E[2]:=V(2,1,1); RejectTable(T,E,'three records reject total capacity overflow');
  for I:=0 to 11 do
  begin
    Inputs(2,T,E);
    case I of
      0:T[1].Rank:=0;
      1:T[1].Rank:=4;
      2:E[1].X:=0;
      3:E[1].X:=-1;
      4:E[1].Y:=2;
      5:E[1].Z:=2;
      6:begin T[1].Rank:=2; E[1].Z:=2; end;
      7:T[1].Pitch.X:=0;
      8:T[1].Pitch.Z:=-1;
      9:T[1].Origin.X:=High(Integer);
      10:begin T[1].Rank:=2; E[1]:=V(High(Integer),2,1); end;
      11:begin T[1].Origin.X:=High(Integer)-2; E[1].X:=3; end;
    end;
    RejectTable(T,E,'late-pass geometry failure '+IntToStr(I));
  end;
  Inputs(0,T,E); RejectTable(T,E,'empty table rejected');
  Inputs(2,T,E); SetLength(E,1); RejectTable(T,E,'mismatched array counts rejected');
  Inputs(1,T,E); T[0].Origin.X:=High(Integer)-1;
  Table:=TWfcPipelineLayoutTable.Create(T,E);
  try
    B:=WfcLatticeCellBox(Table.PassLayoutAt(0),V(0,0,0));
    Check(B.Maximum.X=High(Integer),'exact signed upper endpoint is accepted');
  finally Table.Free; end;
end;

procedure ExhaustiveSmallIndices;
var T:TWfcPipelinePassTopologies; E:TWfcPipelinePassExtents; Table:TWfcPipelineLayoutTable;
  W,H,D,X,Y,Z,N:Integer;
begin
  for W:=1 to 3 do for H:=1 to 3 do for D:=1 to 2 do
  begin
    Inputs(2,T,E); E[0]:=V(2,1,1); T[1]:=LegacyWfcPipelinePassTopology(3,False); E[1]:=V(W,H,D);
    Table:=TWfcPipelineLayoutTable.Create(T,E);
    try
      N:=0;
      for Z:=0 to D-1 do for Y:=0 to H-1 do for X:=0 to W-1 do
      begin
        Check(Table.LocalCellIndex(1,V(X,Y,Z))=N,'literal enumeration local index');
        Check(Table.FlatCellIndex(1,V(X,Y,Z))=2+N,'literal enumeration late-pass flat index');
        Inc(N);
      end;
      Check(Table.TotalCellCount=2+N,'literal enumeration exact total');
    finally Table.Free; end;
  end;
end;

{$IFDEF PAS2JS}
procedure HostileJavaScript;
var T:TWfcPipelinePassTopologies; E:TWfcPipelinePassExtents; Table:TWfcPipelineLayoutTable;
  I,Reads,BadIndex:Integer;
  procedure MalformedCell(const Which:Integer);
  var Cell:TWfcLatticeVector;
  begin
    Cell:=V(0,0,0);
    case Which of 0:asm Cell=null; end; 1:asm Cell=[]; end;
      2:asm Cell.X='0'; end; 3:asm Cell.Y=NaN; end; 4:asm Cell.Z=0.5; end; end;
    RejectCell(Table,0,Cell,'raw JS local cell');
  end;
begin
  for I:=0 to 38 do
  begin
    Inputs(1,T,E); Reads:=0;
    case I of
      0:asm T=null; end;
      1:asm E=null; end;
      2:asm T='array'; end;
      3:asm E={length:1,0:{X:1,Y:1,Z:1}}; end;
      4:asm T={length:1,0:T[0]}; end;
      5:asm delete T[0]; end;
      6:asm delete E[0]; end;
      7:asm T[0]=null; end;
      8:asm T[0]=[]; end;
      9:asm delete T[0].Rank; end;
      10:asm T[0].Origin=null; end;
      11:asm T[0].Pitch=[]; end;
      12:asm E[0]=null; end;
      13:asm E[0]=[]; end;
      14:asm E[0].X=1.5; end;
      15:asm E[0].Y=NaN; end;
      16:asm E[0].Z=Infinity; end;
      17:asm T[0].Rank='1'; end;
      18:asm T[0].Rank=1.5; end;
      19:asm T[0].Rank=new Number(1); end;
      20:asm T[0].Wrap=0; end;
      21:asm T[0].Wrap=new Boolean(false); end;
      22:asm T[0].Origin.X='0'; end;
      23:asm T[0].Pitch.X=undefined; end;
      24:asm T[0].Origin.X=2147483648; end;
      25:asm T[0].Origin.X=-2147483649; end;
      26:asm E[0].X=2147483648; end;
      27:asm T[0].Pitch.X=1.5; end;
      28:asm Object.defineProperty(T,'0',{get:function(){Reads++;throw new Error('array getter');}}); end;
      29:asm Object.defineProperty(T[0],'Origin',{get:function(){Reads++;throw new Error('record getter');}}); end;
      30:asm Object.defineProperty(T[0].Origin,'X',{get:function(){Reads++;throw new Error('vector getter');}}); end;
      31:asm Object.defineProperty(E[0],'X',{get:function(){Reads++;throw new Error('extent getter');}}); end;
      32:asm E=new Uint32Array(1); end;
      33:asm T=new Array(2147483648); end;
      34:asm T[0]=undefined; end;
      35:asm
        const proto = {Y:1,Z:1};
        Object.defineProperty(proto,'X',{get:function(){Reads++;throw new Error('prototype getter');}});
        E[0]=Object.create(proto);
      end;
      36:asm T[0].Origin.X=-Infinity; end;
      37:asm T[0].Pitch.Y=true; end;
      38:asm T[0].Origin.Z=new Number(0); end;
    end;
    RejectTable(T,E,'raw JS malformed container/field '+IntToStr(I));
    Check(Reads=0,'malformed accessors never invoked');
  end;
  Inputs(1,T,E); Table:=TWfcPipelineLayoutTable.Create(T,E);
  try
    for I:=0 to 5 do
    begin
      BadIndex:=0;
      case I of 0:asm BadIndex=NaN; end; 1:asm BadIndex='0'; end;
        2:asm BadIndex=0.5; end; 3:asm BadIndex=Infinity; end;
        4:asm BadIndex=null; end; 5:asm BadIndex=new Number(0); end; end;
      RejectCell(Table,BadIndex,V(0,0,0),'raw JS pass index');
    end;
    for I:=0 to 4 do MalformedCell(I);
  finally Table.Free; end;
end;
{$ENDIF}

begin
  Run('actual-count prefixes and detached ownership',PrefixesAndIsolation);
  Run('rank, world geometry and distinct pass storage',RankAndShape);
  Run('signed capacity and late invalid input',ExtremeAndRejected);
  Run('literal small-shape flat index oracle',ExhaustiveSmallIndices);
  {$IFDEF PAS2JS}Run('hostile JavaScript containers and values',HostileJavaScript);{$ENDIF}
  WriteLn('Pipeline layout tables: ',Checks,' checks, ',Failures,' failures.');
  if Failures<>0 then raise Exception.Create('pipeline layout conformance failed');
end.
