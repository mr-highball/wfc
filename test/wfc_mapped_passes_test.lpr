{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_mapped_passes_test;
{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, Classes, wfc, wfc_lattice, wfc_model,
  wfc_pattern2d, wfc_pattern2d_learn, wfc_pattern3d, wfc_pattern3d_learn;

type
  TTestProcedure = procedure;
  TAtomicGraph = class(TGraph)
  protected
    function DoCreateEntry: TGraphEntry; override;
    function DoValidateCommit(out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean; override;
  end;
  TNeighborHookEntry = class(TGraphEntry)
  public
    OwnerPass, BeforeCalls, AfterCalls: Integer;
  protected
    procedure DoBeforeSetNeighbor(const ANeighbor: TGraphEntry); override;
    procedure DoAfterSetNeighbor(const ANeighbor: TGraphEntry); override;
  end;
  TNeighborHookGraph = class(TGraph)
  protected
    function DoCreateEntry: TGraphEntry; override;
  end;

var
  Checks, Failures: Integer;
  FactoryCalls, FailFactoryAt: Integer;
  GuardGraph: TGraph;
  GuardLayouts: TWfcLatticeLayouts;
  GuardCommit, GuardEnabled: Boolean;
  GuardCalls, GuardLayoutRejections, GuardRuleRejections: Integer;
  FactoryGraph: TGraph;
  FactoryLayouts: TWfcLatticeLayouts;
  FactoryMutation, FactoryRejections: Integer;
  FactoryCatch, FactoryInside: Boolean;
  NeighborHooksEnabled: Boolean;
  NeighborHookLayouts: TWfcLatticeLayouts;
  NeighborBeforeCalls, NeighborAfterCalls: Integer;
  NeighborFailureKind, NeighborFailureAt: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then begin Inc(Failures); WriteLn('  [FAIL] ', AMessage); end;
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try ATest;
  except on E: Exception do begin Inc(Failures); WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message); end; end;
end;

function Values(const AValues: array of String): TGraphValues;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

function Layout(const W, H, D, OX, OY, OZ, PX, PY, PZ: Integer;
  const Wrap: Boolean): TWfcLatticeLayout;
begin
  Result := MakeWfcLatticeLayout(W,H,D,MakeWfcLatticeVector(OX,OY,OZ),
    MakeWfcLatticeVector(PX,PY,PZ),Wrap);
end;

function EqualLayout(const A, B: TWfcLatticeLayout): Boolean;
begin
  Result := (A.Cells.X=B.Cells.X) and (A.Cells.Y=B.Cells.Y) and (A.Cells.Z=B.Cells.Z)
    and (A.Origin.X=B.Origin.X) and (A.Origin.Y=B.Origin.Y) and (A.Origin.Z=B.Origin.Z)
    and (A.Pitch.X=B.Pitch.X) and (A.Pitch.Y=B.Pitch.Y) and (A.Pitch.Z=B.Pitch.Z)
    and (A.Wrap=B.Wrap);
end;

function Snapshot(const G: TGraph): String;
var P,X,Y,Z,I: Integer; A: TGraph; L: TWfcLatticeLayout; V: TGraphValues; E: TGraphEntry;
begin
  Result := IntToStr(G.CurrentPassIndex)+':';
  for P := 0 to G.TotalPassCount-1 do begin
    A := G.PassGraph[P]; L := A.PassLayout;
    Result := Result+'|'+IntToStr(L.Cells.X)+','+IntToStr(L.Cells.Y)+','+IntToStr(L.Cells.Z)
      +'@'+IntToStr(L.Origin.X)+','+IntToStr(L.Origin.Y)+','+IntToStr(L.Origin.Z)
      +'/'+IntToStr(L.Pitch.X)+','+IntToStr(L.Pitch.Y)+','+IntToStr(L.Pitch.Z)
      +'/'+IntToStr(Ord(L.Wrap))+':';
    for Z := 0 to L.Cells.Z-1 do for Y := 0 to L.Cells.Y-1 do for X := 0 to L.Cells.X-1 do begin
      E := A.Entry[X,Y,Z];
      if E.Empty then Result := Result+'-' else
        Result := Result+IntToStr(Ord(E.Generated))+':'+E.Value;
      if A.HasAllowedValues(X,Y,Z) then begin
        Result := Result+'{'; V := A.CopyAllowedValues(X,Y,Z);
        for I := 0 to High(V) do Result := Result+V[I]+',';
        Result := Result+'}';
      end;
      Result := Result+';';
    end;
  end;
end;

function NewPair(const P,C: TWfcLatticeLayout; const Atomic: Boolean=False): TGraph;
var L: TWfcLatticeLayouts;
begin
  if Atomic then Result := TAtomicGraph.Create else Result := TGraph.Create;
  try
    Result.Seed := 0; Result.Reshape(1,1,1);
    Result.CurrentPass := 'provider'; Result.PassMode := gpmOverlay;
    Result.AddValue('a'); Result.AddValue('b');
    Result.SwitchToPass('consumer'); Result.PassMode := gpmOverlay; Result.ClearDependencies;
    Result.AddValue('yes');
    SetLength(L,2); L[0] := P; L[1] := C; Result.ConfigurePassLayouts(L);
  except Result.Free; raise; end;
end;

function RejectedQuery(const G: TGraph; const Q: TGraphPassMapQuery;
  const Source: String='provider'): Boolean;
begin
  Result := False;
  try G.PassGraph[1].Rules['yes'].RequireMappedFromPass(Source,Q);
  except on E: Exception do Result := True; end;
end;

function RejectedLayouts(const G: TGraph; const L: TWfcLatticeLayouts): Boolean;
begin
  Result := False;
  try G.ConfigurePassLayouts(L);
  except on E: Exception do Result := True; end;
end;

function ProviderValue(const X,Y,Z: Integer): String;
begin
  if (X+2*Y+Z) mod 3 = 0 then Result := 'a' else Result := 'b';
end;

{ Deliberately literal, independent oracle. No mapping, coverage, cell-box,
  flattening or wrap helper from wfc_lattice is used here. Small test boxes
  intersect at most four translated copies of the provider on each side. }
function AxisHit(const Lo,Hi,CellLo,CellHi,Period: Integer;
  const PointQuery,Wrap: Boolean): Boolean;
var K,First,Last,A,B: Integer;
begin
  Result := False;
  if Wrap then begin First := -4; Last := 4; end else begin First := 0; Last := 0; end;
  for K := First to Last do begin
    A := CellLo+K*Period; B := CellHi+K*Period;
    if PointQuery then begin if (Lo>=A) and (Lo<B) then Exit(True); end
    else if (Lo<B) and (Hi>A) then Exit(True);
  end;
end;

function LiteralOracle(const P,C: TWfcLatticeLayout; const Q: TGraphPassMapQuery): Boolean;
var LX,LY,LZ,HX,HY,HZ,EX,EY,EZ,X,Y,Z,N,M,I: Integer; Hit,Accepted,PointQuery: Boolean;
begin
  PointQuery := Q.Kind=gpmkPoint;
  LX := C.Origin.X+Q.MinimumOffset.DeltaX;
  LY := C.Origin.Y+Q.MinimumOffset.DeltaY;
  LZ := C.Origin.Z+Q.MinimumOffset.DeltaZ;
  HX := LX; HY := LY; HZ := LZ;
  if Q.Kind=gpmkCellCoverage then begin HX:=LX+C.Pitch.X; HY:=LY+C.Pitch.Y; HZ:=LZ+C.Pitch.Z; end;
  if Q.Kind=gpmkRegionCoverage then begin
    HX:=C.Origin.X+Q.MaximumOffset.DeltaX; HY:=C.Origin.Y+Q.MaximumOffset.DeltaY; HZ:=C.Origin.Z+Q.MaximumOffset.DeltaZ;
  end;
  EX:=P.Cells.X*P.Pitch.X; EY:=P.Cells.Y*P.Pitch.Y; EZ:=P.Cells.Z*P.Pitch.Z;
  if not P.Wrap then begin
    if (LX<P.Origin.X) or (LY<P.Origin.Y) or (LZ<P.Origin.Z) then Exit(False);
    if PointQuery then begin
      if (LX>=P.Origin.X+EX) or (LY>=P.Origin.Y+EY) or (LZ>=P.Origin.Z+EZ) then Exit(False);
    end else if (HX>P.Origin.X+EX) or (HY>P.Origin.Y+EY) or (HZ>P.Origin.Z+EZ) then Exit(False);
  end;
  N:=0; M:=0;
  for Z:=0 to P.Cells.Z-1 do for Y:=0 to P.Cells.Y-1 do for X:=0 to P.Cells.X-1 do begin
    Hit := AxisHit(LX,HX,P.Origin.X+X*P.Pitch.X,P.Origin.X+(X+1)*P.Pitch.X,EX,PointQuery,P.Wrap)
      and AxisHit(LY,HY,P.Origin.Y+Y*P.Pitch.Y,P.Origin.Y+(Y+1)*P.Pitch.Y,EY,PointQuery,P.Wrap)
      and AxisHit(LZ,HZ,P.Origin.Z+Z*P.Pitch.Z,P.Origin.Z+(Z+1)*P.Pitch.Z,EZ,PointQuery,P.Wrap);
    if Hit then begin
      Inc(N); Accepted:=False;
      for I:=0 to High(Q.Values) do if ProviderValue(X,Y,Z)=Q.Values[I] then Accepted:=True;
      if Accepted then Inc(M);
    end;
  end;
  if Q.Match=gpmmAll then Result:=(N>0) and (M=N)
  else Result:=(N>0) and (M>=Q.MinimumMatches) and (M<=Q.MaximumMatches);
end;

procedure CheckMappedCase(const P,C: TWfcLatticeLayout; const Q: TGraphPassMapQuery;
  const Expected,Legacy: Boolean; const Name: String);
var G: TGraph; X,Y,Z: Integer; R: TGraphSolveReport; Actual: Boolean; Before: String;
begin
  G:=NewPair(P,C);
  try
    G.Rules['yes'].RequireMappedFromPass('provider',Q);
    for Z:=0 to P.Cells.Z-1 do for Y:=0 to P.Cells.Y-1 do for X:=0 to P.Cells.X-1 do
      G.PassGraph[0].Entry[X,Y,Z].Value:=ProviderValue(X,Y,Z);
    Before:=Snapshot(G);
    if Legacy then begin
      Actual:=True;
      try G.Run; except on E: Exception do Actual:=False; end;
    end else Actual:=G.TrySolve(DefaultGraphSolveOptions,R);
    Check(Actual=Expected,Name+' matches independent world oracle');
    if Actual then Check(G.PassGraph[1].Entry[0,0,0].Value='yes',Name+' publishes the required consumer')
    else if not Legacy then Check(Snapshot(G)=Before,Name+' failed finite solve is atomic');
  finally G.Free; end;
end;

procedure TestLiteralOracle;
var P,C: TWfcLatticeLayout; Q: TGraphPassMapQuery; W,O,K,B: Integer; Expected: Boolean;
begin
  for W:=0 to 1 do for O:=0 to 4 do for K:=0 to 6 do begin
    P:=Layout(3,2,2,-4,-3,-2,2,3,2,W<>0);
    C:=Layout(1,1,1,-5+O*2,-2,-1,3,2,2,False);
    case K of
      0: Q:=MakeGraphPassPointQuery(MakeGraphOffset(0,0,0),Values(['a']));
      1: Q:=MakeGraphPassPointQuery(MakeGraphOffset(1,-1,-1),Values(['b']));
      2: Q:=MakeGraphPassCellQuery(Values(['a','b']));
      3: Q:=MakeGraphPassCellQuery(MakeGraphOffset(-1,1,0),Values(['a']));
      4: Q:=MakeGraphPassRegionQuery(MakeGraphOffset(-3,-4,-3),MakeGraphOffset(5,4,3),Values(['a']));
      5: Q:=MakeGraphPassRegionQuery(MakeGraphOffset(0,0,0),MakeGraphOffset(1,1,1),Values(['b']));
      else Q:=MakeGraphPassRegionQuery(MakeGraphOffset(-9,-7,-5),MakeGraphOffset(10,8,6),Values(['a','b']));
    end;
    for B:=0 to 2 do begin
      if B=1 then Q:=MakeGraphPassCountQuery(Q,0,0);
      if B=2 then begin
        if Q.Kind=gpmkPoint then Q:=MakeGraphPassCountQuery(Q,1,1)
        else Q:=MakeGraphPassCountQuery(Q,2,4);
      end;
      Expected:=LiteralOracle(P,C,Q);
      CheckMappedCase(P,C,Q,Expected,False,'finite '+IntToStr(W)+':'+IntToStr(O)+':'+IntToStr(K)+':'+IntToStr(B));
      if O=2 then CheckMappedCase(P,C,Q,Expected,True,'legacy '+IntToStr(W)+':'+IntToStr(K)+':'+IntToStr(B));
    end;
  end;
  P:=Layout(3,2,2,-4,-3,-2,2,3,2,True);
  C:=Layout(1,1,1,0,0,0,1,1,1,False);
  Q:=MakeGraphPassRegionQuery(MakeGraphOffset(-1000000,-1000000,-1000000),
    MakeGraphOffset(1000000,1000000,1000000),Values(['a','b']));
  Q:=MakeGraphPassCountQuery(Q,12,12);
  CheckMappedCase(P,C,Q,True,False,'million-world-unit box counts twelve unique cells');
  Q:=MakeGraphPassCountQuery(Q,13,High(Integer));
  CheckMappedCase(P,C,Q,False,False,'minimum above unique footprint is a contradiction');
end;

function NewWorld(const Blocked: Boolean): TGraph;
var L: TWfcLatticeLayouts; X,Y: Integer;
begin
  Result:=TGraph.Create;
  try
    Result.Seed:=4; Result.Reshape(1,1,1);
    Result.CurrentPass:='terrain'; Result.PassMode:=gpmOverlay; Result.AddValue('land');
    Result.SwitchToPass('foliage'); Result.PassMode:=gpmOverlay; Result.ClearDependencies;
    Result.AddValue('clear'); Result.AddValue('tree');
    Result.SwitchToPass('housing'); Result.PassMode:=gpmOverlay; Result.ClearDependencies;
    Result.AddValue('house');
    SetLength(L,3);
    L[0]:=Layout(8,6,1,0,0,0,4,4,1,False);
    L[1]:=Layout(32,24,1,0,0,0,1,1,1,False);
    L[2]:=Layout(3,2,1,4,4,0,8,8,1,False);
    Result.ConfigurePassLayouts(L);
    Result.Rules['house'].RequireMappedFromPass('terrain',MakeGraphPassCellQuery(Values(['land'])));
    Result.Rules['house'].RequireMappedFromPass('foliage',MakeGraphPassCellQuery(Values(['clear'])));
    for Y:=0 to 23 do for X:=0 to 31 do Result.PassGraph[1].Entry[X,Y,0].Value:='clear';
    if Blocked then Result.PassGraph[1].Entry[7,7,0].Value:='tree';
  except Result.Free; raise; end;
end;

procedure ValidateWorld(const G: TGraph);
var X,Y,U,V: Integer; Good: Boolean;
begin
  for Y:=0 to 1 do for X:=0 to 2 do begin
    Good:=G.PassGraph[2].Entry[X,Y,0].Value='house';
    for V:=4+Y*8 to 11+Y*8 do for U:=4+X*8 to 11+X*8 do
      Good:=Good and (G.PassGraph[1].Entry[U,V,0].Value='clear')
        and (G.PassGraph[0].Entry[U div 4,V div 4,0].Value='land');
    Check(Good,'all sixty-four interior/corner foliage cells and terrain cells support house '+IntToStr(X)+','+IntToStr(Y));
  end;
end;

procedure TestWorldAndLayouts;
var G: TGraph; Before: String; R: TGraphSolveReport; L: TWfcLatticeLayouts; Saved: TWfcLatticeLayout; I: Integer;
begin
  G:=NewWorld(True);
  try
    Before:=Snapshot(G);
    Check(not G.TrySolve(DefaultGraphSolveOptions,R),'one blocked interior fine cell rejects the coarse house');
    Check((R.FailedPassIndex=2) and (Snapshot(G)=Before),'failed interior-footprint solve rolls back all differently sized passes');
    Check(G.CurrentPassIndex=2,'finite failure restores the selected pass');
    G.PassGraph[1].Entry[7,7,0].Value:='clear';
    Check(G.TrySolve(DefaultGraphSolveOptions,R),'clearing the interior obstacle permits a fresh solve');
    ValidateWorld(G);
    Check((G.Dimension.Width=8) and (G.PassGraph[1].Dimension.Width=32)
      and (G.PassGraph[2].Dimension.Width=3),'root default and direct pass dimensions remain distinct');
    G.SwitchToPass('foliage'); Saved:=G.PassLayout;
    Check(Saved.Cells.X=32,'root PassLayout follows selection');
    Saved.Cells.X:=1;
    Check(G.PassLayout.Cells.X=32,'returned layout is a detached value');
    SetLength(L,3); for I:=0 to 2 do L[I]:=G.PassGraph[I].PassLayout;
    G.ConfigurePassLayouts(L); L[1].Origin.X:=123;
    Check(G.PassGraph[1].PassLayout.Origin.X=0,'configured layout array is detached');
    Check(G.PassGraph[2].Entry[0,0,0].Empty and G.PassGraph[1].Entry[7,7,0].Empty,
      'successful configuration clears generated cells and caller locks');
    Check(G.PassGraph[2].HasDefinition,'successful configuration preserves model definitions');
    G.SwitchToPass('new-order-only'); G.PassMode:=gpmOverlay; G.ClearDependencies;
    Check(EqualLayout(G.PassLayout,G.PassGraph[0].PassLayout),'new passes inherit the root default, not the selected finer pass');
  finally G.Free; end;
  G:=NewWorld(False);
  try G.Run; ValidateWorld(G); finally G.Free; end;
end;

procedure TestCopiesAndConjunction;
var G,Twin: TGraph; P,C: TWfcLatticeLayout; Q: TGraphPassMapQuery;
  Input: TGraphValues; Options: TGraphSolveOptions; R,T: TGraphSolveReport;
begin
  P:=Layout(2,1,1,0,0,0,1,1,1,False); C:=Layout(1,1,1,0,0,0,2,1,1,False);
  G:=NewPair(P,C); Twin:=NewPair(P,C);
  try
    Input:=Values(['a']); Q:=MakeGraphPassPointQuery(MakeGraphOffset(0,0,0),Input);
    Input[0]:='b';
    Check(Q.Values[0]='a','query factory detaches caller value arrays');
    G.Rules['yes'].RequireMappedFromPass('provider',Q).RequireMappedFromPass('provider',Q);
    Twin.Rules['yes'].RequireMappedFromPass('provider',Q);
    Q.Values[0]:='b';
    G.PassGraph[0].Entry[0,0,0].Value:='a'; G.PassGraph[0].Entry[1,0,0].Value:='b';
    Twin.PassGraph[0].Entry[0,0,0].Value:='a'; Twin.PassGraph[0].Entry[1,0,0].Value:='b';
    Check(G.DependencyCount=1,'identical mapped registration infers one dependency');
    Options:=DefaultGraphSolveOptions; Options.CaptureTrace:=True;
    Check(G.TrySolve(Options,R) and Twin.TrySolve(Options,T),'registered queries own their accepted values');
    Check((R.TraceHash=T.TraceHash) and (Snapshot(G)=Snapshot(Twin)),'idempotent registration preserves exact finite replay');
    G.Rules['yes'].RequireMappedFromPass('provider',MakeGraphPassPointQuery(MakeGraphOffset(1,0,0),Values(['b'])));
    Check(G.TrySolve(Options,R),'distinct mapped clauses are both satisfied');
    G.PassGraph[0].Entry[1,0,0].Value:='a';
    Check(not G.TrySolve(Options,R),'a second mapped clause remains AND, not a merged alternative');
    Check(not G.PassGraph[0].Entry[1,0,0].Generated,'failed solve preserves the caller replacement lock');
  finally Twin.Free; G.Free; end;
end;

procedure TestRootParentedRegistration;
var G: TGraph; Group: TGraph.TParentedGraphRuleGroup;
  R: TGraphSolveReport; Before: String;
begin
  G:=NewPair(Layout(2,1,1,0,0,0,1,1,1,False),
    Layout(1,1,1,0,0,0,2,1,1,False));
  try
    Group:=G.Rules['yes'];
    Group.Parent:=G;
    Group.RequireMappedFromPass('provider',MakeGraphPassCellQuery(Values(['a'])));
    Check((Group.Parent=G) and (G.DependencyCount=1)
      and (G.PassGraph[0].DependencyCount=0),
      'public root-parented group registers the selected consumer dependency');
    G.PassGraph[0].Entry[0,0,0].Value:='a';
    G.PassGraph[0].Entry[1,0,0].Value:='a';
    Check(G.TrySolve(DefaultGraphSolveOptions,R)
      and (G.PassGraph[1].Entry[0,0,0].Value='yes'),
      'root-parented mapping reads the active coarse footprint, not root dimensions');
    G.PassGraph[0].Entry[1,0,0].Value:='b';
    Before:=Snapshot(G);
    Check(not G.TrySolve(DefaultGraphSolveOptions,R),
      'root-parented mapping checks the far fine provider cell');
    Check(Snapshot(G)=Before,'root-parented mapped contradiction is atomic');
  finally G.Free; end;
end;

procedure TestSelectedPassPatternCapture;
var G,Concrete: TGraph; L: TWfcLatticeLayouts; Tokens: TWfcModelTokens;
  M2: TWfcOverlappingModel2D; M3: TWfcOverlappingModel3D;
  Grid2,Direct2: TWfcPatternGrid2D; Grid3,Direct3: TWfcPatternGrid3D;
  Report2: TWfcOverlapping2DValidationReport;
  Report3: TWfcOverlapping3DValidationReport;
  Topology,X,Y,Z,I: Integer; ExpectedBoundary: TWfcModelBoundary;
begin
  SetLength(Tokens,2); Tokens[0]:='a'; Tokens[1]:='b';
  M2:=LearnOverlappingModel2D(Tokens,2,1,1,1,wmbWrap,wmsNone);
  try
    M3:=LearnOverlappingModel3D(Tokens,2,1,1,1,1,1,wmbWrap,wmsNone);
    try
      for Topology:=0 to 1 do begin
        G:=TGraph.Create;
        try
          G.Reshape(1,1,1); G.CurrentPass:='default'; G.PassMode:=gpmOverlay;
          G.SwitchToPass('patterns'); G.PassMode:=gpmOverlay; G.ClearDependencies;
          SetLength(L,2);
          L[0]:=Layout(3,1,1,0,0,0,1,1,1,Topology=0);
          L[1]:=Layout(2,3,2,-4,3,-2,2,3,4,Topology=1);
          G.ConfigurePassLayouts(L); Concrete:=G.PassGraph[1];
          if Topology=1 then ExpectedBoundary:=wmbWrap else ExpectedBoundary:=wmbOpen;
          for Z:=0 to 1 do for Y:=0 to 2 do for X:=0 to 1 do
            Concrete.Entry[X,Y,Z].Value:=TGraphValue(M2.PatternKeyAt((X+Y+Z) mod 2));
          Check(CaptureSolvedPatternGrid2D(M2,G,1,Grid2,Report2) and Report2.Valid,
            '2D capture accepts a selected slice beyond the root default depth');
          Check((Grid2.Width=2) and (Grid2.Height=3) and (Length(Grid2.Patterns)=6)
            and (Grid2.Boundary=ExpectedBoundary) and (Report2.CheckedPatterns=6),
            '2D capture uses selected dimensions, boundary and exact cell count');
          for Y:=0 to 2 do for X:=0 to 1 do
            Check(Grid2.Patterns[Y*2+X]=(X+Y+1) mod 2,
              '2D selected slice preserves every pattern in X-fast order');
          G.SwitchToPass('default');
          Check(CaptureSolvedPatternGrid2D(M2,Concrete,1,Direct2,Report2)
            and (Direct2.Width=Grid2.Width) and (Direct2.Height=Grid2.Height)
            and (Direct2.Boundary=Grid2.Boundary),
            '2D concrete-pass capture ignores the root current selection');
          for I:=0 to High(Grid2.Patterns) do
            Check(Direct2.Patterns[I]=Grid2.Patterns[I],'2D root and concrete captures agree');
          Check(not CaptureSolvedPatternGrid2D(M2,Concrete,2,Direct2,Report2)
            and (Report2.Issue.Kind=woikGridShape),
            '2D out-of-range slice is checked against the concrete pass depth');
          for Z:=0 to 1 do for Y:=0 to 2 do for X:=0 to 1 do
            Concrete.Entry[X,Y,Z].Value:=TGraphValue(M3.PatternKeyAt((X+Y+Z) mod 2));
          G.SwitchToPass('patterns');
          Check(CaptureSolvedPatternGrid3D(M3,G,Grid3,Report3) and Report3.Valid,
            '3D capture accepts the complete differently sized selected pass');
          Check((Grid3.Width=2) and (Grid3.Height=3) and (Grid3.Depth=2)
            and (Length(Grid3.Patterns)=12) and (Grid3.Boundary=ExpectedBoundary)
            and (Report3.CheckedPatterns=12),
            '3D capture uses selected XYZ extent, topology and exact cell count');
          for Z:=0 to 1 do for Y:=0 to 2 do for X:=0 to 1 do
            Check(Grid3.Patterns[(Z*3+Y)*2+X]=(X+Y+Z) mod 2,
              '3D selected volume preserves every pattern in X-fast order');
          G.SwitchToPass('default');
          Check(CaptureSolvedPatternGrid3D(M3,Concrete,Direct3,Report3)
            and (Direct3.Width=Grid3.Width) and (Direct3.Height=Grid3.Height)
            and (Direct3.Depth=Grid3.Depth) and (Direct3.Boundary=Grid3.Boundary),
            '3D concrete-pass capture ignores the root current selection');
          for I:=0 to High(Grid3.Patterns) do
            Check(Direct3.Patterns[I]=Grid3.Patterns[I],'3D root and concrete captures agree');
          Check((G.CurrentPassIndex=0) and (G.Dimension.Width=3)
            and (G.Dimension.Height=1) and (G.Dimension.Depth=1),
            'capture never changes root selection or the historical default dimensions');
        finally G.Free; end;
      end;
    finally M3.Free; end;
  finally M2.Free; end;
end;

function NewRepair(const Seed: TGraphSeed): TGraph;
var L: TWfcLatticeLayouts;
begin
  Result:=TGraph.Create;
  try
    Result.Seed:=Seed; Result.Reshape(1,1,1);
    Result.CurrentPass:='foliage'; Result.PassMode:=gpmOverlay;
    Result.AddValue('blocked'); Result.AddValue('clear');
    Result.SwitchToPass('weather'); Result.PassMode:=gpmOverlay; Result.ClearDependencies;
    Result.AddValue('sun'); Result.AddValue('rain');
    Result.SwitchToPass('housing'); Result.PassMode:=gpmOverlay; Result.ClearDependencies;
    Result.AddValue('house'); Result.AddValue('empty');
    SetLength(L,3); L[0]:=Layout(3,1,1,0,0,0,1,1,1,False);
    L[1]:=Layout(2,2,1,-2,-2,0,2,2,1,False); L[2]:=Layout(1,1,1,0,0,0,3,1,1,False);
    Result.ConfigurePassLayouts(L);
    Result.Rules['house'].RequireMappedFromPass('foliage',MakeGraphPassCellQuery(Values(['clear'])));
    Result.SetAllowedValues(0,0,0,'empty');
    Result.PassGraph[0].Entry[0,0,0].Value:='clear'; Result.PassGraph[0].Entry[2,0,0].Value:='clear';
  except Result.Free; raise; end;
end;

procedure TestSelectiveAndNegotiation;
var G,Twin: TGraph; Seed,P,I: Integer; Found: Boolean; Before,Weather: String;
  R,T: TGraphSolveReport; N: TGraphNegotiationOptions; S: TGraphSelectiveNegotiationReport;
  NR: TGraphNegotiationReport;
begin
  G:=nil; Twin:=nil; Found:=False;
  for Seed:=0 to 31 do begin
    G:=NewRepair(Seed);
    if G.TrySolve(DefaultGraphSolveOptions,R) and (G.PassGraph[0].Entry[1,0,0].Value='blocked') then begin Found:=True; Break; end;
    G.Free; G:=nil;
  end;
  Check(Found,'a deterministic baseline includes a generated interior blocker');
  if not Found then Exit;
  try
    Twin:=NewRepair(Seed); Check(Twin.TrySolve(DefaultGraphSolveOptions,T),'identical ragged baseline replay succeeds');
    G.SetAllowedValues(0,0,0,'house'); Twin.SetAllowedValues(0,0,0,'house');
    Before:=Snapshot(G);
    Check(not G.TryRegenerateFrom('housing',DefaultGraphSolveOptions,R),'housing-only solve cannot bypass the immutable interior blocker');
    Check(Snapshot(G)=Before,'failed selective solve restores every ragged pass and current domains');
    N:=DefaultGraphNegotiationOptions; N.MaxPassBacktracks:=16;
    Check(not G.TryRegenerateNegotiatedFrom('housing',N,S),'leaf-only negotiation cannot silently reopen an unauthorized provider');
    Check(Snapshot(G)=Before,'failed leaf negotiation is atomic across ragged storage');
    for P:=0 to 2 do for I:=0 to 3 do
      Check(G.PassGraph[P].RandomIndex(1000000)=Twin.PassGraph[P].RandomIndex(1000000),'failed selective paths preserve each pass RNG');
    Weather:=G.PassGraph[1].Entry[0,0,0].Value+G.PassGraph[1].Entry[1,0,0].Value
      +G.PassGraph[1].Entry[0,1,0].Value+G.PassGraph[1].Entry[1,1,0].Value;
    Check(G.TryRegenerateNegotiatedFrom('foliage',N,S),'authorized upstream negotiation repairs the blocked footprint');
    Check((G.PassGraph[0].Entry[1,0,0].Value='clear') and (G.PassGraph[2].Entry[0,0,0].Value='house'),
      'repair actually changes the interior provider and publishes the required house');
    Check(Weather=G.PassGraph[1].Entry[0,0,0].Value+G.PassGraph[1].Entry[1,0,0].Value
      +G.PassGraph[1].Entry[0,1,0].Value+G.PassGraph[1].Entry[1,1,0].Value,'independent differently-sized weather is preserved');
    Check(not G.PassGraph[0].Entry[0,0,0].Generated and not G.PassGraph[0].Entry[2,0,0].Generated,
      'negotiated repair preserves caller-owned footprint edge locks');
  finally Twin.Free; G.Free; end;
  G:=NewRepair(Seed);
  try
    G.SetAllowedValues(0,0,0,'house');
    { Full chronological negotiation may enumerate the independent weather
      pass's sixteen assignments before reopening foliage. This is distinct
      from selective negotiation, whose authorized closure omits weather. }
    N:=DefaultGraphNegotiationOptions; N.MaxPassBacktracks:=64;
    Check(G.TrySolveNegotiated(N,NR),'full negotiated solve supports unlike layouts');
    WriteLn('  Full negotiation status=',Ord(NR.Status),' pass-backtracks=',NR.PassBacktracks,
      ' final-pass=',NR.FinalReport.FailedPassIndex);
    Check((NR.PassBacktracks>0) and (G.PassGraph[0].Entry[1,0,0].Value='clear'),
      'full negotiation reopens the actual first blocked provider assignment');
  finally G.Free; end;
end;

procedure TestIndexSpaceAndCopyGuards;
var G: TGraph; L: TWfcLatticeLayouts; Terms: TGraphPassMatchTerms; K: Integer; Raised: Boolean; Before: String;
begin
  G:=NewPair(Layout(2,1,1,0,0,0,1,1,1,False),Layout(2,1,1,0,0,0,2,1,1,False));
  try
    SetLength(Terms,1); Terms[0]:=MakeGraphPassMatchTerm(MakeGraphOffset(0,0,0),Values(['a']));
    for K:=0 to 6 do begin
      if K=5 then G.DependsOn('provider'); { Give transform mode an unambiguous source. }
      Raised:=False;
      try
        case K of
          0: G.Rules['yes'].RequireFromPass('provider','a');
          1: G.Rules['yes'].RequirePrevious('a');
          2: G.Rules['yes'].RequireFromPassAt('provider',MakeGraphOffset(0,0,0),'a');
          3: G.Rules['yes'].RequireAnyFromPass('provider',Terms);
          4: G.Rules['yes'].RequireCountFromPass('provider',Terms,0,1,gpcmDistinctCells);
          5: G.PassMode:=gpmTransform;
          6: G.TransformFrom('provider');
        end;
      except on E: Exception do Raised:=True; end;
      Check(Raised,'index-space/copy relation rejects equal shape but unequal pitch '+IntToStr(K));
      if K=5 then begin
        Check((G.PassMode=gpmOverlay) and (G.DependencyCount=1),'rejected copy mode retains its pre-existing order-only source');
        G.RemoveDependency('provider');
      end else Check((G.PassMode=gpmOverlay) and (G.DependencyCount=0),'rejected old relation leaves mode and dependencies unchanged');
    end;
    G.DependsOn('provider'); Check(G.DependencyCount=1,'explicit order-only dependency permits unequal layouts');
  finally G.Free; end;
  G:=NewPair(Layout(2,1,1,0,0,0,1,1,1,False),Layout(2,1,1,0,0,0,1,1,1,False));
  try
    G.Rules['yes'].RequireFromPass('provider','a');
    G.PassGraph[0].Entry[0,0,0].Value:='a'; G.SetAllowedValues(0,0,0,'yes'); Before:=Snapshot(G);
    SetLength(L,2); L[0]:=G.PassGraph[0].PassLayout; L[1]:=G.PassGraph[1].PassLayout; L[1].Origin.X:=1;
    Check(RejectedLayouts(G,L),'configuration cannot invalidate an existing index-space relation');
    Check(Snapshot(G)=Before,'rejected relation/layout update preserves entries and domains');
    G.Reshape(3,2,1);
    Check(EqualLayout(G.PassGraph[0].PassLayout,Layout(3,2,1,0,0,0,1,1,1,False))
      and EqualLayout(G.PassGraph[1].PassLayout,G.PassGraph[0].PassLayout),'legacy Reshape returns every pass to one unit lattice');
    G.WrapNeighbors:=True;
    Check(G.PassGraph[0].PassLayout.Wrap and G.PassGraph[1].PassLayout.Wrap,'legacy wrap setter remains global');
  finally G.Free; end;
end;

function TAtomicGraph.DoCreateEntry: TGraphEntry;
begin
  Inc(FactoryCalls);
  if (FactoryMutation>0) and (FactoryCalls=1) and not FactoryInside then begin
    FactoryInside:=True;
    try
      try
        case FactoryMutation of
          1: FactoryGraph.ConfigurePassLayouts(FactoryLayouts);
          2: FactoryGraph.Reshape(2,2,1);
          3: FactoryGraph.SwitchToPass('reentrant-new-pass');
        end;
      except on E: EInvalidOperation do begin
        Inc(FactoryRejections);
        if not FactoryCatch then raise;
      end; end;
    finally FactoryInside:=False; end;
  end;
  if (FailFactoryAt>0) and (FactoryCalls=FailFactoryAt) then raise Exception.Create('intentional mapped-layout allocation failure');
  Result:=inherited DoCreateEntry;
end;

procedure ExerciseRunningGuard;
begin
  Inc(GuardCalls);
  try GuardGraph.ConfigurePassLayouts(GuardLayouts);
  except on E: EInvalidOperation do Inc(GuardLayoutRejections); end;
  try GuardGraph.PassGraph[1].Rules['yes'].RequireMappedFromPass('provider',
    MakeGraphPassPointQuery(MakeGraphOffset(0,0,0),Values(['a'])));
  except on E: EInvalidOperation do Inc(GuardRuleRejections); end;
end;

function TAtomicGraph.DoValidateCommit(out AFailedPassIndex,AFailedEntryIndex: Integer): Boolean;
begin
  if GuardEnabled and GuardCommit then ExerciseRunningGuard;
  Result:=inherited DoValidateCommit(AFailedPassIndex,AFailedEntryIndex);
end;

function GuardSelection(const G: TGraph; const E: TGraphEntry; const Valid: TGraphValues): TGraphValue;
begin
  if GuardEnabled then ExerciseRunningGuard;
  if Length(Valid)=0 then Result:=E.Value else Result:=Valid[0];
end;

procedure TestAtomicConfigurationAndRunning;
var G: TGraph; L: TWfcLatticeLayouts; Before: String; K: Integer; R: TGraphSolveReport;
  OldProviderEntry,OldConsumerEntry: TGraphEntry;
begin
  G:=NewPair(Layout(2,1,1,0,0,0,1,1,1,False),Layout(1,1,1,0,0,0,2,1,1,False),True);
  try
    G.PassGraph[0].Entry[0,0,0].Value:='a'; G.SetAllowedValues(0,0,0,'yes'); Before:=Snapshot(G);
    OldProviderEntry:=G.PassGraph[0].Entry[0,0,0]; OldConsumerEntry:=G.PassGraph[1].Entry[0,0,0];
    SetLength(L,2); L[0]:=Layout(3,1,1,0,0,0,1,1,1,False); L[1]:=Layout(2,1,1,0,0,0,2,1,1,False);
    FactoryCalls:=0; FailFactoryAt:=4;
    Check(RejectedLayouts(G,L),'entry allocation failure in a later pass aborts configuration');
    FailFactoryAt:=0;
    Check(Snapshot(G)=Before,'partial allocation cannot change dimensions, locks, values, domains or selection');
    Check((G.PassGraph[0].Entry[0,0,0]=OldProviderEntry) and (G.PassGraph[1].Entry[0,0,0]=OldConsumerEntry),
      'failed allocation preserves caller-held entry object identity');
    Check(G.PassGraph[0].HasDefinition and G.PassGraph[1].HasDefinition,'failed allocation preserves both definitions');
    for K:=0 to 5 do begin
      L[0]:=G.PassGraph[0].PassLayout; L[1]:=G.PassGraph[1].PassLayout;
      case K of
        0: L[1].Cells.X:=0;
        1: L[1].Pitch.Y:=0;
        2: L[1].Origin.X:=High(Integer);
        3: L[1].Cells.X:=High(Integer);
        4: SetLength(L,1);
        5: SetLength(L,0);
      end;
      Check(RejectedLayouts(G,L),'invalid layout rejected before mutation '+IntToStr(K));
      Check(Snapshot(G)=Before,'invalid configuration leaves complete old state '+IntToStr(K));
      SetLength(L,2);
    end;
  finally FailFactoryAt:=0; G.Free; end;
  for K:=0 to 1 do begin
    G:=NewPair(Layout(2,1,1,0,0,0,1,1,1,False),Layout(1,1,1,0,0,0,2,1,1,False),True);
    try
      G.Rules['yes'].RequireMappedFromPass('provider',MakeGraphPassCellQuery(Values(['a','b'])));
      GuardGraph:=G; SetLength(GuardLayouts,2); GuardLayouts[0]:=G.PassGraph[0].PassLayout; GuardLayouts[1]:=G.PassGraph[1].PassLayout;
      GuardCalls:=0; GuardLayoutRejections:=0; GuardRuleRejections:=0; GuardCommit:=K=1; GuardEnabled:=True;
      if K=0 then begin G.PassGraph[0].SelectionCallback:=GuardSelection; G.Run; end
      else Check(G.TrySolve(DefaultGraphSolveOptions,R),'finite commit guard fixture solves');
      GuardEnabled:=False;
      Check((GuardCalls>0) and (GuardLayoutRejections=GuardCalls) and (GuardRuleRejections=GuardCalls),
        'layout and mapped-rule mutations are rejected throughout running phase '+IntToStr(K));
    finally GuardEnabled:=False; GuardGraph:=nil; G.Free; end;
  end;
end;

function TNeighborHookGraph.DoCreateEntry: TGraphEntry;
begin
  Result:=TNeighborHookEntry.Create;
  TNeighborHookEntry(Result).OwnerPass:=CurrentPassIndex;
end;

procedure TNeighborHookEntry.DoBeforeSetNeighbor(const ANeighbor: TGraphEntry);
var L: TWfcLatticeLayout; X,Y,Z: Integer; Outside: Boolean;
begin
  if not NeighborHooksEnabled then Exit;
  if BeforeCalls>=6 then raise Exception.Create('neighbor direction assigned twice during preparation');
  L:=NeighborHookLayouts[OwnerPass];
  X:=Integer(Position.X); Y:=Integer(Position.Y); Z:=Integer(Position.Z);
  case BeforeCalls of
    0: Inc(Y);
    1: Inc(X);
    2: Dec(Y);
    3: Dec(X);
    4: Inc(Z);
    5: Dec(Z);
  end;
  Inc(BeforeCalls); Inc(NeighborBeforeCalls);
  Outside:=(X<0) or (X>=L.Cells.X) or (Y<0) or (Y>=L.Cells.Y)
    or (Z<0) or (Z>=L.Cells.Z);
  if L.Wrap then begin
    if X<0 then Inc(X,L.Cells.X) else if X>=L.Cells.X then Dec(X,L.Cells.X);
    if Y<0 then Inc(Y,L.Cells.Y) else if Y>=L.Cells.Y then Dec(Y,L.Cells.Y);
    if Z<0 then Inc(Z,L.Cells.Z) else if Z>=L.Cells.Z then Dec(Z,L.Cells.Z);
    Outside:=False;
  end;
  if Outside then begin
    if Assigned(ANeighbor) then raise Exception.Create('bounded preparation exposed a wrapped neighbor');
  end else begin
    if not Assigned(ANeighbor) then raise Exception.Create('requested preparation topology lost a neighbor');
    if (TNeighborHookEntry(ANeighbor).OwnerPass<>OwnerPass)
      or (ANeighbor.Position.X<>TGraphCoordinate(X))
      or (ANeighbor.Position.Y<>TGraphCoordinate(Y))
      or (ANeighbor.Position.Z<>TGraphCoordinate(Z)) then
      raise Exception.Create('preparation hook observed a neighbor outside the requested pass topology');
  end;
  if (NeighborFailureKind=1) and (NeighborBeforeCalls=NeighborFailureAt) then
    raise Exception.Create('injected before-neighbor hook failure');
end;

procedure TNeighborHookEntry.DoAfterSetNeighbor(const ANeighbor: TGraphEntry);
begin
  if not NeighborHooksEnabled then Exit;
  Inc(AfterCalls); Inc(NeighborAfterCalls);
  if AfterCalls>6 then raise Exception.Create('after-neighbor hook called twice for a direction');
  if (NeighborFailureKind=2) and (NeighborAfterCalls=NeighborFailureAt) then
    raise Exception.Create('injected after-neighbor hook failure');
end;

procedure ObserveNeighborPreparation(const L: TWfcLatticeLayouts;
  const FailureKind: Integer=0; const FailureAt: Integer=0);
var I: Integer;
begin
  NeighborHookLayouts:=nil; SetLength(NeighborHookLayouts,Length(L));
  for I:=0 to High(L) do NeighborHookLayouts[I]:=L[I];
  NeighborBeforeCalls:=0; NeighborAfterCalls:=0;
  NeighborFailureKind:=FailureKind; NeighborFailureAt:=FailureAt;
  NeighborHooksEnabled:=True;
end;

procedure CheckPreparedNeighborHooks(const G: TGraph; const Name: String);
var P,X,Y,Z,Cells: Integer; E: TNeighborHookEntry; L: TWfcLatticeLayout;
begin
  Cells:=0;
  for P:=0 to G.TotalPassCount-1 do begin
    L:=G.PassGraph[P].PassLayout;
    Check(EqualLayout(L,NeighborHookLayouts[P]),Name+' publishes the requested layout');
    for Z:=0 to L.Cells.Z-1 do for Y:=0 to L.Cells.Y-1 do for X:=0 to L.Cells.X-1 do begin
      Inc(Cells); E:=TNeighborHookEntry(G.PassGraph[P].Entry[X,Y,Z]);
      Check((E.BeforeCalls=6) and (E.AfterCalls=6),Name+' calls both hooks once per direction');
    end;
  end;
  Check((NeighborBeforeCalls=Cells*6) and (NeighborAfterCalls=Cells*6),
    Name+' has exactly six before/after callbacks per new cell');
end;

function NewNeighborHookPair: TGraph;
var L: TWfcLatticeLayouts;
begin
  NeighborHooksEnabled:=False;
  Result:=TNeighborHookGraph.Create;
  try
    Result.Seed:=4; Result.Reshape(1,1,1);
    Result.CurrentPass:='provider'; Result.PassMode:=gpmOverlay; Result.AddValue('a');
    Result.SwitchToPass('consumer'); Result.PassMode:=gpmOverlay;
    Result.ClearDependencies; Result.AddValue('yes');
    SetLength(L,2);
    L[0]:=Layout(2,2,1,0,0,0,1,1,1,True);
    L[1]:=Layout(3,1,2,0,0,0,1,1,1,False);
    Result.ConfigurePassLayouts(L);
  except Result.Free; raise; end;
end;

procedure TestSingleRequestedTopologyPreparation;
var G: TGraph; L: TWfcLatticeLayouts; P,Action,FailureKind: Integer;
  Before: String; Raised: Boolean; OldProvider,OldConsumer: TGraphEntry;
  R: TGraphSolveReport;
begin
  G:=TNeighborHookGraph.Create;
  try
    SetLength(L,1); L[0]:=Layout(1,1,1,0,0,0,1,1,1,True);
    ObserveNeighborPreparation(L); G.Reshape(1,1,1);
    CheckPreparedNeighborHooks(G,'first legacy Reshape');
  finally NeighborHooksEnabled:=False; G.Free; end;
  G:=NewNeighborHookPair;
  try
    SetLength(L,2);
    L[0]:=Layout(2,2,1,0,0,0,2,2,1,False);
    L[1]:=Layout(2,3,1,0,0,0,1,1,2,True);
    ObserveNeighborPreparation(L); G.ConfigurePassLayouts(L);
    CheckPreparedNeighborHooks(G,'mixed Configure with reversed prior wrap');
    L[0].Wrap:=True; L[1].Wrap:=False;
    ObserveNeighborPreparation(L); G.ConfigurePassLayouts(L);
    CheckPreparedNeighborHooks(G,'mixed Configure reverses wrap again');
    for P:=0 to 1 do L[P]:=Layout(2,2,2,0,0,0,1,1,1,True);
    ObserveNeighborPreparation(L); G.Reshape(2,2,2);
    CheckPreparedNeighborHooks(G,'wrapped global Reshape after mixed topology');
    L[0].Wrap:=False; L[1].Wrap:=True;
    ObserveNeighborPreparation(L); G.ConfigurePassLayouts(L);
    CheckPreparedNeighborHooks(G,'configure bounded root and wrapped second pass');
    for P:=0 to 1 do L[P]:=Layout(3,2,1,0,0,0,1,1,1,False);
    ObserveNeighborPreparation(L); G.Reshape(3,2,1);
    CheckPreparedNeighborHooks(G,'bounded global Reshape after mixed topology');
  finally NeighborHooksEnabled:=False; G.Free; end;
  for Action:=0 to 1 do for FailureKind:=1 to 2 do begin
    G:=NewNeighborHookPair;
    try
      G.PassGraph[0].Entry[0,0,0].Value:='a'; G.SetAllowedValues(0,0,0,'yes');
      Before:=Snapshot(G); OldProvider:=G.PassGraph[0].Entry[0,0,0];
      OldConsumer:=G.PassGraph[1].Entry[0,0,0];
      SetLength(L,2);
      if Action=0 then begin
        L[0]:=Layout(3,2,1,0,0,0,1,1,1,False);
        L[1]:=Layout(2,3,1,0,0,0,2,2,1,True);
      end else for P:=0 to 1 do L[P]:=Layout(3,2,1,0,0,0,1,1,1,True);
      { Fail in the later pass after the complete first replacement is ready. }
      ObserveNeighborPreparation(L,FailureKind,37); Raised:=False;
      try
        if Action=0 then G.ConfigurePassLayouts(L) else G.Reshape(3,2,1);
      except on E: Exception do Raised:=True; end;
      NeighborHooksEnabled:=False;
      Check(Raised,'before/after neighbor failure aborts layout preparation');
      Check(((FailureKind=1) and (NeighborBeforeCalls=37) and (NeighborAfterCalls=36))
        or ((FailureKind=2) and (NeighborBeforeCalls=37) and (NeighborAfterCalls=37)),
        'failure occurs at the requested hook in the later pass, not during earlier preparation');
      Check((Snapshot(G)=Before) and (G.PassGraph[0].Entry[0,0,0]=OldProvider)
        and (G.PassGraph[1].Entry[0,0,0]=OldConsumer),
        'failed later-pass hook retains both old layouts, objects, locks, values, domains and selection');
      Check(G.PassGraph[0].HasDefinition and G.PassGraph[1].HasDefinition,
        'failed later-pass hook retains both definitions');
      ObserveNeighborPreparation(L);
      if Action=0 then G.ConfigurePassLayouts(L) else G.Reshape(3,2,1);
      CheckPreparedNeighborHooks(G,'retry after before/after neighbor failure');
      NeighborHooksEnabled:=False;
      Check(G.TrySolve(DefaultGraphSolveOptions,R),'hook failure clears preparation guard for a valid solve');
    finally NeighborHooksEnabled:=False; G.Free; end;
  end;
end;

procedure TestReentrantStorageFactories;
var G: TGraph; Before: String; Outer,Mode,CatchMode: Integer; Raised: Boolean;
  OldEntry: TGraphEntry; R: TGraphSolveReport;
begin
  for Outer:=0 to 1 do for Mode:=1 to 3 do for CatchMode:=0 to 1 do begin
    FactoryMutation:=0;
    G:=NewPair(Layout(2,1,1,0,0,0,1,1,1,False),Layout(1,1,1,0,0,0,2,1,1,False),True);
    try
      G.PassGraph[0].Entry[0,0,0].Value:='a'; G.SetAllowedValues(0,0,0,'yes');
      Before:=Snapshot(G); OldEntry:=G.PassGraph[0].Entry[0,0,0];
      FactoryGraph:=G; SetLength(FactoryLayouts,2);
      FactoryLayouts[0]:=Layout(3,1,1,0,0,0,1,1,1,False);
      FactoryLayouts[1]:=Layout(2,1,1,0,0,0,2,1,1,False);
      FactoryCalls:=0; FactoryRejections:=0; FactoryMutation:=Mode;
      FactoryCatch:=CatchMode<>0; FactoryInside:=False; Raised:=False;
      try
        if Outer=0 then G.ConfigurePassLayouts(FactoryLayouts) else G.Reshape(3,2,1);
      except on E: EInvalidOperation do Raised:=True; end;
      FactoryMutation:=0;
      Check(FactoryRejections=1,'entry factory rejects recursive configure/reshape/new-pass operation '+IntToStr(Outer)+':'+IntToStr(Mode));
      Check(Raised=(CatchMode=0),'uncaught factory rejection aborts while locally caught rejection permits preparation');
      Check((G.TotalPassCount=2) and (G.CurrentPassIndex=1),'entry factory cannot leak a new pass or changed selection');
      if CatchMode=0 then begin
        Check((Snapshot(G)=Before) and (G.PassGraph[0].Entry[0,0,0]=OldEntry),
          'uncaught factory mutation preserves old layout, domains, values and entry identity');
        G.ConfigurePassLayouts(FactoryLayouts);
      end else begin
        Check(G.PassGraph[0].Entry[0,0,0].Empty and not G.HasAllowedValues(0,0,0),
          'successful outer replacement clears old locks/domains after a caught mutation');
      end;
      Check(G.TrySolve(DefaultGraphSolveOptions,R),'storage preparation flag clears after success or factory failure');
      G.SwitchToPass('after-factory'); G.PassMode:=gpmOverlay;
      Check(G.TotalPassCount=3,'normal pass creation remains available after the guarded operation');
    finally FactoryMutation:=0; FactoryInside:=False; FactoryGraph:=nil; G.Free; end;
  end;
end;

procedure TestQueryValidation;
var G: TGraph; Q: TGraphPassMapQuery; I,N: Integer; L: TWfcLatticeLayouts; Before: String;
begin
  G:=NewPair(Layout(2,1,1,0,0,0,1,1,1,False),Layout(1,1,1,0,0,0,1,1,1,False));
  try
    Before:=Snapshot(G);
    for I:=0 to 11 do begin
      Q:=MakeGraphPassPointQuery(MakeGraphOffset(0,0,0),Values(['a']));
      case I of
        0: begin N:=99; Q.Kind:=TGraphPassMapKind(N); end;
        1: begin N:=99; Q.Match:=TGraphPassMapMatch(N); end;
        2: Q.Values:=nil;
        3: Q.Values:=Values(['']);
        4: Q.MaximumOffset.DeltaX:=1;
        5: Q.MinimumMatches:=1;
        6: begin Q.Match:=gpmmCount; Q.MinimumMatches:=-1; end;
        7: begin Q.Match:=gpmmCount; Q.MinimumMatches:=1; Q.MaximumMatches:=0; end;
        8: begin Q.Match:=gpmmCount; Q.MaximumMatches:=2; end;
        9: begin Q.Kind:=gpmkRegionCoverage; Q.MaximumOffset:=MakeGraphOffset(0,1,1); end;
        10: begin Q.Kind:=gpmkCellCoverage; Q.MaximumOffset.DeltaZ:=1; end;
        11: Q.MaximumMatches:=1;
      end;
      Check(RejectedQuery(G,Q),'malformed mapped query rejected '+IntToStr(I));
      Check((G.DependencyCount=0) and (Snapshot(G)=Before),'malformed registration is atomic '+IntToStr(I));
    end;
    Q:=MakeGraphPassPointQuery(MakeGraphOffset(0,0,0),Values(['a']));
    Check(RejectedQuery(G,Q,'absent'),'missing provider label rejected');
    Check(RejectedQuery(G,Q,'consumer'),'self-requirement rejected');
    SetLength(L,2); L[0]:=G.PassGraph[0].PassLayout;
    L[1]:=Layout(1,1,1,High(Integer)-1,0,0,1,1,1,False); G.ConfigurePassLayouts(L);
    Q:=MakeGraphPassPointQuery(MakeGraphOffset(2,0,0),Values(['a']));
    Check(RejectedQuery(G,Q),'consumer anchor plus offset overflow rejects registration');
    Q:=MakeGraphPassPointQuery(MakeGraphOffset(0,0,0),Values(['a']));
    G.Rules['yes'].RequireMappedFromPass('provider',Q);
    L[1]:=Layout(2,1,1,High(Integer)-2,0,0,1,1,1,False);
    G.ConfigurePassLayouts(L);
    Q:=MakeGraphPassCellQuery(MakeGraphOffset(1,0,0),Values(['a']));
    Check(RejectedQuery(G,Q),'all consumer anchors, not just the first, are preflighted for box overflow');
  finally G.Free; end;
end;

procedure TestMappedConfigurationAndReset;
var G: TGraph; L: TWfcLatticeLayouts; Before: String; R: TGraphSolveReport;
  S: TWfcLatticeLayout; I: Integer;
begin
  G:=NewPair(Layout(2,1,1,0,0,0,1,1,1,False),Layout(1,1,1,0,0,0,1,1,1,False));
  try
    G.Rules['yes'].RequireMappedFromPass('provider',
      MakeGraphPassCellQuery(MakeGraphOffset(1,0,0),Values(['a'])));
    G.PassGraph[0].Entry[0,0,0].Value:='a'; G.PassGraph[0].Entry[1,0,0].Value:='a';
    G.SetAllowedValues(0,0,0,'yes'); Before:=Snapshot(G);
    SetLength(L,2); L[0]:=G.PassGraph[0].PassLayout;
    L[1]:=Layout(1,1,1,High(Integer)-1,0,0,1,1,1,False);
    Check(RejectedLayouts(G,L),'layout update preflights the world extent of already registered mapped boxes');
    Check(Snapshot(G)=Before,'mapped-box overflow cannot partially replace existing storage');
    Check(G.TrySolve(DefaultGraphSolveOptions,R),'rejected layout update retains the original working mapped query');
  finally G.Free; end;
  G:=NewPair(Layout(2,1,1,-4,5,6,2,3,4,True),Layout(1,1,1,-4,5,6,4,3,4,False));
  try
    G.Rules['yes'].RequireMappedFromPass('provider',MakeGraphPassCellQuery(Values(['a','b'])));
    G.WrapNeighbors:=False;
    G.Reshape(3,2,1);
    for I:=0 to 1 do
      Check(EqualLayout(G.PassGraph[I].PassLayout,Layout(3,2,1,0,0,0,1,1,1,False)),
        'nonunit world layout returns to uniform unit pitch, zero origin and global wrap policy');
    Check(G.TrySolve(DefaultGraphSolveOptions,R),'mapped cell semantics remain valid after uniform Reshape');
    G.Reset;
    Check((G.TotalPassCount=1) and (G.CurrentPassIndex=0) and not G.HasDefinition,
      'Reset removes mapped definitions and the heterogeneous pass registry');
    S:=G.PassLayout;
    Check((S.Cells.X=0) and (S.Cells.Y=0) and (S.Cells.Z=0)
      and (S.Origin.X=0) and (S.Origin.Y=0) and (S.Origin.Z=0)
      and (S.Pitch.X=1) and (S.Pitch.Y=1) and (S.Pitch.Z=1) and not S.Wrap,
      'Reset exposes an empty unit-layout sentinel and retains the global wrap setting');
    Check((G.Dimension.Width=0) and (G.Dimension.Height=0) and (G.Dimension.Depth=0),
      'Reset sentinel agrees with historical empty dimensions');
    G.Reshape(1,1,1); G.AddValue('fresh');
    Check(G.TrySolve(DefaultGraphSolveOptions,R) and (G.Entry[0,0,0].Value='fresh'),
      'reset graph solves afresh without a stale mapped dependency');
  finally G.Free; end;
end;

{$IFDEF PAS2JS}
function BadBrowserQueryRejected(const G: TGraph; const ACase: Integer): Boolean;
var Q: TGraphPassMapQuery;
begin
  Q:=MakeGraphPassPointQuery(MakeGraphOffset(0,0,0),Values(['a']));
  case ACase of
    0: asm Q=null; end;
    1: asm Q=undefined; end;
    2: asm Q=[]; end;
    3: asm Q=1; end;
    4: asm Q={}; end;
    5: asm Q.MinimumOffset=null; end;
    6: asm Q.MinimumOffset=undefined; end;
    7: asm Q.MinimumOffset=[]; end;
    8: asm Q.MaximumOffset=false; end;
    9: asm Q.MinimumOffset={DeltaX:0,DeltaY:0}; end;
    10: asm Q.Values=null; end;
    11: asm Q.Values=undefined; end;
    12: asm Q.Values='a'; end;
    13: asm Q.Values={}; end;
    14: asm Q.Values={length:1,0:'a'}; end;
    15: asm Q.Values=[]; end;
    16: asm Q.Values=[1]; end;
    17: asm Q.Values=[null]; end;
    18: asm Q.Values=[true]; end;
    19: asm Q.Values=[{}]; end;
    20: asm Q.Values=new Array(2); end;
    21: asm Q.Values=['a',undefined]; end;
    22: asm Q.Values=['a',false]; end;
    23: asm Q.Values=[['a']]; end;
    24: asm Q.Values=['a',new String('a')]; end;
    25: asm Q.Values=['a','']; end;
  end;
  Result:=RejectedQuery(G,Q);
end;

function BadBrowserLayoutsRejected(const G: TGraph; const ACase: Integer): Boolean;
var L: TWfcLatticeLayouts;
begin
  SetLength(L,2); L[0]:=G.PassGraph[0].PassLayout; L[1]:=G.PassGraph[1].PassLayout;
  case ACase of
    0: asm L=null; end;
    1: asm L=undefined; end;
    2: asm L=0; end;
    3: asm L='xx'; end;
    4: asm L={length:2,0:L[0],1:L[1]}; end;
    5: asm L=new Array(2); end;
    6: asm L[1]=undefined; end;
    7: asm L[1]=null; end;
    8: asm L[1]=[]; end;
    9: asm L[1]={}; end;
  end;
  Result:=RejectedLayouts(G,L);
end;

procedure TestBrowserHostileValues;
var G: TGraph; L: TWfcLatticeLayouts; Q: TGraphPassMapQuery; I,J,N: Integer; Before: String;
begin
  G:=NewPair(Layout(2,1,1,0,0,0,1,1,1,False),Layout(1,1,1,0,0,0,2,1,1,False));
  try
    Before:=Snapshot(G);
    for I:=0 to 25 do begin
      Check(BadBrowserQueryRejected(G,I),'malformed browser query/offset/values container rejected '+IntToStr(I));
      Check((G.DependencyCount=0) and (Snapshot(G)=Before),'malformed query object leaves graph and dependency registry unchanged');
    end;
    for I:=0 to 9 do begin
      Check(BadBrowserLayoutsRejected(G,I),'malformed browser layout array or element rejected '+IntToStr(I));
      Check(Snapshot(G)=Before,'malformed layout array leaves graph unchanged');
    end;
    for I:=0 to 8 do begin
      case I of
        0: asm N=NaN; end;
        1: asm N=Infinity; end;
        2: asm N=-Infinity; end;
        3: asm N=0.5; end;
        4: asm N=undefined; end;
        5: asm N=null; end;
        6: asm N='1'; end;
        7: asm N=4294967296; end;
        8: asm N=true; end;
      end;
      for J:=0 to 4 do begin
        if (J=3) and (I=8) then Continue; { True is a valid Boolean, not an invalid layout. }
        L:=nil; SetLength(L,2); L[0]:=G.PassGraph[0].PassLayout; L[1]:=G.PassGraph[1].PassLayout;
        case J of
          0: L[1].Cells.X:=N;
          1: L[1].Origin.Y:=N;
          2: L[1].Pitch.Z:=N;
          3: asm L[1].Wrap=N; end;
          4: asm L[1].Origin=N; end;
        end;
        Check(RejectedLayouts(G,L),'hostile browser layout field rejected');
        Check(Snapshot(G)=Before,'hostile browser layout leaves complete state');
      end;
      for J:=0 to 4 do begin
        Q:=MakeGraphPassPointQuery(MakeGraphOffset(0,0,0),Values(['a']));
        case J of
          0: Q.MinimumOffset.DeltaX:=N;
          1: Q.MinimumMatches:=N;
          2: Q.MaximumMatches:=N;
          3: asm Q.Kind=N; end;
          4: asm Q.Match=N; end;
        end;
        Check(RejectedQuery(G,Q),'hostile browser mapped field rejected');
        Check((G.DependencyCount=0) and (Snapshot(G)=Before),'hostile browser query remains atomic');
      end;
    end;
  finally G.Free; end;
end;
{$ENDIF}

begin
  RunTest('literal signed XYZ mapping oracle, wrap deduplication and legacy parity',TestLiteralOracle);
  RunTest('real terrain/foliage/housing footprint and heterogeneous storage',TestWorldAndLayouts);
  RunTest('query ownership, idempotence and conjunction',TestCopiesAndConjunction);
  RunTest('public root-parented mapped registration',TestRootParentedRegistration);
  RunTest('mixed-layout selected and concrete pattern captures',TestSelectedPassPatternCapture);
  RunTest('ragged selective rollback and authorized upstream repair',TestSelectiveAndNegotiation);
  RunTest('index-space, copy, uniform reshape and topology guards',TestIndexSpaceAndCopyGuards);
  RunTest('atomic multi-pass allocation and running guards',TestAtomicConfigurationAndRunning);
  RunTest('single requested topology preparation and atomic neighbor hooks',TestSingleRequestedTopologyPreparation);
  RunTest('caught and uncaught reentrant entry factories',TestReentrantStorageFactories);
  RunTest('mapped query preflight and portable world limits',TestQueryValidation);
  RunTest('registered mapped boxes, nonunit Reshape and empty Reset sentinel',TestMappedConfigurationAndReset);
  {$IFDEF PAS2JS}RunTest('strict host-side numeric and record guards',TestBrowserHostileValues);{$ENDIF}
  WriteLn('Mapped-pass checks: ',Checks-Failures,'/',Checks);
  if Failures<>0 then Halt(1);
end.
