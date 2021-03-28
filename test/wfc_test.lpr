program wfc_test;

{$Mode delphi}
{$ModeSwitch nestedprocvars}

uses
  SysUtils,
  wfc;

procedure SimplePrintGraph(const AGraph : TGraph);
var
  X, Z, Y: Integer;
begin
  for Z := 0 to Pred(AGraph.Dimension.Depth) do
  begin
    WriteLn('================',Z,'================');
    for Y := 0 to Pred(AGraph.Dimension.Height) do
    begin
      WriteLn('---------------------------');
      for X := 0 to Pred(AGraph.Dimension.Width) do
        Write(AGraph[X, Y, Z].Value, '|');
      WriteLn('');
    end;
  end;
end;

(*
  tests that when an entry is created all directions are accounted for
  and are nil to start
*)
procedure TestEntryNullNeighbors;
var
  LEntry : TGraphEntry;
  LSuccess: Boolean;
begin
  LEntry := TGraphEntry.Create;
  LSuccess :=
    not Assigned(LEntry[gdNorth])
    and (not Assigned(LEntry[gdEast]))
    and (not Assigned(LEntry[gdSouth]))
    and (not Assigned(LEntry[gdWest]))
    and (not Assigned(LEntry[gdUp]))
    and (not Assigned(LEntry[gdDown]));

  LEntry.Free;

  WriteLn(Format('TestEntryNullNeighbors::[success]-%s', [BoolToStr(LSuccess, True)]));
end;

(*
  tests that a neighbor entry can be set and retrieved
*)
procedure TestEntrySetNeighbor;
var
  LEntry , LNeighbor: TGraphEntry;
  LSuccess: Boolean;
begin
  LEntry := TGraphEntry.Create;
  LNeighbor := TGraphEntry.Create;
  LEntry[gdNorth] := LNeighbor;
  LSuccess := Assigned(LEntry[gdNorth]) and (LNeighbor = LEntry[gdNorth]);

  LEntry.Free;
  LNeighbor.Free;

  WriteLn(Format('TestEntrySetNeighbor::[success]-%s', [BoolToStr(LSuccess, True)]));
end;

(*
  tests that a value entry can be set and retrieved
*)
procedure TestEntrySetValue;
var
  LEntry: TGraphEntry;
  LSuccess: Boolean;
begin
  LEntry := TGraphEntry.Create;
  LEntry.Value := 'test';
  LSuccess := LEntry.Value = 'test';

  LEntry.Free;

  WriteLn(Format('TestEntrySetValue::[success]-%s', [BoolToStr(LSuccess, True)]));
end;

(*
  tests that a rule can be added to a rule group
*)
procedure TestRuleGroupNewRule;
var
  LGroup: TGraphRuleGroup;
  LSuccess: Boolean;
begin
  LGroup := TGraphRuleGroup.Create;
  LGroup.NewRule([gdNorth], 'test');
  LSuccess := LGroup.Exists[gdNorth] and (LGroup[gdNorth].Value[0] = 'test');

  LGroup.Free;

  WriteLn(Format('TestRuleGroupNewRule::[success]-%s', [BoolToStr(LSuccess, True)]));
end;

(*
  tests that multiple rules can be added to a rule group
*)
procedure TestRuleGroupNewMultiRule;
var
  LGroup: TGraphRuleGroup;
  LSuccess: Boolean;
begin
  LGroup := TGraphRuleGroup.Create;
  LGroup.NewRule([gdNorth], 'test').NewRule([gdEast], 'test2');
  LSuccess := LGroup.Exists[gdNorth]
    and (LGroup[gdNorth].Value[0] = 'test')
    and (LGroup[gdEast].Value[0] = 'test2');

  LGroup.Free;

  WriteLn(Format('TestRuleGroupNewMultiRule::[success]-%s', [BoolToStr(LSuccess, True)]));
end;

(*
  tests that a value can be added to the graph
*)
procedure TestGraphAddValue;
var
  LGraph: TGraph;
  LSuccess: Boolean;
  LGroup: TGraphRuleGroup;
begin
  LGraph := TGraph.Create;
  LGroup := LGraph.AddValue('test');
  LSuccess := Assigned(LGroup) and (LGraph.RuleGroups.Count > 0);

  LGraph.Free;

  WriteLn(Format('TestGraphAddValue::[success]-%s', [BoolToStr(LSuccess, True)]));
end;

(*
  tests we can reshape the graph
*)
procedure TestGraphReshape;
var
  LGraph: TGraph;
  LSuccess: Boolean;
begin
  LGraph := TGraph.Create.Reshape({width} 10, {height} 10, {depth} 2);
  LSuccess := Assigned(LGraph.Planes)
    and (LGraph.Planes.Count = 2) //z direction
    and (LGraph.Planes[0].Count = 100) //x * y
    and (LGraph.Dimension.Width = 10)
    and (LGraph.Dimension.Height = 10)
    and (LGraph.Dimension.Depth = 2);

  LGraph.Free;

  WriteLn(Format('TestGraphReshape::[success]-%s', [BoolToStr(LSuccess, True)]));
end;

(*
  tests that neighbors are correctly assigned
*)
procedure TestGraphNeighbors;
var
  LGraph: TGraph;
  LSuccess: Boolean;
  LEntry, LEastNeighbor, LUpNeighbor: TGraphEntry;
begin
  LGraph := TGraph.Create.Reshape({width} 2, {height} 2, {depth} 2);
  LEntry := LGraph[{x} 0, {y} 0, {z} 0];
  LEastNeighbor := LGraph[{x} 1, {y} 0, {z} 0];
  LUpNeighbor := LGraph[{x} 0, {y} 0, {z} 1];

  LSuccess := (LEntry[gdEast].ID = LEastNeighbor.ID)
    and (LEntry[gdUp].ID = LUpNeighbor.ID);

  LGraph.Free;

  WriteLn(Format('TestGraphNeighbors::[success]-%s', [BoolToStr(LSuccess, True)]));
end;

(*
  tests that we can run wfc on the graph in 2 dimensions
*)
procedure TestGraphRun2D;
var
  LGraph: TGraph;
  LSuccess: Boolean;
begin
  LGraph := TGraph.Create.Reshape({width} 5, {height} 5, {depth} 1);

  //default "none" state can be anywhere
  LGraph.AddValue('-').NewRule(AllDirections, ['A', 'B', 'C', 'D', '-']);

  //add simple rules (A & B from any direction)
  LGraph.AddValue('A')
    .NewRule(AllDirections, ['A', 'B']);

  //B & C (E & W), itself everywhere
  LGraph.AddValue('B')
    .NewRule([gdEast, gdWest], 'C')
    .NewRule(AllDirections, 'B');

  //A or B (N & S)
  LGraph.AddValue('C')
    .NewRule([gdNorth, gdSouth], ['A', 'B']);

  //D all alone
  LGraph.AddValue('D').NewRule(AllDirections, 'D');

  //run the graph
  LGraph.Run;

  LSuccess := False;

  WriteLn(Format('TestGraphRun2D::[success]-%s', [BoolToStr(LSuccess, True)]));
  SimplePrintGraph(LGraph);

  LGraph.Free;
end;

(*
  tests that we can run wfc on the graph in 2 dimensions
*)
procedure TestGraphRun3D;
var
  LGraph: TGraph;
  LSuccess: Boolean;
begin
  LGraph := TGraph.Create.Reshape({width} 5, {height} 5, {depth} 3);

  LSuccess := False;

  LGraph.Free;

  WriteLn(Format('TestGraphRun3D::[success]-%s', [BoolToStr(LSuccess, True)]));
end;

(*
  tests when adding a new pass that the TotalPassCount property correctly is updated
*)
procedure TestTotalPassCount;
var
  LGraph: TGraph;
  LSuccess: Boolean;
  I: Integer;
begin
  LGraph := TGraph.Create.Reshape({width} 5, {height} 5, {depth} 3);
  LGraph.SwitchToPass('test', I);

  LSuccess := LGraph.TotalPassCount = 2;

  LGraph.Free;

  WriteLn(Format('TestTotalPassCount::[success]-%s', [BoolToStr(LSuccess, True)]));
end;

(*
  tests when adding a new pass CurrentPass and CurrentPassIndex are correct
  as well as correct when no passes have been added
*)
procedure TestCurrentPassProps;
var
  LGraph: TGraph;
  LSuccess: Boolean;
  I: Integer;
begin
  LGraph := TGraph.Create.Reshape({width} 5, {height} 5, {depth} 3);

  //test default first by labeling and ensuring index is 0
  LGraph.CurrentPass := 'first';
  LSuccess := (LGraph.CurrentPass = 'first') and (LGraph.CurrentPassIndex = 0);

  //if default was successful, then switch to the second pass and test
  if LSuccess then
  begin
    LGraph.SwitchToPass('second', I);
    LSuccess := (LGraph.CurrentPass = 'second') and (LGraph.CurrentPassIndex = I) and (I = 1);
  end;

  LGraph.Free;

  WriteLn(Format('TestCurrentPassProps::[success]-%s', [BoolToStr(LSuccess, True)]));
end;

(*
  tests when adding a new pass that the TotalPassCount property correctly is updated
*)
procedure TestForEachPass;
var
  LGraph: TGraph;
  LSuccess: Boolean;
  I: Integer;
  LLoopCount : Integer;

  procedure Loop(const AGraph : TGraph; const APass : String; const AIndex : Integer);
  begin
    Inc(LLoopCount);
  end;

begin
  LGraph := TGraph.Create.Reshape({width} 5, {height} 5, {depth} 3);
  LGraph.SwitchToPass('test', I);
  LLoopCount := 0;

  LGraph.ForEachPass(Loop);
  LSuccess := LLoopCount = 2;

  LGraph.Free;

  WriteLn(Format('TestForEachPass::[success]-%s', [BoolToStr(LSuccess, True)]));
end;

begin
  TestEntryNullNeighbors;
  TestEntrySetNeighbor;
  TestEntrySetValue;
  TestRuleGroupNewRule;
  TestRuleGroupNewMultiRule;
  TestGraphAddValue;
  TestGraphReshape;
  TestGraphNeighbors;
  TestTotalPassCount;
  TestCurrentPassProps;
  TestForEachPass;
  TestGraphRun2D; //todo - fully implement test
  TestGraphRun3D; //todo - fully implement test


  //wait for user to close
  ReadLn;
end.

