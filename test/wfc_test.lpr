program wfc_test;

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
  tests that when a rule requirement is specified it is enforced
*)
procedure TestRequireRule;
var
  LGraph: TGraph;
  LSuccess: Boolean;
  LEntry, LEastNeighbor, LUpNeighbor, LOrphan: TGraphEntry;
begin
  //initialize the graph
  LGraph := TGraph.Create.Reshape({width} 2, {height} 2, {depth} 2);
  LGraph.WrapNeighbors := False;

  //initialize our 'seed' entry with a rule that requires a specific neighbor set
  LGraph.AddValue('0').NewRule([gdDown], 'U', True).NewRule([gdWest], 'E', True);
  LEntry := LGraph[{x} 0, {y} 0, {z} 0];
  LEntry.Value := '0';

  //add the "required" rules for the neighbors
  LGraph.AddValue('E').NewRule([gdEast], '0', True); //must be east of 0
  LGraph.AddValue('U').NewRule([gdUp], '0', True); //must be up from 0

  LEastNeighbor := LEntry.Neighbor[gdEast];
  LUpNeighbor := LEntry.Neighbor[gdUp];
  LOrphan := LUpNeighbor.Neighbor[gdEast];

  //run the graph with our "seeded" entry value
  LGraph.Run;

  LSuccess := (LEastNeighbor.Value = 'E')
    and (LUpNeighbor.Value = 'U');

  //SimplePrintGraph(LGraph); //debug

  LGraph.Free;

  WriteLn(Format('TestRequireRule::[success]-%s', [BoolToStr(LSuccess, True)]));
end;

(*
  tests that we can run wfc on the graph in 2 dimensions
  todo - fix test
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
  todo - implement test
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

begin
  TestEntryNullNeighbors;
  TestEntrySetNeighbor;
  TestEntrySetValue;
  TestRuleGroupNewRule;
  TestRuleGroupNewMultiRule;
  TestGraphAddValue;
  TestGraphReshape;
  TestGraphNeighbors;
  TestRequireRule;

  //TestGraphRun2D;
  //TestGraphRun3D;


  //wait for user to close
  ReadLn;
end.

