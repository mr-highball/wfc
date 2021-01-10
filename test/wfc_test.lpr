program wfc_test;

uses
  SysUtils,
  wfc;

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
  LGroup.NewRule(gdNorth, 'test');
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
  LGroup.NewRule(gdNorth, 'test').NewRule(gdEast, 'test2');
  LSuccess := LGroup.Exists[gdNorth]
    and (LGroup[gdNorth].Value[0] = 'test')
    and (LGroup[gdEast].Value[0] = 'test2');

  LGroup.Free;

  WriteLn(Format('TestRuleGroupNewMultiRule::[success]-%s', [BoolToStr(LSuccess, True)]));
end;

begin
  TestEntryNullNeighbors;
  TestEntrySetNeighbor;
  TestEntrySetValue;
  TestRuleGroupNewRule;
  TestRuleGroupNewMultiRule;

  //wait for user to close
  ReadLn;
end.

