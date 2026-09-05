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
program wfc_connectivity_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  Classes, SysUtils, wfc;

type
  TTestProcedure = procedure;

var
  GChecks, GFailures: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GChecks);
  if ACondition then
    WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailures);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try
    ATest;
  except
    on E: Exception do
    begin
      Inc(GFailures);
      WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function Pos(const X, Y, Z: TGraphCoordinate): TGraphPosition;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function SamePosition(const A, B: TGraphPosition): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.Z = B.Z);
end;

function HasValue(const AValues: TGraphValues;
  const AValue: TGraphValue): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AValues) do
    if AValues[I] = AValue then
      Exit(True);
  Result := False;
end;

function NewGraph(const AWidth, AHeight, ADepth: TGraphCoordinate): TGraph;
begin
  Result := TGraph.Create.Reshape(AWidth, AHeight, ADepth);
  Result.WrapNeighbors := False;
end;

function GraphState(const AGraph: TGraph): String;
var
  E: TGraphEntry;
  LAllowed: TGraphValues;
  P, X, Y, Z, I: Integer;
  LPass: TGraph;
begin
  Result := Format('%d,%d,%d@%d#', [Integer(AGraph.Dimension.Width),
    Integer(AGraph.Dimension.Height), Integer(AGraph.Dimension.Depth),
    AGraph.CurrentPassIndex]);
  for P := 0 to AGraph.TotalPassCount - 1 do
  begin
    LPass := AGraph.PassGraph[P];
    Result := Result + IntToStr(P) + '{';
    for Z := 0 to Integer(LPass.Dimension.Depth) - 1 do
      for Y := 0 to Integer(LPass.Dimension.Height) - 1 do
        for X := 0 to Integer(LPass.Dimension.Width) - 1 do
        begin
          E := LPass.Entry[X, Y, Z];
          Result := Result + IntToStr(Length(E.Value)) + ':' + E.Value + ':'
            + IntToStr(Ord(E.Empty)) + ':' + IntToStr(Ord(E.Generated)) + ':'
            + IntToStr(Ord(LPass.HasAllowedValues(X, Y, Z))) + '[';
          LAllowed := LPass.CopyAllowedValues(X, Y, Z);
          for I := 0 to High(LAllowed) do
            Result := Result + IntToStr(Length(LAllowed[I])) + ':'
              + LAllowed[I] + ',';
          Result := Result + '];';
        end;
    Result := Result + '}';
  end;
end;

function ProfileIndex(const AConstraint: TGraphConnectivityConstraint;
  const AValue: TGraphValue): Integer;
var
  I: Integer;
begin
  for I := 0 to High(AConstraint.Values) do
    if AConstraint.Values[I].Value = AValue then
      Exit(I);
  Result := -1;
end;

function RuleAllows(const AGraph: TGraph; const ASource: TGraphValue;
  const ADirection: TGraphDirection; const ATarget: TGraphValue): Boolean;
var
  LGroup: TGraph.TParentedGraphRuleGroup;
  LRule: TGraphRule;
begin
  LGroup := AGraph.Rules[ASource];
  if LGroup.Denied[ADirection] then
    Exit(False);
  if not LGroup.Exists[ADirection] then
    Exit(True);
  LRule := LGroup.Rule[ADirection];
  Result := (Length(LRule.Value) = 0) or HasValue(LRule.Value, ATarget);
end;

//This is intentionally outside the numeric solver. It traverses only the
//committed public entries, neighbor links, public rules, and copied descriptor.
function ExactConnectivity(const AGraph: TGraph;
  const AConstraint: TGraphConnectivityConstraint): Boolean;
var
  C, D, Head, I, P, RootIndex, Tail: Integer;
  Current, Neighbor: TGraphEntry;
  Direction, Reverse: TGraphDirection;
  Queue: array of Integer;
  Reached: array of Byte;
begin
  Result := False;
  RootIndex := AGraph.Entry[AConstraint.Root.X, AConstraint.Root.Y,
    AConstraint.Root.Z].Index;
  if ProfileIndex(AConstraint,
    AGraph.Entry[AConstraint.Root.X, AConstraint.Root.Y,
      AConstraint.Root.Z].Value) < 0 then
    Exit;

  C := Integer(AGraph.Dimension.Width) * Integer(AGraph.Dimension.Height)
    * Integer(AGraph.Dimension.Depth);
  SetLength(Queue, C);
  SetLength(Reached, C);
  Head := 0;
  Tail := 1;
  Queue[0] := RootIndex;
  Reached[RootIndex] := 1;
  while Head < Tail do
  begin
    C := Queue[Head];
    Inc(Head);
    Current := AGraph.Entry[
      C mod Integer(AGraph.Dimension.Width),
      (C div Integer(AGraph.Dimension.Width))
        mod Integer(AGraph.Dimension.Height),
      C div (Integer(AGraph.Dimension.Width)
        * Integer(AGraph.Dimension.Height))];
    P := ProfileIndex(AConstraint, Current.Value);
    if P < 0 then
      Continue;
    for Direction := Low(TGraphDirection) to High(TGraphDirection) do
    begin
      if not (Direction in AConstraint.Values[P].Openings) then
        Continue;
      Neighbor := Current.Neighbor[Direction];
      if not Assigned(Neighbor) or (Neighbor = Current) then
        Continue;
      Reverse := InverseOfDir(Direction);
      if Neighbor.Neighbor[Reverse] <> Current then
        Continue;
      D := ProfileIndex(AConstraint, Neighbor.Value);
      if (D < 0) or not (Reverse in AConstraint.Values[D].Openings) then
        Continue;
      if not RuleAllows(AGraph, Current.Value, Direction, Neighbor.Value)
        or not RuleAllows(AGraph, Neighbor.Value, Reverse, Current.Value) then
        Continue;
      if Reached[Neighbor.Index] = 0 then
      begin
        Reached[Neighbor.Index] := 1;
        Queue[Tail] := Neighbor.Index;
        Inc(Tail);
      end;
    end;
  end;

  for I := 0 to High(AConstraint.RequiredPositions) do
    if Reached[AGraph.Entry[AConstraint.RequiredPositions[I].X,
      AConstraint.RequiredPositions[I].Y,
      AConstraint.RequiredPositions[I].Z].Index] = 0 then
      Exit;

  C := 0;
  for I := 0 to Integer(AGraph.Dimension.Depth) - 1 do
    for D := 0 to Integer(AGraph.Dimension.Height) - 1 do
      for Head := 0 to Integer(AGraph.Dimension.Width) - 1 do
      begin
        Current := AGraph.Entry[Head, D, I];
        P := ProfileIndex(AConstraint, Current.Value);
        if (P >= 0) and (AConstraint.RequireAllParticipants
          or AConstraint.Values[P].RequiredByValue) and (Reached[C] = 0) then
          Exit;
        Inc(C);
      end;
  Result := True;
end;

function RequireRejected(const AGraph: TGraph;
  const AConstraint: TGraphConnectivityConstraint): Boolean;
begin
  Result := False;
  try
    AGraph.RequireConnectivity(AConstraint);
  except
    on E: Exception do Result := True;
  end;
end;

function InvalidOpenings: TGraphDirections;
begin
  {$IFDEF PAS2JS}
  Result := [];
  asm Result = {6: true}; end;
  {$ELSE}
  Result := [];
  PByte(@Result)^ := $40;
  {$ENDIF}
end;

function InvalidBoolean: Boolean;
begin
  {$IFDEF PAS2JS}
  Result := False;
  asm Result = 2; end;
  {$ELSE}
  Result := False;
  PByte(@Result)^ := 2;
  {$ENDIF}
end;

{$IFDEF PAS2JS}
function MalformedCoordinate(const AIndex: Integer): TGraphCoordinate;
begin
  Result := 0;
  asm
    if (AIndex === 0) Result = 0.5;
    else if (AIndex === 1) Result = NaN;
    else if (AIndex === 2) Result = Infinity;
    else Result = 9007199254740992;
  end;
end;
{$ENDIF}

function TwoCellConstraint(const ALabel: String): TGraphConnectivityConstraint;
begin
  Result := MakeGraphConnectivityConstraint(ALabel, Pos(0, 0, 0),
    [Pos(1, 0, 0)],
    [MakeGraphConnectivityValue('east', [gdEast]),
     MakeGraphConnectivityValue('west', [gdWest])]);
end;

procedure TestFactoriesCopiesAndPasses;
var
  G: TGraph;
  C, Again, Upper: TGraphConnectivityConstraint;
  InputPositions: TGraphPositions;
  InputValues: TGraphConnectivityValues;
  Saved: TGraphConnectivityConstraints;
begin
  G := NewGraph(3, 2, 1);
  try
    G.AddValue('none');
    G.AddValue('first');
    G.AddValue('second');
    G.AddValue('third');
    SetLength(InputPositions, 4);
    InputPositions[0] := Pos(2, 1, 0);
    InputPositions[1] := Pos(0, 0, 0);
    InputPositions[2] := Pos(2, 1, 0);
    InputPositions[3] := Pos(1, 0, 0);
    SetLength(InputValues, 3);
    InputValues[0] := MakeGraphConnectivityValue('third', [gdUp], True);
    InputValues[1] := MakeGraphConnectivityValue('first', [gdEast]);
    InputValues[2] := MakeGraphConnectivityValue('second', []);
    C := MakeGraphConnectivityConstraint('route', Pos(2, 0, 0),
      InputPositions, InputValues);

    InputPositions[0] := Pos(1, 1, 0);
    InputValues[0].Value := 'none';
    Include(InputValues[1].Openings, gdDown);
    Check(SamePosition(C.RequiredPositions[0], Pos(2, 1, 0))
      and (C.Values[0].Value = 'third')
      and not (gdDown in C.Values[1].Openings),
      'constraint factory detaches both nested arrays and direction sets');
    Check(G.RequireConnectivity(C) = G, 'RequireConnectivity is fluent');

    C.LabelText := 'mutated';
    C.Root := Pos(0, 0, 0);
    C.RequiredPositions[0] := Pos(1, 1, 0);
    C.Values[0].Value := 'none';
    Include(C.Values[1].Openings, gdDown);
    C.RequireAllParticipants := True;
    Saved := G.CopyConnectivityConstraints;
    Check((Length(Saved) = 1) and (Saved[0].LabelText = 'route')
      and SamePosition(Saved[0].Root, Pos(2, 0, 0))
      and not Saved[0].RequireAllParticipants,
      'registration detaches scalar descriptor metadata');
    Check((Length(Saved[0].Values) = 3)
      and (Saved[0].Values[0].Value = 'first')
      and (Saved[0].Values[1].Value = 'second')
      and (Saved[0].Values[2].Value = 'third')
      and (Saved[0].Values[0].Openings = [gdEast])
      and (Saved[0].Values[1].Openings = [])
      and (Saved[0].Values[2].Openings = [gdUp])
      and Saved[0].Values[2].RequiredByValue,
      'profiles are copied and canonicalized to AddValue order');
    Check((Length(Saved[0].RequiredPositions) = 3)
      and SamePosition(Saved[0].RequiredPositions[0], Pos(0, 0, 0))
      and SamePosition(Saved[0].RequiredPositions[1], Pos(1, 0, 0))
      and SamePosition(Saved[0].RequiredPositions[2], Pos(2, 1, 0)),
      'required positions are deduplicated in flattened cell order');

    Saved[0].LabelText := 'copy mutation';
    Saved[0].RequiredPositions[0] := Pos(2, 0, 0);
    Saved[0].Values[0].Value := 'none';
    Include(Saved[0].Values[1].Openings, gdSouth);
    Saved := G.CopyConnectivityConstraints;
    Check((Saved[0].LabelText = 'route')
      and SamePosition(Saved[0].RequiredPositions[0], Pos(0, 0, 0))
      and (Saved[0].Values[0].Value = 'first')
      and not (gdSouth in Saved[0].Values[1].Openings),
      'copied constraints detach nested arrays and sets from graph storage');

    Again := MakeGraphConnectivityConstraint('route', Pos(2, 0, 0),
      [Pos(1, 0, 0), Pos(2, 1, 0), Pos(0, 0, 0), Pos(1, 0, 0)],
      [MakeGraphConnectivityValue('third', [gdUp], True),
       MakeGraphConnectivityValue('second', []),
       MakeGraphConnectivityValue('first', [gdEast])]);
    Check(G.RequireConnectivity(Again) = G,
      'semantically identical labeled registration is fluent and idempotent');
    Check(Length(G.CopyConnectivityConstraints) = 1,
      'idempotent registration does not append a duplicate');
    Again.RequireAllParticipants := True;
    Check(RequireRejected(G, Again)
      and (Length(G.CopyConnectivityConstraints) = 1),
      'a conflicting duplicate label rejects without replacing the original');
    Check((G.RemoveConnectivity('missing') = G)
      and (Length(G.CopyConnectivityConstraints) = 1),
      'removing an unknown label is a fluent no-op');

    G.SwitchToPass('upper');
    G.AddValue('none');
    G.AddValue('first');
    G.AddValue('second');
    G.AddValue('third');
    Check(Length(G.CopyConnectivityConstraints) = 0,
      'a newly created pass has no inherited connectivity descriptors');
    Upper := MakeGraphConnectivityConstraint('route', Pos(0, 0, 0), nil,
      [MakeGraphConnectivityValue('second', [])]);
    Check(G.RequireConnectivity(Upper) = G,
      'the same label may have a different definition on another pass');
    Check((Length(G.CopyConnectivityConstraints) = 1)
      and (Length(G.PassGraph[0].CopyConnectivityConstraints) = 1),
      'root and PassGraph access each address their intended pass');
    Check(G.RemoveConnectivity('route') = G,
      'RemoveConnectivity is fluent on the selected pass');
    Check((Length(G.CopyConnectivityConstraints) = 0)
      and (Length(G.PassGraph[0].CopyConnectivityConstraints) = 1),
      'removal is pass-local');
    G.RequireConnectivity(Upper);
    Check((G.ClearConnectivity = G)
      and (Length(G.CopyConnectivityConstraints) = 0)
      and (Length(G.PassGraph[0].CopyConnectivityConstraints) = 1),
      'ClearConnectivity is fluent and pass-local');
    Check(G.PassGraph[0].RemoveConnectivity('route') = G.PassGraph[0],
      'a PassGraph mutation stays fluent on that pass object');
    Check(Length(G.PassGraph[0].CopyConnectivityConstraints) = 0,
      'PassGraph removal does not depend on root selection');
  finally
    G.Free;
  end;
end;

procedure TestValidationAndAtomicRegistration;
var
  G: TGraph;
  Base, Bad: TGraphConnectivityConstraint;
  BeforeCount: Integer;
  {$IFDEF PAS2JS}I: Integer;{$ENDIF}
begin
  G := NewGraph(2, 1, 1);
  try
    G.AddValue('none');
    G.AddValue('east');
    G.AddValue('west');
    Base := TwoCellConstraint('base');
    G.RequireConnectivity(Base);
    BeforeCount := Length(G.CopyConnectivityConstraints);

    Bad := TwoCellConstraint('');
    Check(RequireRejected(G, Bad), 'an empty label rejects');
    Bad := TwoCellConstraint('bad-root');
    Bad.Root := Pos(2, 0, 0);
    Check(RequireRejected(G, Bad), 'an out-of-bounds root rejects');
    Bad := TwoCellConstraint('bad-terminal');
    Bad.RequiredPositions := [Pos(0, 1, 0)];
    Check(RequireRejected(G, Bad), 'an out-of-bounds required position rejects');
    Bad := TwoCellConstraint('no-profiles');
    Bad.Values := nil;
    Check(RequireRejected(G, Bad), 'an empty profile list rejects');
    Bad := TwoCellConstraint('unknown-profile');
    Bad.Values[0].Value := 'outside';
    Check(RequireRejected(G, Bad), 'an unregistered value profile rejects');
    Bad := TwoCellConstraint('empty-profile');
    Bad.Values[0].Value := TGraphValue.Empty;
    Check(RequireRejected(G, Bad), 'the reserved empty value profile rejects');
    Bad := TwoCellConstraint('duplicate-profile');
    Bad.Values[1].Value := Bad.Values[0].Value;
    Check(RequireRejected(G, Bad), 'duplicate value profiles reject');
    Bad := TwoCellConstraint('bad-directions');
    Bad.Values[0].Openings := InvalidOpenings;
    Check(RequireRejected(G, Bad), 'bits outside the direction set reject');
    Bad := TwoCellConstraint('bad-all-flag');
    Bad.RequireAllParticipants := InvalidBoolean;
    Check(RequireRejected(G, Bad), 'a malformed all-participants flag rejects');
    Bad := TwoCellConstraint('bad-required-flag');
    Bad.Values[0].RequiredByValue := InvalidBoolean;
    Check(RequireRejected(G, Bad), 'a malformed required-by-value flag rejects');
    Check(Length(G.CopyConnectivityConstraints) = BeforeCount,
      'all rejected descriptors leave existing registration untouched');

    Bad := MakeGraphConnectivityConstraint('zero-ports', Pos(0, 0, 0), nil,
      [MakeGraphConnectivityValue('east', [])]);
    G.RequireConnectivity(Bad);
    Check(Length(G.CopyConnectivityConstraints) = BeforeCount + 1,
      'a zero-opening profile is a valid isolated participant');

    {$IFDEF PAS2JS}
    for I := 0 to 3 do
    begin
      Bad := TwoCellConstraint('host-coordinate-' + IntToStr(I));
      Bad.Root.X := MalformedCoordinate(I);
      Check(RequireRejected(G, Bad),
        'malformed browser coordinate rejects before registration ' + IntToStr(I));
    end;
    {$ENDIF}
    Check(Length(G.CopyConnectivityConstraints) = BeforeCount + 1,
      'numeric validation failures do not partially register descriptors');
  finally
    G.Free;
  end;
end;

procedure TestReshapeAndReset;
var
  G: TGraph;
  C: TGraphConnectivityConstraint;
  FirstEntry, SecondEntry: TGraphEntry;
  Raised: Boolean;
begin
  G := NewGraph(3, 2, 1);
  try
    G.AddValue('east');
    G.AddValue('west');
    C := MakeGraphConnectivityConstraint('wide', Pos(2, 1, 0),
      [Pos(0, 0, 0)],
      [MakeGraphConnectivityValue('east', [gdEast]),
       MakeGraphConnectivityValue('west', [gdWest])]);
    G.RequireConnectivity(C);
    G.Entry[0, 0, 0].Value := 'east';
    FirstEntry := G.Entry[0, 0, 0];
    G.SwitchToPass('second');
    G.AddValue('east');
    G.AddValue('west');
    G.RequireConnectivity(MakeGraphConnectivityConstraint('small',
      Pos(0, 0, 0), nil,
      [MakeGraphConnectivityValue('east', [])]));
    G.Entry[1, 0, 0].Value := 'west';
    SecondEntry := G.Entry[1, 0, 0];

    Raised := False;
    try
      G.Reshape(2, 2, 1);
    except
      on E: ERangeError do Raised := True;
    end;
    Check(Raised, 'reshape rejects a shape that invalidates any pass descriptor');
    Check((G.Dimension.Width = 3) and (G.Dimension.Height = 2)
      and (G.Dimension.Depth = 1) and (G.CurrentPassIndex = 1),
      'rejected reshape preserves root dimensions and selected pass');
    Check((G.PassGraph[0].Entry[0, 0, 0] = FirstEntry)
      and (FirstEntry.Value = 'east')
      and (G.PassGraph[1].Entry[1, 0, 0] = SecondEntry)
      and (SecondEntry.Value = 'west'),
      'rejected reshape preserves every pass storage object and caller value');
    Check((Length(G.PassGraph[0].CopyConnectivityConstraints) = 1)
      and (Length(G.PassGraph[1].CopyConnectivityConstraints) = 1),
      'rejected reshape preserves every pass descriptor');

    Check(G.Reshape(4, 3, 1) = G, 'a compatible reshape remains fluent');
    Check((G.Dimension.Width = 4) and (G.Dimension.Height = 3)
      and (Length(G.PassGraph[0].CopyConnectivityConstraints) = 1)
      and (Length(G.PassGraph[1].CopyConnectivityConstraints) = 1),
      'compatible reshape retains pass-local descriptors');
    Check(G.Reset = G, 'Reset remains fluent');
    Check((G.TotalPassCount = 1) and (G.Dimension.Width = 0)
      and (G.Dimension.Height = 0) and (G.Dimension.Depth = 0)
      and (Length(G.CopyRegisteredValues) = 0)
      and (Length(G.CopyConnectivityConstraints) = 0),
      'Reset removes dimensions, definitions, passes, and connectivity');
  finally
    G.Free;
  end;
end;

procedure TestLegacyRunPreflight;
var
  G, Twin: TGraph;
  C: TGraphConnectivityConstraint;
  Before: String;
  Raised: Boolean;
begin
  G := NewGraph(2, 1, 1);
  Twin := NewGraph(2, 1, 1);
  try
    G.AddValue('east');
    G.AddValue('west');
    Twin.AddValue('east');
    Twin.AddValue('west');
    G.Seed := 918273;
    Twin.Seed := 918273;
    C := MakeGraphConnectivityConstraint('route', Pos(0, 0, 0),
      [Pos(1, 0, 0)],
      [MakeGraphConnectivityValue('east', [gdEast]),
       MakeGraphConnectivityValue('west', [gdWest])]);
    G.RequireConnectivity(C);
    G.Entry[0, 0, 0].Value := 'east';
    G.Entry[1, 0, 0].Value := 'west';
    Twin.Entry[0, 0, 0].Value := 'east';
    Twin.Entry[1, 0, 0].Value := 'west';
    Before := GraphState(G);
    Raised := False;
    try
      G.Run;
    except
      on E: EInvalidOperation do Raised := True;
    end;
    Check(Raised, 'legacy Run rejects graphs carrying connectivity');
    Check((GraphState(G) = Before)
      and (Length(G.CopyConnectivityConstraints) = 1),
      'legacy Run rejects before entry or descriptor mutation');
    Check(G.RandomIndex(1000000) = Twin.RandomIndex(1000000),
      'legacy Run rejection does not advance the pass random stream');
    G.ClearConnectivity;
    Raised := False;
    try
      G.Run;
    except
      on E: Exception do Raised := True;
    end;
    Check(not Raised, 'legacy Run remains available after connectivity is cleared');
  finally
    Twin.Free;
    G.Free;
  end;
end;

procedure TestSimpleRoutesAndPublicBfs;
var
  G: TGraph;
  C: TGraphConnectivityConstraint;
  Report: TGraphSolveReport;
begin
  G := NewGraph(3, 1, 1);
  try
    G.AddValue('start-east');
    G.AddValue('bridge');
    G.AddValue('finish-west');
    C := MakeGraphConnectivityConstraint('line', Pos(0, 0, 0),
      [Pos(2, 0, 0)],
      [MakeGraphConnectivityValue('start-east', [gdEast]),
       MakeGraphConnectivityValue('bridge', [gdEast, gdWest]),
       MakeGraphConnectivityValue('finish-west', [gdWest])]);
    G.RequireConnectivity(C);
    G.SetAllowedValues(0, 0, 0, 'start-east');
    G.SetAllowedValues(1, 0, 0, 'bridge');
    G.SetAllowedValues(2, 0, 0, 'finish-west');
    Check(G.TrySolve(DefaultGraphSolveOptions, Report)
      and (Report.Status = gssSolved), 'a forced one-dimensional bottleneck solves');
    Check(ExactConnectivity(G, G.CopyConnectivityConstraints[0]),
      'independent public BFS validates the committed bottleneck');
  finally
    G.Free;
  end;

  G := NewGraph(2, 2, 1);
  try
    G.AddValue('none');
    G.AddValue('start-north');
    G.AddValue('turn-south-east');
    G.AddValue('finish-west');
    C := MakeGraphConnectivityConstraint('flat-turn', Pos(0, 0, 0),
      [Pos(1, 1, 0)],
      [MakeGraphConnectivityValue('start-north', [gdNorth]),
       MakeGraphConnectivityValue('turn-south-east', [gdSouth, gdEast]),
       MakeGraphConnectivityValue('finish-west', [gdWest])]);
    G.RequireConnectivity(C);
    G.SetAllowedValues(0, 0, 0, 'start-north');
    G.SetAllowedValues(0, 1, 0, 'turn-south-east');
    G.SetAllowedValues(1, 1, 0, 'finish-west');
    G.SetAllowedValues(1, 0, 0, 'none');
    Check(G.TrySolve(DefaultGraphSolveOptions, Report),
      'a two-axis turn solves through reciprocal ports');
    Check(ExactConnectivity(G, G.CopyConnectivityConstraints[0]),
      'independent public BFS validates the two-axis turn');
  finally
    G.Free;
  end;

  G := NewGraph(2, 1, 2);
  try
    G.AddValue('none');
    G.AddValue('start-up');
    G.AddValue('turn-down-east');
    G.AddValue('finish-west');
    C := MakeGraphConnectivityConstraint('raised-turn', Pos(0, 0, 0),
      [Pos(1, 0, 1)],
      [MakeGraphConnectivityValue('start-up', [gdUp]),
       MakeGraphConnectivityValue('turn-down-east', [gdDown, gdEast]),
       MakeGraphConnectivityValue('finish-west', [gdWest])]);
    G.RequireConnectivity(C);
    G.SetAllowedValues(0, 0, 0, 'start-up');
    G.SetAllowedValues(0, 0, 1, 'turn-down-east');
    G.SetAllowedValues(1, 0, 1, 'finish-west');
    G.SetAllowedValues(1, 0, 0, 'none');
    Check(G.TrySolve(DefaultGraphSolveOptions, Report),
      'a depth-changing turn solves through up/down ports');
    Check(ExactConnectivity(G, G.CopyConnectivityConstraints[0]),
      'independent public BFS validates the depth-changing turn');
  finally
    G.Free;
  end;
end;

function NewIslandGraph(const ARequiredByValue, ARequireAll,
  APlaceIsland: Boolean; const ARequireGap: Boolean = False): TGraph;
var
  Required: TGraphPositions;
begin
  Result := NewGraph(4, 1, 1);
  Result.AddValue('none');
  Result.AddValue('start');
  Result.AddValue('finish');
  Result.AddValue('island');
  Required := [Pos(1, 0, 0)];
  if ARequireGap then
    Required := [Pos(1, 0, 0), Pos(2, 0, 0)];
  Result.RequireConnectivity(MakeGraphConnectivityConstraint('islands',
    Pos(0, 0, 0), Required,
    [MakeGraphConnectivityValue('start', [gdEast]),
     MakeGraphConnectivityValue('finish', [gdWest]),
     MakeGraphConnectivityValue('island', [], ARequiredByValue)], ARequireAll));
  Result.SetAllowedValues(0, 0, 0, 'start');
  Result.SetAllowedValues(1, 0, 0, 'finish');
  Result.SetAllowedValues(2, 0, 0, 'none');
  if APlaceIsland then
    Result.SetAllowedValues(3, 0, 0, 'island')
  else
    Result.SetAllowedValues(3, 0, 0, 'none');
end;

procedure TestOptionalRequiredAndAllParticipants;
var
  G: TGraph;
  Report: TGraphSolveReport;
begin
  G := NewIslandGraph(False, False, True);
  try
    Check(G.TrySolve(DefaultGraphSolveOptions, Report),
      'an optional disconnected participant does not invalidate the route');
    Check(ExactConnectivity(G, G.CopyConnectivityConstraints[0]),
      'public BFS treats an optional disconnected participant as optional');
  finally
    G.Free;
  end;

  G := NewIslandGraph(True, False, True);
  try
    Check(not G.TrySolve(DefaultGraphSolveOptions, Report)
      and (Report.Status = gssContradiction)
      and (Report.Contradiction.Kind = gckConnectivity)
      and (Report.Contradiction.ConstraintIndex = 0),
      'an occurring required-by-value island makes the model impossible');
  finally
    G.Free;
  end;

  G := NewIslandGraph(True, False, False);
  try
    Check(G.TrySolve(DefaultGraphSolveOptions, Report),
      'required-by-value is vacuous when that profile does not occur');
    Check(ExactConnectivity(G, G.CopyConnectivityConstraints[0]),
      'public BFS agrees on the vacuous required-by-value case');
  finally
    G.Free;
  end;

  G := NewIslandGraph(False, True, True);
  try
    Check(not G.TrySolve(DefaultGraphSolveOptions, Report)
      and (Report.Contradiction.Kind = gckConnectivity),
      'all-participants mode rejects an otherwise optional island');
  finally
    G.Free;
  end;

  G := NewIslandGraph(False, False, False, True);
  try
    Check(not G.TrySolve(DefaultGraphSolveOptions, Report)
      and (Report.Contradiction.Kind = gckConnectivity)
      and (Report.Contradiction.EntryIndex = 2),
      'a fixed required position must select a participating value');
  finally
    G.Free;
  end;
end;

procedure ConfigureThreeCellLine(const AGraph: TGraph);
begin
  AGraph.AddValue('start');
  AGraph.AddValue('middle');
  AGraph.AddValue('finish');
  AGraph.SetAllowedValues(0, 0, 0, 'start');
  AGraph.SetAllowedValues(1, 0, 0, 'middle');
  AGraph.SetAllowedValues(2, 0, 0, 'finish');
end;

procedure TestMultipleConstraintsAndReciprocity;
var
  G: TGraph;
  C0, C1: TGraphConnectivityConstraint;
  Copies: TGraphConnectivityConstraints;
  Report: TGraphSolveReport;
begin
  C0 := MakeGraphConnectivityConstraint('end-to-end', Pos(0, 0, 0),
    [Pos(2, 0, 0)],
    [MakeGraphConnectivityValue('start', [gdEast]),
     MakeGraphConnectivityValue('middle', [gdEast, gdWest]),
     MakeGraphConnectivityValue('finish', [gdWest])]);
  C1 := MakeGraphConnectivityConstraint('middle-local', Pos(1, 0, 0), nil,
    [MakeGraphConnectivityValue('middle', [])]);
  G := NewGraph(3, 1, 1);
  try
    ConfigureThreeCellLine(G);
    G.RequireConnectivity(C0);
    G.RequireConnectivity(C1);
    Check(G.TrySolve(DefaultGraphSolveOptions, Report),
      'multiple pass-local connectivity constraints compose as a satisfiable AND');
    Copies := G.CopyConnectivityConstraints;
    Check((Length(Copies) = 2) and ExactConnectivity(G, Copies[0])
      and ExactConnectivity(G, Copies[1]),
      'public BFS independently validates every satisfied constraint');
  finally
    G.Free;
  end;

  G := NewGraph(3, 1, 1);
  try
    ConfigureThreeCellLine(G);
    G.RequireConnectivity(C0);
    C1 := MakeGraphConnectivityConstraint('second-impossible', Pos(0, 0, 0),
      [Pos(2, 0, 0)],
      [MakeGraphConnectivityValue('start', []),
       MakeGraphConnectivityValue('middle', []),
       MakeGraphConnectivityValue('finish', [])]);
    G.RequireConnectivity(C1);
    Check(not G.TrySolve(DefaultGraphSolveOptions, Report)
      and (Report.Contradiction.Kind = gckConnectivity)
      and (Report.Contradiction.ConstraintIndex = 1),
      'a later impossible constraint rejects an assignment satisfying the first');
  finally
    G.Free;
  end;

  G := NewGraph(2, 1, 1);
  try
    G.AddValue('points-east');
    G.AddValue('also-east');
    G.RequireConnectivity(MakeGraphConnectivityConstraint('one-sided',
      Pos(0, 0, 0), [Pos(1, 0, 0)],
      [MakeGraphConnectivityValue('points-east', [gdEast]),
       MakeGraphConnectivityValue('also-east', [gdEast])]));
    G.SetAllowedValues(0, 0, 0, 'points-east');
    G.SetAllowedValues(1, 0, 0, 'also-east');
    Check(not G.TrySolve(DefaultGraphSolveOptions, Report)
      and (Report.Contradiction.Kind = gckConnectivity),
      'one-sided ports never form a connectivity edge');
  finally
    G.Free;
  end;
end;

function NewEitherOrImpossible(const ASeed: TGraphSeed): TGraph;
begin
  Result := NewGraph(3, 1, 1);
  Result.AddValue('west-terminal');
  Result.AddValue('root-to-west');
  Result.AddValue('root-to-east');
  Result.AddValue('east-terminal');
  Result.Seed := ASeed;
  Result.RequireConnectivity(MakeGraphConnectivityConstraint('fork',
    Pos(1, 0, 0), [Pos(0, 0, 0), Pos(2, 0, 0)],
    [MakeGraphConnectivityValue('west-terminal', [gdEast]),
     MakeGraphConnectivityValue('root-to-west', [gdWest]),
     MakeGraphConnectivityValue('root-to-east', [gdEast]),
     MakeGraphConnectivityValue('east-terminal', [gdWest])]));
  Result.SetAllowedValues(0, 0, 0, 'west-terminal');
  Result.SetAllowedValues(1, 0, 0, ['root-to-west', 'root-to-east']);
  Result.SetAllowedValues(2, 0, 0, 'east-terminal');
end;

procedure TestImpossibleRollbackAndRandomState;
var
  G, Twin: TGraph;
  Before: String;
  Report: TGraphSolveReport;
  RandomA, RandomB: Integer;
begin
  G := NewEitherOrImpossible(246813579);
  Twin := NewEitherOrImpossible(246813579);
  try
    Before := GraphState(G);
    Check(not G.TrySolve(DefaultGraphSolveOptions, Report)
      and (Report.Status = gssContradiction)
      and (Report.Contradiction.Kind = gckConnectivity)
      and (Report.Contradiction.ConstraintIndex = 0),
      'an either/or root cannot connect both required terminals');
    Check((Report.Passes[0].Decisions > 0)
      and (Report.Passes[0].Backtracks > 0),
      'the impossible fixture consumes a decision and explores an alternative');
    Check(GraphState(G) = Before,
      'exhausted connectivity search rolls back values, flags, domains, and selection');
    RandomA := G.RandomIndex(1000000);
    RandomB := Twin.RandomIndex(1000000);
    Check(RandomA = RandomB,
      'failed search restores the pass random stream after consuming decisions');
    Check(G.RandomIndex(1000000) = Twin.RandomIndex(1000000),
      'random-stream rollback holds beyond the first sampled value');
    G.RemoveConnectivity('fork');
    Check(G.TrySolve(DefaultGraphSolveOptions, Report),
      'the same public domains solve after the impossible constraint is removed');
  finally
    Twin.Free;
    G.Free;
  end;
end;

begin
  WriteLn('WFC public connectivity conformance');
  RunTest('factories, deep copies, canonicalization, and pass locality',
    @TestFactoriesCopiesAndPasses);
  RunTest('descriptor validation and atomic registration',
    @TestValidationAndAtomicRegistration);
  RunTest('reshape atomicity and Reset cleanup', @TestReshapeAndReset);
  RunTest('legacy Run preflight', @TestLegacyRunPreflight);
  RunTest('simple routes and independent public BFS',
    @TestSimpleRoutesAndPublicBfs);
  RunTest('optional, required-by-value, and all-participant semantics',
    @TestOptionalRequiredAndAllParticipants);
  RunTest('multiple constraints and reciprocal ports',
    @TestMultipleConstraintsAndReciprocity);
  RunTest('impossible rollback and random-state replay',
    @TestImpossibleRollbackAndRandomState);
  WriteLn(Format('%d checks, %d failures', [GChecks, GFailures]));
  if GFailures <> 0 then
    Halt(1);
end.
