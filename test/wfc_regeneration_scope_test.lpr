{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_regeneration_scope_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF} SysUtils,wfc;
var Checks: Integer;

procedure Check(const OK: Boolean; const Why: String);
begin Inc(Checks); if not OK then raise Exception.Create(Why); end;

function Labels(const Names: array of String): TGraphPassLabels;
var I: Integer;
begin Result:=nil; SetLength(Result,Length(Names)); for I:=0 to High(Names) do Result[I]:=Names[I]; end;

function Indices(const Actual: TGraphPassIndices; const Expected: array of Integer): Boolean;
var I: Integer;
begin
  Result:=False; if Length(Actual)<>Length(Expected) then Exit;
  for I:=0 to High(Actual) do if Actual[I]<>Expected[I] then Exit;
  Result:=True;
end;

function NewGraph: TGraph;
const Names: array[0..5] of String=('terrain','leaf','branch','provider','alias','other');
var I: Integer;
begin
  Result:=TGraph.Create;
  try
    Result.Seed:=71; Result.Reshape(1,1,1); Result.WrapNeighbors:=False;
    for I:=0 to High(Names) do
    begin
      if I=0 then Result.CurrentPass:=Names[I] else Result.SwitchToPass(Names[I]);
      Result.PassMode:=gpmOverlay; Result.ClearDependencies;
      if I<>4 then begin Result.AddValue('A'); Result.AddValue('B'); end;
    end;
    Result.PassGraph[1].DependsOn('provider');
    Result.PassGraph[3].DependsOn('terrain');
    Result.PassGraph[4].TransformFrom('leaf');
  except Result.Free; raise; end;
end;

function State(const G: TGraph): String;
var I,J: Integer; E: TGraphEntry; V: TGraphValues;
begin
  Result:=G.CurrentPass+'|'+IntToStr(G.Seed);
  for I:=0 to G.TotalPassCount-1 do
  begin
    E:=G.PassGraph[I].Entry[0,0,0];
    Result:=Result+'|'+String(E.Value)+':'+IntToStr(Ord(E.Empty))+':'+IntToStr(Ord(E.Generated));
    Result:=Result+':'+IntToStr(Ord(G.PassGraph[I].HasAllowedValues(0,0,0)));
    V:=G.PassGraph[I].CopyAllowedValues(0,0,0);
    for J:=0 to High(V) do Result:=Result+':'+String(V[J]);
  end;
end;

procedure TestScope;
var G,Twin: TGraph; Roots,Active,R2,A2: TGraphPassIndices;
  S: String; I,J: Integer; Raised: Boolean;
  O: TGraphSolveOptions; Report: TGraphSolveReport;
  N: TGraphNegotiationOptions; NR: TGraphSelectiveNegotiationReport;
begin
  G:=NewGraph; Twin:=NewGraph;
  try
    O:=DefaultGraphSolveOptions;
    Check(G.TrySolve(O,Report),'initial graph solves');
    Check(Twin.TrySolve(O,Report),'comparison graph solves');
    G.PassGraph[2].Entry[0,0,0].Value:='A';
    Twin.PassGraph[2].Entry[0,0,0].Value:='A';
    G.PassGraph[5].SetAllowedValues(0,0,0,['A','B']);
    Twin.PassGraph[5].SetAllowedValues(0,0,0,['A','B']);
    S:=State(G);
    G.ResolveRegenerationScope(Labels(['provider']),Roots,Active);
    Check(Indices(Roots,[3]),'explicit provider is the sole root');
    Check(Indices(Active,[3,1,4]),'scope follows forward DAG and alias, not numeric suffix');
    Check(State(G)=S,'scope preserves values, generated flags, lock, domains, selection, seed');
    G.ResolveRegenerationScope(Labels(['provider','leaf','provider']),Roots,Active);
    Check(Indices(Roots,[1,3]),'duplicate roots canonicalized by index');
    Check(Indices(Active,[3,1,4]),'active passes retain actual topological order');
    G.PassGraph[1].ResolveRegenerationScope(Labels(['leaf']),R2,A2);
    Check(Indices(R2,[1]) and Indices(A2,[1,4]),'child graph delegates without adding ancestor');
    Check(State(G)=S,'child graph scope preserves root selection');
    Roots[0]:=99; Active[0]:=99;
    G.ResolveRegenerationScope(Labels(['other']),Roots,Active);
    Check(Indices(Roots,[5]) and Indices(Active,[5]),'detached returned arrays and reused variables do not leak prior scope');
    Raised:=False;
    try G.ResolveRegenerationScope(Labels(['provider','missing']),Roots,Active);
    except on E:EArgumentException do Raised:=True; end;
    Check(Raised and (State(G)=S),'unknown later root rejects without entry or selection mutation');
    Raised:=False;
    try G.ResolveRegenerationScope(nil,Roots,Active);
    except on E:EArgumentException do Raised:=True; end;
    Check(Raised and (State(G)=S),'empty roots reject without mutation');
    for I:=0 to G.TotalPassCount-1 do for J:=1 to 8 do
      Check(G.PassGraph[I].RandomIndex(100000)=Twin.PassGraph[I].RandomIndex(100000),'scope inspection consumes no random values');
    G.ResolveRegenerationScope(Labels(['provider','leaf','provider']),Roots,Active);
    N:=DefaultGraphNegotiationOptions;
    Check(G.TryRegenerateNegotiatedFrom(Labels(['leaf','provider']),N,NR),'negotiated selected repair solves');
    Check(Indices(NR.RequestedRootIndices,[1,3]) and Indices(NR.ActivePassIndices,[3,1,4]),'scope matches actual negotiated wrapper');
    Check(Indices(NR.Search.FinalReport.ExecutionOrder,[3,1,4]),'actual selected execution follows preview');
    Check((NR.Search.FinalReport.Passes[0].Disposition=gpdReused) and
      (NR.Search.FinalReport.Passes[2].Disposition=gpdReused) and
      (NR.Search.FinalReport.Passes[5].Disposition=gpdReused),'off-scope passes truly reused');
    Check(G.TryRegenerateFrom(Labels(['provider']),O,Report),'ordinary selected repair solves');
    Check(Indices(Report.ExecutionOrder,[3,1,4]),'ordinary repair shares exact preview closure');
  finally Twin.Free; G.Free; end;
end;

procedure TestAliasAndDerivedRoleBoundaries;
var G: TGraph; Roots,Active: TGraphPassIndices; Replacement: TGraphRuleGroup;
  S: String; Raised: Boolean;
begin
  G:=NewGraph;
  try
    S:=State(G);
    G.ResolveRegenerationScope(Labels(['alias']),Roots,Active);
    Check(Indices(Roots,[4]) and Indices(Active,[4]),
      'alias-only permission never implicitly authorizes its source');
    G.ResolveRegenerationScope(Labels(['terrain']),Roots,Active);
    Check(Indices(Roots,[0]) and Indices(Active,[0,3,1,4]),
      'root permission includes only its descendants, not unrelated passes');
    G.ResolveRegenerationScope(Labels(['other','provider']),Roots,Active);
    Check(Indices(Roots,[3,5]) and Indices(Active,[3,1,4,5]),
      'disconnected roots retain canonical indices and true execution order');
    Raised:=False;
    try G.PassGraph[1].ResolveRegenerationScope(Labels(['leaf','missing']),Roots,Active);
    except on E:EArgumentException do Raised:=True; end;
    Check(Raised and (State(G)=S),'child invalid roots preserve active root selection and entries');

    { A detached base group can gain PreviousValues before it enters the public
      owner dictionary. Scope must inspect the actual current rule registry. }
    Replacement:=TGraphRuleGroup.Create('A');
    try
      Replacement.RequirePrevious('A');
      G.PassGraph[2].RuleGroups.Remove('A');
      G.PassGraph[2].RuleGroups.Add('A',Replacement); Replacement:=nil;
    finally Replacement.Free; end;
    G.ResolveRegenerationScope(Labels(['provider']),Roots,Active);
    Check(Indices(Active,[3,1,2,4]),'late PreviousValues role joins the exact descendant closure');
    Check(State(G)=S,'derived-role synchronization does not mutate entries, domains or selection');

    G.PassGraph[2].RuleGroups.Remove('A');
    G.PassGraph[2].RuleGroups.Add('A',TGraphRuleGroup.Create('A'));
    G.ResolveRegenerationScope(Labels(['provider']),Roots,Active);
    Check(Indices(Active,[3,1,4]),'replacing a legacy requirement removes stale derived reachability');

    G.PassGraph[2].DependsOn('leaf');
    Replacement:=TGraphRuleGroup.Create('A');
    try
      Replacement.RequirePrevious('A');
      G.PassGraph[2].RuleGroups.Remove('A');
      G.PassGraph[2].RuleGroups.Add('A',Replacement); Replacement:=nil;
    finally Replacement.Free; end;
    G.ResolveRegenerationScope(Labels(['provider']),Roots,Active);
    Check(Indices(Active,[3,1,2,4]),'declared and derived roles share one canonical edge');
    G.PassGraph[2].RuleGroups.Remove('A');
    G.PassGraph[2].RuleGroups.Add('A',TGraphRuleGroup.Create('A'));
    G.ResolveRegenerationScope(Labels(['provider']),Roots,Active);
    Check(Indices(Active,[3,1,2,4]),'removing a derived role cannot remove its still-declared edge');
    Check(State(G)=S,'all scope-only operations leave graph cell state intact');
  finally G.Free; end;

  G:=TGraph.Create;
  try
    G.Seed:=71;
    G.ResolveRegenerationScope(Labels(['']),Roots,Active);
    Check(Indices(Roots,[0]) and Indices(Active,[0]),
      'the core unnamed initial pass is a valid explicit root');
    Check(G.CurrentPass='','resolving the unnamed root retains current-pass selection');
  finally G.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestHostile;
var G:TGraph; Bad:TGraphPassLabels; Roots,Active:TGraphPassIndices;
  I,Hits:Integer; Raised:Boolean; S:String;
begin
  G:=NewGraph;
  try
    S:=State(G);
    for I:=0 to 9 do
    begin
      Hits:=0;
      asm
        switch(I) {
          case 0: Bad=undefined;break;
          case 1: Bad={0:'provider',length:1};break;
          case 2: Bad=['provider',null];break;
          case 3: Bad=['provider',42];break;
          case 4: Bad=new Array(2);Bad[0]='provider';break;
          case 5: Bad=['provider'];Object.defineProperty(Bad,'0',{get:function(){Hits++;return 'provider';}});break;
          case 6: Bad=['provider',{}];break;
          case 7: Bad=['provider',NaN];break;
          case 8: Bad=new Array(1);Object.setPrototypeOf(Bad,{0:'provider'});break;
          case 9: Bad=['provider',new String('leaf')];break;
        }
      end;
      Raised:=False;
      try G.ResolveRegenerationScope(Bad,Roots,Active);
      except on E:EArgumentException do Raised:=True; end;
      Check(Raised,'hostile root container rejects with typed error');
      Check((Hits=0) and (State(G)=S),'hostile roots cannot invoke getters or alter graph entries');
    end;
  finally G.Free; end;
end;
{$ENDIF}

begin
  TestScope;
  TestAliasAndDerivedRoleBoundaries;
  {$IFDEF PAS2JS}TestHostile;{$ENDIF}
  WriteLn('Regeneration scope: ',Checks,' checks passed.');
end.
