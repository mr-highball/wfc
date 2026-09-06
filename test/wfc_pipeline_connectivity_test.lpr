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
program wfc_pipeline_connectivity_test;
{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  Classes, SysUtils, wfc, wfc_model, wfc_model_text, wfc_rule_model, wfc_rule_text,
  wfc_pattern2d, wfc_pattern2d_learn, wfc_pattern2d_text,
  wfc_sequence, wfc_sequence_learn, wfc_sequence_text,
  wfc_pipeline_model, wfc_pipeline_text, wfc_pipeline_compile, wfc_pipeline_connectivity,
  wfc_pipeline_run, wfc_pipeline_run_text, wfc_pipeline_runtime,
  wfc_pipeline_result, wfc_pipeline_result_text, wfc_run_app,
  wfc_validate_app, connected_routes_portable;

type
  TFixtureKind = (fkRules, fkModel, fkSequence, fkPattern);
  TGraphValidationAccess = class(TGraph)
    class function Validate(const G: TGraph; out P, E: Integer): Boolean;
  end;

var Checks: Integer;

procedure Check(const Condition: Boolean; const MessageText: String);
begin
  Inc(Checks);
  if not Condition then raise Exception.Create(MessageText);
end;

class function TGraphValidationAccess.Validate(const G: TGraph;
  out P, E: Integer): Boolean;
begin
  Result := TGraphValidationAccess(G).DoValidateCommit(P, E);
end;

function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(Values));
  for I := 0 to High(Values) do Result[I] := Values[I];
end;

function Integers(const Values: array of Integer): TWfcModelIntegerArray;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(Values));
  for I := 0 to High(Values) do Result[I] := Values[I];
end;

function Position(const X, Y, Z: Integer): TGraphPosition;
begin
  Result.X := X; Result.Y := Y; Result.Z := Z;
end;

function Positions(const Values: array of TGraphPosition): TGraphPositions;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(Values));
  for I := 0 to High(Values) do Result[I] := Values[I];
end;

function Connection(const Pass: Integer; const Required: TGraphPositions;
  const All: Boolean; const Openings: TGraphDirections;
  const WithRequiredValue: Boolean = True): TWfcPipelineConnectivity;
var Profiles: TWfcPipelineConnectivityValues;
begin
  SetLength(Profiles, 1 + Ord(WithRequiredValue));
  Profiles[0] := MakeWfcPipelineConnectivityValue('A', Openings);
  if WithRequiredValue then
    Profiles[1] := MakeWfcPipelineConnectivityValue('C', Openings, True);
  Result := MakeWfcPipelineConnectivity(Pass, 'same-label', Position(0, 0, 0),
    Required, Profiles, All);
end;

function OneConnection(const C: TWfcPipelineConnectivity): TWfcPipelineConnectivities;
begin
  Result := nil; SetLength(Result, 1); Result[0] := C;
end;

function Fixture(const Kind: TFixtureKind; const Rank: Integer;
  const Wrap: Boolean; const Connections: TWfcPipelineConnectivities;
  const Quotas: TWfcPipelineValueQuotas = nil;
  const BridgeVersion: Integer = 2): TWfcPipelineModel;
var Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies; Bridges: TWfcPipelineBridges;
  Versions: TWfcPipelineVersions; Model: TWfcModel; Rules: TWfcRuleModel;
  SequenceModel: TWfcSequenceModel; Pattern: TWfcOverlappingModel2D;
  Samples: TWfcSequenceSamples; Relations: TWfcModelIntegerArray;
  DocumentText: String; ResourceKind: TWfcPipelineResourceKind;
  Adapter: TWfcPipelineAdapterKind; I: Integer; Projected: Boolean;
begin
  Projected := Kind in [fkSequence, fkPattern];
  ResourceKind := wprkRules; Adapter := wpakRules;
  case Kind of
    fkRules:
      begin
        Rules := TWfcRuleModel.Create(Rank, Tokens(['A', 'B', 'C']),
          Integers([1, 1, 1]), nil);
        try DocumentText := EncodeWfcRuleText(Rules); finally Rules.Free; end;
      end;
    fkModel:
      begin
        SetLength(Relations, 36);
        for I := 0 to High(Relations) do Relations[I] := 1;
        Model := TWfcModel.Create(2, 2, 2, wmbWrap, wmsNone,
          [wmdNorth, wmdEast, wmdSouth, wmdWest], Tokens(['A', 'B', 'C']),
          Integers([1, 1, 1]), Relations);
        try DocumentText := EncodeWfcModelText(Model); finally Model.Free; end;
        ResourceKind := wprkModel; Adapter := wpakModel;
      end;
    fkSequence:
      begin
        SetLength(Samples, 2);
        Samples[0] := MakeWfcSequenceSample(Tokens(['A', 'A', 'A']));
        Samples[1] := MakeWfcSequenceSample(Tokens(['C', 'B', 'C']));
        SequenceModel := LearnSequenceModelCorpus(Samples, 2);
        try DocumentText := EncodeWfcSequenceText(SequenceModel);
        finally SequenceModel.Free; end;
        ResourceKind := wprkSequence; Adapter := wpakSequence;
      end;
    fkPattern:
      begin
        Pattern := LearnOverlappingModel2D(Tokens(['A', 'A', 'B', 'A']),
          2, 2, 2, 2, wmbWrap, wmsNone);
        try DocumentText := EncodeWfcPattern2DText(Pattern);
        finally Pattern.Free; end;
        ResourceKind := wprkPattern2D; Adapter := wpakPattern2D;
      end;
  end;
  SetLength(Resources, 1);
  Resources[0] := MakeWfcPipelineResource('source-model', ResourceKind,
    DocumentText, 'Project-authored connectivity integration fixture', 'MIT',
    'pipeline-connectivity:1');
  SetLength(Passes, 3); SetLength(Dependencies, 2);
  if Projected then
  begin
    Passes[0] := MakeWfcPipelinePass('private-source', wppvPrivate,
      gpmOverlay, -1, Adapter, 0, Kind = fkSequence, wseWhole);
    Passes[1] := MakeWfcPipelinePass('public-projection', wppvPublic,
      gpmOverlay, -1, wpakEmpty, -1, False, wseWhole);
    SetLength(Bridges, 1);
    if Kind = fkSequence then
      Bridges[0] := MakeWfcPipelineBridge(wpbkSequenceProjection, 0, 1)
    else Bridges[0] := MakeWfcPipelineBridge(wpbkPattern2DProjection, 0, 1);
  end
  else
  begin
    Passes[0] := MakeWfcPipelinePass('source', wppvPublic,
      gpmOverlay, -1, Adapter, 0, False, wseWhole);
    Passes[1] := MakeWfcPipelinePass('copy-one', wppvPublic,
      gpmTransform, 0, wpakEmpty, -1, False, wseWhole);
  end;
  Passes[2] := MakeWfcPipelinePass('copy-two', wppvPublic,
    gpmTransform, 1, wpakEmpty, -1, False, wseWhole);
  Dependencies[0] := MakeWfcPipelineDependency(1, 0);
  Dependencies[1] := MakeWfcPipelineDependency(2, 1);
  Versions := CurrentWfcPipelineVersions;
  Versions.SequenceBridgeVersion := BridgeVersion;
  Versions.Pattern2DBridgeVersion := BridgeVersion;
  Result := TWfcPipelineModel.Create(MakeWfcPipelineMetadata(
    'Portable rooted connectivity', 'MIT', 'Independent integration fixture',
    'pipeline-connectivity:1'), Versions, Rank, Wrap, rmBottomUp,
    Resources, Passes, Dependencies, Bridges, nil, Quotas, Connections);
end;

function FixedDomains(const Pass, Width, Height: Integer;
  const Values: array of TWfcModelToken): TWfcPipelineCellDomains;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(Values));
  for I := 0 to High(Values) do
    Result[I] := MakeWfcPipelineCellDomain(Pass, I mod Width,
      (I div Width) mod Height, I div (Width * Height), Tokens([Values[I]]));
end;

{ Independent finite BFS. This fixture oracle reads public token/profile data,
  never compiler graphs, private states or the production validation helper.
  The authored local rules in these fixtures permit every pair. }
function Connected(const C: TWfcPipelineConnectivity; const Values: TWfcModelTokens;
  const Width, Height, Depth: Integer; const Wrap: Boolean): Boolean;
const DX: array[0..5] of Integer = (0, 1, 0, -1, 0, 0);
  DY: array[0..5] of Integer = (1, 0, -1, 0, 0, 0);
  DZ: array[0..5] of Integer = (0, 0, 0, 0, 1, -1);
  Opposite: array[0..5] of Integer = (2, 3, 0, 1, 5, 4);
var Seen: array of Boolean; Profiles, Queue: array of Integer;
  I, J, Head, Tail, At, Target, X, Y, Z, D, Root: Integer;
begin
  Result := False;
  if Length(Values) <> Width * Height * Depth then Exit;
  SetLength(Seen, Length(Values)); SetLength(Profiles, Length(Values));
  SetLength(Queue, Length(Values));
  for I := 0 to High(Values) do
  begin
    Profiles[I] := -1;
    for J := 0 to High(C.Values) do
      if Values[I] = C.Values[J].Value then Profiles[I] := J;
  end;
  Root := Integer(C.Root.X) + Width * Integer(C.Root.Y) +
    Width * Height * Integer(C.Root.Z);
  if (Root < 0) or (Root >= Length(Values)) or (Profiles[Root] < 0) then Exit;
  Head := 0; Tail := 1; Queue[0] := Root; Seen[Root] := True;
  while Head < Tail do
  begin
    At := Queue[Head]; Inc(Head);
    for D := 0 to 5 do
    begin
      if not (TGraphDirection(D) in C.Values[Profiles[At]].Openings) then Continue;
      X := At mod Width + DX[D]; Y := (At div Width) mod Height + DY[D];
      Z := At div (Width * Height) + DZ[D];
      if Wrap then
      begin
        X := (X + Width) mod Width; Y := (Y + Height) mod Height;
        Z := (Z + Depth) mod Depth;
      end
      else if (X < 0) or (X >= Width) or (Y < 0) or (Y >= Height) or
          (Z < 0) or (Z >= Depth) then Continue;
      Target := X + Width * Y + Width * Height * Z;
      if Seen[Target] or (Profiles[Target] < 0) then Continue;
      if not (TGraphDirection(Opposite[D]) in C.Values[Profiles[Target]].Openings) then Continue;
      Seen[Target] := True; Queue[Tail] := Target; Inc(Tail);
    end;
  end;
  for I := 0 to High(C.RequiredPositions) do
  begin
    Target := Integer(C.RequiredPositions[I].X) + Width * Integer(C.RequiredPositions[I].Y) +
      Width * Height * Integer(C.RequiredPositions[I].Z);
    if (Target < 0) or (Target >= Length(Seen)) or not Seen[Target] then Exit;
  end;
  for I := 0 to High(Values) do
    if (Profiles[I] >= 0) and
      (C.RequireAllParticipants or C.Values[Profiles[I]].RequiredByValue) and
      not Seen[I] then Exit;
  Result := True;
end;

procedure CheckOutput(const Model: TWfcPipelineModel; const Output: TWfcPipelineResult);
var C, L, Q, I, V, N: Integer; Layer: TWfcPipelineResultLayer; Found: Boolean;
  Quota: TWfcPipelineValueQuota;
begin
  Check(Output.Status = wprsSolved, 'independent BFS requires solved output');
  for C := 0 to Model.ConnectivityCount - 1 do
  begin
    Found := False;
    for L := 0 to Output.LayerCount - 1 do
    begin
      Layer := Output.LayerAt(L);
      if Layer.PassIndex <> Model.ConnectivityAt(C).PassIndex then Continue;
      Found := True;
      Check(Connected(Model.ConnectivityAt(C), Layer.Tokens,
        Output.Width, Output.Height, Output.Depth, Model.WrapNeighbors),
        'independent public reciprocal-port BFS satisfies recipe constraint');
    end;
    Check(Found, 'connectivity owner exists among complete public result layers');
  end;
  for Q := 0 to Model.ValueQuotaCount - 1 do
  begin
    Quota := Model.ValueQuotaAt(Q);
    for L := 0 to Output.LayerCount - 1 do
    begin
      Layer := Output.LayerAt(L); if Layer.PassIndex <> Quota.PassIndex then Continue;
      N := 0;
      for I := 0 to High(Layer.Tokens) do for V := 0 to High(Quota.Values) do
        if Layer.Tokens[I] = Quota.Values[V] then Inc(N);
      Check((N >= Quota.MinimumCount) and (N <= Quota.MaximumCount),
        'independent quota recount conjoins with public connectivity');
    end;
  end;
end;

procedure RunAndReplay(const Model: TWfcPipelineModel; const Width, Height, Depth: Integer;
  const ExpectedSolved: Boolean; const Domains: TWfcPipelineCellDomains = nil;
  const Strategy: TWfcPipelineSolveStrategy = wpssOneWay);
var Run, CopyRun: TWfcPipelineRun; Runtime: TWfcPipelineRuntime;
  Output, Replay: TWfcPipelineResult; CopyModel: TWfcPipelineModel; TextValue: String;
begin
  Run := TWfcPipelineRun.Create(Model, Width, Height, Depth, 17, Strategy,
    128, 8 * Ord(Strategy = wpssNegotiated), True, nil, Domains);
  try
    Runtime := TWfcPipelineRuntime.Create(Model, Run);
    try
      Output := Runtime.Execute;
      try
        Check((Output.Status = wprsSolved) = ExpectedSolved,
          'portable connectivity feasibility matches the independent fixture');
        if ExpectedSolved then CheckOutput(Model, Output)
        else Check(Output.LayerCount = 0, 'disconnected run publishes no partial layer');
        TextValue := EncodeWfcPipelineResultText(Output);
        Replay := Runtime.Execute;
        try Check(EncodeWfcPipelineResultText(Replay) = TextValue,
          'prepared connectivity runtime preserves exact repeated execution');
        finally Replay.Free; end;
        Replay := DecodeWfcPipelineResultText(TextValue, Model, Run);
        try Check(EncodeWfcPipelineResultText(Replay) = TextValue,
          'solved or failed connectivity result has an exact canonical roundtrip');
        finally Replay.Free; end;
        CopyModel := DecodeWfcPipelineModelText(EncodeWfcPipelineModelText(Model));
        try
          Check(EncodeWfcPipelineModelText(CopyModel) = EncodeWfcPipelineModelText(Model),
            'root, terminals, ports, flags and aliases survive canonical recipe replay');
          CopyRun := DecodeWfcPipelineRunText(EncodeWfcPipelineRunText(Run), CopyModel);
          try
            Replay := ExecuteWfcPipeline(CopyModel, CopyRun);
            try Check(EncodeWfcPipelineResultText(Replay) = TextValue,
              'independent decoded recipe and invocation replay exact result and evidence');
            finally Replay.Free; end;
          finally CopyRun.Free; end;
        finally CopyModel.Free; end;
      finally Output.Free; end;
    finally Runtime.Free; end;
  finally Run.Free; end;
end;

procedure TestDirectAliasesAndPolicies;
var M: TWfcPipelineModel; C: TWfcPipelineConnectivities; Compiled: TWfcCompiledPipeline;
  Kind: TFixtureKind; Policy: Integer; D: TWfcPipelineCellDomains;
  Q: TWfcPipelineValueQuotas;
begin
  SetLength(C, 2);
  C[0] := Connection(0, nil, False, AllDirections);
  C[1] := Connection(2, Positions([Position(2, 0, 0)]), True, AllDirections);
  for Kind := fkRules to fkModel do
  begin
    M := Fixture(Kind, 2, False, C);
    try
      Compiled := CompileWfcPipeline(M, 3, 2, 1);
      try
        Check(Length(Compiled.Graph.PassGraph[0].CopyConnectivityConstraints) = 2,
          'same-label direct and alias descriptors lower without registry collision');
        Check((not Compiled.Graph.PassGraph[1].HasDefinition) and
          (Length(Compiled.Graph.PassGraph[2].CopyRegisteredValues) = 0),
          'connectivity does not materialize definitionless exact-copy aliases');
      finally Compiled.Free; end;
      RunAndReplay(M, 3, 2, 1, True);
      RunAndReplay(M, 3, 2, 1, True, nil, wpssNegotiated);
    finally M.Free; end;
  end;
  for Policy := 0 to 4 do
  begin
    C := OneConnection(Connection(2, nil, Policy = 1, [gdEast, gdWest]));
    if Policy = 3 then C[0].RequiredPositions := Positions([Position(2, 0, 0)]);
    M := Fixture(fkRules, 1, False, C);
    try
      if Policy = 2 then D := FixedDomains(0, 3, 1, ['A', 'B', 'C'])
      else if Policy = 4 then D := FixedDomains(0, 3, 1, ['B', 'B', 'A'])
      else D := FixedDomains(0, 3, 1, ['A', 'B', 'A']);
      RunAndReplay(M, 3, 1, 1, Policy = 0, D);
    finally M.Free; end;
  end;
  C := OneConnection(Connection(2, Positions([Position(2, 0, 0)]), True, AllDirections));
  SetLength(Q, 1); Q[0] := MakeWfcPipelineValueQuota(2, 'route-count', Tokens(['A']), 3, 3);
  M := Fixture(fkRules, 2, False, C, Q);
  try RunAndReplay(M, 3, 2, 1, True); finally M.Free; end;
  Q[0].MinimumCount := 0; Q[0].MaximumCount := 0;
  C[0].Values := nil; SetLength(C[0].Values, 1);
  C[0].Values[0] := MakeWfcPipelineConnectivityValue('A', AllDirections);
  M := Fixture(fkRules, 2, False, C, Q);
  try RunAndReplay(M, 3, 2, 1, False); finally M.Free; end;
end;

procedure TestWrappingAndThreeDimensions;
var M: TWfcPipelineModel; C: TWfcPipelineConnectivities;
begin
  C := OneConnection(Connection(2, nil, True, [gdEast], False));
  M := Fixture(fkRules, 1, True, C);
  try RunAndReplay(M, 1, 1, 1, True, FixedDomains(0, 1, 1, ['A'])); finally M.Free; end;
  C[0].RequiredPositions := Positions([Position(1, 0, 0)]);
  M := Fixture(fkRules, 1, True, C);
  try RunAndReplay(M, 2, 1, 1, False, FixedDomains(0, 2, 1, ['A', 'A'])); finally M.Free; end;
  C[0].Values[0].Openings := [gdEast, gdWest];
  M := Fixture(fkRules, 1, True, C);
  try RunAndReplay(M, 2, 1, 1, True, FixedDomains(0, 2, 1, ['A', 'A'])); finally M.Free; end;
  C := OneConnection(Connection(2, Positions([Position(0, 0, 1)]),
    True, [gdUp, gdDown], False));
  M := Fixture(fkRules, 3, False, C);
  try RunAndReplay(M, 1, 1, 2, True, FixedDomains(0, 1, 1, ['A', 'A'])); finally M.Free; end;
  C[0].Values[0].Openings := [gdUp];
  M := Fixture(fkRules, 3, False, C);
  try RunAndReplay(M, 1, 1, 2, False, FixedDomains(0, 1, 1, ['A', 'A'])); finally M.Free; end;
end;

procedure TestEveryOrientedPort;
const Opposite: array[0..5] of TGraphDirection =
  (gdSouth, gdWest, gdNorth, gdEast, gdDown, gdUp);
var D: TGraphDirection; Width, Height, Depth: Integer;
  M: TWfcPipelineModel; C: TWfcPipelineConnectivities;
  Domains: TWfcPipelineCellDomains; Positive: Boolean;
begin
  for D := Low(TGraphDirection) to High(TGraphDirection) do
  begin
    Width := 1; Height := 1; Depth := 1;
    if D in [gdEast, gdWest] then Width := 2
    else if D in [gdNorth, gdSouth] then Height := 2
    else Depth := 2;
    Positive := D in [gdNorth, gdEast, gdUp];
    C := OneConnection(Connection(2, nil, False, [D]));
    C[0].Values[1].Openings := [Opposite[Ord(D)]];
    if not Positive then C[0].Root := Position(Width - 1, Height - 1, Depth - 1);
    if Positive then Domains := FixedDomains(0, Width, Height, ['A', 'C'])
    else Domains := FixedDomains(0, Width, Height, ['C', 'A']);
    M := Fixture(fkRules, 3, False, C);
    try RunAndReplay(M, Width, Height, Depth, True, Domains); finally M.Free; end;
    C[0].Values[1].Openings := [D];
    M := Fixture(fkRules, 3, False, C);
    try RunAndReplay(M, Width, Height, Depth, False, Domains); finally M.Free; end;
  end;
end;

procedure CheckImmutableAdjacencyOrientation(const Direction: TGraphDirection;
  const Learned, Reversed: Boolean);
var Width, Height, Depth, I, J, Failed: Integer; Root, Target: TGraphPosition;
  ModelDirection, Opposite: TWfcModelDirection; Rows: TWfcRuleRows; Swap: TWfcRuleRow;
  Relations, Weights: TWfcModelIntegerArray; Values, Assignment: TWfcModelTokens;
  Rules: TWfcRuleModel; Model: TWfcModel; Resources: TWfcPipelineResources;
  Passes: TWfcPipelinePasses; Connections: TWfcPipelineConnectivities;
  Profiles: TWfcPipelineConnectivityValues; Required: TGraphPositions;
  Recipe: TWfcPipelineModel; Run: TWfcPipelineRun; Output: TWfcPipelineResult;
  Locks: TWfcPipelineCellLocks; DocumentText, LabelText: String;
  Key: TGraphDirection; Kind: TWfcPipelineResourceKind; Adapter: TWfcPipelineAdapterKind;
begin
  LabelText := IntToStr(Ord(Direction)) + '/' + BoolToStr(Learned, True) +
    '/' + BoolToStr(Reversed, True);
  Width := 1; Height := 1; Depth := 1;
  Root := Position(0, 0, 0); Target := Root;
  { Model sample directions and owner-relative public rule keys are not
    interchangeable with geometric neighbor directions. Pin every mapping
    independently, including North=+Y and both signed vertical directions. }
  case Direction of
    gdNorth: begin Height := 2; Target.Y := 1; ModelDirection := wmdSouth; Opposite := wmdNorth; end;
    gdEast: begin Width := 2; Target.X := 1; ModelDirection := wmdEast; Opposite := wmdWest; end;
    gdSouth: begin Height := 2; Root.Y := 1; ModelDirection := wmdNorth; Opposite := wmdSouth; end;
    gdWest: begin Width := 2; Root.X := 1; ModelDirection := wmdWest; Opposite := wmdEast; end;
    gdUp: begin Depth := 2; Target.Z := 1; ModelDirection := wmdUp; Opposite := wmdDown; end;
    gdDown: begin Depth := 2; Root.Z := 1; ModelDirection := wmdDown; Opposite := wmdUp; end;
  end;
  Values := Tokens(['A', 'B']); Weights := Integers([1, 1]);
  if Learned then
  begin
    SetLength(Relations, 24);
    for I := 0 to High(Relations) do Relations[I] := 1;
    for I := 0 to 3 do
    begin
      Relations[Ord(ModelDirection) * 4 + I] := 0;
      Relations[Ord(Opposite) * 4 + I] := 0;
    end;
    if Reversed then
    begin
      Relations[(Ord(Opposite) * 2) * 2 + 1] := 1;
      Relations[(Ord(ModelDirection) * 2 + 1) * 2] := 1;
    end
    else
    begin
      Relations[(Ord(ModelDirection) * 2) * 2 + 1] := 1;
      Relations[(Ord(Opposite) * 2 + 1) * 2] := 1;
    end;
    Model := TWfcModel.Create(3, Width, Height, Depth, wmbOpen, wmsNone,
      [wmdNorth, wmdEast, wmdSouth, wmdWest, wmdUp, wmdDown], Values, Weights, Relations);
    try DocumentText := EncodeWfcModelText(Model); finally Model.Free; end;
    Kind := wprkModel; Adapter := wpakModel;
  end
  else
  begin
    SetLength(Rows, 4); Key := InverseOfDir(Direction);
    if Reversed then Key := Direction;
    Rows[0] := MakeWfcAllowRuleRow(0, Key, False, Integers([1]));
    Rows[1] := MakeWfcAllowRuleRow(1, InverseOfDir(Key), False, Integers([0]));
    { Explicit denials matter: an absent row is the wildcard, so omitting
      these would accidentally make a reversed fixture traversable. }
    Rows[2] := MakeWfcDenyRuleRow(0, InverseOfDir(Key));
    Rows[3] := MakeWfcDenyRuleRow(1, Key);
    for I := 0 to 2 do for J := I + 1 to 3 do
      if Rows[I].OwnerIndex * 6 + Ord(Rows[I].Direction) >
          Rows[J].OwnerIndex * 6 + Ord(Rows[J].Direction) then
      begin Swap := Rows[I]; Rows[I] := Rows[J]; Rows[J] := Swap; end;
    Rules := TWfcRuleModel.Create(3, Values, Weights, Rows);
    try DocumentText := EncodeWfcRuleText(Rules); finally Rules.Free; end;
    Kind := wprkRules; Adapter := wpakRules;
  end;
  SetLength(Resources, 1);
  Resources[0] := MakeWfcPipelineResource('local', Kind, DocumentText,
    'Project-authored orientation fixture', 'MIT', '');
  SetLength(Passes, 1);
  Passes[0] := MakeWfcPipelinePass('public', wppvPublic, gpmOverlay, -1,
    Adapter, 0, False, wseWhole);
  SetLength(Profiles, 2);
  Profiles[0] := MakeWfcPipelineConnectivityValue('A', [Direction], False);
  Profiles[1] := MakeWfcPipelineConnectivityValue('B', [InverseOfDir(Direction)], True);
  Required := Positions([Target]); SetLength(Connections, 1);
  Connections[0] := MakeWfcPipelineConnectivity(0, 'route', Root, Required, Profiles, True);
  Recipe := TWfcPipelineModel.Create(MakeWfcPipelineMetadata('orientation', 'MIT', '', ''),
    3, False, rmBottomUp, Resources, Passes, nil, nil, nil, nil, Connections);
  Run := nil; Output := nil;
  try
    if Direction in [gdNorth, gdEast, gdUp] then Assignment := Tokens(['A', 'B'])
    else Assignment := Tokens(['B', 'A']);
    SetLength(Locks, 2);
    for I := 0 to 1 do
      Locks[I] := MakeWfcPipelineCellLock(0, I mod Width,
        (I div Width) mod Height, I div (Width * Height), Assignment[I]);
    PreflightWfcPipelineConnectivity(Recipe, Width, Height, Depth, Failed);
    Check(ValidateWfcPipelineConnectivity(Recipe, 0, Width, Height, Depth,
      Assignment, Failed) = not Reversed, 'immutable local adjacency orientation ' + LabelText);
    Run := TWfcPipelineRun.Create(Recipe, Width, Height, Depth, 0, wpssOneWay,
      128, 0, False, Locks, nil);
    Output := ExecuteWfcPipeline(Recipe, Run);
    Check((Output.Status = wprsSolved) = not Reversed,
      'actual compiler/core adjacency orientation ' + LabelText);
  finally Output.Free; Run.Free; Recipe.Free; end;
end;

procedure TestImmutableAdjacency;
var Direction: TGraphDirection; Learned, Reversed: Boolean;
begin
  for Direction := Low(TGraphDirection) to High(TGraphDirection) do
    for Learned := False to True do for Reversed := False to True do
      CheckImmutableAdjacencyOrientation(Direction, Learned, Reversed);
end;

procedure TestPrivateLowering;
var Kind: TFixtureKind; Version, I, J, Expected, Matches, P, Width, Height: Integer;
  M: TWfcPipelineModel; C: TWfcPipelineConnectivities; Compiled: TWfcCompiledPipeline;
  Core: TGraphConnectivityConstraints; Values: TGraphValues; Token: TWfcModelToken;
begin
  for Kind := fkSequence to fkPattern do for Version := 1 to 2 do
  begin
    if Kind = fkSequence then
    begin
      Width := 3; Height := 1;
      C := OneConnection(Connection(2, Positions([Position(2, 0, 0)]),
        True, [gdEast, gdWest]));
    end
    else
    begin
      Width := 2; Height := 2;
      C := OneConnection(Connection(2, nil, True,
        [gdNorth, gdEast, gdSouth, gdWest], False));
    end;
    M := Fixture(Kind, 1 + Ord(Kind = fkPattern), Kind = fkPattern, C, nil, Version);
    try
      Compiled := CompileWfcPipeline(M, Width, Height, 1);
      try
        Core := Compiled.Graph.PassGraph[0].CopyConnectivityConstraints;
        Values := Compiled.Graph.PassGraph[0].CopyRegisteredValues;
        Check((Length(Core) = 1) and
          (Length(Compiled.Graph.PassGraph[1].CopyConnectivityConstraints) = 1),
          'projected connectivity constrains both latent source and materialized public owner');
        Matches := 0;
        for I := 0 to High(Values) do
        begin
          if Kind = fkSequence then Token := M.BorrowSequenceResource(0).ProjectStateToken(I)
          else Token := M.BorrowPattern2DResource(0).PaletteTokenAt(
            M.BorrowPattern2DResource(0).PatternPaletteIndexAt(I, 0, 0));
          Expected := -1;
          for P := 0 to High(C[0].Values) do if C[0].Values[P].Value = Token then Expected := P;
          J := 0;
          while (J < Length(Core[0].Values)) and (Core[0].Values[J].Value <> Values[I]) do Inc(J);
          Check((J < Length(Core[0].Values)) = (Expected >= 0),
            'latent profile membership is exactly public emission/anchor membership');
          if Expected >= 0 then
          begin
            Inc(Matches);
            Check((Core[0].Values[J].Openings = C[0].Values[Expected].Openings) and
              (Core[0].Values[J].RequiredByValue = C[0].Values[Expected].RequiredByValue),
              'every duplicate-emission candidate retains exact ports and required flag');
          end;
        end;
        Check((Matches = Length(Core[0].Values)) and (Matches > Length(C[0].Values)),
          'inverse lowering includes all latent alternatives, not one representative token');
      finally Compiled.Free; end;
      RunAndReplay(M, Width, Height, 1, True);
      RunAndReplay(M, Width, Height, 1, True, nil, wpssNegotiated);
    finally M.Free; end;
  end;
end;

function GraphState(const G: TGraph): String;
var P, X, Y, Z, I: Integer; E: TGraphEntry; Allowed: TGraphValues;
begin
  Result := IntToStr(G.CurrentPassIndex) + '|';
  for P := 0 to G.TotalPassCount - 1 do
    for Z := 0 to Integer(G.Dimension.Depth) - 1 do
      for Y := 0 to Integer(G.Dimension.Height) - 1 do
        for X := 0 to Integer(G.Dimension.Width) - 1 do
        begin
          E := G.PassGraph[P].Entry[X, Y, Z];
          Result := Result + E.Value + ':' + IntToStr(Ord(E.Empty)) + ':' +
            IntToStr(Ord(E.Generated)) + ':' +
            IntToStr(Ord(G.PassGraph[P].HasAllowedValues(X, Y, Z))) + '[';
          Allowed := G.PassGraph[P].CopyAllowedValues(X, Y, Z);
          for I := 0 to High(Allowed) do Result := Result + Allowed[I] + ',';
          Result := Result + '];';
        end;
end;

procedure TestIndependentValidationAndRepair;
var M: TWfcPipelineModel; C: TWfcPipelineConnectivities;
  Compiled, Twin: TWfcCompiledPipeline; Report: TGraphSolveReport;
  Before: String; P, X, FailedPass, FailedEntry: Integer; Rejected: Boolean;
begin
  SetLength(C, 2);
  C[0] := Connection(0, nil, False, [gdEast, gdWest], False);
  C[1] := Connection(2, Positions([Position(3, 0, 0)]), True, [gdEast, gdWest]);
  M := Fixture(fkRules, 1, False, C);
  try
    Compiled := CompileWfcPipeline(M, 4, 1, 1); Twin := CompileWfcPipeline(M, 4, 1, 1);
    try
      Compiled.Graph.Seed := 91; Twin.Graph.Seed := 91;
      Check(Compiled.Graph.TrySolve(DefaultGraphSolveOptions, Report) and
        Twin.Graph.TrySolve(DefaultGraphSolveOptions, Report), 'connectivity rollback twins solve');
      Check(GraphState(Compiled.Graph) = GraphState(Twin.Graph), 'rollback twins begin identically');
      for P := 0 to 2 do Compiled.Graph.PassGraph[P].ClearConnectivity;
      for X := 0 to 3 do Compiled.Graph.PassGraph[0].SetAllowedValues(X, 0, 0, ['A', 'C']);
      Compiled.Graph.PassGraph[0].SetAllowedValues(1, 0, 0, ['B']);
      Before := GraphState(Compiled.Graph);
      Check(not Compiled.Graph.TrySolve(DefaultGraphSolveOptions, Report),
        'clearing mutable core constraints cannot bypass immutable recipe connectivity');
      Check((Report.Contradiction.Kind = gckFinalValidation) and
        (Compiled.LastValidation.Kind = wpcvkConnectivity) and
        (Compiled.LastValidation.ConnectivityIndex = 1) and
        (Compiled.LastValidation.PassIndex = 2),
        'independent connectivity failure retains declared ordinal and public alias owner');
      Check(Report.Passes[0].Decisions > 0, 'rejected route candidate consumed source randomness');
      Check(GraphState(Compiled.Graph) = Before, 'failed validation restores graph values, flags and domains');
      for P := 0 to 2 do for X := 0 to 2 do
        Check(Compiled.Graph.PassGraph[P].RandomIndex(1000000) =
          Twin.Graph.PassGraph[P].RandomIndex(1000000), 'failed connectivity commit restores each RNG stream');
    finally Twin.Free; Compiled.Free; end;
    Compiled := CompileWfcPipeline(M, 4, 1, 1);
    try
      Check(Compiled.Graph.TrySolve(DefaultGraphSolveOptions, Report), 'selective connectivity baseline solves');
      Before := GraphState(Compiled.Graph);
      Check(Compiled.Graph.TryRegenerateFrom('copy-two', DefaultGraphSolveOptions, Report),
        'copy-only selective repair accepts a preserved connected provider');
      Check(GraphState(Compiled.Graph) = Before, 'selective alias repair preserves connected source exactly');
      Compiled.Graph.PassGraph[0].Entry[1, 0, 0].Value := 'B';
      Before := GraphState(Compiled.Graph); Rejected := False;
      try Compiled.Graph.TryRegenerateFrom('copy-two', DefaultGraphSolveOptions, Report);
      except on E: EInvalidOperation do Rejected := True; end;
      Check(Rejected and (GraphState(Compiled.Graph) = Before),
        'selective repair cannot reuse a disconnected quota-free provider');
    finally Compiled.Free; end;
    Compiled := CompileWfcPipeline(M, 4, 1, 1);
    try
      Check(not TGraphValidationAccess.Validate(Compiled.Graph, FailedPass, FailedEntry),
        'independent commit check rejects incomplete public connectivity assignment');
    finally Compiled.Free; end;
  finally M.Free; end;
end;

procedure TestShapePreflight;
var M: TWfcPipelineModel; C: TWfcPipelineConnectivities;
  Compiled: TWfcCompiledPipeline; Rejected: Boolean; I: Integer; Source: String;
begin
  for I := 0 to 1 do
  begin
    C := OneConnection(Connection(0, nil, True, AllDirections));
    if I = 0 then C[0].Root := Position(3, 0, 0)
    else C[0].RequiredPositions := Positions([Position(3, 0, 0)]);
    M := Fixture(fkRules, 1, False, C);
    try
      Source := EncodeWfcPipelineModelText(M); Rejected := False; Compiled := nil;
      try Compiled := CompileWfcPipeline(M, 3, 1, 1);
      except on E: EWfcPipelineCompile do Rejected := E.Stage = wpcsConnectivity; end;
      Compiled.Free;
      Check(Rejected, 'out-of-shape root or terminal fails connectivity compilation preflight');
      Check(EncodeWfcPipelineModelText(M) = Source, 'failed shape application does not mutate authored recipe');
      RunAndReplay(M, 4, 1, 1, True);
    finally M.Free; end;
  end;
end;

procedure TestAggregateWorkPreflight;
var M: TWfcPipelineModel; C: TWfcPipelineConnectivities;
  I, Failed: Integer; Rejected: Boolean;
begin
  Check((WFC_PIPELINE_RUN_MAX_CELL_COUNT = 4194304) and
    (WFC_PIPELINE_CONNECTIVITY_MAX_CELL_VISITS = 16777216),
    'aggregate traversal fixture pins the supported invocation work limits');
  SetLength(C, 4);
  for I := 0 to High(C) do
  begin
    C[I] := Connection(0, nil, True, [gdEast, gdWest]);
    C[I].LabelText := 'bounded-route-' + IntToStr(I);
  end;
  M := Fixture(fkRules, 1, False, C);
  try
    { This is preflight only: never construct a four-million-cell graph or
      allocate traversal storage just to exercise the arithmetic boundary. }
    PreflightWfcPipelineConnectivity(M, WFC_PIPELINE_RUN_MAX_CELL_COUNT, 1, 1, Failed);
    Check(Failed = -1, 'four maximum-sized descriptor traversals meet the exact work cap');
  finally M.Free; end;
  SetLength(C, 5); C[4] := Connection(0, nil, True, [gdEast, gdWest]);
  C[4].LabelText := 'bounded-route-4';
  M := Fixture(fkRules, 1, False, C);
  try
    Rejected := False;
    try PreflightWfcPipelineConnectivity(M, WFC_PIPELINE_RUN_MAX_CELL_COUNT, 1, 1, Failed);
    except on E: EWfcPipelineConnectivity do
      Rejected := Pos('cell visits', E.Message) > 0; end;
    Check(Rejected and (Failed = 4), 'first over-budget descriptor is rejected before graph allocation');
  finally M.Free; end;
  M := Fixture(fkRules, 1, False, nil);
  try
    PreflightWfcPipelineConnectivity(M, 0, 0, 0, Failed);
    Check(Failed = -1, 'connectivity-free legacy recipe skips opt-in shape/work validation');
  finally M.Free; end;
end;

procedure TestForgeryCliAndLegacy;
var M, Legacy: TWfcPipelineModel; Run: TWfcPipelineRun; Output, Forged: TWfcPipelineResult;
  Layers: TWfcPipelineResultLayers; C: TWfcPipelineConnectivities;
  Command: TWfcRunCommand; Validation: TWfcValidateCommand;
  RecipeText, RunText, StandardOutput, StandardError: String;
  Rejected: Boolean; Code, I: Integer; D: TWfcPipelineCellDomains;
begin
  C := OneConnection(Connection(2, Positions([Position(3, 0, 0)]), True, [gdEast, gdWest]));
  M := Fixture(fkRules, 1, False, C);
  try
    Run := TWfcPipelineRun.Create(M, 4, 1, 1, 17, wpssOneWay, 128, 0, True, nil, nil);
    try
      Output := ExecuteWfcPipeline(M, Run);
      try
        CheckOutput(M, Output); Layers := Output.CopyLayers;
        for I := 0 to High(Layers) do Layers[I].Tokens[1] := 'B';
        Forged := nil; Rejected := False;
        try
          Forged := TWfcPipelineResult.Create(M, Run, Output.CopyVersions, Output.Status,
            Output.PassBacktracks, Output.EvidenceKind, Output.EvidenceSignature,
            Output.CopyFailure, Output.CopyPassOutcomes, Layers);
        except on E: EWfcPipelineResult do Rejected := True; end;
        Forged.Free;
        Check(Rejected, 'in-vocabulary forged disconnected successful layers are rejected independently');
        RecipeText := EncodeWfcPipelineModelText(M); RunText := EncodeWfcPipelineRunText(Run);
        Check(Pos('wfcpipeline=3'#10, RecipeText) = 1, 'connectivity recipe explicitly selects format three');
        Validation := Default(TWfcValidateCommand); Validation.Kind := wvckRecipe;
        Validation.OutputMode := wvomCanonical;
        Code := WfcValidateExecuteText(Validation, RecipeText, StandardOutput, StandardError);
        Check((Code = 0) and (StandardOutput = RecipeText) and (StandardError = ''),
          'shared native/browser validation CLI roundtrips canonical connectivity recipe');
        Validation.OutputMode := wvomSummary;
        Code := WfcValidateExecuteText(Validation, RecipeText, StandardOutput, StandardError);
        Check((Code = 0) and (StandardError = '') and
          (Pos('valid canonical wfcpipeline=3 ', StandardOutput) = 1) and
          (Pos('connectivities=1', StandardOutput) > 0), 'CLI summary advertises connectivity version and count');
        Command := Default(TWfcRunCommand); Command.Kind := wrckExecute; Command.OutputMode := wromCanonical;
        Code := WfcRunExecuteTexts(Command, RecipeText, RunText, StandardOutput, StandardError);
        Check((Code = 0) and (StandardError = '') and
          (StandardOutput = EncodeWfcPipelineResultText(Output)), 'CLI executes portable connected recipe exactly');
      finally Output.Free; end;
    finally Run.Free; end;
    D := FixedDomains(0, 4, 1, ['A', 'B', 'A', 'A']);
    Run := TWfcPipelineRun.Create(M, 4, 1, 1, 17, wpssOneWay, 128, 0, True, nil, D);
    try
      Output := ExecuteWfcPipeline(M, Run);
      try
        Check((Output.Status = wprsContradiction) and
          (Output.CopyFailure.Kind = gckConnectivity), 'disconnection is a canonical connectivity contradiction');
        Code := WfcRunExecuteTexts(Command, RecipeText, EncodeWfcPipelineRunText(Run), StandardOutput, StandardError);
        Check((Code = WFC_RUN_EXIT_NOT_SOLVED) and (StandardError = '') and
          (StandardOutput = EncodeWfcPipelineResultText(Output)) and
          (Pos('connectivity', StandardOutput) > 0), 'CLI emits complete disconnected failure with exit four');
      finally Output.Free; end;
    finally Run.Free; end;
  finally M.Free; end;
  Legacy := Fixture(fkRules, 1, False, nil);
  try
    RecipeText := EncodeWfcPipelineModelText(Legacy);
    Check((Pos('wfcpipeline=1'#10, RecipeText) = 1) and
      (Pos(#10'connectivity', RecipeText) = 0), 'empty extension preserves legacy recipe syntax');
    M := TWfcPipelineModel.Create(Legacy.CopyMetadata, Legacy.CopyVersions,
      Legacy.Rank, Legacy.WrapNeighbors, Legacy.RunMode, Legacy.CopyResources,
      Legacy.CopyPasses, Legacy.CopyDependencies, Legacy.CopyBridges, Legacy.CopyRequirements);
    try Check((M.Signature = Legacy.Signature) and (EncodeWfcPipelineModelText(M) = RecipeText),
      'old constructor and explicit empty connectivity preserve exact bytes and identity');
    finally M.Free; end;
  finally Legacy.Free; end;
  Check(ConnectedRoutesPortableSelfTest = 6, 'maintained portable route demo replays all three artifacts');
end;

begin
  try
    TestDirectAliasesAndPolicies;
    TestWrappingAndThreeDimensions;
    TestEveryOrientedPort;
    TestImmutableAdjacency;
    TestPrivateLowering;
    TestIndependentValidationAndRepair;
    TestShapePreflight;
    TestAggregateWorkPreflight;
    TestForgeryCliAndLegacy;
    WriteLn('Portable pipeline connectivity integration checks: ', Checks);
  except on E: Exception do begin WriteLn('FAIL: ', E.ClassName, ': ', E.Message); Halt(1); end; end;
end.
