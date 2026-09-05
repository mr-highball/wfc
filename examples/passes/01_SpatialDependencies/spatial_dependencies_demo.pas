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
unit spatial_dependencies_demo;

{$mode delphi}{$H+}

interface

procedure RunSpatialDependencies;

implementation

uses
  SysUtils,
  wfc;

const
  DEMO_WIDTH = 5;
  DEFAULT_SEED = TGraphSeed(0);

  PASS_TERRAIN = 'terrain';
  PASS_SETTLEMENT = 'settlement';
  PASS_FOLIAGE = 'foliage';

  TERRAIN_LAND = 'land';
  TERRAIN_FOREST = 'forest';
  TERRAIN_WATER = 'water';
  TERRAIN_MARSH = 'marsh';

  SETTLEMENT_NONE = 'none';
  SETTLEMENT_HOME = 'home';

  FOLIAGE_BARE = 'bare';
  FOLIAGE_REEDS = 'reeds';
  FOLIAGE_TREE = 'tree';

type
  ESpatialDependencies = class(Exception);

  TSpatialResult = record
    Terrain: String;
    Settlement: String;
    Foliage: String;
    Signature: String;
    Report: TGraphSolveReport;
  end;

procedure Require(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise ESpatialDependencies.Create(AMessage);
end;

function ParseSeed: TGraphSeed;
var
  I: Integer;
  LDigit: TGraphSeed;
  LParsed: TGraphSeed;
  LText: String;
begin
  if ParamCount = 0 then
    Exit(DEFAULT_SEED);
  if ParamCount <> 1 then
    raise EConvertError.Create(
      'usage: SpatialDependencies [unsigned-32-bit-seed]');
  LText := ParamStr(1);
  if LText = '' then
    raise EConvertError.Create('seed cannot be empty');
  LParsed := 0;
  for I := 1 to Length(LText) do
  begin
    if not (LText[I] in ['0'..'9']) then
      raise EConvertError.Create(
        'seed must be an unsigned 32-bit integer');
    LDigit := TGraphSeed(Ord(LText[I]) - Ord('0'));
    if LParsed > (High(TGraphSeed) - LDigit) div 10 then
      raise EConvertError.Create('seed exceeds 4294967295');
    LParsed := (LParsed * 10) + LDigit;
  end;
  Result := LParsed;
end;

function BuildableValues: TGraphValues;
begin
  Result := Default(TGraphValues);
  SetLength(Result, 2);
  Result[0] := TERRAIN_LAND;
  Result[1] := TERRAIN_FOREST;
end;

function AquaticValues: TGraphValues;
begin
  Result := Default(TGraphValues);
  SetLength(Result, 2);
  Result[0] := TERRAIN_WATER;
  Result[1] := TERRAIN_MARSH;
end;

function AquaticNeighborTerms: TGraphPassMatchTerms;
var
  LValues: TGraphValues;
begin
  LValues := AquaticValues;
  Result := Default(TGraphPassMatchTerms);
  SetLength(Result, 2);
  Result[0] := MakeGraphPassMatchTerm(
    MakeGraphOffset(-1, 0, 0), LValues);
  Result[1] := MakeGraphPassMatchTerm(
    MakeGraphOffset(1, 0, 0), LValues);
end;

procedure FixTerrain(const AGraph: TGraph);
begin
  AGraph.SetAllowedValues(0, 0, 0, TERRAIN_LAND);
  AGraph.SetAllowedValues(1, 0, 0, TERRAIN_LAND);
  AGraph.SetAllowedValues(2, 0, 0, TERRAIN_FOREST);
  AGraph.SetAllowedValues(3, 0, 0, TERRAIN_LAND);
  AGraph.SetAllowedValues(4, 0, 0, TERRAIN_WATER);
end;

procedure ConfigureGraph(const AGraph: TGraph;
  const ASeed: TGraphSeed; const AWrap: Boolean;
  const AForceEdgeConsumers: Boolean);
begin
  AGraph.Reshape(DEMO_WIDTH, 1, 1);
  AGraph.WrapNeighbors := AWrap;
  AGraph.Seed := ASeed;

  AGraph.CurrentPass := PASS_TERRAIN;
  AGraph.AddValue(TERRAIN_LAND);
  AGraph.AddValue(TERRAIN_FOREST);
  AGraph.AddValue(TERRAIN_WATER);
  AGraph.AddValue(TERRAIN_MARSH);
  FixTerrain(AGraph);

  AGraph.SwitchToPass(PASS_SETTLEMENT);
  AGraph.PassMode := gpmOverlay;
  AGraph.AddValue(SETTLEMENT_NONE);
  AGraph.AddValue(SETTLEMENT_HOME)
    .RequireFromPassAt(PASS_TERRAIN, MakeGraphOffset(0, 0, 0),
      BuildableValues)
    .RequireAnyFromPass(PASS_TERRAIN, AquaticNeighborTerms);
  if AForceEdgeConsumers then
    AGraph.SetAllowedValues(0, 0, 0, SETTLEMENT_HOME);

  AGraph.SwitchToPass(PASS_FOLIAGE);
  AGraph.PassMode := gpmOverlay;
  AGraph.AddValue(FOLIAGE_BARE);
  AGraph.AddValue(FOLIAGE_REEDS)
    .RequireFromPassAt(PASS_TERRAIN, MakeGraphOffset(0, 0, 0),
      TERRAIN_LAND)
    .RequireAnyFromPass(PASS_TERRAIN, AquaticNeighborTerms);
  AGraph.AddValue(FOLIAGE_TREE)
    .RequireFromPassAt(PASS_TERRAIN, MakeGraphOffset(0, 0, 0),
      TERRAIN_FOREST);
  AGraph.SetAllowedValues(2, 0, 0, FOLIAGE_TREE);
  if AForceEdgeConsumers then
    AGraph.SetAllowedValues(0, 0, 0, FOLIAGE_REEDS);
end;

function IsAquatic(const AValue: TGraphValue): Boolean;
begin
  Result := (AValue = TERRAIN_WATER) or
    (AValue = TERRAIN_MARSH);
end;

function TryOffsetX(const AX, ADelta: Integer;
  const AWrap: Boolean; out AResolvedX: Integer): Boolean;
begin
  AResolvedX := AX + ADelta;
  if AWrap then
  begin
    AResolvedX := AResolvedX mod DEMO_WIDTH;
    if AResolvedX < 0 then
      Inc(AResolvedX, DEMO_WIDTH);
    Exit(True);
  end;
  Result := (AResolvedX >= 0) and (AResolvedX < DEMO_WIDTH);
end;

function HasAquaticNeighbor(const ATerrain: TGraph;
  const AX: Integer; const AWrap: Boolean): Boolean;
var
  LResolvedX: Integer;
begin
  Result := False;
  if TryOffsetX(AX, -1, AWrap, LResolvedX) and
      IsAquatic(ATerrain.Entry[LResolvedX, 0, 0].Value) then
    Exit(True);
  if TryOffsetX(AX, 1, AWrap, LResolvedX) and
      IsAquatic(ATerrain.Entry[LResolvedX, 0, 0].Value) then
    Exit(True);
end;

function TerrainGlyph(const AValue: TGraphValue): Char;
begin
  if AValue = TERRAIN_LAND then
    Result := '.'
  else if AValue = TERRAIN_FOREST then
    Result := 'F'
  else if AValue = TERRAIN_WATER then
    Result := '~'
  else if AValue = TERRAIN_MARSH then
    Result := 'M'
  else
    raise ESpatialDependencies.Create('unknown terrain value');
end;

function SettlementGlyph(const AValue: TGraphValue): Char;
begin
  if AValue = SETTLEMENT_NONE then
    Result := '-'
  else if AValue = SETTLEMENT_HOME then
    Result := 'H'
  else
    raise ESpatialDependencies.Create('unknown settlement value');
end;

function FoliageGlyph(const AValue: TGraphValue): Char;
begin
  if AValue = FOLIAGE_BARE then
    Result := '-'
  else if AValue = FOLIAGE_REEDS then
    Result := 'R'
  else if AValue = FOLIAGE_TREE then
    Result := 'T'
  else
    raise ESpatialDependencies.Create('unknown foliage value');
end;

function CaptureLayer(const AGraph: TGraph;
  const APassIndex: Integer): String;
var
  I: Integer;
  LEntry: TGraphEntry;
begin
  Result := '';
  for I := 0 to DEMO_WIDTH - 1 do
  begin
    LEntry := AGraph.PassGraph[APassIndex].Entry[I, 0, 0];
    Require(not LEntry.Empty, 'solved layer contains an empty cell');
    case APassIndex of
      0: Result := Result + TerrainGlyph(LEntry.Value);
      1: Result := Result + SettlementGlyph(LEntry.Value);
      2: Result := Result + FoliageGlyph(LEntry.Value);
    else
      raise ESpatialDependencies.Create('unknown demo pass');
    end;
  end;
end;

procedure HashByte(var AHash: Cardinal; const AValue: Byte);
{$PUSH}
{$Q-}
var
  LValue: Cardinal;
begin
  AHash := AHash xor Cardinal(AValue);
  LValue := AHash;
  AHash := (LValue + (LValue shl 1) + (LValue shl 4) +
    (LValue shl 7) + (LValue shl 8) + (LValue shl 24)) and
    Cardinal($FFFFFFFF);
end;
{$POP}

procedure HashCardinal(var AHash: Cardinal; const AValue: Cardinal);
begin
  HashByte(AHash, Byte(AValue and $FF));
  HashByte(AHash, Byte((AValue shr 8) and $FF));
  HashByte(AHash, Byte((AValue shr 16) and $FF));
  HashByte(AHash, Byte((AValue shr 24) and $FF));
end;

procedure HashText(var AHash: Cardinal; const AText: String);
var
  I: Integer;
begin
  HashCardinal(AHash, Cardinal(Length(AText)));
  for I := 1 to Length(AText) do
    HashByte(AHash, Byte(Ord(AText[I])));
end;

function HexCardinal(const AValue: Cardinal): String;
const
  HEX_DIGITS = '0123456789ABCDEF';
var
  I: Integer;
begin
  SetLength(Result, 8);
  for I := 1 to 8 do
    Result[I] := HEX_DIGITS[((AValue shr ((8 - I) * 4)) and $F) + 1];
end;

function LayerHash(const ALayer: String; const APassIndex: Integer;
  const AWrap: Boolean): Cardinal;
var
  LHash: Cardinal;
begin
  LHash := Cardinal(2166136261);
  HashText(LHash, 'WFC spatial dependencies');
  HashCardinal(LHash, Cardinal(WFC_PIPELINE_ALGORITHM_VERSION));
  HashCardinal(LHash, Cardinal(APassIndex));
  if AWrap then
    HashByte(LHash, 1)
  else
    HashByte(LHash, 0);
  HashText(LHash, ALayer);
  Result := LHash;
end;

function BuildSignature(const ATerrain, ASettlement,
  AFoliage: String; const AWrap: Boolean): String;
var
  LBoundary: Char;
begin
  if AWrap then
    LBoundary := 'W'
  else
    LBoundary := 'B';
  Result := IntToStr(WFC_PIPELINE_ALGORITHM_VERSION) + ':' + LBoundary +
    ':' + HexCardinal(LayerHash(ATerrain, 0, AWrap)) +
    ':' + HexCardinal(LayerHash(ASettlement, 1, AWrap)) +
    ':' + HexCardinal(LayerHash(AFoliage, 2, AWrap));
end;

procedure ValidateScenario(const AGraph: TGraph;
  const AWrap: Boolean);
var
  I: Integer;
  LFoliage: TGraphValue;
  LSettlement: TGraphValue;
  LTerrain: TGraphValue;
begin
  Require(AGraph.TotalPassCount = 3, 'demo pass count changed');
  for I := 0 to DEMO_WIDTH - 1 do
  begin
    LTerrain := AGraph.PassGraph[0].Entry[I, 0, 0].Value;
    LSettlement := AGraph.PassGraph[1].Entry[I, 0, 0].Value;
    LFoliage := AGraph.PassGraph[2].Entry[I, 0, 0].Value;

    Require((LSettlement = SETTLEMENT_NONE) or
      (LSettlement = SETTLEMENT_HOME),
      'settlement contains an unknown value');
    if LSettlement = SETTLEMENT_HOME then
      Require(((LTerrain = TERRAIN_LAND) or
        (LTerrain = TERRAIN_FOREST)) and
        HasAquaticNeighbor(AGraph.PassGraph[0], I, AWrap),
        'home violates its buildable-and-near-water clauses');

    Require((LFoliage = FOLIAGE_BARE) or
      (LFoliage = FOLIAGE_REEDS) or
      (LFoliage = FOLIAGE_TREE),
      'foliage contains an unknown value');
    if LFoliage = FOLIAGE_REEDS then
      Require((LTerrain = TERRAIN_LAND) and
        HasAquaticNeighbor(AGraph.PassGraph[0], I, AWrap),
        'reeds violate their land-and-near-water clauses')
    else if LFoliage = FOLIAGE_TREE then
      Require(LTerrain = TERRAIN_FOREST,
        'tree violates its forest clause');
  end;

  Require(AGraph.PassGraph[2].Entry[2, 0, 0].Value = FOLIAGE_TREE,
    'the forest tree anchor changed');
  if AWrap then
  begin
    Require(AGraph.PassGraph[1].Entry[0, 0, 0].Value = SETTLEMENT_HOME,
      'wrapped west-edge home did not see east-edge water');
    Require(AGraph.PassGraph[2].Entry[0, 0, 0].Value = FOLIAGE_REEDS,
      'wrapped west-edge reeds did not see east-edge water');
  end
  else
  begin
    Require(AGraph.PassGraph[1].Entry[0, 0, 0].Value = SETTLEMENT_NONE,
      'bounded west edge unexpectedly matched outside the graph');
    Require(AGraph.PassGraph[2].Entry[0, 0, 0].Value = FOLIAGE_BARE,
      'bounded west-edge foliage unexpectedly matched outside the graph');
  end;
end;

function SolveScenario(const ASeed: TGraphSeed;
  const AWrap, AForceEdgeConsumers: Boolean;
  out AResult: TSpatialResult): Boolean;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
begin
  AResult := Default(TSpatialResult);
  LGraph := TGraph.Create;
  try
    ConfigureGraph(LGraph, ASeed, AWrap, AForceEdgeConsumers);
    LOptions := DefaultGraphSolveOptions;
    Result := LGraph.TrySolve(LOptions, AResult.Report);
    if not Result then
      Exit;
    ValidateScenario(LGraph, AWrap);
    AResult.Terrain := CaptureLayer(LGraph, 0);
    AResult.Settlement := CaptureLayer(LGraph, 1);
    AResult.Foliage := CaptureLayer(LGraph, 2);
    AResult.Signature := BuildSignature(AResult.Terrain,
      AResult.Settlement, AResult.Foliage, AWrap);
  finally
    LGraph.Free;
  end;
end;

procedure RequireSolvedReport(const AResult: TSpatialResult);
begin
  Require(AResult.Report.Status = gssSolved,
    'successful scenario did not report solved');
  Require(AResult.Report.PipelineAlgorithmVersion =
    WFC_PIPELINE_ALGORITHM_VERSION,
    'solve report pipeline version changed');
  Require(Length(AResult.Report.ExecutionOrder) = 3,
    'solve report execution count changed');
  Require((AResult.Report.ExecutionOrder[0] = 0) and
    (AResult.Report.ExecutionOrder[1] = 1) and
    (AResult.Report.ExecutionOrder[2] = 2),
    'solve report execution order changed');
end;

procedure VerifyReplay(const ASeed: TGraphSeed; const AWrap: Boolean;
  const AExpected: TSpatialResult);
var
  LReplay: TSpatialResult;
begin
  Require(SolveScenario(ASeed, AWrap, AWrap, LReplay),
    'same-seed replay did not solve');
  Require((LReplay.Terrain = AExpected.Terrain) and
    (LReplay.Settlement = AExpected.Settlement) and
    (LReplay.Foliage = AExpected.Foliage) and
    (LReplay.Signature = AExpected.Signature),
    'same seed did not replay byte-identical layer output');
end;

procedure VerifyBoundedRejection(const ASeed: TGraphSeed);
var
  LProbe: TSpatialResult;
begin
  Require(not SolveScenario(ASeed, False, True, LProbe),
    'bounded out-of-bounds requirement unexpectedly solved');
  Require(LProbe.Report.Status = gssContradiction,
    'bounded out-of-bounds requirement did not report contradiction');
  Require(LProbe.Report.FailedPassIndex = 1,
    'bounded out-of-bounds contradiction named the wrong consumer pass');
  Require(LProbe.Report.Contradiction.DependencyPassIndex = 0,
    'bounded out-of-bounds contradiction named the wrong provider pass');
end;

procedure PrintScenario(const AName: String;
  const AResult: TSpatialResult);
begin
  WriteLn(AName, ' terrain:    ', AResult.Terrain);
  WriteLn(AName, ' settlement: ', AResult.Settlement);
  WriteLn(AName, ' foliage:    ', AResult.Foliage);
  WriteLn(AName, ' signature:  ', AResult.Signature);
end;

procedure RunSpatialDependencies;
var
  LBounded: TSpatialResult;
  LSeed: TGraphSeed;
  LWrapped: TSpatialResult;
begin
  LSeed := ParseSeed;
  Require(SolveScenario(LSeed, False, False, LBounded),
    'bounded spatial pipeline did not solve');
  Require(SolveScenario(LSeed, True, True, LWrapped),
    'wrapped spatial pipeline did not solve');
  RequireSolvedReport(LBounded);
  RequireSolvedReport(LWrapped);
  VerifyBoundedRejection(LSeed);
  VerifyReplay(LSeed, False, LBounded);
  VerifyReplay(LSeed, True, LWrapped);

  WriteLn('SpatialDependencies: terrain -> settlement + foliage');
  WriteLn('Seed: ', LSeed);
  WriteLn('Pipeline version: ', WFC_PIPELINE_ALGORITHM_VERSION);
  PrintScenario('bounded', LBounded);
  PrintScenario('wrapped', LWrapped);
  WriteLn('Legend: .=land F=forest ~=water H=home R=reeds T=tree -=none');
  WriteLn('Bounded out-of-bounds probe: rejected');
  WriteLn('Independent checks: 30 cells plus spatial clauses');
  WriteLn('Self-check: passed');
end;

end.
