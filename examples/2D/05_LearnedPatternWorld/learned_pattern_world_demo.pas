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
unit learned_pattern_world_demo;

{$mode delphi}{$H+}

interface

uses
  wfc;

function ParseLearnedPatternWorldSeed: TGraphSeed;
procedure RunLearnedPatternWorldDemo(const ASeed: TGraphSeed);

implementation

uses
  SysUtils,
  wfc_model,
  wfc_learn,
  wfc_pattern2d,
  wfc_pattern2d_learn,
  wfc_pattern2d_text,
  wfc_pattern2d_graph;

const
  DEMO_DEFAULT_SEED = TGraphSeed(0);
  OUTPUT_WIDTH = 8;
  OUTPUT_HEIGHT = 6;
  PATTERN_WIDTH = 2;
  PATTERN_HEIGHT = 2;

  PASS_PATTERNS = WFC_PATTERN_2D_PASS_PATTERNS;
  PASS_TERRAIN = 'terrain';
  PASS_FOLIAGE = 'foliage';
  PASS_STRUCTURE = 'structure';

  TERRAIN_WATER = '~';
  TERRAIN_GROUND = '.';
  TERRAIN_ROCK = '#';

  FOLIAGE_REEDS = 'reeds';
  FOLIAGE_GRASS = 'grass';
  FOLIAGE_TREE = 'tree';
  FOLIAGE_MOSS = 'moss';

  STRUCTURE_DOCK = 'dock';
  STRUCTURE_HUT = 'hut';
  STRUCTURE_MINE = 'mine';

  SAMPLE_ZERO_WIDTH = 5;
  SAMPLE_ZERO_HEIGHT = 5;
  SAMPLE_ZERO_ROWS: array[0..SAMPLE_ZERO_HEIGHT - 1] of String = (
    '~~~~~',
    '~...~',
    '~.#.~',
    '~...~',
    '~~~~~'
  );
  SAMPLE_ONE_WIDTH = 7;
  SAMPLE_ONE_HEIGHT = 5;
  SAMPLE_ONE_ROWS: array[0..SAMPLE_ONE_HEIGHT - 1] of String = (
    '~~~~~~~',
    '~.....~',
    '~.###.~',
    '~.....~',
    '~~~~~~~'
  );

  FNV_OFFSET_BASIS = Cardinal(2166136261);
  HEX_DIGITS = '0123456789ABCDEF';

  EXPECTED_PATTERN_COUNT = 17;
  EXPECTED_CANONICAL_BYTES = 2130;
  EXPECTED_MODEL_HASH = Cardinal($9BF802CC);
  EXPECTED_LATENT_HASH = Cardinal($D800EC4B);
  EXPECTED_TERRAIN_HASH = Cardinal($EBBC9390);
  EXPECTED_FOLIAGE_HASH = Cardinal($92D4BC87);
  EXPECTED_STRUCTURE_HASH = Cardinal($8FA9D854);
  EXPECTED_PIPELINE_HASH = Cardinal($38FE98C4);

type
  ELearnedPatternWorld = class(Exception);

  TWorldValidationIssueKind = (
    wvikNone,
    wvikShape,
    wvikPassIdentity,
    wvikPatternAssignment,
    wvikProjection,
    wvikTerrainMismatch,
    wvikFoliageMismatch,
    wvikStructureMismatch,
    wvikPrivateKeyLeak
  );

  TWorldValidationReport = record
    Valid: Boolean;
    Issue: TWorldValidationIssueKind;
    FailedPassIndex: Integer;
    FailedEntryIndex: Integer;
    CheckedPatterns: Integer;
    CheckedRelations: Integer;
    CheckedProjectionCells: Integer;
    CheckedPublicCells: Integer;
    CheckedConsumerRelations: Integer;
    PrivateKeyLeaks: Integer;
  end;

  TWorldCellState = record
    Value: TGraphValue;
    Empty: Boolean;
    Generated: Boolean;
  end;
  TWorldCellStates = array of TWorldCellState;

  TWorldSnapshot = record
    CurrentPassIndex: Integer;
    Cells: TWorldCellStates;
  end;

  TLearnedPatternWorldGraph = class(TGraph)
  strict private
    FPatternModel: TWfcOverlappingModel2D;
  strict protected
    function DoValidateCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean; override;
  public
    property PatternModel: TWfcOverlappingModel2D
      read FPatternModel write FPatternModel;
  end;

function ValidateLearnedPatternWorld(const AGraph: TGraph;
  const AModel: TWfcOverlappingModel2D;
  out AReport: TWorldValidationReport): Boolean; forward;

procedure Require(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise ELearnedPatternWorld.Create(AMessage);
end;

function ParseLearnedPatternWorldSeed: TGraphSeed;
var
  I: Integer;
  LDigit: TGraphSeed;
  LParsed: TGraphSeed;
  LText: String;
begin
  if ParamCount = 0 then
    Exit(DEMO_DEFAULT_SEED);
  if ParamCount <> 1 then
    raise EConvertError.Create(
      'usage: LearnedPatternWorld [unsigned-32-bit-seed]');
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

function GraphValueFromModelToken(
  const AToken: TWfcModelToken): TGraphValue;
begin
  {$IFDEF PAS2JS}
  Result := TGraphValue(AToken);
  {$ELSE}
  Result := TGraphValue(UTF8Decode(AToken));
  {$ENDIF}
end;

function ModelTokenFromGraphValue(
  const AValue: TGraphValue): TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(AValue);
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(AValue));
  {$ENDIF}
end;

function TokensFromRows(const ARows: array of String;
  const AWidth, AHeight: Integer): TWfcModelTokens;
var
  X: Integer;
  Y: Integer;
begin
  Result := nil;
  if Length(ARows) <> AHeight then
    raise ELearnedPatternWorld.Create(
      'training row count does not match its declared shape');
  SetLength(Result, AWidth * AHeight);
  for Y := 0 to AHeight - 1 do
  begin
    if Length(ARows[Y]) <> AWidth then
      raise ELearnedPatternWorld.CreateFmt(
        'training row %d has %d tokens; expected %d',
        [Y, Length(ARows[Y]), AWidth]);
    for X := 0 to AWidth - 1 do
      Result[Y * AWidth + X] :=
        TWfcModelToken(Copy(ARows[Y], X + 1, 1));
  end;
end;

function BuildTrainingCorpus: TWfcLearnSamples;
begin
  Result := nil;
  SetLength(Result, 2);
  Result[0] := MakeLearnSample2D(
    TokensFromRows(SAMPLE_ZERO_ROWS, SAMPLE_ZERO_WIDTH,
      SAMPLE_ZERO_HEIGHT), SAMPLE_ZERO_WIDTH, SAMPLE_ZERO_HEIGHT);
  Result[1] := MakeLearnSample2D(
    TokensFromRows(SAMPLE_ONE_ROWS, SAMPLE_ONE_WIDTH,
      SAMPLE_ONE_HEIGHT), SAMPLE_ONE_WIDTH, SAMPLE_ONE_HEIGHT);
end;

function FindOriginPatternContaining(
  const AModel: TWfcOverlappingModel2D;
  const AToken: TWfcModelToken): Integer;
var
  LPaletteIndex: Integer;
  P: Integer;
begin
  LPaletteIndex := AModel.FindPaletteToken(AToken);
  if LPaletteIndex < 0 then
    raise ELearnedPatternWorld.Create(
      'the training palette does not contain the anchor token');
  for P := 0 to AModel.PatternCount - 1 do
    if AModel.PatternPaletteIndexAt(P, 0, 0) = LPaletteIndex then
      Exit(P);
  raise ELearnedPatternWorld.Create(
    'no learned pattern projects the anchor token at its origin');
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
  HashByte(AHash, Byte(AValue));
  HashByte(AHash, Byte(AValue shr 8));
  HashByte(AHash, Byte(AValue shr 16));
  HashByte(AHash, Byte(AValue shr 24));
end;

procedure HashAscii(var AHash: Cardinal; const AValue: String);
var
  I: Integer;
  LCode: Integer;
begin
  HashCardinal(AHash, Cardinal(Length(AValue)));
  for I := 1 to Length(AValue) do
  begin
    LCode := Ord(AValue[I]);
    if (LCode < 0) or (LCode > 127) then
      raise ELearnedPatternWorld.Create(
        'the portable demo signature accepts canonical ASCII only');
    HashByte(AHash, Byte(LCode));
  end;
end;

function CardinalHex(const AValue: Cardinal): String;
var
  I: Integer;
  LValue: Cardinal;
begin
  SetLength(Result, 8);
  LValue := AValue;
  for I := 8 downto 1 do
  begin
    Result[I] := HEX_DIGITS[Integer(LValue and Cardinal($F)) + 1];
    LValue := LValue shr 4;
  end;
end;

function CanonicalModelHash(const ACanonical: String): Cardinal;
begin
  Result := FNV_OFFSET_BASIS;
  HashAscii(Result, 'LearnedPatternWorld/model-v1');
  HashAscii(Result, ACanonical);
end;

function LatentAssignmentHash(const AGrid: TWfcPatternGrid2D): Cardinal;
var
  I: Integer;
begin
  Result := FNV_OFFSET_BASIS;
  HashAscii(Result, 'LearnedPatternWorld/latent-v1');
  HashCardinal(Result, Cardinal(AGrid.Width));
  HashCardinal(Result, Cardinal(AGrid.Height));
  HashCardinal(Result, Cardinal(Ord(AGrid.Boundary)));
  for I := 0 to High(AGrid.Patterns) do
    HashCardinal(Result, Cardinal(AGrid.Patterns[I]));
end;

function PublicLayerHash(const AGraph: TGraph;
  const APassIndex: Integer; const ALabel: String): Cardinal;
var
  LEntry: TGraphEntry;
  X: Integer;
  Y: Integer;
begin
  Result := FNV_OFFSET_BASIS;
  HashAscii(Result, 'LearnedPatternWorld/layer-v1');
  HashAscii(Result, ALabel);
  HashCardinal(Result, Cardinal(OUTPUT_WIDTH));
  HashCardinal(Result, Cardinal(OUTPUT_HEIGHT));
  for Y := 0 to OUTPUT_HEIGHT - 1 do
    for X := 0 to OUTPUT_WIDTH - 1 do
    begin
      LEntry := AGraph.PassGraph[APassIndex].Entry[X, Y, 0];
      if LEntry.Empty then
        raise ELearnedPatternWorld.Create(
          'cannot sign a public layer containing an empty entry');
      HashAscii(Result, LEntry.Value);
    end;
end;

function PipelineHash(const AModelHash, ALatentHash, ATerrainHash,
  AFoliageHash, AStructureHash: Cardinal): Cardinal;
begin
  Result := FNV_OFFSET_BASIS;
  HashAscii(Result, 'LearnedPatternWorld/pipeline-v1');
  HashCardinal(Result, WFC_PATTERN_2D_GRAPH_ADAPTER_VERSION);
  HashCardinal(Result, AModelHash);
  HashCardinal(Result, ALatentHash);
  HashCardinal(Result, ATerrainHash);
  HashCardinal(Result, AFoliageHash);
  HashCardinal(Result, AStructureHash);
end;

function FoliageMatchesTerrain(const AFoliage,
  ATerrain: TGraphValue): Boolean;
begin
  if ATerrain = TERRAIN_WATER then
    Exit(AFoliage = FOLIAGE_REEDS);
  if ATerrain = TERRAIN_GROUND then
    Exit((AFoliage = FOLIAGE_GRASS) or
      (AFoliage = FOLIAGE_TREE));
  if ATerrain = TERRAIN_ROCK then
    Exit(AFoliage = FOLIAGE_MOSS);
  Result := False;
end;

function StructureMatchesTerrain(const AStructure,
  ATerrain: TGraphValue): Boolean;
begin
  if ATerrain = TERRAIN_WATER then
    Exit(AStructure = STRUCTURE_DOCK);
  if ATerrain = TERRAIN_GROUND then
    Exit(AStructure = STRUCTURE_HUT);
  if ATerrain = TERRAIN_ROCK then
    Exit(AStructure = STRUCTURE_MINE);
  Result := False;
end;

procedure SetValidationIssue(var AReport: TWorldValidationReport;
  const AKind: TWorldValidationIssueKind;
  const APassIndex, AEntryIndex: Integer);
begin
  AReport.Valid := False;
  AReport.Issue := AKind;
  AReport.FailedPassIndex := APassIndex;
  AReport.FailedEntryIndex := AEntryIndex;
end;

function ValidateLearnedPatternWorld(const AGraph: TGraph;
  const AModel: TWfcOverlappingModel2D;
  out AReport: TWorldValidationReport): Boolean;
var
  I: Integer;
  LAssignmentReport: TWfcOverlapping2DValidationReport;
  LFoliage: TGraphValue;
  LGrid: TWfcPatternGrid2D;
  LProjection: TWfcTokenGrid2D;
  LProjectionReport: TWfcOverlapping2DValidationReport;
  LPublicValue: TGraphValue;
  P: Integer;
  LStructure: TGraphValue;
  LTerrain: TGraphValue;
  X: Integer;
  Y: Integer;
begin
  AReport := Default(TWorldValidationReport);
  AReport.FailedPassIndex := -1;
  AReport.FailedEntryIndex := -1;
  if (not Assigned(AGraph)) or (not Assigned(AModel)) or
      (AGraph.TotalPassCount <> 4) or
      (AGraph.Dimension.Width <> OUTPUT_WIDTH) or
      (AGraph.Dimension.Height <> OUTPUT_HEIGHT) or
      (AGraph.Dimension.Depth <> 1) or
      (not AGraph.WrapNeighbors) then
  begin
    SetValidationIssue(AReport, wvikShape, -1, -1);
    Exit(False);
  end;
  if (AGraph.PassGraph[0].CurrentPass <> PASS_PATTERNS) or
      (AGraph.PassGraph[1].CurrentPass <> PASS_TERRAIN) or
      (AGraph.PassGraph[2].CurrentPass <> PASS_FOLIAGE) or
      (AGraph.PassGraph[3].CurrentPass <> PASS_STRUCTURE) then
  begin
    SetValidationIssue(AReport, wvikPassIdentity, -1, -1);
    Exit(False);
  end;

  if not CaptureSolvedPatternGrid2D(AModel, AGraph.PassGraph[0], 0,
      LGrid, LAssignmentReport) then
  begin
    SetValidationIssue(AReport, wvikPatternAssignment, 0,
      LAssignmentReport.Issue.Y * OUTPUT_WIDTH +
      LAssignmentReport.Issue.X);
    Exit(False);
  end;
  AReport.CheckedPatterns := LAssignmentReport.CheckedPatterns;
  AReport.CheckedRelations := LAssignmentReport.CheckedRelations;
  if not TryProjectOverlappingPatternGrid2D(AModel, LGrid,
      LProjection, LProjectionReport) then
  begin
    SetValidationIssue(AReport, wvikProjection, 1,
      LProjectionReport.Issue.Y * OUTPUT_WIDTH +
      LProjectionReport.Issue.X);
    Exit(False);
  end;
  if not ValidateOverlappingProjection2D(AModel, LGrid,
      LProjection, LProjectionReport) then
  begin
    SetValidationIssue(AReport, wvikProjection, 1,
      LProjectionReport.Issue.Y * OUTPUT_WIDTH +
      LProjectionReport.Issue.X);
    Exit(False);
  end;
  AReport.CheckedProjectionCells :=
    LProjectionReport.CheckedProjectionCells;

  for Y := 0 to OUTPUT_HEIGHT - 1 do
    for X := 0 to OUTPUT_WIDTH - 1 do
    begin
      I := Y * OUTPUT_WIDTH + X;
      LTerrain := AGraph.PassGraph[1].Entry[X, Y, 0].Value;
      if AGraph.PassGraph[1].Entry[X, Y, 0].Empty or
          (ModelTokenFromGraphValue(LTerrain) <> LProjection.Tokens[I]) then
      begin
        SetValidationIssue(AReport, wvikTerrainMismatch, 1, I);
        Exit(False);
      end;
      LFoliage := AGraph.PassGraph[2].Entry[X, Y, 0].Value;
      if AGraph.PassGraph[2].Entry[X, Y, 0].Empty or
          (not FoliageMatchesTerrain(LFoliage, LTerrain)) then
      begin
        SetValidationIssue(AReport, wvikFoliageMismatch, 2, I);
        Exit(False);
      end;
      LStructure := AGraph.PassGraph[3].Entry[X, Y, 0].Value;
      if AGraph.PassGraph[3].Entry[X, Y, 0].Empty or
          (not StructureMatchesTerrain(LStructure, LTerrain)) then
      begin
        SetValidationIssue(AReport, wvikStructureMismatch, 3, I);
        Exit(False);
      end;

      Inc(AReport.CheckedPublicCells, 3);
      Inc(AReport.CheckedConsumerRelations, 2);
      for P := 1 to 3 do
      begin
        LPublicValue := AGraph.PassGraph[P].Entry[X, Y, 0].Value;
        if AModel.FindPatternKey(
            ModelTokenFromGraphValue(LPublicValue)) >= 0 then
        begin
          Inc(AReport.PrivateKeyLeaks);
          SetValidationIssue(AReport, wvikPrivateKeyLeak, P,
            Y * OUTPUT_WIDTH + X);
          Exit(False);
        end;
      end;
    end;
  AReport.Valid := True;
  AReport.Issue := wvikNone;
  Result := True;
end;

function TLearnedPatternWorldGraph.DoValidateCommit(
  out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
var
  LReport: TWorldValidationReport;
begin
  if not Assigned(FPatternModel) then
    Exit(inherited DoValidateCommit(AFailedPassIndex,
      AFailedEntryIndex));
  Result := ValidateLearnedPatternWorld(Self, FPatternModel, LReport);
  AFailedPassIndex := LReport.FailedPassIndex;
  AFailedEntryIndex := LReport.FailedEntryIndex;
end;

procedure ConfigureFoliagePass(const AGraph: TGraph);
begin
  AGraph.AddValue(FOLIAGE_REEDS, 2)
    .RequireFromPass(PASS_TERRAIN, TERRAIN_WATER);
  AGraph.AddValue(FOLIAGE_GRASS, 3)
    .RequireFromPass(PASS_TERRAIN, TERRAIN_GROUND);
  AGraph.AddValue(FOLIAGE_TREE, 1)
    .RequireFromPass(PASS_TERRAIN, TERRAIN_GROUND);
  AGraph.AddValue(FOLIAGE_MOSS, 1)
    .RequireFromPass(PASS_TERRAIN, TERRAIN_ROCK);
end;

procedure ConfigureStructurePass(const AGraph: TGraph);
begin
  AGraph.AddValue(STRUCTURE_DOCK)
    .RequireFromPass(PASS_TERRAIN, TERRAIN_WATER);
  AGraph.AddValue(STRUCTURE_HUT)
    .RequireFromPass(PASS_TERRAIN, TERRAIN_GROUND);
  AGraph.AddValue(STRUCTURE_MINE)
    .RequireFromPass(PASS_TERRAIN, TERRAIN_ROCK);
end;

function CreateWorldGraph(const AModel: TWfcOverlappingModel2D;
  const ASeed: TGraphSeed): TLearnedPatternWorldGraph;
var
  LAnchorPattern: Integer;
  LWaterAnchorPattern: Integer;
begin
  Result := TLearnedPatternWorldGraph.Create;
  try
    Result.PatternModel := AModel;
    Result.Seed := ASeed;
    Result.WrapNeighbors := True;
    Result.Reshape(OUTPUT_WIDTH, OUTPUT_HEIGHT, 1);

    Result.CurrentPass := PASS_PATTERNS;
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    ApplyOverlappingModel2DToGraph(AModel, Result);

    Result.SwitchToPass(PASS_TERRAIN);
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    ValidateOverlappingProjectionFromPass2D(AModel, Result,
      PASS_PATTERNS);
    ApplyOverlappingProjectionFromPass2D(AModel, Result,
      PASS_PATTERNS);

    Result.SwitchToPass(PASS_FOLIAGE);
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    ConfigureFoliagePass(Result);

    Result.SwitchToPass(PASS_STRUCTURE);
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    ConfigureStructurePass(Result);

    LAnchorPattern := FindOriginPatternContaining(AModel,
      TWfcModelToken(TERRAIN_ROCK));
    LWaterAnchorPattern := FindOriginPatternContaining(AModel,
      TWfcModelToken(TERRAIN_WATER));
    Result.PassGraph[0].Entry[0, 0, 0].Value :=
      GraphValueFromModelToken(
        AModel.PatternKeyAt(LWaterAnchorPattern));
    Result.PassGraph[0].Entry[OUTPUT_WIDTH div 2,
      OUTPUT_HEIGHT div 2, 0].Value := GraphValueFromModelToken(
        AModel.PatternKeyAt(LAnchorPattern));
  except
    Result.Free;
    raise;
  end;
end;

function CaptureWorldSnapshot(const AGraph: TGraph): TWorldSnapshot;
var
  LEntry: TGraphEntry;
  LIndex: Integer;
  P: Integer;
  X: Integer;
  Y: Integer;
begin
  Result.CurrentPassIndex := AGraph.CurrentPassIndex;
  SetLength(Result.Cells,
    AGraph.TotalPassCount * OUTPUT_WIDTH * OUTPUT_HEIGHT);
  LIndex := 0;
  for P := 0 to AGraph.TotalPassCount - 1 do
    for Y := 0 to OUTPUT_HEIGHT - 1 do
      for X := 0 to OUTPUT_WIDTH - 1 do
      begin
        LEntry := AGraph.PassGraph[P].Entry[X, Y, 0];
        Result.Cells[LIndex].Value := LEntry.Value;
        Result.Cells[LIndex].Empty := LEntry.Empty;
        Result.Cells[LIndex].Generated := LEntry.Generated;
        Inc(LIndex);
      end;
end;

function WorldSnapshotsEqual(const A, B: TWorldSnapshot): Boolean;
var
  I: Integer;
begin
  if (A.CurrentPassIndex <> B.CurrentPassIndex) or
      (Length(A.Cells) <> Length(B.Cells)) then
    Exit(False);
  for I := 0 to High(A.Cells) do
    if (A.Cells[I].Value <> B.Cells[I].Value) or
        (A.Cells[I].Empty <> B.Cells[I].Empty) or
        (A.Cells[I].Generated <> B.Cells[I].Generated) then
      Exit(False);
  Result := True;
end;

function RandomStreamsMatch(const A, B: TGraph): Boolean;
var
  P: Integer;
begin
  if A.TotalPassCount <> B.TotalPassCount then
    Exit(False);
  for P := 0 to A.TotalPassCount - 1 do
    if A.PassGraph[P].RandomIndex(1000003) <>
        B.PassGraph[P].RandomIndex(1000003) then
      Exit(False);
  Result := True;
end;

function ExecutionOrderEquals(const AReport: TGraphSolveReport;
  const AExpected: array of Integer): Boolean;
var
  I: Integer;
begin
  if Length(AReport.ExecutionOrder) <> Length(AExpected) then
    Exit(False);
  for I := 0 to High(AExpected) do
    if AReport.ExecutionOrder[I] <> AExpected[I] then
      Exit(False);
  Result := True;
end;

procedure CaptureProjectionForSignature(const AGraph: TGraph;
  const AModel: TWfcOverlappingModel2D;
  out AGrid: TWfcPatternGrid2D);
var
  LAssignmentReport: TWfcOverlapping2DValidationReport;
begin
  Require(CaptureSolvedPatternGrid2D(AModel,
      AGraph.PassGraph[0], 0, AGrid, LAssignmentReport),
    'could not capture the latent assignment for its signature: ' +
      DescribeOverlapping2DIssue(LAssignmentReport.Issue));
end;

procedure RenderWorld(const AGraph: TGraph);
var
  P: Integer;
  X: Integer;
  Y: Integer;
begin
  WriteLn('terrain      foliage     structure');
  for Y := 0 to OUTPUT_HEIGHT - 1 do
  begin
    for P := 1 to 3 do
    begin
      for X := 0 to OUTPUT_WIDTH - 1 do
      begin
        if P = 1 then
          Write(AGraph.PassGraph[P].Entry[X, Y, 0].Value)
        else if P = 2 then
        begin
          if AGraph.PassGraph[P].Entry[X, Y, 0].Value = FOLIAGE_REEDS then
            Write('r')
          else if AGraph.PassGraph[P].Entry[X, Y, 0].Value = FOLIAGE_GRASS then
            Write('g')
          else if AGraph.PassGraph[P].Entry[X, Y, 0].Value = FOLIAGE_TREE then
            Write('T')
          else
            Write('m');
        end
        else
        begin
          if AGraph.PassGraph[P].Entry[X, Y, 0].Value = STRUCTURE_DOCK then
            Write('D')
          else if AGraph.PassGraph[P].Entry[X, Y, 0].Value = STRUCTURE_HUT then
            Write('H')
          else
            Write('M');
        end;
      end;
      if P <> 3 then
        Write('   ');
    end;
    WriteLn;
  end;
end;

procedure RequireSeedZeroGoldens(const ASeed: TGraphSeed;
  const ACanonicalBytes: Integer; const AModelHash, ALatentHash,
  ATerrainHash, AFoliageHash, AStructureHash,
  APipelineHash: Cardinal);
begin
  if ASeed <> 0 then
    Exit;
  Require(ACanonicalBytes = EXPECTED_CANONICAL_BYTES,
    'seed-zero canonical model size changed');
  Require(AModelHash = EXPECTED_MODEL_HASH,
    'seed-zero canonical model hash changed');
  Require(ALatentHash = EXPECTED_LATENT_HASH,
    'seed-zero latent assignment hash changed');
  Require(ATerrainHash = EXPECTED_TERRAIN_HASH,
    'seed-zero terrain hash changed');
  Require(AFoliageHash = EXPECTED_FOLIAGE_HASH,
    'seed-zero foliage hash changed');
  Require(AStructureHash = EXPECTED_STRUCTURE_HASH,
    'seed-zero structure hash changed');
  Require(APipelineHash = EXPECTED_PIPELINE_HASH,
    'seed-zero pipeline hash changed');
end;

procedure RunLearnedPatternWorldDemo(const ASeed: TGraphSeed);
var
  LAfterFailure: TWorldSnapshot;
  LAfterRecovery: TWorldSnapshot;
  LBaseline: TWorldSnapshot;
  LCanonical: String;
  LControl: TLearnedPatternWorldGraph;
  LControlBaseline: TWorldSnapshot;
  LControlRecoveryReport: TGraphSolveReport;
  LControlSolveReport: TGraphSolveReport;
  LCorpus: TWfcLearnSamples;
  LFailureReport: TGraphSolveReport;
  LFoliageHash: Cardinal;
  LGraph: TLearnedPatternWorldGraph;
  LGrid: TWfcPatternGrid2D;
  LLatentHash: Cardinal;
  LLearned: TWfcOverlappingModel2D;
  LModel: TWfcOverlappingModel2D;
  LModelHash: Cardinal;
  LOptions: TGraphSolveOptions;
  LPipelineHash: Cardinal;
  LRecoveryReport: TGraphSolveReport;
  LSolveReport: TGraphSolveReport;
  LStructureHash: Cardinal;
  LTerrainHash: Cardinal;
  LValidation: TWorldValidationReport;
  X: Integer;
  Y: Integer;
begin
  LControl := nil;
  LGraph := nil;
  LLearned := nil;
  LModel := nil;
  LCorpus := BuildTrainingCorpus;
  try
    LLearned := LearnOverlappingModel2DCorpus(LCorpus,
      PATTERN_WIDTH, PATTERN_HEIGHT, wmbWrap, wmsD4);
    Require(LLearned.PatternCount = EXPECTED_PATTERN_COUNT,
      'the runtime corpus no longer learns 17 nontrivial patterns');
    LCanonical := EncodeWfcPattern2DText(LLearned);
    LModel := DecodeWfcPattern2DText(LCanonical);
    Require(EncodeWfcPattern2DText(LModel) = LCanonical,
      'wfcp=1 replay changed the learned model bytes');

    LGraph := CreateWorldGraph(LModel, ASeed);
    LControl := CreateWorldGraph(LModel, ASeed);
    LOptions := DefaultGraphSolveOptions;
    LOptions.MaxBacktracks := 65536;
    Require(LGraph.TrySolve(LOptions, LSolveReport),
      'the four-pass learned-pattern world did not solve');
    Require(LControl.TrySolve(LOptions, LControlSolveReport),
      'the independent same-seed control did not solve');
    Require(ExecutionOrderEquals(LSolveReport, [0, 1, 2, 3]),
      'the four-pass execution order changed');
    Require((LSolveReport.Passes[0].Disposition = gpdSolved) and
        (LSolveReport.Passes[1].Disposition = gpdSolved) and
        (LSolveReport.Passes[2].Disposition = gpdSolved) and
        (LSolveReport.Passes[3].Disposition = gpdSolved),
      'the baseline report did not solve all four passes');
    Require(ValidateLearnedPatternWorld(LGraph, LModel, LValidation),
      'independent baseline validation failed');
    Require(LGraph.PassGraph[1].Entry[OUTPUT_WIDTH div 2,
        OUTPUT_HEIGHT div 2, 0].Value = TERRAIN_ROCK,
      'the public terrain did not preserve the latent rock anchor');

    CaptureProjectionForSignature(LGraph, LModel, LGrid);
    LModelHash := CanonicalModelHash(LCanonical);
    LLatentHash := LatentAssignmentHash(LGrid);
    LTerrainHash := PublicLayerHash(LGraph, 1, PASS_TERRAIN);
    LFoliageHash := PublicLayerHash(LGraph, 2, PASS_FOLIAGE);
    LStructureHash := PublicLayerHash(LGraph, 3, PASS_STRUCTURE);
    LPipelineHash := PipelineHash(LModelHash, LLatentHash,
      LTerrainHash, LFoliageHash, LStructureHash);

    LBaseline := CaptureWorldSnapshot(LGraph);
    LControlBaseline := CaptureWorldSnapshot(LControl);
    Require(WorldSnapshotsEqual(LBaseline, LControlBaseline),
      'the independent same-seed control changed baseline entry state');
    X := OUTPUT_WIDTH div 2;
    Y := OUTPUT_HEIGHT div 2;
    LGraph.PassGraph[3].SetAllowedValues(X, Y, 0,
      STRUCTURE_DOCK);
    Require(not LGraph.TryRegenerateFrom(PASS_STRUCTURE,
        LOptions, LFailureReport),
      'an incompatible dock unexpectedly solved on projected rock');
    Require((LFailureReport.Status = gssContradiction) and
        (LFailureReport.FailedPassIndex = 3) and
        (LFailureReport.Contradiction.Kind = gckPassDependency) and
        (LFailureReport.Contradiction.DependencyPassIndex = 1) and
        (LFailureReport.Contradiction.EntryIndex =
          Y * OUTPUT_WIDTH + X),
      'the deliberate contradiction did not identify terrain as its cause');
    Require(ExecutionOrderEquals(LFailureReport, [3]),
      'the failed selective run reopened more than structure');
    Require((LFailureReport.Passes[0].Disposition = gpdReused) and
        (LFailureReport.Passes[1].Disposition = gpdReused) and
        (LFailureReport.Passes[2].Disposition = gpdReused) and
        (LFailureReport.Passes[3].Disposition = gpdFailed),
      'the failed selective report changed its active horizon');
    LAfterFailure := CaptureWorldSnapshot(LGraph);
    Require(WorldSnapshotsEqual(LBaseline, LAfterFailure),
      'the deliberate contradiction changed committed entry state');
    Require(WorldSnapshotsEqual(LAfterFailure,
        CaptureWorldSnapshot(LControl)),
      'the failed run diverged from its untouched same-seed control');
    Require(RandomStreamsMatch(LGraph, LControl),
      'the failed run did not restore every pass random stream');

    LGraph.PassGraph[3].ClearAllowedValues(X, Y, 0);
    Require(LGraph.TryRegenerateFrom(PASS_STRUCTURE,
        LOptions, LRecoveryReport),
      'structure did not recover after clearing the contradiction');
    Require(LControl.TryRegenerateFrom(PASS_STRUCTURE,
        LOptions, LControlRecoveryReport),
      'the same-seed control could not replay structure recovery');
    Require(ExecutionOrderEquals(LRecoveryReport, [3]),
      'recovery reopened more than structure');
    Require((LRecoveryReport.Passes[0].Disposition = gpdReused) and
        (LRecoveryReport.Passes[1].Disposition = gpdReused) and
        (LRecoveryReport.Passes[2].Disposition = gpdReused) and
        (LRecoveryReport.Passes[3].Disposition = gpdSolved),
      'recovery did not preserve its clean provider passes');
    Require(ValidateLearnedPatternWorld(LGraph, LModel, LValidation),
      'independent recovery validation failed');
    LAfterRecovery := CaptureWorldSnapshot(LGraph);
    Require(WorldSnapshotsEqual(LBaseline, LAfterRecovery),
      'clear-and-recover did not replay the exact committed world');
    Require(WorldSnapshotsEqual(LAfterRecovery,
        CaptureWorldSnapshot(LControl)),
      'recovery diverged from the independent same-seed control');
    Require(RandomStreamsMatch(LGraph, LControl),
      'recovery random streams diverged from the same-seed control');
    Require(PublicLayerHash(LGraph, 1, PASS_TERRAIN) = LTerrainHash,
      'recovery changed the public terrain projection');
    Require(PublicLayerHash(LGraph, 2, PASS_FOLIAGE) = LFoliageHash,
      'recovery changed clean foliage');
    Require(PublicLayerHash(LGraph, 3, PASS_STRUCTURE) = LStructureHash,
      'recovery changed deterministic structure');

    RequireSeedZeroGoldens(ASeed, Length(LCanonical), LModelHash,
      LLatentHash, LTerrainHash, LFoliageHash, LStructureHash,
      LPipelineHash);

    WriteLn('LearnedPatternWorld: patterns -> terrain -> foliage + structure');
    WriteLn('Seed: ', ASeed);
    WriteLn('Pattern graph adapter version: ',
      WFC_PATTERN_2D_GRAPH_ADAPTER_VERSION);
    WriteLn('Runtime corpus: samples=2 footprint=2x2 boundary=wrap symmetry=d4');
    WriteLn('Learned patterns: ', LModel.PatternCount,
      ' canonical-bytes=', Length(LCanonical),
      ' model-hash=', CardinalHex(LModelHash));
    WriteLn('Execution order: [0,1,2,3]');
    WriteLn('Latent assignment hash: ', CardinalHex(LLatentHash));
    WriteLn('Terrain hash: ', CardinalHex(LTerrainHash));
    WriteLn('Foliage hash: ', CardinalHex(LFoliageHash));
    WriteLn('Structure hash: ', CardinalHex(LStructureHash));
    WriteLn('Pipeline hash: ', CardinalHex(LPipelineHash));
    WriteLn('Independent validation: patterns=',
      LValidation.CheckedPatterns, ' overlaps=',
      LValidation.CheckedRelations, ' contributions=',
      LValidation.CheckedProjectionCells, ' public-cells=',
      LValidation.CheckedPublicCells, ' consumer-relations=',
      LValidation.CheckedConsumerRelations);
    WriteLn('Private pattern keys in public output: ',
      LValidation.PrivateKeyLeaks);
    WriteLn('Deliberate contradiction: pass=3 provider=1 entry=',
      LFailureReport.Contradiction.EntryIndex,
      ' committed-state=rolled-back');
    WriteLn('Rollback replay: all-entry-state=exact all-pass-rng=exact');
    WriteLn('Recovery: requested=[3] active=[3] providers=[0,1,2] reused');
    WriteLn('Self-check: passed');
    WriteLn;
    RenderWorld(LGraph);
    WriteLn;
    WriteLn('terrain: ~=water .=ground #=rock');
    WriteLn('foliage: r=reeds g=grass T=tree m=moss');
    WriteLn('structure: D=dock H=hut M=mine');
  finally
    LControl.Free;
    LGraph.Free;
    LModel.Free;
    LLearned.Free;
  end;
end;

end.
