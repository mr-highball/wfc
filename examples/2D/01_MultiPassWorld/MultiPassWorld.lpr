program MultiPassWorld;

{$mode delphi}{$H+}

uses
  SysUtils,
  {$IFDEF PAS2JS}
  NodeJSApp,
  {$ENDIF}
  wfc,
  wfc_world2d,
  wfc_world2d_validate;

const
  WORLD_WIDTH = 32;
  WORLD_HEIGHT = 14;
  DEFAULT_SEED = TGraphSeed($4D505731);
  EXPECTED_DEFAULT_SIGNATURE = '1:81F03F86:9069C5F9:41619106';
  EXPECTED_SEED_ZERO_SIGNATURE = '1:5B0DD75D:08022AF1:A40D0955';

procedure ApplyShowcaseAnchors(const AWorld: TWorld2D);
var
  LCenterX: TGraphCoordinate;
  LCenterY: TGraphCoordinate;
begin
  LCenterX := AWorld.Width div 2;
  LCenterY := AWorld.Height div 2;
  AWorld
    .Lock(w2lTerrain, 0, 0, WFC_WORLD2D_TERRAIN_WATER)
    .Lock(w2lTerrain, 1, 0, WFC_WORLD2D_TERRAIN_LAND)
    .Lock(w2lTerrain, LCenterX, LCenterY, WFC_WORLD2D_TERRAIN_LAND)
    .Lock(w2lTerrain, LCenterX - 1, LCenterY,
      WFC_WORLD2D_TERRAIN_LAND)
    .Lock(w2lTerrain, LCenterX + 1, LCenterY,
      WFC_WORLD2D_TERRAIN_LAND)
    .Lock(w2lTerrain, LCenterX, LCenterY - 1,
      WFC_WORLD2D_TERRAIN_LAND)
    .Lock(w2lTerrain, LCenterX, LCenterY + 1,
      WFC_WORLD2D_TERRAIN_LAND)
    .Lock(w2lTerrain, AWorld.Width - 1, AWorld.Height - 1,
      WFC_WORLD2D_TERRAIN_MOUNTAIN)
    .Lock(w2lBiome, 0, 0, WFC_WORLD2D_BIOME_OCEAN)
    .Lock(w2lBiome, 1, 0, WFC_WORLD2D_BIOME_SHORE)
    .Lock(w2lBiome, LCenterX, LCenterY, WFC_WORLD2D_BIOME_WOODLAND)
    .Lock(w2lBiome, AWorld.Width - 1, AWorld.Height - 1,
      WFC_WORLD2D_BIOME_ALPINE)
    .Lock(w2lFoliage, 1, 0, WFC_WORLD2D_FOLIAGE_REEDS)
    .Lock(w2lFoliage, LCenterX, LCenterY, WFC_WORLD2D_FOLIAGE_TREE)
    .Lock(w2lFoliage, AWorld.Width - 1, AWorld.Height - 1,
      WFC_WORLD2D_FOLIAGE_PINE);
end;

function GlyphFor(const ALayer: TWorld2DLayer;
  const AValue: TGraphValue): Char;
begin
  case ALayer of
    w2lTerrain:
      if AValue = WFC_WORLD2D_TERRAIN_WATER then
        Result := '~'
      else if AValue = WFC_WORLD2D_TERRAIN_LAND then
        Result := '.'
      else
        Result := '^';
    w2lBiome:
      if AValue = WFC_WORLD2D_BIOME_OCEAN then
        Result := 'O'
      else if AValue = WFC_WORLD2D_BIOME_SHORE then
        Result := 's'
      else if AValue = WFC_WORLD2D_BIOME_PLAINS then
        Result := 'p'
      else if AValue = WFC_WORLD2D_BIOME_WOODLAND then
        Result := 'w'
      else
        Result := 'a';
    w2lFoliage:
      if AValue = WFC_WORLD2D_FOLIAGE_NONE then
        Result := '-'
      else if AValue = WFC_WORLD2D_FOLIAGE_REEDS then
        Result := 'r'
      else if AValue = WFC_WORLD2D_FOLIAGE_GRASS then
        Result := 'g'
      else if AValue = WFC_WORLD2D_FOLIAGE_TREE then
        Result := 'T'
      else
        Result := 'P';
  else
    Result := '?';
  end;
end;

procedure RenderWorld(const AWorld: TWorld2D);
var
  LLayer: TWorld2DLayer;
  X, Y: Integer;
begin
  WriteLn('terrain                          biome                            foliage');
  for Y := 0 to Pred(Integer(AWorld.Height)) do
  begin
    for LLayer := Low(TWorld2DLayer) to High(TWorld2DLayer) do
    begin
      for X := 0 to Pred(Integer(AWorld.Width)) do
        Write(GlyphFor(LLayer, AWorld.Value[LLayer, X, Y]));
      if LLayer <> High(TWorld2DLayer) then
        Write('   ');
    end;
    WriteLn;
  end;
end;

procedure PrintReport(const AReport: TGraphSolveReport);
var
  I: Integer;
begin
  for I := 0 to High(AReport.Passes) do
    WriteLn('pass ', I,
      ': decisions=', AReport.Passes[I].Decisions,
      ' propagations=', AReport.Passes[I].Propagations,
      ' contradictions=', AReport.Passes[I].Contradictions,
      ' backtracks=', AReport.Passes[I].Backtracks);
end;

procedure VerifyShowcaseSignature(const ASeed: TGraphSeed;
  const ASignature: String);
var
  LExpected: String;
begin
  LExpected := '';
  if ASeed = DEFAULT_SEED then
    LExpected := EXPECTED_DEFAULT_SIGNATURE
  else if ASeed = 0 then
    LExpected := EXPECTED_SEED_ZERO_SIGNATURE;
  if (LExpected <> '') and (ASignature <> LExpected) then
    raise EWorld2D.CreateFmt(
      'showcase signature mismatch for seed %s: expected %s, got %s',
      [UIntToStr(ASeed), LExpected, ASignature]);
end;

var
  LConfig: TWorld2DConfig;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LSignature: String;
  LValidation: TWorld2DValidationReport;
  LWorld: TWorld2D;
begin
  LConfig := DefaultWorld2DConfig;
  LConfig.Seed := DEFAULT_SEED;
  if ParamCount > 0 then
    LConfig.Seed := TGraphSeed(StrToQWord(ParamStr(1)));

  LWorld := TWorld2D.Create(WORLD_WIDTH, WORLD_HEIGHT, LConfig);
  try
    ApplyShowcaseAnchors(LWorld);
    LOptions := DefaultGraphSolveOptions;
    LOptions.MaxBacktracks := 8192;
    if not LWorld.TryGenerate(LOptions, LReport) then
      raise EWorld2D.CreateFmt(
        'generation failed in pass %d (constraint %d, entry %d)',
        [LReport.FailedPassIndex, Ord(LReport.Contradiction.Kind),
         LReport.Contradiction.EntryIndex]);
    if not ValidateWorld2D(LWorld, LValidation) then
      raise EWorld2D.Create(
        DescribeWorld2DValidationIssue(LValidation.Issue));
    LSignature := LWorld.PipelineSignature;
    VerifyShowcaseSignature(LWorld.Seed, LSignature);

    WriteLn('Seed: ', LWorld.Seed);
    WriteLn('Model version: ', WFC_WORLD2D_MODEL_VERSION);
    WriteLn('Signature: ', LSignature);
    WriteLn('Validated cells: ', LValidation.CheckedCells,
      '; relations: ', LValidation.CheckedRelations);
    PrintReport(LReport);
    WriteLn;
    RenderWorld(LWorld);
    WriteLn;
    WriteLn('terrain: ~=water .=land ^=mountain');
    WriteLn('biome: O=ocean s=shore p=plains w=woodland a=alpine');
    WriteLn('foliage: -=none r=reeds g=grass T=tree P=pine');
  finally
    LWorld.Free;
  end;
end.
