program MultiPassWorld;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_world2d,
  wfc_world2d_validate,
  world2d_showcase;

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

var
  LConfig: TWorld2DConfig;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LSignature: String;
  LValidation: TWorld2DValidationReport;
  LWorld: TWorld2D;
begin
  LConfig := DefaultWorld2DConfig;
  LConfig.Seed := WFC_WORLD2D_SHOWCASE_DEFAULT_SEED;
  if ParamCount > 0 then
    LConfig.Seed := TGraphSeed(StrToQWord(ParamStr(1)));

  LWorld := TWorld2D.Create(WFC_WORLD2D_SHOWCASE_WIDTH,
    WFC_WORLD2D_SHOWCASE_HEIGHT, LConfig);
  try
    ApplyWorld2DShowcaseAnchors(LWorld);
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
    VerifyWorld2DShowcaseSignature(LWorld.Seed, LSignature);

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
