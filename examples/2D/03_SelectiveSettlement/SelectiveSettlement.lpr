program SelectiveSettlement;

{$mode delphi}{$H+}

uses
  SysUtils,
  {$IFDEF PAS2JS}
  NodeJSApp,
  {$ENDIF}
  wfc,
  wfc_world2d,
  wfc_world2d_settlement,
  wfc_world2d_settlement_validate;

const
  SHOWCASE_WIDTH = 32;
  SHOWCASE_HEIGHT = 14;
  SHOWCASE_DEFAULT_SEED = TGraphSeed($31474144);

  SHOWCASE_DEFAULT_BASELINE_SIGNATURE =
    '1:A988ED54:00717522:F7C8FA35:6E039D1A:6A60EB5E:4C620338';
  SHOWCASE_DEFAULT_EDITED_SIGNATURE =
    '1:A988ED54:B43B6363:F7C8FA35:099DA8D9:15265DF0:2324AC9D';
  SHOWCASE_DEFAULT_EDIT_X = 5;
  SHOWCASE_DEFAULT_EDIT_Y = 4;
  SHOWCASE_SEED_ZERO_BASELINE_SIGNATURE =
    '1:DBE324CA:52FB935C:4F4DC112:AE59EBFD:4814172B:8408D520';
  SHOWCASE_SEED_ZERO_EDITED_SIGNATURE =
    '1:DBE324CA:B34F0FA0:4F4DC112:7423E816:A3FAAAB4:25FE0A34';
  SHOWCASE_SEED_ZERO_EDIT_X = 1;
  SHOWCASE_SEED_ZERO_EDIT_Y = 4;

type
  TLayerValues = array[TSettlement2DLayer] of TGraphValues;

procedure Require(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise ESettlement2D.Create(AMessage);
end;

function SolveOptions: TGraphSolveOptions;
begin
  Result := DefaultGraphSolveOptions;
  Result.MaxBacktracks := 32768;
end;

procedure ApplyShowcaseAnchors(const AWorld: TSettlement2D);
begin
  Require(Assigned(AWorld), 'cannot anchor an unassigned settlement');
  Require((AWorld.Width = SHOWCASE_WIDTH)
    and (AWorld.Height = SHOWCASE_HEIGHT),
    'showcase anchors require a 32 by 14 settlement');

  AWorld
    .Lock(s2lTerrain, 0, 0, WFC_WORLD2D_TERRAIN_WATER)
    .Lock(s2lHydrology, 0, 0, WFC_SETTLEMENT2D_HYDROLOGY_SEA)
    .Lock(s2lBiome, 0, 0, WFC_WORLD2D_BIOME_OCEAN)
    .Lock(s2lRoads, 0, 0, WFC_SETTLEMENT2D_ROADS_NONE)
    .Lock(s2lHousing, 0, 0, WFC_SETTLEMENT2D_HOUSING_NONE)
    .Lock(s2lFoliage, 0, 0, WFC_WORLD2D_FOLIAGE_NONE)

    .Lock(s2lTerrain, 1, 0, WFC_WORLD2D_TERRAIN_LAND)
    .Lock(s2lHydrology, 1, 0, WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .Lock(s2lBiome, 1, 0, WFC_WORLD2D_BIOME_SHORE)
    .Lock(s2lRoads, 1, 0, WFC_SETTLEMENT2D_ROADS_NONE)
    .Lock(s2lHousing, 1, 0, WFC_SETTLEMENT2D_HOUSING_NONE)
    .Lock(s2lFoliage, 1, 0, WFC_WORLD2D_FOLIAGE_NONE)

    .Lock(s2lTerrain, 16, 7, WFC_WORLD2D_TERRAIN_LAND)
    .Lock(s2lHydrology, 16, 7, WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .Lock(s2lBiome, 16, 7, WFC_WORLD2D_BIOME_WOODLAND)
    .Lock(s2lRoads, 16, 7, WFC_SETTLEMENT2D_ROADS_NONE)
    .Lock(s2lHousing, 16, 7, WFC_SETTLEMENT2D_HOUSING_NONE)
    .Lock(s2lFoliage, 16, 7, WFC_WORLD2D_FOLIAGE_TREE)

    .Lock(s2lTerrain, 10, 5, WFC_WORLD2D_TERRAIN_LAND)
    .Lock(s2lHydrology, 10, 5, WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .Lock(s2lBiome, 10, 5, WFC_WORLD2D_BIOME_PLAINS)
    .Lock(s2lRoads, 10, 5, WFC_SETTLEMENT2D_ROADS_TRAIL)
    .Lock(s2lHousing, 10, 5, WFC_SETTLEMENT2D_HOUSING_HOUSE)
    .Lock(s2lFoliage, 10, 5, WFC_WORLD2D_FOLIAGE_NONE)

    .Lock(s2lTerrain, 12, 5, WFC_WORLD2D_TERRAIN_LAND)
    .Lock(s2lHydrology, 12, 5, WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .Lock(s2lBiome, 12, 5, WFC_WORLD2D_BIOME_WOODLAND)
    .Lock(s2lRoads, 12, 5, WFC_SETTLEMENT2D_ROADS_TRAIL)
    .Lock(s2lHousing, 12, 5, WFC_SETTLEMENT2D_HOUSING_CABIN)
    .Lock(s2lFoliage, 12, 5, WFC_WORLD2D_FOLIAGE_NONE)

    .Lock(s2lTerrain, 14, 5, WFC_WORLD2D_TERRAIN_LAND)
    .Lock(s2lHydrology, 14, 5, WFC_SETTLEMENT2D_HYDROLOGY_RIVER)
    .Lock(s2lBiome, 14, 5, WFC_WORLD2D_BIOME_PLAINS)
    .Lock(s2lRoads, 14, 5, WFC_SETTLEMENT2D_ROADS_BRIDGE)
    .Lock(s2lHousing, 14, 5, WFC_SETTLEMENT2D_HOUSING_NONE)
    .Lock(s2lFoliage, 14, 5, WFC_WORLD2D_FOLIAGE_NONE)

    .Lock(s2lTerrain, 16, 5, WFC_WORLD2D_TERRAIN_LAND)
    .Lock(s2lHydrology, 16, 5, WFC_SETTLEMENT2D_HYDROLOGY_RIVER)
    .Lock(s2lBiome, 16, 5, WFC_WORLD2D_BIOME_WOODLAND)
    .Lock(s2lRoads, 16, 5, WFC_SETTLEMENT2D_ROADS_NONE)
    .Lock(s2lHousing, 16, 5, WFC_SETTLEMENT2D_HOUSING_NONE)
    .Lock(s2lFoliage, 16, 5, WFC_WORLD2D_FOLIAGE_REEDS)

    .Lock(s2lTerrain, 30, 12, WFC_WORLD2D_TERRAIN_MOUNTAIN)
    .Lock(s2lHydrology, 30, 12, WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .Lock(s2lBiome, 30, 12, WFC_WORLD2D_BIOME_ALPINE)
    .Lock(s2lRoads, 30, 12, WFC_SETTLEMENT2D_ROADS_TUNNEL)
    .Lock(s2lHousing, 30, 12, WFC_SETTLEMENT2D_HOUSING_LODGE)
    .Lock(s2lFoliage, 30, 12, WFC_WORLD2D_FOLIAGE_NONE)

    .Lock(s2lTerrain, 31, 13, WFC_WORLD2D_TERRAIN_MOUNTAIN)
    .Lock(s2lHydrology, 31, 13, WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .Lock(s2lBiome, 31, 13, WFC_WORLD2D_BIOME_ALPINE)
    .Lock(s2lRoads, 31, 13, WFC_SETTLEMENT2D_ROADS_NONE)
    .Lock(s2lHousing, 31, 13, WFC_SETTLEMENT2D_HOUSING_NONE)
    .Lock(s2lFoliage, 31, 13, WFC_WORLD2D_FOLIAGE_PINE);
end;

function FindEditCandidate(const AWorld: TSettlement2D;
  out AX, AY: Integer): Boolean;
var
  LLayer: TSettlement2DLayer;
  X, Y: Integer;
begin
  for Y := 0 to Integer(AWorld.Height) - 1 do
    for X := 0 to Integer(AWorld.Width) - 1 do
      if (AWorld.Value[s2lTerrain, X, Y] = WFC_WORLD2D_TERRAIN_LAND)
        and (AWorld.Value[s2lHydrology, X, Y]
          = WFC_SETTLEMENT2D_HYDROLOGY_DRY)
        and ((AWorld.Value[s2lBiome, X, Y]
          = WFC_WORLD2D_BIOME_PLAINS)
          or (AWorld.Value[s2lBiome, X, Y]
          = WFC_WORLD2D_BIOME_WOODLAND))
        and (AWorld.Value[s2lRoads, X, Y]
          = WFC_SETTLEMENT2D_ROADS_TRAIL)
        and ((AWorld.Value[s2lHousing, X, Y]
          = WFC_SETTLEMENT2D_HOUSING_HOUSE)
          or (AWorld.Value[s2lHousing, X, Y]
          = WFC_SETTLEMENT2D_HOUSING_CABIN)) then
      begin
        Result := True;
        for LLayer := Low(TSettlement2DLayer)
          to High(TSettlement2DLayer) do
          Result := Result and AWorld.LayerGraph[LLayer]
            .Entry[X, Y, 0].Generated;
        if Result then
        begin
          AX := X;
          AY := Y;
          Exit;
        end;
      end;
  AX := -1;
  AY := -1;
  Result := False;
end;

procedure CaptureLayerValues(const AWorld: TSettlement2D;
  out AValues: TLayerValues);
var
  I: Integer;
  LLayer: TSettlement2DLayer;
  X, Y: Integer;
begin
  for LLayer := Low(TSettlement2DLayer) to High(TSettlement2DLayer) do
  begin
    SetLength(AValues[LLayer], Integer(AWorld.Width * AWorld.Height));
    I := 0;
    for Y := 0 to Integer(AWorld.Height) - 1 do
      for X := 0 to Integer(AWorld.Width) - 1 do
      begin
        AValues[LLayer][I] := AWorld.Value[LLayer, X, Y];
        Inc(I);
      end;
  end;
end;

function CountLayerChanges(const AWorld: TSettlement2D;
  const ALayer: TSettlement2DLayer;
  const ABefore: TGraphValues): Integer;
var
  I: Integer;
  X, Y: Integer;
begin
  Result := 0;
  I := 0;
  for Y := 0 to Integer(AWorld.Height) - 1 do
    for X := 0 to Integer(AWorld.Width) - 1 do
    begin
      if (I > High(ABefore))
        or (AWorld.Value[ALayer, X, Y] <> ABefore[I]) then
        Inc(Result);
      Inc(I);
    end;
end;

function DispositionName(const ADisposition: TGraphPassDisposition): String;
begin
  Result := 'unknown';
  case ADisposition of
    gpdNotRun: Result := 'not-run';
    gpdReused: Result := 'reused';
    gpdCleared: Result := 'cleared';
    gpdCopied: Result := 'copied';
    gpdSolved: Result := 'solved';
    gpdFailed: Result := 'failed';
  end;
end;

procedure VerifyHydrologyClosure(const AReport: TGraphSolveReport);
const
  EXPECTED: array[0..3] of Integer = (
    Ord(s2lHydrology), Ord(s2lRoads), Ord(s2lHousing), Ord(s2lFoliage)
  );
var
  I: Integer;
  LLayer: TSettlement2DLayer;
begin
  Require(Length(AReport.ExecutionOrder) = Length(EXPECTED),
    'hydrology regeneration executed the wrong number of passes');
  for I := 0 to High(EXPECTED) do
    Require(AReport.ExecutionOrder[I] = EXPECTED[I],
      'hydrology regeneration used the wrong topological order');
  for LLayer := Low(TSettlement2DLayer) to High(TSettlement2DLayer) do
    if (LLayer = s2lHydrology) or (LLayer = s2lRoads)
      or (LLayer = s2lHousing) or (LLayer = s2lFoliage) then
      Require(AReport.Passes[Ord(LLayer)].Executed
        and (AReport.Passes[Ord(LLayer)].Disposition = gpdSolved),
        Settlement2DLayerName(LLayer) + ' was not solved')
    else
      Require((not AReport.Passes[Ord(LLayer)].Executed)
        and (AReport.Passes[Ord(LLayer)].Disposition = gpdReused),
        Settlement2DLayerName(LLayer) + ' was not reused');
end;

procedure PrintSelectiveReport(const AWorld: TSettlement2D;
  const AReport: TGraphSolveReport);
var
  I: Integer;
  LIndex: Integer;
  LLayer: TSettlement2DLayer;
begin
  Write('executed:');
  for I := 0 to High(AReport.ExecutionOrder) do
  begin
    LIndex := AReport.ExecutionOrder[I];
    Write(' ', AWorld.Graph.PassGraph[LIndex].CurrentPass);
  end;
  WriteLn;
  Write('reused:');
  for LLayer := Low(TSettlement2DLayer) to High(TSettlement2DLayer) do
    if not AReport.Passes[Ord(LLayer)].Executed then
      Write(' ', Settlement2DLayerName(LLayer));
  WriteLn;
  for I := 0 to High(AReport.ExecutionOrder) do
  begin
    LIndex := AReport.ExecutionOrder[I];
    WriteLn('  ', AWorld.Graph.PassGraph[LIndex].CurrentPass,
      ': ', DispositionName(AReport.Passes[LIndex].Disposition),
      ', decisions=', AReport.Passes[LIndex].Decisions,
      ', propagations=', AReport.Passes[LIndex].Propagations,
      ', contradictions=', AReport.Passes[LIndex].Contradictions,
      ', backtracks=', AReport.Passes[LIndex].Backtracks);
  end;
end;

function GlyphFor(const ALayer: TSettlement2DLayer;
  const AValue: TGraphValue): Char;
begin
  Result := '?';
  case ALayer of
    s2lTerrain:
      if AValue = WFC_WORLD2D_TERRAIN_WATER then Result := '~'
      else if AValue = WFC_WORLD2D_TERRAIN_LAND then Result := '.'
      else Result := '^';
    s2lHydrology:
      if AValue = WFC_SETTLEMENT2D_HYDROLOGY_SEA then Result := 'S'
      else if AValue = WFC_SETTLEMENT2D_HYDROLOGY_DRY then Result := '.'
      else Result := '|';
    s2lBiome:
      if AValue = WFC_WORLD2D_BIOME_OCEAN then Result := 'O'
      else if AValue = WFC_WORLD2D_BIOME_SHORE then Result := 's'
      else if AValue = WFC_WORLD2D_BIOME_PLAINS then Result := 'p'
      else if AValue = WFC_WORLD2D_BIOME_WOODLAND then Result := 'w'
      else Result := 'a';
    s2lRoads:
      if AValue = WFC_SETTLEMENT2D_ROADS_NONE then Result := '-'
      else if AValue = WFC_SETTLEMENT2D_ROADS_TRAIL then Result := '='
      else if AValue = WFC_SETTLEMENT2D_ROADS_BRIDGE then Result := 'B'
      else Result := '#';
    s2lHousing:
      if AValue = WFC_SETTLEMENT2D_HOUSING_NONE then Result := '-'
      else if AValue = WFC_SETTLEMENT2D_HOUSING_HOUSE then Result := 'H'
      else if AValue = WFC_SETTLEMENT2D_HOUSING_CABIN then Result := 'C'
      else Result := 'L';
    s2lFoliage:
      if AValue = WFC_WORLD2D_FOLIAGE_NONE then Result := '-'
      else if AValue = WFC_WORLD2D_FOLIAGE_REEDS then Result := 'r'
      else if AValue = WFC_WORLD2D_FOLIAGE_GRASS then Result := 'g'
      else if AValue = WFC_WORLD2D_FOLIAGE_TREE then Result := 'T'
      else Result := 'P';
  end;
end;

procedure RenderWorld(const AWorld: TSettlement2D);
var
  LLayer: TSettlement2DLayer;
  X, Y: Integer;
begin
  WriteLn('terrain                          hydrology                        biome                            roads                            housing                          foliage');
  for Y := 0 to Integer(AWorld.Height) - 1 do
  begin
    for LLayer := Low(TSettlement2DLayer) to High(TSettlement2DLayer) do
    begin
      for X := 0 to Integer(AWorld.Width) - 1 do
        Write(GlyphFor(LLayer, AWorld.Value[LLayer, X, Y]));
      if LLayer <> High(TSettlement2DLayer) then
        Write('   ');
    end;
    WriteLn;
  end;
end;

function ExpectedBaselineSignature(const ASeed: TGraphSeed): String;
begin
  if ASeed = SHOWCASE_DEFAULT_SEED then
    Result := SHOWCASE_DEFAULT_BASELINE_SIGNATURE
  else if ASeed = 0 then
    Result := SHOWCASE_SEED_ZERO_BASELINE_SIGNATURE
  else
    Result := '';
end;

function ExpectedEditedSignature(const ASeed: TGraphSeed): String;
begin
  if ASeed = SHOWCASE_DEFAULT_SEED then
    Result := SHOWCASE_DEFAULT_EDITED_SIGNATURE
  else if ASeed = 0 then
    Result := SHOWCASE_SEED_ZERO_EDITED_SIGNATURE
  else
    Result := '';
end;

procedure VerifyKnownReplay(const ASeed: TGraphSeed;
  const ABaseline, AEdited: String; const AX, AY: Integer);
var
  LExpected: String;
begin
  LExpected := ExpectedBaselineSignature(ASeed);
  if LExpected <> '' then
    Require(ABaseline = LExpected, 'baseline replay signature drifted');
  LExpected := ExpectedEditedSignature(ASeed);
  if LExpected <> '' then
    Require(AEdited = LExpected, 'edited replay signature drifted');
  if (ASeed = SHOWCASE_DEFAULT_SEED)
    and (SHOWCASE_DEFAULT_EDIT_X >= 0) then
    Require((AX = SHOWCASE_DEFAULT_EDIT_X)
      and (AY = SHOWCASE_DEFAULT_EDIT_Y),
      'default replay edit coordinate drifted')
  else if (ASeed = 0) and (SHOWCASE_SEED_ZERO_EDIT_X >= 0) then
    Require((AX = SHOWCASE_SEED_ZERO_EDIT_X)
      and (AY = SHOWCASE_SEED_ZERO_EDIT_Y),
      'seed-zero replay edit coordinate drifted');
end;

var
  LBaselineSignature: String;
  LBefore: TLayerValues;
  LConfig: TSettlement2DConfig;
  LEditedSignature: String;
  LEditX: Integer;
  LEditY: Integer;
  LInvalidSignature: String;
  LLayer: TSettlement2DLayer;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LTerrainSignature: TSettlement2DSignature;
  LBiomeSignature: TSettlement2DSignature;
  LValidation: TSettlement2DValidationReport;
  LWorld: TSettlement2D;
begin
  LConfig := DefaultSettlement2DConfig;
  LConfig.Seed := SHOWCASE_DEFAULT_SEED;
  if ParamCount > 0 then
    LConfig.Seed := TGraphSeed(StrToQWord(ParamStr(1)));
  LOptions := SolveOptions;

  LWorld := TSettlement2D.Create(SHOWCASE_WIDTH, SHOWCASE_HEIGHT, LConfig);
  try
    ApplyShowcaseAnchors(LWorld);
    Require(LWorld.TryGenerate(LOptions, LReport),
      'the settlement DAG did not solve');
    Require(ValidateSettlement2D(LWorld, LValidation),
      DescribeSettlement2DValidationIssue(LValidation.Issue));
    LBaselineSignature := LWorld.PipelineSignature;
    Require(FindEditCandidate(LWorld, LEditX, LEditY),
      'the replay fixture has no generated house/cabin edit candidate');
    CaptureLayerValues(LWorld, LBefore);
    LTerrainSignature := LWorld.LayerSignature(s2lTerrain);
    LBiomeSignature := LWorld.LayerSignature(s2lBiome);

    WriteLn('Seed: ', LWorld.Seed);
    WriteLn('Model version: ', WFC_SETTLEMENT2D_MODEL_VERSION);
    WriteLn('Baseline signature: ', LBaselineSignature);
    WriteLn('Validated cells: ', LValidation.CheckedCells,
      '; relations: ', LValidation.CheckedRelations);
    WriteLn('Dependency DAG: terrain -> {hydrology, biome} -> roads -> housing -> foliage');
    WriteLn;
    RenderWorld(LWorld);

    LWorld.Lock(s2lHydrology, LEditX, LEditY,
      WFC_SETTLEMENT2D_HYDROLOGY_RIVER);
    Require(LWorld.TryRegenerateFrom(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      LOptions, LReport), 'selective hydrology regeneration failed');
    VerifyHydrologyClosure(LReport);
    Require((LWorld.LayerSignature(s2lTerrain) = LTerrainSignature)
      and (LWorld.LayerSignature(s2lBiome) = LBiomeSignature),
      'selective regeneration changed a sibling or ancestor');
    Require(LWorld.Value[s2lHousing, LEditX, LEditY]
      = WFC_SETTLEMENT2D_HOUSING_NONE,
      'river edit did not invalidate its former house/cabin');
    Require(ValidateSettlement2D(LWorld, LValidation),
      DescribeSettlement2DValidationIssue(LValidation.Issue));
    LEditedSignature := LWorld.PipelineSignature;
    VerifyKnownReplay(LWorld.Seed, LBaselineSignature,
      LEditedSignature, LEditX, LEditY);

    WriteLn;
    WriteLn('Counterfactual edit: hydrology(', LEditX, ',', LEditY,
      ') dry -> river');
    WriteLn('Edited signature: ', LEditedSignature);
    PrintSelectiveReport(LWorld, LReport);
    Write('changed cells:');
    for LLayer := Low(TSettlement2DLayer) to High(TSettlement2DLayer) do
      Write(' ', Settlement2DLayerName(LLayer), '=',
        CountLayerChanges(LWorld, LLayer, LBefore[LLayer]));
    WriteLn;
    WriteLn;
    RenderWorld(LWorld);

    LWorld.ClearLock(s2lHydrology, LEditX, LEditY);
    Require(LWorld.TryRegenerateFrom(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      LOptions, LReport)
      and (LWorld.PipelineSignature = LBaselineSignature),
      'clearing the edit did not recover the baseline exactly');

    { Recreate the edited state and make a descendant caller lock impossible. }
    LWorld.Lock(s2lHydrology, LEditX, LEditY,
      WFC_SETTLEMENT2D_HYDROLOGY_RIVER);
    Require(LWorld.TryRegenerateFrom(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      LOptions, LReport)
      and (LWorld.PipelineSignature = LEditedSignature),
      'the edited state did not replay before rollback testing');
    LWorld.Lock(s2lHousing, LEditX, LEditY,
      WFC_SETTLEMENT2D_HOUSING_HOUSE);
    LInvalidSignature := LWorld.PipelineSignature;
    Require(not LWorld.TryRegenerateFrom(WFC_SETTLEMENT2D_PASS_HOUSING,
      LOptions, LReport), 'an impossible river house unexpectedly solved');
    Require((LReport.FailedPassIndex = Ord(s2lHousing))
      and (LReport.Contradiction.Kind = gckPassDependency)
      and (LReport.Contradiction.DependencyPassIndex = Ord(s2lHydrology)),
      'rollback failure did not identify the hydrology dependency');
    Require(LWorld.PipelineSignature = LInvalidSignature,
      'failed selective regeneration changed committed state');
    LWorld.ClearLock(s2lHousing, LEditX, LEditY);
    Require(LWorld.TryRegenerateFrom(WFC_SETTLEMENT2D_PASS_HOUSING,
      LOptions, LReport)
      and (LWorld.PipelineSignature = LEditedSignature),
      'clearing the bad house did not recover the edited state');
    LWorld.ClearLock(s2lHydrology, LEditX, LEditY);
    Require(LWorld.TryRegenerateFrom(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      LOptions, LReport)
      and (LWorld.PipelineSignature = LBaselineSignature),
      'final cleanup did not recover the baseline');

    WriteLn;
    WriteLn('Rollback: impossible house rejected by ',
      WFC_SETTLEMENT2D_PASS_HYDROLOGY, ' dependency; state preserved');
    WriteLn('Recovery signature: ', LWorld.PipelineSignature);
    WriteLn('terrain: ~=water .=land ^=mountain');
    WriteLn('hydrology: S=sea .=dry |=river');
    WriteLn('biome: O=ocean s=shore p=plains w=woodland a=alpine');
    WriteLn('roads: -=none ==trail B=bridge #=tunnel');
    WriteLn('housing: -=none H=house C=cabin L=lodge');
    WriteLn('foliage: -=none r=reeds g=grass T=tree P=pine');
  finally
    LWorld.Free;
  end;
end.
