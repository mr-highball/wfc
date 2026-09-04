program wfc_world2d_settlement_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_world2d,
  wfc_world2d_settlement,
  wfc_world2d_settlement_validate;

const
  TEST_WIDTH = 20;
  TEST_HEIGHT = 12;
  TEST_SEED = TGraphSeed($13579BDF);
  EXPECTED_SIGNATURE =
    '1:656F6ADE:BA744A7F:007C805A:BA15604C:2D20A91F:0D173438';

type
  TTestProcedure = procedure;

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GCheckCount);
  if ACondition then
    WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailureCount);
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
      Inc(GFailureCount);
      WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function TestOptions: TGraphSolveOptions;
begin
  Result := DefaultGraphSolveOptions;
  Result.MaxBacktracks := 16384;
end;

procedure ApplyAnchors(const AWorld: TSettlement2D);
begin
  AWorld
    .Lock(s2lTerrain, 0, 0, WFC_WORLD2D_TERRAIN_WATER)
    .Lock(s2lHydrology, 0, 0, WFC_SETTLEMENT2D_HYDROLOGY_SEA)
    .Lock(s2lBiome, 0, 0, WFC_WORLD2D_BIOME_OCEAN)
    .Lock(s2lRoads, 0, 0, WFC_SETTLEMENT2D_ROADS_NONE)
    .Lock(s2lHousing, 0, 0, WFC_SETTLEMENT2D_HOUSING_NONE)
    .Lock(s2lFoliage, 0, 0, WFC_WORLD2D_FOLIAGE_NONE)

    .Lock(s2lTerrain, TEST_WIDTH div 2, TEST_HEIGHT div 2,
      WFC_WORLD2D_TERRAIN_LAND)
    .Lock(s2lHydrology, TEST_WIDTH div 2, TEST_HEIGHT div 2,
      WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .Lock(s2lBiome, TEST_WIDTH div 2, TEST_HEIGHT div 2,
      WFC_WORLD2D_BIOME_PLAINS)
    .Lock(s2lRoads, TEST_WIDTH div 2, TEST_HEIGHT div 2,
      WFC_SETTLEMENT2D_ROADS_TRAIL)
    .Lock(s2lHousing, TEST_WIDTH div 2, TEST_HEIGHT div 2,
      WFC_SETTLEMENT2D_HOUSING_HOUSE)
    .Lock(s2lFoliage, TEST_WIDTH div 2, TEST_HEIGHT div 2,
      WFC_WORLD2D_FOLIAGE_NONE)

    .Lock(s2lTerrain, TEST_WIDTH - 1, TEST_HEIGHT - 1,
      WFC_WORLD2D_TERRAIN_MOUNTAIN)
    .Lock(s2lHydrology, TEST_WIDTH - 1, TEST_HEIGHT - 1,
      WFC_SETTLEMENT2D_HYDROLOGY_DRY)
    .Lock(s2lBiome, TEST_WIDTH - 1, TEST_HEIGHT - 1,
      WFC_WORLD2D_BIOME_ALPINE)
    .Lock(s2lRoads, TEST_WIDTH - 1, TEST_HEIGHT - 1,
      WFC_SETTLEMENT2D_ROADS_TUNNEL)
    .Lock(s2lHousing, TEST_WIDTH - 1, TEST_HEIGHT - 1,
      WFC_SETTLEMENT2D_HOUSING_LODGE)
    .Lock(s2lFoliage, TEST_WIDTH - 1, TEST_HEIGHT - 1,
      WFC_WORLD2D_FOLIAGE_NONE);
end;

function NewAnchoredWorld(const ASeed: TGraphSeed): TSettlement2D;
var
  LConfig: TSettlement2DConfig;
begin
  LConfig := DefaultSettlement2DConfig;
  LConfig.Seed := ASeed;
  Result := TSettlement2D.Create(TEST_WIDTH, TEST_HEIGHT, LConfig);
  try
    ApplyAnchors(Result);
  except
    Result.Free;
    raise;
  end;
end;

function ExpectedDependencyCount(
  const ALayer: TSettlement2DLayer): Integer;
begin
  Result := -1;
  case ALayer of
    s2lTerrain: Result := 0;
    s2lHydrology, s2lBiome: Result := 1;
    s2lRoads: Result := 3;
    s2lHousing: Result := 4;
    s2lFoliage: Result := 5;
  end;
end;

function ExpectedDependencyIndex(const ALayer: TSettlement2DLayer;
  const AOrdinal: Integer): Integer;
begin
  Result := -1;
  case ALayer of
    s2lTerrain:
      Result := -1;
    s2lHydrology, s2lBiome:
      if AOrdinal = 0 then Result := Ord(s2lTerrain);
    s2lRoads:
      case AOrdinal of
        0: Result := Ord(s2lTerrain);
        1: Result := Ord(s2lHydrology);
        2: Result := Ord(s2lBiome);
      end;
    s2lHousing:
      case AOrdinal of
        0: Result := Ord(s2lTerrain);
        1: Result := Ord(s2lHydrology);
        2: Result := Ord(s2lBiome);
        3: Result := Ord(s2lRoads);
      end;
    s2lFoliage:
      case AOrdinal of
        0: Result := Ord(s2lTerrain);
        1: Result := Ord(s2lHydrology);
        2: Result := Ord(s2lBiome);
        3: Result := Ord(s2lRoads);
        4: Result := Ord(s2lHousing);
      end;
  end;
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

function RawSnapshot(const AWorld: TSettlement2D): String;
var
  LEntry: TGraphEntry;
  LLayer: TSettlement2DLayer;
  X, Y: Integer;
begin
  Result := '';
  for LLayer := Low(TSettlement2DLayer) to High(TSettlement2DLayer) do
  begin
    Result := Result + IntToStr(Ord(LLayer)) + ':';
    for Y := 0 to Integer(AWorld.Height) - 1 do
      for X := 0 to Integer(AWorld.Width) - 1 do
      begin
        LEntry := AWorld.LayerGraph[LLayer].Entry[X, Y, 0];
        if LEntry.Empty then
          Result := Result + 'E'
        else if LEntry.Generated then
          Result := Result + 'G'
        else
          Result := Result + 'L';
        Result := Result + IntToStr(Length(LEntry.Value))
          + ':' + LEntry.Value + ';';
      end;
    Result := Result + '|';
  end;
end;

function LayerRawSnapshot(const AWorld: TSettlement2D;
  const ALayer: TSettlement2DLayer): String;
var
  LEntry: TGraphEntry;
  X, Y: Integer;
begin
  Result := '';
  for Y := 0 to Integer(AWorld.Height) - 1 do
    for X := 0 to Integer(AWorld.Width) - 1 do
    begin
      LEntry := AWorld.LayerGraph[ALayer].Entry[X, Y, 0];
      if LEntry.Empty then Result := Result + 'E'
      else if LEntry.Generated then Result := Result + 'G'
      else Result := Result + 'L';
      Result := Result + IntToStr(Length(LEntry.Value))
        + ':' + LEntry.Value + ';';
    end;
end;

function IsHydrologyClosure(const AReport: TGraphSolveReport): Boolean;
const
  EXPECTED: array[0..3] of Integer = (
    Ord(s2lHydrology), Ord(s2lRoads), Ord(s2lHousing), Ord(s2lFoliage)
  );
var
  I: Integer;
begin
  if Length(AReport.ExecutionOrder) <> Length(EXPECTED) then
    Exit(False);
  for I := 0 to High(EXPECTED) do
    if (AReport.ExecutionOrder[I] <> EXPECTED[I])
      or (not AReport.Passes[EXPECTED[I]].Executed)
      or (AReport.Passes[EXPECTED[I]].ExecutionOrdinal <> I)
      or (AReport.Passes[EXPECTED[I]].Disposition <> gpdSolved) then
      Exit(False);
  Result := (not AReport.Passes[Ord(s2lTerrain)].Executed)
    and (AReport.Passes[Ord(s2lTerrain)].Disposition = gpdReused)
    and (not AReport.Passes[Ord(s2lBiome)].Executed)
    and (AReport.Passes[Ord(s2lBiome)].Disposition = gpdReused);
end;

procedure TestConstructionAndTopology;
var
  I: Integer;
  LConfig: TSettlement2DConfig;
  LGraph: TGraph;
  LLayer: TSettlement2DLayer;
  LRaised: Boolean;
  LWorld: TSettlement2D;
begin
  LConfig := DefaultSettlement2DConfig;
  Check((LConfig.Seed = 0) and (not LConfig.WrapNeighbors),
    'the default settlement configuration is deterministic and bounded');
  LWorld := TSettlement2D.Create(7, 5, LConfig);
  try
    Check((LWorld.Width = 7) and (LWorld.Height = 5)
      and (LWorld.Graph.TotalPassCount = 6),
      'the wrapper creates the complete six-layer shape');
    for LLayer := Low(TSettlement2DLayer) to High(TSettlement2DLayer) do
    begin
      LGraph := LWorld.LayerGraph[LLayer];
      Check((LGraph.CurrentPassIndex = Ord(LLayer))
        and (LGraph.CurrentPass = Settlement2DLayerName(LLayer))
        and (LGraph.PassMode = gpmOverlay),
        Settlement2DLayerName(LLayer) + ' has stable overlay identity');
      Check(LGraph.DependencyCount = ExpectedDependencyCount(LLayer),
        Settlement2DLayerName(LLayer) + ' has its exact dependency count');
      for I := 0 to LGraph.DependencyCount - 1 do
        Check(LGraph.DependencyIndex[I]
          = ExpectedDependencyIndex(LLayer, I),
          Settlement2DLayerName(LLayer)
          + ' dependency order is canonical');
    end;

    LRaised := False;
    try
      LWorld.Lock(s2lHydrology, 0, 0, WFC_WORLD2D_BIOME_OCEAN);
    except
      on E: ESettlement2D do LRaised := True;
    end;
    Check(LRaised, 'typed locking rejects another layer value');

    LRaised := False;
    try
      LWorld.Lock(s2lTerrain, 7, 0, WFC_WORLD2D_TERRAIN_LAND);
    except
      on E: ERangeError do LRaised := True;
    end;
    Check(LRaised, 'typed locking rejects an out-of-range coordinate');
  finally
    LWorld.Free;
  end;

  LRaised := False;
  try
    LWorld := TSettlement2D.Create(0, 1);
    LWorld.Free;
  except
    on E: ERangeError do LRaised := True;
  end;
  Check(LRaised, 'a zero-width settlement is rejected');
end;

procedure TestReplayAndValidation;
var
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LSignature: String;
  LValidation: TSettlement2DValidationReport;
  LDifferent: TSettlement2D;
  LTwin: TSettlement2D;
  LWorld: TSettlement2D;
begin
  LOptions := TestOptions;
  LWorld := NewAnchoredWorld(TEST_SEED);
  LTwin := NewAnchoredWorld(TEST_SEED);
  LDifferent := NewAnchoredWorld(TEST_SEED + 1);
  try
    Check(LWorld.TryGenerate(LOptions, LReport),
      'the anchored settlement DAG solves');
    Check((LReport.Status = gssSolved)
      and (Length(LReport.ExecutionOrder) = 6),
      'the full report records every topologically executed pass');
    Check(ValidateSettlement2D(LWorld, LValidation),
      'the independent validator accepts the complete settlement');
    Check(LValidation.CheckedCells = TEST_WIDTH * TEST_HEIGHT * 6,
      'validation checks every cell in every layer');
    Check(LValidation.CheckedRelations > LValidation.CheckedCells,
      'validation independently checks cross-layer and adjacency relations');
    LSignature := LWorld.PipelineSignature;
    WriteLn('  [INFO] canonical signature: ', LSignature);
    Check(LSignature = EXPECTED_SIGNATURE,
      'the canonical settlement fixture matches its golden signature');
    Check(LWorld.TryGenerate(LOptions, LReport)
      and (LWorld.PipelineSignature = LSignature),
      'same-instance full generation replays exactly');
    Check(LTwin.TryGenerate(LOptions, LReport)
      and (LTwin.PipelineSignature = LSignature),
      'an independent settlement replays exactly');
    Check(LDifferent.TryGenerate(LOptions, LReport)
      and (LDifferent.PipelineSignature <> LSignature),
      'a different seed changes unlocked output');
    Check((not LWorld.LayerGraph[s2lTerrain].Entry[0, 0, 0].Generated)
      and LWorld.LayerGraph[s2lTerrain].Entry[1, 0, 0].Generated,
      'caller locks and generated output retain distinct ownership');
  finally
    LDifferent.Free;
    LTwin.Free;
    LWorld.Free;
  end;
end;

procedure TestSelectiveEditAndRecovery;
var
  LBaseline: String;
  LBiomeRaw: String;
  LEditX: Integer;
  LEditY: Integer;
  LEdited: String;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LTerrainRaw: String;
  LValidation: TSettlement2DValidationReport;
  LTwin: TSettlement2D;
  LTwinX: Integer;
  LTwinY: Integer;
  LWorld: TSettlement2D;
begin
  LOptions := TestOptions;
  LWorld := NewAnchoredWorld(TEST_SEED);
  LTwin := NewAnchoredWorld(TEST_SEED);
  try
    Check(LWorld.TryGenerate(LOptions, LReport),
      'the selective fixture establishes a baseline');
    LBaseline := LWorld.PipelineSignature;
    Check(FindEditCandidate(LWorld, LEditX, LEditY),
      'the deterministic fixture contains a generated house/cabin');
    if (LEditX < 0) or (LEditY < 0) then Exit;
    LTerrainRaw := LayerRawSnapshot(LWorld, s2lTerrain);
    LBiomeRaw := LayerRawSnapshot(LWorld, s2lBiome);

    LWorld.Graph.SwitchToPass(WFC_SETTLEMENT2D_PASS_BIOME);
    LWorld.Lock(s2lHydrology, LEditX, LEditY,
      WFC_SETTLEMENT2D_HYDROLOGY_RIVER);
    Check(LWorld.TryRegenerateFrom(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      LOptions, LReport), 'hydrology-rooted selective regeneration solves');
    Check(IsHydrologyClosure(LReport),
      'selective execution is the non-contiguous transitive closure');
    Check(LWorld.Graph.CurrentPassIndex = Ord(s2lBiome),
      'selective generation restores caller pass selection');
    Check((LayerRawSnapshot(LWorld, s2lTerrain) = LTerrainRaw)
      and (LayerRawSnapshot(LWorld, s2lBiome) = LBiomeRaw),
      'terrain and sibling biome are preserved byte-for-byte');
    Check(LWorld.Value[s2lHousing, LEditX, LEditY]
      = WFC_SETTLEMENT2D_HOUSING_NONE,
      'a river counterfactual removes its former house/cabin');
    Check(ValidateSettlement2D(LWorld, LValidation),
      'the selectively regenerated result validates independently');
    LEdited := LWorld.PipelineSignature;
    Check(LEdited <> LBaseline,
      'the counterfactual changes the complete replay signature');

    LWorld.ClearLock(s2lHydrology, LEditX, LEditY);
    Check(LWorld.TryRegenerateFrom(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      LOptions, LReport) and (LWorld.PipelineSignature = LBaseline),
      'clearing the root edit recovers the exact baseline');

    Check(LTwin.TryGenerate(LOptions, LReport)
      and FindEditCandidate(LTwin, LTwinX, LTwinY),
      'an independent world finds its deterministic edit candidate');
    Check((LTwinX = LEditX) and (LTwinY = LEditY),
      'the row-major edit coordinate replays');
    LTwin.Lock(s2lHydrology, LTwinX, LTwinY,
      WFC_SETTLEMENT2D_HYDROLOGY_RIVER);
    Check(LTwin.TryRegenerateFrom(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      LOptions, LReport) and (LTwin.PipelineSignature = LEdited),
      'an independent counterfactual reproduces the edited signature');
  finally
    LTwin.Free;
    LWorld.Free;
  end;
end;

procedure TestSelectiveRollback;
var
  LBaseline: String;
  LEditX: Integer;
  LEditY: Integer;
  LEdited: String;
  LInvalid: String;
  LOptions: TGraphSolveOptions;
  LRaw: String;
  LReport: TGraphSolveReport;
  LWorld: TSettlement2D;
begin
  LOptions := TestOptions;
  LWorld := NewAnchoredWorld(TEST_SEED);
  try
    Check(LWorld.TryGenerate(LOptions, LReport)
      and FindEditCandidate(LWorld, LEditX, LEditY),
      'the rollback fixture establishes its candidate');
    if (LEditX < 0) or (LEditY < 0) then Exit;
    LBaseline := LWorld.PipelineSignature;
    LWorld.Lock(s2lHydrology, LEditX, LEditY,
      WFC_SETTLEMENT2D_HYDROLOGY_RIVER);
    Check(LWorld.TryRegenerateFrom(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      LOptions, LReport), 'the rollback fixture establishes edited output');
    LEdited := LWorld.PipelineSignature;

    LWorld.Lock(s2lHousing, LEditX, LEditY,
      WFC_SETTLEMENT2D_HOUSING_HOUSE);
    LInvalid := LWorld.PipelineSignature;
    LRaw := RawSnapshot(LWorld);
    Check(not LWorld.TryRegenerateFrom(WFC_SETTLEMENT2D_PASS_HOUSING,
      LOptions, LReport), 'an impossible river house is rejected');
    Check((LReport.FailedPassIndex = Ord(s2lHousing))
      and (LReport.Contradiction.Kind = gckPassDependency)
      and (LReport.Contradiction.DependencyPassIndex = Ord(s2lHydrology)),
      'failure names the housing pass and hydrology dependency');
    Check((Length(LReport.ExecutionOrder) = 1)
      and (LReport.ExecutionOrder[0] = Ord(s2lHousing))
      and LReport.Passes[Ord(s2lHousing)].Executed
      and (LReport.Passes[Ord(s2lHousing)].Disposition = gpdFailed),
      'the failed execution report stops at housing');
    Check((RawSnapshot(LWorld) = LRaw)
      and (LWorld.PipelineSignature = LInvalid),
      'failed selective generation is fully atomic');

    LWorld.ClearLock(s2lHousing, LEditX, LEditY);
    Check(LWorld.TryRegenerateFrom(WFC_SETTLEMENT2D_PASS_HOUSING,
      LOptions, LReport) and (LWorld.PipelineSignature = LEdited),
      'clearing the bad descendant recovers the edited state');
    LWorld.ClearLock(s2lHydrology, LEditX, LEditY);
    Check(LWorld.TryRegenerateFrom(WFC_SETTLEMENT2D_PASS_HYDROLOGY,
      LOptions, LReport) and (LWorld.PipelineSignature = LBaseline),
      'clearing the root counterfactual recovers the baseline');
  finally
    LWorld.Free;
  end;
end;

procedure TestIndependentCorruptionChecks;
var
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRaised: Boolean;
  LValidation: TSettlement2DValidationReport;
  LWorld: TSettlement2D;
begin
  LOptions := TestOptions;
  LWorld := NewAnchoredWorld(TEST_SEED);
  try
    Check(LWorld.TryGenerate(LOptions, LReport),
      'the corruption fixture establishes valid output');
    LWorld.LayerGraph[s2lRoads].Entry[0, 0, 0].Value :=
      WFC_SETTLEMENT2D_ROADS_BRIDGE;
    Check((not ValidateSettlement2D(LWorld, LValidation))
      and (LValidation.Issue.Kind = s2vikRoadContext)
      and LValidation.Issue.HasRelatedLayer,
      'validation catches direct cross-layer road corruption');
  finally
    LWorld.Free;
  end;

  LWorld := NewAnchoredWorld(TEST_SEED);
  try
    Check(LWorld.TryGenerate(LOptions, LReport),
      'the topology fixture establishes valid output');
    LWorld.LayerGraph[s2lBiome]
      .DependsOn(WFC_SETTLEMENT2D_PASS_HYDROLOGY);
    Check((not ValidateSettlement2D(LWorld, LValidation))
      and (LValidation.Issue.Kind = s2vikPipelineTopology)
      and (LValidation.Issue.Layer = s2lBiome),
      'validation rejects a mutated dependency DAG');
    LRaised := False;
    try
      LWorld.PipelineSignature;
    except
      on E: ESettlement2D do LRaised := True;
    end;
    Check(LRaised, 'signatures reject a noncanonical dependency DAG');
  finally
    LWorld.Free;
  end;
end;

procedure TestWrappedReplay;
var
  LConfig: TSettlement2DConfig;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LSignature: String;
  LValidation: TSettlement2DValidationReport;
  LWorld: TSettlement2D;
begin
  LConfig := DefaultSettlement2DConfig;
  LConfig.Seed := 2468;
  LConfig.WrapNeighbors := True;
  LOptions := TestOptions;
  LWorld := TSettlement2D.Create(7, 5, LConfig);
  try
    Check(LWorld.WrapNeighbors
      and (LWorld.LayerGraph[s2lTerrain].Entry[0, 0, 0][gdWest]
        = LWorld.LayerGraph[s2lTerrain].Entry[6, 0, 0]),
      'every settlement overlay shares the wrapped topology');
    Check(LWorld.TryGenerate(LOptions, LReport),
      'a wrapped settlement DAG solves');
    Check(ValidateSettlement2D(LWorld, LValidation),
      'independent validation checks wrapped output');
    LSignature := LWorld.PipelineSignature;
    Check(LWorld.TryGenerate(LOptions, LReport)
      and (LWorld.PipelineSignature = LSignature),
      'wrapped full generation replays exactly');
  finally
    LWorld.Free;
  end;
end;

begin
  WriteLn('WFC settlement DAG suite');
  WriteLn('========================');
  RunTest('construction and exact topology', @TestConstructionAndTopology);
  RunTest('replay and independent validation', @TestReplayAndValidation);
  RunTest('selective edit and exact recovery', @TestSelectiveEditAndRecovery);
  RunTest('selective rollback', @TestSelectiveRollback);
  RunTest('independent corruption checks', @TestIndependentCorruptionChecks);
  RunTest('wrapped replay', @TestWrappedReplay);
  WriteLn('========================');
  WriteLn(Format('%d checks, %d failures', [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d settlement DAG checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
