program wfc_world2d_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_world2d,
  wfc_world2d_validate;

const
  TEST_WIDTH = 12;
  TEST_HEIGHT = 8;
  TEST_SEED = TGraphSeed($4D505731);
  EXPECTED_SIGNATURE = '1:F7B994F3:E2E8E15B:21B79A66';

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

procedure ApplyAnchors(const AWorld: TWorld2D);
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

function NewAnchoredWorld(const ASeed: TGraphSeed): TWorld2D;
var
  LConfig: TWorld2DConfig;
begin
  LConfig := DefaultWorld2DConfig;
  LConfig.Seed := ASeed;
  Result := TWorld2D.Create(TEST_WIDTH, TEST_HEIGHT, LConfig);
  try
    ApplyAnchors(Result);
  except
    Result.Free;
    raise;
  end;
end;

function WorldOptions: TGraphSolveOptions;
begin
  Result := DefaultGraphSolveOptions;
  Result.MaxBacktracks := 4096;
end;

procedure TestConstructionAndGuards;
var
  LConfig: TWorld2DConfig;
  LRaised: Boolean;
  LWorld: TWorld2D;
begin
  LConfig := DefaultWorld2DConfig;
  Check((LConfig.Seed = 0) and (not LConfig.WrapNeighbors),
    'the default world configuration is deterministic and bounded');

  LWorld := TWorld2D.Create(4, 3);
  try
    Check((LWorld.Width = 4) and (LWorld.Height = 3),
      'the world wrapper preserves its 2D shape');
    Check(LWorld.Graph.TotalPassCount = 3,
      'the standard model creates exactly three passes');
    Check((LWorld.LayerGraph[w2lTerrain].CurrentPassIndex = 0)
      and (LWorld.LayerGraph[w2lBiome].CurrentPassIndex = 1)
      and (LWorld.LayerGraph[w2lFoliage].CurrentPassIndex = 2),
      'typed layers map to stable pass indices');
    Check((LWorld.Seed = 0) and (not LWorld.WrapNeighbors),
      'the default world instance applies its deterministic configuration');

    LRaised := False;
    try
      LWorld.Lock(w2lTerrain, 0, 0, 'not-terrain');
    except
      on E: EWorld2D do
        LRaised := True;
    end;
    Check(LRaised, 'typed locking rejects a value from outside its layer');

    LRaised := False;
    try
      LWorld.Lock(w2lTerrain, 4, 0, WFC_WORLD2D_TERRAIN_LAND);
    except
      on E: ERangeError do
        LRaised := True;
    end;
    Check(LRaised, 'typed locking rejects an out-of-range coordinate');
  finally
    LWorld.Free;
  end;

  LRaised := False;
  try
    LWorld := TWorld2D.Create(0, 1);
    LWorld.Free;
  except
    on E: ERangeError do
      LRaised := True;
  end;
  Check(LRaised, 'a zero-width specialized world is rejected');
end;

procedure TestReplayAndValidation;
var
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LSolved: Boolean;
  LSignature: String;
  LValidation: TWorld2DValidationReport;
  LDifferent: TWorld2D;
  LWorld: TWorld2D;
  LTwin: TWorld2D;
begin
  LOptions := WorldOptions;
  LWorld := NewAnchoredWorld(TEST_SEED);
  LTwin := NewAnchoredWorld(TEST_SEED);
  LDifferent := NewAnchoredWorld(TEST_SEED + 1);
  try
    LWorld.Graph.SwitchToPass(WFC_WORLD2D_PASS_BIOME);
    LSolved := LWorld.TryGenerate(LOptions, LReport);
    if not LSolved then
      WriteLn('  [INFO] failed pass=', LReport.FailedPassIndex,
        ' kind=', Ord(LReport.Contradiction.Kind),
        ' entry=', LReport.Contradiction.EntryIndex,
        ' neighbor=', LReport.Contradiction.NeighborIndex);
    Check(LSolved,
      'the anchored terrain-biome-foliage pipeline solves');
    Check((LReport.Status = gssSolved) and (LReport.FailedPassIndex = -1),
      'the specialized solve exposes the complete core report');
    Check(LWorld.Graph.CurrentPassIndex = Ord(w2lBiome),
      'specialized generation preserves caller pass selection');
    Check(ValidateWorld2D(LWorld, LValidation),
      'the independent semantic validator accepts solved output');
    Check(LValidation.CheckedCells = TEST_WIDTH * TEST_HEIGHT * 3,
      'the validator checks every cell in every standard layer');
    Check(LValidation.CheckedRelations > LValidation.CheckedCells,
      'the validator independently checks cross-layer and adjacency relations');

    Check((LWorld.Value[w2lTerrain, 0, 0]
        = WFC_WORLD2D_TERRAIN_WATER)
      and (LWorld.Value[w2lBiome, 0, 0]
        = WFC_WORLD2D_BIOME_OCEAN),
      'terrain anchors constrain the dependent biome layer');
    Check((LWorld.Value[w2lFoliage, 1, 0]
        = WFC_WORLD2D_FOLIAGE_REEDS)
      and (not LWorld.LayerGraph[w2lFoliage].Entry[1, 0, 0].Generated),
      'legal foliage anchors remain caller-owned');

    LSignature := LWorld.PipelineSignature;
    WriteLn('  [INFO] canonical signature: ', LSignature);
    Check(LSignature = EXPECTED_SIGNATURE,
      'the canonical specialized pipeline matches its golden signature');
    Check(LWorld.TryGenerate(LOptions, LReport)
      and (LWorld.PipelineSignature = LSignature),
      'repeated generation replays on the same world instance');
    Check(LTwin.TryGenerate(LOptions, LReport)
      and (LTwin.PipelineSignature = LSignature),
      'an independent world reproduces the same layer signatures');
    Check(LDifferent.TryGenerate(LOptions, LReport)
      and (LDifferent.PipelineSignature <> LSignature),
      'a different seed changes the generated unlocked cells');
  finally
    LDifferent.Free;
    LTwin.Free;
    LWorld.Free;
  end;
end;

procedure TestWrappedWorld;
var
  LBiomeValue: TGraphValue;
  LConfig: TWorld2DConfig;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LSignature: String;
  LTerrainValue: TGraphValue;
  LValidation: TWorld2DValidationReport;
  LWorld: TWorld2D;
  X, Y: Integer;
begin
  LConfig := DefaultWorld2DConfig;
  LConfig.Seed := 12345;
  LConfig.WrapNeighbors := True;
  LWorld := TWorld2D.Create(5, 4, LConfig);
  try
    Check(LWorld.WrapNeighbors
      and (LWorld.LayerGraph[w2lTerrain].Entry[0, 0, 0][gdWest]
        = LWorld.LayerGraph[w2lTerrain].Entry[4, 0, 0])
      and (LWorld.LayerGraph[w2lTerrain].Entry[0, 0, 0][gdSouth]
        = LWorld.LayerGraph[w2lTerrain].Entry[0, 3, 0]),
      'the specialized wrapper supports a wrapped cardinal topology');
    LOptions := WorldOptions;
    Check(LWorld.TryGenerate(LOptions, LReport),
      'the wrapped three-pass world solves');
    Check(ValidateWorld2D(LWorld, LValidation),
      'independent validation includes wrapped boundary relations');
    LSignature := LWorld.PipelineSignature;
    Check(LWorld.TryGenerate(LOptions, LReport)
      and (LWorld.PipelineSignature = LSignature),
      'wrapped world generation replays exactly');

    //Every bounded relation in this vertical strip is legal. Only the
    //wrapped west/east seam places water directly beside mountain, so this
    //fixture fails if validation accidentally treats a wrapped world as
    //bounded or skips its boundary relations.
    for Y := 0 to Pred(Integer(LWorld.Height)) do
      for X := 0 to Pred(Integer(LWorld.Width)) do
      begin
        if X = 0 then
        begin
          LTerrainValue := WFC_WORLD2D_TERRAIN_WATER;
          LBiomeValue := WFC_WORLD2D_BIOME_OCEAN;
        end
        else if X = Pred(Integer(LWorld.Width)) then
        begin
          LTerrainValue := WFC_WORLD2D_TERRAIN_MOUNTAIN;
          LBiomeValue := WFC_WORLD2D_BIOME_ALPINE;
        end
        else
        begin
          LTerrainValue := WFC_WORLD2D_TERRAIN_LAND;
          LBiomeValue := WFC_WORLD2D_BIOME_PLAINS;
        end;
        LWorld
          .Lock(w2lTerrain, X, Y, LTerrainValue)
          .Lock(w2lBiome, X, Y, LBiomeValue)
          .Lock(w2lFoliage, X, Y, WFC_WORLD2D_FOLIAGE_NONE);
      end;
    Check((not ValidateWorld2D(LWorld, LValidation))
      and (LValidation.Issue.Kind = w2vikTerrainAdjacency)
      and (LValidation.Issue.X = 0)
      and (LValidation.Issue.Y = 0)
      and (LValidation.Issue.NeighborX = 4)
      and (LValidation.Issue.NeighborY = 0)
      and LValidation.Issue.HasDirection
      and (LValidation.Issue.Direction = gdWest),
      'independent validation rejects a seam-only wrapped adjacency');
  finally
    LWorld.Free;
  end;
end;

procedure TestIllegalLockAndRollback;
var
  LBiomeSignature: TWorld2DSignature;
  LOptions: TGraphSolveOptions;
  LOriginalSignature: String;
  LReport: TGraphSolveReport;
  LTerrainSignature: TWorld2DSignature;
  LValidation: TWorld2DValidationReport;
  LWorld: TWorld2D;
begin
  LOptions := WorldOptions;
  LWorld := NewAnchoredWorld(TEST_SEED);
  try
    Check(LWorld.TryGenerate(LOptions, LReport),
      'the rollback fixture first establishes valid output');
    LOriginalSignature := LWorld.PipelineSignature;
    LTerrainSignature := LWorld.LayerSignature(w2lTerrain);
    LBiomeSignature := LWorld.LayerSignature(w2lBiome);

    LWorld.Lock(w2lFoliage, 0, 0, WFC_WORLD2D_FOLIAGE_TREE);
    Check(not ValidateWorld2D(LWorld, LValidation)
      and (LValidation.Issue.Kind = w2vikFoliageBiome)
      and (LValidation.Issue.X = 0) and (LValidation.Issue.Y = 0),
      'the independent validator catches a tree placed in ocean biome');

    LWorld.Graph.SwitchToPass(WFC_WORLD2D_PASS_FOLIAGE);
    Check(not LWorld.TryGenerate(LOptions, LReport),
      'an illegal foliage lock fails the atomic pipeline');
    Check((LReport.Status = gssContradiction)
      and (LReport.FailedPassIndex = Ord(w2lFoliage))
      and (LReport.Contradiction.Kind = gckPreviousPass),
      'the failed lock reports its dependent pass and constraint kind');
    Check(LWorld.Graph.CurrentPassIndex = Ord(w2lFoliage),
      'failed specialized generation restores pass selection');
    Check((LWorld.LayerSignature(w2lTerrain) = LTerrainSignature)
      and (LWorld.LayerSignature(w2lBiome) = LBiomeSignature),
      'a later failure leaves earlier layer signatures unchanged');
    Check((LWorld.Value[w2lFoliage, 0, 0]
        = WFC_WORLD2D_FOLIAGE_TREE)
      and (not LWorld.LayerGraph[w2lFoliage].Entry[0, 0, 0].Generated),
      'the failed caller lock remains intact and caller-owned');

    LWorld.ClearLock(w2lFoliage, 0, 0);
    Check(LWorld.TryGenerate(LOptions, LReport)
      and (LWorld.PipelineSignature = LOriginalSignature),
      'clearing the illegal lock restores the exact deterministic pipeline');
    Check(ValidateWorld2D(LWorld, LValidation),
      'recovered output passes independent validation');
  finally
    LWorld.Free;
  end;
end;

procedure TestSignatureAndShapeGuards;
var
  LEntry: TGraphEntry;
  LNeighbor: TGraphEntry;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRaised: Boolean;
  LValidation: TWorld2DValidationReport;
  LWorld: TWorld2D;
begin
  LWorld := TWorld2D.Create(3, 2);
  try
    LRaised := False;
    try
      LWorld.LayerSignature(w2lTerrain);
    except
      on E: EWorld2D do
        LRaised := True;
    end;
    Check(LRaised, 'signatures reject incomplete output');
  finally
    LWorld.Free;
  end;

  LOptions := WorldOptions;
  LWorld := NewAnchoredWorld(TEST_SEED);
  try
    Check(LWorld.TryGenerate(LOptions, LReport),
      'the mutation fixture first establishes valid output');
    LEntry := LWorld.LayerGraph[w2lTerrain].Entry[0, 0, 0];
    LNeighbor := LEntry[gdEast];
    LEntry[gdEast] := nil;
    Check(ValidateWorld2D(LWorld, LValidation),
      'validation recomputes coordinates instead of trusting mutable links');
    LEntry[gdEast] := LNeighbor;
    LWorld.LayerGraph[w2lBiome].Entry[0, 0, 0].Value :=
      WFC_WORLD2D_BIOME_PLAINS;
    Check(not ValidateWorld2D(LWorld, LValidation)
      and (LValidation.Issue.Kind = w2vikBiomeTerrain),
      'validation detects direct cross-layer corruption without solving');
  finally
    LWorld.Free;
  end;

  LWorld := NewAnchoredWorld(TEST_SEED);
  try
    Check(LWorld.TryGenerate(LOptions, LReport),
      'the extra-pass fixture first establishes valid output');
    LWorld.Graph.SwitchToPass('extra');
    Check(not ValidateWorld2D(LWorld, LValidation)
      and (LValidation.Issue.Kind = w2vikPipelineShape),
      'validation rejects a nonstandard pipeline shape');
    LRaised := False;
    try
      LWorld.PipelineSignature;
    except
      on E: EWorld2D do
        LRaised := True;
    end;
    Check(LRaised,
      'the canonical pipeline signature cannot silently ignore extra passes');
  finally
    LWorld.Free;
  end;
end;

begin
  WriteLn('WFC 2D ecosystem suite');
  WriteLn('======================');
  RunTest('construction and typed guards', @TestConstructionAndGuards);
  RunTest('deterministic replay and validation', @TestReplayAndValidation);
  RunTest('wrapped world validation', @TestWrappedWorld);
  RunTest('illegal lock and atomic rollback', @TestIllegalLockAndRollback);
  RunTest('signature and shape guards', @TestSignatureAndShapeGuards);
  WriteLn('======================');
  WriteLn(Format('%d checks, %d failures', [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d WFC 2D checks failed', [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
