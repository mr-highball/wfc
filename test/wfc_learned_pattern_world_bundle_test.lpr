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
program wfc_learned_pattern_world_bundle_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  {$IFNDEF PAS2JS}
  Classes,
  {$ENDIF}
  wfc,
  wfc_model,
  wfc_rule_model,
  wfc_pipeline_model,
  wfc_pipeline_text,
  wfc_pipeline_run,
  wfc_pipeline_run_text,
  wfc_pipeline_result,
  wfc_pipeline_result_text,
  wfc_pipeline_runtime,
  learned_pattern_world_bundle;

const
  TERRAIN_ROWS: array[0..5] of String = (
    '~~~~~~~~',
    '~.~...~~',
    '~~~.#.~~',
    '~~~.#.~~',
    '~~~...~~',
    '.~~~~~~~'
  );
  FOLIAGE_ROWS: array[0..5] of String = (
    'rrrrrrrr',
    'rgrgggrr',
    'rrrgmgrr',
    'rrrgmTrr',
    'rrrgggrr',
    'grrrrrrr'
  );
  STRUCTURE_ROWS: array[0..5] of String = (
    'DDDDDDDD',
    'DHDHHHDD',
    'DDDHMHDD',
    'DDDHMHDD',
    'DDDHHHDD',
    'HDDDDDDD'
  );

  FNV_OFFSET_BASIS = Cardinal(2166136261);

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
begin
  HashCardinal(AHash, Cardinal(Length(AValue)));
  for I := 1 to Length(AValue) do
    HashByte(AHash, Byte(Ord(AValue[I])));
end;

function LayerHash(const ALayer: TWfcPipelineResultLayer): Cardinal;
var
  I: Integer;
begin
  Result := FNV_OFFSET_BASIS;
  HashAscii(Result, 'LearnedPatternWorld/layer-v1');
  HashAscii(Result, String(ALayer.LabelName));
  HashCardinal(Result, LEARNED_PATTERN_WORLD_BUNDLE_WIDTH);
  HashCardinal(Result, LEARNED_PATTERN_WORLD_BUNDLE_HEIGHT);
  for I := 0 to Length(ALayer.Tokens) - 1 do
    HashAscii(Result, String(ALayer.Tokens[I]));
end;

function ExpectedToken(const ALayerIndex: Integer;
  const ACode: Char): TWfcModelToken;
begin
  Result := '';
  case ALayerIndex of
    0:
      Result := TWfcModelToken(ACode);
    1:
      case ACode of
        'r': Result := 'reeds';
        'g': Result := 'grass';
        'T': Result := 'tree';
        'm': Result := 'moss';
      end;
    2:
      case ACode of
        'D': Result := 'dock';
        'H': Result := 'hut';
        'M': Result := 'mine';
      end;
  end;
end;

function LayerMatchesRows(const ALayer: TWfcPipelineResultLayer;
  const ALayerIndex: Integer): Boolean;
var
  LCode: Char;
  LExpected: TWfcModelToken;
  X: Integer;
  Y: Integer;
begin
  if Length(ALayer.Tokens) <>
      LEARNED_PATTERN_WORLD_BUNDLE_WIDTH *
      LEARNED_PATTERN_WORLD_BUNDLE_HEIGHT then
    Exit(False);
  for Y := 0 to LEARNED_PATTERN_WORLD_BUNDLE_HEIGHT - 1 do
    for X := 0 to LEARNED_PATTERN_WORLD_BUNDLE_WIDTH - 1 do
    begin
      case ALayerIndex of
        0: LCode := TERRAIN_ROWS[Y][X + 1];
        1: LCode := FOLIAGE_ROWS[Y][X + 1];
        2: LCode := STRUCTURE_ROWS[Y][X + 1];
      else
        Exit(False);
      end;
      LExpected := ExpectedToken(ALayerIndex, LCode);
      if (LExpected = '') or
          (ALayer.Tokens[Y * LEARNED_PATTERN_WORLD_BUNDLE_WIDTH + X] <>
          LExpected) then
        Exit(False);
    end;
  Result := True;
end;

function ResultHasNoPrivatePatternKey(
  const AResult: TWfcPipelineResult): Boolean;
var
  I: Integer;
  J: Integer;
  LLayer: TWfcPipelineResultLayer;
begin
  for I := 0 to AResult.LayerCount - 1 do
  begin
    LLayer := AResult.LayerAt(I);
    for J := 0 to Length(LLayer.Tokens) - 1 do
      if Pos('@p', String(LLayer.Tokens[J])) = 1 then
        Exit(False);
  end;
  Result := True;
end;

{$IFNDEF PAS2JS}
function ReadFixtureBytes(const APath: String): String;
var
  LStream: TFileStream;
begin
  Result := '';
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    if LStream.Size > 8192 then
      raise Exception.CreateFmt('fixture exceeds the test read limit: %s',
        [APath]);
    SetLength(Result, Integer(LStream.Size));
    if Length(Result) <> 0 then
      LStream.ReadBuffer(Result[1], Length(Result));
  finally
    LStream.Free;
  end;
end;
{$ENDIF}

procedure TestPortableBundle;
var
  I: Integer;
  {$IFNDEF PAS2JS}
  LBundleDirectory: String;
  {$ENDIF}
  LDecodedRecipe: TWfcPipelineModel;
  LDecodedResult: TWfcPipelineResult;
  LDecodedRun: TWfcPipelineRun;
  LLayer: TWfcPipelineResultLayer;
  LLocksTargetTerrain: Boolean;
  LProgrammaticResult: TWfcPipelineResult;
  LProgrammaticRun: TWfcPipelineRun;
  LRecipe: TWfcPipelineModel;
  LRecipeText: String;
  LResource: TWfcPipelineResource;
  LResult: TWfcPipelineResult;
  LResultText: String;
  LRun: TWfcPipelineRun;
  LRunText: String;
  LShapeValid: Boolean;
  LVersions: TWfcPipelineVersions;
begin
  WriteLn('[TEST] canonical learned-pattern-world pipeline bundle');
  {$IFNDEF PAS2JS}
  if ParamCount <> 1 then
    raise Exception.Create(
      'usage: wfc_learned_pattern_world_bundle_test BUNDLE-DIRECTORY');
  LBundleDirectory := IncludeTrailingPathDelimiter(ParamStr(1));
  {$ENDIF}
  LRecipe := CreateLearnedPatternWorldBundleRecipe;
  try
    LRecipeText := EncodeWfcPipelineModelText(LRecipe);
    LVersions := LRecipe.CopyVersions;
    Check((LEARNED_PATTERN_WORLD_BUNDLE_VERSION = 1) and
      (LVersions.Pattern2DBridgeVersion = 2),
      'the bundle selects Pattern2D inverse-lowering bridge version 2');
    Check((LRecipe.Rank = 2) and LRecipe.WrapNeighbors and
      (LRecipe.RunMode = rmBottomUp) and
      (LRecipe.ResourceCount = 3) and (LRecipe.PassCount = 4) and
      (LRecipe.DependencyCount = 3) and (LRecipe.BridgeCount = 1) and
      (LRecipe.RequirementCount = 7),
      'the recipe exposes its complete typed four-pass topology');
    Check((Length(LRecipeText) = LEARNED_PATTERN_WORLD_RECIPE_BYTES) and
      (LRecipe.Signature = LEARNED_PATTERN_WORLD_RECIPE_SIGNATURE),
      'the canonical recipe bytes and semantic signature are pinned');

    LResource := LRecipe.ResourceAt(0);
    Check((LResource.Kind = wprkPattern2D) and
      (Length(LResource.Document) = LEARNED_PATTERN_WORLD_PATTERN_BYTES),
      'the recipe owns the exact canonical learned Pattern2D resource');
    LResource := LRecipe.ResourceAt(1);
    Check((Length(LResource.Document) =
        LEARNED_PATTERN_WORLD_FOLIAGE_RULE_BYTES) and
      (LRecipe.BorrowRuleResource(1).Signature =
        LEARNED_PATTERN_WORLD_FOLIAGE_RULE_SIGNATURE),
      'the foliage resource retains its exact rule artifact identity');
    LResource := LRecipe.ResourceAt(2);
    Check((Length(LResource.Document) =
        LEARNED_PATTERN_WORLD_STRUCTURE_RULE_BYTES) and
      (LRecipe.BorrowRuleResource(2).Signature =
        LEARNED_PATTERN_WORLD_STRUCTURE_RULE_SIGNATURE),
      'the structure resource retains its exact rule artifact identity');

    LDecodedRecipe := DecodeWfcPipelineModelText(LRecipeText);
    try
      Check((LDecodedRecipe.Signature = LRecipe.Signature) and
        (EncodeWfcPipelineModelText(LDecodedRecipe) = LRecipeText),
        'programmatic and decoded recipes have one canonical identity');

      LProgrammaticRun := CreateLearnedPatternWorldBundleRun(LRecipe);
      try
        LProgrammaticResult := ExecuteWfcPipeline(LRecipe,
          LProgrammaticRun);
        try
          LRun := CreateLearnedPatternWorldBundleRun(LDecodedRecipe);
          try
            LRunText := EncodeWfcPipelineRunText(LRun);
            Check((LRun.Width = 8) and (LRun.Height = 6) and
              (LRun.Depth = 1) and (LRun.Seed = 0) and
              (LRun.Strategy = wpssOneWay) and
              (LRun.MaxBacktracks = 65536) and
              (LRun.MaxPassBacktracks = 0) and
              (not LRun.CaptureTrace) and (LRun.LockCount = 8) and
              (LRun.DomainCount = 0),
              'the run pins shape, seed, strategy, limits, and eight terrain locks');
            LLocksTargetTerrain := LRun.LockCount = 8;
            if LLocksTargetTerrain then
              for I := 0 to LRun.LockCount - 1 do
                LLocksTargetTerrain := LLocksTargetTerrain and
                  (LRun.LockAt(I).PassIndex = 1);
            Check(LLocksTargetTerrain,
              'all eight portable locks target the public terrain pass');
            Check((Length(LRunText) = LEARNED_PATTERN_WORLD_RUN_BYTES) and
              (LRun.Signature = LEARNED_PATTERN_WORLD_RUN_SIGNATURE),
              'the canonical run bytes and semantic signature are pinned');

            LDecodedRun := DecodeWfcPipelineRunText(LRunText,
              LDecodedRecipe);
            try
              Check((LDecodedRun.Signature = LRun.Signature) and
                (EncodeWfcPipelineRunText(LDecodedRun) = LRunText),
                'the canonical run round-trips exactly');
              LResult := ExecuteWfcPipeline(LDecodedRecipe, LDecodedRun);
              try
                Check((LResult.Status = wprsSolved) and
                  (LResult.PassOutcomeCount = 4) and
                  (LResult.LayerCount = 3),
                  'the decoded bundle solves all passes and publishes three layers');
                LShapeValid := (LResult.Width = 8) and
                  (LResult.Height = 6) and (LResult.Depth = 1) and
                  (LResult.Seed = 0) and
                  (LResult.Strategy = wpssOneWay) and
                  (LResult.MaxBacktracks = 65536) and
                  (not LResult.CaptureTrace);
                Check(LShapeValid,
                  'the result preserves the exact replay boundary');

                if LResult.LayerCount = 3 then
                begin
                  LLayer := LResult.LayerAt(0);
                  Check((LLayer.PassIndex = 1) and
                    (LLayer.LabelName = 'terrain') and
                    LayerMatchesRows(LLayer, 0),
                    'terrain matches the established seed-zero rows');
                  Check(LayerHash(LLayer) =
                    LEARNED_PATTERN_WORLD_TERRAIN_HASH,
                    'terrain retains hash EBBC9390');

                  LLayer := LResult.LayerAt(1);
                  Check((LLayer.PassIndex = 2) and
                    (LLayer.LabelName = 'foliage') and
                    LayerMatchesRows(LLayer, 1),
                    'foliage matches the established seed-zero rows');
                  Check(LayerHash(LLayer) =
                    LEARNED_PATTERN_WORLD_FOLIAGE_HASH,
                    'foliage retains hash 92D4BC87');

                  LLayer := LResult.LayerAt(2);
                  Check((LLayer.PassIndex = 3) and
                    (LLayer.LabelName = 'structure') and
                    LayerMatchesRows(LLayer, 2),
                    'structure matches the established seed-zero rows');
                  Check(LayerHash(LLayer) =
                    LEARNED_PATTERN_WORLD_STRUCTURE_HASH,
                    'structure retains hash 8FA9D854');
                end
                else
                  for I := 0 to 5 do
                    Check(False,
                      'a missing public layer prevents row/hash validation');
                Check(ResultHasNoPrivatePatternKey(LResult),
                  'no public result token exposes a private pattern key');

                LResultText := EncodeWfcPipelineResultText(LResult);
                Check((Length(LResultText) =
                    LEARNED_PATTERN_WORLD_RESULT_BYTES) and
                  (LResult.Signature =
                    LEARNED_PATTERN_WORLD_RESULT_SIGNATURE),
                  'the canonical result bytes and semantic signature are pinned');
                Check(EncodeWfcPipelineResultText(LProgrammaticResult) =
                  LResultText,
                  'programmatic and decoded recipes execute identically');

                {$IFNDEF PAS2JS}
                Check(ReadFixtureBytes(LBundleDirectory +
                    'recipe.wfcpipeline') = LRecipeText,
                  'the committed recipe is byte-exact canonical LF text');
                Check(ReadFixtureBytes(LBundleDirectory +
                    'run.wfcrun') = LRunText,
                  'the committed run is byte-exact canonical LF text');
                Check(ReadFixtureBytes(LBundleDirectory +
                    'result.wfcresult') = LResultText,
                  'the committed result is byte-exact canonical LF text');
                {$ENDIF}

                LDecodedResult := DecodeWfcPipelineResultText(LResultText,
                  LDecodedRecipe, LDecodedRun);
                try
                  Check((LDecodedResult.Signature = LResult.Signature) and
                    (EncodeWfcPipelineResultText(LDecodedResult) =
                    LResultText),
                    'the canonical result round-trips exactly');
                finally
                  LDecodedResult.Free;
                end;
              finally
                LResult.Free;
              end;
            finally
              LDecodedRun.Free;
            end;
          finally
            LRun.Free;
          end;
        finally
          LProgrammaticResult.Free;
        end;
      finally
        LProgrammaticRun.Free;
      end;
    finally
      LDecodedRecipe.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

begin
  TestPortableBundle;
  WriteLn('Checks: ', GCheckCount, ', Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d learned bundle checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
