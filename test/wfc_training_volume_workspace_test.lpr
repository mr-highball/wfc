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
program wfc_training_volume_workspace_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_model,
  wfc_model_text,
  wfc_training,
  wfc_training_workspace,
  wfc_pipeline_model,
  wfc_pipeline_text,
  wfc_pipeline_run,
  wfc_pipeline_run_text,
  wfc_pipeline_result,
  wfc_pipeline_result_text,
  wfc_pipeline_runtime;

type
  TTestProcedure = procedure;

const
  VOLUME_SOURCE =
    'wfclearn=2'#10 +
    'name=VolumeWorkspace'#10 +
    'license=MIT'#10 +
    'source=fixture'#10 +
    'kind=adjacency3d'#10 +
    'boundary=wrap'#10 +
    'symmetry=none'#10 +
    'footprint=0,0'#10 +
    'order=0'#10 +
    'samples=1'#10 +
    'sample=0,1,1,2,vertical'#10 +
    'token=0,0,A'#10 +
    'token=0,1,B'#10 +
    'end'#10;

  FLAT_SOURCE =
    'wfclearn=1'#10 +
    'name=FlatWorkspace'#10 +
    'license=MIT'#10 +
    'source=fixture'#10 +
    'kind=adjacency2d'#10 +
    'boundary=wrap'#10 +
    'symmetry=none'#10 +
    'footprint=0,0'#10 +
    'order=0'#10 +
    'samples=1'#10 +
    'sample=0,1,1,flat'#10 +
    'token=0,0,A'#10 +
    'end'#10;

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

function TokensOf(
  const AValues: array of TWfcModelToken): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function VolumeSolveOptions: TWfcTrainingSolveOptions;
begin
  Result := DefaultWfcTrainingSolveOptions;
  Result.Width := 1;
  Result.Height := 1;
  Result.Seed := 314159;
  Result.Strategy := wpssOneWay;
  Result.MaxBacktracks := 16;
  Result.MaxPassBacktracks := 0;
  Result.CaptureTrace := False;
end;

function TestLimits: TWfcTrainingWorkspaceLimits;
begin
  Result := InteractiveWfcTrainingWorkspaceLimits;
  Result.MaxOutputCells := 8;
  Result.MaxBacktracks := 16;
  Result.MaxPassBacktracks := 4;
end;

function OutputIsAlternating(const ATokens: TWfcModelTokens): Boolean;
begin
  Result := (Length(ATokens) = 4) and
    (ATokens[0] = 'A') and (ATokens[1] = 'B') and
    (ATokens[2] = 'A') and (ATokens[3] = 'B');
end;

function ResultExportIsRejected(
  const AWorkspace: TWfcTrainingWorkspace): Boolean;
var
  LUnused: String;
begin
  Result := False;
  try
    LUnused := AWorkspace.ResultText;
  except
    on E: EWfcTrainingWorkspace do
      Result := True;
  end;
end;

function RunExportIsRejected(
  const AWorkspace: TWfcTrainingWorkspace): Boolean;
var
  LUnused: String;
begin
  Result := False;
  try
    LUnused := AWorkspace.RunText;
  except
    on E: EWfcTrainingWorkspace do
      Result := True;
  end;
end;

function ConfigureVolumeIsRejected(const AWorkspace: TWfcTrainingWorkspace;
  const AOptions: TWfcTrainingSolveOptions; const ADepth: Integer;
  const ALocks: TWfcPipelineCellLocks;
  const ADomains: TWfcPipelineCellDomains): Boolean;
begin
  Result := False;
  try
    AWorkspace.ConfigureVolumeRun(AOptions, ADepth, ALocks, ADomains);
  except
    on E: Exception do
      Result := True;
  end;
end;

function ConfigureFlatIsRejected(const AWorkspace: TWfcTrainingWorkspace;
  const AOptions: TWfcTrainingSolveOptions;
  const ALocks: TWfcPipelineCellLocks;
  const ADomains: TWfcPipelineCellDomains): Boolean;
begin
  Result := False;
  try
    AWorkspace.ConfigureRun(AOptions, ALocks, ADomains);
  except
    on E: Exception do
      Result := True;
  end;
end;

procedure SolveUnconstrainedVolume(const AWorkspace: TWfcTrainingWorkspace;
  const AOptions: TWfcTrainingSolveOptions);
begin
  AWorkspace.ConfigureVolumeRun(AOptions, 4, nil, nil);
  AWorkspace.Solve;
end;

procedure CheckInvalidatedRun(const AWorkspace: TWfcTrainingWorkspace;
  const AMessage: String);
begin
  Check(AWorkspace.HasRecipe and (not AWorkspace.HasRun) and
    (not AWorkspace.HasResult), AMessage);
  Check(RunExportIsRejected(AWorkspace) and
    ResultExportIsRejected(AWorkspace),
    AMessage + ' and makes stale exports unavailable');
end;

procedure TestVolumeWorkflowAndArtifactReplay;
var
  LDecodedResult: TWfcPipelineResult;
  LDecodedRun: TWfcPipelineRun;
  LDomain: TWfcPipelineCellDomain;
  LDomains: TWfcPipelineCellDomains;
  LLayer: TWfcPipelineResultLayer;
  LLocks: TWfcPipelineCellLocks;
  LModel: TWfcModel;
  LOptions: TWfcTrainingSolveOptions;
  LOutput: TWfcModelTokens;
  LRecipe: TWfcPipelineModel;
  LRecipeText: String;
  LReplay: TWfcPipelineResult;
  LResultText: String;
  LRunText: String;
  LWorkspace: TWfcTrainingWorkspace;
begin
  LWorkspace := TWfcTrainingWorkspace.Create(TestLimits);
  try
    LWorkspace.SetSourceText(VOLUME_SOURCE);
    Check(not LWorkspace.HasRecipe and not LWorkspace.HasRun and
      not LWorkspace.HasResult,
      'setting volume source starts with no derived artifacts');
    LWorkspace.Train;
    Check(LWorkspace.HasRecipe and (not LWorkspace.HasRun) and
      (not LWorkspace.HasResult) and (LWorkspace.Rank = 3) and
      LWorkspace.WrapNeighbors and (LWorkspace.SampleCount = 1) and
      (LWorkspace.SourceTokenCount = 2) and
      (LWorkspace.ModelItemCount = 2) and
      (LWorkspace.SourceOptions.Kind = wtkAdjacency3D),
      'training publishes the expected rank-3 recipe metadata');

    LModel := DecodeWfcModelText(LWorkspace.ModelText);
    try
      Check((LModel.Rank = 3) and (LModel.SampleWidth = 1) and
        (LModel.SampleHeight = 1) and (LModel.SampleDepth = 2) and
        (LModel.ValueCount = 2),
        'the workspace model export retains its trained volume shape');
    finally
      LModel.Free;
    end;

    LOptions := VolumeSolveOptions;
    SetLength(LLocks, 1);
    LLocks[0] := MakeWfcPipelineCellLock(
      LWorkspace.PublicPassIndex, 0, 0, 2, 'A');
    SetLength(LDomains, 1);
    LDomains[0] := MakeWfcPipelineCellDomain(
      LWorkspace.PublicPassIndex, 0, 0, 3, TokensOf(['B']));
    LWorkspace.ConfigureVolumeRun(LOptions, 4, LLocks, LDomains);
    Check(LWorkspace.HasRun and not LWorkspace.HasResult,
      'volume configuration publishes an unsolved depth-four run');
    LRunText := LWorkspace.RunText;

    LLocks[0].Token := 'B';
    LLocks[0].Z := 0;
    LDomains[0].AllowedTokens[0] := 'A';
    LDomains[0].Z := 0;
    Check(LWorkspace.RunText = LRunText,
      'the configured run owns detached Z lock and domain inputs');

    LWorkspace.Solve;
    LOutput := LWorkspace.OutputTokens;
    Check(LWorkspace.HasResult and
      (LWorkspace.ResultStatus = wprsSolved) and
      OutputIsAlternating(LOutput),
      'the Z lock and Z domain select the expected vertical phase');
    LOutput[0] := 'mutated';
    Check(OutputIsAlternating(LWorkspace.OutputTokens),
      'volume output access returns a detached token array');

    LRecipeText := LWorkspace.RecipeText;
    LResultText := LWorkspace.ResultText;
    Check((Length(LWorkspace.TrainingSignatureText) = 8) and
      (Length(LWorkspace.RecipeSignatureText) = 8) and
      (Length(LWorkspace.ResultSignatureText) = 8),
      'training, recipe, and result identities are all published');
    LWorkspace.Solve;
    Check(LWorkspace.ResultText = LResultText,
      'repeated volume solve preserves exact seeded replay');

    LRecipe := DecodeWfcPipelineModelText(LRecipeText);
    LDecodedRun := nil;
    LDecodedResult := nil;
    LReplay := nil;
    try
      LDecodedRun := DecodeWfcPipelineRunText(LRunText, LRecipe);
      Check((LDecodedRun.Width = 1) and (LDecodedRun.Height = 1) and
        (LDecodedRun.Depth = 4) and (LDecodedRun.LockCount = 1) and
        (LDecodedRun.DomainCount = 1) and
        (LDecodedRun.LockAt(0).Z = 2) and
        (LDecodedRun.LockAt(0).Token = 'A'),
        'canonical run replay retains the depth and Z lock');
      LDomain := LDecodedRun.DomainAt(0);
      Check((LDomain.Z = 3) and (Length(LDomain.AllowedTokens) = 1) and
        (LDomain.AllowedTokens[0] = 'B'),
        'canonical run replay retains the Z domain');
      Check(EncodeWfcPipelineModelText(LRecipe) = LRecipeText,
        'recipe export decodes and re-encodes byte-for-byte');
      Check(EncodeWfcPipelineRunText(LDecodedRun) = LRunText,
        'volume run export decodes and re-encodes byte-for-byte');

      LDecodedResult := DecodeWfcPipelineResultText(
        LResultText, LRecipe, LDecodedRun);
      Check(EncodeWfcPipelineResultText(LDecodedResult) = LResultText,
        'volume result export decodes and re-encodes byte-for-byte');
      Check((LDecodedResult.Status = wprsSolved) and
        (LDecodedResult.Width = 1) and (LDecodedResult.Height = 1) and
        (LDecodedResult.Depth = 4) and (LDecodedResult.CellCount = 4) and
        (LDecodedResult.LayerCount = 1),
        'decoded result retains its complete volume shape');
      LLayer := LDecodedResult.LayerAt(0);
      Check((LLayer.LabelName = 'output') and
        OutputIsAlternating(LLayer.Tokens),
        'decoded public layer retains x-fast, then Y, then Z output order');

      LReplay := ExecuteWfcPipeline(LRecipe, LDecodedRun);
      Check(EncodeWfcPipelineResultText(LReplay) = LResultText,
        'decoded recipe and run reproduce the exact result artifact');
    finally
      LReplay.Free;
      LDecodedResult.Free;
      LDecodedRun.Free;
      LRecipe.Free;
    end;
  finally
    LWorkspace.Free;
  end;
end;

procedure TestInvalidVolumeEditsDiscardOutput;
var
  LDomains: TWfcPipelineCellDomains;
  LLocks: TWfcPipelineCellLocks;
  LOptions: TWfcTrainingSolveOptions;
  LWorkspace: TWfcTrainingWorkspace;
begin
  LWorkspace := TWfcTrainingWorkspace.Create(TestLimits);
  try
    LWorkspace.SetSourceText(VOLUME_SOURCE);
    LWorkspace.Train;
    LOptions := VolumeSolveOptions;

    SolveUnconstrainedVolume(LWorkspace, LOptions);
    Check(LWorkspace.HasResult and
      (LWorkspace.ResultStatus = wprsSolved),
      'baseline volume result exists before invalid run edits');
    Check(ConfigureVolumeIsRejected(LWorkspace, LOptions, 0, nil, nil),
      'zero output depth is rejected');
    CheckInvalidatedRun(LWorkspace,
      'invalid depth discards the preceding run and result');

    SolveUnconstrainedVolume(LWorkspace, LOptions);
    LOptions.Width := 2;
    LOptions.Height := 2;
    Check(ConfigureVolumeIsRejected(LWorkspace, LOptions, 3, nil, nil),
      'a 2x2x3 product exceeds the eight-cell workspace envelope');
    CheckInvalidatedRun(LWorkspace,
      'invalid volume product discards the preceding run and result');

    LOptions := VolumeSolveOptions;
    SolveUnconstrainedVolume(LWorkspace, LOptions);
    LOptions.MaxBacktracks := 17;
    Check(ConfigureVolumeIsRejected(LWorkspace, LOptions, 4, nil, nil),
      'a local-search budget above the workspace limit is rejected');
    CheckInvalidatedRun(LWorkspace,
      'invalid local-search budget discards the preceding result');

    LOptions := VolumeSolveOptions;
    SolveUnconstrainedVolume(LWorkspace, LOptions);
    LOptions.MaxPassBacktracks := 5;
    Check(ConfigureVolumeIsRejected(LWorkspace, LOptions, 4, nil, nil),
      'a pass-search budget above the workspace limit is rejected');
    CheckInvalidatedRun(LWorkspace,
      'invalid pass-search budget discards the preceding result');

    LOptions := VolumeSolveOptions;
    SolveUnconstrainedVolume(LWorkspace, LOptions);
    SetLength(LLocks, 1);
    LLocks[0] := MakeWfcPipelineCellLock(
      LWorkspace.PublicPassIndex, 0, 0, 4, 'A');
    Check(ConfigureVolumeIsRejected(LWorkspace, LOptions, 4, LLocks, nil),
      'a lock at Z equal to depth is rejected');
    CheckInvalidatedRun(LWorkspace,
      'invalid Z lock discards the preceding result');

    LOptions := VolumeSolveOptions;
    SolveUnconstrainedVolume(LWorkspace, LOptions);
    SetLength(LDomains, 1);
    LDomains[0] := MakeWfcPipelineCellDomain(
      LWorkspace.PublicPassIndex, 0, 0, -1, TokensOf(['A']));
    Check(ConfigureVolumeIsRejected(LWorkspace, LOptions, 4, nil, LDomains),
      'a negative domain Z coordinate is rejected');
    CheckInvalidatedRun(LWorkspace,
      'invalid Z domain discards the preceding result');
  finally
    LWorkspace.Free;
  end;
end;

procedure TestVolumeAndLegacyApiSeparation;
var
  LOptions: TWfcTrainingSolveOptions;
  LWorkspace: TWfcTrainingWorkspace;
begin
  LOptions := VolumeSolveOptions;
  LWorkspace := TWfcTrainingWorkspace.Create(TestLimits);
  try
    LWorkspace.SetSourceText(VOLUME_SOURCE);
    LWorkspace.Train;
    SolveUnconstrainedVolume(LWorkspace, LOptions);
    Check(ConfigureFlatIsRejected(LWorkspace, LOptions, nil, nil),
      'the legacy run API rejects a rank-3 recipe');
    CheckInvalidatedRun(LWorkspace,
      'rank-3 misuse of the legacy API invalidates stale output');
  finally
    LWorkspace.Free;
  end;

  LWorkspace := TWfcTrainingWorkspace.Create(TestLimits);
  try
    LWorkspace.SetSourceText(FLAT_SOURCE);
    LWorkspace.Train;
    LOptions.Width := 2;
    LOptions.Height := 1;
    LWorkspace.ConfigureRun(LOptions, nil, nil);
    LWorkspace.Solve;
    Check(LWorkspace.HasResult and (LWorkspace.Rank = 2) and
      (Length(LWorkspace.OutputTokens) = 2),
      'the legacy API still solves a flat recipe');
    Check(ConfigureVolumeIsRejected(LWorkspace, LOptions, 2, nil, nil),
      'the volume run API rejects a non-volume recipe');
    CheckInvalidatedRun(LWorkspace,
      'flat misuse of the volume API invalidates stale output');
  finally
    LWorkspace.Free;
  end;
end;

{$IFDEF PAS2JS}
procedure TestMalformedBrowserVolumeAxes;
const
  AXIS_NAMES: array[0..2] of String = ('width', 'height', 'depth');
var
  I: Integer;
  LDepth: Integer;
  LOptions: TWfcTrainingSolveOptions;
  LWorkspace: TWfcTrainingWorkspace;
begin
  LWorkspace := TWfcTrainingWorkspace.Create(TestLimits);
  try
    LWorkspace.SetSourceText(VOLUME_SOURCE);
    LWorkspace.Train;
    for I := 0 to 2 do
    begin
      LOptions := VolumeSolveOptions;
      SolveUnconstrainedVolume(LWorkspace, LOptions);
      LOptions.Width := 2;
      LOptions.Height := 2;
      LDepth := 2;
      case I of
        0: asm LOptions.Width = 1.5; end;
        1: asm LOptions.Height = 1.5; end;
        2: asm LDepth = 1.5; end;
      end;
      Check(ConfigureVolumeIsRejected(LWorkspace, LOptions, LDepth, nil, nil),
        'the volume run API rejects fractional ' + AXIS_NAMES[I]);
      CheckInvalidatedRun(LWorkspace,
        'fractional ' + AXIS_NAMES[I] + ' discards stale run state');
    end;
  finally
    LWorkspace.Free;
  end;
end;
{$ENDIF}

begin
  WriteLn('WFC volume training-workspace conformance suite');
  WriteLn('===============================================');
  RunTest('volume workflow and artifact replay',
    @TestVolumeWorkflowAndArtifactReplay);
  RunTest('invalid volume edits and lifecycle',
    @TestInvalidVolumeEditsDiscardOutput);
  RunTest('volume and legacy API separation',
    @TestVolumeAndLegacyApiSeparation);
  {$IFDEF PAS2JS}
  RunTest('browser fractional volume axes', @TestMalformedBrowserVolumeAxes);
  {$ENDIF}
  WriteLn('===============================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d volume workspace checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
