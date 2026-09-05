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
program wfc_pipeline_result_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_model,
  wfc_rule_model,
  wfc_rule_text,
  wfc_sequence,
  wfc_pipeline_model,
  wfc_pipeline_run,
  wfc_pipeline_result;

type
  TTestProcedure = procedure;

  TResultInputs = record
    Versions: TWfcPipelineResultVersions;
    Status: TWfcPipelineResultStatus;
    PassBacktracks: Integer;
    EvidenceKind: TWfcPipelineEvidenceKind;
    EvidenceSignature: TGraphTraceSignature;
    Failure: TWfcPipelineFailure;
    Outcomes: TWfcPipelinePassOutcomes;
    Layers: TWfcPipelineResultLayers;
  end;

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

function TokensOf(const AValues: array of TWfcModelToken): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function IntegersOf(const AValues: array of Integer): TWfcModelIntegerArray;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function BuildRecipe(const AName: TWfcModelToken): TWfcPipelineModel;
var
  LMetadata: TWfcPipelineMetadata;
  LPasses: TWfcPipelinePasses;
  LResources: TWfcPipelineResources;
  LRuleModel: TWfcRuleModel;
  LRuleText: String;
  LRows: TWfcRuleRows;
begin
  LRows := nil;
  LRuleModel := TWfcRuleModel.Create(1,
    TokensOf([TWfcModelToken('A'), TWfcModelToken('B')]),
    IntegersOf([1, 1]), LRows);
  try
    LRuleText := EncodeWfcRuleText(LRuleModel);
  finally
    LRuleModel.Free;
  end;
  LMetadata := MakeWfcPipelineMetadata(AName, 'MIT',
    'result conformance recipe', 'result-recipe:v1');
  SetLength(LResources, 1);
  LResources[0] := MakeWfcPipelineResource('rules', wprkRules,
    LRuleText, 'result rule fixture', 'MIT', 'result-rules:v1');
  SetLength(LPasses, 2);
  LPasses[0] := MakeWfcPipelinePass('latent', wppvPrivate,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  LPasses[1] := MakeWfcPipelinePass('public', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  Result := TWfcPipelineModel.Create(LMetadata, 1, False, rmBottomUp,
    LResources, LPasses, nil, nil, nil);
end;

function BuildRun(const ARecipe: TWfcPipelineModel;
  const AStrategy: TWfcPipelineSolveStrategy;
  const ACaptureTrace: Boolean): TWfcPipelineRun;
var
  LMaxPassBacktracks: Integer;
begin
  if AStrategy = wpssNegotiated then
    LMaxPassBacktracks := 3
  else
    LMaxPassBacktracks := 0;
  Result := TWfcPipelineRun.Create(ARecipe, 2, 1, 1, 7,
    AStrategy, 10, LMaxPassBacktracks, ACaptureTrace, nil, nil);
end;

function BuildSolvedInputs: TResultInputs;
begin
  Result := Default(TResultInputs);
  Result.Versions := CurrentWfcPipelineResultVersions;
  Result.Status := wprsSolved;
  Result.EvidenceKind := wpekTrace;
  Result.EvidenceSignature := Cardinal($12345678);
  Result.Failure := EmptyWfcPipelineFailure;
  SetLength(Result.Outcomes, 2);
  Result.Outcomes[0].PassIndex := 0;
  Result.Outcomes[0].Decisions := 1;
  Result.Outcomes[0].Propagations := 2;
  Result.Outcomes[0].Executed := True;
  Result.Outcomes[0].ExecutionOrdinal := 0;
  Result.Outcomes[0].Disposition := gpdSolved;
  Result.Outcomes[1].PassIndex := 1;
  Result.Outcomes[1].Decisions := 2;
  Result.Outcomes[1].Propagations := 3;
  Result.Outcomes[1].Executed := True;
  Result.Outcomes[1].ExecutionOrdinal := 1;
  Result.Outcomes[1].Disposition := gpdSolved;
  SetLength(Result.Layers, 1);
  Result.Layers[0] := MakeWfcPipelineResultLayer(1, 'public',
    TokensOf([TWfcModelToken('A'), TWfcModelToken('B')]));
end;

function NewResult(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun; const AInputs: TResultInputs):
  TWfcPipelineResult;
begin
  Result := TWfcPipelineResult.Create(ARecipe, ARun, AInputs.Versions,
    AInputs.Status, AInputs.PassBacktracks, AInputs.EvidenceKind,
    AInputs.EvidenceSignature, AInputs.Failure, AInputs.Outcomes,
    AInputs.Layers);
end;

function ResultRejected(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun; const AInputs: TResultInputs;
  const AExpectedFragment: String): Boolean;
var
  LResult: TWfcPipelineResult;
begin
  Result := False;
  LResult := nil;
  try
    try
      LResult := NewResult(ARecipe, ARun, AInputs);
    except
      on E: Exception do
      begin
        Result := (AExpectedFragment = '') or
          (Pos(AExpectedFragment, E.Message) > 0);
        if not Result then
          WriteLn('    unexpected error: ', E.Message);
      end;
    end;
  finally
    LResult.Free;
  end;
end;

procedure TestSolvedResultAndOwnership;
var
  LCopy: TWfcPipelineResultLayers;
  LInputs: TResultInputs;
  LRecipe: TWfcPipelineModel;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
begin
  LRecipe := BuildRecipe('result fixture');
  LRun := BuildRun(LRecipe, wpssOneWay, True);
  LInputs := BuildSolvedInputs;
  LResult := NewResult(LRecipe, LRun, LInputs);
  try
    Check((LResult.RecipeSignature = LRecipe.Signature) and
      (LResult.RunSignature = LRun.Signature),
      'result retains the exact recipe/run provenance chain');
    Check((LResult.Width = 2) and (LResult.Height = 1) and
      (LResult.Depth = 1) and (LResult.CellCount = 2) and
      (LResult.Seed = 7) and (LResult.Strategy = wpssOneWay) and
      (LResult.MaxBacktracks = 10) and
      (LResult.MaxPassBacktracks = 0) and LResult.CaptureTrace,
      'result repeats the complete effective run identity');
    Check((LResult.Status = wprsSolved) and
      (LResult.EvidenceKind = wpekTrace) and
      (LResult.EvidenceSignature = Cardinal($12345678)) and
      (LResult.PassOutcomeCount = 2) and (LResult.LayerCount = 1),
      'result retains status, evidence, terminal pass outcomes, and layers');
    Check((LResult.LayerAt(0).PassIndex = 1) and
      (LResult.LayerAt(0).LabelName = 'public') and
      (LResult.LayerAt(0).Tokens[1] = 'B'),
      'successful results expose only the complete public layer');

    LInputs.Outcomes[0].Decisions := 99;
    LInputs.Layers[0].Tokens[0] := 'B';
    Check((LResult.PassOutcomeAt(0).Decisions = 1) and
      (LResult.LayerAt(0).Tokens[0] = 'A'),
      'constructor deep-copies pass outcomes and public token grids');
    LCopy := LResult.CopyLayers;
    LCopy[0].Tokens[0] := 'B';
    Check(LResult.LayerAt(0).Tokens[0] = 'A',
      'copy accessors detach nested public token arrays');
    Check(Length(WfcPipelineResultSignatureHex(LResult.Signature)) = 8,
      'result semantic identity uses canonical eight-digit FNV-1a text');
  finally
    LResult.Free;
    LRun.Free;
    LRecipe.Free;
  end;
end;

procedure TestResultGuards;
var
  LInputs: TResultInputs;
  LOtherRecipe: TWfcPipelineModel;
  LRecipe: TWfcPipelineModel;
  LRun: TWfcPipelineRun;
begin
  LRecipe := BuildRecipe('result fixture');
  LOtherRecipe := BuildRecipe('other result fixture');
  LRun := BuildRun(LRecipe, wpssOneWay, True);
  try
    LInputs := BuildSolvedInputs;
    Check(ResultRejected(LOtherRecipe, LRun, LInputs,
      'run provenance'),
      'a result cannot be rebound to a different recipe');

    LInputs := BuildSolvedInputs;
    Inc(LInputs.Versions.SolverAlgorithmVersion);
    Check(ResultRejected(LRecipe, LRun, LInputs,
      'solver-algorithm version'),
      'unknown replay-relevant result versions fail closed');

    LInputs := BuildSolvedInputs;
    LInputs.EvidenceKind := wpekNone;
    LInputs.EvidenceSignature := 0;
    Check(ResultRejected(LRecipe, LRun, LInputs,
      'require trace evidence'),
      'trace-enabled one-way results require their trace hash');

    LInputs := BuildSolvedInputs;
    SetLength(LInputs.Outcomes, 1);
    Check(ResultRejected(LRecipe, LRun, LInputs,
      'one outcome for every'),
      'every recipe pass has exactly one terminal outcome');

    LInputs := BuildSolvedInputs;
    LInputs.Outcomes[1].ExecutionOrdinal := 0;
    Check(ResultRejected(LRecipe, LRun, LInputs,
      'ordinals must be unique'),
      'executed-pass ordinals are unique and canonical');

    LInputs := BuildSolvedInputs;
    LInputs.Outcomes[0].Decisions := -1;
    Check(ResultRejected(LRecipe, LRun, LInputs,
      'counters cannot be negative'),
      'pass outcome counters cannot be negative');

    LInputs := BuildSolvedInputs;
    LInputs.Layers[0].PassIndex := 0;
    Check(ResultRejected(LRecipe, LRun, LInputs,
      'public layers must be complete'),
      'private passes cannot appear as result layers');

    LInputs := BuildSolvedInputs;
    LInputs.Layers[0].Tokens[1] := 'outside';
    Check(ResultRejected(LRecipe, LRun, LInputs,
      'outside its public vocabulary'),
      'public result tokens resolve against the recipe vocabulary');

    LInputs := BuildSolvedInputs;
    LInputs.Layers := nil;
    Check(ResultRejected(LRecipe, LRun, LInputs,
      'every public layer'),
      'successful results cannot omit public layers');
  finally
    LRun.Free;
    LOtherRecipe.Free;
    LRecipe.Free;
  end;
end;

procedure TestFailureAndNegotiation;
var
  LInputs: TResultInputs;
  LRecipe: TWfcPipelineModel;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
begin
  LRecipe := BuildRecipe('failure fixture');
  LRun := BuildRun(LRecipe, wpssOneWay, True);
  try
    LInputs := BuildSolvedInputs;
    LInputs.Status := wprsContradiction;
    LInputs.Failure.Kind := gckEmptyDomain;
    LInputs.Failure.PassIndex := 1;
    LInputs.Failure.EntryIndex := 0;
    LInputs.Outcomes[1].Contradictions := 1;
    LInputs.Outcomes[1].Disposition := gpdFailed;
    LInputs.Layers := nil;
    LResult := NewResult(LRecipe, LRun, LInputs);
    try
      Check((LResult.Status = wprsContradiction) and
        (LResult.CopyFailure.Kind = gckEmptyDomain) and
        (LResult.CopyFailure.EntryIndex = 0) and
        (LResult.LayerCount = 0),
        'failed results retain structured diagnostics and no partial layers');
    finally
      LResult.Free;
    end;

    LInputs.Layers := BuildSolvedInputs.Layers;
    Check(ResultRejected(LRecipe, LRun, LInputs,
      'cannot contain partial'),
      'failed results reject partial public output');
  finally
    LRun.Free;
  end;

  LRun := BuildRun(LRecipe, wpssNegotiated, False);
  try
    LInputs := BuildSolvedInputs;
    LInputs.PassBacktracks := 1;
    LInputs.EvidenceKind := wpekNegotiationTranscript;
    LInputs.EvidenceSignature := Cardinal($89ABCDEF);
    LResult := NewResult(LRecipe, LRun, LInputs);
    try
      Check((LResult.Strategy = wpssNegotiated) and
        (LResult.PassBacktracks = 1) and
        (LResult.EvidenceKind = wpekNegotiationTranscript),
        'negotiated results retain bounded pass search and transcript identity');
    finally
      LResult.Free;
    end;
    LInputs.EvidenceKind := wpekNone;
    LInputs.EvidenceSignature := 0;
    Check(ResultRejected(LRecipe, LRun, LInputs,
      'negotiation-transcript evidence'),
      'negotiated results cannot discard their transcript identity');
  finally
    LRun.Free;
    LRecipe.Free;
  end;
end;

procedure TestReportMappingAndCapture;
var
  LGraph: TGraph;
  LRecipe: TWfcPipelineModel;
  LReport: TGraphSolveReport;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
begin
  LRecipe := BuildRecipe('capture fixture');
  LRun := BuildRun(LRecipe, wpssOneWay, True);
  LGraph := TGraph.Create;
  try
    LGraph.CurrentPass := 'latent';
    LGraph.SwitchToPass('public');
    LGraph.Seed := LRun.Seed;
    LGraph.Reshape(2, 1, 1);
    LGraph.PassGraph[1].Entry[0, 0, 0].Value := 'A';
    LGraph.PassGraph[1].Entry[1, 0, 0].Value := 'B';

    LReport := Default(TGraphSolveReport);
    LReport.Status := gssSolved;
    LReport.Seed := LRun.Seed;
    LReport.GraphModelVersion := WFC_GRAPH_MODEL_VERSION;
    LReport.RandomAlgorithmVersion := WFC_RANDOM_ALGORITHM_VERSION;
    LReport.SolverAlgorithmVersion := WFC_SOLVER_ALGORITHM_VERSION;
    LReport.PipelineAlgorithmVersion := WFC_PIPELINE_ALGORITHM_VERSION;
    LReport.FailedPassIndex := -1;
    LReport.Contradiction.Kind := gckNone;
    LReport.Contradiction.PassIndex := -1;
    LReport.Contradiction.EntryIndex := -1;
    LReport.Contradiction.NeighborIndex := -1;
    LReport.Contradiction.Direction := gdNorth;
    LReport.Contradiction.DependencyPassIndex := -1;
    LReport.TraceCaptured := True;
    SetLength(LReport.Passes, 2);
    LReport.Passes[0].Executed := True;
    LReport.Passes[0].ExecutionOrdinal := 0;
    LReport.Passes[0].Disposition := gpdSolved;
    LReport.Passes[1].Executed := True;
    LReport.Passes[1].ExecutionOrdinal := 1;
    LReport.Passes[1].Disposition := gpdSolved;
    SetLength(LReport.ExecutionOrder, 2);
    LReport.ExecutionOrder[0] := 0;
    LReport.ExecutionOrder[1] := 1;
    LReport.TraceHash := CalculateGraphTraceHash(LReport);

    LResult := CreateWfcPipelineResultFromSolveReport(
      LRecipe, LRun, LGraph, LReport);
    try
      Check((LResult.Status = wprsSolved) and
        (LResult.EvidenceSignature = LReport.TraceHash) and
        (LResult.LayerAt(0).Tokens[0] = 'A') and
        (LResult.LayerAt(0).Tokens[1] = 'B'),
        'report helper maps exposed counters/evidence and captures row-major public values');
    finally
      LResult.Free;
    end;

    LReport.TraceHash := LReport.TraceHash xor 1;
    try
      LResult := CreateWfcPipelineResultFromSolveReport(
        LRecipe, LRun, LGraph, LReport);
      LResult.Free;
      Check(False, 'report helper verifies captured trace evidence');
    except
      on E: EWfcPipelineResult do
        Check(Pos('trace signature', E.Message) > 0,
          'report helper verifies captured trace evidence');
    end;
    LReport.TraceHash := CalculateGraphTraceHash(LReport);
    LReport.ExecutionOrder[1] := 0;
    try
      LResult := CreateWfcPipelineResultFromSolveReport(
        LRecipe, LRun, LGraph, LReport);
      LResult.Free;
      Check(False, 'report helper verifies execution-order evidence');
    except
      on E: EWfcPipelineResult do
        Check(Pos('execution order', E.Message) > 0,
          'report helper verifies execution-order evidence');
    end;
    LReport.ExecutionOrder[1] := 1;

    LGraph.PassGraph[1].Entry[1, 0, 0].ClearValue;
    try
      LResult := CreateWfcPipelineResultFromSolveReport(
        LRecipe, LRun, LGraph, LReport);
      LResult.Free;
      Check(False, 'capture rejects an empty successful public entry');
    except
      on E: EWfcPipelineResult do
        Check(Pos('empty entry', E.Message) > 0,
          'capture rejects an empty successful public entry');
    end;
  finally
    LGraph.Free;
    LRun.Free;
    LRecipe.Free;
  end;
end;

procedure TestVersionAndLimitContract;
begin
  Check((WFC_PIPELINE_RESULT_VERSION = 1) and
    (WFC_PIPELINE_RESULT_SIGNATURE_VERSION = 1) and
    (WFC_PIPELINE_RESULT_MAX_PUBLIC_LAYER_COUNT = 256) and
    (WFC_PIPELINE_RESULT_MAX_TOTAL_PUBLIC_CELL_COUNT = 4194304) and
    (WFC_PIPELINE_RESULT_MAX_ENCODED_TOKEN_LENGTH = 1048576) and
    (WFC_PIPELINE_RESULT_MAX_TOTAL_ENCODED_TOKEN_LENGTH = 67108864),
    'version-one result layer, cell, and token boundaries are exact');
end;

begin
  WriteLn('WFC portable pipeline result model suite');
  WriteLn('========================================');
  RunTest('solved result and ownership', @TestSolvedResultAndOwnership);
  RunTest('result guards', @TestResultGuards);
  RunTest('failure and negotiation', @TestFailureAndNegotiation);
  RunTest('report mapping and capture', @TestReportMappingAndCapture);
  RunTest('version and limit contract', @TestVersionAndLimitContract);
  WriteLn('========================================');
  WriteLn(GCheckCount, ' checks, ', GFailureCount, ' failures');
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d pipeline-result checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
