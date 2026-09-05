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
program wfc_selective_negotiation_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_trace;

const
  EXPECTED_LEAF_TRANSCRIPT = TGraphTraceSignature($7A595E38);
  EXPECTED_ROADS_SEARCH_TRANSCRIPT = TGraphTraceSignature($80926222);
  EXPECTED_ROADS_TRANSCRIPT = TGraphTraceSignature($E29050A0);
  EXPECTED_FULL_REPAIR_TRANSCRIPT = TGraphTraceSignature($9DB789E4);
  EXPECTED_CONTEXT_JOIN_TRANSCRIPT = TGraphTraceSignature($DE454137);
  EXPECTED_FULL_NEGOTIATION_V1_TRANSCRIPT =
    TGraphTraceSignature($6F76591D);

  PASS_TERRAIN = 'terrain';
  PASS_CLIMATE = 'climate';
  PASS_ROADS = 'roads';
  PASS_HOUSING = 'housing';
  PASS_DECOR = 'decor';

  TERRAIN_MEADOW = 'meadow';
  TERRAIN_RIDGE = 'ridge';
  CLIMATE_SUN = 'sun';
  CLIMATE_RAIN = 'rain';
  ROADS_TRAIL = 'trail';
  ROADS_PLAZA = 'plaza';
  HOUSING_HOME = 'home';
  HOUSING_MARKET = 'market';
  DECOR_GARDEN = 'garden';
  DECOR_PORCH = 'porch';
  DECOR_MARKET_DAY = 'market-day';
  DECOR_MARKET_LIGHTS = 'market-lights';

type
  TTestProcedure = procedure;

  TRepairValues = record
    Terrain: String;
    Climate: String;
    Roads: String;
    Housing: String;
    Decor: String;
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

function CapturedSolveOptions(const AMaxBacktracks: Integer):
  TGraphSolveOptions;
begin
  Result := DefaultGraphSolveOptions;
  Result.MaxBacktracks := AMaxBacktracks;
  Result.CaptureTrace := True;
end;

function NegotiationOptions(const AMaxPassBacktracks,
  AMaxSolverBacktracks: Integer): TGraphNegotiationOptions;
begin
  Result := DefaultGraphNegotiationOptions;
  Result.MaxPassBacktracks := AMaxPassBacktracks;
  Result.SolveOptions := CapturedSolveOptions(AMaxSolverBacktracks);
end;

function LabelsOf(const AValues: array of String): TGraphPassLabels;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do
    Result[I] := AValues[I];
end;

function IndicesEqual(const AValues: TGraphPassIndices;
  const AExpected: array of Integer): Boolean;
var
  I: Integer;
begin
  if Length(AValues) <> Length(AExpected) then
    Exit(False);
  for I := 0 to High(AValues) do
    if AValues[I] <> AExpected[I] then
      Exit(False);
  Result := True;
end;

function ExecutionOrderEqual(const AReport: TGraphSolveReport;
  const AExpected: array of Integer): Boolean;
begin
  Result := IndicesEqual(AReport.ExecutionOrder, AExpected);
end;

function ValuesEqual(const A, B: TRepairValues): Boolean;
begin
  Result := (A.Terrain = B.Terrain)
    and (A.Climate = B.Climate)
    and (A.Roads = B.Roads)
    and (A.Housing = B.Housing)
    and (A.Decor = B.Decor);
end;

function EntryState(const AEntry: TGraphEntry): String;
begin
  if AEntry.Empty then
    Result := '-'
  else if AEntry.Generated then
    Result := '+' + AEntry.Value
  else
    Result := '!' + AEntry.Value;
end;

function GraphState(const AGraph: TGraph): String;
var
  I, J: Integer;
  LAllowed: TGraphValues;
begin
  Result := IntToStr(AGraph.CurrentPassIndex) + ':';
  for I := 0 to Pred(AGraph.TotalPassCount) do
  begin
    if I > 0 then
      Result := Result + '|';
    Result := Result + EntryState(AGraph.PassGraph[I].Entry[0, 0, 0]);
    if AGraph.PassGraph[I].HasAllowedValues(0, 0, 0) then
    begin
      Result := Result + '{';
      LAllowed := AGraph.PassGraph[I].CopyAllowedValues(0, 0, 0);
      for J := 0 to High(LAllowed) do
      begin
        if J > 0 then
          Result := Result + ',';
        Result := Result + LAllowed[J];
      end;
      Result := Result + '}';
    end;
  end;
end;

function SolveFingerprint(const AReport: TGraphSolveReport): String;
var
  I: Integer;
begin
  Result := IntToStr(Ord(AReport.Status)) + ':'
    + IntToStr(AReport.FailedPassIndex) + ':'
    + IntToStr(Ord(AReport.Contradiction.Kind)) + ':'
    + IntToStr(AReport.Contradiction.PassIndex) + ':'
    + IntToStr(AReport.Contradiction.EntryIndex) + ':'
    + IntToStr(AReport.Contradiction.DependencyPassIndex) + ':'
    + GraphTraceSignatureHex(AReport.TraceHash);
  for I := 0 to High(AReport.ExecutionOrder) do
    Result := Result + ':e' + IntToStr(AReport.ExecutionOrder[I]);
  for I := 0 to High(AReport.Passes) do
    Result := Result + ':p' + IntToStr(I) + '='
      + IntToStr(AReport.Passes[I].Decisions) + ','
      + IntToStr(AReport.Passes[I].Propagations) + ','
      + IntToStr(AReport.Passes[I].Contradictions) + ','
      + IntToStr(AReport.Passes[I].Backtracks) + ','
      + IntToStr(AReport.Passes[I].ExcludedAssignments) + ','
      + IntToStr(Ord(AReport.Passes[I].Disposition));
end;

function NegotiationFingerprint(
  const AReport: TGraphNegotiationReport): String;
var
  I, J: Integer;
begin
  Result := IntToStr(Ord(AReport.Status)) + ':'
    + IntToStr(AReport.PassBacktracks) + ':'
    + GraphTraceSignatureHex(AReport.TranscriptHash);
  for I := 0 to High(AReport.Attempts) do
  begin
    Result := Result + ':a' + IntToStr(I) + '='
      + IntToStr(AReport.Attempts[I].BacktrackedPassIndex) + ','
      + IntToStr(AReport.Attempts[I].BacktrackedExecutionOrdinal) + '[';
    for J := 0 to High(AReport.Attempts[I].ExcludedAssignment) do
      Result := Result
        + IntToStr(AReport.Attempts[I].ExcludedAssignment[J]) + ';';
    Result := Result + ']'
      + SolveFingerprint(AReport.Attempts[I].SolveReport);
  end;
  Result := Result + ':final=' + SolveFingerprint(AReport.FinalReport);
end;

function SelectiveFingerprint(
  const AReport: TGraphSelectiveNegotiationReport): String;
var
  I: Integer;
begin
  Result := IntToStr(AReport.ScopeAlgorithmVersion) + ':';
  for I := 0 to High(AReport.RequestedRootIndices) do
    Result := Result + 'r' + IntToStr(AReport.RequestedRootIndices[I]);
  Result := Result + ':';
  for I := 0 to High(AReport.ActivePassIndices) do
    Result := Result + 'a' + IntToStr(AReport.ActivePassIndices[I]);
  Result := Result + ':' + GraphTraceSignatureHex(AReport.TranscriptHash)
    + ':' + NegotiationFingerprint(AReport.Search);
end;

procedure CheckTraceValid(const AGraph: TGraph;
  const AReport: TGraphSolveReport; const AContext: String);
var
  LValidation: TGraphTraceValidationReport;
begin
  Check(AReport.TraceCaptured, AContext + ' captures a trace');
  Check(AReport.TraceHash = CalculateGraphTraceHash(AReport),
    AContext + ' trace hash recomputes');
  Check(ValidateGraphTrace(AGraph, AReport, LValidation),
    AContext + ' trace validates independently');
  Check(LValidation.CheckedEvents = Length(AReport.Trace),
    AContext + ' validator inspects every event');
end;

procedure CheckSearchTraces(const AGraph: TGraph;
  const AReport: TGraphNegotiationReport; const AContext: String);
var
  I: Integer;
begin
  for I := 0 to High(AReport.Attempts) do
    CheckTraceValid(AGraph, AReport.Attempts[I].SolveReport,
      AContext + ' rejected round ' + IntToStr(I));
  CheckTraceValid(AGraph, AReport.FinalReport,
    AContext + ' terminal round');
end;

procedure ConfigureRepairGraph(const AGraph: TGraph);
begin
  AGraph.Seed := 0;
  AGraph.Reshape(1, 1, 1);
  AGraph.WrapNeighbors := False;

  AGraph.CurrentPass := PASS_TERRAIN;
  AGraph.PassMode := gpmOverlay;
  AGraph.ClearDependencies;
  AGraph.AddValue(TERRAIN_MEADOW);
  AGraph.AddValue(TERRAIN_RIDGE);

  AGraph.SwitchToPass(PASS_CLIMATE);
  AGraph.PassMode := gpmOverlay;
  AGraph.ClearDependencies;
  AGraph.AddValue(CLIMATE_SUN);
  AGraph.AddValue(CLIMATE_RAIN);

  AGraph.SwitchToPass(PASS_ROADS);
  AGraph.PassMode := gpmOverlay;
  AGraph.ClearDependencies.DependsOn(PASS_TERRAIN);
  AGraph.AddValue(ROADS_TRAIL);
  AGraph.AddValue(ROADS_PLAZA);

  AGraph.SwitchToPass(PASS_HOUSING);
  AGraph.PassMode := gpmOverlay;
  AGraph.ClearDependencies;
  AGraph.AddValue(HOUSING_HOME)
    .RequireFromPass(PASS_ROADS, ROADS_TRAIL);
  AGraph.AddValue(HOUSING_MARKET)
    .RequireFromPass(PASS_ROADS, ROADS_PLAZA);

  AGraph.SwitchToPass(PASS_DECOR);
  AGraph.PassMode := gpmOverlay;
  AGraph.ClearDependencies;
  AGraph.AddValue(DECOR_GARDEN)
    .RequireFromPass(PASS_HOUSING, HOUSING_HOME)
    .RequireFromPass(PASS_CLIMATE, CLIMATE_SUN);
  AGraph.AddValue(DECOR_PORCH)
    .RequireFromPass(PASS_HOUSING, HOUSING_HOME)
    .RequireFromPass(PASS_CLIMATE, CLIMATE_RAIN);
  AGraph.AddValue(DECOR_MARKET_DAY)
    .RequireFromPass(PASS_HOUSING, HOUSING_MARKET)
    .RequireFromPass(PASS_CLIMATE, CLIMATE_SUN);
  AGraph.AddValue(DECOR_MARKET_LIGHTS)
    .RequireFromPass(PASS_HOUSING, HOUSING_MARKET)
    .RequireFromPass(PASS_CLIMATE, CLIMATE_RAIN);
end;

function NewRepairGraph: TGraph;
begin
  Result := TGraph.Create;
  try
    ConfigureRepairGraph(Result);
  except
    Result.Free;
    raise;
  end;
end;

function CaptureRepairValues(const AGraph: TGraph): TRepairValues;
begin
  Result.Terrain := AGraph.PassGraph[0].Entry[0, 0, 0].Value;
  Result.Climate := AGraph.PassGraph[1].Entry[0, 0, 0].Value;
  Result.Roads := AGraph.PassGraph[2].Entry[0, 0, 0].Value;
  Result.Housing := AGraph.PassGraph[3].Entry[0, 0, 0].Value;
  Result.Decor := AGraph.PassGraph[4].Entry[0, 0, 0].Value;
end;

function ExpectedDecor(const AHousing, AClimate: String): String;
begin
  if (AHousing = HOUSING_HOME) and (AClimate = CLIMATE_SUN) then
    Exit(DECOR_GARDEN);
  if (AHousing = HOUSING_HOME) and (AClimate = CLIMATE_RAIN) then
    Exit(DECOR_PORCH);
  if (AHousing = HOUSING_MARKET) and (AClimate = CLIMATE_SUN) then
    Exit(DECOR_MARKET_DAY);
  if (AHousing = HOUSING_MARKET) and (AClimate = CLIMATE_RAIN) then
    Exit(DECOR_MARKET_LIGHTS);
  Result := '';
end;

function OpposingHousing(const ARoads: String): String;
begin
  if ARoads = ROADS_TRAIL then
    Exit(HOUSING_MARKET);
  if ARoads = ROADS_PLAZA then
    Exit(HOUSING_HOME);
  raise Exception.Create('unknown road value in repair fixture');
end;

procedure PrepareRepair(const AGraph: TGraph;
  out ABaseline: TRepairValues; out ATargetHousing: String);
var
  LReport: TGraphSolveReport;
begin
  if not AGraph.TrySolve(CapturedSolveOptions(4), LReport) then
    raise Exception.Create('repair fixture baseline failed');
  ABaseline := CaptureRepairValues(AGraph);
  ATargetHousing := OpposingHousing(ABaseline.Roads);
  AGraph.PassGraph[3].SetAllowedValues(0, 0, 0, ATargetHousing);
  AGraph.SwitchToPass(PASS_DECOR);
end;

function RepairValuesValid(const AValues: TRepairValues): Boolean;
begin
  Result := (((AValues.Roads = ROADS_TRAIL)
      and (AValues.Housing = HOUSING_HOME))
    or ((AValues.Roads = ROADS_PLAZA)
      and (AValues.Housing = HOUSING_MARKET)))
    and (AValues.Decor = ExpectedDecor(AValues.Housing,
      AValues.Climate));
end;

function RegisteredValueAt(const AGraph: TGraph;
  const APassIndex, AValueIndex: Integer): String;
var
  LValues: TGraphValues;
begin
  LValues := AGraph.PassGraph[APassIndex].CopyRegisteredValues;
  if (AValueIndex < 0) or (AValueIndex >= Length(LValues)) then
    Exit('');
  Result := LValues[AValueIndex];
end;

function FailureIsHousingFromRoads(
  const AReport: TGraphSolveReport): Boolean;
begin
  Result := (AReport.Status = gssContradiction)
    and (AReport.FailedPassIndex = 3)
    and (AReport.Contradiction.Kind = gckPassDependency)
    and (AReport.Contradiction.PassIndex = 3)
    and (AReport.Contradiction.EntryIndex = 0)
    and (AReport.Contradiction.DependencyPassIndex = 2);
end;

procedure TestPublicContractAndLeafHorizon;
var
  I: Integer;
  LBaseline, LControlBaseline: TRepairValues;
  LControl: TGraph;
  LGraph: TGraph;
  LOptions: TGraphNegotiationOptions;
  LOrdinary: TGraphSolveReport;
  LReport: TGraphSelectiveNegotiationReport;
  LState: String;
  LTarget, LControlTarget: String;
begin
  Check(WFC_SELECTIVE_NEGOTIATION_ALGORITHM_VERSION = 1,
    'selective negotiation algorithm version is one');
  Check(WFC_SELECTIVE_NEGOTIATION_HASH_VERSION = 1,
    'selective negotiation hash version is one');

  LOptions := NegotiationOptions(4, 4);
  LGraph := NewRepairGraph;
  LControl := NewRepairGraph;
  try
    PrepareRepair(LGraph, LBaseline, LTarget);
    PrepareRepair(LControl, LControlBaseline, LControlTarget);
    Check(ValuesEqual(LBaseline, LControlBaseline)
        and (LTarget = LControlTarget),
      'leaf fixture and untouched control share one replay state');
    LState := GraphState(LGraph);
    Check(not LGraph.TryRegenerateNegotiatedFrom(PASS_HOUSING,
        LOptions, LReport),
      'leaf-only negotiation cannot reopen immutable roads');
    Check((LReport.ScopeAlgorithmVersion =
          WFC_SELECTIVE_NEGOTIATION_ALGORITHM_VERSION)
        and IndicesEqual(LReport.RequestedRootIndices, [3])
        and IndicesEqual(LReport.ActivePassIndices, [3, 4]),
      'leaf report publishes its exact requested and descendant scope');
    Check((LReport.Search.Status = gnsContradiction)
        and (LReport.Search.PassBacktracks = 0)
        and (Length(LReport.Search.Attempts) = 0),
      'leaf search stops without an active completed choice frame');
    Check(FailureIsHousingFromRoads(LReport.Search.FinalReport),
      'leaf failure identifies housing and immutable roads');
    Check((LReport.Search.FinalReport.Passes[0].Disposition = gpdReused)
        and (LReport.Search.FinalReport.Passes[1].Disposition = gpdReused)
        and (LReport.Search.FinalReport.Passes[2].Disposition = gpdReused)
        and (LReport.Search.FinalReport.Passes[3].Disposition = gpdFailed)
        and (LReport.Search.FinalReport.Passes[4].Disposition = gpdNotRun),
      'leaf report distinguishes clean, failed, and not-run passes');
    Check(GraphState(LGraph) = LState,
      'failed leaf search restores entries, domains, and selection');
    for I := 0 to 4 do
      Check(LGraph.PassGraph[I].RandomIndex(1000003) =
          LControl.PassGraph[I].RandomIndex(1000003),
        'failed leaf search restores pass RNG ' + IntToStr(I));
    Check((LReport.Search.TranscriptHash =
          CalculateGraphNegotiationTranscriptHash(LOptions, LReport.Search))
        and (LReport.TranscriptHash =
          CalculateGraphSelectiveNegotiationTranscriptHash(LOptions,
            LReport)),
      'leaf nested and scoped transcripts recompute');
    Check(LReport.TranscriptHash = EXPECTED_LEAF_TRANSCRIPT,
      'leaf selective transcript matches its portable golden');
    CheckSearchTraces(LGraph, LReport.Search, 'leaf search');
  finally
    LControl.Free;
    LGraph.Free;
  end;

  LGraph := NewRepairGraph;
  LControl := NewRepairGraph;
  try
    PrepareRepair(LGraph, LBaseline, LTarget);
    PrepareRepair(LControl, LControlBaseline, LControlTarget);
    Check(not LGraph.TryRegenerateFrom(PASS_HOUSING,
        LOptions.SolveOptions, LOrdinary),
      'ordinary leaf regeneration fails on immutable roads');
    Check(not LControl.TryRegenerateNegotiatedFrom(PASS_HOUSING,
        LOptions, LReport),
      'negotiated leaf regeneration has the same terminal failure');
    Check(SolveFingerprint(LOrdinary) =
        SolveFingerprint(LReport.Search.FinalReport),
      'leaf negotiation terminal report equals ordinary selective solving');
  finally
    LControl.Free;
    LGraph.Free;
  end;
end;

procedure TestRoadsHorizonSuccess;
var
  LBaseline, LControlBaseline, LRepaired: TRepairValues;
  LControl: TGraph;
  LGraph: TGraph;
  LOptions: TGraphNegotiationOptions;
  LReport: TGraphSelectiveNegotiationReport;
  LTarget, LControlTarget: String;
begin
  LOptions := NegotiationOptions(1, 1);
  LGraph := NewRepairGraph;
  LControl := NewRepairGraph;
  try
    PrepareRepair(LGraph, LBaseline, LTarget);
    PrepareRepair(LControl, LControlBaseline, LControlTarget);
    Check(LGraph.TryRegenerateNegotiatedFrom(PASS_ROADS,
        LOptions, LReport),
      'roads horizon reopens the provider and repairs housing');
    Check((LReport.Search.Status = gnsSolved)
        and (LReport.Search.PassBacktracks = 1)
        and (Length(LReport.Search.Attempts) = 1),
      'roads horizon retains one rejected round');
    Check(IndicesEqual(LReport.RequestedRootIndices, [2])
        and IndicesEqual(LReport.ActivePassIndices, [2, 3, 4]),
      'roads horizon contains only roads and its descendants');
    Check((LReport.Search.Attempts[0].BacktrackedPassIndex = 2)
        and (LReport.Search.Attempts[0].BacktrackedExecutionOrdinal = 0)
        and (Length(LReport.Search.Attempts[0].ExcludedAssignment) = 1),
      'roads horizon excludes one exact active-provider assignment');
    Check(FailureIsHousingFromRoads(
        LReport.Search.Attempts[0].SolveReport),
      'roads rejected round retains the housing dependency failure');
    Check(RegisteredValueAt(LGraph, 2,
        LReport.Search.Attempts[0].ExcludedAssignment[0]) =
          LBaseline.Roads,
      'roads exclusion decodes to the committed baseline road');
    Check(ExecutionOrderEqual(LReport.Search.FinalReport, [2, 3, 4]),
      'roads terminal round executes the exact active closure');
    Check((LReport.Search.FinalReport.Passes[0].Disposition = gpdReused)
        and (LReport.Search.FinalReport.Passes[1].Disposition = gpdReused)
        and (LReport.Search.FinalReport.Passes[2].Disposition = gpdSolved)
        and (LReport.Search.FinalReport.Passes[3].Disposition = gpdSolved)
        and (LReport.Search.FinalReport.Passes[4].Disposition = gpdSolved),
      'roads terminal report preserves reused providers and active join');
    LRepaired := CaptureRepairValues(LGraph);
    Check((LRepaired.Terrain = LBaseline.Terrain)
        and (LRepaired.Climate = LBaseline.Climate),
      'terrain and independent climate remain byte-for-byte values');
    Check((LRepaired.Roads <> LBaseline.Roads)
        and (LRepaired.Housing = LTarget)
        and RepairValuesValid(LRepaired),
      'roads, housing, and joined decoration commit one valid repair');
    Check(LGraph.CurrentPassIndex = 4,
      'successful repair restores the caller-selected pass');
    Check(LGraph.PassGraph[0].RandomIndex(1000003) =
        LControl.PassGraph[0].RandomIndex(1000003),
      'successful repair preserves terrain RNG');
    Check(LGraph.PassGraph[1].RandomIndex(1000003) =
        LControl.PassGraph[1].RandomIndex(1000003),
      'successful repair preserves independent climate RNG');
    Check((LReport.Search.TranscriptHash =
          EXPECTED_ROADS_SEARCH_TRANSCRIPT)
        and (LReport.TranscriptHash = EXPECTED_ROADS_TRANSCRIPT),
      'roads nested and scoped transcripts match portable goldens');
    Check((LReport.Search.TranscriptHash =
          CalculateGraphNegotiationTranscriptHash(LOptions, LReport.Search))
        and (LReport.TranscriptHash =
          CalculateGraphSelectiveNegotiationTranscriptHash(LOptions,
            LReport)),
      'roads nested and scoped transcripts recompute');
    CheckSearchTraces(LGraph, LReport.Search, 'roads repair');
  finally
    LControl.Free;
    LGraph.Free;
  end;
end;

procedure TestCanonicalRootsAndHashCoverage;
var
  LBaseline: TRepairValues;
  LFirst, LSecond, LSingle, LDuplicate: TGraph;
  LFirstReport, LSecondReport, LSingleReport,
    LDuplicateReport: TGraphSelectiveNegotiationReport;
  LOptions: TGraphNegotiationOptions;
  LOriginal: Integer;
  LTarget: String;
begin
  LOptions := NegotiationOptions(1, 1);
  LFirst := NewRepairGraph;
  LSecond := NewRepairGraph;
  LSingle := NewRepairGraph;
  LDuplicate := NewRepairGraph;
  try
    PrepareRepair(LFirst, LBaseline, LTarget);
    PrepareRepair(LSecond, LBaseline, LTarget);
    PrepareRepair(LSingle, LBaseline, LTarget);
    PrepareRepair(LDuplicate, LBaseline, LTarget);
    Check(LFirst.TryRegenerateNegotiatedFrom(
        LabelsOf([PASS_HOUSING, PASS_ROADS, PASS_HOUSING]),
        LOptions, LFirstReport),
      'reversed duplicate roots solve');
    Check(LSecond.TryRegenerateNegotiatedFrom(
        LabelsOf([PASS_ROADS, PASS_HOUSING]),
        LOptions, LSecondReport),
      'canonical ordered roots solve');
    Check(IndicesEqual(LFirstReport.RequestedRootIndices, [2, 3])
        and IndicesEqual(LFirstReport.ActivePassIndices, [2, 3, 4]),
      'root labels canonicalize by stable index and close descendants');
    Check(SelectiveFingerprint(LFirstReport) =
        SelectiveFingerprint(LSecondReport),
      'duplicate and reordered roots have one canonical transcript');

    Check(LSingle.TryRegenerateNegotiatedFrom(PASS_ROADS,
        LOptions, LSingleReport)
      and LDuplicate.TryRegenerateNegotiatedFrom(
        LabelsOf([PASS_ROADS, PASS_ROADS, PASS_ROADS]),
        LOptions, LDuplicateReport),
      'single and duplicate-only roots both solve');
    Check(SelectiveFingerprint(LSingleReport) =
        SelectiveFingerprint(LDuplicateReport),
      'repeating one root does not change scope or transcript');
    Check(NegotiationFingerprint(LFirstReport.Search) =
        NegotiationFingerprint(LSingleReport.Search),
      'redundant requested roots do not change the nested search');
    Check(LFirstReport.TranscriptHash <> LSingleReport.TranscriptHash,
      'scoped hash distinguishes different canonical requested-root sets');

    LOriginal := LSingleReport.ActivePassIndices[0];
    LSingleReport.ActivePassIndices[0] := 1;
    Check(CalculateGraphSelectiveNegotiationTranscriptHash(LOptions,
        LSingleReport) <> EXPECTED_ROADS_TRANSCRIPT,
      'scoped hash covers every active pass index');
    LSingleReport.ActivePassIndices[0] := LOriginal;
    LSingleReport.Search.TranscriptHash := 0;
    Check(CalculateGraphSelectiveNegotiationTranscriptHash(LOptions,
        LSingleReport) = EXPECTED_ROADS_TRANSCRIPT,
      'scoped hash recomputes nested evidence instead of trusting its hash');
  finally
    LDuplicate.Free;
    LSingle.Free;
    LSecond.Free;
    LFirst.Free;
  end;
end;

procedure TestBudgetsAndAtomicFailure;
var
  LBaseline: TRepairValues;
  LGraph, LOrdinaryGraph: TGraph;
  LOptions: TGraphNegotiationOptions;
  LOrdinary: TGraphSolveReport;
  LReport: TGraphSelectiveNegotiationReport;
  LState, LTarget: String;
begin
  LGraph := NewRepairGraph;
  LOrdinaryGraph := NewRepairGraph;
  try
    PrepareRepair(LGraph, LBaseline, LTarget);
    PrepareRepair(LOrdinaryGraph, LBaseline, LTarget);
    LState := GraphState(LGraph);
    LOptions := NegotiationOptions(0, 1);
    Check(not LOrdinaryGraph.TryRegenerateFrom(PASS_ROADS,
        LOptions.SolveOptions, LOrdinary),
      'ordinary roads regeneration retains its one-way failure');
    Check(not LGraph.TryRegenerateNegotiatedFrom(PASS_ROADS,
        LOptions, LReport),
      'zero pass budget stops before reopening roads');
    Check((LReport.Search.Status = gnsPassBacktrackLimit)
        and (LReport.Search.PassBacktracks = 0)
        and (Length(LReport.Search.Attempts) = 0),
      'zero pass budget reports its distinct outer limit');
    Check(SolveFingerprint(LReport.Search.FinalReport) =
        SolveFingerprint(LOrdinary),
      'zero-budget terminal report is ordinary selective parity');
    Check(GraphState(LGraph) = LState,
      'zero-budget failure is atomic');
  finally
    LOrdinaryGraph.Free;
    LGraph.Free;
  end;

  LGraph := NewRepairGraph;
  try
    PrepareRepair(LGraph, LBaseline, LTarget);
    LState := GraphState(LGraph);
    LOptions := NegotiationOptions(1, 0);
    Check(not LGraph.TryRegenerateNegotiatedFrom(PASS_ROADS,
        LOptions, LReport),
      'zero local budget cannot escape the exact roads exclusion');
    Check((LReport.Search.Status = gnsSolverBacktrackLimit)
        and (LReport.Search.PassBacktracks = 1)
        and (Length(LReport.Search.Attempts) = 1)
        and (LReport.Search.FinalReport.Status = gssBacktrackLimit),
      'local exhaustion remains distinct from the pass budget');
    Check(GraphState(LGraph) = LState,
      'local-limit failure restores all committed state');
    Check((not LGraph.PassGraph[2].HasAllowedValues(0, 0, 0))
        and LGraph.PassGraph[3].HasAllowedValues(0, 0, 0),
      'internal exclusions do not leak into caller domains');
    CheckSearchTraces(LGraph, LReport.Search, 'local-limit repair');
  finally
    LGraph.Free;
  end;
end;

procedure TestCallerLocks;
var
  LBaseline, LRepaired: TRepairValues;
  LGraph: TGraph;
  LOptions: TGraphNegotiationOptions;
  LReport: TGraphSelectiveNegotiationReport;
  LRoad, LState, LTarget: String;
begin
  LOptions := NegotiationOptions(4, 4);
  LGraph := NewRepairGraph;
  try
    PrepareRepair(LGraph, LBaseline, LTarget);
    LRoad := LBaseline.Roads;
    LGraph.PassGraph[2].Entry[0, 0, 0].Value := LRoad;
    LState := GraphState(LGraph);
    Check(not LGraph.TryRegenerateNegotiatedFrom(PASS_ROADS,
        LOptions, LReport),
      'a caller-locked road is never negotiated away');
    Check((LReport.Search.Status = gnsContradiction)
        and (LReport.Search.PassBacktracks = 0)
        and (Length(LReport.Search.Attempts) = 0),
      'locked-only roads do not become a choice frame');
    Check(GraphState(LGraph) = LState,
      'failed locked-road repair restores every pass');
    Check((LGraph.PassGraph[2].Entry[0, 0, 0].Value = LRoad)
        and (not LGraph.PassGraph[2].Entry[0, 0, 0].Generated),
      'the caller retains ownership of the locked road');
  finally
    LGraph.Free;
  end;

  LGraph := NewRepairGraph;
  try
    PrepareRepair(LGraph, LBaseline, LTarget);
    LGraph.PassGraph[0].Entry[0, 0, 0].Value := LBaseline.Terrain;
    Check(LGraph.TryRegenerateNegotiatedFrom(PASS_ROADS,
        NegotiationOptions(1, 1), LReport),
      'an immutable caller lock outside the horizon permits repair');
    LRepaired := CaptureRepairValues(LGraph);
    Check((LRepaired.Terrain = LBaseline.Terrain)
        and (not LGraph.PassGraph[0].Entry[0, 0, 0].Generated)
        and RepairValuesValid(LRepaired),
      'successful repair preserves the caller-owned terrain lock');
  finally
    LGraph.Free;
  end;
end;

function NewContextJoinFixture: TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Seed := 0;
    Result.Reshape(1, 1, 1);
    Result.WrapNeighbors := False;

    Result.CurrentPass := 'stable';
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('S');
    Result.Entry[0, 0, 0].Value := 'S';

    Result.SwitchToPass('A');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    //Pass-one seed zero chooses the second registered value, so this order
    //makes A1 the initial prefix and leaves A2 as its repair alternative.
    Result.AddValue('A2');
    Result.AddValue('A1');

    Result.SwitchToPass('B');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    //Pass-two seed zero chooses the first registered value. B is therefore
    //enumerated B1, B2 under A1, exhausted, and restarted at B1 under A2.
    Result.AddValue('B1');
    Result.AddValue('B2');

    Result.SwitchToPass('C');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('C')
      .RequireFromPass('A', 'A2')
      .RequireFromPass('B', 'B1');
  except
    Result.Free;
    raise;
  end;
end;

procedure TestContextualExclusionClearing;
var
  I: Integer;
  LGraph: TGraph;
  LOptions: TGraphNegotiationOptions;
  LReport: TGraphSelectiveNegotiationReport;
  LRoots: TGraphPassLabels;
  LPath: String;
begin
  LGraph := NewContextJoinFixture;
  try
    LOptions := NegotiationOptions(8, 8);
    LRoots := LabelsOf(['B', 'A']);
    Check(LGraph.TryRegenerateNegotiatedFrom(LRoots, LOptions, LReport),
      'independent active providers solve their downstream join');
    Check(IndicesEqual(LReport.RequestedRootIndices, [1, 2])
        and IndicesEqual(LReport.ActivePassIndices, [1, 2, 3]),
      'join scope canonicalizes both roots and includes the consumer');
    LPath := '';
    for I := 0 to High(LReport.Search.Attempts) do
      LPath := LPath + IntToStr(
        LReport.Search.Attempts[I].BacktrackedPassIndex);
    WriteLn('  [INFO] contextual join path: ', LPath,
      ' transcript=', GraphTraceSignatureHex(LReport.TranscriptHash));
    Check((Length(LReport.Search.Attempts) = 3)
        and (LReport.Search.Attempts[0].BacktrackedPassIndex = 2)
        and (LReport.Search.Attempts[1].BacktrackedPassIndex = 2)
        and (LReport.Search.Attempts[2].BacktrackedPassIndex = 1),
      'join exhausts B before reopening A chronologically');
    Check((LGraph.PassGraph[0].Entry[0, 0, 0].Value = 'S')
        and (not LGraph.PassGraph[0].Entry[0, 0, 0].Generated)
        and (LGraph.PassGraph[1].Entry[0, 0, 0].Value = 'A2')
        and (LGraph.PassGraph[2].Entry[0, 0, 0].Value = 'B1')
        and (LGraph.PassGraph[3].Entry[0, 0, 0].Value = 'C'),
      'join clears stale B exclusions in the new A context');
    Check(LReport.Search.FinalReport.Passes[0].Disposition = gpdReused,
      'join keeps the stable pass outside the repair horizon');
    Check((LReport.Search.TranscriptHash =
          CalculateGraphNegotiationTranscriptHash(LOptions, LReport.Search))
        and (LReport.TranscriptHash =
          CalculateGraphSelectiveNegotiationTranscriptHash(LOptions,
            LReport)),
      'contextual join transcripts recompute');
    Check(LReport.TranscriptHash = EXPECTED_CONTEXT_JOIN_TRANSCRIPT,
      'contextual join matches its portable transcript golden');
    CheckSearchTraces(LGraph, LReport.Search, 'contextual join');
  finally
    LGraph.Free;
  end;
end;

procedure TestMalformedInputsAndForwarding;
var
  LBaseline, LForwardBaseline, LRootBaseline: TRepairValues;
  LControl: TGraph;
  LGraph: TGraph;
  LOptions: TGraphNegotiationOptions;
  LRaised: Boolean;
  LReport: TGraphSelectiveNegotiationReport;
  LForwardReport, LRootReport: TGraphSelectiveNegotiationReport;
  LRoots: TGraphPassLabels;
  LState, LTarget, LControlTarget, LForwardTarget, LRootTarget: String;
begin
  LGraph := NewRepairGraph;
  LControl := NewRepairGraph;
  try
    PrepareRepair(LGraph, LBaseline, LTarget);
    PrepareRepair(LControl, LRootBaseline, LControlTarget);
    LState := GraphState(LGraph);
    LOptions := NegotiationOptions(1, 1);

    LRoots := nil;
    LRaised := False;
    try
      LGraph.TryRegenerateNegotiatedFrom(LRoots, LOptions, LReport);
    except
      on E: EArgumentException do LRaised := True;
    end;
    Check(LRaised, 'empty requested-root array is rejected');

    LRaised := False;
    try
      LGraph.TryRegenerateNegotiatedFrom('missing', LOptions, LReport);
    except
      on E: EArgumentException do LRaised := True;
    end;
    Check(LRaised, 'unknown requested root is rejected');

    LRaised := False;
    try
      LGraph.TryRegenerateNegotiatedFrom(
        LabelsOf([PASS_ROADS, 'missing']), LOptions, LReport);
    except
      on E: EArgumentException do LRaised := True;
    end;
    Check(LRaised, 'a bad later root is rejected atomically');

    LOptions.SolveOptions.MaxBacktracks := -1;
    LRaised := False;
    try
      LGraph.TryRegenerateNegotiatedFrom(PASS_ROADS, LOptions, LReport);
    except
      on E: ERangeError do LRaised := True;
    end;
    Check(LRaised, 'negative local budget is rejected before solving');
    LOptions := NegotiationOptions(-1, 1);
    LRaised := False;
    try
      LGraph.TryRegenerateNegotiatedFrom(PASS_ROADS, LOptions, LReport);
    except
      on E: ERangeError do LRaised := True;
    end;
    Check(LRaised, 'negative pass budget is rejected before solving');
    Check(GraphState(LGraph) = LState,
      'all malformed calls preserve entries, domains, and selection');
    Check(LGraph.PassGraph[0].RandomIndex(1000003) =
        LControl.PassGraph[0].RandomIndex(1000003),
      'malformed calls preserve terrain RNG');
    Check(LGraph.PassGraph[1].RandomIndex(1000003) =
        LControl.PassGraph[1].RandomIndex(1000003),
      'malformed calls preserve climate RNG');
  finally
    LControl.Free;
    LGraph.Free;
  end;

  LGraph := NewRepairGraph;
  LControl := NewRepairGraph;
  try
    PrepareRepair(LGraph, LForwardBaseline, LForwardTarget);
    PrepareRepair(LControl, LRootBaseline, LRootTarget);
    LOptions := NegotiationOptions(1, 1);
    Check(LGraph.PassGraph[4].TryRegenerateNegotiatedFrom(PASS_ROADS,
        LOptions, LForwardReport),
      'a pass graph forwards selective negotiation to its root');
    Check(LControl.TryRegenerateNegotiatedFrom(PASS_ROADS,
        LOptions, LRootReport),
      'the root control selective repair solves');
    Check(SelectiveFingerprint(LForwardReport) =
        SelectiveFingerprint(LRootReport),
      'pass-graph forwarding retains the exact report and transcript');
    Check(ValuesEqual(CaptureRepairValues(LGraph),
        CaptureRepairValues(LControl)),
      'pass-graph forwarding commits the same repair');
  finally
    LControl.Free;
    LGraph.Free;
  end;
end;

function NewOneCellFullNegotiationFixture: TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Seed := 0;
    Result.Reshape(1, 1, 1);
    Result.WrapNeighbors := False;
    Result.CurrentPass := 'terrain';
    Result.PassMode := gpmOverlay;
    Result.AddValue('marsh');
    Result.AddValue('meadow');
    Result.SwitchToPass('housing');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('cottage').RequireFromPass('terrain', 'meadow');
  except
    Result.Free;
    raise;
  end;
end;

procedure TestReplayFullBaselineAndIsolation;
var
  LBaseline, LFullBaseline, LFullValues, LFirstValues,
    LSecondValues: TRepairValues;
  LFirst, LSecond, LFull, LIsolation: TGraph;
  LFirstReport, LSecondReport: TGraphSelectiveNegotiationReport;
  LFullReport, LIsolationReport: TGraphNegotiationReport;
  LOptions: TGraphNegotiationOptions;
  LTarget, LFullTarget: String;
begin
  LOptions := NegotiationOptions(1, 1);
  LFirst := NewRepairGraph;
  LSecond := NewRepairGraph;
  LFull := NewRepairGraph;
  try
    PrepareRepair(LFirst, LBaseline, LTarget);
    PrepareRepair(LSecond, LBaseline, LTarget);
    PrepareRepair(LFull, LFullBaseline, LFullTarget);
    Check(LFirst.TryRegenerateNegotiatedFrom(PASS_ROADS,
        LOptions, LFirstReport)
      and LSecond.TryRegenerateNegotiatedFrom(PASS_ROADS,
        LOptions, LSecondReport),
      'independent same-seed selective repairs solve');
    LFirstValues := CaptureRepairValues(LFirst);
    LSecondValues := CaptureRepairValues(LSecond);
    Check(ValuesEqual(LFirstValues, LSecondValues)
        and (SelectiveFingerprint(LFirstReport) =
          SelectiveFingerprint(LSecondReport)),
      'independent selective repair replays output and transcript exactly');

    Check(LFull.TrySolveNegotiated(LOptions, LFullReport),
      'full-pipeline negotiated baseline solves the same request');
    LFullValues := CaptureRepairValues(LFull);
    Check(ValuesEqual(LFirstValues, LFullValues),
      'selective and full negotiation commit the same repaired district');
    Check(ExecutionOrderEqual(LFirstReport.Search.FinalReport, [2, 3, 4])
        and ExecutionOrderEqual(LFullReport.FinalReport, [0, 1, 2, 3, 4]),
      'selective and full search expose different active horizons');
    Check((LFirstReport.Search.Attempts[0].BacktrackedPassIndex = 2)
        and (LFullReport.Attempts[0].BacktrackedPassIndex = 2),
      'both horizons reopen roads for the same downstream request');
    Check(LFullReport.TranscriptHash = EXPECTED_FULL_REPAIR_TRANSCRIPT,
      'full repair baseline matches its portable transcript golden');
    Check(LFirstReport.TranscriptHash = EXPECTED_ROADS_TRANSCRIPT,
      'selective repair replay retains its portable transcript golden');
  finally
    LFull.Free;
    LSecond.Free;
    LFirst.Free;
  end;

  LIsolation := NewOneCellFullNegotiationFixture;
  try
    Check(LIsolation.TrySolveNegotiated(LOptions, LIsolationReport),
      'ordinary full Negotiation v1 still solves its original fixture');
    Check((LIsolationReport.TranscriptHash =
          EXPECTED_FULL_NEGOTIATION_V1_TRANSCRIPT)
        and (LIsolationReport.TranscriptHash =
          CalculateGraphNegotiationTranscriptHash(LOptions,
            LIsolationReport)),
      'selective API leaves the original Negotiation v1 golden isolated');
  finally
    LIsolation.Free;
  end;
end;

begin
  WriteLn('WFC selective negotiation conformance suite');
  WriteLn('===========================================');
  RunTest('public contract and immutable leaf horizon',
    @TestPublicContractAndLeafHorizon);
  RunTest('wider roads repair horizon', @TestRoadsHorizonSuccess);
  RunTest('canonical roots and scoped hash coverage',
    @TestCanonicalRootsAndHashCoverage);
  RunTest('separate budgets and atomic failure',
    @TestBudgetsAndAtomicFailure);
  RunTest('caller-owned locks', @TestCallerLocks);
  RunTest('contextual exclusion clearing at a join',
    @TestContextualExclusionClearing);
  RunTest('preflight and pass-graph forwarding',
    @TestMalformedInputsAndForwarding);
  RunTest('replay, full baseline, and Negotiation v1 isolation',
    @TestReplayFullBaselineAndIsolation);
  WriteLn('===========================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d selective negotiation checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
