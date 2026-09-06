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
program wfc_pipeline_value_quota_test;
{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  Classes, SysUtils, wfc, wfc_model, wfc_model_text, wfc_rule_model, wfc_rule_text,
  wfc_pattern2d, wfc_pattern2d_learn, wfc_pattern2d_text,
  wfc_sequence, wfc_sequence_learn, wfc_sequence_text,
  wfc_pipeline_model, wfc_pipeline_text, wfc_pipeline_compile,
  wfc_pipeline_run, wfc_pipeline_run_text, wfc_pipeline_runtime,
  wfc_pipeline_result, wfc_pipeline_result_text,
  wfc_run_app, wfc_validate_app;

type
  TFixtureKind = (fkRules, fkModel, fkSequence, fkPattern, fkEmptyAnchor);
  TGraphValidationAccess = class(TGraph)
    class function Validate(const G: TGraph; out P, E: Integer): Boolean;
  end;

var Checks: Integer;

procedure Check(const Condition: Boolean; const MessageText: String);
begin
  Inc(Checks);
  if not Condition then raise Exception.Create(MessageText);
end;

class function TGraphValidationAccess.Validate(const G: TGraph;
  out P, E: Integer): Boolean;
begin
  Result := TGraphValidationAccess(G).DoValidateCommit(P, E);
end;

function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(Values));
  for I := 0 to High(Values) do Result[I] := Values[I];
end;

function Integers(const Values: array of Integer): TWfcModelIntegerArray;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(Values));
  for I := 0 to High(Values) do Result[I] := Values[I];
end;

function Quota(const Pass: Integer; const LabelText: TWfcModelToken;
  const Values: array of TWfcModelToken; const Minimum, Maximum: Integer):
  TWfcPipelineValueQuota;
begin
  Result := MakeWfcPipelineValueQuota(Pass, LabelText, Tokens(Values),
    Minimum, Maximum);
end;

function OneQuota(const Pass: Integer; const Minimum, Maximum: Integer):
  TWfcPipelineValueQuotas;
begin
  Result := nil; SetLength(Result, 1);
  Result[0] := Quota(Pass, 'accepted-A', ['A'], Minimum, Maximum);
end;

function Fixture(const Kind: TFixtureKind;
  const Quotas: TWfcPipelineValueQuotas;
  const BridgeVersion: Integer = 2): TWfcPipelineModel;
var Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies; Bridges: TWfcPipelineBridges;
  Versions: TWfcPipelineVersions; Model: TWfcModel; Rules: TWfcRuleModel;
  SequenceModel: TWfcSequenceModel; Pattern: TWfcOverlappingModel2D;
  Samples: TWfcSequenceSamples; Shapes: TWfcModelSampleShapes;
  Patterns: TWfcPattern2DPayloads; Relations: TWfcModelIntegerArray;
  DocumentText: String; ResourceKind: TWfcPipelineResourceKind;
  Adapter: TWfcPipelineAdapterKind; Rank, I: Integer; Projected: Boolean;
begin
  Rank := 1; Projected := Kind in [fkSequence, fkPattern, fkEmptyAnchor];
  ResourceKind := wprkRules; Adapter := wpakRules;
  case Kind of
    fkRules:
      begin
        Rules := TWfcRuleModel.Create(1, Tokens(['A', 'B', 'C']),
          Integers([1, 1, 1]), nil);
        try DocumentText := EncodeWfcRuleText(Rules); finally Rules.Free; end;
      end;
    fkModel:
      begin
        SetLength(Relations, 36);
        for I := 0 to High(Relations) do
          if (I div 9 = Ord(wmdEast)) or (I div 9 = Ord(wmdWest)) then
            Relations[I] := 1 else Relations[I] := 0;
        Model := TWfcModel.Create(1, 3, 1, wmbOpen, wmsNone,
          [wmdEast, wmdWest], Tokens(['A', 'B', 'C']),
          Integers([1, 1, 1]), Relations);
        try DocumentText := EncodeWfcModelText(Model); finally Model.Free; end;
        ResourceKind := wprkModel; Adapter := wpakModel;
      end;
    fkSequence:
      begin
        SetLength(Samples, 2);
        Samples[0] := MakeWfcSequenceSample(Tokens(['A', 'B', 'A']));
        Samples[1] := MakeWfcSequenceSample(Tokens(['B', 'B', 'B']));
        SequenceModel := LearnSequenceModelCorpus(Samples, 2);
        try DocumentText := EncodeWfcSequenceText(SequenceModel);
        finally SequenceModel.Free; end;
        ResourceKind := wprkSequence; Adapter := wpakSequence;
      end;
    fkPattern, fkEmptyAnchor:
      begin
        Rank := 2;
        if Kind = fkPattern then
          Pattern := LearnOverlappingModel2D(Tokens(['A', 'B', 'B', 'A']),
            2, 2, 2, 2, wmbWrap, wmsNone)
        else
        begin
          SetLength(Shapes, 1); Shapes[0] := MakeWfcModelSampleShape(2, 1);
          SetLength(Patterns, 1); SetLength(Patterns[0], 2);
          Patterns[0][0] := 1; Patterns[0][1] := 0;
          Pattern := TWfcOverlappingModel2D.Create(2, 1, wmbWrap, wmsNone,
            Shapes, Tokens(['A', 'B']), Patterns, Integers([2]));
        end;
        try DocumentText := EncodeWfcPattern2DText(Pattern);
        finally Pattern.Free; end;
        ResourceKind := wprkPattern2D; Adapter := wpakPattern2D;
      end;
  end;
  SetLength(Resources, 1);
  Resources[0] := MakeWfcPipelineResource('source-model', ResourceKind,
    DocumentText, 'project-authored quota fixture', 'MIT', 'quota-fixture:1');
  if Projected then
  begin
    SetLength(Passes, 4); SetLength(Dependencies, 3); SetLength(Bridges, 2);
    Passes[0] := MakeWfcPipelinePass('private-source', wppvPrivate,
      gpmOverlay, -1, Adapter, 0, Kind = fkSequence, wseWhole);
    Passes[1] := MakeWfcPipelinePass('public-one', wppvPublic,
      gpmOverlay, -1, wpakEmpty, -1, False, wseWhole);
    Passes[2] := MakeWfcPipelinePass('public-two', wppvPublic,
      gpmOverlay, -1, wpakEmpty, -1, False, wseWhole);
    Passes[3] := MakeWfcPipelinePass('public-copy', wppvPublic,
      gpmTransform, 1, wpakEmpty, -1, False, wseWhole);
    Dependencies[0] := MakeWfcPipelineDependency(1, 0);
    Dependencies[1] := MakeWfcPipelineDependency(2, 0);
    Dependencies[2] := MakeWfcPipelineDependency(3, 1);
    for I := 0 to 1 do
      if Kind = fkSequence then
        Bridges[I] := MakeWfcPipelineBridge(wpbkSequenceProjection, 0, I + 1)
      else Bridges[I] := MakeWfcPipelineBridge(wpbkPattern2DProjection, 0, I + 1);
  end
  else
  begin
    SetLength(Passes, 3); SetLength(Dependencies, 2);
    Passes[0] := MakeWfcPipelinePass('source', wppvPublic,
      gpmOverlay, -1, Adapter, 0, False, wseWhole);
    Passes[1] := MakeWfcPipelinePass('copy-one', wppvPublic,
      gpmTransform, 0, wpakEmpty, -1, False, wseWhole);
    Passes[2] := MakeWfcPipelinePass('copy-two', wppvPublic,
      gpmTransform, 1, wpakEmpty, -1, False, wseWhole);
    Dependencies[0] := MakeWfcPipelineDependency(1, 0);
    Dependencies[1] := MakeWfcPipelineDependency(2, 1);
  end;
  Versions := CurrentWfcPipelineVersions;
  Versions.SequenceBridgeVersion := BridgeVersion;
  Versions.Pattern2DBridgeVersion := BridgeVersion;
  Result := TWfcPipelineModel.Create(MakeWfcPipelineMetadata(
    'Portable value quotas', 'MIT', 'independent integration fixture',
    'pipeline-value-quotas:1'), Versions, Rank, Rank = 2, rmBottomUp,
    Resources, Passes, Dependencies, Bridges, nil, Quotas);
end;

function Contains(const Values: TWfcModelTokens; const Token: TWfcModelToken): Boolean;
var I: Integer;
begin
  for I := 0 to High(Values) do if Values[I] = Token then Exit(True);
  Result := False;
end;

function Count(const Values, Accepted: TWfcModelTokens): Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to High(Values) do if Contains(Accepted, Values[I]) then Inc(Result);
end;

procedure Recount(const Model: TWfcPipelineModel; const Output: TWfcPipelineResult);
var Q, L, N: Integer; Descriptor: TWfcPipelineValueQuota;
  Layer: TWfcPipelineResultLayer; Found: Boolean;
begin
  Check(Output.Status = wprsSolved, 'recount requires a solved result');
  for Q := 0 to Model.ValueQuotaCount - 1 do
  begin
    Descriptor := Model.ValueQuotaAt(Q); Found := False;
    for L := 0 to Output.LayerCount - 1 do
    begin
      Layer := Output.LayerAt(L);
      if Layer.PassIndex <> Descriptor.PassIndex then Continue;
      Found := True; N := Count(Layer.Tokens, Descriptor.Values);
      Check((N >= Descriptor.MinimumCount) and (N <= Descriptor.MaximumCount),
        'independent public result recount satisfies quota ' + IntToStr(Q));
    end;
    Check(Found, 'quota owner is present among detached public layers');
  end;
end;

procedure RunAndReplay(const Model: TWfcPipelineModel; const Width, Height: Integer;
  const Strategy: TWfcPipelineSolveStrategy; const ExpectedSolved: Boolean;
  const Domains: TWfcPipelineCellDomains = nil);
var Run, RunCopy: TWfcPipelineRun; Runtime: TWfcPipelineRuntime;
  Output, Replay, Decoded: TWfcPipelineResult; RecipeCopy: TWfcPipelineModel;
  TextValue: String; PassBudget: Integer;
begin
  if Strategy = wpssNegotiated then PassBudget := 8 else PassBudget := 0;
  Run := TWfcPipelineRun.Create(Model, Width, Height, 1, 17,
    Strategy, 64, PassBudget, True, nil, Domains);
  try
    Runtime := TWfcPipelineRuntime.Create(Model, Run);
    try
      Output := Runtime.Execute;
      try
        Check((Output.Status = wprsSolved) = ExpectedSolved,
          'portable runtime has the expected quota feasibility');
        if ExpectedSolved then Recount(Model, Output)
        else Check(Output.LayerCount = 0, 'failed quota run publishes no partial public layers');
        Replay := Runtime.Execute;
        try
          Check(EncodeWfcPipelineResultText(Replay) = EncodeWfcPipelineResultText(Output),
            'same prepared runtime replays complete result, counters and evidence');
        finally Replay.Free; end;
        TextValue := EncodeWfcPipelineResultText(Output);
        Decoded := DecodeWfcPipelineResultText(TextValue, Model, Run);
        try Check(EncodeWfcPipelineResultText(Decoded) = TextValue,
          'solved or failed quota result has an exact canonical round trip');
        finally Decoded.Free; end;
        RecipeCopy := DecodeWfcPipelineModelText(EncodeWfcPipelineModelText(Model));
        try
          RunCopy := DecodeWfcPipelineRunText(EncodeWfcPipelineRunText(Run), RecipeCopy);
          try
            Replay := ExecuteWfcPipeline(RecipeCopy, RunCopy);
            try Check(EncodeWfcPipelineResultText(Replay) = TextValue,
              'decoded recipe and run preserve quota replay provenance');
            finally Replay.Free; end;
          finally RunCopy.Free; end;
        finally RecipeCopy.Free; end;
      finally Output.Free; end;
    finally Runtime.Free; end;
  finally Run.Free; end;
end;

procedure TestDirectAndAliases;
var Kind: TFixtureKind; Model: TWfcPipelineModel; Q: TWfcPipelineValueQuotas;
  C: TWfcCompiledPipeline; D: TWfcPipelineCellDomains; Strategy: TWfcPipelineSolveStrategy;
begin
  for Kind := fkRules to fkModel do
  begin
    SetLength(Q, 4);
    Q[0] := Quota(0, 'same-label', ['A'], 1, 3);
    Q[1] := Quota(2, 'same-label', ['A'], 2, 2);
    Q[2] := Quota(1, 'overlapping-set', ['A', 'B'], 3, 3);
    Q[3] := Quota(0, 'second-owner-quota', ['B'], 1, 1);
    Model := Fixture(Kind, Q);
    try
      C := CompileWfcPipeline(Model, 4, 1, 1);
      try
        Check(Length(C.Graph.PassGraph[0].CopyValueQuotaConstraints) = 4,
          'direct and alias quotas conjoin on one materialized owner without label collision');
        Check((not C.Graph.PassGraph[1].HasDefinition) and
          (Length(C.Graph.PassGraph[2].CopyRegisteredValues) = 0),
          'quota lowering does not materialize definitionless transforms');
      finally C.Free; end;
      for Strategy := wpssOneWay to wpssNegotiated do
        RunAndReplay(Model, 4, 1, Strategy, True);
      SetLength(D, 1); D[0] := MakeWfcPipelineCellDomain(2, 0, 0, 0, nil);
      RunAndReplay(Model, 4, 1, wpssOneWay, False, D);
    finally Model.Free; end;
  end;
  Model := Fixture(fkRules, OneQuota(2, 0, 0));
  try RunAndReplay(Model, 4, 1, wpssOneWay, True); finally Model.Free; end;
  Model := Fixture(fkRules, OneQuota(0, 0, 100));
  try RunAndReplay(Model, 4, 1, wpssOneWay, True); finally Model.Free; end;
  Model := Fixture(fkRules, OneQuota(0, 5, 100));
  try RunAndReplay(Model, 4, 1, wpssOneWay, False); finally Model.Free; end;
end;

procedure CheckPrivateProjection(const Model: TWfcPipelineModel;
  const Width, Height: Integer; const SequenceKind: Boolean);
var C: TWfcCompiledPipeline; R: TGraphSolveReport; X, Y, I, N, PublicCount: Integer;
  Values: TGraphValues; ExpectedToken: TWfcModelToken;
  Core: TGraphValueQuotaConstraints; Included: Boolean; J: Integer;
begin
  C := CompileWfcPipeline(Model, Width, Height, 1);
  try
    Values := C.Graph.PassGraph[0].CopyRegisteredValues;
    Core := C.Graph.PassGraph[0].CopyValueQuotaConstraints;
    Check(Length(Core) = Model.ValueQuotaCount,
      'each projected descriptor has exactly one private quota');
    for I := 0 to High(Values) do
    begin
      if SequenceKind then ExpectedToken := Model.BorrowSequenceResource(0).ProjectStateToken(I)
      else ExpectedToken := Model.BorrowPattern2DResource(0).PaletteTokenAt(
        Model.BorrowPattern2DResource(0).PatternPaletteIndexAt(I, 0, 0));
      Included := False;
      for J := 0 to High(Core[0].Values) do
        Included := Included or (Core[0].Values[J] = Values[I]);
      Check(Included = (ExpectedToken = 'A'),
        'private quota membership exactly matches public emission, not merely candidate count');
    end;
    C.Graph.Seed := 17;
    Check(C.Graph.TrySolve(DefaultGraphSolveOptions, R),
      'lowered quota permits direct compiler one-way solve');
    N := 0; PublicCount := 0;
    for Y := 0 to Height - 1 do for X := 0 to Width - 1 do
    begin
      I := 0;
      while (I < Length(Values)) and
        (Values[I] <> C.Graph.PassGraph[0].Entry[X, Y, 0].Value) do Inc(I);
      Check(I < Length(Values), 'private solved value has a registered candidate index');
      if SequenceKind then ExpectedToken := Model.BorrowSequenceResource(0).ProjectStateToken(I)
      else ExpectedToken := Model.BorrowPattern2DResource(0).PaletteTokenAt(
        Model.BorrowPattern2DResource(0).PatternPaletteIndexAt(I, 0, 0));
      Check(ExpectedToken = TWfcModelToken(C.Graph.PassGraph[1].Entry[X, Y, 0].Value),
        'independent private emission equals the public token at the same cell');
      if ExpectedToken = 'A' then Inc(N);
      if C.Graph.PassGraph[3].Entry[X, Y, 0].Value = 'A' then Inc(PublicCount);
    end;
    Check((N = 2) and (PublicCount = N),
      'whole-pass public quota counts cells once, not private histories or pattern footprint slots');
  finally C.Free; end;
end;

procedure TestProjectionLowering;
var Model: TWfcPipelineModel; Q: TWfcPipelineValueQuotas;
  C: TWfcCompiledPipeline; Core: TGraphValueQuotaConstraints;
  V, I, StateCount: Integer; Strategy: TWfcPipelineSolveStrategy;
  Rejected: Boolean; D: TWfcPipelineCellDomains;
begin
  SetLength(Q, 2);
  Q[0] := Quota(2, 'same-label', ['A'], 2, 2);
  Q[1] := Quota(3, 'same-label', ['A'], 1, 2);
  for V := 1 to 2 do
  begin
    Model := Fixture(fkSequence, Q, V);
    try
      C := CompileWfcPipeline(Model, 3, 1, 1);
      try
        Core := C.Graph.PassGraph[0].CopyValueQuotaConstraints;
        Check(Length(Core) = 2, 'two sequence public owners lower independent same-label quotas');
        StateCount := 0;
        for I := 0 to Model.BorrowSequenceResource(0).StateCount - 1 do
          if Model.BorrowSequenceResource(0).ProjectStateToken(I) = 'A' then Inc(StateCount);
        Check((StateCount > 1) and (Length(Core[0].Values) = StateCount) and
          (Length(Core[1].Values) = StateCount),
          'sequence inverse quota includes every duplicate-emission history state');
      finally C.Free; end;
      CheckPrivateProjection(Model, 3, 1, True);
      for Strategy := wpssOneWay to wpssNegotiated do
        RunAndReplay(Model, 3, 1, Strategy, True);
      if V = 2 then
      begin
        SetLength(D, 1);
        D[0] := MakeWfcPipelineCellDomain(3, 0, 0, 0, Tokens(['A']));
        RunAndReplay(Model, 3, 1, wpssOneWay, True, D);
        D[0] := MakeWfcPipelineCellDomain(3, 0, 0, 0, Tokens(['B']));
        RunAndReplay(Model, 3, 1, wpssOneWay, False, D);
      end;
    finally Model.Free; end;
    Model := Fixture(fkPattern, Q, V);
    try
      CheckPrivateProjection(Model, 2, 2, False);
      RunAndReplay(Model, 2, 2, wpssOneWay, True);
    finally Model.Free; end;
  end;
  Q[1] := Quota(3, 'same-label', ['A'], 0, 0);
  Model := Fixture(fkSequence, Q);
  try RunAndReplay(Model, 3, 1, wpssOneWay, False); finally Model.Free; end;
  Model := Fixture(fkSequence, OneQuota(3, 0, 0));
  try RunAndReplay(Model, 3, 1, wpssOneWay, True); finally Model.Free; end;
  for V := 0 to 1 do
  begin
    Model := Fixture(fkEmptyAnchor, OneQuota(1, V, 1));
    try
      C := nil; Rejected := False;
      try C := CompileWfcPipeline(Model, 2, 1, 1);
      except on E: EWfcPipelineCompile do Rejected := E.Stage = wpcsBridges; end;
      C.Free;
      Check(Rejected, 'quota bounds do not weaken existing rejection of a palette with no anchor alternative');
    finally Model.Free; end;
  end;
end;

function GraphState(const G: TGraph): String;
var P, X, Y, I: Integer; E: TGraphEntry; Allowed: TGraphValues;
begin
  Result := IntToStr(G.CurrentPassIndex) + '|';
  for P := 0 to G.TotalPassCount - 1 do
    for Y := 0 to Integer(G.Dimension.Height) - 1 do
      for X := 0 to Integer(G.Dimension.Width) - 1 do
      begin
        E := G.PassGraph[P].Entry[X, Y, 0];
        Result := Result + E.Value + ':' + IntToStr(Ord(E.Empty)) + ':' +
          IntToStr(Ord(E.Generated)) + ':' +
          IntToStr(Ord(G.PassGraph[P].HasAllowedValues(X, Y, 0))) + '[';
        Allowed := G.PassGraph[P].CopyAllowedValues(X, Y, 0);
        for I := 0 to High(Allowed) do Result := Result + Allowed[I] + ',';
        Result := Result + '];';
      end;
end;

procedure TestIndependentRollbackAndRepair;
var Model: TWfcPipelineModel; C, Twin: TWfcCompiledPipeline;
  R: TGraphSolveReport; Before: String; X, P, FailedPass, FailedEntry: Integer;
  Raised: Boolean; Q: TWfcPipelineValueQuotas;
begin
  SetLength(Q, 3);
  Q[0] := Quota(2, 'loose-first', ['A'], 0, 4);
  Q[1] := Quota(0, 'interleaved-owner', ['B'], 0, 4);
  Q[2] := Quota(2, 'exact-last', ['A'], 2, 2);
  Model := Fixture(fkRules, Q);
  try
    C := CompileWfcPipeline(Model, 4, 1, 1);
    Twin := CompileWfcPipeline(Model, 4, 1, 1);
    try
      C.Graph.Seed := 91; Twin.Graph.Seed := 91;
      Check(C.Graph.TrySolve(DefaultGraphSolveOptions, R) and
        Twin.Graph.TrySolve(DefaultGraphSolveOptions, R), 'independent rollback twins solve');
      Check(GraphState(C.Graph) = GraphState(Twin.Graph), 'rollback twins start identically');
      for P := 0 to C.Graph.TotalPassCount - 1 do C.Graph.PassGraph[P].ClearValueQuotas;
      for X := 0 to 3 do C.Graph.PassGraph[0].SetAllowedValues(X, 0, 0, ['B', 'C']);
      Before := GraphState(C.Graph);
      Check(not C.Graph.TrySolve(DefaultGraphSolveOptions, R),
        'clearing every compiled core quota cannot bypass immutable recipe validation');
      Check((R.Contradiction.Kind = gckFinalValidation) and
        (C.LastValidation.Kind = wpcvkValueQuota) and
        (C.LastValidation.ValueQuotaIndex = 2) and
        (C.LastValidation.PassIndex = 2) and (C.LastValidation.EntryIndex = -1),
        'shared owner histogram preserves original global quota ordinal and public alias ownership');
      Check(R.Passes[0].Decisions > 0, 'rejected candidate exercised random source decisions');
      Check(GraphState(C.Graph) = Before, 'quota rejection restores values, generated flags, domains and selected pass');
      for P := 0 to 2 do for X := 0 to 2 do
        Check(C.Graph.PassGraph[P].RandomIndex(1000000) =
          Twin.Graph.PassGraph[P].RandomIndex(1000000),
          'independent commit failure restores every pass random stream');
    finally Twin.Free; C.Free; end;
    C := CompileWfcPipeline(Model, 4, 1, 1);
    try
      Check(C.Graph.TrySolve(DefaultGraphSolveOptions, R), 'selective repair baseline solves');
      Before := GraphState(C.Graph);
      Check(C.Graph.TryRegenerateFrom('copy-two', DefaultGraphSolveOptions, R),
        'selective copy repair accepts the preserved quota-bearing source');
      Check(GraphState(C.Graph) = Before, 'copy-only repair preserves the complete source composition');
      for X := 0 to 3 do C.Graph.PassGraph[0].Entry[X, 0, 0].Value := 'B';
      Before := GraphState(C.Graph); Raised := False;
      try C.Graph.TryRegenerateFrom('copy-two', DefaultGraphSolveOptions, R);
      except on E: EInvalidOperation do Raised := True; end;
      Check(Raised and (GraphState(C.Graph) = Before),
        'selective repair cannot silently reuse a source violating the lowered quota');
    finally C.Free; end;
  finally Model.Free; end;
  Model := Fixture(fkRules, OneQuota(0, 0, 4));
  try
    C := CompileWfcPipeline(Model, 4, 1, 1);
    try
      Check(not TGraphValidationAccess.Validate(C.Graph, FailedPass, FailedEntry),
        'independent quota recount rejects empty owner entries even with zero minimum');
      for P := 0 to 2 do for X := 0 to 3 do C.Graph.PassGraph[P].Entry[X, 0, 0].Value := 'alien';
      Check(not TGraphValidationAccess.Validate(C.Graph, FailedPass, FailedEntry),
        'independent quota recount rejects unknown public tokens, not merely nonmatches');
    finally C.Free; end;
  finally Model.Free; end;
end;

procedure TestResultForgeryAndCli;
var Model, Legacy: TWfcPipelineModel; Run: TWfcPipelineRun;
  Output, Forged: TWfcPipelineResult; Layers: TWfcPipelineResultLayers;
  Command: TWfcRunCommand; ValidateCommand: TWfcValidateCommand;
  RecipeText, RunText, StandardOutput, StandardError: String;
  I, Code: Integer; Raised: Boolean;
  Q: TWfcPipelineValueQuotas;
begin
  SetLength(Q, 2);
  Q[0] := Quota(2, 'loose-first', ['A'], 0, 4);
  Q[1] := Quota(2, 'exact-second', ['A'], 2, 2);
  Model := Fixture(fkRules, Q);
  try
    Run := TWfcPipelineRun.Create(Model, 4, 1, 1, 17, wpssOneWay,
      64, 0, True, nil, nil);
    try
      Output := ExecuteWfcPipeline(Model, Run);
      try
        Recount(Model, Output); Layers := Output.CopyLayers;
        for I := 0 to High(Layers[2].Tokens) do Layers[2].Tokens[I] := 'B';
        Forged := nil; Raised := False;
        try
          Forged := TWfcPipelineResult.Create(Model, Run, Output.CopyVersions,
            Output.Status, Output.PassBacktracks, Output.EvidenceKind,
            Output.EvidenceSignature, Output.CopyFailure,
            Output.CopyPassOutcomes, Layers);
        except on E: EWfcPipelineResult do Raised := True; end;
        Forged.Free;
        Check(Raised, 'shape-valid in-vocabulary forged solved layer cannot bypass recipe quota');
        RecipeText := EncodeWfcPipelineModelText(Model);
        RunText := EncodeWfcPipelineRunText(Run);
        Check(Pos('wfcpipeline=2'#10, RecipeText) = 1, 'quota recipe opts into format version two');
        ValidateCommand := Default(TWfcValidateCommand);
        ValidateCommand.Kind := wvckRecipe; ValidateCommand.OutputMode := wvomCanonical;
        Code := WfcValidateExecuteText(ValidateCommand, RecipeText, StandardOutput, StandardError);
        Check((Code = 0) and (StandardOutput = RecipeText) and (StandardError = ''),
          'portable CLI validates and emits exact canonical quota recipe');
        ValidateCommand.OutputMode := wvomSummary;
        Code := WfcValidateExecuteText(ValidateCommand, RecipeText, StandardOutput, StandardError);
        Check((Code = 0) and (StandardError = '') and
          (Pos('valid canonical wfcpipeline=2 ', StandardOutput) = 1) and
          (Pos('value-quotas=2', StandardOutput) > 0),
          'CLI summary advertises the actual recipe version and quota count');
        Command := Default(TWfcRunCommand); Command.Kind := wrckExecute;
        Command.OutputMode := wromCanonical;
        Code := WfcRunExecuteTexts(Command, RecipeText, RunText, StandardOutput, StandardError);
        Check((Code = 0) and (StandardError = '') and
          (StandardOutput = EncodeWfcPipelineResultText(Output)),
          'portable CLI executes quota recipe with exact canonical solved output');
      finally Output.Free; end;
    finally Run.Free; end;
  finally Model.Free; end;
  Model := Fixture(fkRules, OneQuota(0, 5, 5));
  try
    Run := TWfcPipelineRun.Create(Model, 4, 1, 1, 17, wpssOneWay,
      64, 0, True, nil, nil);
    try
      Output := ExecuteWfcPipeline(Model, Run);
      try
        Check((Output.Status = wprsContradiction) and
          (Output.CopyFailure.Kind = gckValueQuota), 'impossible whole-pass minimum is a normal quota contradiction');
        RecipeText := EncodeWfcPipelineModelText(Model); RunText := EncodeWfcPipelineRunText(Run);
        Code := WfcRunExecuteTexts(Command, RecipeText, RunText, StandardOutput, StandardError);
        Check((Code = WFC_RUN_EXIT_NOT_SOLVED) and (StandardError = '') and
          (StandardOutput = EncodeWfcPipelineResultText(Output)) and
          (Pos('value-quota', StandardOutput) > 0),
          'CLI emits canonical value-quota failure and exit four rather than internal error');
      finally Output.Free; end;
    finally Run.Free; end;
  finally Model.Free; end;
  Legacy := Fixture(fkRules, nil);
  try
    RecipeText := EncodeWfcPipelineModelText(Legacy);
    Check((Pos('wfcpipeline=1'#10, RecipeText) = 1) and
      (Pos(#10'value-quota', RecipeText) = 0), 'quota-free recipe remains exact version-one syntax');
    Model := TWfcPipelineModel.Create(Legacy.CopyMetadata, Legacy.CopyVersions,
      Legacy.Rank, Legacy.WrapNeighbors, Legacy.RunMode, Legacy.CopyResources,
      Legacy.CopyPasses, Legacy.CopyDependencies, Legacy.CopyBridges, Legacy.CopyRequirements);
    try Check((Model.Signature = Legacy.Signature) and
      (EncodeWfcPipelineModelText(Model) = RecipeText),
      'legacy constructor and explicit empty quota constructor preserve identical signatures and bytes');
    finally Model.Free; end;
  finally Legacy.Free; end;
end;

begin
  try
    TestDirectAndAliases;
    TestProjectionLowering;
    TestIndependentRollbackAndRepair;
    TestResultForgeryAndCli;
    WriteLn('Portable pipeline value-quota integration checks: ', Checks);
  except on E: Exception do begin WriteLn('FAIL: ', E.ClassName, ': ', E.Message); Halt(1); end; end;
end.
