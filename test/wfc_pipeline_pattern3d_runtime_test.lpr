{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_pipeline_pattern3d_runtime_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host, JS, Web,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_sequence, wfc_learn3d,
  wfc_pattern3d, wfc_pattern3d_learn, wfc_pattern3d_text,
  wfc_rule_model, wfc_rule_text, wfc_pipeline_model, wfc_pipeline_text,
  wfc_pipeline_compile, wfc_pipeline_run, wfc_pipeline_run_text,
  wfc_pipeline_result, wfc_pipeline_result_text, wfc_pipeline_runtime;

var Checks: Integer;

procedure Check(const Value: Boolean; const MessageText: String);
begin
  Inc(Checks);
  if not Value then raise Exception.Create(MessageText);
end;

function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(Values));
  for I := 0 to High(Values) do Result[I] := Values[I];
end;

function CheckerModel: TWfcOverlappingModel3D;
begin
  Result := LearnOverlappingModel3D(Tokens(['A','B','B','A','B','A','A','B']),
    2, 2, 2, 2, 2, 2, wmbWrap, wmsNone);
end;

function Recipe(const Model: TWfcOverlappingModel3D;
  const QuotaMaximum: Integer = -1; const ConnectivityMode: Integer = 0;
  const WithRequirement: Boolean = False): TWfcPipelineModel;
var Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies; Bridges: TWfcPipelineBridges;
  Quotas: TWfcPipelineValueQuotas; Connectivities: TWfcPipelineConnectivities;
  Profiles: TWfcPipelineConnectivityValues; Required: TGraphPositions;
  Root: TGraphPosition; Requirements: TWfcPipelineRequirements;
  Terms: TWfcPipelineRequirementTerms; Rules: TWfcRuleModel;
  Weights: TWfcModelIntegerArray;
begin
  Quotas := nil; Connectivities := nil; Requirements := nil;
  SetLength(Resources, 1);
  Resources[0] := MakeWfcPipelineResource('volume', wprkPattern3D,
    EncodeWfcPattern3DText(Model), 'authored volume fixture', 'MIT', '');
  SetLength(Passes, 4); SetLength(Dependencies, 3); SetLength(Bridges, 1);
  Passes[0] := MakeWfcPipelinePass('patterns', wppvPrivate, gpmOverlay,
    -1, wpakPattern3D, 0, False, wseWhole);
  Passes[1] := MakeWfcPipelinePass('voxels', wppvPublic, gpmOverlay,
    -1, wpakEmpty, -1, False, wseWhole);
  Passes[2] := MakeWfcPipelinePass('copy-one', wppvPublic, gpmTransform,
    1, wpakEmpty, -1, False, wseWhole);
  Passes[3] := MakeWfcPipelinePass('copy-two', wppvPublic, gpmTransform,
    2, wpakEmpty, -1, False, wseWhole);
  Dependencies[0] := MakeWfcPipelineDependency(1, 0);
  Dependencies[1] := MakeWfcPipelineDependency(2, 1);
  Dependencies[2] := MakeWfcPipelineDependency(3, 2);
  Bridges[0] := MakeWfcPipelineBridge(wpbkPattern3DProjection, 0, 1);
  if QuotaMaximum >= 0 then
  begin
    SetLength(Quotas, 1);
    Quotas[0] := MakeWfcPipelineValueQuota(3, 'A count', Tokens(['A']),
      QuotaMaximum, QuotaMaximum);
  end;
  if ConnectivityMode <> 0 then
  begin
    Root := Default(TGraphPosition); SetLength(Required, 1);
    Required[0] := Root; Required[0].Z := 1;
    SetLength(Profiles, 1);
    Profiles[0] := MakeWfcPipelineConnectivityValue('A',
      [gdNorth,gdEast,gdSouth,gdWest,gdUp,gdDown]);
    if ConnectivityMode = 1 then
    begin
      SetLength(Profiles, 2);
      Profiles[1] := MakeWfcPipelineConnectivityValue('B',
        [gdNorth,gdEast,gdSouth,gdWest,gdUp,gdDown]);
    end;
    SetLength(Connectivities, 1);
    Connectivities[0] := MakeWfcPipelineConnectivity(3, 'vertical route',
      Root, Required, Profiles, True);
  end;
  if WithRequirement then
  begin
    SetLength(Resources, 2); SetLength(Weights, 2); Weights[0] := 1; Weights[1] := 1;
    Rules := TWfcRuleModel.Create(3, Tokens(['A','B']), Weights, nil);
    try Resources[1] := MakeWfcPipelineResource('markers', wprkRules,
      EncodeWfcRuleText(Rules), 'authored marker fixture', 'MIT', '');
    finally Rules.Free; end;
    SetLength(Passes, 5); SetLength(Dependencies, 4);
    Passes[4] := MakeWfcPipelinePass('markers', wppvPublic, gpmOverlay,
      -1, wpakRules, 1, False, wseWhole);
    Dependencies[3] := MakeWfcPipelineDependency(4, 3);
    SetLength(Terms, 1);
    Terms[0] := MakeWfcPipelineRequirementTerm(0, 0, 1, Tokens(['A']));
    SetLength(Requirements, 1);
    Requirements[0] := MakeWfcPipelineRequirement(4, 'A', 3, wprqExact, Terms);
  end;
  Result := TWfcPipelineModel.Create(MakeWfcPipelineMetadata(
    'Volume runtime fixture', 'MIT', 'project-authored', ''), 3, True,
    rmBottomUp, Resources, Passes, Dependencies, Bridges, Requirements,
    Quotas, Connectivities);
end;

procedure CheckProjection(const Output: TWfcPipelineResult; const Phase: Integer);
var L, X, Y, Z, I: Integer; Layer: TWfcPipelineResultLayer; Expected: TWfcModelToken;
begin
  Check(Output.Status = wprsSolved, 'volume runtime must solve');
  Check((Output.Width = 2) and (Output.Height = 2) and (Output.Depth = 2),
    'full XYZ invocation survives');
  Check(Output.LayerCount >= 3, 'all public aliases are materialized');
  for L := 0 to 2 do
  begin
    Layer := Output.LayerAt(L);
    Check((Layer.PassIndex = L + 1) and (Length(Layer.Tokens) = 8),
      'private keys are omitted while every public volume is complete');
    for Z := 0 to 1 do for Y := 0 to 1 do for X := 0 to 1 do
    begin
      I := (Z * 2 + Y) * 2 + X;
      if ((X + Y + Z) mod 2) = Phase then Expected := 'A' else Expected := 'B';
      Check(Layer.Tokens[I] = Expected, 'independent XYZ checker and alias projection');
    end;
  end;
end;

procedure TestRoundTripAndReentry;
var Model: TWfcOverlappingModel3D; R, Reloaded: TWfcPipelineModel;
  Run, LoadedRun: TWfcPipelineRun; Runtime: TWfcPipelineRuntime;
  Output, Again, Replayed, Stored: TWfcPipelineResult;
  Locks: TWfcPipelineCellLocks; Domains: TWfcPipelineCellDomains;
  RecipeText, RunText, ResultText: String; Seed, Mode: Integer;
begin
  Model := CheckerModel;
  try R := Recipe(Model, 4, 1); finally Model.Free; end;
  try
    RecipeText := EncodeWfcPipelineModelText(R);
    Check(Pos('wfcpipeline=4'#10, RecipeText) = 1, 'volume recipe selects canonical version four');
    Reloaded := DecodeWfcPipelineModelText(RecipeText);
    try
      Check(EncodeWfcPipelineModelText(Reloaded) = RecipeText, 'canonical recipe roundtrip');
      SetLength(Locks, 1); SetLength(Domains, 2);
      Locks[0] := MakeWfcPipelineCellLock(3, 1, 1, 1, 'A');
      Domains[0] := MakeWfcPipelineCellDomain(1, 1, 0, 1, Tokens(['A','B']));
      Domains[1] := MakeWfcPipelineCellDomain(2, 0, 1, 1, Tokens(['B']));
      for Seed := 0 to 7 do for Mode := 0 to 1 do
      begin
        Run := TWfcPipelineRun.Create(R, 2, 2, 2, Seed,
          TWfcPipelineSolveStrategy(Mode), 64, Mode * 16, True, Locks, Domains);
        try
          Runtime := TWfcPipelineRuntime.Create(R, Run);
          try
            Output := Runtime.Execute;
            try
              CheckProjection(Output, 1);
              ResultText := EncodeWfcPipelineResultText(Output);
              Again := Runtime.Execute;
              try Check(EncodeWfcPipelineResultText(Again) = ResultText,
                'same runtime reentry resets random streams and exact evidence');
              finally Again.Free; end;
              RunText := EncodeWfcPipelineRunText(Run);
              LoadedRun := DecodeWfcPipelineRunText(RunText, Reloaded);
              try
                Replayed := ExecuteWfcPipeline(Reloaded, LoadedRun);
                try Check(EncodeWfcPipelineResultText(Replayed) = ResultText,
                  'fresh recipe and run replay identical public output and evidence');
                finally Replayed.Free; end;
                Stored := DecodeWfcPipelineResultText(ResultText, Reloaded, LoadedRun);
                try Check(EncodeWfcPipelineResultText(Stored) = ResultText,
                  'stored result validates complete public quota and connectivity');
                finally Stored.Free; end;
              finally LoadedRun.Free; end;
            finally Output.Free; end;
          finally Runtime.Free; end;
        finally Run.Free; end;
      end;
    finally Reloaded.Free; end;
  finally R.Free; end;
end;

procedure TestInverseConflictsAndPolicy;
var Model: TWfcOverlappingModel3D; R: TWfcPipelineModel; Run: TWfcPipelineRun;
  Output: TWfcPipelineResult; Domains: TWfcPipelineCellDomains;
  Failure: TWfcPipelineFailure; Mode: Integer;
begin
  Model := CheckerModel;
  try
    for Mode := 0 to 3 do
    begin
      if Mode = 2 then R := Recipe(Model, 3)
      else if Mode = 3 then R := Recipe(Model, -1, 2)
      else R := Recipe(Model);
      try
        Domains := nil;
        if Mode = 0 then
        begin
          SetLength(Domains, 2);
          Domains[0] := MakeWfcPipelineCellDomain(2, 1, 1, 1, Tokens(['A']));
          Domains[1] := MakeWfcPipelineCellDomain(3, 1, 1, 1, Tokens(['B']));
        end
        else if Mode = 1 then
        begin
          SetLength(Domains, 1);
          Domains[0] := MakeWfcPipelineCellDomain(3, 0, 0, 1, nil);
        end;
        Run := TWfcPipelineRun.Create(R, 2, 2, 2, 4, wpssOneWay, 64, 0,
          True, nil, Domains);
        try
          Output := ExecuteWfcPipeline(R, Run);
          try
            Failure := Output.CopyFailure;
            Check((Output.Status = wprsContradiction) and (Output.LayerCount = 0),
              'incompatible volume policy publishes no partial layers');
            Check(Failure.PassIndex = 0, 'public policy constrains unresolved private patterns');
            if Mode < 2 then Check(Failure.Kind = gckEntryDomain,
              'alias AND and explicit empty domains fail in private domain');
          finally Output.Free; end;
        finally Run.Free; end;
      finally R.Free; end;
    end;
  finally Model.Free; end;
end;

procedure TestRequirementsAndCommitRollback;
var Model: TWfcOverlappingModel3D; R: TWfcPipelineModel; Run: TWfcPipelineRun;
  Locks: TWfcPipelineCellLocks; Output: TWfcPipelineResult;
  C: TWfcCompiledPipeline; Solve: TGraphSolveReport; Options: TGraphSolveOptions;
  Values: TGraphValues; Selective: TGraphSelectiveNegotiationReport;
  Negotiation: TGraphNegotiationOptions; X, Y, Z, I: Integer; Before: TGraphValue;
begin
  Model := CheckerModel;
  try R := Recipe(Model, -1, 0, True); finally Model.Free; end;
  try
    SetLength(Locks, 2);
    Locks[0] := MakeWfcPipelineCellLock(3, 0, 0, 1, 'A');
    Locks[1] := MakeWfcPipelineCellLock(4, 0, 0, 0, 'A');
    Run := TWfcPipelineRun.Create(R, 2, 2, 2, 4, wpssOneWay, 64, 0, True, Locks, nil);
    try
      Output := ExecuteWfcPipeline(R, Run);
      try
        CheckProjection(Output, 1);
        Check(Output.LayerAt(3).Tokens[0] = 'A', 'exact cross-pass positive-Z requirement is honored');
      finally Output.Free; end;
    finally Run.Free; end;
  finally R.Free; end;
  Model := CheckerModel;
  try R := Recipe(Model); finally Model.Free; end;
  try
    Options := DefaultGraphSolveOptions; Options.MaxBacktracks := 64;
    C := CompileWfcPipeline(R, 2, 2, 2);
    try
      C.Graph.Seed := 4;
      Check(C.Graph.TrySolve(Options, Solve), 'baseline compiled full volume solves');
      Before := C.Graph.PassGraph[0].Entry[1,1,1].Value;
      Negotiation := DefaultGraphNegotiationOptions;
      Check(C.Graph.TryRegenerateNegotiatedFrom('voxels', Negotiation, Selective),
        'selective public regeneration validates reused private full volume');
      Check(C.Graph.PassGraph[0].Entry[1,1,1].Value = Before,
        'selective public regeneration preserves private frontier');
    finally C.Free; end;
    C := CompileWfcPipeline(R, 2, 2, 2);
    try
      C.Graph.SwitchToPass(1); C.Graph.AddValue('rogue');
      for Z := 0 to 1 do for Y := 0 to 1 do for X := 0 to 1 do
        C.Graph.SetAllowedValues(X, Y, Z, ['A','B']);
      C.Graph.SetAllowedValues(1, 1, 1, 'rogue');
      Check(not C.Graph.TrySolve(Options, Solve), 'tampered public Z slice is rejected before commit');
      Check((C.LastValidation.Kind = wpcvkPattern3DBridge) and
        (C.LastValidation.EntryIndex = 7) and (Solve.Contradiction.Kind = gckFinalValidation),
        'volume bridge reports exact X-fast XYZ cell inside rollback boundary: kind=' +
        IntToStr(Ord(C.LastValidation.Kind)) + ', entry=' +
        IntToStr(C.LastValidation.EntryIndex) + ', solve=' +
        IntToStr(Ord(Solve.Contradiction.Kind)));
      for Z := 0 to 1 do for Y := 0 to 1 do for X := 0 to 1 do
        Check(C.Graph.PassGraph[1].Entry[X,Y,Z].Empty, 'failed public volume commit restores complete snapshot');
    finally C.Free; end;
    C := CompileWfcPipeline(R, 2, 2, 2);
    try
      Values := C.Graph.PassGraph[0].CopyRegisteredValues;
      for I := 0 to High(Values) do C.Graph.PassGraph[0].Rules[Values[I]].Rules := nil;
      C.Graph.SwitchToPass(1); C.Graph.AddValue('rogue');
      for Z := 0 to 1 do for Y := 0 to 1 do for X := 0 to 1 do
      begin
        C.Graph.PassGraph[0].Entry[X,Y,Z].Value := Values[0];
        C.Graph.SetAllowedValues(X,Y,Z,'rogue');
      end;
      Check(not C.Graph.TrySolve(Options, Solve), 'invalid prefilled private volume cannot bypass validation');
      Check(C.LastValidation.Kind = wpcvkPattern3DPass,
        'private full-volume semantic failure has independent typed attribution');
    finally C.Free; end;
  finally R.Free; end;
end;

procedure TestInverseVolumeBudget;
var Samples: TWfcLearnVolumeSamples; Model: TWfcOverlappingModel3D;
  R: TWfcPipelineModel; Run: TWfcPipelineRun; Runtime: TWfcPipelineRuntime;
  Domains: TWfcPipelineCellDomains; I: Integer; Raised: Boolean;
begin
  SetLength(Samples, 2);
  Samples[0] := MakeLearnSample3D(Tokens(['A']), 1, 1, 1);
  Samples[1] := MakeLearnSample3D(Tokens(['B']), 1, 1, 1);
  Model := LearnOverlappingModel3DCorpus(Samples, 16, 16, 16, wmbWrap, wmsNone);
  try R := Recipe(Model); finally Model.Free; end;
  try
    SetLength(Domains, 257);
    for I := 0 to High(Domains) do
      Domains[I] := MakeWfcPipelineCellDomain(3, I, 0, 0, Tokens(['A']));
    Run := TWfcPipelineRun.Create(R, 257, 1, 1, 4, wpssOneWay, 64, 0, False, nil, Domains);
    try
      Runtime := nil; Raised := False;
      try
        try Runtime := TWfcPipelineRuntime.Create(R, Run);
        except on E: EWfcPipelineRuntime do
          Raised := Pos('inverse bridge contribution count', E.Message) > 0; end;
      finally Runtime.Free; end;
      Check(Raised, 'XYZ inverse expansion applies shared contribution budget before graph allocation');
    finally Run.Free; end;
  finally R.Free; end;
end;

procedure TestZSelfOverlapAndMultipleBridges;
var Samples: TWfcLearnVolumeSamples; Model: TWfcOverlappingModel3D;
  R, Forked: TWfcPipelineModel; Run: TWfcPipelineRun;
  Output: TWfcPipelineResult; Locks: TWfcPipelineCellLocks;
  Domains: TWfcPipelineCellDomains; Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies; Bridges: TWfcPipelineBridges;
  Failure: TWfcPipelineFailure;
begin
  SetLength(Samples, 2);
  Samples[0] := MakeLearnSample3D(Tokens(['B']), 1, 1, 1);
  Samples[1] := MakeLearnSample3D(Tokens(['A','B']), 1, 1, 2);
  Model := LearnOverlappingModel3DCorpus(Samples, 1, 1, 2, wmbWrap, wmsNone);
  try R := Recipe(Model); finally Model.Free; end;
  try
    Run := TWfcPipelineRun.Create(R, 1, 1, 1, 4, wpssOneWay, 64, 0, False, nil, nil);
    try
      Output := ExecuteWfcPipeline(R, Run);
      try Check((Output.Status = wprsSolved) and (Output.LayerAt(0).Tokens[0] = 'B'),
        'wrapped one-voxel baseline retains the all-B realization');
      finally Output.Free; end;
    finally Run.Free; end;
    SetLength(Locks, 1); Locks[0] := MakeWfcPipelineCellLock(3, 0, 0, 0, 'A');
    Run := TWfcPipelineRun.Create(R, 1, 1, 1, 4, wpssOneWay, 64, 0, False, Locks, nil);
    try
      Output := ExecuteWfcPipeline(R, Run);
      try
        Failure := Output.CopyFailure;
        Check((Output.Status = wprsContradiction) and (Failure.PassIndex = 0) and
          (Failure.Kind = gckEntryDomain),
          'distinct Z offsets aliasing one private cell are all intersected before solve');
      finally Output.Free; end;
    finally Run.Free; end;
    Passes := R.CopyPasses; Dependencies := R.CopyDependencies; Bridges := R.CopyBridges;
    Passes[2] := MakeWfcPipelinePass('copy-one', wppvPublic, gpmOverlay,
      -1, wpakEmpty, -1, False, wseWhole);
    Dependencies[1] := MakeWfcPipelineDependency(2, 0);
    SetLength(Bridges, 2); Bridges[1] := MakeWfcPipelineBridge(wpbkPattern3DProjection, 0, 2);
    Forked := TWfcPipelineModel.Create(R.CopyMetadata, 3, True, rmBottomUp,
      R.CopyResources, Passes, Dependencies, Bridges, nil);
    try
      SetLength(Domains, 2);
      Domains[0] := MakeWfcPipelineCellDomain(1, 0, 0, 0, Tokens(['A']));
      Domains[1] := MakeWfcPipelineCellDomain(3, 0, 0, 0, Tokens(['B']));
      Run := TWfcPipelineRun.Create(Forked, 1, 1, 2, 4, wpssOneWay, 64, 0, False, nil, Domains);
      try
        Output := ExecuteWfcPipeline(Forked, Run);
        try
          Failure := Output.CopyFailure;
          Check((Output.Status = wprsContradiction) and (Failure.PassIndex = 0) and
            (Failure.Kind = gckEntryDomain),
            'separate public bridges and a transform alias intersect on their shared latent owner');
        finally Output.Free; end;
      finally Run.Free; end;
    finally Forked.Free; end;
  finally R.Free; end;
end;

begin
  try
    TestRoundTripAndReentry;
    TestInverseConflictsAndPolicy;
    TestRequirementsAndCommitRollback;
    TestInverseVolumeBudget;
    TestZSelfOverlapAndMultipleBridges;
    WriteLn('Volume pipeline runtime checks: ', Checks);
  except
    on E: Exception do
    begin
      {$IFDEF PAS2JS}document.body.setAttribute('data-self-test-message', E.Message);{$ENDIF}
      WriteLn('FAIL: ', E.ClassName, ': ', E.Message); Halt(1);
    end;
  end;
end.
