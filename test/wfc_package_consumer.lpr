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
program wfc_package_consumer;

{$mode delphi}{$H+}

{ Build this separately against generated/installed package units only.
  Do not add the repository's src directory: that would conceal omissions
  in a package manifest by silently compiling the missing source unit. }
uses
  SysUtils, wfc, wfc_model, wfc_rule_model, wfc_rule_text, wfc_sequence,
  wfc_music_arrangement, wfc_music_form,
  wfc_pipeline_model, wfc_pipeline_text, wfc_pipeline_connectivity,
  wfc_pipeline_run, wfc_pipeline_run_text, wfc_pipeline_runtime,
  wfc_pipeline_result, wfc_pipeline_result_text, wfc_volume_symmetry,
  wfc_pattern3d, wfc_pattern3d_learn, wfc_pattern3d_text, wfc_pattern3d_graph,
  wfc_token_volume_view, wfc_voxel3d_isometric, wfc_voxel3d_svg, wfc_lattice;

var Checks: Integer;

procedure Check(const Condition: Boolean; const MessageText: String);
begin
  Inc(Checks);
  if not Condition then raise Exception.Create(MessageText);
end;

procedure UseGraph;
var Graph: TGraph; Report: TGraphSolveReport;
begin
  Graph := TGraph.Create;
  try
    Graph.Reshape(2, 1, 1);
    Graph.AddValue('route');
    Check(Graph.TrySolve(DefaultGraphSolveOptions, Report), 'installed TGraph solves');
    Check((Graph.Entry[0, 0, 0].Value = 'route') and
      (Graph.Entry[1, 0, 0].Value = 'route'), 'installed graph publishes complete values');
  finally Graph.Free; end;
end;

procedure UseLattice;
var Layout: TWfcLatticeLayout; Box: TWfcLatticeBox;
  Coverage: TWfcLatticeCoverage; Cell: TWfcLatticeVector;
begin
  Layout:=MakeWfcLatticeLayout(5,1,1,MakeWfcLatticeVector(-5,0,0),
    MakeWfcLatticeVector(2,1,1),True);
  Check((WFC_LATTICE_VERSION=1) and (WfcLatticeCellCount(Layout)=5),
    'installed lattice exposes its version and checked shape');
  Check(TryWfcLatticePoint(Layout,MakeWfcLatticeVector(-6,0,0),Cell)
    and (Cell.X=4),'installed lattice wraps exact negative-floor coordinates');
  Box.Minimum:=MakeWfcLatticeVector(-6,0,0);
  Box.Maximum:=MakeWfcLatticeVector(-4,1,1);
  Check(TryWfcLatticeCoverage(Layout,Box,Coverage),
    'installed lattice resolves a seam-crossing world box');
  Check((Coverage.X.IntervalCount=2) and
    (WfcLatticeCoverageCellCount(Coverage)=2),
    'installed lattice retains two unique lazy coverage intervals');
  Cell:=WfcLatticeCoverageCell(Coverage,0);
  Check(Cell.X=0,'installed lattice enumerates canonical first cell');
  Cell:=WfcLatticeCoverageCell(Coverage,1);
  Check(Cell.X=4,'installed lattice enumerates canonical seam cell');
end;

procedure UseMusicForm;
var Config, Copied: TWfcMusicFormConfig; Cursor: TWfcMusicFormCursor;
  Plan: TWfcMusicFormPhrasePlan; Bars: TWfcMusicFormBars;
  Report: TGraphNegotiationReport; Frontier: TWfcMusicFormFrontier;
  Failure: String; I: Integer;
begin
  Config := DefaultWfcMusicFormConfig(32, 4);
  SetLength(Config.Harmonies, 4); SetLength(Config.Realizations, 4);
  SetLength(Config.Gestures, 1);
  Config.Gestures[0].LabelText := 'theme';
  Config.Gestures[0].MotifIndex := Config.ThemeMotifIndex;
  Config.Gestures[0].Roles := [wmfrQuestion, wmfrAnswer, wmfrContrast, wmfrReturn];
  Config.Gestures[0].Cadences := [wmfcNone, wmfcHalf, wmfcAuthentic];
  Config.Gestures[0].AttackCount := 4;
  for I := 0 to 3 do
  begin
    Config.Harmonies[I].LabelText := 'intent-' + IntToStr(I);
    Config.Harmonies[I].HarmonicFunction := TWfcMusicFormFunction(I);
    SetLength(Config.Harmonies[I].MotionPitches, 1);
    Config.Harmonies[I].MotionPitches[0] := 10;
    Config.Realizations[I].HarmonyIndex := I;
    Config.Realizations[I].GestureIndex := 0;
    Config.Realizations[I].EntryPitch := 20;
    Config.Realizations[I].ExitPitch := 20;
  end;
  Copied := CopyWfcMusicFormConfig(Config);
  Config.Harmonies[0].MotionPitches[0] := 999;
  Check(Copied.Harmonies[0].MotionPitches[0] = 10,
    'installed music-form config copy owns its nested catalog');
  Cursor := TWfcMusicFormCursor.Create(Copied);
  Plan := nil;
  try
    Frontier := Cursor.CopyFrontier;
    Check(Cursor.Next(Plan, Report) = wmaspProduced, 'installed music-form planner produces a phrase');
    Check((WFC_MUSIC_FORM_VERSION = 1) and (Plan.BarCount = 4) and
      (Plan.PhraseIndex = 0) and
      (Plan.Seed = WfcMusicArrangementSectionSeed(Copied.Seed, 0)),
      'installed phrase retains version, bounded extent and seeded provenance');
    Check(ValidateWfcMusicFormPhrase(Copied, Frontier, Plan, Failure),
      'installed phrase passes independent validation: ' + Failure);
    Bars := Plan.CopyBars; Bars[0].HarmonyIndex := -1;
    Check(Plan.BarAt(0).HarmonyIndex >= 0, 'installed phrase copy is detached');
    Check((Cursor.Status = wmasCompleted) and (Cursor.CopyFrontier.NextBar = 4),
      'installed cursor publishes the completed frontier');
  finally Plan.Free; Cursor.Free; end;
end;

procedure UsePortableConnectivity;
var Values: TWfcModelTokens; Weights: TWfcModelIntegerArray; Rules: TWfcRuleModel;
  Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Connections, Copied: TWfcPipelineConnectivities;
  Profiles: TWfcPipelineConnectivityValues; Root: TGraphPosition;
  Required: TGraphPositions; Recipe, Decoded: TWfcPipelineModel;
  Run, DecodedRun: TWfcPipelineRun; Output, Replay: TWfcPipelineResult;
  RecipeText, RunText, ResultText: String; Failed, I: Integer;
begin
  SetLength(Values, 2); Values[0] := 'route'; Values[1] := 'empty';
  SetLength(Weights, 2); Weights[0] := 1; Weights[1] := 1;
  SetLength(Resources, 1);
  Rules := TWfcRuleModel.Create(1, Values, Weights, nil);
  try
    Resources[0] := MakeWfcPipelineResource('rules', wprkRules,
      EncodeWfcRuleText(Rules), 'Package consumer route vocabulary', 'MIT', '');
  finally Rules.Free; end;
  SetLength(Passes, 1);
  Passes[0] := MakeWfcPipelinePass('routes', wppvPublic, gpmOverlay, -1,
    wpakRules, 0, False, wseWhole);
  Root := Default(TGraphPosition); SetLength(Required, 1);
  Required[0] := Root; Required[0].X := 2;
  SetLength(Profiles, 1);
  Profiles[0] := MakeWfcPipelineConnectivityValue('route', [gdEast, gdWest]);
  SetLength(Connections, 1);
  Connections[0] := MakeWfcPipelineConnectivity(0, 'gate-to-gate', Root,
    Required, Profiles, True);
  Recipe := TWfcPipelineModel.Create(MakeWfcPipelineMetadata(
    'Installed connectivity consumer', 'MIT', '', ''), 1, False, rmBottomUp,
    Resources, Passes, nil, nil, nil, nil, Connections);
  try
    Connections[0].Values[0].Value := 'caller-mutated';
    Copied := Recipe.CopyConnectivities; Copied[0].Values[0].Value := 'copy-mutated';
    Check(Recipe.ConnectivityAt(0).Values[0].Value = 'route',
      'installed portable connectivity owns detached descriptor arrays');
    PreflightWfcPipelineConnectivity(Recipe, 3, 1, 1, Failed);
    Check(Failed = -1, 'installed connectivity helper accepts bounded invocation');
    Run := TWfcPipelineRun.Create(Recipe, 3, 1, 1, 4, wpssOneWay, 64, 0, True, nil, nil);
    try
      Output := ExecuteWfcPipeline(Recipe, Run);
      try
        Check((Output.Status = wprsSolved) and (Output.LayerCount = 1),
          'installed connectivity pipeline publishes a complete result');
        Values := Output.LayerAt(0).Tokens;
        for I := 0 to High(Values) do
          Check(Values[I] = 'route', 'installed pipeline connects every cell between both gates');
        Check(ValidateWfcPipelineConnectivity(Recipe, 0, 3, 1, 1, Values, Failed),
          'installed independent connectivity validator accepts public result');
        RecipeText := EncodeWfcPipelineModelText(Recipe);
        RunText := EncodeWfcPipelineRunText(Run);
        ResultText := EncodeWfcPipelineResultText(Output);
        Check(Pos('wfcpipeline=3'#10, RecipeText) = 1, 'installed codec exports version-three recipe');
        Decoded := DecodeWfcPipelineModelText(RecipeText);
        try
          DecodedRun := DecodeWfcPipelineRunText(RunText, Decoded);
          try
            Replay := ExecuteWfcPipeline(Decoded, DecodedRun);
            try Check(EncodeWfcPipelineResultText(Replay) = ResultText,
              'installed recipe/run codecs preserve exact deterministic replay');
            finally Replay.Free; end;
          finally DecodedRun.Free; end;
        finally Decoded.Free; end;
      finally Output.Free; end;
    finally Run.Free; end;
  finally Recipe.Free; end;
end;

procedure UseVolumePatterns;
var Tokens: TWfcModelTokens; Learned, Decoded: TWfcOverlappingModel3D;
  TextValue: String; Config: TWfcPattern3DPassConfig;
  Pipeline: TWfcPattern3DPassPipeline; Composition: TWfcPattern3DComposition;
  Report: TWfcPattern3DPassReport; Validation: TWfcOverlapping3DValidationReport;
  Projection: TWfcTokenGrid3D; I: Integer; Palette: TWfcModelTokens;
  ViewOptions: TWfcTokenVolumeViewOptions; Scene: TVoxel3DProjectedScene;
begin
  Check(WfcVolumeTransformCount(wmsCubeRotations) = 24,
    'installed cube symmetry helper exposes all proper rotations');
  SetLength(Tokens, 8);
  for I := 0 to High(Tokens) do Tokens[I] := 'solid';
  Learned := LearnOverlappingModel3D(Tokens, 2, 2, 2, 2, 2, 2,
    wmbWrap, wmsCubeRotations);
  try TextValue := EncodeWfcPattern3DText(Learned);
  finally Learned.Free; end;
  Decoded := DecodeWfcPattern3DText(TextValue);
  try
    Check((Pos('wfcp=2'#10'rank=3'#10, TextValue) = 1) and
      (Pos(#10'relations=overlap'#10, TextValue) > 0),
      'installed codec preserves compact full-volume serialization');
    Check((Decoded.PatternDepth = 2) and (Decoded.PatternWeightAt(0) = 192),
      'installed model owns exact XYZ payloads and raw augmented weights');
    Config.Width := 2; Config.Height := 2; Config.Depth := 2;
    Config.Seed := 55; Config.Model := Decoded;
    Pipeline := TWfcPattern3DPassPipeline.Create(Config);
    Composition := nil;
    try
      Pipeline.LockPublicCell(1, 1, 1, 'solid');
      Check(Pipeline.TryGenerate(Composition, Report),
        'installed 3D pass adapter solves a public constrained volume');
      Check(Pipeline.Validate(Composition, Validation),
        'installed 3D composition independently validates');
      Projection := Composition.CopyProjection;
      Check((Projection.Width = 2) and (Projection.Height = 2) and
        (Projection.Depth = 2) and (Length(Projection.Tokens) = 8),
        'installed adapter publishes the exact full XYZ projection');
      for I := 0 to High(Projection.Tokens) do
        Check(Projection.Tokens[I] = 'solid', 'installed projected voxel matches its authored token');
      SetLength(Palette, 1); Palette[0] := 'solid';
      ViewOptions := DefaultWfcTokenVolumeViewOptions(2);
      Scene := ProjectWfcTokenVolume3D(Projection.Tokens, 2, 2, 2, Palette, ViewOptions);
      try
        Check(Scene.QuadCount = 24, 'installed public volume view culls interior faces');
        Check(Pos('<svg ', EncodeVoxel3DProjectedSceneSvg(Scene,
          DefaultVoxel3DSvgOptions)) > 0, 'installed view exports dependency-free SVG');
      finally Scene.Free; end;
    finally Composition.Free; Pipeline.Free; end;
  finally Decoded.Free; end;
end;

begin
  try
    UseGraph;
    UseLattice;
    UseMusicForm;
    UsePortableConnectivity;
    UseVolumePatterns;
    WriteLn('Installed package consumer checks: ', Checks);
  except
    on E: Exception do
    begin WriteLn('FAIL: ', E.ClassName, ': ', E.Message); Halt(1); end;
  end;
end.
