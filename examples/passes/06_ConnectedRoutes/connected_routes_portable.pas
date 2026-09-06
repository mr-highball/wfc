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
unit connected_routes_portable;

{$mode delphi}{$H+}

interface

uses
  SysUtils, wfc, wfc_model, wfc_pipeline_model;

function BuildConnectedRoutesPortableRecipe: TWfcPipelineModel;
procedure ConnectedRoutesPortableArtifacts(out ARecipe, ARun, AResult: String);
function ConnectedRoutesPortableSelfTest: Integer;

implementation

uses
  wfc_rule_model, wfc_rule_text, wfc_sequence,
  wfc_pipeline_text, wfc_pipeline_run, wfc_pipeline_run_text,
  wfc_pipeline_result, wfc_pipeline_result_text, wfc_pipeline_runtime;

function BuildConnectedRoutesPortableRecipe: TWfcPipelineModel;
var
  Rules: TWfcRuleModel;
  Resources: TWfcPipelineResources;
  Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies;
  Quotas: TWfcPipelineValueQuotas;
  Connections: TWfcPipelineConnectivities;
  Profiles: TWfcPipelineConnectivityValues;
  Required: TGraphPositions;
  Root: TGraphPosition;
  Values: TWfcModelTokens;
  Weights: TWfcModelIntegerArray;
  DocumentText: String;
begin
  SetLength(Values, 2); Values[0] := 'route'; Values[1] := 'empty';
  SetLength(Weights, 2); Weights[0] := 1; Weights[1] := 1;
  Rules := TWfcRuleModel.Create(2, Values, Weights, nil);
  try DocumentText := EncodeWfcRuleText(Rules); finally Rules.Free; end;
  SetLength(Resources, 1);
  Resources[0] := MakeWfcPipelineResource('route-rules', wprkRules,
    DocumentText, 'Project-authored cardinal route vocabulary', 'MIT',
    'connected-routes:portable:1');
  SetLength(Passes, 2);
  Passes[0] := MakeWfcPipelinePass('routes', wppvPublic, gpmOverlay, -1,
    wpakRules, 0, False, wseWhole);
  Passes[1] := MakeWfcPipelinePass('published-routes', wppvPublic,
    gpmTransform, 0, wpakEmpty, -1, False, wseWhole);
  SetLength(Dependencies, 1);
  Dependencies[0] := MakeWfcPipelineDependency(1, 0);
  Root := Default(TGraphPosition);
  SetLength(Required, 1); Required[0] := Root; Required[0].X := 2;
  SetLength(Profiles, 1);
  Profiles[0] := MakeWfcPipelineConnectivityValue('route',
    [gdNorth, gdEast, gdSouth, gdWest]);
  SetLength(Connections, 1);
  Connections[0] := MakeWfcPipelineConnectivity(1, 'town-gates', Root,
    Required, Profiles, True);
  SetLength(Values, 1); Values[0] := 'route';
  SetLength(Quotas, 1);
  Quotas[0] := MakeWfcPipelineValueQuota(1, 'three-route-cells', Values, 3, 3);
  Result := TWfcPipelineModel.Create(MakeWfcPipelineMetadata(
    'Connected Routes portable artifacts', 'MIT',
    'Two gates joined by three reciprocal cardinal route cells',
    'connected-routes:portable:1'), 2, False, rmBottomUp,
    Resources, Passes, Dependencies, nil, nil, Quotas, Connections);
end;

procedure ValidateRoute(const Output: TWfcPipelineResult);
var I, L: Integer; Layer: TWfcPipelineResultLayer;
begin
  if (Output.Status <> wprsSolved) or (Output.Width <> 3) or
      (Output.Height <> 2) or (Output.Depth <> 1) or
      (Output.LayerCount <> 2) then
    raise Exception.Create('portable route artifact has no complete solved route');
  { A three-cell path between (0,0) and (2,0) on this open grid can only
    occupy the top row. This independent geometric witness simultaneously
    checks both gates, reciprocal adjacency, every participant and the quota. }
  for L := 0 to Output.LayerCount - 1 do
  begin
    Layer := Output.LayerAt(L);
    for I := 0 to High(Layer.Tokens) do
      if ((I < 3) and (Layer.Tokens[I] <> 'route')) or
          ((I >= 3) and (Layer.Tokens[I] <> 'empty')) then
        raise Exception.Create('portable route artifact failed independent path/count validation');
  end;
end;

procedure ConnectedRoutesPortableArtifacts(out ARecipe, ARun, AResult: String);
var Model: TWfcPipelineModel; Run: TWfcPipelineRun; Output: TWfcPipelineResult;
begin
  ARecipe := ''; ARun := ''; AResult := '';
  Model := BuildConnectedRoutesPortableRecipe;
  try
    Run := TWfcPipelineRun.Create(Model, 3, 2, 1, 17, wpssOneWay,
      128, 0, True, nil, nil);
    try
      Output := ExecuteWfcPipeline(Model, Run);
      try
        ValidateRoute(Output);
        ARecipe := EncodeWfcPipelineModelText(Model);
        ARun := EncodeWfcPipelineRunText(Run);
        AResult := EncodeWfcPipelineResultText(Output);
      finally Output.Free; end;
    finally Run.Free; end;
  finally Model.Free; end;
end;

function ConnectedRoutesPortableSelfTest: Integer;
var RecipeText, RunText, ResultText: String; Model: TWfcPipelineModel;
  Run: TWfcPipelineRun; Output, Decoded: TWfcPipelineResult;
  procedure Check(const Condition: Boolean; const MessageText: String);
  begin
    Inc(Result);
    if not Condition then raise Exception.Create('portable route self-test: ' + MessageText);
  end;
begin
  Result := 0;
  ConnectedRoutesPortableArtifacts(RecipeText, RunText, ResultText);
  Check(Pos('wfcpipeline=3'#10, RecipeText) = 1, 'recipe version');
  Model := DecodeWfcPipelineModelText(RecipeText);
  try
    Check((Model.ConnectivityCount = 1) and (Model.ValueQuotaCount = 1),
      'persisted connectivity and count constraints');
    Check(EncodeWfcPipelineModelText(Model) = RecipeText, 'recipe canonical roundtrip');
    Run := DecodeWfcPipelineRunText(RunText, Model);
    try
      Check(EncodeWfcPipelineRunText(Run) = RunText, 'run canonical roundtrip');
      Output := ExecuteWfcPipeline(Model, Run);
      try
        ValidateRoute(Output);
        Check(EncodeWfcPipelineResultText(Output) = ResultText, 'fresh artifact replay');
        Decoded := DecodeWfcPipelineResultText(ResultText, Model, Run);
        try
          ValidateRoute(Decoded);
          Check(EncodeWfcPipelineResultText(Decoded) = ResultText, 'result canonical roundtrip');
        finally Decoded.Free; end;
      finally Output.Free; end;
    finally Run.Free; end;
  finally Model.Free; end;
end;

end.
