{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_training_pattern3d_workspace_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host, Web,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_pattern3d, wfc_pattern3d_text,
  wfc_training, wfc_training_text, wfc_training_workspace,
  wfc_pipeline_model, wfc_pipeline_text, wfc_pipeline_run,
  wfc_pipeline_run_text, wfc_pipeline_result, wfc_pipeline_result_text,
  wfc_pipeline_runtime, wfc_validate_app, wfc_inspect_app,
  training_studio_presets, training_studio_demo;

var Checks: Integer;

procedure Check(const Condition: Boolean; const MessageText: String);
begin
  Inc(Checks);
  if not Condition then raise Exception.Create(MessageText);
end;

function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(Values));
  for I := 0 to High(Values) do Result[I] := Values[I];
end;

function Args(const Values: array of String): TWfcValidateArguments;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(Values));
  for I := 0 to High(Values) do Result[I] := Values[I];
end;

procedure CheckFootprints(const Model: TWfcOverlappingModel3D;
  const Output: TWfcModelTokens; const Width, Height, Depth: Integer);
var X,Y,Z,DX,DY,DZ,P,Index: Integer; Found, Equal: Boolean;
begin
  Check(Length(Output) = Width * Height * Depth, 'exact unpadded XYZ output size');
  { Enumerate actual public windows, including all wrapped seams. This does
    not trust the solver's private indices, its trace, or its commit report. }
  for Z := 0 to Depth - 1 do for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
    begin
      Found := False;
      for P := 0 to Model.PatternCount - 1 do
      begin
        Equal := True;
        for DZ := 0 to Model.PatternDepth - 1 do
          for DY := 0 to Model.PatternHeight - 1 do
            for DX := 0 to Model.PatternWidth - 1 do
            begin
              Index := (((Z+DZ) mod Depth)*Height + (Y+DY) mod Height)*Width +
                (X+DX) mod Width;
              if Output[Index] <> Model.PaletteTokenAt(
                  Model.PatternPaletteIndexAt(P,DX,DY,DZ)) then Equal := False;
            end;
        if Equal then begin Found := True; Break; end;
      end;
      Check(Found, 'public footprint belongs to learned volume at ' +
        IntToStr(X) + ',' + IntToStr(Y) + ',' + IntToStr(Z));
    end;
  Check(TrainingStudioLatticeOutputIsValid(Width,Height,Depth,Output),
    'independent planted-cell support and headroom semantics');
end;

procedure CheckArtifact(const Family, Input, RecipeText, RunText: String);
var A: TWfcValidateArguments; C: TWfcValidateCommand; P: TWfcInspectCommand;
  O,E: String; Code: Integer;
begin
  if Family = 'result' then A := Args([Family,'recipe','run','input'])
  else if Family = 'run' then A := Args([Family,'recipe','input'])
  else A := Args([Family,'input']);
  Check(WfcValidateParseCommand(A,C,E), 'parse artifact family ' + Family);
  Code := WfcValidateExecuteText(C,Input,RecipeText,RunText,O,E);
  Check((Code=0) and (E='') and (Pos('valid canonical ',O)=1),
    'validate real Studio artifact ' + Family + ': ' + E);
  C.OutputMode := wvomCanonical;
  Code := WfcValidateExecuteText(C,Input,RecipeText,RunText,O,E);
  Check((Code=0) and (E='') and (O=Input), 'exact canonical bytes ' + Family);
  C.OutputMode := wvomQuiet;
  Code := WfcValidateExecuteText(C,Input,RecipeText,RunText,O,E);
  Check((Code=0) and (E='') and (O=''), 'quiet real artifact ' + Family);
  Code := WfcValidateExecuteText(C,Input+'end'#10,RecipeText,RunText,O,E);
  Check((Code=1) and (E<>'') and (O=''), 'reject trailing artifact with no stdout ' + Family);
  Check(WfcInspectParseCommand(A,P,E), 'parse inspection family ' + Family);
  P.DetailLimit := 12;
  Code := WfcInspectExecuteText(P,Input,RecipeText,RunText,O,E);
  Check((Code=0) and (E='') and (Pos('family='+Family+#10,O)>0) and
    (Pos('execution=not-run'#10,O)>0), 'bounded read-only inspection ' + Family);
  if Family='pattern3d' then
  begin
    Check(Pos('footprint=2,2,2',O)>0, 'inspector reports all three footprint axes');
    Check(Pos('xyz=',O)>0, 'inspector reports actual XYZ payload positions');
  end;
end;

procedure CheckReplay(const RecipeText, RunText, ResultText: String);
var Recipe: TWfcPipelineModel; Run: TWfcPipelineRun;
  Stored, Actual: TWfcPipelineResult; C: TWfcValidateCommand; O,E: String;
  Code: Integer;
begin
  Recipe := DecodeWfcPipelineModelText(RecipeText);
  try
    Run := DecodeWfcPipelineRunText(RunText,Recipe);
    try
      Check(EncodeWfcPipelineRunText(Run)=RunText, 'run roundtrip retains exact XYZ inputs');
      Stored := DecodeWfcPipelineResultText(ResultText,Recipe,Run);
      try
        Check(EncodeWfcPipelineResultText(Stored)=ResultText, 'bound stored result exact roundtrip');
        Actual := ExecuteWfcPipeline(Recipe,Run);
        try Check(EncodeWfcPipelineResultText(Actual)=ResultText,
          'fresh decoded recipe and run reproduce complete result bytes');
        finally Actual.Free; end;
      finally Stored.Free; end;
    finally Run.Free; end;
  finally Recipe.Free; end;
  Check(WfcValidateParseCommand(Args(['result','--replay','--emit-canonical',
    'recipe','run','result']),C,E), 'parse explicit replay');
  Code := WfcValidateExecuteText(C,ResultText,RecipeText,RunText,O,E);
  Check((Code=0) and (E='') and (O=ResultText), 'portable CLI exact replay: '+E);
end;

procedure CheckNoDerived(const W: TWfcTrainingWorkspace; const RecipeExpected: Boolean);
var Rejected: Boolean; S: String;
begin
  Check((W.HasRecipe=RecipeExpected) and not W.HasRun and not W.HasResult,
    'invalidation clears run and result, with explicit recipe lifetime');
  Rejected:=False;
  try S:=W.ResultText; except on E: EWfcTrainingWorkspace do Rejected:=True; end;
  Check(Rejected, 'stale result cannot be exported');
  Rejected:=False;
  try S:=W.RunText; except on E: EWfcTrainingWorkspace do Rejected:=True; end;
  Check(Rejected, 'stale run cannot be exported');
end;

procedure TestWorkspace;
var W: TWfcTrainingWorkspace; O: TWfcTrainingSolveOptions;
  D: TWfcTrainingDocument; M: TWfcOverlappingModel3D;
  Locks, CopyLocks: TWfcPipelineCellLocks; Domains: TWfcPipelineCellDomains;
  Q: TWfcTrainingValueQuotas; C: TWfcTrainingConnectivities;
  Profiles: TWfcTrainingConnectivityValues; Required: TGraphPositions;
  Root: TGraphPosition; V,Output: TWfcModelTokens;
  Source,ModelText,RecipeText,RunText,Solved,PolicySource,PolicyResult,S,E: String;
  Cmd: TWfcValidateCommand; I,Seed,Code,LeafCount: Integer; Rejected: Boolean;
begin
  Check((TRAINING_STUDIO_PRESET_COUNT=8) and (TRAINING_STUDIO_PATTERN3D_PRESET=7),
    'volume preset appended without renumbering existing demos');
  Source := TrainingStudioPresetText(7);
  Check(Pos('wfclearn=6'#10,Source)=1, 'explicit volume authoring source version six');
  D := DecodeWfcTrainingText(Source);
  try
    Check(EncodeWfcTrainingText(D)=Source, 'preset source is canonical and reloadable');
    Check((D.CopyOptions.Kind=wtkPattern3D) and (D.CopyOptions.Boundary=wmbWrap) and
      (D.CopyOptions.Symmetry=wmsD4) and (D.CopyOptions.PatternWidth=2) and
      (D.CopyOptions.PatternHeight=2) and (D.CopyOptions.PatternDepth=2),
      'source retains rank-three overlapping footprint and vertical-preserving D4');
    Check((D.SampleCount=1) and (D.TotalTokenCount=64) and
      (D.SampleAt(0).Width=4) and (D.SampleAt(0).Height=4) and
      (D.SampleAt(0).Depth=4), 'authored courtyard is one owned 4x4x4 sample');
  finally D.Free; end;
  W := TWfcTrainingWorkspace.Create(InteractiveWfcTrainingWorkspaceLimits);
  try
    W.SetSourceText(Source); W.Train;
    Check(W.HasRecipe and not W.HasRun and not W.HasResult, 'training publishes only a fresh recipe');
    Check((W.Rank=3) and W.WrapNeighbors and (W.PublicPassIndex=1) and
      (W.SourceTokenCount=64) and (W.ModelItemCount<=128), 'volume fits stated interactive workspace bounds');
    ModelText:=W.ModelText; RecipeText:=W.RecipeText;
    Check(Pos('wfcp=2'#10,ModelText)=1, 'learned model selects compact full-volume format');
    Check(Pos('wfcpipeline=4'#10,RecipeText)=1, 'learned recipe selects volume projection capability');
    WriteLn('volume workspace source=',W.TrainingSignatureText,
      ' recipe=',W.RecipeSignatureText,' patterns=',W.ModelItemCount);
    Check(W.TrainingSignatureText='C87487A5', 'authored volume source portable golden');
    Check(W.RecipeSignatureText='E63D8AD3', 'learned volume recipe portable golden');
    Check(W.ModelItemCount=35, 'authored D4 footprint inventory golden');
    M:=DecodeWfcPattern3DText(ModelText);
    try
      Locks:=TrainingStudioPresetLocks(7,W.PublicPassIndex);
      CopyLocks:=TrainingStudioPresetLocks(7,W.PublicPassIndex);
      CopyLocks[1].Token:='air';
      Check((Locks[1].Token='leaf') and (Locks[1].X=2) and
        (Locks[1].Y=2) and (Locks[1].Z=1), 'preset lock ownership and explicit nonzero XYZ');
      O:=TrainingStudioPresetOptions(7);
      Check((O.Width=4) and (O.Height=4) and (TrainingStudioPresetDepth(7)=4),
        'preset output is explicit 4x4x4, not a flattened preview');
      for Seed:=0 to 2 do
      begin
        O.Seed:=Seed;
        W.ConfigureVolumeRun(O,4,Locks,nil); W.Solve;
        Check(W.ResultStatus=wprsSolved, 'volume seed solves with exact public locks');
        Output:=W.OutputTokens;
        CheckFootprints(M,Output,4,4,4);
        Check((Output[0]='stone') and (Output[26]='leaf') and (Output[37]='air'),
          'all three user XYZ locks survive inverse projection');
      end;
      O.Seed:=0; W.ConfigureVolumeRun(O,4,Locks,nil); W.Solve;
      Solved:=W.ResultText; RunText:=W.RunText; Output:=W.OutputTokens;
      WriteLn('volume workspace result=',W.ResultSignatureText);
      Check(W.ResultSignatureText='32E7DE7F', 'seed-zero locked volume result portable golden');
      Output[26]:='stone';
      Check(W.OutputTokens[26]='leaf', 'returned public output is a detached copy');
      CheckArtifact('training',Source,'','');
      CheckArtifact('pattern3d',ModelText,'','');
      CheckArtifact('recipe',RecipeText,'','');
      CheckArtifact('run',RunText,RecipeText,'');
      CheckArtifact('result',Solved,RecipeText,RunText);
      CheckReplay(RecipeText,RunText,Solved);

      { A stone domain at (2,2,2) conflicts with the learned air headroom
        above the leaf locked at (2,2,1), not with a same-cell input check. }
      SetLength(Domains,1);
      Domains[0]:=MakeWfcPipelineCellDomain(W.PublicPassIndex,2,2,2,Tokens(['stone']));
      W.ConfigureVolumeRun(O,4,Locks,Domains);
      Check(not W.HasResult, 'edited XYZ domain clears old solved output');
      W.Solve;
      Check((W.ResultStatus=wprsContradiction) and (Length(W.OutputTokens)=0),
        'contradictory actual XYZ constraints expose no partial public volume');
      CheckReplay(RecipeText,W.RunText,W.ResultText);
      Check(WfcValidateParseCommand(Args(['result','recipe','run','result']),Cmd,E),
        'binding test parser');
      Code:=WfcValidateExecuteText(Cmd,Solved,RecipeText,W.RunText,S,E);
      Check((Code=1) and (S='') and (E<>''), 'stored solved result rejects a different bound run');
      W.ConfigureVolumeRun(O,4,Locks,nil); W.Solve;
      Check(W.ResultText=Solved, 'removing XYZ conflict restores exact original result');
      Rejected:=False;
      try W.ConfigureRun(O,Locks,nil);
      except on E: EWfcTrainingWorkspace do Rejected:=True; end;
      Check(Rejected, 'rank-three recipe cannot silently use depth-one convenience path');
      CheckNoDerived(W,True);
      Rejected:=False;
      try W.ConfigureVolumeRun(O,0,Locks,nil);
      except on E: EWfcTrainingWorkspace do Rejected:=True; end;
      Check(Rejected, 'zero depth rejected without stale artifacts');
      CheckNoDerived(W,True);
      W.SetSourceText(Source); CheckNoDerived(W,False); W.Train;
      Check((W.RecipeText=RecipeText) and (W.ModelText=ModelText),
        'source save/load/retrain preserves exact derived model and recipe');
      W.ConfigureVolumeRun(O,4,Locks,nil); W.Solve;
      Check(W.ResultText=Solved, 'source plus serialized public choices reproduce full result');

      { Persist policy in source, not as an unsaved post-training overlay. }
      SetLength(Q,1);
      Q[0]:=MakeWfcTrainingValueQuota('planted-courtyard',Tokens(['leaf']),1,64);
      W.ReplaceValueQuotas(Q); CheckNoDerived(W,True);
      Check((W.ValueQuotaCount=1) and (Pos('wfclearn=6'#10,W.SourceText)=1) and
        (Pos('wfcpipeline=4'#10,W.RecipeText)=1), 'volume quota retains explicit source and recipe versions');
      Q[0].MinimumCount:=65;
      Check(W.CopyValueQuotas[0].MinimumCount=1, 'workspace owns copied source quota');
      Rejected:=False;
      try S:=W.ModelText; except on E: EWfcTrainingWorkspace do Rejected:=True; end;
      Check(Rejected, 'model-only export refuses to lose authored volume quota');
      V:=W.PublicVocabulary; SetLength(Profiles,Length(V));
      for I:=0 to High(V) do Profiles[I]:=MakeWfcTrainingConnectivityValue(V[I],
        [gdNorth,gdEast,gdSouth,gdWest,gdUp,gdDown]);
      Root:=Default(TGraphPosition); SetLength(Required,1);
      Required[0]:=Root; Required[0].X:=3; Required[0].Y:=3; Required[0].Z:=3;
      SetLength(C,1);
      C[0]:=MakeWfcTrainingConnectivity('complete-volume',Root,Required,Profiles,True);
      W.ReplaceConnectivities(C); CheckNoDerived(W,True);
      Check((W.ValueQuotaCount=1) and (W.ConnectivityCount=1),
        'connectivity authoring preserves existing quota policy');
      PolicySource:=W.SourceText;
      Code:=WfcValidateExecuteText(Cmd,Solved,W.RecipeText,RunText,S,E);
      Check((Code=1) and (S='') and (E<>''), 'old run/result reject a changed recipe policy binding');
      D:=DecodeWfcTrainingText(PolicySource);
      try Check((D.ValueQuotaCount=1) and (D.ConnectivityCount=1) and
        (D.ConnectivityAt(0).RequiredPositions[0].Z=3),
        'source roundtrip persists full XYZ terminal and both policies');
      finally D.Free; end;
      W.ConfigureVolumeRun(O,4,Locks,nil); W.Solve;
      Check(W.ResultStatus=wprsSolved, 'public quota and all-axis connectivity solve together');
      CheckFootprints(M,W.OutputTokens,4,4,4);
      Output:=W.OutputTokens; LeafCount:=0;
      for I:=0 to High(Output) do if Output[I]='leaf' then Inc(LeafCount);
      Check((LeafCount>=1) and (LeafCount<=64), 'independent public quota recount');
      PolicyResult:=W.ResultText;
      CheckReplay(W.RecipeText,W.RunText,PolicyResult);
      W.SetSourceText(PolicySource); W.Train;
      W.ConfigureVolumeRun(O,4,Locks,nil); W.Solve;
      Check(W.ResultText=PolicyResult, 'saved source policy retrains and replays exactly');
      C:=W.CopyConnectivities;
      for I:=0 to High(C[0].Values) do
        C[0].Values[I].Openings:=[gdNorth,gdEast,gdSouth,gdWest];
      W.ReplaceConnectivities(C); W.ConfigureVolumeRun(O,4,Locks,nil); W.Solve;
      Check((W.ResultStatus=wprsContradiction) and (Length(W.OutputTokens)=0),
        'removing both vertical ports disconnects the required Z=3 terminal');
      W.SetSourceText(PolicySource); W.Train;
      W.ConfigureVolumeRun(O,4,Locks,nil); W.Solve;
      Check(W.ResultText=PolicyResult, 'restored six-port policy recovers exact full-volume result');
      Q:=W.CopyValueQuotas; Q[0].MinimumCount:=65; Q[0].MaximumCount:=65;
      W.ReplaceValueQuotas(Q); W.ConfigureVolumeRun(O,4,Locks,nil); W.Solve;
      Check((W.ResultStatus=wprsContradiction) and (Length(W.OutputTokens)=0),
        'quota exceeding output volume contradicts without clamping or partial output');
      W.ReplaceValueQuotas(nil);
      Check((W.ValueQuotaCount=0) and (W.ConnectivityCount=1),
        'removing quotas preserves source connectivity');
      Rejected:=False;
      try S:=W.ModelText; except on E: EWfcTrainingWorkspace do Rejected:=True; end;
      Check(Rejected, 'model-only export refuses to lose remaining connectivity');
      W.ReplaceConnectivities(nil);
      Check((W.SourceText=Source) and (W.RecipeText=RecipeText) and (W.ModelText=ModelText),
        'removing all policies restores exact original source/model/recipe identity');
      W.ConfigureVolumeRun(O,4,Locks,nil); W.Solve;
      Check(W.ResultText=Solved, 'policy removal recovers exact original solved artifact');
      Q[0].Values:=Tokens(['not-in-this-volume']);
      Rejected:=False;
      try W.ReplaceValueQuotas(Q); except on E: Exception do Rejected:=True; end;
      Check(Rejected and (W.SourceText=Source), 'invalid policy retains prior editable source');
      CheckNoDerived(W,False);
      W.Train; W.ConfigureVolumeRun(O,4,Locks,nil); W.Solve;
      Check(W.ResultText=Solved, 'retrain retained source after failed policy clears failure state');
    finally M.Free; end;
  finally W.Free; end;
end;

begin
  try
    TestWorkspace;
    WriteLn('Volume training workspace checks: ',Checks);
  except
    on E: Exception do
    begin
      {$IFDEF PAS2JS}document.body.setAttribute('data-self-test-message',E.Message);{$ENDIF}
      WriteLn('FAIL: ',E.ClassName,': ',E.Message); Halt(1);
    end;
  end;
end.
