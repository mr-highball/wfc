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
unit training_studio_demo;

{$mode delphi}{$H+}

interface

uses wfc_model;

function TrainingStudioOutputIsValid(const APreset, AWidth, AHeight: Integer;
  const ATokens: TWfcModelTokens): Boolean;
function TrainingStudioVolumeOutputIsValid(const AWidth, AHeight,
  ADepth: Integer; const ATokens: TWfcModelTokens): Boolean;
procedure RunTrainingStudioDemo;

implementation

uses
  SysUtils, wfc, wfc_text_codec, wfc_text_tokenize,
  wfc_training, wfc_training_workspace, wfc_pipeline_result, training_studio_presets,
  training_studio_connectivity;

function TrainingStudioOutputIsValid(const APreset, AWidth, AHeight: Integer;
  const ATokens: TWfcModelTokens): Boolean;
var
  I: Integer;
  LFirst: TWfcModelToken;
  LExpected: TWfcModelToken;
  LText: TWfcModelToken;
begin
  Result := False;
  if (AWidth < 1) or (AHeight < 1) then
    Exit;
  if AWidth > High(Integer) div AHeight then
    Exit;
  if Length(ATokens) <> AWidth * AHeight then
    Exit;
  if APreset in [0, 1, 2] then
  begin
    LFirst := ATokens[0];
    if (LFirst <> 'A') and (LFirst <> 'B') then Exit;
    for I := 0 to Length(ATokens) - 1 do
    begin
      LExpected := LFirst;
      if ((I mod AWidth + I div AWidth) mod 2) <> 0 then
        if LFirst = 'A' then LExpected := 'B' else LExpected := 'A';
      if ATokens[I] <> LExpected then Exit;
    end;
    Result := True;
  end
  else if APreset = 3 then
    Result := (Length(ATokens) = 3) and
      ((ATokens[0] = 'red') or
       (ATokens[0] = WfcTextDecodeToken('caf%C3%A9', 'studio demo'))) and
      (ATokens[1] = 'fox') and (ATokens[2] = '.')
  else if APreset = 4 then
  begin
    LText := DetokenizeWfcText(ATokens, wttkUnicodeScalar);
    Result := (LText = 'a cat.') or (LText = 'a bat.');
  end
  else if APreset = TRAINING_STUDIO_CIRCULAR_PRESET then
    Result := TrainingStudioCircularOutputIsValid(ATokens);
end;

function TrainingStudioVolumeOutputIsValid(const AWidth, AHeight,
  ADepth: Integer; const ATokens: TWfcModelTokens): Boolean;
var
  X, Y, Z, I, LArea: Integer;
begin
  Result := False;
  if (AWidth < 1) or (AHeight < 1) or (ADepth < 1) then Exit;
  if AWidth > High(Integer) div AHeight then Exit;
  LArea := AWidth * AHeight;
  if LArea > High(Integer) div ADepth then Exit;
  if Length(ATokens) <> LArea * ADepth then Exit;
  { Independently check all three wrapped positive-axis edges, not a
    prediction of the solver's seed choice or its private token order. }
  for Z := 0 to ADepth - 1 do
    for Y := 0 to AHeight - 1 do
      for X := 0 to AWidth - 1 do
      begin
        I := X + AWidth * Y + LArea * Z;
        if (ATokens[I] <> 'A') and (ATokens[I] <> 'B') then Exit;
        if ATokens[I] = ATokens[(X + 1) mod AWidth + AWidth * Y +
          LArea * Z] then Exit;
        if ATokens[I] = ATokens[X + AWidth * ((Y + 1) mod AHeight) +
          LArea * Z] then Exit;
        if ATokens[I] = ATokens[X + AWidth * Y +
          LArea * ((Z + 1) mod ADepth)] then Exit;
      end;
  Result := True;
end;

procedure RunOne(const APreset: Integer; const ASeed: TGraphSeed);
var
  LWorkspace: TWfcTrainingWorkspace;
  LOptions: TWfcTrainingSolveOptions;
  LTokens: TWfcModelTokens;
  I, LDepth: Integer;
  LValid: Boolean;
begin
  LWorkspace := TWfcTrainingWorkspace.Create(
    InteractiveWfcTrainingWorkspaceLimits);
  try
    LWorkspace.SetSourceText(TrainingStudioPresetText(APreset));
    LWorkspace.Train;
    LOptions := TrainingStudioPresetOptions(APreset);
    LOptions.Seed := ASeed;
    LDepth := TrainingStudioPresetDepth(APreset);
    if LWorkspace.Rank = 3 then
      LWorkspace.ConfigureVolumeRun(LOptions, LDepth, nil, nil)
    else LWorkspace.ConfigureRun(LOptions,
      TrainingStudioPresetLocks(APreset, LWorkspace.PublicPassIndex), nil);
    LWorkspace.Solve;
    LTokens := LWorkspace.OutputTokens;
    if LWorkspace.Rank = 3 then
      LValid := TrainingStudioVolumeOutputIsValid(LOptions.Width,
        LOptions.Height, LDepth, LTokens)
    else LValid := TrainingStudioOutputIsValid(APreset, LOptions.Width,
      LOptions.Height, LTokens);
    if (LWorkspace.ResultStatus <> wprsSolved) or not LValid then
      raise Exception.Create('training studio independent output validation failed');
    WriteLn('preset=', APreset, ' ', TrainingStudioPresetName(APreset));
    WriteLn('seed=', ASeed, ' source=', LWorkspace.TrainingSignatureText,
      ' recipe=', LWorkspace.RecipeSignatureText,
      ' result=', LWorkspace.ResultSignatureText);
    WriteLn('samples=', LWorkspace.SampleCount, ' source-tokens=',
      LWorkspace.SourceTokenCount, ' model-items=', LWorkspace.ModelItemCount);
    if APreset = TRAINING_STUDIO_CIRCULAR_PRESET then
      WriteLn('source-boundary=circular output-boundary=wrap public-locks=3',
        ' first-phrase=rise-fall second-phrase=rise-rest');
    for I := 0 to Length(LTokens) - 1 do
    begin
      if (LDepth > 1) and (I mod (LOptions.Width * LOptions.Height) = 0) then
        WriteLn('z=', I div (LOptions.Width * LOptions.Height));
      if I mod LOptions.Width <> 0 then Write(' ');
      Write(WfcTextEncodeToken(LTokens[I], 'studio demo'));
      if I mod LOptions.Width = LOptions.Width - 1 then WriteLn;
    end;
  finally
    LWorkspace.Free;
  end;
end;

procedure RunQuotaDemo;
var
  W: TWfcTrainingWorkspace;
  O: TWfcTrainingSolveOptions;
  Q: TWfcTrainingValueQuotas;
  Tokens: TWfcModelTokens;
  Source, Recipe, Solved: String;
  I, Count: Integer;
begin
  W := TWfcTrainingWorkspace.Create(InteractiveWfcTrainingWorkspaceLimits);
  try
    W.SetSourceText(TrainingStudioPresetText(3));
    W.Train;
    Source := W.SourceText;
    Recipe := W.RecipeText;
    SetLength(Tokens, 1);
    Tokens[0] := 'red';
    SetLength(Q, 1);
    Q[0] := MakeWfcTrainingValueQuota('one-red', Tokens, 1, 1);
    W.ReplaceValueQuotas(Q);
    if (Pos('wfclearn=3'#10, W.SourceText) <> 1) or
        (Pos('wfcpipeline=2'#10, W.RecipeText) <> 1) then
      raise Exception.Create('quota authoring did not persist versioned source and recipe');
    O := TrainingStudioPresetOptions(3);
    W.ConfigureRun(O, nil, nil);
    W.Solve;
    Tokens := W.OutputTokens;
    Count := 0;
    for I := 0 to High(Tokens) do if Tokens[I] = 'red' then Inc(Count);
    if (W.ResultStatus <> wprsSolved) or (Count <> 1) or
        (not TrainingStudioOutputIsValid(3, O.Width, O.Height, Tokens)) then
      raise Exception.Create('independent authored quota demonstration failed');
    Solved := W.ResultText;
    WriteLn('quota-demo label=one-red minimum=1 maximum=1 observed=', Count);
    WriteLn('seed=', O.Seed, ' source=', W.TrainingSignatureText,
      ' recipe=', W.RecipeSignatureText, ' result=', W.ResultSignatureText);
    for I := 0 to High(Tokens) do
    begin
      if I <> 0 then Write(' ');
      Write(WfcTextEncodeToken(Tokens[I], 'quota demo'));
    end;
    WriteLn;
    W.Train;
    W.ConfigureRun(O, nil, nil);
    W.Solve;
    if W.ResultText <> Solved then
      raise Exception.Create('persisted quota source did not replay after retraining');
    Q[0].MinimumCount := 4;
    Q[0].MaximumCount := 4;
    W.ReplaceValueQuotas(Q);
    W.ConfigureRun(O, nil, nil);
    W.Solve;
    if (W.ResultStatus <> wprsContradiction) or (Length(W.OutputTokens) <> 0) then
      raise Exception.Create('impossible quota did not produce a clean contradiction');
    WriteLn('impossible-quota=contradiction public-cells=0');
    W.ReplaceValueQuotas(nil);
    if (W.SourceText <> Source) or (W.RecipeText <> Recipe) then
      raise Exception.Create('removing the last quota changed legacy source identity');
    WriteLn('quota-source-replay=passed legacy-restore=passed');
  finally W.Free; end;
end;

procedure RunTrainingStudioDemo;
var
  LPreset: Integer;
  LSeed: TGraphSeed;
  I: Integer;
begin
  if (ParamCount >= 1) and
      ((ParamStr(1) = '--connectivity-demo') or (ParamStr(1) = '--connectivity-selftest')) then
  begin
    if ParamCount <> 1 then
      raise Exception.Create('usage: TrainingStudio --connectivity-demo | --connectivity-selftest');
    if ParamStr(1) = '--connectivity-demo' then RunTrainingStudioConnectivityDemo;
    WriteLn('Training Studio connectivity checks: ', TrainingStudioConnectivitySelfTest);
    Exit;
  end;
  if (ParamCount >= 1) and
      ((ParamStr(1) = '--quota-demo') or (ParamStr(1) = '--quota-selftest')) then
  begin
    if ParamCount <> 1 then
      raise Exception.Create('usage: TrainingStudio --quota-demo | --quota-selftest');
    RunQuotaDemo;
    Exit;
  end;
  if (ParamCount = 1) and (ParamStr(1) = '--selftest') then
  begin
    for I := 0 to TRAINING_STUDIO_PRESET_COUNT - 1 do RunOne(I, 0);
    Exit;
  end;
  if ParamCount > 2 then
    raise Exception.Create('usage: TrainingStudio [preset 0..6] [decimal seed] | --selftest | --quota-demo | --quota-selftest | --connectivity-demo | --connectivity-selftest');
  LPreset := 2;
  LSeed := 0;
  if ParamCount >= 1 then
    LPreset := WfcTextParseCanonicalInteger(ParamStr(1), 'preset', 'studio');
  if ParamCount = 2 then
    LSeed := WfcTextParseCanonicalCardinal(ParamStr(2), 'seed', 'studio');
  RunOne(LPreset, LSeed);
end;

end.
