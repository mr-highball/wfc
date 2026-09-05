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
procedure RunTrainingStudioDemo;

implementation

uses
  SysUtils, wfc, wfc_text_codec, wfc_text_tokenize,
  wfc_training_workspace, wfc_pipeline_result, training_studio_presets;

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
  end;
end;

procedure RunOne(const APreset: Integer; const ASeed: TGraphSeed);
var
  LWorkspace: TWfcTrainingWorkspace;
  LOptions: TWfcTrainingSolveOptions;
  LTokens: TWfcModelTokens;
  I: Integer;
begin
  LWorkspace := TWfcTrainingWorkspace.Create(
    InteractiveWfcTrainingWorkspaceLimits);
  try
    LWorkspace.SetSourceText(TrainingStudioPresetText(APreset));
    LWorkspace.Train;
    LOptions := TrainingStudioPresetOptions(APreset);
    LOptions.Seed := ASeed;
    LWorkspace.ConfigureRun(LOptions, nil, nil);
    LWorkspace.Solve;
    LTokens := LWorkspace.OutputTokens;
    if (LWorkspace.ResultStatus <> wprsSolved) or
        (not TrainingStudioOutputIsValid(APreset, LOptions.Width,
        LOptions.Height, LTokens)) then
      raise Exception.Create('training studio independent output validation failed');
    WriteLn('preset=', APreset, ' ', TrainingStudioPresetName(APreset));
    WriteLn('seed=', ASeed, ' source=', LWorkspace.TrainingSignatureText,
      ' recipe=', LWorkspace.RecipeSignatureText,
      ' result=', LWorkspace.ResultSignatureText);
    WriteLn('samples=', LWorkspace.SampleCount, ' source-tokens=',
      LWorkspace.SourceTokenCount, ' model-items=', LWorkspace.ModelItemCount);
    for I := 0 to Length(LTokens) - 1 do
    begin
      if I mod LOptions.Width <> 0 then Write(' ');
      Write(WfcTextEncodeToken(LTokens[I], 'studio demo'));
      if I mod LOptions.Width = LOptions.Width - 1 then WriteLn;
    end;
  finally
    LWorkspace.Free;
  end;
end;

procedure RunTrainingStudioDemo;
var
  LPreset: Integer;
  LSeed: TGraphSeed;
  I: Integer;
begin
  if (ParamCount = 1) and (ParamStr(1) = '--selftest') then
  begin
    for I := 0 to TRAINING_STUDIO_PRESET_COUNT - 1 do RunOne(I, 0);
    Exit;
  end;
  if ParamCount > 2 then
    raise Exception.Create('usage: TrainingStudio [preset 0..4] [decimal seed] | --selftest');
  LPreset := 2;
  LSeed := 0;
  if ParamCount >= 1 then
    LPreset := WfcTextParseCanonicalInteger(ParamStr(1), 'preset', 'studio');
  if ParamCount = 2 then
    LSeed := WfcTextParseCanonicalCardinal(ParamStr(2), 'seed', 'studio');
  RunOne(LPreset, LSeed);
end;

end.
