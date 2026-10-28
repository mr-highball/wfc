(*
MIT License

Copyright (c) 2021, 2026 mr-highball

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
unit wfc_voice_preparation_fixture;

{$mode delphi}
{$H+}

interface

uses wfc, wfc_model, wfc_sequence, wfc_sequence_graph,
  wfc_music, wfc_music_ensemble, wfc_music_voices_graph;

function PreparationFixture: TWfcMusicVoicesGraphConfig;
procedure FreePreparationFixture(var AConfig: TWfcMusicVoicesGraphConfig);
function PreparationBoundaries: TWfcMusicVoicesBoundaries;
function PreparationEvidence(const AGenerated: TWfcMusicVoicesGenerated): String;

implementation

uses SysUtils, wfc_sequence_learn, wfc_music_sequence, wfc_music_ensemble_graph;

function PreparationFixture: TWfcMusicVoicesGraphConfig;
var
  LVoice: Integer;
  LChoice: Integer;
  LTokens: TWfcModelTokens;
  LFrame: TWfcMusicEnsembleFrame;
begin
  Result := Default(TWfcMusicVoicesGraphConfig);
  try
    Result.StepsPerOctave := 12;
    Result.HarmonyMode := wmehmExact;
    Result.HarmonyModel := LearnSequenceModel([EncodeWfcMusicPitchClassSet(
      MakeWfcMusicPitchClassSet(12, [0]))], 1);
    Result.RhythmModel := LearnSequenceModel([EncodeWfcMusicRhythmFrame(
      MakeWfcMusicRhythmFrame([wmcaAttack, wmcaAttack]))], 1);
    SetLength(Result.Voices, 2);
    SetLength(LTokens, 65);
    SetLength(LFrame.Voices, 1);
    LFrame.Voices[0].Action := wmcaAttack;
    SetLength(LFrame.Voices[0].Tones, 1);
    for LVoice := 0 to 1 do
    begin
      for LChoice := 0 to High(LTokens) do
      begin
        LFrame.Voices[0].Tones[0] := MakeWfcMusicTone(48 + 12 * LVoice, LChoice + 1);
        LTokens[LChoice] := EncodeWfcMusicEnsembleFrame(LFrame);
      end;
      Result.Voices[LVoice].Model := LearnSequenceModel(LTokens, 1);
      Result.Voices[LVoice].MinPitch := 48 + 12 * LVoice;
      Result.Voices[LVoice].MaxPitch := 48 + 12 * LVoice;
    end;
    SetLength(Result.PairConstraints, 1);
    Result.PairConstraints[0].LowerVoice := 0;
    Result.PairConstraints[0].UpperVoice := 1;
    Result.PairConstraints[0].MinGap := 12;
    Result.PairConstraints[0].MaxGap := 12;
    Result.PairConstraints[0].RestPolicy := wmvprSuspend;
  except
    FreePreparationFixture(Result);
    raise;
  end;
end;

procedure FreePreparationFixture(var AConfig: TWfcMusicVoicesGraphConfig);
var
  LVoice: Integer;
begin
  for LVoice := 0 to High(AConfig.Voices) do
  begin
    AConfig.Voices[LVoice].Model.Free;
  end;
  AConfig.RhythmModel.Free;
  AConfig.HarmonyModel.Free;
  AConfig := Default(TWfcMusicVoicesGraphConfig);
end;

function PreparationBoundaries: TWfcMusicVoicesBoundaries;
var
  LModel: Integer;
begin
  Result := nil;
  SetLength(Result, 4);
  for LModel := 0 to High(Result) do
  begin
    Result[LModel] := MakeWfcSequenceInitialSegmentBoundary(False);
  end;
end;

function PreparationEvidence(const AGenerated: TWfcMusicVoicesGenerated): String;
var
  LLayer: Integer;
  LCell: Integer;
begin
  Result := '';
  for LLayer := 0 to High(AGenerated.Layers) do
  begin
    Result := Result + 'L' + IntToStr(LLayer) + ':' +
      IntToStr(Ord(AGenerated.Layers[LLayer].Boundary.HasPrevious)) + '/' +
      IntToStr(AGenerated.Layers[LLayer].Boundary.PreviousState) + '/' +
      IntToStr(Ord(AGenerated.Layers[LLayer].Boundary.RequireObservedEnd)) + ':';
    for LCell := 0 to High(AGenerated.Layers[LLayer].Tokens) do
    begin
      Result := Result + AGenerated.Layers[LLayer].Tokens[LCell] + '=' +
        IntToStr(AGenerated.Layers[LLayer].StateIndices[LCell]) + ',';
    end;
  end;
  for LCell := 0 to High(AGenerated.Frames) do
  begin
    Result := Result + EncodeWfcMusicEnsembleFrame(AGenerated.Frames[LCell]);
  end;
  for LLayer := 0 to High(AGenerated.Coverage) do
  begin
    Result := Result + 'C' + IntToStr(AGenerated.Coverage[LLayer].PitchClass) + ':';
    for LCell := 0 to High(AGenerated.Coverage[LLayer].Suppliers) do
    begin
      Result := Result + IntToStr(AGenerated.Coverage[LLayer].Suppliers[LCell]) + ',';
    end;
  end;
end;

end.
