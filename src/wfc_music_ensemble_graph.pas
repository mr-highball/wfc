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
unit wfc_music_ensemble_graph;

{$mode delphi}{$H+}

interface

uses SysUtils, wfc, wfc_sequence, wfc_sequence_graph, wfc_music_ensemble;

const
  WFC_MUSIC_ENSEMBLE_GRAPH_VERSION = 1;

type
  EWfcMusicEnsembleGraph = class(EWfcSequenceGraph);
  TWfcMusicEnsembleHarmonyMode = (wmehmExact, wmehmAllowed);

procedure ValidateWfcMusicEnsembleModel(const AModel: TWfcSequenceModel;
  const AVoiceCount: Integer);
procedure ValidateWfcMusicEnsembleRhythmModel(const AModel: TWfcSequenceModel;
  const AVoiceCount: Integer);
procedure ValidateWfcMusicEnsembleHarmonyModel(const AModel: TWfcSequenceModel;
  const AStepsPerOctave: Integer);
function BuildWfcMusicEnsembleRhythmProjectionRules(
  const AEnsembleModel, ARhythmModel: TWfcSequenceModel):
  TWfcSequenceProjectionRules;
function BuildWfcMusicEnsembleExactHarmonyProjectionRules(
  const AEnsembleModel, AHarmonyModel: TWfcSequenceModel;
  const AStepsPerOctave: Integer): TWfcSequenceProjectionRules;
function BuildWfcMusicEnsembleAllowedHarmonyProjectionRules(
  const AEnsembleModel, AHarmonyModel: TWfcSequenceModel;
  const AStepsPerOctave: Integer): TWfcSequenceProjectionRules;
procedure RequireWfcMusicEnsembleFromPasses(
  const AEnsembleModel, ARhythmModel, AHarmonyModel: TWfcSequenceModel;
  const AGraph: TGraph; const ARhythmPass, AHarmonyPass: String;
  const AVoiceCount, AStepsPerOctave: Integer;
  const AMode: TWfcMusicEnsembleHarmonyMode);

implementation

uses wfc_model;

procedure NeedModel(const AModel: TWfcSequenceModel);
begin
  if AModel = nil then
    raise EArgumentNilException.Create('ensemble sequence model cannot be nil');
end;

procedure ValidateWfcMusicEnsembleModel(const AModel: TWfcSequenceModel;
  const AVoiceCount: Integer);
var
  I, J, LPrevious, LCurrent, LPair: Integer;
  LFrames: TWfcMusicEnsembleFrames;
  LCanStart: array of Boolean;
  LCheckedPairs: array of Boolean;
begin
  NeedModel(AModel);
  if AVoiceCount < 1 then
    raise EArgumentException.Create('ensemble voice count must be positive');
  SetLength(LFrames, AModel.PublicTokenCount);
  SetLength(LCanStart, AModel.PublicTokenCount);
  //Several latent histories can emit the same public frame. Check every
  //structural edge, but prove a public continuation pair only once. The
  //sequence model already bounds its public vocabulary; no new music cap.
  SetLength(LCheckedPairs, AModel.PublicTokenCount * AModel.PublicTokenCount);
  for I := 0 to High(LFrames) do
  begin
    LFrames[I] := DecodeWfcMusicEnsembleFrame(AModel.PublicTokenAt(I));
    if Length(LFrames[I].Voices) <> AVoiceCount then
      raise EWfcMusicEnsembleGraph.CreateFmt(
        'ensemble token %d has the wrong voice count', [I]);
    LCanStart[I] := WfcMusicEnsembleFrameCanStart(LFrames[I]);
  end;
  for I := 0 to AModel.StateCount - 1 do
  begin
    LPrevious := AModel.StateEmittedTokenIndexAt(I);
    if (AModel.StartCountAt(I) > 0) and not LCanStart[LPrevious] then
      raise EWfcMusicEnsembleGraph.CreateFmt(
        'ensemble observed start state %d begins with a hold', [I]);
    for J := 0 to AModel.StateCount - 1 do
      if AModel.StatesCompatible(I, J) then
      begin
        LCurrent := AModel.StateEmittedTokenIndexAt(J);
        LPair := LPrevious * AModel.PublicTokenCount + LCurrent;
        if not LCheckedPairs[LPair] then
        begin
          if not WfcMusicEnsembleFrameCanFollow(LFrames[LPrevious],
            LFrames[LCurrent]) then
            raise EWfcMusicEnsembleGraph.CreateFmt(
              'ensemble structural edge %d -> %d has invalid hold continuation; increase context or repair the corpus',
              [I, J]);
          LCheckedPairs[LPair] := True;
        end;
      end;
  end;
end;

procedure ValidateWfcMusicEnsembleRhythmModel(const AModel: TWfcSequenceModel;
  const AVoiceCount: Integer);
var
  I: Integer;
  LFrame: TWfcMusicRhythmFrame;
begin
  NeedModel(AModel);
  if AVoiceCount < 1 then
    raise EArgumentException.Create('ensemble voice count must be positive');
  for I := 0 to AModel.PublicTokenCount - 1 do
  begin
    LFrame := DecodeWfcMusicRhythmFrame(AModel.PublicTokenAt(I));
    if Length(LFrame.Actions) <> AVoiceCount then
      raise EWfcMusicEnsembleGraph.CreateFmt(
        'ensemble rhythm token %d has the wrong voice count', [I]);
  end;
end;

procedure ValidateWfcMusicEnsembleHarmonyModel(const AModel: TWfcSequenceModel;
  const AStepsPerOctave: Integer);
var
  I: Integer;
  LSet: TWfcMusicPitchClassSet;
begin
  NeedModel(AModel);
  if AStepsPerOctave < 1 then
    raise EArgumentException.Create('ensemble pitchclass steps must be positive');
  for I := 0 to AModel.PublicTokenCount - 1 do
  begin
    LSet := DecodeWfcMusicPitchClassSet(AModel.PublicTokenAt(I));
    if LSet.StepsPerOctave <> AStepsPerOctave then
      raise EWfcMusicEnsembleGraph.CreateFmt(
        'ensemble harmony token %d uses another pitchclass system', [I]);
  end;
end;

function ModelVoiceCount(const AModel: TWfcSequenceModel): Integer;
var
  LFrame: TWfcMusicEnsembleFrame;
begin
  NeedModel(AModel);
  LFrame := DecodeWfcMusicEnsembleFrame(AModel.PublicTokenAt(0));
  Result := Length(LFrame.Voices);
end;

function BuildPreparedRhythmRules(
  const AEnsembleModel, ARhythmModel: TWfcSequenceModel):
  TWfcSequenceProjectionRules;
var
  I: Integer;
  LToken: TWfcModelToken;
  LSourceTokens: TWfcModelTokens;
begin
  Result := nil;
  SetLength(Result, AEnsembleModel.PublicTokenCount);
  SetLength(LSourceTokens, 1);
  for I := 0 to High(Result) do
  begin
    LToken := EncodeWfcMusicRhythmFrame(ProjectWfcMusicEnsembleFrameToRhythm(
      DecodeWfcMusicEnsembleFrame(AEnsembleModel.PublicTokenAt(I))));
    if ARhythmModel.FindPublicToken(LToken) < 0 then
      raise EWfcMusicEnsembleGraph.CreateFmt(
        'rhythm model cannot project ensemble token %d', [I]);
    LSourceTokens[0] := LToken;
    Result[I] := MakeWfcSequenceProjectionRule(
      AEnsembleModel.PublicTokenAt(I), LSourceTokens);
  end;
end;

function BuildWfcMusicEnsembleRhythmProjectionRules(
  const AEnsembleModel, ARhythmModel: TWfcSequenceModel):
  TWfcSequenceProjectionRules;
var
  LVoices: Integer;
begin
  LVoices := ModelVoiceCount(AEnsembleModel);
  ValidateWfcMusicEnsembleModel(AEnsembleModel, LVoices);
  ValidateWfcMusicEnsembleRhythmModel(ARhythmModel, LVoices);
  Result := BuildPreparedRhythmRules(AEnsembleModel, ARhythmModel);
end;

function BuildPreparedHarmonyRules(const AEnsembleModel,
  AHarmonyModel: TWfcSequenceModel; const AStepsPerOctave: Integer;
  const AMode: TWfcMusicEnsembleHarmonyMode): TWfcSequenceProjectionRules;
var
  I, J, LCount: Integer;
  LActual: TWfcMusicPitchClassSet;
  LSets: TWfcMusicPitchClassSets;
  LAllowed: TWfcModelTokens;
  LMatches: Boolean;
begin
  SetLength(LSets, AHarmonyModel.PublicTokenCount);
  for I := 0 to High(LSets) do
    LSets[I] := DecodeWfcMusicPitchClassSet(AHarmonyModel.PublicTokenAt(I));
  Result := nil;
  SetLength(Result, AEnsembleModel.PublicTokenCount);
  for I := 0 to High(Result) do
  begin
    LActual := ProjectWfcMusicEnsembleFrameToPitchClassSet(
      DecodeWfcMusicEnsembleFrame(AEnsembleModel.PublicTokenAt(I)),
      AStepsPerOctave);
    LAllowed := nil;
    for J := 0 to High(LSets) do
    begin
      if AMode = wmehmExact then
        LMatches := WfcMusicPitchClassSetsEqual(LActual, LSets[J])
      else
        LMatches := WfcMusicPitchClassSetIsSubset(LActual, LSets[J]);
      if LMatches then
      begin
        LCount := Length(LAllowed);
        SetLength(LAllowed, LCount + 1);
        LAllowed[LCount] := AHarmonyModel.PublicTokenAt(J);
      end;
    end;
    if Length(LAllowed) = 0 then
      raise EWfcMusicEnsembleGraph.CreateFmt(
        'harmony model cannot project ensemble token %d', [I]);
    Result[I] := MakeWfcSequenceProjectionRule(
      AEnsembleModel.PublicTokenAt(I), LAllowed);
  end;
end;

function BuildWfcMusicEnsembleExactHarmonyProjectionRules(
  const AEnsembleModel, AHarmonyModel: TWfcSequenceModel;
  const AStepsPerOctave: Integer): TWfcSequenceProjectionRules;
begin
  ValidateWfcMusicEnsembleModel(AEnsembleModel, ModelVoiceCount(AEnsembleModel));
  ValidateWfcMusicEnsembleHarmonyModel(AHarmonyModel, AStepsPerOctave);
  Result := BuildPreparedHarmonyRules(AEnsembleModel, AHarmonyModel,
    AStepsPerOctave, wmehmExact);
end;

function BuildWfcMusicEnsembleAllowedHarmonyProjectionRules(
  const AEnsembleModel, AHarmonyModel: TWfcSequenceModel;
  const AStepsPerOctave: Integer): TWfcSequenceProjectionRules;
begin
  ValidateWfcMusicEnsembleModel(AEnsembleModel, ModelVoiceCount(AEnsembleModel));
  ValidateWfcMusicEnsembleHarmonyModel(AHarmonyModel, AStepsPerOctave);
  Result := BuildPreparedHarmonyRules(AEnsembleModel, AHarmonyModel,
    AStepsPerOctave, wmehmAllowed);
end;

procedure RequireWfcMusicEnsembleFromPasses(
  const AEnsembleModel, ARhythmModel, AHarmonyModel: TWfcSequenceModel;
  const AGraph: TGraph; const ARhythmPass, AHarmonyPass: String;
  const AVoiceCount, AStepsPerOctave: Integer;
  const AMode: TWfcMusicEnsembleHarmonyMode);
var
  LBindings: TWfcSequenceProjectionBindings;
  LHarmony, LRhythm: TWfcSequenceProjectionRules;
begin
  if not (AMode in [wmehmExact, wmehmAllowed]) then
    raise EArgumentException.Create('unknown ensemble harmony mode');
  ValidateWfcMusicEnsembleModel(AEnsembleModel, AVoiceCount);
  ValidateWfcMusicEnsembleRhythmModel(ARhythmModel, AVoiceCount);
  ValidateWfcMusicEnsembleHarmonyModel(AHarmonyModel, AStepsPerOctave);
  LRhythm := BuildPreparedRhythmRules(AEnsembleModel, ARhythmModel);
  LHarmony := BuildPreparedHarmonyRules(AEnsembleModel, AHarmonyModel,
    AStepsPerOctave, AMode);
  SetLength(LBindings, 2);
  LBindings[0] := MakeWfcSequenceProjectionBinding(ARhythmModel, ARhythmPass, LRhythm);
  LBindings[1] := MakeWfcSequenceProjectionBinding(AHarmonyModel, AHarmonyPass, LHarmony);
  RequireSequenceProjectionMapsFromPasses(AEnsembleModel, AGraph, LBindings);
end;

end.
