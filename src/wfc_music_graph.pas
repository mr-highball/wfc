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
unit wfc_music_graph;

{$mode delphi}{$H+}

interface

uses
  wfc,
  wfc_sequence,
  wfc_sequence_graph;

const
  WFC_MUSIC_GRAPH_ADAPTER_VERSION = 1;

type
  EWfcMusicGraph = class(EWfcSequenceGraph);

function BuildWfcMusicRhythmProjectionRules(
  const AMelodyModel, ARhythmModel: TWfcSequenceModel):
  TWfcSequenceProjectionRules;

function BuildWfcMusicHarmonyProjectionRules(
  const AMelodyModel, AHarmonyModel: TWfcSequenceModel;
  const AStepsPerOctave: Integer): TWfcSequenceProjectionRules;

{ The active graph pass must contain AMelodyModel. Rhythm and harmony source
  passes must already contain their matching latent sequence models. The two
  projection maps are distinct named dependencies and are therefore ANDed at
  every cell, while each map can retain multiple source alternatives. }
procedure RequireWfcMusicMelodyFromPasses(
  const AMelodyModel, ARhythmModel,
  AHarmonyModel: TWfcSequenceModel; const AMelodyGraph: TGraph;
  const ARhythmPass, AHarmonyPass: String;
  const AStepsPerOctave: Integer);

implementation

uses
  SysUtils,
  wfc_model,
  wfc_music_sequence;

function TokensOfOne(const AToken: TWfcModelToken): TWfcModelTokens;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0] := AToken;
end;

function EncodeRhythmForAction(
  const AAction: TWfcMusicCellAction): TWfcModelToken;
var
  LCell: TWfcMusicRhythmCell;
begin
  LCell.Action := AAction;
  Result := EncodeWfcMusicRhythmCell(LCell);
end;

function BuildWfcMusicRhythmProjectionRules(
  const AMelodyModel, ARhythmModel: TWfcSequenceModel):
  TWfcSequenceProjectionRules;
var
  I: Integer;
  LCell: TWfcMusicMelodyCell;
  LRhythmToken: TWfcModelToken;
begin
  if not Assigned(AMelodyModel) then
    raise EArgumentNilException.Create('music melody model cannot be nil');
  if not Assigned(ARhythmModel) then
    raise EArgumentNilException.Create('music rhythm model cannot be nil');
  Result := nil;
  SetLength(Result, AMelodyModel.PublicTokenCount);
  for I := 0 to AMelodyModel.PublicTokenCount - 1 do
  begin
    LCell := DecodeWfcMusicMelodyCell(AMelodyModel.PublicTokenAt(I));
    LRhythmToken := EncodeRhythmForAction(LCell.Action);
    if ARhythmModel.FindPublicToken(LRhythmToken) < 0 then
      raise EWfcMusicGraph.CreateFmt(
        'rhythm model cannot project melody public token %d', [I]);
    Result[I] := MakeWfcSequenceProjectionRule(
      AMelodyModel.PublicTokenAt(I), TokensOfOne(LRhythmToken));
  end;
end;

function PositiveModulus(const AValue, AModulus: Integer): Integer;
begin
  Result := AValue mod AModulus;
  if Result < 0 then
    Inc(Result, AModulus);
end;

procedure AppendToken(var ATokens: TWfcModelTokens;
  const AToken: TWfcModelToken);
var
  LIndex: Integer;
begin
  LIndex := Length(ATokens);
  SetLength(ATokens, LIndex + 1);
  ATokens[LIndex] := AToken;
end;

function BuildWfcMusicHarmonyProjectionRules(
  const AMelodyModel, AHarmonyModel: TWfcSequenceModel;
  const AStepsPerOctave: Integer): TWfcSequenceProjectionRules;
var
  I: Integer;
  J: Integer;
  LAllowed: TWfcModelTokens;
  LHarmony: TWfcMusicHarmonyCell;
  LMelody: TWfcMusicMelodyCell;
  LPitchClass: Integer;
begin
  if not Assigned(AMelodyModel) then
    raise EArgumentNilException.Create('music melody model cannot be nil');
  if not Assigned(AHarmonyModel) then
    raise EArgumentNilException.Create('music harmony model cannot be nil');
  if AStepsPerOctave < 1 then
    raise EArgumentException.Create(
      'music steps per octave must be positive');

  Result := nil;
  SetLength(Result, AMelodyModel.PublicTokenCount);
  for I := 0 to AMelodyModel.PublicTokenCount - 1 do
  begin
    LMelody := DecodeWfcMusicMelodyCell(
      AMelodyModel.PublicTokenAt(I));
    LAllowed := nil;
    if LMelody.Action <> wmcaRest then
      LPitchClass := PositiveModulus(LMelody.Pitch,
        AStepsPerOctave)
    else
      LPitchClass := -1;
    for J := 0 to AHarmonyModel.PublicTokenCount - 1 do
    begin
      LHarmony := DecodeWfcMusicHarmonyCell(
        AHarmonyModel.PublicTokenAt(J));
      if LHarmony.StepsPerOctave <> AStepsPerOctave then
        raise EWfcMusicGraph.CreateFmt(
          'harmony public token %d uses %d steps per octave; expected %d',
          [J, LHarmony.StepsPerOctave, AStepsPerOctave]);
      if (LMelody.Action = wmcaRest) or
          ((LHarmony.Kind = wmhckPitchClass) and
           (LHarmony.PitchClass = LPitchClass)) then
        AppendToken(LAllowed, AHarmonyModel.PublicTokenAt(J));
    end;
    if Length(LAllowed) = 0 then
      raise EWfcMusicGraph.CreateFmt(
        'harmony model cannot project melody public token %d', [I]);
    Result[I] := MakeWfcSequenceProjectionRule(
      AMelodyModel.PublicTokenAt(I), LAllowed);
  end;
end;

procedure RequireWfcMusicMelodyFromPasses(
  const AMelodyModel, ARhythmModel,
  AHarmonyModel: TWfcSequenceModel; const AMelodyGraph: TGraph;
  const ARhythmPass, AHarmonyPass: String;
  const AStepsPerOctave: Integer);
var
  LBindings: TWfcSequenceProjectionBindings;
  LHarmonyRules: TWfcSequenceProjectionRules;
  LRhythmRules: TWfcSequenceProjectionRules;
begin
  if not Assigned(AMelodyGraph) then
    raise EArgumentNilException.Create('music melody graph cannot be nil');
  LRhythmRules := BuildWfcMusicRhythmProjectionRules(
    AMelodyModel, ARhythmModel);
  LHarmonyRules := BuildWfcMusicHarmonyProjectionRules(
    AMelodyModel, AHarmonyModel, AStepsPerOctave);
  SetLength(LBindings, 2);
  LBindings[0] := MakeWfcSequenceProjectionBinding(ARhythmModel,
    ARhythmPass, LRhythmRules);
  LBindings[1] := MakeWfcSequenceProjectionBinding(AHarmonyModel,
    AHarmonyPass, LHarmonyRules);
  RequireSequenceProjectionMapsFromPasses(AMelodyModel,
    AMelodyGraph, LBindings);
end;

end.
