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
unit ensemble_studio_profiles;

{$mode delphi}{$H+}

interface

uses
  SysUtils, wfc, wfc_model, wfc_sequence, wfc_music,
  wfc_music_sequence, wfc_music_ensemble, wfc_music_ensemble_passes,
  wfc_music_arrangement, wfc_music_form;

const
  ENSEMBLE_STUDIO_PROFILE_VERSION = 1;
  ENSEMBLE_STUDIO_DEVELOPED_REALIZATION_COUNT = 24;
  ENSEMBLE_STUDIO_PROFILE_CELLS_PER_BAR = 8;

type
  EEnsembleStudioProfile = class(Exception);
  TEnsembleStudioProfile = (espStructuralV1, espDevelopedPeriodV1);

function EnsembleStudioProfileName(const AProfile: TEnsembleStudioProfile): String;
function ParseEnsembleStudioProfile(const AText: String): TEnsembleStudioProfile;
function EnsembleStudioProfileOrder(const AProfile: TEnsembleStudioProfile): Integer;
function EnsembleStudioProfileCorpusCount(const AProfile: TEnsembleStudioProfile): Integer;
function EnsembleStudioProfileCorpus(const AProfile: TEnsembleStudioProfile;
  const AIndex: Integer): TWfcMusicEnsembleFrames;
function BuildEnsembleStudioProfileModels(const AProfile: TEnsembleStudioProfile):
  TWfcMusicEnsembleModels;
procedure FreeEnsembleStudioProfileModels(var AModels: TWfcMusicEnsembleModels);
function EnsembleStudioDevelopedFormConfig(
  const ATotalCells: TWfcMusicArrangementWide; const ASeed: TGraphSeed): TWfcMusicFormConfig;
{ These return detached exact acoustic realizations. They validate catalog,
  role/cadence/motif identity and local CellCount, not the absolute coordinates
  or inter-bar grammar; those belong to the authenticated form plan/adapter. }
function EnsembleStudioPlannedFrames(const ABar: TWfcMusicFormBar): TWfcMusicEnsembleFrames;
function EnsembleStudioPlannedTokens(const ABar: TWfcMusicFormBar;
  const ALayer: TWfcMusicEnsembleLayer): TWfcModelTokens;
function ValidateEnsembleStudioPlannedFrames(const ABar: TWfcMusicFormBar;
  const AFrames: TWfcMusicEnsembleFrames; out AFailure: String): Boolean;

implementation

uses wfc_sequence_learn;

const
  LEGACY_ROOTS: array[0..3] of Integer = (36, 41, 43, 45);
  LEGACY_CHORDS: array[0..3, 0..2] of Integer =
    ((48,52,55), (53,57,60), (55,59,62), (57,60,64));
  LEGACY_UPPER: array[0..3, 0..3] of Integer =
    ((60,64,67,64), (65,69,72,69), (67,71,74,71), (69,72,76,72));
  ROOTS: array[0..4] of Integer = (36, 45, 41, 38, 43);
  CHORDS: array[0..4, 0..2] of Integer =
    ((48,52,55), (48,52,57), (48,53,57), (50,53,57), (47,50,55));
  HARMONY_NAMES: array[0..4] of String = ('I', 'vi', 'IV', 'ii', 'V');
  HARMONY_FUNCTIONS: array[0..4] of TWfcMusicFormFunction =
    (wmffTonic, wmffExpansion, wmffPredominant, wmffPredominant, wmffDominant);
  GESTURE_NAMES: array[0..7] of String = ('theme-rise', 'theme-answer',
    'theme-half', 'theme-close', 'contrast-rise', 'contrast-fall',
    'contrast-half', 'contrast-close');
  NONCADENCE_GESTURES: array[0..3] of Integer = (0, 1, 4, 5);

function IntegerInRange(const AValue, AMinimum, AMaximum: Integer): Boolean;
begin
  {$IFDEF PAS2JS}
  asm
    Result = typeof AValue === 'number' && isFinite(AValue) &&
      Math.floor(AValue) === AValue && AValue >= AMinimum && AValue <= AMaximum;
  end;
  {$ELSE}
  Result := (AValue >= AMinimum) and (AValue <= AMaximum);
  {$ENDIF}
end;

function EnsembleStudioProfileName(const AProfile: TEnsembleStudioProfile): String;
begin
  case AProfile of
    espStructuralV1: Result := 'structural-v1';
    espDevelopedPeriodV1: Result := 'developed-period-v1';
  else raise EEnsembleStudioProfile.Create('unknown ensemble profile'); end;
end;

function ParseEnsembleStudioProfile(const AText: String): TEnsembleStudioProfile;
begin
  if AText = 'structural-v1' then Exit(espStructuralV1);
  if AText = 'developed-period-v1' then Exit(espDevelopedPeriodV1);
  raise EEnsembleStudioProfile.Create('unknown ensemble profile: ' + AText);
end;

function EnsembleStudioProfileOrder(const AProfile: TEnsembleStudioProfile): Integer;
begin
  EnsembleStudioProfileName(AProfile);
  if AProfile = espStructuralV1 then Result := 8 else Result := 2;
end;

function EnsembleStudioProfileCorpusCount(const AProfile: TEnsembleStudioProfile): Integer;
begin
  EnsembleStudioProfileName(AProfile);
  if AProfile = espStructuralV1 then Result := 16
  else Result := ENSEMBLE_STUDIO_DEVELOPED_REALIZATION_COUNT * 2;
end;

function Voice(const AAction: TWfcMusicCellAction; const APitches: array of Integer;
  const AVelocity: Integer): TWfcMusicVoiceCell;
var I: Integer; T: TWfcMusicTones;
begin
  T := nil;
  SetLength(T, Length(APitches));
  for I := 0 to High(T) do T[I] := MakeWfcMusicTone(APitches[I], AVelocity);
  Result := MakeWfcMusicVoiceCell(AAction, T);
end;

function RestFrame: TWfcMusicEnsembleFrame;
var V: TWfcMusicVoiceCells; I: Integer;
begin
  SetLength(V, 3);
  for I := 0 to 2 do V[I] := MakeWfcMusicRestVoiceCell;
  Result := MakeWfcMusicEnsembleFrame(V);
end;

{ Preserve the original corpus's order, pitches, actions and velocities.
  The legacy public wrappers may delegate here without changing fingerprints. }
function LegacyFrame(const ARoot, APosition, AVariant: Integer): TWfcMusicEnsembleFrame;
var V: TWfcMusicVoiceCells; P: Integer; A: TWfcMusicCellAction;
begin
  if APosition = 7 then Exit(RestFrame);
  SetLength(V, 3);
  if APosition = 0 then A := wmcaAttack else A := wmcaHold;
  V[0] := Voice(A, [LEGACY_ROOTS[ARoot]], 72);
  if APosition in [0,1,4,5] then
  begin
    if APosition in [0,4] then A := wmcaAttack else A := wmcaHold;
    V[1] := Voice(A, [LEGACY_CHORDS[ARoot,0], LEGACY_CHORDS[ARoot,1],
      LEGACY_CHORDS[ARoot,2]], 64);
  end else V[1] := MakeWfcMusicRestVoiceCell;
  case APosition of
    0,1: P := LEGACY_CHORDS[ARoot, AVariant mod 3] + 12;
    2,3: P := LEGACY_UPPER[ARoot,1];
    4,5: P := LEGACY_CHORDS[ARoot, (AVariant + 1) mod 3] + 12;
  else P := LEGACY_UPPER[ARoot,3]; end;
  if APosition mod 2 = 0 then A := wmcaAttack else A := wmcaHold;
  V[2] := Voice(A, [P], 96);
  Result := MakeWfcMusicEnsembleFrame(V);
end;

procedure RealizationIndices(const AIndex: Integer; out AHarmony, AGesture: Integer);
begin
  if not IntegerInRange(AIndex, 0, ENSEMBLE_STUDIO_DEVELOPED_REALIZATION_COUNT - 1) then
    raise EEnsembleStudioProfile.Create('unknown developed realization');
  if AIndex < 20 then
  begin
    AHarmony := AIndex div 4;
    AGesture := NONCADENCE_GESTURES[AIndex mod 4];
  end else
  begin
    case AIndex of
      20: begin AHarmony := 4; AGesture := 2; end;
      21: begin AHarmony := 0; AGesture := 3; end;
      22: begin AHarmony := 4; AGesture := 6; end;
    else begin AHarmony := 0; AGesture := 7; end; end;
  end;
end;

{ Original eight-cell motifs, transformed by chord-slot placement. The upper
  line is genuinely rearticulated: holds always preserve pitch AND velocity.
  Every complete bar releases at cell 7. This intentional breath makes the
  observed Order-2 seam finite; the form planner carries melodic/harmonic
  anchors across that silence rather than asking local history to remember it. }
function DevelopedFrames(const ARealization: Integer): TWfcMusicEnsembleFrames;
const
  RISE: array[0..6] of Integer = (0,0,1,1,2,2,1);
  ANSWER: array[0..6] of Integer = (2,2,2,1,0,0,1);
  CONTRAST_RISE: array[0..6] of Integer = (0,1,2,1,0,0,2);
  CONTRAST_FALL: array[0..6] of Integer = (2,1,0,1,2,2,0);
var H, G, I, P, Slot, ChordStart: Integer; V: TWfcMusicVoiceCells;
  A: TWfcMusicCellAction; Cadence, UpperAttack: Boolean;
begin
  RealizationIndices(ARealization, H, G);
  Cadence := G mod 4 >= 2;
  Result := nil;
  SetLength(Result, 8);
  SetLength(V, 3);
  for I := 0 to 6 do
  begin
    if I = 0 then A := wmcaAttack else A := wmcaHold;
    V[0] := Voice(A, [ROOTS[H]], 72);
    if Cadence or (I < 2) then
      V[1] := Voice(A, [CHORDS[H,0], CHORDS[H,1], CHORDS[H,2]], 64)
    else
    begin
      if G = 1 then ChordStart := 3 else ChordStart := 4;
      if I < ChordStart then V[1] := MakeWfcMusicRestVoiceCell
      else
      begin
        if I = ChordStart then A := wmcaAttack else A := wmcaHold;
        V[1] := Voice(A, [CHORDS[H,1], CHORDS[H,2]], 64);
      end;
    end;
    UpperAttack := False;
    if Cadence then
    begin
      UpperAttack := I in [0,2];
      if G mod 4 = 2 then
      begin if I < 2 then P := 67 else P := 62; end
      else begin if I < 2 then P := 64 else P := 60; end;
    end else
    begin
      case G of
        0: begin Slot := RISE[I]; UpperAttack := I in [0,2,4,6]; end;
        1: begin Slot := ANSWER[I]; UpperAttack := I in [0,3,4,6]; end;
        4: begin Slot := CONTRAST_RISE[I]; UpperAttack := I in [0,1,2,3,4,6]; end;
      else begin Slot := CONTRAST_FALL[I]; UpperAttack := I in [0,1,2,3,4,6]; end;
      end;
      P := CHORDS[H,Slot] + 12;
    end;
    if UpperAttack then A := wmcaAttack else A := wmcaHold;
    V[2] := Voice(A, [P], 96);
    Result[I] := MakeWfcMusicEnsembleFrame(V);
  end;
  Result[7] := RestFrame;
  ValidateWfcMusicEnsembleFrames(Result);
end;

function EnsembleStudioProfileCorpus(const AProfile: TEnsembleStudioProfile;
  const AIndex: Integer): TWfcMusicEnsembleFrames;
var I, Offset: Integer; F: TWfcMusicEnsembleFrames;
begin
  if not IntegerInRange(AIndex, 0, EnsembleStudioProfileCorpusCount(AProfile) - 1) then
    raise EEnsembleStudioProfile.Create('unknown ensemble profile corpus');
  Result := nil;
  if AProfile = espStructuralV1 then
  begin
    SetLength(Result, 16);
    for I := 0 to 15 do Result[I] := LegacyFrame(AIndex div 4, I mod 8, AIndex mod 4);
  end else
  begin
    F := DevelopedFrames(AIndex mod ENSEMBLE_STUDIO_DEVELOPED_REALIZATION_COUNT);
    { Standalone bars teach legal starts. Separate release-prefixed fragments
      teach only actual release->attack pairs, not a Cartesian adjacency table. }
    Offset := AIndex div ENSEMBLE_STUDIO_DEVELOPED_REALIZATION_COUNT;
    SetLength(Result, 8 + Offset);
    if Offset = 1 then Result[0] := RestFrame;
    for I := 0 to 7 do Result[I + Offset] := F[I];
  end;
  ValidateWfcMusicEnsembleFrames(Result);
end;

procedure FreeEnsembleStudioProfileModels(var AModels: TWfcMusicEnsembleModels);
begin
  AModels.Ensemble.Free;
  AModels.Rhythm.Free;
  AModels.Harmony.Free;
  AModels := Default(TWfcMusicEnsembleModels);
end;

function BuildEnsembleStudioProfileModels(const AProfile: TEnsembleStudioProfile):
  TWfcMusicEnsembleModels;
var H, R, E: TWfcSequenceSamples; F: TWfcMusicEnsembleFrames; I, N: Integer;
begin
  Result := Default(TWfcMusicEnsembleModels);
  N := EnsembleStudioProfileCorpusCount(AProfile);
  SetLength(H, N); SetLength(R, N); SetLength(E, N);
  for I := 0 to N - 1 do
  begin
    F := EnsembleStudioProfileCorpus(AProfile, I);
    E[I] := MakeWfcSequenceSample(EncodeWfcMusicEnsembleFrames(F));
    R[I] := MakeWfcSequenceSample(EncodeWfcMusicRhythmFrames(
      ProjectWfcMusicEnsembleFramesToRhythm(F)));
    H[I] := MakeWfcSequenceSample(EncodeWfcMusicPitchClassSets(
      ProjectWfcMusicEnsembleFramesToPitchClassSets(F, 12)));
  end;
  try
    Result.Harmony := LearnSequenceModelCorpus(H, EnsembleStudioProfileOrder(AProfile));
    Result.Rhythm := LearnSequenceModelCorpus(R, EnsembleStudioProfileOrder(AProfile));
    Result.Ensemble := LearnSequenceModelCorpus(E, EnsembleStudioProfileOrder(AProfile));
  except
    FreeEnsembleStudioProfileModels(Result);
    raise;
  end;
end;

function EnsembleStudioDevelopedFormConfig(
  const ATotalCells: TWfcMusicArrangementWide; const ASeed: TGraphSeed): TWfcMusicFormConfig;
var I, J, H, G, Attacks: Integer; F: TWfcMusicEnsembleFrames;
begin
  Result := DefaultWfcMusicFormConfig(ATotalCells, ASeed);
  Result.CellsPerBar := 8;
  Result.PhraseBars := 4;
  Result.ContrastMinimumAttacks := 6;
  SetLength(Result.Harmonies, 5);
  for I := 0 to 4 do
  begin
    Result.Harmonies[I].LabelText := HARMONY_NAMES[I];
    Result.Harmonies[I].HarmonicFunction := HARMONY_FUNCTIONS[I];
    SetLength(Result.Harmonies[I].MotionPitches, 3);
    for J := 0 to 2 do Result.Harmonies[I].MotionPitches[J] := CHORDS[I,J];
  end;
  SetLength(Result.Gestures, 8);
  for I := 0 to 7 do
  begin
    Result.Gestures[I].LabelText := GESTURE_NAMES[I];
    Result.Gestures[I].MotifIndex := I div 4;
    if I < 4 then Result.Gestures[I].Roles := [wmfrQuestion,wmfrAnswer,wmfrReturn]
    else Result.Gestures[I].Roles := [wmfrContrast];
    case I mod 4 of
      2: Result.Gestures[I].Cadences := [wmfcHalf];
      3: Result.Gestures[I].Cadences := [wmfcAuthentic];
    else Result.Gestures[I].Cadences := [wmfcNone]; end;
    { AttackCount is the number of UPPER-LINE attacks, never all voices. }
    if I mod 4 >= 2 then Attacks := 2
    else if I < 4 then Attacks := 4 else Attacks := 6;
    Result.Gestures[I].AttackCount := Attacks;
  end;
  SetLength(Result.Realizations, ENSEMBLE_STUDIO_DEVELOPED_REALIZATION_COUNT);
  for I := 0 to High(Result.Realizations) do
  begin
    RealizationIndices(I, H, G);
    Result.Realizations[I].HarmonyIndex := H;
    Result.Realizations[I].GestureIndex := G;
    F := DevelopedFrames(I);
    Result.Realizations[I].EntryPitch := F[0].Voices[2].Tones[0].Pitch;
    for J := 0 to 7 do
      if F[J].Voices[2].Action = wmcaAttack then
        Result.Realizations[I].ExitPitch := F[J].Voices[2].Tones[0].Pitch;
  end;
  ValidateWfcMusicFormConfig(Result);
end;

procedure CheckPlannedBar(const ABar: TWfcMusicFormBar);
var H, G: Integer;
begin
  RealizationIndices(ABar.RealizationIndex, H, G);
  if not IntegerInRange(ABar.CellCount, 1, 8) then
    raise EEnsembleStudioProfile.Create('planned bar cell count is outside 1..8');
  if (ABar.HarmonyIndex <> H) or (ABar.GestureIndex <> G) or
      (ABar.HarmonicFunction <> HARMONY_FUNCTIONS[H]) or
      (ABar.MotifIndex <> G div 4) then
    raise EEnsembleStudioProfile.Create('planned bar disagrees with its realization');
  if ((G < 4) and not (ABar.Role in [wmfrQuestion,wmfrAnswer,wmfrReturn])) or
      ((G >= 4) and (ABar.Role <> wmfrContrast)) then
    raise EEnsembleStudioProfile.Create('planned bar role disagrees with its motif');
  case G mod 4 of
    2: if ABar.Cadence <> wmfcHalf then
      raise EEnsembleStudioProfile.Create('planned half cadence disagrees with gesture');
    3: if ABar.Cadence <> wmfcAuthentic then
      raise EEnsembleStudioProfile.Create('planned authentic cadence disagrees with gesture');
  else if ABar.Cadence <> wmfcNone then
    raise EEnsembleStudioProfile.Create('planned ordinary gesture claims a cadence'); end;
end;

function EnsembleStudioPlannedFrames(const ABar: TWfcMusicFormBar): TWfcMusicEnsembleFrames;
begin
  CheckPlannedBar(ABar);
  Result := DevelopedFrames(ABar.RealizationIndex);
  SetLength(Result, ABar.CellCount);
end;

function EnsembleStudioPlannedTokens(const ABar: TWfcMusicFormBar;
  const ALayer: TWfcMusicEnsembleLayer): TWfcModelTokens;
var F: TWfcMusicEnsembleFrames;
begin
  F := EnsembleStudioPlannedFrames(ABar);
  case ALayer of
    wmelHarmony: Result := EncodeWfcMusicPitchClassSets(
      ProjectWfcMusicEnsembleFramesToPitchClassSets(F, 12));
    wmelRhythm: Result := EncodeWfcMusicRhythmFrames(ProjectWfcMusicEnsembleFramesToRhythm(F));
    wmelEnsemble: Result := EncodeWfcMusicEnsembleFrames(F);
  else raise EEnsembleStudioProfile.Create('unknown planned music layer'); end;
end;

function ValidateEnsembleStudioPlannedFrames(const ABar: TWfcMusicFormBar;
  const AFrames: TWfcMusicEnsembleFrames; out AFailure: String): Boolean;
var E: TWfcMusicEnsembleFrames; I: Integer;
begin
  Result := False;
  AFailure := '';
  try
    E := EnsembleStudioPlannedFrames(ABar);
    if Length(AFrames) <> Length(E) then
      raise EEnsembleStudioProfile.Create('realized bar has the wrong cell count');
    ValidateWfcMusicEnsembleFrames(AFrames);
    for I := 0 to High(E) do
      if EncodeWfcMusicEnsembleFrame(AFrames[I]) <> EncodeWfcMusicEnsembleFrame(E[I]) then
        raise EEnsembleStudioProfile.CreateFmt('realized bar differs from plan at cell %d', [I]);
    Result := True;
  except on X: Exception do AFailure := X.Message; end;
end;

end.
