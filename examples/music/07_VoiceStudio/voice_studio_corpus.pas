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
unit voice_studio_corpus;

{$mode delphi}{$H+}

interface

uses wfc_model, wfc_music, wfc_music_ensemble, wfc_music_ensemble_training,
  wfc_music_voices_training, wfc_sequence;

const
  VOICE_STUDIO_TPQ = 480;
  VOICE_STUDIO_QUANTUM = 240;
  VOICE_STUDIO_STEPS = 12;
  VOICE_STUDIO_ROLE_COUNT = 3;
  VOICE_STUDIO_EXCERPT_CELLS = 16;
  VOICE_STUDIO_CORPUS_COUNT = 2;

{ Every returned array, score, bundle and model is detached and caller-owned.
  Corpus indices are 0 (A) and 1 (B); role order is bass, chord, upper.
  Both authored excerpts repeat an eight-cell C-major/D-minor/rest phrase.
  Bass/chord actions: A H H H A H H R; upper: A H A H A H A R.
  Velocities are fixed by role: 72, 64, 96. Chords have two sorted tones.
  Per-role A/B choices produce eight vertical arrangements; exact harmonic
  coverage permits six (four unseen), rejecting [0,0,1] and [1,1,0].
  This deliberately small proof corpus is not a claim of musical breadth. }
function VoiceStudioCorpus(const AIndex: Integer): TWfcMusicEnsembleFrames;
function BuildVoiceStudioScore: TWfcMusicScore;
function VoiceStudioSelections: TWfcMusicEnsembleTrainingSelections;
function VoiceStudioRoles: TWfcMusicVoiceTrainingRoles;
function BuildVoiceStudioTrainingBundle: TWfcMusicVoicesTrainingBundle;
procedure BuildVoiceStudioModels(out AHarmony, ARhythm: TWfcSequenceModel;
  out AVoices: TWfcMusicVoiceSequenceModels);
function VoiceStudioRoleName(const AIndex: Integer): TWfcModelToken;
function VoiceStudioRoleMinimumPitch(const AIndex: Integer): Integer;
function VoiceStudioRoleMaximumPitch(const AIndex: Integer): Integer;
function VoiceStudioRoleToneCapacity(const AIndex: Integer): Integer;
function VoiceStudioRecombination(const AChoices: array of Integer): TWfcMusicEnsembleFrames;
{ Membership compares ordered per-role pitch sets only. Actions and velocities
  cannot create a cosmetic novelty claim. Silence is an observed vertical. }
function VoiceStudioVerticalWasObserved(const AFrame: TWfcMusicEnsembleFrame): Boolean;

implementation

uses SysUtils, wfc_music_sequence, wfc_training, wfc_sequence_text;

procedure CheckIndex(const AValue, ACount: Integer);
begin
  {$IFDEF PAS2JS}
  if (AValue <> AValue) or (AValue < 0) or (AValue >= ACount) then
    raise EWfcMusicVoicesTraining.Create('voice studio index out of range');
  if AValue <> Trunc(AValue) then
    raise EWfcMusicVoicesTraining.Create('voice studio index must be an integer');
  {$ELSE}
  if (AValue < 0) or (AValue >= ACount) then
    raise EWfcMusicVoicesTraining.Create('voice studio index out of range');
  {$ENDIF}
end;

function VoiceStudioRoleName(const AIndex: Integer): TWfcModelToken;
begin
  CheckIndex(AIndex, VOICE_STUDIO_ROLE_COUNT);
  case AIndex of 0: Result := 'bass'; 1: Result := 'chord'; else Result := 'upper'; end;
end;

function VoiceStudioRoleMinimumPitch(const AIndex: Integer): Integer;
begin
  CheckIndex(AIndex, VOICE_STUDIO_ROLE_COUNT);
  case AIndex of 0: Result := 48; 1: Result := 60; else Result := 72; end;
end;

function VoiceStudioRoleMaximumPitch(const AIndex: Integer): Integer;
begin
  CheckIndex(AIndex, VOICE_STUDIO_ROLE_COUNT);
  case AIndex of 0: Result := 57; 1: Result := 69; else Result := 81; end;
end;

function VoiceStudioRoleToneCapacity(const AIndex: Integer): Integer;
begin
  CheckIndex(AIndex, VOICE_STUDIO_ROLE_COUNT);
  if AIndex = 1 then Result := 2 else Result := 1;
end;

function VoiceStudioCorpus(const AIndex: Integer): TWfcMusicEnsembleFrames;
var Cell, Phase, Role, Base, UpperPitch, Velocity, T: Integer;
  Action: TWfcMusicCellAction; Tones: TWfcMusicTones;
begin
  CheckIndex(AIndex, VOICE_STUDIO_CORPUS_COUNT);
  Result := nil; SetLength(Result, VOICE_STUDIO_EXCERPT_CELLS);
  for Cell := 0 to High(Result) do
  begin
    Phase := Cell mod 8;
    SetLength(Result[Cell].Voices, VOICE_STUDIO_ROLE_COUNT);
    for Role := 0 to VOICE_STUDIO_ROLE_COUNT - 1 do
    begin
      if Phase = 7 then
      begin Result[Cell].Voices[Role] := MakeWfcMusicRestVoiceCell; Continue; end;
      if (Phase = 0) or (Phase = 4) or
        ((Role = 2) and ((Phase = 2) or (Phase = 6))) then Action := wmcaAttack
      else Action := wmcaHold;
      case Role of
        0: begin
          if AIndex = 0 then Base := 48 else Base := 55;
          UpperPitch := Base; Velocity := 72;
        end;
        1: begin
          if AIndex = 0 then begin Base := 60; UpperPitch := 64; end
          else begin Base := 64; UpperPitch := 67; end;
          Velocity := 64;
        end;
      else begin
        if AIndex = 0 then Base := 79 else Base := 72;
        UpperPitch := Base; Velocity := 96;
      end;
      end;
      if Phase >= 4 then
      begin
        if Role = 1 then
        begin
          if AIndex = 0 then begin Base := 62; UpperPitch := 65; end
          else begin Base := 65; UpperPitch := 69; end;
        end
        else begin Inc(Base, 2); Inc(UpperPitch, 2); end;
      end;
      SetLength(Tones, VoiceStudioRoleToneCapacity(Role));
      for T := 0 to High(Tones) do
        if T = 0 then Tones[T] := MakeWfcMusicTone(Base, Velocity)
        else Tones[T] := MakeWfcMusicTone(UpperPitch, Velocity);
      Result[Cell].Voices[Role] := MakeWfcMusicVoiceCell(Action, Tones);
    end;
  end;
  ValidateWfcMusicEnsembleFrames(Result);
end;

function BuildVoiceStudioScore: TWfcMusicScore;
var Frames, Part: TWfcMusicEnsembleFrames; I, J: Integer;
  Tracks: TWfcMusicTracks; Voices: TWfcMusicVoices;
  Meters: TWfcMusicMeterChanges; Tempos: TWfcMusicTempoChanges;
begin
  SetLength(Frames, VOICE_STUDIO_CORPUS_COUNT * VOICE_STUDIO_EXCERPT_CELLS);
  for I := 0 to VOICE_STUDIO_CORPUS_COUNT - 1 do
  begin
    Part := VoiceStudioCorpus(I);
    for J := 0 to High(Part) do Frames[I * VOICE_STUDIO_EXCERPT_CELLS + J] := Part[J];
  end;
  SetLength(Tracks, 1); Tracks[0] := MakeWfcMusicTrack('voice-studio', 'Independent roles');
  SetLength(Voices, VOICE_STUDIO_ROLE_COUNT);
  for I := 0 to High(Voices) do Voices[I] := MakeWfcMusicVoice(0, VoiceStudioRoleName(I));
  SetLength(Meters, 1); Meters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(Tempos, 1); Tempos[0] := MakeWfcMusicTempoChange(0, 500000);
  Result := TWfcMusicScore.Create(VOICE_STUDIO_TPQ, VOICE_STUDIO_STEPS,
    Length(Frames) * VOICE_STUDIO_QUANTUM, Tracks, Voices, Meters, Tempos,
    RebuildWfcMusicEnsembleSpans(Frames, VOICE_STUDIO_QUANTUM));
end;

function VoiceStudioSelections: TWfcMusicEnsembleTrainingSelections;
begin
  Result := nil; SetLength(Result, 2);
  Result[0] := MakeWfcMusicEnsembleTrainingSelection('excerpt-a', 0, 3840);
  Result[1] := MakeWfcMusicEnsembleTrainingSelection('excerpt-b', 3840, 3840);
end;

function VoiceStudioRoles: TWfcMusicVoiceTrainingRoles;
var I: Integer;
begin
  Result := nil; SetLength(Result, VOICE_STUDIO_ROLE_COUNT);
  for I := 0 to High(Result) do
  begin Result[I].Id := VoiceStudioRoleName(I); Result[I].SourceVoiceIndex := I;
    Result[I].Order := 2; end;
end;

function BuildVoiceStudioTrainingBundle: TWfcMusicVoicesTrainingBundle;
var Score: TWfcMusicScore;
begin
  Score := BuildVoiceStudioScore;
  try
    Result := BuildWfcMusicVoicesTrainingBundle(Score, VoiceStudioRoles,
      VoiceStudioSelections, VOICE_STUDIO_QUANTUM, 8, 8,
      MakeWfcTrainingMetadata('Voice Studio A/B', 'MIT',
        'Project-authored two-excerpt independent-role recombination proof'));
  finally Score.Free; end;
end;

procedure BuildVoiceStudioModels(out AHarmony, ARhythm: TWfcSequenceModel;
  out AVoices: TWfcMusicVoiceSequenceModels);
var Bundle: TWfcMusicVoicesTrainingBundle; Doc: TWfcTrainingDocument; I: Integer;
begin
  AHarmony := nil; ARhythm := nil; AVoices := nil; Bundle := nil; Doc := nil;
  try
    try
      Bundle := BuildVoiceStudioTrainingBundle;
      Doc := Bundle.CopyHarmonyDocument;
      AHarmony := DecodeWfcSequenceText(LearnWfcTrainingModelText(Doc)); FreeAndNil(Doc);
      Doc := Bundle.CopyRhythmDocument;
      ARhythm := DecodeWfcSequenceText(LearnWfcTrainingModelText(Doc)); FreeAndNil(Doc);
      SetLength(AVoices, Bundle.RoleCount);
      for I := 0 to High(AVoices) do
      begin
        Doc := Bundle.CopyVoiceDocument(I);
        AVoices[I] := DecodeWfcSequenceText(LearnWfcTrainingModelText(Doc)); FreeAndNil(Doc);
      end;
    except
      FreeAndNil(AHarmony); FreeAndNil(ARhythm);
      for I := 0 to High(AVoices) do AVoices[I].Free;
      AVoices := nil; raise;
    end;
  finally Doc.Free; Bundle.Free; end;
end;

function VoiceStudioRecombination(const AChoices: array of Integer): TWfcMusicEnsembleFrames;
var I, J: Integer; A, B: TWfcMusicEnsembleFrames;
begin
  if Length(AChoices) <> VOICE_STUDIO_ROLE_COUNT then
    raise EWfcMusicVoicesTraining.Create('voice studio needs exactly three choices');
  for I := 0 to High(AChoices) do CheckIndex(AChoices[I], VOICE_STUDIO_CORPUS_COUNT);
  A := VoiceStudioCorpus(0); B := VoiceStudioCorpus(1);
  Result := nil; SetLength(Result, VOICE_STUDIO_EXCERPT_CELLS);
  for I := 0 to High(Result) do
  begin
    SetLength(Result[I].Voices, VOICE_STUDIO_ROLE_COUNT);
    for J := 0 to High(AChoices) do
      if AChoices[J] = 0 then Result[I].Voices[J] := A[I].Voices[J]
      else Result[I].Voices[J] := B[I].Voices[J];
  end;
end;

function SameVertical(const A, B: TWfcMusicEnsembleFrame): Boolean;
var I, J: Integer;
begin
  if Length(A.Voices) <> Length(B.Voices) then Exit(False);
  for I := 0 to High(A.Voices) do
  begin
    if Length(A.Voices[I].Tones) <> Length(B.Voices[I].Tones) then Exit(False);
    for J := 0 to High(A.Voices[I].Tones) do
      if A.Voices[I].Tones[J].Pitch <> B.Voices[I].Tones[J].Pitch then Exit(False);
  end;
  Result := True;
end;

function VoiceStudioVerticalWasObserved(const AFrame: TWfcMusicEnsembleFrame): Boolean;
var I, J: Integer; Frames: TWfcMusicEnsembleFrames;
begin
  EncodeWfcMusicEnsembleFrame(AFrame);
  if Length(AFrame.Voices) <> VOICE_STUDIO_ROLE_COUNT then
    raise EWfcMusicVoicesTraining.Create('voice studio vertical has wrong role count');
  for I := 0 to VOICE_STUDIO_CORPUS_COUNT - 1 do
  begin
    Frames := VoiceStudioCorpus(I);
    for J := 0 to High(Frames) do if SameVertical(AFrame, Frames[J]) then Exit(True);
  end;
  Result := False;
end;

end.
