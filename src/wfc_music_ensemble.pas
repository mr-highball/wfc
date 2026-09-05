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
unit wfc_music_ensemble;

{$mode delphi}{$H+}

interface

uses wfc_model, wfc_music, wfc_music_sequence;

const
  WFC_MUSIC_ENSEMBLE_VERSION = 1;
  WFC_MUSIC_ENSEMBLE_TOKEN_VERSION = 1;

type
  EWfcMusicEnsemble = class(EWfcMusicSequence);
  { One voice may sustain a complete chord independently of every other voice.
    Tone order is canonical pitch order, not performance/voicing order. }
  TWfcMusicVoiceCell = record
    Action: TWfcMusicCellAction;
    Tones: TWfcMusicTones;
  end;
  TWfcMusicVoiceCells = array of TWfcMusicVoiceCell;
  TWfcMusicEnsembleFrame = record
    Voices: TWfcMusicVoiceCells;
  end;
  TWfcMusicEnsembleFrames = array of TWfcMusicEnsembleFrame;
  TWfcMusicCellActions = array of TWfcMusicCellAction;
  TWfcMusicRhythmFrame = record
    Actions: TWfcMusicCellActions;
  end;
  TWfcMusicRhythmFrames = array of TWfcMusicRhythmFrame;
  TWfcMusicPitchClasses = array of Integer;
  TWfcMusicPitchClassSet = record
    StepsPerOctave: Integer;
    PitchClasses: TWfcMusicPitchClasses;
  end;
  TWfcMusicPitchClassSets = array of TWfcMusicPitchClassSet;

function MakeWfcMusicVoiceCell(const AAction: TWfcMusicCellAction;
  const ATones: TWfcMusicTones): TWfcMusicVoiceCell;
function MakeWfcMusicRestVoiceCell: TWfcMusicVoiceCell;
function MakeWfcMusicEnsembleFrame(const AVoices: TWfcMusicVoiceCells):
  TWfcMusicEnsembleFrame;
function MakeWfcMusicRhythmFrame(const AActions: TWfcMusicCellActions):
  TWfcMusicRhythmFrame;
function MakeWfcMusicPitchClassSet(const AStepsPerOctave: Integer;
  const APitchClasses: TWfcMusicPitchClasses): TWfcMusicPitchClassSet;

function EncodeWfcMusicEnsembleFrame(const AFrame: TWfcMusicEnsembleFrame):
  TWfcModelToken;
function DecodeWfcMusicEnsembleFrame(const AToken: TWfcModelToken):
  TWfcMusicEnsembleFrame;
function EncodeWfcMusicEnsembleFrames(const AFrames: TWfcMusicEnsembleFrames):
  TWfcModelTokens;
function DecodeWfcMusicEnsembleFrames(const ATokens: TWfcModelTokens):
  TWfcMusicEnsembleFrames;
function EncodeWfcMusicRhythmFrame(const AFrame: TWfcMusicRhythmFrame):
  TWfcModelToken;
function DecodeWfcMusicRhythmFrame(const AToken: TWfcModelToken):
  TWfcMusicRhythmFrame;
function EncodeWfcMusicRhythmFrames(const AFrames: TWfcMusicRhythmFrames):
  TWfcModelTokens;
function DecodeWfcMusicRhythmFrames(const ATokens: TWfcModelTokens):
  TWfcMusicRhythmFrames;
function EncodeWfcMusicPitchClassSet(const ASet: TWfcMusicPitchClassSet):
  TWfcModelToken;
function DecodeWfcMusicPitchClassSet(const AToken: TWfcModelToken):
  TWfcMusicPitchClassSet;
function EncodeWfcMusicPitchClassSets(const ASets: TWfcMusicPitchClassSets):
  TWfcModelTokens;
function DecodeWfcMusicPitchClassSets(const ATokens: TWfcModelTokens):
  TWfcMusicPitchClassSets;

{ Bulk codecs check individual frames, not the surrounding timeline. A hold
  is a valid vocabulary token; these functions check its temporal meaning. }
procedure ValidateWfcMusicEnsembleFrames(const AFrames: TWfcMusicEnsembleFrames);
function WfcMusicEnsembleFrameCanStart(const AFrame: TWfcMusicEnsembleFrame):
  Boolean;
function WfcMusicEnsembleFrameCanFollow(
  const APrevious, ACurrent: TWfcMusicEnsembleFrame): Boolean;
function WfcMusicPitchClassSetsEqual(const A, B: TWfcMusicPitchClassSet): Boolean;
function WfcMusicPitchClassSetIsSubset(
  const AActual, AAllowed: TWfcMusicPitchClassSet): Boolean;

function ProjectWfcMusicScoreToEnsembleFrames(const AScore: TWfcMusicScore;
  const AQuantumTicks: Integer): TWfcMusicEnsembleFrames;
function RebuildWfcMusicEnsembleSpans(const AFrames: TWfcMusicEnsembleFrames;
  const AQuantumTicks: Integer): TWfcMusicSpanEvents;
function RebuildWfcMusicEnsembleScore(const AFrames: TWfcMusicEnsembleFrames;
  const AQuantumTicks: Integer; const ATemplate: TWfcMusicScore): TWfcMusicScore;
function ProjectWfcMusicEnsembleFrameToRhythm(
  const AFrame: TWfcMusicEnsembleFrame): TWfcMusicRhythmFrame;
function ProjectWfcMusicEnsembleFramesToRhythm(
  const AFrames: TWfcMusicEnsembleFrames): TWfcMusicRhythmFrames;
function ProjectWfcMusicEnsembleFrameToPitchClassSet(
  const AFrame: TWfcMusicEnsembleFrame; const AStepsPerOctave: Integer):
  TWfcMusicPitchClassSet;
function ProjectWfcMusicEnsembleFramesToPitchClassSets(
  const AFrames: TWfcMusicEnsembleFrames; const AStepsPerOctave: Integer):
  TWfcMusicPitchClassSets;

implementation

uses SysUtils;

procedure EnsembleError(const AMessage: String);
begin
  raise EWfcMusicEnsemble.Create('invalid music ensemble: ' + AMessage);
end;

function CheckedCount(const ALength: SizeInt): Integer;
begin
  if (ALength < 0) or (ALength > High(Integer)) then
    EnsembleError('managed count exceeds the Integer range');
  Result := Integer(ALength);
end;

function CheckedProduct(const A, B: Integer): Integer;
begin
  if (A < 0) or (B < 0) then EnsembleError('negative size');
  if (A <> 0) and (B > High(Integer) div A) then
    EnsembleError('expanded size exceeds the Integer range');
  Result := A * B;
end;

procedure ValidateAction(const AAction: TWfcMusicCellAction);
begin
  case AAction of wmcaRest, wmcaAttack, wmcaHold: Exit; end;
  EnsembleError('unknown voice action');
end;

procedure ValidateVoice(const AVoice: TWfcMusicVoiceCell);
var I: Integer;
begin
  ValidateAction(AVoice.Action);
  CheckedCount(Length(AVoice.Tones));
  if AVoice.Action = wmcaRest then
  begin
    if Length(AVoice.Tones) <> 0 then EnsembleError('rest contains tones');
    Exit;
  end;
  if Length(AVoice.Tones) = 0 then EnsembleError('sound has no tones');
  for I := 0 to High(AVoice.Tones) do
  begin
    if AVoice.Tones[I].Pitch < 0 then EnsembleError('negative pitch');
    if (AVoice.Tones[I].Velocity < 1) or
      (AVoice.Tones[I].Velocity > 127) then EnsembleError('velocity outside 1..127');
    if (I > 0) and (AVoice.Tones[I - 1].Pitch >= AVoice.Tones[I].Pitch) then
      EnsembleError('tone pitches must be strictly increasing');
  end;
end;

procedure ValidateFrame(const AFrame: TWfcMusicEnsembleFrame);
var I: Integer;
begin
  if CheckedCount(Length(AFrame.Voices)) = 0 then
    EnsembleError('frame has no voices');
  for I := 0 to High(AFrame.Voices) do ValidateVoice(AFrame.Voices[I]);
end;

procedure ValidateRhythm(const AFrame: TWfcMusicRhythmFrame);
var I: Integer;
begin
  if CheckedCount(Length(AFrame.Actions)) = 0 then
    EnsembleError('rhythm frame has no voices');
  for I := 0 to High(AFrame.Actions) do ValidateAction(AFrame.Actions[I]);
end;

procedure ValidatePitchClasses(const ASet: TWfcMusicPitchClassSet);
var I: Integer;
begin
  if ASet.StepsPerOctave < 1 then EnsembleError('steps per octave must be positive');
  CheckedCount(Length(ASet.PitchClasses));
  for I := 0 to High(ASet.PitchClasses) do
  begin
    if (ASet.PitchClasses[I] < 0) or
      (ASet.PitchClasses[I] >= ASet.StepsPerOctave) then
      EnsembleError('pitchclass outside the step system');
    if (I > 0) and (ASet.PitchClasses[I - 1] >= ASet.PitchClasses[I]) then
      EnsembleError('pitchclasses must be strictly increasing');
  end;
end;

function CopyTones(const AValues: TWfcMusicTones): TWfcMusicTones;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, CheckedCount(Length(AValues)));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

function MakeWfcMusicVoiceCell(const AAction: TWfcMusicCellAction;
  const ATones: TWfcMusicTones): TWfcMusicVoiceCell;
begin
  Result.Action := AAction;
  Result.Tones := ATones;
  ValidateVoice(Result);
  Result.Tones := CopyTones(ATones);
end;

function MakeWfcMusicRestVoiceCell: TWfcMusicVoiceCell;
begin
  Result := Default(TWfcMusicVoiceCell);
  Result.Action := wmcaRest;
end;

function MakeWfcMusicEnsembleFrame(const AVoices: TWfcMusicVoiceCells):
  TWfcMusicEnsembleFrame;
var I: Integer;
begin
  Result.Voices := AVoices;
  ValidateFrame(Result);
  Result.Voices := nil;
  SetLength(Result.Voices, Length(AVoices));
  for I := 0 to High(AVoices) do
    Result.Voices[I] := MakeWfcMusicVoiceCell(AVoices[I].Action, AVoices[I].Tones);
end;

function MakeWfcMusicRhythmFrame(const AActions: TWfcMusicCellActions):
  TWfcMusicRhythmFrame;
var I: Integer;
begin
  Result.Actions := AActions;
  ValidateRhythm(Result);
  Result.Actions := nil;
  SetLength(Result.Actions, Length(AActions));
  for I := 0 to High(AActions) do Result.Actions[I] := AActions[I];
end;

function MakeWfcMusicPitchClassSet(const AStepsPerOctave: Integer;
  const APitchClasses: TWfcMusicPitchClasses): TWfcMusicPitchClassSet;
var I: Integer;
begin
  Result.StepsPerOctave := AStepsPerOctave;
  Result.PitchClasses := APitchClasses;
  ValidatePitchClasses(Result);
  Result.PitchClasses := nil;
  SetLength(Result.PitchClasses, Length(APitchClasses));
  for I := 0 to High(APitchClasses) do Result.PitchClasses[I] := APitchClasses[I];
end;

function ActionText(const AAction: TWfcMusicCellAction): String;
begin
  ValidateAction(AAction);
  case AAction of
    wmcaRest: Result := 'r';
    wmcaAttack: Result := 'a';
    wmcaHold: Result := 'h';
  end;
end;

procedure AppendField(var AText: String; const AField: String);
begin
  if Length(AText) >= High(Integer) - 1 - Length(AField) then
    EnsembleError('encoded token exceeds the Integer range');
  AText := AText + ':' + AField;
end;

function EncodeWfcMusicEnsembleFrame(const AFrame: TWfcMusicEnsembleFrame):
  TWfcModelToken;
var I, J: Integer; S: String;
begin
  ValidateFrame(AFrame);
  S := 'wme1';
  AppendField(S, IntToStr(Length(AFrame.Voices)));
  for I := 0 to High(AFrame.Voices) do
  begin
    AppendField(S, ActionText(AFrame.Voices[I].Action));
    if AFrame.Voices[I].Action = wmcaRest then Continue;
    AppendField(S, IntToStr(Length(AFrame.Voices[I].Tones)));
    for J := 0 to High(AFrame.Voices[I].Tones) do
    begin
      AppendField(S, IntToStr(AFrame.Voices[I].Tones[J].Pitch));
      AppendField(S, IntToStr(AFrame.Voices[I].Tones[J].Velocity));
    end;
  end;
  Result := TWfcModelToken(S);
end;

function EncodeWfcMusicRhythmFrame(const AFrame: TWfcMusicRhythmFrame):
  TWfcModelToken;
var I: Integer; S: String;
begin
  ValidateRhythm(AFrame);
  S := 'wmer1';
  AppendField(S, IntToStr(Length(AFrame.Actions)));
  for I := 0 to High(AFrame.Actions) do AppendField(S, ActionText(AFrame.Actions[I]));
  Result := TWfcModelToken(S);
end;

function EncodeWfcMusicPitchClassSet(const ASet: TWfcMusicPitchClassSet):
  TWfcModelToken;
var I: Integer; S: String;
begin
  ValidatePitchClasses(ASet);
  S := 'wmhs1';
  AppendField(S, IntToStr(ASet.StepsPerOctave));
  AppendField(S, IntToStr(Length(ASet.PitchClasses)));
  for I := 0 to High(ASet.PitchClasses) do AppendField(S, IntToStr(ASet.PitchClasses[I]));
  Result := TWfcModelToken(S);
end;

function TakeField(const S: String; var P: Integer): String;
var Start: Integer;
begin
  if P > Length(S) then EnsembleError('missing token field');
  Start := P;
  while (P <= Length(S)) and (S[P] <> ':') do Inc(P);
  if P = Start then EnsembleError('empty token field');
  Result := Copy(S, Start, P - Start);
  if P <= Length(S) then Inc(P);
end;

function TakeInteger(const S: String; var P: Integer): Integer;
var F: String; I, Digit: Integer;
begin
  F := TakeField(S, P);
  if (Length(F) > 1) and (F[1] = '0') then EnsembleError('noncanonical leading zero');
  Result := 0;
  for I := 1 to Length(F) do
  begin
    if not (F[I] in ['0'..'9']) then EnsembleError('expected unsigned decimal field');
    Digit := Ord(F[I]) - Ord('0');
    if Result > (High(Integer) - Digit) div 10 then
      EnsembleError('decimal field exceeds the Integer range');
    Result := Result * 10 + Digit;
  end;
end;

function TakeAction(const S: String; var P: Integer): TWfcMusicCellAction;
var F: String;
begin
  F := TakeField(S, P);
  if F = 'r' then Exit(wmcaRest);
  if F = 'a' then Exit(wmcaAttack);
  if F = 'h' then Exit(wmcaHold);
  EnsembleError('unknown encoded action');
  Result := wmcaRest;
end;

procedure BeginToken(const AToken: TWfcModelToken; const APrefix: String;
  out S: String; out P: Integer);
var I: Integer;
begin
  if CheckedCount(Length(AToken)) = High(Integer) then
    EnsembleError('token leaves no representable parser terminator index');
  S := String(AToken);
  for I := 1 to Length(S) do
    if Ord(S[I]) > 127 then EnsembleError('token is not ASCII');
  if (S = '') or (S[Length(S)] = ':') then EnsembleError('empty or incomplete token');
  P := 1;
  if TakeField(S, P) <> APrefix then EnsembleError('unexpected token version or kind');
end;

procedure EndToken(const S: String; const P: Integer);
begin
  if P <= Length(S) then EnsembleError('trailing token fields');
end;

procedure CheckEncodedCount(const S: String; const P, ACount,
  AMinimumPerItem: Integer; const AAllowZero: Boolean);
var Remaining: Integer;
begin
  if (ACount < 0) or ((not AAllowZero) and (ACount = 0)) then
    EnsembleError('invalid encoded item count');
  Remaining := Length(S) - P + 1;
  { One final item need not have a trailing separator. Divide before adding
    the remainder so even the longest representable token cannot overflow. }
  if ACount > Remaining div AMinimumPerItem +
    Ord(Remaining mod AMinimumPerItem = AMinimumPerItem - 1) then
    EnsembleError('encoded item count exceeds available token data');
end;

function DecodeWfcMusicEnsembleFrame(const AToken: TWfcModelToken):
  TWfcMusicEnsembleFrame;
var S: String; P, I, J, Count, ToneCount: Integer;
begin
  Result := Default(TWfcMusicEnsembleFrame);
  BeginToken(AToken, 'wme1', S, P);
  Count := TakeInteger(S, P);
  CheckEncodedCount(S, P, Count, 2, False);
  SetLength(Result.Voices, Count);
  for I := 0 to Count - 1 do
  begin
    Result.Voices[I].Action := TakeAction(S, P);
    if Result.Voices[I].Action = wmcaRest then Continue;
    ToneCount := TakeInteger(S, P);
    CheckEncodedCount(S, P, ToneCount, 4, False);
    SetLength(Result.Voices[I].Tones, ToneCount);
    for J := 0 to ToneCount - 1 do
    begin
      Result.Voices[I].Tones[J].Pitch := TakeInteger(S, P);
      Result.Voices[I].Tones[J].Velocity := TakeInteger(S, P);
    end;
  end;
  EndToken(S, P);
  ValidateFrame(Result);
end;

function DecodeWfcMusicRhythmFrame(const AToken: TWfcModelToken):
  TWfcMusicRhythmFrame;
var S: String; P, I, Count: Integer;
begin
  Result := Default(TWfcMusicRhythmFrame);
  BeginToken(AToken, 'wmer1', S, P);
  Count := TakeInteger(S, P);
  CheckEncodedCount(S, P, Count, 2, False);
  SetLength(Result.Actions, Count);
  for I := 0 to Count - 1 do Result.Actions[I] := TakeAction(S, P);
  EndToken(S, P);
  ValidateRhythm(Result);
end;

function DecodeWfcMusicPitchClassSet(const AToken: TWfcModelToken):
  TWfcMusicPitchClassSet;
var S: String; P, I, Count: Integer;
begin
  Result := Default(TWfcMusicPitchClassSet);
  BeginToken(AToken, 'wmhs1', S, P);
  Result.StepsPerOctave := TakeInteger(S, P);
  Count := TakeInteger(S, P);
  CheckEncodedCount(S, P, Count, 2, True);
  SetLength(Result.PitchClasses, Count);
  for I := 0 to Count - 1 do Result.PitchClasses[I] := TakeInteger(S, P);
  EndToken(S, P);
  ValidatePitchClasses(Result);
end;

function EncodeWfcMusicEnsembleFrames(const AFrames: TWfcMusicEnsembleFrames):
  TWfcModelTokens;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, CheckedCount(Length(AFrames)));
  for I := 0 to High(AFrames) do Result[I] := EncodeWfcMusicEnsembleFrame(AFrames[I]);
end;

function DecodeWfcMusicEnsembleFrames(const ATokens: TWfcModelTokens):
  TWfcMusicEnsembleFrames;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, CheckedCount(Length(ATokens)));
  for I := 0 to High(ATokens) do Result[I] := DecodeWfcMusicEnsembleFrame(ATokens[I]);
end;

function EncodeWfcMusicRhythmFrames(const AFrames: TWfcMusicRhythmFrames):
  TWfcModelTokens;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, CheckedCount(Length(AFrames)));
  for I := 0 to High(AFrames) do Result[I] := EncodeWfcMusicRhythmFrame(AFrames[I]);
end;

function DecodeWfcMusicRhythmFrames(const ATokens: TWfcModelTokens):
  TWfcMusicRhythmFrames;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, CheckedCount(Length(ATokens)));
  for I := 0 to High(ATokens) do Result[I] := DecodeWfcMusicRhythmFrame(ATokens[I]);
end;

function EncodeWfcMusicPitchClassSets(const ASets: TWfcMusicPitchClassSets):
  TWfcModelTokens;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, CheckedCount(Length(ASets)));
  for I := 0 to High(ASets) do Result[I] := EncodeWfcMusicPitchClassSet(ASets[I]);
end;

function DecodeWfcMusicPitchClassSets(const ATokens: TWfcModelTokens):
  TWfcMusicPitchClassSets;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, CheckedCount(Length(ATokens)));
  for I := 0 to High(ATokens) do Result[I] := DecodeWfcMusicPitchClassSet(ATokens[I]);
end;

function TonesEqual(const A, B: TWfcMusicTones): Boolean;
var I: Integer;
begin
  Result := False;
  if Length(A) <> Length(B) then Exit;
  for I := 0 to High(A) do
    if (A[I].Pitch <> B[I].Pitch) or (A[I].Velocity <> B[I].Velocity) then Exit;
  Result := True;
end;

function WfcMusicEnsembleFrameCanStart(const AFrame: TWfcMusicEnsembleFrame):
  Boolean;
var I: Integer;
begin
  ValidateFrame(AFrame);
  for I := 0 to High(AFrame.Voices) do
    if AFrame.Voices[I].Action = wmcaHold then Exit(False);
  Result := True;
end;

function WfcMusicEnsembleFrameCanFollow(
  const APrevious, ACurrent: TWfcMusicEnsembleFrame): Boolean;
var I: Integer;
begin
  ValidateFrame(APrevious);
  ValidateFrame(ACurrent);
  if Length(APrevious.Voices) <> Length(ACurrent.Voices) then Exit(False);
  for I := 0 to High(ACurrent.Voices) do
    if ACurrent.Voices[I].Action = wmcaHold then
      if (APrevious.Voices[I].Action = wmcaRest) or
        not TonesEqual(APrevious.Voices[I].Tones, ACurrent.Voices[I].Tones) then
        Exit(False);
  Result := True;
end;

procedure ValidateWfcMusicEnsembleFrames(const AFrames: TWfcMusicEnsembleFrames);
var I: Integer;
begin
  if CheckedCount(Length(AFrames)) = 0 then EnsembleError('timeline is empty');
  if not WfcMusicEnsembleFrameCanStart(AFrames[0]) then
    EnsembleError('initial frame contains an orphan hold');
  for I := 1 to High(AFrames) do
    if not WfcMusicEnsembleFrameCanFollow(AFrames[I - 1], AFrames[I]) then
      EnsembleError(Format('voice arity or continuation mismatch at frame %d', [I]));
end;

function WfcMusicPitchClassSetsEqual(const A, B: TWfcMusicPitchClassSet): Boolean;
var I: Integer;
begin
  ValidatePitchClasses(A);
  ValidatePitchClasses(B);
  if (A.StepsPerOctave <> B.StepsPerOctave) or
    (Length(A.PitchClasses) <> Length(B.PitchClasses)) then Exit(False);
  for I := 0 to High(A.PitchClasses) do
    if A.PitchClasses[I] <> B.PitchClasses[I] then Exit(False);
  Result := True;
end;

function WfcMusicPitchClassSetIsSubset(
  const AActual, AAllowed: TWfcMusicPitchClassSet): Boolean;
var I, J: Integer;
begin
  ValidatePitchClasses(AActual);
  ValidatePitchClasses(AAllowed);
  if AActual.StepsPerOctave <> AAllowed.StepsPerOctave then Exit(False);
  J := 0;
  for I := 0 to High(AActual.PitchClasses) do
  begin
    while (J < Length(AAllowed.PitchClasses)) and
      (AAllowed.PitchClasses[J] < AActual.PitchClasses[I]) do Inc(J);
    if (J = Length(AAllowed.PitchClasses)) or
      (AAllowed.PitchClasses[J] <> AActual.PitchClasses[I]) then Exit(False);
    Inc(J);
  end;
  Result := True;
end;

function ProjectWfcMusicScoreToEnsembleFrames(const AScore: TWfcMusicScore;
  const AQuantumTicks: Integer): TWfcMusicEnsembleFrames;
var I, J, Count, Start, Duration, ExpandedTones, Added: Integer;
  Span: TWfcMusicSpanEvent;
  Action: TWfcMusicCellAction;
begin
  Result := nil;
  if AScore = nil then EnsembleError('score is not assigned');
  if AQuantumTicks < 1 then EnsembleError('quantum must be positive');
  if (AScore.LengthTicks mod AQuantumTicks) <> 0 then
    EnsembleError('score length is not aligned to the quantum');
  Count := AScore.LengthTicks div AQuantumTicks;
  CheckedProduct(Count, AScore.VoiceCount);
  ExpandedTones := 0;
  { The score is an already validated immutable object. SpanAt makes a
    temporary detached tone copy; expansion below is preflighted separately. }
  for I := 0 to AScore.SpanCount - 1 do
  begin
    Span := AScore.SpanAt(I);
    if ((Span.StartTick mod AQuantumTicks) <> 0) or
      ((Span.DurationTicks mod AQuantumTicks) <> 0) then
      EnsembleError(Format('score span %d is not aligned to the quantum', [I]));
    Added := CheckedProduct(CheckedCount(Length(Span.Tones)),
      Span.DurationTicks div AQuantumTicks);
    if ExpandedTones > High(Integer) - Added then
      EnsembleError('expanded tone storage exceeds the Integer range');
    Inc(ExpandedTones, Added);
  end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do SetLength(Result[I].Voices, AScore.VoiceCount);
  for I := 0 to AScore.SpanCount - 1 do
  begin
    Span := AScore.SpanAt(I);
    Start := Span.StartTick div AQuantumTicks;
    Duration := Span.DurationTicks div AQuantumTicks;
    for J := 0 to Duration - 1 do
    begin
      if Span.Kind = wmskRest then Action := wmcaRest
      else if J = 0 then Action := wmcaAttack
      else Action := wmcaHold;
      Result[Start + J].Voices[Span.VoiceIndex] :=
        MakeWfcMusicVoiceCell(Action, Span.Tones);
    end;
  end;
end;

function RebuildWfcMusicEnsembleSpans(const AFrames: TWfcMusicEnsembleFrames;
  const AQuantumTicks: Integer): TWfcMusicSpanEvents;
var V, I, J, Count, Next: Integer;
begin
  Result := nil;
  if AQuantumTicks < 1 then EnsembleError('quantum must be positive');
  CheckedProduct(CheckedCount(Length(AFrames)), AQuantumTicks);
  ValidateWfcMusicEnsembleFrames(AFrames);
  Count := 0;
  for V := 0 to High(AFrames[0].Voices) do
    for I := 0 to High(AFrames) do
      if (AFrames[I].Voices[V].Action = wmcaAttack) or
        ((AFrames[I].Voices[V].Action = wmcaRest) and
          ((I = 0) or (AFrames[I - 1].Voices[V].Action <> wmcaRest))) then
      begin
        if Count = High(Integer) then EnsembleError('span count exceeds Integer');
        Inc(Count);
      end;
  SetLength(Result, Count);
  Next := 0;
  for V := 0 to High(AFrames[0].Voices) do
  begin
    I := 0;
    while I < Length(AFrames) do
    begin
      J := I + 1;
      if AFrames[I].Voices[V].Action = wmcaRest then
      begin
        while (J < Length(AFrames)) and
          (AFrames[J].Voices[V].Action = wmcaRest) do Inc(J);
        Result[Next] := MakeWfcMusicRest(V, CheckedProduct(I, AQuantumTicks),
          CheckedProduct(J - I, AQuantumTicks));
      end
      else
      begin
        while (J < Length(AFrames)) and
          (AFrames[J].Voices[V].Action = wmcaHold) do Inc(J);
        Result[Next] := MakeWfcMusicSound(V, CheckedProduct(I, AQuantumTicks),
          CheckedProduct(J - I, AQuantumTicks), AFrames[I].Voices[V].Tones);
      end;
      Inc(Next);
      I := J;
    end;
  end;
end;

function RebuildWfcMusicEnsembleScore(const AFrames: TWfcMusicEnsembleFrames;
  const AQuantumTicks: Integer; const ATemplate: TWfcMusicScore): TWfcMusicScore;
var Spans: TWfcMusicSpanEvents;
begin
  Result := nil;
  if ATemplate = nil then EnsembleError('score template is not assigned');
  if AQuantumTicks < 1 then EnsembleError('quantum must be positive');
  if CheckedProduct(CheckedCount(Length(AFrames)), AQuantumTicks) <>
    ATemplate.LengthTicks then EnsembleError('template and frame lengths differ');
  if Length(AFrames) = 0 then EnsembleError('timeline is empty');
  if Length(AFrames[0].Voices) <> ATemplate.VoiceCount then
    EnsembleError('template and frame voice counts differ');
  Spans := RebuildWfcMusicEnsembleSpans(AFrames, AQuantumTicks);
  Result := TWfcMusicScore.Create(ATemplate.TicksPerQuarter,
    ATemplate.StepsPerOctave, ATemplate.LengthTicks, ATemplate.CopyTracks,
    ATemplate.CopyVoices, ATemplate.CopyMeters, ATemplate.CopyTempos, Spans);
end;

function ProjectWfcMusicEnsembleFrameToRhythm(
  const AFrame: TWfcMusicEnsembleFrame): TWfcMusicRhythmFrame;
var I: Integer;
begin
  ValidateFrame(AFrame);
  Result.Actions := nil;
  SetLength(Result.Actions, Length(AFrame.Voices));
  for I := 0 to High(AFrame.Voices) do Result.Actions[I] := AFrame.Voices[I].Action;
end;

function ProjectWfcMusicEnsembleFramesToRhythm(
  const AFrames: TWfcMusicEnsembleFrames): TWfcMusicRhythmFrames;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, CheckedCount(Length(AFrames)));
  for I := 0 to High(AFrames) do Result[I] := ProjectWfcMusicEnsembleFrameToRhythm(AFrames[I]);
end;

procedure SortPitchClasses(var AClasses: TWfcMusicPitchClasses);
var I, Temp: Integer;

  procedure SiftDown(const ARoot, ALast: Integer);
  var Root, Child, Value: Integer;
  begin
    if ALast < 1 then Exit;
    Root := ARoot;
    Value := AClasses[Root];
    while Root <= (ALast - 1) div 2 do
    begin
      Child := Root * 2 + 1;
      if (Child < ALast) and (AClasses[Child] < AClasses[Child + 1]) then Inc(Child);
      if Value >= AClasses[Child] then Break;
      AClasses[Root] := AClasses[Child];
      Root := Child;
    end;
    AClasses[Root] := Value;
  end;

begin
  if Length(AClasses) < 2 then Exit;
  { Own in-place heap sort: no tuning-sized mask or quadratic insertion.
    Equal integer classes have no identity requiring a stable sort. }
  for I := Length(AClasses) div 2 - 1 downto 0 do SiftDown(I, High(AClasses));
  for I := High(AClasses) downto 1 do
  begin
    Temp := AClasses[0]; AClasses[0] := AClasses[I]; AClasses[I] := Temp;
    SiftDown(0, I - 1);
  end;
end;

function ProjectWfcMusicEnsembleFrameToPitchClassSet(
  const AFrame: TWfcMusicEnsembleFrame; const AStepsPerOctave: Integer):
  TWfcMusicPitchClassSet;
var I, J, Count, Next: Integer;
begin
  if AStepsPerOctave < 1 then EnsembleError('steps per octave must be positive');
  ValidateFrame(AFrame);
  Result.StepsPerOctave := AStepsPerOctave;
  Result.PitchClasses := nil;
  Count := 0;
  for I := 0 to High(AFrame.Voices) do
  begin
    if Length(AFrame.Voices[I].Tones) > High(Integer) - Count then
      EnsembleError('pitchclass collection exceeds the Integer range');
    Inc(Count, Length(AFrame.Voices[I].Tones));
  end;
  SetLength(Result.PitchClasses, Count);
  Next := 0;
  for I := 0 to High(AFrame.Voices) do
    for J := 0 to High(AFrame.Voices[I].Tones) do
    begin
      Result.PitchClasses[Next] := AFrame.Voices[I].Tones[J].Pitch mod AStepsPerOctave;
      Inc(Next);
    end;
  SortPitchClasses(Result.PitchClasses);
  Next := 0;
  for I := 0 to Count - 1 do
    if (Next = 0) or (Result.PitchClasses[I] <> Result.PitchClasses[Next - 1]) then
    begin
      Result.PitchClasses[Next] := Result.PitchClasses[I];
      Inc(Next);
    end;
  SetLength(Result.PitchClasses, Next);
end;

function ProjectWfcMusicEnsembleFramesToPitchClassSets(
  const AFrames: TWfcMusicEnsembleFrames; const AStepsPerOctave: Integer):
  TWfcMusicPitchClassSets;
var I: Integer;
begin
  if AStepsPerOctave < 1 then EnsembleError('steps per octave must be positive');
  Result := nil;
  SetLength(Result, CheckedCount(Length(AFrames)));
  for I := 0 to High(AFrames) do
    Result[I] := ProjectWfcMusicEnsembleFrameToPitchClassSet(AFrames[I], AStepsPerOctave);
end;

end.
