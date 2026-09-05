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
unit wfc_music_voices_graph;

{$mode delphi}{$H+}

interface

uses SysUtils, wfc, wfc_model, wfc_sequence, wfc_sequence_graph,
  wfc_music_ensemble, wfc_music_ensemble_graph;

const WFC_MUSIC_VOICES_GRAPH_VERSION = 1;

type
  EWfcMusicVoicesGraph = class(EWfcMusicEnsembleGraph);
  TWfcMusicVoiceModel = record
    Model: TWfcSequenceModel;
    MinPitch, MaxPitch: Integer;
  end;
  TWfcMusicVoiceModels = array of TWfcMusicVoiceModel;
  TWfcMusicVoicePairRestPolicy = (wmvprSuspend, wmvprReject);
  { LowerVoice < UpperVoice is the dependency order, not an implicit musical
    restriction. Gap = lowest upper pitch - highest lower pitch. A nonnegative
    minimum prevents crossing; zero permits unisons. Negative gaps are explicit.
    Suspend ignores the pair when either voice rests; Reject forbids that pair. }
  TWfcMusicVoicePairConstraint = record
    LowerVoice, UpperVoice, MinGap, MaxGap: Integer;
    RestPolicy: TWfcMusicVoicePairRestPolicy;
  end;
  TWfcMusicVoicePairConstraints = array of TWfcMusicVoicePairConstraint;
  { Models are borrowed immutable objects. Copy detaches the role/pair vectors.
    Rhythm is a shared ordered action vector. Each voice model emits existing
    one-voice ensemble frame tokens, including complete chords and velocities. }
  TWfcMusicVoicesGraphConfig = record
    HarmonyModel, RhythmModel: TWfcSequenceModel;
    Voices: TWfcMusicVoiceModels;
    StepsPerOctave: Integer;
    HarmonyMode: TWfcMusicEnsembleHarmonyMode;
    PairConstraints: TWfcMusicVoicePairConstraints;
  end;
  TWfcMusicVoicesBoundaries = array of TWfcSequenceSegmentBoundary;
  TWfcMusicVoicesGeneratedLayers = array of TWfcGeneratedSequenceSegment;
  TWfcMusicVoicesSupplierIndices = array of Integer;
  TWfcMusicVoicesCoverageWitness = record
    PitchClass: Integer;
    Suppliers: TWfcMusicVoicesSupplierIndices;
  end;
  TWfcMusicVoicesCoverageWitnesses = array of TWfcMusicVoicesCoverageWitness;
  { Detached result vectors. Model order is H=0, R=1, voices=2+i. Coverage is
    sorted by pitch class; supplier -1 means absent, otherwise a matching voice.
    The witness is the lowest-index supplying voice, not a claim that only one
    voice sounds the class. This canonical choice avoids searching equivalent
    proof assignments. Allowed-palette mode has no coverage passes or records. }
  TWfcMusicVoicesGenerated = record
    Layers: TWfcMusicVoicesGeneratedLayers;
    Frames: TWfcMusicEnsembleFrames;
    Coverage: TWfcMusicVoicesCoverageWitnesses;
  end;
  TWfcMusicVoicesIssueKind = (wmvikNone, wmvikConfiguration, wmvikGraphShape,
    wmvikSequence, wmvikBoundary, wmvikToken, wmvikEntryConstraint,
    wmvikFrame, wmvikRhythm, wmvikHarmony, wmvikRange, wmvikPair,
    wmvikWitness, wmvikInternal);
  TWfcMusicVoicesValidationIssue = record
    Kind: TWfcMusicVoicesIssueKind;
    ModelIndex, VoiceIndex, RelatedVoiceIndex, Position, PitchClass: Integer;
    SequenceIssue: TWfcSequenceGraphIssue;
    Detail: String;
  end;
  TWfcMusicVoicesValidationReport = record
    Valid: Boolean;
    CheckedModels, CheckedCells: Integer;
    Issue: TWfcMusicVoicesValidationIssue;
  end;

function CopyWfcMusicVoicesGraphConfig(const AConfig: TWfcMusicVoicesGraphConfig):
  TWfcMusicVoicesGraphConfig;
procedure ValidateWfcMusicVoicesGraphConfig(const AConfig: TWfcMusicVoicesGraphConfig);
function WfcMusicVoicesModelCount(const AConfig: TWfcMusicVoicesGraphConfig): Integer;
function WfcMusicVoicesModelAt(const AConfig: TWfcMusicVoicesGraphConfig;
  const AModelIndex: Integer): TWfcSequenceModel;
function WfcMusicVoicesPassLabel(const AModelIndex: Integer): String;
function WfcMusicVoicesCoveragePassLabel(const APitchClass: Integer): String;
function WfcMusicVoicesCoverageClasses(const AConfig: TWfcMusicVoicesGraphConfig):
  TWfcMusicPitchClasses;

{ Fresh caller-owned bounded graph. Every pass is CellCount x 1 x 1, open.
  All semantic construction failures dispose the complete private graph.
  No Cartesian product of voice vocabularies is created. Exact coverage uses
  one small witness pass per class occurring in the harmony vocabulary, never
  an allocation indexed by every possible tuning step. }
function BuildWfcMusicVoicesSegmentGraph(const AConfig: TWfcMusicVoicesGraphConfig;
  const ACellCount: Integer; const ASeed: TGraphSeed;
  const ABoundaries: TWfcMusicVoicesBoundaries): TGraph;

{ Independent proof: expected layout, model identities, exact original segment
  boundaries, latent paths/emissions, caller domains/locks, temporal holds,
  range/pairs, rhythm, harmonic union and graph/record witnesses are checked.
  This does not trust the graph's editable cross-pass requirement registry.
  Capture returns an entirely empty generated record on failure. }
function CaptureSolvedWfcMusicVoices(const AConfig: TWfcMusicVoicesGraphConfig;
  const AGraph: TGraph; const ABoundaries: TWfcMusicVoicesBoundaries;
  out AGenerated: TWfcMusicVoicesGenerated;
  out AReport: TWfcMusicVoicesValidationReport): Boolean;
function ValidateWfcMusicVoicesGenerated(const AConfig: TWfcMusicVoicesGraphConfig;
  const AGraph: TGraph; const ABoundaries: TWfcMusicVoicesBoundaries;
  const AGenerated: TWfcMusicVoicesGenerated;
  out AReport: TWfcMusicVoicesValidationReport): Boolean;

implementation

uses wfc_music_sequence;

type
  TVoiceFrameVocabularies = array of TWfcMusicEnsembleFrames;
  TPreparedMusic = record
    Harmony: TWfcMusicPitchClassSets;
    Rhythm: TWfcMusicRhythmFrames;
    Voices: TVoiceFrameVocabularies;
    Classes: TWfcMusicPitchClasses;
  end;

procedure VoiceError(const AText: String);
begin raise EWfcMusicVoicesGraph.Create('independent voices: ' + AText); end;

procedure CheckInteger(const AValue, AMin, AMax: NativeInt; const AName: String);
begin
  if (AValue < AMin) or (AValue > AMax) then VoiceError(AName + ' is out of range');
  {$IFDEF PAS2JS}
  if AValue <> Trunc(AValue) then VoiceError(AName + ' must be an exact integer');
  {$ENDIF}
end;

function WfcMusicVoicesModelCount(const AConfig: TWfcMusicVoicesGraphConfig): Integer;
begin
  CheckInteger(Length(AConfig.Voices), 1, High(Integer) - 2, 'voice count');
  Result := Length(AConfig.Voices) + 2;
end;

function WfcMusicVoicesModelAt(const AConfig: TWfcMusicVoicesGraphConfig;
  const AModelIndex: Integer): TWfcSequenceModel;
begin
  CheckInteger(AModelIndex, 0, WfcMusicVoicesModelCount(AConfig) - 1, 'model index');
  case AModelIndex of
    0: Result := AConfig.HarmonyModel;
    1: Result := AConfig.RhythmModel;
  else Result := AConfig.Voices[AModelIndex - 2].Model;
  end;
end;

function WfcMusicVoicesPassLabel(const AModelIndex: Integer): String;
begin
  CheckInteger(AModelIndex, 0, High(Integer), 'model index');
  case AModelIndex of
    0: Result := 'harmony';
    1: Result := 'rhythm';
  else Result := 'voice.' + IntToStr(AModelIndex - 2);
  end;
end;

function WfcMusicVoicesCoveragePassLabel(const APitchClass: Integer): String;
begin
  CheckInteger(APitchClass, 0, High(Integer), 'pitch class');
  Result := 'coverage.' + IntToStr(APitchClass);
end;

function CopyWfcMusicVoicesGraphConfig(const AConfig: TWfcMusicVoicesGraphConfig):
  TWfcMusicVoicesGraphConfig;
var I: Integer;
begin
  CheckInteger(Length(AConfig.Voices), 0, High(Integer), 'voice count');
  CheckInteger(Length(AConfig.PairConstraints), 0, High(Integer), 'pair count');
  Result := AConfig;
  Result.Voices := nil; Result.PairConstraints := nil;
  SetLength(Result.Voices, Length(AConfig.Voices));
  for I := 0 to High(Result.Voices) do Result.Voices[I] := AConfig.Voices[I];
  SetLength(Result.PairConstraints, Length(AConfig.PairConstraints));
  for I := 0 to High(Result.PairConstraints) do
    Result.PairConstraints[I] := AConfig.PairConstraints[I];
end;

procedure ValidateWfcMusicVoicesGraphConfig(const AConfig: TWfcMusicVoicesGraphConfig);
var I, J, N: Integer; P: TWfcMusicVoicePairConstraint;
begin
  N := WfcMusicVoicesModelCount(AConfig) - 2;
  CheckInteger(AConfig.StepsPerOctave, 1, High(Integer), 'steps per octave');
  CheckInteger(Ord(AConfig.HarmonyMode), Ord(Low(TWfcMusicEnsembleHarmonyMode)),
    Ord(High(TWfcMusicEnsembleHarmonyMode)), 'harmony mode');
  ValidateWfcMusicEnsembleHarmonyModel(AConfig.HarmonyModel, AConfig.StepsPerOctave);
  ValidateWfcMusicEnsembleRhythmModel(AConfig.RhythmModel, N);
  for I := 0 to N - 1 do
  begin
    CheckInteger(AConfig.Voices[I].MinPitch, 0, High(Integer), 'minimum pitch');
    CheckInteger(AConfig.Voices[I].MaxPitch, AConfig.Voices[I].MinPitch,
      High(Integer), 'maximum pitch');
    ValidateWfcMusicEnsembleModel(AConfig.Voices[I].Model, 1);
  end;
  CheckInteger(Length(AConfig.PairConstraints), 0, High(Integer), 'pair count');
  for I := 0 to High(AConfig.PairConstraints) do
  begin
    P := AConfig.PairConstraints[I];
    CheckInteger(P.LowerVoice, 0, N - 1, 'lower voice');
    CheckInteger(P.UpperVoice, P.LowerVoice + 1, N - 1, 'upper voice');
    CheckInteger(P.MinGap, Low(Integer), High(Integer), 'minimum gap');
    CheckInteger(P.MaxGap, P.MinGap, High(Integer), 'maximum gap');
    CheckInteger(Ord(P.RestPolicy), Ord(Low(TWfcMusicVoicePairRestPolicy)),
      Ord(High(TWfcMusicVoicePairRestPolicy)), 'pair rest policy');
    for J := 0 to I - 1 do
      if (P.LowerVoice = AConfig.PairConstraints[J].LowerVoice) and
        (P.UpperVoice = AConfig.PairConstraints[J].UpperVoice) then
        VoiceError('duplicate voice pair; intersect its gap bounds explicitly');
  end;
end;

procedure SortClasses(var A: TWfcMusicPitchClasses);
var Start, Finish, Swap: Integer;
  procedure Sift(Root, Last: Integer);
  var Child, Temp: Integer;
  begin
    while Root <= (Last - 1) div 2 do
    begin
      Child := Root * 2 + 1;
      if Child > Last then Exit;
      if (Child < Last) and (A[Child] < A[Child + 1]) then Inc(Child);
      if A[Root] >= A[Child] then Exit;
      Temp := A[Root]; A[Root] := A[Child]; A[Child] := Temp;
      Root := Child;
    end;
  end;
begin
  if Length(A) < 2 then Exit;
  for Start := (Length(A) div 2) - 1 downto 0 do Sift(Start, High(A));
  for Finish := High(A) downto 1 do
  begin
    Swap := A[0]; A[0] := A[Finish]; A[Finish] := Swap;
    if Finish > 1 then Sift(0, Finish - 1);
  end;
end;

function WfcMusicVoicesCoverageClasses(const AConfig: TWfcMusicVoicesGraphConfig):
  TWfcMusicPitchClasses;
var I, J, Count, Used: Integer; Sets: TWfcMusicPitchClassSets;
begin
  Result := nil;
  CheckInteger(Ord(AConfig.HarmonyMode), Ord(Low(TWfcMusicEnsembleHarmonyMode)),
    Ord(High(TWfcMusicEnsembleHarmonyMode)), 'harmony mode');
  if AConfig.HarmonyMode = wmehmAllowed then Exit;
  CheckInteger(AConfig.StepsPerOctave, 1, High(Integer), 'steps per octave');
  ValidateWfcMusicEnsembleHarmonyModel(AConfig.HarmonyModel, AConfig.StepsPerOctave);
  SetLength(Sets, AConfig.HarmonyModel.PublicTokenCount);
  Count := 0;
  for I := 0 to High(Sets) do
  begin
    Sets[I] := DecodeWfcMusicPitchClassSet(AConfig.HarmonyModel.PublicTokenAt(I));
    if Length(Sets[I].PitchClasses) > High(Integer) - Count then
      VoiceError('coverage vocabulary exceeds Integer indexing');
    Inc(Count, Length(Sets[I].PitchClasses));
  end;
  SetLength(Result, Count); Count := 0;
  for I := 0 to High(Sets) do
    for J := 0 to High(Sets[I].PitchClasses) do
    begin Result[Count] := Sets[I].PitchClasses[J]; Inc(Count); end;
  SortClasses(Result);
  Used := 0;
  for I := 0 to High(Result) do
    if (Used = 0) or (Result[I] <> Result[Used - 1]) then
    begin Result[Used] := Result[I]; Inc(Used); end;
  SetLength(Result, Used);
end;

function PrepareMusic(const C: TWfcMusicVoicesGraphConfig): TPreparedMusic;
var I, J: Integer;
begin
  ValidateWfcMusicVoicesGraphConfig(C);
  Result := Default(TPreparedMusic);
  SetLength(Result.Harmony, C.HarmonyModel.PublicTokenCount);
  for I := 0 to High(Result.Harmony) do
    Result.Harmony[I] := DecodeWfcMusicPitchClassSet(C.HarmonyModel.PublicTokenAt(I));
  SetLength(Result.Rhythm, C.RhythmModel.PublicTokenCount);
  for I := 0 to High(Result.Rhythm) do
    Result.Rhythm[I] := DecodeWfcMusicRhythmFrame(C.RhythmModel.PublicTokenAt(I));
  SetLength(Result.Voices, Length(C.Voices));
  for I := 0 to High(Result.Voices) do
  begin
    SetLength(Result.Voices[I], C.Voices[I].Model.PublicTokenCount);
    for J := 0 to High(Result.Voices[I]) do
      Result.Voices[I][J] := DecodeWfcMusicEnsembleFrame(C.Voices[I].Model.PublicTokenAt(J));
  end;
  Result.Classes := WfcMusicVoicesCoverageClasses(C);
end;

function ContainsClass(const S: TWfcMusicPitchClassSet; const C: Integer): Boolean;
var L, H, M: Integer;
begin
  L := 0; H := High(S.PitchClasses);
  while L <= H do
  begin
    M := L + (H - L) div 2;
    if S.PitchClasses[M] = C then Exit(True);
    if S.PitchClasses[M] < C then L := M + 1 else H := M - 1;
  end;
  Result := False;
end;

function VoiceContains(const V: TWfcMusicVoiceCell; const C, Steps: Integer): Boolean;
var I: Integer;
begin
  for I := 0 to High(V.Tones) do
    if V.Tones[I].Pitch mod Steps = C then Exit(True);
  Result := False;
end;

function InRange(const V: TWfcMusicVoiceCell; const Spec: TWfcMusicVoiceModel): Boolean;
begin
  Result := (Length(V.Tones) = 0) or
    ((V.Tones[0].Pitch >= Spec.MinPitch) and
     (V.Tones[High(V.Tones)].Pitch <= Spec.MaxPitch));
end;

function PairAllows(const L, U: TWfcMusicVoiceCell;
  const P: TWfcMusicVoicePairConstraint): Boolean;
var Gap: Integer;
begin
  if (Length(L.Tones) = 0) or (Length(U.Tones) = 0) then
    Exit(P.RestPolicy = wmvprSuspend);
  Gap := U.Tones[0].Pitch - L.Tones[High(L.Tones)].Pitch;
  Result := (Gap >= P.MinGap) and (Gap <= P.MaxGap);
end;

procedure AppendToken(var A: TWfcModelTokens; const T: TWfcModelToken);
var N: Integer;
begin
  N := Length(A); SetLength(A, N + 1); A[N] := T;
end;

function VoiceBindings(const C: TWfcMusicVoicesGraphConfig;
  const P: TPreparedMusic; const VoiceIndex: Integer): TWfcSequenceProjectionBindings;
var I, J, K, B: Integer; Rules: TWfcSequenceProjectionRules;
  Allowed: TWfcModelTokens; Actual: TWfcMusicPitchClassSet;
begin
  Result := nil; B := 2;
  for I := 0 to High(C.PairConstraints) do
    if C.PairConstraints[I].UpperVoice = VoiceIndex then Inc(B);
  SetLength(Result, B);
  SetLength(Rules, C.Voices[VoiceIndex].Model.PublicTokenCount);
  for I := 0 to High(Rules) do
  begin
    Allowed := nil;
    Actual := ProjectWfcMusicEnsembleFrameToPitchClassSet(P.Voices[VoiceIndex][I], C.StepsPerOctave);
    if InRange(P.Voices[VoiceIndex][I].Voices[0], C.Voices[VoiceIndex]) then
      for J := 0 to High(P.Harmony) do
        if WfcMusicPitchClassSetIsSubset(Actual, P.Harmony[J]) then
          AppendToken(Allowed, C.HarmonyModel.PublicTokenAt(J));
    Rules[I] := MakeWfcSequenceProjectionRule(C.Voices[VoiceIndex].Model.PublicTokenAt(I), Allowed);
  end;
  Result[0] := MakeWfcSequenceProjectionBinding(C.HarmonyModel, WfcMusicVoicesPassLabel(0), Rules);
  for I := 0 to High(Rules) do
  begin
    Allowed := nil;
    for J := 0 to High(P.Rhythm) do
      if P.Rhythm[J].Actions[VoiceIndex] = P.Voices[VoiceIndex][I].Voices[0].Action then
        AppendToken(Allowed, C.RhythmModel.PublicTokenAt(J));
    Rules[I] := MakeWfcSequenceProjectionRule(C.Voices[VoiceIndex].Model.PublicTokenAt(I), Allowed);
  end;
  Result[1] := MakeWfcSequenceProjectionBinding(C.RhythmModel, WfcMusicVoicesPassLabel(1), Rules);
  B := 2;
  for K := 0 to High(C.PairConstraints) do
    if C.PairConstraints[K].UpperVoice = VoiceIndex then
    begin
      J := C.PairConstraints[K].LowerVoice;
      for I := 0 to High(Rules) do
      begin
        Allowed := nil;
        for J := 0 to High(P.Voices[C.PairConstraints[K].LowerVoice]) do
          if PairAllows(P.Voices[C.PairConstraints[K].LowerVoice][J].Voices[0],
            P.Voices[VoiceIndex][I].Voices[0], C.PairConstraints[K]) then
            AppendToken(Allowed, C.Voices[C.PairConstraints[K].LowerVoice].Model.PublicTokenAt(J));
        Rules[I] := MakeWfcSequenceProjectionRule(C.Voices[VoiceIndex].Model.PublicTokenAt(I), Allowed);
      end;
      J := C.PairConstraints[K].LowerVoice;
      Result[B] := MakeWfcSequenceProjectionBinding(C.Voices[J].Model,
        WfcMusicVoicesPassLabel(J + 2), Rules);
      Inc(B);
    end;
end;

procedure ValidateBoundaries(const C: TWfcMusicVoicesGraphConfig;
  const B: TWfcMusicVoicesBoundaries);
var I: Integer; M: TWfcSequenceModel;
begin
  if Length(B) <> WfcMusicVoicesModelCount(C) then VoiceError('boundary vector differs from models');
  for I := 0 to High(B) do
  begin
    M := WfcMusicVoicesModelAt(C, I);
    {$IFDEF PAS2JS}
    if ((B[I].HasPrevious <> True) and (B[I].HasPrevious <> False)) or
      ((B[I].RequireObservedEnd <> True) and (B[I].RequireObservedEnd <> False)) then
      VoiceError('boundary flags must be Boolean');
    {$ENDIF}
    if B[I].HasPrevious then
      CheckInteger(B[I].PreviousState, 0, M.StateCount - 1, 'previous state')
    else CheckInteger(B[I].PreviousState, -1, -1, 'initial previous state');
  end;
end;

function SupplierValue(const I: Integer): TGraphValue;
begin
  if I = -1 then Result := 'absent' else Result := 'voice.' + IntToStr(I);
end;

function BuildWfcMusicVoicesSegmentGraph(const AConfig: TWfcMusicVoicesGraphConfig;
  const ACellCount: Integer; const ASeed: TGraphSeed;
  const ABoundaries: TWfcMusicVoicesBoundaries): TGraph;
var P: TPreparedMusic; I, J, K, N, C, Earlier, TokenIndex: Integer;
  Allowed, Present, Absent: TWfcModelTokens;
  Bindings: TWfcSequenceProjectionBindings; Value: TGraphValue;
begin
  Result := nil;
  CheckInteger(ACellCount, 1, High(Integer), 'cell count');
  {$IFDEF PAS2JS}
  if (ASeed < 0) or (ASeed > Cardinal($FFFFFFFF)) or (ASeed <> Trunc(ASeed)) then
    VoiceError('seed must be an exact unsigned 32-bit integer');
  {$ENDIF}
  P := PrepareMusic(AConfig);
  ValidateBoundaries(AConfig, ABoundaries);
  N := WfcMusicVoicesModelCount(AConfig);
  if Length(P.Classes) > High(Integer) - N then VoiceError('pass count exceeds Integer indexing');
  if N + Length(P.Classes) > High(Integer) div ACellCount then
    VoiceError('total graph entries exceed Integer indexing');
  Result := TGraph.Create;
  try
    Result.Seed := ASeed; Result.Reshape(ACellCount, 1, 1); Result.WrapNeighbors := False;
    for I := 0 to N - 1 do
    begin
      if I = 0 then Result.CurrentPass := WfcMusicVoicesPassLabel(I)
      else Result.SwitchToPass(WfcMusicVoicesPassLabel(I));
      Result.PassMode := gpmOverlay; Result.ClearDependencies;
      ApplySequenceModelSegmentToGraph(WfcMusicVoicesModelAt(AConfig, I), Result, ABoundaries[I]);
    end;
    for I := 0 to High(AConfig.Voices) do
    begin
      Result.SwitchToPass(WfcMusicVoicesPassLabel(I + 2));
      Bindings := VoiceBindings(AConfig, P, I);
      RequireSequencePartialProjectionMapsFromPasses(AConfig.Voices[I].Model, Result, Bindings);
    end;
    for K := 0 to High(P.Classes) do
    begin
      C := P.Classes[K]; Present := nil; Absent := nil;
      for J := 0 to High(P.Harmony) do
        if ContainsClass(P.Harmony[J], C) then AppendToken(Present, AConfig.HarmonyModel.PublicTokenAt(J))
        else AppendToken(Absent, AConfig.HarmonyModel.PublicTokenAt(J));
      Result.SwitchToPass(WfcMusicVoicesCoveragePassLabel(C));
      Result.PassMode := gpmOverlay; Result.ClearDependencies;
      Value := SupplierValue(-1); Result.AddValue(Value);
      RequirePartialProjectedSequenceFromPass(AConfig.HarmonyModel, Result, Value,
        WfcMusicVoicesPassLabel(0), Absent);
      for I := 0 to High(AConfig.Voices) do
      begin
        Value := SupplierValue(I); Result.AddValue(Value);
        RequirePartialProjectedSequenceFromPass(AConfig.HarmonyModel, Result, Value,
          WfcMusicVoicesPassLabel(0), Present);
        Allowed := nil;
        for J := 0 to High(P.Voices[I]) do
          if VoiceContains(P.Voices[I][J].Voices[0], C, AConfig.StepsPerOctave) then
            AppendToken(Allowed, AConfig.Voices[I].Model.PublicTokenAt(J));
        RequirePartialProjectedSequenceFromPass(AConfig.Voices[I].Model, Result, Value,
          WfcMusicVoicesPassLabel(I + 2), Allowed);
        { One proof for each musical assignment: a later supplier is legal only
          when every earlier voice omits this class. Empty negative relations
          explicitly forbid it; they must never become no-op requirements. }
        for Earlier := 0 to I - 1 do
        begin
          Allowed := nil;
          for TokenIndex := 0 to High(P.Voices[Earlier]) do
            if not VoiceContains(P.Voices[Earlier][TokenIndex].Voices[0], C,
              AConfig.StepsPerOctave) then
              AppendToken(Allowed, AConfig.Voices[Earlier].Model.PublicTokenAt(TokenIndex));
          RequirePartialProjectedSequenceFromPass(AConfig.Voices[Earlier].Model,
            Result, Value, WfcMusicVoicesPassLabel(Earlier + 2), Allowed);
        end;
      end;
    end;
    Result.SwitchToPass(0);
  except Result.Free; Result := nil; raise; end;
end;

procedure InitReport(out R: TWfcMusicVoicesValidationReport);
begin
  R := Default(TWfcMusicVoicesValidationReport);
  R.Issue.ModelIndex := -1; R.Issue.VoiceIndex := -1; R.Issue.RelatedVoiceIndex := -1;
  R.Issue.Position := -1; R.Issue.PitchClass := -1;
end;

function Invalid(var R: TWfcMusicVoicesValidationReport;
  const Kind: TWfcMusicVoicesIssueKind; const Detail: String): Boolean;
begin R.Valid := False; R.Issue.Kind := Kind; R.Issue.Detail := Detail; Result := False; end;

function SameBoundary(const A, B: TWfcSequenceSegmentBoundary): Boolean;
begin
  Result := (A.HasPrevious = B.HasPrevious) and (A.PreviousState = B.PreviousState) and
    (A.RequireObservedEnd = B.RequireObservedEnd);
end;

function GraphLayout(const C: TWfcMusicVoicesGraphConfig; const G: TGraph;
  const Classes: TWfcMusicPitchClasses; out Width: Integer): Boolean;
var I, N: Integer; P: TGraph; LabelText: String;
begin
  Result := False; Width := 0;
  if G = nil then Exit;
  N := WfcMusicVoicesModelCount(C);
  if G.Running or G.WrapNeighbors or (G.Dimension.Width < 1) or
    (G.Dimension.Width > TGraphCoordinate(High(Integer))) or
    (G.Dimension.Height <> 1) or (G.Dimension.Depth <> 1) or
    (G.TotalPassCount <> N + Length(Classes)) then Exit;
  Width := Integer(G.Dimension.Width);
  for I := 0 to G.TotalPassCount - 1 do
  begin
    if I < N then LabelText := WfcMusicVoicesPassLabel(I)
    else LabelText := WfcMusicVoicesCoveragePassLabel(Classes[I - N]);
    P := G.PassGraph[I];
    if (P.CurrentPass <> LabelText) or (P.Dimension.Width <> G.Dimension.Width) or
      (P.Dimension.Height <> 1) or (P.Dimension.Depth <> 1) or
      P.WrapNeighbors or (P.PassMode <> gpmOverlay) then Exit;
  end;
  Result := True;
end;

function ValidateWfcMusicVoicesGenerated(const AConfig: TWfcMusicVoicesGraphConfig;
  const AGraph: TGraph; const ABoundaries: TWfcMusicVoicesBoundaries;
  const AGenerated: TWfcMusicVoicesGenerated;
  out AReport: TWfcMusicVoicesValidationReport): Boolean;
var
  P: TPreparedMusic; I, J, K, Width, N, BadPosition, State, Supplier: Integer;
  M: TWfcSequenceModel; SReport: TWfcSequenceGraphValidationReport;
  Captured: TWfcGeneratedSequenceSegment; F, Previous: TWfcMusicEnsembleFrame;
  H, Actual: TWfcMusicPitchClassSet; Rhythm: TWfcMusicRhythmFrame;
  G: TGraph; Value: TGraphValue; Domain: TGraphValues; Found: Boolean;
begin
  Result := False; InitReport(AReport);
  try
    P := PrepareMusic(AConfig); ValidateBoundaries(AConfig, ABoundaries);
    if not GraphLayout(AConfig, AGraph, P.Classes, Width) then
      Exit(Invalid(AReport, wmvikGraphShape, 'graph layout, names or modes differ'));
    N := WfcMusicVoicesModelCount(AConfig);
    if (Length(AGenerated.Layers) <> N) or (Length(AGenerated.Frames) <> Width) or
      (Length(AGenerated.Coverage) <> Length(P.Classes)) then
      Exit(Invalid(AReport, wmvikGraphShape, 'generated vector lengths differ'));
    for I := 0 to N - 1 do
    begin
      AReport.Issue.ModelIndex := I;
      M := WfcMusicVoicesModelAt(AConfig, I); G := AGraph.PassGraph[I];
      if not SameBoundary(ABoundaries[I], AGenerated.Layers[I].Boundary) then
        Exit(Invalid(AReport, wmvikBoundary, 'generated boundary differs from original'));
      if (Length(AGenerated.Layers[I].StateIndices) <> Width) or
        (Length(AGenerated.Layers[I].Tokens) <> Width) then
        Exit(Invalid(AReport, wmvikSequence, 'generated layer length differs'));
      if not CaptureSolvedSequenceSegment(M, G, ABoundaries[I], Captured, SReport) then
      begin
        AReport.Issue.SequenceIssue := SReport.Issue;
        Exit(Invalid(AReport, wmvikSequence, 'latent graph capture failed'));
      end;
      if not ValidateSequenceSegmentStatePath(M, AGenerated.Layers[I].StateIndices,
        ABoundaries[I], SReport) then
      begin
        AReport.Issue.SequenceIssue := SReport.Issue;
        Exit(Invalid(AReport, wmvikSequence, 'latent state path is invalid'));
      end;
      for J := 0 to Width - 1 do
      begin
        AReport.Issue.Position := J;
        State := AGenerated.Layers[I].StateIndices[J];
        if (Captured.StateIndices[J] <> State) or
          (AGenerated.Layers[I].Tokens[J] <> M.PublicTokenAt(M.StateEmittedTokenIndexAt(State))) then
          Exit(Invalid(AReport, wmvikToken, 'state projection or graph capture differs'));
      end;
      if not SequenceStatesSatisfyEntryConstraints(M, G,
        AGenerated.Layers[I].StateIndices, BadPosition) then
      begin
        AReport.Issue.Position := BadPosition;
        Exit(Invalid(AReport, wmvikEntryConstraint, 'state violates caller domain or lock'));
      end;
      Inc(AReport.CheckedModels);
    end;
    AReport.Issue.ModelIndex := -1;
    for J := 0 to Width - 1 do
    begin
      AReport.Issue.Position := J;
      F := Default(TWfcMusicEnsembleFrame); SetLength(F.Voices, Length(AConfig.Voices));
      for I := 0 to High(AConfig.Voices) do
      begin
        AReport.Issue.VoiceIndex := I;
        Previous := DecodeWfcMusicEnsembleFrame(AGenerated.Layers[I + 2].Tokens[J]);
        F.Voices[I] := MakeWfcMusicVoiceCell(Previous.Voices[0].Action, Previous.Voices[0].Tones);
        if not InRange(F.Voices[I], AConfig.Voices[I]) then
          Exit(Invalid(AReport, wmvikRange, 'voice exceeds its pitch range'));
      end;
      AReport.Issue.VoiceIndex := -1;
      if EncodeWfcMusicEnsembleFrame(F) <> EncodeWfcMusicEnsembleFrame(AGenerated.Frames[J]) then
        Exit(Invalid(AReport, wmvikFrame, 'assembled frame differs from independent voices'));
      Rhythm := DecodeWfcMusicRhythmFrame(AGenerated.Layers[1].Tokens[J]);
      if EncodeWfcMusicRhythmFrame(ProjectWfcMusicEnsembleFrameToRhythm(F)) <>
        EncodeWfcMusicRhythmFrame(Rhythm) then
        Exit(Invalid(AReport, wmvikRhythm, 'shared rhythm differs from voice actions'));
      H := DecodeWfcMusicPitchClassSet(AGenerated.Layers[0].Tokens[J]);
      Actual := ProjectWfcMusicEnsembleFrameToPitchClassSet(F, AConfig.StepsPerOctave);
      if AConfig.HarmonyMode = wmehmExact then Found := WfcMusicPitchClassSetsEqual(Actual, H)
      else Found := WfcMusicPitchClassSetIsSubset(Actual, H);
      if not Found then Exit(Invalid(AReport, wmvikHarmony, 'independent harmonic union differs'));
      for I := 0 to High(AConfig.PairConstraints) do
      begin
        AReport.Issue.VoiceIndex := AConfig.PairConstraints[I].LowerVoice;
        AReport.Issue.RelatedVoiceIndex := AConfig.PairConstraints[I].UpperVoice;
        if not PairAllows(F.Voices[AReport.Issue.VoiceIndex],
          F.Voices[AReport.Issue.RelatedVoiceIndex], AConfig.PairConstraints[I]) then
          Exit(Invalid(AReport, wmvikPair, 'voice pair gap or rest policy differs'));
      end;
      AReport.Issue.VoiceIndex := -1; AReport.Issue.RelatedVoiceIndex := -1;
      for K := 0 to High(P.Classes) do
      begin
        AReport.Issue.PitchClass := P.Classes[K];
        if (AGenerated.Coverage[K].PitchClass <> P.Classes[K]) or
          (Length(AGenerated.Coverage[K].Suppliers) <> Width) then
          Exit(Invalid(AReport, wmvikWitness, 'coverage layout differs'));
        Supplier := AGenerated.Coverage[K].Suppliers[J];
        CheckInteger(Supplier, -1, High(AConfig.Voices), 'witness supplier');
        if Supplier = -1 then Found := not ContainsClass(H, P.Classes[K])
        else Found := ContainsClass(H, P.Classes[K]) and
          VoiceContains(F.Voices[Supplier], P.Classes[K], AConfig.StepsPerOctave);
        if not Found then Exit(Invalid(AReport, wmvikWitness, 'selected voice does not witness coverage'));
        for I := 0 to Supplier - 1 do
          if VoiceContains(F.Voices[I], P.Classes[K], AConfig.StepsPerOctave) then
            Exit(Invalid(AReport, wmvikWitness, 'coverage supplier is not the lowest matching voice'));
        G := AGraph.PassGraph[N + K]; Value := SupplierValue(Supplier);
        if G[J, 0, 0].Empty or (G[J, 0, 0].Value <> Value) then
          Exit(Invalid(AReport, wmvikWitness, 'graph witness differs from captured witness'));
        if G.HasAllowedValues(J, 0, 0) then
        begin
          Domain := G.CopyAllowedValues(J, 0, 0); Found := False;
          for I := 0 to High(Domain) do if Domain[I] = Value then Found := True;
          if not Found then Exit(Invalid(AReport, wmvikEntryConstraint, 'witness violates caller domain'));
        end;
      end;
      Inc(AReport.CheckedCells);
    end;
    AReport.Valid := True; AReport.Issue := Default(TWfcMusicVoicesValidationIssue);
    AReport.Issue.ModelIndex := -1; AReport.Issue.VoiceIndex := -1;
    AReport.Issue.RelatedVoiceIndex := -1; AReport.Issue.Position := -1; AReport.Issue.PitchClass := -1;
    Result := True;
  except
    on E: EOutOfMemory do raise;
    on E: Exception do Result := Invalid(AReport, wmvikInternal, E.Message);
  end;
end;

function CaptureSolvedWfcMusicVoices(const AConfig: TWfcMusicVoicesGraphConfig;
  const AGraph: TGraph; const ABoundaries: TWfcMusicVoicesBoundaries;
  out AGenerated: TWfcMusicVoicesGenerated;
  out AReport: TWfcMusicVoicesValidationReport): Boolean;
var P: TPreparedMusic; Pending: TWfcMusicVoicesGenerated;
  I, J, K, N, Width: Integer; One: TWfcMusicEnsembleFrame;
  SReport: TWfcSequenceGraphValidationReport; G: TGraph; Found: Boolean;
begin
  Result := False; AGenerated := Default(TWfcMusicVoicesGenerated); InitReport(AReport);
  try
    P := PrepareMusic(AConfig); ValidateBoundaries(AConfig, ABoundaries);
    if not GraphLayout(AConfig, AGraph, P.Classes, Width) then
      Exit(Invalid(AReport, wmvikGraphShape, 'graph layout differs'));
    N := WfcMusicVoicesModelCount(AConfig);
    Pending := Default(TWfcMusicVoicesGenerated); SetLength(Pending.Layers, N);
    for I := 0 to N - 1 do
      if not CaptureSolvedSequenceSegment(WfcMusicVoicesModelAt(AConfig, I),
        AGraph.PassGraph[I], ABoundaries[I], Pending.Layers[I], SReport) then
      begin
        AReport.Issue.ModelIndex := I; AReport.Issue.SequenceIssue := SReport.Issue;
        Exit(Invalid(AReport, wmvikSequence, 'latent capture failed'));
      end;
    SetLength(Pending.Frames, Width);
    for J := 0 to Width - 1 do
    begin
      SetLength(Pending.Frames[J].Voices, Length(AConfig.Voices));
      for I := 0 to High(AConfig.Voices) do
      begin
        One := DecodeWfcMusicEnsembleFrame(Pending.Layers[I + 2].Tokens[J]);
        Pending.Frames[J].Voices[I] := MakeWfcMusicVoiceCell(One.Voices[0].Action, One.Voices[0].Tones);
      end;
    end;
    SetLength(Pending.Coverage, Length(P.Classes));
    for K := 0 to High(P.Classes) do
    begin
      Pending.Coverage[K].PitchClass := P.Classes[K];
      SetLength(Pending.Coverage[K].Suppliers, Width);
      G := AGraph.PassGraph[N + K];
      for J := 0 to Width - 1 do
      begin
        Found := False;
        if not G[J, 0, 0].Empty then
          for I := -1 to High(AConfig.Voices) do
            if G[J, 0, 0].Value = SupplierValue(I) then
            begin Pending.Coverage[K].Suppliers[J] := I; Found := True; Break; end;
        if not Found then
        begin
          AReport.Issue.Position := J; AReport.Issue.PitchClass := P.Classes[K];
          Exit(Invalid(AReport, wmvikWitness, 'unknown or empty graph witness'));
        end;
      end;
    end;
    if not ValidateWfcMusicVoicesGenerated(AConfig, AGraph, ABoundaries, Pending, AReport) then Exit;
    AGenerated := Pending; Result := True;
  except
    on E: EOutOfMemory do raise;
    on E: Exception do Result := Invalid(AReport, wmvikInternal, E.Message);
  end;
end;

end.
