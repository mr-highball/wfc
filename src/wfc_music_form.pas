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
unit wfc_music_form;

{$mode delphi}{$H+}

interface

uses SysUtils, wfc, wfc_model, wfc_music_arrangement;

const
  WFC_MUSIC_FORM_VERSION = 1;
  WFC_MUSIC_FORM_SIGNATURE_VERSION = 1;
  WFC_MUSIC_FORM_PASS_FORM = 'form';
  WFC_MUSIC_FORM_PASS_HARMONY = 'harmonic-intent';
  WFC_MUSIC_FORM_PASS_GESTURE = 'gesture';

type
  EWfcMusicForm = class(Exception);
  TWfcMusicFormRole = (wmfrQuestion, wmfrAnswer, wmfrContrast, wmfrReturn);
  TWfcMusicFormRoles = set of TWfcMusicFormRole;
  TWfcMusicFormFunction = (wmffTonic, wmffExpansion,
    wmffPredominant, wmffDominant);
  TWfcMusicFormCadence = (wmfcNone, wmfcHalf, wmfcAuthentic);
  TWfcMusicFormCadences = set of TWfcMusicFormCadence;

  { A finite caller-owned vocabulary. MotionPitches are ordered chord slots,
    not claims of separately tracked contrapuntal voices. No MIDI scale,
    instrument, chord spelling, or temperament is hardcoded here. }
  TWfcMusicFormHarmony = record
    LabelText: String;
    HarmonicFunction: TWfcMusicFormFunction;
    MotionPitches: TWfcModelIntegerArray;
  end;
  TWfcMusicFormHarmonies = array of TWfcMusicFormHarmony;
  TWfcMusicFormGesture = record
    LabelText: String;
    MotifIndex: Integer;
    Roles: TWfcMusicFormRoles;
    Cadences: TWfcMusicFormCadences;
    AttackCount: Integer;
  end;
  TWfcMusicFormGestures = array of TWfcMusicFormGesture;
  { Entries identify the actual authored realization, not an equivalence
    class. Several entries may use the same harmony and gesture. Anchors are
    its first/last upper-line attacks; the acoustic adapter must verify them. }
  TWfcMusicFormRealization = record
    HarmonyIndex: Integer;
    GestureIndex: Integer;
    EntryPitch: Integer;
    ExitPitch: Integer;
  end;
  TWfcMusicFormRealizations = array of TWfcMusicFormRealization;

  TWfcMusicFormConfig = record
    TotalCells: TWfcMusicArrangementWide;
    CellsPerBar: Integer;
    PhraseBars: Integer;
    Seed: TGraphSeed;
    ThemeMotifIndex: Integer;
    ContrastMotifIndex: Integer;
    MaxChordMotion: Integer;
    MaxMelodyMotion: Integer;
    ContrastMinimumAttacks: Integer;
    Harmonies: TWfcMusicFormHarmonies;
    Gestures: TWfcMusicFormGestures;
    Realizations: TWfcMusicFormRealizations;
    Search: TGraphNegotiationOptions;
  end;

  TWfcMusicFormBar = record
    Index: TWfcMusicArrangementWide;
    StartCell: TWfcMusicArrangementWide;
    PhraseIndex: TWfcMusicArrangementWide;
    CellCount: Integer;
    PhrasePosition: Integer;
    Role: TWfcMusicFormRole;
    HarmonicFunction: TWfcMusicFormFunction;
    Cadence: TWfcMusicFormCadence;
    MotifIndex: Integer;
    HarmonyIndex: Integer;
    GestureIndex: Integer;
    RealizationIndex: Integer;
  end;
  TWfcMusicFormBars = array of TWfcMusicFormBar;

  TWfcMusicFormFrontier = record
    HasPrevious: Boolean;
    NextBar: TWfcMusicArrangementWide;
    LastHarmonyIndex: Integer;
    LastRealizationIndex: Integer;
  end;

  { Immutable owned result. CopyBars returns a detached array; BarAt returns
    an unmanaged value record. The cursor never retains returned plans. }
  TWfcMusicFormPhrasePlan = class
  private
    FBars: TWfcMusicFormBars;
    FPhraseIndex: TWfcMusicArrangementWide;
    FSeed: TGraphSeed;
    FConfigSignature: Cardinal;
    FSignature: Cardinal;
    function GetBarCount: Integer;
  public
    function BarAt(const AIndex: Integer): TWfcMusicFormBar;
    function CopyBars: TWfcMusicFormBars;
    property BarCount: Integer read GetBarCount;
    property PhraseIndex: TWfcMusicArrangementWide read FPhraseIndex;
    property Seed: TGraphSeed read FSeed;
    property ConfigSignature: Cardinal read FConfigSignature;
    property Signature: Cardinal read FSignature;
  end;

  { Deep-copies the catalog. Retains only that finite catalog and a boundary;
    each Next allocates one phrase-sized graph, then releases it. Previously
    returned history is never repaired. Numeric/search limits are explicit;
    no allocation is proportional to TotalCells. }
  TWfcMusicFormCursor = class
  strict private
    FConfig: TWfcMusicFormConfig;
    FFrontier: TWfcMusicFormFrontier;
    FStatus: TWfcMusicArrangementStatus;
    FFailure: String;
    FRunning: Boolean;
  public
    constructor Create(const AConfig: TWfcMusicFormConfig);
    function Next(out APlan: TWfcMusicFormPhrasePlan;
      out AReport: TGraphNegotiationReport): TWfcMusicArrangementStep;
    function CopyFrontier: TWfcMusicFormFrontier;
    procedure Cancel;
    property Status: TWfcMusicArrangementStatus read FStatus;
    property Failure: String read FFailure;
  end;

function DefaultWfcMusicFormConfig(const ATotalCells: TWfcMusicArrangementWide;
  const ASeed: TGraphSeed): TWfcMusicFormConfig;
function CopyWfcMusicFormConfig(const AConfig: TWfcMusicFormConfig):
  TWfcMusicFormConfig;
function InitialWfcMusicFormFrontier: TWfcMusicFormFrontier;
procedure ValidateWfcMusicFormConfig(const AConfig: TWfcMusicFormConfig);
function WfcMusicFormTotalBars(const AConfig: TWfcMusicFormConfig):
  TWfcMusicArrangementWide;
function TryPlanWfcMusicFormPhrase(const AConfig: TWfcMusicFormConfig;
  const AFrontier: TWfcMusicFormFrontier; out APlan: TWfcMusicFormPhrasePlan;
  out AReport: TGraphNegotiationReport): Boolean;
function ValidateWfcMusicFormPhrase(const AConfig: TWfcMusicFormConfig;
  const AFrontier: TWfcMusicFormFrontier; const APlan: TWfcMusicFormPhrasePlan;
  out AFailure: String): Boolean;
{ Independent semantic audit of detached records; does not trust a signature
  or a solver report as proof. The frontier is a caller-supplied boundary
  witness, not authentication of previously generated history. }
function ValidateWfcMusicFormBars(const AConfig: TWfcMusicFormConfig;
  const AFrontier: TWfcMusicFormFrontier; const ABars: TWfcMusicFormBars;
  out AFailure: String): Boolean;
function WfcMusicFormRoleName(const AValue: TWfcMusicFormRole): String;
function WfcMusicFormFunctionName(const AValue: TWfcMusicFormFunction): String;
function WfcMusicFormCadenceName(const AValue: TWfcMusicFormCadence): String;

implementation

procedure FormError(const AMessage: String);
begin
  raise EWfcMusicForm.Create('invalid music form: ' + AMessage);
end;

procedure CheckNumber(const AValue, AMinimum, AMaximum:
  TWfcMusicArrangementWide; const ALabel: String);
begin
  { Positive relational tests fail closed for NaN and undefined. The strict
    comparison against Trunc also rejects JS strings, null and fractions. }
  if not ((AValue >= AMinimum) and (AValue <= AMaximum)) then
    FormError(ALabel + ' is outside its exact integer range');
  {$IFDEF PAS2JS}
  if AValue <> Trunc(AValue) then
    FormError(ALabel + ' must be an exact integer');
  {$ENDIF}
end;

procedure CheckBoolean(const AValue: Boolean; const ALabel: String);
begin
  {$IFDEF PAS2JS}
  if (AValue <> True) and (AValue <> False) then
    FormError(ALabel + ' must be Boolean');
  {$ENDIF}
end;

procedure CheckLabel(const AValue, ALabel: String);
var I: Integer;
begin
  if Length(AValue) = 0 then FormError(ALabel + ' cannot be empty');
  for I := 1 to Length(AValue) do
    if not (Ord(AValue[I]) in [32..126]) then
      FormError(ALabel + ' must be printable ASCII');
end;

function DefaultWfcMusicFormConfig(const ATotalCells: TWfcMusicArrangementWide;
  const ASeed: TGraphSeed): TWfcMusicFormConfig;
begin
  Result := Default(TWfcMusicFormConfig);
  Result.TotalCells := ATotalCells;
  Result.CellsPerBar := 8;
  Result.PhraseBars := 4;
  Result.Seed := ASeed;
  Result.ThemeMotifIndex := 0;
  Result.ContrastMotifIndex := 1;
  Result.MaxChordMotion := 7;
  Result.MaxMelodyMotion := 7;
  Result.ContrastMinimumAttacks := 4;
  Result.Search := DefaultGraphNegotiationOptions;
end;

function CopyWfcMusicFormConfig(const AConfig: TWfcMusicFormConfig):
  TWfcMusicFormConfig;
var I: Integer;
begin
  Result := AConfig;
  Result.Harmonies := Copy(AConfig.Harmonies, 0, Length(AConfig.Harmonies));
  for I := 0 to High(Result.Harmonies) do
    Result.Harmonies[I].MotionPitches := Copy(AConfig.Harmonies[I].MotionPitches,
      0, Length(AConfig.Harmonies[I].MotionPitches));
  Result.Gestures := Copy(AConfig.Gestures, 0, Length(AConfig.Gestures));
  Result.Realizations := Copy(AConfig.Realizations, 0, Length(AConfig.Realizations));
end;

procedure ValidateWfcMusicFormConfig(const AConfig: TWfcMusicFormConfig);
var I, J, Slots: Integer;
begin
  CheckNumber(AConfig.TotalCells, 1,
    WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER, 'total cells');
  CheckNumber(AConfig.CellsPerBar, 1, High(Integer), 'cells per bar');
  CheckNumber(AConfig.PhraseBars, 1, High(Integer) - 1, 'phrase bars');
  CheckNumber(AConfig.Seed, 0, Cardinal($FFFFFFFF), 'seed');
  CheckNumber(AConfig.ThemeMotifIndex, 0, High(Integer), 'theme motif');
  CheckNumber(AConfig.ContrastMotifIndex, 0, High(Integer), 'contrast motif');
  if AConfig.ThemeMotifIndex = AConfig.ContrastMotifIndex then
    FormError('theme and contrast motifs must differ');
  CheckNumber(AConfig.MaxChordMotion, 0, High(Integer), 'chord motion');
  CheckNumber(AConfig.MaxMelodyMotion, 0, High(Integer), 'melody motion');
  CheckNumber(AConfig.ContrastMinimumAttacks, 0, High(Integer), 'contrast attacks');
  CheckNumber(AConfig.Search.SolveOptions.MaxBacktracks, 0, High(Integer), 'local budget');
  CheckNumber(AConfig.Search.MaxPassBacktracks, 0, High(Integer), 'pass budget');
  CheckBoolean(AConfig.Search.SolveOptions.CaptureTrace, 'capture trace');
  { The adapter uses the existing versioned dense immutable-model envelope.
    These are finite CATALOG bounds, never a limit on composition duration. }
  CheckNumber(Length(AConfig.Harmonies), 1, WFC_MODEL_MAX_VALUE_COUNT, 'harmony count');
  CheckNumber(Length(AConfig.Gestures), 1, WFC_MODEL_MAX_VALUE_COUNT, 'gesture count');
  CheckNumber(Length(AConfig.Realizations), 1, WFC_MODEL_MAX_VALUE_COUNT, 'realization count');
  Slots := Length(AConfig.Harmonies[0].MotionPitches);
  if Slots = 0 then FormError('chord slots cannot be empty');
  for I := 0 to High(AConfig.Harmonies) do
  begin
    CheckLabel(AConfig.Harmonies[I].LabelText, 'harmony label');
    for J := 0 to I - 1 do
      if AConfig.Harmonies[J].LabelText = AConfig.Harmonies[I].LabelText then
        FormError('duplicate harmony label');
    CheckNumber(Ord(AConfig.Harmonies[I].HarmonicFunction),
      Ord(Low(TWfcMusicFormFunction)), Ord(High(TWfcMusicFormFunction)), 'function');
    if Length(AConfig.Harmonies[I].MotionPitches) <> Slots then
      FormError('all harmonies must have the same chord-slot count');
    for J := 0 to Slots - 1 do
    begin
      CheckNumber(AConfig.Harmonies[I].MotionPitches[J], 0, High(Integer), 'chord pitch');
      if J > 0 then
        if AConfig.Harmonies[I].MotionPitches[J - 1] > AConfig.Harmonies[I].MotionPitches[J] then
          FormError('chord slots must be sorted');
    end;
  end;
  for I := 0 to High(AConfig.Gestures) do
  begin
    CheckLabel(AConfig.Gestures[I].LabelText, 'gesture label');
    for J := 0 to I - 1 do
      if AConfig.Gestures[J].LabelText = AConfig.Gestures[I].LabelText then
        FormError('duplicate gesture label');
    CheckNumber(AConfig.Gestures[I].MotifIndex, 0, High(Integer), 'gesture motif');
    CheckNumber(AConfig.Gestures[I].AttackCount, 0, AConfig.CellsPerBar, 'gesture attacks');
    if AConfig.Gestures[I].Roles = [] then FormError('gesture needs a role');
    if AConfig.Gestures[I].Cadences = [] then FormError('gesture needs a cadence policy');
  end;
  for I := 0 to High(AConfig.Realizations) do
  begin
    CheckNumber(AConfig.Realizations[I].HarmonyIndex, 0, High(AConfig.Harmonies), 'realization harmony');
    CheckNumber(AConfig.Realizations[I].GestureIndex, 0, High(AConfig.Gestures), 'realization gesture');
    CheckNumber(AConfig.Realizations[I].EntryPitch, 0, High(Integer), 'entry pitch');
    CheckNumber(AConfig.Realizations[I].ExitPitch, 0, High(Integer), 'exit pitch');
  end;
end;

function WfcMusicFormTotalBars(const AConfig: TWfcMusicFormConfig):
  TWfcMusicArrangementWide;
begin
  CheckNumber(AConfig.TotalCells, 1, WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER, 'total cells');
  CheckNumber(AConfig.CellsPerBar, 1, High(Integer), 'cells per bar');
  Result := AConfig.TotalCells div AConfig.CellsPerBar;
  if AConfig.TotalCells mod AConfig.CellsPerBar <> 0 then Inc(Result);
end;

function InitialWfcMusicFormFrontier: TWfcMusicFormFrontier;
begin
  Result.HasPrevious := False;
  Result.NextBar := 0;
  Result.LastHarmonyIndex := -1;
  Result.LastRealizationIndex := -1;
end;

procedure ValidateFrontier(const AConfig: TWfcMusicFormConfig;
  const AFrontier: TWfcMusicFormFrontier);
var Total: TWfcMusicArrangementWide;
begin
  Total := WfcMusicFormTotalBars(AConfig);
  CheckBoolean(AFrontier.HasPrevious, 'boundary presence');
  CheckNumber(AFrontier.NextBar, 0, Total, 'next bar');
  if AFrontier.NextBar = 0 then
  begin
    CheckNumber(AFrontier.LastHarmonyIndex, -1, -1, 'initial harmony');
    CheckNumber(AFrontier.LastRealizationIndex, -1, -1, 'initial realization');
    if AFrontier.HasPrevious then FormError('initial frontier has a predecessor');
    Exit;
  end;
  if not AFrontier.HasPrevious then FormError('noninitial frontier needs a predecessor');
  if (AFrontier.NextBar <> Total) and
      (AFrontier.NextBar mod AConfig.PhraseBars <> 0) then
    FormError('next bar must begin a phrase');
  CheckNumber(AFrontier.LastHarmonyIndex, 0, High(AConfig.Harmonies), 'prior harmony');
  CheckNumber(AFrontier.LastRealizationIndex, 0, High(AConfig.Realizations), 'prior realization');
  if AConfig.Realizations[AFrontier.LastRealizationIndex].HarmonyIndex <>
      AFrontier.LastHarmonyIndex then FormError('inconsistent prior realization');
end;

function ExpectedBars(const AConfig: TWfcMusicFormConfig;
  const AFrontier: TWfcMusicFormFrontier): TWfcMusicFormBars;
var
  Total, Remaining, PhraseIndex, LeftCells: TWfcMusicArrangementWide;
  N, I: Integer;
  Role: TWfcMusicFormRole;
  Closing: Boolean;
begin
  Result := nil;
  Total := WfcMusicFormTotalBars(AConfig);
  Remaining := Total - AFrontier.NextBar;
  if Remaining = 0 then Exit;
  N := AConfig.PhraseBars;
  if Remaining < N then N := Integer(Remaining);
  PhraseIndex := AFrontier.NextBar div AConfig.PhraseBars;
  Role := TWfcMusicFormRole(PhraseIndex mod 4);
  if Remaining = N then
    if PhraseIndex = 0 then Role := wmfrAnswer
    else if Role in [wmfrQuestion, wmfrContrast] then Role := wmfrReturn;
  Closing := Role in [wmfrAnswer, wmfrReturn];
  SetLength(Result, N);
  for I := 0 to N - 1 do
  begin
    Result[I].Index := AFrontier.NextBar + I;
    { Every bar begins before TotalCells, so this product stays in the exact
      wide envelope even for a partial final bar near 2^53-1. }
    Result[I].StartCell := Result[I].Index * AConfig.CellsPerBar;
    Result[I].PhraseIndex := PhraseIndex;
    Result[I].PhrasePosition := I;
    LeftCells := AConfig.TotalCells - Result[I].StartCell;
    Result[I].CellCount := AConfig.CellsPerBar;
    if LeftCells < AConfig.CellsPerBar then Result[I].CellCount := Integer(LeftCells);
    Result[I].Role := Role;
    Result[I].Cadence := wmfcNone;
    if Role = wmfrContrast then Result[I].MotifIndex := AConfig.ContrastMotifIndex
    else Result[I].MotifIndex := AConfig.ThemeMotifIndex;
    if Closing then
    begin
      if I = N - 1 then Result[I].HarmonicFunction := wmffTonic
      else if I = N - 2 then Result[I].HarmonicFunction := wmffDominant
      else if I = 0 then Result[I].HarmonicFunction := wmffTonic
      else Result[I].HarmonicFunction := wmffPredominant;
      if I = N - 1 then Result[I].Cadence := wmfcAuthentic;
    end
    else
    begin
      if I = N - 1 then Result[I].HarmonicFunction := wmffDominant
      else if I = 0 then
        if Role = wmfrQuestion then Result[I].HarmonicFunction := wmffTonic
        else Result[I].HarmonicFunction := wmffExpansion
      else if (I = N - 2) or (Role = wmfrContrast) then
        Result[I].HarmonicFunction := wmffPredominant
      else Result[I].HarmonicFunction := wmffExpansion;
      if I = N - 1 then Result[I].Cadence := wmfcHalf;
    end;
    Result[I].HarmonyIndex := -1;
    Result[I].GestureIndex := -1;
    Result[I].RealizationIndex := -1;
  end;
end;

function ChordMotionAllowed(const AConfig: TWfcMusicFormConfig;
  const ALeft, ARight: Integer): Boolean;
var I: Integer;
begin
  for I := 0 to High(AConfig.Harmonies[ALeft].MotionPitches) do
    if Abs(AConfig.Harmonies[ALeft].MotionPitches[I] -
        AConfig.Harmonies[ARight].MotionPitches[I]) > AConfig.MaxChordMotion then
      Exit(False);
  Result := True;
end;

function MelodyMotionAllowed(const AConfig: TWfcMusicFormConfig;
  const ALeft, ARight: Integer): Boolean;
begin
  Result := Abs(AConfig.Realizations[ALeft].ExitPitch -
    AConfig.Realizations[ARight].EntryPitch) <= AConfig.MaxMelodyMotion;
end;

function GestureAllowed(const AConfig: TWfcMusicFormConfig;
  const ABar: TWfcMusicFormBar; const AIndex: Integer): Boolean;
var G: TWfcMusicFormGesture;
begin
  G := AConfig.Gestures[AIndex];
  Result := (ABar.Role in G.Roles) and (ABar.Cadence in G.Cadences) and
    (ABar.MotifIndex = G.MotifIndex);
  if Result and (ABar.Role = wmfrContrast) and (ABar.Cadence = wmfcNone) then
    Result := G.AttackCount >= AConfig.ContrastMinimumAttacks;
end;

function ValidateWfcMusicFormBars(const AConfig: TWfcMusicFormConfig;
  const AFrontier: TWfcMusicFormFrontier; const ABars: TWfcMusicFormBars;
  out AFailure: String): Boolean;
var E: TWfcMusicFormBars; I, H, G, R, PriorH, PriorR: Integer;
begin
  Result := False;
  AFailure := '';
  try
    ValidateWfcMusicFormConfig(AConfig);
    ValidateFrontier(AConfig, AFrontier);
    E := ExpectedBars(AConfig, AFrontier);
    if (Length(E) = 0) or (Length(ABars) <> Length(E)) then
      FormError('phrase bar count differs from the requested extent');
    PriorH := AFrontier.LastHarmonyIndex;
    PriorR := AFrontier.LastRealizationIndex;
    for I := 0 to High(ABars) do
    begin
      if (ABars[I].Index <> E[I].Index) or
          (ABars[I].StartCell <> E[I].StartCell) or
          (ABars[I].PhraseIndex <> E[I].PhraseIndex) or
          (ABars[I].PhrasePosition <> E[I].PhrasePosition) or
          (ABars[I].CellCount <> E[I].CellCount) or
          (ABars[I].Role <> E[I].Role) or
          (ABars[I].HarmonicFunction <> E[I].HarmonicFunction) or
          (ABars[I].Cadence <> E[I].Cadence) or
          (ABars[I].MotifIndex <> E[I].MotifIndex) then
        FormError('bar intent or position differs from the form grammar');
      H := ABars[I].HarmonyIndex;
      G := ABars[I].GestureIndex;
      R := ABars[I].RealizationIndex;
      CheckNumber(H, 0, High(AConfig.Harmonies), 'bar harmony');
      CheckNumber(G, 0, High(AConfig.Gestures), 'bar gesture');
      CheckNumber(R, 0, High(AConfig.Realizations), 'bar realization');
      if (AConfig.Realizations[R].HarmonyIndex <> H) or
          (AConfig.Realizations[R].GestureIndex <> G) then
        FormError('bar does not match its exact realization');
      if AConfig.Harmonies[H].HarmonicFunction <> ABars[I].HarmonicFunction then
        FormError('bar harmony does not realize its required function');
      if not GestureAllowed(AConfig, ABars[I], G) then
        FormError('bar gesture does not realize its role, motif or cadence');
      if PriorH >= 0 then
        if not ChordMotionAllowed(AConfig, PriorH, H) then
          FormError('chord-slot motion exceeds policy');
      if PriorR >= 0 then
        if not MelodyMotionAllowed(AConfig, PriorR, R) then
          FormError('upper-line boundary motion exceeds policy');
      PriorH := H;
      PriorR := R;
    end;
    Result := True;
  except
    on EForm: EWfcMusicForm do AFailure := EForm.Message;
  end;
end;

procedure HashByte(var AHash: Cardinal; const AByte: Byte);
{$PUSH}{$Q-}
var H: Cardinal;
begin
  H := AHash xor Cardinal(AByte);
  AHash := (H + (H shl 1) + (H shl 4) + (H shl 7) +
    (H shl 8) + (H shl 24)) and Cardinal($FFFFFFFF);
end;
{$POP}

procedure HashNumber(var AHash: Cardinal; const AValue: TWfcMusicArrangementWide);
var V: TWfcMusicArrangementWide; I: Integer;
begin
  V := AValue;
  for I := 0 to 7 do
  begin
    HashByte(AHash, Byte(V mod 256));
    V := V div 256;
  end;
end;

procedure HashText(var AHash: Cardinal; const AValue: String);
var I: Integer;
begin
  HashNumber(AHash, Length(AValue));
  for I := 1 to Length(AValue) do HashByte(AHash, Byte(Ord(AValue[I])));
end;

function ConfigSignature(const C: TWfcMusicFormConfig): Cardinal;
var I, J: Integer; R: TWfcMusicFormRole; K: TWfcMusicFormCadence;
begin
  Result := Cardinal(2166136261);
  HashNumber(Result, WFC_MUSIC_FORM_VERSION);
  HashNumber(Result, WFC_MUSIC_FORM_SIGNATURE_VERSION);
  HashNumber(Result, C.TotalCells); HashNumber(Result, C.Seed);
  HashNumber(Result, C.CellsPerBar); HashNumber(Result, C.PhraseBars);
  HashNumber(Result, C.ThemeMotifIndex); HashNumber(Result, C.ContrastMotifIndex);
  HashNumber(Result, C.MaxChordMotion); HashNumber(Result, C.MaxMelodyMotion);
  HashNumber(Result, C.ContrastMinimumAttacks);
  { Search budgets/capture are operational, not musical identity. Actual
    output is hashed below; solver reports retain search configuration. }
  HashNumber(Result, Length(C.Harmonies));
  for I := 0 to High(C.Harmonies) do
  begin
    HashText(Result, C.Harmonies[I].LabelText);
    HashNumber(Result, Ord(C.Harmonies[I].HarmonicFunction));
    HashNumber(Result, Length(C.Harmonies[I].MotionPitches));
    for J := 0 to High(C.Harmonies[I].MotionPitches) do
      HashNumber(Result, C.Harmonies[I].MotionPitches[J]);
  end;
  HashNumber(Result, Length(C.Gestures));
  for I := 0 to High(C.Gestures) do
  begin
    HashText(Result, C.Gestures[I].LabelText);
    HashNumber(Result, C.Gestures[I].MotifIndex);
    for R := Low(TWfcMusicFormRole) to High(TWfcMusicFormRole) do
      HashByte(Result, Ord(R in C.Gestures[I].Roles));
    for K := Low(TWfcMusicFormCadence) to High(TWfcMusicFormCadence) do
      HashByte(Result, Ord(K in C.Gestures[I].Cadences));
    HashNumber(Result, C.Gestures[I].AttackCount);
  end;
  HashNumber(Result, Length(C.Realizations));
  for I := 0 to High(C.Realizations) do
  begin
    HashNumber(Result, C.Realizations[I].HarmonyIndex);
    HashNumber(Result, C.Realizations[I].GestureIndex);
    HashNumber(Result, C.Realizations[I].EntryPitch);
    HashNumber(Result, C.Realizations[I].ExitPitch);
  end;
end;

function PlanSignature(const APlan: TWfcMusicFormPhrasePlan;
  const AFrontier: TWfcMusicFormFrontier): Cardinal;
var I: Integer; B: TWfcMusicFormBar;
begin
  Result := Cardinal(2166136261);
  HashNumber(Result, WFC_MUSIC_FORM_SIGNATURE_VERSION);
  HashNumber(Result, APlan.ConfigSignature);
  HashNumber(Result, APlan.PhraseIndex); HashNumber(Result, APlan.Seed);
  HashByte(Result, Ord(AFrontier.HasPrevious));
  HashNumber(Result, AFrontier.NextBar);
  HashNumber(Result, TWfcMusicArrangementWide(AFrontier.LastHarmonyIndex) + 1);
  HashNumber(Result, TWfcMusicArrangementWide(AFrontier.LastRealizationIndex) + 1);
  HashNumber(Result, APlan.BarCount);
  for I := 0 to APlan.BarCount - 1 do
  begin
    B := APlan.BarAt(I);
    HashNumber(Result, B.Index); HashNumber(Result, B.StartCell);
    HashNumber(Result, B.PhraseIndex); HashNumber(Result, B.PhrasePosition);
    HashNumber(Result, B.CellCount); HashNumber(Result, Ord(B.Role));
    HashNumber(Result, Ord(B.HarmonicFunction)); HashNumber(Result, Ord(B.Cadence));
    HashNumber(Result, B.MotifIndex); HashNumber(Result, B.HarmonyIndex);
    HashNumber(Result, B.GestureIndex); HashNumber(Result, B.RealizationIndex);
  end;
end;

function ValidateWfcMusicFormPhrase(const AConfig: TWfcMusicFormConfig;
  const AFrontier: TWfcMusicFormFrontier; const APlan: TWfcMusicFormPhrasePlan;
  out AFailure: String): Boolean;
begin
  AFailure := '';
  if not Assigned(APlan) then
  begin AFailure := 'music form plan is nil'; Exit(False); end;
  Result := ValidateWfcMusicFormBars(AConfig, AFrontier, APlan.FBars, AFailure);
  if not Result then Exit;
  if (APlan.ConfigSignature <> ConfigSignature(AConfig)) or
      (APlan.PhraseIndex <> AFrontier.NextBar div AConfig.PhraseBars) or
      (APlan.Seed <> WfcMusicArrangementSectionSeed(AConfig.Seed, APlan.PhraseIndex)) or
      (APlan.Signature <> PlanSignature(APlan, AFrontier)) then
  begin AFailure := 'music form signature or phrase provenance mismatch'; Result := False; end;
end;

function Key(const APrefix: String; const AIndex: Integer): TGraphValue;
begin
  Result := TGraphValue(APrefix + IntToStr(AIndex));
end;

procedure AppendValue(var AValues: TGraphValues; const AValue: TGraphValue);
var N: Integer;
begin
  N := Length(AValues); SetLength(AValues, N + 1); AValues[N] := AValue;
end;

procedure AddMotionModel(const AGraph: TGraph; const AConfig: TWfcMusicFormConfig;
  const ARealizations: Boolean);
var
  Tokens: TWfcModelTokens;
  Weights, Relations: TWfcModelIntegerArray;
  Model: TWfcModel;
  N, I, J: Integer;
  Allowed: Boolean;
begin
  if ARealizations then N := Length(AConfig.Realizations)
  else N := Length(AConfig.Harmonies);
  SetLength(Tokens, N); SetLength(Weights, N); SetLength(Relations, 4 * N * N);
  for I := 0 to N - 1 do
  begin
    if ARealizations then Tokens[I] := TWfcModelToken(Key('r:', I))
    else Tokens[I] := TWfcModelToken(Key('h:', I));
    Weights[I] := 1;
    for J := 0 to N - 1 do
    begin
      if ARealizations then Allowed := MelodyMotionAllowed(AConfig, I, J)
      else Allowed := ChordMotionAllowed(AConfig, I, J);
      if Allowed then
      begin
        Relations[(Ord(wmdEast) * N + I) * N + J] := 1;
        Relations[(Ord(wmdWest) * N + J) * N + I] := 1;
      end;
    end;
  end;
  Model := TWfcModel.Create(1, 1, 1, wmbOpen, wmsNone,
    [wmdEast, wmdWest], Tokens, Weights, Relations);
  try ApplyModelToGraph(Model, AGraph); finally Model.Free; end;
end;

function DecodeIndex(const AValue: TGraphValue; const APrefix: String;
  const ACount: Integer): Integer;
var I: Integer;
begin
  for I := 0 to ACount - 1 do if AValue = Key(APrefix, I) then Exit(I);
  FormError('solver produced an unregistered catalog value');
  Result := -1;
end;

function TryPlanWfcMusicFormPhrase(const AConfig: TWfcMusicFormConfig;
  const AFrontier: TWfcMusicFormFrontier; out APlan: TWfcMusicFormPhrasePlan;
  out AReport: TGraphNegotiationReport): Boolean;
const IMPOSSIBLE_FORM = 'form:none';
var
  Graph: TGraph;
  Bars: TWfcMusicFormBars;
  Allowed: TGraphValues;
  Candidate: TWfcMusicFormPhrasePlan;
  I, J, R: Integer;
  Failure: String;
begin
  Result := False; APlan := nil; AReport := Default(TGraphNegotiationReport);
  ValidateWfcMusicFormConfig(AConfig);
  ValidateFrontier(AConfig, AFrontier);
  Bars := ExpectedBars(AConfig, AFrontier);
  if Length(Bars) = 0 then FormError('cannot plan after the requested extent');
  Graph := TGraph.Create;
  Candidate := nil;
  try
    Graph.Reshape(Length(Bars), 1, 1);
    Graph.WrapNeighbors := False;
    Graph.Seed := WfcMusicArrangementSectionSeed(AConfig.Seed, Bars[0].PhraseIndex);
    Graph.CurrentPass := WFC_MUSIC_FORM_PASS_FORM;
    for I := 0 to High(Bars) do Graph.AddValue(Key('f:', I));
    Graph.AddValue(IMPOSSIBLE_FORM);
    for I := 0 to High(Bars) do Graph.SetAllowedValues(I, 0, 0, Key('f:', I));

    Graph.SwitchToPass(WFC_MUSIC_FORM_PASS_HARMONY);
    AddMotionModel(Graph, AConfig, False);
    for I := 0 to High(AConfig.Harmonies) do
    begin
      Allowed := nil;
      for J := 0 to High(Bars) do
        if AConfig.Harmonies[I].HarmonicFunction = Bars[J].HarmonicFunction then
          AppendValue(Allowed, Key('f:', J));
      if Length(Allowed) = 0 then AppendValue(Allowed, IMPOSSIBLE_FORM);
      Graph.Rules[Key('h:', I)].RequireFromPass(WFC_MUSIC_FORM_PASS_FORM, Allowed);
    end;
    if AFrontier.HasPrevious then
    begin
      Allowed := nil;
      for I := 0 to High(AConfig.Harmonies) do
        if ChordMotionAllowed(AConfig, AFrontier.LastHarmonyIndex, I) then
          AppendValue(Allowed, Key('h:', I));
      Graph.SetAllowedValues(0, 0, 0, Allowed);
    end;

    Graph.SwitchToPass(WFC_MUSIC_FORM_PASS_GESTURE);
    AddMotionModel(Graph, AConfig, True);
    for I := 0 to High(AConfig.Realizations) do
    begin
      Graph.Rules[Key('r:', I)].RequireFromPass(WFC_MUSIC_FORM_PASS_HARMONY,
        Key('h:', AConfig.Realizations[I].HarmonyIndex));
      Allowed := nil;
      for J := 0 to High(Bars) do
        if GestureAllowed(AConfig, Bars[J], AConfig.Realizations[I].GestureIndex) then
          AppendValue(Allowed, Key('f:', J));
      if Length(Allowed) = 0 then AppendValue(Allowed, IMPOSSIBLE_FORM);
      Graph.Rules[Key('r:', I)].RequireFromPass(WFC_MUSIC_FORM_PASS_FORM, Allowed);
    end;
    if AFrontier.HasPrevious then
    begin
      Allowed := nil;
      for I := 0 to High(AConfig.Realizations) do
        if MelodyMotionAllowed(AConfig, AFrontier.LastRealizationIndex, I) then
          AppendValue(Allowed, Key('r:', I));
      Graph.SetAllowedValues(0, 0, 0, Allowed);
    end;
    if not Graph.TrySolveNegotiated(AConfig.Search, AReport) then Exit;
    for I := 0 to High(Bars) do
    begin
      Bars[I].HarmonyIndex := DecodeIndex(Graph.PassGraph[1].Entry[I, 0, 0].Value,
        'h:', Length(AConfig.Harmonies));
      R := DecodeIndex(Graph.PassGraph[2].Entry[I, 0, 0].Value,
        'r:', Length(AConfig.Realizations));
      Bars[I].RealizationIndex := R;
      Bars[I].GestureIndex := AConfig.Realizations[R].GestureIndex;
    end;
    Candidate := TWfcMusicFormPhrasePlan.Create;
    Candidate.FBars := Bars;
    Candidate.FPhraseIndex := Bars[0].PhraseIndex;
    Candidate.FSeed := Graph.Seed;
    Candidate.FConfigSignature := ConfigSignature(AConfig);
    Candidate.FSignature := PlanSignature(Candidate, AFrontier);
    if not ValidateWfcMusicFormPhrase(AConfig, AFrontier, Candidate, Failure) then
      FormError('independent candidate audit failed: ' + Failure);
    APlan := Candidate; Candidate := nil; Result := True;
  finally
    Candidate.Free;
    Graph.Free;
  end;
end;

function TWfcMusicFormPhrasePlan.GetBarCount: Integer;
begin Result := Length(FBars); end;

function TWfcMusicFormPhrasePlan.BarAt(const AIndex: Integer): TWfcMusicFormBar;
begin
  CheckNumber(AIndex, 0, High(FBars), 'phrase bar index');
  Result := FBars[AIndex];
end;

function TWfcMusicFormPhrasePlan.CopyBars: TWfcMusicFormBars;
begin Result := Copy(FBars, 0, Length(FBars)); end;

constructor TWfcMusicFormCursor.Create(const AConfig: TWfcMusicFormConfig);
begin
  inherited Create;
  ValidateWfcMusicFormConfig(AConfig);
  FConfig := CopyWfcMusicFormConfig(AConfig);
  FFrontier := InitialWfcMusicFormFrontier;
  FStatus := wmasReady;
end;

function TWfcMusicFormCursor.Next(out APlan: TWfcMusicFormPhrasePlan;
  out AReport: TGraphNegotiationReport): TWfcMusicArrangementStep;
var Last: TWfcMusicFormBar;
begin
  APlan := nil; AReport := Default(TGraphNegotiationReport);
  if FRunning then FormError('cursor Next is not reentrant');
  case FStatus of
    wmasCompleted: Exit(wmaspCompleted);
    wmasCancelled: Exit(wmaspCancelled);
    wmasFailed: Exit(wmaspFailed);
  end;
  FRunning := True;
  try
    try
      if not TryPlanWfcMusicFormPhrase(FConfig, FFrontier, APlan, AReport) then
      begin
        FStatus := wmasFailed;
        FFailure := 'phrase search failed (negotiation status ' + IntToStr(Ord(AReport.Status)) + ')';
        Exit(wmaspFailed);
      end;
      Last := APlan.BarAt(APlan.BarCount - 1);
      FFrontier.HasPrevious := True;
      FFrontier.NextBar := Last.Index + 1;
      FFrontier.LastHarmonyIndex := Last.HarmonyIndex;
      FFrontier.LastRealizationIndex := Last.RealizationIndex;
      if FFrontier.NextBar = WfcMusicFormTotalBars(FConfig) then FStatus := wmasCompleted
      else FStatus := wmasActive;
      Result := wmaspProduced;
    except
      on E: Exception do
      begin
        APlan.Free; APlan := nil;
        FStatus := wmasFailed; FFailure := E.Message;
        raise;
      end;
    end;
  finally FRunning := False; end;
end;

function TWfcMusicFormCursor.CopyFrontier: TWfcMusicFormFrontier;
begin Result := FFrontier; end;

procedure TWfcMusicFormCursor.Cancel;
begin
  if FRunning then FormError('cannot cancel during synchronous Next');
  if FStatus in [wmasReady, wmasActive] then FStatus := wmasCancelled;
end;

function WfcMusicFormRoleName(const AValue: TWfcMusicFormRole): String;
begin
  case AValue of
    wmfrQuestion: Result := 'question'; wmfrAnswer: Result := 'answer';
    wmfrContrast: Result := 'contrast'; wmfrReturn: Result := 'return';
  else FormError('unknown role'); Result := ''; end;
end;

function WfcMusicFormFunctionName(const AValue: TWfcMusicFormFunction): String;
begin
  case AValue of
    wmffTonic: Result := 'tonic'; wmffExpansion: Result := 'expansion';
    wmffPredominant: Result := 'predominant'; wmffDominant: Result := 'dominant';
  else FormError('unknown harmonic function'); Result := ''; end;
end;

function WfcMusicFormCadenceName(const AValue: TWfcMusicFormCadence): String;
begin
  case AValue of
    wmfcNone: Result := 'none'; wmfcHalf: Result := 'half';
    wmfcAuthentic: Result := 'authentic';
  else FormError('unknown cadence'); Result := ''; end;
end;

end.
