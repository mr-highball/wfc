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
program wfc_music_form_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_music_arrangement, wfc_music_form;

type TTestProcedure = procedure;
var Checks, Failures: Integer;

procedure Check(const ACondition: Boolean; const ALabel: String);
begin
  Inc(Checks);
  if ACondition then Exit;
  Inc(Failures); WriteLn('[FAIL] ', ALabel);
end;

procedure RunTest(const ALabel: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', ALabel);
  try ATest;
  except on E: Exception do
    begin Inc(Failures); WriteLn('[EXCEPTION] ', E.ClassName, ': ', E.Message); end; end;
end;

{ Independent abstract catalog: pitch coordinates are deliberately not MIDI
  notes from the Ensemble example. Realizations declare comparable anchors;
  concrete sounding-frame validation belongs to the acoustic adapter tests. }
function Catalog(const ACells: TWfcMusicArrangementWide;
  const ASeed: TGraphSeed = 4): TWfcMusicFormConfig;
const
  FUNCTIONS: array[0..4] of TWfcMusicFormFunction =
    (wmffTonic, wmffExpansion, wmffPredominant, wmffPredominant, wmffDominant);
  SLOTS: array[0..4, 0..2] of Integer =
    ((10,14,17), (10,14,19), (10,15,19), (12,15,19), (9,12,17));
  GESTURES: array[0..3] of Integer = (0,1,4,5);
var I, J, H, G: Integer;
begin
  Result := DefaultWfcMusicFormConfig(ACells, ASeed);
  SetLength(Result.Harmonies, 5);
  for I := 0 to 4 do
  begin
    Result.Harmonies[I].LabelText := 'harmony-' + IntToStr(I);
    Result.Harmonies[I].HarmonicFunction := FUNCTIONS[I];
    SetLength(Result.Harmonies[I].MotionPitches, 3);
    for J := 0 to 2 do Result.Harmonies[I].MotionPitches[J] := SLOTS[I,J];
  end;
  SetLength(Result.Gestures, 8);
  for I := 0 to 7 do
  begin
    Result.Gestures[I].LabelText := 'gesture-' + IntToStr(I);
    Result.Gestures[I].MotifIndex := I div 4;
    if I < 4 then Result.Gestures[I].Roles := [wmfrQuestion,wmfrAnswer,wmfrReturn]
    else Result.Gestures[I].Roles := [wmfrContrast];
    case I mod 4 of
      2: Result.Gestures[I].Cadences := [wmfcHalf];
      3: Result.Gestures[I].Cadences := [wmfcAuthentic];
    else Result.Gestures[I].Cadences := [wmfcNone]; end;
    if I mod 4 >= 2 then Result.Gestures[I].AttackCount := 2
    else if I >= 4 then Result.Gestures[I].AttackCount := 6
    else Result.Gestures[I].AttackCount := 4;
  end;
  SetLength(Result.Realizations, 24);
  for I := 0 to 23 do
  begin
    if I < 20 then begin H := I div 4; G := GESTURES[I mod 4]; end
    else case I of
      20: begin H := 4; G := 2; end;
      21: begin H := 0; G := 3; end;
      22: begin H := 4; G := 6; end;
    else begin H := 0; G := 7; end; end;
    Result.Realizations[I].HarmonyIndex := H;
    Result.Realizations[I].GestureIndex := G;
    Result.Realizations[I].EntryPitch := 30 + H + G mod 2;
    Result.Realizations[I].ExitPitch := 30 + H + (G + 1) mod 2;
  end;
end;

function SameFrontier(const A, B: TWfcMusicFormFrontier): Boolean;
begin
  Result := (A.HasPrevious = B.HasPrevious) and (A.NextBar = B.NextBar) and
    (A.LastHarmonyIndex = B.LastHarmonyIndex) and
    (A.LastRealizationIndex = B.LastRealizationIndex);
end;

function Rejected(const C: TWfcMusicFormConfig): Boolean;
begin
  Result := False;
  try ValidateWfcMusicFormConfig(C);
  except on E: EWfcMusicForm do Result := True; end;
end;

procedure TestPeriodAndReplay;
const FNS: array[0..15] of TWfcMusicFormFunction =
  (wmffTonic,wmffExpansion,wmffPredominant,wmffDominant,
   wmffTonic,wmffPredominant,wmffDominant,wmffTonic,
   wmffExpansion,wmffPredominant,wmffPredominant,wmffDominant,
   wmffTonic,wmffPredominant,wmffDominant,wmffTonic);
var
  C: TWfcMusicFormConfig; F: TWfcMusicFormFrontier;
  Cursor: TWfcMusicFormCursor; P, Q: TWfcMusicFormPhrasePlan;
  R, S: TGraphNegotiationReport; B: TWfcMusicFormBar;
  Why: String; I, J: Integer; H: Cardinal;
begin
  C := Catalog(128); C.Search.SolveOptions.CaptureTrace := True;
  Cursor := TWfcMusicFormCursor.Create(C); H := 0;
  try
    Check(Cursor.Status = wmasReady, 'cursor begins ready');
    for I := 0 to 3 do
    begin
      F := Cursor.CopyFrontier; P := nil; Q := nil;
      Check(Cursor.Next(P, R) = wmaspProduced, 'four phrases produced');
      try
        Check(P.BarCount = 4, 'bounded four-bar phrase');
        Check(P.PhraseIndex = I, 'absolute phrase index');
        Check(P.Seed = WfcMusicArrangementSectionSeed(C.Seed, I), 'absolute phrase seed');
        Check(ValidateWfcMusicFormPhrase(C, F, P, Why), 'independent plan audit: ' + Why);
        Check(Length(R.FinalReport.Passes) = 3, 'form, harmonic intent, gesture passes');
        for J := 0 to 2 do
          Check(R.FinalReport.Passes[J].Executed, 'each genuine constraint pass executed');
        Check(Length(R.FinalReport.Trace) > 0, 'form pipeline trace retained');
        for J := 0 to 3 do
        begin
          B := P.BarAt(J);
          Check(B.Role = TWfcMusicFormRole(I), 'question/answer/contrast/return roles');
          Check(B.HarmonicFunction = FNS[I * 4 + J], 'authored functional movement');
          Check(B.StartCell = (I * 4 + J) * 8, 'absolute cell provenance');
          Check(B.CellCount = 8, 'full bar extent');
          if I = 2 then
            Check(B.MotifIndex = C.ContrastMotifIndex, 'contrast realizes different motif family')
          else Check(B.MotifIndex = C.ThemeMotifIndex, 'return recalls actual theme family');
          if J < 3 then Check(B.Cadence = wmfcNone, 'interior is not a cadence')
          else if I in [0,2] then Check(B.Cadence = wmfcHalf, 'open phrase ends half cadence')
          else Check(B.Cadence = wmfcAuthentic, 'closing phrase ends authentic cadence');
          if (I = 2) and (J < 3) then
            Check(C.Gestures[B.GestureIndex].AttackCount >= C.ContrastMinimumAttacks,
              'contrast activity quota actually realized');
        end;
        Check(TryPlanWfcMusicFormPhrase(C, F, Q, S), 'detached deterministic replay solves');
        Check(P.Signature = Q.Signature, 'replay signature identical');
        Check(R.TranscriptHash = S.TranscriptHash, 'replay solver transcript identical');
        H := ((H shl 5) or (H shr 27)) xor P.Signature;
      finally Q.Free; P.Free; end;
    end;
    Check(Cursor.Status = wmasCompleted, 'last produced phrase completes cursor');
    F := Cursor.CopyFrontier;
    Check(F.NextBar = 16, 'exact final frontier');
    Check(Cursor.Next(P, R) = wmaspCompleted, 'completed cursor yields no more');
    Check(P = nil, 'completed result nil');
    Check(H = Cardinal($91B5B2DD), 'version-1 period signature golden');
    WriteLn('MUSIC_FORM_SIGNATURE=', IntToHex(H, 8));
  finally Cursor.Free; end;
end;

procedure TestPartialAndPhraseSizes;
var
  C: TWfcMusicFormConfig; Cursor: TWfcMusicFormCursor;
  P: TWfcMusicFormPhrasePlan; R: TGraphNegotiationReport;
  B, Prior: TWfcMusicFormBar; F: TWfcMusicFormFrontier;
  Cells, Size, Count, Phrases, I: Integer; Why: String;
begin
  for Size := 1 to 6 do
    for Cells := 1 to 49 do
    begin
      C := Catalog(Cells); C.PhraseBars := Size;
      Cursor := TWfcMusicFormCursor.Create(C);
      Count := 0; Phrases := 0;
      try
        repeat
          F := Cursor.CopyFrontier;
          Check(Cursor.Next(P, R) = wmaspProduced, 'short arbitrary extent solves');
          if P = nil then Break;
          try
            Inc(Phrases);
            Check(P.BarCount <= Size, 'bounded caller-selected phrase size');
            Check(ValidateWfcMusicFormPhrase(C, F, P, Why), 'short phrase audit');
            for I := 0 to P.BarCount - 1 do
            begin
              B := P.BarAt(I); Inc(Count, B.CellCount);
              Check(B.StartCell + B.CellCount <= Cells, 'never pads beyond requested extent');
            end;
            if Cursor.Status = wmasCompleted then
            begin
              Check(B.Cadence = wmfcAuthentic, 'every finite extent closes');
              Check(B.HarmonicFunction = wmffTonic, 'final bar resolves to tonic');
              if P.BarCount > 1 then
              begin
                Prior := P.BarAt(P.BarCount - 2);
                Check(Prior.HarmonicFunction = wmffDominant,
                  'short closing phrase preserves dominant-to-tonic cadence');
              end;
            end;
          finally P.Free; end;
        until Cursor.Status in [wmasCompleted,wmasFailed];
        Check(Count = Cells, 'exact user cell extent, including partial final bar');
        Check(Phrases = (Integer(WfcMusicFormTotalBars(C)) - 1) div Size + 1,
          'number of bounded phrase allocations');
      finally Cursor.Free; end;
    end;
end;

procedure TestOwnershipAndAudit;
var
  C, D: TWfcMusicFormConfig; Cursor: TWfcMusicFormCursor;
  F: TWfcMusicFormFrontier; P, Q: TWfcMusicFormPhrasePlan;
  R: TGraphNegotiationReport; Bars: TWfcMusicFormBars; B: TWfcMusicFormBar;
  Why: String; I: Integer; Raised: Boolean;
begin
  C := Catalog(128); D := CopyWfcMusicFormConfig(C);
  Cursor := TWfcMusicFormCursor.Create(C); P := nil; Q := nil;
  try
    C.Harmonies[0].MotionPitches[0] := 999;
    C.Harmonies[0].LabelText := 'changed';
    C.Gestures[0].LabelText := 'changed';
    C.Realizations[0].EntryPitch := 999;
    Check(D.Harmonies[0].MotionPitches[0] = 10, 'config nested pitches detached');
    F := Cursor.CopyFrontier;
    Check(Cursor.Next(P, R) = wmaspProduced, 'cursor owns catalog independent of caller edits');
    Check(TryPlanWfcMusicFormPhrase(D, F, Q, R), 'original catalog replay');
    Check(P.Signature = Q.Signature, 'all cursor-owned catalog arrays detached');
    Check(not ValidateWfcMusicFormPhrase(C, F, P, Why), 'changed config rejected');
    Bars := P.CopyBars; B := P.BarAt(0); Bars[0].Index := 111;
    B := P.BarAt(0);
    Check(B.Index = 0, 'CopyBars cannot mutate immutable plan');
    for I := 0 to 12 do
    begin
      Bars := P.CopyBars;
      case I of
        0: Inc(Bars[0].Index); 1: Inc(Bars[0].StartCell);
        2: Inc(Bars[0].PhraseIndex); 3: Inc(Bars[0].PhrasePosition);
        4: Dec(Bars[0].CellCount); 5: Bars[0].Role := wmfrReturn;
        6: Bars[0].HarmonicFunction := wmffDominant;
        7: Bars[0].Cadence := wmfcAuthentic; 8: Inc(Bars[0].MotifIndex);
        9: Bars[0].HarmonyIndex := -1; 10: Bars[0].GestureIndex := -1;
        11: Bars[0].RealizationIndex := Length(D.Realizations);
        12: SetLength(Bars, 0);
      end;
      Check(not ValidateWfcMusicFormBars(D, F, Bars, Why), 'independent corrupted bar rejection');
      Check(Why <> '', 'corrupt bar diagnostic');
    end;
    Check(not ValidateWfcMusicFormPhrase(D, F, nil, Why), 'nil plan rejected');
    Raised := False;
    try B := P.BarAt(-1); except on E: EWfcMusicForm do Raised := True; end;
    Check(Raised, 'negative immutable bar index rejected');
    Raised := False;
    try B := P.BarAt(P.BarCount); except on E: EWfcMusicForm do Raised := True; end;
    Check(Raised, 'high immutable bar index rejected');
    D.Search.SolveOptions.CaptureTrace := not D.Search.SolveOptions.CaptureTrace;
    Check(ValidateWfcMusicFormPhrase(D, F, P, Why), 'capture flag not musical signature identity');
  finally Q.Free; P.Free; Cursor.Free; end;
end;

procedure TestValidation;
var C: TWfcMusicFormConfig; I: Integer;
begin
  for I := 0 to 23 do
  begin
    C := Catalog(128);
    case I of
      0: C.TotalCells := 0; 1: C.TotalCells := -1;
      2: C.TotalCells := WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER + 1;
      3: C.CellsPerBar := 0; 4: C.PhraseBars := 0;
      5: C.ThemeMotifIndex := C.ContrastMotifIndex;
      6: C.MaxChordMotion := -1; 7: C.MaxMelodyMotion := -1;
      8: C.ContrastMinimumAttacks := -1;
      9: C.Harmonies := nil; 10: C.Gestures := nil; 11: C.Realizations := nil;
      12: C.Harmonies[0].MotionPitches := nil;
      13: C.Harmonies[0].MotionPitches[1] := 0;
      14: C.Harmonies[0].LabelText := '';
      15: C.Harmonies[0].LabelText := #10;
      16: C.Harmonies[1].LabelText := C.Harmonies[0].LabelText;
      17: C.Gestures[1].LabelText := C.Gestures[0].LabelText;
      18: C.Gestures[0].Roles := []; 19: C.Gestures[0].Cadences := [];
      20: C.Gestures[0].AttackCount := 9;
      21: C.Realizations[0].HarmonyIndex := 99;
      22: C.Realizations[0].GestureIndex := -1;
      23: C.Search.MaxPassBacktracks := -1;
    end;
    Check(Rejected(C), 'malformed catalog or policy rejected');
  end;
end;

procedure TestFailuresAndFrontier;
var
  C: TWfcMusicFormConfig; Cursor: TWfcMusicFormCursor;
  P: TWfcMusicFormPhrasePlan; R: TGraphNegotiationReport;
  F, Original: TWfcMusicFormFrontier; I: Integer; Raised: Boolean;
begin
  C := Catalog(64);
  { Question remains satisfiable; the next answer cannot close. }
  C.Gestures[3].Cadences := [wmfcNone];
  Cursor := TWfcMusicFormCursor.Create(C);
  try
    Check(Cursor.Next(P, R) = wmaspProduced, 'prefix remains satisfiable'); P.Free;
    Original := Cursor.CopyFrontier;
    Check(Cursor.Next(P, R) = wmaspFailed, 'missing closing realization reports failure');
    Check(P = nil, 'failed candidate never published');
    Check(SameFrontier(Original, Cursor.CopyFrontier), 'failed phrase preserves committed boundary');
    Check(Cursor.Failure <> '', 'failed cursor diagnostic retained');
    Check(Cursor.Next(P, R) = wmaspFailed, 'failure is terminal, no silent reroll');
  finally Cursor.Free; end;
  C := Catalog(128); C.MaxChordMotion := 0;
  Check(not TryPlanWfcMusicFormPhrase(C, InitialWfcMusicFormFrontier, P, R),
    'hard zero chord-slot motion makes moving function sequence unsatisfiable');
  Check(P = nil, 'unsatisfiable motion leaves no plan');
  C := Catalog(128); C.MaxMelodyMotion := 0;
  for I := 0 to High(C.Realizations) do
  begin C.Realizations[I].EntryPitch := 0; C.Realizations[I].ExitPitch := 1; end;
  Check(not TryPlanWfcMusicFormPhrase(C, InitialWfcMusicFormFrontier, P, R),
    'hard melody motion enforced between bars');
  C := Catalog(128);
  for I := 0 to 6 do
  begin
    F := InitialWfcMusicFormFrontier;
    case I of
      0: F.NextBar := -1; 1: F.NextBar := 17;
      2: F.HasPrevious := True; 3: F.LastHarmonyIndex := 0;
      4: F.NextBar := 4;
      5,6: begin
        F.NextBar := 4; F.HasPrevious := True;
        F.LastHarmonyIndex := 0; F.LastRealizationIndex := 0;
        if I = 5 then F.NextBar := 3 else F.LastHarmonyIndex := 1;
      end;
    end;
    Raised := False;
    try TryPlanWfcMusicFormPhrase(C, F, P, R);
    except on E: EWfcMusicForm do Raised := True; end;
    Check(Raised, 'invalid boundary rejected before solving');
    Check(P = nil, 'invalid boundary no candidate');
  end;
  F := InitialWfcMusicFormFrontier;
  F.HasPrevious := True; F.NextBar := 4;
  F.LastHarmonyIndex := 0; F.LastRealizationIndex := 0;
  C.Realizations[0].ExitPitch := 1000;
  Check(not TryPlanWfcMusicFormPhrase(C, F, P, R), 'prior phrase upper anchor is a hard constraint');
end;

procedure TestHugeAndCancel;
var
  C: TWfcMusicFormConfig; Cursor: TWfcMusicFormCursor;
  F: TWfcMusicFormFrontier; P: TWfcMusicFormPhrasePlan;
  R: TGraphNegotiationReport; B: TWfcMusicFormBar;
  Why: String; Total, Start: TWfcMusicArrangementWide;
begin
  C := Catalog(WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER);
  Cursor := TWfcMusicFormCursor.Create(C);
  try
    Check(Cursor.Next(P, R) = wmaspProduced, 'near-2^53 extent allocates only first phrase');
    Check(P.BarCount = 4, 'huge duration still four bars resident'); P.Free;
    F := Cursor.CopyFrontier; Cursor.Cancel;
    Check(Cursor.Next(P, R) = wmaspCancelled, 'cancel between phrases');
    Check(P = nil, 'cancel no candidate');
    Check(SameFrontier(F, Cursor.CopyFrontier), 'cancel retains last valid boundary');
  finally Cursor.Free; end;
  Cursor := TWfcMusicFormCursor.Create(C);
  try Cursor.Cancel; Check(Cursor.Next(P, R) = wmaspCancelled, 'cancel before first solve');
  finally Cursor.Free; end;
  Total := WfcMusicFormTotalBars(C);
  Start := ((Total - 1) div C.PhraseBars) * C.PhraseBars;
  F := InitialWfcMusicFormFrontier;
  F.HasPrevious := True; F.NextBar := Start;
  F.LastHarmonyIndex := 0; F.LastRealizationIndex := 0;
  Check(TryPlanWfcMusicFormPhrase(C, F, P, R), 'near-limit last phrase independently materialized');
  try
    Check(ValidateWfcMusicFormPhrase(C, F, P, Why), 'huge final phrase independently audited');
    B := P.BarAt(P.BarCount - 1);
    Check(B.StartCell > High(Integer), 'wide positions never narrow to Int32');
    Check(B.StartCell + B.CellCount = C.TotalCells, 'near-limit partial final cell exact');
    Check(B.CellCount = 7, '2^53-1 retains its seventh final cell');
    Check(B.Cadence = wmfcAuthentic, 'huge finite extent still closes');
    Check(P.PhraseIndex = Start div C.PhraseBars, 'wide absolute phrase identity');
    Check(P.Signature = Cardinal($422A7BF6), 'version-1 wide signature golden');
    WriteLn('MUSIC_FORM_WIDE_SIGNATURE=', IntToHex(P.Signature, 8));
  finally P.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestBrowserNumbers;
var C: TWfcMusicFormConfig; X: TWfcMusicArrangementWide; I, J: Integer;
  F: TWfcMusicFormFrontier; P: TWfcMusicFormPhrasePlan; R: TGraphNegotiationReport;
  Raised: Boolean; Bars: TWfcMusicFormBars; Why: String;
begin
  for I := 0 to 8 do
  begin
    case I of
      0: asm X = NaN; end; 1: asm X = Infinity; end;
      2: asm X = -Infinity; end; 3: asm X = undefined; end;
      4: asm X = null; end; 5: asm X = "4"; end;
      6: asm X = 4.5; end; 7: asm X = true; end;
      8: asm X = 9007199254740992; end;
    end;
    for J := 0 to 7 do
    begin
      C := Catalog(128);
      case J of
        0: C.TotalCells := X; 1: C.CellsPerBar := X;
        2: C.PhraseBars := X; 3: C.Seed := X;
        4: C.MaxMelodyMotion := X; 5: C.Realizations[0].EntryPitch := X;
        6: C.Harmonies[0].MotionPitches[0] := X; 7: C.Gestures[0].AttackCount := X;
      end;
      Check(Rejected(C), 'malformed JS exact-number field rejected');
    end;
    C := Catalog(128); F := InitialWfcMusicFormFrontier; F.NextBar := X;
    Raised := False;
    try TryPlanWfcMusicFormPhrase(C, F, P, R);
    except on E: EWfcMusicForm do Raised := True; end;
    Check(Raised, 'malformed JS boundary position rejected');
    F := InitialWfcMusicFormFrontier;
    Check(TryPlanWfcMusicFormPhrase(C, F, P, R), 'valid browser baseline');
    try
      Bars := P.CopyBars; Bars[0].HarmonyIndex := X;
      Check(not ValidateWfcMusicFormBars(C, F, Bars, Why), 'malformed JS realized index rejected');
    finally P.Free; end;
  end;
end;
{$ENDIF}

begin
  RunTest('developed period, genuine passes, motifs and portable replay', @TestPeriodAndReplay);
  RunTest('arbitrary exact extent and caller-selected phrase sizes', @TestPartialAndPhraseSizes);
  RunTest('immutable ownership and independent candidate audit', @TestOwnershipAndAudit);
  RunTest('catalog and policy validation', @TestValidation);
  RunTest('hard motion, cadence failures and transactional frontier', @TestFailuresAndFrontier);
  RunTest('wide bounded iteration and cancellation', @TestHugeAndCancel);
  {$IFDEF PAS2JS}RunTest('strict browser numeric contract', @TestBrowserNumbers);{$ENDIF}
  WriteLn('[SUMMARY] checks=', Checks, ' failures=', Failures);
  if Failures <> 0 then Halt(1);
end.
