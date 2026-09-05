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
program wfc_music_studio_test;
{$mode delphi}{$H+}
uses
  SysUtils, wfc, wfc_model, wfc_music, wfc_music_sequence, wfc_music_passes,
  wfc_music_passes_text, wfc_music_text, wfc_midi_smf, music_studio_workbench;

var Checks, Failures: Integer;
procedure Check(const B: Boolean; const S: String);
begin
  Inc(Checks);
  if not B then begin Inc(Failures); WriteLn('[FAIL] ', S); end;
end;
function SameTokens(const A,B: TWfcModelTokens): Boolean;
var I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do if A[I] <> B[I] then Exit(False);
  Result := True;
end;
function SameBytes(const A,B: TWfcMidiBytes): Boolean;
var I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do if A[I] <> B[I] then Exit(False);
  Result := True;
end;
function SameIndices(const A: TGraphPassIndices; const B: array of Integer): Boolean;
var I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do if A[I] <> B[I] then Exit(False);
  Result := True;
end;

procedure TestWorkbench;
var
  W: TWfcMusicStudio;
  O: TWfcMusicStudioOptions;
  R, R2: TWfcMusicStudioReport;
  C: TWfcMusicComposition;
  S: TWfcMusicScore;
  M: TWfcMidiFile;
  Baseline, Tokens, Rhythm, Harmony: TWfcModelTokens;
  Locks: TWfcMusicStudioLocks;
  Text, Signature: String;
  Bytes: TWfcMidiBytes;
  Raised: Boolean;
begin
  W := TWfcMusicStudio.Create(0);
  try
    Check((W.Status=mssIdle) and not W.HasCurrent and not W.HasBaseline,
      'new workspace has no baseline or current output');
    O := DefaultMusicStudioOptions;
    Check(W.Run(msaGenerate,O), 'seed zero generates');
    Check(W.HasCurrent and W.HasBaseline, 'successful publication establishes both views');
    Check(W.SignatureText='216F6EBB', 'seed-zero composition identity');
    R := W.CopyReport;
    Check((R.MusicStatus=wmpsCompleted) and R.Validation.Valid and
      (R.ValidationPosition=-1), 'successful domain report retained');
    Signature := W.SignatureText;
    Baseline := W.CellTokens(wmplMelody);
    Rhythm := W.CellTokens(wmplRhythm);
    Harmony := W.CellTokens(wmplHarmony);
    Check(Length(Baseline)=16, 'sixteen public quanta');
    C := DecodeWfcMusicPassesText(W.CompositionText);
    try
      Check(MusicStudioCompositionIsValid(C), 'independent public score and relation validation');
      Check(C.Seed=W.Seed, 'published composition belongs to session seed');
      Check(EncodeWfcMusicPassesText(C)=W.CompositionText, 'canonical composition round trip');
    finally C.Free; end;
    S := DecodeWfcMusicText(W.ScoreText);
    try
      Check((S.LengthTicks=3840) and (S.TempoAt(0).MicrosecondsPerQuarter=500000),
        'exact four-second two-bar score');
      Check(EncodeWfcMusicText(S)=W.ScoreText, 'canonical score round trip');
    finally S.Free; end;
    Bytes := W.MidiBytes;
    M := DecodeWfcMidiFile(Bytes);
    Check(SameBytes(EncodeWfcMidiFile(M),Bytes), 'canonical SMF round trip');
    Tokens := W.CellTokens(wmplMelody); Tokens[0] := 'changed';
    Check(W.CellTokens(wmplMelody)[0]=Baseline[0], 'public output array detached');
    Tokens := W.PublicTokens(wmplMelody); Tokens[0] := 'changed';
    Check(W.PublicTokens(wmplMelody)[0]<>'changed', 'vocabulary detached');
    R := W.CopyReport; R.Passes[0].Decisions := -99;
    R.ActivePassIndices[0] := 99; R2 := W.CopyReport;
    Check((R2.Passes[0].Decisions<>-99) and (R2.ActivePassIndices[0]=0),
      'report arrays detached');
    Check(W.Run(msaGenerate,O) and (W.SignatureText=Signature), 'full replay rewinds streams');
    W.LockOpeningMotif(2);
    Check(not W.HasCurrent and W.HasBaseline, 'motif edit hides current and preserves baseline');
    Locks := W.CopyLocks;
    Check((Length(Locks)=2) and (Locks[0].Token=Baseline[0]) and
      (Locks[1].Token=Baseline[1]), 'exact opening motif recorded');
    Locks[0].Token := 'changed';
    Check(W.CopyLocks[0].Token=Baseline[0], 'lock records detached');
    W.SetLock(wmplMelody,2,'wm1:a:67:96');
    O.Negotiated := False; O.MaxPassBacktracks := 0;
    Check(not W.Run(msaHarmony,O), 'one-way variation reaches contradiction');
    Check((W.Status=mssContradiction) and not W.HasCurrent and W.HasBaseline,
      'contradiction leaves only hidden baseline');
    Check((Length(W.CellTokens(wmplMelody))=0) and (Length(W.MelodyCells)=0),
      'failed attempt exposes no stale cells');
    Raised := False;
    try Text := W.CompositionText; except on EMusicStudio do Raised := True; end;
    Check(Raised, 'failed attempt cannot export prior composition');
    R := W.CopyReport;
    Check((R.MusicStatus=wmpsSolveFailed) and
      (R.ValidationKind=wmpvikNone) and (R.ValidationPosition=-1),
      'solve failure does not invent a domain validation cell');
    Check(SameIndices(R.RequestedRootIndices,[0]) and SameIndices(R.ActivePassIndices,[0,2]),
      'ordinary attempt exposes intended scope even on failure');
    O := DefaultMusicStudioOptions;
    Check(W.Run(msaHarmony,O), 'bounded selective negotiation repairs variation');
    Check(W.SignatureText='1C1075DB', 'repaired composition identity');
    R := W.CopyReport;
    Check((R.Rounds=2) and (R.PassBacktracks=1) and
      (R.TranscriptHash=Cardinal($4A9D9975)), 'bounded repair transcript identity');
    Check(SameIndices(R.RequestedRootIndices,[0]) and SameIndices(R.ActivePassIndices,[0,2]),
      'negotiation scope matches ordinary scope');
    Check(not R.Passes[1].Executed and SameTokens(W.CellTokens(wmplRhythm),Rhythm),
      'rhythm is reused unchanged outside repair scope');
    Tokens := W.CellTokens(wmplMelody);
    Check((Tokens[0]=Baseline[0]) and (Tokens[1]=Baseline[1]) and
      (Tokens[2]='wm1:a:67:96'), 'motif retained while requested cell changes');
    W.ClearLocks;
    Check(W.Run(msaGenerate,O) and (W.SignatureText=Signature), 'clear and recover exact baseline');
    Raised := False;
    try W.SetLock(wmplMelody,0,'unknown'); except on EMusicStudio do Raised := True; end;
    Check(Raised and W.HasCurrent and (Length(W.CopyLocks)=0),
      'unknown lock rejected before mutation');
    W.SetLock(wmplRhythm,0,Rhythm[0]);
    O.Negotiated := False; O.MaxPassBacktracks := 0;
    Check(W.Run(msaMelody,O), 'dirty rhythm is not ignored by melody-only request');
    R := W.CopyReport;
    Check(SameIndices(R.RequestedRootIndices,[1,2]) and
      SameIndices(R.ActivePassIndices,[1,2]), 'dirty-provider scope is explicit');
    Check(not R.Passes[0].Executed and SameTokens(W.CellTokens(wmplHarmony),Harmony),
      'unrelated harmony reused on expanded dirty scope');
    W.ClearLocks;
    O := DefaultMusicStudioOptions;
    Check(W.Run(msaGenerate,O), 'restore before impossible form edit');
    W.SetLock(wmplRhythm,0,'wr1:r');
    O.Negotiated := False; O.MaxPassBacktracks := 0;
    Check(not W.Run(msaGenerate,O) and (W.Status=mssContradiction),
      'rhythm form contradiction is not silently relaxed');
    W.ClearLock(wmplRhythm,0);
    O := DefaultMusicStudioOptions;
    Check(W.Run(msaGenerate,O) and (W.SignatureText=Signature),
      'remove impossible lock and restore baseline');
    W.SetLock(wmplMelody,2,'wm1:a:67:96');
    W.SetLock(wmplMelody,2,Baseline[2]);
    Check((Length(W.CopyLocks)=1) and W.Run(msaGenerate,O) and
      (W.SignatureText=Signature), 'replacing a token removes the previous intersection');
    W.InvalidateCurrent;
    Check(not W.HasCurrent and W.HasBaseline, 'pending option edit hides current output');
    O.MaxBacktracks := 1025;
    Raised := False;
    try W.Run(msaGenerate,O); except on EMusicStudio do Raised := True; end;
    Check(Raised and not W.HasCurrent, 'over-budget request fails before solving');
    O := DefaultMusicStudioOptions; O.Negotiated := False;
    Raised := False;
    try W.Run(msaGenerate,O); except on EMusicStudio do Raised := True; end;
    Check(Raised, 'one-way pass budget must be zero');
    W.Reset(0);
    Check(not W.HasCurrent and not W.HasBaseline and (Length(W.CopyLocks)=0),
      'reset clears baseline, locks, and current output');
    O := DefaultMusicStudioOptions;
    Raised := False;
    try W.Run(msaHarmony,O); except on EMusicStudio do Raised := True; end;
    Check(Raised and not W.HasCurrent, 'selective operation requires baseline');
    Check(W.Run(msaGenerate,O) and (W.SignatureText=Signature), 'reset replays exact fixture');
    Check(MusicStudioPitchName(60)='C4', 'pitch label convention');
    Check(MusicStudioTokenLabel(wmplRhythm,'wr1:h')='hold', 'rhythm label');
    Check(not MusicStudioCompositionIsValid(nil), 'independent validator rejects absent output');
  finally W.Free; end;
end;

procedure TestSeedMatrix;
const SOLVES: set of Byte = [0,4,6,7,13,14,15];
var W: TWfcMusicStudio; O: TWfcMusicStudioOptions; I: Integer; B: Boolean;
    C: TWfcMusicComposition;
begin
  W := TWfcMusicStudio.Create(0);
  try
    O := DefaultMusicStudioOptions;
    for I := 0 to 15 do
    begin
      W.Reset(I); B := W.Run(msaGenerate,O);
      Check(B=(I in SOLVES), 'bounded seed matrix status '+IntToStr(I));
      if B then
      begin
        C := DecodeWfcMusicPassesText(W.CompositionText);
        try Check(MusicStudioCompositionIsValid(C), 'seed semantic validation '+IntToStr(I));
        finally C.Free; end;
      end
      else Check((W.Status=mssPassLimit) and not W.HasCurrent and
        (W.CopyReport.PassBacktracks=16), 'seed limit has no invented result '+IntToStr(I));
    end;
  finally W.Free; end;
end;

begin
  Checks := 0; Failures := 0;
  TestWorkbench; TestSeedMatrix;
  WriteLn('Checks: ',Checks,', Failures: ',Failures);
  if Failures <> 0 then raise Exception.Create('Music Studio checks failed');
end.
