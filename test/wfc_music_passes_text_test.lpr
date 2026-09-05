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
program wfc_music_passes_text_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_model,
  wfc_text_codec,
  wfc_music,
  wfc_music_text,
  wfc_music_sequence,
  wfc_music_passes,
  wfc_music_passes_text,
  wfc_sequence_graph;

type
  TTestProcedure = procedure;

const
  FIXTURE_SEED = TGraphSeed($F1234567);
  FIXTURE_QUANTUM = 120;
  FIXTURE_CELL_COUNT = 4;
  FIXTURE_SIGNATURE = 'C8541A90';
  FIXTURE_SCORE_TEXT =
    'wfcmusic=1'#10 +
    'tpq=120'#10 +
    'steps=12'#10 +
    'length=480'#10 +
    'tracks=1'#10 +
    'track=0,lead,Lead'#10 +
    'voices=1'#10 +
    'voice=0,0,voice'#10 +
    'meters=1'#10 +
    'meter=0,0,4,4'#10 +
    'tempos=1'#10 +
    'tempo=0,0,500000'#10 +
    'spans=3'#10 +
    'span=0,0,0,240,N,60@90'#10 +
    'span=1,0,240,120,R,'#10 +
    'span=2,0,360,120,N,67@100'#10 +
    'end'#10;

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GCheckCount);
  if ACondition then
    WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailureCount);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try
    ATest;
  except
    on E: Exception do
    begin
      Inc(GFailureCount);
      WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function MelodyCellsOf(const AValues: array of TWfcMusicMelodyCell):
  TWfcMusicMelodyCells;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function TokensMatch(const A, B: TWfcModelTokens): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to Length(A) - 1 do
    if A[I] <> B[I] then
      Exit(False);
  Result := True;
end;

function BuildFixtureScore(const AMelody: TWfcMusicMelodyCells):
  TWfcMusicScore;
var
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  SetLength(LTracks, 1);
  LTracks[0] := MakeWfcMusicTrack('lead', 'Lead');
  SetLength(LVoices, 1);
  LVoices[0] := MakeWfcMusicVoice(0, 'voice');
  SetLength(LMeters, 1);
  LMeters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, 500000);
  LSpans := RebuildWfcMusicVoiceSpans(AMelody, 0, FIXTURE_QUANTUM);
  Result := TWfcMusicScore.Create(120, 12,
    Length(AMelody) * FIXTURE_QUANTUM, LTracks, LVoices,
    LMeters, LTempos, LSpans);
end;

function BuildFixture: TWfcMusicComposition;
var
  LHarmony: TWfcMusicHarmonyCells;
  LMelody: TWfcMusicMelodyCells;
  LRhythm: TWfcMusicRhythmCells;
  LScore: TWfcMusicScore;
begin
  LMelody := MelodyCellsOf([
    MakeWfcMusicAttackCell(60, 90),
    MakeWfcMusicHoldCell(60, 90),
    MakeWfcMusicRestCell,
    MakeWfcMusicAttackCell(67, 100)
  ]);
  LRhythm := ProjectWfcMusicMelodyToRhythm(LMelody);
  LHarmony := ProjectWfcMusicMelodyToHarmony(LMelody, 12);
  LScore := BuildFixtureScore(LMelody);
  try
    Result := CreateWfcMusicComposition(FIXTURE_SEED,
      FIXTURE_QUANTUM, EncodeWfcMusicHarmonyCells(LHarmony),
      EncodeWfcMusicRhythmCells(LRhythm),
      EncodeWfcMusicMelodyCells(LMelody), LScore);
  finally
    LScore.Free;
  end;
end;

function ExpectedFixtureText: String;
begin
  Result :=
    'wfcmusicpass=1'#10 +
    'seed=4045620583'#10 +
    'quantum=120'#10 +
    'cells=4'#10 +
    'harmony=0,wh1%3Ap%3A12%3A0'#10 +
    'harmony=1,wh1%3Ap%3A12%3A0'#10 +
    'harmony=2,wh1%3Ar%3A12%3A0'#10 +
    'harmony=3,wh1%3Ap%3A12%3A7'#10 +
    'rhythm=0,wr1%3Aa'#10 +
    'rhythm=1,wr1%3Ah'#10 +
    'rhythm=2,wr1%3Ar'#10 +
    'rhythm=3,wr1%3Aa'#10 +
    'melody=0,wm1%3Aa%3A60%3A90'#10 +
    'melody=1,wm1%3Ah%3A60%3A90'#10 +
    'melody=2,wm1%3Ar'#10 +
    'melody=3,wm1%3Aa%3A67%3A100'#10 +
    'signature=' + FIXTURE_SIGNATURE + #10 +
    'score=' + WfcTextEncodeToken(TWfcModelToken(FIXTURE_SCORE_TEXT),
      'wfcmusicpass=1') + #10 +
    'end'#10;
end;

function ReplaceOnce(const AText, AOld, ANew: String): String;
var
  LPosition: Integer;
begin
  LPosition := Pos(AOld, AText);
  if LPosition = 0 then
    raise Exception.CreateFmt('test mutation source is absent: %s', [AOld]);
  Result := Copy(AText, 1, LPosition - 1) + ANew +
    Copy(AText, LPosition + Length(AOld), Length(AText));
end;

procedure ExpectRejected(const AText, ALabel: String);
var
  LComposition: TWfcMusicComposition;
  LRejected: Boolean;
begin
  LComposition := nil;
  LRejected := False;
  try
    try
      LComposition := DecodeWfcMusicPassesText(AText);
    except
      on E: Exception do
        LRejected := True;
    end;
  finally
    LComposition.Free;
  end;
  Check(LRejected, ALabel);
end;

procedure TestCanonicalRoundTrip;
var
  LDecoded: TWfcMusicComposition;
  LEncoded: String;
  LExpected: String;
  LFixture: TWfcMusicComposition;
  LScore: TWfcMusicScore;
begin
  LDecoded := nil;
  LFixture := BuildFixture;
  try
    LEncoded := EncodeWfcMusicPassesText(LFixture);
    LExpected := ExpectedFixtureText;
    Check(LEncodeD = LExpected,
      'wfcmusicpass=1 encoding matches the exact canonical document');
    Check((Pos(#13, LEncoded) = 0) and
      (LEncodeD[Length(LEncodeD)] = #10),
      'canonical composition text is LF-only and LF-terminated');
    Check(Pos('@wfcs', LEncoded) = 0,
      'canonical composition text contains no latent sequence key');
    Check(WfcMusicCompositionSignatureHex(LFixture.Signature) =
      FIXTURE_SIGNATURE,
      'composition signature is pinned across compiler targets');

    LDecoded := DecodeWfcMusicPassesText(LEncodeD);
    Check((LDecoded.Seed = FIXTURE_SEED) and
      (LDecoded.QuantumTicks = FIXTURE_QUANTUM) and
      (LDecoded.CellCount = FIXTURE_CELL_COUNT),
      'decode reconstructs the public composition configuration');
    Check(LDecoded.Signature = LFixture.Signature,
      'decode recomputes the exact composition signature');
    Check(not LDecoded.HasLatentCapture,
      'decoded public composition does not invent latent state indices');
    Check(EncodeWfcMusicPassesText(LDecoded) = LEncoded,
      'decoded composition re-encodes byte-for-byte');
    LScore := LDecoded.CopyScore;
    try
      Check(EncodeWfcMusicText(LScore) = FIXTURE_SCORE_TEXT,
        'nested canonical score semantics round-trip exactly');
    finally
      LScore.Free;
    end;
  finally
    LDecoded.Free;
    LFixture.Free;
  end;
end;

procedure TestPublicTokensAndOwnership;
var
  LDecoded: TWfcMusicComposition;
  LEncoded: String;
  LFixture: TWfcMusicComposition;
  LGenerated: TWfcGeneratedSequence;
  LScore: TWfcMusicScore;
  LTokens: TWfcModelTokens;
begin
  LFixture := BuildFixture;
  LEncoded := EncodeWfcMusicPassesText(LFixture);
  LDecoded := DecodeWfcMusicPassesText(LEncodeD);
  LFixture.Free;
  LFixture := nil;
  try
    LGenerated := LDecoded.CopyGenerated(wmplMelody);
    LTokens := LGenerated.Tokens;
    Check(TokensMatch(LTokens, EncodeWfcMusicMelodyCells(MelodyCellsOf([
      MakeWfcMusicAttackCell(60, 90),
      MakeWfcMusicHoldCell(60, 90),
      MakeWfcMusicRestCell,
      MakeWfcMusicAttackCell(67, 100)
    ]))), 'decode preserves ordered public melody tokens');
    LGenerated.Tokens[0] := 'tampered';
    Check(LDecoded.CopyGenerated(wmplMelody).Tokens[0] =
      'wm1:a:60:90',
      'generated-token getters return detached storage');
    LScore := LDecoded.CopyScore;
    LScore.Free;
    Check(EncodeWfcMusicPassesText(LDecoded) = LEncoded,
      'freeing a copied score does not affect decoded ownership');
  finally
    LDecoded.Free;
    LFixture.Free;
  end;
end;

procedure TestMalformedDocuments;
var
  LEncoded: String;
  LFixture: TWfcMusicComposition;
begin
  LFixture := BuildFixture;
  try
    LEncoded := EncodeWfcMusicPassesText(LFixture);
  finally
    LFixture.Free;
  end;

  ExpectRejected('', 'decoder rejects an empty document');
  ExpectRejected(StringReplace(LEncodeD, #10, #13#10, [rfReplaceAll]),
    'decoder rejects CRLF');
  ExpectRejected(Copy(LEncodeD, 1, Length(LEncodeD) - 1),
    'decoder rejects a missing final LF');
  ExpectRejected(ReplaceOnce(LEncodeD, 'wfcmusicpass=1',
    'wfcmusicpass=2'), 'decoder rejects an unknown format version');
  ExpectRejected(ReplaceOnce(LEncodeD, 'seed=4045620583',
    'seed=04045620583'), 'decoder rejects a noncanonical seed');
  ExpectRejected(ReplaceOnce(LEncodeD, 'quantum=120', 'quantum=0120'),
    'decoder rejects a noncanonical quantum');
  ExpectRejected(ReplaceOnce(LEncodeD, 'cells=4', 'cells=0'),
    'decoder rejects an empty composition');
  ExpectRejected(ReplaceOnce(LEncodeD, 'harmony=1,', 'harmony=2,'),
    'decoder rejects a noncontiguous harmony index');
  ExpectRejected(ReplaceOnce(LEncodeD, 'rhythm=0,wr1%3Aa',
    'rhythm=0,wr1%3aa'),
    'decoder rejects lowercase percent escapes');
  ExpectRejected(ReplaceOnce(LEncodeD,
    'harmony=0,wh1%3Ap%3A12%3A0', 'harmony=0,wr1%3Aa'),
    'decoder rejects the wrong public token family');
  ExpectRejected(ReplaceOnce(LEncodeD,
    'melody=3,wm1%3Aa%3A67%3A100',
    'melody=3,wm1%3Aa%3A60%3A100'),
    'decoder rejects cross-layer public-token tampering');
  ExpectRejected(ReplaceOnce(LEncodeD, 'signature=', 'signature=0'),
    'decoder rejects a noncanonical signature width');
  ExpectRejected(ReplaceOnce(LEncodeD,
    'length%3D480', 'length%3D360'),
    'decoder rejects invalid nested score semantics');
  ExpectRejected(ReplaceOnce(LEncodeD, 'end'#10, 'end'#10'extra'#10),
    'decoder rejects trailing data');
end;

procedure TestSignatureTamper;
var
  LEncoded: String;
  LFixture: TWfcMusicComposition;
  LSignature: String;
  LTamperedSignature: String;
begin
  LFixture := BuildFixture;
  try
    LEncoded := EncodeWfcMusicPassesText(LFixture);
    LSignature := WfcMusicCompositionSignatureHex(LFixture.Signature);
    if LSignature[1] = '0' then
      LTamperedSignature := '1' + Copy(LSignature, 2, 7)
    else
      LTamperedSignature := '0' + Copy(LSignature, 2, 7);
    ExpectRejected(ReplaceOnce(LEncodeD,
      'signature=' + LSignature, 'signature=' + LTamperedSignature),
      'decoder rejects a canonical-width but incorrect signature');
  finally
    LFixture.Free;
  end;
end;

procedure TestGuardsAndVersion;
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    EncodeWfcMusicPassesText(nil);
  except
    on E: Exception do
      LRejected := True;
  end;
  Check(LRejected, 'encoder rejects a nil composition');
  Check(WFC_MUSIC_PASSES_TEXT_VERSION = 1,
    'music-pass text contract is version one');
end;

begin
  WriteLn('WFC music-pass text conformance suite');
  WriteLn('====================================');
  RunTest('canonical round-trip', @TestCanonicalRoundTrip);
  RunTest('public tokens and ownership', @TestPublicTokensAndOwnership);
  RunTest('malformed documents', @TestMalformedDocuments);
  RunTest('signature tamper', @TestSignatureTamper);
  RunTest('guards and version', @TestGuardsAndVersion);
  WriteLn('====================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d music-pass text checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
