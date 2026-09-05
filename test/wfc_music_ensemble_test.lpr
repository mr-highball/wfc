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
program wfc_music_ensemble_test;
{$mode delphi}{$H+}
uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc_model, wfc_music, wfc_music_sequence, wfc_music_ensemble,
  wfc_music_text;

var Checks, Failures: Integer;

procedure Check(const ACondition: Boolean; const ALabel: String);
begin
  Inc(Checks);
  if not ACondition then begin Inc(Failures); WriteLn('[FAIL] ', ALabel); end;
end;

function Tones(const APitches: array of Integer;
  const AVelocities: array of Integer): TWfcMusicTones;
var I: Integer;
begin
  Result := nil;
  if Length(APitches) <> Length(AVelocities) then raise Exception.Create('fixture tone arity');
  SetLength(Result, Length(APitches));
  for I := 0 to High(Result) do
    Result[I] := MakeWfcMusicTone(APitches[I], AVelocities[I]);
end;

function Fixture: TWfcMusicScore;
var Tracks: TWfcMusicTracks; Voices: TWfcMusicVoices;
  Meters: TWfcMusicMeterChanges; Tempos: TWfcMusicTempoChanges;
  Spans: TWfcMusicSpanEvents; I: Integer;
begin
  SetLength(Tracks, 3); SetLength(Voices, 3);
  for I := 0 to 2 do
  begin
    Tracks[I] := MakeWfcMusicTrack(TWfcModelToken('track' + IntToStr(I)), TWfcModelToken('role' + IntToStr(I)));
    Voices[I] := MakeWfcMusicVoice(I, TWfcModelToken('voice' + IntToStr(I)));
  end;
  SetLength(Meters, 2);
  Meters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  Meters[1] := MakeWfcMusicMeterChange(480, 2, 4);
  SetLength(Tempos, 2);
  Tempos[0] := MakeWfcMusicTempoChange(0, 500000);
  Tempos[1] := MakeWfcMusicTempoChange(480, 600000);
  SetLength(Spans, 11);
  Spans[0] := MakeWfcMusicSound(0, 0, 960, Tones([36,43], [80,76]));
  Spans[1] := MakeWfcMusicSound(1, 0, 240, Tones([60,64,67], [70,80,90]));
  Spans[2] := MakeWfcMusicSound(1, 240, 240, Tones([60,64,67], [70,80,90]));
  Spans[3] := MakeWfcMusicRest(1, 480, 240);
  Spans[4] := MakeWfcMusicSound(1, 720, 240, Tones([62,65,69], [71,81,91]));
  Spans[5] := MakeWfcMusicSound(2, 0, 120, Tones([72], [96]));
  Spans[6] := MakeWfcMusicRest(2, 120, 120);
  Spans[7] := MakeWfcMusicSound(2, 240, 240, Tones([74], [94]));
  Spans[8] := MakeWfcMusicSound(2, 480, 120, Tones([76], [92]));
  Spans[9] := MakeWfcMusicSound(2, 600, 120, Tones([76], [92]));
  Spans[10] := MakeWfcMusicRest(2, 720, 240);
  Result := TWfcMusicScore.Create(120, 12, 960, Tracks, Voices, Meters, Tempos, Spans);
end;

function Template(const AVoiceCount, ALength: Integer): TWfcMusicScore;
var Tracks: TWfcMusicTracks; Voices: TWfcMusicVoices;
  Meters: TWfcMusicMeterChanges; Tempos: TWfcMusicTempoChanges;
  Spans: TWfcMusicSpanEvents; I: Integer;
begin
  SetLength(Tracks, 1); Tracks[0] := MakeWfcMusicTrack('ensemble', 'ensemble');
  SetLength(Voices, AVoiceCount); SetLength(Spans, AVoiceCount);
  for I := 0 to AVoiceCount - 1 do
  begin
    Voices[I] := MakeWfcMusicVoice(0, TWfcModelToken('voice' + IntToStr(I)));
    Spans[I] := MakeWfcMusicRest(I, 0, ALength);
  end;
  SetLength(Meters, 1); Meters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(Tempos, 1); Tempos[0] := MakeWfcMusicTempoChange(0, 500000);
  Result := TWfcMusicScore.Create(1, 12, ALength, Tracks, Voices, Meters, Tempos, Spans);
end;

procedure BadToken(const AText: String; const AKind: Integer);
var Rejected: Boolean;
begin
  Rejected := False;
  try
    case AKind of
      0: DecodeWfcMusicEnsembleFrame(TWfcModelToken(AText));
      1: DecodeWfcMusicRhythmFrame(TWfcModelToken(AText));
      2: DecodeWfcMusicPitchClassSet(TWfcModelToken(AText));
    end;
  except on E: EWfcMusicEnsemble do Rejected := True; end;
  Check(Rejected, 'reject token ' + AText);
end;

procedure BadTimeline(const AFrames: TWfcMusicEnsembleFrames; const ALabel: String);
var Rejected: Boolean;
begin
  Rejected := False;
  try ValidateWfcMusicEnsembleFrames(AFrames);
  except on E: EWfcMusicEnsemble do Rejected := True; end;
  Check(Rejected, ALabel);
end;

procedure CodecChecks;
var F, CopyF: TWfcMusicEnsembleFrame; R: TWfcMusicRhythmFrame;
  P, Q: TWfcMusicPitchClassSet; V: TWfcMusicVoiceCells;
  T: TWfcMusicTones; C: TWfcMusicPitchClasses;
  A: TWfcMusicCellActions; Rejected: Boolean; I: Integer;
begin
  F := DecodeWfcMusicEnsembleFrame('wme1:2:a:1:48:80:h:2:60:90:64:75');
  Check(EncodeWfcMusicEnsembleFrame(F) = 'wme1:2:a:1:48:80:h:2:60:90:64:75', 'frame canonical roundtrip');
  R := DecodeWfcMusicRhythmFrame('wmer1:3:r:a:h');
  Check(EncodeWfcMusicRhythmFrame(R) = 'wmer1:3:r:a:h', 'rhythm vector roundtrip');
  P := DecodeWfcMusicPitchClassSet('wmhs1:19:3:0:12:18');
  Check(EncodeWfcMusicPitchClassSet(P) = 'wmhs1:19:3:0:12:18', 'non-twelve pitchclass codec');
  P := DecodeWfcMusicPitchClassSet('wmhs1:12:0');
  Check(Length(P.PitchClasses) = 0, 'canonical empty pitchclass set');
  Check(EncodeWfcMusicEnsembleFrame(DecodeWfcMusicEnsembleFrame('wme1:1:r')) = 'wme1:1:r', 'silent frame roundtrip');
  Check(not WfcMusicEnsembleFrameCanStart(F), 'standalone hold vocabulary is not a timeline start');
  Check(Length(DecodeWfcMusicEnsembleFrames(nil)) = 0, 'empty bulk decoder has no temporal implication');
  Check(Length(EncodeWfcMusicEnsembleFrames(nil)) = 0, 'empty bulk encoder');
  Check(Length(EncodeWfcMusicRhythmFrames(DecodeWfcMusicRhythmFrames(nil))) = 0, 'empty rhythm bulk');
  Check(Length(EncodeWfcMusicPitchClassSets(DecodeWfcMusicPitchClassSets(nil))) = 0, 'empty set bulk');

  BadToken('', 0); BadToken('wme2:1:r', 0); BadToken('wm1:r', 0);
  BadToken('wme1:0', 0); BadToken('wme1:01:r', 0); BadToken('wme1:-1:r', 0);
  BadToken('wme1:1:r:', 0); BadToken('wme1:1:r:r', 0); BadToken('wme1:1::r', 0);
  BadToken('wme1:1:r:0', 0); BadToken('wme1:1:z', 0); BadToken('wme1:1:R', 0);
  BadToken('wme1:1:a:0', 0); BadToken('wme1:1:h:0', 0);
  BadToken('wme1:1:a:1:-1:90', 0); BadToken('wme1:1:a:1:60:0', 0);
  BadToken('wme1:1:a:1:60:128', 0); BadToken('wme1:1:a:1:060:90', 0);
  BadToken('wme1:1:a:1:60:+90', 0); BadToken('wme1:1:a:1:60:90 ', 0);
  BadToken('wme1:1:a:1:2147483648:90', 0);
  BadToken('wme1:1:a:2:60:90:60:91', 0);
  BadToken('wme1:1:a:2:64:90:60:91', 0);
  BadToken('wme1:2147483647:r', 0);
  BadToken('wme1:1:a:2147483647:60:90', 0);
  BadToken('wme1:1:a:2:60:90', 0);
  BadToken('wme1:2:r', 0);
  BadToken('wmer1:0', 1); BadToken('wmer1:1:h:60', 1);
  BadToken('wmer1:2:a', 1); BadToken('wmer1:2147483647:r', 1);
  BadToken('wmer1:1:a:', 1); BadToken('wmer1:1:x', 1);
  BadToken('wmhs1:0:0', 2); BadToken('wmhs1:12:1:12', 2);
  BadToken('wmhs1:12:2:0:0', 2); BadToken('wmhs1:12:2:7:4', 2);
  BadToken('wmhs1:12:1:-1', 2); BadToken('wmhs1:12:0:0', 2);
  BadToken('wmhs1:12:0:', 2); BadToken('wmhs1:12:2147483647:0', 2);
  BadToken('wmhs1:012:0', 2);
  BadToken('wme1:1:r' + Char(128), 0);

  T := Tones([60,64], [90,75]);
  SetLength(V, 1); V[0] := MakeWfcMusicVoiceCell(wmcaAttack, T);
  T[0].Pitch := 59;
  Check(V[0].Tones[0].Pitch = 60, 'voice constructor detaches tone array');
  F := MakeWfcMusicEnsembleFrame(V);
  V[0].Tones[0].Pitch := 58;
  Check(F.Voices[0].Tones[0].Pitch = 60, 'frame constructor detaches nested tones');
  CopyF := MakeWfcMusicEnsembleFrame(F.Voices);
  CopyF.Voices[0].Tones[1].Velocity := 100;
  Check(F.Voices[0].Tones[1].Velocity = 75, 'frame copies detach in both directions');
  SetLength(C, 2); C[0] := 0; C[1] := 7;
  P := MakeWfcMusicPitchClassSet(12, C); C[1] := 8;
  Check(P.PitchClasses[1] = 7, 'pitchclass constructor detached');
  SetLength(A, 1); A[0] := wmcaHold;
  R := MakeWfcMusicRhythmFrame(A); A[0] := wmcaRest;
  Check(R.Actions[0] = wmcaHold, 'rhythm constructor detached');
  Rejected := False;
  try MakeWfcMusicVoiceCell(wmcaRest, Tones([60], [90]));
  except on E: EWfcMusicEnsemble do Rejected := True; end;
  Check(Rejected, 'rest constructor rejects hidden tones');
  Rejected := False;
  try MakeWfcMusicVoiceCell(wmcaAttack, nil);
  except on E: EWfcMusicEnsemble do Rejected := True; end;
  Check(Rejected, 'attack constructor rejects no tones');
  Rejected := False;
  try MakeWfcMusicEnsembleFrame(nil);
  except on E: EWfcMusicEnsemble do Rejected := True; end;
  Check(Rejected, 'zero voice frame rejected');
  Rejected := False;
  try MakeWfcMusicRhythmFrame(nil);
  except on E: EWfcMusicEnsemble do Rejected := True; end;
  Check(Rejected, 'zero voice rhythm rejected');

  Q := DecodeWfcMusicPitchClassSet('wmhs1:12:3:0:4:7');
  Check(not WfcMusicPitchClassSetsEqual(P,Q), 'exact projection is not subset');
  Check(WfcMusicPitchClassSetIsSubset(P,Q), 'allowed-set subset relation');
  Check(not WfcMusicPitchClassSetIsSubset(Q,P), 'missing allowed class rejects');
  P := DecodeWfcMusicPitchClassSet('wmhs1:12:0');
  Check(WfcMusicPitchClassSetIsSubset(P,Q), 'silence satisfies any allowed palette');
  Check(not WfcMusicPitchClassSetsEqual(P,Q), 'silence differs from nonempty exact set');
  Check(WfcMusicPitchClassSetsEqual(P,P), 'empty exact equals empty');
  Q := DecodeWfcMusicPitchClassSet('wmhs1:19:0');
  Check(not WfcMusicPitchClassSetIsSubset(P,Q), 'different step systems do not compare as subsets');
  Check(not WfcMusicPitchClassSetsEqual(P,Q), 'different step systems are not equal');
  F := DecodeWfcMusicEnsembleFrame('wme1:1:a:1:2147483647:127');
  Check(EncodeWfcMusicPitchClassSet(ProjectWfcMusicEnsembleFrameToPitchClassSet(F, High(Integer))) =
    'wmhs1:2147483647:1:0', 'max Integer pitch and tuning remain exact');
  SetLength(T, 17);
  for I := 0 to High(T) do T[I] := MakeWfcMusicTone(I, 80);
  SetLength(V, 40);
  for I := 0 to High(V) do V[I] := MakeWfcMusicVoiceCell(wmcaAttack,T);
  F := MakeWfcMusicEnsembleFrame(V);
  CopyF := DecodeWfcMusicEnsembleFrame(EncodeWfcMusicEnsembleFrame(F));
  Check((Length(CopyF.Voices)=40) and (Length(CopyF.Voices[39].Tones)=17),
    'core has no inherited 16-tone or 32-voice preview cap');
end;

procedure TimelineChecks;
var S, R, Other: TWfcMusicScore; F, Bad: TWfcMusicEnsembleFrames;
  Tokens: TWfcModelTokens; Spans: TWfcMusicSpanEvents;
  I: Integer; Rejected: Boolean;
begin
  S := Fixture;
  try
    F := ProjectWfcMusicScoreToEnsembleFrames(S, 120);
    Check((Length(F)=8) and (Length(F[0].Voices)=3), 'synchronized frame shape');
    ValidateWfcMusicEnsembleFrames(F); Check(True, 'valid independent timelines');
    Tokens := EncodeWfcMusicEnsembleFrames(F);
    Check(Tokens[0]='wme1:3:a:2:36:80:43:76:a:3:60:70:64:80:67:90:a:1:72:96', 'complete opening frame golden');
    Check(EncodeWfcMusicRhythmFrame(ProjectWfcMusicEnsembleFrameToRhythm(F[1])) =
      'wmer1:3:h:h:r', 'held chord and independent rest rhythm');
    Check((F[2].Voices[0].Action=wmcaHold) and
      (F[2].Voices[1].Action=wmcaAttack) and (F[2].Voices[2].Action=wmcaAttack),
      'other onsets do not retrigger bass');
    Check(EncodeWfcMusicPitchClassSet(ProjectWfcMusicEnsembleFrameToPitchClassSet(F[1],12)) =
      'wmhs1:12:3:0:4:7', 'holds contribute to exact sonority and octave duplicates deduplicate');
    Check(EncodeWfcMusicPitchClassSet(ProjectWfcMusicEnsembleFrameToPitchClassSet(F[0],19)) =
      'wmhs1:19:6:3:5:7:10:15:17', 'general tuning projection sorted');
    Check(EncodeWfcMusicPitchClassSet(ProjectWfcMusicEnsembleFrameToPitchClassSet(F[6],12)) =
      'wmhs1:12:5:0:2:5:7:9', 'union includes held bass under changed accompaniment');
    Check(Length(EncodeWfcMusicRhythmFrames(ProjectWfcMusicEnsembleFramesToRhythm(F)))=8, 'rhythm series projection');
    Check(Length(EncodeWfcMusicPitchClassSets(ProjectWfcMusicEnsembleFramesToPitchClassSets(F,12)))=8, 'sonority series projection');
    R := RebuildWfcMusicEnsembleScore(F,120,S);
    try
      Check(EncodeWfcMusicText(R)=EncodeWfcMusicText(S), 'exact score roundtrip includes metadata and all voice attacks');
      Check(R.SpanCount=11, 'no chord or repeated note attacks coalesced');
      Check(R.SpanAt(0).DurationTicks=960, 'bass remains one uninterrupted chord span');
      Check((R.SpanAt(1).StartTick=0) and (R.SpanAt(2).StartTick=240),
        'equal chord reattack retained');
      Check((R.SpanAt(8).StartTick=480) and (R.SpanAt(9).StartTick=600),
        'equal note reattack retained');
    finally R.Free; end;
    Spans := RebuildWfcMusicEnsembleSpans(F,120);
    F[0].Voices[0].Tones[0].Velocity := 1;
    Check(Spans[0].Tones[0].Velocity=80, 'rebuilt spans own detached tones');
    Check(F[1].Voices[0].Tones[0].Velocity=80, 'projected holds do not alias attack tone array');
    Check(S.SpanAt(0).Tones[0].Velocity=80, 'projection does not alias score');
    F := DecodeWfcMusicEnsembleFrames(Tokens);
    for I := 0 to High(F) do
      Check(EncodeWfcMusicEnsembleFrame(F[I])=Tokens[I], 'bulk codec frame '+IntToStr(I));
    Bad := DecodeWfcMusicEnsembleFrames(Tokens);
    Bad[0].Voices[0].Action := wmcaHold; BadTimeline(Bad,'orphan initial hold');
    Bad := DecodeWfcMusicEnsembleFrames(Tokens);
    Bad[1].Voices[0].Tones[0].Velocity := 79; BadTimeline(Bad,'held chord velocity mismatch');
    Bad := DecodeWfcMusicEnsembleFrames(Tokens);
    Bad[1].Voices[0].Tones[0].Pitch := 35; BadTimeline(Bad,'held chord pitch mismatch');
    Bad := DecodeWfcMusicEnsembleFrames(Tokens);
    SetLength(Bad[1].Voices[0].Tones,1); BadTimeline(Bad,'held chord membership mismatch');
    Bad := DecodeWfcMusicEnsembleFrames(Tokens);
    Bad[1].Voices[2] := MakeWfcMusicRestVoiceCell;
    Bad[2].Voices[2].Action := wmcaHold; BadTimeline(Bad,'hold after rest rejected');
    Bad := DecodeWfcMusicEnsembleFrames(Tokens);
    SetLength(Bad[1].Voices,2); BadTimeline(Bad,'changing voice arity rejected');
    BadTimeline(nil,'empty temporal series rejected');
    Rejected := False;
    try ProjectWfcMusicScoreToEnsembleFrames(nil,120);
    except on E: EWfcMusicEnsemble do Rejected := True; end;
    Check(Rejected,'nil projection score rejected');
    Rejected := False;
    try ProjectWfcMusicScoreToEnsembleFrames(S,0);
    except on E: EWfcMusicEnsemble do Rejected := True; end;
    Check(Rejected,'zero projection quantum rejected');
    Rejected := False;
    try ProjectWfcMusicScoreToEnsembleFrames(S,7);
    except on E: EWfcMusicEnsemble do Rejected := True; end;
    Check(Rejected,'nonaligned score length rejected');
    Rejected := False;
    try ProjectWfcMusicScoreToEnsembleFrames(S,240);
    except on E: EWfcMusicEnsemble do Rejected := True; end;
    Check(Rejected,'nonaligned individual span rejected before projection');
    Rejected := False;
    try RebuildWfcMusicEnsembleSpans(F,High(Integer));
    except on E: EWfcMusicEnsemble do Rejected := True; end;
    Check(Rejected,'rebuilt tick overflow rejected');
    Rejected := False;
    try RebuildWfcMusicEnsembleSpans(F,0);
    except on E: EWfcMusicEnsemble do Rejected := True; end;
    Check(Rejected,'zero rebuild quantum rejected');
    Rejected := False;
    try R := RebuildWfcMusicEnsembleScore(F,120,nil); R.Free;
    except on E: EWfcMusicEnsemble do Rejected := True; end;
    Check(Rejected,'nil template rejected');
    Rejected := False;
    try R := RebuildWfcMusicEnsembleScore(F,60,S); R.Free;
    except on E: EWfcMusicEnsemble do Rejected := True; end;
    Check(Rejected,'template length mismatch rejected');
    Other := Template(1,960);
    try
      Rejected := False;
      try R := RebuildWfcMusicEnsembleScore(F,120,Other); R.Free;
      except on E: EWfcMusicEnsemble do Rejected := True; end;
      Check(Rejected,'template voice arity mismatch rejected');
    finally Other.Free; end;
  finally S.Free; end;
  S := Template(40,4);
  try
    F := ProjectWfcMusicScoreToEnsembleFrames(S,1);
    Check((Length(F)=4) and (Length(F[3].Voices)=40), 'silent voices retain their slots');
    R := RebuildWfcMusicEnsembleScore(F,1,S);
    try Check(EncodeWfcMusicText(R)=EncodeWfcMusicText(S), 'forty silent voices exact roundtrip');
    finally R.Free; end;
  finally S.Free; end;
end;

procedure SetProjectionChecks;
var Steps, N, I, VIndex, Next: Integer;
  Seen: array[0..66] of Boolean;
  V: TWfcMusicVoiceCells; T: TWfcMusicTones;
  P: TWfcMusicPitchClassSet; Matches: Boolean;
begin
  SetLength(V,2);
  for Steps := 1 to 67 do
  begin
    for I := 0 to High(Seen) do Seen[I] := False;
    N := (Steps - 1) * 2;
    for VIndex := 0 to 1 do
    begin
      SetLength(T,N);
      for I := 0 to N - 1 do
      begin
        T[I] := MakeWfcMusicTone(I * 7 + VIndex * 11, 1 + I mod 127);
        Seen[T[I].Pitch mod Steps] := True;
      end;
      if N = 0 then V[VIndex] := MakeWfcMusicRestVoiceCell
      else V[VIndex] := MakeWfcMusicVoiceCell(wmcaAttack,T);
    end;
    P := ProjectWfcMusicEnsembleFrameToPitchClassSet(MakeWfcMusicEnsembleFrame(V),Steps);
    Next := 0; Matches := True;
    for I := 0 to Steps - 1 do
      if Seen[I] then
      begin
        if Next >= Length(P.PitchClasses) then Matches := False
        else if P.PitchClasses[Next] <> I then Matches := False;
        Inc(Next);
      end;
    Check(Matches and (Next=Length(P.PitchClasses)),
      'independent set membership oracle for '+IntToStr(Steps)+' steps');
  end;
  SetLength(V,1); SetLength(T,4096);
  for I := 0 to High(T) do T[I] := MakeWfcMusicTone(I * 5002,80);
  V[0] := MakeWfcMusicVoiceCell(wmcaAttack,T);
  P := ProjectWfcMusicEnsembleFrameToPitchClassSet(MakeWfcMusicEnsembleFrame(V),5003);
  Matches := Length(P.PitchClasses)=4096;
  if Matches then
  begin
    Matches := P.PitchClasses[0]=0;
    for I := 1 to High(P.PitchClasses) do
      if P.PitchClasses[I] <> 907 + I then Matches := False;
  end;
  Check(Matches,'large descending modular class sequence sorted and deduplicated');
end;

begin
  Checks := 0; Failures := 0;
  try CodecChecks; TimelineChecks; SetProjectionChecks;
  except on E: Exception do begin Inc(Failures); WriteLn('[EXCEPTION] ',E.ClassName,': ',E.Message); end; end;
  WriteLn('Ensemble checks: ',Checks,', failures: ',Failures);
  if Failures <> 0 then Halt(1);
end.
