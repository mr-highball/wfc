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
program wfc_music_studio_arrangement_test;
{$mode delphi}{$H+}
uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,wfc,wfc_music,wfc_music_sequence,wfc_music_passes,
  wfc_music_arrangement,music_studio_workbench,music_studio_arrangement;
var Checks:Integer;
procedure Check(const B:Boolean;const M:String);
begin Inc(Checks);if not B then raise Exception.Create(M);end;
procedure BadDuration(const S:String);
var C:TWfcMusicArrangementConfig;Raised:Boolean;
begin
  Raised:=False;
  try C:=MusicStudioArrangementConfig(S,0);except on EMusicStudio do Raised:=True;end;
  Check(Raised,'reject duration '+S);
end;
procedure RunPlan(const Seconds:String;const Seed:TGraphSeed);
var Source:TMusicStudioSectionSource;A:TWfcMusicArrangement;C:TWfcMusicArrangementConfig;
  Section:TWfcMusicArrangementSection;Step:TWfcMusicArrangementStep;
  S:TWfcMusicScore;M,Prior:TWfcMusicMelodyCells;Count:Integer;Total:TWfcMusicArrangementWide;
begin
  C:=MusicStudioArrangementConfig(Seconds,Seed);
  Source:=TMusicStudioSectionSource.Create(nil);A:=nil;Total:=0;Count:=0;Prior:=nil;
  try
    A:=TWfcMusicArrangement.Create(C,Source);
    repeat
      Section:=nil;Step:=A.Next(Section);
      if Step=wmaspCompleted then Break;
      Check(Step=wmaspProduced,'Studio source solve: '+A.Failure);
      try
        Check(Section.Seed=WfcMusicArrangementSectionSeed(Seed,Count),'per-section seed provenance');
        Check(Section.StartTick=Total,'continuous section timeline');
        S:=Section.Composition.CopyScore;
        try
          M:=ProjectWfcMusicVoiceToMelodyCells(S,0,240);
          Check((S.LengthTicks=Section.LengthTicks) and(S.LengthTicks mod 1920=0),'exact full-bar score extent');
          Check((M[0].Action=wmcaAttack) and(M[High(M)].Action=wmcaRest) and
            (M[High(M)-1].Action=wmcaRest),'attack/cadence seam semantics');
          { Independently compare pitch classes projected from the actual
            scores, without calling the source's own continuity validator. }
          if Count>0 then
          begin
            if Length(M)=16 then
            begin
              Check((Prior[2].Action=wmcaAttack) and(M[2].Action=wmcaAttack),
                'full-phrase form keys are actual attacks');
              Check((Prior[2].Pitch mod 12)<>(M[2].Pitch mod 12),
                'adjacent full phrases do not repeat their harmonic form');
            end
            else
            begin
              Check((Prior[Length(Prior)-8].Action=wmcaAttack) and(M[0].Action=wmcaAttack),
                'half-phrase form keys are actual attacks');
              Check((Prior[Length(Prior)-8].Pitch mod 12)<>(M[0].Pitch mod 12),
                'final half phrase differs from the preceding closing bar');
            end;
          end;
          Prior:=Copy(M,0,Length(M));
        finally S.Free;end;
        Inc(Total,Section.LengthTicks);Inc(Count);
      finally Section.Free;end;
    until False;
    Check((Total=C.RequestedTicks) and(A.ProducedTicks=Total),'requested arrangement extent produced');
    Check(Count=A.SectionCount,'exact section count');
    Check((Seconds<>'6') or(Count=2),'six seconds needs full and half phrase');
  finally A.Free;Source.Free;end;
end;
procedure TestLocks;
var L:TWfcMusicStudioLocks;Source:TMusicStudioSectionSource;A:TWfcMusicArrangement;
  S:TWfcMusicArrangementSection;Step:TWfcMusicArrangementStep;
begin
  SetLength(L,1);L[0].Layer:=wmplRhythm;L[0].Position:=0;L[0].Token:='wr1:r';
  Source:=TMusicStudioSectionSource.Create(L);A:=nil;
  L[0].Token:='wr1:a';
  try
    A:=TWfcMusicArrangement.Create(MusicStudioArrangementConfig('6',0),Source);
    Step:=A.Next(S);
    Check((Step=wmaspFailed) and(S=nil) and(A.ProducedTicks=0),'detached impossible opening lock rejects atomically');
  finally A.Free;Source.Free;end;
end;
procedure TestSuccessfulOpeningLock;
var L:TWfcMusicStudioLocks;Source:TMusicStudioSectionSource;A:TWfcMusicArrangement;
  S:TWfcMusicArrangementSection;R:TWfcMusicRhythmCells;
begin
  SetLength(L,1);L[0].Layer:=wmplRhythm;L[0].Position:=15;L[0].Token:='wr1:r';
  Source:=TMusicStudioSectionSource.Create(L);A:=nil;
  { Mutating the caller's record after construction must not turn the valid
    copied final-rest lock into an impossible final attack. }
  L[0].Token:='wr1:a';
  try
    A:=TWfcMusicArrangement.Create(MusicStudioArrangementConfig('6',0),Source);
    S:=nil;Check(A.Next(S)=wmaspProduced,'feasible detached opening lock solves');
    try
      R:=S.Composition.CopyRhythmCells;
      Check((S.CellCount=16) and(R[15].Action=wmcaRest),
        'opening position-15 lock is preserved in the full first phrase');
    finally S.Free;end;
    S:=nil;Check(A.Next(S)=wmaspProduced,'position-15 lock does not constrain the eight-cell successor');
    try
      Check((S.CellCount=8) and(S.StartTick=3840),
        'half-phrase successor keeps its independent exact extent');
    finally S.Free;end;
    Check((A.Next(S)=wmaspCompleted) and(S=nil) and(A.ProducedTicks=5760),
      'opening-only lock permits all six seconds to complete');
  finally A.Free;Source.Free;end;
end;
var I:Integer;C:TWfcMusicArrangementConfig;Source:TMusicStudioSectionSource;
  A:TWfcMusicArrangement;S:TWfcMusicArrangementSection;
begin
  Checks:=0;
  Check(MusicStudioRequestedTicks('0.001')=1,'ceil fractional seconds to exact tick');
  C:=MusicStudioArrangementConfig('5.001',0);
  Check(C.RequestedTicks=5760,'round fractional duration up to complete bar');
  C:=MusicStudioArrangementConfig('180',0);
  Check(C.RequestedTicks=172800,'three minutes is one ordinary chosen duration');
  BadDuration('');BadDuration('0');BadDuration('-1');BadDuration('1.0001');
  BadDuration('1.');BadDuration('.5');BadDuration('1e6');BadDuration('9999999999999999999');
  for I:=0 to 15 do RunPlan('6',I);
  RunPlan('2',55);RunPlan('4',55);RunPlan('180',55);
  TestLocks;
  TestSuccessfulOpeningLock;
  Source:=TMusicStudioSectionSource.Create(nil);A:=nil;
  try
    C:=MusicStudioArrangementConfig('1000000000',55);
    A:=TWfcMusicArrangement.Create(C,Source);
    Check(A.Next(S)=wmaspProduced,'huge plan yields first small section lazily');
    S.Free;
    Check(A.ProducedTicks=3840,'no allocation/work proportional to total duration');
    A.Cancel;Check(A.Next(S)=wmaspCancelled,'long plan cancels between sections');
  finally A.Free;Source.Free;end;
  WriteLn('Checks: ',Checks,', Failures: 0');
end.
