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
unit music_studio_arrangement;
{$mode delphi}{$H+}

interface

uses
  wfc, wfc_music_passes, wfc_music_arrangement, music_studio_workbench;

type
  { A reusable bounded working source for this showcase. Each section is a new
    three-pass solve. Whole-phrase harmony preserves a satisfiable authored
    form; short melody contexts allow recombination within it. Public opening
    locks affect section zero only. No immediate harmonic-form repetition is
    permitted, and every section ends in two rests before the next attack. }
  TMusicStudioSectionSource = class(TWfcMusicArrangementSectionSource)
  private
    FModels: array[0..1] of TWfcMusicPassModels;
    FOpeningLocks: TWfcMusicStudioLocks;
    procedure BuildModels(const AForm: Integer);
  public
    constructor Create(const AOpeningLocks: TWfcMusicStudioLocks);
    destructor Destroy; override;
    function GenerateSection(const ARequest: TWfcMusicArrangementSectionRequest;
      out AComposition: TWfcMusicComposition;
      out AFailure: String): Boolean; override;
    function ValidateContinuity(const ARequest: TWfcMusicArrangementSectionRequest;
      const AComposition: TWfcMusicComposition;
      out AFailure: String): Boolean; override;
  end;

{ Positive decimal seconds with up to three fractional places. Rounded up to
  the next exact score tick, then Config rounds up to a complete 4/4 bar.
  There is no duration policy cap, only the portable exact-integer envelope. }
function MusicStudioRequestedTicks(const ASeconds: String): TWfcMusicArrangementWide;
function MusicStudioArrangementConfig(const ADurationSeconds: String;
  const ASeed: TGraphSeed): TWfcMusicArrangementConfig;

implementation

uses
  SysUtils, wfc_model, wfc_sequence, wfc_sequence_learn, wfc_music,
  wfc_music_sequence;

const
  TICKS_PER_SECOND = 960;
  TICKS_PER_BAR = 1920;

function MusicStudioRequestedTicks(const ASeconds: String): TWfcMusicArrangementWide;
var I, D, Fraction, Digits: Integer; Whole, Extra: TWfcMusicArrangementWide;
  InFraction, HasWhole: Boolean;
begin
  Whole:=0;Fraction:=0;Digits:=0;InFraction:=False;HasWhole:=False;
  if ASeconds='' then raise EMusicStudio.Create('duration seconds are required');
  for I:=1 to Length(ASeconds) do
  begin
    if ASeconds[I]='.' then
    begin
      if InFraction or not HasWhole then raise EMusicStudio.Create('invalid decimal duration');
      InFraction:=True;Continue;
    end;
    if not(ASeconds[I] in ['0'..'9']) then raise EMusicStudio.Create('duration must be positive decimal seconds');
    D:=Ord(ASeconds[I])-48;
    if InFraction then
    begin
      Inc(Digits);
      if Digits>3 then raise EMusicStudio.Create('duration supports at most three decimal places');
      Fraction:=Fraction*10+D;
    end
    else
    begin
      HasWhole:=True;
      if Whole>((WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER div TICKS_PER_SECOND)-D) div 10 then
        raise EMusicStudio.Create('duration exceeds exact tick representation');
      Whole:=Whole*10+D;
    end;
  end;
  if InFraction and(Digits=0) then raise EMusicStudio.Create('duration fraction is empty');
  while Digits<3 do begin Fraction:=Fraction*10;Inc(Digits);end;
  Extra:=(Fraction*TICKS_PER_SECOND+999) div 1000;
  Result:=Whole*TICKS_PER_SECOND;
  if Result>WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER-Extra then
    raise EMusicStudio.Create('duration exceeds exact tick representation');
  Inc(Result,Extra);
  if Result<1 then raise EMusicStudio.Create('duration must be greater than zero');
end;

function MusicStudioArrangementConfig(const ADurationSeconds: String;
  const ASeed: TGraphSeed): TWfcMusicArrangementConfig;
var T, R: TWfcMusicArrangementWide;
begin
  T:=MusicStudioRequestedTicks(ADurationSeconds);
  R:=T mod TICKS_PER_BAR;
  if R<>0 then
  begin
    if T>WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER-(TICKS_PER_BAR-R) then
      raise EMusicStudio.Create('bar-rounded duration exceeds exact tick representation');
    Inc(T,TICKS_PER_BAR-R);
  end;
  Result:=MakeWfcMusicArrangementConfig(T,MUSIC_STUDIO_QUANTUM,16,16,ASeed,wmarExact);
end;

function FormCells(const ACorpus,AForm:Integer):TWfcMusicMelodyCells;
var All:TWfcMusicMelodyCells;
begin
  All:=MusicStudioCorpus(ACorpus);
  if AForm=0 then Result:=Copy(All,8,8)
  else Result:=All;
end;

function Template(const AForm:Integer):TWfcMusicScore;
var T:TWfcMusicTracks;V:TWfcMusicVoices;M:TWfcMusicMeterChanges;
  P:TWfcMusicTempoChanges;Cells:TWfcMusicMelodyCells;
begin
  SetLength(T,1);T[0]:=MakeWfcMusicTrack('lead','Music Studio arrangement');
  SetLength(V,1);V[0]:=MakeWfcMusicVoice(0,'melody');
  SetLength(M,1);M[0]:=MakeWfcMusicMeterChange(0,4,4);
  SetLength(P,1);P[0]:=MakeWfcMusicTempoChange(0,MUSIC_STUDIO_TEMPO);
  Cells:=FormCells(0,AForm);
  Result:=TWfcMusicScore.Create(MUSIC_STUDIO_TPQ,12,Length(Cells)*MUSIC_STUDIO_QUANTUM,
    T,V,M,P,RebuildWfcMusicVoiceSpans(Cells,0,MUSIC_STUDIO_QUANTUM));
end;

procedure TMusicStudioSectionSource.BuildModels(const AForm:Integer);
var H,R,M:TWfcSequenceSamples;I,N:Integer;C:TWfcMusicMelodyCells;
begin
  SetLength(H,MUSIC_STUDIO_CORPUS_COUNT);SetLength(R,Length(H));SetLength(M,Length(H));
  N:=0;
  for I:=0 to High(H) do
  begin
    C:=FormCells(I,AForm);N:=Length(C);
    H[I]:=MakeWfcSequenceSample(EncodeWfcMusicHarmonyCells(ProjectWfcMusicMelodyToHarmony(C,12)));
    R[I]:=MakeWfcSequenceSample(EncodeWfcMusicRhythmCells(ProjectWfcMusicMelodyToRhythm(C)));
    M[I]:=MakeWfcSequenceSample(EncodeWfcMusicMelodyCells(C));
  end;
  FModels[AForm].Harmony:=LearnSequenceModelCorpus(H,N);
  FModels[AForm].Rhythm:=LearnSequenceModelCorpus(R,N);
  FModels[AForm].Melody:=LearnSequenceModelCorpus(M,3);
end;

constructor TMusicStudioSectionSource.Create(const AOpeningLocks:TWfcMusicStudioLocks);
begin
  inherited Create;
  FOpeningLocks:=Copy(AOpeningLocks,0,Length(AOpeningLocks));
  BuildModels(0);BuildModels(1);
end;

destructor TMusicStudioSectionSource.Destroy;
var I:Integer;
begin
  for I:=0 to 1 do
  begin
    FModels[I].Harmony.Free;FModels[I].Rhythm.Free;FModels[I].Melody.Free;
  end;
  inherited Destroy;
end;

function PreviousFormKey(const R:TWfcMusicArrangementSectionRequest;
  out AKey:TWfcModelToken;out APosition:Integer):Boolean;
var N:Integer;
begin
  Result:=R.PriorContext.HasPrevious;AKey:='';APosition:=0;
  if not Result then Exit;
  N:=Length(R.PriorContext.HarmonyTokens);
  if R.CellCount=16 then
  begin
    if N<16 then raise EMusicStudio.Create('arrangement source needs the previous full phrase context');
    APosition:=2;AKey:=R.PriorContext.HarmonyTokens[N-16+2];
  end
  else
  begin
    if N<8 then raise EMusicStudio.Create('arrangement source needs the previous bar context');
    APosition:=0;AKey:=R.PriorContext.HarmonyTokens[N-8];
  end;
end;

function TMusicStudioSectionSource.GenerateSection(
  const ARequest:TWfcMusicArrangementSectionRequest;
  out AComposition:TWfcMusicComposition;out AFailure:String):Boolean;
var
  Form,I,J,Position:Integer;S:TWfcMusicScore;C:TWfcMusicPassConfig;
  P:TWfcMusicPassPipeline;O:TGraphSolveOptions;Report:TWfcMusicPassReport;
  Prior:TWfcModelToken;Allowed:TWfcModelTokens;
  H:TWfcMusicHarmonyCells;Cells:TWfcMusicMelodyCells;
begin
  AComposition:=nil;AFailure:='';Result:=False;S:=nil;P:=nil;
  try
    try
      if not(ARequest.CellCount in [8,16]) or
          (ARequest.LengthTicks<>ARequest.CellCount*MUSIC_STUDIO_QUANTUM) then
        raise EMusicStudio.Create('Studio source needs one or two complete 4/4 bars');
      if ARequest.CellCount=8 then Form:=0 else Form:=1;
      S:=Template(Form);
      C:=DefaultWfcMusicPassConfig(S,MUSIC_STUDIO_QUANTUM,ARequest.Seed);
      C.Models:=FModels[Form];P:=TWfcMusicPassPipeline.Create(C);
      if PreviousFormKey(ARequest,Prior,Position) then
      begin
        SetLength(Allowed,MUSIC_STUDIO_CORPUS_COUNT);J:=0;
        for I:=0 to MUSIC_STUDIO_CORPUS_COUNT-1 do
        begin
          Cells:=FormCells(I,Form);H:=ProjectWfcMusicMelodyToHarmony(Cells,12);
          if EncodeWfcMusicHarmonyCell(H[Position])<>Prior then
          begin Allowed[J]:=EncodeWfcMusicHarmonyCell(H[Position]);Inc(J);end;
        end;
        SetLength(Allowed,J);P.IntersectAllowedTokens(wmplHarmony,Position,Allowed);
      end;
      if ARequest.Index=0 then
        for I:=0 to High(FOpeningLocks) do
        begin
          if(FOpeningLocks[I].Position<0) or(FOpeningLocks[I].Position>=ARequest.CellCount) then
            raise EMusicStudio.Create('an opening lock lies outside the requested first section');
          P.IntersectAllowedTokens(FOpeningLocks[I].Layer,FOpeningLocks[I].Position,FOpeningLocks[I].Token);
        end;
      O:=DefaultGraphSolveOptions;O.MaxBacktracks:=256;
      Result:=P.TryGenerate(O,AComposition,Report);
      if not Result then AFailure:='section constraints did not solve; opening locks may contradict the phrase form';
    except
      on E:EMusicStudio do begin AFailure:=E.Message;Result:=False;end;
      on E:EWfcMusic do begin AFailure:=E.Message;Result:=False;end;
    end;
  finally P.Free;S.Free;end;
  if not Result then FreeAndNil(AComposition);
end;

function TMusicStudioSectionSource.ValidateContinuity(
  const ARequest:TWfcMusicArrangementSectionRequest;
  const AComposition:TWfcMusicComposition;out AFailure:String):Boolean;
var M:TWfcMusicMelodyCells;R:TWfcMusicRhythmCells;H:TWfcMusicHarmonyCells;
  Prior:TWfcModelToken;Position,I,N:Integer;
begin
  Result:=False;AFailure:='';
  if AComposition=nil then begin AFailure:='missing section composition';Exit;end;
  M:=AComposition.CopyMelodyCells;R:=AComposition.CopyRhythmCells;H:=AComposition.CopyHarmonyCells;
  N:=Length(M);
  if(N<2) or(M[0].Action<>wmcaAttack) or(M[N-1].Action<>wmcaRest) or
      (M[N-2].Action<>wmcaRest) then
  begin AFailure:='section must attack after the boundary and end with two rests';Exit;end;
  for I:=0 to N-1 do
    if(M[I].Action<>R[I].Action) or
        ((M[I].Action<>wmcaRest) and ((H[I].Kind<>wmhckPitchClass) or
        (H[I].PitchClass<>M[I].Pitch mod 12))) then
    begin AFailure:='section public music layers disagree';Exit;end;
  if ARequest.PriorContext.HasPrevious then
  begin
    N:=Length(ARequest.PriorContext.RhythmTokens);
    if(N<2) or(ARequest.PriorContext.RhythmTokens[N-1]<>'wr1:r') or
        (ARequest.PriorContext.RhythmTokens[N-2]<>'wr1:r') then
    begin AFailure:='previous section does not provide the required cadence rests';Exit;end;
    if PreviousFormKey(ARequest,Prior,Position) and
        (EncodeWfcMusicHarmonyCell(H[Position])=Prior) then
    begin AFailure:='immediate harmonic-form repetition is forbidden';Exit;end;
  end;
  Result:=True;
end;

end.
