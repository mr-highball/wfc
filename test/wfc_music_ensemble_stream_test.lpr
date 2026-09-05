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
program wfc_music_ensemble_stream_test;
{$mode delphi}{$H+}
uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_sequence, wfc_sequence_learn,
  wfc_sequence_graph, wfc_music, wfc_music_sequence, wfc_music_ensemble,
  wfc_music_ensemble_graph, wfc_music_ensemble_passes,
  wfc_music_ensemble_stream, wfc_music_arrangement;

type
  TFixture = record
    Models: TWfcMusicEnsembleModels;
    Frames: TWfcMusicEnsembleFrames;
    Tokens: TWfcModelTokens;
  end;
  TProbeStream = class(TWfcMusicEnsembleStream)
  public
    Mode: Integer;
    Rejected: Boolean;
  protected
    procedure ConfigureSegment(const AIndex,AStartTick:TWfcMusicArrangementWide;
      const ACellCount:Integer; const AGraph:TGraph); override;
  end;
  TTest = procedure;
  TLayerPaths = array[TWfcMusicEnsembleLayer] of TWfcSequenceStateIndices;

var Checks, Failures: Integer;

procedure Check(const ACondition:Boolean; const AMessage:String);
begin
  Inc(Checks);
  if not ACondition then begin Inc(Failures); WriteLn('FAIL: ',AMessage) end;
end;

procedure Run(const AName:String; const ATest:TTest);
begin
  WriteLn('Test: ',AName);
  try ATest;
  except on E:Exception do begin Inc(Failures);
    WriteLn('EXCEPTION: ',E.ClassName,': ',E.Message) end end;
end;

function Voice(const AAction:TWfcMusicCellAction; const APitch:Integer):
  TWfcMusicVoiceCell;
var T:TWfcMusicTones;
begin
  if AAction=wmcaRest then Exit(MakeWfcMusicRestVoiceCell);
  SetLength(T,1); T[0]:=MakeWfcMusicTone(APitch,80);
  Result:=MakeWfcMusicVoiceCell(AAction,T);
end;

function Phrase(const AVariant:Integer):TWfcMusicEnsembleFrames;
var I,P:Integer; V:TWfcMusicVoiceCells;
begin
  Result:=nil; SetLength(Result,24); SetLength(V,2);
  for I:=0 to High(Result) do
  begin
    if I=0 then V[0]:=Voice(wmcaAttack,48)
    else V[0]:=Voice(wmcaHold,48);
    SetLength(V[0].Tones,2);
    V[0].Tones[1]:=MakeWfcMusicTone(60,70);
    if AVariant=0 then P:=60 else P:=72;
    case I mod 4 of
      0: V[1]:=Voice(wmcaAttack,P);
      1: V[1]:=Voice(wmcaAttack,132-P);
      2: V[1]:=Voice(wmcaHold,132-P);
      3: V[1]:=Voice(wmcaRest,0);
    end;
    Result[I]:=MakeWfcMusicEnsembleFrame(V);
  end;
end;

procedure Init(out F:TFixture; const AOrder:Integer);
var S:TWfcSequenceSamples;
begin
  F:=Default(TFixture);
  F.Frames:=Phrase(0);
  F.Tokens:=EncodeWfcMusicEnsembleFrames(F.Frames);
  SetLength(S,2);
  S[0]:=MakeWfcSequenceSample(F.Tokens);
  S[1]:=MakeWfcSequenceSample(EncodeWfcMusicEnsembleFrames(Phrase(1)));
  F.Models.Ensemble:=LearnSequenceModelCorpus(S,AOrder);
  F.Models.Rhythm:=LearnSequenceModel(EncodeWfcMusicRhythmFrames(
    ProjectWfcMusicEnsembleFramesToRhythm(F.Frames)),AOrder);
  F.Models.Harmony:=LearnSequenceModel(EncodeWfcMusicPitchClassSets(
    ProjectWfcMusicEnsembleFramesToPitchClassSets(F.Frames,12)),1);
end;

procedure Done(var F:TFixture);
begin
  F.Models.Ensemble.Free; F.Models.Rhythm.Free; F.Models.Harmony.Free;
  F:=Default(TFixture);
end;

function Config(const F:TFixture; const ACells,ASegment:Integer):
  TWfcMusicEnsembleStreamConfig;
begin
  Result:=DefaultWfcMusicEnsembleStreamConfig(F.Models,2,12,2,ACells*2,0);
  Result.SegmentCellCount:=ASegment;
end;

function Tokens(const AToken:TWfcModelToken):TWfcModelTokens;
begin
  Result:=nil; SetLength(Result,1); Result[0]:=AToken;
end;

procedure TProbeStream.ConfigureSegment(
  const AIndex,AStartTick:TWfcMusicArrangementWide;
  const ACellCount:Integer; const AGraph:TGraph);
var S:TWfcMusicEnsembleSegment; R:TGraphNegotiationReport;
begin
  case Mode of
    1: Cancel;
    2: begin
      try Next(S,R); except on E:EWfcMusicEnsembleStream do Rejected:=True end;
    end;
    3: begin
      try ClearAllowedTokens(wmelEnsemble,0);
      except on E:EWfcMusicEnsembleStream do Rejected:=True end;
    end;
    4: AGraph.Seed:=AGraph.Seed xor 1;
    5: AGraph.PassGraph[0].ClearAllowedValues(0,0,0);
    6: raise Exception.Create('application fault');
  end;
end;

procedure TestContinuousFrontier;
var
  F:TFixture; C:TWfcMusicEnsembleStreamConfig; P:TWfcMusicEnsembleStream;
  S,Saved:TWfcMusicEnsembleSegment; R:TGraphNegotiationReport;
  Frontier,Before:TWfcMusicEnsembleStreamFrontier;
  G:TWfcGeneratedSequenceSegment; Paths:TLayerPaths; L:TWfcMusicEnsembleLayer;
  All,Part:TWfcMusicEnsembleFrames; Spans:TWfcMusicSpanEvents;
  V:TWfcSequenceGraphValidationReport; I,J,N,SegmentIndex,Order:Integer;
  Step:TWfcMusicArrangementStep; FirstSignature:Cardinal;
begin
  for Order:=2 to 6 do
  begin
    Init(F,Order); C:=Config(F,23,1);
    P:=TWfcMusicEnsembleStream.Create(C); Saved:=nil;
    All:=nil; Paths:=Default(TLayerPaths); N:=0; SegmentIndex:=0;
    try
      Check(P.SegmentCount=23,'one-cell plan count');
      Check((P.Status=wmasReady) and not P.CopyFrontier.HasPrevious,
        'initial frontier is empty');
      repeat
        Before:=P.CopyFrontier;
        Step:=P.Next(S,R);
        if Step<>wmaspProduced then Break;
        Check((S.Index=SegmentIndex) and (S.StartTick=N*2) and
          (S.CellCount=1) and (S.QuantumTicks=2),'exact segment coordinates');
        Check((S.Seed=WfcMusicArrangementSectionSeed(0,SegmentIndex)) and
          (R.Seed=S.Seed) and (R.Status=gnsSolved),'derived seed matches actual solve');
        Check(Length(R.FinalReport.Passes)=3,'all three passes execute');
        Part:=S.CopyFrames;
        Check(Length(Part)=S.CellCount,'detached segment frame count');
        if SegmentIndex>0 then Check(Part[0].Voices[0].Action=wmcaHold,
          'sustained bass crosses segment boundary without reattack');
        SetLength(All,N+Length(Part));
        for I:=0 to High(Part) do All[N+I]:=Part[I];
        for L:=Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
        begin
          G:=S.CopyGenerated(L);
          Check(G.Boundary.HasPrevious=(SegmentIndex>0),'exact predecessor presence');
          if SegmentIndex>0 then
            Check(G.Boundary.PreviousState=Before.StateIndices[L],
              'predecessor is authenticated last state, not just matching token');
          SetLength(Paths[L],N+Length(G.StateIndices));
          for J:=0 to High(G.StateIndices) do Paths[L][N+J]:=G.StateIndices[J];
        end;
        Inc(N,Length(Part));
        Frontier:=P.CopyFrontier;
        Check((Frontier.EndTick=N*2) and (P.ProducedTicks=N*2),
          'frontier advances only by published cells');
        if SegmentIndex=0 then
        begin Saved:=S; S:=nil; FirstSignature:=Saved.Signature end;
        S.Free; Inc(SegmentIndex);
      until False;
      Check((Step=wmaspCompleted) and (S=nil) and (N=23) and
        (P.RemainingTicks=0) and (P.Status=wmasCompleted),'exact terminal completion');
      ValidateWfcMusicEnsembleFrames(All);
      Spans:=RebuildWfcMusicEnsembleSpans(All,2);
      Check((Spans[0].VoiceIndex=0) and (Spans[0].DurationTicks=46) and
        (Spans[0].Kind=wmskChord) and (Length(Spans[0].Tones)=2),
        'held bass chord remains one exact sound span');
      Check(ValidateSequenceStatePath(F.Models.Harmony,Paths[wmelHarmony],
        wsePrefix,V),'aggregate harmony path is globally valid');
      Check(ValidateSequenceStatePath(F.Models.Rhythm,Paths[wmelRhythm],
        wsePrefix,V),'aggregate rhythm path is globally valid');
      Check(ValidateSequenceStatePath(F.Models.Ensemble,Paths[wmelEnsemble],
        wsePrefix,V),'aggregate ensemble path is globally valid');
      Check(Saved.Signature=FirstSignature,'later calls cannot mutate retained result');
      Check(P.Next(S,R)=wmaspCompleted,'completed Next is idempotent');
      P.Cancel; Check(P.Status=wmasCompleted,'cancel preserves completed status');
    finally P.Free end;
    Done(F);
    Part:=Saved.CopyFrames;
    Check((Length(Part)=1) and (Part[0].Voices[0].Action=wmcaAttack),
      'result outlives stream and borrowed models');
    Saved.Free;
  end;
end;

procedure TestSizedSegmentsAndCopies;
var F:TFixture; C:TWfcMusicEnsembleStreamConfig;
  P,Q:TWfcMusicEnsembleStream; A,B:TWfcMusicEnsembleSegment;
  R:TGraphNegotiationReport; G,H:TWfcGeneratedSequenceSegment;
  X,Y:TWfcMusicEnsembleFrames; Count:Integer;
begin
  Init(F,4); C:=Config(F,23,5);
  P:=TWfcMusicEnsembleStream.Create(C); Q:=TWfcMusicEnsembleStream.Create(C);
  try
    Check(P.SegmentCount=5,'short final segment count');
    Count:=0;
    while P.Next(A,R)=wmaspProduced do
    begin
      Check(Q.Next(B,R)=wmaspProduced,'replay produces matching segment');
      try
        Check(A.Signature=B.Signature,'segment signature replays');
        Check(A.FinalSegment=(Count=4),'only actual final segment is marked final');
        if A.FinalSegment then Check(A.CellCount=3,'no padding of final segment');
        G:=A.CopyGenerated(wmelEnsemble); G.StateIndices[0]:=-99; G.Tokens[0]:='bad';
        H:=A.CopyGenerated(wmelEnsemble);
        Check((H.StateIndices[0]>=0) and (H.Tokens[0]<>'bad'),'generated arrays detach');
        X:=A.CopyFrames; X[0].Voices[0].Tones[0].Pitch:=99;
        Y:=A.CopyFrames;
        Check(Y[0].Voices[0].Tones[0].Pitch=48,'nested result frames detach');
        if Count=0 then Check(A.Signature=Cardinal($5ED51FA1),
          'versioned first segment signature golden');
      finally A.Free; B.Free end;
      Inc(Count);
    end;
    Check((Count=5) and (Q.Next(B,R)=wmaspCompleted),'both replays end exactly');
  finally P.Free; Q.Free; Done(F) end;
end;

procedure TestConstraintsAndFailure;
var F:TFixture; C:TWfcMusicEnsembleStreamConfig; P:TWfcMusicEnsembleStream;
  S:TWfcMusicEnsembleSegment; R:TGraphNegotiationReport; Before,After:
    TWfcMusicEnsembleStreamFrontier; A:TWfcMusicEnsembleStreamConstraints;
  T:TWfcModelTokens; Raised:Boolean; I:Integer;
begin
  Init(F,4); C:=Config(F,20,4); P:=TWfcMusicEnsembleStream.Create(C);
  try
    T:=Tokens(F.Tokens[0]); P.IntersectAllowedTokens(wmelEnsemble,0,T);
    T[0]:='changed'; A:=P.CopyConstraints; A[0].AllowedTokens[0]:='changed';
    Check(P.CopyConstraints[0].AllowedTokens[0]=F.Tokens[0],'caller masks detach');
    Raised:=False;
    try P.IntersectAllowedTokens(wmelEnsemble,4,Tokens('unknown'));
    except on E:EWfcMusicEnsembleStream do Raised:=True end;
    Check(Raised and (Length(P.CopyConstraints)=1),'unknown token rejects atomically');
    T:=Tokens(F.Tokens[0]); SetLength(T,2); T[1]:=T[0]; Raised:=False;
    try P.IntersectAllowedTokens(wmelEnsemble,4,T);
    except on E:EWfcMusicEnsembleStream do Raised:=True end;
    Check(Raised and (Length(P.CopyConstraints)=1),'duplicate token rejects atomically');
    Check(P.Next(S,R)=wmaspProduced,'opening lock solves');
    try Check(S.CopyGenerated(wmelEnsemble).Tokens[0]=F.Tokens[0],
      'public opening lock honored') finally S.Free end;
    Check(Length(P.CopyConstraints)=0,'past constraints retire without retaining song');
    Raised:=False;
    try P.ClearAllowedTokens(wmelEnsemble,0);
    except on E:EWfcMusicEnsembleStream do Raised:=True end;
    Check(Raised,'cannot revise emitted history');
    P.IntersectAllowedTokens(wmelEnsemble,11,nil);
    Check(P.Next(S,R)=wmaspProduced,'future contradiction does not alter prior segment');
    S.Free; Before:=P.CopyFrontier;
    Check((P.Next(S,R)=wmaspFailed) and (S=nil) and (P.Status=wmasFailed),
      'empty future domain fails at its segment');
    After:=P.CopyFrontier;
    Check((After.EndTick=Before.EndTick) and (P.ProducedTicks=16) and
      (P.Failure<>''),'failed segment does not advance frontier');
    Check((P.NextIndex=2) and (Length(P.CopyConstraints)=1) and
      (P.CopyConstraints[0].Position=11),'failure preserves pending constraint and index');
    for I:=0 to 2 do Check(After.StateIndices[TWfcMusicEnsembleLayer(I)]=
      Before.StateIndices[TWfcMusicEnsembleLayer(I)],'failed segment preserves exact predecessor');
    After.StateIndices[wmelHarmony]:=-99; After.Tokens[wmelHarmony]:='bad';
    After.EndTick:=99;
    Check((P.CopyFrontier.EndTick=Before.EndTick) and
      (P.CopyFrontier.StateIndices[wmelHarmony]>=0) and
      (P.CopyFrontier.Tokens[wmelHarmony]<>'bad'),'diagnostic frontier arrays detach');
    Check(P.Next(S,R)=wmaspFailed,'failed stream stays terminal');
    P.Cancel; Check(P.Status=wmasFailed,'cancel cannot erase failure');
  finally P.Free end;
  P:=TWfcMusicEnsembleStream.Create(C);
  try
    P.IntersectAllowedTokens(wmelEnsemble,0,nil).ClearAllowedTokens(wmelEnsemble,0);
    Check(P.Next(S,R)=wmaspProduced,'clearing unproduced mask restores baseline');
    S.Free; Before:=P.CopyFrontier; P.Cancel;
    Check((P.Next(S,R)=wmaspCancelled) and (S=nil) and
      (P.CopyFrontier.EndTick=Before.EndTick),'cancellation retains only committed frontier');
  finally P.Free; Done(F) end;
end;

procedure BadConfig(const C:TWfcMusicEnsembleStreamConfig; const AName:String);
var P:TWfcMusicEnsembleStream; Raised:Boolean;
begin
  P:=nil; Raised:=False;
  try
    try P:=TWfcMusicEnsembleStream.Create(C);
    except on E:Exception do Raised:=True end;
  finally P.Free end;
  Check(Raised,AName);
end;

procedure TestAllowedHarmonyContinuation;
const
  GoodPalette = 'wmhs1:12:4:0:4:7:9';
  MissingHeldClass = 'wmhs1:12:3:0:4:9';
var
  F:TFixture; C:TWfcMusicEnsembleStreamConfig; P:TWfcMusicEnsembleStream;
  S:TWfcMusicEnsembleSegment; R:TGraphNegotiationReport;
  H:TWfcModelTokens; G:TWfcGeneratedSequenceSegment;
  Part:TWfcMusicEnsembleFrames; Before,After:TWfcMusicEnsembleStreamFrontier;
  L:TWfcMusicEnsembleLayer; I:Integer; Raised:Boolean;
begin
  F:=Default(TFixture);
  P:=nil;
  try
    //Distinct pitch classes in the held chord make omission observable even
    //when the newly attacked upper voice still fits the provider palette.
    F.Frames:=Phrase(0);
    for I:=0 to High(F.Frames) do F.Frames[I].Voices[0].Tones[1].Pitch:=55;
    F.Tokens:=EncodeWfcMusicEnsembleFrames(F.Frames);
    F.Models.Ensemble:=LearnSequenceModel(F.Tokens,4);
    F.Models.Rhythm:=LearnSequenceModel(EncodeWfcMusicRhythmFrames(
      ProjectWfcMusicEnsembleFramesToRhythm(F.Frames)),4);
    SetLength(H,2); H[0]:=GoodPalette; H[1]:=MissingHeldClass;
    F.Models.Harmony:=LearnSequenceModel(H,1);
    C:=Config(F,8,1); C.HarmonyMode:=wmehmAllowed;
    P:=TWfcMusicEnsembleStream.Create(C);
    try
      for I:=0 to 7 do
      begin
        Before:=P.CopyFrontier;
        Check(P.Next(S,R)=wmaspProduced,'allowed superset continues across one-cell segments');
        try
          G:=S.CopyGenerated(wmelHarmony);
          Check(G.Tokens[0]=GoodPalette,'allowed provider retains unused pitch classes');
          Part:=S.CopyFrames;
          Check((Length(Part[0].Voices[0].Tones)=2) and
            (Part[0].Voices[0].Tones[0].Pitch=48) and
            (Part[0].Voices[0].Tones[1].Pitch=55),
            'allowed stream preserves both chord classes and exact pitches');
          if I>0 then
          begin
            Check(Part[0].Voices[0].Action=wmcaHold,
              'allowed harmony never retriggers the held chord');
            G:=S.CopyGenerated(wmelEnsemble);
            Check(G.Boundary.HasPrevious and
              (G.Boundary.PreviousState=Before.StateIndices[wmelEnsemble]),
              'allowed stream preserves authenticated latent predecessor');
          end;
          if I=1 then Check(Part[0].Voices[1].Action=wmcaAttack,
            'another voice attacks while the allowed chord holds across the seam');
        finally S.Free end;
      end;
      Check((P.Next(S,R)=wmaspCompleted) and (S=nil) and (P.ProducedTicks=16),
        'allowed stream completes its exact finite cutoff');
    finally P.Free; P:=nil end;

    //The bad provider still allows the upper voice and root class zero, but
    //omits class seven sounding only in the held chord. It is valid vocabulary
    //and legal sequence context, so rejection must come from cross-pass music.
    P:=TWfcMusicEnsembleStream.Create(C);
    try
      P.IntersectAllowedTokens(wmelHarmony,1,Tokens(MissingHeldClass));
      Check(P.Next(S,R)=wmaspProduced,'future missing chord class permits the valid prefix');
      S.Free; Before:=P.CopyFrontier;
      Check((P.Next(S,R)=wmaspFailed) and (S=nil) and (P.Status=wmasFailed),
        'allowed palette must contain classes belonging to held tones');
      After:=P.CopyFrontier;
      Check((P.NextIndex=1) and (P.ProducedTicks=2) and
        (After.EndTick=Before.EndTick) and (Length(P.CopyConstraints)=1),
        'rejected allowed seam preserves progress and its future constraint');
      for L:=Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
        Check((After.StateIndices[L]=Before.StateIndices[L]) and
          (After.Tokens[L]=Before.Tokens[L]),
          'rejected allowed seam preserves every exact frontier witness');
    finally P.Free; P:=nil end;

    C.HarmonyMode:=wmehmExact; Raised:=False;
    try P:=TWfcMusicEnsembleStream.Create(C);
    except on E:Exception do
      Raised:=Pos('harmony model cannot project',E.Message)>0 end;
    Check(Raised,'strict superset palettes are not silently treated as exact sets');
  finally P.Free; Done(F) end;
end;

procedure TestCapacityAndBoundaries;
var F:TFixture; C:TWfcMusicEnsembleStreamConfig; P:TWfcMusicEnsembleStream;
  S:TWfcMusicEnsembleSegment; R:TGraphNegotiationReport;
  G:TWfcGeneratedSequenceSegment; L:TWfcMusicEnsembleLayer;
  M:TWfcSequenceModel; N:Integer;
begin
  Init(F,4);
  try
    C:=Config(F,20,4); C.RequestedTicks:=0; BadConfig(C,'zero duration');
    C:=Config(F,20,4); C.QuantumTicks:=0; BadConfig(C,'zero quantum');
    C:=Config(F,20,4); C.SegmentCellCount:=High(Integer); BadConfig(C,'local tick overflow');
    C:=Config(F,20,4); C.VoiceCount:=0; BadConfig(C,'zero voice count');
    C:=Config(F,20,4); C.StepsPerOctave:=19; BadConfig(C,'mismatched tuning');
    C:=Config(F,20,4); C.Models.Ensemble:=nil; BadConfig(C,'nil model');
    C:=Config(F,20,4); C.Search.SolveOptions.MaxBacktracks:=-1; BadConfig(C,'negative local budget');
    C:=Config(F,20,4); C.Search.MaxPassBacktracks:=-1; BadConfig(C,'negative pass budget');
    C:=Config(F,20,4); C.RequestedTicks:=3; BadConfig(C,'exact partial quantum');
    C.Rounding:=wmarCeilToCell; P:=TWfcMusicEnsembleStream.Create(C);
    try Check((P.ActualTicks=4) and (P.RequestedTicks=3),'visible ceiling rounding')
    finally P.Free end;
    C.Rounding:=wmarFloorToCell; P:=TWfcMusicEnsembleStream.Create(C);
    try Check(P.ActualTicks=2,'floor rounding') finally P.Free end;
    C:=Config(F,20,4); C.RequestedTicks:=WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER+1;
    BadConfig(C,'unsafe wide duration');
    C.QuantumTicks:=1; C.RequestedTicks:=WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER;
    P:=TWfcMusicEnsembleStream.Create(C);
    try
      Check(P.SegmentCount=2251799813685248,'wide plan without per-song allocation');
      Check(P.Next(S,R)=wmaspProduced,'huge plan lazily yields small prefix');
      try Check((S.CellCount=4) and (P.ProducedTicks=4),'prefix remains bounded')
      finally S.Free end;
      Check(P.Next(S,R)=wmaspProduced,'huge plan continues from bounded exact frontier');
      S.Free;
      Check(P.RemainingTicks=WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER-8,
        'huge progress subtraction remains exact');
      P.Cancel; Check(P.Status=wmasCancelled,'huge plan cancels');
    finally P.Free end;
    C:=Config(F,1,1); C.RequireObservedEnd:=True;
    P:=TWfcMusicEnsembleStream.Create(C);
    try Check((P.Next(S,R)=wmaspFailed) and (P.ProducedTicks=0),
      'requested observed end is a real constraint') finally P.Free end;
    C.RequireObservedEnd:=False; P:=TWfcMusicEnsembleStream.Create(C);
    try
      Check(P.Next(S,R)=wmaspProduced,'finite cutoff need not be an observed corpus end');
      S.Free;
    finally P.Free end;
    C:=Config(F,24,5); C.RequireObservedEnd:=True;
    P:=TWfcMusicEnsembleStream.Create(C);
    try
      N:=0;
      while P.Next(S,R)=wmaspProduced do
      begin
        try
          for L:=Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
          begin
            G:=S.CopyGenerated(L);
            Check(G.Boundary.RequireObservedEnd=S.FinalSegment,
              'observed end applies only to the final local horizon');
            if S.FinalSegment then
            begin
              case L of
                wmelHarmony:M:=F.Models.Harmony;
                wmelRhythm:M:=F.Models.Rhythm;
              else M:=F.Models.Ensemble end;
              Check(M.EndCountAt(G.StateIndices[High(G.StateIndices)])>0,
                'final continued path ends in an observed state');
            end;
          end;
          if S.FinalSegment then Check((S.CellCount=4) and (S.Index=4),
            'short final segment keeps the requested endpoint');
        finally S.Free end;
        Inc(N);
      end;
      Check((N=5) and (P.Status=wmasCompleted),'whole learned endpoint streams successfully');
    finally P.Free end;
  finally Done(F) end;
end;

{$IFDEF PAS2JS}
function MalformedNumber(const AIndex:Integer):NativeInt;
begin
  asm
    if (AIndex === 0) Result = 1.5;
    else if (AIndex === 1) Result = NaN;
    else if (AIndex === 2) Result = Infinity;
    else Result = 9007199254740992;
  end;
end;

procedure TestHostNumbers;
var F:TFixture; C:TWfcMusicEnsembleStreamConfig; P:TWfcMusicEnsembleStream;
  L:TWfcMusicEnsembleLayer; I,J:Integer; V:NativeInt; Raised:Boolean;
begin
  Init(F,4);
  try
    for I:=0 to 3 do
      for J:=0 to 9 do
      begin
        C:=Config(F,20,4);
        V:=MalformedNumber(I);
        case J of
          0:C.QuantumTicks:=MalformedNumber(I);
          1:C.SegmentCellCount:=MalformedNumber(I);
          2:C.VoiceCount:=MalformedNumber(I);
          3:C.StepsPerOctave:=MalformedNumber(I);
          4:C.RequestedTicks:=MalformedNumber(I);
          5:C.Seed:=MalformedNumber(I);
          6:C.Search.SolveOptions.MaxBacktracks:=MalformedNumber(I);
          7:C.Search.MaxPassBacktracks:=MalformedNumber(I);
          8:asm C.HarmonyMode = V; end;
          9:asm C.Rounding = V; end;
        end;
        BadConfig(C,'malformed browser config '+IntToStr(I)+'/'+IntToStr(J));
      end;
    C:=Config(F,20,4); P:=TWfcMusicEnsembleStream.Create(C);
    try
      for I:=0 to 3 do
      begin
        V:=MalformedNumber(I);
        asm L = V; end;
        Raised:=False;
        try P.ClearAllowedTokens(L,0);
        except on EWfcMusicEnsembleStream do Raised:=True end;
        Check(Raised and (P.ProducedTicks=0) and (Length(P.CopyConstraints)=0),
          'malformed browser layer rejects without mutation');
      end;
    finally P.Free end;
  finally Done(F) end;
end;
{$ENDIF}

procedure TestHooks;
var F:TFixture; C:TWfcMusicEnsembleStreamConfig; P:TProbeStream;
  S:TWfcMusicEnsembleSegment; R:TGraphNegotiationReport; M:Integer; Raised:Boolean;
begin
  Init(F,4); C:=Config(F,12,4);
  try
    for M:=1 to 6 do
    begin
      P:=TProbeStream.Create(C); P.Mode:=M;
      try
        if M=5 then P.IntersectAllowedTokens(wmelHarmony,0,nil);
        Raised:=False;
        try
          case M of
            1: Check(P.Next(S,R)=wmaspCancelled,'hook cancellation yields no candidate');
            2,3: begin
              Check(P.Next(S,R)=wmaspProduced,'rejected reentry/edit leaves outer call valid');
              S.Free; Check(P.Rejected,'hook reentry/edit is rejected');
            end;
          else P.Next(S,R);
          end;
        except on E:Exception do begin
          Raised:=True;
          if M=5 then Check(Pos('global constraint',E.Message)>0,
            'stored mask proof survives hook domain erasure');
        end end;
        if M>=4 then Check(Raised and (S=nil) and
          (P.ProducedTicks=0) and (P.Status=wmasFailed),'hook fault cannot publish fabricated result');
      finally P.Free end;
    end;
  finally Done(F) end;
end;

begin
  Run('continuity and exact frontier',TestContinuousFrontier);
  Run('sized segments and ownership',TestSizedSegmentsAndCopies);
  Run('sparse constraints and terminal failure',TestConstraintsAndFailure);
  Run('allowed harmony across held chord seams',TestAllowedHarmonyContinuation);
  Run('capacity and endpoint policy',TestCapacityAndBoundaries);
  Run('application hook isolation',TestHooks);
  {$IFDEF PAS2JS}Run('malformed browser numbers',TestHostNumbers);{$ENDIF}
  WriteLn('Ensemble stream checks: ',Checks,', failures: ',Failures);
  if Failures<>0 then Halt(1);
end.
