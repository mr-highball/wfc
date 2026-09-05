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
program wfc_music_ensemble_graph_test;
{$mode delphi}{$H+}
uses SysUtils, wfc, wfc_model, wfc_music, wfc_music_sequence,
  wfc_music_ensemble, wfc_music_ensemble_graph, wfc_sequence,
  wfc_sequence_learn, wfc_sequence_graph
  {$IFDEF PAS2JS}, wfc_browser_test_host{$ENDIF};

var Checks, Failures: Integer;
procedure Check(const AValue: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not AValue then begin Inc(Failures); WriteLn('[FAIL] ', AMessage) end;
end;

function Frame(const AHold, ASilent: Boolean; const ALead: Integer):
  TWfcMusicEnsembleFrame;
var V: TWfcMusicVoiceCells; T: TWfcMusicTones; A: TWfcMusicCellAction;
begin
  SetLength(V, 2);
  if ASilent then
  begin V[0] := MakeWfcMusicRestVoiceCell; V[1] := MakeWfcMusicRestVoiceCell end
  else
  begin
    if AHold then A := wmcaHold else A := wmcaAttack;
    SetLength(T, 2);
    T[0] := MakeWfcMusicTone(48,80); T[1] := MakeWfcMusicTone(55,70);
    V[0] := MakeWfcMusicVoiceCell(A,T);
    SetLength(T,1); T[0] := MakeWfcMusicTone(ALead,90);
    V[1] := MakeWfcMusicVoiceCell(wmcaAttack,T);
  end;
  Result := MakeWfcMusicEnsembleFrame(V);
end;

function Frames: TWfcMusicEnsembleFrames;
begin
  Result := nil;
  SetLength(Result,3);
  Result[0] := Frame(False,False,64);
  Result[1] := Frame(True,False,65);
  Result[2] := Frame(False,True,0);
end;

procedure ExpectModelRejected(const M: TWfcSequenceModel; const Voices: Integer;
  const Msg: String);
var Rejected: Boolean;
begin
  Rejected := False;
  try ValidateWfcMusicEnsembleModel(M, Voices)
  except on E: Exception do Rejected := True end;
  Check(Rejected,Msg);
end;

function NewGraph(const E,R,H: TWfcSequenceModel): TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Seed := 0; Result.Reshape(3,1,1); Result.WrapNeighbors := False;
    Result.CurrentPass := 'h'; Result.PassMode := gpmOverlay;
    Result.ClearDependencies; ApplySequenceModelToGraph(H,Result,wseWhole);
    Result.SwitchToPass('r'); Result.PassMode := gpmOverlay;
    Result.ClearDependencies; ApplySequenceModelToGraph(R,Result,wseWhole);
    Result.SwitchToPass('e'); Result.PassMode := gpmOverlay;
    Result.ClearDependencies; ApplySequenceModelToGraph(E,Result,wseWhole);
  except Result.Free; raise end;
end;

procedure TestModelsAndRelations;
var
  F,Bad: TWfcMusicEnsembleFrames;
  E,R,H,P,Invalid: TWfcSequenceModel;
  Sets: TWfcMusicPitchClassSets;
  PC: TWfcMusicPitchClasses;
  Rules, ExactRules: TWfcSequenceProjectionRules;
  I: Integer;
  Rejected: Boolean;
begin
  F := Frames;
  E := LearnSequenceModel(EncodeWfcMusicEnsembleFrames(F),2);
  R := LearnSequenceModel(EncodeWfcMusicRhythmFrames(
    ProjectWfcMusicEnsembleFramesToRhythm(F)),2);
  H := LearnSequenceModel(EncodeWfcMusicPitchClassSets(
    ProjectWfcMusicEnsembleFramesToPitchClassSets(F,12)),2);
  P := nil; Invalid := nil;
  try
    ValidateWfcMusicEnsembleModel(E,2);
    Check(True,'order-two model preserves independent bass holds');
    ValidateWfcMusicEnsembleRhythmModel(R,2);
    ValidateWfcMusicEnsembleHarmonyModel(H,12);
    Rules := BuildWfcMusicEnsembleRhythmProjectionRules(E,R);
    Check(Length(Rules)=E.PublicTokenCount,'rhythm maps cover vocabulary');
    for I := 0 to High(Rules) do
      Check((Length(Rules[I].SourceTokens)=1) and
        (Rules[I].SourceTokens[0]=EncodeWfcMusicRhythmFrame(
          ProjectWfcMusicEnsembleFrameToRhythm(
            DecodeWfcMusicEnsembleFrame(Rules[I].TargetToken)))),
        'rhythm vector preserves each voice action');
    ExactRules := BuildWfcMusicEnsembleExactHarmonyProjectionRules(E,H,12);
    for I := 0 to High(ExactRules) do
      Check(Length(ExactRules[I].SourceTokens)=1,'exact union has one exact provider');
    SetLength(Sets,2); SetLength(PC,4);
    PC[0]:=0; PC[1]:=4; PC[2]:=5; PC[3]:=7;
    Sets[0]:=MakeWfcMusicPitchClassSet(12,PC); PC:=nil;
    Sets[1]:=MakeWfcMusicPitchClassSet(12,PC);
    P := LearnSequenceModel(EncodeWfcMusicPitchClassSets(Sets),1);
    Rules := BuildWfcMusicEnsembleAllowedHarmonyProjectionRules(E,P,12);
    for I := 0 to High(Rules) do
      if Rules[I].TargetToken=EncodeWfcMusicEnsembleFrame(F[2]) then
        Check(Length(Rules[I].SourceTokens)=2,'silence fits every allowed palette')
      else
        Check(Length(Rules[I].SourceTokens)=1,'sound requires palette membership');
    Rejected:=False;
    try BuildWfcMusicEnsembleExactHarmonyProjectionRules(E,P,12)
    except on Exception do Rejected:=True end;
    Check(Rejected,'allowed superset is not an exact sounding set');
    Rejected:=False;
    try ValidateWfcMusicEnsembleHarmonyModel(H,19)
    except on Exception do Rejected:=True end;
    Check(Rejected,'tuning mismatch rejected');
    ExpectModelRejected(E,3,'voice arity mismatch rejected');
    Invalid:=LearnSequenceModel(EncodeWfcMusicEnsembleFrames(F),1);
    ExpectModelRejected(Invalid,2,'order-one hold edges rejected');
    FreeAndNil(Invalid);
    Bad:=nil; SetLength(Bad,2); Bad[0]:=F[0]; Bad[1]:=F[2];
    Invalid:=LearnSequenceModel(EncodeWfcMusicEnsembleFrames(Bad),1);
    ValidateWfcMusicEnsembleModel(Invalid,2);
    Check(True,'order-one rest and attack vocabulary is valid');
    FreeAndNil(Invalid);
    SetLength(Bad,1); Bad[0]:=F[1];
    Invalid:=LearnSequenceModel(EncodeWfcMusicEnsembleFrames(Bad),2);
    ExpectModelRejected(Invalid,2,'observed initial hold rejected');
    FreeAndNil(Invalid);
    Bad:=DecodeWfcMusicEnsembleFrames(EncodeWfcMusicEnsembleFrames(F));
    Bad[1].Voices[0].Tones[0].Velocity:=81;
    Invalid:=LearnSequenceModel(EncodeWfcMusicEnsembleFrames(Bad),2);
    ExpectModelRejected(Invalid,2,'hold velocity mismatch edge rejected');
  finally Invalid.Free; P.Free; H.Free; R.Free; E.Free end;
end;

procedure TestAtomicBindingAndSolve;
var
  F: TWfcMusicEnsembleFrames;
  E,R,H: TWfcSequenceModel;
  G: TGraph;
  O: TGraphSolveOptions;
  Report: TGraphSolveReport;
  Capture: TWfcSequenceGraphValidationReport;
  Generated: TWfcGeneratedSequence;
  Rejected: Boolean;
  I: Integer;
  B: TWfcSequenceProjectionBindings;
begin
  F:=Frames;
  E:=LearnSequenceModel(EncodeWfcMusicEnsembleFrames(F),2);
  R:=LearnSequenceModel(EncodeWfcMusicRhythmFrames(
    ProjectWfcMusicEnsembleFramesToRhythm(F)),2);
  H:=LearnSequenceModel(EncodeWfcMusicPitchClassSets(
    ProjectWfcMusicEnsembleFramesToPitchClassSets(F,12)),2);
  G:=nil;
  try
    G:=NewGraph(E,R,H);
    Check(G.DependencyCount=0,'target begins without dependencies');
    Rejected:=False;
    try RequireWfcMusicEnsembleFromPasses(E,R,H,G,'r','missing',2,12,wmehmExact)
    except on Exception do Rejected:=True end;
    Check(Rejected and (G.DependencyCount=0),'late invalid provider is atomic');
    Rejected:=False;
    try RequireWfcMusicEnsembleFromPasses(E,R,H,G,'r','r',2,12,wmehmExact)
    except on Exception do Rejected:=True end;
    Check(Rejected and (G.DependencyCount=0),'duplicate providers rejected atomically');
    RequireWfcMusicEnsembleFromPasses(E,R,H,G,'r','h',2,12,wmehmExact);
    Check(G.DependencyCount=2,'distinct providers are both required');
    O:=DefaultGraphSolveOptions; O.CaptureTrace:=True;
    Check(G.TrySolve(O,Report),'bound ensemble graph solves');
    Check(CaptureSolvedSequence(E,G,wseWhole,Generated,Capture),'latent capture validates');
    Check(Length(Generated.Tokens)=3,'capture has synchronized frame count');
    for I:=0 to High(F) do
      Check(Generated.Tokens[I]=EncodeWfcMusicEnsembleFrame(F[I]),
        'generated frame matches provider-constrained witness');
    G.Free; G:=NewGraph(E,R,H);
    G.SwitchToPass('r2'); G.PassMode:=gpmOverlay;
    G.ClearDependencies; ApplySequenceModelToGraph(R,G,wseWhole);
    G.SwitchToPass('e');
    SetLength(B,3);
    B[0]:=MakeWfcSequenceProjectionBinding(R,'r',
      BuildWfcMusicEnsembleRhythmProjectionRules(E,R));
    B[1]:=MakeWfcSequenceProjectionBinding(H,'h',
      BuildWfcMusicEnsembleExactHarmonyProjectionRules(E,H,12));
    B[2]:=MakeWfcSequenceProjectionBinding(R,'r2',
      BuildWfcMusicEnsembleRhythmProjectionRules(E,R));
    RequireSequenceProjectionMapsFromPasses(E,G,B);
    Check(G.DependencyCount=3,'generic N-source bundle accepts third provider');
    Check(G.TrySolve(O,Report),'three-source ensemble solve succeeds');
  finally G.Free; H.Free; R.Free; E.Free end;
end;

begin
  try
    TestModelsAndRelations;
    TestAtomicBindingAndSolve;
  except on E: Exception do begin Inc(Failures); WriteLn('[FAIL] ',E.ClassName,': ',E.Message) end end;
  WriteLn('Checks: ',Checks,', Failures: ',Failures);
  if Failures<>0 then Halt(1);
end.
