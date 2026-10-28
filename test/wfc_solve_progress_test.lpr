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
program wfc_solve_progress_test;
{$mode delphi}{$H+}
uses
  { The maintained browser harness supplies its output/exit adapter. The same
    core fixture also executes directly under Node with WFC_NODE_TEST defined. }
  {$IFDEF PAS2JS}{$IFNDEF WFC_NODE_TEST}wfc_browser_test_host,{$ENDIF}{$ENDIF}
  SysUtils, wfc, wfc_solve_progress;
const Cells=256;
type
  { Each public solver family must carry the same call-scoped observer. }
  TMode=(smPlain,smNegotiated,smRestarted,smNegotiatedRestarted,
    smSelective,smSelectiveNegotiated);
  { The original exception is the cancellation token, never a contradiction. }
  TStopPoint=(spNone,spStart,spInterior,spFinal);
  { Also verifies observable counts while retaining no graph ownership. }
  TObserver=class
  public
    Graph:TGraph;
    Calls:Integer;
    Last:TWfcSolveProgress;
    StopPhase:TWfcSolvePhase;
    StopPoint:TStopPoint;
    Marker:Exception;
    SawCommitFinal:Boolean;
    AttemptStarts:Integer;
    StopAfterFirstAttempt:Boolean;
    procedure Observe(const AWork:TWfcSolveProgress);
  end;
  { Diagnostic lifecycle remains complete when a separate work receiver cancels. }
  TCheckedSink=class(TGraphTraceSink)
  public
    Begins,Events,Ends:Integer;
    FailBegin:Boolean;
    procedure BeginTrace(const AHeader:TGraphTraceHeader);override;
    procedure AppendEvent(const AEvent:TGraphTraceEvent);override;
    procedure EndTrace(const ADelivery:TGraphTraceDelivery);override;
  end;
var Checks,Aborts:Integer;
procedure Check(const ACondition:Boolean; const AMessage:String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;
procedure TObserver.Observe(const AWork:TWfcSolveProgress);
var Stop:Boolean;
begin
  Check(Graph.Running,'Callback outside guarded solve');
  Check(not SawCommitFinal,'Callback after final acceptance boundary');
  Check((AWork.Done>=0) and (AWork.Total>=0),'Negative work');
  Check((AWork.PassIndex>=-1) and (AWork.PassIndex<3),'Invalid pass');
  if AWork.KnownTotal then Check(AWork.Done=AWork.Total,'False completion total');
  if (Calls>0) and (AWork.Phase=Last.Phase) and
    (AWork.PassIndex=Last.PassIndex) and (AWork.Done<>0) then
  begin
    Check(AWork.Done>=Last.Done,'Work decreased inside phase');
    Check(AWork.Done-Last.Done<=256,'Unreported visit interval');
  end;
  if (AWork.Phase=wspPrepare) and (AWork.PassIndex=-1) and
    (AWork.Done=0) and not AWork.KnownTotal then Inc(AttemptStarts);
  Inc(Calls);
  Last:=AWork;
  Stop:=False;
  case StopPoint of
    spStart: Stop:=(AWork.Done=0) and not AWork.KnownTotal;
    spInterior: Stop:=(AWork.Done>0) and not AWork.KnownTotal;
    spFinal: Stop:=AWork.KnownTotal;
  end;
  if (AWork.Phase=wspCommit) and AWork.KnownTotal then SawCommitFinal:=True;
  if (AWork.Phase=StopPhase) and Stop and
    (not StopAfterFirstAttempt or (AttemptStarts>1)) then
  begin
    Marker:=Exception.Create('original cancellation');
    raise Marker;
  end;
end;
function BuildGraph:TGraph;
begin
  Result:=TGraph.Create;
  Result.Seed:=731;
  Result.Reshape(Cells,1,1);
  Result.WrapNeighbors:=False;
  Result.CurrentPass:='base';
  Result.PassMode:=gpmOverlay;
  Result.AddValue('a');
  Result.AddValue('b');
  Result.SetAllowedValues(Cells-1,0,0,['b']);
  Result[0,0,0].Value:='a';
  Result.SwitchToPass('leaf');
  Result.PassMode:=gpmOverlay;
  Result.AddValue('x').RequireFromPass('base','a');
  Result.AddValue('y').RequireFromPass('base','b');
  Result.SwitchToPass('stable');
  Result.PassMode:=gpmOverlay;
  Result.AddValue('s');
end;
function Snapshot(const G:TGraph; const Count:Integer=Cells;
  const Passes:Integer=3):String;
var P,I,J:Integer; V:TGraphValues; E:TGraphEntry;
begin
  Result:=IntToStr(G.CurrentPassIndex)+'|';
  for P:=0 to Passes-1 do
    for I:=0 to Count-1 do
    begin
      E:=G.PassGraph[P].Entry[I,0,0];
      Result:=Result+E.Value+IntToStr(Ord(E.Empty))+IntToStr(Ord(E.Generated))+':';
      Result:=Result+IntToStr(Ord(G.PassGraph[P].HasAllowedValues(I,0,0)));
      V:=G.PassGraph[P].CopyAllowedValues(I,0,0);
      for J:=0 to High(V) do Result:=Result+V[J]+',';
      Result:=Result+';';
    end;
end;
procedure Prepare(const G:TGraph; const Mode:TMode);
var R:TGraphSolveReport;
begin
  if Mode in [smSelective,smSelectiveNegotiated] then
    Check(G.TrySolve(DefaultGraphSolveOptions,R),'Initial accepted solve failed');
end;
function Run(const G:TGraph; const Mode:TMode; const Observer:TWfcSolveObserver;
  out Signature:String):Boolean;
var O:TGraphSolveOptions; N:TGraphNegotiationOptions; X:TGraphRestartOptions;
    R:TGraphSolveReport; NR:TGraphNegotiationReport; RR:TGraphRestartReport;
    SR:TGraphSelectiveNegotiationReport;
begin
  O:=DefaultGraphSolveOptions;
  O.CaptureTrace:=True;
  N:=DefaultGraphNegotiationOptions;
  N.SolveOptions:=O;
  X:=DefaultGraphRestartOptions;
  case Mode of
    smPlain: Result:=G.TrySolve(O,R,Observer);
    smNegotiated:
    begin
      Result:=G.TrySolveNegotiated(N,NR,Observer);
      R:=NR.FinalReport;
    end;
    smRestarted:
    begin
      Result:=G.TrySolveRestarted(O,X,RR,Observer);
      R:=RR.FinalReport;
    end;
    smNegotiatedRestarted:
    begin
      Result:=G.TrySolveNegotiatedRestarted(N,X,RR,Observer);
      R:=RR.FinalReport;
    end;
    smSelective: Result:=G.TryRegenerateFrom('base',O,R,Observer);
    smSelectiveNegotiated:
    begin
      Result:=G.TryRegenerateNegotiatedFrom('base',N,SR,Observer);
      R:=SR.Search.FinalReport;
    end;
  end;
  Signature:=IntToStr(Ord(R.Status))+'|'+IntToHex(R.TraceHash,8)+'|'+
    IntToStr(Length(R.Trace))+'|'+IntToStr(R.FailedPassIndex);
end;
procedure CheckReplay(const Mode:TMode);
var A,B:TGraph; Obs:TObserver; SA,SB:String; I:Integer;
begin
  A:=BuildGraph; B:=BuildGraph; Obs:=TObserver.Create;
  try
    Prepare(A,Mode); Prepare(B,Mode);
    Obs.Graph:=B;
    Check(Run(A,Mode,nil,SA),'Nil solve failed');
    Check(Run(B,Mode,Obs.Observe,SB),'Observed solve failed');
    Check(Snapshot(A)=Snapshot(B),'Observed graph differs');
    Check(SA=SB,'Observed trace/report differs');
    Check((Obs.Calls>12) and Obs.SawCommitFinal,'Incomplete phase observation');
    for I:=0 to 2 do Check(A.PassGraph[I].RandomIndex(100000)=
      B.PassGraph[I].RandomIndex(100000),'Observed RNG differs');
    WriteLn('replay mode=',Ord(Mode),' ',SA,' callbacks=',Obs.Calls);
  finally
    Obs.Free; A.Free; B.Free;
  end;
end;
procedure CheckAbort(const Mode:TMode; const Phase:TWfcSolvePhase;
  const Point:TStopPoint);
var A,B:TGraph; Obs:TObserver; Before,SA,SB:String; Aborted:Boolean; I:Integer;
begin
  A:=BuildGraph; B:=BuildGraph; Obs:=TObserver.Create;
  try
    Prepare(A,Mode); Prepare(B,Mode);
    Before:=Snapshot(A);
    Obs.Graph:=A; Obs.StopPhase:=Phase; Obs.StopPoint:=Point;
    Aborted:=False;
    try
      Run(A,Mode,Obs.Observe,SA);
    except
      on E:Exception do
      begin
        Check(E=Obs.Marker,'Original observer exception changed');
        Obs.Marker:=nil;
        Aborted:=True;
      end;
    end;
    Check(Aborted,'Requested abort point not reached');
    Inc(Aborts);
    Check(not A.Running,'Running state stranded');
    Check(Snapshot(A)=Before,'Abort changed accepted entry/domain/pass state');
    for I:=0 to 2 do Check(A.PassGraph[I].RandomIndex(100000)=
      B.PassGraph[I].RandomIndex(100000),'Abort changed random stream');
    Check(Run(A,Mode,nil,SA) and Run(B,Mode,nil,SB),'Retry failed');
    Check((Snapshot(A)=Snapshot(B)) and (SA=SB),'Retry differs from untouched control');
  finally
    Obs.Free; A.Free; B.Free;
  end;
end;
function BuildRounds(const Negotiated:Boolean):TGraph;
begin
  Result:=TGraph.Create;
  Result.Seed:=0;
  if Negotiated then
  begin
    Result.Reshape(1,1,1);
    Result.WrapNeighbors:=False;
    Result.CurrentPass:='terrain';
    Result.PassMode:=gpmOverlay;
    Result.AddValue('marsh');
    Result.AddValue('meadow');
    Result.SwitchToPass('housing');
    Result.PassMode:=gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('cottage').RequireFromPass('terrain','meadow');
  end
  else
  begin
    Result.Reshape(3,1,1);
    Result.WrapNeighbors:=True;
    Result.AddValue('A').NewRule([gdEast,gdWest],'B');
    Result.AddValue('C').NewRule([gdEast,gdWest],'C');
  end;
end;
function RunRounds(const G:TGraph; const Negotiated:Boolean;
  const Observer:TWfcSolveObserver; out Transcript:TGraphTraceSignature;
  out Rounds:Integer):Boolean;
var S:TGraphSolveOptions; N:TGraphNegotiationOptions; X:TGraphRestartOptions;
    R:TGraphRestartReport; NR:TGraphNegotiationReport;
begin
  S:=DefaultGraphSolveOptions;
  S.MaxBacktracks:=0;
  S.CaptureTrace:=True;
  if Negotiated then
  begin
    N:=DefaultGraphNegotiationOptions;
    N.SolveOptions:=S;
    N.SolveOptions.MaxBacktracks:=1;
    N.MaxPassBacktracks:=1;
    Result:=G.TrySolveNegotiated(N,NR,Observer);
    Transcript:=NR.TranscriptHash;
    Rounds:=NR.PassBacktracks;
  end
  else
  begin
    X:=DefaultGraphRestartOptions;
    X.MaxRestarts:=32;
    Result:=G.TrySolveRestarted(S,X,R,Observer);
    Transcript:=R.TranscriptHash;
    Rounds:=R.Restarts;
  end;
end;
procedure CheckRounds(const Negotiated,Cancel:Boolean);
var A,B:TGraph; Obs:TObserver; TA,TB:TGraphTraceSignature;
    RA,RB,Count,Passes:Integer; Before:String; Aborted:Boolean;
begin
  A:=BuildRounds(Negotiated); B:=BuildRounds(Negotiated); Obs:=TObserver.Create;
  try
    Count:=3; Passes:=1;
    if Negotiated then begin Count:=1; Passes:=2; end;
    Obs.Graph:=A;
    Before:=Snapshot(A,Count,Passes);
    if Cancel then
    begin
      Obs.StopAfterFirstAttempt:=True;
      Obs.StopPhase:=wspPrepare;
      Obs.StopPoint:=spStart;
      Aborted:=False;
      try
        RunRounds(A,Negotiated,Obs.Observe,TA,RA);
      except
        on E:Exception do
        begin
          Check(E=Obs.Marker,'Later-round cancellation identity changed');
          Obs.Marker:=nil;
          Aborted:=True;
        end;
      end;
      Check(Aborted and (Obs.AttemptStarts=2),'Second attempt did not abort');
      Inc(Aborts);
      Check(not A.Running,'Later-round cancellation stranded Running');
      Check(Snapshot(A,Count,Passes)=Before,'Later-round cancellation changed state');
      Check(RunRounds(A,Negotiated,nil,TA,RA),'Later-round retry failed');
    end
    else Check(RunRounds(A,Negotiated,Obs.Observe,TA,RA),'Observed rounds failed');
    Check(RunRounds(B,Negotiated,nil,TB,RB),'Nil rounds failed');
    Check((RA>0) and (RA=RB) and (TA=TB),'Round policy/transcript differs');
    Check(Snapshot(A,Count,Passes)=Snapshot(B,Count,Passes),'Round replay differs');
    if not Negotiated then Check((RA=3) and (TA=$36DF8F92),
      'Frozen original restart transcript changed');
    WriteLn('rounds negotiated=',Negotiated,' cancelled=',Cancel,
      ' retries=',RA,' transcript=',IntToHex(TA,8));
  finally
    Obs.Free; A.Free; B.Free;
  end;
end;
procedure TCheckedSink.BeginTrace(const AHeader:TGraphTraceHeader);
begin
  Check((Begins=0) and (Events=0) and (Ends=0),'Repeated trace header');
  Inc(Begins);
  if FailBegin then raise Exception.Create('contained diagnostic failure');
end;
procedure TCheckedSink.AppendEvent(const AEvent:TGraphTraceEvent);
begin
  Check((Begins=1) and (Ends=0),'Trace event outside header/footer');
  Inc(Events);
end;
procedure TCheckedSink.EndTrace(const ADelivery:TGraphTraceDelivery);
begin
  Check((Begins=1) and (Ends=0),'Trace footer without header');
  Check(ADelivery.Status=gtdsInterrupted,'Cancellation footer not interrupted');
  Check(ADelivery.DeliveredEventCount=Events,'Trace footer count differs');
  Inc(Ends);
end;
procedure CheckTraceCancellation(const FailBegin:Boolean);
var G:TGraph; Obs:TObserver; Sink:TCheckedSink; R:TGraphSolveReport;
    Stopped:Boolean; Before:String;
begin
  G:=BuildGraph; Obs:=TObserver.Create; Sink:=TCheckedSink.Create;
  try
    Before:=Snapshot(G);
    Obs.Graph:=G;
    Obs.StopPhase:=wspPrepare;
    Obs.StopPoint:=spStart;
    Sink.FailBegin:=FailBegin;
    G.TraceSink:=Sink;
    Stopped:=False;
    try
      G.TrySolve(DefaultGraphSolveOptions,R,Obs.Observe);
    except
      on E:Exception do
      begin
        Check(E=Obs.Marker,'Trace delivery replaced original cancellation');
        Obs.Marker:=nil;
        Stopped:=True;
        Inc(Aborts);
      end;
    end;
    Check(Stopped and (Obs.Calls=1) and not G.Running,'Initial cancellation lost');
    Check(Snapshot(G)=Before,'Trace/work cancellation published graph state');
    Check(Sink.Begins=1,'Cancellation skipped trace header');
    if FailBegin then
    begin
      Check((Sink.Events=0) and (Sink.Ends=0) and
        (R.TraceDelivery.Status=gtdsSinkFailed) and
        (R.TraceDelivery.FailurePhase=gtdpBegin),'Contained trace failure changed');
    end
    else
    begin
      Check((Sink.Events=1) and (Sink.Ends=1) and
        (R.TraceDelivery.Status=gtdsInterrupted),'Interrupted trace lifecycle differs');
    end;
  finally
    G.TraceSink:=nil;
    G.Free; Obs.Free; Sink.Free;
  end;
end;
var Mode:TMode; Phase:TWfcSolvePhase; Point:TStopPoint;
begin
  for Mode:=Low(TMode) to High(TMode) do
  begin
    CheckReplay(Mode);
    for Phase:=Low(TWfcSolvePhase) to High(TWfcSolvePhase) do
      for Point:=spStart to spFinal do
      begin
        WriteLn('abort mode=',Ord(Mode),' phase=',Ord(Phase),' point=',Ord(Point));
        CheckAbort(Mode,Phase,Point);
      end;
  end;
  CheckRounds(False,False);
  CheckRounds(False,True);
  CheckRounds(True,False);
  CheckRounds(True,True);
  CheckTraceCancellation(False);
  CheckTraceCancellation(True);
  WriteLn('PASS checks=',Checks,' original_aborts=',Aborts);
end.