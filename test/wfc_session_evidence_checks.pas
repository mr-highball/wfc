{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Test-only ordered reader: consumes every complete evidence line and
  compares decoded values with actual captured owners. No writer internals. }
unit wfc_session_evidence_checks;
{$mode delphi}{$H+}
interface
uses wfc_pipeline_session;
var EvidenceDocuments: Integer;
procedure CheckOutcomeEvidence(const O: TWfcPipelineSessionOutcome);
procedure CheckEditEvidence(const O: TWfcPipelineSessionEditOutcome);
procedure CheckInvalidEvidenceInputs;
procedure CheckEvidenceEnvelopeRejections(const O: TWfcPipelineSessionOutcome;
  const E: TWfcPipelineSessionEditOutcome);
implementation
uses SysUtils,wfc,wfc_model,wfc_lattice,wfc_pipeline_run,wfc_pipeline_compile,
  wfc_pipeline_prepare,wfc_pipeline_session_evidence,wfc_text_codec,
  wfc_session_oracle_helpers {$IFDEF PAS2JS},Web{$ENDIF};
type
  TReader = class
    Lines: TWfcTextLines;
    Cursor: Integer;
    constructor Create(const Text: String);
    function Take(const Key: String): String;
    procedure TextField(const Key, Expected: String);
    procedure Number(const Key: String; const Expected: Integer);
    procedure Unsigned(const Key: String; const Expected: Cardinal);
    procedure Flag(const Key: String; const Expected: Boolean);
    procedure CSV(const Key: String; const Expected: array of String);
    procedure Ints(const Key: String; const Expected: array of Integer);
    procedure Indices(const Key: String; const Expected: TGraphPassIndices);
    procedure Finish;
  end;
function Limits: TWfcPipelineSessionEvidenceLimits;
begin Result.Version:=1; Result.MaxTextBytes:=4000000; Result.MaxLines:=100000; end;
function N(const V: Integer): String;
begin Result:=IntToStr(V); end;
function U(const V: Cardinal): String;
begin Result:=UIntToStr(V); end;
function T(const V: TWfcModelToken): String;
begin Result:=WfcTextEncodeToken(V,'independent evidence check'); end;
function G(const V: String): String;
begin
  {$IFDEF PAS2JS}Result:=T(V);{$ELSE}Result:=T(UTF8Encode(UnicodeString(V)));{$ENDIF}
end;
constructor TReader.Create(const Text: String);
var I: Integer;
begin
  inherited Create;
  WfcTextSplitCanonicalLines(Text,'private evidence',Lines);
  Check(Length(Text)>0,'evidence nonempty');
  Check(Text[Length(Text)]=#10,'canonical final LF');
  for I:=1 to Length(Text) do Check(Ord(Text[I])<128,'evidence ASCII byte');
end;
function TReader.Take(const Key: String): String;
var S: String;
begin
  Check(Cursor<Length(Lines),'evidence line present '+Key);
  S:=Lines[Cursor]; Inc(Cursor);
  Check(Copy(S,1,Length(Key)+1)=Key+'=','evidence exact next field '+Key);
  Result:=Copy(S,Length(Key)+2,Length(S));
end;
procedure TReader.TextField(const Key,Expected: String);
begin Check(Take(Key)=Expected,'evidence value '+Key); end;
procedure TReader.Number(const Key: String; const Expected: Integer);
begin Check(WfcTextParseCanonicalSignedInteger(Take(Key),Key,'evidence')=Expected,'evidence integer '+Key); end;
procedure TReader.Unsigned(const Key: String; const Expected: Cardinal);
begin Check(WfcTextParseCanonicalCardinal(Take(Key),Key,'evidence')=Expected,'evidence Cardinal '+Key); end;
procedure TReader.Flag(const Key: String; const Expected: Boolean);
begin Number(Key,Ord(Expected)); end;
procedure TReader.CSV(const Key: String; const Expected: array of String);
var S,Part: String; I,Start,Stop: Integer;
begin
  S:=Take(Key); Start:=1;
  for I:=0 to High(Expected) do
  begin
    Stop:=Start; while (Stop<=Length(S)) and (S[Stop]<>',') do Inc(Stop);
    Part:=Copy(S,Start,Stop-Start);
    Check(Part=Expected[I],'evidence tuple '+Key+' item '+N(I));
    if I<High(Expected) then Check(Stop<=Length(S),'tuple separator '+Key)
    else Check(Stop=Length(S)+1,'no extra tuple items '+Key);
    Start:=Stop+1;
  end;
end;
procedure TReader.Ints(const Key: String; const Expected: array of Integer);
var Values: array of String; I: Integer;
begin
  SetLength(Values,Length(Expected));
  for I:=0 to High(Expected) do Values[I]:=N(Expected[I]); CSV(Key,Values);
end;
procedure TReader.Indices(const Key: String; const Expected: TGraphPassIndices);
var I: Integer;
begin Number(Key+'.count',Length(Expected)); for I:=0 to High(Expected) do Number(Key+'.'+N(I),Expected[I]); end;
procedure TReader.Finish;
begin Check(Cursor=Length(Lines),'every evidence line consumed, no hidden extras'); end;
procedure CommonHeader(const R: TReader; const Kind: String);
begin
  R.Number('wfc-session-evidence',1); R.TextField('kind',Kind);
  R.Number('session-version',WFC_PIPELINE_SESSION_VERSION);
  R.Number('trace-version',WFC_TRACE_VERSION); R.Number('trace-hash-version',WFC_TRACE_HASH_VERSION);
  R.Number('negotiation-hash-version',WFC_PASS_NEGOTIATION_HASH_VERSION);
  R.Number('selective-hash-version',WFC_SELECTIVE_NEGOTIATION_HASH_VERSION);
end;
procedure Invocation(const R: TReader; const V: TWfcPipelineSessionInvocation);
var I,J: Integer; Prefix: String;
begin
  R.Number('invocation.format',V.FormatVersion); R.Unsigned('invocation.recipe-signature',V.RecipeSignature);
  R.Unsigned('invocation.seed',V.Seed); R.Number('invocation.strategy',Ord(V.Strategy));
  R.Number('invocation.local-budget',V.MaxBacktracks); R.Number('invocation.pass-budget',V.MaxPassBacktracks);
  R.Flag('invocation.capture',V.CaptureTrace); R.Number('invocation.topology-count',Length(V.Topologies));
  for I:=0 to High(V.Topologies) do with V.Topologies[I] do
    R.Ints('invocation.topology.'+N(I),[Rank,Ord(Wrap),Origin.X,Origin.Y,Origin.Z,Pitch.X,Pitch.Y,Pitch.Z]);
  R.Number('invocation.extent-count',Length(V.Extents));
  for I:=0 to High(V.Extents) do with V.Extents[I] do R.Ints('invocation.extent.'+N(I),[X,Y,Z]);
  R.Number('invocation.lock-count',Length(V.Locks));
  for I:=0 to High(V.Locks) do with V.Locks[I] do
    R.CSV('invocation.lock.'+N(I),[N(PassIndex),N(X),N(Y),N(Z),T(Token)]);
  R.Number('invocation.domain-count',Length(V.Domains));
  for I:=0 to High(V.Domains) do with V.Domains[I] do
  begin
    Prefix:='invocation.domain.'+N(I); R.Ints(Prefix,[PassIndex,X,Y,Z,Length(AllowedTokens)]);
    for J:=0 to High(AllowedTokens) do
      Check(WfcTextDecodeToken(R.Take(Prefix+'.token.'+N(J)),'evidence')=AllowedTokens[J],'decoded invocation domain token');
  end;
end;
procedure PublicState(const R: TReader; const S: TWfcPipelineSessionPublicState);
var I,J: Integer; L: TWfcPipelineSessionLayer; Prefix: String;
begin
  R.Number('state.layer-count',S.LayerCount);
  for I:=0 to S.LayerCount-1 do
  begin
    L:=S.LayerAt(I); Prefix:='state.layer.'+N(I);
    R.CSV(Prefix,[N(L.PassIndex),N(L.Rank),T(L.LabelName)]);
    with L.Layout do R.Ints(Prefix+'.layout',[Cells.X,Cells.Y,Cells.Z,Origin.X,Origin.Y,Origin.Z,Pitch.X,Pitch.Y,Pitch.Z,Ord(Wrap)]);
    R.Number(Prefix+'.cell-count',Length(L.Cells));
    for J:=0 to High(L.Cells) do with L.Cells[J] do
      R.CSV(Prefix+'.cell.'+N(J),[N(Ord(Empty)),N(Ord(Generated)),T(Token)]);
  end;
end;
procedure Scope(const R: TReader; const V: TWfcPipelineSessionScope);
begin
  R.Number('scope.version',V.ScopeAlgorithmVersion);
  R.Indices('scope.roots',V.RequestedRootIndices); R.Indices('scope.active',V.ActivePassIndices);
  R.Indices('scope.authored',V.AuthoredPassIndices); R.Indices('scope.required',V.RequiredPassIndices);
  R.Indices('scope.missing',V.MissingPassIndices);
end;
procedure Solve(const R: TReader; const Prefix: String; const S: TGraphSolveReport);
var I: Integer;
begin
  R.Number(Prefix+'.status',Ord(S.Status)); R.Unsigned(Prefix+'.seed',S.Seed);
  R.Ints(Prefix+'.versions',[S.RandomAlgorithmVersion,S.SolverAlgorithmVersion,S.GraphModelVersion,S.PipelineAlgorithmVersion]);
  R.Number(Prefix+'.failed-pass',S.FailedPassIndex);
  with S.Contradiction do R.Ints(Prefix+'.contradiction',[Ord(Kind),PassIndex,EntryIndex,NeighborIndex,Ord(HasDirection),Ord(Direction),DependencyPassIndex,ConstraintIndex]);
  R.Number(Prefix+'.pass-count',Length(S.Passes));
  for I:=0 to High(S.Passes) do with S.Passes[I] do
    R.Ints(Prefix+'.pass.'+N(I),[Decisions,Propagations,Contradictions,Backtracks,ExcludedAssignments,Ord(Executed),ExecutionOrdinal,Ord(Disposition),TraceStart,TraceCount]);
  R.Indices(Prefix+'.order',S.ExecutionOrder); R.Flag(Prefix+'.trace-captured',S.TraceCaptured);
  R.Unsigned(Prefix+'.trace-hash',S.TraceHash); R.Number(Prefix+'.trace-count',Length(S.Trace));
  for I:=0 to High(S.Trace) do with S.Trace[I] do
    R.CSV(Prefix+'.event.'+N(I),[N(EventId),N(CauseEventId),N(Ord(Kind)),N(Ord(CauseKind)),N(PassIndex),N(EntryIndex),N(ValueIndex),G(Value),N(NeighborIndex),N(Ord(HasDirection)),N(Ord(Direction)),N(DependencyPassIndex),N(DecisionDepth),N(DomainCountBefore),N(DomainCountAfter),N(ConstraintIndex)]);
  with S.TraceDelivery do R.CSV(Prefix+'.delivery',[N(Version),N(Ord(Status)),N(ProducedEventCount),N(DeliveredEventCount),U(TraceHash),N(Ord(FailurePhase)),N(FailureEventId),G(FailureMessage)]);
end;
procedure Negotiation(const R: TReader; const Prefix: String; const V: TGraphNegotiationReport);
var I,J: Integer; Path: String;
begin
  R.Number(Prefix+'.status',Ord(V.Status)); R.Unsigned(Prefix+'.seed',V.Seed);
  R.Number(Prefix+'.version',V.NegotiationAlgorithmVersion); R.Number(Prefix+'.pass-backtracks',V.PassBacktracks);
  R.Unsigned(Prefix+'.transcript-hash',V.TranscriptHash); R.Number(Prefix+'.attempt-count',Length(V.Attempts));
  for I:=0 to High(V.Attempts) do with V.Attempts[I] do
  begin
    Path:=Prefix+'.attempt.'+N(I); R.Ints(Path+'.backtracked',[BacktrackedPassIndex,BacktrackedExecutionOrdinal]);
    R.Number(Path+'.assignment-count',Length(ExcludedAssignment));
    for J:=0 to High(ExcludedAssignment) do R.Number(Path+'.assignment.'+N(J),ExcludedAssignment[J]);
    Solve(R,Path+'.solve',SolveReport);
  end;
  Solve(R,Prefix+'.final',V.FinalReport);
end;
procedure CaptureDocument(const Text: String);
{$IFDEF PAS2JS}var E: TJSElement;{$ENDIF}
begin
  Inc(EvidenceDocuments);
  {$IFDEF PAS2JS}
  E:=document.createElement('pre'); E.id:='evidence-'+N(EvidenceDocuments);
  E.setAttribute('class','complete-session-evidence'); E.textContent:=Text;
  document.body.appendChild(E);
  {$ELSE}
  WriteLn('EVIDENCE-BEGIN ',EvidenceDocuments); Write(Text); WriteLn('EVIDENCE-END ',EvidenceDocuments);
  {$ENDIF}
end;
procedure CheckOutcomeEvidence(const O: TWfcPipelineSessionOutcome);
const Names: array[TWfcPipelineSessionOutcomeKind] of String =
  ('ordinary-full','negotiated-full','ordinary-selective','negotiated-selective');
var Text,Again: String; L: TWfcPipelineSessionEvidenceLimits; R: TReader;
  State: TWfcPipelineSessionPublicState; Q: TGraphSelectiveNegotiationReport;
  V: TWfcPipelineCommitValidation; Rejected: Boolean;
begin
  L:=Limits; Text:=EncodeWfcPipelineSessionOutcomeEvidence(O,L); R:=TReader.Create(Text);
  try
    CommonHeader(R,Names[O.Kind]); R.Number('revision',O.Revision); R.Flag('solved',O.Solved);
    R.Flag('current',O.HasCurrentOutput); R.Flag('baseline',O.HasSuccessfulBaseline);
    Invocation(R,O.CopyInvocation); Scope(R,O.CopyScope); R.Indices('pending',O.CopyPendingPassIndices); R.Indices('authored',O.CopyAuthoredPassIndices);
    State:=O.CopyPublicState; try PublicState(R,State); finally State.Free; end;
    V:=O.LastValidation; R.Ints('validation',[Ord(V.Kind),V.PassIndex,V.BridgeIndex,V.RequirementIndex,V.ValueQuotaIndex,V.ConnectivityIndex,V.EntryIndex]);
    case O.Kind of
      wpsokOrdinaryFull,wpsokOrdinarySelective:Solve(R,'solve',O.CopySolveReport);
      wpsokNegotiatedFull:Negotiation(R,'search',O.CopyNegotiationReport);
      wpsokNegotiatedSelective:begin
        Q:=O.CopySelectiveNegotiationReport; R.Number('selection.version',Q.ScopeAlgorithmVersion);
        R.Indices('selection.roots',Q.RequestedRootIndices); R.Indices('selection.active',Q.ActivePassIndices);
        R.Unsigned('selection.transcript-hash',Q.TranscriptHash); Negotiation(R,'selection.search',Q.Search);
      end;
    end;
    R.Finish; L.MaxTextBytes:=Length(Text); L.MaxLines:=Length(R.Lines);
    Again:=EncodeWfcPipelineSessionOutcomeEvidence(O,L); Check(Text=Again,'exact byte and line envelopes accepted');
    Dec(L.MaxTextBytes); Rejected:=False;
    try Again:=EncodeWfcPipelineSessionOutcomeEvidence(O,L); except on E: EWfcPipelineSessionEvidence do Rejected:=True; end;
    Check(Rejected,'one fewer outcome byte rejected'); Inc(L.MaxTextBytes); Dec(L.MaxLines); Rejected:=False;
    try Again:=EncodeWfcPipelineSessionOutcomeEvidence(O,L); except on E: EWfcPipelineSessionEvidence do Rejected:=True; end;
    Check(Rejected,'one fewer outcome line rejected');
    Check(Text=EncodeWfcPipelineSessionOutcomeEvidence(O,Limits),'outcome unchanged after failed encoding');
    CaptureDocument(Text);
  finally R.Free; end;
end;
procedure CheckEditEvidence(const O: TWfcPipelineSessionEditOutcome);
var Text,Again: String; L: TWfcPipelineSessionEvidenceLimits; R: TReader;
  State: TWfcPipelineSessionPublicState; Impact: TWfcPipelineInputImpact; Rejected: Boolean;
begin
  L:=Limits; Text:=EncodeWfcPipelineSessionEditEvidence(O,L); R:=TReader.Create(Text);
  try
    CommonHeader(R,'edit'); R.Number('revision',O.Revision); R.Flag('current',O.HasCurrentOutput); R.Flag('baseline',O.HasSuccessfulBaseline);
    Invocation(R,O.CopyInvocation); R.Indices('pending',O.CopyPendingPassIndices); R.Indices('authored',O.CopyAuthoredPassIndices);
    Impact:=O.CopyImpact; R.Flag('impact.authored-changed',Impact.AuthoredInputsChanged); R.Flag('impact.graph-changed',Impact.GraphInputsChanged);
    R.Indices('impact.authored',Impact.AuthoredPassIndices); R.Indices('impact.changed',Impact.ChangedPassIndices);
    State:=O.CopyPublicState; try PublicState(R,State); finally State.Free; end; R.Finish;
    L.MaxTextBytes:=Length(Text); L.MaxLines:=Length(R.Lines);
    Again:=EncodeWfcPipelineSessionEditEvidence(O,L); Check(Text=Again,'exact edit byte and line envelopes accepted');
    Dec(L.MaxTextBytes); Rejected:=False;
    try Again:=EncodeWfcPipelineSessionEditEvidence(O,L); except on E: EWfcPipelineSessionEvidence do Rejected:=True; end;
    Check(Rejected,'one fewer edit byte rejected'); Inc(L.MaxTextBytes); Dec(L.MaxLines); Rejected:=False;
    try Again:=EncodeWfcPipelineSessionEditEvidence(O,L); except on E: EWfcPipelineSessionEvidence do Rejected:=True; end;
    Check(Rejected,'one fewer edit line rejected');
    Check(Text=EncodeWfcPipelineSessionEditEvidence(O,Limits),'edit unchanged after failed encoding');
    CaptureDocument(Text);
  finally R.Free; end;
end;
procedure CheckInvalidEvidenceInputs;
var L: TWfcPipelineSessionEvidenceLimits; Text: String; Rejected: Boolean; I: Integer;
begin
  for I:=0 to 3 do
  begin
    L:=Limits;
    case I of 1:L.Version:=2; 2:L.MaxTextBytes:=0; 3:L.MaxLines:=-1; end;
    Rejected:=False; try Text:=EncodeWfcPipelineSessionOutcomeEvidence(nil,L); except on E: EWfcPipelineSessionEvidence do Rejected:=True; end;
    Check(Rejected,'nil outcome or invalid envelope rejected');
    Rejected:=False; try Text:=EncodeWfcPipelineSessionEditEvidence(nil,L); except on E: EWfcPipelineSessionEvidence do Rejected:=True; end;
    Check(Rejected,'nil edit or invalid envelope rejected');
  end;
end;
procedure CheckEvidenceEnvelopeRejections(const O: TWfcPipelineSessionOutcome;
  const E: TWfcPipelineSessionEditOutcome);
var L: TWfcPipelineSessionEvidenceLimits; Text: String; Rejected: Boolean; I: Integer;
  {$IFDEF PAS2JS}
  procedure CheckRawCase(const CaseIndex: Integer);
  var RawLimits: TWfcPipelineSessionEvidenceLimits; GetterCalls: Integer;
    RawText: String; RawRejected: Boolean;
  begin
    { A malformed plain object/array has no Pascal record $assign method.
      Each case must get a fresh typed local before replacing its raw shape. }
    RawLimits:=Limits; GetterCalls:=0;
    asm
      switch (CaseIndex) {
        case 0: RawLimits={Version:1,MaxTextBytes:1000}; break;
        case 1: RawLimits.MaxLines=1.5; break;
        case 2: RawLimits.MaxLines=Infinity; break;
        case 3: RawLimits.MaxTextBytes=2147483648; break;
        case 4: RawLimits=[]; break;
        case 5: Object.defineProperty(RawLimits,'MaxLines',{get:function(){GetterCalls++; return 10000;}}); break;
      }
    end;
    RawRejected:=False;
    try RawText:=EncodeWfcPipelineSessionOutcomeEvidence(O,RawLimits);
    except on X: EWfcPipelineSessionEvidence do RawRejected:=True; end;
    Check(RawRejected and (GetterCalls=0),'passive raw JS limit check before access');
    RawRejected:=False;
    try RawText:=EncodeWfcPipelineSessionEditEvidence(E,RawLimits);
    except on X: EWfcPipelineSessionEvidence do RawRejected:=True; end;
    Check(RawRejected and (GetterCalls=0),'passive raw JS edit limit check before access');
  end;
  {$ENDIF}
begin
  for I:=0 to 5 do
  begin
    L:=Limits;
    case I of 0:L.Version:=0; 1:L.Version:=2; 2:L.MaxTextBytes:=0;
      3:L.MaxLines:=0; 4:L.MaxTextBytes:=-1; 5:L.MaxLines:=-1; end;
    Rejected:=False; try Text:=EncodeWfcPipelineSessionOutcomeEvidence(O,L); except on X: EWfcPipelineSessionEvidence do Rejected:=True; end;
    Check(Rejected,'actual outcome rejects invalid limit field');
    Rejected:=False; try Text:=EncodeWfcPipelineSessionEditEvidence(E,L); except on X: EWfcPipelineSessionEvidence do Rejected:=True; end;
    Check(Rejected,'actual edit rejects invalid limit field');
  end;
  {$IFDEF PAS2JS}
  for I:=0 to 5 do CheckRawCase(I);
  {$ENDIF}
end;
end.
