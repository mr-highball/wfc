{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Browser DOM/file glue only. The shared controller owns all live history. }
unit browser_pipeline_workspace_app;
{$mode delphi}{$H+}
interface
uses JS,Web,WebOrWorker,SysUtils,wfc,wfc_model,wfc_lattice,wfc_sequence,
  wfc_pipeline_layout,wfc_pipeline_model,wfc_pipeline_run,wfc_pipeline_session,
  wfc_pipeline_workspace_context,wfc_pipeline_workspace_journal,
  wfc_pipeline_workspace_replay,pipeline_workspace_workbench,
  pipeline_workspace_presets,pipeline_workspace_view;
type
  TWorkspaceUiRequest = record
    Action,RecipeText,RunText,JournalText,PolicyKey,Preset: String;
    Expected,PassIndex,X,Y,Z: Integer;
    Token: TWfcModelToken;
    Allowed: TWfcModelTokens;
    Roots: TGraphPassIndices;
    Policy: TWfcPipelineWorkspacePolicy;
    RunOptions: TWfcWorkspacePresetRunOptions;
    Weights: TWfcWorkspaceLandscapeWeights;
    SequenceOptions: TWfcWorkspaceSequenceOptions;
    Topologies: TWfcPipelinePassTopologies;
    Extents: TWfcPipelinePassExtents;
    Slice: TWfcPipelineWorkspaceSlice;
  end;
  TBrowserPipelineWorkspaceApplication = class
  private
    FWorkbench: TWfcPipelineWorkspaceWorkbench;
    FPreview: TWfcPipelineWorkspaceRepairPreview;
    FPreviewPolicyKey,FPreset,FGeometryLiveRecipe,FRootRecipe: String;
    FRecipeUrl,FRunUrl,FJournalUrl,FSvgUrl: String;
    FGeometryRows,FOperationTimer,FReadLimit: Integer;
    FBusy,FBound,FDisposed,FInitialAttempted,FNeedsRecovery,FGeometryMapped: Boolean;
    FReader: TJSFileReader;
    FReadTarget: String;
    FPending: TWorkspaceUiRequest;
    function Element(const Id: String): TJSElement;
    function Input(const Id: String): TJSHTMLInputElement;
    function Choice(const Id: String): TJSHTMLSelectElement;
    function Area(const Id: String): TJSHTMLTextAreaElement;
    function Decimal(const Text,Name: String; const Maximum: Cardinal): Cardinal;
    function Number(const Id: String; const Minimum: Integer=0): Integer;
    function Signed(const Id: String): Integer;
    function Encoded(const Token: TWfcModelToken): String;
    function ReadPolicy(out Key: String): TWfcPipelineWorkspacePolicy;
    function ContextLimits(const P: TWfcPipelineWorkspacePolicy): TWfcPipelineWorkspaceContextLimits;
    function ReadRunOptions: TWfcWorkspacePresetRunOptions;
    function ReadRoots: TGraphPassIndices;
    function ReadDomain: TWfcModelTokens;
    function ReadSlice: TWfcPipelineWorkspaceSlice;
    function AppliedText: String;
    function ScopeText(const S: TWfcPipelineSessionScope): String;
    function Indices(const Values: TGraphPassIndices): String;
    procedure ReadGeometry(out Topologies: TWfcPipelinePassTopologies; out Extents: TWfcPipelinePassExtents);
    procedure PopulateGeometry(const M: TWfcPipelineModel; const R: TWfcPipelineRun; const Live: Boolean);
    procedure PopulatePolicy;
    procedure PopulateLivePasses;
    procedure PopulateTokens(const ResetWindow: Boolean);
    procedure SetButtons;
    procedure SetStatus(const Text: String);
    procedure ShowError(const E: Exception);
    procedure InvalidatePreview;
    procedure InvalidateView;
    procedure MarkDraft(const Kind: String);
    procedure Revoke(const Id: String; var Url: String);
    procedure Download(const Id,Text,Name,Mime: String; var Url: String);
    procedure CancelPending;
    procedure CancelReader;
    procedure Queue(const Action: String);
    procedure ExecutePending;
    procedure Perform(const Q: TWorkspaceUiRequest);
    procedure SyncLive;
    procedure ShowReceipt(const R: TWfcPipelineWorkspaceReceipt);
    procedure ShowDefinition(const Contexts: TWfcPipelineWorkspaceContexts);
    procedure Render(const Slice: TWfcPipelineWorkspaceSlice);
    procedure InspectCell(const Q: TWorkspaceUiRequest);
    procedure BeginRead(const Id: String);
    function HandleClick(Event: TJSMouseEvent): Boolean;
    function HandleInput(Event: TJSEvent): Boolean;
    function HandleChange(Event: TJSEvent): Boolean;
    function HandleRead(Event: TJSEvent): Boolean;
    function HandleReadError(Event: TJSEvent): Boolean;
    function HandleRuntimeError(Event: TJSEvent): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
    { Read-only event-test seam. Tests drive the same visible DOM controls. }
    property IsBusy: Boolean read FBusy;
  end;
implementation
uses wfc_text_codec,wfc_pipeline_text,wfc_pipeline_run_text,
  wfc_pipeline_workspace_journal_text;

const
  PolicyIds: array[0..20] of String = ('recipes','runs','context-bytes','actions',
    'roots','evidence-bytes','journal-bytes','input-cells','input-values','input-visits',
    'public-cells','token-bytes','report-passes','trace-events','excluded-values',
    'outcome-bytes','outcome-lines','epochs','solves','instantiated-cells','replay-evidence-bytes');
  PolicyLabels: array[0..20] of String = ('Journal recipe rows','Journal run rows',
    'Canonical context bytes','Journal actions','Journal root references','Journal evidence bytes',
    'Encoded journal bytes','Replacement cell records','Replacement value items',
    'Replacement candidate visits','Captured public cells','Captured encoded token bytes',
    'Captured report pass rows','Captured trace events','Captured excluded assignments',
    'One encoded outcome bytes','One encoded outcome lines','Replay epochs','Replay solve actions',
    'Replay logical cell instances','Replay actual evidence bytes');
  PolicyDefaults: array[0..20] of Integer = (1024,16384,67108864,16384,65536,
    67108864,268435456,1048576,4194304,64000000,1048576,16777216,65536,
    1048576,4194304,33554432,1048576,1024,16384,16777216,67108864);

procedure UiError(const Detail: String);
begin raise EWfcPipelineWorkspaceWorkbench.Create(Detail); end;

constructor TBrowserPipelineWorkspaceApplication.Create;
begin inherited Create; FWorkbench:=TWfcPipelineWorkspaceWorkbench.Create; end;

destructor TBrowserPipelineWorkspaceApplication.Destroy;
var ErrorHandler: WebOrWorker.TJSEventHandler;
begin
  FDisposed:=True; CancelPending; CancelReader;
  if FBound then
  begin
    TJSHTMLElement(Element('workbench')).onclick:=nil;
    TJSHTMLElement(Element('workbench')).oninput:=nil;
    TJSHTMLElement(Element('workbench')).onchange:=nil;
    ErrorHandler:=@HandleRuntimeError; window.removeEventListener('error',ErrorHandler);
    Revoke('recipe-download',FRecipeUrl); Revoke('run-download',FRunUrl);
    Revoke('journal-download',FJournalUrl); Revoke('svg-download',FSvgUrl);
  end;
  FPreview.Free; FWorkbench.Free; inherited Destroy;
end;

function TBrowserPipelineWorkspaceApplication.Element(const Id: String): TJSElement;
begin Result:=document.getElementById(Id); if Result=nil then UiError('Missing editor element: '+Id); end;
function TBrowserPipelineWorkspaceApplication.Input(const Id: String): TJSHTMLInputElement;
begin Result:=TJSHTMLInputElement(Element(Id)); end;
function TBrowserPipelineWorkspaceApplication.Choice(const Id: String): TJSHTMLSelectElement;
begin Result:=TJSHTMLSelectElement(Element(Id)); end;
function TBrowserPipelineWorkspaceApplication.Area(const Id: String): TJSHTMLTextAreaElement;
begin Result:=TJSHTMLTextAreaElement(Element(Id)); end;
function TBrowserPipelineWorkspaceApplication.Encoded(const Token: TWfcModelToken): String;
begin Result:=WfcTextEncodeToken(Token,'workspace UI token'); end;

function TBrowserPipelineWorkspaceApplication.Decimal(const Text,Name: String;
  const Maximum: Cardinal): Cardinal;
var I: Integer; D: Cardinal; S: String;
begin
  Result:=0; S:=Trim(Text); if S='' then UiError(Name+' requires decimal digits');
  for I:=1 to Length(S) do
  begin
    if not (S[I] in ['0'..'9']) then UiError(Name+' requires decimal digits');
    D:=Ord(S[I])-Ord('0');
    if (D>Maximum) or (Result>(Maximum-D) div 10) then UiError(Name+' exceeds the supported Integer range');
    Result:=Result*10+D;
  end;
end;
function TBrowserPipelineWorkspaceApplication.Number(const Id: String; const Minimum: Integer): Integer;
begin Result:=Integer(Decimal(Input(Id).value,Id,High(Integer))); if Result<Minimum then UiError(Id+' must be at least '+IntToStr(Minimum)); end;
function TBrowserPipelineWorkspaceApplication.Signed(const Id: String): Integer;
var S: String; N: Cardinal;
begin
  S:=Trim(Input(Id).value);
  if (S<>'') and (S[1]='-') then
  begin
    N:=Decimal(Copy(S,2,Length(S)),Id,Cardinal(High(Integer))+1);
    if N=Cardinal(High(Integer))+1 then Result:=Low(Integer) else Result:=-Integer(N);
  end else Result:=Integer(Decimal(S,Id,High(Integer)));
end;

procedure TBrowserPipelineWorkspaceApplication.PopulatePolicy;
var I: Integer; LabelNode: TJSElement; N: TJSHTMLInputElement;
begin
  for I:=0 to High(PolicyIds) do
  begin
    LabelNode:=document.createElement('label'); LabelNode.textContent:=PolicyLabels[I];
    N:=TJSHTMLInputElement(document.createElement('input')); N.id:='policy-'+PolicyIds[I];
    N.value:=IntToStr(PolicyDefaults[I]); N.setAttribute('inputmode','numeric');
    LabelNode.appendChild(N); Element('policy-fields').appendChild(LabelNode);
  end;
end;

function TBrowserPipelineWorkspaceApplication.ReadPolicy(out Key: String): TWfcPipelineWorkspacePolicy;
var V: array[0..20] of Integer; I: Integer;
begin
  Key:='';
  for I:=0 to High(V) do begin V[I]:=Number('policy-'+PolicyIds[I],1); Key:=Key+IntToStr(V[I])+','; end;
  Result:=Default(TWfcPipelineWorkspacePolicy); Result.Version:=1;
  with Result.Journal do begin Version:=1; MaxRecipes:=V[0]; MaxRuns:=V[1];
    MaxContextTextBytes:=V[2]; MaxActions:=V[3]; MaxRootReferences:=V[4];
    MaxEvidenceTextBytes:=V[5]; MaxEncodedTextBytes:=V[6]; end;
  with Result.Replacement do begin Version:=1; MaxRetainedCellRecords:=V[7];
    MaxRetainedValueItems:=V[8]; MaxCandidateVisits:=V[9]; end;
  with Result.Outcome do begin Version:=1; MaxPublicCellRecords:=V[10];
    MaxEncodedTokenBytes:=V[11]; MaxReportPassRecords:=V[12]; MaxTraceEvents:=V[13];
    MaxExcludedAssignmentItems:=V[14]; end;
  with Result.Evidence do begin Version:=1; MaxTextBytes:=V[15]; MaxLines:=V[16]; end;
  with Result.Replay do begin Version:=1; MaxEpochs:=V[17]; MaxSolveActions:=V[18];
    MaxInstantiatedCellRecords:=V[19]; MaxEvidenceTextBytes:=V[20]; end;
end;

function TBrowserPipelineWorkspaceApplication.ContextLimits(const P: TWfcPipelineWorkspacePolicy): TWfcPipelineWorkspaceContextLimits;
begin Result.Version:=1; Result.MaxRecipes:=P.Journal.MaxRecipes; Result.MaxRuns:=P.Journal.MaxRuns; Result.MaxTextBytes:=P.Journal.MaxContextTextBytes; end;

function TBrowserPipelineWorkspaceApplication.ReadRunOptions: TWfcWorkspacePresetRunOptions;
begin
  Result.Seed:=Decimal(Input('seed').value,'Seed',High(Cardinal));
  if Choice('strategy').value='one-way' then Result.Strategy:=wpssOneWay
  else if Choice('strategy').value='negotiated' then Result.Strategy:=wpssNegotiated
  else UiError('Choose a supported strategy');
  Result.MaxBacktracks:=Number('backtracks'); Result.MaxPassBacktracks:=Number('pass-backtracks');
  Result.CaptureTrace:=Input('capture-trace').checked;
end;

function TBrowserPipelineWorkspaceApplication.ReadRoots: TGraphPassIndices;
var Nodes: TJSNodeList; I,N: Integer; C: TJSHTMLInputElement;
begin
  Result:=nil; N:=0; Nodes:=Element('repair-roots').querySelectorAll('input[type=checkbox]');
  for I:=0 to Nodes.length-1 do
  begin
    C:=TJSHTMLInputElement(Nodes[I]); if not C.checked then Continue;
    SetLength(Result,N+1); Result[N]:=Integer(Decimal(C.value,'Root index',High(Integer))); Inc(N);
  end;
  if N=0 then UiError('Select at least one explicit repair root');
end;
function TBrowserPipelineWorkspaceApplication.ReadDomain: TWfcModelTokens;
var Nodes: TJSNodeList; I,N: Integer; C: TJSHTMLInputElement;
begin
  Result:=nil; N:=0; Nodes:=Element('domain-tokens').querySelectorAll('input[type=checkbox]');
  for I:=0 to Nodes.length-1 do
  begin
    C:=TJSHTMLInputElement(Nodes[I]); if not C.checked then Continue;
    SetLength(Result,N+1); Result[N]:=WfcTextDecodeToken(C.value,'domain token'); Inc(N);
  end;
end;
function TBrowserPipelineWorkspaceApplication.ReadSlice: TWfcPipelineWorkspaceSlice;
begin
  Result.Version:=1; Result.PassIndex:=Integer(Decimal(Choice('view-pass').value,'Public pass',High(Integer)));
  Result.StartX:=Number('view-x'); Result.StartY:=Number('view-y'); Result.SliceZ:=Number('view-z');
  Result.Width:=Number('view-width',1); Result.Height:=Number('view-height',1);
  Result.CellPixels:=Number('view-pixels',1); Result.MaxRenderedCells:=Number('view-cells',1);
  Result.MaxSvgBytes:=Number('view-bytes',1);
  if (Choice('view-state').value<>'baseline') and (Choice('view-state').value<>'current') then
    UiError('Choose current state or historical baseline');
  Result.UseBaseline:=Choice('view-state').value='baseline';
end;
function TBrowserPipelineWorkspaceApplication.AppliedText: String;
var R: TWfcPipelineRun;
begin R:=FWorkbench.CopyAppliedRun; try Result:=EncodeWfcPipelineRunText(R); finally R.Free; end; end;
function TBrowserPipelineWorkspaceApplication.Indices(const Values: TGraphPassIndices): String;
var I: Integer;
begin Result:='['; for I:=0 to High(Values) do begin if I>0 then Result:=Result+', '; Result:=Result+IntToStr(Values[I]); end; Result:=Result+']'; end;
function TBrowserPipelineWorkspaceApplication.ScopeText(const S: TWfcPipelineSessionScope): String;
begin
  Result:='Requested roots: '+Indices(S.RequestedRootIndices)+#10+
    'Active closure: '+Indices(S.ActivePassIndices)+#10+'Authored passes: '+Indices(S.AuthoredPassIndices)+#10+
    'Required passes: '+Indices(S.RequiredPassIndices)+#10+'Missing permission: '+Indices(S.MissingPassIndices);
end;

procedure TBrowserPipelineWorkspaceApplication.ReadGeometry(out Topologies: TWfcPipelinePassTopologies;
  out Extents: TWfcPipelinePassExtents);
var I: Integer; Prefix: String;
begin
  if FGeometryRows=0 then UiError('Load a complete geometry table first');
  SetLength(Topologies,FGeometryRows); SetLength(Extents,FGeometryRows);
  for I:=0 to FGeometryRows-1 do
  begin
    Prefix:='geometry-'+IntToStr(I)+'-';
    Topologies[I]:=MakeWfcPipelinePassTopology(Number(Prefix+'rank',1),
      MakeWfcLatticeVector(Signed(Prefix+'ox'),Signed(Prefix+'oy'),Signed(Prefix+'oz')),
      MakeWfcLatticeVector(Number(Prefix+'px',1),Number(Prefix+'py',1),Number(Prefix+'pz',1)),
      Input(Prefix+'wrap').checked);
    Extents[I]:=MakeWfcLatticeVector(Number(Prefix+'x',1),Number(Prefix+'y',1),Number(Prefix+'z',1));
  end;
end;

procedure TBrowserPipelineWorkspaceApplication.PopulateGeometry(const M: TWfcPipelineModel;
  const R: TWfcPipelineRun; const Live: Boolean);
var I: Integer; Row,CellNode,Group: TJSElement; C: TJSHTMLInputElement;
  Prefix: String; T: TWfcPipelinePassTopology; L: TWfcLatticeLayout;
  procedure Field(const Parent: TJSElement; const Suffix: String; const Value: Integer);
  var N: TJSHTMLInputElement;
  begin
    N:=TJSHTMLInputElement(document.createElement('input')); N.id:=Prefix+Suffix;
    N.value:=IntToStr(Value); N.setAttribute('aria-label','Pass '+IntToStr(I)+' '+Suffix);
    N.setAttribute('inputmode','numeric'); Parent.appendChild(N);
  end;
  procedure Triple(const A,B,CName: String; const V: TWfcLatticeVector);
  begin
    CellNode:=document.createElement('td'); Group:=document.createElement('div'); Group.className:='triple';
    Field(Group,A,V.X); Field(Group,B,V.Y); Field(Group,CName,V.Z);
    CellNode.appendChild(Group); Row.appendChild(CellNode);
  end;
begin
  Element('geometry-rows').textContent:=''; FGeometryRows:=M.PassCount; FGeometryMapped:=M.HasPassMapping;
  FGeometryLiveRecipe:=''; if Live then FGeometryLiveRecipe:=EncodeWfcPipelineModelText(M);
  for I:=0 to M.PassCount-1 do
  begin
    Prefix:='geometry-'+IntToStr(I)+'-'; T:=R.PassTopologyAt(I); L:=R.PassLayoutAt(I);
    Row:=document.createElement('tr'); CellNode:=document.createElement('td');
    CellNode.textContent:=IntToStr(I)+' / '+Encoded(M.PassAt(I).LabelName); Row.appendChild(CellNode);
    CellNode:=document.createElement('td'); CellNode.className:='rank'; Field(CellNode,'rank',T.Rank); Row.appendChild(CellNode);
    Triple('x','y','z',L.Cells); Triple('ox','oy','oz',T.Origin); Triple('px','py','pz',T.Pitch);
    CellNode:=document.createElement('td'); C:=TJSHTMLInputElement(document.createElement('input'));
    C._type:='checkbox'; C.id:=Prefix+'wrap'; C.checked:=T.Wrap;
    C.setAttribute('aria-label','Pass '+IntToStr(I)+' wrap'); CellNode.appendChild(C); Row.appendChild(CellNode);
    Element('geometry-rows').appendChild(Row);
  end;
  if Live then Element('geometry-status').textContent:='Form is based on the current live recipe. Editing does not apply until Begin mapped epoch.'
  else Element('geometry-status').textContent:='Draft geometry. Use Rebuild selected preset to stage it, or Load live geometry before changing an existing epoch.';
end;

procedure TBrowserPipelineWorkspaceApplication.Revoke(const Id: String; var Url: String);
begin
  if Url<>'' then TJSURL.revokeObjectURL(Url); Url:='';
  Element(Id).removeAttribute('href'); Element(Id).removeAttribute('download'); Element(Id).setAttribute('aria-disabled','true');
end;
procedure TBrowserPipelineWorkspaceApplication.Download(const Id,Text,Name,Mime: String; var Url: String);
var Parts: TJSArray; Options: TJSBlobInit; LBlob: TJSBlob; A: TJSHTMLAnchorElement;
begin
  Revoke(Id,Url); Parts:=TJSArray.new; Parts.push(Text); Options:=TJSBlobInit.new;
  Options.type_:=Mime; LBlob:=TJSBlob.new(Parts,Options); Url:=TJSURL.createObjectURL(LBlob);
  A:=TJSHTMLAnchorElement(Element(Id)); A.href:=Url; A.download:=Name; A.setAttribute('aria-disabled','false');
end;
procedure TBrowserPipelineWorkspaceApplication.InvalidatePreview;
begin
  FreeAndNil(FPreview); FPreviewPolicyKey:='';
  Element('scope-report').textContent:='No current repair preview. Preview the selected roots and current policy again.';
end;
procedure TBrowserPipelineWorkspaceApplication.InvalidateView;
begin
  Revoke('svg-download',FSvgUrl); Element('map-output').textContent:='';
  Element('view-status').textContent:='View settings or live state changed. Render the exact window to refresh this diagnostic.';
end;
procedure TBrowserPipelineWorkspaceApplication.MarkDraft(const Kind: String);
begin
  if Kind='journal' then
  begin
    Element('journal-status').textContent:='Journal draft changed; its claims have not been inspected or replayed.';
    Element('journal-report').textContent:='No inspection for the current journal draft.';
  end
  else
  begin
    Revoke('recipe-download',FRecipeUrl); Revoke('run-download',FRunUrl);
    Element('draft-status').textContent:='Recipe/run drafts changed and are not applied. Inspect or execute an explicit action.';
    Element('definition-report').textContent:='No inspection for the current definition drafts.';
  end;
end;
procedure TBrowserPipelineWorkspaceApplication.SetStatus(const Text: String);
begin
  Element('status').textContent:=Text; Element('error').textContent:=''; Element('error').setAttribute('hidden','');
  if FBusy then document.body.setAttribute('data-state','busy') else document.body.setAttribute('data-state','ready');
end;
procedure TBrowserPipelineWorkspaceApplication.ShowError(const E: Exception);
var Detail: String;
begin
  Detail:=E.ClassName+': '+E.Message;
  if E is EWfcPipelineWorkspaceReplay then
    Detail:=Detail+#10+'Action index: '+IntToStr(EWfcPipelineWorkspaceReplay(E).ActionIndex)+
      '; first differing evidence byte: '+IntToStr(EWfcPipelineWorkspaceReplay(E).MismatchOffset);
  Element('error').textContent:=Detail; Element('error').removeAttribute('hidden');
  Element('status').textContent:='Action not completed. Drafts and live history are separate; no automatic retry was made.';
  if FNeedsRecovery then Element('status').textContent:='Presentation recovery required. Use Replace drafts with live recipe / run to inspect the authoritative publication.';
  document.body.setAttribute('data-state','error');
end;

procedure TBrowserPipelineWorkspaceApplication.SetButtons;
const LiveIds: array[0..14] of String = ('copy-live','apply-run','initial','live-geometry',
  'copy-journal','save-journal','render','set-lock','clear-lock','set-domain','clear-domain',
  'inspect-cell','preview-repair','repair','mapped-epoch');
var Nodes: TJSNodeList; I: Integer; E: TJSElement; Live,CanEdit,HasPublic: Boolean;
  procedure Disable(const Id: String; const Disabled: Boolean);
  begin
    if Disabled then Element(Id).setAttribute('disabled','') else Element(Id).removeAttribute('disabled');
  end;
begin
  if not FBound then Exit;
  Nodes:=Element('workbench').querySelectorAll('input,textarea,select,button');
  for I:=0 to Nodes.length-1 do
  begin E:=TJSElement(Nodes[I]); if FBusy then E.setAttribute('disabled','') else E.removeAttribute('disabled'); end;
  Live:=FWorkbench.HasExecution; CanEdit:=Live and not FNeedsRecovery;
  HasPublic:=Live and (Choice('view-pass').value<>'');
  for I:=0 to High(LiveIds) do Disable(LiveIds[I],FBusy or not CanEdit);
  Disable('copy-live',FBusy or not Live); Disable('save-journal',FBusy or not Live);
  Disable('copy-journal',FBusy or not Live);
  Disable('initial',FBusy or not CanEdit or FInitialAttempted);
  Disable('mapped-epoch',FBusy or not CanEdit or not FGeometryMapped or (FGeometryLiveRecipe='') or
    (FGeometryLiveRecipe<>FRootRecipe));
  Disable('preset-rebuild',FBusy or (FPreset='') or (FGeometryRows=0));
  Disable('view-pass',FBusy or not HasPublic); Disable('cell-token',FBusy or not CanEdit or not HasPublic);
  Disable('render',FBusy or not CanEdit or not HasPublic);
  Disable('set-lock',FBusy or not CanEdit or not HasPublic); Disable('clear-lock',FBusy or not CanEdit or not HasPublic);
  Disable('set-domain',FBusy or not CanEdit or not HasPublic); Disable('clear-domain',FBusy or not CanEdit or not HasPublic);
  Disable('inspect-cell',FBusy or not CanEdit or not HasPublic);
  Disable('repair',FBusy or not CanEdit or (FPreview=nil));
  if FPreview<>nil then Disable('repair',FBusy or not CanEdit or not FPreview.CanExecute);
  Disable('cancel-operation',(FOperationTimer=0) and (FReader=nil));
  Element('workbench').setAttribute('aria-busy',LowerCase(BoolToStr(FBusy,True)));
end;

procedure TBrowserPipelineWorkspaceApplication.PopulateLivePasses;
var M: TWfcPipelineModel; I: Integer; Previous,Text,RecipeText: String;
  OptionNode: TJSHTMLOptionElement; LabelNode: TJSElement; C: TJSHTMLInputElement;
begin
  M:=FWorkbench.CopyCurrentRecipe;
  try
    RecipeText:=EncodeWfcPipelineModelText(M);
    if RecipeText=FRootRecipe then begin PopulateTokens(False); Exit; end;
    Previous:=Choice('view-pass').value; Choice('view-pass').textContent:='';
    Element('repair-roots').textContent:='';
    for I:=0 to M.PassCount-1 do
    begin
      Text:=IntToStr(I)+' / '+Encoded(M.PassAt(I).LabelName);
      if M.PassAt(I).Visibility=wppvPublic then
      begin
        OptionNode:=TJSHTMLOptionElement(document.createElement('option'));
        OptionNode.value:=IntToStr(I); OptionNode.textContent:=Text; Choice('view-pass').appendChild(OptionNode);
      end;
      LabelNode:=document.createElement('label'); C:=TJSHTMLInputElement(document.createElement('input'));
      C._type:='checkbox'; C.id:='root-'+IntToStr(I); C.value:=IntToStr(I); LabelNode.appendChild(C);
      if M.PassAt(I).Visibility=wppvPrivate then Text:=Text+' [private provider]'
      else Text:=Text+' [public]';
      LabelNode.appendChild(document.createTextNode(Text)); Element('repair-roots').appendChild(LabelNode);
    end;
    if Previous<>'' then Choice('view-pass').value:=Previous;
    if Choice('view-pass').selectedIndex<0 then Choice('view-pass').selectedIndex:=0;
    FRootRecipe:=RecipeText; PopulateTokens(True);
  finally M.Free; end;
end;

procedure TBrowserPipelineWorkspaceApplication.PopulateTokens(const ResetWindow: Boolean);
var M: TWfcPipelineModel; R: TWfcPipelineRun; Values: TWfcModelTokens;
  I,P,W,H: Integer; Text: String; C: TJSHTMLInputElement;
  OptionNode: TJSHTMLOptionElement; LabelNode: TJSElement; L: TWfcLatticeLayout;
begin
  if not FWorkbench.HasExecution or (Choice('view-pass').value='') then
  begin
    Choice('cell-token').textContent:=''; Element('domain-tokens').textContent:='';
    Element('pass-report').textContent:='This epoch has no selectable public pass. Private passes remain available as explicit repair roots.';
    Element('cell-report').textContent:='No public cell target.'; InvalidateView; Exit;
  end;
  P:=Integer(Decimal(Choice('view-pass').value,'Selected pass',High(Integer)));
  M:=nil; R:=nil;
  try
    M:=FWorkbench.CopyCurrentRecipe; R:=FWorkbench.CopyAppliedRun;
    Values:=M.CopyPublicVocabulary(P); L:=R.PassLayoutAt(P);
    Choice('cell-token').textContent:=''; Element('domain-tokens').textContent:='';
    for I:=0 to High(Values) do
    begin
      Text:=Encoded(Values[I]); OptionNode:=TJSHTMLOptionElement(document.createElement('option'));
      OptionNode.value:=Text; OptionNode.textContent:=Text; Choice('cell-token').appendChild(OptionNode);
      LabelNode:=document.createElement('label'); C:=TJSHTMLInputElement(document.createElement('input'));
      C._type:='checkbox'; C.id:='domain-'+IntToStr(I); C.value:=Text; C.checked:=True;
      LabelNode.appendChild(C); LabelNode.appendChild(document.createTextNode(Text)); Element('domain-tokens').appendChild(LabelNode);
    end;
    Element('pass-report').textContent:='Pass '+IntToStr(P)+' / '+Encoded(M.PassAt(P).LabelName)+#10+
      'Rank '+IntToStr(R.PassTopologyAt(P).Rank)+'; cells '+IntToStr(L.Cells.X)+' × '+IntToStr(L.Cells.Y)+' × '+IntToStr(L.Cells.Z)+#10+
      'Origin '+IntToStr(L.Origin.X)+','+IntToStr(L.Origin.Y)+','+IntToStr(L.Origin.Z)+#10+
      'Pitch '+IntToStr(L.Pitch.X)+','+IntToStr(L.Pitch.Y)+','+IntToStr(L.Pitch.Z)+#10+
      'Wrap '+BoolToStr(L.Wrap,True)+'; public vocabulary '+IntToStr(Length(Values))+' tokens.';
    if ResetWindow then
    begin
      W:=L.Cells.X; if W>16 then W:=16; H:=L.Cells.Y; if H>12 then H:=12;
      Input('view-x').value:='0'; Input('view-y').value:='0'; Input('view-z').value:='0';
      Input('view-width').value:=IntToStr(W); Input('view-height').value:=IntToStr(H);
      Input('cell-x').value:='0'; Input('cell-y').value:='0'; Input('cell-z').value:='0';
    end;
    Element('cell-report').textContent:='Inputs not loaded for this cell. Inspect the coordinate before editing if you need its existing domain.';
    InvalidateView;
  finally R.Free; M.Free; end;
end;

procedure TBrowserPipelineWorkspaceApplication.SyncLive;
var J: TWfcPipelineWorkspaceJournal; I,Epochs: Integer; A: TWfcPipelineWorkspaceAction;
  Limits: TWfcPipelineWorkspaceJournalLimits; Text: String;
begin
  if not FWorkbench.HasExecution then Exit;
  { This is a derived presentation of the actual journal, not shadow history.
    A permissive Integer envelope avoids making display depend on a newly
    edited, possibly smaller policy. The live journal was already validated. }
  Limits.Version:=1; Limits.MaxRecipes:=High(Integer); Limits.MaxRuns:=High(Integer);
  Limits.MaxContextTextBytes:=High(Integer); Limits.MaxActions:=High(Integer);
  Limits.MaxRootReferences:=High(Integer); Limits.MaxEvidenceTextBytes:=High(Integer);
  Limits.MaxEncodedTextBytes:=High(Integer);
  J:=DecodeWfcPipelineWorkspaceJournalText(FWorkbench.CopyCanonicalJournal,Limits);
  try
    FInitialAttempted:=False; Epochs:=0;
    for I:=0 to J.ActionCount-1 do
    begin
      A:=J.ActionAt(I);
      if A.Kind=wpwakBeginEpoch then begin Inc(Epochs); FInitialAttempted:=False; end
      else if A.Kind=wpwakInitial then FInitialAttempted:=True;
    end;
    Element('publication').textContent:=IntToStr(FWorkbench.PublicationRevision);
    Element('session-revision').textContent:=IntToStr(FWorkbench.SessionRevision);
    Element('action-count').textContent:=IntToStr(J.ActionCount);
    Element('baseline-state').textContent:='none';
    if FWorkbench.HasSuccessfulBaseline then Element('baseline-state').textContent:='historical snapshot retained';
    Text:='Epoch '+IntToStr(Epochs)+' / ';
    if FWorkbench.HasCurrentOutput then Text:=Text+'current output'
    else if FWorkbench.HasSuccessfulBaseline then Text:=Text+'not current; baseline retained'
    else Text:=Text+'no successful baseline';
    Element('live-heading').textContent:=Text;
    document.body.setAttribute('data-workspace-current',LowerCase(BoolToStr(FWorkbench.HasCurrentOutput,True)));
    document.body.setAttribute('data-has-baseline',LowerCase(BoolToStr(FWorkbench.HasSuccessfulBaseline,True)));
    document.body.setAttribute('data-publication-revision',IntToStr(FWorkbench.PublicationRevision));
    if not FInitialAttempted then Element('initial-note').textContent:='This epoch has not attempted its initial solve. Current applied inputs and budgets will be used once.'
    else if not FWorkbench.HasSuccessfulBaseline then Element('initial-note').textContent:='Initial solve was attempted and did not establish a baseline. It cannot be retried or selectively repaired in this epoch. Begin an explicit new epoch to try different inputs or budgets.'
    else Element('initial-note').textContent:='Initial solve already recorded. Further work uses explicit scoped repair, not another initial attempt.';
  finally J.Free; end;
  PopulateLivePasses; InvalidatePreview; InvalidateView; Revoke('journal-download',FJournalUrl);
  FNeedsRecovery:=False;
end;

procedure TBrowserPipelineWorkspaceApplication.ShowReceipt(const R: TWfcPipelineWorkspaceReceipt);
var Text: String; O: TWfcPipelineSessionOutcome; E: TWfcPipelineSessionEditOutcome;
begin
  SyncLive;
  Text:=WfcPipelineWorkspaceActionName(R.Kind)+' accepted at publication '+IntToStr(R.PublicationRevision)+#10+
    'Action '+IntToStr(R.ActionIndex)+'; epoch '+IntToStr(R.EpochCount)+'; session revision '+IntToStr(R.SessionRevision)+#10+
    'Recipe row '+IntToStr(R.RecipeIndex)+'; run row '+IntToStr(R.RunIndex)+#10+
    'Current '+BoolToStr(R.HasCurrentOutput,True)+'; baseline '+BoolToStr(R.HasSuccessfulBaseline,True);
  O:=R.BorrowSolveOutcome; E:=R.BorrowEditOutcome;
  if O<>nil then Text:=Text+#10+'Solved '+BoolToStr(O.Solved,True)+#10+ScopeText(O.CopyScope)+#10+
    'Pending '+Indices(O.CopyPendingPassIndices)+'; commit validation kind '+IntToStr(Ord(O.LastValidation.Kind));
  if E<>nil then Text:=Text+#10+'Authored '+Indices(E.CopyAuthoredPassIndices)+#10+'Pending '+Indices(E.CopyPendingPassIndices);
  Element('outcome-report').textContent:=Text;
  Element('outcome-evidence').textContent:=R.EvidenceText;
  if R.EvidenceText='' then Element('outcome-evidence').textContent:='Epoch construction records context only; no invented solver evidence.';
  if (O<>nil) and not O.Solved then SetStatus('A normal unsuccessful solve was recorded in live history. The outcome is inspectable and saveable; no automatic retry occurred.')
  else SetStatus('Accepted '+WfcPipelineWorkspaceActionName(R.Kind)+'. Live history updated; render the window to inspect it. Draft documents were not overwritten.');
end;

procedure TBrowserPipelineWorkspaceApplication.ShowDefinition(const Contexts: TWfcPipelineWorkspaceContexts);
var M: TWfcPipelineModel; R: TWfcPipelineRun; I: Integer; Text: String;
  D: TWfcPipelineDependency; Resource: TWfcPipelineResource;
begin
  M:=Contexts.BorrowRecipe(0); R:=Contexts.BorrowRun(0);
  Text:='GRAPH-FREE: valid canonical definition / invocation. No output or feasibility is claimed.'+#10+
    'Name '+Encoded(M.CopyMetadata.Name)+'; license '+Encoded(M.CopyMetadata.LicenseIdentifier)+#10+
    'Passes '+IntToStr(M.PassCount)+'; resources '+IntToStr(M.ResourceCount)+
    '; requirements '+IntToStr(M.RequirementCount)+'; bridges '+IntToStr(M.BridgeCount)+#10+
    'Quotas '+IntToStr(M.ValueQuotaCount)+'; connectivity clauses '+IntToStr(M.ConnectivityCount)+#10+
    'Run format '+IntToStr(R.FormatVersion)+'; all-pass cell instances '+IntToStr(R.TotalCellCount)+#10+
    'Seed '+UIntToStr(R.Seed)+'; locks '+IntToStr(R.LockCount)+'; domains '+IntToStr(R.DomainCount)+#10;
  for I:=0 to M.PassCount-1 do Text:=Text+#10+'Pass '+IntToStr(I)+' '+Encoded(M.PassAt(I).LabelName)+
    ' / visibility '+IntToStr(Ord(M.PassAt(I).Visibility))+' / rank '+IntToStr(R.PassTopologyAt(I).Rank)+
    ' / cells '+IntToStr(R.PassCellCount(I));
  for I:=0 to M.DependencyCount-1 do
  begin D:=M.DependencyAt(I); Text:=Text+#10+'Dependency consumer '+IntToStr(D.ConsumerPassIndex)+' → provider '+IntToStr(D.ProviderPassIndex); end;
  for I:=0 to M.ResourceCount-1 do
  begin
    Resource:=M.ResourceAt(I); Text:=Text+#10+'Resource '+Encoded(Resource.Id)+' / license '+
      Encoded(Resource.SourceLicenseIdentifier)+' / '+Encoded(Resource.SourceDescription)+
      ' / provenance label '+Encoded(Resource.SourceFingerprint);
  end;
  Element('definition-report').textContent:=Text;
  Element('draft-status').textContent:='These exact drafts passed graph-free inspection. They have not been applied by this inspection.';
  Download('recipe-download',Contexts.RecipeTextAt(0),'workspace-recipe.wfc','text/plain;charset=us-ascii',FRecipeUrl);
  Download('run-download',Contexts.RunTextAt(0).Text,'workspace-run.wfc','text/plain;charset=us-ascii',FRunUrl);
end;

procedure TBrowserPipelineWorkspaceApplication.Render(const Slice: TWfcPipelineWorkspaceSlice);
var Svg: String;
begin
  InvalidateView; Svg:=PipelineWorkspaceSliceSvg(FWorkbench,Slice);
  { Only the project renderer's escaped, typed output enters innerHTML.
    Imported documents, labels, errors and evidence always use textContent. }
  Element('map-output').innerHTML:=Svg;
  Download('svg-download',Svg,'workspace-diagnostic.svg','image/svg+xml;charset=us-ascii',FSvgUrl);
  if Slice.UseBaseline then Element('view-status').textContent:='HISTORICAL BASELINE. This is not a claim about current inputs. Viewport and full world footprints are encoded in the SVG.'
  else if FWorkbench.HasCurrentOutput then Element('view-status').textContent:='CURRENT owned output for the executed recipe. This diagnostic is not an independent physical-safety validation.'
  else Element('view-status').textContent:='NOT CURRENT. Actual owned cells may be empty, pending or from an unsuccessful solve; a prior green result is not being substituted.';
end;

procedure TBrowserPipelineWorkspaceApplication.InspectCell(const Q: TWorkspaceUiRequest);
var R: TWfcPipelineRun; M: TWfcPipelineModel; L: TWfcLatticeLayout;
  Lock: TWfcPipelineCellLock; Domain: TWfcPipelineCellDomain;
  I,J,K: Integer; Text,Value: String; Nodes: TJSNodeList; C: TJSHTMLInputElement;
begin
  R:=nil; M:=nil;
  try
    R:=FWorkbench.CopyAppliedRun; M:=FWorkbench.CopyCurrentRecipe;
    if (Q.PassIndex<0) or (Q.PassIndex>=M.PassCount) or (M.PassAt(Q.PassIndex).Visibility<>wppvPublic) then UiError('Select a public pass');
    L:=R.PassLayoutAt(Q.PassIndex);
    if (Q.X>=L.Cells.X) or (Q.Y>=L.Cells.Y) or (Q.Z>=L.Cells.Z) then UiError('Cell is outside its local pass extent');
    Text:='Pass '+IntToStr(Q.PassIndex)+'; local '+IntToStr(Q.X)+','+IntToStr(Q.Y)+','+IntToStr(Q.Z)+#10+
      'No lock row.'+#10+'No domain row (absent, not an empty domain).';
    Nodes:=Element('domain-tokens').querySelectorAll('input[type=checkbox]');
    for I:=0 to Nodes.length-1 do TJSHTMLInputElement(Nodes[I]).checked:=True;
    for I:=0 to R.LockCount-1 do
    begin
      Lock:=R.LockAt(I);
      if (Lock.PassIndex=Q.PassIndex) and (Lock.X=Q.X) and (Lock.Y=Q.Y) and (Lock.Z=Q.Z) then
      begin
        Value:=Encoded(Lock.Token); Choice('cell-token').value:=Value;
        Text:=StringReplace(Text,'No lock row.','Lock row: '+Value,[]);
      end;
    end;
    for I:=0 to R.DomainCount-1 do
    begin
      Domain:=R.DomainAt(I);
      if (Domain.PassIndex<>Q.PassIndex) or (Domain.X<>Q.X) or (Domain.Y<>Q.Y) or (Domain.Z<>Q.Z) then Continue;
      Value:=''; for J:=0 to High(Domain.AllowedTokens) do begin if J>0 then Value:=Value+', '; Value:=Value+Encoded(Domain.AllowedTokens[J]); end;
      Text:=StringReplace(Text,'No domain row (absent, not an empty domain).','Explicit domain row: ['+Value+']',[]);
      for J:=0 to Nodes.length-1 do
      begin
        C:=TJSHTMLInputElement(Nodes[J]); C.checked:=False;
        for K:=0 to High(Domain.AllowedTokens) do
          if C.value=Encoded(Domain.AllowedTokens[K]) then C.checked:=True;
      end;
      Break;
    end;
    Element('cell-report').textContent:=Text;
  finally M.Free; R.Free; end;
end;

procedure TBrowserPipelineWorkspaceApplication.CancelPending;
begin
  if FOperationTimer<>0 then window.clearTimeout(FOperationTimer);
  FOperationTimer:=0; FPending:=Default(TWorkspaceUiRequest);
end;

procedure TBrowserPipelineWorkspaceApplication.CancelReader;
var Reader: TJSFileReader;
begin
  Reader:=FReader; FReader:=nil; FReadTarget:='';
  if Reader=nil then Exit;
  Reader.onload:=nil; Reader.onerror:=nil; Reader.onabort:=nil;
  if Reader.readyState=TJSFileReader.LOADING then Reader.abort;
end;

procedure TBrowserPipelineWorkspaceApplication.Queue(const Action: String);
var Q: TWorkspaceUiRequest; S: String; Scope: TWfcPipelineSessionScope;
  procedure PresetOptions;
  begin
    Q.RunOptions:=ReadRunOptions;
    if Q.Preset='landscape' then
    begin
      Q.Weights.Land:=Number('weight-land',1); Q.Weights.Water:=Number('weight-water',1);
      Q.Weights.Clear:=Number('weight-clear',1); Q.Weights.Tree:=Number('weight-tree',1);
      Q.Weights.House:=Number('weight-house',1); Q.Weights.Vacant:=Number('weight-vacant',1);
    end
    else if Q.Preset='sequence' then
    begin
      Q.SequenceOptions.Order:=Number('sequence-order',1);
      S:=Choice('sequence-boundary').value;
      if S='open' then Q.SequenceOptions.Boundary:=wmbOpen
      else if S='wrap' then Q.SequenceOptions.Boundary:=wmbWrap
      else UiError('Choose an explicit source boundary');
      S:=Choice('sequence-extent').value;
      if S='whole' then Q.SequenceOptions.Extent:=wseWhole
      else if S='prefix' then Q.SequenceOptions.Extent:=wsePrefix
      else if S='suffix' then Q.SequenceOptions.Extent:=wseSuffix
      else if S='fragment' then Q.SequenceOptions.Extent:=wseFragment
      else if S='wrap' then Q.SequenceOptions.Extent:=wseWrap
      else UiError('Choose an explicit sequence extent policy');
    end else UiError('Select a preset before rebuilding its geometry');
  end;
begin
  if FDisposed or FBusy then Exit;
  Q:=Default(TWorkspaceUiRequest); Q.Action:=Action;
  Q.Expected:=FWorkbench.PublicationRevision;
  { Capture only arguments used by this action. An unrelated invalid draft or
    cell coordinate must not become a hidden argument to another operation. }
  if (Action<>'copy-live') and (Action<>'copy-journal') and (Action<>'save-journal') and
    (Action<>'live-geometry') and (Action<>'render') and (Action<>'inspect-cell') then
    Q.Policy:=ReadPolicy(Q.PolicyKey);
  if (Action='inspect-definition') or (Action='begin') or (Action='stage-options') then
  begin Q.RecipeText:=Area('recipe-draft').value; Q.RunText:=Area('run-draft').value; end;
  if (Action='inspect-journal') or (Action='restore') then Q.JournalText:=Area('journal-draft').value;
  if Action='preset-landscape' then
  begin Q.Preset:='landscape'; PresetOptions; DefaultMappedLandscapeGeometry(Q.Topologies,Q.Extents); end
  else if Action='preset-sequence' then
  begin Q.Preset:='sequence'; PresetOptions; DefaultLearnedSequenceGeometry(Q.Topologies,Q.Extents); end
  else if Action='preset-rebuild' then
  begin Q.Preset:=FPreset; PresetOptions; ReadGeometry(Q.Topologies,Q.Extents); end;
  if Action='stage-options' then Q.RunOptions:=ReadRunOptions;
  if Action='apply-run' then Q.RunText:=Area('run-draft').value;
  if Action='mapped-epoch' then
  begin
    if (FGeometryLiveRecipe='') or (FGeometryLiveRecipe<>FRootRecipe) then
      UiError('Load the current live geometry before beginning a mapped epoch');
    ReadGeometry(Q.Topologies,Q.Extents);
    Q.RunOptions.Seed:=Decimal(Input('seed').value,'Seed',High(Cardinal));
  end;
  if (Action='set-lock') or (Action='clear-lock') or (Action='set-domain') or
    (Action='clear-domain') or (Action='inspect-cell') then
  begin
    Q.PassIndex:=Integer(Decimal(Choice('view-pass').value,'Public pass',High(Integer)));
    Q.X:=Number('cell-x'); Q.Y:=Number('cell-y'); Q.Z:=Number('cell-z');
    if Action='set-lock' then Q.Token:=WfcTextDecodeToken(Choice('cell-token').value,'cell lock token');
    if Action='set-domain' then Q.Allowed:=ReadDomain;
  end;
  if (Action='preview-repair') or (Action='repair') then
  begin
    Q.RunText:=AppliedText; Q.Roots:=ReadRoots;
    if Action='repair' then
    begin
      if FPreview=nil then UiError('Preview the current roots and policy first');
      Scope:=FPreview.CopyScope;
      if (not FPreview.CanExecute) or (FPreview.PublicationRevision<>Q.Expected) or
        (FPreview.RunText<>Q.RunText) or (FPreviewPolicyKey<>Q.PolicyKey) or
        (Indices(Scope.RequestedRootIndices)<>Indices(Q.Roots)) then
        UiError('Repair preview no longer authorizes these exact roots, inputs and policy');
    end else InvalidatePreview;
  end;
  if Action='render' then Q.Slice:=ReadSlice;
  FPending:=Q; FBusy:=True;
  { Yield once so disabled controls and the queued status are visible. The
    solver itself is synchronous; cancellation is offered only before it starts. }
  FOperationTimer:=window.setTimeout(@ExecutePending,0);
  document.body.setAttribute('data-operation',Action);
  SetStatus('Queued '+Action+'. Arguments and expected publication were captured; controls are temporarily disabled.');
  SetButtons;
end;

procedure TBrowserPipelineWorkspaceApplication.ExecutePending;
var Q: TWorkspaceUiRequest; Completed: Boolean;
begin
  if FDisposed or (FOperationTimer=0) or not FBusy then Exit;
  FOperationTimer:=0; Q:=FPending; FPending:=Default(TWorkspaceUiRequest);
  Completed:=False; SetButtons;
  try
    try
      if Q.Expected<>FWorkbench.PublicationRevision then UiError('Publication changed before queued action dispatch');
      Perform(Q); Completed:=True;
    except
      on E: Exception do
      begin
        if Q.Expected<>FWorkbench.PublicationRevision then
        begin
          FNeedsRecovery:=True; InvalidatePreview; InvalidateView;
          Revoke('journal-download',FJournalUrl);
          Element('outcome-report').textContent:='An action published, but its presentation did not complete. Recover from the authoritative live history.';
          Element('outcome-evidence').textContent:='';
        end;
        ShowError(E);
      end;
    end;
  finally
    FBusy:=False; SetButtons; document.body.removeAttribute('data-operation');
    document.body.setAttribute('data-last-operation',Q.Action);
    if Completed then document.body.setAttribute('data-operation-result','completed')
    else document.body.setAttribute('data-operation-result','rejected');
    if document.body.getAttribute('data-state')='busy' then document.body.setAttribute('data-state','ready');
  end;
end;

procedure TBrowserPipelineWorkspaceApplication.Perform(const Q: TWorkspaceUiRequest);
var C: TWfcPipelineWorkspaceContexts; M: TWfcPipelineModel; R,NewRun: TWfcPipelineRun;
  Receipt: TWfcPipelineWorkspaceReceipt; J: TWfcPipelineWorkspaceJournal;
  A: TWfcPipelineWorkspaceAction; I: Integer; Text: String;
  procedure LiveGeometry;
  begin
    M:=FWorkbench.CopyCurrentRecipe; R:=FWorkbench.CopyAppliedRun;
    PopulateGeometry(M,R,True);
  end;
  procedure ShowRunOptions(const Value: TWfcPipelineRun);
  begin
    Input('seed').value:=UIntToStr(Value.Seed);
    if Value.Strategy=wpssOneWay then Choice('strategy').value:='one-way'
    else Choice('strategy').value:='negotiated';
    Input('backtracks').value:=IntToStr(Value.MaxBacktracks);
    Input('pass-backtracks').value:=IntToStr(Value.MaxPassBacktracks);
    Input('capture-trace').checked:=Value.CaptureTrace;
  end;
begin
  C:=nil; M:=nil; R:=nil; NewRun:=nil; Receipt:=nil; J:=nil;
  try
    if Q.Action='inspect-definition' then
    begin
      C:=TWfcPipelineWorkspaceWorkbench.InspectDefinition(Q.RecipeText,Q.RunText,ContextLimits(Q.Policy));
      ShowDefinition(C); PopulateGeometry(C.BorrowRecipe(0),C.BorrowRun(0),False);
      ShowRunOptions(C.BorrowRun(0)); FPreset:='';
      SetStatus('Graph-free inspection succeeded. No graph was allocated and live history was not changed.');
    end
    else if (Q.Action='preset-landscape') or (Q.Action='preset-sequence') or (Q.Action='preset-rebuild') then
    begin
      if Q.Preset='landscape' then C:=BuildMappedLandscapePreset(Q.Topologies,Q.Extents,Q.RunOptions,
        Q.Weights,nil,nil,ContextLimits(Q.Policy))
      else C:=BuildLearnedSequencePreset(Q.Topologies,Q.Extents,Q.RunOptions,
        Q.SequenceOptions,nil,nil,ContextLimits(Q.Policy));
      Area('recipe-draft').value:=C.RecipeTextAt(0); Area('run-draft').value:=C.RunTextAt(0).Text;
      MarkDraft('definition'); FPreset:=Q.Preset;
      ShowDefinition(C); PopulateGeometry(C.BorrowRecipe(0),C.BorrowRun(0),False);
      ShowRunOptions(C.BorrowRun(0));
      SetStatus('Preset staged as canonical recipe/run drafts. Geometry is explicit and editable; begin an epoch to execute it. No solve occurred.');
    end
    else if Q.Action='begin' then
    begin
      Receipt:=FWorkbench.BeginEpoch(Q.RecipeText,Q.RunText,Q.Policy,Q.Expected);
      ShowReceipt(Receipt); LiveGeometry;
    end
    else if Q.Action='stage-options' then
    begin
      C:=TWfcPipelineWorkspaceWorkbench.InspectDefinition(Q.RecipeText,Q.RunText,ContextLimits(Q.Policy));
      if C.BorrowRun(0).FormatVersion=1 then
        NewRun:=TWfcPipelineRun.Create(C.BorrowRecipe(0),C.BorrowRun(0).Width,C.BorrowRun(0).Height,
          C.BorrowRun(0).Depth,Q.RunOptions.Seed,Q.RunOptions.Strategy,Q.RunOptions.MaxBacktracks,
          Q.RunOptions.MaxPassBacktracks,Q.RunOptions.CaptureTrace,C.BorrowRun(0).CopyLocks,C.BorrowRun(0).CopyDomains)
      else NewRun:=TWfcPipelineRun.Create(C.BorrowRecipe(0),C.BorrowRun(0).CopyPassExtents,
        Q.RunOptions.Seed,Q.RunOptions.Strategy,Q.RunOptions.MaxBacktracks,Q.RunOptions.MaxPassBacktracks,
        Q.RunOptions.CaptureTrace,C.BorrowRun(0).CopyLocks,C.BorrowRun(0).CopyDomains);
      Area('run-draft').value:=EncodeWfcPipelineRunText(NewRun); MarkDraft('definition');
      SetStatus('Options staged in the run draft with all input rows retained. Apply complete run for this epoch, or begin a new epoch when changing seed or layout.');
    end
    else if Q.Action='apply-run' then
    begin Receipt:=FWorkbench.ApplyRun(Q.RunText,Q.Policy,Q.Expected); ShowReceipt(Receipt); end
    else if Q.Action='initial' then
    begin Receipt:=FWorkbench.ExecuteInitial(Q.Policy,Q.Expected); ShowReceipt(Receipt); end
    else if Q.Action='set-lock' then
    begin Receipt:=FWorkbench.SetCellLock(Q.PassIndex,Q.X,Q.Y,Q.Z,Q.Token,Q.Policy,Q.Expected); ShowReceipt(Receipt); end
    else if Q.Action='clear-lock' then
    begin Receipt:=FWorkbench.ClearCellLock(Q.PassIndex,Q.X,Q.Y,Q.Z,Q.Policy,Q.Expected); ShowReceipt(Receipt); end
    else if Q.Action='set-domain' then
    begin Receipt:=FWorkbench.SetCellDomain(Q.PassIndex,Q.X,Q.Y,Q.Z,Q.Allowed,Q.Policy,Q.Expected); ShowReceipt(Receipt); end
    else if Q.Action='clear-domain' then
    begin Receipt:=FWorkbench.ClearCellDomain(Q.PassIndex,Q.X,Q.Y,Q.Z,Q.Policy,Q.Expected); ShowReceipt(Receipt); end
    else if Q.Action='inspect-cell' then
    begin InspectCell(Q); SetStatus('Inspected applied input rows for this public cell. No input or history action was authored.'); end
    else if Q.Action='preview-repair' then
    begin
      FPreview:=FWorkbench.PreviewRepair(Q.RunText,Q.Roots,Q.Policy,Q.Expected); FPreviewPolicyKey:=Q.PolicyKey;
      Text:=ScopeText(FPreview.CopyScope)+#10+'Can execute: '+BoolToStr(FPreview.CanExecute,True)+#10+
        'Missing successful baseline: '+BoolToStr(FPreview.MissingBaseline,True)+#10+
        'Bound publication '+IntToStr(FPreview.PublicationRevision)+'; session revision '+IntToStr(FPreview.SessionRevision);
      Element('scope-report').textContent:=Text;
      SetStatus('Preview replayed prior history under the current policy, then inspected repair permission. The requested repair was not executed or published; missing required passes were not added to your roots.');
    end
    else if Q.Action='repair' then
    begin Receipt:=FWorkbench.ExecuteRepair(Q.RunText,Q.Roots,Q.Policy,Q.Expected); ShowReceipt(Receipt); end
    else if Q.Action='mapped-epoch' then
    begin
      Receipt:=FWorkbench.BeginMappedEpoch(Q.Topologies,Q.Extents,Q.RunOptions.Seed,Q.Policy,Q.Expected);
      ShowReceipt(Receipt); LiveGeometry;
    end
    else if Q.Action='live-geometry' then
    begin LiveGeometry; SetStatus('Loaded actual live geometry into the separate form. No epoch was created.'); end
    else if Q.Action='copy-live' then
    begin
      LiveGeometry;
      Area('recipe-draft').value:=EncodeWfcPipelineModelText(M); Area('run-draft').value:=EncodeWfcPipelineRunText(R);
      MarkDraft('definition'); FPreset:=''; ShowRunOptions(R); SyncLive;
      SetStatus('Replaced definition drafts with complete live recipe / applied run. History was not changed; presentation has been refreshed from its owner.');
    end
    else if Q.Action='inspect-journal' then
    begin
      J:=TWfcPipelineWorkspaceWorkbench.InspectJournal(Q.JournalText,Q.Policy.Journal);
      Text:='UNVERIFIED CLAIMS / graph-free canonical inspection only.'+#10+
        'Recipes '+IntToStr(J.RecipeCount)+'; runs '+IntToStr(J.RunCount)+'; actions '+IntToStr(J.ActionCount)+#10+
        'Context bytes '+IntToStr(J.ContextTextBytes)+'; evidence bytes '+IntToStr(J.EvidenceTextBytes)+
        '; encoded journal bytes '+IntToStr(J.EncodedTextBytes)+'; root references '+IntToStr(J.RootReferenceCount);
      for I:=0 to J.ActionCount-1 do
      begin
        A:=J.ActionAt(I); Text:=Text+#10+IntToStr(I)+' '+WfcPipelineWorkspaceActionName(A.Kind)+
          ' / run '+IntToStr(A.RunIndex)+' / recipe '+IntToStr(J.RunTextAt(A.RunIndex).RecipeIndex)+
          ' / roots '+Indices(A.RequestedRootIndices);
      end;
      Element('journal-report').textContent:=Text;
      Element('journal-status').textContent:='Canonical journal inspected, but its solver evidence remains unverified. Only explicit replay can verify and restore it.';
      SetStatus('Journal inspection did not allocate a solver or replace live history.');
    end
    else if Q.Action='restore' then
    begin
      FWorkbench.RestoreJournal(Q.JournalText,Q.Policy,Q.Expected); SyncLive; LiveGeometry;
      Element('outcome-report').textContent:='The complete journal was replayed and every claimed evidence byte matched actual execution before atomic publication. Failed solve outcomes remain recorded failures.';
      Element('outcome-evidence').textContent:='Use Copy live journal to inspect complete verified history. The draft itself remains editable and separate.';
      SetStatus('Verified replay restored the retained execution owner. Definition drafts were not overwritten; no replacement was published before complete verification.');
    end
    else if Q.Action='copy-journal' then
    begin
      Area('journal-draft').value:=FWorkbench.CopyCanonicalJournal; MarkDraft('journal');
      SetStatus('Copied live history into the journal draft. Editing or inspecting this draft does not change the live owner; a later restore must replay it again.');
    end
    else if Q.Action='save-journal' then
    begin
      Download('journal-download',FWorkbench.CopyCanonicalJournal,
        'workspace-publication-'+IntToStr(Q.Expected)+'.journal','text/plain;charset=us-ascii',FJournalUrl);
      SetStatus('Prepared the complete canonical live journal download, including normal failed outcomes. Follow Save live journal file to download it.');
    end
    else if Q.Action='render' then
    begin Render(Q.Slice); SetStatus('Rendered the explicitly selected owned state and exact viewport. The full composition was not clipped or resized.'); end
    else UiError('Unknown editor action: '+Q.Action);
  finally J.Free; Receipt.Free; NewRun.Free; R.Free; M.Free; C.Free; end;
end;

procedure TBrowserPipelineWorkspaceApplication.BeginRead(const Id: String);
var FileInput: TJSHTMLInputElement; FileValue: TJSHTMLFile; Maximum: Integer;
begin
  if FBusy or FDisposed then Exit;
  FileInput:=Input(Id);
  if (FileInput.files=nil) or (FileInput.files.length<>1) then Exit;
  FileValue:=FileInput.files[0]; FileInput.value:='';
  if Id='journal-file' then Maximum:=Number('policy-journal-bytes',1)
  else Maximum:=Number('policy-context-bytes',1);
  if FileValue.size>Maximum then UiError('Selected file exceeds the current input byte limit');
  if Id='recipe-file' then FReadTarget:='recipe-draft'
  else if Id='run-file' then FReadTarget:='run-draft'
  else if Id='journal-file' then FReadTarget:='journal-draft'
  else UiError('Unknown file input');
  FReadLimit:=Maximum; FReader:=TJSFileReader.new;
  FReader.onload:=@HandleRead; FReader.onerror:=@HandleReadError; FReader.onabort:=@HandleReadError;
  FBusy:=True;
  document.body.setAttribute('data-operation','import-'+FReadTarget);
  SetStatus('Reading selected local file as exact bytes. ASCII plus LF only; no BOM, CRLF conversion, trimming or automatic newline is permitted.');
  SetButtons;
  try FReader.readAsArrayBuffer(FileValue);
  except
    CancelReader; FBusy:=False; SetButtons; document.body.removeAttribute('data-operation'); raise;
  end;
end;

function TBrowserPipelineWorkspaceApplication.HandleRead(Event: TJSEvent): Boolean;
var Reader: TJSFileReader; Bytes: TJSUint8Array; Parts: TJSArray;
  I,V: Integer; Chunk,Text,Target: String; Completed: Boolean;
begin
  Result:=True; Reader:=TJSFileReader(Event.target);
  if FDisposed or (FReader=nil) or (Reader<>FReader) then Exit;
  Target:=FReadTarget; Completed:=False;
  try
    try
      Bytes:=TJSUint8Array.new(TJSArrayBuffer(Reader.Result));
      if Bytes.length>FReadLimit then UiError('Read result exceeds the captured input byte limit');
      Parts:=TJSArray.new; Chunk:='';
      for I:=0 to Bytes.length-1 do
      begin
        V:=Bytes[I];
        if (V<>10) and ((V<32) or (V>126)) then
          UiError('Canonical import requires ASCII with LF only; invalid byte at offset '+IntToStr(I));
        Chunk:=Chunk+Chr(V);
        if Length(Chunk)=16384 then begin Parts.push(Chunk); Chunk:=''; end;
      end;
      Parts.push(Chunk); Text:=String(Parts.join(''));
      { Assignment occurs only after every byte passes. Decoders still decide
        canonical grammar; import itself does not certify validity. }
      Area(Target).value:=Text;
      if Target='journal-draft' then MarkDraft('journal')
      else begin MarkDraft('definition'); FPreset:=''; end;
      SetStatus('Exact local bytes loaded into a draft only. Inspect before explicit execution; live history was unchanged.');
      Completed:=True;
    except on E: Exception do ShowError(E); end;
  finally
    CancelReader; FBusy:=False; SetButtons; document.body.removeAttribute('data-operation');
    document.body.setAttribute('data-last-operation','import-'+Target);
    if Completed then document.body.setAttribute('data-operation-result','completed')
    else document.body.setAttribute('data-operation-result','rejected');
    if document.body.getAttribute('data-state')='busy' then document.body.setAttribute('data-state','ready');
  end;
end;

function TBrowserPipelineWorkspaceApplication.HandleReadError(Event: TJSEvent): Boolean;
var Target: String; E: Exception;
begin
  Result:=True;
  if FDisposed or (FReader=nil) or (Event.target<>FReader) then Exit;
  Target:=FReadTarget; CancelReader; FBusy:=False;
  E:=EWfcPipelineWorkspaceWorkbench.Create('Local file read failed or was aborted. Existing drafts and live history were retained.');
  try ShowError(E); finally E.Free; end;
  document.body.removeAttribute('data-operation');
  document.body.setAttribute('data-last-operation','import-'+Target);
  document.body.setAttribute('data-operation-result','rejected'); SetButtons;
end;

function TBrowserPipelineWorkspaceApplication.HandleClick(Event: TJSMouseEvent): Boolean;
var Node: TJSNode; Target,ActionNode,Anchor,CellNode: TJSElement;
  Action,Coordinates,Part: String; I,P: Integer;
begin
  Result:=True; if FDisposed then Exit;
  Node:=TJSNode(Event.target); if Node=nil then Exit;
  if Node.nodeType=1 then Target:=TJSElement(Node) else Target:=Node.parentElement;
  if Target=nil then Exit;
  Anchor:=Target.closest('a');
  if Anchor<>nil then
  begin
    if FBusy or (Anchor.getAttribute('aria-disabled')='true') then
    begin Event.preventDefault; Result:=False; end;
    Exit;
  end;
  ActionNode:=Target.closest('[data-action]');
  if ActionNode<>nil then
  begin
    Event.preventDefault; Result:=False; Action:=ActionNode.getAttribute('data-action');
    if Action='cancel' then
    begin
      if (FOperationTimer=0) and (FReader=nil) then Exit;
      Action:=document.body.getAttribute('data-operation');
      CancelPending; CancelReader; FBusy:=False; SetButtons;
      document.body.removeAttribute('data-operation'); document.body.setAttribute('data-last-operation',Action);
      document.body.setAttribute('data-operation-result','cancelled');
      SetStatus('Cancelled before execution or before file import completion. The live owner and draft documents were not changed.');
      Exit;
    end;
    if FBusy or ActionNode.hasAttribute('disabled') then Exit;
    try Queue(Action);
    except
      on E: Exception do
      begin
        ShowError(E); SetButtons; document.body.setAttribute('data-last-operation',Action);
        document.body.setAttribute('data-operation-result','rejected');
      end;
    end;
    Exit;
  end;
  if FBusy then Exit;
  CellNode:=Target.closest('g[data-cell]');
  if (CellNode=nil) or not Element('map-output').contains(CellNode) then Exit;
  try
    Coordinates:=CellNode.getAttribute('data-cell');
    for I:=0 to 2 do
    begin
      P:=Pos(',',Coordinates);
      if I<2 then
      begin
        if P=0 then UiError('Renderer cell coordinate is incomplete');
        Part:=Copy(Coordinates,1,P-1); Delete(Coordinates,1,P);
      end else Part:=Coordinates;
      Decimal(Part,'Renderer local coordinate',High(Integer));
      case I of 0: Input('cell-x').value:=Part; 1: Input('cell-y').value:=Part; 2: Input('cell-z').value:=Part; end;
    end;
    Queue('inspect-cell');
  except on E: Exception do begin ShowError(E); SetButtons; end; end;
end;

function TBrowserPipelineWorkspaceApplication.HandleInput(Event: TJSEvent): Boolean;
var Target: TJSElement; Id: String;
begin
  Result:=True; if FDisposed or FBusy then Exit;
  Target:=TJSElement(Event.target); if Target=nil then Exit; Id:=Target.id;
  try
    if Id='journal-draft' then MarkDraft('journal')
    else if (Id='recipe-draft') or (Id='run-draft') then
    begin MarkDraft('definition'); FPreset:=''; end
    else if (Copy(Id,1,7)='policy-') or (Copy(Id,1,5)='root-') then InvalidatePreview
    else if Copy(Id,1,5)='view-' then
    begin
      InvalidateView;
      if Id='view-pass' then PopulateTokens(True);
    end
    else if Copy(Id,1,9)='geometry-' then
      Element('geometry-status').textContent:='Geometry form edited only. Rebuild the selected preset draft, or explicitly begin a mapped epoch from a live-based form. Linked projections are not automatically resized.'
    else if (Copy(Id,1,5)='cell-') or (Copy(Id,1,7)='domain-') then
      Element('cell-report').textContent:='Cell editor changed; these controls are not applied input rows until an explicit cell action.';
    SetButtons;
  except on E: Exception do begin ShowError(E); SetButtons; end; end;
end;

function TBrowserPipelineWorkspaceApplication.HandleChange(Event: TJSEvent): Boolean;
var Id: String;
begin
  Result:=True; if FDisposed or FBusy then Exit;
  Id:=TJSElement(Event.target).id;
  if (Id='recipe-file') or (Id='run-file') or (Id='journal-file') then
  begin
    try BeginRead(Id);
    except on E: Exception do begin ShowError(E); SetButtons; end; end;
  end else Result:=HandleInput(Event);
end;

function TBrowserPipelineWorkspaceApplication.HandleRuntimeError(Event: TJSEvent): Boolean;
var E: Exception; MessageText: String;
begin
  Result:=True; if FDisposed then Exit;
  CancelPending; CancelReader; FBusy:=False; FNeedsRecovery:=FWorkbench.HasExecution;
  InvalidatePreview; InvalidateView; Revoke('journal-download',FJournalUrl);
  Element('outcome-report').textContent:='Presentation interrupted; inspect the authoritative live publication before further mutation.';
  Element('outcome-evidence').textContent:='';
  Element('live-heading').textContent:='Presentation interrupted';
  Element('publication').textContent:='unrefreshed'; Element('session-revision').textContent:='unrefreshed';
  Element('action-count').textContent:='unrefreshed'; Element('baseline-state').textContent:='unrefreshed';
  document.body.setAttribute('data-workspace-current','unknown');
  document.body.setAttribute('data-has-baseline','unknown');
  document.body.removeAttribute('data-operation'); document.body.setAttribute('data-operation-result','interrupted');
  MessageText:='An unhandled browser runtime error interrupted the editor.';
  if Event is TJSErrorEvent then MessageText:=MessageText+' '+TJSErrorEvent(Event).message;
  E:=EWfcPipelineWorkspaceWorkbench.Create(MessageText);
  try ShowError(E); finally E.Free; end;
  SetButtons;
end;

procedure TBrowserPipelineWorkspaceApplication.Run;
var ErrorHandler: WebOrWorker.TJSEventHandler;
begin
  if FBound or FDisposed then Exit;
  PopulatePolicy;
  TJSHTMLElement(Element('workbench')).onclick:=@HandleClick;
  TJSHTMLElement(Element('workbench')).oninput:=@HandleInput;
  TJSHTMLElement(Element('workbench')).onchange:=@HandleChange;
  ErrorHandler:=@HandleRuntimeError; window.addEventListener('error',ErrorHandler);
  FBound:=True;
  Revoke('recipe-download',FRecipeUrl); Revoke('run-download',FRunUrl);
  Revoke('journal-download',FJournalUrl); Revoke('svg-download',FSvgUrl);
  document.body.setAttribute('data-workspace-current','false');
  document.body.setAttribute('data-has-baseline','false');
  document.body.setAttribute('data-publication-revision','0');
  SetStatus('Ready for drafts. Choose a preset or load canonical recipe / run files, inspect them, then explicitly begin an epoch. No graph or solve has been created.');
  SetButtons;
end;

end.
