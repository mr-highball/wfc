{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Actual DOM workflow through the compiled application and its controls. }
program pipeline_workspace_ui_test;
{$mode delphi}{$H+}
{$IFNDEF PAS2JS}{$FATAL actual workspace UI test requires pas2js browser}{$ENDIF}
uses SysUtils,JS,Web,wfc_browser_test_host;
const
  STAGE_COUNT=14;
  RUN_TIMEOUT_MS=45000;
  ACTION_TIMEOUT_MS=5000;
  POLL_MS=20;
type
  TWorkspaceUiWorkflow=class
  private
    FFrame:TJSHTMLIFrameElement;
    FStep,FChecks,FStages,FCount,FPublication:Integer;
    FStarted,FActionStarted:Double;
    FFinished:Boolean;
    FExpectedUrl,FWaitingAction,FWaitingResult,FGroup:String;
    FInitialRecipe,FInitialRun,FBaselineJournal,FSolvedJournal,FSavedDraft,FLockToken:String;
    FSolvedCount:Integer;
    function Child:TJSDocument;
    function E(const Id:String):TJSElement;
    function Value(const Id:String):String;
    function Text(const Id:String):String;
    function Attr(const Name:String):String;
    function Button(const Action:String):TJSHTMLElement;
    procedure Check(const Condition:Boolean;const Detail:String);
    procedure HasText(const Id,Needle:String);
    procedure SetValue(const Id,NewValue:String);
    procedure SetChecked(const Id:String;const Checked:Boolean);
    procedure Dispatch(const Node:TJSElement;const EventName:String);
    procedure Click(const Action:String);
    procedure Action(const Name:String;const ExpectedResult:String='completed');
    procedure Mark(const Name:String);
    procedure Unchanged;
    procedure Accept(const Current,Baseline:Boolean);
    procedure State(const Current,Baseline:Boolean);
    procedure Roots(const Indices:array of Integer);
    procedure DomainNone;
    procedure Cell(const PassIndex,X:Integer);
    procedure GeometryWidth(const Width:Integer);
    procedure CheckQueued;
    procedure RequireResources;
    procedure ImportFile(const Payload:String);
    procedure Advance;
    procedure Poll;
    procedure Fail(const Detail:String);
    procedure Finish;
  public
    constructor Create;
    procedure Start;
  end;

function ErrorText(const Error:JSValue):String;
begin
  if isObject(Error) and isString(TJSObject(Error)['message']) then Result:=String(TJSObject(Error)['message'])
  else if isObject(Error) and isString(TJSObject(Error)['fMessage']) then Result:=String(TJSObject(Error)['fMessage'])
  else Result:=String(Error);
end;
function BoolText(const B:Boolean):String;
begin if B then Result:='true' else Result:='false'; end;

constructor TWorkspaceUiWorkflow.Create;
begin
  inherited Create;
  document.body.setAttribute('data-self-test','pending');
  document.body.setAttribute('data-workspace-ui-self-test','pending');
  document.body.setAttribute('data-workspace-ui-stage-count','0');
end;
function TWorkspaceUiWorkflow.Child:TJSDocument;
begin Result:=FFrame.contentDocument; end;
function TWorkspaceUiWorkflow.E(const Id:String):TJSElement;
begin
  Result:=Child.getElementById(Id);
  if Result=nil then raise Exception.Create('actual UI element absent: '+Id);
end;
function TWorkspaceUiWorkflow.Value(const Id:String):String;
begin Result:=TJSHTMLInputElement(E(Id)).value; end;
function TWorkspaceUiWorkflow.Text(const Id:String):String;
begin Result:=E(Id).textContent; end;
function TWorkspaceUiWorkflow.Attr(const Name:String):String;
begin Result:=Child.body.getAttribute(Name); end;
function TWorkspaceUiWorkflow.Button(const Action:String):TJSHTMLElement;
begin
  Result:=TJSHTMLElement(Child.querySelector('[data-action="'+Action+'"]'));
  if Result=nil then raise Exception.Create('actual visible action absent: '+Action);
end;
procedure TWorkspaceUiWorkflow.Check(const Condition:Boolean;const Detail:String);
begin
  Inc(FChecks);
  if not Condition then raise Exception.Create('step '+IntToStr(FStep)+' / '+Detail);
end;
procedure TWorkspaceUiWorkflow.HasText(const Id,Needle:String);
begin Check(Pos(Needle,Text(Id))>0,Id+' missing '+Needle); end;
procedure TWorkspaceUiWorkflow.Dispatch(const Node:TJSElement;const EventName:String);
var ChildWindow:TJSWindow;
begin
  ChildWindow:=FFrame.contentWindow;
  asm Node.dispatchEvent(new ChildWindow.Event(EventName,{bubbles:true})); end;
end;
procedure TWorkspaceUiWorkflow.SetValue(const Id,NewValue:String);
var Node:TJSElement;
begin
  Node:=E(Id);Check(not Node.hasAttribute('disabled'),'editable visible input '+Id);
  TJSHTMLInputElement(Node).value:=NewValue;
  Dispatch(Node,'input');Dispatch(Node,'change');
end;
procedure TWorkspaceUiWorkflow.SetChecked(const Id:String;const Checked:Boolean);
var Node:TJSElement;
begin
  Node:=E(Id);Check(not Node.hasAttribute('disabled'),'editable visible checkbox '+Id);
  TJSHTMLInputElement(Node).checked:=Checked;Dispatch(Node,'change');
end;
procedure TWorkspaceUiWorkflow.Click(const Action:String);
var Node:TJSHTMLElement;
begin
  Node:=Button(Action);Check(not Node.hasAttribute('disabled'),'enabled visible button '+Action);
  Node.click;
end;
procedure TWorkspaceUiWorkflow.CheckQueued;
var Nodes:TJSNodeList;I:Integer;Node:TJSElement;
begin
  Check(E('workbench').getAttribute('aria-busy')='true','queued work is visibly busy');
  Check(Attr('data-state')='busy','body reports queued busy state');
  Nodes:=E('workbench').querySelectorAll('input,textarea,select,button');
  for I:=0 to Nodes.length-1 do
  begin
    Node:=TJSElement(Nodes[I]);
    if Node.id<>'cancel-operation' then
      Check(Node.hasAttribute('disabled'),'all authoring controls disabled while queued: '+Node.id);
  end;
  Check(not E('cancel-operation').hasAttribute('disabled'),'queued cancel remains enabled');
end;
procedure TWorkspaceUiWorkflow.Action(const Name,ExpectedResult:String);
begin
  FWaitingAction:=Name;FWaitingResult:=ExpectedResult;FActionStarted:=window.performance.now;
  document.body.setAttribute('data-workspace-ui-action',Name);
  Click(Name);
  if E('workbench').getAttribute('aria-busy')='true' then CheckQueued
  else Check((ExpectedResult='rejected') and (Attr('data-last-operation')=Name) and
    (Attr('data-operation-result')='rejected'),'only actual preflight refusal may complete before queuing');
end;
procedure TWorkspaceUiWorkflow.Mark(const Name:String);
begin
  Check(document.body.getAttribute('data-workspace-ui-'+Name)<>'passed','stage completed once');
  document.body.setAttribute('data-workspace-ui-'+Name,'passed');
  Inc(FStages);document.body.setAttribute('data-workspace-ui-stage-count',IntToStr(FStages));
  WriteLn('[PASS] actual workspace UI stage ',Name);
end;
procedure TWorkspaceUiWorkflow.Unchanged;
begin
  Check(Text('action-count')=IntToStr(FCount),'exact retained action count');
  Check(Attr('data-publication-revision')=IntToStr(FPublication),'exact publication revision');
end;
procedure TWorkspaceUiWorkflow.State(const Current,Baseline:Boolean);
begin
  Check(Attr('data-workspace-current')=BoolText(Current),'actual currentness');
  Check(Attr('data-has-baseline')=BoolText(Baseline),'actual retained baseline flag');
end;
procedure TWorkspaceUiWorkflow.Accept(const Current,Baseline:Boolean);
begin Inc(FCount);Inc(FPublication);Unchanged;State(Current,Baseline); end;
procedure TWorkspaceUiWorkflow.Roots(const Indices:array of Integer);
var Nodes:TJSNodeList;I,J:Integer;Selected:Boolean;Node:TJSHTMLInputElement;
begin
  Nodes:=E('repair-roots').querySelectorAll('input[type=checkbox]');
  for I:=0 to Nodes.length-1 do
  begin
    Node:=TJSHTMLInputElement(Nodes[I]);Selected:=False;
    for J:=0 to High(Indices) do if Node.value=IntToStr(Indices[J]) then Selected:=True;
    SetChecked(Node.id,Selected);
  end;
end;
procedure TWorkspaceUiWorkflow.DomainNone;
var Nodes:TJSNodeList;I:Integer;
begin
  Nodes:=E('domain-tokens').querySelectorAll('input[type=checkbox]');
  Check(Nodes.length>0,'actual public vocabulary checkboxes exist');
  for I:=0 to Nodes.length-1 do SetChecked(TJSElement(Nodes[I]).id,False);
end;
procedure TWorkspaceUiWorkflow.Cell(const PassIndex,X:Integer);
begin
  SetValue('view-pass',IntToStr(PassIndex));SetValue('cell-x',IntToStr(X));
  SetValue('cell-y','0');SetValue('cell-z','0');
end;
procedure TWorkspaceUiWorkflow.GeometryWidth(const Width:Integer);
var I:Integer;
begin for I:=1 to 3 do SetValue('geometry-'+IntToStr(I)+'-x',IntToStr(Width)); end;
procedure TWorkspaceUiWorkflow.RequireResources;
var Entries:TJSArray;Entry:TJSObject;I:Integer;Bundle,Style:Boolean;
  Sheets:TJSStyleSheetList;Sheet:TJSStyleSheet;Loaded:Boolean;Status:JSValue;Name:String;
begin
  Bundle:=False;Style:=False;Loaded:=False;
  Entries:=TJSArray(FFrame.contentWindow.performance.getEntriesByType('resource'));
  for I:=0 to Entries.length-1 do
  begin
    Entry:=TJSObject(Entries[I]);if not isString(Entry['name']) then Continue;
    Name:=String(Entry['name']);Status:=Entry['responseStatus'];
    if isNumber(Status) then Check(Double(Status)<400,'actual staged resource status '+Name);
    if Name=TJSURL.new('BrowserPipelineWorkspace.js',FExpectedUrl).href then Bundle:=True;
    if Name=TJSURL.new('workspace.css',FExpectedUrl).href then Style:=True;
  end;
  Sheets:=Child.styleSheets;
  for I:=0 to Sheets.length-1 do
  begin
    Sheet:=Sheets.item(I);
    if Sheet.href=TJSURL.new('workspace.css',FExpectedUrl).href then
      Loaded:=TJSCSSStyleSheet(Sheet).cssRules.length>0;
  end;
  Check(Bundle and Style and Loaded,'actual compiled entry and nonempty stylesheet loaded');
end;
procedure TWorkspaceUiWorkflow.ImportFile(const Payload:String);
var Node:TJSElement;ChildWindow:TJSWindow;
begin
  Node:=E('journal-file');ChildWindow:=FFrame.contentWindow;
  Check(not Node.hasAttribute('disabled'),'actual file input enabled');
  FWaitingAction:='import-journal-draft';FWaitingResult:='completed';FActionStarted:=window.performance.now;
  { Browser-provided File/DataTransfer populate the actual input; the app's
    unchanged change/FileReader handlers still perform every byte check. }
  asm
    const transfer=new ChildWindow.DataTransfer();
    transfer.items.add(new ChildWindow.File([Payload],'ui-workflow.journal',{type:'text/plain'}));
    Node.files=transfer.files;
    Node.dispatchEvent(new ChildWindow.Event('change',{bubbles:true}));
  end;
  CheckQueued;
end;

procedure TWorkspaceUiWorkflow.Start;
begin
  try
    document.body.setAttribute('data-self-test','pending');
    FGroup:='startup';FStarted:=window.performance.now;FActionStarted:=FStarted;
    FExpectedUrl:=TJSURL.new('demo-entries/workspace/index.html',window.location.href).href;
    FFrame:=TJSHTMLIFrameElement(document.createElement('iframe'));
    FFrame.width:='1280';FFrame.height:='1000';FFrame.setAttribute('title','Actual pipeline workspace workflow');
    FFrame.setAttribute('style','position:absolute;left:-12000px;width:1280px;height:1000px;border:0');
    FFrame.src:=FExpectedUrl;document.body.appendChild(FFrame);
    window.setTimeout(@Poll,POLL_MS);
  except Fail(ErrorText(JSExceptValue)); end;
end;

procedure TWorkspaceUiWorkflow.Advance;
var Node:TJSElement;I:Integer;Tampered:String;
begin
  case FStep of
    0:begin
      RequireResources;Unchanged;State(False,False);
      Check((Value('recipe-draft')='') and (Value('run-draft')='') and (Value('journal-draft')=''),'startup has no hidden preset or epoch');
      Check(E('initial').hasAttribute('disabled') and E('apply-run').hasAttribute('disabled'),'no execution buttons before explicit epoch');
      HasText('live-heading','No epoch');Mark('startup');FGroup:='cancelled-queue';
      Action('preset-sequence','cancelled');Click('cancel');
    end;
    1:begin
      Unchanged;State(False,False);Check(Value('recipe-draft')='','cancelled preset never published drafts');
      Check(E('cancel-operation').hasAttribute('disabled'),'cancel disabled after queue cancellation');
      Mark('cancelled-queue');FGroup:='preset-drafts';Action('preset-sequence');
    end;
    2:begin
      Unchanged;State(False,False);
      Check((Value('recipe-draft')<>'') and (Value('run-draft')<>''),'preset creates actual canonical drafts only');
      Check(E('geometry-rows').querySelectorAll('tr').length=4,'all four real preset pass rows');
      Check(E('recipe-download').getAttribute('aria-disabled')='false','actual inspected recipe download prepared');
      Node:=E('sequence-order').closest('details');Check(Node<>nil,'preset settings have their actual disclosure');
      if not Node.hasAttribute('open') then TJSHTMLElement(Node.querySelector('summary')).click;
      Check(Node.hasAttribute('open'),'preset settings opened through the visible disclosure');
      SetValue('sequence-order','1');SetValue('sequence-boundary','wrap');SetValue('sequence-extent','wrap');
      SetValue('strategy','one-way');SetValue('pass-backtracks','0');
      SetValue('geometry-0-x','2');SetValue('geometry-0-y','1');
      GeometryWidth(4);for I:=1 to 3 do SetChecked('geometry-'+IntToStr(I)+'-wrap',True);
      Action('preset-rebuild');
    end;
    3:begin
      Unchanged;FInitialRecipe:=Value('recipe-draft');FInitialRun:=Value('run-draft');
      Check(Pos('seed=7'+#10,FInitialRun)>0,'explicit requested seed retained in draft');
      Action('inspect-definition');
    end;
    4:begin
      Unchanged;State(False,False);HasText('definition-report','GRAPH-FREE');
      HasText('definition-report','all-pass cell instances 14');Mark('preset-drafts');
      FGroup:='initial-once';Action('begin');
    end;
    5:begin
      Accept(False,False);Check(not E('initial').hasAttribute('disabled'),'new epoch allows initial once');
      Action('initial');Button('initial').click;
    end;
    6:begin
      Accept(True,True);Check(E('initial').hasAttribute('disabled'),'actual initial disables further initial attempts');
      HasText('initial-note','already recorded');Mark('initial-once');
      FGroup:='draft-isolation';Action('copy-journal');
    end;
    7:begin
      Unchanged;FBaselineJournal:=Value('journal-draft');Check(FBaselineJournal<>'','actual solved history copied');
      Action('preset-landscape');
    end;
    8:begin
      Unchanged;State(True,True);Check(Value('recipe-draft')<>FInitialRecipe,'other preset replaces only drafts');
      Check(Value('journal-draft')=FBaselineJournal,'preset draft never rewrites saved live journal draft');
      Action('copy-journal');
    end;
    9:begin
      Unchanged;Check(Value('journal-draft')=FBaselineJournal,'live journal itself unchanged by preset draft');
      Action('copy-live');
    end;
    10:begin
      Unchanged;Check((Value('recipe-draft')=FInitialRecipe) and (Value('run-draft')=FInitialRun),'explicit copy restores full original live definition');
      Mark('draft-isolation');FGroup:='seed-epochs';SetValue('seed','8');Action('stage-options');
    end;
    11:begin
      Unchanged;State(True,True);Check(Pos('seed=8'+#10,Value('run-draft'))>0,'new seed staged only');
      Action('apply-run','rejected');
    end;
    12:begin
      Unchanged;State(True,True);Check(Text('error')<>'','seed replacement refused visibly');
      Action('copy-journal');
    end;
    13:begin
      Unchanged;Check(Value('journal-draft')=FBaselineJournal,'refused seed application leaves all live history bytes unchanged');
      Action('begin');
    end;
    14:begin
      Accept(False,False);Check(not E('initial').hasAttribute('disabled'),'explicit seed epoch resets initial permission and baseline');
      Action('initial');
    end;
    15:begin
      Accept(True,True);Mark('seed-epochs');FGroup:='cell-inputs';
      Cell(0,1);SetValue('cell-token','closed');Action('set-lock');
    end;
    16:begin Accept(False,True);Action('inspect-cell');end;
    17:begin Unchanged;HasText('cell-report','Lock row: closed');Action('clear-lock');end;
    18:begin Accept(False,True);Action('inspect-cell');end;
    19:begin
      Unchanged;HasText('cell-report','No lock row.');DomainNone;Action('set-domain');
    end;
    20:begin Accept(False,True);Action('inspect-cell');end;
    21:begin
      Unchanged;HasText('cell-report','Explicit domain row: []');Mark('cell-inputs');
      FGroup:='failed-repair';Roots([0]);Action('preview-repair');
    end;
    22:begin
      Unchanged;HasText('scope-report','Requested roots: [0]');
      Check(not E('repair').hasAttribute('disabled'),'explicit repair authorized after baseline');
      Action('repair');
    end;
    23:begin
      Accept(False,True);HasText('status','unsuccessful solve');HasText('outcome-report','Solved False');
      Check(E('repair').hasAttribute('disabled'),'used preview invalidated');
      Action('clear-domain');
    end;
    24:begin Accept(False,True);Action('inspect-cell');end;
    25:begin Unchanged;HasText('cell-report','No domain row');Roots([0]);Action('preview-repair');end;
    26:begin Unchanged;Action('repair');end;
    27:begin
      Accept(True,True);Mark('failed-repair');FGroup:='private-scope';
      Cell(3,0);SetValue('view-width','4');SetValue('view-height','1');Action('render');
    end;
    28:begin
      Unchanged;Node:=E('map-output').querySelector('g[data-cell-index="0"]');
      Check(Node<>nil,'actual alias cell rendered');FLockToken:=Node.getAttribute('data-token');
      Check(FLockToken<>'','existing generated token available through SVG');
      SetValue('cell-token',FLockToken);Action('set-lock');
    end;
    29:begin
      Accept(False,True);HasText('repair-roots','1 / sequence-states [private provider]');
      Roots([3]);Action('preview-repair');
    end;
    30:begin
      Unchanged;HasText('scope-report','Requested roots: [3]');
      Check(E('repair').hasAttribute('disabled'),'alias root does not silently authorize provider');
      HasText('scope-report','Missing permission: [1');
      Roots([1]);Action('preview-repair');
    end;
    31:begin
      Unchanged;HasText('scope-report','Requested roots: [1]');HasText('scope-report','Active closure: [1, 2, 3]');
      Check(not E('repair').hasAttribute('disabled'),'explicit private provider root enables required closure');
      Action('repair');
    end;
    32:begin
      Accept(True,True);HasText('outcome-report','Requested roots: [1]');
      HasText('outcome-report','Active closure: [1, 2, 3]');Mark('private-scope');
      Action('copy-journal');
    end;
    33:begin
      Unchanged;FSolvedJournal:=Value('journal-draft');FSolvedCount:=FCount;
      FGroup:='geometry';Action('live-geometry');
    end;
    34:begin Unchanged;SetValue('geometry-3-x','3');Action('mapped-epoch','rejected');end;
    35:begin
      Unchanged;State(True,True);Check(Text('error')<>'','linked alias resize refusal visible');
      GeometryWidth(8);Action('mapped-epoch');
    end;
    36:begin
      Accept(False,False);Check(Value('geometry-3-x')='8','larger actual epoch retained');
      Cell(3,7);SetValue('cell-token',FLockToken);Action('set-lock');
    end;
    37:begin Accept(False,False);GeometryWidth(2);Action('mapped-epoch','rejected');end;
    38:begin
      Unchanged;State(False,False);Check(Pos('lock',LowerCase(Text('error')))>0,'authored out-of-range lock caused shrink refusal');
      Action('inspect-cell');
    end;
    39:begin
      Unchanged;HasText('cell-report','Lock row: '+FLockToken);
      FGroup:='view-window';SetValue('view-x','0');SetValue('view-y','0');SetValue('view-z','0');
      SetValue('view-width','2');SetValue('view-height','1');Action('render');
    end;
    40:begin
      Unchanged;Node:=E('map-output').querySelector('svg');
      Check(Node<>nil,'real read-only SVG view exists');
      Check(Node.getAttribute('data-cells')='8,1,1','view window never shrinks live composition');
      Check(Node.getAttribute('data-local-window')='0,0,0,2,1','exact requested independent window');
      Check(E('map-output').querySelectorAll('g[data-cell-index]').length=2,'only requested view cells displayed');
      Check(Node.getAttribute('data-status')='not-current','ungenerated epoch view not mislabeled current');
      Mark('view-window');FGroup:='geometry';Action('clear-lock');
    end;
    41:begin Accept(False,False);GeometryWidth(2);Action('mapped-epoch');end;
    42:begin
      Accept(False,False);Check(Value('geometry-3-x')='2','explicit compatible smaller epoch accepted');
      Check(E('initial').hasAttribute('disabled')=False,'smaller new epoch still requires its own initial solve');
      Mark('geometry');FGroup:='journal-claims';Action('copy-journal');
    end;
    43:begin
      Unchanged;FSavedDraft:=Value('journal-draft');
      Check(Pos('action-count='+IntToStr(FCount)+#10,FSavedDraft)>0,'complete cumulative history retained across new epochs');
      Action('inspect-journal');
    end;
    44:begin
      Unchanged;State(False,False);HasText('journal-report','UNVERIFIED CLAIMS');
      Check(Pos('solved%3D1%0A',FSolvedJournal)>0,'actual saved solve evidence exists for a same-length claim forgery');
      Tampered:=StringReplace(FSolvedJournal,'solved%3D1%0A','solved%3D0%0A',[]);
      Check((Tampered<>FSolvedJournal) and (Length(Tampered)=Length(FSolvedJournal)),'tamper changes one claim, not envelope lengths');
      SetValue('journal-draft',Tampered);Action('inspect-journal');
    end;
    45:begin
      Unchanged;State(False,False);HasText('journal-report','UNVERIFIED CLAIMS');
      Action('restore','rejected');
    end;
    46:begin
      Unchanged;State(False,False);HasText('error','first differing evidence byte:');
      Check((Pos('first differing evidence byte: -1',Text('error'))=0) and
        (Pos('Action index: -1',Text('error'))=0),'restore refusal identifies actual replay evidence mismatch, not an unrelated preflight failure');
      Action('copy-journal');
    end;
    47:begin
      Unchanged;Check(Value('journal-draft')=FSavedDraft,'forged restore leaves exact complete live journal unchanged');
      Mark('journal-claims');FGroup:='file-import';
      ImportFile(FSolvedJournal);FWaitingResult:='cancelled';Click('cancel');
    end;
    48:begin
      Unchanged;Check(Value('journal-draft')=FSavedDraft,'cancelled actual file reader leaves draft unchanged');
      ImportFile(#13+FSolvedJournal);FWaitingResult:='rejected';
    end;
    49:begin
      Unchanged;Check(Value('journal-draft')=FSavedDraft,'invalid ASCII/CR import never partially overwrites draft');
      HasText('error','invalid byte');ImportFile(FSolvedJournal);
    end;
    50:begin
      Unchanged;State(False,False);Check(Value('journal-draft')=FSolvedJournal,'actual FileReader copied all selected bytes into draft only');
      HasText('journal-status','not been inspected');Mark('file-import');FGroup:='restored-history';
      Action('restore');
    end;
    51:begin
      FCount:=FSolvedCount;Inc(FPublication);Unchanged;State(True,True);
      HasText('status','Verified replay restored');Check(E('initial').hasAttribute('disabled'),'restored initial remains recorded');
      Action('copy-journal');
    end;
    52:begin
      Unchanged;Check(Value('journal-draft')=FSolvedJournal,'restored live journal matches complete user-imported original bytes');
      Mark('restored-history');Finish;Exit;
    end;
    else raise Exception.Create('unexpected UI workflow step');
  end;
  Inc(FStep);window.setTimeout(@Poll,POLL_MS);
end;

procedure TWorkspaceUiWorkflow.Poll;
var Ready:Boolean;
begin
  if FFinished then Exit;
  try
    document.body.setAttribute('data-self-test','pending');
    document.body.setAttribute('data-workspace-ui-current',FGroup);
    document.body.setAttribute('data-workspace-ui-step',IntToStr(FStep));
    if window.performance.now-FStarted>=RUN_TIMEOUT_MS then
      raise Exception.Create('bounded whole workflow deadline exceeded');
    Ready:=(FFrame.contentWindow<>nil) and (FFrame.contentWindow.location.href=FExpectedUrl) and
      (Child<>nil) and (Child.body<>nil) and (Child.readyState='complete');
    if Ready and (FStep=0) then Ready:=Attr('data-state')='ready'
    else if Ready then
    begin
      if Attr('data-operation-result')='interrupted' then raise Exception.Create('actual app runtime interruption: '+Text('error'));
      Ready:=(E('workbench').getAttribute('aria-busy')='false') and
        (Attr('data-last-operation')=FWaitingAction);
      if Ready then
      begin
        Check(Attr('data-operation-result')=FWaitingResult,'actual terminal action '+FWaitingAction+' expected '+FWaitingResult+', got '+Attr('data-operation-result')+' / '+Text('error'));
        if FWaitingResult='completed' then Check(Text('error')='','completed action has no hidden UI error');
      end;
    end;
    if Ready then begin Advance;Exit;end;
    if window.performance.now-FActionStarted>=ACTION_TIMEOUT_MS then
      raise Exception.Create('actual UI action did not finish: '+FWaitingAction);
    window.setTimeout(@Poll,POLL_MS);
  except Fail(ErrorText(JSExceptValue)); end;
end;
procedure TWorkspaceUiWorkflow.Fail(const Detail:String);
var Evidence,Node:TJSElement;S:String;
begin
  if FFinished then Exit;FFinished:=True;
  document.body.setAttribute('data-workspace-ui-self-test','failed');
  document.body.setAttribute('data-workspace-ui-'+FGroup,'failed');
  document.body.setAttribute('data-self-test','failed');
  document.body.setAttribute('data-self-test-message',Detail);
  Node:=document.getElementById('failure');if Node<>nil then Node.textContent:=Detail;
  WriteLn('[FAIL] actual workspace UI ',FGroup,': ',Detail);
  { Preserve the original child and useful exact visible state. This is a
    diagnostic, not a substitute fake DOM or a retry of the failed operation. }
  try
    S:='step='+IntToStr(FStep)+#10+'group='+FGroup+#10+'error='+Text('error')+#10+
      'status='+Text('status')+#10+'actions='+Text('action-count')+#10+
      'publication='+Text('publication')+#10+'scope='+Text('scope-report')+#10+
      'outcome='+Text('outcome-report')+#10+'journal-report='+Text('journal-report');
    Evidence:=document.createElement('pre');Evidence.id:='workspace-ui-failure-evidence';
    Evidence.textContent:=S;document.body.appendChild(Evidence);
  except { Preserve the original failure even if the child never loaded. } end;
end;
procedure TWorkspaceUiWorkflow.Finish;
var Node:TJSElement;
begin
  Check(FStages=STAGE_COUNT,'all independent actual workflow stages completed');
  FFinished:=True;
  Node:=document.createElement('pre');Node.id:='workspace-ui-restored-journal';
  Node.textContent:=FSolvedJournal;document.body.appendChild(Node);
  WriteLn('Workspace UI workflow checks: ',FChecks,'; stages: ',FStages,'/',STAGE_COUNT);
  document.body.setAttribute('data-workspace-ui-current','complete');
  document.body.setAttribute('data-workspace-ui-checks',IntToStr(FChecks));
  document.body.setAttribute('data-workspace-ui-self-test','passed');
  document.body.setAttribute('data-self-test','passed');
end;
var Workflow:TWorkspaceUiWorkflow;
begin
  Workflow:=TWorkspaceUiWorkflow.Create;
  window.setTimeout(@Workflow.Start,0);
end.
