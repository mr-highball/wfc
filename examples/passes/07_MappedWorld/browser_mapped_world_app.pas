{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
unit browser_mapped_world_app;
{$mode delphi}{$H+}
interface
uses JS, Web, SysUtils, wfc, wfc_lattice,
  mapped_world_types, mapped_world_workbench;
type
  TMappedWorldEditError = (mweeCell, mweeDemand);
  TBrowserMappedWorldApplication = class
  private
    FSession: TMappedWorldSession;
    FSafeUrl, FDiagnosticUrl: String;
    FSite, FTestStep: Integer;
    FNeedsReset, FBusy, FTesting, FPresentationInvalid, FEditError, FBound: Boolean;
    FEditErrors: set of TMappedWorldEditError;
    FOperationTimer, FTestTimer: Integer;
    FPendingAction: TMappedWorldAction;
    FOptions: TMappedWorldSearchOptions;
    function Element(const AId: String): TJSElement;
    function Input(const AId: String): TJSHTMLInputElement;
    function Choice(const AId: String): TJSHTMLSelectElement;
    function Button(const AId: String): TJSHTMLButtonElement;
    function Decimal(const AText, AName: String; const AMaximum: Cardinal): Cardinal;
    function SignedDecimal(const AText, AName: String): Integer;
    function Offset(const AText, AName: String): TGraphOffset;
    function ReadConfig: TMappedWorldConfig;
    function ReadOptions: TMappedWorldSearchOptions;
    function EditLayer: TMappedWorldLayer;
    function ReadDemand: TMappedWorldDemand;
    procedure BindEvents;
    procedure UpdateDefinitionControls;
    procedure UpdateEditTokens;
    procedure RevokeDownloads;
    procedure Download(const AId, AText, AName: String; var AUrl: String);
    procedure ClearPresentation(const AState, AMessage: String);
    procedure SetButtons;
    procedure NewSession;
    procedure SelectSite(const AIndex: Integer);
    procedure ShowInspection;
    procedure Publish;
    procedure QueueRun(const AAction: TMappedWorldAction);
    procedure CancelPending;
    procedure ExecutePending;
    procedure Fail(const E: Exception);
    procedure TestStep;
    procedure AssertTest(const ACondition: Boolean; const AMessage: String);
    procedure Dispatch(const AId, AName: String);
    function DefinitionInput(AEvent: TJSEvent): Boolean;
    function LayerInput(AEvent: TJSEvent): Boolean;
    function HandleNew(AEvent: TJSMouseEvent): Boolean;
    function HandleGenerate(AEvent: TJSMouseEvent): Boolean;
    function HandleRepair(AEvent: TJSMouseEvent): Boolean;
    function HandleDemand(AEvent: TJSMouseEvent): Boolean;
    function HandleEdit(AEvent: TJSMouseEvent): Boolean;
    function HandleSite(AEvent: TJSMouseEvent): Boolean;
    function HandleMap(AEvent: TJSMouseEvent): Boolean;
    function HandleRuntimeError(AEvent: TJSEvent): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;
implementation

constructor TBrowserMappedWorldApplication.Create;
begin
  inherited Create;
  FNeedsReset:=True;
end;

destructor TBrowserMappedWorldApplication.Destroy;
var Nodes:TJSNodeList; I:Integer; E:TJSHTMLElement;
begin
  FTesting:=False; CancelPending;
  if FBound then begin
    window.removeEventListener('error',@HandleRuntimeError);
    Nodes:=Element('workbench').querySelectorAll('input,select,button');
    for I:=0 to Nodes.length-1 do begin
      E:=TJSHTMLElement(Nodes[I]); E.onclick:=nil; E.oninput:=nil; E.onchange:=nil;
    end;
    TJSHTMLElement(Element('map-output')).onclick:=nil;
    RevokeDownloads;
  end;
  FSession.Free;
  inherited Destroy;
end;

function TBrowserMappedWorldApplication.Element(const AId: String): TJSElement;
begin
  Result:=document.getElementById(AId);
  if not Assigned(Result) then raise EMappedWorld.Create('Missing page element: '+AId);
end;

function TBrowserMappedWorldApplication.Input(const AId: String): TJSHTMLInputElement;
begin Result:=TJSHTMLInputElement(Element(AId)); end;
function TBrowserMappedWorldApplication.Choice(const AId: String): TJSHTMLSelectElement;
begin Result:=TJSHTMLSelectElement(Element(AId)); end;
function TBrowserMappedWorldApplication.Button(const AId: String): TJSHTMLButtonElement;
begin Result:=TJSHTMLButtonElement(Element(AId)); end;

function TBrowserMappedWorldApplication.Decimal(const AText, AName: String;
  const AMaximum: Cardinal): Cardinal;
var S:String; I:Integer; D:Cardinal;
begin
  Result:=0; S:=Trim(AText);
  if S='' then raise EMappedWorld.Create(AName+' needs decimal digits');
  for I:=1 to Length(S) do begin
    if not (S[I] in ['0'..'9']) then raise EMappedWorld.Create(AName+' needs decimal digits');
    D:=Ord(S[I])-Ord('0');
    if (D>AMaximum) or (Result>(AMaximum-D) div 10) then
      raise EMappedWorld.Create(AName+' exceeds its integer range');
    Result:=Result*10+D;
  end;
end;

function TBrowserMappedWorldApplication.SignedDecimal(const AText, AName: String): Integer;
var S:String; N:Cardinal;
begin
  S:=Trim(AText);
  if (S<>'') and (S[1]='-') then begin
    N:=Decimal(Copy(S,2,Length(S)),AName,Cardinal(High(Integer))+1);
    if N=Cardinal(High(Integer))+1 then Result:=Low(Integer) else Result:=-Integer(N);
  end else Result:=Integer(Decimal(S,AName,High(Integer)));
end;

function TBrowserMappedWorldApplication.Offset(const AText, AName: String): TGraphOffset;
var I,P,N:Integer; Parts:array[0..2] of String;
begin
  P:=1; N:=0;
  for I:=1 to Length(AText)+1 do
    if (I>Length(AText)) or (AText[I]=',') then begin
      if N>2 then raise EMappedWorld.Create(AName+' needs exactly X,Y,Z');
      Parts[N]:=Copy(AText,P,I-P); Inc(N); P:=I+1;
    end;
  if N<>3 then raise EMappedWorld.Create(AName+' needs exactly X,Y,Z');
  Result:=MakeGraphOffset(SignedDecimal(Parts[0],AName+' X'),
    SignedDecimal(Parts[1],AName+' Y'),SignedDecimal(Parts[2],AName+' Z'));
end;

function TBrowserMappedWorldApplication.ReadConfig: TMappedWorldConfig;
begin
  Result:=DefaultMappedWorldConfig;
  Result.Seed:=Decimal(Input('seed-input').value,'Seed',High(Cardinal));
  case Choice('preset-select').value of
    'interior':Result.Preset:=mwpInteriorStudy;
    'sandbox':Result.Preset:=mwpLandscapeSandbox;
    else raise EMappedWorld.Create('Choose a supported preset');
  end;
  case Choice('sampling-select').value of
    'cell':Result.Sampling:=mwsCell;
    'point':Result.Sampling:=mwsPointStudy;
    'region':Result.Sampling:=mwsRegion;
    else raise EMappedWorld.Create('Choose a supported sampling model');
  end;
  if Result.Sampling=mwsRegion then begin
    Result.RegionMinimum:=Offset(Input('region-min-input').value,'Region minimum');
    Result.RegionMaximum:=Offset(Input('region-max-input').value,'Region maximum');
  end;
  if Result.Preset=mwpLandscapeSandbox then begin
    Result.LandWeight:=Integer(Decimal(Input('land-weight-input').value,'Land weight',High(Integer)));
    Result.WaterWeight:=Integer(Decimal(Input('water-weight-input').value,'Water weight',High(Integer)));
    Result.ClearWeight:=Integer(Decimal(Input('clear-weight-input').value,'Clear weight',High(Integer)));
    Result.TreeWeight:=Integer(Decimal(Input('tree-weight-input').value,'Tree weight',High(Integer)));
  end;
  ValidateMappedWorldConfig(Result);
end;

function TBrowserMappedWorldApplication.ReadOptions: TMappedWorldSearchOptions;
begin
  Result:=DefaultMappedWorldSearchOptions;
  Result.MaxBacktracks:=Integer(Decimal(Input('backtracks-input').value,'Local backtracks',High(Integer)));
  Result.MaxPassBacktracks:=Integer(Decimal(Input('pass-backtracks-input').value,'Pass backtracks',High(Integer)));
  Result.Negotiated:=Input('negotiated-input').checked;
  Result.CaptureTrace:=Input('trace-input').checked;
  ValidateMappedWorldSearchOptions(Result);
end;

function TBrowserMappedWorldApplication.EditLayer: TMappedWorldLayer;
begin
  case Choice('edit-layer-select').value of
    'terrain':Result:=mwlTerrain;
    'foliage':Result:=mwlFoliage;
    else raise EMappedWorld.Create('Choose terrain or foliage to edit');
  end;
end;

function TBrowserMappedWorldApplication.ReadDemand: TMappedWorldDemand;
begin
  case Choice('demand-select').value of
    'vacant':Result:=mwdVacant;
    'optional':Result:=mwdOptional;
    'required':Result:=mwdRequired;
    else raise EMappedWorld.Create('Choose a supported house demand');
  end;
end;

procedure TBrowserMappedWorldApplication.BindEvents;
const DefinitionIds:array[0..8] of String=('seed-input','region-min-input',
  'region-max-input','land-weight-input','water-weight-input','clear-weight-input',
  'tree-weight-input','preset-select','sampling-select');
var I:Integer; E:TJSNodeList;
begin
  window.addEventListener('error',@HandleRuntimeError);
  for I:=0 to High(DefinitionIds) do begin
    TJSHTMLElement(Element(DefinitionIds[I])).oninput:=@DefinitionInput;
    TJSHTMLElement(Element(DefinitionIds[I])).onchange:=@DefinitionInput;
  end;
  Choice('edit-layer-select').onchange:=@LayerInput;
  Button('new-button').onclick:=@HandleNew;
  Button('generate-button').onclick:=@HandleGenerate;
  Button('repair-button').onclick:=@HandleRepair;
  Button('demand-button').onclick:=@HandleDemand;
  Button('domain-button').onclick:=@HandleEdit;
  Button('allow-both-button').onclick:=@HandleEdit;
  Button('clear-domain-button').onclick:=@HandleEdit;
  Button('lock-button').onclick:=@HandleEdit;
  Button('clear-lock-button').onclick:=@HandleEdit;
  E:=Element('site-buttons').querySelectorAll('[data-site]');
  for I:=0 to E.length-1 do TJSHTMLButtonElement(E[I]).onclick:=@HandleSite;
  TJSHTMLElement(Element('map-output')).onclick:=@HandleMap;
  FBound:=True;
end;

procedure TBrowserMappedWorldApplication.UpdateDefinitionControls;
var Sandbox,Region:Boolean;
begin
  Sandbox:=Choice('preset-select').value='sandbox';
  Region:=Choice('sampling-select').value='region';
  if Region then Element('region-fields').removeAttribute('hidden')
  else Element('region-fields').setAttribute('hidden','');
  Input('region-min-input').disabled:=not Region;
  Input('region-max-input').disabled:=not Region;
  Input('land-weight-input').disabled:=not Sandbox;
  Input('water-weight-input').disabled:=not Sandbox;
  Input('clear-weight-input').disabled:=not Sandbox;
  Input('tree-weight-input').disabled:=not Sandbox;
  if Sandbox then Element('preset-disclosure').textContent:=
    'Sandbox: every terrain and foliage cell starts free. Housing starts optional. Your later domains, locks and demands remain explicit. Budgets may be exhausted.'
  else Element('preset-disclosure').textContent:=
    'Study zoning: 48 terrain cells land; 767 foliage cells clear; one genuinely free site at (7,7). All housing starts vacant. No caller locks. These are initial constraints; edits may change them.';
end;

procedure TBrowserMappedWorldApplication.UpdateEditTokens;
var V:TGraphValues; I:Integer; O:TJSHTMLOptionElement;
begin
  V:=MappedWorldTokens(EditLayer); Choice('edit-token-select').innerHTML:='';
  for I:=0 to High(V) do begin
    O:=TJSHTMLOptionElement(document.createElement('option'));
    O.value:=V[I]; O.textContent:=V[I]; Choice('edit-token-select').appendChild(O);
  end;
end;

procedure TBrowserMappedWorldApplication.RevokeDownloads;
  procedure Revoke(const AId:String; var AUrl:String);
  begin
    if AUrl<>'' then TJSURL.revokeObjectURL(AUrl);
    AUrl:='';
    Element(AId).removeAttribute('href'); Element(AId).removeAttribute('download');
    Element(AId).setAttribute('aria-disabled','true');
  end;
begin Revoke('svg-download',FSafeUrl); Revoke('diagnostic-download',FDiagnosticUrl); end;

procedure TBrowserMappedWorldApplication.Download(const AId, AText, AName: String; var AUrl: String);
var Parts:TJSArray; Options:TJSBlobInit; LBlob:TJSBlob; A:TJSHTMLAnchorElement;
begin
  if AUrl<>'' then TJSURL.revokeObjectURL(AUrl);
  AUrl:=''; A:=TJSHTMLAnchorElement(Element(AId));
  A.removeAttribute('href'); A.removeAttribute('download'); A.setAttribute('aria-disabled','true');
  if AText='' then Exit;
  Parts:=TJSArray.new; Parts.push(AText); Options:=TJSBlobInit.new;
  Options.type_:='image/svg+xml;charset=utf-8'; LBlob:=TJSBlob.new(Parts,Options);
  AUrl:=TJSURL.createObjectURL(LBlob); A.href:=AUrl; A.download:=AName;
  A.setAttribute('aria-disabled','false');
end;

procedure TBrowserMappedWorldApplication.ClearPresentation(const AState, AMessage: String);
begin
  RevokeDownloads;
  document.body.setAttribute('data-state',AState);
  document.body.setAttribute('data-signature','');
  document.body.setAttribute('data-model-valid','false');
  document.body.setAttribute('data-physical-safe','false');
  document.body.removeAttribute('data-inspected-site');
  document.body.removeAttribute('data-blocker-count');
  document.body.removeAttribute('data-foliage-samples');
  document.body.removeAttribute('data-inspection-current');
  Element('status').textContent:=AMessage;
  Element('map-output').innerHTML:='';
  Element('model-output').textContent:='Not current';
  Element('physical-output').textContent:='Not current';
  Element('signature-output').textContent:='—';
  Element('inspection-output').textContent:='No current result is published.';
  Element('report-output').textContent:='No current search report. '+AMessage;
  SetButtons;
end;

procedure TBrowserMappedWorldApplication.SetButtons;
var Nodes:TJSNodeList; I:Integer; E:TJSElement;
begin
  //CSS pointer blocking alone does not stop keyboard edits while a queued
  //solve is using its already captured configuration and search options.
  Nodes:=Element('workbench').querySelectorAll('input,select,button');
  for I:=0 to Nodes.length-1 do begin
    E:=TJSElement(Nodes[I]);
    if FBusy then E.setAttribute('disabled','') else E.removeAttribute('disabled');
  end;
  if not FBusy then UpdateDefinitionControls;
  Button('new-button').disabled:=FBusy;
  Button('generate-button').disabled:=FBusy or FNeedsReset or not Assigned(FSession);
  Button('repair-button').disabled:=FBusy or FNeedsReset or not Assigned(FSession);
  if Assigned(FSession) and not FSession.HasBaseline then Button('repair-button').disabled:=True;
  Button('demand-button').disabled:=FBusy or FNeedsReset or not Assigned(FSession);
  Button('domain-button').disabled:=Button('demand-button').disabled;
  Button('allow-both-button').disabled:=Button('demand-button').disabled;
  Button('clear-domain-button').disabled:=Button('demand-button').disabled;
  Button('lock-button').disabled:=Button('demand-button').disabled;
  Button('clear-lock-button').disabled:=Button('demand-button').disabled;
end;

procedure TBrowserMappedWorldApplication.NewSession;
var C:TMappedWorldConfig; Replacement:TMappedWorldSession;
begin
  if FBusy then Exit;
  C:=ReadConfig;
  Replacement:=TMappedWorldSession.Create(C);
  FSession.Free; FSession:=Replacement; FNeedsReset:=False; FSite:=0;
  CancelPending;
  FPresentationInvalid:=False; FEditError:=False; FEditErrors:=[];
  ClearPresentation('idle','New session ready. Generate a baseline.');
  Element('detail').textContent:='All entries are undecided. Previous outputs and caller edits have been discarded.';
  Element('report-output').textContent:='No search has run.';
  SelectSite(0);
end;

procedure TBrowserMappedWorldApplication.SelectSite(const AIndex: Integer);
var Nodes:TJSNodeList; I:Integer; D:TMappedWorldDemands;
begin
  RequireMappedWorldInteger(AIndex,0,5,'site'); FSite:=AIndex;
  Nodes:=Element('site-buttons').querySelectorAll('[data-site]');
  for I:=0 to Nodes.length-1 do
    if I=FSite then TJSElement(Nodes[I]).setAttribute('aria-pressed','true')
    else TJSElement(Nodes[I]).setAttribute('aria-pressed','false');
  if Assigned(FSession) then begin
    D:=FSession.CopyDemands;
    case D[FSite] of
      mwdVacant:Choice('demand-select').value:='vacant';
      mwdOptional:Choice('demand-select').value:='optional';
      mwdRequired:Choice('demand-select').value:='required';
    end;
    if not FNeedsReset then Publish;
  end;
end;

procedure TBrowserMappedWorldApplication.ShowInspection;
var A:TMappedWorldInspection; I:Integer; S:String;
  function Vector(const V:TWfcLatticeVector):String;
  begin Result:='('+IntToStr(V.X)+','+IntToStr(V.Y)+','+IntToStr(V.Z)+')'; end;
  procedure Samples(const Title:String; const Values:TMappedWorldSamples);
  var J:Integer; V:TMappedWorldSample;
  begin
    S:=S+#10+Title+': '+IntToStr(Length(Values))+' cells'+#10;
    for J:=0 to High(Values) do begin
      V:=Values[J]; S:=S+MappedWorldLayerName(V.Layer)+' '+Vector(V.Position)+' '+V.Cell.Value;
      S:=S+' world ['+Vector(V.Bounds.Minimum)+', '+Vector(V.Bounds.Maximum)+')';
      if V.Cell.Locked then S:=S+' [caller lock]' else if V.Cell.Generated then S:=S+' [generated]';
      if V.Cell.HasDomain then S:=S+' [zoned]';
      if V.IsCorner then S:=S+' [corner]';
      if V.Accepted then S:=S+' [accepted]' else S:=S+' [rejected]';
      S:=S+#10;
    end;
  end;
begin
  if not FSession.CopyInspection(FSite mod 3,FSite div 3,A) then Exit;
  S:=A.Banner+#10+'Site '+IntToStr(A.SiteX)+','+IntToStr(A.SiteY)+'; revision '+IntToStr(A.Revision)+#10+
    'House: ['+Vector(A.HouseBounds.Minimum)+', '+Vector(A.HouseBounds.Maximum)+')'+#10+
    'Foliage query: ['+Vector(A.QueryBounds.Minimum)+', '+Vector(A.QueryBounds.Maximum)+')'+#10+
    'Terrain query in bounds: '+BoolToStr(A.TerrainQueryInBounds,True)+#10+
    'Foliage query in bounds: '+BoolToStr(A.FoliageQueryInBounds,True)+#10+
    'Physical blockers: '+IntToStr(Length(A.PhysicalBlockers));
  Samples('Terrain samples',A.TerrainSamples); Samples('Foliage samples',A.FoliageSamples);
  if Length(A.PhysicalBlockers)>0 then Samples('Complete-footprint blockers',A.PhysicalBlockers);
  Element('inspection-output').textContent:=S;
  document.body.setAttribute('data-inspected-site',IntToStr(FSite));
  document.body.setAttribute('data-blocker-count',IntToStr(Length(A.PhysicalBlockers)));
  if A.IsCurrent then document.body.setAttribute('data-inspection-current','true')
  else document.body.setAttribute('data-inspection-current','false');
  I:=Length(A.FoliageSamples); document.body.setAttribute('data-foliage-samples',IntToStr(I));
end;

procedure TBrowserMappedWorldApplication.Publish;
var R:TMappedWorldResult; S:String;
begin
  if not Assigned(FSession) or FNeedsReset or FBusy or FPresentationInvalid then Exit;
  RevokeDownloads;
  if FSession.CopyCurrent(R) then begin
    document.body.setAttribute('data-state','solved');
    document.body.setAttribute('data-signature',IntToHex(R.Signature,8));
    if R.ModelValid then begin
      Element('model-output').textContent:='Satisfied'; document.body.setAttribute('data-model-valid','true');
    end else Element('model-output').textContent:='Failed';
    if R.PhysicalSafe then begin
      Element('physical-output').textContent:='Clear'; document.body.setAttribute('data-physical-safe','true');
      Element('status').textContent:='Solved and independently checked.';
    end else begin
      Element('physical-output').textContent:='UNSAFE STUDY'; document.body.setAttribute('data-physical-safe','false');
      Element('status').textContent:='Selected model satisfied — full house footprint is NOT clear.';
    end;
    Element('signature-output').textContent:=IntToStr(R.Revision)+' / '+IntToHex(R.Signature,8);
    if FSession.TryCurrentSvg(FSite mod 3,FSite div 3,S) then begin
      Element('map-output').innerHTML:=S;
      Download('svg-download',S,'mapped-world-'+IntToHex(R.Signature,8)+'.svg',FSafeUrl);
    end;
    if FSession.TryDiagnosticSvg(FSite mod 3,FSite div 3,S) then begin
      if not R.PhysicalSafe then Element('map-output').innerHTML:=S;
      Download('diagnostic-download',S,'mapped-world-diagnostic.svg',FDiagnosticUrl);
    end;
    Element('detail').textContent:='Select any site to inspect its exact provider cells. Safe SVG requires both model validity and physical clearance.';
  end else begin
    //Retained baseline inspection is explicitly diagnostic, never current output.
    if FSession.TryDiagnosticSvg(FSite mod 3,FSite div 3,S) then begin
      Element('map-output').innerHTML:=S;
      Download('diagnostic-download',S,'mapped-world-NOT-CURRENT.svg',FDiagnosticUrl);
    end;
  end;
  ShowInspection;
  Element('report-output').textContent:=FSession.RunReportText;
  SetButtons;
end;

procedure TBrowserMappedWorldApplication.QueueRun(const AAction: TMappedWorldAction);
begin
  if FBusy then Exit;
  if FNeedsReset or not Assigned(FSession) then raise EMappedWorld.Create('Start a new session first');
  if FEditError then raise EMappedWorld.Create('Correct and apply the rejected edit, or start a new session');
  FOptions:=ReadOptions; FPresentationInvalid:=False; FPendingAction:=AAction; FBusy:=True;
  ClearPresentation('solving','Solving the explicitly authorized scope…');
  //Allow the busy state to paint. The core solve itself is synchronous; no
  //cancellation or elapsed-time bound is promised by this adapter.
  FOperationTimer:=window.setTimeout(@ExecutePending,0);
end;

procedure TBrowserMappedWorldApplication.CancelPending;
begin
  if FOperationTimer<>0 then window.clearTimeout(FOperationTimer);
  if FTestTimer<>0 then window.clearTimeout(FTestTimer);
  FOperationTimer:=0; FTestTimer:=0; FBusy:=False;
end;

procedure TBrowserMappedWorldApplication.ExecutePending;
var Solved:Boolean;
begin
  if (FOperationTimer=0) or not FBusy then Exit;
  FOperationTimer:=0;
  try
    Solved:=FSession.Run(FPendingAction,FOptions); FBusy:=False;
    if not Solved then ClearPresentation('failed','Search did not publish a current result. Read the exact status below.');
    Publish;
  except on E:Exception do begin FBusy:=False; Fail(E); end; end;
  SetButtons;
  if FTesting then FTestTimer:=window.setTimeout(@TestStep,0);
end;

procedure TBrowserMappedWorldApplication.Fail(const E: Exception);
begin
  FPresentationInvalid:=True;
  ClearPresentation('error',E.Message);
  Element('detail').textContent:='No output is current in this presentation. Correct the input or start a new session.';
end;

function TBrowserMappedWorldApplication.DefinitionInput(AEvent: TJSEvent): Boolean;
begin
  Result:=True; if FBusy then Exit;
  FNeedsReset:=True; UpdateDefinitionControls;
  ClearPresentation('dirty','Definition changed. Start a new session to use it.');
end;

function TBrowserMappedWorldApplication.LayerInput(AEvent: TJSEvent): Boolean;
begin Result:=True; if not FBusy then UpdateEditTokens; end;
function TBrowserMappedWorldApplication.HandleNew(AEvent: TJSMouseEvent): Boolean;
begin Result:=False; try NewSession; except on E:Exception do Fail(E); end; end;
function TBrowserMappedWorldApplication.HandleGenerate(AEvent: TJSMouseEvent): Boolean;
begin Result:=False; try QueueRun(mwaGenerate); except on E:Exception do Fail(E); end; end;
function TBrowserMappedWorldApplication.HandleRepair(AEvent: TJSMouseEvent): Boolean;
var A:TMappedWorldAction;
begin
  Result:=False;
  try
    case Choice('scope-select').value of
      'housing':A:=mwaHousingOnly;
      'foliage':A:=mwaFoliageAndHousing;
      'all':A:=mwaAllPasses;
      else raise EMappedWorld.Create('Choose an explicit repair scope');
    end;
    QueueRun(A);
  except on E:Exception do Fail(E); end;
end;

function TBrowserMappedWorldApplication.HandleDemand(AEvent: TJSMouseEvent): Boolean;
begin
  Result:=False; if FBusy or FNeedsReset then Exit;
  try
    FSession.SetDemand(FSite mod 3,FSite div 3,ReadDemand);
    Exclude(FEditErrors,mweeDemand); FEditError:=FEditErrors<>[];
    FPresentationInvalid:=FEditError;
    if FEditError then ClearPresentation('error','Correct and apply the rejected cell edit, or start a new session.')
    else begin ClearPresentation('dirty','House demand changed. Repair the selected scope.'); Publish; end;
  except on E:Exception do begin Include(FEditErrors,mweeDemand); FEditError:=True; Fail(E); end; end;
end;

function TBrowserMappedWorldApplication.HandleEdit(AEvent: TJSMouseEvent): Boolean;
var L:TMappedWorldLayer; X,Y:Integer; Id,V:String; Values:TGraphValues;
begin
  Result:=False; if FBusy or FNeedsReset then Exit;
  try
    L:=EditLayer; X:=Integer(Decimal(Input('edit-x-input').value,'Cell X',High(Integer)));
    Y:=Integer(Decimal(Input('edit-y-input').value,'Cell Y',High(Integer)));
    Id:=TJSElement(AEvent.currentTarget).id; V:=Choice('edit-token-select').value;
    if Id='domain-button' then begin SetLength(Values,1); Values[0]:=V; FSession.SetDomain(L,X,Y,Values); end
    else if Id='allow-both-button' then FSession.SetDomain(L,X,Y,MappedWorldTokens(L))
    else if Id='clear-domain-button' then FSession.ClearDomain(L,X,Y)
    else if Id='lock-button' then FSession.SetLock(L,X,Y,V)
    else if Id='clear-lock-button' then FSession.ClearLock(L,X,Y)
    else raise EMappedWorld.Create('Unsupported edit action');
    Exclude(FEditErrors,mweeCell); FEditError:=FEditErrors<>[];
    FPresentationInvalid:=FEditError;
    if FEditError then ClearPresentation('error','Correct and apply the rejected house demand, or start a new session.')
    else begin ClearPresentation('dirty','Caller constraint changed. Choose a repair scope that includes its layer.'); Publish; end;
  except on E:Exception do begin Include(FEditErrors,mweeCell); FEditError:=True; Fail(E); end; end;
end;

function TBrowserMappedWorldApplication.HandleSite(AEvent: TJSMouseEvent): Boolean;
begin
  Result:=False; if FBusy then Exit;
  try SelectSite(Integer(Decimal(TJSElement(AEvent.currentTarget).getAttribute('data-site'),'Site',5)));
  except on E:Exception do Fail(E); end;
end;

function TBrowserMappedWorldApplication.HandleMap(AEvent: TJSMouseEvent): Boolean;
var E:TJSElement;
begin
  Result:=False; if FBusy then Exit;
  E:=TJSElement(AEvent.target);
  while Assigned(E) and (E<>Element('map-output')) do begin
    if E.hasAttribute('data-house-index') then begin
      SelectSite(Integer(Decimal(E.getAttribute('data-house-index'),'Site',5))); Exit;
    end;
    E:=E.parentElement;
  end;
end;

function TBrowserMappedWorldApplication.HandleRuntimeError(AEvent: TJSEvent): Boolean;
var MessageText:String;
begin
  Result:=False; CancelPending; FPresentationInvalid:=True;
  MessageText:=String(TJSObject(AEvent)['message']);
  //Runtime/DOM errors are not necessarily Pascal Exception objects. Publish a
  //terminal failure and revoke artifacts instead of leaving a stale busy state.
  ClearPresentation('error','Unexpected browser error');
  Element('detail').textContent:=MessageText;
  if FTesting then begin
    FTesting:=False;
    document.body.setAttribute('data-self-test','failed');
    document.body.setAttribute('data-self-test-message','Step '+IntToStr(FTestStep)+': '+MessageText);
  end;
  SetButtons;
end;

procedure TBrowserMappedWorldApplication.AssertTest(const ACondition: Boolean; const AMessage: String);
begin if not ACondition then raise EMappedWorld.Create('Browser self-test: '+AMessage); end;
procedure TBrowserMappedWorldApplication.Dispatch(const AId, AName: String);
begin Element(AId).dispatchEvent(TJSEvent.new(AName)); end;

procedure TBrowserMappedWorldApplication.TestStep;
var R:TMappedWorldResult; OldUrl:String;
begin
  FTestTimer:=0;
  if not FTesting then Exit;
  try
    case FTestStep of
      0:begin
        document.body.setAttribute('data-self-test','running');
        Choice('preset-select').value:='interior'; Choice('sampling-select').value:='cell';
        Input('seed-input').value:='3'; Dispatch('seed-input','input');
        Button('new-button').click;
        AssertTest(not FSession.HasBaseline,'new session retained a baseline');
        Inc(FTestStep); Button('generate-button').click;
      end;
      1:begin
        AssertTest(FSession.CopyCurrent(R) and R.ModelValid and R.PhysicalSafe,'study baseline failed');
        AssertTest(R.Layers[mwlFoliage].Cells[7+7*32].Value='tree','study did not generate the interior tree');
        AssertTest(Element('map-output').querySelectorAll('[data-layer="terrain"][data-cell-index]').length=48,'terrain renderer count');
        AssertTest(Element('svg-download').hasAttribute('href'),'safe baseline SVG unavailable');
        AssertTest(document.body.getAttribute('data-blocker-count')='1','inspector missed interior blocker');
        document.body.setAttribute('data-baseline','passed');
        OldUrl:=FSafeUrl; Choice('demand-select').value:='required'; Button('demand-button').click;
        AssertTest(not Element('svg-download').hasAttribute('href') and (FSafeUrl=''),'dirty demand retained safe export');
        AssertTest(FDiagnosticUrl<>OldUrl,'diagnostic reused safe URL');
        Choice('scope-select').value:='housing'; Inc(FTestStep); Button('repair-button').click;
      end;
      2:begin
        AssertTest(not FSession.HasCurrent and FSession.HasBaseline,'housing-only did not fail atomically');
        AssertTest(not Element('svg-download').hasAttribute('href'),'failed repair leaked safe output');
        document.body.setAttribute('data-housing-failure','passed');
        Choice('scope-select').value:='foliage'; Inc(FTestStep); Button('repair-button').click;
      end;
      3:begin
        AssertTest(FSession.CopyCurrent(R) and R.PhysicalSafe,'authorized foliage repair failed');
        AssertTest(R.Layers[mwlHousing].Cells[0].Value='house','required house not generated');
        AssertTest(R.Layers[mwlFoliage].Cells[7+7*32].Value='clear','interior blocker not repaired');
        document.body.setAttribute('data-upstream-repair','passed');
        Choice('sampling-select').value:='point'; Dispatch('sampling-select','change');
        AssertTest(not Element('svg-download').hasAttribute('href') and FNeedsReset,'definition did not invalidate');
        Button('new-button').click; Inc(FTestStep); Button('generate-button').click;
      end;
      4:begin
        AssertTest(FSession.HasCurrent,'point baseline failed');
        Choice('demand-select').value:='required'; Button('demand-button').click;
        Choice('scope-select').value:='housing'; Inc(FTestStep); Button('repair-button').click;
      end;
      5:begin
        AssertTest(FSession.CopyCurrent(R) and R.ModelValid and not R.PhysicalSafe,'point counterexample missing');
        AssertTest(not Element('svg-download').hasAttribute('href') and Element('diagnostic-download').hasAttribute('href'),'unsafe export gating failed');
        AssertTest(Pos('UNSAFE',Element('map-output').textContent)>0,'unsafe diagnostic not labeled');
        document.body.setAttribute('data-point-counterexample','passed');
        Input('seed-input').value:='bad'; Dispatch('seed-input','input'); Button('new-button').click;
        AssertTest(document.body.getAttribute('data-state')='error','bad seed was accepted');
        AssertTest(not Element('diagnostic-download').hasAttribute('href'),'bad definition retained diagnostic');
        Input('seed-input').value:='3'; Choice('sampling-select').value:='cell'; Dispatch('seed-input','input');
        Button('new-button').click; Inc(FTestStep); Button('generate-button').click;
      end;
      6:begin
        AssertTest(FSession.HasCurrent,'seed/new-session Generate recovery failed');
        Choice('edit-layer-select').value:='foliage'; Dispatch('edit-layer-select','change');
        Input('edit-x-input').value:='7'; Input('edit-y-input').value:='7'; Choice('edit-token-select').value:='clear';
        Button('domain-button').click; Choice('scope-select').value:='housing';
        Inc(FTestStep); Button('repair-button').click;
      end;
      7:begin
        AssertTest(FSession.Status=mwstScopeMismatch,'edited provider was silently reused');
        document.body.setAttribute('data-scope-guard','passed');
        Choice('scope-select').value:='foliage'; Inc(FTestStep); Button('repair-button').click;
      end;
      8:begin
        AssertTest(FSession.HasCurrent,'explicit edited-provider repair failed');
        document.body.setAttribute('data-invalidation','passed');
        Input('edit-x-input').value:='7.5'; Button('domain-button').click;
        AssertTest(FEditError and (document.body.getAttribute('data-state')='error'),'fractional cell edit accepted');
        TJSHTMLButtonElement(Element('site-buttons').querySelector('[data-site="1"]')).click;
        AssertTest(not Element('svg-download').hasAttribute('href') and
          (Element('map-output').childElementCount=0),'read-only selection republished rejected edit output');
        Button('demand-button').click;
        AssertTest(FEditError and FPresentationInvalid and
          not Element('svg-download').hasAttribute('href'),'unrelated demand cleared the rejected cell edit');
        Choice('demand-select').value:='unsupported'; Button('demand-button').click;
        Choice('demand-select').value:='vacant'; Button('demand-button').click;
        Button('generate-button').click;
        AssertTest(FEditError and FPresentationInvalid and not FBusy and
          (FEditErrors=[mweeCell]) and not Element('diagnostic-download').hasAttribute('href') and
          (Element('map-output').childElementCount=0),'correcting demand forgot an earlier rejected cell edit');
        AssertTest(not document.body.hasAttribute('data-blocker-count') and
          (Pos('status=solved',Element('report-output').textContent)=0),'rejected edit retained solved diagnostics');
        Input('edit-x-input').value:='7'; Button('domain-button').click;
        AssertTest(not FEditError,'correcting both edit groups did not recover');
        Choice('demand-select').value:='unsupported'; Button('demand-button').click;
        Input('edit-x-input').value:='7.5'; Button('domain-button').click;
        Input('edit-x-input').value:='7'; Button('domain-button').click;
        Button('repair-button').click;
        AssertTest(FEditError and FPresentationInvalid and not FBusy and
          (FEditErrors=[mweeDemand]) and not Element('svg-download').hasAttribute('href') and
          not Element('diagnostic-download').hasAttribute('href') and
          (Element('map-output').childElementCount=0),'correcting cell forgot an earlier rejected demand');
        Choice('demand-select').value:='vacant'; Button('demand-button').click;
        AssertTest(not FEditError and not FPresentationInvalid,'both corrected edit groups remained blocked');
        document.body.setAttribute('data-edit-error-isolation','passed');
        Inc(FTestStep); Button('repair-button').click;
      end;
      9:begin
        AssertTest(FSession.HasCurrent and not FEditError,'corrected edit did not recover');
        Choice('preset-select').value:='sandbox'; Dispatch('preset-select','change');
        Input('land-weight-input').value:='invalid'; Button('new-button').click;
        AssertTest(document.body.getAttribute('data-state')='error','invalid sandbox weight was accepted');
        Choice('preset-select').value:='interior'; Dispatch('preset-select','change');
        Button('new-button').click; Inc(FTestStep); Button('generate-button').click;
      end;
      10:begin
        AssertTest(FSession.CopyCurrent(R) and R.PhysicalSafe and
          (R.Config.Preset=mwpInteriorStudy),'disabled sandbox weight prevented interior generation');
        Choice('preset-select').value:='sandbox'; Dispatch('preset-select','change');
        Button('new-button').click;
        AssertTest((document.body.getAttribute('data-state')='error') and FNeedsReset and
          not Element('svg-download').hasAttribute('href'),'invalid sandbox weight was forgotten across preset change');
        Input('land-weight-input').value:='12'; Button('new-button').click;
        document.body.setAttribute('data-preset-input-isolation','passed');
        Inc(FTestStep); Button('generate-button').click;
      end;
      11:begin
        AssertTest(FSession.CopyCurrent(R) and R.ModelValid,'sandbox baseline failed');
        AssertTest(not R.Layers[mwlFoliage].Cells[0].HasDomain and
          not R.Layers[mwlFoliage].Cells[100].HasDomain and
          not R.Layers[mwlTerrain].Cells[0].HasDomain,'sandbox retained study zoning');
        document.body.setAttribute('data-sandbox','passed');
        Choice('preset-select').value:='interior'; Choice('sampling-select').value:='region';
        Input('region-min-input').value:='-1,-1,0'; Input('region-max-input').value:='9,9,1';
        Dispatch('preset-select','change'); Button('new-button').click;
        Inc(FTestStep); Button('generate-button').click;
      end;
      12:begin
        AssertTest(FSession.HasCurrent and (document.body.getAttribute('data-foliage-samples')='100'),'setback region did not inspect100 cells');
        Choice('demand-select').value:='required'; Button('demand-button').click;
        Choice('scope-select').value:='foliage'; Inc(FTestStep); Button('repair-button').click;
      end;
      13:begin
        AssertTest(FSession.CopyCurrent(R) and R.PhysicalSafe,'setback repair failed');
        AssertTest(document.body.getAttribute('data-foliage-samples')='100','setback inspector changed after repair');
        document.body.setAttribute('data-region','passed');
        Choice('edit-layer-select').value:='foliage'; Dispatch('edit-layer-select','change');
        Input('edit-x-input').value:='7'; Input('edit-y-input').value:='7';
        Choice('edit-token-select').value:='tree'; Button('lock-button').click;
        Inc(FTestStep); Button('repair-button').click;
      end;
      14:begin
        AssertTest(not FSession.HasCurrent and FSession.HasBaseline,'locked interior tree was silently cleared');
        AssertTest(not Element('svg-download').hasAttribute('href'),'locked contradiction retained safe export');
        Button('clear-lock-button').click; Inc(FTestStep); Button('repair-button').click;
      end;
      15:begin
        AssertTest(FSession.CopyCurrent(R) and R.PhysicalSafe,'cleared lock did not permit repair');
        AssertTest(not R.Layers[mwlFoliage].Cells[7+7*32].Locked and
          R.Layers[mwlFoliage].Cells[7+7*32].Generated,'cleared lock did not become generated output');
        document.body.setAttribute('data-lock-lifecycle','passed');
        //Dispatch a real browser error between queuing and executing a solve.
        //The handler must cancel that callback and revoke all presentation.
        FTesting:=False; Button('generate-button').click;
        AssertTest(FBusy and (FOperationTimer<>0),'runtime-error test did not queue a solve');
        window.dispatchEvent(TJSErrorEvent.new('error'));
        AssertTest(not FBusy and (FOperationTimer=0) and (FTestTimer=0) and
          FPresentationInvalid and not Element('svg-download').hasAttribute('href') and
          (Element('map-output').childElementCount=0),'runtime error did not cancel queued publication');
        FTesting:=True; Inc(FTestStep); FTestTimer:=window.setTimeout(@TestStep,0);
      end;
      16:begin
        AssertTest(FPresentationInvalid and not FBusy and
          (document.body.getAttribute('data-state')='error') and
          (Element('map-output').childElementCount=0),'canceled callback republished output');
        AssertTest(not document.body.hasAttribute('data-blocker-count') and
          not document.body.hasAttribute('data-inspection-current') and
          (Element('model-output').textContent='Not current'),'runtime error retained solved diagnostics');
        Button('new-button').click; Inc(FTestStep); Button('generate-button').click;
      end;
      17:begin
        AssertTest(FSession.CopyCurrent(R) and R.PhysicalSafe and not FPresentationInvalid,
          'new session did not recover from canceled runtime-error work');
        document.body.setAttribute('data-queued-error-lifecycle','passed');
        document.body.setAttribute('data-self-test','passed'); FTesting:=False;
      end;
    end;
  except on E:Exception do begin
    FTesting:=False; FBusy:=False; Fail(E);
    document.body.setAttribute('data-self-test','failed');
    document.body.setAttribute('data-self-test-message',E.Message);
  end; end;
end;

procedure TBrowserMappedWorldApplication.Run;
begin
  BindEvents; UpdateDefinitionControls; UpdateEditTokens;
  if Pos('selftest=1',window.location.search)>0 then begin
    FTesting:=True; FTestStep:=0; TestStep;
  end else begin
    try NewSession; except on E:Exception do Fail(E); end;
  end;
end;
end.
