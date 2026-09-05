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
unit browser_terraces_app;
{$mode delphi}{$H+}
interface
uses Classes, SysUtils, JS, Web, wfc, wfc_voxel3d, wfc_voxel3d_isometric,
  wfc_voxel3d_svg, wfc_terraces3d, wfc_terraces3d_view;
type
  TBrowserTerraces = class
  private
    FOwner: TTerraces3D;
    FScene: TTerraces3DScene;
    FProjection: TVoxel3DProjectedScene;
    FUrl: String;
    FYaw: Integer;
    function E(const Id: String): TJSElement;
    function Input(const Id: String): TJSHTMLInputElement;
    function Number(const Id: String; const Minimum: Integer): Integer;
    function Seed: TGraphSeed;
    function Options: TGraphNegotiationOptions;
    procedure ClearOutput(const State, Message: String);
    procedure NewSession;
    procedure Present;
    procedure Solve(const Full: Boolean; const Stage: TTerraces3DStage);
    function Click(Event: TJSMouseEvent): Boolean;
    function SessionInput(Event: TJSEvent): Boolean;
    function BudgetInput(Event: TJSEvent): Boolean;
    procedure SelfTest;
  public
    destructor Destroy; override;
    procedure Run;
  end;
implementation

function TBrowserTerraces.E(const Id: String): TJSElement;
begin
  Result := document.getElementById(Id);
  if not Assigned(Result) then raise Exception.Create('missing page element '+Id);
end;
function TBrowserTerraces.Input(const Id: String): TJSHTMLInputElement;
begin Result := TJSHTMLInputElement(E(Id)); end;

function TBrowserTerraces.Number(const Id: String; const Minimum: Integer): Integer;
var I,Digit: Integer; S: String;
begin
  S := Input(Id).value; Result := 0;
  if S = '' then raise EConvertError.Create(Id+' needs an integer');
  for I := 1 to Length(S) do
  begin
    if not (S[I] in ['0'..'9']) then raise EConvertError.Create(Id+' needs an integer');
    Digit := Ord(S[I])-Ord('0');
    if Result > (High(Integer)-Digit) div 10 then
      raise EConvertError.Create(Id+' exceeds Integer');
    Result := Result*10+Digit;
  end;
  if Result < Minimum then raise EConvertError.Create(Id+' must be at least '+IntToStr(Minimum));
end;

function TBrowserTerraces.Seed: TGraphSeed;
var I: Integer; Digit: TGraphSeed; S: String;
begin
  S := Input('seed').value; Result := 0;
  if S = '' then raise EConvertError.Create('seed needs an unsigned 32-bit integer');
  for I := 1 to Length(S) do
  begin
    if not (S[I] in ['0'..'9']) then raise EConvertError.Create('invalid seed');
    Digit := Ord(S[I])-Ord('0');
    if Result > (High(TGraphSeed)-Digit) div 10 then raise EConvertError.Create('seed exceeds 4294967295');
    Result := Result*10+Digit;
  end;
end;

function TBrowserTerraces.Options: TGraphNegotiationOptions;
begin
  Result := DefaultTerraces3DOptions;
  Result.SolveOptions.MaxBacktracks := Number('local-budget',0);
  Result.MaxPassBacktracks := Number('pass-budget',0);
end;

procedure TBrowserTerraces.ClearOutput(const State, Message: String);
begin
  FreeAndNil(FProjection); FreeAndNil(FScene);
  if FUrl <> '' then begin TJSURL.revokeObjectURL(FUrl); FUrl := ''; end;
  E('download').removeAttribute('href');
  E('download').setAttribute('aria-disabled','true');
  E('preview').innerHTML := '';
  E('signature').textContent := '-';
  E('status').textContent := Message;
  document.body.setAttribute('data-state',State);
  document.body.removeAttribute('data-signature');
  document.body.removeAttribute('data-view-signature');
end;

procedure TBrowserTerraces.NewSession;
begin
  ClearOutput('dirty','New session. Generate a full baseline.');
  FreeAndNil(FOwner);
  FOwner := TTerraces3D.Create(Number('width',1),Number('height',1),Number('depth',3),Seed);
end;

procedure TBrowserTerraces.Present;
var
  V: TVoxel3DIsometricOptions;
  S: TVoxel3DSvgOptions;
  Svg: String;
  Parts: TJSArray; BlobOptions: TJSBlobInit;
begin
  FreeAndNil(FProjection);
  V := DefaultVoxel3DIsometricOptions; V.Yaw := TVoxel3DViewYaw(FYaw);
  FProjection := ProjectTerraces3D(FOwner,FScene,V);
  S := DefaultVoxel3DSvgOptions;
  S.Title := 'Learned Terraces: terrain, structure, foliage';
  Svg := EncodeVoxel3DProjectedSceneSvg(FProjection,S);
  { Exact same Pascal SVG encoder as the native demo. Only our own escaped
    generated markup reaches this preview. }
  E('preview').innerHTML := Copy(Svg,Pos('<svg ',Svg),Length(Svg));
  E('signature').textContent := FScene.Signature;
  if FUrl <> '' then TJSURL.revokeObjectURL(FUrl);
  Parts := TJSArray.new; Parts.push(Svg);
  BlobOptions := TJSBlobInit.new; BlobOptions.type_ := 'image/svg+xml;charset=utf-8';
  FUrl := TJSURL.createObjectURL(TJSBlob.new(Parts,BlobOptions));
  TJSHTMLAnchorElement(E('download')).href := FUrl;
  TJSHTMLAnchorElement(E('download')).download := 'learned-terraces.svg';
  E('download').setAttribute('aria-disabled','false');
  document.body.setAttribute('data-state','solved');
  document.body.setAttribute('data-signature',FScene.Signature);
  document.body.setAttribute('data-view-signature',Voxel3DSignatureHex(FProjection.Signature));
end;

procedure TBrowserTerraces.Solve(const Full: Boolean; const Stage: TTerraces3DStage);
var
  R: TGraphNegotiationReport;
  Candidate: TTerraces3DScene;
  O: TGraphNegotiationOptions;
  Solved: Boolean;
begin
  ClearOutput('running','Solving the pass composition...');
  O := Options;
  if not Assigned(FOwner) then NewSession;
  Candidate := nil;
  try
    if Full then Solved := FOwner.TryGenerate(O,Candidate,R)
    else Solved := FOwner.TryRegenerateFrom(Stage,O,Candidate,R);
    if not Solved then
    begin
      ClearOutput('error','No composition committed. Status '+IntToStr(Ord(R.Status))+
        '; '+FOwner.ValidationMessage+' Clear conflicting edits or expand the repair scope/budget.');
      Exit;
    end;
    FScene := Candidate; Candidate := nil;
    Present;
    E('status').textContent := 'Validated and committed. Pass backtracks: '+IntToStr(R.PassBacktracks)+
      '. Scope: '+Terraces3DStageName(Stage)+' and descendants.';
  finally Candidate.Free; end;
end;

function TBrowserTerraces.Click(Event: TJSMouseEvent): Boolean;
var Id,Kind,Value: String; X,Y,Z: Integer;
begin
  Result := False;
  Id := TJSElement(Event.currentTarget).id;
  try
    if Id = 'new-session' then NewSession
    else if Id = 'generate' then Solve(True,t3sTerrain)
    else if Id = 'terrain-repair' then Solve(False,t3sTerrain)
    else if Id = 'structure-repair' then Solve(False,t3sStructure)
    else if Id = 'foliage-repair' then Solve(False,t3sFoliage)
    else if Id = 'rotate' then
    begin
      if not Assigned(FScene) then raise EInvalidOperation.Create('generate output before rotating');
      FYaw := (FYaw+1) mod 4; Present;
    end
    else if Id = 'apply' then
    begin
      if not Assigned(FOwner) then NewSession;
      X := Number('x',0); Y := Number('y',0); Z := Number('z',0);
      Kind := TJSHTMLSelectElement(E('edit-kind')).value;
      Value := TJSHTMLSelectElement(E('edit-value')).value;
      if Kind = 'terrain' then FOwner.SetTerrainToken(X,Y,Z,Value)
      else FOwner.SetFoliage(X,Y,Z,Value);
      ClearOutput('dirty','Edit applied at '+IntToStr(X)+','+IntToStr(Y)+','+IntToStr(Z)+
        '. Generate or repair from '+Terraces3DStageName(TTerraces3DStage(FOwner.DirtyStage))+'.');
    end;
  except on Ex: Exception do ClearOutput('error',Ex.Message); end;
end;

function TBrowserTerraces.SessionInput(Event: TJSEvent): Boolean;
begin
  Result := True; FreeAndNil(FOwner);
  ClearOutput('dirty','Session inputs changed. Generate creates a new full baseline.');
end;
function TBrowserTerraces.BudgetInput(Event: TJSEvent): Boolean;
begin
  Result := True;
  ClearOutput('dirty','Search budget changed. Generate or selectively regenerate.');
end;

procedure TBrowserTerraces.SelfTest;
var Base, Structure: String;
  procedure Assert(const OK: Boolean; const Message: String);
  begin if not OK then raise Exception.Create(Message); end;
  procedure Press(const Id: String);
  begin TJSHTMLButtonElement(E(Id)).click; end;
begin
  try
    TJSHTMLSelectElement(E('edit-value')).value := 'ground';
    Assert(TJSHTMLSelectElement(E('edit-value')).value = 'ground','ground is selectable in the real document');
    TJSHTMLSelectElement(E('edit-value')).value := '';
    Press('generate');
    Assert(Assigned(FScene) and (FScene.Signature = '1:6D695B99:2D23CF62'),'initial scene golden');
    Assert(Voxel3DSignatureHex(FProjection.Signature) = 'C3D25917','native SVG view golden');
    Base := FScene.Signature; Structure := Voxel3DSignatureHex(FScene.Structure.Signature);
    Press('foliage-repair');
    Assert(Assigned(FScene) and (Voxel3DSignatureHex(FScene.Structure.Signature) = Structure),'selective provider reuse');
    document.body.setAttribute('data-selective','passed');
    Input('x').value := '0'; Input('y').value := '0'; Input('z').value := '0';
    TJSHTMLSelectElement(E('edit-kind')).value := 'foliage';
    TJSHTMLSelectElement(E('edit-value')).value := 'fern';
    Press('apply');
    Assert(not Assigned(FScene) and not E('download').hasAttribute('href'),'edit invalidation');
    document.body.setAttribute('data-invalidation','passed');
    Press('foliage-repair');
    Assert((document.body.getAttribute('data-state') = 'error') and not Assigned(FScene),'contradiction withheld output');
    TJSHTMLSelectElement(E('edit-value')).value := '';
    Press('apply'); Press('foliage-repair');
    Assert(Assigned(FScene),'clear and recover');
    document.body.setAttribute('data-recovery','passed');
    Input('seed').value := '55';
    Input('seed').dispatchEvent(TJSEvent.new('input'));
    Press('new-session'); Press('generate');
    Assert(Assigned(FScene) and (FScene.Signature <> Base),'seed and new session can generate');
    document.body.setAttribute('data-new-session','passed');
    Input('width').value := '1.5'; Input('width').dispatchEvent(TJSEvent.new('input'));
    Press('generate');
    Assert(not Assigned(FScene) and not E('download').hasAttribute('href'),'fractional dimension cannot expose stale output');
    Input('width').value := '6'; Input('seed').value := '0';
    Press('new-session'); FYaw := 0; Press('generate');
    Assert(Assigned(FScene) and (FScene.Signature = Base),'restore default baseline');
    document.body.setAttribute('data-self-test','passed');
  except on Ex: Exception do
    begin
      document.body.setAttribute('data-self-test','failed');
      document.body.setAttribute('data-self-test-message',Ex.Message);
      E('status').textContent := 'Self-test failed: '+Ex.Message;
    end;
  end;
end;

procedure TBrowserTerraces.Run;
const Buttons: array[0..6] of String =
  ('new-session','generate','terrain-repair','structure-repair','foliage-repair','rotate','apply');
  Inputs: array[0..3] of String = ('seed','width','height','depth');
var I: Integer;
begin
  for I := 0 to High(Buttons) do TJSHTMLButtonElement(E(Buttons[I])).onclick := Click;
  for I := 0 to High(Inputs) do Input(Inputs[I]).oninput := SessionInput;
  Input('local-budget').oninput := BudgetInput;
  Input('pass-budget').oninput := BudgetInput;
  document.body.setAttribute('data-self-test','not-requested');
  if Pos('selftest=1',window.location.search) > 0 then SelfTest
  else TJSHTMLButtonElement(E('generate')).click;
end;

destructor TBrowserTerraces.Destroy;
begin
  if FUrl <> '' then TJSURL.revokeObjectURL(FUrl);
  FProjection.Free; FScene.Free; FOwner.Free;
  inherited Destroy;
end;
end.
