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
unit browser_connected_routes_app;

{$mode delphi}{$H+}

interface

uses
  JS,
  Web,
  SysUtils,
  wfc,
  connected_routes_demo;

type
  TBrowserConnectedRoutesApplication = class
  strict private
    FSession: TConnectedRoutesSession;
    FResult: TConnectedRoutesResult;
    FDownloadUrl: String;

    FCaseSelect, FPortalSelect: TJSHTMLSelectElement;
    FSeedInput, FAllInput, FBacktracksInput, FPassBacktracksInput,
      FTraceInput: TJSHTMLInputElement;
    FGenerateButton, FRepairButton: TJSHTMLButtonElement;
    FStatus, FDetail, FCaseOutput, FPortalOutput, FResultOutput,
      FWitnessOutput, FParticipantsOutput, FPassBacktracksOutput,
      FSignatureOutput, FScopeOutput, FMapOutput, FLegendOutput,
      FSvgPreview, FDiagnosticsOutput: TJSElement;
    FSvgDownload: TJSHTMLAnchorElement;

    function Element(const AId: String): TJSElement;
    procedure BindDocument;
    procedure BindEvents;
    function SelectedCase: TConnectedRoutesCase;
    function SelectedPortal: TConnectedRoutesPortal;
    function ParseSeed(const AText: String): TGraphSeed;
    function ParseNonnegativeInteger(const AText, AName: String): Integer;
    function ReadConfig: TConnectedRoutesConfig;
    procedure RevokeDownload;
    procedure SetDownload(const AText, AFileName: String);
    procedure ClearPresentation(const AState, AStatus,
      ADetail: String);
    procedure Invalidate(const AReason: String);
    procedure UpdateLegend;
    procedure PublishFailure(const AResult: TConnectedRoutesResult);
    procedure PublishSolved(const AResult: TConnectedRoutesResult);
    procedure DrawMap(const AResult: TConnectedRoutesResult);
    procedure RunOperation(const ARepair: Boolean);
    procedure DispatchEvent(const AElement: TJSElement;
      const AEventName: String);
    procedure AssertTest(const ACondition: Boolean;
      const AMessage: String);
    procedure SelfTest;

    function HandleRepairInput(AEvent: TJSEvent): Boolean;
    function HandleSessionInput(AEvent: TJSEvent): Boolean;
    function HandleCaseChange(AEvent: TJSEvent): Boolean;
    function HandleGenerate(AEvent: TJSMouseEvent): Boolean;
    function HandleRepair(AEvent: TJSMouseEvent): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

implementation

const
  TOWN_FIRST_SIGNATURE = '61943F3F';
  TOWN_SECOND_SIGNATURE = '5C735E7A';
  CIRCULATION_FIRST_SIGNATURE = '9F2CC7A4';
  CIRCULATION_SECOND_SIGNATURE = '47E47400';

constructor TBrowserConnectedRoutesApplication.Create;
begin
  inherited Create;
  FSession := nil;
  FResult := Default(TConnectedRoutesResult);
  FDownloadUrl := '';
end;

destructor TBrowserConnectedRoutesApplication.Destroy;
begin
  RevokeDownload;
  FSession.Free;
  inherited Destroy;
end;

function TBrowserConnectedRoutesApplication.Element(
  const AId: String): TJSElement;
begin
  Result := document.getElementById(AId);
  if not Assigned(Result) then
    raise EConnectedRoutesDemo.Create('Connected Routes is missing #' + AId);
end;

procedure TBrowserConnectedRoutesApplication.BindDocument;
begin
  FCaseSelect := TJSHTMLSelectElement(Element('case-select'));
  FPortalSelect := TJSHTMLSelectElement(Element('portal-select'));
  FSeedInput := TJSHTMLInputElement(Element('seed-input'));
  FAllInput := TJSHTMLInputElement(Element('all-input'));
  FBacktracksInput := TJSHTMLInputElement(Element('backtracks-input'));
  FPassBacktracksInput := TJSHTMLInputElement(
    Element('pass-backtracks-input'));
  FTraceInput := TJSHTMLInputElement(Element('trace-input'));
  FGenerateButton := TJSHTMLButtonElement(Element('generate-button'));
  FRepairButton := TJSHTMLButtonElement(Element('repair-button'));
  FStatus := Element('status');
  FDetail := Element('detail');
  FCaseOutput := Element('case-output');
  FPortalOutput := Element('portal-output');
  FResultOutput := Element('result-output');
  FWitnessOutput := Element('witness-output');
  FParticipantsOutput := Element('participants-output');
  FPassBacktracksOutput := Element('pass-backtracks-output');
  FSignatureOutput := Element('signature-output');
  FScopeOutput := Element('scope-output');
  FMapOutput := Element('map-output');
  FLegendOutput := Element('legend-output');
  FSvgDownload := TJSHTMLAnchorElement(Element('svg-download'));
  FSvgPreview := Element('svg-preview');
  FDiagnosticsOutput := Element('diagnostics-output');
end;

procedure TBrowserConnectedRoutesApplication.BindEvents;
begin
  FCaseSelect.onchange := @HandleCaseChange;
  FPortalSelect.onchange := @HandleRepairInput;
  FSeedInput.oninput := @HandleSessionInput;
  FSeedInput.onchange := @HandleSessionInput;
  FAllInput.onchange := @HandleRepairInput;
  FBacktracksInput.oninput := @HandleRepairInput;
  FBacktracksInput.onchange := @HandleRepairInput;
  FPassBacktracksInput.oninput := @HandleRepairInput;
  FPassBacktracksInput.onchange := @HandleRepairInput;
  FTraceInput.onchange := @HandleRepairInput;
  FGenerateButton.onclick := @HandleGenerate;
  FRepairButton.onclick := @HandleRepair;
end;

function TBrowserConnectedRoutesApplication.SelectedCase:
  TConnectedRoutesCase;
begin
  if FCaseSelect.value = 'town' then
    Result := crcTown2D
  else if FCaseSelect.value = 'circulation' then
    Result := crcCirculation3D
  else
    raise EConnectedRoutesDemo.Create('choose a supported domain case');
end;

function TBrowserConnectedRoutesApplication.SelectedPortal:
  TConnectedRoutesPortal;
begin
  if FPortalSelect.value = 'first' then
    Result := crpFirst
  else if FPortalSelect.value = 'second' then
    Result := crpSecond
  else if FPortalSelect.value = 'both' then
    Result := crpBoth
  else if FPortalSelect.value = 'none' then
    Result := crpNone
  else
    raise EConnectedRoutesDemo.Create('choose a supported crossing or shaft');
end;

function TBrowserConnectedRoutesApplication.ParseSeed(
  const AText: String): TGraphSeed;
var
  D, I: Integer;
  S: String;
begin
  S := Trim(AText);
  if S = '' then
    raise EConnectedRoutesDemo.Create(
      'seed requires unsigned decimal digits');
  Result := 0;
  for I := 1 to Length(S) do
  begin
    if not (S[I] in ['0'..'9']) then
      raise EConnectedRoutesDemo.Create(
        'seed requires unsigned decimal digits');
    D := Ord(S[I]) - Ord('0');
    if Result > (Cardinal($FFFFFFFF) - Cardinal(D)) div 10 then
      raise EConnectedRoutesDemo.Create(
        'seed exceeds the unsigned 32-bit range');
    Result := Result * 10 + Cardinal(D);
  end;
end;

function TBrowserConnectedRoutesApplication.ParseNonnegativeInteger(
  const AText, AName: String): Integer;
var
  D, I: Integer;
  S: String;
begin
  S := Trim(AText);
  if S = '' then
    raise EConnectedRoutesDemo.Create(AName +
      ' requires unsigned decimal digits');
  Result := 0;
  for I := 1 to Length(S) do
  begin
    if not (S[I] in ['0'..'9']) then
      raise EConnectedRoutesDemo.Create(AName +
        ' requires unsigned decimal digits');
    D := Ord(S[I]) - Ord('0');
    if Result > (High(Integer) - D) div 10 then
      raise EConnectedRoutesDemo.Create(AName +
        ' exceeds the Integer range');
    Result := Result * 10 + D;
  end;
end;

function TBrowserConnectedRoutesApplication.ReadConfig:
  TConnectedRoutesConfig;
begin
  Result := DefaultConnectedRoutesConfig(SelectedCase);
  Result.Portal := SelectedPortal;
  Result.Seed := ParseSeed(FSeedInput.value);
  Result.RequireAllParticipants := FAllInput.checked;
  Result.MaxBacktracks := ParseNonnegativeInteger(
    FBacktracksInput.value, 'local backtracks');
  Result.MaxPassBacktracks := ParseNonnegativeInteger(
    FPassBacktracksInput.value, 'pass backtracks');
  Result.CaptureTrace := FTraceInput.checked;
end;

procedure TBrowserConnectedRoutesApplication.RevokeDownload;
begin
  if FDownloadUrl <> '' then
  begin
    TJSURL.revokeObjectURL(FDownloadUrl);
    FDownloadUrl := '';
  end;
  if Assigned(FSvgDownload) then
  begin
    FSvgDownload.removeAttribute('href');
    FSvgDownload.removeAttribute('download');
    FSvgDownload.setAttribute('aria-disabled', 'true');
    FSvgDownload.className := 'button-link disabled';
  end;
end;

procedure TBrowserConnectedRoutesApplication.SetDownload(
  const AText, AFileName: String);
var
  LBlob: TJSBlob;
  LOptions: TJSBlobInit;
  LParts: TJSArray;
begin
  RevokeDownload;
  if AText = '' then Exit;
  LParts := TJSArray.new;
  LParts.push(AText);
  LOptions := TJSBlobInit.new;
  LOptions.type_ := 'image/svg+xml;charset=utf-8';
  LBlob := TJSBlob.new(LParts, LOptions);
  FDownloadUrl := TJSURL.createObjectURL(LBlob);
  FSvgDownload.href := FDownloadUrl;
  FSvgDownload.download := AFileName;
  FSvgDownload.setAttribute('aria-disabled', 'false');
  FSvgDownload.className := 'button-link';
end;

procedure TBrowserConnectedRoutesApplication.ClearPresentation(
  const AState, AStatus, ADetail: String);
begin
  FResult := Default(TConnectedRoutesResult);
  FMapOutput.textContent := '';
  FSvgPreview.textContent := '';
  FDiagnosticsOutput.textContent := 'No terminal report is current.';
  RevokeDownload;
  FStatus.textContent := AStatus;
  FDetail.textContent := ADetail;
  FCaseOutput.textContent := FCaseSelect.value;
  FPortalOutput.textContent := FPortalSelect.value;
  FResultOutput.textContent := 'not current';
  FWitnessOutput.textContent := '0';
  FParticipantsOutput.textContent := '0';
  FPassBacktracksOutput.textContent := '0';
  FSignatureOutput.textContent := '-';
  FScopeOutput.textContent := '-';
  document.body.setAttribute('data-state', AState);
  document.body.setAttribute('data-case', FCaseSelect.value);
  document.body.setAttribute('data-signature', '');
  document.body.setAttribute('data-provider-reused', 'false');
  document.body.setAttribute('data-cell-count', '0');
end;

procedure TBrowserConnectedRoutesApplication.Invalidate(
  const AReason: String);
begin
  ClearPresentation('dirty', 'Controls changed', AReason +
    ' No generated route or SVG is current. Generate a new baseline or repair the existing pass lineage.');
end;

procedure TBrowserConnectedRoutesApplication.UpdateLegend;
begin
  if FCaseSelect.value = 'circulation' then
    FLegendOutput.textContent :=
      'E entrance, G gallery terminal, S shaft, # wall, F floor. ' +
      'N/E/S/W are horizontal openings; U/D are reciprocal vertical openings.'
  else
    FLegendOutput.textContent :=
      'R root town, T terminal town, B bridge, W water, L land. ' +
      'N/E/S/W are route openings; orange cells belong to the checked witness.';
end;

function PositionEqualsCell(const APosition: TGraphPosition;
  const AX, AY, AZ: Integer): Boolean;
begin
  Result := (Integer(APosition.X) = AX) and
    (Integer(APosition.Y) = AY) and (Integer(APosition.Z) = AZ);
end;

function IsTerminalCell(const AResult: TConnectedRoutesResult;
  const AX, AY, AZ: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(AResult.RequiredPositions) do
    if PositionEqualsCell(AResult.RequiredPositions[I], AX, AY, AZ) then
      Exit(True);
end;

function BaseGlyph(const AValue: TGraphValue): String;
begin
  if AValue = 'land' then Result := 'L'
  else if AValue = 'water' then Result := 'W'
  else if AValue = 'bridge' then Result := 'B'
  else if AValue = 'floor' then Result := 'F'
  else if AValue = 'wall' then Result := '#'
  else if AValue = 'shaft' then Result := 'S'
  else Result := '?';
end;

function FeatureGlyph(const AValue: TGraphValue): String;
begin
  if AValue = 'town-root' then Result := 'R'
  else if AValue = 'town-terminal' then Result := 'T'
  else if AValue = 'entrance' then Result := 'E'
  else if AValue = 'gallery' then Result := 'G'
  else Result := '';
end;

function PotentialPortGlyph(const AOpenings: TGraphDirections;
  const AParticipant: Boolean): String;
begin
  if not AParticipant then Exit('.');
  Result := '';
  if gdNorth in AOpenings then Result := Result + 'N';
  if gdEast in AOpenings then Result := Result + 'E';
  if gdSouth in AOpenings then Result := Result + 'S';
  if gdWest in AOpenings then Result := Result + 'W';
  if gdUp in AOpenings then Result := Result + 'U';
  if gdDown in AOpenings then Result := Result + 'D';
  if Result = '' then Result := 'o';
end;

function OppositeDirection(
  const ADirection: TGraphDirection): TGraphDirection;
begin
  case ADirection of
    gdNorth: Result := gdSouth;
    gdEast: Result := gdWest;
    gdSouth: Result := gdNorth;
    gdWest: Result := gdEast;
    gdUp: Result := gdDown;
    gdDown: Result := gdUp;
  else
    raise EConnectedRoutesDemo.Create('browser route direction is invalid');
  end;
end;

function HasActualLink(const AResult: TConnectedRoutesResult;
  const AX, AY, AZ: Integer;
  const ADirection: TGraphDirection): Boolean;
var
  I, NX, NY, NZ: Integer;
begin
  Result := False;
  I := ((AZ * AResult.Height) + AY) * AResult.Width + AX;
  if not (ADirection in AResult.Cells[I].Openings) then Exit;
  NX := AX;
  NY := AY;
  NZ := AZ;
  case ADirection of
    gdNorth: Inc(NY);
    gdEast: Inc(NX);
    gdSouth: Dec(NY);
    gdWest: Dec(NX);
    gdUp: Inc(NZ);
    gdDown: Dec(NZ);
  end;
  if (NX < 0) or (NX >= AResult.Width) or
      (NY < 0) or (NY >= AResult.Height) or
      (NZ < 0) or (NZ >= AResult.Depth) then Exit;
  I := ((NZ * AResult.Height) + NY) * AResult.Width + NX;
  Result := OppositeDirection(ADirection) in AResult.Cells[I].Openings;
end;

function ActualLinkGlyph(const AResult: TConnectedRoutesResult;
  const AX, AY, AZ: Integer): String;
var
  D: TGraphDirection;
begin
  Result := '';
  for D := Low(TGraphDirection) to High(TGraphDirection) do
    if HasActualLink(AResult, AX, AY, AZ, D) then
      case D of
        gdNorth: Result := Result + 'N';
        gdEast: Result := Result + 'E';
        gdSouth: Result := Result + 'S';
        gdWest: Result := Result + 'W';
        gdUp: Result := Result + 'U';
        gdDown: Result := Result + 'D';
      end;
  if Result = '' then Result := '.';
end;

function SafeClassValue(const AValue: TGraphValue): String;
begin
  Result := StringReplace(LowerCase(AValue), ' ', '-', [rfReplaceAll]);
end;

procedure TBrowserConnectedRoutesApplication.DrawMap(
  const AResult: TConnectedRoutesResult);
var
  I, X, Y, Z: Integer;
  LBase, LCell, LFeature, LFloor, LGrid, LPorts, LRoute: TJSElement;
  LClass, LLabel: String;
begin
  FMapOutput.textContent := '';
  for Z := 0 to AResult.Depth - 1 do
  begin
    LFloor := document.createElement('section');
    LFloor.className := 'floor-card';
    LBase := document.createElement('h3');
    if AResult.Depth = 1 then
      LBase.textContent := 'Town map'
    else
      LBase.textContent := 'Floor z=' + IntToStr(Z);
    LFloor.appendChild(LBase);
    LGrid := document.createElement('div');
    LGrid.className := 'route-grid';
    LGrid.setAttribute('style', '--columns:' + IntToStr(AResult.Width));
    for Y := AResult.Height - 1 downto 0 do
      for X := 0 to AResult.Width - 1 do
      begin
        I := ((Z * AResult.Height) + Y) * AResult.Width + X;
        LCell := document.createElement('div');
        LClass := 'map-cell base-' +
          SafeClassValue(AResult.Cells[I].BaseValue);
        if AResult.Cells[I].Participant then
          LClass := LClass + ' participant';
        if AResult.Cells[I].OnWitness then
          LClass := LClass + ' witness';
        if PositionEqualsCell(AResult.Root, X, Y, Z) then
          LClass := LClass + ' root';
        if IsTerminalCell(AResult, X, Y, Z) then
          LClass := LClass + ' terminal';
        LCell.className := LClass;
        LCell.setAttribute('data-x', IntToStr(X));
        LCell.setAttribute('data-y', IntToStr(Y));
        LCell.setAttribute('data-z', IntToStr(Z));

        LBase := document.createElement('span');
        LBase.className := 'base-glyph';
        LBase.textContent := BaseGlyph(AResult.Cells[I].BaseValue);
        LCell.appendChild(LBase);
        LRoute := document.createElement('strong');
        LRoute.className := 'route-glyph';
        LRoute.textContent := ActualLinkGlyph(AResult, X, Y, Z);
        LCell.appendChild(LRoute);
        LPorts := document.createElement('span');
        LPorts.className := 'potential-glyph';
        LPorts.textContent := PotentialPortGlyph(AResult.Cells[I].Openings,
          AResult.Cells[I].Participant);
        LPorts.setAttribute('aria-label', 'available ports ' +
          LPorts.textContent);
        LCell.appendChild(LPorts);
        LFeature := document.createElement('span');
        LFeature.className := 'feature-glyph';
        LFeature.textContent := FeatureGlyph(AResult.Cells[I].FeatureValue);
        LCell.appendChild(LFeature);
        LLabel := 'cell (' + IntToStr(X) + ',' + IntToStr(Y) + ',' +
          IntToStr(Z) + '): ' + AResult.Cells[I].BaseValue + ', ' +
          AResult.Cells[I].RouteValue + ', ' +
          AResult.Cells[I].FeatureValue;
        if AResult.Cells[I].OnWitness then
          LLabel := LLabel + ', on rooted witness';
        LCell.setAttribute('aria-label', LLabel);
        LCell.setAttribute('title', LLabel);
        LGrid.appendChild(LCell);
      end;
    LFloor.appendChild(LGrid);
    FMapOutput.appendChild(LFloor);
  end;
end;

procedure TBrowserConnectedRoutesApplication.PublishFailure(
  const AResult: TConnectedRoutesResult);
begin
  FResult := AResult;
  FMapOutput.textContent := '';
  FSvgPreview.textContent := '';
  RevokeDownload;
  FStatus.textContent := 'No route committed';
  FDetail.textContent := AResult.Detail;
  FResultOutput.textContent := AResult.Status;
  FScopeOutput.textContent := 'terminal failure; no public route';
  FDiagnosticsOutput.textContent := AResult.Detail;
  document.body.setAttribute('data-state', AResult.Status);
  document.body.setAttribute('data-signature', '');
  document.body.setAttribute('data-provider-reused', 'false');
  document.body.setAttribute('data-cell-count', '0');
  FRepairButton.disabled := (FSession = nil) or
    (not FSession.HasBaseline);
end;

procedure TBrowserConnectedRoutesApplication.PublishSolved(
  const AResult: TConnectedRoutesResult);
var
  LName: String;
begin
  FResult := AResult;
  DrawMap(AResult);
  FStatus.textContent := 'Solved and independently checked';
  FDetail.textContent := AResult.Detail;
  FCaseOutput.textContent := ConnectedRoutesCaseName(AResult.CaseKind);
  FPortalOutput.textContent := ConnectedRoutesPortalName(AResult.Portal);
  FResultOutput.textContent := AResult.Status;
  FWitnessOutput.textContent := IntToStr(AResult.WitnessCellCount);
  FParticipantsOutput.textContent := IntToStr(AResult.ParticipantCount);
  FPassBacktracksOutput.textContent := IntToStr(AResult.PassBacktracks);
  FSignatureOutput.textContent := AResult.Signature;
  if AResult.WasRepair then
    FScopeOutput.textContent := 'route pass + feature descendant; provider reused'
  else
    FScopeOutput.textContent := 'full three-pass generation';
  FDiagnosticsOutput.textContent := AResult.Detail + LineEnding +
    'Independent reciprocal-port BFS: passed.';
  FSvgPreview.textContent := AResult.SvgText;
  LName := 'connected-routes-' + ConnectedRoutesCaseName(AResult.CaseKind) +
    '-' + LowerCase(AResult.Signature) + '.svg';
  SetDownload(AResult.SvgText, LName);
  document.body.setAttribute('data-state', 'solved');
  document.body.setAttribute('data-case',
    ConnectedRoutesCaseName(AResult.CaseKind));
  document.body.setAttribute('data-signature', AResult.Signature);
  document.body.setAttribute('data-provider-reused',
    LowerCase(BoolToStr(AResult.ProviderReused, True)));
  document.body.setAttribute('data-cell-count',
    IntToStr(Length(AResult.Cells)));
  FRepairButton.disabled := False;
end;

procedure TBrowserConnectedRoutesApplication.RunOperation(
  const ARepair: Boolean);
var
  C, LExisting: TConnectedRoutesConfig;
  LSolved: Boolean;
begin
  ClearPresentation('running', 'Solving...',
    'The public map and SVG stay unavailable until validation completes.');
  try
    C := ReadConfig;
    if ARepair then
    begin
      if (FSession = nil) or (not FSession.HasBaseline) then
        raise EConnectedRoutesDemo.Create(
          'repair needs a successful baseline generation');
      LExisting := FSession.Config;
      if (LExisting.CaseKind <> C.CaseKind) or
          (LExisting.Seed <> C.Seed) then
        raise EConnectedRoutesDemo.Create(
          'case or seed changed; generate a new baseline first');
      FSession.SetSearchLimits(C.MaxBacktracks, C.MaxPassBacktracks,
        C.CaptureTrace);
      FSession.SetRequireAllParticipants(C.RequireAllParticipants);
      FSession.SetPortal(C.Portal);
      LSolved := FSession.Repair(FResult);
    end
    else
    begin
      if FSession = nil then
        FSession := TConnectedRoutesSession.Create(C)
      else
        FSession.Reset(C);
      LSolved := FSession.Generate(FResult);
    end;
    if LSolved then PublishSolved(FResult)
    else PublishFailure(FResult);
  except
    on E: Exception do
    begin
      ClearPresentation('error', 'Invalid request', E.Message);
      FDiagnosticsOutput.textContent := E.ClassName + ': ' + E.Message;
    end;
  end;
end;

procedure TBrowserConnectedRoutesApplication.DispatchEvent(
  const AElement: TJSElement; const AEventName: String);
begin
  AElement.dispatchEvent(TJSEvent.new(AEventName));
end;

procedure TBrowserConnectedRoutesApplication.AssertTest(
  const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise EConnectedRoutesDemo.Create('browser self-test: ' + AMessage);
end;

procedure TBrowserConnectedRoutesApplication.SelfTest;
begin
  document.body.setAttribute('data-self-test', 'running');
  try
    FCaseSelect.value := 'town';
    FPortalSelect.value := 'first';
    FSeedInput.value := '0';
    FAllInput.checked := True;
    FBacktracksInput.value := '4096';
    FPassBacktracksInput.value := '32';
    FTraceInput.checked := False;
    DispatchEvent(FCaseSelect, 'change');
    FGenerateButton.click;
    AssertTest(FResult.Solved and (FResult.Signature = TOWN_FIRST_SIGNATURE),
      'initial town baseline changed: expected ' + TOWN_FIRST_SIGNATURE +
      ', actual ' + FResult.Signature + ', output ' + FResult.OutputKey);

    FPortalSelect.value := 'second';
    DispatchEvent(FPortalSelect, 'change');
    AssertTest((document.body.getAttribute('data-state') = 'dirty') and
      (FMapOutput.childElementCount = 0) and
      (not FSvgDownload.hasAttribute('href')),
      'pending crossing edit retained public output');
    document.body.setAttribute('data-invalidation', 'passed');
    FRepairButton.click;
    AssertTest(FResult.Solved and FResult.ProviderReused and
      (FResult.Signature = TOWN_SECOND_SIGNATURE),
      'town alternate crossing repair changed');
    document.body.setAttribute('data-town-repair', 'passed');

    FPortalSelect.value := 'none';
    DispatchEvent(FPortalSelect, 'change');
    FRepairButton.click;
    AssertTest((not FResult.Solved) and
      (FMapOutput.childElementCount = 0) and
      (not FSvgDownload.hasAttribute('href')) and FSession.HasBaseline,
      'failed repair exposed output or discarded baseline');
    FPortalSelect.value := 'first';
    DispatchEvent(FPortalSelect, 'change');
    FRepairButton.click;
    AssertTest(FResult.Solved and
      (FResult.Signature = TOWN_FIRST_SIGNATURE),
      'town rollback recovery changed');
    document.body.setAttribute('data-town-rollback', 'passed');

    FCaseSelect.value := 'circulation';
    DispatchEvent(FCaseSelect, 'change');
    FPortalSelect.value := 'first';
    FGenerateButton.click;
    AssertTest(FResult.Solved and
      (FResult.Signature = CIRCULATION_FIRST_SIGNATURE) and
      (FResult.WitnessVerticalEdgeCount > 0),
      'cross-floor circulation baseline changed');
    document.body.setAttribute('data-circulation', 'passed');

    FPortalSelect.value := 'second';
    DispatchEvent(FPortalSelect, 'change');
    FRepairButton.click;
    AssertTest(FResult.Solved and FResult.ProviderReused and
      (FResult.Signature = CIRCULATION_SECOND_SIGNATURE) and
      (FResult.WitnessVerticalEdgeCount > 0),
      'cross-floor circulation repair changed');
    document.body.setAttribute('data-circulation-repair', 'passed');

    FSeedInput.value := '1x';
    DispatchEvent(FSeedInput, 'input');
    FGenerateButton.click;
    AssertTest((document.body.getAttribute('data-state') = 'error') and
      (FMapOutput.childElementCount = 0) and
      (not FSvgDownload.hasAttribute('href')),
      'malformed seed retained public output');
    FSeedInput.value := '0';
    FCaseSelect.value := 'circulation';
    FPortalSelect.value := 'first';
    FAllInput.checked := True;
    FBacktracksInput.value := '4096';
    FPassBacktracksInput.value := '32';
    FTraceInput.checked := False;
    DispatchEvent(FSeedInput, 'input');
    FGenerateButton.click;
    AssertTest(FResult.Solved and
      (FResult.Signature = CIRCULATION_FIRST_SIGNATURE) and
      (document.body.getAttribute('data-case') = 'circulation'),
      'final recovery changed');
    document.body.setAttribute('data-self-test', 'passed');
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-self-test', 'failed');
      document.body.setAttribute('data-self-test-message', E.Message);
      FStatus.textContent := 'Self-test failed';
      FDetail.textContent := E.Message;
    end;
  end;
end;

function TBrowserConnectedRoutesApplication.HandleRepairInput(
  AEvent: TJSEvent): Boolean;
begin
  Result := True;
  Invalidate('A generation control changed.');
end;

function TBrowserConnectedRoutesApplication.HandleSessionInput(
  AEvent: TJSEvent): Boolean;
begin
  Result := True;
  Invalidate('The seed changed and needs a new baseline.');
  FRepairButton.disabled := True;
end;

function TBrowserConnectedRoutesApplication.HandleCaseChange(
  AEvent: TJSEvent): Boolean;
begin
  Result := True;
  UpdateLegend;
  Invalidate('The domain case changed.');
  FRepairButton.disabled := True;
end;

function TBrowserConnectedRoutesApplication.HandleGenerate(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  RunOperation(False);
end;

function TBrowserConnectedRoutesApplication.HandleRepair(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  RunOperation(True);
end;

procedure TBrowserConnectedRoutesApplication.Run;
begin
  BindDocument;
  BindEvents;
  document.body.setAttribute('data-self-test', 'not-requested');
  UpdateLegend;
  if Pos('selftest=1', window.location.search) > 0 then
    SelfTest
  else
    RunOperation(False);
end;

end.
