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
unit browser_world_app;

{$mode delphi}{$H+}

interface

uses
  JS,
  Web,
  SysUtils,
  wfc,
  wfc_world2d,
  wfc_world2d_validate,
  world2d_showcase;

type
  TBrowserWorldApplication = class
  strict private
    FWorld: TWorld2D;
    FCanvases: array[TWorld2DLayer] of TJSHTMLCanvasElement;
    FContexts: array[TWorld2DLayer] of TJSCanvasRenderingContext2D;
    FSeedInput: TJSHTMLInputElement;
    FGenerateButton: TJSHTMLButtonElement;
    FNextSeedButton: TJSHTMLButtonElement;
    FWrapInput: TJSHTMLInputElement;
    FResetButton: TJSHTMLButtonElement;
    FClearLocksButton: TJSHTMLButtonElement;
    FSelectedXInput: TJSHTMLInputElement;
    FSelectedYInput: TJSHTMLInputElement;
    FLockLayerSelect: TJSHTMLSelectElement;
    FLockValueSelect: TJSHTMLSelectElement;
    FLockButton: TJSHTMLButtonElement;
    FUnlockButton: TJSHTMLButtonElement;
    FStatusElement: TJSElement;
    FSignatureElement: TJSElement;
    FValidationElement: TJSElement;
    FSolverReportElement: TJSElement;
    FSelectedX: TGraphCoordinate;
    FSelectedY: TGraphCoordinate;

    function RequireElement(const AId: String): TJSElement;
    procedure BindDocument;
    procedure BindEvents;
    procedure SetBodyState(const AState, ASignature: String);
    procedure ShowError(const AMessage: String);

    function TryParseSeed(const AText: String;
      out ASeed: TGraphSeed): Boolean;
    function ReadSeed: TGraphSeed;
    function SelectedLayer: TWorld2DLayer;
    procedure ReadSelectedCoordinates;
    procedure RefreshLockValues;
    procedure RefreshInspector;

    procedure RecreateShowcase;
    procedure ClearAllLocks;
    procedure GenerateWorld;
    procedure RenderWorld;
    procedure RenderLayer(const ALayer: TWorld2DLayer);
    function ColorFor(const ALayer: TWorld2DLayer;
      const AValue: TGraphValue): String;
    function TextColorFor(const ALayer: TWorld2DLayer;
      const AValue: TGraphValue): String;
    function GlyphFor(const ALayer: TWorld2DLayer;
      const AValue: TGraphValue): String;
    function SolverReportText(const AReport: TGraphSolveReport): String;
    procedure SelectCanvasCell(const ALayer: TWorld2DLayer;
      AEvent: TJSMouseEvent);
    function HandleCanvasKeyDown(const ALayer: TWorld2DLayer;
      AEvent: TJSKeyboardEvent): Boolean;
    procedure RunSelfTest;

    function HandleGenerate(AEvent: TJSMouseEvent): Boolean;
    function HandleNextSeed(AEvent: TJSMouseEvent): Boolean;
    function HandleReset(AEvent: TJSMouseEvent): Boolean;
    function HandleClearLocks(AEvent: TJSMouseEvent): Boolean;
    function HandleLock(AEvent: TJSMouseEvent): Boolean;
    function HandleUnlock(AEvent: TJSMouseEvent): Boolean;
    function HandleWrapChange(AEvent: TJSEvent): Boolean;
    function HandleCoordinateChange(AEvent: TJSEvent): Boolean;
    function HandleLayerChange(AEvent: TJSEvent): Boolean;
    function HandleTerrainClick(AEvent: TJSMouseEvent): Boolean;
    function HandleBiomeClick(AEvent: TJSMouseEvent): Boolean;
    function HandleFoliageClick(AEvent: TJSMouseEvent): Boolean;
    function HandleTerrainKeyDown(AEvent: TJSKeyboardEvent): Boolean;
    function HandleBiomeKeyDown(AEvent: TJSKeyboardEvent): Boolean;
    function HandleFoliageKeyDown(AEvent: TJSKeyboardEvent): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

implementation

const
  CELL_SIZE = 20;
  MAX_SEED = Cardinal($FFFFFFFF);

constructor TBrowserWorldApplication.Create;
begin
  inherited Create;
  FWorld := nil;
end;

destructor TBrowserWorldApplication.Destroy;
begin
  FWorld.Free;
  inherited Destroy;
end;

function TBrowserWorldApplication.RequireElement(
  const AId: String): TJSElement;
begin
  Result := document.getElementById(AId);
  if not Assigned(Result) then
    raise EWorld2D.Create('browser demo is missing #' + AId);
end;

procedure TBrowserWorldApplication.BindDocument;
var
  LLayer: TWorld2DLayer;
begin
  FSeedInput := TJSHTMLInputElement(RequireElement('seed-input'));
  FGenerateButton := TJSHTMLButtonElement(
    RequireElement('generate-button'));
  FNextSeedButton := TJSHTMLButtonElement(
    RequireElement('next-seed-button'));
  FWrapInput := TJSHTMLInputElement(RequireElement('wrap-input'));
  FResetButton := TJSHTMLButtonElement(
    RequireElement('reset-showcase-button'));
  FClearLocksButton := TJSHTMLButtonElement(
    RequireElement('clear-locks-button'));
  FCanvases[w2lTerrain] := TJSHTMLCanvasElement(
    RequireElement('terrain-canvas'));
  FCanvases[w2lBiome] := TJSHTMLCanvasElement(
    RequireElement('biome-canvas'));
  FCanvases[w2lFoliage] := TJSHTMLCanvasElement(
    RequireElement('foliage-canvas'));
  FSelectedXInput := TJSHTMLInputElement(RequireElement('selected-x'));
  FSelectedYInput := TJSHTMLInputElement(RequireElement('selected-y'));
  FLockLayerSelect := TJSHTMLSelectElement(
    RequireElement('lock-layer'));
  FLockValueSelect := TJSHTMLSelectElement(
    RequireElement('lock-value'));
  FLockButton := TJSHTMLButtonElement(RequireElement('lock-button'));
  FUnlockButton := TJSHTMLButtonElement(RequireElement('unlock-button'));
  FStatusElement := RequireElement('status');
  FSignatureElement := RequireElement('signature');
  FValidationElement := RequireElement('validation');
  FSolverReportElement := RequireElement('solver-report');

  for LLayer := Low(TWorld2DLayer) to High(TWorld2DLayer) do
  begin
    FCanvases[LLayer].width := WFC_WORLD2D_SHOWCASE_WIDTH * CELL_SIZE;
    FCanvases[LLayer].height := WFC_WORLD2D_SHOWCASE_HEIGHT * CELL_SIZE;
    FContexts[LLayer] := FCanvases[LLayer].getContextAs2DContext('2d');
    if not Assigned(FContexts[LLayer]) then
      raise EWorld2D.Create('2D canvas context is unavailable for '
        + World2DLayerName(LLayer));
  end;
end;

procedure TBrowserWorldApplication.BindEvents;
begin
  FGenerateButton.onclick := @HandleGenerate;
  FNextSeedButton.onclick := @HandleNextSeed;
  FResetButton.onclick := @HandleReset;
  FClearLocksButton.onclick := @HandleClearLocks;
  FLockButton.onclick := @HandleLock;
  FUnlockButton.onclick := @HandleUnlock;
  FWrapInput.onchange := @HandleWrapChange;
  FSelectedXInput.onchange := @HandleCoordinateChange;
  FSelectedYInput.onchange := @HandleCoordinateChange;
  FLockLayerSelect.onchange := @HandleLayerChange;
  FCanvases[w2lTerrain].onclick := @HandleTerrainClick;
  FCanvases[w2lBiome].onclick := @HandleBiomeClick;
  FCanvases[w2lFoliage].onclick := @HandleFoliageClick;
  FCanvases[w2lTerrain].onkeydown := @HandleTerrainKeyDown;
  FCanvases[w2lBiome].onkeydown := @HandleBiomeKeyDown;
  FCanvases[w2lFoliage].onkeydown := @HandleFoliageKeyDown;
end;

procedure TBrowserWorldApplication.SetBodyState(
  const AState, ASignature: String);
begin
  document.body.setAttribute('data-state', AState);
  document.body.setAttribute('data-signature', ASignature);
end;

procedure TBrowserWorldApplication.ShowError(const AMessage: String);
begin
  SetBodyState('error', '');
  if Assigned(FStatusElement) then
    FStatusElement.textContent := 'Error: ' + AMessage;
  if Assigned(FSignatureElement) then
    FSignatureElement.textContent := '';
  if Assigned(FValidationElement) then
    FValidationElement.textContent :=
      'Independent validation was not run for this error.';
  if Assigned(FSolverReportElement) then
    FSolverReportElement.textContent := 'No solver report for this error.';
end;

function TBrowserWorldApplication.TryParseSeed(const AText: String;
  out ASeed: TGraphSeed): Boolean;
var
  LBase: Cardinal;
  LCharacter: Char;
  LDigit: Cardinal;
  LIndex: Integer;
  LStart: Integer;
  LText: String;
  LValue: Cardinal;
begin
  Result := False;
  ASeed := 0;
  LText := Trim(AText);
  if LText = '' then
    Exit;

  LBase := 10;
  LStart := 1;
  if LText[1] = '$' then
  begin
    LBase := 16;
    LStart := 2;
  end
  else if (Length(LText) >= 2) and (LText[1] = '0')
    and ((LText[2] = 'x') or (LText[2] = 'X')) then
  begin
    LBase := 16;
    LStart := 3;
  end;
  if LStart > Length(LText) then
    Exit;

  LValue := 0;
  for LIndex := LStart to Length(LText) do
  begin
    LCharacter := LText[LIndex];
    if (LCharacter >= '0') and (LCharacter <= '9') then
      LDigit := Ord(LCharacter) - Ord('0')
    else if (LCharacter >= 'a') and (LCharacter <= 'f') then
      LDigit := Ord(LCharacter) - Ord('a') + 10
    else if (LCharacter >= 'A') and (LCharacter <= 'F') then
      LDigit := Ord(LCharacter) - Ord('A') + 10
    else
      Exit;
    if LDigit >= LBase then
      Exit;
    if LValue > (MAX_SEED - LDigit) div LBase then
      Exit;
    LValue := (LValue * LBase) + LDigit;
  end;
  ASeed := LValue;
  Result := True;
end;

function TBrowserWorldApplication.ReadSeed: TGraphSeed;
begin
  if not TryParseSeed(FSeedInput.value, Result) then
    raise EConvertError.Create(
      'seed must be a 32-bit unsigned decimal or hexadecimal value');
end;

function TBrowserWorldApplication.SelectedLayer: TWorld2DLayer;
var
  LLayer: Integer;
begin
  if FLockLayerSelect.value = World2DLayerName(w2lTerrain) then
    Exit(w2lTerrain);
  if FLockLayerSelect.value = World2DLayerName(w2lBiome) then
    Exit(w2lBiome);
  if FLockLayerSelect.value = World2DLayerName(w2lFoliage) then
    Exit(w2lFoliage);
  if not TryStrToInt(FLockLayerSelect.value, LLayer)
    or (LLayer < Ord(Low(TWorld2DLayer)))
    or (LLayer > Ord(High(TWorld2DLayer))) then
    raise EWorld2D.Create('select a valid world layer');
  Result := TWorld2DLayer(LLayer);
end;

procedure TBrowserWorldApplication.ReadSelectedCoordinates;
var
  LX: Integer;
  LY: Integer;
begin
  if not TryStrToInt(FSelectedXInput.value, LX) then
    LX := 0;
  if not TryStrToInt(FSelectedYInput.value, LY) then
    LY := 0;
  if LX < 0 then
    LX := 0
  else if LX >= WFC_WORLD2D_SHOWCASE_WIDTH then
    LX := WFC_WORLD2D_SHOWCASE_WIDTH - 1;
  if LY < 0 then
    LY := 0
  else if LY >= WFC_WORLD2D_SHOWCASE_HEIGHT then
    LY := WFC_WORLD2D_SHOWCASE_HEIGHT - 1;
  FSelectedX := TGraphCoordinate(LX);
  FSelectedY := TGraphCoordinate(LY);
  FSelectedXInput.value := IntToStr(LX);
  FSelectedYInput.value := IntToStr(LY);
end;

procedure TBrowserWorldApplication.RefreshLockValues;
var
  LCurrentValue: TGraphValue;
  LLayer: TWorld2DLayer;

  procedure AddValue(const AValue: TGraphValue);
  begin
    FLockValueSelect.add(TJSHTMLOptionElement.new(AValue, AValue));
  end;

begin
  LLayer := SelectedLayer;
  LCurrentValue := '';
  if Assigned(FWorld) then
    LCurrentValue := FWorld.Value[LLayer, FSelectedX, FSelectedY];
  FLockValueSelect.innerHTML := '';
  case LLayer of
    w2lTerrain:
      begin
        AddValue(WFC_WORLD2D_TERRAIN_WATER);
        AddValue(WFC_WORLD2D_TERRAIN_LAND);
        AddValue(WFC_WORLD2D_TERRAIN_MOUNTAIN);
      end;
    w2lBiome:
      begin
        AddValue(WFC_WORLD2D_BIOME_OCEAN);
        AddValue(WFC_WORLD2D_BIOME_SHORE);
        AddValue(WFC_WORLD2D_BIOME_PLAINS);
        AddValue(WFC_WORLD2D_BIOME_WOODLAND);
        AddValue(WFC_WORLD2D_BIOME_ALPINE);
      end;
    w2lFoliage:
      begin
        AddValue(WFC_WORLD2D_FOLIAGE_NONE);
        AddValue(WFC_WORLD2D_FOLIAGE_REEDS);
        AddValue(WFC_WORLD2D_FOLIAGE_GRASS);
        AddValue(WFC_WORLD2D_FOLIAGE_TREE);
        AddValue(WFC_WORLD2D_FOLIAGE_PINE);
      end;
  end;
  if IsWorld2DLayerValue(LLayer, LCurrentValue) then
    FLockValueSelect.value := LCurrentValue;
end;

procedure TBrowserWorldApplication.RefreshInspector;
begin
  FSelectedXInput.value := UIntToStr(Cardinal(FSelectedX));
  FSelectedYInput.value := UIntToStr(Cardinal(FSelectedY));
  RefreshLockValues;
end;

procedure TBrowserWorldApplication.RecreateShowcase;
var
  LConfig: TWorld2DConfig;
  LNewWorld: TWorld2D;
begin
  LConfig := DefaultWorld2DConfig;
  LConfig.Seed := ReadSeed;
  LConfig.WrapNeighbors := FWrapInput.checked;
  LNewWorld := TWorld2D.Create(WFC_WORLD2D_SHOWCASE_WIDTH,
    WFC_WORLD2D_SHOWCASE_HEIGHT, LConfig);
  try
    ApplyWorld2DShowcaseAnchors(LNewWorld);
  except
    LNewWorld.Free;
    raise;
  end;
  FWorld.Free;
  FWorld := LNewWorld;
  GenerateWorld;
end;

procedure TBrowserWorldApplication.ClearAllLocks;
var
  LLayer: TWorld2DLayer;
  X, Y: Integer;
begin
  for LLayer := Low(TWorld2DLayer) to High(TWorld2DLayer) do
    for Y := 0 to WFC_WORLD2D_SHOWCASE_HEIGHT - 1 do
      for X := 0 to WFC_WORLD2D_SHOWCASE_WIDTH - 1 do
        FWorld.ClearLock(LLayer, X, Y);
end;

function TBrowserWorldApplication.SolverReportText(
  const AReport: TGraphSolveReport): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AReport.Passes) do
  begin
    if Result <> '' then
      Result := Result + ' | ';
    Result := Result + 'pass ' + IntToStr(I)
      + ': decisions ' + IntToStr(AReport.Passes[I].Decisions)
      + ', propagations ' + IntToStr(AReport.Passes[I].Propagations)
      + ', contradictions ' + IntToStr(AReport.Passes[I].Contradictions)
      + ', backtracks ' + IntToStr(AReport.Passes[I].Backtracks);
  end;
end;

procedure TBrowserWorldApplication.GenerateWorld;
var
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LSignature: String;
  LValidation: TWorld2DValidationReport;
begin
  if not Assigned(FWorld) then
    raise EWorld2D.Create('the browser world has not been created');
  FWorld.Seed := ReadSeed;
  FWorld.WrapNeighbors := FWrapInput.checked;
  LOptions := DefaultGraphSolveOptions;
  LOptions.MaxBacktracks := 8192;
  if not FWorld.TryGenerate(LOptions, LReport) then
  begin
    SetBodyState('contradiction', '');
    FStatusElement.textContent := 'Contradiction in pass '
      + IntToStr(LReport.FailedPassIndex) + ', entry '
      + IntToStr(LReport.Contradiction.EntryIndex) + ' (kind '
      + IntToStr(Ord(LReport.Contradiction.Kind)) + ').';
    FSignatureElement.textContent := '';
    FValidationElement.textContent :=
      'Independent validation was not run because no pipeline was committed.';
    FSolverReportElement.textContent := SolverReportText(LReport);
    RenderWorld;
    RefreshInspector;
    Exit;
  end;

  FSolverReportElement.textContent := SolverReportText(LReport);
  if not ValidateWorld2D(FWorld, LValidation) then
  begin
    SetBodyState('error', '');
    FStatusElement.textContent := 'Independent validation failed.';
    FSignatureElement.textContent := '';
    FValidationElement.textContent :=
      DescribeWorld2DValidationIssue(LValidation.Issue);
    RenderWorld;
    RefreshInspector;
    Exit;
  end;

  LSignature := FWorld.PipelineSignature;
  SetBodyState('solved', LSignature);
  FStatusElement.textContent := 'Generated seed ' + UIntToStr(FWorld.Seed)
    + ' with ' + IntToStr(LValidation.CheckedCells)
    + ' independently checked cells.';
  FSignatureElement.textContent := LSignature;
  FValidationElement.textContent := 'valid: '
    + IntToStr(LValidation.CheckedCells) + ' cells, '
    + IntToStr(LValidation.CheckedRelations) + ' relations';
  RenderWorld;
  RefreshInspector;
end;

function TBrowserWorldApplication.ColorFor(
  const ALayer: TWorld2DLayer; const AValue: TGraphValue): String;
begin
  case ALayer of
    w2lTerrain:
      if AValue = WFC_WORLD2D_TERRAIN_WATER then
        Result := '#2B6CB0'
      else if AValue = WFC_WORLD2D_TERRAIN_LAND then
        Result := '#68D391'
      else if AValue = WFC_WORLD2D_TERRAIN_MOUNTAIN then
        Result := '#718096'
      else
        Result := '#E2E8F0';
    w2lBiome:
      if AValue = WFC_WORLD2D_BIOME_OCEAN then
        Result := '#3182CE'
      else if AValue = WFC_WORLD2D_BIOME_SHORE then
        Result := '#ECC94B'
      else if AValue = WFC_WORLD2D_BIOME_PLAINS then
        Result := '#9AE6B4'
      else if AValue = WFC_WORLD2D_BIOME_WOODLAND then
        Result := '#2F855A'
      else if AValue = WFC_WORLD2D_BIOME_ALPINE then
        Result := '#CBD5E0'
      else
        Result := '#E2E8F0';
    w2lFoliage:
      if AValue = WFC_WORLD2D_FOLIAGE_NONE then
        Result := '#F7FAFC'
      else if AValue = WFC_WORLD2D_FOLIAGE_REEDS then
        Result := '#D69E2E'
      else if AValue = WFC_WORLD2D_FOLIAGE_GRASS then
        Result := '#48BB78'
      else if AValue = WFC_WORLD2D_FOLIAGE_TREE then
        Result := '#22543D'
      else if AValue = WFC_WORLD2D_FOLIAGE_PINE then
        Result := '#234E52'
      else
        Result := '#E2E8F0';
  else
    Result := '#E2E8F0';
  end;
end;

function TBrowserWorldApplication.TextColorFor(
  const ALayer: TWorld2DLayer; const AValue: TGraphValue): String;
begin
  if ((ALayer = w2lTerrain)
      and (AValue = WFC_WORLD2D_TERRAIN_WATER))
    or ((ALayer = w2lBiome)
      and (AValue = WFC_WORLD2D_BIOME_WOODLAND))
    or ((ALayer = w2lFoliage)
      and ((AValue = WFC_WORLD2D_FOLIAGE_TREE)
        or (AValue = WFC_WORLD2D_FOLIAGE_PINE))) then
    Result := '#FFFFFF'
  else
    Result := '#050B0E';
end;

function TBrowserWorldApplication.GlyphFor(
  const ALayer: TWorld2DLayer; const AValue: TGraphValue): String;
begin
  case ALayer of
    w2lTerrain:
      if AValue = WFC_WORLD2D_TERRAIN_WATER then
        Result := '~'
      else if AValue = WFC_WORLD2D_TERRAIN_LAND then
        Result := '.'
      else if AValue = WFC_WORLD2D_TERRAIN_MOUNTAIN then
        Result := '^'
      else
        Result := '?';
    w2lBiome:
      if AValue = WFC_WORLD2D_BIOME_OCEAN then
        Result := 'O'
      else if AValue = WFC_WORLD2D_BIOME_SHORE then
        Result := 's'
      else if AValue = WFC_WORLD2D_BIOME_PLAINS then
        Result := 'p'
      else if AValue = WFC_WORLD2D_BIOME_WOODLAND then
        Result := 'w'
      else if AValue = WFC_WORLD2D_BIOME_ALPINE then
        Result := 'a'
      else
        Result := '?';
    w2lFoliage:
      if AValue = WFC_WORLD2D_FOLIAGE_NONE then
        Result := '-'
      else if AValue = WFC_WORLD2D_FOLIAGE_REEDS then
        Result := 'r'
      else if AValue = WFC_WORLD2D_FOLIAGE_GRASS then
        Result := 'g'
      else if AValue = WFC_WORLD2D_FOLIAGE_TREE then
        Result := 'T'
      else if AValue = WFC_WORLD2D_FOLIAGE_PINE then
        Result := 'P'
      else
        Result := '?';
  else
    Result := '?';
  end;
end;

procedure TBrowserWorldApplication.RenderLayer(
  const ALayer: TWorld2DLayer);
var
  LContext: TJSCanvasRenderingContext2D;
  LEntry: TGraphEntry;
  LValue: TGraphValue;
  X, Y: Integer;
begin
  LContext := FContexts[ALayer];
  LContext.clearRect(0, 0, FCanvases[ALayer].width,
    FCanvases[ALayer].height);
  LContext.font := 'bold 13px monospace';
  LContext.textAlign := 'center';
  LContext.textBaseline := 'middle';
  for Y := 0 to WFC_WORLD2D_SHOWCASE_HEIGHT - 1 do
    for X := 0 to WFC_WORLD2D_SHOWCASE_WIDTH - 1 do
    begin
      LEntry := FWorld.LayerGraph[ALayer].Entry[X, Y, 0];
      LValue := LEntry.Value;
      LContext.fillStyleAsColor := ColorFor(ALayer, LValue);
      LContext.fillRect(X * CELL_SIZE, Y * CELL_SIZE,
        CELL_SIZE, CELL_SIZE);
      LContext.strokeStyleAsColor := 'rgba(255,255,255,0.28)';
      LContext.lineWidth := 1;
      LContext.strokeRect(X * CELL_SIZE + 0.5,
        Y * CELL_SIZE + 0.5, CELL_SIZE - 1, CELL_SIZE - 1);
      LContext.fillStyleAsColor := TextColorFor(ALayer, LValue);
      LContext.fillText(GlyphFor(ALayer, LValue),
        X * CELL_SIZE + (CELL_SIZE div 2),
        Y * CELL_SIZE + (CELL_SIZE div 2));
      if (not LEntry.Empty) and (not LEntry.Generated) then
      begin
        LContext.strokeStyleAsColor := '#F97316';
        LContext.lineWidth := 3;
        LContext.strokeRect(X * CELL_SIZE + 2,
          Y * CELL_SIZE + 2, CELL_SIZE - 4, CELL_SIZE - 4);
      end;
    end;

  LContext.strokeStyleAsColor := '#FFFFFF';
  LContext.lineWidth := 4;
  LContext.strokeRect(FSelectedX * CELL_SIZE + 2,
    FSelectedY * CELL_SIZE + 2, CELL_SIZE - 4, CELL_SIZE - 4);
  LContext.strokeStyleAsColor := '#111827';
  LContext.lineWidth := 2;
  LContext.strokeRect(FSelectedX * CELL_SIZE + 2,
    FSelectedY * CELL_SIZE + 2, CELL_SIZE - 4, CELL_SIZE - 4);
end;

procedure TBrowserWorldApplication.RenderWorld;
var
  LLayer: TWorld2DLayer;
begin
  if not Assigned(FWorld) then
    Exit;
  for LLayer := Low(TWorld2DLayer) to High(TWorld2DLayer) do
    RenderLayer(LLayer);
end;

procedure TBrowserWorldApplication.SelectCanvasCell(
  const ALayer: TWorld2DLayer; AEvent: TJSMouseEvent);
var
  LRect: TJSDOMRect;
  LX: Integer;
  LY: Integer;
begin
  LRect := FCanvases[ALayer].getBoundingClientRect;
  if (LRect.width <= 0) or (LRect.height <= 0) then
    Exit;
  LX := Trunc((AEvent.clientX - LRect.left)
    * FCanvases[ALayer].width / LRect.width) div CELL_SIZE;
  LY := Trunc((AEvent.clientY - LRect.top)
    * FCanvases[ALayer].height / LRect.height) div CELL_SIZE;
  if LX < 0 then
    LX := 0
  else if LX >= WFC_WORLD2D_SHOWCASE_WIDTH then
    LX := WFC_WORLD2D_SHOWCASE_WIDTH - 1;
  if LY < 0 then
    LY := 0
  else if LY >= WFC_WORLD2D_SHOWCASE_HEIGHT then
    LY := WFC_WORLD2D_SHOWCASE_HEIGHT - 1;
  FSelectedX := TGraphCoordinate(LX);
  FSelectedY := TGraphCoordinate(LY);
  FLockLayerSelect.value := World2DLayerName(ALayer);
  RefreshInspector;
  RenderWorld;
end;

function TBrowserWorldApplication.HandleCanvasKeyDown(
  const ALayer: TWorld2DLayer;
  AEvent: TJSKeyboardEvent): Boolean;
var
  LHandled: Boolean;
begin
  Result := True;
  LHandled := True;
  if AEvent.Key = 'ArrowLeft' then
  begin
    if FSelectedX > 0 then
      Dec(FSelectedX);
  end
  else if AEvent.Key = 'ArrowRight' then
  begin
    if FSelectedX < WFC_WORLD2D_SHOWCASE_WIDTH - 1 then
      Inc(FSelectedX);
  end
  else if AEvent.Key = 'ArrowUp' then
  begin
    if FSelectedY > 0 then
      Dec(FSelectedY);
  end
  else if AEvent.Key = 'ArrowDown' then
  begin
    if FSelectedY < WFC_WORLD2D_SHOWCASE_HEIGHT - 1 then
      Inc(FSelectedY);
  end
  else
    LHandled := False;

  if not LHandled then
    Exit;
  AEvent.preventDefault;
  FLockLayerSelect.value := World2DLayerName(ALayer);
  RefreshInspector;
  RenderWorld;
  Result := False;
end;

procedure TBrowserWorldApplication.RunSelfTest;

  procedure AssertTest(const ACondition: Boolean;
    const AMessage: String);
  begin
    if not ACondition then
      raise EWorld2D.Create(AMessage);
  end;

var
  LLayer: TWorld2DLayer;
begin
  try
    document.body.setAttribute('data-self-test', 'pending');
    FSeedInput.value := '0';
    FGenerateButton.click;
    AssertTest(document.body.getAttribute('data-state') = 'solved',
      'seed zero did not solve');
    AssertTest(document.body.getAttribute('data-signature')
      = WFC_WORLD2D_SHOWCASE_SEED_ZERO_SIGNATURE,
      'seed-zero signature changed');

    FSelectedXInput.value := '0';
    FSelectedYInput.value := '0';
    FSelectedXInput.onchange(nil);
    FLockLayerSelect.value := World2DLayerName(w2lFoliage);
    FLockLayerSelect.onchange(nil);
    FLockValueSelect.value := WFC_WORLD2D_FOLIAGE_TREE;
    FLockButton.click;
    AssertTest(document.body.getAttribute('data-state') = 'contradiction',
      'an incompatible foliage lock was accepted');

    FUnlockButton.click;
    AssertTest(document.body.getAttribute('data-state') = 'solved',
      'unlock did not restore a solved pipeline');
    AssertTest(document.body.getAttribute('data-signature')
      = WFC_WORLD2D_SHOWCASE_SEED_ZERO_SIGNATURE,
      'unlock did not restore the seed-zero signature');

    for LLayer := Low(TWorld2DLayer) to High(TWorld2DLayer) do
    begin
      AssertTest(Assigned(FContexts[LLayer]),
        World2DLayerName(LLayer) + ' canvas has no context');
      AssertTest((FCanvases[LLayer].width
          = WFC_WORLD2D_SHOWCASE_WIDTH * CELL_SIZE)
        and (FCanvases[LLayer].height
          = WFC_WORLD2D_SHOWCASE_HEIGHT * CELL_SIZE),
        World2DLayerName(LLayer) + ' canvas has the wrong size');
      AssertTest(FContexts[LLayer].getImageData(1, 1, 1, 1).data[3] <> 0,
        World2DLayerName(LLayer) + ' canvas did not render pixels');
    end;
    document.body.setAttribute('data-self-test', 'passed');
  except
    on E: Exception do
    begin
      ShowError('self-test failed: ' + E.Message);
      document.body.setAttribute('data-self-test', 'failed');
      document.body.setAttribute('data-self-test-message', E.Message);
    end;
  end;
end;

function TBrowserWorldApplication.HandleGenerate(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    GenerateWorld;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserWorldApplication.HandleNextSeed(
  AEvent: TJSMouseEvent): Boolean;
var
  LSeed: TGraphSeed;
begin
  Result := False;
  try
    LSeed := ReadSeed;
    if LSeed = MAX_SEED then
      LSeed := 0
    else
      Inc(LSeed);
    FSeedInput.value := UIntToStr(LSeed);
    GenerateWorld;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserWorldApplication.HandleReset(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    FSeedInput.value := UIntToStr(WFC_WORLD2D_SHOWCASE_DEFAULT_SEED);
    FWrapInput.checked := False;
    FSelectedX := 0;
    FSelectedY := 0;
    FSelectedXInput.value := '0';
    FSelectedYInput.value := '0';
    FLockLayerSelect.value := World2DLayerName(w2lTerrain);
    RecreateShowcase;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserWorldApplication.HandleClearLocks(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    ClearAllLocks;
    GenerateWorld;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserWorldApplication.HandleLock(
  AEvent: TJSMouseEvent): Boolean;
var
  LLayer: TWorld2DLayer;
begin
  Result := False;
  try
    ReadSelectedCoordinates;
    LLayer := SelectedLayer;
    FWorld.Lock(LLayer, FSelectedX, FSelectedY,
      FLockValueSelect.value);
    GenerateWorld;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserWorldApplication.HandleUnlock(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    ReadSelectedCoordinates;
    FWorld.ClearLock(SelectedLayer, FSelectedX, FSelectedY);
    GenerateWorld;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserWorldApplication.HandleWrapChange(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    GenerateWorld;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserWorldApplication.HandleCoordinateChange(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    ReadSelectedCoordinates;
    RefreshInspector;
    RenderWorld;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserWorldApplication.HandleLayerChange(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    RefreshLockValues;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserWorldApplication.HandleTerrainClick(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  SelectCanvasCell(w2lTerrain, AEvent);
end;

function TBrowserWorldApplication.HandleBiomeClick(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  SelectCanvasCell(w2lBiome, AEvent);
end;

function TBrowserWorldApplication.HandleFoliageClick(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  SelectCanvasCell(w2lFoliage, AEvent);
end;

function TBrowserWorldApplication.HandleTerrainKeyDown(
  AEvent: TJSKeyboardEvent): Boolean;
begin
  Result := HandleCanvasKeyDown(w2lTerrain, AEvent);
end;

function TBrowserWorldApplication.HandleBiomeKeyDown(
  AEvent: TJSKeyboardEvent): Boolean;
begin
  Result := HandleCanvasKeyDown(w2lBiome, AEvent);
end;

function TBrowserWorldApplication.HandleFoliageKeyDown(
  AEvent: TJSKeyboardEvent): Boolean;
begin
  Result := HandleCanvasKeyDown(w2lFoliage, AEvent);
end;

procedure TBrowserWorldApplication.Run;
begin
  try
    BindDocument;
    BindEvents;
    FSeedInput.value := UIntToStr(WFC_WORLD2D_SHOWCASE_DEFAULT_SEED);
    FWrapInput.checked := False;
    FSelectedX := 0;
    FSelectedY := 0;
    FSelectedXInput.value := '0';
    FSelectedYInput.value := '0';
    FLockLayerSelect.value := World2DLayerName(w2lTerrain);
    RecreateShowcase;
    if Pos('selftest=1', window.location.search) > 0 then
      RunSelfTest
    else
      document.body.setAttribute('data-self-test', 'not-requested');
  except
    on E: Exception do
      if Assigned(FStatusElement) then
        ShowError(E.Message)
      else
        window.console.error(E.Message);
  end;
end;

end.
