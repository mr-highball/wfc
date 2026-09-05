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
unit browser_neighborhood_counts_app;

{$mode delphi}{$H+}

interface

uses SysUtils, JS, Web, wfc, neighborhood_count_demo;

type
  TBrowserNeighborhoodCounts = class
  private
    FConfig: TCountDemoConfig;
    FResult: TCountDemoResult;
    FSeed, FMinimum, FMaximum, FWaterMaximum: TJSHTMLInputElement;
    FWrap: TJSHTMLInputElement;
    FMode, FPreset: TJSHTMLSelectElement;
    FGenerate, FRepair: TJSHTMLButtonElement;
    FStatus, FDetail, FInputTerrain, FInputRoads: TJSElement;
    FOutputTerrain, FOutputRoads, FOutputMarket: TJSElement;
    function Element(const AId: String): TJSElement;
    procedure Invalidate;
    procedure LoadPreset;
    procedure ReadControls;
    procedure DrawInputs;
    procedure DrawOutput(const AParent: TJSElement; const AText: String);
    procedure Generate(const ARepair: Boolean);
    procedure SelfTest;
    function OnPreset(AEvent: TJSEvent): Boolean;
    function OnInput(AEvent: TJSEvent): Boolean;
    function OnGenerate(AEvent: TJSMouseEvent): Boolean;
    function OnRepair(AEvent: TJSMouseEvent): Boolean;
    function OnTerrain(AEvent: TJSMouseEvent): Boolean;
    function OnRoad(AEvent: TJSMouseEvent): Boolean;
  public
    procedure Run;
  end;

implementation

function TBrowserNeighborhoodCounts.Element(const AId: String): TJSElement;
begin
  Result := document.getElementById(AId);
  if Result = nil then raise Exception.Create('missing element: ' + AId);
end;

function ParseSeed(const S: String): TGraphSeed;
var I, D: Integer;
begin
  if S = '' then raise Exception.Create('seed requires unsigned decimal digits');
  Result := 0;
  for I := 1 to Length(S) do
  begin
    if not (S[I] in ['0'..'9']) then
      raise Exception.Create('seed requires unsigned decimal digits');
    D := Ord(S[I]) - Ord('0');
    if Result > (High(Cardinal) - Cardinal(D)) div 10 then
      raise Exception.Create('seed exceeds the unsigned 32-bit range');
    Result := Result * 10 + Cardinal(D);
  end;
end;

procedure TBrowserNeighborhoodCounts.Invalidate;
begin
  FResult := Default(TCountDemoResult);
  FOutputTerrain.textContent := '';
  FOutputRoads.textContent := '';
  FOutputMarket.textContent := '';
  FStatus.textContent := 'Inputs changed. Generate or repair when ready.';
  FDetail.textContent := 'No generated output is current. Input cells remain caller-owned.';
  document.body.setAttribute('data-state', 'dirty');
  document.body.setAttribute('data-output-key', '');
  document.body.setAttribute('data-road-count', '');
  document.body.setAttribute('data-water-count', '');
  document.body.setAttribute('data-terrain-reused', 'false');
end;

procedure TBrowserNeighborhoodCounts.DrawInputs;
var I: Integer; B: TJSHTMLButtonElement; S: String;
begin
  FInputTerrain.textContent := ''; FInputRoads.textContent := '';
  FInputTerrain.setAttribute('style', '--columns:' + IntToStr(FConfig.Width));
  FInputRoads.setAttribute('style', '--columns:' + IntToStr(FConfig.Width));
  for I := 0 to Length(FConfig.Water) - 1 do
  begin
    B := TJSHTMLButtonElement(document.createElement('button'));
    B.setAttribute('type', 'button');
    B.setAttribute('data-cell', IntToStr(I));
    if FConfig.Water[I] = 0 then begin S := 'land'; B.textContent := 'L'; end
    else begin S := 'water'; B.textContent := 'W'; end;
    B.className := 'cell ' + S;
    B.setAttribute('aria-label', 'Terrain cell ' + IntToStr(I) + ': ' + S + '; click to toggle');
    B.title := 'cell ' + IntToStr(I) + ': ' + S;
    if I = CountDemoTarget(FConfig) then B.className := B.className + ' target';
    B.onclick := @OnTerrain;
    FInputTerrain.appendChild(B);

    B := TJSHTMLButtonElement(document.createElement('button'));
    B.setAttribute('type', 'button');
    B.setAttribute('data-cell', IntToStr(I));
    case FConfig.RoadLocks[I] of
      -1: begin S := 'auto'; B.textContent := '?'; end;
      0: begin S := 'empty'; B.textContent := '.'; end;
      1: begin S := 'road'; B.textContent := 'R'; end;
    end;
    B.className := 'cell ' + S;
    B.setAttribute('aria-label', 'Road cell ' + IntToStr(I) + ': ' + S + '; click to cycle');
    B.title := 'cell ' + IntToStr(I) + ': ' + S;
    if I = CountDemoTarget(FConfig) then B.className := B.className + ' target';
    B.onclick := @OnRoad;
    FInputRoads.appendChild(B);
  end;
  Element('grid-note').textContent := IntToStr(FConfig.Width) + ' × ' +
    IntToStr(FConfig.Height) + '. The outlined cell is the locked market probe.';
end;

procedure TBrowserNeighborhoodCounts.LoadPreset;
begin
  Invalidate;
  FConfig := CountDemoPreset(FPreset.value);
  FSeed.value := IntToStr(FConfig.Seed);
  FMinimum.value := IntToStr(FConfig.MinimumRoads);
  FMaximum.value := IntToStr(FConfig.MaximumRoads);
  FWaterMaximum.value := IntToStr(FConfig.MaximumWater);
  FWrap.checked := FConfig.Wrap;
  if FConfig.Mode = gpcmMatchingTerms then FMode.value := 'terms'
  else FMode.value := 'cells';
  DrawInputs;
end;

procedure TBrowserNeighborhoodCounts.ReadControls;
begin
  FConfig.Seed := ParseSeed(Trim(FSeed.value));
  if not TryStrToInt(FMinimum.value, FConfig.MinimumRoads) or
      not TryStrToInt(FMaximum.value, FConfig.MaximumRoads) or
      not TryStrToInt(FWaterMaximum.value, FConfig.MaximumWater) then
    raise Exception.Create('count bounds require whole numbers');
  if FMode.value = 'terms' then FConfig.Mode := gpcmMatchingTerms
  else if FMode.value = 'cells' then FConfig.Mode := gpcmDistinctCells
  else raise Exception.Create('choose a supported count mode');
  FConfig.Wrap := FWrap.checked;
end;

procedure TBrowserNeighborhoodCounts.DrawOutput(const AParent: TJSElement;
  const AText: String);
var I: Integer; E: TJSElement; S: String;
begin
  AParent.textContent := '';
  AParent.setAttribute('style', '--columns:' + IntToStr(FConfig.Width));
  for I := 1 to Length(AText) do
  begin
    E := document.createElement('span');
    case AText[I] of
      'L': S := 'land';
      'W': S := 'water';
      'R': S := 'road';
      'M': S := 'market';
      else S := 'empty';
    end;
    E.className := 'cell ' + S;
    E.textContent := AText[I];
    E.setAttribute('aria-label', 'Cell ' + IntToStr(I - 1) + ': ' + S);
    AParent.appendChild(E);
  end;
end;

procedure TBrowserNeighborhoodCounts.Generate(const ARepair: Boolean);
begin
  Invalidate;
  try
    ReadControls;
    FResult := SolveCountDemo(FConfig, ARepair);
    document.body.setAttribute('data-state', FResult.Status);
    FDetail.textContent := FResult.Detail;
    if not FResult.Solved then
    begin
      FStatus.textContent := 'No solution committed: ' + FResult.Status;
      Exit;
    end;
    DrawOutput(FOutputTerrain, FResult.Terrain);
    DrawOutput(FOutputRoads, FResult.Roads);
    DrawOutput(FOutputMarket, FResult.Market);
    FStatus.textContent := 'Solved and independently checked.';
    document.body.setAttribute('data-output-key', FResult.OutputKey);
    document.body.setAttribute('data-road-count', IntToStr(FResult.RoadCount));
    document.body.setAttribute('data-water-count', IntToStr(FResult.WaterCount));
    if FResult.TerrainReused then
      document.body.setAttribute('data-terrain-reused', 'true');
    if ARepair then FDetail.textContent := FResult.Detail +
      ' Terrain reused; only roads and market were in the repair horizon.';
  except on E: Exception do
    begin
      Invalidate;
      document.body.setAttribute('data-state', 'error');
      FStatus.textContent := 'Invalid request: ' + E.Message;
    end;
  end;
end;

function TBrowserNeighborhoodCounts.OnPreset(AEvent: TJSEvent): Boolean;
begin Result := False; LoadPreset; end;
function TBrowserNeighborhoodCounts.OnInput(AEvent: TJSEvent): Boolean;
begin Result := True; Invalidate; end;
function TBrowserNeighborhoodCounts.OnGenerate(AEvent: TJSMouseEvent): Boolean;
begin Result := False; Generate(False); end;
function TBrowserNeighborhoodCounts.OnRepair(AEvent: TJSMouseEvent): Boolean;
begin Result := False; Generate(True); end;

function TBrowserNeighborhoodCounts.OnTerrain(AEvent: TJSMouseEvent): Boolean;
var I: Integer;
begin
  Result := False; Invalidate;
  I := StrToInt(TJSElement(AEvent.currentTarget).getAttribute('data-cell'));
  FConfig.Water[I] := 1 - FConfig.Water[I];
  DrawInputs;
end;

function TBrowserNeighborhoodCounts.OnRoad(AEvent: TJSMouseEvent): Boolean;
var I: Integer;
begin
  Result := False; Invalidate;
  I := StrToInt(TJSElement(AEvent.currentTarget).getAttribute('data-cell'));
  FConfig.RoadLocks[I] := FConfig.RoadLocks[I] + 1;
  if FConfig.RoadLocks[I] > 1 then FConfig.RoadLocks[I] := -1;
  DrawInputs;
end;

procedure TBrowserNeighborhoodCounts.SelfTest;
  procedure Require(const B: Boolean; const M: String);
  begin if not B then raise Exception.Create(M); end;
  procedure Preset(const S: String);
  begin FPreset.value := S; FPreset.dispatchEvent(TJSEvent.new('change')); end;
  procedure Marker(const S: String);
  begin document.body.setAttribute('data-' + S, 'passed'); end;
  procedure ClickGenerate;
  begin FGenerate.click; end;
begin
  document.body.setAttribute('data-self-test', 'running');
  try
    Preset('two'); ClickGenerate;
    Require(FResult.OutputKey = 'LLLLLLLLL/.R.R...../....M....', 'baseline output');
    Preset('lower'); ClickGenerate;
    Require((FResult.Status = 'contradiction') and
      (FOutputRoads.childElementCount = 0), 'lower bound');
    Marker('lower');
    Preset('upper'); ClickGenerate;
    Require((FResult.Status = 'contradiction') and (FResult.Roads = ''), 'upper bound');
    Marker('upper');
    Preset('flood'); ClickGenerate;
    Require(FResult.Status = 'contradiction', 'conjunctive flood bound'); Marker('flood');
    Preset('alias'); ClickGenerate;
    Require(FResult.Solved and (FResult.RoadCount = 2), 'matching aliases');
    FMode.value := 'cells'; FMode.dispatchEvent(TJSEvent.new('change'));
    Require((FOutputRoads.childElementCount = 0) and
      (document.body.getAttribute('data-output-key') = ''), 'pending edit invalidation');
    Marker('invalidation'); ClickGenerate;
    Require(FResult.Status = 'contradiction', 'distinct alias rejection');
    FMinimum.value := '1'; FMaximum.value := '1'; ClickGenerate;
    Require(FResult.Solved and (FResult.RoadCount = 1), 'distinct alias recovery');
    Marker('alias');
    Preset('repair'); ClickGenerate;
    Require(FResult.Status = 'contradiction', 'one-way repair fixture');
    FRepair.click;
    Require(FResult.Solved and FResult.TerrainReused and
      (FResult.PassBacktracks = 1), 'scoped provider repair');
    Marker('repair');
    FSeed.value := '-1'; FSeed.dispatchEvent(TJSEvent.new('input')); ClickGenerate;
    Require((document.body.getAttribute('data-state') = 'error') and
      (FOutputMarket.childElementCount = 0), 'invalid seed clears output');
    Preset('two'); ClickGenerate;
    Require(FResult.Solved, 'final recovery');
    document.body.setAttribute('data-self-test', 'passed');
  except on E: Exception do
    begin
      document.body.setAttribute('data-self-test', 'failed');
      document.body.setAttribute('data-self-test-message', E.Message);
      FStatus.textContent := 'Self-test failed: ' + E.Message;
    end;
  end;
end;

procedure TBrowserNeighborhoodCounts.Run;
begin
  FSeed := TJSHTMLInputElement(Element('seed'));
  FMinimum := TJSHTMLInputElement(Element('minimum'));
  FMaximum := TJSHTMLInputElement(Element('maximum'));
  FWaterMaximum := TJSHTMLInputElement(Element('water-maximum'));
  FWrap := TJSHTMLInputElement(Element('wrap'));
  FMode := TJSHTMLSelectElement(Element('mode'));
  FPreset := TJSHTMLSelectElement(Element('preset'));
  FGenerate := TJSHTMLButtonElement(Element('generate'));
  FRepair := TJSHTMLButtonElement(Element('repair'));
  FStatus := Element('status'); FDetail := Element('detail');
  FInputTerrain := Element('input-terrain'); FInputRoads := Element('input-roads');
  FOutputTerrain := Element('output-terrain');
  FOutputRoads := Element('output-roads'); FOutputMarket := Element('output-market');
  FPreset.onchange := @OnPreset;
  FSeed.oninput := @OnInput; FMinimum.oninput := @OnInput;
  FMaximum.oninput := @OnInput; FWaterMaximum.oninput := @OnInput;
  FSeed.onchange := @OnInput; FMinimum.onchange := @OnInput;
  FMaximum.onchange := @OnInput; FWaterMaximum.onchange := @OnInput;
  FWrap.onchange := @OnInput; FMode.onchange := @OnInput;
  FGenerate.onclick := @OnGenerate; FRepair.onclick := @OnRepair;
  LoadPreset; Generate(False);
  if Pos('selftest=1', window.location.search) > 0 then SelfTest;
end;

end.
