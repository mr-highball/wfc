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
unit browser_building_app;

{$mode delphi}{$H+}

interface

uses
  JS,
  Web,
  SysUtils,
  wfc,
  wfc_voxel3d,
  wfc_voxel3d_isometric,
  wfc_building3d,
  wfc_building3d_validate,
  wfc_building3d_view,
  building3d_showcase;

type
  TBrowserBuildingApplication = class
  strict private
    FBuilding: TBuilding3D;
    FView: TBuilding3DView;
    FProjection: TVoxel3DProjectedScene;
    FYaw: TVoxel3DViewYaw;
    FViewMode: TBuilding3DViewMode;
    FSelectedX: Integer;
    FSelectedY: Integer;
    FSelectedZ: Integer;
    FSelectedQuad: Integer;
    FDrawOffsetX: Integer;
    FDrawOffsetY: Integer;

    FCanvas: TJSHTMLCanvasElement;
    FContext: TJSCanvasRenderingContext2D;
    FSeedInput: TJSHTMLInputElement;
    FGenerateButton: TJSHTMLButtonElement;
    FNextSeedButton: TJSHTMLButtonElement;
    FRegenerateStructureButton: TJSHTMLButtonElement;
    FRegenerateEnvelopeButton: TJSHTMLButtonElement;
    FRegeneratePropsButton: TJSHTMLButtonElement;
    FViewModeSelect: TJSHTMLSelectElement;
    FYawLeftButton: TJSHTMLButtonElement;
    FYawRightButton: TJSHTMLButtonElement;
    FYawOutput: TJSElement;
    FZClipInput: TJSHTMLInputElement;
    FZClipOutput: TJSElement;
    FStatusElement: TJSElement;
    FSignatureElement: TJSElement;
    FViewSignatureElement: TJSElement;
    FFaceCountElement: TJSElement;
    FSeedOutputElement: TJSElement;
    FValidationElement: TJSElement;
    FSolverReportElement: TJSElement;
    FSelectedCoordinateElement: TJSElement;
    FSelectedFootprintElement: TJSElement;
    FSelectedStructureElement: TJSElement;
    FSelectedEnvelopeElement: TJSElement;
    FSelectedPropElement: TJSElement;
    FSelectedFaceElement: TJSElement;

    function RequireElement(const AId: String): TJSElement;
    procedure BindDocument;
    procedure BindEvents;
    procedure SetBodyState(const AState: String);
    procedure ShowError(const AMessage: String);
    function ReadSeed: TGraphSeed;
    function ReadZClip: Integer;
    function ReadViewMode: TBuilding3DViewMode;
    function SolverReportText(const AReport: TGraphSolveReport): String;

    procedure GenerateBuilding;
    procedure RegenerateFrom(const AStage: TBuilding3DStage);
    procedure RefreshPresentation;
    procedure RebuildProjection;
    procedure RenderProjection;
    procedure RefreshInspector;
    procedure Rotate(const ADelta: Integer);
    procedure SelectFace(AEvent: TJSMouseEvent);
    procedure RunSelfTest;

    function HandleGenerate(AEvent: TJSMouseEvent): Boolean;
    function HandleNextSeed(AEvent: TJSMouseEvent): Boolean;
    function HandleRegenerateStructure(AEvent: TJSMouseEvent): Boolean;
    function HandleRegenerateEnvelope(AEvent: TJSMouseEvent): Boolean;
    function HandleRegenerateProps(AEvent: TJSMouseEvent): Boolean;
    function HandleViewMode(AEvent: TJSEvent): Boolean;
    function HandleZClip(AEvent: TJSEvent): Boolean;
    function HandleYawLeft(AEvent: TJSMouseEvent): Boolean;
    function HandleYawRight(AEvent: TJSMouseEvent): Boolean;
    function HandleCanvasClick(AEvent: TJSMouseEvent): Boolean;
    function HandleCanvasKeyDown(AEvent: TJSKeyboardEvent): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

implementation

const
  MAX_SEED = Cardinal($FFFFFFFF);

function HexByte(const AValue: Byte): String;
const
  D = '0123456789ABCDEF';
begin
  SetLength(Result, 2);
  Result[1] := D[(AValue shr 4) + 1];
  Result[2] := D[(AValue and $0F) + 1];
end;

function CanvasColor(const AColor: TVoxel3DColor): String;
begin
  Result := '#' + HexByte(AColor.R) + HexByte(AColor.G) +
    HexByte(AColor.B) + HexByte(AColor.A);
end;

constructor TBrowserBuildingApplication.Create;
begin
  inherited Create;
  FBuilding := nil;
  FView := nil;
  FProjection := nil;
  FYaw := v3vy0;
  FViewMode := b3vmComplete;
  FSelectedX := -1;
  FSelectedY := -1;
  FSelectedZ := -1;
  FSelectedQuad := -1;
end;

destructor TBrowserBuildingApplication.Destroy;
begin
  FProjection.Free;
  FView.Free;
  FBuilding.Free;
  inherited Destroy;
end;

function TBrowserBuildingApplication.RequireElement(
  const AId: String): TJSElement;
begin
  Result := document.getElementById(AId);
  if not Assigned(Result) then
    raise EBuilding3DView.Create('browser demo is missing #' + AId);
end;

procedure TBrowserBuildingApplication.BindDocument;
begin
  FCanvas := TJSHTMLCanvasElement(RequireElement('building-canvas'));
  FContext := FCanvas.getContextAs2DContext('2d');
  if not Assigned(FContext) then
    raise EBuilding3DView.Create('2D canvas context is unavailable');
  FSeedInput := TJSHTMLInputElement(RequireElement('seed-input'));
  FGenerateButton := TJSHTMLButtonElement(RequireElement('generate-button'));
  FNextSeedButton := TJSHTMLButtonElement(RequireElement('next-seed-button'));
  FRegenerateStructureButton := TJSHTMLButtonElement(
    RequireElement('regenerate-structure-button'));
  FRegenerateEnvelopeButton := TJSHTMLButtonElement(
    RequireElement('regenerate-envelope-button'));
  FRegeneratePropsButton := TJSHTMLButtonElement(
    RequireElement('regenerate-props-button'));
  FViewModeSelect := TJSHTMLSelectElement(RequireElement('view-mode'));
  FYawLeftButton := TJSHTMLButtonElement(RequireElement('yaw-left-button'));
  FYawRightButton := TJSHTMLButtonElement(RequireElement('yaw-right-button'));
  FYawOutput := RequireElement('yaw-output');
  FZClipInput := TJSHTMLInputElement(RequireElement('z-clip'));
  FZClipOutput := RequireElement('z-clip-output');
  FStatusElement := RequireElement('status');
  FSignatureElement := RequireElement('signature');
  FViewSignatureElement := RequireElement('view-signature');
  FFaceCountElement := RequireElement('face-count');
  FSeedOutputElement := RequireElement('seed-output');
  FValidationElement := RequireElement('validation');
  FSolverReportElement := RequireElement('solver-report');
  FSelectedCoordinateElement := RequireElement('selected-coordinate');
  FSelectedFootprintElement := RequireElement('selected-footprint');
  FSelectedStructureElement := RequireElement('selected-structure');
  FSelectedEnvelopeElement := RequireElement('selected-envelope');
  FSelectedPropElement := RequireElement('selected-prop');
  FSelectedFaceElement := RequireElement('selected-face');
end;

procedure TBrowserBuildingApplication.BindEvents;
begin
  FGenerateButton.onclick := @HandleGenerate;
  FNextSeedButton.onclick := @HandleNextSeed;
  FRegenerateStructureButton.onclick := @HandleRegenerateStructure;
  FRegenerateEnvelopeButton.onclick := @HandleRegenerateEnvelope;
  FRegeneratePropsButton.onclick := @HandleRegenerateProps;
  FViewModeSelect.onchange := @HandleViewMode;
  FZClipInput.oninput := @HandleZClip;
  FYawLeftButton.onclick := @HandleYawLeft;
  FYawRightButton.onclick := @HandleYawRight;
  FCanvas.onclick := @HandleCanvasClick;
  FCanvas.onkeydown := @HandleCanvasKeyDown;
end;

procedure TBrowserBuildingApplication.SetBodyState(const AState: String);
begin
  document.body.setAttribute('data-state', AState);
  if Assigned(FBuilding) and FBuilding.HasSolution then
    document.body.setAttribute('data-signature',
      FBuilding.PipelineSignature)
  else
    document.body.setAttribute('data-signature', '');
end;

procedure TBrowserBuildingApplication.ShowError(const AMessage: String);
begin
  SetBodyState('error');
  if Assigned(FStatusElement) then
    FStatusElement.textContent := 'Error: ' + AMessage;
  if Assigned(FValidationElement) then
    FValidationElement.textContent := 'Independent validation did not pass.';
end;

function TBrowserBuildingApplication.ReadSeed: TGraphSeed;
var
  D, I: Integer;
  Parsed: TGraphSeed;
  Text: String;
begin
  Text := FSeedInput.value;
  if Text = '' then
    raise EConvertError.Create('seed cannot be empty');
  Parsed := 0;
  for I := 1 to Length(Text) do
  begin
    if not (Text[I] in ['0'..'9']) then
      raise EConvertError.Create('seed must be an unsigned 32-bit integer');
    D := Ord(Text[I]) - Ord('0');
    if Parsed > (High(TGraphSeed) - TGraphSeed(D)) div 10 then
      raise EConvertError.Create('seed exceeds 4294967295');
    Parsed := Parsed * 10 + TGraphSeed(D);
  end;
  Result := Parsed;
end;

function TBrowserBuildingApplication.ReadZClip: Integer;
begin
  Result := StrToInt(FZClipInput.value);
  if Result < 0 then Result := 0;
  if Assigned(FBuilding) and (Result > Integer(FBuilding.Depth)) then
    Result := Integer(FBuilding.Depth);
end;

function TBrowserBuildingApplication.ReadViewMode: TBuilding3DViewMode;
begin
  if FViewModeSelect.value = 'footprint' then
    Result := b3vmFootprint
  else if FViewModeSelect.value = 'structure' then
    Result := b3vmStructure
  else if FViewModeSelect.value = 'envelope' then
    Result := b3vmEnvelopeRoof
  else if FViewModeSelect.value = 'complete' then
    Result := b3vmComplete
  else
    raise EBuilding3DView.Create('unknown browser view mode');
end;

function TBrowserBuildingApplication.SolverReportText(
  const AReport: TGraphSolveReport): String;
var
  Backtracks, Decisions, I, Propagations: Integer;
begin
  Backtracks := 0;
  Decisions := 0;
  Propagations := 0;
  for I := 0 to High(AReport.Passes) do
  begin
    Inc(Backtracks, AReport.Passes[I].Backtracks);
    Inc(Decisions, AReport.Passes[I].Decisions);
    Inc(Propagations, AReport.Passes[I].Propagations);
  end;
  Result := IntToStr(Length(AReport.ExecutionOrder)) +
    ' pass(es) executed · ' + IntToStr(Decisions) + ' decisions · ' +
    IntToStr(Propagations) + ' propagations · ' +
    IntToStr(Backtracks) + ' backtracks';
end;

procedure TBrowserBuildingApplication.GenerateBuilding;
var
  Candidate: TBuilding3D;
  Report: TGraphSolveReport;
  Validation: TBuilding3DValidationReport;
begin
  Candidate := nil;
  try
    Candidate := NewSolvedBuilding3DShowcase(ReadSeed, Report);
    VerifyBuilding3DShowcase(Candidate, Report, Validation);
  except
    Candidate.Free;
    raise;
  end;

  FProjection.Free;
  FProjection := nil;
  FView.Free;
  FView := nil;
  FBuilding.Free;
  FBuilding := Candidate;
  Candidate := nil;
  FSelectedX := BUILDING3D_SHOWCASE_ENTRANCE_X;
  FSelectedY := BUILDING3D_SHOWCASE_ENTRANCE_Y;
  FSelectedZ := BUILDING3D_SHOWCASE_ENTRANCE_Z;
  FSelectedQuad := -1;
  FValidationElement.textContent := 'Valid · ' +
    IntToStr(Validation.CheckedCells) + ' cells · ' +
    IntToStr(Validation.Structure.ReachableCount) + ' reachable · ' +
    IntToStr(Validation.PropCount) + ' prop';
  FSolverReportElement.textContent := SolverReportText(Report);
  FStatusElement.textContent := 'Solved all four passes';
  RefreshPresentation;
  SetBodyState('solved');
end;

procedure TBrowserBuildingApplication.RegenerateFrom(
  const AStage: TBuilding3DStage);
var
  Options: TBuilding3DValidationOptions;
  Report: TGraphSolveReport;
  Validation: TBuilding3DValidationReport;
begin
  if not Assigned(FBuilding) then
  begin
    GenerateBuilding;
    Exit;
  end;
  FBuilding.Seed := ReadSeed;
  if not FBuilding.TryRegenerateFrom(AStage, Report) then
  begin
    SetBodyState('contradiction');
    FStatusElement.textContent := 'Contradiction in pass ' +
      IntToStr(Report.FailedPassIndex);
    FSolverReportElement.textContent := SolverReportText(Report);
    Exit;
  end;
  Options := DefaultBuilding3DValidationOptions;
  Options.RequireFeature := True;
  if not ValidateBuilding3D(FBuilding, Options, Validation) then
    raise EBuilding3DView.Create('validation failed: ' +
      DescribeBuilding3DValidationIssue(Validation.Issue));
  FValidationElement.textContent := 'Valid · ' +
    IntToStr(Validation.CheckedCells) + ' cells · ' +
    IntToStr(Validation.Structure.ReachableCount) + ' reachable · ' +
    IntToStr(Validation.PropCount) + ' prop';
  FSolverReportElement.textContent := SolverReportText(Report);
  FStatusElement.textContent := 'Regenerated from ' + Building3DStageName(AStage);
  RefreshPresentation;
  SetBodyState('solved');
end;

procedure TBrowserBuildingApplication.RefreshPresentation;
begin
  FViewMode := ReadViewMode;
  FView.Free;
  FView := BuildBuilding3DView(FBuilding, FViewMode);
  RebuildProjection;
  RenderProjection;
  RefreshInspector;
end;

procedure TBrowserBuildingApplication.RebuildProjection;
var
  I, N, ZClip: Integer;
  Options: TVoxel3DIsometricOptions;
  Quad: TVoxel3DViewQuad;
  Source, Visible: TVoxel3DViewQuads;
begin
  FProjection.Free;
  FProjection := nil;
  //A painter-list index belongs only to the projection that produced it.
  //Keep the selected cell, but discard stale face metadata on every rebuild.
  FSelectedQuad := -1;
  Source := FView.CopyQuads;
  Visible := nil;
  ZClip := ReadZClip;
  SetLength(Visible, Length(Source));
  N := 0;
  for I := 0 to High(Source) do
  begin
    Quad := Source[I];
    if Quad.CellZ < ZClip then
    begin
      Visible[N] := Quad;
      Inc(N);
    end;
  end;
  SetLength(Visible, N);
  Options := DefaultVoxel3DIsometricOptions;
  Options.Yaw := FYaw;
  FProjection := ProjectVoxel3DIsometric(Visible, Options);
  FYawOutput.textContent := IntToStr(Ord(FYaw) * 90) + '°';
  if ZClip >= Integer(FBuilding.Depth) then
    FZClipOutput.textContent := 'all'
  else
    FZClipOutput.textContent := IntToStr(ZClip);
end;

procedure TBrowserBuildingApplication.RenderProjection;
var
  I, J: Integer;
  Quad: TVoxel3DProjectedQuad;
begin
  FContext.fillStyleAsColor := '#071416';
  FContext.fillRect(0, 0, FCanvas.width, FCanvas.height);
  FDrawOffsetX := (FCanvas.width - FProjection.Bounds.Width) div 2;
  FDrawOffsetY := (FCanvas.height - FProjection.Bounds.Height) div 2;
  for I := 0 to FProjection.QuadCount - 1 do
  begin
    Quad := FProjection.QuadAt(I);
    FContext.beginPath;
    FContext.moveTo(Quad.ScreenVertices[0].X + FDrawOffsetX,
      Quad.ScreenVertices[0].Y + FDrawOffsetY);
    for J := 1 to 3 do
      FContext.lineTo(Quad.ScreenVertices[J].X + FDrawOffsetX,
        Quad.ScreenVertices[J].Y + FDrawOffsetY);
    FContext.closePath;
    FContext.fillStyleAsColor := CanvasColor(Quad.FillColor);
    FContext.fill;
    FContext.strokeStyleAsColor := CanvasColor(Quad.EdgeColor);
    FContext.lineWidth := 1;
    FContext.stroke;

    if (Quad.CellX = FSelectedX) and
        (Quad.CellY = FSelectedY) and
        (Quad.CellZ = FSelectedZ) then
    begin
      FContext.strokeStyleAsColor := '#FFF1B8';
      FContext.lineWidth := 3;
      FContext.stroke;
    end;
  end;
  document.body.setAttribute('data-view-signature',
    Voxel3DSignatureHex(FProjection.Signature));
  document.body.setAttribute('data-face-count',
    IntToStr(FProjection.QuadCount));
  FViewSignatureElement.textContent :=
    Voxel3DSignatureHex(FProjection.Signature);
  FFaceCountElement.textContent := IntToStr(FProjection.QuadCount);
  FSignatureElement.textContent := FBuilding.PipelineSignature;
  FSeedOutputElement.textContent := UIntToStr(FBuilding.Seed);
end;

procedure TBrowserBuildingApplication.RefreshInspector;
var
  Quad: TVoxel3DProjectedQuad;
begin
  document.body.setAttribute('data-selected-x', IntToStr(FSelectedX));
  document.body.setAttribute('data-selected-y', IntToStr(FSelectedY));
  document.body.setAttribute('data-selected-z', IntToStr(FSelectedZ));
  if (FSelectedX < 0) or not Assigned(FBuilding) then
  begin
    FSelectedCoordinateElement.textContent := 'none';
    Exit;
  end;
  FSelectedCoordinateElement.textContent := '(' + IntToStr(FSelectedX) +
    ', ' + IntToStr(FSelectedY) + ', ' + IntToStr(FSelectedZ) + ')';
  FSelectedFootprintElement.textContent := Building3DFootprintRoleToken(
    FBuilding.FootprintRoleAt(FSelectedX, FSelectedY, FSelectedZ));
  FSelectedStructureElement.textContent := Building3DStructureKindToken(
    FBuilding.StructureKindAt(FSelectedX, FSelectedY, FSelectedZ));
  FSelectedEnvelopeElement.textContent := Building3DEnvelopeKindToken(
    FBuilding.EnvelopeKindAt(FSelectedX, FSelectedY, FSelectedZ));
  FSelectedPropElement.textContent := Building3DPropKindToken(
    FBuilding.PropKindAt(FSelectedX, FSelectedY, FSelectedZ));
  if (FSelectedQuad >= 0) and (FSelectedQuad < FProjection.QuadCount) then
  begin
    Quad := FProjection.QuadAt(FSelectedQuad);
    FSelectedFaceElement.textContent := Quad.LayerId + ' · ' +
      Quad.Material + ' · ' + Quad.Semantic + ' · face ' +
      IntToStr(Ord(Quad.Direction)) + ' · yaw ' +
      IntToStr(Voxel3DRotationDegrees(Quad.Rotation)) + '°';
  end
  else
    FSelectedFaceElement.textContent :=
      'The entrance is selected. Pick a visible face for draw metadata.';
end;

procedure TBrowserBuildingApplication.Rotate(const ADelta: Integer);
var
  N: Integer;
begin
  N := (Ord(FYaw) + ADelta) mod 4;
  if N < 0 then Inc(N, 4);
  FYaw := TVoxel3DViewYaw(N);
  FSelectedQuad := -1;
  RebuildProjection;
  RenderProjection;
  RefreshInspector;
end;

procedure TBrowserBuildingApplication.SelectFace(AEvent: TJSMouseEvent);
var
  Index, X, Y: Integer;
  Quad: TVoxel3DProjectedQuad;
  Rect: TJSDOMRect;
begin
  Rect := FCanvas.getBoundingClientRect;
  if (Rect.width <= 0) or (Rect.height <= 0) then Exit;
  X := Trunc((AEvent.clientX - Rect.left) * FCanvas.width / Rect.width) -
    FDrawOffsetX;
  Y := Trunc((AEvent.clientY - Rect.top) * FCanvas.height / Rect.height) -
    FDrawOffsetY;
  if not FProjection.HitTest(X, Y, Index) then Exit;
  Quad := FProjection.QuadAt(Index);
  FSelectedQuad := Index;
  FSelectedX := Quad.CellX;
  FSelectedY := Quad.CellY;
  FSelectedZ := Quad.CellZ;
  RenderProjection;
  RefreshInspector;
end;

procedure TBrowserBuildingApplication.RunSelfTest;

  procedure AssertTest(const ACondition: Boolean; const AMessage: String);
  begin
    if not ACondition then raise EBuilding3DView.Create(AMessage);
  end;

var
  CompleteCount, I, Index, StructureCount: Integer;
  PixelData: TJSUint8ClampedArray;
  PixelX, PixelY: Integer;
  Quad: TVoxel3DProjectedQuad;
  InitialSignature, RotatedSignature: TVoxel3DSignature;
begin
  try
    document.body.setAttribute('data-self-test', 'pending');
    FSeedInput.value := '0';
    FViewModeSelect.value := 'complete';
    FZClipInput.value := IntToStr(BUILDING3D_SHOWCASE_DEPTH);
    FYaw := v3vy0;
    GenerateBuilding;
    AssertTest(FBuilding.PipelineSignature =
      BUILDING3D_SHOWCASE_SEED_ZERO_PIPELINE_SIGNATURE,
      'seed-zero pipeline signature changed');
    CompleteCount := FProjection.QuadCount;
    AssertTest(CompleteCount > BUILDING3D_SHOWCASE_STRUCTURE_QUAD_COUNT,
      'complete view did not add prop geometry');
    InitialSignature := FProjection.Signature;

    Quad := FProjection.QuadAt(FProjection.QuadCount - 1);
    PixelX := 0;
    PixelY := 0;
    for I := 0 to 3 do
    begin
      Inc(PixelX, Quad.ScreenVertices[I].X);
      Inc(PixelY, Quad.ScreenVertices[I].Y);
    end;
    PixelX := PixelX div 4;
    PixelY := PixelY div 4;
    AssertTest(FProjection.HitTest(PixelX, PixelY, Index),
      'projected command hit testing failed');
    PixelData := FContext.getImageData(PixelX + FDrawOffsetX,
      PixelY + FDrawOffsetY, 1, 1).data;
    AssertTest((PixelData[0] <> 7) or (PixelData[1] <> 20) or
      (PixelData[2] <> 22) or (PixelData[3] <> 255),
      'Canvas2D did not render the signed command list');

    Rotate(1);
    RotatedSignature := FProjection.Signature;
    AssertTest(RotatedSignature <> InitialSignature,
      'quarter-turn camera did not change the view signature');
    Rotate(1);
    Rotate(1);
    Rotate(1);
    AssertTest(FProjection.Signature = InitialSignature,
      'four quarter turns did not recover the exact view signature');

    FViewModeSelect.value := 'structure';
    RefreshPresentation;
    StructureCount := FProjection.QuadCount;
    AssertTest(StructureCount = BUILDING3D_SHOWCASE_STRUCTURE_QUAD_COUNT,
      'structure view face count changed');
    AssertTest(CompleteCount > StructureCount,
      'complete view did not expose prop geometry');

    FViewModeSelect.value := 'complete';
    RefreshPresentation;
    AssertTest(FProjection.QuadCount = CompleteCount,
      'complete mode did not recover its exact face count');
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

function TBrowserBuildingApplication.HandleGenerate(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try GenerateBuilding except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserBuildingApplication.HandleNextSeed(
  AEvent: TJSMouseEvent): Boolean;
var
  Seed: TGraphSeed;
begin
  Result := False;
  try
    Seed := ReadSeed;
    if Seed = MAX_SEED then Seed := 0 else Inc(Seed);
    FSeedInput.value := UIntToStr(Seed);
    GenerateBuilding;
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserBuildingApplication.HandleRegenerateStructure(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try RegenerateFrom(b3sStructure) except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserBuildingApplication.HandleRegenerateEnvelope(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try RegenerateFrom(b3sEnvelopeRoof) except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserBuildingApplication.HandleRegenerateProps(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try RegenerateFrom(b3sProps) except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserBuildingApplication.HandleViewMode(AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try RefreshPresentation except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserBuildingApplication.HandleZClip(AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    RebuildProjection;
    RenderProjection;
    RefreshInspector;
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserBuildingApplication.HandleYawLeft(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try Rotate(-1) except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserBuildingApplication.HandleYawRight(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try Rotate(1) except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserBuildingApplication.HandleCanvasClick(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try SelectFace(AEvent) except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserBuildingApplication.HandleCanvasKeyDown(
  AEvent: TJSKeyboardEvent): Boolean;
begin
  Result := True;
  if AEvent.Key = 'ArrowLeft' then Rotate(-1)
  else if AEvent.Key = 'ArrowRight' then Rotate(1)
  else Exit;
  AEvent.preventDefault;
  Result := False;
end;

procedure TBrowserBuildingApplication.Run;
begin
  try
    BindDocument;
    BindEvents;
    FSeedInput.value := UIntToStr(BUILDING3D_SHOWCASE_DEFAULT_SEED);
    FViewModeSelect.value := 'complete';
    FZClipInput.max := IntToStr(BUILDING3D_SHOWCASE_DEPTH);
    FZClipInput.value := IntToStr(BUILDING3D_SHOWCASE_DEPTH);
    GenerateBuilding;
    if Pos('selftest=1', window.location.search) > 0 then
      RunSelfTest
    else
      document.body.setAttribute('data-self-test', 'not-requested');
  except
    on E: Exception do
      if Assigned(FStatusElement) then ShowError(E.Message)
      else window.console.error(E.Message);
  end;
end;

end.
