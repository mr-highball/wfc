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
unit browser_text_pass_composition_app;

{$mode delphi}{$H+}

interface

uses
  JS,
  Web,
  SysUtils,
  wfc,
  wfc_model,
  wfc_sequence,
  wfc_sequence_graph,
  wfc_text_passes,
  text_pass_composition_showcase;

type
  TBrowserTextPassCompositionApplication = class
  strict private
    FShowcase: TTextPassCompositionShowcase;
    FResult: TWfcTextPassResult;
    FReport: TWfcTextPassReport;
    FHasResult: Boolean;
    FSelectedPosition: Integer;

    FSeedInput: TJSHTMLInputElement;
    FGenerateButton: TJSHTMLButtonElement;
    FNextSeedButton: TJSHTMLButtonElement;
    FResetButton: TJSHTMLButtonElement;
    FConflictButton: TJSHTMLButtonElement;
    FClearAllButton: TJSHTMLButtonElement;
    FLayerSelect: TJSHTMLSelectElement;
    FPositionInput: TJSHTMLInputElement;
    FTokenSelect: TJSHTMLSelectElement;
    FLockButton: TJSHTMLButtonElement;
    FUnlockButton: TJSHTMLButtonElement;
    FOutputElement: TJSElement;
    FStatusElement: TJSElement;
    FSignatureElement: TJSElement;
    FInspectorElement: TJSElement;
    FValidationElement: TJSElement;
    FSolverElement: TJSElement;
    FTokenButtons: array[TWfcTextPassLayer,
      0..WFC_TEXT_PASS_SHOWCASE_LENGTH - 1] of TJSHTMLButtonElement;

    function RequireElement(const AId: String): TJSElement;
    procedure BindDocument;
    procedure BindEvents;
    procedure RecreateShowcase(const ASeed: TGraphSeed);
    procedure GenerateComposition;
    procedure RenderResult;
    procedure RefreshInspector;
    procedure RefreshTokenChoices;
    procedure SetBodyState(const AState, ASignature: String);
    procedure ShowError(const AMessage: String);
    function SelectedLayer: TWfcTextPassLayer;
    function ReadPosition: Integer;
    function ReadSeed: TGraphSeed;
    function TryParseSeed(const AText: String;
      out ASeed: TGraphSeed): Boolean;
    function SolverText: String;
    procedure RunSelfTest;
    procedure AssertTest(const ACondition: Boolean;
      const AMessage: String);

    function HandleGenerate(AEvent: TJSMouseEvent): Boolean;
    function HandleNextSeed(AEvent: TJSMouseEvent): Boolean;
    function HandleReset(AEvent: TJSMouseEvent): Boolean;
    function HandleConflict(AEvent: TJSMouseEvent): Boolean;
    function HandleClearAll(AEvent: TJSMouseEvent): Boolean;
    function HandleLock(AEvent: TJSMouseEvent): Boolean;
    function HandleUnlock(AEvent: TJSMouseEvent): Boolean;
    function HandleLayerChange(AEvent: TJSEvent): Boolean;
    function HandlePositionChange(AEvent: TJSEvent): Boolean;
    function HandleTokenClick(AEvent: TJSMouseEvent): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

implementation

const
  MAX_SEED = Cardinal($FFFFFFFF);
  SEED_ZERO_TEXT = 'A sun rises brightly!';
  SEED_ZERO_SIGNATURE = '1:69ABA6CE';

constructor TBrowserTextPassCompositionApplication.Create;
begin
  inherited Create;
  FShowcase := nil;
  FHasResult := False;
  FSelectedPosition := 0;
end;

destructor TBrowserTextPassCompositionApplication.Destroy;
begin
  FShowcase.Free;
  inherited Destroy;
end;

function TBrowserTextPassCompositionApplication.RequireElement(
  const AId: String): TJSElement;
begin
  Result := document.getElementById(AId);
  if not Assigned(Result) then
    raise ETextPassCompositionShowcase.Create(
      'browser demo is missing #' + AId);
end;

procedure TBrowserTextPassCompositionApplication.BindDocument;
var
  I: Integer;
  LLayer: TWfcTextPassLayer;
  LPrefix: String;
begin
  FSeedInput := TJSHTMLInputElement(RequireElement('seed-input'));
  FGenerateButton := TJSHTMLButtonElement(RequireElement('generate-button'));
  FNextSeedButton := TJSHTMLButtonElement(RequireElement('next-seed-button'));
  FResetButton := TJSHTMLButtonElement(RequireElement('reset-button'));
  FConflictButton := TJSHTMLButtonElement(RequireElement('conflict-button'));
  FClearAllButton := TJSHTMLButtonElement(RequireElement('clear-all-button'));
  FLayerSelect := TJSHTMLSelectElement(RequireElement('lock-layer'));
  FPositionInput := TJSHTMLInputElement(RequireElement('lock-position'));
  FTokenSelect := TJSHTMLSelectElement(RequireElement('lock-token'));
  FLockButton := TJSHTMLButtonElement(RequireElement('lock-button'));
  FUnlockButton := TJSHTMLButtonElement(RequireElement('unlock-button'));
  FOutputElement := RequireElement('output-text');
  FStatusElement := RequireElement('status');
  FSignatureElement := RequireElement('signature');
  FInspectorElement := RequireElement('lineage-inspector');
  FValidationElement := RequireElement('validation');
  FSolverElement := RequireElement('solver-report');

  for LLayer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
  begin
    LPrefix := WfcTextPassLayerName(LLayer);
    for I := 0 to WFC_TEXT_PASS_SHOWCASE_LENGTH - 1 do
    begin
      FTokenButtons[LLayer, I] := TJSHTMLButtonElement(
        RequireElement(LPrefix + '-' + IntToStr(I)));
      FTokenButtons[LLayer, I].setAttribute('data-layer',
        IntToStr(Ord(LLayer)));
      FTokenButtons[LLayer, I].setAttribute('data-position', IntToStr(I));
    end;
  end;
end;

procedure TBrowserTextPassCompositionApplication.BindEvents;
var
  I: Integer;
  LLayer: TWfcTextPassLayer;
begin
  FGenerateButton.onclick := @HandleGenerate;
  FNextSeedButton.onclick := @HandleNextSeed;
  FResetButton.onclick := @HandleReset;
  FConflictButton.onclick := @HandleConflict;
  FClearAllButton.onclick := @HandleClearAll;
  FLockButton.onclick := @HandleLock;
  FUnlockButton.onclick := @HandleUnlock;
  FLayerSelect.onchange := @HandleLayerChange;
  FPositionInput.onchange := @HandlePositionChange;
  for LLayer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
    for I := 0 to WFC_TEXT_PASS_SHOWCASE_LENGTH - 1 do
      FTokenButtons[LLayer, I].onclick := @HandleTokenClick;
end;

procedure TBrowserTextPassCompositionApplication.SetBodyState(
  const AState, ASignature: String);
begin
  document.body.setAttribute('data-state', AState);
  document.body.setAttribute('data-signature', ASignature);
  document.body.setAttribute('data-output-signature', ASignature);
  document.body.setAttribute('data-pass-count', '3');
  if FReport.TraceCaptured then
    document.body.setAttribute('data-trace-hash',
      UIntToStr(FReport.TraceHash))
  else
    document.body.setAttribute('data-trace-hash', '');
end;

procedure TBrowserTextPassCompositionApplication.ShowError(
  const AMessage: String);
begin
  SetBodyState('error', '');
  FStatusElement.textContent := 'Error: ' + AMessage;
  FSignatureElement.textContent := '';
  FValidationElement.textContent := 'Validation did not complete.';
  FSolverElement.textContent := 'No solver report is available.';
end;

function TBrowserTextPassCompositionApplication.TryParseSeed(
  const AText: String; out ASeed: TGraphSeed): Boolean;
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
  else if (Length(LText) >= 2) and (LText[1] = '0') and
      ((LText[2] = 'x') or (LText[2] = 'X')) then
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
    if LCharacter in ['0'..'9'] then
      LDigit := Cardinal(Ord(LCharacter) - Ord('0'))
    else if (LBase = 16) and (LCharacter in ['a'..'f']) then
      LDigit := Cardinal(Ord(LCharacter) - Ord('a') + 10)
    else if (LBase = 16) and (LCharacter in ['A'..'F']) then
      LDigit := Cardinal(Ord(LCharacter) - Ord('A') + 10)
    else
      Exit;
    if LValue > (MAX_SEED - LDigit) div LBase then
      Exit;
    LValue := (LValue * LBase) + LDigit;
  end;
  ASeed := TGraphSeed(LValue);
  Result := True;
end;

function TBrowserTextPassCompositionApplication.ReadSeed: TGraphSeed;
begin
  if not TryParseSeed(FSeedInput.value, Result) then
    raise EConvertError.Create('seed must be decimal, $hex, or 0xhex');
end;

function TBrowserTextPassCompositionApplication.SelectedLayer:
  TWfcTextPassLayer;
begin
  if FLayerSelect.value = 'structure' then
    Result := wtplStructure
  else if FLayerSelect.value = 'lexical' then
    Result := wtplLexical
  else if FLayerSelect.value = 'punctuation' then
    Result := wtplPunctuation
  else
    raise ERangeError.Create('unknown selected pass');
end;

function TBrowserTextPassCompositionApplication.ReadPosition: Integer;
begin
  if not TryStrToInt(FPositionInput.value, Result) or
      (Result < 0) or (Result >= WFC_TEXT_PASS_SHOWCASE_LENGTH) then
    raise ERangeError.Create('position must be from 0 through 4');
end;

procedure TBrowserTextPassCompositionApplication.RecreateShowcase(
  const ASeed: TGraphSeed);
var
  LCandidate: TTextPassCompositionShowcase;
begin
  LCandidate := TTextPassCompositionShowcase.Create(ASeed);
  FShowcase.Free;
  FShowcase := LCandidate;
  FHasResult := False;
  FSeedInput.value := UIntToStr(ASeed);
  RefreshTokenChoices;
end;

function TBrowserTextPassCompositionApplication.SolverText: String;
var
  I: Integer;
begin
  if FReport.Status = wtpsSolveFailed then
    Exit('contradiction pass=' + IntToStr(FReport.Solve.FailedPassIndex) +
      ' kind=' + IntToStr(Ord(FReport.Solve.Contradiction.Kind)) +
      ' entry=' + IntToStr(FReport.Solve.Contradiction.EntryIndex));
  Result := 'execution';
  for I := 0 to Length(FReport.Solve.ExecutionOrder) - 1 do
    Result := Result + ' ' + IntToStr(FReport.Solve.ExecutionOrder[I]);
  Result := Result + LineEnding + 'trace events=' +
    IntToStr(Length(FReport.Trace)) + ' hash=' +
    UIntToStr(FReport.TraceHash);
  for I := 0 to Length(FReport.Solve.Passes) - 1 do
    Result := Result + LineEnding + 'pass ' + IntToStr(I) +
      ': decisions=' + IntToStr(FReport.Solve.Passes[I].Decisions) +
      ' propagations=' + IntToStr(FReport.Solve.Passes[I].Propagations) +
      ' backtracks=' + IntToStr(FReport.Solve.Passes[I].Backtracks);
end;

procedure TBrowserTextPassCompositionApplication.GenerateComposition;
var
  LSignature: String;
begin
  FShowcase.Pipeline.Seed := ReadSeed;
  if not FShowcase.TryGenerate(FResult, FReport) then
  begin
    if FReport.Status = wtpsSolveFailed then
    begin
      SetBodyState('contradiction', '');
      FStatusElement.textContent := 'Constraints contradict in pass ' +
        IntToStr(FReport.Solve.FailedPassIndex) + '.';
      FValidationElement.textContent :=
        'No new result committed; the previous public result is preserved.';
      FSolverElement.textContent := SolverText;
      Exit;
    end;
    raise ETextPassCompositionShowcase.CreateFmt(
      'generation failed with status %d', [Ord(FReport.Status)]);
  end;
  FHasResult := True;
  LSignature := TextPassShowcaseSignature(FResult);
  SetBodyState('solved', LSignature);
  document.body.setAttribute('data-output', String(FResult.Text));
  FStatusElement.textContent := 'Solved all three passes atomically.';
  FSignatureElement.textContent := LSignature;
  FValidationElement.textContent := 'valid: ' +
    IntToStr(FReport.Validation.CheckedLayers) + ' paths, ' +
    IntToStr(FReport.Validation.CheckedRelations) +
    ' cross-pass relations, sanitized trace verified';
  FSolverElement.textContent := SolverText;
  RenderResult;
end;

procedure TBrowserTextPassCompositionApplication.RenderResult;
var
  I: Integer;
  LGenerated: TWfcGeneratedSequence;
  LLayer: TWfcTextPassLayer;
begin
  if not FHasResult then
    Exit;
  FOutputElement.textContent := String(FResult.Text);
  for LLayer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
  begin
    LGenerated := TextPassShowcaseSequence(FResult, LLayer);
    for I := 0 to WFC_TEXT_PASS_SHOWCASE_LENGTH - 1 do
    begin
      FTokenButtons[LLayer, I].textContent :=
        TextPassShowcaseDisplayToken(LLayer, LGenerated.Tokens[I]);
      if I = FSelectedPosition then
        FTokenButtons[LLayer, I].className := 'token selected'
      else
        FTokenButtons[LLayer, I].className := 'token';
    end;
  end;
  RefreshInspector;
end;

procedure TBrowserTextPassCompositionApplication.RefreshInspector;
var
  LGenerated: TWfcGeneratedSequence;
  LLayer: TWfcTextPassLayer;
  LText: String;
begin
  if not FHasResult then
  begin
    FInspectorElement.textContent := 'No solved lineage yet.';
    Exit;
  end;
  LText := 'position ' + IntToStr(FSelectedPosition);
  for LLayer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
  begin
    LGenerated := TextPassShowcaseSequence(FResult, LLayer);
    LText := LText + LineEnding + WfcTextPassLayerName(LLayer) +
      ': ' + TextPassShowcaseDisplayToken(LLayer,
        LGenerated.Tokens[FSelectedPosition]) +
      ' (state ' + IntToStr(
        LGenerated.StateIndices[FSelectedPosition]) + ')';
  end;
  FInspectorElement.textContent := LText;
end;

procedure TBrowserTextPassCompositionApplication.RefreshTokenChoices;
var
  I: Integer;
  LLayer: TWfcTextPassLayer;
  LModel: TWfcSequenceModel;
  LOption: TJSHTMLOptionElement;
begin
  if not Assigned(FShowcase) then
    Exit;
  LLayer := SelectedLayer;
  LModel := FShowcase.Pipeline.Model[LLayer];
  FTokenSelect.textContent := '';
  for I := 0 to LModel.PublicTokenCount - 1 do
  begin
    LOption := TJSHTMLOptionElement(document.createElement('option'));
    LOption.value := IntToStr(I);
    LOption.textContent := TextPassShowcaseDisplayToken(LLayer,
      LModel.PublicTokenAt(I));
    FTokenSelect.appendChild(LOption);
  end;
end;

procedure TBrowserTextPassCompositionApplication.AssertTest(
  const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise ETextPassCompositionShowcase.Create('self-test: ' + AMessage);
end;

procedure TBrowserTextPassCompositionApplication.RunSelfTest;
var
  LHash: TGraphTraceSignature;
begin
  document.body.setAttribute('data-self-test', 'pending');
  try
    RecreateShowcase(0);
    GenerateComposition;
    AssertTest(document.body.getAttribute('data-state') = 'solved',
      'seed zero did not solve');
    AssertTest(String(FResult.Text) = SEED_ZERO_TEXT,
      'seed-zero text changed');
    AssertTest(TextPassShowcaseSignature(FResult) = SEED_ZERO_SIGNATURE,
      'seed-zero signature changed');
    AssertTest(FReport.Validation.Valid and FReport.TraceValidation.Valid,
      'validation or sanitized trace failed');
    LHash := FReport.TraceHash;

    GenerateComposition;
    AssertTest((FReport.TraceHash = LHash) and
      (TextPassShowcaseSignature(FResult) = SEED_ZERO_SIGNATURE),
      'same-seed replay changed');

    FShowcase.ClearAllConstraints;
    FShowcase.Pipeline.IntersectAllowedTokens(wtplStructure, 1, 'ADJ');
    FShowcase.Pipeline.IntersectAllowedTokens(wtplLexical, 1, 'fox');
    GenerateComposition;
    AssertTest(document.body.getAttribute('data-state') = 'contradiction',
      'incompatible lineage did not contradict');

    FShowcase.ClearAllConstraints;
    GenerateComposition;
    AssertTest((document.body.getAttribute('data-state') = 'solved') and
      (TextPassShowcaseSignature(FResult) = SEED_ZERO_SIGNATURE) and
      (FReport.TraceHash = LHash),
      'clearing constraints did not restore exact replay');
    document.body.setAttribute('data-self-test', 'passed');
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-self-test', 'failed');
      document.body.setAttribute('data-self-test-message', E.Message);
      ShowError(E.Message);
    end;
  end;
end;

function TBrowserTextPassCompositionApplication.HandleGenerate(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    GenerateComposition;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserTextPassCompositionApplication.HandleNextSeed(
  AEvent: TJSMouseEvent): Boolean;
var
  LSeed: TGraphSeed;
begin
  Result := False;
  try
    LSeed := ReadSeed;
    if LSeed = High(TGraphSeed) then
      LSeed := 0
    else
      Inc(LSeed);
    FSeedInput.value := UIntToStr(LSeed);
    GenerateComposition;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserTextPassCompositionApplication.HandleReset(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    RecreateShowcase(WFC_TEXT_PASS_SHOWCASE_DEFAULT_SEED);
    FSelectedPosition := 0;
    FPositionInput.value := '0';
    GenerateComposition;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserTextPassCompositionApplication.HandleConflict(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    FShowcase.ClearAllConstraints;
    FShowcase.Pipeline.IntersectAllowedTokens(wtplStructure, 1, 'ADJ');
    FShowcase.Pipeline.IntersectAllowedTokens(wtplLexical, 1, 'fox');
    GenerateComposition;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserTextPassCompositionApplication.HandleClearAll(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    FShowcase.ClearAllConstraints;
    GenerateComposition;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserTextPassCompositionApplication.HandleLock(
  AEvent: TJSMouseEvent): Boolean;
var
  LLayer: TWfcTextPassLayer;
  LModel: TWfcSequenceModel;
  LPosition: Integer;
  LTokenIndex: Integer;
begin
  Result := False;
  try
    LLayer := SelectedLayer;
    LPosition := ReadPosition;
    if not TryStrToInt(FTokenSelect.value, LTokenIndex) then
      raise EConvertError.Create('select a public token');
    LModel := FShowcase.Pipeline.Model[LLayer];
    FShowcase.Pipeline.ClearAllowedTokens(LLayer, LPosition);
    FShowcase.Pipeline.IntersectAllowedTokens(LLayer, LPosition,
      LModel.PublicTokenAt(LTokenIndex));
    GenerateComposition;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserTextPassCompositionApplication.HandleUnlock(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    FShowcase.Pipeline.ClearAllowedTokens(SelectedLayer, ReadPosition);
    GenerateComposition;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserTextPassCompositionApplication.HandleLayerChange(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    RefreshTokenChoices;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserTextPassCompositionApplication.HandlePositionChange(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    FSelectedPosition := ReadPosition;
    RenderResult;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

function TBrowserTextPassCompositionApplication.HandleTokenClick(
  AEvent: TJSMouseEvent): Boolean;
var
  LElement: TJSElement;
  LPosition: Integer;
begin
  Result := False;
  try
    LElement := TJSElement(AEvent.currentTarget);
    if not TryStrToInt(LElement.getAttribute('data-position'), LPosition) then
      raise EConvertError.Create('token position is invalid');
    FSelectedPosition := LPosition;
    FPositionInput.value := IntToStr(LPosition);
    RenderResult;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

procedure TBrowserTextPassCompositionApplication.Run;
begin
  try
    BindDocument;
    BindEvents;
    RecreateShowcase(WFC_TEXT_PASS_SHOWCASE_DEFAULT_SEED);
    GenerateComposition;
    if Pos('selftest=1', window.location.search) > 0 then
      RunSelfTest
    else
      document.body.setAttribute('data-self-test', 'not-requested');
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

end.
