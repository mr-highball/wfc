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
unit wfc_training_text;

{$mode delphi}{$H+}

interface

uses
  wfc_training;

const
  WFC_TRAINING_TEXT_VERSION = 4;
  WFC_TRAINING_VALUE_QUOTA_TEXT_VERSION = 3;
  WFC_TRAINING_CONNECTIVITY_TEXT_VERSION = 4;
  WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH = 8388608;
  WFC_TRAINING_MAX_TEXT_LINE_COUNT = 11 +
    WFC_TRAINING_MAX_SAMPLE_COUNT + WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT;
  WFC_TRAINING_VALUE_QUOTA_MAX_TEXT_LINE_COUNT =
    WFC_TRAINING_MAX_TEXT_LINE_COUNT + 2 +
    WFC_TRAINING_MAX_VALUE_QUOTA_COUNT +
    WFC_TRAINING_MAX_TOTAL_VALUE_QUOTA_TOKEN_COUNT;
  WFC_TRAINING_CONNECTIVITY_MAX_TEXT_LINE_COUNT =
    WFC_TRAINING_VALUE_QUOTA_MAX_TEXT_LINE_COUNT + 2 +
    WFC_TRAINING_MAX_CONNECTIVITY_COUNT +
    WFC_TRAINING_MAX_TOTAL_CONNECTIVITY_VALUE_COUNT +
    WFC_TRAINING_MAX_TOTAL_CONNECTIVITY_TERMINAL_COUNT;

{ The editable source has no caller-supplied signature. Its immutable
  fingerprint is computed from validated contents by TWfcTrainingDocument. }
function EncodeWfcTrainingText(
  const ADocument: TWfcTrainingDocument): String;
function DecodeWfcTrainingText(const AText: String): TWfcTrainingDocument;
function WfcTrainingDocumentTextVersion(
  const ADocument: TWfcTrainingDocument): Integer;

implementation

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_text_codec;

const
  ARTIFACT_NAME = 'wfclearn';

procedure Fail(const AMessage: String);
begin
  WfcTextError(ARTIFACT_NAME, AMessage);
end;

function KindName(const AKind: TWfcTrainingKind): String;
begin
  Result := '';
  case AKind of
    wtkAdjacency1D: Result := 'adjacency1d';
    wtkAdjacency2D: Result := 'adjacency2d';
    wtkPattern2D: Result := 'pattern2d';
    wtkSequence: Result := 'sequence';
    wtkAdjacency3D: Result := 'adjacency3d';
  end;
  if Result = '' then
    Fail('unknown training kind');
end;

function BoundaryName(const ABoundary: TWfcModelBoundary): String;
begin
  case ABoundary of
    wmbOpen: Result := 'open';
    wmbWrap: Result := 'wrap';
  else
    Fail('unknown boundary');
  end;
end;

function SymmetryName(const ASymmetry: TWfcModelSymmetry): String;
begin
  case ASymmetry of
    wmsNone: Result := 'none';
    wmsD4: Result := 'd4';
    wmsCubeRotations: Result := 'cube24';
    wmsCubeFull: Result := 'cube48';
  else
    Fail('unknown symmetry');
  end;
end;

function ConnectivityMask(const AOpenings: TGraphDirections): Integer;
begin
  Result := 0;
  if gdNorth in AOpenings then Inc(Result, 1);
  if gdEast in AOpenings then Inc(Result, 2);
  if gdSouth in AOpenings then Inc(Result, 4);
  if gdWest in AOpenings then Inc(Result, 8);
  if gdUp in AOpenings then Inc(Result, 16);
  if gdDown in AOpenings then Inc(Result, 32);
end;

function ConnectivityOpenings(const AMask: Integer): TGraphDirections;
begin
  if (AMask < 0) or (AMask > 63) then Fail('connectivity opening mask is outside 0..63');
  Result := [];
  if (AMask and 1) <> 0 then Include(Result, gdNorth);
  if (AMask and 2) <> 0 then Include(Result, gdEast);
  if (AMask and 4) <> 0 then Include(Result, gdSouth);
  if (AMask and 8) <> 0 then Include(Result, gdWest);
  if (AMask and 16) <> 0 then Include(Result, gdUp);
  if (AMask and 32) <> 0 then Include(Result, gdDown);
end;

function BooleanText(const AValue: Boolean): String;
begin
  if AValue then Result := 'true' else Result := 'false';
end;

function ParseBoolean(const AValue: String): Boolean;
begin
  if AValue = 'true' then Exit(True);
  if AValue = 'false' then Exit(False);
  Fail('connectivity boolean must be true or false');
  Result := False;
end;

function EncodeWfcTrainingText(
  const ADocument: TWfcTrainingDocument): String;
var
  I: Integer;
  J: Integer;
  LLines: TWfcTextLines;
  LLine: Integer;
  LLength: Integer;
  LMetadata: TWfcTrainingMetadata;
  LOptions: TWfcTrainingOptions;
  LSample: TWfcTrainingSample;
  LVersion: Integer;
  LQuota: TWfcTrainingValueQuota;
  LConnectivity: TWfcTrainingConnectivity;
  LLineCount: Integer;

  procedure Add(const ALine: String);
  begin
    if Length(ALine) > WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH -
        LLength - 1 then
      Fail('encoded document exceeds the version-1 length limit');
    Inc(LLength, Length(ALine) + 1);
    LLines[LLine] := ALine;
    Inc(LLine);
  end;

  function Token(const AValue: TWfcModelToken): String;
  begin
    Result := WfcTextEncodeToken(AValue, ARTIFACT_NAME);
  end;

begin
  if ADocument = nil then
    Fail('document is nil');
  LLineCount := 11 + ADocument.SampleCount + ADocument.TotalTokenCount;
  if (ADocument.ValueQuotaCount > 0) or (ADocument.ConnectivityCount > 0) then
  begin
    Inc(LLineCount, 2 + ADocument.ValueQuotaCount);
    for I := 0 to ADocument.ValueQuotaCount - 1 do
      Inc(LLineCount, Length(ADocument.ValueQuotaAt(I).Values));
  end;
  if ADocument.ConnectivityCount > 0 then
  begin
    Inc(LLineCount, 2 + ADocument.ConnectivityCount);
    for I := 0 to ADocument.ConnectivityCount - 1 do
    begin
      LConnectivity := ADocument.ConnectivityAt(I);
      Inc(LLineCount, Length(LConnectivity.RequiredPositions) +
        Length(LConnectivity.Values));
    end;
  end;
  SetLength(LLines, LLineCount);
  LLine := 0;
  LLength := 0;
  LMetadata := ADocument.CopyMetadata;
  LOptions := ADocument.CopyOptions;
  LVersion := WfcTrainingDocumentTextVersion(ADocument);
  Add('wfclearn=' + IntToStr(LVersion));
  Add('name=' + Token(LMetadata.Name));
  Add('license=' + Token(LMetadata.LicenseIdentifier));
  Add('source=' + Token(LMetadata.SourceDescription));
  Add('kind=' + KindName(LOptions.Kind));
  Add('boundary=' + BoundaryName(LOptions.Boundary));
  Add('symmetry=' + SymmetryName(LOptions.Symmetry));
  Add('footprint=' + IntToStr(LOptions.PatternWidth) + ',' +
    IntToStr(LOptions.PatternHeight));
  Add('order=' + IntToStr(LOptions.Order));
  Add('samples=' + IntToStr(ADocument.SampleCount));
  for I := 0 to ADocument.SampleCount - 1 do
  begin
    LSample := ADocument.SampleAt(I);
    if LVersion >= 2 then
      Add('sample=' + IntToStr(I) + ',' + IntToStr(LSample.Width) +
        ',' + IntToStr(LSample.Height) + ',' + IntToStr(LSample.Depth) +
        ',' + Token(LSample.Name))
    else
      Add('sample=' + IntToStr(I) + ',' + IntToStr(LSample.Width) +
        ',' + IntToStr(LSample.Height) + ',' + Token(LSample.Name));
    for J := 0 to Length(LSample.Tokens) - 1 do
      Add('token=' + IntToStr(I) + ',' + IntToStr(J) + ',' +
        Token(LSample.Tokens[J]));
  end;
  if (ADocument.ValueQuotaCount > 0) or (ADocument.ConnectivityCount > 0) then
  begin
    Add('value-quota-version=' + IntToStr(ADocument.ValueQuotaVersion));
    Add('value-quotas=' + IntToStr(ADocument.ValueQuotaCount));
    for I := 0 to ADocument.ValueQuotaCount - 1 do
    begin
      LQuota := ADocument.ValueQuotaAt(I);
      Add('value-quota=' + IntToStr(I) + ',' + Token(LQuota.LabelText) +
        ',' + IntToStr(LQuota.MinimumCount) + ',' +
        IntToStr(LQuota.MaximumCount) + ',' + IntToStr(Length(LQuota.Values)));
      for J := 0 to Length(LQuota.Values) - 1 do
        Add('quota-token=' + IntToStr(I) + ',' + IntToStr(J) + ',' +
          Token(LQuota.Values[J]));
    end;
  end;
  if ADocument.ConnectivityCount > 0 then
  begin
    Add('connectivity-version=' + IntToStr(WFC_TRAINING_CONNECTIVITY_VERSION));
    Add('connectivities=' + IntToStr(ADocument.ConnectivityCount));
    for I := 0 to ADocument.ConnectivityCount - 1 do
    begin
      LConnectivity := ADocument.ConnectivityAt(I);
      Add('connectivity=' + IntToStr(I) + ',' + Token(LConnectivity.LabelText) +
        ',' + IntToStr(LConnectivity.Root.X) + ',' + IntToStr(LConnectivity.Root.Y) +
        ',' + IntToStr(LConnectivity.Root.Z) + ',' +
        BooleanText(LConnectivity.RequireAllParticipants) + ',' +
        IntToStr(Length(LConnectivity.RequiredPositions)) + ',' +
        IntToStr(Length(LConnectivity.Values)));
      for J := 0 to Length(LConnectivity.RequiredPositions) - 1 do
        Add('terminal=' + IntToStr(I) + ',' + IntToStr(J) + ',' +
          IntToStr(LConnectivity.RequiredPositions[J].X) + ',' +
          IntToStr(LConnectivity.RequiredPositions[J].Y) + ',' +
          IntToStr(LConnectivity.RequiredPositions[J].Z));
      for J := 0 to Length(LConnectivity.Values) - 1 do
        Add('profile=' + IntToStr(I) + ',' + IntToStr(J) + ',' +
          Token(LConnectivity.Values[J].Value) + ',' +
          IntToStr(ConnectivityMask(LConnectivity.Values[J].Openings)) + ',' +
          BooleanText(LConnectivity.Values[J].RequiredByValue));
    end;
  end;
  Add('end');
  Result := WfcTextJoinCanonicalLines(LLines, ARTIFACT_NAME);
end;

function WfcTrainingDocumentTextVersion(
  const ADocument: TWfcTrainingDocument): Integer;
begin
  if ADocument = nil then Fail('document is nil');
  if ADocument.ConnectivityCount > 0 then Exit(WFC_TRAINING_CONNECTIVITY_TEXT_VERSION);
  if ADocument.ValueQuotaCount > 0 then Exit(WFC_TRAINING_VALUE_QUOTA_TEXT_VERSION);
  if ADocument.CopyOptions.Kind = wtkAdjacency3D then Exit(2);
  Result := 1;
end;

procedure PreflightText(const AText: String);
var
  I: Integer;
  LLines: Integer;
  LLineLimit: Integer;
  LHeader: String;
begin
  if Length(AText) > WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH then
    Fail('document exceeds the version-1 encoded length limit');
  LHeader := Copy(AText, 1, 11);
  if (LHeader = 'wfclearn=1'#10) or (LHeader = 'wfclearn=2'#10) then
    LLineLimit := WFC_TRAINING_MAX_TEXT_LINE_COUNT
  else if LHeader = 'wfclearn=3'#10 then
    LLineLimit := WFC_TRAINING_VALUE_QUOTA_MAX_TEXT_LINE_COUNT
  else if LHeader = 'wfclearn=4'#10 then
    LLineLimit := WFC_TRAINING_CONNECTIVITY_MAX_TEXT_LINE_COUNT
  else
    Fail('expected supported wfclearn header');
  LLines := 0;
  for I := 1 to Length(AText) do
  begin
    if Ord(AText[I]) > 127 then
      Fail('document must be ASCII with percent-encoded UTF-8 tokens');
    if AText[I] = #10 then
    begin
      if LLines = LLineLimit then
        Fail('document exceeds its versioned line count limit');
      Inc(LLines);
    end;
  end;
end;

function DecodeWfcTrainingText(const AText: String): TWfcTrainingDocument;
var
  I: Integer;
  J: Integer;
  LArea: Integer;
  LCount: Integer;
  LEncodedTotal: Integer;
  LFields: TWfcTextLines;
  LLine: Integer;
  LLines: TWfcTextLines;
  LMetadata: TWfcTrainingMetadata;
  LOptions: TWfcTrainingOptions;
  LSamples: TWfcTrainingSamples;
  LText: String;
  LTotal: Integer;
  LVersion: Integer;
  LQuotaCount, LQuotaTokens, LTotalQuotaTokens: Integer;
  LQuotaVersion, LConnectivityCount, LTerminalCount, LProfileCount: Integer;
  LTotalTerminals, LTotalProfiles: Integer;
  LQuotas: TWfcTrainingValueQuotas;
  LConnectivities: TWfcTrainingConnectivities;

  function ReadLine: String;
  begin
    if LLine >= Length(LLines) then
      Fail('document ended before all declared records');
    Result := LLines[LLine];
    Inc(LLine);
  end;

  function ReadValue(const APrefix: String): String;
  begin
    Result := WfcTextValueAfterPrefix(ReadLine, APrefix, APrefix,
      ARTIFACT_NAME);
  end;

  function Number(const AValue: String): Integer;
  begin
    Result := WfcTextParseCanonicalInteger(AValue, 'integer',
      ARTIFACT_NAME);
  end;

  function Token(const AValue: String): TWfcModelToken;
  begin
    if Length(AValue) > WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH then
      Fail('token exceeds the version-1 encoded length limit');
    if Length(AValue) > WFC_TRAINING_MAX_TOTAL_ENCODED_TOKEN_LENGTH -
        LEncodedTotal then
      Fail('aggregate encoded tokens exceed the version-1 limit');
    Inc(LEncodedTotal, Length(AValue));
    Result := WfcTextDecodeToken(AValue, ARTIFACT_NAME);
  end;

  procedure Fields(const AValue: String; const ACount: Integer);
  var
    K: Integer;
    LStart: Integer;
    LStop: Integer;
  begin
    SetLength(LFields, ACount);
    LStart := 1;
    for K := 0 to ACount - 2 do
    begin
      LStop := WfcTextFindCharacter(AValue, ',', LStart);
      if LStop = 0 then
        Fail('record has too few fields');
      LFields[K] := Copy(AValue, LStart, LStop - LStart);
      LStart := LStop + 1;
    end;
    if WfcTextFindCharacter(AValue, ',', LStart) <> 0 then
      Fail('record has too many fields');
    LFields[ACount - 1] := Copy(AValue, LStart,
      Length(AValue) - LStart + 1);
  end;

begin
  Result := nil;
  PreflightText(AText);
  WfcTextSplitCanonicalLines(AText, ARTIFACT_NAME, LLines);
  LLine := 0;
  LEncodedTotal := 0;
  LText := ReadLine;
  if LText = 'wfclearn=1' then
    LVersion := 1
  else if LText = 'wfclearn=2' then
    LVersion := 2
  else if LText = 'wfclearn=3' then
    LVersion := 3
  else if LText = 'wfclearn=4' then
    LVersion := 4
  else
    Fail('expected supported wfclearn header');
  LMetadata.Name := Token(ReadValue('name='));
  LMetadata.LicenseIdentifier := Token(ReadValue('license='));
  LMetadata.SourceDescription := Token(ReadValue('source='));
  LText := ReadValue('kind=');
  if LText = 'adjacency1d' then LOptions.Kind := wtkAdjacency1D
  else if LText = 'adjacency2d' then LOptions.Kind := wtkAdjacency2D
  else if LText = 'pattern2d' then LOptions.Kind := wtkPattern2D
  else if LText = 'sequence' then LOptions.Kind := wtkSequence
  else if LText = 'adjacency3d' then LOptions.Kind := wtkAdjacency3D
  else Fail('unknown training kind');
  if (LVersion = 1) and (LOptions.Kind = wtkAdjacency3D) then
    Fail('wfclearn=1 cannot encode adjacency3d training')
  else if (LVersion = 2) and (LOptions.Kind <> wtkAdjacency3D) then
    Fail('wfclearn=2 requires adjacency3d training');
  LText := ReadValue('boundary=');
  if LText = 'open' then LOptions.Boundary := wmbOpen
  else if LText = 'wrap' then LOptions.Boundary := wmbWrap
  else Fail('unknown boundary');
  LText := ReadValue('symmetry=');
  if LText = 'none' then LOptions.Symmetry := wmsNone
  else if LText = 'd4' then LOptions.Symmetry := wmsD4
  else if LText = 'cube24' then LOptions.Symmetry := wmsCubeRotations
  else if LText = 'cube48' then LOptions.Symmetry := wmsCubeFull
  else Fail('unknown symmetry');
  if (LVersion = 1) and
      not (LOptions.Symmetry in [wmsNone, wmsD4]) then
    Fail('wfclearn=1 cannot encode cube symmetry');
  Fields(ReadValue('footprint='), 2);
  LOptions.PatternWidth := Number(LFields[0]);
  LOptions.PatternHeight := Number(LFields[1]);
  LOptions.Order := Number(ReadValue('order='));
  LCount := Number(ReadValue('samples='));
  if (LCount < 1) or (LCount > WFC_TRAINING_MAX_SAMPLE_COUNT) then
    Fail('sample count is outside the version-1 limit');
  { At least one sample header and one token per sample must follow. }
  if LCount > (Length(LLines) - LLine - 1) div 2 then
    Fail('sample count exceeds the available records');
  SetLength(LSamples, LCount);
  LTotal := 0;
  for I := 0 to LCount - 1 do
  begin
    if LVersion >= 2 then
      Fields(ReadValue('sample='), 5)
    else
      Fields(ReadValue('sample='), 4);
    if Number(LFields[0]) <> I then
      Fail('sample indices must be contiguous and ordered');
    LSamples[I].Width := Number(LFields[1]);
    LSamples[I].Height := Number(LFields[2]);
    if LVersion >= 2 then
    begin
      LSamples[I].Depth := Number(LFields[3]);
      LSamples[I].Name := Token(LFields[4]);
    end
    else
    begin
      LSamples[I].Depth := 1;
      LSamples[I].Name := Token(LFields[3]);
    end;
    if (LSamples[I].Width < 1) or (LSamples[I].Height < 1) or
        (LSamples[I].Width > WFC_TRAINING_MAX_DIMENSION) or
        (LSamples[I].Height > WFC_TRAINING_MAX_DIMENSION) then
      Fail('sample dimensions are outside the version-1 limit');
    if (LVersion >= 2) and ((LSamples[I].Depth < 1) or
        (LSamples[I].Depth > WFC_TRAINING_MAX_DIMENSION)) then
      Fail('sample depth is outside the version-1 limit');
    if (LOptions.Kind <> wtkAdjacency3D) and (LSamples[I].Depth <> 1) then
      Fail('non-volume samples require depth one');
    if LSamples[I].Width > WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT div
        LSamples[I].Height then
      Fail('sample area exceeds the version-1 token limit');
    LArea := LSamples[I].Width * LSamples[I].Height;
    if (LVersion >= 2) and
        (LArea > WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT div
        LSamples[I].Depth) then
      Fail('sample volume exceeds the version-1 token limit');
    if LVersion >= 2 then
      LArea := LArea * LSamples[I].Depth;
    if LArea > WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT - LTotal then
      Fail('aggregate sample area exceeds the version-1 token limit');
    Inc(LTotal, LArea);
    if LArea > Length(LLines) - LLine - 1 then
      Fail('sample area exceeds the available token records');
    SetLength(LSamples[I].Tokens, LArea);
    for J := 0 to LArea - 1 do
    begin
      Fields(ReadValue('token='), 3);
      if (Number(LFields[0]) <> I) or (Number(LFields[1]) <> J) then
        Fail('token indices must follow sample and row-major order');
      LSamples[I].Tokens[J] := Token(LFields[2]);
    end;
  end;
  if LVersion >= WFC_TRAINING_VALUE_QUOTA_TEXT_VERSION then
  begin
    LQuotaVersion := Number(ReadValue('value-quota-version='));
    if (LQuotaVersion <> WFC_TRAINING_VALUE_QUOTA_VERSION) and
        not ((LVersion = WFC_TRAINING_CONNECTIVITY_TEXT_VERSION) and (LQuotaVersion = 0)) then
      Fail('unsupported training value-quota version');
    LQuotaCount := Number(ReadValue('value-quotas='));
    if (LQuotaCount < 0) or (LQuotaCount > WFC_TRAINING_MAX_VALUE_QUOTA_COUNT) then
      Fail('quota count is outside the allowed range');
    if ((LQuotaCount = 0) and (LQuotaVersion <> 0)) or
        ((LQuotaCount <> 0) and (LQuotaVersion <> WFC_TRAINING_VALUE_QUOTA_VERSION)) then
      Fail('quota capability version does not match its registry');
    if LQuotaCount > (Length(LLines) - LLine - 1) div 2 then
      Fail('quota count exceeds the available records');
    SetLength(LQuotas, LQuotaCount);
    LTotalQuotaTokens := 0;
    for I := 0 to LQuotaCount - 1 do
    begin
      Fields(ReadValue('value-quota='), 5);
      if Number(LFields[0]) <> I then
        Fail('quota indices must be contiguous and ordered');
      LQuotas[I].LabelText := Token(LFields[1]);
      if LQuotas[I].LabelText = '' then Fail('quota label cannot be empty');
      LQuotas[I].MinimumCount := Number(LFields[2]);
      LQuotas[I].MaximumCount := Number(LFields[3]);
      if LQuotas[I].MinimumCount > LQuotas[I].MaximumCount then
        Fail('quota minimum exceeds maximum');
      LQuotaTokens := Number(LFields[4]);
      if (LQuotaTokens < 1) or (LQuotaTokens > WFC_TRAINING_MAX_VALUE_QUOTA_TOKEN_COUNT) then
        Fail('quota token count is outside the allowed range');
      if LQuotaTokens > WFC_TRAINING_MAX_TOTAL_VALUE_QUOTA_TOKEN_COUNT - LTotalQuotaTokens then
        Fail('aggregate quota tokens exceed the limit');
      Inc(LTotalQuotaTokens, LQuotaTokens);
      if LQuotaTokens > Length(LLines) - LLine - 1 - 2 * (LQuotaCount - I - 1) then
        Fail('quota token count exceeds the available records');
      SetLength(LQuotas[I].Values, LQuotaTokens);
      for J := 0 to LQuotaTokens - 1 do
      begin
        Fields(ReadValue('quota-token='), 3);
        if (Number(LFields[0]) <> I) or (Number(LFields[1]) <> J) then
          Fail('quota token indices must be contiguous and ordered');
        LQuotas[I].Values[J] := Token(LFields[2]);
        if LQuotas[I].Values[J] = '' then Fail('quota token cannot be empty');
      end;
    end;
  end;
  if LVersion = WFC_TRAINING_CONNECTIVITY_TEXT_VERSION then
  begin
    if Number(ReadValue('connectivity-version=')) <> WFC_TRAINING_CONNECTIVITY_VERSION then
      Fail('unsupported training connectivity version');
    LConnectivityCount := Number(ReadValue('connectivities='));
    if (LConnectivityCount < 1) or
        (LConnectivityCount > WFC_TRAINING_MAX_CONNECTIVITY_COUNT) then
      Fail('version 4 requires a nonempty bounded connectivity registry');
    if LConnectivityCount > (Length(LLines) - LLine - 1) div 2 then
      Fail('connectivity count exceeds the available records');
    SetLength(LConnectivities, LConnectivityCount);
    LTotalTerminals := 0; LTotalProfiles := 0;
    for I := 0 to LConnectivityCount - 1 do
    begin
      Fields(ReadValue('connectivity='), 8);
      if Number(LFields[0]) <> I then
        Fail('connectivity indices must be contiguous and ordered');
      LConnectivities[I].LabelText := Token(LFields[1]);
      if LConnectivities[I].LabelText = '' then Fail('connectivity label cannot be empty');
      LConnectivities[I].Root.X := Number(LFields[2]);
      LConnectivities[I].Root.Y := Number(LFields[3]);
      LConnectivities[I].Root.Z := Number(LFields[4]);
      LConnectivities[I].RequireAllParticipants := ParseBoolean(LFields[5]);
      LTerminalCount := Number(LFields[6]);
      LProfileCount := Number(LFields[7]);
      if (LTerminalCount < 0) or (LTerminalCount > WFC_TRAINING_MAX_CONNECTIVITY_TERMINAL_COUNT) then
        Fail('connectivity terminal count exceeds the limit');
      if (LProfileCount < 1) or (LProfileCount > WFC_TRAINING_MAX_CONNECTIVITY_VALUE_COUNT) then
        Fail('connectivity profile count is outside the allowed range');
      if LTerminalCount > WFC_TRAINING_MAX_TOTAL_CONNECTIVITY_TERMINAL_COUNT - LTotalTerminals then
        Fail('aggregate connectivity terminals exceed the limit');
      if LProfileCount > WFC_TRAINING_MAX_TOTAL_CONNECTIVITY_VALUE_COUNT - LTotalProfiles then
        Fail('aggregate connectivity profiles exceed the limit');
      Inc(LTotalTerminals, LTerminalCount); Inc(LTotalProfiles, LProfileCount);
      if LTerminalCount + LProfileCount > Length(LLines) - LLine - 1 -
          2 * (LConnectivityCount - I - 1) then
        Fail('connectivity child counts exceed the available records');
      SetLength(LConnectivities[I].RequiredPositions, LTerminalCount);
      SetLength(LConnectivities[I].Values, LProfileCount);
      for J := 0 to LTerminalCount - 1 do
      begin
        Fields(ReadValue('terminal='), 5);
        if (Number(LFields[0]) <> I) or (Number(LFields[1]) <> J) then
          Fail('terminal indices must be contiguous and ordered');
        LConnectivities[I].RequiredPositions[J].X := Number(LFields[2]);
        LConnectivities[I].RequiredPositions[J].Y := Number(LFields[3]);
        LConnectivities[I].RequiredPositions[J].Z := Number(LFields[4]);
      end;
      for J := 0 to LProfileCount - 1 do
      begin
        Fields(ReadValue('profile='), 5);
        if (Number(LFields[0]) <> I) or (Number(LFields[1]) <> J) then
          Fail('profile indices must be contiguous and ordered');
        LConnectivities[I].Values[J].Value := Token(LFields[2]);
        if LConnectivities[I].Values[J].Value = '' then Fail('profile token cannot be empty');
        LConnectivities[I].Values[J].Openings := ConnectivityOpenings(Number(LFields[3]));
        LConnectivities[I].Values[J].RequiredByValue := ParseBoolean(LFields[4]);
      end;
    end;
  end;
  if ReadLine <> 'end' then
    Fail('expected end marker');
  if LLine <> Length(LLines) then
    Fail('records follow the end marker');
  try
    Result := TWfcTrainingDocument.Create(LMetadata, LOptions, LSamples,
      LQuotas, LConnectivities);
  except
    on E: EWfcTraining do
      Fail(E.Message);
  end;
  try
    if EncodeWfcTrainingText(Result) <> AText then
      Fail('document is not canonical');
  except
    Result.Free;
    Result := nil;
    raise;
  end;
end;

end.
