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
  WFC_TRAINING_TEXT_VERSION = 1;
  WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH = 8388608;
  WFC_TRAINING_MAX_TEXT_LINE_COUNT = 11 +
    WFC_TRAINING_MAX_SAMPLE_COUNT + WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT;

{ The editable source has no caller-supplied signature. Its immutable
  fingerprint is computed from validated contents by TWfcTrainingDocument. }
function EncodeWfcTrainingText(
  const ADocument: TWfcTrainingDocument): String;
function DecodeWfcTrainingText(const AText: String): TWfcTrainingDocument;

implementation

uses
  SysUtils,
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
  else
    Fail('unknown symmetry');
  end;
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
  SetLength(LLines, 11 + ADocument.SampleCount +
    ADocument.TotalTokenCount);
  LLine := 0;
  LLength := 0;
  LMetadata := ADocument.CopyMetadata;
  LOptions := ADocument.CopyOptions;
  Add('wfclearn=1');
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
    Add('sample=' + IntToStr(I) + ',' + IntToStr(LSample.Width) +
      ',' + IntToStr(LSample.Height) + ',' + Token(LSample.Name));
    for J := 0 to Length(LSample.Tokens) - 1 do
      Add('token=' + IntToStr(I) + ',' + IntToStr(J) + ',' +
        Token(LSample.Tokens[J]));
  end;
  Add('end');
  Result := WfcTextJoinCanonicalLines(LLines, ARTIFACT_NAME);
end;

procedure PreflightText(const AText: String);
var
  I: Integer;
  LLines: Integer;
begin
  if Length(AText) > WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH then
    Fail('document exceeds the version-1 encoded length limit');
  LLines := 0;
  for I := 1 to Length(AText) do
  begin
    if Ord(AText[I]) > 127 then
      Fail('document must be ASCII with percent-encoded UTF-8 tokens');
    if AText[I] = #10 then
    begin
      if LLines = WFC_TRAINING_MAX_TEXT_LINE_COUNT then
        Fail('document exceeds the version-1 line count limit');
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
  if ReadLine <> 'wfclearn=1' then
    Fail('expected wfclearn=1 header');
  LMetadata.Name := Token(ReadValue('name='));
  LMetadata.LicenseIdentifier := Token(ReadValue('license='));
  LMetadata.SourceDescription := Token(ReadValue('source='));
  LText := ReadValue('kind=');
  if LText = 'adjacency1d' then LOptions.Kind := wtkAdjacency1D
  else if LText = 'adjacency2d' then LOptions.Kind := wtkAdjacency2D
  else if LText = 'pattern2d' then LOptions.Kind := wtkPattern2D
  else if LText = 'sequence' then LOptions.Kind := wtkSequence
  else Fail('unknown training kind');
  LText := ReadValue('boundary=');
  if LText = 'open' then LOptions.Boundary := wmbOpen
  else if LText = 'wrap' then LOptions.Boundary := wmbWrap
  else Fail('unknown boundary');
  LText := ReadValue('symmetry=');
  if LText = 'none' then LOptions.Symmetry := wmsNone
  else if LText = 'd4' then LOptions.Symmetry := wmsD4
  else Fail('unknown symmetry');
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
    Fields(ReadValue('sample='), 4);
    if Number(LFields[0]) <> I then
      Fail('sample indices must be contiguous and ordered');
    LSamples[I].Width := Number(LFields[1]);
    LSamples[I].Height := Number(LFields[2]);
    LSamples[I].Name := Token(LFields[3]);
    if (LSamples[I].Width < 1) or (LSamples[I].Height < 1) or
        (LSamples[I].Width > WFC_TRAINING_MAX_DIMENSION) or
        (LSamples[I].Height > WFC_TRAINING_MAX_DIMENSION) then
      Fail('sample dimensions are outside the version-1 limit');
    if LSamples[I].Width > WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT div
        LSamples[I].Height then
      Fail('sample area exceeds the version-1 token limit');
    LArea := LSamples[I].Width * LSamples[I].Height;
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
  if ReadLine <> 'end' then
    Fail('expected end marker');
  if LLine <> Length(LLines) then
    Fail('records follow the end marker');
  try
    Result := TWfcTrainingDocument.Create(LMetadata, LOptions, LSamples);
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
