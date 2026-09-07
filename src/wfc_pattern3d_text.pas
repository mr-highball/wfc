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
unit wfc_pattern3d_text;

{$mode delphi}{$H+}

interface

uses wfc_pattern3d;

const
  WFC_PATTERN_3D_TEXT_VERSION = 2;
  WFC_PATTERN_3D_MAX_ENCODED_TEXT_LENGTH = 16777216;
  { Eleven fixed lines plus bounded source, palette, and pattern records.
    Compatibility is derived, so no quadratic relation records occur. }
  WFC_PATTERN_3D_MAX_TEXT_LINE_COUNT = 70667;

function EncodeWfcPattern3DText(const AModel: TWfcOverlappingModel3D): String;
function DecodeWfcPattern3DText(const AText: String): TWfcOverlappingModel3D;

implementation

uses SysUtils, wfc_model, wfc_text_codec {$IFDEF PAS2JS}, JS{$ENDIF};

const
  ARTIFACT_NAME = 'WFC volume pattern';
  FIXED_LINE_COUNT = 11;

procedure Fail(const AMessage: String);
begin
  WfcTextError(ARTIFACT_NAME, AMessage);
end;

function Number(const AText, AName: String): Integer;
begin
  Result := WfcTextParseCanonicalInteger(AText, AName, ARTIFACT_NAME);
end;

function Bounded(const AValue, AMinimum, AMaximum: Integer;
  const AName: String): Integer;
begin
  if (AValue < AMinimum) or (AValue > AMaximum) then
    Fail(AName + ' exceeds its supported range');
  Result := AValue;
end;

function Product(const A, B, ALimit: Integer; const AName: String): Integer;
begin
  if (A < 1) or (B < 1) or (A > ALimit div B) then
    Fail(AName + ' exceeds its supported cell limit');
  Result := A * B;
end;

function Volume(const AWidth, AHeight, ADepth, ADimensionLimit,
  ACellLimit: Integer; const AName: String): Integer;
begin
  Bounded(AWidth, 1, ADimensionLimit, AName + ' width');
  Bounded(AHeight, 1, ADimensionLimit, AName + ' height');
  Bounded(ADepth, 1, ADimensionLimit, AName + ' depth');
  Result := Product(AWidth, AHeight, ACellLimit, AName);
  Result := Product(Result, ADepth, ACellLimit, AName);
end;

procedure Envelope(const AText: String);
var I, Lines: Integer;
begin
  {$IFDEF PAS2JS}
  if not isString(AText) then Fail('document must be a string');
  {$ENDIF}
  if Length(AText) > WFC_PATTERN_3D_MAX_ENCODED_TEXT_LENGTH then
    Fail('document exceeds the version-2 encoded length limit');
  Lines := 0;
  for I := 1 to Length(AText) do
  begin
    if Ord(AText[I]) > 127 then Fail('document must use canonical ASCII text');
    if AText[I] = #10 then
    begin
      if Lines = WFC_PATTERN_3D_MAX_TEXT_LINE_COUNT then
        Fail('document exceeds the version-2 line-count limit');
      Inc(Lines);
    end;
  end;
end;

function ReadValue(const ALines: TWfcTextLines; var AIndex: Integer;
  const APrefix: String): String;
begin
  if AIndex >= Length(ALines) then Fail(APrefix + ' is missing');
  Result := WfcTextValueAfterPrefix(ALines[AIndex], APrefix, APrefix,
    ARTIFACT_NAME);
  Inc(AIndex);
end;

procedure Exact(const ALines: TWfcTextLines; var AIndex: Integer;
  const AText: String);
begin
  if (AIndex >= Length(ALines)) or (ALines[AIndex] <> AText) then
    Fail('expected exactly ' + AText);
  Inc(AIndex);
end;

procedure FieldCount(const AText: String; const ACount: Integer;
  const AName: String);
var I, Count: Integer;
begin
  Count := 1;
  for I := 1 to Length(AText) do
    if AText[I] = ',' then
    begin
      Inc(Count);
      if Count > ACount then Fail(AName + ' has extra fields');
    end;
  if Count <> ACount then Fail(AName + ' has missing fields');
end;

function Field(const AText: String; var AStart: Integer): String;
var Finish: Integer;
begin
  Finish := WfcTextFindCharacter(AText, ',', AStart);
  if Finish = 0 then Finish := Length(AText) + 1;
  Result := Copy(AText, AStart, Finish - AStart);
  AStart := Finish + 1;
end;

function ParseBoundary(const AText: String): TWfcModelBoundary;
begin
  if AText = 'open' then Exit(wmbOpen);
  if AText = 'wrap' then Exit(wmbWrap);
  Fail('boundary has an unknown value');
  Result := wmbOpen;
end;

function ParseSymmetry(const AText: String): TWfcModelSymmetry;
begin
  if AText = 'none' then Exit(wmsNone);
  if AText = 'd4' then Exit(wmsD4);
  if AText = 'cube24' then Exit(wmsCubeRotations);
  if AText = 'cube48' then Exit(wmsCubeFull);
  Fail('symmetry has an unknown value');
  Result := wmsNone;
end;

function BoundaryName(const AValue: TWfcModelBoundary): String;
begin
  case AValue of
    wmbOpen: Result := 'open';
    wmbWrap: Result := 'wrap';
  else Fail('boundary has an unknown value'); end;
end;

function SymmetryName(const AValue: TWfcModelSymmetry): String;
begin
  case AValue of
    wmsNone: Result := 'none';
    wmsD4: Result := 'd4';
    wmsCubeRotations: Result := 'cube24';
    wmsCubeFull: Result := 'cube48';
  else Fail('symmetry has an unknown value'); end;
end;

function JoinFields(const AFields: TWfcTextLines): String;
var I, Total: Integer;
  {$IFNDEF PAS2JS}J, Position: Integer;{$ENDIF}
begin
  Total := 0;
  for I := 0 to High(AFields) do
  begin
    if Length(AFields[I]) > WFC_PATTERN_3D_MAX_ENCODED_TEXT_LENGTH - Total - 1 then
      Fail('pattern record exceeds the version-2 encoded length limit');
    Inc(Total, Length(AFields[I]) + 1);
  end;
  if Length(AFields) = 0 then Exit('');
  {$IFDEF PAS2JS}
  Result := TJSArray(AFields).join(',');
  {$ELSE}
  SetLength(Result, Total - 1);
  Position := 1;
  for I := 0 to High(AFields) do
  begin
    if I > 0 then begin Result[Position] := ','; Inc(Position); end;
    for J := 1 to Length(AFields[I]) do
    begin Result[Position] := AFields[I][J]; Inc(Position); end;
  end;
  {$ENDIF}
end;

procedure CheckEncodedTokenBudget(const AToken: TWfcModelToken;
  const ABudget: Integer);
var I, CodeUnit, Cost, Total: Integer;
  {$IFDEF PAS2JS}LowSurrogate: Integer;{$ENDIF}
begin
  { Count the canonical percent-encoded UTF-8 bytes without allocating an
    encoded copy. The immutable model has already validated token Unicode. }
  I := 1; Total := 0;
  while I <= Length(AToken) do
  begin
    CodeUnit := Ord(AToken[I]); Inc(I);
    if ((CodeUnit >= Ord('A')) and (CodeUnit <= Ord('Z'))) or
      ((CodeUnit >= Ord('a')) and (CodeUnit <= Ord('z'))) or
      ((CodeUnit >= Ord('0')) and (CodeUnit <= Ord('9'))) or
      (CodeUnit = Ord('-')) or (CodeUnit = Ord('.')) or
      (CodeUnit = Ord('_')) or (CodeUnit = Ord('~')) then Cost := 1
    else Cost := 3;
    {$IFDEF PAS2JS}
    if (CodeUnit >= $D800) and (CodeUnit <= $DBFF) then
    begin
      if I > Length(AToken) then Fail('token contains invalid UTF-16');
      LowSurrogate := Ord(AToken[I]); Inc(I);
      if (LowSurrogate < $DC00) or (LowSurrogate > $DFFF) then
        Fail('token contains invalid UTF-16');
      Cost := 12;
    end
    else if (CodeUnit >= $DC00) and (CodeUnit <= $DFFF) then
      Fail('token contains invalid UTF-16')
    else if CodeUnit > $7FF then Cost := 9
    else if CodeUnit > $7F then Cost := 6;
    {$ENDIF}
    if Cost > ABudget - Total then
      Fail('palette token exceeds the remaining encoded length limit');
    Inc(Total, Cost);
  end;
end;

function EncodeWfcPattern3DText(const AModel: TWfcOverlappingModel3D): String;
var Lines, Fields: TWfcTextLines; LineIndex, Total, I, J: Integer;
  Shape: TWfcModelSampleShape; Payload: TWfcPattern3DPayload;
  Token: TWfcModelToken; Prefix: String;

  procedure Add(const ALine: String);
  begin
    if Length(ALine) > WFC_PATTERN_3D_MAX_ENCODED_TEXT_LENGTH - Total - 1 then
      Fail('document exceeds the version-2 encoded length limit');
    Inc(Total, Length(ALine) + 1);
    Lines[LineIndex] := ALine;
    Inc(LineIndex);
  end;

begin
  if AModel = nil then raise EArgumentNilException.Create('volume pattern model is nil');
  LineIndex := FIXED_LINE_COUNT + AModel.SourceCount +
    AModel.PaletteCount + AModel.PatternCount;
  if LineIndex > WFC_PATTERN_3D_MAX_TEXT_LINE_COUNT then
    Fail('document exceeds the version-2 line-count limit');
  SetLength(Lines, LineIndex);
  LineIndex := 0; Total := 0;
  Add('wfcp=2'); Add('rank=3');
  Add('samples=' + IntToStr(AModel.SourceCount));
  for I := 0 to AModel.SourceCount - 1 do
  begin
    Shape := AModel.SourceShapeAt(I);
    Add('s=' + IntToStr(I) + ',' + IntToStr(Shape.Width) + ',' +
      IntToStr(Shape.Height) + ',' + IntToStr(Shape.Depth));
  end;
  Add('footprint=' + IntToStr(AModel.PatternWidth) + ',' +
    IntToStr(AModel.PatternHeight) + ',' + IntToStr(AModel.PatternDepth));
  Add('boundary=' + BoundaryName(AModel.SourceBoundary));
  Add('symmetry=' + SymmetryName(AModel.Symmetry));
  Add('directions=N,E,S,W,U,D');
  Add('palette=' + IntToStr(AModel.PaletteCount));
  for I := 0 to AModel.PaletteCount - 1 do
  begin
    Token := AModel.PaletteTokenAt(I);
    Prefix := 't=' + IntToStr(I) + ',';
    CheckEncodedTokenBudget(Token, WFC_PATTERN_3D_MAX_ENCODED_TEXT_LENGTH -
      Total - Length(Prefix) - 1);
    Add(Prefix + WfcTextEncodeToken(Token, ARTIFACT_NAME));
  end;
  Add('patterns=' + IntToStr(AModel.PatternCount));
  for I := 0 to AModel.PatternCount - 1 do
  begin
    Payload := AModel.CopyPattern(I);
    SetLength(Fields, Length(Payload) + 2);
    Fields[0] := 'p=' + IntToStr(I);
    Fields[1] := IntToStr(AModel.PatternWeightAt(I));
    for J := 0 to High(Payload) do Fields[J + 2] := IntToStr(Payload[J]);
    Add(JoinFields(Fields));
  end;
  Add('relations=overlap'); Add('end');
  Result := WfcTextJoinCanonicalLines(Lines, ARTIFACT_NAME);
end;

function DecodeWfcPattern3DText(const AText: String): TWfcOverlappingModel3D;
var Lines: TWfcTextLines; Index, I, J, Start, Count, Cells, TotalCells,
  Width, Height, Depth, Footprint, PaletteCount, PatternCount: Integer;
  Shapes: TWfcModelSampleShapes; Palette: TWfcModelTokens;
  Patterns: TWfcPattern3DPayloads; Weights: TWfcModelIntegerArray;
  Boundary: TWfcModelBoundary; Symmetry: TWfcModelSymmetry; S: String;
begin
  Result := nil;
  Envelope(AText);
  WfcTextSplitCanonicalLines(AText, ARTIFACT_NAME, Lines);
  Index := 0;
  Exact(Lines, Index, 'wfcp=2'); Exact(Lines, Index, 'rank=3');
  Count := Bounded(Number(ReadValue(Lines, Index, 'samples='), 'samples'),
    1, WFC_PATTERN_3D_MAX_SOURCE_COUNT, 'samples');
  if Count > Length(Lines) - Index - 10 then Fail('sample records are incomplete');
  SetLength(Shapes, Count); TotalCells := 0;
  for I := 0 to Count - 1 do
  begin
    S := ReadValue(Lines, Index, 's='); FieldCount(S, 4, 'sample'); Start := 1;
    if Number(Field(S, Start), 'sample index') <> I then
      Fail('sample indices must be complete and ordered');
    Shapes[I].Width := Number(Field(S, Start), 'sample width');
    Shapes[I].Height := Number(Field(S, Start), 'sample height');
    Shapes[I].Depth := Number(Field(S, Start), 'sample depth');
    Cells := Volume(Shapes[I].Width, Shapes[I].Height, Shapes[I].Depth,
      WFC_PATTERN_3D_MAX_SOURCE_DIMENSION, WFC_PATTERN_3D_MAX_SOURCE_CELL_COUNT,
      'sample');
    if TotalCells > WFC_PATTERN_3D_MAX_TOTAL_SOURCE_CELL_COUNT - Cells then
      Fail('aggregate sample cells exceed the supported limit');
    Inc(TotalCells, Cells);
  end;
  S := ReadValue(Lines, Index, 'footprint='); FieldCount(S, 3, 'footprint'); Start := 1;
  Width := Number(Field(S, Start), 'footprint width');
  Height := Number(Field(S, Start), 'footprint height');
  Depth := Number(Field(S, Start), 'footprint depth');
  Footprint := Volume(Width, Height, Depth, WFC_PATTERN_3D_MAX_FOOTPRINT_DIMENSION,
    WFC_PATTERN_3D_MAX_FOOTPRINT_CELL_COUNT, 'footprint');
  Boundary := ParseBoundary(ReadValue(Lines, Index, 'boundary='));
  Symmetry := ParseSymmetry(ReadValue(Lines, Index, 'symmetry='));
  if Boundary = wmbOpen then
    for I := 0 to High(Shapes) do
      if (Shapes[I].Width < Width) or (Shapes[I].Height < Height) or
          (Shapes[I].Depth < Depth) then Fail('open sample is smaller than footprint');
  if (Symmetry = wmsD4) and (Width <> Height) then
    Fail('D4 requires an XY-square footprint');
  if (Symmetry in [wmsCubeRotations, wmsCubeFull]) and
      ((Width <> Height) or (Width <> Depth)) then
    Fail('cube symmetry requires a cubic footprint');
  Exact(Lines, Index, 'directions=N,E,S,W,U,D');
  PaletteCount := Bounded(Number(ReadValue(Lines, Index, 'palette='), 'palette'),
    1, WFC_PATTERN_3D_MAX_PALETTE_COUNT, 'palette');
  if PaletteCount > Length(Lines) - Index - 4 then Fail('palette records are incomplete');
  SetLength(Palette, PaletteCount);
  for I := 0 to PaletteCount - 1 do
  begin
    S := ReadValue(Lines, Index, 't='); FieldCount(S, 2, 'palette'); Start := 1;
    if Number(Field(S, Start), 'palette index') <> I then
      Fail('palette indices must be complete and ordered');
    Palette[I] := WfcTextDecodeToken(Field(S, Start), ARTIFACT_NAME);
  end;
  PatternCount := Bounded(Number(ReadValue(Lines, Index, 'patterns='), 'patterns'),
    1, WFC_PATTERN_3D_MAX_PATTERN_COUNT, 'patterns');
  if PatternCount > Length(Lines) - Index - 2 then Fail('pattern records are incomplete');
  Product(PatternCount, Footprint, WFC_PATTERN_3D_MAX_TOTAL_PATTERN_CELL_COUNT,
    'aggregate pattern');
  Product(Product(PatternCount, PatternCount, WFC_PATTERN_3D_MAX_RELATION_SLOT_COUNT,
    'relation table'), 6, WFC_PATTERN_3D_MAX_RELATION_SLOT_COUNT, 'relation table');
  SetLength(Patterns, PatternCount); SetLength(Weights, PatternCount);
  for I := 0 to PatternCount - 1 do
  begin
    S := ReadValue(Lines, Index, 'p=');
    { Verify the exact cell count before allocating a declared payload. }
    FieldCount(S, Footprint + 2, 'pattern'); Start := 1;
    if Number(Field(S, Start), 'pattern index') <> I then
      Fail('pattern indices must be complete and ordered');
    Weights[I] := Bounded(Number(Field(S, Start), 'pattern weight'), 1,
      High(Integer), 'pattern weight');
    SetLength(Patterns[I], Footprint);
    for J := 0 to Footprint - 1 do
      Patterns[I][J] := Bounded(Number(Field(S, Start), 'palette index'),
        0, PaletteCount - 1, 'pattern palette index');
  end;
  Exact(Lines, Index, 'relations=overlap'); Exact(Lines, Index, 'end');
  if Index <> Length(Lines) then Fail('trailing data is not permitted');
  try
    { The immutable model independently validates raw observation totals,
      duplicate payloads/palette, symmetry closure and all six derived planes. }
    Result := TWfcOverlappingModel3D.Create(Width, Height, Depth, Boundary,
      Symmetry, Shapes, Palette, Patterns, Weights);
  except
    on E: EWfcModel do Fail(E.Message);
  end;
  try
    if EncodeWfcPattern3DText(Result) <> AText then Fail('document is not canonical');
  except
    Result.Free; Result := nil; raise;
  end;
end;

end.
