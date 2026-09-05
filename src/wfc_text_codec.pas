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
unit wfc_text_codec;

{$mode delphi}{$H+}

interface

uses
  wfc_model;

type
  TWfcTextLines = array of String;

procedure WfcTextError(const AArtifactName, AMessage: String);
function WfcTextParseCanonicalInteger(const AText, AFieldName,
  AArtifactName: String): Integer;
function WfcTextParseCanonicalCardinal(const AText, AFieldName,
  AArtifactName: String): Cardinal;
function WfcTextParseCanonicalSignedInteger(const AText, AFieldName,
  AArtifactName: String): Integer;
function WfcTextValueAfterPrefix(const ALine, APrefix, AFieldName,
  AArtifactName: String): String;
function WfcTextEncodeToken(const AToken: TWfcModelToken;
  const AArtifactName: String): String;
function WfcTextDecodeToken(const AText,
  AArtifactName: String): TWfcModelToken;
function WfcTextJoinCanonicalLines(const ALines: TWfcTextLines;
  const AArtifactName: String): String;
procedure WfcTextSplitCanonicalLines(const AText, AArtifactName: String;
  out ALines: TWfcTextLines);
function WfcTextFindCharacter(const AText: String;
  const ACharacter: Char; const AStart: Integer): Integer;

implementation

uses
  SysUtils
  {$IFDEF PAS2JS}, JS{$ENDIF};

type
  TWfcTextBytes = array of Byte;

const
  WFC_TEXT_HEX = '0123456789ABCDEF';

procedure WfcTextError(const AArtifactName, AMessage: String);
begin
  raise EConvertError.Create('invalid ' + AArtifactName +
    ' text: ' + AMessage);
end;

function WfcTextParseCanonicalInteger(const AText, AFieldName,
  AArtifactName: String): Integer;
var
  I: Integer;
  LDigit: Integer;
begin
  if AText = '' then
    WfcTextError(AArtifactName, AFieldName + ' is empty');
  if (Length(AText) > 1) and (AText[1] = '0') then
    WfcTextError(AArtifactName, AFieldName + ' has a leading zero');

  Result := 0;
  for I := 1 to Length(AText) do
  begin
    if not (AText[I] in ['0'..'9']) then
      WfcTextError(AArtifactName,
        AFieldName + ' is not a canonical decimal integer');
    LDigit := Ord(AText[I]) - Ord('0');
    if Result > ((High(Integer) - LDigit) div 10) then
      WfcTextError(AArtifactName,
        AFieldName + ' exceeds the supported integer range');
    Result := (Result * 10) + LDigit;
  end;
end;

function WfcTextParseCanonicalCardinal(const AText, AFieldName,
  AArtifactName: String): Cardinal;
var
  I: Integer;
  LDigit: Cardinal;
begin
  if AText = '' then
    WfcTextError(AArtifactName, AFieldName + ' is empty');
  if (Length(AText) > 1) and (AText[1] = '0') then
    WfcTextError(AArtifactName, AFieldName + ' has a leading zero');

  Result := 0;
  for I := 1 to Length(AText) do
  begin
    if not (AText[I] in ['0'..'9']) then
      WfcTextError(AArtifactName,
        AFieldName + ' is not a canonical unsigned decimal integer');
    LDigit := Cardinal(Ord(AText[I]) - Ord('0'));
    if Result > ((High(Cardinal) - LDigit) div Cardinal(10)) then
      WfcTextError(AArtifactName,
        AFieldName + ' exceeds the supported Cardinal range');
    Result := (Result * Cardinal(10)) + LDigit;
  end;
end;

function WfcTextParseCanonicalSignedInteger(const AText, AFieldName,
  AArtifactName: String): Integer;
var
  I: Integer;
  LDigit: Cardinal;
  LFirstDigit: Integer;
  LLimit: Cardinal;
  LMagnitude: Cardinal;
  LNegative: Boolean;
begin
  if AText = '' then
    WfcTextError(AArtifactName, AFieldName + ' is empty');

  LNegative := AText[1] = '-';
  if LNegative then
    LFirstDigit := 2
  else
    LFirstDigit := 1;
  if LFirstDigit > Length(AText) then
    WfcTextError(AArtifactName,
      AFieldName + ' is not a canonical signed decimal integer');
  if (Length(AText) - LFirstDigit >= 1) and
      (AText[LFirstDigit] = '0') then
    WfcTextError(AArtifactName, AFieldName + ' has a leading zero');
  if LNegative and (AText[LFirstDigit] = '0') then
    WfcTextError(AArtifactName, AFieldName + ' is negative zero');

  if LNegative then
    LLimit := Cardinal(High(Integer)) + Cardinal(1)
  else
    LLimit := Cardinal(High(Integer));
  LMagnitude := 0;
  for I := LFirstDigit to Length(AText) do
  begin
    if not (AText[I] in ['0'..'9']) then
      WfcTextError(AArtifactName,
        AFieldName + ' is not a canonical signed decimal integer');
    LDigit := Cardinal(Ord(AText[I]) - Ord('0'));
    if LMagnitude > ((LLimit - LDigit) div Cardinal(10)) then
      WfcTextError(AArtifactName,
        AFieldName + ' exceeds the supported Integer range');
    LMagnitude := (LMagnitude * Cardinal(10)) + LDigit;
  end;

  if not LNegative then
    Result := Integer(LMagnitude)
  else if LMagnitude = Cardinal(High(Integer)) + Cardinal(1) then
    Result := Low(Integer)
  else
    Result := -Integer(LMagnitude);
end;

function WfcTextValueAfterPrefix(const ALine, APrefix, AFieldName,
  AArtifactName: String): String;
begin
  if Copy(ALine, 1, Length(APrefix)) <> APrefix then
    WfcTextError(AArtifactName, 'expected ' + AFieldName);
  Result := Copy(ALine, Length(APrefix) + 1,
    Length(ALine) - Length(APrefix));
end;

function IsUnescapedTokenByte(const AValue: Byte): Boolean;
begin
  Result := ((AValue >= Ord('A')) and (AValue <= Ord('Z'))) or
    ((AValue >= Ord('a')) and (AValue <= Ord('z'))) or
    ((AValue >= Ord('0')) and (AValue <= Ord('9'))) or
    (AValue = Ord('-')) or (AValue = Ord('.')) or
    (AValue = Ord('_')) or (AValue = Ord('~'));
end;

function UpperHexValue(const ACharacter: Char): Integer;
begin
  if ACharacter in ['0'..'9'] then
    Result := Ord(ACharacter) - Ord('0')
  else if ACharacter in ['A'..'F'] then
    Result := Ord(ACharacter) - Ord('A') + 10
  else
    Result := -1;
end;

procedure AppendUtf8Byte(var ABytes: TWfcTextBytes; var ACount: Integer;
  const AValue: Integer);
begin
  ABytes[ACount] := Byte(AValue);
  Inc(ACount);
end;

function TokenToUtf8Bytes(const AToken: TWfcModelToken;
  const AArtifactName: String): TWfcTextBytes;
{$IFDEF PAS2JS}
var
  I: Integer;
  LCount: Integer;
  LCodePoint: Integer;
  LCodeUnit: Integer;
  LLowSurrogate: Integer;
{$ELSE}
var
  I: Integer;
{$ENDIF}
begin
  Result := nil;
  {$IFDEF PAS2JS}
  if Length(AToken) > (High(Integer) div 4) then
    raise ERangeError.Create(AArtifactName + ' token is too large');
  SetLength(Result, Length(AToken) * 4);
  LCount := 0;
  I := 1;
  while I <= Length(AToken) do
  begin
    LCodeUnit := Ord(AToken[I]);
    Inc(I);
    if (LCodeUnit >= $D800) and (LCodeUnit <= $DBFF) then
    begin
      if I > Length(AToken) then
        raise EConvertError.Create(AArtifactName +
          ' token contains invalid UTF-16');
      LLowSurrogate := Ord(AToken[I]);
      if (LLowSurrogate < $DC00) or (LLowSurrogate > $DFFF) then
        raise EConvertError.Create(AArtifactName +
          ' token contains invalid UTF-16');
      Inc(I);
      LCodePoint := $10000 + ((LCodeUnit - $D800) shl 10) +
        (LLowSurrogate - $DC00);
    end
    else
    begin
      if (LCodeUnit >= $DC00) and (LCodeUnit <= $DFFF) then
        raise EConvertError.Create(AArtifactName +
          ' token contains invalid UTF-16');
      LCodePoint := LCodeUnit;
    end;

    if LCodePoint <= $7F then
      AppendUtf8Byte(Result, LCount, LCodePoint)
    else if LCodePoint <= $7FF then
    begin
      AppendUtf8Byte(Result, LCount, $C0 or (LCodePoint shr 6));
      AppendUtf8Byte(Result, LCount, $80 or (LCodePoint and $3F));
    end
    else if LCodePoint <= $FFFF then
    begin
      AppendUtf8Byte(Result, LCount, $E0 or (LCodePoint shr 12));
      AppendUtf8Byte(Result, LCount,
        $80 or ((LCodePoint shr 6) and $3F));
      AppendUtf8Byte(Result, LCount, $80 or (LCodePoint and $3F));
    end
    else
    begin
      AppendUtf8Byte(Result, LCount, $F0 or (LCodePoint shr 18));
      AppendUtf8Byte(Result, LCount,
        $80 or ((LCodePoint shr 12) and $3F));
      AppendUtf8Byte(Result, LCount,
        $80 or ((LCodePoint shr 6) and $3F));
      AppendUtf8Byte(Result, LCount, $80 or (LCodePoint and $3F));
    end;
  end;
  SetLength(Result, LCount);
  {$ELSE}
  SetLength(Result, Length(AToken));
  for I := 1 to Length(AToken) do
    Result[I - 1] := Ord(AToken[I]);
  {$ENDIF}
end;

function Utf8BytesToToken(const ABytes: TWfcTextBytes;
  const AArtifactName: String): TWfcModelToken;
var
  I: Integer;
  J: Integer;
  LCodePoint: Integer;
  LContinuationCount: Integer;
  LMinimumCodePoint: Integer;
begin
  Result := '';
  I := 0;
  while I < Length(ABytes) do
  begin
    LCodePoint := ABytes[I];
    Inc(I);
    if LCodePoint <= $7F then
    begin
      LContinuationCount := 0;
      LMinimumCodePoint := 0;
    end
    else if (LCodePoint >= $C2) and (LCodePoint <= $DF) then
    begin
      LCodePoint := LCodePoint and $1F;
      LContinuationCount := 1;
      LMinimumCodePoint := $80;
    end
    else if (LCodePoint >= $E0) and (LCodePoint <= $EF) then
    begin
      LCodePoint := LCodePoint and $0F;
      LContinuationCount := 2;
      LMinimumCodePoint := $800;
    end
    else if (LCodePoint >= $F0) and (LCodePoint <= $F4) then
    begin
      LCodePoint := LCodePoint and $07;
      LContinuationCount := 3;
      LMinimumCodePoint := $10000;
    end
    else
      WfcTextError(AArtifactName, 'token contains invalid UTF-8');

    if LContinuationCount > (Length(ABytes) - I) then
      WfcTextError(AArtifactName, 'token contains truncated UTF-8');
    for J := 1 to LContinuationCount do
    begin
      if (ABytes[I] < $80) or (ABytes[I] > $BF) then
        WfcTextError(AArtifactName,
          'token contains invalid UTF-8 continuation bytes');
      LCodePoint := (LCodePoint shl 6) or (ABytes[I] and $3F);
      Inc(I);
    end;

    if (LCodePoint < LMinimumCodePoint) or (LCodePoint > $10FFFF) or
      ((LCodePoint >= $D800) and (LCodePoint <= $DFFF)) then
      WfcTextError(AArtifactName,
        'token contains a noncanonical UTF-8 scalar value');

    {$IFDEF PAS2JS}
    if LCodePoint <= $FFFF then
      Result := Result + TWfcModelToken(Chr(LCodePoint))
    else
    begin
      Dec(LCodePoint, $10000);
      Result := Result + TWfcModelToken(Chr($D800 +
        (LCodePoint shr 10)));
      Result := Result + TWfcModelToken(Chr($DC00 +
        (LCodePoint and $3FF)));
    end;
    {$ENDIF}
  end;

  {$IFNDEF PAS2JS}
  SetLength(Result, Length(ABytes));
  for I := 0 to High(ABytes) do
    Result[I + 1] := AnsiChar(ABytes[I]);
  {$ENDIF}
end;

function WfcTextEncodeToken(const AToken: TWfcModelToken;
  const AArtifactName: String): String;
var
  LBytes: TWfcTextBytes;
  I: Integer;
  LLength: Integer;
  {$IFDEF PAS2JS}
  LParts: array of String;
  {$ELSE}
  LPosition: Integer;
  {$ENDIF}
begin
  LBytes := TokenToUtf8Bytes(AToken, AArtifactName);

  { Validate native UTF8String values as well as decoder output. }
  Utf8BytesToToken(LBytes, AArtifactName);

  LLength := 0;
  for I := 0 to High(LBytes) do
  begin
    if IsUnescapedTokenByte(LBytes[I]) then
    begin
      if LLength = High(Integer) then
        raise ERangeError.Create('encoded ' + AArtifactName +
          ' token is too large');
      Inc(LLength);
    end
    else
    begin
      if LLength > (High(Integer) - 3) then
        raise ERangeError.Create('encoded ' + AArtifactName +
          ' token is too large');
      Inc(LLength, 3);
    end;
  end;

  {$IFDEF PAS2JS}
  { Browser strings are immutable. Indexed character writes become full-string
    slices in the generated RTL, so they are quadratic for large tokens.
    All output bounds above are checked before allocating these bounded parts;
    each input byte contributes exactly one one- or three-character part. }
  SetLength(LParts, Length(LBytes));
  for I := 0 to High(LBytes) do
    if IsUnescapedTokenByte(LBytes[I]) then LParts[I] := Chr(LBytes[I])
    else LParts[I] := '%' + WFC_TEXT_HEX[(LBytes[I] shr 4) + 1] +
      WFC_TEXT_HEX[(LBytes[I] and $F) + 1];
  Result := TJSArray(LParts).join('');
  {$ELSE}
  SetLength(Result, LLength);
  LPosition := 1;
  for I := 0 to High(LBytes) do
  begin
    if IsUnescapedTokenByte(LBytes[I]) then
    begin
      Result[LPosition] := Chr(LBytes[I]);
      Inc(LPosition);
    end
    else
    begin
      Result[LPosition] := '%';
      Result[LPosition + 1] := WFC_TEXT_HEX[(LBytes[I] shr 4) + 1];
      Result[LPosition + 2] := WFC_TEXT_HEX[(LBytes[I] and $F) + 1];
      Inc(LPosition, 3);
    end;
  end;
  {$ENDIF}
end;

function WfcTextDecodeToken(const AText,
  AArtifactName: String): TWfcModelToken;
var
  LBytes: TWfcTextBytes;
  LByteCount: Integer;
  I: Integer;
  LHighNibble: Integer;
  LLowNibble: Integer;
  LValue: Integer;
begin
  SetLength(LBytes, Length(AText));
  LByteCount := 0;
  I := 1;
  while I <= Length(AText) do
  begin
    LValue := Ord(AText[I]);
    if IsUnescapedTokenByte(Byte(LValue and $FF)) and
      (LValue <= $7F) then
    begin
      LBytes[LByteCount] := Byte(LValue);
      Inc(LByteCount);
      Inc(I);
    end
    else if AText[I] = '%' then
    begin
      if I > (Length(AText) - 2) then
        WfcTextError(AArtifactName,
          'token has a truncated percent escape');
      LHighNibble := UpperHexValue(AText[I + 1]);
      LLowNibble := UpperHexValue(AText[I + 2]);
      if (LHighNibble < 0) or (LLowNibble < 0) then
        WfcTextError(AArtifactName,
          'token percent escapes must use uppercase hexadecimal');
      LValue := (LHighNibble shl 4) or LLowNibble;
      if IsUnescapedTokenByte(Byte(LValue)) then
        WfcTextError(AArtifactName,
          'token unnecessarily escapes an unreserved byte');
      LBytes[LByteCount] := Byte(LValue);
      Inc(LByteCount);
      Inc(I, 3);
    end
    else
      WfcTextError(AArtifactName,
        'token contains a noncanonical unescaped character');
  end;
  SetLength(LBytes, LByteCount);
  Result := Utf8BytesToToken(LBytes, AArtifactName);
end;

function WfcTextJoinCanonicalLines(const ALines: TWfcTextLines;
  const AArtifactName: String): String;
var
  I: Integer;
  {$IFNDEF PAS2JS}
  J: Integer;
  LPosition: Integer;
  {$ENDIF}
  LLength: Integer;
begin
  LLength := 0;
  for I := 0 to High(ALines) do
  begin
    if Length(ALines[I]) > (High(Integer) - LLength - 1) then
      raise ERangeError.Create('encoded ' + AArtifactName +
        ' text is too large');
    Inc(LLength, Length(ALines[I]) + 1);
  end;

  {$IFDEF PAS2JS}
  { Array.join copies the complete lines once; it does not mutate the borrowed
    line vector. Keep the existing empty-vector and final-LF behavior. }
  if Length(ALines) = 0 then Result := ''
  else Result := TJSArray(ALines).join(#10) + #10;
  {$ELSE}
  SetLength(Result, LLength);
  LPosition := 1;
  for I := 0 to High(ALines) do
  begin
    for J := 1 to Length(ALines[I]) do
    begin
      Result[LPosition] := ALines[I][J];
      Inc(LPosition);
    end;
    Result[LPosition] := #10;
    Inc(LPosition);
  end;
  {$ENDIF}
end;

procedure WfcTextSplitCanonicalLines(const AText, AArtifactName: String;
  out ALines: TWfcTextLines);
var
  I: Integer;
  LLineCount: Integer;
  LLineIndex: Integer;
  LStart: Integer;
begin
  if AText = '' then
    WfcTextError(AArtifactName, 'document is empty');
  if AText[Length(AText)] <> #10 then
    WfcTextError(AArtifactName, 'document must end with LF');

  LLineCount := 0;
  for I := 1 to Length(AText) do
  begin
    if AText[I] = #13 then
      WfcTextError(AArtifactName, 'CR is not permitted');
    if AText[I] = #10 then
      Inc(LLineCount);
  end;

  SetLength(ALines, LLineCount);
  LLineIndex := 0;
  LStart := 1;
  for I := 1 to Length(AText) do
    if AText[I] = #10 then
    begin
      ALines[LLineIndex] := Copy(AText, LStart, I - LStart);
      if ALines[LLineIndex] = '' then
        WfcTextError(AArtifactName, 'blank lines are not permitted');
      Inc(LLineIndex);
      LStart := I + 1;
    end;
end;

function WfcTextFindCharacter(const AText: String;
  const ACharacter: Char; const AStart: Integer): Integer;
begin
  Result := AStart;
  while (Result <= Length(AText)) and
    (AText[Result] <> ACharacter) do
    Inc(Result);
  if Result > Length(AText) then
    Result := 0;
end;

end.
