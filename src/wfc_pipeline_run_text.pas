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
unit wfc_pipeline_run_text;

{$mode delphi}{$H+}

interface

uses
  wfc_pipeline_model,
  wfc_pipeline_run;

const
  WFC_PIPELINE_RUN_TEXT_VERSION = 1;
  WFC_PIPELINE_RUN_MAX_ENCODED_TEXT_LENGTH = 67108864;
  WFC_PIPELINE_RUN_MAX_TEXT_LINE_COUNT = 14 +
    WFC_PIPELINE_RUN_MAX_LOCK_COUNT + WFC_PIPELINE_RUN_MAX_DOMAIN_COUNT +
    WFC_PIPELINE_RUN_MAX_TOTAL_DOMAIN_TOKEN_COUNT;

function EncodeWfcPipelineRunText(const ARun: TWfcPipelineRun): String;
function DecodeWfcPipelineRunText(const AText: String;
  const ARecipe: TWfcPipelineModel): TWfcPipelineRun;

implementation

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_text_codec;

const
  WFC_PIPELINE_RUN_TEXT_ARTIFACT = 'WFC pipeline run';
  WFC_PIPELINE_RUN_FIXED_LINE_COUNT = 14;

type
  TWfcPipelineRunTextFields = array of String;

procedure TextError(const AMessage: String);
begin
  WfcTextError(WFC_PIPELINE_RUN_TEXT_ARTIFACT, AMessage);
end;

function ParseCanonicalInteger(const AText,
  AFieldName: String): Integer;
begin
  Result := WfcTextParseCanonicalInteger(AText, AFieldName,
    WFC_PIPELINE_RUN_TEXT_ARTIFACT);
end;

function ParseCanonicalCardinal(const AText,
  AFieldName: String): Cardinal;
begin
  Result := WfcTextParseCanonicalCardinal(AText, AFieldName,
    WFC_PIPELINE_RUN_TEXT_ARTIFACT);
end;

function ValueAfterPrefix(const ALine, APrefix,
  AFieldName: String): String;
begin
  Result := WfcTextValueAfterPrefix(ALine, APrefix, AFieldName,
    WFC_PIPELINE_RUN_TEXT_ARTIFACT);
end;

function RequireLine(const ALines: TWfcTextLines;
  const AIndex: Integer; const AFieldName: String): String;
begin
  if (AIndex < 0) or (AIndex >= Length(ALines)) then
    TextError(AFieldName + ' is missing');
  Result := ALines[AIndex];
end;

function ReadValueLine(const ALines: TWfcTextLines;
  var ALineIndex: Integer; const APrefix,
  AFieldName: String): String;
begin
  Result := ValueAfterPrefix(RequireLine(ALines, ALineIndex,
    AFieldName), APrefix, AFieldName);
  Inc(ALineIndex);
end;

procedure AppendLine(var ALines: TWfcTextLines;
  var ALineCount: Integer; const ALine: String);
begin
  if (ALineCount < 0) or (ALineCount >= Length(ALines)) then
    raise ERangeError.Create(
      'canonical WFC pipeline run line capacity was exceeded');
  ALines[ALineCount] := ALine;
  Inc(ALineCount);
end;

procedure AddLineCapacity(var ACount: Integer;
  const AAdditional: Integer);
begin
  if AAdditional < 0 then
    raise EArgumentOutOfRangeException.Create(
      'canonical WFC pipeline run line addition cannot be negative');
  if ACount > WFC_PIPELINE_RUN_MAX_TEXT_LINE_COUNT - AAdditional then
    raise ERangeError.Create(
      'canonical WFC pipeline run text has too many lines');
  Inc(ACount, AAdditional);
end;

procedure RequireEncodedTextLength(const ALines: TWfcTextLines);
var
  I: Integer;
  LLineLength: SizeInt;
  LTotalLength: Integer;
begin
  LTotalLength := 0;
  for I := 0 to Length(ALines) - 1 do
  begin
    LLineLength := Length(ALines[I]);
    if LLineLength > SizeInt(
        WFC_PIPELINE_RUN_MAX_ENCODED_TEXT_LENGTH - LTotalLength - 1) then
      raise ERangeError.Create(
        'canonical WFC pipeline run text exceeds the version-1 length limit');
    Inc(LTotalLength, Integer(LLineLength) + 1);
  end;
end;

procedure PreflightTextEnvelope(const AText: String);
var
  I: SizeInt;
  LLineCount: Integer;
  LTextLength: SizeInt;
begin
  LTextLength := Length(AText);
  if (LTextLength < 0) or
      (LTextLength > SizeInt(
        WFC_PIPELINE_RUN_MAX_ENCODED_TEXT_LENGTH)) then
    TextError('document exceeds the version-1 encoded length limit');
  LLineCount := 0;
  for I := 1 to LTextLength do
    if AText[I] = #10 then
    begin
      if LLineCount = WFC_PIPELINE_RUN_MAX_TEXT_LINE_COUNT then
        TextError('document exceeds the version-1 line-count limit');
      Inc(LLineCount);
    end;
end;

function SplitRecord(const AText: String;
  const AFieldCount: Integer; const ARecordName: String):
  TWfcPipelineRunTextFields;
var
  I: Integer;
  LComma: Integer;
  LStart: Integer;
begin
  Result := nil;
  if AFieldCount < 1 then
    raise EArgumentOutOfRangeException.Create(
      'pipeline run record field count must be positive');
  SetLength(Result, AFieldCount);
  LStart := 1;
  for I := 0 to AFieldCount - 2 do
  begin
    LComma := WfcTextFindCharacter(AText, ',', LStart);
    if LComma = 0 then
      TextError(ARecordName + ' record is missing fields');
    Result[I] := Copy(AText, LStart, LComma - LStart);
    LStart := LComma + 1;
  end;
  if WfcTextFindCharacter(AText, ',', LStart) <> 0 then
    TextError(ARecordName + ' record has extra fields');
  Result[AFieldCount - 1] := Copy(AText, LStart,
    Length(AText) - LStart + 1);
end;

function ParseBoundedCount(const AText, AFieldName: String;
  const AMaximum: Integer): Integer;
begin
  Result := ParseCanonicalInteger(AText, AFieldName);
  if Result > AMaximum then
    TextError(AFieldName + ' exceeds the version-1 limit');
end;

procedure RequireRecordCapacity(const ACount, AMinimumTail,
  ALineIndex: Integer; const ALines: TWfcTextLines;
  const ARecordName: String);
begin
  if ACount < 0 then
    TextError(ARecordName + ' count cannot be negative');
  if AMinimumTail < 0 then
    raise EArgumentOutOfRangeException.Create(
      'pipeline run record tail cannot be negative');
  if ALineIndex > Length(ALines) - AMinimumTail then
    TextError(ARecordName + ' records are incomplete');
  if ACount > Length(ALines) - ALineIndex - AMinimumTail then
    TextError(ARecordName + ' records are incomplete');
end;

function SignatureTextIsCanonical(const AText: String): Boolean;
const
  HEX_DIGITS = '0123456789ABCDEF';
var
  I: Integer;
begin
  if Length(AText) <> 8 then
    Exit(False);
  for I := 1 to Length(AText) do
    if Pos(AText[I], HEX_DIGITS) = 0 then
      Exit(False);
  Result := True;
end;

function BooleanName(const AValue: Boolean): String;
begin
  if AValue then
    Result := 'true'
  else
    Result := 'false';
end;

function ParseBooleanName(const AText,
  AFieldName: String): Boolean;
begin
  if AText = 'false' then
    Result := False
  else if AText = 'true' then
    Result := True
  else
    TextError(AFieldName + ' must be false or true');
end;

function StrategyName(const AValue: TWfcPipelineSolveStrategy): String;
begin
  if (Ord(AValue) < Ord(Low(TWfcPipelineSolveStrategy))) or
      (Ord(AValue) > Ord(High(TWfcPipelineSolveStrategy))) then
    raise ERangeError.Create('unknown WFC pipeline run strategy');
  case AValue of
    wpssOneWay:
      Result := 'one-way';
    wpssNegotiated:
      Result := 'negotiated';
  end;
end;

function ParseStrategy(const AText: String): TWfcPipelineSolveStrategy;
begin
  if AText = 'one-way' then
    Result := wpssOneWay
  else if AText = 'negotiated' then
    Result := wpssNegotiated
  else
    TextError('strategy has an unknown value');
end;

function DecodeOuterToken(const AValue,
  AFieldName: String; var ATotalLength: Integer): TWfcModelToken;
var
  LEncodedLength: SizeInt;
begin
  LEncodedLength := Length(AValue);
  if (LEncodedLength < 0) or
      (LEncodedLength > SizeInt(
        WFC_PIPELINE_RUN_MAX_ENCODED_TOKEN_LENGTH)) then
    TextError(AFieldName +
      ' exceeds the version-1 encoded token-length limit');
  if LEncodedLength > SizeInt(
      WFC_PIPELINE_RUN_MAX_TOTAL_ENCODED_TOKEN_LENGTH -
      ATotalLength) then
    TextError('aggregate encoded token length exceeds the version-1 limit');
  Inc(ATotalLength, Integer(LEncodedLength));
  Result := WfcTextDecodeToken(AValue,
    WFC_PIPELINE_RUN_TEXT_ARTIFACT);
end;

function EncodeWfcPipelineRunText(const ARun: TWfcPipelineRun): String;
var
  I: Integer;
  J: Integer;
  LCount: Integer;
  LDomain: TWfcPipelineCellDomain;
  LExpectedLineCount: Integer;
  LLines: TWfcTextLines;
  LLock: TWfcPipelineCellLock;
begin
  if not Assigned(ARun) then
    raise EArgumentNilException.Create('WFC pipeline run cannot be nil');
  LExpectedLineCount := WFC_PIPELINE_RUN_FIXED_LINE_COUNT;
  AddLineCapacity(LExpectedLineCount, ARun.LockCount);
  AddLineCapacity(LExpectedLineCount, ARun.DomainCount);
  for I := 0 to ARun.DomainCount - 1 do
  begin
    LDomain := ARun.DomainAt(I);
    AddLineCapacity(LExpectedLineCount,
      Length(LDomain.AllowedTokens));
  end;
  SetLength(LLines, LExpectedLineCount);
  LCount := 0;
  AppendLine(LLines, LCount, 'wfcpipeline-run=' +
    IntToStr(WFC_PIPELINE_RUN_TEXT_VERSION));
  AppendLine(LLines, LCount, 'recipe-signature=' +
    WfcPipelineSignatureHex(ARun.RecipeSignature));
  AppendLine(LLines, LCount, 'width=' + IntToStr(ARun.Width));
  AppendLine(LLines, LCount, 'height=' + IntToStr(ARun.Height));
  AppendLine(LLines, LCount, 'depth=' + IntToStr(ARun.Depth));
  AppendLine(LLines, LCount, 'seed=' + UIntToStr(ARun.Seed));
  AppendLine(LLines, LCount, 'strategy=' + StrategyName(ARun.Strategy));
  AppendLine(LLines, LCount, 'max-backtracks=' +
    IntToStr(ARun.MaxBacktracks));
  AppendLine(LLines, LCount, 'max-pass-backtracks=' +
    IntToStr(ARun.MaxPassBacktracks));
  AppendLine(LLines, LCount, 'trace=' + BooleanName(ARun.CaptureTrace));
  AppendLine(LLines, LCount, 'locks=' + IntToStr(ARun.LockCount));
  for I := 0 to ARun.LockCount - 1 do
  begin
    LLock := ARun.LockAt(I);
    AppendLine(LLines, LCount, 'lock=' + IntToStr(I) + ',' +
      IntToStr(LLock.PassIndex) + ',' + IntToStr(LLock.X) + ',' +
      IntToStr(LLock.Y) + ',' + IntToStr(LLock.Z) + ',' +
      WfcTextEncodeToken(LLock.Token,
        WFC_PIPELINE_RUN_TEXT_ARTIFACT));
  end;
  AppendLine(LLines, LCount, 'domains=' + IntToStr(ARun.DomainCount));
  for I := 0 to ARun.DomainCount - 1 do
  begin
    LDomain := ARun.DomainAt(I);
    AppendLine(LLines, LCount, 'domain=' + IntToStr(I) + ',' +
      IntToStr(LDomain.PassIndex) + ',' + IntToStr(LDomain.X) + ',' +
      IntToStr(LDomain.Y) + ',' + IntToStr(LDomain.Z) + ',' +
      IntToStr(Length(LDomain.AllowedTokens)));
    for J := 0 to Length(LDomain.AllowedTokens) - 1 do
      AppendLine(LLines, LCount, 'allowed=' + IntToStr(I) + ',' +
        IntToStr(J) + ',' + WfcTextEncodeToken(
          LDomain.AllowedTokens[J], WFC_PIPELINE_RUN_TEXT_ARTIFACT));
  end;
  AppendLine(LLines, LCount, 'signature=' +
    WfcPipelineRunSignatureHex(ARun.Signature));
  AppendLine(LLines, LCount, 'end');
  if LCount <> LExpectedLineCount then
    raise Exception.Create(
      'canonical WFC pipeline run line count does not match its preflight');
  RequireEncodedTextLength(LLines);
  Result := WfcTextJoinCanonicalLines(LLines,
    WFC_PIPELINE_RUN_TEXT_ARTIFACT);
end;

function DecodeWfcPipelineRunText(const AText: String;
  const ARecipe: TWfcPipelineModel): TWfcPipelineRun;
var
  I: Integer;
  J: Integer;
  LAllowedCount: Integer;
  LAllowedTokens: TWfcModelTokens;
  LCaptureTrace: Boolean;
  LDepth: Integer;
  LDomainCount: Integer;
  LDomainPassIndex: Integer;
  LDomainX: Integer;
  LDomainY: Integer;
  LDomainZ: Integer;
  LDomains: TWfcPipelineCellDomains;
  LFields: TWfcPipelineRunTextFields;
  LHeight: Integer;
  LLineIndex: Integer;
  LLines: TWfcTextLines;
  LLockCount: Integer;
  LLocks: TWfcPipelineCellLocks;
  LMaxBacktracks: Integer;
  LMaxPassBacktracks: Integer;
  LRecipeSignatureText: String;
  LRun: TWfcPipelineRun;
  LSeed: TGraphSeed;
  LSignatureText: String;
  LStrategy: TWfcPipelineSolveStrategy;
  LTotalAllowedCount: Integer;
  LTotalEncodedTokenLength: Integer;
  LWidth: Integer;
begin
  if not Assigned(ARecipe) then
    raise EArgumentNilException.Create('WFC pipeline run recipe cannot be nil');
  Result := nil;
  PreflightTextEnvelope(AText);
  WfcTextSplitCanonicalLines(AText,
    WFC_PIPELINE_RUN_TEXT_ARTIFACT, LLines);
  if Length(LLines) > WFC_PIPELINE_RUN_MAX_TEXT_LINE_COUNT then
    TextError('document exceeds the version-1 line-count limit');
  LLineIndex := 0;
  if RequireLine(LLines, LLineIndex, 'header') <>
      'wfcpipeline-run=1' then
    TextError('header or version is unsupported');
  Inc(LLineIndex);
  LRecipeSignatureText := ReadValueLine(LLines, LLineIndex,
    'recipe-signature=', 'recipe signature');
  if not SignatureTextIsCanonical(LRecipeSignatureText) then
    TextError(
      'recipe signature must be eight uppercase hexadecimal digits');
  if LRecipeSignatureText <>
      WfcPipelineSignatureHex(ARecipe.Signature) then
    TextError('recipe signature does not match the supplied recipe');
  LWidth := ParseCanonicalInteger(ReadValueLine(LLines,
    LLineIndex, 'width=', 'width'), 'width');
  LHeight := ParseCanonicalInteger(ReadValueLine(LLines,
    LLineIndex, 'height=', 'height'), 'height');
  LDepth := ParseCanonicalInteger(ReadValueLine(LLines,
    LLineIndex, 'depth=', 'depth'), 'depth');
  LSeed := ParseCanonicalCardinal(ReadValueLine(LLines,
    LLineIndex, 'seed=', 'seed'), 'seed');
  LStrategy := ParseStrategy(ReadValueLine(LLines,
    LLineIndex, 'strategy=', 'strategy'));
  LMaxBacktracks := ParseCanonicalInteger(ReadValueLine(LLines,
    LLineIndex, 'max-backtracks=', 'backtrack limit'),
    'backtrack limit');
  LMaxPassBacktracks := ParseCanonicalInteger(ReadValueLine(LLines,
    LLineIndex, 'max-pass-backtracks=', 'pass-backtrack limit'),
    'pass-backtrack limit');
  LCaptureTrace := ParseBooleanName(ReadValueLine(LLines,
    LLineIndex, 'trace=', 'trace flag'), 'trace flag');

  LTotalEncodedTokenLength := 0;
  LLockCount := ParseBoundedCount(ReadValueLine(LLines,
    LLineIndex, 'locks=', 'lock count'), 'lock count',
    WFC_PIPELINE_RUN_MAX_LOCK_COUNT);
  RequireRecordCapacity(LLockCount, 3, LLineIndex, LLines, 'lock');
  SetLength(LLocks, LLockCount);
  for I := 0 to LLockCount - 1 do
  begin
    LFields := SplitRecord(ReadValueLine(LLines, LLineIndex,
      'lock=', 'lock record'), 6, 'lock');
    if ParseCanonicalInteger(LFields[0], 'lock index') <> I then
      TextError('lock indices must be complete and ordered');
    LLocks[I] := MakeWfcPipelineCellLock(
      ParseCanonicalInteger(LFields[1], 'lock pass index'),
      ParseCanonicalInteger(LFields[2], 'lock X coordinate'),
      ParseCanonicalInteger(LFields[3], 'lock Y coordinate'),
      ParseCanonicalInteger(LFields[4], 'lock Z coordinate'),
      DecodeOuterToken(LFields[5], 'lock token',
        LTotalEncodedTokenLength));
  end;

  LDomainCount := ParseBoundedCount(ReadValueLine(LLines,
    LLineIndex, 'domains=', 'domain count'), 'domain count',
    WFC_PIPELINE_RUN_MAX_DOMAIN_COUNT);
  RequireRecordCapacity(LDomainCount, 2, LLineIndex, LLines, 'domain');
  SetLength(LDomains, LDomainCount);
  LTotalAllowedCount := 0;
  for I := 0 to LDomainCount - 1 do
  begin
    LFields := SplitRecord(ReadValueLine(LLines, LLineIndex,
      'domain=', 'domain record'), 6, 'domain');
    if ParseCanonicalInteger(LFields[0], 'domain index') <> I then
      TextError('domain indices must be complete and ordered');
    LDomainPassIndex := ParseCanonicalInteger(LFields[1],
      'domain pass index');
    LDomainX := ParseCanonicalInteger(LFields[2],
      'domain X coordinate');
    LDomainY := ParseCanonicalInteger(LFields[3],
      'domain Y coordinate');
    LDomainZ := ParseCanonicalInteger(LFields[4],
      'domain Z coordinate');
    LAllowedCount := ParseBoundedCount(LFields[5],
      'domain token count', WFC_PIPELINE_RUN_MAX_DOMAIN_TOKEN_COUNT);
    if LAllowedCount > WFC_PIPELINE_RUN_MAX_TOTAL_DOMAIN_TOKEN_COUNT -
        LTotalAllowedCount then
      TextError(
        'aggregate domain-token count exceeds the version-1 limit');
    Inc(LTotalAllowedCount, LAllowedCount);
    RequireRecordCapacity(LAllowedCount, 2, LLineIndex,
      LLines, 'allowed-token');
    SetLength(LAllowedTokens, LAllowedCount);
    for J := 0 to LAllowedCount - 1 do
    begin
      LFields := SplitRecord(ReadValueLine(LLines, LLineIndex,
        'allowed=', 'allowed-token record'), 3, 'allowed-token');
      if (ParseCanonicalInteger(LFields[0],
          'allowed-token domain index') <> I) or
          (ParseCanonicalInteger(LFields[1],
          'allowed-token index') <> J) then
        TextError('allowed-token indices must be complete and ordered');
      LAllowedTokens[J] := DecodeOuterToken(LFields[2],
        'allowed token', LTotalEncodedTokenLength);
    end;
    LDomains[I] := MakeWfcPipelineCellDomain(LDomainPassIndex,
      LDomainX, LDomainY, LDomainZ, LAllowedTokens);
  end;

  LSignatureText := ReadValueLine(LLines, LLineIndex,
    'signature=', 'run signature');
  if not SignatureTextIsCanonical(LSignatureText) then
    TextError('run signature must be eight uppercase hexadecimal digits');
  if RequireLine(LLines, LLineIndex, 'end marker') <> 'end' then
    TextError('end marker is missing or misplaced');
  Inc(LLineIndex);
  if LLineIndex <> Length(LLines) then
    TextError('trailing data is not permitted');

  LRun := nil;
  try
    try
      LRun := TWfcPipelineRun.Create(ARecipe, LWidth, LHeight,
        LDepth, LSeed, LStrategy, LMaxBacktracks,
        LMaxPassBacktracks, LCaptureTrace, LLocks, LDomains);
    except
      on E: EWfcPipelineRun do
        TextError(E.Message);
    end;
    if WfcPipelineRunSignatureHex(LRun.Signature) <>
        LSignatureText then
      TextError('run signature does not match its semantic invocation');
    if EncodeWfcPipelineRunText(LRun) <> AText then
      TextError('document is not in canonical form');
    Result := LRun;
    LRun := nil;
  finally
    LRun.Free;
  end;
end;

end.
