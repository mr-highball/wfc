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
unit wfc_pipeline_text;

{$mode delphi}{$H+}

interface

uses
  wfc_pipeline_model;

const
  WFC_PIPELINE_TEXT_VERSION = 1;
  WFC_PIPELINE_MAX_ENCODED_TEXT_LENGTH = 268435456;
  WFC_PIPELINE_MAX_TEXT_LINE_COUNT = 26 +
    WFC_PIPELINE_MAX_RESOURCE_COUNT + WFC_PIPELINE_MAX_PASS_COUNT +
    WFC_PIPELINE_MAX_DEPENDENCY_COUNT + WFC_PIPELINE_MAX_BRIDGE_COUNT +
    WFC_PIPELINE_MAX_REQUIREMENT_COUNT +
    WFC_PIPELINE_MAX_TOTAL_REQUIREMENT_TERM_COUNT +
    WFC_PIPELINE_MAX_TOTAL_ALLOWED_TOKEN_COUNT;

function EncodeWfcPipelineModelText(
  const AModel: TWfcPipelineModel): String;
function DecodeWfcPipelineModelText(
  const AText: String): TWfcPipelineModel;

implementation

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_sequence,
  wfc_text_codec;

const
  WFC_PIPELINE_TEXT_ARTIFACT = 'WFC pipeline';
  WFC_PIPELINE_FIXED_LINE_COUNT = 26;

type
  TWfcPipelineTextFields = array of String;

procedure TextError(const AMessage: String);
begin
  WfcTextError(WFC_PIPELINE_TEXT_ARTIFACT, AMessage);
end;

function ParseCanonicalInteger(const AText,
  AFieldName: String): Integer;
begin
  Result := WfcTextParseCanonicalInteger(AText, AFieldName,
    WFC_PIPELINE_TEXT_ARTIFACT);
end;

function ParseCanonicalSignedInteger(const AText,
  AFieldName: String): Integer;
begin
  Result := WfcTextParseCanonicalSignedInteger(AText, AFieldName,
    WFC_PIPELINE_TEXT_ARTIFACT);
end;

function ValueAfterPrefix(const ALine, APrefix,
  AFieldName: String): String;
begin
  Result := WfcTextValueAfterPrefix(ALine, APrefix, AFieldName,
    WFC_PIPELINE_TEXT_ARTIFACT);
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
      'canonical WFC pipeline line capacity was exceeded');
  ALines[ALineCount] := ALine;
  Inc(ALineCount);
end;

procedure AddLineCapacity(var ACount: Integer; const AAdditional: Integer);
begin
  if AAdditional < 0 then
    raise EArgumentOutOfRangeException.Create(
      'canonical WFC pipeline line addition cannot be negative');
  if ACount > WFC_PIPELINE_MAX_TEXT_LINE_COUNT - AAdditional then
    raise ERangeError.Create('canonical WFC pipeline text has too many lines');
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
        WFC_PIPELINE_MAX_ENCODED_TEXT_LENGTH - LTotalLength - 1) then
      raise ERangeError.Create(
        'canonical WFC pipeline text exceeds the version-1 length limit');
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
      (LTextLength > SizeInt(WFC_PIPELINE_MAX_ENCODED_TEXT_LENGTH)) then
    TextError('document exceeds the version-1 encoded length limit');
  LLineCount := 0;
  for I := 1 to LTextLength do
    if AText[I] = #10 then
    begin
      if LLineCount = WFC_PIPELINE_MAX_TEXT_LINE_COUNT then
        TextError('document exceeds the version-1 line-count limit');
      Inc(LLineCount);
    end;
end;

function SplitRecord(const AText: String;
  const AFieldCount: Integer; const ARecordName: String):
  TWfcPipelineTextFields;
var
  I: Integer;
  LComma: Integer;
  LStart: Integer;
begin
  Result := nil;
  if AFieldCount < 1 then
    raise EArgumentOutOfRangeException.Create(
      'pipeline record field count must be positive');
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

function EncodeToken(const AValue: TWfcModelToken): String;
begin
  Result := WfcTextEncodeToken(AValue, WFC_PIPELINE_TEXT_ARTIFACT);
end;

function DecodeToken(const AValue: String): TWfcModelToken;
begin
  Result := WfcTextDecodeToken(AValue, WFC_PIPELINE_TEXT_ARTIFACT);
end;

function EncodeDocument(const AValue: String): String;
begin
  Result := WfcTextEncodeToken(TWfcModelToken(AValue),
    WFC_PIPELINE_TEXT_ARTIFACT);
end;

function DecodeDocument(const AValue: String): String;
begin
  Result := String(WfcTextDecodeToken(AValue,
    WFC_PIPELINE_TEXT_ARTIFACT));
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

function ResourceKindName(
  const AKind: TWfcPipelineResourceKind): String;
begin
  case AKind of
    wprkRules:
      Result := 'rules';
    wprkModel:
      Result := 'model';
    wprkPattern2D:
      Result := 'pattern2d';
    wprkSequence:
      Result := 'sequence';
  else
    raise ERangeError.Create('unknown WFC pipeline resource kind');
  end;
end;

function ParseResourceKind(
  const AText: String): TWfcPipelineResourceKind;
begin
  if AText = 'rules' then
    Result := wprkRules
  else if AText = 'model' then
    Result := wprkModel
  else if AText = 'pattern2d' then
    Result := wprkPattern2D
  else if AText = 'sequence' then
    Result := wprkSequence
  else
    TextError('resource has an unknown kind');
end;

function VisibilityName(
  const AVisibility: TWfcPipelinePassVisibility): String;
begin
  case AVisibility of
    wppvPrivate:
      Result := 'private';
    wppvPublic:
      Result := 'public';
  else
    raise ERangeError.Create('unknown WFC pipeline pass visibility');
  end;
end;

function ParseVisibility(
  const AText: String): TWfcPipelinePassVisibility;
begin
  if AText = 'private' then
    Result := wppvPrivate
  else if AText = 'public' then
    Result := wppvPublic
  else
    TextError('pass has an unknown visibility');
end;

function PassModeName(const AMode: TGraphPassMode): String;
begin
  case AMode of
    gpmLegacy:
      Result := 'legacy';
    gpmTransform:
      Result := 'transform';
    gpmOverlay:
      Result := 'overlay';
  else
    raise ERangeError.Create('unknown WFC pipeline pass mode');
  end;
end;

function ParsePassMode(const AText: String): TGraphPassMode;
begin
  if AText = 'legacy' then
    Result := gpmLegacy
  else if AText = 'transform' then
    Result := gpmTransform
  else if AText = 'overlay' then
    Result := gpmOverlay
  else
    TextError('pass has an unknown mode');
end;

function AdapterKindName(
  const AKind: TWfcPipelineAdapterKind): String;
begin
  case AKind of
    wpakEmpty:
      Result := 'empty';
    wpakRules:
      Result := 'rules';
    wpakModel:
      Result := 'model';
    wpakPattern2D:
      Result := 'pattern2d';
    wpakSequence:
      Result := 'sequence';
  else
    raise ERangeError.Create('unknown WFC pipeline adapter kind');
  end;
end;

function ParseAdapterKind(
  const AText: String): TWfcPipelineAdapterKind;
begin
  if AText = 'empty' then
    Result := wpakEmpty
  else if AText = 'rules' then
    Result := wpakRules
  else if AText = 'model' then
    Result := wpakModel
  else if AText = 'pattern2d' then
    Result := wpakPattern2D
  else if AText = 'sequence' then
    Result := wpakSequence
  else
    TextError('pass has an unknown adapter kind');
end;

function BridgeKindName(
  const AKind: TWfcPipelineBridgeKind): String;
begin
  case AKind of
    wpbkPattern2DProjection:
      Result := 'pattern2d-projection';
    wpbkSequenceProjection:
      Result := 'sequence-projection';
  else
    raise ERangeError.Create('unknown WFC pipeline bridge kind');
  end;
end;

function ParseBridgeKind(
  const AText: String): TWfcPipelineBridgeKind;
begin
  if AText = 'pattern2d-projection' then
    Result := wpbkPattern2DProjection
  else if AText = 'sequence-projection' then
    Result := wpbkSequenceProjection
  else
    TextError('bridge has an unknown kind');
end;

function RequirementKindName(
  const AKind: TWfcPipelineRequirementKind): String;
begin
  case AKind of
    wprqExact:
      Result := 'exact';
    wprqAny:
      Result := 'any';
  else
    raise ERangeError.Create('unknown WFC pipeline requirement kind');
  end;
end;

function ParseRequirementKind(
  const AText: String): TWfcPipelineRequirementKind;
begin
  if AText = 'exact' then
    Result := wprqExact
  else if AText = 'any' then
    Result := wprqAny
  else
    TextError('requirement has an unknown kind');
end;

function RunModeName(const AMode: TGraphRunMode): String;
begin
  case AMode of
    rmBottomUp:
      Result := 'bottom-up';
    rmTopDown:
      Result := 'top-down';
  else
    raise ERangeError.Create('unknown WFC pipeline traversal mode');
  end;
end;

function ParseRunMode(const AText: String): TGraphRunMode;
begin
  if AText = 'bottom-up' then
    Result := rmBottomUp
  else if AText = 'top-down' then
    Result := rmTopDown
  else
    TextError('traversal mode has an unknown value');
end;

function SequenceExtentName(const AExtent: TWfcSequenceExtent): String;
begin
  case AExtent of
    wseWhole:
      Result := 'whole';
    wsePrefix:
      Result := 'prefix';
    wseSuffix:
      Result := 'suffix';
    wseFragment:
      Result := 'fragment';
    wseWrap:
      Result := 'wrap';
  else
    raise ERangeError.Create('unknown WFC pipeline sequence extent');
  end;
end;

function ParseSequenceExtent(
  const AText: String): TWfcSequenceExtent;
begin
  if AText = 'whole' then
    Result := wseWhole
  else if AText = 'prefix' then
    Result := wsePrefix
  else if AText = 'suffix' then
    Result := wseSuffix
  else if AText = 'fragment' then
    Result := wseFragment
  else if AText = 'wrap' then
    Result := wseWrap
  else
    TextError('pass has an unknown sequence extent');
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

procedure RequireRecordCapacity(const ACount, AMinimumTail,
  ALineIndex: Integer; const ALines: TWfcTextLines;
  const ARecordName: String);
begin
  if ACount < 0 then
    TextError(ARecordName + ' count cannot be negative');
  if AMinimumTail < 0 then
    raise EArgumentOutOfRangeException.Create(
      'pipeline record tail cannot be negative');
  if ALineIndex > Length(ALines) - AMinimumTail then
    TextError(ARecordName + ' records are incomplete');
  if ACount > Length(ALines) - ALineIndex - AMinimumTail then
    TextError(ARecordName + ' records are incomplete');
end;

function ParseBoundedCount(const AText, AFieldName: String;
  const AMaximum: Integer): Integer;
begin
  Result := ParseCanonicalInteger(AText, AFieldName);
  if Result > AMaximum then
    TextError(AFieldName + ' exceeds the version-1 limit');
end;

function PreflightEncodedPayloadLength(const AText: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  I := 1;
  while I <= Length(AText) do
  begin
    if Result = WFC_PIPELINE_MAX_RESOURCE_PAYLOAD_LENGTH then
      TextError('resource payload exceeds the version-1 limit');
    Inc(Result);
    if AText[I] = '%' then
    begin
      if I > Length(AText) - 2 then
        TextError('resource payload has a truncated percent escape');
      Inc(I, 3);
    end
    else
      Inc(I);
  end;
end;

function EncodeWfcPipelineModelText(
  const AModel: TWfcPipelineModel): String;
var
  I: Integer;
  J: Integer;
  K: Integer;
  LBridge: TWfcPipelineBridge;
  LCount: Integer;
  LDependency: TWfcPipelineDependency;
  LExpectedLineCount: Integer;
  LLines: TWfcTextLines;
  LMetadata: TWfcPipelineMetadata;
  LPass: TWfcPipelinePass;
  LRequirement: TWfcPipelineRequirement;
  LResource: TWfcPipelineResource;
  LTerm: TWfcPipelineRequirementTerm;
  LVersions: TWfcPipelineVersions;
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create('WFC pipeline model cannot be nil');

  LMetadata := AModel.CopyMetadata;
  LVersions := AModel.CopyVersions;
  LExpectedLineCount := WFC_PIPELINE_FIXED_LINE_COUNT;
  AddLineCapacity(LExpectedLineCount, AModel.ResourceCount);
  AddLineCapacity(LExpectedLineCount, AModel.PassCount);
  AddLineCapacity(LExpectedLineCount, AModel.DependencyCount);
  AddLineCapacity(LExpectedLineCount, AModel.BridgeCount);
  AddLineCapacity(LExpectedLineCount, AModel.RequirementCount);
  for I := 0 to AModel.RequirementCount - 1 do
  begin
    LRequirement := AModel.RequirementAt(I);
    AddLineCapacity(LExpectedLineCount, Length(LRequirement.Terms));
    for J := 0 to Length(LRequirement.Terms) - 1 do
      AddLineCapacity(LExpectedLineCount,
        Length(LRequirement.Terms[J].AllowedProviderTokens));
  end;
  SetLength(LLines, LExpectedLineCount);
  LCount := 0;
  AppendLine(LLines, LCount, 'wfcpipeline=' +
    IntToStr(WFC_PIPELINE_TEXT_VERSION));
  AppendLine(LLines, LCount, 'name=' + EncodeToken(LMetadata.Name));
  AppendLine(LLines, LCount, 'license=' +
    EncodeToken(LMetadata.LicenseIdentifier));
  AppendLine(LLines, LCount, 'source=' +
    EncodeToken(LMetadata.SourceDescription));
  AppendLine(LLines, LCount, 'fingerprint=' +
    EncodeToken(LMetadata.SourceFingerprint));

  AppendLine(LLines, LCount, 'graph-model-version=' +
    IntToStr(LVersions.GraphModelVersion));
  AppendLine(LLines, LCount, 'random-algorithm-version=' +
    IntToStr(LVersions.RandomAlgorithmVersion));
  AppendLine(LLines, LCount, 'solver-algorithm-version=' +
    IntToStr(LVersions.SolverAlgorithmVersion));
  AppendLine(LLines, LCount, 'pipeline-algorithm-version=' +
    IntToStr(LVersions.PipelineAlgorithmVersion));
  AppendLine(LLines, LCount, 'bundle-graph-adapter-version=' +
    IntToStr(LVersions.BundleGraphAdapterVersion));
  AppendLine(LLines, LCount, 'model-graph-adapter-version=' +
    IntToStr(LVersions.ModelGraphAdapterVersion));
  AppendLine(LLines, LCount, 'rules-graph-adapter-version=' +
    IntToStr(LVersions.RulesGraphAdapterVersion));
  AppendLine(LLines, LCount, 'pattern2d-graph-adapter-version=' +
    IntToStr(LVersions.Pattern2DGraphAdapterVersion));
  AppendLine(LLines, LCount, 'sequence-graph-adapter-version=' +
    IntToStr(LVersions.SequenceGraphAdapterVersion));
  AppendLine(LLines, LCount, 'pattern2d-bridge-version=' +
    IntToStr(LVersions.Pattern2DBridgeVersion));
  AppendLine(LLines, LCount, 'sequence-bridge-version=' +
    IntToStr(LVersions.SequenceBridgeVersion));

  AppendLine(LLines, LCount, 'rank=' + IntToStr(AModel.Rank));
  AppendLine(LLines, LCount, 'wrap=' +
    BooleanName(AModel.WrapNeighbors));
  AppendLine(LLines, LCount, 'traversal=' +
    RunModeName(AModel.RunMode));

  AppendLine(LLines, LCount, 'resources=' +
    IntToStr(AModel.ResourceCount));
  for I := 0 to AModel.ResourceCount - 1 do
  begin
    LResource := AModel.ResourceAt(I);
    AppendLine(LLines, LCount, 'resource=' + IntToStr(I) + ',' +
      EncodeToken(LResource.Id) + ',' +
      ResourceKindName(LResource.Kind) + ',' +
      EncodeDocument(LResource.Document) + ',' +
      EncodeToken(LResource.SourceDescription) + ',' +
      EncodeToken(LResource.SourceLicenseIdentifier) + ',' +
      EncodeToken(LResource.SourceFingerprint));
  end;

  AppendLine(LLines, LCount, 'passes=' +
    IntToStr(AModel.PassCount));
  for I := 0 to AModel.PassCount - 1 do
  begin
    LPass := AModel.PassAt(I);
    AppendLine(LLines, LCount, 'pass=' + IntToStr(I) + ',' +
      EncodeToken(LPass.LabelName) + ',' +
      VisibilityName(LPass.Visibility) + ',' +
      PassModeName(LPass.Mode) + ',' +
      IntToStr(LPass.TransformSourceIndex) + ',' +
      AdapterKindName(LPass.AdapterKind) + ',' +
      IntToStr(LPass.ResourceIndex) + ',' +
      BooleanName(LPass.HasSequenceExtent) + ',' +
      SequenceExtentName(LPass.SequenceExtent));
  end;

  AppendLine(LLines, LCount, 'dependencies=' +
    IntToStr(AModel.DependencyCount));
  for I := 0 to AModel.DependencyCount - 1 do
  begin
    LDependency := AModel.DependencyAt(I);
    AppendLine(LLines, LCount, 'dependency=' + IntToStr(I) + ',' +
      IntToStr(LDependency.ConsumerPassIndex) + ',' +
      IntToStr(LDependency.ProviderPassIndex));
  end;

  AppendLine(LLines, LCount, 'bridges=' +
    IntToStr(AModel.BridgeCount));
  for I := 0 to AModel.BridgeCount - 1 do
  begin
    LBridge := AModel.BridgeAt(I);
    AppendLine(LLines, LCount, 'bridge=' + IntToStr(I) + ',' +
      BridgeKindName(LBridge.Kind) + ',' +
      IntToStr(LBridge.SourcePassIndex) + ',' +
      IntToStr(LBridge.TargetPassIndex));
  end;

  AppendLine(LLines, LCount, 'requirements=' +
    IntToStr(AModel.RequirementCount));
  for I := 0 to AModel.RequirementCount - 1 do
  begin
    LRequirement := AModel.RequirementAt(I);
    AppendLine(LLines, LCount, 'requirement=' + IntToStr(I) + ',' +
      IntToStr(LRequirement.ConsumerPassIndex) + ',' +
      EncodeToken(LRequirement.ConsumerToken) + ',' +
      IntToStr(LRequirement.ProviderPassIndex) + ',' +
      RequirementKindName(LRequirement.Kind) + ',' +
      IntToStr(Length(LRequirement.Terms)));
    for J := 0 to Length(LRequirement.Terms) - 1 do
    begin
      LTerm := LRequirement.Terms[J];
      AppendLine(LLines, LCount, 'term=' + IntToStr(I) + ',' +
        IntToStr(J) + ',' + IntToStr(LTerm.OffsetX) + ',' +
        IntToStr(LTerm.OffsetY) + ',' + IntToStr(LTerm.OffsetZ) + ',' +
        IntToStr(Length(LTerm.AllowedProviderTokens)));
      for K := 0 to Length(LTerm.AllowedProviderTokens) - 1 do
        AppendLine(LLines, LCount, 'allowed=' + IntToStr(I) + ',' +
          IntToStr(J) + ',' + IntToStr(K) + ',' +
          EncodeToken(LTerm.AllowedProviderTokens[K]));
    end;
  end;

  AppendLine(LLines, LCount, 'signature=' +
    WfcPipelineSignatureHex(AModel.Signature));
  AppendLine(LLines, LCount, 'end');
  if LCount <> LExpectedLineCount then
    raise Exception.Create(
      'canonical WFC pipeline line count does not match its preflight');
  RequireEncodedTextLength(LLines);
  Result := WfcTextJoinCanonicalLines(LLines,
    WFC_PIPELINE_TEXT_ARTIFACT);
end;

function DecodeWfcPipelineModelText(
  const AText: String): TWfcPipelineModel;
var
  I: Integer;
  J: Integer;
  K: Integer;
  LAllowedCount: Integer;
  LAllowedTokens: TWfcModelTokens;
  LBridgeCount: Integer;
  LBridges: TWfcPipelineBridges;
  LDependencyCount: Integer;
  LDependencies: TWfcPipelineDependencies;
  LFields: TWfcPipelineTextFields;
  LHasSequenceExtent: Boolean;
  LLineIndex: Integer;
  LLines: TWfcTextLines;
  LMetadata: TWfcPipelineMetadata;
  LModel: TWfcPipelineModel;
  LPassCount: Integer;
  LPasses: TWfcPipelinePasses;
  LRank: Integer;
  LRequirementConsumerIndex: Integer;
  LRequirementConsumerToken: TWfcModelToken;
  LRequirementCount: Integer;
  LRequirementKind: TWfcPipelineRequirementKind;
  LRequirementProviderIndex: Integer;
  LRequirements: TWfcPipelineRequirements;
  LResourceCount: Integer;
  LResourceFingerprint: TWfcModelToken;
  LResourceId: TWfcModelToken;
  LResourceLicense: TWfcModelToken;
  LResourcePayloadLength: Integer;
  LResources: TWfcPipelineResources;
  LResourceSource: TWfcModelToken;
  LRunMode: TGraphRunMode;
  LSequenceExtent: TWfcSequenceExtent;
  LSignatureText: String;
  LTermCount: Integer;
  LTermOffsetX: Integer;
  LTermOffsetY: Integer;
  LTermOffsetZ: Integer;
  LTerms: TWfcPipelineRequirementTerms;
  LTotalAllowedTokenCount: Integer;
  LTotalEncodedTokenLength: Integer;
  LTotalPayloadLength: Integer;
  LTotalRequirementTermCount: Integer;
  LVersions: TWfcPipelineVersions;
  LWrapNeighbors: Boolean;

  function DecodeOuterToken(const AValue,
    AFieldName: String): TWfcModelToken;
  var
    LEncodedLength: SizeInt;
  begin
    LEncodedLength := Length(AValue);
    if (LEncodedLength < 0) or
        (LEncodedLength >
        SizeInt(WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH)) then
      TextError(AFieldName +
        ' exceeds the version-1 encoded token-length limit');
    if LTotalEncodedTokenLength >
        WFC_PIPELINE_MAX_TOTAL_ENCODED_TOKEN_LENGTH -
        Integer(LEncodedLength) then
      TextError('aggregate outer-token encoding exceeds the version-1 limit');
    Inc(LTotalEncodedTokenLength, Integer(LEncodedLength));
    Result := DecodeToken(AValue);
  end;
begin
  Result := nil;
  PreflightTextEnvelope(AText);
  WfcTextSplitCanonicalLines(AText, WFC_PIPELINE_TEXT_ARTIFACT,
    LLines);
  if Length(LLines) < WFC_PIPELINE_FIXED_LINE_COUNT then
    TextError('document is incomplete');
  LLineIndex := 0;
  if RequireLine(LLines, LLineIndex, 'format version') <>
      'wfcpipeline=1' then
    TextError('unsupported or noncanonical format version');
  Inc(LLineIndex);

  { Pascal does not define argument evaluation order. Read stateful fields
    one at a time before any constructor/helper call. }
  LTotalEncodedTokenLength := 0;
  LMetadata.Name := DecodeOuterToken(ReadValueLine(LLines, LLineIndex,
    'name=', 'name'), 'name');
  LMetadata.LicenseIdentifier := DecodeOuterToken(ReadValueLine(LLines,
    LLineIndex, 'license=', 'license'), 'license');
  LMetadata.SourceDescription := DecodeOuterToken(ReadValueLine(LLines,
    LLineIndex, 'source=', 'source'), 'source');
  LMetadata.SourceFingerprint := DecodeOuterToken(ReadValueLine(LLines,
    LLineIndex, 'fingerprint=', 'fingerprint'), 'fingerprint');

  LVersions.GraphModelVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'graph-model-version=',
      'graph-model version'), 'graph-model version');
  LVersions.RandomAlgorithmVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'random-algorithm-version=',
      'random-algorithm version'), 'random-algorithm version');
  LVersions.SolverAlgorithmVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'solver-algorithm-version=',
      'solver-algorithm version'), 'solver-algorithm version');
  LVersions.PipelineAlgorithmVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'pipeline-algorithm-version=',
      'pipeline-algorithm version'), 'pipeline-algorithm version');
  LVersions.BundleGraphAdapterVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'bundle-graph-adapter-version=',
      'bundle graph-adapter version'), 'bundle graph-adapter version');
  LVersions.ModelGraphAdapterVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'model-graph-adapter-version=',
      'model graph-adapter version'), 'model graph-adapter version');
  LVersions.RulesGraphAdapterVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'rules-graph-adapter-version=',
      'rules graph-adapter version'), 'rules graph-adapter version');
  LVersions.Pattern2DGraphAdapterVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'pattern2d-graph-adapter-version=',
      'pattern2d graph-adapter version'), 'pattern2d graph-adapter version');
  LVersions.SequenceGraphAdapterVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'sequence-graph-adapter-version=',
      'sequence graph-adapter version'), 'sequence graph-adapter version');
  LVersions.Pattern2DBridgeVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'pattern2d-bridge-version=',
      'pattern2d bridge version'), 'pattern2d bridge version');
  LVersions.SequenceBridgeVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'sequence-bridge-version=',
      'sequence bridge version'), 'sequence bridge version');

  LRank := ParseCanonicalInteger(ReadValueLine(LLines, LLineIndex,
    'rank=', 'rank'), 'rank');
  LWrapNeighbors := ParseBooleanName(ReadValueLine(LLines,
    LLineIndex, 'wrap=', 'wrap policy'), 'wrap policy');
  LRunMode := ParseRunMode(ReadValueLine(LLines, LLineIndex,
    'traversal=', 'traversal mode'));

  LResourceCount := ParseBoundedCount(ReadValueLine(LLines,
    LLineIndex, 'resources=', 'resource count'), 'resource count',
    WFC_PIPELINE_MAX_RESOURCE_COUNT);
  RequireRecordCapacity(LResourceCount, 6, LLineIndex, LLines,
    'resource');
  SetLength(LResources, LResourceCount);
  LTotalPayloadLength := 0;
  for I := 0 to LResourceCount - 1 do
  begin
    LFields := SplitRecord(ReadValueLine(LLines, LLineIndex,
      'resource=', 'resource record'), 7, 'resource');
    if ParseCanonicalInteger(LFields[0], 'resource index') <> I then
      TextError('resource indices must be complete and ordered');
    LResourcePayloadLength := PreflightEncodedPayloadLength(LFields[3]);
    if LTotalPayloadLength >
        WFC_PIPELINE_MAX_TOTAL_RESOURCE_PAYLOAD_LENGTH -
        LResourcePayloadLength then
      TextError('aggregate resource payload exceeds the version-1 limit');
    Inc(LTotalPayloadLength, LResourcePayloadLength);
    LResourceId := DecodeOuterToken(LFields[1], 'resource id');
    LResourceSource := DecodeOuterToken(LFields[4],
      'resource source description');
    LResourceLicense := DecodeOuterToken(LFields[5],
      'resource source license');
    LResourceFingerprint := DecodeOuterToken(LFields[6],
      'resource source fingerprint');
    LResources[I] := MakeWfcPipelineResource(
      LResourceId, ParseResourceKind(LFields[2]),
      DecodeDocument(LFields[3]), LResourceSource,
      LResourceLicense, LResourceFingerprint);
  end;

  LPassCount := ParseBoundedCount(ReadValueLine(LLines, LLineIndex,
    'passes=', 'pass count'), 'pass count', WFC_PIPELINE_MAX_PASS_COUNT);
  RequireRecordCapacity(LPassCount, 5, LLineIndex, LLines, 'pass');
  SetLength(LPasses, LPassCount);
  for I := 0 to LPassCount - 1 do
  begin
    LFields := SplitRecord(ReadValueLine(LLines, LLineIndex,
      'pass=', 'pass record'), 9, 'pass');
    if ParseCanonicalInteger(LFields[0], 'pass index') <> I then
      TextError('pass indices must be complete and ordered');
    LHasSequenceExtent := ParseBooleanName(LFields[7],
      'pass sequence-extent presence');
    LSequenceExtent := ParseSequenceExtent(LFields[8]);
    LPasses[I] := MakeWfcPipelinePass(
      DecodeOuterToken(LFields[1], 'pass label'),
      ParseVisibility(LFields[2]), ParsePassMode(LFields[3]),
      ParseCanonicalSignedInteger(LFields[4], 'pass transform-source index'),
      ParseAdapterKind(LFields[5]),
      ParseCanonicalSignedInteger(LFields[6], 'pass resource index'),
      LHasSequenceExtent, LSequenceExtent);
  end;

  LDependencyCount := ParseBoundedCount(ReadValueLine(LLines,
    LLineIndex, 'dependencies=', 'dependency count'),
    'dependency count', WFC_PIPELINE_MAX_DEPENDENCY_COUNT);
  RequireRecordCapacity(LDependencyCount, 4, LLineIndex, LLines,
    'dependency');
  SetLength(LDependencies, LDependencyCount);
  for I := 0 to LDependencyCount - 1 do
  begin
    LFields := SplitRecord(ReadValueLine(LLines, LLineIndex,
      'dependency=', 'dependency record'), 3, 'dependency');
    if ParseCanonicalInteger(LFields[0], 'dependency index') <> I then
      TextError('dependency indices must be complete and ordered');
    LDependencies[I] := MakeWfcPipelineDependency(
      ParseCanonicalInteger(LFields[1], 'dependency consumer index'),
      ParseCanonicalInteger(LFields[2], 'dependency provider index'));
  end;

  LBridgeCount := ParseBoundedCount(ReadValueLine(LLines,
    LLineIndex, 'bridges=', 'bridge count'), 'bridge count',
    WFC_PIPELINE_MAX_BRIDGE_COUNT);
  RequireRecordCapacity(LBridgeCount, 3, LLineIndex, LLines, 'bridge');
  SetLength(LBridges, LBridgeCount);
  for I := 0 to LBridgeCount - 1 do
  begin
    LFields := SplitRecord(ReadValueLine(LLines, LLineIndex,
      'bridge=', 'bridge record'), 4, 'bridge');
    if ParseCanonicalInteger(LFields[0], 'bridge index') <> I then
      TextError('bridge indices must be complete and ordered');
    LBridges[I] := MakeWfcPipelineBridge(ParseBridgeKind(LFields[1]),
      ParseCanonicalInteger(LFields[2], 'bridge source index'),
      ParseCanonicalInteger(LFields[3], 'bridge target index'));
  end;

  LTotalAllowedTokenCount := 0;
  LTotalRequirementTermCount := 0;
  LRequirementCount := ParseBoundedCount(ReadValueLine(LLines,
    LLineIndex, 'requirements=', 'requirement count'),
    'requirement count', WFC_PIPELINE_MAX_REQUIREMENT_COUNT);
  RequireRecordCapacity(LRequirementCount, 2, LLineIndex, LLines,
    'requirement');
  SetLength(LRequirements, LRequirementCount);
  for I := 0 to LRequirementCount - 1 do
  begin
    LFields := SplitRecord(ReadValueLine(LLines, LLineIndex,
      'requirement=', 'requirement record'), 6, 'requirement');
    if ParseCanonicalInteger(LFields[0], 'requirement index') <> I then
      TextError('requirement indices must be complete and ordered');
    LRequirementConsumerIndex := ParseCanonicalInteger(LFields[1],
      'requirement consumer index');
    LRequirementConsumerToken := DecodeOuterToken(LFields[2],
      'requirement consumer token');
    LRequirementProviderIndex := ParseCanonicalInteger(LFields[3],
      'requirement provider index');
    LRequirementKind := ParseRequirementKind(LFields[4]);
    LTermCount := ParseBoundedCount(LFields[5],
      'requirement term count', WFC_PIPELINE_MAX_REQUIREMENT_TERM_COUNT);
    if LTotalRequirementTermCount >
        WFC_PIPELINE_MAX_TOTAL_REQUIREMENT_TERM_COUNT - LTermCount then
      TextError('aggregate requirement-term count exceeds the version-1 limit');
    Inc(LTotalRequirementTermCount, LTermCount);
    RequireRecordCapacity(LTermCount, 2, LLineIndex, LLines,
      'requirement term');
    SetLength(LTerms, LTermCount);
    for J := 0 to LTermCount - 1 do
    begin
      LFields := SplitRecord(ReadValueLine(LLines, LLineIndex,
        'term=', 'requirement term record'), 6, 'requirement term');
      if (ParseCanonicalInteger(LFields[0],
          'requirement term parent index') <> I) or
          (ParseCanonicalInteger(LFields[1],
          'requirement term index') <> J) then
        TextError('requirement term indices must be complete and ordered');
      LTermOffsetX := ParseCanonicalSignedInteger(LFields[2],
        'requirement X offset');
      LTermOffsetY := ParseCanonicalSignedInteger(LFields[3],
        'requirement Y offset');
      LTermOffsetZ := ParseCanonicalSignedInteger(LFields[4],
        'requirement Z offset');
      LAllowedCount := ParseBoundedCount(LFields[5],
        'allowed-token count', WFC_PIPELINE_MAX_ALLOWED_TOKEN_COUNT);
      if LTotalAllowedTokenCount >
          WFC_PIPELINE_MAX_TOTAL_ALLOWED_TOKEN_COUNT - LAllowedCount then
        TextError('aggregate allowed-token count exceeds the version-1 limit');
      Inc(LTotalAllowedTokenCount, LAllowedCount);
      RequireRecordCapacity(LAllowedCount, 2, LLineIndex, LLines,
        'allowed-token');
      SetLength(LAllowedTokens, LAllowedCount);
      for K := 0 to LAllowedCount - 1 do
      begin
        LFields := SplitRecord(ReadValueLine(LLines, LLineIndex,
          'allowed=', 'allowed-token record'), 4, 'allowed-token');
        if (ParseCanonicalInteger(LFields[0],
            'allowed-token requirement index') <> I) or
            (ParseCanonicalInteger(LFields[1],
            'allowed-token term index') <> J) or
            (ParseCanonicalInteger(LFields[2],
            'allowed-token index') <> K) then
          TextError('allowed-token indices must be complete and ordered');
        LAllowedTokens[K] := DecodeOuterToken(LFields[3],
          'allowed provider token');
      end;
      LTerms[J] := MakeWfcPipelineRequirementTerm(
        LTermOffsetX, LTermOffsetY, LTermOffsetZ, LAllowedTokens);
    end;
    LRequirements[I] := MakeWfcPipelineRequirement(
      LRequirementConsumerIndex, LRequirementConsumerToken,
      LRequirementProviderIndex, LRequirementKind, LTerms);
  end;

  LSignatureText := ReadValueLine(LLines, LLineIndex,
    'signature=', 'pipeline signature');
  if not SignatureTextIsCanonical(LSignatureText) then
    TextError('pipeline signature must be eight uppercase hexadecimal digits');
  if RequireLine(LLines, LLineIndex, 'end marker') <> 'end' then
    TextError('end marker is missing or misplaced');
  Inc(LLineIndex);
  if LLineIndex <> Length(LLines) then
    TextError('trailing data is not permitted');

  LModel := nil;
  try
    try
      LModel := TWfcPipelineModel.Create(LMetadata, LVersions,
        LRank, LWrapNeighbors, LRunMode, LResources, LPasses,
        LDependencies, LBridges, LRequirements);
    except
      on E: EWfcPipelineModel do
        TextError(E.Message);
    end;
    if WfcPipelineSignatureHex(LModel.Signature) <> LSignatureText then
      TextError('pipeline signature does not match its semantic recipe');
    if EncodeWfcPipelineModelText(LModel) <> AText then
      TextError('document is not in canonical form');
    Result := LModel;
    LModel := nil;
  finally
    LModel.Free;
  end;
end;

end.
