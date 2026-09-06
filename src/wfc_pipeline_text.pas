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
  WFC_PIPELINE_VALUE_QUOTA_TEXT_VERSION = 2;
  WFC_PIPELINE_CONNECTIVITY_TEXT_VERSION = 3;
  WFC_PIPELINE_PATTERN_3D_TEXT_VERSION = 4;
  WFC_PIPELINE_MAPPED_TEXT_VERSION = 5;
  WFC_PIPELINE_MAX_SUPPORTED_TEXT_VERSION = WFC_PIPELINE_MAPPED_TEXT_VERSION;
  WFC_PIPELINE_MAX_ENCODED_TEXT_LENGTH = 268435456;
  WFC_PIPELINE_MAX_TEXT_LINE_COUNT = 26 +
    WFC_PIPELINE_MAX_RESOURCE_COUNT + WFC_PIPELINE_MAX_PASS_COUNT +
    WFC_PIPELINE_MAX_DEPENDENCY_COUNT + WFC_PIPELINE_MAX_BRIDGE_COUNT +
    WFC_PIPELINE_MAX_REQUIREMENT_COUNT +
    WFC_PIPELINE_MAX_REQUIREMENT_COUNT +
    WFC_PIPELINE_MAX_TOTAL_REQUIREMENT_TERM_COUNT +
    WFC_PIPELINE_MAX_TOTAL_ALLOWED_TOKEN_COUNT;
  WFC_PIPELINE_VALUE_QUOTA_MAX_TEXT_LINE_COUNT = WFC_PIPELINE_MAX_TEXT_LINE_COUNT +
    2 + WFC_PIPELINE_MAX_VALUE_QUOTA_COUNT +
    WFC_PIPELINE_MAX_TOTAL_VALUE_QUOTA_TOKEN_COUNT;
  WFC_PIPELINE_CONNECTIVITY_MAX_TEXT_LINE_COUNT =
    WFC_PIPELINE_VALUE_QUOTA_MAX_TEXT_LINE_COUNT + 2 +
    WFC_PIPELINE_MAX_CONNECTIVITY_COUNT +
    WFC_PIPELINE_MAX_TOTAL_CONNECTIVITY_REQUIRED_POSITION_COUNT +
    WFC_PIPELINE_MAX_TOTAL_CONNECTIVITY_VALUE_COUNT;
  WFC_PIPELINE_PATTERN_3D_MAX_TEXT_LINE_COUNT =
    WFC_PIPELINE_CONNECTIVITY_MAX_TEXT_LINE_COUNT + 2;
  WFC_PIPELINE_MAPPED_MAX_TEXT_LINE_COUNT =
    WFC_PIPELINE_PATTERN_3D_MAX_TEXT_LINE_COUNT + 4 + WFC_PIPELINE_MAX_PASS_COUNT;

function WfcPipelineModelTextVersion(const AModel: TWfcPipelineModel): Integer;
function EncodeWfcPipelineModelText(
  const AModel: TWfcPipelineModel): String;
function DecodeWfcPipelineModelText(
  const AText: String): TWfcPipelineModel;

implementation

uses
  SysUtils,
  wfc,
  wfc_lattice,
  wfc_pipeline_layout,
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

procedure AddLineCapacity(var ACount: Integer; const AAdditional: Integer;
  const AMaximum: Integer = WFC_PIPELINE_MAX_TEXT_LINE_COUNT);
begin
  if AAdditional < 0 then
    raise EArgumentOutOfRangeException.Create(
      'canonical WFC pipeline line addition cannot be negative');
  if ACount > AMaximum - AAdditional then
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

procedure PreflightTextEnvelope(const AText: String; out AVersion: Integer);
var
  I: SizeInt;
  LLineCount: Integer;
  LTextLength: SizeInt;
  LMaximumLines: Integer;
  LKnownVersion: Boolean;
begin
  LTextLength := Length(AText);
  if (LTextLength < 0) or
      (LTextLength > SizeInt(WFC_PIPELINE_MAX_ENCODED_TEXT_LENGTH)) then
    TextError('document exceeds the version-1 encoded length limit');
  { Determine the exact supported header before allocating the line array.
    V1 retains its original denial-of-service envelope even when V2 exists. }
  LKnownVersion := True;
  if Copy(AText, 1, 14) = 'wfcpipeline=1'#10 then
  begin
    AVersion := WFC_PIPELINE_TEXT_VERSION;
    LMaximumLines := WFC_PIPELINE_MAX_TEXT_LINE_COUNT;
  end
  else if Copy(AText, 1, 14) = 'wfcpipeline=2'#10 then
  begin
    AVersion := WFC_PIPELINE_VALUE_QUOTA_TEXT_VERSION;
    LMaximumLines := WFC_PIPELINE_VALUE_QUOTA_MAX_TEXT_LINE_COUNT;
  end
  else if Copy(AText, 1, 14) = 'wfcpipeline=3'#10 then
  begin
    AVersion := WFC_PIPELINE_CONNECTIVITY_TEXT_VERSION;
    LMaximumLines := WFC_PIPELINE_CONNECTIVITY_MAX_TEXT_LINE_COUNT;
  end
  else if Copy(AText, 1, 14) = 'wfcpipeline=4'#10 then
  begin
    AVersion := WFC_PIPELINE_PATTERN_3D_TEXT_VERSION;
    LMaximumLines := WFC_PIPELINE_PATTERN_3D_MAX_TEXT_LINE_COUNT;
  end
  else if Copy(AText, 1, 14) = 'wfcpipeline=5'#10 then
  begin
    AVersion := WFC_PIPELINE_MAPPED_TEXT_VERSION;
    LMaximumLines := WFC_PIPELINE_MAPPED_MAX_TEXT_LINE_COUNT;
  end
  else
  begin
    { Preserve V1's early envelope diagnostic for malformed, line-heavy
      inputs too. No unsupported header is admitted after this scan. }
    LKnownVersion := False;
    AVersion := WFC_PIPELINE_TEXT_VERSION;
    LMaximumLines := WFC_PIPELINE_MAX_TEXT_LINE_COUNT;
  end;
  LLineCount := 0;
  for I := 1 to LTextLength do
    if AText[I] = #10 then
    begin
      if LLineCount = LMaximumLines then
        TextError('document exceeds the version-' + IntToStr(AVersion) + ' line-count limit');
      Inc(LLineCount);
    end;
  if not LKnownVersion then TextError('unsupported or noncanonical format version');
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
    wprkPattern3D:
      Result := 'pattern3d';
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
  else if AText = 'pattern3d' then
    Result := wprkPattern3D
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
    wpakPattern3D:
      Result := 'pattern3d';
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
  else if AText = 'pattern3d' then
    Result := wpakPattern3D
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
    wpbkPattern3DProjection:
      Result := 'pattern3d-projection';
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
  else if AText = 'pattern3d-projection' then
    Result := wpbkPattern3DProjection
  else
    TextError('bridge has an unknown kind');
end;

function RequirementKindName(
  const AKind: TWfcPipelineRequirementKind;
  const ACountMode: TGraphPassCountMode): String;
begin
  case AKind of
    wprqExact:
      Result := 'exact';
    wprqAny:
      Result := 'any';
    wprqMapped:
      Result := 'mapped-v1';
    wprqCount:
      case ACountMode of
        gpcmMatchingTerms:
          Result := 'count-terms-v1';
        gpcmDistinctCells:
          Result := 'count-cells-v1';
      else
        raise ERangeError.Create(
          'unknown WFC pipeline count requirement mode');
      end;
  else
    raise ERangeError.Create('unknown WFC pipeline requirement kind');
  end;
end;

procedure ParseRequirementKind(const AText: String;
  out AKind: TWfcPipelineRequirementKind;
  out ACountMode: TGraphPassCountMode);
begin
  ACountMode := gpcmMatchingTerms;
  if AText = 'exact' then
    AKind := wprqExact
  else if AText = 'any' then
    AKind := wprqAny
  else if AText = 'mapped-v1' then
    AKind := wprqMapped
  else if AText = 'count-terms-v1' then
  begin
    AKind := wprqCount;
    ACountMode := gpcmMatchingTerms;
  end
  else if AText = 'count-cells-v1' then
  begin
    AKind := wprqCount;
    ACountMode := gpcmDistinctCells;
  end
  else
    TextError('requirement has an unknown kind');
end;

function MappedKindName(const AKind: TGraphPassMapKind): String;
begin
  case AKind of gpmkPoint:Result:='point'; gpmkCellCoverage:Result:='cell';
    gpmkRegionCoverage:Result:='region';
    else raise ERangeError.Create('unknown mapped query kind'); end;
end;

function ParseMappedKind(const AText: String): TGraphPassMapKind;
begin
  if AText='point' then Result:=gpmkPoint else if AText='cell' then Result:=gpmkCellCoverage
  else if AText='region' then Result:=gpmkRegionCoverage else TextError('unknown mapped query kind');
end;

function MappedMatchName(const AMatch: TGraphPassMapMatch): String;
begin
  case AMatch of gpmmAll:Result:='all'; gpmmCount:Result:='count';
    else raise ERangeError.Create('unknown mapped query match'); end;
end;

function ParseMappedMatch(const AText: String): TGraphPassMapMatch;
begin
  if AText='all' then Result:=gpmmAll else if AText='count' then Result:=gpmmCount
  else TextError('unknown mapped query match');
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

function WfcPipelineModelTextVersion(const AModel: TWfcPipelineModel): Integer;
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create('WFC pipeline model cannot be nil');
  if AModel.HasPassMapping then Result := WFC_PIPELINE_MAPPED_TEXT_VERSION
  else if AModel.HasPattern3D then Result := WFC_PIPELINE_PATTERN_3D_TEXT_VERSION
  else if AModel.ConnectivityCount <> 0 then Result := WFC_PIPELINE_CONNECTIVITY_TEXT_VERSION
  else if AModel.ValueQuotaCount = 0 then Result := WFC_PIPELINE_TEXT_VERSION
  else Result := WFC_PIPELINE_VALUE_QUOTA_TEXT_VERSION;
end;

function OpeningMask(const AOpenings: TGraphDirections): Integer;
var D: TGraphDirection;
begin
  Result := 0;
  for D := Low(TGraphDirection) to High(TGraphDirection) do
    if D in AOpenings then Result := Result or (1 shl Ord(D));
end;

function ParseOpenings(const AText: String): TGraphDirections;
var D: TGraphDirection; Mask: Integer;
begin
  Mask := ParseCanonicalInteger(AText, 'connectivity opening mask');
  if Mask > 63 then TextError('connectivity opening mask must be in 0..63');
  Result := [];
  for D := Low(TGraphDirection) to High(TGraphDirection) do
    if (Mask and (1 shl Ord(D))) <> 0 then Include(Result, D);
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
  LQuota: TWfcPipelineValueQuota;
  LConnectivity: TWfcPipelineConnectivity;
  LTextVersion: Integer;
  LTopology: TWfcPipelinePassTopology;
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create('WFC pipeline model cannot be nil');

  LMetadata := AModel.CopyMetadata;
  LVersions := AModel.CopyVersions;
  LTextVersion := WfcPipelineModelTextVersion(AModel);
  LExpectedLineCount := WFC_PIPELINE_FIXED_LINE_COUNT;
  AddLineCapacity(LExpectedLineCount, AModel.ResourceCount);
  AddLineCapacity(LExpectedLineCount, AModel.PassCount);
  AddLineCapacity(LExpectedLineCount, AModel.DependencyCount);
  AddLineCapacity(LExpectedLineCount, AModel.BridgeCount);
  AddLineCapacity(LExpectedLineCount, AModel.RequirementCount);
  for I := 0 to AModel.RequirementCount - 1 do
  begin
    LRequirement := AModel.RequirementAt(I);
    if LRequirement.Kind = wprqMapped then
    begin
      AddLineCapacity(LExpectedLineCount,1+Length(LRequirement.MappedQuery.AllowedProviderTokens));
      Continue;
    end;
    if LRequirement.Kind = wprqCount then
      AddLineCapacity(LExpectedLineCount, 1);
    AddLineCapacity(LExpectedLineCount, Length(LRequirement.Terms));
    for J := 0 to Length(LRequirement.Terms) - 1 do
      AddLineCapacity(LExpectedLineCount,
        Length(LRequirement.Terms[J].AllowedProviderTokens));
  end;
  if LTextVersion >= WFC_PIPELINE_VALUE_QUOTA_TEXT_VERSION then
  begin
    AddLineCapacity(LExpectedLineCount, 2 + AModel.ValueQuotaCount,
      WFC_PIPELINE_VALUE_QUOTA_MAX_TEXT_LINE_COUNT);
    for I := 0 to AModel.ValueQuotaCount - 1 do
    begin
      LQuota := AModel.ValueQuotaAt(I);
      AddLineCapacity(LExpectedLineCount, Length(LQuota.Values),
        WFC_PIPELINE_VALUE_QUOTA_MAX_TEXT_LINE_COUNT);
    end;
  end;
  if LTextVersion >= WFC_PIPELINE_CONNECTIVITY_TEXT_VERSION then
  begin
    AddLineCapacity(LExpectedLineCount, 2 + AModel.ConnectivityCount,
      WFC_PIPELINE_CONNECTIVITY_MAX_TEXT_LINE_COUNT);
    for I := 0 to AModel.ConnectivityCount - 1 do
    begin
      LConnectivity := AModel.ConnectivityAt(I);
      AddLineCapacity(LExpectedLineCount, Length(LConnectivity.RequiredPositions),
        WFC_PIPELINE_CONNECTIVITY_MAX_TEXT_LINE_COUNT);
      AddLineCapacity(LExpectedLineCount, Length(LConnectivity.Values),
        WFC_PIPELINE_CONNECTIVITY_MAX_TEXT_LINE_COUNT);
    end;
  end;
  if AModel.HasPattern3D then
    AddLineCapacity(LExpectedLineCount, 2, WFC_PIPELINE_PATTERN_3D_MAX_TEXT_LINE_COUNT);
  if AModel.HasPassMapping then
    AddLineCapacity(LExpectedLineCount,4+AModel.PassCount,WFC_PIPELINE_MAPPED_MAX_TEXT_LINE_COUNT);
  SetLength(LLines, LExpectedLineCount);
  LCount := 0;
  AppendLine(LLines, LCount, 'wfcpipeline=' +
    IntToStr(LTextVersion));
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
  if AModel.HasPassMapping then
    AppendLine(LLines,LCount,'pattern3d-present='+BooleanName(AModel.HasPattern3D));
  if AModel.HasPattern3D then
  begin
    AppendLine(LLines, LCount, 'pattern3d-graph-adapter-version=' +
      IntToStr(LVersions.Pattern3DGraphAdapterVersion));
    AppendLine(LLines, LCount, 'pattern3d-bridge-version=' +
      IntToStr(LVersions.Pattern3DBridgeVersion));
  end;

  AppendLine(LLines, LCount, 'rank=' + IntToStr(AModel.Rank));
  AppendLine(LLines, LCount, 'wrap=' +
    BooleanName(AModel.WrapNeighbors));
  AppendLine(LLines, LCount, 'traversal=' +
    RunModeName(AModel.RunMode));

  if AModel.HasPassMapping then
  begin
    AppendLine(LLines,LCount,'pass-mapping-version='+IntToStr(AModel.PassMappingVersion));
    AppendLine(LLines,LCount,'graph-pass-mapping-version='+IntToStr(WFC_PASS_MAPPING_VERSION));
    AppendLine(LLines,LCount,'pass-topologies='+IntToStr(AModel.PassCount));
    for I := 0 to AModel.PassCount-1 do
    begin
      LTopology:=AModel.PassTopologyAt(I);
      with LTopology do
        AppendLine(LLines,LCount,'pass-topology='+IntToStr(I)+','+IntToStr(Rank)+','+
          IntToStr(Origin.X)+','+IntToStr(Origin.Y)+','+IntToStr(Origin.Z)+','+
          IntToStr(Pitch.X)+','+IntToStr(Pitch.Y)+','+IntToStr(Pitch.Z)+','+BooleanName(Wrap));
    end;
  end;

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
      RequirementKindName(LRequirement.Kind, LRequirement.CountMode) + ',' +
      IntToStr(Length(LRequirement.Terms)));
    if LRequirement.Kind=wprqMapped then
    begin
      with LRequirement.MappedQuery do
      begin
        AppendLine(LLines,LCount,'mapped='+IntToStr(I)+','+MappedKindName(Kind)+','+MappedMatchName(Match)+','+
          IntToStr(MinimumOffset.DeltaX)+','+IntToStr(MinimumOffset.DeltaY)+','+IntToStr(MinimumOffset.DeltaZ)+','+
          IntToStr(MaximumOffset.DeltaX)+','+IntToStr(MaximumOffset.DeltaY)+','+IntToStr(MaximumOffset.DeltaZ)+','+
          IntToStr(MinimumMatches)+','+IntToStr(MaximumMatches)+','+IntToStr(Length(AllowedProviderTokens)));
        for J := 0 to High(AllowedProviderTokens) do
          AppendLine(LLines,LCount,'mapped-token='+IntToStr(I)+','+IntToStr(J)+','+EncodeToken(AllowedProviderTokens[J]));
      end;
      Continue;
    end;
    if LRequirement.Kind = wprqCount then
      AppendLine(LLines, LCount, 'count=' + IntToStr(I) + ',' +
        IntToStr(LRequirement.MinimumCount) + ',' +
        IntToStr(LRequirement.MaximumCount));
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

  if LTextVersion >= WFC_PIPELINE_VALUE_QUOTA_TEXT_VERSION then
  begin
    AppendLine(LLines, LCount, 'value-quota-version=' + IntToStr(AModel.ValueQuotaVersion));
    AppendLine(LLines, LCount, 'value-quotas=' + IntToStr(AModel.ValueQuotaCount));
    for I := 0 to AModel.ValueQuotaCount - 1 do
    begin
      LQuota := AModel.ValueQuotaAt(I);
      AppendLine(LLines, LCount, 'value-quota=' + IntToStr(I) + ',' +
        IntToStr(LQuota.PassIndex) + ',' + EncodeToken(LQuota.LabelText) + ',' +
        IntToStr(LQuota.MinimumCount) + ',' + IntToStr(LQuota.MaximumCount) + ',' +
        IntToStr(Length(LQuota.Values)));
      for J := 0 to High(LQuota.Values) do
        AppendLine(LLines, LCount, 'quota-token=' + IntToStr(I) + ',' +
          IntToStr(J) + ',' + EncodeToken(LQuota.Values[J]));
    end;
  end;

  if LTextVersion >= WFC_PIPELINE_CONNECTIVITY_TEXT_VERSION then
  begin
    AppendLine(LLines, LCount, 'connectivity-version=' + IntToStr(AModel.ConnectivityVersion));
    AppendLine(LLines, LCount, 'connectivities=' + IntToStr(AModel.ConnectivityCount));
    for I := 0 to AModel.ConnectivityCount - 1 do
    begin
      LConnectivity := AModel.ConnectivityAt(I);
      AppendLine(LLines, LCount, 'connectivity=' + IntToStr(I) + ',' +
        IntToStr(LConnectivity.PassIndex) + ',' + EncodeToken(LConnectivity.LabelText) + ',' +
        IntToStr(LConnectivity.Root.X) + ',' + IntToStr(LConnectivity.Root.Y) + ',' +
        IntToStr(LConnectivity.Root.Z) + ',' + BooleanName(LConnectivity.RequireAllParticipants) + ',' +
        IntToStr(Length(LConnectivity.RequiredPositions)) + ',' + IntToStr(Length(LConnectivity.Values)));
      for J := 0 to High(LConnectivity.RequiredPositions) do
        with LConnectivity.RequiredPositions[J] do
          AppendLine(LLines, LCount, 'terminal=' + IntToStr(I) + ',' + IntToStr(J) + ',' +
            IntToStr(X) + ',' + IntToStr(Y) + ',' + IntToStr(Z));
      for J := 0 to High(LConnectivity.Values) do
        with LConnectivity.Values[J] do
          AppendLine(LLines, LCount, 'profile=' + IntToStr(I) + ',' + IntToStr(J) + ',' +
            EncodeToken(Value) + ',' + IntToStr(OpeningMask(Openings)) + ',' +
            BooleanName(RequiredByValue));
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
  LCountMaximum: Integer;
  LCountMinimum: Integer;
  LCountMode: TGraphPassCountMode;
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
  LTextVersion: Integer;
  LQuotaTail: Integer;
  LConnectivityTail: Integer;
  LQuotaVersion: Integer;
  LConnectivityVersion: Integer;
  LQuotaCount: Integer;
  LQuotaTokenCount: Integer;
  LTotalQuotaTokenCount: Integer;
  LQuotaPassIndex: Integer;
  LQuotaMinimum: Integer;
  LQuotaMaximum: Integer;
  LQuotaLabel: TWfcModelToken;
  LQuotaTokens: TWfcModelTokens;
  LQuotas: TWfcPipelineValueQuotas;
  LConnectivityCount, LTerminalCount, LProfileCount: Integer;
  LTotalTerminalCount, LTotalProfileCount: Integer;
  LConnectivity: TWfcPipelineConnectivity;
  LConnectivities: TWfcPipelineConnectivities;
  LHasPattern3D: Boolean;
  LMappingVersion,LTopologyCount: Integer;
  LTopologies: TWfcPipelinePassTopologies;
  LTopology: TWfcPipelinePassTopology;
  LMappedQuery: TWfcPipelineMappedQuery;

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

  function ReadQuotaRecord(const APrefix: String; const AFields: Integer;
    const AName: String): TWfcPipelineTextFields;
  var LLine: String;
  begin
    LLine := RequireLine(LLines, LLineIndex, AName);
    { A quota row contains one escaped outer token and a fixed number of
      bounded decimal fields. Reject an oversized row BEFORE splitting it
      into copied field strings. Nested resource rows have their own limits. }
    if Length(LLine) > WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH + 128 then
      TextError(AName + ' exceeds the encoded row-length limit');
    Inc(LLineIndex);
    Result := SplitRecord(ValueAfterPrefix(LLine, APrefix, AName), AFields, AName);
  end;

  function ReadCoordinate(const AText, AFieldName: String;
    const AAxis: Integer): TGraphCoordinate;
  var LValue: Integer;
  begin
    LValue := ParseCanonicalInteger(AText, AFieldName);
    if LTextVersion = WFC_PIPELINE_MAPPED_TEXT_VERSION then
    begin
      if (AAxis >= LTopologies[LConnectivity.PassIndex].Rank) and (LValue <> 0) then
        TextError(AFieldName + ' lies outside the owner pass rank');
    end
    else if (AAxis >= LRank) and (LValue <> 0) then
      TextError(AFieldName + ' lies outside the recipe rank');
    Result := TGraphCoordinate(LValue);
  end;
begin
  Result := nil;
  PreflightTextEnvelope(AText, LTextVersion);
  WfcTextSplitCanonicalLines(AText, WFC_PIPELINE_TEXT_ARTIFACT,
    LLines);
  if Length(LLines) < WFC_PIPELINE_FIXED_LINE_COUNT then
    TextError('document is incomplete');
  LLineIndex := 0;
  if RequireLine(LLines, LLineIndex, 'format version') <>
      'wfcpipeline=' + IntToStr(LTextVersion) then
    TextError('unsupported or noncanonical format version');
  Inc(LLineIndex);
  LQuotas := nil;
  LConnectivities := nil;
  LTopologies := nil; LMappingVersion := 0;
  LHasPattern3D := LTextVersion = WFC_PIPELINE_PATTERN_3D_TEXT_VERSION;
  { A canonical V2 section needs a version, count, one descriptor and at
    least one token. Reserve this minimum before allocating older sections. }
  if LTextVersion = WFC_PIPELINE_VALUE_QUOTA_TEXT_VERSION then LQuotaTail := 4
  else LQuotaTail := 0;
  LConnectivityTail := 0;
  if LTextVersion = WFC_PIPELINE_CONNECTIVITY_TEXT_VERSION then
  begin
    { V3 has a possibly empty two-line quota section followed by a nonempty
      connectivity section: version, count, descriptor and one profile. }
    LConnectivityTail := 4;
    LQuotaTail := 2 + LConnectivityTail;
  end;
  if LTextVersion >= WFC_PIPELINE_PATTERN_3D_TEXT_VERSION then
  begin
    { Both optional registries still have explicit zero-version/zero-count
      sections in V4. The two new version lines precede all registries. }
    LConnectivityTail := 2;
    LQuotaTail := 2 + LConnectivityTail;
  end;

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

  LVersions := CurrentWfcPipelineVersions;
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
  if LTextVersion = WFC_PIPELINE_MAPPED_TEXT_VERSION then
    LHasPattern3D := ParseBooleanName(ReadValueLine(LLines,LLineIndex,
      'pattern3d-present=','pattern3d presence'),'pattern3d presence');
  if LHasPattern3D then
  begin
    LVersions.Pattern3DGraphAdapterVersion := ParseCanonicalInteger(
      ReadValueLine(LLines, LLineIndex, 'pattern3d-graph-adapter-version=',
        'pattern3d graph-adapter version'), 'pattern3d graph-adapter version');
    LVersions.Pattern3DBridgeVersion := ParseCanonicalInteger(
      ReadValueLine(LLines, LLineIndex, 'pattern3d-bridge-version=',
        'pattern3d bridge version'), 'pattern3d bridge version');
  end;

  LRank := ParseCanonicalInteger(ReadValueLine(LLines, LLineIndex,
    'rank=', 'rank'), 'rank');
  LWrapNeighbors := ParseBooleanName(ReadValueLine(LLines,
    LLineIndex, 'wrap=', 'wrap policy'), 'wrap policy');
  LRunMode := ParseRunMode(ReadValueLine(LLines, LLineIndex,
    'traversal=', 'traversal mode'));

  if LTextVersion = WFC_PIPELINE_MAPPED_TEXT_VERSION then
  begin
    LMappingVersion := ParseCanonicalInteger(ReadValueLine(LLines,LLineIndex,
      'pass-mapping-version=','pass-mapping version'),'pass-mapping version');
    if LMappingVersion<>WFC_PIPELINE_PASS_MAPPING_VERSION then TextError('unsupported pass-mapping version');
    if ParseCanonicalInteger(ReadValueLine(LLines,LLineIndex,
      'graph-pass-mapping-version=','graph pass-mapping version'),'graph pass-mapping version')<>WFC_PASS_MAPPING_VERSION then
      TextError('unsupported graph pass-mapping version');
    LTopologyCount := ParseBoundedCount(ReadValueLine(LLines,LLineIndex,
      'pass-topologies=','pass topology count'),'pass topology count',WFC_PIPELINE_MAX_PASS_COUNT);
    if LTopologyCount=0 then TextError('spatial recipe requires pass topologies');
    RequireRecordCapacity(LTopologyCount,7+LQuotaTail,LLineIndex,LLines,'pass topology');
    SetLength(LTopologies,LTopologyCount);
    for I := 0 to LTopologyCount-1 do
    begin
      LFields:=ReadQuotaRecord('pass-topology=',9,'pass topology');
      if ParseCanonicalInteger(LFields[0],'topology index')<>I then TextError('pass topology indices must be complete and ordered');
      LTopology.Rank:=ParseCanonicalInteger(LFields[1],'pass rank');
      LTopology.Origin.X:=ParseCanonicalSignedInteger(LFields[2],'pass origin X');
      LTopology.Origin.Y:=ParseCanonicalSignedInteger(LFields[3],'pass origin Y');
      LTopology.Origin.Z:=ParseCanonicalSignedInteger(LFields[4],'pass origin Z');
      LTopology.Pitch.X:=ParseCanonicalInteger(LFields[5],'pass pitch X');
      LTopology.Pitch.Y:=ParseCanonicalInteger(LFields[6],'pass pitch Y');
      LTopology.Pitch.Z:=ParseCanonicalInteger(LFields[7],'pass pitch Z');
      LTopology.Wrap:=ParseBooleanName(LFields[8],'pass wrap');
      try
        LTopologies[I]:=MakeWfcPipelinePassTopology(LTopology.Rank,LTopology.Origin,LTopology.Pitch,LTopology.Wrap);
      except on E:Exception do TextError(E.Message); end;
    end;
  end;

  LResourceCount := ParseBoundedCount(ReadValueLine(LLines,
    LLineIndex, 'resources=', 'resource count'), 'resource count',
    WFC_PIPELINE_MAX_RESOURCE_COUNT);
  RequireRecordCapacity(LResourceCount, 6 + LQuotaTail, LLineIndex, LLines,
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
  if (LTextVersion=WFC_PIPELINE_MAPPED_TEXT_VERSION) and (LPassCount<>Length(LTopologies)) then
    TextError('pass topology count must equal pass count');
  RequireRecordCapacity(LPassCount, 5 + LQuotaTail, LLineIndex, LLines, 'pass');
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
  RequireRecordCapacity(LDependencyCount, 4 + LQuotaTail, LLineIndex, LLines,
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
  RequireRecordCapacity(LBridgeCount, 3 + LQuotaTail, LLineIndex, LLines, 'bridge');
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
  RequireRecordCapacity(LRequirementCount, 2 + LQuotaTail, LLineIndex, LLines,
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
    ParseRequirementKind(LFields[4], LRequirementKind, LCountMode);
    LTermCount := ParseBoundedCount(LFields[5],
      'requirement term count', WFC_PIPELINE_MAX_REQUIREMENT_TERM_COUNT);
    if LRequirementKind=wprqMapped then
    begin
      if LTextVersion<>WFC_PIPELINE_MAPPED_TEXT_VERSION then TextError('mapped clauses require recipe version 5');
      if LTermCount<>0 then TextError('mapped requirements cannot contain legacy terms');
      LFields:=ReadQuotaRecord('mapped=',12,'mapped query');
      if ParseCanonicalInteger(LFields[0],'mapped parent index')<>I then TextError('mapped query parent index is incorrect');
      LMappedQuery:=Default(TWfcPipelineMappedQuery);
      LMappedQuery.Kind:=ParseMappedKind(LFields[1]); LMappedQuery.Match:=ParseMappedMatch(LFields[2]);
      LMappedQuery.MinimumOffset.DeltaX:=ParseCanonicalSignedInteger(LFields[3],'mapped minimum X');
      LMappedQuery.MinimumOffset.DeltaY:=ParseCanonicalSignedInteger(LFields[4],'mapped minimum Y');
      LMappedQuery.MinimumOffset.DeltaZ:=ParseCanonicalSignedInteger(LFields[5],'mapped minimum Z');
      LMappedQuery.MaximumOffset.DeltaX:=ParseCanonicalSignedInteger(LFields[6],'mapped maximum X');
      LMappedQuery.MaximumOffset.DeltaY:=ParseCanonicalSignedInteger(LFields[7],'mapped maximum Y');
      LMappedQuery.MaximumOffset.DeltaZ:=ParseCanonicalSignedInteger(LFields[8],'mapped maximum Z');
      LMappedQuery.MinimumMatches:=ParseCanonicalInteger(LFields[9],'mapped minimum matches');
      LMappedQuery.MaximumMatches:=ParseCanonicalInteger(LFields[10],'mapped maximum matches');
      LAllowedCount:=ParseBoundedCount(LFields[11],'mapped allowed-token count',WFC_PIPELINE_MAX_ALLOWED_TOKEN_COUNT);
      if LAllowedCount=0 then TextError('mapped query must allow at least one token');
      if LTotalAllowedTokenCount>WFC_PIPELINE_MAX_TOTAL_ALLOWED_TOKEN_COUNT-LAllowedCount then
        TextError('aggregate mapped allowed-token count exceeds the limit');
      Inc(LTotalAllowedTokenCount,LAllowedCount);
      RequireRecordCapacity(LAllowedCount,2+LQuotaTail,LLineIndex,LLines,'mapped token');
      SetLength(LMappedQuery.AllowedProviderTokens,LAllowedCount);
      for J := 0 to LAllowedCount-1 do
      begin
        LFields:=ReadQuotaRecord('mapped-token=',3,'mapped token');
        if (ParseCanonicalInteger(LFields[0],'mapped token parent index')<>I) or
          (ParseCanonicalInteger(LFields[1],'mapped token index')<>J) then TextError('mapped token indices must be complete and ordered');
        LMappedQuery.AllowedProviderTokens[J]:=DecodeOuterToken(LFields[2],'mapped provider token');
      end;
      try
        LRequirements[I]:=MakeWfcPipelineMappedRequirement(LRequirementConsumerIndex,
          LRequirementConsumerToken,LRequirementProviderIndex,LMappedQuery);
      except on E:Exception do TextError(E.Message); end;
      Continue;
    end;
    if LTotalRequirementTermCount >
        WFC_PIPELINE_MAX_TOTAL_REQUIREMENT_TERM_COUNT - LTermCount then
      TextError('aggregate requirement-term count exceeds the version-1 limit');
    Inc(LTotalRequirementTermCount, LTermCount);
    RequireRecordCapacity(LTermCount + Ord(LRequirementKind = wprqCount),
      2 + LQuotaTail, LLineIndex, LLines,
      'requirement term');
    LCountMinimum := 0;
    LCountMaximum := 0;
    if LRequirementKind = wprqCount then
    begin
      LFields := SplitRecord(ReadValueLine(LLines, LLineIndex,
        'count=', 'requirement count record'), 3,
        'requirement count');
      if ParseCanonicalInteger(LFields[0],
          'requirement count parent index') <> I then
        TextError('requirement count parent index is incorrect');
      LCountMinimum := ParseCanonicalInteger(LFields[1],
        'requirement minimum count');
      LCountMaximum := ParseCanonicalInteger(LFields[2],
        'requirement maximum count');
    end;
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
      RequireRecordCapacity(LAllowedCount, 2 + LQuotaTail, LLineIndex, LLines,
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
    if LRequirementKind = wprqCount then
      LRequirements[I] := MakeWfcPipelineCountRequirement(
        LRequirementConsumerIndex, LRequirementConsumerToken,
        LRequirementProviderIndex, LTerms, LCountMinimum,
        LCountMaximum, LCountMode)
    else
      LRequirements[I] := MakeWfcPipelineRequirement(
        LRequirementConsumerIndex, LRequirementConsumerToken,
        LRequirementProviderIndex, LRequirementKind, LTerms);
  end;

  if LTextVersion >= WFC_PIPELINE_VALUE_QUOTA_TEXT_VERSION then
  begin
    LQuotaVersion := ParseCanonicalInteger(ReadValueLine(LLines, LLineIndex,
        'value-quota-version=', 'value-quota version'), 'value-quota version');
    if (LQuotaVersion <> WFC_PIPELINE_VALUE_QUOTA_VERSION) and
        not ((LTextVersion >= WFC_PIPELINE_CONNECTIVITY_TEXT_VERSION) and (LQuotaVersion = 0)) then
      TextError('unsupported value-quota version');
    LQuotaCount := ParseBoundedCount(ReadValueLine(LLines, LLineIndex,
      'value-quotas=', 'value-quota count'), 'value-quota count',
      WFC_PIPELINE_MAX_VALUE_QUOTA_COUNT);
    if (LTextVersion = WFC_PIPELINE_VALUE_QUOTA_TEXT_VERSION) and (LQuotaCount = 0) then
      TextError('version 2 requires at least one value quota');
    if ((LQuotaCount = 0) and (LQuotaVersion <> 0)) or
        ((LQuotaCount <> 0) and (LQuotaVersion = 0)) then
      TextError('value-quota version must be zero exactly when the registry is empty');
    { Every quota needs a descriptor AND a token. The cap makes this product
      safe; the remaining-line check precedes the descriptor array allocation. }
    RequireRecordCapacity(2 * LQuotaCount, 2 + LConnectivityTail,
      LLineIndex, LLines, 'value-quota');
    SetLength(LQuotas, LQuotaCount);
    LTotalQuotaTokenCount := 0;
    for I := 0 to LQuotaCount - 1 do
    begin
      LFields := ReadQuotaRecord('value-quota=', 6, 'value-quota record');
      if ParseCanonicalInteger(LFields[0], 'value-quota index') <> I then
        TextError('value-quota indices must be complete and ordered');
      LQuotaPassIndex := ParseCanonicalInteger(LFields[1], 'value-quota pass index');
      if LQuotaPassIndex >= LPassCount then TextError('value-quota pass index is outside the recipe');
      LQuotaLabel := DecodeOuterToken(LFields[2], 'value-quota label');
      if LQuotaLabel = '' then TextError('value-quota label cannot be empty');
      LQuotaMinimum := ParseCanonicalInteger(LFields[3], 'value-quota minimum');
      LQuotaMaximum := ParseCanonicalInteger(LFields[4], 'value-quota maximum');
      if LQuotaMinimum > LQuotaMaximum then TextError('value-quota minimum exceeds maximum');
      LQuotaTokenCount := ParseBoundedCount(LFields[5], 'value-quota token count',
        WFC_PIPELINE_MAX_VALUE_QUOTA_TOKEN_COUNT);
      if LQuotaTokenCount = 0 then TextError('value-quota token set cannot be empty');
      if LTotalQuotaTokenCount > WFC_PIPELINE_MAX_TOTAL_VALUE_QUOTA_TOKEN_COUNT - LQuotaTokenCount then
        TextError('aggregate value-quota token count exceeds the limit');
      Inc(LTotalQuotaTokenCount, LQuotaTokenCount);
      RequireRecordCapacity(LQuotaTokenCount, 2 + LConnectivityTail + 2 * (LQuotaCount - I - 1),
        LLineIndex, LLines, 'value-quota token');
      SetLength(LQuotaTokens, LQuotaTokenCount);
      for J := 0 to LQuotaTokenCount - 1 do
      begin
        LFields := ReadQuotaRecord('quota-token=', 3, 'value-quota token record');
        if (ParseCanonicalInteger(LFields[0], 'value-quota token parent index') <> I) or
            (ParseCanonicalInteger(LFields[1], 'value-quota token index') <> J) then
          TextError('value-quota token indices must be complete and ordered');
        LQuotaTokens[J] := DecodeOuterToken(LFields[2], 'value-quota token');
        if LQuotaTokens[J] = '' then TextError('value-quota token cannot be empty');
      end;
      LQuotas[I] := MakeWfcPipelineValueQuota(LQuotaPassIndex, LQuotaLabel,
        LQuotaTokens, LQuotaMinimum, LQuotaMaximum);
    end;
  end;

  if LTextVersion >= WFC_PIPELINE_CONNECTIVITY_TEXT_VERSION then
  begin
    LConnectivityVersion := ParseCanonicalInteger(ReadValueLine(LLines, LLineIndex,
      'connectivity-version=', 'connectivity version'), 'connectivity version');
    if (LConnectivityVersion <> WFC_PIPELINE_CONNECTIVITY_VERSION) and
        not ((LTextVersion >= WFC_PIPELINE_PATTERN_3D_TEXT_VERSION) and (LConnectivityVersion = 0)) then
      TextError('unsupported connectivity version');
    LConnectivityCount := ParseBoundedCount(ReadValueLine(LLines, LLineIndex,
      'connectivities=', 'connectivity count'), 'connectivity count',
      WFC_PIPELINE_MAX_CONNECTIVITY_COUNT);
    if (LTextVersion = WFC_PIPELINE_CONNECTIVITY_TEXT_VERSION) and (LConnectivityCount = 0) then
      TextError('version 3 requires at least one connectivity');
    if ((LConnectivityCount = 0) and (LConnectivityVersion <> 0)) or
        ((LConnectivityCount <> 0) and (LConnectivityVersion = 0)) then
      TextError('connectivity version must be zero exactly when the registry is empty');
    RequireRecordCapacity(2 * LConnectivityCount, 2, LLineIndex, LLines, 'connectivity');
    SetLength(LConnectivities, LConnectivityCount);
    LTotalTerminalCount := 0;
    LTotalProfileCount := 0;
    for I := 0 to LConnectivityCount - 1 do
    begin
      LFields := ReadQuotaRecord('connectivity=', 9, 'connectivity record');
      if ParseCanonicalInteger(LFields[0], 'connectivity index') <> I then
        TextError('connectivity indices must be complete and ordered');
      LConnectivity.PassIndex := ParseCanonicalInteger(LFields[1], 'connectivity pass index');
      if LConnectivity.PassIndex >= LPassCount then
        TextError('connectivity pass index is outside the recipe');
      LConnectivity.LabelText := DecodeOuterToken(LFields[2], 'connectivity label');
      if LConnectivity.LabelText = '' then TextError('connectivity label cannot be empty');
      LConnectivity.Root.X := ReadCoordinate(LFields[3], 'connectivity root X', 0);
      LConnectivity.Root.Y := ReadCoordinate(LFields[4], 'connectivity root Y', 1);
      LConnectivity.Root.Z := ReadCoordinate(LFields[5], 'connectivity root Z', 2);
      LConnectivity.RequireAllParticipants := ParseBooleanName(LFields[6], 'connectivity all-participants');
      LTerminalCount := ParseBoundedCount(LFields[7], 'connectivity terminal count',
        WFC_PIPELINE_MAX_CONNECTIVITY_REQUIRED_POSITION_COUNT);
      LProfileCount := ParseBoundedCount(LFields[8], 'connectivity profile count',
        WFC_PIPELINE_MAX_CONNECTIVITY_VALUE_COUNT);
      if LProfileCount = 0 then TextError('connectivity profiles cannot be empty');
      if LTotalTerminalCount > WFC_PIPELINE_MAX_TOTAL_CONNECTIVITY_REQUIRED_POSITION_COUNT - LTerminalCount then
        TextError('aggregate connectivity terminal count exceeds the limit');
      if LTotalProfileCount > WFC_PIPELINE_MAX_TOTAL_CONNECTIVITY_VALUE_COUNT - LProfileCount then
        TextError('aggregate connectivity profile count exceeds the limit');
      Inc(LTotalTerminalCount, LTerminalCount);
      Inc(LTotalProfileCount, LProfileCount);
      RequireRecordCapacity(LTerminalCount + LProfileCount,
        2 + 2 * (LConnectivityCount - I - 1), LLineIndex, LLines, 'connectivity child');
      { Release the previous iteration's arrays before sizing new children. }
      LConnectivity.RequiredPositions := nil;
      LConnectivity.Values := nil;
      SetLength(LConnectivity.RequiredPositions, LTerminalCount);
      for J := 0 to LTerminalCount - 1 do
      begin
        LFields := ReadQuotaRecord('terminal=', 5, 'connectivity terminal record');
        if (ParseCanonicalInteger(LFields[0], 'terminal parent index') <> I) or
            (ParseCanonicalInteger(LFields[1], 'terminal index') <> J) then
          TextError('connectivity terminal indices must be complete and ordered');
        LConnectivity.RequiredPositions[J].X := ReadCoordinate(LFields[2], 'terminal X', 0);
        LConnectivity.RequiredPositions[J].Y := ReadCoordinate(LFields[3], 'terminal Y', 1);
        LConnectivity.RequiredPositions[J].Z := ReadCoordinate(LFields[4], 'terminal Z', 2);
      end;
      SetLength(LConnectivity.Values, LProfileCount);
      for J := 0 to LProfileCount - 1 do
      begin
        LFields := ReadQuotaRecord('profile=', 5, 'connectivity profile record');
        if (ParseCanonicalInteger(LFields[0], 'profile parent index') <> I) or
            (ParseCanonicalInteger(LFields[1], 'profile index') <> J) then
          TextError('connectivity profile indices must be complete and ordered');
        LConnectivity.Values[J].Value := DecodeOuterToken(LFields[2], 'connectivity profile token');
        if LConnectivity.Values[J].Value = '' then TextError('connectivity profile token cannot be empty');
        LConnectivity.Values[J].Openings := ParseOpenings(LFields[3]);
        LConnectivity.Values[J].RequiredByValue := ParseBooleanName(LFields[4], 'connectivity required-by-value');
      end;
      LConnectivities[I] := LConnectivity;
    end;
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
      if LTextVersion=WFC_PIPELINE_MAPPED_TEXT_VERSION then
        LModel:=TWfcPipelineModel.Create(LMetadata,LVersions,LRank,LWrapNeighbors,LRunMode,
          LResources,LPasses,LDependencies,LBridges,LRequirements,LQuotas,LConnectivities,
          LMappingVersion,LTopologies)
      else LModel := TWfcPipelineModel.Create(LMetadata, LVersions,
        LRank, LWrapNeighbors, LRunMode, LResources, LPasses,
        LDependencies, LBridges, LRequirements, LQuotas, LConnectivities);
    except
      on E: EWfcPipelineModel do
        TextError(E.Message);
    end;
    if LHasPattern3D <> LModel.HasPattern3D then
    begin
      if LTextVersion = WFC_PIPELINE_MAPPED_TEXT_VERSION then
        TextError('pattern3d capability presence must match the recipe features')
      else
        TextError('version 4 is required exactly for pattern3d recipes');
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
