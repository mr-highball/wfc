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
unit wfc_pipeline_model;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_rule_model,
  wfc_pattern2d,
  wfc_pattern3d,
  wfc_sequence;

const
  WFC_PIPELINE_MODEL_VERSION = 1;
  WFC_PIPELINE_MODEL_SIGNATURE_VERSION = 1;
  WFC_PIPELINE_GRAPH_ADAPTER_VERSION = 1;
  { Opt-in whole-pass public-token quotas. The legacy model/adapter versions,
    version record, and all quota-free identities remain unchanged. }
  WFC_PIPELINE_VALUE_QUOTA_VERSION = 1;
  WFC_PIPELINE_CONNECTIVITY_VERSION = 1;
  { Bridge version 2 adds deterministic inverse lowering of public run inputs
    into the private source pass. Version 1 remains accepted as the portable
    forward-only contract. }
  WFC_PIPELINE_PATTERN_BRIDGE_VERSION = 2;
  WFC_PIPELINE_SEQUENCE_BRIDGE_VERSION = 2;
  WFC_PIPELINE_PATTERN_3D_BRIDGE_VERSION = 1;

  WFC_PIPELINE_NO_INDEX = -1;

  { These are portability and denial-of-service boundaries, not hints. They
    are intentionally expressed as fixed Integer constants on both FPC and
    pas2js. A constructor checks resource lengths before allocating owned
    resource storage or invoking a nested decoder. }
  WFC_PIPELINE_MAX_RESOURCE_COUNT = 64;
  WFC_PIPELINE_MAX_RESOURCE_PAYLOAD_LENGTH = 16777216;
  WFC_PIPELINE_MAX_TOTAL_RESOURCE_PAYLOAD_LENGTH = 67108864;
  WFC_PIPELINE_MAX_PASS_COUNT = 256;
  WFC_PIPELINE_MAX_DEPENDENCY_COUNT = 4096;
  WFC_PIPELINE_MAX_BRIDGE_COUNT = 256;
  WFC_PIPELINE_MAX_REQUIREMENT_COUNT = 4096;
  WFC_PIPELINE_MAX_REQUIREMENT_TERM_COUNT = 256;
  WFC_PIPELINE_MAX_ALLOWED_TOKEN_COUNT = 1024;
  WFC_PIPELINE_MAX_TOTAL_REQUIREMENT_TERM_COUNT = 8192;
  WFC_PIPELINE_MAX_TOTAL_ALLOWED_TOKEN_COUNT = 65536;
  WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH = 1048576;
  WFC_PIPELINE_MAX_TOTAL_ENCODED_TOKEN_LENGTH = 16777216;
  WFC_PIPELINE_MAX_TOTAL_RESOURCE_RELATION_SLOT_COUNT = 16777216;
  WFC_PIPELINE_MAX_VALUE_QUOTA_COUNT = 4096;
  WFC_PIPELINE_MAX_VALUE_QUOTA_TOKEN_COUNT = 1024;
  WFC_PIPELINE_MAX_TOTAL_VALUE_QUOTA_TOKEN_COUNT = 65536;
  WFC_PIPELINE_MAX_CONNECTIVITY_COUNT = 4096;
  WFC_PIPELINE_MAX_CONNECTIVITY_VALUE_COUNT = 1024;
  WFC_PIPELINE_MAX_TOTAL_CONNECTIVITY_VALUE_COUNT = 65536;
  WFC_PIPELINE_MAX_CONNECTIVITY_REQUIRED_POSITION_COUNT = 65536;
  WFC_PIPELINE_MAX_TOTAL_CONNECTIVITY_REQUIRED_POSITION_COUNT = 65536;

type
  EWfcPipelineModel = class(Exception);

  { A portable FNV-1a semantic identity. It detects accidental differences;
    it is explicitly not a security or authenticity primitive. }
  TWfcPipelineSignature = Cardinal;

  TWfcPipelineResourceKind = (
    wprkModel,
    wprkRules,
    wprkPattern2D,
    wprkSequence,
    wprkPattern3D
  );

  TWfcPipelinePassVisibility = (
    wppvPrivate,
    wppvPublic
  );

  TWfcPipelineAdapterKind = (
    wpakEmpty,
    wpakModel,
    wpakRules,
    wpakPattern2D,
    wpakSequence,
    wpakPattern3D
  );

  TWfcPipelineBridgeKind = (
    wpbkPattern2DProjection,
    wpbkSequenceProjection,
    wpbkPattern3DProjection
  );

  TWfcPipelineRequirementKind = (
    wprqExact,
    wprqAny,
    wprqCount
  );

  TWfcPipelineVersions = record
    GraphModelVersion: Integer;
    RandomAlgorithmVersion: Integer;
    SolverAlgorithmVersion: Integer;
    PipelineAlgorithmVersion: Integer;
    BundleGraphAdapterVersion: Integer;
    ModelGraphAdapterVersion: Integer;
    RulesGraphAdapterVersion: Integer;
    Pattern2DGraphAdapterVersion: Integer;
    SequenceGraphAdapterVersion: Integer;
    Pattern2DBridgeVersion: Integer;
    SequenceBridgeVersion: Integer;
    { Read only for recipes that use a 3D overlapping resource/adapter/bridge.
      Legacy callers may still initialize only the original eleven fields. }
    Pattern3DGraphAdapterVersion: Integer;
    Pattern3DBridgeVersion: Integer;
  end;

  TWfcPipelineMetadata = record
    Name: TWfcModelToken;
    LicenseIdentifier: TWfcModelToken;
    SourceDescription: TWfcModelToken;
    SourceFingerprint: TWfcModelToken;
  end;

  TWfcPipelineResource = record
    Id: TWfcModelToken;
    Kind: TWfcPipelineResourceKind;
    Document: String;
    SourceDescription: TWfcModelToken;
    SourceLicenseIdentifier: TWfcModelToken;
    SourceFingerprint: TWfcModelToken;
  end;
  TWfcPipelineResources = array of TWfcPipelineResource;

  TWfcPipelinePass = record
    LabelName: TWfcModelToken;
    Visibility: TWfcPipelinePassVisibility;
    Mode: TGraphPassMode;
    TransformSourceIndex: Integer;
    AdapterKind: TWfcPipelineAdapterKind;
    ResourceIndex: Integer;
    HasSequenceExtent: Boolean;
    SequenceExtent: TWfcSequenceExtent;
  end;
  TWfcPipelinePasses = array of TWfcPipelinePass;

  TWfcPipelineDependency = record
    ConsumerPassIndex: Integer;
    ProviderPassIndex: Integer;
  end;
  TWfcPipelineDependencies = array of TWfcPipelineDependency;

  TWfcPipelineBridge = record
    Kind: TWfcPipelineBridgeKind;
    SourcePassIndex: Integer;
    TargetPassIndex: Integer;
  end;
  TWfcPipelineBridges = array of TWfcPipelineBridge;

  TWfcPipelineRequirementTerm = record
    OffsetX: Integer;
    OffsetY: Integer;
    OffsetZ: Integer;
    AllowedProviderTokens: TWfcModelTokens;
  end;
  TWfcPipelineRequirementTerms = array of TWfcPipelineRequirementTerm;

  TWfcPipelineRequirement = record
    ConsumerPassIndex: Integer;
    ConsumerToken: TWfcModelToken;
    ProviderPassIndex: Integer;
    Kind: TWfcPipelineRequirementKind;
    CountMode: TGraphPassCountMode;
    MinimumCount: Integer;
    MaximumCount: Integer;
    Terms: TWfcPipelineRequirementTerms;
  end;
  TWfcPipelineRequirements = array of TWfcPipelineRequirement;

  { Values is a nonempty set in the target's strict public-vocabulary order.
    Whole-pass bounds are independent of invocation shape. Public transform
    aliases retain their own declaration/diagnostic identity; the compiler
    lowers their quota through the exact-copy chain, never materializing a
    rule definition on the alias. }
  TWfcPipelineValueQuota = record
    PassIndex: Integer;
    LabelText: TWfcModelToken;
    Values: TWfcModelTokens;
    MinimumCount: Integer;
    MaximumCount: Integer;
  end;
  TWfcPipelineValueQuotas = array of TWfcPipelineValueQuota;

  { Portable public tokens, not native graph-string encodings. All six port
    directions are meaningful profile data, including on lower-rank grids. }
  TWfcPipelineConnectivityValue = record
    Value: TWfcModelToken;
    Openings: TGraphDirections;
    RequiredByValue: Boolean;
  end;
  TWfcPipelineConnectivityValues = array of TWfcPipelineConnectivityValue;
  { Profiles follow strict public-vocabulary order. Explicit terminals follow
    strict (Z,Y,X) order and exclude the already-required root. Coordinates
    are exact nonnegative Integers; invocation bounds are checked at compile. }
  TWfcPipelineConnectivity = record
    PassIndex: Integer;
    LabelText: TWfcModelToken;
    Root: TGraphPosition;
    RequiredPositions: TGraphPositions;
    Values: TWfcPipelineConnectivityValues;
    RequireAllParticipants: Boolean;
  end;
  TWfcPipelineConnectivities = array of TWfcPipelineConnectivity;

  { TWfcPipelineModel }

  (*
    Immutable declarative pipeline recipe. It owns the strictly decoded typed
    resources. Borrow*Resource accessors never transfer that ownership and
    their result must not outlive this object. All record/array accessors return
    detached copies, including nested requirement terms and token arrays.
  *)
  TWfcPipelineModel = class
  strict private
    FMetadata: TWfcPipelineMetadata;
    FVersions: TWfcPipelineVersions;
    FRank: Integer;
    FWrapNeighbors: Boolean;
    FRunMode: TGraphRunMode;
    FResources: TWfcPipelineResources;
    FPasses: TWfcPipelinePasses;
    FDependencies: TWfcPipelineDependencies;
    FBridges: TWfcPipelineBridges;
    FRequirements: TWfcPipelineRequirements;
    FValueQuotas: TWfcPipelineValueQuotas;
    FConnectivities: TWfcPipelineConnectivities;
    FVocabularies: array of TWfcModelTokens;
    FModelResources: array of TWfcModel;
    FRuleResources: array of TWfcRuleModel;
    FPatternResources: array of TWfcOverlappingModel2D;
    FSequenceResources: array of TWfcSequenceModel;
    FPattern3DResources: array of TWfcOverlappingModel3D;
    FHasPattern3D: Boolean;
    FSignature: TWfcPipelineSignature;

    function GetResourceCount: Integer;
    function GetPassCount: Integer;
    function GetDependencyCount: Integer;
    function GetBridgeCount: Integer;
    function GetRequirementCount: Integer;
    function GetValueQuotaCount: Integer;
    function GetValueQuotaVersion: Integer;
    function GetConnectivityCount: Integer;
    function GetConnectivityVersion: Integer;
    procedure Initialize(const AMetadata: TWfcPipelineMetadata;
      const AVersions: TWfcPipelineVersions;
      const ARank: Integer; const AWrapNeighbors: Boolean;
      const ARunMode: TGraphRunMode;
      const AResources: TWfcPipelineResources;
      const APasses: TWfcPipelinePasses;
      const ADependencies: TWfcPipelineDependencies;
      const ABridges: TWfcPipelineBridges;
      const ARequirements: TWfcPipelineRequirements;
      const AValueQuotas: TWfcPipelineValueQuotas;
      const AConnectivities: TWfcPipelineConnectivities);
    procedure ValidateResourceIndex(const AIndex: Integer);
    procedure ValidatePassIndex(const AIndex: Integer);
    procedure ValidateDependencyIndex(const AIndex: Integer);
    procedure ValidateBridgeIndex(const AIndex: Integer);
    procedure ValidateRequirementIndex(const AIndex: Integer);
    procedure ValidateValueQuotaIndex(const AIndex: Integer);
    procedure ValidateConnectivityIndex(const AIndex: Integer);
  public
    constructor Create(const AMetadata: TWfcPipelineMetadata;
      const ARank: Integer; const AWrapNeighbors: Boolean;
      const ARunMode: TGraphRunMode;
      const AResources: TWfcPipelineResources;
      const APasses: TWfcPipelinePasses;
      const ADependencies: TWfcPipelineDependencies;
      const ABridges: TWfcPipelineBridges;
      const ARequirements: TWfcPipelineRequirements); overload;
    constructor Create(const AMetadata: TWfcPipelineMetadata;
      const ARank: Integer; const AWrapNeighbors: Boolean;
      const ARunMode: TGraphRunMode;
      const AResources: TWfcPipelineResources;
      const APasses: TWfcPipelinePasses;
      const ADependencies: TWfcPipelineDependencies;
      const ABridges: TWfcPipelineBridges;
      const ARequirements: TWfcPipelineRequirements;
      const AValueQuotas: TWfcPipelineValueQuotas); overload;
    constructor Create(const AMetadata: TWfcPipelineMetadata;
      const AVersions: TWfcPipelineVersions;
      const ARank: Integer; const AWrapNeighbors: Boolean;
      const ARunMode: TGraphRunMode;
      const AResources: TWfcPipelineResources;
      const APasses: TWfcPipelinePasses;
      const ADependencies: TWfcPipelineDependencies;
      const ABridges: TWfcPipelineBridges;
      const ARequirements: TWfcPipelineRequirements;
      const AValueQuotas: TWfcPipelineValueQuotas); overload;
    constructor Create(const AMetadata: TWfcPipelineMetadata;
      const AVersions: TWfcPipelineVersions;
      const ARank: Integer; const AWrapNeighbors: Boolean;
      const ARunMode: TGraphRunMode;
      const AResources: TWfcPipelineResources;
      const APasses: TWfcPipelinePasses;
      const ADependencies: TWfcPipelineDependencies;
      const ABridges: TWfcPipelineBridges;
      const ARequirements: TWfcPipelineRequirements); overload;
    constructor Create(const AMetadata: TWfcPipelineMetadata;
      const ARank: Integer; const AWrapNeighbors: Boolean;
      const ARunMode: TGraphRunMode;
      const AResources: TWfcPipelineResources;
      const APasses: TWfcPipelinePasses;
      const ADependencies: TWfcPipelineDependencies;
      const ABridges: TWfcPipelineBridges;
      const ARequirements: TWfcPipelineRequirements;
      const AValueQuotas: TWfcPipelineValueQuotas;
      const AConnectivities: TWfcPipelineConnectivities); overload;
    constructor Create(const AMetadata: TWfcPipelineMetadata;
      const AVersions: TWfcPipelineVersions;
      const ARank: Integer; const AWrapNeighbors: Boolean;
      const ARunMode: TGraphRunMode;
      const AResources: TWfcPipelineResources;
      const APasses: TWfcPipelinePasses;
      const ADependencies: TWfcPipelineDependencies;
      const ABridges: TWfcPipelineBridges;
      const ARequirements: TWfcPipelineRequirements;
      const AValueQuotas: TWfcPipelineValueQuotas;
      const AConnectivities: TWfcPipelineConnectivities); overload;
    destructor Destroy; override;

    function CopyMetadata: TWfcPipelineMetadata;
    function CopyVersions: TWfcPipelineVersions;
    function ResourceAt(const AIndex: Integer): TWfcPipelineResource;
    function PassAt(const AIndex: Integer): TWfcPipelinePass;
    function DependencyAt(const AIndex: Integer): TWfcPipelineDependency;
    function BridgeAt(const AIndex: Integer): TWfcPipelineBridge;
    function RequirementAt(const AIndex: Integer): TWfcPipelineRequirement;
    function ValueQuotaAt(const AIndex: Integer): TWfcPipelineValueQuota;
    function ConnectivityAt(const AIndex: Integer): TWfcPipelineConnectivity;
    function CopyResources: TWfcPipelineResources;
    function CopyPasses: TWfcPipelinePasses;
    function CopyDependencies: TWfcPipelineDependencies;
    function CopyBridges: TWfcPipelineBridges;
    function CopyRequirements: TWfcPipelineRequirements;
    function CopyValueQuotas: TWfcPipelineValueQuotas;
    function CopyConnectivities: TWfcPipelineConnectivities;
    function CopyPublicVocabulary(
      const APassIndex: Integer): TWfcModelTokens;
    function FindResource(const AId: TWfcModelToken): Integer;
    function FindPass(const ALabelName: TWfcModelToken): Integer;

    function BorrowModelResource(const AIndex: Integer): TWfcModel;
    function BorrowRuleResource(const AIndex: Integer): TWfcRuleModel;
    function BorrowPattern2DResource(
      const AIndex: Integer): TWfcOverlappingModel2D;
    function BorrowSequenceResource(
      const AIndex: Integer): TWfcSequenceModel;
    function BorrowPattern3DResource(
      const AIndex: Integer): TWfcOverlappingModel3D;

    property Rank: Integer read FRank;
    property WrapNeighbors: Boolean read FWrapNeighbors;
    property RunMode: TGraphRunMode read FRunMode;
    property ResourceCount: Integer read GetResourceCount;
    property PassCount: Integer read GetPassCount;
    property DependencyCount: Integer read GetDependencyCount;
    property BridgeCount: Integer read GetBridgeCount;
    property RequirementCount: Integer read GetRequirementCount;
    property ValueQuotaCount: Integer read GetValueQuotaCount;
    { Zero means no quota extension; otherwise the explicit supported version. }
    property ValueQuotaVersion: Integer read GetValueQuotaVersion;
    property ConnectivityCount: Integer read GetConnectivityCount;
    property ConnectivityVersion: Integer read GetConnectivityVersion;
    property Signature: TWfcPipelineSignature read FSignature;
    property HasPattern3D: Boolean read FHasPattern3D;
  end;

function CurrentWfcPipelineVersions: TWfcPipelineVersions;

function MakeWfcPipelineMetadata(const AName,
  ALicenseIdentifier, ASourceDescription,
  ASourceFingerprint: TWfcModelToken): TWfcPipelineMetadata;

function MakeWfcPipelineResource(const AId: TWfcModelToken;
  const AKind: TWfcPipelineResourceKind; const ADocument: String;
  const ASourceDescription,
  ASourceLicenseIdentifier,
  ASourceFingerprint: TWfcModelToken): TWfcPipelineResource;

function MakeWfcPipelinePass(const ALabelName: TWfcModelToken;
  const AVisibility: TWfcPipelinePassVisibility;
  const AMode: TGraphPassMode; const ATransformSourceIndex: Integer;
  const AAdapterKind: TWfcPipelineAdapterKind;
  const AResourceIndex: Integer; const AHasSequenceExtent: Boolean;
  const ASequenceExtent: TWfcSequenceExtent): TWfcPipelinePass;

function MakeWfcPipelineDependency(const AConsumerPassIndex,
  AProviderPassIndex: Integer): TWfcPipelineDependency;

function MakeWfcPipelineBridge(const AKind: TWfcPipelineBridgeKind;
  const ASourcePassIndex,
  ATargetPassIndex: Integer): TWfcPipelineBridge;

function MakeWfcPipelineRequirementTerm(const AOffsetX, AOffsetY,
  AOffsetZ: Integer;
  const AAllowedProviderTokens: TWfcModelTokens):
  TWfcPipelineRequirementTerm;

function MakeWfcPipelineRequirement(const AConsumerPassIndex: Integer;
  const AConsumerToken: TWfcModelToken;
  const AProviderPassIndex: Integer;
  const AKind: TWfcPipelineRequirementKind;
  const ATerms: TWfcPipelineRequirementTerms): TWfcPipelineRequirement;

function MakeWfcPipelineCountRequirement(
  const AConsumerPassIndex: Integer;
  const AConsumerToken: TWfcModelToken;
  const AProviderPassIndex: Integer;
  const ATerms: TWfcPipelineRequirementTerms;
  const AMinimumCount, AMaximumCount: Integer;
  const ACountMode: TGraphPassCountMode): TWfcPipelineRequirement;

function MakeWfcPipelineValueQuota(const APassIndex: Integer;
  const ALabelText: TWfcModelToken; const AValues: TWfcModelTokens;
  const AMinimumCount, AMaximumCount: Integer): TWfcPipelineValueQuota;

function MakeWfcPipelineConnectivityValue(const AValue: TWfcModelToken;
  const AOpenings: TGraphDirections;
  const ARequiredByValue: Boolean = False): TWfcPipelineConnectivityValue;

function MakeWfcPipelineConnectivity(const APassIndex: Integer;
  const ALabelText: TWfcModelToken; const ARoot: TGraphPosition;
  const ARequiredPositions: TGraphPositions;
  const AValues: TWfcPipelineConnectivityValues;
  const ARequireAllParticipants: Boolean = False): TWfcPipelineConnectivity;

function WfcPipelineSignatureHex(
  const ASignature: TWfcPipelineSignature): String;

implementation

uses
  wfc_text_codec,
  wfc_model_text,
  wfc_rule_text,
  wfc_pattern2d_text,
  wfc_pattern2d_graph,
  wfc_pattern3d_text,
  wfc_pattern3d_graph,
  wfc_sequence_text,
  wfc_sequence_graph;

type
  TPipelineBooleanArray = array of Boolean;
  TPipelineIntegerArray = array of Integer;

function CheckedLength(const ALength: SizeInt; const ALabel: String;
  const AMaximum: Integer): Integer;
begin
  if (ALength < 0) or
      ((ALength and (not SizeInt(High(Integer)))) <> 0) then
    raise EWfcPipelineModel.Create(ALabel + ' exceeds the Integer range');
  Result := Integer(ALength);
  if Result > AMaximum then
    raise EWfcPipelineModel.CreateFmt('%s exceeds the version-1 limit [%d > %d]',
      [ALabel, Result, AMaximum]);
end;

procedure RequireEnumResourceKind(const AValue: TWfcPipelineResourceKind;
  const ALabel: String);
begin
  case AValue of
    wprkModel, wprkRules, wprkPattern2D, wprkSequence, wprkPattern3D:
      Exit;
  else
    raise EWfcPipelineModel.CreateFmt('%s is unknown [%d]',
      [ALabel, Ord(AValue)]);
  end;
end;

procedure RequireEnumVisibility(const AValue: TWfcPipelinePassVisibility;
  const ALabel: String);
begin
  case AValue of
    wppvPrivate, wppvPublic:
      Exit;
  else
    raise EWfcPipelineModel.CreateFmt('%s is unknown [%d]',
      [ALabel, Ord(AValue)]);
  end;
end;

procedure RequireEnumAdapter(const AValue: TWfcPipelineAdapterKind;
  const ALabel: String);
begin
  case AValue of
    wpakEmpty, wpakModel, wpakRules, wpakPattern2D, wpakSequence, wpakPattern3D:
      Exit;
  else
    raise EWfcPipelineModel.CreateFmt('%s is unknown [%d]',
      [ALabel, Ord(AValue)]);
  end;
end;

procedure RequireEnumBridge(const AValue: TWfcPipelineBridgeKind;
  const ALabel: String);
begin
  case AValue of
    wpbkPattern2DProjection, wpbkSequenceProjection, wpbkPattern3DProjection:
      Exit;
  else
    raise EWfcPipelineModel.CreateFmt('%s is unknown [%d]',
      [ALabel, Ord(AValue)]);
  end;
end;

procedure RequireEnumRequirement(const AValue: TWfcPipelineRequirementKind;
  const ALabel: String);
begin
  case AValue of
    wprqExact, wprqAny, wprqCount:
      Exit;
  else
    raise EWfcPipelineModel.CreateFmt('%s is unknown [%d]',
      [ALabel, Ord(AValue)]);
  end;
end;

procedure RequireEnumCountMode(const AValue: TGraphPassCountMode;
  const ALabel: String);
begin
  case AValue of
    gpcmMatchingTerms, gpcmDistinctCells:
      Exit;
  else
    raise EWfcPipelineModel.CreateFmt('%s is unknown [%d]',
      [ALabel, Ord(AValue)]);
  end;
end;

procedure RequireEnumPassMode(const AValue: TGraphPassMode;
  const ALabel: String);
begin
  case AValue of
    gpmLegacy, gpmTransform, gpmOverlay:
      Exit;
  else
    raise EWfcPipelineModel.CreateFmt('%s is unknown [%d]',
      [ALabel, Ord(AValue)]);
  end;
end;

procedure RequireEnumRunMode(const AValue: TGraphRunMode;
  const ALabel: String);
begin
  case AValue of
    rmBottomUp, rmTopDown:
      Exit;
  else
    raise EWfcPipelineModel.CreateFmt('%s is unknown [%d]',
      [ALabel, Ord(AValue)]);
  end;
end;

procedure RequireEnumSequenceExtent(const AValue: TWfcSequenceExtent;
  const ALabel: String);
begin
  case AValue of
    wseWhole, wsePrefix, wseSuffix, wseFragment, wseWrap:
      Exit;
  else
    raise EWfcPipelineModel.CreateFmt('%s is unknown [%d]',
      [ALabel, Ord(AValue)]);
  end;
end;

procedure RequireToken(const AValue: TWfcModelToken;
  const ALabel: String; const AAllowEmpty: Boolean);
begin
  if Length(AValue) = 0 then
  begin
    if AAllowEmpty then
      Exit;
    raise EWfcPipelineModel.Create(ALabel + ' cannot be empty');
  end;
  if not WfcModelTokenIsValid(AValue) then
    raise EWfcPipelineModel.Create(ALabel +
      ' must contain valid Unicode scalar values');
end;

function CloneTokens(const AValues: TWfcModelTokens): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function CloneTerm(const AValue: TWfcPipelineRequirementTerm):
  TWfcPipelineRequirementTerm;
begin
  Result.OffsetX := AValue.OffsetX;
  Result.OffsetY := AValue.OffsetY;
  Result.OffsetZ := AValue.OffsetZ;
  Result.AllowedProviderTokens := CloneTokens(
    AValue.AllowedProviderTokens);
end;

function CloneTerms(const AValues: TWfcPipelineRequirementTerms):
  TWfcPipelineRequirementTerms;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := CloneTerm(AValues[I]);
end;

function CloneRequirement(const AValue: TWfcPipelineRequirement):
  TWfcPipelineRequirement;
begin
  Result.ConsumerPassIndex := AValue.ConsumerPassIndex;
  Result.ConsumerToken := AValue.ConsumerToken;
  Result.ProviderPassIndex := AValue.ProviderPassIndex;
  Result.Kind := AValue.Kind;
  if AValue.Kind = wprqCount then
  begin
    Result.CountMode := AValue.CountMode;
    Result.MinimumCount := AValue.MinimumCount;
    Result.MaximumCount := AValue.MaximumCount;
  end
  else
  begin
    Result.CountMode := gpcmMatchingTerms;
    Result.MinimumCount := 0;
    Result.MaximumCount := 0;
  end;
  Result.Terms := CloneTerms(AValue.Terms);
end;

function CloneValueQuota(const AValue: TWfcPipelineValueQuota):
  TWfcPipelineValueQuota;
begin
  Result.PassIndex := AValue.PassIndex;
  Result.LabelText := AValue.LabelText;
  Result.MinimumCount := AValue.MinimumCount;
  Result.MaximumCount := AValue.MaximumCount;
  Result.Values := CloneTokens(AValue.Values);
end;

procedure RequireQuotaInteger(const AValue: Integer; const ALabel: String);
begin
  { Match the core quota guard: positive comparisons reject NaN/undefined,
    and strict Trunc equality rejects fractional and string-valued JS input. }
  if not ((AValue >= 0) and (AValue <= High(Integer))) then
    raise EWfcPipelineModel.Create(ALabel +
      ' must be an exact integer in 0..High(Integer)');
  {$IFDEF PAS2JS}
  if AValue <> Trunc(AValue) then
    raise EWfcPipelineModel.Create(ALabel + ' must be an exact integer');
  {$ENDIF}
end;

procedure RequirePattern3DSignedInteger(const AValue: Integer; const ALabel: String);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    Valid = typeof AValue === 'number' && Number.isFinite(AValue) &&
      Math.floor(AValue) === AValue && AValue >= -2147483648 && AValue <= 2147483647;
  end;
  if not Valid then
    raise EWfcPipelineModel.Create(ALabel + ' must be an exact signed Integer');
  {$ENDIF}
end;

function CloneConnectivity(const AValue: TWfcPipelineConnectivity):
  TWfcPipelineConnectivity;
begin
  Result.PassIndex := AValue.PassIndex;
  Result.LabelText := AValue.LabelText;
  Result.Root := AValue.Root;
  Result.RequiredPositions := Copy(AValue.RequiredPositions, 0,
    Length(AValue.RequiredPositions));
  Result.Values := Copy(AValue.Values, 0, Length(AValue.Values));
  Result.RequireAllParticipants := AValue.RequireAllParticipants;
end;

procedure RequireConnectivityBoolean(const AValue: Boolean;
  const ALabel: String);
begin
  if (AValue <> False) and (AValue <> True) then
    raise EWfcPipelineModel.Create(ALabel + ' must be Boolean');
  {$IFNDEF PAS2JS}
  if Ord(AValue) > 1 then
    raise EWfcPipelineModel.Create(ALabel + ' must be Boolean');
  {$ENDIF}
end;

procedure RequireConnectivityPosition(const AValue: TGraphPosition;
  const ARank: Integer; const ALabel: String);

  procedure Axis(const ACoordinate: TGraphCoordinate);
  begin
    if not ((ACoordinate >= 0) and
        (ACoordinate <= TGraphCoordinate(High(Integer)))) then
      raise EWfcPipelineModel.Create(ALabel +
        ' coordinate must be an exact integer in 0..High(Integer)');
    {$IFDEF PAS2JS}
    if ACoordinate <> Trunc(ACoordinate) then
      raise EWfcPipelineModel.Create(ALabel + ' coordinate must be an exact integer');
    {$ENDIF}
  end;

begin
  Axis(AValue.X);
  Axis(AValue.Y);
  Axis(AValue.Z);
  if ((ARank = 1) and (AValue.Y <> 0)) or
      ((ARank < 3) and (AValue.Z <> 0)) then
    raise EWfcPipelineModel.Create(ALabel + ' coordinate exceeds the recipe rank');
end;

function ConnectivityPositionBefore(const ALeft,
  ARight: TGraphPosition): Boolean;
begin
  Result := (ALeft.Z < ARight.Z) or
    ((ALeft.Z = ARight.Z) and ((ALeft.Y < ARight.Y) or
    ((ALeft.Y = ARight.Y) and (ALeft.X < ARight.X))));
end;

function ConnectivityOpeningMask(const AOpenings: TGraphDirections): Integer;
var
  D: TGraphDirection;
begin
  Result := 0;
  for D := Low(TGraphDirection) to High(TGraphDirection) do
    if D in AOpenings then
      Result := Result or (1 shl Ord(D));
end;

function CurrentWfcPipelineVersions: TWfcPipelineVersions;
begin
  Result.GraphModelVersion := WFC_GRAPH_MODEL_VERSION;
  Result.RandomAlgorithmVersion := WFC_RANDOM_ALGORITHM_VERSION;
  Result.SolverAlgorithmVersion := WFC_SOLVER_ALGORITHM_VERSION;
  Result.PipelineAlgorithmVersion := WFC_PIPELINE_ALGORITHM_VERSION;
  Result.BundleGraphAdapterVersion := WFC_PIPELINE_GRAPH_ADAPTER_VERSION;
  Result.ModelGraphAdapterVersion := WFC_MODEL_GRAPH_ADAPTER_VERSION;
  Result.RulesGraphAdapterVersion := WFC_RULE_GRAPH_ADAPTER_VERSION;
  Result.Pattern2DGraphAdapterVersion :=
    WFC_PATTERN_2D_GRAPH_ADAPTER_VERSION;
  Result.SequenceGraphAdapterVersion := WFC_SEQUENCE_GRAPH_ADAPTER_VERSION;
  Result.Pattern2DBridgeVersion := WFC_PIPELINE_PATTERN_BRIDGE_VERSION;
  Result.SequenceBridgeVersion := WFC_PIPELINE_SEQUENCE_BRIDGE_VERSION;
  Result.Pattern3DGraphAdapterVersion := WFC_PATTERN_3D_GRAPH_ADAPTER_VERSION;
  Result.Pattern3DBridgeVersion := WFC_PIPELINE_PATTERN_3D_BRIDGE_VERSION;
end;

function NormalizeVersions(const AValue: TWfcPipelineVersions;
  const AHasPattern3D: Boolean): TWfcPipelineVersions;
begin
  { Do not copy the complete caller record: the appended scalar fields may be
    uninitialized in legacy native callers (or absent properties in JS). }
  Result := CurrentWfcPipelineVersions;
  Result.GraphModelVersion := AValue.GraphModelVersion;
  Result.RandomAlgorithmVersion := AValue.RandomAlgorithmVersion;
  Result.SolverAlgorithmVersion := AValue.SolverAlgorithmVersion;
  Result.PipelineAlgorithmVersion := AValue.PipelineAlgorithmVersion;
  Result.BundleGraphAdapterVersion := AValue.BundleGraphAdapterVersion;
  Result.ModelGraphAdapterVersion := AValue.ModelGraphAdapterVersion;
  Result.RulesGraphAdapterVersion := AValue.RulesGraphAdapterVersion;
  Result.Pattern2DGraphAdapterVersion := AValue.Pattern2DGraphAdapterVersion;
  Result.SequenceGraphAdapterVersion := AValue.SequenceGraphAdapterVersion;
  Result.Pattern2DBridgeVersion := AValue.Pattern2DBridgeVersion;
  Result.SequenceBridgeVersion := AValue.SequenceBridgeVersion;
  if AHasPattern3D then
  begin
    RequireQuotaInteger(AValue.Pattern3DGraphAdapterVersion, 'pattern3d graph-adapter version');
    RequireQuotaInteger(AValue.Pattern3DBridgeVersion, 'pattern3d bridge version');
    if AValue.Pattern3DGraphAdapterVersion <> WFC_PATTERN_3D_GRAPH_ADAPTER_VERSION then
      raise EWfcPipelineModel.Create('unsupported pattern3d graph-adapter version');
    if AValue.Pattern3DBridgeVersion <> WFC_PIPELINE_PATTERN_3D_BRIDGE_VERSION then
      raise EWfcPipelineModel.Create('unsupported pattern3d bridge version');
    Result.Pattern3DGraphAdapterVersion := AValue.Pattern3DGraphAdapterVersion;
    Result.Pattern3DBridgeVersion := AValue.Pattern3DBridgeVersion;
  end;
end;

procedure ValidateVersions(const AValue: TWfcPipelineVersions);
var
  LCurrent: TWfcPipelineVersions;
begin
  LCurrent := CurrentWfcPipelineVersions;
  if AValue.GraphModelVersion <> LCurrent.GraphModelVersion then
    raise EWfcPipelineModel.Create('unsupported graph-model version');
  if AValue.RandomAlgorithmVersion <> LCurrent.RandomAlgorithmVersion then
    raise EWfcPipelineModel.Create('unsupported random-algorithm version');
  if AValue.SolverAlgorithmVersion <> LCurrent.SolverAlgorithmVersion then
    raise EWfcPipelineModel.Create('unsupported reference-solver version');
  if AValue.PipelineAlgorithmVersion <>
      LCurrent.PipelineAlgorithmVersion then
    raise EWfcPipelineModel.Create('unsupported pass-pipeline version');
  if AValue.BundleGraphAdapterVersion <>
      LCurrent.BundleGraphAdapterVersion then
    raise EWfcPipelineModel.Create('unsupported bundle graph-adapter version');
  if AValue.ModelGraphAdapterVersion <>
      LCurrent.ModelGraphAdapterVersion then
    raise EWfcPipelineModel.Create('unsupported model graph-adapter version');
  if AValue.RulesGraphAdapterVersion <>
      LCurrent.RulesGraphAdapterVersion then
    raise EWfcPipelineModel.Create('unsupported rules graph-adapter version');
  if AValue.Pattern2DGraphAdapterVersion <>
      LCurrent.Pattern2DGraphAdapterVersion then
    raise EWfcPipelineModel.Create(
      'unsupported pattern2d graph-adapter version');
  if AValue.SequenceGraphAdapterVersion <>
      LCurrent.SequenceGraphAdapterVersion then
    raise EWfcPipelineModel.Create(
      'unsupported sequence graph-adapter version');
  if (AValue.Pattern2DBridgeVersion <> 1) and
      (AValue.Pattern2DBridgeVersion <> 2) then
    raise EWfcPipelineModel.Create('unsupported pattern2d bridge version');
  if (AValue.SequenceBridgeVersion <> 1) and
      (AValue.SequenceBridgeVersion <> 2) then
    raise EWfcPipelineModel.Create('unsupported sequence bridge version');
end;

function MakeWfcPipelineMetadata(const AName,
  ALicenseIdentifier, ASourceDescription,
  ASourceFingerprint: TWfcModelToken): TWfcPipelineMetadata;
begin
  Result.Name := AName;
  Result.LicenseIdentifier := ALicenseIdentifier;
  Result.SourceDescription := ASourceDescription;
  Result.SourceFingerprint := ASourceFingerprint;
end;

function MakeWfcPipelineResource(const AId: TWfcModelToken;
  const AKind: TWfcPipelineResourceKind; const ADocument: String;
  const ASourceDescription, ASourceLicenseIdentifier,
  ASourceFingerprint: TWfcModelToken): TWfcPipelineResource;
begin
  Result.Id := AId;
  Result.Kind := AKind;
  Result.Document := ADocument;
  Result.SourceDescription := ASourceDescription;
  Result.SourceLicenseIdentifier := ASourceLicenseIdentifier;
  Result.SourceFingerprint := ASourceFingerprint;
end;

function MakeWfcPipelinePass(const ALabelName: TWfcModelToken;
  const AVisibility: TWfcPipelinePassVisibility;
  const AMode: TGraphPassMode; const ATransformSourceIndex: Integer;
  const AAdapterKind: TWfcPipelineAdapterKind;
  const AResourceIndex: Integer; const AHasSequenceExtent: Boolean;
  const ASequenceExtent: TWfcSequenceExtent): TWfcPipelinePass;
begin
  Result.LabelName := ALabelName;
  Result.Visibility := AVisibility;
  Result.Mode := AMode;
  Result.TransformSourceIndex := ATransformSourceIndex;
  Result.AdapterKind := AAdapterKind;
  Result.ResourceIndex := AResourceIndex;
  Result.HasSequenceExtent := AHasSequenceExtent;
  Result.SequenceExtent := ASequenceExtent;
end;

function MakeWfcPipelineDependency(const AConsumerPassIndex,
  AProviderPassIndex: Integer): TWfcPipelineDependency;
begin
  Result.ConsumerPassIndex := AConsumerPassIndex;
  Result.ProviderPassIndex := AProviderPassIndex;
end;

function MakeWfcPipelineBridge(const AKind: TWfcPipelineBridgeKind;
  const ASourcePassIndex,
  ATargetPassIndex: Integer): TWfcPipelineBridge;
begin
  Result.Kind := AKind;
  Result.SourcePassIndex := ASourcePassIndex;
  Result.TargetPassIndex := ATargetPassIndex;
end;

function MakeWfcPipelineRequirementTerm(const AOffsetX, AOffsetY,
  AOffsetZ: Integer; const AAllowedProviderTokens: TWfcModelTokens):
  TWfcPipelineRequirementTerm;
begin
  CheckedLength(Length(AAllowedProviderTokens),
    'requirement-term allowed-token count',
    WFC_PIPELINE_MAX_ALLOWED_TOKEN_COUNT);
  Result.OffsetX := AOffsetX;
  Result.OffsetY := AOffsetY;
  Result.OffsetZ := AOffsetZ;
  Result.AllowedProviderTokens := CloneTokens(AAllowedProviderTokens);
end;

function MakeWfcPipelineRequirement(const AConsumerPassIndex: Integer;
  const AConsumerToken: TWfcModelToken;
  const AProviderPassIndex: Integer;
  const AKind: TWfcPipelineRequirementKind;
  const ATerms: TWfcPipelineRequirementTerms): TWfcPipelineRequirement;
var
  I: Integer;
begin
  if AKind = wprqCount then
    raise EWfcPipelineModel.Create(
      'count requirements require MakeWfcPipelineCountRequirement');
  CheckedLength(Length(ATerms), 'requirement term count',
    WFC_PIPELINE_MAX_REQUIREMENT_TERM_COUNT);
  for I := 0 to Length(ATerms) - 1 do
    CheckedLength(Length(ATerms[I].AllowedProviderTokens),
      Format('requirement term %d allowed-token count', [I]),
      WFC_PIPELINE_MAX_ALLOWED_TOKEN_COUNT);
  Result.ConsumerPassIndex := AConsumerPassIndex;
  Result.ConsumerToken := AConsumerToken;
  Result.ProviderPassIndex := AProviderPassIndex;
  Result.Kind := AKind;
  Result.CountMode := gpcmMatchingTerms;
  Result.MinimumCount := 0;
  Result.MaximumCount := 0;
  Result.Terms := CloneTerms(ATerms);
end;

function MakeWfcPipelineCountRequirement(
  const AConsumerPassIndex: Integer;
  const AConsumerToken: TWfcModelToken;
  const AProviderPassIndex: Integer;
  const ATerms: TWfcPipelineRequirementTerms;
  const AMinimumCount, AMaximumCount: Integer;
  const ACountMode: TGraphPassCountMode): TWfcPipelineRequirement;
var
  I: Integer;
begin
  CheckedLength(Length(ATerms), 'requirement term count',
    WFC_PIPELINE_MAX_REQUIREMENT_TERM_COUNT);
  for I := 0 to Length(ATerms) - 1 do
    CheckedLength(Length(ATerms[I].AllowedProviderTokens),
      Format('requirement term %d allowed-token count', [I]),
      WFC_PIPELINE_MAX_ALLOWED_TOKEN_COUNT);
  Result.ConsumerPassIndex := AConsumerPassIndex;
  Result.ConsumerToken := AConsumerToken;
  Result.ProviderPassIndex := AProviderPassIndex;
  Result.Kind := wprqCount;
  Result.CountMode := ACountMode;
  Result.MinimumCount := AMinimumCount;
  Result.MaximumCount := AMaximumCount;
  Result.Terms := CloneTerms(ATerms);
end;

function MakeWfcPipelineValueQuota(const APassIndex: Integer;
  const ALabelText: TWfcModelToken; const AValues: TWfcModelTokens;
  const AMinimumCount, AMaximumCount: Integer): TWfcPipelineValueQuota;
begin
  { Like the other IR factories, this detaches bounded input storage; the
    immutable recipe constructor performs semantic/vocabulary validation. }
  CheckedLength(Length(AValues), 'value-quota token count',
    WFC_PIPELINE_MAX_VALUE_QUOTA_TOKEN_COUNT);
  Result.PassIndex := APassIndex;
  Result.LabelText := ALabelText;
  Result.Values := CloneTokens(AValues);
  Result.MinimumCount := AMinimumCount;
  Result.MaximumCount := AMaximumCount;
end;

function MakeWfcPipelineConnectivityValue(const AValue: TWfcModelToken;
  const AOpenings: TGraphDirections;
  const ARequiredByValue: Boolean): TWfcPipelineConnectivityValue;
begin
  Result.Value := AValue;
  Result.Openings := AOpenings;
  Result.RequiredByValue := ARequiredByValue;
end;

function MakeWfcPipelineConnectivity(const APassIndex: Integer;
  const ALabelText: TWfcModelToken; const ARoot: TGraphPosition;
  const ARequiredPositions: TGraphPositions;
  const AValues: TWfcPipelineConnectivityValues;
  const ARequireAllParticipants: Boolean): TWfcPipelineConnectivity;
begin
  CheckedLength(Length(ARequiredPositions), 'connectivity terminal count',
    WFC_PIPELINE_MAX_CONNECTIVITY_REQUIRED_POSITION_COUNT);
  CheckedLength(Length(AValues), 'connectivity profile count',
    WFC_PIPELINE_MAX_CONNECTIVITY_VALUE_COUNT);
  Result.PassIndex := APassIndex;
  Result.LabelText := ALabelText;
  Result.Root := ARoot;
  Result.RequiredPositions := Copy(ARequiredPositions, 0, Length(ARequiredPositions));
  Result.Values := Copy(AValues, 0, Length(AValues));
  Result.RequireAllParticipants := ARequireAllParticipants;
end;

function TextIsAscii(const AValue: String): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(AValue) do
    if Ord(AValue[I]) > 127 then
      Exit(False);
  Result := True;
end;

function CheckedDenseRelationSlotCount(const AStateCount: Integer;
  const ALabel: String; const ADirectionCount: Integer = 4): Integer;
var
  LSquare: Integer;
begin
  if AStateCount < 0 then
    raise EWfcPipelineModel.Create(ALabel + ' cannot be negative');
  if (AStateCount <> 0) and
      (AStateCount > High(Integer) div AStateCount) then
    raise EWfcPipelineModel.Create(ALabel + ' dimensions overflow Integer');
  LSquare := AStateCount * AStateCount;
  if LSquare > High(Integer) div ADirectionCount then
    raise EWfcPipelineModel.Create(ALabel + ' dimensions overflow Integer');
  Result := ADirectionCount * LSquare;
end;

function TokenIndex(const AValues: TWfcModelTokens;
  const AValue: TWfcModelToken): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(AValues) - 1 do
    if AValues[I] = AValue then
      Exit(I);
  Result := -1;
end;

function DependencyExists(const ADependencies: TWfcPipelineDependencies;
  const AConsumer, AProvider: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(ADependencies) - 1 do
    if (ADependencies[I].ConsumerPassIndex = AConsumer) and
        (ADependencies[I].ProviderPassIndex = AProvider) then
      Exit(True);
  Result := False;
end;

function TermsAreStrictlyOrdered(const APrevious,
  ACurrent: TWfcPipelineRequirementTerm): Boolean;
begin
  if APrevious.OffsetX <> ACurrent.OffsetX then
    Exit(APrevious.OffsetX < ACurrent.OffsetX);
  if APrevious.OffsetY <> ACurrent.OffsetY then
    Exit(APrevious.OffsetY < ACurrent.OffsetY);
  Result := APrevious.OffsetZ < ACurrent.OffsetZ;
end;

procedure HashByte(var AHash: TWfcPipelineSignature;
  const AValue: Byte);
{$PUSH}
{$Q-}
var
  LValue: Cardinal;
begin
  AHash := AHash xor Cardinal(AValue);
  LValue := AHash;
  AHash := (LValue + (LValue shl 1) + (LValue shl 4) +
    (LValue shl 7) + (LValue shl 8) + (LValue shl 24)) and
    Cardinal($FFFFFFFF);
end;
{$POP}

procedure HashCardinal(var AHash: TWfcPipelineSignature;
  const AValue: Cardinal);
begin
  HashByte(AHash, Byte(AValue and $FF));
  HashByte(AHash, Byte((AValue shr 8) and $FF));
  HashByte(AHash, Byte((AValue shr 16) and $FF));
  HashByte(AHash, Byte((AValue shr 24) and $FF));
end;

procedure HashInteger(var AHash: TWfcPipelineSignature;
  const AValue: Integer);
begin
  HashCardinal(AHash, Cardinal(AValue));
end;

procedure HashBoolean(var AHash: TWfcPipelineSignature;
  const AValue: Boolean);
begin
  if AValue then
    HashByte(AHash, 1)
  else
    HashByte(AHash, 0);
end;

procedure HashAscii(var AHash: TWfcPipelineSignature;
  const AValue: String);
var
  I: Integer;
begin
  HashCardinal(AHash, Cardinal(Length(AValue)));
  for I := 1 to Length(AValue) do
    HashByte(AHash, Byte(Ord(AValue[I])));
end;

procedure HashToken(var AHash: TWfcPipelineSignature;
  const AValue: TWfcModelToken);
var
  LCanonical: String;
begin
  LCanonical := WfcTextEncodeToken(AValue,
    'pipeline semantic signature');
  HashAscii(AHash, LCanonical);
end;

function CalculateSignature(const AModel: TWfcPipelineModel):
  TWfcPipelineSignature;
var
  I: Integer;
  J: Integer;
  K: Integer;
  LBridge: TWfcPipelineBridge;
  LDependency: TWfcPipelineDependency;
  LMetadata: TWfcPipelineMetadata;
  LPass: TWfcPipelinePass;
  LRequirement: TWfcPipelineRequirement;
  LResource: TWfcPipelineResource;
  LTerm: TWfcPipelineRequirementTerm;
  LQuota: TWfcPipelineValueQuota;
  LConnectivity: TWfcPipelineConnectivity;
  LVersions: TWfcPipelineVersions;
begin
  Result := Cardinal(2166136261);
  HashAscii(Result, 'wfcpipeline-model');
  HashCardinal(Result, WFC_PIPELINE_MODEL_VERSION);
  HashCardinal(Result, WFC_PIPELINE_MODEL_SIGNATURE_VERSION);

  LVersions := AModel.CopyVersions;
  HashInteger(Result, LVersions.GraphModelVersion);
  HashInteger(Result, LVersions.RandomAlgorithmVersion);
  HashInteger(Result, LVersions.SolverAlgorithmVersion);
  HashInteger(Result, LVersions.PipelineAlgorithmVersion);
  HashInteger(Result, LVersions.BundleGraphAdapterVersion);
  HashInteger(Result, LVersions.ModelGraphAdapterVersion);
  HashInteger(Result, LVersions.RulesGraphAdapterVersion);
  HashInteger(Result, LVersions.Pattern2DGraphAdapterVersion);
  HashInteger(Result, LVersions.SequenceGraphAdapterVersion);
  HashInteger(Result, LVersions.Pattern2DBridgeVersion);
  HashInteger(Result, LVersions.SequenceBridgeVersion);
  if AModel.HasPattern3D then
  begin
    HashAscii(Result, 'pattern3d-feature-v1');
    HashInteger(Result, LVersions.Pattern3DGraphAdapterVersion);
    HashInteger(Result, LVersions.Pattern3DBridgeVersion);
  end;

  LMetadata := AModel.CopyMetadata;
  HashToken(Result, LMetadata.Name);
  HashToken(Result, LMetadata.LicenseIdentifier);
  HashToken(Result, LMetadata.SourceDescription);
  HashToken(Result, LMetadata.SourceFingerprint);
  HashInteger(Result, AModel.Rank);
  HashBoolean(Result, AModel.WrapNeighbors);
  HashInteger(Result, Ord(AModel.RunMode));

  HashInteger(Result, AModel.ResourceCount);
  for I := 0 to AModel.ResourceCount - 1 do
  begin
    LResource := AModel.ResourceAt(I);
    HashInteger(Result, Ord(LResource.Kind));
    HashToken(Result, LResource.Id);
    HashAscii(Result, LResource.Document);
    HashToken(Result, LResource.SourceDescription);
    HashToken(Result, LResource.SourceLicenseIdentifier);
    HashToken(Result, LResource.SourceFingerprint);
  end;

  HashInteger(Result, AModel.PassCount);
  for I := 0 to AModel.PassCount - 1 do
  begin
    LPass := AModel.PassAt(I);
    HashToken(Result, LPass.LabelName);
    HashInteger(Result, Ord(LPass.Visibility));
    HashInteger(Result, Ord(LPass.Mode));
    HashInteger(Result, LPass.TransformSourceIndex);
    HashInteger(Result, Ord(LPass.AdapterKind));
    HashInteger(Result, LPass.ResourceIndex);
    HashBoolean(Result, LPass.HasSequenceExtent);
    HashInteger(Result, Ord(LPass.SequenceExtent));
  end;

  HashInteger(Result, AModel.DependencyCount);
  for I := 0 to AModel.DependencyCount - 1 do
  begin
    LDependency := AModel.DependencyAt(I);
    HashInteger(Result, LDependency.ConsumerPassIndex);
    HashInteger(Result, LDependency.ProviderPassIndex);
  end;

  HashInteger(Result, AModel.BridgeCount);
  for I := 0 to AModel.BridgeCount - 1 do
  begin
    LBridge := AModel.BridgeAt(I);
    HashInteger(Result, Ord(LBridge.Kind));
    HashInteger(Result, LBridge.SourcePassIndex);
    HashInteger(Result, LBridge.TargetPassIndex);
  end;

  HashInteger(Result, AModel.RequirementCount);
  for I := 0 to AModel.RequirementCount - 1 do
  begin
    LRequirement := AModel.RequirementAt(I);
    HashInteger(Result, LRequirement.ConsumerPassIndex);
    HashToken(Result, LRequirement.ConsumerToken);
    HashInteger(Result, LRequirement.ProviderPassIndex);
    HashInteger(Result, Ord(LRequirement.Kind));
    if LRequirement.Kind = wprqCount then
    begin
      HashInteger(Result, Ord(LRequirement.CountMode));
      HashInteger(Result, LRequirement.MinimumCount);
      HashInteger(Result, LRequirement.MaximumCount);
    end;
    HashInteger(Result, Length(LRequirement.Terms));
    for J := 0 to Length(LRequirement.Terms) - 1 do
    begin
      LTerm := LRequirement.Terms[J];
      HashInteger(Result, LTerm.OffsetX);
      HashInteger(Result, LTerm.OffsetY);
      HashInteger(Result, LTerm.OffsetZ);
      HashInteger(Result, Length(LTerm.AllowedProviderTokens));
      for K := 0 to Length(LTerm.AllowedProviderTokens) - 1 do
        HashToken(Result, LTerm.AllowedProviderTokens[K]);
    end;
  end;
  { Do not even hash a zero count for legacy recipes: their complete version-1
    identities are already public goldens and must remain byte-for-byte. }
  if AModel.ValueQuotaCount <> 0 then
  begin
    HashAscii(Result, 'wfcpipeline-value-quotas');
    HashInteger(Result, AModel.ValueQuotaVersion);
    HashInteger(Result, WFC_GRAPH_VALUE_QUOTA_VERSION);
    HashInteger(Result, AModel.ValueQuotaCount);
    for I := 0 to AModel.ValueQuotaCount - 1 do
    begin
      LQuota := AModel.ValueQuotaAt(I);
      HashInteger(Result, LQuota.PassIndex);
      HashToken(Result, LQuota.LabelText);
      HashInteger(Result, LQuota.MinimumCount);
      HashInteger(Result, LQuota.MaximumCount);
      HashInteger(Result, Length(LQuota.Values));
      for J := 0 to Length(LQuota.Values) - 1 do
        HashToken(Result, LQuota.Values[J]);
    end;
  end;
  if AModel.ConnectivityCount <> 0 then
  begin
    HashAscii(Result, 'wfcpipeline-connectivity');
    HashInteger(Result, AModel.ConnectivityVersion);
    HashInteger(Result, WFC_GRAPH_CONNECTIVITY_VERSION);
    HashInteger(Result, AModel.ConnectivityCount);
    for I := 0 to AModel.ConnectivityCount - 1 do
    begin
      LConnectivity := AModel.ConnectivityAt(I);
      HashInteger(Result, LConnectivity.PassIndex);
      HashToken(Result, LConnectivity.LabelText);
      HashInteger(Result, Integer(LConnectivity.Root.X));
      HashInteger(Result, Integer(LConnectivity.Root.Y));
      HashInteger(Result, Integer(LConnectivity.Root.Z));
      HashBoolean(Result, LConnectivity.RequireAllParticipants);
      HashInteger(Result, Length(LConnectivity.RequiredPositions));
      for J := 0 to Length(LConnectivity.RequiredPositions) - 1 do
      begin
        HashInteger(Result, Integer(LConnectivity.RequiredPositions[J].X));
        HashInteger(Result, Integer(LConnectivity.RequiredPositions[J].Y));
        HashInteger(Result, Integer(LConnectivity.RequiredPositions[J].Z));
      end;
      HashInteger(Result, Length(LConnectivity.Values));
      for J := 0 to Length(LConnectivity.Values) - 1 do
      begin
        HashToken(Result, LConnectivity.Values[J].Value);
        HashInteger(Result, ConnectivityOpeningMask(LConnectivity.Values[J].Openings));
        HashBoolean(Result, LConnectivity.Values[J].RequiredByValue);
      end;
    end;
  end;
end;

function WfcPipelineSignatureHex(
  const ASignature: TWfcPipelineSignature): String;
begin
  Result := IntToHex(ASignature, 8);
end;

{ TWfcPipelineModel }

constructor TWfcPipelineModel.Create(
  const AMetadata: TWfcPipelineMetadata; const ARank: Integer;
  const AWrapNeighbors: Boolean; const ARunMode: TGraphRunMode;
  const AResources: TWfcPipelineResources;
  const APasses: TWfcPipelinePasses;
  const ADependencies: TWfcPipelineDependencies;
  const ABridges: TWfcPipelineBridges;
  const ARequirements: TWfcPipelineRequirements);
begin
  inherited Create;
  Initialize(AMetadata, CurrentWfcPipelineVersions, ARank,
    AWrapNeighbors, ARunMode, AResources, APasses, ADependencies,
    ABridges, ARequirements, nil, nil);
end;

constructor TWfcPipelineModel.Create(
  const AMetadata: TWfcPipelineMetadata;
  const AVersions: TWfcPipelineVersions; const ARank: Integer;
  const AWrapNeighbors: Boolean; const ARunMode: TGraphRunMode;
  const AResources: TWfcPipelineResources;
  const APasses: TWfcPipelinePasses;
  const ADependencies: TWfcPipelineDependencies;
  const ABridges: TWfcPipelineBridges;
  const ARequirements: TWfcPipelineRequirements);
begin
  inherited Create;
  Initialize(AMetadata, AVersions, ARank, AWrapNeighbors, ARunMode,
    AResources, APasses, ADependencies, ABridges, ARequirements, nil, nil);
end;

constructor TWfcPipelineModel.Create(
  const AMetadata: TWfcPipelineMetadata; const ARank: Integer;
  const AWrapNeighbors: Boolean; const ARunMode: TGraphRunMode;
  const AResources: TWfcPipelineResources;
  const APasses: TWfcPipelinePasses;
  const ADependencies: TWfcPipelineDependencies;
  const ABridges: TWfcPipelineBridges;
  const ARequirements: TWfcPipelineRequirements;
  const AValueQuotas: TWfcPipelineValueQuotas);
begin
  inherited Create;
  Initialize(AMetadata, CurrentWfcPipelineVersions, ARank,
    AWrapNeighbors, ARunMode, AResources, APasses, ADependencies,
    ABridges, ARequirements, AValueQuotas, nil);
end;

constructor TWfcPipelineModel.Create(
  const AMetadata: TWfcPipelineMetadata;
  const AVersions: TWfcPipelineVersions; const ARank: Integer;
  const AWrapNeighbors: Boolean; const ARunMode: TGraphRunMode;
  const AResources: TWfcPipelineResources;
  const APasses: TWfcPipelinePasses;
  const ADependencies: TWfcPipelineDependencies;
  const ABridges: TWfcPipelineBridges;
  const ARequirements: TWfcPipelineRequirements;
  const AValueQuotas: TWfcPipelineValueQuotas);
begin
  inherited Create;
  Initialize(AMetadata, AVersions, ARank, AWrapNeighbors, ARunMode,
    AResources, APasses, ADependencies, ABridges, ARequirements, AValueQuotas, nil);
end;

constructor TWfcPipelineModel.Create(
  const AMetadata: TWfcPipelineMetadata; const ARank: Integer;
  const AWrapNeighbors: Boolean; const ARunMode: TGraphRunMode;
  const AResources: TWfcPipelineResources;
  const APasses: TWfcPipelinePasses;
  const ADependencies: TWfcPipelineDependencies;
  const ABridges: TWfcPipelineBridges;
  const ARequirements: TWfcPipelineRequirements;
  const AValueQuotas: TWfcPipelineValueQuotas;
  const AConnectivities: TWfcPipelineConnectivities);
begin
  inherited Create;
  Initialize(AMetadata, CurrentWfcPipelineVersions, ARank,
    AWrapNeighbors, ARunMode, AResources, APasses, ADependencies,
    ABridges, ARequirements, AValueQuotas, AConnectivities);
end;

constructor TWfcPipelineModel.Create(
  const AMetadata: TWfcPipelineMetadata;
  const AVersions: TWfcPipelineVersions; const ARank: Integer;
  const AWrapNeighbors: Boolean; const ARunMode: TGraphRunMode;
  const AResources: TWfcPipelineResources;
  const APasses: TWfcPipelinePasses;
  const ADependencies: TWfcPipelineDependencies;
  const ABridges: TWfcPipelineBridges;
  const ARequirements: TWfcPipelineRequirements;
  const AValueQuotas: TWfcPipelineValueQuotas;
  const AConnectivities: TWfcPipelineConnectivities);
begin
  inherited Create;
  Initialize(AMetadata, AVersions, ARank, AWrapNeighbors, ARunMode,
    AResources, APasses, ADependencies, ABridges, ARequirements,
    AValueQuotas, AConnectivities);
end;

destructor TWfcPipelineModel.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FPattern3DResources) - 1 do
    FPattern3DResources[I].Free;
  for I := 0 to Length(FSequenceResources) - 1 do
    FSequenceResources[I].Free;
  for I := 0 to Length(FPatternResources) - 1 do
    FPatternResources[I].Free;
  for I := 0 to Length(FRuleResources) - 1 do
    FRuleResources[I].Free;
  for I := 0 to Length(FModelResources) - 1 do
    FModelResources[I].Free;
  inherited Destroy;
end;

procedure TWfcPipelineModel.Initialize(
  const AMetadata: TWfcPipelineMetadata;
  const AVersions: TWfcPipelineVersions; const ARank: Integer;
  const AWrapNeighbors: Boolean; const ARunMode: TGraphRunMode;
  const AResources: TWfcPipelineResources;
  const APasses: TWfcPipelinePasses;
  const ADependencies: TWfcPipelineDependencies;
  const ABridges: TWfcPipelineBridges;
  const ARequirements: TWfcPipelineRequirements;
  const AValueQuotas: TWfcPipelineValueQuotas;
  const AConnectivities: TWfcPipelineConnectivities);
var
  I: Integer;
  J: Integer;
  K: Integer;
  LAllowedCount: Integer;
  LBridgeCount: Integer;
  LCurrentPayloadLength: Integer;
  LCurrentRelationSlotCount: Integer;
  LDependencyCount: Integer;
  LIndegree: TPipelineIntegerArray;
  LIteration: Integer;
  LPassCount: Integer;
  LProcessed: TPipelineBooleanArray;
  LProcessedCount: Integer;
  LProgress: Boolean;
  LProviderTokenIndex: Integer;
  LRequirementCount: Integer;
  LResourceCount: Integer;
  LPreviousTokenIndex: Integer;
  LTermCount: Integer;
  LTotalAllowedTokenCount: Integer;
  LTotalEncodedTokenLength: Integer;
  LTotalPayloadLength: Integer;
  LTotalRequirementTermCount: Integer;
  LTotalResourceRelationSlotCount: Integer;
  LVersions: TWfcPipelineVersions;
  LQuotaCount: Integer;
  LTotalQuotaTokenCount: Integer;
  LConnectivityCount: Integer;
  LTotalConnectivityValueCount: Integer;
  LTotalConnectivityPositionCount: Integer;
  LDirections: TGraphDirections;
  LDirection: TGraphDirection;

  procedure ValidateIndex(const AIndex, ACount: Integer;
    const ALabel: String);
  begin
    if FHasPattern3D then RequireQuotaInteger(AIndex, ALabel);
    if (AIndex < 0) or (AIndex >= ACount) then
      raise EWfcPipelineModel.CreateFmt('%s is out of bounds [%d]',
        [ALabel, AIndex]);
  end;

  procedure RequireDependency(const AConsumer, AProvider: Integer;
    const ALabel: String);
  begin
    if not DependencyExists(FDependencies, AConsumer, AProvider) then
      raise EWfcPipelineModel.CreateFmt(
        '%s requires dependency %d -> %d',
        [ALabel, AConsumer, AProvider]);
  end;

  function BridgeTargetsPass(const APassIndex: Integer): Boolean;
  var
    LIndex: Integer;
  begin
    for LIndex := 0 to Length(FBridges) - 1 do
      if FBridges[LIndex].TargetPassIndex = APassIndex then
        Exit(True);
    Result := False;
  end;

  procedure AssignVocabulary(const APassIndex: Integer;
    const AValues: TWfcModelTokens; const ALabel: String);
  begin
    if Length(FVocabularies[APassIndex]) <> 0 then
      raise EWfcPipelineModel.Create(ALabel +
        ' conflicts with an existing public vocabulary owner');
    FVocabularies[APassIndex] := CloneTokens(AValues);
  end;

  procedure AccumulateOuterToken(const AValue: TWfcModelToken;
    const ALabel: String; const AAllowEmpty: Boolean);
  var
    LEncoded: String;
    LEncodedLength: Integer;
  begin
    CheckedLength(Length(AValue), ALabel + ' raw token length',
      WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH);
    RequireToken(AValue, ALabel, AAllowEmpty);
    LEncoded := WfcTextEncodeToken(AValue, 'WFC pipeline model');
    LEncodedLength := CheckedLength(Length(LEncoded),
      ALabel + ' encoded token length',
      WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH);
    if LTotalEncodedTokenLength >
        WFC_PIPELINE_MAX_TOTAL_ENCODED_TOKEN_LENGTH - LEncodedLength then
      raise EWfcPipelineModel.Create(
        'aggregate outer-token encoding exceeds the version-1 limit');
    Inc(LTotalEncodedTokenLength, LEncodedLength);
  end;

begin
  ValidateVersions(AVersions);

  if (ARank < 1) or (ARank > 3) then
    raise EWfcPipelineModel.CreateFmt(
      'pipeline rank must be 1, 2, or 3 [%d]', [ARank]);
  RequireEnumRunMode(ARunMode, 'pipeline run mode');

  { Preflight every externally controlled collection length and every resource
    byte count before allocating owner arrays or invoking nested codecs. }
  LResourceCount := CheckedLength(Length(AResources),
    'pipeline resource count', WFC_PIPELINE_MAX_RESOURCE_COUNT);
  LPassCount := CheckedLength(Length(APasses), 'pipeline pass count',
    WFC_PIPELINE_MAX_PASS_COUNT);
  if LPassCount = 0 then
    raise EWfcPipelineModel.Create('pipeline must contain at least one pass');
  LDependencyCount := CheckedLength(Length(ADependencies),
    'pipeline dependency count', WFC_PIPELINE_MAX_DEPENDENCY_COUNT);
  LBridgeCount := CheckedLength(Length(ABridges),
    'pipeline bridge count', WFC_PIPELINE_MAX_BRIDGE_COUNT);
  LRequirementCount := CheckedLength(Length(ARequirements),
    'pipeline requirement count', WFC_PIPELINE_MAX_REQUIREMENT_COUNT);
  LQuotaCount := CheckedLength(Length(AValueQuotas),
    'pipeline value-quota count', WFC_PIPELINE_MAX_VALUE_QUOTA_COUNT);
  LConnectivityCount := CheckedLength(Length(AConnectivities),
    'pipeline connectivity count', WFC_PIPELINE_MAX_CONNECTIVITY_COUNT);
  FHasPattern3D := False;
  for I := 0 to LResourceCount - 1 do
    if AResources[I].Kind = wprkPattern3D then FHasPattern3D := True;
  for I := 0 to LPassCount - 1 do
    if APasses[I].AdapterKind = wpakPattern3D then FHasPattern3D := True;
  for I := 0 to LBridgeCount - 1 do
    if ABridges[I].Kind = wpbkPattern3DProjection then FHasPattern3D := True;
  LVersions := NormalizeVersions(AVersions, FHasPattern3D);
  if FHasPattern3D then
  begin
    RequireQuotaInteger(ARank, 'pattern3d recipe rank');
    RequireConnectivityBoolean(AWrapNeighbors, 'pattern3d recipe wrap policy');
  end;
  if LConnectivityCount <> 0 then
    RequireQuotaInteger(ARank, 'connectivity recipe rank');

  { Preflight all nested record counts and all copied outer token bytes before
    allocating owner arrays or invoking a nested resource decoder. }
  LTotalAllowedTokenCount := 0;
  LTotalEncodedTokenLength := 0;
  LTotalPayloadLength := 0;
  LTotalRequirementTermCount := 0;
  LTotalResourceRelationSlotCount := 0;
  LTotalQuotaTokenCount := 0;
  LTotalConnectivityValueCount := 0;
  LTotalConnectivityPositionCount := 0;
  AccumulateOuterToken(AMetadata.Name, 'pipeline name', False);
  AccumulateOuterToken(AMetadata.LicenseIdentifier,
    'pipeline license identifier', False);
  AccumulateOuterToken(AMetadata.SourceDescription,
    'pipeline source description', True);
  AccumulateOuterToken(AMetadata.SourceFingerprint,
    'pipeline source fingerprint', True);
  for I := 0 to LResourceCount - 1 do
  begin
    LCurrentPayloadLength := CheckedLength(
      Length(AResources[I].Document), Format('resource %d payload', [I]),
      WFC_PIPELINE_MAX_RESOURCE_PAYLOAD_LENGTH);
    if not TextIsAscii(AResources[I].Document) then
      raise EWfcPipelineModel.CreateFmt(
        'resource %d document must be canonical ASCII', [I]);
    if LTotalPayloadLength >
        WFC_PIPELINE_MAX_TOTAL_RESOURCE_PAYLOAD_LENGTH -
        LCurrentPayloadLength then
      raise EWfcPipelineModel.Create(
        'aggregate resource payload exceeds the version-1 limit');
    Inc(LTotalPayloadLength, LCurrentPayloadLength);
    AccumulateOuterToken(AResources[I].Id,
      Format('resource %d id', [I]), False);
    AccumulateOuterToken(AResources[I].SourceDescription,
      Format('resource %d source description', [I]), False);
    AccumulateOuterToken(AResources[I].SourceLicenseIdentifier,
      Format('resource %d source license identifier', [I]), False);
    AccumulateOuterToken(AResources[I].SourceFingerprint,
      Format('resource %d source fingerprint', [I]), True);
  end;
  for I := 0 to LPassCount - 1 do
    AccumulateOuterToken(APasses[I].LabelName,
      Format('pass %d label', [I]), False);
  for I := 0 to LRequirementCount - 1 do
  begin
    AccumulateOuterToken(ARequirements[I].ConsumerToken,
      Format('requirement %d consumer token', [I]), False);
    LTermCount := CheckedLength(Length(ARequirements[I].Terms),
      Format('requirement %d term count', [I]),
      WFC_PIPELINE_MAX_REQUIREMENT_TERM_COUNT);
    if LTotalRequirementTermCount >
        WFC_PIPELINE_MAX_TOTAL_REQUIREMENT_TERM_COUNT - LTermCount then
      raise EWfcPipelineModel.Create(
        'aggregate requirement-term count exceeds the version-1 limit');
    Inc(LTotalRequirementTermCount, LTermCount);
    for J := 0 to LTermCount - 1 do
    begin
      LAllowedCount := CheckedLength(
        Length(ARequirements[I].Terms[J].AllowedProviderTokens),
        Format('requirement %d term %d allowed-token count', [I, J]),
        WFC_PIPELINE_MAX_ALLOWED_TOKEN_COUNT);
      if LTotalAllowedTokenCount >
          WFC_PIPELINE_MAX_TOTAL_ALLOWED_TOKEN_COUNT - LAllowedCount then
        raise EWfcPipelineModel.Create(
          'aggregate allowed-token count exceeds the version-1 limit');
      Inc(LTotalAllowedTokenCount, LAllowedCount);
      for K := 0 to LAllowedCount - 1 do
        AccumulateOuterToken(
          ARequirements[I].Terms[J].AllowedProviderTokens[K],
          Format('requirement %d term %d allowed token %d', [I, J, K]),
          False);
    end;
  end;

  for I := 0 to LQuotaCount - 1 do
  begin
    RequireQuotaInteger(AValueQuotas[I].PassIndex,
      Format('value-quota %d pass index', [I]));
    RequireQuotaInteger(AValueQuotas[I].MinimumCount,
      Format('value-quota %d minimum', [I]));
    RequireQuotaInteger(AValueQuotas[I].MaximumCount,
      Format('value-quota %d maximum', [I]));
    if AValueQuotas[I].MinimumCount > AValueQuotas[I].MaximumCount then
      raise EWfcPipelineModel.CreateFmt(
        'value-quota %d bounds must satisfy 0 <= minimum <= maximum', [I]);
    AccumulateOuterToken(AValueQuotas[I].LabelText,
      Format('value-quota %d label', [I]), False);
    LAllowedCount := CheckedLength(Length(AValueQuotas[I].Values),
      Format('value-quota %d token count', [I]),
      WFC_PIPELINE_MAX_VALUE_QUOTA_TOKEN_COUNT);
    if LAllowedCount = 0 then
      raise EWfcPipelineModel.CreateFmt(
        'value-quota %d must contain at least one public token', [I]);
    if LTotalQuotaTokenCount >
        WFC_PIPELINE_MAX_TOTAL_VALUE_QUOTA_TOKEN_COUNT - LAllowedCount then
      raise EWfcPipelineModel.Create(
        'aggregate value-quota token count exceeds the quota version-1 limit');
    Inc(LTotalQuotaTokenCount, LAllowedCount);
    for J := 0 to LAllowedCount - 1 do
      AccumulateOuterToken(AValueQuotas[I].Values[J],
        Format('value-quota %d token %d', [I, J]), False);
  end;

  for I := 0 to LConnectivityCount - 1 do
  begin
    RequireQuotaInteger(AConnectivities[I].PassIndex,
      Format('connectivity %d pass index', [I]));
    AccumulateOuterToken(AConnectivities[I].LabelText,
      Format('connectivity %d label', [I]), False);
    RequireConnectivityPosition(AConnectivities[I].Root, ARank,
      Format('connectivity %d root', [I]));
    RequireConnectivityBoolean(AConnectivities[I].RequireAllParticipants,
      Format('connectivity %d all-participants flag', [I]));
    LTermCount := CheckedLength(Length(AConnectivities[I].RequiredPositions),
      Format('connectivity %d terminal count', [I]),
      WFC_PIPELINE_MAX_CONNECTIVITY_REQUIRED_POSITION_COUNT);
    if LTotalConnectivityPositionCount >
        WFC_PIPELINE_MAX_TOTAL_CONNECTIVITY_REQUIRED_POSITION_COUNT - LTermCount then
      raise EWfcPipelineModel.Create('aggregate connectivity terminal count exceeds the limit');
    Inc(LTotalConnectivityPositionCount, LTermCount);
    LAllowedCount := CheckedLength(Length(AConnectivities[I].Values),
      Format('connectivity %d profile count', [I]),
      WFC_PIPELINE_MAX_CONNECTIVITY_VALUE_COUNT);
    if LAllowedCount = 0 then
      raise EWfcPipelineModel.CreateFmt('connectivity %d requires participating profiles', [I]);
    if LTotalConnectivityValueCount >
        WFC_PIPELINE_MAX_TOTAL_CONNECTIVITY_VALUE_COUNT - LAllowedCount then
      raise EWfcPipelineModel.Create('aggregate connectivity profile count exceeds the limit');
    Inc(LTotalConnectivityValueCount, LAllowedCount);
    for J := 0 to LTermCount - 1 do
    begin
      RequireConnectivityPosition(AConnectivities[I].RequiredPositions[J], ARank,
        Format('connectivity %d terminal %d', [I, J]));
      if (AConnectivities[I].RequiredPositions[J].X = AConnectivities[I].Root.X) and
          (AConnectivities[I].RequiredPositions[J].Y = AConnectivities[I].Root.Y) and
          (AConnectivities[I].RequiredPositions[J].Z = AConnectivities[I].Root.Z) then
        raise EWfcPipelineModel.CreateFmt('connectivity %d terminal repeats the root', [I]);
      if (J <> 0) and not ConnectivityPositionBefore(
          AConnectivities[I].RequiredPositions[J - 1],
          AConnectivities[I].RequiredPositions[J]) then
        raise EWfcPipelineModel.CreateFmt(
          'connectivity %d terminals must use strict Z,Y,X order', [I]);
    end;
    for J := 0 to LAllowedCount - 1 do
    begin
      AccumulateOuterToken(AConnectivities[I].Values[J].Value,
        Format('connectivity %d profile %d token', [I, J]), False);
      RequireConnectivityBoolean(AConnectivities[I].Values[J].RequiredByValue,
        Format('connectivity %d profile %d required-by-value flag', [I, J]));
      LDirections := [];
      for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
        if LDirection in AConnectivities[I].Values[J].Openings then
          Include(LDirections, LDirection);
      if LDirections <> AConnectivities[I].Values[J].Openings then
        raise EWfcPipelineModel.CreateFmt(
          'connectivity %d profile %d has an invalid opening direction', [I, J]);
    end;
  end;

  FMetadata := AMetadata;
  FVersions := LVersions;
  FRank := ARank;
  FWrapNeighbors := AWrapNeighbors;
  FRunMode := ARunMode;

  SetLength(FResources, LResourceCount);
  SetLength(FModelResources, LResourceCount);
  SetLength(FRuleResources, LResourceCount);
  SetLength(FPatternResources, LResourceCount);
  SetLength(FSequenceResources, LResourceCount);
  SetLength(FPattern3DResources, LResourceCount);
  for I := 0 to LResourceCount - 1 do
  begin
    RequireEnumResourceKind(AResources[I].Kind,
      Format('resource %d kind', [I]));
    RequireToken(AResources[I].Id, Format('resource %d id', [I]), False);
    RequireToken(AResources[I].SourceDescription,
      Format('resource %d source description', [I]), False);
    RequireToken(AResources[I].SourceLicenseIdentifier,
      Format('resource %d source license identifier', [I]), False);
    RequireToken(AResources[I].SourceFingerprint,
      Format('resource %d source fingerprint', [I]), True);
    for J := 0 to I - 1 do
      if AResources[J].Id = AResources[I].Id then
        raise EWfcPipelineModel.CreateFmt(
          'resource ids must be unique [%d, %d]', [J, I]);

    FResources[I] := AResources[I];
    try
      LCurrentRelationSlotCount := 0;
      case AResources[I].Kind of
        wprkModel:
          begin
            FModelResources[I] :=
              DecodeWfcModelText(AResources[I].Document);
            LCurrentRelationSlotCount := CheckedDenseRelationSlotCount(
              FModelResources[I].ValueCount,
              Format('resource %d model relation slots', [I]),
              WfcModelStoredDirectionCount(FModelResources[I].Rank));
            if EncodeWfcModelText(FModelResources[I]) <>
                AResources[I].Document then
              raise EWfcPipelineModel.CreateFmt(
                'resource %d model document is not canonical', [I]);
          end;
        wprkRules:
          begin
            FRuleResources[I] :=
              DecodeWfcRuleText(AResources[I].Document);
            if EncodeWfcRuleText(FRuleResources[I]) <>
                AResources[I].Document then
              raise EWfcPipelineModel.CreateFmt(
                'resource %d rules document is not canonical', [I]);
          end;
        wprkPattern2D:
          begin
            FPatternResources[I] :=
              DecodeWfcPattern2DText(AResources[I].Document);
            LCurrentRelationSlotCount := CheckedDenseRelationSlotCount(
              FPatternResources[I].PatternCount,
              Format('resource %d pattern relation slots', [I]));
            if EncodeWfcPattern2DText(FPatternResources[I]) <>
                AResources[I].Document then
              raise EWfcPipelineModel.CreateFmt(
                'resource %d pattern2d document is not canonical', [I]);
          end;
        wprkSequence:
          begin
            FSequenceResources[I] :=
              DecodeWfcSequenceText(AResources[I].Document);
            LCurrentRelationSlotCount := CheckedDenseRelationSlotCount(
              FSequenceResources[I].StateCount,
              Format('resource %d sequence relation slots', [I]));
            if EncodeWfcSequenceText(FSequenceResources[I]) <>
                AResources[I].Document then
              raise EWfcPipelineModel.CreateFmt(
                'resource %d sequence document is not canonical', [I]);
          end;
        wprkPattern3D:
          begin
            FPattern3DResources[I] := DecodeWfcPattern3DText(AResources[I].Document);
            LCurrentRelationSlotCount := CheckedDenseRelationSlotCount(
              FPattern3DResources[I].PatternCount,
              Format('resource %d pattern3d relation slots', [I]), 6);
            if EncodeWfcPattern3DText(FPattern3DResources[I]) <> AResources[I].Document then
              raise EWfcPipelineModel.CreateFmt(
                'resource %d pattern3d document is not canonical', [I]);
          end;
      end;
      if LTotalResourceRelationSlotCount >
          WFC_PIPELINE_MAX_TOTAL_RESOURCE_RELATION_SLOT_COUNT -
          LCurrentRelationSlotCount then
        raise EWfcPipelineModel.Create(
          'aggregate typed-resource relation slots exceed the version-1 limit');
      Inc(LTotalResourceRelationSlotCount, LCurrentRelationSlotCount);
    except
      on E: EWfcPipelineModel do
        raise;
      on E: Exception do
        raise EWfcPipelineModel.CreateFmt(
          'resource %d cannot be decoded as its declared kind: %s',
          [I, E.Message]);
    end;
  end;

  SetLength(FPasses, LPassCount);
  SetLength(FVocabularies, LPassCount);
  for I := 0 to LPassCount - 1 do
  begin
    RequireToken(APasses[I].LabelName, Format('pass %d label', [I]), False);
    for J := 0 to I - 1 do
      if APasses[J].LabelName = APasses[I].LabelName then
        raise EWfcPipelineModel.CreateFmt(
          'pass labels must be unique [%d, %d]', [J, I]);
    RequireEnumVisibility(APasses[I].Visibility,
      Format('pass %d visibility', [I]));
    RequireEnumPassMode(APasses[I].Mode, Format('pass %d mode', [I]));
    RequireEnumAdapter(APasses[I].AdapterKind,
      Format('pass %d adapter', [I]));
    if FHasPattern3D then
      RequireConnectivityBoolean(APasses[I].HasSequenceExtent,
        Format('pass %d sequence extent flag', [I]));

    if APasses[I].Mode = gpmTransform then
    begin
      ValidateIndex(APasses[I].TransformSourceIndex, LPassCount,
        Format('pass %d transform source', [I]));
      if APasses[I].TransformSourceIndex = I then
        raise EWfcPipelineModel.CreateFmt(
          'pass %d cannot transform itself', [I]);
      if APasses[I].AdapterKind <> wpakEmpty then
        raise EWfcPipelineModel.CreateFmt(
          'transform pass %d must use the empty adapter', [I]);
    end
    else if APasses[I].TransformSourceIndex <> WFC_PIPELINE_NO_INDEX then
      raise EWfcPipelineModel.CreateFmt(
        'non-transform pass %d cannot name a transform source', [I]);

    case APasses[I].AdapterKind of
      wpakEmpty:
        begin
          if APasses[I].ResourceIndex <> WFC_PIPELINE_NO_INDEX then
            raise EWfcPipelineModel.CreateFmt(
              'empty pass %d cannot name a resource', [I]);
          if APasses[I].HasSequenceExtent or
              (APasses[I].SequenceExtent <> wseWhole) then
            raise EWfcPipelineModel.CreateFmt(
              'empty pass %d cannot name a sequence extent', [I]);
        end;
      wpakModel:
        begin
          ValidateIndex(APasses[I].ResourceIndex, LResourceCount,
            Format('pass %d resource', [I]));
          if FResources[APasses[I].ResourceIndex].Kind <> wprkModel then
            raise EWfcPipelineModel.CreateFmt(
              'pass %d model adapter requires a model resource', [I]);
          if FModelResources[APasses[I].ResourceIndex].Rank <> ARank then
            raise EWfcPipelineModel.CreateFmt(
              'pass %d model rank does not match pipeline rank', [I]);
          if APasses[I].HasSequenceExtent or
              (APasses[I].SequenceExtent <> wseWhole) then
            raise EWfcPipelineModel.CreateFmt(
              'model pass %d cannot name a sequence extent', [I]);
          if APasses[I].Visibility = wppvPublic then
            FVocabularies[I] :=
              FModelResources[APasses[I].ResourceIndex].CopyTokens;
        end;
      wpakRules:
        begin
          ValidateIndex(APasses[I].ResourceIndex, LResourceCount,
            Format('pass %d resource', [I]));
          if FResources[APasses[I].ResourceIndex].Kind <> wprkRules then
            raise EWfcPipelineModel.CreateFmt(
              'pass %d rules adapter requires a rules resource', [I]);
          if FRuleResources[APasses[I].ResourceIndex].Rank <> ARank then
            raise EWfcPipelineModel.CreateFmt(
              'pass %d rules rank does not match pipeline rank', [I]);
          if APasses[I].HasSequenceExtent or
              (APasses[I].SequenceExtent <> wseWhole) then
            raise EWfcPipelineModel.CreateFmt(
              'rules pass %d cannot name a sequence extent', [I]);
          if APasses[I].Visibility = wppvPublic then
            FVocabularies[I] :=
              FRuleResources[APasses[I].ResourceIndex].CopyTokens;
        end;
      wpakPattern2D:
        begin
          ValidateIndex(APasses[I].ResourceIndex, LResourceCount,
            Format('pass %d resource', [I]));
          if FResources[APasses[I].ResourceIndex].Kind <> wprkPattern2D then
            raise EWfcPipelineModel.CreateFmt(
              'pass %d pattern2d adapter requires a pattern2d resource', [I]);
          if ARank <> 2 then
            raise EWfcPipelineModel.CreateFmt(
              'pass %d pattern2d adapter requires pipeline rank 2', [I]);
          if APasses[I].Visibility <> wppvPrivate then
            raise EWfcPipelineModel.CreateFmt(
              'pass %d pattern2d adapter must remain private', [I]);
          if APasses[I].HasSequenceExtent or
              (APasses[I].SequenceExtent <> wseWhole) then
            raise EWfcPipelineModel.CreateFmt(
              'pattern2d pass %d cannot name a sequence extent', [I]);
        end;
      wpakSequence:
        begin
          ValidateIndex(APasses[I].ResourceIndex, LResourceCount,
            Format('pass %d resource', [I]));
          if FResources[APasses[I].ResourceIndex].Kind <> wprkSequence then
            raise EWfcPipelineModel.CreateFmt(
              'pass %d sequence adapter requires a sequence resource', [I]);
          if ARank <> 1 then
            raise EWfcPipelineModel.CreateFmt(
              'pass %d sequence adapter requires pipeline rank 1', [I]);
          if APasses[I].Visibility <> wppvPrivate then
            raise EWfcPipelineModel.CreateFmt(
              'pass %d sequence adapter must remain private', [I]);
          if not APasses[I].HasSequenceExtent then
            raise EWfcPipelineModel.CreateFmt(
              'pass %d sequence adapter requires an extent', [I]);
          RequireEnumSequenceExtent(APasses[I].SequenceExtent,
            Format('pass %d sequence extent', [I]));
          if AWrapNeighbors and
              (APasses[I].SequenceExtent <> wseWrap) then
            raise EWfcPipelineModel.CreateFmt(
              'wrapped sequence pass %d requires wrap extent', [I]);
          if (not AWrapNeighbors) and
              (APasses[I].SequenceExtent = wseWrap) then
            raise EWfcPipelineModel.CreateFmt(
              'open sequence pass %d cannot use wrap extent', [I]);
        end;
      wpakPattern3D:
        begin
          RequireQuotaInteger(APasses[I].ResourceIndex, Format('pass %d resource', [I]));
          ValidateIndex(APasses[I].ResourceIndex, LResourceCount,
            Format('pass %d resource', [I]));
          if FResources[APasses[I].ResourceIndex].Kind <> wprkPattern3D then
            raise EWfcPipelineModel.CreateFmt(
              'pass %d pattern3d adapter requires a pattern3d resource', [I]);
          if ARank <> 3 then
            raise EWfcPipelineModel.CreateFmt(
              'pass %d pattern3d adapter requires pipeline rank 3', [I]);
          if APasses[I].Visibility <> wppvPrivate then
            raise EWfcPipelineModel.CreateFmt(
              'pass %d pattern3d adapter must remain private', [I]);
          RequireConnectivityBoolean(APasses[I].HasSequenceExtent,
            Format('pattern3d pass %d sequence extent flag', [I]));
          if APasses[I].HasSequenceExtent or (APasses[I].SequenceExtent <> wseWhole) then
            raise EWfcPipelineModel.CreateFmt(
              'pattern3d pass %d cannot name a sequence extent', [I]);
        end;
    end;
    FPasses[I] := APasses[I];
  end;

  SetLength(FDependencies, LDependencyCount);
  SetLength(LIndegree, LPassCount);
  for I := 0 to LDependencyCount - 1 do
  begin
    ValidateIndex(ADependencies[I].ConsumerPassIndex, LPassCount,
      Format('dependency %d consumer', [I]));
    ValidateIndex(ADependencies[I].ProviderPassIndex, LPassCount,
      Format('dependency %d provider', [I]));
    if ADependencies[I].ConsumerPassIndex =
        ADependencies[I].ProviderPassIndex then
      raise EWfcPipelineModel.CreateFmt(
        'dependency %d cannot target its own pass', [I]);
    for J := 0 to I - 1 do
      if (ADependencies[J].ConsumerPassIndex =
          ADependencies[I].ConsumerPassIndex) and
          (ADependencies[J].ProviderPassIndex =
          ADependencies[I].ProviderPassIndex) then
        raise EWfcPipelineModel.CreateFmt(
          'dependency %d duplicates dependency %d', [I, J]);
    FDependencies[I] := ADependencies[I];
    Inc(LIndegree[ADependencies[I].ConsumerPassIndex]);
  end;

  SetLength(LProcessed, LPassCount);
  LProcessedCount := 0;
  repeat
    LProgress := False;
    for I := 0 to LPassCount - 1 do
      if (not LProcessed[I]) and (LIndegree[I] = 0) then
      begin
        LProcessed[I] := True;
        Inc(LProcessedCount);
        LProgress := True;
        for J := 0 to LDependencyCount - 1 do
          if ADependencies[J].ProviderPassIndex = I then
            Dec(LIndegree[ADependencies[J].ConsumerPassIndex]);
      end;
  until not LProgress;
  if LProcessedCount <> LPassCount then
    raise EWfcPipelineModel.Create('pipeline dependencies contain a cycle');

  for I := 0 to LPassCount - 1 do
  begin
    if FPasses[I].Mode = gpmTransform then
      RequireDependency(I, FPasses[I].TransformSourceIndex,
        Format('transform pass %d', [I]));
    if (FPasses[I].Mode = gpmLegacy) and (I > 0) then
      RequireDependency(I, I - 1, Format('legacy pass %d', [I]));
  end;

  SetLength(FBridges, LBridgeCount);
  for I := 0 to LBridgeCount - 1 do
  begin
    RequireEnumBridge(ABridges[I].Kind, Format('bridge %d kind', [I]));
    ValidateIndex(ABridges[I].SourcePassIndex, LPassCount,
      Format('bridge %d source', [I]));
    ValidateIndex(ABridges[I].TargetPassIndex, LPassCount,
      Format('bridge %d target', [I]));
    if ABridges[I].SourcePassIndex = ABridges[I].TargetPassIndex then
      raise EWfcPipelineModel.CreateFmt(
        'bridge %d cannot target its source pass', [I]);
    RequireDependency(ABridges[I].TargetPassIndex,
      ABridges[I].SourcePassIndex, Format('bridge %d', [I]));
    for J := 0 to I - 1 do
      if ABridges[J].TargetPassIndex = ABridges[I].TargetPassIndex then
        raise EWfcPipelineModel.CreateFmt(
          'bridge targets must have one vocabulary owner [%d, %d]', [J, I]);

    case ABridges[I].Kind of
      wpbkPattern2DProjection:
        begin
          if (ARank <> 2) or (not AWrapNeighbors) then
            raise EWfcPipelineModel.CreateFmt(
              'pattern2d bridge %d requires a wrapped rank-2 pipeline', [I]);
          if (FPasses[ABridges[I].SourcePassIndex].AdapterKind <>
              wpakPattern2D) or
              (FPasses[ABridges[I].SourcePassIndex].Visibility <>
              wppvPrivate) then
            raise EWfcPipelineModel.CreateFmt(
              'pattern2d bridge %d requires a private pattern2d source', [I]);
          if (FPasses[ABridges[I].TargetPassIndex].AdapterKind <> wpakEmpty) or
              (FPasses[ABridges[I].TargetPassIndex].Visibility <>
              wppvPublic) or
              (FPasses[ABridges[I].TargetPassIndex].Mode <> gpmOverlay) then
            raise EWfcPipelineModel.CreateFmt(
              'pattern2d bridge %d requires an empty public overlay target',
              [I]);
          for J := 0 to FPatternResources[
              FPasses[ABridges[I].SourcePassIndex].ResourceIndex
              ].PaletteCount - 1 do
            if WfcPattern2DTokenUsesReservedKeySyntax(
                FPatternResources[
                  FPasses[ABridges[I].SourcePassIndex].ResourceIndex
                  ].PaletteTokenAt(J)) then
              raise EWfcPipelineModel.CreateFmt(
                'pattern2d bridge %d palette token %d uses the reserved latent-key syntax',
                [I, J]);
          AssignVocabulary(ABridges[I].TargetPassIndex,
            FPatternResources[
              FPasses[ABridges[I].SourcePassIndex].ResourceIndex].CopyPalette,
            Format('pattern2d bridge %d', [I]));
        end;
      wpbkSequenceProjection:
        begin
          if ARank <> 1 then
            raise EWfcPipelineModel.CreateFmt(
              'sequence bridge %d requires a rank-1 pipeline', [I]);
          if (FPasses[ABridges[I].SourcePassIndex].AdapterKind <>
              wpakSequence) or
              (FPasses[ABridges[I].SourcePassIndex].Visibility <>
              wppvPrivate) then
            raise EWfcPipelineModel.CreateFmt(
              'sequence bridge %d requires a private sequence source', [I]);
          if (FPasses[ABridges[I].TargetPassIndex].AdapterKind <> wpakEmpty) or
              (FPasses[ABridges[I].TargetPassIndex].Visibility <>
              wppvPublic) or
              (FPasses[ABridges[I].TargetPassIndex].Mode <> gpmOverlay) then
            raise EWfcPipelineModel.CreateFmt(
              'sequence bridge %d requires an empty public overlay target',
              [I]);
          AssignVocabulary(ABridges[I].TargetPassIndex,
            FSequenceResources[
              FPasses[ABridges[I].SourcePassIndex].ResourceIndex].CopyPublicTokens,
            Format('sequence bridge %d', [I]));
        end;
      wpbkPattern3DProjection:
        begin
          if (ARank <> 3) or (not AWrapNeighbors) then
            raise EWfcPipelineModel.CreateFmt(
              'pattern3d bridge %d requires a wrapped rank-3 pipeline', [I]);
          if (FPasses[ABridges[I].SourcePassIndex].AdapterKind <> wpakPattern3D) or
              (FPasses[ABridges[I].SourcePassIndex].Visibility <> wppvPrivate) then
            raise EWfcPipelineModel.CreateFmt(
              'pattern3d bridge %d requires a private pattern3d source', [I]);
          if (FPasses[ABridges[I].TargetPassIndex].AdapterKind <> wpakEmpty) or
              (FPasses[ABridges[I].TargetPassIndex].Visibility <> wppvPublic) or
              (FPasses[ABridges[I].TargetPassIndex].Mode <> gpmOverlay) then
            raise EWfcPipelineModel.CreateFmt(
              'pattern3d bridge %d requires an empty public overlay target', [I]);
          for J := 0 to FPattern3DResources[
              FPasses[ABridges[I].SourcePassIndex].ResourceIndex].PaletteCount - 1 do
            if WfcPattern3DTokenUsesReservedKeySyntax(FPattern3DResources[
                FPasses[ABridges[I].SourcePassIndex].ResourceIndex].PaletteTokenAt(J)) then
              raise EWfcPipelineModel.CreateFmt(
                'pattern3d bridge %d palette token %d uses the reserved latent-key syntax', [I, J]);
          { A palette token need not occur at every footprint offset. A wrapped
            output may use only a feasible subset, or be contradictory. }
          AssignVocabulary(ABridges[I].TargetPassIndex,
            FPattern3DResources[FPasses[ABridges[I].SourcePassIndex].ResourceIndex].CopyPalette,
            Format('pattern3d bridge %d', [I]));
        end;
    end;
    FBridges[I] := ABridges[I];
  end;

  { Resolve empty public transform vocabularies without assuming declaration
    order. The already-proved DAG guarantees that a valid chain terminates. }
  for LIteration := 0 to LPassCount - 1 do
    for I := 0 to LPassCount - 1 do
      if (FPasses[I].Visibility = wppvPublic) and
          (FPasses[I].AdapterKind = wpakEmpty) and
          (FPasses[I].Mode = gpmTransform) and
          (Length(FVocabularies[I]) = 0) then
      begin
        J := FPasses[I].TransformSourceIndex;
        if FPasses[J].Visibility <> wppvPublic then
          raise EWfcPipelineModel.CreateFmt(
            'public transform pass %d cannot expose a private source', [I]);
        if Length(FVocabularies[J]) <> 0 then
          FVocabularies[I] := CloneTokens(FVocabularies[J]);
      end;

  for I := 0 to LPassCount - 1 do
    if (FPasses[I].Visibility = wppvPublic) and
        (FPasses[I].AdapterKind = wpakEmpty) and
        (Length(FVocabularies[I]) = 0) then
    begin
      if BridgeTargetsPass(I) then
        raise EWfcPipelineModel.CreateFmt(
          'public pass %d bridge produced an empty vocabulary', [I]);
      raise EWfcPipelineModel.CreateFmt(
        'empty public pass %d has no statically known vocabulary owner', [I]);
    end;

  SetLength(FRequirements, LRequirementCount);
  for I := 0 to LRequirementCount - 1 do
  begin
    ValidateIndex(ARequirements[I].ConsumerPassIndex, LPassCount,
      Format('requirement %d consumer', [I]));
    ValidateIndex(ARequirements[I].ProviderPassIndex, LPassCount,
      Format('requirement %d provider', [I]));
    if ARequirements[I].ConsumerPassIndex =
        ARequirements[I].ProviderPassIndex then
      raise EWfcPipelineModel.CreateFmt(
        'requirement %d cannot read its own pass', [I]);
    if (FPasses[ARequirements[I].ConsumerPassIndex].Visibility <>
        wppvPublic) or
        (FPasses[ARequirements[I].ProviderPassIndex].Visibility <>
        wppvPublic) then
      raise EWfcPipelineModel.CreateFmt(
        'requirement %d endpoints must both be public', [I]);
    RequireDependency(ARequirements[I].ConsumerPassIndex,
      ARequirements[I].ProviderPassIndex, Format('requirement %d', [I]));
    RequireEnumRequirement(ARequirements[I].Kind,
      Format('requirement %d kind', [I]));
    RequireToken(ARequirements[I].ConsumerToken,
      Format('requirement %d consumer token', [I]), False);
    if TokenIndex(FVocabularies[ARequirements[I].ConsumerPassIndex],
        ARequirements[I].ConsumerToken) < 0 then
      raise EWfcPipelineModel.CreateFmt(
        'requirement %d consumer token is outside its public vocabulary', [I]);
    LTermCount := CheckedLength(Length(ARequirements[I].Terms),
      Format('requirement %d term count', [I]),
      WFC_PIPELINE_MAX_REQUIREMENT_TERM_COUNT);
    if (ARequirements[I].Kind = wprqExact) and (LTermCount <> 1) then
      raise EWfcPipelineModel.CreateFmt(
        'exact requirement %d must contain exactly one term', [I]);
    if (ARequirements[I].Kind = wprqAny) and (LTermCount = 0) then
      raise EWfcPipelineModel.CreateFmt(
        'any requirement %d must contain at least one term', [I]);
    if ARequirements[I].Kind = wprqCount then
    begin
      if FHasPattern3D then
      begin
        RequireQuotaInteger(ARequirements[I].MinimumCount, 'pattern3d requirement minimum');
        RequireQuotaInteger(ARequirements[I].MaximumCount, 'pattern3d requirement maximum');
      end;
      if LTermCount = 0 then
        raise EWfcPipelineModel.CreateFmt(
          'count requirement %d must contain at least one term', [I]);
      RequireEnumCountMode(ARequirements[I].CountMode,
        Format('requirement %d count mode', [I]));
      if (ARequirements[I].MinimumCount < 0) or
          (ARequirements[I].MaximumCount <
            ARequirements[I].MinimumCount) or
          (ARequirements[I].MaximumCount > LTermCount) then
        raise EWfcPipelineModel.CreateFmt(
          'count requirement %d bounds must satisfy 0 <= minimum <= maximum <= %d',
          [I, LTermCount]);
    end;

    for J := 0 to LTermCount - 1 do
    begin
      if FHasPattern3D then
      begin
        RequirePattern3DSignedInteger(ARequirements[I].Terms[J].OffsetX, 'pattern3d requirement offset X');
        RequirePattern3DSignedInteger(ARequirements[I].Terms[J].OffsetY, 'pattern3d requirement offset Y');
        RequirePattern3DSignedInteger(ARequirements[I].Terms[J].OffsetZ, 'pattern3d requirement offset Z');
      end;
      if (ARank = 1) and
          ((ARequirements[I].Terms[J].OffsetY <> 0) or
           (ARequirements[I].Terms[J].OffsetZ <> 0)) then
        raise EWfcPipelineModel.CreateFmt(
          'requirement %d term %d uses an inactive rank-1 axis', [I, J]);
      if (ARank = 2) and
          (ARequirements[I].Terms[J].OffsetZ <> 0) then
        raise EWfcPipelineModel.CreateFmt(
          'requirement %d term %d uses an inactive rank-2 axis', [I, J]);
      if (J > 0) and
          (ARequirements[I].Kind in [wprqAny, wprqCount]) and
          (not TermsAreStrictlyOrdered(ARequirements[I].Terms[J - 1],
            ARequirements[I].Terms[J])) then
        raise EWfcPipelineModel.CreateFmt(
          'requirement %d terms must use strict X/Y/Z order', [I]);
      LAllowedCount := CheckedLength(
        Length(ARequirements[I].Terms[J].AllowedProviderTokens),
        Format('requirement %d term %d allowed-token count', [I, J]),
        WFC_PIPELINE_MAX_ALLOWED_TOKEN_COUNT);
      if LAllowedCount = 0 then
        raise EWfcPipelineModel.CreateFmt(
          'requirement %d term %d must allow at least one token', [I, J]);
      LPreviousTokenIndex := -1;
      for K := 0 to LAllowedCount - 1 do
      begin
        RequireToken(ARequirements[I].Terms[J].AllowedProviderTokens[K],
          Format('requirement %d term %d allowed token %d', [I, J, K]),
          False);
        LProviderTokenIndex := TokenIndex(
          FVocabularies[ARequirements[I].ProviderPassIndex],
          ARequirements[I].Terms[J].AllowedProviderTokens[K]);
        if LProviderTokenIndex < 0 then
          raise EWfcPipelineModel.CreateFmt(
            'requirement %d term %d token %d is outside the provider vocabulary',
            [I, J, K]);
        if LProviderTokenIndex <= LPreviousTokenIndex then
          raise EWfcPipelineModel.CreateFmt(
            'requirement %d term %d allowed tokens must use provider-vocabulary order',
            [I, J]);
        LPreviousTokenIndex := LProviderTokenIndex;
      end;
    end;

    if (FPasses[ARequirements[I].ConsumerPassIndex].AdapterKind =
        wpakEmpty) and
        (not BridgeTargetsPass(ARequirements[I].ConsumerPassIndex)) then
      raise EWfcPipelineModel.CreateFmt(
        'requirement %d consumer has no materialized rule definition', [I]);

    if ARequirements[I].Kind = wprqExact then
      for J := 0 to I - 1 do
        if (ARequirements[J].Kind = wprqExact) and
            (ARequirements[J].ConsumerPassIndex =
              ARequirements[I].ConsumerPassIndex) and
            (ARequirements[J].ConsumerToken =
              ARequirements[I].ConsumerToken) and
            (ARequirements[J].ProviderPassIndex =
              ARequirements[I].ProviderPassIndex) and
            (ARequirements[J].Terms[0].OffsetX =
              ARequirements[I].Terms[0].OffsetX) and
            (ARequirements[J].Terms[0].OffsetY =
              ARequirements[I].Terms[0].OffsetY) and
            (ARequirements[J].Terms[0].OffsetZ =
              ARequirements[I].Terms[0].OffsetZ) then
          raise EWfcPipelineModel.CreateFmt(
            'exact requirement %d duplicates requirement %d', [I, J]);

    FRequirements[I] := CloneRequirement(ARequirements[I]);
  end;

  { Resolve quotas only after bridges and transform aliases have their final
    statically known public vocabulary. Private latent state keys never leak
    into the portable quota surface. No dependency is introduced by a quota. }
  SetLength(FValueQuotas, LQuotaCount);
  for I := 0 to LQuotaCount - 1 do
  begin
    ValidateIndex(AValueQuotas[I].PassIndex, LPassCount,
      Format('value-quota %d owner', [I]));
    J := AValueQuotas[I].PassIndex;
    if FPasses[J].Visibility <> wppvPublic then
      raise EWfcPipelineModel.CreateFmt(
        'value-quota %d owner must be public', [I]);
    if Length(FVocabularies[J]) = 0 then
      raise EWfcPipelineModel.CreateFmt(
        'value-quota %d owner has no statically known public vocabulary', [I]);
    for K := 0 to I - 1 do
      if (AValueQuotas[K].PassIndex = J) and
          (AValueQuotas[K].LabelText = AValueQuotas[I].LabelText) then
        raise EWfcPipelineModel.CreateFmt(
          'value-quota %d duplicates the pass/label key of quota %d', [I, K]);
    LPreviousTokenIndex := -1;
    for K := 0 to Length(AValueQuotas[I].Values) - 1 do
    begin
      LProviderTokenIndex := TokenIndex(FVocabularies[J], AValueQuotas[I].Values[K]);
      if LProviderTokenIndex < 0 then
        raise EWfcPipelineModel.CreateFmt(
          'value-quota %d token %d is outside the public vocabulary', [I, K]);
      if LProviderTokenIndex <= LPreviousTokenIndex then
        raise EWfcPipelineModel.CreateFmt(
          'value-quota %d tokens must use strict public-vocabulary order', [I]);
      LPreviousTokenIndex := LProviderTokenIndex;
    end;
    FValueQuotas[I] := CloneValueQuota(AValueQuotas[I]);
  end;

  SetLength(FConnectivities, LConnectivityCount);
  for I := 0 to LConnectivityCount - 1 do
  begin
    ValidateIndex(AConnectivities[I].PassIndex, LPassCount,
      Format('connectivity %d owner', [I]));
    J := AConnectivities[I].PassIndex;
    if FPasses[J].Visibility <> wppvPublic then
      raise EWfcPipelineModel.CreateFmt('connectivity %d owner must be public', [I]);
    if Length(FVocabularies[J]) = 0 then
      raise EWfcPipelineModel.CreateFmt(
        'connectivity %d owner has no statically known public vocabulary', [I]);
    for K := 0 to I - 1 do
      if (AConnectivities[K].PassIndex = J) and
          (AConnectivities[K].LabelText = AConnectivities[I].LabelText) then
        raise EWfcPipelineModel.CreateFmt(
          'connectivity %d duplicates the pass/label key of connectivity %d', [I, K]);
    LPreviousTokenIndex := -1;
    for K := 0 to Length(AConnectivities[I].Values) - 1 do
    begin
      LProviderTokenIndex := TokenIndex(FVocabularies[J], AConnectivities[I].Values[K].Value);
      if LProviderTokenIndex < 0 then
        raise EWfcPipelineModel.CreateFmt(
          'connectivity %d profile %d is outside the public vocabulary', [I, K]);
      if LProviderTokenIndex <= LPreviousTokenIndex then
        raise EWfcPipelineModel.CreateFmt(
          'connectivity %d profiles must use strict public-vocabulary order', [I]);
      LPreviousTokenIndex := LProviderTokenIndex;
    end;
    FConnectivities[I] := CloneConnectivity(AConnectivities[I]);
  end;

  FSignature := CalculateSignature(Self);
end;

function TWfcPipelineModel.GetResourceCount: Integer;
begin
  Result := Length(FResources);
end;

function TWfcPipelineModel.GetPassCount: Integer;
begin
  Result := Length(FPasses);
end;

function TWfcPipelineModel.GetDependencyCount: Integer;
begin
  Result := Length(FDependencies);
end;

function TWfcPipelineModel.GetBridgeCount: Integer;
begin
  Result := Length(FBridges);
end;

function TWfcPipelineModel.GetRequirementCount: Integer;
begin
  Result := Length(FRequirements);
end;

function TWfcPipelineModel.GetValueQuotaCount: Integer;
begin
  Result := Length(FValueQuotas);
end;

function TWfcPipelineModel.GetValueQuotaVersion: Integer;
begin
  if ValueQuotaCount = 0 then Result := 0
  else Result := WFC_PIPELINE_VALUE_QUOTA_VERSION;
end;

function TWfcPipelineModel.GetConnectivityCount: Integer;
begin
  Result := Length(FConnectivities);
end;

function TWfcPipelineModel.GetConnectivityVersion: Integer;
begin
  if ConnectivityCount = 0 then Result := 0
  else Result := WFC_PIPELINE_CONNECTIVITY_VERSION;
end;

procedure TWfcPipelineModel.ValidateResourceIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= ResourceCount) then
    raise ERangeError.CreateFmt('pipeline resource index out of bounds [%d]',
      [AIndex]);
end;

procedure TWfcPipelineModel.ValidatePassIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= PassCount) then
    raise ERangeError.CreateFmt('pipeline pass index out of bounds [%d]',
      [AIndex]);
end;

procedure TWfcPipelineModel.ValidateDependencyIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= DependencyCount) then
    raise ERangeError.CreateFmt(
      'pipeline dependency index out of bounds [%d]', [AIndex]);
end;

procedure TWfcPipelineModel.ValidateBridgeIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= BridgeCount) then
    raise ERangeError.CreateFmt('pipeline bridge index out of bounds [%d]',
      [AIndex]);
end;

procedure TWfcPipelineModel.ValidateRequirementIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= RequirementCount) then
    raise ERangeError.CreateFmt(
      'pipeline requirement index out of bounds [%d]', [AIndex]);
end;

procedure TWfcPipelineModel.ValidateValueQuotaIndex(const AIndex: Integer);
begin
  if not ((AIndex >= 0) and (AIndex < ValueQuotaCount)) then
    raise ERangeError.Create('pipeline value-quota index out of bounds');
  {$IFDEF PAS2JS}
  if AIndex <> Trunc(AIndex) then
    raise ERangeError.Create('pipeline value-quota index must be an exact integer');
  {$ENDIF}
end;

procedure TWfcPipelineModel.ValidateConnectivityIndex(const AIndex: Integer);
begin
  if not ((AIndex >= 0) and (AIndex < ConnectivityCount)) then
    raise ERangeError.Create('pipeline connectivity index out of bounds');
  {$IFDEF PAS2JS}
  if AIndex <> Trunc(AIndex) then
    raise ERangeError.Create('pipeline connectivity index must be an exact integer');
  {$ENDIF}
end;

function TWfcPipelineModel.CopyMetadata: TWfcPipelineMetadata;
begin
  Result := FMetadata;
end;

function TWfcPipelineModel.CopyVersions: TWfcPipelineVersions;
begin
  Result := FVersions;
end;

function TWfcPipelineModel.ResourceAt(
  const AIndex: Integer): TWfcPipelineResource;
begin
  ValidateResourceIndex(AIndex);
  Result := FResources[AIndex];
end;

function TWfcPipelineModel.PassAt(
  const AIndex: Integer): TWfcPipelinePass;
begin
  ValidatePassIndex(AIndex);
  Result := FPasses[AIndex];
end;

function TWfcPipelineModel.DependencyAt(
  const AIndex: Integer): TWfcPipelineDependency;
begin
  ValidateDependencyIndex(AIndex);
  Result := FDependencies[AIndex];
end;

function TWfcPipelineModel.BridgeAt(
  const AIndex: Integer): TWfcPipelineBridge;
begin
  ValidateBridgeIndex(AIndex);
  Result := FBridges[AIndex];
end;

function TWfcPipelineModel.RequirementAt(
  const AIndex: Integer): TWfcPipelineRequirement;
begin
  ValidateRequirementIndex(AIndex);
  Result := CloneRequirement(FRequirements[AIndex]);
end;

function TWfcPipelineModel.ValueQuotaAt(const AIndex: Integer): TWfcPipelineValueQuota;
begin
  ValidateValueQuotaIndex(AIndex);
  Result := CloneValueQuota(FValueQuotas[AIndex]);
end;

function TWfcPipelineModel.ConnectivityAt(const AIndex: Integer): TWfcPipelineConnectivity;
begin
  ValidateConnectivityIndex(AIndex);
  Result := CloneConnectivity(FConnectivities[AIndex]);
end;

function TWfcPipelineModel.CopyResources: TWfcPipelineResources;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, ResourceCount);
  for I := 0 to ResourceCount - 1 do
    Result[I] := FResources[I];
end;

function TWfcPipelineModel.CopyPasses: TWfcPipelinePasses;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, PassCount);
  for I := 0 to PassCount - 1 do
    Result[I] := FPasses[I];
end;

function TWfcPipelineModel.CopyDependencies: TWfcPipelineDependencies;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, DependencyCount);
  for I := 0 to DependencyCount - 1 do
    Result[I] := FDependencies[I];
end;

function TWfcPipelineModel.CopyBridges: TWfcPipelineBridges;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, BridgeCount);
  for I := 0 to BridgeCount - 1 do
    Result[I] := FBridges[I];
end;

function TWfcPipelineModel.CopyRequirements: TWfcPipelineRequirements;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, RequirementCount);
  for I := 0 to RequirementCount - 1 do
    Result[I] := CloneRequirement(FRequirements[I]);
end;

function TWfcPipelineModel.CopyValueQuotas: TWfcPipelineValueQuotas;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, ValueQuotaCount);
  for I := 0 to ValueQuotaCount - 1 do
    Result[I] := CloneValueQuota(FValueQuotas[I]);
end;

function TWfcPipelineModel.CopyConnectivities: TWfcPipelineConnectivities;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, ConnectivityCount);
  for I := 0 to ConnectivityCount - 1 do
    Result[I] := CloneConnectivity(FConnectivities[I]);
end;

function TWfcPipelineModel.CopyPublicVocabulary(
  const APassIndex: Integer): TWfcModelTokens;
begin
  ValidatePassIndex(APassIndex);
  if FPasses[APassIndex].Visibility <> wppvPublic then
    raise EWfcPipelineModel.CreateFmt(
      'pass %d has no public vocabulary', [APassIndex]);
  Result := CloneTokens(FVocabularies[APassIndex]);
end;

function TWfcPipelineModel.FindResource(
  const AId: TWfcModelToken): Integer;
var
  I: Integer;
begin
  for I := 0 to ResourceCount - 1 do
    if FResources[I].Id = AId then
      Exit(I);
  Result := -1;
end;

function TWfcPipelineModel.FindPass(
  const ALabelName: TWfcModelToken): Integer;
var
  I: Integer;
begin
  for I := 0 to PassCount - 1 do
    if FPasses[I].LabelName = ALabelName then
      Exit(I);
  Result := -1;
end;

function TWfcPipelineModel.BorrowModelResource(
  const AIndex: Integer): TWfcModel;
begin
  ValidateResourceIndex(AIndex);
  if FResources[AIndex].Kind <> wprkModel then
    raise EWfcPipelineModel.CreateFmt(
      'resource %d is not a model resource', [AIndex]);
  Result := FModelResources[AIndex];
end;

function TWfcPipelineModel.BorrowRuleResource(
  const AIndex: Integer): TWfcRuleModel;
begin
  ValidateResourceIndex(AIndex);
  if FResources[AIndex].Kind <> wprkRules then
    raise EWfcPipelineModel.CreateFmt(
      'resource %d is not a rules resource', [AIndex]);
  Result := FRuleResources[AIndex];
end;

function TWfcPipelineModel.BorrowPattern2DResource(
  const AIndex: Integer): TWfcOverlappingModel2D;
begin
  ValidateResourceIndex(AIndex);
  if FResources[AIndex].Kind <> wprkPattern2D then
    raise EWfcPipelineModel.CreateFmt(
      'resource %d is not a pattern2d resource', [AIndex]);
  Result := FPatternResources[AIndex];
end;

function TWfcPipelineModel.BorrowSequenceResource(
  const AIndex: Integer): TWfcSequenceModel;
begin
  ValidateResourceIndex(AIndex);
  if FResources[AIndex].Kind <> wprkSequence then
    raise EWfcPipelineModel.CreateFmt(
      'resource %d is not a sequence resource', [AIndex]);
  Result := FSequenceResources[AIndex];
end;

function TWfcPipelineModel.BorrowPattern3DResource(
  const AIndex: Integer): TWfcOverlappingModel3D;
begin
  RequireQuotaInteger(AIndex, 'pattern3d resource index');
  ValidateResourceIndex(AIndex);
  if FResources[AIndex].Kind <> wprkPattern3D then
    raise EWfcPipelineModel.CreateFmt('resource %d is not a pattern3d resource', [AIndex]);
  Result := FPattern3DResources[AIndex];
end;

end.
