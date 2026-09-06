{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Immutable fragment assembly, no graph or execution state. }
unit wfc_pipeline_compose;
{$mode delphi}{$H+}
interface

uses SysUtils, wfc_model, wfc_pipeline_model;

const WFC_PIPELINE_COMPOSE_VERSION = 1;

type
  TWfcPipelineFragmentInput = record
    FragmentId: TWfcModelToken;
    RecipeText: String;
    ResourceIds, PassLabels: TWfcModelTokens;
  end;
  TWfcPipelineFragmentInputs = array of TWfcPipelineFragmentInput;
  TWfcPipelineComposeSection = (wpcosResource, wpcosPass, wpcosDependency,
    wpcosBridge, wpcosRequirement, wpcosValueQuota, wpcosConnectivity);
  EWfcPipelineCompose = class(Exception);
  TWfcPipelineComposeCounts = array[TWfcPipelineComposeSection] of Integer;

  TWfcPipelineComposition = class
  strict private
    FRecipe: TWfcPipelineModel;
    FRecipeText: String;
    FFragments: TWfcPipelineFragmentInputs;
    FBases, FCounts: array of TWfcPipelineComposeCounts;
    FOriginalPassLabels, FOriginalResourceIds: array of TWfcModelTokens;
    function GetFragmentCount: Integer;
    function FindFragment(const AFragmentId: TWfcModelToken): Integer;
  public
    constructor Create(const AMetadata: TWfcPipelineMetadata;
      const AFragments: TWfcPipelineFragmentInputs);
    destructor Destroy; override;
    function FragmentAt(const AIndex: Integer): TWfcPipelineFragmentInput;
    function MapIndex(const AFragmentId: TWfcModelToken;
      const ASection: TWfcPipelineComposeSection;
      const ALocalIndex: Integer): Integer;
    function ResolvePass(const AFragmentId, AOriginalLabel: TWfcModelToken): Integer;
    function ResolveResource(const AFragmentId, AOriginalId: TWfcModelToken): Integer;
    function BorrowRecipe: TWfcPipelineModel;
    function RecipeText: String;
    property FragmentCount: Integer read GetFragmentCount;
  end;

implementation

uses wfc, wfc_pipeline_text, wfc_pipeline_layout, wfc_text_codec;

const
  SECTION_LIMITS: TWfcPipelineComposeCounts = (
    WFC_PIPELINE_MAX_RESOURCE_COUNT, WFC_PIPELINE_MAX_PASS_COUNT,
    WFC_PIPELINE_MAX_DEPENDENCY_COUNT, WFC_PIPELINE_MAX_BRIDGE_COUNT,
    WFC_PIPELINE_MAX_REQUIREMENT_COUNT, WFC_PIPELINE_MAX_VALUE_QUOTA_COUNT,
    WFC_PIPELINE_MAX_CONNECTIVITY_COUNT);
  SECTION_NAMES: array[TWfcPipelineComposeSection] of String = (
    'resource', 'pass', 'dependency', 'bridge', 'requirement', 'value quota', 'connectivity');

type
  TModels = array of TWfcPipelineModel;
  TBudget = record
    Encoded, Payload, Relations, Terms, Allowed, QuotaTokens,
    ConnectivityValues, ConnectivityPositions: Integer;
  end;

procedure ComposeError(const Detail: String);
begin raise EWfcPipelineCompose.Create('pipeline composition: '+Detail); end;

procedure RequireInteger(const Value, Minimum, Maximum: Integer; const Name: String);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid=typeof Value==='number' && Number.isFinite(Value) && Number.isInteger(Value); end;
  if not Valid then ComposeError(Name+' must be an exact finite Integer');
  {$ENDIF}
  if (Value<Minimum) or (Value>Maximum) then ComposeError(Name+' is out of bounds');
end;

procedure Add(var Total: Integer; const Count, Limit: Integer; const Name: String);
begin
  if (Count<0) or (Count>Limit) or (Total>Limit-Count) then
    ComposeError('aggregate '+Name+' exceeds the version-1 limit');
  Inc(Total,Count);
end;

function BoundedLength(const Value: SizeInt; const Maximum: Integer;
  const Name: String): Integer;
begin
  { Check native SizeInt before narrowing to the portable Integer capacity. }
  if (Value<0) or (Value>Maximum) then ComposeError(Name+' exceeds the version-1 limit');
  Result:=Integer(Value);
end;

procedure RequireString(const Value: String; const Name: String);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid=typeof Value==='string'; end;
  if not Valid then ComposeError(Name+' must be a string');
  {$ENDIF}
end;

function TokenBytes(const Value: TWfcModelToken; const Name: String;
  const AllowEmpty: Boolean): Integer;
var Encoded: String;
begin
  RequireString(String(Value),Name);
  if Length(Value)>WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH then ComposeError(Name+' raw token is too long');
  if (not AllowEmpty) and (Length(Value)=0) then ComposeError(Name+' cannot be empty');
  if (Length(Value) <> 0) and not WfcModelTokenIsValid(Value) then ComposeError(Name+' contains invalid Unicode');
  Encoded:=WfcTextEncodeToken(Value,'WFC pipeline composition');
  if Length(Encoded)>WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH then ComposeError(Name+' encoded token is too long');
  Result:=Length(Encoded);
end;

procedure AddToken(var Total: Integer; const Value: TWfcModelToken;
  const Name: String; const AllowEmpty: Boolean=False);
begin Add(Total,TokenBytes(Value,Name,AllowEmpty),WFC_PIPELINE_MAX_TOTAL_ENCODED_TOKEN_LENGTH,'encoded tokens'); end;

procedure RequireInputShape(const Metadata: TWfcPipelineMetadata;
  const Fragments: TWfcPipelineFragmentInputs);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    function data(o, names) {
      if (o===null || typeof o!=='object' || Array.isArray(o)) return false;
      for (const key of names) {
        let p=o,d;
        while (p!==null) { d=Object.getOwnPropertyDescriptor(p,key); if(d)break; p=Object.getPrototypeOf(p); }
        if (!d || !Object.prototype.hasOwnProperty.call(d,'value')) return false;
      }
      return true;
    }
    function names(a,max,allowNil) {
      if (a===null && allowNil) return true;
      if (!Array.isArray(a) || a.length>max) return false;
      for(let i=0;i<a.length;i++) {
        const d=Object.getOwnPropertyDescriptor(a,String(i));
        if(!d || !Object.prototype.hasOwnProperty.call(d,'value') || typeof d.value!=='string')return false;
      }
      return true;
    }
    Valid=data(Metadata,['Name','LicenseIdentifier','SourceDescription','SourceFingerprint']);
    if(Valid) for(const key of ['Name','LicenseIdentifier','SourceDescription','SourceFingerprint'])
      if(typeof Metadata[key]!=='string')Valid=false;
    Valid=Valid && Array.isArray(Fragments) && Fragments.length>0 && Fragments.length<=256;
    if(Valid) for(let i=0;i<Fragments.length;i++) {
      const d=Object.getOwnPropertyDescriptor(Fragments,String(i));
      if(!d || !Object.prototype.hasOwnProperty.call(d,'value')) {Valid=false;break;}
      const f=d.value;
      if(!data(f,['FragmentId','RecipeText','ResourceIds','PassLabels'])) {Valid=false;break;}
      if(typeof f.FragmentId!=='string' || typeof f.RecipeText!=='string' ||
        !names(f.ResourceIds,64,true) || !names(f.PassLabels,256,false)) {Valid=false;break;}
    }
  end;
  if not Valid then ComposeError('fragment/metadata containers require bounded passive data and own string-array slots');
  {$ENDIF}
end;

function CopyTokens(const Values: TWfcModelTokens): TWfcModelTokens;
var I: Integer;
begin
  Result:=nil; SetLength(Result,Length(Values));
  for I:=0 to High(Values) do Result[I]:=Values[I];
end;

function CopyFragment(const Value: TWfcPipelineFragmentInput): TWfcPipelineFragmentInput;
begin
  Result.FragmentId:=Value.FragmentId; Result.RecipeText:=Value.RecipeText;
  Result.ResourceIds:=CopyTokens(Value.ResourceIds); Result.PassLabels:=CopyTokens(Value.PassLabels);
end;

function Counts(const Model: TWfcPipelineModel): TWfcPipelineComposeCounts;
begin
  Result[wpcosResource]:=Model.ResourceCount; Result[wpcosPass]:=Model.PassCount;
  Result[wpcosDependency]:=Model.DependencyCount; Result[wpcosBridge]:=Model.BridgeCount;
  Result[wpcosRequirement]:=Model.RequirementCount; Result[wpcosValueQuota]:=Model.ValueQuotaCount;
  Result[wpcosConnectivity]:=Model.ConnectivityCount;
end;

procedure RequireSameVersions(const Left, Right: TWfcPipelineVersions);
  procedure Equal(const A,B: Integer; const Name: String);
  begin if A<>B then ComposeError('version mismatch: '+Name); end;
begin
  Equal(Left.GraphModelVersion,Right.GraphModelVersion,'GraphModelVersion');
  Equal(Left.RandomAlgorithmVersion,Right.RandomAlgorithmVersion,'RandomAlgorithmVersion');
  Equal(Left.SolverAlgorithmVersion,Right.SolverAlgorithmVersion,'SolverAlgorithmVersion');
  Equal(Left.PipelineAlgorithmVersion,Right.PipelineAlgorithmVersion,'PipelineAlgorithmVersion');
  Equal(Left.BundleGraphAdapterVersion,Right.BundleGraphAdapterVersion,'BundleGraphAdapterVersion');
  Equal(Left.ModelGraphAdapterVersion,Right.ModelGraphAdapterVersion,'ModelGraphAdapterVersion');
  Equal(Left.RulesGraphAdapterVersion,Right.RulesGraphAdapterVersion,'RulesGraphAdapterVersion');
  Equal(Left.Pattern2DGraphAdapterVersion,Right.Pattern2DGraphAdapterVersion,'Pattern2DGraphAdapterVersion');
  Equal(Left.SequenceGraphAdapterVersion,Right.SequenceGraphAdapterVersion,'SequenceGraphAdapterVersion');
  Equal(Left.Pattern2DBridgeVersion,Right.Pattern2DBridgeVersion,'Pattern2DBridgeVersion');
  Equal(Left.SequenceBridgeVersion,Right.SequenceBridgeVersion,'SequenceBridgeVersion');
end;

procedure ScanBudget(const Model: TWfcPipelineModel; var B: TBudget);
var I,J,K,States,Directions,Slots: Integer; Resource: TWfcPipelineResource;
  Requirement: TWfcPipelineRequirement; Quota: TWfcPipelineValueQuota;
  Connectivity: TWfcPipelineConnectivity;
begin
  { Source owners are already strictly decoded. Public accessors detach at most
    one bounded descriptor here; all aggregate checks precede concatenated rows
    and the final model's owned clones/nested resource decoding. }
  for I:=0 to Model.ResourceCount-1 do
  begin
    Resource:=Model.ResourceAt(I);
    Add(B.Payload,Length(Resource.Document),WFC_PIPELINE_MAX_TOTAL_RESOURCE_PAYLOAD_LENGTH,'resource payload');
    AddToken(B.Encoded,Resource.SourceDescription,'resource source description');
    AddToken(B.Encoded,Resource.SourceLicenseIdentifier,'resource license');
    AddToken(B.Encoded,Resource.SourceFingerprint,'resource fingerprint',True);
    States:=0; Directions:=0;
    case Resource.Kind of
      wprkModel: begin States:=Model.BorrowModelResource(I).ValueCount;
        Directions:=WfcModelStoredDirectionCount(Model.BorrowModelResource(I).Rank); end;
      wprkPattern2D: begin States:=Model.BorrowPattern2DResource(I).PatternCount; Directions:=4; end;
      wprkPattern3D: begin States:=Model.BorrowPattern3DResource(I).PatternCount; Directions:=6; end;
      wprkSequence: begin States:=Model.BorrowSequenceResource(I).StateCount; Directions:=4; end;
    end;
    if States>0 then
    begin
      if States>WFC_PIPELINE_MAX_TOTAL_RESOURCE_RELATION_SLOT_COUNT div States then ComposeError('resource relation slots exceed the limit');
      Slots:=States*States;
      if Directions>WFC_PIPELINE_MAX_TOTAL_RESOURCE_RELATION_SLOT_COUNT div Slots then ComposeError('resource relation slots exceed the limit');
      Add(B.Relations,Slots*Directions,WFC_PIPELINE_MAX_TOTAL_RESOURCE_RELATION_SLOT_COUNT,'resource relation slots');
    end;
  end;
  for I:=0 to Model.RequirementCount-1 do
  begin
    Requirement:=Model.RequirementAt(I);
    AddToken(B.Encoded,Requirement.ConsumerToken,'requirement consumer token');
    if Requirement.Kind=wprqMapped then
    begin
      Add(B.Allowed,Length(Requirement.MappedQuery.AllowedProviderTokens),WFC_PIPELINE_MAX_TOTAL_ALLOWED_TOKEN_COUNT,'allowed tokens');
      for J:=0 to High(Requirement.MappedQuery.AllowedProviderTokens) do
        AddToken(B.Encoded,Requirement.MappedQuery.AllowedProviderTokens[J],'mapped provider token');
    end
    else
    begin
      Add(B.Terms,Length(Requirement.Terms),WFC_PIPELINE_MAX_TOTAL_REQUIREMENT_TERM_COUNT,'requirement terms');
      for J:=0 to High(Requirement.Terms) do
      begin
        Add(B.Allowed,Length(Requirement.Terms[J].AllowedProviderTokens),WFC_PIPELINE_MAX_TOTAL_ALLOWED_TOKEN_COUNT,'allowed tokens');
        for K:=0 to High(Requirement.Terms[J].AllowedProviderTokens) do
          AddToken(B.Encoded,Requirement.Terms[J].AllowedProviderTokens[K],'provider token');
      end;
    end;
  end;
  for I:=0 to Model.ValueQuotaCount-1 do
  begin
    Quota:=Model.ValueQuotaAt(I); AddToken(B.Encoded,Quota.LabelText,'quota label');
    Add(B.QuotaTokens,Length(Quota.Values),WFC_PIPELINE_MAX_TOTAL_VALUE_QUOTA_TOKEN_COUNT,'quota tokens');
    for J:=0 to High(Quota.Values) do AddToken(B.Encoded,Quota.Values[J],'quota value');
  end;
  for I:=0 to Model.ConnectivityCount-1 do
  begin
    Connectivity:=Model.ConnectivityAt(I); AddToken(B.Encoded,Connectivity.LabelText,'connectivity label');
    Add(B.ConnectivityValues,Length(Connectivity.Values),WFC_PIPELINE_MAX_TOTAL_CONNECTIVITY_VALUE_COUNT,'connectivity profiles');
    Add(B.ConnectivityPositions,Length(Connectivity.RequiredPositions),WFC_PIPELINE_MAX_TOTAL_CONNECTIVITY_REQUIRED_POSITION_COUNT,'connectivity positions');
    for J:=0 to High(Connectivity.Values) do AddToken(B.Encoded,Connectivity.Values[J].Value,'connectivity value');
  end;
end;

constructor TWfcPipelineComposition.Create(const AMetadata: TWfcPipelineMetadata;
  const AFragments: TWfcPipelineFragmentInputs);
var Models: TModels; Total,Local: TWfcPipelineComposeCounts;
  I,J,K,N,ResourceBase,PassBase,InputBytes,InputTokens,NamesEncoded,ResourceNames,PassNames: Integer;
  Section: TWfcPipelineComposeSection; Budget: TBudget;
  Versions,OtherVersions: TWfcPipelineVersions; Has3D: Boolean; Context: String;
  Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies; Bridges: TWfcPipelineBridges;
  Requirements: TWfcPipelineRequirements; Quotas: TWfcPipelineValueQuotas;
  Connectivities: TWfcPipelineConnectivities; Topologies: TWfcPipelinePassTopologies;
  Resource: TWfcPipelineResource; Pass: TWfcPipelinePass;
  Dependency: TWfcPipelineDependency; Bridge: TWfcPipelineBridge;
  Requirement: TWfcPipelineRequirement; Quota: TWfcPipelineValueQuota;
  Connectivity: TWfcPipelineConnectivity;
begin
  inherited Create;
  Models:=nil; Context:='composition-wide inputs';
  try
    try
      RequireInputShape(AMetadata,AFragments);
      RequireInteger(BoundedLength(Length(AFragments),WFC_PIPELINE_MAX_PASS_COUNT,'fragment count'),1,WFC_PIPELINE_MAX_PASS_COUNT,'fragment count');
      InputBytes:=0; InputTokens:=0; NamesEncoded:=0; ResourceNames:=0; PassNames:=0;
      AddToken(NamesEncoded,AMetadata.Name,'metadata name');
      AddToken(NamesEncoded,AMetadata.LicenseIdentifier,'metadata license');
      AddToken(NamesEncoded,AMetadata.SourceDescription,'metadata source',True);
      AddToken(NamesEncoded,AMetadata.SourceFingerprint,'metadata fingerprint',True);
      InputTokens:=NamesEncoded;
      { Inspect every raw fragment before any decoder or owner-array clone. }
      for I:=0 to High(AFragments) do
      begin
        Context:='fragment '+IntToStr(I)+' input';
        AddToken(InputTokens,AFragments[I].FragmentId,'fragment ID');
        for J:=0 to I-1 do if AFragments[J].FragmentId=AFragments[I].FragmentId then ComposeError('duplicate fragment ID');
        RequireString(AFragments[I].RecipeText,'recipe text');
        if Length(AFragments[I].RecipeText)=0 then ComposeError('recipe text cannot be empty');
        Add(InputBytes,BoundedLength(Length(AFragments[I].RecipeText),WFC_PIPELINE_MAX_ENCODED_TEXT_LENGTH,'source text'),WFC_PIPELINE_MAX_ENCODED_TEXT_LENGTH,'source text');
        RequireInteger(BoundedLength(Length(AFragments[I].ResourceIds),WFC_PIPELINE_MAX_RESOURCE_COUNT,'resource names'),0,WFC_PIPELINE_MAX_RESOURCE_COUNT,'resource names');
        RequireInteger(BoundedLength(Length(AFragments[I].PassLabels),WFC_PIPELINE_MAX_PASS_COUNT,'pass names'),1,WFC_PIPELINE_MAX_PASS_COUNT,'pass names');
        Add(ResourceNames,Length(AFragments[I].ResourceIds),WFC_PIPELINE_MAX_RESOURCE_COUNT,'resource name count');
        Add(PassNames,Length(AFragments[I].PassLabels),WFC_PIPELINE_MAX_PASS_COUNT,'pass name count');
        for J:=0 to High(AFragments[I].ResourceIds) do
        begin
          N:=TokenBytes(AFragments[I].ResourceIds[J],'resource ID',False);
          Add(InputTokens,N,WFC_PIPELINE_MAX_TOTAL_ENCODED_TOKEN_LENGTH,'input tokens');
          Add(NamesEncoded,N,WFC_PIPELINE_MAX_TOTAL_ENCODED_TOKEN_LENGTH,'output tokens');
          for K:=0 to J-1 do if AFragments[I].ResourceIds[J]=AFragments[I].ResourceIds[K] then ComposeError('duplicate final resource ID');
          for K:=0 to I-1 do for N:=0 to High(AFragments[K].ResourceIds) do
            if AFragments[I].ResourceIds[J]=AFragments[K].ResourceIds[N] then ComposeError('duplicate final resource ID');
        end;
        for J:=0 to High(AFragments[I].PassLabels) do
        begin
          N:=TokenBytes(AFragments[I].PassLabels[J],'pass label',False);
          Add(InputTokens,N,WFC_PIPELINE_MAX_TOTAL_ENCODED_TOKEN_LENGTH,'input tokens');
          Add(NamesEncoded,N,WFC_PIPELINE_MAX_TOTAL_ENCODED_TOKEN_LENGTH,'output tokens');
          for K:=0 to J-1 do if AFragments[I].PassLabels[J]=AFragments[I].PassLabels[K] then ComposeError('duplicate final pass label');
          for K:=0 to I-1 do for N:=0 to High(AFragments[K].PassLabels) do
            if AFragments[I].PassLabels[J]=AFragments[K].PassLabels[N] then ComposeError('duplicate final pass label');
        end;
      end;
      SetLength(Models,Length(AFragments)); SetLength(FBases,Length(AFragments));
      SetLength(FCounts,Length(AFragments)); Total:=Default(TWfcPipelineComposeCounts);
      Budget:=Default(TBudget); Budget.Encoded:=NamesEncoded; Has3D:=False;
      for I:=0 to High(AFragments) do
      begin
        Context:='fragment '+IntToStr(I)+' ['+String(AFragments[I].FragmentId)+']';
        Models[I]:=DecodeWfcPipelineModelText(AFragments[I].RecipeText);
        if EncodeWfcPipelineModelText(Models[I])<>AFragments[I].RecipeText then ComposeError('source document is not canonical');
        Local:=Counts(Models[I]);
        if (Local[wpcosResource]<>Length(AFragments[I].ResourceIds)) or
          (Local[wpcosPass]<>Length(AFragments[I].PassLabels)) then ComposeError('complete final name-vector lengths must match the source recipe');
        FBases[I]:=Total; FCounts[I]:=Local;
        for Section:=Low(Section) to High(Section) do
          Add(Total[Section],Local[Section],SECTION_LIMITS[Section],SECTION_NAMES[Section]+' count');
        OtherVersions:=Models[I].CopyVersions;
        if I=0 then Versions:=OtherVersions else
        begin
          RequireSameVersions(Versions,OtherVersions);
          if Models[I].RunMode<>Models[0].RunMode then ComposeError('global run mode mismatch');
          if Models[I].PassAt(0).Mode=gpmLegacy then
            ComposeError('legacy root ['+String(Models[I].PassAt(0).LabelName)+'] cannot relocate to pass '+
              IntToStr(FBases[I][wpcosPass])+' without changing its predecessor semantics; no edge or mode rewrite is authorized');
        end;
        if Models[I].HasPattern3D then
        begin
          if Has3D and ((Versions.Pattern3DGraphAdapterVersion<>OtherVersions.Pattern3DGraphAdapterVersion) or
            (Versions.Pattern3DBridgeVersion<>OtherVersions.Pattern3DBridgeVersion)) then ComposeError('active Pattern3D version mismatch');
          Versions.Pattern3DGraphAdapterVersion:=OtherVersions.Pattern3DGraphAdapterVersion;
          Versions.Pattern3DBridgeVersion:=OtherVersions.Pattern3DBridgeVersion; Has3D:=True;
        end;
        ScanBudget(Models[I],Budget);
      end;
      Context:='composition-wide remapping';
      SetLength(Resources,Total[wpcosResource]); SetLength(Passes,Total[wpcosPass]);
      SetLength(Dependencies,Total[wpcosDependency]); SetLength(Bridges,Total[wpcosBridge]);
      SetLength(Requirements,Total[wpcosRequirement]); SetLength(Quotas,Total[wpcosValueQuota]);
      SetLength(Connectivities,Total[wpcosConnectivity]); SetLength(Topologies,Total[wpcosPass]);
      SetLength(FOriginalPassLabels,Length(Models)); SetLength(FOriginalResourceIds,Length(Models));
      for I:=0 to High(Models) do
      begin
        Context:='fragment '+IntToStr(I)+' ['+String(AFragments[I].FragmentId)+'] remapping';
        ResourceBase:=FBases[I][wpcosResource]; PassBase:=FBases[I][wpcosPass];
        SetLength(FOriginalResourceIds[I],Models[I].ResourceCount); SetLength(FOriginalPassLabels[I],Models[I].PassCount);
        for J:=0 to Models[I].ResourceCount-1 do
        begin
          Resource:=Models[I].ResourceAt(J); FOriginalResourceIds[I][J]:=Resource.Id;
          Resource.Id:=AFragments[I].ResourceIds[J]; Resources[ResourceBase+J]:=Resource;
        end;
        for J:=0 to Models[I].PassCount-1 do
        begin
          Pass:=Models[I].PassAt(J); FOriginalPassLabels[I][J]:=Pass.LabelName;
          Pass.LabelName:=AFragments[I].PassLabels[J];
          if Pass.ResourceIndex<>WFC_PIPELINE_NO_INDEX then Inc(Pass.ResourceIndex,ResourceBase);
          if Pass.Mode=gpmTransform then Inc(Pass.TransformSourceIndex,PassBase);
          Passes[PassBase+J]:=Pass; Topologies[PassBase+J]:=Models[I].PassTopologyAt(J);
        end;
        for J:=0 to Models[I].DependencyCount-1 do
        begin
          Dependency:=Models[I].DependencyAt(J); Inc(Dependency.ConsumerPassIndex,PassBase); Inc(Dependency.ProviderPassIndex,PassBase);
          Dependencies[FBases[I][wpcosDependency]+J]:=Dependency;
        end;
        for J:=0 to Models[I].BridgeCount-1 do
        begin
          Bridge:=Models[I].BridgeAt(J); Inc(Bridge.SourcePassIndex,PassBase); Inc(Bridge.TargetPassIndex,PassBase);
          Bridges[FBases[I][wpcosBridge]+J]:=Bridge;
        end;
        for J:=0 to Models[I].RequirementCount-1 do
        begin
          { RequirementAt already returns an active-tag-only detached record. }
          Requirement:=Models[I].RequirementAt(J); Inc(Requirement.ConsumerPassIndex,PassBase); Inc(Requirement.ProviderPassIndex,PassBase);
          Requirements[FBases[I][wpcosRequirement]+J]:=Requirement;
        end;
        for J:=0 to Models[I].ValueQuotaCount-1 do
        begin Quota:=Models[I].ValueQuotaAt(J); Inc(Quota.PassIndex,PassBase); Quotas[FBases[I][wpcosValueQuota]+J]:=Quota; end;
        for J:=0 to Models[I].ConnectivityCount-1 do
        begin Connectivity:=Models[I].ConnectivityAt(J); Inc(Connectivity.PassIndex,PassBase); Connectivities[FBases[I][wpcosConnectivity]+J]:=Connectivity; end;
      end;
      Context:='composition-wide final recipe validation';
      FRecipe:=TWfcPipelineModel.Create(AMetadata,Versions,Topologies[0].Rank,Topologies[0].Wrap,
        Models[0].RunMode,Resources,Passes,Dependencies,Bridges,Requirements,Quotas,Connectivities,
        WFC_PIPELINE_PASS_MAPPING_VERSION,Topologies);
      FRecipeText:=EncodeWfcPipelineModelText(FRecipe);
      SetLength(FFragments,Length(AFragments));
      for I:=0 to High(AFragments) do FFragments[I]:=CopyFragment(AFragments[I]);
    except
      on E: EOutOfMemory do raise;
      on E: Exception do ComposeError(Context+': '+E.Message);
    end;
  finally
    for I:=0 to High(Models) do Models[I].Free;
  end;
end;

destructor TWfcPipelineComposition.Destroy;
begin FRecipe.Free; inherited Destroy; end;

function TWfcPipelineComposition.GetFragmentCount: Integer;
begin Result:=Length(FFragments); end;

function TWfcPipelineComposition.FindFragment(const AFragmentId: TWfcModelToken): Integer;
var I: Integer;
begin
  RequireString(String(AFragmentId),'fragment ID');
  for I:=0 to High(FFragments) do if FFragments[I].FragmentId=AFragmentId then Exit(I);
  ComposeError('unknown fragment ID'); Result:=-1;
end;

function TWfcPipelineComposition.FragmentAt(const AIndex: Integer): TWfcPipelineFragmentInput;
begin RequireInteger(AIndex,0,FragmentCount-1,'fragment index'); Result:=CopyFragment(FFragments[AIndex]); end;

function TWfcPipelineComposition.MapIndex(const AFragmentId: TWfcModelToken;
  const ASection: TWfcPipelineComposeSection; const ALocalIndex: Integer): Integer;
var I: Integer;
begin
  RequireInteger(Ord(ASection),Ord(Low(TWfcPipelineComposeSection)),Ord(High(TWfcPipelineComposeSection)),'section');
  I:=FindFragment(AFragmentId); RequireInteger(ALocalIndex,0,FCounts[I][ASection]-1,'local '+SECTION_NAMES[ASection]+' index');
  Result:=FBases[I][ASection]+ALocalIndex;
end;

function TWfcPipelineComposition.ResolvePass(const AFragmentId,AOriginalLabel: TWfcModelToken): Integer;
var I,J: Integer;
begin
  RequireString(String(AOriginalLabel),'original pass label'); I:=FindFragment(AFragmentId);
  for J:=0 to High(FOriginalPassLabels[I]) do if FOriginalPassLabels[I][J]=AOriginalLabel then Exit(FBases[I][wpcosPass]+J);
  ComposeError('unknown original pass label in fragment '+String(AFragmentId)); Result:=-1;
end;

function TWfcPipelineComposition.ResolveResource(const AFragmentId,AOriginalId: TWfcModelToken): Integer;
var I,J: Integer;
begin
  RequireString(String(AOriginalId),'original resource ID'); I:=FindFragment(AFragmentId);
  for J:=0 to High(FOriginalResourceIds[I]) do if FOriginalResourceIds[I][J]=AOriginalId then Exit(FBases[I][wpcosResource]+J);
  ComposeError('unknown original resource ID in fragment '+String(AFragmentId)); Result:=-1;
end;

function TWfcPipelineComposition.BorrowRecipe: TWfcPipelineModel;
begin Result:=FRecipe; end;

function TWfcPipelineComposition.RecipeText: String;
begin Result:=FRecipeText; end;

end.
