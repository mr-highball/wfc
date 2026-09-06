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
unit wfc_pipeline_compile;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  wfc,
  wfc_pipeline_model;

const
  { Identifies this closed recipe-to-graph implementation. The compatible
    value is also carried by TWfcPipelineVersions.BundleGraphAdapterVersion. }
  WFC_PIPELINE_COMPILER_VERSION = 1;
  { Direct compiler callers do not yet have a run artifact to enforce its
    envelope, so this boundary independently prevents an otherwise valid tiny
    recipe from requesting an impractical dense graph. }
  WFC_PIPELINE_COMPILE_MAX_DIMENSION = 4194304;
  WFC_PIPELINE_COMPILE_MAX_CELL_COUNT = 4194304;
  { Aggregate latent-state visits for recipe quota lowering, checked before
    allocating a graph. This bounds compilation, not the solver's search. }
  WFC_PIPELINE_COMPILE_MAX_QUOTA_CANDIDATE_VISITS = 16777216;

type
  TWfcPipelineCompileStage = (
    wpcsPreflight,
    wpcsPasses,
    wpcsDependencies,
    wpcsShape,
    wpcsAdapters,
    wpcsBridges,
    wpcsRequirements,
    wpcsVerification,
    wpcsValueQuotas,
    wpcsConnectivity
  );

  EWfcPipelineCompile = class(Exception)
  strict private
    FStage: TWfcPipelineCompileStage;
    FItemIndex: Integer;
  public
    constructor CreateFailure(const AStage: TWfcPipelineCompileStage;
      const AItemIndex: Integer; const ADetail: String);
    property Stage: TWfcPipelineCompileStage read FStage;
    { The pass, dependency, bridge, or requirement index for the active
      stage. It is -1 when the failure concerns the whole recipe or shape. }
    property ItemIndex: Integer read FItemIndex;
  end;

  TWfcPipelineCommitValidationKind = (
    wpcvkNone,
    wpcvkPatternPass,
    wpcvkSequencePass,
    wpcvkTransform,
    wpcvkPatternBridge,
    wpcvkSequenceBridge,
    wpcvkRequirement,
    wpcvkValueQuota,
    wpcvkConnectivity
  );

  TWfcPipelineCommitValidation = record
    Kind: TWfcPipelineCommitValidationKind;
    PassIndex: Integer;
    BridgeIndex: Integer;
    RequirementIndex: Integer;
    ValueQuotaIndex: Integer;
    ConnectivityIndex: Integer;
    EntryIndex: Integer;
  end;

  { Owns one fresh executable graph. The immutable recipe is borrowed rather
    than copied and must outlive this object. Graph is likewise borrowed by
    callers and must not be freed or retained beyond this object. }
  TWfcCompiledPipeline = class
  strict private
    FRecipe: TWfcPipelineModel;
    FGraph: TGraph;
    FLastValidation: TWfcPipelineCommitValidation;
    procedure Initialize(const ARecipe: TWfcPipelineModel;
      const AWidth, AHeight, ADepth: Integer);
    function QuotaMaterializedPass(const APassIndex: Integer): Integer;
    function ProjectionBridgeForPass(const APassIndex: Integer): Integer;
    procedure ValidateValueQuotaWork;
    procedure InstallValueQuotas;
    function ValidateValueQuotaCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean;
    procedure InstallConnectivity;
    function ValidateConnectivityCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean;
  private
    function ValidatePendingCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean;
  public
    constructor Create(const ARecipe: TWfcPipelineModel;
      const AWidth, AHeight, ADepth: Integer);
    destructor Destroy; override;

    property Recipe: TWfcPipelineModel read FRecipe;
    property Graph: TGraph read FGraph;
    property LastValidation: TWfcPipelineCommitValidation
      read FLastValidation;
  end;

function CompileWfcPipeline(const ARecipe: TWfcPipelineModel;
  const AWidth, AHeight, ADepth: Integer): TWfcCompiledPipeline;

function WfcPipelineCompileStageName(
  const AStage: TWfcPipelineCompileStage): String;

implementation

uses
  wfc_model,
  wfc_rule_model,
  wfc_pattern2d,
  wfc_pattern2d_graph,
  wfc_sequence,
  wfc_sequence_graph,
  wfc_token_lookup,
  wfc_pipeline_connectivity;

type
  TStringArray = array of String;
  TIntegerArray = array of Integer;
  TQuotaRecount = record
    Lookup: TWfcTokenLookup;
    Counts: TIntegerArray;
    InvalidEntry: Integer;
  end;

  { The root and every pass retain the same owner. The core invokes this hook
    after staged assignments have reached live entries but before it discards
    the entry and per-pass random-stream snapshots. }
  TWfcPipelineCommitGraph = class(TGraph)
  private
    FOwner: TWfcCompiledPipeline;
  strict protected
    function DoValidateCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean; override;
  public
    constructor CreatePass(const ARoot: TGraph;
      const APassIndex: Integer); override;
    property Owner: TWfcCompiledPipeline read FOwner write FOwner;
  end;

function WfcPipelineCompileStageName(
  const AStage: TWfcPipelineCompileStage): String;
begin
  case AStage of
    wpcsPreflight:
      Result := 'preflight';
    wpcsPasses:
      Result := 'passes';
    wpcsDependencies:
      Result := 'dependencies';
    wpcsShape:
      Result := 'shape';
    wpcsAdapters:
      Result := 'adapters';
    wpcsBridges:
      Result := 'bridges';
    wpcsRequirements:
      Result := 'requirements';
    wpcsVerification:
      Result := 'verification';
    wpcsValueQuotas:
      Result := 'value-quotas';
    wpcsConnectivity:
      Result := 'connectivity';
  else
    Result := 'unknown';
  end;
end;

constructor EWfcPipelineCompile.CreateFailure(
  const AStage: TWfcPipelineCompileStage; const AItemIndex: Integer;
  const ADetail: String);
var
  LPrefix: String;
begin
  FStage := AStage;
  FItemIndex := AItemIndex;
  LPrefix := 'pipeline compile ' + WfcPipelineCompileStageName(AStage);
  if AItemIndex >= 0 then
    LPrefix := LPrefix + ' ' + IntToStr(AItemIndex);
  inherited Create(LPrefix + ': ' + ADetail);
end;

function TokenToGraphValue(const AToken: TWfcModelToken;
  const ALabel: String): TGraphValue;
begin
  {$IFDEF PAS2JS}
  Result := TGraphValue(AToken);
  {$ELSE}
  Result := TGraphValue(UTF8Decode(AToken));
  {$ENDIF}
  if Result = TGraphValue.Empty then
    raise EArgumentException.Create(ALabel + ' converts to an empty graph value');
  {$IFDEF PAS2JS}
  if TWfcModelToken(Result) <> AToken then
  {$ELSE}
  if UTF8Encode(UnicodeString(Result)) <> AToken then
  {$ENDIF}
    raise EArgumentException.Create(ALabel +
      ' cannot be represented by the graph string type');
end;

function GraphValueToToken(const AValue: TGraphValue): TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(AValue);
  {$ELSE}
  Result := UTF8Encode(UnicodeString(AValue));
  {$ENDIF}
end;

function CheckedEntryCount(const AWidth, AHeight,
  ADepth: Integer): Integer;
var
  LPlane: Integer;
begin
  if (AWidth < 1) or (AHeight < 1) or (ADepth < 1) then
    raise ERangeError.CreateFmt(
      'pipeline dimensions must be positive [%d x %d x %d]',
      [AWidth, AHeight, ADepth]);
  if (AWidth > WFC_PIPELINE_COMPILE_MAX_DIMENSION) or
      (AHeight > WFC_PIPELINE_COMPILE_MAX_DIMENSION) or
      (ADepth > WFC_PIPELINE_COMPILE_MAX_DIMENSION) then
    raise ERangeError.Create('pipeline dimension exceeds the compiler limit');
  if AWidth > WFC_PIPELINE_COMPILE_MAX_CELL_COUNT div AHeight then
    raise ERangeError.Create('pipeline cell count exceeds the compiler limit');
  LPlane := AWidth * AHeight;
  if LPlane > WFC_PIPELINE_COMPILE_MAX_CELL_COUNT div ADepth then
    raise ERangeError.Create('pipeline cell count exceeds the compiler limit');
  Result := LPlane * ADepth;
end;

procedure ValidateRankShape(const ARecipe: TWfcPipelineModel;
  const AWidth, AHeight, ADepth: Integer);
begin
  CheckedEntryCount(AWidth, AHeight, ADepth);
  case ARecipe.Rank of
    1:
      if (AHeight <> 1) or (ADepth <> 1) then
        raise EArgumentException.CreateFmt(
          'rank-1 pipeline requires height and depth 1 [%d x %d x %d]',
          [AWidth, AHeight, ADepth]);
    2:
      if ADepth <> 1 then
        raise EArgumentException.CreateFmt(
          'rank-2 pipeline requires depth 1 [%d x %d x %d]',
          [AWidth, AHeight, ADepth]);
    3:
      ;
  else
    raise EArgumentException.CreateFmt(
      'pipeline rank is unsupported [%d]', [ARecipe.Rank]);
  end;
end;

function RecipeHasDependency(const ARecipe: TWfcPipelineModel;
  const AConsumer, AProvider: Integer): Boolean;
var
  I: Integer;
  LDependency: TWfcPipelineDependency;
begin
  for I := 0 to ARecipe.DependencyCount - 1 do
  begin
    LDependency := ARecipe.DependencyAt(I);
    if (LDependency.ConsumerPassIndex = AConsumer) and
        (LDependency.ProviderPassIndex = AProvider) then
      Exit(True);
  end;
  Result := False;
end;

function ExpectedDependencyCount(const ARecipe: TWfcPipelineModel;
  const AConsumer: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ARecipe.DependencyCount - 1 do
    if ARecipe.DependencyAt(I).ConsumerPassIndex = AConsumer then
      Inc(Result);
end;

procedure InitializeCommitValidation(
  out AValue: TWfcPipelineCommitValidation);
begin
  AValue := Default(TWfcPipelineCommitValidation);
  AValue.Kind := wpcvkNone;
  AValue.PassIndex := -1;
  AValue.BridgeIndex := -1;
  AValue.RequirementIndex := -1;
  AValue.ValueQuotaIndex := -1;
  AValue.ConnectivityIndex := -1;
  AValue.EntryIndex := -1;
end;

function ResolveRequirementAxis(const ACoordinate, ASize,
  ADelta: Integer; const AWrap: Boolean;
  out AResolved: Integer): Boolean;
var
  LRemainder: Integer;
begin
  AResolved := -1;
  if (ACoordinate < 0) or (ASize < 1) or
      (ACoordinate >= ASize) then
    Exit(False);
  if AWrap then
  begin
    { Remaindering avoids negating Low(Integer) and bounds the following sum
      to less than twice ASize on both native FPC and JavaScript. }
    LRemainder := ADelta mod ASize;
    AResolved := ACoordinate + LRemainder;
    if AResolved < 0 then
      Inc(AResolved, ASize)
    else if AResolved >= ASize then
      Dec(AResolved, ASize);
    Exit(True);
  end;
  if (ADelta < -ACoordinate) or
      (ADelta > (ASize - 1) - ACoordinate) then
    Exit(False);
  AResolved := ACoordinate + ADelta;
  Result := True;
end;

function ProviderEntryAllows(const AEntry: TGraphEntry;
  const AAllowed: TWfcModelTokens): Boolean;
var
  I: Integer;
  LToken: TWfcModelToken;
begin
  if AEntry.Empty then
    Exit(False);
  LToken := GraphValueToToken(AEntry.Value);
  for I := 0 to Length(AAllowed) - 1 do
    if LToken = AAllowed[I] then
      Exit(True);
  Result := False;
end;

function RequirementTermMatches(const AProvider: TGraph;
  const AX, AY, AZ: Integer;
  const ATerm: TWfcPipelineRequirementTerm;
  const AWrap: Boolean): Boolean;
var
  LX: Integer;
  LY: Integer;
  LZ: Integer;
begin
  if not ResolveRequirementAxis(AX, Integer(AProvider.Dimension.Width),
      ATerm.OffsetX, AWrap, LX) then
    Exit(False);
  if not ResolveRequirementAxis(AY, Integer(AProvider.Dimension.Height),
      ATerm.OffsetY, AWrap, LY) then
    Exit(False);
  if not ResolveRequirementAxis(AZ, Integer(AProvider.Dimension.Depth),
      ATerm.OffsetZ, AWrap, LZ) then
    Exit(False);
  Result := ProviderEntryAllows(AProvider.Entry[LX, LY, LZ],
    ATerm.AllowedProviderTokens);
end;

function RequirementCountMatches(const AProvider: TGraph;
  const AX, AY, AZ: Integer;
  const ARequirement: TWfcPipelineRequirement;
  const AWrap: Boolean;
  var AMatchedIndices: TIntegerArray): Boolean;
var
  I: Integer;
  J: Integer;
  LAlreadyMatched: Boolean;
  LCount: Integer;
  LResolvedIndex: Integer;
  LX: Integer;
  LY: Integer;
  LZ: Integer;
begin
  LCount := 0;
  for I := 0 to Length(ARequirement.Terms) - 1 do
  begin
    if not ResolveRequirementAxis(AX, Integer(AProvider.Dimension.Width),
        ARequirement.Terms[I].OffsetX, AWrap, LX) then
      Continue;
    if not ResolveRequirementAxis(AY, Integer(AProvider.Dimension.Height),
        ARequirement.Terms[I].OffsetY, AWrap, LY) then
      Continue;
    if not ResolveRequirementAxis(AZ, Integer(AProvider.Dimension.Depth),
        ARequirement.Terms[I].OffsetZ, AWrap, LZ) then
      Continue;
    if not ProviderEntryAllows(AProvider.Entry[LX, LY, LZ],
        ARequirement.Terms[I].AllowedProviderTokens) then
      Continue;

    if ARequirement.CountMode = gpcmDistinctCells then
    begin
      LResolvedIndex :=
        (LZ * Integer(AProvider.Dimension.Height) + LY) *
        Integer(AProvider.Dimension.Width) + LX;
      LAlreadyMatched := False;
      for J := 0 to LCount - 1 do
        if AMatchedIndices[J] = LResolvedIndex then
        begin
          LAlreadyMatched := True;
          Break;
        end;
      if LAlreadyMatched then
        Continue;
      AMatchedIndices[LCount] := LResolvedIndex;
    end
    else if ARequirement.CountMode <> gpcmMatchingTerms then
      raise EInvalidOperation.Create(
        'recipe contains an unknown count requirement mode');
    Inc(LCount);
    if LCount > ARequirement.MaximumCount then
      Exit(False);
  end;
  Result := LCount >= ARequirement.MinimumCount;
end;

function PatternIssueEntry(const AGraph: TGraph;
  const AIssue: TWfcOverlapping2DIssue): Integer;
begin
  Result := -1;
  if (AIssue.X < 0) or (AIssue.Y < 0) or
      (AIssue.X >= Integer(AGraph.Dimension.Width)) or
      (AIssue.Y >= Integer(AGraph.Dimension.Height)) then
    Exit;
  Result := AIssue.Y * Integer(AGraph.Dimension.Width) + AIssue.X;
end;

constructor TWfcPipelineCommitGraph.CreatePass(const ARoot: TGraph;
  const APassIndex: Integer);
begin
  inherited CreatePass(ARoot, APassIndex);
  if not (ARoot is TWfcPipelineCommitGraph) then
    raise EInvalidOperation.Create(
      'compiled pipeline root has an incompatible graph class');
  FOwner := TWfcPipelineCommitGraph(ARoot).Owner;
end;

function TWfcPipelineCommitGraph.DoValidateCommit(
  out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
begin
  if not Assigned(FOwner) then
  begin
    AFailedPassIndex := 0;
    AFailedEntryIndex := -1;
    Exit(False);
  end;
  Result := FOwner.ValidatePendingCommit(AFailedPassIndex,
    AFailedEntryIndex);
end;

constructor TWfcCompiledPipeline.Create(const ARecipe: TWfcPipelineModel;
  const AWidth, AHeight, ADepth: Integer);
begin
  inherited Create;
  InitializeCommitValidation(FLastValidation);
  Initialize(ARecipe, AWidth, AHeight, ADepth);
end;

destructor TWfcCompiledPipeline.Destroy;
begin
  FGraph.Free;
  FGraph := nil;
  FRecipe := nil;
  inherited Destroy;
end;

function TWfcCompiledPipeline.QuotaMaterializedPass(
  const APassIndex: Integer): Integer;
var
  LSteps: Integer;
begin
  Result := APassIndex;
  LSteps := 0;
  while FRecipe.PassAt(Result).Mode = gpmTransform do
  begin
    Inc(LSteps);
    if LSteps >= FRecipe.PassCount then
      raise EInvalidOperation.Create('quota transform chain does not terminate');
    Result := FRecipe.PassAt(Result).TransformSourceIndex;
  end;
end;

function TWfcCompiledPipeline.ProjectionBridgeForPass(
  const APassIndex: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to FRecipe.BridgeCount - 1 do
    if FRecipe.BridgeAt(I).TargetPassIndex = APassIndex then
      Exit(I);
  Result := -1;
end;

procedure TWfcCompiledPipeline.ValidateValueQuotaWork;
var
  I, LBridgeIndex, LPassIndex, LCount, LTotal: Integer;
  LPass: TWfcPipelinePass;
begin
  LTotal := 0;
  for I := 0 to FRecipe.ValueQuotaCount - 1 do
  begin
    LPassIndex := QuotaMaterializedPass(FRecipe.ValueQuotaAt(I).PassIndex);
    LBridgeIndex := ProjectionBridgeForPass(LPassIndex);
    if LBridgeIndex < 0 then Continue;
    LPass := FRecipe.PassAt(FRecipe.BridgeAt(LBridgeIndex).SourcePassIndex);
    case FRecipe.BridgeAt(LBridgeIndex).Kind of
      wpbkPattern2DProjection:
        LCount := FRecipe.BorrowPattern2DResource(LPass.ResourceIndex).PatternCount;
      wpbkSequenceProjection:
        LCount := FRecipe.BorrowSequenceResource(LPass.ResourceIndex).StateCount;
    else
      raise EInvalidOperation.Create('unknown quota projection bridge');
    end;
    if LCount > WFC_PIPELINE_COMPILE_MAX_QUOTA_CANDIDATE_VISITS - LTotal then
      raise EWfcPipelineCompile.CreateFailure(wpcsValueQuotas, I,
        'aggregate quota lowering exceeds the candidate-visit limit');
    Inc(LTotal, LCount);
  end;
end;

procedure TWfcCompiledPipeline.InstallValueQuotas;
var
  I, J, LPassIndex, LBridgeIndex, LSourceIndex, LCount: Integer;
  LQuota: TWfcPipelineValueQuota;
  LBridge: TWfcPipelineBridge;
  LValues, LSourceValues, LAllowedValues: TGraphValues;
  LLookup: TWfcTokenLookup;
  LPattern: TWfcOverlappingModel2D;
  LSequence: TWfcSequenceModel;
  LToken: TWfcModelToken;
  LLabel: String;
begin
  for I := 0 to FRecipe.ValueQuotaCount - 1 do
  begin
    try
      LQuota := FRecipe.ValueQuotaAt(I);
      LPassIndex := QuotaMaterializedPass(LQuota.PassIndex);
      { Internal labels use immutable descriptor ordinals, not user labels:
        two public aliases may use the same label on one shared source. }
      LLabel := 'pipeline-value-quota:' + IntToStr(I);
      SetLength(LValues, Length(LQuota.Values));
      for J := 0 to Length(LValues) - 1 do
        LValues[J] := TokenToGraphValue(LQuota.Values[J], 'quota token');
      FGraph.SwitchToPass(LPassIndex);
      FGraph.RequireValueQuota(MakeGraphValueQuotaConstraint(LLabel,
        LValues, LQuota.MinimumCount, LQuota.MaximumCount));

      LBridgeIndex := ProjectionBridgeForPass(LPassIndex);
      if LBridgeIndex < 0 then Continue;
      LBridge := FRecipe.BridgeAt(LBridgeIndex);
      LSourceIndex := LBridge.SourcePassIndex;
      LSourceValues := FGraph.PassGraph[LSourceIndex].CopyRegisteredValues;
      SetLength(LAllowedValues, Length(LSourceValues));
      LCount := 0;
      LLookup := TWfcTokenLookup.Create(LQuota.Values);
      try
        LPattern := nil;
        LSequence := nil;
        case LBridge.Kind of
          wpbkPattern2DProjection:
            begin
              LPattern := FRecipe.BorrowPattern2DResource(
                FRecipe.PassAt(LSourceIndex).ResourceIndex);
              if Length(LSourceValues) <> LPattern.PatternCount then
                raise EInvalidOperation.Create('quota pattern registry mismatch');
            end;
          wpbkSequenceProjection:
            begin
              LSequence := FRecipe.BorrowSequenceResource(
                FRecipe.PassAt(LSourceIndex).ResourceIndex);
              if Length(LSourceValues) <> LSequence.StateCount then
                raise EInvalidOperation.Create('quota sequence registry mismatch');
            end;
        end;
        for J := 0 to Length(LSourceValues) - 1 do
        begin
          if Assigned(LPattern) then
            { Wrapped projection has one public cell per latent anchor. Do
              not count every overlapping footprint occurrence. }
            LToken := LPattern.PaletteTokenAt(
              LPattern.PatternPaletteIndexAt(J, 0, 0))
          else
            LToken := LSequence.PublicTokenAt(
              LSequence.StateEmittedTokenIndexAt(J));
          if LLookup.Find(LToken) >= 0 then
          begin
            LAllowedValues[LCount] := LSourceValues[J];
            Inc(LCount);
          end;
        end;
      finally
        LLookup.Free;
      end;
      SetLength(LAllowedValues, LCount);
      FGraph.SwitchToPass(LSourceIndex);
      if LCount > 0 then
        FGraph.RequireValueQuota(MakeGraphValueQuotaConstraint(LLabel,
          LAllowedValues, LQuota.MinimumCount, LQuota.MaximumCount))
      else if LQuota.MinimumCount > 0 then
        { No source state can emit a requested member. A positive-shape
          graph cannot contain zero members of its entire vocabulary. This
          expresses unsatisfiability without an illegal empty quota set. }
        FGraph.RequireValueQuota(MakeGraphValueQuotaConstraint(LLabel,
          LSourceValues, 0, 0));
    except
      on E: Exception do
        raise EWfcPipelineCompile.CreateFailure(wpcsValueQuotas, I, E.Message);
    end;
  end;
end;

function TWfcCompiledPipeline.ValidateValueQuotaCommit(
  out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
var
  I, J, LCount, LPassIndex: Integer;
  LQuota: TWfcPipelineValueQuota;
  LRecounts: array of TQuotaRecount;

  procedure RecountOwner(const APassIndex: Integer);
  var
    X, Y, Z, LTokenIndex: Integer;
    LEntry: TGraphEntry;
  begin
    if Assigned(LRecounts[APassIndex].Lookup) then Exit;
    LRecounts[APassIndex].Lookup := TWfcTokenLookup.Create(
      FRecipe.CopyPublicVocabulary(APassIndex));
    SetLength(LRecounts[APassIndex].Counts,
      LRecounts[APassIndex].Lookup.Count);
    LRecounts[APassIndex].InvalidEntry := -1;
    for Z := 0 to Integer(FGraph.Dimension.Depth) - 1 do
      for Y := 0 to Integer(FGraph.Dimension.Height) - 1 do
        for X := 0 to Integer(FGraph.Dimension.Width) - 1 do
        begin
          LEntry := FGraph.PassGraph[APassIndex].Entry[X, Y, Z];
          LTokenIndex := LRecounts[APassIndex].Lookup.Find(
            GraphValueToToken(LEntry.Value));
          if LEntry.Empty or (LTokenIndex < 0) then
          begin
            LRecounts[APassIndex].InvalidEntry :=
              (Z * Integer(FGraph.Dimension.Height) + Y) *
              Integer(FGraph.Dimension.Width) + X;
            Exit;
          end;
          Inc(LRecounts[APassIndex].Counts[LTokenIndex]);
        end;
  end;

begin
  Result := False;
  AFailedPassIndex := -1;
  AFailedEntryIndex := -1;
  if FRecipe.ValueQuotaCount = 0 then Exit(True);
  SetLength(LRecounts, FRecipe.PassCount);
  try
    for I := 0 to FRecipe.ValueQuotaCount - 1 do
    begin
      LQuota := FRecipe.ValueQuotaAt(I);
      LPassIndex := LQuota.PassIndex;
      { Transaction-local histograms scan each public owner once. They read
        immutable recipe vocabularies and live public entries, never mutable
        graph quota registrations, and cannot survive rollback or replay. }
      RecountOwner(LPassIndex);
      LCount := 0;
      for J := 0 to Length(LQuota.Values) - 1 do
        Inc(LCount, LRecounts[LPassIndex].Counts[
          LRecounts[LPassIndex].Lookup.Find(LQuota.Values[J])]);
      if (LRecounts[LPassIndex].InvalidEntry >= 0) or
          (LCount < LQuota.MinimumCount) or (LCount > LQuota.MaximumCount) then
      begin
        FLastValidation.Kind := wpcvkValueQuota;
        FLastValidation.PassIndex := LPassIndex;
        FLastValidation.ValueQuotaIndex := I;
        FLastValidation.EntryIndex := LRecounts[LPassIndex].InvalidEntry;
        AFailedPassIndex := LPassIndex;
        AFailedEntryIndex := FLastValidation.EntryIndex;
        Exit;
      end;
    end;
  finally
    for I := 0 to Length(LRecounts) - 1 do LRecounts[I].Lookup.Free;
  end;
  Result := True;
end;

procedure TWfcCompiledPipeline.InstallConnectivity;
var I, J, K, LPassIndex, LBridgeIndex, LSourceIndex, LCount: Integer;
  Q: TWfcPipelineConnectivity; B: TWfcPipelineBridge;
  LProfiles, LLatentProfiles: TGraphConnectivityValues;
  LSourceValues: TGraphValues; LProfileTokens: TWfcModelTokens;
  LLookup: TWfcTokenLookup; LPattern: TWfcOverlappingModel2D;
  LSequence: TWfcSequenceModel; LToken: TWfcModelToken; LLabel: String;
begin
  for I := 0 to FRecipe.ConnectivityCount - 1 do
  begin
    try
      Q := FRecipe.ConnectivityAt(I);
      LPassIndex := WfcPipelineMaterializedPublicPass(FRecipe, Q.PassIndex);
      LLabel := 'pipeline-connectivity:' + IntToStr(I);
      SetLength(LProfiles, Length(Q.Values));
      SetLength(LProfileTokens, Length(Q.Values));
      for J := 0 to Length(Q.Values) - 1 do
      begin
        LProfiles[J] := MakeGraphConnectivityValue(
          TokenToGraphValue(Q.Values[J].Value, 'connectivity public profile'),
          Q.Values[J].Openings, Q.Values[J].RequiredByValue);
        LProfileTokens[J] := Q.Values[J].Value;
      end;
      FGraph.SwitchToPass(LPassIndex);
      FGraph.RequireConnectivity(MakeGraphConnectivityConstraint(LLabel,
        Q.Root, Q.RequiredPositions, LProfiles, Q.RequireAllParticipants));
      LBridgeIndex := WfcPipelineProjectionBridgeForPass(FRecipe, LPassIndex);
      if LBridgeIndex < 0 then Continue;
      B := FRecipe.BridgeAt(LBridgeIndex); LSourceIndex := B.SourcePassIndex;
      LSourceValues := FGraph.PassGraph[LSourceIndex].CopyRegisteredValues;
      SetLength(LLatentProfiles, Length(LSourceValues));
      LLookup := TWfcTokenLookup.Create(LProfileTokens);
      try
        LPattern := nil; LSequence := nil;
        case B.Kind of
          wpbkPattern2DProjection:
            begin
              LPattern := FRecipe.BorrowPattern2DResource(
                FRecipe.PassAt(LSourceIndex).ResourceIndex);
              if Length(LSourceValues) <> LPattern.PatternCount then
                raise EInvalidOperation.Create('connectivity pattern registry mismatch');
            end;
          wpbkSequenceProjection:
            begin
              LSequence := FRecipe.BorrowSequenceResource(
                FRecipe.PassAt(LSourceIndex).ResourceIndex);
              if Length(LSourceValues) <> LSequence.StateCount then
                raise EInvalidOperation.Create('connectivity sequence registry mismatch');
            end;
        else
          raise EInvalidOperation.Create('unknown connectivity projection bridge');
        end;
        LCount := 0;
        for J := 0 to Length(LSourceValues) - 1 do
        begin
          if Assigned(LPattern) then
            LToken := LPattern.PaletteTokenAt(LPattern.PatternPaletteIndexAt(J, 0, 0))
          else
            LToken := LSequence.PublicTokenAt(LSequence.StateEmittedTokenIndexAt(J));
          K := LLookup.Find(LToken);
          if K < 0 then Continue;
          { One latent choice is one public cell. Preserve the exact public
            ports and mandatory flag on EVERY state emitting this token. }
          LLatentProfiles[LCount] := MakeGraphConnectivityValue(LSourceValues[J],
            Q.Values[K].Openings, Q.Values[K].RequiredByValue);
          Inc(LCount);
        end;
      finally LLookup.Free; end;
      if LCount = 0 then
        { Existing bridge adapters reject unrepresented public tokens first;
          do not invent participation when that invariant is broken. }
        raise EInvalidOperation.Create('connectivity projection has no participating source state');
      SetLength(LLatentProfiles, LCount);
      FGraph.SwitchToPass(LSourceIndex);
      FGraph.RequireConnectivity(MakeGraphConnectivityConstraint(LLabel,
        Q.Root, Q.RequiredPositions, LLatentProfiles, Q.RequireAllParticipants));
    except
      on E: Exception do
        raise EWfcPipelineCompile.CreateFailure(wpcsConnectivity, I, E.Message);
    end;
  end;
end;

function TWfcCompiledPipeline.ValidateConnectivityCommit(
  out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
var I, X, Y, Z, LWidth, LHeight, LDepth, LCell: Integer;
  Q: TWfcPipelineConnectivity; LTokens: TWfcModelTokens;
begin
  AFailedPassIndex := -1; AFailedEntryIndex := -1;
  if FRecipe.ConnectivityCount = 0 then Exit(True);
  LWidth := Integer(FGraph.Dimension.Width);
  LHeight := Integer(FGraph.Dimension.Height);
  LDepth := Integer(FGraph.Dimension.Depth);
  PreflightWfcPipelineConnectivity(FRecipe, LWidth, LHeight, LDepth, I);
  SetLength(LTokens, LWidth * LHeight * LDepth);
  for I := 0 to FRecipe.ConnectivityCount - 1 do
  begin
    Q := FRecipe.ConnectivityAt(I);
    LCell := 0;
    for Z := 0 to LDepth - 1 do
      for Y := 0 to LHeight - 1 do
        for X := 0 to LWidth - 1 do
        begin
          LTokens[LCell] := GraphValueToToken(FGraph.PassGraph[Q.PassIndex].Entry[X,Y,Z].Value);
          Inc(LCell);
        end;
    if not ValidateWfcPipelineConnectivity(FRecipe, I, LWidth, LHeight,
      LDepth, LTokens, AFailedEntryIndex) then
    begin
      AFailedPassIndex := Q.PassIndex;
      FLastValidation.Kind := wpcvkConnectivity;
      FLastValidation.PassIndex := Q.PassIndex;
      FLastValidation.ConnectivityIndex := I;
      FLastValidation.EntryIndex := AFailedEntryIndex;
      Exit(False);
    end;
  end;
  Result := True;
end;

procedure TWfcCompiledPipeline.Initialize(const ARecipe: TWfcPipelineModel;
  const AWidth, AHeight, ADepth: Integer);
var
  I: Integer;
  J: Integer;
  K: Integer;
  LActualValues: TGraphValues;
  LAllowedValues: TGraphValues;
  LBridge: TWfcPipelineBridge;
  LDependency: TWfcPipelineDependency;
  LExpectedTokens: TWfcModelTokens;
  LGraphLabels: TStringArray;
  LItemIndex: Integer;
  LPass: TWfcPipelinePass;
  LPassGraph: TGraph;
  LRequirement: TWfcPipelineRequirement;
  LSequenceModel: TWfcSequenceModel;
  LSourceValues: TGraphValues;
  LStage: TWfcPipelineCompileStage;
  LTerms: TGraphPassMatchTerms;
  LTokenIndex: Integer;
  LTokenValues: TGraphValues;
  LVersions: TWfcPipelineVersions;
begin
  if not Assigned(ARecipe) then
    raise EWfcPipelineCompile.CreateFailure(wpcsPreflight, -1,
      'recipe cannot be nil');
  FRecipe := ARecipe;
  LStage := wpcsPreflight;
  LItemIndex := -1;
  try
    ValidateRankShape(ARecipe, AWidth, AHeight, ADepth);
    ValidateValueQuotaWork;
    if ARecipe.ConnectivityCount <> 0 then
    begin
      LStage := wpcsConnectivity;
      PreflightWfcPipelineConnectivity(ARecipe, AWidth, AHeight, ADepth, LItemIndex);
      LStage := wpcsPreflight;
    end;
    LVersions := ARecipe.CopyVersions;
    if LVersions.BundleGraphAdapterVersion <>
        WFC_PIPELINE_COMPILER_VERSION then
      raise EArgumentException.CreateFmt(
        'unsupported bundle graph-adapter version [%d]',
        [LVersions.BundleGraphAdapterVersion]);
    SetLength(LGraphLabels, ARecipe.PassCount);
    for I := 0 to ARecipe.PassCount - 1 do
    begin
      LItemIndex := I;
      LGraphLabels[I] := TokenToGraphValue(
        ARecipe.PassAt(I).LabelName, 'pass label ' + IntToStr(I));
      for J := 0 to I - 1 do
        if LGraphLabels[I] = LGraphLabels[J] then
          raise EArgumentException.CreateFmt(
            'pass labels collide after graph-string conversion [%d, %d]',
            [J, I]);
    end;

    LStage := wpcsPasses;
    LItemIndex := -1;
    FGraph := TWfcPipelineCommitGraph.Create;
    TWfcPipelineCommitGraph(FGraph).Owner := Self;
    FGraph.WrapNeighbors := ARecipe.WrapNeighbors;
    FGraph.Mode := ARecipe.RunMode;
    FGraph.CurrentPass := LGraphLabels[0];
    for I := 1 to ARecipe.PassCount - 1 do
    begin
      LItemIndex := I;
      FGraph.SwitchToPass(LGraphLabels[I]);
    end;

    { Rebuild every mode from an empty dependency surface. This avoids
      retaining the core's sequential compatibility edge on overlay and
      transform passes. }
    for I := 0 to ARecipe.PassCount - 1 do
    begin
      LItemIndex := I;
      LPass := ARecipe.PassAt(I);
      FGraph.SwitchToPass(I);
      FGraph.PassMode := gpmOverlay;
      FGraph.ClearDependencies;
      case LPass.Mode of
        gpmLegacy:
          FGraph.PassMode := gpmLegacy;
        gpmTransform:
          FGraph.TransformFrom(LGraphLabels[LPass.TransformSourceIndex]);
        gpmOverlay:
          ;
      else
        raise ERangeError.Create('recipe contains an unknown pass mode');
      end;
    end;

    LStage := wpcsDependencies;
    for I := 0 to ARecipe.DependencyCount - 1 do
    begin
      LItemIndex := I;
      LDependency := ARecipe.DependencyAt(I);
      FGraph.SwitchToPass(LDependency.ConsumerPassIndex);
      FGraph.DependsOn(LGraphLabels[LDependency.ProviderPassIndex]);
    end;

    for I := 0 to ARecipe.PassCount - 1 do
    begin
      LItemIndex := I;
      LPassGraph := FGraph.PassGraph[I];
      if LPassGraph.DependencyCount <> ExpectedDependencyCount(ARecipe, I) then
        raise EInvalidOperation.CreateFmt(
          'constructed dependency count differs for pass %d [%d, %d]',
          [I, LPassGraph.DependencyCount,
           ExpectedDependencyCount(ARecipe, I)]);
      for J := 0 to LPassGraph.DependencyCount - 1 do
        if not RecipeHasDependency(ARecipe, I,
            LPassGraph.DependencyIndex[J]) then
          raise EInvalidOperation.CreateFmt(
            'constructed dependency %d -> %d is not declared',
            [I, LPassGraph.DependencyIndex[J]]);
    end;

    LStage := wpcsShape;
    LItemIndex := -1;
    FGraph.Reshape(AWidth, AHeight, ADepth);

    LStage := wpcsAdapters;
    for I := 0 to ARecipe.PassCount - 1 do
    begin
      LItemIndex := I;
      LPass := ARecipe.PassAt(I);
      FGraph.SwitchToPass(I);
      case LPass.AdapterKind of
        wpakEmpty:
          ;
        wpakModel:
          ApplyModelToGraph(ARecipe.BorrowModelResource(
            LPass.ResourceIndex), FGraph);
        wpakRules:
          ApplyRuleModelToGraph(ARecipe.BorrowRuleResource(
            LPass.ResourceIndex), FGraph);
        wpakPattern2D:
          ApplyOverlappingModel2DToGraph(
            ARecipe.BorrowPattern2DResource(LPass.ResourceIndex), FGraph);
        wpakSequence:
          ApplySequenceModelToGraph(
            ARecipe.BorrowSequenceResource(LPass.ResourceIndex), FGraph,
            LPass.SequenceExtent);
      else
        raise ERangeError.Create('recipe contains an unknown adapter kind');
      end;
    end;

    LStage := wpcsBridges;
    for I := 0 to ARecipe.BridgeCount - 1 do
    begin
      LItemIndex := I;
      LBridge := ARecipe.BridgeAt(I);
      FGraph.SwitchToPass(LBridge.TargetPassIndex);
      case LBridge.Kind of
        wpbkPattern2DProjection:
          ApplyOverlappingProjectionFromPass2D(
            ARecipe.BorrowPattern2DResource(
              ARecipe.PassAt(LBridge.SourcePassIndex).ResourceIndex),
            FGraph, LGraphLabels[LBridge.SourcePassIndex]);
        wpbkSequenceProjection:
          begin
            LSequenceModel := ARecipe.BorrowSequenceResource(
              ARecipe.PassAt(LBridge.SourcePassIndex).ResourceIndex);
            LSourceValues := FGraph.PassGraph[
              LBridge.SourcePassIndex].CopyRegisteredValues;
            if Length(LSourceValues) <> LSequenceModel.StateCount then
              raise EInvalidOperation.Create(
                'sequence adapter state registry does not match its model');

            SetLength(LTokenValues, LSequenceModel.PublicTokenCount);
            for J := 0 to LSequenceModel.PublicTokenCount - 1 do
              LTokenValues[J] := TokenToGraphValue(
                LSequenceModel.PublicTokenAt(J),
                'sequence bridge public token ' + IntToStr(J));
            for J := 0 to LSequenceModel.PublicTokenCount - 1 do
              FGraph.AddValue(LTokenValues[J], WFC_DEFAULT_VALUE_WEIGHT);

            for J := 0 to LSequenceModel.PublicTokenCount - 1 do
            begin
              SetLength(LAllowedValues, 0);
              for K := 0 to LSequenceModel.StateCount - 1 do
                if LSequenceModel.StateEmittedTokenIndexAt(K) = J then
                begin
                  SetLength(LAllowedValues, Length(LAllowedValues) + 1);
                  LAllowedValues[High(LAllowedValues)] := LSourceValues[K];
                end;
              if Length(LAllowedValues) = 0 then
                raise EInvalidOperation.CreateFmt(
                  'sequence bridge public token %d has no source state', [J]);
              FGraph.Rules[LTokenValues[J]].RequireFromPassAt(
                LGraphLabels[LBridge.SourcePassIndex],
                MakeGraphOffset(0, 0, 0), LAllowedValues);
            end;
          end;
      else
        raise ERangeError.Create('recipe contains an unknown bridge kind');
      end;
    end;

    LStage := wpcsRequirements;
    for I := 0 to ARecipe.RequirementCount - 1 do
    begin
      LItemIndex := I;
      LRequirement := ARecipe.RequirementAt(I);
      FGraph.SwitchToPass(LRequirement.ConsumerPassIndex);
      SetLength(LTerms, Length(LRequirement.Terms));
      for J := 0 to Length(LRequirement.Terms) - 1 do
      begin
        SetLength(LAllowedValues,
          Length(LRequirement.Terms[J].AllowedProviderTokens));
        for K := 0 to Length(LAllowedValues) - 1 do
          LAllowedValues[K] := TokenToGraphValue(
            LRequirement.Terms[J].AllowedProviderTokens[K],
            'requirement ' + IntToStr(I) + ' provider token ' +
            IntToStr(K));
        LTerms[J] := MakeGraphPassMatchTerm(
          MakeGraphOffset(LRequirement.Terms[J].OffsetX,
            LRequirement.Terms[J].OffsetY,
            LRequirement.Terms[J].OffsetZ), LAllowedValues);
      end;
      case LRequirement.Kind of
        wprqExact:
          FGraph.Rules[TokenToGraphValue(LRequirement.ConsumerToken,
            'requirement ' + IntToStr(I) + ' consumer token')]
            .RequireFromPassAt(
              LGraphLabels[LRequirement.ProviderPassIndex],
              LTerms[0].Offset, LTerms[0].Values);
        wprqAny:
          FGraph.Rules[TokenToGraphValue(LRequirement.ConsumerToken,
            'requirement ' + IntToStr(I) + ' consumer token')]
            .RequireAnyFromPass(
              LGraphLabels[LRequirement.ProviderPassIndex], LTerms);
        wprqCount:
          FGraph.Rules[TokenToGraphValue(LRequirement.ConsumerToken,
            'requirement ' + IntToStr(I) + ' consumer token')]
            .RequireCountFromPass(
              LGraphLabels[LRequirement.ProviderPassIndex], LTerms,
              LRequirement.MinimumCount, LRequirement.MaximumCount,
              LRequirement.CountMode);
      else
        raise ERangeError.Create(
          'recipe contains an unknown requirement kind');
      end;
    end;

    LStage := wpcsValueQuotas;
    LItemIndex := -1;
    InstallValueQuotas;

    LStage := wpcsConnectivity;
    LItemIndex := -1;
    InstallConnectivity;

    LStage := wpcsVerification;
    for I := 0 to ARecipe.PassCount - 1 do
    begin
      LItemIndex := I;
      LPass := ARecipe.PassAt(I);
      LPassGraph := FGraph.PassGraph[I];
      if LPassGraph.CurrentPass <> LGraphLabels[I] then
        raise EInvalidOperation.CreateFmt(
          'constructed pass label differs at index %d', [I]);
      if LPassGraph.PassMode <> LPass.Mode then
        raise EInvalidOperation.CreateFmt(
          'constructed pass mode differs at index %d', [I]);
      if LPassGraph.TransformSourceIndex <> LPass.TransformSourceIndex then
        raise EInvalidOperation.CreateFmt(
          'constructed transform source differs at index %d', [I]);

      if LPass.Visibility = wppvPublic then
      begin
        LExpectedTokens := ARecipe.CopyPublicVocabulary(I);
        LActualValues := LPassGraph.CopyRegisteredValues;
        if LPass.Mode = gpmTransform then
        begin
          if LPassGraph.HasDefinition or (Length(LActualValues) <> 0) then
            raise EInvalidOperation.CreateFmt(
              'public transform pass %d unexpectedly has a definition', [I]);
        end
        else
        begin
          if Length(LActualValues) <> Length(LExpectedTokens) then
            raise EInvalidOperation.CreateFmt(
              'public vocabulary count differs for pass %d [%d, %d]',
              [I, Length(LActualValues), Length(LExpectedTokens)]);
          for J := 0 to Length(LExpectedTokens) - 1 do
          begin
            LTokenIndex := J;
            if GraphValueToToken(LActualValues[J]) <>
                LExpectedTokens[LTokenIndex] then
              raise EInvalidOperation.CreateFmt(
                'public vocabulary differs for pass %d token %d', [I, J]);
          end;
        end;
      end;
    end;
    FGraph.SwitchToPass(0);
  except
    on E: EWfcPipelineCompile do
      raise;
    on E: Exception do
      raise EWfcPipelineCompile.CreateFailure(LStage, LItemIndex,
        E.Message);
  end;
end;

function TWfcCompiledPipeline.ValidatePendingCommit(
  out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
var
  I: Integer;
  LBridge: TWfcPipelineBridge;
  LEntry: TGraphEntry;
  LPass: TWfcPipelinePass;
  LPosition: Integer;
  LPatternGrid: TWfcPatternGrid2D;
  LPatternReport: TWfcOverlapping2DValidationReport;
  LProjection: TWfcTokenGrid2D;
  LProviderGraph: TGraph;
  LRequirement: TWfcPipelineRequirement;
  LSequence: TWfcGeneratedSequence;
  LSequenceReport: TWfcSequenceGraphValidationReport;
  LSourceEntry: TGraphEntry;
  LToken: TWfcModelToken;
  LConsumerValue: TGraphValue;
  LCountMatchedIndices: TIntegerArray;
  LMatched: Boolean;
  X: Integer;
  Y: Integer;
  Z: Integer;
begin
  InitializeCommitValidation(FLastValidation);
  AFailedPassIndex := -1;
  AFailedEntryIndex := -1;

  { Validate private typed representations before their materialized public
    bridges. Rule and generic-model passes use the graph's own complete local
    constraint surface; the current adapters do not expose independent solved
    capture validators for those two resource kinds. }
  for I := 0 to FRecipe.PassCount - 1 do
  begin
    LPass := FRecipe.PassAt(I);
    case LPass.AdapterKind of
      wpakEmpty, wpakModel, wpakRules:
        ;
      wpakPattern2D:
        if not CaptureSolvedPatternGrid2D(
            FRecipe.BorrowPattern2DResource(LPass.ResourceIndex),
            FGraph.PassGraph[I], 0, LPatternGrid, LPatternReport) then
        begin
          FLastValidation.Kind := wpcvkPatternPass;
          FLastValidation.PassIndex := I;
          FLastValidation.EntryIndex := PatternIssueEntry(
            FGraph.PassGraph[I], LPatternReport.Issue);
          AFailedPassIndex := I;
          AFailedEntryIndex := FLastValidation.EntryIndex;
          Exit(False);
        end;
      wpakSequence:
        if not CaptureSolvedSequence(
            FRecipe.BorrowSequenceResource(LPass.ResourceIndex),
            FGraph.PassGraph[I], LPass.SequenceExtent,
            LSequence, LSequenceReport) then
        begin
          FLastValidation.Kind := wpcvkSequencePass;
          FLastValidation.PassIndex := I;
          FLastValidation.EntryIndex := LSequenceReport.Issue.Position;
          AFailedPassIndex := I;
          AFailedEntryIndex := FLastValidation.EntryIndex;
          Exit(False);
        end;
    end;
  end;

  { A caller lock on a definitionless transform is observed by the core as a
    pre-existing value and therefore takes precedence over ordinary copying.
    Recheck exact source-copy semantics inside the transaction so such a lock
    cannot silently change the declarative transform. }
  for I := 0 to FRecipe.PassCount - 1 do
  begin
    LPass := FRecipe.PassAt(I);
    if LPass.Mode <> gpmTransform then
      Continue;
    for Z := 0 to Integer(FGraph.Dimension.Depth) - 1 do
      for Y := 0 to Integer(FGraph.Dimension.Height) - 1 do
        for X := 0 to Integer(FGraph.Dimension.Width) - 1 do
        begin
          LSourceEntry := FGraph.PassGraph[
            LPass.TransformSourceIndex].Entry[X, Y, Z];
          LEntry := FGraph.PassGraph[I].Entry[X, Y, Z];
          if (LEntry.Empty <> LSourceEntry.Empty) or
              ((not LEntry.Empty) and
               (LEntry.Value <> LSourceEntry.Value)) then
          begin
            LPosition := (Z * Integer(FGraph.Dimension.Height) + Y) *
              Integer(FGraph.Dimension.Width) + X;
            FLastValidation.Kind := wpcvkTransform;
            FLastValidation.PassIndex := I;
            FLastValidation.EntryIndex := LPosition;
            AFailedPassIndex := I;
            AFailedEntryIndex := LPosition;
            Exit(False);
          end;
        end;
  end;

  for I := 0 to FRecipe.BridgeCount - 1 do
  begin
    LBridge := FRecipe.BridgeAt(I);
    case LBridge.Kind of
      wpbkPattern2DProjection:
        if not CaptureSolvedOverlappingProjectionPass2D(
            FRecipe.BorrowPattern2DResource(
              FRecipe.PassAt(LBridge.SourcePassIndex).ResourceIndex),
            FGraph.PassGraph[LBridge.SourcePassIndex],
            FGraph.PassGraph[LBridge.TargetPassIndex],
            LPatternGrid, LProjection, LPatternReport) then
        begin
          FLastValidation.Kind := wpcvkPatternBridge;
          FLastValidation.PassIndex := LBridge.TargetPassIndex;
          FLastValidation.BridgeIndex := I;
          FLastValidation.EntryIndex := PatternIssueEntry(
            FGraph.PassGraph[LBridge.TargetPassIndex],
            LPatternReport.Issue);
          AFailedPassIndex := LBridge.TargetPassIndex;
          AFailedEntryIndex := FLastValidation.EntryIndex;
          Exit(False);
        end;
      wpbkSequenceProjection:
        begin
          LPass := FRecipe.PassAt(LBridge.SourcePassIndex);
          if not CaptureSolvedSequence(
              FRecipe.BorrowSequenceResource(LPass.ResourceIndex),
              FGraph.PassGraph[LBridge.SourcePassIndex],
              LPass.SequenceExtent, LSequence, LSequenceReport) then
          begin
            FLastValidation.Kind := wpcvkSequenceBridge;
            FLastValidation.PassIndex := LBridge.SourcePassIndex;
            FLastValidation.BridgeIndex := I;
            FLastValidation.EntryIndex := LSequenceReport.Issue.Position;
            AFailedPassIndex := LBridge.SourcePassIndex;
            AFailedEntryIndex := FLastValidation.EntryIndex;
            Exit(False);
          end;
          for LPosition := 0 to Length(LSequence.Tokens) - 1 do
          begin
            LEntry := FGraph.PassGraph[LBridge.TargetPassIndex].Entry[
              TGraphCoordinate(LPosition), 0, 0];
            if LEntry.Empty then
              LToken := ''
            else
              LToken := GraphValueToToken(LEntry.Value);
            if LEntry.Empty or
                (LToken <> LSequence.Tokens[LPosition]) then
            begin
              FLastValidation.Kind := wpcvkSequenceBridge;
              FLastValidation.PassIndex := LBridge.TargetPassIndex;
              FLastValidation.BridgeIndex := I;
              FLastValidation.EntryIndex := LPosition;
              AFailedPassIndex := LBridge.TargetPassIndex;
              AFailedEntryIndex := LPosition;
              Exit(False);
            end;
          end;
        end;
    end;
  end;

  { Re-evaluate the portable requirement IR rather than inspecting the core's
    private compiled clauses. This catches any divergence caused by later
    graph mutation and keeps exact/open/wrapped offset semantics independently
    testable at the rollback-capable boundary. }
  for I := 0 to FRecipe.RequirementCount - 1 do
  begin
    LRequirement := FRecipe.RequirementAt(I);
    LProviderGraph := FGraph.PassGraph[
      LRequirement.ProviderPassIndex];
    if (LRequirement.Kind = wprqCount) and
        (LRequirement.CountMode = gpcmDistinctCells) then
      SetLength(LCountMatchedIndices, Length(LRequirement.Terms))
    else
      SetLength(LCountMatchedIndices, 0);
    LConsumerValue := TokenToGraphValue(LRequirement.ConsumerToken,
      'commit requirement consumer token');
    for Z := 0 to Integer(FGraph.Dimension.Depth) - 1 do
      for Y := 0 to Integer(FGraph.Dimension.Height) - 1 do
        for X := 0 to Integer(FGraph.Dimension.Width) - 1 do
        begin
          LEntry := FGraph.PassGraph[
            LRequirement.ConsumerPassIndex].Entry[X, Y, Z];
          if LEntry.Empty or (LEntry.Value <> LConsumerValue) then
            Continue;
          case LRequirement.Kind of
            wprqExact, wprqAny:
              begin
                LMatched := False;
                for LPosition := 0 to
                    Length(LRequirement.Terms) - 1 do
                  if RequirementTermMatches(LProviderGraph, X, Y, Z,
                      LRequirement.Terms[LPosition],
                      FRecipe.WrapNeighbors) then
                  begin
                    LMatched := True;
                    Break;
                  end;
              end;
            wprqCount:
              LMatched := RequirementCountMatches(LProviderGraph,
                X, Y, Z, LRequirement, FRecipe.WrapNeighbors,
                LCountMatchedIndices);
          else
            raise EInvalidOperation.Create(
              'recipe contains an unknown requirement kind');
          end;
          if not LMatched then
          begin
            FLastValidation.Kind := wpcvkRequirement;
            FLastValidation.PassIndex :=
              LRequirement.ConsumerPassIndex;
            FLastValidation.RequirementIndex := I;
            FLastValidation.EntryIndex :=
              (Z * Integer(FGraph.Dimension.Height) + Y) *
              Integer(FGraph.Dimension.Width) + X;
            AFailedPassIndex := FLastValidation.PassIndex;
            AFailedEntryIndex := FLastValidation.EntryIndex;
            Exit(False);
          end;
        end;
  end;
  Result := ValidateValueQuotaCommit(AFailedPassIndex, AFailedEntryIndex);
  if Result then
    Result := ValidateConnectivityCommit(AFailedPassIndex, AFailedEntryIndex);
end;

function CompileWfcPipeline(const ARecipe: TWfcPipelineModel;
  const AWidth, AHeight, ADepth: Integer): TWfcCompiledPipeline;
begin
  Result := TWfcCompiledPipeline.Create(ARecipe, AWidth, AHeight, ADepth);
end;

end.
