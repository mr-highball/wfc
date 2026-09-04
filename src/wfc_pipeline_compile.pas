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

type
  TWfcPipelineCompileStage = (
    wpcsPreflight,
    wpcsPasses,
    wpcsDependencies,
    wpcsShape,
    wpcsAdapters,
    wpcsBridges,
    wpcsRequirements,
    wpcsVerification
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
    wpcvkRequirement
  );

  TWfcPipelineCommitValidation = record
    Kind: TWfcPipelineCommitValidationKind;
    PassIndex: Integer;
    BridgeIndex: Integer;
    RequirementIndex: Integer;
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
  wfc_sequence_graph;

type
  TStringArray = array of String;

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
      else
        raise ERangeError.Create(
          'recipe contains an unknown requirement kind');
      end;
    end;

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
          LMatched := False;
          for LPosition := 0 to Length(LRequirement.Terms) - 1 do
            if RequirementTermMatches(LProviderGraph, X, Y, Z,
                LRequirement.Terms[LPosition],
                FRecipe.WrapNeighbors) then
            begin
              LMatched := True;
              Break;
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
  Result := True;
end;

function CompileWfcPipeline(const ARecipe: TWfcPipelineModel;
  const AWidth, AHeight, ADepth: Integer): TWfcCompiledPipeline;
begin
  Result := TWfcCompiledPipeline.Create(ARecipe, AWidth, AHeight, ADepth);
end;

end.
