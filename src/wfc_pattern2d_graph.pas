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
unit wfc_pattern2d_graph;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  wfc,
  wfc_model,
  wfc_pattern2d;

const
  { Identifies the conversion from a wrapped latent pattern pass to one
    same-sized public palette pass. }
  WFC_PATTERN_2D_GRAPH_ADAPTER_VERSION = 1;
  WFC_PATTERN_2D_PASS_PIPELINE_VERSION = 1;
  WFC_PATTERN_2D_COMPOSITION_SIGNATURE_VERSION = 1;

  WFC_PATTERN_2D_PASS_PATTERNS = 'patterns';
  WFC_PATTERN_2D_PASS_PROJECTION = 'projection';

type
  EWfcPattern2DGraph = class(EWfcOverlapping2D);

  TWfcPattern2DPassLayer = (
    wpplPatterns,
    wpplProjection
  );

  TWfcPattern2DPassStatus = (
    wpppsNotRun,
    wpppsCompleted,
    wpppsSolveFailed,
    wpppsCaptureFailed,
    wpppsValidationFailed
  );

  TWfcPattern2DCompositionSignature = Cardinal;

  TWfcPattern2DPassConfig = record
    Width: Integer;
    Height: Integer;
    Seed: TGraphSeed;
    Model: TWfcOverlappingModel2D;
  end;

  TWfcPattern2DPassReport = record
    Status: TWfcPattern2DPassStatus;
    FailedLayer: TWfcPattern2DPassLayer;
    Solve: TGraphSolveReport;
    Validation: TWfcOverlapping2DValidationReport;
  end;

  { Immutable public result. Pattern indices are stable model identities; the
    graph-private @pN strings never leave the adapter. Copy methods detach all
    managed arrays from the result. }
  TWfcPattern2DComposition = class
  private
    FSeed: TGraphSeed;
    FPatterns: TWfcPatternGrid2D;
    FProjection: TWfcTokenGrid2D;
    FSignature: TWfcPattern2DCompositionSignature;
    constructor CreateInternal(const AModel: TWfcOverlappingModel2D;
      const ASeed: TGraphSeed; const APatterns: TWfcPatternGrid2D;
      const AProjection: TWfcTokenGrid2D);
    function GetWidth: Integer;
    function GetHeight: Integer;
  public
    function CopyPatternGrid: TWfcPatternGrid2D;
    function CopyProjection: TWfcTokenGrid2D;

    property Seed: TGraphSeed read FSeed;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Signature: TWfcPattern2DCompositionSignature read FSignature;
  end;

  { Ready-to-use patterns -> projection owner. The model remains caller-owned
    and must outlive the pipeline. Its graph is intentionally not exposed;
    applications that need a larger DAG use the free bridge below. }
  TWfcPattern2DPassPipeline = class
  strict private
    FGraph: TGraph;
    FModel: TWfcOverlappingModel2D;
    FWidth: Integer;
    FHeight: Integer;
    FCommittedSeed: TGraphSeed;
    FHasCommit: Boolean;
    FPendingComposition: TWfcPattern2DComposition;
    FPendingValidation: TWfcOverlapping2DValidationReport;
    FPendingStatus: TWfcPattern2DPassStatus;
    FPendingFailedLayer: TWfcPattern2DPassLayer;

    procedure Initialize(const AConfig: TWfcPattern2DPassConfig);
    procedure ClearPendingCommit;
    function GetSeed: TGraphSeed;
    procedure SetSeed(const AValue: TGraphSeed);
    function CaptureComposition(const ASeed: TGraphSeed;
      out AComposition: TWfcPattern2DComposition;
      out AValidation: TWfcOverlapping2DValidationReport;
      out AFailedLayer: TWfcPattern2DPassLayer;
      out AStatus: TWfcPattern2DPassStatus): Boolean;
    function TakePendingComposition(
      out AComposition: TWfcPattern2DComposition;
      out AValidation: TWfcOverlapping2DValidationReport;
      out AFailedLayer: TWfcPattern2DPassLayer;
      out AStatus: TWfcPattern2DPassStatus): Boolean;
  private
    function ValidatePendingCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean;
  strict protected
    { Extension hook for a domain owner that needs semantics in addition to
      exact pattern projection. Returning False rejects the graph transaction
      while its entry and random-stream snapshots are still live. }
    function DoValidateProjection(const APatterns: TWfcPatternGrid2D;
      const AProjection: TWfcTokenGrid2D;
      out AReport: TWfcOverlapping2DValidationReport): Boolean; virtual;
  public
    constructor Create(const AConfig: TWfcPattern2DPassConfig);
    destructor Destroy; override;

    function TryGenerate(const AOptions: TGraphSolveOptions;
      out AComposition: TWfcPattern2DComposition;
      out AReport: TWfcPattern2DPassReport): Boolean; overload;
    function TryGenerate(out AComposition: TWfcPattern2DComposition;
      out AReport: TWfcPattern2DPassReport): Boolean; overload;
    function TryCopyCommitted(out AComposition: TWfcPattern2DComposition;
      out AValidation: TWfcOverlapping2DValidationReport): Boolean;
    function Validate(const AComposition: TWfcPattern2DComposition;
      out AReport: TWfcOverlapping2DValidationReport): Boolean;

    property Model: TWfcOverlappingModel2D read FModel;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Seed: TGraphSeed read GetSeed write SetSeed;
  end;

function DefaultWfcPattern2DPassConfig(
  const AModel: TWfcOverlappingModel2D;
  const AWidth, AHeight: Integer;
  const ASeed: TGraphSeed): TWfcPattern2DPassConfig;

{ Public bridge preflight shared by declarative recipes and graph adapters. }
function WfcPattern2DTokenUsesReservedKeySyntax(
  const AToken: TWfcModelToken): Boolean;

{ Preflights the complete bridge without changing the target pass. The active
  target must be an empty overlay pass in the same wrapped, depth-one graph as
  the named source pass. Source identity means its exact compiled latent graph
  semantics: ordered keys, weights, rules, and denials. Isomorphic wrapper
  metadata that is absent from TGraph cannot be distinguished here. }
procedure ValidateOverlappingProjectionFromPass2D(
  const AModel: TWfcOverlappingModel2D;
  const ATargetGraph: TGraph; const ASourcePass: String);

{ Materializes the model's public palette in the active target pass. For each
  public token and footprint coordinate (PX,PY), one exact signed-offset clause
  requires a matching latent pattern at (X-PX,Y-PY). Distinct offsets are AND
  clauses; pattern alternatives within one offset are OR choices. }
procedure ApplyOverlappingProjectionFromPass2D(
  const AModel: TWfcOverlappingModel2D;
  const ATargetGraph: TGraph; const ASourcePass: String);

{ Captures exact pass objects without changing root pass selection and checks
  every latent overlap and every footprint contribution independently. }
function CaptureSolvedOverlappingProjectionPass2D(
  const AModel: TWfcOverlappingModel2D;
  const APatternGraph, AProjectionGraph: TGraph;
  out APatternGrid: TWfcPatternGrid2D;
  out AProjection: TWfcTokenGrid2D;
  out AReport: TWfcOverlapping2DValidationReport): Boolean;

function CalculateWfcPattern2DCompositionSignature(
  const AModel: TWfcOverlappingModel2D;
  const ASeed: TGraphSeed;
  const APatterns: TWfcPatternGrid2D;
  const AProjection: TWfcTokenGrid2D):
  TWfcPattern2DCompositionSignature; overload;
function CalculateWfcPattern2DCompositionSignature(
  const AModel: TWfcOverlappingModel2D;
  const AComposition: TWfcPattern2DComposition):
  TWfcPattern2DCompositionSignature; overload;
function WfcPattern2DCompositionSignatureHex(
  const ASignature: TWfcPattern2DCompositionSignature): String;

implementation

uses
  wfc_text_codec;

type
  TPatternByteArray = array of Byte;
  TPatternIntegerArray = array of Integer;
  TPatternGraphValueArrays = array of TGraphValues;

  TPreparedPatternProjection = record
    SourcePass: String;
    PublicValues: TGraphValues;
    { Flattened [palette * footprint + offset]. }
    SourceValues: TPatternGraphValueArrays;
  end;

  { The core invokes this after all staged values have been copied to entries,
    while the transaction can still restore entries and random streams. }
  TWfcPattern2DPassCommitGraph = class(TGraph)
  private
    FOwner: TWfcPattern2DPassPipeline;
  strict protected
    function DoValidateCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean; override;
  public
    constructor CreatePass(const ARoot: TGraph;
      const APassIndex: Integer); override;
    property Owner: TWfcPattern2DPassPipeline read FOwner write FOwner;
  end;

function CheckedProduct(const A, B: Integer;
  const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) then
    raise ERangeError.Create(ALabel + ' cannot be negative');
  if (A <> 0) and (B > High(Integer) div A) then
    raise ERangeError.Create(ALabel + ' exceeds the Integer range');
  Result := A * B;
end;

function ModelTokenToGraphValue(
  const AToken: TWfcModelToken): TGraphValue;
begin
  {$IFDEF PAS2JS}
  Result := TGraphValue(AToken);
  {$ELSE}
  Result := TGraphValue(UTF8Decode(AToken));
  {$ENDIF}
end;

function GraphValueToModelToken(
  const AValue: TGraphValue): TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(AValue);
  {$ELSE}
  Result := UTF8Encode(UnicodeString(AValue));
  {$ENDIF}
end;

function WfcPattern2DTokenUsesReservedKeySyntax(
  const AToken: TWfcModelToken): Boolean;
var
  I: Integer;
begin
  Result := (Length(AToken) >= 3) and
    (AToken[1] = '@') and (AToken[2] = 'p');
  if not Result then
    Exit;
  for I := 3 to Length(AToken) do
    if not (AToken[I] in ['0'..'9']) then
      Exit(False);
end;

function PatternGraphValue(const AModel: TWfcOverlappingModel2D;
  const APatternIndex: Integer): TGraphValue;
begin
  Result := ModelTokenToGraphValue(AModel.PatternKeyAt(APatternIndex));
end;

function ModelDirectionForGraphDirection(
  const ADirection: TGraphDirection;
  out AModelDirection: TWfcModelDirection): Boolean;
begin
  Result := True;
  case ADirection of
    gdNorth:
      AModelDirection := wmdNorth;
    gdWest:
      AModelDirection := wmdEast;
    gdSouth:
      AModelDirection := wmdSouth;
    gdEast:
      AModelDirection := wmdWest;
  else
    Result := False;
  end;
end;

function PatternRuleGroupMatches(
  const AModel: TWfcOverlappingModel2D;
  const AGroup: TGraphRuleGroup;
  const APatternIndex: Integer): Boolean;
var
  D: TGraphDirection;
  I: Integer;
  LExpectedRuleCount: Integer;
  LExpectedTargetCount: Integer;
  LModelDirection: TWfcModelDirection;
  LRule: TGraphRule;
  LTargetIndex: Integer;
begin
  Result := False;
  LExpectedRuleCount := 0;
  for D := Low(TGraphDirection) to High(TGraphDirection) do
  begin
    if not ModelDirectionForGraphDirection(D, LModelDirection) then
    begin
      if AGroup.Exists[D] or AGroup.Denied[D] then
        Exit;
      Continue;
    end;

    LExpectedTargetCount := 0;
    for I := 0 to AModel.PatternCount - 1 do
      if AModel.CompiledModel.RelationCount(LModelDirection,
          APatternIndex, I) > 0 then
        Inc(LExpectedTargetCount);
    if LExpectedTargetCount = 0 then
    begin
      if AGroup.Exists[D] or (not AGroup.Denied[D]) then
        Exit;
    end
    else
    begin
      Inc(LExpectedRuleCount);
      if (not AGroup.Exists[D]) or AGroup.Denied[D] then
        Exit;
      LRule := AGroup[D];
      if LRule.Info or
          (Length(LRule.Value) <> LExpectedTargetCount) then
        Exit;
      LTargetIndex := 0;
      for I := 0 to AModel.PatternCount - 1 do
        if AModel.CompiledModel.RelationCount(LModelDirection,
            APatternIndex, I) > 0 then
        begin
          if LRule.Value[LTargetIndex] <>
              PatternGraphValue(AModel, I) then
            Exit;
          Inc(LTargetIndex);
        end;
    end;
  end;
  Result := Length(AGroup.Rules) = LExpectedRuleCount;
end;

function AppliedPatternModelMatches(
  const AModel: TWfcOverlappingModel2D;
  const AGraph: TGraph): Boolean;
var
  I: Integer;
  LActivePass: TGraph;
  LExpectedValues: TGraphValues;
  LGroup: TGraphRuleGroup;
  LParented: TGraph.TParentedGraphRuleGroup;
  LValue: TGraphValue;
begin
  { TGraph retains the compiled latent definition, not its originating wrapper
    object or public payload names. Compare every observable semantic input;
    models that compile to the same ordered latent graph are equivalent here. }
  Result := False;
  if (not Assigned(AModel)) or (not Assigned(AGraph)) or
      (AGraph.RuleGroups.Count <> AModel.PatternCount) then
    Exit;
  LExpectedValues := AGraph.CopyRegisteredValues;
  if Length(LExpectedValues) <> AModel.PatternCount then
    Exit;
  LActivePass := AGraph.PassGraph[AGraph.CurrentPassIndex];
  for I := 0 to AModel.PatternCount - 1 do
  begin
    LValue := PatternGraphValue(AModel, I);
    if LExpectedValues[I] <> LValue then
      Exit;
    if (not AGraph.RuleGroups.TryGetValue(LValue, LGroup)) or
        (not Assigned(LGroup)) or (LGroup.Value <> LValue) or
        (not (LGroup is TGraph.TParentedGraphRuleGroup)) then
      Exit;
    LParented := TGraph.TParentedGraphRuleGroup(LGroup);
    if (LParented.Parent <> LActivePass) or
        (LGroup.Weight <> AModel.PatternWeightAt(I)) or
        (not PatternRuleGroupMatches(AModel, LGroup, I)) then
      Exit;
  end;
  Result := True;
end;

procedure RequireAssigned(const AModel: TWfcOverlappingModel2D;
  const AGraph: TGraph);
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create(
      'overlapping projection model cannot be nil');
  if not Assigned(AGraph) then
    raise EArgumentNilException.Create(
      'overlapping projection graph cannot be nil');
end;

procedure ValidateWrappedPlane(const AGraph: TGraph;
  const AOperation: String);
begin
  if (AGraph.Dimension.Width = 0) or
      (AGraph.Dimension.Height = 0) or
      (AGraph.Dimension.Width > TGraphCoordinate(High(Integer))) or
      (AGraph.Dimension.Height > TGraphCoordinate(High(Integer))) or
      (AGraph.Dimension.Depth <> 1) or
      (Integer(AGraph.Dimension.Width) >
        High(Integer) div Integer(AGraph.Dimension.Height)) then
    raise EWfcPattern2DGraph.Create(AOperation +
      ' requires a positive depth-one 2D graph');
  if not AGraph.WrapNeighbors then
    raise EWfcPattern2DGraph.Create(AOperation +
      ' v1 requires wrapped graph topology');
end;

function FindPassGraph(const AGraph: TGraph; const APass,
  AOperation: String): TGraph;
var
  I: Integer;
  LPass: TGraph;
begin
  for I := 0 to AGraph.TotalPassCount - 1 do
  begin
    LPass := AGraph.PassGraph[I];
    if LPass.CurrentPass = APass then
      Exit(LPass);
  end;
  raise EWfcPattern2DGraph.CreateFmt(
    '%s cannot find source pass "%s"', [AOperation, APass]);
end;

procedure ValidateProjectionDependencyEdge(const ATargetGraph,
  ASourceGraph: TGraph; const ASourcePass: String);
var
  I: Integer;
  LDependencyIndex: Integer;
  LNodeGraph: TGraph;
  LNodeIndex: Integer;
  LPassCount: Integer;
  LSeen: TPatternByteArray;
  LSourceIndex: Integer;
  LStack: TPatternIntegerArray;
  LStackCount: Integer;
  LTargetIndex: Integer;
begin
  LPassCount := ATargetGraph.TotalPassCount;
  LTargetIndex := ATargetGraph.CurrentPassIndex;
  LSourceIndex := ASourceGraph.CurrentPassIndex;
  if (LTargetIndex < 0) or (LTargetIndex >= LPassCount) or
      (LSourceIndex < 0) or (LSourceIndex >= LPassCount) then
    raise EWfcPattern2DGraph.Create(
      'overlapping projection has an invalid pass index');
  if LSourceIndex = LTargetIndex then
    raise EWfcPattern2DGraph.Create(
      'overlapping projection cannot depend on its own pass');

  SetLength(LSeen, LPassCount);
  SetLength(LStack, LPassCount);
  LStackCount := 1;
  LStack[0] := LSourceIndex;
  LSeen[LSourceIndex] := 1;
  while LStackCount > 0 do
  begin
    Dec(LStackCount);
    LNodeIndex := LStack[LStackCount];
    if LNodeIndex = LTargetIndex then
      raise EWfcPattern2DGraph.CreateFmt(
        'overlapping projection dependency on pass "%s" would create a cycle',
        [ASourcePass]);
    LNodeGraph := ATargetGraph.PassGraph[LNodeIndex];
    for I := 0 to LNodeGraph.DependencyCount - 1 do
    begin
      LDependencyIndex := LNodeGraph.DependencyIndex[I];
      if (LDependencyIndex < 0) or
          (LDependencyIndex >= LPassCount) then
        raise EWfcPattern2DGraph.Create(
          'overlapping projection found a malformed dependency graph');
      if LSeen[LDependencyIndex] = 0 then
      begin
        if LStackCount >= Length(LStack) then
          raise EWfcPattern2DGraph.Create(
            'overlapping projection found a malformed dependency graph');
        LStack[LStackCount] := LDependencyIndex;
        Inc(LStackCount);
        LSeen[LDependencyIndex] := 1;
      end;
    end;
    LSeen[LNodeIndex] := 2;
  end;
end;

procedure PrepareProjection(const AModel: TWfcOverlappingModel2D;
  const ATargetGraph: TGraph; const ASourcePass: String;
  out APrepared: TPreparedPatternProjection);
var
  I: Integer;
  J: Integer;
  LFootprintSize: Integer;
  LOffsetIndex: Integer;
  LPaletteIndex: Integer;
  LPatternIndex: Integer;
  LSourceGraph: TGraph;
  LTargetGraph: TGraph;
begin
  APrepared := Default(TPreparedPatternProjection);
  RequireAssigned(AModel, ATargetGraph);
  if ATargetGraph.Running then
    raise EWfcPattern2DGraph.Create(
      'overlapping projection cannot change while the pipeline is running');
  ValidateWrappedPlane(ATargetGraph, 'overlapping projection');
  LTargetGraph := ATargetGraph.PassGraph[
    ATargetGraph.CurrentPassIndex];
  if LTargetGraph.PassMode <> gpmOverlay then
    raise EWfcPattern2DGraph.Create(
      'overlapping projection target must be an overlay pass');
  if LTargetGraph.HasDefinition or
      (LTargetGraph.RuleGroups.Count <> 0) or
      (Length(LTargetGraph.CopyRegisteredValues) <> 0) then
    raise EWfcPattern2DGraph.Create(
      'overlapping projection target pass must be empty');

  LSourceGraph := FindPassGraph(ATargetGraph, ASourcePass,
    'overlapping projection');
  ValidateWrappedPlane(LSourceGraph, 'overlapping projection source');
  if not AppliedPatternModelMatches(AModel, LSourceGraph) then
    raise EWfcPattern2DGraph.Create(
      'overlapping projection source does not contain the matching compiled latent model');
  ValidateProjectionDependencyEdge(ATargetGraph, LSourceGraph,
    ASourcePass);

  APrepared.SourcePass := ASourcePass;
  SetLength(APrepared.PublicValues, AModel.PaletteCount);
  for I := 0 to AModel.PaletteCount - 1 do
  begin
    if WfcPattern2DTokenUsesReservedKeySyntax(
        AModel.PaletteTokenAt(I)) then
      raise EWfcPattern2DGraph.CreateFmt(
        'overlapping palette token %d uses the reserved latent-key syntax',
        [I]);
    APrepared.PublicValues[I] := ModelTokenToGraphValue(
      AModel.PaletteTokenAt(I));
    if (Length(APrepared.PublicValues[I]) = 0) or
        (GraphValueToModelToken(APrepared.PublicValues[I]) <>
          AModel.PaletteTokenAt(I)) then
      raise EWfcPattern2DGraph.CreateFmt(
        'overlapping palette token cannot be represented by graph strings [%d]',
        [I]);
    for J := 0 to I - 1 do
      if APrepared.PublicValues[I] = APrepared.PublicValues[J] then
        raise EWfcPattern2DGraph.CreateFmt(
          'overlapping palette conversion is not unique [%d, %d]', [J, I]);
  end;

  LFootprintSize := CheckedProduct(AModel.PatternWidth,
    AModel.PatternHeight, 'overlapping projection footprint');
  SetLength(APrepared.SourceValues,
    CheckedProduct(AModel.PaletteCount, LFootprintSize,
      'overlapping projection clause table'));
  for LPaletteIndex := 0 to AModel.PaletteCount - 1 do
    for LOffsetIndex := 0 to LFootprintSize - 1 do
    begin
      I := LPaletteIndex * LFootprintSize + LOffsetIndex;
      SetLength(APrepared.SourceValues[I], 0);
      for LPatternIndex := 0 to AModel.PatternCount - 1 do
        if AModel.PatternPaletteIndexAt(LPatternIndex,
            LOffsetIndex mod AModel.PatternWidth,
            LOffsetIndex div AModel.PatternWidth) = LPaletteIndex then
        begin
          SetLength(APrepared.SourceValues[I],
            Length(APrepared.SourceValues[I]) + 1);
          APrepared.SourceValues[I][High(APrepared.SourceValues[I])] :=
            PatternGraphValue(AModel, LPatternIndex);
        end;
      if Length(APrepared.SourceValues[I]) = 0 then
        raise EWfcPattern2DGraph.CreateFmt(
          'palette token %d has no pattern alternative at footprint offset (%d,%d)',
          [LPaletteIndex,
           LOffsetIndex mod AModel.PatternWidth,
           LOffsetIndex div AModel.PatternWidth]);
    end;
end;

procedure ValidateOverlappingProjectionFromPass2D(
  const AModel: TWfcOverlappingModel2D;
  const ATargetGraph: TGraph; const ASourcePass: String);
var
  LPrepared: TPreparedPatternProjection;
begin
  PrepareProjection(AModel, ATargetGraph, ASourcePass, LPrepared);
end;

procedure ApplyOverlappingProjectionFromPass2D(
  const AModel: TWfcOverlappingModel2D;
  const ATargetGraph: TGraph; const ASourcePass: String);
var
  I: Integer;
  LFootprintSize: Integer;
  LOffsetIndex: Integer;
  LPrepared: TPreparedPatternProjection;
  LRuleGroup: TGraphRuleGroup;
begin
  PrepareProjection(AModel, ATargetGraph, ASourcePass, LPrepared);

  { Every model/topology/dependency/conversion check has completed before the
    first public value or dependency role is added. }
  for I := 0 to AModel.PaletteCount - 1 do
    ATargetGraph.AddValue(LPrepared.PublicValues[I]);

  LFootprintSize := AModel.PatternWidth * AModel.PatternHeight;
  for I := 0 to AModel.PaletteCount - 1 do
  begin
    LRuleGroup := ATargetGraph.Rules[LPrepared.PublicValues[I]];
    for LOffsetIndex := 0 to LFootprintSize - 1 do
      LRuleGroup.RequireFromPassAt(LPrepared.SourcePass,
        MakeGraphOffset(
          -(LOffsetIndex mod AModel.PatternWidth),
          -(LOffsetIndex div AModel.PatternWidth), 0),
        LPrepared.SourceValues[I * LFootprintSize + LOffsetIndex]);
  end;
end;

procedure InitializeValidationReport(
  out AReport: TWfcOverlapping2DValidationReport);
begin
  AReport := Default(TWfcOverlapping2DValidationReport);
  AReport.Valid := False;
  AReport.Issue.Kind := woikNone;
  AReport.Issue.X := -1;
  AReport.Issue.Y := -1;
  AReport.Issue.NeighborX := -1;
  AReport.Issue.NeighborY := -1;
  AReport.Issue.PatternIndex := -1;
  AReport.Issue.RelatedPatternIndex := -1;
  AReport.Issue.PatternOffsetX := -1;
  AReport.Issue.PatternOffsetY := -1;
  AReport.Issue.ExpectedPaletteIndex := -1;
  AReport.Issue.ActualPaletteIndex := -1;
end;

function CaptureSolvedOverlappingProjectionPass2D(
  const AModel: TWfcOverlappingModel2D;
  const APatternGraph, AProjectionGraph: TGraph;
  out APatternGrid: TWfcPatternGrid2D;
  out AProjection: TWfcTokenGrid2D;
  out AReport: TWfcOverlapping2DValidationReport): Boolean;
var
  LEntry: TGraphEntry;
  LPatternGrid: TWfcPatternGrid2D;
  LProjection: TWfcTokenGrid2D;
  LToken: TWfcModelToken;
  X: Integer;
  Y: Integer;
begin
  APatternGrid := Default(TWfcPatternGrid2D);
  AProjection := Default(TWfcTokenGrid2D);
  if not Assigned(AModel) then
    raise EArgumentNilException.Create(
      'overlapping projection model cannot be nil');
  if not Assigned(APatternGraph) then
    raise EArgumentNilException.Create(
      'overlapping pattern pass cannot be nil');
  if not Assigned(AProjectionGraph) then
    raise EArgumentNilException.Create(
      'overlapping public projection pass cannot be nil');
  InitializeValidationReport(AReport);
  ValidateWrappedPlane(APatternGraph,
    'overlapping projection capture source');
  ValidateWrappedPlane(AProjectionGraph,
    'overlapping projection capture target');
  if (APatternGraph.Dimension.Width <> AProjectionGraph.Dimension.Width) or
      (APatternGraph.Dimension.Height <> AProjectionGraph.Dimension.Height) or
      (APatternGraph.Dimension.Depth <> AProjectionGraph.Dimension.Depth) then
    raise EWfcPattern2DGraph.Create(
      'overlapping projection capture pass shapes differ');
  if APatternGraph.PassGraph[0] <> AProjectionGraph.PassGraph[0] then
    raise EWfcPattern2DGraph.Create(
      'overlapping projection capture passes must share one pipeline');
  if not AppliedPatternModelMatches(AModel, APatternGraph) then
    raise EWfcPattern2DGraph.Create(
      'overlapping projection capture source model does not match');

  if not CaptureSolvedPatternGrid2D(AModel, APatternGraph, 0,
      LPatternGrid, AReport) then
  begin
    { A graph-private value may explain a malformed latent cell internally,
      but the public bridge never returns that representation. }
    AReport.Issue.Value := '';
    Exit(False);
  end;
  LProjection := Default(TWfcTokenGrid2D);
  LProjection.Width := Integer(AProjectionGraph.Dimension.Width);
  LProjection.Height := Integer(AProjectionGraph.Dimension.Height);
  SetLength(LProjection.Tokens,
    LProjection.Width * LProjection.Height);
  for Y := 0 to LProjection.Height - 1 do
    for X := 0 to LProjection.Width - 1 do
    begin
      LEntry := AProjectionGraph.Entry[X, Y, 0];
      if LEntry.Empty then
      begin
        InitializeValidationReport(AReport);
        AReport.Issue.Kind := woikEmptyGraphCell;
        AReport.Issue.X := X;
        AReport.Issue.Y := Y;
        Exit(False);
      end;
      LToken := GraphValueToModelToken(LEntry.Value);
      LProjection.Tokens[Y * LProjection.Width + X] := LToken;
    end;
  Result := ValidateOverlappingProjection2D(AModel, LPatternGrid,
    LProjection, AReport);
  if not Result then
  begin
    AReport.Issue.Value := '';
    Exit(False);
  end;
  APatternGrid := LPatternGrid;
  AProjection := LProjection;
end;

procedure HashByte(var AHash: TWfcPattern2DCompositionSignature;
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

procedure HashCardinal(var AHash: TWfcPattern2DCompositionSignature;
  const AValue: Cardinal);
begin
  HashByte(AHash, Byte(AValue and $FF));
  HashByte(AHash, Byte((AValue shr 8) and $FF));
  HashByte(AHash, Byte((AValue shr 16) and $FF));
  HashByte(AHash, Byte((AValue shr 24) and $FF));
end;

procedure HashInteger(var AHash: TWfcPattern2DCompositionSignature;
  const AValue: Integer);
begin
  HashCardinal(AHash, Cardinal(AValue));
end;

procedure HashToken(var AHash: TWfcPattern2DCompositionSignature;
  const AValue: TWfcModelToken);
var
  LCanonical: String;
  I: Integer;
begin
  { Percent encoding is canonical ASCII on native FPC and pas2js, so Unicode
    public tokens contribute identical bytes on both runtimes. }
  LCanonical := WfcTextEncodeToken(AValue,
    'pattern composition signature');
  HashCardinal(AHash, Cardinal(Length(LCanonical)));
  for I := 1 to Length(LCanonical) do
    HashByte(AHash, Byte(Ord(LCanonical[I])));
end;

function CalculateWfcPattern2DCompositionSignature(
  const AModel: TWfcOverlappingModel2D;
  const ASeed: TGraphSeed;
  const APatterns: TWfcPatternGrid2D;
  const AProjection: TWfcTokenGrid2D):
  TWfcPattern2DCompositionSignature;
var
  I: Integer;
  X: Integer;
  Y: Integer;
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create(
      'overlapping signature model cannot be nil');
  Result := Cardinal(2166136261);
  HashCardinal(Result, WFC_PATTERN_2D_COMPOSITION_SIGNATURE_VERSION);
  HashCardinal(Result, WFC_PATTERN_2D_GRAPH_ADAPTER_VERSION);
  HashCardinal(Result, ASeed);
  HashInteger(Result, APatterns.Width);
  HashInteger(Result, APatterns.Height);
  HashInteger(Result, Ord(APatterns.Boundary));
  HashInteger(Result, AModel.PatternWidth);
  HashInteger(Result, AModel.PatternHeight);
  HashInteger(Result, Ord(AModel.SourceBoundary));
  HashInteger(Result, Ord(AModel.Symmetry));
  HashInteger(Result, AModel.SourceCount);
  for I := 0 to AModel.SourceCount - 1 do
  begin
    HashInteger(Result, AModel.SourceShapeAt(I).Width);
    HashInteger(Result, AModel.SourceShapeAt(I).Height);
  end;
  HashInteger(Result, AModel.PaletteCount);
  for I := 0 to AModel.PaletteCount - 1 do
    HashToken(Result, AModel.PaletteTokenAt(I));
  HashInteger(Result, AModel.PatternCount);
  for I := 0 to AModel.PatternCount - 1 do
  begin
    HashInteger(Result, AModel.PatternWeightAt(I));
    for Y := 0 to AModel.PatternHeight - 1 do
      for X := 0 to AModel.PatternWidth - 1 do
        HashInteger(Result,
          AModel.PatternPaletteIndexAt(I, X, Y));
  end;
  HashInteger(Result, Length(APatterns.Patterns));
  for I := 0 to Length(APatterns.Patterns) - 1 do
    HashInteger(Result, APatterns.Patterns[I]);
  HashInteger(Result, AProjection.Width);
  HashInteger(Result, AProjection.Height);
  HashInteger(Result, Length(AProjection.Tokens));
  for I := 0 to Length(AProjection.Tokens) - 1 do
    HashToken(Result, AProjection.Tokens[I]);
end;

function CalculateWfcPattern2DCompositionSignature(
  const AModel: TWfcOverlappingModel2D;
  const AComposition: TWfcPattern2DComposition):
  TWfcPattern2DCompositionSignature;
begin
  if not Assigned(AComposition) then
    raise EArgumentNilException.Create(
      'overlapping composition cannot be nil');
  Result := CalculateWfcPattern2DCompositionSignature(AModel,
    AComposition.FSeed, AComposition.FPatterns,
    AComposition.FProjection);
end;

function WfcPattern2DCompositionSignatureHex(
  const ASignature: TWfcPattern2DCompositionSignature): String;
begin
  Result := IntToHex(ASignature, 8);
end;

function ClonePatternGrid(const ASource: TWfcPatternGrid2D):
  TWfcPatternGrid2D;
var
  I: Integer;
begin
  Result := Default(TWfcPatternGrid2D);
  Result.Width := ASource.Width;
  Result.Height := ASource.Height;
  Result.Boundary := ASource.Boundary;
  SetLength(Result.Patterns, Length(ASource.Patterns));
  for I := 0 to Length(ASource.Patterns) - 1 do
    Result.Patterns[I] := ASource.Patterns[I];
end;

function CloneTokenGrid(const ASource: TWfcTokenGrid2D): TWfcTokenGrid2D;
var
  I: Integer;
begin
  Result := Default(TWfcTokenGrid2D);
  Result.Width := ASource.Width;
  Result.Height := ASource.Height;
  SetLength(Result.Tokens, Length(ASource.Tokens));
  for I := 0 to Length(ASource.Tokens) - 1 do
    Result.Tokens[I] := ASource.Tokens[I];
end;

constructor TWfcPattern2DComposition.CreateInternal(
  const AModel: TWfcOverlappingModel2D; const ASeed: TGraphSeed;
  const APatterns: TWfcPatternGrid2D;
  const AProjection: TWfcTokenGrid2D);
begin
  inherited Create;
  FSeed := ASeed;
  FPatterns := ClonePatternGrid(APatterns);
  FProjection := CloneTokenGrid(AProjection);
  FSignature := CalculateWfcPattern2DCompositionSignature(AModel,
    FSeed, FPatterns, FProjection);
end;

function TWfcPattern2DComposition.GetWidth: Integer;
begin
  Result := FProjection.Width;
end;

function TWfcPattern2DComposition.GetHeight: Integer;
begin
  Result := FProjection.Height;
end;

function TWfcPattern2DComposition.CopyPatternGrid: TWfcPatternGrid2D;
begin
  Result := ClonePatternGrid(FPatterns);
end;

function TWfcPattern2DComposition.CopyProjection: TWfcTokenGrid2D;
begin
  Result := CloneTokenGrid(FProjection);
end;

constructor TWfcPattern2DPassCommitGraph.CreatePass(const ARoot: TGraph;
  const APassIndex: Integer);
begin
  inherited CreatePass(ARoot, APassIndex);
  if not (ARoot is TWfcPattern2DPassCommitGraph) then
    raise EInvalidOperation.Create(
      'pattern pass root has an incompatible graph class');
  FOwner := TWfcPattern2DPassCommitGraph(ARoot).Owner;
end;

function TWfcPattern2DPassCommitGraph.DoValidateCommit(
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

function DefaultWfcPattern2DPassConfig(
  const AModel: TWfcOverlappingModel2D;
  const AWidth, AHeight: Integer;
  const ASeed: TGraphSeed): TWfcPattern2DPassConfig;
begin
  Result := Default(TWfcPattern2DPassConfig);
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.Seed := ASeed;
  Result.Model := AModel;
end;

constructor TWfcPattern2DPassPipeline.Create(
  const AConfig: TWfcPattern2DPassConfig);
begin
  inherited Create;
  Initialize(AConfig);
end;

procedure TWfcPattern2DPassPipeline.Initialize(
  const AConfig: TWfcPattern2DPassConfig);
begin
  if not Assigned(AConfig.Model) then
    raise EArgumentNilException.Create(
      'pattern pass model cannot be nil');
  if (AConfig.Width < 1) or (AConfig.Height < 1) then
    raise ERangeError.CreateFmt(
      'pattern pass dimensions must be positive [%d x %d]',
      [AConfig.Width, AConfig.Height]);
  CheckedProduct(AConfig.Width, AConfig.Height,
    'pattern pass dimensions');

  FModel := AConfig.Model;
  FWidth := AConfig.Width;
  FHeight := AConfig.Height;
  FCommittedSeed := AConfig.Seed;
  FHasCommit := False;
  FGraph := nil;
  FPendingComposition := nil;
  InitializeValidationReport(FPendingValidation);
  FPendingStatus := wpppsNotRun;
  FPendingFailedLayer := wpplPatterns;
  try
    FGraph := TWfcPattern2DPassCommitGraph.Create;
    TWfcPattern2DPassCommitGraph(FGraph).Owner := Self;
    FGraph.Reshape(FWidth, FHeight, 1);
    FGraph.WrapNeighbors := True;
    FGraph.Seed := AConfig.Seed;

    FGraph.CurrentPass := WFC_PATTERN_2D_PASS_PATTERNS;
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies;
    ApplyOverlappingModel2DToGraph(FModel, FGraph);

    FGraph.SwitchToPass(WFC_PATTERN_2D_PASS_PROJECTION);
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies;
    ApplyOverlappingProjectionFromPass2D(FModel, FGraph,
      WFC_PATTERN_2D_PASS_PATTERNS);
    FGraph.SwitchToPass(WFC_PATTERN_2D_PASS_PATTERNS);
  except
    FGraph.Free;
    FGraph := nil;
    raise;
  end;
end;

destructor TWfcPattern2DPassPipeline.Destroy;
begin
  ClearPendingCommit;
  FGraph.Free;
  inherited Destroy;
end;

procedure TWfcPattern2DPassPipeline.ClearPendingCommit;
begin
  FPendingComposition.Free;
  FPendingComposition := nil;
  InitializeValidationReport(FPendingValidation);
  FPendingStatus := wpppsNotRun;
  FPendingFailedLayer := wpplPatterns;
end;

function TWfcPattern2DPassPipeline.GetSeed: TGraphSeed;
begin
  Result := FGraph.Seed;
end;

procedure TWfcPattern2DPassPipeline.SetSeed(const AValue: TGraphSeed);
begin
  FGraph.Seed := AValue;
end;

function TWfcPattern2DPassPipeline.DoValidateProjection(
  const APatterns: TWfcPatternGrid2D;
  const AProjection: TWfcTokenGrid2D;
  out AReport: TWfcOverlapping2DValidationReport): Boolean;
begin
  Result := ValidateOverlappingProjection2D(FModel, APatterns,
    AProjection, AReport);
end;

function TWfcPattern2DPassPipeline.CaptureComposition(
  const ASeed: TGraphSeed;
  out AComposition: TWfcPattern2DComposition;
  out AValidation: TWfcOverlapping2DValidationReport;
  out AFailedLayer: TWfcPattern2DPassLayer;
  out AStatus: TWfcPattern2DPassStatus): Boolean;
var
  LPatterns: TWfcPatternGrid2D;
  LProjection: TWfcTokenGrid2D;
begin
  AComposition := nil;
  AFailedLayer := wpplPatterns;
  AStatus := wpppsCaptureFailed;
  InitializeValidationReport(AValidation);
  if not CaptureSolvedOverlappingProjectionPass2D(FModel,
      FGraph.PassGraph[Ord(wpplPatterns)],
      FGraph.PassGraph[Ord(wpplProjection)], LPatterns,
      LProjection, AValidation) then
  begin
    if AValidation.Issue.Kind in
        [woikProjectionShape, woikProjectionToken] then
      AFailedLayer := wpplProjection;
    Exit(False);
  end;

  AFailedLayer := wpplProjection;
  AStatus := wpppsValidationFailed;
  if not DoValidateProjection(LPatterns, LProjection,
      AValidation) then
    Exit(False);
  try
    AComposition := TWfcPattern2DComposition.CreateInternal(FModel,
      ASeed, LPatterns, LProjection);
  except
    AComposition.Free;
    AComposition := nil;
    raise;
  end;
  AStatus := wpppsCompleted;
  Result := True;
end;

function TWfcPattern2DPassPipeline.ValidatePendingCommit(
  out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
var
  LFailedX: Integer;
  LFailedY: Integer;

  function AddWrappedNonNegative(const ACoordinate, AOffset,
    ASize: Integer): Integer;
  var
    LOffset: Integer;
  begin
    LOffset := AOffset mod ASize;
    if LOffset >= ASize - ACoordinate then
      Result := LOffset - (ASize - ACoordinate)
    else
      Result := ACoordinate + LOffset;
  end;
begin
  ClearPendingCommit;
  Result := CaptureComposition(FGraph.Seed, FPendingComposition,
    FPendingValidation, FPendingFailedLayer, FPendingStatus);
  if Result then
  begin
    AFailedPassIndex := -1;
    AFailedEntryIndex := -1;
    Exit;
  end;
  AFailedPassIndex := Ord(FPendingFailedLayer);
  if (FPendingValidation.Issue.X >= 0) and
      (FPendingValidation.Issue.Y >= 0) and
      (FPendingValidation.Issue.X < FWidth) and
      (FPendingValidation.Issue.Y < FHeight) then
  begin
    LFailedX := FPendingValidation.Issue.X;
    LFailedY := FPendingValidation.Issue.Y;
    if (FPendingFailedLayer = wpplProjection) and
        (FPendingValidation.Issue.Kind = woikProjectionToken) and
        (FPendingValidation.Issue.PatternOffsetX >= 0) and
        (FPendingValidation.Issue.PatternOffsetY >= 0) then
    begin
      { Projection-token reports identify the contributing latent anchor.
        The failed entry belongs to the public pass at anchor + footprint
        offset, wrapped to the shared v1 plane. A custom validator that leaves
        the offsets negative continues to report X,Y directly. }
      LFailedX := AddWrappedNonNegative(LFailedX,
        FPendingValidation.Issue.PatternOffsetX, FWidth);
      LFailedY := AddWrappedNonNegative(LFailedY,
        FPendingValidation.Issue.PatternOffsetY, FHeight);
    end;
    AFailedEntryIndex := LFailedY * FWidth + LFailedX;
  end
  else
    AFailedEntryIndex := -1;
end;

function TWfcPattern2DPassPipeline.TakePendingComposition(
  out AComposition: TWfcPattern2DComposition;
  out AValidation: TWfcOverlapping2DValidationReport;
  out AFailedLayer: TWfcPattern2DPassLayer;
  out AStatus: TWfcPattern2DPassStatus): Boolean;
begin
  AComposition := FPendingComposition;
  FPendingComposition := nil;
  AValidation := FPendingValidation;
  AFailedLayer := FPendingFailedLayer;
  AStatus := FPendingStatus;
  Result := Assigned(AComposition) and
    (AStatus = wpppsCompleted) and AValidation.Valid;
  InitializeValidationReport(FPendingValidation);
  FPendingStatus := wpppsNotRun;
  FPendingFailedLayer := wpplPatterns;
end;

function TWfcPattern2DPassPipeline.TryGenerate(
  const AOptions: TGraphSolveOptions;
  out AComposition: TWfcPattern2DComposition;
  out AReport: TWfcPattern2DPassReport): Boolean;
var
  LSolved: Boolean;
begin
  AComposition := nil;
  AReport := Default(TWfcPattern2DPassReport);
  AReport.Status := wpppsNotRun;
  AReport.FailedLayer := wpplPatterns;
  InitializeValidationReport(AReport.Validation);
  ClearPendingCommit;
  try
    LSolved := FGraph.TrySolve(AOptions, AReport.Solve);
  except
    ClearPendingCommit;
    raise;
  end;
  if not LSolved then
  begin
    AReport.Status := wpppsSolveFailed;
    if (AReport.Solve.FailedPassIndex >= Ord(Low(TWfcPattern2DPassLayer))) and
        (AReport.Solve.FailedPassIndex <= Ord(High(TWfcPattern2DPassLayer))) then
      AReport.FailedLayer :=
        TWfcPattern2DPassLayer(AReport.Solve.FailedPassIndex);
    if AReport.Solve.Contradiction.Kind = gckFinalValidation then
    begin
      AReport.Status := FPendingStatus;
      AReport.FailedLayer := FPendingFailedLayer;
      AReport.Validation := FPendingValidation;
    end;
    ClearPendingCommit;
    Exit(False);
  end;
  Result := TakePendingComposition(AComposition,
    AReport.Validation, AReport.FailedLayer, AReport.Status);
  if Result then
  begin
    FCommittedSeed := AComposition.Seed;
    FHasCommit := True;
  end
  else
  begin
    AComposition.Free;
    AComposition := nil;
  end;
end;

function TWfcPattern2DPassPipeline.TryGenerate(
  out AComposition: TWfcPattern2DComposition;
  out AReport: TWfcPattern2DPassReport): Boolean;
var
  LOptions: TGraphSolveOptions;
begin
  LOptions := DefaultGraphSolveOptions;
  Result := TryGenerate(LOptions, AComposition, AReport);
end;

function TWfcPattern2DPassPipeline.TryCopyCommitted(
  out AComposition: TWfcPattern2DComposition;
  out AValidation: TWfcOverlapping2DValidationReport): Boolean;
var
  LFailedLayer: TWfcPattern2DPassLayer;
  LStatus: TWfcPattern2DPassStatus;
begin
  AComposition := nil;
  InitializeValidationReport(AValidation);
  if not FHasCommit then
    Exit(False);
  Result := CaptureComposition(FCommittedSeed, AComposition,
    AValidation, LFailedLayer, LStatus);
end;

function TWfcPattern2DPassPipeline.Validate(
  const AComposition: TWfcPattern2DComposition;
  out AReport: TWfcOverlapping2DValidationReport): Boolean;
begin
  InitializeValidationReport(AReport);
  if not Assigned(AComposition) then
    raise EArgumentNilException.Create(
      'pattern pass composition cannot be nil');
  Result := DoValidateProjection(AComposition.FPatterns,
    AComposition.FProjection, AReport);
  if Result and
      (AComposition.Signature <>
        CalculateWfcPattern2DCompositionSignature(FModel,
          AComposition)) then
  begin
    AReport.Valid := False;
    AReport.Issue.Kind := woikProjectionToken;
    AReport.Issue.X := -1;
    AReport.Issue.Y := -1;
    Result := False;
  end;
end;

end.
