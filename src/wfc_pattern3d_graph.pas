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
unit wfc_pattern3d_graph;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  wfc,
  wfc_model,
  wfc_pattern3d;

const
  { Identifies the conversion from a wrapped latent pattern pass to one
    same-sized public palette pass. }
  WFC_PATTERN_3D_GRAPH_ADAPTER_VERSION = 1;
  WFC_PATTERN_3D_PASS_PIPELINE_VERSION = 1;
  WFC_PATTERN_3D_COMPOSITION_SIGNATURE_VERSION = 1;

  WFC_PATTERN_3D_PASS_PATTERNS = 'patterns';
  WFC_PATTERN_3D_PASS_PROJECTION = 'projection';

type
  EWfcPattern3DGraph = class(EWfcOverlapping3D);

  TWfcPattern3DPassLayer = (
    wp3lPatterns,
    wp3lProjection
  );

  TWfcPattern3DPassStatus = (
    wp3psNotRun,
    wp3psCompleted,
    wp3psSolveFailed,
    wp3psCaptureFailed,
    wp3psValidationFailed
  );

  TWfcPattern3DCompositionSignature = Cardinal;

  TWfcPattern3DPassConfig = record
    Width: Integer;
    Height: Integer;
    Depth: Integer;
    Seed: TGraphSeed;
    Model: TWfcOverlappingModel3D;
  end;

  TWfcPattern3DPassReport = record
    Status: TWfcPattern3DPassStatus;
    FailedLayer: TWfcPattern3DPassLayer;
    Solve: TGraphSolveReport;
    Validation: TWfcOverlapping3DValidationReport;
  end;

  { Immutable public result. Pattern indices are stable model identities; the
    graph-private payload keys never leave the adapter. Copy methods detach all
    managed arrays from the result. }
  TWfcPattern3DComposition = class
  private
    FSeed: TGraphSeed;
    FPatterns: TWfcPatternGrid3D;
    FProjection: TWfcTokenGrid3D;
    FSignature: TWfcPattern3DCompositionSignature;
    constructor CreateInternal(const AModel: TWfcOverlappingModel3D;
      const ASeed: TGraphSeed; const APatterns: TWfcPatternGrid3D;
      const AProjection: TWfcTokenGrid3D);
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetDepth: Integer;
  public
    function CopyPatternGrid: TWfcPatternGrid3D;
    function CopyProjection: TWfcTokenGrid3D;

    property Seed: TGraphSeed read FSeed;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Depth: Integer read GetDepth;
    property Signature: TWfcPattern3DCompositionSignature read FSignature;
  end;

  { Ready-to-use patterns -> projection owner. The model remains caller-owned
    and must outlive the pipeline. Its graph is intentionally not exposed;
    applications that need a larger DAG use the free bridge below. }
  TWfcPattern3DPassPipeline = class
  strict private
    FGraph: TGraph;
    FModel: TWfcOverlappingModel3D;
    FWidth: Integer;
    FHeight: Integer;
    FDepth: Integer;
    FCellCount: Integer;
    FPublicDomains: array of TWfcModelIntegerArray;
    FPublicDomainAssigned: array of Byte;
    FPublicDomainsDirty: Boolean;
    FCommittedSeed: TGraphSeed;
    FHasCommit: Boolean;
    FPendingComposition: TWfcPattern3DComposition;
    FPendingValidation: TWfcOverlapping3DValidationReport;
    FPendingStatus: TWfcPattern3DPassStatus;
    FPendingFailedLayer: TWfcPattern3DPassLayer;

    procedure Initialize(const AConfig: TWfcPattern3DPassConfig);
    procedure ClearPendingCommit;
    function GetSeed: TGraphSeed;
    procedure SetSeed(const AValue: TGraphSeed);
    function PublicCellIndex(const AX, AY, AZ: Integer): Integer;
    procedure InvalidateComposition;
    procedure PreparePublicDomains;
    function ValidatePublicDomains(const AProjection: TWfcTokenGrid3D;
      out AReport: TWfcOverlapping3DValidationReport): Boolean;
    function CaptureComposition(const ASeed: TGraphSeed;
      out AComposition: TWfcPattern3DComposition;
      out AValidation: TWfcOverlapping3DValidationReport;
      out AFailedLayer: TWfcPattern3DPassLayer;
      out AStatus: TWfcPattern3DPassStatus): Boolean;
    function TakePendingComposition(
      out AComposition: TWfcPattern3DComposition;
      out AValidation: TWfcOverlapping3DValidationReport;
      out AFailedLayer: TWfcPattern3DPassLayer;
      out AStatus: TWfcPattern3DPassStatus): Boolean;
  private
    function ValidatePendingCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean;
  strict protected
    { Extension hook for a domain owner that needs semantics in addition to
      exact pattern projection. Returning False rejects the graph transaction
      while its entry and random-stream snapshots are still live. }
    function DoValidateProjection(const APatterns: TWfcPatternGrid3D;
      const AProjection: TWfcTokenGrid3D;
      out AReport: TWfcOverlapping3DValidationReport): Boolean; virtual;
  public
    constructor Create(const AConfig: TWfcPattern3DPassConfig);
    destructor Destroy; override;

    function TryGenerate(const AOptions: TGraphSolveOptions;
      out AComposition: TWfcPattern3DComposition;
      out AReport: TWfcPattern3DPassReport): Boolean; overload;
    function TryGenerate(out AComposition: TWfcPattern3DComposition;
      out AReport: TWfcPattern3DPassReport): Boolean; overload;
    function TryCopyCommitted(out AComposition: TWfcPattern3DComposition;
      out AValidation: TWfcOverlapping3DValidationReport): Boolean;
    function Validate(const AComposition: TWfcPattern3DComposition;
      out AReport: TWfcOverlapping3DValidationReport): Boolean;

    { Public palette constraints are projected back into EVERY covering
      latent anchor before solving. An assigned empty set means contradiction;
      clearing a domain is a different operation. Copies are detached. }
    procedure SetPublicDomain(const AX, AY, AZ: Integer;
      const AValues: TWfcModelTokens);
    procedure LockPublicCell(const AX, AY, AZ: Integer;
      const AValue: TWfcModelToken);
    procedure ClearPublicDomain(const AX, AY, AZ: Integer);
    procedure ClearPublicDomains;
    function HasPublicDomain(const AX, AY, AZ: Integer): Boolean;
    function CopyPublicDomain(const AX, AY, AZ: Integer): TWfcModelTokens;

    property Model: TWfcOverlappingModel3D read FModel;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Depth: Integer read FDepth;
    property Seed: TGraphSeed read GetSeed write SetSeed;
  end;

function DefaultWfcPattern3DPassConfig(
  const AModel: TWfcOverlappingModel3D;
  const AWidth, AHeight, ADepth: Integer;
  const ASeed: TGraphSeed): TWfcPattern3DPassConfig;

{ Public bridge preflight shared by declarative recipes and graph adapters. }
function WfcPattern3DTokenUsesReservedKeySyntax(
  const AToken: TWfcModelToken): Boolean;

{ Preflights the complete bridge without changing the target pass. The active
  target must be an empty overlay pass in the same wrapped XYZ graph as the
  named source. Exact keys bind footprint and actual payload tokens; ordered
  keys, weights, rules and denials bind local graph semantics. Source provenance
  metadata is not an additional executable graph constraint. }
procedure ValidateOverlappingProjectionFromPass3D(
  const AModel: TWfcOverlappingModel3D;
  const ATargetGraph: TGraph; const ASourcePass: String);

{ Materializes the model's public palette in the active target pass. For each
  public token and footprint coordinate (PX,PY,PZ), one signed-offset clause
  requires a matching latent pattern at (X-PX,Y-PY,Z-PZ). Distinct offsets are AND
  clauses; pattern alternatives within one offset are OR choices. }
procedure ApplyOverlappingProjectionFromPass3D(
  const AModel: TWfcOverlappingModel3D;
  const ATargetGraph: TGraph; const ASourcePass: String);

{ Captures exact pass objects without changing root pass selection and checks
  every latent overlap and every footprint contribution independently. }
function CaptureSolvedOverlappingProjectionPass3D(
  const AModel: TWfcOverlappingModel3D;
  const APatternGraph, AProjectionGraph: TGraph;
  out APatternGrid: TWfcPatternGrid3D;
  out AProjection: TWfcTokenGrid3D;
  out AReport: TWfcOverlapping3DValidationReport): Boolean;

function CalculateWfcPattern3DCompositionSignature(
  const AModel: TWfcOverlappingModel3D;
  const ASeed: TGraphSeed;
  const APatterns: TWfcPatternGrid3D;
  const AProjection: TWfcTokenGrid3D):
  TWfcPattern3DCompositionSignature; overload;
function CalculateWfcPattern3DCompositionSignature(
  const AModel: TWfcOverlappingModel3D;
  const AComposition: TWfcPattern3DComposition):
  TWfcPattern3DCompositionSignature; overload;
function WfcPattern3DCompositionSignatureHex(
  const ASignature: TWfcPattern3DCompositionSignature): String;

implementation

uses
  wfc_text_codec, wfc_lattice;

type
  TPatternByteArray = array of Byte;
  TPatternIntegerArray = array of Integer;
  TPatternGraphValueArrays = array of TGraphValues;

  TPreparedPatternProjection = record
    SourcePass: String;
    PublicValues: TGraphValues;
    { -1 marks a token impossible in a wrapped projection. Only remaining
      rows allocate footprint clauses, bounded by pattern-count * footprint. }
    SourceRows: TPatternIntegerArray;
    { Flattened [eligible row * footprint + offset]. }
    SourceValues: TPatternGraphValueArrays;
  end;

  { The core invokes this after all staged values have been copied to entries,
    while the transaction can still restore entries and random streams. }
  TWfcPattern3DPassCommitGraph = class(TGraph)
  private
    FOwner: TWfcPattern3DPassPipeline;
  strict protected
    function DoValidateCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean; override;
  public
    constructor CreatePass(const ARoot: TGraph;
      const APassIndex: Integer); override;
    property Owner: TWfcPattern3DPassPipeline read FOwner write FOwner;
  end;

procedure RequireInteger(const AValue: Integer; const AMinimum,
  AMaximum: Integer; const ALabel: String);
{$IFDEF PAS2JS}var LValid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    LValid = typeof AValue === 'number' && Number.isFinite(AValue) &&
      Number.isInteger(AValue);
  end;
  if not LValid then
    raise EWfcPattern3DGraph.Create(ALabel + ' must be an exact finite integer');
  {$ENDIF}
  if (AValue < AMinimum) or (AValue > AMaximum) then
    raise EWfcPattern3DGraph.Create(ALabel + ' is outside its supported range');
end;

procedure RequireCardinal(const AValue: Cardinal; const ALabel: String);
{$IFDEF PAS2JS}var LValid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    LValid = typeof AValue === 'number' && Number.isFinite(AValue) &&
      Number.isInteger(AValue) && AValue >= 0 && AValue <= 4294967295;
  end;
  if not LValid then
    raise EWfcPattern3DGraph.Create(ALabel + ' must be an exact unsigned 32-bit integer');
  {$ENDIF}
end;

function CheckedProduct(const A, B: Integer;
  const ALabel: String): Integer;
begin
  RequireInteger(A, 0, High(Integer), ALabel);
  RequireInteger(B, 0, High(Integer), ALabel);
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

function WfcPattern3DTokenUsesReservedKeySyntax(
  const AToken: TWfcModelToken): Boolean;
begin
  Result := Copy(AToken, 1, 5) = '@p3v1';
end;

function PatternGraphValue(const AModel: TWfcOverlappingModel3D;
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
    gdUp:
      AModelDirection := wmdDown;
    gdDown:
      AModelDirection := wmdUp;
  else
    Result := False;
  end;
end;

function PatternRuleGroupMatches(
  const AModel: TWfcOverlappingModel3D;
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
  const AModel: TWfcOverlappingModel3D;
  const AGraph: TGraph): Boolean;
var
  I: Integer;
  LActivePass: TGraph;
  LExpectedValues: TGraphValues;
  LGroup: TGraphRuleGroup;
  LParented: TGraph.TParentedGraphRuleGroup;
  LValue: TGraphValue;
begin
  { Canonical keys bind actual public payloads and footprint dimensions.
    Source provenance is not executable graph state. Read-only comparison is
    also valid inside the graph's still-live commit transaction. }
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

procedure RequireAssigned(const AModel: TWfcOverlappingModel3D;
  const AGraph: TGraph);
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create(
      'overlapping projection model cannot be nil');
  if not Assigned(AGraph) then
    raise EArgumentNilException.Create(
      'overlapping projection graph cannot be nil');
end;

procedure RequireIdenticalBridgeLayout(const ATargetGraph,
  ASourceGraph: TGraph; const AOperation: String);
var Target, Source: TWfcLatticeLayout;
begin
  Target := ATargetGraph.PassLayout; Source := ASourceGraph.PassLayout;
  { Compare identity directly: legacy adapters can be prepared before Reshape,
    when both layouts are empty. Do not validate or normalize either record. }
  if (Target.Cells.X <> Source.Cells.X) or
    (Target.Cells.Y <> Source.Cells.Y) or
    (Target.Cells.Z <> Source.Cells.Z) or
    (Target.Origin.X <> Source.Origin.X) or
    (Target.Origin.Y <> Source.Origin.Y) or
    (Target.Origin.Z <> Source.Origin.Z) or
    (Target.Pitch.X <> Source.Pitch.X) or
    (Target.Pitch.Y <> Source.Pitch.Y) or
    (Target.Pitch.Z <> Source.Pitch.Z) or
    (Target.Wrap <> Source.Wrap) then
    raise EWfcPattern3DGraph.Create(AOperation + ' requires identical pass layouts');
end;

procedure ValidateWrappedVolume(const AGraph: TGraph;
  const AOperation: String);
var LPlane: Integer;
begin
  {$IFDEF PAS2JS}
  RequireInteger(AGraph.Dimension.Width, 1, High(Integer), AOperation + ' width');
  RequireInteger(AGraph.Dimension.Height, 1, High(Integer), AOperation + ' height');
  RequireInteger(AGraph.Dimension.Depth, 1, High(Integer), AOperation + ' depth');
  {$ENDIF}
  if (AGraph.Dimension.Width = 0) or
      (AGraph.Dimension.Height = 0) or
      (AGraph.Dimension.Depth = 0) or
      (AGraph.Dimension.Width > TGraphCoordinate(High(Integer))) or
      (AGraph.Dimension.Height > TGraphCoordinate(High(Integer))) or
      (AGraph.Dimension.Depth > TGraphCoordinate(High(Integer))) then
    raise EWfcPattern3DGraph.Create(AOperation +
      ' requires a positive XYZ graph');
  LPlane := CheckedProduct(Integer(AGraph.Dimension.Width),
    Integer(AGraph.Dimension.Height), AOperation + ' plane');
  CheckedProduct(LPlane, Integer(AGraph.Dimension.Depth), AOperation + ' volume');
  if not AGraph.WrapNeighbors then
    raise EWfcPattern3DGraph.Create(AOperation +
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
  raise EWfcPattern3DGraph.CreateFmt(
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
    raise EWfcPattern3DGraph.Create(
      'overlapping projection has an invalid pass index');
  if LSourceIndex = LTargetIndex then
    raise EWfcPattern3DGraph.Create(
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
      raise EWfcPattern3DGraph.CreateFmt(
        'overlapping projection dependency on pass "%s" would create a cycle',
        [ASourcePass]);
    LNodeGraph := ATargetGraph.PassGraph[LNodeIndex];
    for I := 0 to LNodeGraph.DependencyCount - 1 do
    begin
      LDependencyIndex := LNodeGraph.DependencyIndex[I];
      if (LDependencyIndex < 0) or
          (LDependencyIndex >= LPassCount) then
        raise EWfcPattern3DGraph.Create(
          'overlapping projection found a malformed dependency graph');
      if LSeen[LDependencyIndex] = 0 then
      begin
        if LStackCount >= Length(LStack) then
          raise EWfcPattern3DGraph.Create(
            'overlapping projection found a malformed dependency graph');
        LStack[LStackCount] := LDependencyIndex;
        Inc(LStackCount);
        LSeen[LDependencyIndex] := 1;
      end;
    end;
    LSeen[LNodeIndex] := 2;
  end;
end;

procedure PrepareProjection(const AModel: TWfcOverlappingModel3D;
  const ATargetGraph: TGraph; const ASourcePass: String;
  out APrepared: TPreparedPatternProjection);
var
  I: Integer;
  J: Integer;
  LFootprintSize: Integer;
  LPlane: Integer;
  LRowCount: Integer;
  LOffsetIndex: Integer;
  LPaletteIndex: Integer;
  LPatternIndex: Integer;
  LPossible: TPatternByteArray;
  LSeen: TPatternIntegerArray;
  LCounts: TPatternIntegerArray;
  LKeys: TGraphValues;
  LSourceGraph: TGraph;
  LTargetGraph: TGraph;
begin
  APrepared := Default(TPreparedPatternProjection);
  RequireAssigned(AModel, ATargetGraph);
  if ATargetGraph.Running then
    raise EWfcPattern3DGraph.Create(
      'overlapping projection cannot change while the pipeline is running');
  LTargetGraph := ATargetGraph.PassGraph[
    ATargetGraph.CurrentPassIndex];
  ValidateWrappedVolume(LTargetGraph, 'overlapping projection');
  if LTargetGraph.PassMode <> gpmOverlay then
    raise EWfcPattern3DGraph.Create(
      'overlapping projection target must be an overlay pass');
  if LTargetGraph.HasDefinition or
      (LTargetGraph.RuleGroups.Count <> 0) or
      (Length(LTargetGraph.CopyRegisteredValues) <> 0) then
    raise EWfcPattern3DGraph.Create(
      'overlapping projection target pass must be empty');

  LSourceGraph := FindPassGraph(ATargetGraph, ASourcePass,
    'overlapping projection');
  ValidateWrappedVolume(LSourceGraph, 'overlapping projection source');
  RequireIdenticalBridgeLayout(LTargetGraph, LSourceGraph,
    'overlapping projection');
  if not AppliedPatternModelMatches(AModel, LSourceGraph) then
    raise EWfcPattern3DGraph.Create(
      'overlapping projection source does not contain the matching compiled latent model');
  ValidateProjectionDependencyEdge(ATargetGraph, LSourceGraph,
    ASourcePass);

  APrepared.SourcePass := ASourcePass;
  SetLength(APrepared.PublicValues, AModel.PaletteCount);
  for I := 0 to AModel.PaletteCount - 1 do
  begin
    if WfcPattern3DTokenUsesReservedKeySyntax(
        AModel.PaletteTokenAt(I)) then
      raise EWfcPattern3DGraph.CreateFmt(
        'overlapping palette token %d uses the reserved latent-key syntax',
        [I]);
    APrepared.PublicValues[I] := ModelTokenToGraphValue(
      AModel.PaletteTokenAt(I));
    if (Length(APrepared.PublicValues[I]) = 0) or
        (GraphValueToModelToken(APrepared.PublicValues[I]) <>
          AModel.PaletteTokenAt(I)) then
      raise EWfcPattern3DGraph.CreateFmt(
        'overlapping palette token cannot be represented by graph strings [%d]',
        [I]);
    for J := 0 to I - 1 do
      if APrepared.PublicValues[I] = APrepared.PublicValues[J] then
        raise EWfcPattern3DGraph.CreateFmt(
          'overlapping palette conversion is not unique [%d, %d]', [J, I]);
  end;

  LPlane := CheckedProduct(AModel.PatternWidth, AModel.PatternHeight,
    'overlapping projection footprint plane');
  LFootprintSize := CheckedProduct(LPlane, AModel.PatternDepth,
    'overlapping projection footprint');
  SetLength(LPossible, AModel.PaletteCount);
  SetLength(LSeen, AModel.PaletteCount);
  for I := 0 to High(LPossible) do LPossible[I] := 1;
  //A wrapped public cell is covered at every footprint offset. Tokens absent
  //at one offset are impossible outputs, not malformed source models.
  for LOffsetIndex := 0 to LFootprintSize - 1 do
  begin
    for LPatternIndex := 0 to AModel.PatternCount - 1 do
    begin
      I := AModel.PatternPaletteIndexAt(LPatternIndex,
        LOffsetIndex mod AModel.PatternWidth,
        (LOffsetIndex div AModel.PatternWidth) mod AModel.PatternHeight,
        LOffsetIndex div LPlane);
      LSeen[I] := LOffsetIndex + 1;
    end;
    for I := 0 to High(LPossible) do
      if LSeen[I] <> LOffsetIndex + 1 then LPossible[I] := 0;
  end;
  SetLength(APrepared.SourceRows, AModel.PaletteCount);
  LRowCount := 0;
  for I := 0 to AModel.PaletteCount - 1 do
  begin
    APrepared.SourceRows[I] := -1;
    if LPossible[I] <> 0 then
    begin
      APrepared.SourceRows[I] := LRowCount;
      Inc(LRowCount);
    end;
  end;
  SetLength(APrepared.SourceValues, CheckedProduct(LRowCount,
    LFootprintSize, 'overlapping projection clause table'));
  SetLength(LCounts, Length(APrepared.SourceValues));
  //Count and fill by pattern, not palette * footprint * pattern scans. Every
  //stored alternative corresponds to one original payload contribution.
  for LOffsetIndex := 0 to LFootprintSize - 1 do
    for LPatternIndex := 0 to AModel.PatternCount - 1 do
    begin
      LPaletteIndex := AModel.PatternPaletteIndexAt(LPatternIndex,
        LOffsetIndex mod AModel.PatternWidth,
        (LOffsetIndex div AModel.PatternWidth) mod AModel.PatternHeight,
        LOffsetIndex div LPlane);
      J := APrepared.SourceRows[LPaletteIndex];
      if J >= 0 then
      begin
        I := J * LFootprintSize + LOffsetIndex;
        LCounts[I] := LCounts[I] + 1;
      end;
    end;
  for I := 0 to High(LCounts) do
  begin
    SetLength(APrepared.SourceValues[I], LCounts[I]);
    LCounts[I] := 0;
  end;
  SetLength(LKeys, AModel.PatternCount);
  for I := 0 to High(LKeys) do LKeys[I] := PatternGraphValue(AModel, I);
  for LOffsetIndex := 0 to LFootprintSize - 1 do
    for LPatternIndex := 0 to AModel.PatternCount - 1 do
    begin
      LPaletteIndex := AModel.PatternPaletteIndexAt(LPatternIndex,
        LOffsetIndex mod AModel.PatternWidth,
        (LOffsetIndex div AModel.PatternWidth) mod AModel.PatternHeight,
        LOffsetIndex div LPlane);
      J := APrepared.SourceRows[LPaletteIndex];
      if J >= 0 then
      begin
        I := J * LFootprintSize + LOffsetIndex;
        APrepared.SourceValues[I][LCounts[I]] := LKeys[LPatternIndex];
        LCounts[I] := LCounts[I] + 1;
      end;
    end;
end;

procedure ValidateOverlappingProjectionFromPass3D(
  const AModel: TWfcOverlappingModel3D;
  const ATargetGraph: TGraph; const ASourcePass: String);
var
  LPrepared: TPreparedPatternProjection;
begin
  PrepareProjection(AModel, ATargetGraph, ASourcePass, LPrepared);
end;

procedure ApplyOverlappingProjectionFromPass3D(
  const AModel: TWfcOverlappingModel3D;
  const ATargetGraph: TGraph; const ASourcePass: String);
var
  I: Integer;
  LFootprintSize: Integer;
  LPlane: Integer;
  LOffsetIndex: Integer;
  LPrepared: TPreparedPatternProjection;
  LRuleGroup: TGraphRuleGroup;
begin
  PrepareProjection(AModel, ATargetGraph, ASourcePass, LPrepared);

  { Every model/topology/dependency/conversion check has completed before the
    first public value or dependency role is added. }
  for I := 0 to AModel.PaletteCount - 1 do
    ATargetGraph.AddValue(LPrepared.PublicValues[I]);
  ATargetGraph.DependsOn(LPrepared.SourcePass);

  LPlane := AModel.PatternWidth * AModel.PatternHeight;
  LFootprintSize := LPlane * AModel.PatternDepth;
  for I := 0 to AModel.PaletteCount - 1 do
  begin
    LRuleGroup := ATargetGraph.Rules[LPrepared.PublicValues[I]];
    if LPrepared.SourceRows[I] < 0 then
    begin
      //Every wrapped cell has a north neighbor, including singleton axes.
      //A deny-all row excludes this impossible public value without rejecting
      //a model that may still have other, perfectly valid public outputs.
      LRuleGroup.DenyAll([gdNorth]);
      Continue;
    end;
    for LOffsetIndex := 0 to LFootprintSize - 1 do
      LRuleGroup.RequireFromPassAt(LPrepared.SourcePass,
        MakeGraphOffset(
          -(LOffsetIndex mod AModel.PatternWidth),
          -((LOffsetIndex div AModel.PatternWidth) mod AModel.PatternHeight),
          -(LOffsetIndex div LPlane)),
        LPrepared.SourceValues[LPrepared.SourceRows[I] * LFootprintSize + LOffsetIndex]);
  end;
end;

procedure InitializeValidationReport(
  out AReport: TWfcOverlapping3DValidationReport);
begin
  AReport := Default(TWfcOverlapping3DValidationReport);
  AReport.Valid := False;
  AReport.Issue.Kind := wo3ikNone;
  AReport.Issue.X := -1;
  AReport.Issue.Y := -1;
  AReport.Issue.Z := -1;
  AReport.Issue.NeighborX := -1;
  AReport.Issue.NeighborY := -1;
  AReport.Issue.NeighborZ := -1;
  AReport.Issue.PatternIndex := -1;
  AReport.Issue.RelatedPatternIndex := -1;
  AReport.Issue.PatternOffsetX := -1;
  AReport.Issue.PatternOffsetY := -1;
  AReport.Issue.PatternOffsetZ := -1;
  AReport.Issue.ExpectedPaletteIndex := -1;
  AReport.Issue.ActualPaletteIndex := -1;
end;

function CaptureSolvedOverlappingProjectionPass3D(
  const AModel: TWfcOverlappingModel3D;
  const APatternGraph, AProjectionGraph: TGraph;
  out APatternGrid: TWfcPatternGrid3D;
  out AProjection: TWfcTokenGrid3D;
  out AReport: TWfcOverlapping3DValidationReport): Boolean;
var
  LEntry: TGraphEntry;
  LPatternGrid: TWfcPatternGrid3D;
  LProjection: TWfcTokenGrid3D;
  LToken: TWfcModelToken;
  X: Integer;
  Y: Integer;
  Z: Integer;
begin
  APatternGrid := Default(TWfcPatternGrid3D);
  AProjection := Default(TWfcTokenGrid3D);
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
  ValidateWrappedVolume(APatternGraph,
    'overlapping projection capture source');
  ValidateWrappedVolume(AProjectionGraph,
    'overlapping projection capture target');
  if (APatternGraph.Dimension.Width <> AProjectionGraph.Dimension.Width) or
      (APatternGraph.Dimension.Height <> AProjectionGraph.Dimension.Height) or
      (APatternGraph.Dimension.Depth <> AProjectionGraph.Dimension.Depth) then
    raise EWfcPattern3DGraph.Create(
      'overlapping projection capture pass shapes differ');
  if APatternGraph.PassGraph[0] <> AProjectionGraph.PassGraph[0] then
    raise EWfcPattern3DGraph.Create(
      'overlapping projection capture passes must share one pipeline');
  RequireIdenticalBridgeLayout(APatternGraph, AProjectionGraph,
    'overlapping projection capture');
  if not AppliedPatternModelMatches(AModel, APatternGraph) then
    raise EWfcPattern3DGraph.Create(
      'overlapping projection capture source model does not match');

  if not CaptureSolvedPatternGrid3D(AModel, APatternGraph,
      LPatternGrid, AReport) then
  begin
    { A graph-private value may explain a malformed latent cell internally,
      but the public bridge never returns that representation. }
    AReport.Issue.Value := '';
    Exit(False);
  end;
  LProjection := Default(TWfcTokenGrid3D);
  LProjection.Width := Integer(AProjectionGraph.Dimension.Width);
  LProjection.Height := Integer(AProjectionGraph.Dimension.Height);
  LProjection.Depth := Integer(AProjectionGraph.Dimension.Depth);
  SetLength(LProjection.Tokens,
    LProjection.Width * LProjection.Height * LProjection.Depth);
  for Z := 0 to LProjection.Depth - 1 do
   for Y := 0 to LProjection.Height - 1 do
    for X := 0 to LProjection.Width - 1 do
    begin
      LEntry := AProjectionGraph.Entry[X, Y, Z];
      if LEntry.Empty then
      begin
        InitializeValidationReport(AReport);
        AReport.Issue.Kind := wo3ikEmptyGraphCell;
        AReport.Issue.X := X;
        AReport.Issue.Y := Y;
        AReport.Issue.Z := Z;
        Exit(False);
      end;
      LToken := GraphValueToModelToken(LEntry.Value);
      LProjection.Tokens[(Z * LProjection.Height + Y) * LProjection.Width + X] := LToken;
    end;
  Result := ValidateOverlappingProjection3D(AModel, LPatternGrid,
    LProjection, AReport);
  if not Result then
  begin
    AReport.Issue.Value := '';
    Exit(False);
  end;
  APatternGrid := LPatternGrid;
  AProjection := LProjection;
end;

procedure HashByte(var AHash: TWfcPattern3DCompositionSignature;
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

procedure HashCardinal(var AHash: TWfcPattern3DCompositionSignature;
  const AValue: Cardinal);
begin
  HashByte(AHash, Byte(AValue and $FF));
  HashByte(AHash, Byte((AValue shr 8) and $FF));
  HashByte(AHash, Byte((AValue shr 16) and $FF));
  HashByte(AHash, Byte((AValue shr 24) and $FF));
end;

procedure HashInteger(var AHash: TWfcPattern3DCompositionSignature;
  const AValue: Integer);
begin
  HashCardinal(AHash, Cardinal(AValue));
end;

procedure HashToken(var AHash: TWfcPattern3DCompositionSignature;
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

function CalculateWfcPattern3DCompositionSignature(
  const AModel: TWfcOverlappingModel3D;
  const ASeed: TGraphSeed;
  const APatterns: TWfcPatternGrid3D;
  const AProjection: TWfcTokenGrid3D):
  TWfcPattern3DCompositionSignature;
var
  I: Integer;
  X: Integer;
  Y: Integer;
  Z: Integer;
  LValidation: TWfcOverlapping3DValidationReport;
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create(
      'overlapping signature model cannot be nil');
  RequireCardinal(ASeed, 'composition seed');
  if not ValidateOverlappingProjection3D(AModel, APatterns, AProjection,
      LValidation) then
    raise EWfcPattern3DGraph.Create('cannot sign an invalid volume projection');
  Result := Cardinal(2166136261);
  HashToken(Result, 'wfc-pattern3d-composition');
  HashCardinal(Result, WFC_PATTERN_3D_COMPOSITION_SIGNATURE_VERSION);
  HashCardinal(Result, WFC_PATTERN_3D_GRAPH_ADAPTER_VERSION);
  HashCardinal(Result, ASeed);
  HashInteger(Result, APatterns.Width);
  HashInteger(Result, APatterns.Height);
  HashInteger(Result, APatterns.Depth);
  HashInteger(Result, Ord(APatterns.Boundary));
  HashInteger(Result, AModel.PatternWidth);
  HashInteger(Result, AModel.PatternHeight);
  HashInteger(Result, AModel.PatternDepth);
  HashInteger(Result, Ord(AModel.SourceBoundary));
  HashInteger(Result, Ord(AModel.Symmetry));
  HashInteger(Result, AModel.SourceCount);
  for I := 0 to AModel.SourceCount - 1 do
  begin
    HashInteger(Result, AModel.SourceShapeAt(I).Width);
    HashInteger(Result, AModel.SourceShapeAt(I).Height);
    HashInteger(Result, AModel.SourceShapeAt(I).Depth);
  end;
  HashInteger(Result, AModel.PaletteCount);
  for I := 0 to AModel.PaletteCount - 1 do
    HashToken(Result, AModel.PaletteTokenAt(I));
  HashInteger(Result, AModel.PatternCount);
  for I := 0 to AModel.PatternCount - 1 do
  begin
    HashInteger(Result, AModel.PatternWeightAt(I));
    for Z := 0 to AModel.PatternDepth - 1 do
     for Y := 0 to AModel.PatternHeight - 1 do
      for X := 0 to AModel.PatternWidth - 1 do
        HashInteger(Result,
          AModel.PatternPaletteIndexAt(I, X, Y, Z));
  end;
  HashInteger(Result, Length(APatterns.Patterns));
  for I := 0 to Length(APatterns.Patterns) - 1 do
    HashInteger(Result, APatterns.Patterns[I]);
  HashInteger(Result, AProjection.Width);
  HashInteger(Result, AProjection.Height);
  HashInteger(Result, AProjection.Depth);
  HashInteger(Result, Length(AProjection.Tokens));
  for I := 0 to Length(AProjection.Tokens) - 1 do
    HashToken(Result, AProjection.Tokens[I]);
end;

function CalculateWfcPattern3DCompositionSignature(
  const AModel: TWfcOverlappingModel3D;
  const AComposition: TWfcPattern3DComposition):
  TWfcPattern3DCompositionSignature;
begin
  if not Assigned(AComposition) then
    raise EArgumentNilException.Create(
      'overlapping composition cannot be nil');
  Result := CalculateWfcPattern3DCompositionSignature(AModel,
    AComposition.FSeed, AComposition.FPatterns,
    AComposition.FProjection);
end;

function WfcPattern3DCompositionSignatureHex(
  const ASignature: TWfcPattern3DCompositionSignature): String;
begin
  RequireCardinal(ASignature, 'composition signature');
  Result := IntToHex(ASignature, 8);
end;

function ClonePatternGrid(const ASource: TWfcPatternGrid3D):
  TWfcPatternGrid3D;
var
  I: Integer;
begin
  Result := Default(TWfcPatternGrid3D);
  Result.Width := ASource.Width;
  Result.Height := ASource.Height;
  Result.Depth := ASource.Depth;
  Result.Boundary := ASource.Boundary;
  SetLength(Result.Patterns, Length(ASource.Patterns));
  for I := 0 to Length(ASource.Patterns) - 1 do
    Result.Patterns[I] := ASource.Patterns[I];
end;

function CloneTokenGrid(const ASource: TWfcTokenGrid3D): TWfcTokenGrid3D;
var
  I: Integer;
begin
  Result := Default(TWfcTokenGrid3D);
  Result.Width := ASource.Width;
  Result.Height := ASource.Height;
  Result.Depth := ASource.Depth;
  SetLength(Result.Tokens, Length(ASource.Tokens));
  for I := 0 to Length(ASource.Tokens) - 1 do
    Result.Tokens[I] := ASource.Tokens[I];
end;

constructor TWfcPattern3DComposition.CreateInternal(
  const AModel: TWfcOverlappingModel3D; const ASeed: TGraphSeed;
  const APatterns: TWfcPatternGrid3D;
  const AProjection: TWfcTokenGrid3D);
begin
  inherited Create;
  FSeed := ASeed;
  FPatterns := ClonePatternGrid(APatterns);
  FProjection := CloneTokenGrid(AProjection);
  FSignature := CalculateWfcPattern3DCompositionSignature(AModel,
    FSeed, FPatterns, FProjection);
end;

function TWfcPattern3DComposition.GetWidth: Integer;
begin
  Result := FProjection.Width;
end;

function TWfcPattern3DComposition.GetHeight: Integer;
begin
  Result := FProjection.Height;
end;

function TWfcPattern3DComposition.GetDepth: Integer;
begin
  Result := FProjection.Depth;
end;

function TWfcPattern3DComposition.CopyPatternGrid: TWfcPatternGrid3D;
begin
  Result := ClonePatternGrid(FPatterns);
end;

function TWfcPattern3DComposition.CopyProjection: TWfcTokenGrid3D;
begin
  Result := CloneTokenGrid(FProjection);
end;

constructor TWfcPattern3DPassCommitGraph.CreatePass(const ARoot: TGraph;
  const APassIndex: Integer);
begin
  inherited CreatePass(ARoot, APassIndex);
  if not (ARoot is TWfcPattern3DPassCommitGraph) then
    raise EInvalidOperation.Create(
      'pattern pass root has an incompatible graph class');
  FOwner := TWfcPattern3DPassCommitGraph(ARoot).Owner;
end;

function TWfcPattern3DPassCommitGraph.DoValidateCommit(
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

function DefaultWfcPattern3DPassConfig(
  const AModel: TWfcOverlappingModel3D;
  const AWidth, AHeight, ADepth: Integer;
  const ASeed: TGraphSeed): TWfcPattern3DPassConfig;
begin
  Result := Default(TWfcPattern3DPassConfig);
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.Depth := ADepth;
  Result.Seed := ASeed;
  Result.Model := AModel;
end;

constructor TWfcPattern3DPassPipeline.Create(
  const AConfig: TWfcPattern3DPassConfig);
begin
  inherited Create;
  Initialize(AConfig);
end;

procedure TWfcPattern3DPassPipeline.Initialize(
  const AConfig: TWfcPattern3DPassConfig);
begin
  if not Assigned(AConfig.Model) then
    raise EArgumentNilException.Create(
      'pattern pass model cannot be nil');
  RequireCardinal(AConfig.Seed, 'pattern pass seed');
  RequireInteger(AConfig.Width, 1, High(Integer), 'pattern pass width');
  RequireInteger(AConfig.Height, 1, High(Integer), 'pattern pass height');
  RequireInteger(AConfig.Depth, 1, High(Integer), 'pattern pass depth');
  FCellCount := CheckedProduct(CheckedProduct(AConfig.Width, AConfig.Height,
    'pattern pass plane'), AConfig.Depth, 'pattern pass volume');

  FModel := AConfig.Model;
  FWidth := AConfig.Width;
  FHeight := AConfig.Height;
  FDepth := AConfig.Depth;
  SetLength(FPublicDomains, FCellCount);
  SetLength(FPublicDomainAssigned, FCellCount);
  FPublicDomainsDirty := False;
  FCommittedSeed := AConfig.Seed;
  FHasCommit := False;
  FGraph := nil;
  FPendingComposition := nil;
  InitializeValidationReport(FPendingValidation);
  FPendingStatus := wp3psNotRun;
  FPendingFailedLayer := wp3lPatterns;
  try
    FGraph := TWfcPattern3DPassCommitGraph.Create;
    TWfcPattern3DPassCommitGraph(FGraph).Owner := Self;
    FGraph.Reshape(FWidth, FHeight, FDepth);
    FGraph.WrapNeighbors := True;
    FGraph.Seed := AConfig.Seed;

    FGraph.CurrentPass := WFC_PATTERN_3D_PASS_PATTERNS;
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies;
    ApplyOverlappingModel3DToGraph(FModel, FGraph);

    FGraph.SwitchToPass(WFC_PATTERN_3D_PASS_PROJECTION);
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies;
    ApplyOverlappingProjectionFromPass3D(FModel, FGraph,
      WFC_PATTERN_3D_PASS_PATTERNS);
    FGraph.SwitchToPass(WFC_PATTERN_3D_PASS_PATTERNS);
  except
    FGraph.Free;
    FGraph := nil;
    raise;
  end;
end;

destructor TWfcPattern3DPassPipeline.Destroy;
begin
  ClearPendingCommit;
  FGraph.Free;
  inherited Destroy;
end;

procedure TWfcPattern3DPassPipeline.ClearPendingCommit;
begin
  FPendingComposition.Free;
  FPendingComposition := nil;
  InitializeValidationReport(FPendingValidation);
  FPendingStatus := wp3psNotRun;
  FPendingFailedLayer := wp3lPatterns;
end;

function TWfcPattern3DPassPipeline.GetSeed: TGraphSeed;
begin
  Result := FGraph.Seed;
end;

procedure TWfcPattern3DPassPipeline.SetSeed(const AValue: TGraphSeed);
begin
  if FGraph.Running then
    raise EInvalidOperation.Create('cannot change seed while generating');
  RequireCardinal(AValue, 'pattern pass seed');
  FGraph.Seed := AValue;
  InvalidateComposition;
end;

function TWfcPattern3DPassPipeline.PublicCellIndex(
  const AX, AY, AZ: Integer): Integer;
begin
  RequireInteger(AX, 0, FWidth - 1, 'public X');
  RequireInteger(AY, 0, FHeight - 1, 'public Y');
  RequireInteger(AZ, 0, FDepth - 1, 'public Z');
  Result := (AZ * FHeight + AY) * FWidth + AX;
end;

procedure TWfcPattern3DPassPipeline.InvalidateComposition;
begin
  FHasCommit := False;
  ClearPendingCommit;
end;

procedure TWfcPattern3DPassPipeline.SetPublicDomain(
  const AX, AY, AZ: Integer; const AValues: TWfcModelTokens);
var I, P, N, LCell: Integer; LSeen: TPatternByteArray;
  LDomain: TWfcModelIntegerArray;
begin
  if FGraph.Running then
    raise EInvalidOperation.Create('cannot change public domains while generating');
  LCell := PublicCellIndex(AX, AY, AZ);
  SetLength(LSeen, FModel.PaletteCount);
  N := 0;
  for I := 0 to High(AValues) do
  begin
    P := FModel.FindPaletteToken(AValues[I]);
    if P < 0 then
      raise EWfcPattern3DGraph.Create('public domain contains an unknown palette token');
    if LSeen[P] = 0 then begin LSeen[P] := 1; Inc(N); end;
  end;
  SetLength(LDomain, N);
  N := 0;
  for P := 0 to FModel.PaletteCount - 1 do
    if LSeen[P] <> 0 then begin LDomain[N] := P; Inc(N); end;
  FPublicDomains[LCell] := LDomain;
  FPublicDomainAssigned[LCell] := 1;
  FPublicDomainsDirty := True;
  InvalidateComposition;
end;

procedure TWfcPattern3DPassPipeline.LockPublicCell(
  const AX, AY, AZ: Integer; const AValue: TWfcModelToken);
var LValues: TWfcModelTokens;
begin
  SetLength(LValues, 1);
  LValues[0] := AValue;
  SetPublicDomain(AX, AY, AZ, LValues);
end;

procedure TWfcPattern3DPassPipeline.ClearPublicDomain(
  const AX, AY, AZ: Integer);
var LCell: Integer;
begin
  if FGraph.Running then
    raise EInvalidOperation.Create('cannot change public domains while generating');
  LCell := PublicCellIndex(AX, AY, AZ);
  if FPublicDomainAssigned[LCell] = 0 then Exit;
  FPublicDomains[LCell] := nil;
  FPublicDomainAssigned[LCell] := 0;
  FPublicDomainsDirty := True;
  InvalidateComposition;
end;

procedure TWfcPattern3DPassPipeline.ClearPublicDomains;
var I: Integer;
begin
  if FGraph.Running then
    raise EInvalidOperation.Create('cannot change public domains while generating');
  for I := 0 to FCellCount - 1 do
  begin FPublicDomains[I] := nil; FPublicDomainAssigned[I] := 0; end;
  FPublicDomainsDirty := True;
  InvalidateComposition;
end;

function TWfcPattern3DPassPipeline.HasPublicDomain(
  const AX, AY, AZ: Integer): Boolean;
begin
  Result := FPublicDomainAssigned[PublicCellIndex(AX, AY, AZ)] <> 0;
end;

function TWfcPattern3DPassPipeline.CopyPublicDomain(
  const AX, AY, AZ: Integer): TWfcModelTokens;
var I, LCell: Integer;
begin
  LCell := PublicCellIndex(AX, AY, AZ);
  Result := nil;
  SetLength(Result, Length(FPublicDomains[LCell]));
  for I := 0 to High(Result) do
    Result[I] := FModel.PaletteTokenAt(FPublicDomains[LCell][I]);
end;

procedure TWfcPattern3DPassPipeline.PreparePublicDomains;
var
  LRows, LAnchors: TPatternIntegerArray;
  LMasks: array of TPatternByteArray;
  LAllowed: TPatternByteArray;
  LValues, LPublicValues: TPatternGraphValueArrays;
  LKeys: TGraphValues;
  LRowCount, I, J, P, X, Y, Z, PX, PY, PZ, AX, AY, AZ,
    LAnchor, LRow, N: Integer;
  LPatternsGraph, LProjectionGraph: TGraph;

  function SubtractWrapped(const ACoordinate, AOffset, ASize: Integer): Integer;
  var LOffset: Integer;
  begin
    LOffset := AOffset mod ASize;
    if ACoordinate >= LOffset then Result := ACoordinate - LOffset
    else Result := ASize - (LOffset - ACoordinate);
  end;
begin
  if not FPublicDomainsDirty then Exit;
  { Build all intersections before touching graph domains. Only affected
    anchors allocate a pattern mask. Offsets that alias on a small torus must
    still ALL constrain that same mask; dropping an alias loses information. }
  SetLength(LRows, FCellCount);
  SetLength(LAnchors, FCellCount);
  SetLength(LMasks, FCellCount);
  SetLength(LAllowed, FModel.PaletteCount);
  SetLength(LPublicValues, FCellCount);
  for I := 0 to FCellCount - 1 do LRows[I] := -1;
  LRowCount := 0;
  for I := 0 to FCellCount - 1 do
    if FPublicDomainAssigned[I] <> 0 then
    begin
      X := I mod FWidth; Y := (I div FWidth) mod FHeight;
      Z := I div (FWidth * FHeight);
      for P := 0 to FModel.PaletteCount - 1 do LAllowed[P] := 0;
      SetLength(LPublicValues[I], Length(FPublicDomains[I]));
      for J := 0 to High(FPublicDomains[I]) do
      begin
        P := FPublicDomains[I][J];
        LAllowed[P] := 1;
        LPublicValues[I][J] := ModelTokenToGraphValue(FModel.PaletteTokenAt(P));
      end;
      for PZ := 0 to FModel.PatternDepth - 1 do
       for PY := 0 to FModel.PatternHeight - 1 do
        for PX := 0 to FModel.PatternWidth - 1 do
        begin
          AX := SubtractWrapped(X, PX, FWidth);
          AY := SubtractWrapped(Y, PY, FHeight);
          AZ := SubtractWrapped(Z, PZ, FDepth);
          LAnchor := (AZ * FHeight + AY) * FWidth + AX;
          LRow := LRows[LAnchor];
          if LRow < 0 then
          begin
            CheckedProduct(LRowCount + 1, FModel.PatternCount,
              'public inverse mask cells');
            LRow := LRowCount; Inc(LRowCount);
            LRows[LAnchor] := LRow; LAnchors[LRow] := LAnchor;
            SetLength(LMasks[LRow], FModel.PatternCount);
            for P := 0 to FModel.PatternCount - 1 do LMasks[LRow][P] := 1;
          end;
          for P := 0 to FModel.PatternCount - 1 do
            if LAllowed[FModel.PatternPaletteIndexAt(P, PX, PY, PZ)] = 0 then
              LMasks[LRow][P] := 0;
        end;
    end;
  SetLength(LKeys, FModel.PatternCount);
  for P := 0 to FModel.PatternCount - 1 do LKeys[P] := PatternGraphValue(FModel, P);
  SetLength(LValues, LRowCount);
  for I := 0 to LRowCount - 1 do
  begin
    N := 0;
    for P := 0 to FModel.PatternCount - 1 do
      if LMasks[I][P] <> 0 then Inc(N);
    SetLength(LValues[I], N);
    N := 0;
    for P := 0 to FModel.PatternCount - 1 do
      if LMasks[I][P] <> 0 then begin LValues[I][N] := LKeys[P]; Inc(N); end;
  end;
  LPatternsGraph := FGraph.PassGraph[Ord(wp3lPatterns)];
  LProjectionGraph := FGraph.PassGraph[Ord(wp3lProjection)];
  for I := 0 to FCellCount - 1 do
  begin
    X := I mod FWidth; Y := (I div FWidth) mod FHeight;
    Z := I div (FWidth * FHeight);
    LPatternsGraph.ClearAllowedValues(X, Y, Z);
    LProjectionGraph.ClearAllowedValues(X, Y, Z);
    if FPublicDomainAssigned[I] <> 0 then
      LProjectionGraph.SetAllowedValues(X, Y, Z, LPublicValues[I]);
  end;
  for I := 0 to LRowCount - 1 do
  begin
    LAnchor := LAnchors[I];
    X := LAnchor mod FWidth; Y := (LAnchor div FWidth) mod FHeight;
    Z := LAnchor div (FWidth * FHeight);
    LPatternsGraph.SetAllowedValues(X, Y, Z, LValues[I]);
  end;
  FPublicDomainsDirty := False;
end;

function TWfcPattern3DPassPipeline.ValidatePublicDomains(
  const AProjection: TWfcTokenGrid3D;
  out AReport: TWfcOverlapping3DValidationReport): Boolean;
var I, J, P: Integer; LFound: Boolean;
begin
  InitializeValidationReport(AReport);
  if (AProjection.Width <> FWidth) or (AProjection.Height <> FHeight) or
      (AProjection.Depth <> FDepth) or (Length(AProjection.Tokens) <> FCellCount) then
  begin AReport.Issue.Kind := wo3ikProjectionShape; Exit(False); end;
  for I := 0 to FCellCount - 1 do
    if FPublicDomainAssigned[I] <> 0 then
    begin
      P := FModel.FindPaletteToken(AProjection.Tokens[I]);
      LFound := False;
      for J := 0 to High(FPublicDomains[I]) do
        if FPublicDomains[I][J] = P then begin LFound := True; Break; end;
      if not LFound then
      begin
        AReport.Issue.Kind := wo3ikProjectionToken;
        AReport.Issue.X := I mod FWidth;
        AReport.Issue.Y := (I div FWidth) mod FHeight;
        AReport.Issue.Z := I div (FWidth * FHeight);
        AReport.Issue.ActualPaletteIndex := P;
        Exit(False);
      end;
    end;
  AReport.Valid := True;
  Result := True;
end;

function TWfcPattern3DPassPipeline.DoValidateProjection(
  const APatterns: TWfcPatternGrid3D;
  const AProjection: TWfcTokenGrid3D;
  out AReport: TWfcOverlapping3DValidationReport): Boolean;
begin
  Result := ValidateOverlappingProjection3D(FModel, APatterns,
    AProjection, AReport);
end;

function TWfcPattern3DPassPipeline.CaptureComposition(
  const ASeed: TGraphSeed;
  out AComposition: TWfcPattern3DComposition;
  out AValidation: TWfcOverlapping3DValidationReport;
  out AFailedLayer: TWfcPattern3DPassLayer;
  out AStatus: TWfcPattern3DPassStatus): Boolean;
var
  LPatterns: TWfcPatternGrid3D;
  LProjection: TWfcTokenGrid3D;
  LDomainValidation: TWfcOverlapping3DValidationReport;
begin
  AComposition := nil;
  AFailedLayer := wp3lPatterns;
  AStatus := wp3psCaptureFailed;
  InitializeValidationReport(AValidation);
  if not CaptureSolvedOverlappingProjectionPass3D(FModel,
      FGraph.PassGraph[Ord(wp3lPatterns)],
      FGraph.PassGraph[Ord(wp3lProjection)], LPatterns,
      LProjection, AValidation) then
  begin
    if AValidation.Issue.Kind in
        [wo3ikProjectionShape, wo3ikProjectionToken] then
      AFailedLayer := wp3lProjection;
    Exit(False);
  end;

  AFailedLayer := wp3lProjection;
  AStatus := wp3psValidationFailed;
  if not ValidatePublicDomains(LProjection, LDomainValidation) then
  begin AValidation := LDomainValidation; Exit(False); end;
  if not DoValidateProjection(LPatterns, LProjection,
      AValidation) then
    Exit(False);
  try
    AComposition := TWfcPattern3DComposition.CreateInternal(FModel,
      ASeed, LPatterns, LProjection);
  except
    AComposition.Free;
    AComposition := nil;
    raise;
  end;
  AStatus := wp3psCompleted;
  Result := True;
end;

function TWfcPattern3DPassPipeline.ValidatePendingCommit(
  out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
var
  LFailedX: Integer;
  LFailedY: Integer;
  LFailedZ: Integer;

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
      (FPendingValidation.Issue.Z >= 0) and
      (FPendingValidation.Issue.X < FWidth) and
      (FPendingValidation.Issue.Y < FHeight) and
      (FPendingValidation.Issue.Z < FDepth) then
  begin
    LFailedX := FPendingValidation.Issue.X;
    LFailedY := FPendingValidation.Issue.Y;
    LFailedZ := FPendingValidation.Issue.Z;
    if (FPendingFailedLayer = wp3lProjection) and
        (FPendingValidation.Issue.Kind = wo3ikProjectionToken) and
        (FPendingValidation.Issue.PatternOffsetX >= 0) and
        (FPendingValidation.Issue.PatternOffsetY >= 0) and
        (FPendingValidation.Issue.PatternOffsetZ >= 0) then
    begin
      { Projection-token reports identify the contributing latent anchor.
        The failed entry belongs to the public pass at anchor + footprint
        offset, wrapped to the shared v1 volume. A custom validator that leaves
        the offsets negative continues to report X,Y,Z directly. }
      LFailedX := AddWrappedNonNegative(LFailedX,
        FPendingValidation.Issue.PatternOffsetX, FWidth);
      LFailedY := AddWrappedNonNegative(LFailedY,
        FPendingValidation.Issue.PatternOffsetY, FHeight);
      LFailedZ := AddWrappedNonNegative(LFailedZ,
        FPendingValidation.Issue.PatternOffsetZ, FDepth);
    end;
    AFailedEntryIndex := (LFailedZ * FHeight + LFailedY) * FWidth + LFailedX;
  end
  else
    AFailedEntryIndex := -1;
end;

function TWfcPattern3DPassPipeline.TakePendingComposition(
  out AComposition: TWfcPattern3DComposition;
  out AValidation: TWfcOverlapping3DValidationReport;
  out AFailedLayer: TWfcPattern3DPassLayer;
  out AStatus: TWfcPattern3DPassStatus): Boolean;
begin
  AComposition := FPendingComposition;
  FPendingComposition := nil;
  AValidation := FPendingValidation;
  AFailedLayer := FPendingFailedLayer;
  AStatus := FPendingStatus;
  Result := Assigned(AComposition) and
    (AStatus = wp3psCompleted) and AValidation.Valid;
  InitializeValidationReport(FPendingValidation);
  FPendingStatus := wp3psNotRun;
  FPendingFailedLayer := wp3lPatterns;
end;

function TWfcPattern3DPassPipeline.TryGenerate(
  const AOptions: TGraphSolveOptions;
  out AComposition: TWfcPattern3DComposition;
  out AReport: TWfcPattern3DPassReport): Boolean;
var
  LSolved: Boolean;
begin
  if FGraph.Running then
    raise EInvalidOperation.Create('cannot reenter volume generation');
  AComposition := nil;
  AReport := Default(TWfcPattern3DPassReport);
  AReport.Status := wp3psNotRun;
  AReport.FailedLayer := wp3lPatterns;
  InitializeValidationReport(AReport.Validation);
  ClearPendingCommit;
  try
    PreparePublicDomains;
    LSolved := FGraph.TrySolve(AOptions, AReport.Solve);
  except
    ClearPendingCommit;
    raise;
  end;
  if not LSolved then
  begin
    AReport.Status := wp3psSolveFailed;
    if (AReport.Solve.FailedPassIndex >= Ord(Low(TWfcPattern3DPassLayer))) and
        (AReport.Solve.FailedPassIndex <= Ord(High(TWfcPattern3DPassLayer))) then
      AReport.FailedLayer :=
        TWfcPattern3DPassLayer(AReport.Solve.FailedPassIndex);
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

function TWfcPattern3DPassPipeline.TryGenerate(
  out AComposition: TWfcPattern3DComposition;
  out AReport: TWfcPattern3DPassReport): Boolean;
var
  LOptions: TGraphSolveOptions;
begin
  LOptions := DefaultGraphSolveOptions;
  Result := TryGenerate(LOptions, AComposition, AReport);
end;

function TWfcPattern3DPassPipeline.TryCopyCommitted(
  out AComposition: TWfcPattern3DComposition;
  out AValidation: TWfcOverlapping3DValidationReport): Boolean;
var
  LFailedLayer: TWfcPattern3DPassLayer;
  LStatus: TWfcPattern3DPassStatus;
begin
  if FGraph.Running then
    raise EInvalidOperation.Create('cannot copy staged volume output while generating');
  AComposition := nil;
  InitializeValidationReport(AValidation);
  if not FHasCommit then
    Exit(False);
  Result := CaptureComposition(FCommittedSeed, AComposition,
    AValidation, LFailedLayer, LStatus);
end;

function TWfcPattern3DPassPipeline.Validate(
  const AComposition: TWfcPattern3DComposition;
  out AReport: TWfcOverlapping3DValidationReport): Boolean;
var LDomainValidation: TWfcOverlapping3DValidationReport;
begin
  if FGraph.Running then
    raise EInvalidOperation.Create('cannot reenter public volume validation while generating');
  InitializeValidationReport(AReport);
  if not Assigned(AComposition) then
    raise EArgumentNilException.Create(
      'pattern pass composition cannot be nil');
  Result := DoValidateProjection(AComposition.FPatterns,
    AComposition.FProjection, AReport);
  if Result and not ValidatePublicDomains(AComposition.FProjection,
      LDomainValidation) then
  begin AReport := LDomainValidation; Exit(False); end;
  if Result and
      (AComposition.Signature <>
        CalculateWfcPattern3DCompositionSignature(FModel,
          AComposition)) then
  begin
    AReport.Valid := False;
    AReport.Issue.Kind := wo3ikProjectionToken;
    AReport.Issue.X := -1;
    AReport.Issue.Y := -1;
    AReport.Issue.Z := -1;
    Result := False;
  end;
end;

end.
