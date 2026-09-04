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
unit wfc_pattern2d;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_model;

const
  WFC_OVERLAPPING_2D_ALGORITHM_VERSION = 1;
  WFC_OVERLAPPING_2D_PROJECTION_VERSION = 1;

type
  EWfcOverlapping2D = class(EWfcModel);

  TWfcPaletteIndex = Integer;
  TWfcPatternIndex = Integer;
  TWfcPattern2DPayload = array of TWfcPaletteIndex;
  TWfcPattern2DPayloads = array of TWfcPattern2DPayload;
  TWfcPatternIndices = array of TWfcPatternIndex;

  TWfcPatternGrid2D = record
    Width: Integer;
    Height: Integer;
    { Output topology. This is deliberately independent of the source
      boundary policy stored by TWfcOverlappingModel2D. }
    Boundary: TWfcModelBoundary;
    Patterns: TWfcPatternIndices;
  end;

  TWfcTokenGrid2D = record
    Width: Integer;
    Height: Integer;
    Tokens: TWfcModelTokens;
  end;

  TWfcOverlapping2DIssueKind = (
    woikNone,
    woikGridShape,
    woikPatternCount,
    woikPatternIndex,
    woikEmptyGraphCell,
    woikUnknownPatternKey,
    woikOverlap,
    woikProjectionShape,
    woikProjectionToken
  );

  TWfcOverlapping2DIssue = record
    Kind: TWfcOverlapping2DIssueKind;
    X: Integer;
    Y: Integer;
    NeighborX: Integer;
    NeighborY: Integer;
    HasDirection: Boolean;
    Direction: TWfcModelDirection;
    PatternIndex: Integer;
    RelatedPatternIndex: Integer;
    PatternOffsetX: Integer;
    PatternOffsetY: Integer;
    ExpectedPaletteIndex: Integer;
    ActualPaletteIndex: Integer;
    Value: TGraphValue;
  end;

  TWfcOverlapping2DValidationReport = record
    Valid: Boolean;
    CheckedPatterns: Integer;
    CheckedRelations: Integer;
    CheckedProjectionCells: Integer;
    Issue: TWfcOverlapping2DIssue;
  end;

  { TWfcOverlappingModel2D }

  (*
    Immutable overlapping-pattern data. Each compiled-model value represents
    one row-major payload, not one projected palette token. CompiledModel is a
    borrowed immutable reference owned by this wrapper; callers must not free
    it and must not retain it beyond the wrapper's lifetime.
  *)
  TWfcOverlappingModel2D = class
  strict private
    FPatternWidth: Integer;
    FPatternHeight: Integer;
    FPalette: TWfcModelTokens;
    FPatterns: TWfcPattern2DPayloads;
    FCompiledModel: TWfcModel;

    function GetSourceBoundary: TWfcModelBoundary;
    function GetSymmetry: TWfcModelSymmetry;
    function GetSourceCount: Integer;
    function GetPaletteCount: Integer;
    function GetPatternCount: Integer;
    procedure ValidatePaletteIndex(const AIndex: Integer);
    procedure ValidatePatternIndex(const AIndex: Integer);
  public
    constructor Create(const APatternWidth, APatternHeight: Integer;
      const ASourceBoundary: TWfcModelBoundary;
      const ASymmetry: TWfcModelSymmetry;
      const ASourceShapes: TWfcModelSampleShapes;
      const APalette: TWfcModelTokens;
      const APatterns: TWfcPattern2DPayloads;
      const APatternWeights: TWfcModelIntegerArray);
    destructor Destroy; override;

    function SourceShapeAt(
      const ASourceIndex: Integer): TWfcModelSampleShape;
    function PaletteTokenAt(
      const APaletteIndex: Integer): TWfcModelToken;
    function FindPaletteToken(const AToken: TWfcModelToken): Integer;
    function PatternPaletteIndexAt(const APatternIndex, AX,
      AY: Integer): Integer;
    function PatternWeightAt(const APatternIndex: Integer): Integer;
    function PatternKeyAt(
      const APatternIndex: Integer): TWfcModelToken;
    function FindPatternKey(const AKey: TWfcModelToken): Integer;
    function PatternsCompatible(const ASourcePattern,
      ATargetPattern: Integer;
      const ADirection: TWfcModelDirection): Boolean;

    function CopySourceShapes: TWfcModelSampleShapes;
    function CopyPalette: TWfcModelTokens;
    function CopyPattern(
      const APatternIndex: Integer): TWfcPattern2DPayload;
    function CopyPatterns: TWfcPattern2DPayloads;
    function CopyPatternWeights: TWfcModelIntegerArray;

    property PatternWidth: Integer read FPatternWidth;
    property PatternHeight: Integer read FPatternHeight;
    property SourceBoundary: TWfcModelBoundary read GetSourceBoundary;
    property Symmetry: TWfcModelSymmetry read GetSymmetry;
    property SourceCount: Integer read GetSourceCount;
    property PaletteCount: Integer read GetPaletteCount;
    property PatternCount: Integer read GetPatternCount;
    property CompiledModel: TWfcModel read FCompiledModel;
  end;

function MakeWfcPatternGrid2D(const AWidth, AHeight: Integer;
  const ABoundary: TWfcModelBoundary;
  const APatterns: TWfcPatternIndices): TWfcPatternGrid2D;

{ Applies the latent pattern model to the exact already-selected empty graph
  pass supplied by the caller. It never reshapes, relinks, or selects a pass. }
procedure ApplyOverlappingModel2DToGraph(
  const AModel: TWfcOverlappingModel2D; const AGraph: TGraph);

{ Captures the exact graph object supplied by the caller. Pipeline callers
  should pass Graph.PassGraph[Index] explicitly. }
function CaptureSolvedPatternGrid2D(
  const AModel: TWfcOverlappingModel2D; const AGraph: TGraph;
  const AZ: Integer; out AGrid: TWfcPatternGrid2D;
  out AReport: TWfcOverlapping2DValidationReport): Boolean;

function ValidateOverlappingPatternGrid2D(
  const AModel: TWfcOverlappingModel2D;
  const AGrid: TWfcPatternGrid2D;
  out AReport: TWfcOverlapping2DValidationReport): Boolean;

function TryProjectOverlappingPatternGrid2D(
  const AModel: TWfcOverlappingModel2D;
  const AGrid: TWfcPatternGrid2D; out AOutput: TWfcTokenGrid2D;
  out AReport: TWfcOverlapping2DValidationReport): Boolean;

function ValidateOverlappingProjection2D(
  const AModel: TWfcOverlappingModel2D;
  const AGrid: TWfcPatternGrid2D;
  const AOutput: TWfcTokenGrid2D;
  out AReport: TWfcOverlapping2DValidationReport): Boolean;

function DescribeOverlapping2DIssue(
  const AIssue: TWfcOverlapping2DIssue): String;

implementation

const
  WFC_CARDINAL_DIRECTIONS: TWfcModelDirections =
    [wmdNorth, wmdEast, wmdSouth, wmdWest];
  D4_TRANSFORM_COUNT = 8;

type
  TBooleanArray = array of Boolean;

function CheckedLength(const ALength: SizeInt;
  const ALabel: String): Integer;
begin
  if (ALength < 0) or
      ((ALength and (not SizeInt(High(Integer)))) <> 0) then
    raise EWfcOverlapping2D.Create(ALabel + ' exceeds the Integer range');
  Result := Integer(ALength);
end;

function CheckedProduct(const A, B: Integer;
  const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) then
    raise EWfcOverlapping2D.Create(ALabel + ' cannot be negative');
  if (A <> 0) and (B > High(Integer) div A) then
    raise EWfcOverlapping2D.Create(ALabel + ' exceeds the Integer range');
  Result := A * B;
end;

function CheckedAdd(const A, B: Integer;
  const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) then
    raise EWfcOverlapping2D.Create(ALabel + ' cannot be negative');
  if A > High(Integer) - B then
    raise EWfcOverlapping2D.Create(ALabel + ' exceeds the Integer range');
  Result := A + B;
end;

function CheckedRelationLength(const APatternCount: Integer): Integer;
var
  LSquare: Integer;
begin
  if APatternCount < 1 then
    raise EWfcOverlapping2D.Create(
      'an overlapping model must contain at least one pattern');
  LSquare := CheckedProduct(APatternCount, APatternCount,
    'overlapping relation dimensions');
  Result := CheckedProduct(4, LSquare,
    'overlapping relation dimensions');
end;

procedure ValidateBoundary(const ABoundary: TWfcModelBoundary);
begin
  case ABoundary of
    wmbOpen, wmbWrap:
      Exit;
  else
    raise EWfcOverlapping2D.CreateFmt(
      'unknown overlapping source boundary [%d]', [Ord(ABoundary)]);
  end;
end;

procedure ValidateSymmetry(const ASymmetry: TWfcModelSymmetry);
begin
  case ASymmetry of
    wmsNone, wmsD4:
      Exit;
  else
    raise EWfcOverlapping2D.CreateFmt(
      'unknown overlapping symmetry [%d]', [Ord(ASymmetry)]);
  end;
end;

function PatternKey(const AIndex: Integer): TWfcModelToken;
begin
  Result := TWfcModelToken('@p' + IntToStr(AIndex));
end;

function PayloadsEqual(const A, B: TWfcPattern2DPayload): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to Length(A) - 1 do
    if A[I] <> B[I] then
      Exit(False);
  Result := True;
end;

function FindPayload(const APatterns: TWfcPattern2DPayloads;
  const APayload: TWfcPattern2DPayload): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(APatterns) - 1 do
    if PayloadsEqual(APatterns[I], APayload) then
      Exit(I);
  Result := -1;
end;

procedure D4TransformToSource(const AX, AY, ASize,
  ATransform: Integer; out ASourceX, ASourceY: Integer);
var
  LRotation: Integer;
begin
  LRotation := ATransform mod 4;
  case LRotation of
    0:
      begin
        ASourceX := AX;
        ASourceY := AY;
      end;
    1:
      begin
        ASourceX := AY;
        ASourceY := ASize - 1 - AX;
      end;
    2:
      begin
        ASourceX := ASize - 1 - AX;
        ASourceY := ASize - 1 - AY;
      end;
    3:
      begin
        ASourceX := ASize - 1 - AY;
        ASourceY := AX;
      end;
  else
    raise ERangeError.Create('unknown overlapping D4 transform');
  end;
  if ATransform >= 4 then
    ASourceX := ASize - 1 - ASourceX;
end;

function RelationIndex(const ADirection: TWfcModelDirection;
  const ASource, ATarget, APatternCount: Integer): Integer;
begin
  Result := ((Ord(ADirection) * APatternCount + ASource) *
    APatternCount) + ATarget;
end;

procedure InitializeReport(out AReport: TWfcOverlapping2DValidationReport);
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

function InvalidReport(var AReport: TWfcOverlapping2DValidationReport;
  const AKind: TWfcOverlapping2DIssueKind): Boolean;
begin
  AReport.Valid := False;
  AReport.Issue.Kind := AKind;
  Result := False;
end;

procedure CheckedReportIncrement(var AValue: Integer);
begin
  if AValue = High(Integer) then
    raise EWfcOverlapping2D.Create(
      'overlapping validation counter exceeds the Integer range');
  Inc(AValue);
end;

function SafeModuloAdd(const ABase, AOffset,
  AModulus: Integer): Integer;
var
  LOffset: Integer;
begin
  LOffset := AOffset mod AModulus;
  if LOffset = 0 then
    Exit(ABase);
  if ABase >= AModulus - LOffset then
    Result := ABase - (AModulus - LOffset)
  else
    Result := ABase + LOffset;
end;

function ProjectionDimensions(const AModel: TWfcOverlappingModel2D;
  const AGrid: TWfcPatternGrid2D; out AWidth, AHeight: Integer): Boolean;
begin
  Result := False;
  if AGrid.Boundary = wmbWrap then
  begin
    AWidth := AGrid.Width;
    AHeight := AGrid.Height;
    Exit(True);
  end;
  if AGrid.Boundary <> wmbOpen then
    Exit;
  if AGrid.Width > High(Integer) - AModel.PatternWidth + 1 then
    Exit;
  if AGrid.Height > High(Integer) - AModel.PatternHeight + 1 then
    Exit;
  AWidth := AGrid.Width + AModel.PatternWidth - 1;
  AHeight := AGrid.Height + AModel.PatternHeight - 1;
  Result := True;
end;

{ TWfcOverlappingModel2D }

constructor TWfcOverlappingModel2D.Create(const APatternWidth,
  APatternHeight: Integer; const ASourceBoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry;
  const ASourceShapes: TWfcModelSampleShapes;
  const APalette: TWfcModelTokens;
  const APatterns: TWfcPattern2DPayloads;
  const APatternWeights: TWfcModelIntegerArray);
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  K: Integer;
  LBaseOrigins: Integer;
  LExpectedObservations: Integer;
  LFootprintSize: Integer;
  LObservedWeight: Integer;
  LOrbitPattern: Integer;
  LPaletteCount: Integer;
  LPatternCount: Integer;
  LRelationLength: Integer;
  LRelations: TWfcModelIntegerArray;
  LSampleCount: Integer;
  LShape: TWfcModelSampleShape;
  LTokens: TWfcModelTokens;
  LTransform: Integer;
  LTransformCount: Integer;
  LTransformedPayload: TWfcPattern2DPayload;
  LUsedPalette: TBooleanArray;
  LSourceX: Integer;
  LSourceY: Integer;
  X: Integer;
  Y: Integer;
begin
  inherited Create;
  ValidateBoundary(ASourceBoundary);
  ValidateSymmetry(ASymmetry);
  if (APatternWidth < 1) or (APatternHeight < 1) then
    raise EWfcOverlapping2D.Create(
      'overlapping pattern dimensions must be positive');
  if (ASymmetry = wmsD4) and
      (APatternWidth <> APatternHeight) then
    raise EWfcOverlapping2D.Create(
      'D4 overlapping patterns must have a square footprint');
  LFootprintSize := CheckedProduct(APatternWidth, APatternHeight,
    'overlapping pattern footprint');

  LSampleCount := CheckedLength(Length(ASourceShapes),
    'overlapping source-shape count');
  if LSampleCount = 0 then
    raise EWfcOverlapping2D.Create(
      'an overlapping model must retain at least one source shape');
  if ASymmetry = wmsD4 then
    LTransformCount := D4_TRANSFORM_COUNT
  else
    LTransformCount := 1;
  LExpectedObservations := 0;
  for I := 0 to LSampleCount - 1 do
  begin
    LShape := ASourceShapes[I];
    if (LShape.Width < 1) or (LShape.Height < 1) then
      raise EWfcOverlapping2D.CreateFmt(
        'overlapping source dimensions must be positive [%d: %d x %d]',
        [I, LShape.Width, LShape.Height]);
    if ASourceBoundary = wmbOpen then
    begin
      if (LShape.Width < APatternWidth) or
          (LShape.Height < APatternHeight) then
        raise EWfcOverlapping2D.CreateFmt(
          'open overlapping source %d is smaller than the pattern [%d x %d < %d x %d]',
          [I, LShape.Width, LShape.Height,
            APatternWidth, APatternHeight]);
      LBaseOrigins := CheckedProduct(
        LShape.Width - APatternWidth + 1,
        LShape.Height - APatternHeight + 1,
        'open overlapping origin count');
    end
    else
      LBaseOrigins := CheckedProduct(LShape.Width, LShape.Height,
        'wrapped overlapping origin count');
    LExpectedObservations := CheckedAdd(LExpectedObservations,
      CheckedProduct(LBaseOrigins, LTransformCount,
        'overlapping transformed observation count'),
      'overlapping corpus observation count');
  end;

  LPaletteCount := CheckedLength(Length(APalette),
    'overlapping palette count');
  if LPaletteCount = 0 then
    raise EWfcOverlapping2D.Create(
      'an overlapping model must contain at least one palette token');
  for I := 0 to LPaletteCount - 1 do
  begin
    if not WfcModelTokenIsValid(APalette[I]) then
      raise EWfcOverlapping2D.CreateFmt(
        'overlapping palette token must be nonempty, well-formed UTF-8 [%d]',
        [I]);
    for J := 0 to I - 1 do
      if APalette[I] = APalette[J] then
        raise EWfcOverlapping2D.CreateFmt(
          'overlapping palette tokens must be unique [%d, %d]', [J, I]);
  end;

  LPatternCount := CheckedLength(Length(APatterns),
    'overlapping pattern count');
  LRelationLength := CheckedRelationLength(LPatternCount);
  if Length(APatternWeights) <> LPatternCount then
    raise EWfcOverlapping2D.CreateFmt(
      'overlapping pattern weight count must match pattern count [%d <> %d]',
      [Length(APatternWeights), LPatternCount]);

  SetLength(LUsedPalette, LPaletteCount);
  LObservedWeight := 0;
  for I := 0 to LPatternCount - 1 do
  begin
    if Length(APatterns[I]) <> LFootprintSize then
      raise EWfcOverlapping2D.CreateFmt(
        'overlapping pattern %d has %d cells; expected %d',
        [I, Length(APatterns[I]), LFootprintSize]);
    if APatternWeights[I] < 1 then
      raise EWfcOverlapping2D.CreateFmt(
        'overlapping pattern weight must be positive [%d]', [I]);
    LObservedWeight := CheckedAdd(LObservedWeight, APatternWeights[I],
      'overlapping pattern weight total');
    for J := 0 to LFootprintSize - 1 do
    begin
      K := APatterns[I][J];
      if (K < 0) or (K >= LPaletteCount) then
        raise EWfcOverlapping2D.CreateFmt(
          'overlapping pattern palette index is out of bounds [%d, %d: %d]',
          [I, J, K]);
      LUsedPalette[K] := True;
    end;
    for J := 0 to I - 1 do
      if PayloadsEqual(APatterns[I], APatterns[J]) then
        raise EWfcOverlapping2D.CreateFmt(
          'overlapping patterns must be structurally unique [%d, %d]',
          [J, I]);
  end;
  if LObservedWeight <> LExpectedObservations then
    raise EWfcOverlapping2D.CreateFmt(
      'overlapping pattern weights do not match source observations [%d <> %d]',
      [LObservedWeight, LExpectedObservations]);
  for I := 0 to LPaletteCount - 1 do
    if not LUsedPalette[I] then
      raise EWfcOverlapping2D.CreateFmt(
        'overlapping palette token is not used by any pattern [%d]', [I]);

  { Explicit D4 augmentation observes a complete group orbit. Transforming
    any retained payload must therefore find another retained payload with the
    same raw frequency. This rejects metadata that no D4 learner run could
    produce while naturally accepting stabilizers of symmetric patterns. }
  if ASymmetry = wmsD4 then
  begin
    SetLength(LTransformedPayload, LFootprintSize);
    for I := 0 to LPatternCount - 1 do
      for LTransform := 0 to D4_TRANSFORM_COUNT - 1 do
      begin
        for Y := 0 to APatternHeight - 1 do
          for X := 0 to APatternWidth - 1 do
          begin
            D4TransformToSource(X, Y, APatternWidth, LTransform,
              LSourceX, LSourceY);
            LTransformedPayload[Y * APatternWidth + X] :=
              APatterns[I][LSourceY * APatternWidth + LSourceX];
          end;
        LOrbitPattern := FindPayload(APatterns, LTransformedPayload);
        if LOrbitPattern < 0 then
          raise EWfcOverlapping2D.CreateFmt(
            'D4 overlapping pattern orbit is incomplete [%d, transform %d]',
            [I, LTransform]);
        if APatternWeights[LOrbitPattern] <> APatternWeights[I] then
          raise EWfcOverlapping2D.CreateFmt(
            'D4 overlapping pattern orbit weights differ [%d, %d]',
            [I, LOrbitPattern]);
      end;
  end;

  FPatternWidth := APatternWidth;
  FPatternHeight := APatternHeight;
  SetLength(FPalette, LPaletteCount);
  for I := 0 to LPaletteCount - 1 do
    FPalette[I] := APalette[I];
  SetLength(FPatterns, LPatternCount);
  for I := 0 to LPatternCount - 1 do
  begin
    SetLength(FPatterns[I], LFootprintSize);
    for J := 0 to LFootprintSize - 1 do
      FPatterns[I][J] := APatterns[I][J];
  end;

  SetLength(LTokens, LPatternCount);
  for I := 0 to LPatternCount - 1 do
    LTokens[I] := PatternKey(I);
  SetLength(LRelations, LRelationLength);
  for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    for I := 0 to LPatternCount - 1 do
      for J := 0 to LPatternCount - 1 do
        if PatternsCompatible(I, J, D) then
          LRelations[RelationIndex(D, I, J, LPatternCount)] := 1;

  FCompiledModel := TWfcModel.Create(2, ASourceShapes,
    ASourceBoundary, ASymmetry, WFC_CARDINAL_DIRECTIONS,
    LTokens, APatternWeights, LRelations);
end;

destructor TWfcOverlappingModel2D.Destroy;
begin
  FCompiledModel.Free;
  inherited Destroy;
end;

function TWfcOverlappingModel2D.GetSourceBoundary: TWfcModelBoundary;
begin
  Result := FCompiledModel.Boundary;
end;

function TWfcOverlappingModel2D.GetSymmetry: TWfcModelSymmetry;
begin
  Result := FCompiledModel.Symmetry;
end;

function TWfcOverlappingModel2D.GetSourceCount: Integer;
begin
  Result := FCompiledModel.SampleCount;
end;

function TWfcOverlappingModel2D.GetPaletteCount: Integer;
begin
  Result := Integer(Length(FPalette));
end;

function TWfcOverlappingModel2D.GetPatternCount: Integer;
begin
  Result := Integer(Length(FPatterns));
end;

procedure TWfcOverlappingModel2D.ValidatePaletteIndex(
  const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= PaletteCount) then
    raise ERangeError.CreateFmt(
      'overlapping palette index out of bounds [%d]', [AIndex]);
end;

procedure TWfcOverlappingModel2D.ValidatePatternIndex(
  const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= PatternCount) then
    raise ERangeError.CreateFmt(
      'overlapping pattern index out of bounds [%d]', [AIndex]);
end;

function TWfcOverlappingModel2D.SourceShapeAt(
  const ASourceIndex: Integer): TWfcModelSampleShape;
begin
  Result := FCompiledModel.SampleShapeAt(ASourceIndex);
end;

function TWfcOverlappingModel2D.PaletteTokenAt(
  const APaletteIndex: Integer): TWfcModelToken;
begin
  ValidatePaletteIndex(APaletteIndex);
  Result := FPalette[APaletteIndex];
end;

function TWfcOverlappingModel2D.FindPaletteToken(
  const AToken: TWfcModelToken): Integer;
var
  I: Integer;
begin
  for I := 0 to PaletteCount - 1 do
    if FPalette[I] = AToken then
      Exit(I);
  Result := -1;
end;

function TWfcOverlappingModel2D.PatternPaletteIndexAt(
  const APatternIndex, AX, AY: Integer): Integer;
begin
  ValidatePatternIndex(APatternIndex);
  if (AX < 0) or (AX >= FPatternWidth) or
      (AY < 0) or (AY >= FPatternHeight) then
    raise ERangeError.CreateFmt(
      'overlapping pattern coordinate out of bounds [%d, %d]', [AX, AY]);
  Result := FPatterns[APatternIndex][AY * FPatternWidth + AX];
end;

function TWfcOverlappingModel2D.PatternWeightAt(
  const APatternIndex: Integer): Integer;
begin
  ValidatePatternIndex(APatternIndex);
  Result := FCompiledModel.WeightAt(APatternIndex);
end;

function TWfcOverlappingModel2D.PatternKeyAt(
  const APatternIndex: Integer): TWfcModelToken;
begin
  ValidatePatternIndex(APatternIndex);
  Result := FCompiledModel.TokenAt(APatternIndex);
end;

function TWfcOverlappingModel2D.FindPatternKey(
  const AKey: TWfcModelToken): Integer;
var
  I: Integer;
  LDigit: Integer;
  LValue: Integer;
begin
  Result := -1;
  if (Length(AKey) < 3) or (AKey[1] <> '@') or (AKey[2] <> 'p') then
    Exit;
  if (Length(AKey) > 3) and (AKey[3] = '0') then
    Exit;
  LValue := 0;
  for I := 3 to Length(AKey) do
  begin
    if (AKey[I] < '0') or (AKey[I] > '9') then
      Exit;
    LDigit := Ord(AKey[I]) - Ord('0');
    if LValue > (High(Integer) - LDigit) div 10 then
      Exit;
    LValue := LValue * 10 + LDigit;
  end;
  if LValue >= PatternCount then
    Exit;
  if PatternKeyAt(LValue) <> AKey then
    Exit;
  Result := LValue;
end;

function TWfcOverlappingModel2D.PatternsCompatible(
  const ASourcePattern, ATargetPattern: Integer;
  const ADirection: TWfcModelDirection): Boolean;
var
  X: Integer;
  Y: Integer;
begin
  ValidatePatternIndex(ASourcePattern);
  ValidatePatternIndex(ATargetPattern);
  case ADirection of
    wmdNorth:
      for Y := 0 to FPatternHeight - 2 do
        for X := 0 to FPatternWidth - 1 do
          if PatternPaletteIndexAt(ASourcePattern, X, Y) <>
              PatternPaletteIndexAt(ATargetPattern, X, Y + 1) then
            Exit(False);
    wmdEast:
      for Y := 0 to FPatternHeight - 1 do
        for X := 1 to FPatternWidth - 1 do
          if PatternPaletteIndexAt(ASourcePattern, X, Y) <>
              PatternPaletteIndexAt(ATargetPattern, X - 1, Y) then
            Exit(False);
    wmdSouth:
      for Y := 1 to FPatternHeight - 1 do
        for X := 0 to FPatternWidth - 1 do
          if PatternPaletteIndexAt(ASourcePattern, X, Y) <>
              PatternPaletteIndexAt(ATargetPattern, X, Y - 1) then
            Exit(False);
    wmdWest:
      for Y := 0 to FPatternHeight - 1 do
        for X := 0 to FPatternWidth - 2 do
          if PatternPaletteIndexAt(ASourcePattern, X, Y) <>
              PatternPaletteIndexAt(ATargetPattern, X + 1, Y) then
            Exit(False);
  else
    raise ERangeError.CreateFmt(
      'unknown overlapping model direction [%d]', [Ord(ADirection)]);
  end;
  Result := True;
end;

function TWfcOverlappingModel2D.CopySourceShapes: TWfcModelSampleShapes;
begin
  Result := FCompiledModel.CopySampleShapes;
end;

function TWfcOverlappingModel2D.CopyPalette: TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, PaletteCount);
  for I := 0 to PaletteCount - 1 do
    Result[I] := FPalette[I];
end;

function TWfcOverlappingModel2D.CopyPattern(
  const APatternIndex: Integer): TWfcPattern2DPayload;
var
  I: Integer;
begin
  ValidatePatternIndex(APatternIndex);
  Result := nil;
  SetLength(Result, Length(FPatterns[APatternIndex]));
  for I := 0 to Length(Result) - 1 do
    Result[I] := FPatterns[APatternIndex][I];
end;

function TWfcOverlappingModel2D.CopyPatterns: TWfcPattern2DPayloads;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, PatternCount);
  for I := 0 to PatternCount - 1 do
    Result[I] := CopyPattern(I);
end;

function TWfcOverlappingModel2D.CopyPatternWeights: TWfcModelIntegerArray;
begin
  Result := FCompiledModel.CopyWeights;
end;

function MakeWfcPatternGrid2D(const AWidth, AHeight: Integer;
  const ABoundary: TWfcModelBoundary;
  const APatterns: TWfcPatternIndices): TWfcPatternGrid2D;
var
  I: Integer;
  LCellCount: Integer;
begin
  ValidateBoundary(ABoundary);
  if (AWidth < 1) or (AHeight < 1) then
    raise EWfcOverlapping2D.Create(
      'overlapping pattern-grid dimensions must be positive');
  LCellCount := CheckedProduct(AWidth, AHeight,
    'overlapping pattern-grid dimensions');
  if Length(APatterns) <> LCellCount then
    raise EWfcOverlapping2D.CreateFmt(
      'overlapping pattern-grid has %d patterns; expected %d',
      [Length(APatterns), LCellCount]);
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.Boundary := ABoundary;
  SetLength(Result.Patterns, LCellCount);
  for I := 0 to LCellCount - 1 do
    Result.Patterns[I] := APatterns[I];
end;

procedure ApplyOverlappingModel2DToGraph(
  const AModel: TWfcOverlappingModel2D; const AGraph: TGraph);
begin
  if not Assigned(AModel) then
    raise EWfcOverlapping2D.Create(
      'overlapping model must be assigned');
  if not Assigned(AGraph) then
    raise EWfcOverlapping2D.Create(
      'target graph pass must be assigned');
  ApplyModelToGraph(AModel.CompiledModel, AGraph);
end;

function ValidateOverlappingPatternGrid2D(
  const AModel: TWfcOverlappingModel2D;
  const AGrid: TWfcPatternGrid2D;
  out AReport: TWfcOverlapping2DValidationReport): Boolean;
var
  LCellCount: Integer;
  LCurrent: Integer;
  LNeighbor: Integer;
  LNeighborX: Integer;
  LNeighborY: Integer;
  X: Integer;
  Y: Integer;

  function CheckRelation(const ADirection: TWfcModelDirection;
    const ANeighborX, ANeighborY: Integer): Boolean;
  begin
    LNeighbor := AGrid.Patterns[ANeighborY * AGrid.Width + ANeighborX];
    CheckedReportIncrement(AReport.CheckedRelations);
    if not AModel.PatternsCompatible(LCurrent, LNeighbor, ADirection) then
    begin
      AReport.Issue.X := X;
      AReport.Issue.Y := Y;
      AReport.Issue.NeighborX := ANeighborX;
      AReport.Issue.NeighborY := ANeighborY;
      AReport.Issue.HasDirection := True;
      AReport.Issue.Direction := ADirection;
      AReport.Issue.PatternIndex := LCurrent;
      AReport.Issue.RelatedPatternIndex := LNeighbor;
      Exit(InvalidReport(AReport, woikOverlap));
    end;
    Result := True;
  end;

begin
  if not Assigned(AModel) then
    raise EWfcOverlapping2D.Create(
      'overlapping model must be assigned');
  InitializeReport(AReport);
  if (AGrid.Width < 1) or (AGrid.Height < 1) or
      ((AGrid.Boundary <> wmbOpen) and
       (AGrid.Boundary <> wmbWrap)) or
      (AGrid.Width > High(Integer) div AGrid.Height) then
    Exit(InvalidReport(AReport, woikGridShape));
  LCellCount := AGrid.Width * AGrid.Height;
  if Length(AGrid.Patterns) <> LCellCount then
    Exit(InvalidReport(AReport, woikPatternCount));

  for Y := 0 to AGrid.Height - 1 do
    for X := 0 to AGrid.Width - 1 do
    begin
      LCurrent := AGrid.Patterns[Y * AGrid.Width + X];
      if (LCurrent < 0) or (LCurrent >= AModel.PatternCount) then
      begin
        AReport.Issue.X := X;
        AReport.Issue.Y := Y;
        AReport.Issue.PatternIndex := LCurrent;
        Exit(InvalidReport(AReport, woikPatternIndex));
      end;
      CheckedReportIncrement(AReport.CheckedPatterns);
    end;

  for Y := 0 to AGrid.Height - 1 do
    for X := 0 to AGrid.Width - 1 do
    begin
      LCurrent := AGrid.Patterns[Y * AGrid.Width + X];
      if X + 1 < AGrid.Width then
        LNeighborX := X + 1
      else if AGrid.Boundary = wmbWrap then
        LNeighborX := 0
      else
        LNeighborX := -1;
      if LNeighborX >= 0 then
        if not CheckRelation(wmdEast, LNeighborX, Y) then
          Exit(False);

      if Y + 1 < AGrid.Height then
        LNeighborY := Y + 1
      else if AGrid.Boundary = wmbWrap then
        LNeighborY := 0
      else
        LNeighborY := -1;
      if LNeighborY >= 0 then
        if not CheckRelation(wmdSouth, X, LNeighborY) then
          Exit(False);
    end;
  AReport.Valid := True;
  AReport.Issue.Kind := woikNone;
  Result := True;
end;

function CaptureSolvedPatternGrid2D(
  const AModel: TWfcOverlappingModel2D; const AGraph: TGraph;
  const AZ: Integer; out AGrid: TWfcPatternGrid2D;
  out AReport: TWfcOverlapping2DValidationReport): Boolean;
var
  LCellCount: Integer;
  LEntry: TGraphEntry;
  LKey: TWfcModelToken;
  LPattern: Integer;
  X: Integer;
  Y: Integer;
begin
  if not Assigned(AModel) then
    raise EWfcOverlapping2D.Create(
      'overlapping model must be assigned');
  if not Assigned(AGraph) then
    raise EWfcOverlapping2D.Create(
      'source graph pass must be assigned');
  AGrid := Default(TWfcPatternGrid2D);
  InitializeReport(AReport);
  if (AGraph.Dimension.Width = 0) or
      (AGraph.Dimension.Height = 0) or
      (AGraph.Dimension.Width > TGraphCoordinate(High(Integer))) or
      (AGraph.Dimension.Height > TGraphCoordinate(High(Integer))) or
      (Integer(AGraph.Dimension.Width) >
        High(Integer) div Integer(AGraph.Dimension.Height)) or
      (AZ < 0) or
      (TGraphCoordinate(AZ) >= AGraph.Dimension.Depth) then
    Exit(InvalidReport(AReport, woikGridShape));
  AGrid.Width := Integer(AGraph.Dimension.Width);
  AGrid.Height := Integer(AGraph.Dimension.Height);
  if AGraph.WrapNeighbors then
    AGrid.Boundary := wmbWrap
  else
    AGrid.Boundary := wmbOpen;
  LCellCount := AGrid.Width * AGrid.Height;
  SetLength(AGrid.Patterns, LCellCount);
  for Y := 0 to AGrid.Height - 1 do
    for X := 0 to AGrid.Width - 1 do
    begin
      LEntry := AGraph.Entry[TGraphCoordinate(X), TGraphCoordinate(Y),
        TGraphCoordinate(AZ)];
      if LEntry.Empty then
      begin
        AReport.Issue.X := X;
        AReport.Issue.Y := Y;
        Exit(InvalidReport(AReport, woikEmptyGraphCell));
      end;
      {$IFDEF PAS2JS}
      LKey := TWfcModelToken(LEntry.Value);
      {$ELSE}
      LKey := UTF8Encode(UnicodeString(LEntry.Value));
      {$ENDIF}
      LPattern := AModel.FindPatternKey(LKey);
      if LPattern < 0 then
      begin
        AReport.Issue.X := X;
        AReport.Issue.Y := Y;
        AReport.Issue.Value := LEntry.Value;
        Exit(InvalidReport(AReport, woikUnknownPatternKey));
      end;
      AGrid.Patterns[Y * AGrid.Width + X] := LPattern;
    end;
  Result := ValidateOverlappingPatternGrid2D(AModel, AGrid, AReport);
end;

function TryProjectOverlappingPatternGrid2D(
  const AModel: TWfcOverlappingModel2D;
  const AGrid: TWfcPatternGrid2D; out AOutput: TWfcTokenGrid2D;
  out AReport: TWfcOverlapping2DValidationReport): Boolean;
var
  LAssigned: TBooleanArray;
  LCellCount: Integer;
  LExpected: TWfcModelToken;
  LOutputIndex: Integer;
  LPattern: Integer;
  LWriteCount: Integer;
  LOutputX: Integer;
  LOutputY: Integer;
  PX: Integer;
  PY: Integer;
  X: Integer;
  Y: Integer;
begin
  AOutput := Default(TWfcTokenGrid2D);
  if not ValidateOverlappingPatternGrid2D(AModel, AGrid, AReport) then
    Exit(False);
  if not ProjectionDimensions(AModel, AGrid,
      AOutput.Width, AOutput.Height) or
      (AOutput.Width > High(Integer) div AOutput.Height) then
    Exit(InvalidReport(AReport, woikProjectionShape));
  LCellCount := AOutput.Width * AOutput.Height;
  if (AGrid.Width * AGrid.Height) >
      High(Integer) div (AModel.PatternWidth * AModel.PatternHeight) then
    Exit(InvalidReport(AReport, woikProjectionShape));
  SetLength(AOutput.Tokens, LCellCount);
  SetLength(LAssigned, LCellCount);
  LWriteCount := 0;
  for Y := 0 to AGrid.Height - 1 do
    for X := 0 to AGrid.Width - 1 do
    begin
      LPattern := AGrid.Patterns[Y * AGrid.Width + X];
      for PY := 0 to AModel.PatternHeight - 1 do
        for PX := 0 to AModel.PatternWidth - 1 do
        begin
          if AGrid.Boundary = wmbWrap then
          begin
            LOutputX := SafeModuloAdd(X, PX, AOutput.Width);
            LOutputY := SafeModuloAdd(Y, PY, AOutput.Height);
          end
          else
          begin
            LOutputX := X + PX;
            LOutputY := Y + PY;
          end;
          LOutputIndex := LOutputY * AOutput.Width + LOutputX;
          LExpected := AModel.PaletteTokenAt(
            AModel.PatternPaletteIndexAt(LPattern, PX, PY));
          if LAssigned[LOutputIndex] then
          begin
            if AOutput.Tokens[LOutputIndex] <> LExpected then
            begin
              AReport.Issue.X := X;
              AReport.Issue.Y := Y;
              AReport.Issue.PatternIndex := LPattern;
              AReport.Issue.PatternOffsetX := PX;
              AReport.Issue.PatternOffsetY := PY;
              AReport.Issue.ExpectedPaletteIndex :=
                AModel.PatternPaletteIndexAt(LPattern, PX, PY);
              AReport.Issue.ActualPaletteIndex :=
                AModel.FindPaletteToken(AOutput.Tokens[LOutputIndex]);
              Exit(InvalidReport(AReport, woikProjectionToken));
            end;
          end
          else
          begin
            LAssigned[LOutputIndex] := True;
            AOutput.Tokens[LOutputIndex] := LExpected;
          end;
          Inc(LWriteCount);
        end;
    end;
  AReport.CheckedProjectionCells := LWriteCount;
  AReport.Valid := True;
  AReport.Issue.Kind := woikNone;
  Result := True;
end;

function ValidateOverlappingProjection2D(
  const AModel: TWfcOverlappingModel2D;
  const AGrid: TWfcPatternGrid2D;
  const AOutput: TWfcTokenGrid2D;
  out AReport: TWfcOverlapping2DValidationReport): Boolean;
var
  LExpectedHeight: Integer;
  LExpectedPalette: Integer;
  LExpectedWidth: Integer;
  LOutputIndex: Integer;
  LOutputX: Integer;
  LOutputY: Integer;
  LPattern: Integer;
  PX: Integer;
  PY: Integer;
  X: Integer;
  Y: Integer;
begin
  if not ValidateOverlappingPatternGrid2D(AModel, AGrid, AReport) then
    Exit(False);
  if not ProjectionDimensions(AModel, AGrid,
      LExpectedWidth, LExpectedHeight) or
      (AOutput.Width <> LExpectedWidth) or
      (AOutput.Height <> LExpectedHeight) or
      (AOutput.Width < 1) or (AOutput.Height < 1) or
      (AOutput.Width > High(Integer) div AOutput.Height) or
      (Length(AOutput.Tokens) <> AOutput.Width * AOutput.Height) or
      ((AGrid.Width * AGrid.Height) >
        High(Integer) div (AModel.PatternWidth * AModel.PatternHeight)) then
    Exit(InvalidReport(AReport, woikProjectionShape));

  for Y := 0 to AGrid.Height - 1 do
    for X := 0 to AGrid.Width - 1 do
    begin
      LPattern := AGrid.Patterns[Y * AGrid.Width + X];
      for PY := 0 to AModel.PatternHeight - 1 do
        for PX := 0 to AModel.PatternWidth - 1 do
        begin
          if AGrid.Boundary = wmbWrap then
          begin
            LOutputX := SafeModuloAdd(X, PX, AOutput.Width);
            LOutputY := SafeModuloAdd(Y, PY, AOutput.Height);
          end
          else
          begin
            LOutputX := X + PX;
            LOutputY := Y + PY;
          end;
          LOutputIndex := LOutputY * AOutput.Width + LOutputX;
          LExpectedPalette := AModel.PatternPaletteIndexAt(
            LPattern, PX, PY);
          CheckedReportIncrement(AReport.CheckedProjectionCells);
          if AOutput.Tokens[LOutputIndex] <>
              AModel.PaletteTokenAt(LExpectedPalette) then
          begin
            AReport.Issue.X := X;
            AReport.Issue.Y := Y;
            AReport.Issue.PatternIndex := LPattern;
            AReport.Issue.PatternOffsetX := PX;
            AReport.Issue.PatternOffsetY := PY;
            AReport.Issue.ExpectedPaletteIndex := LExpectedPalette;
            AReport.Issue.ActualPaletteIndex :=
              AModel.FindPaletteToken(AOutput.Tokens[LOutputIndex]);
            Exit(InvalidReport(AReport, woikProjectionToken));
          end;
        end;
    end;
  AReport.Valid := True;
  AReport.Issue.Kind := woikNone;
  Result := True;
end;

function DescribeOverlapping2DIssue(
  const AIssue: TWfcOverlapping2DIssue): String;
begin
  case AIssue.Kind of
    woikNone:
      Result := 'no overlapping-pattern issue';
    woikGridShape:
      Result := 'the pattern grid has an invalid shape or boundary';
    woikPatternCount:
      Result := 'the pattern grid cell count does not match its shape';
    woikPatternIndex:
      Result := Format('pattern index %d is invalid at (%d,%d)',
        [AIssue.PatternIndex, AIssue.X, AIssue.Y]);
    woikEmptyGraphCell:
      Result := Format('the solved pattern graph is empty at (%d,%d)',
        [AIssue.X, AIssue.Y]);
    woikUnknownPatternKey:
      Result := Format('unknown pattern key "%s" at (%d,%d)',
        [AIssue.Value, AIssue.X, AIssue.Y]);
    woikOverlap:
      Result := Format(
        'patterns %d and %d have an invalid overlap at (%d,%d) -> (%d,%d)',
        [AIssue.PatternIndex, AIssue.RelatedPatternIndex,
          AIssue.X, AIssue.Y, AIssue.NeighborX, AIssue.NeighborY]);
    woikProjectionShape:
      Result := 'the projected token grid has an invalid shape';
    woikProjectionToken:
      Result := Format(
        'pattern %d projection differs at anchor (%d,%d), offset (%d,%d)',
        [AIssue.PatternIndex, AIssue.X, AIssue.Y,
          AIssue.PatternOffsetX, AIssue.PatternOffsetY]);
  else
    Result := 'unknown overlapping-pattern issue';
  end;
end;

end.
