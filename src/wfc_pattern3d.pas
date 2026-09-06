{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
unit wfc_pattern3d;

{$mode delphi}{$H+}

interface

uses SysUtils, wfc, wfc_model;

const
  WFC_OVERLAPPING_3D_ALGORITHM_VERSION = 1;
  WFC_OVERLAPPING_3D_PROJECTION_VERSION = 1;
  WFC_PATTERN_3D_LIMITS_VERSION = 1;
  WFC_PATTERN_3D_MAX_SOURCE_COUNT = 65536;
  WFC_PATTERN_3D_MAX_SOURCE_DIMENSION = 4194304;
  WFC_PATTERN_3D_MAX_SOURCE_CELL_COUNT = 4194304;
  WFC_PATTERN_3D_MAX_TOTAL_SOURCE_CELL_COUNT = 4194304;
  WFC_PATTERN_3D_MAX_FOOTPRINT_DIMENSION = 4096;
  WFC_PATTERN_3D_MAX_FOOTPRINT_CELL_COUNT = 4096;
  WFC_PATTERN_3D_MAX_PALETTE_COUNT = 4096;
  WFC_PATTERN_3D_MAX_PATTERN_COUNT = 1024;
  WFC_PATTERN_3D_MAX_TOTAL_PATTERN_CELL_COUNT = 4194304;
  WFC_PATTERN_3D_MAX_RELATION_SLOT_COUNT = 6291456;

type
  EWfcOverlapping3D = class(EWfcModel);
  TWfcPattern3DPayload = TWfcModelIntegerArray;
  TWfcPattern3DPayloads = array of TWfcPattern3DPayload;
  TWfcPattern3DIndices = TWfcModelIntegerArray;
  TWfcPatternGrid3D = record
    Width, Height, Depth: Integer;
    { Output topology, independent of the learned source boundary. }
    Boundary: TWfcModelBoundary;
    Patterns: TWfcPattern3DIndices;
  end;
  TWfcTokenGrid3D = record
    Width, Height, Depth: Integer;
    Tokens: TWfcModelTokens;
  end;
  TWfcOverlapping3DIssueKind = (wo3ikNone, wo3ikGridShape, wo3ikPatternCount,
    wo3ikPatternIndex, wo3ikEmptyGraphCell, wo3ikUnknownPatternKey,
    wo3ikOverlap, wo3ikProjectionShape, wo3ikProjectionToken);
  TWfcOverlapping3DIssue = record
    Kind: TWfcOverlapping3DIssueKind;
    X, Y, Z, NeighborX, NeighborY, NeighborZ: Integer;
    HasDirection: Boolean;
    Direction: TWfcModelDirection;
    PatternIndex, RelatedPatternIndex: Integer;
    PatternOffsetX, PatternOffsetY, PatternOffsetZ: Integer;
    ExpectedPaletteIndex, ActualPaletteIndex: Integer;
    Value: TGraphValue;
  end;
  TWfcOverlapping3DValidationReport = record
    Valid: Boolean;
    CheckedPatterns, CheckedRelations, CheckedProjectionCells: Integer;
    Issue: TWfcOverlapping3DIssue;
  end;

  { Immutable X-fast, then Y, then Z footprints. CompiledModel is a borrowed
    immutable reference owned by this wrapper. Exact latent keys bind each
    footprint's dimensions and actual token payload, not just an index/hash. }
  TWfcOverlappingModel3D = class
  strict private
    FPatternWidth, FPatternHeight, FPatternDepth: Integer;
    FPalette: TWfcModelTokens;
    FPatterns: TWfcPattern3DPayloads;
    FFaceClasses: TWfcModelIntegerArray;
    FCompiledModel: TWfcModel;
    function GetSourceBoundary: TWfcModelBoundary;
    function GetSymmetry: TWfcModelSymmetry;
    function GetSourceCount: Integer;
    function GetPaletteCount: Integer;
    function GetPatternCount: Integer;
    procedure ValidatePaletteIndex(const AIndex: Integer);
    procedure ValidatePatternIndex(const AIndex: Integer);
    procedure BuildFaceClasses;
  public
    constructor Create(const APatternWidth, APatternHeight, APatternDepth: Integer;
      const ASourceBoundary: TWfcModelBoundary; const ASymmetry: TWfcModelSymmetry;
      const ASourceShapes: TWfcModelSampleShapes; const APalette: TWfcModelTokens;
      const APatterns: TWfcPattern3DPayloads; const APatternWeights: TWfcModelIntegerArray);
    destructor Destroy; override;
    function SourceShapeAt(const ASourceIndex: Integer): TWfcModelSampleShape;
    function PaletteTokenAt(const APaletteIndex: Integer): TWfcModelToken;
    function FindPaletteToken(const AToken: TWfcModelToken): Integer;
    function PatternPaletteIndexAt(const APatternIndex, AX, AY, AZ: Integer): Integer;
    function PatternWeightAt(const APatternIndex: Integer): Integer;
    function PatternKeyAt(const APatternIndex: Integer): TWfcModelToken;
    function FindPatternKey(const AKey: TWfcModelToken): Integer;
    function PatternsCompatible(const ASourcePattern, ATargetPattern: Integer;
      const ADirection: TWfcModelDirection): Boolean;
    function CopySourceShapes: TWfcModelSampleShapes;
    function CopyPalette: TWfcModelTokens;
    function CopyPattern(const APatternIndex: Integer): TWfcPattern3DPayload;
    function CopyPatterns: TWfcPattern3DPayloads;
    function CopyPatternWeights: TWfcModelIntegerArray;
    property PatternWidth: Integer read FPatternWidth;
    property PatternHeight: Integer read FPatternHeight;
    property PatternDepth: Integer read FPatternDepth;
    property SourceBoundary: TWfcModelBoundary read GetSourceBoundary;
    property Symmetry: TWfcModelSymmetry read GetSymmetry;
    property SourceCount: Integer read GetSourceCount;
    property PaletteCount: Integer read GetPaletteCount;
    property PatternCount: Integer read GetPatternCount;
    property CompiledModel: TWfcModel read FCompiledModel;
  end;

function MakeWfcPatternGrid3D(const AWidth, AHeight, ADepth: Integer;
  const ABoundary: TWfcModelBoundary;
  const APatterns: TWfcPattern3DIndices): TWfcPatternGrid3D;
{ Exact selected empty graph pass: no reshape, relink, or pass selection. }
procedure ApplyOverlappingModel3DToGraph(const AModel: TWfcOverlappingModel3D;
  const AGraph: TGraph);
{ Captures the exact full XYZ graph object, not a selected slice. }
function CaptureSolvedPatternGrid3D(const AModel: TWfcOverlappingModel3D;
  const AGraph: TGraph; out AGrid: TWfcPatternGrid3D;
  out AReport: TWfcOverlapping3DValidationReport): Boolean;
function ValidateOverlappingPatternGrid3D(const AModel: TWfcOverlappingModel3D;
  const AGrid: TWfcPatternGrid3D;
  out AReport: TWfcOverlapping3DValidationReport): Boolean;
function TryProjectOverlappingPatternGrid3D(const AModel: TWfcOverlappingModel3D;
  const AGrid: TWfcPatternGrid3D; out AOutput: TWfcTokenGrid3D;
  out AReport: TWfcOverlapping3DValidationReport): Boolean;
function ValidateOverlappingProjection3D(const AModel: TWfcOverlappingModel3D;
  const AGrid: TWfcPatternGrid3D; const AOutput: TWfcTokenGrid3D;
  out AReport: TWfcOverlapping3DValidationReport): Boolean;
function DescribeOverlapping3DIssue(const AIssue: TWfcOverlapping3DIssue): String;

implementation

uses wfc_volume_symmetry, wfc_text_codec {$IFDEF PAS2JS}, JS{$ENDIF};

const
  VOLUME_DIRECTIONS: TWfcModelDirections =
    [wmdNorth,wmdEast,wmdSouth,wmdWest,wmdUp,wmdDown];
  OPPOSITE: array[0..5] of Integer = (2,3,0,1,5,4);

type
  TBooleans = array of Boolean;
  TStrings = array of String;
  THashes = array of Cardinal;

function IntegerInRange(const V, Minimum, Maximum: Integer): Boolean;
begin
  {$IFDEF PAS2JS}
  asm
    return typeof V === 'number' && isFinite(V) && Math.floor(V) === V &&
      V >= Minimum && V <= Maximum;
  end;
  {$ELSE}
  Result := (V >= Minimum) and (V <= Maximum);
  {$ENDIF}
end;

procedure RequireInteger(const V, Minimum, Maximum: Integer; const Name: String);
begin
  if not IntegerInRange(V,Minimum,Maximum) then
    raise EWfcOverlapping3D.Create(Name + ' must be an exact integer in range');
end;

function ValidBoundary(const B: TWfcModelBoundary): Boolean;
begin Result := IntegerInRange(Ord(B),Ord(wmbOpen),Ord(wmbWrap)); end;

function VolumeSize(const W,H,D,MaxDimension,MaxCells: Integer; const Name: String): Integer;
begin
  RequireInteger(W,1,MaxDimension,Name + ' width');
  RequireInteger(H,1,MaxDimension,Name + ' height');
  RequireInteger(D,1,MaxDimension,Name + ' depth');
  if W > MaxCells div H then raise EWfcOverlapping3D.Create(Name + ' cells exceed the limit');
  Result := W * H;
  if Result > MaxCells div D then raise EWfcOverlapping3D.Create(Name + ' cells exceed the limit');
  Result := Result * D;
end;

function GridSize(const W,H,D: Integer; out Cells: Integer): Boolean;
begin
  Cells := 0;
  if not IntegerInRange(W,1,High(Integer)) or
    not IntegerInRange(H,1,High(Integer)) or not IntegerInRange(D,1,High(Integer)) then Exit(False);
  if W > High(Integer) div H then Exit(False);
  Cells := W * H;
  if Cells > High(Integer) div D then begin Cells := 0; Exit(False); end;
  Cells := Cells * D; Result := True;
end;

function Add(const A,B: Integer; const Name: String): Integer;
begin
  if A > High(Integer) - B then raise EWfcOverlapping3D.Create(Name + ' exceeds Integer capacity');
  Result := A + B;
end;

procedure RequireToken(const T: TWfcModelToken);
{$IFDEF PAS2JS}var IsString: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm IsString = typeof T === 'string'; end;
  if not IsString then raise EWfcOverlapping3D.Create('palette token must be a string');
  {$ENDIF}
  if not WfcModelTokenIsValid(T) then
    raise EWfcOverlapping3D.Create('palette token must be nonempty Unicode-scalar text');
end;

function HashValue(const H: Cardinal; const V: Integer): Cardinal;
{$PUSH}{$Q-}
begin Result := ((H shl 5) + H + Cardinal(V) + 1) and Cardinal($7FFFFFFF); end;
{$POP}

function PayloadHash(const P: TWfcPattern3DPayload): Cardinal;
var I: Integer;
begin
  Result := 5381;
  for I := 0 to High(P) do Result := HashValue(Result,P[I]);
end;

function PayloadEqual(const A,B: TWfcPattern3DPayload): Boolean;
var I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do if A[I] <> B[I] then Exit(False);
  Result := True;
end;

function LookupPayload(const Patterns: TWfcPattern3DPayloads;
  const Slots: TWfcModelIntegerArray; const Hashes: THashes;
  const Payload: TWfcPattern3DPayload; const H: Cardinal): Integer;
var Slot, I: Integer;
begin
  Slot := Integer(H mod Cardinal(Length(Slots)));
  while Slots[Slot] <> 0 do
  begin
    I := Slots[Slot] - 1;
    if (Hashes[I] = H) and PayloadEqual(Patterns[I],Payload) then Exit(I);
    Inc(Slot); if Slot = Length(Slots) then Slot := 0;
  end;
  Result := -1;
end;

function JoinParts(const Parts: TStrings): String;
var I, Total: Integer;
{$IFNDEF PAS2JS}var P,N: Integer;{$ENDIF}
begin
  Total := 0;
  for I := 0 to High(Parts) do
  begin
    if Length(Parts[I]) > High(Integer) - Total then
      raise EWfcOverlapping3D.Create('exact pattern key exceeds Integer capacity');
    Inc(Total,Integer(Length(Parts[I])));
  end;
  {$IFDEF PAS2JS}
  Result := TJSArray(Parts).join('');
  {$ELSE}
  SetLength(Result,Total); P := 1;
  for I := 0 to High(Parts) do
  begin
    N := Length(Parts[I]);
    if N > 0 then begin Move(Parts[I][1],Result[P],N); Inc(P,N); end;
  end;
  {$ENDIF}
end;

function ExactPatternKeyLength(const W,H,D: Integer; const EncodedPalette: TStrings;
  const Payload: TWfcPattern3DPayload): Integer;
var Map, Dictionary: TWfcModelIntegerArray; I,N,V,TokenLength: Integer;
begin
  { Compute exact ASCII bytes without materializing a key. A compact source
    can reuse one large palette token in many patterns; its individual keys
    may fit Integer while their aggregate representation does not. }
  SetLength(Map,Length(EncodedPalette));
  for I := 0 to High(Map) do Map[I] := -1;
  SetLength(Dictionary,Length(Payload)); N := 0;
  for I := 0 to High(Payload) do
  begin
    V := Payload[I]; if Map[V] >= 0 then Continue;
    Map[V] := N; Dictionary[N] := V; Inc(N);
  end;
  Result := 15 + Length(IntToStr(W)) + Length(IntToStr(H)) + Length(IntToStr(D)) + 3;
  Result := Add(Result,Length(IntToStr(N)) + 1,'exact pattern key');
  for I := 0 to N - 1 do
  begin
    if Length(EncodedPalette[Dictionary[I]]) > High(Integer) then
      raise EWfcOverlapping3D.Create('encoded palette token exceeds Integer capacity');
    TokenLength := Length(EncodedPalette[Dictionary[I]]);
    Result := Add(Result,Length(IntToStr(TokenLength)) + 2,'exact pattern key');
    Result := Add(Result,TokenLength,'exact pattern key');
  end;
  Result := Add(Result,Length(IntToStr(Length(Payload))) + 1,'exact pattern key');
  for I := 0 to High(Payload) do
    Result := Add(Result,Length(IntToStr(Map[Payload[I]])) + 1,'exact pattern key');
end;

function ExactPatternKey(const W,H,D: Integer; const EncodedPalette: TStrings;
  const Payload: TWfcPattern3DPayload): TWfcModelToken;
var Map, Dictionary: TWfcModelIntegerArray; Parts: TStrings;
  I,N,C,V: Integer; Body: String; Hash: Cardinal;
begin
  SetLength(Map,Length(EncodedPalette));
  for I := 0 to High(Map) do Map[I] := -1;
  SetLength(Dictionary,Length(Payload)); N := 0;
  for I := 0 to High(Payload) do
  begin
    V := Payload[I];
    if Map[V] >= 0 then Continue;
    Map[V] := N; Dictionary[N] := V; Inc(N);
  end;
  SetLength(Parts,3 + N * 3 + Length(Payload) * 2);
  Parts[0] := IntToStr(W) + ',' + IntToStr(H) + ',' + IntToStr(D) + ';';
  Parts[1] := IntToStr(N) + ';'; C := 2;
  for I := 0 to N - 1 do
  begin
    V := Dictionary[I];
    Parts[C] := IntToStr(Length(EncodedPalette[V])) + ':'; Inc(C);
    Parts[C] := EncodedPalette[V]; Inc(C); Parts[C] := ';'; Inc(C);
  end;
  Parts[C] := IntToStr(Length(Payload)) + ';'; Inc(C);
  for I := 0 to High(Payload) do
  begin Parts[C] := IntToStr(Map[Payload[I]]); Inc(C); Parts[C] := ','; Inc(C); end;
  Body := JoinParts(Parts);
  if Length(Body) > High(Integer) - 15 then
    raise EWfcOverlapping3D.Create('exact pattern key exceeds Integer capacity');
  { This routing hash only lets ordinary string equality reject different
    long keys early. The complete exact body is always retained and compared;
    hash collisions never merge keys, patterns, or projection semantics. }
  Hash := 5381;
  for I := 1 to Length(Body) do Hash := HashValue(Hash,Ord(Body[I]));
  Result := TWfcModelToken('@p3v1;' + IntToHex(Hash,8) + ';' + Body);
end;

constructor TWfcOverlappingModel3D.Create(const APatternWidth,APatternHeight,
  APatternDepth: Integer; const ASourceBoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry; const ASourceShapes: TWfcModelSampleShapes;
  const APalette: TWfcModelTokens; const APatterns: TWfcPattern3DPayloads;
  const APatternWeights: TWfcModelIntegerArray);
var Footprint, Transforms, Expected, TotalSources, Observed, Cells, Origins, TotalKeyBytes: Integer;
  I,J,K,T,Slot,PatternCountValue,PaletteCountValue,RelationSlots: Integer;
  Shape, FootprintShape: TWfcModelSampleShape; Used: TBooleans;
  Slots, Relations, KeyLengths: TWfcModelIntegerArray; Hashes: THashes;
  Transformed: TWfcPattern3DPayload; EncodedPalette: TStrings; Keys: TWfcModelTokens;
  Direction: TWfcModelDirection;
begin
  inherited Create;
  if not ValidBoundary(ASourceBoundary) then raise EWfcOverlapping3D.Create('unknown source boundary');
  RequireInteger(Ord(ASymmetry),Ord(wmsNone),Ord(wmsCubeFull),'volume symmetry');
  Transforms := WfcVolumeTransformCount(ASymmetry);
  Footprint := VolumeSize(APatternWidth,APatternHeight,APatternDepth,
    WFC_PATTERN_3D_MAX_FOOTPRINT_DIMENSION,WFC_PATTERN_3D_MAX_FOOTPRINT_CELL_COUNT,'pattern footprint');
  if (ASymmetry = wmsD4) and (APatternWidth <> APatternHeight) then
    raise EWfcOverlapping3D.Create('D4 volume footprints must be square in XY');
  if (ASymmetry in [wmsCubeRotations,wmsCubeFull]) and
    ((APatternWidth <> APatternHeight) or (APatternWidth <> APatternDepth)) then
    raise EWfcOverlapping3D.Create('cube-symmetric volume footprints must be cubic');
  RequireInteger(Length(ASourceShapes),1,WFC_PATTERN_3D_MAX_SOURCE_COUNT,'source-shape count');
  Expected := 0; TotalSources := 0;
  for I := 0 to High(ASourceShapes) do
  begin
    Shape := ASourceShapes[I];
    Cells := VolumeSize(Shape.Width,Shape.Height,Shape.Depth,
      WFC_PATTERN_3D_MAX_SOURCE_DIMENSION,WFC_PATTERN_3D_MAX_SOURCE_CELL_COUNT,'source volume');
    if TotalSources > WFC_PATTERN_3D_MAX_TOTAL_SOURCE_CELL_COUNT - Cells then
      raise EWfcOverlapping3D.Create('aggregate source cells exceed the limit');
    Inc(TotalSources,Cells);
    if ASourceBoundary = wmbOpen then
    begin
      if (Shape.Width < APatternWidth) or (Shape.Height < APatternHeight) or
        (Shape.Depth < APatternDepth) then
        raise EWfcOverlapping3D.Create('open source is smaller than the footprint');
      Origins := (Shape.Width - APatternWidth + 1) *
        (Shape.Height - APatternHeight + 1) * (Shape.Depth - APatternDepth + 1);
    end else Origins := Cells;
    if Origins > High(Integer) div Transforms then
      raise EWfcOverlapping3D.Create('transformed observations exceed Integer capacity');
    Expected := Add(Expected,Origins * Transforms,'source observations');
  end;
  PaletteCountValue := Length(APalette);
  RequireInteger(PaletteCountValue,1,WFC_PATTERN_3D_MAX_PALETTE_COUNT,'palette count');
  PatternCountValue := Length(APatterns);
  RequireInteger(PatternCountValue,1,WFC_PATTERN_3D_MAX_PATTERN_COUNT,'pattern count');
  if PatternCountValue > WFC_PATTERN_3D_MAX_TOTAL_PATTERN_CELL_COUNT div Footprint then
    raise EWfcOverlapping3D.Create('aggregate pattern payload exceeds the limit');
  RelationSlots := 6 * PatternCountValue * PatternCountValue;
  if RelationSlots > WFC_PATTERN_3D_MAX_RELATION_SLOT_COUNT then
    raise EWfcOverlapping3D.Create('volume relation slots exceed the limit');
  if Length(APatternWeights) <> PatternCountValue then
    raise EWfcOverlapping3D.Create('pattern weights differ in count from payloads');
  for I := 0 to PaletteCountValue - 1 do
  begin
    RequireToken(APalette[I]);
    for J := 0 to I - 1 do if APalette[I] = APalette[J] then
      raise EWfcOverlapping3D.Create('palette tokens must be unique');
  end;
  { All scalar/source/capacity preflight precedes model working arrays. }
  SetLength(Used,PaletteCountValue); SetLength(Slots,PatternCountValue * 2 + 1);
  SetLength(Hashes,PatternCountValue); Observed := 0;
  for I := 0 to PatternCountValue - 1 do
  begin
    if Length(APatterns[I]) <> Footprint then raise EWfcOverlapping3D.Create('pattern payload size differs from footprint');
    RequireInteger(APatternWeights[I],1,High(Integer),'pattern weight');
    Observed := Add(Observed,APatternWeights[I],'observed pattern weights');
    for J := 0 to Footprint - 1 do
    begin
      K := APatterns[I][J]; RequireInteger(K,0,PaletteCountValue - 1,'pattern palette index'); Used[K] := True;
    end;
    Hashes[I] := PayloadHash(APatterns[I]);
    if LookupPayload(APatterns,Slots,Hashes,APatterns[I],Hashes[I]) >= 0 then
      raise EWfcOverlapping3D.Create('pattern payloads must be structurally unique');
    Slot := Integer(Hashes[I] mod Cardinal(Length(Slots)));
    while Slots[Slot] <> 0 do begin Inc(Slot); if Slot = Length(Slots) then Slot := 0; end;
    Slots[Slot] := I + 1;
  end;
  if Observed <> Expected then raise EWfcOverlapping3D.Create('pattern weights do not match source observations');
  for I := 0 to High(Used) do if not Used[I] then raise EWfcOverlapping3D.Create('palette token is not used by any pattern');
  FootprintShape := MakeWfcModelSampleShape(APatternWidth,APatternHeight,APatternDepth);
  { Full literal orbit closure and raw-weight equality, including stabilizers.
    Hashes only find candidates; complete payload equality is authoritative. }
  if Transforms > 1 then for I := 0 to PatternCountValue - 1 do
    for T := 1 to Transforms - 1 do
    begin
      Transformed := WfcTransformVolumeIntegers(APatterns[I],FootprintShape,
        WfcVolumeTransformAt(ASymmetry,T));
      J := LookupPayload(APatterns,Slots,Hashes,Transformed,PayloadHash(Transformed));
      if J < 0 then raise EWfcOverlapping3D.Create('literal pattern symmetry orbit is incomplete');
      if APatternWeights[J] <> APatternWeights[I] then
        raise EWfcOverlapping3D.Create('literal pattern symmetry orbit weights differ');
    end;
  FPatternWidth := APatternWidth; FPatternHeight := APatternHeight; FPatternDepth := APatternDepth;
  SetLength(EncodedPalette,PaletteCountValue);
  for I := 0 to PaletteCountValue - 1 do
    EncodedPalette[I] := WfcTextEncodeToken(APalette[I],'3D pattern key');
  TotalKeyBytes := 0; SetLength(KeyLengths,PatternCountValue);
  for I := 0 to PatternCountValue - 1 do
  begin
    KeyLengths[I] := ExactPatternKeyLength(APatternWidth,APatternHeight,APatternDepth,
      EncodedPalette,APatterns[I]);
    TotalKeyBytes := Add(TotalKeyBytes,KeyLengths[I],'aggregate exact pattern keys');
  end;
  { Both single-key and aggregate bytes now fit the public Integer numeric
    envelope before any key is built. Physical memory may impose a lower
    limit; the separately bounded artifact text is not a peak-memory bound. }
  SetLength(FPalette,PaletteCountValue);
  for I := 0 to PaletteCountValue - 1 do FPalette[I] := APalette[I];
  SetLength(FPatterns,PatternCountValue); SetLength(Keys,PatternCountValue);
  for I := 0 to PatternCountValue - 1 do
  begin
    SetLength(FPatterns[I],Footprint);
    for J := 0 to Footprint - 1 do FPatterns[I][J] := APatterns[I][J];
    Keys[I] := ExactPatternKey(APatternWidth,APatternHeight,APatternDepth,EncodedPalette,FPatterns[I]);
    if Length(Keys[I]) <> KeyLengths[I] then
      raise EWfcOverlapping3D.Create('exact pattern key length differs from preflight');
  end;
  BuildFaceClasses;
  SetLength(Relations,RelationSlots);
  for Direction := wmdNorth to wmdDown do for I := 0 to PatternCountValue - 1 do
    for J := 0 to PatternCountValue - 1 do
      if FFaceClasses[Ord(Direction) * PatternCountValue + I] =
        FFaceClasses[OPPOSITE[Ord(Direction)] * PatternCountValue + J] then
        Relations[(Ord(Direction) * PatternCountValue + I) * PatternCountValue + J] := 1;
  FCompiledModel := TWfcModel.Create(3,ASourceShapes,ASourceBoundary,ASymmetry,
    VOLUME_DIRECTIONS,Keys,APatternWeights,Relations);
end;

procedure TWfcOverlappingModel3D.BuildFaceClasses;
const PositiveDirection: array[0..2] of Integer = (1,2,4);
  NegativeDirection: array[0..2] of Integer = (3,0,5);
var Axis, P, Side, Slot, Existing, ClassCount, Direction: Integer;
  W,H,D,SlotsCount: Integer; Slots, Representatives, RepresentativeSides: TWfcModelIntegerArray;
  Hashes: THashes; Hash: Cardinal;

  function FaceHash(const Pattern, HighSide: Integer): Cardinal;
  var X,Y,Z,Offset,Index: Integer;
  begin
    Result := 5381;
    Offset := 0;
    if HighSide <> 0 then
      case Axis of 0: Offset := 1; 1: Offset := FPatternWidth;
        2: Offset := FPatternWidth * FPatternHeight; end;
    for Z := 0 to D - 1 do for Y := 0 to H - 1 do for X := 0 to W - 1 do
    begin
      Index := (Z * FPatternHeight + Y) * FPatternWidth + X + Offset;
      Result := HashValue(Result,FPatterns[Pattern][Index]);
    end;
  end;

  function FacesEqual(const A,ASide,B,BSide: Integer): Boolean;
  var X,Y,Z,AO,BO,Step,Index: Integer;
  begin
    case Axis of 0: Step := 1; 1: Step := FPatternWidth;
      2: Step := FPatternWidth * FPatternHeight; else Step := 0; end;
    AO := ASide * Step; BO := BSide * Step;
    for Z := 0 to D - 1 do for Y := 0 to H - 1 do for X := 0 to W - 1 do
    begin
      Index := (Z * FPatternHeight + Y) * FPatternWidth + X;
      if FPatterns[A][Index + AO] <> FPatterns[B][Index + BO] then Exit(False);
    end;
    Result := True;
  end;

begin
  { Intern overlap slabs once, without materializing copies of their cells.
    Matrix construction subsequently compares exact class IDs, never P² full
    footprints. Per-axis classes include both low and high slabs; a unit axis
    has an empty slab, so all patterns are compatible in that direction. }
  SetLength(FFaceClasses,PatternCount * 6); SlotsCount := PatternCount * 4 + 1;
  SetLength(Representatives,PatternCount * 2); SetLength(RepresentativeSides,PatternCount * 2);
  SetLength(Hashes,PatternCount * 2);
  for Axis := 0 to 2 do
  begin
    SetLength(Slots,0); SetLength(Slots,SlotsCount); ClassCount := 0;
    W := FPatternWidth; H := FPatternHeight; D := FPatternDepth;
    case Axis of 0: Dec(W); 1: Dec(H); 2: Dec(D); end;
    for P := 0 to PatternCount - 1 do for Side := 0 to 1 do
    begin
      Hash := FaceHash(P,Side); Slot := Integer(Hash mod Cardinal(SlotsCount));
      while Slots[Slot] <> 0 do
      begin
        Existing := Slots[Slot] - 1;
        if (Hashes[Existing] = Hash) and
          FacesEqual(P,Side,Representatives[Existing],RepresentativeSides[Existing]) then Break;
        Inc(Slot); if Slot = SlotsCount then Slot := 0;
      end;
      if Slots[Slot] = 0 then
      begin
        Existing := ClassCount; Inc(ClassCount); Slots[Slot] := Existing + 1;
        Representatives[Existing] := P; RepresentativeSides[Existing] := Side; Hashes[Existing] := Hash;
      end else Existing := Slots[Slot] - 1;
      if Side = 0 then Direction := NegativeDirection[Axis] else Direction := PositiveDirection[Axis];
      FFaceClasses[Direction * PatternCount + P] := Existing;
    end;
  end;
end;

destructor TWfcOverlappingModel3D.Destroy;
begin FCompiledModel.Free; inherited Destroy; end;
function TWfcOverlappingModel3D.GetSourceBoundary: TWfcModelBoundary;
begin Result := FCompiledModel.Boundary; end;
function TWfcOverlappingModel3D.GetSymmetry: TWfcModelSymmetry;
begin Result := FCompiledModel.Symmetry; end;
function TWfcOverlappingModel3D.GetSourceCount: Integer;
begin Result := FCompiledModel.SampleCount; end;
function TWfcOverlappingModel3D.GetPaletteCount: Integer;
begin Result := Length(FPalette); end;
function TWfcOverlappingModel3D.GetPatternCount: Integer;
begin Result := Length(FPatterns); end;
procedure TWfcOverlappingModel3D.ValidatePaletteIndex(const AIndex: Integer);
begin RequireInteger(AIndex,0,PaletteCount - 1,'palette index'); end;
procedure TWfcOverlappingModel3D.ValidatePatternIndex(const AIndex: Integer);
begin RequireInteger(AIndex,0,PatternCount - 1,'pattern index'); end;
function TWfcOverlappingModel3D.SourceShapeAt(const ASourceIndex: Integer): TWfcModelSampleShape;
begin RequireInteger(ASourceIndex,0,SourceCount - 1,'source index'); Result := FCompiledModel.SampleShapeAt(ASourceIndex); end;
function TWfcOverlappingModel3D.PaletteTokenAt(const APaletteIndex: Integer): TWfcModelToken;
begin ValidatePaletteIndex(APaletteIndex); Result := FPalette[APaletteIndex]; end;
function TWfcOverlappingModel3D.FindPaletteToken(const AToken: TWfcModelToken): Integer;
var I: Integer;
begin
  for I := 0 to PaletteCount - 1 do if FPalette[I] = AToken then Exit(I);
  Result := -1;
end;
function TWfcOverlappingModel3D.PatternPaletteIndexAt(const APatternIndex,AX,AY,AZ: Integer): Integer;
begin
  ValidatePatternIndex(APatternIndex);
  RequireInteger(AX,0,FPatternWidth - 1,'pattern X');
  RequireInteger(AY,0,FPatternHeight - 1,'pattern Y');
  RequireInteger(AZ,0,FPatternDepth - 1,'pattern Z');
  Result := FPatterns[APatternIndex][(AZ * FPatternHeight + AY) * FPatternWidth + AX];
end;
function TWfcOverlappingModel3D.PatternWeightAt(const APatternIndex: Integer): Integer;
begin ValidatePatternIndex(APatternIndex); Result := FCompiledModel.WeightAt(APatternIndex); end;
function TWfcOverlappingModel3D.PatternKeyAt(const APatternIndex: Integer): TWfcModelToken;
begin ValidatePatternIndex(APatternIndex); Result := FCompiledModel.TokenAt(APatternIndex); end;
function TWfcOverlappingModel3D.FindPatternKey(const AKey: TWfcModelToken): Integer;
begin Result := FCompiledModel.FindToken(AKey); end;
function TWfcOverlappingModel3D.PatternsCompatible(const ASourcePattern,ATargetPattern: Integer;
  const ADirection: TWfcModelDirection): Boolean;
begin
  ValidatePatternIndex(ASourcePattern); ValidatePatternIndex(ATargetPattern);
  RequireInteger(Ord(ADirection),Ord(wmdNorth),Ord(wmdDown),'overlap direction');
  Result := FFaceClasses[Ord(ADirection) * PatternCount + ASourcePattern] =
    FFaceClasses[OPPOSITE[Ord(ADirection)] * PatternCount + ATargetPattern];
end;
function TWfcOverlappingModel3D.CopySourceShapes: TWfcModelSampleShapes;
begin Result := FCompiledModel.CopySampleShapes; end;
function TWfcOverlappingModel3D.CopyPalette: TWfcModelTokens;
var I: Integer;
begin Result := nil; SetLength(Result,PaletteCount); for I := 0 to High(Result) do Result[I] := FPalette[I]; end;
function TWfcOverlappingModel3D.CopyPattern(const APatternIndex: Integer): TWfcPattern3DPayload;
var I: Integer;
begin
  ValidatePatternIndex(APatternIndex); Result := nil; SetLength(Result,Length(FPatterns[APatternIndex]));
  for I := 0 to High(Result) do Result[I] := FPatterns[APatternIndex][I];
end;
function TWfcOverlappingModel3D.CopyPatterns: TWfcPattern3DPayloads;
var I: Integer;
begin Result := nil; SetLength(Result,PatternCount); for I := 0 to High(Result) do Result[I] := CopyPattern(I); end;
function TWfcOverlappingModel3D.CopyPatternWeights: TWfcModelIntegerArray;
begin Result := FCompiledModel.CopyWeights; end;

procedure InitializeReport(out R: TWfcOverlapping3DValidationReport);
begin
  R := Default(TWfcOverlapping3DValidationReport);
  R.Issue.Kind := wo3ikNone;
  R.Issue.X := -1; R.Issue.Y := -1; R.Issue.Z := -1;
  R.Issue.NeighborX := -1; R.Issue.NeighborY := -1; R.Issue.NeighborZ := -1;
  R.Issue.PatternIndex := -1; R.Issue.RelatedPatternIndex := -1;
  R.Issue.PatternOffsetX := -1; R.Issue.PatternOffsetY := -1; R.Issue.PatternOffsetZ := -1;
  R.Issue.ExpectedPaletteIndex := -1; R.Issue.ActualPaletteIndex := -1;
end;

function Invalid(var R: TWfcOverlapping3DValidationReport;
  const Kind: TWfcOverlapping3DIssueKind): Boolean;
begin R.Valid := False; R.Issue.Kind := Kind; Result := False; end;

procedure Count(var N: Integer);
begin
  if N = High(Integer) then raise EWfcOverlapping3D.Create('validation count exceeds Integer capacity');
  Inc(N);
end;

procedure RequireModel(const M: TWfcOverlappingModel3D);
begin if not Assigned(M) then raise EWfcOverlapping3D.Create('overlapping volume model must be assigned'); end;

function MakeWfcPatternGrid3D(const AWidth,AHeight,ADepth: Integer;
  const ABoundary: TWfcModelBoundary;
  const APatterns: TWfcPattern3DIndices): TWfcPatternGrid3D;
var I,N: Integer;
begin
  if not ValidBoundary(ABoundary) or not GridSize(AWidth,AHeight,ADepth,N) then
    raise EWfcOverlapping3D.Create('pattern grid shape or boundary is invalid');
  if Length(APatterns) <> N then raise EWfcOverlapping3D.Create('pattern-grid count differs from shape');
  for I := 0 to N - 1 do RequireInteger(APatterns[I],0,High(Integer),'grid pattern index');
  Result.Width := AWidth; Result.Height := AHeight; Result.Depth := ADepth;
  Result.Boundary := ABoundary; SetLength(Result.Patterns,N);
  for I := 0 to N - 1 do Result.Patterns[I] := APatterns[I];
end;

procedure ApplyOverlappingModel3DToGraph(const AModel: TWfcOverlappingModel3D;
  const AGraph: TGraph);
begin
  RequireModel(AModel);
  if not Assigned(AGraph) then raise EWfcOverlapping3D.Create('target graph pass must be assigned');
  ApplyModelToGraph(AModel.CompiledModel,AGraph);
end;

function ValidateOverlappingPatternGrid3D(const AModel: TWfcOverlappingModel3D;
  const AGrid: TWfcPatternGrid3D;
  out AReport: TWfcOverlapping3DValidationReport): Boolean;
var N,I,P,Q,X,Y,Z,NX,NY,NZ: Integer;

  function Relation(const Direction: TWfcModelDirection;
    const NeighborX,NeighborY,NeighborZ: Integer): Boolean;
  begin
    Q := AGrid.Patterns[(NeighborZ * AGrid.Height + NeighborY) * AGrid.Width + NeighborX];
    Count(AReport.CheckedRelations);
    if AModel.PatternsCompatible(P,Q,Direction) then Exit(True);
    AReport.Issue.X := X; AReport.Issue.Y := Y; AReport.Issue.Z := Z;
    AReport.Issue.NeighborX := NeighborX; AReport.Issue.NeighborY := NeighborY;
    AReport.Issue.NeighborZ := NeighborZ; AReport.Issue.HasDirection := True;
    AReport.Issue.Direction := Direction; AReport.Issue.PatternIndex := P;
    AReport.Issue.RelatedPatternIndex := Q;
    Result := Invalid(AReport,wo3ikOverlap);
  end;

begin
  RequireModel(AModel); InitializeReport(AReport);
  if not ValidBoundary(AGrid.Boundary) or not GridSize(AGrid.Width,AGrid.Height,AGrid.Depth,N) then
    Exit(Invalid(AReport,wo3ikGridShape));
  if Length(AGrid.Patterns) <> N then Exit(Invalid(AReport,wo3ikPatternCount));
  for I := 0 to N - 1 do
  begin
    P := AGrid.Patterns[I];
    if not IntegerInRange(P,0,AModel.PatternCount - 1) then
    begin
      AReport.Issue.X := I mod AGrid.Width;
      AReport.Issue.Y := (I div AGrid.Width) mod AGrid.Height;
      AReport.Issue.Z := (I div AGrid.Width) div AGrid.Height;
      AReport.Issue.PatternIndex := P; Exit(Invalid(AReport,wo3ikPatternIndex));
    end;
    Count(AReport.CheckedPatterns);
  end;
  for Z := 0 to AGrid.Depth - 1 do for Y := 0 to AGrid.Height - 1 do
    for X := 0 to AGrid.Width - 1 do
    begin
      P := AGrid.Patterns[(Z * AGrid.Height + Y) * AGrid.Width + X];
      if X < AGrid.Width - 1 then NX := X + 1
      else if AGrid.Boundary = wmbWrap then NX := 0 else NX := -1;
      if (NX >= 0) and not Relation(wmdEast,NX,Y,Z) then Exit(False);
      if Y < AGrid.Height - 1 then NY := Y + 1
      else if AGrid.Boundary = wmbWrap then NY := 0 else NY := -1;
      if (NY >= 0) and not Relation(wmdSouth,X,NY,Z) then Exit(False);
      if Z < AGrid.Depth - 1 then NZ := Z + 1
      else if AGrid.Boundary = wmbWrap then NZ := 0 else NZ := -1;
      if (NZ >= 0) and not Relation(wmdUp,X,Y,NZ) then Exit(False);
    end;
  AReport.Valid := True; Result := True;
end;

function CaptureSolvedPatternGrid3D(const AModel: TWfcOverlappingModel3D;
  const AGraph: TGraph; out AGrid: TWfcPatternGrid3D;
  out AReport: TWfcOverlapping3DValidationReport): Boolean;
var Captured: TWfcPatternGrid3D; N,P,X,Y,Z: Integer;
  E: TGraphEntry; Key: TWfcModelToken;
  Graph: TGraph;
begin
  RequireModel(AModel);
  if not Assigned(AGraph) then raise EWfcOverlapping3D.Create('source graph pass must be assigned');
  //Resolve once: a root's default dimensions/wrapping need not describe its
  //currently selected pass, while graph entry access is already pass-scoped.
  Graph := AGraph.PassGraph[AGraph.CurrentPassIndex];
  AGrid := Default(TWfcPatternGrid3D); Captured := Default(TWfcPatternGrid3D);
  InitializeReport(AReport);
  if (Graph.Dimension.Width > TGraphCoordinate(High(Integer))) or
    (Graph.Dimension.Height > TGraphCoordinate(High(Integer))) or
    (Graph.Dimension.Depth > TGraphCoordinate(High(Integer))) then
    Exit(Invalid(AReport,wo3ikGridShape));
  if not GridSize(Integer(Graph.Dimension.Width),Integer(Graph.Dimension.Height),
    Integer(Graph.Dimension.Depth),N) then Exit(Invalid(AReport,wo3ikGridShape));
  Captured.Width := Integer(Graph.Dimension.Width);
  Captured.Height := Integer(Graph.Dimension.Height);
  Captured.Depth := Integer(Graph.Dimension.Depth);
  if Graph.WrapNeighbors then Captured.Boundary := wmbWrap else Captured.Boundary := wmbOpen;
  SetLength(Captured.Patterns,N);
  for Z := 0 to Captured.Depth - 1 do for Y := 0 to Captured.Height - 1 do
    for X := 0 to Captured.Width - 1 do
    begin
      E := Graph.Entry[TGraphCoordinate(X),TGraphCoordinate(Y),TGraphCoordinate(Z)];
      if not Assigned(E) or E.Empty then
      begin
        AReport.Issue.X := X; AReport.Issue.Y := Y; AReport.Issue.Z := Z;
        Exit(Invalid(AReport,wo3ikEmptyGraphCell));
      end;
      {$IFDEF PAS2JS}Key := TWfcModelToken(E.Value);
      {$ELSE}Key := UTF8Encode(UnicodeString(E.Value));{$ENDIF}
      P := AModel.FindPatternKey(Key);
      if P < 0 then
      begin
        AReport.Issue.X := X; AReport.Issue.Y := Y; AReport.Issue.Z := Z;
        AReport.Issue.Value := E.Value; Exit(Invalid(AReport,wo3ikUnknownPatternKey));
      end;
      Captured.Patterns[(Z * Captured.Height + Y) * Captured.Width + X] := P;
    end;
  Result := ValidateOverlappingPatternGrid3D(AModel,Captured,AReport);
  if Result then AGrid := Captured;
end;

function ProjectionShape(const M: TWfcOverlappingModel3D; const G: TWfcPatternGrid3D;
  out W,H,D,Cells,Writes: Integer): Boolean;
var GridCells,Footprint: Integer;
begin
  W := 0; H := 0; D := 0; Cells := 0; Writes := 0;
  if G.Boundary = wmbWrap then begin W := G.Width; H := G.Height; D := G.Depth; end
  else
  begin
    if (G.Width > High(Integer) - M.PatternWidth + 1) or
      (G.Height > High(Integer) - M.PatternHeight + 1) or
      (G.Depth > High(Integer) - M.PatternDepth + 1) then Exit(False);
    W := G.Width + (M.PatternWidth - 1); H := G.Height + (M.PatternHeight - 1);
    D := G.Depth + (M.PatternDepth - 1);
  end;
  if not GridSize(W,H,D,Cells) or not GridSize(G.Width,G.Height,G.Depth,GridCells) then Exit(False);
  Footprint := M.PatternWidth * M.PatternHeight * M.PatternDepth;
  if GridCells > High(Integer) div Footprint then Exit(False);
  Writes := GridCells * Footprint; Result := True;
end;

function ModuloAdd(const Base,Offset,Modulus: Integer): Integer;
var N: Integer;
begin
  N := Offset mod Modulus;
  if Base >= Modulus - N then Result := Base - (Modulus - N) else Result := Base + N;
end;

procedure ProjectionIssue(var R: TWfcOverlapping3DValidationReport;
  const X,Y,Z,P,PX,PY,PZ,Expected,Actual: Integer);
begin
  R.Issue.X := X; R.Issue.Y := Y; R.Issue.Z := Z; R.Issue.PatternIndex := P;
  R.Issue.PatternOffsetX := PX; R.Issue.PatternOffsetY := PY; R.Issue.PatternOffsetZ := PZ;
  R.Issue.ExpectedPaletteIndex := Expected; R.Issue.ActualPaletteIndex := Actual;
end;

function TryProjectOverlappingPatternGrid3D(const AModel: TWfcOverlappingModel3D;
  const AGrid: TWfcPatternGrid3D; out AOutput: TWfcTokenGrid3D;
  out AReport: TWfcOverlapping3DValidationReport): Boolean;
var Output: TWfcTokenGrid3D; AssignedCells: TBooleans;
  Cells,Writes,X,Y,Z,PX,PY,PZ,OX,OY,OZ,Index,P,V: Integer;
  Expected: TWfcModelToken;
begin
  AOutput := Default(TWfcTokenGrid3D); Output := Default(TWfcTokenGrid3D);
  if not ValidateOverlappingPatternGrid3D(AModel,AGrid,AReport) then Exit(False);
  if not ProjectionShape(AModel,AGrid,Output.Width,Output.Height,Output.Depth,Cells,Writes) then
    Exit(Invalid(AReport,wo3ikProjectionShape));
  SetLength(Output.Tokens,Cells); SetLength(AssignedCells,Cells);
  for Z := 0 to AGrid.Depth - 1 do for Y := 0 to AGrid.Height - 1 do
    for X := 0 to AGrid.Width - 1 do
    begin
      P := AGrid.Patterns[(Z * AGrid.Height + Y) * AGrid.Width + X];
      for PZ := 0 to AModel.PatternDepth - 1 do for PY := 0 to AModel.PatternHeight - 1 do
        for PX := 0 to AModel.PatternWidth - 1 do
        begin
          if AGrid.Boundary = wmbWrap then
          begin
            OX := ModuloAdd(X,PX,Output.Width); OY := ModuloAdd(Y,PY,Output.Height);
            OZ := ModuloAdd(Z,PZ,Output.Depth);
          end else begin OX := X + PX; OY := Y + PY; OZ := Z + PZ; end;
          Index := (OZ * Output.Height + OY) * Output.Width + OX;
          V := AModel.PatternPaletteIndexAt(P,PX,PY,PZ); Expected := AModel.PaletteTokenAt(V);
          Count(AReport.CheckedProjectionCells);
          if AssignedCells[Index] then
          begin
            if Output.Tokens[Index] <> Expected then
            begin
              ProjectionIssue(AReport,X,Y,Z,P,PX,PY,PZ,V,AModel.FindPaletteToken(Output.Tokens[Index]));
              Exit(Invalid(AReport,wo3ikProjectionToken));
            end;
          end else begin AssignedCells[Index] := True; Output.Tokens[Index] := Expected; end;
        end;
    end;
  if AReport.CheckedProjectionCells <> Writes then
    raise EWfcOverlapping3D.Create('projection did not visit its exact write extent');
  AReport.Valid := True; AOutput := Output; Result := True;
end;

function ValidateOverlappingProjection3D(const AModel: TWfcOverlappingModel3D;
  const AGrid: TWfcPatternGrid3D; const AOutput: TWfcTokenGrid3D;
  out AReport: TWfcOverlapping3DValidationReport): Boolean;
var W,H,D,Cells,ActualCells,Writes,X,Y,Z,PX,PY,PZ,OX,OY,OZ,Index,P,V: Integer;
begin
  if not ValidateOverlappingPatternGrid3D(AModel,AGrid,AReport) then Exit(False);
  if not ProjectionShape(AModel,AGrid,W,H,D,Cells,Writes) or
    not GridSize(AOutput.Width,AOutput.Height,AOutput.Depth,ActualCells) then
    Exit(Invalid(AReport,wo3ikProjectionShape));
  if (AOutput.Width <> W) or (AOutput.Height <> H) or (AOutput.Depth <> D) or
    (Length(AOutput.Tokens) <> Cells) then Exit(Invalid(AReport,wo3ikProjectionShape));
  for Z := 0 to AGrid.Depth - 1 do for Y := 0 to AGrid.Height - 1 do
    for X := 0 to AGrid.Width - 1 do
    begin
      P := AGrid.Patterns[(Z * AGrid.Height + Y) * AGrid.Width + X];
      for PZ := 0 to AModel.PatternDepth - 1 do for PY := 0 to AModel.PatternHeight - 1 do
        for PX := 0 to AModel.PatternWidth - 1 do
        begin
          if AGrid.Boundary = wmbWrap then
          begin OX := ModuloAdd(X,PX,W); OY := ModuloAdd(Y,PY,H); OZ := ModuloAdd(Z,PZ,D); end
          else begin OX := X + PX; OY := Y + PY; OZ := Z + PZ; end;
          Index := (OZ * H + OY) * W + OX;
          V := AModel.PatternPaletteIndexAt(P,PX,PY,PZ); Count(AReport.CheckedProjectionCells);
          if AOutput.Tokens[Index] <> AModel.PaletteTokenAt(V) then
          begin
            ProjectionIssue(AReport,X,Y,Z,P,PX,PY,PZ,V,AModel.FindPaletteToken(AOutput.Tokens[Index]));
            Exit(Invalid(AReport,wo3ikProjectionToken));
          end;
        end;
    end;
  AReport.Valid := True; Result := True;
end;

function DescribeOverlapping3DIssue(const AIssue: TWfcOverlapping3DIssue): String;
begin
  RequireInteger(Ord(AIssue.Kind),Ord(wo3ikNone),Ord(wo3ikProjectionToken),'overlapping issue kind');
  case AIssue.Kind of
    wo3ikNone: Result := 'no overlapping-volume issue';
    wo3ikGridShape: Result := 'the pattern volume has an invalid shape or boundary';
    wo3ikPatternCount: Result := 'the pattern-volume count does not match its shape';
    wo3ikPatternIndex: Result := Format('pattern index %d is invalid at (%d,%d,%d)',
      [AIssue.PatternIndex,AIssue.X,AIssue.Y,AIssue.Z]);
    wo3ikEmptyGraphCell: Result := Format('the solved pattern graph is empty at (%d,%d,%d)',
      [AIssue.X,AIssue.Y,AIssue.Z]);
    wo3ikUnknownPatternKey: Result := Format('unknown pattern key "%s" at (%d,%d,%d)',
      [AIssue.Value,AIssue.X,AIssue.Y,AIssue.Z]);
    wo3ikOverlap: Result := Format('patterns %d and %d have an invalid overlap at (%d,%d,%d) -> (%d,%d,%d)',
      [AIssue.PatternIndex,AIssue.RelatedPatternIndex,AIssue.X,AIssue.Y,AIssue.Z,
        AIssue.NeighborX,AIssue.NeighborY,AIssue.NeighborZ]);
    wo3ikProjectionShape: Result := 'the projected token volume has an invalid shape';
    wo3ikProjectionToken: Result := Format('pattern %d projection differs at anchor (%d,%d,%d), offset (%d,%d,%d)',
      [AIssue.PatternIndex,AIssue.X,AIssue.Y,AIssue.Z,AIssue.PatternOffsetX,AIssue.PatternOffsetY,AIssue.PatternOffsetZ]);
    else raise EWfcOverlapping3D.Create('unknown overlapping issue kind');
  end;
end;

end.
