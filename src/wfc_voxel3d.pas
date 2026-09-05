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
unit wfc_voxel3d;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc;

const
  WFC_VOXEL3D_KIT_VERSION = 1;
  WFC_VOXEL3D_ADAPTER_VERSION = 1;
  WFC_VOXEL3D_SCENE_VERSION = 1;
  WFC_VOXEL3D_SIGNATURE_VERSION = 1;

  //Version 1 deliberately uses one small, explicit ASCII token grammar. This
  //keeps model identity and graph keys byte-identical between native String
  //storage and pas2js UTF-16 strings without relying on a host code page.
  WFC_VOXEL3D_MAX_TOKEN_LENGTH = 255;

type
  EVoxel3D = class(Exception);
  EVoxel3DModel = class(EVoxel3D);
  EVoxel3DAdapter = class(EVoxel3D);
  EVoxel3DScene = class(EVoxel3D);

  TVoxel3DSignature = Cardinal;

  //Yaw is clockwise when viewed from above. North therefore becomes East at
  //90 degrees. The vertical directions never change under a yaw rotation.
  TVoxel3DRotation = (
    v3r0,
    v3r90,
    v3r180,
    v3r270
  );
  TVoxel3DRotations = set of TVoxel3DRotation;

const
  ALL_VOXEL3D_ROTATIONS: TVoxel3DRotations =
    [v3r0, v3r90, v3r180, v3r270];

type
  TVoxel3DPrototypeFlag = (
    v3pfEmpty,
    v3pfSolid,
    v3pfRequiresSupport,
    v3pfProvidesSupport,
    v3pfWalkable,
    v3pfEntrance,
    v3pfRequiredReachable
  );
  TVoxel3DPrototypeFlags = set of TVoxel3DPrototypeFlag;

  TVoxel3DSockets = array[TGraphDirection] of String;

  //A prototype is caller-friendly input data. TVoxel3DKit validates and
  //deep-copies the complete array before exposing an immutable model.
  TVoxel3DPrototype = record
    Id: String;
    Material: String;
    Weight: TGraphWeight;
    Sockets: TVoxel3DSockets;
    AllowedRotations: TVoxel3DRotations;
    Flags: TVoxel3DPrototypeFlags;
    WalkOpenings: TGraphDirections;
  end;
  TVoxel3DPrototypes = array of TVoxel3DPrototype;

  //Socket compatibility is an unordered pair. (A,B) and (B,A) are the same
  //declaration; duplicate declarations are rejected by the kit constructor.
  TVoxel3DSocketPair = record
    First: String;
    Second: String;
  end;
  TVoxel3DSocketPairs = array of TVoxel3DSocketPair;

  //Variants are expanded in prototype order, then rotation ordinal. Sockets
  //and walk openings are already rotated, so adapters and renderers never
  //need to reinterpret prototype-local faces.
  TVoxel3DVariant = record
    PrototypeIndex: Integer;
    PrototypeId: String;
    Material: String;
    Weight: TGraphWeight;
    Rotation: TVoxel3DRotation;
    Sockets: TVoxel3DSockets;
    Flags: TVoxel3DPrototypeFlags;
    WalkOpenings: TGraphDirections;
  end;
  TVoxel3DVariants = array of TVoxel3DVariant;

  TVoxel3DVariantIndices = array of Integer;
  TVoxel3DGraphKeys = array of TGraphValue;

  TVoxel3DGraphAdapter = class;
  TVoxel3DScene = class;

  { TVoxel3DKit }

  TVoxel3DKit = class
  private
    FId: String;
    FIdentity: String;
    FPrototypes: TVoxel3DPrototypes;
    FSignature: TVoxel3DSignature;
    FSocketPairs: TVoxel3DSocketPairs;
    FVariants: TVoxel3DVariants;
    FVariantGraphKeys: TVoxel3DGraphKeys;

    function GetPrototypeCount: Integer;
    function GetSocketPairCount: Integer;
    function GetVariantCount: Integer;
    procedure Initialize(const AId: String;
      const APrototypes: TVoxel3DPrototypes;
      const ASocketPairs: TVoxel3DSocketPairs);
  public
    constructor Create(const AId: String;
      const APrototypes: TVoxel3DPrototypes;
      const ASocketPairs: TVoxel3DSocketPairs);

    function PrototypeAt(const AIndex: Integer): TVoxel3DPrototype;
    function SocketPairAt(const AIndex: Integer): TVoxel3DSocketPair;
    function VariantAt(const AIndex: Integer): TVoxel3DVariant;
    function VariantGraphKeyAt(const AIndex: Integer): TGraphValue;
    function FindVariantGraphKey(const AValue: TGraphValue;
      out AVariantIndex: Integer): Boolean;

    function SocketsCompatible(const AFirst, ASecond: String): Boolean;
    function VariantsCompatible(const ASourceIndex: Integer;
      const ADirection: TGraphDirection;
      const ANeighborIndex: Integer): Boolean;

    //Collision-hard semantic comparison for reusable graph bridges. This
    //checks the complete copied model and graph-key map, not just signatures.
    function MatchesAdapter(const AAdapter: TVoxel3DGraphAdapter): Boolean;

    //Performs collision-hard semantic comparison. CRC-32 remains a compact
    //portable signature, never the sole authorization for a scene.
    function MatchesScene(const AScene: TVoxel3DScene): Boolean;

    //The active pass must already have nonzero dimensions and contain no
    //values, rules, entry locks, or caller domains. The returned adapter binds
    //the exact pass object and must be freed before that graph is destroyed.
    function ApplyToGraph(const AGraph: TGraph): TVoxel3DGraphAdapter;

    //Capture verifies both the kit semantics and the exact applied graph
    //definition before accepting a solved scene.
    function CaptureScene(const AAdapter: TVoxel3DGraphAdapter): TVoxel3DScene;

    property Id: String read FId;
    property Identity: String read FIdentity;
    property Signature: TVoxel3DSignature read FSignature;
    property PrototypeCount: Integer read GetPrototypeCount;
    property SocketPairCount: Integer read GetSocketPairCount;
    property VariantCount: Integer read GetVariantCount;
  end;

  { TVoxel3DGraphAdapter }

  TVoxel3DGraphAdapter = class
  private
    FApplicationIdentity: String;
    FAppliedGraph: TGraph;
    FDepth: TGraphCoordinate;
    FGraphKeys: TVoxel3DGraphKeys;
    FHeight: TGraphCoordinate;
    FKitId: String;
    FKitIdentity: String;
    FKitSignature: TVoxel3DSignature;
    FPassIndex: Integer;
    FSocketPairs: TVoxel3DSocketPairs;
    FVariants: TVoxel3DVariants;
    FWidth: TGraphCoordinate;
    FWrapNeighbors: Boolean;

    function Capture: TVoxel3DScene;
    function GetVariantCount: Integer;
    function VariantsCompatible(const ASourceIndex: Integer;
      const ADirection: TGraphDirection;
      const ANeighborIndex: Integer): Boolean;
  public
    //Normally created by TVoxel3DKit.ApplyToGraph. Keeping the checked
    //constructor public also lets serializers reconstruct an adapter-shaped
    //identity without privileged access; capture still verifies the graph.
    constructor Create(const AKit: TVoxel3DKit;
      const AGraph: TGraph);
    function VariantAt(const AIndex: Integer): TVoxel3DVariant;
    function VariantGraphKeyAt(const AIndex: Integer): TGraphValue;
    function FindVariantGraphKey(const AValue: TGraphValue;
      out AVariantIndex: Integer): Boolean;

    //Proves that the bound pass still contains the exact voxel registry,
    //weights, adjacency rows, and explicit denials captured at application.
    //Named cross-pass requirements are additive and do not change this local
    //voxel definition identity.
    function DefinitionMatchesGraph: Boolean;

    property ApplicationIdentity: String read FApplicationIdentity;
    property AppliedGraph: TGraph read FAppliedGraph;
    property Depth: TGraphCoordinate read FDepth;
    property Height: TGraphCoordinate read FHeight;
    property KitId: String read FKitId;
    property KitIdentity: String read FKitIdentity;
    property KitSignature: TVoxel3DSignature read FKitSignature;
    property PassIndex: Integer read FPassIndex;
    property VariantCount: Integer read GetVariantCount;
    property Width: TGraphCoordinate read FWidth;
    property WrapNeighbors: Boolean read FWrapNeighbors;
  end;

  { TVoxel3DScene }

  TVoxel3DScene = class
  private
    FApplicationIdentity: String;
    FDepth: TGraphCoordinate;
    FHeight: TGraphCoordinate;
    FKitId: String;
    FKitIdentity: String;
    FKitSignature: TVoxel3DSignature;
    FSignature: TVoxel3DSignature;
    FSocketPairs: TVoxel3DSocketPairs;
    FVariantIndices: TVoxel3DVariantIndices;
    FVariants: TVoxel3DVariants;
    FWidth: TGraphCoordinate;
    FWrapNeighbors: Boolean;

    function CoordToIndex(const AX, AY,
      AZ: TGraphCoordinate): Integer;
    function GetCellCount: Integer;
    function GetVariantCount: Integer;
  public
    //The constructor deep-copies and checks every index. Ordinary generated
    //scenes should still be obtained through TVoxel3DKit.CaptureScene, which
    //additionally proves the live graph definition and solved cells.
    constructor Create(const AAdapter: TVoxel3DGraphAdapter;
      const AVariantIndices: TVoxel3DVariantIndices);
    function VariantAt(const AIndex: Integer): TVoxel3DVariant;
    function VariantIndexAt(const AX, AY,
      AZ: TGraphCoordinate): Integer;
    function VariantAtCell(const AX, AY,
      AZ: TGraphCoordinate): TVoxel3DVariant;
    function CopyVariantIndices: TVoxel3DVariantIndices;

    property ApplicationIdentity: String read FApplicationIdentity;
    property CellCount: Integer read GetCellCount;
    property Depth: TGraphCoordinate read FDepth;
    property Height: TGraphCoordinate read FHeight;
    property KitId: String read FKitId;
    property KitIdentity: String read FKitIdentity;
    property KitSignature: TVoxel3DSignature read FKitSignature;
    property Signature: TVoxel3DSignature read FSignature;
    property VariantCount: Integer read GetVariantCount;
    property Width: TGraphCoordinate read FWidth;
    property WrapNeighbors: Boolean read FWrapNeighbors;
  end;

function IsVoxel3DToken(const AValue: String): Boolean;
function Voxel3DSignatureHex(const ASignature: TVoxel3DSignature): String;
function Voxel3DRotationDegrees(const ARotation: TVoxel3DRotation): Integer;
function OppositeVoxel3DDirection(
  const ADirection: TGraphDirection): TGraphDirection;
function RotateVoxel3DDirection(const ADirection: TGraphDirection;
  const ARotation: TVoxel3DRotation): TGraphDirection;
function RotateVoxel3DDirections(const ADirections: TGraphDirections;
  const ARotation: TVoxel3DRotation): TGraphDirections;
function RotateVoxel3DSockets(const ASockets: TVoxel3DSockets;
  const ARotation: TVoxel3DRotation): TVoxel3DSockets;
function Voxel3DPrototypeHasFlag(const APrototype: TVoxel3DPrototype;
  const AFlag: TVoxel3DPrototypeFlag): Boolean;
function Voxel3DVariantHasFlag(const AVariant: TVoxel3DVariant;
  const AFlag: TVoxel3DPrototypeFlag): Boolean;

function MakeVoxel3DSockets(const ANorth, AEast, ASouth, AWest,
  AUp, ADown: String): TVoxel3DSockets;
function MakeVoxel3DPrototype(const AId, AMaterial: String;
  const AWeight: TGraphWeight; const ASockets: TVoxel3DSockets;
  const AAllowedRotations: TVoxel3DRotations;
  const AFlags: TVoxel3DPrototypeFlags;
  const AWalkOpenings: TGraphDirections): TVoxel3DPrototype;
function MakeVoxel3DSocketPair(const AFirst,
  ASecond: String): TVoxel3DSocketPair;

implementation

const
  CRC32_POLYNOMIAL = Cardinal($EDB88320);
  HEX_DIGITS = '0123456789ABCDEF';
  KIT_IDENTITY_PREFIX = 'wfcv3-kit-1:';
  ADAPTER_IDENTITY_PREFIX = 'wfcv3-adapter-1:';
  GRAPH_KEY_MARKER = ':variant:';

type
  TVoxel3DGraphRuleMatrix = array of TGraphRules;
  TVoxel3DDirectionSets = array of TGraphDirections;

function IsValidDirection(const ADirection: TGraphDirection): Boolean;
begin
  Result := (Ord(ADirection) >= Ord(Low(TGraphDirection))) and
    (Ord(ADirection) <= Ord(High(TGraphDirection)));
end;

function IsValidRotation(const ARotation: TVoxel3DRotation): Boolean;
begin
  Result := (Ord(ARotation) >= Ord(Low(TVoxel3DRotation))) and
    (Ord(ARotation) <= Ord(High(TVoxel3DRotation)));
end;

procedure RequireDirection(const ADirection: TGraphDirection;
  const AOperation: String);
begin
  if not IsValidDirection(ADirection) then
    raise ERangeError.CreateFmt('%s direction is out of bounds [%d]',
      [AOperation, Ord(ADirection)]);
end;

procedure RequireRotation(const ARotation: TVoxel3DRotation;
  const AOperation: String);
begin
  if not IsValidRotation(ARotation) then
    raise ERangeError.CreateFmt('%s rotation is out of bounds [%d]',
      [AOperation, Ord(ARotation)]);
end;

function IsVoxel3DToken(const AValue: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Length(AValue) = 0) or
      (Length(AValue) > WFC_VOXEL3D_MAX_TOKEN_LENGTH) then
    Exit;
  if not (AValue[1] in ['A'..'Z', 'a'..'z', '0'..'9']) then
    Exit;
  for I := 2 to Length(AValue) do
    if not (AValue[I] in ['A'..'Z', 'a'..'z', '0'..'9',
        '-', '.', '_', '~']) then
      Exit;
  Result := True;
end;

procedure RequireToken(const AValue, ALabel: String);
begin
  if not IsVoxel3DToken(AValue) then
    raise EVoxel3DModel.CreateFmt(
      '%s must be 1..%d portable ASCII token characters',
      [ALabel, WFC_VOXEL3D_MAX_TOKEN_LENGTH]);
end;

function Voxel3DSignatureHex(
  const ASignature: TVoxel3DSignature): String;
var
  I: Integer;
  LValue: Cardinal;
begin
  SetLength(Result, 8);
  LValue := ASignature;
  for I := 8 downto 1 do
  begin
    Result[I] := HEX_DIGITS[Integer(LValue and Cardinal($F)) + 1];
    LValue := LValue shr 4;
  end;
end;

function Voxel3DRotationDegrees(
  const ARotation: TVoxel3DRotation): Integer;
begin
  RequireRotation(ARotation, 'Voxel3DRotationDegrees');
  Result := 0;
  case ARotation of
    v3r0:
      Result := 0;
    v3r90:
      Result := 90;
    v3r180:
      Result := 180;
    v3r270:
      Result := 270;
  end;
end;

function OppositeVoxel3DDirection(
  const ADirection: TGraphDirection): TGraphDirection;
begin
  RequireDirection(ADirection, 'OppositeVoxel3DDirection');
  Result := gdNorth;
  case ADirection of
    gdNorth:
      Result := gdSouth;
    gdEast:
      Result := gdWest;
    gdSouth:
      Result := gdNorth;
    gdWest:
      Result := gdEast;
    gdUp:
      Result := gdDown;
    gdDown:
      Result := gdUp;
  end;
end;

function RotateVoxel3DDirection(const ADirection: TGraphDirection;
  const ARotation: TVoxel3DRotation): TGraphDirection;
var
  LTurns: Integer;
begin
  RequireDirection(ADirection, 'RotateVoxel3DDirection');
  RequireRotation(ARotation, 'RotateVoxel3DDirection');
  if ADirection in [gdUp, gdDown] then
    Exit(ADirection);
  LTurns := Ord(ARotation);
  Result := TGraphDirection((Ord(ADirection) + LTurns) mod 4);
end;

function RotateVoxel3DDirections(const ADirections: TGraphDirections;
  const ARotation: TVoxel3DRotation): TGraphDirections;
var
  LDirection: TGraphDirection;
begin
  RequireRotation(ARotation, 'RotateVoxel3DDirections');
  Result := [];
  for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
    if LDirection in ADirections then
      Include(Result, RotateVoxel3DDirection(LDirection, ARotation));
end;

function RotateVoxel3DSockets(const ASockets: TVoxel3DSockets;
  const ARotation: TVoxel3DRotation): TVoxel3DSockets;
var
  LDirection: TGraphDirection;
begin
  RequireRotation(ARotation, 'RotateVoxel3DSockets');
  for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
    Result[RotateVoxel3DDirection(LDirection, ARotation)] :=
      ASockets[LDirection];
end;

function Voxel3DPrototypeHasFlag(const APrototype: TVoxel3DPrototype;
  const AFlag: TVoxel3DPrototypeFlag): Boolean;
begin
  Result := AFlag in APrototype.Flags;
end;

function Voxel3DVariantHasFlag(const AVariant: TVoxel3DVariant;
  const AFlag: TVoxel3DPrototypeFlag): Boolean;
begin
  Result := AFlag in AVariant.Flags;
end;

function MakeVoxel3DSockets(const ANorth, AEast, ASouth, AWest,
  AUp, ADown: String): TVoxel3DSockets;
begin
  Result[gdNorth] := ANorth;
  Result[gdEast] := AEast;
  Result[gdSouth] := ASouth;
  Result[gdWest] := AWest;
  Result[gdUp] := AUp;
  Result[gdDown] := ADown;
end;

function MakeVoxel3DPrototype(const AId, AMaterial: String;
  const AWeight: TGraphWeight; const ASockets: TVoxel3DSockets;
  const AAllowedRotations: TVoxel3DRotations;
  const AFlags: TVoxel3DPrototypeFlags;
  const AWalkOpenings: TGraphDirections): TVoxel3DPrototype;
begin
  Result.Id := AId;
  Result.Material := AMaterial;
  Result.Weight := AWeight;
  Result.Sockets := ASockets;
  Result.AllowedRotations := AAllowedRotations;
  Result.Flags := AFlags;
  Result.WalkOpenings := AWalkOpenings;
end;

function MakeVoxel3DSocketPair(const AFirst,
  ASecond: String): TVoxel3DSocketPair;
begin
  Result.First := AFirst;
  Result.Second := ASecond;
end;

function UpdateCrc32(const ACrc, AByte: Cardinal): Cardinal;
var
  I: Integer;
begin
  Result := ACrc xor (AByte and Cardinal($FF));
  for I := 0 to 7 do
    if (Result and Cardinal(1)) <> 0 then
      Result := (Result shr 1) xor CRC32_POLYNOMIAL
    else
      Result := Result shr 1;
end;

procedure MixCardinal(var ACrc: Cardinal; const AValue: Cardinal);
begin
  ACrc := UpdateCrc32(ACrc, AValue and Cardinal($FF));
  ACrc := UpdateCrc32(ACrc, (AValue shr 8) and Cardinal($FF));
  ACrc := UpdateCrc32(ACrc, (AValue shr 16) and Cardinal($FF));
  ACrc := UpdateCrc32(ACrc, (AValue shr 24) and Cardinal($FF));
end;

procedure MixAsciiString(var ACrc: Cardinal; const AValue: String);
var
  I: Integer;
begin
  MixCardinal(ACrc, Cardinal(Length(AValue)));
  for I := 1 to Length(AValue) do
    ACrc := UpdateCrc32(ACrc, Cardinal(Ord(AValue[I])));
end;

function RotationMask(const ARotations: TVoxel3DRotations): Cardinal;
var
  LRotation: TVoxel3DRotation;
begin
  Result := 0;
  for LRotation := Low(TVoxel3DRotation) to High(TVoxel3DRotation) do
    if LRotation in ARotations then
      Result := Result or (Cardinal(1) shl Ord(LRotation));
end;

function FlagMask(const AFlags: TVoxel3DPrototypeFlags): Cardinal;
var
  LFlag: TVoxel3DPrototypeFlag;
begin
  Result := 0;
  for LFlag := Low(TVoxel3DPrototypeFlag) to
      High(TVoxel3DPrototypeFlag) do
    if LFlag in AFlags then
      Result := Result or (Cardinal(1) shl Ord(LFlag));
end;

function DirectionMask(const ADirections: TGraphDirections): Cardinal;
var
  LDirection: TGraphDirection;
begin
  Result := 0;
  for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
    if LDirection in ADirections then
      Result := Result or (Cardinal(1) shl Ord(LDirection));
end;

function CopyPrototype(const AValue: TVoxel3DPrototype): TVoxel3DPrototype;
var
  LDirection: TGraphDirection;
begin
  Result.Id := AValue.Id;
  Result.Material := AValue.Material;
  Result.Weight := AValue.Weight;
  for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
    Result.Sockets[LDirection] := AValue.Sockets[LDirection];
  Result.AllowedRotations := AValue.AllowedRotations;
  Result.Flags := AValue.Flags;
  Result.WalkOpenings := AValue.WalkOpenings;
end;

function CopyVariant(const AValue: TVoxel3DVariant): TVoxel3DVariant;
var
  LDirection: TGraphDirection;
begin
  Result.PrototypeIndex := AValue.PrototypeIndex;
  Result.PrototypeId := AValue.PrototypeId;
  Result.Material := AValue.Material;
  Result.Weight := AValue.Weight;
  Result.Rotation := AValue.Rotation;
  for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
    Result.Sockets[LDirection] := AValue.Sockets[LDirection];
  Result.Flags := AValue.Flags;
  Result.WalkOpenings := AValue.WalkOpenings;
end;

function CopySocketPair(
  const AValue: TVoxel3DSocketPair): TVoxel3DSocketPair;
begin
  Result.First := AValue.First;
  Result.Second := AValue.Second;
end;

function SameSocketPair(const A, B: TVoxel3DSocketPair): Boolean;
begin
  Result := (A.First = B.First) and (A.Second = B.Second);
end;

function SameVariant(const A, B: TVoxel3DVariant): Boolean;
var
  LDirection: TGraphDirection;
begin
  Result := (A.PrototypeIndex = B.PrototypeIndex) and
    (A.PrototypeId = B.PrototypeId) and
    (A.Material = B.Material) and
    (A.Weight = B.Weight) and
    (A.Rotation = B.Rotation) and
    (A.Flags = B.Flags) and
    (A.WalkOpenings = B.WalkOpenings);
  if not Result then
    Exit;
  for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
    if A.Sockets[LDirection] <> B.Sockets[LDirection] then
      Exit(False);
end;

procedure CanonicalizeSocketPair(var APair: TVoxel3DSocketPair);
var
  LSwap: String;
begin
  if APair.Second < APair.First then
  begin
    LSwap := APair.First;
    APair.First := APair.Second;
    APair.Second := LSwap;
  end;
end;

function CompareSocketPair(const A, B: TVoxel3DSocketPair): Integer;
begin
  if A.First < B.First then
    Exit(-1);
  if A.First > B.First then
    Exit(1);
  if A.Second < B.Second then
    Exit(-1);
  if A.Second > B.Second then
    Exit(1);
  Result := 0;
end;

procedure SortSocketPairs(var APairs: TVoxel3DSocketPairs);
var
  I, LLeft, LMiddle, LRight, LSourceLeft, LSourceRight,
    LTarget, LWidth: Integer;
  LSorted: TVoxel3DSocketPairs;
begin
  if Length(APairs) < 2 then
    Exit;
  SetLength(LSorted, Length(APairs));
  LWidth := 1;
  while LWidth < Length(APairs) do
  begin
    LLeft := 0;
    while LLeft < Length(APairs) do
    begin
      if LWidth > Length(APairs) - LLeft then
        LMiddle := Length(APairs)
      else
        LMiddle := LLeft + LWidth;
      if LWidth > Length(APairs) - LMiddle then
        LRight := Length(APairs)
      else
        LRight := LMiddle + LWidth;
      LSourceLeft := LLeft;
      LSourceRight := LMiddle;
      LTarget := LLeft;
      while (LSourceLeft < LMiddle) and
          (LSourceRight < LRight) do
      begin
        if CompareSocketPair(APairs[LSourceLeft],
            APairs[LSourceRight]) <= 0 then
        begin
          LSorted[LTarget] := APairs[LSourceLeft];
          Inc(LSourceLeft);
        end
        else
        begin
          LSorted[LTarget] := APairs[LSourceRight];
          Inc(LSourceRight);
        end;
        Inc(LTarget);
      end;
      while LSourceLeft < LMiddle do
      begin
        LSorted[LTarget] := APairs[LSourceLeft];
        Inc(LSourceLeft);
        Inc(LTarget);
      end;
      while LSourceRight < LRight do
      begin
        LSorted[LTarget] := APairs[LSourceRight];
        Inc(LSourceRight);
        Inc(LTarget);
      end;
      LLeft := LRight;
    end;
    for I := 0 to High(APairs) do
      APairs[I] := LSorted[I];
    if LWidth > Length(APairs) div 2 then
      LWidth := Length(APairs)
    else
      LWidth := LWidth * 2;
  end;
end;

function SocketPairsContain(const APairs: TVoxel3DSocketPairs;
  const AFirst, ASecond: String): Boolean;
var
  LComparison, LHigh, LLow, LMiddle: Integer;
  LNeedle: TVoxel3DSocketPair;
begin
  LNeedle.First := AFirst;
  LNeedle.Second := ASecond;
  CanonicalizeSocketPair(LNeedle);
  LLow := 0;
  LHigh := High(APairs);
  while LLow <= LHigh do
  begin
    LMiddle := LLow + ((LHigh - LLow) div 2);
    LComparison := CompareSocketPair(APairs[LMiddle], LNeedle);
    if LComparison = 0 then
      Exit(True);
    if LComparison < 0 then
      LLow := LMiddle + 1
    else
      LHigh := LMiddle - 1;
  end;
  Result := False;
end;

procedure ValidatePrototype(const APrototype: TVoxel3DPrototype;
  const AIndex: Integer);
var
  LDirection: TGraphDirection;
  LPrefix: String;
begin
  LPrefix := 'prototype ' + IntToStr(AIndex);
  RequireToken(APrototype.Id, LPrefix + ' id');
  RequireToken(APrototype.Material, LPrefix + ' material');
  if APrototype.Weight <= 0 then
    raise EVoxel3DModel.CreateFmt('%s weight must be positive', [LPrefix]);
  if APrototype.AllowedRotations = [] then
    raise EVoxel3DModel.CreateFmt(
      '%s must allow at least one rotation', [LPrefix]);
  for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
    RequireToken(APrototype.Sockets[LDirection],
      LPrefix + ' socket ' + IntToStr(Ord(LDirection)));

  if (v3pfEmpty in APrototype.Flags) and
      (v3pfSolid in APrototype.Flags) then
    raise EVoxel3DModel.CreateFmt(
      '%s cannot be both empty and solid', [LPrefix]);
  if (v3pfEmpty in APrototype.Flags) and
      ((v3pfRequiresSupport in APrototype.Flags) or
       (v3pfProvidesSupport in APrototype.Flags)) then
    raise EVoxel3DModel.CreateFmt(
      '%s empty cells cannot require or provide support', [LPrefix]);
  if (v3pfSolid in APrototype.Flags) and
      (v3pfWalkable in APrototype.Flags) then
    raise EVoxel3DModel.CreateFmt(
      '%s solid cells cannot be walkable', [LPrefix]);
  if (v3pfEntrance in APrototype.Flags) and
      not (v3pfWalkable in APrototype.Flags) then
    raise EVoxel3DModel.CreateFmt(
      '%s entrances must be walkable', [LPrefix]);
  if (v3pfRequiredReachable in APrototype.Flags) and
      not (v3pfWalkable in APrototype.Flags) then
    raise EVoxel3DModel.CreateFmt(
      '%s required-reachable cells must be walkable', [LPrefix]);
  if (APrototype.WalkOpenings <> []) and
      not (v3pfWalkable in APrototype.Flags) then
    raise EVoxel3DModel.CreateFmt(
      '%s walk openings require a walkable prototype', [LPrefix]);
  if (v3pfEntrance in APrototype.Flags) and
      (APrototype.WalkOpenings = []) then
    raise EVoxel3DModel.CreateFmt(
      '%s entrances need at least one walk opening', [LPrefix]);
end;

function CountRotations(const ARotations: TVoxel3DRotations): Integer;
var
  LRotation: TVoxel3DRotation;
begin
  Result := 0;
  for LRotation := Low(TVoxel3DRotation) to High(TVoxel3DRotation) do
    if LRotation in ARotations then
      Inc(Result);
end;

function BuildKitSignature(const AId: String;
  const APrototypes: TVoxel3DPrototypes;
  const APairs: TVoxel3DSocketPairs): TVoxel3DSignature;
var
  I: Integer;
  LDirection: TGraphDirection;
begin
  Result := Cardinal($FFFFFFFF);
  MixCardinal(Result, WFC_VOXEL3D_SIGNATURE_VERSION);
  MixCardinal(Result, WFC_VOXEL3D_KIT_VERSION);
  MixAsciiString(Result, AId);
  MixCardinal(Result, Cardinal(Length(APrototypes)));
  for I := 0 to High(APrototypes) do
  begin
    MixAsciiString(Result, APrototypes[I].Id);
    MixAsciiString(Result, APrototypes[I].Material);
    MixCardinal(Result, Cardinal(APrototypes[I].Weight));
    MixCardinal(Result, RotationMask(APrototypes[I].AllowedRotations));
    MixCardinal(Result, FlagMask(APrototypes[I].Flags));
    MixCardinal(Result, DirectionMask(APrototypes[I].WalkOpenings));
    for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
      MixAsciiString(Result, APrototypes[I].Sockets[LDirection]);
  end;
  MixCardinal(Result, Cardinal(Length(APairs)));
  for I := 0 to High(APairs) do
  begin
    MixAsciiString(Result, APairs[I].First);
    MixAsciiString(Result, APairs[I].Second);
  end;
  Result := Result xor Cardinal($FFFFFFFF);
end;

function BuildApplicationIdentity(const AKitId: String;
  const AKitSignature: TVoxel3DSignature;
  const AWrapNeighbors: Boolean): String;
begin
  Result := ADAPTER_IDENTITY_PREFIX + AKitId + ':' +
    Voxel3DSignatureHex(AKitSignature) + ':wrap:';
  if AWrapNeighbors then
    Result := Result + '1'
  else
    Result := Result + '0';
end;

function BuildVariantGraphKey(const AKitIdentity: String;
  const AVariantIndex: Integer;
  const AVariant: TVoxel3DVariant): TGraphValue;
begin
  Result := AKitIdentity + GRAPH_KEY_MARKER +
    IntToStr(AVariantIndex) + ':' + AVariant.PrototypeId + ':' +
    IntToStr(Voxel3DRotationDegrees(AVariant.Rotation));
end;

function FindVariantGraphKeyInOrder(const AKeys: TVoxel3DGraphKeys;
  const AIdentity: String; const AValue: TGraphValue;
  out AVariantIndex: Integer): Boolean;
var
  I, LDigit, LPosition: Integer;
  LPrefix: String;
begin
  Result := False;
  AVariantIndex := -1;
  LPrefix := AIdentity + GRAPH_KEY_MARKER;
  if Length(AValue) <= Length(LPrefix) + 1 then
    Exit;
  for I := 1 to Length(LPrefix) do
    if AValue[I] <> LPrefix[I] then
      Exit;

  LPosition := Length(LPrefix) + 1;
  if not (AValue[LPosition] in ['0'..'9']) then
    Exit;
  AVariantIndex := 0;
  if AValue[LPosition] = '0' then
    Inc(LPosition)
  else
    while (LPosition <= Length(AValue)) and
        (AValue[LPosition] in ['0'..'9']) do
    begin
      LDigit := Ord(AValue[LPosition]) - Ord('0');
      if AVariantIndex > (High(Integer) - LDigit) div 10 then
      begin
        AVariantIndex := -1;
        Exit;
      end;
      AVariantIndex := (AVariantIndex * 10) + LDigit;
      Inc(LPosition);
    end;
  if (LPosition > Length(AValue)) or
      (AValue[LPosition] <> ':') or
      (AVariantIndex < 0) or (AVariantIndex >= Length(AKeys)) or
      (AKeys[AVariantIndex] <> AValue) then
  begin
    AVariantIndex := -1;
    Exit;
  end;
  Result := True;
end;

function ResolveActivePass(const AGraph: TGraph): TGraph;
begin
  if not Assigned(AGraph) then
    raise EVoxel3DAdapter.Create('voxel graph cannot be nil');
  Result := AGraph.PassGraph[AGraph.CurrentPassIndex];
end;

procedure ValidateShapedEmptyPass(const AGraph: TGraph);
var
  X, Y, Z: Integer;
begin
  if not Assigned(AGraph) then
    raise EVoxel3DAdapter.Create('voxel graph pass cannot be nil');
  if AGraph.Running then
    raise EVoxel3DAdapter.Create(
      'cannot apply a voxel kit while the graph is running');
  if (AGraph.Dimension.Width = 0) or
      (AGraph.Dimension.Height = 0) or
      (AGraph.Dimension.Depth = 0) then
    raise EVoxel3DAdapter.Create(
      'voxel graph pass must be shaped before kit application');
  if AGraph.HasDefinition then
    raise EVoxel3DAdapter.Create(
      'voxel graph pass already contains a model definition');

  for Z := 0 to Integer(AGraph.Dimension.Depth) - 1 do
    for Y := 0 to Integer(AGraph.Dimension.Height) - 1 do
      for X := 0 to Integer(AGraph.Dimension.Width) - 1 do
      begin
        if not AGraph.Entry[X, Y, Z].Empty then
          raise EVoxel3DAdapter.CreateFmt(
            'voxel graph pass entry [%d,%d,%d] is not empty', [X, Y, Z]);
        if AGraph.HasAllowedValues(X, Y, Z) then
          raise EVoxel3DAdapter.CreateFmt(
            'voxel graph pass entry [%d,%d,%d] already has a domain',
            [X, Y, Z]);
      end;
end;

function CheckedCellCount(const AWidth, AHeight,
  ADepth: TGraphCoordinate; const AOperation: String): Integer;
var
  LWidth, LHeight, LDepth, LPlane: Integer;
begin
  LWidth := Integer(AWidth);
  LHeight := Integer(AHeight);
  LDepth := Integer(ADepth);
  if (LWidth <= 0) or (LHeight <= 0) or (LDepth <= 0) then
    raise EVoxel3DScene.Create(AOperation + ' dimensions must be positive');
  if LWidth > High(Integer) div LHeight then
    raise ERangeError.Create(AOperation + ' plane size exceeds Integer');
  LPlane := LWidth * LHeight;
  if LPlane > High(Integer) div LDepth then
    raise ERangeError.Create(AOperation + ' cell count exceeds Integer');
  Result := LPlane * LDepth;
end;

{ TVoxel3DKit }

constructor TVoxel3DKit.Create(const AId: String;
  const APrototypes: TVoxel3DPrototypes;
  const ASocketPairs: TVoxel3DSocketPairs);
begin
  inherited Create;
  Initialize(AId, APrototypes, ASocketPairs);
end;

procedure TVoxel3DKit.Initialize(const AId: String;
  const APrototypes: TVoxel3DPrototypes;
  const ASocketPairs: TVoxel3DSocketPairs);
var
  I, J, LRotationCount, LVariantCount, LVariantIndex: Integer;
  LRotation: TVoxel3DRotation;
begin
  RequireToken(AId, 'kit id');
  if Length(APrototypes) = 0 then
    raise EVoxel3DModel.Create('voxel kit needs at least one prototype');
  if Length(ASocketPairs) = 0 then
    raise EVoxel3DModel.Create(
      'voxel kit needs at least one socket compatibility pair');

  FId := AId;
  SetLength(FPrototypes, Length(APrototypes));
  LVariantCount := 0;
  for I := 0 to High(APrototypes) do
  begin
    ValidatePrototype(APrototypes[I], I);
    for J := 0 to I - 1 do
      if APrototypes[J].Id = APrototypes[I].Id then
        raise EVoxel3DModel.CreateFmt(
          'duplicate prototype id "%s"', [APrototypes[I].Id]);
    FPrototypes[I] := CopyPrototype(APrototypes[I]);
    LRotationCount := CountRotations(APrototypes[I].AllowedRotations);
    if LVariantCount > High(Integer) - LRotationCount then
      raise ERangeError.Create('voxel variant count exceeds Integer');
    Inc(LVariantCount, LRotationCount);
  end;

  SetLength(FSocketPairs, Length(ASocketPairs));
  for I := 0 to High(ASocketPairs) do
  begin
    RequireToken(ASocketPairs[I].First,
      'socket pair ' + IntToStr(I) + ' first token');
    RequireToken(ASocketPairs[I].Second,
      'socket pair ' + IntToStr(I) + ' second token');
    FSocketPairs[I] := CopySocketPair(ASocketPairs[I]);
    CanonicalizeSocketPair(FSocketPairs[I]);
  end;
  SortSocketPairs(FSocketPairs);
  for I := 1 to High(FSocketPairs) do
    if SameSocketPair(FSocketPairs[I - 1], FSocketPairs[I]) then
      raise EVoxel3DModel.CreateFmt(
        'duplicate socket compatibility pair "%s"/"%s"',
        [FSocketPairs[I].First, FSocketPairs[I].Second]);

  SetLength(FVariants, LVariantCount);
  LVariantIndex := 0;
  for I := 0 to High(FPrototypes) do
    for LRotation := Low(TVoxel3DRotation) to
        High(TVoxel3DRotation) do
      if LRotation in FPrototypes[I].AllowedRotations then
      begin
        FVariants[LVariantIndex].PrototypeIndex := I;
        FVariants[LVariantIndex].PrototypeId := FPrototypes[I].Id;
        FVariants[LVariantIndex].Material := FPrototypes[I].Material;
        FVariants[LVariantIndex].Weight := FPrototypes[I].Weight;
        FVariants[LVariantIndex].Rotation := LRotation;
        FVariants[LVariantIndex].Sockets := RotateVoxel3DSockets(
          FPrototypes[I].Sockets, LRotation);
        FVariants[LVariantIndex].Flags := FPrototypes[I].Flags;
        FVariants[LVariantIndex].WalkOpenings :=
          RotateVoxel3DDirections(FPrototypes[I].WalkOpenings,
            LRotation);
        Inc(LVariantIndex);
      end;

  FSignature := BuildKitSignature(FId, FPrototypes, FSocketPairs);
  FIdentity := KIT_IDENTITY_PREFIX + FId + ':' +
    Voxel3DSignatureHex(FSignature);
  SetLength(FVariantGraphKeys, Length(FVariants));
  for I := 0 to High(FVariants) do
    FVariantGraphKeys[I] := BuildVariantGraphKey(
      FIdentity, I, FVariants[I]);
end;

function TVoxel3DKit.GetPrototypeCount: Integer;
begin
  Result := Length(FPrototypes);
end;

function TVoxel3DKit.GetSocketPairCount: Integer;
begin
  Result := Length(FSocketPairs);
end;

function TVoxel3DKit.GetVariantCount: Integer;
begin
  Result := Length(FVariants);
end;

function TVoxel3DKit.PrototypeAt(
  const AIndex: Integer): TVoxel3DPrototype;
begin
  if (AIndex < 0) or (AIndex >= Length(FPrototypes)) then
    raise ERangeError.CreateFmt(
      'voxel prototype index out of bounds [%d]', [AIndex]);
  Result := CopyPrototype(FPrototypes[AIndex]);
end;

function TVoxel3DKit.SocketPairAt(
  const AIndex: Integer): TVoxel3DSocketPair;
begin
  if (AIndex < 0) or (AIndex >= Length(FSocketPairs)) then
    raise ERangeError.CreateFmt(
      'voxel socket pair index out of bounds [%d]', [AIndex]);
  Result := CopySocketPair(FSocketPairs[AIndex]);
end;

function TVoxel3DKit.VariantAt(
  const AIndex: Integer): TVoxel3DVariant;
begin
  if (AIndex < 0) or (AIndex >= Length(FVariants)) then
    raise ERangeError.CreateFmt(
      'voxel variant index out of bounds [%d]', [AIndex]);
  Result := CopyVariant(FVariants[AIndex]);
end;

function TVoxel3DKit.VariantGraphKeyAt(
  const AIndex: Integer): TGraphValue;
begin
  if (AIndex < 0) or (AIndex >= Length(FVariantGraphKeys)) then
    raise ERangeError.CreateFmt(
      'voxel variant key index out of bounds [%d]', [AIndex]);
  Result := FVariantGraphKeys[AIndex];
end;

function TVoxel3DKit.FindVariantGraphKey(const AValue: TGraphValue;
  out AVariantIndex: Integer): Boolean;
begin
  Result := FindVariantGraphKeyInOrder(FVariantGraphKeys,
    FIdentity, AValue, AVariantIndex);
end;

function TVoxel3DKit.SocketsCompatible(const AFirst,
  ASecond: String): Boolean;
begin
  Result := SocketPairsContain(FSocketPairs, AFirst, ASecond);
end;

function VariantsAreCompatible(const AVariants: TVoxel3DVariants;
  const APairs: TVoxel3DSocketPairs; const ASourceIndex: Integer;
  const ADirection: TGraphDirection;
  const ANeighborIndex: Integer): Boolean;
var
  LOpposite: TGraphDirection;
begin
  RequireDirection(ADirection, 'VariantsCompatible');
  if (ASourceIndex < 0) or (ASourceIndex >= Length(AVariants)) then
    raise ERangeError.CreateFmt(
      'source voxel variant index out of bounds [%d]', [ASourceIndex]);
  if (ANeighborIndex < 0) or
      (ANeighborIndex >= Length(AVariants)) then
    raise ERangeError.CreateFmt(
      'neighbor voxel variant index out of bounds [%d]', [ANeighborIndex]);

  LOpposite := OppositeVoxel3DDirection(ADirection);
  Result := SocketPairsContain(APairs,
    AVariants[ASourceIndex].Sockets[ADirection],
    AVariants[ANeighborIndex].Sockets[LOpposite]);
  if not Result then
    Exit;

  //A support requirement is directional. A source looking down needs its
  //neighbor to provide support. The reverse source looking up applies the
  //same relation to the upper neighbor, so compatibility remains symmetric.
  //On a bounded graph, z=0 has no downward neighbor and is therefore the
  //explicit implicit-ground exception. Wrapped depth has no such exception.
  if (ADirection = gdDown) and
      (v3pfRequiresSupport in AVariants[ASourceIndex].Flags) and
      not (v3pfProvidesSupport in AVariants[ANeighborIndex].Flags) then
    Exit(False);
  if (ADirection = gdUp) and
      (v3pfRequiresSupport in AVariants[ANeighborIndex].Flags) and
      not (v3pfProvidesSupport in AVariants[ASourceIndex].Flags) then
    Exit(False);
end;

function TVoxel3DKit.VariantsCompatible(const ASourceIndex: Integer;
  const ADirection: TGraphDirection;
  const ANeighborIndex: Integer): Boolean;
begin
  Result := VariantsAreCompatible(FVariants, FSocketPairs,
    ASourceIndex, ADirection, ANeighborIndex);
end;

function TVoxel3DKit.ApplyToGraph(
  const AGraph: TGraph): TVoxel3DGraphAdapter;
var
  I, J, LAllowedCount, LRuleCount: Integer;
  LDenyDirections: TVoxel3DDirectionSets;
  LDirection, LRuleDirection: TGraphDirection;
  LGraph: TGraph;
  LRules: TVoxel3DGraphRuleMatrix;
begin
  Result := nil;
  LGraph := ResolveActivePass(AGraph);
  ValidateShapedEmptyPass(LGraph);

  //Build the complete symmetric model before the first graph mutation.
  //Calling NewRule once per dense edge would run whole-model inverse closure
  //after every insertion. The kit constructor has already proved its socket
  //relation symmetric, so one checked matrix commit is both exact and avoids
  //quartic construction for realistic rotated kits.
  SetLength(LRules, Length(FVariants));
  SetLength(LDenyDirections, Length(FVariants));
  for I := 0 to High(FVariants) do
  begin
    LRuleCount := 0;
    for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
    begin
      LAllowedCount := 0;
      for J := 0 to High(FVariants) do
        if VariantsCompatible(I, LDirection, J) then
          Inc(LAllowedCount);
      //TGraph directions describe where the rule-owning value is relative to
      //the target value. The voxel API describes the more usual direction
      //from source cell to neighbor, hence this explicit inverse mapping.
      LRuleDirection := OppositeVoxel3DDirection(LDirection);
      if LAllowedCount = 0 then
        Include(LDenyDirections[I], LRuleDirection)
      else
      begin
        //SetLength creates a distinct managed record slot under pas2js as
        //well as native FPC. Values retain canonical variant order.
        SetLength(LRules[I], LRuleCount + 1);
        LRules[I][LRuleCount].Key := LRuleDirection;
        LRules[I][LRuleCount].Info := False;
        SetLength(LRules[I][LRuleCount].Value, LAllowedCount);
        LAllowedCount := 0;
        for J := 0 to High(FVariants) do
          if VariantsCompatible(I, LDirection, J) then
          begin
            LRules[I][LRuleCount].Value[LAllowedCount] :=
              FVariantGraphKeys[J];
            Inc(LAllowedCount);
          end;
        Inc(LRuleCount);
      end;
    end;
  end;

  for I := 0 to High(FVariants) do
    LGraph.AddValue(FVariantGraphKeys[I], FVariants[I].Weight);
  //Install explicit zero-support faces before finite rows. DenyAll still
  //validates the owning registry, but there are no reciprocal value arrays to
  //scan or rewrite yet; this keeps sparse kit application quadratic.
  for I := 0 to High(FVariants) do
    if LDenyDirections[I] <> [] then
      LGraph.Rules[FVariantGraphKeys[I]].DenyAll(
        LDenyDirections[I]);
  for I := 0 to High(FVariants) do
    LGraph.Rules[FVariantGraphKeys[I]].Rules := LRules[I];

  Result := TVoxel3DGraphAdapter.Create(Self, LGraph);
end;

function TVoxel3DKit.MatchesAdapter(
  const AAdapter: TVoxel3DGraphAdapter): Boolean;
var
  I: Integer;
begin
  Result := Assigned(AAdapter) and
    (FId = AAdapter.FKitId) and
    (FIdentity = AAdapter.FKitIdentity) and
    (FSignature = AAdapter.FKitSignature) and
    (Length(FVariants) = Length(AAdapter.FVariants)) and
    (Length(FSocketPairs) = Length(AAdapter.FSocketPairs)) and
    (Length(FVariantGraphKeys) = Length(AAdapter.FGraphKeys));
  if not Result then
    Exit;
  for I := 0 to High(FVariants) do
    if (not SameVariant(FVariants[I], AAdapter.FVariants[I])) or
        (FVariantGraphKeys[I] <> AAdapter.FGraphKeys[I]) then
      Exit(False);
  for I := 0 to High(FSocketPairs) do
    if not SameSocketPair(FSocketPairs[I], AAdapter.FSocketPairs[I]) then
      Exit(False);
end;

function TVoxel3DKit.CaptureScene(
  const AAdapter: TVoxel3DGraphAdapter): TVoxel3DScene;
begin
  if not Assigned(AAdapter) then
    raise EVoxel3DScene.Create('voxel graph adapter cannot be nil');
  if not MatchesAdapter(AAdapter) then
    raise EVoxel3DScene.Create(
      'voxel graph adapter belongs to a different kit identity');
  Result := AAdapter.Capture;
end;

function TVoxel3DKit.MatchesScene(const AScene: TVoxel3DScene): Boolean;
var
  I: Integer;
begin
  Result := Assigned(AScene) and
    (FId = AScene.FKitId) and
    (FIdentity = AScene.FKitIdentity) and
    (FSignature = AScene.FKitSignature) and
    (Length(FVariants) = Length(AScene.FVariants)) and
    (Length(FSocketPairs) = Length(AScene.FSocketPairs));
  if not Result then
    Exit;
  for I := 0 to High(FVariants) do
    if not SameVariant(FVariants[I], AScene.FVariants[I]) then
      Exit(False);
  for I := 0 to High(FSocketPairs) do
    if not SameSocketPair(FSocketPairs[I], AScene.FSocketPairs[I]) then
      Exit(False);
end;

{ TVoxel3DGraphAdapter }

constructor TVoxel3DGraphAdapter.Create(const AKit: TVoxel3DKit;
  const AGraph: TGraph);
var
  I: Integer;
  LGraph: TGraph;
begin
  inherited Create;
  if not Assigned(AKit) then
    raise EVoxel3DAdapter.Create('voxel kit cannot be nil');
  if not Assigned(AGraph) then
    raise EVoxel3DAdapter.Create('applied voxel graph cannot be nil');
  LGraph := ResolveActivePass(AGraph);
  FAppliedGraph := LGraph;
  FPassIndex := LGraph.CurrentPassIndex;
  FWidth := LGraph.Dimension.Width;
  FHeight := LGraph.Dimension.Height;
  FDepth := LGraph.Dimension.Depth;
  FWrapNeighbors := LGraph.WrapNeighbors;
  FKitId := AKit.Id;
  FKitIdentity := AKit.Identity;
  FKitSignature := AKit.Signature;
  FApplicationIdentity := BuildApplicationIdentity(FKitId, FKitSignature,
    FWrapNeighbors);

  SetLength(FVariants, AKit.VariantCount);
  SetLength(FGraphKeys, AKit.VariantCount);
  for I := 0 to AKit.VariantCount - 1 do
  begin
    FVariants[I] := AKit.VariantAt(I);
    FGraphKeys[I] := AKit.VariantGraphKeyAt(I);
  end;
  SetLength(FSocketPairs, AKit.SocketPairCount);
  for I := 0 to AKit.SocketPairCount - 1 do
    FSocketPairs[I] := AKit.SocketPairAt(I);
end;

function TVoxel3DGraphAdapter.GetVariantCount: Integer;
begin
  Result := Length(FVariants);
end;

function TVoxel3DGraphAdapter.VariantAt(
  const AIndex: Integer): TVoxel3DVariant;
begin
  if (AIndex < 0) or (AIndex >= Length(FVariants)) then
    raise ERangeError.CreateFmt(
      'adapter voxel variant index out of bounds [%d]', [AIndex]);
  Result := CopyVariant(FVariants[AIndex]);
end;

function TVoxel3DGraphAdapter.VariantGraphKeyAt(
  const AIndex: Integer): TGraphValue;
begin
  if (AIndex < 0) or (AIndex >= Length(FGraphKeys)) then
    raise ERangeError.CreateFmt(
      'adapter voxel variant key index out of bounds [%d]', [AIndex]);
  Result := FGraphKeys[AIndex];
end;

function TVoxel3DGraphAdapter.FindVariantGraphKey(
  const AValue: TGraphValue; out AVariantIndex: Integer): Boolean;
begin
  Result := FindVariantGraphKeyInOrder(FGraphKeys,
    FKitIdentity, AValue, AVariantIndex);
end;

function TVoxel3DGraphAdapter.VariantsCompatible(
  const ASourceIndex: Integer; const ADirection: TGraphDirection;
  const ANeighborIndex: Integer): Boolean;
begin
  Result := VariantsAreCompatible(FVariants, FSocketPairs,
    ASourceIndex, ADirection, ANeighborIndex);
end;

function GraphRuleValuesMatch(const AActual,
  AExpected: TGraphValues): Boolean;
var
  I: Integer;
begin
  Result := Length(AActual) = Length(AExpected);
  if not Result then
    Exit;
  for I := 0 to High(AExpected) do
    if AActual[I] <> AExpected[I] then
      Exit(False);
end;

function TVoxel3DGraphAdapter.DefinitionMatchesGraph: Boolean;
var
  I, J, K, LAllowedCount, LRuleOrdinal: Integer;
  LAllowed: TGraphValues;
  LDirection, LRuleDirection: TGraphDirection;
  LGroup: TGraphRuleGroup;
  LRegisteredValues: TGraphValues;
  LRule: TGraphRule;
  LRuleDirections: TGraphDirections;
begin
  Result := False;
  if not Assigned(FAppliedGraph) then
    Exit;
  LRegisteredValues := FAppliedGraph.CopyRegisteredValues;
  if (FAppliedGraph.Dimension.Width <> FWidth) or
      (FAppliedGraph.Dimension.Height <> FHeight) or
      (FAppliedGraph.Dimension.Depth <> FDepth) or
      (FAppliedGraph.WrapNeighbors <> FWrapNeighbors) or
      (FAppliedGraph.CurrentPassIndex <> FPassIndex) or
      (Length(LRegisteredValues) <> Length(FGraphKeys)) or
      (FAppliedGraph.RuleGroups.Count <> Length(FVariants)) then
    Exit;

  for I := 0 to High(FGraphKeys) do
    if LRegisteredValues[I] <> FGraphKeys[I] then
      Exit;

  SetLength(LAllowed, Length(FVariants));
  for I := 0 to High(FVariants) do
  begin
    if not FAppliedGraph.RuleGroups.ContainsKey(FGraphKeys[I]) then
      Exit;
    LGroup := FAppliedGraph.RuleGroups[FGraphKeys[I]];
    if (not Assigned(LGroup)) or
        (LGroup.Value <> FGraphKeys[I]) or
        (LGroup.Weight <> FVariants[I].Weight) then
      Exit;

    //Rules is intentionally public for legacy callers. Prove that the whole
    //array is a canonical six-direction partition before relying on indexed
    //lookup: no invalid key, duplicate row, unexpected row, or direction that
    //is simultaneously finite and denied may survive capture.
    LRuleDirections := [];
    for K := 0 to High(LGroup.Rules) do
    begin
      LRule := LGroup.Rules[K];
      LRuleOrdinal := Ord(LRule.Key);
      if (LRuleOrdinal < Ord(Low(TGraphDirection))) or
          (LRuleOrdinal > Ord(High(TGraphDirection))) or
          (LRule.Key in LRuleDirections) then
        Exit;
      Include(LRuleDirections, LRule.Key);
    end;
    for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
      if ((LDirection in LRuleDirections) and
          LGroup.Denied[LDirection]) or
          ((not (LDirection in LRuleDirections)) and
          (not LGroup.Denied[LDirection])) then
        Exit;

    for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
    begin
      LAllowedCount := 0;
      for J := 0 to High(FVariants) do
        if VariantsCompatible(I, LDirection, J) then
        begin
          LAllowed[LAllowedCount] := FGraphKeys[J];
          Inc(LAllowedCount);
        end;

      LRuleDirection := OppositeVoxel3DDirection(LDirection);

      if LAllowedCount = 0 then
      begin
        if (not LGroup.Denied[LRuleDirection]) or
            LGroup.Exists[LRuleDirection] then
          Exit;
      end
      else
      begin
        if LGroup.Denied[LRuleDirection] or
            (not LGroup.Exists[LRuleDirection]) then
          Exit;
        SetLength(LAllowed, LAllowedCount);
        LRule := LGroup.Rule[LRuleDirection];
        if LRule.Info or
            (not GraphRuleValuesMatch(LRule.Value, LAllowed)) then
          Exit;
        SetLength(LAllowed, Length(FVariants));
      end;
    end;
  end;
  Result := True;
end;

function TVoxel3DGraphAdapter.Capture: TVoxel3DScene;
var
  LCellCount, LIndex, LVariantIndex, X, Y, Z: Integer;
  LIndices: TVoxel3DVariantIndices;
  LValue: TGraphValue;
begin
  Result := nil;
  if not Assigned(FAppliedGraph) then
    raise EVoxel3DScene.Create('applied voxel graph is no longer assigned');
  if FAppliedGraph.Running then
    raise EVoxel3DScene.Create(
      'cannot capture a voxel scene while the graph is running');
  if not DefinitionMatchesGraph then
    raise EVoxel3DScene.Create(
      'applied voxel graph no longer matches its adapter identity');

  LCellCount := CheckedCellCount(FWidth, FHeight, FDepth,
    'voxel scene capture');
  SetLength(LIndices, LCellCount);
  LIndex := 0;
  for Z := 0 to Integer(FDepth) - 1 do
    for Y := 0 to Integer(FHeight) - 1 do
      for X := 0 to Integer(FWidth) - 1 do
      begin
        if FAppliedGraph.Entry[X, Y, Z].Empty then
          raise EVoxel3DScene.CreateFmt(
            'voxel scene entry [%d,%d,%d] is empty', [X, Y, Z]);
        LValue := FAppliedGraph.Entry[X, Y, Z].Value;
        if not FindVariantGraphKey(LValue, LVariantIndex) then
          raise EVoxel3DScene.CreateFmt(
            'voxel scene entry [%d,%d,%d] has an unknown value',
            [X, Y, Z]);
        LIndices[LIndex] := LVariantIndex;
        Inc(LIndex);
      end;
  Result := TVoxel3DScene.Create(Self, LIndices);
end;

{ TVoxel3DScene }

constructor TVoxel3DScene.Create(const AAdapter: TVoxel3DGraphAdapter;
  const AVariantIndices: TVoxel3DVariantIndices);
var
  I, LExpectedCount: Integer;
  LCrc: Cardinal;
begin
  inherited Create;
  if not Assigned(AAdapter) then
    raise EVoxel3DScene.Create('voxel scene adapter cannot be nil');
  LExpectedCount := CheckedCellCount(AAdapter.Width, AAdapter.Height,
    AAdapter.Depth, 'voxel scene');
  if Length(AVariantIndices) <> LExpectedCount then
    raise EVoxel3DScene.Create(
      'voxel scene variant array does not match its dimensions');

  FWidth := AAdapter.Width;
  FHeight := AAdapter.Height;
  FDepth := AAdapter.Depth;
  FWrapNeighbors := AAdapter.WrapNeighbors;
  FKitId := AAdapter.KitId;
  FKitIdentity := AAdapter.KitIdentity;
  FKitSignature := AAdapter.KitSignature;
  FApplicationIdentity := AAdapter.ApplicationIdentity;

  SetLength(FVariants, AAdapter.VariantCount);
  for I := 0 to AAdapter.VariantCount - 1 do
    FVariants[I] := AAdapter.VariantAt(I);
  SetLength(FSocketPairs, Length(AAdapter.FSocketPairs));
  for I := 0 to High(FSocketPairs) do
    FSocketPairs[I] := CopySocketPair(AAdapter.FSocketPairs[I]);
  SetLength(FVariantIndices, Length(AVariantIndices));
  for I := 0 to High(AVariantIndices) do
  begin
    if (AVariantIndices[I] < 0) or
        (AVariantIndices[I] >= Length(FVariants)) then
      raise EVoxel3DScene.CreateFmt(
        'voxel scene variant index out of bounds at cell %d', [I]);
    FVariantIndices[I] := AVariantIndices[I];
  end;

  LCrc := Cardinal($FFFFFFFF);
  MixCardinal(LCrc, WFC_VOXEL3D_SIGNATURE_VERSION);
  MixCardinal(LCrc, WFC_VOXEL3D_SCENE_VERSION);
  MixCardinal(LCrc, FKitSignature);
  MixAsciiString(LCrc, FApplicationIdentity);
  MixCardinal(LCrc, Cardinal(FWidth));
  MixCardinal(LCrc, Cardinal(FHeight));
  MixCardinal(LCrc, Cardinal(FDepth));
  if FWrapNeighbors then
    MixCardinal(LCrc, 1)
  else
    MixCardinal(LCrc, 0);
  MixCardinal(LCrc, Cardinal(Length(FVariantIndices)));
  for I := 0 to High(FVariantIndices) do
    MixCardinal(LCrc, Cardinal(FVariantIndices[I]));
  FSignature := LCrc xor Cardinal($FFFFFFFF);
end;

function TVoxel3DScene.GetCellCount: Integer;
begin
  Result := Length(FVariantIndices);
end;

function TVoxel3DScene.GetVariantCount: Integer;
begin
  Result := Length(FVariants);
end;

function TVoxel3DScene.CoordToIndex(const AX, AY,
  AZ: TGraphCoordinate): Integer;
var
  LX, LY, LZ, LWidth, LHeight: Integer;
begin
  if (AX >= FWidth) or (AY >= FHeight) or (AZ >= FDepth) then
    raise ERangeError.CreateFmt(
      'voxel scene coordinate out of bounds [%d,%d,%d]',
      [AX, AY, AZ]);
  LX := Integer(AX);
  LY := Integer(AY);
  LZ := Integer(AZ);
  LWidth := Integer(FWidth);
  LHeight := Integer(FHeight);
  Result := (LWidth * LHeight * LZ) + LX + (LY * LWidth);
end;

function TVoxel3DScene.VariantAt(
  const AIndex: Integer): TVoxel3DVariant;
begin
  if (AIndex < 0) or (AIndex >= Length(FVariants)) then
    raise ERangeError.CreateFmt(
      'scene voxel variant index out of bounds [%d]', [AIndex]);
  Result := CopyVariant(FVariants[AIndex]);
end;

function TVoxel3DScene.VariantIndexAt(const AX, AY,
  AZ: TGraphCoordinate): Integer;
begin
  Result := FVariantIndices[CoordToIndex(AX, AY, AZ)];
end;

function TVoxel3DScene.VariantAtCell(const AX, AY,
  AZ: TGraphCoordinate): TVoxel3DVariant;
begin
  Result := VariantAt(VariantIndexAt(AX, AY, AZ));
end;

function TVoxel3DScene.CopyVariantIndices: TVoxel3DVariantIndices;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FVariantIndices));
  for I := 0 to High(FVariantIndices) do
    Result[I] := FVariantIndices[I];
end;

end.
