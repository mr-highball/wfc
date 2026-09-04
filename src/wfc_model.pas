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
unit wfc_model;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc;

type
  EWfcModel = class(Exception);

  {$IFDEF PAS2JS}
  //pas2js strings are Unicode strings and its RTL does not declare the
  //native compiler's UTF8String alias.
  UTF8String = String;
  {$ENDIF}
  TWfcModelToken = UTF8String;
  TWfcModelTokens = array of TWfcModelToken;
  TWfcModelIntegerArray = array of Integer;

  TWfcModelSampleShape = record
    Width: Integer;
    Height: Integer;
  end;
  TWfcModelSampleShapes = array of TWfcModelSampleShape;

  TWfcModelDirection = (
    wmdNorth,
    wmdEast,
    wmdSouth,
    wmdWest
  );
  TWfcModelDirections = set of TWfcModelDirection;
  TWfcModelBoundary = (wmbOpen, wmbWrap);
  TWfcModelSymmetry = (wmsNone, wmsD4);

  { TWfcModel }

  (*
    Immutable, one-layer learned-model data. Source samples retain their
    ordered shapes; SampleWidth and SampleHeight remain compatibility views of
    shape zero. Relation storage is dense and direction-major:

      ((Ord(direction) * ValueCount) + source) * ValueCount + target

    Counts retain their raw observed frequencies. A positive count represents
    an allowed edge when the model is adapted to TGraph.
  *)
  TWfcModel = class
  strict private
    FRank: Integer;
    FSampleShapes: TWfcModelSampleShapes;
    FBoundary: TWfcModelBoundary;
    FSymmetry: TWfcModelSymmetry;
    FDirections: TWfcModelDirections;
    FTokens: TWfcModelTokens;
    FWeights: TWfcModelIntegerArray;
    FRelations: TWfcModelIntegerArray;

    function GetSampleCount: Integer;
    function GetSampleWidth: Integer;
    function GetSampleHeight: Integer;
    function GetValueCount: Integer;
    function RelationIndex(const ADirection: TWfcModelDirection;
      const ASourceValue, ATargetValue: Integer): Integer;
    procedure ValidateValueIndex(const AValueIndex: Integer);
    procedure ValidateSampleIndex(const ASampleIndex: Integer);
    procedure Initialize(const ARank: Integer;
      const ASampleShapes: TWfcModelSampleShapes;
      const ABoundary: TWfcModelBoundary;
      const ASymmetry: TWfcModelSymmetry;
      const ADirections: TWfcModelDirections;
      const ATokens: TWfcModelTokens;
      const AWeights, ARelations: TWfcModelIntegerArray);
  public
    constructor Create(const ARank, ASampleWidth, ASampleHeight: Integer;
      const ABoundary: TWfcModelBoundary;
      const ASymmetry: TWfcModelSymmetry;
      const ADirections: TWfcModelDirections;
      const ATokens: TWfcModelTokens;
      const AWeights, ARelations: TWfcModelIntegerArray); overload;
    constructor Create(const ARank: Integer;
      const ASampleShapes: TWfcModelSampleShapes;
      const ABoundary: TWfcModelBoundary;
      const ASymmetry: TWfcModelSymmetry;
      const ADirections: TWfcModelDirections;
      const ATokens: TWfcModelTokens;
      const AWeights, ARelations: TWfcModelIntegerArray); overload;

    function SampleShapeAt(
      const ASampleIndex: Integer): TWfcModelSampleShape;
    function TokenAt(const AValueIndex: Integer): TWfcModelToken;
    function WeightAt(const AValueIndex: Integer): Integer;
    function RelationCount(const ADirection: TWfcModelDirection;
      const ASourceValue, ATargetValue: Integer): Integer;
    function FindToken(const AToken: TWfcModelToken): Integer;

    function CopySampleShapes: TWfcModelSampleShapes;
    function CopyTokens: TWfcModelTokens;
    function CopyWeights: TWfcModelIntegerArray;
    function CopyRelations: TWfcModelIntegerArray;

    property Rank: Integer read FRank;
    property SampleCount: Integer read GetSampleCount;
    property SampleWidth: Integer read GetSampleWidth;
    property SampleHeight: Integer read GetSampleHeight;
    property Boundary: TWfcModelBoundary read FBoundary;
    property Symmetry: TWfcModelSymmetry read FSymmetry;
    property Directions: TWfcModelDirections read FDirections;
    property ValueCount: Integer read GetValueCount;
  end;

  TWfcModels = array of TWfcModel;

const
  WFC_MODEL_MERGE_ALGORITHM_VERSION = 1;
  //Identifies the conversion from immutable model relations to the public
  //TGraph rule model, including explicit deny-all rows for finite learned
  //support. Increment when adapter semantics or observable rule construction
  //change incompatibly.
  WFC_MODEL_GRAPH_ADAPTER_VERSION = 1;

function MakeWfcModelSampleShape(const AWidth,
  AHeight: Integer): TWfcModelSampleShape;

function MergeWfcModels(const AModels: TWfcModels): TWfcModel;

function OppositeModelDirection(
  const ADirection: TWfcModelDirection): TWfcModelDirection;

function WfcModelTokenIsValid(const AToken: TWfcModelToken): Boolean;

procedure ApplyModelToGraph(const AModel: TWfcModel; const AGraph: TGraph);

implementation

const
  WFC_MODEL_CARDINAL_DIRECTIONS: TWfcModelDirections =
    [wmdNorth, wmdEast, wmdSouth, wmdWest];
  WFC_MODEL_HORIZONTAL_DIRECTIONS: TWfcModelDirections =
    [wmdEast, wmdWest];

function MakeWfcModelSampleShape(const AWidth,
  AHeight: Integer): TWfcModelSampleShape;
begin
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

function OppositeModelDirection(
  const ADirection: TWfcModelDirection): TWfcModelDirection;
begin
  case ADirection of
    wmdNorth:
      Result := wmdSouth;
    wmdEast:
      Result := wmdWest;
    wmdSouth:
      Result := wmdNorth;
    wmdWest:
      Result := wmdEast;
  else
    raise ERangeError.Create('unknown model direction');
  end;
end;

function CheckedRelationLength(const AValueCount: Integer): Integer;
var
  LSquare: Integer;
begin
  if AValueCount < 1 then
    raise EWfcModel.Create('model must contain at least one token');
  if AValueCount > High(Integer) div AValueCount then
    raise EWfcModel.Create('model relation dimensions overflow Integer');
  LSquare := AValueCount * AValueCount;
  if LSquare > High(Integer) div 4 then
    raise EWfcModel.Create('model relation dimensions overflow Integer');
  Result := 4 * LSquare;
end;

function CheckedSampleShapeCount(
  const ASampleShapes: TWfcModelSampleShapes): Integer;
var
  LLength: SizeInt;
begin
  LLength := Length(ASampleShapes);
  if LLength = 0 then
    raise EWfcModel.Create('model must contain at least one sample shape');
  if (LLength < 0) or
    ((LLength and (not SizeInt(High(Integer)))) <> 0) then
    raise EWfcModel.Create('model has too many sample shapes');
  Result := Integer(LLength);
end;

function CheckedModelCount(const AModels: TWfcModels): Integer;
var
  LLength: SizeInt;
begin
  LLength := Length(AModels);
  if LLength = 0 then
    raise EWfcModel.Create('cannot merge an empty model list');
  if (LLength < 0) or
    ((LLength and (not SizeInt(High(Integer)))) <> 0) then
    raise EWfcModel.Create('cannot merge too many models');
  Result := Integer(LLength);
end;

{$IFNDEF PAS2JS}
function IsValidUtf8(const AToken: TWfcModelToken): Boolean;
var
  B0: Byte;
  B1: Byte;
  I: Integer;
  LLength: Integer;

  function IsContinuation(const AIndex: Integer): Boolean;
  var
    B: Byte;
  begin
    if AIndex > LLength then
      Exit(False);
    B := Byte(AToken[AIndex]);
    Result := (B >= $80) and (B <= $BF);
  end;

begin
  I := 1;
  LLength := Length(AToken);
  while I <= LLength do
  begin
    B0 := Byte(AToken[I]);
    if B0 <= $7F then
      Inc(I)
    else if (B0 >= $C2) and (B0 <= $DF) then
    begin
      if not IsContinuation(I + 1) then
        Exit(False);
      Inc(I, 2);
    end
    else if B0 = $E0 then
    begin
      if I + 2 > LLength then
        Exit(False);
      B1 := Byte(AToken[I + 1]);
      if (B1 < $A0) or (B1 > $BF) or
          (not IsContinuation(I + 2)) then
        Exit(False);
      Inc(I, 3);
    end
    else if ((B0 >= $E1) and (B0 <= $EC)) or
        ((B0 >= $EE) and (B0 <= $EF)) then
    begin
      if (not IsContinuation(I + 1)) or
          (not IsContinuation(I + 2)) then
        Exit(False);
      Inc(I, 3);
    end
    else if B0 = $ED then
    begin
      if I + 2 > LLength then
        Exit(False);
      B1 := Byte(AToken[I + 1]);
      if (B1 < $80) or (B1 > $9F) or
          (not IsContinuation(I + 2)) then
        Exit(False);
      Inc(I, 3);
    end
    else if B0 = $F0 then
    begin
      if I + 3 > LLength then
        Exit(False);
      B1 := Byte(AToken[I + 1]);
      if (B1 < $90) or (B1 > $BF) or
          (not IsContinuation(I + 2)) or
          (not IsContinuation(I + 3)) then
        Exit(False);
      Inc(I, 4);
    end
    else if (B0 >= $F1) and (B0 <= $F3) then
    begin
      if (not IsContinuation(I + 1)) or
          (not IsContinuation(I + 2)) or
          (not IsContinuation(I + 3)) then
        Exit(False);
      Inc(I, 4);
    end
    else if B0 = $F4 then
    begin
      if I + 3 > LLength then
        Exit(False);
      B1 := Byte(AToken[I + 1]);
      if (B1 < $80) or (B1 > $8F) or
          (not IsContinuation(I + 2)) or
          (not IsContinuation(I + 3)) then
        Exit(False);
      Inc(I, 4);
    end
    else
      Exit(False);
  end;
  Result := True;
end;
{$ENDIF}

function TokenIsValidUtf8(const AToken: TWfcModelToken): Boolean;
{$IFDEF PAS2JS}
var
  I: Integer;
  LCodeUnit: Integer;
  LLowSurrogate: Integer;
{$ENDIF}
begin
  if Length(AToken) = 0 then
    Exit(False);
  {$IFDEF PAS2JS}
  //JavaScript strings are UTF-16. Require a sequence of Unicode scalar values
  //so every accepted model token has a canonical UTF-8 serialization.
  I := 1;
  while I <= Length(AToken) do
  begin
    LCodeUnit := Ord(AToken[I]);
    if (LCodeUnit >= $D800) and (LCodeUnit <= $DBFF) then
    begin
      if I = Length(AToken) then
        Exit(False);
      LLowSurrogate := Ord(AToken[I + 1]);
      if (LLowSurrogate < $DC00) or (LLowSurrogate > $DFFF) then
        Exit(False);
      Inc(I, 2);
    end
    else
    begin
      if (LCodeUnit >= $DC00) and (LCodeUnit <= $DFFF) then
        Exit(False);
      Inc(I);
    end;
  end;
  Result := True;
  {$ELSE}
  Result := IsValidUtf8(AToken);
  {$ENDIF}
end;

function WfcModelTokenIsValid(const AToken: TWfcModelToken): Boolean;
begin
  Result := TokenIsValidUtf8(AToken);
end;

function ModelTokenToGraphValue(const AToken: TWfcModelToken): TGraphValue;
begin
  {$IFDEF PAS2JS}
  Result := String(AToken);
  {$ELSE}
  Result := String(UTF8Decode(AToken));
  {$ENDIF}
end;

function GraphValueToModelToken(const AValue: TGraphValue): TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(AValue);
  {$ELSE}
  Result := UTF8Encode(UnicodeString(AValue));
  {$ENDIF}
end;

function ModelDirectionToGraphDirection(
  const ADirection: TWfcModelDirection): TGraphDirection;
begin
  //A TGraph rule is stored on the already assigned neighbor and keyed by that
  //neighbor's position relative to the candidate being validated. A learned
  //relation is expressed from source toward target, which reverses the rule
  //key. TGraph also historically names increasing row coordinates North while
  //the row-major learner names them South, so those two reversals cancel on Y.
  case ADirection of
    wmdNorth:
      Result := gdNorth;
    wmdEast:
      Result := gdWest;
    wmdSouth:
      Result := gdSouth;
    wmdWest:
      Result := gdEast;
  else
    raise ERangeError.Create('unknown model direction');
  end;
end;

{ TWfcModel }

function TWfcModel.GetSampleCount: Integer;
begin
  Result := Integer(Length(FSampleShapes));
end;

function TWfcModel.GetSampleWidth: Integer;
begin
  Result := FSampleShapes[0].Width;
end;

function TWfcModel.GetSampleHeight: Integer;
begin
  Result := FSampleShapes[0].Height;
end;

function TWfcModel.GetValueCount: Integer;
begin
  Result := Length(FTokens);
end;

procedure TWfcModel.ValidateValueIndex(const AValueIndex: Integer);
begin
  if (AValueIndex < 0) or (AValueIndex >= ValueCount) then
    raise ERangeError.CreateFmt('model value index out of bounds [%d]',
      [AValueIndex]);
end;

procedure TWfcModel.ValidateSampleIndex(const ASampleIndex: Integer);
begin
  if (ASampleIndex < 0) or (ASampleIndex >= SampleCount) then
    raise ERangeError.CreateFmt('model sample index out of bounds [%d]',
      [ASampleIndex]);
end;

function TWfcModel.RelationIndex(const ADirection: TWfcModelDirection;
  const ASourceValue, ATargetValue: Integer): Integer;
begin
  case ADirection of
    wmdNorth, wmdEast, wmdSouth, wmdWest:
      ;
  else
    raise ERangeError.CreateFmt('unknown model direction [%d]',
      [Ord(ADirection)]);
  end;
  ValidateValueIndex(ASourceValue);
  ValidateValueIndex(ATargetValue);
  Result := ((Ord(ADirection) * ValueCount) + ASourceValue) *
    ValueCount + ATargetValue;
end;

constructor TWfcModel.Create(const ARank, ASampleWidth,
  ASampleHeight: Integer; const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry;
  const ADirections: TWfcModelDirections; const ATokens: TWfcModelTokens;
  const AWeights, ARelations: TWfcModelIntegerArray);
var
  LSampleShapes: TWfcModelSampleShapes;
begin
  inherited Create;
  SetLength(LSampleShapes, 1);
  LSampleShapes[0] := MakeWfcModelSampleShape(ASampleWidth,
    ASampleHeight);
  Initialize(ARank, LSampleShapes, ABoundary, ASymmetry, ADirections,
    ATokens, AWeights, ARelations);
end;

constructor TWfcModel.Create(const ARank: Integer;
  const ASampleShapes: TWfcModelSampleShapes;
  const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry;
  const ADirections: TWfcModelDirections; const ATokens: TWfcModelTokens;
  const AWeights, ARelations: TWfcModelIntegerArray);
begin
  inherited Create;
  Initialize(ARank, ASampleShapes, ABoundary, ASymmetry, ADirections,
    ATokens, AWeights, ARelations);
end;

procedure TWfcModel.Initialize(const ARank: Integer;
  const ASampleShapes: TWfcModelSampleShapes;
  const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry;
  const ADirections: TWfcModelDirections; const ATokens: TWfcModelTokens;
  const AWeights, ARelations: TWfcModelIntegerArray);
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LSampleCount: Integer;
  LShapeIndex: Integer;
  LExpectedRelations: Integer;
  LOpposite: TWfcModelDirection;
  LValueCount: Integer;
begin
  case ABoundary of
    wmbOpen, wmbWrap:
      ;
  else
    raise EWfcModel.CreateFmt('unknown model boundary [%d]',
      [Ord(ABoundary)]);
  end;
  case ASymmetry of
    wmsNone, wmsD4:
      ;
  else
    raise EWfcModel.CreateFmt('unknown model symmetry [%d]',
      [Ord(ASymmetry)]);
  end;
  if (ARank <> 1) and (ARank <> 2) then
    raise EWfcModel.CreateFmt('model rank must be 1 or 2 [%d]', [ARank]);
  LSampleCount := CheckedSampleShapeCount(ASampleShapes);
  for LShapeIndex := 0 to LSampleCount - 1 do
  begin
    if (ASampleShapes[LShapeIndex].Width < 1) or
        (ASampleShapes[LShapeIndex].Height < 1) then
      raise EWfcModel.CreateFmt(
        'model sample dimensions must be positive [%d: %d x %d]',
        [LShapeIndex, ASampleShapes[LShapeIndex].Width,
          ASampleShapes[LShapeIndex].Height]);
    if (ARank = 1) and (ASampleShapes[LShapeIndex].Height <> 1) then
      raise EWfcModel.CreateFmt(
        'rank-1 model sample height must be 1 [%d: %d]',
        [LShapeIndex, ASampleShapes[LShapeIndex].Height]);
  end;

  if ARank = 1 then
  begin
    if ADirections <> WFC_MODEL_HORIZONTAL_DIRECTIONS then
      raise EWfcModel.Create(
        'rank-1 model directions must be exactly east and west');
    if ASymmetry = wmsD4 then
      raise EWfcModel.Create('rank-1 model does not support D4 symmetry');
  end
  else if ADirections <> WFC_MODEL_CARDINAL_DIRECTIONS then
    raise EWfcModel.Create(
      'rank-2 model directions must contain every cardinal direction');

  LValueCount := Length(ATokens);
  LExpectedRelations := CheckedRelationLength(LValueCount);
  if Length(AWeights) <> LValueCount then
    raise EWfcModel.CreateFmt(
      'model weight count must match token count [%d <> %d]',
      [Length(AWeights), LValueCount]);
  if Length(ARelations) <> LExpectedRelations then
    raise EWfcModel.CreateFmt(
      'model relation count has invalid length [%d <> %d]',
      [Length(ARelations), LExpectedRelations]);

  for I := 0 to Pred(LValueCount) do
  begin
    if not TokenIsValidUtf8(ATokens[I]) then
      raise EWfcModel.CreateFmt(
        'model token must be nonempty, well-formed UTF-8 [%d]', [I]);
    for J := 0 to Pred(I) do
      if ATokens[I] = ATokens[J] then
        raise EWfcModel.CreateFmt('model tokens must be unique [%d, %d]',
          [J, I]);
    if AWeights[I] < 1 then
      raise EWfcModel.CreateFmt('model weight must be positive [%d]', [I]);
  end;

  for I := 0 to Pred(LExpectedRelations) do
    if ARelations[I] < 0 then
      raise EWfcModel.CreateFmt(
        'model relation count must be nonnegative [%d]', [I]);

  for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    for I := 0 to Pred(LValueCount) do
      for J := 0 to Pred(LValueCount) do
      begin
        if (not (D in ADirections)) and
            (ARelations[((Ord(D) * LValueCount) + I) *
              LValueCount + J] <> 0) then
          raise EWfcModel.CreateFmt(
            'inactive model direction has a nonzero relation [%d, %d, %d]',
            [Ord(D), I, J]);

        LOpposite := OppositeModelDirection(D);
        if ARelations[((Ord(D) * LValueCount) + I) *
             LValueCount + J] <>
            ARelations[((Ord(LOpposite) * LValueCount) + J) *
             LValueCount + I] then
          raise EWfcModel.CreateFmt(
            'opposite model relations must be reciprocal [%d, %d, %d]',
            [Ord(D), I, J]);
      end;

  FRank := ARank;
  SetLength(FSampleShapes, LSampleCount);
  for I := 0 to LSampleCount - 1 do
    FSampleShapes[I] := ASampleShapes[I];
  FBoundary := ABoundary;
  FSymmetry := ASymmetry;
  FDirections := ADirections;

  SetLength(FTokens, LValueCount);
  SetLength(FWeights, LValueCount);
  for I := 0 to Pred(LValueCount) do
  begin
    FTokens[I] := ATokens[I];
    FWeights[I] := AWeights[I];
  end;

  SetLength(FRelations, LExpectedRelations);
  for I := 0 to Pred(LExpectedRelations) do
    FRelations[I] := ARelations[I];
end;

function TWfcModel.SampleShapeAt(
  const ASampleIndex: Integer): TWfcModelSampleShape;
begin
  ValidateSampleIndex(ASampleIndex);
  Result := FSampleShapes[ASampleIndex];
end;

function TWfcModel.TokenAt(const AValueIndex: Integer): TWfcModelToken;
begin
  ValidateValueIndex(AValueIndex);
  Result := FTokens[AValueIndex];
end;

function TWfcModel.WeightAt(const AValueIndex: Integer): Integer;
begin
  ValidateValueIndex(AValueIndex);
  Result := FWeights[AValueIndex];
end;

function TWfcModel.RelationCount(const ADirection: TWfcModelDirection;
  const ASourceValue, ATargetValue: Integer): Integer;
begin
  Result := FRelations[RelationIndex(ADirection, ASourceValue, ATargetValue)];
end;

function TWfcModel.FindToken(const AToken: TWfcModelToken): Integer;
begin
  for Result := 0 to Pred(ValueCount) do
    if FTokens[Result] = AToken then
      Exit;
  Result := -1;
end;

function TWfcModel.CopySampleShapes: TWfcModelSampleShapes;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, SampleCount);
  for I := 0 to Pred(SampleCount) do
    Result[I] := FSampleShapes[I];
end;

function TWfcModel.CopyTokens: TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, ValueCount);
  for I := 0 to Pred(ValueCount) do
    Result[I] := FTokens[I];
end;

function TWfcModel.CopyWeights: TWfcModelIntegerArray;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FWeights));
  for I := 0 to High(FWeights) do
    Result[I] := FWeights[I];
end;

function TWfcModel.CopyRelations: TWfcModelIntegerArray;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FRelations));
  for I := 0 to High(FRelations) do
    Result[I] := FRelations[I];
end;

function MergeWfcModels(const AModels: TWfcModels): TWfcModel;
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LBase: TWfcModel;
  LGlobalSource: Integer;
  LGlobalTarget: Integer;
  LLocalToGlobal: TWfcModelIntegerArray;
  LModel: TWfcModel;
  LModelCount: Integer;
  LModelIndex: Integer;
  LRelationIndex: Integer;
  LRelations: TWfcModelIntegerArray;
  LSampleIndex: Integer;
  LSampleOffset: Integer;
  LSampleShapes: TWfcModelSampleShapes;
  LToken: TWfcModelToken;
  LTokens: TWfcModelTokens;
  LTotalSamples: Integer;
  LValueIndex: Integer;
  LWeights: TWfcModelIntegerArray;

  function FindMergedToken(const AToken: TWfcModelToken): Integer;
  var
    LIndex: Integer;
  begin
    for LIndex := 0 to High(LTokens) do
      if LTokens[LIndex] = AToken then
        Exit(LIndex);
    Result := -1;
  end;

  procedure CheckedAdd(var ATarget: Integer; const ADelta: Integer;
    const ALabel: String);
  begin
    if ADelta < 0 then
      raise EWfcModel.Create(ALabel + ' cannot be negative');
    if ATarget > High(Integer) - ADelta then
      raise EWfcModel.Create(ALabel + ' exceeds the supported Integer range');
    Inc(ATarget, ADelta);
  end;

begin
  Result := nil;
  LModelCount := CheckedModelCount(AModels);
  for LModelIndex := 0 to LModelCount - 1 do
    if not Assigned(AModels[LModelIndex]) then
      raise EWfcModel.CreateFmt('cannot merge an unassigned model [%d]',
        [LModelIndex]);

  LBase := AModels[0];
  LTotalSamples := 0;
  SetLength(LTokens, 0);
  for LModelIndex := 0 to LModelCount - 1 do
  begin
    LModel := AModels[LModelIndex];
    if LModel.Rank <> LBase.Rank then
      raise EWfcModel.Create('cannot merge models with different ranks');
    if LModel.Boundary <> LBase.Boundary then
      raise EWfcModel.Create(
        'cannot merge models with different boundary policies');
    if LModel.Symmetry <> LBase.Symmetry then
      raise EWfcModel.Create(
        'cannot merge models with different symmetry policies');
    if LModel.Directions <> LBase.Directions then
      raise EWfcModel.Create(
        'cannot merge models with different direction policies');
    if LModel.SampleCount > High(Integer) - LTotalSamples then
      raise EWfcModel.Create('merged model has too many sample shapes');
    Inc(LTotalSamples, LModel.SampleCount);

    for I := 0 to Pred(LModel.ValueCount) do
    begin
      LToken := LModel.TokenAt(I);
      if FindMergedToken(LToken) < 0 then
      begin
        if Length(LTokens) = High(Integer) then
          raise EWfcModel.Create('merged model has too many tokens');
        SetLength(LTokens, Length(LTokens) + 1);
        LTokens[High(LTokens)] := LToken;
      end;
    end;
  end;

  SetLength(LSampleShapes, LTotalSamples);
  LSampleOffset := 0;
  for LModelIndex := 0 to LModelCount - 1 do
  begin
    LModel := AModels[LModelIndex];
    for LSampleIndex := 0 to Pred(LModel.SampleCount) do
    begin
      LSampleShapes[LSampleOffset] :=
        LModel.SampleShapeAt(LSampleIndex);
      Inc(LSampleOffset);
    end;
  end;

  SetLength(LWeights, Length(LTokens));
  SetLength(LRelations, CheckedRelationLength(Length(LTokens)));
  for LModelIndex := 0 to LModelCount - 1 do
  begin
    LModel := AModels[LModelIndex];
    SetLength(LLocalToGlobal, LModel.ValueCount);
    for I := 0 to Pred(LModel.ValueCount) do
    begin
      LValueIndex := FindMergedToken(LModel.TokenAt(I));
      if LValueIndex < 0 then
        raise EWfcModel.Create('internal merged-token lookup failed');
      LLocalToGlobal[I] := LValueIndex;
      CheckedAdd(LWeights[LValueIndex], LModel.WeightAt(I),
        'merged model weight');
    end;

    for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
      for I := 0 to Pred(LModel.ValueCount) do
        for J := 0 to Pred(LModel.ValueCount) do
        begin
          LGlobalSource := LLocalToGlobal[I];
          LGlobalTarget := LLocalToGlobal[J];
          LRelationIndex := ((Ord(D) * Length(LTokens) + LGlobalSource)
            * Length(LTokens)) + LGlobalTarget;
          CheckedAdd(LRelations[LRelationIndex],
            LModel.RelationCount(D, I, J), 'merged model relation count');
        end;
  end;

  Result := TWfcModel.Create(LBase.Rank, LSampleShapes, LBase.Boundary,
    LBase.Symmetry, LBase.Directions, LTokens, LWeights, LRelations);
end;

procedure ApplyModelToGraph(const AModel: TWfcModel; const AGraph: TGraph);
type
  TGraphRuleMatrix = array of TGraphRules;
  TGraphDirectionArray = array of TGraphDirection;
  TGraphDirectionMatrix = array of TGraphDirectionArray;
  TGraphDirectionsArray = array of TGraphDirections;
  TIntegerArray = array of Integer;
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LGraphDirection: TGraphDirection;
  LGraphValues: TGraphValues;
  LDenyAllDirections: TGraphDirectionsArray;
  LRuleCount: TIntegerArray;
  LRuleDirections: TGraphDirectionMatrix;
  LRuleSlots: TIntegerArray;
  LRules: TGraphRuleMatrix;
  LSlot: Integer;
  LTargetCount: Integer;

  function RuleSlotIndex(const AValue: Integer;
    const ADirection: TGraphDirection): Integer;
  begin
    Result := (AValue * (Ord(High(TGraphDirection)) + 1))
      + Ord(ADirection);
  end;

  procedure EnsureRuleSlot(const AValue: Integer;
    const ADirection: TGraphDirection);
  var
    LIndex: Integer;
  begin
    LIndex := RuleSlotIndex(AValue, ADirection);
    if LRuleSlots[LIndex] >= 0 then
      Exit;
    LRuleSlots[LIndex] := LRuleCount[AValue];
    Inc(LRuleCount[AValue]);
    SetLength(LRuleDirections[AValue], LRuleCount[AValue]);
    LRuleDirections[AValue][Pred(LRuleCount[AValue])] := ADirection;
  end;
begin
  if not Assigned(AModel) then
    raise EWfcModel.Create('model must be assigned');
  if not Assigned(AGraph) then
    raise EWfcModel.Create('target graph must be assigned');
  if AGraph.Running then
    raise EWfcModel.Create(
      'target graph pass cannot be modified while the pipeline is running');
  if AGraph.RuleGroups.Count <> 0 then
    raise EWfcModel.Create(
      'target graph pass must be empty before applying a model');

  //Preflight all conversion and representability checks before mutating the
  //target pass. TGraphValue can use a host code page on native builds, so
  //distinct UTF-8 model tokens must remain distinct after conversion.
  SetLength(LGraphValues, AModel.ValueCount);
  for I := 0 to Pred(AModel.ValueCount) do
  begin
    LGraphValues[I] := ModelTokenToGraphValue(AModel.TokenAt(I));
    if Length(LGraphValues[I]) = 0 then
      raise EWfcModel.CreateFmt(
        'model token converts to an empty graph value [%d]', [I]);
    if GraphValueToModelToken(LGraphValues[I]) <> AModel.TokenAt(I) then
      raise EWfcModel.CreateFmt(
        'model token cannot be represented by the target graph string type [%d]',
        [I]);
    for J := 0 to Pred(I) do
      if LGraphValues[I] = LGraphValues[J] then
        raise EWfcModel.CreateFmt(
          'model token conversion is not unique [%d, %d]', [J, I]);
  end;

  //Build the complete public rule arrays before mutating the target graph.
  //Calling NewRule once per dense edge makes every call run inverse-rule
  //fixed-point synchronization over the model built so far. TWfcModel has
  //already proved that every relation is reciprocal, and imported rules are
  //never required, so that repeated closure is unnecessary here. Discover
  //slots in the same edge order as the fluent builder to preserve observable
  //TGraphRuleGroup.Rules ordering as well as solver semantics.
  SetLength(LRuleCount, AModel.ValueCount);
  SetLength(LRuleDirections, AModel.ValueCount);
  SetLength(LDenyAllDirections, AModel.ValueCount);
  SetLength(LRuleSlots, AModel.ValueCount *
    (Ord(High(TGraphDirection)) + 1));
  for I := 0 to High(LRuleSlots) do
    LRuleSlots[I] := -1;

  for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    if D in AModel.Directions then
    begin
      LGraphDirection := ModelDirectionToGraphDirection(D);
      for I := 0 to Pred(AModel.ValueCount) do
        for J := 0 to Pred(AModel.ValueCount) do
          if AModel.RelationCount(D, I, J) > 0 then
          begin
            EnsureRuleSlot(I, LGraphDirection);
            EnsureRuleSlot(J, InverseOfDir(LGraphDirection));
          end;
    end;

  SetLength(LRules, AModel.ValueCount);
  for I := 0 to Pred(AModel.ValueCount) do
  begin
    SetLength(LRules[I], LRuleCount[I]);
    for J := 0 to Pred(LRuleCount[I]) do
    begin
      LRules[I][J].Key := LRuleDirections[I][J];
      LRules[I][J].Info := False;
      SetLength(LRules[I][J].Value, 0);
    end;
  end;

  for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    if D in AModel.Directions then
    begin
      LGraphDirection := ModelDirectionToGraphDirection(D);
      for I := 0 to Pred(AModel.ValueCount) do
      begin
        LTargetCount := 0;
        for J := 0 to Pred(AModel.ValueCount) do
          if AModel.RelationCount(D, I, J) > 0 then
            Inc(LTargetCount);
        if LTargetCount = 0 then
          Include(LDenyAllDirections[I], LGraphDirection)
        else
        begin
          LSlot := LRuleSlots[RuleSlotIndex(I, LGraphDirection)];
          SetLength(LRules[I][LSlot].Value, LTargetCount);
          LTargetCount := 0;
          for J := 0 to Pred(AModel.ValueCount) do
            if AModel.RelationCount(D, I, J) > 0 then
            begin
              LRules[I][LSlot].Value[LTargetCount] := LGraphValues[J];
              Inc(LTargetCount);
            end;
        end;
      end;
    end;

  for I := 0 to Pred(AModel.ValueCount) do
    AGraph.AddValue(LGraphValues[I], AModel.WeightAt(I));

  for I := 0 to Pred(AModel.ValueCount) do
    AGraph.Rules[LGraphValues[I]].Rules := LRules[I];

  //An absent direction remains the graph's historical wildcard. Active model
  //directions are finite: a source row with no learned targets therefore
  //denies every present neighbor in that direction. Install these only after
  //the finite rule arrays so nonempty models retain their exact public rule
  //ordering.
  for I := 0 to Pred(AModel.ValueCount) do
    if LDenyAllDirections[I] <> [] then
      AGraph.Rules[LGraphValues[I]].DenyAll(LDenyAllDirections[I]);
end;

end.
