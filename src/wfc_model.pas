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
    Immutable, one-layer learned-model data. Relation storage is dense and
    direction-major:

      ((Ord(direction) * ValueCount) + source) * ValueCount + target

    Counts retain their raw observed frequencies. A positive count represents
    an allowed edge when the model is adapted to TGraph.
  *)
  TWfcModel = class
  strict private
    FRank: Integer;
    FSampleWidth: Integer;
    FSampleHeight: Integer;
    FBoundary: TWfcModelBoundary;
    FSymmetry: TWfcModelSymmetry;
    FDirections: TWfcModelDirections;
    FTokens: TWfcModelTokens;
    FWeights: TWfcModelIntegerArray;
    FRelations: TWfcModelIntegerArray;

    function GetValueCount: Integer;
    function RelationIndex(const ADirection: TWfcModelDirection;
      const ASourceValue, ATargetValue: Integer): Integer;
    procedure ValidateValueIndex(const AValueIndex: Integer);
  public
    constructor Create(const ARank, ASampleWidth, ASampleHeight: Integer;
      const ABoundary: TWfcModelBoundary;
      const ASymmetry: TWfcModelSymmetry;
      const ADirections: TWfcModelDirections;
      const ATokens: TWfcModelTokens;
      const AWeights, ARelations: TWfcModelIntegerArray);

    function TokenAt(const AValueIndex: Integer): TWfcModelToken;
    function WeightAt(const AValueIndex: Integer): Integer;
    function RelationCount(const ADirection: TWfcModelDirection;
      const ASourceValue, ATargetValue: Integer): Integer;
    function FindToken(const AToken: TWfcModelToken): Integer;

    function CopyTokens: TWfcModelTokens;
    function CopyWeights: TWfcModelIntegerArray;
    function CopyRelations: TWfcModelIntegerArray;

    property Rank: Integer read FRank;
    property SampleWidth: Integer read FSampleWidth;
    property SampleHeight: Integer read FSampleHeight;
    property Boundary: TWfcModelBoundary read FBoundary;
    property Symmetry: TWfcModelSymmetry read FSymmetry;
    property Directions: TWfcModelDirections read FDirections;
    property ValueCount: Integer read GetValueCount;
  end;

function OppositeModelDirection(
  const ADirection: TWfcModelDirection): TWfcModelDirection;

procedure ApplyModelToGraph(const AModel: TWfcModel; const AGraph: TGraph);

implementation

const
  WFC_MODEL_CARDINAL_DIRECTIONS: TWfcModelDirections =
    [wmdNorth, wmdEast, wmdSouth, wmdWest];
  WFC_MODEL_HORIZONTAL_DIRECTIONS: TWfcModelDirections =
    [wmdEast, wmdWest];

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
  case ADirection of
    wmdNorth:
      Result := gdNorth;
    wmdEast:
      Result := gdEast;
    wmdSouth:
      Result := gdSouth;
    wmdWest:
      Result := gdWest;
  else
    raise ERangeError.Create('unknown model direction');
  end;
end;

{ TWfcModel }

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
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LExpectedRelations: Integer;
  LOpposite: TWfcModelDirection;
  LValueCount: Integer;
begin
  inherited Create;

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
  if (ASampleWidth < 1) or (ASampleHeight < 1) then
    raise EWfcModel.CreateFmt(
      'model sample dimensions must be positive [%d x %d]',
      [ASampleWidth, ASampleHeight]);

  if ARank = 1 then
  begin
    if ASampleHeight <> 1 then
      raise EWfcModel.CreateFmt(
        'rank-1 model sample height must be 1 [%d]', [ASampleHeight]);
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
  FSampleWidth := ASampleWidth;
  FSampleHeight := ASampleHeight;
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

procedure ApplyModelToGraph(const AModel: TWfcModel; const AGraph: TGraph);
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LGraphDirection: TGraphDirection;
  LGraphValues: TGraphValues;
  LTargets: TGraphValues;
  LTargetCount: Integer;
begin
  if not Assigned(AModel) then
    raise EWfcModel.Create('model must be assigned');
  if not Assigned(AGraph) then
    raise EWfcModel.Create('target graph must be assigned');
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

  for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    if D in AModel.Directions then
      for I := 0 to Pred(AModel.ValueCount) do
      begin
        LTargetCount := 0;
        for J := 0 to Pred(AModel.ValueCount) do
          if AModel.RelationCount(D, I, J) > 0 then
            Inc(LTargetCount);
        if LTargetCount = 0 then
          raise EWfcModel.CreateFmt(
            'empty-support-not-representable: direction %d, source %d',
            [Ord(D), I]);
      end;

  for I := 0 to Pred(AModel.ValueCount) do
    AGraph.AddValue(LGraphValues[I], AModel.WeightAt(I));

  for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    if D in AModel.Directions then
    begin
      LGraphDirection := ModelDirectionToGraphDirection(D);
      for I := 0 to Pred(AModel.ValueCount) do
      begin
        SetLength(LTargets, 0);
        for J := 0 to Pred(AModel.ValueCount) do
          if AModel.RelationCount(D, I, J) > 0 then
          begin
            SetLength(LTargets, Length(LTargets) + 1);
            LTargets[High(LTargets)] := LGraphValues[J];
          end;
        AGraph.Rules[LGraphValues[I]].NewRule(
          [LGraphDirection], LTargets);
      end;
    end;
end;

end.
