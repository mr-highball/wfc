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
unit wfc_rule_model;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_model;

const
  { Immutable pass-local rule semantics. }
  WFC_RULE_MODEL_VERSION = 1;
  { Conversion from TWfcRuleModel to one empty public TGraph pass. }
  WFC_RULE_GRAPH_ADAPTER_VERSION = 1;
  { Portable FNV-1a encoding of the complete semantic model. }
  WFC_RULE_MODEL_SIGNATURE_VERSION = 1;

  { Version-1 portability and denial-of-service boundaries. The value limit
    bounds quadratic token/conversion uniqueness checks. The aggregate target
    limit also bounds reciprocal-closure lookup work and prepared graph data. }
  WFC_RULE_LIMITS_VERSION = 1;
  WFC_RULE_MAX_VALUE_COUNT = 1024;
  WFC_RULE_MAX_RULE_COUNT = WFC_RULE_MAX_VALUE_COUNT * 6;
  WFC_RULE_MAX_TOTAL_TARGET_COUNT = 65536;

type
  EWfcRuleModel = class(Exception);

  TWfcRuleState = (wrsAllow, wrsDeny);

  { Constructor input. TargetIndices is caller-owned and is deep-copied. }
  TWfcRuleRow = record
    OwnerIndex: Integer;
    Direction: TGraphDirection;
    State: TWfcRuleState;
    Required: Boolean;
    TargetIndices: TWfcModelIntegerArray;
  end;
  TWfcRuleRows = array of TWfcRuleRow;

  TWfcRuleModelSignature = Cardinal;

  { Immutable normalized definition of one pass's public values and local
    neighbor rules. An absent owner/direction row is the legacy wildcard;
    explicit deny and finite allow rows are stored separately. }
  TWfcRuleModel = class
  strict private
    FRank: Integer;
    FTokens: TWfcModelTokens;
    FWeights: TWfcModelIntegerArray;
    FRows: TWfcRuleRows;
    FSignature: TWfcRuleModelSignature;
    procedure Initialize(const ARank: Integer;
      const ATokens: TWfcModelTokens;
      const AWeights: TWfcModelIntegerArray;
      const ARows: TWfcRuleRows);
    procedure ValidateValueIndex(const AValueIndex: Integer);
    procedure ValidateRuleIndex(const ARuleIndex: Integer);
    function GetValueCount: Integer;
    function GetRuleCount: Integer;
  public
    constructor Create(const ARank: Integer;
      const ATokens: TWfcModelTokens;
      const AWeights: TWfcModelIntegerArray;
      const ARows: TWfcRuleRows);

    function TokenAt(const AValueIndex: Integer): TWfcModelToken;
    function FindToken(const AToken: TWfcModelToken): Integer;
    function WeightAt(const AValueIndex: Integer): Integer;
    function CopyTokens: TWfcModelTokens;
    function CopyWeights: TWfcModelIntegerArray;

    function RuleOwnerAt(const ARuleIndex: Integer): Integer;
    function RuleDirectionAt(
      const ARuleIndex: Integer): TGraphDirection;
    function RuleStateAt(const ARuleIndex: Integer): TWfcRuleState;
    function RuleRequiredAt(const ARuleIndex: Integer): Boolean;
    function RuleTargetCountAt(const ARuleIndex: Integer): Integer;
    function RuleTargetAt(const ARuleIndex,
      ATargetOrdinal: Integer): Integer;
    function CopyRuleTargets(
      const ARuleIndex: Integer): TWfcModelIntegerArray;

    property Rank: Integer read FRank;
    property ValueCount: Integer read GetValueCount;
    property RuleCount: Integer read GetRuleCount;
    property Signature: TWfcRuleModelSignature read FSignature;
  end;

function MakeWfcAllowRuleRow(const AOwnerIndex: Integer;
  const ADirection: TGraphDirection; const ARequired: Boolean;
  const ATargetIndices: TWfcModelIntegerArray): TWfcRuleRow;
function MakeWfcDenyRuleRow(const AOwnerIndex: Integer;
  const ADirection: TGraphDirection): TWfcRuleRow;

function CalculateWfcRuleModelSignature(
  const AModel: TWfcRuleModel): TWfcRuleModelSignature;
function WfcRuleModelSignatureHex(
  const ASignature: TWfcRuleModelSignature): String;

{ Applies a completely validated model to the active pass. The pass must have
  no registered values or public rule groups. All conversions and rule arrays
  are prepared before the first graph mutation. }
procedure ApplyRuleModelToGraph(const AModel: TWfcRuleModel;
  const AGraph: TGraph);

implementation

uses
  wfc_text_codec;

const
  RULE_DIRECTION_COUNT = Ord(High(TGraphDirection)) + 1;

type
  TGraphRuleMatrix = array of TGraphRules;
  TGraphDirectionsArray = array of TGraphDirections;
  TIntegerArray = array of Integer;

function DirectionIsKnown(const ADirection: TGraphDirection): Boolean;
begin
  case ADirection of
    gdNorth, gdEast, gdSouth, gdWest, gdUp, gdDown:
      Result := True;
  else
    Result := False;
  end;
end;

function DirectionIsActive(const ARank: Integer;
  const ADirection: TGraphDirection): Boolean;
begin
  case ARank of
    1:
      Result := ADirection in [gdEast, gdWest];
    2:
      Result := ADirection in [gdNorth, gdEast, gdSouth, gdWest];
    3:
      Result := DirectionIsKnown(ADirection);
  else
    Result := False;
  end;
end;

function OppositeDirection(
  const ADirection: TGraphDirection): TGraphDirection;
begin
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
  else
    raise EWfcRuleModel.Create('unknown rule direction');
  end;
end;

function CheckedRuleSlotCount(const AValueCount: Integer): Integer;
begin
  if AValueCount < 0 then
    raise EWfcRuleModel.Create('rule value count cannot be negative');
  if AValueCount > (High(Integer) div RULE_DIRECTION_COUNT) then
    raise EWfcRuleModel.Create('rule model is too large');
  Result := AValueCount * RULE_DIRECTION_COUNT;
end;

function RuleSlot(const AOwnerIndex: Integer;
  const ADirection: TGraphDirection): Integer;
begin
  Result := AOwnerIndex * RULE_DIRECTION_COUNT + Ord(ADirection);
end;

function ContainsTarget(const ATargets: TWfcModelIntegerArray;
  const AValue: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(ATargets) - 1 do
    if ATargets[I] = AValue then
      Exit(True);
  Result := False;
end;

function MakeWfcAllowRuleRow(const AOwnerIndex: Integer;
  const ADirection: TGraphDirection; const ARequired: Boolean;
  const ATargetIndices: TWfcModelIntegerArray): TWfcRuleRow;
begin
  Result := Default(TWfcRuleRow);
  Result.OwnerIndex := AOwnerIndex;
  Result.Direction := ADirection;
  Result.State := wrsAllow;
  Result.Required := ARequired;
  Result.TargetIndices := ATargetIndices;
end;

function MakeWfcDenyRuleRow(const AOwnerIndex: Integer;
  const ADirection: TGraphDirection): TWfcRuleRow;
begin
  Result := Default(TWfcRuleRow);
  Result.OwnerIndex := AOwnerIndex;
  Result.Direction := ADirection;
  Result.State := wrsDeny;
  Result.Required := False;
  Result.TargetIndices := nil;
end;

constructor TWfcRuleModel.Create(const ARank: Integer;
  const ATokens: TWfcModelTokens;
  const AWeights: TWfcModelIntegerArray;
  const ARows: TWfcRuleRows);
begin
  inherited Create;
  Initialize(ARank, ATokens, AWeights, ARows);
end;

procedure TWfcRuleModel.Initialize(const ARank: Integer;
  const ATokens: TWfcModelTokens;
  const AWeights: TWfcModelIntegerArray;
  const ARows: TWfcRuleRows);
var
  I: Integer;
  J: Integer;
  LInverseIndex: Integer;
  LInverseRow: TWfcRuleRow;
  LInverseSlot: Integer;
  LLookup: TIntegerArray;
  LPreviousSlot: Integer;
  LSlot: Integer;
  LTarget: Integer;
  LTotalTargetCount: Integer;
  LValueCount: Integer;
begin
  if (ARank < 1) or (ARank > 3) then
    raise EWfcRuleModel.CreateFmt(
      'rule model rank must be 1, 2, or 3 [%d]', [ARank]);

  LValueCount := Length(ATokens);
  if LValueCount < 1 then
    raise EWfcRuleModel.Create('rule model must contain at least one value');
  if LValueCount > WFC_RULE_MAX_VALUE_COUNT then
    raise EWfcRuleModel.Create(
      'rule model value count exceeds the version-1 limit');
  if Length(AWeights) <> LValueCount then
    raise EWfcRuleModel.CreateFmt(
      'rule model weight count must match value count [%d <> %d]',
      [Length(AWeights), LValueCount]);

  if Length(ARows) > WFC_RULE_MAX_RULE_COUNT then
    raise EWfcRuleModel.Create(
      'rule model row count exceeds the version-1 limit');
  LTotalTargetCount := 0;
  for I := 0 to Length(ARows) - 1 do
  begin
    if Length(ARows[I].TargetIndices) >
        WFC_RULE_MAX_TOTAL_TARGET_COUNT - LTotalTargetCount then
      raise EWfcRuleModel.Create(
        'rule model target count exceeds the version-1 aggregate limit');
    Inc(LTotalTargetCount, Length(ARows[I].TargetIndices));
  end;

  for I := 0 to LValueCount - 1 do
  begin
    if not WfcModelTokenIsValid(ATokens[I]) then
      raise EWfcRuleModel.CreateFmt(
        'rule model token must be nonempty, well-formed UTF-8 [%d]', [I]);
    for J := 0 to I - 1 do
      if ATokens[I] = ATokens[J] then
        raise EWfcRuleModel.CreateFmt(
          'rule model tokens must be unique [%d, %d]', [J, I]);
    if AWeights[I] < 1 then
      raise EWfcRuleModel.CreateFmt(
        'rule model weight must be positive [%d]', [I]);
  end;

  SetLength(LLookup, CheckedRuleSlotCount(LValueCount));
  for I := 0 to Length(LLookup) - 1 do
    LLookup[I] := -1;
  if Length(ARows) > Length(LLookup) then
    raise EWfcRuleModel.Create('rule model has too many rule rows');

  LPreviousSlot := -1;
  for I := 0 to Length(ARows) - 1 do
  begin
    if (ARows[I].OwnerIndex < 0) or
        (ARows[I].OwnerIndex >= LValueCount) then
      raise EWfcRuleModel.CreateFmt(
        'rule owner index is out of range [%d]', [I]);
    if not DirectionIsKnown(ARows[I].Direction) then
      raise EWfcRuleModel.CreateFmt('rule direction is unknown [%d]', [I]);
    if not DirectionIsActive(ARank, ARows[I].Direction) then
      raise EWfcRuleModel.CreateFmt(
        'rule direction is inactive for rank %d [%d]', [ARank, I]);
    case ARows[I].State of
      wrsAllow, wrsDeny:
        ;
    else
      raise EWfcRuleModel.CreateFmt('rule state is unknown [%d]', [I]);
    end;

    LSlot := RuleSlot(ARows[I].OwnerIndex, ARows[I].Direction);
    if LSlot <= LPreviousSlot then
      raise EWfcRuleModel.Create(
        'rule rows must be unique and strictly ordered by owner and direction');
    LPreviousSlot := LSlot;
    LLookup[LSlot] := I;

    if ARows[I].State = wrsDeny then
    begin
      if ARows[I].Required then
        raise EWfcRuleModel.CreateFmt(
          'deny rule cannot be required [%d]', [I]);
      if Length(ARows[I].TargetIndices) <> 0 then
        raise EWfcRuleModel.CreateFmt(
          'deny rule cannot contain targets [%d]', [I]);
    end
    else
    begin
      if Length(ARows[I].TargetIndices) < 1 then
        raise EWfcRuleModel.CreateFmt(
          'allow rule must contain at least one target [%d]', [I]);
      if Length(ARows[I].TargetIndices) > LValueCount then
        raise EWfcRuleModel.CreateFmt(
          'allow rule has too many targets [%d]', [I]);
      for J := 0 to Length(ARows[I].TargetIndices) - 1 do
      begin
        LTarget := ARows[I].TargetIndices[J];
        if (LTarget < 0) or (LTarget >= LValueCount) then
          raise EWfcRuleModel.CreateFmt(
            'rule target index is out of range [%d, %d]', [I, J]);
        if (J > 0) and
            (LTarget <= ARows[I].TargetIndices[J - 1]) then
          raise EWfcRuleModel.CreateFmt(
            'rule targets must be unique and strictly ordered [%d]', [I]);
      end;
    end;
  end;

  { TGraph.NewRule maintains a reciprocal finite edge model and propagates
    required metadata at direction granularity. Requiring this exact fixed
    point means the adapter may install prepared arrays without invoking a
    mutation-time closure algorithm. }
  for I := 0 to Length(ARows) - 1 do
    if ARows[I].State = wrsAllow then
      for J := 0 to Length(ARows[I].TargetIndices) - 1 do
      begin
        LTarget := ARows[I].TargetIndices[J];
        LInverseSlot := RuleSlot(LTarget,
          OppositeDirection(ARows[I].Direction));
        LInverseIndex := LLookup[LInverseSlot];
        if LInverseIndex < 0 then
          raise EWfcRuleModel.CreateFmt(
            'allow rule has no reciprocal row [%d, %d]', [I, J]);
        LInverseRow := ARows[LInverseIndex];
        if (LInverseRow.State <> wrsAllow) or
            (not ContainsTarget(LInverseRow.TargetIndices,
              ARows[I].OwnerIndex)) then
          raise EWfcRuleModel.CreateFmt(
            'allow rule has no reciprocal target [%d, %d]', [I, J]);
        if LInverseRow.Required <> ARows[I].Required then
          raise EWfcRuleModel.CreateFmt(
            'reciprocal rule required metadata does not match [%d, %d]',
            [I, LInverseIndex]);
      end;

  FRank := ARank;
  SetLength(FTokens, LValueCount);
  SetLength(FWeights, LValueCount);
  for I := 0 to LValueCount - 1 do
  begin
    FTokens[I] := ATokens[I];
    FWeights[I] := AWeights[I];
  end;

  SetLength(FRows, Length(ARows));
  for I := 0 to Length(ARows) - 1 do
  begin
    FRows[I].OwnerIndex := ARows[I].OwnerIndex;
    FRows[I].Direction := ARows[I].Direction;
    FRows[I].State := ARows[I].State;
    FRows[I].Required := ARows[I].Required;
    SetLength(FRows[I].TargetIndices,
      Length(ARows[I].TargetIndices));
    for J := 0 to Length(ARows[I].TargetIndices) - 1 do
      FRows[I].TargetIndices[J] := ARows[I].TargetIndices[J];
  end;
  FSignature := CalculateWfcRuleModelSignature(Self);
end;

procedure TWfcRuleModel.ValidateValueIndex(const AValueIndex: Integer);
begin
  if (AValueIndex < 0) or (AValueIndex >= ValueCount) then
    raise ERangeError.CreateFmt(
      'rule model value index out of range [%d]', [AValueIndex]);
end;

procedure TWfcRuleModel.ValidateRuleIndex(const ARuleIndex: Integer);
begin
  if (ARuleIndex < 0) or (ARuleIndex >= RuleCount) then
    raise ERangeError.CreateFmt(
      'rule model row index out of range [%d]', [ARuleIndex]);
end;

function TWfcRuleModel.GetValueCount: Integer;
begin
  Result := Length(FTokens);
end;

function TWfcRuleModel.GetRuleCount: Integer;
begin
  Result := Length(FRows);
end;

function TWfcRuleModel.TokenAt(
  const AValueIndex: Integer): TWfcModelToken;
begin
  ValidateValueIndex(AValueIndex);
  Result := FTokens[AValueIndex];
end;

function TWfcRuleModel.FindToken(const AToken: TWfcModelToken): Integer;
begin
  for Result := 0 to ValueCount - 1 do
    if FTokens[Result] = AToken then
      Exit;
  Result := -1;
end;

function TWfcRuleModel.WeightAt(const AValueIndex: Integer): Integer;
begin
  ValidateValueIndex(AValueIndex);
  Result := FWeights[AValueIndex];
end;

function TWfcRuleModel.CopyTokens: TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, ValueCount);
  for I := 0 to ValueCount - 1 do
    Result[I] := FTokens[I];
end;

function TWfcRuleModel.CopyWeights: TWfcModelIntegerArray;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, ValueCount);
  for I := 0 to ValueCount - 1 do
    Result[I] := FWeights[I];
end;

function TWfcRuleModel.RuleOwnerAt(const ARuleIndex: Integer): Integer;
begin
  ValidateRuleIndex(ARuleIndex);
  Result := FRows[ARuleIndex].OwnerIndex;
end;

function TWfcRuleModel.RuleDirectionAt(
  const ARuleIndex: Integer): TGraphDirection;
begin
  ValidateRuleIndex(ARuleIndex);
  Result := FRows[ARuleIndex].Direction;
end;

function TWfcRuleModel.RuleStateAt(
  const ARuleIndex: Integer): TWfcRuleState;
begin
  ValidateRuleIndex(ARuleIndex);
  Result := FRows[ARuleIndex].State;
end;

function TWfcRuleModel.RuleRequiredAt(
  const ARuleIndex: Integer): Boolean;
begin
  ValidateRuleIndex(ARuleIndex);
  Result := FRows[ARuleIndex].Required;
end;

function TWfcRuleModel.RuleTargetCountAt(
  const ARuleIndex: Integer): Integer;
begin
  ValidateRuleIndex(ARuleIndex);
  Result := Length(FRows[ARuleIndex].TargetIndices);
end;

function TWfcRuleModel.RuleTargetAt(const ARuleIndex,
  ATargetOrdinal: Integer): Integer;
begin
  ValidateRuleIndex(ARuleIndex);
  if (ATargetOrdinal < 0) or
      (ATargetOrdinal >= Length(FRows[ARuleIndex].TargetIndices)) then
    raise ERangeError.CreateFmt(
      'rule model target ordinal out of range [%d, %d]',
      [ARuleIndex, ATargetOrdinal]);
  Result := FRows[ARuleIndex].TargetIndices[ATargetOrdinal];
end;

function TWfcRuleModel.CopyRuleTargets(
  const ARuleIndex: Integer): TWfcModelIntegerArray;
var
  I: Integer;
begin
  ValidateRuleIndex(ARuleIndex);
  Result := nil;
  SetLength(Result, Length(FRows[ARuleIndex].TargetIndices));
  for I := 0 to Length(Result) - 1 do
    Result[I] := FRows[ARuleIndex].TargetIndices[I];
end;

procedure HashByte(var AHash: TWfcRuleModelSignature;
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

procedure HashCardinal(var AHash: TWfcRuleModelSignature;
  const AValue: Cardinal);
begin
  HashByte(AHash, Byte(AValue and $FF));
  HashByte(AHash, Byte((AValue shr 8) and $FF));
  HashByte(AHash, Byte((AValue shr 16) and $FF));
  HashByte(AHash, Byte((AValue shr 24) and $FF));
end;

procedure HashInteger(var AHash: TWfcRuleModelSignature;
  const AValue: Integer);
begin
  HashCardinal(AHash, Cardinal(AValue));
end;

procedure HashToken(var AHash: TWfcRuleModelSignature;
  const AValue: TWfcModelToken);
var
  I: Integer;
  LCanonical: String;
begin
  LCanonical := WfcTextEncodeToken(AValue, 'rule model signature');
  HashInteger(AHash, Length(LCanonical));
  for I := 1 to Length(LCanonical) do
    HashByte(AHash, Byte(Ord(LCanonical[I])));
end;

function CalculateWfcRuleModelSignature(
  const AModel: TWfcRuleModel): TWfcRuleModelSignature;
var
  I: Integer;
  J: Integer;
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create('rule model cannot be nil');
  Result := Cardinal(2166136261);
  HashInteger(Result, WFC_RULE_MODEL_SIGNATURE_VERSION);
  HashInteger(Result, WFC_RULE_MODEL_VERSION);
  HashInteger(Result, AModel.Rank);
  HashInteger(Result, AModel.ValueCount);
  for I := 0 to AModel.ValueCount - 1 do
  begin
    HashToken(Result, AModel.TokenAt(I));
    HashInteger(Result, AModel.WeightAt(I));
  end;
  HashInteger(Result, AModel.RuleCount);
  for I := 0 to AModel.RuleCount - 1 do
  begin
    HashInteger(Result, AModel.RuleOwnerAt(I));
    HashInteger(Result, Ord(AModel.RuleDirectionAt(I)));
    HashInteger(Result, Ord(AModel.RuleStateAt(I)));
    if AModel.RuleRequiredAt(I) then
      HashInteger(Result, 1)
    else
      HashInteger(Result, 0);
    HashInteger(Result, AModel.RuleTargetCountAt(I));
    for J := 0 to AModel.RuleTargetCountAt(I) - 1 do
      HashInteger(Result, AModel.RuleTargetAt(I, J));
  end;
end;

function WfcRuleModelSignatureHex(
  const ASignature: TWfcRuleModelSignature): String;
begin
  Result := IntToHex(ASignature, 8);
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

procedure ApplyRuleModelToGraph(const AModel: TWfcRuleModel;
  const AGraph: TGraph);
var
  I: Integer;
  J: Integer;
  LDenied: TGraphDirectionsArray;
  LGraphValues: TGraphValues;
  LRegistered: TGraphValues;
  LRuleCounts: TIntegerArray;
  LRulePositions: TIntegerArray;
  LRules: TGraphRuleMatrix;
  LTargetIndex: Integer;
begin
  if not Assigned(AModel) then
    raise EWfcRuleModel.Create('rule model must be assigned');
  if not Assigned(AGraph) then
    raise EWfcRuleModel.Create('target graph must be assigned');
  if AGraph.Running then
    raise EWfcRuleModel.Create(
      'target graph pass cannot be modified while the pipeline is running');
  LRegistered := AGraph.CopyRegisteredValues;
  if AGraph.HasDefinition or (AGraph.RuleGroups.Count <> 0) or
      (Length(LRegistered) <> 0) then
    raise EWfcRuleModel.Create(
      'target graph pass must be empty before applying a rule model');
  if CalculateWfcRuleModelSignature(AModel) <> AModel.Signature then
    raise EWfcRuleModel.Create('rule model signature is inconsistent');

  SetLength(LGraphValues, AModel.ValueCount);
  for I := 0 to AModel.ValueCount - 1 do
  begin
    LGraphValues[I] := ModelTokenToGraphValue(AModel.TokenAt(I));
    if LGraphValues[I] = TGraphValue.Empty then
      raise EWfcRuleModel.CreateFmt(
        'rule token converts to an empty graph value [%d]', [I]);
    if GraphValueToModelToken(LGraphValues[I]) <> AModel.TokenAt(I) then
      raise EWfcRuleModel.CreateFmt(
        'rule token cannot be represented by the target graph string type [%d]',
        [I]);
    for J := 0 to I - 1 do
      if LGraphValues[I] = LGraphValues[J] then
        raise EWfcRuleModel.CreateFmt(
          'rule token conversion is not unique [%d, %d]', [J, I]);
  end;

  SetLength(LRuleCounts, AModel.ValueCount);
  SetLength(LRulePositions, AModel.ValueCount);
  SetLength(LDenied, AModel.ValueCount);
  for I := 0 to AModel.RuleCount - 1 do
    if AModel.RuleStateAt(I) = wrsAllow then
      Inc(LRuleCounts[AModel.RuleOwnerAt(I)])
    else
      Include(LDenied[AModel.RuleOwnerAt(I)],
        AModel.RuleDirectionAt(I));
  SetLength(LRules, AModel.ValueCount);
  for I := 0 to AModel.ValueCount - 1 do
    SetLength(LRules[I], LRuleCounts[I]);

  for I := 0 to AModel.RuleCount - 1 do
    if AModel.RuleStateAt(I) = wrsAllow then
    begin
      J := LRulePositions[AModel.RuleOwnerAt(I)];
      LRules[AModel.RuleOwnerAt(I)][J].Key :=
        AModel.RuleDirectionAt(I);
      LRules[AModel.RuleOwnerAt(I)][J].Info :=
        AModel.RuleRequiredAt(I);
      SetLength(LRules[AModel.RuleOwnerAt(I)][J].Value,
        AModel.RuleTargetCountAt(I));
      for LTargetIndex := 0 to AModel.RuleTargetCountAt(I) - 1 do
        LRules[AModel.RuleOwnerAt(I)][J].Value[LTargetIndex] :=
          LGraphValues[AModel.RuleTargetAt(I, LTargetIndex)];
      Inc(LRulePositions[AModel.RuleOwnerAt(I)]);
    end;

  { Nothing below can reject well-formed prepared data on a pristine pass. }
  for I := 0 to AModel.ValueCount - 1 do
    AGraph.AddValue(LGraphValues[I], AModel.WeightAt(I));
  for I := 0 to AModel.ValueCount - 1 do
    AGraph.Rules[LGraphValues[I]].Rules := LRules[I];
  for I := 0 to AModel.ValueCount - 1 do
    if LDenied[I] <> [] then
      AGraph.Rules[LGraphValues[I]].DenyAll(LDenied[I]);
end;

end.
