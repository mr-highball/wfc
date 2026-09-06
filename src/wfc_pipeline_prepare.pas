{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Reusable pure lowering and opaque fresh/editable graph binding. }
unit wfc_pipeline_prepare;
{$mode delphi}{$H+}
interface
uses SysUtils, wfc, wfc_model, wfc_pipeline_model, wfc_pipeline_run,
  wfc_pipeline_layout, wfc_pipeline_compile, wfc_pipeline_result;

const
  WFC_PIPELINE_RUNTIME_VERSION = 2;
  WFC_PIPELINE_RUNTIME_INVERSE_LIMITS_VERSION = 1;

  { A run already limits one grid to 4,194,304 cells. The executable boundary
    additionally limits the sum of every materialized pass grid so a recipe
    cannot multiply a small-looking run into an unbounded allocation. }
  WFC_PIPELINE_RUNTIME_MAX_TOTAL_PASS_CELL_COUNT = 16777216;

  { Fixed aggregate work and storage boundaries for versioned inverse public
    input lowering. They apply to one runtime construction across every
    Pattern2D-v2, Sequence-v2 and Pattern3D-v1 bridge. }
  WFC_PIPELINE_RUNTIME_MAX_INVERSE_CONTRIBUTION_COUNT = 1048576;
  WFC_PIPELINE_RUNTIME_MAX_INVERSE_CANDIDATE_VISIT_COUNT = 16777216;
  WFC_PIPELINE_RUNTIME_MAX_INVERSE_PRIVATE_INDEX_COUNT = 4194304;

type
  EWfcPipelineRuntime = class(Exception);

  TWfcPipelineReplacementLimits = record
    Version: Integer;
    MaxRetainedCellRecords: Integer;
    MaxRetainedValueItems: Integer;
    MaxCandidateVisits: Integer;
  end;
  TWfcPipelineInputImpact = record
    AuthoredInputsChanged: Boolean;
    GraphInputsChanged: Boolean;
    AuthoredPassIndices: TGraphPassIndices;
    ChangedPassIndices: TGraphPassIndices;
  end;

  TWfcPipelineInputPlan = class
  private
    FData: TObject;
    constructor CreateOwned(const AData: TObject);
  public
    destructor Destroy; override;
    function CopyLocks: TWfcPipelineCellLocks;
    function CopyDomains: TWfcPipelineCellDomains;
    function CopyPassLayouts: TWfcPipelineLayoutTable;
  end;

  TWfcPipelinePreparation = class
  private
    FData: TObject;
  public
    constructor Create(const ARecipe: TWfcPipelineModel;
      const AInitialRun: TWfcPipelineRun);
    destructor Destroy; override;
    function PrepareInputs(const ARun: TWfcPipelineRun): TWfcPipelineInputPlan;
  end;

  { Owns a fresh compiled graph. Recipe is borrowed and must outlive this owner.
    Preparation and the caller's plan wrapper are needed only during Create. }
  TWfcPipelineInputBinding = class
  private
    FData: TObject;
    FCompiled: TWfcCompiledPipeline;
    FEditable: TObject;
    FUsable: Boolean;
    function GetUsable: Boolean;
  public
    constructor Create(const APreparation: TWfcPipelinePreparation;
      const AInitialPlan: TWfcPipelineInputPlan);
    { Opt-in: the graph borrow permits solve/inspection/selection only. No
      external definitions, values, domains, seed or hook mutations. }
    constructor CreateEditable(const APreparation: TWfcPipelinePreparation;
      const AInitialPlan: TWfcPipelineInputPlan;
      const ALimits: TWfcPipelineReplacementLimits);
    function ReplaceInputs(const ADesiredPlan: TWfcPipelineInputPlan): TWfcPipelineInputImpact;
    destructor Destroy; override;
    function BorrowCompiled: TWfcCompiledPipeline;
    property Usable: Boolean read GetUsable;
  end;

implementation
uses wfc_lattice, wfc_text_codec, wfc_token_lookup, wfc_pattern2d,
  wfc_pattern3d, wfc_sequence;

type
  TIntegerArray = array of Integer;
  TVocabularyArray = array of TWfcModelTokens;
  TTokenLookupArray = array of TWfcTokenLookup;

  TEffectiveLock = record
    Key: Integer;
    PassIndex: Integer;
    X: Integer;
    Y: Integer;
    Z: Integer;
    TokenIndex: Integer;
    InputIndex: Integer;
  end;
  TEffectiveLocks = array of TEffectiveLock;

  TEffectiveDomain = record
    Key: Integer;
    PassIndex: Integer;
    X: Integer;
    Y: Integer;
    Z: Integer;
    AllowedTokenIndices: TIntegerArray;
    InputIndex: Integer;
  end;
  TEffectiveDomains = array of TEffectiveDomain;

  TEffectiveConstraint = record
    Key: Integer;
    PassIndex: Integer;
    X: Integer;
    Y: Integer;
    Z: Integer;
    AllowedTokenIndices: TIntegerArray;
  end;
  TEffectiveConstraints = array of TEffectiveConstraint;

  TInverseContribution = record
    Key: Integer;
    PassIndex: Integer;
    X: Integer;
    Y: Integer;
    Z: Integer;
    BridgeIndex: Integer;
    ConstraintIndex: Integer;
    OffsetX: Integer;
    OffsetY: Integer;
    OffsetZ: Integer;
    CandidateCount: Integer;
  end;
  TInverseContributions = array of TInverseContribution;

  TInverseDomain = record
    PassIndex: Integer;
    X: Integer;
    Y: Integer;
    Z: Integer;
    AllowedValueIndices: TIntegerArray;
  end;
  TInverseDomains = array of TInverseDomain;

procedure PreflightResultAndGraphBudgets(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun);
var
  I, LCount, LPublicCells, LPublicCount: Integer;
begin
  if ARun.TotalCellCount > WFC_PIPELINE_RUNTIME_MAX_TOTAL_PASS_CELL_COUNT then
    raise EWfcPipelineRuntime.Create(
      'pipeline materialization exceeds the runtime pass-cell limit');
  LPublicCount := 0;
  LPublicCells := 0;
  for I := 0 to ARecipe.PassCount - 1 do
    if ARecipe.PassAt(I).Visibility = wppvPublic then
    begin
      Inc(LPublicCount);
      LCount := ARun.PassCellCount(I);
      if LCount > WFC_PIPELINE_RESULT_MAX_TOTAL_PUBLIC_CELL_COUNT - LPublicCells then
        raise EWfcPipelineRuntime.Create(
          'pipeline public output exceeds the result cell limit');
      Inc(LPublicCells, LCount);
    end;
  if LPublicCount > WFC_PIPELINE_RESULT_MAX_PUBLIC_LAYER_COUNT then
    raise EWfcPipelineRuntime.Create(
      'pipeline public layer count exceeds the result limit');
end;

function EncodedTokenLength(const AToken: TWfcModelToken;
  const ALabel: String): Integer;
var
  LEncoded: String;
begin
  LEncoded := WfcTextEncodeToken(AToken,
    'pipeline runtime result-budget validation');
  Result := Length(LEncoded);
  if Result > WFC_PIPELINE_RESULT_MAX_ENCODED_TOKEN_LENGTH then
    raise EWfcPipelineRuntime.Create(ALabel +
      ' exceeds the result encoded-token limit');
end;

procedure PreflightEncodedResultBudget(const ARecipe: TWfcPipelineModel;
  const AVocabularies: TVocabularyArray; const ARun: TWfcPipelineRun);
var
  I: Integer;
  J: Integer;
  LLength: Integer;
  LMaximumTokenLength: Integer;
  LRemaining: Integer;
  LTotal: Integer;
begin
  LTotal := 0;
  for I := 0 to ARecipe.PassCount - 1 do
  begin
    if ARecipe.PassAt(I).Visibility <> wppvPublic then
      Continue;
    LLength := EncodedTokenLength(ARecipe.PassAt(I).LabelName,
      'public pass label');
    if LLength > WFC_PIPELINE_RESULT_MAX_TOTAL_ENCODED_TOKEN_LENGTH -
        LTotal then
      raise EWfcPipelineRuntime.Create(
        'pipeline public labels exceed the result encoded-token budget');
    Inc(LTotal, LLength);

    LMaximumTokenLength := 0;
    for J := 0 to Length(AVocabularies[I]) - 1 do
    begin
      LLength := EncodedTokenLength(AVocabularies[I][J],
        'public vocabulary token');
      if LLength > LMaximumTokenLength then
        LMaximumTokenLength := LLength;
    end;
    LRemaining := WFC_PIPELINE_RESULT_MAX_TOTAL_ENCODED_TOKEN_LENGTH -
      LTotal;
    if (LMaximumTokenLength <> 0) and
        (ARun.PassCellCount(I) > LRemaining div LMaximumTokenLength) then
      raise EWfcPipelineRuntime.CreateFmt(
        'public pass %d can exceed the result encoded-token budget', [I]);
    Inc(LTotal, ARun.PassCellCount(I) * LMaximumTokenLength);
  end;
end;

function ResolveMaterializedPass(const ARecipe: TWfcPipelineModel;
  const APassIndex: Integer): Integer;
var
  LPass: TWfcPipelinePass;
  LStepCount: Integer;
begin
  Result := APassIndex;
  LStepCount := 0;
  repeat
    LPass := ARecipe.PassAt(Result);
    if LPass.Visibility <> wppvPublic then
      raise EWfcPipelineRuntime.CreateFmt(
        'public input resolves through private pass %d', [Result]);
    if LPass.Mode <> gpmTransform then
      Break;
    Result := LPass.TransformSourceIndex;
    Inc(LStepCount);
    if LStepCount > ARecipe.PassCount then
      raise EWfcPipelineRuntime.Create(
        'public transform input contains a source cycle');
  until False;
end;

procedure FreeTokenLookups(var AValues: TTokenLookupArray);
var
  I: Integer;
begin
  for I := 0 to Length(AValues) - 1 do
    AValues[I].Free;
  AValues := nil;
end;

procedure RequireEqualVocabularies(const ALeft, ARight: TWfcModelTokens;
  const AInputKind: String; const AInputIndex, ALeftPass,
  ARightPass: Integer);
var
  I: Integer;
begin
  if Length(ALeft) <> Length(ARight) then
    raise EWfcPipelineRuntime.CreateFmt(
      '%s %d transform vocabulary count differs between passes %d and %d',
      [AInputKind, AInputIndex, ALeftPass, ARightPass]);
  for I := 0 to Length(ALeft) - 1 do
    if ALeft[I] <> ARight[I] then
      raise EWfcPipelineRuntime.CreateFmt(
        '%s %d transform vocabulary differs at token %d',
        [AInputKind, AInputIndex, I]);
end;

function CellKey(const APassIndex, AX, AY, AZ: Integer;
  const ARun: TWfcPipelineRun): Integer;
var
  LCell: Integer;
  LLayout: TWfcLatticeLayout;
begin
  LLayout := ARun.PassLayoutAt(APassIndex);
  if (AX < 0) or (AY < 0) or (AZ < 0) or
    (AX >= LLayout.Cells.X) or (AY >= LLayout.Cells.Y) or
    (AZ >= LLayout.Cells.Z) then
    raise EWfcPipelineRuntime.Create('effective input coordinate is outside its pass');
  LCell := (AZ * LLayout.Cells.Y + AY) * LLayout.Cells.X + AX;
  if ARun.PassOffsetAt(APassIndex) > High(Integer) - LCell then
    raise EWfcPipelineRuntime.Create('effective input key exceeds Integer');
  Result := ARun.PassOffsetAt(APassIndex) + LCell;
end;

procedure MergeSortLocks(var AValues: TEffectiveLocks);
var
  I: Integer;
  LLeft: Integer;
  LLeftEnd: Integer;
  LMiddle: Integer;
  LRight: Integer;
  LRightEnd: Integer;
  LTarget: Integer;
  LTemporary: TEffectiveLocks;
  LWidth: Integer;
begin
  if Length(AValues) < 2 then
    Exit;
  SetLength(LTemporary, Length(AValues));
  LWidth := 1;
  while LWidth < Length(AValues) do
  begin
    LLeft := 0;
    while LLeft < Length(AValues) do
    begin
      LMiddle := LLeft + LWidth;
      if LMiddle > Length(AValues) then
        LMiddle := Length(AValues);
      LRightEnd := LMiddle + LWidth;
      if LRightEnd > Length(AValues) then
        LRightEnd := Length(AValues);
      LLeftEnd := LMiddle;
      I := LLeft;
      LRight := LMiddle;
      LTarget := LLeft;
      while (I < LLeftEnd) and (LRight < LRightEnd) do
      begin
        if (AValues[I].Key < AValues[LRight].Key) or
            ((AValues[I].Key = AValues[LRight].Key) and
             (AValues[I].InputIndex <= AValues[LRight].InputIndex)) then
        begin
          LTemporary[LTarget] := AValues[I];
          Inc(I);
        end
        else
        begin
          LTemporary[LTarget] := AValues[LRight];
          Inc(LRight);
        end;
        Inc(LTarget);
      end;
      while I < LLeftEnd do
      begin
        LTemporary[LTarget] := AValues[I];
        Inc(I);
        Inc(LTarget);
      end;
      while LRight < LRightEnd do
      begin
        LTemporary[LTarget] := AValues[LRight];
        Inc(LRight);
        Inc(LTarget);
      end;
      LLeft := LRightEnd;
    end;
    for I := 0 to Length(AValues) - 1 do
      AValues[I] := LTemporary[I];
    if LWidth > Length(AValues) div 2 then
      LWidth := Length(AValues)
    else
      LWidth := LWidth * 2;
  end;
end;

procedure MergeSortDomains(var AValues: TEffectiveDomains);
var
  I: Integer;
  LLeft: Integer;
  LLeftEnd: Integer;
  LMiddle: Integer;
  LRight: Integer;
  LRightEnd: Integer;
  LTarget: Integer;
  LTemporary: TEffectiveDomains;
  LWidth: Integer;
begin
  if Length(AValues) < 2 then
    Exit;
  SetLength(LTemporary, Length(AValues));
  LWidth := 1;
  while LWidth < Length(AValues) do
  begin
    LLeft := 0;
    while LLeft < Length(AValues) do
    begin
      LMiddle := LLeft + LWidth;
      if LMiddle > Length(AValues) then
        LMiddle := Length(AValues);
      LRightEnd := LMiddle + LWidth;
      if LRightEnd > Length(AValues) then
        LRightEnd := Length(AValues);
      LLeftEnd := LMiddle;
      I := LLeft;
      LRight := LMiddle;
      LTarget := LLeft;
      while (I < LLeftEnd) and (LRight < LRightEnd) do
      begin
        if (AValues[I].Key < AValues[LRight].Key) or
            ((AValues[I].Key = AValues[LRight].Key) and
             (AValues[I].InputIndex <= AValues[LRight].InputIndex)) then
        begin
          LTemporary[LTarget] := AValues[I];
          Inc(I);
        end
        else
        begin
          LTemporary[LTarget] := AValues[LRight];
          Inc(LRight);
        end;
        Inc(LTarget);
      end;
      while I < LLeftEnd do
      begin
        LTemporary[LTarget] := AValues[I];
        Inc(I);
        Inc(LTarget);
      end;
      while LRight < LRightEnd do
      begin
        LTemporary[LTarget] := AValues[LRight];
        Inc(LRight);
        Inc(LTarget);
      end;
      LLeft := LRightEnd;
    end;
    for I := 0 to Length(AValues) - 1 do
      AValues[I] := LTemporary[I];
    if LWidth > Length(AValues) div 2 then
      LWidth := Length(AValues)
    else
      LWidth := LWidth * 2;
  end;
end;

function IntersectIndices(const ALeft,
  ARight: TIntegerArray): TIntegerArray;
var
  I: Integer;
  J: Integer;
  LCount: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ALeft));
  I := 0;
  J := 0;
  LCount := 0;
  while (I < Length(ALeft)) and (J < Length(ARight)) do
  begin
    if ALeft[I] = ARight[J] then
    begin
      Result[LCount] := ALeft[I];
      Inc(LCount);
      Inc(I);
      Inc(J);
    end
    else if ALeft[I] < ARight[J] then
      Inc(I)
    else
      Inc(J);
  end;
  SetLength(Result, LCount);
end;

function ContainsIndex(const AValues: TIntegerArray;
  const AValue: Integer): Boolean;
var
  LHigh: Integer;
  LLow: Integer;
  LMiddle: Integer;
begin
  LLow := 0;
  LHigh := Length(AValues) - 1;
  while LLow <= LHigh do
  begin
    LMiddle := LLow + (LHigh - LLow) div 2;
    if AValues[LMiddle] = AValue then
      Exit(True);
    if AValues[LMiddle] < AValue then
      LLow := LMiddle + 1
    else
      LHigh := LMiddle - 1;
  end;
  Result := False;
end;

procedure BuildVocabularies(const ARecipe: TWfcPipelineModel;
  out AValues: TVocabularyArray; out AResolvedPasses: TIntegerArray;
  out ALookups: TTokenLookupArray);
var
  I: Integer;
  LEffectivePass: Integer;
begin
  AValues := nil;
  ALookups := nil;
  SetLength(AValues, ARecipe.PassCount);
  SetLength(AResolvedPasses, ARecipe.PassCount);
  SetLength(ALookups, ARecipe.PassCount);
  for I := 0 to ARecipe.PassCount - 1 do
  begin
    AResolvedPasses[I] := -1;
    if ARecipe.PassAt(I).Visibility = wppvPublic then
    begin
      AValues[I] := ARecipe.CopyPublicVocabulary(I);
      ALookups[I] := TWfcTokenLookup.Create(AValues[I]);
    end;
  end;
  for I := 0 to ARecipe.PassCount - 1 do
    if ARecipe.PassAt(I).Visibility = wppvPublic then
    begin
      LEffectivePass := ResolveMaterializedPass(ARecipe, I);
      AResolvedPasses[I] := LEffectivePass;
      RequireEqualVocabularies(AValues[I], AValues[LEffectivePass],
        'public pass', I, I, LEffectivePass);
    end;
end;

procedure BuildEffectiveLocks(const ARun: TWfcPipelineRun;
  const ALookups: TTokenLookupArray;
  const AResolvedPasses: TIntegerArray;
  out AValues: TEffectiveLocks);
var
  I: Integer;
  LEffectivePass: Integer;
  LInput: TWfcPipelineCellLock;
begin
  AValues := nil;
  SetLength(AValues, ARun.LockCount);
  for I := 0 to ARun.LockCount - 1 do
  begin
    LInput := ARun.LockAt(I);
    LEffectivePass := AResolvedPasses[LInput.PassIndex];
    if LEffectivePass < 0 then
      raise EWfcPipelineRuntime.CreateFmt(
        'lock %d does not target a public pass', [I]);
    AValues[I].TokenIndex := ALookups[LEffectivePass].Find(LInput.Token);
    if AValues[I].TokenIndex < 0 then
      raise EWfcPipelineRuntime.CreateFmt(
        'lock %d token is outside its effective pass vocabulary', [I]);
    AValues[I].PassIndex := LEffectivePass;
    AValues[I].X := LInput.X;
    AValues[I].Y := LInput.Y;
    AValues[I].Z := LInput.Z;
    AValues[I].Key := CellKey(LEffectivePass, LInput.X, LInput.Y,
      LInput.Z, ARun);
    AValues[I].InputIndex := I;
  end;
end;

procedure BuildEffectiveDomains(const ARun: TWfcPipelineRun;
  const ALookups: TTokenLookupArray;
  const AResolvedPasses: TIntegerArray;
  out AValues: TEffectiveDomains);
var
  I: Integer;
  J: Integer;
  LEffectivePass: Integer;
  LInput: TWfcPipelineCellDomain;
  LPreviousIndex: Integer;
begin
  AValues := nil;
  SetLength(AValues, ARun.DomainCount);
  for I := 0 to ARun.DomainCount - 1 do
  begin
    LInput := ARun.DomainAt(I);
    LEffectivePass := AResolvedPasses[LInput.PassIndex];
    if LEffectivePass < 0 then
      raise EWfcPipelineRuntime.CreateFmt(
        'domain %d does not target a public pass', [I]);
    SetLength(AValues[I].AllowedTokenIndices,
      Length(LInput.AllowedTokens));
    LPreviousIndex := -1;
    for J := 0 to Length(LInput.AllowedTokens) - 1 do
    begin
      AValues[I].AllowedTokenIndices[J] := ALookups[
        LEffectivePass].Find(LInput.AllowedTokens[J]);
      if AValues[I].AllowedTokenIndices[J] < 0 then
        raise EWfcPipelineRuntime.CreateFmt(
          'domain %d token %d is outside its effective pass vocabulary',
          [I, J]);
      if AValues[I].AllowedTokenIndices[J] <= LPreviousIndex then
        raise EWfcPipelineRuntime.CreateFmt(
          'domain %d tokens are not in effective vocabulary order', [I]);
      LPreviousIndex := AValues[I].AllowedTokenIndices[J];
    end;
    AValues[I].PassIndex := LEffectivePass;
    AValues[I].X := LInput.X;
    AValues[I].Y := LInput.Y;
    AValues[I].Z := LInput.Z;
    AValues[I].Key := CellKey(LEffectivePass, LInput.X, LInput.Y,
      LInput.Z, ARun);
    AValues[I].InputIndex := I;
  end;
end;

procedure ConsolidateLocks(var AValues: TEffectiveLocks);
var
  I: Integer;
  LWrite: Integer;
begin
  MergeSortLocks(AValues);
  LWrite := -1;
  for I := 0 to Length(AValues) - 1 do
  begin
    if (LWrite < 0) or (AValues[I].Key <> AValues[LWrite].Key) then
    begin
      Inc(LWrite);
      AValues[LWrite] := AValues[I];
      Continue;
    end;
    if AValues[I].TokenIndex <> AValues[LWrite].TokenIndex then
      raise EWfcPipelineRuntime.CreateFmt(
        'locks %d and %d conflict after transform resolution at pass %d [%d, %d, %d]',
        [AValues[LWrite].InputIndex, AValues[I].InputIndex,
         AValues[I].PassIndex, AValues[I].X, AValues[I].Y, AValues[I].Z]);
  end;
  SetLength(AValues, LWrite + 1);
end;

procedure ConsolidateDomains(var AValues: TEffectiveDomains);
var
  I: Integer;
  LWrite: Integer;
begin
  MergeSortDomains(AValues);
  LWrite := -1;
  for I := 0 to Length(AValues) - 1 do
  begin
    if (LWrite < 0) or (AValues[I].Key <> AValues[LWrite].Key) then
    begin
      Inc(LWrite);
      AValues[LWrite] := AValues[I];
      Continue;
    end;
    AValues[LWrite].AllowedTokenIndices := IntersectIndices(
      AValues[LWrite].AllowedTokenIndices,
      AValues[I].AllowedTokenIndices);
  end;
  SetLength(AValues, LWrite + 1);
end;

function CopyIndices(const AValues: TIntegerArray): TIntegerArray;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function IsWholeVocabulary(const AValues: TIntegerArray;
  const AVocabularyCount: Integer): Boolean;
var
  I: Integer;
begin
  if Length(AValues) <> AVocabularyCount then
    Exit(False);
  for I := 0 to Length(AValues) - 1 do
    if AValues[I] <> I then
      Exit(False);
  Result := True;
end;

procedure BuildEffectiveConstraints(const ALocks: TEffectiveLocks;
  const ADomains: TEffectiveDomains;
  const AVocabularies: TVocabularyArray;
  out AValues: TEffectiveConstraints);
var
  LDomainIndex: Integer;
  LLockIndex: Integer;
  LWrite: Integer;

  procedure AppendLock(const ALock: TEffectiveLock);
  begin
    AValues[LWrite].Key := ALock.Key;
    AValues[LWrite].PassIndex := ALock.PassIndex;
    AValues[LWrite].X := ALock.X;
    AValues[LWrite].Y := ALock.Y;
    AValues[LWrite].Z := ALock.Z;
    SetLength(AValues[LWrite].AllowedTokenIndices, 1);
    AValues[LWrite].AllowedTokenIndices[0] := ALock.TokenIndex;
    Inc(LWrite);
  end;

  procedure AppendDomain(const ADomain: TEffectiveDomain);
  begin
    if IsWholeVocabulary(ADomain.AllowedTokenIndices,
        Length(AVocabularies[ADomain.PassIndex])) then
      Exit;
    AValues[LWrite].Key := ADomain.Key;
    AValues[LWrite].PassIndex := ADomain.PassIndex;
    AValues[LWrite].X := ADomain.X;
    AValues[LWrite].Y := ADomain.Y;
    AValues[LWrite].Z := ADomain.Z;
    AValues[LWrite].AllowedTokenIndices := CopyIndices(
      ADomain.AllowedTokenIndices);
    Inc(LWrite);
  end;
begin
  AValues := nil;
  SetLength(AValues, Length(ALocks) + Length(ADomains));
  LLockIndex := 0;
  LDomainIndex := 0;
  LWrite := 0;
  while (LLockIndex < Length(ALocks)) or
      (LDomainIndex < Length(ADomains)) do
  begin
    if (LDomainIndex >= Length(ADomains)) or
        ((LLockIndex < Length(ALocks)) and
         (ALocks[LLockIndex].Key < ADomains[LDomainIndex].Key)) then
    begin
      AppendLock(ALocks[LLockIndex]);
      Inc(LLockIndex);
    end
    else if (LLockIndex >= Length(ALocks)) or
        (ADomains[LDomainIndex].Key < ALocks[LLockIndex].Key) then
    begin
      AppendDomain(ADomains[LDomainIndex]);
      Inc(LDomainIndex);
    end
    else
    begin
      { Compatibility has already been proved. A lock is the exact
        intersection and retains the stronger public constraint. }
      AppendLock(ALocks[LLockIndex]);
      Inc(LLockIndex);
      Inc(LDomainIndex);
    end;
  end;
  SetLength(AValues, LWrite);
end;

function WrappedSubtract(const ACoordinate, AOffset,
  ADimension: Integer): Integer;
var
  LOffset: Integer;
begin
  LOffset := AOffset mod ADimension;
  if ACoordinate >= LOffset then
    Result := ACoordinate - LOffset
  else
    Result := ADimension - (LOffset - ACoordinate);
end;

procedure AddBoundedWork(var ATotal: Integer; const ACount,
  AUnitCost, AMaximum: Integer; const ALabel: String);
begin
  if (ACount < 0) or (AUnitCost < 0) then
    raise EWfcPipelineRuntime.Create(ALabel + ' contains a negative count');
  if (ACount <> 0) and
      (AUnitCost > (AMaximum - ATotal) div ACount) then
    raise EWfcPipelineRuntime.Create(ALabel + ' exceeds the runtime limit');
  Inc(ATotal, ACount * AUnitCost);
end;

procedure MergeSortInverseContributions(
  var AValues: TInverseContributions);
var
  I: Integer;
  LLeft: Integer;
  LLeftEnd: Integer;
  LMiddle: Integer;
  LRight: Integer;
  LRightEnd: Integer;
  LTarget: Integer;
  LTemporary: TInverseContributions;
  LWidth: Integer;
begin
  if Length(AValues) < 2 then
    Exit;
  SetLength(LTemporary, Length(AValues));
  LWidth := 1;
  while LWidth < Length(AValues) do
  begin
    LLeft := 0;
    while LLeft < Length(AValues) do
    begin
      LMiddle := LLeft + LWidth;
      if LMiddle > Length(AValues) then
        LMiddle := Length(AValues);
      LRightEnd := LMiddle + LWidth;
      if LRightEnd > Length(AValues) then
        LRightEnd := Length(AValues);
      LLeftEnd := LMiddle;
      I := LLeft;
      LRight := LMiddle;
      LTarget := LLeft;
      while (I < LLeftEnd) and (LRight < LRightEnd) do
      begin
        if AValues[I].Key <= AValues[LRight].Key then
        begin
          LTemporary[LTarget] := AValues[I];
          Inc(I);
        end
        else
        begin
          LTemporary[LTarget] := AValues[LRight];
          Inc(LRight);
        end;
        Inc(LTarget);
      end;
      while I < LLeftEnd do
      begin
        LTemporary[LTarget] := AValues[I];
        Inc(I);
        Inc(LTarget);
      end;
      while LRight < LRightEnd do
      begin
        LTemporary[LTarget] := AValues[LRight];
        Inc(LRight);
        Inc(LTarget);
      end;
      LLeft := LRightEnd;
    end;
    for I := 0 to Length(AValues) - 1 do
      AValues[I] := LTemporary[I];
    if LWidth > Length(AValues) div 2 then
      LWidth := Length(AValues)
    else
      LWidth := LWidth * 2;
  end;
end;

procedure BuildInverseContributions(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun;
  const AConstraints: TEffectiveConstraints;
  out AValues: TInverseContributions);
var
  I: Integer;
  J: Integer;
  LBridge: TWfcPipelineBridge;
  LLayout: TWfcLatticeLayout;
  LBridgeForTarget: TIntegerArray;
  LContributionCount: Integer;
  LFootprintSize: Integer;
  LModel2D: TWfcOverlappingModel2D;
  LModel3D: TWfcOverlappingModel3D;
  LSequence: TWfcSequenceModel;
  LVersions: TWfcPipelineVersions;
  LVisitCount: Integer;
  LWrite: Integer;
  X: Integer;
  Y: Integer;
  Z: Integer;

  function VolumeFootprint(const AModel: TWfcOverlappingModel3D): Integer;
  var LPlane: Integer;
  begin
    { Keep products checked here as well as at the immutable model boundary:
      this runtime work preflight precedes contribution and graph allocation. }
    Result := 0;
    AddBoundedWork(Result, AModel.PatternWidth, AModel.PatternHeight,
      WFC_PIPELINE_RUNTIME_MAX_INVERSE_CONTRIBUTION_COUNT,
      'inverse volume footprint');
    LPlane := Result;
    Result := 0;
    AddBoundedWork(Result, LPlane, AModel.PatternDepth,
      WFC_PIPELINE_RUNTIME_MAX_INVERSE_CONTRIBUTION_COUNT,
      'inverse volume footprint');
  end;
begin
  AValues := nil;
  LVersions := ARecipe.CopyVersions;
  SetLength(LBridgeForTarget, ARecipe.PassCount);
  for I := 0 to Length(LBridgeForTarget) - 1 do
    LBridgeForTarget[I] := WFC_PIPELINE_NO_INDEX;
  for I := 0 to ARecipe.BridgeCount - 1 do
  begin
    LBridge := ARecipe.BridgeAt(I);
    if not SameWfcLatticeLayout(ARun.PassLayoutAt(LBridge.SourcePassIndex),
      ARun.PassLayoutAt(LBridge.TargetPassIndex)) then
      raise EWfcPipelineRuntime.Create('inverse bridge requires identical pass layouts');
    if ((LBridge.Kind = wpbkPattern2DProjection) and
        (LVersions.Pattern2DBridgeVersion = 2)) or
        ((LBridge.Kind = wpbkSequenceProjection) and
        (LVersions.SequenceBridgeVersion = 2)) or
        ((LBridge.Kind = wpbkPattern3DProjection) and
        (LVersions.Pattern3DBridgeVersion = 1)) then
      LBridgeForTarget[LBridge.TargetPassIndex] := I;
  end;

  LContributionCount := 0;
  LVisitCount := 0;
  for I := 0 to Length(AConstraints) - 1 do
  begin
    J := LBridgeForTarget[AConstraints[I].PassIndex];
    if J = WFC_PIPELINE_NO_INDEX then
      Continue;
    LBridge := ARecipe.BridgeAt(J);
    case LBridge.Kind of
      wpbkPattern2DProjection:
        begin
          LModel2D := ARecipe.BorrowPattern2DResource(
            ARecipe.PassAt(LBridge.SourcePassIndex).ResourceIndex);
          LFootprintSize := LModel2D.PatternWidth *
            LModel2D.PatternHeight;
          AddBoundedWork(LContributionCount, 1, LFootprintSize,
            WFC_PIPELINE_RUNTIME_MAX_INVERSE_CONTRIBUTION_COUNT,
            'inverse bridge contribution count');
          AddBoundedWork(LVisitCount, LFootprintSize,
            LModel2D.PatternCount,
            WFC_PIPELINE_RUNTIME_MAX_INVERSE_CANDIDATE_VISIT_COUNT,
            'inverse bridge candidate visits');
        end;
      wpbkPattern3DProjection:
        begin
          LModel3D := ARecipe.BorrowPattern3DResource(
            ARecipe.PassAt(LBridge.SourcePassIndex).ResourceIndex);
          LFootprintSize := VolumeFootprint(LModel3D);
          AddBoundedWork(LContributionCount, 1, LFootprintSize,
            WFC_PIPELINE_RUNTIME_MAX_INVERSE_CONTRIBUTION_COUNT,
            'inverse bridge contribution count');
          AddBoundedWork(LVisitCount, LFootprintSize, LModel3D.PatternCount,
            WFC_PIPELINE_RUNTIME_MAX_INVERSE_CANDIDATE_VISIT_COUNT,
            'inverse bridge candidate visits');
        end;
      wpbkSequenceProjection:
        begin
          LSequence := ARecipe.BorrowSequenceResource(
            ARecipe.PassAt(LBridge.SourcePassIndex).ResourceIndex);
          AddBoundedWork(LContributionCount, 1, 1,
            WFC_PIPELINE_RUNTIME_MAX_INVERSE_CONTRIBUTION_COUNT,
            'inverse bridge contribution count');
          AddBoundedWork(LVisitCount, 1, LSequence.StateCount,
            WFC_PIPELINE_RUNTIME_MAX_INVERSE_CANDIDATE_VISIT_COUNT,
            'inverse bridge candidate visits');
        end;
    end;
  end;

  SetLength(AValues, LContributionCount);
  LWrite := 0;
  for I := 0 to Length(AConstraints) - 1 do
  begin
    J := LBridgeForTarget[AConstraints[I].PassIndex];
    if J = WFC_PIPELINE_NO_INDEX then
      Continue;
    LBridge := ARecipe.BridgeAt(J);
    LLayout := ARun.PassLayoutAt(LBridge.SourcePassIndex);
    case LBridge.Kind of
      wpbkPattern2DProjection:
        begin
          LModel2D := ARecipe.BorrowPattern2DResource(
            ARecipe.PassAt(LBridge.SourcePassIndex).ResourceIndex);
          for Y := 0 to LModel2D.PatternHeight - 1 do
            for X := 0 to LModel2D.PatternWidth - 1 do
            begin
              AValues[LWrite].PassIndex := LBridge.SourcePassIndex;
              AValues[LWrite].X := WrappedSubtract(
                AConstraints[I].X, X, LLayout.Cells.X);
              AValues[LWrite].Y := WrappedSubtract(
                AConstraints[I].Y, Y, LLayout.Cells.Y);
              AValues[LWrite].Z := 0;
              AValues[LWrite].Key := CellKey(LBridge.SourcePassIndex,
                AValues[LWrite].X, AValues[LWrite].Y, 0,
                ARun);
              AValues[LWrite].BridgeIndex := J;
              AValues[LWrite].ConstraintIndex := I;
              AValues[LWrite].OffsetX := X;
              AValues[LWrite].OffsetY := Y;
              AValues[LWrite].OffsetZ := 0;
              AValues[LWrite].CandidateCount := LModel2D.PatternCount;
              Inc(LWrite);
            end;
        end;
      wpbkPattern3DProjection:
        begin
          LModel3D := ARecipe.BorrowPattern3DResource(
            ARecipe.PassAt(LBridge.SourcePassIndex).ResourceIndex);
          for Z := 0 to LModel3D.PatternDepth - 1 do
            for Y := 0 to LModel3D.PatternHeight - 1 do
              for X := 0 to LModel3D.PatternWidth - 1 do
              begin
                AValues[LWrite].PassIndex := LBridge.SourcePassIndex;
                AValues[LWrite].X := WrappedSubtract(AConstraints[I].X, X, LLayout.Cells.X);
                AValues[LWrite].Y := WrappedSubtract(AConstraints[I].Y, Y, LLayout.Cells.Y);
                AValues[LWrite].Z := WrappedSubtract(AConstraints[I].Z, Z, LLayout.Cells.Z);
                AValues[LWrite].Key := CellKey(LBridge.SourcePassIndex,
                  AValues[LWrite].X, AValues[LWrite].Y, AValues[LWrite].Z,
                  ARun);
                AValues[LWrite].BridgeIndex := J;
                AValues[LWrite].ConstraintIndex := I;
                AValues[LWrite].OffsetX := X;
                AValues[LWrite].OffsetY := Y;
                AValues[LWrite].OffsetZ := Z;
                AValues[LWrite].CandidateCount := LModel3D.PatternCount;
                Inc(LWrite);
              end;
        end;
      wpbkSequenceProjection:
        begin
          LSequence := ARecipe.BorrowSequenceResource(
            ARecipe.PassAt(LBridge.SourcePassIndex).ResourceIndex);
          AValues[LWrite].PassIndex := LBridge.SourcePassIndex;
          AValues[LWrite].X := AConstraints[I].X;
          AValues[LWrite].Y := 0;
          AValues[LWrite].Z := 0;
          AValues[LWrite].Key := CellKey(LBridge.SourcePassIndex,
            AConstraints[I].X, 0, 0, ARun);
          AValues[LWrite].BridgeIndex := J;
          AValues[LWrite].ConstraintIndex := I;
          AValues[LWrite].OffsetX := 0;
          AValues[LWrite].OffsetY := 0;
          AValues[LWrite].OffsetZ := 0;
          AValues[LWrite].CandidateCount := LSequence.StateCount;
          Inc(LWrite);
        end;
    end;
  end;
  if LWrite <> Length(AValues) then
    raise EWfcPipelineRuntime.Create(
      'inverse bridge contribution preflight disagrees with construction');
  MergeSortInverseContributions(AValues);

  LVisitCount := 0;
  for I := 0 to Length(AValues) - 1 do
    if (I = 0) or (AValues[I].Key <> AValues[I - 1].Key) then
      AddBoundedWork(LVisitCount, 1, AValues[I].CandidateCount,
        WFC_PIPELINE_RUNTIME_MAX_INVERSE_PRIVATE_INDEX_COUNT,
        'inverse bridge private indices')
    else if AValues[I].CandidateCount <> AValues[I - 1].CandidateCount then
      raise EWfcPipelineRuntime.Create(
        'inverse bridge source registry count is inconsistent');
end;

function GenerateInverseCandidates(const ARecipe: TWfcPipelineModel;
  const AContribution: TInverseContribution;
  const AConstraint: TEffectiveConstraint): TIntegerArray;
var
  I: Integer;
  LBridge: TWfcPipelineBridge;
  LModel2D: TWfcOverlappingModel2D;
  LModel3D: TWfcOverlappingModel3D;
  LSequence: TWfcSequenceModel;
  LWrite: Integer;
begin
  Result := nil;
  LBridge := ARecipe.BridgeAt(AContribution.BridgeIndex);
  SetLength(Result, AContribution.CandidateCount);
  LWrite := 0;
  case LBridge.Kind of
    wpbkPattern2DProjection:
      begin
        LModel2D := ARecipe.BorrowPattern2DResource(
          ARecipe.PassAt(LBridge.SourcePassIndex).ResourceIndex);
        for I := 0 to LModel2D.PatternCount - 1 do
          if ContainsIndex(AConstraint.AllowedTokenIndices,
              LModel2D.PatternPaletteIndexAt(I,
                AContribution.OffsetX, AContribution.OffsetY)) then
          begin
            Result[LWrite] := I;
            Inc(LWrite);
          end;
      end;
    wpbkPattern3DProjection:
      begin
        LModel3D := ARecipe.BorrowPattern3DResource(
          ARecipe.PassAt(LBridge.SourcePassIndex).ResourceIndex);
        for I := 0 to LModel3D.PatternCount - 1 do
          if ContainsIndex(AConstraint.AllowedTokenIndices,
              LModel3D.PatternPaletteIndexAt(I, AContribution.OffsetX,
                AContribution.OffsetY, AContribution.OffsetZ)) then
          begin
            Result[LWrite] := I;
            Inc(LWrite);
          end;
      end;
    wpbkSequenceProjection:
      begin
        LSequence := ARecipe.BorrowSequenceResource(
          ARecipe.PassAt(LBridge.SourcePassIndex).ResourceIndex);
        for I := 0 to LSequence.StateCount - 1 do
          if ContainsIndex(AConstraint.AllowedTokenIndices,
              LSequence.StateEmittedTokenIndexAt(I)) then
          begin
            Result[LWrite] := I;
            Inc(LWrite);
          end;
      end;
  end;
  SetLength(Result, LWrite);
end;

procedure BuildInverseDomains(const ARecipe: TWfcPipelineModel;
  const AConstraints: TEffectiveConstraints;
  const AContributions: TInverseContributions;
  out AValues: TInverseDomains);
var
  I: Integer;
  LCurrent: TIntegerArray;
  LCandidates: TIntegerArray;
  LGroupCount: Integer;
  LGroupEnd: Integer;
  LWrite: Integer;
begin
  AValues := nil;
  LGroupCount := 0;
  for I := 0 to Length(AContributions) - 1 do
    if (I = 0) or (AContributions[I].Key <>
        AContributions[I - 1].Key) then
      Inc(LGroupCount);
  SetLength(AValues, LGroupCount);
  I := 0;
  LWrite := 0;
  while I < Length(AContributions) do
  begin
    LGroupEnd := I + 1;
    while (LGroupEnd < Length(AContributions)) and
        (AContributions[LGroupEnd].Key = AContributions[I].Key) do
      Inc(LGroupEnd);

    LCurrent := GenerateInverseCandidates(ARecipe, AContributions[I],
      AConstraints[AContributions[I].ConstraintIndex]);
    Inc(I);
    while I < LGroupEnd do
    begin
      LCandidates := GenerateInverseCandidates(ARecipe,
        AContributions[I],
        AConstraints[AContributions[I].ConstraintIndex]);
      LCurrent := IntersectIndices(LCurrent, LCandidates);
      Inc(I);
    end;

    AValues[LWrite].PassIndex := AContributions[LGroupEnd - 1].PassIndex;
    AValues[LWrite].X := AContributions[LGroupEnd - 1].X;
    AValues[LWrite].Y := AContributions[LGroupEnd - 1].Y;
    AValues[LWrite].Z := AContributions[LGroupEnd - 1].Z;
    AValues[LWrite].AllowedValueIndices := LCurrent;
    Inc(LWrite);
  end;
  SetLength(AValues, LWrite);
end;

procedure ValidateLockDomainCompatibility(const ALocks: TEffectiveLocks;
  const ADomains: TEffectiveDomains);
var
  LDomainIndex: Integer;
  LLockIndex: Integer;
begin
  LLockIndex := 0;
  LDomainIndex := 0;
  while (LLockIndex < Length(ALocks)) and
      (LDomainIndex < Length(ADomains)) do
  begin
    if ALocks[LLockIndex].Key < ADomains[LDomainIndex].Key then
      Inc(LLockIndex)
    else if ALocks[LLockIndex].Key > ADomains[LDomainIndex].Key then
      Inc(LDomainIndex)
    else
    begin
      if not ContainsIndex(ADomains[LDomainIndex].AllowedTokenIndices,
          ALocks[LLockIndex].TokenIndex) then
        raise EWfcPipelineRuntime.CreateFmt(
          'lock %d is excluded by the effective domain at pass %d [%d, %d, %d]',
          [ALocks[LLockIndex].InputIndex, ALocks[LLockIndex].PassIndex,
           ALocks[LLockIndex].X, ALocks[LLockIndex].Y,
           ALocks[LLockIndex].Z]);
      Inc(LLockIndex);
      Inc(LDomainIndex);
    end;
  end;
end;

procedure ApplyInverseDomains(const ACompiled: TWfcCompiledPipeline;
  const ADomains: TInverseDomains);
type
  TBooleanArray = array of Boolean;
  TGraphValuesArray = array of TGraphValues;
var
  I: Integer;
  J: Integer;
  LAllowed: TGraphValues;
  LCandidateIndex: Integer;
  LCached: TBooleanArray;
  LCachedPassValues: TGraphValuesArray;
  LExisting: TGraphValues;
  LExistingIndex: Integer;
  LGraph: TGraph;
  LPassValues: TGraphValues;
  LWrite: Integer;

  procedure LoadPassValues(const APassIndex: Integer;
    out AValues: TGraphValues);
  begin
    if not LCached[APassIndex] then
    begin
      LCachedPassValues[APassIndex] := ACompiled.Graph.PassGraph[
        APassIndex].CopyRegisteredValues;
      LCached[APassIndex] := True;
    end;
    AValues := LCachedPassValues[APassIndex];
  end;
begin
  SetLength(LCached, ACompiled.Graph.TotalPassCount);
  SetLength(LCachedPassValues, ACompiled.Graph.TotalPassCount);
  for I := 0 to Length(ADomains) - 1 do
  begin
    LoadPassValues(ADomains[I].PassIndex, LPassValues);
    for J := 0 to Length(ADomains[I].AllowedValueIndices) - 1 do
      if (ADomains[I].AllowedValueIndices[J] < 0) or
          (ADomains[I].AllowedValueIndices[J] >= Length(LPassValues)) then
        raise EWfcPipelineRuntime.CreateFmt(
          'compiled private vocabulary is incomplete at pass %d',
          [ADomains[I].PassIndex]);

    LGraph := ACompiled.Graph.PassGraph[ADomains[I].PassIndex];
    if not LGraph.HasAllowedValues(ADomains[I].X,
        ADomains[I].Y, ADomains[I].Z) then
    begin
      SetLength(LAllowed, Length(ADomains[I].AllowedValueIndices));
      for J := 0 to Length(ADomains[I].AllowedValueIndices) - 1 do
        LAllowed[J] := LPassValues[
          ADomains[I].AllowedValueIndices[J]];
    end
    else
    begin
      { Both arrays are canonical subsequences of the registered value order.
        Walk that order once so no host-specific map or sort is needed. }
      LExisting := LGraph.CopyAllowedValues(ADomains[I].X,
        ADomains[I].Y, ADomains[I].Z);
      SetLength(LAllowed, Length(ADomains[I].AllowedValueIndices));
      LCandidateIndex := 0;
      LExistingIndex := 0;
      LWrite := 0;
      for J := 0 to Length(LPassValues) - 1 do
      begin
        if (LExistingIndex < Length(LExisting)) and
            (LExisting[LExistingIndex] = LPassValues[J]) then
        begin
          if (LCandidateIndex <
              Length(ADomains[I].AllowedValueIndices)) and
              (ADomains[I].AllowedValueIndices[LCandidateIndex] = J) then
          begin
            LAllowed[LWrite] := LPassValues[J];
            Inc(LWrite);
          end;
          Inc(LExistingIndex);
        end;
        if (LCandidateIndex <
            Length(ADomains[I].AllowedValueIndices)) and
            (ADomains[I].AllowedValueIndices[LCandidateIndex] = J) then
          Inc(LCandidateIndex);
      end;
      SetLength(LAllowed, LWrite);
    end;
    LGraph.SetAllowedValues(ADomains[I].X, ADomains[I].Y,
      ADomains[I].Z, LAllowed);
  end;
end;

procedure ApplyInputs(const ACompiled: TWfcCompiledPipeline;
  const ALocks: TEffectiveLocks; const ADomains: TEffectiveDomains);
type
  TBooleanArray = array of Boolean;
  TGraphValuesArray = array of TGraphValues;
var
  I: Integer;
  J: Integer;
  LCached: TBooleanArray;
  LCachedPassValues: TGraphValuesArray;
  LGraphValues: TGraphValues;
  LPassValues: TGraphValues;

  procedure LoadPassValues(const APassIndex: Integer;
    out AValues: TGraphValues);
  begin
    if not LCached[APassIndex] then
    begin
      LCachedPassValues[APassIndex] := ACompiled.Graph.PassGraph[
        APassIndex].CopyRegisteredValues;
      LCached[APassIndex] := True;
    end;
    AValues := LCachedPassValues[APassIndex];
  end;
begin
  SetLength(LCached, ACompiled.Graph.TotalPassCount);
  SetLength(LCachedPassValues, ACompiled.Graph.TotalPassCount);
  for I := 0 to Length(ADomains) - 1 do
  begin
    LoadPassValues(ADomains[I].PassIndex, LPassValues);
    SetLength(LGraphValues, Length(ADomains[I].AllowedTokenIndices));
    for J := 0 to Length(ADomains[I].AllowedTokenIndices) - 1 do
    begin
      if ADomains[I].AllowedTokenIndices[J] >= Length(LPassValues) then
        raise EWfcPipelineRuntime.CreateFmt(
          'compiled vocabulary is incomplete for domain %d',
          [ADomains[I].InputIndex]);
      LGraphValues[J] := LPassValues[
        ADomains[I].AllowedTokenIndices[J]];
    end;
    ACompiled.Graph.PassGraph[ADomains[I].PassIndex].SetAllowedValues(
      ADomains[I].X, ADomains[I].Y, ADomains[I].Z, LGraphValues);
  end;

  for I := 0 to Length(ALocks) - 1 do
  begin
    LoadPassValues(ALocks[I].PassIndex, LPassValues);
    if ALocks[I].TokenIndex >= Length(LPassValues) then
      raise EWfcPipelineRuntime.CreateFmt(
        'compiled vocabulary is incomplete for lock %d',
        [ALocks[I].InputIndex]);
    ACompiled.Graph.PassGraph[ALocks[I].PassIndex].Entry[
      ALocks[I].X, ALocks[I].Y, ALocks[I].Z].Value :=
      LPassValues[ALocks[I].TokenIndex];
  end;
end;

type
  {$IFDEF PAS2JS}TOwnerRegistry = array of TObject;{$ENDIF}
  TPreparationIdentity = class
    References: Integer;
    constructor Create;
    procedure Retain;
    procedure Release;
  end;
  TPreparationData = class
    Identity: TPreparationIdentity;
    Recipe: TWfcPipelineModel;
    Layouts: TWfcPipelineLayoutTable;
    FormatVersion: Integer;
    Seed: TGraphSeed;
    Vocabularies: TVocabularyArray;
    ResolvedPasses: TIntegerArray;
    TokenLookups: TTokenLookupArray;
    destructor Destroy; override;
  end;
  TInputPlanData = class
    References: Integer;
    Identity: TPreparationIdentity;
    Run: TWfcPipelineRun;
    Locks: TEffectiveLocks;
    Domains: TEffectiveDomains;
    InverseDomains: TInverseDomains;
    AuthoredDomainItems: Integer;
    MaxAuthoredDomainItems: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Retain;
    procedure Release;
  end;

{$IFDEF PAS2JS}
var Preparations, Plans, Bindings: TOwnerRegistry;

procedure RegisterOwner(var Registry: TOwnerRegistry; const Owner: TObject);
var N: Integer;
begin
  if Length(Registry)>=High(Integer) then
    raise EWfcPipelineRuntime.Create('preparation live-owner registry exceeds Integer');
  N:=Length(Registry); SetLength(Registry,N+1); Registry[N]:=Owner;
end;

procedure UnregisterOwner(var Registry: TOwnerRegistry; const Owner: TObject);
var I,J: Integer;
begin
  for I:=0 to High(Registry) do if Registry[I]=Owner then
  begin
    for J:=I+1 to High(Registry) do Registry[J-1]:=Registry[J];
    SetLength(Registry,Length(Registry)-1); Exit;
  end;
end;

procedure RequireOwner(const Registry: TOwnerRegistry; const Owner: TObject;
  const LabelText: String);
var I: Integer;
begin
  { Identity comparison reads no fields from an arbitrary JS argument.
    Only constructors in this unit register successful live wrappers. }
  for I:=0 to High(Registry) do if Registry[I]=Owner then Exit;
  raise EWfcPipelineRuntime.Create(LabelText+' is not a live preparation owner');
end;
{$ELSE}
procedure RequireOwner(const Owner: TObject; const LabelText: String);
begin
  { Native callers obey the ordinary typed-object lifetime contract. There is
    deliberately no process-global mutable owner registry on FPC threads. }
  if Owner=nil then
    raise EWfcPipelineRuntime.Create(LabelText+' is not a live preparation owner');
end;
{$ENDIF}

procedure RequirePreparation(const APreparation: TWfcPipelinePreparation);
begin
  {$IFDEF PAS2JS}RequireOwner(Preparations,APreparation,'preparation');{$ELSE}RequireOwner(APreparation,'preparation');{$ENDIF}
  if APreparation.FData=nil then
    raise EWfcPipelineRuntime.Create('preparation has not been initialized');
end;

procedure RequirePlan(const AInitialPlan: TWfcPipelineInputPlan);
begin
  {$IFDEF PAS2JS}RequireOwner(Plans,AInitialPlan,'input plan');{$ELSE}RequireOwner(AInitialPlan,'input plan');{$ENDIF}
  if AInitialPlan.FData=nil then
    raise EWfcPipelineRuntime.Create('input plan has no prepared payload');
end;

procedure RetainLease(var References: Integer; const LabelText: String);
{$IFNDEF PAS2JS}var Previous: Integer;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  if References=High(Integer) then
    raise EWfcPipelineRuntime.Create(LabelText+' lease count exceeds Integer');
  Inc(References);
  {$ELSE}
  { A caller must own a live lease while acquiring another. Destruction racing
    the last external owner is outside the typed-object lifetime contract. }
  repeat
    Previous:=System.InterlockedCompareExchange(References,0,0);
    if (Previous<=0) or (Previous=High(Integer)) then
      raise EWfcPipelineRuntime.Create(LabelText+' lease count exceeds Integer');
  until System.InterlockedCompareExchange(References,Previous+1,Previous)=Previous;
  {$ENDIF}
end;

function ReleaseLease(var References: Integer): Boolean;
begin
  {$IFDEF PAS2JS}Dec(References); Result:=References=0;
  {$ELSE}Result:=System.InterlockedDecrement(References)=0;{$ENDIF}
end;

constructor TPreparationIdentity.Create;
begin inherited Create; References:=1; end;
procedure TPreparationIdentity.Retain;
begin RetainLease(References,'identity'); end;
procedure TPreparationIdentity.Release;
begin if ReleaseLease(References) then Free; end;

destructor TPreparationData.Destroy;
begin
  FreeTokenLookups(TokenLookups); Layouts.Free;
  if Identity<>nil then Identity.Release;
  inherited Destroy;
end;

constructor TInputPlanData.Create;
begin inherited Create; References:=1; end;
destructor TInputPlanData.Destroy;
begin Run.Free; if Identity<>nil then Identity.Release; inherited Destroy; end;
procedure TInputPlanData.Retain;
begin RetainLease(References,'input-plan'); end;
procedure TInputPlanData.Release;
begin if ReleaseLease(References) then Free; end;

function CopyRunLayoutTable(const Run: TWfcPipelineRun): TWfcPipelineLayoutTable;
var Topologies: TWfcPipelinePassTopologies; I: Integer;
begin
  SetLength(Topologies,Run.PassCount);
  for I:=0 to Run.PassCount-1 do Topologies[I]:=Run.PassTopologyAt(I);
  Result:=TWfcPipelineLayoutTable.Create(Topologies,Run.CopyPassExtents);
end;

function SameLayout(const A,B: TWfcLatticeLayout): Boolean;
begin
  Result:=(A.Cells.X=B.Cells.X) and (A.Cells.Y=B.Cells.Y) and (A.Cells.Z=B.Cells.Z) and
    (A.Origin.X=B.Origin.X) and (A.Origin.Y=B.Origin.Y) and (A.Origin.Z=B.Origin.Z) and
    (A.Pitch.X=B.Pitch.X) and (A.Pitch.Y=B.Pitch.Y) and (A.Pitch.Z=B.Pitch.Z) and
    (A.Wrap=B.Wrap);
end;

procedure RequireEpoch(const Data: TPreparationData; const Run: TWfcPipelineRun);
var I: Integer;
begin
  if not Assigned(Run) then raise EWfcPipelineRuntime.Create('runtime run cannot be nil');
  if Run.RecipeSignature<>Data.Recipe.Signature then
    raise EWfcPipelineRuntime.Create('runtime recipe does not match the run provenance');
  if (Run.FormatVersion<>Data.FormatVersion) or (Run.Seed<>Data.Seed) or
      (Run.PassCount<>Data.Layouts.PassCount) then
    raise EWfcPipelineRuntime.Create('input run does not match preparation epoch');
  for I:=0 to Run.PassCount-1 do
    if (Run.PassTopologyAt(I).Rank<>Data.Layouts.PassTopologyAt(I).Rank) or
        not SameLayout(Run.PassLayoutAt(I),Data.Layouts.PassLayoutAt(I)) then
      raise EWfcPipelineRuntime.Create('input run does not match preparation epoch layout');
end;

function CopyPublicRun(const Recipe: TWfcPipelineModel; const Run: TWfcPipelineRun): TWfcPipelineRun;
begin
  if Run.FormatVersion=WFC_PIPELINE_RUN_VERSION then
    Result:=TWfcPipelineRun.Create(Recipe,Run.Width,Run.Height,Run.Depth,Run.Seed,
      Run.Strategy,Run.MaxBacktracks,Run.MaxPassBacktracks,Run.CaptureTrace,
      Run.CopyLocks,Run.CopyDomains)
  else
    Result:=TWfcPipelineRun.Create(Recipe,Run.CopyPassExtents,Run.Seed,
      Run.Strategy,Run.MaxBacktracks,Run.MaxPassBacktracks,Run.CaptureTrace,
      Run.CopyLocks,Run.CopyDomains);
end;

constructor TWfcPipelinePreparation.Create(const ARecipe: TWfcPipelineModel;
  const AInitialRun: TWfcPipelineRun);
var Data: TPreparationData;
begin
  inherited Create;
  if not Assigned(ARecipe) then raise EWfcPipelineRuntime.Create('runtime recipe cannot be nil');
  if not Assigned(AInitialRun) then raise EWfcPipelineRuntime.Create('runtime run cannot be nil');
  if AInitialRun.RecipeSignature<>ARecipe.Signature then
    raise EWfcPipelineRuntime.Create('runtime recipe does not match the run provenance');
  Data:=TPreparationData.Create; FData:=Data;
  PreflightResultAndGraphBudgets(ARecipe,AInitialRun);
  BuildVocabularies(ARecipe,Data.Vocabularies,Data.ResolvedPasses,Data.TokenLookups);
  PreflightEncodedResultBudget(ARecipe,Data.Vocabularies,AInitialRun);
  Data.Recipe:=ARecipe;
  Data.Layouts:=CopyRunLayoutTable(AInitialRun);
  Data.FormatVersion:=AInitialRun.FormatVersion; Data.Seed:=AInitialRun.Seed;
  Data.Identity:=TPreparationIdentity.Create;
  {$IFDEF PAS2JS}RegisterOwner(Preparations,Self);{$ENDIF}
end;

destructor TWfcPipelinePreparation.Destroy;
begin {$IFDEF PAS2JS}UnregisterOwner(Preparations,Self);{$ENDIF} FData.Free; inherited Destroy; end;

function TWfcPipelinePreparation.PrepareInputs(const ARun: TWfcPipelineRun): TWfcPipelineInputPlan;
var Data: TPreparationData; Payload: TInputPlanData;
  Constraints: TEffectiveConstraints; Contributions: TInverseContributions;
  I: Integer;
begin
  RequirePreparation(Self);
  Data:=TPreparationData(FData); RequireEpoch(Data,ARun);
  Payload:=TInputPlanData.Create;
  try
    { Existing ordering and limits precede every call to the compiler. }
    BuildEffectiveLocks(ARun,Data.TokenLookups,Data.ResolvedPasses,Payload.Locks);
    BuildEffectiveDomains(ARun,Data.TokenLookups,Data.ResolvedPasses,Payload.Domains);
    { Record exact authored storage before alias consolidation, without any
      additional copies or allocation on the unchanged fresh-binding path. }
    for I:=0 to High(Payload.Domains) do
    begin
      Inc(Payload.AuthoredDomainItems,Length(Payload.Domains[I].AllowedTokenIndices));
      if Length(Payload.Domains[I].AllowedTokenIndices)>Payload.MaxAuthoredDomainItems then
        Payload.MaxAuthoredDomainItems:=Length(Payload.Domains[I].AllowedTokenIndices);
    end;
    ConsolidateLocks(Payload.Locks);
    ConsolidateDomains(Payload.Domains);
    ValidateLockDomainCompatibility(Payload.Locks,Payload.Domains);
    BuildEffectiveConstraints(Payload.Locks,Payload.Domains,Data.Vocabularies,Constraints);
    BuildInverseContributions(Data.Recipe,ARun,Constraints,Contributions);
    BuildInverseDomains(Data.Recipe,Constraints,Contributions,Payload.InverseDomains);
    Payload.Run:=CopyPublicRun(Data.Recipe,ARun);
    Data.Identity.Retain; Payload.Identity:=Data.Identity;
    Result:=TWfcPipelineInputPlan.CreateOwned(Payload);
  finally Payload.Release; end;
end;

constructor TWfcPipelineInputPlan.CreateOwned(const AData: TObject);
begin
  inherited Create;
  TInputPlanData(AData).Retain; FData:=AData;
  {$IFDEF PAS2JS}RegisterOwner(Plans,Self);{$ENDIF}
end;

destructor TWfcPipelineInputPlan.Destroy;
begin
  {$IFDEF PAS2JS}UnregisterOwner(Plans,Self);{$ENDIF}
  if FData<>nil then TInputPlanData(FData).Release;
  inherited Destroy;
end;

function TWfcPipelineInputPlan.CopyLocks: TWfcPipelineCellLocks;
begin RequirePlan(Self); Result:=TInputPlanData(FData).Run.CopyLocks; end;
function TWfcPipelineInputPlan.CopyDomains: TWfcPipelineCellDomains;
begin RequirePlan(Self); Result:=TInputPlanData(FData).Run.CopyDomains; end;
function TWfcPipelineInputPlan.CopyPassLayouts: TWfcPipelineLayoutTable;
begin RequirePlan(Self); Result:=CopyRunLayoutTable(TInputPlanData(FData).Run); end;

type
  TBaseCell = record
    Key: Integer;
    HasDomain: Boolean;
    Indices: TIntegerArray;
  end;
  TBaseCells = array of TBaseCell;
  TEditableData = class
    Limits: TWfcPipelineReplacementLimits;
    Bases: TBaseCells;
  end;
  TUnionCursor = array[0..5] of Integer;
  TCellPatch = record
    Key, PassIndex, X, Y, Z: Integer;
    Rows: TUnionCursor;
    HasDomain, DomainChanged, LockChanged: Boolean;
    Values: TGraphValues;
    LockIndex: Integer;
    LockValue: TGraphValue;
  end;
  TCellPatches = array of TCellPatch;

procedure ValidateReplacementLimits(const Limits: TWfcPipelineReplacementLimits);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    Valid = Limits !== null && typeof Limits === 'object' && !Array.isArray(Limits);
    if (Valid) for (const key of ['Version','MaxRetainedCellRecords','MaxRetainedValueItems','MaxCandidateVisits']) {
      let p=Limits,d;
      while(p!==null) {d=Object.getOwnPropertyDescriptor(p,key);if(d)break;p=Object.getPrototypeOf(p);}
      if(!d || !Object.prototype.hasOwnProperty.call(d,'value') ||
         typeof d.value!=='number' || !Number.isInteger(d.value) || d.value<1 || d.value>2147483647) {Valid=false;break;}
    }
  end;
  if not Valid then raise EWfcPipelineRuntime.Create('replacement limits require passive positive Integer fields');
  {$ENDIF}
  if (Limits.Version<>1) or (Limits.MaxRetainedCellRecords<1) or
     (Limits.MaxRetainedValueItems<1) or (Limits.MaxCandidateVisits<1) then
    raise EWfcPipelineRuntime.Create('replacement limits require version 1 and positive counts');
end;

procedure Charge(var Used: Integer; const Count, Limit: Integer; const LabelText: String);
begin
  if (Count<0) or (Count>Limit-Used) then
    raise EWfcPipelineRuntime.Create('replacement '+LabelText+' limit exceeded');
  Inc(Used,Count);
end;

procedure ChargeProduct(var Used: Integer; const Count, Width, Limit: Integer;
  const LabelText: String);
begin
  if (Count<0) or (Width<0) then raise EWfcPipelineRuntime.Create('negative replacement charge');
  if (Width<>0) and (Count>(Limit-Used) div Width) then
    raise EWfcPipelineRuntime.Create('replacement '+LabelText+' limit exceeded');
  Inc(Used,Count*Width);
end;

function UnionRowKey(const Plan: TInputPlanData; const Family, Row: Integer): Integer;
begin
  Result:=High(Integer);
  if Plan=nil then Exit;
  case Family of
    0: if Row<Length(Plan.Locks) then Result:=Plan.Locks[Row].Key;
    1: if Row<Length(Plan.Domains) then Result:=Plan.Domains[Row].Key;
    2: if Row<Length(Plan.InverseDomains) then with Plan.InverseDomains[Row] do
         Result:=CellKey(PassIndex,X,Y,Z,Plan.Run);
  end;
end;

function NextUnion(const OldPlan, NewPlan: TInputPlanData; var Cursor: TUnionCursor;
  out Patch: TCellPatch): Boolean;
var I,K: Integer; P: TInputPlanData; L: TWfcLatticeLayout;
begin
  Patch:=Default(TCellPatch); Patch.Key:=High(Integer);
  for I:=0 to 5 do
  begin
    if I<3 then P:=OldPlan else P:=NewPlan;
    K:=UnionRowKey(P,I mod 3,Cursor[I]);
    if K<Patch.Key then Patch.Key:=K;
    Patch.Rows[I]:=-1;
  end;
  Result:=Patch.Key<>High(Integer); if not Result then Exit;
  for I:=0 to 5 do
  begin
    if I<3 then P:=OldPlan else P:=NewPlan;
    if UnionRowKey(P,I mod 3,Cursor[I])<>Patch.Key then Continue;
    Patch.Rows[I]:=Cursor[I];
    case I mod 3 of
      0: Patch.PassIndex:=P.Locks[Cursor[I]].PassIndex;
      1: Patch.PassIndex:=P.Domains[Cursor[I]].PassIndex;
      2: Patch.PassIndex:=P.InverseDomains[Cursor[I]].PassIndex;
    end;
    Cursor[I]:=Cursor[I]+1;
  end;
  K:=Patch.Key-NewPlan.Run.PassOffsetAt(Patch.PassIndex);
  L:=NewPlan.Run.PassLayoutAt(Patch.PassIndex);
  Patch.X:=K mod L.Cells.X; K:=K div L.Cells.X;
  Patch.Y:=K mod L.Cells.Y; Patch.Z:=K div L.Cells.Y;
end;

function FindBase(const Bases: TBaseCells; const Key: Integer): Integer;
var L,H,M: Integer;
begin
  L:=0; H:=High(Bases);
  while L<=H do
  begin
    M:=L+(H-L) div 2;
    if Bases[M].Key=Key then Exit(M);
    if Bases[M].Key<Key then L:=M+1 else H:=M-1;
  end;
  Result:=-1;
end;

procedure ChargePlan(const Plan: TInputPlanData; const Limits: TWfcPipelineReplacementLimits;
  var Cells,Values,Visits: Integer);
var I: Integer;
begin
  if Plan=nil then Exit;
  Charge(Cells,Plan.Run.PassCount,Limits.MaxRetainedCellRecords,'cell-record');
  Charge(Cells,Plan.Run.LockCount,Limits.MaxRetainedCellRecords,'cell-record');
  Charge(Cells,Plan.Run.DomainCount,Limits.MaxRetainedCellRecords,'cell-record');
  Charge(Cells,Length(Plan.Locks),Limits.MaxRetainedCellRecords,'cell-record');
  Charge(Cells,Length(Plan.Domains),Limits.MaxRetainedCellRecords,'cell-record');
  Charge(Cells,Length(Plan.InverseDomains),Limits.MaxRetainedCellRecords,'cell-record');
  Charge(Values,Plan.Run.LockCount,Limits.MaxRetainedValueItems,'value-item');
  Charge(Values,Plan.AuthoredDomainItems,Limits.MaxRetainedValueItems,'value-item');
  Charge(Values,Length(Plan.Locks),Limits.MaxRetainedValueItems,'value-item');
  for I:=0 to High(Plan.Domains) do
    Charge(Values,Length(Plan.Domains[I].AllowedTokenIndices),Limits.MaxRetainedValueItems,'value-item');
  for I:=0 to High(Plan.InverseDomains) do
    Charge(Values,Length(Plan.InverseDomains[I].AllowedValueIndices),Limits.MaxRetainedValueItems,'value-item');
  { DomainAt's detached row during authored comparison. }
  Charge(Values,Plan.MaxAuthoredDomainItems,Limits.MaxRetainedValueItems,'value-item');
  Charge(Visits,Plan.Run.LockCount,Limits.MaxCandidateVisits,'candidate-visit');
  Charge(Visits,Plan.Run.DomainCount,Limits.MaxCandidateVisits,'candidate-visit');
  Charge(Visits,Plan.AuthoredDomainItems,Limits.MaxCandidateVisits,'candidate-visit');
  Charge(Visits,Length(Plan.Locks),Limits.MaxCandidateVisits,'candidate-visit');
  Charge(Visits,Length(Plan.Domains),Limits.MaxCandidateVisits,'candidate-visit');
  Charge(Visits,Length(Plan.InverseDomains),Limits.MaxCandidateVisits,'candidate-visit');
end;

procedure PreflightReplacement(const Compiled: TWfcCompiledPipeline;
  const OldPlan,NewPlan: TInputPlanData; const Edit: TEditableData;
  out UnionCount,NewBaseCount: Integer);
var Cells,Values,Visits,I,V,B,MaxV: Integer; Cursor: TUnionCursor; P: TCellPatch;
begin
  Cells:=0; Values:=0; Visits:=0; MaxV:=0;
  ChargePlan(NewPlan,Edit.Limits,Cells,Values,Visits);
  if OldPlan<>NewPlan then ChargePlan(OldPlan,Edit.Limits,Cells,Values,Visits);
  ChargeProduct(Cells,Length(Edit.Bases),2,Edit.Limits.MaxRetainedCellRecords,'cell-record');
  ChargeProduct(Visits,Length(Edit.Bases),3,Edit.Limits.MaxCandidateVisits,'candidate-visit');
  for I:=0 to High(Edit.Bases) do
    Charge(Values,Length(Edit.Bases[I].Indices),Edit.Limits.MaxRetainedValueItems,'value-item');
  { Two detached pass arrays, and two transient DomainAt records. }
  ChargeProduct(Cells,NewPlan.Run.PassCount,2,Edit.Limits.MaxRetainedCellRecords,'cell-record');
  Charge(Cells,2,Edit.Limits.MaxRetainedCellRecords,'cell-record');
  ChargeProduct(Visits,NewPlan.Run.PassCount,2,Edit.Limits.MaxCandidateVisits,'candidate-visit');
  Cursor:=Default(TUnionCursor); UnionCount:=0; NewBaseCount:=Length(Edit.Bases);
  while NextUnion(OldPlan,NewPlan,Cursor,P) do
  begin
    Inc(UnionCount);
    Charge(Cells,1,Edit.Limits.MaxRetainedCellRecords,'cell-record');
    V:=Compiled.Graph.PassGraph[P.PassIndex].RuleGroups.Count;
    if V>MaxV then MaxV:=V;
    B:=FindBase(Edit.Bases,P.Key);
    if B<0 then
    begin
      Inc(NewBaseCount);
      Charge(Cells,1,Edit.Limits.MaxRetainedCellRecords,'cell-record');
      Charge(Values,V,Edit.Limits.MaxRetainedValueItems,'value-item');
    end;
    { Final desired values + one optional lock value. Base indices are charged
      above; temporary registered/current arrays are charged once at max size. }
    Charge(Values,V,Edit.Limits.MaxRetainedValueItems,'value-item');
    Charge(Values,1,Edit.Limits.MaxRetainedValueItems,'value-item');
    { Conservative bound includes canonical setter membership scans AND
      repeated append copying (quadratic), base-index conversion, intersections,
      all six cursor reads, and binary searches (Integer width <=31). }
    Charge(Visits,256,Edit.Limits.MaxCandidateVisits,'candidate-visit');
    ChargeProduct(Visits,V,80,Edit.Limits.MaxCandidateVisits,'candidate-visit');
    for I:=1 to 4 do ChargeProduct(Visits,V,V,Edit.Limits.MaxCandidateVisits,'candidate-visit');
  end;
  { One vocabulary, one current-domain copy, and the core setter's temporary
    canonical vector. No registered vocabulary survives the operation. }
  ChargeProduct(Values,MaxV,3,Edit.Limits.MaxRetainedValueItems,'value-item');
end;

function SameTokens(const A,B: TWfcModelTokens): Boolean;
var I: Integer;
begin
  Result:=False; if Length(A)<>Length(B) then Exit;
  for I:=0 to High(A) do if A[I]<>B[I] then Exit;
  Result:=True;
end;

procedure CompactPasses(var A: TGraphPassIndices);
var I,N: Integer;
begin
  N:=0; for I:=0 to High(A) do if A[I]>=0 then begin A[N]:=I; Inc(N); end;
  SetLength(A,N);
end;

procedure AuthoredImpact(const OldPlan,NewPlan: TInputPlanData; var Impact: TWfcPipelineInputImpact);
var I,N: Integer; A,B: TWfcPipelineCellLock; C,D: TWfcPipelineCellDomain; Same: Boolean;
begin
  if OldPlan=NewPlan then Exit;
  if OldPlan=nil then
  begin
    for I:=0 to NewPlan.Run.LockCount-1 do Impact.AuthoredPassIndices[NewPlan.Run.LockAt(I).PassIndex]:=0;
    for I:=0 to NewPlan.Run.DomainCount-1 do Impact.AuthoredPassIndices[NewPlan.Run.DomainAt(I).PassIndex]:=0;
    Exit;
  end;
  N:=OldPlan.Run.LockCount; if NewPlan.Run.LockCount>N then N:=NewPlan.Run.LockCount;
  for I:=0 to N-1 do
  begin
    Same:=False;
    if I<OldPlan.Run.LockCount then A:=OldPlan.Run.LockAt(I);
    if I<NewPlan.Run.LockCount then B:=NewPlan.Run.LockAt(I);
    if (I<OldPlan.Run.LockCount) and (I<NewPlan.Run.LockCount) then
      Same:=(A.PassIndex=B.PassIndex) and (A.X=B.X) and (A.Y=B.Y) and (A.Z=B.Z) and (A.Token=B.Token);
    if not Same then
    begin
      if I<OldPlan.Run.LockCount then Impact.AuthoredPassIndices[A.PassIndex]:=0;
      if I<NewPlan.Run.LockCount then Impact.AuthoredPassIndices[B.PassIndex]:=0;
    end;
  end;
  N:=OldPlan.Run.DomainCount; if NewPlan.Run.DomainCount>N then N:=NewPlan.Run.DomainCount;
  for I:=0 to N-1 do
  begin
    Same:=False;
    if I<OldPlan.Run.DomainCount then C:=OldPlan.Run.DomainAt(I);
    if I<NewPlan.Run.DomainCount then D:=NewPlan.Run.DomainAt(I);
    if (I<OldPlan.Run.DomainCount) and (I<NewPlan.Run.DomainCount) then
      Same:=(C.PassIndex=D.PassIndex) and (C.X=D.X) and (C.Y=D.Y) and (C.Z=D.Z) and SameTokens(C.AllowedTokens,D.AllowedTokens);
    if not Same then
    begin
      if I<OldPlan.Run.DomainCount then Impact.AuthoredPassIndices[C.PassIndex]:=0;
      if I<NewPlan.Run.DomainCount then Impact.AuthoredPassIndices[D.PassIndex]:=0;
    end;
  end;
end;

procedure StageReplacement(const Compiled: TWfcCompiledPipeline;
  const OldPlan,NewPlan: TInputPlanData; const Edit: TEditableData;
  out Bases: TBaseCells; out Patches: TCellPatches; out Impact: TWfcPipelineInputImpact);
var N,M,I,J,K,B,V,WriteAt,ReadAt,OldLock,NewLock: Integer;
  Cursor: TUnionCursor; P: TCellPatch; G: TGraph;
  Registered,Current: TGraphValues; Base: TBaseCell; Admit: Boolean;
begin
  { Caller has selected root zero. This entire routine is read-only on graph
    and live cache; all budget rejection and output allocation precede setters. }
  PreflightReplacement(Compiled,OldPlan,NewPlan,Edit,N,M);
  SetLength(Bases,M); SetLength(Patches,N);
  Impact:=Default(TWfcPipelineInputImpact);
  SetLength(Impact.AuthoredPassIndices,NewPlan.Run.PassCount);
  SetLength(Impact.ChangedPassIndices,NewPlan.Run.PassCount);
  for I:=0 to NewPlan.Run.PassCount-1 do
  begin Impact.AuthoredPassIndices[I]:=-1; Impact.ChangedPassIndices[I]:=-1; end;
  AuthoredImpact(OldPlan,NewPlan,Impact);
  Cursor:=Default(TUnionCursor); I:=0; WriteAt:=0; ReadAt:=0;
  while NextUnion(OldPlan,NewPlan,Cursor,P) do
  begin
    while (ReadAt<Length(Edit.Bases)) and (Edit.Bases[ReadAt].Key<P.Key) do
    begin Bases[WriteAt]:=Edit.Bases[ReadAt]; Inc(WriteAt); Inc(ReadAt); end;
    G:=Compiled.Graph.PassGraph[P.PassIndex]; Registered:=G.CopyRegisteredValues;
    V:=Length(Registered);
    Base:=Default(TBaseCell); Base.Key:=P.Key;
    B:=FindBase(Edit.Bases,P.Key);
    if B>=0 then
    begin Base:=Edit.Bases[B]; Inc(ReadAt); end
    else
    begin
      Base.HasDomain:=G.HasAllowedValues(P.X,P.Y,P.Z);
      if Base.HasDomain then
      begin
        Current:=G.CopyAllowedValues(P.X,P.Y,P.Z);
        SetLength(Base.Indices,Length(Current)); K:=0;
        for J:=0 to V-1 do
          if (K<Length(Current)) and (Registered[J]=Current[K]) then
          begin Base.Indices[K]:=J; Inc(K); end;
        if K<>Length(Current) then raise EWfcPipelineRuntime.Create('compiler base is not canonical');
      end;
    end;
    Bases[WriteAt]:=Base; Inc(WriteAt);
    { Public model/rule/empty adapters have no compiler domains. Refuse future
      overlap instead of silently changing initial public overwrite semantics. }
    if (Compiled.Recipe.PassAt(P.PassIndex).Visibility=wppvPublic) and Base.HasDomain then
      raise EWfcPipelineRuntime.Create('public compiler base domain is unsupported by editable binding');
    if (P.Rows[4]>=0) and (P.Rows[5]>=0) then
      raise EWfcPipelineRuntime.Create('public and inverse domains overlap unexpectedly');
    P.HasDomain:=Base.HasDomain or (P.Rows[4]>=0) or (P.Rows[5]>=0);
    if P.HasDomain then
    begin
      SetLength(P.Values,V); K:=0;
      for J:=0 to V-1 do
      begin
        Admit:=not Base.HasDomain or ContainsIndex(Base.Indices,J);
        if P.Rows[4]>=0 then Admit:=Admit and ContainsIndex(NewPlan.Domains[P.Rows[4]].AllowedTokenIndices,J);
        if P.Rows[5]>=0 then Admit:=Admit and ContainsIndex(NewPlan.InverseDomains[P.Rows[5]].AllowedValueIndices,J);
        if Admit then begin P.Values[K]:=Registered[J]; Inc(K); end;
      end;
      SetLength(P.Values,K);
    end;
    P.DomainChanged:=P.HasDomain<>G.HasAllowedValues(P.X,P.Y,P.Z);
    if P.HasDomain and not P.DomainChanged then
    begin
      Current:=G.CopyAllowedValues(P.X,P.Y,P.Z);
      P.DomainChanged:=Length(Current)<>Length(P.Values);
      if not P.DomainChanged then
        for J:=0 to High(Current) do if Current[J]<>P.Values[J] then begin P.DomainChanged:=True; Break; end;
    end;
    OldLock:=-1; NewLock:=-1;
    if P.Rows[0]>=0 then OldLock:=OldPlan.Locks[P.Rows[0]].TokenIndex;
    if P.Rows[3]>=0 then NewLock:=NewPlan.Locks[P.Rows[3]].TokenIndex;
    P.LockChanged:=OldLock<>NewLock; P.LockIndex:=NewLock;
    if NewLock>=0 then
    begin
      if NewLock>=V then raise EWfcPipelineRuntime.Create('compiled replacement lock vocabulary is incomplete');
      P.LockValue:=Registered[NewLock];
    end;
    if P.DomainChanged or P.LockChanged then Impact.ChangedPassIndices[P.PassIndex]:=0;
    Patches[I]:=P; Inc(I);
  end;
  while ReadAt<Length(Edit.Bases) do
  begin Bases[WriteAt]:=Edit.Bases[ReadAt]; Inc(WriteAt); Inc(ReadAt); end;
  if (WriteAt<>M) or (I<>N) then raise EWfcPipelineRuntime.Create('replacement preflight shape mismatch');
  CompactPasses(Impact.AuthoredPassIndices); CompactPasses(Impact.ChangedPassIndices);
  Impact.AuthoredInputsChanged:=Length(Impact.AuthoredPassIndices)>0;
  Impact.GraphInputsChanged:=Length(Impact.ChangedPassIndices)>0;
end;

constructor TWfcPipelineInputBinding.CreateEditable(const APreparation: TWfcPipelinePreparation;
  const AInitialPlan: TWfcPipelineInputPlan; const ALimits: TWfcPipelineReplacementLimits);
var D: TPreparationData; P: TInputPlanData; E: TEditableData;
  Bases: TBaseCells; Patches: TCellPatches; Impact: TWfcPipelineInputImpact;
begin
  inherited Create;
  ValidateReplacementLimits(ALimits);
  RequirePreparation(APreparation); RequirePlan(AInitialPlan);
  D:=TPreparationData(APreparation.FData); P:=TInputPlanData(AInitialPlan.FData);
  if P.Identity<>D.Identity then raise EWfcPipelineRuntime.Create('input plan belongs to a different preparation lifetime');
  RequireEpoch(D,P.Run);
  P.Retain; FData:=P;
  E:=TEditableData.Create; FEditable:=E; E.Limits:=ALimits;
  FCompiled:=CompileWfcPipeline(D.Recipe,P.Run.CopyPassExtents);
  { Compiler selected zero. Base capture MUST precede both initial installers. }
  StageReplacement(FCompiled,nil,P,E,Bases,Patches,Impact);
  { Initial installers cache whole touched-pass vocabularies. Release the
    unused patch vectors first: their sum(per-key registry capacity) bounds
    sum(distinct touched-pass registries). The two released impact arrays
    likewise cover each installer's Boolean + vocabulary outer arrays. The
    installers are sequential; the three-MaxV scratch allowance covers their
    allowed/current/canonical vectors. Only pristine Bases must survive. }
  Patches:=nil; Impact:=Default(TWfcPipelineInputImpact);
  ApplyInverseDomains(FCompiled,P.InverseDomains);
  ApplyInputs(FCompiled,P.Locks,P.Domains);
  FCompiled.Graph.Seed:=P.Run.Seed;
  E.Bases:=Bases; FUsable:=True;
  {$IFDEF PAS2JS}RegisterOwner(Bindings,Self);{$ENDIF}
end;

function TWfcPipelineInputBinding.ReplaceInputs(const ADesiredPlan: TWfcPipelineInputPlan): TWfcPipelineInputImpact;
var OldPlan,NewPlan: TInputPlanData; E: TEditableData; G,Target: TGraph;
  Saved: Integer; Bases: TBaseCells; Patches: TCellPatches; I: Integer; Started: Boolean;
begin
  G:=BorrowCompiled.Graph;
  if FEditable=nil then raise EWfcPipelineRuntime.Create('input binding is fresh-only, not editable');
  RequirePlan(ADesiredPlan);
  OldPlan:=TInputPlanData(FData); NewPlan:=TInputPlanData(ADesiredPlan.FData);
  if OldPlan.Identity<>NewPlan.Identity then
    raise EWfcPipelineRuntime.Create('input plan belongs to a different preparation lifetime');
  if G.Running then raise EWfcPipelineRuntime.Create('cannot replace inputs while graph is running');
  if OldPlan=NewPlan then Exit(Default(TWfcPipelineInputImpact));
  E:=TEditableData(FEditable);
  NewPlan.Retain;
  try
    Saved:=G.CurrentPassIndex; Started:=False;
    try
      G.SwitchToPass(0);
      StageReplacement(FCompiled,OldPlan,NewPlan,E,Bases,Patches,Result);
      for I:=0 to High(Patches) do if Patches[I].DomainChanged then
      begin
        Target:=G.PassGraph[Patches[I].PassIndex]; Started:=True;
        if Patches[I].HasDomain then Target.SetAllowedValues(Patches[I].X,Patches[I].Y,Patches[I].Z,Patches[I].Values)
        else Target.ClearAllowedValues(Patches[I].X,Patches[I].Y,Patches[I].Z);
      end;
      for I:=0 to High(Patches) do if Patches[I].LockChanged then
      begin
        Target:=G.PassGraph[Patches[I].PassIndex]; Started:=True;
        if Patches[I].LockIndex>=0 then Target.Entry[Patches[I].X,Patches[I].Y,Patches[I].Z].Value:=Patches[I].LockValue
        else Target.Entry[Patches[I].X,Patches[I].Y,Patches[I].Z].ClearValue;
      end;
      E.Bases:=Bases;
      FData:=NewPlan; NewPlan:=nil; OldPlan.Release;
    except
      if Started then FUsable:=False;
      raise;
    end;
  finally
    { The original valid label is retained, including on read-only rejection. }
    G.SwitchToPass(Saved);
    if NewPlan<>nil then NewPlan.Release;
  end;
end;

constructor TWfcPipelineInputBinding.Create(const APreparation: TWfcPipelinePreparation;
  const AInitialPlan: TWfcPipelineInputPlan);
var PreparationData: TPreparationData; Payload: TInputPlanData; Compiled: TWfcCompiledPipeline;
begin
  inherited Create;
  RequirePreparation(APreparation);
  RequirePlan(AInitialPlan);
  PreparationData:=TPreparationData(APreparation.FData);
  Payload:=TInputPlanData(AInitialPlan.FData);
  if Payload.Identity<>PreparationData.Identity then
    raise EWfcPipelineRuntime.Create('input plan belongs to a different preparation lifetime');
  RequireEpoch(PreparationData,Payload.Run);
  Payload.Retain; FData:=Payload;
  Compiled:=nil;
  try
    { No graph allocation occurs until the pure plan and every existing result,
      actual-cell and inverse-work budget have already completed. }
    Compiled:=CompileWfcPipeline(PreparationData.Recipe,Payload.Run.CopyPassExtents);
    ApplyInverseDomains(Compiled,Payload.InverseDomains);
    ApplyInputs(Compiled,Payload.Locks,Payload.Domains);
    Compiled.Graph.Seed:=Payload.Run.Seed;
    FCompiled:=Compiled; Compiled:=nil;
    FUsable:=True;
    {$IFDEF PAS2JS}RegisterOwner(Bindings,Self);{$ENDIF}
  finally Compiled.Free; end;
end;

destructor TWfcPipelineInputBinding.Destroy;
begin
  {$IFDEF PAS2JS}UnregisterOwner(Bindings,Self);{$ENDIF} FCompiled.Free;
  FEditable.Free;
  if FData<>nil then TInputPlanData(FData).Release;
  inherited Destroy;
end;

function TWfcPipelineInputBinding.BorrowCompiled: TWfcCompiledPipeline;
begin {$IFDEF PAS2JS}RequireOwner(Bindings,Self,'input binding');{$ELSE}RequireOwner(Self,'input binding');{$ENDIF}
  if (FData=nil) or (FCompiled=nil) then
    raise EWfcPipelineRuntime.Create('input binding has not been initialized');
  if not FUsable then raise EWfcPipelineRuntime.Create('input binding is unusable after installation failure');
  Result:=FCompiled; end;

function TWfcPipelineInputBinding.GetUsable: Boolean;
begin
  {$IFDEF PAS2JS}RequireOwner(Bindings,Self,'input binding');{$ELSE}RequireOwner(Self,'input binding');{$ENDIF}
  Result:=(FData<>nil) and (FCompiled<>nil) and FUsable;
end;

end.
