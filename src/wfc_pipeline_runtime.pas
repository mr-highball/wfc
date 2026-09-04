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
unit wfc_pipeline_runtime;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_pipeline_model,
  wfc_pipeline_run,
  wfc_pipeline_compile,
  wfc_pipeline_result;

const
  WFC_PIPELINE_RUNTIME_VERSION = 1;

  { A run already limits one grid to 4,194,304 cells. The executable boundary
    additionally limits the sum of every materialized pass grid so a recipe
    cannot multiply a small-looking run into an unbounded allocation. }
  WFC_PIPELINE_RUNTIME_MAX_TOTAL_PASS_CELL_COUNT = 16777216;

type
  EWfcPipelineRuntime = class(Exception);

  { Owns one prepared executable graph. Recipe and Run are immutable borrowed
    inputs and must outlive this object. Execute returns a detached result
    owned by the caller. Repeated execution rewinds the core's versioned random
    streams and therefore replays the same invocation. }
  TWfcPipelineRuntime = class
  strict private
    FRecipe: TWfcPipelineModel;
    FRun: TWfcPipelineRun;
    FCompiled: TWfcCompiledPipeline;
    procedure Initialize(const ARecipe: TWfcPipelineModel;
      const ARun: TWfcPipelineRun);
  public
    constructor Create(const ARecipe: TWfcPipelineModel;
      const ARun: TWfcPipelineRun);
    destructor Destroy; override;

    function Execute: TWfcPipelineResult;

    property Recipe: TWfcPipelineModel read FRecipe;
    property Run: TWfcPipelineRun read FRun;
  end;

function ExecuteWfcPipeline(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun): TWfcPipelineResult;

implementation

uses
  wfc_text_codec,
  wfc_token_lookup;

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

function CheckedCellCount(const ARun: TWfcPipelineRun): Integer;
var
  LPlane: Integer;
begin
  if ARun.Width > WFC_PIPELINE_RUN_MAX_CELL_COUNT div ARun.Height then
    raise EWfcPipelineRuntime.Create(
      'run cell count exceeds the executable limit');
  LPlane := ARun.Width * ARun.Height;
  if LPlane > WFC_PIPELINE_RUN_MAX_CELL_COUNT div ARun.Depth then
    raise EWfcPipelineRuntime.Create(
      'run cell count exceeds the executable limit');
  Result := LPlane * ARun.Depth;
end;

procedure PreflightResultAndGraphBudgets(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun; out ACellCount: Integer);
var
  I: Integer;
  LPublicCount: Integer;
begin
  ACellCount := CheckedCellCount(ARun);
  if (ARecipe.PassCount <> 0) and
      (ACellCount > WFC_PIPELINE_RUNTIME_MAX_TOTAL_PASS_CELL_COUNT div
      ARecipe.PassCount) then
    raise EWfcPipelineRuntime.CreateFmt(
      'pipeline materialization exceeds the runtime pass-cell limit [%d x %d]',
      [ARecipe.PassCount, ACellCount]);

  LPublicCount := 0;
  for I := 0 to ARecipe.PassCount - 1 do
    if ARecipe.PassAt(I).Visibility = wppvPublic then
      Inc(LPublicCount);
  if LPublicCount > WFC_PIPELINE_RESULT_MAX_PUBLIC_LAYER_COUNT then
    raise EWfcPipelineRuntime.Create(
      'pipeline public layer count exceeds the result limit');
  if (LPublicCount <> 0) and
      (ACellCount > WFC_PIPELINE_RESULT_MAX_TOTAL_PUBLIC_CELL_COUNT div
      LPublicCount) then
    raise EWfcPipelineRuntime.CreateFmt(
      'pipeline public output exceeds the result cell limit [%d x %d]',
      [LPublicCount, ACellCount]);
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
  const AVocabularies: TVocabularyArray; const ACellCount: Integer);
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
        (ACellCount > LRemaining div LMaximumTokenLength) then
      raise EWfcPipelineRuntime.CreateFmt(
        'public pass %d can exceed the result encoded-token budget', [I]);
    Inc(LTotal, ACellCount * LMaximumTokenLength);
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

function CellKey(const APassIndex, AX, AY, AZ,
  ACellCount: Integer; const ARun: TWfcPipelineRun): Integer;
var
  LCell: Integer;
begin
  LCell := (AZ * ARun.Height + AY) * ARun.Width + AX;
  if APassIndex > (High(Integer) - LCell) div ACellCount then
    raise EWfcPipelineRuntime.Create('effective input key exceeds Integer');
  Result := APassIndex * ACellCount + LCell;
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
  const ACellCount: Integer; out AValues: TEffectiveLocks);
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
      LInput.Z, ACellCount, ARun);
    AValues[I].InputIndex := I;
  end;
end;

procedure BuildEffectiveDomains(const ARun: TWfcPipelineRun;
  const ALookups: TTokenLookupArray;
  const AResolvedPasses: TIntegerArray;
  const ACellCount: Integer; out AValues: TEffectiveDomains);
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
      LInput.Z, ACellCount, ARun);
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

constructor TWfcPipelineRuntime.Create(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun);
begin
  inherited Create;
  Initialize(ARecipe, ARun);
end;

destructor TWfcPipelineRuntime.Destroy;
begin
  FCompiled.Free;
  FCompiled := nil;
  FRun := nil;
  FRecipe := nil;
  inherited Destroy;
end;

procedure TWfcPipelineRuntime.Initialize(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun);
var
  LCellCount: Integer;
  LCompiled: TWfcCompiledPipeline;
  LDomains: TEffectiveDomains;
  LLocks: TEffectiveLocks;
  LResolvedPasses: TIntegerArray;
  LTokenLookups: TTokenLookupArray;
  LVocabularies: TVocabularyArray;
begin
  if not Assigned(ARecipe) then
    raise EWfcPipelineRuntime.Create('runtime recipe cannot be nil');
  if not Assigned(ARun) then
    raise EWfcPipelineRuntime.Create('runtime run cannot be nil');
  if ARun.RecipeSignature <> ARecipe.Signature then
    raise EWfcPipelineRuntime.Create(
      'runtime recipe does not match the run provenance');

  LTokenLookups := nil;
  try
    PreflightResultAndGraphBudgets(ARecipe, ARun, LCellCount);
    BuildVocabularies(ARecipe, LVocabularies, LResolvedPasses,
      LTokenLookups);
    PreflightEncodedResultBudget(ARecipe, LVocabularies, LCellCount);
    BuildEffectiveLocks(ARun, LTokenLookups, LResolvedPasses,
      LCellCount, LLocks);
    BuildEffectiveDomains(ARun, LTokenLookups, LResolvedPasses,
      LCellCount, LDomains);
    ConsolidateLocks(LLocks);
    ConsolidateDomains(LDomains);
    ValidateLockDomainCompatibility(LLocks, LDomains);

    LCompiled := nil;
    try
      LCompiled := CompileWfcPipeline(ARecipe, ARun.Width,
        ARun.Height, ARun.Depth);
      ApplyInputs(LCompiled, LLocks, LDomains);
      LCompiled.Graph.Seed := ARun.Seed;
      FRecipe := ARecipe;
      FRun := ARun;
      FCompiled := LCompiled;
      LCompiled := nil;
    finally
      LCompiled.Free;
    end;
  finally
    FreeTokenLookups(LTokenLookups);
  end;
end;

function TWfcPipelineRuntime.Execute: TWfcPipelineResult;
var
  LNegotiationOptions: TGraphNegotiationOptions;
  LNegotiationReport: TGraphNegotiationReport;
  LSolveOptions: TGraphSolveOptions;
  LSolveReport: TGraphSolveReport;
begin
  Result := nil;
  case Ord(FRun.Strategy) of
    Ord(wpssOneWay):
      begin
        LSolveOptions := Default(TGraphSolveOptions);
        LSolveOptions.MaxBacktracks := FRun.MaxBacktracks;
        LSolveOptions.CaptureTrace := FRun.CaptureTrace;
        FCompiled.Graph.TrySolve(LSolveOptions, LSolveReport);
        Result := CreateWfcPipelineResultFromSolveReport(
          FRecipe, FRun, FCompiled.Graph, LSolveReport);
      end;
    Ord(wpssNegotiated):
      begin
        LNegotiationOptions := Default(TGraphNegotiationOptions);
        LNegotiationOptions.SolveOptions.MaxBacktracks :=
          FRun.MaxBacktracks;
        LNegotiationOptions.SolveOptions.CaptureTrace :=
          FRun.CaptureTrace;
        LNegotiationOptions.MaxPassBacktracks :=
          FRun.MaxPassBacktracks;
        FCompiled.Graph.TrySolveNegotiated(
          LNegotiationOptions, LNegotiationReport);
        Result := CreateWfcPipelineResultFromNegotiationReport(
          FRecipe, FRun, FCompiled.Graph, LNegotiationReport);
      end;
  else
    raise EWfcPipelineRuntime.Create('runtime solve strategy is unknown');
  end;
end;

function ExecuteWfcPipeline(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun): TWfcPipelineResult;
var
  LRuntime: TWfcPipelineRuntime;
begin
  Result := nil;
  LRuntime := TWfcPipelineRuntime.Create(ARecipe, ARun);
  try
    Result := LRuntime.Execute;
  finally
    LRuntime.Free;
  end;
end;

end.
