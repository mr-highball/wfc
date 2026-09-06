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
unit wfc_pipeline_connectivity;

{$mode delphi}{$H+}

interface

uses SysUtils, wfc, wfc_model, wfc_pipeline_model;

const
  { Per immutable invocation, not a solver elapsed-time guarantee. }
  WFC_PIPELINE_CONNECTIVITY_MAX_CELL_VISITS = 16777216;
  WFC_PIPELINE_CONNECTIVITY_MAX_MODEL_VISITS = 16777216;

type
  EWfcPipelineConnectivity = class(Exception);

function WfcPipelineMaterializedPublicPass(const ARecipe: TWfcPipelineModel;
  const APassIndex: Integer): Integer;
function WfcPipelineProjectionBridgeForPass(const ARecipe: TWfcPipelineModel;
  const APassIndex: Integer): Integer;

{ Called before graph or traversal storage is allocated. AFailedIndex is the
  offending immutable descriptor, or -1 for an invocation-wide shape error. }
procedure PreflightWfcPipelineConnectivity(const ARecipe: TWfcPipelineModel;
  const AWidth, AHeight, ADepth: Integer; out AFailedIndex: Integer);

{ Independent exact public-token traversal. No graph, solver analyzer, mutable
  constraint registry or solver reachability cache is consulted. Callers must
  first preflight the complete invocation's aggregate work. }
function ValidateWfcPipelineConnectivity(const ARecipe: TWfcPipelineModel;
  const AConnectivityIndex, AWidth, AHeight, ADepth: Integer;
  const ATokens: TWfcModelTokens; out AFailedEntryIndex: Integer): Boolean;

implementation

uses wfc_rule_model, wfc_pipeline_run, wfc_token_lookup;

type
  TIntegers = array of Integer;
  TBytes = array of Byte;

procedure RequireInteger(const AValue, AMinimum, AMaximum: Integer;
  const AName: String);
begin
  if not ((AValue >= AMinimum) and (AValue <= AMaximum)) then
    raise EWfcPipelineConnectivity.Create(AName + ' is out of bounds');
  {$IFDEF PAS2JS}
  if AValue <> Trunc(AValue) then
    raise EWfcPipelineConnectivity.Create(AName + ' must be an exact integer');
  {$ENDIF}
end;

function CellCount(const ARecipe: TWfcPipelineModel;
  const AWidth, AHeight, ADepth: Integer): Integer;
begin
  if not Assigned(ARecipe) then
    raise EWfcPipelineConnectivity.Create('connectivity recipe cannot be nil');
  RequireInteger(AWidth, 1, WFC_PIPELINE_RUN_MAX_DIMENSION, 'connectivity width');
  RequireInteger(AHeight, 1, WFC_PIPELINE_RUN_MAX_DIMENSION, 'connectivity height');
  RequireInteger(ADepth, 1, WFC_PIPELINE_RUN_MAX_DIMENSION, 'connectivity depth');
  if (ARecipe.Rank = 1) and ((AHeight <> 1) or (ADepth <> 1)) then
    raise EWfcPipelineConnectivity.Create('rank-one connectivity needs height/depth one');
  if (ARecipe.Rank = 2) and (ADepth <> 1) then
    raise EWfcPipelineConnectivity.Create('rank-two connectivity needs depth one');
  if AWidth > WFC_PIPELINE_RUN_MAX_CELL_COUNT div AHeight then
    raise EWfcPipelineConnectivity.Create('connectivity cell count exceeds the limit');
  Result := AWidth * AHeight;
  if Result > WFC_PIPELINE_RUN_MAX_CELL_COUNT div ADepth then
    raise EWfcPipelineConnectivity.Create('connectivity cell count exceeds the limit');
  Result := Result * ADepth;
end;

procedure ValidatePosition(const APosition: TGraphPosition;
  const AWidth, AHeight, ADepth: Integer);
begin
  RequireInteger(APosition.X, 0, AWidth - 1, 'connectivity X');
  RequireInteger(APosition.Y, 0, AHeight - 1, 'connectivity Y');
  RequireInteger(APosition.Z, 0, ADepth - 1, 'connectivity Z');
end;

function PositionIndex(const APosition: TGraphPosition;
  const AWidth, AHeight: Integer): Integer;
begin
  Result := (APosition.Z * AHeight + APosition.Y) * AWidth + APosition.X;
end;

function WfcPipelineMaterializedPublicPass(const ARecipe: TWfcPipelineModel;
  const APassIndex: Integer): Integer;
var I: Integer;
begin
  if not Assigned(ARecipe) then
    raise EWfcPipelineConnectivity.Create('recipe cannot be nil');
  RequireInteger(APassIndex, 0, ARecipe.PassCount - 1, 'public owner');
  Result := APassIndex;
  for I := 0 to ARecipe.PassCount - 1 do
  begin
    if ARecipe.PassAt(Result).Mode <> gpmTransform then Exit;
    Result := ARecipe.PassAt(Result).TransformSourceIndex;
  end;
  raise EWfcPipelineConnectivity.Create('public transform chain does not terminate');
end;

function WfcPipelineProjectionBridgeForPass(const ARecipe: TWfcPipelineModel;
  const APassIndex: Integer): Integer;
var I: Integer;
begin
  for I := 0 to ARecipe.BridgeCount - 1 do
    if ARecipe.BridgeAt(I).TargetPassIndex = APassIndex then Exit(I);
  Result := -1;
end;

procedure PreflightWfcPipelineConnectivity(const ARecipe: TWfcPipelineModel;
  const AWidth, AHeight, ADepth: Integer; out AFailedIndex: Integer);
var I, J, C, LPassIndex, LBridgeIndex, LCount, LCells, LModel: Integer;
  Q: TWfcPipelineConnectivity; P: TWfcPipelinePass;

  procedure AddModelWork(const ACount: Integer);
  begin
    if ACount > WFC_PIPELINE_CONNECTIVITY_MAX_MODEL_VISITS - LModel then
      raise EWfcPipelineConnectivity.Create('aggregate connectivity model visits exceed the limit');
    Inc(LModel, ACount);
  end;
begin
  AFailedIndex := -1;
  if not Assigned(ARecipe) then
    raise EWfcPipelineConnectivity.Create('connectivity recipe cannot be nil');
  if ARecipe.ConnectivityCount = 0 then Exit;
  C := CellCount(ARecipe, AWidth, AHeight, ADepth);
  LCells := 0; LModel := 0;
  for I := 0 to ARecipe.ConnectivityCount - 1 do
  begin
    AFailedIndex := I;
    Q := ARecipe.ConnectivityAt(I);
    ValidatePosition(Q.Root, AWidth, AHeight, ADepth);
    for J := 0 to Length(Q.RequiredPositions) - 1 do
      ValidatePosition(Q.RequiredPositions[J], AWidth, AHeight, ADepth);
    if C > WFC_PIPELINE_CONNECTIVITY_MAX_CELL_VISITS - LCells then
      raise EWfcPipelineConnectivity.Create('aggregate connectivity cell visits exceed the limit');
    Inc(LCells, C);
    AddModelWork(Length(Q.Values));
    AddModelWork(Length(Q.RequiredPositions));
    AddModelWork(Length(ARecipe.CopyPublicVocabulary(Q.PassIndex)));
    LPassIndex := WfcPipelineMaterializedPublicPass(ARecipe, Q.PassIndex);
    P := ARecipe.PassAt(LPassIndex);
    if P.AdapterKind = wpakRules then
      AddModelWork(ARecipe.BorrowRuleResource(P.ResourceIndex).RuleCount);
    LBridgeIndex := WfcPipelineProjectionBridgeForPass(ARecipe, LPassIndex);
    if LBridgeIndex >= 0 then
    begin
      P := ARecipe.PassAt(ARecipe.BridgeAt(LBridgeIndex).SourcePassIndex);
      case ARecipe.BridgeAt(LBridgeIndex).Kind of
        wpbkPattern2DProjection:
          LCount := ARecipe.BorrowPattern2DResource(P.ResourceIndex).PatternCount;
        wpbkSequenceProjection:
          LCount := ARecipe.BorrowSequenceResource(P.ResourceIndex).StateCount;
      else
        raise EWfcPipelineConnectivity.Create('unknown connectivity projection');
      end;
      AddModelWork(LCount);
    end;
  end;
  AFailedIndex := -1;
end;

function ValidateWfcPipelineConnectivity(const ARecipe: TWfcPipelineModel;
  const AConnectivityIndex, AWidth, AHeight, ADepth: Integer;
  const ATokens: TWfcModelTokens; out AFailedEntryIndex: Integer): Boolean;
var Q: TWfcPipelineConnectivity; P: TWfcPipelinePass;
  LLookup: TWfcTokenLookup; LModel: TWfcModel; LRules: TWfcRuleModel;
  LIndices, LProfileForValue, LQueue, LRuleRows: TIntegers;
  LReached, LRequired: TBytes;
  C, I, J, LRoot, LHead, LTail, LCell, LNext, LProfile, LOther: Integer;
  D, R: TGraphDirection;

  function Neighbor(const ACell: Integer; const ADirection: TGraphDirection): Integer;
  var X, Y, Z: Integer;
  begin
    X := ACell mod AWidth;
    Y := (ACell div AWidth) mod AHeight;
    Z := ACell div (AWidth * AHeight);
    { TGraph's geometric north increases Y; up increases Z. Singleton wrapped
      axes retain their actual reciprocal self-neighbors at every rank. }
    case ADirection of
      gdNorth: Inc(Y); gdEast: Inc(X); gdSouth: Dec(Y);
      gdWest: Dec(X); gdUp: Inc(Z); gdDown: Dec(Z);
    end;
    if ARecipe.WrapNeighbors then
    begin
      if X < 0 then X := AWidth - 1 else if X = AWidth then X := 0;
      if Y < 0 then Y := AHeight - 1 else if Y = AHeight then Y := 0;
      if Z < 0 then Z := ADepth - 1 else if Z = ADepth then Z := 0;
    end
    else if (X < 0) or (Y < 0) or (Z < 0) or
      (X >= AWidth) or (Y >= AHeight) or (Z >= ADepth) then Exit(-1);
    Result := (Z * AHeight + Y) * AWidth + X;
  end;

  function RuleAllows(const AOwner: Integer; const ADirection: TGraphDirection;
    const ATarget: Integer): Boolean;
  var LDirection: TWfcModelDirection; LRow, Lo, Hi, Mid, V: Integer;
  begin
    if Assigned(LModel) then
    begin
      { Exact inverse of wfc_model's adapter mapping: stored graph rule keys
        describe the owner's position relative to the candidate. }
      case ADirection of
        gdNorth: LDirection := wmdNorth;
        gdEast: LDirection := wmdWest;
        gdSouth: LDirection := wmdSouth;
        gdWest: LDirection := wmdEast;
        gdUp: LDirection := wmdDown;
        gdDown: LDirection := wmdUp;
      end;
      if not (LDirection in LModel.Directions) then Exit(True);
      Exit(LModel.RelationCount(LDirection, AOwner, ATarget) > 0);
    end;
    if not Assigned(LRules) then Exit(True); //neutral public projection
    LRow := LRuleRows[AOwner * 6 + Ord(ADirection)];
    if LRow < 0 then Exit(True);
    if LRules.RuleStateAt(LRow) = wrsDeny then Exit(False);
    Lo := 0; Hi := LRules.RuleTargetCountAt(LRow) - 1;
    while Lo <= Hi do
    begin
      Mid := Lo + (Hi - Lo) div 2; V := LRules.RuleTargetAt(LRow, Mid);
      if V = ATarget then Exit(True);
      if V < ATarget then Lo := Mid + 1 else Hi := Mid - 1;
    end;
    Result := False;
  end;
begin
  AFailedEntryIndex := -1;
  C := CellCount(ARecipe, AWidth, AHeight, ADepth);
  RequireInteger(AConnectivityIndex, 0, ARecipe.ConnectivityCount - 1,
    'connectivity descriptor index');
  Q := ARecipe.ConnectivityAt(AConnectivityIndex);
  ValidatePosition(Q.Root, AWidth, AHeight, ADepth);
  for I := 0 to Length(Q.RequiredPositions) - 1 do
    ValidatePosition(Q.RequiredPositions[I], AWidth, AHeight, ADepth);
  if Length(ATokens) <> C then
    raise EWfcPipelineConnectivity.Create('connectivity output shape mismatch');
  LLookup := TWfcTokenLookup.Create(ARecipe.CopyPublicVocabulary(Q.PassIndex));
  try
    P := ARecipe.PassAt(WfcPipelineMaterializedPublicPass(ARecipe, Q.PassIndex));
    LModel := nil; LRules := nil;
    case P.AdapterKind of
      wpakModel: LModel := ARecipe.BorrowModelResource(P.ResourceIndex);
      wpakRules: LRules := ARecipe.BorrowRuleResource(P.ResourceIndex);
      wpakEmpty:
        if WfcPipelineProjectionBridgeForPass(ARecipe,
          WfcPipelineMaterializedPublicPass(ARecipe, Q.PassIndex)) < 0 then
          raise EWfcPipelineConnectivity.Create('connectivity public adjacency is unknown');
    else
      raise EWfcPipelineConnectivity.Create('connectivity owner must be public');
    end;
    SetLength(LProfileForValue, LLookup.Count);
    for I := 0 to Length(LProfileForValue) - 1 do LProfileForValue[I] := -1;
    for I := 0 to Length(Q.Values) - 1 do
    begin
      J := LLookup.Find(Q.Values[I].Value);
      if J < 0 then raise EWfcPipelineConnectivity.Create('unknown connectivity public profile');
      LProfileForValue[J] := I;
    end;
    if Assigned(LRules) then
    begin
      SetLength(LRuleRows, LLookup.Count * 6);
      for I := 0 to Length(LRuleRows) - 1 do LRuleRows[I] := -1;
      for I := 0 to LRules.RuleCount - 1 do
        LRuleRows[LRules.RuleOwnerAt(I) * 6 + Ord(LRules.RuleDirectionAt(I))] := I;
    end;
    SetLength(LIndices, C); SetLength(LQueue, C);
    SetLength(LReached, C); SetLength(LRequired, C);
    for I := 0 to C - 1 do
    begin
      J := LLookup.Find(ATokens[I]);
      if J < 0 then begin AFailedEntryIndex := I; Exit(False); end;
      LIndices[I] := J; LProfile := LProfileForValue[J];
      if (LProfile >= 0) and (Q.RequireAllParticipants or
        Q.Values[LProfile].RequiredByValue) then LRequired[I] := 1;
    end;
    LRoot := PositionIndex(Q.Root, AWidth, AHeight); LRequired[LRoot] := 1;
    for I := 0 to Length(Q.RequiredPositions) - 1 do
      LRequired[PositionIndex(Q.RequiredPositions[I], AWidth, AHeight)] := 1;
    LHead := 0; LTail := 0;
    if LProfileForValue[LIndices[LRoot]] >= 0 then
    begin LQueue[0] := LRoot; LTail := 1; LReached[LRoot] := 1; end;
    while LHead < LTail do
    begin
      LCell := LQueue[LHead]; Inc(LHead);
      LProfile := LProfileForValue[LIndices[LCell]];
      for D := Low(TGraphDirection) to High(TGraphDirection) do
      begin
        if not (D in Q.Values[LProfile].Openings) then Continue;
        LNext := Neighbor(LCell, D);
        if (LNext < 0) or (LReached[LNext] <> 0) then Continue;
        LOther := LProfileForValue[LIndices[LNext]];
        if LOther < 0 then Continue;
        R := InverseOfDir(D);
        if (Neighbor(LNext, R) <> LCell) or
          not (R in Q.Values[LOther].Openings) then Continue;
        if not RuleAllows(LIndices[LNext], D, LIndices[LCell]) or
          not RuleAllows(LIndices[LCell], R, LIndices[LNext]) then Continue;
        LReached[LNext] := 1; LQueue[LTail] := LNext; Inc(LTail);
      end;
    end;
    for I := 0 to C - 1 do
      if (LRequired[I] <> 0) and (LReached[I] = 0) then
      begin AFailedEntryIndex := I; Exit(False); end;
    Result := True;
  finally LLookup.Free; end;
end;

end.
