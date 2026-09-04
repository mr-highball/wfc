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
unit wfc_sequence_graph;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_sequence;

const
  WFC_SEQUENCE_GRAPH_ADAPTER_VERSION = 1;

type
  EWfcSequenceGraph = class(EWfcSequence);

  TWfcSequenceGraphIssueKind = (
    wsgikNone,
    wsgikGraphShape,
    wsgikModelIdentity,
    wsgikEmptyCell,
    wsgikUnknownStateKey,
    wsgikStateIndex,
    wsgikBoundaryState,
    wsgikStartState,
    wsgikEndState,
    wsgikTransition
  );

  TWfcSequenceGraphIssue = record
    Kind: TWfcSequenceGraphIssueKind;
    Position: Integer;
    RelatedPosition: Integer;
    StateIndex: Integer;
    RelatedStateIndex: Integer;
  end;

  TWfcSequenceGraphValidationReport = record
    Valid: Boolean;
    CheckedStates: Integer;
    CheckedTransitions: Integer;
    Issue: TWfcSequenceGraphIssue;
  end;

  TWfcGeneratedSequence = record
    Boundary: TWfcModelBoundary;
    StateIndices: TWfcSequenceStateIndices;
    Tokens: TWfcModelTokens;
  end;

  { A complete public-token mapping from one latent sequence pass to another.
    TargetToken identifies one public token in the consumer model. Every
    SourceTokens item is an OR alternative projected by the provider model.
    Distinct source passes remain conjunctive under the graph pass contract. }
  TWfcSequenceProjectionRule = record
    TargetToken: TWfcModelToken;
    SourceTokens: TWfcModelTokens;
  end;
  TWfcSequenceProjectionRules = array of TWfcSequenceProjectionRule;

function MakeWfcSequenceProjectionRule(
  const ATargetToken: TWfcModelToken;
  const ASourceTokens: TWfcModelTokens): TWfcSequenceProjectionRule;

{ Applies the latent model to an already shaped, empty, one-dimensional graph
  pass. Open graphs receive observed start/end domains. Wrapped graphs receive
  BOS-free domains and a derived structural cycle; that is not wrapped
  training evidence. }
procedure ApplySequenceModelToGraph(const AModel: TWfcSequenceModel;
  const AGraph: TGraph);

{ Intersects one cell's latent domain with every state that projects to one of
  the supplied public tokens. Existing endpoint and caller domains survive. }
procedure IntersectSequenceAllowedTokens(
  const AModel: TWfcSequenceModel; const AGraph: TGraph;
  const APosition: Integer; const ATokens: TWfcModelTokens); overload;
procedure IntersectSequenceAllowedTokens(
  const AModel: TWfcSequenceModel; const AGraph: TGraph;
  const APosition: Integer; const AToken: TWfcModelToken); overload;

{ Makes every latent state require its projected public token from a named
  pass at the same coordinate. The provider pass therefore exposes public
  tokens while this pass keeps its context-bearing keys private. }
procedure RequireSequenceProjectionFromTokenPass(
  const AModel: TWfcSequenceModel; const AGraph: TGraph;
  const ASourcePass: String);

{ Lets one value in a downstream pass depend on the projected token of a
  latent sequence source pass. Every source state with an allowed emission is
  added as an OR alternative without exposing its private key to the caller. }
procedure RequireProjectedSequenceFromPass(
  const ASourceModel: TWfcSequenceModel; const ATargetGraph: TGraph;
  const ATargetValue: TGraphValue; const ASourcePass: String;
  const AAllowedPublicTokens: TWfcModelTokens);

{ Checks a complete projection-to-projection relation without adding it.
  Rules must cover every target public token exactly once; alternatives must
  be nonempty, known, and unique. Applied model identities and dependency
  acyclicity are also validated. }
procedure ValidateSequenceProjectionMapFromPass(
  const ATargetModel, ASourceModel: TWfcSequenceModel;
  const ATargetGraph: TGraph; const ASourcePass: String;
  const ARules: TWfcSequenceProjectionRules);

{ Validates, then adds the complete relation to the active target pass. }
procedure RequireSequenceProjectionMapFromPass(
  const ATargetModel, ASourceModel: TWfcSequenceModel;
  const ATargetGraph: TGraph; const ASourcePass: String;
  const ARules: TWfcSequenceProjectionRules);

function ValidateSequenceStatePath(const AModel: TWfcSequenceModel;
  const AStateIndices: TWfcSequenceStateIndices;
  const ABoundary: TWfcModelBoundary;
  out AReport: TWfcSequenceGraphValidationReport): Boolean;

function CaptureSolvedSequence(const AModel: TWfcSequenceModel;
  const AGraph: TGraph; out ASequence: TWfcGeneratedSequence;
  out AReport: TWfcSequenceGraphValidationReport): Boolean;

function DescribeSequenceGraphIssue(
  const AIssue: TWfcSequenceGraphIssue): String;

implementation

uses
  wfc_text_codec;

type
  TSequenceBooleanArray = array of Boolean;
  TSequenceByteArray = array of Byte;
  TSequenceGraphValueArrays = array of TGraphValues;
  TSequenceIntegerArray = array of Integer;

function MakeWfcSequenceProjectionRule(
  const ATargetToken: TWfcModelToken;
  const ASourceTokens: TWfcModelTokens): TWfcSequenceProjectionRule;
var
  I: Integer;
begin
  Result.TargetToken := ATargetToken;
  SetLength(Result.SourceTokens, Length(ASourceTokens));
  for I := 0 to Length(ASourceTokens) - 1 do
    Result.SourceTokens[I] := ASourceTokens[I];
end;

function AsciiToModelToken(const AText: String): TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(AText);
  {$ELSE}
  Result := UTF8Encode(UnicodeString(AText));
  {$ENDIF}
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

function StateIdentity(const AModel: TWfcSequenceModel;
  const AStateIndex: Integer): String;
var
  I: Integer;
  LHistory: TWfcSequenceHistoryItem;
  LTokenIndex: Integer;
begin
  Result := 'o' + IntToStr(AModel.Order);
  if AStateIndex = 0 then
  begin
    Result := Result + ':n' + IntToStr(AModel.SampleCount);
    for I := 0 to AModel.SampleCount - 1 do
      Result := Result + '.l' + IntToStr(AModel.SampleLengthAt(I));
  end;
  Result := Result + ':c' +
    IntToStr(AModel.StateObservationCountAt(AStateIndex)) + '.' +
    IntToStr(AModel.StartCountAt(AStateIndex)) + '.' +
    IntToStr(AModel.EndCountAt(AStateIndex)) + ':h';
  for I := 0 to AModel.HistorySize - 1 do
  begin
    LHistory := AModel.HistoryItemAt(AStateIndex, I);
    if LHistory.Kind = wshBos then
      Result := Result + '.b'
    else
      Result := Result + '.t' + IntToStr(LHistory.TokenIndex);
  end;
  LTokenIndex := AModel.StateEmittedTokenIndexAt(AStateIndex);
  Result := Result + ':e' + IntToStr(LTokenIndex) + '.' +
    WfcTextEncodeToken(AModel.PublicTokenAt(LTokenIndex),
      'sequence state key');
end;

function RawStateKey(const AModel: TWfcSequenceModel;
  const ASalt, AStateIndex: Integer): TWfcModelToken;
begin
  Result := AsciiToModelToken('@wfcs1:' + IntToStr(ASalt) + ':' +
    IntToStr(AStateIndex) + ':' + StateIdentity(AModel, AStateIndex));
end;

function TryParseRawStateKey(const AToken: TWfcModelToken;
  out ASalt, AStateIndex: Integer): Boolean;
const
  PREFIX = '@wfcs1:';
var
  I: Integer;

  function ParseCanonicalNumber(var APosition: Integer;
    out AValue: Integer): Boolean;
  var
    LDigit: Integer;
    LStart: Integer;
  begin
    AValue := 0;
    LStart := APosition;
    while (APosition <= Length(AToken)) and
        (AToken[APosition] >= '0') and (AToken[APosition] <= '9') do
    begin
      LDigit := Ord(AToken[APosition]) - Ord('0');
      if AValue > (High(Integer) - LDigit) div 10 then
        Exit(False);
      AValue := AValue * 10 + LDigit;
      Inc(APosition);
    end;
    Result := (APosition > LStart) and
      ((APosition - LStart = 1) or (AToken[LStart] <> '0'));
  end;

begin
  Result := False;
  ASalt := -1;
  AStateIndex := -1;
  if Length(AToken) <= Length(PREFIX) + 2 then
    Exit;
  for I := 1 to Length(PREFIX) do
    if AToken[I] <> PREFIX[I] then
      Exit;
  I := Length(PREFIX) + 1;
  if not ParseCanonicalNumber(I, ASalt) then
    Exit;
  if (I > Length(AToken)) or (AToken[I] <> ':') then
    Exit;
  Inc(I);
  if not ParseCanonicalNumber(I, AStateIndex) then
    Exit;
  Result := (I <= Length(AToken)) and (AToken[I] = ':');
end;

function SequenceKeySalt(const AModel: TWfcSequenceModel): Integer;
var
  I: Integer;
  LSalt: Integer;
  LState: Integer;
  LUsed: array of Boolean;
begin
  SetLength(LUsed, AModel.PublicTokenCount + 1);
  for I := 0 to AModel.PublicTokenCount - 1 do
    if TryParseRawStateKey(AModel.PublicTokenAt(I), LSalt, LState) and
        (LSalt >= 0) and (LSalt <= AModel.PublicTokenCount) and
        (LState >= 0) and (LState < AModel.StateCount) then
      LUsed[LSalt] := True;
  for Result := 0 to AModel.PublicTokenCount do
    if not LUsed[Result] then
      Exit;
  raise EWfcSequenceGraph.Create(
    'sequence state-key salt search failed');
end;

procedure RequireAssigned(const AModel: TWfcSequenceModel;
  const AGraph: TGraph);
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create('sequence model cannot be nil');
  if not Assigned(AGraph) then
    raise EArgumentNilException.Create('sequence graph cannot be nil');
end;

procedure ValidateGraphShape(const AGraph: TGraph;
  const AOperation: String);
begin
  if (AGraph.Dimension.Width = 0) or
      (AGraph.Dimension.Width > TGraphCoordinate(High(Integer))) or
      (AGraph.Dimension.Height <> 1) or
      (AGraph.Dimension.Depth <> 1) then
    raise EWfcSequenceGraph.Create(AOperation +
      ' requires a positive one-dimensional graph');
end;

function SaltedStateGraphValue(const ASalt,
  AStateIndex: Integer; const AModel: TWfcSequenceModel): TGraphValue;
begin
  Result := ModelTokenToGraphValue(
    RawStateKey(AModel, ASalt, AStateIndex));
end;

function CopyStateKeys(const AModel: TWfcSequenceModel): TWfcModelTokens;
var
  I: Integer;
  LSalt: Integer;
begin
  Result := nil;
  LSalt := SequenceKeySalt(AModel);
  SetLength(Result, AModel.StateCount);
  for I := 0 to AModel.StateCount - 1 do
    Result[I] := RawStateKey(AModel, LSalt, I);
end;

function StateAllowedInDirection(const AModel: TWfcSequenceModel;
  const ASourceState, ATargetState: Integer;
  const ADirection: TGraphDirection): Boolean;
begin
  case ADirection of
    gdWest:
      Result := AModel.StatesCompatible(ASourceState, ATargetState);
    gdEast:
      Result := AModel.StatesCompatible(ATargetState, ASourceState);
  else
    Result := False;
  end;
end;

function StateRulesMatch(const AModel: TWfcSequenceModel;
  const AGroup: TGraphRuleGroup; const AStateIndex,
  ASalt: Integer): Boolean;
var
  D: TGraphDirection;
  I: Integer;
  LExpectedRuleCount: Integer;
  LExpectedTargetCount: Integer;
  LRule: TGraphRule;
  LTargetIndex: Integer;
begin
  Result := False;
  LExpectedRuleCount := 0;
  for D := Low(TGraphDirection) to High(TGraphDirection) do
  begin
    if D in [gdEast, gdWest] then
    begin
      LExpectedTargetCount := 0;
      for I := 0 to AModel.StateCount - 1 do
        if StateAllowedInDirection(AModel, AStateIndex, I, D) then
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
        if LRule.Info or (Length(LRule.Value) <> LExpectedTargetCount) then
          Exit;
        LTargetIndex := 0;
        for I := 0 to AModel.StateCount - 1 do
          if StateAllowedInDirection(AModel, AStateIndex, I, D) then
          begin
            if LRule.Value[LTargetIndex] <>
                SaltedStateGraphValue(ASalt, I, AModel) then
              Exit;
            Inc(LTargetIndex);
          end;
      end;
    end
    else if AGroup.Exists[D] or AGroup.Denied[D] then
      Exit;
  end;
  Result := Length(AGroup.Rules) = LExpectedRuleCount;
end;

function AppliedModelMatches(const AModel: TWfcSequenceModel;
  const AGraph: TGraph): Boolean;
var
  I: Integer;
  LActivePass: TGraph;
  LGroup: TGraphRuleGroup;
  LParentedGroup: TGraph.TParentedGraphRuleGroup;
  LSalt: Integer;
  LValue: TGraphValue;
begin
  Result := False;
  if AGraph.RuleGroups.Count <> AModel.StateCount then
    Exit;
  LActivePass := AGraph.PassGraph[AGraph.CurrentPassIndex];
  LSalt := SequenceKeySalt(AModel);
  for I := 0 to AModel.StateCount - 1 do
  begin
    LValue := SaltedStateGraphValue(LSalt, I, AModel);
    if (not AGraph.RuleGroups.TryGetValue(LValue, LGroup)) or
        (not Assigned(LGroup)) or (LGroup.Value <> LValue) or
        (not (LGroup is TGraph.TParentedGraphRuleGroup)) then
      Exit;
    LParentedGroup := TGraph.TParentedGraphRuleGroup(LGroup);
    if (LParentedGroup.Parent <> LActivePass) or
        (LGroup.Weight <>
          AModel.StateObservationCountAt(I)) or
        (not StateRulesMatch(AModel, LGroup, I, LSalt)) then
      Exit;
  end;
  Result := True;
end;

procedure ValidateAppliedModel(const AModel: TWfcSequenceModel;
  const AGraph: TGraph; const AOperation: String);
begin
  if not AppliedModelMatches(AModel, AGraph) then
    raise EWfcSequenceGraph.Create(AOperation +
      ' requires the matching sequence model on the graph pass');
end;

function ContainsGraphValue(const AValues: TGraphValues;
  const AValue: TGraphValue): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(AValues) - 1 do
    if AValues[I] = AValue then
      Exit(True);
  Result := False;
end;

function CopyProjectedGraphValues(const AModel: TWfcSequenceModel;
  const AOperation: String): TGraphValues;
var
  I: Integer;
  J: Integer;
  LToken: TWfcModelToken;
begin
  Result := nil;
  SetLength(Result, AModel.PublicTokenCount);
  for I := 0 to AModel.PublicTokenCount - 1 do
  begin
    LToken := AModel.PublicTokenAt(I);
    Result[I] := ModelTokenToGraphValue(LToken);
    if (Length(Result[I]) = 0) or
        (GraphValueToModelToken(Result[I]) <> LToken) then
      raise EWfcSequenceGraph.CreateFmt(
        '%s cannot represent public token %d in graph strings',
        [AOperation, I]);
    for J := 0 to I - 1 do
      if Result[I] = Result[J] then
        raise EWfcSequenceGraph.CreateFmt(
          '%s public-token conversion is not unique [%d, %d]',
          [AOperation, J, I]);
  end;
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
  raise EWfcSequenceGraph.CreateFmt(
    '%s cannot find source pass "%s"', [AOperation, APass]);
end;

procedure IntersectGraphDomain(const AGraph: TGraph;
  const APosition: Integer; const ACandidates: TGraphValues);
var
  I: Integer;
  LExisting: TGraphValues;
  LIntersection: TGraphValues;
begin
  if not AGraph.HasAllowedValues(TGraphCoordinate(APosition), 0, 0) then
  begin
    AGraph.SetAllowedValues(TGraphCoordinate(APosition), 0, 0,
      ACandidates);
    Exit;
  end;

  LExisting := AGraph.CopyAllowedValues(
    TGraphCoordinate(APosition), 0, 0);
  SetLength(LIntersection, 0);
  for I := 0 to Length(ACandidates) - 1 do
    if ContainsGraphValue(LExisting, ACandidates[I]) then
    begin
      SetLength(LIntersection, Length(LIntersection) + 1);
      LIntersection[High(LIntersection)] := ACandidates[I];
    end;
  AGraph.SetAllowedValues(TGraphCoordinate(APosition), 0, 0,
    LIntersection);
end;

procedure ApplyEndpointDomains(const AModel: TWfcSequenceModel;
  const AGraph: TGraph);
var
  I: Integer;
  LEndValues: TGraphValues;
  LSalt: Integer;
  LStartValues: TGraphValues;
  LWidth: Integer;
begin
  LWidth := Integer(AGraph.Dimension.Width);
  LSalt := SequenceKeySalt(AModel);
  SetLength(LStartValues, 0);
  SetLength(LEndValues, 0);
  for I := 0 to AModel.StateCount - 1 do
  begin
    if (AModel.StartCountAt(I) > 0) and
        ((LWidth > 1) or (AModel.EndCountAt(I) > 0)) then
    begin
      SetLength(LStartValues, Length(LStartValues) + 1);
      LStartValues[High(LStartValues)] :=
        SaltedStateGraphValue(LSalt, I, AModel);
    end;
    if (LWidth > 1) and (AModel.EndCountAt(I) > 0) then
    begin
      SetLength(LEndValues, Length(LEndValues) + 1);
      LEndValues[High(LEndValues)] :=
        SaltedStateGraphValue(LSalt, I, AModel);
    end;
  end;
  IntersectGraphDomain(AGraph, 0, LStartValues);
  if LWidth > 1 then
    IntersectGraphDomain(AGraph, LWidth - 1, LEndValues);
end;

function StateHasBos(const AModel: TWfcSequenceModel;
  const AStateIndex: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to AModel.HistorySize - 1 do
    if AModel.HistoryItemAt(AStateIndex, I).Kind = wshBos then
      Exit(True);
  Result := False;
end;

procedure ApplyWrappedDomains(const AModel: TWfcSequenceModel;
  const AGraph: TGraph);
var
  I: Integer;
  LAllowed: TGraphValues;
  LPosition: Integer;
  LSalt: Integer;
begin
  LSalt := SequenceKeySalt(AModel);
  SetLength(LAllowed, 0);
  for I := 0 to AModel.StateCount - 1 do
    if not StateHasBos(AModel, I) then
    begin
      SetLength(LAllowed, Length(LAllowed) + 1);
      LAllowed[High(LAllowed)] :=
        SaltedStateGraphValue(LSalt, I, AModel);
    end;
  for LPosition := 0 to Integer(AGraph.Dimension.Width) - 1 do
    IntersectGraphDomain(AGraph, LPosition, LAllowed);
end;

procedure ApplySequenceModelToGraph(const AModel: TWfcSequenceModel;
  const AGraph: TGraph);
var
  LGraphModel: TWfcModel;
  LKeys: TWfcModelTokens;
begin
  RequireAssigned(AModel, AGraph);
  if AGraph.Running then
    raise EWfcSequenceGraph.Create(
      'sequence model cannot be applied while the pipeline is running');
  ValidateGraphShape(AGraph, 'sequence model application');
  LKeys := CopyStateKeys(AModel);
  LGraphModel := AModel.CreateGraphModel(LKeys);
  try
    ApplyModelToGraph(LGraphModel, AGraph);
  finally
    LGraphModel.Free;
  end;
  if AGraph.WrapNeighbors then
    ApplyWrappedDomains(AModel, AGraph)
  else
    ApplyEndpointDomains(AModel, AGraph);
end;

procedure IntersectSequenceAllowedTokens(
  const AModel: TWfcSequenceModel; const AGraph: TGraph;
  const APosition: Integer; const ATokens: TWfcModelTokens);
var
  I: Integer;
  J: Integer;
  LAllowed: TGraphValues;
  LSalt: Integer;
  LTokenIndices: array of Integer;
begin
  RequireAssigned(AModel, AGraph);
  if AGraph.Running then
    raise EWfcSequenceGraph.Create(
      'sequence token domains cannot change while the pipeline is running');
  ValidateGraphShape(AGraph, 'sequence token-domain intersection');
  ValidateAppliedModel(AModel, AGraph,
    'sequence token-domain intersection');
  if (APosition < 0) or
      (APosition >= Integer(AGraph.Dimension.Width)) then
    raise ERangeError.CreateFmt(
      'sequence position is out of bounds [%d]', [APosition]);
  LSalt := SequenceKeySalt(AModel);

  SetLength(LTokenIndices, Length(ATokens));
  for I := 0 to Length(ATokens) - 1 do
  begin
    LTokenIndices[I] := AModel.FindPublicToken(ATokens[I]);
    if LTokenIndices[I] < 0 then
      raise EArgumentException.CreateFmt(
        'unknown sequence public token [%d]', [I]);
  end;

  SetLength(LAllowed, 0);
  for I := 0 to AModel.StateCount - 1 do
    for J := 0 to Length(LTokenIndices) - 1 do
      if AModel.StateEmittedTokenIndexAt(I) = LTokenIndices[J] then
      begin
        SetLength(LAllowed, Length(LAllowed) + 1);
        LAllowed[High(LAllowed)] :=
          SaltedStateGraphValue(LSalt, I, AModel);
        Break;
      end;
  IntersectGraphDomain(AGraph, APosition, LAllowed);
end;

procedure IntersectSequenceAllowedTokens(
  const AModel: TWfcSequenceModel; const AGraph: TGraph;
  const APosition: Integer; const AToken: TWfcModelToken);
var
  LTokens: TWfcModelTokens;
begin
  SetLength(LTokens, 1);
  LTokens[0] := AToken;
  IntersectSequenceAllowedTokens(AModel, AGraph, APosition, LTokens);
end;

procedure RequireSequenceProjectionFromTokenPass(
  const AModel: TWfcSequenceModel; const AGraph: TGraph;
  const ASourcePass: String);
var
  I: Integer;
  LActivePass: TGraph;
  LPublicValues: TGraphValues;
  LProjectedValues: TGraphValues;
  LSalt: Integer;
  LSourceGraph: TGraph;
begin
  RequireAssigned(AModel, AGraph);
  if AGraph.Running then
    raise EWfcSequenceGraph.Create(
      'sequence pass projection cannot change while the pipeline is running');
  ValidateGraphShape(AGraph, 'sequence pass projection');
  ValidateAppliedModel(AModel, AGraph, 'sequence pass projection');
  LSourceGraph := FindPassGraph(AGraph, ASourcePass,
    'sequence pass projection');
  LActivePass := AGraph.PassGraph[AGraph.CurrentPassIndex];
  if LSourceGraph = LActivePass then
    raise EWfcSequenceGraph.Create(
      'sequence pass projection cannot depend on its own pass');
  LPublicValues := CopyProjectedGraphValues(AModel,
    'sequence pass projection');
  LSalt := SequenceKeySalt(AModel);

  SetLength(LProjectedValues, AModel.StateCount);
  for I := 0 to AModel.StateCount - 1 do
    LProjectedValues[I] := LPublicValues[
      AModel.StateEmittedTokenIndexAt(I)];
  for I := 0 to AModel.StateCount - 1 do
    AGraph.Rules[SaltedStateGraphValue(LSalt, I, AModel)].RequireFromPass(
      ASourcePass, LProjectedValues[I]);
end;

procedure RequireProjectedSequenceFromPass(
  const ASourceModel: TWfcSequenceModel; const ATargetGraph: TGraph;
  const ATargetValue: TGraphValue; const ASourcePass: String;
  const AAllowedPublicTokens: TWfcModelTokens);
var
  I: Integer;
  J: Integer;
  LAllowedTokenIndices: array of Integer;
  LGroup: TGraphRuleGroup;
  LParentedGroup: TGraph.TParentedGraphRuleGroup;
  LSalt: Integer;
  LSourceGraph: TGraph;
  LSourceValues: TGraphValues;
  LTargetPass: TGraph;
begin
  RequireAssigned(ASourceModel, ATargetGraph);
  if ATargetGraph.Running then
    raise EWfcSequenceGraph.Create(
      'projected sequence requirements cannot change while the pipeline is running');
  LSourceGraph := FindPassGraph(ATargetGraph, ASourcePass,
    'projected sequence requirements');
  ValidateGraphShape(LSourceGraph, 'projected sequence requirements');
  ValidateAppliedModel(ASourceModel, LSourceGraph,
    'projected sequence requirements');
  LTargetPass := ATargetGraph.PassGraph[
    ATargetGraph.CurrentPassIndex];
  if (not ATargetGraph.RuleGroups.TryGetValue(ATargetValue, LGroup)) or
      (not Assigned(LGroup)) or (LGroup.Value <> ATargetValue) or
      (not (LGroup is TGraph.TParentedGraphRuleGroup)) then
    raise EArgumentException.CreateFmt(
      'unknown target graph value "%s"', [ATargetValue]);
  LParentedGroup := TGraph.TParentedGraphRuleGroup(LGroup);
  if LParentedGroup.Parent <> LTargetPass then
    raise EWfcSequenceGraph.Create(
      'projected sequence target value is owned by another pass');
  if Length(AAllowedPublicTokens) = 0 then
    raise EArgumentException.Create(
      'projected sequence requirements need at least one public token');

  SetLength(LAllowedTokenIndices, Length(AAllowedPublicTokens));
  for I := 0 to Length(AAllowedPublicTokens) - 1 do
  begin
    LAllowedTokenIndices[I] :=
      ASourceModel.FindPublicToken(AAllowedPublicTokens[I]);
    if LAllowedTokenIndices[I] < 0 then
      raise EArgumentException.CreateFmt(
        'unknown sequence public token [%d]', [I]);
  end;

  SetLength(LSourceValues, 0);
  LSalt := SequenceKeySalt(ASourceModel);
  for I := 0 to ASourceModel.StateCount - 1 do
    for J := 0 to Length(LAllowedTokenIndices) - 1 do
      if ASourceModel.StateEmittedTokenIndexAt(I) =
          LAllowedTokenIndices[J] then
      begin
        SetLength(LSourceValues, Length(LSourceValues) + 1);
        LSourceValues[High(LSourceValues)] :=
          SaltedStateGraphValue(LSalt, I, ASourceModel);
        Break;
      end;
  LGroup.RequireFromPass(ASourcePass, LSourceValues);
end;

procedure ValidateProjectionDependencyEdge(const ATargetGraph,
  ASourceGraph: TGraph; const ASourcePass: String);
var
  I: Integer;
  LDependencyIndex: Integer;
  LNodeGraph: TGraph;
  LNodeIndex: Integer;
  LPassCount: Integer;
  LSeen: TSequenceByteArray;
  LSourceIndex: Integer;
  LStack: TSequenceIntegerArray;
  LStackCount: Integer;
  LTargetIndex: Integer;
begin
  LPassCount := ATargetGraph.TotalPassCount;
  LTargetIndex := ATargetGraph.CurrentPassIndex;
  LSourceIndex := ASourceGraph.CurrentPassIndex;
  if (LTargetIndex < 0) or (LTargetIndex >= LPassCount) or
      (LSourceIndex < 0) or (LSourceIndex >= LPassCount) then
    raise EWfcSequenceGraph.Create(
      'sequence projection map has an invalid pass index');
  if LSourceIndex = LTargetIndex then
    raise EWfcSequenceGraph.Create(
      'sequence projection map cannot depend on its own pass');

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
      raise EWfcSequenceGraph.CreateFmt(
        'sequence projection dependency on pass "%s" would create a cycle',
        [ASourcePass]);
    LNodeGraph := ATargetGraph.PassGraph[LNodeIndex];
    for I := 0 to LNodeGraph.DependencyCount - 1 do
    begin
      LDependencyIndex := LNodeGraph.DependencyIndex[I];
      if (LDependencyIndex < 0) or
          (LDependencyIndex >= LPassCount) then
        raise EWfcSequenceGraph.Create(
          'sequence projection map found a malformed dependency graph');
      if LSeen[LDependencyIndex] = 0 then
      begin
        if LStackCount >= Length(LStack) then
          raise EWfcSequenceGraph.Create(
            'sequence projection map found a malformed dependency graph');
        LStack[LStackCount] := LDependencyIndex;
        Inc(LStackCount);
        LSeen[LDependencyIndex] := 1;
      end;
    end;
    LSeen[LNodeIndex] := 2;
  end;
end;

procedure PrepareSequenceProjectionMap(
  const ATargetModel, ASourceModel: TWfcSequenceModel;
  const ATargetGraph: TGraph; const ASourcePass: String;
  const ARules: TWfcSequenceProjectionRules;
  out ATargetSalt: Integer;
  out ASourceValuesByTarget: TSequenceGraphValueArrays);
var
  I: Integer;
  J: Integer;
  K: Integer;
  LAllowedSourceIndices: TSequenceIntegerArray;
  LSeenTargets: TSequenceBooleanArray;
  LSourceGraph: TGraph;
  LSourceSalt: Integer;
  LSourceTokenIndex: Integer;
  LTargetTokenIndex: Integer;
begin
  ATargetSalt := 0;
  ASourceValuesByTarget := nil;
  RequireAssigned(ATargetModel, ATargetGraph);
  RequireAssigned(ASourceModel, ATargetGraph);
  if ATargetGraph.Running then
    raise EWfcSequenceGraph.Create(
      'sequence projection maps cannot change while the pipeline is running');

  ValidateGraphShape(ATargetGraph, 'sequence projection map');
  ValidateAppliedModel(ATargetModel, ATargetGraph,
    'sequence projection map target');
  LSourceGraph := FindPassGraph(ATargetGraph, ASourcePass,
    'sequence projection map');
  ValidateGraphShape(LSourceGraph, 'sequence projection map source');
  ValidateAppliedModel(ASourceModel, LSourceGraph,
    'sequence projection map source');
  ValidateProjectionDependencyEdge(ATargetGraph,
    LSourceGraph, ASourcePass);

  if Length(ARules) <> ATargetModel.PublicTokenCount then
    raise EArgumentException.CreateFmt(
      'sequence projection map must cover %d target public tokens',
      [ATargetModel.PublicTokenCount]);

  SetLength(LSeenTargets, ATargetModel.PublicTokenCount);
  SetLength(ASourceValuesByTarget, ATargetModel.PublicTokenCount);
  LSourceSalt := SequenceKeySalt(ASourceModel);
  for I := 0 to Length(ARules) - 1 do
  begin
    LTargetTokenIndex :=
      ATargetModel.FindPublicToken(ARules[I].TargetToken);
    if LTargetTokenIndex < 0 then
      raise EArgumentException.CreateFmt(
        'unknown target sequence public token in projection rule %d', [I]);
    if LSeenTargets[LTargetTokenIndex] then
      raise EArgumentException.CreateFmt(
        'duplicate target sequence public token in projection rule %d', [I]);
    LSeenTargets[LTargetTokenIndex] := True;

    if Length(ARules[I].SourceTokens) = 0 then
      raise EArgumentException.CreateFmt(
        'sequence projection rule %d needs a source alternative', [I]);
    SetLength(LAllowedSourceIndices,
      Length(ARules[I].SourceTokens));
    for J := 0 to Length(ARules[I].SourceTokens) - 1 do
    begin
      LSourceTokenIndex :=
        ASourceModel.FindPublicToken(ARules[I].SourceTokens[J]);
      if LSourceTokenIndex < 0 then
        raise EArgumentException.CreateFmt(
          'unknown source sequence public token in projection rule %d alternative %d',
          [I, J]);
      for K := 0 to J - 1 do
        if LAllowedSourceIndices[K] = LSourceTokenIndex then
          raise EArgumentException.CreateFmt(
            'duplicate source sequence public token in projection rule %d alternative %d',
            [I, J]);
      LAllowedSourceIndices[J] := LSourceTokenIndex;
    end;

    SetLength(ASourceValuesByTarget[LTargetTokenIndex], 0);
    for J := 0 to ASourceModel.StateCount - 1 do
      for K := 0 to Length(LAllowedSourceIndices) - 1 do
        if ASourceModel.StateEmittedTokenIndexAt(J) =
            LAllowedSourceIndices[K] then
        begin
          SetLength(ASourceValuesByTarget[LTargetTokenIndex],
            Length(ASourceValuesByTarget[LTargetTokenIndex]) + 1);
          ASourceValuesByTarget[LTargetTokenIndex][
            High(ASourceValuesByTarget[LTargetTokenIndex])] :=
              SaltedStateGraphValue(LSourceSalt, J, ASourceModel);
          Break;
        end;
  end;

  for I := 0 to ATargetModel.PublicTokenCount - 1 do
    if not LSeenTargets[I] then
      raise EArgumentException.CreateFmt(
        'sequence projection map is missing target public token %d', [I]);

  ATargetSalt := SequenceKeySalt(ATargetModel);
end;

procedure ValidateSequenceProjectionMapFromPass(
  const ATargetModel, ASourceModel: TWfcSequenceModel;
  const ATargetGraph: TGraph; const ASourcePass: String;
  const ARules: TWfcSequenceProjectionRules);
var
  LSourceValuesByTarget: TSequenceGraphValueArrays;
  LTargetSalt: Integer;
begin
  PrepareSequenceProjectionMap(ATargetModel, ASourceModel,
    ATargetGraph, ASourcePass, ARules, LTargetSalt,
    LSourceValuesByTarget);
end;

procedure RequireSequenceProjectionMapFromPass(
  const ATargetModel, ASourceModel: TWfcSequenceModel;
  const ATargetGraph: TGraph; const ASourcePass: String;
  const ARules: TWfcSequenceProjectionRules);
var
  I: Integer;
  LGroup: TGraphRuleGroup;
  LSourceValuesByTarget: TSequenceGraphValueArrays;
  LTargetSalt: Integer;
  LTargetTokenIndex: Integer;
begin
  PrepareSequenceProjectionMap(ATargetModel, ASourceModel,
    ATargetGraph, ASourcePass, ARules, LTargetSalt,
    LSourceValuesByTarget);

  { Preparation checks the dependency edge, applied identities, groups, and
    values before the first requirement is added. }
  for I := 0 to ATargetModel.StateCount - 1 do
  begin
    LTargetTokenIndex :=
      ATargetModel.StateEmittedTokenIndexAt(I);
    LGroup := ATargetGraph.Rules[
      SaltedStateGraphValue(LTargetSalt, I, ATargetModel)];
    LGroup.RequireFromPass(ASourcePass,
      LSourceValuesByTarget[LTargetTokenIndex]);
  end;
end;

procedure InitializeReport(
  out AReport: TWfcSequenceGraphValidationReport);
begin
  AReport := Default(TWfcSequenceGraphValidationReport);
  AReport.Issue.Position := -1;
  AReport.Issue.RelatedPosition := -1;
  AReport.Issue.StateIndex := -1;
  AReport.Issue.RelatedStateIndex := -1;
end;

function InvalidReport(var AReport: TWfcSequenceGraphValidationReport;
  const AKind: TWfcSequenceGraphIssueKind): Boolean;
begin
  AReport.Valid := False;
  AReport.Issue.Kind := AKind;
  Result := False;
end;

procedure CheckedReportIncrement(var AValue: Integer);
begin
  if AValue < High(Integer) then
    Inc(AValue);
end;

function ValidateSequenceStatePath(const AModel: TWfcSequenceModel;
  const AStateIndices: TWfcSequenceStateIndices;
  const ABoundary: TWfcModelBoundary;
  out AReport: TWfcSequenceGraphValidationReport): Boolean;
var
  I: Integer;
  LBoundaryOrdinal: Integer;
  LCount: Integer;
  LLength: SizeInt;
  LNextPosition: Integer;
  LState: Integer;
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create('sequence model cannot be nil');
  InitializeReport(AReport);
  LLength := Length(AStateIndices);
  if (LLength < 1) or
      ((LLength and (not SizeInt(High(Integer)))) <> 0) then
    Exit(InvalidReport(AReport, wsgikGraphShape));
  LCount := Integer(LLength);
  LBoundaryOrdinal := Ord(ABoundary);
  if (LBoundaryOrdinal <> Ord(wmbOpen)) and
      (LBoundaryOrdinal <> Ord(wmbWrap)) then
    Exit(InvalidReport(AReport, wsgikGraphShape));

  for I := 0 to LCount - 1 do
  begin
    LState := AStateIndices[I];
    if (LState < 0) or (LState >= AModel.StateCount) then
    begin
      AReport.Issue.Position := I;
      AReport.Issue.StateIndex := LState;
      Exit(InvalidReport(AReport, wsgikStateIndex));
    end;
    CheckedReportIncrement(AReport.CheckedStates);
    if (ABoundary = wmbWrap) and StateHasBos(AModel, LState) then
    begin
      AReport.Issue.Position := I;
      AReport.Issue.StateIndex := LState;
      Exit(InvalidReport(AReport, wsgikBoundaryState));
    end;
  end;

  if (ABoundary = wmbOpen) and
      (AModel.StartCountAt(AStateIndices[0]) < 1) then
  begin
    AReport.Issue.Position := 0;
    AReport.Issue.StateIndex := AStateIndices[0];
    Exit(InvalidReport(AReport, wsgikStartState));
  end;
  for I := 0 to LCount - 1 do
  begin
    LNextPosition := I + 1;
    if LNextPosition = LCount then
    begin
      if ABoundary = wmbOpen then
        Break;
      LNextPosition := 0;
    end;
    CheckedReportIncrement(AReport.CheckedTransitions);
    if not AModel.StatesCompatible(AStateIndices[I],
        AStateIndices[LNextPosition]) then
    begin
      AReport.Issue.Position := I;
      AReport.Issue.RelatedPosition := LNextPosition;
      AReport.Issue.StateIndex := AStateIndices[I];
      AReport.Issue.RelatedStateIndex :=
        AStateIndices[LNextPosition];
      Exit(InvalidReport(AReport, wsgikTransition));
    end;
  end;
  if (ABoundary = wmbOpen) and
      (AModel.EndCountAt(AStateIndices[LCount - 1]) < 1) then
  begin
    AReport.Issue.Position := LCount - 1;
    AReport.Issue.StateIndex := AStateIndices[LCount - 1];
    Exit(InvalidReport(AReport, wsgikEndState));
  end;
  AReport.Valid := True;
  AReport.Issue.Kind := wsgikNone;
  Result := True;
end;

function FindStateValue(const AModel: TWfcSequenceModel;
  const AValue: TGraphValue): Integer;
var
  LSalt: Integer;
begin
  LSalt := SequenceKeySalt(AModel);
  for Result := 0 to AModel.StateCount - 1 do
    if SaltedStateGraphValue(LSalt, Result, AModel) = AValue then
      Exit;
  Result := -1;
end;

function CaptureSolvedSequence(const AModel: TWfcSequenceModel;
  const AGraph: TGraph; out ASequence: TWfcGeneratedSequence;
  out AReport: TWfcSequenceGraphValidationReport): Boolean;
var
  I: Integer;
  LEntry: TGraphEntry;
  LSequence: TWfcGeneratedSequence;
  LState: Integer;
  LWidth: Integer;
begin
  RequireAssigned(AModel, AGraph);
  ASequence := Default(TWfcGeneratedSequence);
  LSequence := Default(TWfcGeneratedSequence);
  InitializeReport(AReport);
  if (AGraph.Dimension.Width = 0) or
      (AGraph.Dimension.Width > TGraphCoordinate(High(Integer))) or
      (AGraph.Dimension.Height <> 1) or
      (AGraph.Dimension.Depth <> 1) then
    Exit(InvalidReport(AReport, wsgikGraphShape));
  if not AppliedModelMatches(AModel, AGraph) then
    Exit(InvalidReport(AReport, wsgikModelIdentity));

  LWidth := Integer(AGraph.Dimension.Width);
  if AGraph.WrapNeighbors then
    LSequence.Boundary := wmbWrap
  else
    LSequence.Boundary := wmbOpen;
  SetLength(LSequence.StateIndices, LWidth);
  for I := 0 to LWidth - 1 do
  begin
    LEntry := AGraph.Entry[TGraphCoordinate(I), 0, 0];
    if LEntry.Empty then
    begin
      AReport.Issue.Position := I;
      Exit(InvalidReport(AReport, wsgikEmptyCell));
    end;
    LState := FindStateValue(AModel, LEntry.Value);
    if LState < 0 then
    begin
      AReport.Issue.Position := I;
      Exit(InvalidReport(AReport, wsgikUnknownStateKey));
    end;
    LSequence.StateIndices[I] := LState;
  end;
  if not ValidateSequenceStatePath(AModel, LSequence.StateIndices,
      LSequence.Boundary, AReport) then
    Exit(False);
  LSequence.Tokens := AModel.ProjectStateIndices(LSequence.StateIndices);
  ASequence := LSequence;
  Result := True;
end;

function DescribeSequenceGraphIssue(
  const AIssue: TWfcSequenceGraphIssue): String;
begin
  Result := 'unknown sequence graph issue';
  case AIssue.Kind of
    wsgikNone:
      Result := 'no sequence graph issue';
    wsgikGraphShape:
      Result := 'sequence graph shape or boundary is invalid';
    wsgikModelIdentity:
      Result := 'graph pass does not contain the matching sequence model';
    wsgikEmptyCell:
      Result := Format('sequence position %d is empty', [AIssue.Position]);
    wsgikUnknownStateKey:
      Result := Format('sequence position %d has an unknown latent key',
        [AIssue.Position]);
    wsgikStateIndex:
      Result := Format('sequence position %d has invalid state %d',
        [AIssue.Position, AIssue.StateIndex]);
    wsgikBoundaryState:
      Result := Format(
        'wrapped sequence position %d contains a BOS-bearing state',
        [AIssue.Position]);
    wsgikStartState:
      Result := Format('sequence state %d was not observed at a start',
        [AIssue.StateIndex]);
    wsgikEndState:
      Result := Format('sequence state %d was not observed at an end',
        [AIssue.StateIndex]);
    wsgikTransition:
      Result := Format('sequence states %d and %d do not overlap',
        [AIssue.StateIndex, AIssue.RelatedStateIndex]);
  end;
end;

end.
