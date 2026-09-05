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
unit wfc_sequence_analyze;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc_model,
  wfc_sequence;

const
  WFC_SEQUENCE_ANALYZE_VERSION = 1;

type
  TWfcSequenceAnalyzeIssueKind = (
    wsaikNone,
    wsaikEmptyDomain,
    wsaikNoPath,
    wsaikNoCycle
  );

  TWfcSequenceAnalyzeIssue = record
    Kind: TWfcSequenceAnalyzeIssueKind;
    Position: Integer;
    RelatedPosition: Integer;
  end;

  TWfcSequencePositionDomain = record
    StateIndices: TWfcSequenceStateIndices;
    Tokens: TWfcModelTokens;
    Weights: TWfcModelIntegerArray;
  end;
  TWfcSequencePositionDomains = array of TWfcSequencePositionDomain;

  TWfcSequenceDomainAnalysis = record
    Extent: TWfcSequenceExtent;
    Satisfiable: Boolean;
    Positions: TWfcSequencePositionDomains;
    Issue: TWfcSequenceAnalyzeIssue;
  end;

function AnalyzeSequenceTokenDomains(const AModel: TWfcSequenceModel;
  const ALength: Integer; const AExtent: TWfcSequenceExtent;
  const AConstraints: TWfcSequenceTokenConstraints;
  out AAnalysis: TWfcSequenceDomainAnalysis): Boolean;

function DescribeSequenceAnalyzeIssue(
  const AIssue: TWfcSequenceAnalyzeIssue): String;

implementation

type
  TAnalyzeBooleanArray = array of Boolean;

procedure InitializeAnalysis(const AExtent: TWfcSequenceExtent;
  out AAnalysis: TWfcSequenceDomainAnalysis);
begin
  AAnalysis := Default(TWfcSequenceDomainAnalysis);
  AAnalysis.Extent := AExtent;
  AAnalysis.Issue.Position := -1;
  AAnalysis.Issue.RelatedPosition := -1;
end;

procedure ValidateExtent(const AExtent: TWfcSequenceExtent);
var
  LOrdinal: Integer;
begin
  LOrdinal := Ord(AExtent);
  if (LOrdinal < Ord(Low(TWfcSequenceExtent))) or
      (LOrdinal > Ord(High(TWfcSequenceExtent))) then
    raise EArgumentException.CreateFmt(
      'unknown sequence analysis extent [%d]', [LOrdinal]);
end;

function CheckedCellCount(const ALength, AStateCount: Integer): Integer;
begin
  if ALength < 1 then
    raise ERangeError.CreateFmt(
      'sequence analysis length must be positive [%d]', [ALength]);
  if AStateCount < 1 then
    raise EWfcSequence.Create(
      'sequence analysis model must contain at least one state');
  if ALength > High(Integer) div AStateCount then
    raise ERangeError.Create(
      'sequence analysis dimensions exceed the Integer range');
  Result := ALength * AStateCount;
end;

function CellIndex(const APosition, AState,
  AStateCount: Integer): Integer; inline;
begin
  Result := APosition * AStateCount + AState;
end;

function HasBos(const AModel: TWfcSequenceModel;
  const AStateIndex: Integer): Boolean; inline;
begin
  Result := AModel.StateLeadingBosCountAt(AStateIndex) > 0;
end;

procedure ValidateConstraints(const AModel: TWfcSequenceModel;
  const ALength: Integer;
  const AConstraints: TWfcSequenceTokenConstraints);
begin
  ValidateSequenceTokenConstraints(AModel, ALength, AConstraints);
end;

procedure InitializeAllowedStates(const AModel: TWfcSequenceModel;
  const ALength, ACellCount: Integer;
  const AExtent: TWfcSequenceExtent;
  const AConstraints: TWfcSequenceTokenConstraints;
  out AAllowed: TAnalyzeBooleanArray);
var
  I: Integer;
  LAllowedTokenIndices: TAnalyzeBooleanArray;
  LConstraint: Integer;
  LPosition: Integer;
  LState: Integer;
  LStateCount: Integer;
  LToken: Integer;
begin
  LStateCount := AModel.StateCount;
  SetLength(AAllowed, ACellCount);
  for I := 0 to ACellCount - 1 do
    AAllowed[I] := True;

  case AExtent of
    wseWhole,
    wsePrefix:
      for LState := 0 to LStateCount - 1 do
        if AModel.StartCountAt(LState) < 1 then
          AAllowed[CellIndex(0, LState, LStateCount)] := False;
    wseSuffix,
    wseFragment,
    wseWrap:
      for LPosition := 0 to ALength - 1 do
        for LState := 0 to LStateCount - 1 do
          if HasBos(AModel, LState) then
            AAllowed[CellIndex(LPosition, LState, LStateCount)] := False;
  end;

  if AExtent in [wseWhole, wseSuffix] then
    for LState := 0 to LStateCount - 1 do
      if AModel.EndCountAt(LState) < 1 then
        AAllowed[CellIndex(ALength - 1, LState,
          LStateCount)] := False;

  SetLength(LAllowedTokenIndices, AModel.PublicTokenCount);
  for LConstraint := 0 to Length(AConstraints) - 1 do
  begin
    for I := 0 to Length(LAllowedTokenIndices) - 1 do
      LAllowedTokenIndices[I] := False;
    for I := 0 to Length(AConstraints[
        LConstraint].AllowedTokens) - 1 do
    begin
      LToken := AModel.FindPublicToken(
        AConstraints[LConstraint].AllowedTokens[I]);
      LAllowedTokenIndices[LToken] := True;
    end;

    LPosition := AConstraints[LConstraint].Position;
    for LState := 0 to LStateCount - 1 do
      if not LAllowedTokenIndices[
          AModel.StateEmittedTokenIndexAt(LState)] then
        AAllowed[CellIndex(LPosition, LState, LStateCount)] := False;
  end;
end;

function PositionHasState(const AValues: TAnalyzeBooleanArray;
  const APosition, AStateCount: Integer): Boolean;
var
  LState: Integer;
begin
  for LState := 0 to AStateCount - 1 do
    if AValues[CellIndex(APosition, LState, AStateCount)] then
      Exit(True);
  Result := False;
end;

function FindEmptyPosition(const AAllowed: TAnalyzeBooleanArray;
  const ALength, AStateCount: Integer): Integer;
begin
  for Result := 0 to ALength - 1 do
    if not PositionHasState(AAllowed, Result, AStateCount) then
      Exit;
  Result := -1;
end;

procedure AnalyzeOpenPath(const AModel: TWfcSequenceModel;
  const ALength, ACellCount: Integer;
  const AAllowed: TAnalyzeBooleanArray;
  out AFeasible: TAnalyzeBooleanArray;
  out ASatisfiable: Boolean; out AFailurePosition: Integer);
var
  LBackward: TAnalyzeBooleanArray;
  LForward: TAnalyzeBooleanArray;
  LPosition: Integer;
  LSource: Integer;
  LStateCount: Integer;
  LTarget: Integer;
begin
  LStateCount := AModel.StateCount;
  SetLength(LForward, ACellCount);
  SetLength(LBackward, ACellCount);
  SetLength(AFeasible, ACellCount);

  for LTarget := 0 to LStateCount - 1 do
    LForward[CellIndex(0, LTarget, LStateCount)] :=
      AAllowed[CellIndex(0, LTarget, LStateCount)];

  for LPosition := 1 to ALength - 1 do
    for LTarget := 0 to LStateCount - 1 do
      if AAllowed[CellIndex(LPosition, LTarget, LStateCount)] then
        for LSource := 0 to LStateCount - 1 do
          if LForward[CellIndex(LPosition - 1, LSource,
              LStateCount)] and
              AModel.StatesCompatible(LSource, LTarget) then
          begin
            LForward[CellIndex(LPosition, LTarget, LStateCount)] := True;
            Break;
          end;

  ASatisfiable := PositionHasState(LForward, ALength - 1, LStateCount);
  AFailurePosition := -1;
  if not ASatisfiable then
    for LPosition := 0 to ALength - 1 do
      if not PositionHasState(LForward, LPosition, LStateCount) then
      begin
        AFailurePosition := LPosition;
        Break;
      end;

  for LSource := 0 to LStateCount - 1 do
    LBackward[CellIndex(ALength - 1, LSource, LStateCount)] :=
      AAllowed[CellIndex(ALength - 1, LSource, LStateCount)];

  for LPosition := ALength - 2 downto 0 do
    for LSource := 0 to LStateCount - 1 do
      if AAllowed[CellIndex(LPosition, LSource, LStateCount)] then
        for LTarget := 0 to LStateCount - 1 do
          if LBackward[CellIndex(LPosition + 1, LTarget,
              LStateCount)] and
              AModel.StatesCompatible(LSource, LTarget) then
          begin
            LBackward[CellIndex(LPosition, LSource, LStateCount)] := True;
            Break;
          end;

  for LPosition := 0 to ALength - 1 do
    for LSource := 0 to LStateCount - 1 do
      AFeasible[CellIndex(LPosition, LSource, LStateCount)] :=
        LForward[CellIndex(LPosition, LSource, LStateCount)] and
        LBackward[CellIndex(LPosition, LSource, LStateCount)];
end;

procedure AnalyzeWrappedPath(const AModel: TWfcSequenceModel;
  const ALength, ACellCount: Integer;
  const AAllowed: TAnalyzeBooleanArray;
  out AFeasible: TAnalyzeBooleanArray;
  out ASatisfiable: Boolean);
var
  LBackward: TAnalyzeBooleanArray;
  LClosingState: Integer;
  LForward: TAnalyzeBooleanArray;
  LPosition: Integer;
  LSource: Integer;
  LStart: Integer;
  LStateCount: Integer;
  LTarget: Integer;
begin
  LStateCount := AModel.StateCount;
  SetLength(AFeasible, ACellCount);
  SetLength(LForward, ACellCount);
  SetLength(LBackward, ACellCount);
  ASatisfiable := False;

  for LStart := 0 to LStateCount - 1 do
  begin
    if not AAllowed[CellIndex(0, LStart, LStateCount)] then
      Continue;
    for LPosition := 0 to ACellCount - 1 do
    begin
      LForward[LPosition] := False;
      LBackward[LPosition] := False;
    end;

    LForward[CellIndex(0, LStart, LStateCount)] := True;
    for LPosition := 1 to ALength - 1 do
      for LTarget := 0 to LStateCount - 1 do
        if AAllowed[CellIndex(LPosition, LTarget, LStateCount)] then
          for LSource := 0 to LStateCount - 1 do
            if LForward[CellIndex(LPosition - 1, LSource,
                LStateCount)] and
                AModel.StatesCompatible(LSource, LTarget) then
            begin
              LForward[CellIndex(LPosition, LTarget,
                LStateCount)] := True;
              Break;
            end;

    for LClosingState := 0 to LStateCount - 1 do
      if LForward[CellIndex(ALength - 1, LClosingState,
          LStateCount)] and
          AModel.StatesCompatible(LClosingState, LStart) then
      begin
        LBackward[CellIndex(ALength - 1, LClosingState,
          LStateCount)] := True;
        ASatisfiable := True;
      end;
    if not PositionHasState(LBackward, ALength - 1, LStateCount) then
      Continue;

    for LPosition := ALength - 2 downto 0 do
      for LSource := 0 to LStateCount - 1 do
        if AAllowed[CellIndex(LPosition, LSource, LStateCount)] then
          for LTarget := 0 to LStateCount - 1 do
            if LBackward[CellIndex(LPosition + 1, LTarget,
                LStateCount)] and
                AModel.StatesCompatible(LSource, LTarget) then
            begin
              LBackward[CellIndex(LPosition, LSource,
                LStateCount)] := True;
              Break;
            end;

    for LPosition := 0 to ALength - 1 do
      for LSource := 0 to LStateCount - 1 do
        if LForward[CellIndex(LPosition, LSource, LStateCount)] and
            LBackward[CellIndex(LPosition, LSource, LStateCount)] then
          AFeasible[CellIndex(LPosition, LSource,
            LStateCount)] := True;
  end;
end;

procedure BuildPositionDomains(const AModel: TWfcSequenceModel;
  const ALength: Integer; const AFeasible: TAnalyzeBooleanArray;
  out APositions: TWfcSequencePositionDomains);
var
  LPosition: Integer;
  LState: Integer;
  LStateCount: Integer;
  LStateWrite: Integer;
  LToken: Integer;
  LTokenCount: Integer;
  LTokenWrite: Integer;
  LWeight: Integer;
begin
  LStateCount := AModel.StateCount;
  LTokenCount := AModel.PublicTokenCount;
  SetLength(APositions, ALength);
  for LPosition := 0 to ALength - 1 do
  begin
    LStateWrite := 0;
    for LState := 0 to LStateCount - 1 do
      if AFeasible[CellIndex(LPosition, LState, LStateCount)] then
        Inc(LStateWrite);
    SetLength(APositions[LPosition].StateIndices, LStateWrite);
    LStateWrite := 0;
    for LState := 0 to LStateCount - 1 do
      if AFeasible[CellIndex(LPosition, LState, LStateCount)] then
      begin
        APositions[LPosition].StateIndices[LStateWrite] := LState;
        Inc(LStateWrite);
      end;

    LTokenWrite := 0;
    for LToken := 0 to LTokenCount - 1 do
    begin
      LWeight := 0;
      for LState := 0 to LStateCount - 1 do
        if AFeasible[CellIndex(LPosition, LState, LStateCount)] and
            (AModel.StateEmittedTokenIndexAt(LState) = LToken) then
          Inc(LWeight, AModel.StateObservationCountAt(LState));
      if LWeight > 0 then
        Inc(LTokenWrite);
    end;

    SetLength(APositions[LPosition].Tokens, LTokenWrite);
    SetLength(APositions[LPosition].Weights, LTokenWrite);
    LTokenWrite := 0;
    for LToken := 0 to LTokenCount - 1 do
    begin
      LWeight := 0;
      for LState := 0 to LStateCount - 1 do
        if AFeasible[CellIndex(LPosition, LState, LStateCount)] and
            (AModel.StateEmittedTokenIndexAt(LState) = LToken) then
          Inc(LWeight, AModel.StateObservationCountAt(LState));
      if LWeight > 0 then
      begin
        APositions[LPosition].Tokens[LTokenWrite] :=
          AModel.PublicTokenAt(LToken);
        APositions[LPosition].Weights[LTokenWrite] := LWeight;
        Inc(LTokenWrite);
      end;
    end;
  end;
end;

function AnalyzeSequenceTokenDomains(const AModel: TWfcSequenceModel;
  const ALength: Integer; const AExtent: TWfcSequenceExtent;
  const AConstraints: TWfcSequenceTokenConstraints;
  out AAnalysis: TWfcSequenceDomainAnalysis): Boolean;
var
  LAllowed: TAnalyzeBooleanArray;
  LCellCount: Integer;
  LEmptyPosition: Integer;
  LFailurePosition: Integer;
  LFeasible: TAnalyzeBooleanArray;
  LSatisfiable: Boolean;
begin
  InitializeAnalysis(AExtent, AAnalysis);
  if not Assigned(AModel) then
    raise EArgumentNilException.Create(
      'sequence analysis model cannot be nil');
  ValidateExtent(AExtent);
  LCellCount := CheckedCellCount(ALength, AModel.StateCount);
  ValidateConstraints(AModel, ALength, AConstraints);
  InitializeAllowedStates(AModel, ALength, LCellCount, AExtent,
    AConstraints, LAllowed);

  LEmptyPosition := FindEmptyPosition(LAllowed, ALength,
    AModel.StateCount);
  LFailurePosition := -1;
  if AExtent = wseWrap then
    AnalyzeWrappedPath(AModel, ALength, LCellCount, LAllowed,
      LFeasible, LSatisfiable)
  else
    AnalyzeOpenPath(AModel, ALength, LCellCount, LAllowed,
      LFeasible, LSatisfiable, LFailurePosition);

  BuildPositionDomains(AModel, ALength, LFeasible,
    AAnalysis.Positions);
  AAnalysis.Satisfiable := LSatisfiable;
  if LSatisfiable then
    AAnalysis.Issue.Kind := wsaikNone
  else if LEmptyPosition >= 0 then
  begin
    AAnalysis.Issue.Kind := wsaikEmptyDomain;
    AAnalysis.Issue.Position := LEmptyPosition;
  end
  else if AExtent = wseWrap then
    AAnalysis.Issue.Kind := wsaikNoCycle
  else
  begin
    AAnalysis.Issue.Kind := wsaikNoPath;
    AAnalysis.Issue.Position := LFailurePosition;
    if LFailurePosition > 0 then
      AAnalysis.Issue.RelatedPosition := LFailurePosition - 1;
  end;
  Result := LSatisfiable;
end;

function DescribeSequenceAnalyzeIssue(
  const AIssue: TWfcSequenceAnalyzeIssue): String;
begin
  Result := 'unknown sequence analysis issue';
  case AIssue.Kind of
    wsaikNone:
      Result := 'no sequence analysis issue';
    wsaikEmptyDomain:
      Result := Format('sequence position %d has no permitted states',
        [AIssue.Position]);
    wsaikNoPath:
      if AIssue.RelatedPosition >= 0 then
        Result := Format(
          'no sequence path reaches position %d from position %d',
          [AIssue.Position, AIssue.RelatedPosition])
      else
        Result := 'no sequence path satisfies the requested extent';
    wsaikNoCycle:
      Result := 'no sequence cycle satisfies the requested constraints';
  end;
end;

end.
