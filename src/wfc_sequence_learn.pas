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
unit wfc_sequence_learn;

{$mode delphi}{$H+}

interface

uses
  wfc_model,
  wfc_sequence;

const
  WFC_SEQUENCE_LEARN_ALGORITHM_VERSION = 1;

type
  TWfcSequenceSample = record
    Tokens: TWfcModelTokens;
  end;
  TWfcSequenceSamples = array of TWfcSequenceSample;

function MakeWfcSequenceSample(
  const ATokens: TWfcModelTokens): TWfcSequenceSample;

function LearnSequenceModelCorpus(const ASamples: TWfcSequenceSamples;
  const AOrder: Integer): TWfcSequenceModel;

function LearnSequenceModel(const ATokens: TWfcModelTokens;
  const AOrder: Integer): TWfcSequenceModel;

implementation

uses
  SysUtils;

type
  TIntegerArray = array of Integer;
  TIntegerArrays = array of TIntegerArray;

function CheckedLength(const ALength: SizeInt;
  const ALabel: String): Integer;
begin
  if (ALength < 0) or
      ((ALength and (not SizeInt(High(Integer)))) <> 0) then
    raise EWfcSequence.Create(ALabel + ' exceeds the Integer range');
  Result := Integer(ALength);
end;

function CheckedAdd(const A, B: Integer;
  const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) then
    raise EWfcSequence.Create(ALabel + ' cannot be negative');
  if A > High(Integer) - B then
    raise EWfcSequence.Create(ALabel + ' exceeds the Integer range');
  Result := A + B;
end;

procedure CheckedIncrement(var AValue: Integer;
  const ALabel: String);
begin
  if AValue = High(Integer) then
    raise EWfcSequence.Create(ALabel +
      ' exceeds the Integer count range');
  Inc(AValue);
end;

procedure CheckStateCapacity(const AStateCount: Integer);
var
  LSquare: Integer;
begin
  if AStateCount < 1 then
    Exit;
  if AStateCount > High(Integer) div AStateCount then
    raise EWfcSequence.Create(
      'sequence state relation dimensions exceed the Integer range');
  LSquare := AStateCount * AStateCount;
  if LSquare > High(Integer) div 4 then
    raise EWfcSequence.Create(
      'sequence state relation dimensions exceed the Integer range');
end;

function FindPublicToken(const APublicTokens: TWfcModelTokens;
  const AToken: TWfcModelToken): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(APublicTokens) - 1 do
    if APublicTokens[I] = AToken then
      Exit(I);
  Result := -1;
end;

function HistoryItemsEqual(const A, B: TWfcSequenceHistoryItem): Boolean;
begin
  Result := (A.Kind = B.Kind) and (A.TokenIndex = B.TokenIndex);
end;

function StatesEqual(const A, B: TWfcSequenceState): Boolean;
var
  I: Integer;
begin
  if (A.EmittedTokenIndex <> B.EmittedTokenIndex) or
      (Length(A.History) <> Length(B.History)) then
    Exit(False);
  for I := 0 to Length(A.History) - 1 do
    if not HistoryItemsEqual(A.History[I], B.History[I]) then
      Exit(False);
  Result := True;
end;

function FindState(const AStates: TWfcSequenceStates;
  const AState: TWfcSequenceState): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(AStates) - 1 do
    if StatesEqual(AStates[I], AState) then
      Exit(I);
  Result := -1;
end;

procedure AppendPublicToken(const AToken: TWfcModelToken;
  var APublicTokens: TWfcModelTokens; out AIndex: Integer);
begin
  AIndex := CheckedLength(Length(APublicTokens),
    'sequence public-token count');
  if AIndex = High(Integer) then
    raise EWfcSequence.Create(
      'sequence public-token count exceeds the Integer range');
  SetLength(APublicTokens, AIndex + 1);
  APublicTokens[AIndex] := AToken;
end;

procedure AppendState(const AState: TWfcSequenceState;
  var AStates: TWfcSequenceStates;
  var AStateCounts, AStartCounts,
  AEndCounts: TWfcModelIntegerArray; out AIndex: Integer);
var
  I: Integer;
begin
  AIndex := CheckedLength(Length(AStates),
    'sequence state count');
  if AIndex = High(Integer) then
    raise EWfcSequence.Create(
      'sequence state count exceeds the Integer range');
  CheckStateCapacity(AIndex + 1);
  SetLength(AStates, AIndex + 1);
  SetLength(AStates[AIndex].History, Length(AState.History));
  for I := 0 to Length(AState.History) - 1 do
    AStates[AIndex].History[I] := AState.History[I];
  AStates[AIndex].EmittedTokenIndex := AState.EmittedTokenIndex;
  SetLength(AStateCounts, AIndex + 1);
  SetLength(AStartCounts, AIndex + 1);
  SetLength(AEndCounts, AIndex + 1);
  AStateCounts[AIndex] := 0;
  AStartCounts[AIndex] := 0;
  AEndCounts[AIndex] := 0;
end;

function MakeWfcSequenceSample(
  const ATokens: TWfcModelTokens): TWfcSequenceSample;
var
  I: Integer;
  LTokenCount: Integer;
begin
  LTokenCount := CheckedLength(Length(ATokens),
    'sequence learning sample token count');
  if LTokenCount = 0 then
    raise EWfcSequence.Create(
      'a sequence learning sample cannot be empty');
  Result.Tokens := nil;
  SetLength(Result.Tokens, LTokenCount);
  for I := 0 to LTokenCount - 1 do
    Result.Tokens[I] := ATokens[I];
end;

function LearnSequenceModelCorpus(const ASamples: TWfcSequenceSamples;
  const AOrder: Integer): TWfcSequenceModel;
var
  H: Integer;
  I: Integer;
  LCandidate: TWfcSequenceState;
  LDistance: Integer;
  LEndCounts: TWfcModelIntegerArray;
  LHistorySize: Integer;
  LObservationTotal: Integer;
  LPosition: Integer;
  LPublicTokenIndex: Integer;
  LPublicTokens: TWfcModelTokens;
  LSampleCount: Integer;
  LSampleIndex: Integer;
  LSampleLengths: TWfcSequenceSampleLengths;
  LStartCounts: TWfcModelIntegerArray;
  LStateCounts: TWfcModelIntegerArray;
  LStateIndex: Integer;
  LStates: TWfcSequenceStates;
  LTokenCount: Integer;
  LValues: TIntegerArrays;
begin
  if AOrder < 1 then
    raise EWfcSequence.CreateFmt(
      'sequence order must be positive [%d]', [AOrder]);
  LHistorySize := AOrder - 1;
  LSampleCount := CheckedLength(Length(ASamples),
    'sequence learning corpus sample count');
  if LSampleCount = 0 then
    raise EWfcSequence.Create(
      'a sequence learning corpus cannot be empty');

  SetLength(LSampleLengths, LSampleCount);
  SetLength(LValues, LSampleCount);
  SetLength(LPublicTokens, 0);
  LObservationTotal := 0;
  for LSampleIndex := 0 to LSampleCount - 1 do
  begin
    LTokenCount := CheckedLength(
      Length(ASamples[LSampleIndex].Tokens),
      Format('sequence learning sample %d token count', [LSampleIndex]));
    if LTokenCount = 0 then
      raise EWfcSequence.CreateFmt(
        'sequence learning sample cannot be empty [%d]', [LSampleIndex]);
    LObservationTotal := CheckedAdd(LObservationTotal, LTokenCount,
      'sequence learning observation total');
    LSampleLengths[LSampleIndex] := LTokenCount;
    SetLength(LValues[LSampleIndex], LTokenCount);
    for I := 0 to LTokenCount - 1 do
    begin
      if not WfcModelTokenIsValid(ASamples[LSampleIndex].Tokens[I]) then
        raise EWfcSequence.CreateFmt(
          'sequence sample token must be nonempty, well-formed UTF-8 [%d, %d]',
          [LSampleIndex, I]);
      LPublicTokenIndex := FindPublicToken(LPublicTokens,
        ASamples[LSampleIndex].Tokens[I]);
      if LPublicTokenIndex < 0 then
        AppendPublicToken(ASamples[LSampleIndex].Tokens[I],
          LPublicTokens, LPublicTokenIndex);
      LValues[LSampleIndex][I] := LPublicTokenIndex;
    end;
  end;

  SetLength(LStates, 0);
  SetLength(LStateCounts, 0);
  SetLength(LStartCounts, 0);
  SetLength(LEndCounts, 0);
  SetLength(LCandidate.History, LHistorySize);
  for LSampleIndex := 0 to LSampleCount - 1 do
    for LPosition := 0 to LSampleLengths[LSampleIndex] - 1 do
    begin
      for H := 0 to LHistorySize - 1 do
      begin
        LDistance := LHistorySize - H;
        if LPosition < LDistance then
          LCandidate.History[H] := MakeWfcSequenceBosHistoryItem
        else
          LCandidate.History[H] := MakeWfcSequenceTokenHistoryItem(
            LValues[LSampleIndex][LPosition - LDistance]);
      end;
      LCandidate.EmittedTokenIndex :=
        LValues[LSampleIndex][LPosition];
      LStateIndex := FindState(LStates, LCandidate);
      if LStateIndex < 0 then
        AppendState(LCandidate, LStates, LStateCounts,
          LStartCounts, LEndCounts, LStateIndex);
      CheckedIncrement(LStateCounts[LStateIndex],
        'sequence state observation count');
      if LPosition = 0 then
        CheckedIncrement(LStartCounts[LStateIndex],
          'sequence start observation count');
      if LPosition = LSampleLengths[LSampleIndex] - 1 then
        CheckedIncrement(LEndCounts[LStateIndex],
          'sequence end observation count');
    end;

  if LObservationTotal = 0 then
    raise EWfcSequence.Create(
      'a sequence learning corpus must contain observations');
  Result := TWfcSequenceModel.Create(AOrder, LSampleLengths,
    LPublicTokens, LStates, LStateCounts, LStartCounts, LEndCounts);
end;

function LearnSequenceModel(const ATokens: TWfcModelTokens;
  const AOrder: Integer): TWfcSequenceModel;
var
  LSamples: TWfcSequenceSamples;
begin
  SetLength(LSamples, 1);
  LSamples[0] := MakeWfcSequenceSample(ATokens);
  Result := LearnSequenceModelCorpus(LSamples, AOrder);
end;

end.
