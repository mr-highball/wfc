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
unit wfc_sequence;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc_model;

const
  WFC_SEQUENCE_MODEL_VERSION = 1;
  WFC_SEQUENCE_WRAPPED_MODEL_VERSION = 2;
  WFC_SEQUENCE_GRAPH_MODEL_VERSION = 1;
  WFC_SEQUENCE_EXTENT_VERSION = 1;

  { Version-1 portability and denial-of-service boundaries. Together the
    state and history limits bound quadratic structural uniqueness and graph
    relation work while retaining room for large practical corpora. }
  WFC_SEQUENCE_LIMITS_VERSION = 1;
  WFC_SEQUENCE_MAX_ORDER = 1024;
  WFC_SEQUENCE_MAX_SAMPLE_COUNT = 4096;
  WFC_SEQUENCE_MAX_PUBLIC_TOKEN_COUNT = 1024;
  WFC_SEQUENCE_MAX_STATE_COUNT = 1024;
  WFC_SEQUENCE_MAX_TOTAL_HISTORY_ITEM_COUNT = 65536;

type
  EWfcSequence = class(EWfcModel);

  TWfcSequenceHistoryKind = (
    wshBos,
    wshToken
  );

  (*
    BOS is a typed value, never a vocabulary token. Its canonical TokenIndex
    is -1. Token history items always contain an index into the ordered public
    vocabulary.
  *)
  TWfcSequenceHistoryItem = record
    Kind: TWfcSequenceHistoryKind;
    TokenIndex: Integer;
  end;
  TWfcSequenceHistory = array of TWfcSequenceHistoryItem;

  (*
    One latent state corresponds to one public output position. History has
    exactly Order - 1 items. Open samples are left-padded with typed BOS;
    circular samples wrap history within their own sample and contain no BOS.
    The emitted token is projected at the same coordinate.
  *)
  TWfcSequenceState = record
    History: TWfcSequenceHistory;
    EmittedTokenIndex: Integer;
  end;
  TWfcSequenceStates = array of TWfcSequenceState;
  TWfcSequenceStateIndices = array of Integer;
  TWfcSequenceSampleLengths = array of Integer;

  (*
    Describes which learned corpus boundaries a generated path represents.
    Whole paths begin and end at observed sample boundaries. Prefixes begin at
    an observed sample boundary but may stop anywhere; suffixes may begin at
    an interior state but end at an observed boundary; fragments require
    neither endpoint. Interior starts exclude every BOS-bearing latent state.
    Wrapped paths are BOS-free structural cycles.
  *)
  TWfcSequenceExtent = (
    wseWhole,
    wsePrefix,
    wseSuffix,
    wseFragment,
    wseWrap
  );

  TWfcSequenceTokenConstraint = record
    Position: Integer;
    AllowedTokens: TWfcModelTokens;
  end;
  TWfcSequenceTokenConstraints = array of TWfcSequenceTokenConstraint;

  { TWfcSequenceModel }

  (*
    Immutable exact bounded n-gram data. State order and public-token order
    are significant. Counts retain raw observations; StartCountAt and
    EndCountAt are zero for states not observed at the corresponding open
    boundary.

    CreateGraphModel deliberately accepts caller-supplied state keys. This
    unit owns the typed sequence model, not a private string-key codec. The
    returned TWfcModel is caller-owned and uses structural suffix/history
    overlap for exact east/west support.
  *)
  TWfcSequenceModel = class
  strict private
    FOrder: Integer;
    FBoundary: TWfcModelBoundary;
    FSampleLengths: TWfcSequenceSampleLengths;
    FPublicTokens: TWfcModelTokens;
    FStates: TWfcSequenceStates;
    FStateCounts: TWfcModelIntegerArray;
    FStartCounts: TWfcModelIntegerArray;
    FEndCounts: TWfcModelIntegerArray;
    FObservationCount: Integer;

    function GetHistorySize: Integer;
    function GetModelVersion: Integer;
    function GetSampleCount: Integer;
    function GetPublicTokenCount: Integer;
    function GetStateCount: Integer;
    procedure ValidateSampleIndex(const AIndex: Integer);
    procedure ValidatePublicTokenIndex(const AIndex: Integer);
    procedure ValidateStateIndex(const AIndex: Integer);
    procedure ValidateHistoryIndex(const AIndex: Integer);
    procedure ValidateCircularCounts;
  public
    constructor Create(const AOrder: Integer;
      const ASampleLengths: TWfcSequenceSampleLengths;
      const APublicTokens: TWfcModelTokens;
      const AStates: TWfcSequenceStates;
      const AStateCounts, AStartCounts,
      AEndCounts: TWfcModelIntegerArray;
      const ABoundary: TWfcModelBoundary = wmbOpen);

    function SampleLengthAt(const ASampleIndex: Integer): Integer;
    function PublicTokenAt(
      const APublicTokenIndex: Integer): TWfcModelToken;
    function FindPublicToken(const AToken: TWfcModelToken): Integer;
    function HistoryItemAt(const AStateIndex,
      AHistoryIndex: Integer): TWfcSequenceHistoryItem;
    function StateEmittedTokenIndexAt(
      const AStateIndex: Integer): Integer;
    function StateLeadingBosCountAt(
      const AStateIndex: Integer): Integer;
    function ProjectStateToken(
      const AStateIndex: Integer): TWfcModelToken;
    function StateObservationCountAt(
      const AStateIndex: Integer): Integer;
    function StartCountAt(const AStateIndex: Integer): Integer;
    function EndCountAt(const AStateIndex: Integer): Integer;
    function StatesCompatible(const ASourceState,
      ATargetState: Integer): Boolean;

    function CopySampleLengths: TWfcSequenceSampleLengths;
    function CopyPublicTokens: TWfcModelTokens;
    function CopyState(
      const AStateIndex: Integer): TWfcSequenceState;
    function CopyStates: TWfcSequenceStates;
    function CopyStateCounts: TWfcModelIntegerArray;
    function CopyStartCounts: TWfcModelIntegerArray;
    function CopyEndCounts: TWfcModelIntegerArray;
    function CopyStartStateIndices: TWfcSequenceStateIndices;
    function CopyEndStateIndices: TWfcSequenceStateIndices;
    function ProjectStateIndices(
      const AStateIndices: TWfcSequenceStateIndices): TWfcModelTokens;

    function CreateGraphModel(
      const AStateKeys: TWfcModelTokens): TWfcModel;

    property Order: Integer read FOrder;
    property Boundary: TWfcModelBoundary read FBoundary;
    property ModelVersion: Integer read GetModelVersion;
    property HistorySize: Integer read GetHistorySize;
    property SampleCount: Integer read GetSampleCount;
    property PublicTokenCount: Integer read GetPublicTokenCount;
    property StateCount: Integer read GetStateCount;
    property ObservationCount: Integer read FObservationCount;
  end;

function MakeWfcSequenceBosHistoryItem: TWfcSequenceHistoryItem;

function MakeWfcSequenceTokenHistoryItem(
  const APublicTokenIndex: Integer): TWfcSequenceHistoryItem;

function MakeWfcSequenceState(const AHistory: TWfcSequenceHistory;
  const AEmittedTokenIndex: Integer): TWfcSequenceState;

function MakeWfcSequenceTokenConstraint(const APosition: Integer;
  const AAllowedTokens: TWfcModelTokens): TWfcSequenceTokenConstraint;

{ Validates every position and public token before a caller mutates graph or
  analysis state. Empty allowed-token sets are valid constraints that make a
  position unsatisfiable. }
procedure ValidateSequenceTokenConstraints(
  const AModel: TWfcSequenceModel; const ALength: Integer;
  const AConstraints: TWfcSequenceTokenConstraints);

implementation

type
  TBooleanArray = array of Boolean;

procedure RequireCircularInteger(const AValue, AMinimum, AMaximum: Integer;
  const ALabel: String);
var LValid: Boolean;
begin
  {$IFDEF PAS2JS}
  asm
    LValid = typeof AValue === 'number' && isFinite(AValue) &&
      Math.floor(AValue) === AValue && AValue >= AMinimum && AValue <= AMaximum;
  end;
  {$ELSE}
  LValid := (AValue >= AMinimum) and (AValue <= AMaximum);
  {$ENDIF}
  if not LValid then
    raise EWfcSequence.Create(ALabel + ' must be an exact integer in range');
end;

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

function CheckedGraphRelationLength(
  const AStateCount: Integer): Integer;
var
  LSquare: Integer;
begin
  if AStateCount < 1 then
    raise EWfcSequence.Create(
      'a sequence model must contain at least one state');
  if AStateCount > High(Integer) div AStateCount then
    raise EWfcSequence.Create(
      'sequence graph relation dimensions exceed the Integer range');
  LSquare := AStateCount * AStateCount;
  if LSquare > High(Integer) div 4 then
    raise EWfcSequence.Create(
      'sequence graph relation dimensions exceed the Integer range');
  Result := 4 * LSquare;
end;

function HistoryItemsEqual(const A, B: TWfcSequenceHistoryItem): Boolean;
begin
  Result := (A.Kind = B.Kind) and (A.TokenIndex = B.TokenIndex);
end;

function SequenceStatesEqual(const A, B: TWfcSequenceState): Boolean;
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

function GraphRelationIndex(const ADirection: TWfcModelDirection;
  const ASourceState, ATargetState, AStateCount: Integer): Integer;
begin
  Result := ((Ord(ADirection) * AStateCount) + ASourceState) *
    AStateCount + ATargetState;
end;

function CopyIntegerArray(
  const AValues: TWfcModelIntegerArray): TWfcModelIntegerArray;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function MakeWfcSequenceBosHistoryItem: TWfcSequenceHistoryItem;
begin
  Result.Kind := wshBos;
  Result.TokenIndex := -1;
end;

function MakeWfcSequenceTokenHistoryItem(
  const APublicTokenIndex: Integer): TWfcSequenceHistoryItem;
begin
  if APublicTokenIndex < 0 then
    raise EWfcSequence.CreateFmt(
      'sequence public-token index cannot be negative [%d]',
      [APublicTokenIndex]);
  Result.Kind := wshToken;
  Result.TokenIndex := APublicTokenIndex;
end;

function MakeWfcSequenceState(const AHistory: TWfcSequenceHistory;
  const AEmittedTokenIndex: Integer): TWfcSequenceState;
var
  I: Integer;
begin
  if AEmittedTokenIndex < 0 then
    raise EWfcSequence.CreateFmt(
      'sequence emitted-token index cannot be negative [%d]',
      [AEmittedTokenIndex]);
  Result.History := nil;
  SetLength(Result.History, Length(AHistory));
  for I := 0 to Length(AHistory) - 1 do
    Result.History[I] := AHistory[I];
  Result.EmittedTokenIndex := AEmittedTokenIndex;
end;

function MakeWfcSequenceTokenConstraint(const APosition: Integer;
  const AAllowedTokens: TWfcModelTokens): TWfcSequenceTokenConstraint;
var
  I: Integer;
  LTokenCount: Integer;
begin
  LTokenCount := CheckedLength(Length(AAllowedTokens),
    'sequence constraint token count');
  Result.Position := APosition;
  Result.AllowedTokens := nil;
  SetLength(Result.AllowedTokens, LTokenCount);
  for I := 0 to LTokenCount - 1 do
    Result.AllowedTokens[I] := AAllowedTokens[I];
end;

procedure ValidateSequenceTokenConstraints(
  const AModel: TWfcSequenceModel; const ALength: Integer;
  const AConstraints: TWfcSequenceTokenConstraints);
var
  I: Integer;
  J: Integer;
  LConstraintCount: Integer;
  LTokenCount: Integer;
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create(
      'sequence constraint model cannot be nil');
  if ALength < 0 then
    raise ERangeError.CreateFmt(
      'sequence constraint length cannot be negative [%d]', [ALength]);
  LConstraintCount := CheckedLength(Length(AConstraints),
    'sequence constraint count');
  for I := 0 to LConstraintCount - 1 do
  begin
    if (AConstraints[I].Position < 0) or
        (AConstraints[I].Position >= ALength) then
      raise ERangeError.CreateFmt(
        'sequence constraint position is out of bounds [%d: %d]',
        [I, AConstraints[I].Position]);
    LTokenCount := CheckedLength(
      Length(AConstraints[I].AllowedTokens),
      Format('sequence constraint %d token count', [I]));
    for J := 0 to LTokenCount - 1 do
      if AModel.FindPublicToken(
          AConstraints[I].AllowedTokens[J]) < 0 then
        raise EArgumentException.CreateFmt(
          'unknown sequence constraint token [%d, %d]', [I, J]);
  end;
end;

constructor TWfcSequenceModel.Create(const AOrder: Integer;
  const ASampleLengths: TWfcSequenceSampleLengths;
  const APublicTokens: TWfcModelTokens;
  const AStates: TWfcSequenceStates;
  const AStateCounts, AStartCounts,
  AEndCounts: TWfcModelIntegerArray;
  const ABoundary: TWfcModelBoundary);
var
  I: Integer;
  J: Integer;
  LAllBos: Boolean;
  LEndTotal: Integer;
  LDepth: Integer;
  LDepthEndCounts: TWfcModelIntegerArray;
  LDepthObservationCounts: TWfcModelIntegerArray;
  LExpectedObservations: Integer;
  LExpectedDepthEndCounts: TWfcModelIntegerArray;
  LExpectedDepthObservationCounts: TWfcModelIntegerArray;
  LHistorySize: Integer;
  LHasPredecessor: Boolean;
  LHasSuccessor: Boolean;
  LPublicTokenCount: Integer;
  LLeadingBosCount: Integer;
  LPrefixPositionCount: Integer;
  LSampleCount: Integer;
  LStartTotal: Integer;
  LStateCount: Integer;
  LTokenHistorySeen: Boolean;
  LUsedPublicTokens: TBooleanArray;
begin
  inherited Create;
  case ABoundary of
    wmbOpen, wmbWrap: FBoundary := ABoundary;
  else
    raise EWfcSequence.Create('unknown sequence learning boundary');
  end;
  if FBoundary = wmbWrap then
    RequireCircularInteger(AOrder, 1, WFC_SEQUENCE_MAX_ORDER, 'sequence order');
  if AOrder < 1 then
    raise EWfcSequence.CreateFmt(
      'sequence order must be positive [%d]', [AOrder]);
  if AOrder > WFC_SEQUENCE_MAX_ORDER then
    raise EWfcSequence.Create(
      'sequence order exceeds the version-1 limit');
  LHistorySize := AOrder - 1;

  LSampleCount := CheckedLength(Length(ASampleLengths),
    'sequence sample count');
  if LSampleCount > WFC_SEQUENCE_MAX_SAMPLE_COUNT then
    raise EWfcSequence.Create(
      'sequence sample count exceeds the version-1 limit');
  LPublicTokenCount := CheckedLength(Length(APublicTokens),
    'sequence public-token count');
  if LPublicTokenCount > WFC_SEQUENCE_MAX_PUBLIC_TOKEN_COUNT then
    raise EWfcSequence.Create(
      'sequence public-token count exceeds the version-1 limit');
  LStateCount := CheckedLength(Length(AStates),
    'sequence state count');
  if LStateCount > WFC_SEQUENCE_MAX_STATE_COUNT then
    raise EWfcSequence.Create(
      'sequence state count exceeds the version-1 limit');
  if (LStateCount > 0) and (LHistorySize > 0) and
      (LStateCount > WFC_SEQUENCE_MAX_TOTAL_HISTORY_ITEM_COUNT div
        LHistorySize) then
    raise EWfcSequence.Create(
      'sequence history size exceeds the version-1 aggregate limit');
  if LSampleCount = 0 then
    raise EWfcSequence.Create(
      'a sequence model must retain at least one sample');
  SetLength(FSampleLengths, LSampleCount);
  LExpectedObservations := 0;
  for I := 0 to LSampleCount - 1 do
  begin
    if FBoundary = wmbWrap then
      RequireCircularInteger(ASampleLengths[I], 1, High(Integer),
        'circular sequence sample length');
    if ASampleLengths[I] < 1 then
      raise EWfcSequence.CreateFmt(
        'sequence sample length must be positive [%d: %d]',
        [I, ASampleLengths[I]]);
    FSampleLengths[I] := ASampleLengths[I];
    LExpectedObservations := CheckedAdd(LExpectedObservations,
      ASampleLengths[I], 'sequence observation total');
  end;

  { A BOS depth identifies an absolute position while the history window is
    still entering the sample. Preserve enough source metadata to reject
    state/count combinations that no bounded corpus of these lengths could
    have produced. Depth zero contains all positions at or beyond the full
    history width. }
  SetLength(LExpectedDepthObservationCounts, AOrder);
  SetLength(LExpectedDepthEndCounts, AOrder);
  if FBoundary = wmbWrap then
    LExpectedDepthObservationCounts[0] := LExpectedObservations
  else for I := 0 to LSampleCount - 1 do
  begin
    LPrefixPositionCount := LHistorySize;
    if ASampleLengths[I] < LPrefixPositionCount then
      LPrefixPositionCount := ASampleLengths[I];
    for J := 0 to LPrefixPositionCount - 1 do
    begin
      LDepth := LHistorySize - J;
      LExpectedDepthObservationCounts[LDepth] := CheckedAdd(
        LExpectedDepthObservationCounts[LDepth], 1,
        'sequence BOS-depth observation total');
      if J = ASampleLengths[I] - 1 then
        LExpectedDepthEndCounts[LDepth] := CheckedAdd(
          LExpectedDepthEndCounts[LDepth], 1,
          'sequence BOS-depth end total');
    end;
    if ASampleLengths[I] > LHistorySize then
    begin
      LExpectedDepthObservationCounts[0] := CheckedAdd(
        LExpectedDepthObservationCounts[0],
        ASampleLengths[I] - LHistorySize,
        'sequence full-history observation total');
      LExpectedDepthEndCounts[0] := CheckedAdd(
        LExpectedDepthEndCounts[0], 1,
        'sequence full-history end total');
    end;
  end;

  if LPublicTokenCount = 0 then
    raise EWfcSequence.Create(
      'a sequence model must contain at least one public token');
  SetLength(FPublicTokens, LPublicTokenCount);
  for I := 0 to LPublicTokenCount - 1 do
  begin
    if not WfcModelTokenIsValid(APublicTokens[I]) then
      raise EWfcSequence.CreateFmt(
        'sequence public token must be nonempty, well-formed UTF-8 [%d]',
        [I]);
    for J := 0 to I - 1 do
      if APublicTokens[I] = APublicTokens[J] then
        raise EWfcSequence.CreateFmt(
          'sequence public tokens must be unique [%d, %d]', [J, I]);
    FPublicTokens[I] := APublicTokens[I];
  end;

  CheckedGraphRelationLength(LStateCount);
  if Length(AStateCounts) <> LStateCount then
    raise EWfcSequence.CreateFmt(
      'sequence state-count length must match state count [%d <> %d]',
      [Length(AStateCounts), LStateCount]);
  if Length(AStartCounts) <> LStateCount then
    raise EWfcSequence.CreateFmt(
      'sequence start-count length must match state count [%d <> %d]',
      [Length(AStartCounts), LStateCount]);
  if Length(AEndCounts) <> LStateCount then
    raise EWfcSequence.CreateFmt(
      'sequence end-count length must match state count [%d <> %d]',
      [Length(AEndCounts), LStateCount]);

  FOrder := AOrder;
  SetLength(FStates, LStateCount);
  SetLength(FStateCounts, LStateCount);
  SetLength(FStartCounts, LStateCount);
  SetLength(FEndCounts, LStateCount);
  SetLength(LUsedPublicTokens, LPublicTokenCount);
  SetLength(LDepthObservationCounts, AOrder);
  SetLength(LDepthEndCounts, AOrder);
  FObservationCount := 0;
  LStartTotal := 0;
  LEndTotal := 0;
  for I := 0 to LStateCount - 1 do
  begin
    if FBoundary = wmbWrap then
    begin
      RequireCircularInteger(AStates[I].EmittedTokenIndex, 0,
        LPublicTokenCount - 1, 'circular sequence emitted-token index');
      RequireCircularInteger(AStateCounts[I], 1, High(Integer),
        'circular sequence state observation count');
      RequireCircularInteger(AStartCounts[I], 0, 0,
        'circular sequence start count');
      RequireCircularInteger(AEndCounts[I], 0, 0,
        'circular sequence end count');
    end;
    if CheckedLength(Length(AStates[I].History),
        'sequence state history length') <> LHistorySize then
      raise EWfcSequence.CreateFmt(
        'sequence state %d has %d history items; expected %d',
        [I, Length(AStates[I].History), LHistorySize]);
    if (AStates[I].EmittedTokenIndex < 0) or
        (AStates[I].EmittedTokenIndex >= LPublicTokenCount) then
      raise EWfcSequence.CreateFmt(
        'sequence state emitted-token index is out of bounds [%d: %d]',
        [I, AStates[I].EmittedTokenIndex]);

    SetLength(FStates[I].History, LHistorySize);
    LTokenHistorySeen := False;
    LAllBos := True;
    LLeadingBosCount := 0;
    for J := 0 to LHistorySize - 1 do
    begin
      case AStates[I].History[J].Kind of
        wshBos:
          begin
            if FBoundary = wmbWrap then
              raise EWfcSequence.Create('circular sequence history cannot contain BOS');
            if AStates[I].History[J].TokenIndex <> -1 then
              raise EWfcSequence.CreateFmt(
                'sequence BOS history index must be -1 [%d, %d: %d]',
                [I, J, AStates[I].History[J].TokenIndex]);
            if LTokenHistorySeen then
              raise EWfcSequence.CreateFmt(
                'sequence BOS history must be a canonical prefix [%d, %d]',
                [I, J]);
            Inc(LLeadingBosCount);
          end;
        wshToken:
          begin
            if FBoundary = wmbWrap then
              RequireCircularInteger(AStates[I].History[J].TokenIndex, 0,
                LPublicTokenCount - 1, 'circular sequence history token index');
            if (AStates[I].History[J].TokenIndex < 0) or
                (AStates[I].History[J].TokenIndex >=
                  LPublicTokenCount) then
              raise EWfcSequence.CreateFmt(
                'sequence history token index is out of bounds [%d, %d: %d]',
                [I, J, AStates[I].History[J].TokenIndex]);
            LTokenHistorySeen := True;
            LAllBos := False;
          end;
      else
        raise EWfcSequence.CreateFmt(
          'unknown sequence history kind [%d, %d: %d]',
          [I, J, Ord(AStates[I].History[J].Kind)]);
      end;
      FStates[I].History[J] := AStates[I].History[J];
    end;
    FStates[I].EmittedTokenIndex := AStates[I].EmittedTokenIndex;
    LUsedPublicTokens[AStates[I].EmittedTokenIndex] := True;
    for J := 0 to I - 1 do
      if SequenceStatesEqual(FStates[I], FStates[J]) then
        raise EWfcSequence.CreateFmt(
          'sequence states must be structurally unique [%d, %d]', [J, I]);

    if AStateCounts[I] < 1 then
      raise EWfcSequence.CreateFmt(
        'sequence state observation count must be positive [%d: %d]',
        [I, AStateCounts[I]]);
    if (AStartCounts[I] < 0) or
        (AStartCounts[I] > AStateCounts[I]) then
      raise EWfcSequence.CreateFmt(
        'sequence start count is outside the state observation count [%d: %d > %d]',
        [I, AStartCounts[I], AStateCounts[I]]);
    if (AEndCounts[I] < 0) or
        (AEndCounts[I] > AStateCounts[I]) then
      raise EWfcSequence.CreateFmt(
        'sequence end count is outside the state observation count [%d: %d > %d]',
        [I, AEndCounts[I], AStateCounts[I]]);
    if (AOrder > 1) and (AStartCounts[I] > 0) and not LAllBos then
      raise EWfcSequence.CreateFmt(
        'sequence start count requires an all-BOS history [%d]', [I]);
    if (AOrder > 1) and LAllBos and
        (AStartCounts[I] <> AStateCounts[I]) then
      raise EWfcSequence.CreateFmt(
        'all-BOS sequence state observations must all be starts [%d: %d <> %d]',
        [I, AStartCounts[I], AStateCounts[I]]);

    FStateCounts[I] := AStateCounts[I];
    FStartCounts[I] := AStartCounts[I];
    FEndCounts[I] := AEndCounts[I];
    LDepthObservationCounts[LLeadingBosCount] := CheckedAdd(
      LDepthObservationCounts[LLeadingBosCount], AStateCounts[I],
      'sequence BOS-depth state observation total');
    LDepthEndCounts[LLeadingBosCount] := CheckedAdd(
      LDepthEndCounts[LLeadingBosCount], AEndCounts[I],
      'sequence BOS-depth state end total');
    FObservationCount := CheckedAdd(FObservationCount,
      AStateCounts[I], 'sequence state observation total');
    LStartTotal := CheckedAdd(LStartTotal, AStartCounts[I],
      'sequence start observation total');
    LEndTotal := CheckedAdd(LEndTotal, AEndCounts[I],
      'sequence end observation total');
  end;

  if FObservationCount <> LExpectedObservations then
    raise EWfcSequence.CreateFmt(
      'sequence state observations do not match sample lengths [%d <> %d]',
      [FObservationCount, LExpectedObservations]);
  if (FBoundary = wmbOpen) and (LStartTotal <> LSampleCount) then
    raise EWfcSequence.CreateFmt(
      'sequence start observations do not match sample count [%d <> %d]',
      [LStartTotal, LSampleCount]);
  if (FBoundary = wmbOpen) and (LEndTotal <> LSampleCount) then
    raise EWfcSequence.CreateFmt(
      'sequence end observations do not match sample count [%d <> %d]',
      [LEndTotal, LSampleCount]);
  for I := 0 to LPublicTokenCount - 1 do
    if not LUsedPublicTokens[I] then
      raise EWfcSequence.CreateFmt(
        'sequence public token is not emitted by any state [%d]', [I]);
  for I := 0 to AOrder - 1 do
  begin
    if LDepthObservationCounts[I] <>
        LExpectedDepthObservationCounts[I] then
      raise EWfcSequence.CreateFmt(
        'sequence BOS-depth observations do not match sample lengths [%d: %d <> %d]',
        [I, LDepthObservationCounts[I],
          LExpectedDepthObservationCounts[I]]);
    if LDepthEndCounts[I] <> LExpectedDepthEndCounts[I] then
      raise EWfcSequence.CreateFmt(
        'sequence BOS-depth end counts do not match sample lengths [%d: %d <> %d]',
        [I, LDepthEndCounts[I], LExpectedDepthEndCounts[I]]);
  end;

  { Every non-start observation needs a structural predecessor, and every
    non-end observation needs a structural successor. These checks catch
    malformed persisted models without inventing observed pair counts. }
  for I := 0 to LStateCount - 1 do
  begin
    if FStateCounts[I] > FStartCounts[I] then
    begin
      LHasPredecessor := False;
      for J := 0 to LStateCount - 1 do
        if StatesCompatible(J, I) then
        begin
          LHasPredecessor := True;
          Break;
        end;
      if not LHasPredecessor then
        raise EWfcSequence.CreateFmt(
          'sequence state has non-start observations but no predecessor [%d]',
          [I]);
    end;
    if FStateCounts[I] > FEndCounts[I] then
    begin
      LHasSuccessor := False;
      for J := 0 to LStateCount - 1 do
        if StatesCompatible(I, J) then
        begin
          LHasSuccessor := True;
          Break;
        end;
      if not LHasSuccessor then
        raise EWfcSequence.CreateFmt(
          'sequence state has non-end observations but no successor [%d]',
          [I]);
    end;
  end;
  if FBoundary = wmbWrap then ValidateCircularCounts;
end;

procedure TWfcSequenceModel.ValidateCircularCounts;
var
  I, J, H: Integer;
  LSameHistory: Boolean;
  LIncoming, LOutgoing: Integer;
begin
  { A state is a weighted edge from its history context to the context formed
    by shifting that history and appending its emission. Every context in a
    collection of circles must have equal incoming and outgoing weight.
    Merely finding one compatible predecessor/successor is insufficient.
    This validates aggregate circulation, not the original ordered samples or
    a partition of the multigraph into their recorded individual lengths. }
  for I := 0 to StateCount - 1 do
  begin
    LIncoming := 0;
    LOutgoing := 0;
    for J := 0 to StateCount - 1 do
    begin
      LSameHistory := True;
      for H := 0 to HistorySize - 1 do
        if not HistoryItemsEqual(FStates[I].History[H], FStates[J].History[H]) then
        begin LSameHistory := False; Break; end;
      if LSameHistory then
        LOutgoing := CheckedAdd(LOutgoing, FStateCounts[J],
          'circular sequence outgoing context count');
      if StatesCompatible(J, I) then
        LIncoming := CheckedAdd(LIncoming, FStateCounts[J],
          'circular sequence incoming context count');
    end;
    if LIncoming <> LOutgoing then
      raise EWfcSequence.CreateFmt(
        'circular sequence context counts are not balanced [%d: %d <> %d]',
        [I, LIncoming, LOutgoing]);
  end;
end;

function TWfcSequenceModel.GetModelVersion: Integer;
begin
  if FBoundary = wmbWrap then Result := WFC_SEQUENCE_WRAPPED_MODEL_VERSION
  else Result := WFC_SEQUENCE_MODEL_VERSION;
end;

function TWfcSequenceModel.GetHistorySize: Integer;
begin
  Result := FOrder - 1;
end;

function TWfcSequenceModel.GetSampleCount: Integer;
begin
  Result := Integer(Length(FSampleLengths));
end;

function TWfcSequenceModel.GetPublicTokenCount: Integer;
begin
  Result := Integer(Length(FPublicTokens));
end;

function TWfcSequenceModel.GetStateCount: Integer;
begin
  Result := Integer(Length(FStates));
end;

procedure TWfcSequenceModel.ValidateSampleIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= SampleCount) then
    raise ERangeError.CreateFmt(
      'sequence sample index out of bounds [%d]', [AIndex]);
end;

procedure TWfcSequenceModel.ValidatePublicTokenIndex(
  const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= PublicTokenCount) then
    raise ERangeError.CreateFmt(
      'sequence public-token index out of bounds [%d]', [AIndex]);
end;

procedure TWfcSequenceModel.ValidateStateIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= StateCount) then
    raise ERangeError.CreateFmt(
      'sequence state index out of bounds [%d]', [AIndex]);
end;

procedure TWfcSequenceModel.ValidateHistoryIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= HistorySize) then
    raise ERangeError.CreateFmt(
      'sequence history index out of bounds [%d]', [AIndex]);
end;

function TWfcSequenceModel.SampleLengthAt(
  const ASampleIndex: Integer): Integer;
begin
  ValidateSampleIndex(ASampleIndex);
  Result := FSampleLengths[ASampleIndex];
end;

function TWfcSequenceModel.PublicTokenAt(
  const APublicTokenIndex: Integer): TWfcModelToken;
begin
  ValidatePublicTokenIndex(APublicTokenIndex);
  Result := FPublicTokens[APublicTokenIndex];
end;

function TWfcSequenceModel.FindPublicToken(
  const AToken: TWfcModelToken): Integer;
var
  I: Integer;
begin
  for I := 0 to PublicTokenCount - 1 do
    if FPublicTokens[I] = AToken then
      Exit(I);
  Result := -1;
end;

function TWfcSequenceModel.HistoryItemAt(const AStateIndex,
  AHistoryIndex: Integer): TWfcSequenceHistoryItem;
begin
  ValidateStateIndex(AStateIndex);
  ValidateHistoryIndex(AHistoryIndex);
  Result := FStates[AStateIndex].History[AHistoryIndex];
end;

function TWfcSequenceModel.StateEmittedTokenIndexAt(
  const AStateIndex: Integer): Integer;
begin
  ValidateStateIndex(AStateIndex);
  Result := FStates[AStateIndex].EmittedTokenIndex;
end;

function TWfcSequenceModel.StateLeadingBosCountAt(
  const AStateIndex: Integer): Integer;
begin
  ValidateStateIndex(AStateIndex);
  Result := 0;
  while (Result < HistorySize) and
      (FStates[AStateIndex].History[Result].Kind = wshBos) do
    Inc(Result);
end;

function TWfcSequenceModel.ProjectStateToken(
  const AStateIndex: Integer): TWfcModelToken;
begin
  Result := PublicTokenAt(StateEmittedTokenIndexAt(AStateIndex));
end;

function TWfcSequenceModel.StateObservationCountAt(
  const AStateIndex: Integer): Integer;
begin
  ValidateStateIndex(AStateIndex);
  Result := FStateCounts[AStateIndex];
end;

function TWfcSequenceModel.StartCountAt(
  const AStateIndex: Integer): Integer;
begin
  ValidateStateIndex(AStateIndex);
  Result := FStartCounts[AStateIndex];
end;

function TWfcSequenceModel.EndCountAt(
  const AStateIndex: Integer): Integer;
begin
  ValidateStateIndex(AStateIndex);
  Result := FEndCounts[AStateIndex];
end;

function TWfcSequenceModel.StatesCompatible(const ASourceState,
  ATargetState: Integer): Boolean;
var
  I: Integer;
  LLastHistory: TWfcSequenceHistoryItem;
begin
  ValidateStateIndex(ASourceState);
  ValidateStateIndex(ATargetState);
  if HistorySize = 0 then
    Exit(True);
  for I := 0 to HistorySize - 2 do
    if not HistoryItemsEqual(FStates[ASourceState].History[I + 1],
        FStates[ATargetState].History[I]) then
      Exit(False);
  LLastHistory := FStates[ATargetState].History[HistorySize - 1];
  Result := (LLastHistory.Kind = wshToken) and
    (LLastHistory.TokenIndex =
      FStates[ASourceState].EmittedTokenIndex);
end;

function TWfcSequenceModel.CopySampleLengths: TWfcSequenceSampleLengths;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, SampleCount);
  for I := 0 to SampleCount - 1 do
    Result[I] := FSampleLengths[I];
end;

function TWfcSequenceModel.CopyPublicTokens: TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, PublicTokenCount);
  for I := 0 to PublicTokenCount - 1 do
    Result[I] := FPublicTokens[I];
end;

function TWfcSequenceModel.CopyState(
  const AStateIndex: Integer): TWfcSequenceState;
var
  I: Integer;
begin
  ValidateStateIndex(AStateIndex);
  Result.History := nil;
  SetLength(Result.History, HistorySize);
  for I := 0 to HistorySize - 1 do
    Result.History[I] := FStates[AStateIndex].History[I];
  Result.EmittedTokenIndex :=
    FStates[AStateIndex].EmittedTokenIndex;
end;

function TWfcSequenceModel.CopyStates: TWfcSequenceStates;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, StateCount);
  for I := 0 to StateCount - 1 do
    Result[I] := CopyState(I);
end;

function TWfcSequenceModel.CopyStateCounts: TWfcModelIntegerArray;
begin
  Result := CopyIntegerArray(FStateCounts);
end;

function TWfcSequenceModel.CopyStartCounts: TWfcModelIntegerArray;
begin
  Result := CopyIntegerArray(FStartCounts);
end;

function TWfcSequenceModel.CopyEndCounts: TWfcModelIntegerArray;
begin
  Result := CopyIntegerArray(FEndCounts);
end;

function TWfcSequenceModel.CopyStartStateIndices:
  TWfcSequenceStateIndices;
var
  I: Integer;
  LCount: Integer;
begin
  Result := nil;
  LCount := 0;
  for I := 0 to StateCount - 1 do
    if FStartCounts[I] > 0 then
      Inc(LCount);
  SetLength(Result, LCount);
  LCount := 0;
  for I := 0 to StateCount - 1 do
    if FStartCounts[I] > 0 then
    begin
      Result[LCount] := I;
      Inc(LCount);
    end;
end;

function TWfcSequenceModel.CopyEndStateIndices:
  TWfcSequenceStateIndices;
var
  I: Integer;
  LCount: Integer;
begin
  Result := nil;
  LCount := 0;
  for I := 0 to StateCount - 1 do
    if FEndCounts[I] > 0 then
      Inc(LCount);
  SetLength(Result, LCount);
  LCount := 0;
  for I := 0 to StateCount - 1 do
    if FEndCounts[I] > 0 then
    begin
      Result[LCount] := I;
      Inc(LCount);
    end;
end;

function TWfcSequenceModel.ProjectStateIndices(
  const AStateIndices: TWfcSequenceStateIndices): TWfcModelTokens;
var
  I: Integer;
  LCount: Integer;
begin
  LCount := CheckedLength(Length(AStateIndices),
    'sequence projection state count');
  for I := 0 to LCount - 1 do
    ValidateStateIndex(AStateIndices[I]);
  Result := nil;
  SetLength(Result, LCount);
  for I := 0 to LCount - 1 do
    Result[I] := ProjectStateToken(AStateIndices[I]);
end;

function TWfcSequenceModel.CreateGraphModel(
  const AStateKeys: TWfcModelTokens): TWfcModel;
var
  I: Integer;
  J: Integer;
  LRelations: TWfcModelIntegerArray;
  LSampleShapes: TWfcModelSampleShapes;
  LWeights: TWfcModelIntegerArray;
begin
  if Length(AStateKeys) <> StateCount then
    raise EWfcSequence.CreateFmt(
      'sequence graph-key count must match state count [%d <> %d]',
      [Length(AStateKeys), StateCount]);
  for I := 0 to StateCount - 1 do
  begin
    if not WfcModelTokenIsValid(AStateKeys[I]) then
      raise EWfcSequence.CreateFmt(
        'sequence graph key must be nonempty, well-formed UTF-8 [%d]',
        [I]);
    for J := 0 to I - 1 do
      if AStateKeys[I] = AStateKeys[J] then
        raise EWfcSequence.CreateFmt(
          'sequence graph keys must be unique [%d, %d]', [J, I]);
  end;

  SetLength(LSampleShapes, SampleCount);
  for I := 0 to SampleCount - 1 do
    LSampleShapes[I] := MakeWfcModelSampleShape(
      FSampleLengths[I], 1);
  LWeights := CopyStateCounts;
  SetLength(LRelations, CheckedGraphRelationLength(StateCount));
  for I := 0 to StateCount - 1 do
    for J := 0 to StateCount - 1 do
      if StatesCompatible(I, J) then
      begin
        LRelations[GraphRelationIndex(wmdEast, I, J,
          StateCount)] := 1;
        LRelations[GraphRelationIndex(wmdWest, J, I,
          StateCount)] := 1;
      end;
  Result := TWfcModel.Create(1, LSampleShapes, FBoundary, wmsNone,
    [wmdEast, wmdWest], AStateKeys, LWeights, LRelations);
end;

end.
