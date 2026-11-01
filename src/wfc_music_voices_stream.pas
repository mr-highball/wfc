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
unit wfc_music_voices_stream;

{$mode delphi}{$H+}

interface

uses
  SysUtils, wfc, wfc_model, wfc_sequence, wfc_sequence_graph,
  wfc_music_ensemble, wfc_music_voices_graph, wfc_music_arrangement;

const
  WFC_MUSIC_VOICES_STREAM_VERSION = 2;
  WFC_MUSIC_VOICES_SEGMENT_SIGNATURE_VERSION = 2;

type
  EWfcMusicVoicesStream = class(EWfcMusicVoicesGraph);

  TWfcMusicVoicesStreamConfig = record
    Graph: TWfcMusicVoicesGraphConfig;
    QuantumTicks, SegmentCellCount: Integer;
    { Zero plans just the returned segment. A positive count bounds a private
      future window; only SegmentCellCount cells are committed per Next. }
    LookaheadCellCount: Integer;
    RequestedTicks: TWfcMusicArrangementWide;
    Seed: TGraphSeed;
    Rounding: TWfcMusicArrangementRounding;
    RequireObservedEnd: Boolean;
    Search: TGraphNegotiationOptions;
  end;

  TWfcMusicVoicesStreamFrontier = record
    HasPrevious: Boolean;
    EndTick: TWfcMusicArrangementWide;
    StateIndices: TWfcSequenceStateIndices;
    Tokens: TWfcModelTokens;
  end;
  TWfcMusicVoicesStreamConstraint = record
    ModelIndex: Integer;
    Position: TWfcMusicArrangementWide;
    AllowedTokens: TWfcModelTokens;
  end;
  TWfcMusicVoicesStreamConstraints = array of TWfcMusicVoicesStreamConstraint;

  { Detached immutable result. Model indices are H=0, R=1, voice i=2+i.
    Coverage suppliers are diagnostic existential witnesses, not voice states.
    A continued segment can begin with holds and is not a standalone score. }
  TWfcMusicVoicesSegment = class
  private
    FIndex, FStartTick: TWfcMusicArrangementWide;
    FCellCount, FPlanningCellCount, FQuantumTicks, FModelCount: Integer;
    FSeed: TGraphSeed;
    FFinalSegment: Boolean;
    FGenerated: TWfcMusicVoicesGenerated;
    FSignature: Cardinal;
    constructor Create(const AIndex, AStartTick: TWfcMusicArrangementWide;
      const ACellCount, APlanningCellCount: Integer; const ASeed: TGraphSeed;
      const AFinal: Boolean; const AConfig: TWfcMusicVoicesStreamConfig;
      const AGenerated: TWfcMusicVoicesGenerated);
  public
    function CopyGenerated(const AModelIndex: Integer): TWfcGeneratedSequenceSegment;
    function CopyFrames: TWfcMusicEnsembleFrames;
    function CopyCoverage: TWfcMusicVoicesCoverageWitnesses;
    property Index: TWfcMusicArrangementWide read FIndex;
    property StartTick: TWfcMusicArrangementWide read FStartTick;
    property CellCount: Integer read FCellCount;
    property PlanningCellCount: Integer read FPlanningCellCount;
    property QuantumTicks: Integer read FQuantumTicks;
    property ModelCount: Integer read FModelCount;
    property Seed: TGraphSeed read FSeed;
    property FinalSegment: Boolean read FFinalSegment;
    property Signature: Cardinal read FSignature;
  end;

  { Borrows immutable sequence models; copies config arrays. Storage is a
    frontier linear in model count, one temporary segment graph, and explicit
    sparse future constraints. Previously published history is never retained
    or repaired. Calls are synchronous, non-reentrant, and locally bounded by
    the caller's segment/lookahead/search settings, not by a duration policy.
    Provisional suffixes are independently validated but never retained. }
  TWfcMusicVoicesStream = class
  strict private
    FConfig: TWfcMusicVoicesStreamConfig;
    FActualTicks, FProducedTicks, FNextIndex: TWfcMusicArrangementWide;
    FFrontier: TWfcMusicVoicesStreamFrontier;
    FConstraints: TWfcMusicVoicesStreamConstraints;
    FStatus: TWfcMusicArrangementStatus;
    FFailure: String;
    FRunning: Boolean;
    procedure CheckModelIndex(const AModelIndex: Integer);
    function ModelAt(const AModelIndex: Integer): TWfcSequenceModel;
    function GetRequestedTicks: TWfcMusicArrangementWide;
    function GetRemainingTicks: TWfcMusicArrangementWide;
    function GetSegmentCount: TWfcMusicArrangementWide;
    procedure RequireFuturePosition(const AModelIndex: Integer;
      const APosition: TWfcMusicArrangementWide);
    procedure ApplyConstraints(const AGraph: TGraph; const ACellCount: Integer);
    procedure ValidateConstraints(const ALayers: TWfcMusicVoicesGeneratedLayers;
      const ACellCount: Integer);
    function RemainingConstraints(const AEndTick: TWfcMusicArrangementWide):
      TWfcMusicVoicesStreamConstraints;
    procedure SetFailure(const AMessage: String);
  protected
    { Borrowed graph over ACellCount planned cells, valid only during this call.
      Add restrictions and never
      retain it. Independent capture checks paths, original boundaries, caller
      domains, rhythm, ranges, pair constraints, collective harmony and coverage.
      Stored global constraints are checked separately even if the hook clears
      graph domains. Changing the derived seed is rejected. }
    procedure ConfigureSegment(const AIndex, AStartTick: TWfcMusicArrangementWide;
      const ACellCount: Integer; const AGraph: TGraph); virtual;
  public
    constructor Create(const AConfig: TWfcMusicVoicesStreamConfig);
    { Optional per-call cap: useful when future application domains are not yet
      available. Zero uses the config bound. A nonzero cap must fit the actual
      returned prefix and cannot enlarge the configured planning window. }
    function Next(out ASegment: TWfcMusicVoicesSegment;
      out AReport: TGraphNegotiationReport;
      const AMaximumPlanningCells: Integer = 0): TWfcMusicArrangementStep;
    procedure Cancel;
    function CopyFrontier: TWfcMusicVoicesStreamFrontier;
    function CopyConstraints: TWfcMusicVoicesStreamConstraints;
    function IntersectAllowedTokens(const AModelIndex: Integer;
      const APosition: TWfcMusicArrangementWide; const ATokens: TWfcModelTokens):
      TWfcMusicVoicesStream;
    function ClearAllowedTokens(const AModelIndex: Integer;
      const APosition: TWfcMusicArrangementWide): TWfcMusicVoicesStream;
    property RequestedTicks: TWfcMusicArrangementWide read GetRequestedTicks;
    property ActualTicks: TWfcMusicArrangementWide read FActualTicks;
    property ProducedTicks: TWfcMusicArrangementWide read FProducedTicks;
    property RemainingTicks: TWfcMusicArrangementWide read GetRemainingTicks;
    property SegmentCount: TWfcMusicArrangementWide read GetSegmentCount;
    property NextIndex: TWfcMusicArrangementWide read FNextIndex;
    property Status: TWfcMusicArrangementStatus read FStatus;
    property Failure: String read FFailure;
  end;

function DefaultWfcMusicVoicesStreamConfig(
  const AGraphConfig: TWfcMusicVoicesGraphConfig; const AQuantumTicks: Integer;
  const ARequestedTicks: TWfcMusicArrangementWide; const ASeed: TGraphSeed):
  TWfcMusicVoicesStreamConfig;

implementation

procedure StreamError(const AMessage: String);
begin
  raise EWfcMusicVoicesStream.Create('voices stream: ' + AMessage);
end;

procedure CheckWide(const AValue: TWfcMusicArrangementWide; const AName: String);
begin
  if (AValue < 0) or (AValue > WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER) then
    StreamError(AName + ' exceeds the exact portable integer range');
  {$IFDEF PAS2JS}
  if AValue <> Trunc(AValue) then
    StreamError(AName + ' must be an exact integer');
  {$ENDIF}
end;

procedure CheckIndex(const AIndex, ACount: Integer);
begin
  if (AIndex < 0) or (AIndex >= ACount) then StreamError('unknown model index');
  {$IFDEF PAS2JS}
  if AIndex <> Trunc(AIndex) then StreamError('model index must be an integer');
  {$ENDIF}
end;

procedure CheckInteger(const AValue, AMinimum, AMaximum:
  TWfcMusicArrangementWide; const AName: String);
begin
  CheckWide(AValue, AName);
  if (AValue < AMinimum) or (AValue > AMaximum) then
    StreamError(AName + ' is outside its supported integer range');
end;

function CopyTokens(const ATokens: TWfcModelTokens): TWfcModelTokens;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ATokens));
  for I := 0 to High(ATokens) do Result[I] := ATokens[I];
end;

function CopySegment(const AValue: TWfcGeneratedSequenceSegment):
  TWfcGeneratedSequenceSegment;
var I: Integer;
begin
  Result := Default(TWfcGeneratedSequenceSegment);
  Result.Boundary := AValue.Boundary;
  Result.Tokens := CopyTokens(AValue.Tokens);
  SetLength(Result.StateIndices, Length(AValue.StateIndices));
  for I := 0 to High(AValue.StateIndices) do
    Result.StateIndices[I] := AValue.StateIndices[I];
end;

procedure HashByte(var AHash: Cardinal; const AValue: Byte);
{$PUSH}{$Q-}
var LValue: Cardinal;
begin
  AHash := AHash xor Cardinal(AValue);
  LValue := AHash;
  AHash := (LValue + (LValue shl 1) + (LValue shl 4) +
    (LValue shl 7) + (LValue shl 8) + (LValue shl 24)) and Cardinal($FFFFFFFF);
end;
{$POP}

procedure HashWide(var AHash: Cardinal; AValue: TWfcMusicArrangementWide);
var I: Integer;
begin
  for I := 0 to 7 do
  begin
    HashByte(AHash, Byte(AValue mod 256));
    AValue := AValue div 256;
  end;
end;

procedure HashText(var AHash: Cardinal; const AValue: TWfcModelToken);
var I: Integer;
begin
  HashWide(AHash, Length(AValue));
  for I := 1 to Length(AValue) do HashByte(AHash, Byte(Ord(AValue[I])));
end;

function CopyCoverageWitnesses(const AValue: TWfcMusicVoicesCoverageWitnesses):
  TWfcMusicVoicesCoverageWitnesses;
var I, J: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValue));
  for I := 0 to High(AValue) do
  begin
    Result[I].PitchClass := AValue[I].PitchClass;
    SetLength(Result[I].Suppliers, Length(AValue[I].Suppliers));
    for J := 0 to High(AValue[I].Suppliers) do
      Result[I].Suppliers[J] := AValue[I].Suppliers[J];
  end;
end;

constructor TWfcMusicVoicesSegment.Create(
  const AIndex, AStartTick: TWfcMusicArrangementWide;
  const ACellCount, APlanningCellCount: Integer; const ASeed: TGraphSeed;
  const AFinal: Boolean; const AConfig: TWfcMusicVoicesStreamConfig;
  const AGenerated: TWfcMusicVoicesGenerated);
var I, J, K: Integer;
begin
  inherited Create;
  FIndex := AIndex;
  FStartTick := AStartTick;
  FCellCount := ACellCount;
  FPlanningCellCount := APlanningCellCount;
  FQuantumTicks := AConfig.QuantumTicks;
  FModelCount := Length(AGenerated.Layers);
  FSeed := ASeed;
  FFinalSegment := AFinal;
  SetLength(FGenerated.Layers, FModelCount);
  FGenerated.Coverage := CopyCoverageWitnesses(AGenerated.Coverage);
  for I := 0 to High(FGenerated.Coverage) do
    SetLength(FGenerated.Coverage[I].Suppliers, ACellCount);
  FSignature := Cardinal(2166136261);
  HashWide(FSignature, WFC_MUSIC_VOICES_SEGMENT_SIGNATURE_VERSION);
  HashWide(FSignature, WFC_MUSIC_VOICES_STREAM_VERSION);
  HashWide(FSignature, WFC_MUSIC_VOICES_GRAPH_VERSION);
  HashWide(FSignature, WFC_SEQUENCE_SEGMENT_VERSION);
  HashWide(FSignature, WFC_MUSIC_ENSEMBLE_TOKEN_VERSION);
  HashWide(FSignature, FIndex);
  HashWide(FSignature, FStartTick);
  HashWide(FSignature, FCellCount);
  HashWide(FSignature, FPlanningCellCount);
  HashWide(FSignature, FQuantumTicks);
  HashWide(FSignature, Cardinal(FSeed));
  HashWide(FSignature, Ord(FFinalSegment));
  HashWide(FSignature, FModelCount);
  HashWide(FSignature, AConfig.Graph.StepsPerOctave);
  HashWide(FSignature, Ord(AConfig.Graph.HarmonyMode));
  for I := 0 to High(AConfig.Graph.Voices) do
  begin
    HashWide(FSignature, AConfig.Graph.Voices[I].MinPitch);
    HashWide(FSignature, AConfig.Graph.Voices[I].MaxPitch);
  end;
  HashWide(FSignature, Length(AConfig.Graph.PairConstraints));
  for I := 0 to High(AConfig.Graph.PairConstraints) do
  begin
    HashWide(FSignature, AConfig.Graph.PairConstraints[I].LowerVoice);
    HashWide(FSignature, AConfig.Graph.PairConstraints[I].UpperVoice);
    //Gaps may be signed. Bias into a nonnegative portable integer domain.
    HashWide(FSignature, TWfcMusicArrangementWide(
      AConfig.Graph.PairConstraints[I].MinGap) - Low(Integer));
    HashWide(FSignature, TWfcMusicArrangementWide(
      AConfig.Graph.PairConstraints[I].MaxGap) - Low(Integer));
    HashWide(FSignature, Ord(AConfig.Graph.PairConstraints[I].RestPolicy));
  end;
  { Preserve the original no-composition replay identity. Opt-in compositions
    also bind declared names and relations, not only selected state tokens. }
  if (Length(AConfig.Graph.Auxiliary) > 0) or (Length(AConfig.Graph.Projections) > 0) then
  begin
    HashWide(FSignature, WFC_MUSIC_VOICES_COMPOSITION_VERSION);
    HashWide(FSignature, Length(AConfig.Graph.Auxiliary));
    for I := 0 to High(AConfig.Graph.Auxiliary) do
      HashText(FSignature, AConfig.Graph.Auxiliary[I].Name);
    HashWide(FSignature, Length(AConfig.Graph.Projections));
    for I := 0 to High(AConfig.Graph.Projections) do
    begin
      HashWide(FSignature, AConfig.Graph.Projections[I].SourceModelIndex);
      HashWide(FSignature, AConfig.Graph.Projections[I].TargetModelIndex);
      HashWide(FSignature, Length(AConfig.Graph.Projections[I].Rules));
      for J := 0 to High(AConfig.Graph.Projections[I].Rules) do
      begin
        HashText(FSignature, AConfig.Graph.Projections[I].Rules[J].TargetToken);
        HashWide(FSignature, Length(AConfig.Graph.Projections[I].Rules[J].SourceTokens));
        for K := 0 to High(AConfig.Graph.Projections[I].Rules[J].SourceTokens) do
          HashText(FSignature, AConfig.Graph.Projections[I].Rules[J].SourceTokens[K]);
      end;
    end;
  end;
  for I := 0 to FModelCount - 1 do
  begin
    FGenerated.Layers[I] := CopySegment(AGenerated.Layers[I]);
    SetLength(FGenerated.Layers[I].Tokens, ACellCount);
    SetLength(FGenerated.Layers[I].StateIndices, ACellCount);
    if APlanningCellCount > ACellCount then
    begin
      FGenerated.Layers[I].Boundary.RequireObservedEnd := False;
      Inc(FGenerated.Layers[I].Boundary.MinimumFollowingCells,
        APlanningCellCount - ACellCount);
    end;
    HashWide(FSignature, Ord(FGenerated.Layers[I].Boundary.HasPrevious));
    if FGenerated.Layers[I].Boundary.HasPrevious then
      HashWide(FSignature, FGenerated.Layers[I].Boundary.PreviousState);
    HashWide(FSignature, Ord(FGenerated.Layers[I].Boundary.RequireObservedEnd));
    HashWide(FSignature, FGenerated.Layers[I].Boundary.MinimumFollowingCells);
    for J := 0 to ACellCount - 1 do
    begin
      HashWide(FSignature, AGenerated.Layers[I].StateIndices[J]);
      HashText(FSignature, AGenerated.Layers[I].Tokens[J]);
    end;
  end;
  HashWide(FSignature, Length(FGenerated.Coverage));
  for I := 0 to High(FGenerated.Coverage) do
  begin
    HashWide(FSignature, FGenerated.Coverage[I].PitchClass);
    for J := 0 to ACellCount - 1 do
      HashWide(FSignature, FGenerated.Coverage[I].Suppliers[J] + 1);
  end;
end;

function TWfcMusicVoicesSegment.CopyGenerated(const AModelIndex: Integer):
  TWfcGeneratedSequenceSegment;
begin
  CheckIndex(AModelIndex, FModelCount);
  Result := CopySegment(FGenerated.Layers[AModelIndex]);
end;

function TWfcMusicVoicesSegment.CopyFrames: TWfcMusicEnsembleFrames;
var I, J: Integer; LVoice: TWfcMusicEnsembleFrame;
begin
  Result := nil;
  SetLength(Result, FCellCount);
  for I := 0 to FCellCount - 1 do
  begin
    SetLength(Result[I].Voices, FModelCount - 2);
    for J := 2 to FModelCount - 1 do
    begin
      LVoice := DecodeWfcMusicEnsembleFrame(FGenerated.Layers[J].Tokens[I]);
      Result[I].Voices[J - 2] := LVoice.Voices[0];
    end;
  end;
end;

function TWfcMusicVoicesSegment.CopyCoverage: TWfcMusicVoicesCoverageWitnesses;
begin
  Result := CopyCoverageWitnesses(FGenerated.Coverage);
end;

function DefaultWfcMusicVoicesStreamConfig(
  const AGraphConfig: TWfcMusicVoicesGraphConfig; const AQuantumTicks: Integer;
  const ARequestedTicks: TWfcMusicArrangementWide; const ASeed: TGraphSeed):
  TWfcMusicVoicesStreamConfig;
begin
  Result := Default(TWfcMusicVoicesStreamConfig);
  Result.Graph := CopyWfcMusicVoicesGraphConfig(AGraphConfig);
  Result.QuantumTicks := AQuantumTicks;
  Result.SegmentCellCount := 16;
  Result.RequestedTicks := ARequestedTicks;
  Result.Seed := ASeed;
  Result.Rounding := wmarExact;
  Result.Search := DefaultGraphNegotiationOptions;
  Result.Search.SolveOptions.MaxBacktracks := 256;
  Result.Search.MaxPassBacktracks := 16;
end;

constructor TWfcMusicVoicesStream.Create(const AConfig: TWfcMusicVoicesStreamConfig);
var I: Integer;
begin
  inherited Create;
  CheckWide(AConfig.RequestedTicks, 'requested ticks');
  CheckInteger(AConfig.QuantumTicks, 1, High(Integer), 'quantum ticks');
  CheckInteger(AConfig.SegmentCellCount, 1, High(Integer), 'segment cell count');
  CheckInteger(AConfig.LookaheadCellCount, 0, High(Integer), 'lookahead cell count');
  if (AConfig.LookaheadCellCount <> 0) and
      (AConfig.LookaheadCellCount < AConfig.SegmentCellCount) then
    StreamError('lookahead cell count must fit the returned segment');
  if AConfig.SegmentCellCount > High(Integer) div AConfig.QuantumTicks then
    StreamError('local segment ticks exceed Integer capacity');
  if AConfig.LookaheadCellCount > High(Integer) div AConfig.QuantumTicks then
    StreamError('local planning ticks exceed Integer capacity');
  CheckInteger(Ord(AConfig.Rounding), Ord(Low(TWfcMusicArrangementRounding)),
    Ord(High(TWfcMusicArrangementRounding)), 'rounding mode');
  CheckInteger(AConfig.Seed, 0, Cardinal($FFFFFFFF), 'seed');
  CheckInteger(AConfig.Search.SolveOptions.MaxBacktracks, 0, High(Integer),
    'local search allowance');
  CheckInteger(AConfig.Search.MaxPassBacktracks, 0, High(Integer),
    'pass search allowance');
  {$IFDEF PAS2JS}
  if ((AConfig.RequireObservedEnd <> True) and
      (AConfig.RequireObservedEnd <> False)) or
      ((AConfig.Search.SolveOptions.CaptureTrace <> True) and
      (AConfig.Search.SolveOptions.CaptureTrace <> False)) then
    StreamError('endpoint and trace flags must be Boolean');
  {$ENDIF}
  FActualTicks := ResolveWfcMusicArrangementTicks(AConfig.RequestedTicks,
    AConfig.QuantumTicks, AConfig.Rounding);
  if FActualTicks < 1 then StreamError('rounded duration must be positive');
  ValidateWfcMusicVoicesGraphConfig(AConfig.Graph);
  FConfig := AConfig;
  FConfig.Graph := CopyWfcMusicVoicesGraphConfig(AConfig.Graph);
  SetLength(FFrontier.StateIndices, WfcMusicVoicesModelCount(FConfig.Graph));
  SetLength(FFrontier.Tokens, Length(FFrontier.StateIndices));
  for I := 0 to High(FFrontier.StateIndices) do FFrontier.StateIndices[I] := -1;
  FStatus := wmasReady;
end;

procedure TWfcMusicVoicesStream.CheckModelIndex(const AModelIndex: Integer);
begin
  CheckIndex(AModelIndex, Length(FFrontier.StateIndices));
end;

function TWfcMusicVoicesStream.ModelAt(const AModelIndex: Integer): TWfcSequenceModel;
begin
  CheckModelIndex(AModelIndex);
  Result := WfcMusicVoicesModelAt(FConfig.Graph, AModelIndex);
end;

function TWfcMusicVoicesStream.GetRequestedTicks: TWfcMusicArrangementWide;
begin Result := FConfig.RequestedTicks end;

function TWfcMusicVoicesStream.GetRemainingTicks: TWfcMusicArrangementWide;
begin Result := FActualTicks - FProducedTicks end;

function TWfcMusicVoicesStream.GetSegmentCount: TWfcMusicArrangementWide;
var LCells: TWfcMusicArrangementWide;
begin
  LCells := FActualTicks div FConfig.QuantumTicks;
  Result := LCells div FConfig.SegmentCellCount;
  if LCells mod FConfig.SegmentCellCount <> 0 then Inc(Result);
end;

procedure TWfcMusicVoicesStream.RequireFuturePosition(
  const AModelIndex: Integer; const APosition: TWfcMusicArrangementWide);
begin
  CheckModelIndex(AModelIndex);
  CheckWide(APosition, 'constraint position');
  if FRunning then StreamError('cannot edit constraints during Next');
  if FStatus in [wmasCompleted, wmasCancelled, wmasFailed] then
    StreamError('cannot edit a terminal stream');
  if (APosition < FProducedTicks div FConfig.QuantumTicks) or
      (APosition >= FActualTicks div FConfig.QuantumTicks) then
    StreamError('constraint position must address an unproduced cell');
end;

function TWfcMusicVoicesStream.IntersectAllowedTokens(
  const AModelIndex: Integer;
  const APosition: TWfcMusicArrangementWide; const ATokens: TWfcModelTokens):
  TWfcMusicVoicesStream;
var I, J, LCount: Integer; LNew: TWfcMusicVoicesStreamConstraint;
  LPrepared: TWfcMusicVoicesStreamConstraints;
begin
  RequireFuturePosition(AModelIndex, APosition);
  for I := 0 to High(ATokens) do
  begin
    if ModelAt(AModelIndex).FindPublicToken(ATokens[I]) < 0 then
      StreamError('constraint token is not in the selected model');
    for J := 0 to I - 1 do
      if ATokens[J] = ATokens[I] then StreamError('duplicate constraint token');
  end;
  LNew.ModelIndex := AModelIndex;
  LNew.Position := APosition;
  LNew.AllowedTokens := CopyTokens(ATokens);
  LCount := Length(FConstraints);
  if LCount = High(Integer) then StreamError('constraint storage is full');
  SetLength(LPrepared, LCount + 1);
  for I := 0 to LCount - 1 do LPrepared[I] := FConstraints[I];
  LPrepared[LCount] := LNew;
  FConstraints := LPrepared;
  Result := Self;
end;

function TWfcMusicVoicesStream.ClearAllowedTokens(
  const AModelIndex: Integer;
  const APosition: TWfcMusicArrangementWide): TWfcMusicVoicesStream;
var I, J: Integer; LPrepared: TWfcMusicVoicesStreamConstraints;
begin
  RequireFuturePosition(AModelIndex, APosition);
  LPrepared := nil;
  J := 0;
  for I := 0 to High(FConstraints) do
    if (FConstraints[I].ModelIndex <> AModelIndex) or
        (FConstraints[I].Position <> APosition) then Inc(J);
  SetLength(LPrepared, J);
  J := 0;
  for I := 0 to High(FConstraints) do
    if (FConstraints[I].ModelIndex <> AModelIndex) or
        (FConstraints[I].Position <> APosition) then
    begin
      LPrepared[J] := FConstraints[I];
      Inc(J);
    end;
  FConstraints := LPrepared;
  Result := Self;
end;

function TWfcMusicVoicesStream.CopyConstraints:
  TWfcMusicVoicesStreamConstraints;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FConstraints));
  for I := 0 to High(FConstraints) do
  begin
    Result[I] := FConstraints[I];
    Result[I].AllowedTokens := CopyTokens(FConstraints[I].AllowedTokens);
  end;
end;

function TWfcMusicVoicesStream.CopyFrontier: TWfcMusicVoicesStreamFrontier;
var I: Integer;
begin
  Result := Default(TWfcMusicVoicesStreamFrontier);
  Result.HasPrevious := FFrontier.HasPrevious;
  Result.EndTick := FFrontier.EndTick;
  Result.Tokens := CopyTokens(FFrontier.Tokens);
  SetLength(Result.StateIndices, Length(FFrontier.StateIndices));
  for I := 0 to High(Result.StateIndices) do
    Result.StateIndices[I] := FFrontier.StateIndices[I];
end;

procedure TWfcMusicVoicesStream.ApplyConstraints(
  const AGraph: TGraph; const ACellCount: Integer);
var I: Integer; LStart, LLocal: TWfcMusicArrangementWide;
begin
  LStart := FProducedTicks div FConfig.QuantumTicks;
  for I := 0 to High(FConstraints) do
  begin
    LLocal := FConstraints[I].Position - LStart;
    if (LLocal >= 0) and (LLocal < ACellCount) then
      IntersectSequenceAllowedTokens(ModelAt(FConstraints[I].ModelIndex),
        AGraph.PassGraph[Ord(FConstraints[I].ModelIndex)], Integer(LLocal),
        FConstraints[I].AllowedTokens);
  end;
end;

function TWfcMusicVoicesStream.RemainingConstraints(
  const AEndTick: TWfcMusicArrangementWide): TWfcMusicVoicesStreamConstraints;
var I, J: Integer; LPosition: TWfcMusicArrangementWide;
begin
  LPosition := AEndTick div FConfig.QuantumTicks;
  Result := nil;
  J := 0;
  for I := 0 to High(FConstraints) do
    if FConstraints[I].Position >= LPosition then Inc(J);
  SetLength(Result, J);
  J := 0;
  for I := 0 to High(FConstraints) do
    if FConstraints[I].Position >= LPosition then
    begin
      Result[J] := FConstraints[I];
      Inc(J);
    end;
end;

procedure TWfcMusicVoicesStream.ConfigureSegment(
  const AIndex, AStartTick: TWfcMusicArrangementWide;
  const ACellCount: Integer; const AGraph: TGraph);
begin
end;

procedure TWfcMusicVoicesStream.ValidateConstraints(
  const ALayers: TWfcMusicVoicesGeneratedLayers; const ACellCount: Integer);
var I, J: Integer; LLocal: TWfcMusicArrangementWide; LFound: Boolean;
begin
  for I := 0 to High(FConstraints) do
  begin
    LLocal := FConstraints[I].Position - FProducedTicks div FConfig.QuantumTicks;
    if (LLocal < 0) or (LLocal >= ACellCount) then Continue;
    LFound := False;
    for J := 0 to High(FConstraints[I].AllowedTokens) do
      if FConstraints[I].AllowedTokens[J] =
          ALayers[FConstraints[I].ModelIndex].Tokens[Integer(LLocal)] then
        LFound := True;
    if not LFound then StreamError('captured token violates a global constraint');
  end;
end;

procedure TWfcMusicVoicesStream.SetFailure(const AMessage: String);
begin FStatus := wmasFailed; FFailure := AMessage end;

procedure TWfcMusicVoicesStream.Cancel;
begin
  if FStatus in [wmasReady, wmasActive] then FStatus := wmasCancelled;
end;

function TWfcMusicVoicesStream.Next(out ASegment: TWfcMusicVoicesSegment;
  out AReport: TGraphNegotiationReport;
  const AMaximumPlanningCells: Integer): TWfcMusicArrangementStep;
var
  LGraph: TGraph;
  LBoundaries: TWfcMusicVoicesBoundaries;
  LGenerated: TWfcMusicVoicesGenerated;
  LCapture: TWfcMusicVoicesValidationReport;
  I, LCells, LPlanningCells: Integer;
  LRemainingCells, LEndTick: TWfcMusicArrangementWide;
  LFinal, LPlanningFinal: Boolean;
  LSeed: TGraphSeed;
  LCandidate: TWfcMusicVoicesSegment;
  LFrontier: TWfcMusicVoicesStreamFrontier;
  LRemainingConstraints: TWfcMusicVoicesStreamConstraints;
begin
  ASegment := nil;
  AReport := Default(TGraphNegotiationReport);
  if FRunning then StreamError('Next cannot be reentered');
  case FStatus of
    wmasCompleted: Exit(wmaspCompleted);
    wmasCancelled: Exit(wmaspCancelled);
    wmasFailed: Exit(wmaspFailed);
  end;
  LGraph := nil;
  LCandidate := nil;
  FRunning := True;
  FStatus := wmasActive;
  try
    try
      LRemainingCells := RemainingTicks div FConfig.QuantumTicks;
      LCells := FConfig.SegmentCellCount;
      if LRemainingCells < LCells then LCells := Integer(LRemainingCells);
      LFinal := LRemainingCells = LCells;
      CheckInteger(AMaximumPlanningCells, 0, High(Integer), 'planning cap');
      LPlanningCells := FConfig.LookaheadCellCount;
      if LPlanningCells = 0 then LPlanningCells := FConfig.SegmentCellCount;
      if (AMaximumPlanningCells > 0) and (AMaximumPlanningCells < LPlanningCells) then
        LPlanningCells := AMaximumPlanningCells;
      if LRemainingCells < LPlanningCells then LPlanningCells := Integer(LRemainingCells);
      if LPlanningCells < LCells then StreamError('planning cap does not fit the returned prefix');
      LPlanningFinal := LRemainingCells = LPlanningCells;
      LSeed := WfcMusicArrangementSectionSeed(FConfig.Seed, FNextIndex);
      SetLength(LBoundaries, Length(FFrontier.StateIndices));
      for I := 0 to High(LBoundaries) do
        if FFrontier.HasPrevious then
          LBoundaries[I] := MakeWfcSequenceContinuingSegmentBoundary(
            FFrontier.StateIndices[I], LPlanningFinal and FConfig.RequireObservedEnd)
        else LBoundaries[I] := MakeWfcSequenceInitialSegmentBoundary(
          LPlanningFinal and FConfig.RequireObservedEnd);
      LGraph := BuildWfcMusicVoicesSegmentGraph(FConfig.Graph, LPlanningCells, LSeed,
        LBoundaries);
      ApplyConstraints(LGraph, LPlanningCells);
      LGraph.SwitchToPass(WfcMusicVoicesPassLabel(0));
      ConfigureSegment(FNextIndex, FProducedTicks, LPlanningCells, LGraph);
      if FStatus = wmasCancelled then Exit(wmaspCancelled);
      if LGraph.Seed <> LSeed then
        StreamError('application hook changed the derived segment seed');
      if (LGraph.Dimension.Width <> TGraphCoordinate(LPlanningCells)) or
          (LGraph.Dimension.Height <> 1) or (LGraph.Dimension.Depth <> 1) then
        StreamError('application hook changed the segment shape');
      if not LGraph.TrySolveNegotiated(FConfig.Search, AReport) then
      begin
        SetFailure('finite segment search failed (status ' +
          IntToStr(Ord(AReport.Status)) + ') at segment ' + IntToStr(FNextIndex));
        Exit(wmaspFailed);
      end;
      if FStatus = wmasCancelled then Exit(wmaspCancelled);
      if not CaptureSolvedWfcMusicVoices(FConfig.Graph, LGraph, LBoundaries,
          LGenerated, LCapture) then
        StreamError('invalid captured voices: ' + LCapture.Issue.Detail);
      ValidateConstraints(LGenerated.Layers, LPlanningCells);
      LCandidate := TWfcMusicVoicesSegment.Create(FNextIndex, FProducedTicks,
        LCells, LPlanningCells, LSeed, LFinal, FConfig, LGenerated);
      LEndTick := FProducedTicks + LCells * FConfig.QuantumTicks;
      LFrontier := Default(TWfcMusicVoicesStreamFrontier);
      SetLength(LFrontier.StateIndices, Length(LBoundaries));
      SetLength(LFrontier.Tokens, Length(LBoundaries));
      for I := 0 to High(LBoundaries) do
      begin
        LFrontier.StateIndices[I] := LGenerated.Layers[I].StateIndices[LCells - 1];
        LFrontier.Tokens[I] := LGenerated.Layers[I].Tokens[LCells - 1];
      end;
      LRemainingConstraints := RemainingConstraints(LEndTick);
      //All allocations and independent proof precede public progress mutation.
      FFrontier.StateIndices := LFrontier.StateIndices;
      FFrontier.Tokens := LFrontier.Tokens;
      FFrontier.HasPrevious := True;
      FFrontier.EndTick := LEndTick;
      FProducedTicks := LEndTick;
      Inc(FNextIndex);
      FConstraints := LRemainingConstraints;
      ASegment := LCandidate;
      LCandidate := nil;
      if LFinal then FStatus := wmasCompleted;
      Result := wmaspProduced;
    except
      on E: Exception do
      begin
        SetFailure(E.Message);
        raise;
      end;
    end;
  finally
    LCandidate.Free;
    LGraph.Free;
    FRunning := False;
  end;
end;

end.
