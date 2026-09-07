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
unit wfc_music_ensemble_stream;

{$mode delphi}{$H+}

interface

uses
  SysUtils, wfc, wfc_model, wfc_sequence, wfc_sequence_graph,
  wfc_music_ensemble, wfc_music_ensemble_graph, wfc_music_ensemble_passes,
  wfc_music_arrangement;

const
  WFC_MUSIC_ENSEMBLE_STREAM_VERSION = 1;
  WFC_MUSIC_ENSEMBLE_SEGMENT_SIGNATURE_VERSION = 1;

type
  EWfcMusicEnsembleStream = class(EWfcMusicEnsembleGraph);

  TWfcMusicEnsembleStreamConfig = record
    Models: TWfcMusicEnsembleModels;
    VoiceCount, StepsPerOctave, QuantumTicks, SegmentCellCount: Integer;
    RequestedTicks: TWfcMusicArrangementWide;
    Seed: TGraphSeed;
    Rounding: TWfcMusicArrangementRounding;
    HarmonyMode: TWfcMusicEnsembleHarmonyMode;
    RequireObservedEnd: Boolean;
    Search: TGraphNegotiationOptions;
  end;

  TWfcMusicEnsembleStreamLayers =
    array[TWfcMusicEnsembleLayer] of TWfcGeneratedSequenceSegment;
  TWfcMusicEnsembleStreamFrontier = record
    HasPrevious: Boolean;
    EndTick: TWfcMusicArrangementWide;
    StateIndices: array[TWfcMusicEnsembleLayer] of Integer;
    Tokens: array[TWfcMusicEnsembleLayer] of TWfcModelToken;
  end;
  TWfcMusicEnsembleStreamConstraint = record
    Layer: TWfcMusicEnsembleLayer;
    Position: TWfcMusicArrangementWide;
    AllowedTokens: TWfcModelTokens;
  end;
  TWfcMusicEnsembleStreamConstraints =
    array of TWfcMusicEnsembleStreamConstraint;

  { Immutable caller-owned result. A continued segment may begin with holds;
    it is intentionally not a standalone finite score. State witnesses use
    the same immutable model triple and are not portable private graph keys. }
  TWfcMusicEnsembleSegment = class
  private
    FIndex, FStartTick: TWfcMusicArrangementWide;
    FCellCount, FQuantumTicks: Integer;
    FSeed: TGraphSeed;
    FFinalSegment: Boolean;
    FLayers: TWfcMusicEnsembleStreamLayers;
    FSignature: Cardinal;
    constructor Create(const AIndex, AStartTick: TWfcMusicArrangementWide;
      const ACellCount: Integer; const ASeed: TGraphSeed;
      const AFinal: Boolean; const AConfig: TWfcMusicEnsembleStreamConfig;
      const ALayers: TWfcMusicEnsembleStreamLayers);
  public
    function CopyGenerated(const ALayer: TWfcMusicEnsembleLayer):
      TWfcGeneratedSequenceSegment;
    function CopyFrames: TWfcMusicEnsembleFrames;
    property Index: TWfcMusicArrangementWide read FIndex;
    property StartTick: TWfcMusicArrangementWide read FStartTick;
    property CellCount: Integer read FCellCount;
    property QuantumTicks: Integer read FQuantumTicks;
    property Seed: TGraphSeed read FSeed;
    property FinalSegment: Boolean read FFinalSegment;
    property Signature: Cardinal read FSignature;
  end;

  { Borrows immutable Models. Retains a fixed frontier, not yielded segments,
    frames, or search reports. Sparse caller constraints consume space only
    for explicitly constrained future coordinates. Calls are synchronous and
    non-reentrant. Previously emitted history cannot be repaired in place. }
  TWfcMusicEnsembleStream = class
  strict private
    FConfig: TWfcMusicEnsembleStreamConfig;
    FActualTicks, FProducedTicks, FNextIndex: TWfcMusicArrangementWide;
    FFrontier: TWfcMusicEnsembleStreamFrontier;
    FConstraints: TWfcMusicEnsembleStreamConstraints;
    FBindings: TWfcSequenceProjectionBindings;
    FStatus: TWfcMusicArrangementStatus;
    FFailure: String;
    FRunning: Boolean;
    function ModelForLayer(const ALayer: TWfcMusicEnsembleLayer):
      TWfcSequenceModel;
    function GetRequestedTicks: TWfcMusicArrangementWide;
    function GetRemainingTicks: TWfcMusicArrangementWide;
    function GetSegmentCount: TWfcMusicArrangementWide;
    procedure RequireFuturePosition(const ALayer: TWfcMusicEnsembleLayer;
      const APosition: TWfcMusicArrangementWide);
    procedure ApplyConstraints(const AGraph: TGraph; const ACellCount: Integer);
    procedure ValidateLayers(const AGraph: TGraph;
      const ALayers: TWfcMusicEnsembleStreamLayers; const ACellCount: Integer);
    procedure SetFailure(const AMessage: String);
    function RemainingConstraints(const AEndTick: TWfcMusicArrangementWide):
      TWfcMusicEnsembleStreamConstraints;
  protected
    { AGraph is borrowed for this call only. Add application restrictions;
      do not retain it. Independent path/domain/music validation cannot be
      bypassed by this hook. It runs once before this segment's finite search. }
    procedure ConfigureSegment(const AIndex, AStartTick: TWfcMusicArrangementWide;
      const ACellCount: Integer; const AGraph: TGraph); virtual;
    { Observational application proof after built-in path/domain/music checks.
      Candidate is borrowed for this call only. Do not retain it or advance
      application state here: later preparation may still fail. False rejects
      the segment before any public frontier, count, or constraint changes. }
    function ValidateSegment(const Candidate: TWfcMusicEnsembleSegment;
      out Failure: String): Boolean; virtual;
  public
    constructor Create(const AConfig: TWfcMusicEnsembleStreamConfig);
    function Next(out ASegment: TWfcMusicEnsembleSegment;
      out AReport: TGraphNegotiationReport): TWfcMusicArrangementStep;
    procedure Cancel;
    function CopyFrontier: TWfcMusicEnsembleStreamFrontier;
    function CopyConstraints: TWfcMusicEnsembleStreamConstraints;
    function IntersectAllowedTokens(const ALayer: TWfcMusicEnsembleLayer;
      const APosition: TWfcMusicArrangementWide; const ATokens: TWfcModelTokens):
      TWfcMusicEnsembleStream;
    function ClearAllowedTokens(const ALayer: TWfcMusicEnsembleLayer;
      const APosition: TWfcMusicArrangementWide): TWfcMusicEnsembleStream;
    property RequestedTicks: TWfcMusicArrangementWide read GetRequestedTicks;
    property ActualTicks: TWfcMusicArrangementWide read FActualTicks;
    property ProducedTicks: TWfcMusicArrangementWide read FProducedTicks;
    property RemainingTicks: TWfcMusicArrangementWide read GetRemainingTicks;
    property SegmentCount: TWfcMusicArrangementWide read GetSegmentCount;
    property NextIndex: TWfcMusicArrangementWide read FNextIndex;
    property Status: TWfcMusicArrangementStatus read FStatus;
    property Failure: String read FFailure;
  end;

function DefaultWfcMusicEnsembleStreamConfig(
  const AModels: TWfcMusicEnsembleModels;
  const AVoiceCount, AStepsPerOctave, AQuantumTicks: Integer;
  const ARequestedTicks: TWfcMusicArrangementWide; const ASeed: TGraphSeed):
  TWfcMusicEnsembleStreamConfig;

implementation

uses wfc_music_sequence;

procedure StreamError(const AMessage: String);
begin
  raise EWfcMusicEnsembleStream.Create('ensemble stream: ' + AMessage);
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

procedure CheckLayer(const ALayer: TWfcMusicEnsembleLayer);
begin
  if (Ord(ALayer) < Ord(Low(TWfcMusicEnsembleLayer))) or
      (Ord(ALayer) > Ord(High(TWfcMusicEnsembleLayer))) then
    StreamError('unknown layer');
  {$IFDEF PAS2JS}
  if Ord(ALayer) <> Trunc(Ord(ALayer)) then StreamError('unknown layer');
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

constructor TWfcMusicEnsembleSegment.Create(
  const AIndex, AStartTick: TWfcMusicArrangementWide;
  const ACellCount: Integer; const ASeed: TGraphSeed;
  const AFinal: Boolean; const AConfig: TWfcMusicEnsembleStreamConfig;
  const ALayers: TWfcMusicEnsembleStreamLayers);
var LLayer: TWfcMusicEnsembleLayer; I: Integer;
begin
  inherited Create;
  FIndex := AIndex;
  FStartTick := AStartTick;
  FCellCount := ACellCount;
  FQuantumTicks := AConfig.QuantumTicks;
  FSeed := ASeed;
  FFinalSegment := AFinal;
  FSignature := Cardinal(2166136261);
  HashWide(FSignature, WFC_MUSIC_ENSEMBLE_SEGMENT_SIGNATURE_VERSION);
  HashWide(FSignature, WFC_MUSIC_ENSEMBLE_STREAM_VERSION);
  HashWide(FSignature, WFC_SEQUENCE_SEGMENT_VERSION);
  HashWide(FSignature, WFC_MUSIC_ENSEMBLE_TOKEN_VERSION);
  HashWide(FSignature, FIndex);
  HashWide(FSignature, FStartTick);
  HashWide(FSignature, FCellCount);
  HashWide(FSignature, FQuantumTicks);
  HashWide(FSignature, Cardinal(FSeed));
  HashWide(FSignature, Ord(FFinalSegment));
  HashWide(FSignature, AConfig.VoiceCount);
  HashWide(FSignature, AConfig.StepsPerOctave);
  HashWide(FSignature, Ord(AConfig.HarmonyMode));
  for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
  begin
    FLayers[LLayer] := CopySegment(ALayers[LLayer]);
    HashWide(FSignature, Ord(ALayers[LLayer].Boundary.HasPrevious));
    if ALayers[LLayer].Boundary.HasPrevious then
      HashWide(FSignature, ALayers[LLayer].Boundary.PreviousState);
    HashWide(FSignature, Ord(ALayers[LLayer].Boundary.RequireObservedEnd));
    for I := 0 to ACellCount - 1 do
    begin
      HashWide(FSignature, ALayers[LLayer].StateIndices[I]);
      HashText(FSignature, ALayers[LLayer].Tokens[I]);
    end;
  end;
end;

function TWfcMusicEnsembleSegment.CopyGenerated(
  const ALayer: TWfcMusicEnsembleLayer): TWfcGeneratedSequenceSegment;
begin
  CheckLayer(ALayer);
  Result := CopySegment(FLayers[ALayer]);
end;

function TWfcMusicEnsembleSegment.CopyFrames: TWfcMusicEnsembleFrames;
begin
  Result := DecodeWfcMusicEnsembleFrames(FLayers[wmelEnsemble].Tokens);
end;

function DefaultWfcMusicEnsembleStreamConfig(
  const AModels: TWfcMusicEnsembleModels;
  const AVoiceCount, AStepsPerOctave, AQuantumTicks: Integer;
  const ARequestedTicks: TWfcMusicArrangementWide; const ASeed: TGraphSeed):
  TWfcMusicEnsembleStreamConfig;
begin
  Result := Default(TWfcMusicEnsembleStreamConfig);
  Result.Models := AModels;
  Result.VoiceCount := AVoiceCount;
  Result.StepsPerOctave := AStepsPerOctave;
  Result.QuantumTicks := AQuantumTicks;
  Result.SegmentCellCount := 16;
  Result.RequestedTicks := ARequestedTicks;
  Result.Seed := ASeed;
  Result.Rounding := wmarExact;
  Result.HarmonyMode := wmehmExact;
  Result.Search := DefaultGraphNegotiationOptions;
  Result.Search.SolveOptions.MaxBacktracks := 256;
  Result.Search.MaxPassBacktracks := 16;
end;

constructor TWfcMusicEnsembleStream.Create(
  const AConfig: TWfcMusicEnsembleStreamConfig);
var LLayer: TWfcMusicEnsembleLayer; LRules: TWfcSequenceProjectionRules;
begin
  inherited Create;
  CheckWide(AConfig.RequestedTicks, 'requested ticks');
  CheckInteger(AConfig.QuantumTicks, 1, High(Integer), 'quantum ticks');
  CheckInteger(AConfig.SegmentCellCount, 1, High(Integer), 'segment cell count');
  if AConfig.SegmentCellCount > High(Integer) div AConfig.QuantumTicks then
    StreamError('local segment ticks exceed Integer capacity');
  CheckInteger(AConfig.VoiceCount, 1, High(Integer), 'voice count');
  CheckInteger(AConfig.StepsPerOctave, 1, High(Integer), 'pitch-class system');
  CheckInteger(Ord(AConfig.HarmonyMode), Ord(Low(TWfcMusicEnsembleHarmonyMode)),
    Ord(High(TWfcMusicEnsembleHarmonyMode)), 'harmony interpretation');
  CheckInteger(Ord(AConfig.Rounding), Ord(Low(TWfcMusicArrangementRounding)),
    Ord(High(TWfcMusicArrangementRounding)), 'rounding mode');
  CheckInteger(AConfig.Seed, 0, Cardinal($FFFFFFFF), 'seed');
  CheckInteger(AConfig.Search.SolveOptions.MaxBacktracks, 0, High(Integer),
    'local search allowance');
  CheckInteger(AConfig.Search.MaxPassBacktracks, 0, High(Integer),
    'pass search allowance');
  FActualTicks := ResolveWfcMusicArrangementTicks(AConfig.RequestedTicks,
    AConfig.QuantumTicks, AConfig.Rounding);
  if FActualTicks < 1 then StreamError('rounded duration must be positive');
  FConfig := AConfig;
  ValidateWfcMusicEnsembleModel(FConfig.Models.Ensemble, FConfig.VoiceCount);
  ValidateWfcMusicEnsembleRhythmModel(FConfig.Models.Rhythm, FConfig.VoiceCount);
  ValidateWfcMusicEnsembleHarmonyModel(FConfig.Models.Harmony,
    FConfig.StepsPerOctave);
  SetLength(FBindings, 2);
  LRules := BuildWfcMusicEnsembleRhythmProjectionRules(
    FConfig.Models.Ensemble, FConfig.Models.Rhythm);
  FBindings[0] := MakeWfcSequenceProjectionBinding(FConfig.Models.Rhythm,
    WFC_MUSIC_ENSEMBLE_PASS_RHYTHM, LRules);
  if FConfig.HarmonyMode = wmehmExact then
    LRules := BuildWfcMusicEnsembleExactHarmonyProjectionRules(
      FConfig.Models.Ensemble, FConfig.Models.Harmony, FConfig.StepsPerOctave)
  else
    LRules := BuildWfcMusicEnsembleAllowedHarmonyProjectionRules(
      FConfig.Models.Ensemble, FConfig.Models.Harmony, FConfig.StepsPerOctave);
  FBindings[1] := MakeWfcSequenceProjectionBinding(FConfig.Models.Harmony,
    WFC_MUSIC_ENSEMBLE_PASS_HARMONY, LRules);
  FFrontier := Default(TWfcMusicEnsembleStreamFrontier);
  for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
    FFrontier.StateIndices[LLayer] := -1;
  FStatus := wmasReady;
end;

function TWfcMusicEnsembleStream.ModelForLayer(
  const ALayer: TWfcMusicEnsembleLayer): TWfcSequenceModel;
begin
  CheckLayer(ALayer);
  case ALayer of
    wmelHarmony: Result := FConfig.Models.Harmony;
    wmelRhythm: Result := FConfig.Models.Rhythm;
    wmelEnsemble: Result := FConfig.Models.Ensemble;
  else Result := nil;
  end;
end;

function TWfcMusicEnsembleStream.GetRequestedTicks: TWfcMusicArrangementWide;
begin Result := FConfig.RequestedTicks end;

function TWfcMusicEnsembleStream.GetRemainingTicks: TWfcMusicArrangementWide;
begin Result := FActualTicks - FProducedTicks end;

function TWfcMusicEnsembleStream.GetSegmentCount: TWfcMusicArrangementWide;
var LCells: TWfcMusicArrangementWide;
begin
  LCells := FActualTicks div FConfig.QuantumTicks;
  Result := LCells div FConfig.SegmentCellCount;
  if LCells mod FConfig.SegmentCellCount <> 0 then Inc(Result);
end;

procedure TWfcMusicEnsembleStream.RequireFuturePosition(
  const ALayer: TWfcMusicEnsembleLayer; const APosition: TWfcMusicArrangementWide);
begin
  CheckLayer(ALayer);
  CheckWide(APosition, 'constraint position');
  if FRunning then StreamError('cannot edit constraints during Next');
  if FStatus in [wmasCompleted, wmasCancelled, wmasFailed] then
    StreamError('cannot edit a terminal stream');
  if (APosition < FProducedTicks div FConfig.QuantumTicks) or
      (APosition >= FActualTicks div FConfig.QuantumTicks) then
    StreamError('constraint position must address an unproduced cell');
end;

function TWfcMusicEnsembleStream.IntersectAllowedTokens(
  const ALayer: TWfcMusicEnsembleLayer;
  const APosition: TWfcMusicArrangementWide; const ATokens: TWfcModelTokens):
  TWfcMusicEnsembleStream;
var I, J, LCount: Integer; LNew: TWfcMusicEnsembleStreamConstraint;
  LPrepared: TWfcMusicEnsembleStreamConstraints;
begin
  RequireFuturePosition(ALayer, APosition);
  for I := 0 to High(ATokens) do
  begin
    if ModelForLayer(ALayer).FindPublicToken(ATokens[I]) < 0 then
      StreamError('constraint token is not in the selected model');
    for J := 0 to I - 1 do
      if ATokens[J] = ATokens[I] then StreamError('duplicate constraint token');
  end;
  LNew.Layer := ALayer;
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

function TWfcMusicEnsembleStream.ClearAllowedTokens(
  const ALayer: TWfcMusicEnsembleLayer;
  const APosition: TWfcMusicArrangementWide): TWfcMusicEnsembleStream;
var I, J: Integer; LPrepared: TWfcMusicEnsembleStreamConstraints;
begin
  RequireFuturePosition(ALayer, APosition);
  LPrepared := nil;
  J := 0;
  for I := 0 to High(FConstraints) do
    if (FConstraints[I].Layer <> ALayer) or
        (FConstraints[I].Position <> APosition) then Inc(J);
  SetLength(LPrepared, J);
  J := 0;
  for I := 0 to High(FConstraints) do
    if (FConstraints[I].Layer <> ALayer) or
        (FConstraints[I].Position <> APosition) then
    begin
      LPrepared[J] := FConstraints[I];
      Inc(J);
    end;
  FConstraints := LPrepared;
  Result := Self;
end;

function TWfcMusicEnsembleStream.CopyConstraints:
  TWfcMusicEnsembleStreamConstraints;
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

function TWfcMusicEnsembleStream.CopyFrontier: TWfcMusicEnsembleStreamFrontier;
begin Result := FFrontier end;

procedure TWfcMusicEnsembleStream.ApplyConstraints(
  const AGraph: TGraph; const ACellCount: Integer);
var I: Integer; LStart, LLocal: TWfcMusicArrangementWide;
begin
  LStart := FProducedTicks div FConfig.QuantumTicks;
  for I := 0 to High(FConstraints) do
  begin
    LLocal := FConstraints[I].Position - LStart;
    if (LLocal >= 0) and (LLocal < ACellCount) then
      IntersectSequenceAllowedTokens(ModelForLayer(FConstraints[I].Layer),
        AGraph.PassGraph[Ord(FConstraints[I].Layer)], Integer(LLocal),
        FConstraints[I].AllowedTokens);
  end;
end;

function TWfcMusicEnsembleStream.RemainingConstraints(
  const AEndTick: TWfcMusicArrangementWide): TWfcMusicEnsembleStreamConstraints;
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

procedure TWfcMusicEnsembleStream.ConfigureSegment(
  const AIndex, AStartTick: TWfcMusicArrangementWide;
  const ACellCount: Integer; const AGraph: TGraph);
begin
end;

function TWfcMusicEnsembleStream.ValidateSegment(
  const Candidate: TWfcMusicEnsembleSegment; out Failure: String): Boolean;
begin
  Failure := '';
  Result := True;
end;

function HarmonyMatches(const AFrame: TWfcMusicEnsembleFrame;
  const AHarmony: TWfcMusicPitchClassSet; const ASteps: Integer;
  const AMode: TWfcMusicEnsembleHarmonyMode): Boolean;
var I, J, K, LClass: Integer; LFound: Boolean;
begin
  Result := False;
  if AHarmony.StepsPerOctave <> ASteps then Exit;
  for I := 0 to High(AFrame.Voices) do
    for J := 0 to High(AFrame.Voices[I].Tones) do
    begin
      LClass := AFrame.Voices[I].Tones[J].Pitch mod ASteps;
      LFound := False;
      for K := 0 to High(AHarmony.PitchClasses) do
        if AHarmony.PitchClasses[K] = LClass then
        begin LFound := True; Break end;
      if not LFound then Exit;
    end;
  if AMode = wmehmExact then
    for K := 0 to High(AHarmony.PitchClasses) do
    begin
      LFound := False;
      for I := 0 to High(AFrame.Voices) do
        for J := 0 to High(AFrame.Voices[I].Tones) do
          if AFrame.Voices[I].Tones[J].Pitch mod ASteps =
              AHarmony.PitchClasses[K] then LFound := True;
      if not LFound then Exit;
    end;
  Result := True;
end;

procedure TWfcMusicEnsembleStream.ValidateLayers(const AGraph: TGraph;
  const ALayers: TWfcMusicEnsembleStreamLayers; const ACellCount: Integer);
var
  I, J, LInvalid: Integer;
  LLayer: TWfcMusicEnsembleLayer;
  LFrame, LPrevious: TWfcMusicEnsembleFrame;
  LRhythm: TWfcMusicRhythmFrame;
  LHarmony: TWfcMusicPitchClassSet;
  LLocal: TWfcMusicArrangementWide;
  LFound: Boolean;
begin
  if AGraph.TotalPassCount <> 3 then StreamError('segment must retain three passes');
  for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
  begin
    if AGraph.PassGraph[Ord(LLayer)].CurrentPass <>
        WfcMusicEnsembleLayerName(LLayer) then StreamError('pass identity changed');
    if (Length(ALayers[LLayer].Tokens) <> ACellCount) or
        (Length(ALayers[LLayer].StateIndices) <> ACellCount) then
      StreamError('captured layer length differs');
    if not SequenceStatesSatisfyEntryConstraints(ModelForLayer(LLayer),
        AGraph.PassGraph[Ord(LLayer)], ALayers[LLayer].StateIndices, LInvalid) then
      StreamError('captured path violates caller constraints at ' + IntToStr(LInvalid));
  end;
  //The application hook can inspect and narrow graph domains, but clearing a
  //domain there must not erase a caller's separately stored global constraint.
  for I := 0 to High(FConstraints) do
  begin
    LLocal := FConstraints[I].Position - FProducedTicks div FConfig.QuantumTicks;
    if (LLocal < 0) or (LLocal >= ACellCount) then Continue;
    LFound := False;
    for J := 0 to High(FConstraints[I].AllowedTokens) do
      if FConstraints[I].AllowedTokens[J] =
          ALayers[FConstraints[I].Layer].Tokens[Integer(LLocal)] then
        LFound := True;
    if not LFound then StreamError('captured public token violates a global constraint');
  end;
  if FFrontier.HasPrevious then
    LPrevious := DecodeWfcMusicEnsembleFrame(FFrontier.Tokens[wmelEnsemble])
  else LPrevious := Default(TWfcMusicEnsembleFrame);
  for I := 0 to ACellCount - 1 do
  begin
    LFrame := DecodeWfcMusicEnsembleFrame(ALayers[wmelEnsemble].Tokens[I]);
    if Length(LFrame.Voices) <> FConfig.VoiceCount then
      StreamError('segment voice arity changed');
    if (I > 0) or FFrontier.HasPrevious then
    begin
      if not WfcMusicEnsembleFrameCanFollow(LPrevious, LFrame) then
        StreamError('invalid continued voice at segment cell ' + IntToStr(I));
    end
    else if not WfcMusicEnsembleFrameCanStart(LFrame) then
      StreamError('initial segment begins with a hold');
    LRhythm := DecodeWfcMusicRhythmFrame(ALayers[wmelRhythm].Tokens[I]);
    if Length(LRhythm.Actions) <> Length(LFrame.Voices) then
      StreamError('rhythm voice arity differs');
    for J := 0 to High(LFrame.Voices) do
      if LRhythm.Actions[J] <> LFrame.Voices[J].Action then
        StreamError('rhythm projection differs');
    LHarmony := DecodeWfcMusicPitchClassSet(ALayers[wmelHarmony].Tokens[I]);
    if not HarmonyMatches(LFrame, LHarmony, FConfig.StepsPerOctave,
        FConfig.HarmonyMode) then StreamError('harmony projection differs');
    LPrevious := LFrame;
  end;
end;

procedure TWfcMusicEnsembleStream.SetFailure(const AMessage: String);
begin FStatus := wmasFailed; FFailure := AMessage end;

procedure TWfcMusicEnsembleStream.Cancel;
begin
  if FStatus in [wmasReady, wmasActive] then FStatus := wmasCancelled;
end;

function TWfcMusicEnsembleStream.Next(out ASegment: TWfcMusicEnsembleSegment;
  out AReport: TGraphNegotiationReport): TWfcMusicArrangementStep;
var
  LGraph: TGraph;
  LLayer: TWfcMusicEnsembleLayer;
  LBoundary: TWfcSequenceSegmentBoundary;
  LLayers: TWfcMusicEnsembleStreamLayers;
  LCapture: TWfcSequenceGraphValidationReport;
  LCells: Integer;
  LRemainingCells: TWfcMusicArrangementWide;
  LFinal: Boolean;
  LSeed: TGraphSeed;
  LCandidate: TWfcMusicEnsembleSegment;
  LFrontier: TWfcMusicEnsembleStreamFrontier;
  LRemainingConstraints: TWfcMusicEnsembleStreamConstraints;
  LValidationFailure: String;
  LValid: Boolean;
begin
  ASegment := nil;
  AReport := Default(TGraphNegotiationReport);
  if FRunning then StreamError('Next cannot be reentered');
  case FStatus of
    wmasCompleted: Exit(wmaspCompleted);
    wmasCancelled: Exit(wmaspCancelled);
    wmasFailed: Exit(wmaspFailed);
  end;
  if FProducedTicks = FActualTicks then
  begin FStatus := wmasCompleted; Exit(wmaspCompleted) end;
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
      LSeed := WfcMusicArrangementSectionSeed(FConfig.Seed, FNextIndex);
      LGraph := TGraph.Create;
      LGraph.Reshape(LCells, 1, 1);
      LGraph.WrapNeighbors := False;
      LGraph.Seed := LSeed;
      for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
      begin
        if LLayer = wmelHarmony then
          LGraph.CurrentPass := WfcMusicEnsembleLayerName(LLayer)
        else LGraph.SwitchToPass(WfcMusicEnsembleLayerName(LLayer));
        LGraph.PassMode := gpmOverlay;
        LGraph.ClearDependencies;
        if FFrontier.HasPrevious then
          LBoundary := MakeWfcSequenceContinuingSegmentBoundary(
            FFrontier.StateIndices[LLayer], LFinal and FConfig.RequireObservedEnd)
        else LBoundary := MakeWfcSequenceInitialSegmentBoundary(
          LFinal and FConfig.RequireObservedEnd);
        ApplySequenceModelSegmentToGraph(ModelForLayer(LLayer), LGraph, LBoundary);
        LLayers[LLayer].Boundary := LBoundary;
      end;
      RequireSequenceProjectionMapsFromPasses(FConfig.Models.Ensemble,
        LGraph, FBindings);
      ApplyConstraints(LGraph, LCells);
      LGraph.SwitchToPass(WFC_MUSIC_ENSEMBLE_PASS_HARMONY);
      ConfigureSegment(FNextIndex, FProducedTicks, LCells, LGraph);
      if FStatus = wmasCancelled then Exit(wmaspCancelled);
      if LGraph.Seed <> LSeed then
        StreamError('application hook changed the derived segment seed');
      if not LGraph.TrySolveNegotiated(FConfig.Search, AReport) then
      begin
        SetFailure('finite segment search failed (status ' +
          IntToStr(Ord(AReport.Status)) + ') at segment ' + IntToStr(FNextIndex));
        Exit(wmaspFailed);
      end;
      if FStatus = wmasCancelled then Exit(wmaspCancelled);
      for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
      begin
        LBoundary := LLayers[LLayer].Boundary;
        if not CaptureSolvedSequenceSegment(ModelForLayer(LLayer),
            LGraph.PassGraph[Ord(LLayer)], LBoundary, LLayers[LLayer], LCapture) then
          StreamError('invalid captured state path: ' +
            DescribeSequenceGraphIssue(LCapture.Issue));
      end;
      ValidateLayers(LGraph, LLayers, LCells);
      LCandidate := TWfcMusicEnsembleSegment.Create(FNextIndex, FProducedTicks,
        LCells, LSeed, LFinal, FConfig, LLayers);
      LValid := ValidateSegment(LCandidate, LValidationFailure);
      if FStatus = wmasCancelled then Exit(wmaspCancelled);
      if not LValid then
      begin
        if LValidationFailure = '' then
          LValidationFailure := 'application segment validation failed';
        SetFailure(LValidationFailure);
        Exit(wmaspFailed);
      end;
      LFrontier := Default(TWfcMusicEnsembleStreamFrontier);
      LFrontier.HasPrevious := True;
      LFrontier.EndTick := FProducedTicks + LCells * FConfig.QuantumTicks;
      for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
      begin
        LFrontier.StateIndices[LLayer] := LLayers[LLayer].StateIndices[LCells - 1];
        LFrontier.Tokens[LLayer] := LLayers[LLayer].Tokens[LCells - 1];
      end;
      LRemainingConstraints := RemainingConstraints(LFrontier.EndTick);
      //All allocations and semantic proof precede public progress mutation.
      //Record assignment clones static arrays in pas2js. Write the already
      //allocated scalar slots directly so publication needs no array clone.
      for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
      begin
        FFrontier.StateIndices[LLayer] := LFrontier.StateIndices[LLayer];
        FFrontier.Tokens[LLayer] := LFrontier.Tokens[LLayer];
      end;
      FFrontier.HasPrevious := True;
      FFrontier.EndTick := LFrontier.EndTick;
      FProducedTicks := LFrontier.EndTick;
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
