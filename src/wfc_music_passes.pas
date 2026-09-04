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
unit wfc_music_passes;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_sequence,
  wfc_sequence_graph,
  wfc_music,
  wfc_music_sequence,
  wfc_music_graph;

const
  WFC_MUSIC_PASS_PIPELINE_VERSION = 1;
  WFC_MUSIC_PASS_VALIDATION_VERSION = 1;
  WFC_MUSIC_COMPOSITION_SIGNATURE_VERSION = 1;

  WFC_MUSIC_PASS_HARMONY = 'harmony';
  WFC_MUSIC_PASS_RHYTHM = 'rhythm';
  WFC_MUSIC_PASS_MELODY = 'melody';

type
  EWfcMusicPasses = class(EWfcMusicGraph);

  TWfcMusicPassLayer = (
    wmplHarmony,
    wmplRhythm,
    wmplMelody
  );
  TWfcMusicPassLayers = array of TWfcMusicPassLayer;
  TWfcMusicPassLayerSet = set of TWfcMusicPassLayer;

  TWfcMusicPassModels = record
    Harmony: TWfcSequenceModel;
    Rhythm: TWfcSequenceModel;
    Melody: TWfcSequenceModel;
  end;

  { ScoreTemplate is read only during construction and is deep-copied by the
    pipeline. Models remain caller-owned and must outlive the pipeline. V1
    deliberately accepts exactly one score voice. }
  TWfcMusicPassConfig = record
    QuantumTicks: Integer;
    Seed: TGraphSeed;
    Models: TWfcMusicPassModels;
    ScoreTemplate: TWfcMusicScore;
  end;

  TWfcMusicCompositionSignature = Cardinal;

  TWfcMusicGeneratedLayers =
    array[TWfcMusicPassLayer] of TWfcGeneratedSequence;
  TWfcMusicPassCaptureReports =
    array[TWfcMusicPassLayer] of TWfcSequenceGraphValidationReport;

  TWfcMusicPassValidationIssueKind = (
    wmpvikNone,
    wmpvikComposition,
    wmpvikLength,
    wmpvikExtent,
    wmpvikBoundary,
    wmpvikLatentCapture,
    wmpvikStatePath,
    wmpvikStateProjection,
    wmpvikCallerConstraint,
    wmpvikCell,
    wmpvikMelodyContinuation,
    wmpvikRhythmProjection,
    wmpvikHarmonyProjection,
    wmpvikScore,
    wmpvikSignature,
    wmpvikInternal
  );

  TWfcMusicPassValidationIssue = record
    Kind: TWfcMusicPassValidationIssueKind;
    Layer: TWfcMusicPassLayer;
    Position: Integer;
    SequenceIssue: TWfcSequenceGraphIssue;
    Detail: String;
  end;

  TWfcMusicPassValidationReport = record
    Valid: Boolean;
    CheckedLayers: Integer;
    CheckedCells: Integer;
    CheckedRelations: Integer;
    Issue: TWfcMusicPassValidationIssue;
  end;

  TWfcMusicPassStatus = (
    wmpsNotRun,
    wmpsCompleted,
    wmpsSolveFailed,
    wmpsCaptureFailed,
    wmpsValidationFailed
  );

  TWfcMusicPassReport = record
    Status: TWfcMusicPassStatus;
    FailedLayer: TWfcMusicPassLayer;
    Solve: TGraphSolveReport;
    Capture: TWfcMusicPassCaptureReports;
    Validation: TWfcMusicPassValidationReport;
  end;

  TWfcMusicPassNegotiationReport = record
    Status: TWfcMusicPassStatus;
    FailedLayer: TWfcMusicPassLayer;
    Search: TGraphNegotiationReport;
    Capture: TWfcMusicPassCaptureReports;
    Validation: TWfcMusicPassValidationReport;
  end;

  TWfcMusicPassSelectiveNegotiationReport = record
    Status: TWfcMusicPassStatus;
    FailedLayer: TWfcMusicPassLayer;
    Search: TGraphSelectiveNegotiationReport;
    Capture: TWfcMusicPassCaptureReports;
    Validation: TWfcMusicPassValidationReport;
  end;

  { An immutable public composition. Copy methods detach all managed arrays and
    return a newly owned score. Public-artifact instances have no latent state
    capture; pipeline results additionally retain a checked detached capture. }
  TWfcMusicComposition = class
  private
    FSeed: TGraphSeed;
    FQuantumTicks: Integer;
    FLayers: TWfcMusicGeneratedLayers;
    FScore: TWfcMusicScore;
    FSignature: TWfcMusicCompositionSignature;
    FHasLatentCapture: Boolean;
    constructor CreateInternal(const ASeed: TGraphSeed;
      const AQuantumTicks: Integer;
      const AHarmonyTokens, ARhythmTokens,
      AMelodyTokens: TWfcModelTokens;
      const AScore: TWfcMusicScore);
    procedure AttachLatentCapture(
      const ALayers: TWfcMusicGeneratedLayers);
    function GetCellCount: Integer;
  public
    destructor Destroy; override;
    function CopyGenerated(
      const ALayer: TWfcMusicPassLayer): TWfcGeneratedSequence;
    function CopyHarmonyCells: TWfcMusicHarmonyCells;
    function CopyRhythmCells: TWfcMusicRhythmCells;
    function CopyMelodyCells: TWfcMusicMelodyCells;
    function CopyScore: TWfcMusicScore;

    property Seed: TGraphSeed read FSeed;
    property QuantumTicks: Integer read FQuantumTicks;
    property CellCount: Integer read GetCellCount;
    property Signature: TWfcMusicCompositionSignature read FSignature;
    property HasLatentCapture: Boolean read FHasLatentCapture;
  end;

  { A reusable owner for harmony and rhythm providers joined by a melody pass.
    Public constraints are expanded through the existing latent sequence
    adapter. Solving and negotiation remain the TGraph implementations. }
  TWfcMusicPassPipeline = class
  strict private
    FCellCount: Integer;
    FDirtyLayers: TWfcMusicPassLayerSet;
    FGraph: TGraph;
    FModels: TWfcMusicPassModels;
    FPendingCapture: TWfcMusicPassCaptureReports;
    FPendingComposition: TWfcMusicComposition;
    FPendingFailedLayer: TWfcMusicPassLayer;
    FPendingStatus: TWfcMusicPassStatus;
    FPendingValidation: TWfcMusicPassValidationReport;
    FQuantumTicks: Integer;
    FScoreTemplate: TWfcMusicScore;
    FBaselineDomains:
      array[TWfcMusicPassLayer] of array of TGraphValues;
    FBaselineHasDomains:
      array[TWfcMusicPassLayer] of array of Boolean;

    procedure Initialize(const AConfig: TWfcMusicPassConfig);
    procedure CaptureBaselineDomains;
    procedure MarkDirty(const ALayer: TWfcMusicPassLayer);
    function GetLayerGraph(const ALayer: TWfcMusicPassLayer): TGraph;
    function GetModel(const ALayer: TWfcMusicPassLayer): TWfcSequenceModel;
    function GetSeed: TGraphSeed;
    procedure SetSeed(const AValue: TGraphSeed);
    procedure BuildEffectiveRoots(const ARoots: TWfcMusicPassLayers;
      out ALabels: TGraphPassLabels);
    procedure ClearPendingCommit;
    procedure CopyPendingFailure(out ACapture: TWfcMusicPassCaptureReports;
      out AValidation: TWfcMusicPassValidationReport;
      out AFailedLayer: TWfcMusicPassLayer;
      out AStatus: TWfcMusicPassStatus);
    function TakePendingComposition(out AComposition: TWfcMusicComposition;
      out ACapture: TWfcMusicPassCaptureReports;
      out AValidation: TWfcMusicPassValidationReport;
      out AFailedLayer: TWfcMusicPassLayer;
      out AStatus: TWfcMusicPassStatus): Boolean;
    function CaptureComposition(const ASeed: TGraphSeed;
      out AComposition: TWfcMusicComposition;
      out ACapture: TWfcMusicPassCaptureReports;
      out AValidation: TWfcMusicPassValidationReport;
      out AFailedLayer: TWfcMusicPassLayer;
      out AStatus: TWfcMusicPassStatus): Boolean;
  private
    function ValidatePendingCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean;
  public
    constructor Create(const AConfig: TWfcMusicPassConfig);
    destructor Destroy; override;

    function IntersectAllowedTokens(const ALayer: TWfcMusicPassLayer;
      const APosition: Integer;
      const ATokens: TWfcModelTokens): TWfcMusicPassPipeline; overload;
    function IntersectAllowedTokens(const ALayer: TWfcMusicPassLayer;
      const APosition: Integer;
      const AToken: TWfcModelToken): TWfcMusicPassPipeline; overload;
    function IntersectTokenConstraints(const ALayer: TWfcMusicPassLayer;
      const AConstraints: TWfcSequenceTokenConstraints):
      TWfcMusicPassPipeline;
    function IntersectLockedSpan(const ALayer: TWfcMusicPassLayer;
      const AStart: Integer; const ATokens: TWfcModelTokens):
      TWfcMusicPassPipeline;
    function LockMelodyCells(const AStart: Integer;
      const ACells: TWfcMusicMelodyCells): TWfcMusicPassPipeline;
    function ClearAllowedTokens(const ALayer: TWfcMusicPassLayer;
      const APosition: Integer): TWfcMusicPassPipeline;

    function TryGenerate(const AOptions: TGraphSolveOptions;
      out AComposition: TWfcMusicComposition;
      out AReport: TWfcMusicPassReport): Boolean; overload;
    function TryGenerate(out AComposition: TWfcMusicComposition;
      out AReport: TWfcMusicPassReport): Boolean; overload;

    function TryRegenerateFrom(const ALayer: TWfcMusicPassLayer;
      const AOptions: TGraphSolveOptions;
      out AComposition: TWfcMusicComposition;
      out AReport: TWfcMusicPassReport): Boolean; overload;
    function TryRegenerateFrom(const ALayers: TWfcMusicPassLayers;
      const AOptions: TGraphSolveOptions;
      out AComposition: TWfcMusicComposition;
      out AReport: TWfcMusicPassReport): Boolean; overload;

    function TryGenerateNegotiated(const AOptions: TGraphNegotiationOptions;
      out AComposition: TWfcMusicComposition;
      out AReport: TWfcMusicPassNegotiationReport): Boolean; overload;
    function TryGenerateNegotiated(out AComposition: TWfcMusicComposition;
      out AReport: TWfcMusicPassNegotiationReport): Boolean; overload;

    function TryRegenerateNegotiatedFrom(
      const ALayer: TWfcMusicPassLayer;
      const AOptions: TGraphNegotiationOptions;
      out AComposition: TWfcMusicComposition;
      out AReport: TWfcMusicPassSelectiveNegotiationReport): Boolean; overload;
    function TryRegenerateNegotiatedFrom(
      const ALayers: TWfcMusicPassLayers;
      const AOptions: TGraphNegotiationOptions;
      out AComposition: TWfcMusicComposition;
      out AReport: TWfcMusicPassSelectiveNegotiationReport): Boolean; overload;

    function Validate(const AComposition: TWfcMusicComposition;
      out AReport: TWfcMusicPassValidationReport): Boolean;
    function TryCopyCommittedLayer(const ALayer: TWfcMusicPassLayer;
      out AGenerated: TWfcGeneratedSequence;
      out AReport: TWfcSequenceGraphValidationReport): Boolean;
    function CopyCommittedTokens(
      const ALayer: TWfcMusicPassLayer): TWfcModelTokens;
    function CopyScoreTemplate: TWfcMusicScore;

    property Model[const ALayer: TWfcMusicPassLayer]: TWfcSequenceModel
      read GetModel;
    property CellCount: Integer read FCellCount;
    property QuantumTicks: Integer read FQuantumTicks;
    property Seed: TGraphSeed read GetSeed write SetSeed;
  end;

function DefaultWfcMusicPassConfig(const AScoreTemplate: TWfcMusicScore;
  const AQuantumTicks: Integer; const ASeed: TGraphSeed):
  TWfcMusicPassConfig;

function WfcMusicPassLayerName(const ALayer: TWfcMusicPassLayer): String;

{ Checked public-artifact construction. The returned composition is caller
  owned. No latent state identities are invented by this boundary. }
function CreateWfcMusicComposition(const ASeed: TGraphSeed;
  const AQuantumTicks: Integer;
  const AHarmonyTokens, ARhythmTokens,
  AMelodyTokens: TWfcModelTokens;
  const AScore: TWfcMusicScore): TWfcMusicComposition;

function CalculateWfcMusicCompositionSignature(const ASeed: TGraphSeed;
  const AQuantumTicks: Integer;
  const AHarmonyTokens, ARhythmTokens,
  AMelodyTokens: TWfcModelTokens;
  const AScore: TWfcMusicScore): TWfcMusicCompositionSignature; overload;
function CalculateWfcMusicCompositionSignature(
  const AComposition: TWfcMusicComposition):
  TWfcMusicCompositionSignature; overload;
function WfcMusicCompositionSignatureHex(
  const ASignature: TWfcMusicCompositionSignature): String;

function DescribeWfcMusicPassValidationIssue(
  const AIssue: TWfcMusicPassValidationIssue): String;

implementation

uses
  wfc_music_text;

type
  { The generic graph transaction invokes DoValidateCommit after staging has
    been copied into live entries, while its entry and RNG snapshots are still
    available. The music owner builds its immutable result there; returning
    False makes the graph report final validation and roll the whole attempt
    back before negotiation decides whether to reopen a provider. }
  TWfcMusicPassCommitGraph = class(TGraph)
  private
    FOwner: TWfcMusicPassPipeline;
  strict protected
    function DoValidateCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean; override;
  public
    constructor CreatePass(const ARoot: TGraph;
      const APassIndex: Integer); override;
    property Owner: TWfcMusicPassPipeline read FOwner write FOwner;
  end;

constructor TWfcMusicPassCommitGraph.CreatePass(const ARoot: TGraph;
  const APassIndex: Integer);
begin
  inherited CreatePass(ARoot, APassIndex);
  if not (ARoot is TWfcMusicPassCommitGraph) then
    raise EWfcMusicPasses.Create(
      'music pass graph root has the wrong runtime type');
  FOwner := TWfcMusicPassCommitGraph(ARoot).Owner;
end;

function TWfcMusicPassCommitGraph.DoValidateCommit(
  out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
begin
  if not Assigned(FOwner) then
  begin
    AFailedPassIndex := Ord(wmplMelody);
    AFailedEntryIndex := -1;
    Exit(False);
  end;
  Result := FOwner.ValidatePendingCommit(AFailedPassIndex,
    AFailedEntryIndex);
end;

function CopyTokens(const AValues: TWfcModelTokens): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function CopyStateIndices(
  const AValues: TWfcSequenceStateIndices): TWfcSequenceStateIndices;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function CopyGeneratedSequence(
  const AValue: TWfcGeneratedSequence): TWfcGeneratedSequence;
begin
  Result := Default(TWfcGeneratedSequence);
  Result.Boundary := AValue.Boundary;
  Result.Extent := AValue.Extent;
  Result.StateIndices := CopyStateIndices(AValue.StateIndices);
  Result.Tokens := CopyTokens(AValue.Tokens);
end;

function CloneMusicScore(const AScore: TWfcMusicScore): TWfcMusicScore;
begin
  if not Assigned(AScore) then
    raise EArgumentNilException.Create('music score cannot be nil');
  Result := TWfcMusicScore.Create(AScore.TicksPerQuarter,
    AScore.StepsPerOctave, AScore.LengthTicks, AScore.CopyTracks,
    AScore.CopyVoices, AScore.CopyMeters, AScore.CopyTempos,
    AScore.CopySpans);
end;

function CheckedProduct(const A, B: Integer; const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) then
    raise EWfcMusicPasses.Create(ALabel + ' cannot be negative');
  if (A <> 0) and (B > High(Integer) div A) then
    raise EWfcMusicPasses.Create(ALabel + ' exceeds the Integer range');
  Result := A * B;
end;

function ExpectedBoundary: TWfcModelBoundary;
begin
  Result := wmbOpen;
end;

function WfcMusicPassLayerName(const ALayer: TWfcMusicPassLayer): String;
begin
  case ALayer of
    wmplHarmony: Result := WFC_MUSIC_PASS_HARMONY;
    wmplRhythm: Result := WFC_MUSIC_PASS_RHYTHM;
    wmplMelody: Result := WFC_MUSIC_PASS_MELODY;
  else
    raise ERangeError.Create('unknown music pass layer');
  end;
end;

function DefaultWfcMusicPassConfig(const AScoreTemplate: TWfcMusicScore;
  const AQuantumTicks: Integer; const ASeed: TGraphSeed):
  TWfcMusicPassConfig;
begin
  Result := Default(TWfcMusicPassConfig);
  Result.QuantumTicks := AQuantumTicks;
  Result.Seed := ASeed;
  Result.ScoreTemplate := AScoreTemplate;
end;

function MelodyContinuationIsValid(
  const ACells: TWfcMusicMelodyCells;
  out AInvalidPosition: Integer): Boolean;
var
  I: Integer;
begin
  AInvalidPosition := -1;
  for I := 0 to Length(ACells) - 1 do
    if ACells[I].Action = wmcaHold then
    begin
      if (I = 0) or
          not (ACells[I - 1].Action in [wmcaAttack, wmcaHold]) or
          (ACells[I - 1].Pitch <> ACells[I].Pitch) or
          (ACells[I - 1].Velocity <> ACells[I].Velocity) then
      begin
        AInvalidPosition := I;
        Exit(False);
      end;
    end;
  Result := True;
end;

procedure ValidatePublicCompositionInputs(const AQuantumTicks: Integer;
  const AHarmonyTokens, ARhythmTokens,
  AMelodyTokens: TWfcModelTokens; const AScore: TWfcMusicScore);
var
  I: Integer;
  LInvalidPosition: Integer;
  LExpected: TWfcMusicScore;
  LHarmony: TWfcMusicHarmonyCell;
  LMelody: TWfcMusicMelodyCell;
  LMelodyCells: TWfcMusicMelodyCells;
  LRhythm: TWfcMusicRhythmCell;
  LSpans: TWfcMusicSpanEvents;
begin
  if not Assigned(AScore) then
    raise EArgumentNilException.Create('music composition score cannot be nil');
  if AQuantumTicks < 1 then
    raise EWfcMusicPasses.Create('music composition quantum must be positive');
  if Length(AMelodyTokens) < 1 then
    raise EWfcMusicPasses.Create('music composition cannot be empty');
  if (Length(AHarmonyTokens) <> Length(AMelodyTokens)) or
      (Length(ARhythmTokens) <> Length(AMelodyTokens)) then
    raise EWfcMusicPasses.Create(
      'music composition layer lengths must match');
  if AScore.VoiceCount <> 1 then
    raise EWfcMusicPasses.Create(
      'music composition v1 requires exactly one score voice');
  if CheckedProduct(Length(AMelodyTokens), AQuantumTicks,
      'music composition length') <> AScore.LengthTicks then
    raise EWfcMusicPasses.Create(
      'music composition cells do not fill the score');

  LMelodyCells := nil;
  SetLength(LMelodyCells, Length(AMelodyTokens));
  for I := 0 to Length(AMelodyTokens) - 1 do
  begin
    LMelody := DecodeWfcMusicMelodyCell(AMelodyTokens[I]);
    LRhythm := DecodeWfcMusicRhythmCell(ARhythmTokens[I]);
    LHarmony := DecodeWfcMusicHarmonyCell(AHarmonyTokens[I]);
    if LMelody.Action <> LRhythm.Action then
      raise EWfcMusicPasses.CreateFmt(
        'music rhythm projection differs at cell %d', [I]);
    if LHarmony.StepsPerOctave <> AScore.StepsPerOctave then
      raise EWfcMusicPasses.CreateFmt(
        'music harmony step system differs at cell %d', [I]);
    if (LMelody.Action <> wmcaRest) and
        ((LHarmony.Kind <> wmhckPitchClass) or
         (LHarmony.PitchClass <> (LMelody.Pitch mod
           AScore.StepsPerOctave))) then
      raise EWfcMusicPasses.CreateFmt(
        'music harmony projection differs at cell %d', [I]);
    LMelodyCells[I] := LMelody;
  end;

  if not MelodyContinuationIsValid(LMelodyCells, LInvalidPosition) then
    raise EWfcMusicPasses.CreateFmt(
      'music hold continuation is invalid at cell %d',
      [LInvalidPosition]);

  LSpans := RebuildWfcMusicVoiceSpans(LMelodyCells, 0, AQuantumTicks);
  LExpected := TWfcMusicScore.Create(AScore.TicksPerQuarter,
    AScore.StepsPerOctave, AScore.LengthTicks, AScore.CopyTracks,
    AScore.CopyVoices, AScore.CopyMeters, AScore.CopyTempos, LSpans);
  try
    if EncodeWfcMusicText(LExpected) <> EncodeWfcMusicText(AScore) then
      raise EWfcMusicPasses.Create(
        'music composition score differs from its melody cells');
  finally
    LExpected.Free;
  end;
end;

procedure HashByte(var AHash: TWfcMusicCompositionSignature;
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

procedure HashCardinal(var AHash: TWfcMusicCompositionSignature;
  const AValue: Cardinal);
begin
  HashByte(AHash, Byte(AValue and $FF));
  HashByte(AHash, Byte((AValue shr 8) and $FF));
  HashByte(AHash, Byte((AValue shr 16) and $FF));
  HashByte(AHash, Byte((AValue shr 24) and $FF));
end;

procedure HashInteger(var AHash: TWfcMusicCompositionSignature;
  const AValue: Integer);
begin
  HashCardinal(AHash, Cardinal(AValue));
end;

procedure HashAsciiString(var AHash: TWfcMusicCompositionSignature;
  const AValue: String; const ALabel: String);
var
  I: Integer;
begin
  HashInteger(AHash, Length(AValue));
  for I := 1 to Length(AValue) do
  begin
    if Ord(AValue[I]) > 127 then
      raise EWfcMusicPasses.Create(ALabel + ' must be canonical ASCII');
    HashByte(AHash, Byte(Ord(AValue[I])));
  end;
end;

procedure HashTokens(var AHash: TWfcMusicCompositionSignature;
  const AValues: TWfcModelTokens; const ALabel: String);
var
  I: Integer;
begin
  HashInteger(AHash, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    HashAsciiString(AHash, String(AValues[I]), ALabel);
end;

function CalculatePublicSignatureUnchecked(const ASeed: TGraphSeed;
  const AQuantumTicks: Integer;
  const AHarmonyTokens, ARhythmTokens,
  AMelodyTokens: TWfcModelTokens;
  const AScore: TWfcMusicScore): TWfcMusicCompositionSignature;
begin
  Result := Cardinal(2166136261);
  HashInteger(Result, WFC_MUSIC_COMPOSITION_SIGNATURE_VERSION);
  HashInteger(Result, WFC_MUSIC_PASS_PIPELINE_VERSION);
  HashInteger(Result, WFC_MUSIC_PASS_VALIDATION_VERSION);
  HashInteger(Result, WFC_MUSIC_MODEL_VERSION);
  HashInteger(Result, WFC_MUSIC_VALIDATION_VERSION);
  HashInteger(Result, WFC_MUSIC_SEQUENCE_VERSION);
  HashInteger(Result, WFC_MUSIC_CELL_TOKEN_VERSION);
  HashInteger(Result, WFC_MUSIC_GRAPH_ADAPTER_VERSION);
  HashInteger(Result, WFC_SEQUENCE_MODEL_VERSION);
  HashInteger(Result, WFC_SEQUENCE_GRAPH_MODEL_VERSION);
  HashInteger(Result, WFC_SEQUENCE_EXTENT_VERSION);
  HashCardinal(Result, Cardinal(ASeed));
  HashInteger(Result, AQuantumTicks);
  HashInteger(Result, AScore.StepsPerOctave);
  HashTokens(Result, AHarmonyTokens, 'music harmony signature token');
  HashTokens(Result, ARhythmTokens, 'music rhythm signature token');
  HashTokens(Result, AMelodyTokens, 'music melody signature token');
  HashAsciiString(Result, EncodeWfcMusicText(AScore),
    'music score signature text');
end;

function CalculateWfcMusicCompositionSignature(const ASeed: TGraphSeed;
  const AQuantumTicks: Integer;
  const AHarmonyTokens, ARhythmTokens,
  AMelodyTokens: TWfcModelTokens;
  const AScore: TWfcMusicScore): TWfcMusicCompositionSignature;
begin
  ValidatePublicCompositionInputs(AQuantumTicks, AHarmonyTokens,
    ARhythmTokens, AMelodyTokens, AScore);
  Result := CalculatePublicSignatureUnchecked(ASeed, AQuantumTicks,
    AHarmonyTokens, ARhythmTokens, AMelodyTokens, AScore);
end;

function CalculateWfcMusicCompositionSignature(
  const AComposition: TWfcMusicComposition):
  TWfcMusicCompositionSignature;
begin
  if not Assigned(AComposition) then
    raise EArgumentNilException.Create('music composition cannot be nil');
  Result := CalculatePublicSignatureUnchecked(AComposition.FSeed,
    AComposition.FQuantumTicks,
    AComposition.FLayers[wmplHarmony].Tokens,
    AComposition.FLayers[wmplRhythm].Tokens,
    AComposition.FLayers[wmplMelody].Tokens,
    AComposition.FScore);
end;

function WfcMusicCompositionSignatureHex(
  const ASignature: TWfcMusicCompositionSignature): String;
begin
  Result := IntToHex(ASignature, 8);
end;

constructor TWfcMusicComposition.CreateInternal(const ASeed: TGraphSeed;
  const AQuantumTicks: Integer;
  const AHarmonyTokens, ARhythmTokens,
  AMelodyTokens: TWfcModelTokens; const AScore: TWfcMusicScore);
begin
  inherited Create;
  FSeed := ASeed;
  FQuantumTicks := AQuantumTicks;
  FHasLatentCapture := False;
  FLayers[wmplHarmony] := Default(TWfcGeneratedSequence);
  FLayers[wmplRhythm] := Default(TWfcGeneratedSequence);
  FLayers[wmplMelody] := Default(TWfcGeneratedSequence);
  FLayers[wmplHarmony].Boundary := ExpectedBoundary;
  FLayers[wmplRhythm].Boundary := ExpectedBoundary;
  FLayers[wmplMelody].Boundary := ExpectedBoundary;
  FLayers[wmplHarmony].Extent := wseWhole;
  FLayers[wmplRhythm].Extent := wseWhole;
  FLayers[wmplMelody].Extent := wseWhole;
  FLayers[wmplHarmony].Tokens := CopyTokens(AHarmonyTokens);
  FLayers[wmplRhythm].Tokens := CopyTokens(ARhythmTokens);
  FLayers[wmplMelody].Tokens := CopyTokens(AMelodyTokens);
  FScore := CloneMusicScore(AScore);
  FSignature := CalculatePublicSignatureUnchecked(FSeed, FQuantumTicks,
    FLayers[wmplHarmony].Tokens, FLayers[wmplRhythm].Tokens,
    FLayers[wmplMelody].Tokens, FScore);
end;

procedure TWfcMusicComposition.AttachLatentCapture(
  const ALayers: TWfcMusicGeneratedLayers);
var
  I: Integer;
  LLayer: TWfcMusicPassLayer;
begin
  for LLayer := Low(TWfcMusicPassLayer) to High(TWfcMusicPassLayer) do
  begin
    if (ALayers[LLayer].Boundary <> ExpectedBoundary) or
        (ALayers[LLayer].Extent <> wseWhole) or
        (Length(ALayers[LLayer].Tokens) < 1) or
        (Length(ALayers[LLayer].Tokens) <>
          Length(FLayers[LLayer].Tokens)) or
        (Length(ALayers[LLayer].StateIndices) <>
          Length(ALayers[LLayer].Tokens)) then
      raise EWfcMusicPasses.Create(
        'music latent capture shape does not match the composition');
    for I := 0 to Length(ALayers[LLayer].Tokens) - 1 do
      if ALayers[LLayer].Tokens[I] <> FLayers[LLayer].Tokens[I] then
        raise EWfcMusicPasses.Create(
          'music latent capture token differs from the composition');
  end;
  for LLayer := Low(TWfcMusicPassLayer) to High(TWfcMusicPassLayer) do
    FLayers[LLayer] := CopyGeneratedSequence(ALayers[LLayer]);
  FHasLatentCapture := True;
end;

destructor TWfcMusicComposition.Destroy;
begin
  FScore.Free;
  inherited Destroy;
end;

function TWfcMusicComposition.GetCellCount: Integer;
begin
  Result := Length(FLayers[wmplMelody].Tokens);
end;

function TWfcMusicComposition.CopyGenerated(
  const ALayer: TWfcMusicPassLayer): TWfcGeneratedSequence;
begin
  WfcMusicPassLayerName(ALayer);
  Result := CopyGeneratedSequence(FLayers[ALayer]);
end;

function TWfcMusicComposition.CopyHarmonyCells: TWfcMusicHarmonyCells;
begin
  Result := DecodeWfcMusicHarmonyCells(FLayers[wmplHarmony].Tokens);
end;

function TWfcMusicComposition.CopyRhythmCells: TWfcMusicRhythmCells;
begin
  Result := DecodeWfcMusicRhythmCells(FLayers[wmplRhythm].Tokens);
end;

function TWfcMusicComposition.CopyMelodyCells: TWfcMusicMelodyCells;
begin
  Result := DecodeWfcMusicMelodyCells(FLayers[wmplMelody].Tokens);
end;

function TWfcMusicComposition.CopyScore: TWfcMusicScore;
begin
  Result := CloneMusicScore(FScore);
end;

function CreateWfcMusicComposition(const ASeed: TGraphSeed;
  const AQuantumTicks: Integer;
  const AHarmonyTokens, ARhythmTokens,
  AMelodyTokens: TWfcModelTokens;
  const AScore: TWfcMusicScore): TWfcMusicComposition;
begin
  ValidatePublicCompositionInputs(AQuantumTicks, AHarmonyTokens,
    ARhythmTokens, AMelodyTokens, AScore);
  Result := TWfcMusicComposition.CreateInternal(ASeed, AQuantumTicks,
    AHarmonyTokens, ARhythmTokens, AMelodyTokens, AScore);
end;

function ModelForLayer(const AModels: TWfcMusicPassModels;
  const ALayer: TWfcMusicPassLayer): TWfcSequenceModel;
begin
  case ALayer of
    wmplHarmony: Result := AModels.Harmony;
    wmplRhythm: Result := AModels.Rhythm;
    wmplMelody: Result := AModels.Melody;
  else
    raise ERangeError.Create('unknown music pass layer');
  end;
end;

procedure ValidateModelVocabulary(const AModel: TWfcSequenceModel;
  const ALayer: TWfcMusicPassLayer; const AStepsPerOctave: Integer);
var
  I: Integer;
  LHarmony: TWfcMusicHarmonyCell;
begin
  for I := 0 to AModel.PublicTokenCount - 1 do
    try
      case ALayer of
        wmplHarmony:
          begin
            LHarmony := DecodeWfcMusicHarmonyCell(AModel.PublicTokenAt(I));
            if LHarmony.StepsPerOctave <> AStepsPerOctave then
              raise EWfcMusicPasses.CreateFmt(
                'harmony public token %d uses the wrong step system', [I]);
          end;
        wmplRhythm:
          DecodeWfcMusicRhythmCell(AModel.PublicTokenAt(I));
        wmplMelody:
          DecodeWfcMusicMelodyCell(AModel.PublicTokenAt(I));
      end;
    except
      on E: EWfcMusicPasses do
        raise;
      on E: EWfcModel do
        raise EArgumentException.CreateFmt(
          '%s public token %d is not canonical music data: %s',
          [WfcMusicPassLayerName(ALayer), I, E.Message]);
    end;
end;

constructor TWfcMusicPassPipeline.Create(const AConfig: TWfcMusicPassConfig);
begin
  inherited Create;
  Initialize(AConfig);
end;

procedure TWfcMusicPassPipeline.Initialize(
  const AConfig: TWfcMusicPassConfig);
var
  LLayer: TWfcMusicPassLayer;
begin
  if AConfig.QuantumTicks < 1 then
    raise ERangeError.CreateFmt(
      'music pass quantum must be positive [%d]', [AConfig.QuantumTicks]);
  if not Assigned(AConfig.ScoreTemplate) then
    raise EArgumentNilException.Create('music score template cannot be nil');
  if AConfig.ScoreTemplate.VoiceCount <> 1 then
    raise EArgumentException.Create(
      'music pass pipeline v1 requires exactly one score voice');
  if (AConfig.ScoreTemplate.LengthTicks mod AConfig.QuantumTicks) <> 0 then
    raise EArgumentException.Create(
      'music score length is not divisible by the quantum');
  if not Assigned(AConfig.Models.Harmony) then
    raise EArgumentNilException.Create('music harmony model cannot be nil');
  if not Assigned(AConfig.Models.Rhythm) then
    raise EArgumentNilException.Create('music rhythm model cannot be nil');
  if not Assigned(AConfig.Models.Melody) then
    raise EArgumentNilException.Create('music melody model cannot be nil');

  FQuantumTicks := AConfig.QuantumTicks;
  FCellCount := AConfig.ScoreTemplate.LengthTicks div FQuantumTicks;
  if FCellCount < 1 then
    raise EArgumentException.Create('music pass cell count must be positive');
  FModels := AConfig.Models;
  FDirtyLayers := [];
  FPendingComposition := nil;
  FPendingCapture := Default(TWfcMusicPassCaptureReports);
  FPendingValidation := Default(TWfcMusicPassValidationReport);
  FPendingFailedLayer := wmplHarmony;
  FPendingStatus := wmpsNotRun;
  FScoreTemplate := CloneMusicScore(AConfig.ScoreTemplate);
  FGraph := nil;
  try
    for LLayer := Low(TWfcMusicPassLayer) to High(TWfcMusicPassLayer) do
      ValidateModelVocabulary(GetModel(LLayer), LLayer,
        FScoreTemplate.StepsPerOctave);

    FGraph := TWfcMusicPassCommitGraph.Create;
    TWfcMusicPassCommitGraph(FGraph).Owner := Self;
    FGraph.Reshape(FCellCount, 1, 1);
    FGraph.WrapNeighbors := False;
    FGraph.Seed := AConfig.Seed;

    FGraph.CurrentPass := WFC_MUSIC_PASS_HARMONY;
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies;
    ApplySequenceModelToGraph(FModels.Harmony, FGraph, wseWhole);

    FGraph.SwitchToPass(WFC_MUSIC_PASS_RHYTHM);
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies;
    ApplySequenceModelToGraph(FModels.Rhythm, FGraph, wseWhole);

    FGraph.SwitchToPass(WFC_MUSIC_PASS_MELODY);
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies;
    ApplySequenceModelToGraph(FModels.Melody, FGraph, wseWhole);
    RequireWfcMusicMelodyFromPasses(FModels.Melody,
      FModels.Rhythm, FModels.Harmony, FGraph,
      WFC_MUSIC_PASS_RHYTHM, WFC_MUSIC_PASS_HARMONY,
      FScoreTemplate.StepsPerOctave);

    CaptureBaselineDomains;
    FGraph.SwitchToPass(WFC_MUSIC_PASS_HARMONY);
  except
    FGraph.Free;
    FGraph := nil;
    FScoreTemplate.Free;
    FScoreTemplate := nil;
    raise;
  end;
end;

procedure TWfcMusicPassPipeline.CaptureBaselineDomains;
var
  I: Integer;
  LGraph: TGraph;
  LLayer: TWfcMusicPassLayer;
begin
  for LLayer := Low(TWfcMusicPassLayer) to High(TWfcMusicPassLayer) do
  begin
    SetLength(FBaselineDomains[LLayer], FCellCount);
    SetLength(FBaselineHasDomains[LLayer], FCellCount);
    LGraph := GetLayerGraph(LLayer);
    for I := 0 to FCellCount - 1 do
    begin
      FBaselineHasDomains[LLayer][I] :=
        LGraph.HasAllowedValues(I, 0, 0);
      if FBaselineHasDomains[LLayer][I] then
        FBaselineDomains[LLayer][I] :=
          LGraph.CopyAllowedValues(I, 0, 0)
      else
        FBaselineDomains[LLayer][I] := nil;
    end;
  end;
end;

destructor TWfcMusicPassPipeline.Destroy;
begin
  ClearPendingCommit;
  FGraph.Free;
  FScoreTemplate.Free;
  inherited Destroy;
end;

function TWfcMusicPassPipeline.GetLayerGraph(
  const ALayer: TWfcMusicPassLayer): TGraph;
begin
  WfcMusicPassLayerName(ALayer);
  Result := FGraph.PassGraph[Ord(ALayer)];
end;

function TWfcMusicPassPipeline.GetModel(
  const ALayer: TWfcMusicPassLayer): TWfcSequenceModel;
begin
  Result := ModelForLayer(FModels, ALayer);
end;

function TWfcMusicPassPipeline.GetSeed: TGraphSeed;
begin
  Result := FGraph.Seed;
end;

procedure TWfcMusicPassPipeline.SetSeed(const AValue: TGraphSeed);
begin
  FGraph.Seed := AValue;
end;

procedure TWfcMusicPassPipeline.MarkDirty(
  const ALayer: TWfcMusicPassLayer);
begin
  WfcMusicPassLayerName(ALayer);
  Include(FDirtyLayers, ALayer);
end;

function TWfcMusicPassPipeline.IntersectAllowedTokens(
  const ALayer: TWfcMusicPassLayer; const APosition: Integer;
  const ATokens: TWfcModelTokens): TWfcMusicPassPipeline;
begin
  IntersectSequenceAllowedTokens(GetModel(ALayer), GetLayerGraph(ALayer),
    APosition, ATokens);
  MarkDirty(ALayer);
  Result := Self;
end;

function TWfcMusicPassPipeline.IntersectAllowedTokens(
  const ALayer: TWfcMusicPassLayer; const APosition: Integer;
  const AToken: TWfcModelToken): TWfcMusicPassPipeline;
begin
  IntersectSequenceAllowedTokens(GetModel(ALayer), GetLayerGraph(ALayer),
    APosition, AToken);
  MarkDirty(ALayer);
  Result := Self;
end;

function TWfcMusicPassPipeline.IntersectTokenConstraints(
  const ALayer: TWfcMusicPassLayer;
  const AConstraints: TWfcSequenceTokenConstraints): TWfcMusicPassPipeline;
begin
  IntersectSequenceTokenConstraints(GetModel(ALayer), GetLayerGraph(ALayer),
    AConstraints);
  MarkDirty(ALayer);
  Result := Self;
end;

function TWfcMusicPassPipeline.IntersectLockedSpan(
  const ALayer: TWfcMusicPassLayer; const AStart: Integer;
  const ATokens: TWfcModelTokens): TWfcMusicPassPipeline;
begin
  IntersectSequenceLockedSpan(GetModel(ALayer), GetLayerGraph(ALayer),
    AStart, ATokens);
  MarkDirty(ALayer);
  Result := Self;
end;

function TWfcMusicPassPipeline.LockMelodyCells(const AStart: Integer;
  const ACells: TWfcMusicMelodyCells): TWfcMusicPassPipeline;
begin
  Result := IntersectLockedSpan(wmplMelody, AStart,
    EncodeWfcMusicMelodyCells(ACells));
end;

function TWfcMusicPassPipeline.ClearAllowedTokens(
  const ALayer: TWfcMusicPassLayer;
  const APosition: Integer): TWfcMusicPassPipeline;
begin
  if (APosition < 0) or (APosition >= FCellCount) then
    raise ERangeError.CreateFmt(
      'music pass position is out of bounds [%d]', [APosition]);
  GetLayerGraph(ALayer).ClearAllowedValues(APosition, 0, 0);
  if FBaselineHasDomains[ALayer][APosition] then
    GetLayerGraph(ALayer).SetAllowedValues(APosition, 0, 0,
      FBaselineDomains[ALayer][APosition]);
  MarkDirty(ALayer);
  Result := Self;
end;

procedure TWfcMusicPassPipeline.BuildEffectiveRoots(
  const ARoots: TWfcMusicPassLayers; out ALabels: TGraphPassLabels);
var
  I: Integer;
  LCovered: TWfcMusicPassLayerSet;
  LLayer: TWfcMusicPassLayer;
  LSelected: TWfcMusicPassLayerSet;
begin
  if Length(ARoots) < 1 then
    raise EArgumentException.Create(
      'music pass regeneration requires at least one root');
  LSelected := [];
  for I := 0 to Length(ARoots) - 1 do
  begin
    WfcMusicPassLayerName(ARoots[I]);
    Include(LSelected, ARoots[I]);
  end;
  LCovered := LSelected;
  if (wmplHarmony in LCovered) or (wmplRhythm in LCovered) then
    Include(LCovered, wmplMelody);
  { A constraint in an already active descendant is not a second requested
    root. A dirty pass outside the requested closure must become a root so its
    changed caller domain cannot be silently ignored. }
  for LLayer := Low(TWfcMusicPassLayer) to High(TWfcMusicPassLayer) do
    if (LLayer in FDirtyLayers) and not (LLayer in LCovered) then
    begin
      Include(LSelected, LLayer);
      Include(LCovered, LLayer);
      if LLayer in [wmplHarmony, wmplRhythm] then
        Include(LCovered, wmplMelody);
    end;
  ALabels := nil;
  SetLength(ALabels, 0);
  for LLayer := Low(TWfcMusicPassLayer) to High(TWfcMusicPassLayer) do
    if LLayer in LSelected then
    begin
      SetLength(ALabels, Length(ALabels) + 1);
      ALabels[Length(ALabels) - 1] := WfcMusicPassLayerName(LLayer);
    end;
end;

procedure InitializeValidationReport(
  out AReport: TWfcMusicPassValidationReport);
begin
  AReport := Default(TWfcMusicPassValidationReport);
  AReport.Issue.Layer := wmplHarmony;
  AReport.Issue.Position := -1;
  AReport.Issue.SequenceIssue.Position := -1;
  AReport.Issue.SequenceIssue.RelatedPosition := -1;
  AReport.Issue.SequenceIssue.StateIndex := -1;
  AReport.Issue.SequenceIssue.RelatedStateIndex := -1;
  AReport.Issue.Detail := '';
end;

procedure SetValidationIssue(var AReport: TWfcMusicPassValidationReport;
  const AKind: TWfcMusicPassValidationIssueKind;
  const ALayer: TWfcMusicPassLayer; const APosition: Integer);
begin
  AReport.Valid := False;
  AReport.Issue.Kind := AKind;
  AReport.Issue.Layer := ALayer;
  AReport.Issue.Position := APosition;
  AReport.Issue.Detail := '';
end;

procedure SetValidationIssueDetail(
  var AReport: TWfcMusicPassValidationReport;
  const AKind: TWfcMusicPassValidationIssueKind;
  const ALayer: TWfcMusicPassLayer; const APosition: Integer;
  const ADetail: String);
begin
  SetValidationIssue(AReport, AKind, ALayer, APosition);
  AReport.Issue.Detail := ADetail;
end;

function ScoresEqual(const A, B: TWfcMusicScore): Boolean;
begin
  Result := EncodeWfcMusicText(A) = EncodeWfcMusicText(B);
end;

function TWfcMusicPassPipeline.Validate(
  const AComposition: TWfcMusicComposition;
  out AReport: TWfcMusicPassValidationReport): Boolean;
var
  I: Integer;
  LConstraintPosition: Integer;
  LExpectedScore: TWfcMusicScore;
  LGenerated: TWfcGeneratedSequence;
  LHarmony: TWfcMusicHarmonyCell;
  LInvalidPosition: Integer;
  LLayer: TWfcMusicPassLayer;
  LMelody: TWfcMusicMelodyCell;
  LMelodyCells: TWfcMusicMelodyCells;
  LModel: TWfcSequenceModel;
  LRhythm: TWfcMusicRhythmCell;
  LSequenceReport: TWfcSequenceGraphValidationReport;
  LSpans: TWfcMusicSpanEvents;
begin
  InitializeValidationReport(AReport);
  if not Assigned(AComposition) then
  begin
    SetValidationIssue(AReport, wmpvikComposition, wmplHarmony, -1);
    Exit(False);
  end;
  if (AComposition.QuantumTicks <> FQuantumTicks) or
      (AComposition.CellCount <> FCellCount) then
  begin
    SetValidationIssue(AReport, wmpvikLength, wmplMelody, -1);
    Exit(False);
  end;
  if not AComposition.HasLatentCapture then
  begin
    SetValidationIssue(AReport, wmpvikLatentCapture, wmplHarmony, -1);
    Exit(False);
  end;

  for LLayer := Low(TWfcMusicPassLayer) to High(TWfcMusicPassLayer) do
  begin
    LGenerated := AComposition.FLayers[LLayer];
    LModel := GetModel(LLayer);
    if (Length(LGenerated.StateIndices) <> FCellCount) or
        (Length(LGenerated.Tokens) <> FCellCount) then
    begin
      SetValidationIssue(AReport, wmpvikLength, LLayer, -1);
      Exit(False);
    end;
    if LGenerated.Extent <> wseWhole then
    begin
      SetValidationIssue(AReport, wmpvikExtent, LLayer, -1);
      Exit(False);
    end;
    if LGenerated.Boundary <> ExpectedBoundary then
    begin
      SetValidationIssue(AReport, wmpvikBoundary, LLayer, -1);
      Exit(False);
    end;
    if not ValidateSequenceStatePath(LModel, LGenerated.StateIndices,
        wseWhole, LSequenceReport) then
    begin
      SetValidationIssue(AReport, wmpvikStatePath, LLayer,
        LSequenceReport.Issue.Position);
      AReport.Issue.SequenceIssue := LSequenceReport.Issue;
      Exit(False);
    end;
    if not SequenceStatesSatisfyEntryConstraints(LModel,
        GetLayerGraph(LLayer), LGenerated.StateIndices,
        LConstraintPosition) then
    begin
      SetValidationIssue(AReport, wmpvikCallerConstraint, LLayer,
        LConstraintPosition);
      Exit(False);
    end;
    for I := 0 to FCellCount - 1 do
    begin
      if LModel.ProjectStateToken(LGenerated.StateIndices[I]) <>
          LGenerated.Tokens[I] then
      begin
        SetValidationIssue(AReport, wmpvikStateProjection, LLayer, I);
        Exit(False);
      end;
      Inc(AReport.CheckedCells);
    end;
    Inc(AReport.CheckedLayers);
  end;

  LMelodyCells := nil;
  SetLength(LMelodyCells, FCellCount);
  for I := 0 to FCellCount - 1 do
  begin
    try
      LMelody := DecodeWfcMusicMelodyCell(
        AComposition.FLayers[wmplMelody].Tokens[I]);
      LRhythm := DecodeWfcMusicRhythmCell(
        AComposition.FLayers[wmplRhythm].Tokens[I]);
      LHarmony := DecodeWfcMusicHarmonyCell(
        AComposition.FLayers[wmplHarmony].Tokens[I]);
    except
      on EWfcModel do
      begin
        SetValidationIssue(AReport, wmpvikCell, wmplMelody, I);
        Exit(False);
      end;
    end;
    if LMelody.Action <> LRhythm.Action then
    begin
      SetValidationIssue(AReport, wmpvikRhythmProjection, wmplMelody, I);
      Exit(False);
    end;
    Inc(AReport.CheckedRelations);
    if (LHarmony.StepsPerOctave <> FScoreTemplate.StepsPerOctave) or
        ((LMelody.Action <> wmcaRest) and
         ((LHarmony.Kind <> wmhckPitchClass) or
          (LHarmony.PitchClass <> (LMelody.Pitch mod
            FScoreTemplate.StepsPerOctave)))) then
    begin
      SetValidationIssue(AReport, wmpvikHarmonyProjection, wmplMelody, I);
      Exit(False);
    end;
    Inc(AReport.CheckedRelations);
    LMelodyCells[I] := LMelody;
  end;

  if not MelodyContinuationIsValid(LMelodyCells,
      LInvalidPosition) then
  begin
    SetValidationIssue(AReport, wmpvikMelodyContinuation,
      wmplMelody, LInvalidPosition);
    Exit(False);
  end;

  try
    LSpans := RebuildWfcMusicVoiceSpans(LMelodyCells, 0, FQuantumTicks);
    LExpectedScore := TWfcMusicScore.Create(FScoreTemplate.TicksPerQuarter,
      FScoreTemplate.StepsPerOctave, FScoreTemplate.LengthTicks,
      FScoreTemplate.CopyTracks, FScoreTemplate.CopyVoices,
      FScoreTemplate.CopyMeters, FScoreTemplate.CopyTempos, LSpans);
    try
      if not ScoresEqual(LExpectedScore, AComposition.FScore) then
      begin
        SetValidationIssue(AReport, wmpvikScore, wmplMelody, -1);
        Exit(False);
      end;
    finally
      LExpectedScore.Free;
    end;
  except
    on E: EWfcModel do
    begin
      SetValidationIssueDetail(AReport, wmpvikScore, wmplMelody, -1,
        'canonical score comparison failed');
      Exit(False);
    end;
  end;

  if AComposition.Signature <>
      CalculateWfcMusicCompositionSignature(AComposition) then
  begin
    SetValidationIssue(AReport, wmpvikSignature, wmplMelody, -1);
    Exit(False);
  end;
  AReport.Valid := True;
  AReport.Issue.Kind := wmpvikNone;
  Result := True;
end;

function TWfcMusicPassPipeline.CaptureComposition(const ASeed: TGraphSeed;
  out AComposition: TWfcMusicComposition;
  out ACapture: TWfcMusicPassCaptureReports;
  out AValidation: TWfcMusicPassValidationReport;
  out AFailedLayer: TWfcMusicPassLayer;
  out AStatus: TWfcMusicPassStatus): Boolean;
var
  I: Integer;
  LComposition: TWfcMusicComposition;
  LGenerated: TWfcMusicGeneratedLayers;
  LInvalidPosition: Integer;
  LLayer: TWfcMusicPassLayer;
  LMelodyCells: TWfcMusicMelodyCells;
  LScore: TWfcMusicScore;
  LSpans: TWfcMusicSpanEvents;
begin
  AComposition := nil;
  ACapture := Default(TWfcMusicPassCaptureReports);
  InitializeValidationReport(AValidation);
  AFailedLayer := wmplHarmony;
  AStatus := wmpsNotRun;
  LScore := nil;
  LComposition := nil;

  for LLayer := Low(TWfcMusicPassLayer) to High(TWfcMusicPassLayer) do
    if not CaptureSolvedSequence(GetModel(LLayer), GetLayerGraph(LLayer),
        wseWhole, LGenerated[LLayer], ACapture[LLayer]) then
    begin
      AFailedLayer := LLayer;
      AStatus := wmpsCaptureFailed;
      Exit(False);
    end;

  try
    SetLength(LMelodyCells, Length(LGenerated[wmplMelody].Tokens));
    for I := 0 to Length(LGenerated[wmplMelody].Tokens) - 1 do
      try
        LMelodyCells[I] := DecodeWfcMusicMelodyCell(
          LGenerated[wmplMelody].Tokens[I]);
      except
        on E: EWfcModel do
        begin
          SetValidationIssueDetail(AValidation, wmpvikCell,
            wmplMelody, I, 'canonical melody token decoding failed');
          AFailedLayer := wmplMelody;
          AStatus := wmpsValidationFailed;
          Exit(False);
        end;
      end;
    if not MelodyContinuationIsValid(LMelodyCells,
        LInvalidPosition) then
    begin
      SetValidationIssue(AValidation, wmpvikMelodyContinuation,
        wmplMelody, LInvalidPosition);
      AFailedLayer := wmplMelody;
      AStatus := wmpsValidationFailed;
      Exit(False);
    end;

    try
      LSpans := RebuildWfcMusicVoiceSpans(LMelodyCells, 0, FQuantumTicks);
    except
      on E: EWfcModel do
      begin
        SetValidationIssueDetail(AValidation, wmpvikScore,
          wmplMelody, -1, 'melody span rebuild failed');
        AFailedLayer := wmplMelody;
        AStatus := wmpsValidationFailed;
        Exit(False);
      end;
    end;

    try
      LScore := TWfcMusicScore.Create(FScoreTemplate.TicksPerQuarter,
        FScoreTemplate.StepsPerOctave, FScoreTemplate.LengthTicks,
        FScoreTemplate.CopyTracks, FScoreTemplate.CopyVoices,
        FScoreTemplate.CopyMeters, FScoreTemplate.CopyTempos, LSpans);
    except
      on E: EWfcModel do
      begin
        SetValidationIssueDetail(AValidation, wmpvikScore,
          wmplMelody, -1, 'canonical score construction failed');
        AFailedLayer := wmplMelody;
        AStatus := wmpsValidationFailed;
        Exit(False);
      end;
    end;

    try
      LComposition := TWfcMusicComposition.CreateInternal(
        ASeed, FQuantumTicks,
        LGenerated[wmplHarmony].Tokens, LGenerated[wmplRhythm].Tokens,
        LGenerated[wmplMelody].Tokens, LScore);
    except
      on E: EWfcModel do
      begin
        SetValidationIssueDetail(AValidation, wmpvikSignature,
          wmplMelody, -1, 'immutable composition construction failed');
        AFailedLayer := wmplMelody;
        AStatus := wmpsValidationFailed;
        Exit(False);
      end;
    end;

    try
      LComposition.AttachLatentCapture(LGenerated);
    except
      on E: EWfcModel do
      begin
        SetValidationIssueDetail(AValidation, wmpvikLatentCapture,
          wmplMelody, -1, 'latent capture attachment failed');
        AFailedLayer := wmplMelody;
        AStatus := wmpsValidationFailed;
        Exit(False);
      end;
    end;

    try
      if not Validate(LComposition, AValidation) then
      begin
        AFailedLayer := AValidation.Issue.Layer;
        AStatus := wmpsValidationFailed;
        Exit(False);
      end;
      AComposition := LComposition;
      LComposition := nil;
      AStatus := wmpsCompleted;
      Result := True;
    except
      on E: EWfcModel do
      begin
        SetValidationIssueDetail(AValidation, wmpvikInternal,
          wmplMelody, -1, 'independent composition validation raised');
        AFailedLayer := wmplMelody;
        AStatus := wmpsValidationFailed;
        Result := False;
      end;
    end;
  finally
    LComposition.Free;
    LScore.Free;
  end;
end;

procedure TWfcMusicPassPipeline.ClearPendingCommit;
begin
  FPendingComposition.Free;
  FPendingComposition := nil;
  FPendingCapture := Default(TWfcMusicPassCaptureReports);
  InitializeValidationReport(FPendingValidation);
  FPendingFailedLayer := wmplHarmony;
  FPendingStatus := wmpsNotRun;
end;

procedure TWfcMusicPassPipeline.CopyPendingFailure(
  out ACapture: TWfcMusicPassCaptureReports;
  out AValidation: TWfcMusicPassValidationReport;
  out AFailedLayer: TWfcMusicPassLayer;
  out AStatus: TWfcMusicPassStatus);
begin
  ACapture := FPendingCapture;
  AValidation := FPendingValidation;
  AFailedLayer := FPendingFailedLayer;
  AStatus := FPendingStatus;
  if AStatus = wmpsNotRun then
    AStatus := wmpsValidationFailed;
end;

function TWfcMusicPassPipeline.TakePendingComposition(
  out AComposition: TWfcMusicComposition;
  out ACapture: TWfcMusicPassCaptureReports;
  out AValidation: TWfcMusicPassValidationReport;
  out AFailedLayer: TWfcMusicPassLayer;
  out AStatus: TWfcMusicPassStatus): Boolean;
begin
  AComposition := FPendingComposition;
  FPendingComposition := nil;
  ACapture := FPendingCapture;
  AValidation := FPendingValidation;
  AFailedLayer := FPendingFailedLayer;
  AStatus := FPendingStatus;
  Result := Assigned(AComposition) and (AStatus = wmpsCompleted);
  if not Result then
  begin
    AComposition.Free;
    AComposition := nil;
    if AStatus = wmpsNotRun then
    begin
      SetValidationIssueDetail(AValidation, wmpvikInternal,
        wmplMelody, -1,
        'graph reported success without a validated pending composition');
      AFailedLayer := wmplMelody;
      AStatus := wmpsValidationFailed;
    end;
  end;
end;

function TWfcMusicPassPipeline.ValidatePendingCommit(
  out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
begin
  ClearPendingCommit;
  Result := CaptureComposition(FGraph.Seed, FPendingComposition,
    FPendingCapture, FPendingValidation, FPendingFailedLayer,
    FPendingStatus);
  if Result then
  begin
    AFailedPassIndex := -1;
    AFailedEntryIndex := -1;
    Exit;
  end;

  AFailedPassIndex := Ord(FPendingFailedLayer);
  if FPendingStatus = wmpsCaptureFailed then
    AFailedEntryIndex :=
      FPendingCapture[FPendingFailedLayer].Issue.Position
  else
    AFailedEntryIndex := FPendingValidation.Issue.Position;
end;

function TWfcMusicPassPipeline.TryGenerate(
  const AOptions: TGraphSolveOptions;
  out AComposition: TWfcMusicComposition;
  out AReport: TWfcMusicPassReport): Boolean;
var
  LSolved: Boolean;
begin
  AComposition := nil;
  AReport := Default(TWfcMusicPassReport);
  AReport.Status := wmpsNotRun;
  AReport.FailedLayer := wmplHarmony;
  ClearPendingCommit;
  try
    LSolved := FGraph.TrySolve(AOptions, AReport.Solve);
  except
    ClearPendingCommit;
    raise;
  end;
  if not LSolved then
  begin
    if AReport.Solve.Contradiction.Kind = gckFinalValidation then
      CopyPendingFailure(AReport.Capture, AReport.Validation,
        AReport.FailedLayer, AReport.Status)
    else
    begin
      AReport.Status := wmpsSolveFailed;
      if (AReport.Solve.FailedPassIndex >=
          Ord(Low(TWfcMusicPassLayer))) and
          (AReport.Solve.FailedPassIndex <=
            Ord(High(TWfcMusicPassLayer))) then
        AReport.FailedLayer :=
          TWfcMusicPassLayer(AReport.Solve.FailedPassIndex);
    end;
    ClearPendingCommit;
    Exit(False);
  end;
  Result := TakePendingComposition(AComposition,
    AReport.Capture, AReport.Validation, AReport.FailedLayer,
    AReport.Status);
  if not Result then
    raise EWfcMusicPasses.Create(
      'music graph committed without a validated composition');
  FDirtyLayers := [];
end;

function TWfcMusicPassPipeline.TryGenerate(
  out AComposition: TWfcMusicComposition;
  out AReport: TWfcMusicPassReport): Boolean;
var
  LOptions: TGraphSolveOptions;
begin
  LOptions := DefaultGraphSolveOptions;
  Result := TryGenerate(LOptions, AComposition, AReport);
end;

function TWfcMusicPassPipeline.TryRegenerateFrom(
  const ALayer: TWfcMusicPassLayer; const AOptions: TGraphSolveOptions;
  out AComposition: TWfcMusicComposition;
  out AReport: TWfcMusicPassReport): Boolean;
var
  LLayers: TWfcMusicPassLayers;
begin
  SetLength(LLayers, 1);
  LLayers[0] := ALayer;
  Result := TryRegenerateFrom(LLayers, AOptions, AComposition, AReport);
end;

function TWfcMusicPassPipeline.TryRegenerateFrom(
  const ALayers: TWfcMusicPassLayers; const AOptions: TGraphSolveOptions;
  out AComposition: TWfcMusicComposition;
  out AReport: TWfcMusicPassReport): Boolean;
var
  LLabels: TGraphPassLabels;
  LSolved: Boolean;
begin
  AComposition := nil;
  AReport := Default(TWfcMusicPassReport);
  AReport.Status := wmpsNotRun;
  AReport.FailedLayer := wmplHarmony;
  BuildEffectiveRoots(ALayers, LLabels);
  ClearPendingCommit;
  try
    LSolved := FGraph.TryRegenerateFrom(LLabels, AOptions, AReport.Solve);
  except
    ClearPendingCommit;
    raise;
  end;
  if not LSolved then
  begin
    if AReport.Solve.Contradiction.Kind = gckFinalValidation then
      CopyPendingFailure(AReport.Capture, AReport.Validation,
        AReport.FailedLayer, AReport.Status)
    else
    begin
      AReport.Status := wmpsSolveFailed;
      if (AReport.Solve.FailedPassIndex >=
          Ord(Low(TWfcMusicPassLayer))) and
          (AReport.Solve.FailedPassIndex <=
            Ord(High(TWfcMusicPassLayer))) then
        AReport.FailedLayer :=
          TWfcMusicPassLayer(AReport.Solve.FailedPassIndex);
    end;
    ClearPendingCommit;
    Exit(False);
  end;
  Result := TakePendingComposition(AComposition,
    AReport.Capture, AReport.Validation, AReport.FailedLayer,
    AReport.Status);
  if not Result then
    raise EWfcMusicPasses.Create(
      'music graph committed without a validated composition');
  FDirtyLayers := [];
end;

function TWfcMusicPassPipeline.TryGenerateNegotiated(
  const AOptions: TGraphNegotiationOptions;
  out AComposition: TWfcMusicComposition;
  out AReport: TWfcMusicPassNegotiationReport): Boolean;
var
  LSolved: Boolean;
begin
  AComposition := nil;
  AReport := Default(TWfcMusicPassNegotiationReport);
  AReport.Status := wmpsNotRun;
  AReport.FailedLayer := wmplHarmony;
  ClearPendingCommit;
  try
    LSolved := FGraph.TrySolveNegotiated(AOptions, AReport.Search);
  except
    ClearPendingCommit;
    raise;
  end;
  if not LSolved then
  begin
    if AReport.Search.FinalReport.Contradiction.Kind =
        gckFinalValidation then
      CopyPendingFailure(AReport.Capture, AReport.Validation,
        AReport.FailedLayer, AReport.Status)
    else
    begin
      AReport.Status := wmpsSolveFailed;
      if (AReport.Search.FinalReport.FailedPassIndex >=
          Ord(Low(TWfcMusicPassLayer))) and
          (AReport.Search.FinalReport.FailedPassIndex <=
            Ord(High(TWfcMusicPassLayer))) then
        AReport.FailedLayer := TWfcMusicPassLayer(
          AReport.Search.FinalReport.FailedPassIndex);
    end;
    ClearPendingCommit;
    Exit(False);
  end;
  Result := TakePendingComposition(AComposition,
    AReport.Capture, AReport.Validation, AReport.FailedLayer,
    AReport.Status);
  if not Result then
    raise EWfcMusicPasses.Create(
      'music graph committed without a validated composition');
  FDirtyLayers := [];
end;

function TWfcMusicPassPipeline.TryGenerateNegotiated(
  out AComposition: TWfcMusicComposition;
  out AReport: TWfcMusicPassNegotiationReport): Boolean;
var
  LOptions: TGraphNegotiationOptions;
begin
  LOptions := DefaultGraphNegotiationOptions;
  Result := TryGenerateNegotiated(LOptions, AComposition, AReport);
end;

function TWfcMusicPassPipeline.TryRegenerateNegotiatedFrom(
  const ALayer: TWfcMusicPassLayer;
  const AOptions: TGraphNegotiationOptions;
  out AComposition: TWfcMusicComposition;
  out AReport: TWfcMusicPassSelectiveNegotiationReport): Boolean;
var
  LLayers: TWfcMusicPassLayers;
begin
  SetLength(LLayers, 1);
  LLayers[0] := ALayer;
  Result := TryRegenerateNegotiatedFrom(LLayers, AOptions,
    AComposition, AReport);
end;

function TWfcMusicPassPipeline.TryRegenerateNegotiatedFrom(
  const ALayers: TWfcMusicPassLayers;
  const AOptions: TGraphNegotiationOptions;
  out AComposition: TWfcMusicComposition;
  out AReport: TWfcMusicPassSelectiveNegotiationReport): Boolean;
var
  LLabels: TGraphPassLabels;
  LSolved: Boolean;
begin
  AComposition := nil;
  AReport := Default(TWfcMusicPassSelectiveNegotiationReport);
  AReport.Status := wmpsNotRun;
  AReport.FailedLayer := wmplHarmony;
  BuildEffectiveRoots(ALayers, LLabels);
  ClearPendingCommit;
  try
    LSolved := FGraph.TryRegenerateNegotiatedFrom(LLabels, AOptions,
      AReport.Search);
  except
    ClearPendingCommit;
    raise;
  end;
  if not LSolved then
  begin
    if AReport.Search.Search.FinalReport.Contradiction.Kind =
        gckFinalValidation then
      CopyPendingFailure(AReport.Capture, AReport.Validation,
        AReport.FailedLayer, AReport.Status)
    else
    begin
      AReport.Status := wmpsSolveFailed;
      if (AReport.Search.Search.FinalReport.FailedPassIndex >=
          Ord(Low(TWfcMusicPassLayer))) and
          (AReport.Search.Search.FinalReport.FailedPassIndex <=
            Ord(High(TWfcMusicPassLayer))) then
        AReport.FailedLayer := TWfcMusicPassLayer(
          AReport.Search.Search.FinalReport.FailedPassIndex);
    end;
    ClearPendingCommit;
    Exit(False);
  end;
  Result := TakePendingComposition(AComposition,
    AReport.Capture, AReport.Validation, AReport.FailedLayer,
    AReport.Status);
  if not Result then
    raise EWfcMusicPasses.Create(
      'music graph committed without a validated composition');
  FDirtyLayers := [];
end;

function TWfcMusicPassPipeline.CopyScoreTemplate: TWfcMusicScore;
begin
  Result := CloneMusicScore(FScoreTemplate);
end;

function TWfcMusicPassPipeline.TryCopyCommittedLayer(
  const ALayer: TWfcMusicPassLayer;
  out AGenerated: TWfcGeneratedSequence;
  out AReport: TWfcSequenceGraphValidationReport): Boolean;
begin
  Result := CaptureSolvedSequence(GetModel(ALayer),
    GetLayerGraph(ALayer), wseWhole, AGenerated, AReport);
end;

function TWfcMusicPassPipeline.CopyCommittedTokens(
  const ALayer: TWfcMusicPassLayer): TWfcModelTokens;
var
  LGenerated: TWfcGeneratedSequence;
  LReport: TWfcSequenceGraphValidationReport;
begin
  if not TryCopyCommittedLayer(ALayer, LGenerated, LReport) then
    raise EWfcMusicPasses.CreateFmt(
      'cannot copy committed %s pass: %s',
      [WfcMusicPassLayerName(ALayer),
       DescribeSequenceGraphIssue(LReport.Issue)]);
  Result := CopyTokens(LGenerated.Tokens);
end;

function DescribeWfcMusicPassValidationIssue(
  const AIssue: TWfcMusicPassValidationIssue): String;
begin
  case AIssue.Kind of
    wmpvikNone:
      Result := 'no music pass validation issue';
    wmpvikComposition:
      Result := 'music composition is not assigned';
    wmpvikLength:
      Result := Format('%s pass has the wrong cell length',
        [WfcMusicPassLayerName(AIssue.Layer)]);
    wmpvikExtent:
      Result := Format('%s pass has the wrong sequence extent',
        [WfcMusicPassLayerName(AIssue.Layer)]);
    wmpvikBoundary:
      Result := Format('%s pass has the wrong sequence boundary',
        [WfcMusicPassLayerName(AIssue.Layer)]);
    wmpvikLatentCapture:
      Result := 'music composition has no latent pipeline capture';
    wmpvikStatePath:
      Result := Format('%s pass state path failed at %d: %s',
        [WfcMusicPassLayerName(AIssue.Layer), AIssue.Position,
         DescribeSequenceGraphIssue(AIssue.SequenceIssue)]);
    wmpvikStateProjection:
      Result := Format('%s pass state/token projection failed at %d',
        [WfcMusicPassLayerName(AIssue.Layer), AIssue.Position]);
    wmpvikCallerConstraint:
      Result := Format('%s pass violates a caller constraint at %d',
        [WfcMusicPassLayerName(AIssue.Layer), AIssue.Position]);
    wmpvikCell:
      Result := Format('music cell decoding failed at %d',
        [AIssue.Position]);
    wmpvikMelodyContinuation:
      Result := Format('music melody continuation failed at %d',
        [AIssue.Position]);
    wmpvikRhythmProjection:
      Result := Format('music rhythm projection failed at %d',
        [AIssue.Position]);
    wmpvikHarmonyProjection:
      Result := Format('music harmony projection failed at %d',
        [AIssue.Position]);
    wmpvikScore:
      Result := 'music composition score does not match its melody cells';
    wmpvikSignature:
      Result := 'music composition signature does not recompute';
    wmpvikInternal:
      Result := 'music composition validation failed internally';
  else
    Result := 'unknown music pass validation issue';
  end;
  if AIssue.Detail <> '' then
    Result := Result + ': ' + AIssue.Detail;
end;

end.
