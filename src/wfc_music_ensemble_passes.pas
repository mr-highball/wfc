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
unit wfc_music_ensemble_passes;

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
  wfc_music_ensemble, wfc_music_ensemble_graph;

const
  WFC_MUSIC_ENSEMBLE_PIPELINE_VERSION = 1;
  WFC_MUSIC_ENSEMBLE_VALIDATION_VERSION = 1;
  WFC_MUSIC_ENSEMBLE_SIGNATURE_VERSION = 1;

  WFC_MUSIC_ENSEMBLE_PASS_HARMONY = 'harmony';
  WFC_MUSIC_ENSEMBLE_PASS_RHYTHM = 'rhythm';
  WFC_MUSIC_ENSEMBLE_PASS_ENSEMBLE = 'ensemble';

type
  EWfcMusicEnsemblePasses = class(EWfcMusicEnsembleGraph);

  TWfcMusicEnsembleLayer = (
    wmelHarmony,
    wmelRhythm,
    wmelEnsemble
  );
  TWfcMusicEnsembleLayers = array of TWfcMusicEnsembleLayer;
  TWfcMusicEnsembleLayerSet = set of TWfcMusicEnsembleLayer;

  TWfcMusicEnsembleModels = record
    Harmony: TWfcSequenceModel;
    Rhythm: TWfcSequenceModel;
    Ensemble: TWfcSequenceModel;
  end;

  TWfcMusicEnsembleCompositionSignature = Cardinal;

  TWfcMusicEnsembleGeneratedLayers =
    array[TWfcMusicEnsembleLayer] of TWfcGeneratedSequence;
  TWfcMusicEnsembleCaptureReports =
    array[TWfcMusicEnsembleLayer] of TWfcSequenceGraphValidationReport;

  TWfcMusicEnsembleValidationIssueKind = (
    wmevikNone,
    wmevikComposition,
    wmevikLength,
    wmevikExtent,
    wmevikBoundary,
    wmevikLatentCapture,
    wmevikStatePath,
    wmevikStateProjection,
    wmevikCallerConstraint,
    wmevikCell,
    wmevikEnsembleContinuation,
    wmevikRhythmProjection,
    wmevikHarmonyProjection,
    wmevikScore,
    wmevikSignature,
    wmevikInternal
  );

  TWfcMusicEnsembleValidationIssue = record
    Kind: TWfcMusicEnsembleValidationIssueKind;
    Layer: TWfcMusicEnsembleLayer;
    Position: Integer;
    SequenceIssue: TWfcSequenceGraphIssue;
    Detail: String;
  end;

  TWfcMusicEnsembleValidationReport = record
    Valid: Boolean;
    CheckedLayers: Integer;
    CheckedCells: Integer;
    CheckedRelations: Integer;
    Issue: TWfcMusicEnsembleValidationIssue;
  end;

  TWfcMusicEnsembleComposition = class;
  { The receiver is borrowed and must outlive its pipeline. Validation is
    synchronous and observational: do not retain the borrowed composition,
    change generation inputs, or reenter the pipeline. False rejects the
    candidate while the graph transaction can still restore entries and RNG. }
  TWfcMusicEnsembleCompositionValidator = function(
    const Composition: TWfcMusicEnsembleComposition;
    out Issue: TWfcMusicEnsembleValidationIssue): Boolean of object;

  { ScoreTemplate and InitialTokenConstraints are read only during construction.
    The pipeline owns detached score/domain baselines. Models and the optional
    validator receiver remain caller-owned and must outlive the pipeline. }
  TWfcMusicEnsembleConfig = record
    QuantumTicks: Integer;
    Seed: TGraphSeed;
    Models: TWfcMusicEnsembleModels;
    ScoreTemplate: TWfcMusicScore;
    Extent: TWfcSequenceExtent;
    HarmonyMode: TWfcMusicEnsembleHarmonyMode;
    InitialTokenConstraints:
      array[TWfcMusicEnsembleLayer] of TWfcSequenceTokenConstraints;
    ValidateComposition: TWfcMusicEnsembleCompositionValidator;
  end;

  TWfcMusicEnsembleStatus = (
    wmesNotRun,
    wmesCompleted,
    wmesSolveFailed,
    wmesCaptureFailed,
    wmesValidationFailed
  );

  TWfcMusicEnsembleReport = record
    Status: TWfcMusicEnsembleStatus;
    FailedLayer: TWfcMusicEnsembleLayer;
    Solve: TGraphSolveReport;
    Capture: TWfcMusicEnsembleCaptureReports;
    Validation: TWfcMusicEnsembleValidationReport;
  end;

  TWfcMusicEnsembleNegotiationReport = record
    Status: TWfcMusicEnsembleStatus;
    FailedLayer: TWfcMusicEnsembleLayer;
    Search: TGraphNegotiationReport;
    Capture: TWfcMusicEnsembleCaptureReports;
    Validation: TWfcMusicEnsembleValidationReport;
  end;

  TWfcMusicEnsembleSelectiveNegotiationReport = record
    Status: TWfcMusicEnsembleStatus;
    FailedLayer: TWfcMusicEnsembleLayer;
    Search: TGraphSelectiveNegotiationReport;
    Capture: TWfcMusicEnsembleCaptureReports;
    Validation: TWfcMusicEnsembleValidationReport;
  end;

  { An immutable public composition. Copy methods detach all managed arrays and
    return a newly owned score. Public-artifact instances have no latent state
    capture; pipeline results additionally retain a checked detached capture. }
  TWfcMusicEnsembleComposition = class
  private
    FSeed: TGraphSeed;
    FQuantumTicks: Integer;
    FExtent: TWfcSequenceExtent;
    FHarmonyMode: TWfcMusicEnsembleHarmonyMode;
    FLayers: TWfcMusicEnsembleGeneratedLayers;
    FScore: TWfcMusicScore;
    FSignature: TWfcMusicEnsembleCompositionSignature;
    FHasLatentCapture: Boolean;
    constructor CreateInternal(const ASeed: TGraphSeed;
      const AQuantumTicks: Integer;
      const AExtent: TWfcSequenceExtent;
      const AHarmonyMode: TWfcMusicEnsembleHarmonyMode;
      const AHarmonyTokens, ARhythmTokens,
      AEnsembleTokens: TWfcModelTokens;
      const AScore: TWfcMusicScore);
    procedure AttachLatentCapture(
      const ALayers: TWfcMusicEnsembleGeneratedLayers);
    function GetCellCount: Integer;
  public
    destructor Destroy; override;
    function CopyGenerated(
      const ALayer: TWfcMusicEnsembleLayer): TWfcGeneratedSequence;
    function CopyHarmonyCells: TWfcMusicPitchClassSets;
    function CopyRhythmCells: TWfcMusicRhythmFrames;
    function CopyEnsembleFrames: TWfcMusicEnsembleFrames;
    function CopyScore: TWfcMusicScore;

    property Seed: TGraphSeed read FSeed;
    property QuantumTicks: Integer read FQuantumTicks;
    property Extent: TWfcSequenceExtent read FExtent;
    property HarmonyMode: TWfcMusicEnsembleHarmonyMode read FHarmonyMode;
    property CellCount: Integer read GetCellCount;
    property Signature: TWfcMusicEnsembleCompositionSignature read FSignature;
    property HasLatentCapture: Boolean read FHasLatentCapture;
  end;

  { A reusable owner for harmony and rhythm providers joined by an ensemble pass.
    Public constraints are expanded through the existing latent sequence
    adapter. Solving and negotiation remain the TGraph implementations. }
  TWfcMusicEnsemblePipeline = class
  strict private
    FCellCount: Integer;
    FDirtyLayers: TWfcMusicEnsembleLayerSet;
    FGraph: TGraph;
    FModels: TWfcMusicEnsembleModels;
    FPendingCapture: TWfcMusicEnsembleCaptureReports;
    FPendingComposition: TWfcMusicEnsembleComposition;
    FPendingFailedLayer: TWfcMusicEnsembleLayer;
    FPendingStatus: TWfcMusicEnsembleStatus;
    FPendingValidation: TWfcMusicEnsembleValidationReport;
    FQuantumTicks: Integer;
    FExtent: TWfcSequenceExtent;
    FHarmonyMode: TWfcMusicEnsembleHarmonyMode;
    FScoreTemplate: TWfcMusicScore;
    FValidateComposition: TWfcMusicEnsembleCompositionValidator;
    FBaselineDomains:
      array[TWfcMusicEnsembleLayer] of array of TGraphValues;
    FBaselineHasDomains:
      array[TWfcMusicEnsembleLayer] of array of Boolean;

    procedure Initialize(const AConfig: TWfcMusicEnsembleConfig);
    procedure CaptureBaselineDomains;
    procedure MarkDirty(const ALayer: TWfcMusicEnsembleLayer);
    function GetLayerGraph(const ALayer: TWfcMusicEnsembleLayer): TGraph;
    function GetModel(const ALayer: TWfcMusicEnsembleLayer): TWfcSequenceModel;
    function GetSeed: TGraphSeed;
    procedure SetSeed(const AValue: TGraphSeed);
    procedure BuildEffectiveRoots(const ARoots: TWfcMusicEnsembleLayers;
      out ALabels: TGraphPassLabels);
    procedure ClearPendingCommit;
    procedure CopyPendingFailure(out ACapture: TWfcMusicEnsembleCaptureReports;
      out AValidation: TWfcMusicEnsembleValidationReport;
      out AFailedLayer: TWfcMusicEnsembleLayer;
      out AStatus: TWfcMusicEnsembleStatus);
    function TakePendingComposition(out AComposition: TWfcMusicEnsembleComposition;
      out ACapture: TWfcMusicEnsembleCaptureReports;
      out AValidation: TWfcMusicEnsembleValidationReport;
      out AFailedLayer: TWfcMusicEnsembleLayer;
      out AStatus: TWfcMusicEnsembleStatus): Boolean;
    function CaptureComposition(const ASeed: TGraphSeed;
      out AComposition: TWfcMusicEnsembleComposition;
      out ACapture: TWfcMusicEnsembleCaptureReports;
      out AValidation: TWfcMusicEnsembleValidationReport;
      out AFailedLayer: TWfcMusicEnsembleLayer;
      out AStatus: TWfcMusicEnsembleStatus): Boolean;
  private
    function ValidatePendingCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean;
  public
    constructor Create(const AConfig: TWfcMusicEnsembleConfig);
    destructor Destroy; override;

    function IntersectAllowedTokens(const ALayer: TWfcMusicEnsembleLayer;
      const APosition: Integer;
      const ATokens: TWfcModelTokens): TWfcMusicEnsemblePipeline; overload;
    function IntersectAllowedTokens(const ALayer: TWfcMusicEnsembleLayer;
      const APosition: Integer;
      const AToken: TWfcModelToken): TWfcMusicEnsemblePipeline; overload;
    function IntersectTokenConstraints(const ALayer: TWfcMusicEnsembleLayer;
      const AConstraints: TWfcSequenceTokenConstraints):
      TWfcMusicEnsemblePipeline;
    function IntersectLockedSpan(const ALayer: TWfcMusicEnsembleLayer;
      const AStart: Integer; const ATokens: TWfcModelTokens):
      TWfcMusicEnsemblePipeline;
    function LockEnsembleFrames(const AStart: Integer;
      const ACells: TWfcMusicEnsembleFrames): TWfcMusicEnsemblePipeline;
    { Match one ordered voice's complete action, pitches, and velocities in
      each synchronized frame. Other voices remain unconstrained. The whole
      request is validated before applying any restriction; a valid cell not
      represented by the vocabulary creates an explicit empty domain. }
    function LockVoiceCells(const AVoiceIndex, AStart: Integer;
      const ACells: TWfcMusicVoiceCells): TWfcMusicEnsemblePipeline;
    function ClearAllowedTokens(const ALayer: TWfcMusicEnsembleLayer;
      const APosition: Integer): TWfcMusicEnsemblePipeline;

    function TryGenerate(const AOptions: TGraphSolveOptions;
      out AComposition: TWfcMusicEnsembleComposition;
      out AReport: TWfcMusicEnsembleReport): Boolean; overload;
    function TryGenerate(out AComposition: TWfcMusicEnsembleComposition;
      out AReport: TWfcMusicEnsembleReport): Boolean; overload;

    function TryRegenerateFrom(const ALayer: TWfcMusicEnsembleLayer;
      const AOptions: TGraphSolveOptions;
      out AComposition: TWfcMusicEnsembleComposition;
      out AReport: TWfcMusicEnsembleReport): Boolean; overload;
    function TryRegenerateFrom(const ALayers: TWfcMusicEnsembleLayers;
      const AOptions: TGraphSolveOptions;
      out AComposition: TWfcMusicEnsembleComposition;
      out AReport: TWfcMusicEnsembleReport): Boolean; overload;

    function TryGenerateNegotiated(const AOptions: TGraphNegotiationOptions;
      out AComposition: TWfcMusicEnsembleComposition;
      out AReport: TWfcMusicEnsembleNegotiationReport): Boolean; overload;
    function TryGenerateNegotiated(out AComposition: TWfcMusicEnsembleComposition;
      out AReport: TWfcMusicEnsembleNegotiationReport): Boolean; overload;

    function TryRegenerateNegotiatedFrom(
      const ALayer: TWfcMusicEnsembleLayer;
      const AOptions: TGraphNegotiationOptions;
      out AComposition: TWfcMusicEnsembleComposition;
      out AReport: TWfcMusicEnsembleSelectiveNegotiationReport): Boolean; overload;
    function TryRegenerateNegotiatedFrom(
      const ALayers: TWfcMusicEnsembleLayers;
      const AOptions: TGraphNegotiationOptions;
      out AComposition: TWfcMusicEnsembleComposition;
      out AReport: TWfcMusicEnsembleSelectiveNegotiationReport): Boolean; overload;

    function Validate(const AComposition: TWfcMusicEnsembleComposition;
      out AReport: TWfcMusicEnsembleValidationReport): Boolean;
    function TryCopyCommittedLayer(const ALayer: TWfcMusicEnsembleLayer;
      out AGenerated: TWfcGeneratedSequence;
      out AReport: TWfcSequenceGraphValidationReport): Boolean;
    function CopyCommittedTokens(
      const ALayer: TWfcMusicEnsembleLayer): TWfcModelTokens;
    function CopyScoreTemplate: TWfcMusicScore;

    property Model[const ALayer: TWfcMusicEnsembleLayer]: TWfcSequenceModel
      read GetModel;
    property CellCount: Integer read FCellCount;
    property QuantumTicks: Integer read FQuantumTicks;
    property Extent: TWfcSequenceExtent read FExtent;
    property HarmonyMode: TWfcMusicEnsembleHarmonyMode read FHarmonyMode;
    property Seed: TGraphSeed read GetSeed write SetSeed;
  end;

function DefaultWfcMusicEnsembleConfig(const AScoreTemplate: TWfcMusicScore;
  const AQuantumTicks: Integer; const ASeed: TGraphSeed):
  TWfcMusicEnsembleConfig;

function WfcMusicEnsembleLayerName(const ALayer: TWfcMusicEnsembleLayer): String;

{ Checked public-artifact construction. The returned composition is caller
  owned. No latent state identities are invented by this boundary. }
function CreateWfcMusicEnsembleComposition(const ASeed: TGraphSeed;
  const AQuantumTicks: Integer;
  const AExtent: TWfcSequenceExtent;
  const AHarmonyMode: TWfcMusicEnsembleHarmonyMode;
  const AHarmonyTokens, ARhythmTokens,
  AEnsembleTokens: TWfcModelTokens;
  const AScore: TWfcMusicScore): TWfcMusicEnsembleComposition;

function CalculateWfcMusicEnsembleCompositionSignature(const ASeed: TGraphSeed;
  const AQuantumTicks: Integer;
  const AExtent: TWfcSequenceExtent;
  const AHarmonyMode: TWfcMusicEnsembleHarmonyMode;
  const AHarmonyTokens, ARhythmTokens,
  AEnsembleTokens: TWfcModelTokens;
  const AScore: TWfcMusicScore): TWfcMusicEnsembleCompositionSignature; overload;
function CalculateWfcMusicEnsembleCompositionSignature(
  const AComposition: TWfcMusicEnsembleComposition):
  TWfcMusicEnsembleCompositionSignature; overload;
function WfcMusicEnsembleCompositionSignatureHex(
  const ASignature: TWfcMusicEnsembleCompositionSignature): String;

function DescribeWfcMusicEnsembleValidationIssue(
  const AIssue: TWfcMusicEnsembleValidationIssue): String;

implementation

uses
  wfc_music_text;

type
  { The generic graph transaction invokes DoValidateCommit after staging has
    been copied into live entries, while its entry and RNG snapshots are still
    available. The music owner builds its immutable result there; returning
    False makes the graph report final validation and roll the whole attempt
    back before negotiation decides whether to reopen a provider. }
  TWfcMusicEnsembleCommitGraph = class(TGraph)
  private
    FOwner: TWfcMusicEnsemblePipeline;
  strict protected
    function DoValidateCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean; override;
  public
    constructor CreatePass(const ARoot: TGraph;
      const APassIndex: Integer); override;
    property Owner: TWfcMusicEnsemblePipeline read FOwner write FOwner;
  end;

constructor TWfcMusicEnsembleCommitGraph.CreatePass(const ARoot: TGraph;
  const APassIndex: Integer);
begin
  inherited CreatePass(ARoot, APassIndex);
  if not (ARoot is TWfcMusicEnsembleCommitGraph) then
    raise EWfcMusicEnsemblePasses.Create(
      'music pass graph root has the wrong runtime type');
  FOwner := TWfcMusicEnsembleCommitGraph(ARoot).Owner;
end;

function TWfcMusicEnsembleCommitGraph.DoValidateCommit(
  out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
begin
  if not Assigned(FOwner) then
  begin
    AFailedPassIndex := Ord(wmelEnsemble);
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
    raise EWfcMusicEnsemblePasses.Create(ALabel + ' cannot be negative');
  if (A <> 0) and (B > High(Integer) div A) then
    raise EWfcMusicEnsemblePasses.Create(ALabel + ' exceeds the Integer range');
  Result := A * B;
end;

function ExpectedBoundary: TWfcModelBoundary;
begin
  Result := wmbOpen;
end;

function WfcMusicEnsembleLayerName(const ALayer: TWfcMusicEnsembleLayer): String;
begin
  case ALayer of
    wmelHarmony: Result := WFC_MUSIC_ENSEMBLE_PASS_HARMONY;
    wmelRhythm: Result := WFC_MUSIC_ENSEMBLE_PASS_RHYTHM;
    wmelEnsemble: Result := WFC_MUSIC_ENSEMBLE_PASS_ENSEMBLE;
  else
    raise ERangeError.Create('unknown music pass layer');
  end;
end;

function DefaultWfcMusicEnsembleConfig(const AScoreTemplate: TWfcMusicScore;
  const AQuantumTicks: Integer; const ASeed: TGraphSeed):
  TWfcMusicEnsembleConfig;
begin
  Result := Default(TWfcMusicEnsembleConfig);
  Result.QuantumTicks := AQuantumTicks;
  Result.Seed := ASeed;
  Result.ScoreTemplate := AScoreTemplate;
  Result.Extent := wseWhole;
  Result.HarmonyMode := wmehmExact;
end;

function VoiceCellsEqual(const A, B: TWfcMusicVoiceCell): Boolean;
var I: Integer;
begin
  Result := False;
  if (A.Action <> B.Action) or (Length(A.Tones) <> Length(B.Tones)) then Exit;
  for I := 0 to High(A.Tones) do
    if (A.Tones[I].Pitch <> B.Tones[I].Pitch) or
      (A.Tones[I].Velocity <> B.Tones[I].Velocity) then Exit;
  Result := True;
end;

function EnsembleContinuationIsValid(const ACells: TWfcMusicEnsembleFrames;
  out AInvalidPosition: Integer): Boolean;
var I, J, K: Integer;
begin
  AInvalidPosition := -1;
  Result := False;
  if Length(ACells) = 0 then Exit;
  for I := 0 to High(ACells) do
  begin
    AInvalidPosition := I;
    if Length(ACells[I].Voices) <> Length(ACells[0].Voices) then Exit;
    for J := 0 to High(ACells[I].Voices) do
      if ACells[I].Voices[J].Action = wmcaHold then
      begin
        if I = 0 then Exit;
        if ACells[I-1].Voices[J].Action = wmcaRest then Exit;
        if Length(ACells[I].Voices[J].Tones) <>
          Length(ACells[I-1].Voices[J].Tones) then Exit;
        for K := 0 to High(ACells[I].Voices[J].Tones) do
          if (ACells[I].Voices[J].Tones[K].Pitch <>
              ACells[I-1].Voices[J].Tones[K].Pitch) or
            (ACells[I].Voices[J].Tones[K].Velocity <>
              ACells[I-1].Voices[J].Tones[K].Velocity) then Exit;
      end;
  end;
  AInvalidPosition := -1;
  Result := True;
end;

function FrameRhythmMatches(const AFrame: TWfcMusicEnsembleFrame;
  const ARhythm: TWfcMusicRhythmFrame): Boolean;
var I: Integer;
begin
  Result := False;
  if Length(AFrame.Voices) <> Length(ARhythm.Actions) then Exit;
  for I := 0 to High(AFrame.Voices) do
    if AFrame.Voices[I].Action <> ARhythm.Actions[I] then Exit;
  Result := True;
end;

function FrameHarmonyMatches(const AFrame: TWfcMusicEnsembleFrame;
  const AHarmony: TWfcMusicPitchClassSet; const ASteps: Integer;
  const AMode: TWfcMusicEnsembleHarmonyMode): Boolean;
var I, J, K, LClass: Integer; LFound: Boolean;
begin
  //Independent relational proof: never uses the projection builder or its
  //projected set. Both directions are required for exact sounding harmony.
  Result := False;
  if AHarmony.StepsPerOctave <> ASteps then Exit;
  for I := 0 to High(AFrame.Voices) do
    for J := 0 to High(AFrame.Voices[I].Tones) do
    begin
      LClass := AFrame.Voices[I].Tones[J].Pitch mod ASteps;
      LFound := False;
      for K := 0 to High(AHarmony.PitchClasses) do
        if AHarmony.PitchClasses[K] = LClass then begin LFound := True; Break end;
      if not LFound then Exit;
    end;
  if AMode = wmehmExact then
    for K := 0 to High(AHarmony.PitchClasses) do
    begin
      LFound := False;
      for I := 0 to High(AFrame.Voices) do
        for J := 0 to High(AFrame.Voices[I].Tones) do
          if (AFrame.Voices[I].Tones[J].Pitch mod ASteps) =
            AHarmony.PitchClasses[K] then LFound := True;
      if not LFound then Exit;
    end;
  Result := True;
end;

procedure ValidateCompositionPolicy(const AExtent: TWfcSequenceExtent;
  const AMode: TWfcMusicEnsembleHarmonyMode);
begin
  if not (AExtent in [wseWhole, wsePrefix]) then
    raise EArgumentException.Create('ensemble extent must be whole or prefix');
  if not (AMode in [wmehmExact, wmehmAllowed]) then
    raise EArgumentException.Create('unknown ensemble harmony mode');
end;

function ScoreMatchesFrameTimeline(const AScore: TWfcMusicScore;
  const AFrames: TWfcMusicEnsembleFrames; const AQuantum: Integer): Boolean;
var
  I, J, K, LSpan, LTick: Integer;
  LSpans: TWfcMusicSpanEvents;
  LCell: TWfcMusicVoiceCell;
begin
  //Independent interval proof, not a call to either frame projection or
  //reconstruction. A sound span beginning at a grid position means attack;
  //the same span covering a later position means hold. This detects accidental
  //active-set slicing even if the reconstruction path repeats the same bug.
  Result := False;
  if (AScore = nil) or (AQuantum < 1) or (Length(AFrames) < 1) then Exit;
  if Length(AFrames) > High(Integer) div AQuantum then Exit;
  if AScore.LengthTicks <> Length(AFrames) * AQuantum then Exit;
  for I := 0 to High(AFrames) do
    if Length(AFrames[I].Voices) <> AScore.VoiceCount then Exit;
  for J := 0 to AScore.VoiceCount - 1 do
  begin
    LSpans := AScore.CopyVoiceSpans(J);
    if Length(LSpans) = 0 then Exit;
    for I := 0 to High(LSpans) do
      if (LSpans[I].StartTick mod AQuantum <> 0) or
        (LSpans[I].DurationTicks mod AQuantum <> 0) then Exit;
    LSpan := 0;
    for I := 0 to High(AFrames) do
    begin
      LTick := I * AQuantum;
      while (LSpan < High(LSpans)) and
        (LSpans[LSpan + 1].StartTick <= LTick) do Inc(LSpan);
      LCell := AFrames[I].Voices[J];
      if LCell.Action = wmcaRest then
      begin
        if LSpans[LSpan].Kind <> wmskRest then Exit;
      end
      else
      begin
        if not (LSpans[LSpan].Kind in [wmskNote, wmskChord]) then Exit;
        if (LCell.Action = wmcaAttack) <>
          (LSpans[LSpan].StartTick = LTick) then Exit;
        if Length(LCell.Tones) <> Length(LSpans[LSpan].Tones) then Exit;
        for K := 0 to High(LCell.Tones) do
          if (LCell.Tones[K].Pitch <> LSpans[LSpan].Tones[K].Pitch) or
            (LCell.Tones[K].Velocity <> LSpans[LSpan].Tones[K].Velocity) then Exit;
      end;
    end;
  end;
  Result := True;
end;

procedure ValidatePublicCompositionInputs(const AQuantumTicks: Integer;
  const AExtent: TWfcSequenceExtent;
  const AHarmonyMode: TWfcMusicEnsembleHarmonyMode;
  const AHarmonyTokens, ARhythmTokens, AEnsembleTokens: TWfcModelTokens;
  const AScore: TWfcMusicScore);
var
  I, LInvalidPosition: Integer;
  LFrames: TWfcMusicEnsembleFrames;
  LExpected: TWfcMusicScore;
begin
  ValidateCompositionPolicy(AExtent, AHarmonyMode);
  if AScore = nil then
    raise EArgumentNilException.Create('ensemble composition score cannot be nil');
  if AQuantumTicks < 1 then
    raise EWfcMusicEnsemblePasses.Create('ensemble quantum must be positive');
  if (Length(AEnsembleTokens) < 1) or
    (Length(AHarmonyTokens) <> Length(AEnsembleTokens)) or
    (Length(ARhythmTokens) <> Length(AEnsembleTokens)) then
    raise EWfcMusicEnsemblePasses.Create('ensemble layer lengths must match and be positive');
  if CheckedProduct(Length(AEnsembleTokens), AQuantumTicks,
    'ensemble composition length') <> AScore.LengthTicks then
    raise EWfcMusicEnsemblePasses.Create('ensemble frames do not fill the score');
  LFrames := DecodeWfcMusicEnsembleFrames(AEnsembleTokens);
  for I := 0 to High(LFrames) do
  begin
    if Length(LFrames[I].Voices) <> AScore.VoiceCount then
      raise EWfcMusicEnsemblePasses.CreateFmt('ensemble voice count differs at %d', [I]);
    if not FrameRhythmMatches(LFrames[I],
      DecodeWfcMusicRhythmFrame(ARhythmTokens[I])) then
      raise EWfcMusicEnsemblePasses.CreateFmt('ensemble rhythm differs at %d', [I]);
    if not FrameHarmonyMatches(LFrames[I],
      DecodeWfcMusicPitchClassSet(AHarmonyTokens[I]), AScore.StepsPerOctave,
      AHarmonyMode) then
      raise EWfcMusicEnsemblePasses.CreateFmt('ensemble harmony differs at %d', [I]);
  end;
  if not EnsembleContinuationIsValid(LFrames, LInvalidPosition) then
    raise EWfcMusicEnsemblePasses.CreateFmt('ensemble hold continuation is invalid at %d',
      [LInvalidPosition]);
  if not ScoreMatchesFrameTimeline(AScore, LFrames, AQuantumTicks) then
    raise EWfcMusicEnsemblePasses.Create(
      'ensemble exact score intervals differ from the frame timeline');
  LExpected := RebuildWfcMusicEnsembleScore(LFrames, AQuantumTicks, AScore);
  try
    if EncodeWfcMusicText(LExpected) <> EncodeWfcMusicText(AScore) then
      raise EWfcMusicEnsemblePasses.Create('ensemble score differs from its frame tokens');
  finally LExpected.Free end;
end;

procedure HashByte(var AHash: TWfcMusicEnsembleCompositionSignature;
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

procedure HashCardinal(var AHash: TWfcMusicEnsembleCompositionSignature;
  const AValue: Cardinal);
begin
  HashByte(AHash, Byte(AValue and $FF));
  HashByte(AHash, Byte((AValue shr 8) and $FF));
  HashByte(AHash, Byte((AValue shr 16) and $FF));
  HashByte(AHash, Byte((AValue shr 24) and $FF));
end;

procedure HashInteger(var AHash: TWfcMusicEnsembleCompositionSignature;
  const AValue: Integer);
begin
  HashCardinal(AHash, Cardinal(AValue));
end;

procedure HashAsciiString(var AHash: TWfcMusicEnsembleCompositionSignature;
  const AValue: String; const ALabel: String);
var
  I: Integer;
begin
  HashInteger(AHash, Length(AValue));
  for I := 1 to Length(AValue) do
  begin
    if Ord(AValue[I]) > 127 then
      raise EWfcMusicEnsemblePasses.Create(ALabel + ' must be canonical ASCII');
    HashByte(AHash, Byte(Ord(AValue[I])));
  end;
end;

procedure HashTokens(var AHash: TWfcMusicEnsembleCompositionSignature;
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
  const AExtent: TWfcSequenceExtent;
  const AHarmonyMode: TWfcMusicEnsembleHarmonyMode;
  const AHarmonyTokens, ARhythmTokens,
  AEnsembleTokens: TWfcModelTokens;
  const AScore: TWfcMusicScore): TWfcMusicEnsembleCompositionSignature;
begin
  Result := Cardinal(2166136261);
  HashInteger(Result, WFC_MUSIC_ENSEMBLE_SIGNATURE_VERSION);
  HashInteger(Result, WFC_MUSIC_ENSEMBLE_PIPELINE_VERSION);
  HashInteger(Result, WFC_MUSIC_ENSEMBLE_VALIDATION_VERSION);
  HashInteger(Result, WFC_MUSIC_MODEL_VERSION);
  HashInteger(Result, WFC_MUSIC_VALIDATION_VERSION);
  HashInteger(Result, WFC_MUSIC_ENSEMBLE_VERSION);
  HashInteger(Result, WFC_MUSIC_ENSEMBLE_TOKEN_VERSION);
  HashInteger(Result, WFC_MUSIC_ENSEMBLE_GRAPH_VERSION);
  HashInteger(Result, WFC_SEQUENCE_MODEL_VERSION);
  HashInteger(Result, WFC_SEQUENCE_GRAPH_MODEL_VERSION);
  HashInteger(Result, WFC_SEQUENCE_EXTENT_VERSION);
  HashCardinal(Result, Cardinal(ASeed));
  HashInteger(Result, AQuantumTicks);
  HashInteger(Result, Ord(AExtent));
  HashInteger(Result, Ord(AHarmonyMode));
  HashInteger(Result, AScore.StepsPerOctave);
  HashTokens(Result, AHarmonyTokens, 'music harmony signature token');
  HashTokens(Result, ARhythmTokens, 'music rhythm signature token');
  HashTokens(Result, AEnsembleTokens, 'music ensemble signature token');
  HashAsciiString(Result, EncodeWfcMusicText(AScore),
    'music score signature text');
end;

function CalculateWfcMusicEnsembleCompositionSignature(const ASeed: TGraphSeed;
  const AQuantumTicks: Integer;
  const AExtent: TWfcSequenceExtent;
  const AHarmonyMode: TWfcMusicEnsembleHarmonyMode;
  const AHarmonyTokens, ARhythmTokens,
  AEnsembleTokens: TWfcModelTokens;
  const AScore: TWfcMusicScore): TWfcMusicEnsembleCompositionSignature;
begin
  ValidatePublicCompositionInputs(AQuantumTicks, AExtent, AHarmonyMode, AHarmonyTokens,
    ARhythmTokens, AEnsembleTokens, AScore);
  Result := CalculatePublicSignatureUnchecked(ASeed, AQuantumTicks, AExtent, AHarmonyMode,
    AHarmonyTokens, ARhythmTokens, AEnsembleTokens, AScore);
end;

function CalculateWfcMusicEnsembleCompositionSignature(
  const AComposition: TWfcMusicEnsembleComposition):
  TWfcMusicEnsembleCompositionSignature;
begin
  if not Assigned(AComposition) then
    raise EArgumentNilException.Create('music composition cannot be nil');
  Result := CalculatePublicSignatureUnchecked(AComposition.FSeed,
    AComposition.FQuantumTicks, AComposition.FExtent, AComposition.FHarmonyMode,
    AComposition.FLayers[wmelHarmony].Tokens,
    AComposition.FLayers[wmelRhythm].Tokens,
    AComposition.FLayers[wmelEnsemble].Tokens,
    AComposition.FScore);
end;

function WfcMusicEnsembleCompositionSignatureHex(
  const ASignature: TWfcMusicEnsembleCompositionSignature): String;
begin
  Result := IntToHex(ASignature, 8);
end;

constructor TWfcMusicEnsembleComposition.CreateInternal(const ASeed: TGraphSeed;
  const AQuantumTicks: Integer;
  const AExtent: TWfcSequenceExtent;
  const AHarmonyMode: TWfcMusicEnsembleHarmonyMode;
  const AHarmonyTokens, ARhythmTokens,
  AEnsembleTokens: TWfcModelTokens; const AScore: TWfcMusicScore);
begin
  inherited Create;
  FSeed := ASeed;
  FQuantumTicks := AQuantumTicks;
  FExtent := AExtent;
  FHarmonyMode := AHarmonyMode;
  FHasLatentCapture := False;
  FLayers[wmelHarmony] := Default(TWfcGeneratedSequence);
  FLayers[wmelRhythm] := Default(TWfcGeneratedSequence);
  FLayers[wmelEnsemble] := Default(TWfcGeneratedSequence);
  FLayers[wmelHarmony].Boundary := ExpectedBoundary;
  FLayers[wmelRhythm].Boundary := ExpectedBoundary;
  FLayers[wmelEnsemble].Boundary := ExpectedBoundary;
  FLayers[wmelHarmony].Extent := FExtent;
  FLayers[wmelRhythm].Extent := FExtent;
  FLayers[wmelEnsemble].Extent := FExtent;
  FLayers[wmelHarmony].Tokens := CopyTokens(AHarmonyTokens);
  FLayers[wmelRhythm].Tokens := CopyTokens(ARhythmTokens);
  FLayers[wmelEnsemble].Tokens := CopyTokens(AEnsembleTokens);
  FScore := CloneMusicScore(AScore);
  FSignature := CalculatePublicSignatureUnchecked(FSeed, FQuantumTicks, FExtent, FHarmonyMode,
    FLayers[wmelHarmony].Tokens, FLayers[wmelRhythm].Tokens,
    FLayers[wmelEnsemble].Tokens, FScore);
end;

procedure TWfcMusicEnsembleComposition.AttachLatentCapture(
  const ALayers: TWfcMusicEnsembleGeneratedLayers);
var
  I: Integer;
  LLayer: TWfcMusicEnsembleLayer;
begin
  for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
  begin
    if (ALayers[LLayer].Boundary <> ExpectedBoundary) or
        (ALayers[LLayer].Extent <> FExtent) or
        (Length(ALayers[LLayer].Tokens) < 1) or
        (Length(ALayers[LLayer].Tokens) <>
          Length(FLayers[LLayer].Tokens)) or
        (Length(ALayers[LLayer].StateIndices) <>
          Length(ALayers[LLayer].Tokens)) then
      raise EWfcMusicEnsemblePasses.Create(
        'music latent capture shape does not match the composition');
    for I := 0 to Length(ALayers[LLayer].Tokens) - 1 do
      if ALayers[LLayer].Tokens[I] <> FLayers[LLayer].Tokens[I] then
        raise EWfcMusicEnsemblePasses.Create(
          'music latent capture token differs from the composition');
  end;
  for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
    FLayers[LLayer] := CopyGeneratedSequence(ALayers[LLayer]);
  FHasLatentCapture := True;
end;

destructor TWfcMusicEnsembleComposition.Destroy;
begin
  FScore.Free;
  inherited Destroy;
end;

function TWfcMusicEnsembleComposition.GetCellCount: Integer;
begin
  Result := Length(FLayers[wmelEnsemble].Tokens);
end;

function TWfcMusicEnsembleComposition.CopyGenerated(
  const ALayer: TWfcMusicEnsembleLayer): TWfcGeneratedSequence;
begin
  WfcMusicEnsembleLayerName(ALayer);
  Result := CopyGeneratedSequence(FLayers[ALayer]);
end;

function TWfcMusicEnsembleComposition.CopyHarmonyCells: TWfcMusicPitchClassSets;
begin
  Result := DecodeWfcMusicPitchClassSets(FLayers[wmelHarmony].Tokens);
end;

function TWfcMusicEnsembleComposition.CopyRhythmCells: TWfcMusicRhythmFrames;
begin
  Result := DecodeWfcMusicRhythmFrames(FLayers[wmelRhythm].Tokens);
end;

function TWfcMusicEnsembleComposition.CopyEnsembleFrames: TWfcMusicEnsembleFrames;
begin
  Result := DecodeWfcMusicEnsembleFrames(FLayers[wmelEnsemble].Tokens);
end;

function TWfcMusicEnsembleComposition.CopyScore: TWfcMusicScore;
begin
  Result := CloneMusicScore(FScore);
end;

function CreateWfcMusicEnsembleComposition(const ASeed: TGraphSeed;
  const AQuantumTicks: Integer;
  const AExtent: TWfcSequenceExtent;
  const AHarmonyMode: TWfcMusicEnsembleHarmonyMode;
  const AHarmonyTokens, ARhythmTokens,
  AEnsembleTokens: TWfcModelTokens;
  const AScore: TWfcMusicScore): TWfcMusicEnsembleComposition;
begin
  ValidatePublicCompositionInputs(AQuantumTicks, AExtent, AHarmonyMode, AHarmonyTokens,
    ARhythmTokens, AEnsembleTokens, AScore);
  Result := TWfcMusicEnsembleComposition.CreateInternal(ASeed, AQuantumTicks, AExtent, AHarmonyMode,
    AHarmonyTokens, ARhythmTokens, AEnsembleTokens, AScore);
end;

function ModelForLayer(const AModels: TWfcMusicEnsembleModels;
  const ALayer: TWfcMusicEnsembleLayer): TWfcSequenceModel;
begin
  case ALayer of
    wmelHarmony: Result := AModels.Harmony;
    wmelRhythm: Result := AModels.Rhythm;
    wmelEnsemble: Result := AModels.Ensemble;
  else
    raise ERangeError.Create('unknown music pass layer');
  end;
end;

procedure ValidateModelVocabulary(const AModel: TWfcSequenceModel;
  const ALayer: TWfcMusicEnsembleLayer; const AVoiceCount, AStepsPerOctave: Integer);
begin
  case ALayer of
    wmelHarmony: ValidateWfcMusicEnsembleHarmonyModel(AModel, AStepsPerOctave);
    wmelRhythm: ValidateWfcMusicEnsembleRhythmModel(AModel, AVoiceCount);
    wmelEnsemble: ValidateWfcMusicEnsembleModel(AModel, AVoiceCount);
  else
    raise EArgumentException.Create('unknown ensemble model layer');
  end;
end;

constructor TWfcMusicEnsemblePipeline.Create(const AConfig: TWfcMusicEnsembleConfig);
begin
  inherited Create;
  Initialize(AConfig);
end;

procedure TWfcMusicEnsemblePipeline.Initialize(
  const AConfig: TWfcMusicEnsembleConfig);
var
  LLayer: TWfcMusicEnsembleLayer;
begin
  if AConfig.QuantumTicks < 1 then
    raise ERangeError.CreateFmt(
      'music pass quantum must be positive [%d]', [AConfig.QuantumTicks]);
  if not Assigned(AConfig.ScoreTemplate) then
    raise EArgumentNilException.Create('music score template cannot be nil');
  ValidateCompositionPolicy(AConfig.Extent, AConfig.HarmonyMode);
  if (AConfig.ScoreTemplate.LengthTicks mod AConfig.QuantumTicks) <> 0 then
    raise EArgumentException.Create(
      'music score length is not divisible by the quantum');
  if not Assigned(AConfig.Models.Harmony) then
    raise EArgumentNilException.Create('music harmony model cannot be nil');
  if not Assigned(AConfig.Models.Rhythm) then
    raise EArgumentNilException.Create('music rhythm model cannot be nil');
  if not Assigned(AConfig.Models.Ensemble) then
    raise EArgumentNilException.Create('music ensemble model cannot be nil');

  FQuantumTicks := AConfig.QuantumTicks;
  FExtent := AConfig.Extent;
  FHarmonyMode := AConfig.HarmonyMode;
  FCellCount := AConfig.ScoreTemplate.LengthTicks div FQuantumTicks;
  if FCellCount < 1 then
    raise EArgumentException.Create('music pass cell count must be positive');
  FModels := AConfig.Models;
  FValidateComposition := AConfig.ValidateComposition;
  FDirtyLayers := [wmelHarmony, wmelRhythm, wmelEnsemble];
  FPendingComposition := nil;
  FPendingCapture := Default(TWfcMusicEnsembleCaptureReports);
  FPendingValidation := Default(TWfcMusicEnsembleValidationReport);
  FPendingFailedLayer := wmelHarmony;
  FPendingStatus := wmesNotRun;
  FScoreTemplate := CloneMusicScore(AConfig.ScoreTemplate);
  FGraph := nil;
  try
    for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
      ValidateModelVocabulary(GetModel(LLayer), LLayer,
        FScoreTemplate.VoiceCount, FScoreTemplate.StepsPerOctave);

    FGraph := TWfcMusicEnsembleCommitGraph.Create;
    TWfcMusicEnsembleCommitGraph(FGraph).Owner := Self;
    FGraph.Reshape(FCellCount, 1, 1);
    FGraph.WrapNeighbors := False;
    FGraph.Seed := AConfig.Seed;

    FGraph.CurrentPass := WFC_MUSIC_ENSEMBLE_PASS_HARMONY;
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies;
    ApplySequenceModelToGraph(FModels.Harmony, FGraph, FExtent);

    FGraph.SwitchToPass(WFC_MUSIC_ENSEMBLE_PASS_RHYTHM);
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies;
    ApplySequenceModelToGraph(FModels.Rhythm, FGraph, FExtent);

    FGraph.SwitchToPass(WFC_MUSIC_ENSEMBLE_PASS_ENSEMBLE);
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies;
    ApplySequenceModelToGraph(FModels.Ensemble, FGraph, FExtent);
    RequireWfcMusicEnsembleFromPasses(FModels.Ensemble,
      FModels.Rhythm, FModels.Harmony, FGraph,
      WFC_MUSIC_ENSEMBLE_PASS_RHYTHM, WFC_MUSIC_ENSEMBLE_PASS_HARMONY,
      FScoreTemplate.VoiceCount, FScoreTemplate.StepsPerOctave,
      FHarmonyMode);

    for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
      if Length(AConfig.InitialTokenConstraints[LLayer]) <> 0 then
        IntersectSequenceTokenConstraints(GetModel(LLayer), GetLayerGraph(LLayer),
          AConfig.InitialTokenConstraints[LLayer]);
    CaptureBaselineDomains;
    FGraph.SwitchToPass(WFC_MUSIC_ENSEMBLE_PASS_HARMONY);
  except
    FGraph.Free;
    FGraph := nil;
    FScoreTemplate.Free;
    FScoreTemplate := nil;
    raise;
  end;
end;

procedure TWfcMusicEnsemblePipeline.CaptureBaselineDomains;
var
  I: Integer;
  LGraph: TGraph;
  LLayer: TWfcMusicEnsembleLayer;
begin
  for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
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

destructor TWfcMusicEnsemblePipeline.Destroy;
begin
  ClearPendingCommit;
  FGraph.Free;
  FScoreTemplate.Free;
  inherited Destroy;
end;

function TWfcMusicEnsemblePipeline.GetLayerGraph(
  const ALayer: TWfcMusicEnsembleLayer): TGraph;
begin
  WfcMusicEnsembleLayerName(ALayer);
  Result := FGraph.PassGraph[Ord(ALayer)];
end;

function TWfcMusicEnsemblePipeline.GetModel(
  const ALayer: TWfcMusicEnsembleLayer): TWfcSequenceModel;
begin
  Result := ModelForLayer(FModels, ALayer);
end;

function TWfcMusicEnsemblePipeline.GetSeed: TGraphSeed;
begin
  Result := FGraph.Seed;
end;

procedure TWfcMusicEnsemblePipeline.SetSeed(const AValue: TGraphSeed);
begin
  FGraph.Seed := AValue;
  FDirtyLayers := [wmelHarmony, wmelRhythm, wmelEnsemble];
end;

procedure TWfcMusicEnsemblePipeline.MarkDirty(
  const ALayer: TWfcMusicEnsembleLayer);
begin
  WfcMusicEnsembleLayerName(ALayer);
  Include(FDirtyLayers, ALayer);
end;

function TWfcMusicEnsemblePipeline.IntersectAllowedTokens(
  const ALayer: TWfcMusicEnsembleLayer; const APosition: Integer;
  const ATokens: TWfcModelTokens): TWfcMusicEnsemblePipeline;
begin
  IntersectSequenceAllowedTokens(GetModel(ALayer), GetLayerGraph(ALayer),
    APosition, ATokens);
  MarkDirty(ALayer);
  Result := Self;
end;

function TWfcMusicEnsemblePipeline.IntersectAllowedTokens(
  const ALayer: TWfcMusicEnsembleLayer; const APosition: Integer;
  const AToken: TWfcModelToken): TWfcMusicEnsemblePipeline;
begin
  IntersectSequenceAllowedTokens(GetModel(ALayer), GetLayerGraph(ALayer),
    APosition, AToken);
  MarkDirty(ALayer);
  Result := Self;
end;

function TWfcMusicEnsemblePipeline.IntersectTokenConstraints(
  const ALayer: TWfcMusicEnsembleLayer;
  const AConstraints: TWfcSequenceTokenConstraints): TWfcMusicEnsemblePipeline;
begin
  IntersectSequenceTokenConstraints(GetModel(ALayer), GetLayerGraph(ALayer),
    AConstraints);
  MarkDirty(ALayer);
  Result := Self;
end;

function TWfcMusicEnsemblePipeline.IntersectLockedSpan(
  const ALayer: TWfcMusicEnsembleLayer; const AStart: Integer;
  const ATokens: TWfcModelTokens): TWfcMusicEnsemblePipeline;
begin
  IntersectSequenceLockedSpan(GetModel(ALayer), GetLayerGraph(ALayer),
    AStart, ATokens);
  MarkDirty(ALayer);
  Result := Self;
end;

function TWfcMusicEnsemblePipeline.LockEnsembleFrames(const AStart: Integer;
  const ACells: TWfcMusicEnsembleFrames): TWfcMusicEnsemblePipeline;
begin
  Result := IntersectLockedSpan(wmelEnsemble, AStart,
    EncodeWfcMusicEnsembleFrames(ACells));
end;

function TWfcMusicEnsemblePipeline.LockVoiceCells(
  const AVoiceIndex, AStart: Integer; const ACells: TWfcMusicVoiceCells):
  TWfcMusicEnsemblePipeline;
var
  I, J, LCount: Integer;
  LCell: TWfcMusicVoiceCell;
  LFrames: TWfcMusicEnsembleFrames;
  LConstraints: TWfcSequenceTokenConstraints;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= FScoreTemplate.VoiceCount) then
    raise ERangeError.Create('ensemble voice lock index is out of bounds');
  if (AStart < 0) or (AStart > FCellCount) or (Length(ACells) = 0) then
    raise ERangeError.Create('ensemble voice lock range is invalid');
  if Length(ACells) > FCellCount - AStart then
    raise ERangeError.Create('ensemble voice lock extends beyond the score');
  LFrames := DecodeWfcMusicEnsembleFrames(FModels.Ensemble.CopyPublicTokens);
  SetLength(LConstraints, Length(ACells));
  for I := 0 to High(ACells) do
  begin
    LCell := MakeWfcMusicVoiceCell(ACells[I].Action, ACells[I].Tones);
    LConstraints[I].Position := AStart + I;
    LConstraints[I].AllowedTokens := nil;
    for J := 0 to High(LFrames) do
      if VoiceCellsEqual(LCell, LFrames[J].Voices[AVoiceIndex]) then
      begin
        LCount := Length(LConstraints[I].AllowedTokens);
        SetLength(LConstraints[I].AllowedTokens, LCount + 1);
        LConstraints[I].AllowedTokens[LCount] := FModels.Ensemble.PublicTokenAt(J);
      end;
  end;
  Result := IntersectTokenConstraints(wmelEnsemble, LConstraints);
end;

function TWfcMusicEnsemblePipeline.ClearAllowedTokens(
  const ALayer: TWfcMusicEnsembleLayer;
  const APosition: Integer): TWfcMusicEnsemblePipeline;
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

procedure TWfcMusicEnsemblePipeline.BuildEffectiveRoots(
  const ARoots: TWfcMusicEnsembleLayers; out ALabels: TGraphPassLabels);
var
  I: Integer;
  LCovered: TWfcMusicEnsembleLayerSet;
  LLayer: TWfcMusicEnsembleLayer;
  LSelected: TWfcMusicEnsembleLayerSet;
begin
  if Length(ARoots) < 1 then
    raise EArgumentException.Create(
      'music pass regeneration requires at least one root');
  LSelected := [];
  for I := 0 to Length(ARoots) - 1 do
  begin
    WfcMusicEnsembleLayerName(ARoots[I]);
    Include(LSelected, ARoots[I]);
  end;
  LCovered := LSelected;
  if (wmelHarmony in LCovered) or (wmelRhythm in LCovered) then
    Include(LCovered, wmelEnsemble);
  { A constraint in an already active descendant is not a second requested
    root. A dirty pass outside the requested closure must become a root so its
    changed caller domain cannot be silently ignored. }
  for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
    if (LLayer in FDirtyLayers) and not (LLayer in LCovered) then
    begin
      Include(LSelected, LLayer);
      Include(LCovered, LLayer);
      if LLayer in [wmelHarmony, wmelRhythm] then
        Include(LCovered, wmelEnsemble);
    end;
  ALabels := nil;
  SetLength(ALabels, 0);
  for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
    if LLayer in LSelected then
    begin
      SetLength(ALabels, Length(ALabels) + 1);
      ALabels[Length(ALabels) - 1] := WfcMusicEnsembleLayerName(LLayer);
    end;
end;

procedure InitializeValidationReport(
  out AReport: TWfcMusicEnsembleValidationReport);
begin
  AReport := Default(TWfcMusicEnsembleValidationReport);
  AReport.Issue.Layer := wmelHarmony;
  AReport.Issue.Position := -1;
  AReport.Issue.SequenceIssue.Position := -1;
  AReport.Issue.SequenceIssue.RelatedPosition := -1;
  AReport.Issue.SequenceIssue.StateIndex := -1;
  AReport.Issue.SequenceIssue.RelatedStateIndex := -1;
  AReport.Issue.Detail := '';
end;

procedure SetValidationIssue(var AReport: TWfcMusicEnsembleValidationReport;
  const AKind: TWfcMusicEnsembleValidationIssueKind;
  const ALayer: TWfcMusicEnsembleLayer; const APosition: Integer);
begin
  AReport.Valid := False;
  AReport.Issue.Kind := AKind;
  AReport.Issue.Layer := ALayer;
  AReport.Issue.Position := APosition;
  AReport.Issue.Detail := '';
end;

procedure SetValidationIssueDetail(
  var AReport: TWfcMusicEnsembleValidationReport;
  const AKind: TWfcMusicEnsembleValidationIssueKind;
  const ALayer: TWfcMusicEnsembleLayer; const APosition: Integer;
  const ADetail: String);
begin
  SetValidationIssue(AReport, AKind, ALayer, APosition);
  AReport.Issue.Detail := ADetail;
end;

function ScoresEqual(const A, B: TWfcMusicScore): Boolean;
begin
  Result := EncodeWfcMusicText(A) = EncodeWfcMusicText(B);
end;

function ValidApplicationIssueInteger(const AValue, AMinimum,
  AMaximum: Integer): Boolean;
begin
  Result := (AValue >= AMinimum) and (AValue <= AMaximum);
  {$IFDEF PAS2JS}
  Result := Result and (AValue = Trunc(AValue));
  {$ENDIF}
end;

function TWfcMusicEnsemblePipeline.Validate(
  const AComposition: TWfcMusicEnsembleComposition;
  out AReport: TWfcMusicEnsembleValidationReport): Boolean;
var
  I: Integer;
  LConstraintPosition: Integer;
  LExpectedScore: TWfcMusicScore;
  LGenerated: TWfcGeneratedSequence;
  LHarmony: TWfcMusicPitchClassSet;
  LInvalidPosition: Integer;
  LLayer: TWfcMusicEnsembleLayer;
  LEnsemble: TWfcMusicEnsembleFrame;
  LEnsembleCells: TWfcMusicEnsembleFrames;
  LModel: TWfcSequenceModel;
  LRhythm: TWfcMusicRhythmFrame;
  LSequenceReport: TWfcSequenceGraphValidationReport;
  LSpans: TWfcMusicSpanEvents;
  LApplicationIssue: TWfcMusicEnsembleValidationIssue;
begin
  InitializeValidationReport(AReport);
  if not Assigned(AComposition) then
  begin
    SetValidationIssue(AReport, wmevikComposition, wmelHarmony, -1);
    Exit(False);
  end;
  if AComposition.Extent <> FExtent then
  begin
    SetValidationIssue(AReport, wmevikExtent, wmelEnsemble, -1);
    Exit(False);
  end;
  if AComposition.HarmonyMode <> FHarmonyMode then
  begin
    SetValidationIssueDetail(AReport, wmevikComposition, wmelHarmony, -1,
      'composition harmony interpretation differs from the owner');
    Exit(False);
  end;
  if (AComposition.QuantumTicks <> FQuantumTicks) or
      (AComposition.CellCount <> FCellCount) then
  begin
    SetValidationIssue(AReport, wmevikLength, wmelEnsemble, -1);
    Exit(False);
  end;
  if not AComposition.HasLatentCapture then
  begin
    SetValidationIssue(AReport, wmevikLatentCapture, wmelHarmony, -1);
    Exit(False);
  end;

  for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
  begin
    LGenerated := AComposition.FLayers[LLayer];
    LModel := GetModel(LLayer);
    if (Length(LGenerated.StateIndices) <> FCellCount) or
        (Length(LGenerated.Tokens) <> FCellCount) then
    begin
      SetValidationIssue(AReport, wmevikLength, LLayer, -1);
      Exit(False);
    end;
    if LGenerated.Extent <> FExtent then
    begin
      SetValidationIssue(AReport, wmevikExtent, LLayer, -1);
      Exit(False);
    end;
    if LGenerated.Boundary <> ExpectedBoundary then
    begin
      SetValidationIssue(AReport, wmevikBoundary, LLayer, -1);
      Exit(False);
    end;
    if not ValidateSequenceStatePath(LModel, LGenerated.StateIndices,
        FExtent, LSequenceReport) then
    begin
      SetValidationIssue(AReport, wmevikStatePath, LLayer,
        LSequenceReport.Issue.Position);
      AReport.Issue.SequenceIssue := LSequenceReport.Issue;
      Exit(False);
    end;
    if not SequenceStatesSatisfyEntryConstraints(LModel,
        GetLayerGraph(LLayer), LGenerated.StateIndices,
        LConstraintPosition) then
    begin
      SetValidationIssue(AReport, wmevikCallerConstraint, LLayer,
        LConstraintPosition);
      Exit(False);
    end;
    for I := 0 to FCellCount - 1 do
    begin
      if LModel.ProjectStateToken(LGenerated.StateIndices[I]) <>
          LGenerated.Tokens[I] then
      begin
        SetValidationIssue(AReport, wmevikStateProjection, LLayer, I);
        Exit(False);
      end;
      Inc(AReport.CheckedCells);
    end;
    Inc(AReport.CheckedLayers);
  end;

  LEnsembleCells := nil;
  SetLength(LEnsembleCells, FCellCount);
  for I := 0 to FCellCount - 1 do
  begin
    try
      LEnsemble := DecodeWfcMusicEnsembleFrame(
        AComposition.FLayers[wmelEnsemble].Tokens[I]);
      LRhythm := DecodeWfcMusicRhythmFrame(
        AComposition.FLayers[wmelRhythm].Tokens[I]);
      LHarmony := DecodeWfcMusicPitchClassSet(
        AComposition.FLayers[wmelHarmony].Tokens[I]);
    except
      on EWfcModel do
      begin
        SetValidationIssue(AReport, wmevikCell, wmelEnsemble, I);
        Exit(False);
      end;
    end;
    if not FrameRhythmMatches(LEnsemble, LRhythm) then
    begin
      SetValidationIssue(AReport, wmevikRhythmProjection, wmelEnsemble, I);
      Exit(False);
    end;
    Inc(AReport.CheckedRelations);
    if Length(LEnsemble.Voices) <> FScoreTemplate.VoiceCount then
    begin
      SetValidationIssue(AReport, wmevikCell, wmelEnsemble, I);
      Exit(False);
    end;
    if not FrameHarmonyMatches(LEnsemble, LHarmony,
        FScoreTemplate.StepsPerOctave, FHarmonyMode) then
    begin
      SetValidationIssue(AReport, wmevikHarmonyProjection, wmelEnsemble, I);
      Exit(False);
    end;
    Inc(AReport.CheckedRelations);
    LEnsembleCells[I] := LEnsemble;
  end;

  if not EnsembleContinuationIsValid(LEnsembleCells,
      LInvalidPosition) then
  begin
    SetValidationIssue(AReport, wmevikEnsembleContinuation,
      wmelEnsemble, LInvalidPosition);
    Exit(False);
  end;

  if not ScoreMatchesFrameTimeline(AComposition.FScore, LEnsembleCells,
      FQuantumTicks) then
  begin
    SetValidationIssueDetail(AReport, wmevikScore, wmelEnsemble, -1,
      'exact score intervals differ from the frame timeline');
    Exit(False);
  end;

  try
    LSpans := RebuildWfcMusicEnsembleSpans(LEnsembleCells, FQuantumTicks);
    LExpectedScore := TWfcMusicScore.Create(FScoreTemplate.TicksPerQuarter,
      FScoreTemplate.StepsPerOctave, FScoreTemplate.LengthTicks,
      FScoreTemplate.CopyTracks, FScoreTemplate.CopyVoices,
      FScoreTemplate.CopyMeters, FScoreTemplate.CopyTempos, LSpans);
    try
      if not ScoresEqual(LExpectedScore, AComposition.FScore) then
      begin
        SetValidationIssue(AReport, wmevikScore, wmelEnsemble, -1);
        Exit(False);
      end;
    finally
      LExpectedScore.Free;
    end;
  except
    on E: EWfcModel do
    begin
      SetValidationIssueDetail(AReport, wmevikScore, wmelEnsemble, -1,
        'canonical score comparison failed');
      Exit(False);
    end;
  end;

  if AComposition.Signature <>
      CalculateWfcMusicEnsembleCompositionSignature(AComposition) then
  begin
    SetValidationIssue(AReport, wmevikSignature, wmelEnsemble, -1);
    Exit(False);
  end;
  if Assigned(FValidateComposition) then
  begin
    LApplicationIssue := Default(TWfcMusicEnsembleValidationIssue);
    LApplicationIssue.Layer := wmelEnsemble;
    LApplicationIssue.Position := -1;
    if not FValidateComposition(AComposition, LApplicationIssue) then
    begin
      if not ValidApplicationIssueInteger(Ord(LApplicationIssue.Kind),
          Ord(Low(TWfcMusicEnsembleValidationIssueKind)),
          Ord(High(TWfcMusicEnsembleValidationIssueKind))) or
          not ValidApplicationIssueInteger(Ord(LApplicationIssue.Layer),
            Ord(Low(TWfcMusicEnsembleLayer)), Ord(High(TWfcMusicEnsembleLayer))) or
          not ValidApplicationIssueInteger(LApplicationIssue.Position, -1, FCellCount - 1) then
        SetValidationIssueDetail(AReport, wmevikInternal, wmelEnsemble, -1,
          'composition validator returned an invalid issue')
      else
      begin
        if LApplicationIssue.Kind = wmevikNone then
          LApplicationIssue.Kind := wmevikCallerConstraint;
        if LApplicationIssue.Detail = '' then
          LApplicationIssue.Detail := 'application composition validation failed';
        AReport.Issue := LApplicationIssue;
      end;
      Exit(False);
    end;
  end;
  AReport.Valid := True;
  AReport.Issue.Kind := wmevikNone;
  Result := True;
end;

function TWfcMusicEnsemblePipeline.CaptureComposition(const ASeed: TGraphSeed;
  out AComposition: TWfcMusicEnsembleComposition;
  out ACapture: TWfcMusicEnsembleCaptureReports;
  out AValidation: TWfcMusicEnsembleValidationReport;
  out AFailedLayer: TWfcMusicEnsembleLayer;
  out AStatus: TWfcMusicEnsembleStatus): Boolean;
var
  I: Integer;
  LComposition: TWfcMusicEnsembleComposition;
  LGenerated: TWfcMusicEnsembleGeneratedLayers;
  LInvalidPosition: Integer;
  LLayer: TWfcMusicEnsembleLayer;
  LEnsembleCells: TWfcMusicEnsembleFrames;
  LScore: TWfcMusicScore;
  LSpans: TWfcMusicSpanEvents;
begin
  AComposition := nil;
  ACapture := Default(TWfcMusicEnsembleCaptureReports);
  InitializeValidationReport(AValidation);
  AFailedLayer := wmelHarmony;
  AStatus := wmesNotRun;
  LScore := nil;
  LComposition := nil;

  for LLayer := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
    if not CaptureSolvedSequence(GetModel(LLayer), GetLayerGraph(LLayer),
        FExtent, LGenerated[LLayer], ACapture[LLayer]) then
    begin
      AFailedLayer := LLayer;
      AStatus := wmesCaptureFailed;
      Exit(False);
    end;

  try
    SetLength(LEnsembleCells, Length(LGenerated[wmelEnsemble].Tokens));
    for I := 0 to Length(LGenerated[wmelEnsemble].Tokens) - 1 do
      try
        LEnsembleCells[I] := DecodeWfcMusicEnsembleFrame(
          LGenerated[wmelEnsemble].Tokens[I]);
      except
        on E: EWfcModel do
        begin
          SetValidationIssueDetail(AValidation, wmevikCell,
            wmelEnsemble, I, 'canonical ensemble token decoding failed');
          AFailedLayer := wmelEnsemble;
          AStatus := wmesValidationFailed;
          Exit(False);
        end;
      end;
    if not EnsembleContinuationIsValid(LEnsembleCells,
        LInvalidPosition) then
    begin
      SetValidationIssue(AValidation, wmevikEnsembleContinuation,
        wmelEnsemble, LInvalidPosition);
      AFailedLayer := wmelEnsemble;
      AStatus := wmesValidationFailed;
      Exit(False);
    end;

    try
      LSpans := RebuildWfcMusicEnsembleSpans(LEnsembleCells, FQuantumTicks);
    except
      on E: EWfcModel do
      begin
        SetValidationIssueDetail(AValidation, wmevikScore,
          wmelEnsemble, -1, 'ensemble span rebuild failed');
        AFailedLayer := wmelEnsemble;
        AStatus := wmesValidationFailed;
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
        SetValidationIssueDetail(AValidation, wmevikScore,
          wmelEnsemble, -1, 'canonical score construction failed');
        AFailedLayer := wmelEnsemble;
        AStatus := wmesValidationFailed;
        Exit(False);
      end;
    end;

    try
      LComposition := TWfcMusicEnsembleComposition.CreateInternal(
        ASeed, FQuantumTicks, FExtent, FHarmonyMode,
        LGenerated[wmelHarmony].Tokens, LGenerated[wmelRhythm].Tokens,
        LGenerated[wmelEnsemble].Tokens, LScore);
    except
      on E: EWfcModel do
      begin
        SetValidationIssueDetail(AValidation, wmevikSignature,
          wmelEnsemble, -1, 'immutable composition construction failed');
        AFailedLayer := wmelEnsemble;
        AStatus := wmesValidationFailed;
        Exit(False);
      end;
    end;

    try
      LComposition.AttachLatentCapture(LGenerated);
    except
      on E: EWfcModel do
      begin
        SetValidationIssueDetail(AValidation, wmevikLatentCapture,
          wmelEnsemble, -1, 'latent capture attachment failed');
        AFailedLayer := wmelEnsemble;
        AStatus := wmesValidationFailed;
        Exit(False);
      end;
    end;

    try
      if not Validate(LComposition, AValidation) then
      begin
        AFailedLayer := AValidation.Issue.Layer;
        AStatus := wmesValidationFailed;
        Exit(False);
      end;
      AComposition := LComposition;
      LComposition := nil;
      AStatus := wmesCompleted;
      Result := True;
    except
      on E: EWfcModel do
      begin
        SetValidationIssueDetail(AValidation, wmevikInternal,
          wmelEnsemble, -1, 'independent composition validation raised');
        AFailedLayer := wmelEnsemble;
        AStatus := wmesValidationFailed;
        Result := False;
      end;
    end;
  finally
    LComposition.Free;
    LScore.Free;
  end;
end;

procedure TWfcMusicEnsemblePipeline.ClearPendingCommit;
begin
  FPendingComposition.Free;
  FPendingComposition := nil;
  FPendingCapture := Default(TWfcMusicEnsembleCaptureReports);
  InitializeValidationReport(FPendingValidation);
  FPendingFailedLayer := wmelHarmony;
  FPendingStatus := wmesNotRun;
end;

procedure TWfcMusicEnsemblePipeline.CopyPendingFailure(
  out ACapture: TWfcMusicEnsembleCaptureReports;
  out AValidation: TWfcMusicEnsembleValidationReport;
  out AFailedLayer: TWfcMusicEnsembleLayer;
  out AStatus: TWfcMusicEnsembleStatus);
begin
  ACapture := FPendingCapture;
  AValidation := FPendingValidation;
  AFailedLayer := FPendingFailedLayer;
  AStatus := FPendingStatus;
  if AStatus = wmesNotRun then
    AStatus := wmesValidationFailed;
end;

function TWfcMusicEnsemblePipeline.TakePendingComposition(
  out AComposition: TWfcMusicEnsembleComposition;
  out ACapture: TWfcMusicEnsembleCaptureReports;
  out AValidation: TWfcMusicEnsembleValidationReport;
  out AFailedLayer: TWfcMusicEnsembleLayer;
  out AStatus: TWfcMusicEnsembleStatus): Boolean;
begin
  AComposition := FPendingComposition;
  FPendingComposition := nil;
  ACapture := FPendingCapture;
  AValidation := FPendingValidation;
  AFailedLayer := FPendingFailedLayer;
  AStatus := FPendingStatus;
  Result := Assigned(AComposition) and (AStatus = wmesCompleted);
  if not Result then
  begin
    AComposition.Free;
    AComposition := nil;
    if AStatus = wmesNotRun then
    begin
      SetValidationIssueDetail(AValidation, wmevikInternal,
        wmelEnsemble, -1,
        'graph reported success without a validated pending composition');
      AFailedLayer := wmelEnsemble;
      AStatus := wmesValidationFailed;
    end;
  end;
end;

function TWfcMusicEnsemblePipeline.ValidatePendingCommit(
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
  if FPendingStatus = wmesCaptureFailed then
    AFailedEntryIndex :=
      FPendingCapture[FPendingFailedLayer].Issue.Position
  else
    AFailedEntryIndex := FPendingValidation.Issue.Position;
end;

function TWfcMusicEnsemblePipeline.TryGenerate(
  const AOptions: TGraphSolveOptions;
  out AComposition: TWfcMusicEnsembleComposition;
  out AReport: TWfcMusicEnsembleReport): Boolean;
var
  LSolved: Boolean;
begin
  AComposition := nil;
  AReport := Default(TWfcMusicEnsembleReport);
  AReport.Status := wmesNotRun;
  AReport.FailedLayer := wmelHarmony;
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
      AReport.Status := wmesSolveFailed;
      if (AReport.Solve.FailedPassIndex >=
          Ord(Low(TWfcMusicEnsembleLayer))) and
          (AReport.Solve.FailedPassIndex <=
            Ord(High(TWfcMusicEnsembleLayer))) then
        AReport.FailedLayer :=
          TWfcMusicEnsembleLayer(AReport.Solve.FailedPassIndex);
    end;
    ClearPendingCommit;
    Exit(False);
  end;
  Result := TakePendingComposition(AComposition,
    AReport.Capture, AReport.Validation, AReport.FailedLayer,
    AReport.Status);
  if not Result then
    raise EWfcMusicEnsemblePasses.Create(
      'music graph committed without a validated composition');
  FDirtyLayers := [];
end;

function TWfcMusicEnsemblePipeline.TryGenerate(
  out AComposition: TWfcMusicEnsembleComposition;
  out AReport: TWfcMusicEnsembleReport): Boolean;
var
  LOptions: TGraphSolveOptions;
begin
  LOptions := DefaultGraphSolveOptions;
  Result := TryGenerate(LOptions, AComposition, AReport);
end;

function TWfcMusicEnsemblePipeline.TryRegenerateFrom(
  const ALayer: TWfcMusicEnsembleLayer; const AOptions: TGraphSolveOptions;
  out AComposition: TWfcMusicEnsembleComposition;
  out AReport: TWfcMusicEnsembleReport): Boolean;
var
  LLayers: TWfcMusicEnsembleLayers;
begin
  SetLength(LLayers, 1);
  LLayers[0] := ALayer;
  Result := TryRegenerateFrom(LLayers, AOptions, AComposition, AReport);
end;

function TWfcMusicEnsemblePipeline.TryRegenerateFrom(
  const ALayers: TWfcMusicEnsembleLayers; const AOptions: TGraphSolveOptions;
  out AComposition: TWfcMusicEnsembleComposition;
  out AReport: TWfcMusicEnsembleReport): Boolean;
var
  LLabels: TGraphPassLabels;
  LSolved: Boolean;
begin
  AComposition := nil;
  AReport := Default(TWfcMusicEnsembleReport);
  AReport.Status := wmesNotRun;
  AReport.FailedLayer := wmelHarmony;
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
      AReport.Status := wmesSolveFailed;
      if (AReport.Solve.FailedPassIndex >=
          Ord(Low(TWfcMusicEnsembleLayer))) and
          (AReport.Solve.FailedPassIndex <=
            Ord(High(TWfcMusicEnsembleLayer))) then
        AReport.FailedLayer :=
          TWfcMusicEnsembleLayer(AReport.Solve.FailedPassIndex);
    end;
    ClearPendingCommit;
    Exit(False);
  end;
  Result := TakePendingComposition(AComposition,
    AReport.Capture, AReport.Validation, AReport.FailedLayer,
    AReport.Status);
  if not Result then
    raise EWfcMusicEnsemblePasses.Create(
      'music graph committed without a validated composition');
  FDirtyLayers := [];
end;

function TWfcMusicEnsemblePipeline.TryGenerateNegotiated(
  const AOptions: TGraphNegotiationOptions;
  out AComposition: TWfcMusicEnsembleComposition;
  out AReport: TWfcMusicEnsembleNegotiationReport): Boolean;
var
  LSolved: Boolean;
begin
  AComposition := nil;
  AReport := Default(TWfcMusicEnsembleNegotiationReport);
  AReport.Status := wmesNotRun;
  AReport.FailedLayer := wmelHarmony;
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
      AReport.Status := wmesSolveFailed;
      if (AReport.Search.FinalReport.FailedPassIndex >=
          Ord(Low(TWfcMusicEnsembleLayer))) and
          (AReport.Search.FinalReport.FailedPassIndex <=
            Ord(High(TWfcMusicEnsembleLayer))) then
        AReport.FailedLayer := TWfcMusicEnsembleLayer(
          AReport.Search.FinalReport.FailedPassIndex);
    end;
    ClearPendingCommit;
    Exit(False);
  end;
  Result := TakePendingComposition(AComposition,
    AReport.Capture, AReport.Validation, AReport.FailedLayer,
    AReport.Status);
  if not Result then
    raise EWfcMusicEnsemblePasses.Create(
      'music graph committed without a validated composition');
  FDirtyLayers := [];
end;

function TWfcMusicEnsemblePipeline.TryGenerateNegotiated(
  out AComposition: TWfcMusicEnsembleComposition;
  out AReport: TWfcMusicEnsembleNegotiationReport): Boolean;
var
  LOptions: TGraphNegotiationOptions;
begin
  LOptions := DefaultGraphNegotiationOptions;
  Result := TryGenerateNegotiated(LOptions, AComposition, AReport);
end;

function TWfcMusicEnsemblePipeline.TryRegenerateNegotiatedFrom(
  const ALayer: TWfcMusicEnsembleLayer;
  const AOptions: TGraphNegotiationOptions;
  out AComposition: TWfcMusicEnsembleComposition;
  out AReport: TWfcMusicEnsembleSelectiveNegotiationReport): Boolean;
var
  LLayers: TWfcMusicEnsembleLayers;
begin
  SetLength(LLayers, 1);
  LLayers[0] := ALayer;
  Result := TryRegenerateNegotiatedFrom(LLayers, AOptions,
    AComposition, AReport);
end;

function TWfcMusicEnsemblePipeline.TryRegenerateNegotiatedFrom(
  const ALayers: TWfcMusicEnsembleLayers;
  const AOptions: TGraphNegotiationOptions;
  out AComposition: TWfcMusicEnsembleComposition;
  out AReport: TWfcMusicEnsembleSelectiveNegotiationReport): Boolean;
var
  LLabels: TGraphPassLabels;
  LSolved: Boolean;
begin
  AComposition := nil;
  AReport := Default(TWfcMusicEnsembleSelectiveNegotiationReport);
  AReport.Status := wmesNotRun;
  AReport.FailedLayer := wmelHarmony;
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
      AReport.Status := wmesSolveFailed;
      if (AReport.Search.Search.FinalReport.FailedPassIndex >=
          Ord(Low(TWfcMusicEnsembleLayer))) and
          (AReport.Search.Search.FinalReport.FailedPassIndex <=
            Ord(High(TWfcMusicEnsembleLayer))) then
        AReport.FailedLayer := TWfcMusicEnsembleLayer(
          AReport.Search.Search.FinalReport.FailedPassIndex);
    end;
    ClearPendingCommit;
    Exit(False);
  end;
  Result := TakePendingComposition(AComposition,
    AReport.Capture, AReport.Validation, AReport.FailedLayer,
    AReport.Status);
  if not Result then
    raise EWfcMusicEnsemblePasses.Create(
      'music graph committed without a validated composition');
  FDirtyLayers := [];
end;

function TWfcMusicEnsemblePipeline.CopyScoreTemplate: TWfcMusicScore;
begin
  Result := CloneMusicScore(FScoreTemplate);
end;

function TWfcMusicEnsemblePipeline.TryCopyCommittedLayer(
  const ALayer: TWfcMusicEnsembleLayer;
  out AGenerated: TWfcGeneratedSequence;
  out AReport: TWfcSequenceGraphValidationReport): Boolean;
begin
  Result := CaptureSolvedSequence(GetModel(ALayer),
    GetLayerGraph(ALayer), FExtent, AGenerated, AReport);
end;

function TWfcMusicEnsemblePipeline.CopyCommittedTokens(
  const ALayer: TWfcMusicEnsembleLayer): TWfcModelTokens;
var
  LGenerated: TWfcGeneratedSequence;
  LReport: TWfcSequenceGraphValidationReport;
begin
  if not TryCopyCommittedLayer(ALayer, LGenerated, LReport) then
    raise EWfcMusicEnsemblePasses.CreateFmt(
      'cannot copy committed %s pass: %s',
      [WfcMusicEnsembleLayerName(ALayer),
       DescribeSequenceGraphIssue(LReport.Issue)]);
  Result := CopyTokens(LGenerated.Tokens);
end;

function DescribeWfcMusicEnsembleValidationIssue(
  const AIssue: TWfcMusicEnsembleValidationIssue): String;
begin
  case AIssue.Kind of
    wmevikNone:
      Result := 'no music pass validation issue';
    wmevikComposition:
      Result := 'music composition is not assigned';
    wmevikLength:
      Result := Format('%s pass has the wrong cell length',
        [WfcMusicEnsembleLayerName(AIssue.Layer)]);
    wmevikExtent:
      Result := Format('%s pass has the wrong sequence extent',
        [WfcMusicEnsembleLayerName(AIssue.Layer)]);
    wmevikBoundary:
      Result := Format('%s pass has the wrong sequence boundary',
        [WfcMusicEnsembleLayerName(AIssue.Layer)]);
    wmevikLatentCapture:
      Result := 'music composition has no latent pipeline capture';
    wmevikStatePath:
      Result := Format('%s pass state path failed at %d: %s',
        [WfcMusicEnsembleLayerName(AIssue.Layer), AIssue.Position,
         DescribeSequenceGraphIssue(AIssue.SequenceIssue)]);
    wmevikStateProjection:
      Result := Format('%s pass state/token projection failed at %d',
        [WfcMusicEnsembleLayerName(AIssue.Layer), AIssue.Position]);
    wmevikCallerConstraint:
      Result := Format('%s pass violates a caller constraint at %d',
        [WfcMusicEnsembleLayerName(AIssue.Layer), AIssue.Position]);
    wmevikCell:
      Result := Format('music cell decoding failed at %d',
        [AIssue.Position]);
    wmevikEnsembleContinuation:
      Result := Format('music ensemble continuation failed at %d',
        [AIssue.Position]);
    wmevikRhythmProjection:
      Result := Format('music rhythm projection failed at %d',
        [AIssue.Position]);
    wmevikHarmonyProjection:
      Result := Format('music harmony projection failed at %d',
        [AIssue.Position]);
    wmevikScore:
      Result := 'music composition score does not match its ensemble cells';
    wmevikSignature:
      Result := 'music composition signature does not recompute';
    wmevikInternal:
      Result := 'music composition validation failed internally';
  else
    Result := 'unknown music pass validation issue';
  end;
  if AIssue.Detail <> '' then
    Result := Result + ': ' + AIssue.Detail;
end;

end.
