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
unit ensemble_studio_workbench;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_sequence,
  wfc_music,
  wfc_music_sequence,
  wfc_music_ensemble,
  wfc_music_ensemble_graph,
  wfc_music_ensemble_passes,
  wfc_music_audio,
  wfc_midi_smf;

const
  ENSEMBLE_STUDIO_VERSION = 1;
  ENSEMBLE_STUDIO_VOICE_COUNT = 3;
  ENSEMBLE_STUDIO_CELLS_PER_BAR = 8;
  ENSEMBLE_STUDIO_DEFAULT_BARS = 2;
  ENSEMBLE_STUDIO_QUANTUM = 240;
  ENSEMBLE_STUDIO_TPQ = 480;
  ENSEMBLE_STUDIO_TEMPO = 500000;
  ENSEMBLE_STUDIO_CORPUS_COUNT = 16;
  ENSEMBLE_STUDIO_MAX_BARS = High(Integer) div
    (ENSEMBLE_STUDIO_CELLS_PER_BAR * ENSEMBLE_STUDIO_QUANTUM);
  ENSEMBLE_STUDIO_BASELINE_COMPOSITION_SIGNATURE = '573E2010';
  ENSEMBLE_STUDIO_BASELINE_SCORE_SIGNATURE = '33123E67';
  ENSEMBLE_STUDIO_BASELINE_MIDI_SIGNATURE = '07361333';
  ENSEMBLE_STUDIO_BASELINE_WAVE_SIGNATURE = 'A273067B';
  ENSEMBLE_STUDIO_BASELINE_TRANSCRIPT_SIGNATURE = '2E106AD9';
  ENSEMBLE_STUDIO_BASELINE_MIDI_BYTES = 227;
  ENSEMBLE_STUDIO_BASELINE_WAVE_BYTES = 352844;

type
  EEnsembleStudio = class(Exception);

  TEnsembleStudioAction = (
    esaGenerate,
    esaHarmony,
    esaRhythm,
    esaEnsemble
  );

  TEnsembleStudioStatus = (
    essIdle,
    essDirty,
    essSolved,
    essContradiction,
    essSolverLimit,
    essPassLimit
  );

  TEnsembleStudioOptions = record
    Negotiated: Boolean;
    MaxBacktracks: Integer;
    MaxPassBacktracks: Integer;
    CaptureTrace: Boolean;
  end;

  TEnsembleStudioLock = record
    Layer: TWfcMusicEnsembleLayer;
    Position: Integer;
    Token: TWfcModelToken;
  end;
  TEnsembleStudioLocks = array of TEnsembleStudioLock;

  TEnsembleStudioReport = record
    Status: TEnsembleStudioStatus;
    Action: TEnsembleStudioAction;
    Negotiated: Boolean;
    Rounds: Integer;
    PassBacktracks: Integer;
    Decisions: Integer;
    Propagations: Integer;
    Contradictions: Integer;
    Backtracks: Integer;
    FailedPass: Integer;
    FailedCell: Integer;
    DependencyPass: Integer;
    FailureKind: TGraphContradictionKind;
    MusicStatus: TWfcMusicEnsembleStatus;
    MusicFailedLayer: TWfcMusicEnsembleLayer;
    Capture: TWfcMusicEnsembleCaptureReports;
    Validation: TWfcMusicEnsembleValidationReport;
    TraceHash: Cardinal;
    TranscriptHash: Cardinal;
    Passes: TGraphPassSolveReports;
    RequestedRootIndices: TGraphPassIndices;
    ActivePassIndices: TGraphPassIndices;
  end;

  { Shared, portable example state. A hidden committed composition is retained
    as the baseline for an explicit repair, but current artifact access always
    follows Status. Reset rebuilds the owner and clears locks and baseline. }
  TEnsembleStudio = class
  strict private
    FModels: TWfcMusicEnsembleModels;
    FPipeline: TWfcMusicEnsemblePipeline;
    FComposition: TWfcMusicEnsembleComposition;
    FLocks: TEnsembleStudioLocks;
    FReport: TEnsembleStudioReport;
    FSeed: TGraphSeed;
    FBars: Integer;
    FCellCount: Integer;
    FStatus: TEnsembleStudioStatus;
    FDirtyLayers: TWfcMusicEnsembleLayerSet;
    procedure BuildModels;
    procedure RequireSessionEngine;
    procedure RequireCurrent;
    procedure CheckLayer(const ALayer: TWfcMusicEnsembleLayer);
    procedure CheckCell(const ALayer: TWfcMusicEnsembleLayer;
      const APosition: Integer);
    procedure PutLock(const ALayer: TWfcMusicEnsembleLayer;
      const APosition: Integer; const AToken: TWfcModelToken);
    procedure AddSolveCounters(const AReport: TGraphSolveReport);
    procedure CaptureFinal(const AReport: TGraphSolveReport);
    procedure CaptureNegotiation(const AReport: TGraphNegotiationReport);
    procedure BuildScope(const ARoot: TWfcMusicEnsembleLayer);
    function GetHasCurrent: Boolean;
    function GetHasBaseline: Boolean;
  public
    constructor Create(const ASeed: TGraphSeed; const ABars: Integer);
    destructor Destroy; override;
    procedure Reset(const ASeed: TGraphSeed; const ABars: Integer);
    procedure InvalidateCurrent;
    procedure SetLock(const ALayer: TWfcMusicEnsembleLayer;
      const APosition: Integer; const AToken: TWfcModelToken);
    procedure ClearLock(const ALayer: TWfcMusicEnsembleLayer;
      const APosition: Integer);
    procedure ClearLocks;
    function CopyLocks: TEnsembleStudioLocks;
    function PublicTokens(
      const ALayer: TWfcMusicEnsembleLayer): TWfcModelTokens;
    function CellTokens(
      const ALayer: TWfcMusicEnsembleLayer): TWfcModelTokens;
    function EnsembleFrames: TWfcMusicEnsembleFrames;
    function Run(const AAction: TEnsembleStudioAction;
      const AOptions: TEnsembleStudioOptions): Boolean;
    function CurrentIsValid: Boolean;
    function CopyReport: TEnsembleStudioReport;
    function RunReportText: String;
    function SignatureText: String;
    function ScoreText: String;
    function CopyScore: TWfcMusicScore;
    function MidiBytes: TWfcMidiBytes;
    function TryWavePreview(out ABytes: TWfcMusicAudioBytes;
      out AFrameCount: Integer; out AFailure: String): Boolean;
    property Seed: TGraphSeed read FSeed;
    property Bars: Integer read FBars;
    property CellCount: Integer read FCellCount;
    property HasCurrent: Boolean read GetHasCurrent;
    property HasBaseline: Boolean read GetHasBaseline;
    property Status: TEnsembleStudioStatus read FStatus;
  end;

function DefaultEnsembleStudioOptions: TEnsembleStudioOptions;
function EnsembleStudioBarsToCellCount(const ABars: Integer): Integer;
function EnsembleStudioDurationSeconds(const ABars: Integer): Integer;
function EnsembleStudioStatusName(
  const AStatus: TEnsembleStudioStatus): String;
function EnsembleStudioPitchName(const APitch: Integer): String;
function EnsembleStudioActionName(const AAction: TWfcMusicCellAction): String;
function EnsembleStudioVoiceCellLabel(
  const ACell: TWfcMusicVoiceCell): String;
function EnsembleStudioTokenLabel(const ALayer: TWfcMusicEnsembleLayer;
  const AToken: TWfcModelToken): String;
function EnsembleStudioCorpus(
  const AIndex: Integer): TWfcMusicEnsembleFrames;
function EnsembleStudioCompositionIsValid(
  const AComposition: TWfcMusicEnsembleComposition;
  const AExpectedCellCount: Integer): Boolean;
function EnsembleStudioByteSignature(
  const ABytes: array of Byte): String;
function EnsembleStudioTextSignature(const AText: String): String;

implementation

uses
  wfc_sequence_learn,
  wfc_sequence_graph,
  wfc_music_text,
  wfc_music_midi;

const
  ROOT_PITCHES: array[0..3] of Integer = (36, 41, 43, 45);
  CHORD_PITCHES: array[0..3, 0..2] of Integer = (
    (48, 52, 55),
    (53, 57, 60),
    (55, 59, 62),
    (57, 60, 64)
  );
  UPPER_PITCHES: array[0..3, 0..3] of Integer = (
    (60, 64, 67, 64),
    (65, 69, 72, 69),
    (67, 71, 74, 71),
    (69, 72, 76, 72)
  );

procedure HashByte(var AHash: Cardinal; const AByte: Byte);
{$PUSH}{$Q-}
var
  LValue: Cardinal;
begin
  LValue := AHash xor Cardinal(AByte);
  AHash := (LValue + (LValue shl 1) + (LValue shl 4) +
    (LValue shl 7) + (LValue shl 8) + (LValue shl 24)) and
    Cardinal($FFFFFFFF);
end;
{$POP}

function EnsembleStudioByteSignature(
  const ABytes: array of Byte): String;
var
  I: Integer;
  LHash: Cardinal;
begin
  LHash := Cardinal(2166136261);
  for I := 0 to High(ABytes) do
    HashByte(LHash, ABytes[I]);
  Result := UpperCase(IntToHex(LHash, 8));
end;

function EnsembleStudioTextSignature(const AText: String): String;
var
  I: Integer;
  LHash: Cardinal;
begin
  LHash := Cardinal(2166136261);
  for I := 1 to Length(AText) do
  begin
    if Ord(AText[I]) > 127 then
      raise EEnsembleStudio.Create(
        'artifact signature requires canonical ASCII');
    HashByte(LHash, Byte(Ord(AText[I])));
  end;
  Result := UpperCase(IntToHex(LHash, 8));
end;

function DefaultEnsembleStudioOptions: TEnsembleStudioOptions;
begin
  Result.Negotiated := True;
  Result.MaxBacktracks := 256;
  Result.MaxPassBacktracks := 16;
  Result.CaptureTrace := False;
end;

function EnsembleStudioBarsToCellCount(const ABars: Integer): Integer;
begin
  if (ABars < 1) or (ABars > ENSEMBLE_STUDIO_MAX_BARS) then
    raise EEnsembleStudio.CreateFmt(
      'bars must be from 1 through %d', [ENSEMBLE_STUDIO_MAX_BARS]);
  Result := ABars * ENSEMBLE_STUDIO_CELLS_PER_BAR;
end;

function EnsembleStudioDurationSeconds(const ABars: Integer): Integer;
begin
  EnsembleStudioBarsToCellCount(ABars);
  Result := ABars * 2;
end;

function EnsembleStudioStatusName(
  const AStatus: TEnsembleStudioStatus): String;
begin
  case AStatus of
    essIdle: Result := 'idle';
    essDirty: Result := 'dirty';
    essSolved: Result := 'solved';
    essContradiction: Result := 'contradiction';
    essSolverLimit: Result := 'solver-limit';
    essPassLimit: Result := 'pass-limit';
  else
    Result := 'unknown';
  end;
end;

function EnsembleStudioPitchName(const APitch: Integer): String;
const
  NAMES: array[0..11] of String =
    ('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B');
begin
  if (APitch < 0) or (APitch > 127) then
    Exit('pitch ' + IntToStr(APitch));
  Result := NAMES[APitch mod 12] + IntToStr(APitch div 12 - 1);
end;

function EnsembleStudioActionName(
  const AAction: TWfcMusicCellAction): String;
begin
  case AAction of
    wmcaRest: Result := 'rest';
    wmcaAttack: Result := 'attack';
    wmcaHold: Result := 'hold';
  else
    Result := 'unknown';
  end;
end;

function EnsembleStudioVoiceCellLabel(
  const ACell: TWfcMusicVoiceCell): String;
var
  I: Integer;
begin
  Result := EnsembleStudioActionName(ACell.Action);
  if ACell.Action = wmcaRest then
    Exit;
  Result := Result + ' ';
  for I := 0 to High(ACell.Tones) do
  begin
    if I > 0 then
      Result := Result + '/';
    Result := Result + EnsembleStudioPitchName(ACell.Tones[I].Pitch);
  end;
end;

function PitchClassName(const AValue: Integer): String;
begin
  Result := EnsembleStudioPitchName(60 + AValue);
end;

function ActionLetter(const AAction: TWfcMusicCellAction): String;
begin
  case AAction of
    wmcaRest: Result := 'R';
    wmcaAttack: Result := 'A';
    wmcaHold: Result := 'H';
  else
    Result := '?';
  end;
end;

function EnsembleStudioTokenLabel(const ALayer: TWfcMusicEnsembleLayer;
  const AToken: TWfcModelToken): String;
var
  I: Integer;
  LFrame: TWfcMusicEnsembleFrame;
  LRhythm: TWfcMusicRhythmFrame;
  LSet: TWfcMusicPitchClassSet;
begin
  Result := '';
  case ALayer of
    wmelHarmony:
      begin
        LSet := DecodeWfcMusicPitchClassSet(AToken);
        if Length(LSet.PitchClasses) = 0 then
          Exit('silence');
        Result := '{';
        for I := 0 to High(LSet.PitchClasses) do
        begin
          if I > 0 then Result := Result + ',';
          Result := Result + PitchClassName(LSet.PitchClasses[I]);
        end;
        Result := Result + '}';
      end;
    wmelRhythm:
      begin
        LRhythm := DecodeWfcMusicRhythmFrame(AToken);
        for I := 0 to High(LRhythm.Actions) do
        begin
          if I > 0 then Result := Result + '/';
          Result := Result + ActionLetter(LRhythm.Actions[I]);
        end;
      end;
    wmelEnsemble:
      begin
        LFrame := DecodeWfcMusicEnsembleFrame(AToken);
        for I := 0 to High(LFrame.Voices) do
        begin
          if I > 0 then Result := Result + ' | ';
          Result := Result + EnsembleStudioVoiceCellLabel(LFrame.Voices[I]);
        end;
      end;
  else
    raise EEnsembleStudio.Create('unknown ensemble layer');
  end;
end;

function VoiceCell(const AAction: TWfcMusicCellAction;
  const APitches: array of Integer; const AVelocity: Integer):
  TWfcMusicVoiceCell;
var
  I: Integer;
  LTones: TWfcMusicTones;
begin
  LTones := nil;
  SetLength(LTones, Length(APitches));
  for I := 0 to High(APitches) do
    LTones[I] := MakeWfcMusicTone(APitches[I], AVelocity);
  Result := MakeWfcMusicVoiceCell(AAction, LTones);
end;

function CorpusBarCell(const ARootIndex, APosition, AVariant: Integer):
  TWfcMusicEnsembleFrame;
var
  LFirstUpper: Integer;
  LSecondUpper: Integer;
  LVoices: TWfcMusicVoiceCells;
begin
  if (ARootIndex < 0) or (ARootIndex > High(ROOT_PITCHES)) then
    raise EEnsembleStudio.Create('unknown corpus harmony');
  if (APosition < 0) or
      (APosition >= ENSEMBLE_STUDIO_CELLS_PER_BAR) then
    raise EEnsembleStudio.Create('corpus position is outside a bar');
  if (AVariant < 0) or (AVariant > 3) then
    raise EEnsembleStudio.Create('unknown corpus voicing');
  { Variant tones remain members of the sounding triad, so all authored
    ensemble alternatives share the same exact harmony and rhythm providers. }
  LFirstUpper := CHORD_PITCHES[ARootIndex, AVariant mod 3] + 12;
  LSecondUpper := CHORD_PITCHES[ARootIndex, (AVariant + 1) mod 3] + 12;
  SetLength(LVoices, ENSEMBLE_STUDIO_VOICE_COUNT);
  case APosition of
    0:
      begin
        LVoices[0] := VoiceCell(wmcaAttack,
          [ROOT_PITCHES[ARootIndex]], 72);
        LVoices[1] := VoiceCell(wmcaAttack,
          [CHORD_PITCHES[ARootIndex, 0], CHORD_PITCHES[ARootIndex, 1],
           CHORD_PITCHES[ARootIndex, 2]], 64);
        LVoices[2] := VoiceCell(wmcaAttack,
          [LFirstUpper], 96);
      end;
    1:
      begin
        LVoices[0] := VoiceCell(wmcaHold,
          [ROOT_PITCHES[ARootIndex]], 72);
        LVoices[1] := VoiceCell(wmcaHold,
          [CHORD_PITCHES[ARootIndex, 0], CHORD_PITCHES[ARootIndex, 1],
           CHORD_PITCHES[ARootIndex, 2]], 64);
        LVoices[2] := VoiceCell(wmcaHold,
          [LFirstUpper], 96);
      end;
    2:
      begin
        LVoices[0] := VoiceCell(wmcaHold,
          [ROOT_PITCHES[ARootIndex]], 72);
        LVoices[1] := MakeWfcMusicRestVoiceCell;
        LVoices[2] := VoiceCell(wmcaAttack,
          [UPPER_PITCHES[ARootIndex, 1]], 96);
      end;
    3:
      begin
        LVoices[0] := VoiceCell(wmcaHold,
          [ROOT_PITCHES[ARootIndex]], 72);
        LVoices[1] := MakeWfcMusicRestVoiceCell;
        LVoices[2] := VoiceCell(wmcaHold,
          [UPPER_PITCHES[ARootIndex, 1]], 96);
      end;
    4:
      begin
        LVoices[0] := VoiceCell(wmcaHold,
          [ROOT_PITCHES[ARootIndex]], 72);
        LVoices[1] := VoiceCell(wmcaAttack,
          [CHORD_PITCHES[ARootIndex, 0], CHORD_PITCHES[ARootIndex, 1],
           CHORD_PITCHES[ARootIndex, 2]], 64);
        LVoices[2] := VoiceCell(wmcaAttack,
          [LSecondUpper], 96);
      end;
    5:
      begin
        LVoices[0] := VoiceCell(wmcaHold,
          [ROOT_PITCHES[ARootIndex]], 72);
        LVoices[1] := VoiceCell(wmcaHold,
          [CHORD_PITCHES[ARootIndex, 0], CHORD_PITCHES[ARootIndex, 1],
           CHORD_PITCHES[ARootIndex, 2]], 64);
        LVoices[2] := VoiceCell(wmcaHold,
          [LSecondUpper], 96);
      end;
    6:
      begin
        LVoices[0] := VoiceCell(wmcaHold,
          [ROOT_PITCHES[ARootIndex]], 72);
        LVoices[1] := MakeWfcMusicRestVoiceCell;
        LVoices[2] := VoiceCell(wmcaAttack,
          [UPPER_PITCHES[ARootIndex, 3]], 96);
      end;
  else
    begin
      LVoices[0] := MakeWfcMusicRestVoiceCell;
      LVoices[1] := MakeWfcMusicRestVoiceCell;
      LVoices[2] := MakeWfcMusicRestVoiceCell;
    end;
  end;
  Result := MakeWfcMusicEnsembleFrame(LVoices);
end;

function EnsembleStudioCorpus(
  const AIndex: Integer): TWfcMusicEnsembleFrames;
var
  I: Integer;
begin
  if (AIndex < 0) or (AIndex >= ENSEMBLE_STUDIO_CORPUS_COUNT) then
    raise EEnsembleStudio.Create('unknown project-authored ensemble corpus');
  Result := nil;
  SetLength(Result, ENSEMBLE_STUDIO_DEFAULT_BARS *
    ENSEMBLE_STUDIO_CELLS_PER_BAR);
  for I := 0 to High(Result) do
    Result[I] := CorpusBarCell(
      AIndex div 4,
      I mod ENSEMBLE_STUDIO_CELLS_PER_BAR, AIndex mod 4);
  ValidateWfcMusicEnsembleFrames(Result);
end;

function BuildTemplateFrames(const ACellCount: Integer):
  TWfcMusicEnsembleFrames;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, ACellCount);
  for I := 0 to ACellCount - 1 do
    Result[I] := CorpusBarCell(0,
      I mod ENSEMBLE_STUDIO_CELLS_PER_BAR, 0);
  ValidateWfcMusicEnsembleFrames(Result);
end;

function BuildTemplate(const ABars: Integer): TWfcMusicScore;
var
  LCellCount: Integer;
  LFrames: TWfcMusicEnsembleFrames;
  LMeters: TWfcMusicMeterChanges;
  LSpans: TWfcMusicSpanEvents;
  LTempos: TWfcMusicTempoChanges;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
begin
  LCellCount := EnsembleStudioBarsToCellCount(ABars);
  LFrames := BuildTemplateFrames(LCellCount);
  SetLength(LTracks, ENSEMBLE_STUDIO_VOICE_COUNT);
  LTracks[0] := MakeWfcMusicTrack('bass', 'Held bass');
  LTracks[1] := MakeWfcMusicTrack('chords', 'Chord accompaniment');
  LTracks[2] := MakeWfcMusicTrack('upper', 'Upper line');
  SetLength(LVoices, ENSEMBLE_STUDIO_VOICE_COUNT);
  LVoices[0] := MakeWfcMusicVoice(0, 'bass');
  LVoices[1] := MakeWfcMusicVoice(1, 'chords');
  LVoices[2] := MakeWfcMusicVoice(2, 'upper');
  SetLength(LMeters, 1);
  LMeters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(LTempos, 1);
  LTempos[0] := MakeWfcMusicTempoChange(0, ENSEMBLE_STUDIO_TEMPO);
  LSpans := RebuildWfcMusicEnsembleSpans(LFrames,
    ENSEMBLE_STUDIO_QUANTUM);
  Result := TWfcMusicScore.Create(ENSEMBLE_STUDIO_TPQ, 12,
    LCellCount * ENSEMBLE_STUDIO_QUANTUM, LTracks, LVoices,
    LMeters, LTempos, LSpans);
end;

function SameTokens(const A, B: TWfcModelTokens): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to High(A) do
    if A[I] <> B[I] then
      Exit(False);
  Result := True;
end;

function EnsembleStudioCompositionIsValid(
  const AComposition: TWfcMusicEnsembleComposition;
  const AExpectedCellCount: Integer): Boolean;
var
  LEnsemble: TWfcMusicEnsembleFrames;
  LHarmony: TWfcMusicPitchClassSets;
  LProjectedEnsemble: TWfcMusicEnsembleFrames;
  LRhythm: TWfcMusicRhythmFrames;
  LScore: TWfcMusicScore;
  I: Integer;
begin
  Result := False;
  try
    if AComposition = nil then Exit;
    if (AComposition.CellCount <> AExpectedCellCount) or
        (AComposition.QuantumTicks <> ENSEMBLE_STUDIO_QUANTUM) or
        (AComposition.Extent <> wsePrefix) or
        (AComposition.HarmonyMode <> wmehmExact) then Exit;
    LEnsemble := AComposition.CopyEnsembleFrames;
    LRhythm := AComposition.CopyRhythmCells;
    LHarmony := AComposition.CopyHarmonyCells;
    if (Length(LEnsemble) <> AExpectedCellCount) or
        (Length(LRhythm) <> AExpectedCellCount) or
        (Length(LHarmony) <> AExpectedCellCount) then Exit;
    ValidateWfcMusicEnsembleFrames(LEnsemble);
    for I := 0 to AExpectedCellCount - 1 do
    begin
      if EncodeWfcMusicRhythmFrame(LRhythm[I]) <>
          EncodeWfcMusicRhythmFrame(
            ProjectWfcMusicEnsembleFrameToRhythm(LEnsemble[I])) then Exit;
      if EncodeWfcMusicPitchClassSet(LHarmony[I]) <>
          EncodeWfcMusicPitchClassSet(
            ProjectWfcMusicEnsembleFrameToPitchClassSet(
              LEnsemble[I], 12)) then Exit;
    end;
    LScore := AComposition.CopyScore;
    try
      if (LScore.TrackCount <> ENSEMBLE_STUDIO_VOICE_COUNT) or
          (LScore.VoiceCount <> ENSEMBLE_STUDIO_VOICE_COUNT) or
          (LScore.TicksPerQuarter <> ENSEMBLE_STUDIO_TPQ) or
          (LScore.StepsPerOctave <> 12) or
          (LScore.LengthTicks <> AExpectedCellCount *
            ENSEMBLE_STUDIO_QUANTUM) or
          (LScore.MeterCount <> 1) or
          (LScore.MeterAt(0).Tick <> 0) or
          (LScore.MeterAt(0).Numerator <> 4) or
          (LScore.MeterAt(0).Denominator <> 4) or
          (LScore.TempoCount <> 1) or
          (LScore.TempoAt(0).Tick <> 0) or
          (LScore.TempoAt(0).MicrosecondsPerQuarter <>
            ENSEMBLE_STUDIO_TEMPO) then Exit;
      LProjectedEnsemble := ProjectWfcMusicScoreToEnsembleFrames(
        LScore, ENSEMBLE_STUDIO_QUANTUM);
      if not SameTokens(EncodeWfcMusicEnsembleFrames(LProjectedEnsemble),
          EncodeWfcMusicEnsembleFrames(LEnsemble)) then Exit;
    finally
      LScore.Free;
    end;
    Result := CalculateWfcMusicEnsembleCompositionSignature(AComposition) =
      AComposition.Signature;
  except
    on EWfcMusic do Result := False;
  end;
end;

constructor TEnsembleStudio.Create(const ASeed: TGraphSeed;
  const ABars: Integer);
begin
  inherited Create;
  BuildModels;
  Reset(ASeed, ABars);
end;

destructor TEnsembleStudio.Destroy;
begin
  FComposition.Free;
  FPipeline.Free;
  FModels.Ensemble.Free;
  FModels.Rhythm.Free;
  FModels.Harmony.Free;
  inherited Destroy;
end;

procedure TEnsembleStudio.BuildModels;
var
  LEnsembleSamples: TWfcSequenceSamples;
  LHarmonySamples: TWfcSequenceSamples;
  LRhythmSamples: TWfcSequenceSamples;
  LFrames: TWfcMusicEnsembleFrames;
  I: Integer;
begin
  SetLength(LEnsembleSamples, ENSEMBLE_STUDIO_CORPUS_COUNT);
  SetLength(LHarmonySamples, ENSEMBLE_STUDIO_CORPUS_COUNT);
  SetLength(LRhythmSamples, ENSEMBLE_STUDIO_CORPUS_COUNT);
  for I := 0 to ENSEMBLE_STUDIO_CORPUS_COUNT - 1 do
  begin
    LFrames := EnsembleStudioCorpus(I);
    LEnsembleSamples[I] := MakeWfcSequenceSample(
      EncodeWfcMusicEnsembleFrames(LFrames));
    LRhythmSamples[I] := MakeWfcSequenceSample(
      EncodeWfcMusicRhythmFrames(
        ProjectWfcMusicEnsembleFramesToRhythm(LFrames)));
    LHarmonySamples[I] := MakeWfcSequenceSample(
      EncodeWfcMusicPitchClassSets(
        ProjectWfcMusicEnsembleFramesToPitchClassSets(LFrames, 12)));
  end;
  { A complete bar of context preserves phase while the duplicated second bar
    provides a BOS-free transition back to the next bar. Prefix generation can
    therefore extend to any requested whole-bar grid without losing the
    authored attack/hold/rest position. }
  FModels.Harmony := LearnSequenceModelCorpus(LHarmonySamples,
    ENSEMBLE_STUDIO_CELLS_PER_BAR);
  FModels.Rhythm := LearnSequenceModelCorpus(LRhythmSamples,
    ENSEMBLE_STUDIO_CELLS_PER_BAR);
  FModels.Ensemble := LearnSequenceModelCorpus(LEnsembleSamples,
    ENSEMBLE_STUDIO_CELLS_PER_BAR);
end;

procedure TEnsembleStudio.Reset(const ASeed: TGraphSeed;
  const ABars: Integer);
var
  LConfig: TWfcMusicEnsembleConfig;
  LNewPipeline: TWfcMusicEnsemblePipeline;
  LTemplate: TWfcMusicScore;
begin
  LTemplate := BuildTemplate(ABars);
  LNewPipeline := nil;
  try
    LConfig := DefaultWfcMusicEnsembleConfig(LTemplate,
      ENSEMBLE_STUDIO_QUANTUM, ASeed);
    LConfig.Models := FModels;
    LConfig.Extent := wsePrefix;
    LConfig.HarmonyMode := wmehmExact;
    LNewPipeline := TWfcMusicEnsemblePipeline.Create(LConfig);
  finally
    LTemplate.Free;
  end;
  FPipeline.Free;
  FPipeline := LNewPipeline;
  FreeAndNil(FComposition);
  FSeed := ASeed;
  FBars := ABars;
  FCellCount := EnsembleStudioBarsToCellCount(ABars);
  FLocks := nil;
  FDirtyLayers := [];
  InvalidateCurrent;
  FStatus := essIdle;
  FReport.Status := FStatus;
end;

procedure TEnsembleStudio.InvalidateCurrent;
begin
  FStatus := essDirty;
  FReport := Default(TEnsembleStudioReport);
  FReport.Status := FStatus;
  FReport.FailedPass := -1;
  FReport.FailedCell := -1;
  FReport.DependencyPass := -1;
  FReport.Validation.Issue.Position := -1;
end;

function TEnsembleStudio.GetHasCurrent: Boolean;
begin
  Result := (FStatus = essSolved) and Assigned(FComposition);
end;

function TEnsembleStudio.GetHasBaseline: Boolean;
begin
  Result := Assigned(FComposition);
end;

procedure TEnsembleStudio.RequireCurrent;
begin
  if not HasCurrent then
    raise EEnsembleStudio.Create('no current solved ensemble composition');
end;

procedure TEnsembleStudio.RequireSessionEngine;
begin
  if FPipeline = nil then
    raise EEnsembleStudio.Create(
      'session engine is unavailable; start a new session');
end;

procedure TEnsembleStudio.CheckLayer(
  const ALayer: TWfcMusicEnsembleLayer);
begin
  if (Ord(ALayer) < Ord(Low(TWfcMusicEnsembleLayer))) or
      (Ord(ALayer) > Ord(High(TWfcMusicEnsembleLayer))) then
    raise EEnsembleStudio.Create('unknown ensemble layer');
end;

procedure TEnsembleStudio.CheckCell(
  const ALayer: TWfcMusicEnsembleLayer; const APosition: Integer);
begin
  CheckLayer(ALayer);
  if (APosition < 0) or (APosition >= FCellCount) then
    raise EEnsembleStudio.CreateFmt(
      'ensemble cell must be in 0..%d', [FCellCount - 1]);
end;

procedure TEnsembleStudio.PutLock(
  const ALayer: TWfcMusicEnsembleLayer; const APosition: Integer;
  const AToken: TWfcModelToken);
var
  I: Integer;
  J: Integer;
begin
  I := 0;
  while I < Length(FLocks) do
  begin
    if (Ord(FLocks[I].Layer) > Ord(ALayer)) or
        ((FLocks[I].Layer = ALayer) and
         (FLocks[I].Position >= APosition)) then Break;
    Inc(I);
  end;
  if (I < Length(FLocks)) and (FLocks[I].Layer = ALayer) and
      (FLocks[I].Position = APosition) then
    FLocks[I].Token := AToken
  else
  begin
    SetLength(FLocks, Length(FLocks) + 1);
    for J := High(FLocks) downto I + 1 do
      FLocks[J] := FLocks[J - 1];
    FLocks[I].Layer := ALayer;
    FLocks[I].Position := APosition;
    FLocks[I].Token := AToken;
  end;
end;

procedure TEnsembleStudio.SetLock(
  const ALayer: TWfcMusicEnsembleLayer; const APosition: Integer;
  const AToken: TWfcModelToken);
begin
  RequireSessionEngine;
  CheckCell(ALayer, APosition);
  if FPipeline.Model[ALayer].FindPublicToken(AToken) < 0 then
    raise EEnsembleStudio.Create(
      'lock token is outside the selected public vocabulary');
  InvalidateCurrent;
  FPipeline.ClearAllowedTokens(ALayer, APosition);
  FPipeline.IntersectAllowedTokens(ALayer, APosition, AToken);
  Include(FDirtyLayers, ALayer);
  PutLock(ALayer, APosition, AToken);
end;

procedure TEnsembleStudio.ClearLock(
  const ALayer: TWfcMusicEnsembleLayer; const APosition: Integer);
var
  I: Integer;
  J: Integer;
begin
  RequireSessionEngine;
  CheckCell(ALayer, APosition);
  InvalidateCurrent;
  FPipeline.ClearAllowedTokens(ALayer, APosition);
  Include(FDirtyLayers, ALayer);
  for I := 0 to High(FLocks) do
    if (FLocks[I].Layer = ALayer) and
        (FLocks[I].Position = APosition) then
    begin
      for J := I to High(FLocks) - 1 do
        FLocks[J] := FLocks[J + 1];
      SetLength(FLocks, Length(FLocks) - 1);
      Exit;
    end;
end;

procedure TEnsembleStudio.ClearLocks;
var
  I: Integer;
begin
  RequireSessionEngine;
  InvalidateCurrent;
  for I := 0 to High(FLocks) do
  begin
    FPipeline.ClearAllowedTokens(FLocks[I].Layer, FLocks[I].Position);
    Include(FDirtyLayers, FLocks[I].Layer);
  end;
  FLocks := nil;
end;

function TEnsembleStudio.CopyLocks: TEnsembleStudioLocks;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FLocks));
  for I := 0 to High(Result) do
    Result[I] := FLocks[I];
end;

function TEnsembleStudio.PublicTokens(
  const ALayer: TWfcMusicEnsembleLayer): TWfcModelTokens;
begin
  CheckLayer(ALayer);
  Result := FModels.Harmony.CopyPublicTokens;
  case ALayer of
    wmelHarmony: Result := FModels.Harmony.CopyPublicTokens;
    wmelRhythm: Result := FModels.Rhythm.CopyPublicTokens;
    wmelEnsemble: Result := FModels.Ensemble.CopyPublicTokens;
  end;
end;

function TEnsembleStudio.CellTokens(
  const ALayer: TWfcMusicEnsembleLayer): TWfcModelTokens;
begin
  CheckLayer(ALayer);
  if not HasCurrent then Exit(nil);
  Result := FComposition.CopyGenerated(ALayer).Tokens;
end;

function TEnsembleStudio.EnsembleFrames: TWfcMusicEnsembleFrames;
begin
  if not HasCurrent then Exit(nil);
  Result := FComposition.CopyEnsembleFrames;
end;

procedure TEnsembleStudio.AddSolveCounters(
  const AReport: TGraphSolveReport);
var
  I: Integer;
begin
  for I := 0 to High(AReport.Passes) do
  begin
    Inc(FReport.Decisions, AReport.Passes[I].Decisions);
    Inc(FReport.Propagations, AReport.Passes[I].Propagations);
    Inc(FReport.Contradictions, AReport.Passes[I].Contradictions);
    Inc(FReport.Backtracks, AReport.Passes[I].Backtracks);
  end;
end;

procedure TEnsembleStudio.CaptureFinal(const AReport: TGraphSolveReport);
var
  I: Integer;
begin
  AddSolveCounters(AReport);
  FReport.FailedPass := AReport.FailedPassIndex;
  FReport.FailedCell := AReport.Contradiction.EntryIndex;
  FReport.DependencyPass := AReport.Contradiction.DependencyPassIndex;
  FReport.FailureKind := AReport.Contradiction.Kind;
  FReport.TraceHash := AReport.TraceHash;
  SetLength(FReport.Passes, Length(AReport.Passes));
  for I := 0 to High(AReport.Passes) do
    FReport.Passes[I] := AReport.Passes[I];
end;

procedure TEnsembleStudio.CaptureNegotiation(
  const AReport: TGraphNegotiationReport);
var
  I: Integer;
begin
  FReport.PassBacktracks := AReport.PassBacktracks;
  FReport.Rounds := Length(AReport.Attempts) + 1;
  FReport.TranscriptHash := AReport.TranscriptHash;
  for I := 0 to High(AReport.Attempts) do
    AddSolveCounters(AReport.Attempts[I].SolveReport);
  CaptureFinal(AReport.FinalReport);
  case AReport.Status of
    gnsSolved: FStatus := essSolved;
    gnsContradiction: FStatus := essContradiction;
    gnsSolverBacktrackLimit: FStatus := essSolverLimit;
    gnsPassBacktrackLimit: FStatus := essPassLimit;
  end;
end;

procedure TEnsembleStudio.BuildScope(
  const ARoot: TWfcMusicEnsembleLayer);
var
  LCovered: TWfcMusicEnsembleLayerSet;
  LLayer: TWfcMusicEnsembleLayer;
  LSelected: TWfcMusicEnsembleLayerSet;
begin
  LSelected := [ARoot];
  LCovered := LSelected;
  if (wmelHarmony in LCovered) or (wmelRhythm in LCovered) then
    Include(LCovered, wmelEnsemble);
  for LLayer := Low(TWfcMusicEnsembleLayer) to
      High(TWfcMusicEnsembleLayer) do
    if (LLayer in FDirtyLayers) and not (LLayer in LCovered) then
    begin
      Include(LSelected, LLayer);
      Include(LCovered, LLayer);
      if LLayer in [wmelHarmony, wmelRhythm] then
        Include(LCovered, wmelEnsemble);
    end;
  for LLayer := Low(TWfcMusicEnsembleLayer) to
      High(TWfcMusicEnsembleLayer) do
  begin
    if LLayer in LSelected then
    begin
      SetLength(FReport.RequestedRootIndices,
        Length(FReport.RequestedRootIndices) + 1);
      FReport.RequestedRootIndices[
        High(FReport.RequestedRootIndices)] := Ord(LLayer);
    end;
    if LLayer in LCovered then
    begin
      SetLength(FReport.ActivePassIndices,
        Length(FReport.ActivePassIndices) + 1);
      FReport.ActivePassIndices[
        High(FReport.ActivePassIndices)] := Ord(LLayer);
    end;
  end;
end;

function TEnsembleStudio.Run(const AAction: TEnsembleStudioAction;
  const AOptions: TEnsembleStudioOptions): Boolean;
var
  LCommitted: Boolean;
  LComposition: TWfcMusicEnsembleComposition;
  LGenerated: TWfcGeneratedSequence;
  LNegotiation: TGraphNegotiationOptions;
  LNegotiationReport: TWfcMusicEnsembleNegotiationReport;
  LReport: TWfcMusicEnsembleReport;
  LRoot: TWfcMusicEnsembleLayer;
  LSelective: TWfcMusicEnsembleSelectiveNegotiationReport;
  LSolve: TGraphSolveOptions;
  I: Integer;
begin
  InvalidateCurrent;
  RequireSessionEngine;
  if (Ord(AAction) < Ord(Low(TEnsembleStudioAction))) or
      (Ord(AAction) > Ord(High(TEnsembleStudioAction))) then
    raise EEnsembleStudio.Create('unknown regeneration action');
  if (AOptions.MaxBacktracks < 0) or
      (AOptions.MaxPassBacktracks < 0) then
    raise EEnsembleStudio.Create(
      'Studio backtrack allowances must be nonnegative Integers');
  if (not AOptions.Negotiated) and
      (AOptions.MaxPassBacktracks <> 0) then
    raise EEnsembleStudio.Create(
      'one-way solving requires zero pass backtracks');
  if (AAction <> esaGenerate) and (not HasBaseline) then
    raise EEnsembleStudio.Create(
      'generate a baseline before selective regeneration');

  FReport.Action := AAction;
  FReport.Negotiated := AOptions.Negotiated;
  LSolve := DefaultGraphSolveOptions;
  LSolve.MaxBacktracks := AOptions.MaxBacktracks;
  LSolve.CaptureTrace := AOptions.CaptureTrace;
  LNegotiation := DefaultGraphNegotiationOptions;
  LNegotiation.SolveOptions := LSolve;
  LNegotiation.MaxPassBacktracks := AOptions.MaxPassBacktracks;
  case AAction of
    esaGenerate, esaHarmony: LRoot := wmelHarmony;
    esaRhythm: LRoot := wmelRhythm;
    esaEnsemble: LRoot := wmelEnsemble;
  else
    LRoot := wmelHarmony;
  end;
  if AAction <> esaGenerate then
    BuildScope(LRoot);

  LComposition := nil;
  LCommitted := False;
  try
    if AOptions.Negotiated then
    begin
      if AAction = esaGenerate then
      begin
        Result := FPipeline.TryGenerateNegotiated(LNegotiation,
          LComposition, LNegotiationReport);
        LCommitted := Result;
        FReport.MusicStatus := LNegotiationReport.Status;
        FReport.MusicFailedLayer := LNegotiationReport.FailedLayer;
        FReport.Capture := LNegotiationReport.Capture;
        FReport.Validation := LNegotiationReport.Validation;
        CaptureNegotiation(LNegotiationReport.Search);
      end
      else
      begin
        Result := FPipeline.TryRegenerateNegotiatedFrom(LRoot,
          LNegotiation, LComposition, LSelective);
        LCommitted := Result;
        FReport.MusicStatus := LSelective.Status;
        FReport.MusicFailedLayer := LSelective.FailedLayer;
        FReport.Capture := LSelective.Capture;
        FReport.Validation := LSelective.Validation;
        CaptureNegotiation(LSelective.Search.Search);
        FReport.RequestedRootIndices := Copy(
          LSelective.Search.RequestedRootIndices, 0,
          Length(LSelective.Search.RequestedRootIndices));
        FReport.ActivePassIndices := Copy(
          LSelective.Search.ActivePassIndices, 0,
          Length(LSelective.Search.ActivePassIndices));
        FReport.TranscriptHash := LSelective.Search.TranscriptHash;
      end;
    end
    else
    begin
      if AAction = esaGenerate then
        Result := FPipeline.TryGenerate(LSolve, LComposition, LReport)
      else
        Result := FPipeline.TryRegenerateFrom(LRoot, LSolve,
          LComposition, LReport);
      LCommitted := Result;
      FReport.MusicStatus := LReport.Status;
      FReport.MusicFailedLayer := LReport.FailedLayer;
      FReport.Capture := LReport.Capture;
      FReport.Validation := LReport.Validation;
      CaptureFinal(LReport.Solve);
      FReport.Rounds := 1;
      case LReport.Solve.Status of
        gssSolved: FStatus := essSolved;
        gssContradiction: FStatus := essContradiction;
        gssBacktrackLimit: FStatus := essSolverLimit;
      end;
    end;
    if AAction = esaGenerate then
    begin
      SetLength(FReport.ActivePassIndices, 3);
      for I := 0 to 2 do
        FReport.ActivePassIndices[I] := I;
    end;
    if Result then
    begin
      if not EnsembleStudioCompositionIsValid(
          LComposition, FCellCount) then
        raise EEnsembleStudio.Create(
          'independent Ensemble Studio validation failed');
      if LComposition.Seed <> FSeed then
        raise EEnsembleStudio.Create(
          'generated composition seed differs from session');
      for I := 0 to High(FLocks) do
      begin
        LGenerated := LComposition.CopyGenerated(FLocks[I].Layer);
        if LGenerated.Tokens[FLocks[I].Position] <>
            FLocks[I].Token then
          raise EEnsembleStudio.Create(
            'independent public lock validation failed');
      end;
      FreeAndNil(FComposition);
      FComposition := LComposition;
      LComposition := nil;
      FDirtyLayers := [];
      FStatus := essSolved;
    end
    else if FStatus = essSolved then
      FStatus := essContradiction;
    FReport.Status := FStatus;
  except
    if LCommitted then
    begin
      FreeAndNil(FComposition);
      FreeAndNil(FPipeline);
      FLocks := nil;
      FDirtyLayers := [];
    end;
    FStatus := essDirty;
    FReport.Status := FStatus;
    LComposition.Free;
    raise;
  end;
  LComposition.Free;
end;

function TEnsembleStudio.CopyReport: TEnsembleStudioReport;
begin
  Result := FReport;
  Result.Passes := Copy(FReport.Passes, 0, Length(FReport.Passes));
  Result.RequestedRootIndices := Copy(FReport.RequestedRootIndices, 0,
    Length(FReport.RequestedRootIndices));
  Result.ActivePassIndices := Copy(FReport.ActivePassIndices, 0,
    Length(FReport.ActivePassIndices));
end;

function TEnsembleStudio.CurrentIsValid: Boolean;
begin
  Result := HasCurrent and EnsembleStudioCompositionIsValid(
    FComposition, FCellCount);
end;

function IndicesText(const AIndices: TGraphPassIndices): String;
var
  I: Integer;
begin
  Result := '[';
  for I := 0 to High(AIndices) do
  begin
    if I > 0 then Result := Result + ',';
    Result := Result + IntToStr(AIndices[I]);
  end;
  Result := Result + ']';
end;

function FailureName(const AKind: TGraphContradictionKind): String;
const
  NAMES: array[TGraphContradictionKind] of String = (
    'none', 'empty-domain', 'invalid-lock', 'adjacency', 'previous-pass',
    'required-support', 'final-validation', 'pass-dependency',
    'entry-domain', 'excluded-assignment', 'connectivity');
begin
  Result := NAMES[AKind];
end;

function DispositionName(const ADisposition: TGraphPassDisposition): String;
const
  NAMES: array[TGraphPassDisposition] of String = (
    'not-run', 'reused', 'cleared', 'copied', 'solved', 'failed');
begin
  Result := NAMES[ADisposition];
end;

function TEnsembleStudio.RunReportText: String;
const
  MUSIC_STATUS_NAMES: array[TWfcMusicEnsembleStatus] of String = (
    'not-run', 'completed', 'solve-failed', 'capture-failed',
    'validation-failed');
var
  I: Integer;
  LPass: TGraphPassSolveReport;
begin
  Result := EnsembleStudioStatusName(FStatus) +
    ' seed=' + UIntToStr(FSeed) +
    ' bars=' + IntToStr(FBars) +
    ' cells=' + IntToStr(FCellCount) +
    ' rounds=' + IntToStr(FReport.Rounds) +
    ' pass-backtracks=' + IntToStr(FReport.PassBacktracks) + #10 +
    'requested=' + IndicesText(FReport.RequestedRootIndices) +
    ' active=' + IndicesText(FReport.ActivePassIndices) + #10 +
    'all-round decisions=' + IntToStr(FReport.Decisions) +
    ' propagations=' + IntToStr(FReport.Propagations) +
    ' contradictions=' + IntToStr(FReport.Contradictions) +
    ' backtracks=' + IntToStr(FReport.Backtracks) + #10 +
    'failed-pass=' + IntToStr(FReport.FailedPass) +
    ' cell=' + IntToStr(FReport.FailedCell) +
    ' provider=' + IntToStr(FReport.DependencyPass) +
    ' kind=' + FailureName(FReport.FailureKind) + #10 +
    'music-status=' + MUSIC_STATUS_NAMES[FReport.MusicStatus] + #10 +
    'music-validation=' +
      DescribeWfcMusicEnsembleValidationIssue(FReport.Validation.Issue) + #10 +
    'trace=' + UpperCase(IntToHex(FReport.TraceHash, 8)) +
    ' transcript=' + UpperCase(IntToHex(FReport.TranscriptHash, 8));
  if FReport.MusicStatus in [wmesCaptureFailed, wmesValidationFailed] then
    Result := Result + #10 + 'music-failed-layer=' +
      WfcMusicEnsembleLayerName(FReport.MusicFailedLayer);
  for I := 0 to High(FReport.Passes) do
  begin
    LPass := FReport.Passes[I];
    Result := Result + #10 + IntToStr(I) + ' ' +
      WfcMusicEnsembleLayerName(TWfcMusicEnsembleLayer(I)) +
      ' executed=' + LowerCase(BoolToStr(LPass.Executed, True)) +
      ' disposition=' + DispositionName(LPass.Disposition) +
      ' decisions=' + IntToStr(LPass.Decisions) +
      ' propagations=' + IntToStr(LPass.Propagations) +
      ' backtracks=' + IntToStr(LPass.Backtracks);
  end;
end;

function TEnsembleStudio.SignatureText: String;
begin
  RequireCurrent;
  Result := WfcMusicEnsembleCompositionSignatureHex(
    FComposition.Signature);
end;

function TEnsembleStudio.CopyScore: TWfcMusicScore;
begin
  RequireCurrent;
  Result := FComposition.CopyScore;
end;

function TEnsembleStudio.ScoreText: String;
var
  LScore: TWfcMusicScore;
begin
  LScore := CopyScore;
  try
    Result := EncodeWfcMusicText(LScore);
  finally
    LScore.Free;
  end;
end;

function TEnsembleStudio.MidiBytes: TWfcMidiBytes;
var
  LScore: TWfcMusicScore;
begin
  LScore := CopyScore;
  try
    Result := EncodeWfcMusicMidi(LScore);
  finally
    LScore.Free;
  end;
end;

function TEnsembleStudio.TryWavePreview(
  out ABytes: TWfcMusicAudioBytes; out AFrameCount: Integer;
  out AFailure: String): Boolean;
var
  LClip: TWfcMusicPcm16Clip;
  LScore: TWfcMusicScore;
begin
  ABytes := nil;
  AFrameCount := 0;
  AFailure := '';
  RequireCurrent;
  LScore := CopyScore;
  LClip := nil;
  try
    try
      LClip := RenderWfcMusicAudio(LScore,
        DefaultWfcMusicAudioOptions);
      AFrameCount := LClip.FrameCount;
      ABytes := EncodeWfcMusicWave(LClip);
      Result := True;
    except
      on E: EWfcMusicAudio do
      begin
        ABytes := nil;
        AFrameCount := 0;
        AFailure := E.Message;
        Result := False;
      end;
    end;
  finally
    LClip.Free;
    LScore.Free;
  end;
end;

end.
