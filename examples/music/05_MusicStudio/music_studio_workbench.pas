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
unit music_studio_workbench;

{$mode delphi}{$H+}

interface

uses
  SysUtils, wfc, wfc_model, wfc_sequence, wfc_music, wfc_music_sequence,
  wfc_music_passes, wfc_midi_smf;

const
  MUSIC_STUDIO_VERSION = 1;
  MUSIC_STUDIO_CELL_COUNT = 16;
  MUSIC_STUDIO_QUANTUM = 240;
  MUSIC_STUDIO_TPQ = 480;
  MUSIC_STUDIO_TEMPO = 500000;
  MUSIC_STUDIO_CORPUS_COUNT = 4;

type
  EMusicStudio = class(Exception);
  TWfcMusicStudioAction = (msaGenerate, msaHarmony, msaRhythm, msaMelody);
  TWfcMusicStudioStatus = (mssIdle, mssDirty, mssSolved, mssContradiction,
    mssSolverLimit, mssPassLimit);
  TWfcMusicStudioOptions = record
    Negotiated: Boolean;
    MaxBacktracks: Integer;
    MaxPassBacktracks: Integer;
    CaptureTrace: Boolean;
  end;
  TWfcMusicStudioLock = record
    Layer: TWfcMusicPassLayer;
    Position: Integer;
    Token: TWfcModelToken;
  end;
  TWfcMusicStudioLocks = array of TWfcMusicStudioLock;
  TWfcMusicStudioReport = record
    Status: TWfcMusicStudioStatus;
    Action: TWfcMusicStudioAction;
    Negotiated: Boolean;
    Rounds, PassBacktracks: Integer;
    Decisions, Propagations, Contradictions, Backtracks: Integer;
    FailedPass, FailedCell, DependencyPass: Integer;
    FailureKind: TGraphContradictionKind;
    MusicStatus: TWfcMusicPassStatus;
    MusicFailedLayer: TWfcMusicPassLayer;
    Capture: TWfcMusicPassCaptureReports;
    Validation: TWfcMusicPassValidationReport;
    ValidationKind: TWfcMusicPassValidationIssueKind;
    ValidationLayer: TWfcMusicPassLayer;
    ValidationPosition: Integer;
    TraceHash, TranscriptHash: Cardinal;
    Passes: TGraphPassSolveReports;
    RequestedRootIndices, ActivePassIndices: TGraphPassIndices;
  end;

  { Shared example owner. A committed engine baseline can survive a failed edit
    without being exposed as current output. Models and graph remain private. }
  TWfcMusicStudio = class
  strict private
    FModels: TWfcMusicPassModels;
    FPipeline: TWfcMusicPassPipeline;
    FComposition: TWfcMusicComposition;
    FLocks: TWfcMusicStudioLocks;
    FReport: TWfcMusicStudioReport;
    FSeed: TGraphSeed;
    FStatus: TWfcMusicStudioStatus;
    FDirtyLayers: TWfcMusicPassLayerSet;
    procedure BuildModels;
    procedure RequireCurrent;
    procedure CheckCell(const ALayer: TWfcMusicPassLayer;
      const APosition: Integer);
    procedure PutLock(const ALayer: TWfcMusicPassLayer;
      const APosition: Integer; const AToken: TWfcModelToken);
    procedure AddSolveCounters(const AReport: TGraphSolveReport);
    procedure CaptureFinal(const AReport: TGraphSolveReport);
    procedure CaptureNegotiation(const AReport: TGraphNegotiationReport);
    procedure CaptureValidation(const AReport: TWfcMusicPassValidationReport);
    procedure BuildScope(const ARoot: TWfcMusicPassLayer);
    function GetHasCurrent: Boolean;
    function GetHasBaseline: Boolean;
  public
    constructor Create(const ASeed: TGraphSeed);
    destructor Destroy; override;
    procedure Reset(const ASeed: TGraphSeed);
    procedure InvalidateCurrent;
    procedure SetLock(const ALayer: TWfcMusicPassLayer;
      const APosition: Integer; const AToken: TWfcModelToken);
    procedure ClearLock(const ALayer: TWfcMusicPassLayer;
      const APosition: Integer);
    procedure ClearLocks;
    procedure LockOpeningMotif(const ACellCount: Integer);
    function CopyLocks: TWfcMusicStudioLocks;
    function PublicTokens(const ALayer: TWfcMusicPassLayer): TWfcModelTokens;
    function CellTokens(const ALayer: TWfcMusicPassLayer): TWfcModelTokens;
    function MelodyCells: TWfcMusicMelodyCells;
    function Run(const AAction: TWfcMusicStudioAction;
      const AOptions: TWfcMusicStudioOptions): Boolean;
    function CopyReport: TWfcMusicStudioReport;
    function RunReportText: String;
    function SignatureText: String;
    function CompositionText: String;
    function ScoreText: String;
    function CopyScore: TWfcMusicScore;
    function MidiBytes: TWfcMidiBytes;
    property Seed: TGraphSeed read FSeed;
    property HasCurrent: Boolean read GetHasCurrent;
    property HasBaseline: Boolean read GetHasBaseline;
    property Status: TWfcMusicStudioStatus read FStatus;
  end;

function DefaultMusicStudioOptions: TWfcMusicStudioOptions;
function MusicStudioStatusName(const AStatus: TWfcMusicStudioStatus): String;
function MusicStudioPitchName(const APitch: Integer): String;
function MusicStudioTokenLabel(const ALayer: TWfcMusicPassLayer;
  const AToken: TWfcModelToken): String;
function MusicStudioCorpus(const AIndex: Integer): TWfcMusicMelodyCells;
function MusicStudioCompositionIsValid(
  const AComposition: TWfcMusicComposition): Boolean;
function MusicStudioByteSignature(const ABytes: array of Byte): String;
function MusicStudioTextSignature(const AText: String): String;

implementation

uses
  wfc_sequence_learn, wfc_sequence_graph, wfc_music_passes_text,
  wfc_music_text, wfc_music_midi;

procedure HashByte(var H: Cardinal; const B: Byte);
{$PUSH}{$Q-}
var V: Cardinal;
begin
  V := H xor Cardinal(B);
  H := (V + (V shl 1) + (V shl 4) + (V shl 7) + (V shl 8) +
    (V shl 24)) and Cardinal($FFFFFFFF);
end;
{$POP}

function MusicStudioByteSignature(const ABytes: array of Byte): String;
var H: Cardinal; I: Integer;
begin
  H := Cardinal(2166136261);
  for I := 0 to High(ABytes) do HashByte(H, ABytes[I]);
  Result := UpperCase(IntToHex(H, 8));
end;

function MusicStudioTextSignature(const AText: String): String;
var H: Cardinal; I: Integer;
begin
  H := Cardinal(2166136261);
  for I := 1 to Length(AText) do
  begin
    if Ord(AText[I]) > 127 then
      raise EMusicStudio.Create('artifact signature requires canonical ASCII');
    HashByte(H, Byte(Ord(AText[I])));
  end;
  Result := UpperCase(IntToHex(H, 8));
end;

function DefaultMusicStudioOptions: TWfcMusicStudioOptions;
begin
  Result.Negotiated := True;
  Result.MaxBacktracks := 256;
  Result.MaxPassBacktracks := 16;
  Result.CaptureTrace := False;
end;

function MusicStudioStatusName(const AStatus: TWfcMusicStudioStatus): String;
begin
  case AStatus of
    mssIdle: Result := 'idle';
    mssDirty: Result := 'dirty';
    mssSolved: Result := 'solved';
    mssContradiction: Result := 'contradiction';
    mssSolverLimit: Result := 'solver-limit';
    mssPassLimit: Result := 'pass-limit';
  else Result := 'unknown';
  end;
end;

function MusicStudioPitchName(const APitch: Integer): String;
const NAMES: array[0..11] of String =
  ('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B');
begin
  if (APitch < 0) or (APitch > 127) then
    raise EMusicStudio.Create('preview pitch must be in 0..127');
  Result := NAMES[APitch mod 12] + IntToStr(APitch div 12 - 1);
end;

function MusicStudioTokenLabel(const ALayer: TWfcMusicPassLayer;
  const AToken: TWfcModelToken): String;
var
  M: TWfcMusicMelodyCell;
  H: TWfcMusicHarmonyCell;
  R: TWfcMusicRhythmCell;
begin
  case ALayer of
    wmplHarmony:
      begin
        H := DecodeWfcMusicHarmonyCell(AToken);
        if H.Kind = wmhckRest then Result := 'rest'
        else Result := 'class ' + IntToStr(H.PitchClass) + ' (' +
          MusicStudioPitchName(60 + H.PitchClass) + ')';
      end;
    wmplRhythm:
      begin
        R := DecodeWfcMusicRhythmCell(AToken);
        case R.Action of
          wmcaRest: Result := 'rest';
          wmcaAttack: Result := 'attack';
          wmcaHold: Result := 'hold';
        end;
      end;
    wmplMelody:
      begin
        M := DecodeWfcMusicMelodyCell(AToken);
        if M.Action = wmcaRest then Result := 'rest'
        else
        begin
          Result := MusicStudioPitchName(M.Pitch);
          if M.Action = wmcaAttack then Result := Result + ' attack'
          else Result := Result + ' hold';
          Result := Result + ' / ' + IntToStr(M.Velocity);
        end;
      end;
  else raise EMusicStudio.Create('unknown music layer');
  end;
end;

function MusicStudioCorpus(const AIndex: Integer): TWfcMusicMelodyCells;
const
  PITCHES: array[0..3, 0..8] of Integer = (
    (60,64,67,69,67,65,64,62,60),
    (60,62,64,67,64,62,65,64,60),
    (60,67,64,72,67,69,67,64,60),
    (60,65,69,67,65,64,62,67,60));
  ATTACKS: array[0..8] of Integer = (0,2,3,4,6,8,10,11,12);
var I, J: Integer;
begin
  if (AIndex < 0) or (AIndex >= MUSIC_STUDIO_CORPUS_COUNT) then
    raise EMusicStudio.Create('unknown project-authored music corpus');
  Result := nil;
  SetLength(Result, MUSIC_STUDIO_CELL_COUNT);
  for I := 0 to High(Result) do Result[I] := MakeWfcMusicRestCell;
  for I := 0 to High(ATTACKS) do
  begin
    J := ATTACKS[I];
    Result[J] := MakeWfcMusicAttackCell(PITCHES[AIndex,I], 96);
    if (J + 1 < 14) and
        ((I = High(ATTACKS)) or (ATTACKS[I + 1] <> J + 1)) then
      Result[J + 1] := MakeWfcMusicHoldCell(PITCHES[AIndex,I], 96);
  end;
end;

function BuildTemplate: TWfcMusicScore;
var
  T: TWfcMusicTracks;
  V: TWfcMusicVoices;
  M: TWfcMusicMeterChanges;
  P: TWfcMusicTempoChanges;
begin
  SetLength(T, 1); T[0] := MakeWfcMusicTrack('lead', 'Music Studio');
  SetLength(V, 1); V[0] := MakeWfcMusicVoice(0, 'melody');
  SetLength(M, 1); M[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(P, 1); P[0] := MakeWfcMusicTempoChange(0, MUSIC_STUDIO_TEMPO);
  Result := TWfcMusicScore.Create(MUSIC_STUDIO_TPQ, 12,
    MUSIC_STUDIO_CELL_COUNT * MUSIC_STUDIO_QUANTUM, T, V, M, P,
    RebuildWfcMusicVoiceSpans(MusicStudioCorpus(0), 0, MUSIC_STUDIO_QUANTUM));
end;

function MusicStudioCompositionIsValid(
  const AComposition: TWfcMusicComposition): Boolean;
var
  H: TWfcMusicHarmonyCells;
  R: TWfcMusicRhythmCells;
  M, Reprojected: TWfcMusicMelodyCells;
  S: TWfcMusicScore;
  I: Integer;
begin
  Result := False;
  try
  if not Assigned(AComposition) then Exit;
  if (AComposition.CellCount <> MUSIC_STUDIO_CELL_COUNT) or
      (AComposition.QuantumTicks <> MUSIC_STUDIO_QUANTUM) then Exit;
  H := AComposition.CopyHarmonyCells;
  R := AComposition.CopyRhythmCells;
  M := AComposition.CopyMelodyCells;
  if (Length(H) <> MUSIC_STUDIO_CELL_COUNT) or
      (Length(R) <> Length(H)) or (Length(M) <> Length(H)) then Exit;
  for I := 0 to High(M) do
  begin
    if H[I].StepsPerOctave <> 12 then Exit;
    if M[I].Action <> R[I].Action then Exit;
    if M[I].Action <> wmcaRest then
    begin
      if (M[I].Pitch < 60) or (M[I].Pitch > 72) or
          (M[I].Velocity <> 96) or (H[I].Kind <> wmhckPitchClass) or
          (H[I].StepsPerOctave <> 12) or
          (H[I].PitchClass <> M[I].Pitch mod 12) then Exit;
      if M[I].Action = wmcaHold then
      begin
        if I = 0 then Exit;
        if (M[I - 1].Action = wmcaRest) or
            (M[I - 1].Pitch <> M[I].Pitch) or
            (M[I - 1].Velocity <> M[I].Velocity) then Exit;
      end;
    end;
  end;
  S := AComposition.CopyScore;
  try
    if (S.TrackCount <> 1) or (S.VoiceCount <> 1) or
        (S.TrackAt(0).Id <> 'lead') or (S.TrackAt(0).Name <> 'Music Studio') or
        (S.VoiceAt(0).Id <> 'melody') or (S.VoiceAt(0).TrackIndex <> 0) or
        (S.MeterCount <> 1) or (S.MeterAt(0).Tick <> 0) or
        (S.MeterAt(0).Numerator <> 4) or (S.MeterAt(0).Denominator <> 4) or
        (S.StepsPerOctave <> 12) or
        (S.TicksPerQuarter <> MUSIC_STUDIO_TPQ) or
        (S.LengthTicks <> MUSIC_STUDIO_CELL_COUNT * MUSIC_STUDIO_QUANTUM) or
        (S.TempoCount <> 1) or
        (S.TempoAt(0).Tick <> 0) or
        (S.TempoAt(0).MicrosecondsPerQuarter <> MUSIC_STUDIO_TEMPO) then Exit;
    Reprojected := ProjectWfcMusicVoiceToMelodyCells(S, 0, MUSIC_STUDIO_QUANTUM);
    if Length(Reprojected) <> Length(M) then Exit;
    for I := 0 to High(M) do
      if (M[I].Action <> Reprojected[I].Action) or
          (M[I].Pitch <> Reprojected[I].Pitch) or
          (M[I].Velocity <> Reprojected[I].Velocity) then Exit;
  finally S.Free; end;
  Result := CalculateWfcMusicCompositionSignature(AComposition) =
    AComposition.Signature;
  except
    on EWfcMusic do Result := False;
  end;
end;

constructor TWfcMusicStudio.Create(const ASeed: TGraphSeed);
begin
  inherited Create;
  BuildModels;
  Reset(ASeed);
end;

destructor TWfcMusicStudio.Destroy;
begin
  FComposition.Free;
  FPipeline.Free;
  FModels.Melody.Free;
  FModels.Rhythm.Free;
  FModels.Harmony.Free;
  inherited Destroy;
end;

procedure TWfcMusicStudio.BuildModels;
var
  H, R, M: TWfcSequenceSamples;
  C: TWfcMusicMelodyCells;
  I: Integer;
begin
  SetLength(H, MUSIC_STUDIO_CORPUS_COUNT);
  SetLength(R, MUSIC_STUDIO_CORPUS_COUNT);
  SetLength(M, MUSIC_STUDIO_CORPUS_COUNT);
  for I := 0 to MUSIC_STUDIO_CORPUS_COUNT - 1 do
  begin
    C := MusicStudioCorpus(I);
    H[I] := MakeWfcSequenceSample(EncodeWfcMusicHarmonyCells(
      ProjectWfcMusicMelodyToHarmony(C, 12)));
    R[I] := MakeWfcSequenceSample(EncodeWfcMusicRhythmCells(
      ProjectWfcMusicMelodyToRhythm(C)));
    M[I] := MakeWfcSequenceSample(EncodeWfcMusicMelodyCells(C));
  end;
  FModels.Harmony := LearnSequenceModelCorpus(H, 3);
  { Rhythm is the fixed two-bar form; harmony and melody retain shorter
    contexts for structural recombination within that form. }
  FModels.Rhythm := LearnSequenceModelCorpus(R, MUSIC_STUDIO_CELL_COUNT);
  FModels.Melody := LearnSequenceModelCorpus(M, 3);
end;

procedure TWfcMusicStudio.Reset(const ASeed: TGraphSeed);
var
  T: TWfcMusicScore;
  C: TWfcMusicPassConfig;
  P: TWfcMusicPassPipeline;
begin
  T := BuildTemplate;
  try
    C := DefaultWfcMusicPassConfig(T, MUSIC_STUDIO_QUANTUM, ASeed);
    C.Models := FModels;
    P := TWfcMusicPassPipeline.Create(C);
  finally T.Free; end;
  FPipeline.Free; FPipeline := P;
  FreeAndNil(FComposition);
  FSeed := ASeed;
  FLocks := nil;
  FDirtyLayers := [];
  InvalidateCurrent;
  FStatus := mssIdle;
  FReport.Status := FStatus;
end;

procedure TWfcMusicStudio.InvalidateCurrent;
begin
  FStatus := mssDirty;
  FReport := Default(TWfcMusicStudioReport);
  FReport.Status := FStatus;
  FReport.FailedPass := -1;
  FReport.FailedCell := -1;
  FReport.DependencyPass := -1;
  FReport.ValidationPosition := -1;
end;

function TWfcMusicStudio.GetHasCurrent: Boolean;
begin Result := (FStatus = mssSolved) and Assigned(FComposition); end;

function TWfcMusicStudio.GetHasBaseline: Boolean;
begin Result := Assigned(FComposition); end;

procedure TWfcMusicStudio.RequireCurrent;
begin
  if not HasCurrent then raise EMusicStudio.Create('no current solved composition');
end;

procedure TWfcMusicStudio.CheckCell(const ALayer: TWfcMusicPassLayer;
  const APosition: Integer);
begin
  if (Ord(ALayer) < Ord(Low(TWfcMusicPassLayer))) or
      (Ord(ALayer) > Ord(High(TWfcMusicPassLayer))) then
    raise EMusicStudio.Create('unknown music layer');
  if (APosition < 0) or (APosition >= MUSIC_STUDIO_CELL_COUNT) then
    raise EMusicStudio.Create('music cell must be in 0..15');
end;

procedure TWfcMusicStudio.PutLock(const ALayer: TWfcMusicPassLayer;
  const APosition: Integer; const AToken: TWfcModelToken);
var I, J: Integer;
begin
  I := 0;
  while I < Length(FLocks) do
  begin
    if (Ord(FLocks[I].Layer) > Ord(ALayer)) or
        ((FLocks[I].Layer = ALayer) and (FLocks[I].Position >= APosition)) then Break;
    Inc(I);
  end;
  if (I < Length(FLocks)) and (FLocks[I].Layer = ALayer) and
      (FLocks[I].Position = APosition) then FLocks[I].Token := AToken
  else
  begin
    SetLength(FLocks, Length(FLocks) + 1);
    for J := High(FLocks) downto I + 1 do FLocks[J] := FLocks[J - 1];
    FLocks[I].Layer := ALayer; FLocks[I].Position := APosition;
    FLocks[I].Token := AToken;
  end;
end;

procedure TWfcMusicStudio.SetLock(const ALayer: TWfcMusicPassLayer;
  const APosition: Integer; const AToken: TWfcModelToken);
begin
  CheckCell(ALayer, APosition);
  if not Assigned(FPipeline) then Reset(FSeed);
  if FPipeline.Model[ALayer].FindPublicToken(AToken) < 0 then
    raise EMusicStudio.Create('lock token is outside the selected public vocabulary');
  InvalidateCurrent;
  FPipeline.ClearAllowedTokens(ALayer, APosition);
  FPipeline.IntersectAllowedTokens(ALayer, APosition, AToken);
  Include(FDirtyLayers, ALayer);
  PutLock(ALayer, APosition, AToken);
end;

procedure TWfcMusicStudio.ClearLock(const ALayer: TWfcMusicPassLayer;
  const APosition: Integer);
var I, J: Integer;
begin
  CheckCell(ALayer, APosition);
  if not Assigned(FPipeline) then Reset(FSeed);
  InvalidateCurrent;
  FPipeline.ClearAllowedTokens(ALayer, APosition);
  Include(FDirtyLayers, ALayer);
  for I := 0 to High(FLocks) do
    if (FLocks[I].Layer = ALayer) and (FLocks[I].Position = APosition) then
    begin
      for J := I to High(FLocks) - 1 do FLocks[J] := FLocks[J + 1];
      SetLength(FLocks, Length(FLocks) - 1);
      Exit;
    end;
end;

procedure TWfcMusicStudio.ClearLocks;
var I: Integer;
begin
  InvalidateCurrent;
  for I := 0 to High(FLocks) do
  begin
    FPipeline.ClearAllowedTokens(FLocks[I].Layer, FLocks[I].Position);
    Include(FDirtyLayers, FLocks[I].Layer);
  end;
  FLocks := nil;
end;

procedure TWfcMusicStudio.LockOpeningMotif(const ACellCount: Integer);
var T: TWfcModelTokens; I: Integer;
begin
  RequireCurrent;
  if (ACellCount < 1) or (ACellCount > MUSIC_STUDIO_CELL_COUNT) then
    raise EMusicStudio.Create('motif length must be in 1..16');
  T := FComposition.CopyGenerated(wmplMelody).Tokens;
  SetLength(T, ACellCount);
  InvalidateCurrent;
  FPipeline.IntersectLockedSpan(wmplMelody, 0, T);
  Include(FDirtyLayers, wmplMelody);
  for I := 0 to High(T) do PutLock(wmplMelody, I, T[I]);
end;

function TWfcMusicStudio.CopyLocks: TWfcMusicStudioLocks;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(FLocks));
  for I := 0 to High(Result) do Result[I] := FLocks[I];
end;

function TWfcMusicStudio.PublicTokens(
  const ALayer: TWfcMusicPassLayer): TWfcModelTokens;
begin
  CheckCell(ALayer, 0);
  case ALayer of
    wmplHarmony: Result := FModels.Harmony.CopyPublicTokens;
    wmplRhythm: Result := FModels.Rhythm.CopyPublicTokens;
    wmplMelody: Result := FModels.Melody.CopyPublicTokens;
  end;
end;

function TWfcMusicStudio.CellTokens(
  const ALayer: TWfcMusicPassLayer): TWfcModelTokens;
begin
  CheckCell(ALayer, 0);
  if not HasCurrent then Exit(nil);
  Result := FComposition.CopyGenerated(ALayer).Tokens;
end;

function TWfcMusicStudio.MelodyCells: TWfcMusicMelodyCells;
begin
  if not HasCurrent then Exit(nil);
  Result := FComposition.CopyMelodyCells;
end;

procedure TWfcMusicStudio.AddSolveCounters(const AReport: TGraphSolveReport);
var I: Integer;
begin
  for I := 0 to High(AReport.Passes) do
  begin
    Inc(FReport.Decisions, AReport.Passes[I].Decisions);
    Inc(FReport.Propagations, AReport.Passes[I].Propagations);
    Inc(FReport.Contradictions, AReport.Passes[I].Contradictions);
    Inc(FReport.Backtracks, AReport.Passes[I].Backtracks);
  end;
end;

procedure TWfcMusicStudio.CaptureFinal(const AReport: TGraphSolveReport);
var I: Integer;
begin
  AddSolveCounters(AReport);
  FReport.FailedPass := AReport.FailedPassIndex;
  FReport.FailedCell := AReport.Contradiction.EntryIndex;
  FReport.DependencyPass := AReport.Contradiction.DependencyPassIndex;
  FReport.FailureKind := AReport.Contradiction.Kind;
  FReport.TraceHash := AReport.TraceHash;
  SetLength(FReport.Passes, Length(AReport.Passes));
  for I := 0 to High(AReport.Passes) do FReport.Passes[I] := AReport.Passes[I];
end;

procedure TWfcMusicStudio.CaptureValidation(
  const AReport: TWfcMusicPassValidationReport);
begin
  FReport.Validation := AReport;
  FReport.ValidationKind := AReport.Issue.Kind;
  FReport.ValidationLayer := AReport.Issue.Layer;
  FReport.ValidationPosition := AReport.Issue.Position;
  if AReport.Issue.Kind = wmpvikNone then
  begin
    FReport.ValidationPosition := -1;
    FReport.Validation.Issue.Position := -1;
  end;
end;

procedure TWfcMusicStudio.CaptureNegotiation(
  const AReport: TGraphNegotiationReport);
var I: Integer;
begin
  FReport.PassBacktracks := AReport.PassBacktracks;
  FReport.Rounds := Length(AReport.Attempts) + 1;
  FReport.TranscriptHash := AReport.TranscriptHash;
  for I := 0 to High(AReport.Attempts) do
    AddSolveCounters(AReport.Attempts[I].SolveReport);
  CaptureFinal(AReport.FinalReport);
  case AReport.Status of
    gnsSolved: FStatus := mssSolved;
    gnsContradiction: FStatus := mssContradiction;
    gnsSolverBacktrackLimit: FStatus := mssSolverLimit;
    gnsPassBacktrackLimit: FStatus := mssPassLimit;
  end;
end;

procedure TWfcMusicStudio.BuildScope(const ARoot: TWfcMusicPassLayer);
var Selected, Covered: TWfcMusicPassLayerSet; L: TWfcMusicPassLayer;
begin
  { Mirror the documented three-layer descendant equation for presentation
    of ordinary runs, whose core report exposes execution but not scope. }
  Selected := [ARoot]; Covered := Selected;
  if (wmplHarmony in Covered) or (wmplRhythm in Covered) then
    Include(Covered, wmplMelody);
  for L := Low(TWfcMusicPassLayer) to High(TWfcMusicPassLayer) do
    if (L in FDirtyLayers) and not (L in Covered) then
    begin
      Include(Selected, L); Include(Covered, L);
      if L in [wmplHarmony, wmplRhythm] then Include(Covered, wmplMelody);
    end;
  for L := Low(TWfcMusicPassLayer) to High(TWfcMusicPassLayer) do
  begin
    if L in Selected then
    begin
      SetLength(FReport.RequestedRootIndices, Length(FReport.RequestedRootIndices) + 1);
      FReport.RequestedRootIndices[High(FReport.RequestedRootIndices)] := Ord(L);
    end;
    if L in Covered then
    begin
      SetLength(FReport.ActivePassIndices, Length(FReport.ActivePassIndices) + 1);
      FReport.ActivePassIndices[High(FReport.ActivePassIndices)] := Ord(L);
    end;
  end;
end;

function TWfcMusicStudio.Run(const AAction: TWfcMusicStudioAction;
  const AOptions: TWfcMusicStudioOptions): Boolean;
var
  O: TGraphSolveOptions;
  N: TGraphNegotiationOptions;
  C: TWfcMusicComposition;
  R: TWfcMusicPassReport;
  NR: TWfcMusicPassNegotiationReport;
  SR: TWfcMusicPassSelectiveNegotiationReport;
  Root: TWfcMusicPassLayer;
  I: Integer;
  G: TWfcGeneratedSequence;
  Committed: Boolean;
begin
  if not Assigned(FPipeline) then Reset(FSeed);
  InvalidateCurrent;
  if (Ord(AAction) < Ord(Low(TWfcMusicStudioAction))) or
      (Ord(AAction) > Ord(High(TWfcMusicStudioAction))) then
    raise EMusicStudio.Create('unknown regeneration action');
  if (AOptions.MaxBacktracks < 0) or (AOptions.MaxBacktracks > 1024) or
      (AOptions.MaxPassBacktracks < 0) or (AOptions.MaxPassBacktracks > 32) then
    raise EMusicStudio.Create('Studio budgets are local 0..1024 and pass 0..32');
  if (not AOptions.Negotiated) and (AOptions.MaxPassBacktracks <> 0) then
    raise EMusicStudio.Create('one-way solving requires zero pass backtracks');
  if (AAction <> msaGenerate) and (not HasBaseline) then
    raise EMusicStudio.Create('generate a baseline before selective regeneration');
  FReport.Action := AAction; FReport.Negotiated := AOptions.Negotiated;
  O := DefaultGraphSolveOptions;
  O.MaxBacktracks := AOptions.MaxBacktracks; O.CaptureTrace := AOptions.CaptureTrace;
  N := DefaultGraphNegotiationOptions; N.SolveOptions := O;
  N.MaxPassBacktracks := AOptions.MaxPassBacktracks;
  Root := wmplHarmony;
  case AAction of
    msaGenerate: Root := wmplHarmony;
    msaHarmony: Root := wmplHarmony;
    msaRhythm: Root := wmplRhythm;
    msaMelody: Root := wmplMelody;
  end;
  if AAction <> msaGenerate then BuildScope(Root);
  C := nil;
  Committed := False;
  try
    if AOptions.Negotiated then
    begin
      if AAction = msaGenerate then
      begin
        Result := FPipeline.TryGenerateNegotiated(N, C, NR);
        Committed := Result;
        FReport.MusicStatus := NR.Status;
        FReport.MusicFailedLayer := NR.FailedLayer;
        FReport.Capture := NR.Capture;
        CaptureValidation(NR.Validation);
        CaptureNegotiation(NR.Search);
      end
      else
      begin
        Result := FPipeline.TryRegenerateNegotiatedFrom(Root, N, C, SR);
        Committed := Result;
        FReport.MusicStatus := SR.Status;
        FReport.MusicFailedLayer := SR.FailedLayer;
        FReport.Capture := SR.Capture;
        CaptureValidation(SR.Validation);
        CaptureNegotiation(SR.Search.Search);
        FReport.RequestedRootIndices := Copy(SR.Search.RequestedRootIndices, 0,
          Length(SR.Search.RequestedRootIndices));
        FReport.ActivePassIndices := Copy(SR.Search.ActivePassIndices, 0,
          Length(SR.Search.ActivePassIndices));
        FReport.TranscriptHash := SR.Search.TranscriptHash;
      end;
    end
    else
    begin
      if AAction = msaGenerate then
        Result := FPipeline.TryGenerate(O, C, R)
      else Result := FPipeline.TryRegenerateFrom(Root, O, C, R);
      Committed := Result;
      FReport.MusicStatus := R.Status;
      FReport.MusicFailedLayer := R.FailedLayer;
      FReport.Capture := R.Capture;
      CaptureValidation(R.Validation);
      CaptureFinal(R.Solve);
      FReport.Rounds := 1;
      case R.Solve.Status of
        gssSolved: FStatus := mssSolved;
        gssContradiction: FStatus := mssContradiction;
        gssBacktrackLimit: FStatus := mssSolverLimit;
      end;
    end;
    if AAction = msaGenerate then
    begin
      SetLength(FReport.ActivePassIndices, 3);
      for I := 0 to 2 do FReport.ActivePassIndices[I] := I;
    end;
    if Result then
    begin
      if not MusicStudioCompositionIsValid(C) then
        raise EMusicStudio.Create('independent Studio music validation failed');
      if C.Seed <> FSeed then
        raise EMusicStudio.Create('generated composition seed differs from session');
      for I := 0 to High(FLocks) do
      begin
        G := C.CopyGenerated(FLocks[I].Layer);
        if G.Tokens[FLocks[I].Position] <> FLocks[I].Token then
          raise EMusicStudio.Create('independent public lock validation failed');
      end;
      FreeAndNil(FComposition); FComposition := C; C := nil;
      FDirtyLayers := [];
      FStatus := mssSolved;
    end
    else if FStatus = mssSolved then FStatus := mssContradiction;
    FReport.Status := FStatus;
  except
    if Committed then
    begin
      { The core has already committed. A failing presentation invariant or
        report copy cannot roll it back; drop both baselines instead of
        keeping an old composition paired with a new engine state. }
      FreeAndNil(FComposition);
      FreeAndNil(FPipeline);
      FLocks := nil;
      FDirtyLayers := [];
    end;
    FStatus := mssDirty; FReport.Status := FStatus;
    C.Free;
    raise;
  end;
  C.Free;
end;

function TWfcMusicStudio.CopyReport: TWfcMusicStudioReport;
begin
  Result := FReport;
  Result.Passes := Copy(FReport.Passes, 0, Length(FReport.Passes));
  Result.RequestedRootIndices := Copy(FReport.RequestedRootIndices, 0,
    Length(FReport.RequestedRootIndices));
  Result.ActivePassIndices := Copy(FReport.ActivePassIndices, 0,
    Length(FReport.ActivePassIndices));
end;

function IndicesText(const A: TGraphPassIndices): String;
var I: Integer;
begin
  Result := '[';
  for I := 0 to High(A) do
  begin
    if I > 0 then Result := Result + ',';
    Result := Result + IntToStr(A[I]);
  end;
  Result := Result + ']';
end;

function FailureName(const K: TGraphContradictionKind): String;
const NAMES: array[TGraphContradictionKind] of String =
  ('none','empty-domain','invalid-lock','adjacency','previous-pass',
   'required-support','final-validation','pass-dependency','entry-domain',
   'excluded-assignment','connectivity');
begin Result := NAMES[K]; end;

function DispositionName(const D: TGraphPassDisposition): String;
const NAMES: array[TGraphPassDisposition] of String =
  ('not-run','reused','cleared','copied','solved','failed');
begin Result := NAMES[D]; end;

function ValidationName(const K: TWfcMusicPassValidationIssueKind): String;
const NAMES: array[TWfcMusicPassValidationIssueKind] of String =
  ('none','composition','length','extent','boundary','latent-capture',
   'state-path','state-projection','caller-constraint','cell',
   'melody-continuation','rhythm-projection','harmony-projection','score',
   'signature','internal');
begin Result := NAMES[K]; end;

function TWfcMusicStudio.RunReportText: String;
const MUSIC_STATUS_NAMES: array[TWfcMusicPassStatus] of String =
  ('not-run','completed','solve-failed','capture-failed','validation-failed');
  CAPTURE_NAMES: array[TWfcSequenceGraphIssueKind] of String =
  ('none','graph-shape','model-identity','empty-cell','unknown-state',
   'state-index','boundary','start','end','transition');
var I: Integer; P: TGraphPassSolveReport; L: TWfcMusicPassLayer;
begin
  Result := MusicStudioStatusName(FStatus) + ' seed=' + UIntToStr(FSeed) +
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
    'music-validation=' + ValidationName(FReport.ValidationKind) +
    ' layer=' + WfcMusicPassLayerName(FReport.ValidationLayer) +
    ' cell=' + IntToStr(FReport.ValidationPosition) + #10 +
    'trace=' + UpperCase(IntToHex(FReport.TraceHash, 8)) +
    ' transcript=' + UpperCase(IntToHex(FReport.TranscriptHash, 8));
  if FReport.MusicStatus in [wmpsCaptureFailed,wmpsValidationFailed] then
    Result := Result + #10 + 'music-failed-layer=' +
      WfcMusicPassLayerName(FReport.MusicFailedLayer);
  if FReport.Validation.Issue.Detail <> '' then
    Result := Result + #10 + 'validation-detail=' + FReport.Validation.Issue.Detail;
  for L := Low(TWfcMusicPassLayer) to High(TWfcMusicPassLayer) do
    if FReport.Capture[L].Issue.Kind <> wsgikNone then
      Result := Result + #10 + 'capture=' + WfcMusicPassLayerName(L) +
        ' kind=' + CAPTURE_NAMES[FReport.Capture[L].Issue.Kind] +
        ' cell=' + IntToStr(FReport.Capture[L].Issue.Position) +
        ' related-cell=' + IntToStr(FReport.Capture[L].Issue.RelatedPosition);
  for I := 0 to High(FReport.Passes) do
  begin
    P := FReport.Passes[I];
    Result := Result + #10 + IntToStr(I) + ' ' +
      WfcMusicPassLayerName(TWfcMusicPassLayer(I)) +
      ' executed=' + LowerCase(BoolToStr(P.Executed, True)) +
      ' disposition=' + DispositionName(P.Disposition) +
      ' decisions=' + IntToStr(P.Decisions) +
      ' propagations=' + IntToStr(P.Propagations) +
      ' backtracks=' + IntToStr(P.Backtracks);
  end;
end;

function TWfcMusicStudio.SignatureText: String;
begin
  RequireCurrent;
  Result := WfcMusicCompositionSignatureHex(FComposition.Signature);
end;

function TWfcMusicStudio.CompositionText: String;
begin RequireCurrent; Result := EncodeWfcMusicPassesText(FComposition); end;

function TWfcMusicStudio.CopyScore: TWfcMusicScore;
begin RequireCurrent; Result := FComposition.CopyScore; end;

function TWfcMusicStudio.ScoreText: String;
var S: TWfcMusicScore;
begin
  S := CopyScore;
  try Result := EncodeWfcMusicText(S); finally S.Free; end;
end;

function TWfcMusicStudio.MidiBytes: TWfcMidiBytes;
var S: TWfcMusicScore;
begin
  S := CopyScore;
  try Result := EncodeWfcMusicMidi(S); finally S.Free; end;
end;

end.
