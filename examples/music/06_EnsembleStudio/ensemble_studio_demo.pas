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
unit ensemble_studio_demo;

{$mode delphi}{$H+}

interface

uses
  wfc,
  ensemble_studio_workbench;

function CreateSolvedEnsembleStudio(const ASeed: TGraphSeed;
  const ABars: Integer): TEnsembleStudio;
procedure PrintEnsembleStudioShowcase(const ASeed: TGraphSeed;
  const ABars: Integer);
function EnsembleStudioSelfTest: Integer;

implementation

uses
  SysUtils,
  wfc_model,
  wfc_music,
  wfc_music_sequence,
  wfc_music_ensemble,
  wfc_music_ensemble_passes,
  wfc_music_audio,
  wfc_music_midi,
  wfc_music_text,
  wfc_midi_smf;

procedure Need(const ACondition: Boolean; const AMessage: String;
  var AChecks: Integer);
begin
  Inc(AChecks);
  if not ACondition then
    raise EEnsembleStudio.Create('self-test: ' + AMessage);
end;

function SameBytes(const A, B: TWfcMidiBytes): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do
    if A[I] <> B[I] then Exit(False);
  Result := True;
end;

function SameTokens(const A, B: TWfcModelTokens): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do
    if A[I] <> B[I] then Exit(False);
  Result := True;
end;

function CreateSolvedEnsembleStudio(const ASeed: TGraphSeed;
  const ABars: Integer): TEnsembleStudio;
begin
  Result := TEnsembleStudio.Create(ASeed, ABars);
  try
    if not Result.Run(esaGenerate, DefaultEnsembleStudioOptions) then
      raise EEnsembleStudio.Create(Result.RunReportText);
  except
    Result.Free;
    raise;
  end;
end;

procedure PrintEnsembleStudioShowcase(const ASeed: TGraphSeed;
  const ABars: Integer);
var
  LBytes: TWfcModelTokens;
  LLayer: TWfcMusicEnsembleLayer;
  LMidi: TWfcMidiBytes;
  LMidiFailure: String;
  LStudio: TEnsembleStudio;
  I: Integer;
begin
  LStudio := CreateSolvedEnsembleStudio(ASeed, ABars);
  try
    WriteLn(LStudio.RunReportText);
    WriteLn('composition=', LStudio.SignatureText,
      ' score=', EnsembleStudioTextSignature(LStudio.ScoreText));
    LMidiFailure := '';
    try
      LMidi := LStudio.MidiBytes;
    except
      on E: EWfcMusicMidi do LMidiFailure := E.Message;
      on E: EWfcMidiSmf do LMidiFailure := E.Message;
    end;
    if LMidiFailure = '' then
      WriteLn('midi=', EnsembleStudioByteSignature(LMidi))
    else
      WriteLn('MIDI unavailable: ', LMidiFailure);
    for LLayer := Low(TWfcMusicEnsembleLayer) to
        High(TWfcMusicEnsembleLayer) do
    begin
      Write(WfcMusicEnsembleLayerName(LLayer), ':');
      LBytes := LStudio.CellTokens(LLayer);
      for I := 0 to High(LBytes) do
        Write(' ', EnsembleStudioTokenLabel(LLayer, LBytes[I]));
      WriteLn;
    end;
  finally
    LStudio.Free;
  end;
end;

function EnsembleStudioSelfTest: Integer;
var
  LBaseline: String;
  LBaselineEnsemble: TWfcModelTokens;
  LBaselineHarmony: TWfcModelTokens;
  LBaselineRhythm: TWfcModelTokens;
  LBytes: TWfcMidiBytes;
  LCells: TWfcModelTokens;
  LDecodedMidi: TWfcMidiFile;
  LDecodedScore: TWfcMusicScore;
  LFrames: TWfcMusicEnsembleFrames;
  LFrame: TWfcMusicEnsembleFrame;
  LFound: Boolean;
  LLocks: TEnsembleStudioLocks;
  LOptions: TEnsembleStudioOptions;
  LPreview: TWfcMusicAudioBytes;
  LPreviewFailure: String;
  LPreviewFrames: Integer;
  LRaised: Boolean;
  LReport: TEnsembleStudioReport;
  LStudio: TEnsembleStudio;
  LText: String;
  I: Integer;
begin
  Result := 0;
  Need(EnsembleStudioBarsToCellCount(1) = 8,
    'one bar contains eight cells', Result);
  Need(EnsembleStudioDurationSeconds(2) = 4,
    'two bars last four seconds', Result);
  LRaised := False;
  try
    EnsembleStudioBarsToCellCount(0);
  except
    on E: EEnsembleStudio do LRaised := True;
  end;
  Need(LRaised, 'zero bars are rejected', Result);
  for I := 0 to ENSEMBLE_STUDIO_CORPUS_COUNT - 1 do
  begin
    LFrames := EnsembleStudioCorpus(I);
    Need((Length(LFrames) = 16) and
      (Length(LFrames[0].Voices) = ENSEMBLE_STUDIO_VOICE_COUNT),
      'authored corpus shape ' + IntToStr(I), Result);
    Need((LFrames[0].Voices[0].Action = wmcaAttack) and
      (LFrames[1].Voices[0].Action = wmcaHold) and
      (Length(LFrames[0].Voices[1].Tones) = 3),
      'held bass and chord texture ' + IntToStr(I), Result);
  end;

  LStudio := TEnsembleStudio.Create(0, ENSEMBLE_STUDIO_DEFAULT_BARS);
  try
    Need((LStudio.Status = essIdle) and not LStudio.HasCurrent and
      not LStudio.HasBaseline, 'new session starts empty', Result);
    LOptions := DefaultEnsembleStudioOptions;
    Need(LStudio.Run(esaGenerate, LOptions),
      'default negotiated generation solves', Result);
    Need(LStudio.HasCurrent and LStudio.HasBaseline and
      (LStudio.CellCount = 16), 'baseline is current', Result);
    Need(LStudio.CurrentIsValid,
      'independent composition validation', Result);
    LBaseline := LStudio.SignatureText;
    Need(LBaseline = ENSEMBLE_STUDIO_BASELINE_COMPOSITION_SIGNATURE,
      'baseline composition signature', Result);
    LReport := LStudio.CopyReport;
    Need(UpperCase(IntToHex(LReport.TranscriptHash, 8)) =
      ENSEMBLE_STUDIO_BASELINE_TRANSCRIPT_SIGNATURE,
      'baseline negotiation transcript', Result);
    LFrames := LStudio.EnsembleFrames;
    Need((Length(LFrames) = 16) and
      (Length(LFrames[0].Voices) = 3), 'three public score voices', Result);
    Need(Length(LFrames[0].Voices[1].Tones) = 3,
      'chord accompaniment remains polyphonic', Result);

    LText := LStudio.ScoreText;
    Need(EnsembleStudioTextSignature(LText) =
      ENSEMBLE_STUDIO_BASELINE_SCORE_SIGNATURE,
      'baseline score signature', Result);
    LDecodedScore := DecodeWfcMusicText(LText);
    try
      Need((LDecodedScore.VoiceCount = 3) and
        (LDecodedScore.LengthTicks = 3840) and
        (EncodeWfcMusicText(LDecodedScore) = LText),
        'canonical score round trip', Result);
    finally
      LDecodedScore.Free;
    end;
    LBytes := LStudio.MidiBytes;
    Need((Length(LBytes) = ENSEMBLE_STUDIO_BASELINE_MIDI_BYTES) and
      (EnsembleStudioByteSignature(LBytes) =
       ENSEMBLE_STUDIO_BASELINE_MIDI_SIGNATURE),
      'baseline MIDI identity', Result);
    LDecodedMidi := DecodeWfcMidiFile(LBytes);
    Need(SameBytes(EncodeWfcMidiFile(LDecodedMidi), LBytes),
      'canonical MIDI round trip', Result);
    Need(LStudio.TryWavePreview(LPreview, LPreviewFrames,
      LPreviewFailure) and (LPreviewFrames = 176400) and
      (Length(LPreview) = ENSEMBLE_STUDIO_BASELINE_WAVE_BYTES) and
      (EnsembleStudioByteSignature(LPreview) =
       ENSEMBLE_STUDIO_BASELINE_WAVE_SIGNATURE),
      'four-second exact WAV preview', Result);

    LBaselineEnsemble := LStudio.CellTokens(wmelEnsemble);
    LBaselineHarmony := LStudio.CellTokens(wmelHarmony);
    LBaselineRhythm := LStudio.CellTokens(wmelRhythm);
    LCells := LStudio.PublicTokens(wmelEnsemble);
    LFound := False;
    for I := 0 to High(LCells) do
    begin
      LFrame := DecodeWfcMusicEnsembleFrame(LCells[I]);
      if (LCells[I] <> LBaselineEnsemble[0]) and
          WfcMusicEnsembleFrameCanStart(LFrame) and
          (EncodeWfcMusicRhythmFrame(
            ProjectWfcMusicEnsembleFrameToRhythm(LFrame)) =
           LBaselineRhythm[0]) and
          (EncodeWfcMusicPitchClassSet(
            ProjectWfcMusicEnsembleFrameToPitchClassSet(LFrame, 12)) =
           LBaselineHarmony[0]) then
      begin
        LFound := True;
        Break;
      end;
    end;
    Need(LFound, 'compatible alternate ensemble opening exists', Result);
    LStudio.SetLock(wmelEnsemble, 0, LCells[I]);
    Need(not LStudio.HasCurrent and LStudio.HasBaseline,
      'lock edit hides current output', Result);
    LLocks := LStudio.CopyLocks;
    Need((Length(LLocks) = 1) and
      (LLocks[0].Token = LCells[I]), 'lock is recorded exactly', Result);
    LOptions := DefaultEnsembleStudioOptions;
    Need(LStudio.Run(esaEnsemble, LOptions),
      'ensemble-only repair succeeds', Result);
    LReport := LStudio.CopyReport;
    Need((Length(LReport.ActivePassIndices) = 1) and
      (LReport.ActivePassIndices[0] = Ord(wmelEnsemble)),
      'selective repair scope is explicit', Result);
    Need((LStudio.CellTokens(wmelEnsemble)[0] = LCells[I]) and
      (LStudio.SignatureText <> LBaseline),
      'public lock survives repair', Result);
    Need(SameTokens(LStudio.CellTokens(wmelHarmony), LBaselineHarmony) and
      SameTokens(LStudio.CellTokens(wmelRhythm), LBaselineRhythm),
      'providers are reused by ensemble-only repair', Result);

    LCells := LStudio.PublicTokens(wmelEnsemble);
    LFound := False;
    for I := 0 to High(LCells) do
      if DecodeWfcMusicEnsembleFrame(LCells[I]).Voices[0].Action =
          wmcaRest then
      begin
        LFound := True;
        Break;
      end;
    Need(LFound, 'ensemble vocabulary contains a rest frame', Result);
    LStudio.SetLock(wmelEnsemble, 0, LCells[I]);
    LOptions := DefaultEnsembleStudioOptions;
    Need(not LStudio.Run(esaGenerate, LOptions),
      'illegal opening hold/rest choice is not published', Result);
    Need(not LStudio.HasCurrent and LStudio.HasBaseline and
      (Length(LStudio.CellTokens(wmelEnsemble)) = 0),
      'failed run exposes no stale public cells', Result);
    LStudio.ClearLocks;
    Need(LStudio.Run(esaGenerate, DefaultEnsembleStudioOptions) and
      (LStudio.SignatureText = LBaseline),
      'clearing failure recovers the exact baseline', Result);

    LCells := LStudio.CellTokens(wmelHarmony);
    LCells[0] := 'changed';
    Need(LStudio.CellTokens(wmelHarmony)[0] <> 'changed',
      'public cell arrays are detached', Result);
    LLocks := LStudio.CopyLocks;
    SetLength(LLocks, 1);
    LLocks[0].Token := 'changed';
    Need(Length(LStudio.CopyLocks) = 0,
      'lock arrays are detached', Result);
    LStudio.InvalidateCurrent;
    Need(not LStudio.HasCurrent and LStudio.HasBaseline,
      'pending option edit hides artifacts', Result);
    LRaised := False;
    try
      LText := LStudio.ScoreText;
    except
      on E: EEnsembleStudio do LRaised := True;
    end;
    Need(LRaised, 'dirty score cannot be exported', Result);

    LStudio.Reset(7, 1);
    Need((LStudio.Seed = 7) and (LStudio.Bars = 1) and
      not LStudio.HasBaseline and (Length(LStudio.CopyLocks) = 0),
      'new session clears baseline and locks', Result);
    Need(LStudio.Run(esaGenerate, DefaultEnsembleStudioOptions) and
      (LStudio.CellCount = 8), 'one-bar grid generates', Result);
  finally
    LStudio.Free;
  end;

  LStudio := TEnsembleStudio.Create(0, 31);
  try
    Need(LStudio.Run(esaGenerate, DefaultEnsembleStudioOptions),
      'user-defined 31-bar score generates', Result);
    Need(not LStudio.TryWavePreview(LPreview, LPreviewFrames,
      LPreviewFailure), 'preview adapter rejects over-60-second score', Result);
    Need(LStudio.HasCurrent and (LStudio.ScoreText <> '') and
      (Length(LStudio.MidiBytes) > 0) and (LPreviewFailure <> ''),
      'preview limit does not invalidate score or MIDI', Result);
  finally
    LStudio.Free;
  end;
end;

end.
