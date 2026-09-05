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
unit main;

{$mode delphi}{$H+}

interface

uses
  SysUtils, wfc, wfc_music, wfc_music_audio, wfc_music_audio_stream;

type
  { The original fluent A-major constraints, without a presentation backend.
    Descendants override InitWFC to explore another authored note grammar. }
  TSimpleMusic = class
  strict private
    FGraph: TGraph;
    FNoteCount: Integer;
  protected
    procedure InitWFC(const AGraph: TGraph); virtual;
    function NotePitch(const ANote: String): Integer; virtual;
    property NoteCount: Integer read FNoteCount;
  public
    constructor Create;
    destructor Destroy; override;
    function GenerateMusic(const ACount, ATempoMicroseconds: Integer;
      const ASeed: TGraphSeed): TWfcMusicScore;
  end;

{ Bounded-memory PCM/RF64 output for the study's single-voice, constant-tempo,
  twelve-tone note scores; other score shapes fail before writing sink bytes.
  The complete note graph/score is still resident. No preview-duration or
  arbitrary note-count cap is applied. }
procedure RenderSimpleMusicWave(const AScore: TWfcMusicScore;
  const ASink: TWfcMusicAudioByteSink;
  const AOptions: TWfcMusicAudioOptions);

implementation

uses
  wfc_music_sequence, wfc_music_ensemble, wfc_music_ensemble_audio;

constructor TSimpleMusic.Create;
begin
  inherited Create;
  FGraph := TGraph.Create;
end;

destructor TSimpleMusic.Destroy;
begin
  FGraph.Free;
  inherited Destroy;
end;

procedure TSimpleMusic.InitWFC(const AGraph: TGraph);
begin
  AGraph.Reshape(NoteCount, 1, 1);
  AGraph.WrapNeighbors := False;
  AGraph.AddValue('A').NewRule([gdEast], 'B').NewRule([gdWest], 'A+');
  AGraph.AddValue('B').NewRule([gdEast], 'C#');
  AGraph.AddValue('C#').NewRule([gdEast], 'D');
  AGraph.AddValue('D').NewRule([gdEast], 'E');
  AGraph.AddValue('E').NewRule([gdEast], 'F#');
  AGraph.AddValue('F#').NewRule([gdEast], 'G#');
  AGraph.AddValue('G#').NewRule([gdEast], 'A+');
  AGraph.AddValue('A+').NewRule([gdEast], 'A');
end;

function TSimpleMusic.NotePitch(const ANote: String): Integer;
const Names: array[0..11] of String =
  ('A','A#','B','C','C#','D','D#','E','F','F#','G','G#');
var I, LOctaves: Integer; LName: String;
begin
  LName := ANote; LOctaves := 0;
  while (Length(LName) > 0) and (LName[Length(LName)] = '+') do
  begin
    Inc(LOctaves); Delete(LName, Length(LName), 1);
    if LOctaves > 5 then raise ERangeError.Create('note exceeds MIDI pitch range');
  end;
  for I := 0 to High(Names) do
    if LName = Names[I] then
    begin
      //The original study starts its authored octave at A = 220 Hz (MIDI 57).
      Result := 57 + I + 12 * LOctaves;
      if Result > 127 then raise ERangeError.Create('note exceeds MIDI pitch range');
      Exit;
    end;
  raise EConvertError.Create('unknown authored note: ' + ANote);
end;

function TSimpleMusic.GenerateMusic(const ACount, ATempoMicroseconds: Integer;
  const ASeed: TGraphSeed): TWfcMusicScore;
var
  I: Integer;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
  LMeters: TWfcMusicMeterChanges;
  LTempos: TWfcMusicTempoChanges;
  LSpans: TWfcMusicSpanEvents;
  LTones: TWfcMusicTones;
begin
  Result := nil;
  if ACount < 1 then raise ERangeError.Create('note count must be positive');
  if ATempoMicroseconds < 1 then
    raise ERangeError.Create('microseconds per quarter must be positive');
  FNoteCount := ACount;
  FGraph.Reset;
  FGraph.Seed := ASeed;
  InitWFC(FGraph);
  LOptions := DefaultGraphSolveOptions;
  if not FGraph.TrySolve(LOptions, LReport) then
    raise EWfcMusic.CreateFmt('note constraints failed in pass %d',
      [LReport.FailedPassIndex]);
  SetLength(LTracks, 1); LTracks[0] := MakeWfcMusicTrack('study', 'Authored note study');
  SetLength(LVoices, 1); LVoices[0] := MakeWfcMusicVoice(0, 'melody');
  //One quarter per measure permits every positive requested note count.
  SetLength(LMeters, 1); LMeters[0] := MakeWfcMusicMeterChange(0, 1, 4);
  SetLength(LTempos, 1); LTempos[0] := MakeWfcMusicTempoChange(0, ATempoMicroseconds);
  SetLength(LSpans, ACount); SetLength(LTones, 1);
  for I := 0 to ACount - 1 do
  begin
    LTones[0] := MakeWfcMusicTone(NotePitch(FGraph.Entry[I, 0, 0].Value), 96);
    LSpans[I] := MakeWfcMusicSound(0, I, 1, LTones);
  end;
  //One exact tick per quarter avoids multiplying the requested note count.
  Result := TWfcMusicScore.Create(1, 12, ACount, LTracks, LVoices,
    LMeters, LTempos, LSpans);
end;

procedure RenderSimpleMusicWave(const AScore: TWfcMusicScore;
  const ASink: TWfcMusicAudioByteSink;
  const AOptions: TWfcMusicAudioOptions);
var
  I: Integer;
  LClock: TWfcMusicEnsembleAudioClock;
  LRenderer: TWfcMusicEnsembleAudioRenderer;
  LWave: TWfcMusicWaveStream;
  LCapacities: TWfcMusicEnsembleAudioVoiceCapacities;
  LFrame: TWfcMusicEnsembleFrame;
  LSpan: TWfcMusicSpanEvent;
  LSamples: TWfcMusicPcm16Samples;
begin
  if not Assigned(AScore) or not Assigned(ASink) then
    raise EArgumentNilException.Create('score and WAVE sink must be assigned');
  if (AScore.VoiceCount <> 1) or (AScore.TempoCount <> 1) or
    (AScore.StepsPerOctave <> 12) then
    raise EArgumentException.Create('simple music renderer needs one voice, one tempo and twelve-tone pitches');
  for I := 0 to AScore.SpanCount - 1 do
  begin
    LSpan := AScore.SpanAt(I);
    if LSpan.Kind <> wmskNote then
      raise EArgumentException.Create('simple music renderer needs note-only spans');
  end;
  LClock := Default(TWfcMusicEnsembleAudioClock);
  AdvanceWfcMusicEnsembleAudioClock(LClock, AScore.LengthTicks,
    AScore.TempoAt(0).MicrosecondsPerQuarter, AScore.TicksPerQuarter, AOptions.SampleRate);
  SetLength(LCapacities, 1); LCapacities[0] := 1;
  SetLength(LFrame.Voices, 1);
  LRenderer := TWfcMusicEnsembleAudioRenderer.Create(AOptions,
    AScore.TicksPerQuarter, LCapacities);
  try
    LWave := TWfcMusicWaveStream.Create(ASink, AOptions.SampleRate, LClock.FrameCount);
    try
      for I := 0 to AScore.SpanCount - 1 do
      begin
        LSpan := AScore.SpanAt(I);
        LFrame.Voices[0] := MakeWfcMusicVoiceCell(wmcaAttack, LSpan.Tones);
        LRenderer.AdmitFrame(LFrame, LSpan.DurationTicks,
          AScore.TempoAt(0).MicrosecondsPerQuarter);
        while LRenderer.ReadSamples(WFC_MUSIC_ENSEMBLE_AUDIO_BLOCK_FRAMES, LSamples) do
          LWave.AppendSamples(LSamples);
      end;
      LRenderer.EndInput;
      while LRenderer.ReadSamples(WFC_MUSIC_ENSEMBLE_AUDIO_BLOCK_FRAMES, LSamples) do
        LWave.AppendSamples(LSamples);
      LWave.Finish;
    finally LWave.Free; end;
  finally LRenderer.Free; end;
end;

end.

