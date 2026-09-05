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
unit music_studio_demo;
{$mode delphi}{$H+}

interface
procedure RunMusicStudioDemo;

implementation

uses
  SysUtils, wfc, wfc_model, wfc_music, wfc_music_passes, wfc_music_audio,
  wfc_text_codec, music_studio_workbench, Classes;



procedure Require(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise EMusicStudio.Create(AMessage);
end;

function AsciiBytes(const AText: String): TWfcMusicAudioBytes;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AText));
  for I := 1 to Length(AText) do
  begin
    Require(Ord(AText[I]) <= 127, 'artifact must be canonical ASCII');
    Result[I - 1] := Byte(Ord(AText[I]));
  end;
end;

procedure CreateOutputDirectory(const APath: String);
begin
  Require(APath <> '', 'output directory cannot be empty');
  Require(not DirectoryExists(APath) and not FileExists(APath),
    'output directory must not already exist');
  Require(CreateDir(APath), 'could not create new output directory');
end;

procedure WriteArtifact(const ADirectory, AName: String;
  const ABytes: array of Byte);
var
  Path: String;
  Stream: TFileStream;
begin
  Path := IncludeTrailingPathDelimiter(ADirectory) + AName;
  Require(not FileExists(Path), 'refusing to overwrite artifact ' + Path);
  Stream := TFileStream.Create(Path, fmCreate);
  try
    if Length(ABytes) > 0 then Stream.WriteBuffer(ABytes[0], Length(ABytes));
  finally Stream.Free; end;
end;

procedure InspectAndExport(const W: TWfcMusicStudio; const ADirectory: String);
var
  Score: TWfcMusicScore;
  Clip: TWfcMusicPcm16Clip;
  Wave: TWfcMusicAudioBytes;
  Tokens: TWfcModelTokens;
  Layer: TWfcMusicPassLayer;
  I: Integer;
begin
  Require(W.HasCurrent, 'cannot inspect an unsuccessful attempt');
  Score := W.CopyScore;
  try
    Clip := RenderWfcMusicAudio(Score, DefaultWfcMusicAudioOptions);
    try
      Wave := EncodeWfcMusicWave(Clip);
      Require((Clip.SampleRate=44100) and (Clip.FrameCount=176400) and
        (Length(Wave)=352844), 'default four-second audio extent changed');
      if W.SignatureText='216F6EBB' then
        Require((MusicStudioTextSignature(W.ScoreText)='4167E7E5') and
          (MusicStudioByteSignature(W.MidiBytes)='86E4DCA3') and
          (MusicStudioByteSignature(Wave)='64679FF8'),
          'baseline score/MIDI/WAVE identity changed');
      if W.SignatureText='1C1075DB' then
        Require((MusicStudioTextSignature(W.ScoreText)='3690AE9B') and
          (MusicStudioByteSignature(W.MidiBytes)='93A9B159') and
          (MusicStudioByteSignature(Wave)='73A8591A'),
          'repaired score/MIDI/WAVE identity changed');
      WriteLn('composition=',W.SignatureText,
        ' score=',MusicStudioTextSignature(W.ScoreText),
        ' midi=',MusicStudioByteSignature(W.MidiBytes),
        ' wave=',MusicStudioByteSignature(Wave));
      WriteLn('audio-version=',WFC_MUSIC_AUDIO_VERSION,
        ' sample-rate=',Clip.SampleRate,' frames=',Clip.FrameCount,
        ' wave-bytes=',Length(Wave));
      for Layer := Low(TWfcMusicPassLayer) to High(TWfcMusicPassLayer) do
      begin
        Write(WfcMusicPassLayerName(Layer),':');
        Tokens := W.CellTokens(Layer);
        for I := 0 to High(Tokens) do Write(' ',Tokens[I]);
        WriteLn;
      end;
      if ADirectory <> '' then
      begin
        CreateOutputDirectory(ADirectory);
        WriteArtifact(ADirectory,'composition.wfcmusicpass',AsciiBytes(W.CompositionText));
        WriteArtifact(ADirectory,'score.wfcmusic',AsciiBytes(W.ScoreText));
        WriteArtifact(ADirectory,'score.mid',W.MidiBytes);
        WriteArtifact(ADirectory,'preview.wav',Wave);
        WriteLn('Artifacts written to ',ADirectory);
      end;
    finally Clip.Free; end;
  finally Score.Free; end;
end;

procedure SelfTest;
var W: TWfcMusicStudio; O: TWfcMusicStudioOptions; B, R: TWfcModelTokens; I: Integer;
begin
  W := TWfcMusicStudio.Create(0);
  try
    O := DefaultMusicStudioOptions;
    Require(W.Run(msaGenerate,O), 'baseline solve failed');
    Require(W.SignatureText='216F6EBB', 'baseline identity changed');
    B := W.CellTokens(wmplMelody); R := W.CellTokens(wmplRhythm);
    InspectAndExport(W,'');
    W.LockOpeningMotif(2);
    W.SetLock(wmplMelody,2,'wm1:a:67:96');
    O.Negotiated:=False; O.MaxPassBacktracks:=0;
    Require(not W.Run(msaHarmony,O) and not W.HasCurrent,
      'ordinary repair did not expose its contradiction');
    O:=DefaultMusicStudioOptions;
    Require(W.Run(msaHarmony,O), 'negotiated repair failed');
    Require((W.SignatureText='1C1075DB') and
      (W.CopyReport.PassBacktracks=1), 'repair identity changed');
    for I:=0 to High(R) do Require(W.CellTokens(wmplRhythm)[I]=R[I],
      'clean rhythm changed during repair');
    for I:=0 to 1 do Require(W.CellTokens(wmplMelody)[I]=B[I],
      'opening motif changed during repair');
    InspectAndExport(W,'');
    W.ClearLocks;
    Require(W.Run(msaGenerate,O) and (W.SignatureText='216F6EBB'),
      'exact recovery failed');
    WriteLn('Self-check: passed');
  finally W.Free; end;
end;

procedure RunMusicStudioDemo;
var W: TWfcMusicStudio; Seed: TGraphSeed; Directory: String;
begin
  if (ParamCount=1) and (ParamStr(1)='--selftest') then
  begin SelfTest; Exit; end;
  Require(ParamCount<=2,
    'usage: MusicStudio [decimal seed] [new-output-directory] | --selftest');
  Seed:=0; Directory:='';
  if ParamCount>=1 then Seed:=WfcTextParseCanonicalCardinal(ParamStr(1),'seed','MusicStudio');
  if ParamCount=2 then Directory:=ParamStr(2);
  W:=TWfcMusicStudio.Create(Seed);
  try
    if not W.Run(msaGenerate,DefaultMusicStudioOptions) then
      raise EMusicStudio.Create(W.RunReportText);
    WriteLn(W.RunReportText);
    InspectAndExport(W,Directory);
  finally W.Free; end;
end;

end.
