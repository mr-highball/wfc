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
program EnsembleStudio;

{$mode delphi}{$H+}

uses
  Classes,
  SysUtils,
  wfc,
  wfc_text_codec,
  wfc_music_audio,
  wfc_music_midi,
  wfc_midi_smf,
  ensemble_studio_workbench,
  ensemble_studio_demo;

type
  TEnsembleArguments = record
    SelfTest: Boolean;
    Seed: TGraphSeed;
    Bars: Integer;
    OutputDirectory: String;
  end;

procedure Usage;
begin
  WriteLn('EnsembleStudio [--seed UINT32] [--bars POSITIVE] [--output NEW-DIRECTORY]');
  WriteLn('EnsembleStudio --selftest');
  WriteLn('The default is seed 0 and two 4/4 bars (16 eighth-note cells).');
  WriteLn('Score and MIDI exports follow the requested grid. Short supported scores');
  WriteLn('also receive a deterministic mono PCM16 WAV preview. Existing paths are refused.');
end;

function ParsePositiveInteger(const AText, AName: String): Integer;
begin
  Result := WfcTextParseCanonicalInteger(AText, AName,
    'EnsembleStudio');
  if Result < 1 then
    raise EEnsembleStudio.Create(AName + ' must be positive');
end;

function ParseArguments: TEnsembleArguments;
var
  I: Integer;
  LOption: String;
begin
  Result := Default(TEnsembleArguments);
  Result.Bars := ENSEMBLE_STUDIO_DEFAULT_BARS;
  if (ParamCount = 1) and (ParamStr(1) = '--selftest') then
  begin
    Result.SelfTest := True;
    Exit;
  end;
  if (ParamCount = 1) and ((ParamStr(1) = '--help') or
      (ParamStr(1) = '-h')) then
  begin
    Usage;
    Halt(0);
  end;
  if (ParamCount = 1) and (ParamStr(1) = '--version') then
  begin
    WriteLn('EnsembleStudio ', ENSEMBLE_STUDIO_VERSION);
    Halt(0);
  end;
  I := 1;
  while I <= ParamCount do
  begin
    LOption := ParamStr(I);
    Inc(I);
    if I > ParamCount then
      raise EEnsembleStudio.Create('missing value for ' + LOption);
    if LOption = '--seed' then
      Result.Seed := WfcTextParseCanonicalCardinal(
        ParamStr(I), 'seed', 'EnsembleStudio')
    else if LOption = '--bars' then
      Result.Bars := ParsePositiveInteger(ParamStr(I), 'bars')
    else if LOption = '--output' then
      Result.OutputDirectory := ParamStr(I)
    else
      raise EEnsembleStudio.Create('unknown option: ' + LOption);
    Inc(I);
  end;
  EnsembleStudioBarsToCellCount(Result.Bars);
end;

function AsciiBytes(const AText: String): TWfcMusicAudioBytes;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AText));
  for I := 1 to Length(AText) do
  begin
    if Ord(AText[I]) > 127 then
      raise EEnsembleStudio.Create('artifact must be canonical ASCII');
    Result[I - 1] := Byte(Ord(AText[I]));
  end;
end;

procedure WriteArtifact(const ADirectory, AName: String;
  const ABytes: array of Byte);
var
  LPath: String;
  LStream: TFileStream;
begin
  LPath := IncludeTrailingPathDelimiter(ADirectory) + AName;
  if FileExists(LPath) or DirectoryExists(LPath) then
    raise EEnsembleStudio.Create('refusing to replace artifact ' + LPath);
  LStream := TFileStream.Create(LPath, fmCreate);
  try
    if Length(ABytes) > 0 then
      LStream.WriteBuffer(ABytes[0], Length(ABytes));
  finally
    LStream.Free;
  end;
end;

function TryMidiArtifact(const AStudio: TEnsembleStudio;
  out ABytes: TWfcMidiBytes; out AFailure: String): Boolean;
begin
  ABytes := nil;
  AFailure := '';
  try
    ABytes := AStudio.MidiBytes;
    Result := True;
  except
    on E: EWfcMusicMidi do
    begin
      ABytes := nil;
      AFailure := E.Message;
      Result := False;
    end;
    on E: EWfcMidiSmf do
    begin
      ABytes := nil;
      AFailure := E.Message;
      Result := False;
    end;
  end;
end;

procedure ExportStudio(const AStudio: TEnsembleStudio;
  const ADirectory: String);
var
  LFailure, LMidiFailure: String;
  LFrameCount: Integer;
  LMidi: TWfcMidiBytes;
  LScore: TWfcMusicAudioBytes;
  LWave: TWfcMusicAudioBytes;
  LWaveAvailable: Boolean;
begin
  if ADirectory = '' then Exit;
  if DirectoryExists(ADirectory) or FileExists(ADirectory) then
    raise EEnsembleStudio.Create('output directory must be a new path');
  LScore := AsciiBytes(AStudio.ScoreText);
  TryMidiArtifact(AStudio, LMidi, LMidiFailure);
  LWaveAvailable := AStudio.TryWavePreview(LWave, LFrameCount, LFailure);
  if not CreateDir(ADirectory) then
    raise EEnsembleStudio.Create('could not create output directory');
  WriteArtifact(ADirectory, 'score.wfcmusic', LScore);
  if LMidiFailure = '' then
    WriteArtifact(ADirectory, 'score.mid', LMidi)
  else
    WriteLn('MIDI unavailable: ', LMidiFailure);
  if LWaveAvailable then
  begin
    WriteArtifact(ADirectory, 'preview.wav', LWave);
    WriteLn('Preview: ', LFrameCount, ' frames; ', Length(LWave),
      ' bytes; signature=', EnsembleStudioByteSignature(LWave), '.');
  end
  else
    WriteLn('Preview unavailable: ', LFailure);
  WriteLn('Artifacts written to ', ADirectory);
end;

procedure Main;
var
  LArguments: TEnsembleArguments;
  LMidi: TWfcMidiBytes;
  LMidiFailure: String;
  LStudio: TEnsembleStudio;
begin
  LArguments := ParseArguments;
  if LArguments.SelfTest then
  begin
    WriteLn('Ensemble Studio checks: ', EnsembleStudioSelfTest);
    Exit;
  end;
  LStudio := CreateSolvedEnsembleStudio(
    LArguments.Seed, LArguments.Bars);
  try
    WriteLn(LStudio.RunReportText);
    WriteLn('composition=', LStudio.SignatureText,
      ' score=', EnsembleStudioTextSignature(LStudio.ScoreText));
    if TryMidiArtifact(LStudio, LMidi, LMidiFailure) then
      WriteLn('midi=', EnsembleStudioByteSignature(LMidi))
    else
      WriteLn('MIDI unavailable: ', LMidiFailure);
    ExportStudio(LStudio, LArguments.OutputDirectory);
  finally
    LStudio.Free;
  end;
end;

begin
  try
    Main;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'EnsembleStudio: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
