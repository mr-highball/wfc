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
program MusicStudioRender;

{$mode delphi}{$H+}

uses
  Classes, SysUtils, wfc, wfc_music, wfc_music_audio,
  wfc_music_audio_stream, wfc_music_arrangement, music_studio_arrangement
  {$IFDEF MSWINDOWS}, Windows{$ELSE}, BaseUnix{$ENDIF};

type
  TRenderFileSink = class(TWfcMusicAudioByteSink)
  private
    FHandle: THandle;
    FPath: String;
  public
    constructor Create(const AOutput: String);
    destructor Destroy; override;
    procedure WriteBytes(const ABytes: array of Byte); override;
    procedure FinishFile;
    property Path: String read FPath;
  end;

constructor TRenderFileSink.Create(const AOutput: String);
var
  I: Integer;
  LCandidate: String;
begin
  inherited Create;
  FHandle := THandle(-1);
  for I := 0 to 99 do
  begin
    LCandidate := AOutput + '.partial-' + IntToHex(GetTickCount64, 16) +
      '-' + IntToStr(I);
    {$IFDEF MSWINDOWS}
    FHandle := CreateFile(PChar(LCandidate), GENERIC_WRITE, 0, nil,
      CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
    {$ELSE}
    FHandle := fpOpen(PChar(LCandidate), O_WRONLY or O_CREAT or O_EXCL,
      S_IRUSR or S_IWUSR);
    {$ENDIF}
    if FHandle <> THandle(-1) then
    begin
      FPath := LCandidate;
      Exit;
    end;
    {$IFDEF MSWINDOWS}
    if (GetLastError <> ERROR_FILE_EXISTS) and
        (GetLastError <> ERROR_ALREADY_EXISTS) then
    {$ELSE}
    if fpGetErrno <> ESysEEXIST then
    {$ENDIF}
      raise EWriteError.Create('cannot create exclusive temporary output');
  end;
  raise EWriteError.Create('exclusive temporary output names are exhausted');
end;

destructor TRenderFileSink.Destroy;
begin
  if FHandle <> THandle(-1) then
    FileClose(FHandle);
  inherited Destroy;
end;

procedure TRenderFileSink.WriteBytes(const ABytes: array of Byte);
var
  LOffset, LWritten: Integer;
begin
  if FHandle = THandle(-1) then
    raise EWriteError.Create('output is already closed');
  LOffset := 0;
  while LOffset < Length(ABytes) do
  begin
    LWritten := FileWrite(FHandle, ABytes[LOffset], Length(ABytes) - LOffset);
    if LWritten <= 0 then
      raise EWriteError.Create('output write failed');
    Inc(LOffset, LWritten);
  end;
end;

procedure TRenderFileSink.FinishFile;
begin
  if FHandle = THandle(-1) then
    raise EWriteError.Create('output is already closed');
  if not FileFlush(FHandle) then
    raise EWriteError.Create('output flush failed');
  FileClose(FHandle);
  FHandle := THandle(-1);
end;

procedure PublishFile(const ATemporary, AOutput: String);
begin
  {$IFDEF MSWINDOWS}
  { MoveFile never replaces an existing destination, even if it appeared
    after the initial existence check. Both paths are siblings. }
  if not MoveFile(PChar(ATemporary), PChar(AOutput)) then
    raise EWriteError.Create('cannot publish output without replacing an existing path');
  {$ELSE}
  { link creates the destination atomically with no replacement. Unlike
    rename, this also preserves a destination that appeared during rendering. }
  if fpLink(PChar(ATemporary), PChar(AOutput)) <> 0 then
    raise EWriteError.Create('cannot publish output without replacing an existing path');
  if fpUnlink(PChar(ATemporary)) <> 0 then
    WriteLn(StdErr, 'Completed output published; temporary cleanup will be retried.');
  {$ENDIF}
end;

function ParseSeed(const AText: String): TGraphSeed;
var
  I: Integer;
  LDigit: Cardinal;
begin
  Result := 0;
  if AText = '' then
    raise Exception.Create('seed must be an unsigned decimal integer');
  for I := 1 to Length(AText) do
  begin
    if not (AText[I] in ['0'..'9']) then
      raise Exception.Create('seed must be an unsigned decimal integer');
    LDigit := Ord(AText[I]) - Ord('0');
    if Result > (High(Cardinal) - LDigit) div 10 then
      raise Exception.Create('seed exceeds UINT32');
    Result := Result * 10 + LDigit;
  end;
end;

procedure Render(const ASeconds: String; const ASeed: TGraphSeed;
  const AOutput: String);
var
  LConfig: TWfcMusicArrangementConfig;
  LSource: TMusicStudioSectionSource;
  LArrangement: TWfcMusicArrangement;
  LSection: TWfcMusicArrangementSection;
  LScore: TWfcMusicScore;
  LClip: TWfcMusicPcm16Clip;
  LSink: TRenderFileSink;
  LWave: TWfcMusicWaveStream;
  LStep: TWfcMusicArrangementStep;
  LFrames, LSeconds, LSections: TWfcMusicArrangementWide;
  LOutput, LTemporary: String;
  LOptions: TWfcMusicAudioOptions;
begin
  LConfig := MusicStudioArrangementConfig(ASeconds, ASeed);
  if (LConfig.RequestedTicks <= 0) or
      ((LConfig.RequestedTicks mod 1920) <> 0) then
    raise Exception.Create('arrangement duration must resolve to positive whole bars');
  LSeconds := LConfig.RequestedTicks div 960;
  if LSeconds > WFC_MUSIC_AUDIO_STREAM_MAX_FRAMES div 44100 then
    raise Exception.Create('duration exceeds the exact WAVE/RF64 file-size envelope');
  LFrames := LSeconds * 44100;
  if AOutput = '' then
    raise Exception.Create('--output must name a new WAVE file');
  LOutput := ExpandFileName(AOutput);
  if not DirectoryExists(ExtractFilePath(LOutput)) then
    raise Exception.Create('output parent directory must already exist');
  if FileExists(LOutput) or DirectoryExists(LOutput) then
    raise Exception.Create('refusing to overwrite existing output');
  LSource := nil;
  LArrangement := nil;
  LSink := nil;
  LWave := nil;
  LTemporary := '';
  try
    LSource := TMusicStudioSectionSource.Create(nil);
    LArrangement := TWfcMusicArrangement.Create(LConfig, LSource);
    LSink := TRenderFileSink.Create(LOutput);
    LTemporary := LSink.Path;
    LWave := TWfcMusicWaveStream.Create(LSink, 44100, LFrames);
    LOptions := DefaultWfcMusicAudioOptions;
    LSections := 0;
    WriteLn('Rendering ', LSeconds, ' seconds, seed ', ASeed,
      ', ', LFrames, ' frames at 44100 Hz.');
    repeat
      LSection := nil;
      LStep := LArrangement.Next(LSection);
      case LStep of
        wmaspProduced:
          begin
            if LSection = nil then
              raise Exception.Create('arrangement produced no owned section');
            LScore := nil;
            LClip := nil;
            try
              LScore := LSection.Composition.CopyScore;
              LClip := RenderWfcMusicAudio(LScore, LOptions);
              LWave.AppendClip(LClip);
              Inc(LSections);
              if (LSections mod 64) = 0 then
              begin
                WriteLn('Sections: ', LSections, '; frames: ', LWave.FrameCount,
                  '/', LWave.ExpectedFrames);
                Flush(Output);
              end;
            finally
              LClip.Free;
              LScore.Free;
              LSection.Free;
            end;
          end;
        wmaspCompleted: Break;
        wmaspCancelled:
          raise Exception.Create('arrangement was cancelled; output was not published');
        wmaspFailed:
          raise Exception.Create('arrangement failed: ' + LArrangement.Failure);
      end;
    until False;
    LWave.Finish;
    LSink.FinishFile;
    PublishFile(LTemporary, LOutput);
    WriteLn('Wrote ', LOutput);
    WriteLn('Sections: ', LSections, '; frames: ', LWave.FrameCount,
      '; RF64: ', BoolToStr(LWave.IsRF64, True));
  finally
    LWave.Free;
    LSink.Free;
    LArrangement.Free;
    LSource.Free;
    { Only the exact exclusively-created sibling is eligible for cleanup. }
    if (LTemporary <> '') and FileExists(LTemporary) then
      if not SysUtils.DeleteFile(LTemporary) then
        WriteLn(StdErr, 'Temporary output could not be removed: ', LTemporary);
  end;
end;

procedure Usage;
begin
  WriteLn('MusicStudioRender --seconds DURATION --output NEW-WAVE-PATH [--seed UINT32]');
  WriteLn('Duration: positive decimal seconds, at most three fractional digits.');
  WriteLn('Duration rounds up to whole 4/4 bars at 120 BPM (two seconds per bar).');
  WriteLn('Seed defaults to 0. Existing output is never replaced.');
  WriteLn('Audio is streamed section by section; RIFF or RF64 is selected by size.');
  WriteLn('There is no song-length policy cap; exact arithmetic and file capacity apply.');
end;

procedure Main;
var
  LSeconds, LOutput, LOption, LValue: String;
  LSeed: TGraphSeed;
  I: Integer;
  LSeenSeconds, LSeenOutput, LSeenSeed: Boolean;
begin
  if (ParamCount = 1) and (ParamStr(1) = '--version') then
  begin
    WriteLn('MusicStudioRender 1');
    Exit;
  end;
  if (ParamCount = 1) and ((ParamStr(1) = '--help') or (ParamStr(1) = '-h')) then
  begin
    Usage;
    Exit;
  end;
  LSeconds := '';
  LOutput := '';
  LSeed := 0;
  LSeenSeconds := False;
  LSeenOutput := False;
  LSeenSeed := False;
  I := 1;
  while I <= ParamCount do
  begin
    LOption := ParamStr(I);
    Inc(I);
    if I > ParamCount then
      raise Exception.Create('missing value for ' + LOption);
    LValue := ParamStr(I);
    if LOption = '--seconds' then
    begin
      if LSeenSeconds then raise Exception.Create('duplicate --seconds');
      LSeenSeconds := True;
      LSeconds := LValue;
    end
    else if LOption = '--output' then
    begin
      if LSeenOutput then raise Exception.Create('duplicate --output');
      LSeenOutput := True;
      LOutput := LValue;
    end
    else if LOption = '--seed' then
    begin
      if LSeenSeed then raise Exception.Create('duplicate --seed');
      LSeenSeed := True;
      LSeed := ParseSeed(LValue);
    end
    else
      raise Exception.Create('unknown option: ' + LOption);
    Inc(I);
  end;
  if not LSeenSeconds or not LSeenOutput then
    raise Exception.Create('--seconds and --output are required; see --help');
  Render(LSeconds, LSeed, LOutput);
end;

begin
  try
    Main;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'MusicStudioRender: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
