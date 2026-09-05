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
unit music_study_console;

{$mode delphi}{$H+}

interface

procedure RunSimpleMusicStudy(const ARiff: Boolean);

implementation

uses
  SysUtils, main, simpleriff, wfc, wfc_music, wfc_music_audio,
  wfc_music_audio_stream, wfc_music_midi, wfc_midi_smf, wfc_atomic_new_file;

type
  TStudyFileSink = class(TWfcMusicAudioByteSink)
  private
    FOutput: TWfcAtomicNewFile;
  public
    constructor Create(const AOutput: TWfcAtomicNewFile);
    procedure WriteBytes(const ABytes: array of Byte); override;
  end;

constructor TStudyFileSink.Create(const AOutput: TWfcAtomicNewFile);
begin
  inherited Create;
  FOutput := AOutput;
end;

procedure TStudyFileSink.WriteBytes(const ABytes: array of Byte);
begin
  FOutput.WriteBytes(ABytes);
end;

function UnsignedValue(const AText: String; const AMaximum: Int64): Int64;
var I, D: Integer;
begin
  Result := 0;
  if AText = '' then raise EConvertError.Create('numeric argument cannot be empty');
  for I := 1 to Length(AText) do
  begin
    if not (AText[I] in ['0'..'9']) then
      raise EConvertError.Create('numeric argument must contain decimal digits');
    D := Ord(AText[I]) - Ord('0');
    if Result > (AMaximum - D) div 10 then
      raise EConvertError.Create('numeric argument exceeds its exact range');
    Result := Result * 10 + D;
  end;
end;

function ReadSongs(const AText: String): TSimpleRiffSongs;
var I, Start: Integer; S: String;
begin
  Result := []; Start := 1;
  for I := 1 to Length(AText) + 1 do
    if (I > Length(AText)) or (AText[I] = ',') then
    begin
      S := Copy(AText, Start, I - Start); Start := I + 1;
      if S = 'mary' then Include(Result, srsMary)
      else if S = 'bridge' then Include(Result, srsBridge)
      else if S = 'hot-cross' then Include(Result, srsHotCross)
      else raise EConvertError.Create('songs must be mary,bridge,hot-cross');
    end;
end;

procedure RunSimpleMusicStudy(const ARiff: Boolean);
var
  Study: TSimpleMusic;
  Score: TWfcMusicScore;
  Output: TWfcAtomicNewFile;
  Sink: TStudyFileSink;
  Bytes: TWfcMidiBytes;
  Seed: TGraphSeed;
  Count, Tempo, I: Integer;
  Option, Value, Path, FormatName: String;
  Songs: TSimpleRiffSongs;
begin
  Count := 24; Tempo := 500000; Seed := 0;
  Path := ''; FormatName := ''; Songs := [srsMary, srsBridge, srsHotCross];
  I := 1;
  while I <= ParamCount do
  begin
    Option := ParamStr(I);
    if Option = '--help' then
    begin
      WriteLn('Options: --seed N --notes N --tempo-us N --wave NEW.wav | --midi NEW.mid');
      if ARiff then WriteLn('         --songs mary,bridge,hot-cross');
      WriteLn('Each generated note is one quarter note. Defaults: 24 notes, 120 BPM, seed 0.');
      WriteLn('No files are written without --wave or --midi; existing paths are never replaced.');
      Exit;
    end;
    Inc(I);
    if I > ParamCount then raise EConvertError.Create('option requires a value: ' + Option);
    Value := ParamStr(I); Inc(I);
    if Option = '--seed' then Seed := TGraphSeed(UnsignedValue(Value, 4294967295))
    else if Option = '--notes' then Count := Integer(UnsignedValue(Value, High(Integer)))
    else if Option = '--tempo-us' then Tempo := Integer(UnsignedValue(Value, High(Integer)))
    else if (Option = '--songs') and ARiff then Songs := ReadSongs(Value)
    else if (Option = '--wave') or (Option = '--midi') then
    begin
      if FormatName <> '' then raise EConvertError.Create('select exactly one output file per run');
      FormatName := Option; Path := Value;
      if Path = '' then raise EConvertError.Create('output path cannot be empty');
    end
    else raise EConvertError.Create('unknown option: ' + Option);
  end;
  if ARiff then
  begin
    Study := TSimpleRiff.Create;
    TSimpleRiff(Study).Songs := Songs;
  end
  else Study := TSimpleMusic.Create;
  try
    Score := Study.GenerateMusic(Count, Tempo, Seed);
    try
      WriteLn('seed=', Seed, ' notes=', Score.SpanCount, ' tempo-us=', Tempo);
      WriteLn('duration-us=', Int64(Count) * Tempo);
      if FormatName <> '' then
      begin
        Output := TWfcAtomicNewFile.Create(Path);
        try
          if FormatName = '--midi' then
          begin
            Bytes := EncodeWfcMusicMidi(Score);
            Output.WriteBytes(Bytes);
          end
          else
          begin
            Sink := TStudyFileSink.Create(Output);
            try RenderSimpleMusicWave(Score, Sink, DefaultWfcMusicAudioOptions);
            finally Sink.Free; end;
          end;
          Output.Publish;
          WriteLn('published=', Output.OutputPath, ' bytes=', Output.ByteCount);
        finally Output.Free; end;
      end;
    finally Score.Free; end;
  finally Study.Free; end;
end;

end.
