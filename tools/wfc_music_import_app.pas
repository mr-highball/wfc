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
unit wfc_music_import_app;

{$mode delphi}{$H+}

interface

uses
  SysUtils, wfc_midi_smf, wfc_music_midi_import, wfc_music_training,
  wfc_training;

const
  WFC_MUSIC_IMPORT_CLI_VERSION = 1;
  WFC_MUSIC_IMPORT_MAX_INPUT_LENGTH = 16 * 1024 * 1024;
  WFC_MUSIC_IMPORT_EXIT_SUCCESS = 0;
  WFC_MUSIC_IMPORT_EXIT_INVALID_INPUT = 1;
  WFC_MUSIC_IMPORT_EXIT_USAGE = 2;
  WFC_MUSIC_IMPORT_EXIT_IO = 3;
  WFC_MUSIC_IMPORT_EXIT_INTERNAL = 70;

type
  TWfcMusicImportArguments = array of String;
  TWfcMusicImportCommandKind = (wmickHelp, wmickVersion, wmickImport);
  TWfcMusicImportOutputMode = (wmiomScore, wmiomReport,
    wmiomMelody, wmiomRhythm, wmiomHarmony);
  TWfcMusicImportFailureKind = (wmifkInvalidInput, wmifkUsage,
    wmifkIo, wmifkInternal);
  TWfcMusicImportCommand = record
    Kind: TWfcMusicImportCommandKind;
    OutputMode: TWfcMusicImportOutputMode;
    InputPath: String;
    ImportOptions: TWfcMusicMidiImportOptions;
    QuantumTicks, Order: Integer;
    Metadata: TWfcTrainingMetadata;
    Selections: TWfcMusicTrainingSelections;
  end;

function WfcMusicImportParseCommand(const AArguments: TWfcMusicImportArguments;
  out ACommand: TWfcMusicImportCommand; out AError: String): Boolean;
function WfcMusicImportExecuteBytes(const ACommand: TWfcMusicImportCommand;
  const AInput: TWfcMidiBytes; out AStandardOutput,
  AStandardError: String): Integer;
{ Thin hosts read binary input into an 8-bit/Latin-1 string, never UTF-8.
  Browser callers should use ExecuteBytes with FileReader ArrayBuffer bytes. }
function WfcMusicImportExecuteBinaryString(const ACommand: TWfcMusicImportCommand;
  const AInput: String; out AStandardOutput, AStandardError: String): Integer;
function WfcMusicImportHelpText: String;
function WfcMusicImportVersionText: String;
function WfcMusicImportFormatFailure(const AKind: TWfcMusicImportFailureKind;
  const AMessage: String): String;

implementation

uses
  wfc_model, wfc_music, wfc_music_text, wfc_training_text, wfc_text_codec;

type
  EWfcMusicImportUsage = class(Exception);

procedure Usage(const AMessage: String);
begin
  raise EWfcMusicImportUsage.Create(AMessage);
end;

function WfcMusicImportHelpText: String;
begin
  Result :=
    'Usage:'#10 +
    '  wfc_music_import [--score | --report] [POLICY] [--] INPUT'#10 +
    '  wfc_music_import (--melody | --rhythm | --harmony) [POLICY]'#10 +
    '    --quantum TICKS --order N --name NAME --license ID --source TEXT'#10 +
    '    --sample NAME,VOICE,START,LENGTH [--sample ...] [--] INPUT'#10 +
    '  wfc_music_import --help | --version'#10 + #10 +
    'INPUT is an SMF format-0/1 PPQ file, or - for binary standard input.'#10 +
    'Default output: canonical score. --report: deterministic import receipt.'#10 +
    'Training modes emit wfclearn=1; feed it to wfc_learn or Training Studio.'#10 +
    'POLICY: --ignore-performance opts into reported event omission;'#10 +
    '        --require-measure rejects incomplete final bars instead of padding.'#10 +
    'Names, license and source use canonical percent-encoded UTF-8 tokens.'#10 +
    'All options precede INPUT. Sample voice indices are zero-based.'#10 +
    'No quantization, cut notes, guessed licenses, file writes or network I/O.'#10;
end;

function WfcMusicImportVersionText: String;
begin
  Result := 'wfc_music_import 1 (MIDI import=1; music training=1)'#10;
end;

function WfcMusicImportFormatFailure(const AKind: TWfcMusicImportFailureKind;
  const AMessage: String): String;
var I: Integer; LLabel, LMessage: String;
begin
  LLabel := 'internal error';
  case AKind of
    wmifkInvalidInput: LLabel := 'invalid input';
    wmifkUsage: LLabel := 'usage error';
    wmifkIo: LLabel := 'I/O error';
    wmifkInternal: LLabel := 'internal error';
  end;
  LMessage := AMessage;
  if LMessage = '' then LMessage := 'unspecified failure';
  for I := 1 to Length(LMessage) do
    if Ord(LMessage[I]) < 32 then LMessage[I] := ' ';
  Result := 'wfc_music_import: ' + LLabel + ': ' + LMessage + #10;
end;

function InitialCommand: TWfcMusicImportCommand;
begin
  Result.Kind := wmickImport;
  Result.OutputMode := wmiomScore;
  Result.InputPath := '';
  Result.ImportOptions := DefaultWfcMusicMidiImportOptions;
  Result.QuantumTicks := 0;
  Result.Order := 0;
  Result.Metadata := MakeWfcTrainingMetadata('', '', '');
  Result.Selections := nil;
end;

function ParseSample(const AValue: String): TWfcMusicTrainingSelection;
var I, LPosition, LStart: Integer; LFields: array[0..3] of String;
begin
  LPosition := 1;
  for I := 0 to 3 do
  begin
    LStart := LPosition;
    while (LPosition <= Length(AValue)) and (AValue[LPosition] <> ',') do
      Inc(LPosition);
    LFields[I] := Copy(AValue, LStart, LPosition - LStart);
    if (I < 3) and (LPosition > Length(AValue)) then
      Usage('--sample requires NAME,VOICE,START,LENGTH');
    if (I = 3) and (LPosition <= Length(AValue)) then
      Usage('--sample has too many fields');
    Inc(LPosition);
  end;
  Result := MakeWfcMusicTrainingSelection(
    WfcTextDecodeToken(LFields[0], 'music import sample'),
    WfcTextParseCanonicalInteger(LFields[1], 'voice', 'music import'),
    WfcTextParseCanonicalInteger(LFields[2], 'start', 'music import'),
    WfcTextParseCanonicalInteger(LFields[3], 'length', 'music import'));
end;

function WfcMusicImportParseCommand(const AArguments: TWfcMusicImportArguments;
  out ACommand: TWfcMusicImportCommand; out AError: String): Boolean;
var
  C: TWfcMusicImportCommand;
  I, J, N: Integer;
  LArgument, LValue: String;
  LSeen: array[0..11] of Boolean;
  LEnd, LTraining: Boolean;
begin
  ACommand := InitialCommand;
  C := InitialCommand;
  AError := '';
  Result := False;
  for I := 0 to High(LSeen) do LSeen[I] := False;
  LEnd := False;
  try
    if (Length(AArguments) = 1) and (AArguments[0] = '--help') then
    begin C.Kind := wmickHelp; ACommand := C; Exit(True); end;
    if (Length(AArguments) = 1) and (AArguments[0] = '--version') then
    begin C.Kind := wmickVersion; ACommand := C; Exit(True); end;
    I := 0;
    while I < Length(AArguments) do
    begin
      LArgument := AArguments[I];
      if C.InputPath <> '' then Usage('exactly one INPUT; options must precede it');
      if not LEnd and (LArgument = '--') then
      begin LEnd := True; Inc(I); Continue; end;
      if not LEnd and (LArgument <> '-') and
          (LArgument <> '') and (LArgument[1] = '-') then
      begin
        J := -1;
        if LArgument = '--score' then J := 0
        else if LArgument = '--report' then J := 0
        else if LArgument = '--melody' then J := 0
        else if LArgument = '--rhythm' then J := 0
        else if LArgument = '--harmony' then J := 0
        else if LArgument = '--ignore-performance' then J := 1
        else if LArgument = '--require-measure' then J := 2
        else if LArgument = '--quantum' then J := 3
        else if LArgument = '--order' then J := 4
        else if LArgument = '--name' then J := 5
        else if LArgument = '--license' then J := 6
        else if LArgument = '--source' then J := 7
        else if LArgument = '--sample' then J := 8;
        if J < 0 then Usage('unknown option ' + LArgument);
        if (J <> 8) and LSeen[J] then Usage('duplicate or conflicting option ' + LArgument);
        LSeen[J] := True;
        LValue := '';
        if J >= 3 then
        begin
          Inc(I);
          if I >= Length(AArguments) then Usage('missing value for ' + LArgument);
          LValue := AArguments[I];
          if Length(LValue) > WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH then
            Usage('option value exceeds the training token length limit');
        end;
        case J of
          0:
            if LArgument = '--score' then C.OutputMode := wmiomScore
            else if LArgument = '--report' then C.OutputMode := wmiomReport
            else if LArgument = '--melody' then C.OutputMode := wmiomMelody
            else if LArgument = '--rhythm' then C.OutputMode := wmiomRhythm
            else C.OutputMode := wmiomHarmony;
          1: C.ImportOptions.UnsupportedEvents := wmmupIgnoreAndReport;
          2: C.ImportOptions.EndPolicy := wmmepRequireMeasure;
          3: C.QuantumTicks := WfcTextParseCanonicalInteger(LValue, 'quantum', 'music import');
          4: C.Order := WfcTextParseCanonicalInteger(LValue, 'order', 'music import');
          5: C.Metadata.Name := WfcTextDecodeToken(LValue, 'music import name');
          6: C.Metadata.LicenseIdentifier := WfcTextDecodeToken(LValue, 'music import license');
          7: C.Metadata.SourceDescription := WfcTextDecodeToken(LValue, 'music import source');
          8:
            begin
              N := Length(C.Selections);
              if N >= WFC_TRAINING_MAX_SAMPLE_COUNT then Usage('too many samples');
              SetLength(C.Selections, N + 1);
              C.Selections[N] := ParseSample(LValue);
            end;
        end;
      end
      else
      begin
        if LArgument = '' then Usage('INPUT cannot be empty');
        C.InputPath := LArgument;
      end;
      Inc(I);
    end;
    if C.InputPath = '' then Usage('one INPUT is required');
    LTraining := C.OutputMode in [wmiomMelody, wmiomRhythm, wmiomHarmony];
    if LTraining then
    begin
      for I := 3 to 8 do
        if not LSeen[I] then Usage('training requires quantum, order, name, license, source and samples');
      if (C.QuantumTicks < 1) or (C.Order < 1) or
          (C.Order > WFC_TRAINING_MAX_ORDER) then Usage('quantum must be positive; order must be 1..64');
      if (C.Metadata.Name = '') or (C.Metadata.LicenseIdentifier = '') or
          (C.Metadata.SourceDescription = '') then Usage('metadata must be explicit and nonempty');
    end
    else
      for I := 3 to 8 do
        if LSeen[I] then Usage('training options require a training output mode');
    ACommand := C;
    Result := True;
  except
    on E: EWfcMusicImportUsage do AError := E.Message;
    on E: EConvertError do AError := E.Message;
    on E: EWfcMusic do AError := E.Message;
  end;
end;

function SourceFingerprint(const ABytes: TWfcMidiBytes): String;
{$PUSH}{$Q-}
var I: Integer; H, V: Cardinal;
begin
  H := Cardinal(2166136261);
  for I := 0 to High(ABytes) do
  begin
    V := H xor Cardinal(ABytes[I]);
    H := (V + (V shl 1) + (V shl 4) + (V shl 7) +
      (V shl 8) + (V shl 24)) and Cardinal($FFFFFFFF);
  end;
  Result := IntToHex(H, 8);
end;
{$POP}

function ImportReceipt(const C: TWfcMusicImportCommand;
  const B: TWfcMidiBytes; const R: TWfcMusicMidiImportReport): String;
var I: Integer;
  procedure Field(const K: String; const V: Integer);
  begin Result := Result + K + '=' + IntToStr(V) + #10; end;
begin
  Result := 'wfcmidiimport=1'#10;
  Result := Result + 'source-fnv1a32=' + SourceFingerprint(B) + #10;
  Field('source-bytes', Length(B));
  Field('unsupported-policy', Ord(C.ImportOptions.UnsupportedEvents));
  Field('end-policy', Ord(C.ImportOptions.EndPolicy));
  Field('format', R.SourceFormat);
  Field('tracks', R.SourceTrackCount);
  Field('events-including-eot', R.SourceEventCount);
  Field('notes', R.NoteCount);
  Field('source-ticks', R.SourceLengthTicks);
  Field('score-ticks', R.ScoreLengthTicks);
  Field('padding-ticks', R.PaddingTicks);
  Field('ignored-channel', R.IgnoredChannelEvents);
  Field('ignored-system', R.IgnoredSystemEvents);
  Field('ignored-meta', R.IgnoredMetaEvents);
  Field('omitted-track-names', R.OmittedTrackNames);
  Field('discarded-release-velocities', R.DiscardedReleaseVelocities);
  Field('terminal-timing', R.TerminalTimingEvents);
  Field('redundant-timing', R.RedundantTimingEvents);
  Field('default-tempo', Ord(R.UsedDefaultTempo));
  Field('default-meter', Ord(R.UsedDefaultMeter));
  Field('synthetic-silent-voice', Ord(R.AddedSilentVoice));
  Field('voices', Length(R.Voices));
  for I := 0 to High(R.Voices) do
    Result := Result + Format('voice=%d,%d,%d,%d'#10,
      [I, R.Voices[I].SourceTrack, R.Voices[I].Channel, R.Voices[I].Lane]);
  if C.OutputMode in [wmiomMelody, wmiomRhythm, wmiomHarmony] then
  begin
    case C.OutputMode of
      wmiomMelody: Result := Result + 'projection=melody'#10;
      wmiomRhythm: Result := Result + 'projection=rhythm'#10;
      wmiomHarmony: Result := Result + 'projection=harmony'#10;
    end;
    Field('quantum', C.QuantumTicks);
    Field('order', C.Order);
    Field('samples', Length(C.Selections));
    for I := 0 to High(C.Selections) do
      Result := Result + 'sample=' +
        WfcTextEncodeToken(C.Selections[I].Name, 'music import receipt') + ',' +
        IntToStr(C.Selections[I].VoiceIndex) + ',' +
        IntToStr(C.Selections[I].StartTick) + ',' +
        IntToStr(C.Selections[I].LengthTicks) + #10;
  end;
  Result := Result + 'end'#10;
end;

function WfcMusicImportExecuteBytes(const ACommand: TWfcMusicImportCommand;
  const AInput: TWfcMidiBytes; out AStandardOutput,
  AStandardError: String): Integer;
var
  S: TWfcMusicScore;
  D: TWfcTrainingDocument;
  R: TWfcMusicMidiImportReport;
  P: TWfcMusicTrainingProjection;
  M: TWfcTrainingMetadata;
begin
  AStandardOutput := '';
  AStandardError := '';
  S := nil;
  D := nil;
  try
    try
      case ACommand.Kind of
        wmickHelp: AStandardOutput := WfcMusicImportHelpText;
        wmickVersion: AStandardOutput := WfcMusicImportVersionText;
        wmickImport:
          begin
            S := DecodeWfcMusicMidi(AInput, ACommand.ImportOptions, R);
            case ACommand.OutputMode of
              wmiomScore: AStandardOutput := EncodeWfcMusicText(S);
              wmiomReport: AStandardOutput := ImportReceipt(ACommand, AInput, R);
              wmiomMelody, wmiomRhythm, wmiomHarmony:
                begin
                  P := wmtpMelody;
                  if ACommand.OutputMode = wmiomRhythm then P := wmtpRhythm;
                  if ACommand.OutputMode = wmiomHarmony then P := wmtpHarmony;
                  M := ACommand.Metadata;
                  if (M.Name = '') or (M.LicenseIdentifier = '') or
                      (M.SourceDescription = '') then
                    raise EWfcMusic.Create('training metadata must be explicit and nonempty');
                  M.SourceDescription := M.SourceDescription + TWfcModelToken(#10 +
                    ImportReceipt(ACommand, AInput, R));
                  D := BuildWfcMusicTrainingDocument(S, ACommand.Selections, P,
                    ACommand.QuantumTicks, ACommand.Order, M);
                  AStandardOutput := EncodeWfcTrainingText(D);
                end;
            else
              raise EWfcMusic.Create('invalid output mode');
            end;
          end;
      else
        raise EWfcMusic.Create('invalid command kind');
      end;
      Result := WFC_MUSIC_IMPORT_EXIT_SUCCESS;
    except
      on E: EWfcModel do
      begin AStandardError := WfcMusicImportFormatFailure(wmifkInvalidInput,E.Message); Result := 1; end;
      on E: EWfcMidiSmf do
      begin AStandardError := WfcMusicImportFormatFailure(wmifkInvalidInput,E.Message); Result := 1; end;
      on E: EWfcTraining do
      begin AStandardError := WfcMusicImportFormatFailure(wmifkInvalidInput,E.Message); Result := 1; end;
      on E: EConvertError do
      begin AStandardError := WfcMusicImportFormatFailure(wmifkInvalidInput,E.Message); Result := 1; end;
      on E: Exception do
      begin AStandardError := WfcMusicImportFormatFailure(wmifkInternal,E.ClassName+': '+E.Message); Result := 70; end;
    end;
  finally
    D.Free;
    S.Free;
  end;
  if Result <> 0 then AStandardOutput := '';
end;

function WfcMusicImportExecuteBinaryString(const ACommand: TWfcMusicImportCommand;
  const AInput: String; out AStandardOutput, AStandardError: String): Integer;
var B: TWfcMidiBytes; I: Integer;
begin
  AStandardOutput := '';
  AStandardError := '';
  if Length(AInput) > WFC_MUSIC_IMPORT_MAX_INPUT_LENGTH then
  begin
    AStandardError := WfcMusicImportFormatFailure(wmifkInvalidInput, 'MIDI input exceeds 16 MiB');
    Exit(1);
  end;
  B := nil;
  SetLength(B, Length(AInput));
  for I := 1 to Length(AInput) do
  begin
    if Ord(AInput[I]) > 255 then
    begin
      AStandardError := WfcMusicImportFormatFailure(wmifkInvalidInput, 'binary input is not an 8-bit string');
      Exit(1);
    end;
    B[I - 1] := Byte(Ord(AInput[I]));
  end;
  Result := WfcMusicImportExecuteBytes(ACommand, B, AStandardOutput, AStandardError);
end;

end.
