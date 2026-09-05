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
program wfc_music_import_app_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc_model,
  wfc_midi_smf,
  wfc_music,
  wfc_music_text,
  wfc_music_midi_import,
  wfc_music_sequence,
  wfc_music_training,
  wfc_music_import_app,
  wfc_training,
  wfc_training_text,
  wfc_sequence,
  wfc_sequence_text,
  wfc_text_codec;

type
  TTestProcedure = procedure;

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GCheckCount);
  if ACondition then
    WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailureCount);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try
    ATest;
  except
    on E: Exception do
    begin
      Inc(GFailureCount);
      WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function ArgumentsOf(const AValues: array of String):
  TWfcMusicImportArguments;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function MusicToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($266B));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(
    UnicodeString(WideChar($266B))));
  {$ENDIF}
end;

function ContainsLine(const AText, ALine: String): Boolean;
begin
  Result := Pos(ALine + #10, AText) > 0;
end;

procedure RequireParse(const AArguments: TWfcMusicImportArguments;
  out ACommand: TWfcMusicImportCommand);
var
  LError: String;
begin
  if not WfcMusicImportParseCommand(AArguments, ACommand, LError) then
    raise Exception.Create('test setup command did not parse: ' + LError);
end;

function ParseRejected(const AArguments: TWfcMusicImportArguments;
  const AExpected: String): Boolean;
var
  LCommand: TWfcMusicImportCommand;
  LError: String;
begin
  Result := not WfcMusicImportParseCommand(AArguments, LCommand, LError) and
    ((AExpected = '') or (Pos(AExpected, LError) > 0));
end;

procedure AddEvent(var AFile: TWfcMidiFile;
  const AEvent: TWfcMidiEvent);
var
  LCount: Integer;
begin
  LCount := Length(AFile.Tracks[0].Events);
  SetLength(AFile.Tracks[0].Events, LCount + 1);
  AFile.Tracks[0].Events[LCount] := AEvent;
end;

procedure AddOn(var AFile: TWfcMidiFile; const ADelta: Cardinal;
  const APitch, AVelocity: Byte);
begin
  AddEvent(AFile, MakeWfcMidiChannelEvent(ADelta, $90,
    [APitch, AVelocity]));
end;

procedure AddOff(var AFile: TWfcMidiFile; const ADelta: Cardinal;
  const APitch, AVelocity: Byte);
begin
  AddEvent(AFile, MakeWfcMidiChannelEvent(ADelta, $80,
    [APitch, AVelocity]));
end;

function FixtureFile: TWfcMidiFile;
begin
  Result := Default(TWfcMidiFile);
  Result.Format := 0;
  Result.TicksPerQuarter := 120;
  SetLength(Result.Tracks, 1);
  AddOn(Result, 0, 60, 100);
  AddOff(Result, 120, 60, 17);
  AddOn(Result, 0, 64, 90);
  AddOff(Result, 120, 64, 0);
  AddOn(Result, 0, 67, 80);
  AddOff(Result, 120, 67, 0);
  Result.Tracks[0].EndDeltaTicks := 120;
end;

function FixtureBytes: TWfcMidiBytes;
begin
  Result := EncodeWfcMidiFile(FixtureFile);
end;

function UnsupportedFixtureBytes: TWfcMidiBytes;
var
  LFile: TWfcMidiFile;
begin
  LFile := FixtureFile;
  SetLength(LFile.Tracks[0].Events,
    Length(LFile.Tracks[0].Events) + 1);
  LFile.Tracks[0].Events[High(LFile.Tracks[0].Events)] :=
    MakeWfcMidiChannelEvent(0, $C0, [5]);
  Result := EncodeWfcMidiFile(LFile);
end;

function IncompleteFixtureBytes: TWfcMidiBytes;
var
  LFile: TWfcMidiFile;
begin
  LFile := FixtureFile;
  LFile.Tracks[0].EndDeltaTicks := 0;
  Result := EncodeWfcMidiFile(LFile);
end;

function BinaryStringOf(const ABytes: TWfcMidiBytes): String;
var
  I: Integer;
begin
  Result := '';
  SetLength(Result, Length(ABytes));
  for I := 0 to Length(ABytes) - 1 do
    Result[I + 1] := Char(ABytes[I]);
end;

function ProjectionArguments(const AMode: String):
  TWfcMusicImportArguments;
var
  LName: TWfcModelToken;
  LSource: TWfcModelToken;
  LSample: TWfcModelToken;
begin
  LName := TWfcModelToken('fixture-') + MusicToken;
  LSource := TWfcModelToken('caller source ') + MusicToken;
  LSample := TWfcModelToken('first,') + MusicToken;
  Result := ArgumentsOf([
    AMode,
    '--quantum', '120',
    '--order', '2',
    '--name', WfcTextEncodeToken(LName, 'test argument'),
    '--license', 'MIT',
    '--source', WfcTextEncodeToken(LSource, 'test argument'),
    '--sample', WfcTextEncodeToken(LSample, 'test argument') + ',0,0,240',
    '--sample', 'second,0,240,240',
    'fixture.mid']);
end;

function ExactReport: String;
begin
  Result :=
    'wfcmidiimport=1'#10 +
    'source-fnv1a32=E0067D90'#10 +
    'source-bytes=50'#10 +
    'unsupported-policy=0'#10 +
    'end-policy=1'#10 +
    'format=0'#10 +
    'tracks=1'#10 +
    'events-including-eot=7'#10 +
    'notes=3'#10 +
    'source-ticks=480'#10 +
    'score-ticks=480'#10 +
    'padding-ticks=0'#10 +
    'ignored-channel=0'#10 +
    'ignored-system=0'#10 +
    'ignored-meta=0'#10 +
    'omitted-track-names=0'#10 +
    'discarded-release-velocities=1'#10 +
    'terminal-timing=0'#10 +
    'redundant-timing=0'#10 +
    'default-tempo=1'#10 +
    'default-meter=1'#10 +
    'synthetic-silent-voice=0'#10 +
    'voices=1'#10 +
    'voice=0,0,0,0'#10 +
    'end'#10;
end;

{$PUSH}{$R-}
function InvalidOutputMode: TWfcMusicImportOutputMode;
var
  LValue: Integer;
begin
  LValue := Ord(High(TWfcMusicImportOutputMode)) + 1;
  Result := TWfcMusicImportOutputMode(LValue);
end;

function InvalidCommandKind: TWfcMusicImportCommandKind;
var
  LValue: Integer;
begin
  LValue := Ord(High(TWfcMusicImportCommandKind)) + 1;
  Result := TWfcMusicImportCommandKind(LValue);
end;
{$POP}

procedure TestParserSurface;
var
  C: TWfcMusicImportCommand;
  LError: String;
begin
  Check(WfcMusicImportParseCommand(ArgumentsOf(['--help']), C, LError) and
    (C.Kind = wmickHelp), 'help is a complete standalone command');
  Check(WfcMusicImportParseCommand(ArgumentsOf(['--version']), C, LError) and
    (C.Kind = wmickVersion), 'version is a complete standalone command');
  Check(WfcMusicImportParseCommand(ArgumentsOf(['song.mid']), C, LError) and
    (C.Kind = wmickImport) and (C.OutputMode = wmiomScore) and
    (C.InputPath = 'song.mid'), 'score import is the default command');
  Check(WfcMusicImportParseCommand(ArgumentsOf([
    '--score', 'song.mid']), C, LError) and
    (C.OutputMode = wmiomScore), 'score mode may be selected explicitly');
  Check(WfcMusicImportParseCommand(ArgumentsOf([
    '--report', '--ignore-performance', '--require-measure', '--',
    '--named.mid']), C, LError) and
    (C.OutputMode = wmiomReport) and
    (C.ImportOptions.UnsupportedEvents = wmmupIgnoreAndReport) and
    (C.ImportOptions.EndPolicy = wmmepRequireMeasure) and
    (C.InputPath = '--named.mid'),
    'report, both policies, and end-of-options parse together');

  RequireParse(ProjectionArguments('--melody'), C);
  Check((C.OutputMode = wmiomMelody) and (C.QuantumTicks = 120) and
    (C.Order = 2) and (Length(C.Selections) = 2) and
    (C.Selections[0].Name = TWfcModelToken('first,') + MusicToken) and
    (C.Selections[1].StartTick = 240),
    'melody grammar decodes metadata and repeated selections');
  RequireParse(ProjectionArguments('--rhythm'), C);
  Check(C.OutputMode = wmiomRhythm,
    'rhythm selects its training projection');
  RequireParse(ProjectionArguments('--harmony'), C);
  Check(C.OutputMode = wmiomHarmony,
    'harmony selects its training projection');

  Check(ParseRejected(nil, 'INPUT'), 'missing input is usage failure');
  Check(ParseRejected(ArgumentsOf(['--help', 'song.mid']), 'unknown option'),
    'help cannot be combined with an import');
  Check(ParseRejected(ArgumentsOf(['--unknown', 'song.mid']), 'unknown'),
    'unknown options are rejected');
  Check(ParseRejected(ArgumentsOf(['--score', '--report', 'song.mid']),
    'duplicate or conflicting'), 'output modes are mutually exclusive');
  Check(ParseRejected(ArgumentsOf([
    '--ignore-performance', '--ignore-performance', 'song.mid']),
    'duplicate'), 'policy options cannot be duplicated');
  Check(ParseRejected(ArgumentsOf(['song.mid', '--report']),
    'exactly one'), 'options after INPUT are rejected');
  Check(ParseRejected(ArgumentsOf(['a.mid', 'b.mid']), 'exactly one'),
    'exactly one input is accepted');
  Check(ParseRejected(ArgumentsOf(['--']), 'INPUT'),
    'a bare end-of-options marker still needs input');
  Check(ParseRejected(ArgumentsOf(['']), 'empty'),
    'empty input names are rejected');
  Check(ParseRejected(ArgumentsOf(['--quantum']), 'missing value'),
    'value options diagnose missing values');
  Check(ParseRejected(ArgumentsOf(['--score', '--quantum', '120', 'song.mid']),
    'training options'), 'score output rejects training-only options');
  Check(ParseRejected(ArgumentsOf([
    '--melody', '--quantum', '120', '--order', '2', '--name', 'n',
    '--source', 's', '--sample', 'x,0,0,240', 'song.mid']),
    'training requires'), 'training never infers an omitted license');
  Check(ParseRejected(ArgumentsOf([
    '--melody', '--quantum', '0', '--order', '2', '--name', 'n',
    '--license', 'MIT', '--source', 's', '--sample', 'x,0,0,240',
    'song.mid']), 'quantum'), 'zero quantum is rejected as usage');
  Check(ParseRejected(ArgumentsOf([
    '--melody', '--quantum', '120', '--order', '65', '--name', 'n',
    '--license', 'MIT', '--source', 's', '--sample', 'x,0,0,240',
    'song.mid']), 'order'), 'out-of-range order is rejected as usage');
  Check(ParseRejected(ArgumentsOf([
    '--melody', '--quantum', '120', '--order', '2', '--name', 'n',
    '--license', 'MIT', '--source', 's', '--sample', 'x,0,0',
    'song.mid']), 'NAME,VOICE,START,LENGTH'),
    'short sample records are rejected');
  Check(ParseRejected(ArgumentsOf([
    '--melody', '--quantum', '120', '--order', '2', '--name', 'n',
    '--license', 'MIT', '--source', 's', '--sample', 'x,0,0,240,extra',
    'song.mid']), 'too many'), 'long sample records are rejected');
  Check(ParseRejected(ArgumentsOf([
    '--melody', '--quantum', '120', '--order', '2', '--name', '%e2%99%ab',
    '--license', 'MIT', '--source', 's', '--sample', 'x,0,0,240',
    'song.mid']), 'uppercase'), 'noncanonical percent escapes are rejected');
  Check(WFC_MUSIC_IMPORT_EXIT_USAGE = 2,
    'a parser rejection maps to the documented usage status');
end;

procedure TestCanonicalScoreAndReport;
var
  B: TWfcMidiBytes;
  C: TWfcMusicImportCommand;
  LError: String;
  LOutput: String;
  LRepeat: String;
  LScore: TWfcMusicScore;
begin
  B := FixtureBytes;
  Check(Length(B) = 50, 'programmatic fixture has stable source bytes');
  RequireParse(ArgumentsOf(['fixture.mid']), C);
  Check(WfcMusicImportExecuteBytes(C, B, LOutput, LError) = 0,
    'default score import succeeds');
  Check((LError = '') and ContainsLine(LOutput, 'wfcmusic=1'),
    'successful score output is clean canonical text');
  LScore := DecodeWfcMusicText(LOutput);
  try
    Check(EncodeWfcMusicText(LScore) = LOutput,
      'score output survives exact canonical decode and encode');
    Check((LScore.LengthTicks = 480) and (LScore.VoiceCount = 1) and
      (LScore.SpanCount = 4),
      'score output preserves notes and terminal silence');
  finally
    LScore.Free;
  end;
  Check(WfcMusicImportExecuteBinaryString(C, BinaryStringOf(B),
    LRepeat, LError) = 0, 'Latin-1 binary-string entry point succeeds');
  Check(LRepeat = LOutput,
    'byte-array and Latin-1 entry points have identical score output');

  RequireParse(ArgumentsOf(['--report', 'fixture.mid']), C);
  Check(WfcMusicImportExecuteBytes(C, B, LOutput, LError) = 0,
    'receipt mode succeeds');
  Check(LOutput = ExactReport,
    'receipt has exact canonical ordering, counts, policy, and fingerprint');
  Check(WfcMusicImportExecuteBytes(C, B, LRepeat, LError) = 0,
    'receipt mode can be repeated');
  Check(LRepeat = LOutput,
    'receipt output is deterministic');
end;

function ProjectionToken(const AProjection, ASample,
  ACell: Integer): TWfcModelToken;
begin
  Result := '';
  case AProjection of
    0:
      case ASample * 2 + ACell of
        0: Result := 'wm1:a:60:100';
        1: Result := 'wm1:a:64:90';
        2: Result := 'wm1:a:67:80';
        3: Result := 'wm1:r';
      end;
    1:
      case ASample * 2 + ACell of
        0, 1, 2: Result := 'wr1:a';
        3: Result := 'wr1:r';
      end;
    2:
      case ASample * 2 + ACell of
        0: Result := 'wh1:p:12:0';
        1: Result := 'wh1:p:12:4';
        2: Result := 'wh1:p:12:7';
        3: Result := 'wh1:r:12:0';
      end;
  end;
end;

procedure CheckProjection(const AMode: String;
  const AProjectionIndex: Integer);
var
  B: TWfcMidiBytes;
  C: TWfcMusicImportCommand;
  D: TWfcTrainingDocument;
  I: Integer;
  J: Integer;
  LMetadata: TWfcTrainingMetadata;
  LModel: TWfcSequenceModel;
  LModelText: String;
  LOptions: TWfcTrainingOptions;
  LOutput: String;
  LError: String;
  LSample: TWfcTrainingSample;
  LStartTotal: Integer;
begin
  B := FixtureBytes;
  RequireParse(ProjectionArguments(AMode), C);
  Check(WfcMusicImportExecuteBytes(C, B, LOutput, LError) = 0,
    AMode + ' training output succeeds');
  Check((LError = '') and ContainsLine(LOutput, 'wfclearn=1'),
    AMode + ' emits canonical training text');
  D := DecodeWfcTrainingText(LOutput);
  try
    Check(EncodeWfcTrainingText(D) = LOutput,
      AMode + ' output survives exact canonical decode and encode');
    LMetadata := D.CopyMetadata;
    Check((LMetadata.Name = TWfcModelToken('fixture-') + MusicToken) and
      (LMetadata.LicenseIdentifier = 'MIT'),
      AMode + ' preserves caller name and license exactly');
    Check(Pos(String(TWfcModelToken('caller source ') + MusicToken) + #10 +
      'wfcmidiimport=1'#10, String(LMetadata.SourceDescription)) = 1,
      AMode + ' appends the import receipt to caller source text');
    Check(Pos('source-fnv1a32=E0067D90',
      String(LMetadata.SourceDescription)) > 0,
      AMode + ' source receipt carries the deterministic fingerprint');
    Check((Pos('unsupported-policy=0',
      String(LMetadata.SourceDescription)) > 0) and
      (Pos('end-policy=1', String(LMetadata.SourceDescription)) > 0),
      AMode + ' appended receipt preserves both import policies');
    Check(Pos('source=caller%20source%20%E2%99%AB%0A', LOutput) > 0,
      AMode + ' metadata uses canonical UTF-8 percent encoding');
    LOptions := D.CopyOptions;
    Check((LOptions.Kind = wtkSequence) and
      (LOptions.Boundary = wmbOpen) and
      (LOptions.Symmetry = wmsNone) and
      (LOptions.PatternWidth = 0) and
      (LOptions.PatternHeight = 0) and (LOptions.Order = 2),
      AMode + ' emits the exact sequence training options');
    Check((D.SampleCount = 2) and (D.TotalTokenCount = 4),
      AMode + ' retains selections as two explicit samples');
    for I := 0 to 1 do
    begin
      LSample := D.SampleAt(I);
      Check((LSample.Width = 2) and (LSample.Height = 1),
        AMode + ' sample ' + IntToStr(I) + ' has exact quantum dimensions');
      for J := 0 to 1 do
        Check(LSample.Tokens[J] = ProjectionToken(
          AProjectionIndex, I, J),
          AMode + ' sample ' + IntToStr(I) + ' cell ' +
          IntToStr(J) + ' has the expected projection token');
    end;

    LModelText := LearnWfcTrainingModelText(D);
    LModel := DecodeWfcSequenceText(LModelText);
    try
      Check((LModel.SampleCount = 2) and
        (LModel.SampleLengthAt(0) = 2) and
        (LModel.SampleLengthAt(1) = 2),
        AMode + ' learning preserves both source boundaries');
      LStartTotal := 0;
      for I := 0 to LModel.StateCount - 1 do
        Inc(LStartTotal, LModel.StartCountAt(I));
      Check(LStartTotal = 2,
        AMode + ' learned model has one independent start per sample');
    finally
      LModel.Free;
    end;
  finally
    D.Free;
  end;
end;

procedure TestProjectionDocuments;
begin
  CheckProjection('--melody', 0);
  CheckProjection('--rhythm', 1);
  CheckProjection('--harmony', 2);
end;

procedure TestPoliciesAndFailures;
var
  B: TWfcMidiBytes;
  C: TWfcMusicImportCommand;
  LError: String;
  LOutput: String;
begin
  RequireParse(ArgumentsOf(['--report', 'fixture.mid']), C);
  B := UnsupportedFixtureBytes;
  LOutput := 'stale output';
  LError := 'stale error';
  Check(WfcMusicImportExecuteBytes(C, B, LOutput, LError) =
    WFC_MUSIC_IMPORT_EXIT_INVALID_INPUT,
    'strict policy rejects an unsupported performance event');
  Check((LOutput = '') and (Pos('invalid input', LError) > 0),
    'domain failure clears output and has the normal invalid-input class');

  RequireParse(ArgumentsOf([
    '--report', '--ignore-performance', 'fixture.mid']), C);
  Check(WfcMusicImportExecuteBytes(C, B, LOutput, LError) = 0,
    'explicit lossy policy accepts the same performance event');
  Check(ContainsLine(LOutput, 'ignored-channel=1'),
    'lossy receipt counts the omitted performance event');

  B := IncompleteFixtureBytes;
  RequireParse(ArgumentsOf(['--report', 'fixture.mid']), C);
  Check(WfcMusicImportExecuteBytes(C, B, LOutput, LError) = 0,
    'default end policy pads an incomplete final measure');
  Check(ContainsLine(LOutput, 'source-ticks=360') and
    ContainsLine(LOutput, 'score-ticks=480') and
    ContainsLine(LOutput, 'padding-ticks=120'),
    'receipt distinguishes source duration from explicit rest padding');
  RequireParse(ArgumentsOf([
    '--report', '--require-measure', 'fixture.mid']), C);
  Check(WfcMusicImportExecuteBytes(C, B, LOutput, LError) = 1,
    'require-measure rejects that same incomplete final measure');
  Check(LOutput = '', 'require-measure failure leaves no partial receipt');

  B := FixtureBytes;
  SetLength(B, Length(B) - 1);
  LOutput := 'stale output';
  LError := 'stale error';
  Check(WfcMusicImportExecuteBytes(C, B, LOutput, LError) = 1,
    'malformed binary data is an ordinary invalid-input failure');
  Check((LOutput = '') and (Pos('invalid input', LError) > 0),
    'malformed data cannot leak stale standard output');

  B := nil;
  LOutput := 'stale output';
  Check(WfcMusicImportExecuteBytes(C, B, LOutput, LError) = 1,
    'nil byte input is rejected by the API');
  Check(LOutput = '', 'nil byte rejection clears output');

  B := FixtureBytes;
  C.OutputMode := InvalidOutputMode;
  LOutput := 'stale output';
  Check(WfcMusicImportExecuteBytes(C, B, LOutput, LError) = 1,
    'invalid output mode is rejected as invalid input');
  Check(LOutput = '', 'invalid output mode clears output');
  C.OutputMode := wmiomScore;
  C.Kind := InvalidCommandKind;
  Check(WfcMusicImportExecuteBytes(C, B, LOutput, LError) = 1,
    'invalid command kind is rejected as invalid input');

  RequireParse(ProjectionArguments('--melody'), C);
  C.Metadata.LicenseIdentifier := '';
  LOutput := 'stale output';
  Check(WfcMusicImportExecuteBytes(C, B, LOutput, LError) = 1,
    'direct training API calls cannot omit the license declaration');
  Check(LOutput = '', 'invalid direct metadata clears output');

  Check(Pos('Usage:', WfcMusicImportHelpText) = 1,
    'help text exposes the command grammar');
  Check(Pos('No quantization, cut notes, guessed licenses',
    WfcMusicImportHelpText) > 0,
    'help states its non-lossy training boundary');
  Check(WfcMusicImportVersionText =
    'wfc_music_import 1 (MIDI import=1; music training=1)'#10,
    'version text identifies all three versioned surfaces');
  Check(WfcMusicImportFormatFailure(wmifkUsage,
    'bad'#10'argument') =
    'wfc_music_import: usage error: bad argument'#10,
    'failure formatter labels and flattens messages');
end;

procedure TestBinaryStringBoundary;
var
  B: TWfcMidiBytes;
  C: TWfcMusicImportCommand;
  LBinary: String;
  LByteOutput: String;
  LError: String;
  LStringOutput: String;
begin
  B := FixtureBytes;
  RequireParse(ArgumentsOf(['--report', 'fixture.mid']), C);
  Check(WfcMusicImportExecuteBytes(C, B, LByteOutput, LError) = 0,
    'byte baseline for binary-string boundary succeeds');
  LBinary := BinaryStringOf(B);
  Check(WfcMusicImportExecuteBinaryString(C, LBinary,
    LStringOutput, LError) = 0,
    'all 8-bit source characters are accepted');
  Check(LStringOutput = LByteOutput,
    'Latin-1 conversion preserves every source byte exactly');
  {$IFDEF PAS2JS}
  LBinary := Chr($0100);
  LStringOutput := 'stale output';
  Check(WfcMusicImportExecuteBinaryString(C, LBinary,
    LStringOutput, LError) = 1,
    'browser strings containing a character above 255 are rejected');
  Check((LStringOutput = '') and (Pos('8-bit string', LError) > 0),
    'non-Latin-1 browser input clears output and explains the boundary');
  {$ENDIF}
end;

procedure TestInformationalExecution;
var
  C: TWfcMusicImportCommand;
  LError: String;
  LOutput: String;
begin
  RequireParse(ArgumentsOf(['--help']), C);
  Check(WfcMusicImportExecuteBytes(C, nil, LOutput, LError) = 0,
    'help execution needs no input bytes');
  Check((LOutput = WfcMusicImportHelpText) and (LError = ''),
    'help execution emits only canonical help text');
  RequireParse(ArgumentsOf(['--version']), C);
  Check(WfcMusicImportExecuteBytes(C, nil, LOutput, LError) = 0,
    'version execution needs no input bytes');
  Check((LOutput = WfcMusicImportVersionText) and (LError = ''),
    'version execution emits only canonical version text');
end;

begin
  WriteLn('WFC music import application conformance suite');
  WriteLn('==============================================');
  RunTest('complete command grammar', @TestParserSurface);
  RunTest('canonical score and receipt output', @TestCanonicalScoreAndReport);
  RunTest('canonical projection training documents', @TestProjectionDocuments);
  RunTest('explicit policies and failure classes', @TestPoliciesAndFailures);
  RunTest('portable binary-string boundary', @TestBinaryStringBoundary);
  RunTest('informational command execution', @TestInformationalExecution);
  WriteLn('==============================================');
  WriteLn(Format('%d checks, %d failures', [GCheckCount, GFailureCount]));
  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d music import application checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
