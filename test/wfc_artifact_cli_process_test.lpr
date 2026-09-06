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
program wfc_artifact_cli_process_test;

{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL artifact process tests require native FPC}{$ENDIF}

uses
  {$IFDEF UNIX}cthreads, BaseUnix,{$ENDIF}
  Classes, SysUtils, Process, Pipes, wfc_process_test_support, wfc_browser_socket,
  wfc_pipeline_model, wfc_pipeline_text, wfc_pipeline_run,
  wfc_pipeline_run_text, wfc_pipeline_result, wfc_pipeline_result_text;

const
  CHILD_TIMEOUT = 15000;
  CAPTURE_LIMIT = 2097152;
  TRAINING_INPUT_LIMIT = 8388608;
  RULES_TEXT = 'wfcrules=1'#10'rank=1'#10'values=2'#10 +
    'v=0,1,A'#10'v=1,1,B'#10'rules=0'#10'signature=CAE54441'#10'end'#10;
  RECIPE_SUMMARY = 'valid canonical wfcpipeline=1 signature=A8FD55BC ' +
    'resources=1 passes=3 dependencies=2 bridges=0 requirements=0'#10;
  OPEN_PATTERN_SOURCE = 'wfclearn=1'#10'name=tiny%1B%5B2J'#10'license=MIT'#10 +
    'source=project-authored%20process%20fixture'#10'kind=pattern2d'#10 +
    'boundary=open'#10'symmetry=none'#10'footprint=2,2'#10'order=0'#10 +
    'samples=1'#10'sample=0,2,2,tiny'#10'token=0,0,A'#10 +
    'token=0,1,A'#10'token=0,2,A'#10'token=0,3,A'#10'end'#10;
  CIRCULAR_SEQUENCE_SOURCE = 'wfclearn=5'#10'name=circular'#10'license=MIT'#10 +
    'source=project-authored%20process%20fixture'#10'kind=sequence'#10 +
    'boundary=wrap'#10'symmetry=none'#10'footprint=0,0'#10'order=2'#10 +
    'samples=1'#10'sample=0,2,1,1,ring'#10'token=0,0,A'#10'token=0,1,B'#10 +
    'value-quota-version=0'#10'value-quotas=0'#10 +
    'connectivity-version=0'#10'connectivities=0'#10'end'#10;
  CIRCULAR_SEQUENCE_MODEL = 'wfcs=2'#10'boundary=wrap'#10'order=2'#10 +
    'samples=1'#10's=0,2'#10'tokens=2'#10't=0,A'#10't=1,B'#10 +
    'states=2'#10'q=0,1,0,0,T1,E0'#10'q=1,1,0,0,T0,E1'#10'end'#10;

type
  {$IF DECLARED(TIODescriptor)}
  TDescriptorAccess = class(TIODescriptor)
  public
    function HasOwnHandle: Boolean;
  end;
  {$ENDIF}

  TChildOutcome = record
    Code: Integer;
    OutputText, ErrorText: String;
  end;

  { A separate writer lets the parent drain both output pipes and enforce its
    deadline even when a child stops consuming standard input. The child is
    always stopped before its pipe owner or this worker can be released. }
  TInputWriter = class(TThread)
  private
    FChild: TProcess;
    FText, FError: String;
  protected
    procedure Execute; override;
  public
    constructor Create(const AChild: TProcess; const AText: String);
    property ErrorText: String read FError;
  end;

var
  Checks, Cases: Integer;
  Validator, Inspector, RepoRoot, FixtureRoot: String;
  OwnedFiles: TStringList;
  RecipePath, RunPath, ResultPath, NonRunPath, NonResultPath: String;
  RecipeText, RunText, ResultText, NonRunText, NonResultText: String;
  EmptyPath, BadPath, MissingPath, OversizePath, ChangedResultPath: String;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;

constructor TInputWriter.Create(const AChild: TProcess; const AText: String);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FChild := AChild;
  FText := AText;
  Start;
end;

{$IF DECLARED(TIODescriptor)}
function TDescriptorAccess.HasOwnHandle: Boolean;
begin
  Result := OurHandle <> THandle(-1);
end;
{$ENDIF}

procedure TInputWriter.Execute;
var LOffset, LCount, LWritten: Integer;
begin
  try
    try
      LOffset := 0;
      while LOffset < Length(FText) do
      begin
        LCount := Length(FText) - LOffset;
        if LCount > 4096 then LCount := 4096;
        LWritten := FChild.Input.Write(FText[LOffset + 1], LCount);
        if LWritten <= 0 then raise EWriteError.Create('stdin write made no progress');
        Inc(LOffset, LWritten);
      end;
    finally
      {$IF DECLARED(TIODescriptor)}
      { The descriptor-based FCL Process transfers its input handle into the
        stream on first access. In 3.3.1 CloseInput alone then leaves that
        stream open. Release the stream-owned writer exactly once so the
        tested child receives EOF; retain the stream object for Process to
        destroy, with no later close of a possibly recycled OS handle. }
      { Resolve the stream before querying its descriptor: only the observed
        transferred-ownership state needs this compatibility close. }
      if FChild.Input <> nil then
        if not TDescriptorAccess(FChild.InputDescriptor).HasOwnHandle then
        begin
          FChild.Input.DontClose := True;
          FileClose(FChild.Input.Handle);
        end;
      {$ENDIF}
      FChild.CloseInput;
    end;
  except on E: Exception do FError := E.Message; end;
end;

procedure ReadAvailable(const APipe: TInputPipeStream; var AText: String);
var LBuffer: array[0..4095] of Byte; LCount, LOffset: Integer;
begin
  while APipe.NumBytesAvailable > 0 do
  begin
    LCount := APipe.NumBytesAvailable;
    if LCount > SizeOf(LBuffer) then LCount := SizeOf(LBuffer);
    LCount := APipe.Read(LBuffer[0], LCount);
    if LCount <= 0 then Exit;
    if Length(AText) > CAPTURE_LIMIT - LCount then
      raise Exception.Create('artifact child exceeded the bounded output capture');
    LOffset := Length(AText);
    SetLength(AText, LOffset + LCount);
    Move(LBuffer[0], AText[LOffset + 1], LCount);
  end;
end;

function ElapsedMilliseconds(const AStart, ANow: QWord): QWord;
begin
  { The clock is monotonic, so a smaller value can only be counter wrap, not a
    wall-clock correction. Split subtraction keeps checked arithmetic valid
    on the wrap path as well as on ordinary deadlines. }
  if ANow >= AStart then Result := ANow - AStart
  else Result := (High(QWord) - AStart) + ANow + 1;
end;

function WaitWriter(const AWriter: TInputWriter): Boolean;
var LStart: QWord;
begin
  LStart := WfcBrowserTickCount64;
  repeat
    if AWriter.Finished then Exit(True);
    Sleep(2);
  until ElapsedMilliseconds(LStart, WfcBrowserTickCount64) >= 5000;
  Result := AWriter.Finished;
end;

function WaitOwnedChild(const AChild: TProcess): Boolean;
var LStart: QWord;
begin
  { Stable Unix WaitOnExit(timeout) uses gettimeofday internally. Poll its
    nonblocking Running state under the same OS-monotonic clock as execution
    and writer teardown, including stable FPC on macOS. }
  LStart := WfcBrowserTickCount64;
  repeat
    if not AChild.Running then Exit(True);
    Sleep(2);
  until ElapsedMilliseconds(LStart, WfcBrowserTickCount64) >= 5000;
  Result := not AChild.Running;
end;

function Invoke(const AExecutable: String; const AArguments: array of String;
  const AInput: String = ''): TChildOutcome;
var LChild: TProcess; LWriter: TInputWriter; LStart: QWord; I: Integer;
begin
  Inc(Cases);
  Result.Code := -1; Result.OutputText := ''; Result.ErrorText := '';
  LChild := TProcess.Create(nil); LWriter := nil;
  try
    LChild.Executable := AExecutable;
    LChild.CurrentDirectory := FixtureRoot;
    for I := 0 to High(AArguments) do LChild.Parameters.Add(AArguments[I]);
    LChild.Options := [poUsePipes, poNoConsole];
    LChild.Execute;
    LWriter := TInputWriter.Create(LChild, AInput);
    LStart := WfcBrowserTickCount64;
    repeat
      ReadAvailable(LChild.Output, Result.OutputText);
      ReadAvailable(LChild.Stderr, Result.ErrorText);
      if not LChild.Running then Break;
      if ElapsedMilliseconds(LStart, WfcBrowserTickCount64) >= CHILD_TIMEOUT then
        raise Exception.Create('artifact child exceeded its finite process deadline; case=' +
          IntToStr(Cases) + '; executable=' + AExecutable + '; arguments=' +
          StringReplace(LChild.Parameters.Text, LineEnding, ' | ', [rfReplaceAll]));
      Sleep(2);
    until False;
    ReadAvailable(LChild.Output, Result.OutputText);
    ReadAvailable(LChild.Stderr, Result.ErrorText);
    Result.Code := WfcProcessExitCode(LChild);
    Check(WaitWriter(LWriter), 'stdin worker has a bounded completion');
    if Result.Code = 0 then
      Check(LWriter.ErrorText = '', 'successful child consumed supplied stdin: ' + LWriter.ErrorText);
  finally
    if LChild.Running then
    begin
      {$IFDEF UNIX}
      { Stable Unix Terminate calls an untimed wait internally. Signal only
        this owned child, and use the explicitly bounded wait below. }
      fpKill(LChild.ProcessID, SIGKILL);
      {$ELSE}
      LChild.Terminate(1);
      {$ENDIF}
      Check(WaitOwnedChild(LChild), 'owned artifact child stopped after failure');
    end;
    if LWriter <> nil then
    begin
      { Closing the child's end by terminating it releases a blocked writer.
        Do not free either pipe owner while a worker could still reference it. }
      if not WaitWriter(LWriter) then
        raise Exception.Create('owned stdin writer did not stop after child exit');
      LWriter.Free;
    end;
    LChild.Free;
  end;
end;

function Success(const AExecutable: String; const AArguments: array of String;
  const AInput, ALabel: String): String;
var LOutcome: TChildOutcome; I: Integer;
begin
  LOutcome := Invoke(AExecutable, AArguments, AInput);
  Check(LOutcome.Code = 0, ALabel + ': success exit; stderr=' + LOutcome.ErrorText);
  Check(LOutcome.ErrorText = '', ALabel + ': empty stderr');
  Check(Pos(#13, LOutcome.OutputText) = 0, ALabel + ': LF-only output bytes');
  for I := 1 to Length(LOutcome.OutputText) do
    if (LOutcome.OutputText[I] <> #10) and not (LOutcome.OutputText[I] in [#32..#126]) then
      raise Exception.Create(ALabel + ': output must escape control and non-ASCII bytes');
  Result := LOutcome.OutputText;
end;

procedure Failure(const AExecutable: String; const AArguments: array of String;
  const AInput: String; const ACode: Integer; const APrefix, ALabel: String);
var LOutcome: TChildOutcome;
begin
  LOutcome := Invoke(AExecutable, AArguments, AInput);
  Check(LOutcome.Code = ACode, ALabel + ': expected exit ' + IntToStr(ACode) +
    ', got ' + IntToStr(LOutcome.Code) + '; stderr=' + LOutcome.ErrorText);
  Check(LOutcome.OutputText = '', ALabel + ': failure stdout is exactly empty');
  Check((Pos(APrefix, LOutcome.ErrorText) = 1) and
    (Length(LOutcome.ErrorText) <= 2048), ALabel + ': bounded diagnostic prefix');
  Check((LOutcome.ErrorText <> '') and
    (LOutcome.ErrorText[Length(LOutcome.ErrorText)] = #10) and
    (Pos(#13, LOutcome.ErrorText) = 0), ALabel + ': LF-terminated diagnostic');
end;

function ReadBytes(const APath: String): String;
var LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    if LStream.Size > CAPTURE_LIMIT then raise Exception.Create('test fixture exceeds read bound');
    SetLength(Result, LStream.Size);
    if Result <> '' then LStream.ReadBuffer(Result[1], Length(Result));
  finally LStream.Free; end;
end;

function WriteOwned(const AName, AText: String): String;
var LStream: TFileStream;
begin
  Result := ExpandFileName(FixtureRoot + DirectorySeparator + AName);
  if Pos(IncludeTrailingPathDelimiter(FixtureRoot), Result) <> 1 then
    raise Exception.Create('fixture path escaped its owned directory');
  if FileExists(Result) then raise Exception.Create('fixture target already exists');
  LStream := TFileStream.Create(Result, fmCreate);
  OwnedFiles.Add(Result);
  try if AText <> '' then LStream.WriteBuffer(AText[1], Length(AText));
  finally LStream.Free; end;
end;

function RepositoryPath(const ARelative: String): String;
begin
  Result := RepoRoot + DirectorySeparator +
    StringReplace(ARelative, '/', DirectorySeparator, [rfReplaceAll]);
end;

function CopyFixture(const ARelative, AName: String): String;
begin Result := WriteOwned(AName, ReadBytes(RepositoryPath(ARelative))); end;

procedure CreateChangedResult;
var LRecipe: TWfcPipelineModel; LRun: TWfcPipelineRun;
  LStored, LChanged: TWfcPipelineResult; LOutcomes: TWfcPipelinePassOutcomes;
begin
  LRecipe := nil; LRun := nil; LStored := nil; LChanged := nil;
  try
    LRecipe := DecodeWfcPipelineModelText(RecipeText);
    LRun := DecodeWfcPipelineRunText(RunText, LRecipe);
    LStored := DecodeWfcPipelineResultText(ResultText, LRecipe, LRun);
    LOutcomes := LStored.CopyPassOutcomes;
    Inc(LOutcomes[0].Decisions);
    { This is a valid signed stored report, but not the deterministic report
      produced by this invocation. Ordinary validation must not promise replay. }
    LChanged := TWfcPipelineResult.Create(LRecipe, LRun, LStored.CopyVersions,
      LStored.Status, LStored.PassBacktracks, LStored.EvidenceKind,
      LStored.EvidenceSignature, LStored.CopyFailure, LOutcomes, LStored.CopyLayers);
    ChangedResultPath := WriteOwned('changed report.wfcresult',
      EncodeWfcPipelineResultText(LChanged));
  finally LChanged.Free; LStored.Free; LRun.Free; LRecipe.Free; end;
end;

procedure TestSingle(const AFamily, APath: String);
var LBytes, LReport: String;
begin
  LBytes := ReadBytes(APath);
  Check(Success(Validator, [AFamily, '--emit-canonical', APath], '',
    AFamily + ' file canonical') = LBytes, AFamily + ': exact canonical file bytes');
  Check(Success(Validator, [AFamily, '--emit-canonical', '-'], LBytes,
    AFamily + ' stdin canonical') = LBytes, AFamily + ': exact canonical stdin bytes');
  Check(Success(Validator, [AFamily, '--quiet', APath], '', AFamily + ' quiet') = '',
    AFamily + ': quiet has zero stdout bytes');
  LReport := Success(Inspector, [AFamily, APath], '', AFamily + ' inspect file');
  Check((Pos('wfc-inspect=1'#10'family=' + AFamily + #10, LReport) = 1) and
    (Pos(#10'validation=', LReport) > 0) and
    (Pos(#10'execution=not-run'#10, LReport) > 0),
    AFamily + ': inspector declares its validation scope');
  Check(Success(Inspector, [AFamily, '-'], LBytes, AFamily + ' inspect stdin') = LReport,
    AFamily + ': inspector stdin/file byte parity');
  Failure(Validator, [AFamily, EmptyPath], '', 1,
    'wfc-validate: invalid ', AFamily + ' empty file is invalid, not I/O');
  Failure(Inspector, [AFamily, '-'], '', 1,
    'wfc-inspect: invalid ', AFamily + ' empty stdin is invalid, not I/O');
  Failure(Validator, [AFamily, '-'], 'not-an-artifact'#10, 1,
    'wfc-validate: invalid ', AFamily + ' malformed stdin');
  Failure(Inspector, [AFamily, BadPath], '', 1,
    'wfc-inspect: invalid ', AFamily + ' malformed file');
  Failure(Validator, [AFamily, MissingPath], '', 3,
    'wfc-validate: I/O error: ', AFamily + ' missing input');
  Failure(Inspector, [AFamily, MissingPath], '', 3,
    'wfc-inspect: I/O error: ', AFamily + ' inspector missing input');
end;

procedure TestContexts;
var LReport, LChanged: String; I: Integer;
  LArgs: array[0..4] of String; LInput: String;
begin
  Check(Success(Validator, ['recipe', RecipePath], '', 'legacy recipe summary') =
    RECIPE_SUMMARY, 'legacy recipe success bytes remain unchanged');
  Check(Success(Validator, ['run', '--emit-canonical', RecipePath, RunPath], '',
    'bound run file') = RunText, 'bound run canonical bytes');
  Check(Success(Validator, ['run', '--emit-canonical', '-', RunPath], RecipeText,
    'run recipe stdin') = RunText, 'run recipe stdin bytes');
  Check(Success(Validator, ['run', '--emit-canonical', RecipePath, '-'], RunText,
    'run primary stdin') = RunText, 'run primary stdin bytes');
  Check(Success(Validator, ['run', '--quiet', RecipePath, RunPath], '', 'run quiet') = '',
    'bound run quiet has no output bytes');
  Check(Success(Validator, ['result', '--quiet', RecipePath, RunPath, ResultPath], '',
    'result quiet') = '', 'bound result quiet has no output bytes');
  Check(Success(Validator, ['result', '--emit-canonical', RecipePath, RunPath, ResultPath], '',
    'bound result file') = ResultText, 'bound result file canonical bytes');
  LReport := Success(Inspector, ['run', RecipePath, RunPath], '', 'inspect bound run');
  Check(Success(Inspector, ['run', '-', RunPath], RecipeText, 'inspect run recipe stdin') =
    LReport, 'inspector run recipe stdin parity');
  Check(Success(Inspector, ['run', RecipePath, '-'], RunText, 'inspect run stdin') =
    LReport, 'inspector run stdin parity');
  LReport := Success(Inspector, ['result', RecipePath, RunPath, ResultPath], '', 'inspect result');
  for I := 0 to 2 do
  begin
    LArgs[0] := 'result'; LArgs[1] := '--emit-canonical';
    LArgs[2] := RecipePath; LArgs[3] := RunPath; LArgs[4] := ResultPath;
    LArgs[I + 2] := '-';
    case I of 0: LInput := RecipeText; 1: LInput := RunText; else LInput := ResultText; end;
    Check(Success(Validator, LArgs, LInput, 'result stdin position ' + IntToStr(I)) =
      ResultText, 'result each stdin position returns exact canonical bytes');
    Check(Success(Inspector, ['result', LArgs[2], LArgs[3], LArgs[4]], LInput,
      'inspector result stdin position ' + IntToStr(I)) = LReport,
      'inspector result each stdin position byte parity');
  end;
  Check(Success(Validator, ['result', '--replay', '--emit-canonical',
    RecipePath, RunPath, ResultPath], '', 'solved exact replay') = ResultText,
    'solved replay preserves exact stored bytes');
  Check(Success(Validator, ['result', '--quiet', '--replay',
    RecipePath, NonRunPath, NonResultPath], '', 'valid nonsolved replay quiet') = '',
    'nonsolved replay succeeds with no output');
  Check(Success(Validator, ['result', '--replay', '--emit-canonical',
    RecipePath, NonRunPath, '-'], NonResultText, 'nonsolved replay stdin') = NonResultText,
    'nonsolved is valid stored evidence, not exit 4');
  LReport := Success(Inspector, ['result', RecipePath, NonRunPath, NonResultPath], '',
    'inspect valid nonsolved result');
  Check(Pos('solved-public-quotas-and-connectivity=not-applicable', LReport) > 0,
    'nonsolved inspection does not claim to validate absent solved layers');
  LChanged := ReadBytes(ChangedResultPath);
  Check(Success(Validator, ['result', '--emit-canonical', RecipePath, RunPath,
    ChangedResultPath], '', 'valid non-replayed changed report') = LChanged,
    'ordinary result validation does not claim deterministic execution');
  Success(Inspector, ['result', RecipePath, RunPath, ChangedResultPath], '',
    'inspector accepts stored report without replay');
  Failure(Validator, ['result', '--replay', RecipePath, RunPath, ChangedResultPath], '', 1,
    'wfc-validate: invalid result: ', 'exact replay rejects altered terminal counters');
  Failure(Validator, ['result', '--emit-canonical', RecipePath, NonRunPath, ResultPath], '', 1,
    'wfc-validate: invalid result: ', 'wrong invocation binding');
  Failure(Inspector, ['result', RecipePath, NonRunPath, ResultPath], '', 1,
    'wfc-inspect: invalid result: ', 'inspector wrong invocation binding');
  Failure(Validator, ['run', RepositoryPath('test/fixtures/training-cli/connectivity.wfcpipeline'),
    RunPath], '', 1, 'wfc-validate: invalid run: ', 'wrong recipe binding');
  Failure(Inspector, ['run', RepositoryPath('test/fixtures/training-cli/connectivity.wfcpipeline'),
    RunPath], '', 1, 'wfc-inspect: invalid run: ', 'inspector wrong recipe binding');
  Failure(Validator, ['run', BadPath, RunPath], '', 1,
    'wfc-validate: invalid run: ', 'invalid recipe context');
  Failure(Validator, ['result', RecipePath, BadPath, ResultPath], '', 1,
    'wfc-validate: invalid result: ', 'invalid run context');
  Failure(Inspector, ['result', RecipePath, RunPath, EmptyPath], '', 1,
    'wfc-inspect: invalid result: ', 'empty result document');
  Failure(Validator, ['result', MissingPath, RunPath, ResultPath], '', 3,
    'wfc-validate: I/O error: ', 'missing recipe context');
  Failure(Inspector, ['result', RecipePath, MissingPath, ResultPath], '', 3,
    'wfc-inspect: I/O error: ', 'missing run context');
  Failure(Validator, ['result', RecipePath, RunPath, MissingPath], '', 3,
    'wfc-validate: I/O error: ', 'missing primary result');
  Failure(Inspector, ['result', RecipePath, RunPath, MissingPath], '', 3,
    'wfc-inspect: I/O error: ', 'inspector missing primary result');
  Failure(Validator, ['run', RecipePath, '-'], '', 1,
    'wfc-validate: invalid run: ', 'empty primary run stdin');
  Failure(Inspector, ['run', RecipePath, EmptyPath], '', 1,
    'wfc-inspect: invalid run: ', 'empty primary run file');
end;

procedure TestArguments;
var LTool, LPrefix, LText, LName: String; I: Integer;
begin
  Check(Success(Validator, ['--version'], '', 'validator version') =
    'wfc-validate 2 (wfcpipeline=1,2,3)'#10, 'validator exact version bytes');
  Check(Success(Inspector, ['--version'], '', 'inspector version') =
    'wfc-inspect 1'#10, 'inspector exact version bytes');
  for I := 0 to 1 do
  begin
    if I = 0 then begin LTool := Validator; LName := 'wfc-validate'; end
    else begin LTool := Inspector; LName := 'wfc-inspect'; end;
    LPrefix := LName + ': usage error: ';
    LText := Success(LTool, ['--help'], '', LName + ' help');
    Check(Pos('Usage:', LText) = 1, LName + ': help describes usage');
    Failure(LTool, [], '', 2, LPrefix, 'missing family');
    Failure(LTool, ['unknown', RecipePath], '', 2, LPrefix, 'unknown family');
    Failure(LTool, ['recipe'], '', 2, LPrefix, 'missing primary path');
    Failure(LTool, ['recipe', ''], '', 2, LPrefix, 'quoted empty path');
    Failure(LTool, ['recipe', '--unknown', RecipePath], '', 2, LPrefix, 'unknown option');
    Failure(LTool, ['recipe', RecipePath, '--quiet'], '', 2, LPrefix, 'option after path');
    Failure(LTool, ['--help', '--version'], '', 2, LPrefix, 'standalone help cannot hide arguments');
    Failure(LTool, ['run', '-', '-'], '', 2, LPrefix, 'run duplicate stdin');
    Failure(LTool, ['result', '-', '-', ResultPath], '', 2, LPrefix, 'result duplicate context stdin');
    Failure(LTool, ['result', '-', RunPath, '-'], '', 2, LPrefix, 'result duplicate separated stdin');
    Failure(LTool, ['result', RecipePath, '-', '-'], '', 2, LPrefix, 'result duplicate primary stdin');
  end;
  Failure(Validator, ['recipe', '--quiet', '--emit-canonical', RecipePath], '', 2,
    'wfc-validate: usage error: ', 'incompatible output modes');
  Failure(Validator, ['run', '--replay', RecipePath, RunPath], '', 2,
    'wfc-validate: usage error: ', 'replay requires a result');
  Failure(Inspector, ['result', '--replay', RecipePath, RunPath, ResultPath], '', 2,
    'wfc-inspect: usage error: ', 'inspector never runs a replay');
  Failure(Inspector, ['recipe', '--quiet', RecipePath], '', 2,
    'wfc-inspect: usage error: ', 'inspector has no quiet output mode');
  Failure(Inspector, ['recipe', '--emit-canonical', RecipePath], '', 2,
    'wfc-inspect: usage error: ', 'inspector is not canonical exporter');
  Failure(Inspector, ['recipe', '--limit', RecipePath], '', 2,
    'wfc-inspect: usage error: ', 'limit requires an integer');
  Failure(Inspector, ['recipe', '--limit', '-1', RecipePath], '', 2,
    'wfc-inspect: usage error: ', 'negative detail limit');
  Failure(Inspector, ['recipe', '--limit', '01', RecipePath], '', 2,
    'wfc-inspect: usage error: ', 'noncanonical padded limit');
  Failure(Inspector, ['recipe', '--limit', '+1', RecipePath], '', 2,
    'wfc-inspect: usage error: ', 'signed positive limit');
  Failure(Inspector, ['recipe', '--limit', '2147483648', RecipePath], '', 2,
    'wfc-inspect: usage error: ', 'overflow detail limit');
  Failure(Inspector, ['recipe', '--limit', '0', '--limit', '1', RecipePath], '', 2,
    'wfc-inspect: usage error: ', 'duplicate detail limit');
  WriteOwned('--quiet', RecipeText);
  Check(Success(Validator, ['recipe', '--emit-canonical', '--', '--quiet'], '',
    'literal option-like filename') = RecipeText, 'option terminator preserves exact filename');
  Check(Success(Inspector, ['recipe', '--', '--quiet'], '', 'inspector option-like filename') =
    Success(Inspector, ['recipe', RecipePath], '', 'inspector ordinary filename'),
    'inspector option terminator byte parity');
  LText := Success(Inspector, ['recipe', '--limit', '0', RecipePath], '', 'zero detail limit');
  Check(LText = 'wfc-inspect=1'#10'family=recipe'#10'summary=' + RECIPE_SUMMARY +
    'validation=canonical-static-contract;satisfiability=not-proven'#10 +
    'execution=not-run'#10'detail-limit=0'#10'details-shown=0'#10 +
    'truncated=true'#10'truncation=record-limit'#10,
    'summary-only inspection has exact documented report bytes');
  Check((Pos('details-shown=0'#10, LText) > 0) and
    (Pos('truncated=true'#10'truncation=record-limit'#10, LText) > 0),
    'zero limit retains summary and declares record-limit truncation');
  Check(Length(Success(Inspector, ['recipe', '--limit', '2147483647', RecipePath], '',
    'maximum integer detail limit')) > Length(LText), 'maximum limit renders actual details only');
end;

procedure TestInputs;
const Kinds: array[0..4] of String = ('adjacency1d', 'adjacency2d',
  'adjacency3d', 'pattern2d', 'sequence');
var I: Integer; LPath, LFamily: String; LStream: TFileStream;
begin
  TestSingle('recipe', RecipePath);
  Failure(Validator, ['recipe', '-'], #239#187#191 + RecipeText, 1,
    'wfc-validate: invalid recipe: ', 'raw stdin BOM is not silently normalized');
  Failure(Inspector, ['recipe', '-'], #239#187#191 + RecipeText, 1,
    'wfc-inspect: invalid recipe: ', 'inspector raw stdin BOM is not normalized');
  Failure(Validator, ['recipe', '-'], StringReplace(RecipeText, #10, #13#10, [rfReplaceAll]), 1,
    'wfc-validate: invalid recipe: ', 'CRLF stdin is rejected rather than rewritten');
  Failure(Inspector, ['recipe', '-'], StringReplace(RecipeText, #10, #13#10, [rfReplaceAll]), 1,
    'wfc-inspect: invalid recipe: ', 'inspector preserves input byte strictness');
  TestSingle('rules', WriteOwned('rules data.wfcrules', RULES_TEXT));
  for I := 0 to High(Kinds) do
  begin
    LPath := CopyFixture('examples/learning/04_TrainingDocuments/' + Kinds[I] + '.model',
      Kinds[I] + ' model');
    if I < 3 then LFamily := 'model' else LFamily := Kinds[I];
    TestSingle(LFamily, LPath);
    TestSingle('training', CopyFixture('examples/learning/04_TrainingDocuments/' +
      Kinds[I] + '.wfclearn', Kinds[I] + ' source.wfclearn'));
  end;
  TestSingle('training', CopyFixture('test/fixtures/training-cli/connectivity.wfclearn',
    'connectivity source.wfclearn'));
  TestSingle('recipe', CopyFixture('test/fixtures/training-cli/connectivity.wfcpipeline',
    'connectivity recipe.wfcpipeline'));
  { Open pattern input is a valid source contract even though its portable
    recipe export is currently unsupported. Inspection must not train/export. }
  TestSingle('training', WriteOwned('open pattern source.wfclearn', OPEN_PATTERN_SOURCE));
  TestSingle('training', WriteOwned('circular sequence source.wfclearn', CIRCULAR_SEQUENCE_SOURCE));
  TestSingle('sequence', WriteOwned('circular sequence model.wfcs', CIRCULAR_SEQUENCE_MODEL));
  Failure(Validator, ['sequence', '-'], StringReplace(CIRCULAR_SEQUENCE_MODEL,
    'T1,E0', 'B,E0', []), 1, 'wfc-validate: invalid sequence: ',
    'circular source cannot smuggle an open BOS state');
  Failure(Inspector, ['training', '-'], StringReplace(CIRCULAR_SEQUENCE_SOURCE,
    'wfclearn=5', 'wfclearn=4', []), 1, 'wfc-inspect: invalid training: ',
    'circular training cannot masquerade as a legacy source');
  OversizePath := WriteOwned('oversize training', '');
  LStream := TFileStream.Create(OversizePath, fmOpenWrite);
  try LStream.Size := TRAINING_INPUT_LIMIT + 1; finally LStream.Free; end;
  Failure(Validator, ['training', OversizePath], '', 1,
    'wfc-validate: invalid training: ', 'training size is invalid artifact, not I/O');
  Failure(Inspector, ['training', OversizePath], '', 1,
    'wfc-inspect: invalid training: ', 'inspector applies family-specific file limit');
  Failure(Validator, ['training', '-'], StringOfChar('x', TRAINING_INPUT_LIMIT + 1), 1,
    'wfc-validate: invalid training: ', 'bounded streaming input limit');
  Failure(Inspector, ['training', '-'], StringOfChar('x', TRAINING_INPUT_LIMIT + 1), 1,
    'wfc-inspect: invalid training: ', 'inspector bounded streaming input limit');
  Failure(Validator, ['training', FixtureRoot], '', 3,
    'wfc-validate: I/O error: ', 'directory cannot supply artifact bytes');
  Failure(Inspector, ['training', FixtureRoot], '', 3,
    'wfc-inspect: I/O error: ', 'inspector directory input is I/O failure');
end;

procedure Main;
var LId: TGuid; I: Integer;
begin
  {$IFDEF UNIX}
  { A child may reject an oversized stream and close its pipe before the
    writer finishes. Report that through the pipe write result, not SIGPIPE. }
  fpSignal(SIGPIPE, SignalHandler(SIG_IGN));
  {$ENDIF}
  if ParamCount <> 3 then raise Exception.Create(
    'usage: wfc_artifact_cli_process_test VALIDATOR INSPECTOR REPO_ROOT');
  Validator := ExpandFileName(ParamStr(1)); Inspector := ExpandFileName(ParamStr(2));
  RepoRoot := ExpandFileName(ParamStr(3));
  Check(FileExists(Validator) and FileExists(Inspector), 'both artifact executables exist');
  Check(DirectoryExists(RepoRoot), 'repository root exists');
  Check(ElapsedMilliseconds(7, 12) = 5, 'ordinary monotonic elapsed time');
  Check(ElapsedMilliseconds(High(QWord) - 3, 2) = 6,
    'counter wrap remains safe under checked arithmetic');
  if CreateGuid(LId) <> 0 then raise Exception.Create('cannot name owned fixture directory');
  FixtureRoot := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(ParamStr(0)))) +
    'artifact process ' + GuidToString(LId);
  Check(CreateDir(FixtureRoot), 'create fresh owned path with spaces');
  OwnedFiles := TStringList.Create;
  try
    try
      RecipePath := CopyFixture('test/fixtures/pipeline-cli/recipe.wfcpipeline', 'recipe data.wfcpipeline');
      RunPath := CopyFixture('test/fixtures/pipeline-cli/solved.wfcrun', 'solved run.wfcrun');
      ResultPath := CopyFixture('test/fixtures/pipeline-cli/solved.wfcresult', 'solved result.wfcresult');
      NonRunPath := CopyFixture('test/fixtures/pipeline-cli/nonsolved.wfcrun', 'nonsolved run.wfcrun');
      NonResultPath := CopyFixture('test/fixtures/pipeline-cli/nonsolved.wfcresult', 'nonsolved result.wfcresult');
      RecipeText := ReadBytes(RecipePath); RunText := ReadBytes(RunPath); ResultText := ReadBytes(ResultPath);
      NonRunText := ReadBytes(NonRunPath); NonResultText := ReadBytes(NonResultPath);
      EmptyPath := WriteOwned('empty input', ''); BadPath := WriteOwned('bad input', 'not-an-artifact'#10);
      MissingPath := FixtureRoot + DirectorySeparator + 'missing input';
      CreateChangedResult;
      TestArguments; TestInputs; TestContexts;
      Check(ReadBytes(RecipePath) = RecipeText, 'input recipe unchanged');
      Check(ReadBytes(RunPath) = RunText, 'input invocation unchanged');
      Check(ReadBytes(ResultPath) = ResultText, 'input result unchanged');
    finally
      { Remove only the exact files created by this invocation; never recurse
        through a directory that might have gained an unrelated entry. }
      for I := OwnedFiles.Count - 1 downto 0 do
        Check(DeleteFile(OwnedFiles[I]), 'remove exact owned test fixture');
      Check(RemoveDir(FixtureRoot), 'remove empty owned test fixture directory');
    end;
  finally OwnedFiles.Free; end;
end;

begin
  try
    Main;
    WriteLn('Artifact CLI process cases: ', Cases, '; checks: ', Checks, '/', Checks);
  except on E: Exception do
    begin WriteLn(StdErr, 'wfc_artifact_cli_process_test: ', E.Message); Halt(1); end;
  end;
end.
