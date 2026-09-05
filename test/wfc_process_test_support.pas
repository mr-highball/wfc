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
unit wfc_process_test_support;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Process;

type
  EWfcProcessAbnormalExit = class(Exception);

function WfcProcessExitCode(const AProcess: TProcess): Integer;

implementation

{$IFDEF UNIX}
uses BaseUnix;
{$ENDIF}

function WfcProcessExitCode(const AProcess: TProcess): Integer;
{$IFDEF UNIX}
var Status: Integer;
{$ENDIF}
begin
  if AProcess.Running then
    raise Exception.Create('cannot read an exit code from a running child');
  {$IFDEF UNIX}
  Status := AProcess.ExitStatus;
  // ExitStatus is a waitpid status word on Unix, not the child's exit code.
  // FPC 3.2.2 ExitCode alone returns zero for a signal, so validate it first.
  if not WIFEXITED(Status) then
  begin
    if WIFSIGNALED(Status) then
      raise EWfcProcessAbnormalExit.CreateFmt(
        'child terminated by signal %d (wait status %d)',
        [WTERMSIG(Status), Status]);
    raise EWfcProcessAbnormalExit.CreateFmt(
      'child did not exit normally (wait status %d)', [Status]);
  end;
  {$ENDIF}
  Result := AProcess.ExitCode;
end;

end.
