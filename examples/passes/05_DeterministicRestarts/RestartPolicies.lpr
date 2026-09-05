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
program RestartPolicies;

{$mode delphi}{$H+}

uses SysUtils, restart_policies_demo;

var
  LDemo: TRestartPoliciesDemoResult;
begin
  try
    if (ParamCount = 1) and (ParamStr(1) = '--selftest') then
      WriteLn('Restart policy demo checks: ', RestartPoliciesSelfTest)
    else if (ParamCount = 1) and (ParamStr(1) = '--timing') then
    begin
      LDemo := RunRestartPoliciesDemo(True);
      WriteLn(FormatRestartPoliciesDemo(LDemo));
    end
    else if ParamCount = 0 then
    begin
      LDemo := RunRestartPoliciesDemo(False);
      WriteLn(FormatRestartPoliciesDemo(LDemo));
    end
    else
      raise Exception.Create('expected no argument, --selftest, or --timing');
  except
    on E: Exception do
    begin
      WriteLn('RestartPolicies: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
