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
program NeighborhoodCounts;
{$mode delphi}{$H+}
uses SysUtils, neighborhood_count_demo;
var C: TCountDemoConfig; R: TCountDemoResult; Name: String;
begin
  try
    if (ParamCount = 1) and (ParamStr(1) = '--selftest') then
      WriteLn('Neighborhood count demo checks: ', CountDemoSelfTest)
    else
    begin
      if ParamCount > 2 then raise Exception.Create('expected [PRESET] [--repair]');
      if (ParamCount = 2) and (ParamStr(2) <> '--repair') then
        raise Exception.Create('second argument must be --repair');
      Name := 'two'; if ParamCount > 0 then Name := ParamStr(1);
      C := CountDemoPreset(Name);
      R := SolveCountDemo(C, ParamCount = 2);
      WriteLn('Count constraints v1: ', R.Status);
      WriteLn(R.Detail);
      if R.Solved then WriteLn(R.OutputKey) else ExitCode := 2;
    end;
  except on E: Exception do
    begin WriteLn('NeighborhoodCounts: ', E.Message); ExitCode := 1; end;
  end;
end.
