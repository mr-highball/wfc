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
program wfc_timing_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, Math, wfc_timing;

var
  Checks: Integer;
  Elapsed, First, Last: Double;
  FirstAvailable, LastAvailable: Boolean;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then
    raise Exception.Create('timing check failed: ' + AMessage);
  WriteLn('[PASS] ', AMessage);
end;

procedure RejectInterval(const AStart, AFinish: Double;
  const AMessage: String);
begin
  Elapsed := 123;
  Check((not WfcElapsedMilliseconds(AStart, AFinish, Elapsed)) and
    (Elapsed = 0), AMessage);
end;

begin
  Check(WfcElapsedMilliseconds(0, 0, Elapsed) and (Elapsed = 0),
    'zero is a valid measured interval');
  Check(WfcElapsedMilliseconds(100, 100, Elapsed) and (Elapsed = 0),
    'coarse clock resolution is not unavailability');
  Check(WfcElapsedMilliseconds(12.25, 14.75, Elapsed) and (Elapsed = 2.5),
    'fractional browser milliseconds are preserved');
  Check(WfcElapsedMilliseconds(9007199254740990.0, 9007199254740991.0,
    Elapsed) and (Elapsed = 1), 'exact clock range boundary');
  RejectInterval(10, 9, 'backward clock readings are unavailable');
  RejectInterval(-1, 9, 'negative start is unavailable');
  RejectInterval(0, -1, 'negative finish is unavailable');
  RejectInterval(0, 9007199254740992.0,
    'inexact integral-millisecond range is unavailable');
  RejectInterval(NaN, 1, 'NaN start is unavailable');
  RejectInterval(0, NaN, 'NaN finish is unavailable');
  RejectInterval(Infinity, Infinity, 'infinite interval is unavailable');
  RejectInterval(0, Infinity, 'infinite finish is unavailable');
  RejectInterval(NegInfinity, 1, 'negative infinity is unavailable');
  FirstAvailable := TryReadWfcMonotonicMilliseconds(First);
  LastAvailable := TryReadWfcMonotonicMilliseconds(Last);
  Check(FirstAvailable or (First = 0), 'unavailable first reading is explicit');
  Check(LastAvailable or (Last = 0), 'unavailable final reading is explicit');
  if FirstAvailable and LastAvailable then
    Check(WfcElapsedMilliseconds(First, Last, Elapsed) and (Elapsed >= 0),
      'host clock produces a valid nonnegative diagnostic interval')
  else
    WriteLn('Host monotonic timing unavailable; deterministic work is unaffected.');
  WriteLn(Checks, ' checks, 0 failures');
end.
