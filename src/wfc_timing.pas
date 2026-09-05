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
unit wfc_timing;

{$mode delphi}{$H+}

interface

{ Diagnostic clock readings never participate in deterministic solve choices,
  budgets, replay identity, or canonical artifacts. A zero interval is valid.
  False means unavailable, not a zero-duration measurement. }
function TryReadWfcMonotonicMilliseconds(out AValue: Double): Boolean;
function WfcElapsedMilliseconds(const AStart, AFinish: Double;
  out AElapsed: Double): Boolean;

implementation

uses
  SysUtils, Math
  {$IFDEF PAS2JS}, Web{$ENDIF};

const
  { Both hosts can exactly represent integral millisecond readings here. }
  MAX_EXACT_MILLISECONDS = 9007199254740991.0;

function ValidReading(const AValue: Double): Boolean;
begin
  Result := (not IsNan(AValue)) and (not IsInfinite(AValue));
  if Result then
    Result := (AValue >= 0) and (AValue <= MAX_EXACT_MILLISECONDS);
end;

function TryReadWfcMonotonicMilliseconds(out AValue: Double): Boolean;
{$IFNDEF PAS2JS}
var LTicks: QWord;
{$ENDIF}
begin
  Result := False;
  AValue := 0;
  try
    {$IFDEF PAS2JS}
    if not Assigned(Window.Performance) then Exit;
    AValue := Window.Performance.Now;
    {$ELSE}
    LTicks := GetTickCount64;
    if LTicks > QWord(9007199254740991) then Exit;
    AValue := LTicks;
    {$ENDIF}
    Result := ValidReading(AValue);
    if not Result then AValue := 0;
  except
    { An unavailable host clock must not invalidate a generated result. }
    AValue := 0;
    Result := False;
  end;
end;

function WfcElapsedMilliseconds(const AStart, AFinish: Double;
  out AElapsed: Double): Boolean;
begin
  Result := False;
  AElapsed := 0;
  if not ValidReading(AStart) or not ValidReading(AFinish) then Exit;
  if AFinish < AStart then Exit;
  AElapsed := AFinish - AStart;
  Result := True;
end;

end.
