{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_solver_benchmark;

{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL This timing benchmark requires native FPC}{$ENDIF}

uses SysUtils, Math, wfc_solver_reference, wfc_browser_socket;

{ Reproducible whole-solve timing, not a complexity-derived estimate. This
  deliberately has no pass/fail wall-time threshold. The same program can be
  compiled against an archived solver using an earlier -Fu search directory.
  Model construction and result checksums are outside each timed interval;
  validation, solver allocation, propagation, decisions, tracing, reporting,
  and solver destruction are inside. No sockets are opened by this program. }

type
  TOptions = record
    Cells, Values, Repeats: Integer;
    Weighted, Line, EqualOnly, Trace: Boolean;
  end;
  TDraws = class
  public
    State, Count: Integer;
    function Next(const ACount: Integer): Integer;
  end;

function TDraws.Next(const ACount: Integer): Integer;
var P: Double;
begin
  P := State; P := P * 16807.0;
  State := Trunc(P - Floor(P / 2147483647.0) * 2147483647.0);
  Result := State mod ACount; Inc(Count);
end;

function BoundedInteger(const S: String; const Maximum: Integer): Integer;
begin
  if not TryStrToInt(S, Result) or (Result < 1) or (Result > Maximum) or
    (IntToStr(Result) <> S) then
    raise EConvertError.CreateFmt('expected canonical integer 1..%d, got "%s"', [Maximum, S]);
end;

function Choice(const S, FalseName, TrueName: String): Boolean;
begin
  if S = FalseName then Exit(False);
  if S = TrueName then Exit(True);
  raise EConvertError.CreateFmt('expected %s or %s, got "%s"', [FalseName, TrueName, S]);
end;

procedure Help;
begin
  WriteLn('wfc-solver-benchmark 1');
  WriteLn('Usage: wfc_solver_benchmark [OPTION VALUE] ...');
  WriteLn('  --cells N                     default 256; 1..262144');
  WriteLn('  --values N                    default 4; 1..1024');
  WriteLn('  --weights unit|skewed         default unit');
  WriteLn('  --topology independent|line   default independent');
  WriteLn('  --compatibility dense|equal   default dense');
  WriteLn('  --trace 0|1                   default 0');
  WriteLn('  --repeat N                    default 1; 1..1000');
  WriteLn('  --help or --version           alone');
  WriteLn('Bounds are benchmark safety limits, not library or composition limits.');
  WriteLn('Monotonic whole-solve milliseconds; no timing pass/fail threshold.');
  WriteLn('Model setup and result checksum are excluded; solver work is included.');
  WriteLn('Skewed weights are 1 + value-index squared; seed resets to 1 per solve.');
  WriteLn('Equal compatibility constrains line neighbors; independent has no arcs.');
end;

function Options: TOptions;
var I, K: Integer; Name, Value: String; Seen: array[0..6] of Boolean;
begin
  Result := Default(TOptions); Result.Cells := 256; Result.Values := 4;
  Result.Repeats := 1;
  for K := 0 to High(Seen) do Seen[K] := False;
  I := 1;
  while I <= ParamCount do
  begin
    Name := ParamStr(I);
    if I = ParamCount then raise EConvertError.Create('missing value for ' + Name);
    Value := ParamStr(I + 1);
    if Name = '--cells' then K := 0
    else if Name = '--values' then K := 1
    else if Name = '--weights' then K := 2
    else if Name = '--topology' then K := 3
    else if Name = '--compatibility' then K := 4
    else if Name = '--trace' then K := 5
    else if Name = '--repeat' then K := 6
    else raise EConvertError.Create('unknown option ' + Name);
    if Seen[K] then raise EConvertError.Create('duplicate option ' + Name);
    Seen[K] := True;
    case K of
      0: Result.Cells := BoundedInteger(Value, 262144);
      1: Result.Values := BoundedInteger(Value, 1024);
      2: Result.Weighted := Choice(Value, 'unit', 'skewed');
      3: Result.Line := Choice(Value, 'independent', 'line');
      4: Result.EqualOnly := Choice(Value, 'dense', 'equal');
      5: Result.Trace := Choice(Value, '0', '1');
      6: Result.Repeats := BoundedInteger(Value, 1000);
    end;
    Inc(I, 2);
  end;
end;

procedure Model(const O: TOptions; out M: TReferenceModel);
var I, D, A, B: Integer;
begin
  M := Default(TReferenceModel); M.CellCount := O.Cells; M.ValueCount := O.Values;
  SetLength(M.Neighbors, O.Cells * 6);
  for I := 0 to High(M.Neighbors) do M.Neighbors[I] := -1;
  if O.Line then for I := 0 to O.Cells - 2 do
  begin M.Neighbors[I * 6] := I + 1; M.Neighbors[(I + 1) * 6 + 2] := I; end;
  SetLength(M.Compatibility, 6 * O.Values * O.Values);
  for D := 0 to 5 do for A := 0 to O.Values - 1 do for B := 0 to O.Values - 1 do
    M.Compatibility[(D * O.Values + A) * O.Values + B] := Ord(not O.EqualOnly or (A = B));
  SetLength(M.RequiredValues, O.Values);
  SetLength(M.RequiredSupport, Length(M.Compatibility));
  SetLength(M.InitialAllowed, O.Cells * O.Values);
  for I := 0 to High(M.InitialAllowed) do M.InitialAllowed[I] := 1;
  SetLength(M.InitialFailureKinds, O.Cells);
  SetLength(M.LockedValues, O.Cells); SetLength(M.CellOrder, O.Cells);
  for I := 0 to O.Cells - 1 do
  begin
    M.InitialFailureKinds[I] := rckEntryDomain;
    M.LockedValues[I] := -1; M.CellOrder[I] := I;
  end;
  if O.Weighted then
  begin
    SetLength(M.ValueWeights, O.Values);
    for I := 0 to O.Values - 1 do M.ValueWeights[I] := 1 + I * I;
  end;
end;

function AssignmentChecksum(const A: TReferenceIntegerArray): Integer;
var I: Integer; P: Double;
begin
  Result := 104729;
  for I := 0 to High(A) do
  begin
    P := Result; P := P * 65599.0 + A[I] + 1;
    Result := Trunc(P - Floor(P / 2147483647.0) * 2147483647.0);
  end;
end;

procedure Benchmark(const O: TOptions);
var M: TReferenceModel; A: TReferenceIntegerArray; R: TReferenceSolveReport;
  Draws: TDraws; I, Hash, ExpectedHash, ExpectedDraws: Integer;
  Started, Elapsed, Total, Minimum, Maximum: QWord; Solved: Boolean;
begin
  Model(O, M); Total := 0; Minimum := High(QWord); Maximum := 0;
  ExpectedHash := -1; ExpectedDraws := -1; Draws := TDraws.Create;
  try
    for I := 1 to O.Repeats do
    begin
      { Release previous retained output before, not during, the next solve. }
      A := nil; R := Default(TReferenceSolveReport);
      Draws.State := 1; Draws.Count := 0;
      Started := WfcBrowserTickCount64;
      Solved := SolveReferenceModel(M, 0, O.Trace, Draws.Next, A, R);
      Elapsed := WfcBrowserTickCount64 - Started;
      if not Solved or (R.Status <> rssSolved) or (Length(A) <> O.Cells) then
        raise Exception.Create('benchmark fixture unexpectedly failed to solve');
      Hash := AssignmentChecksum(A);
      if I = 1 then begin ExpectedHash := Hash; ExpectedDraws := Draws.Count; end
      else if (Hash <> ExpectedHash) or (Draws.Count <> ExpectedDraws) then
        raise Exception.Create('identical benchmark repetitions diverged');
      Inc(Total, Elapsed);
      if Elapsed < Minimum then Minimum := Elapsed;
      if Elapsed > Maximum then Maximum := Elapsed;
    end;
    WriteLn('wfc-solver-benchmark=1');
    WriteLn('clock=monotonic-milliseconds');
    WriteLn('timed=whole-solve');
    WriteLn('cells=', O.Cells); WriteLn('values=', O.Values);
    if O.Weighted then WriteLn('weights=skewed') else WriteLn('weights=unit');
    if O.Line then WriteLn('topology=line') else WriteLn('topology=independent');
    if O.EqualOnly then WriteLn('compatibility=equal') else WriteLn('compatibility=dense');
    WriteLn('trace=', Ord(O.Trace)); WriteLn('repeat=', O.Repeats);
    WriteLn('elapsed-ms=', Total); WriteLn('minimum-ms=', Minimum);
    WriteLn('maximum-ms=', Maximum); WriteLn('solved=true');
    WriteLn('decisions-per-solve=', R.Decisions);
    WriteLn('propagations-per-solve=', R.Propagations);
    WriteLn('backtracks-per-solve=', R.Backtracks);
    WriteLn('contradictions-per-solve=', R.Contradictions);
    WriteLn('trace-events-per-solve=', Length(R.Trace));
    WriteLn('rng-draws-per-solve=', ExpectedDraws);
    WriteLn('assignment-checksum=', IntToHex(ExpectedHash, 8));
  finally Draws.Free; end;
end;

function Diagnostic(const S: String): String;
var I, N: Integer;
begin
  N := Length(S); if N > 4096 then N := 4096;
  Result := Copy(S, 1, N);
  for I := 1 to N do
    if (Ord(Result[I]) < 32) or (Ord(Result[I]) = 127) then Result[I] := ' ';
end;

begin
  if (ParamCount = 1) and (ParamStr(1) = '--version') then
  begin WriteLn('wfc-solver-benchmark 1'); Halt(0); end;
  if (ParamCount = 1) and (ParamStr(1) = '--help') then
  begin Help; Halt(0); end;
  try Benchmark(Options);
  except
    on E: EConvertError do
    begin WriteLn(StdErr, 'wfc-solver-benchmark: usage error: ', Diagnostic(E.Message)); Halt(2); end;
    on E: Exception do
    begin WriteLn(StdErr, 'wfc-solver-benchmark: failed: ', Diagnostic(E.Message)); Halt(1); end;
  end;
end.
