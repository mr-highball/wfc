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
unit neighborhood_count_demo;

{$mode delphi}{$H+}

interface

uses SysUtils, wfc;

type
  TCountDemoInts = array of Integer;
  TCountDemoConfig = record
    Width, Height: Integer;
    Seed: TGraphSeed;
    Wrap: Boolean;
    Mode: TGraphPassCountMode;
    MinimumRoads, MaximumRoads, MaximumWater: Integer;
    Water: TCountDemoInts;
    RoadLocks: TCountDemoInts; { -1 auto, 0 empty, 1 road }
  end;
  TCountDemoResult = record
    Solved, TerrainReused: Boolean;
    RoadCount, WaterCount, PassBacktracks: Integer;
    Terrain, Roads, Market: String;
    Status, Detail, OutputKey: String;
  end;

function CountDemoPreset(const AName: String): TCountDemoConfig;
function CountDemoTarget(const AConfig: TCountDemoConfig): Integer;
function SolveCountDemo(const AConfig: TCountDemoConfig;
  const ARepair: Boolean; const APassBudget: Integer = 16): TCountDemoResult;
function CountDemoSelfTest: Integer;

implementation

procedure Require(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

function CountDemoTarget(const AConfig: TCountDemoConfig): Integer;
begin
  Result := (AConfig.Height div 2) * AConfig.Width +
    ((AConfig.Width - 1) div 2);
end;

function CountDemoPreset(const AName: String): TCountDemoConfig;
var I: Integer;
begin
  Result := Default(TCountDemoConfig);
  Result.Width := 3;
  Result.Height := 3;
  Result.Mode := gpcmDistinctCells;
  Result.MinimumRoads := 2;
  Result.MaximumRoads := 2;
  Result.MaximumWater := 1;
  SetLength(Result.Water, 9);
  SetLength(Result.RoadLocks, 9);
  for I := 0 to 8 do Result.RoadLocks[I] := 0;
  Result.RoadLocks[1] := 1;
  Result.RoadLocks[3] := 1;
  if AName = 'two' then Exit;
  if AName = 'lower' then Result.RoadLocks[3] := 0
  else if AName = 'upper' then Result.RoadLocks[5] := 1
  else if AName = 'flood' then
  begin Result.Water[0] := 1; Result.Water[2] := 1; end
  else if AName = 'repair' then
  begin Result.RoadLocks[3] := -1; Result.Seed := 3; end
  else if AName = 'alias' then
  begin
    Result.Width := 2; Result.Height := 1; Result.Wrap := True;
    SetLength(Result.Water, 2); SetLength(Result.RoadLocks, 2);
    Result.RoadLocks[0] := 0; Result.RoadLocks[1] := 1;
    Result.Mode := gpcmMatchingTerms;
  end
  else raise Exception.Create('unknown count demo preset');
end;

function RoadTerms: TGraphPassMatchTerms;
begin
  SetLength(Result, 4);
  Result[0] := MakeGraphPassMatchTerm(MakeGraphOffset(-1, 0, 0), ['road']);
  Result[1] := MakeGraphPassMatchTerm(MakeGraphOffset(0, -1, 0), ['road']);
  Result[2] := MakeGraphPassMatchTerm(MakeGraphOffset(0, 1, 0), ['road']);
  Result[3] := MakeGraphPassMatchTerm(MakeGraphOffset(1, 0, 0), ['road']);
end;

function WaterTerms: TGraphPassMatchTerms;
var X, Y, I: Integer;
begin
  SetLength(Result, 8); I := 0;
  for X := -1 to 1 do
    for Y := -1 to 1 do
      if (X <> 0) or (Y <> 0) then
      begin
        Result[I] := MakeGraphPassMatchTerm(MakeGraphOffset(X, Y, 0), ['water']);
        Inc(I);
      end;
end;

{ Independent result check: no core clause evaluator or term builder is used.
  A small explicitly drawn stencil is sampled directly from captured public text. }
function CountCaptured(const AConfig: TCountDemoConfig; const AText: String;
  const AValue: Char; const ARing: Boolean;
  const AMode: TGraphPassCountMode): Integer;
var X, Y, DX, DY, I, Target: Integer; Seen: array of Boolean;
begin
  Result := 0;
  SetLength(Seen, Length(AText));
  Target := CountDemoTarget(AConfig);
  for DX := -1 to 1 do
    for DY := -1 to 1 do
    begin
      if ((DX = 0) and (DY = 0)) or
          ((not ARing) and (Abs(DX) + Abs(DY) <> 1)) then Continue;
      X := (Target mod AConfig.Width) + DX;
      Y := (Target div AConfig.Width) + DY;
      if AConfig.Wrap then
      begin
        X := ((X mod AConfig.Width) + AConfig.Width) mod AConfig.Width;
        Y := ((Y mod AConfig.Height) + AConfig.Height) mod AConfig.Height;
      end
      else if (X < 0) or (Y < 0) or (X >= AConfig.Width) or
          (Y >= AConfig.Height) then Continue;
      I := Y * AConfig.Width + X;
      if AText[I + 1] <> AValue then Continue;
      if (AMode = gpcmDistinctCells) and Seen[I] then Continue;
      Seen[I] := True;
      Inc(Result);
    end;
end;

procedure ValidateCaptured(const C: TCountDemoConfig; var R: TCountDemoResult);
var I: Integer;
begin
  for I := 0 to C.Width * C.Height - 1 do
  begin
    Require((C.Water[I] = 1) = (R.Terrain[I + 1] = 'W'),
      'captured terrain differs from caller input');
    Require(not ((R.Terrain[I + 1] = 'W') and (R.Roads[I + 1] = 'R')),
      'a road occupies water');
    if C.RoadLocks[I] >= 0 then
      Require((C.RoadLocks[I] = 1) = (R.Roads[I + 1] = 'R'),
        'captured road differs from a caller lock');
    Require((I = CountDemoTarget(C)) = (R.Market[I + 1] = 'M'),
      'the market probe moved');
  end;
  I := CountDemoTarget(C) + 1;
  Require((R.Terrain[I] = 'L') and (R.Roads[I] = '.'),
    'market foundation must be empty land');
  R.RoadCount := CountCaptured(C, R.Roads, 'R', False, C.Mode);
  R.WaterCount := CountCaptured(C, R.Terrain, 'W', True, gpcmDistinctCells);
  Require((R.RoadCount >= C.MinimumRoads) and
    (R.RoadCount <= C.MaximumRoads), 'independent road count failed');
  Require(R.WaterCount <= C.MaximumWater, 'independent water count failed');
end;

function SolveCountDemo(const AConfig: TCountDemoConfig;
  const ARepair: Boolean; const APassBudget: Integer): TCountDemoResult;
var
  G: TGraph;
  O: TGraphSolveOptions;
  N: TGraphNegotiationOptions;
  S: TGraphSelectiveNegotiationReport;
  R: TGraphSolveReport;
  I, X, Y, Target: Integer;
  C: TCountDemoConfig;
  E: TGraphEntry;
begin
  Result := Default(TCountDemoResult);
  Result.RoadCount := -1; Result.WaterCount := -1;
  C := AConfig;
  Require(APassBudget >= 0, 'pass budget must be nonnegative');
  Require((C.Width > 0) and (C.Width <= 9) and (C.Height > 0) and
    (C.Height <= 9), 'this inspection demo supports dimensions 1..9');
  Require((Length(C.Water) = C.Width * C.Height) and
    (Length(C.RoadLocks) = Length(C.Water)), 'input grid shape differs');
  Require((C.MinimumRoads >= 0) and (C.MaximumRoads <= 4) and
    (C.MinimumRoads <= C.MaximumRoads), 'road range must lie within 0..4');
  Require((C.MaximumWater >= 0) and (C.MaximumWater <= 8),
    'water maximum must lie within 0..8');
  for I := 0 to High(C.Water) do
    Require((C.Water[I] in [0,1]) and (C.RoadLocks[I] >= -1) and
      (C.RoadLocks[I] <= 1), 'invalid input cell');
  G := TGraph.Create;
  try
    G.Seed := C.Seed;
    G.Reshape(C.Width, C.Height, 1);
    G.WrapNeighbors := C.Wrap;
    G.CurrentPass := 'terrain';
    G.PassMode := gpmOverlay;
    for I := 0 to High(C.Water) do
    begin
      X := I mod C.Width; Y := I div C.Width;
      if C.Water[I] = 1 then G.Entry[X,Y,0].Value := 'water'
      else G.Entry[X,Y,0].Value := 'land';
    end;
    G.SwitchToPass('roads'); G.PassMode := gpmOverlay; G.ClearDependencies;
    G.AddValue('empty');
    G.AddValue('road').RequireFromPass('terrain', 'land');
    for I := 0 to High(C.RoadLocks) do
      if C.RoadLocks[I] >= 0 then
      begin
        X := I mod C.Width; Y := I div C.Width;
        if C.RoadLocks[I] = 0 then G.Entry[X,Y,0].Value := 'empty'
        else G.Entry[X,Y,0].Value := 'road';
      end;
    G.SwitchToPass('market'); G.PassMode := gpmOverlay; G.ClearDependencies;
    G.AddValue('empty');
    G.AddValue('market').RequireFromPass('terrain', 'land')
      .RequireFromPass('roads', 'empty')
      .RequireCountFromPass('roads', RoadTerms, C.MinimumRoads,
        C.MaximumRoads, C.Mode)
      .RequireCountFromPass('terrain', WaterTerms, 0, C.MaximumWater,
        gpcmDistinctCells);
    Target := CountDemoTarget(C);
    for I := 0 to High(C.Water) do
    begin
      X := I mod C.Width; Y := I div C.Width;
      if I = Target then G.Entry[X,Y,0].Value := 'market'
      else G.Entry[X,Y,0].Value := 'empty';
    end;
    O := DefaultGraphSolveOptions; O.MaxBacktracks := 64;
    if ARepair then
    begin
      N := DefaultGraphNegotiationOptions;
      N.SolveOptions := O; N.MaxPassBacktracks := APassBudget;
      Result.Solved := G.TryRegenerateNegotiatedFrom('roads', N, S);
      R := S.Search.FinalReport;
      Result.PassBacktracks := S.Search.PassBacktracks;
      Result.TerrainReused := (Length(R.Passes) = 3) and
        (R.Passes[0].Disposition = gpdReused);
    end
    else Result.Solved := G.TrySolve(O, R);
    if not Result.Solved then
    begin
      if ARepair then
      begin
        if S.Search.Status = gnsContradiction then Result.Status := 'contradiction'
        else Result.Status := 'limit';
      end
      else if R.Status = gssContradiction then Result.Status := 'contradiction'
      else Result.Status := 'limit';
      Result.Detail := 'No generated layers committed. Failure pass=' +
        IntToStr(R.FailedPassIndex) + ', provider=' +
        IntToStr(R.Contradiction.DependencyPassIndex) + '.';
      { Only configured input locks may survive a failed transaction. }
      for I := 0 to High(C.RoadLocks) do
        if C.RoadLocks[I] = -1 then
          Require(G.PassGraph[1].Entry[I mod C.Width,I div C.Width,0].Empty,
            'failed solve leaked a generated road');
      Exit;
    end;
    if ARepair then Require(Result.TerrainReused,
      'selective repair did not report its immutable terrain provider');
    for I := 0 to High(C.Water) do
    begin
      X := I mod C.Width; Y := I div C.Width;
      E := G.PassGraph[0].Entry[X,Y,0];
      Require(not E.Empty and ((E.Value = 'water') or (E.Value = 'land')),
        'terrain capture is empty or contains an unknown value');
      if E.Value = 'water' then
        Result.Terrain := Result.Terrain + 'W'
      else Result.Terrain := Result.Terrain + 'L';
      E := G.PassGraph[1].Entry[X,Y,0];
      Require(not E.Empty and ((E.Value = 'road') or (E.Value = 'empty')),
        'road capture is empty or contains an unknown value');
      if E.Value = 'road' then
        Result.Roads := Result.Roads + 'R'
      else Result.Roads := Result.Roads + '.';
      E := G.PassGraph[2].Entry[X,Y,0];
      Require(not E.Empty and ((E.Value = 'market') or (E.Value = 'empty')),
        'market capture is empty or contains an unknown value');
      if E.Value = 'market' then
        Result.Market := Result.Market + 'M'
      else Result.Market := Result.Market + '.';
    end;
    ValidateCaptured(C, Result);
    Result.OutputKey := Result.Terrain + '/' + Result.Roads + '/' + Result.Market;
    Result.Status := 'solved';
    Result.Detail := 'Road count=' + IntToStr(Result.RoadCount) +
      '; water count=' + IntToStr(Result.WaterCount) +
      '; pass backtracks=' + IntToStr(Result.PassBacktracks) + '.';
  finally G.Free; end;
end;

function CountDemoSelfTest: Integer;
var C: TCountDemoConfig; R: TCountDemoResult;
  procedure Check(const B: Boolean; const M: String);
  begin Inc(Result); Require(B, 'count demo: ' + M); end;
begin
  Result := 0;
  C := CountDemoPreset('two'); R := SolveCountDemo(C, False);
  Check(R.Solved and (R.RoadCount = 2), 'two distinct roads solve');
  Check(R.OutputKey = 'LLLLLLLLL/.R.R...../....M....', 'portable output key');
  R := SolveCountDemo(CountDemoPreset('lower'), False);
  Check(not R.Solved and (R.Roads = ''), 'lower failure exposes no output');
  R := SolveCountDemo(CountDemoPreset('upper'), False);
  Check(not R.Solved and (R.Market = ''), 'upper failure exposes no output');
  R := SolveCountDemo(CountDemoPreset('flood'), False);
  Check(not R.Solved, 'second-provider water clause is conjunctive');
  C := CountDemoPreset('alias'); R := SolveCountDemo(C, False);
  Check(R.Solved and (R.RoadCount = 2), 'wrapped directional aliases count twice');
  C.Mode := gpcmDistinctCells; R := SolveCountDemo(C, False);
  Check(not R.Solved, 'wrapped cell aliases count once');
  C.MinimumRoads := 1; C.MaximumRoads := 1; R := SolveCountDemo(C, False);
  Check(R.Solved and (R.RoadCount = 1), 'distinct wrapped threshold recovers');
  C := CountDemoPreset('repair'); R := SolveCountDemo(C, False);
  Check((not R.Solved) and (R.Status = 'contradiction'),
    'seed three exercises a one-way contradiction');
  R := SolveCountDemo(C, True, 0);
  Check((not R.Solved) and (R.Status = 'limit'),
    'pass-budget exhaustion is not mislabeled as a proved contradiction');
  R := SolveCountDemo(C, True);
  Check(R.Solved and (R.PassBacktracks = 1) and R.TerrainReused,
    'selective repair reopens roads while terrain remains reused');
  R := SolveCountDemo(CountDemoPreset('two'), False);
  Check(R.Solved, 'fresh recovery after failed experiments');
end;

end.
