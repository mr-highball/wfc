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
unit wfc_connectivity_reference;

{$mode delphi}{$H+}

interface

//Numeric solver implementation detail, not a stable application API. A
//possible edge is an over-approximation: its two values need not be the same
//witnesses as the values supporting other edges at either endpoint. This
//supports sound reachability/bottleneck pruning, not generalized arc
//consistency. Complete assignments receive a separate exact traversal.
type
  TReferenceIntegerArray = array of Integer;
  TReferenceByteArray = array of Byte;
  TReferenceConnectivityValueProfile = record
    ValueIndex: Integer;
    //Bits north, east, south, west, up, down, respectively. Zero ports is a
    //participating isolated value; an absent profile is a nonparticipant.
    Ports: Byte;
    RequiredByValue: Boolean;
  end;
  TReferenceConnectivityValueProfiles = array of TReferenceConnectivityValueProfile;
  TReferenceConnectivityConstraint = record
    RootCell: Integer;
    RequiredCells: TReferenceIntegerArray;
    Profiles: TReferenceConnectivityValueProfiles;
    RequireAllParticipants: Boolean;
  end;
  TReferenceConnectivityConstraints = array of TReferenceConnectivityConstraint;

  TReferenceConnectivityAnalysis = record
    //Borrowed scratch until the analyzer's next call; the solver consumes
    //these immediately and never exposes them in a public solve report.
    Reachable: TReferenceByteArray;
    ForceParticipation: TReferenceByteArray;
    FailureCell: Integer;
  end;

  TReferenceConnectivityAnalyzer = class
  strict private
    FCellCount, FValueCount, FRoot: Integer;
    FNeighbors: TReferenceIntegerArray;
    FCompatibility: TReferenceByteArray;
    FPorts: TReferenceIntegerArray;
    FRequired, FFixed, FMandatory, FEdges, FReachable,
      FForce: TReferenceByteArray;
    FDiscovery, FLow, FParent, FNext, FSubtreeRequired,
      FStack: TReferenceIntegerArray;
    function PairConnects(const ACell, ADirection, AValue,
      ANeighborValue: Integer): Boolean;
  public
    //The enclosing numeric model must have passed validation. Neighbor and
    //compatibility arrays are borrowed immutable for the solver lifetime.
    constructor Create(const ACellCount, AValueCount: Integer;
      const ANeighbors: TReferenceIntegerArray;
      const ACompatibility: TReferenceByteArray;
      const AConstraint: TReferenceConnectivityConstraint);
    function Participates(const AValue: Integer): Boolean;
    function RequiresConnection(const AValue: Integer): Boolean;
    procedure Analyze(const ADomains: TReferenceByteArray;
      out AAnalysis: TReferenceConnectivityAnalysis);
    function ValidateComplete(const AAssignment: TReferenceIntegerArray;
      out AFailureCell: Integer): Boolean;
  end;
  TReferenceConnectivityAnalyzers = array of TReferenceConnectivityAnalyzer;

procedure RequireReferenceConnectivityInteger(const AValue, AMinimum,
  AMaximum: Integer; const AName: String);
procedure ValidateReferenceConnectivity(const ACellCount, AValueCount: Integer;
  const AConstraints: TReferenceConnectivityConstraints);

implementation

uses Classes, SysUtils;

const
  DIRECTION_COUNT = 6;
  OPPOSITE: array[0..5] of Integer = (2, 3, 0, 1, 5, 4);

procedure RequireReferenceConnectivityInteger(const AValue, AMinimum,
  AMaximum: Integer; const AName: String);
begin
  if not ((AValue >= AMinimum) and (AValue <= AMaximum)) then
    raise ERangeError.Create(AName + ' is out of bounds');
  {$IFDEF PAS2JS}
  if AValue <> Trunc(AValue) then
    raise ERangeError.Create(AName + ' must be an exact integer');
  {$ENDIF}
end;

procedure RequireBoolean(const AValue: Boolean; const AName: String);
begin
  {$IFDEF PAS2JS}
  if (AValue <> False) and (AValue <> True) then
  {$ELSE}
  if Ord(AValue) > 1 then
  {$ENDIF}
    raise ERangeError.Create(AName + ' must be Boolean');
end;

procedure ValidateReferenceConnectivity(const ACellCount, AValueCount: Integer;
  const AConstraints: TReferenceConnectivityConstraints);
var I, J, K: Integer;
begin
  if Length(AConstraints) = 0 then Exit;
  RequireReferenceConnectivityInteger(ACellCount, 1, High(Integer),
    'connectivity cell count');
  RequireReferenceConnectivityInteger(AValueCount, 1, High(Integer),
    'connectivity value count');
  for I := 0 to High(AConstraints) do
  begin
    RequireReferenceConnectivityInteger(AConstraints[I].RootCell, 0,
      ACellCount - 1, 'connectivity root');
    RequireBoolean(AConstraints[I].RequireAllParticipants,
      'connectivity all-participants flag');
    if Length(AConstraints[I].Profiles) = 0 then
      raise EInvalidOperation.Create('connectivity needs participating value profiles');
    if Length(AConstraints[I].Profiles) > AValueCount then
      raise EInvalidOperation.Create('connectivity has too many value profiles');
    for J := 0 to High(AConstraints[I].RequiredCells) do
      RequireReferenceConnectivityInteger(AConstraints[I].RequiredCells[J], 0,
        ACellCount - 1, 'connectivity required cell');
    for J := 0 to High(AConstraints[I].Profiles) do
    begin
      RequireReferenceConnectivityInteger(AConstraints[I].Profiles[J].ValueIndex,
        0, AValueCount - 1, 'connectivity profile value');
      RequireReferenceConnectivityInteger(AConstraints[I].Profiles[J].Ports,
        0, 63, 'connectivity profile ports');
      RequireBoolean(AConstraints[I].Profiles[J].RequiredByValue,
        'connectivity required-by-value flag');
      for K := 0 to J - 1 do
        if AConstraints[I].Profiles[K].ValueIndex =
          AConstraints[I].Profiles[J].ValueIndex then
          raise EInvalidOperation.Create('connectivity repeats a value profile');
    end;
  end;
end;

constructor TReferenceConnectivityAnalyzer.Create(const ACellCount,
  AValueCount: Integer; const ANeighbors: TReferenceIntegerArray;
  const ACompatibility: TReferenceByteArray;
  const AConstraint: TReferenceConnectivityConstraint);
var I, V: Integer;
begin
  inherited Create;
  FCellCount := ACellCount;
  FValueCount := AValueCount;
  FRoot := AConstraint.RootCell;
  FNeighbors := ANeighbors;
  FCompatibility := ACompatibility;
  SetLength(FPorts, FValueCount);
  SetLength(FRequired, FValueCount);
  for I := 0 to FValueCount - 1 do FPorts[I] := -1;
  for I := 0 to High(AConstraint.Profiles) do
  begin
    V := AConstraint.Profiles[I].ValueIndex;
    FPorts[V] := AConstraint.Profiles[I].Ports;
    if AConstraint.RequireAllParticipants or
      AConstraint.Profiles[I].RequiredByValue then FRequired[V] := 1;
  end;
  SetLength(FFixed, FCellCount);
  FFixed[FRoot] := 1;
  for I := 0 to High(AConstraint.RequiredCells) do
    FFixed[AConstraint.RequiredCells[I]] := 1;
  SetLength(FMandatory, FCellCount);
  SetLength(FEdges, FCellCount * DIRECTION_COUNT);
  SetLength(FReachable, FCellCount);
  SetLength(FForce, FCellCount);
  SetLength(FDiscovery, FCellCount);
  SetLength(FLow, FCellCount);
  SetLength(FParent, FCellCount);
  SetLength(FNext, FCellCount);
  SetLength(FSubtreeRequired, FCellCount);
  SetLength(FStack, FCellCount);
end;

function TReferenceConnectivityAnalyzer.Participates(const AValue: Integer): Boolean;
begin Result := FPorts[AValue] >= 0; end;

function TReferenceConnectivityAnalyzer.RequiresConnection(const AValue: Integer): Boolean;
begin Result := FRequired[AValue] <> 0; end;

function TReferenceConnectivityAnalyzer.PairConnects(const ACell, ADirection,
  AValue, ANeighborValue: Integer): Boolean;
var Neighbor, Reverse: Integer;
begin
  Result := False;
  if (FPorts[AValue] < 0) or (FPorts[ANeighborValue] < 0) then Exit;
  Reverse := OPPOSITE[ADirection];
  if ((FPorts[AValue] and (1 shl ADirection)) = 0) or
    ((FPorts[ANeighborValue] and (1 shl Reverse)) = 0) then Exit;
  Neighbor := FNeighbors[ACell * DIRECTION_COUNT + ADirection];
  if (Neighbor < 0) or
    (FNeighbors[Neighbor * DIRECTION_COUNT + Reverse] <> ACell) then Exit;
  Result := (FCompatibility[(ADirection * FValueCount + AValue) *
    FValueCount + ANeighborValue] <> 0) and
    (FCompatibility[(Reverse * FValueCount + ANeighborValue) *
    FValueCount + AValue] <> 0);
end;

procedure TReferenceConnectivityAnalyzer.Analyze(const ADomains: TReferenceByteArray;
  out AAnalysis: TReferenceConnectivityAnalysis);
var C, D, N, V, W, Depth, Clock, P: Integer;
  HasCandidate, HasParticipant, EveryRequired, Found: Boolean;
begin
  AAnalysis.Reachable := FReachable;
  AAnalysis.ForceParticipation := FForce;
  AAnalysis.FailureCell := -1;
  for C := 0 to FCellCount - 1 do
  begin
    FReachable[C] := 0;
    FForce[C] := FFixed[C];
    FDiscovery[C] := 0;
    FLow[C] := 0;
    FParent[C] := -1;
    FNext[C] := 0;
    FSubtreeRequired[C] := 0;
    HasCandidate := False;
    HasParticipant := False;
    EveryRequired := True;
    for V := 0 to FValueCount - 1 do
      if ADomains[C * FValueCount + V] <> 0 then
      begin
        HasCandidate := True;
        if Participates(V) then HasParticipant := True;
        if not RequiresConnection(V) then EveryRequired := False;
      end;
    FMandatory[C] := 0;
    if (FFixed[C] <> 0) or (HasCandidate and EveryRequired) then
      FMandatory[C] := 1;
    if not HasCandidate or ((FFixed[C] <> 0) and not HasParticipant) then
    begin AAnalysis.FailureCell := C; Exit; end;
  end;

  //Edges are undirected physical-cell connections. Self aliases cannot
  //connect any new cell. Parallel wrapped directions remain harmless: the
  //DFS visits a vertex once, and parent-vertex edges do not alter cut tests.
  for C := 0 to FCellCount - 1 do
    for D := 0 to DIRECTION_COUNT - 1 do
    begin
      FEdges[C * DIRECTION_COUNT + D] := 0;
      N := FNeighbors[C * DIRECTION_COUNT + D];
      if (N < 0) or (N = C) then Continue;
      Found := False;
      for V := 0 to FValueCount - 1 do
      begin
        if ADomains[C * FValueCount + V] = 0 then Continue;
        for W := 0 to FValueCount - 1 do
          if (ADomains[N * FValueCount + W] <> 0) and
            PairConnects(C, D, V, W) then
          begin Found := True; Break; end;
        if Found then Break;
      end;
      if Found then FEdges[C * DIRECTION_COUNT + D] := 1;
    end;

  Depth := 0;
  Clock := 1;
  FStack[0] := FRoot;
  FDiscovery[FRoot] := Clock;
  FLow[FRoot] := Clock;
  FReachable[FRoot] := 1;
  FSubtreeRequired[FRoot] := FMandatory[FRoot];
  while Depth >= 0 do
  begin
    C := FStack[Depth];
    if FNext[C] < DIRECTION_COUNT then
    begin
      D := FNext[C];
      Inc(FNext[C]);
      if FEdges[C * DIRECTION_COUNT + D] = 0 then Continue;
      N := FNeighbors[C * DIRECTION_COUNT + D];
      if FDiscovery[N] = 0 then
      begin
        Inc(Clock);
        FDiscovery[N] := Clock;
        FLow[N] := Clock;
        FParent[N] := C;
        FReachable[N] := 1;
        FSubtreeRequired[N] := FMandatory[N];
        Inc(Depth);
        FStack[Depth] := N;
      end
      else if (N <> FParent[C]) and (FDiscovery[N] < FLow[C]) then
        FLow[C] := FDiscovery[N];
    end
    else
    begin
      Dec(Depth);
      P := FParent[C];
      if P >= 0 then
      begin
        if (FLow[C] >= FDiscovery[P]) and (FSubtreeRequired[C] <> 0) then
          FForce[P] := 1;
        //Only existence matters; using OR avoids subtree-size overflow.
        if FSubtreeRequired[C] <> 0 then FSubtreeRequired[P] := 1;
        if FLow[C] < FLow[P] then FLow[P] := FLow[C];
      end;
    end;
  end;
  for C := 0 to FCellCount - 1 do
    if (FMandatory[C] <> 0) and (FReachable[C] = 0) then
    begin AAnalysis.FailureCell := C; Exit; end;
end;

function TReferenceConnectivityAnalyzer.ValidateComplete(
  const AAssignment: TReferenceIntegerArray; out AFailureCell: Integer): Boolean;
var C, D, N, V, Head, Tail: Integer;
begin
  //Do not reuse the possible-graph edges, lowlinks, or mandatory analysis.
  //This exact BFS reads only the selected values and descriptor semantics.
  AFailureCell := -1;
  if Length(AAssignment) <> FCellCount then Exit(False);
  for C := 0 to FCellCount - 1 do
  begin
    FReachable[C] := 0;
    V := AAssignment[C];
    if (V < 0) or (V >= FValueCount) or
      ((FFixed[C] <> 0) and not Participates(V)) then
    begin AFailureCell := C; Exit(False); end;
  end;
  Head := 0;
  Tail := 1;
  FStack[0] := FRoot;
  FReachable[FRoot] := 1;
  while Head < Tail do
  begin
    C := FStack[Head];
    Inc(Head);
    for D := 0 to DIRECTION_COUNT - 1 do
    begin
      N := FNeighbors[C * DIRECTION_COUNT + D];
      if (N < 0) or (FReachable[N] <> 0) then Continue;
      if PairConnects(C, D, AAssignment[C], AAssignment[N]) then
      begin
        FReachable[N] := 1;
        FStack[Tail] := N;
        Inc(Tail);
      end;
    end;
  end;
  for C := 0 to FCellCount - 1 do
    if ((FFixed[C] <> 0) or RequiresConnection(AAssignment[C])) and
      (FReachable[C] = 0) then
    begin AFailureCell := C; Exit(False); end;
  Result := True;
end;

end.
