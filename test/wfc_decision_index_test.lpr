{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_decision_index_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, Math, wfc_solver_reference;

{ These are compatibility goldens, not a second implementation of selection.
  They were captured from the unmodified scan solver in commit e1b9194.
  DECISION_INDEX_CAPTURE prints replacement candidates for deliberate review;
  it must only be used with that frozen baseline, never to bless new behavior.
  DECISION_INDEX_TRANSCRIPT additionally emits every integer field and random
  request/result for direct, collision-free native differential comparison.
  The normal test uses dual bounded integer hashes so it also runs in pas2js. }

const
  GROUP_COUNT = 25;
  GROUP_NAMES: array[0..GROUP_COUNT - 1] of String = (
    'empty', 'singleton', 'independent-unit', 'explicit-unit-gcd',
    'weighted', 'weighted-gcd', 'shuffled-ties', 'weighted-domain-ties',
    'initial-domains', 'locks', 'empty-domains', 'conflicting-locks',
    'directed-arcs', 'self-neighbors', 'required-support', 'weighted-changes',
    'excluded-recovery', 'excluded-exhaustion', 'backtrack-limits',
    'connectivity', 'overlapping-quotas', 'connectivity-and-quotas',
    'generated-directed', 'generated-weighted', 'q16-count-collision');
  GOLDENS: array[0..GROUP_COUNT - 1] of String = (
    '56CA7081446D00CC', '5E811A173A3BB75D', '5A6748AF4A8791D4', '7D242C511CE5B7C6',
    '13BAEDE4578A1CB6', '50889F1C5F38F624', '50EF8ED772934D10', '21FF2EEF7CA1B9B0',
    '612C85CC7CE02EE5', '223EE8DF7B949F26', '4D5F65F070A39F8E', '2A90658A69C37BFB',
    '5B7A735A2CC15663', '6AC737CD1950EF1A', '58B4370B69C19746', '452FD13979BD0FC2',
    '6212618A241FE8E2', '02C709AD7D57F5FF', '02C566CD7D33DAC7', '5118ACC44B93B76C',
    '0FBD105134EEEB7F', '2E3CB09640D4E3BE', '6C1B29456F6F8FB6', '4FF094F25DFC4E70',
    '3942D3314512E3A1');

type
  TDigest = record A, B: Integer; end;
  TRecorder = class
  public
    State, DrawCount: Integer;
    Draws: String;
    Events: TReferenceTraceEvents;
    procedure Reset(const ASeed: Integer);
    function Draw(const ACount: Integer): Integer;
    procedure Event(const AEvent: TReferenceTraceEvent);
  end;

var
  Checks, Cases, SolvedCases, ContradictionCases, LimitCases: Integer;
  Backtracks, Restored, EntropyIncreases, EntropyDecreases: Integer;
  TraceKinds: array[TReferenceTraceEventKind] of Boolean;
  TraceCauses: array[TReferenceTraceCauseKind] of Boolean;

procedure Check(const B: Boolean; const MessageText: String);
begin
  Inc(Checks);
  if not B then raise Exception.Create(MessageText);
end;

procedure BeginDigest(out D: TDigest);
begin D.A := 104729; D.B := 130363; end;

procedure DigestText(var D: TDigest; const S: String);
var I: Integer; P: Double;
begin
  for I := 1 to Length(S) do
  begin
    { Each product is an exactly representable integer below 2^48. Avoid
      overflowing native signed integers and JavaScript bitwise coercion. }
    P := D.A; P := P * 65599.0 + Ord(S[I]);
    D.A := Trunc(P - Floor(P / 2147483647.0) * 2147483647.0);
    P := D.B; P := P * 65521.0 + Ord(S[I]);
    D.B := Trunc(P - Floor(P / 2147483629.0) * 2147483629.0);
  end;
end;

function DigestHex(const D: TDigest): String;
begin Result := IntToHex(D.A, 8) + IntToHex(D.B, 8); end;

procedure TRecorder.Reset(const ASeed: Integer);
begin State := ASeed; DrawCount := 0; Draws := ''; Events := nil; end;

function TRecorder.Draw(const ACount: Integer): Integer;
var P: Double;
begin
  if ACount <= 0 then raise Exception.Create('nonpositive random request');
  { Zero deliberately selects the first remaining alternative. Nonzero
    seeds use exact Park-Miller arithmetic independently of the library RNG. }
  P := State; P := P * 16807.0;
  State := Trunc(P - Floor(P / 2147483647.0) * 2147483647.0);
  Result := State mod ACount;
  Inc(DrawCount);
  Draws := Draws + IntToStr(ACount) + ':' + IntToStr(Result) + ';';
end;

procedure TRecorder.Event(const AEvent: TReferenceTraceEvent);
var N: Integer;
begin
  N := Length(Events); SetLength(Events, N + 1); Events[N] := AEvent;
end;

procedure Number(var S: String; const I: Integer);
begin S := S + IntToStr(I) + ','; end;

function EventText(const E: TReferenceTraceEvent): String;
begin
  Result := '';
  Number(Result, E.EventId); Number(Result, E.CauseEventId);
  Number(Result, Ord(E.Kind)); Number(Result, Ord(E.CauseKind));
  Number(Result, E.EntryIndex); Number(Result, E.ValueIndex);
  Number(Result, E.NeighborIndex); Number(Result, E.Direction);
  Number(Result, E.DecisionDepth); Number(Result, E.DomainCountBefore);
  Number(Result, E.DomainCountAfter); Number(Result, E.ConstraintIndex);
end;

function ReportText(const Solved: Boolean; const A: TReferenceIntegerArray;
  const R: TReferenceSolveReport; const IncludeTrace: Boolean): String;
var I: Integer;
begin
  Result := 'return='; Number(Result, Ord(Solved));
  Result := Result + 'assignment='; Number(Result, Length(A));
  for I := 0 to High(A) do Number(Result, A[I]);
  Result := Result + 'report=';
  Number(Result, Ord(R.Status)); Number(Result, R.Decisions);
  Number(Result, R.Propagations); Number(Result, R.Contradictions);
  Number(Result, R.Backtracks); Number(Result, R.ExcludedAssignments);
  Number(Result, Ord(R.Contradiction.Kind));
  Number(Result, R.Contradiction.EntryIndex);
  Number(Result, R.Contradiction.NeighborIndex);
  Number(Result, R.Contradiction.Direction);
  Number(Result, R.Contradiction.ConstraintIndex);
  if IncludeTrace then
  begin
    Result := Result + 'trace='; Number(Result, Length(R.Trace));
    for I := 0 to High(R.Trace) do Result := Result + EventText(R.Trace[I]) + ';';
  end;
end;

procedure Model(const Cells, Values: Integer; out M: TReferenceModel);
var I: Integer;
begin
  M := Default(TReferenceModel); M.CellCount := Cells; M.ValueCount := Values;
  SetLength(M.Neighbors, Cells * 6);
  for I := 0 to High(M.Neighbors) do M.Neighbors[I] := -1;
  SetLength(M.Compatibility, 6 * Values * Values);
  for I := 0 to High(M.Compatibility) do M.Compatibility[I] := 1;
  SetLength(M.RequiredValues, Values);
  SetLength(M.RequiredSupport, Length(M.Compatibility));
  SetLength(M.InitialAllowed, Cells * Values);
  for I := 0 to High(M.InitialAllowed) do M.InitialAllowed[I] := 1;
  SetLength(M.InitialFailureKinds, Cells);
  SetLength(M.LockedValues, Cells); SetLength(M.CellOrder, Cells);
  for I := 0 to Cells - 1 do
  begin
    M.InitialFailureKinds[I] := rckEntryDomain;
    M.LockedValues[I] := -1; M.CellOrder[I] := I;
  end;
end;

procedure Domain(var M: TReferenceModel; const Cell, Mask: Integer);
var V: Integer;
begin
  for V := 0 to M.ValueCount - 1 do
    M.InitialAllowed[Cell * M.ValueCount + V] := Ord((Mask and (1 shl V)) <> 0);
end;

procedure Weights(var M: TReferenceModel; const W: array of Integer);
var I: Integer;
begin
  SetLength(M.ValueWeights, Length(W));
  for I := 0 to High(W) do M.ValueWeights[I] := W[I];
end;

procedure Shuffle(var M: TReferenceModel; const Variant: Integer);
var I, J, T: Integer;
begin
  for I := M.CellCount - 1 downto 1 do
  begin
    J := (I * 13 + Variant * 7) mod (I + 1);
    T := M.CellOrder[I]; M.CellOrder[I] := M.CellOrder[J]; M.CellOrder[J] := T;
  end;
end;

procedure Pair(var M: TReferenceModel; const D, A, B: Integer;
  const Allowed: Boolean);
begin M.Compatibility[(D * M.ValueCount + A) * M.ValueCount + B] := Ord(Allowed); end;

procedure Edge(var M: TReferenceModel; const A, B, D: Integer);
const Opposite: array[0..5] of Integer = (2, 3, 0, 1, 5, 4);
begin M.Neighbors[A * 6 + D] := B; M.Neighbors[B * 6 + Opposite[D]] := A; end;

procedure AddQuota(var M: TReferenceModel; const Mask, Minimum, Maximum: Integer);
var N, I, J: Integer;
begin
  N := Length(M.ValueQuotas); SetLength(M.ValueQuotas, N + 1);
  M.ValueQuotas[N].MinimumCount := Minimum;
  M.ValueQuotas[N].MaximumCount := Maximum;
  for I := 0 to M.ValueCount - 1 do if (Mask and (1 shl I)) <> 0 then
  begin
    J := Length(M.ValueQuotas[N].Values);
    SetLength(M.ValueQuotas[N].Values, J + 1); M.ValueQuotas[N].Values[J] := I;
  end;
end;

procedure Exclusions(var M: TReferenceModel; const Count: Integer);
var I, J: Integer;
begin
  SetLength(M.ExcludedAssignments, Count);
  for I := 0 to Count - 1 do
  begin
    SetLength(M.ExcludedAssignments[I], M.CellCount);
    for J := 0 to M.CellCount - 1 do M.ExcludedAssignments[I][J] := (I shr J) and 1;
  end;
end;

procedure ConnectedGrid(var M: TReferenceModel; const Variant: Integer);
var I: Integer;
begin
  Model(6, 3, M);
  for I := 0 to 5 do
  begin
    if I mod 3 < 2 then Edge(M, I, I + 1, 0);
    if I < 3 then Edge(M, I, I + 3, 1);
  end;
  SetLength(M.Connectivity, 1);
  M.Connectivity[0].RootCell := 0;
  M.Connectivity[0].RequireAllParticipants := Variant mod 2 <> 0;
  SetLength(M.Connectivity[0].RequiredCells, 1);
  M.Connectivity[0].RequiredCells[0] := 5;
  SetLength(M.Connectivity[0].Profiles, 2);
  M.Connectivity[0].Profiles[0].ValueIndex := 1;
  M.Connectivity[0].Profiles[0].Ports := 15;
  M.Connectivity[0].Profiles[1].ValueIndex := 2;
  M.Connectivity[0].Profiles[1].Ports := 5;
  M.Connectivity[0].Profiles[1].RequiredByValue := True;
  if Variant = 7 then begin Domain(M, 1, 1); Domain(M, 3, 1); end;
  Weights(M, [3, 2, 1]); Shuffle(M, Variant);
end;

procedure BuildCase(const Group, Variant: Integer; out M: TReferenceModel;
  out Budget: Integer);
var I, J, D, A, B, Mask: Integer;
begin
  Budget := 128;
  case Group of
    0: Model(0, Variant mod 3, M);
    1: begin Model(1 + Variant, 1, M); Shuffle(M, Variant); end;
    2..7:
      begin
        Model(5 + Variant, 4, M);
        case Group of
          3: Weights(M, [7, 7, 7, 7]);
          4, 7: Weights(M, [32, 1, 1, 1]);
          5: Weights(M, [224, 7, 7, 7]);
        end;
        if Group >= 6 then Shuffle(M, Variant);
        if Group = 7 then
          for I := 0 to M.CellCount - 1 do
            case I mod 4 of
              0: Domain(M, I, 3);
              1: Domain(M, I, 5);
              2: Domain(M, I, 6);
              3: Domain(M, I, 14);
            end;
      end;
    8..11:
      begin
        Model(7, 4, M); Shuffle(M, Variant);
        for I := 0 to 6 do Domain(M, I, 1 + (I * 7 + Variant * 3) mod 15);
        if Variant mod 2 <> 0 then Weights(M, [1, 2, 7, 13]);
        if Group = 9 then M.LockedValues[Variant mod 7] := Variant mod 4;
        if Group = 10 then Domain(M, Variant mod 7, 0);
        if Group = 11 then
        begin Domain(M, 0, 1); M.LockedValues[0] := 3; end;
      end;
    12:
      begin
        Model(8, 3, M); Shuffle(M, Variant);
        for I := 0 to 6 do M.Neighbors[I * 6 + (I + Variant) mod 6] := I + 1;
        for D := 0 to 5 do for A := 0 to 2 do for B := 0 to 2 do
          Pair(M, D, A, B, (A + B + D + Variant) mod 3 <> 0);
        Domain(M, 7, 1 + Variant mod 7);
      end;
    13:
      begin
        Model(5, 3, M); Shuffle(M, Variant);
        for I := 0 to 4 do M.Neighbors[I * 6 + I] := I;
        for D := 0 to 5 do for A := 0 to 2 do for B := 0 to 2 do
          Pair(M, D, A, B, (A + B + Variant) mod 3 <> 1);
        if Variant mod 2 <> 0 then Weights(M, [2, 4, 9]);
      end;
    14:
      begin
        Model(7, 3, M); Shuffle(M, Variant);
        for I := 0 to 5 do Edge(M, I, I + 1, 0);
        M.RequiredValues[2] := 1;
        for D := 0 to 5 do M.RequiredSupport[(D * 3 + 2) * 3 + 1] := 1;
        if Variant mod 2 <> 0 then M.LockedValues[3] := 2;
        Domain(M, 0, 1 + Variant mod 7);
        Weights(M, [1, 2, 8]);
      end;
    15:
      begin
        Model(7, 4, M); Weights(M, [32, 1, 1, 1]); Shuffle(M, Variant);
        for I := 0 to 5 do M.Neighbors[I * 6] := I + 1;
        Pair(M, 0, 0, 0, False); Pair(M, 0, 1, 3, False);
        Domain(M, 0, 3); Domain(M, 3, 14);
        if Variant >= 4 then Domain(M, 6, 1 shl (Variant mod 4));
      end;
    16..18:
      begin
        Model(3, 2, M); Shuffle(M, Variant);
        if Group = 17 then Exclusions(M, 8) else Exclusions(M, 7);
        if Variant mod 2 <> 0 then Weights(M, [3, 1]);
        if Group = 18 then Budget := Variant mod 4;
      end;
    19: ConnectedGrid(M, Variant);
    20:
      begin
        Model(7, 4, M); Shuffle(M, Variant); Weights(M, [9, 3, 6, 12]);
        AddQuota(M, 3, 2, 4); AddQuota(M, 6, 3, 5); AddQuota(M, 12, 1, 3);
        if Variant = 7 then AddQuota(M, 15, 0, 6);
        if Variant mod 2 <> 0 then Domain(M, 1, 5);
      end;
    21:
      begin
        ConnectedGrid(M, Variant);
        AddQuota(M, 6, 4, 5); AddQuota(M, 3, 4, 6);
        if Variant mod 2 <> 0 then AddQuota(M, 2, 3, 3);
      end;
    22..23:
      begin
        Model(3 + Variant mod 5, 4, M); Shuffle(M, Variant);
        if Group = 23 then Weights(M, [1, 3, 17, 41]);
        for I := 0 to M.CellCount - 1 do
        begin
          Mask := 1 + (Variant * 11 + I * 7) mod 15; Domain(M, I, Mask);
          for D := 0 to 5 do
          begin
            J := (I * 11 + D * 7 + Variant * 3) mod (M.CellCount + 2);
            if J < M.CellCount then M.Neighbors[I * 6 + D] := J;
          end;
        end;
        for D := 0 to 5 do for A := 0 to 3 do for B := 0 to 3 do
          Pair(M, D, A, B, (A * 3 + B * 7 + D + Variant) mod 7 <> 0);
        if Variant mod 3 = 0 then AddQuota(M, 5, 1, M.CellCount - 1);
        Budget := 8;
      end;
    24:
      begin
        { Frozen scan fixture: Q16 normalization of 2^24, 2^24+1, and
          2^24+2 is the same 65536. The two entropies floor to zero despite
          domain counts two and three. CellOrder alone must break this tie;
          using domain count as a secondary weighted key changes behavior. }
        Model(2, 3, M); Weights(M, [16777216, 1, 1]); Domain(M, 0, 3);
        if Variant < 4 then begin M.CellOrder[0] := 1; M.CellOrder[1] := 0; end;
      end;
    else raise Exception.Create('unknown corpus group');
  end;
end;

function Entropy(const M: TReferenceModel; const D: TReferenceByteArray;
  const C: Integer): Double;
var V, W, Sum: Integer; WeightedLog: Double;
begin
  Sum := 0; WeightedLog := 0;
  for V := 0 to M.ValueCount - 1 do if D[C * M.ValueCount + V] <> 0 then
  begin
    if Length(M.ValueWeights) = 0 then W := 1 else W := M.ValueWeights[V];
    Inc(Sum, W); WeightedLog := WeightedLog + W * Ln(W);
  end;
  if Sum = 0 then Result := 0 else Result := Ln(Sum) - WeightedLog / Sum;
end;

procedure ObserveCoverage(const M: TReferenceModel; const R: TReferenceSolveReport);
var D: TReferenceByteArray; I, K: Integer; Before, After: Double;
begin
  case R.Status of
    rssSolved: Inc(SolvedCases);
    rssContradiction: Inc(ContradictionCases);
    rssBacktrackLimit: Inc(LimitCases);
  end;
  Inc(Backtracks, R.Backtracks);
  SetLength(D, Length(M.InitialAllowed));
  for I := 0 to High(D) do D[I] := 1;
  for I := 0 to High(R.Trace) do
  begin
    TraceKinds[R.Trace[I].Kind] := True; TraceCauses[R.Trace[I].CauseKind] := True;
    if R.Trace[I].Kind in [rtekInitialCandidateRemoved, rtekCandidateRemoved,
      rtekCandidateRestored] then
    begin
      K := R.Trace[I].EntryIndex * M.ValueCount + R.Trace[I].ValueIndex;
      Check((K >= 0) and (K < Length(D)), 'trace domain index');
      Before := Entropy(M, D, R.Trace[I].EntryIndex);
      if R.Trace[I].Kind = rtekCandidateRestored then
      begin D[K] := 1; Inc(Restored); end
      else D[K] := 0;
      After := Entropy(M, D, R.Trace[I].EntryIndex);
      if Length(M.ValueWeights) > 0 then
      begin
        if After > Before + 0.0000001 then Inc(EntropyIncreases);
        if After < Before - 0.0000001 then Inc(EntropyDecreases);
      end;
    end;
  end;
end;

procedure RunCase(const Group, Variant, Seed: Integer; var Digest: TDigest);
var M: TReferenceModel; A, B, C: TReferenceIntegerArray;
  R, S, T: TReferenceSolveReport; Recorder: TRecorder;
  Budget, I: Integer; X, Y, Z: Boolean;
  Core, Draws, Transcript, LabelText: String;
begin
  Inc(Cases); BuildCase(Group, Variant, M, Budget);
  LabelText := GROUP_NAMES[Group] + '/' + IntToStr(Variant) + '/' + IntToStr(Seed);
  Recorder := TRecorder.Create;
  try
    Recorder.Reset(Seed);
    X := SolveReferenceModel(M, Budget, True, Recorder.Event, Recorder.Draw, A, R);
    if Group = 24 then
      for I := 0 to High(R.Trace) do if R.Trace[I].Kind = rtekDecision then
      begin
        Check(R.Trace[I].EntryIndex = M.CellOrder[0], LabelText + ': Q16 tie uses only CellOrder');
        if Variant < 4 then
          Check(R.Trace[I].DomainCountBefore = 3, LabelText + ': larger tied domain wins')
        else Check(R.Trace[I].DomainCountBefore = 2, LabelText + ': reversed CellOrder wins');
        Break;
      end;
    Check(Length(R.Trace) = Length(Recorder.Events), LabelText + ': captured/sink count');
    for I := 0 to High(R.Trace) do
      Check(EventText(R.Trace[I]) = EventText(Recorder.Events[I]), LabelText + ': sink event');
    Core := ReportText(X, A, R, False); Draws := Recorder.Draws;
    Transcript := LabelText + '|' + ReportText(X, A, R, True) +
      '|draw-count=' + IntToStr(Recorder.DrawCount) + '|draws=' + Draws + #10;
    DigestText(Digest, Transcript);
    {$IFDEF DECISION_INDEX_TRANSCRIPT}Write(Transcript);{$ENDIF}
    ObserveCoverage(M, R);
    Recorder.Reset(Seed);
    Y := SolveReferenceModel(M, Budget, False, Recorder.Draw, B, S);
    Check(ReportText(Y, B, S, False) = Core, LabelText + ': trace-off complete result');
    Check(Length(S.Trace) = 0, LabelText + ': trace-off storage');
    Check(Recorder.Draws = Draws, LabelText + ': trace-off exact draws');
    Recorder.Reset(Seed);
    Z := SolveReferenceModel(M, Budget, False, Recorder.Event, Recorder.Draw, C, T);
    Check(ReportText(Z, C, T, False) = Core, LabelText + ': sink-only complete result');
    Check(Length(T.Trace) = 0, LabelText + ': sink-only storage');
    Check(Recorder.Draws = Draws, LabelText + ': sink-only exact draws');
    Check(Length(R.Trace) = Length(Recorder.Events), LabelText + ': sink-only count');
    for I := 0 to High(R.Trace) do
      Check(EventText(R.Trace[I]) = EventText(Recorder.Events[I]), LabelText + ': sink-only event');
  finally Recorder.Free; end;
end;

procedure Main;
const Seeds: array[0..3] of Integer = (0, 1, 4, 7919);
var G, V, S: Integer; D: TDigest; K: TReferenceTraceEventKind;
begin
  for G := 0 to GROUP_COUNT - 1 do
  begin
    BeginDigest(D);
    for V := 0 to 7 do for S := 0 to High(Seeds) do RunCase(G, V, Seeds[S], D);
    {$IFDEF DECISION_INDEX_CAPTURE}
    WriteLn('GOLDEN ', G, ' ', GROUP_NAMES[G], ' ', DigestHex(D));
    {$ELSE}
    Check(DigestHex(D) = GOLDENS[G], GROUP_NAMES[G] + ' baseline golden: got ' + DigestHex(D));
    {$ENDIF}
  end;
  Check(SolvedCases > 0, 'solved coverage');
  Check(ContradictionCases > 0, 'contradiction coverage');
  Check(LimitCases > 0, 'backtrack limit coverage');
  Check((Backtracks > 0) and (Restored > 0), 'trailed restoration coverage');
  Check((EntropyIncreases > 0) and (EntropyDecreases > 0), 'both entropy update directions');
  for K := Low(K) to High(K) do Check(TraceKinds[K], 'trace kind coverage ' + IntToStr(Ord(K)));
  Check(TraceCauses[rtckRequiredSupport], 'required-only support coverage');
  Check(TraceCauses[rtckConnectivity], 'connectivity pruning coverage');
  Check(TraceCauses[rtckValueQuota], 'overlapping quota coverage');
  Check(TraceCauses[rtckExcludedAssignment], 'exclusion recovery coverage');
  WriteLn('Decision-index corpus: ', Cases, ' cases; ', Checks, ' checks; ',
    SolvedCases, ' solved, ', ContradictionCases, ' contradictions, ', LimitCases, ' limits.');
  WriteLn('Coverage: ', Backtracks, ' backtracks; ', Restored, ' restorations; ',
    EntropyIncreases, ' entropy increases; ', EntropyDecreases, ' decreases.');
end;

begin
  try Main;
  except on E: Exception do
    begin WriteLn('[FAIL] ', E.Message); Halt(1); end;
  end;
end.
