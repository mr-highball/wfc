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
program wfc_sequence_wrap_test;
{$mode delphi}{$H+}
uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, {$IFDEF PAS2JS}Web,{$ENDIF} wfc, wfc_model, wfc_text_codec, wfc_sequence,
  wfc_sequence_learn, wfc_sequence_text, wfc_sequence_graph,
  wfc_sequence_analyze;

type
  TStrings = array of String;
  TIntArray = array of Integer;
  TTestProcedure = procedure;

const
  GOLDEN_AB = 'wfcs=2'#10 + 'boundary=wrap'#10 + 'order=2'#10 +
    'samples=1'#10 + 's=0,2'#10 + 'tokens=2'#10 + 't=0,A'#10 +
    't=1,B'#10 + 'states=2'#10 + 'q=0,1,0,0,T1,E0'#10 +
    'q=1,1,0,0,T0,E1'#10 + 'end'#10;

var
  GChecks, GFailures: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GChecks);
  if not ACondition then
  begin
    Inc(GFailures);
    WriteLn('[FAIL] ', AMessage);
    {$IFDEF PAS2JS}
    document.body.setAttribute('data-self-test-message', AMessage);
    {$ENDIF}
  end;
end;

procedure Run(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try ATest;
  except on E: Exception do
    begin
      Inc(GFailures); WriteLn('[EXCEPTION] ', E.ClassName, ': ', E.Message);
      {$IFDEF PAS2JS}
      document.body.setAttribute('data-self-test-message', AName + ': ' + E.Message);
      {$ENDIF}
    end;
  end;
end;

function Tokens(const A: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(A));
  for I := 0 to High(A) do Result[I] := A[I];
end;

function Atom(const A: TWfcModelToken): String;
begin Result := WfcTextEncodeToken(A, 'literal circular oracle') + '|'; end;

function StateKey(const M: TWfcSequenceModel; const S: Integer): String;
var H: Integer; Item: TWfcSequenceHistoryItem;
begin
  Result := '';
  for H := 0 to M.HistorySize - 1 do
  begin
    Item := M.HistoryItemAt(S, H);
    if Item.Kind = wshBos then Result := Result + 'BOS|'
    else Result := Result + Atom(M.PublicTokenAt(Item.TokenIndex));
  end;
  Result := Result + Atom(M.ProjectStateToken(S));
end;

function FindKey(const A: TStrings; const K: String): Integer;
var I: Integer;
begin
  for I := 0 to High(A) do if A[I] = K then Exit(I);
  Result := -1;
end;

{ Literal oracle: concatenate complete copies, then read ordinary contiguous
  windows in the last copy. No modular indexing or production history helpers. }
procedure LiteralHistogram(const Samples: TWfcSequenceSamples; const Order: Integer;
  out Keys: TStrings; out Counts: TIntArray);
var S, R, I, H, N, Base, Found: Integer; Expanded: TWfcModelTokens; K: String;
begin
  Keys := nil; Counts := nil;
  for S := 0 to High(Samples) do
  begin
    N := Length(Samples[S].Tokens); Base := Order * N;
    SetLength(Expanded, (Order + 1) * N);
    for R := 0 to Order do
      for I := 0 to N - 1 do Expanded[R * N + I] := Samples[S].Tokens[I];
    for I := 0 to N - 1 do
    begin
      K := '';
      for H := 1 - Order to 0 do K := K + Atom(Expanded[Base + I + H]);
      Found := FindKey(Keys, K);
      if Found < 0 then
      begin
        Found := Length(Keys); SetLength(Keys, Found + 1);
        SetLength(Counts, Found + 1); Keys[Found] := K; Counts[Found] := 0;
      end;
      Counts[Found] := Counts[Found] + 1;
    end;
  end;
end;

procedure CompareOracle(const Samples: TWfcSequenceSamples; const Order: Integer);
var M, CopyM: TWfcSequenceModel; Keys: TStrings; Counts: TIntArray;
  S, I, Total: Integer; Text: String;
begin
  LiteralHistogram(Samples, Order, Keys, Counts);
  M := LearnSequenceModelCorpus(Samples, Order, wmbWrap);
  try
    Check((M.Boundary = wmbWrap) and (M.ModelVersion = 2), 'explicit circular identity');
    Check(M.StateCount = Length(Keys), 'literal histogram state count');
    Total := 0;
    for S := 0 to High(Samples) do
    begin
      Inc(Total, Length(Samples[S].Tokens));
      Check(M.SampleLengthAt(S) = Length(Samples[S].Tokens), 'source length retained without padding');
    end;
    Check(M.ObservationCount = Total, 'one observation per original token');
    for I := 0 to M.StateCount - 1 do
    begin
      Check(StateKey(M, I) = Keys[I], 'first-appearance state matches literal window');
      Check(M.StateObservationCountAt(I) = Counts[I], 'raw count matches literal histogram');
      Check((M.StartCountAt(I) = 0) and (M.EndCountAt(I) = 0) and
        (M.StateLeadingBosCountAt(I) = 0), 'circles invent neither BOS nor endpoints');
    end;
    Text := EncodeWfcSequenceText(M);
    CopyM := DecodeWfcSequenceText(Text);
    try Check(EncodeWfcSequenceText(CopyM) = Text, 'wrapped canonical exact round-trip');
    finally CopyM.Free; end;
  finally M.Free; end;
end;

procedure TestLiteralCorpusOracle;
var Samples: TWfcSequenceSamples; N, Mask, I, Order, Count: Integer;
begin
  SetLength(Samples, 1);
  for N := 1 to 5 do
  begin
    SetLength(Samples[0].Tokens, N); Count := 1 shl N;
    for Mask := 0 to Count - 1 do
    begin
      for I := 0 to N - 1 do
        if (Mask and (1 shl I)) = 0 then Samples[0].Tokens[I] := 'A'
        else Samples[0].Tokens[I] := 'B';
      for Order := 1 to 7 do CompareOracle(Samples, Order);
    end;
  end;
  SetLength(Samples, 3);
  Samples[0] := MakeWfcSequenceSample(Tokens(['A', 'B', 'A']));
  Samples[1] := MakeWfcSequenceSample(Tokens(['C']));
  Samples[2] := MakeWfcSequenceSample(Tokens(['A', 'B']));
  for Order := 1 to 9 do CompareOracle(Samples, Order);
  Samples[0] := MakeWfcSequenceSample(Tokens([
    WfcTextDecodeToken('%F0%9F%8E%B5', 'oracle'), '%', '|']));
  Samples[1] := MakeWfcSequenceSample(Tokens(['@wfcs1:0:0', 'BOS']));
  Samples[2] := MakeWfcSequenceSample(Tokens([#10]));
  CompareOracle(Samples, 6);
end;

procedure RejectText(const Text, LabelText: String);
var M: TWfcSequenceModel; Rejected: Boolean;
begin
  M := nil; Rejected := False;
  try
    try M := DecodeWfcSequenceText(Text);
    except on E: EConvertError do Rejected := True; end;
  finally M.Free; end;
  Check(Rejected, LabelText);
end;

procedure TestCodecAndGuards;
var M, OpenM, CopyM: TWfcSequenceModel; States: TWfcSequenceStates;
  Counts, Starts, Ends: TWfcModelIntegerArray; Lengths: TWfcSequenceSampleLengths;
  T: TWfcModelTokens; Raised: Boolean;

  procedure RejectConstructor(const LabelText: String);
  begin
    CopyM := nil; Raised := False;
    try
      try CopyM := TWfcSequenceModel.Create(2, Lengths, T, States,
        Counts, Starts, Ends, wmbWrap);
      except on E: EWfcSequence do Raised := True; end;
    finally CopyM.Free; end;
    Check(Raised, LabelText);
  end;

begin
  M := LearnSequenceModel(Tokens(['A', 'B']), 2, wmbWrap);
  try
    Check(EncodeWfcSequenceText(M) = GOLDEN_AB, 'literal wfcs2 AB golden');
    Lengths := M.CopySampleLengths; T := M.CopyPublicTokens;
    States := M.CopyStates; Counts := M.CopyStateCounts;
    Starts := M.CopyStartCounts; Ends := M.CopyEndCounts;
    Starts[0] := 1; RejectConstructor('circular starts rejected'); Starts[0] := 0;
    Ends[0] := 1; RejectConstructor('circular ends rejected'); Ends[0] := 0;
    States[0].History[0] := MakeWfcSequenceBosHistoryItem;
    RejectConstructor('circular BOS rejected'); States := M.CopyStates;
    Lengths[0] := 3; Counts[0] := 2;
    RejectConstructor('unbalanced weighted contexts rejected despite positive predecessor/successor support');
    Lengths[0] := 2; Counts := M.CopyStateCounts;
    States[0].History[0] := MakeWfcSequenceTokenHistoryItem(0);
    RejectConstructor('structural dangling context rejected');
    Check(EncodeWfcSequenceText(M) = GOLDEN_AB, 'detached mutations do not change source');
  finally M.Free; end;
  RejectText(StringReplace(GOLDEN_AB, 'boundary=wrap', 'boundary=open', []), 'v2 open alias rejected');
  RejectText(StringReplace(GOLDEN_AB, 'boundary=wrap'#10, '', []), 'v2 missing boundary rejected');
  RejectText(StringReplace(GOLDEN_AB, 'wfcs=2', 'wfcs=1', []), 'v1 cannot carry circular header');
  RejectText(StringReplace(GOLDEN_AB, 'T1,E0', 'B,E0', []), 'persisted circular BOS rejected');
  RejectText(StringReplace(GOLDEN_AB, 'q=0,1,0,0', 'q=0,1,1,0', []), 'persisted circular endpoint rejected');
  RejectText(GOLDEN_AB + #10, 'noncanonical trailing newline rejected');
  RejectText(StringReplace(GOLDEN_AB, 'boundary=wrap', 'boundary=wrap'#10 + 'boundary=wrap', []),
    'duplicate boundary rejected');
  M := LearnSequenceModel(Tokens(['A', 'B']), 2);
  OpenM := LearnSequenceModel(Tokens(['A', 'B']), 2, wmbOpen);
  try
    Check((M.Boundary = wmbOpen) and (M.ModelVersion = 1), 'open default retained');
    Check(EncodeWfcSequenceText(M) = EncodeWfcSequenceText(OpenM), 'explicit open is byte-identical');
    Check(Pos('wfcs=1'#10 + 'order=2'#10, EncodeWfcSequenceText(M)) = 1,
      'open canonical text retains original layout');
  finally OpenM.Free; M.Free; end;
  Raised := False; M := nil;
  try
    try M := LearnSequenceModel(Tokens(['A']), WFC_SEQUENCE_MAX_ORDER + 1, wmbWrap);
    except on E: EWfcSequence do Raised := True; end;
  finally M.Free; end;
  Check(Raised, 'circular order limit not bypassed');
  M := LearnSequenceModel(Tokens(['A']), WFC_SEQUENCE_MAX_ORDER, wmbWrap);
  try Check((M.StateCount = 1) and (M.ObservationCount = 1),
    'maximum order singleton wraps without synthetic observations');
  finally M.Free; end;
end;

procedure TestGraphAndExtents;
var M, OpenM, Clone: TWfcSequenceModel; G: TGraph; O: TGraphSolveOptions;
  Report: TGraphSolveReport; Generated: TWfcGeneratedSequence;
  Validation: TWfcSequenceGraphValidationReport; Analysis: TWfcSequenceDomainAnalysis;
  I: Integer;
begin
  M := LearnSequenceModel(Tokens(['A', 'B']), 2, wmbWrap);
  OpenM := LearnSequenceModel(Tokens(['A', 'B']), 2);
  Clone := DecodeWfcSequenceText(EncodeWfcSequenceText(M));
  G := TGraph.Create;
  try
    Check(not AnalyzeSequenceTokenDomains(OpenM, 4, wseWrap, nil, Analysis),
      'open AB has no derived circle');
    Check(AnalyzeSequenceTokenDomains(M, 4, wseWrap, nil, Analysis),
      'real wrapped training contributes missing seam');
    Check(not AnalyzeSequenceTokenDomains(M, 3, wseWrap, nil, Analysis),
      'odd ring cannot satisfy AB closure');
    Check(AnalyzeSequenceTokenDomains(M, 3, wseFragment, nil, Analysis),
      'finite fragment need not close');
    Check(not AnalyzeSequenceTokenDomains(M, 4, wseWhole, nil, Analysis),
      'circle is not an observed bounded whole sample');
    Check(not AnalyzeSequenceTokenDomains(M, 4, wsePrefix, nil, Analysis),
      'circle does not invent an observed prefix start');
    Check(not AnalyzeSequenceTokenDomains(M, 4, wseSuffix, nil, Analysis),
      'circle does not invent an observed suffix end');
    G.Reshape(6, 1, 1); G.WrapNeighbors := True;
    ApplySequenceModelToGraph(M, G, wseWrap);
    IntersectSequenceAllowedTokens(M, G, 0, 'A');
    O := DefaultGraphSolveOptions;
    Check(G.TrySolve(O, Report), 'locked circular graph solves');
    Check(CaptureSolvedSequence(M, G, Generated, Validation), 'circle independently captured');
    Check(Length(Generated.Tokens) = 6, 'full circular output extent retained');
    for I := 0 to High(Generated.Tokens) do
      if I mod 2 = 0 then Check(Generated.Tokens[I] = 'A', 'even A projection')
      else Check(Generated.Tokens[I] = 'B', 'odd B projection');
    Check(CaptureSolvedSequence(Clone, G, Generated, Validation), 'canonical clone matches graph model identity');
    Check(not CaptureSolvedSequence(OpenM, G, Generated, Validation),
      'open model cannot capture circular-model keys');
    Check(Length(Generated.Tokens) = 0, 'mismatched capture publishes no tokens');
  finally G.Free; Clone.Free; OpenM.Free; M.Free; end;
end;

{ Independently enumerate all public candidate rings and compare exact feasible
  public domains to the latent forward/backward/cycle analyzer. }
procedure TestExhaustiveDomains;
var Samples: TWfcSequenceSamples; M: TWfcSequenceModel;
  Keys: TStrings; Counts: TIntArray; Candidate: TWfcModelTokens;
  Expected: array of Boolean; Constraints: TWfcSequenceTokenConstraints;
  Analysis: TWfcSequenceDomainAnalysis; N, Total, Code, Remaining, I, J, H, P: Integer;
  Possible, AnyPath, Actual: Boolean; K: String;
begin
  SetLength(Samples, 2);
  Samples[0] := MakeWfcSequenceSample(Tokens(['A', 'B']));
  Samples[1] := MakeWfcSequenceSample(Tokens(['A', 'C']));
  LiteralHistogram(Samples, 2, Keys, Counts);
  M := LearnSequenceModelCorpus(Samples, 2, wmbWrap);
  try
    SetLength(Constraints, 1);
    Constraints[0] := MakeWfcSequenceTokenConstraint(0, Tokens(['A']));
    for N := 1 to 6 do
    begin
      SetLength(Candidate, N); SetLength(Expected, N * 3);
      for I := 0 to High(Expected) do Expected[I] := False;
      Total := 1; for I := 1 to N do Total := Total * 3;
      AnyPath := False;
      for Code := 0 to Total - 1 do
      begin
        Remaining := Code;
        for I := 0 to N - 1 do
        begin Candidate[I] := M.PublicTokenAt(Remaining mod 3); Remaining := Remaining div 3; end;
        Possible := Candidate[0] = 'A';
        for I := 0 to N - 1 do
        begin
          K := '';
          for H := -1 to 0 do
          begin P := I + H; if P < 0 then P := P + N; K := K + Atom(Candidate[P]); end;
          if FindKey(Keys, K) < 0 then Possible := False;
        end;
        if Possible then
        begin
          AnyPath := True;
          for I := 0 to N - 1 do Expected[I*3 + M.FindPublicToken(Candidate[I])] := True;
        end;
      end;
      Actual := AnalyzeSequenceTokenDomains(M, N, wseWrap, Constraints, Analysis);
      Check(Actual = AnyPath, 'enumerated public rings agree with latent cycle feasibility');
      if Actual then
        for I := 0 to N - 1 do
          for J := 0 to 2 do
          begin
            Possible := False;
            for H := 0 to High(Analysis.Positions[I].Tokens) do
              if Analysis.Positions[I].Tokens[H] = M.PublicTokenAt(J) then Possible := True;
            Check(Possible = Expected[I*3 + J], 'exact public feasible domain matches enumerated rings');
          end;
    end;
  finally M.Free; end;
end;

procedure TestApiAndCapacityGuards;
var
  M, Candidate: TWfcSequenceModel;
  GraphModel: TWfcModel;
  Samples: TWfcSequenceSamples;
  T: TWfcModelTokens;
  States: TWfcSequenceStates;
  Lengths: TWfcSequenceSampleLengths;
  Counts, Starts, Ends: TWfcModelIntegerArray;
  Order, I, J: Integer;
  Raised: Boolean;

  procedure RejectCorpus(const Expected: String);
  begin
    Candidate := nil; Raised := False;
    try
      try Candidate := LearnSequenceModelCorpus(Samples, Order, wmbWrap);
      except on E: EWfcSequence do Raised := Pos(Expected, E.Message) > 0; end;
    finally Candidate.Free; end;
    Check(Raised, 'circular corpus rejects ' + Expected);
  end;

  procedure RejectConstructor(const Expected: String);
  begin
    Candidate := nil; Raised := False;
    try
      try Candidate := TWfcSequenceModel.Create(Order, Lengths, T, States,
        Counts, Starts, Ends, wmbWrap);
      except on E: EWfcSequence do Raised := Pos(Expected, E.Message) > 0; end;
    finally Candidate.Free; end;
    Check(Raised, 'circular constructor rejects ' + Expected);
  end;

begin
  Order := 2; Samples := nil;
  RejectCorpus('corpus cannot be empty');
  SetLength(Samples, 1);
  RejectCorpus('sample cannot be empty');
  Candidate := nil; Raised := False;
  try
    try Candidate := LearnSequenceModel(nil, 2, wmbWrap);
    except on E: EWfcSequence do Raised := Pos('sample cannot be empty', E.Message) > 0; end;
  finally Candidate.Free; end;
  Check(Raised, 'circular single-sample API rejects empty input');
  Samples[0] := MakeWfcSequenceSample(Tokens(['A']));
  Order := 0; RejectCorpus('order must');
  Order := -1; RejectCorpus('order must');
  Order := 2; SetLength(Samples, WFC_SEQUENCE_MAX_SAMPLE_COUNT + 1);
  RejectCorpus('sample count exceeds');

  M := LearnSequenceModel(Tokens(['A', 'B']), 2, wmbWrap);
  try
    GraphModel := M.CreateGraphModel(Tokens(['private-a', 'private-b']));
    try
      Check(GraphModel.Boundary = wmbWrap, 'standalone graph model preserves circular training boundary');
      Check((GraphModel.SampleCount = 1) and (GraphModel.SampleWidth = 2),
        'standalone graph model retains original unexpanded sample shape');
    finally GraphModel.Free; end;
    T := M.CopyPublicTokens; Lengths := nil; States := M.CopyStates;
    Counts := M.CopyStateCounts; Starts := M.CopyStartCounts; Ends := M.CopyEndCounts;
    RejectConstructor('retain at least one sample');
    Lengths := M.CopySampleLengths;
    SetLength(States, WFC_SEQUENCE_MAX_STATE_COUNT + 1);
    RejectConstructor('state count exceeds');
    Order := WFC_SEQUENCE_MAX_ORDER;
    SetLength(States, WFC_SEQUENCE_MAX_TOTAL_HISTORY_ITEM_COUNT div (Order - 1) + 1);
    { Deliberately unallocated histories: the aggregate cardinality must be
      rejected before copying them or allocating a relation matrix. }
    RejectConstructor('history size exceeds');
  finally M.Free; end;

  { A modest 33-token corpus contains every ordered pair. This exceeds the
    state cap without first exceeding public-token or sample-count limits. }
  Order := 2; SetLength(Samples, 33 * 33);
  for I := 0 to 32 do
    for J := 0 to 32 do
      Samples[I * 33 + J] := MakeWfcSequenceSample(Tokens([
        TWfcModelToken('t' + IntToStr(I)), TWfcModelToken('t' + IntToStr(J))]));
  RejectCorpus('state count exceeds');
  Order := WFC_SEQUENCE_MAX_ORDER;
  SetLength(T, WFC_SEQUENCE_MAX_TOTAL_HISTORY_ITEM_COUNT div (Order - 1) + 1);
  for I := 0 to High(T) do T[I] := TWfcModelToken('t' + IntToStr(I));
  SetLength(Samples, 1); Samples[0] := MakeWfcSequenceSample(T);
  RejectCorpus('history exceeds');
end;

{$IFDEF PAS2JS}
function HostileScalar(const ACase: Integer): Integer;
begin
  { All values are intentionally outside the public Pascal scalar type. }
  asm Result = ['1', NaN, Infinity, -Infinity, 0.5, null, undefined, true, false, {}][ACase]; end;
end;

procedure TestHostileBrowserScalars;
var
  M, Candidate: TWfcSequenceModel;
  Samples: TWfcSequenceSamples;
  T: TWfcModelTokens;
  States: TWfcSequenceStates;
  Lengths: TWfcSequenceSampleLengths;
  Counts, Starts, Ends: TWfcModelIntegerArray;
  Order, Bad, Field, Kind, Single: Integer;
  Boundary: TWfcModelBoundary;
  Raised: Boolean;
  LabelText: String;

  procedure ResetInputs;
  begin
    Order := 2; Boundary := wmbWrap;
    T := M.CopyPublicTokens; Lengths := M.CopySampleLengths;
    States := M.CopyStates; Counts := M.CopyStateCounts;
    Starts := M.CopyStartCounts; Ends := M.CopyEndCounts;
  end;

begin
  M := LearnSequenceModel(Tokens(['A', 'B']), 2, wmbWrap);
  try
    SetLength(Samples, 1); Samples[0] := MakeWfcSequenceSample(M.CopyPublicTokens);
    for Field := 0 to 8 do
      for Kind := 0 to 9 do
      begin
        ResetInputs; Bad := HostileScalar(Kind);
        case Field of
          0: begin Boundary := TWfcModelBoundary(Bad); LabelText := 'boundary'; end;
          1: begin Order := Bad; LabelText := 'order'; end;
          2: begin Lengths[0] := Bad; LabelText := 'sample length'; end;
          3: begin Counts[0] := Bad; LabelText := 'observation count'; end;
          4: begin Starts[0] := Bad; LabelText := 'start count'; end;
          5: begin Ends[0] := Bad; LabelText := 'end count'; end;
          6: begin States[0].EmittedTokenIndex := Bad; LabelText := 'emission index'; end;
          7: begin States[0].History[0].TokenIndex := Bad; LabelText := 'history index'; end;
          8: begin States[0].History[0].Kind := TWfcSequenceHistoryKind(Bad); LabelText := 'history kind'; end;
        end;
        Candidate := nil; Raised := False;
        try
          try Candidate := TWfcSequenceModel.Create(Order, Lengths, T, States,
            Counts, Starts, Ends, Boundary);
          except on E: EWfcSequence do Raised := True; end;
        finally Candidate.Free; end;
        Check(Raised, 'browser constructor rejects hostile ' + LabelText + ' #' + IntToStr(Kind));
      end;
    for Field := 0 to 1 do
      for Kind := 0 to 9 do
        for Single := 0 to 1 do
        begin
          ResetInputs; Bad := HostileScalar(Kind);
          if Field = 0 then Boundary := TWfcModelBoundary(Bad) else Order := Bad;
          Candidate := nil; Raised := False;
          try
            try
              if Single = 0 then Candidate := LearnSequenceModel(T, Order, Boundary)
              else Candidate := LearnSequenceModelCorpus(Samples, Order, Boundary);
            except on E: EWfcSequence do Raised := True; end;
          finally Candidate.Free; end;
          Check(Raised, 'browser learner rejects hostile boundary/order #' +
            IntToStr(Field) + '/' + IntToStr(Kind) + '/' + IntToStr(Single));
        end;
    Check(EncodeWfcSequenceText(M) = GOLDEN_AB, 'hostile detached scalars leave immutable model unchanged');
  finally M.Free; end;
end;
{$ENDIF}

begin
  Run('literal circular corpus oracle', @TestLiteralCorpusOracle);
  Run('canonical model and strict guards', @TestCodecAndGuards);
  Run('graph closure, fragments and model identity', @TestGraphAndExtents);
  Run('exhaustive public ring domain oracle', @TestExhaustiveDomains);
  Run('circular API and capacity guards', @TestApiAndCapacityGuards);
  {$IFDEF PAS2JS}
  Run('hostile browser scalar API guards', @TestHostileBrowserScalars);
  {$ENDIF}
  WriteLn(GChecks, ' checks, ', GFailures, ' failures');
  if GFailures <> 0 then Halt(1);
end.
