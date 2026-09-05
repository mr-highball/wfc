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
program wfc_sequence_segment_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_sequence, wfc_sequence_learn,
  wfc_sequence_graph;

type TTest = procedure;
var Checks, Failures: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then begin Inc(Failures); WriteLn('FAIL: ', AMessage); end;
end;

procedure Run(const AName: String; const ATest: TTest);
begin
  WriteLn('Test: ', AName);
  try ATest;
  except on E: Exception do
  begin Inc(Failures); WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message); end;
  end;
end;

function Tokens(const AValues: array of String): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := TWfcModelToken(AValues[I]);
end;

function Path(const AValues: array of Integer): TWfcSequenceStateIndices;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

function EqualTokens(const A, B: TWfcModelTokens): Boolean;
var I: Integer;
begin
  Result := False;
  if Length(A) <> Length(B) then Exit;
  for I := 0 to High(A) do if A[I] <> B[I] then Exit;
  Result := True;
end;

function NewGraph(const AWidth: Integer): TGraph;
begin
  Result := TGraph.Create;
  Result.Reshape(AWidth, 1, 1);
  Result.WrapNeighbors := False;
  Result.Seed := 0;
end;

function FindState(const M: TWfcSequenceModel; const AToken: String): Integer;
begin
  for Result := 0 to M.StateCount - 1 do
    if M.ProjectStateToken(Result) = TWfcModelToken(AToken) then Exit;
  raise Exception.Create('fixture state not found: ' + AToken);
end;

function IndependentCompatible(const M: TWfcSequenceModel;
  const APrevious, ACurrent: Integer): Boolean;
var I: Integer; A, B: TWfcSequenceHistoryItem;
begin
  Result := False;
  for I := 0 to M.HistorySize - 2 do
  begin
    A := M.HistoryItemAt(APrevious, I + 1);
    B := M.HistoryItemAt(ACurrent, I);
    if (A.Kind <> B.Kind) or (A.TokenIndex <> B.TokenIndex) then Exit;
  end;
  if M.HistorySize > 0 then
  begin
    B := M.HistoryItemAt(ACurrent, M.HistorySize - 1);
    if (B.Kind <> wshToken) or
      (B.TokenIndex <> M.StateEmittedTokenIndexAt(APrevious)) then Exit;
  end;
  Result := True;
end;

procedure TestOneCellEarlyBos;
var M: TWfcSequenceModel; G: TGraph; B: TWfcSequenceSegmentBoundary;
  S, S2: TWfcGeneratedSequenceSegment; V: TWfcSequenceGraphValidationReport;
  R: TGraphSolveReport; O: TGraphSolveOptions; Source: TWfcModelTokens;
  I, Prev, LOrder: Integer;
begin
  Source := Tokens(['A','B','C','D','E','F']);
  O := DefaultGraphSolveOptions;
  for LOrder := 1 to 6 do
  begin
    M := LearnSequenceModel(Source, LOrder); G := nil; Prev := -1;
    try
      for I := 0 to High(Source) do
      begin
        G := NewGraph(1);
        if I = 0 then B := MakeWfcSequenceInitialSegmentBoundary(False)
        else B := MakeWfcSequenceContinuingSegmentBoundary(Prev, I = High(Source));
        ApplySequenceModelSegmentToGraph(M, G, B);
        IntersectSequenceAllowedTokens(M, G, 0, Source[I]);
        Check(G.TrySolve(O, R), 'one-cell segment solves through partial history');
        Check(CaptureSolvedSequenceSegment(M, G, B, S, V), 'segment captures with exact predecessor');
        Check((Length(S.Tokens) = 1) and (S.Tokens[0] = Source[I]), 'seam emits only the fresh cell');
        Check((V.CheckedStates = 1) and
          (V.CheckedTransitions = Ord(I > 0)), 'incoming transition counted exactly once');
        if (I > 0) and (I < LOrder - 1) then
        begin
          Check(M.StateLeadingBosCountAt(S.StateIndices[0]) > 0, 'early continued state keeps legitimate BOS');
          Check(not ValidateSequenceStatePath(M, S.StateIndices, wseFragment, V),
            'old fragment semantics still reject that BOS-bearing start');
        end;
        Prev := S.StateIndices[0];
        S.Tokens[0] := 'changed'; S.StateIndices[0] := -1;
        Check(CaptureSolvedSequenceSegment(M, G, B, S2, V) and
          (S2.StateIndices[0] = Prev) and (S2.Tokens[0] = Source[I]),
          'captured arrays do not mutate graph or model');
        G.Free; G := nil;
      end;
    finally G.Free; M.Free; end;
  end;
end;

procedure TestExactDomainsAndIndependentOracle;
var M: TWfcSequenceModel; G: TGraph; B: TWfcSequenceSegmentBoundary;
  S: TWfcSequenceStateIndices; V: TWfcSequenceGraphValidationReport;
  Prev, Next, Order, EndFlag: Integer; Expected: Boolean;
begin
  for Order := 1 to 4 do
  begin
    M := LearnSequenceModel(Tokens(['A','B','A','C']), Order); G := nil;
    try
      for EndFlag := 0 to 1 do
        for Prev := -1 to M.StateCount - 1 do
        begin
          G := NewGraph(1);
          if Prev < 0 then B := MakeWfcSequenceInitialSegmentBoundary(EndFlag = 1)
          else B := MakeWfcSequenceContinuingSegmentBoundary(Prev, EndFlag = 1);
          ApplySequenceModelSegmentToGraph(M, G, B);
          for Next := 0 to M.StateCount - 1 do
          begin
            if Prev < 0 then Expected := M.StartCountAt(Next) > 0
            else Expected := IndependentCompatible(M, Prev, Next);
            Expected := Expected and ((EndFlag = 0) or (M.EndCountAt(Next) > 0));
            Check(SequenceStateSatisfiesEntryConstraints(M, G, 0, Next) = Expected,
              'exact endpoint domain agrees with independent history oracle');
            S := Path([Next]);
            Check(ValidateSequenceSegmentStatePath(M, S, B, V) = Expected,
              'independent path validator agrees with exact domain');
          end;
          G.Free; G := nil;
        end;
    finally G.Free; M.Free; end;
  end;
end;

procedure TestLatentIdentityNotJustPublicToken;
var Samples: TWfcSequenceSamples; M: TWfcSequenceModel; G: TGraph;
  B: TWfcSequenceSegmentBoundary; S: TWfcGeneratedSequenceSegment;
  R: TGraphSolveReport; V: TWfcSequenceGraphValidationReport;
  I, PreviousA, PreviousC, StateB: Integer;
begin
  SetLength(Samples, 2);
  Samples[0] := MakeWfcSequenceSample(Tokens(['A','X','B']));
  Samples[1] := MakeWfcSequenceSample(Tokens(['C','X','D']));
  M := LearnSequenceModelCorpus(Samples, 3); G := nil;
  try
    PreviousA := -1; PreviousC := -1;
    for I := 0 to M.StateCount - 1 do
      if M.ProjectStateToken(I) = 'X' then
        if M.PublicTokenAt(M.HistoryItemAt(I, 1).TokenIndex) = 'A' then PreviousA := I
        else PreviousC := I;
    Check((PreviousA >= 0) and (PreviousC >= 0) and (PreviousA <> PreviousC),
      'same public emission has distinct context-bearing identities');
    StateB := FindState(M, 'B');
    B := MakeWfcSequenceContinuingSegmentBoundary(PreviousA, True);
    G := NewGraph(1); ApplySequenceModelSegmentToGraph(M, G, B);
    Check(G.TrySolve(DefaultGraphSolveOptions, R), 'history A-X continuation solves');
    Check(CaptureSolvedSequenceSegment(M, G, B, S, V) and (S.Tokens[0] = 'B'),
      'history A-X permits B, not unrelated D');
    B := MakeWfcSequenceContinuingSegmentBoundary(PreviousC, True);
    Check(not ValidateSequenceSegmentStatePath(M, Path([StateB]), B, V),
      'same emission with C-X history cannot authenticate B seam');
    Check((V.Issue.Kind = wsgikTransition) and (V.Issue.Position = -1) and
      (V.Issue.RelatedPosition = 0) and (V.Issue.StateIndex = PreviousC) and
      (V.Issue.RelatedStateIndex = StateB), 'incoming seam diagnostic retains both state identities');
    G.Free; G := NewGraph(1); ApplySequenceModelSegmentToGraph(M, G, B);
    Check(G.TrySolve(DefaultGraphSolveOptions, R), 'history C-X continuation solves');
    Check(CaptureSolvedSequenceSegment(M, G, B, S, V) and (S.Tokens[0] = 'D'),
      'history C-X permits D');
  finally G.Free; M.Free; end;
end;

procedure TestInitialLegacyParity;
var M: TWfcSequenceModel; OldGraph, NewSegmentGraph: TGraph;
  B: TWfcSequenceSegmentBoundary; OldSequence: TWfcGeneratedSequence;
  Segment: TWfcGeneratedSequenceSegment; OldReport, NewReport: TGraphSolveReport;
  OldValidation, NewValidation: TWfcSequenceGraphValidationReport;
  O: TGraphSolveOptions; E: TWfcSequenceExtent; EndFlag, FalsePosition: Integer;
begin
  M := LearnSequenceModel(Tokens(['A','B','A','C']), 2);
  OldGraph := nil; NewSegmentGraph := nil; O := DefaultGraphSolveOptions;
  O.CaptureTrace := True;
  try
    for EndFlag := 0 to 1 do
    begin
      OldGraph := NewGraph(4); NewSegmentGraph := NewGraph(4);
      if EndFlag = 0 then E := wsePrefix else E := wseWhole;
      ApplySequenceModelToGraph(M, OldGraph, E);
      B := MakeWfcSequenceInitialSegmentBoundary(EndFlag = 1);
      ApplySequenceModelSegmentToGraph(M, NewSegmentGraph, B);
      Check(OldGraph.TrySolve(O, OldReport) and NewSegmentGraph.TrySolve(O, NewReport),
        'initial segment and legacy path both solve');
      Check(OldReport.TraceHash = NewReport.TraceHash,
        'initial segment preserves complete old trace identity');
      Check(CaptureSolvedSequence(M, OldGraph, E, OldSequence, OldValidation),
        'legacy capture remains valid');
      Check(CaptureSolvedSequenceSegment(M, NewSegmentGraph, B, Segment, NewValidation),
        'initial segment capture valid');
      Check(EqualTokens(OldSequence.Tokens, Segment.Tokens) and
        (OldValidation.CheckedStates = NewValidation.CheckedStates) and
        (OldValidation.CheckedTransitions = NewValidation.CheckedTransitions),
        'initial public output and validation counts unchanged');
      Check(SequenceStatesSatisfyEntryConstraints(M, NewSegmentGraph,
        Segment.StateIndices, FalsePosition), 'ordinary caller-domain identity accepts segment model');
      OldGraph.Free; OldGraph := nil; NewSegmentGraph.Free; NewSegmentGraph := nil;
    end;
  finally NewSegmentGraph.Free; OldGraph.Free; M.Free; end;
end;

procedure TestPreflightAndFailureReset;
var M, Other: TWfcSequenceModel; G: TGraph; B: TWfcSequenceSegmentBoundary;
  S: TWfcGeneratedSequenceSegment; V: TWfcSequenceGraphValidationReport;
  R: TGraphSolveReport; Rejected: Boolean; I: Integer;
begin
  M := LearnSequenceModel(Tokens(['A','B','C']), 3);
  Other := LearnSequenceModel(Tokens(['X','Y','Z']), 3);
  G := nil;
  try
    B := MakeWfcSequenceInitialSegmentBoundary(False);
    Check(not B.HasPrevious and (B.PreviousState = -1) and not B.RequireObservedEnd,
      'initial boundary is canonical');
    Rejected := False;
    try B := MakeWfcSequenceContinuingSegmentBoundary(-1, False);
    except on E: ERangeError do Rejected := True; end;
    Check(Rejected, 'negative predecessor constructor rejected');
    for I := 0 to 2 do
    begin
      G := NewGraph(1);
      B := MakeWfcSequenceInitialSegmentBoundary(False);
      case I of
        0: B.PreviousState := 0;
        1: begin B.HasPrevious := True; B.PreviousState := -1 end;
        2: B := MakeWfcSequenceContinuingSegmentBoundary(M.StateCount, False);
      end;
      Rejected := False;
      try ApplySequenceModelSegmentToGraph(M, G, B);
      except on E: EWfcSequenceGraph do Rejected := True; end;
      Check(Rejected and (G.RuleGroups.Count = 0), 'malformed boundary preflight leaves graph empty');
      Check(not ValidateSequenceSegmentStatePath(M, Path([0]), B, V) and
        (V.Issue.Kind = wsgikGraphShape), 'malformed boundary validator reports shape');
      S.Tokens := Tokens(['poison']); S.StateIndices := Path([42]);
      Check(not CaptureSolvedSequenceSegment(M, G, B, S, V) and
        (Length(S.Tokens) = 0) and (Length(S.StateIndices) = 0),
        'failed boundary capture clears old result arrays');
      G.Free; G := nil;
    end;
    G := NewGraph(1); G.WrapNeighbors := True;
    B := MakeWfcSequenceInitialSegmentBoundary(False); Rejected := False;
    try ApplySequenceModelSegmentToGraph(M, G, B);
    except on E: EWfcSequenceGraph do Rejected := True; end;
    Check(Rejected and (G.RuleGroups.Count = 0), 'wrapped segment rejected before application');
    G.Free; G := NewGraph(1);
    ApplySequenceModelSegmentToGraph(M, G, B);
    Check(not CaptureSolvedSequenceSegment(M, G, B, S, V) and
      (V.Issue.Kind = wsgikEmptyCell), 'unsolved graph capture rejected');
    Check(G.TrySolve(DefaultGraphSolveOptions, R), 'valid graph still solves after empty capture');
    Check(not CaptureSolvedSequenceSegment(Other, G, B, S, V) and
      (V.Issue.Kind = wsgikModelIdentity) and (Length(S.Tokens) = 0),
      'wrong applied model cannot produce segment evidence');
    Check(not ValidateSequenceSegmentStatePath(M, nil, B, V), 'empty path rejected');
    Check(not ValidateSequenceSegmentStatePath(M, Path([-1]), B, V) and
      (V.Issue.Kind = wsgikStateIndex), 'invalid path index rejected');
    Check(not ValidateSequenceSegmentStatePath(M, Path([1]), B, V) and
      (V.Issue.Kind = wsgikStartState), 'initial segment requires observed start');
    Check(not ValidateSequenceSegmentStatePath(M, Path([0,2]), B, V) and
      (V.Issue.Kind = wsgikTransition) and (V.Issue.Position = 0) and
      (V.Issue.RelatedPosition = 1), 'interior transition diagnostic');
    B := MakeWfcSequenceInitialSegmentBoundary(True);
    Check(not ValidateSequenceSegmentStatePath(M, Path([0]), B, V) and
      (V.Issue.Kind = wsgikEndState), 'optional observed end enforced');
    G.Free; G := NewGraph(1);
    B := MakeWfcSequenceContinuingSegmentBoundary(FindState(M,'C'), False);
    ApplySequenceModelSegmentToGraph(M, G, B);
    Check(not G.TrySolve(DefaultGraphSolveOptions, R),
      'valid terminal predecessor without successor is an ordinary contradiction');
    Check(not CaptureSolvedSequenceSegment(M, G, B, S, V) and
      (Length(S.StateIndices) = 0), 'failed solve cannot publish partial segment');
  finally G.Free; Other.Free; M.Free; end;
end;

procedure TestThreePassContinuations;
var M: TWfcSequenceModel; G: TGraph; B: TWfcSequenceSegmentBoundary;
  Bindings: TWfcSequenceProjectionBindings; Rules: TWfcSequenceProjectionRules;
  S: TWfcGeneratedSequenceSegment; V: TWfcSequenceGraphValidationReport;
  R: TGraphNegotiationReport; N: TGraphNegotiationOptions;
  I, J, Prev: Integer; Labels: array[0..2] of String;
begin
  M := LearnSequenceModel(Tokens(['A','B','C']), 4); G := nil;
  Labels[0] := 'harmony'; Labels[1] := 'rhythm'; Labels[2] := 'ensemble';
  SetLength(Rules, M.PublicTokenCount);
  for I := 0 to High(Rules) do
    Rules[I] := MakeWfcSequenceProjectionRule(M.PublicTokenAt(I),
      Tokens([String(M.PublicTokenAt(I))]));
  SetLength(Bindings, 2);
  Bindings[0] := MakeWfcSequenceProjectionBinding(M, Labels[0], Rules);
  Bindings[1] := MakeWfcSequenceProjectionBinding(M, Labels[1], Rules);
  N := DefaultGraphNegotiationOptions; N.SolveOptions.CaptureTrace := True; Prev := -1;
  try
    for I := 0 to 2 do
    begin
      G := NewGraph(1);
      if I = 0 then B := MakeWfcSequenceInitialSegmentBoundary(False)
      else B := MakeWfcSequenceContinuingSegmentBoundary(Prev, I = 2);
      for J := 0 to 2 do
      begin
        if J = 0 then G.CurrentPass := Labels[J] else G.SwitchToPass(Labels[J]);
        G.PassMode := gpmOverlay; G.ClearDependencies;
        ApplySequenceModelSegmentToGraph(M, G, B);
      end;
      RequireSequenceProjectionMapsFromPasses(M, G, Bindings);
      Check(G.DependencyCount = 2, 'ordinary N-source projection identity accepts segment graph');
      Check(G.TrySolveNegotiated(N, R), 'three constrained continuing passes negotiate successfully');
      for J := 0 to 2 do
      begin
        G.SwitchToPass(Labels[J]);
        Check(CaptureSolvedSequenceSegment(M, G, B, S, V) and
          (S.Tokens[0] = M.PublicTokenAt(I)), 'all provider and ensemble seam states are checked');
      end;
      Prev := S.StateIndices[0];
      G.Free; G := nil;
    end;
  finally G.Free; M.Free; end;
end;

begin
  Checks := 0; Failures := 0;
  Run('single-cell segments and early BOS', TestOneCellEarlyBos);
  Run('exact endpoint domains and independent oracle', TestExactDomainsAndIndependentOracle);
  Run('latent identity at ambiguous public seams', TestLatentIdentityNotJustPublicToken);
  Run('legacy initial path parity', TestInitialLegacyParity);
  Run('preflight and failed output reset', TestPreflightAndFailureReset);
  Run('three-pass continuations', TestThreePassContinuations);
  WriteLn('Sequence segment checks: ', Checks, ', failures: ', Failures);
  if Failures <> 0 then Halt(1);
end.
