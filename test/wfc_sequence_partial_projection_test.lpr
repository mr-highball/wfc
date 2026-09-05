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
program wfc_sequence_partial_projection_test;
{$mode delphi}{$H+}
uses SysUtils, wfc, wfc_model, wfc_sequence, wfc_sequence_learn, wfc_sequence_graph
  {$IFDEF PAS2JS}, wfc_browser_test_host{$ENDIF};

var Checks, Failures: Integer;
procedure Check(const B: Boolean; const S: String);
begin Inc(Checks); if not B then begin Inc(Failures); WriteLn('FAIL: ', S); end; end;

function Vocabulary(const A: array of String): TWfcSequenceModel;
var Samples: TWfcSequenceSamples; T: TWfcModelTokens; I: Integer;
begin
  SetLength(Samples, Length(A)); SetLength(T, 1);
  for I := 0 to High(A) do begin T[0] := A[I]; Samples[I] := MakeWfcSequenceSample(T); end;
  Result := LearnSequenceModelCorpus(Samples, 1);
end;

function Relation(const M: TWfcSequenceModel; const LabelText: String;
  const TAlternatives, UAlternatives: array of String): TWfcSequenceProjectionBinding;
var Rules: TWfcSequenceProjectionRules; T: TWfcModelTokens; I: Integer;
begin
  SetLength(Rules, 2); SetLength(T, Length(TAlternatives));
  for I := 0 to High(T) do T[I] := TAlternatives[I];
  Rules[0] := MakeWfcSequenceProjectionRule('T', T);
  T := nil; SetLength(T, Length(UAlternatives));
  for I := 0 to High(T) do T[I] := UAlternatives[I];
  Rules[1] := MakeWfcSequenceProjectionRule('U', T);
  Result := MakeWfcSequenceProjectionBinding(M, LabelText, Rules);
end;

function Graph(const P, Q, Target: TWfcSequenceModel; const Plain: Boolean = False): TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Seed := 0; Result.Reshape(1, 1, 1); Result.WrapNeighbors := False;
    Result.CurrentPass := 'p'; Result.PassMode := gpmOverlay; Result.ClearDependencies;
    ApplySequenceModelToGraph(P, Result, wseWhole);
    Result.SwitchToPass('q'); Result.PassMode := gpmOverlay; Result.ClearDependencies;
    ApplySequenceModelToGraph(Q, Result, wseWhole);
    Result.SwitchToPass('target'); Result.PassMode := gpmOverlay; Result.ClearDependencies;
    if Plain then Result.AddValue('witness') else ApplySequenceModelToGraph(Target, Result, wseWhole);
  except Result.Free; raise; end;
end;

procedure TestPartialRelations;
var P, Q, T: TWfcSequenceModel; G: TGraph; B: TWfcSequenceProjectionBindings;
  R: TGraphSolveReport; D: TWfcGeneratedSequence; Proof: TWfcSequenceGraphValidationReport;
  I: Integer; Rejected: Boolean;
begin
  P := Vocabulary(['a', 'b']); Q := Vocabulary(['x']); T := Vocabulary(['T', 'U']);
  try
    SetLength(B, 1); B[0] := Relation(P, 'p', ['a'], []);
    for I := 0 to 1 do
    begin
      G := Graph(P, Q, T);
      try
        if I = 0 then IntersectSequenceAllowedTokens(P, G.PassGraph[0], 0, 'a')
        else IntersectSequenceAllowedTokens(P, G.PassGraph[0], 0, 'b');
        RequireSequencePartialProjectionMapsFromPasses(T, G, B);
        Check(G.DependencyCount = 1, 'partial relation retains its provider dependency');
        Check(G.PassGraph[0].Entry[0, 0, 0].Empty, 'provider starts genuinely unsolved');
        Check(G.TrySolve(DefaultGraphSolveOptions, R) = (I = 0), 'empty alternative explicitly denies target token');
        if I = 0 then
        begin
          Check(CaptureSolvedSequence(T, G.PassGraph[2], D, Proof), 'supported partial relation captures');
          Check(D.Tokens[0] = 'T', 'unsupported U cannot leak through zero-match clause');
        end
        else Check(G.PassGraph[2].Entry[0, 0, 0].Empty, 'failed unsupported target does not commit');
      finally G.Free; end;
    end;
    G := Graph(P, Q, T);
    try
      RequireSequencePartialProjectionMapsFromPasses(T, G, B);
      IntersectSequenceAllowedTokens(T, G, 0, 'U');
      Check(not G.TrySolve(DefaultGraphSolveOptions, R), 'caller lock/domain cannot revive unsupported target');
      Check(G.HasAllowedValues(0, 0, 0), 'partial relation leaves caller domains explicit');
    finally G.Free; end;

    for I := 0 to 1 do
    begin
      G := Graph(P, Q, T);
      try
        B[0] := Relation(P, 'p', ['a', 'b'], []);
        RequireSequencePartialProjectionMapsFromPasses(T, G, B);
        B[0] := Relation(P, 'p', ['b'], []);
        RequireSequencePartialProjectionMapsFromPasses(T, G, B);
        if I = 0 then IntersectSequenceAllowedTokens(P, G.PassGraph[0], 0, 'a')
        else IntersectSequenceAllowedTokens(P, G.PassGraph[0], 0, 'b');
        Check(G.TrySolve(DefaultGraphSolveOptions, R) = (I = 1), 'repeated partial relations are AND, not merged OR');
      finally G.Free; end;
    end;

    G := Graph(P, Q, T);
    try
      B[0] := Relation(P, 'p', ['a'], []); Rejected := False;
      try RequireSequenceProjectionMapsFromPasses(T, G, B);
      except on EArgumentException do Rejected := True; end;
      Check(Rejected and (G.DependencyCount = 0), 'old complete-map API still rejects empty alternatives atomically');
      B[0] := Relation(P, 'p', ['a'], ['a']);
      RequireSequenceProjectionMapsFromPasses(T, G, B);
      B[0] := Relation(P, 'p', ['b'], ['b']);
      RequireSequenceProjectionMapsFromPasses(T, G, B);
      IntersectSequenceAllowedTokens(P, G.PassGraph[0], 0, 'a');
      Check(G.TrySolve(DefaultGraphSolveOptions, R), 'historical complete-map repeated-provider OR semantics remain intact');
    finally G.Free; end;
  finally T.Free; Q.Free; P.Free; end;
end;

procedure TestAtomicPreflightAndPlainWitness;
var P, Q, T: TWfcSequenceModel; G: TGraph; B: TWfcSequenceProjectionBindings;
  R: TGraphSolveReport; I: Integer; Rejected: Boolean; Tokens: TWfcModelTokens;
begin
  P := Vocabulary(['a']); Q := Vocabulary(['x']); T := Vocabulary(['T', 'U']);
  try
    for I := 0 to 3 do
    begin
      G := Graph(P, Q, T);
      try
        SetLength(B, 2); B[0] := Relation(P, 'p', ['a'], []);
        B[1] := Relation(Q, 'q', ['x'], ['x']);
        case I of
          0: B[1].Rules[1].SourceTokens[0] := 'unknown';
          1: B[1].SourcePass := 'missing';
          2: B[1] := B[0];
          3: B[1].Rules[1].TargetToken := 'T';
        end;
        Rejected := False;
        try RequireSequencePartialProjectionMapsFromPasses(T, G, B);
        except on Exception do Rejected := True; end;
        Check(Rejected and (G.DependencyCount = 0), 'malformed later binding leaves earlier relation uninstalled ' + IntToStr(I));
        IntersectSequenceAllowedTokens(T, G, 0, 'U');
        Check(G.TrySolve(DefaultGraphSolveOptions, R), 'failed preflight did not silently forbid U');
      finally G.Free; end;
    end;
    G := Graph(P, Q, T, True);
    try
      Tokens := nil;
      RequirePartialProjectedSequenceFromPass(P, G, 'witness', 'p', Tokens);
      Check((G.DependencyCount = 1) and G.PassGraph[0].Entry[0, 0, 0].Empty,
        'plain denied witness keeps singleton unsolved provider dependency');
      Check(not G.TrySolve(DefaultGraphSolveOptions, R), 'singleton provider resolves before explicit absence denial');
      Check(G[0, 0, 0].Empty, 'denied plain witness never commits');
    finally G.Free; end;
    G := Graph(P, Q, T, True);
    try
      SetLength(Tokens, 1); Tokens[0] := 'a';
      RequirePartialProjectedSequenceFromPass(P, G, 'witness', 'p', Tokens);
      Tokens[0] := 'changed';
      Check(G.TrySolve(DefaultGraphSolveOptions, R), 'plain witness retains detached expanded alternatives');
    finally G.Free; end;
  finally T.Free; Q.Free; P.Free; end;
end;

begin
  try
    Check(WFC_SEQUENCE_PARTIAL_PROJECTION_VERSION = 1, 'partial relation version');
    Check((WFC_SEQUENCE_GRAPH_ADAPTER_VERSION = 1) and (WFC_SEQUENCE_SEGMENT_VERSION = 1), 'old versions unchanged');
    TestPartialRelations;
    TestAtomicPreflightAndPlainWitness;
  except on E: Exception do begin Inc(Failures); WriteLn(E.ClassName, ': ', E.Message); end; end;
  WriteLn('Sequence partial projection checks: ', Checks, ', failures: ', Failures);
  if Failures <> 0 then Halt(1);
end.
