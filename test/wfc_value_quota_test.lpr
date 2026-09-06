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
program wfc_value_quota_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  Classes, SysUtils, wfc;

type
  TTestProcedure = procedure;
  TInitializedQuotaGraph = class(TGraph)
  protected
    procedure DoInitializePass; override;
  end;
  TQuotaCommitGraph = class(TGraph)
  public
    RejectCommit: Boolean;
    Rejections: Integer;
  protected
    function DoValidateCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean; override;
  end;

var
  Checks, Failures, MutationRejections: Integer;

procedure Check(const ACondition: Boolean; const ALabel: String);
begin
  Inc(Checks);
  if ACondition then Exit;
  Inc(Failures); WriteLn('[FAIL] ', ALabel);
end;

procedure RunTest(const ALabel: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', ALabel);
  try ATest;
  except on E: Exception do
    begin Inc(Failures); WriteLn('[EXCEPTION] ', E.ClassName, ': ', E.Message); end; end;
end;

function Quota(const ALabel: String; const AValues: TGraphValues;
  const AMin, AMax: Integer): TGraphValueQuotaConstraint;
begin
  Result := MakeGraphValueQuotaConstraint(ALabel, AValues, AMin, AMax);
end;

procedure TInitializedQuotaGraph.DoInitializePass;
begin
  inherited DoInitializePass;
  AddValue('A'); AddValue('B');
  RequireValueQuota(Quota('initialized', ['A'], 1, 1));
end;

function TQuotaCommitGraph.DoValidateCommit(out AFailedPassIndex,
  AFailedEntryIndex: Integer): Boolean;
begin
  Result := inherited DoValidateCommit(AFailedPassIndex, AFailedEntryIndex);
  if not RejectCommit then Exit;
  try RequireValueQuota(Quota('commit-added', ['A'], 0, 2));
  except on E: EInvalidOperation do Inc(Rejections); end;
  try RemoveValueQuota('base');
  except on E: EInvalidOperation do Inc(Rejections); end;
  try ClearValueQuotas;
  except on E: EInvalidOperation do Inc(Rejections); end;
  AFailedPassIndex := 0; AFailedEntryIndex := -1; Result := False;
end;

function NewGraph(const W, H, D: Integer): TGraph;
begin
  Result := TGraph.Create.Reshape(W, H, D);
  Result.WrapNeighbors := False;
  Result.AddValue('A'); Result.AddValue('B'); Result.AddValue('C');
end;

function CountValues(const G: TGraph; const Values: TGraphValues): Integer;
var X, Y, Z, I: Integer; V: TGraphValue;
begin
  Result := 0;
  for Z := 0 to Integer(G.Dimension.Depth) - 1 do
    for Y := 0 to Integer(G.Dimension.Height) - 1 do
      for X := 0 to Integer(G.Dimension.Width) - 1 do
      begin
        V := G.Entry[X, Y, Z].Value;
        for I := 0 to High(Values) do
          if V = Values[I] then begin Inc(Result); Break; end;
      end;
end;

function QuotasSatisfied(const G: TGraph): Boolean;
var Q: TGraphValueQuotaConstraints; I, N: Integer;
begin
  Q := G.CopyValueQuotaConstraints;
  for I := 0 to High(Q) do
  begin
    N := CountValues(G, Q[I].Values);
    if (N < Q[I].MinimumCount) or (N > Q[I].MaximumCount) then Exit(False);
  end;
  Result := True;
end;

function State(const G: TGraph): String;
var P, X, Y, Z, I: Integer; E: TGraphEntry; Allowed: TGraphValues;
begin
  Result := IntToStr(G.CurrentPassIndex) + '|';
  for P := 0 to G.TotalPassCount - 1 do
    for Z := 0 to Integer(G.Dimension.Depth) - 1 do
      for Y := 0 to Integer(G.Dimension.Height) - 1 do
        for X := 0 to Integer(G.Dimension.Width) - 1 do
        begin
          E := G.PassGraph[P].Entry[X, Y, Z];
          Result := Result + IntToStr(Length(E.Value)) + ':' + E.Value + ':' +
            IntToStr(Ord(E.Empty)) + ':' + IntToStr(Ord(E.Generated)) + ':' +
            IntToStr(Ord(G.PassGraph[P].HasAllowedValues(X, Y, Z))) + '[';
          Allowed := G.PassGraph[P].CopyAllowedValues(X, Y, Z);
          for I := 0 to High(Allowed) do Result := Result + Allowed[I] + ',';
          Result := Result + '];';
        end;
end;

function Rejected(const G: TGraph; const Q: TGraphValueQuotaConstraint): Boolean;
begin
  Result := False;
  try G.RequireValueQuota(Q);
  except on E: EArgumentException do Result := True;
    on E: ERangeError do Result := True;
    on E: EInvalidOperation do Result := True; end;
end;

procedure TestCopiesAndPasses;
var G: TGraph; Input: TGraphValues; Q: TGraphValueQuotaConstraint;
  Saved: TGraphValueQuotaConstraints;
begin
  G := NewGraph(3, 1, 1);
  try
    Input := ['C', 'A', 'C']; Q := Quota('chosen', Input, 1, 2);
    Input[0] := 'B';
    Check(Q.Values[0] = 'C', 'factory deep-copies its values array');
    Check(G.RequireValueQuota(Q) = G, 'RequireValueQuota is fluent');
    Q.LabelText := 'changed'; Q.Values[0] := 'B'; Q.MinimumCount := 0;
    Saved := G.CopyValueQuotaConstraints;
    Check((Length(Saved) = 1) and (Saved[0].LabelText = 'chosen') and
      (Saved[0].MinimumCount = 1) and (Saved[0].MaximumCount = 2),
      'registration owns descriptor metadata');
    Check((Length(Saved[0].Values) = 2) and (Saved[0].Values[0] = 'A') and
      (Saved[0].Values[1] = 'C'), 'value set deduplicates in registration order');
    Saved[0].Values[0] := 'B'; Saved[0].MaximumCount := 99;
    Saved := G.CopyValueQuotaConstraints;
    Check((Saved[0].Values[0] = 'A') and (Saved[0].MaximumCount = 2),
      'copied descriptor arrays are detached from graph storage');
    Check(G.RequireValueQuota(Quota('chosen', ['A', 'C', 'A'], 1, 2)) = G,
      'canonical-equivalent registration is fluent');
    Check(Length(G.CopyValueQuotaConstraints) = 1, 'equivalent label registration is idempotent');
    Check(Rejected(G, Quota('chosen', ['A', 'C'], 1, 3)), 'conflicting label rejects');
    Saved := G.CopyValueQuotaConstraints;
    Check((Length(Saved) = 1) and (Saved[0].MaximumCount = 2),
      'conflicting registration does not replace prior descriptor');
    Check((G.RemoveValueQuota('missing') = G) and
      (Length(G.CopyValueQuotaConstraints) = 1), 'missing removal is a fluent no-op');
    G.SwitchToPass('upper'); G.AddValue('A'); G.AddValue('B');
    Check(Length(G.CopyValueQuotaConstraints) = 0, 'new passes do not inherit quotas');
    G.RequireValueQuota(Quota('chosen', ['B'], 0, 3));
    Check((Length(G.CopyValueQuotaConstraints) = 1) and
      (Length(G.PassGraph[0].CopyValueQuotaConstraints) = 1), 'same label is pass-local');
    Check((G.RemoveValueQuota('chosen') = G) and
      (Length(G.CopyValueQuotaConstraints) = 0), 'selected-pass removal is fluent');
    G.RequireValueQuota(Quota('chosen', ['A'], 0, 3));
    Check((G.ClearValueQuotas = G) and (Length(G.CopyValueQuotaConstraints) = 0) and
      (Length(G.PassGraph[0].CopyValueQuotaConstraints) = 1), 'clear is fluent and pass-local');
    Check(G.PassGraph[0].RemoveValueQuota('chosen') = G.PassGraph[0],
      'PassGraph removal addresses that pass regardless of selection');
    G.RequireValueQuota(Quota('reset', ['A'], 0, 10));
    Check(G.Reset = G, 'Reset is fluent');
    Check((G.TotalPassCount = 1) and (G.Dimension.Width = 0) and
      (Length(G.CopyRegisteredValues) = 0) and
      (Length(G.CopyValueQuotaConstraints) = 0), 'Reset clears quotas with all definitions and passes');
  finally G.Free; end;
end;

procedure TestValidation;
var G: TGraph; Q: TGraphValueQuotaConstraint; Saved: TGraphValueQuotaConstraints;
  R: TGraphSolveReport; Before: String;
begin
  G := NewGraph(2, 1, 1);
  try
    G.RequireValueQuota(Quota('base', ['A'], 0, 2)); Before := State(G);
    Check(Rejected(G, Quota('', ['A'], 0, 1)), 'empty quota label rejected');
    Check(Rejected(G, Quota('empty', nil, 0, 1)), 'empty value set rejected');
    Check(Rejected(G, Quota('unknown', ['outside'], 0, 1)), 'unregistered value rejected');
    Check(Rejected(G, Quota('reserved', [TGraphValue.Empty], 0, 1)), 'reserved empty value rejected');
    Check(Rejected(G, Quota('negative', ['A'], -1, 1)), 'negative minimum rejected');
    Check(Rejected(G, Quota('negative-max', ['A'], 0, -1)), 'negative maximum rejected');
    Check(Rejected(G, Quota('inverted', ['A'], 2, 1)), 'inverted quota rejected');
    Saved := G.CopyValueQuotaConstraints;
    Check((Length(Saved) = 1) and (Saved[0].LabelText = 'base') and (State(G) = Before),
      'invalid registrations preserve definitions and caller state atomically');
    Q := Quota('wide-max', ['B'], 0, High(Integer));
    Check(G.RequireValueQuota(Q) = G, 'maximum above cell count remains legal');
    Check(G.TrySolve(DefaultGraphSolveOptions, R) and QuotasSatisfied(G),
      'wide maximum solves with independent recount');
    G.RequireValueQuota(Quota('too-many', ['C'], 3, 4)); Before := State(G);
    Check(not G.TrySolve(DefaultGraphSolveOptions, R) and
      (R.Contradiction.Kind = gckValueQuota), 'minimum above cell count is unsatisfiable, not invalid');
    Check(State(G) = Before, 'unsatisfiable quota preserves existing generated output');
    Check(G.Reshape(1, 1, 1) = G, 'reshape preserves a legal but currently unsatisfiable quota');
    Check(Length(G.CopyValueQuotaConstraints) = 3, 'reshape retains quota definitions');
  finally G.Free; end;
end;

function MutateWhileRunning(const G: TGraph; const E: TGraphEntry;
  const Values: TGraphValues): TGraphValue;
begin
  try G.RequireValueQuota(Quota('during-run', ['A'], 0, 2));
  except on X: EInvalidOperation do Inc(MutationRejections); end;
  try G.RemoveValueQuota('missing');
  except on X: EInvalidOperation do Inc(MutationRejections); end;
  try G.ClearValueQuotas;
  except on X: EInvalidOperation do Inc(MutationRejections); end;
  if Length(Values) = 0 then Result := E.Value else Result := Values[0];
end;

procedure TestMutationAndLegacy;
var G, Twin: TGraph; Before: String; Raised: Boolean;
begin
  G := NewGraph(1, 1, 1);
  try
    MutationRejections := 0; G.SelectionCallback := MutateWhileRunning;
    G.Run;
    Check((MutationRejections = 3) and (Length(G.CopyValueQuotaConstraints) = 0),
      'running callbacks cannot require, remove, or clear quota definitions');
  finally G.Free; end;
  G := NewGraph(2, 1, 1); Twin := NewGraph(2, 1, 1);
  try
    G.Seed := 987; Twin.Seed := 987;
    G.RequireValueQuota(Quota('finite-only', ['A'], 0, 2));
    Before := State(G); Raised := False;
    try G.Run; except on E: EInvalidOperation do Raised := True; end;
    Check(Raised and (State(G) = Before), 'legacy Run rejects quotas before mutation');
    Check(G.RandomIndex(1000000) = Twin.RandomIndex(1000000),
      'legacy quota rejection preserves the random stream');
  finally Twin.Free; G.Free; end;
end;

procedure TestDerivedHooks;
var G: TInitializedQuotaGraph; H, Twin: TQuotaCommitGraph;
  R: TGraphSolveReport; Before: String;
begin
  G := TInitializedQuotaGraph.Create;
  try
    G.Reshape(3, 1, 1); G.WrapNeighbors := False;
    Check(Length(G.CopyValueQuotaConstraints) = 1,
      'derived initializer can register a quota while creating the first pass');
    G.SwitchToPass('upper');
    Check((Length(G.CopyValueQuotaConstraints) = 1) and
      (Length(G.PassGraph[0].CopyValueQuotaConstraints) = 1),
      'later derived pass initialization keeps independent quota registries');
    Check(G.TrySolve(DefaultGraphSolveOptions, R) and
      QuotasSatisfied(G.PassGraph[0]) and QuotasSatisfied(G.PassGraph[1]),
      'derived initialized quotas solve and independently recount on both passes');
  finally G.Free; end;
  H := TQuotaCommitGraph.Create; Twin := TQuotaCommitGraph.Create;
  try
    H.Reshape(3, 1, 1); Twin.Reshape(3, 1, 1);
    H.AddValue('A'); H.AddValue('B'); Twin.AddValue('A'); Twin.AddValue('B');
    H.Seed := 78; Twin.Seed := 78;
    H.RequireValueQuota(Quota('base', ['A'], 1, 1));
    Twin.RequireValueQuota(Quota('base', ['A'], 1, 1));
    H.RejectCommit := True; Before := State(H);
    Check(not H.TrySolve(DefaultGraphSolveOptions, R) and
      (R.Contradiction.Kind = gckFinalValidation), 'quota-bearing commit rejection fails the solve');
    Check(H.Rejections = 3, 'finite commit hook cannot require, remove, or clear quotas');
    Check((State(H) = Before) and (Length(H.CopyValueQuotaConstraints) = 1) and
      (H.CopyValueQuotaConstraints[0].LabelText = 'base'),
      'rejected quota mutation and commit preserve registry and graph state');
    Check(H.RandomIndex(1000000) = Twin.RandomIndex(1000000),
      'rejected quota-bearing commit restores the random stream');
    H.RejectCommit := False;
    Check(H.TrySolve(DefaultGraphSolveOptions, R) and QuotasSatisfied(H),
      'graph remains reusable after guarded quota commit rollback');
  finally Twin.Free; H.Free; end;
end;

procedure TestWholePassAndLocks;
var G: TGraph; R: TGraphSolveReport; Before: String;
begin
  G := NewGraph(2, 2, 2);
  try
    G.RequireValueQuota(Quota('AB', ['A', 'B'], 3, 3));
    G.RequireValueQuota(Quota('BC', ['B', 'C'], 6, 6));
    G.Entry[0, 0, 0].Value := 'A';
    Check(G.TrySolve(DefaultGraphSolveOptions, R), 'overlapping whole-volume quotas solve');
    Check(QuotasSatisfied(G) and (CountValues(G, ['A']) = 2) and
      (CountValues(G, ['B']) = 1) and (CountValues(G, ['C']) = 5),
      'independent recount proves every physical cell contributes once');
    Check((G.Entry[0, 0, 0].Value = 'A') and not G.Entry[0, 0, 0].Generated,
      'caller lock counts toward quotas without becoming generated');
    G.SetAllowedValues(1, 1, 1, TGraphValues(nil)); Before := State(G);
    Check(not G.TrySolve(DefaultGraphSolveOptions, R), 'explicit empty domain stays contradictory with quotas');
    Check(State(G) = Before, 'empty-domain failure is atomic');
  finally G.Free; end;
  G := NewGraph(3, 1, 1);
  try
    G.RequireValueQuota(Quota('zero-A', ['A'], 0, 0));
    G.RequireValueQuota(Quota('all-B', ['B'], 3, 3));
    Check(G.TrySolve(DefaultGraphSolveOptions, R) and QuotasSatisfied(G) and
      (CountValues(G, ['B']) = 3), 'zero and tight minimum quotas force exact values');
    G.Entry[0, 0, 0].Value := 'A'; Before := State(G);
    Check(not G.TrySolve(DefaultGraphSolveOptions, R), 'a conflicting caller lock cannot be pruned away');
    Check(State(G) = Before, 'conflicting lock remains caller-owned after failure');
  finally G.Free; end;
end;

function TerrainFixture: TGraph;
var X: Integer;
begin
  Result := TGraph.Create.Reshape(5, 1, 1);
  Result.Seed := 246813579; Result.WrapNeighbors := False;
  Result.CurrentPass := 'terrain'; Result.PassMode := gpmOverlay;
  Result.AddValue('land'); Result.AddValue('water');
  for X := 0 to 4 do
    if X < 3 then Result.SetAllowedValues(X, 0, 0, 'land')
    else Result.SetAllowedValues(X, 0, 0, 'water');
  Result.RequireValueQuota(Quota('dry-ground', ['land'], 3, 3));
  Result.SwitchToPass('housing'); Result.PassMode := gpmOverlay; Result.ClearDependencies;
  Result.AddValue('none'); Result.AddValue('home').RequireFromPass('terrain', 'land');
  Result.RequireValueQuota(Quota('homes', ['home'], 2, 2));
  Result.SwitchToPass('foliage'); Result.PassMode := gpmOverlay; Result.ClearDependencies;
  Result.AddValue('empty');
  Result.AddValue('grass').RequireFromPass('terrain', 'land').RequireFromPass('housing', 'none');
  Result.AddValue('reeds').RequireFromPass('terrain', 'water').RequireFromPass('housing', 'none');
  Result.RequireValueQuota(Quota('grass', ['grass'], 1, 1));
end;

procedure TestPipelineAndRollback;
var G, Twin: TGraph; R: TGraphSolveReport; Before: String; X, P: Integer;
  Good: Boolean;
begin
  G := TerrainFixture; Twin := TerrainFixture;
  try
    Check(G.TrySolve(DefaultGraphSolveOptions, R) and
      Twin.TrySolve(DefaultGraphSolveOptions, R), 'terrain, housing, and foliage quota pipeline solves');
    Check((CountValues(G.PassGraph[1], ['home']) = 2) and
      (CountValues(G.PassGraph[2], ['grass']) = 1), 'independent recount confirms exact housing and foliage');
    Good := True;
    for P := 0 to 2 do Good := Good and QuotasSatisfied(G.PassGraph[P]);
    for X := 0 to 4 do
    begin
      if G.PassGraph[1].Entry[X, 0, 0].Value = 'home' then
        Good := Good and (G.PassGraph[0].Entry[X, 0, 0].Value = 'land');
      if G.PassGraph[2].Entry[X, 0, 0].Value = 'grass' then
        Good := Good and (G.PassGraph[0].Entry[X, 0, 0].Value = 'land') and
          (G.PassGraph[1].Entry[X, 0, 0].Value = 'none');
    end;
    Check(Good, 'public recount and direct coordinates validate pass dependencies');
    Check(State(G) = State(Twin), 'same seed replays the complete quota pipeline');
    G.RemoveValueQuota('grass').RequireValueQuota(Quota('grass', ['grass'], 4, 4));
    Before := State(G);
    Check(not G.TrySolve(DefaultGraphSolveOptions, R) and
      (R.FailedPassIndex = 2) and (R.Contradiction.Kind = gckValueQuota),
      'late impossible foliage quota rejects freshly staged housing');
    Check(R.Passes[1].Decisions > 0, 'failed pipeline exercised random housing decisions');
    Check(State(G) = Before, 'late quota failure restores values, flags, domains, and selected pass');
    Good := True;
    for P := 0 to 2 do
      for X := 0 to 1 do
        Good := Good and (G.PassGraph[P].RandomIndex(1000000) =
          Twin.PassGraph[P].RandomIndex(1000000));
    Check(Good, 'late failure restores every pass random stream');
  finally Twin.Free; G.Free; end;
end;

procedure TestNegotiationAndPreservedProviders;
var G: TGraph; R: TGraphSolveReport; N: TGraphNegotiationReport;
  S: TGraphSelectiveNegotiationReport; Before: String; Raised: Boolean;
begin
  G := TGraph.Create.Reshape(1, 1, 1);
  try
    G.Seed := 0; G.WrapNeighbors := False;
    G.CurrentPass := 'terrain'; G.PassMode := gpmOverlay;
    G.AddValue('marsh'); G.AddValue('meadow');
    G.SwitchToPass('housing'); G.PassMode := gpmOverlay; G.ClearDependencies;
    G.AddValue('none'); G.AddValue('home').RequireFromPass('terrain', 'meadow');
    G.RequireValueQuota(Quota('home', ['home'], 1, 1));
    Check(not G.TrySolve(DefaultGraphSolveOptions, R), 'one-way solve cannot reopen an unsuitable provider');
    Check(G.TrySolveNegotiated(DefaultGraphNegotiationOptions, N) and
      (N.PassBacktracks > 0), 'negotiation reopens provider choices for the downstream quota');
    Check((G.PassGraph[0].Entry[0, 0, 0].Value = 'meadow') and QuotasSatisfied(G),
      'negotiated output independently satisfies quota and dependency');
  finally G.Free; end;
  G := TerrainFixture;
  try
    Check(G.TrySolve(DefaultGraphSolveOptions, R), 'preserved-provider fixture initially solves');
    G.PassGraph[1].RemoveValueQuota('homes').RequireValueQuota(Quota('homes', ['home'], 0, 0));
    Before := State(G); Raised := False;
    try G.TryRegenerateFrom('foliage', DefaultGraphSolveOptions, R);
    except on E: EInvalidOperation do Raised := True; end;
    Check(Raised and (State(G) = Before), 'selective solve refuses a preserved provider violating its quota');
    Raised := False;
    try G.TryRegenerateNegotiatedFrom('foliage', DefaultGraphNegotiationOptions, S);
    except on E: EInvalidOperation do Raised := True; end;
    Check(Raised and (State(G) = Before), 'selective negotiation cannot silently repair an out-of-scope provider');
    Check(G.TrySolveNegotiated(DefaultGraphNegotiationOptions, N),
      'full authorized negotiation regenerates the invalid provider');
    Check((CountValues(G.PassGraph[1], ['home']) = 0) and
      QuotasSatisfied(G.PassGraph[2]), 'full regeneration recount matches changed provider quota');
    G.PassGraph[1].Entry[0, 0, 0].ClearValue; Before := State(G); Raised := False;
    try G.TryRegenerateFrom('foliage', DefaultGraphSolveOptions, R);
    except on E: EInvalidOperation do Raised := True; end;
    Check(Raised and (State(G) = Before), 'quota-bearing preserved provider must be completely assigned');
  finally G.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestBrowserNumbers;
var G: TGraph; Q: TGraphValueQuotaConstraint; X, I, J: Integer;
begin
  G := NewGraph(2, 1, 1);
  try
    G.RequireValueQuota(Quota('base', ['A'], 0, 2));
    for I := 0 to 7 do
    begin
      case I of
        0: asm X = NaN; end;
        1: asm X = Infinity; end;
        2: asm X = -Infinity; end;
        3: asm X = 0.5; end;
        4: asm X = undefined; end;
        5: asm X = "0"; end;
        6: asm X = null; end;
        7: asm X = 4294967296; end;
      end;
      for J := 0 to 1 do
      begin
        Q := Quota('host-bounds', ['A'], 0, 2);
        if J = 0 then Q.MinimumCount := X else Q.MaximumCount := X;
        Check(Rejected(G, Q), 'malformed browser quota bound rejected');
        Check(Length(G.CopyValueQuotaConstraints) = 1, 'malformed browser bound cannot partially register');
      end;
    end;
  finally G.Free; end;
end;
{$ENDIF}

begin
  RunTest('factory ownership, canonical sets, fluent pass-local registry and Reset', @TestCopiesAndPasses);
  RunTest('validation and atomic registry changes', @TestValidation);
  RunTest('running mutation guard and legacy preflight', @TestMutationAndLegacy);
  RunTest('derived initialization and finite commit-hook guards', @TestDerivedHooks);
  RunTest('whole-pass quotas, overlap, locks, and empty domains', @TestWholePassAndLocks);
  RunTest('terrain/housing/foliage independent recount and rollback', @TestPipelineAndRollback);
  RunTest('negotiation and immutable provider validation', @TestNegotiationAndPreservedProviders);
  {$IFDEF PAS2JS}RunTest('strict browser numeric bounds', @TestBrowserNumbers);{$ENDIF}
  WriteLn('Value-quota public checks: ', Checks - Failures, '/', Checks);
  if Failures <> 0 then Halt(1);
end.
