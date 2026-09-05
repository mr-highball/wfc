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
program wfc_text_pass_transaction_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  Classes, SysUtils, wfc, wfc_trace, wfc_sequence_graph, wfc_text_passes,
  text_pass_composition_showcase;

const
  CELL_COUNT = WFC_TEXT_PASS_SHOWCASE_LENGTH;
  RANDOM_BOUND = 1000000007;
  SAMPLE_COUNT = 6;

type
  TCellState = record
    Entry: TGraphEntry;
    Value: TGraphValue;
    Empty, Generated, HasDomain: Boolean;
    Domain: TGraphValues;
  end;
  TPipelineState = array[TWfcTextPassLayer, 0..CELL_COUNT - 1] of TCellState;
  TRandomSamples = array[0..3, 0..SAMPLE_COUNT - 1] of Integer;
  TCaseEvidence = record
    RandomSamples: TRandomSamples;
    Status: TWfcTextPassStatus;
    FailedLayer: TWfcTextPassLayer;
    CoreStatus: TGraphSolveStatus;
    Text: String;
  end;

var
  Checks, Failures: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if ACondition then Exit;
  Inc(Failures);
  WriteLn('FAIL: ', AMessage);
end;

function Options(const ACapture: Boolean): TGraphSolveOptions;
begin
  Result := DefaultGraphSolveOptions;
  Result.CaptureTrace := ACapture;
end;

function Snapshot(const P: TWfcTextPassPipeline): TPipelineState;
var
  Layer: TWfcTextPassLayer;
  I: Integer;
  G: TGraph;
begin
  Result := Default(TPipelineState);
  for Layer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
  begin
    G := P.LayerGraph[Layer];
    for I := 0 to CELL_COUNT - 1 do
    begin
      Result[Layer, I].Entry := G.Entry[I, 0, 0];
      Result[Layer, I].Value := G.Entry[I, 0, 0].Value;
      Result[Layer, I].Empty := G.Entry[I, 0, 0].Empty;
      Result[Layer, I].Generated := G.Entry[I, 0, 0].Generated;
      Result[Layer, I].HasDomain := G.HasAllowedValues(I, 0, 0);
      Result[Layer, I].Domain := G.CopyAllowedValues(I, 0, 0);
    end;
  end;
end;

function SameValues(const A, B: TGraphValues): Boolean;
var I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do if A[I] <> B[I] then Exit(False);
  Result := True;
end;

procedure CheckSnapshot(const P: TWfcTextPassPipeline;
  const Before: TPipelineState; const AContext: String);
var
  After: TPipelineState;
  Layer: TWfcTextPassLayer;
  I: Integer;
begin
  After := Snapshot(P);
  for Layer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
    for I := 0 to CELL_COUNT - 1 do
    begin
      Check((After[Layer, I].Entry = Before[Layer, I].Entry)
        and (After[Layer, I].Value = Before[Layer, I].Value)
        and (After[Layer, I].Empty = Before[Layer, I].Empty)
        and (After[Layer, I].Generated = Before[Layer, I].Generated),
        AContext + ' cell identity/value/ownership ' + IntToStr(Ord(Layer)) + '/' + IntToStr(I));
      Check((After[Layer, I].HasDomain = Before[Layer, I].HasDomain)
        and SameValues(After[Layer, I].Domain, Before[Layer, I].Domain),
        AContext + ' caller domain ' + IntToStr(Ord(Layer)) + '/' + IntToStr(I));
    end;
end;

procedure AdvanceStreams(const P: TWfcTextPassPipeline);
var Layer: TWfcTextPassLayer; I: Integer;
begin
  for Layer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
    for I := 0 to 6 + Ord(Layer) do P.LayerGraph[Layer].RandomIndex(RANDOM_BOUND);
  P.Graph.SwitchToPass(WFC_TEXT_PASS_LEXICAL);
  for I := 0 to 3 do P.Graph.RandomIndex(RANDOM_BOUND);
end;

function ReadStreams(const P: TWfcTextPassPipeline): TRandomSamples;
var Layer: TWfcTextPassLayer; I: Integer;
begin
  for Layer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
    for I := 0 to SAMPLE_COUNT - 1 do
      Result[Ord(Layer), I] := P.LayerGraph[Layer].RandomIndex(RANDOM_BOUND);
  //The root-facing API delegates to its selected pass outside a solve.
  //This also tests selected-pass restoration, not an inaccessible root RNG.
  for I := 0 to SAMPLE_COUNT - 1 do Result[3, I] := P.Graph.RandomIndex(RANDOM_BOUND);
end;

procedure CheckSamples(const A, B: TRandomSamples; const AContext: String);
var I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to SAMPLE_COUNT - 1 do
      Check(A[I, J] = B[I, J], AContext + ' stream ' + IntToStr(I) + '/' + IntToStr(J));
end;

function IsClear(const R: TWfcTextPassResult): Boolean;
begin
  Result := (R.Text = '')
    and (Length(R.Structure.StateIndices) = 0) and (Length(R.Structure.Tokens) = 0)
    and (Length(R.Lexical.StateIndices) = 0) and (Length(R.Lexical.Tokens) = 0)
    and (Length(R.Punctuation.StateIndices) = 0) and (Length(R.Punctuation.Tokens) = 0);
end;

procedure CheckFailurePublication(const P: TWfcTextPassPipeline;
  const R: TWfcTextPassResult; const Report: TWfcTextPassReport;
  const Layer: TWfcTextPassLayer; const ACapture: Boolean;
  const AStatus: TWfcTextPassStatus);
var I: Integer;
begin
  Check(IsClear(R), 'failure discards all pending public sequences/text');
  Check((Report.Status = AStatus) and (Report.FailedLayer = Layer),
    'public failure preserves its semantic status and layer');
  Check((Report.Solve.Status = gssContradiction)
    and (Report.Solve.Contradiction.Kind = gckFinalValidation)
    and (Report.Solve.FailedPassIndex = Ord(Layer))
    and (Report.Solve.Contradiction.PassIndex = Ord(Layer)),
    'False owner result cannot hide a committed gssSolved transaction');
  Check(not Report.Solve.TraceCaptured and (Report.Solve.TraceHash = 0)
    and (Length(Report.Solve.Trace) = 0), 'private generic trace is stripped');
  for I := 0 to High(Report.Solve.Passes) do
    Check((Report.Solve.Passes[I].TraceStart = -1)
      and (Report.Solve.Passes[I].TraceCount = 0), 'generic private slices are stripped');
  if ACapture then
  begin
    Check(Report.TraceCaptured and (Report.TraceHash <> 0)
      and Report.TraceValidation.Valid and (Length(Report.Trace) > 0),
      'capture retains a validated public rollback trace');
    if Length(Report.Trace) > 0 then
      Check(Report.Trace[High(Report.Trace)].Kind = gtekPipelineRollback,
        'public trace terminates with rollback, never commit');
    for I := 0 to High(Report.Trace) do
      Check(Pos('@wfcs', String(Report.Trace[I].Token)) = 0,
        'public failure trace contains no graph-private key');
  end
  else Check(not Report.TraceCaptured and (Report.TraceHash = 0)
    and (Length(Report.Trace) = 0), 'disabled failure capture stays empty');
  Check(not P.Graph.Running, 'failure releases the graph transaction guard');
end;

procedure Baseline(const S: TTextPassCompositionShowcase; const Capture: Boolean;
  out R: TWfcTextPassResult);
var Report: TWfcTextPassReport;
begin
  Check(S.Pipeline.TryGenerate(Options(Capture), R, Report), 'baseline commits');
  Check((Report.Status = wtpsCompleted) and (R.Text = 'A sun rises brightly!')
    and (TextPassShowcaseSignature(R) = '1:69ABA6CE'), 'existing successful output golden');
  if Capture then Check(Report.TraceHash = TGraphTraceSignature(2412171679),
    'existing successful trace golden');
end;

procedure MutateWeight(const P: TWfcTextPassPipeline;
  const Layer, ClearLayer: TWfcTextPassLayer; out Key: TGraphValue;
  out OldWeight: TGraphWeight);
var G: TGraph; Values: TGraphValues;
begin
  G := P.LayerGraph[Layer]; Values := G.CopyRegisteredValues;
  Key := Values[0]; OldWeight := G.Rules[Key].Weight;
  G.Rules[Key].Weight := OldWeight + 1;
  //Retain a real caller lock and an exact caller domain through rollback.
  G.Entry[0, 0, 0].Value := G.Entry[0, 0, 0].Value;
  G.SetAllowedValues(CELL_COUNT - 1, 0, 0, G.Entry[CELL_COUNT - 1, 0, 0].Value);
  P.LayerGraph[ClearLayer].Entry[1, 0, 0].ClearValue;
end;

function RunCaptureFailure(const Layer: TWfcTextPassLayer;
  const Scope: Integer; const Capture: Boolean): TCaseEvidence;
var
  Owner, Idle: TTextPassCompositionShowcase;
  Base, OtherBase, R, Retry: TWfcTextPassResult;
  Report, OtherReport: TWfcTextPassReport;
  Before: TPipelineState;
  Key, OtherKey: TGraphValue;
  Weight, OtherWeight: TGraphWeight;
  IdleSamples: TRandomSamples;
  Selected: Integer;
  Solved: Boolean;
begin
  Result := Default(TCaseEvidence);
  Owner := TTextPassCompositionShowcase.Create(0);
  Idle := TTextPassCompositionShowcase.Create(0);
  try
    Baseline(Owner, Capture, Base); Baseline(Idle, Capture, OtherBase);
    MutateWeight(Owner.Pipeline, Layer, Layer, Key, Weight);
    MutateWeight(Idle.Pipeline, Layer, Layer, OtherKey, OtherWeight);
    AdvanceStreams(Owner.Pipeline); AdvanceStreams(Idle.Pipeline);
    Before := Snapshot(Owner.Pipeline);
    Selected := Owner.Pipeline.Graph.CurrentPassIndex;
    R := Base;
    if Scope < 0 then Solved := Owner.Pipeline.TryGenerate(Options(Capture), R, Report)
    else Solved := Owner.Pipeline.TryRegenerateFrom(TWfcTextPassLayer(Scope),
      Options(Capture), R, Report);
    Check(not Solved, 'mutated applied-model weight cannot publish');
    CheckFailurePublication(Owner.Pipeline, R, Report, Layer, Capture, wtpsCaptureFailed);
    Check((not Report.Capture[Layer].Valid)
      and (Report.Capture[Layer].Issue.Kind = wsgikModelIdentity),
      'capture failure retains model-identity diagnostic');
    CheckSnapshot(Owner.Pipeline, Before, 'capture rollback');
    Check(Owner.Pipeline.Graph.CurrentPassIndex = Selected, 'selected pass restored');
    Check(Owner.Pipeline.LayerGraph[Layer].Rules[Key].Weight = Weight + 1,
      'caller model edit remains caller-owned, not silently reverted');
    Result.RandomSamples := ReadStreams(Owner.Pipeline);
    IdleSamples := ReadStreams(Idle.Pipeline);
    CheckSamples(Result.RandomSamples, IdleSamples, 'capture rollback');
    Result.Status := Report.Status; Result.FailedLayer := Report.FailedLayer;
    Result.CoreStatus := Report.Solve.Status;
    //Only repair our exact edit. A retry must discard the rejected pending
    //capture and retain the original caller constraints and empty cell.
    Owner.Pipeline.LayerGraph[Layer].Rules[Key].Weight := Weight;
    Idle.Pipeline.LayerGraph[Layer].Rules[OtherKey].Weight := OtherWeight;
    if Scope < 0 then
    begin
      Check(Owner.Pipeline.TryGenerate(Options(Capture), Retry, Report), 'full retry after exact model repair');
      Check(Idle.Pipeline.TryGenerate(Options(Capture), OtherBase, OtherReport), 'idle full retry control');
    end
    else
    begin
      Check(Owner.Pipeline.TryRegenerateFrom(TWfcTextPassLayer(Scope), Options(Capture), Retry, Report),
        'selective retry after exact model repair');
      Check(Idle.Pipeline.TryRegenerateFrom(TWfcTextPassLayer(Scope), Options(Capture), OtherBase, OtherReport),
        'idle selective retry control');
    end;
    Check((Report.Status = wtpsCompleted) and (Retry.Text = OtherBase.Text)
      and (TextPassShowcaseSignature(Retry) = TextPassShowcaseSignature(OtherBase)),
      'retry matches untouched control and publishes no stale pending result');
    Result.Text := String(Retry.Text);
    Check(TextPassShowcaseSignature(Base) = '1:69ABA6CE', 'failure did not alias prior public result');
  finally Idle.Free; Owner.Free; end;
end;

procedure TestCaptureMatrix;
var Layer: TWfcTextPassLayer; Scope: Integer;
  CaptureOff, CaptureOn: TCaseEvidence;
begin
  for Layer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
    for Scope := -1 to Ord(Layer) do
    begin
      CaptureOff := RunCaptureFailure(Layer, Scope, False);
      CaptureOn := RunCaptureFailure(Layer, Scope, True);
      Check((CaptureOff.Status = CaptureOn.Status)
        and (CaptureOff.FailedLayer = CaptureOn.FailedLayer)
        and (CaptureOff.CoreStatus = CaptureOn.CoreStatus)
        and (CaptureOff.Text = CaptureOn.Text),
        'capture opt-in does not change failure or retry outcome');
      CheckSamples(CaptureOff.RandomSamples, CaptureOn.RandomSamples, 'capture on/off parity');
    end;
end;

procedure TestReusedProviderBoundary;
var
  Owner, Idle: TTextPassCompositionShowcase;
  Base, Other, R: TWfcTextPassResult;
  Report: TWfcTextPassReport;
  Before: TPipelineState;
  Layer: TWfcTextPassLayer;
  Scope, Capture, Selected: Integer;
  Key, OtherKey: TGraphValue;
  Weight, OtherWeight: TGraphWeight;
  Raised: Boolean;
  Samples, Control: TRandomSamples;
begin
  for Layer := wtplStructure to wtplLexical do
    for Scope := Ord(Layer) + 1 to Ord(wtplPunctuation) do
      for Capture := 0 to 1 do
      begin
        Owner := TTextPassCompositionShowcase.Create(0);
        Idle := TTextPassCompositionShowcase.Create(0);
        try
          Baseline(Owner, Capture = 1, Base); Baseline(Idle, Capture = 1, Other);
          MutateWeight(Owner.Pipeline, Layer, TWfcTextPassLayer(Scope), Key, Weight);
          MutateWeight(Idle.Pipeline, Layer, TWfcTextPassLayer(Scope), OtherKey, OtherWeight);
          AdvanceStreams(Owner.Pipeline); AdvanceStreams(Idle.Pipeline);
          Before := Snapshot(Owner.Pipeline); Selected := Owner.Pipeline.Graph.CurrentPassIndex;
          R := Base; Raised := False;
          try
            Owner.Pipeline.TryRegenerateFrom(TWfcTextPassLayer(Scope), Options(Capture = 1), R, Report);
          except on E: EInvalidOperation do Raised := True; end;
          Check(Raised, 'corrupt reused provider is not reattributed or silently regenerated');
          Check(IsClear(R), 'out-of-scope exception clears caller-seeded public result');
          CheckSnapshot(Owner.Pipeline, Before, 'out-of-scope rollback');
          Check(not Owner.Pipeline.Graph.Running
            and (Owner.Pipeline.Graph.CurrentPassIndex = Selected), 'exception releases guard and selection');
          Samples := ReadStreams(Owner.Pipeline); Control := ReadStreams(Idle.Pipeline);
          CheckSamples(Samples, Control, 'out-of-scope RNG rollback');
          Owner.Pipeline.LayerGraph[Layer].Rules[Key].Weight := Weight;
          Idle.Pipeline.LayerGraph[Layer].Rules[OtherKey].Weight := OtherWeight;
          Check(Owner.Pipeline.TryRegenerateFrom(TWfcTextPassLayer(Scope), Options(Capture = 1), R, Report),
            'out-of-scope exception leaves owner reusable after caller repair');
          Check(Idle.Pipeline.TryRegenerateFrom(TWfcTextPassLayer(Scope), Options(Capture = 1), Other, Report)
            and (R.Text = Other.Text), 'out-of-scope retry agrees with idle control');
        finally Idle.Free; Owner.Free; end;
      end;
end;

procedure BroadenRelation(const P: TWfcTextPassPipeline;
  const Target, Provider: TWfcTextPassLayer);
var Targets, Providers: TGraphValues; I: Integer;
begin
  Targets := P.LayerGraph[Target].CopyRegisteredValues;
  Providers := P.LayerGraph[Provider].CopyRegisteredValues;
  for I := 0 to High(Targets) do
    P.LayerGraph[Target].Rules[Targets[I]].RequireFromPass(
      WfcTextPassLayerName(Provider), Providers);
end;

procedure ConfigureSemanticMismatch(const P: TWfcTextPassPipeline;
  const PunctuationOnly: Boolean);
begin
  if PunctuationOnly then
  begin
    BroadenRelation(P, wtplPunctuation, wtplLexical);
    P.IntersectAllowedTokens(wtplLexical, 1, 'sun');
    P.IntersectAllowedTokens(wtplLexical, 4, 'bang');
    P.IntersectAllowedTokens(wtplPunctuation, 4, EncodeWfcTextPassFragment('.'));
  end
  else
  begin
    BroadenRelation(P, wtplLexical, wtplStructure);
    BroadenRelation(P, wtplPunctuation, wtplStructure);
    P.IntersectAllowedTokens(wtplStructure, 1, 'ADJ');
    P.IntersectAllowedTokens(wtplLexical, 1, 'sun');
  end;
  P.LayerGraph[wtplPunctuation].Entry[1, 0, 0].ClearValue;
end;

procedure TestSemanticValidation;
var
  Owner, Idle: TTextPassCompositionShowcase;
  Base, Other, R: TWfcTextPassResult;
  Report: TWfcTextPassReport;
  Before: TPipelineState;
  Capture, CaseIndex: Integer;
  Layer: TWfcTextPassLayer;
  Relation: TWfcTextPassRelation;
  Samples, Control: TRandomSamples;
begin
  for CaseIndex := 0 to 1 do
    for Capture := 0 to 1 do
    begin
      Owner := TTextPassCompositionShowcase.Create(0);
      Idle := TTextPassCompositionShowcase.Create(0);
      try
        Baseline(Owner, Capture = 1, Base); Baseline(Idle, Capture = 1, Other);
        ConfigureSemanticMismatch(Owner.Pipeline, CaseIndex = 1);
        ConfigureSemanticMismatch(Idle.Pipeline, CaseIndex = 1);
        AdvanceStreams(Owner.Pipeline); AdvanceStreams(Idle.Pipeline);
        Before := Snapshot(Owner.Pipeline); R := Base;
        Check(not Owner.Pipeline.TryGenerate(Options(Capture = 1), R, Report),
          'real broadened graph relation cannot defeat owner semantic maps');
        if CaseIndex = 0 then
        begin Layer := wtplLexical; Relation := wtprLexicalFromStructure; end
        else
        begin Layer := wtplPunctuation; Relation := wtprPunctuationFromLexical; end;
        CheckFailurePublication(Owner.Pipeline, R, Report, Layer, Capture = 1, wtpsValidationFailed);
        Check((Report.Validation.Issue.Kind = wtpvikPassProjection)
          and (Report.Validation.Issue.Layer = Layer)
          and (Report.Validation.Issue.Relation = Relation),
          'semantic rejection identifies the original public projection relation');
        Check(Report.Capture[wtplStructure].Valid and Report.Capture[wtplLexical].Valid
          and Report.Capture[wtplPunctuation].Valid,
          'semantic fixture passed all independent learned sequence captures');
        CheckSnapshot(Owner.Pipeline, Before, 'semantic rollback');
        Samples := ReadStreams(Owner.Pipeline); Control := ReadStreams(Idle.Pipeline);
        CheckSamples(Samples, Control, 'semantic RNG rollback');
      finally Idle.Free; Owner.Free; end;
    end;
end;

procedure AddUnprojectableValue(const P: TWfcTextPassPipeline;
  const Layer: TWfcTextPassLayer);
var G: TGraph; Values: TGraphValues; I: Integer;
begin
  G := P.LayerGraph[Layer]; Values := G.CopyRegisteredValues;
  G.AddValue('@wfcs-owner-mismatch-private-extra');
  //The extra registered state is removed by an explicit caller domain. Its
  //removal is traceable, although no public model token exists for its index.
  for I := 0 to CELL_COUNT - 1 do G.SetAllowedValues(I, 0, 0, Values);
  G.Entry[1, 0, 0].ClearValue;
end;

procedure TestUnprojectableTracePrivacy;
var
  Owner, Idle: TTextPassCompositionShowcase;
  Base, Other, R: TWfcTextPassResult;
  Report: TWfcTextPassReport;
  Before: TPipelineState;
  Layer: TWfcTextPassLayer;
  Capture, I, Selected: Integer;
  Samples, Control: TRandomSamples;
begin
  for Layer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
    for Capture := 0 to 1 do
    begin
      Owner := TTextPassCompositionShowcase.Create(0);
      Idle := TTextPassCompositionShowcase.Create(0);
      try
        Baseline(Owner, Capture = 1, Base); Baseline(Idle, Capture = 1, Other);
        AddUnprojectableValue(Owner.Pipeline, Layer);
        AddUnprojectableValue(Idle.Pipeline, Layer);
        AdvanceStreams(Owner.Pipeline); AdvanceStreams(Idle.Pipeline);
        Before := Snapshot(Owner.Pipeline); Selected := Owner.Pipeline.Graph.CurrentPassIndex;
        R := Base;
        Check(not Owner.Pipeline.TryGenerate(Options(Capture = 1), R, Report),
          'extra graph state returns a domain failure without a projection exception');
        Check(IsClear(R) and (Report.Status = wtpsCaptureFailed)
          and (Report.FailedLayer = Layer)
          and (Report.Capture[Layer].Issue.Kind = wsgikModelIdentity),
          'unprojectable optional trace cannot replace the primary domain diagnostic');
        Check((Report.Solve.Status = gssContradiction)
          and (Report.Solve.Contradiction.Kind = gckFinalValidation)
          and (Report.Solve.FailedPassIndex = Ord(Layer)),
          'extra graph state is rejected before core commit');
        Check(not Report.Solve.TraceCaptured and (Report.Solve.TraceHash = 0)
          and (Length(Report.Solve.Trace) = 0), 'extra-state generic trace remains private');
        for I := 0 to High(Report.Solve.Passes) do
          Check((Report.Solve.Passes[I].TraceStart = -1)
            and (Report.Solve.Passes[I].TraceCount = 0), 'extra-state generic slices sanitized');
        Check(Length(Report.Trace) = 0, 'unprojectable trace has no partially projected prefix');
        if Capture = 1 then
          Check(not Report.TraceValidation.Valid
            and (Report.TraceValidation.Issue.Kind = gtvikValueIndex),
            'optional trace reports the unowned state index without exposing its string')
        else Check(not Report.TraceCaptured and (Report.TraceHash = 0),
          'disabled extra-state trace remains empty');
        CheckSnapshot(Owner.Pipeline, Before, 'extra-state rollback');
        Check(not Owner.Pipeline.Graph.Running
          and (Owner.Pipeline.Graph.CurrentPassIndex = Selected), 'extra-state guard and selection restored');
        Samples := ReadStreams(Owner.Pipeline); Control := ReadStreams(Idle.Pipeline);
        CheckSamples(Samples, Control, 'extra-state RNG rollback');
      finally Idle.Free; Owner.Free; end;
    end;
end;

function AddUnownedPass(const P: TWfcTextPassPipeline): TGraph;
begin
  P.Graph.SwitchToPass('unowned-fourth-pass');
  Result := P.Graph.PassGraph[3];
  //Passes share the owner's shape; reshaping this pass would also reshape
  //all three existing layers and invalidate the fixture before the attempt.
  Result.AddValue('private-fourth-a'); Result.AddValue('private-fourth-b');
  Result.SetAllowedValues(0, 0, 0, Result.CopyRegisteredValues);
  Result.RandomIndex(RANDOM_BOUND);
end;

procedure TestUnownedPassRollback;
var
  Owner, Idle: TTextPassCompositionShowcase;
  Base, Other, R: TWfcTextPassResult;
  Report: TWfcTextPassReport;
  Before: TPipelineState;
  Extra, OtherExtra: TGraph;
  ExtraEntries: array[0..CELL_COUNT - 1] of TGraphEntry;
  Values: TGraphValues;
  Capture, I, Selected: Integer;
  Raised: Boolean;
  Samples, Control: TRandomSamples;
begin
  for Capture := 0 to 1 do
  begin
    Owner := TTextPassCompositionShowcase.Create(0);
    Idle := TTextPassCompositionShowcase.Create(0);
    try
      Baseline(Owner, Capture = 1, Base); Baseline(Idle, Capture = 1, Other);
      Extra := AddUnownedPass(Owner.Pipeline); OtherExtra := AddUnownedPass(Idle.Pipeline);
      Owner.Pipeline.LayerGraph[wtplLexical].Entry[1, 0, 0].ClearValue;
      Idle.Pipeline.LayerGraph[wtplLexical].Entry[1, 0, 0].ClearValue;
      AdvanceStreams(Owner.Pipeline); AdvanceStreams(Idle.Pipeline);
      Before := Snapshot(Owner.Pipeline); Selected := Owner.Pipeline.Graph.CurrentPassIndex;
      for I := 0 to CELL_COUNT - 1 do ExtraEntries[I] := Extra.Entry[I, 0, 0];
      Values := Extra.CopyAllowedValues(0, 0, 0);
      R := Base; Raised := False;
      try
        Owner.Pipeline.TryGenerate(Options(Capture = 1), R, Report);
      except on E: EInvalidOperation do Raised := True; end;
      Check(Raised and IsClear(R), 'unowned fourth pass raises before any public result commits');
      CheckSnapshot(Owner.Pipeline, Before, 'unowned-pass rollback');
      for I := 0 to CELL_COUNT - 1 do
        Check((Extra.Entry[I, 0, 0] = ExtraEntries[I]) and ExtraEntries[I].Empty
          and not ExtraEntries[I].Generated and (ExtraEntries[I].Value = '')
          and (Extra.HasAllowedValues(I, 0, 0) = (I = 0)),
          'unowned pass restores every entry and explicit domain presence');
      Check(Extra.HasAllowedValues(0, 0, 0)
        and SameValues(Values, Extra.CopyAllowedValues(0, 0, 0)),
        'unowned pass also preserves its explicit domain values');
      Check(not Owner.Pipeline.Graph.Running
        and (Owner.Pipeline.Graph.CurrentPassIndex = Selected), 'unowned-pass guard and selection restored');
      Samples := ReadStreams(Owner.Pipeline); Control := ReadStreams(Idle.Pipeline);
      CheckSamples(Samples, Control, 'owned RNG after unowned-pass rollback');
      for I := 0 to SAMPLE_COUNT - 1 do
        Check(Extra.RandomIndex(RANDOM_BOUND) = OtherExtra.RandomIndex(RANDOM_BOUND),
          'unowned fourth-pass RNG also rolls back');
      Check(Owner.Pipeline.Graph.TotalPassCount = 4, 'caller-added pass remains caller-owned');
    finally Idle.Free; Owner.Free; end;
  end;
end;

begin
  try
    TestCaptureMatrix;
    TestReusedProviderBoundary;
    TestSemanticValidation;
    TestUnprojectableTracePrivacy;
    TestUnownedPassRollback;
  except
    on E: Exception do
    begin
      Inc(Failures);
      WriteLn('UNEXPECTED: ', E.ClassName, ': ', E.Message);
    end;
  end;
  WriteLn('TEXT_TRANSACTION_BASELINE=1:69ABA6CE/2412171679');
  WriteLn('checks=', Checks, ' failures=', Failures);
  if Failures <> 0 then Halt(1);
end.
