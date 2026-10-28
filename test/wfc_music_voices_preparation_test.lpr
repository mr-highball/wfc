(*
MIT License

Copyright (c) 2021, 2026 mr-highball

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
program wfc_music_voices_preparation_test;

{$mode delphi}
{$H+}

uses SysUtils, wfc, wfc_model, wfc_preparation, wfc_sequence, wfc_sequence_graph,
  wfc_solve_progress, wfc_sequence_learn, wfc_music_sequence, wfc_music_ensemble_graph,
  wfc_music, wfc_music_ensemble, wfc_music_voices_graph, wfc_voice_preparation_fixture
  {$IFDEF PAS2JS}{$IFNDEF WFC_NODE_PREPARATION_TEST}, wfc_browser_test_host{$ENDIF}{$ENDIF};

const
  RequiredStages: array[0..13] of String = (
    'voice_build_visits', 'voice_prepare_visits', 'voice_config_visits',
    'voice_coverage_visits', 'ensemble_model_visits', 'ensemble_harmony_rows',
    'ensemble_rhythm_rows', 'sequence_segment_graph_visits', 'sequence_graph_model_visits',
    'model_construct_visits', 'model_apply_visits', 'sequence_projection_visits',
    'sequence_model_rows', 'sequence_witness_projection_visits');

type
  TPoint = (ptStart, ptInterior, ptFinish);
  TPoints = set of TPoint;
  TStage = record
    Name: String;
    Points: TPoints;
  end;
  TStages = array of TStage;
  TObserver = class
  public
    Recording: Boolean;
    Stages: TStages;
    AbortStage: String;
    AbortPoint: TPoint;
    Marker: Exception;
    Raised: Boolean;
    Calls: Integer;
    SawModelOracle: Boolean;
    SawRunningReadOnly: Boolean;
    AbortDone: Int64;
    procedure Observe(const AProgress: TWfcPreparationProgress);
    procedure ObserveSolve(const AProgress: TWfcSolveProgress);
  end;
  TProbeGraph = class(TGraph)
  private
    FRegistered: Boolean;
  protected
    function DoValidateCommit(out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean; override;
  public
    PreferredGroup: TGraphRuleGroup;
    OriginalWeight: Integer;
    PreferredWeight: Integer;
    constructor Create; override;
    constructor CreatePass(const ARoot: TGraph; const APassIndex: Integer); override;
    destructor Destroy; override;
  end;

var
  Checks: Integer;
  Aborts: Integer;
  Alive: Integer;
  CommitCalls: Integer;
  ObserveCommit: Boolean;
  O: TObserver;
  C: TWfcMusicVoicesGraphConfig;
  B: TWfcMusicVoicesBoundaries;
  G: TGraph;
  Expected: TWfcMusicVoicesGenerated;
  Guarded: TWfcMusicVoicesGenerated;
  ExpectedText: String;
  RunningProbe: TGraph;

procedure Check(const AValue: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not AValue then raise Exception.Create(AMessage);
end;

constructor TProbeGraph.Create;
begin
  inherited Create;
  Inc(Alive);
  FRegistered := True;
end;

constructor TProbeGraph.CreatePass(const ARoot: TGraph; const APassIndex: Integer);
begin
  inherited CreatePass(ARoot, APassIndex);
  Inc(Alive);
  FRegistered := True;
end;

destructor TProbeGraph.Destroy;
begin
  if FRegistered then Dec(Alive);
  inherited Destroy;
end;

function TProbeGraph.DoValidateCommit(out AFailedPassIndex,
  AFailedEntryIndex: Integer): Boolean;
var
  P: TWfcMusicVoicesValidationReport;
begin
  Result := inherited DoValidateCommit(AFailedPassIndex, AFailedEntryIndex);
  if Result and ObserveCommit then
  begin
    Check(Running and ValidatingCommit, 'Read-only commit proof scope is absent');
    Inc(CommitCalls);
    if PreferredGroup <> nil then PreferredGroup.Weight := OriginalWeight;
    try
      Check(CaptureSolvedWfcMusicVoices(C, Self, B, Guarded, P, O.Observe),
        'Guarded voice capture failed: ' + P.Issue.Detail);
    finally
      if PreferredGroup <> nil then PreferredGroup.Weight := PreferredWeight;
    end;
  end;
end;

procedure TObserver.Observe(const AProgress: TWfcPreparationProgress);
var
  I: Integer;
  Seen: TPoints;
begin
  Inc(Calls);
  Check((AProgress.Stage <> '') and (AProgress.Done >= 0) and
    (AProgress.Total >= 0) and ((AProgress.Total = 0) or
    (AProgress.Done <= AProgress.Total)) and (AProgress.PassIndex >= -1) and
    (AProgress.PassIndex <= 4), 'Invalid preparation units or pass');
  Seen := [];
  if AProgress.Done = 0 then Include(Seen, ptStart);
  if (AProgress.Done > 0) and ((AProgress.Total = 0) or
    (AProgress.Done < AProgress.Total)) then Include(Seen, ptInterior);
  if (AProgress.Total > 0) and (AProgress.Done = AProgress.Total) then
    Include(Seen, ptFinish);
  if Recording then
  begin
    I := 0;
    while (I < Length(Stages)) and (Stages[I].Name <> AProgress.Stage) do Inc(I);
    if I = Length(Stages) then
    begin
      SetLength(Stages, I + 1);
      Stages[I].Name := AProgress.Stage;
    end;
    Stages[I].Points := Stages[I].Points + Seen;
  end;
  if (AProgress.Stage = 'model_construct_visits') and
    (AProgress.Total = 52912) then SawModelOracle := True;
  if (AbortStage = AProgress.Stage) and (AbortPoint in Seen) then
  begin
    Raised := True;
    AbortDone := AProgress.Done;
    raise Marker;
  end;
end;

procedure TObserver.ObserveSolve(const AProgress: TWfcSolveProgress);
var
  V: TWfcMusicVoicesGenerated;
  P: TWfcMusicVoicesValidationReport;
begin
  if (RunningProbe <> nil) and RunningProbe.Running and not SawRunningReadOnly then
  begin
    Check(not RunningProbe.ValidatingCommit, 'Preparation falsely exposed commit scope');
    V := Expected;
    Check(not CaptureSolvedWfcMusicVoices(C, RunningProbe, B, V, P),
      'Read-only proof accepted an ordinary running graph');
    Check(Length(V.Layers) = 0, 'Ordinary running rejection changed False output contract');
    SawRunningReadOnly := True;
  end;
end;

function HasStage(const AStages: TStages; const AName: String): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AStages) do
    if AStages[I].Name = AName then Exit(True);
  Result := False;
end;

procedure SolveAndCompare(const AGraph: TGraph);
var
  R: TGraphSolveReport;
  P: TWfcMusicVoicesValidationReport;
  V: TWfcMusicVoicesGenerated;
begin
  Check(AGraph.TrySolve(DefaultGraphSolveOptions, R), 'Retry solve failed');
  Check(CaptureSolvedWfcMusicVoices(C, AGraph, B, V, P, O.Observe),
    'Retry capture failed: ' + P.Issue.Detail);
  Check(PreparationEvidence(V) = ExpectedText, 'Actual nil/observed token/frame/witness replay changed');
end;

procedure RunOperation(const AOperation: Integer;
  var AGraph: TGraph; var AGenerated: TWfcMusicVoicesGenerated;
  var AConfig: TWfcMusicVoicesGraphConfig; var AClasses: TWfcMusicPitchClasses);
var
  P: TWfcMusicVoicesValidationReport;
begin
  case AOperation of
    0: AGraph := BuildWfcMusicVoicesSegmentGraph(C, 65, 731, B, O.Observe, TProbeGraph);
    1: Check(CaptureSolvedWfcMusicVoices(C, G, B, AGenerated, P, O.Observe), 'Observed capture rejected');
    2: Check(ValidateWfcMusicVoicesGenerated(C, G, B, Expected, P, O.Observe), 'Observed proof rejected');
    3: AConfig := CopyWfcMusicVoicesGraphConfig(C, O.Observe);
    4: AClasses := WfcMusicVoicesCoverageClasses(C, O.Observe);
  end;
end;

procedure CheckOperation(const AOperation: Integer);
var
  Candidate: TGraph;
  V: TWfcMusicVoicesGenerated;
  ConfigCopy: TWfcMusicVoicesGraphConfig;
  Classes: TWfcMusicPitchClasses;
  Stages: TStages;
  One: TStage;
  Point: TPoint;
  Aborted: Boolean;
  BeforeAlive: Integer;
  I: Integer;
  Required: String;
begin
  Candidate := nil;
  O.Stages := nil;
  O.Recording := True;
  try
    RunOperation(AOperation, Candidate, V, ConfigCopy, Classes);
  finally
    O.Recording := False;
    Candidate.Free;
  end;
  Stages := O.Stages;
  Check(Length(Stages) > 0, 'No actual interior observation');
  WriteLn('Preparation operation ', AOperation, ': ', Length(Stages), ' actual phases');
  for One in Stages do
    for Point := Low(TPoint) to High(TPoint) do
      if Point in One.Points then
      begin
        Candidate := G;
        V := Expected;
        ConfigCopy := C;
        Classes := [19];
        BeforeAlive := Alive;
        O.AbortStage := One.Name;
        O.AbortPoint := Point;
        O.Marker := Exception.Create('Big Boss original preparation marker');
        O.Raised := False;
        Aborted := False;
        try
          try
            RunOperation(AOperation, Candidate, V, ConfigCopy, Classes);
          except
            on E: Exception do
            begin
              if E <> O.Marker then raise;
              Aborted := True;
              Inc(Aborts);
            end;
          end;
        finally
          if not O.Raised then O.Marker.Free;
          O.Marker := nil;
          O.AbortStage := '';
          if Candidate <> G then Candidate.Free;
        end;
        Check(Aborted and O.Raised, 'Original abort did not escape: ' + One.Name);
        Check((Candidate = G) and (Alive = BeforeAlive), 'Builder assigned or leaked a partial candidate');
        Check(PreparationEvidence(V) = ExpectedText, 'Aborted capture changed previous managed output');
        Check((Length(ConfigCopy.Voices) = 2) and
          (ConfigCopy.Voices[0].Model = C.Voices[0].Model), 'Aborted config copy changed old result');
        Check((Length(Classes) = 1) and (Classes[0] = 19), 'Aborted class copy changed old result');
        Candidate := nil;
        RunOperation(AOperation, Candidate, V, ConfigCopy, Classes);
        if AOperation = 0 then
        begin
          try
            SolveAndCompare(Candidate);
          finally
            Candidate.Free;
          end;
        end;
        if AOperation = 1 then
        begin
          Check(PreparationEvidence(V) = ExpectedText, 'Same-graph capture retry differs');
          V.Layers[0].Tokens[0] := 'caller edit';
          V.Layers[0].StateIndices[0] := -99;
          V.Frames[0].Voices[0].Tones[0].Velocity := 1;
          V.Coverage[0].Suppliers[0] := 1;
          Check(PreparationEvidence(Expected) = ExpectedText, 'Captured output aliases prior result');
        end;
        Check(PreparationEvidence(Expected) = ExpectedText, 'Operation changed independent control');
      end;
  if AOperation = 0 then
  begin
    Check(O.SawModelOracle, 'Exact 65-state generic constructor visit oracle was absent');
    for Required in RequiredStages do
      Check(HasStage(Stages, Required), 'Required construction interior absent: ' + Required);
    for I := 0 to High(Stages) do
      Check(Stages[I].Name <> '', 'Unnamed recorded phase');
  end;
end;

function ConstructModel(const AKind: Integer; const ASource: TWfcModel): TWfcModel;
begin
  case AKind of
    0: Result := TWfcModel.Create(ASource.Rank, ASource.SampleWidth, ASource.SampleHeight,
      ASource.Boundary, ASource.Symmetry, ASource.Directions, ASource.CopyTokens,
      ASource.CopyWeights, ASource.CopyRelations, O.Observe);
    1: Result := TWfcModel.Create(ASource.Rank, ASource.CopySampleShapes,
      ASource.Boundary, ASource.Symmetry, ASource.Directions, ASource.CopyTokens,
      ASource.CopyWeights, ASource.CopyRelations, O.Observe);
  else
    Result := TWfcModel.Create(ASource.Rank, ASource.SampleWidth, ASource.SampleHeight,
      ASource.SampleDepth, ASource.Boundary, ASource.Symmetry, ASource.Directions,
      ASource.CopyTokens, ASource.CopyWeights, ASource.CopyRelations, O.Observe);
  end;
end;

procedure CheckModelConstructors;
var
  Source: TWfcModel;
  Candidate: TWfcModel;
  Keys: TWfcModelTokens;
  Kind: Integer;
  I: Integer;
  J: Integer;
  D: TWfcModelDirection;
  Point: TPoint;
  Aborted: Boolean;
begin
  SetLength(Keys, C.Voices[0].Model.StateCount);
  for I := 0 to High(Keys) do Keys[I] := 'authored.' + IntToStr(I);
  Source := C.Voices[0].Model.CreateGraphModel(Keys);
  try
    for Kind := 0 to 2 do
      for Point := Low(TPoint) to High(TPoint) do
      begin
        Candidate := Source;
        O.AbortStage := 'model_construct_visits';
        O.AbortPoint := Point;
        O.Marker := Exception.Create('Big Boss model constructor marker');
        O.Raised := False;
        Aborted := False;
        try
          try
            Candidate := ConstructModel(Kind, Source);
          except
            on E: Exception do
            begin
              if E <> O.Marker then raise;
              Aborted := True;
              Inc(Aborts);
            end;
          end;
        finally
          if not O.Raised then O.Marker.Free;
          O.Marker := nil;
          O.AbortStage := '';
          if Candidate <> Source then Candidate.Free;
        end;
        Check(Aborted and (Candidate = Source), 'Constructor published on original observer abort');
        Candidate := ConstructModel(Kind, Source);
        try
          Check((Candidate.SampleCount = Source.SampleCount) and
            (Candidate.ValueCount = Source.ValueCount) and
            (Candidate.Rank = Source.Rank) and (Candidate.Boundary = Source.Boundary) and
            (Candidate.Directions = Source.Directions), 'Constructor metadata replay differs');
          for I := 0 to Source.ValueCount - 1 do
          begin
            Check((Candidate.TokenAt(I) = Source.TokenAt(I)) and
              (Candidate.WeightAt(I) = Source.WeightAt(I)), 'Constructor token/weight replay differs');
            for J := 0 to Source.ValueCount - 1 do
              for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
                if D in Source.Directions then
                  Check(Candidate.RelationCount(D, I, J) = Source.RelationCount(D, I, J),
                    'Constructor relation replay differs');
          end;
        finally
          Candidate.Free;
        end;
      end;
  finally
    Source.Free;
  end;
end;

procedure CheckFailureContracts;
var
  V: TWfcMusicVoicesGenerated;
  P: TWfcMusicVoicesValidationReport;
  Aborted: Boolean;
begin
  V := Expected;
  Check(not CaptureSolvedWfcMusicVoices(C, nil, B, V, P, O.Observe),
    'Invalid graph capture was accepted');
  Check((Length(V.Layers) = 0) and (Length(V.Frames) = 0) and (Length(V.Coverage) = 0),
    'Ordinary False no longer empties capture');
  V := Expected;
  O.AbortStage := 'voice_capture_visits';
  O.AbortPoint := ptFinish;
  O.Marker := Exception.Create('Big Boss failed-capture publication marker');
  O.Raised := False;
  Aborted := False;
  try
    try
      CaptureSolvedWfcMusicVoices(C, nil, B, V, P, O.Observe);
    except
      on E: Exception do
      begin
        if E <> O.Marker then raise;
        Aborted := True;
        Inc(Aborts);
      end;
    end;
  finally
    if not O.Raised then O.Marker.Free;
    O.Marker := nil;
    O.AbortStage := '';
  end;
  Check(Aborted and (PreparationEvidence(V) = ExpectedText),
    'Observer abort published a failed-capture empty result');
end;

procedure CheckLargeCancellation;
var
  Large: TWfcMusicVoicesGraphConfig;
  Boundaries: TWfcMusicVoicesBoundaries;
  Tokens: TWfcModelTokens;
  Frame: TWfcMusicEnsembleFrame;
  Candidate: TGraph;
  I: Integer;
  Aborted: Boolean;
begin
  Large := Default(TWfcMusicVoicesGraphConfig);
  SetLength(Large.Voices, 1);
  try
    Large.StepsPerOctave := 12;
    Large.HarmonyMode := wmehmAllowed;
    Large.HarmonyModel := LearnSequenceModel([EncodeWfcMusicPitchClassSet(
      MakeWfcMusicPitchClassSet(12, [0,1,2,3,4,5,6,7,8,9,10,11]))], 1);
    Large.RhythmModel := LearnSequenceModel([EncodeWfcMusicRhythmFrame(
      MakeWfcMusicRhythmFrame([wmcaAttack]))], 1);
    SetLength(Frame.Voices, 1);
    Frame.Voices[0].Action := wmcaAttack;
    SetLength(Frame.Voices[0].Tones, 1);
    SetLength(Tokens, 1024);
    for I := 0 to High(Tokens) do
    begin
      Frame.Voices[0].Tones[0] := MakeWfcMusicTone(48 + I mod 12, 1 + I div 12);
      Tokens[I] := EncodeWfcMusicEnsembleFrame(Frame);
    end;
    Large.Voices[0].Model := LearnSequenceModel(Tokens, 1);
    Large.Voices[0].MinPitch := 48;
    Large.Voices[0].MaxPitch := 59;
    SetLength(Boundaries, 3);
    for I := 0 to High(Boundaries) do
      Boundaries[I] := MakeWfcSequenceInitialSegmentBoundary(False);
    Check((Large.Voices[0].Model.StateCount = 1024) and
      ((Large.Voices[0].Model.StateCount + 2) * 255 = 261630),
      'Original admitted large construction geometry changed');
    Candidate := G;
    O.AbortStage := 'ensemble_model_visits';
    O.AbortPoint := ptInterior;
    O.Marker := Exception.Create('Big Boss original large-construction marker');
    O.Raised := False;
    Aborted := False;
    try
      try
        Candidate := BuildWfcMusicVoicesSegmentGraph(Large, 255, 731, Boundaries, O.Observe);
      except
        on E: Exception do
        begin
          if E <> O.Marker then raise;
          Aborted := True;
          Inc(Aborts);
        end;
      end;
    finally
      if not O.Raised then O.Marker.Free;
      O.Marker := nil;
      O.AbortStage := '';
      if Candidate <> G then Candidate.Free;
    end;
    Check(Aborted and (Candidate = G) and (O.AbortDone = 256),
      'Large graph did not abort from its real 256-visit preparation checkpoint');
    WriteLn('Original 1024-state/255-cell construction cancelled at 256 real ensemble visits');
  finally
    FreePreparationFixture(Large);
  end;
end;

procedure ApplyPreference(const AGraph: TProbeGraph);
var
  Pass: TGraph;
  Saved: TGraphValues;
  Values: TGraphValues;
  HadDomain: Boolean;
begin
  Pass := AGraph.PassGraph[2];
  HadDomain := Pass.HasAllowedValues(0,0,0);
  Saved := Pass.CopyAllowedValues(0,0,0);
  try
    Pass.ClearAllowedValues(0,0,0);
    IntersectSequenceTokenConstraints(C.Voices[0].Model, Pass,
      [MakeWfcSequenceTokenConstraint(0, [C.Voices[0].Model.PublicTokenAt(0)])]);
    Values := Pass.CopyAllowedValues(0,0,0);
    Check(Length(Values) = 1, 'Preference fixture requires its exact public state domain');
    AGraph.PreferredGroup := Pass.Rules[Values[0]];
    AGraph.OriginalWeight := AGraph.PreferredGroup.Weight;
    AGraph.PreferredWeight := AGraph.OriginalWeight * 7;
    AGraph.PreferredGroup.Weight := AGraph.PreferredWeight;
  finally
    Pass.ClearAllowedValues(0,0,0);
    if HadDomain then Pass.SetAllowedValues(0,0,0, Saved);
  end;
end;

procedure CheckGuard;
var
  Candidate: TProbeGraph;
  Control: TProbeGraph;
  R: TGraphSolveReport;
  P: TWfcMusicVoicesValidationReport;
  Preferred: TWfcMusicVoicesGenerated;
  PreferredText: String;
  BeforeCalls: Integer;
  Aborted: Boolean;
begin
  Candidate := TProbeGraph(BuildWfcMusicVoicesSegmentGraph(C, 65, 731, B, nil, TProbeGraph));
  Control := nil;
  try
    Control := TProbeGraph(BuildWfcMusicVoicesSegmentGraph(C, 65, 731, B, nil, TProbeGraph));
    ApplyPreference(Control);
    ApplyPreference(Candidate);
    RunningProbe := Control;
    try
      Check(Control.TrySolve(DefaultGraphSolveOptions, R, O.ObserveSolve), 'Preferred control solve failed');
    finally
      RunningProbe := nil;
    end;
    Check(O.SawRunningReadOnly, 'Ordinary running-graph rejection was not exercised');
    Check(not CaptureSolvedWfcMusicVoices(C, Control, B, Preferred, P),
      'Model identity proof silently accepted changed preference weights');
    Control.PreferredGroup.Weight := Control.OriginalWeight;
    Check(CaptureSolvedWfcMusicVoices(C, Control, B, Preferred, P),
      'Original-weight control proof failed');
    PreferredText := PreparationEvidence(Preferred);
    Control.Free;
    Control := nil;
    ObserveCommit := True;
    BeforeCalls := CommitCalls;
    O.AbortStage := 'voice_capture_ready';
    O.AbortPoint := ptFinish;
    O.Marker := Exception.Create('Big Boss guarded capture marker');
    O.Raised := False;
    Aborted := False;
    try
      try
        Candidate.TrySolve(DefaultGraphSolveOptions, R);
      except
        on E: Exception do
        begin
          if E <> O.Marker then raise;
          Aborted := True;
          Inc(Aborts);
        end;
      end;
    finally
      if not O.Raised then O.Marker.Free;
      O.Marker := nil;
      O.AbortStage := '';
    end;
    Check(Aborted and (CommitCalls = BeforeCalls + 1), 'Guarded callback was not reached');
    Check(not Candidate.ValidatingCommit, 'Observer abort retained commit-validation scope');
    Check(Candidate.PreferredGroup.Weight = Candidate.PreferredWeight,
      'Aborted proof did not reapply the search preference');
    Check(Length(Guarded.Layers) = 0, 'Aborted guarded capture assigned private result');
    Check(Candidate.PassGraph[0].Entry[0,0,0].Empty, 'Graph did not roll back guarded capture');
    Check(Candidate.TrySolve(DefaultGraphSolveOptions, R), 'Exact same-graph guarded retry failed');
    Check(not Candidate.ValidatingCommit, 'Successful solve retained commit-validation scope');
    Check(Candidate.PreferredGroup.Weight = Candidate.PreferredWeight,
      'Successful proof did not reapply the search preference');
    Check(PreparationEvidence(Guarded) = PreferredText, 'Guarded preferred retry changed musical output');
  finally
    ObserveCommit := False;
    Control.Free;
    Candidate.Free;
  end;
end;

var
  R: TGraphSolveReport;
  P: TWfcMusicVoicesValidationReport;
  Operation: Integer;
begin
  C := PreparationFixture;
  O := TObserver.Create;
  G := nil;
  try
    B := PreparationBoundaries;
    G := BuildWfcMusicVoicesSegmentGraph(C, 65, 731, B);
    Check(G.TrySolve(DefaultGraphSolveOptions, R), 'Baseline solve failed');
    Check(CaptureSolvedWfcMusicVoices(C, G, B, Expected, P), 'Baseline capture failed');
    ExpectedText := PreparationEvidence(Expected);
    if (ParamCount > 0) and (ParamStr(1) = 'guard-only') then
      CheckGuard
    else if (ParamCount > 0) and (ParamStr(1) = 'large-only') then
      CheckLargeCancellation
    else
    begin
      for Operation := 0 to 4 do CheckOperation(Operation);
      CheckModelConstructors;
      CheckFailureContracts;
      CheckLargeCancellation;
      CheckGuard;
    end;
    WriteLn('Voice preparation: ', Checks, ' checks; ', Aborts,
      ' original abort/retries; ', O.Calls, ' actual callbacks');
  finally
    G.Free;
    O.Free;
    FreePreparationFixture(C);
  end;
  Check(Alive = 0, 'Observer or guarded candidate graph remains alive');
end.
