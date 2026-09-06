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
program wfc_music_ensemble_plan_hooks_test;
{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_sequence, wfc_sequence_learn,
  wfc_sequence_graph, wfc_music, wfc_music_sequence, wfc_music_ensemble,
  wfc_music_ensemble_graph, wfc_music_ensemble_passes,
  wfc_music_ensemble_stream, wfc_music_arrangement;

type
  TTest = procedure;
  TFixture = record
    Models: TWfcMusicEnsembleModels;
    Harmony, Rhythm, Ensemble: TWfcModelTokens;
    Template: TWfcMusicScore;
  end;
  TValidator = class
  public
    Mode, Calls, BadField, BadValue: Integer;
    function Validate(const Composition: TWfcMusicEnsembleComposition;
      out Issue: TWfcMusicEnsembleValidationIssue): Boolean;
  end;
  TProbeStream = class(TWfcMusicEnsembleStream)
  public
    Mode, Calls: Integer;
    ReentryRejected, EditRejected: Boolean;
  protected
    function ValidateSegment(const Candidate: TWfcMusicEnsembleSegment;
      out Failure: String): Boolean; override;
  end;

var Checks, Failures: Integer;

procedure Check(const Condition: Boolean; const Text: String);
begin
  Inc(Checks);
  if not Condition then begin Inc(Failures); WriteLn('FAIL: ', Text); end;
end;

procedure Run(const Text: String; const Test: TTest);
begin
  WriteLn('Test: ', Text);
  try Test;
  except on E: Exception do begin Inc(Failures);
    WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message); end; end;
end;

procedure Init(out F: TFixture);
const Pitches: array[0..7] of Integer = (60,60,67,67,60,67,60,60);
var I: Integer; Frames: TWfcMusicEnsembleFrames; Tones: TWfcMusicTones;
  Voices: TWfcMusicVoiceCells; Tracks: TWfcMusicTracks; ScoreVoices: TWfcMusicVoices;
  Meters: TWfcMusicMeterChanges; Tempos: TWfcMusicTempoChanges; Spans: TWfcMusicSpanEvents;
begin
  F := Default(TFixture);
  SetLength(Frames, 8); SetLength(Tones, 1); SetLength(Voices, 1);
  for I := 0 to 7 do
  begin
    Tones[0] := MakeWfcMusicTone(Pitches[I], 80);
    Voices[0] := MakeWfcMusicVoiceCell(wmcaAttack, Tones);
    Frames[I] := MakeWfcMusicEnsembleFrame(Voices);
  end;
  F.Ensemble := EncodeWfcMusicEnsembleFrames(Frames);
  F.Harmony := EncodeWfcMusicPitchClassSets(
    ProjectWfcMusicEnsembleFramesToPitchClassSets(Frames, 12));
  F.Rhythm := EncodeWfcMusicRhythmFrames(ProjectWfcMusicEnsembleFramesToRhythm(Frames));
  F.Models.Ensemble := LearnSequenceModel(F.Ensemble, 1);
  F.Models.Harmony := LearnSequenceModel(F.Harmony, 1);
  F.Models.Rhythm := LearnSequenceModel(F.Rhythm, 1);
  SetLength(Tracks, 1); Tracks[0] := MakeWfcMusicTrack('voice', 'hook fixture');
  SetLength(ScoreVoices, 1); ScoreVoices[0] := MakeWfcMusicVoice(0, 'voice');
  SetLength(Meters, 1); Meters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  SetLength(Tempos, 1); Tempos[0] := MakeWfcMusicTempoChange(0, 500000);
  SetLength(Spans, 1); Spans[0] := MakeWfcMusicRest(0, 0, 8);
  F.Template := TWfcMusicScore.Create(2, 12, 8, Tracks, ScoreVoices, Meters, Tempos, Spans);
end;

procedure Done(var F: TFixture);
begin
  F.Template.Free; F.Models.Ensemble.Free; F.Models.Harmony.Free; F.Models.Rhythm.Free;
end;

function Config(const F: TFixture): TWfcMusicEnsembleConfig;
begin
  Result := DefaultWfcMusicEnsembleConfig(F.Template, 2, 9876);
  Result.Models := F.Models; Result.Extent := wsePrefix;
end;

function StreamConfig(const F: TFixture): TWfcMusicEnsembleStreamConfig;
begin
  Result := DefaultWfcMusicEnsembleStreamConfig(F.Models, 1, 12, 2, 18, 9876);
  Result.SegmentCellCount := 3; Result.Search.SolveOptions.CaptureTrace := True;
end;

function Fingerprint(const P: TWfcMusicEnsemblePipeline): String;
var L: TWfcMusicEnsembleLayer; G: TWfcGeneratedSequence;
  R: TWfcSequenceGraphValidationReport; I: Integer;
begin
  Result := '';
  for L := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
    if P.TryCopyCommittedLayer(L, G, R) then
      for I := 0 to High(G.Tokens) do
        Result := Result + '/' + IntToStr(G.StateIndices[I]) + ':' + G.Tokens[I]
    else Result := Result + '!';
end;

function SameFrontier(const A, B: TWfcMusicEnsembleStreamFrontier): Boolean;
var L: TWfcMusicEnsembleLayer;
begin
  Result := (A.HasPrevious = B.HasPrevious) and (A.EndTick = B.EndTick);
  for L := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
    Result := Result and (A.StateIndices[L] = B.StateIndices[L]) and (A.Tokens[L] = B.Tokens[L]);
end;

function TValidator.Validate(const Composition: TWfcMusicEnsembleComposition;
  out Issue: TWfcMusicEnsembleValidationIssue): Boolean;
begin
  Inc(Calls);
  Check(Assigned(Composition) and Composition.HasLatentCapture and
    (Composition.CellCount = 4), 'finite hook receives immutable checked latent capture');
  Issue := Default(TWfcMusicEnsembleValidationIssue);
  Issue.Kind := wmevikCallerConstraint; Issue.Layer := wmelEnsemble;
  Issue.Position := 1; Issue.Detail := 'planned phrase rejected';
  if Mode = 2 then raise Exception.Create('validator exception');
  if Mode = 3 then Issue.Position := 4;
  {$IFDEF PAS2JS}
  if Mode = 4 then
    case BadField of
      0: Issue.Kind := TWfcMusicEnsembleValidationIssueKind(BadValue);
      1: Issue.Layer := TWfcMusicEnsembleLayer(BadValue);
      2: Issue.Position := BadValue;
    end;
  {$ENDIF}
  Result := Mode = 0;
end;

function TProbeStream.ValidateSegment(const Candidate: TWfcMusicEnsembleSegment;
  out Failure: String): Boolean;
var S: TWfcMusicEnsembleSegment; R: TGraphNegotiationReport;
begin
  Inc(Calls); Failure := 'planned segment rejected';
  Check((Candidate <> nil) and (Candidate.Index = NextIndex) and
    (Candidate.StartTick = ProducedTicks), 'stream validator runs before publication');
  case Mode of
    2: raise Exception.Create('segment validator exception');
    3,6: Cancel;
    4,5:
      begin
        S := nil;
        try Next(S, R);
        except on E: EWfcMusicEnsembleStream do ReentryRejected := True; end;
        Check(S = nil, 'reentrant Next cannot publish an inner candidate');
        try ClearAllowedTokens(wmelHarmony, ProducedTicks div 2);
        except on E: EWfcMusicEnsembleStream do EditRejected := True; end;
      end;
    7: Failure := '';
  end;
  Result := Mode in [0,3,5];
end;

procedure TestFiniteRollback;
var F: TFixture; Cfg: TWfcMusicEnsembleConfig; P, Twin: TWfcMusicEnsemblePipeline;
  V: TValidator; C, TC, Candidate, Artifact: TWfcMusicEnsembleComposition;
  R, TR: TWfcMusicEnsembleReport; Validation: TWfcMusicEnsembleValidationReport;
  Options: TGraphSolveOptions; Score: TWfcMusicScore;
  Before: String; Signature: Cardinal; M, I, CallsBefore: Integer; Raised: Boolean;
begin
  Init(F); V := TValidator.Create;
  try
    Cfg := Config(F);
    Check(not Assigned(Cfg.ValidateComposition) and
      (Length(Cfg.InitialTokenConstraints[wmelHarmony]) = 0) and
      (Length(Cfg.InitialTokenConstraints[wmelRhythm]) = 0) and
      (Length(Cfg.InitialTokenConstraints[wmelEnsemble]) = 0), 'all new configuration defaults are inert');
    Options := DefaultGraphSolveOptions; Options.CaptureTrace := True;
    for M := 1 to 3 do
    begin
      V.Mode := 0; Cfg.ValidateComposition := nil; Twin := TWfcMusicEnsemblePipeline.Create(Cfg);
      Cfg.ValidateComposition := V.Validate; P := TWfcMusicEnsemblePipeline.Create(Cfg);
      C := nil; TC := nil; Candidate := nil; Artifact := nil; Score := nil;
      try
        Check(P.TryGenerate(Options, C, R) and Twin.TryGenerate(Options, TC, TR), 'baseline solves with nil and accepting hooks');
        Check((C.Signature = TC.Signature) and (R.Solve.TraceHash = TR.Solve.TraceHash),
          'accepting hook changes neither default composition nor solver trace');
        Signature := C.Signature; Before := Fingerprint(P);
        CallsBefore := V.Calls;
        Score := C.CopyScore;
        Artifact := CreateWfcMusicEnsembleComposition(C.Seed, C.QuantumTicks, C.Extent,
          C.HarmonyMode, C.CopyGenerated(wmelHarmony).Tokens, C.CopyGenerated(wmelRhythm).Tokens,
          C.CopyGenerated(wmelEnsemble).Tokens, Score);
        Check(not P.Validate(Artifact, Validation) and (V.Calls = CallsBefore),
          'built-in latent-capture rejection cannot be bypassed by callback');
        V.Mode := M;
        for I := 0 to 2 do
        begin
          Raised := False;
          try Check(not P.TryGenerate(Options, Candidate, R), 'false validator rejects the finite transaction');
          except on E: Exception do begin Raised := True;
            Check((M = 2) and (E.Message = 'validator exception'), 'validator exception is preserved'); end; end;
          Check(Raised = (M = 2), 'only throwing validator raises');
          Check((Candidate = nil) and (Fingerprint(P) = Before) and (C.Signature = Signature),
            'rejection preserves all committed latent paths and old immutable composition');
          if M <> 2 then
          begin
            Check((R.Status = wmesValidationFailed) and not R.Validation.Valid and
              (R.Solve.Contradiction.Kind = gckFinalValidation), 'false callback reports transactional final validation');
            if M = 3 then Check((R.Validation.Issue.Kind = wmevikInternal) and
              (R.Validation.Issue.Position = -1), 'invalid callback location is sanitized');
          end;
        end;
        V.Mode := 0;
        TC.Free; TC := nil;
        Check(P.TryGenerate(Options, Candidate, R) and Twin.TryGenerate(Options, TC, TR), 'pipeline remains reusable after callback failures');
        Check((Candidate.Signature = TC.Signature) and (R.Solve.TraceHash = TR.Solve.TraceHash) and
          (Fingerprint(P) = Fingerprint(Twin)), 'repeated rejected attempts restore every pass RNG stream');
        CallsBefore := V.Calls;
        Check(P.Validate(Candidate, Validation) and (V.Calls = CallsBefore + 1), 'public Validate also invokes application proof');
      finally Score.Free; Artifact.Free; Candidate.Free; TC.Free; C.Free; P.Free; Twin.Free; end;
    end;
  finally V.Free; Done(F); end;
end;

procedure TestInitialBaselines;
var F: TFixture; Cfg: TWfcMusicEnsembleConfig; P: TWfcMusicEnsemblePipeline;
  C: TWfcMusicEnsembleComposition; R: TWfcMusicEnsembleReport;
  L: TWfcMusicEnsembleLayer; I: Integer;
begin
  Init(F);
  try
    Cfg := Config(F); SetLength(Cfg.InitialTokenConstraints[wmelHarmony], 1);
    Cfg.InitialTokenConstraints[wmelHarmony][0].Position := 1;
    Cfg.InitialTokenConstraints[wmelHarmony][0].AllowedTokens := [F.Harmony[0]];
    P := TWfcMusicEnsemblePipeline.Create(Cfg); C := nil;
    try
      Cfg.InitialTokenConstraints[wmelHarmony][0].AllowedTokens[0] := F.Harmony[2];
      Cfg.InitialTokenConstraints[wmelHarmony][0].Position := 2;
      Check(P.TryGenerate(C, R) and (C.CopyGenerated(wmelHarmony).Tokens[1] = F.Harmony[0]),
        'constructor baseline detaches caller constraint arrays');
      C.Free; C := nil;
      P.IntersectAllowedTokens(wmelHarmony, 1, F.Harmony[2]);
      Check(not P.TryGenerate(C, R) and (C = nil), 'valid disjoint lock intersects plan to explicit contradiction');
      P.ClearAllowedTokens(wmelHarmony, 1);
      Check(P.TryGenerate(C, R) and (C.CopyGenerated(wmelHarmony).Tokens[1] = F.Harmony[0]),
        'clearing a conflicting lock restores planned baseline');
      C.Free; C := nil;
      for L := Low(TWfcMusicEnsembleLayer) to High(TWfcMusicEnsembleLayer) do
        for I := 0 to 3 do P.ClearAllowedTokens(L, I);
      Check(P.TryGenerate(C, R) and (C.CopyGenerated(wmelHarmony).Tokens[0] = F.Harmony[0]) and
        (C.CopyGenerated(wmelHarmony).Tokens[1] = F.Harmony[0]), 'clear-all retains both model start boundary and planned masks');
    finally C.Free; P.Free; end;
    Cfg.InitialTokenConstraints[wmelHarmony][0].Position := 1;
    Cfg.InitialTokenConstraints[wmelHarmony][0].AllowedTokens := nil;
    P := TWfcMusicEnsemblePipeline.Create(Cfg); C := nil;
    try
      Check(not P.TryGenerate(C, R) and (C = nil), 'explicit empty initial mask remains unsatisfiable');
      P.ClearAllowedTokens(wmelHarmony, 1);
      Check(not P.TryGenerate(C, R) and (C = nil), 'clear preserves explicit-empty planned baseline');
    finally C.Free; P.Free; end;
  finally Done(F); end;
end;

procedure TestStreamPublication;
var F: TFixture; P: TProbeStream; Twin: TWfcMusicEnsembleStream;
  S, TS: TWfcMusicEnsembleSegment; R, TR: TGraphNegotiationReport;
  Before: TWfcMusicEnsembleStreamFrontier; Step: TWfcMusicArrangementStep;
  Constraints: TWfcMusicEnsembleStreamConstraints; M: Integer; Raised: Boolean;
begin
  Init(F);
  try
    for M := 1 to 7 do
    begin
      P := TProbeStream.Create(StreamConfig(F)); Twin := TWfcMusicEnsembleStream.Create(StreamConfig(F));
      S := nil; TS := nil;
      try
        Check((P.Next(S, R) = wmaspProduced) and (Twin.Next(TS, TR) = wmaspProduced), 'nil/default stream hooks produce baseline');
        Check((S.Signature = TS.Signature) and (R.TranscriptHash = TR.TranscriptHash), 'default stream hook preserves signature and trace');
        S.Free; S := nil;
        P.IntersectAllowedTokens(wmelHarmony, 3, [F.Harmony[0], F.Harmony[2]]);
        P.IntersectAllowedTokens(wmelHarmony, 8, [F.Harmony[0]]);
        Before := P.CopyFrontier; P.Mode := M; Raised := False;
        Step := wmaspFailed;
        try Step := P.Next(S, R);
        except on E: Exception do begin Raised := True;
          Check((M = 2) and (E.Message = 'segment validator exception'), 'stream validator exception is preserved'); end; end;
        if M = 5 then
        begin
          Check((Step = wmaspProduced) and (S <> nil) and (P.NextIndex = 2), 'rejected inner operations allow one outer publication');
          Check(P.ReentryRejected and P.EditRejected, 'realized hook retains reentry and constraint-edit guards');
          Continue;
        end;
        Check(Raised = (M = 2), 'only throwing realized hook raises');
        Check((S = nil) and (P.NextIndex = 1) and (P.ProducedTicks = 6) and
          SameFrontier(Before, P.CopyFrontier), 'failed or cancelled validator cannot advance any frontier field');
        Constraints := P.CopyConstraints;
        Check((Length(Constraints) = 2) and (Constraints[0].Position = 3) and
          (Constraints[1].Position = 8), 'rejected segment cannot retire current or future constraints');
        if M in [3,6] then Check((Step = wmaspCancelled) and (P.Status = wmasCancelled), 'post-validation cancellation wins before publication')
        else Check((P.Status = wmasFailed) and (P.Failure <> ''), 'rejected realized segment fails with a diagnostic');
        if M = 4 then Check(P.ReentryRejected and P.EditRejected, 'rejecting hook cannot reenter or edit future masks');
      finally TS.Free; S.Free; Twin.Free; P.Free; end;
    end;
  finally Done(F); end;
end;

{$IFDEF PAS2JS}
procedure TestBrowserIssues;
var F: TFixture; P: TWfcMusicEnsemblePipeline; Cfg: TWfcMusicEnsembleConfig;
  V: TValidator; C: TWfcMusicEnsembleComposition; R: TWfcMusicEnsembleReport; I, J, X: Integer;
begin
  Init(F); V := TValidator.Create;
  try
    Cfg := Config(F); Cfg.ValidateComposition := V.Validate;
    P := TWfcMusicEnsemblePipeline.Create(Cfg); C := nil;
    try
      V.Mode := 4;
      for I := 0 to 6 do
      begin
        case I of
          0: asm X = NaN; end;
          1: asm X = Infinity; end;
          2: asm X = 0.5; end;
          3: asm X = undefined; end;
          4: asm X = "0"; end;
          5: asm X = null; end;
          6: asm X = -Infinity; end;
        end;
        for J := 0 to 2 do
        begin
          V.BadValue := X; V.BadField := J;
          Check(not P.TryGenerate(C, R) and (C = nil) and
            (R.Validation.Issue.Kind = wmevikInternal) and
            (R.Validation.Issue.Layer = wmelEnsemble) and (R.Validation.Issue.Position = -1),
            'malformed browser callback issue is normalized before graph reporting');
        end;
      end;
    finally C.Free; P.Free; end;
  finally V.Free; Done(F); end;
end;
{$ENDIF}

begin
  Run('finite validation rollback and inert defaults', TestFiniteRollback);
  Run('planned immutable initial baselines and clear behavior', TestInitialBaselines);
  Run('stream validation before frontier publication', TestStreamPublication);
  {$IFDEF PAS2JS}Run('strict browser callback issue diagnostics', TestBrowserIssues);{$ENDIF}
  WriteLn('Ensemble plan-hook checks: ', Checks - Failures, '/', Checks);
  if Failures <> 0 then Halt(1);
end.
