{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_training_circular_studio_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc_model, wfc_sequence, wfc_sequence_text, wfc_training,
  wfc_training_text, wfc_text_training, wfc_training_workspace,
  wfc_pipeline_run, wfc_pipeline_result, training_studio_presets,
  training_studio_demo;

var Checks, Failures: Integer;

procedure Check(const Condition: Boolean; const MessageText: String);
begin
  Inc(Checks);
  if not Condition then begin Inc(Failures); WriteLn('FAIL: ', MessageText); end;
end;

function Metadata: TWfcTrainingMetadata;
begin
  Result := MakeWfcTrainingMetadata('circular-text-test', 'MIT',
    'project-authored independent circles');
end;

procedure TestBridge;
var Samples: TWfcTextTrainingSamples; D, OpenD, Again: TWfcTrainingDocument;
  M: TWfcSequenceModel; S, ModelText: String; I, J: Integer;
  Token: TWfcModelToken; H: TWfcSequenceHistoryItem; FirstCircle: Boolean;
begin
  SetLength(Samples, 2);
  Samples[0] := MakeWfcTextTrainingSample('first', 'AB');
  Samples[1] := MakeWfcTextTrainingSample('second', 'CD');
  D := BuildWfcTextTrainingDocument(Metadata, Samples, 4);
  try
    OpenD := BuildWfcTextTrainingDocument(Metadata, Samples, 4, wmbOpen);
    try
      Check(EncodeWfcTrainingText(D) = EncodeWfcTrainingText(OpenD),
        'omitted boundary preserves exact legacy open source');
      Check(LearnWfcTrainingModelText(D) = LearnWfcTrainingModelText(OpenD),
        'omitted boundary preserves exact open model bytes');
      Check(Pos('wfclearn=1'#10, EncodeWfcTrainingText(D)) = 1,
        'open raw text retains source version one');
    finally OpenD.Free; end;
  finally D.Free; end;

  D := BuildWfcTextTrainingDocument(Metadata, Samples, 4, wmbWrap);
  try
    S := EncodeWfcTrainingText(D);
    Check(Pos('wfclearn=5'#10, S) = 1, 'circular raw text opts into source version five');
    Check((D.CopyOptions.Boundary = wmbWrap) and (D.TotalTokenCount = 4),
      'circular declaration retains exact token count');
    Samples[0].Text := 'changed';
    Check((D.SampleAt(0).Width = 2) and (D.SampleAt(0).Tokens[0] = 'A'),
      'builder owns tokenized samples after caller mutation');
    Again := DecodeWfcTrainingText(S);
    try
      Check(EncodeWfcTrainingText(Again) = S, 'circular source save/load is canonical');
      ModelText := LearnWfcTrainingModelText(Again);
      Check(Pos('wfcs=2'#10, ModelText) = 1, 'circular source learns explicit wrapped model');
      M := DecodeWfcSequenceText(ModelText);
      try
        Check((M.Boundary = wmbWrap) and (M.ObservationCount = 4) and
          (M.SampleCount = 2), 'one observation per scalar, independent sample circles');
        for I := 0 to M.StateCount - 1 do
        begin
          Check((M.StartCountAt(I) = 0) and (M.EndCountAt(I) = 0) and
            (M.StateLeadingBosCountAt(I) = 0), 'circular model has no invented endpoints or BOS');
          Token := M.ProjectStateToken(I);
          FirstCircle := (Token = 'A') or (Token = 'B');
          for J := 0 to M.HistorySize - 1 do
          begin
            H := M.HistoryItemAt(I, J);
            Check(H.Kind = wshToken, 'history is entirely actual sample tokens');
            Token := M.PublicTokenAt(H.TokenIndex);
            if FirstCircle then Check((Token = 'A') or (Token = 'B'),
              'short first circle wraps history within itself')
            else Check((Token = 'C') or (Token = 'D'),
              'short second circle never joins the first sample');
          end;
        end;
      finally M.Free; end;
    finally Again.Free; end;
  finally D.Free; end;
end;

procedure ExpectReject(const Boundary: TWfcModelBoundary; const Order: Integer);
var Samples: TWfcTextTrainingSamples; D: TWfcTrainingDocument; Rejected: Boolean;
begin
  SetLength(Samples, 1);
  Samples[0] := MakeWfcTextTrainingSample('one', 'ab');
  D := nil; Rejected := False;
  try
    try D := BuildWfcTextTrainingDocument(Metadata, Samples, Order, Boundary);
    except on E: EWfcTextTraining do Rejected := True; end;
    Check(Rejected and (D = nil), 'invalid bridge scalar rejected before an owned document escapes');
  finally D.Free; end;
end;

{$IFDEF PAS2JS}
function HostileBoundary(const AIndex: Integer): TWfcModelBoundary;
begin
  asm return [NaN, Infinity, -Infinity, 0.5, undefined, null, '0', false, {}, 2][AIndex]; end;
end;
function HostileOrder(const AIndex: Integer): Integer;
begin
  asm return [NaN, Infinity, -Infinity, 1.5, undefined, null, '2', true, {}, 4294967296][AIndex]; end;
end;
{$ENDIF}

procedure TestValidation;
var Boundary: TWfcModelBoundary; BadOrdinal: Integer;
  {$IFDEF PAS2JS}I: Integer;{$ENDIF}
begin
  BadOrdinal := 99;
  {$push}{$R-}Boundary := TWfcModelBoundary(BadOrdinal);{$pop}
  ExpectReject(Boundary, 2);
  ExpectReject(wmbWrap, 0);
  ExpectReject(wmbWrap, WFC_TRAINING_MAX_ORDER + 1);
  {$IFDEF PAS2JS}
  for I := 0 to 9 do
  begin
    ExpectReject(HostileBoundary(I), 2);
    ExpectReject(wmbWrap, HostileOrder(I));
  end;
  {$ENDIF}
end;

procedure TestStudio;
var W: TWfcTrainingWorkspace; O: TWfcTrainingSolveOptions;
  Locks, OriginalLocks: TWfcPipelineCellLocks; Tokens: TWfcModelTokens;
  S, Recipe, Solved: String; I, Seed: Integer;
begin
  Check((TRAINING_STUDIO_PRESET_COUNT = 8) and (TRAINING_STUDIO_CIRCULAR_PRESET=6),
    'volume preset appends without renumbering circular or older presets');
  for I := 0 to 5 do Check(Length(TrainingStudioPresetLocks(I, 0)) = 0,
    'historical preset keeps its unlocked run');
  W := TWfcTrainingWorkspace.Create(InteractiveWfcTrainingWorkspaceLimits);
  try
    S := TrainingStudioPresetText(TRAINING_STUDIO_CIRCULAR_PRESET);
    W.SetSourceText(S); W.Train;
    Check(W.WrapNeighbors and (W.Rank = 1) and (W.SourceTokenCount = 20) and
      (W.ModelItemCount = 17), 'authored circles fit bounded Studio model with wrapped output');
    Check(W.TrainingSignatureText = '7078B027', 'circular source portable golden');
    Check(W.RecipeSignatureText = '6F936E07', 'circular recipe portable golden');
    Recipe := W.RecipeText;
    OriginalLocks := TrainingStudioPresetLocks(TRAINING_STUDIO_CIRCULAR_PRESET, W.PublicPassIndex);
    Locks := TrainingStudioPresetLocks(TRAINING_STUDIO_CIRCULAR_PRESET, W.PublicPassIndex);
    Locks[0].Token := 'f';
    Check(OriginalLocks[0].Token = 'r', 'preset lock arrays are independently owned');
    for Seed := 0 to 7 do
      for I := 2 to 5 do
      begin
        O := TrainingStudioPresetOptions(TRAINING_STUDIO_CIRCULAR_PRESET);
        O.Seed := Seed; O.Width := I * 10;
        W.ConfigureRun(O, OriginalLocks, nil); W.Solve;
        Check((W.ResultStatus = wprsSolved) and
          TrainingStudioCircularOutputIsValid(W.OutputTokens),
          'different seeds and circular extents obey independent public phrase grammar');
      end;
    O := TrainingStudioPresetOptions(TRAINING_STUDIO_CIRCULAR_PRESET);
    W.ConfigureRun(O, OriginalLocks, nil); W.Solve;
    Solved := W.ResultText;
    Check(W.ResultSignatureText = '3437A49D', 'locked forty-cell result portable golden');
    Tokens := W.OutputTokens; Tokens[High(Tokens)] := '!';
    Check(not TrainingStudioCircularOutputIsValid(Tokens),
      'independent output checker rejects broken final seam');
    Check(TrainingStudioOutputIsValid(TRAINING_STUDIO_CIRCULAR_PRESET, 40, 1,
      W.OutputTokens), 'native Studio shares independent circular validation');
    W.SetSourceText(S);
    Check(not W.HasRecipe and not W.HasRun and not W.HasResult,
      'source import invalidates the previous recipe, locks request and output');
    W.Train;
    Check(W.RecipeText = Recipe, 'source reload repeats exact learned recipe');
    W.ConfigureRun(O, OriginalLocks, nil); W.Solve;
    Check(W.ResultText = Solved, 'source plus public run locks replay full result bytes');
    Locks := TrainingStudioPresetLocks(TRAINING_STUDIO_CIRCULAR_PRESET, W.PublicPassIndex);
    SetLength(Locks, 4); Locks[3] := Locks[2]; Locks[2] := Locks[1];
    Locks[1] := MakeWfcPipelineCellLock(W.PublicPassIndex, 1, 0, 0, 'r');
    W.ConfigureRun(O, Locks, nil);
    Check(not W.HasResult, 'edited public locks remove stale circular output');
    W.Solve;
    Check((W.ResultStatus = wprsContradiction) and (Length(W.OutputTokens) = 0),
      'impossible public token lock cannot leak partial circular output');
    W.ConfigureRun(O, OriginalLocks, nil); W.Solve;
    Check(W.ResultText = Solved, 'removing conflict recovers deterministic full output');
    O.Width := 39;
    W.ConfigureRun(O, OriginalLocks, nil); W.Solve;
    Check((W.ResultStatus = wprsContradiction) and (Length(W.OutputTokens) = 0),
      'incompatible output circumference is rejected, not padded or clipped');
  finally W.Free; end;
end;

begin
  TestBridge;
  TestValidation;
  TestStudio;
  WriteLn('Circular Training Studio checks: ', Checks, ', Failures: ', Failures);
  if Failures <> 0 then raise Exception.Create('circular Training Studio checks failed');
end.
