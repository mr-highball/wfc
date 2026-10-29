(*
MIT License

Copyright (c) 2026 mr-highball

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
program wfc_sequence_learning_progress_test;

{$mode delphi}{$H+}

uses
  SysUtils, wfc_model, wfc_sequence, wfc_sequence_learn, wfc_sequence_text;

type
  TObserver = class
  public
    Calls: Integer;
    Enabled: Boolean;
    FinalOnly: Boolean;
    Phase: TWfcSequencePreparationPhase;
    Marker: Exception;
    Last: array[TWfcSequencePreparationPhase] of Integer;
    Finals: array[TWfcSequencePreparationPhase] of Integer;
    procedure Reset;
    procedure Observe(const AWork: TWfcSequencePreparationProgress);
  end;

  { Big Boss: exercises each actual primary constructor checkpoint while the
    caller still owns an unchanged immutable source and no published clone. }
  TCloneObserver = class
    Calls: Integer;
    AbortAt: Integer;
    Last: Integer;
    Interior: Boolean;
    Final: Boolean;
    Marker: Exception;
    Source: TWfcSequenceModel;
    procedure Observe(const AWork: TWfcSequencePreparationProgress);
    procedure Reset(const ASource: TWfcSequenceModel; const AAbortAt: Integer);
  end;

var
  GChecks: Integer;
  GAborts: Integer;
  GObserver: TObserver;
  GCloneAborts: Integer;

procedure Check(const AValue: Boolean; const AMessage: String);
begin
  Inc(GChecks);
  if not AValue then
    raise Exception.Create(AMessage);
end;

procedure TCloneObserver.Reset(const ASource: TWfcSequenceModel; const AAbortAt: Integer);
begin
  Calls := 0;
  AbortAt := AAbortAt;
  Last := 0;
  Interior := False;
  Final := False;
  Marker := nil;
  Source := ASource;
end;

procedure TCloneObserver.Observe(const AWork: TWfcSequencePreparationProgress);
begin
  Inc(Calls);
  Check(AWork.Phase = wspModelBuildVisits, 'Clone uses primary model construction');
  Check((AWork.Done >= Last) and (AWork.Done - Last <= 256) and
    ((AWork.Total = 0) or (AWork.Done <= AWork.Total)), 'Clone callback cadence and bounds');
  Check((Source.SampleCount > 0) and (Source.PublicTokenCount > 0),
    'Source remains readable during independent clone construction');
  Last := AWork.Done;
  Interior := Interior or ((AWork.Done > 0) and (AWork.Total = 0));
  Final := (AWork.Total > 0) and (AWork.Done = AWork.Total);
  if Calls = AbortAt then
  begin
    Marker := Exception.Create('Original immutable clone cancellation');
    raise Marker;
  end;
end;

procedure TestClones;
const
  COrders: array[0..2] of Integer = (1, 4, 17);
var
  LSamples: TWfcSequenceSamples;
  LSource: TWfcSequenceModel;
  LCopy: TWfcSequenceModel;
  LObserver: TCloneObserver;
  LText: String;
  LBoundary: TWfcModelBoundary;
  LOrder: Integer;
  LCalls: Integer;
  I: Integer;
  J: Integer;
  LAborted: Boolean;
begin
  SetLength(LSamples, 2);
  SetLength(LSamples[0].Tokens, 64);
  SetLength(LSamples[1].Tokens, 9);
  for I := 0 to 63 do
    LSamples[0].Tokens[I] := 'literal token #|' + IntToStr(I);
  {$ifdef PAS2JS}
  LSamples[0].Tokens[0] := #$03BB + #$D83C#$DFB5;
  {$else}
  LSamples[0].Tokens[0] := #$CE#$BB#$F0#$9F#$8E#$B5;
  {$endif}
  LSamples[0].Tokens[1] := ' quoted " token '#9' with spaces ';
  for I := 0 to 8 do
    LSamples[1].Tokens[I] := LSamples[0].Tokens[(I * 7) mod 64];
  LObserver := TCloneObserver.Create;
  try
    for LBoundary := Low(TWfcModelBoundary) to High(TWfcModelBoundary) do
      for J := 0 to High(COrders) do
      begin
        LOrder := COrders[J];
        LSource := LearnSequenceModelCorpus(LSamples, LOrder, LBoundary);
        LCopy := nil;
        try
          LText := EncodeWfcSequenceText(LSource);
          LObserver.Reset(LSource, 0);
          LCopy := LSource.Clone(LObserver.Observe);
          Check((LCopy <> LSource) and (EncodeWfcSequenceText(LCopy) = LText),
            'Observed clone preserves exact literal source model');
          Check(LObserver.Interior and LObserver.Final, 'Clone has real interior/final work');
          LCalls := LObserver.Calls;
          FreeAndNil(LCopy);
          for I := 1 to LCalls do
          begin
            LObserver.Reset(LSource, I);
            LAborted := False;
            try
              LCopy := LSource.Clone(LObserver.Observe);
            except
              on E: Exception do
              begin
                LAborted := True;
                Check(E = LObserver.Marker, 'Clone preserves original callback exception');
              end;
            end;
            Check(LAborted and (LCopy = nil) and (LObserver.Calls = I),
              'Failed clone publishes nothing at every checkpoint');
            Check(EncodeWfcSequenceText(LSource) = LText, 'Clone failure preserves complete source');
            Inc(GCloneAborts);
          end;
          LCopy := LSource.Clone;
          Check(EncodeWfcSequenceText(LCopy) = LText, 'Nil-observer exact retry');
          FreeAndNil(LSource);
          Check(EncodeWfcSequenceText(LCopy) = LText, 'Clone survives independent source destruction');
          WriteLn('Clone order=', LOrder, ' boundary=', Ord(LBoundary), ' checkpoints=', LCalls);
        finally
          LCopy.Free;
          LSource.Free;
        end;
      end;
  finally
    LObserver.Free;
  end;
end;

procedure TObserver.Reset;
var
  LPhase: TWfcSequencePreparationPhase;
begin
  Enabled := False;
  for LPhase := Low(LPhase) to High(LPhase) do
  begin
    Last[LPhase] := 0;
    Finals[LPhase] := 0;
  end;
end;

procedure TObserver.Observe(const AWork: TWfcSequencePreparationProgress);
var
  LFinal: Boolean;
begin
  Inc(Calls);
  Check(AWork.Phase in [wspLearnTokenRows, wspLearnStateRows, wspModelBuildVisits],
    'Learning emitted an unrelated graph phase');
  Check((AWork.Done >= 0) and (AWork.Total >= 0) and
    ((AWork.Total = 0) or (AWork.Done <= AWork.Total)), 'Invalid progress count');
  Check(AWork.Done >= Last[AWork.Phase], 'Phase completed count went backwards');
  Last[AWork.Phase] := AWork.Done;
  LFinal := (AWork.Total > 0) and (AWork.Done = AWork.Total);
  if LFinal then
    Inc(Finals[AWork.Phase]);
  if Enabled and (Phase = AWork.Phase) and (AWork.Done > 0) and
      ((FinalOnly and LFinal) or ((not FinalOnly) and (not LFinal))) then
    raise Marker;
end;

function AuthoredSamples: TWfcSequenceSamples;
var
  I: Integer;
  J: Integer;
begin
  Result := nil;
  SetLength(Result, 2);
  for I := 0 to 1 do
  begin
    SetLength(Result[I].Tokens, 769 + I);
    for J := 0 to High(Result[I].Tokens) do
      Result[I].Tokens[J] := 'sample' + IntToStr(I) + '.token' + IntToStr(J mod 19);
  end;
end;

procedure CheckInputs(const ASamples: TWfcSequenceSamples);
var
  I: Integer;
  J: Integer;
begin
  Check(Length(ASamples) = 2, 'Corpus changed');
  for I := 0 to 1 do
  begin
    Check(Length(ASamples[I].Tokens) = 769 + I, 'Sample length changed');
    for J := 0 to High(ASamples[I].Tokens) do
      Check(ASamples[I].Tokens[J] =
        'sample' + IntToStr(I) + '.token' + IntToStr(J mod 19), 'Source token changed');
  end;
end;

function CloneModel(const AModel: TWfcSequenceModel;
  const AObserver: TWfcSequencePreparationObserver): TWfcSequenceModel;
begin
  Result := TWfcSequenceModel.Create(AModel.Order, AModel.CopySampleLengths,
    AModel.CopyPublicTokens, AModel.CopyStates, AModel.CopyStateCounts,
    AModel.CopyStartCounts, AModel.CopyEndCounts, AModel.Boundary, AObserver);
end;

procedure TestLearning(const ABoundary: TWfcModelBoundary);
var
  LSamples: TWfcSequenceSamples;
  LReference: TWfcSequenceModel;
  LObserved: TWfcSequenceModel;
  LSingle: TWfcSequenceModel;
  LSingleReference: TWfcSequenceModel;
  LPhase: TWfcSequencePreparationPhase;
  LFinal: Boolean;
  LMode: Integer;
  LExact: Boolean;
  LText: String;
begin
  LSamples := AuthoredSamples;
  LReference := LearnSequenceModelCorpus(LSamples, 4, ABoundary);
  LObserved := nil;
  LSingle := nil;
  LSingleReference := nil;
  try
    LText := EncodeWfcSequenceText(LReference);
    GObserver.Reset;
    LObserved := LearnSequenceModelCorpus(LSamples, 4, ABoundary, GObserver.Observe);
    Check(EncodeWfcSequenceText(LObserved) = LText, 'Observed corpus model differs');
    Check((LObserved.SampleLengthAt(0) = 769) and
      (LObserved.SampleLengthAt(1) = 770), 'Sample isolation differs');
    for LPhase := wspLearnTokenRows to wspModelBuildVisits do
      Check(GObserver.Finals[LPhase] = 1, 'Missing or repeated final learning phase');
    Check(GObserver.Last[wspLearnTokenRows] = 1539, 'Token rows counted capacity');
    Check(GObserver.Last[wspLearnStateRows] = 1539, 'Observation rows differ');
    FreeAndNil(LObserved);
    LSingleReference := LearnSequenceModel(LSamples[0].Tokens, 4, ABoundary);
    GObserver.Reset;
    LSingle := LearnSequenceModel(LSamples[0].Tokens, 4, ABoundary, GObserver.Observe);
    Check(EncodeWfcSequenceText(LSingle) = EncodeWfcSequenceText(LSingleReference),
      'Single wrapper model differs');
    FreeAndNil(LSingle);

    { Corpus and single APIs share the learner; direct construction shares its
      actual model-validation loop. Abort before each candidate can escape. }
    for LMode := 0 to 2 do
      for LPhase := wspLearnTokenRows to wspModelBuildVisits do
      begin
        if (LMode = 2) and (LPhase <> wspModelBuildVisits) then
          Continue;
        for LFinal := False to True do
        begin
          GObserver.Reset;
          GObserver.Phase := LPhase;
          GObserver.FinalOnly := LFinal;
          GObserver.Enabled := True;
          GObserver.Marker := Exception.Create('Big Boss original learning abort');
          LObserved := LReference;
          LExact := False;
          try
            try
              case LMode of
                0: LObserved := LearnSequenceModelCorpus(LSamples, 4,
                  ABoundary, GObserver.Observe);
                1: LObserved := LearnSequenceModel(LSamples[0].Tokens, 4,
                  ABoundary, GObserver.Observe);
                2: LObserved := CloneModel(LReference, GObserver.Observe);
              end;
            except
              on LException: Exception do
              begin
                if LException <> GObserver.Marker then
                  raise;
                GObserver.Marker := nil;
                LExact := True;
                Inc(GAborts);
              end;
            end;
            Check(LExact, 'Original interior/final exception not propagated');
            Check(LObserved = LReference, 'Abort published a partial model');
          finally
            GObserver.Enabled := False;
            GObserver.Marker.Free;
            GObserver.Marker := nil;
            if LObserved <> LReference then
              LObserved.Free;
            LObserved := nil;
          end;
          CheckInputs(LSamples);
          GObserver.Reset;
          case LMode of
            0: LObserved := LearnSequenceModelCorpus(LSamples, 4,
              ABoundary, GObserver.Observe);
            1: LObserved := LearnSequenceModel(LSamples[0].Tokens, 4,
              ABoundary, GObserver.Observe);
            2: LObserved := CloneModel(LReference, GObserver.Observe);
          end;
          try
            if LMode = 1 then
              Check(EncodeWfcSequenceText(LObserved) = EncodeWfcSequenceText(LSingleReference),
                'Single retry model differs')
            else
              Check(EncodeWfcSequenceText(LObserved) = LText, 'Corpus/construction retry differs');
          finally
            FreeAndNil(LObserved);
          end;
        end;
      end;
    CheckInputs(LSamples);
  finally
    LSingle.Free;
    LSingleReference.Free;
    LObserved.Free;
    LReference.Free;
  end;
end;

function FailureText(const ASamples: TWfcSequenceSamples; const AOrder: Integer;
  const AObserved: Boolean): String;
var
  LModel: TWfcSequenceModel;
begin
  Result := '';
  LModel := nil;
  GObserver.Reset;
  try
    try
      if AObserved then
        LModel := LearnSequenceModelCorpus(ASamples, AOrder, wmbOpen, GObserver.Observe)
      else
        LModel := LearnSequenceModelCorpus(ASamples, AOrder);
    except
      on LException: Exception do
        Result := LException.ClassName + ': ' + LException.Message;
    end;
    Check(Result <> '', 'Malformed learning input accepted');
  finally
    LModel.Free;
  end;
end;

procedure TestMalformed;
var
  LSamples: TWfcSequenceSamples;
  LCase: Integer;
  LOrder: Integer;
  LText: String;
  LCalls: Integer;
begin
  for LCase := 0 to 5 do
  begin
    LSamples := AuthoredSamples;
    LOrder := 4;
    case LCase of
      0: LOrder := 0;
      1: LOrder := WFC_SEQUENCE_MAX_ORDER + 1;
      2: LSamples := nil;
      3: LSamples[0].Tokens := nil;
      4:
        begin
          LSamples[0].Tokens[1] := '';
          LSamples[1].Tokens := nil;
        end;
      5:
        begin
          {$ifdef PAS2JS}
          LSamples[0].Tokens[1] := #$D800;
          {$else}
          SetLength(LSamples[0].Tokens[1], 2);
          LSamples[0].Tokens[1][1] := AnsiChar($C0);
          LSamples[0].Tokens[1][2] := AnsiChar($80);
          {$endif}
          LSamples[1].Tokens := nil;
        end;
    end;
    LText := FailureText(LSamples, LOrder, False);
    Check(FailureText(LSamples, LOrder, True) = LText, 'Validation exception/order changed');
    if LCase >= 4 then
      Check(Pos('[0, 1]', LText) > 0,
        'Later empty sample hid earlier invalid token case' + IntToStr(LCase) + ': ' + LText);
  end;
  GObserver.Reset;
  LCalls := GObserver.Calls;
  ReportSequencePreparation(GObserver.Observe, wspLearnTokenRows, 7, 0);
  Check((GObserver.Calls = LCalls + 1) and
    (GObserver.Last[wspLearnTokenRows] = 7), 'Unknown total rejected or fabricated');
end;

procedure TestLarge;
var
  LTokens: TWfcModelTokens;
  LModel: TWfcSequenceModel;
  LObserved: TWfcSequenceModel;
  I: Integer;
  {$ifndef PAS2JS} LStart: QWord; {$endif}
begin
  SetLength(LTokens, 65536);
  { Exact generic token stream from the authored Pythian context repro. The
    generic fixture owns no audio semantics or Pythian dependency. }
  for I := 0 to High(LTokens) do
    LTokens[I] := 'pythian.context.tempo.v1.' + IntToStr(500000 + (I mod 1020));
  LModel := LearnSequenceModel(LTokens, 4);
  LObserved := nil;
  try
    GObserver.Reset;
    {$ifndef PAS2JS} LStart := GetTickCount64; {$endif}
    LObserved := LearnSequenceModel(LTokens, 4, wmbOpen, GObserver.Observe);
    {$ifndef PAS2JS}
    WriteLn('Authored 65536-row/1020-token model observed ms=', GetTickCount64 - LStart);
    {$endif}
    Check((LObserved.StateCount = 1023) and (LObserved.PublicTokenCount = 1020),
      'Original admitted model dimensions differ');
    Check(EncodeWfcSequenceText(LObserved) = EncodeWfcSequenceText(LModel),
      'Large observed model differs');
    Check((GObserver.Last[wspLearnTokenRows] = 65536) and
      (GObserver.Last[wspLearnStateRows] = 65536), 'Large source count differs');
    for I := 0 to High(LTokens) do
      Check(LTokens[I] = 'pythian.context.tempo.v1.' +
        IntToStr(500000 + (I mod 1020)), 'Large source mutated');
    FreeAndNil(LObserved);
    GObserver.Reset;
    LObserved := LModel.Clone(GObserver.Observe);
    Check(EncodeWfcSequenceText(LObserved) = EncodeWfcSequenceText(LModel),
      'Clone preserves the full admitted 1023-state/1020-token model');
    Check(GObserver.Finals[wspModelBuildVisits] = 1,
      'Large clone completes primary observed construction');
  finally
    LObserved.Free;
    LModel.Free;
  end;
end;

begin
  GObserver := TObserver.Create;
  try
    TestMalformed;
    TestLearning(wmbOpen);
    TestLearning(wmbWrap);
    TestLarge;
    TestClones;
    WriteLn('Sequence learning progress: ', GChecks, ' checks; ', GAborts,
      ' original interior/final aborts; ', GObserver.Calls, ' callbacks; ',
      GCloneAborts, ' original clone aborts');
  finally
    GObserver.Free;
  end;
end.
