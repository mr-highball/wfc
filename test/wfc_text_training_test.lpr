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
program wfc_text_training_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc_model,
  wfc_sequence,
  wfc_sequence_learn,
  wfc_sequence_text,
  wfc_text_tokenize,
  wfc_training,
  wfc_pipeline_model,
  wfc_text_training;

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GCheckCount);
  if ACondition then
    WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailureCount);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

procedure BeginTest(const AName: String);
begin
  WriteLn('[TEST] ', AName);
end;

function BmpToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($266B));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(
    UnicodeString(WideChar($266B))));
  {$ENDIF}
end;

function CombiningToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($0301));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(
    UnicodeString(WideChar($0301))));
  {$ENDIF}
end;

function NonBmpToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($D83D) + Chr($DE42));
  {$ELSE}
  SetLength(Result, 4);
  Result[1] := AnsiChar($F0);
  Result[2] := AnsiChar($9F);
  Result[3] := AnsiChar($99);
  Result[4] := AnsiChar($82);
  {$ENDIF}
end;

function MalformedText: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($D800));
  {$ELSE}
  SetLength(Result, 1);
  Result[1] := AnsiChar($FF);
  {$ENDIF}
end;

function RepeatedNonBmpText(const ACount: Integer): TWfcModelToken;
var
  I: Integer;
begin
  {$IFDEF PAS2JS}
  SetLength(Result, ACount * 2);
  for I := 0 to ACount - 1 do
  begin
    Result[I * 2 + 1] := Chr($D83D);
    Result[I * 2 + 2] := Chr($DE42);
  end;
  {$ELSE}
  SetLength(Result, ACount * 4);
  for I := 0 to ACount - 1 do
  begin
    Result[I * 4 + 1] := AnsiChar($F0);
    Result[I * 4 + 2] := AnsiChar($9F);
    Result[I * 4 + 3] := AnsiChar($99);
    Result[I * 4 + 4] := AnsiChar($82);
  end;
  {$ENDIF}
end;

function Metadata: TWfcTrainingMetadata;
begin
  Result := MakeWfcTrainingMetadata('raw text corpus', 'MIT',
    'two explicitly named project-authored texts');
end;

function TokensMatch(const AActual: TWfcModelTokens;
  const AExpected: array of TWfcModelToken): Boolean;
var
  I: Integer;
begin
  if Length(AActual) <> Length(AExpected) then
    Exit(False);
  for I := 0 to Length(AActual) - 1 do
    if AActual[I] <> AExpected[I] then
      Exit(False);
  Result := True;
end;

function HasObservedPair(const AModel: TWfcSequenceModel;
  const APrevious, AEmitted: TWfcModelToken): Boolean;
var
  I: Integer;
  LHistory: TWfcSequenceHistoryItem;
begin
  for I := 0 to AModel.StateCount - 1 do
    if AModel.ProjectStateToken(I) = AEmitted then
    begin
      LHistory := AModel.HistoryItemAt(I, 0);
      if (LHistory.Kind = wshToken) and
          (AModel.PublicTokenAt(LHistory.TokenIndex) = APrevious) then
        Exit(True);
    end;
  Result := False;
end;

procedure CheckRejected(const AMetadata: TWfcTrainingMetadata;
  const ASamples: TWfcTextTrainingSamples; const AOrder: Integer;
  const AExpected, AMessage: String);
var
  LDocument: TWfcTrainingDocument;
  LError: String;
  LRaised: Boolean;
begin
  LDocument := nil;
  LError := '';
  LRaised := False;
  try
    try
      LDocument := BuildWfcTextTrainingDocument(AMetadata, ASamples,
        AOrder);
    except
      on E: EWfcTextTraining do
      begin
        LRaised := True;
        LError := E.Message;
      end;
    end;
  finally
    LDocument.Free;
  end;
  Check(LRaised and (Pos(AExpected, LError) > 0), AMessage);
end;

procedure TestExactBridge;
var
  LDirect: TWfcSequenceModel;
  LDirectSamples: TWfcSequenceSamples;
  LDocument: TWfcTrainingDocument;
  LFirstText: TWfcModelToken;
  LMetadata: TWfcTrainingMetadata;
  LOptions: TWfcTrainingOptions;
  LPipelineMetadata: TWfcPipelineMetadata;
  LRecipe: TWfcPipelineModel;
  LSample: TWfcTrainingSample;
  LSamples: TWfcTextTrainingSamples;
  LSecondText: TWfcModelToken;
begin
  BeginTest('exact raw text bridge and provenance');
  LFirstText := 'A ' + BmpToken + NonBmpToken + #9#13#10;
  LSecondText := 'B' + CombiningToken + '!';
  LMetadata := Metadata;
  SetLength(LSamples, 2);
  LSamples[0] := MakeWfcTextTrainingSample('first text', LFirstText);
  LSamples[1] := MakeWfcTextTrainingSample('second text', LSecondText);
  LDocument := BuildWfcTextTrainingDocument(LMetadata, LSamples, 2);
  try
    Check(WFC_TEXT_TRAINING_VERSION = 1,
      'text-training bridge version is pinned');
    LOptions := LDocument.CopyOptions;
    Check((LOptions.Kind = wtkSequence) and
      (LOptions.Boundary = wmbOpen) and
      (LOptions.Symmetry = wmsNone) and
      (LOptions.PatternWidth = 0) and
      (LOptions.PatternHeight = 0) and (LOptions.Order = 2),
      'raw text lowers to explicit open sequence options');
    Check((LDocument.SampleCount = 2) and
      (LDocument.TotalTokenCount = 10),
      'explicit raw samples retain independent counts and order');

    LSample := LDocument.SampleAt(0);
    Check((LSample.Name = 'first text') and (LSample.Width = 7) and
      (LSample.Height = 1) and TokensMatch(LSample.Tokens,
      ['A', ' ', BmpToken, NonBmpToken, #9, #13, #10]),
      'spaces, tabs, CR, LF, BMP, and supplementary scalars remain exact');
    Check(DetokenizeWfcText(LSample.Tokens, wttkUnicodeScalar) =
      LFirstText, 'first raw text round-trips without line splitting');
    LSample := LDocument.SampleAt(1);
    Check(TokensMatch(LSample.Tokens,
      ['B', CombiningToken, '!']) and
      (DetokenizeWfcText(LSample.Tokens, wttkUnicodeScalar) =
      LSecondText),
      'combining marks retain scalar identity without normalization');
    Check((LDocument.CopyMetadata.Name = LMetadata.Name) and
      (LDocument.CopyMetadata.LicenseIdentifier =
      LMetadata.LicenseIdentifier) and
      (LDocument.CopyMetadata.SourceDescription =
      LMetadata.SourceDescription),
      'caller metadata is preserved verbatim');
    Check(WfcTrainingSignatureHex(LDocument.Signature) = '8FC01692',
      'portable raw-text training identity is pinned (' +
      WfcTrainingSignatureHex(LDocument.Signature) + ')');

    SetLength(LDirectSamples, 2);
    LDirectSamples[0] := MakeWfcSequenceSample(
      TokenizeWfcText(LFirstText, wttkUnicodeScalar));
    LDirectSamples[1] := MakeWfcSequenceSample(
      TokenizeWfcText(LSecondText, wttkUnicodeScalar));
    LDirect := LearnSequenceModelCorpus(LDirectSamples, 2);
    try
      Check(LearnWfcTrainingModelText(LDocument) =
        EncodeWfcSequenceText(LDirect),
        'bridge output exactly matches the existing scalar sequence learner');
      Check(HasObservedPair(LDirect, #13, #10),
        'CR-to-LF structure inside one sample is learned');
      Check(not HasObservedPair(LDirect, #10, 'B'),
        'no relationship is invented across explicit sample boundaries');
    finally
      LDirect.Free;
    end;

    LRecipe := LearnWfcTrainingRecipe(LDocument);
    try
      LPipelineMetadata := LRecipe.CopyMetadata;
      Check((LPipelineMetadata.Name = LMetadata.Name) and
        (LPipelineMetadata.LicenseIdentifier =
        LMetadata.LicenseIdentifier) and
        (Pos('first%20text', LPipelineMetadata.SourceDescription) > 0) and
        (Pos('second%20text', LPipelineMetadata.SourceDescription) > 0) and
        (LPipelineMetadata.SourceFingerprint =
        TWfcModelToken('wfclearn-v1/' +
        WfcTrainingSignatureHex(LDocument.Signature))),
        'recipe provenance retains metadata, names, and document identity');
    finally
      LRecipe.Free;
    end;

    LSamples[0].Name := 'mutated';
    LSamples[0].Text := 'mutated';
    LMetadata.Name := 'mutated';
    Check((LDocument.SampleAt(0).Name = 'first text') and
      (DetokenizeWfcText(LDocument.SampleAt(0).Tokens,
      wttkUnicodeScalar) = LFirstText) and
      (LDocument.CopyMetadata.Name = 'raw text corpus'),
      'document is detached from raw caller records and metadata');
  finally
    LDocument.Free;
  end;
end;

procedure TestValidationAndBudgets;
var
  LDocument: TWfcTrainingDocument;
  LMetadata: TWfcTrainingMetadata;
  LSamples: TWfcTextTrainingSamples;
  LStorageCeiling: Integer;
  LSupplementaryCount: Integer;
begin
  BeginTest('raw text validation and preflight budgets');
  LMetadata := Metadata;
  CheckRejected(LMetadata, nil, 2, 'sample count',
    'an empty raw corpus is rejected before allocation');

  SetLength(LSamples, WFC_TRAINING_MAX_SAMPLE_COUNT + 1);
  CheckRejected(LMetadata, LSamples, 2, 'sample count',
    'oversized sample count is rejected before inspecting records');

  SetLength(LSamples, 1);
  LSamples[0] := MakeWfcTextTrainingSample('empty', '');
  CheckRejected(LMetadata, LSamples, 2, 'cannot be empty',
    'empty raw text is rejected rather than hidden or skipped');
  CheckRejected(LMetadata, LSamples, 0, 'order',
    'zero sequence order is rejected before tokenization');
  CheckRejected(LMetadata, LSamples, WFC_TRAINING_MAX_ORDER + 1,
    'order', 'oversized sequence order is rejected before tokenization');

  SetLength(LSamples, 2);
  LSamples[0] := MakeWfcTextTrainingSample('same', 'A');
  LSamples[1] := MakeWfcTextTrainingSample('same', 'B');
  CheckRejected(LMetadata, LSamples, 2, 'must be unique',
    'duplicate explicit sample names are rejected before tokenization');

  SetLength(LSamples, 1);
  LSamples[0] := MakeWfcTextTrainingSample('malformed', MalformedText);
  CheckRejected(LMetadata, LSamples, 2, 'invalid Unicode',
    'malformed UTF-8 or UTF-16 raw text is rejected with sample context');

  LSamples[0] := MakeWfcTextTrainingSample('too-many-scalars',
    StringOfChar('x', WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT + 1));
  CheckRejected(LMetadata, LSamples, 1, 'scalar count',
    'exact scalar count is bounded before immutable-document duplication');

  {$IFDEF PAS2JS}
  LStorageCeiling := WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT * 2;
  {$ELSE}
  LStorageCeiling := WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT * 4;
  {$ENDIF}
  LSamples[0] := MakeWfcTextTrainingSample('impossible-storage',
    StringOfChar('x', LStorageCeiling + 1));
  CheckRejected(LMetadata, LSamples, 1, 'scalar count',
    'impossible raw storage is rejected before canonical encoding');

  { This valid scalar count occupies more than 65,536 native UTF-8 bytes.
    It catches a host-dependent raw-byte/token-count comparison without
    making the portable test pay the full maximum-corpus runtime. }
  LSupplementaryCount :=
    (WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT div 4) + 1;
  LSamples[0] := MakeWfcTextTrainingSample('supplementary-storage',
    RepeatedNonBmpText(LSupplementaryCount));
  LDocument := BuildWfcTextTrainingDocument(LMetadata, LSamples, 1);
  try
    Check(LDocument.TotalTokenCount = LSupplementaryCount,
      'supplementary scalars are not rejected by native UTF-8 storage width');
  finally
    LDocument.Free;
  end;

  LMetadata := Metadata;
  LMetadata.Name := TWfcModelToken(StringOfChar('n',
    WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH + 1));
  LSamples[0] := MakeWfcTextTrainingSample('small', 'A');
  CheckRejected(LMetadata, LSamples, 1, 'encoded-token limit',
    'oversized metadata is rejected before raw token arrays');
end;

begin
  TestExactBridge;
  TestValidationAndBudgets;
  WriteLn('Checks: ', GCheckCount, ', Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d text-training checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
