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
unit wfc_text_training;

{$mode delphi}{$H+}

interface

uses
  wfc_model,
  wfc_training;

const
  WFC_TEXT_TRAINING_VERSION = 1;

type
  EWfcTextTraining = class(EWfcTraining);

  { Raw text samples are explicit and ordered. Text is never split into
    implicit lines or documents: each record becomes one sequence sample. }
  TWfcTextTrainingSample = record
    Name: TWfcModelToken;
    Text: TWfcModelToken;
  end;
  TWfcTextTrainingSamples = array of TWfcTextTrainingSample;

function MakeWfcTextTrainingSample(const AName,
  AText: TWfcModelToken): TWfcTextTrainingSample;

{ Builds an owned immutable sequence-training document using the version-one
  Unicode-scalar tokenizer. Metadata and sample boundaries are preserved. }
function BuildWfcTextTrainingDocument(
  const AMetadata: TWfcTrainingMetadata;
  const ASamples: TWfcTextTrainingSamples;
  const AOrder: Integer): TWfcTrainingDocument;

implementation

uses
  SysUtils,
  wfc_text_codec,
  wfc_text_tokenize,
  wfc_token_lookup;

{$IFDEF PAS2JS}
const
  { JavaScript strings use at most one UTF-16 surrogate pair per scalar. }
  MAX_SCALAR_STORAGE_LENGTH = 2;
{$ELSE}
const
  { Native TWfcModelToken values use at most four UTF-8 bytes per scalar. }
  MAX_SCALAR_STORAGE_LENGTH = 4;
{$ENDIF}

function MakeWfcTextTrainingSample(const AName,
  AText: TWfcModelToken): TWfcTextTrainingSample;
begin
  Result.Name := AName;
  Result.Text := AText;
end;

procedure AccumulateEncodedTokenField(const AValue: TWfcModelToken;
  const ALabel: String; var AEncodedTotal: Integer);
var
  LEncoded: String;
  LStorageLength: SizeInt;
begin
  LStorageLength := Length(AValue);
  if LStorageLength = 0 then
    raise EWfcTextTraining.Create(ALabel + ' cannot be empty');
  if (LStorageLength < 0) or
      (LStorageLength > WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH) then
    raise EWfcTextTraining.Create(ALabel +
      ' exceeds the version-1 encoded-token limit');
  if LStorageLength >
      WFC_TRAINING_MAX_TOTAL_ENCODED_TOKEN_LENGTH - AEncodedTotal then
    raise EWfcTextTraining.Create(
      'aggregate encoded text-training content exceeds the version-1 limit');
  try
    LEncoded := WfcTextEncodeToken(AValue, 'WFC text training');
  except
    on E: EConvertError do
      raise EWfcTextTraining.Create(ALabel + ': ' + E.Message);
  end;
  if Length(LEncoded) > WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH then
    raise EWfcTextTraining.Create(ALabel +
      ' exceeds the version-1 encoded-token limit');
  if Length(LEncoded) >
      WFC_TRAINING_MAX_TOTAL_ENCODED_TOKEN_LENGTH - AEncodedTotal then
    raise EWfcTextTraining.Create(
      'aggregate encoded text-training content exceeds the version-1 limit');
  Inc(AEncodedTotal, Length(LEncoded));
end;

procedure AccumulateEncodedRawText(const AText: TWfcModelToken;
  const ASampleIndex: Integer; var AStorageTotal,
  AEncodedTotal: Integer);
var
  LEncoded: String;
  LStorageLength: SizeInt;
begin
  LStorageLength := Length(AText);
  if LStorageLength = 0 then
    raise EWfcTextTraining.CreateFmt(
      'raw text sample cannot be empty [%d]', [ASampleIndex]);
  { Every valid scalar occupies at most MAX_SCALAR_STORAGE_LENGTH units on
    this target. This conservative aggregate bound rejects a corpus that
    cannot fit the scalar budget before canonical encoding duplicates it. }
  if (LStorageLength < 0) or
      (LStorageLength >
      (WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT * MAX_SCALAR_STORAGE_LENGTH) -
      AStorageTotal) then
    raise EWfcTextTraining.Create(
      'aggregate raw text scalar count exceeds the version-1 limit');
  Inc(AStorageTotal, LStorageLength);
  { Canonical percent encoding is never shorter than native UTF-8 bytes or
    pas2js UTF-16 code units. Reject impossible aggregate budgets before the
    codec allocates an encoded copy. }
  if (LStorageLength < 0) or
      (LStorageLength >
      WFC_TRAINING_MAX_TOTAL_ENCODED_TOKEN_LENGTH - AEncodedTotal) then
    raise EWfcTextTraining.Create(
      'aggregate encoded text-training content exceeds the version-1 limit');
  try
    LEncoded := WfcTextEncodeToken(AText, 'WFC text training');
  except
    on E: EConvertError do
      raise EWfcTextTraining.CreateFmt(
        'raw text sample contains invalid Unicode [%d]: %s',
        [ASampleIndex, E.Message]);
  end;
  { Encoding the complete raw text has exactly the sum of encoding its
    individual scalar tokens, so this is the eventual document budget. }
  if Length(LEncoded) >
      WFC_TRAINING_MAX_TOTAL_ENCODED_TOKEN_LENGTH - AEncodedTotal then
    raise EWfcTextTraining.Create(
      'aggregate encoded text-training content exceeds the version-1 limit');
  Inc(AEncodedTotal, Length(LEncoded));
end;

procedure ValidateSampleNames(const ASamples: TWfcTextTrainingSamples;
  var AEncodedTotal: Integer);
var
  I: Integer;
  LLookup: TWfcTokenLookup;
  LNames: TWfcModelTokens;
begin
  LNames := nil;
  SetLength(LNames, Length(ASamples));
  for I := 0 to Length(ASamples) - 1 do
  begin
    AccumulateEncodedTokenField(ASamples[I].Name,
      Format('raw text sample name %d', [I]), AEncodedTotal);
    LNames[I] := ASamples[I].Name;
  end;

  LLookup := nil;
  try
    try
      LLookup := TWfcTokenLookup.Create(LNames);
    except
      on E: EWfcTokenLookup do
        raise EWfcTextTraining.Create(
          'raw text sample names must be unique: ' + E.Message);
    end;
  finally
    LLookup.Free;
  end;
end;

function TokenizeSamples(const ASamples: TWfcTextTrainingSamples):
  TWfcTrainingSamples;
var
  I: Integer;
  LRemaining: Integer;
  LStorageLength: Integer;
  LTokenCount: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ASamples));
  LTokenCount := 0;
  for I := 0 to Length(ASamples) - 1 do
  begin
    LRemaining := WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT - LTokenCount;
    LStorageLength := Integer(Length(ASamples[I].Text));
    if LStorageLength > LRemaining * MAX_SCALAR_STORAGE_LENGTH then
      raise EWfcTextTraining.Create(
        'aggregate raw text scalar count exceeds the version-1 limit');
    try
      Result[I].Tokens := TokenizeWfcText(ASamples[I].Text,
        wttkUnicodeScalar);
    except
      on E: EWfcTextTokenize do
        raise EWfcTextTraining.CreateFmt(
          'raw text sample tokenization failed [%d]: %s',
          [I, E.Message]);
    end;
    if Length(Result[I].Tokens) > LRemaining then
      raise EWfcTextTraining.Create(
        'aggregate raw text scalar count exceeds the version-1 limit');
    Inc(LTokenCount, Length(Result[I].Tokens));
    Result[I].Name := ASamples[I].Name;
    Result[I].Width := Length(Result[I].Tokens);
    Result[I].Height := 1;
  end;
end;

function BuildWfcTextTrainingDocument(
  const AMetadata: TWfcTrainingMetadata;
  const ASamples: TWfcTextTrainingSamples;
  const AOrder: Integer): TWfcTrainingDocument;
var
  I: Integer;
  LEncodedTotal: Integer;
  LOptions: TWfcTrainingOptions;
  LStorageTotal: Integer;
  LTrainingSamples: TWfcTrainingSamples;
begin
  Result := nil;
  if (AOrder < 1) or (AOrder > WFC_TRAINING_MAX_ORDER) then
    raise EWfcTextTraining.Create(
      'text training order is outside the version-1 limit');
  if (Length(ASamples) = 0) or
      (Length(ASamples) > WFC_TRAINING_MAX_SAMPLE_COUNT) then
    raise EWfcTextTraining.Create(
      'text training sample count is outside the version-1 limit');

  LEncodedTotal := 0;
  AccumulateEncodedTokenField(AMetadata.Name,
    'text training name', LEncodedTotal);
  AccumulateEncodedTokenField(AMetadata.LicenseIdentifier,
    'text training license identifier', LEncodedTotal);
  AccumulateEncodedTokenField(AMetadata.SourceDescription,
    'text training source description', LEncodedTotal);
  ValidateSampleNames(ASamples, LEncodedTotal);
  LStorageTotal := 0;
  for I := 0 to Length(ASamples) - 1 do
    AccumulateEncodedRawText(ASamples[I].Text, I, LStorageTotal,
      LEncodedTotal);

  LTrainingSamples := TokenizeSamples(ASamples);
  LOptions := MakeWfcTrainingOptions(wtkSequence, wmbOpen, wmsNone,
    0, 0, AOrder);
  try
    Result := TWfcTrainingDocument.Create(AMetadata, LOptions,
      LTrainingSamples);
  except
    on E: EWfcTraining do
      raise EWfcTextTraining.Create(E.Message);
  end;
end;

end.
