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
unit wfc_text_tokenize;

{$mode delphi}{$H+}

interface

uses
  wfc_model,
  wfc_sequence,
  wfc_sequence_learn;

const
  WFC_TEXT_TOKENIZER_VERSION = 1;

type
  EWfcTextTokenize = class(EWfcSequence);

  TWfcTextTokenizerKind = (
    wttkUnicodeScalar
  );

  TWfcTextDocuments = array of TWfcModelToken;

  { Portable caller-defined tokenizers use ordinary procedure variables. The
    sequence learner remains the validation boundary for their output. }
  TWfcTextTokenizeCallback = function(
    const AText: TWfcModelToken): TWfcModelTokens;

function WfcTextTokenizerName(
  const AKind: TWfcTextTokenizerKind): String;

function TokenizeWfcText(const AText: TWfcModelToken;
  const AKind: TWfcTextTokenizerKind): TWfcModelTokens;

function DetokenizeWfcText(const ATokens: TWfcModelTokens;
  const AKind: TWfcTextTokenizerKind): TWfcModelToken;

function TokenizeWfcTextDocuments(const ADocuments: TWfcTextDocuments;
  const AKind: TWfcTextTokenizerKind): TWfcSequenceSamples; overload;

function TokenizeWfcTextDocuments(const ADocuments: TWfcTextDocuments;
  const ATokenizer: TWfcTextTokenizeCallback):
  TWfcSequenceSamples; overload;

function LearnWfcTextModel(const ADocuments: TWfcTextDocuments;
  const AOrder: Integer; const AKind: TWfcTextTokenizerKind):
  TWfcSequenceModel; overload;

function LearnWfcTextModel(const ADocuments: TWfcTextDocuments;
  const AOrder: Integer; const ATokenizer: TWfcTextTokenizeCallback):
  TWfcSequenceModel; overload;

implementation

uses
  SysUtils;

function CheckedTextManagedLength(const ALength: SizeInt;
  const ALabel: String): Integer;
begin
  if (ALength < 0) or
      ((ALength and (not SizeInt(High(Integer)))) <> 0) then
    raise ERangeError.Create(ALabel + ' exceeds the Integer range');
  Result := Integer(ALength);
end;

function TextTokenizerKindIsValid(
  const AKind: TWfcTextTokenizerKind): Boolean;
begin
  Result := Ord(AKind) = Ord(wttkUnicodeScalar);
end;

procedure ValidateTextTokenizerKind(
  const AKind: TWfcTextTokenizerKind);
begin
  if not TextTokenizerKindIsValid(AKind) then
    raise EArgumentException.CreateFmt(
      'unknown WFC text tokenizer kind [%d]', [Ord(AKind)]);
end;

function WfcTextTokenizerName(
  const AKind: TWfcTextTokenizerKind): String;
begin
  ValidateTextTokenizerKind(AKind);
  Result := 'unicode-scalar';
end;

function ScalarStorageLengthAt(const AText: TWfcModelToken;
  const AIndex: Integer): Integer;
{$IFDEF PAS2JS}
var
  LCodeUnit: Integer;
{$ELSE}
var
  LFirstByte: Byte;
{$ENDIF}
begin
  {$IFDEF PAS2JS}
  LCodeUnit := Ord(AText[AIndex]);
  if (LCodeUnit >= $D800) and (LCodeUnit <= $DBFF) then
    Result := 2
  else
    Result := 1;
  {$ELSE}
  LFirstByte := Byte(AText[AIndex]);
  if LFirstByte <= $7F then
    Result := 1
  else if LFirstByte <= $DF then
    Result := 2
  else if LFirstByte <= $EF then
    Result := 3
  else
    Result := 4;
  {$ENDIF}
end;

procedure ValidateTextStorage(const AText: TWfcModelToken;
  const AAllowEmpty: Boolean);
begin
  if Length(AText) = 0 then
  begin
    if AAllowEmpty then
      Exit;
    raise EWfcTextTokenize.Create('text token cannot be empty');
  end;
  if not WfcModelTokenIsValid(AText) then
    raise EWfcTextTokenize.Create(
      'text contains an invalid Unicode scalar sequence');
end;

function TokenizeWfcText(const AText: TWfcModelToken;
  const AKind: TWfcTextTokenizerKind): TWfcModelTokens;
var
  I: Integer;
  LCount: Integer;
  LScalarLength: Integer;
  LStorageLength: Integer;
begin
  Result := nil;
  ValidateTextTokenizerKind(AKind);
  LStorageLength := CheckedTextManagedLength(Length(AText),
    'text storage length');
  ValidateTextStorage(AText, True);
  if LStorageLength = 0 then
    Exit;

  { A Unicode scalar occupies at least one native UTF-8 byte or JavaScript
    UTF-16 code unit, so this is a checked upper bound with no host codec. }
  SetLength(Result, LStorageLength);
  I := 1;
  LCount := 0;
  while I <= LStorageLength do
  begin
    LScalarLength := ScalarStorageLengthAt(AText, I);
    Result[LCount] := Copy(AText, I, LScalarLength);
    Inc(LCount);
    Inc(I, LScalarLength);
  end;
  SetLength(Result, LCount);
end;

function DetokenizeWfcText(const ATokens: TWfcModelTokens;
  const AKind: TWfcTextTokenizerKind): TWfcModelToken;
var
  I: Integer;
  LLength: Integer;
  LTokenCount: Integer;
  LTokenLength: Integer;
begin
  Result := '';
  ValidateTextTokenizerKind(AKind);
  LLength := 0;
  LTokenCount := CheckedTextManagedLength(Length(ATokens),
    'text token count');
  for I := 0 to LTokenCount - 1 do
  begin
    LTokenLength := CheckedTextManagedLength(Length(ATokens[I]),
      Format('text token %d storage length', [I]));
    ValidateTextStorage(ATokens[I], False);
    if ScalarStorageLengthAt(ATokens[I], 1) <> LTokenLength then
      raise EWfcTextTokenize.CreateFmt(
        'text token %d is not exactly one Unicode scalar', [I]);
    if LTokenLength > High(Integer) - LLength then
      raise ERangeError.Create('detokenized text is too large');
    Inc(LLength, LTokenLength);
  end;
  for I := 0 to LTokenCount - 1 do
    Result := Result + ATokens[I];
end;

function TokenizeWfcTextDocuments(const ADocuments: TWfcTextDocuments;
  const AKind: TWfcTextTokenizerKind): TWfcSequenceSamples;
var
  I: Integer;
  LDocumentCount: Integer;
begin
  ValidateTextTokenizerKind(AKind);
  Result := nil;
  LDocumentCount := CheckedTextManagedLength(Length(ADocuments),
    'text document count');
  SetLength(Result, LDocumentCount);
  for I := 0 to LDocumentCount - 1 do
    Result[I] := MakeWfcSequenceSample(
      TokenizeWfcText(ADocuments[I], AKind));
end;

function TokenizeWfcTextDocuments(const ADocuments: TWfcTextDocuments;
  const ATokenizer: TWfcTextTokenizeCallback): TWfcSequenceSamples;
var
  I: Integer;
  LDocumentCount: Integer;
begin
  if not Assigned(ATokenizer) then
    raise EArgumentNilException.Create('text tokenizer cannot be nil');
  Result := nil;
  LDocumentCount := CheckedTextManagedLength(Length(ADocuments),
    'text document count');
  SetLength(Result, LDocumentCount);
  for I := 0 to LDocumentCount - 1 do
    Result[I] := MakeWfcSequenceSample(ATokenizer(ADocuments[I]));
end;

function LearnWfcTextModel(const ADocuments: TWfcTextDocuments;
  const AOrder: Integer; const AKind: TWfcTextTokenizerKind):
  TWfcSequenceModel;
var
  LSamples: TWfcSequenceSamples;
begin
  LSamples := TokenizeWfcTextDocuments(ADocuments, AKind);
  Result := LearnSequenceModelCorpus(LSamples, AOrder);
end;

function LearnWfcTextModel(const ADocuments: TWfcTextDocuments;
  const AOrder: Integer; const ATokenizer: TWfcTextTokenizeCallback):
  TWfcSequenceModel;
var
  LSamples: TWfcSequenceSamples;
begin
  LSamples := TokenizeWfcTextDocuments(ADocuments, ATokenizer);
  Result := LearnSequenceModelCorpus(LSamples, AOrder);
end;

end.
