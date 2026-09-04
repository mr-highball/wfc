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
unit text_completion_demo;

{$mode delphi}{$H+}

interface

uses
  wfc;

function ParseTextCompletionSeed: TGraphSeed;
procedure RunTextCompletionDemo(const ASeed: TGraphSeed);

implementation

uses
  SysUtils,
  wfc_model,
  wfc_sequence,
  wfc_sequence_analyze,
  wfc_text_complete,
  wfc_text_tokenize;

type
  ETextCompletionDemo = class(Exception);

function ParseTextCompletionSeed: TGraphSeed;
var
  I: Integer;
  LDigit: TGraphSeed;
  LParsed: TGraphSeed;
  LText: String;
begin
  if ParamCount = 0 then
    Exit(0);
  if ParamCount <> 1 then
    raise EConvertError.Create(
      'usage: ConstraintCompletion [unsigned-32-bit-seed]');
  LText := ParamStr(1);
  if LText = '' then
    raise EConvertError.Create(
      'invalid seed: expected an unsigned 32-bit integer');
  LParsed := 0;
  for I := 1 to Length(LText) do
  begin
    if not (LText[I] in ['0'..'9']) then
      raise EConvertError.CreateFmt(
        'invalid seed "%s": expected an unsigned 32-bit integer',
        [LText]);
    LDigit := TGraphSeed(Ord(LText[I]) - Ord('0'));
    if LParsed > (High(TGraphSeed) - LDigit) div 10 then
      raise EConvertError.CreateFmt(
        'invalid seed "%s": maximum value is 4294967295', [LText]);
    LParsed := (LParsed * 10) + LDigit;
  end;
  Result := LParsed;
end;

function TextOf(const AValue: String): TWfcModelToken;
begin
  Result := TWfcModelToken(AValue);
end;

function BuildDocuments: TWfcTextDocuments;
begin
  Result := nil;
  SetLength(Result, 2);
  Result[0] := TextOf('the quick fox rests.');
  Result[1] := TextOf('the quiet owl rests.');
end;

function TokensEqual(const A, B: TWfcModelTokens): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to Length(A) - 1 do
    if A[I] <> B[I] then
      Exit(False);
  Result := True;
end;

function StateIndicesEqual(const A, B: TWfcSequenceStateIndices): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to Length(A) - 1 do
    if A[I] <> B[I] then
      Exit(False);
  Result := True;
end;

procedure WriteDomain(const ADomain: TWfcSequencePositionDomain);
var
  I: Integer;
begin
  for I := 0 to Length(ADomain.Tokens) - 1 do
  begin
    if I > 0 then
      Write(' | ');
    Write(String(ADomain.Tokens[I]));
  end;
  WriteLn;
end;

function BuildInfillRequest(const ASeed: TGraphSeed):
  TWfcTextCompletionRequest;
begin
  Result := DefaultWfcTextCompletionRequest(20, wseWhole, ASeed);
  Result.Prefix := TokenizeWfcText(TextOf('the qui'),
    wttkUnicodeScalar);
  Result.Suffix := TokenizeWfcText(TextOf(' rests.'),
    wttkUnicodeScalar);
  SetLength(Result.LockedSpans, 1);
  Result.LockedSpans[0] := MakeWfcTextLockedSpan(9,
    TokenizeWfcText(TextOf(' '), wttkUnicodeScalar));
  SetLength(Result.Domains, 1);
  Result.Domains[0] := MakeWfcSequenceTokenConstraint(7,
    TokenizeWfcText(TextOf('ce'), wttkUnicodeScalar));
end;

function BuildPrefixRequest(const ASeed: TGraphSeed;
  const AExtent: TWfcSequenceExtent): TWfcTextCompletionRequest;
begin
  Result := DefaultWfcTextCompletionRequest(9, AExtent, ASeed);
  Result.Prefix := TokenizeWfcText(TextOf('the qu'),
    wttkUnicodeScalar);
end;

procedure RequireCompletion(const AModel: TWfcSequenceModel;
  const ARequest: TWfcTextCompletionRequest;
  out ACompletion: TWfcTextCompletion;
  out AReport: TWfcTextCompletionReport; const ALabel: String);
begin
  if not TryCompleteWfcText(AModel, ARequest, ACompletion, AReport) then
  begin
    if AReport.Status = wctcsUnsatisfiable then
      raise ETextCompletionDemo.Create(ALabel + ': ' +
        DescribeSequenceAnalyzeIssue(AReport.Analysis.Issue));
    raise ETextCompletionDemo.CreateFmt('%s failed with status %d',
      [ALabel, Ord(AReport.Status)]);
  end;
end;

procedure RunTextCompletionDemo(const ASeed: TGraphSeed);
var
  LCompletion: TWfcTextCompletion;
  LDocuments: TWfcTextDocuments;
  LInfillRequest: TWfcTextCompletionRequest;
  LModel: TWfcSequenceModel;
  LPrefixCompletion: TWfcTextCompletion;
  LPrefixReport: TWfcTextCompletionReport;
  LReplay: TWfcTextCompletion;
  LReplayReport: TWfcTextCompletionReport;
  LReport: TWfcTextCompletionReport;
  LWholeShortRequest: TWfcTextCompletionRequest;
begin
  LModel := nil;
  LDocuments := BuildDocuments;
  try
    LModel := LearnWfcTextModel(LDocuments, 3, wttkUnicodeScalar);
    LInfillRequest := BuildInfillRequest(ASeed);
    RequireCompletion(LModel, LInfillRequest, LCompletion, LReport,
      'anchored infill');
    RequireCompletion(LModel, LInfillRequest, LReplay, LReplayReport,
      'replay');
    if (LReplay.Text <> LCompletion.Text) or
        (not TokensEqual(LReplay.Generated.Tokens,
          LCompletion.Generated.Tokens)) or
        (not StateIndicesEqual(LReplay.Generated.StateIndices,
          LCompletion.Generated.StateIndices)) then
      raise ETextCompletionDemo.Create('fixed-seed replay changed');

    RequireCompletion(LModel, BuildPrefixRequest(ASeed, wsePrefix),
      LPrefixCompletion, LPrefixReport, 'prefix continuation');
    LWholeShortRequest := BuildPrefixRequest(ASeed, wseWhole);
    if TryCompleteWfcText(LModel, LWholeShortRequest, LReplay,
        LReplayReport) or
        (LReplayReport.Status <> wctcsUnsatisfiable) then
      raise ETextCompletionDemo.Create(
        'whole-sample boundary accepted a truncated completion');

    if not TokensEqual(LReport.Analysis.Positions[7].Tokens,
        TokenizeWfcText(TextOf('ec'), wttkUnicodeScalar)) then
      raise ETextCompletionDemo.Create(
        'position-seven feasible domain changed');
    if ASeed = 0 then
    begin
      if LCompletion.Text <> TextOf('the quick fox rests.') then
        raise ETextCompletionDemo.Create(
          'seed-zero anchored infill replay changed');
      if LPrefixCompletion.Text <> TextOf('the quick') then
        raise ETextCompletionDemo.Create(
          'seed-zero prefix continuation replay changed');
    end;

    WriteLn('WFC Text Constraint Completion');
    WriteLn('Tokenizer: ', WfcTextTokenizerName(wttkUnicodeScalar));
    WriteLn('Order: ', LModel.Order);
    WriteLn('Samples: ', LModel.SampleCount);
    WriteLn('Public scalars: ', LModel.PublicTokenCount);
    WriteLn('Latent states: ', LModel.StateCount);
    WriteLn('Seed: ', ASeed);
    Write('Position 7 domain: ');
    WriteDomain(LReport.Analysis.Positions[7]);
    WriteLn('Anchored infill: ', String(LCompletion.Text));
    WriteLn('Prefix continuation: ', String(LPrefixCompletion.Text));
    WriteLn('Truncated whole sample: rejected');
    WriteLn('Independent validation: verified');
    WriteLn('Replay: verified');
  finally
    LModel.Free;
  end;
end;

end.
