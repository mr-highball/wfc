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
unit wfc_text_complete;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_sequence,
  wfc_sequence_analyze,
  wfc_sequence_graph,
  wfc_text_tokenize;

const
  WFC_TEXT_COMPLETION_VERSION = 1;

type
  EWfcTextComplete = class(EWfcSequence);

  TWfcTextLockedSpan = record
    Start: Integer;
    Tokens: TWfcModelTokens;
  end;
  TWfcTextLockedSpans = array of TWfcTextLockedSpan;

  TWfcTextCompletionRequest = record
    TokenLength: Integer;
    Extent: TWfcSequenceExtent;
    Seed: TGraphSeed;
    Tokenizer: TWfcTextTokenizerKind;
    Prefix: TWfcModelTokens;
    Suffix: TWfcModelTokens;
    LockedSpans: TWfcTextLockedSpans;
    Domains: TWfcSequenceTokenConstraints;
    SolveOptions: TGraphSolveOptions;
  end;

  TWfcTextValidationIssueKind = (
    wctvikNone,
    wctvikLength,
    wctvikExtent,
    wctvikBoundary,
    wctvikStatePath,
    wctvikProjection,
    wctvikConstraint,
    wctvikDetokenization,
    wctvikRetokenization
  );

  TWfcTextValidationIssue = record
    Kind: TWfcTextValidationIssueKind;
    Position: Integer;
    ConstraintIndex: Integer;
    SequenceIssue: TWfcSequenceGraphIssue;
  end;

  TWfcTextValidationReport = record
    Valid: Boolean;
    CheckedTokens: Integer;
    CheckedConstraints: Integer;
    Issue: TWfcTextValidationIssue;
  end;

  TWfcTextCompletion = record
    Tokenizer: TWfcTextTokenizerKind;
    Text: TWfcModelToken;
    Generated: TWfcGeneratedSequence;
  end;

  TWfcTextCompletionStatus = (
    wctcsNotRun,
    wctcsCompleted,
    wctcsUnsatisfiable,
    wctcsSolveFailed,
    wctcsCaptureFailed,
    wctcsValidationFailed
  );

  TWfcTextCompletionReport = record
    Status: TWfcTextCompletionStatus;
    Analysis: TWfcSequenceDomainAnalysis;
    Solve: TGraphSolveReport;
    Capture: TWfcSequenceGraphValidationReport;
    Validation: TWfcTextValidationReport;
  end;

function MakeWfcTextLockedSpan(const AStart: Integer;
  const ATokens: TWfcModelTokens): TWfcTextLockedSpan;

function DefaultWfcTextCompletionRequest(const ATokenLength: Integer;
  const AExtent: TWfcSequenceExtent; const ASeed: TGraphSeed):
  TWfcTextCompletionRequest;

function BuildWfcTextCompletionConstraints(
  const ARequest: TWfcTextCompletionRequest):
  TWfcSequenceTokenConstraints;

function ValidateWfcTextCompletion(const AModel: TWfcSequenceModel;
  const ARequest: TWfcTextCompletionRequest;
  const ACompletion: TWfcTextCompletion;
  out AReport: TWfcTextValidationReport): Boolean;

function TryCompleteWfcText(const AModel: TWfcSequenceModel;
  const ARequest: TWfcTextCompletionRequest;
  out ACompletion: TWfcTextCompletion;
  out AReport: TWfcTextCompletionReport): Boolean;

function DescribeWfcTextValidationIssue(
  const AIssue: TWfcTextValidationIssue): String;

implementation

function CheckedCompletionManagedLength(const ALength: SizeInt;
  const ALabel: String): Integer;
begin
  if (ALength < 0) or
      ((ALength and (not SizeInt(High(Integer)))) <> 0) then
    raise ERangeError.Create(ALabel + ' exceeds the Integer range');
  Result := Integer(ALength);
end;

function SequenceExtentIsValid(
  const AExtent: TWfcSequenceExtent): Boolean;
var
  LOrdinal: Integer;
begin
  LOrdinal := Ord(AExtent);
  Result := (LOrdinal >= Ord(Low(TWfcSequenceExtent))) and
    (LOrdinal <= Ord(High(TWfcSequenceExtent)));
end;

procedure ValidateRequestShape(
  const ARequest: TWfcTextCompletionRequest);
var
  I: Integer;
  LDomainCount: Integer;
  LPrefixCount: Integer;
  LSpanCount: Integer;
  LSpanTokenCount: Integer;
  LSuffixCount: Integer;
begin
  if ARequest.TokenLength < 1 then
    raise ERangeError.CreateFmt(
      'text completion length must be positive [%d]',
      [ARequest.TokenLength]);
  if not SequenceExtentIsValid(ARequest.Extent) then
    raise EArgumentException.CreateFmt(
      'text completion extent is invalid [%d]', [Ord(ARequest.Extent)]);
  { This validates the currently implemented tokenizer without relying on a
    host enum range check. }
  WfcTextTokenizerName(ARequest.Tokenizer);
  if ARequest.SolveOptions.MaxBacktracks < 0 then
    raise ERangeError.CreateFmt(
      'text completion maximum backtracks cannot be negative [%d]',
      [ARequest.SolveOptions.MaxBacktracks]);

  LPrefixCount := CheckedCompletionManagedLength(
    Length(ARequest.Prefix), 'text completion prefix token count');
  LSuffixCount := CheckedCompletionManagedLength(
    Length(ARequest.Suffix), 'text completion suffix token count');
  LSpanCount := CheckedCompletionManagedLength(
    Length(ARequest.LockedSpans), 'text completion locked-span count');
  LDomainCount := CheckedCompletionManagedLength(
    Length(ARequest.Domains), 'text completion domain count');
  if LPrefixCount > ARequest.TokenLength then
    raise ERangeError.Create('text completion prefix is too long');
  if LSuffixCount > ARequest.TokenLength then
    raise ERangeError.Create('text completion suffix is too long');
  for I := 0 to LSpanCount - 1 do
  begin
    LSpanTokenCount := CheckedCompletionManagedLength(
      Length(ARequest.LockedSpans[I].Tokens),
      Format('text completion locked span %d token count', [I]));
    if ARequest.LockedSpans[I].Start < 0 then
      raise ERangeError.CreateFmt(
        'text completion locked span is out of bounds [%d]', [I]);
    if (ARequest.LockedSpans[I].Start > ARequest.TokenLength) or
        (LSpanTokenCount > ARequest.TokenLength -
          ARequest.LockedSpans[I].Start) then
      raise ERangeError.CreateFmt(
        'text completion locked span is out of bounds [%d]', [I]);
  end;
  for I := 0 to LDomainCount - 1 do
  begin
    if (ARequest.Domains[I].Position < 0) or
        (ARequest.Domains[I].Position >= ARequest.TokenLength) then
      raise ERangeError.CreateFmt(
        'text completion domain is out of bounds [%d]', [I]);
  end;
end;

procedure ValidateModelTokenizerCompatibility(
  const AModel: TWfcSequenceModel;
  const AKind: TWfcTextTokenizerKind);
var
  I: Integer;
  LToken: TWfcModelTokens;
begin
  SetLength(LToken, 1);
  for I := 0 to AModel.PublicTokenCount - 1 do
  begin
    LToken[0] := AModel.PublicTokenAt(I);
    try
      DetokenizeWfcText(LToken, AKind);
    except
      on E: EWfcTextTokenize do
        raise EWfcTextComplete.CreateFmt(
          'text model token %d is incompatible with tokenizer %s',
          [I, WfcTextTokenizerName(AKind)]);
    end;
  end;
end;

function CheckedConstraintCount(const ARequest: TWfcTextCompletionRequest):
  Integer;
var
  I: Integer;
  LAdd: Integer;
begin
  Result := 0;
  LAdd := CheckedCompletionManagedLength(Length(ARequest.Prefix),
    'text completion prefix token count');
  if LAdd > High(Integer) - Result then
    raise ERangeError.Create('too many text completion constraints');
  Inc(Result, LAdd);
  LAdd := CheckedCompletionManagedLength(Length(ARequest.Suffix),
    'text completion suffix token count');
  if LAdd > High(Integer) - Result then
    raise ERangeError.Create('too many text completion constraints');
  Inc(Result, LAdd);
  for I := 0 to CheckedCompletionManagedLength(
      Length(ARequest.LockedSpans),
      'text completion locked-span count') - 1 do
  begin
    LAdd := CheckedCompletionManagedLength(
      Length(ARequest.LockedSpans[I].Tokens),
      Format('text completion locked span %d token count', [I]));
    if LAdd > High(Integer) - Result then
      raise ERangeError.Create('too many text completion constraints');
    Inc(Result, LAdd);
  end;
  LAdd := CheckedCompletionManagedLength(Length(ARequest.Domains),
    'text completion domain count');
  if LAdd > High(Integer) - Result then
    raise ERangeError.Create('too many text completion constraints');
  Inc(Result, LAdd);
end;

procedure SetExactConstraint(var AConstraint: TWfcSequenceTokenConstraint;
  const APosition: Integer; const AToken: TWfcModelToken);
begin
  AConstraint.Position := APosition;
  AConstraint.AllowedTokens := nil;
  SetLength(AConstraint.AllowedTokens, 1);
  AConstraint.AllowedTokens[0] := AToken;
end;

function MakeWfcTextLockedSpan(const AStart: Integer;
  const ATokens: TWfcModelTokens): TWfcTextLockedSpan;
var
  I: Integer;
  LTokenCount: Integer;
begin
  LTokenCount := CheckedCompletionManagedLength(Length(ATokens),
    'text completion locked-span token count');
  Result.Start := AStart;
  Result.Tokens := nil;
  SetLength(Result.Tokens, LTokenCount);
  for I := 0 to LTokenCount - 1 do
    Result.Tokens[I] := ATokens[I];
end;

function DefaultWfcTextCompletionRequest(const ATokenLength: Integer;
  const AExtent: TWfcSequenceExtent; const ASeed: TGraphSeed):
  TWfcTextCompletionRequest;
begin
  Result := Default(TWfcTextCompletionRequest);
  Result.TokenLength := ATokenLength;
  Result.Extent := AExtent;
  Result.Seed := ASeed;
  Result.Tokenizer := wttkUnicodeScalar;
  Result.SolveOptions := DefaultGraphSolveOptions;
end;

function BuildWfcTextCompletionConstraints(
  const ARequest: TWfcTextCompletionRequest):
  TWfcSequenceTokenConstraints;
var
  I: Integer;
  J: Integer;
  LCount: Integer;
  LWrite: Integer;
begin
  Result := nil;
  ValidateRequestShape(ARequest);
  LCount := CheckedConstraintCount(ARequest);
  SetLength(Result, LCount);
  LWrite := 0;

  for I := 0 to Length(ARequest.Prefix) - 1 do
  begin
    SetExactConstraint(Result[LWrite], I, ARequest.Prefix[I]);
    Inc(LWrite);
  end;
  for I := 0 to Length(ARequest.Suffix) - 1 do
  begin
    SetExactConstraint(Result[LWrite],
      ARequest.TokenLength - Length(ARequest.Suffix) + I,
      ARequest.Suffix[I]);
    Inc(LWrite);
  end;
  for I := 0 to Length(ARequest.LockedSpans) - 1 do
    for J := 0 to Length(ARequest.LockedSpans[I].Tokens) - 1 do
    begin
      SetExactConstraint(Result[LWrite],
        ARequest.LockedSpans[I].Start + J,
        ARequest.LockedSpans[I].Tokens[J]);
      Inc(LWrite);
    end;
  for I := 0 to Length(ARequest.Domains) - 1 do
  begin
    Result[LWrite] := MakeWfcSequenceTokenConstraint(
      ARequest.Domains[I].Position,
      ARequest.Domains[I].AllowedTokens);
    Inc(LWrite);
  end;
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

function ContainsToken(const ATokens: TWfcModelTokens;
  const AToken: TWfcModelToken): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(ATokens) - 1 do
    if ATokens[I] = AToken then
      Exit(True);
  Result := False;
end;

procedure InitializeValidationReport(
  out AReport: TWfcTextValidationReport);
begin
  AReport := Default(TWfcTextValidationReport);
  AReport.Issue.Position := -1;
  AReport.Issue.ConstraintIndex := -1;
  AReport.Issue.SequenceIssue.Position := -1;
  AReport.Issue.SequenceIssue.RelatedPosition := -1;
  AReport.Issue.SequenceIssue.StateIndex := -1;
  AReport.Issue.SequenceIssue.RelatedStateIndex := -1;
end;

function InvalidValidation(var AReport: TWfcTextValidationReport;
  const AKind: TWfcTextValidationIssueKind): Boolean;
begin
  AReport.Valid := False;
  AReport.Issue.Kind := AKind;
  Result := False;
end;

function ValidateWfcTextCompletion(const AModel: TWfcSequenceModel;
  const ARequest: TWfcTextCompletionRequest;
  const ACompletion: TWfcTextCompletion;
  out AReport: TWfcTextValidationReport): Boolean;
var
  I: Integer;
  LConstraints: TWfcSequenceTokenConstraints;
  LExpectedBoundary: TWfcModelBoundary;
  LProjected: TWfcModelTokens;
  LRetokenized: TWfcModelTokens;
  LSequenceReport: TWfcSequenceGraphValidationReport;
  LText: TWfcModelToken;
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create('text completion model cannot be nil');
  LConstraints := BuildWfcTextCompletionConstraints(ARequest);
  ValidateModelTokenizerCompatibility(AModel, ARequest.Tokenizer);
  ValidateSequenceTokenConstraints(AModel, ARequest.TokenLength,
    LConstraints);
  InitializeValidationReport(AReport);

  if (Length(ACompletion.Generated.StateIndices) <>
      ARequest.TokenLength) or
      (Length(ACompletion.Generated.Tokens) <>
      ARequest.TokenLength) then
    Exit(InvalidValidation(AReport, wctvikLength));
  if ACompletion.Generated.Extent <> ARequest.Extent then
    Exit(InvalidValidation(AReport, wctvikExtent));
  if ARequest.Extent = wseWrap then
    LExpectedBoundary := wmbWrap
  else
    LExpectedBoundary := wmbOpen;
  if ACompletion.Generated.Boundary <> LExpectedBoundary then
    Exit(InvalidValidation(AReport, wctvikBoundary));

  if not ValidateSequenceStatePath(AModel,
      ACompletion.Generated.StateIndices, ARequest.Extent,
      LSequenceReport) then
  begin
    AReport.Issue.SequenceIssue := LSequenceReport.Issue;
    AReport.Issue.Position := LSequenceReport.Issue.Position;
    Exit(InvalidValidation(AReport, wctvikStatePath));
  end;
  LProjected := AModel.ProjectStateIndices(
    ACompletion.Generated.StateIndices);
  if not TokensEqual(LProjected, ACompletion.Generated.Tokens) then
    Exit(InvalidValidation(AReport, wctvikProjection));
  AReport.CheckedTokens := Length(LProjected);

  for I := 0 to Length(LConstraints) - 1 do
  begin
    if not ContainsToken(LConstraints[I].AllowedTokens,
        LProjected[LConstraints[I].Position]) then
    begin
      AReport.Issue.Position := LConstraints[I].Position;
      AReport.Issue.ConstraintIndex := I;
      Exit(InvalidValidation(AReport, wctvikConstraint));
    end;
    Inc(AReport.CheckedConstraints);
  end;

  try
    LText := DetokenizeWfcText(LProjected, ARequest.Tokenizer);
  except
    on E: EWfcTextTokenize do
      Exit(InvalidValidation(AReport, wctvikDetokenization));
  end;
  if (ACompletion.Tokenizer <> ARequest.Tokenizer) or
      (LText <> ACompletion.Text) then
    Exit(InvalidValidation(AReport, wctvikDetokenization));
  try
    LRetokenized := TokenizeWfcText(ACompletion.Text,
      ARequest.Tokenizer);
  except
    on E: EWfcTextTokenize do
      Exit(InvalidValidation(AReport, wctvikRetokenization));
  end;
  if not TokensEqual(LRetokenized, LProjected) then
    Exit(InvalidValidation(AReport, wctvikRetokenization));

  AReport.Valid := True;
  AReport.Issue.Kind := wctvikNone;
  Result := True;
end;

function TryCompleteWfcText(const AModel: TWfcSequenceModel;
  const ARequest: TWfcTextCompletionRequest;
  out ACompletion: TWfcTextCompletion;
  out AReport: TWfcTextCompletionReport): Boolean;
var
  LCompletion: TWfcTextCompletion;
  LConstraints: TWfcSequenceTokenConstraints;
  LGraph: TGraph;
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create('text completion model cannot be nil');
  ACompletion := Default(TWfcTextCompletion);
  AReport := Default(TWfcTextCompletionReport);
  LCompletion := Default(TWfcTextCompletion);
  LConstraints := BuildWfcTextCompletionConstraints(ARequest);
  ValidateModelTokenizerCompatibility(AModel, ARequest.Tokenizer);
  ValidateSequenceTokenConstraints(AModel, ARequest.TokenLength,
    LConstraints);

  if not AnalyzeSequenceTokenDomains(AModel, ARequest.TokenLength,
      ARequest.Extent, LConstraints, AReport.Analysis) then
  begin
    AReport.Status := wctcsUnsatisfiable;
    Exit(False);
  end;

  LGraph := TGraph.Create;
  try
    LGraph.Reshape(TGraphCoordinate(ARequest.TokenLength), 1, 1);
    LGraph.WrapNeighbors := ARequest.Extent = wseWrap;
    LGraph.Seed := ARequest.Seed;
    ApplySequenceModelToGraph(AModel, LGraph, ARequest.Extent);
    IntersectSequenceTokenConstraints(AModel, LGraph, LConstraints);
    if not LGraph.TrySolve(ARequest.SolveOptions, AReport.Solve) then
    begin
      AReport.Status := wctcsSolveFailed;
      Exit(False);
    end;
    if not CaptureSolvedSequence(AModel, LGraph, ARequest.Extent,
        LCompletion.Generated, AReport.Capture) then
    begin
      AReport.Status := wctcsCaptureFailed;
      Exit(False);
    end;
    LCompletion.Tokenizer := ARequest.Tokenizer;
    LCompletion.Text := DetokenizeWfcText(
      LCompletion.Generated.Tokens, ARequest.Tokenizer);
    if not ValidateWfcTextCompletion(AModel, ARequest,
        LCompletion, AReport.Validation) then
    begin
      AReport.Status := wctcsValidationFailed;
      Exit(False);
    end;
    ACompletion := LCompletion;
    AReport.Status := wctcsCompleted;
    Result := True;
  finally
    LGraph.Free;
  end;
end;

function DescribeWfcTextValidationIssue(
  const AIssue: TWfcTextValidationIssue): String;
begin
  Result := 'unknown WFC text validation issue';
  case AIssue.Kind of
    wctvikNone:
      Result := 'no WFC text validation issue';
    wctvikLength:
      Result := 'completion token and state lengths do not match the request';
    wctvikExtent:
      Result := 'completion extent does not match the request';
    wctvikBoundary:
      Result := 'completion boundary does not match its extent';
    wctvikStatePath:
      Result := DescribeSequenceGraphIssue(AIssue.SequenceIssue);
    wctvikProjection:
      Result := 'completion tokens do not match the latent-state projection';
    wctvikConstraint:
      Result := Format(
        'completion position %d violates constraint %d',
        [AIssue.Position, AIssue.ConstraintIndex]);
    wctvikDetokenization:
      Result := 'completion text is not the exact token detokenization';
    wctvikRetokenization:
      Result := 'completion text does not retokenize identically';
  end;
end;

end.
