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
unit wfc_pipeline_result_text;

{$mode delphi}{$H+}

interface

uses
  wfc_pipeline_model,
  wfc_pipeline_run,
  wfc_pipeline_result;

const
  WFC_PIPELINE_RESULT_TEXT_VERSION = 2;
  WFC_PIPELINE_RESULT_MAX_ENCODED_TEXT_LENGTH = 268435456;
  WFC_PIPELINE_RESULT_MAX_TEXT_LINE_COUNT = 37 +
    2 * WFC_PIPELINE_MAX_PASS_COUNT +
    WFC_PIPELINE_RESULT_MAX_PUBLIC_LAYER_COUNT +
    WFC_PIPELINE_RESULT_MAX_TOTAL_PUBLIC_CELL_COUNT;

function EncodeWfcPipelineResultText(
  const AResult: TWfcPipelineResult): String;
function DecodeWfcPipelineResultText(const AText: String;
  const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun): TWfcPipelineResult;

implementation

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_lattice,
  wfc_pipeline_layout,
  wfc_text_codec;

const
  WFC_PIPELINE_RESULT_TEXT_ARTIFACT = 'WFC pipeline result';
  WFC_PIPELINE_RESULT_FIXED_LINE_COUNT = 34;

type
  TWfcPipelineResultTextFields = array of String;

procedure TextError(const AMessage: String);
begin
  WfcTextError(WFC_PIPELINE_RESULT_TEXT_ARTIFACT, AMessage);
end;

function ParseCanonicalInteger(const AText,
  AFieldName: String): Integer;
begin
  Result := WfcTextParseCanonicalInteger(AText, AFieldName,
    WFC_PIPELINE_RESULT_TEXT_ARTIFACT);
end;

function ParseCanonicalSignedInteger(const AText,
  AFieldName: String): Integer;
begin
  Result := WfcTextParseCanonicalSignedInteger(AText, AFieldName,
    WFC_PIPELINE_RESULT_TEXT_ARTIFACT);
end;

function ParseCanonicalCardinal(const AText,
  AFieldName: String): Cardinal;
begin
  Result := WfcTextParseCanonicalCardinal(AText, AFieldName,
    WFC_PIPELINE_RESULT_TEXT_ARTIFACT);
end;

function ValueAfterPrefix(const ALine, APrefix,
  AFieldName: String): String;
begin
  Result := WfcTextValueAfterPrefix(ALine, APrefix, AFieldName,
    WFC_PIPELINE_RESULT_TEXT_ARTIFACT);
end;

function RequireLine(const ALines: TWfcTextLines;
  const AIndex: Integer; const AFieldName: String): String;
begin
  if (AIndex < 0) or (AIndex >= Length(ALines)) then
    TextError(AFieldName + ' is missing');
  Result := ALines[AIndex];
end;

function ReadValueLine(const ALines: TWfcTextLines;
  var ALineIndex: Integer; const APrefix,
  AFieldName: String): String;
begin
  Result := ValueAfterPrefix(RequireLine(ALines, ALineIndex,
    AFieldName), APrefix, AFieldName);
  Inc(ALineIndex);
end;

procedure AppendLine(var ALines: TWfcTextLines;
  var ALineCount: Integer; const ALine: String);
begin
  if (ALineCount < 0) or (ALineCount >= Length(ALines)) then
    raise ERangeError.Create(
      'canonical WFC pipeline result line capacity was exceeded');
  ALines[ALineCount] := ALine;
  Inc(ALineCount);
end;

procedure AddLineCapacity(var ACount: Integer;
  const AAdditional: Integer);
begin
  if AAdditional < 0 then
    raise EArgumentOutOfRangeException.Create(
      'canonical WFC pipeline result line addition cannot be negative');
  if ACount > WFC_PIPELINE_RESULT_MAX_TEXT_LINE_COUNT - AAdditional then
    raise ERangeError.Create(
      'canonical WFC pipeline result text has too many lines');
  Inc(ACount, AAdditional);
end;

procedure RequireEncodedTextLength(const ALines: TWfcTextLines);
var
  I: Integer;
  LLineLength: SizeInt;
  LTotalLength: Integer;
begin
  LTotalLength := 0;
  for I := 0 to Length(ALines) - 1 do
  begin
    LLineLength := Length(ALines[I]);
    if LLineLength > SizeInt(
        WFC_PIPELINE_RESULT_MAX_ENCODED_TEXT_LENGTH - LTotalLength - 1) then
      raise ERangeError.Create(
        'canonical WFC pipeline result exceeds the version-1 length limit');
    Inc(LTotalLength, Integer(LLineLength) + 1);
  end;
end;

procedure PreflightTextEnvelope(const AText: String);
var
  I: SizeInt;
  LLineCount: Integer;
  LTextLength: SizeInt;
begin
  LTextLength := Length(AText);
  if (LTextLength < 0) or
      (LTextLength >
      SizeInt(WFC_PIPELINE_RESULT_MAX_ENCODED_TEXT_LENGTH)) then
    TextError('document exceeds the version-1 encoded length limit');
  LLineCount := 0;
  for I := 1 to LTextLength do
    if AText[I] = #10 then
    begin
      if LLineCount = WFC_PIPELINE_RESULT_MAX_TEXT_LINE_COUNT then
        TextError('document exceeds the version-1 line-count limit');
      Inc(LLineCount);
    end;
end;

function SplitRecord(const AText: String;
  const AFieldCount: Integer; const ARecordName: String):
  TWfcPipelineResultTextFields;
var
  I: Integer;
  LComma: Integer;
  LStart: Integer;
begin
  Result := nil;
  if AFieldCount < 1 then
    raise EArgumentOutOfRangeException.Create(
      'pipeline result record field count must be positive');
  SetLength(Result, AFieldCount);
  LStart := 1;
  for I := 0 to AFieldCount - 2 do
  begin
    LComma := WfcTextFindCharacter(AText, ',', LStart);
    if LComma = 0 then
      TextError(ARecordName + ' record is missing fields');
    Result[I] := Copy(AText, LStart, LComma - LStart);
    LStart := LComma + 1;
  end;
  if WfcTextFindCharacter(AText, ',', LStart) <> 0 then
    TextError(ARecordName + ' record has extra fields');
  Result[AFieldCount - 1] := Copy(AText, LStart,
    Length(AText) - LStart + 1);
end;

function ParseBoundedCount(const AText, AFieldName: String;
  const AMaximum: Integer): Integer;
begin
  Result := ParseCanonicalInteger(AText, AFieldName);
  if Result > AMaximum then
    TextError(AFieldName + ' exceeds the version-1 limit');
end;

procedure RequireRecordCapacity(const ACount, ALineIndex,
  AReservedLines: Integer; const ALines: TWfcTextLines;
  const ARecordName: String);
begin
  if (ACount < 0) or (ALineIndex < 0) or (AReservedLines < 0) or
      (ALineIndex > Length(ALines)) or
      (AReservedLines > Length(ALines) - ALineIndex) or
      (ACount > Length(ALines) - ALineIndex - AReservedLines) then
    TextError(ARecordName + ' records are incomplete');
end;

function EncodeToken(const AValue: TWfcModelToken): String;
begin
  Result := WfcTextEncodeToken(AValue,
    WFC_PIPELINE_RESULT_TEXT_ARTIFACT);
end;

function BooleanName(const AValue: Boolean): String;
begin
  if AValue then
    Result := 'true'
  else
    Result := 'false';
end;

function ParseBooleanName(const AText,
  AFieldName: String): Boolean;
begin
  if AText = 'false' then
    Result := False
  else if AText = 'true' then
    Result := True
  else
  begin
    TextError(AFieldName + ' must be true or false');
    Result := False;
  end;
end;

function StrategyName(const AValue: TWfcPipelineSolveStrategy): String;
begin
  case AValue of
    wpssOneWay: Result := 'one-way';
    wpssNegotiated: Result := 'negotiated';
  end;
end;

function ParseStrategy(const AText: String): TWfcPipelineSolveStrategy;
begin
  if AText = 'one-way' then
    Result := wpssOneWay
  else if AText = 'negotiated' then
    Result := wpssNegotiated
  else
  begin
    TextError('strategy is unknown');
    Result := wpssOneWay;
  end;
end;

function StatusName(const AValue: TWfcPipelineResultStatus): String;
begin
  case AValue of
    wprsSolved: Result := 'solved';
    wprsContradiction: Result := 'contradiction';
    wprsSolverBacktrackLimit: Result := 'solver-backtrack-limit';
    wprsPassBacktrackLimit: Result := 'pass-backtrack-limit';
  end;
end;

function ParseStatus(const AText: String): TWfcPipelineResultStatus;
begin
  if AText = 'solved' then
    Result := wprsSolved
  else if AText = 'contradiction' then
    Result := wprsContradiction
  else if AText = 'solver-backtrack-limit' then
    Result := wprsSolverBacktrackLimit
  else if AText = 'pass-backtrack-limit' then
    Result := wprsPassBacktrackLimit
  else
  begin
    TextError('result status is unknown');
    Result := wprsContradiction;
  end;
end;

function EvidenceName(const AValue: TWfcPipelineEvidenceKind): String;
begin
  case AValue of
    wpekNone: Result := 'none';
    wpekTrace: Result := 'trace';
    wpekNegotiationTranscript: Result := 'negotiation-transcript';
  end;
end;

function ParseEvidence(const AText: String): TWfcPipelineEvidenceKind;
begin
  if AText = 'none' then
    Result := wpekNone
  else if AText = 'trace' then
    Result := wpekTrace
  else if AText = 'negotiation-transcript' then
    Result := wpekNegotiationTranscript
  else
  begin
    TextError('result evidence kind is unknown');
    Result := wpekNone;
  end;
end;

function DispositionName(const AValue: TGraphPassDisposition): String;
begin
  case AValue of
    gpdNotRun: Result := 'not-run';
    gpdReused: Result := 'reused';
    gpdCleared: Result := 'cleared';
    gpdCopied: Result := 'copied';
    gpdSolved: Result := 'solved';
    gpdFailed: Result := 'failed';
  end;
end;

function ParseDisposition(const AText: String): TGraphPassDisposition;
begin
  if AText = 'not-run' then
    Result := gpdNotRun
  else if AText = 'reused' then
    Result := gpdReused
  else if AText = 'cleared' then
    Result := gpdCleared
  else if AText = 'copied' then
    Result := gpdCopied
  else if AText = 'solved' then
    Result := gpdSolved
  else if AText = 'failed' then
    Result := gpdFailed
  else
  begin
    TextError('pass disposition is unknown');
    Result := gpdNotRun;
  end;
end;

function ContradictionName(const AValue: TGraphContradictionKind): String;
begin
  case AValue of
    gckNone: Result := 'none';
    gckEmptyDomain: Result := 'empty-domain';
    gckInvalidLock: Result := 'invalid-lock';
    gckAdjacency: Result := 'adjacency';
    gckPreviousPass: Result := 'previous-pass';
    gckRequiredSupport: Result := 'required-support';
    gckFinalValidation: Result := 'final-validation';
    gckPassDependency: Result := 'pass-dependency';
    gckEntryDomain: Result := 'entry-domain';
    gckExcludedAssignment: Result := 'excluded-assignment';
    gckConnectivity: Result := 'connectivity';
    gckValueQuota: Result := 'value-quota';
  else
    TextError('failure contradiction kind is unknown');
  end;
end;

function ParseContradiction(const AText: String): TGraphContradictionKind;
begin
  if AText = 'none' then
    Result := gckNone
  else if AText = 'empty-domain' then
    Result := gckEmptyDomain
  else if AText = 'invalid-lock' then
    Result := gckInvalidLock
  else if AText = 'adjacency' then
    Result := gckAdjacency
  else if AText = 'previous-pass' then
    Result := gckPreviousPass
  else if AText = 'required-support' then
    Result := gckRequiredSupport
  else if AText = 'final-validation' then
    Result := gckFinalValidation
  else if AText = 'pass-dependency' then
    Result := gckPassDependency
  else if AText = 'entry-domain' then
    Result := gckEntryDomain
  else if AText = 'excluded-assignment' then
    Result := gckExcludedAssignment
  else if AText = 'connectivity' then
    Result := gckConnectivity
  else if AText = 'value-quota' then
    Result := gckValueQuota
  else
  begin
    TextError('failure contradiction kind is unknown');
    Result := gckNone;
  end;
end;

function DirectionName(const AValue: TGraphDirection): String;
begin
  case AValue of
    gdNorth: Result := 'N';
    gdEast: Result := 'E';
    gdSouth: Result := 'S';
    gdWest: Result := 'W';
    gdUp: Result := 'U';
    gdDown: Result := 'D';
  end;
end;

function ParseDirection(const AText: String): TGraphDirection;
begin
  if AText = 'N' then
    Result := gdNorth
  else if AText = 'E' then
    Result := gdEast
  else if AText = 'S' then
    Result := gdSouth
  else if AText = 'W' then
    Result := gdWest
  else if AText = 'U' then
    Result := gdUp
  else if AText = 'D' then
    Result := gdDown
  else
  begin
    TextError('failure direction is unknown');
    Result := gdNorth;
  end;
end;

function SignatureTextIsCanonical(const AText: String): Boolean;
var
  I: Integer;
begin
  if Length(AText) <> 8 then
    Exit(False);
  for I := 1 to 8 do
    if not (AText[I] in ['0'..'9', 'A'..'F']) then
      Exit(False);
  Result := True;
end;

function ParseSignature(const AText, AFieldName: String): Cardinal;
var
  I: Integer;
  LDigit: Cardinal;
begin
  if not SignatureTextIsCanonical(AText) then
    TextError(AFieldName +
      ' must be eight uppercase hexadecimal digits');
  Result := 0;
  for I := 1 to 8 do
  begin
    if AText[I] in ['0'..'9'] then
      LDigit := Cardinal(Ord(AText[I]) - Ord('0'))
    else
      LDigit := Cardinal(Ord(AText[I]) - Ord('A') + 10);
    Result := (Result shl 4) or LDigit;
  end;
end;

function EncodeWfcPipelineResultText(
  const AResult: TWfcPipelineResult): String;
var
  I: Integer;
  J: Integer;
  LExpectedLineCount: Integer;
  LFailure: TWfcPipelineFailure;
  LLayer: TWfcPipelineResultLayer;
  LLineCount: Integer;
  LLines: TWfcTextLines;
  LOutcome: TWfcPipelinePassOutcome;
  LVersions: TWfcPipelineResultVersions;
  LLayout: TWfcLatticeLayout;
begin
  if not Assigned(AResult) then
    raise EArgumentNilException.Create('pipeline result cannot be nil');
  LExpectedLineCount := WFC_PIPELINE_RESULT_FIXED_LINE_COUNT;
  if AResult.FormatVersion = 2 then
    AddLineCapacity(LExpectedLineCount, 3 + AResult.PassCount);
  AddLineCapacity(LExpectedLineCount, AResult.PassOutcomeCount);
  AddLineCapacity(LExpectedLineCount, AResult.LayerCount);
  for I := 0 to AResult.LayerCount - 1 do
    AddLineCapacity(LExpectedLineCount,
      Length(AResult.LayerAt(I).Tokens));
  SetLength(LLines, LExpectedLineCount);
  LLineCount := 0;
  LVersions := AResult.CopyVersions;
  LFailure := AResult.CopyFailure;

  AppendLine(LLines, LLineCount, 'wfcpipeline-result=' +
    IntToStr(AResult.FormatVersion));
  AppendLine(LLines, LLineCount, 'recipe-signature=' +
    WfcPipelineSignatureHex(AResult.RecipeSignature));
  AppendLine(LLines, LLineCount, 'run-signature=' +
    WfcPipelineRunSignatureHex(AResult.RunSignature));
  AppendLine(LLines, LLineCount, 'graph-model-version=' +
    IntToStr(LVersions.GraphModelVersion));
  AppendLine(LLines, LLineCount, 'random-algorithm-version=' +
    IntToStr(LVersions.RandomAlgorithmVersion));
  AppendLine(LLines, LLineCount, 'solver-algorithm-version=' +
    IntToStr(LVersions.SolverAlgorithmVersion));
  AppendLine(LLines, LLineCount, 'pipeline-algorithm-version=' +
    IntToStr(LVersions.PipelineAlgorithmVersion));
  AppendLine(LLines, LLineCount, 'trace-version=' +
    IntToStr(LVersions.TraceVersion));
  AppendLine(LLines, LLineCount, 'trace-hash-version=' +
    IntToStr(LVersions.TraceHashVersion));
  AppendLine(LLines, LLineCount, 'negotiation-algorithm-version=' +
    IntToStr(LVersions.NegotiationAlgorithmVersion));
  AppendLine(LLines, LLineCount, 'negotiation-hash-version=' +
    IntToStr(LVersions.NegotiationHashVersion));
  AppendLine(LLines, LLineCount, 'width=' + IntToStr(AResult.Width));
  AppendLine(LLines, LLineCount, 'height=' + IntToStr(AResult.Height));
  AppendLine(LLines, LLineCount, 'depth=' + IntToStr(AResult.Depth));
  if AResult.FormatVersion = 2 then
  begin
    AppendLine(LLines, LLineCount, 'layout-version=' + IntToStr(WFC_PIPELINE_LAYOUT_VERSION));
    AppendLine(LLines, LLineCount, 'mapping-version=' + IntToStr(WFC_PASS_MAPPING_VERSION));
    AppendLine(LLines, LLineCount, 'layouts=' + IntToStr(AResult.PassCount));
    for I := 0 to AResult.PassCount - 1 do
    begin
      LLayout := AResult.PassLayoutAt(I);
      AppendLine(LLines, LLineCount, 'layout=' + IntToStr(I) + ',' +
        IntToStr(AResult.PassTopologyAt(I).Rank) + ',' +
        IntToStr(LLayout.Origin.X) + ',' + IntToStr(LLayout.Origin.Y) + ',' +
        IntToStr(LLayout.Origin.Z) + ',' + IntToStr(LLayout.Pitch.X) + ',' +
        IntToStr(LLayout.Pitch.Y) + ',' + IntToStr(LLayout.Pitch.Z) + ',' +
        IntToStr(LLayout.Cells.X) + ',' + IntToStr(LLayout.Cells.Y) + ',' +
        IntToStr(LLayout.Cells.Z) + ',' + BooleanName(LLayout.Wrap));
    end;
  end;
  AppendLine(LLines, LLineCount, 'seed=' + UIntToStr(AResult.Seed));
  AppendLine(LLines, LLineCount, 'strategy=' +
    StrategyName(AResult.Strategy));
  AppendLine(LLines, LLineCount, 'max-backtracks=' +
    IntToStr(AResult.MaxBacktracks));
  AppendLine(LLines, LLineCount, 'max-pass-backtracks=' +
    IntToStr(AResult.MaxPassBacktracks));
  AppendLine(LLines, LLineCount, 'trace=' +
    BooleanName(AResult.CaptureTrace));
  AppendLine(LLines, LLineCount, 'status=' + StatusName(AResult.Status));
  AppendLine(LLines, LLineCount, 'pass-backtracks=' +
    IntToStr(AResult.PassBacktracks));
  AppendLine(LLines, LLineCount, 'evidence=' +
    EvidenceName(AResult.EvidenceKind));
  AppendLine(LLines, LLineCount, 'evidence-signature=' +
    IntToHex(AResult.EvidenceSignature, 8));
  AppendLine(LLines, LLineCount, 'failure-kind=' +
    ContradictionName(LFailure.Kind));
  AppendLine(LLines, LLineCount, 'failure-pass=' +
    IntToStr(LFailure.PassIndex));
  AppendLine(LLines, LLineCount, 'failure-entry=' +
    IntToStr(LFailure.EntryIndex));
  AppendLine(LLines, LLineCount, 'failure-neighbor=' +
    IntToStr(LFailure.NeighborIndex));
  AppendLine(LLines, LLineCount, 'failure-direction-present=' +
    BooleanName(LFailure.HasDirection));
  AppendLine(LLines, LLineCount, 'failure-direction=' +
    DirectionName(LFailure.Direction));
  AppendLine(LLines, LLineCount, 'failure-dependency-pass=' +
    IntToStr(LFailure.DependencyPassIndex));

  AppendLine(LLines, LLineCount, 'passes=' +
    IntToStr(AResult.PassOutcomeCount));
  for I := 0 to AResult.PassOutcomeCount - 1 do
  begin
    LOutcome := AResult.PassOutcomeAt(I);
    AppendLine(LLines, LLineCount, 'pass=' + IntToStr(I) + ',' +
      BooleanName(LOutcome.Executed) + ',' +
      IntToStr(LOutcome.ExecutionOrdinal) + ',' +
      DispositionName(LOutcome.Disposition) + ',' +
      IntToStr(LOutcome.Decisions) + ',' +
      IntToStr(LOutcome.Propagations) + ',' +
      IntToStr(LOutcome.Contradictions) + ',' +
      IntToStr(LOutcome.Backtracks) + ',' +
      IntToStr(LOutcome.ExcludedAssignments));
  end;

  AppendLine(LLines, LLineCount, 'layers=' +
    IntToStr(AResult.LayerCount));
  for I := 0 to AResult.LayerCount - 1 do
  begin
    LLayer := AResult.LayerAt(I);
    AppendLine(LLines, LLineCount, 'layer=' + IntToStr(I) + ',' +
      IntToStr(LLayer.PassIndex) + ',' + EncodeToken(LLayer.LabelName) +
      ',' + IntToStr(Length(LLayer.Tokens)));
    for J := 0 to Length(LLayer.Tokens) - 1 do
      AppendLine(LLines, LLineCount, 'value=' + IntToStr(I) + ',' +
        IntToStr(J) + ',' + EncodeToken(LLayer.Tokens[J]));
  end;

  AppendLine(LLines, LLineCount, 'signature=' +
    WfcPipelineResultSignatureHex(AResult.Signature));
  AppendLine(LLines, LLineCount, 'end');
  if LLineCount <> LExpectedLineCount then
    raise Exception.Create(
      'canonical WFC pipeline result line count does not match its preflight');
  RequireEncodedTextLength(LLines);
  Result := WfcTextJoinCanonicalLines(LLines,
    WFC_PIPELINE_RESULT_TEXT_ARTIFACT);
end;

function DecodeWfcPipelineResultText(const AText: String;
  const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun): TWfcPipelineResult;
var
  I: Integer;
  J: Integer;
  LCaptureTrace: Boolean;
  LCellCount: Integer;
  LDepth: Integer;
  LEvidenceKind: TWfcPipelineEvidenceKind;
  LEvidenceSignature: Cardinal;
  LFailure: TWfcPipelineFailure;
  LFields: TWfcPipelineResultTextFields;
  LHeight: Integer;
  LLayerCount: Integer;
  LLayers: TWfcPipelineResultLayers;
  LLineIndex: Integer;
  LLines: TWfcTextLines;
  LMaxBacktracks: Integer;
  LMaxPassBacktracks: Integer;
  LModel: TWfcPipelineResult;
  LOutcomeCount: Integer;
  LOutcomes: TWfcPipelinePassOutcomes;
  LPassBacktracks: Integer;
  LRecipeSignatureText: String;
  LRunSignatureText: String;
  LSeed: Cardinal;
  LSignatureText: String;
  LStatus: TWfcPipelineResultStatus;
  LStrategy: TWfcPipelineSolveStrategy;
  LTokens: TWfcModelTokens;
  LTotalCellCount: Integer;
  LTotalTokenLength: Integer;
  LVersions: TWfcPipelineResultVersions;
  LWidth: Integer;
  LFormatVersion, LLayoutCount, LRank, LExpectedPublicCount, LExpectedPassIndex: Integer;
  LLayout: TWfcLatticeLayout;

  function DecodeResultToken(const ATextValue,
    AFieldName: String): TWfcModelToken;
  var
    LLength: SizeInt;
  begin
    LLength := Length(ATextValue);
    if LLength > SizeInt(WFC_PIPELINE_RESULT_MAX_ENCODED_TOKEN_LENGTH) then
      TextError(AFieldName +
        ' exceeds the version-1 encoded token-length limit');
    if LLength > SizeInt(
        WFC_PIPELINE_RESULT_MAX_TOTAL_ENCODED_TOKEN_LENGTH -
        LTotalTokenLength) then
      TextError(
        'result encoded token bytes exceed the version-1 aggregate limit');
    Inc(LTotalTokenLength, Integer(LLength));
    Result := WfcTextDecodeToken(ATextValue,
      WFC_PIPELINE_RESULT_TEXT_ARTIFACT);
  end;
begin
  Result := nil;
  if not Assigned(ARecipe) then
    TextError('recipe cannot be nil');
  if not Assigned(ARun) then
    TextError('run cannot be nil');
  PreflightTextEnvelope(AText);
  WfcTextSplitCanonicalLines(AText,
    WFC_PIPELINE_RESULT_TEXT_ARTIFACT, LLines);
  if Length(LLines) < WFC_PIPELINE_RESULT_FIXED_LINE_COUNT then
    TextError('document is incomplete');
  LLineIndex := 0;
  if RequireLine(LLines, LLineIndex, 'format version') = 'wfcpipeline-result=1' then
    LFormatVersion := 1
  else if RequireLine(LLines, LLineIndex, 'format version') = 'wfcpipeline-result=2' then
    LFormatVersion := 2
  else
    TextError('unsupported or noncanonical format version');
  if LFormatVersion <> ARun.FormatVersion then
    TextError('result format must match the supplied run format');
  Inc(LLineIndex);

  LRecipeSignatureText := ReadValueLine(LLines, LLineIndex,
    'recipe-signature=', 'recipe signature');
  if not SignatureTextIsCanonical(LRecipeSignatureText) then
    TextError('recipe signature must be eight uppercase hexadecimal digits');
  if LRecipeSignatureText <>
      WfcPipelineSignatureHex(ARecipe.Signature) then
    TextError('recipe signature does not match the supplied recipe');
  LRunSignatureText := ReadValueLine(LLines, LLineIndex,
    'run-signature=', 'run signature');
  if not SignatureTextIsCanonical(LRunSignatureText) then
    TextError('run signature must be eight uppercase hexadecimal digits');
  if LRunSignatureText <> WfcPipelineRunSignatureHex(ARun.Signature) then
    TextError('run signature does not match the supplied run');

  LVersions.GraphModelVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'graph-model-version=',
      'graph-model version'), 'graph-model version');
  LVersions.RandomAlgorithmVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'random-algorithm-version=',
      'random-algorithm version'), 'random-algorithm version');
  LVersions.SolverAlgorithmVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'solver-algorithm-version=',
      'solver-algorithm version'), 'solver-algorithm version');
  LVersions.PipelineAlgorithmVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'pipeline-algorithm-version=',
      'pipeline-algorithm version'), 'pipeline-algorithm version');
  LVersions.TraceVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'trace-version=',
      'trace version'), 'trace version');
  LVersions.TraceHashVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'trace-hash-version=',
      'trace-hash version'), 'trace-hash version');
  LVersions.NegotiationAlgorithmVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'negotiation-algorithm-version=',
      'negotiation-algorithm version'), 'negotiation-algorithm version');
  LVersions.NegotiationHashVersion := ParseCanonicalInteger(
    ReadValueLine(LLines, LLineIndex, 'negotiation-hash-version=',
      'negotiation-hash version'), 'negotiation-hash version');

  LWidth := ParseCanonicalInteger(ReadValueLine(LLines, LLineIndex,
    'width=', 'width'), 'width');
  LHeight := ParseCanonicalInteger(ReadValueLine(LLines, LLineIndex,
    'height=', 'height'), 'height');
  LDepth := ParseCanonicalInteger(ReadValueLine(LLines, LLineIndex,
    'depth=', 'depth'), 'depth');
  if (LWidth <> ARun.Width) or (LHeight <> ARun.Height) or
      (LDepth <> ARun.Depth) then
    TextError('embedded pass-zero extent does not match the supplied run');
  if LFormatVersion = 2 then
  begin
    if ParseCanonicalInteger(ReadValueLine(LLines, LLineIndex,
        'layout-version=', 'layout version'), 'layout version') <>
        WFC_PIPELINE_LAYOUT_VERSION then
      TextError('unsupported layout version');
    if ParseCanonicalInteger(ReadValueLine(LLines, LLineIndex,
        'mapping-version=', 'mapping version'), 'mapping version') <>
        WFC_PASS_MAPPING_VERSION then
      TextError('unsupported mapping version');
    LLayoutCount := ParseBoundedCount(ReadValueLine(LLines, LLineIndex,
      'layouts=', 'layout count'), 'layout count', WFC_PIPELINE_MAX_PASS_COUNT);
    if LLayoutCount <> ARun.PassCount then
      TextError('result layout table must cover every run pass');
    RequireRecordCapacity(LLayoutCount, LLineIndex, 20, LLines, 'layout');
    for I := 0 to LLayoutCount - 1 do
    begin
      LFields := SplitRecord(ReadValueLine(LLines, LLineIndex,
        'layout=', 'layout record'), 12, 'layout');
      if ParseCanonicalInteger(LFields[0], 'layout pass index') <> I then
        TextError('layout indices must be complete and ordered');
      LRank := ParseCanonicalInteger(LFields[1], 'layout rank');
      try
        LLayout := MakeWfcLatticeLayout(
          ParseCanonicalInteger(LFields[8], 'layout cells X'),
          ParseCanonicalInteger(LFields[9], 'layout cells Y'),
          ParseCanonicalInteger(LFields[10], 'layout cells Z'),
          MakeWfcLatticeVector(
            ParseCanonicalSignedInteger(LFields[2], 'layout origin X'),
            ParseCanonicalSignedInteger(LFields[3], 'layout origin Y'),
            ParseCanonicalSignedInteger(LFields[4], 'layout origin Z')),
          MakeWfcLatticeVector(
            ParseCanonicalInteger(LFields[5], 'layout pitch X'),
            ParseCanonicalInteger(LFields[6], 'layout pitch Y'),
            ParseCanonicalInteger(LFields[7], 'layout pitch Z')),
          ParseBooleanName(LFields[11], 'layout wrap'));
      except
        on E: EWfcLattice do TextError('result layout: ' + E.Message);
      end;
      if (LRank <> ARun.PassTopologyAt(I).Rank) or
          not SameWfcLatticeLayout(LLayout, ARun.PassLayoutAt(I)) then
        TextError('result layout does not match the bound recipe/run');
    end;
  end;
  LSeed := ParseCanonicalCardinal(ReadValueLine(LLines, LLineIndex,
    'seed=', 'seed'), 'seed');
  LStrategy := ParseStrategy(ReadValueLine(LLines, LLineIndex,
    'strategy=', 'strategy'));
  LMaxBacktracks := ParseCanonicalInteger(ReadValueLine(LLines,
    LLineIndex, 'max-backtracks=', 'maximum backtracks'),
    'maximum backtracks');
  LMaxPassBacktracks := ParseCanonicalInteger(ReadValueLine(LLines,
    LLineIndex, 'max-pass-backtracks=', 'maximum pass backtracks'),
    'maximum pass backtracks');
  LCaptureTrace := ParseBooleanName(ReadValueLine(LLines, LLineIndex,
    'trace=', 'trace flag'), 'trace flag');
  if (LWidth <> ARun.Width) or (LHeight <> ARun.Height) or
      (LDepth <> ARun.Depth) or (LSeed <> ARun.Seed) or
      (LStrategy <> ARun.Strategy) or
      (LMaxBacktracks <> ARun.MaxBacktracks) or
      (LMaxPassBacktracks <> ARun.MaxPassBacktracks) or
      (LCaptureTrace <> ARun.CaptureTrace) then
    TextError('embedded run fields do not match the supplied run');

  LStatus := ParseStatus(ReadValueLine(LLines, LLineIndex,
    'status=', 'result status'));
  LPassBacktracks := ParseCanonicalInteger(ReadValueLine(LLines,
    LLineIndex, 'pass-backtracks=', 'pass-backtrack count'),
    'pass-backtrack count');
  LEvidenceKind := ParseEvidence(ReadValueLine(LLines, LLineIndex,
    'evidence=', 'evidence kind'));
  LEvidenceSignature := ParseSignature(ReadValueLine(LLines, LLineIndex,
    'evidence-signature=', 'evidence signature'), 'evidence signature');

  LFailure.Kind := ParseContradiction(ReadValueLine(LLines, LLineIndex,
    'failure-kind=', 'failure kind'));
  LFailure.PassIndex := ParseCanonicalSignedInteger(ReadValueLine(LLines,
    LLineIndex, 'failure-pass=', 'failure pass'), 'failure pass');
  LFailure.EntryIndex := ParseCanonicalSignedInteger(ReadValueLine(LLines,
    LLineIndex, 'failure-entry=', 'failure entry'), 'failure entry');
  LFailure.NeighborIndex := ParseCanonicalSignedInteger(ReadValueLine(LLines,
    LLineIndex, 'failure-neighbor=', 'failure neighbor'),
    'failure neighbor');
  LFailure.HasDirection := ParseBooleanName(ReadValueLine(LLines,
    LLineIndex, 'failure-direction-present=', 'failure direction flag'),
    'failure direction flag');
  LFailure.Direction := ParseDirection(ReadValueLine(LLines, LLineIndex,
    'failure-direction=', 'failure direction'));
  LFailure.DependencyPassIndex := ParseCanonicalSignedInteger(
    ReadValueLine(LLines, LLineIndex, 'failure-dependency-pass=',
      'failure dependency pass'), 'failure dependency pass');

  LOutcomeCount := ParseBoundedCount(ReadValueLine(LLines, LLineIndex,
    'passes=', 'pass-outcome count'), 'pass-outcome count',
    WFC_PIPELINE_MAX_PASS_COUNT);
  RequireRecordCapacity(LOutcomeCount, LLineIndex, 3, LLines,
    'pass-outcome');
  SetLength(LOutcomes, LOutcomeCount);
  for I := 0 to LOutcomeCount - 1 do
  begin
    LFields := SplitRecord(ReadValueLine(LLines, LLineIndex,
      'pass=', 'pass-outcome record'), 9, 'pass-outcome');
    if ParseCanonicalInteger(LFields[0], 'pass-outcome index') <> I then
      TextError('pass-outcome indices must be complete and ordered');
    LOutcomes[I].PassIndex := I;
    LOutcomes[I].Executed := ParseBooleanName(LFields[1],
      'pass executed flag');
    LOutcomes[I].ExecutionOrdinal := ParseCanonicalSignedInteger(
      LFields[2], 'pass execution ordinal');
    LOutcomes[I].Disposition := ParseDisposition(LFields[3]);
    LOutcomes[I].Decisions := ParseCanonicalInteger(LFields[4],
      'pass decision count');
    LOutcomes[I].Propagations := ParseCanonicalInteger(LFields[5],
      'pass propagation count');
    LOutcomes[I].Contradictions := ParseCanonicalInteger(LFields[6],
      'pass contradiction count');
    LOutcomes[I].Backtracks := ParseCanonicalInteger(LFields[7],
      'pass backtrack count');
    LOutcomes[I].ExcludedAssignments := ParseCanonicalInteger(LFields[8],
      'pass excluded-assignment count');
  end;

  LLayerCount := ParseBoundedCount(ReadValueLine(LLines, LLineIndex,
    'layers=', 'public-layer count'), 'public-layer count',
    WFC_PIPELINE_RESULT_MAX_PUBLIC_LAYER_COUNT);
  RequireRecordCapacity(LLayerCount, LLineIndex, 2, LLines,
    'public-layer');
  LExpectedPublicCount := 0;
  if LStatus = wprsSolved then
    for I := 0 to ARecipe.PassCount - 1 do
      if ARecipe.PassAt(I).Visibility = wppvPublic then Inc(LExpectedPublicCount);
  if LLayerCount <> LExpectedPublicCount then
    TextError('result layer count does not match its terminal status and recipe');
  SetLength(LLayers, LLayerCount);
  LExpectedPassIndex := 0;
  LTotalCellCount := 0;
  LTotalTokenLength := 0;
  for I := 0 to LLayerCount - 1 do
  begin
    LFields := SplitRecord(ReadValueLine(LLines, LLineIndex,
      'layer=', 'public-layer record'), 4, 'public-layer');
    if ParseCanonicalInteger(LFields[0], 'public-layer index') <> I then
      TextError('public-layer indices must be complete and ordered');
    LLayers[I].PassIndex := ParseCanonicalInteger(LFields[1],
      'public-layer pass index');
    while (LExpectedPassIndex < ARecipe.PassCount) and
        (ARecipe.PassAt(LExpectedPassIndex).Visibility <> wppvPublic) do
      Inc(LExpectedPassIndex);
    if LLayers[I].PassIndex <> LExpectedPassIndex then
      TextError('public layers must be complete and in recipe pass order');
    LCellCount := ParseBoundedCount(LFields[3],
      'public-layer cell count', WFC_PIPELINE_RUN_MAX_CELL_COUNT);
    if LCellCount <> ARun.PassCellCount(LExpectedPassIndex) then
      TextError('public layer extent differs from its bound pass');
    if LCellCount > WFC_PIPELINE_RESULT_MAX_TOTAL_PUBLIC_CELL_COUNT -
        LTotalCellCount then
      TextError('public result cells exceed the version-1 aggregate limit');
    Inc(LTotalCellCount, LCellCount);
    RequireRecordCapacity(LCellCount, LLineIndex,
      (LLayerCount - I - 1) + 2, LLines, 'public value');
    LLayers[I].LabelName := DecodeResultToken(LFields[2],
      'public-layer label');
    SetLength(LTokens, LCellCount);
    for J := 0 to LCellCount - 1 do
    begin
      LFields := SplitRecord(ReadValueLine(LLines, LLineIndex,
        'value=', 'public value record'), 3, 'public value');
      if (ParseCanonicalInteger(LFields[0],
          'public value layer index') <> I) or
          (ParseCanonicalInteger(LFields[1],
          'public value cell index') <> J) then
        TextError('public value indices must be complete and ordered');
      LTokens[J] := DecodeResultToken(LFields[2], 'public value token');
    end;
    LLayers[I].Tokens := LTokens;
    LTokens := nil;
    Inc(LExpectedPassIndex);
  end;

  LSignatureText := ReadValueLine(LLines, LLineIndex,
    'signature=', 'result signature');
  if not SignatureTextIsCanonical(LSignatureText) then
    TextError('result signature must be eight uppercase hexadecimal digits');
  if RequireLine(LLines, LLineIndex, 'end marker') <> 'end' then
    TextError('end marker is missing or misplaced');
  Inc(LLineIndex);
  if LLineIndex <> Length(LLines) then
    TextError('trailing data is not permitted');

  LModel := nil;
  try
    try
      LModel := TWfcPipelineResult.Create(ARecipe, ARun, LVersions,
        LStatus, LPassBacktracks, LEvidenceKind, LEvidenceSignature,
        LFailure, LOutcomes, LLayers);
    except
      on E: EWfcPipelineResult do
        TextError(E.Message);
    end;
    if WfcPipelineResultSignatureHex(LModel.Signature) <>
        LSignatureText then
      TextError('result signature does not match its semantic result');
    if EncodeWfcPipelineResultText(LModel) <> AText then
      TextError('document is not in canonical form');
    Result := LModel;
    LModel := nil;
  finally
    LModel.Free;
  end;
end;

end.
