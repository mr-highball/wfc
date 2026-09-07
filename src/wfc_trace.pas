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
unit wfc_trace;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc;

const
  WFC_TRACE_UTILITY_VERSION = 1;
  //Derived indexing only: the original event schema and hashes stay v1.
  WFC_TRACE_LAYOUT_VERSION = 1;

type
  EWfcTrace = class(Exception);

  TGraphTraceValidationIssueKind = (
    gtvikNone,
    gtvikGraph,
    gtvikPassCount,
    gtvikDisabledCapture,
    gtvikMissingTrace,
    gtvikTraceHash,
    gtvikEventId,
    gtvikCauseEventId,
    gtvikCausalLink,
    gtvikEventKind,
    gtvikCauseKind,
    gtvikPassIndex,
    gtvikEntryIndex,
    gtvikValueIndex,
    gtvikValue,
    gtvikNeighborIndex,
    gtvikDirection,
    gtvikDependencyPassIndex,
    gtvikDecisionDepth,
    gtvikDomainCount,
    gtvikEventFields,
    gtvikPassSlice,
    gtvikTerminalEvent,
    gtvikConstraintIndex,
    gtvikTraceLayout,
    gtvikPassLifecycle
  );

  TGraphTraceValidationIssue = record
    Kind: TGraphTraceValidationIssueKind;
    EventIndex: Integer;
    PassIndex: Integer;
  end;

  TGraphTraceValidationReport = record
    Valid: Boolean;
    CheckedEvents: Integer;
    Issue: TGraphTraceValidationIssue;
  end;

  TGraphTraceRange = record
    Start, Count: Integer;
  end;
  TGraphTraceRanges = array of TGraphTraceRange;
  TGraphPassTraceLayout = record
    EventCount: Integer;
    Ranges: TGraphTraceRanges;
  end;
  TGraphPassTraceLayouts = array of TGraphPassTraceLayout;
  TGraphTraceLayout = record
    Version: Integer;
    TraceCaptured: Boolean;
    TraceHash: TGraphTraceSignature;
    EventCount, TerminalEventIndex: Integer;
    Passes: TGraphPassTraceLayouts;
  end;

function GraphTraceEventKindName(
  const AKind: TGraphTraceEventKind): String;
function GraphTraceCauseKindName(
  const AKind: TGraphTraceCauseKind): String;
function GraphTraceDirectionName(
  const ADirection: TGraphDirection): String;
function GraphTraceValidationIssueKindName(
  const AKind: TGraphTraceValidationIssueKind): String;

function GraphTraceSignatureHex(
  const ASignature: TGraphTraceSignature): String;

function TryFindGraphTraceEvent(const AEvents: TGraphTraceEvents;
  const AEventId: Integer; out AEvent: TGraphTraceEvent): Boolean;
function FindGraphTraceEvent(const AEvents: TGraphTraceEvents;
  const AEventId: Integer): TGraphTraceEvent;

function CopyGraphTraceEvents(
  const AEvents: TGraphTraceEvents): TGraphTraceEvents;
function CopyGraphTraceEventsForPass(const AEvents: TGraphTraceEvents;
  const APassIndex: Integer): TGraphTraceEvents;
function CopyGraphTraceEventsForEntry(const AEvents: TGraphTraceEvents;
  const APassIndex, AEntryIndex: Integer): TGraphTraceEvents;

function TryGraphEntryIndexToPosition(
  const ADimension: TGraph.TDimension; const AEntryIndex: Integer;
  out APosition: TGraphPosition): Boolean;
function GraphEntryIndexToPosition(const ADimension: TGraph.TDimension;
  const AEntryIndex: Integer): TGraphPosition;

function FormatGraphTraceEvent(const AEvent: TGraphTraceEvent): String;

function ValidateGraphTrace(const AGraph: TGraph;
  const AReport: TGraphSolveReport;
  out AValidation: TGraphTraceValidationReport): Boolean;
//The layout owns only detached maximal ranges into the unchanged event array.
//Construction validates the chronological lifecycle, including a late commit
//failure suffix. Failure publishes no partial layout. The legacy validator
//above deliberately retains its single-contiguous-slice contract.
function TryBuildGraphTraceLayout(const AGraph: TGraph;
  const AReport: TGraphSolveReport; out ALayout: TGraphTraceLayout;
  out AValidation: TGraphTraceValidationReport): Boolean;
function ValidateGraphTraceLayout(const AGraph: TGraph;
  const AReport: TGraphSolveReport; const ALayout: TGraphTraceLayout;
  out AValidation: TGraphTraceValidationReport): Boolean;
function CopyGraphTraceLayout(
  const ASource: TGraphTraceLayout): TGraphTraceLayout;
function DescribeGraphTraceValidationIssue(
  const AIssue: TGraphTraceValidationIssue): String;

implementation

const
  TRACE_HEX_DIGITS = '0123456789ABCDEF';

type
  TTraceByteArray = array of Byte;
  TTraceGraphValueArrays = array of TGraphValues;

function IsValidTraceEventKind(
  const AKind: TGraphTraceEventKind): Boolean;
begin
  Result := (Ord(AKind) >= Ord(Low(TGraphTraceEventKind))) and
    (Ord(AKind) <= Ord(High(TGraphTraceEventKind)));
end;

function IsValidTraceCauseKind(
  const AKind: TGraphTraceCauseKind): Boolean;
begin
  Result := (Ord(AKind) >= Ord(Low(TGraphTraceCauseKind))) and
    (Ord(AKind) <= Ord(High(TGraphTraceCauseKind)));
end;

function IsValidDirection(const ADirection: TGraphDirection): Boolean;
begin
  Result := (Ord(ADirection) >= Ord(Low(TGraphDirection))) and
    (Ord(ADirection) <= Ord(High(TGraphDirection)));
end;

function IsValidValidationIssueKind(
  const AKind: TGraphTraceValidationIssueKind): Boolean;
begin
  Result := (Ord(AKind) >= Ord(Low(TGraphTraceValidationIssueKind))) and
    (Ord(AKind) <= Ord(High(TGraphTraceValidationIssueKind)));
end;

function GraphTraceEventKindName(
  const AKind: TGraphTraceEventKind): String;
begin
  case AKind of
    gtekPassBegin: Result := 'pass-begin';
    gtekInitialCandidateRemoved:
      Result := 'initial-candidate-removed';
    gtekDecision: Result := 'decision';
    gtekCandidateRemoved: Result := 'candidate-removed';
    gtekContradiction: Result := 'contradiction';
    gtekBacktrack: Result := 'backtrack';
    gtekCandidateRestored: Result := 'candidate-restored';
    gtekPassStaged: Result := 'pass-staged';
    gtekPassFailed: Result := 'pass-failed';
    gtekPassSkipped: Result := 'pass-skipped';
    gtekPipelineCommit: Result := 'pipeline-commit';
    gtekPipelineRollback: Result := 'pipeline-rollback';
  else
    Result := 'unknown-event-' + IntToStr(Ord(AKind));
  end;
end;

function GraphTraceCauseKindName(
  const AKind: TGraphTraceCauseKind): String;
begin
  case AKind of
    gtckNone: Result := 'none';
    gtckCallerDomain: Result := 'caller-domain';
    gtckCallerLock: Result := 'caller-lock';
    gtckDecision: Result := 'decision';
    gtckAdjacency: Result := 'adjacency';
    gtckPassDependency: Result := 'pass-dependency';
    gtckRequiredSupport: Result := 'required-support';
    gtckBacktrack: Result := 'backtrack';
    gtckFinalValidation: Result := 'final-validation';
    gtckTransaction: Result := 'transaction';
    gtckExactAssignmentExclusion:
      Result := 'exact-assignment-exclusion';
    gtckConnectivity: Result := 'connectivity';
    gtckValueQuota: Result := 'value-quota';
  else
    Result := 'unknown-cause-' + IntToStr(Ord(AKind));
  end;
end;

function GraphTraceDirectionName(
  const ADirection: TGraphDirection): String;
begin
  case ADirection of
    gdNorth: Result := 'north';
    gdEast: Result := 'east';
    gdSouth: Result := 'south';
    gdWest: Result := 'west';
    gdUp: Result := 'up';
    gdDown: Result := 'down';
  else
    Result := 'unknown-direction-' + IntToStr(Ord(ADirection));
  end;
end;

function GraphTraceValidationIssueKindName(
  const AKind: TGraphTraceValidationIssueKind): String;
begin
  case AKind of
    gtvikNone: Result := 'none';
    gtvikGraph: Result := 'graph';
    gtvikPassCount: Result := 'pass-count';
    gtvikDisabledCapture: Result := 'disabled-capture';
    gtvikMissingTrace: Result := 'missing-trace';
    gtvikTraceHash: Result := 'trace-hash';
    gtvikEventId: Result := 'event-id';
    gtvikCauseEventId: Result := 'cause-event-id';
    gtvikCausalLink: Result := 'causal-link';
    gtvikEventKind: Result := 'event-kind';
    gtvikCauseKind: Result := 'cause-kind';
    gtvikPassIndex: Result := 'pass-index';
    gtvikEntryIndex: Result := 'entry-index';
    gtvikValueIndex: Result := 'value-index';
    gtvikValue: Result := 'value';
    gtvikNeighborIndex: Result := 'neighbor-index';
    gtvikDirection: Result := 'direction';
    gtvikDependencyPassIndex: Result := 'dependency-pass-index';
    gtvikDecisionDepth: Result := 'decision-depth';
    gtvikDomainCount: Result := 'domain-count';
    gtvikEventFields: Result := 'event-fields';
    gtvikPassSlice: Result := 'pass-slice';
    gtvikTerminalEvent: Result := 'terminal-event';
    gtvikConstraintIndex: Result := 'constraint-index';
    gtvikTraceLayout: Result := 'trace-layout';
    gtvikPassLifecycle: Result := 'pass-lifecycle';
  else
    Result := 'unknown-validation-issue-' + IntToStr(Ord(AKind));
  end;
end;

function GraphTraceSignatureHex(
  const ASignature: TGraphTraceSignature): String;
var
  I: Integer;
  LValue: Cardinal;
begin
  SetLength(Result, 8);
  LValue := ASignature;
  for I := 8 downto 1 do
  begin
    Result[I] := TRACE_HEX_DIGITS[
      Integer(LValue and Cardinal($F)) + 1];
    LValue := LValue shr 4;
  end;
end;

function TryFindGraphTraceEvent(const AEvents: TGraphTraceEvents;
  const AEventId: Integer; out AEvent: TGraphTraceEvent): Boolean;
var
  I: Integer;
begin
  AEvent := Default(TGraphTraceEvent);
  for I := 0 to Length(AEvents) - 1 do
    if AEvents[I].EventId = AEventId then
    begin
      AEvent := AEvents[I];
      Exit(True);
    end;
  Result := False;
end;

function FindGraphTraceEvent(const AEvents: TGraphTraceEvents;
  const AEventId: Integer): TGraphTraceEvent;
begin
  if not TryFindGraphTraceEvent(AEvents, AEventId, Result) then
    raise EWfcTrace.CreateFmt('trace event id was not found [%d]',
      [AEventId]);
end;

function CopyGraphTraceEvents(
  const AEvents: TGraphTraceEvents): TGraphTraceEvents;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AEvents));
  for I := 0 to Length(AEvents) - 1 do
    Result[I] := AEvents[I];
end;

function CopyGraphTraceEventsForPass(const AEvents: TGraphTraceEvents;
  const APassIndex: Integer): TGraphTraceEvents;
var
  I: Integer;
  LCount: Integer;
begin
  Result := nil;
  LCount := 0;
  for I := 0 to Length(AEvents) - 1 do
    if AEvents[I].PassIndex = APassIndex then
      Inc(LCount);
  SetLength(Result, LCount);
  LCount := 0;
  for I := 0 to Length(AEvents) - 1 do
    if AEvents[I].PassIndex = APassIndex then
    begin
      Result[LCount] := AEvents[I];
      Inc(LCount);
    end;
end;

function CopyGraphTraceEventsForEntry(const AEvents: TGraphTraceEvents;
  const APassIndex, AEntryIndex: Integer): TGraphTraceEvents;
var
  I: Integer;
  LCount: Integer;
begin
  Result := nil;
  LCount := 0;
  for I := 0 to Length(AEvents) - 1 do
    if (AEvents[I].PassIndex = APassIndex) and
        (AEvents[I].EntryIndex = AEntryIndex) then
      Inc(LCount);
  SetLength(Result, LCount);
  LCount := 0;
  for I := 0 to Length(AEvents) - 1 do
    if (AEvents[I].PassIndex = APassIndex) and
        (AEvents[I].EntryIndex = AEntryIndex) then
    begin
      Result[LCount] := AEvents[I];
      Inc(LCount);
    end;
end;

function TryDimensionEntryCount(const ADimension: TGraph.TDimension;
  out AEntryCount: Integer): Boolean;
var
  LPlaneSize: TGraphCoordinate;
begin
  AEntryCount := 0;
  if (ADimension.Width = 0) or (ADimension.Height = 0) or
      (ADimension.Depth = 0) then
    Exit(True);
  if ADimension.Width > TGraphCoordinate(High(Integer)) div
      ADimension.Height then
    Exit(False);
  LPlaneSize := ADimension.Width * ADimension.Height;
  if LPlaneSize > TGraphCoordinate(High(Integer)) div
      ADimension.Depth then
    Exit(False);
  AEntryCount := Integer(LPlaneSize * ADimension.Depth);
  Result := True;
end;

function TryGraphEntryIndexToPosition(
  const ADimension: TGraph.TDimension; const AEntryIndex: Integer;
  out APosition: TGraphPosition): Boolean;
var
  LEntryCount: Integer;
  LHeight: Integer;
  LPlaneSize: Integer;
  LWidth: Integer;
begin
  APosition.X := 0;
  APosition.Y := 0;
  APosition.Z := 0;
  if not TryDimensionEntryCount(ADimension, LEntryCount) then
    Exit(False);
  if (AEntryIndex < 0) or (AEntryIndex >= LEntryCount) then
    Exit(False);
  LWidth := Integer(ADimension.Width);
  LHeight := Integer(ADimension.Height);
  LPlaneSize := LWidth * LHeight;
  APosition.X := TGraphCoordinate(AEntryIndex mod LWidth);
  APosition.Y := TGraphCoordinate(
    (AEntryIndex div LWidth) mod LHeight);
  APosition.Z := TGraphCoordinate(AEntryIndex div LPlaneSize);
  Result := True;
end;

function GraphEntryIndexToPosition(const ADimension: TGraph.TDimension;
  const AEntryIndex: Integer): TGraphPosition;
begin
  if not TryGraphEntryIndexToPosition(ADimension, AEntryIndex,
      Result) then
    raise EWfcTrace.CreateFmt(
      'trace entry index does not fit the graph dimensions [%d]',
      [AEntryIndex]);
end;

function AppendFixedHex(const AValue: Cardinal;
  const ADigits: Integer): String;
var
  I: Integer;
  LValue: Cardinal;
begin
  SetLength(Result, ADigits);
  LValue := AValue;
  for I := ADigits downto 1 do
  begin
    Result[I] := TRACE_HEX_DIGITS[
      Integer(LValue and Cardinal($F)) + 1];
    LValue := LValue shr 4;
  end;
end;

function IsTraceValueLiteral(const ACodePoint: Cardinal): Boolean;
begin
  Result := ((ACodePoint >= Ord('A')) and
      (ACodePoint <= Ord('Z'))) or
    ((ACodePoint >= Ord('a')) and
      (ACodePoint <= Ord('z'))) or
    ((ACodePoint >= Ord('0')) and
      (ACodePoint <= Ord('9'))) or
    (ACodePoint = Ord('-')) or (ACodePoint = Ord('.')) or
    (ACodePoint = Ord('_')) or (ACodePoint = Ord('~'));
end;

function EscapedGraphValue(const AValue: TGraphValue): String;
{$IFDEF PAS2JS}
var
  LText: String;
{$ELSE}
var
  LText: UnicodeString;
{$ENDIF}
var
  I: Integer;
  LCodePoint: Cardinal;
  LCodeUnit: Cardinal;
  LLow: Cardinal;
begin
  Result := '';
  {$IFDEF PAS2JS}
  LText := String(AValue);
  {$ELSE}
  LText := UnicodeString(AValue);
  {$ENDIF}
  I := 1;
  while I <= Length(LText) do
  begin
    LCodeUnit := Ord(LText[I]);
    Inc(I);
    if (LCodeUnit >= $D800) and (LCodeUnit <= $DBFF) and
        (I <= Length(LText)) then
    begin
      LLow := Ord(LText[I]);
      if (LLow >= $DC00) and (LLow <= $DFFF) then
      begin
        Inc(I);
        LCodePoint := $10000 + ((LCodeUnit - $D800) shl 10) +
          (LLow - $DC00);
      end
      else
        LCodePoint := LCodeUnit;
    end
    else
      LCodePoint := LCodeUnit;

    if IsTraceValueLiteral(LCodePoint) then
      Result := Result + Chr(LCodePoint)
    else if LCodePoint <= $FF then
      Result := Result + '%' + AppendFixedHex(LCodePoint, 2)
    else if LCodePoint <= $FFFF then
      Result := Result + '%U' + AppendFixedHex(LCodePoint, 4)
    else
      Result := Result + '%U' + AppendFixedHex(LCodePoint, 6);
  end;
end;

function FormatGraphTraceEvent(const AEvent: TGraphTraceEvent): String;
var
  LDirection: String;
begin
  if AEvent.HasDirection then
    LDirection := GraphTraceDirectionName(AEvent.Direction)
  else
    LDirection := 'none';
  Result := 'event=' + IntToStr(AEvent.EventId) +
    ' kind=' + GraphTraceEventKindName(AEvent.Kind) +
    ' cause=' + GraphTraceCauseKindName(AEvent.CauseKind) +
    ' cause-event=' + IntToStr(AEvent.CauseEventId) +
    ' pass=' + IntToStr(AEvent.PassIndex) +
    ' entry=' + IntToStr(AEvent.EntryIndex) +
    ' value-index=' + IntToStr(AEvent.ValueIndex) +
    ' value=' + EscapedGraphValue(AEvent.Value) +
    ' neighbor=' + IntToStr(AEvent.NeighborIndex) +
    ' direction=' + LDirection +
    ' dependency-pass=' + IntToStr(AEvent.DependencyPassIndex) +
    ' depth=' + IntToStr(AEvent.DecisionDepth) +
    ' domain=' + IntToStr(AEvent.DomainCountBefore) + '->' +
      IntToStr(AEvent.DomainCountAfter);
  if AEvent.CauseKind = gtckConnectivity then
    Result := Result + ' connectivity=' + IntToStr(AEvent.ConstraintIndex);
  if AEvent.CauseKind = gtckValueQuota then
    Result := Result + ' value-quota=' + IntToStr(AEvent.ConstraintIndex);
end;

procedure InitializeValidation(out AValidation: TGraphTraceValidationReport);
begin
  AValidation := Default(TGraphTraceValidationReport);
  AValidation.Valid := True;
  AValidation.Issue.Kind := gtvikNone;
  AValidation.Issue.EventIndex := -1;
  AValidation.Issue.PassIndex := -1;
end;

function InvalidTrace(var AValidation: TGraphTraceValidationReport;
  const AKind: TGraphTraceValidationIssueKind;
  const AEventIndex, APassIndex: Integer): Boolean;
begin
  AValidation.Valid := False;
  AValidation.Issue.Kind := AKind;
  AValidation.Issue.EventIndex := AEventIndex;
  AValidation.Issue.PassIndex := APassIndex;
  Result := False;
end;

function IsPipelineEventKind(
  const AKind: TGraphTraceEventKind): Boolean;
begin
  Result := AKind in [gtekPipelineCommit, gtekPipelineRollback];
end;

function IsPassMetadataEventKind(
  const AKind: TGraphTraceEventKind): Boolean;
begin
  Result := AKind in [gtekPassBegin, gtekPassStaged,
    gtekPassFailed, gtekPassSkipped];
end;

function IsRemovalEventKind(
  const AKind: TGraphTraceEventKind): Boolean;
begin
  Result := AKind in [gtekInitialCandidateRemoved,
    gtekCandidateRemoved];
end;

function EventRequiresValue(
  const AKind: TGraphTraceEventKind): Boolean;
begin
  Result := AKind in [gtekInitialCandidateRemoved, gtekDecision,
    gtekCandidateRemoved, gtekCandidateRestored];
end;

function EventRequiresEntry(
  const AKind: TGraphTraceEventKind): Boolean;
begin
  Result := AKind in [gtekInitialCandidateRemoved, gtekDecision,
    gtekCandidateRemoved, gtekContradiction, gtekBacktrack,
    gtekCandidateRestored];
end;

function HasDeclaredDependency(const AGraph: TGraph;
  const APassIndex, ADependencyPassIndex: Integer): Boolean;
var
  I: Integer;
  LPass: TGraph;
begin
  LPass := AGraph.PassGraph[APassIndex];
  for I := 0 to LPass.DependencyCount - 1 do
    if LPass.DependencyIndex[I] = ADependencyPassIndex then
      Exit(True);
  Result := False;
end;

function IsTraceInteger(const AValue, AMinimum, AMaximum: Double): Boolean;
begin
  Result := (AValue >= AMinimum) and (AValue <= AMaximum);
  {$IFDEF PAS2JS}
  Result := Result and (AValue = Trunc(AValue));
  {$ENDIF}
end;

function IsTraceBoolean(const AValue: Boolean): Boolean;
begin
  {$IFDEF PAS2JS}
  Result := (AValue = False) or (AValue = True);
  {$ELSE}
  Result := Ord(AValue) <= 1;
  {$ENDIF}
end;

function ValidateTraceInputNumbers(const AReport: TGraphSolveReport;
  var AValidation: TGraphTraceValidationReport): Boolean;
var I, P: Integer; E: TGraphTraceEvent; R: TGraphPassSolveReport;
begin
  //The additive validator accepts host-created records too. Check exact
  //numbers before array indexing, enum membership, integer arithmetic or
  //hash conversion; NaN must never bypass a pair of negative comparisons.
  if not IsTraceInteger(Length(AReport.Trace), 0, High(Integer)) or
    not IsTraceInteger(Length(AReport.Passes), 0, High(Integer)) or
    not IsTraceInteger(Length(AReport.ExecutionOrder), 0, High(Integer)) then
    Exit(InvalidTrace(AValidation, gtvikTraceLayout, -1, -1));
  P := Length(AReport.Passes);
  if not IsTraceBoolean(AReport.TraceCaptured) or
    not IsTraceInteger(AReport.TraceHash, 0, 4294967295.0) or
    not IsTraceInteger(AReport.Seed, 0, 4294967295.0) or
    not IsTraceInteger(AReport.RandomAlgorithmVersion, 0, High(Integer)) or
    not IsTraceInteger(AReport.SolverAlgorithmVersion, 0, High(Integer)) or
    not IsTraceInteger(AReport.GraphModelVersion, 0, High(Integer)) or
    not IsTraceInteger(AReport.PipelineAlgorithmVersion, 0, High(Integer)) then
    Exit(InvalidTrace(AValidation, gtvikTraceLayout, -1, -1));
  if not IsTraceInteger(Ord(AReport.Status), Ord(Low(TGraphSolveStatus)),
    Ord(High(TGraphSolveStatus))) or
    not IsTraceInteger(AReport.FailedPassIndex, -1, P - 1) then
    Exit(InvalidTrace(AValidation, gtvikPassLifecycle, -1, -1));
  with AReport.Contradiction do
    if not IsTraceInteger(Ord(Kind), Ord(Low(TGraphContradictionKind)),
      Ord(High(TGraphContradictionKind))) or
      not IsTraceInteger(PassIndex, -1, P - 1) or
      not IsTraceInteger(EntryIndex, -1, High(Integer)) or
      not IsTraceInteger(NeighborIndex, -1, High(Integer)) or
      not IsTraceBoolean(HasDirection) or
      not IsTraceInteger(Ord(Direction), Ord(Low(TGraphDirection)),
        Ord(High(TGraphDirection))) or
      not IsTraceInteger(DependencyPassIndex, -1, P - 1) or
      not IsTraceInteger(ConstraintIndex, -1, High(Integer)) then
      Exit(InvalidTrace(AValidation, gtvikPassLifecycle, -1, -1));
  for I := 0 to High(AReport.ExecutionOrder) do
    if not IsTraceInteger(AReport.ExecutionOrder[I], 0, P - 1) then
      Exit(InvalidTrace(AValidation, gtvikPassLifecycle, -1, -1));
  for I := 0 to P - 1 do
  begin
    R := AReport.Passes[I];
    if not IsTraceInteger(R.TraceStart, -1, High(Integer)) or
      not IsTraceInteger(R.TraceCount, 0, High(Integer)) then
      Exit(InvalidTrace(AValidation, gtvikPassSlice, -1, I));
    if not IsTraceBoolean(R.Executed) or
      not IsTraceInteger(R.ExecutionOrdinal, -1, P - 1) or
      not IsTraceInteger(Ord(R.Disposition), Ord(Low(TGraphPassDisposition)),
        Ord(High(TGraphPassDisposition))) or
      not IsTraceInteger(R.Decisions, 0, High(Integer)) or
      not IsTraceInteger(R.Propagations, 0, High(Integer)) or
      not IsTraceInteger(R.Contradictions, 0, High(Integer)) or
      not IsTraceInteger(R.Backtracks, 0, High(Integer)) or
      not IsTraceInteger(R.ExcludedAssignments, 0, High(Integer)) then
      Exit(InvalidTrace(AValidation, gtvikPassLifecycle, -1, I));
  end;
  for I := 0 to High(AReport.Trace) do
  begin
    E := AReport.Trace[I];
    if not IsTraceInteger(E.EventId, 0, High(Integer)) then
      Exit(InvalidTrace(AValidation, gtvikEventId, I, -1));
    if not IsTraceInteger(E.CauseEventId, -1, High(Integer)) then
      Exit(InvalidTrace(AValidation, gtvikCauseEventId, I, -1));
    if not IsTraceInteger(E.PassIndex, -1, P - 1) then
      Exit(InvalidTrace(AValidation, gtvikPassIndex, I, -1));
    if not IsTraceInteger(Ord(E.Kind), Ord(Low(TGraphTraceEventKind)),
      Ord(High(TGraphTraceEventKind))) then
      Exit(InvalidTrace(AValidation, gtvikEventKind, I, E.PassIndex));
    if not IsTraceInteger(Ord(E.CauseKind), Ord(Low(TGraphTraceCauseKind)),
      Ord(High(TGraphTraceCauseKind))) then
      Exit(InvalidTrace(AValidation, gtvikCauseKind, I, E.PassIndex));
    if not IsTraceInteger(E.EntryIndex, -1, High(Integer)) then
      Exit(InvalidTrace(AValidation, gtvikEntryIndex, I, E.PassIndex));
    if not IsTraceInteger(E.ValueIndex, -1, High(Integer)) then
      Exit(InvalidTrace(AValidation, gtvikValueIndex, I, E.PassIndex));
    if not IsTraceInteger(E.NeighborIndex, -1, High(Integer)) then
      Exit(InvalidTrace(AValidation, gtvikNeighborIndex, I, E.PassIndex));
    if not IsTraceBoolean(E.HasDirection) or
      not IsTraceInteger(Ord(E.Direction), Ord(Low(TGraphDirection)),
        Ord(High(TGraphDirection))) then
      Exit(InvalidTrace(AValidation, gtvikDirection, I, E.PassIndex));
    if not IsTraceInteger(E.DependencyPassIndex, -1, P - 1) then
      Exit(InvalidTrace(AValidation, gtvikDependencyPassIndex, I, E.PassIndex));
    if not IsTraceInteger(E.DecisionDepth, 0, High(Integer)) then
      Exit(InvalidTrace(AValidation, gtvikDecisionDepth, I, E.PassIndex));
    if not IsTraceInteger(E.DomainCountBefore, 0, High(Integer)) or
      not IsTraceInteger(E.DomainCountAfter, 0, High(Integer)) then
      Exit(InvalidTrace(AValidation, gtvikDomainCount, I, E.PassIndex));
    if not IsTraceInteger(E.ConstraintIndex, -1, High(Integer)) then
      Exit(InvalidTrace(AValidation, gtvikConstraintIndex, I, E.PassIndex));
  end;
  Result := True;
end;

procedure BuildTraceLayout(const AReport: TGraphSolveReport;
  out ALayout: TGraphTraceLayout);
var I, P, R, LPrevious: Integer; LRangeCounts: TGraphPassIndices;
begin
  ALayout := Default(TGraphTraceLayout);
  ALayout.Version := WFC_TRACE_LAYOUT_VERSION;
  ALayout.TraceCaptured := AReport.TraceCaptured;
  ALayout.TraceHash := AReport.TraceHash;
  ALayout.EventCount := Length(AReport.Trace);
  ALayout.TerminalEventIndex := -1;
  SetLength(ALayout.Passes, Length(AReport.Passes));
  SetLength(LRangeCounts, Length(AReport.Passes));
  LPrevious := -1;
  for I := 0 to High(AReport.Trace) do
  begin
    P := AReport.Trace[I].PassIndex;
    if P >= 0 then
    begin
      Inc(ALayout.Passes[P].EventCount);
      if P <> LPrevious then Inc(LRangeCounts[P]);
    end
    else ALayout.TerminalEventIndex := I;
    LPrevious := P;
  end;
  for P := 0 to High(ALayout.Passes) do
  begin
    SetLength(ALayout.Passes[P].Ranges, LRangeCounts[P]);
    LRangeCounts[P] := 0;
  end;
  LPrevious := -1;
  for I := 0 to High(AReport.Trace) do
  begin
    P := AReport.Trace[I].PassIndex;
    if P >= 0 then
    begin
      if P <> LPrevious then
      begin
        R := LRangeCounts[P];
        ALayout.Passes[P].Ranges[R].Start := I;
        Inc(LRangeCounts[P]);
      end;
      R := LRangeCounts[P] - 1;
      Inc(ALayout.Passes[P].Ranges[R].Count);
    end;
    LPrevious := P;
  end;
end;

function ValidateFailureSummary(const AGraph: TGraph;
  const AReport: TGraphSolveReport; const AEventIndex: Integer;
  var AValidation: TGraphTraceValidationReport): Boolean;
var
  E: TGraphTraceEvent;
  LAllowedKinds: set of TGraphContradictionKind;
begin
  if AEventIndex < 0 then
    Exit(InvalidTrace(AValidation, gtvikPassLifecycle, -1,
      AReport.FailedPassIndex));
  E := AReport.Trace[AEventIndex];
  //Initial-domain causes describe the last removal, whereas the aggregate
  //classification can describe another candidate's failed filter. Trace v1
  //does not encode enough evidence to distinguish all initial-filter kinds.
  //Do not infer a unique kind/provider from that last removal alone.
  LAllowedKinds := [gckEmptyDomain, gckEntryDomain, gckPreviousPass,
    gckPassDependency];
  case E.CauseKind of
    gtckNone, gtckCallerDomain, gtckPassDependency: ;
    gtckCallerLock:
      if E.CauseEventId = -1 then LAllowedKinds := [gckInvalidLock];
    gtckAdjacency: LAllowedKinds := [gckAdjacency];
    gtckRequiredSupport: LAllowedKinds := [gckRequiredSupport];
    gtckFinalValidation: LAllowedKinds := [gckFinalValidation];
    gtckExactAssignmentExclusion: LAllowedKinds := [gckExcludedAssignment];
    gtckConnectivity: LAllowedKinds := [gckConnectivity];
    gtckValueQuota: LAllowedKinds := [gckValueQuota];
  else LAllowedKinds := [];
  end;
  with AReport.Contradiction do
  begin
    if not (Kind in LAllowedKinds) or
      (PassIndex <> E.PassIndex) or (EntryIndex <> E.EntryIndex) or
      (NeighborIndex <> E.NeighborIndex) or
      (HasDirection <> E.HasDirection) or (Direction <> E.Direction) or
      (ConstraintIndex <> E.ConstraintIndex) then
      Exit(InvalidTrace(AValidation, gtvikPassLifecycle, AEventIndex, E.PassIndex));
    if Kind in [gckPreviousPass, gckPassDependency] then
    begin
      if (DependencyPassIndex < 0) or
        not HasDeclaredDependency(AGraph, PassIndex, DependencyPassIndex) then
        Exit(InvalidTrace(AValidation, gtvikPassLifecycle, AEventIndex, E.PassIndex));
    end
    else if DependencyPassIndex <> -1 then
      Exit(InvalidTrace(AValidation, gtvikPassLifecycle, AEventIndex, E.PassIndex));
  end;
  Result := True;
end;

function ValidateTraceLifecycle(const AGraph: TGraph;
  const AReport: TGraphSolveReport;
  var AValidation: TGraphTraceValidationReport): Boolean;
var
  I, J, P, D, LActive, LOrdinal, LLastSkipped, LLate, LEnd: Integer;
  LLastContradiction: Integer;
  LState: TTraceByteArray;
  LStagedEvent: TGraphPassIndices;
  E: TGraphTraceEvent;
  LLayout: TGraphTraceLayout;
begin
  //States: unseen, active, staged, skipped, failed. Only the recognized
  //commit suffix may revisit a staged pass. Negotiation rounds remain
  //separate reports; arbitrary interleaved work is not accepted here.
  SetLength(LState, Length(AReport.Passes));
  SetLength(LStagedEvent, Length(AReport.Passes));
  for I := 0 to High(LStagedEvent) do LStagedEvent[I] := -1;
  LActive := -1; LOrdinal := 0; LLastSkipped := -1;
  LLastContradiction := -1;
  LLate := -1;
  LEnd := High(AReport.Trace);
  if LEnd >= 2 then
  begin
    E := AReport.Trace[LEnd - 2];
    if (E.Kind = gtekContradiction) and
      (E.CauseKind = gtckFinalValidation) and (E.CauseEventId >= 0) then
      if AReport.Trace[E.CauseEventId].Kind = gtekPassStaged then
        LLate := LEnd - 2;
  end;
  if LLate >= 0 then LEnd := LLate;
  for I := 0 to LEnd - 1 do
  begin
    E := AReport.Trace[I]; P := E.PassIndex;
    if P < 0 then
      Exit(InvalidTrace(AValidation, gtvikTerminalEvent, I, P));
    case E.Kind of
      gtekPassSkipped:
        begin
          if (LActive <> -1) or (LOrdinal <> 0) or
            (P <= LLastSkipped) or (LState[P] <> 0) or
            AReport.Passes[P].Executed or
            (AReport.Passes[P].ExecutionOrdinal <> -1) then
            Exit(InvalidTrace(AValidation, gtvikPassLifecycle, I, P));
          LState[P] := 3; LLastSkipped := P;
        end;
      gtekPassBegin:
        begin
          if (LActive <> -1) or (LState[P] <> 0) or
            (LOrdinal >= Length(AReport.ExecutionOrder)) then
            Exit(InvalidTrace(AValidation, gtvikPassLifecycle, I, P));
          if (AReport.ExecutionOrder[LOrdinal] <> P) or
            not AReport.Passes[P].Executed or
            (AReport.Passes[P].ExecutionOrdinal <> LOrdinal) then
            Exit(InvalidTrace(AValidation, gtvikPassLifecycle, I, P));
          for J := 0 to AGraph.PassGraph[P].DependencyCount - 1 do
          begin
            D := AGraph.PassGraph[P].DependencyIndex[J];
            if not (LState[D] in [2, 3]) then
              Exit(InvalidTrace(AValidation, gtvikPassLifecycle, I, P));
          end;
          Inc(LOrdinal); LActive := P; LState[P] := 1;
        end;
      gtekPassStaged:
        begin
          if (LActive <> P) or (LState[P] <> 1) then
            Exit(InvalidTrace(AValidation, gtvikPassLifecycle, I, P));
          LState[P] := 2; LActive := -1; LStagedEvent[P] := I;
        end;
      gtekPassFailed:
        begin
          if (LActive <> P) or (LState[P] <> 1) or
            (I <> High(AReport.Trace) - 1) or (E.CauseEventId <> I - 1) or
            (AReport.Trace[I - 1].PassIndex <> P) then
            Exit(InvalidTrace(AValidation, gtvikPassLifecycle, I, P));
          LState[P] := 4; LActive := -1;
        end;
    else
      if (LActive <> P) or (LState[P] <> 1) then
        Exit(InvalidTrace(AValidation, gtvikPassLifecycle, I, P));
    end;
    if E.Kind = gtekContradiction then LLastContradiction := I;
  end;
  if (LActive <> -1) or (LOrdinal <> Length(AReport.ExecutionOrder)) then
    Exit(InvalidTrace(AValidation, gtvikPassLifecycle, -1, LActive));
  if LLate >= 0 then
  begin
    E := AReport.Trace[LLate]; P := E.PassIndex;
    if (LState[P] <> 2) or (E.CauseEventId <> LStagedEvent[P]) or
      (E.NeighborIndex <> -1) or E.HasDirection or
      (E.DependencyPassIndex <> -1) or (E.DecisionDepth <> 0) or
      (E.DomainCountBefore <> 0) or (E.DomainCountAfter <> 0) or
      (AReport.Trace[LLate + 1].Kind <> gtekPassFailed) or
      (AReport.Trace[LLate + 1].PassIndex <> P) or
      (AReport.Trace[LLate + 1].CauseEventId <> LLate) or
      (AReport.Trace[LLate + 2].Kind <> gtekPipelineRollback) or
      (AReport.Status <> gssContradiction) or
      (AReport.FailedPassIndex <> P) or
      (AReport.Contradiction.Kind <> gckFinalValidation) or
      (AReport.Contradiction.PassIndex <> P) or
      (AReport.Contradiction.EntryIndex <> E.EntryIndex) or
      (AReport.Contradiction.NeighborIndex <> -1) or
      AReport.Contradiction.HasDirection or
      (AReport.Contradiction.Direction <> gdNorth) or
      (AReport.Contradiction.DependencyPassIndex <> -1) or
      (AReport.Contradiction.ConstraintIndex <> -1) then
      Exit(InvalidTrace(AValidation, gtvikPassLifecycle, LLate, P));
    for I := 0 to High(LState) do
      if not (LState[I] in [2, 3]) then
        Exit(InvalidTrace(AValidation, gtvikPassLifecycle, LLate, I));
    LState[P] := 4;
  end;
  for P := 0 to High(LState) do
  begin
    case LState[P] of
      0:
        if AReport.Passes[P].Executed or
          (AReport.Passes[P].ExecutionOrdinal <> -1) or
          (AReport.Passes[P].Disposition <> gpdNotRun) or
          (AReport.Status = gssSolved) then
          Exit(InvalidTrace(AValidation, gtvikPassLifecycle, -1, P));
      2:
        if not (AReport.Passes[P].Disposition in
          [gpdSolved, gpdCopied, gpdCleared, gpdReused]) then
          Exit(InvalidTrace(AValidation, gtvikPassLifecycle, -1, P));
      3:
        if AReport.Passes[P].Disposition <> gpdReused then
          Exit(InvalidTrace(AValidation, gtvikPassLifecycle, -1, P));
      4:
        if (AReport.Passes[P].Disposition <> gpdFailed) or
          (AReport.Status = gssSolved) or (AReport.FailedPassIndex <> P) or
          (AReport.Contradiction.PassIndex <> P) or
          (AReport.Passes[P].Contradictions < 1) then
          Exit(InvalidTrace(AValidation, gtvikPassLifecycle, -1, P));
    else Exit(InvalidTrace(AValidation, gtvikPassLifecycle, -1, P));
    end;
  end;
  if AReport.Status = gssSolved then
  begin
    if (AReport.FailedPassIndex <> -1) or
      (AReport.Contradiction.Kind <> gckNone) or
      (AReport.Contradiction.PassIndex <> -1) or
      (AReport.Contradiction.EntryIndex <> -1) or
      (AReport.Contradiction.NeighborIndex <> -1) or
      AReport.Contradiction.HasDirection or
      (AReport.Contradiction.Direction <> gdNorth) or
      (AReport.Contradiction.DependencyPassIndex <> -1) or
      (AReport.Contradiction.ConstraintIndex <> -1) then
      Exit(InvalidTrace(AValidation, gtvikPassLifecycle, -1, -1));
  end
  else if (AReport.FailedPassIndex < 0) or
    (LState[AReport.FailedPassIndex] <> 4) then
    Exit(InvalidTrace(AValidation, gtvikPassLifecycle, -1, -1));
  if (AReport.Status <> gssSolved) and (LLate < 0) then
    if not ValidateFailureSummary(AGraph, AReport, LLastContradiction,
      AValidation) then Exit(False);
  BuildTraceLayout(AReport, LLayout);
  for P := 0 to High(LLayout.Passes) do
  begin
    I := -1;
    if Length(LLayout.Passes[P].Ranges) <> 0 then
      I := LLayout.Passes[P].Ranges[0].Start;
    //Do not repair the old fields: they also belong to existing negotiation
    //transcripts. For the explicitly recognized suffix, check the emitter's
    //first-event/total counters without pretending they describe one span.
    if (AReport.Passes[P].TraceStart <> I) or
      (AReport.Passes[P].TraceCount <> LLayout.Passes[P].EventCount) then
      Exit(InvalidTrace(AValidation, gtvikPassSlice, -1, P));
  end;
  Result := True;
end;

function ValidateGraphTraceInternal(const AGraph: TGraph;
  const AReport: TGraphSolveReport; const AChronological: Boolean;
  out AValidation: TGraphTraceValidationReport): Boolean;
var
  I: Integer;
  J: Integer;
  LCovered: TTraceByteArray;
  LEntryCount: Integer;
  LEvent: TGraphTraceEvent;
  LExpectedTerminal: TGraphTraceEventKind;
  LPassCount: Integer;
  LSliceEnd: Integer;
  LValues: TTraceGraphValueArrays;
  LConstraintCounts: TGraphPassIndices;
  LQuotaCounts: TGraphPassIndices;
  LConstraintCount: Integer;
begin
  InitializeValidation(AValidation);
  if not Assigned(AGraph) then
    Exit(InvalidTrace(AValidation, gtvikGraph, -1, -1));

  LPassCount := AGraph.TotalPassCount;
  if Length(AReport.Passes) <> LPassCount then
    Exit(InvalidTrace(AValidation, gtvikPassCount, -1, -1));
  if not TryDimensionEntryCount(AGraph.Dimension, LEntryCount) then
    Exit(InvalidTrace(AValidation, gtvikGraph, -1, -1));
  if AChronological and
    not ValidateTraceInputNumbers(AReport, AValidation) then Exit(False);

  if not AReport.TraceCaptured then
  begin
    if (Length(AReport.Trace) <> 0) or (AReport.TraceHash <> 0) then
      Exit(InvalidTrace(AValidation, gtvikDisabledCapture, -1, -1));
    for I := 0 to LPassCount - 1 do
      if (AReport.Passes[I].TraceStart <> -1) or
          (AReport.Passes[I].TraceCount <> 0) then
        Exit(InvalidTrace(AValidation, gtvikPassSlice, -1, I));
    Exit(True);
  end;

  if Length(AReport.Trace) = 0 then
    Exit(InvalidTrace(AValidation, gtvikMissingTrace, -1, -1));

  SetLength(LValues, LPassCount);
  SetLength(LConstraintCounts, LPassCount);
  SetLength(LQuotaCounts, LPassCount);
  for I := 0 to LPassCount - 1 do
  begin
    LValues[I] := AGraph.PassGraph[I].CopyRegisteredValues;
    LConstraintCounts[I] := Length(AGraph.PassGraph[I].CopyConnectivityConstraints);
    LQuotaCounts[I] := Length(AGraph.PassGraph[I].CopyValueQuotaConstraints);
  end;

  for I := 0 to Length(AReport.Trace) - 1 do
  begin
    LEvent := AReport.Trace[I];
    if LEvent.EventId <> I then
      Exit(InvalidTrace(AValidation, gtvikEventId, I,
        LEvent.PassIndex));
    if (LEvent.CauseEventId < -1) or
        (LEvent.CauseEventId >= I) then
      Exit(InvalidTrace(AValidation, gtvikCauseEventId, I,
        LEvent.PassIndex));
    if not IsValidTraceEventKind(LEvent.Kind) then
      Exit(InvalidTrace(AValidation, gtvikEventKind, I,
        LEvent.PassIndex));
    if not IsValidTraceCauseKind(LEvent.CauseKind) then
      Exit(InvalidTrace(AValidation, gtvikCauseKind, I,
        LEvent.PassIndex));

    if IsPipelineEventKind(LEvent.Kind) then
    begin
      if LEvent.PassIndex <> -1 then
        Exit(InvalidTrace(AValidation, gtvikPassIndex, I,
          LEvent.PassIndex));
    end
    else if (LEvent.PassIndex < 0) or
        (LEvent.PassIndex >= LPassCount) then
      Exit(InvalidTrace(AValidation, gtvikPassIndex, I,
        LEvent.PassIndex));

    if (LEvent.EntryIndex < -1) or
        (LEvent.EntryIndex >= LEntryCount) then
      Exit(InvalidTrace(AValidation, gtvikEntryIndex, I,
        LEvent.PassIndex));

    if LEvent.CauseKind in [gtckConnectivity, gtckValueQuota] then
    begin
      if LEvent.PassIndex < 0 then
        Exit(InvalidTrace(AValidation, gtvikConstraintIndex, I, LEvent.PassIndex));
      if LEvent.CauseKind = gtckConnectivity then
        LConstraintCount := LConstraintCounts[LEvent.PassIndex]
      else
        LConstraintCount := LQuotaCounts[LEvent.PassIndex];
      if not ((LEvent.ConstraintIndex >= 0)
        and (LEvent.ConstraintIndex < LConstraintCount)) then
        Exit(InvalidTrace(AValidation, gtvikConstraintIndex, I, LEvent.PassIndex));
      {$IFDEF PAS2JS}
      if LEvent.ConstraintIndex <> Trunc(LEvent.ConstraintIndex) then
        Exit(InvalidTrace(AValidation, gtvikConstraintIndex, I, LEvent.PassIndex));
      {$ENDIF}
      if not (LEvent.Kind in [gtekDecision, gtekCandidateRemoved,
        gtekContradiction]) or LEvent.HasDirection
        or (LEvent.NeighborIndex <> -1) or (LEvent.DependencyPassIndex <> -1) then
        Exit(InvalidTrace(AValidation, gtvikEventFields, I, LEvent.PassIndex));
      if (LEvent.CauseKind = gtckValueQuota) and
        (LEvent.Kind = gtekContradiction) and
        ((LEvent.EntryIndex <> -1) or (LEvent.DomainCountBefore <> 0) or
          (LEvent.DomainCountAfter <> 0)) then
        Exit(InvalidTrace(AValidation, gtvikEventFields, I, LEvent.PassIndex));
      if (LEvent.CauseEventId >= 0) and
        (AReport.Trace[LEvent.CauseEventId].PassIndex <> LEvent.PassIndex) then
        Exit(InvalidTrace(AValidation, gtvikCausalLink, I, LEvent.PassIndex));
    end
    else if LEvent.ConstraintIndex <> -1 then
      Exit(InvalidTrace(AValidation, gtvikConstraintIndex, I, LEvent.PassIndex));
    if (LEvent.NeighborIndex < -1) or
        (LEvent.NeighborIndex >= LEntryCount) then
      Exit(InvalidTrace(AValidation, gtvikNeighborIndex, I,
        LEvent.PassIndex));

    if LEvent.ValueIndex < -1 then
      Exit(InvalidTrace(AValidation, gtvikValueIndex, I,
        LEvent.PassIndex));
    if LEvent.ValueIndex < 0 then
    begin
      if LEvent.Value <> '' then
        Exit(InvalidTrace(AValidation, gtvikValue, I,
          LEvent.PassIndex));
    end
    else
    begin
      if (LEvent.PassIndex < 0) or
          (LEvent.ValueIndex >= Length(LValues[LEvent.PassIndex])) then
        Exit(InvalidTrace(AValidation, gtvikValueIndex, I,
          LEvent.PassIndex));
      if LEvent.Value <> LValues[LEvent.PassIndex][LEvent.ValueIndex] then
        Exit(InvalidTrace(AValidation, gtvikValue, I,
          LEvent.PassIndex));
    end;

    if LEvent.HasDirection then
    begin
      if not IsValidDirection(LEvent.Direction) then
        Exit(InvalidTrace(AValidation, gtvikDirection, I,
          LEvent.PassIndex));
    end
    else if LEvent.Direction <> gdNorth then
      Exit(InvalidTrace(AValidation, gtvikDirection, I,
        LEvent.PassIndex));

    if (LEvent.DependencyPassIndex < -1) or
        (LEvent.DependencyPassIndex >= LPassCount) then
      Exit(InvalidTrace(AValidation, gtvikDependencyPassIndex, I,
        LEvent.PassIndex));
    if (LEvent.DependencyPassIndex >= 0) and
        ((LEvent.PassIndex < 0) or
          (not HasDeclaredDependency(AGraph, LEvent.PassIndex,
            LEvent.DependencyPassIndex))) then
      Exit(InvalidTrace(AValidation, gtvikDependencyPassIndex, I,
        LEvent.PassIndex));

    if LEvent.DecisionDepth < 0 then
      Exit(InvalidTrace(AValidation, gtvikDecisionDepth, I,
        LEvent.PassIndex));
    if (LEvent.DomainCountBefore < 0) or
        (LEvent.DomainCountAfter < 0) then
      Exit(InvalidTrace(AValidation, gtvikDomainCount, I,
        LEvent.PassIndex));
    if IsRemovalEventKind(LEvent.Kind) and
        ((LEvent.DomainCountBefore <= 0) or
          (LEvent.DomainCountAfter < 0) or
          (LEvent.DomainCountAfter <> Pred(LEvent.DomainCountBefore))) then
      Exit(InvalidTrace(AValidation, gtvikDomainCount, I,
        LEvent.PassIndex));
    if (LEvent.Kind = gtekCandidateRestored) and
        ((LEvent.DomainCountBefore < 0) or
          (LEvent.DomainCountBefore = High(Integer)) or
          (LEvent.DomainCountAfter <> Succ(LEvent.DomainCountBefore))) then
      Exit(InvalidTrace(AValidation, gtvikDomainCount, I,
        LEvent.PassIndex));
    if (LEvent.Kind in [gtekDecision, gtekContradiction,
        gtekBacktrack]) and
        (LEvent.DomainCountAfter <> LEvent.DomainCountBefore) then
      Exit(InvalidTrace(AValidation, gtvikDomainCount, I,
        LEvent.PassIndex));

    if EventRequiresEntry(LEvent.Kind)
      and not ((LEvent.Kind = gtekContradiction)
        and ((LEvent.CauseKind = gtckExactAssignmentExclusion)
          or ((LEvent.CauseKind = gtckValueQuota)
            and (LEvent.EntryIndex = -1))
          or ((LEvent.CauseKind = gtckFinalValidation)
            and (LEvent.EntryIndex = -1)))) then
    begin
      if LEvent.EntryIndex < 0 then
        Exit(InvalidTrace(AValidation, gtvikEventFields, I,
          LEvent.PassIndex));
    end
    else if LEvent.EntryIndex <> -1 then
      Exit(InvalidTrace(AValidation, gtvikEventFields, I,
        LEvent.PassIndex));

    if EventRequiresValue(LEvent.Kind) then
    begin
      if LEvent.ValueIndex < 0 then
        Exit(InvalidTrace(AValidation, gtvikEventFields, I,
          LEvent.PassIndex));
    end
    else if LEvent.ValueIndex <> -1 then
      Exit(InvalidTrace(AValidation, gtvikEventFields, I,
        LEvent.PassIndex));

    if (IsPipelineEventKind(LEvent.Kind) or
        IsPassMetadataEventKind(LEvent.Kind)) and
        ((LEvent.NeighborIndex <> -1) or LEvent.HasDirection or
          (LEvent.DependencyPassIndex <> -1) or
          (LEvent.DecisionDepth <> 0) or
          (LEvent.DomainCountBefore <> 0) or
          (LEvent.DomainCountAfter <> 0)) then
      Exit(InvalidTrace(AValidation, gtvikEventFields, I,
        LEvent.PassIndex));

    //Cause ids are more than backward references: for the direct causal
    //relationships guaranteed by schema version 1, validate the referenced
    //event's role as well. This rejects a report that was edited and rehashed
    //into a numerically well-formed but causally misleading stream.
    case LEvent.Kind of
      gtekPassBegin,
      gtekPassSkipped:
        if (LEvent.CauseKind <> gtckTransaction) or
            (LEvent.CauseEventId <> -1) then
          Exit(InvalidTrace(AValidation, gtvikCausalLink, I,
            LEvent.PassIndex));
      gtekInitialCandidateRemoved:
        case LEvent.CauseKind of
          gtckCallerDomain,
          gtckCallerLock:
            if (LEvent.CauseEventId <> -1) or
                (LEvent.DependencyPassIndex <> -1) then
              Exit(InvalidTrace(AValidation, gtvikCausalLink, I,
                LEvent.PassIndex));
          gtckPassDependency:
            if (LEvent.CauseEventId < 0) or
                (LEvent.DependencyPassIndex < 0) or
                (AReport.Trace[LEvent.CauseEventId].PassIndex <>
                  LEvent.DependencyPassIndex) or
                (not (AReport.Trace[LEvent.CauseEventId].Kind in
                  [gtekPassStaged, gtekPassSkipped])) then
              Exit(InvalidTrace(AValidation, gtvikCausalLink, I,
                LEvent.PassIndex));
        else
          Exit(InvalidTrace(AValidation, gtvikCausalLink, I,
            LEvent.PassIndex));
        end;
      gtekDecision:
        if LEvent.CauseEventId < 0 then
        begin
          if LEvent.CauseKind <> gtckNone then
            Exit(InvalidTrace(AValidation, gtvikCausalLink, I,
              LEvent.PassIndex));
        end
        else if (LEvent.CauseKind <>
              AReport.Trace[LEvent.CauseEventId].CauseKind) or
            (LEvent.ConstraintIndex <>
              AReport.Trace[LEvent.CauseEventId].ConstraintIndex) or
            (AReport.Trace[LEvent.CauseEventId].PassIndex <>
              LEvent.PassIndex) or
            (AReport.Trace[LEvent.CauseEventId].EntryIndex <>
              LEvent.EntryIndex) then
          Exit(InvalidTrace(AValidation, gtvikCausalLink, I,
            LEvent.PassIndex));
      gtekCandidateRemoved:
        begin
          if not (LEvent.CauseKind in [gtckDecision, gtckAdjacency,
              gtckRequiredSupport, gtckConnectivity, gtckValueQuota]) then
            Exit(InvalidTrace(AValidation, gtvikCausalLink, I,
              LEvent.PassIndex));
          if (LEvent.CauseKind = gtckDecision) and
              ((LEvent.CauseEventId < 0) or
                (AReport.Trace[LEvent.CauseEventId].Kind <>
                  gtekDecision) or
                (AReport.Trace[LEvent.CauseEventId].PassIndex <>
                  LEvent.PassIndex) or
                (AReport.Trace[LEvent.CauseEventId].EntryIndex <>
                  LEvent.EntryIndex)) then
            Exit(InvalidTrace(AValidation, gtvikCausalLink, I,
              LEvent.PassIndex));
          if (LEvent.CauseKind = gtckAdjacency) and
              ((LEvent.NeighborIndex < 0) or
                (not LEvent.HasDirection)) then
            Exit(InvalidTrace(AValidation, gtvikCausalLink, I,
              LEvent.PassIndex));
          if (LEvent.CauseKind = gtckRequiredSupport) and
              ((LEvent.NeighborIndex <> -1) or
              LEvent.HasDirection) then
            Exit(InvalidTrace(AValidation, gtvikCausalLink, I,
              LEvent.PassIndex));
        end;
      gtekContradiction:
        if (LEvent.CauseKind = gtckExactAssignmentExclusion)
          and ((LEvent.EntryIndex <> -1)
            or (LEvent.NeighborIndex <> -1)
            or LEvent.HasDirection
            or (LEvent.DependencyPassIndex <> -1)
            or ((LEvent.CauseEventId >= 0)
              and (AReport.Trace[LEvent.CauseEventId].PassIndex <>
                LEvent.PassIndex))) then
          Exit(InvalidTrace(AValidation, gtvikCausalLink, I,
            LEvent.PassIndex));
      gtekBacktrack:
        if (LEvent.CauseKind <> gtckBacktrack) or
            (LEvent.CauseEventId < 0) or
            (AReport.Trace[LEvent.CauseEventId].Kind <>
              gtekContradiction) or
            (AReport.Trace[LEvent.CauseEventId].PassIndex <>
              LEvent.PassIndex) then
          Exit(InvalidTrace(AValidation, gtvikCausalLink, I,
            LEvent.PassIndex));
      gtekCandidateRestored:
        if (LEvent.CauseKind <> gtckBacktrack) or
            (LEvent.CauseEventId < 0) or
            (AReport.Trace[LEvent.CauseEventId].Kind <> gtekBacktrack) or
            (AReport.Trace[LEvent.CauseEventId].PassIndex <>
              LEvent.PassIndex) then
          Exit(InvalidTrace(AValidation, gtvikCausalLink, I,
            LEvent.PassIndex));
      gtekPassStaged,
      gtekPassFailed:
        if LEvent.CauseKind <> gtckTransaction then
          Exit(InvalidTrace(AValidation, gtvikCausalLink, I,
            LEvent.PassIndex));
      gtekPipelineCommit,
      gtekPipelineRollback:
        if (LEvent.CauseKind <> gtckTransaction) or
            (LEvent.CauseEventId <> Pred(I)) then
          Exit(InvalidTrace(AValidation, gtvikCausalLink, I, -1));
    end;

    Inc(AValidation.CheckedEvents);
  end;

  if AChronological then
  begin
    if not ValidateTraceLifecycle(AGraph, AReport, AValidation) then Exit(False);
  end
  else
  begin
    SetLength(LCovered, Length(AReport.Trace));
    for I := 0 to LPassCount - 1 do
    begin
      if AReport.Passes[I].TraceCount < 0 then
        Exit(InvalidTrace(AValidation, gtvikPassSlice, -1, I));
      if AReport.Passes[I].TraceCount = 0 then
      begin
        if AReport.Passes[I].TraceStart <> -1 then
          Exit(InvalidTrace(AValidation, gtvikPassSlice, -1, I));
        Continue;
      end;
      if AReport.Passes[I].TraceStart < 0 then
        Exit(InvalidTrace(AValidation, gtvikPassSlice, -1, I));
      if AReport.Passes[I].TraceStart >
        Length(AReport.Trace) - AReport.Passes[I].TraceCount then
        Exit(InvalidTrace(AValidation, gtvikPassSlice, -1, I));
      LSliceEnd := AReport.Passes[I].TraceStart + AReport.Passes[I].TraceCount;
      for J := AReport.Passes[I].TraceStart to LSliceEnd - 1 do
      begin
        if (AReport.Trace[J].PassIndex <> I) or
          IsPipelineEventKind(AReport.Trace[J].Kind) or
          (LCovered[J] <> 0) then
          Exit(InvalidTrace(AValidation, gtvikPassSlice, J, I));
        LCovered[J] := 1;
      end;
    end;
  end;

  for I := 0 to Length(AReport.Trace) - 1 do
    if IsPipelineEventKind(AReport.Trace[I].Kind) then
    begin
      if (I <> High(AReport.Trace)) or
        ((not AChronological) and (LCovered[I] <> 0)) then
        Exit(InvalidTrace(AValidation, gtvikTerminalEvent, I, -1));
    end
    else if not AChronological then
      if LCovered[I] = 0 then
        Exit(InvalidTrace(AValidation, gtvikPassSlice, I,
          AReport.Trace[I].PassIndex));

  case AReport.Status of
    gssSolved: LExpectedTerminal := gtekPipelineCommit;
    gssContradiction,
    gssBacktrackLimit: LExpectedTerminal := gtekPipelineRollback;
  else
    Exit(InvalidTrace(AValidation, gtvikTerminalEvent,
      High(AReport.Trace), -1));
  end;
  if AReport.Trace[High(AReport.Trace)].Kind <> LExpectedTerminal then
    Exit(InvalidTrace(AValidation, gtvikTerminalEvent,
      High(AReport.Trace), -1));
  if AReport.TraceHash <> CalculateGraphTraceHash(AReport) then
    Exit(InvalidTrace(AValidation, gtvikTraceHash, -1, -1));

  Result := True;
end;

function ValidateGraphTrace(const AGraph: TGraph;
  const AReport: TGraphSolveReport;
  out AValidation: TGraphTraceValidationReport): Boolean;
begin
  Result := ValidateGraphTraceInternal(AGraph, AReport, False, AValidation);
end;

function TryBuildGraphTraceLayout(const AGraph: TGraph;
  const AReport: TGraphSolveReport; out ALayout: TGraphTraceLayout;
  out AValidation: TGraphTraceValidationReport): Boolean;
var LLayout: TGraphTraceLayout;
begin
  ALayout := Default(TGraphTraceLayout);
  Result := ValidateGraphTraceInternal(AGraph, AReport, True, AValidation);
  if Result then
  begin
    BuildTraceLayout(AReport, LLayout);
    ALayout := LLayout;
  end;
end;

function ValidateGraphTraceLayout(const AGraph: TGraph;
  const AReport: TGraphSolveReport; const ALayout: TGraphTraceLayout;
  out AValidation: TGraphTraceValidationReport): Boolean;
var LExpected: TGraphTraceLayout; P, R: Integer;
begin
  if not TryBuildGraphTraceLayout(AGraph, AReport, LExpected,
    AValidation) then Exit(False);
  if (ALayout.Version <> LExpected.Version) or
    not IsTraceBoolean(ALayout.TraceCaptured) or
    (ALayout.TraceCaptured <> LExpected.TraceCaptured) or
    (ALayout.TraceHash <> LExpected.TraceHash) or
    (ALayout.EventCount <> LExpected.EventCount) or
    (ALayout.TerminalEventIndex <> LExpected.TerminalEventIndex) or
    (Length(ALayout.Passes) <> Length(LExpected.Passes)) then
    Exit(InvalidTrace(AValidation, gtvikTraceLayout, -1, -1));
  for P := 0 to High(LExpected.Passes) do
  begin
    if (ALayout.Passes[P].EventCount <> LExpected.Passes[P].EventCount) or
      (Length(ALayout.Passes[P].Ranges) <> Length(LExpected.Passes[P].Ranges)) then
      Exit(InvalidTrace(AValidation, gtvikTraceLayout, -1, P));
    for R := 0 to High(LExpected.Passes[P].Ranges) do
      if (ALayout.Passes[P].Ranges[R].Start <> LExpected.Passes[P].Ranges[R].Start) or
        (ALayout.Passes[P].Ranges[R].Count <> LExpected.Passes[P].Ranges[R].Count) then
        Exit(InvalidTrace(AValidation, gtvikTraceLayout, -1, P));
  end;
  Result := True;
end;

function CopyGraphTraceLayout(
  const ASource: TGraphTraceLayout): TGraphTraceLayout;
var P, R: Integer;
begin
  Result := ASource;
  Result.Passes := nil;
  SetLength(Result.Passes, Length(ASource.Passes));
  for P := 0 to High(ASource.Passes) do
  begin
    Result.Passes[P].EventCount := ASource.Passes[P].EventCount;
    SetLength(Result.Passes[P].Ranges, Length(ASource.Passes[P].Ranges));
    for R := 0 to High(ASource.Passes[P].Ranges) do
      Result.Passes[P].Ranges[R] := ASource.Passes[P].Ranges[R];
  end;
end;

function DescribeGraphTraceValidationIssue(
  const AIssue: TGraphTraceValidationIssue): String;
begin
  if not IsValidValidationIssueKind(AIssue.Kind) then
    Exit('unknown trace validation issue');
  if AIssue.Kind = gtvikNone then
    Exit('trace is valid');
  Result := GraphTraceValidationIssueKindName(AIssue.Kind);
  if AIssue.EventIndex >= 0 then
    Result := Result + ' at event ' + IntToStr(AIssue.EventIndex);
  if AIssue.PassIndex >= 0 then
    Result := Result + ' in pass ' + IntToStr(AIssue.PassIndex);
end;

end.
