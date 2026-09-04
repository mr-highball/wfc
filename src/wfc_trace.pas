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
    gtvikTerminalEvent
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

function ValidateGraphTrace(const AGraph: TGraph;
  const AReport: TGraphSolveReport;
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
begin
  InitializeValidation(AValidation);
  if not Assigned(AGraph) then
    Exit(InvalidTrace(AValidation, gtvikGraph, -1, -1));

  LPassCount := AGraph.TotalPassCount;
  if Length(AReport.Passes) <> LPassCount then
    Exit(InvalidTrace(AValidation, gtvikPassCount, -1, -1));
  if not TryDimensionEntryCount(AGraph.Dimension, LEntryCount) then
    Exit(InvalidTrace(AValidation, gtvikGraph, -1, -1));

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
  for I := 0 to LPassCount - 1 do
    LValues[I] := AGraph.PassGraph[I].CopyRegisteredValues;

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

    if EventRequiresEntry(LEvent.Kind) then
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
            (AReport.Trace[LEvent.CauseEventId].PassIndex <>
              LEvent.PassIndex) or
            (AReport.Trace[LEvent.CauseEventId].EntryIndex <>
              LEvent.EntryIndex) then
          Exit(InvalidTrace(AValidation, gtvikCausalLink, I,
            LEvent.PassIndex));
      gtekCandidateRemoved:
        begin
          if not (LEvent.CauseKind in [gtckDecision, gtckAdjacency,
              gtckRequiredSupport]) then
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
    LSliceEnd := AReport.Passes[I].TraceStart +
      AReport.Passes[I].TraceCount;
    for J := AReport.Passes[I].TraceStart to LSliceEnd - 1 do
    begin
      if (AReport.Trace[J].PassIndex <> I) or
          IsPipelineEventKind(AReport.Trace[J].Kind) or
          (LCovered[J] <> 0) then
        Exit(InvalidTrace(AValidation, gtvikPassSlice, J, I));
      LCovered[J] := 1;
    end;
  end;

  for I := 0 to Length(AReport.Trace) - 1 do
    if IsPipelineEventKind(AReport.Trace[I].Kind) then
    begin
      if (I <> High(AReport.Trace)) or (LCovered[I] <> 0) then
        Exit(InvalidTrace(AValidation, gtvikTerminalEvent, I, -1));
    end
    else if LCovered[I] = 0 then
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
