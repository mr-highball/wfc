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
unit wfc_trace_stream;

{$mode delphi}{$H+}

interface

uses SysUtils, wfc;

const
  WFC_TRACE_WINDOW_VERSION = 1;

type
  EWfcTraceStream = class(Exception);

  //A bounded chronological suffix, not a rewritten or silently truncated
  //Trace-v1 report. Complete attests delivery, not graph-model validation.
  TGraphTraceWindowSink = class(TGraphTraceSink)
  strict private
    FCapacity, FStart, FCount, FProduced: Integer;
    FEvents: TGraphTraceEvents;
    FHeader: TGraphTraceHeader;
    FDelivery: TGraphTraceDelivery;
    FActive, FEnded, FTerminalSeen: Boolean;
    function GetComplete: Boolean;
    function GetDroppedEventCount: Integer;
    procedure Reject(const AMessage: String;
      const APhase: TGraphTraceDeliveryPhase);
  public
    constructor Create(const ACapacity: Integer);
    procedure BeginTrace(const AHeader: TGraphTraceHeader); override;
    procedure AppendEvent(const AEvent: TGraphTraceEvent); override;
    procedure EndTrace(const ADelivery: TGraphTraceDelivery); override;
    function CopyEvents: TGraphTraceEvents;
    property Capacity: Integer read FCapacity;
    property Header: TGraphTraceHeader read FHeader;
    property Delivery: TGraphTraceDelivery read FDelivery;
    property RetainedEventCount: Integer read FCount;
    property DroppedEventCount: Integer read GetDroppedEventCount;
    property Complete: Boolean read GetComplete;
  end;

implementation

function IsExactInteger(const AValue, AMinimum, AMaximum: Double): Boolean;
begin
  Result := (AValue >= AMinimum) and (AValue <= AMaximum);
  {$IFDEF PAS2JS}
  Result := Result and (AValue = Trunc(AValue));
  {$ENDIF}
end;

constructor TGraphTraceWindowSink.Create(const ACapacity: Integer);
begin
  inherited Create;
  if not IsExactInteger(ACapacity, 0, High(Integer)) then
    raise ERangeError.Create('trace window capacity must be a nonnegative Integer');
  FCapacity := ACapacity;
  SetLength(FEvents, FCapacity);
  FDelivery.FailureEventId := -1;
end;

function TGraphTraceWindowSink.GetComplete: Boolean;
begin
  Result := FEnded and (FDelivery.Status = gtdsComplete)
    and (GetDroppedEventCount = 0);
end;

function TGraphTraceWindowSink.GetDroppedEventCount: Integer;
begin
  Result := FProduced - FCount;
end;

procedure TGraphTraceWindowSink.Reject(const AMessage: String;
  const APhase: TGraphTraceDeliveryPhase);
begin
  FActive := False;
  FEnded := False;
  FDelivery := Default(TGraphTraceDelivery);
  FDelivery.Version := WFC_TRACE_DELIVERY_VERSION;
  FDelivery.Status := gtdsSinkFailed;
  FDelivery.ProducedEventCount := FProduced;
  FDelivery.DeliveredEventCount := FProduced;
  FDelivery.FailurePhase := APhase;
  FDelivery.FailureEventId := -1;
  if APhase = gtdpEvent then FDelivery.FailureEventId := FProduced;
  FDelivery.FailureMessage := AMessage;
  raise EWfcTraceStream.Create(AMessage);
end;

procedure TGraphTraceWindowSink.BeginTrace(const AHeader: TGraphTraceHeader);
var I, LIndex: Integer;
begin
  if FActive then Reject('trace window begin overlaps an active transaction', gtdpBegin);
  if (AHeader.DeliveryVersion <> WFC_TRACE_DELIVERY_VERSION)
    or (AHeader.TraceVersion <> WFC_TRACE_VERSION)
    or (AHeader.TraceHashVersion <> WFC_TRACE_HASH_VERSION)
    or not IsExactInteger(AHeader.PassCount, 0, High(Integer))
    or not IsExactInteger(AHeader.Seed, 0, 4294967295.0)
    or not IsExactInteger(AHeader.RandomAlgorithmVersion, 0, High(Integer))
    or not IsExactInteger(AHeader.SolverAlgorithmVersion, 0, High(Integer))
    or not IsExactInteger(AHeader.GraphModelVersion, 0, High(Integer))
    or not IsExactInteger(AHeader.PipelineAlgorithmVersion, 0, High(Integer)) then
    Reject('trace window header is invalid or unsupported', gtdpBegin);

  //Release retained strings even if the next attempt emits fewer events.
  LIndex := FStart;
  for I := 0 to FCount - 1 do
  begin
    FEvents[LIndex] := Default(TGraphTraceEvent);
    Inc(LIndex);
    if LIndex = FCapacity then LIndex := 0;
  end;
  FStart := 0;
  FCount := 0;
  FProduced := 0;
  FHeader := AHeader;
  FDelivery := Default(TGraphTraceDelivery);
  FDelivery.FailureEventId := -1;
  FEnded := False;
  FTerminalSeen := False;
  FActive := True;
end;

procedure TGraphTraceWindowSink.AppendEvent(const AEvent: TGraphTraceEvent);
var LIndex: Integer; LTerminal: Boolean;
begin
  if not FActive then Reject('trace window event requires an active transaction', gtdpEvent);
  if FTerminalSeen then Reject('trace window event follows a terminal event', gtdpEvent);
  if FProduced = High(Integer) then
    Reject('trace window event count exceeds Integer capacity', gtdpEvent);
  if not IsExactInteger(AEvent.EventId, 0, High(Integer))
    or (AEvent.EventId <> FProduced)
    or not IsExactInteger(AEvent.CauseEventId, -1, FProduced - 1)
    or not IsExactInteger(Ord(AEvent.Kind), Ord(Low(TGraphTraceEventKind)),
      Ord(High(TGraphTraceEventKind)))
    or not IsExactInteger(AEvent.PassIndex, -1, FHeader.PassCount - 1) then
    Reject('trace window event identity or ordering is invalid', gtdpEvent);
  LTerminal := AEvent.Kind in [gtekPipelineCommit, gtekPipelineRollback];
  if LTerminal then
  begin
    if (AEvent.PassIndex <> -1) or (AEvent.CauseKind <> gtckTransaction)
      or (AEvent.CauseEventId <> FProduced - 1) then
      Reject('trace window terminal event is invalid', gtdpEvent);
  end
  else if AEvent.PassIndex < 0 then
    Reject('trace window pass event has no pass', gtdpEvent);

  if FCapacity <> 0 then
  begin
    if FCount < FCapacity then
    begin
      //The start stays zero until the ring is full. Avoid adding two
      //potentially large Integer indices when choosing the write slot.
      LIndex := FCount;
    end
    else
      LIndex := FStart;
    FEvents[LIndex] := AEvent;
    if FCount < FCapacity then
      Inc(FCount)
    else
    begin
      Inc(FStart);
      if FStart = FCapacity then FStart := 0;
    end;
  end;
  Inc(FProduced);
  FTerminalSeen := LTerminal;
end;

procedure TGraphTraceWindowSink.EndTrace(const ADelivery: TGraphTraceDelivery);
begin
  if not FActive then Reject('trace window end requires an active transaction', gtdpEnd);
  if (ADelivery.Version <> WFC_TRACE_DELIVERY_VERSION)
    or not IsExactInteger(ADelivery.ProducedEventCount, 0, High(Integer))
    or not IsExactInteger(ADelivery.DeliveredEventCount, 0, High(Integer))
    or (ADelivery.ProducedEventCount <> FProduced)
    or (ADelivery.DeliveredEventCount <> FProduced)
    or not IsExactInteger(ADelivery.TraceHash, 0, 4294967295.0)
    or not ((ADelivery.Status = gtdsComplete) or (ADelivery.Status = gtdsInterrupted))
    or (ADelivery.FailurePhase <> gtdpNone)
    or (ADelivery.FailureEventId <> -1)
    or (ADelivery.FailureMessage <> '') then
    Reject('trace window delivery footer is inconsistent', gtdpEnd);
  if (ADelivery.Status = gtdsComplete) and not FTerminalSeen then
    Reject('complete trace delivery has no terminal event', gtdpEnd);
  FDelivery := ADelivery;
  FActive := False;
  FEnded := True;
end;

function TGraphTraceWindowSink.CopyEvents: TGraphTraceEvents;
var I, LIndex: Integer;
begin
  Result := nil;
  SetLength(Result, FCount);
  LIndex := FStart;
  for I := 0 to FCount - 1 do
  begin
    Result[I] := FEvents[LIndex];
    Inc(LIndex);
    if LIndex = FCapacity then LIndex := 0;
  end;
end;

end.
