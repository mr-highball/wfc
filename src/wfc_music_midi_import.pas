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
unit wfc_music_midi_import;

{$mode delphi}{$H+}

interface

uses
  wfc_music,
  wfc_midi_smf;

const
  WFC_MUSIC_MIDI_IMPORT_VERSION = 1;
  WFC_MUSIC_MIDI_IMPORT_MAX_BYTES = 16 * 1024 * 1024;
  WFC_MUSIC_MIDI_IMPORT_MAX_TRACKS = 256;
  WFC_MUSIC_MIDI_IMPORT_MAX_EVENTS = 131072;
  WFC_MUSIC_MIDI_IMPORT_MAX_NOTES = 65536;
  WFC_MUSIC_MIDI_IMPORT_MAX_VOICES = 256;
  WFC_MUSIC_MIDI_IMPORT_MAX_SPANS = 131328;
  WFC_MUSIC_MIDI_IMPORT_MAX_TIMING_EVENTS = 4096;

type
  EWfcMusicMidiImport = class(EWfcMusic);

  TWfcMusicMidiUnsupportedPolicy = (
    wmmupReject,
    wmmupIgnoreAndReport
  );
  TWfcMusicMidiEndPolicy = (
    wmmepRequireMeasure,
    wmmepPadMeasure
  );

  TWfcMusicMidiImportOptions = record
    UnsupportedEvents: TWfcMusicMidiUnsupportedPolicy;
    EndPolicy: TWfcMusicMidiEndPolicy;
  end;

  TWfcMusicMidiVoiceSource = record
    SourceTrack: Integer;
    Channel: Integer;
    Lane: Integer;
  end;
  TWfcMusicMidiVoiceSources = array of TWfcMusicMidiVoiceSource;

  TWfcMusicMidiImportReport = record
    SourceFormat: Integer;
    SourceTrackCount: Integer;
    { Includes the structural end-of-track event of every source track. }
    SourceEventCount: Integer;
    NoteCount: Integer;
    SourceLengthTicks: Integer;
    ScoreLengthTicks: Integer;
    PaddingTicks: Integer;
    IgnoredChannelEvents: Integer;
    IgnoredSystemEvents: Integer;
    { Includes harmless metadata and accepted meters whose click byte is
      not 24: that metronome setting is not represented in the score. }
    IgnoredMetaEvents: Integer;
    OmittedTrackNames: Integer;
    DiscardedReleaseVelocities: Integer;
    { Raw timing events at the global source end are omitted. They are not
      also counted as redundant; same-tick conflicts still reject. }
    TerminalTimingEvents: Integer;
    RedundantTimingEvents: Integer;
    UsedDefaultTempo: Boolean;
    UsedDefaultMeter: Boolean;
    AddedSilentVoice: Boolean;
    { Voice order is first note-on encounter order. Channel is zero-based;
      -1 denotes the synthetic voice used for a positive silent timeline. }
    Voices: TWfcMusicMidiVoiceSources;
  end;

function DefaultWfcMusicMidiImportOptions: TWfcMusicMidiImportOptions;

{ Import format 0/1, PPQ, note-on/off MIDI into exact monophonic lanes.
  Global event order is (absolute tick, source track, source event index).
  Notes pair by global (channel, pitch); the note-on owns the source track.
  Same-key overlaps are ambiguous and reject, rather than guessing a pairing.
  Input records are preflighted before timeline/note allocation. The byte cap
  also applies to their canonical encoding without running status.

  Port, device-name, channel-prefix and SMPTE-offset metadata always reject.
  Meter notation must use eight thirty-seconds per MIDI quarter. Unsupported
  performance/opaque events reject unless explicitly ignored and reported.
  Output score is caller-owned; report is detached. Rejection resets report. }
function ImportWfcMusicMidi(const AFile: TWfcMidiFile;
  const AOptions: TWfcMusicMidiImportOptions;
  out AReport: TWfcMusicMidiImportReport): TWfcMusicScore;

function DecodeWfcMusicMidi(const ABytes: TWfcMidiBytes;
  const AOptions: TWfcMusicMidiImportOptions;
  out AReport: TWfcMusicMidiImportReport): TWfcMusicScore;

implementation

uses
  SysUtils;

type
  TImportEvent = record
    Tick: Integer;
    Track: Integer;
    EventIndex: Integer;
  end;
  TImportEvents = array of TImportEvent;

  TImportNote = record
    Track: Integer;
    Channel: Integer;
    Pitch: Integer;
    Velocity: Integer;
    StartTick: Integer;
    EndTick: Integer;
    Next: Integer;
  end;
  TImportNotes = array of TImportNote;

  TImportLane = record
    Source: TWfcMusicMidiVoiceSource;
    EndTick: Integer;
    FirstNote: Integer;
    LastNote: Integer;
  end;
  TImportLanes = array of TImportLane;

procedure ImportError(const AMessage: String);
begin
  raise EWfcMusicMidiImport.Create('cannot import MIDI into WFC music: ' +
    AMessage);
end;

procedure ResetReport(out AReport: TWfcMusicMidiImportReport);
begin
  AReport.SourceFormat := 0;
  AReport.SourceTrackCount := 0;
  AReport.SourceEventCount := 0;
  AReport.NoteCount := 0;
  AReport.SourceLengthTicks := 0;
  AReport.ScoreLengthTicks := 0;
  AReport.PaddingTicks := 0;
  AReport.IgnoredChannelEvents := 0;
  AReport.IgnoredSystemEvents := 0;
  AReport.IgnoredMetaEvents := 0;
  AReport.OmittedTrackNames := 0;
  AReport.DiscardedReleaseVelocities := 0;
  AReport.TerminalTimingEvents := 0;
  AReport.RedundantTimingEvents := 0;
  AReport.UsedDefaultTempo := False;
  AReport.UsedDefaultMeter := False;
  AReport.AddedSilentVoice := False;
  AReport.Voices := nil;
end;

function DefaultWfcMusicMidiImportOptions: TWfcMusicMidiImportOptions;
begin
  Result.UnsupportedEvents := wmmupReject;
  Result.EndPolicy := wmmepPadMeasure;
end;

procedure ValidateOptions(const AOptions: TWfcMusicMidiImportOptions);
begin
  case AOptions.UnsupportedEvents of
    wmmupReject, wmmupIgnoreAndReport: ;
  else
    ImportError('unknown unsupported-event policy');
  end;
  case AOptions.EndPolicy of
    wmmepRequireMeasure, wmmepPadMeasure: ;
  else
    ImportError('unknown end policy');
  end;
end;

function VariableLengthSize(const AValue: Cardinal): Integer;
begin
  if AValue > WFC_MIDI_MAX_VARIABLE_LENGTH then
    ImportError('delta or payload exceeds the four-byte variable-length range');
  if AValue < $80 then
    Result := 1
  else if AValue < $4000 then
    Result := 2
  else if AValue < $200000 then
    Result := 3
  else
    Result := 4;
end;

procedure AddSourceBytes(var ATotal: Integer; const ACount: Integer);
begin
  if (ACount < 0) or
      (ACount > WFC_MUSIC_MIDI_IMPORT_MAX_BYTES - ATotal) then
    ImportError('canonical source size exceeds the 16 MiB import limit');
  Inc(ATotal, ACount);
end;

function AddTick(const ATick: Integer; const ADelta: Cardinal): Integer;
begin
  if ADelta > WFC_MIDI_MAX_VARIABLE_LENGTH then
    ImportError('delta exceeds the four-byte variable-length range');
  if Integer(ADelta) > High(Integer) - ATick then
    ImportError('absolute source tick exceeds the Integer range');
  Result := ATick + Integer(ADelta);
end;

function MeterDenominator(const AEvent: TWfcMidiEvent): Integer;
begin
  if AEvent.Data[1] > 30 then
    ImportError('meter denominator exceeds the positive Integer range');
  Result := Integer(1) shl AEvent.Data[1];
end;

function MeasureTicks(const ATicksPerQuarter, ANumerator,
  ADenominator: Integer): Integer;
var
  LProduct: Integer;
begin
  { PPQ <= 32767 and MIDI numerator <= 255: this product fits Integer. }
  if (ANumerator < 1) or (ANumerator > 255) or
      (ADenominator < 1) then
    ImportError('meter numerator and denominator must be positive');
  LProduct := ATicksPerQuarter * 4 * ANumerator;
  if (LProduct mod ADenominator) <> 0 then
    ImportError('meter measure length is not an exact number of source ticks');
  Result := LProduct div ADenominator;
  if Result < 1 then
    ImportError('meter measure length must be positive');
end;

function EventTempo(const AEvent: TWfcMidiEvent): Integer;
begin
  Result := Integer(AEvent.Data[0]) * 65536 +
    Integer(AEvent.Data[1]) * 256 + Integer(AEvent.Data[2]);
end;

procedure CountUnsupported(const AOptions: TWfcMusicMidiImportOptions;
  var ACount: Integer; const ALabel: String);
begin
  if AOptions.UnsupportedEvents = wmmupReject then
    ImportError('unsupported ' + ALabel +
      '; dropping it requires the explicit ignore-and-report policy');
  Inc(ACount);
end;

procedure PreflightEvent(const AEvent: TWfcMidiEvent;
  const ATicksPerQuarter: Integer;
  const AOptions: TWfcMusicMidiImportOptions;
  var AReport: TWfcMusicMidiImportReport;
  var ABytes, ATempoCount, AMeterCount: Integer);
var
  I: Integer;
  LLength: Integer;
  LExpected: Integer;
begin
  AddSourceBytes(ABytes, VariableLengthSize(AEvent.DeltaTicks) + 1);
  if Length(AEvent.Data) > WFC_MUSIC_MIDI_IMPORT_MAX_BYTES then
    ImportError('event payload exceeds the 16 MiB import limit');
  LLength := Integer(Length(AEvent.Data));
  AddSourceBytes(ABytes, LLength);
  if (AEvent.Status >= $80) and (AEvent.Status <= $EF) then
  begin
    if AEvent.MetaType <> 0 then
      ImportError('channel events cannot have a meta type');
    if ((AEvent.Status and $F0) = $C0) or
        ((AEvent.Status and $F0) = $D0) then
      LExpected := 1
    else
      LExpected := 2;
    if LLength <> LExpected then
      ImportError('channel event has an invalid data length');
    for I := 0 to LLength - 1 do
      if AEvent.Data[I] >= $80 then
        ImportError('channel event data must contain seven-bit values');
    case AEvent.Status and $F0 of
      $80:
        if AEvent.Data[1] <> 0 then
          Inc(AReport.DiscardedReleaseVelocities);
      $90:
        if AEvent.Data[1] <> 0 then
        begin
          if AReport.NoteCount = WFC_MUSIC_MIDI_IMPORT_MAX_NOTES then
            ImportError('note count exceeds the 65536 import limit');
          Inc(AReport.NoteCount);
        end;
    else
      CountUnsupported(AOptions, AReport.IgnoredChannelEvents,
        'performance channel event');
    end;
    Exit;
  end;

  case AEvent.Status of
    $F0, $F7:
      begin
        if AEvent.MetaType <> 0 then
          ImportError('system-exclusive events cannot have a meta type');
        AddSourceBytes(ABytes, VariableLengthSize(Cardinal(LLength)));
        CountUnsupported(AOptions, AReport.IgnoredSystemEvents,
          'system-exclusive event');
      end;
    $FF:
      begin
        if AEvent.MetaType >= $80 then
          ImportError('meta-event types must be seven-bit values');
        AddSourceBytes(ABytes, 1 + VariableLengthSize(Cardinal(LLength)));
        case AEvent.MetaType of
          $2F:
            ImportError('end-of-track belongs in the track structure');
          $09, $20, $21, $54:
            ImportError('device, channel-prefix, port and SMPTE-offset ' +
              'metadata cannot be represented safely');
          $51:
            begin
              if LLength <> 3 then
                ImportError('tempo event must contain three bytes');
              if EventTempo(AEvent) = 0 then
                ImportError('tempo must be positive');
              if ATempoCount = WFC_MUSIC_MIDI_IMPORT_MAX_TIMING_EVENTS then
                ImportError('raw tempo count exceeds the 4096 import limit');
              Inc(ATempoCount);
            end;
          $58:
            begin
              if LLength <> 4 then
                ImportError('meter event must contain four bytes');
              if AEvent.Data[3] <> 8 then
                ImportError('meter must use eight thirty-seconds per MIDI quarter');
              MeasureTicks(ATicksPerQuarter, AEvent.Data[0],
                MeterDenominator(AEvent));
              if AMeterCount = WFC_MUSIC_MIDI_IMPORT_MAX_TIMING_EVENTS then
                ImportError('raw meter count exceeds the 4096 import limit');
              Inc(AMeterCount);
              if AEvent.Data[2] <> 24 then
                Inc(AReport.IgnoredMetaEvents);
            end;
          $00:
            begin
              if LLength <> 2 then
                ImportError('sequence-number metadata must contain two bytes');
              Inc(AReport.IgnoredMetaEvents);
            end;
          $01, $02, $03, $04, $05, $06, $07, $08:
            begin
              Inc(AReport.IgnoredMetaEvents);
              if AEvent.MetaType = $03 then
                Inc(AReport.OmittedTrackNames);
            end;
          $59:
            begin
              if LLength <> 2 then
                ImportError('key-signature metadata must contain two bytes');
              if ((AEvent.Data[0] > 7) and (AEvent.Data[0] < 249)) or
                  (AEvent.Data[1] > 1) then
                ImportError('key-signature metadata has invalid values');
              Inc(AReport.IgnoredMetaEvents);
            end;
        else
          CountUnsupported(AOptions, AReport.IgnoredMetaEvents,
            'opaque meta event');
        end;
      end;
  else
    ImportError('unsupported or invalid event status');
  end;
end;

procedure Preflight(const AFile: TWfcMidiFile;
  const AOptions: TWfcMusicMidiImportOptions;
  var AReport: TWfcMusicMidiImportReport);
var
  I: Integer;
  J: Integer;
  LBytes: Integer;
  LTick: Integer;
  LCount: Integer;
  LTempoCount: Integer;
  LMeterCount: Integer;
begin
  ValidateOptions(AOptions);
  if (AFile.Format <> 0) and (AFile.Format <> 1) then
    ImportError('only synchronous SMF format 0 or 1 is supported');
  if (Length(AFile.Tracks) < 1) or
      (Length(AFile.Tracks) > WFC_MUSIC_MIDI_IMPORT_MAX_TRACKS) then
    ImportError('source must contain from 1 through 256 tracks');
  if (AFile.Format = 0) and (Length(AFile.Tracks) <> 1) then
    ImportError('SMF format 0 must contain exactly one track');
  if (AFile.TicksPerQuarter < 1) or (AFile.TicksPerQuarter > $7FFF) then
    ImportError('only a positive PPQ time division is supported');

  AReport.SourceFormat := AFile.Format;
  AReport.SourceTrackCount := Integer(Length(AFile.Tracks));
  AReport.SourceEventCount := AReport.SourceTrackCount;
  LBytes := 14;
  LTempoCount := 0;
  LMeterCount := 0;
  for I := 0 to AReport.SourceTrackCount - 1 do
  begin
    if Length(AFile.Tracks[I].Events) >
        WFC_MUSIC_MIDI_IMPORT_MAX_EVENTS - AReport.SourceEventCount then
      ImportError('source event count exceeds the 131072 import limit');
    LCount := Integer(Length(AFile.Tracks[I].Events));
    Inc(AReport.SourceEventCount, LCount);
    AddSourceBytes(LBytes, 8 +
      VariableLengthSize(AFile.Tracks[I].EndDeltaTicks) + 3);
    LTick := 0;
    for J := 0 to LCount - 1 do
    begin
      PreflightEvent(AFile.Tracks[I].Events[J], AFile.TicksPerQuarter,
        AOptions, AReport, LBytes, LTempoCount, LMeterCount);
      LTick := AddTick(LTick, AFile.Tracks[I].Events[J].DeltaTicks);
    end;
    LTick := AddTick(LTick, AFile.Tracks[I].EndDeltaTicks);
    if LTick > AReport.SourceLengthTicks then
      AReport.SourceLengthTicks := LTick;
  end;
  if AReport.SourceLengthTicks < 1 then
    ImportError('source timeline must have positive length');
end;

function EventBeforeOrEqual(const A, B: TImportEvent): Boolean;
begin
  if A.Tick <> B.Tick then
    Exit(A.Tick < B.Tick);
  if A.Track <> B.Track then
    Exit(A.Track < B.Track);
  Result := A.EventIndex <= B.EventIndex;
end;

procedure SortEvents(var AEvents: TImportEvents);
var
  LScratch: TImportEvents;

  procedure SortRange(const AFirst, ALast: Integer);
  var
    LMiddle: Integer;
    LLeft: Integer;
    LRight: Integer;
    LNext: Integer;
    I: Integer;
  begin
    if AFirst >= ALast then
      Exit;
    LMiddle := AFirst + (ALast - AFirst) div 2;
    SortRange(AFirst, LMiddle);
    SortRange(LMiddle + 1, ALast);
    LLeft := AFirst;
    LRight := LMiddle + 1;
    LNext := AFirst;
    while (LLeft <= LMiddle) and (LRight <= ALast) do
    begin
      if EventBeforeOrEqual(AEvents[LLeft], AEvents[LRight]) then
      begin
        LScratch[LNext] := AEvents[LLeft];
        Inc(LLeft);
      end
      else
      begin
        LScratch[LNext] := AEvents[LRight];
        Inc(LRight);
      end;
      Inc(LNext);
    end;
    while LLeft <= LMiddle do
    begin
      LScratch[LNext] := AEvents[LLeft];
      Inc(LLeft);
      Inc(LNext);
    end;
    while LRight <= ALast do
    begin
      LScratch[LNext] := AEvents[LRight];
      Inc(LRight);
      Inc(LNext);
    end;
    for I := AFirst to ALast do
      AEvents[I] := LScratch[I];
  end;

begin
  SetLength(LScratch, Length(AEvents));
  SortRange(0, Length(AEvents) - 1);
end;

function BuildTimeline(const AFile: TWfcMidiFile;
  const AEventCount: Integer): TImportEvents;
var
  I: Integer;
  J: Integer;
  LTick: Integer;
  LCount: Integer;
begin
  Result := nil;
  SetLength(Result, AEventCount - Length(AFile.Tracks));
  LCount := 0;
  for I := 0 to Length(AFile.Tracks) - 1 do
  begin
    LTick := 0;
    for J := 0 to Length(AFile.Tracks[I].Events) - 1 do
    begin
      LTick := AddTick(LTick, AFile.Tracks[I].Events[J].DeltaTicks);
      Result[LCount].Tick := LTick;
      Result[LCount].Track := I;
      Result[LCount].EventIndex := J;
      Inc(LCount);
    end;
  end;
  SortEvents(Result);
end;

function PairNotes(const AFile: TWfcMidiFile;
  const AEvents: TImportEvents; const ANoteCount: Integer): TImportNotes;
var
  LActive: array[0..15, 0..127] of Integer;
  I: Integer;
  J: Integer;
  LCount: Integer;
  LChannel: Integer;
  LPitch: Integer;
  LNote: Integer;
  LEvent: TWfcMidiEvent;
begin
  Result := nil;
  SetLength(Result, ANoteCount);
  for I := 0 to 15 do
    for J := 0 to 127 do
      LActive[I, J] := -1;
  LCount := 0;
  for I := 0 to Length(AEvents) - 1 do
  begin
    LEvent := AFile.Tracks[AEvents[I].Track].Events[AEvents[I].EventIndex];
    if ((LEvent.Status and $F0) <> $80) and
        ((LEvent.Status and $F0) <> $90) then
      Continue;
    LChannel := LEvent.Status and $0F;
    LPitch := LEvent.Data[0];
    if ((LEvent.Status and $F0) = $90) and (LEvent.Data[1] <> 0) then
    begin
      if LActive[LChannel, LPitch] >= 0 then
        ImportError('overlapping note-ons for one global channel/pitch key');
      Result[LCount].Track := AEvents[I].Track;
      Result[LCount].Channel := LChannel;
      Result[LCount].Pitch := LPitch;
      Result[LCount].Velocity := LEvent.Data[1];
      Result[LCount].StartTick := AEvents[I].Tick;
      Result[LCount].EndTick := -1;
      Result[LCount].Next := -1;
      LActive[LChannel, LPitch] := LCount;
      Inc(LCount);
    end
    else
    begin
      LNote := LActive[LChannel, LPitch];
      if LNote < 0 then
        ImportError('note-off has no matching global channel/pitch note-on');
      if AEvents[I].Tick <= Result[LNote].StartTick then
        ImportError('zero-duration notes cannot form a positive score span');
      Result[LNote].EndTick := AEvents[I].Tick;
      LActive[LChannel, LPitch] := -1;
    end;
  end;
  for I := 0 to 15 do
    for J := 0 to 127 do
      if LActive[I, J] >= 0 then
        ImportError('note-on has no matching note-off before source end');
end;

procedure BuildTiming(const AFile: TWfcMidiFile;
  const AEvents: TImportEvents;
  const AOptions: TWfcMusicMidiImportOptions;
  var AReport: TWfcMusicMidiImportReport;
  out AMeters: TWfcMusicMeterChanges;
  out ATempos: TWfcMusicTempoChanges);
var
  I: Integer;
  LTick: Integer;
  LTempo: Integer;
  LNumerator: Integer;
  LDenominator: Integer;
  LTempoCount: Integer;
  LMeterCount: Integer;
  LLastTempoTick: Integer;
  LLastTempoValue: Integer;
  LLastMeterTick: Integer;
  LLastMeterNumerator: Integer;
  LLastMeterDenominator: Integer;
  LMeasure: Integer;
  LRemainder: Integer;
  LEvent: TWfcMidiEvent;
begin
  SetLength(AMeters, WFC_MUSIC_MIDI_IMPORT_MAX_TIMING_EVENTS);
  SetLength(ATempos, WFC_MUSIC_MIDI_IMPORT_MAX_TIMING_EVENTS);
  AMeters[0] := MakeWfcMusicMeterChange(0, 4, 4);
  ATempos[0] := MakeWfcMusicTempoChange(0, 500000);
  LMeterCount := 1;
  LTempoCount := 1;
  AReport.UsedDefaultTempo := True;
  AReport.UsedDefaultMeter := True;
  LLastTempoTick := -1;
  LLastTempoValue := 0;
  LLastMeterTick := -1;
  LLastMeterNumerator := 0;
  LLastMeterDenominator := 0;
  for I := 0 to Length(AEvents) - 1 do
  begin
    LEvent := AFile.Tracks[AEvents[I].Track].Events[AEvents[I].EventIndex];
    if LEvent.Status <> $FF then
      Continue;
    LTick := AEvents[I].Tick;
    if LEvent.MetaType = $51 then
    begin
      LTempo := EventTempo(LEvent);
      if (LLastTempoTick = LTick) and (LLastTempoValue <> LTempo) then
        ImportError('conflicting tempos share one absolute tick');
      LLastTempoTick := LTick;
      LLastTempoValue := LTempo;
      if LTick = AReport.SourceLengthTicks then
      begin
        Inc(AReport.TerminalTimingEvents);
        Continue;
      end;
      if (LTick = 0) and AReport.UsedDefaultTempo then
      begin
        ATempos[0] := MakeWfcMusicTempoChange(0, LTempo);
        AReport.UsedDefaultTempo := False;
      end
      else if ATempos[LTempoCount - 1].MicrosecondsPerQuarter = LTempo then
        Inc(AReport.RedundantTimingEvents)
      else
      begin
        if LTempoCount = WFC_MUSIC_MIDI_IMPORT_MAX_TIMING_EVENTS then
          ImportError('canonical tempo count exceeds the 4096 import limit');
        ATempos[LTempoCount] := MakeWfcMusicTempoChange(LTick, LTempo);
        Inc(LTempoCount);
      end;
    end
    else if LEvent.MetaType = $58 then
    begin
      LNumerator := LEvent.Data[0];
      LDenominator := MeterDenominator(LEvent);
      if (LLastMeterTick = LTick) and
          ((LLastMeterNumerator <> LNumerator) or
           (LLastMeterDenominator <> LDenominator)) then
        ImportError('conflicting meters share one absolute tick');
      LLastMeterTick := LTick;
      LLastMeterNumerator := LNumerator;
      LLastMeterDenominator := LDenominator;
      if LTick = AReport.SourceLengthTicks then
      begin
        Inc(AReport.TerminalTimingEvents);
        Continue;
      end;
      if (LTick = 0) and AReport.UsedDefaultMeter then
      begin
        AMeters[0] := MakeWfcMusicMeterChange(0, LNumerator, LDenominator);
        AReport.UsedDefaultMeter := False;
      end
      else if (AMeters[LMeterCount - 1].Numerator = LNumerator) and
          (AMeters[LMeterCount - 1].Denominator = LDenominator) then
        Inc(AReport.RedundantTimingEvents)
      else
      begin
        LMeasure := MeasureTicks(AFile.TicksPerQuarter,
          AMeters[LMeterCount - 1].Numerator,
          AMeters[LMeterCount - 1].Denominator);
        if ((LTick - AMeters[LMeterCount - 1].Tick) mod LMeasure) <> 0 then
          ImportError('actual meter change is not at a measure boundary');
        if LMeterCount = WFC_MUSIC_MIDI_IMPORT_MAX_TIMING_EVENTS then
          ImportError('canonical meter count exceeds the 4096 import limit');
        AMeters[LMeterCount] := MakeWfcMusicMeterChange(LTick,
          LNumerator, LDenominator);
        Inc(LMeterCount);
      end;
    end;
  end;
  SetLength(AMeters, LMeterCount);
  SetLength(ATempos, LTempoCount);
  LMeasure := MeasureTicks(AFile.TicksPerQuarter,
    AMeters[LMeterCount - 1].Numerator,
    AMeters[LMeterCount - 1].Denominator);
  LRemainder := (AReport.SourceLengthTicks -
    AMeters[LMeterCount - 1].Tick) mod LMeasure;
  AReport.ScoreLengthTicks := AReport.SourceLengthTicks;
  if LRemainder <> 0 then
  begin
    if AOptions.EndPolicy = wmmepRequireMeasure then
      ImportError('source ends inside a measure; explicit padding is required');
    AReport.PaddingTicks := LMeasure - LRemainder;
    if AReport.PaddingTicks > High(Integer) - AReport.SourceLengthTicks then
      ImportError('padded score length exceeds the Integer range');
    Inc(AReport.ScoreLengthTicks, AReport.PaddingTicks);
  end;
end;

function AllocateLanes(var ANotes: TImportNotes;
  var AReport: TWfcMusicMidiImportReport): TImportLanes;
var
  I: Integer;
  J: Integer;
  LCount: Integer;
  LFound: Integer;
  LNextLane: Integer;
begin
  Result := nil;
  SetLength(Result, WFC_MUSIC_MIDI_IMPORT_MAX_VOICES);
  LCount := 0;
  for I := 0 to Length(ANotes) - 1 do
  begin
    LFound := -1;
    LNextLane := 0;
    for J := 0 to LCount - 1 do
      if (Result[J].Source.SourceTrack = ANotes[I].Track) and
          (Result[J].Source.Channel = ANotes[I].Channel) then
      begin
        Inc(LNextLane);
        if Result[J].EndTick <= ANotes[I].StartTick then
        begin
          LFound := J;
          Break;
        end;
      end;
    if LFound < 0 then
    begin
      if LCount = WFC_MUSIC_MIDI_IMPORT_MAX_VOICES then
        ImportError('monophonic lane count exceeds the 256 import limit');
      LFound := LCount;
      Inc(LCount);
      Result[LFound].Source.SourceTrack := ANotes[I].Track;
      Result[LFound].Source.Channel := ANotes[I].Channel;
      Result[LFound].Source.Lane := LNextLane;
      Result[LFound].FirstNote := I;
      Result[LFound].LastNote := -1;
    end;
    if Result[LFound].LastNote >= 0 then
      ANotes[Result[LFound].LastNote].Next := I;
    Result[LFound].LastNote := I;
    Result[LFound].EndTick := ANotes[I].EndTick;
  end;
  if LCount = 0 then
  begin
    LCount := 1;
    Result[0].Source.SourceTrack := 0;
    Result[0].Source.Channel := -1;
    Result[0].Source.Lane := 0;
    Result[0].FirstNote := -1;
    Result[0].LastNote := -1;
    Result[0].EndTick := 0;
    AReport.AddedSilentVoice := True;
  end;
  SetLength(Result, LCount);
  SetLength(AReport.Voices, LCount);
  for I := 0 to LCount - 1 do
    AReport.Voices[I] := Result[I].Source;
end;

function BuildScore(const AFile: TWfcMidiFile;
  const ANotes: TImportNotes; const ALanes: TImportLanes;
  const AMeters: TWfcMusicMeterChanges; const ATempos: TWfcMusicTempoChanges;
  const ALengthTicks: Integer): TWfcMusicScore;
var
  LTracks: TWfcMusicTracks;
  LVoices: TWfcMusicVoices;
  LSpans: TWfcMusicSpanEvents;
  LTones: TWfcMusicTones;
  I: Integer;
  LNote: Integer;
  LTick: Integer;
  LSpanCount: Integer;
  LCapacity: Integer;
  LId: String;

  procedure AppendSpan(const ASpan: TWfcMusicSpanEvent);
  begin
    if LSpanCount >= LCapacity then
      ImportError('span count exceeds the bounded note/rest partition');
    LSpans[LSpanCount] := ASpan;
    Inc(LSpanCount);
  end;

begin
  SetLength(LTracks, Length(AFile.Tracks));
  for I := 0 to Length(LTracks) - 1 do
    LTracks[I] := MakeWfcMusicTrack('track-' + IntToStr(I),
      'MIDI track ' + IntToStr(I));
  SetLength(LVoices, Length(ALanes));
  LCapacity := Length(ANotes) * 2 + Length(ALanes);
  if LCapacity > WFC_MUSIC_MIDI_IMPORT_MAX_SPANS then
    ImportError('span capacity exceeds the 131328 import limit');
  SetLength(LSpans, LCapacity);
  SetLength(LTones, 1);
  LSpanCount := 0;
  for I := 0 to Length(ALanes) - 1 do
  begin
    LId := 'track-' + IntToStr(ALanes[I].Source.SourceTrack) +
      '-channel-' + IntToStr(ALanes[I].Source.Channel) +
      '-lane-' + IntToStr(ALanes[I].Source.Lane);
    LVoices[I] := MakeWfcMusicVoice(ALanes[I].Source.SourceTrack, LId);
    LTick := 0;
    LNote := ALanes[I].FirstNote;
    while LNote >= 0 do
    begin
      if ANotes[LNote].StartTick > LTick then
        AppendSpan(MakeWfcMusicRest(I, LTick,
          ANotes[LNote].StartTick - LTick));
      LTones[0] := MakeWfcMusicTone(ANotes[LNote].Pitch,
        ANotes[LNote].Velocity);
      AppendSpan(MakeWfcMusicSound(I, ANotes[LNote].StartTick,
        ANotes[LNote].EndTick - ANotes[LNote].StartTick, LTones));
      LTick := ANotes[LNote].EndTick;
      LNote := ANotes[LNote].Next;
    end;
    if LTick < ALengthTicks then
      AppendSpan(MakeWfcMusicRest(I, LTick, ALengthTicks - LTick));
  end;
  SetLength(LSpans, LSpanCount);
  Result := TWfcMusicScore.Create(AFile.TicksPerQuarter, 12, ALengthTicks,
    LTracks, LVoices, AMeters, ATempos, LSpans);
end;

function ImportWfcMusicMidi(const AFile: TWfcMidiFile;
  const AOptions: TWfcMusicMidiImportOptions;
  out AReport: TWfcMusicMidiImportReport): TWfcMusicScore;
var
  LReport: TWfcMusicMidiImportReport;
  LEvents: TImportEvents;
  LNotes: TImportNotes;
  LLanes: TImportLanes;
  LMeters: TWfcMusicMeterChanges;
  LTempos: TWfcMusicTempoChanges;
begin
  Result := nil;
  ResetReport(AReport);
  ResetReport(LReport);
  try
    Preflight(AFile, AOptions, LReport);
    LEvents := BuildTimeline(AFile, LReport.SourceEventCount);
    LNotes := PairNotes(AFile, LEvents, LReport.NoteCount);
    BuildTiming(AFile, LEvents, AOptions, LReport, LMeters, LTempos);
    LLanes := AllocateLanes(LNotes, LReport);
    Result := BuildScore(AFile, LNotes, LLanes, LMeters, LTempos,
      LReport.ScoreLengthTicks);
    AReport := LReport;
  except
    on E: EWfcMusicMidiImport do
      raise;
    on E: EWfcMusic do
      ImportError(E.Message);
  end;
end;

function DecodeWfcMusicMidi(const ABytes: TWfcMidiBytes;
  const AOptions: TWfcMusicMidiImportOptions;
  out AReport: TWfcMusicMidiImportReport): TWfcMusicScore;
var
  LLimits: TWfcMidiReadLimits;
  LFile: TWfcMidiFile;
begin
  Result := nil;
  ResetReport(AReport);
  ValidateOptions(AOptions);
  LLimits := DefaultWfcMidiReadLimits;
  LLimits.MaxFileBytes := WFC_MUSIC_MIDI_IMPORT_MAX_BYTES;
  LLimits.MaxTrackBytes := WFC_MUSIC_MIDI_IMPORT_MAX_BYTES;
  LLimits.MaxTracks := WFC_MUSIC_MIDI_IMPORT_MAX_TRACKS;
  LLimits.MaxEvents := WFC_MUSIC_MIDI_IMPORT_MAX_EVENTS;
  LLimits.MaxEventDataBytes := WFC_MUSIC_MIDI_IMPORT_MAX_BYTES;
  try
    LFile := DecodeWfcMidiFileWithLimits(ABytes, LLimits);
    Result := ImportWfcMusicMidi(LFile, AOptions, AReport);
  except
    on E: EWfcMidiSmf do
      ImportError(E.Message);
  end;
end;

end.
