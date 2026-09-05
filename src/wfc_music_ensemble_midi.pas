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

unit wfc_music_ensemble_midi;

{$mode delphi}{$H+}

interface

uses wfc_music_ensemble, wfc_midi_smf, wfc_midi_stream;

const WFC_MUSIC_ENSEMBLE_MIDI_VERSION = 1;

type
  EWfcMusicEnsembleMidi = class(EWfcMidiStream);
  TWfcMusicEnsembleMidiChannels = array of Integer;
  TWfcMusicEnsembleMidiOptions = record
    TicksPerQuarter, TempoMicrosecondsPerQuarter: Integer;
    MeterNumerator, MeterDenominatorPower: Integer;
    Channels: TWfcMusicEnsembleMidiChannels;
  end;
  { Zero tempo / zero numerator mean no change. Denominator must also be zero
    when no meter is supplied. Explicit changes, including repeated values, are
    emitted at the frame start, tempo before meter, before any note events. }
  TWfcMusicEnsembleMidiTiming = record
    TempoMicrosecondsPerQuarter, MeterNumerator, MeterDenominatorPower: Integer;
  end;

  { Implementation cursor shared by the two public transports. Its memory is
    bounded by the active and pending frame, not elapsed ticks/events. Prefer
    Counter and Stream below; neither retains a score or event timeline. }
  TWfcMusicEnsembleMidiEvents = class
  private
    FOptions: TWfcMusicEnsembleMidiOptions;
    FActive, FPending: TWfcMusicEnsembleFrame;
    FTiming: TWfcMusicEnsembleMidiTiming;
    FTick, FEventTick: TWfcMidiStreamCount;
    FPhase, FVoice, FTone: Integer;
    FInputEnded, FCancelled: Boolean;
    function GetNeedsInput: Boolean;
    function GetFinished: Boolean;
    procedure AdvancePhase;
  public
    constructor Create(const AOptions: TWfcMusicEnsembleMidiOptions);
    procedure AdmitFrame(const AFrame: TWfcMusicEnsembleFrame;
      const ALength: TWfcMidiStreamCount;
      const ATiming: TWfcMusicEnsembleMidiTiming);
    function NextEvent(out ATick: TWfcMidiStreamCount;
      out AEvent: TWfcMidiEvent): Boolean;
    procedure EndInput;
    procedure Cancel;
    property NeedsInput: Boolean read GetNeedsInput;
    property Finished: Boolean read GetFinished;
    property InputEnded: Boolean read FInputEnded;
    property TickCount: TWfcMidiStreamCount read FTick;
  end;

  { Caller-owned immutable configuration and track plan. CopyOptions detaches
    its channel array. The diagnostic signature covers track events, NOT the
    TPQ file header; this wrapper binds TPQ through its immutable options.
    FNV32 is not cryptographic proof: replay the same trusted immutable source.
    ByteCount excludes the 22-byte file/chunk header. }
  TWfcMusicEnsembleMidiPlan = class
  private
    FOptions: TWfcMusicEnsembleMidiOptions;
    FTrack: TWfcMidiTrackPlan;
    constructor Create(const AOptions: TWfcMusicEnsembleMidiOptions;
      const ATrack: TWfcMidiTrackPlan);
    function GetEndTick: TWfcMidiStreamCount;
    function GetByteCount: TWfcMidiStreamCount;
    function GetEventCount: TWfcMidiStreamCount;
    function GetBridgeCount: TWfcMidiStreamCount;
    function GetSignature: Cardinal;
  public
    destructor Destroy; override;
    function CopyOptions: TWfcMusicEnsembleMidiOptions;
    property EndTick: TWfcMidiStreamCount read GetEndTick;
    property ByteCount: TWfcMidiStreamCount read GetByteCount;
    property EventCount: TWfcMidiStreamCount read GetEventCount;
    property BridgeCount: TWfcMidiStreamCount read GetBridgeCount;
    property Signature: Cardinal read GetSignature;
  end;

  { Counting pass. Channels must map 1..16 voices uniquely to MIDI 0..15.
    Under General MIDI channel 9 is percussion; mapping it is explicit, never
    silently skipped or selected. No program/bank/tuning messages are invented.
    Tones use sorted unique 12-step MIDI pitches 0..127 and velocities 1..127.
    Positive frame lengths and their sum fit the portable exact-integer range.
    Holds require identical active pitches AND velocities and emit no seam
    events. Rest/attack close previous tones. All offs precede all ons, ordered
    by voice then pitch. Finish closes held notes at the exact accepted end.
    Empty input is valid initial tempo/meter plus EOT at zero.
    Invalid arguments are retryable; processing/capacity failures set Failed. }
  TWfcMusicEnsembleMidiCounter = class
  private
    FOptions: TWfcMusicEnsembleMidiOptions;
    FEvents: TWfcMusicEnsembleMidiEvents;
    FCounter: TWfcMidiTrackCounter;
    FFailed: Boolean;
    procedure Drain;
    function GetTickCount: TWfcMidiStreamCount;
    function GetFinished: Boolean;
  public
    constructor Create(const AOptions: TWfcMusicEnsembleMidiOptions);
    destructor Destroy; override;
    procedure AdmitFrame(const AFrame: TWfcMusicEnsembleFrame;
      const ALength: TWfcMidiStreamCount); overload;
    procedure AdmitFrame(const AFrame: TWfcMusicEnsembleFrame;
      const ALength: TWfcMidiStreamCount;
      const ATiming: TWfcMusicEnsembleMidiTiming); overload;
    function Finish: TWfcMusicEnsembleMidiPlan;
    property TickCount: TWfcMidiStreamCount read GetTickCount;
    property Finished: Boolean read GetFinished;
    property Failed: Boolean read FFailed;
  end;

  { Replay pass. Create copies all plan data; the caller can free the plan.
    Drain initial header/metadata, AdmitFrame only when NeedsInput, and drain
    each frame. EndInput closes voices and verifies the planned track before
    the final EOT can be returned. Publish only after Finished; any exception,
    cancellation or mismatch requires discarding the entire already-read
    prefix. The stream never owns files, callbacks or publication.
    ReadBytes returns detached blocks <=4096 bytes; False always sets nil.
    Invalid arguments/state remain retryable. Unexpected processing errors
    poison Failed. Cancel is terminal, discards pending data and emits no EOT.
    Same-tick ordering and delay bridges are project encoding policies.
    Sequential/non-reentrant; a plan is not a resumable generation checkpoint. }
  TWfcMusicEnsembleMidiStream = class
  private
    FEvents: TWfcMusicEnsembleMidiEvents;
    FStream: TWfcMidiFileStream;
    FEmittedBytes: TWfcMidiStreamCount;
    FFailed, FCancelled: Boolean;
    function GetNeedsInput: Boolean;
    function GetFinished: Boolean;
    function GetFailed: Boolean;
    function GetInputEnded: Boolean;
    function GetTickCount: TWfcMidiStreamCount;
    function GetEmittedBytes: TWfcMidiStreamCount;
  public
    constructor Create(const APlan: TWfcMusicEnsembleMidiPlan);
    destructor Destroy; override;
    procedure AdmitFrame(const AFrame: TWfcMusicEnsembleFrame;
      const ALength: TWfcMidiStreamCount); overload;
    procedure AdmitFrame(const AFrame: TWfcMusicEnsembleFrame;
      const ALength: TWfcMidiStreamCount;
      const ATiming: TWfcMusicEnsembleMidiTiming); overload;
    function ReadBytes(const AMaxBytes: Integer; out ABytes: TWfcMidiBytes): Boolean;
    procedure EndInput;
    procedure Cancel;
    property NeedsInput: Boolean read GetNeedsInput;
    property InputEnded: Boolean read GetInputEnded;
    property Finished: Boolean read GetFinished;
    property Failed: Boolean read GetFailed;
    property Cancelled: Boolean read FCancelled;
    property TickCount: TWfcMidiStreamCount read GetTickCount;
    property EmittedBytes: TWfcMidiStreamCount read GetEmittedBytes;
  end;

function DefaultWfcMusicEnsembleMidiOptions(const AChannels: array of Integer):
  TWfcMusicEnsembleMidiOptions;

implementation

uses SysUtils, wfc_music_sequence;

procedure MidiError(const AMessage: String);
begin
  raise EWfcMusicEnsembleMidi.Create('cannot stream ensemble MIDI: ' + AMessage);
end;

procedure CheckInteger(const AValue, AMin, AMax: TWfcMidiStreamCount;
  const AName: String);
begin
  if (AValue < AMin) or (AValue > AMax) then
    MidiError(AName + ' is outside its exact integer range');
  {$IFDEF PAS2JS}
  if AValue <> Trunc(AValue) then MidiError(AName + ' must be an exact integer');
  {$ENDIF}
end;

function CloneOptions(const AOptions: TWfcMusicEnsembleMidiOptions):
  TWfcMusicEnsembleMidiOptions;
var I: Integer;
begin
  Result := AOptions;
  Result.Channels := nil;
  SetLength(Result.Channels, Length(AOptions.Channels));
  for I := 0 to High(Result.Channels) do Result.Channels[I] := AOptions.Channels[I];
end;

procedure ValidateOptions(const AOptions: TWfcMusicEnsembleMidiOptions);
var I, J: Integer;
begin
  CheckInteger(AOptions.TicksPerQuarter, 1, 32767, 'ticks per quarter');
  CheckInteger(AOptions.TempoMicrosecondsPerQuarter, 1, $FFFFFF, 'tempo');
  CheckInteger(AOptions.MeterNumerator, 1, 255, 'meter numerator');
  CheckInteger(AOptions.MeterDenominatorPower, 0, 255, 'meter denominator power');
  if (Length(AOptions.Channels) < 1) or (Length(AOptions.Channels) > 16) then
    MidiError('voice count must fit the 16 MIDI channels');
  for I := 0 to High(AOptions.Channels) do
  begin
    CheckInteger(AOptions.Channels[I], 0, 15, 'channel');
    for J := 0 to I - 1 do
      if AOptions.Channels[I] = AOptions.Channels[J] then
        MidiError('voice channels must be unique');
  end;
end;

procedure ValidateTiming(const ATiming: TWfcMusicEnsembleMidiTiming);
begin
  CheckInteger(ATiming.TempoMicrosecondsPerQuarter, 0, $FFFFFF, 'tempo change');
  CheckInteger(ATiming.MeterNumerator, 0, 255, 'meter numerator change');
  CheckInteger(ATiming.MeterDenominatorPower, 0, 255, 'meter denominator power change');
  if (ATiming.MeterNumerator = 0) and (ATiming.MeterDenominatorPower <> 0) then
    MidiError('absent meter change must have zero denominator power');
end;

function DefaultWfcMusicEnsembleMidiOptions(const AChannels: array of Integer):
  TWfcMusicEnsembleMidiOptions;
var I: Integer;
begin
  Result := Default(TWfcMusicEnsembleMidiOptions);
  Result.TicksPerQuarter := 480;
  Result.TempoMicrosecondsPerQuarter := 500000;
  Result.MeterNumerator := 4;
  Result.MeterDenominatorPower := 2;
  if (Length(AChannels) < 1) or (Length(AChannels) > 16) then
    MidiError('voice count must fit the 16 MIDI channels');
  SetLength(Result.Channels, Length(AChannels));
  for I := 0 to High(AChannels) do Result.Channels[I] := AChannels[I];
  ValidateOptions(Result);
end;

constructor TWfcMusicEnsembleMidiEvents.Create(
  const AOptions: TWfcMusicEnsembleMidiOptions);
var I: Integer;
begin
  inherited Create;
  ValidateOptions(AOptions);
  FOptions := CloneOptions(AOptions);
  SetLength(FActive.Voices, Length(FOptions.Channels));
  SetLength(FPending.Voices, Length(FOptions.Channels));
  for I := 0 to High(FActive.Voices) do
  begin
    FActive.Voices[I] := MakeWfcMusicRestVoiceCell;
    FPending.Voices[I] := MakeWfcMusicRestVoiceCell;
  end;
  FTiming.TempoMicrosecondsPerQuarter := FOptions.TempoMicrosecondsPerQuarter;
  FTiming.MeterNumerator := FOptions.MeterNumerator;
  FTiming.MeterDenominatorPower := FOptions.MeterDenominatorPower;
end;

function TWfcMusicEnsembleMidiEvents.GetNeedsInput: Boolean;
begin
  Result := (FPhase = 4) and not FInputEnded and not FCancelled;
end;

function TWfcMusicEnsembleMidiEvents.GetFinished: Boolean;
begin
  Result := (FPhase = 4) and FInputEnded and not FCancelled;
end;

procedure TWfcMusicEnsembleMidiEvents.AdvancePhase;
begin
  Inc(FPhase);
  FVoice := 0;
  FTone := 0;
  if FPhase = 4 then
  begin
    if not FInputEnded then FActive := FPending else FActive.Voices := nil;
    FPending.Voices := nil;
  end;
end;

procedure TWfcMusicEnsembleMidiEvents.AdmitFrame(
  const AFrame: TWfcMusicEnsembleFrame; const ALength: TWfcMidiStreamCount;
  const ATiming: TWfcMusicEnsembleMidiTiming);
var I, J: Integer; LCopy: TWfcMusicEnsembleFrame;
begin
  if not NeedsInput then MidiError('drain the current frame before admission');
  CheckInteger(ALength, 1, WFC_MIDI_STREAM_MAX_SAFE_INTEGER, 'frame length');
  if ALength > WFC_MIDI_STREAM_MAX_SAFE_INTEGER - FTick then
    MidiError('end tick exceeds the portable exact integer envelope');
  ValidateTiming(ATiming);
  if Length(AFrame.Voices) <> Length(FOptions.Channels) then
    MidiError('frame voice count differs from the channel mapping');
  for I := 0 to High(AFrame.Voices) do
  begin
    CheckInteger(Ord(AFrame.Voices[I].Action), Ord(wmcaRest), Ord(wmcaHold), 'voice action');
    if AFrame.Voices[I].Action = wmcaRest then
    begin
      if Length(AFrame.Voices[I].Tones) <> 0 then MidiError('rest contains tones');
    end
    else
    begin
      if (Length(AFrame.Voices[I].Tones) < 1) or
        (Length(AFrame.Voices[I].Tones) > 128) then
        MidiError('sounding voice must contain 1..128 unique MIDI pitches');
      for J := 0 to High(AFrame.Voices[I].Tones) do
      begin
        CheckInteger(AFrame.Voices[I].Tones[J].Pitch, 0, 127, 'pitch');
        CheckInteger(AFrame.Voices[I].Tones[J].Velocity, 1, 127, 'velocity');
        if (J > 0) and (AFrame.Voices[I].Tones[J - 1].Pitch >=
          AFrame.Voices[I].Tones[J].Pitch) then
          MidiError('tone pitches must be strictly increasing');
      end;
      if AFrame.Voices[I].Action = wmcaHold then
      begin
        if Length(AFrame.Voices[I].Tones) <> Length(FActive.Voices[I].Tones) then
          MidiError('hold has no identical active predecessor');
        for J := 0 to High(AFrame.Voices[I].Tones) do
          if (AFrame.Voices[I].Tones[J].Pitch <> FActive.Voices[I].Tones[J].Pitch) or
            (AFrame.Voices[I].Tones[J].Velocity <> FActive.Voices[I].Tones[J].Velocity) then
            MidiError('hold differs from its active pitches or velocities');
      end;
    end;
  end;
  LCopy := MakeWfcMusicEnsembleFrame(AFrame.Voices);
  FPending := LCopy;
  FTiming := ATiming;
  FEventTick := FTick;
  FTick := FTick + ALength;
  FPhase := 0;
  FVoice := 0;
  FTone := 0;
end;

function TWfcMusicEnsembleMidiEvents.NextEvent(out ATick: TWfcMidiStreamCount;
  out AEvent: TWfcMidiEvent): Boolean;
var LTempo, LPitch, LVelocity: Integer;
begin
  Result := False;
  AEvent := Default(TWfcMidiEvent);
  ATick := FEventTick;
  if FCancelled then Exit;
  while FPhase < 4 do
    case FPhase of
      0:
        begin
          LTempo := FTiming.TempoMicrosecondsPerQuarter;
          if LTempo <> 0 then
            AEvent := MakeWfcMidiMetaEvent(0, $51,
              [Byte(LTempo shr 16), Byte((LTempo shr 8) and 255), Byte(LTempo and 255)]);
          AdvancePhase;
          if LTempo <> 0 then Exit(True);
        end;
      1:
        begin
          if FTiming.MeterNumerator <> 0 then
            AEvent := MakeWfcMidiMetaEvent(0, $58,
              [Byte(FTiming.MeterNumerator), Byte(FTiming.MeterDenominatorPower), 24, 8]);
          AdvancePhase;
          if FTiming.MeterNumerator <> 0 then Exit(True);
        end;
      2:
        begin
          while FVoice < Length(FActive.Voices) do
          begin
            if FInputEnded or (FPending.Voices[FVoice].Action <> wmcaHold) then
              if FTone < Length(FActive.Voices[FVoice].Tones) then
              begin
                LPitch := FActive.Voices[FVoice].Tones[FTone].Pitch;
                AEvent := MakeWfcMidiChannelEvent(0, Byte($80 + FOptions.Channels[FVoice]),
                  [Byte(LPitch), 0]);
                Inc(FTone);
                Exit(True);
              end;
            Inc(FVoice);
            FTone := 0;
          end;
          AdvancePhase;
        end;
      3:
        begin
          if not FInputEnded then
            while FVoice < Length(FPending.Voices) do
            begin
              if FPending.Voices[FVoice].Action = wmcaAttack then
                if FTone < Length(FPending.Voices[FVoice].Tones) then
                begin
                  LPitch := FPending.Voices[FVoice].Tones[FTone].Pitch;
                  LVelocity := FPending.Voices[FVoice].Tones[FTone].Velocity;
                  AEvent := MakeWfcMidiChannelEvent(0, Byte($90 + FOptions.Channels[FVoice]),
                    [Byte(LPitch), Byte(LVelocity)]);
                  Inc(FTone);
                  Exit(True);
                end;
              Inc(FVoice);
              FTone := 0;
            end;
          AdvancePhase;
        end;
    end;
end;

procedure TWfcMusicEnsembleMidiEvents.EndInput;
begin
  if FCancelled then MidiError('stream was cancelled');
  if FInputEnded then Exit;
  if not NeedsInput then MidiError('drain the current frame before ending input');
  FInputEnded := True;
  FEventTick := FTick;
  FPhase := 2;
  FVoice := 0;
  FTone := 0;
end;

procedure TWfcMusicEnsembleMidiEvents.Cancel;
begin
  FCancelled := True;
  FActive.Voices := nil;
  FPending.Voices := nil;
end;

constructor TWfcMusicEnsembleMidiPlan.Create(
  const AOptions: TWfcMusicEnsembleMidiOptions; const ATrack: TWfcMidiTrackPlan);
begin
  inherited Create;
  FOptions := CloneOptions(AOptions);
  FTrack := ATrack;
end;

destructor TWfcMusicEnsembleMidiPlan.Destroy;
begin
  FTrack.Free;
  inherited Destroy;
end;

function TWfcMusicEnsembleMidiPlan.CopyOptions: TWfcMusicEnsembleMidiOptions;
begin Result := CloneOptions(FOptions); end;
function TWfcMusicEnsembleMidiPlan.GetEndTick: TWfcMidiStreamCount;
begin Result := FTrack.EndTick; end;
function TWfcMusicEnsembleMidiPlan.GetByteCount: TWfcMidiStreamCount;
begin Result := FTrack.ByteCount; end;
function TWfcMusicEnsembleMidiPlan.GetEventCount: TWfcMidiStreamCount;
begin Result := FTrack.EventCount; end;
function TWfcMusicEnsembleMidiPlan.GetBridgeCount: TWfcMidiStreamCount;
begin Result := FTrack.BridgeCount; end;
function TWfcMusicEnsembleMidiPlan.GetSignature: Cardinal;
begin Result := FTrack.Signature; end;

constructor TWfcMusicEnsembleMidiCounter.Create(
  const AOptions: TWfcMusicEnsembleMidiOptions);
begin
  inherited Create;
  ValidateOptions(AOptions);
  FOptions := CloneOptions(AOptions);
  FCounter := TWfcMidiTrackCounter.Create;
  FEvents := TWfcMusicEnsembleMidiEvents.Create(FOptions);
  Drain;
end;

destructor TWfcMusicEnsembleMidiCounter.Destroy;
begin
  FEvents.Free;
  FCounter.Free;
  inherited Destroy;
end;

procedure TWfcMusicEnsembleMidiCounter.Drain;
var LTick: TWfcMidiStreamCount; LEvent: TWfcMidiEvent;
begin
  try
    while FEvents.NextEvent(LTick, LEvent) do FCounter.AppendEvent(LTick, LEvent);
  except
    FFailed := True;
    raise;
  end;
end;

procedure TWfcMusicEnsembleMidiCounter.AdmitFrame(
  const AFrame: TWfcMusicEnsembleFrame; const ALength: TWfcMidiStreamCount);
begin
  AdmitFrame(AFrame, ALength, Default(TWfcMusicEnsembleMidiTiming));
end;

procedure TWfcMusicEnsembleMidiCounter.AdmitFrame(
  const AFrame: TWfcMusicEnsembleFrame; const ALength: TWfcMidiStreamCount;
  const ATiming: TWfcMusicEnsembleMidiTiming);
begin
  if FFailed then MidiError('counter failed');
  FEvents.AdmitFrame(AFrame, ALength, ATiming);
  Drain;
end;

function TWfcMusicEnsembleMidiCounter.Finish: TWfcMusicEnsembleMidiPlan;
var LTrack: TWfcMidiTrackPlan;
begin
  if FFailed then MidiError('counter failed');
  FEvents.EndInput;
  Drain;
  LTrack := nil;
  try
    try
      LTrack := FCounter.Finish(FEvents.TickCount);
      Result := TWfcMusicEnsembleMidiPlan.Create(FOptions, LTrack);
      LTrack := nil;
    except
      FFailed := True;
      raise;
    end;
  finally
    LTrack.Free;
  end;
end;

function TWfcMusicEnsembleMidiCounter.GetTickCount: TWfcMidiStreamCount;
begin Result := FEvents.TickCount; end;
function TWfcMusicEnsembleMidiCounter.GetFinished: Boolean;
begin Result := FCounter.Finished and not FFailed; end;

constructor TWfcMusicEnsembleMidiStream.Create(const APlan: TWfcMusicEnsembleMidiPlan);
begin
  inherited Create;
  if APlan = nil then MidiError('plan is required');
  FEvents := TWfcMusicEnsembleMidiEvents.Create(APlan.FOptions);
  FStream := TWfcMidiFileStream.Create(APlan.FOptions.TicksPerQuarter, APlan.FTrack);
end;

destructor TWfcMusicEnsembleMidiStream.Destroy;
begin
  FStream.Free;
  FEvents.Free;
  inherited Destroy;
end;

function TWfcMusicEnsembleMidiStream.GetNeedsInput: Boolean;
begin
  Result := not Failed and not FCancelled and FStream.NeedsInput and FEvents.NeedsInput;
end;
function TWfcMusicEnsembleMidiStream.GetFinished: Boolean;
begin Result := not Failed and not FCancelled and FStream.Finished; end;
function TWfcMusicEnsembleMidiStream.GetFailed: Boolean;
begin Result := FFailed or FStream.Failed; end;
function TWfcMusicEnsembleMidiStream.GetInputEnded: Boolean;
begin Result := FEvents.InputEnded; end;
function TWfcMusicEnsembleMidiStream.GetTickCount: TWfcMidiStreamCount;
begin Result := FEvents.TickCount; end;
function TWfcMusicEnsembleMidiStream.GetEmittedBytes: TWfcMidiStreamCount;
begin Result := FEmittedBytes; end;

procedure TWfcMusicEnsembleMidiStream.AdmitFrame(
  const AFrame: TWfcMusicEnsembleFrame; const ALength: TWfcMidiStreamCount);
begin AdmitFrame(AFrame, ALength, Default(TWfcMusicEnsembleMidiTiming)); end;

procedure TWfcMusicEnsembleMidiStream.AdmitFrame(
  const AFrame: TWfcMusicEnsembleFrame; const ALength: TWfcMidiStreamCount;
  const ATiming: TWfcMusicEnsembleMidiTiming);
begin
  if not NeedsInput then MidiError('stream is not ready for a frame');
  FEvents.AdmitFrame(AFrame, ALength, ATiming);
end;

function TWfcMusicEnsembleMidiStream.ReadBytes(const AMaxBytes: Integer;
  out ABytes: TWfcMidiBytes): Boolean;
var
  LTick: TWfcMidiStreamCount;
  LEvent: TWfcMidiEvent;
  LBlock: TWfcMidiBytes;
  LLimit, LCount, I: Integer;
begin
  ABytes := nil;
  CheckInteger(AMaxBytes, 1, High(Integer), 'read byte count');
  if Failed then MidiError('stream failed; discard the output prefix');
  if FCancelled then Exit(False);
  LLimit := AMaxBytes;
  if LLimit > WFC_MIDI_STREAM_BLOCK_BYTES then LLimit := WFC_MIDI_STREAM_BLOCK_BYTES;
  LCount := 0;
  try
    while LCount < LLimit do
    begin
      if FStream.ReadBytes(LLimit - LCount, LBlock) then
      begin
        if LCount = 0 then SetLength(ABytes, LLimit);
        for I := 0 to High(LBlock) do ABytes[LCount + I] := LBlock[I];
        Inc(LCount, Length(LBlock));
        Continue;
      end;
      if FStream.Finished then Break;
      if FEvents.NextEvent(LTick, LEvent) then
        FStream.AdmitEvent(LTick, LEvent)
      else if FEvents.Finished then
        FStream.Finish(FEvents.TickCount)
      else Break;
    end;
    SetLength(ABytes, LCount);
    Result := LCount <> 0;
    if Result then Inc(FEmittedBytes, LCount);
  except
    FFailed := True;
    ABytes := nil;
    raise;
  end;
end;

procedure TWfcMusicEnsembleMidiStream.EndInput;
begin
  if Failed or FCancelled then MidiError('stream failed or was cancelled');
  if FEvents.InputEnded then Exit;
  if not NeedsInput then MidiError('drain the current frame before ending input');
  FEvents.EndInput;
end;

procedure TWfcMusicEnsembleMidiStream.Cancel;
begin
  if Finished then Exit;
  FCancelled := True;
  FEvents.Cancel;
  FStream.Cancel;
end;

end.
