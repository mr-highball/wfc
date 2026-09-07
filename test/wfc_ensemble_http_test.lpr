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
program wfc_ensemble_http_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host, JS,{$ENDIF}
  SysUtils, wfc, wfc_music_audio, wfc_music_audio_stream,
  ensemble_studio_profiles, ensemble_studio_stream, ensemble_studio_http;

const
  TOKEN = '0123456789abcdef0123456789abcdef';

type
  THeaderSink = class(TWfcMusicAudioByteSink)
  public
    Bytes: TWfcMusicAudioBytes;
    procedure WriteBytes(const ABytes: array of Byte); override;
  end;

var Checks: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure THeaderSink.WriteBytes(const ABytes: array of Byte);
var I: Integer;
begin
  if (Length(Bytes) <> 0) or (Length(ABytes) > 80) then
    raise Exception.Create('header-only probe received unexpected audio');
  SetLength(Bytes, Length(ABytes));
  for I := 0 to High(ABytes) do Bytes[I] := ABytes[I];
end;

function U64(const ABytes: TWfcMusicAudioBytes; const AOffset: Integer): TEnsembleStudioHttpByteCount;
var I: Integer;
begin
  Result := 0;
  for I := 7 downto 0 do Result := Result * 256 + ABytes[AOffset + I];
end;

function U32(const ABytes: TWfcMusicAudioBytes; const AOffset: Integer): TEnsembleStudioHttpByteCount;
var I: Integer;
begin
  Result := 0;
  for I := 3 downto 0 do Result := Result * 256 + ABytes[AOffset + I];
end;

function Ascii(const ABytes: TWfcMusicAudioBytes; const AOffset, ALength: Integer): String;
var I: Integer;
begin
  Result := '';
  for I := 0 to ALength - 1 do Result := Result + Chr(ABytes[AOffset + I]);
end;

function Target(const ASeconds: String = '61.25'): String;
begin
  Result := BuildEnsembleStudioHttpTarget(TOKEN, ASeconds, DefaultEnsembleStudioStreamOptions);
end;

procedure RejectTarget(const ATarget: String);
var Failed: Boolean; R: TEnsembleStudioHttpRequest;
begin
  Failed := False;
  try R := ParseEnsembleStudioHttpRequest(ATarget);
  except on E: EEnsembleStudioHttp do Failed := True; end;
  Check(Failed, 'malformed target rejected with protocol exception');
end;

procedure RejectBuild(const O: TEnsembleStudioStreamOptions; const S: String = '1');
var Failed: Boolean; T: String;
begin
  Failed := False;
  try T := BuildEnsembleStudioHttpTarget(TOKEN, S, O);
  except on E: EEnsembleStudioHttp do Failed := True; end;
  Check(Failed, 'invalid builder input rejected with protocol exception');
end;

procedure RejectPlan(const P: TEnsembleStudioStreamPlan);
var Failed: Boolean; N: TEnsembleStudioHttpByteCount;
begin
  Failed := False;
  try N := EnsembleStudioHttpWaveBytes(P);
  except on E: EEnsembleStudioHttp do Failed := True; end;
  Check(Failed, 'untrusted plan rejected with protocol exception');
end;

procedure RejectFileName(const R: TEnsembleStudioHttpRequest);
var Failed: Boolean; S: String;
begin
  Failed := False;
  try S := EnsembleStudioHttpFileName(R);
  except on E: EEnsembleStudioHttp do Failed := True; end;
  Check(Failed, 'inconsistent filename request rejected');
end;

function Mutate(const AOld, ANew: String): String;
begin
  Result := StringReplace(Target, AOld, ANew, []);
  Check(Result <> Target, 'negative mutation has an actual needle');
end;

procedure TestRoundTrips;
const Seeds: array[0..4] of Cardinal = (0, 1, 2147483647, 2147483648, 4294967295);
var O: TEnsembleStudioStreamOptions; R: TEnsembleStudioHttpRequest;
  P: TEnsembleStudioProfile; I, J: Integer; S, T, FileName: String;
begin
  Check((ENSEMBLE_STUDIO_HTTP_VERSION = 1) and
    (ENSEMBLE_STUDIO_HTTP_MAX_TARGET_LENGTH = 2048) and
    (ENSEMBLE_STUDIO_HTTP_CAPABILITIES_PATH = '/ensemble-stream-v1') and
    (ENSEMBLE_STUDIO_HTTP_WAVE_PATH = '/ensemble-wave-v1'), 'versioned protocol constants');
  O := DefaultEnsembleStudioStreamOptions;
  Check(Target = '/ensemble-wave-v1?token=' + TOKEN + '&seconds=61.25&seed=0' +
    '&profile=structural-v1&segment-cells=5&backtracks=256&pass-backtracks=16&trace=0',
    'canonical order and explicit complete options');
  for P := Low(TEnsembleStudioProfile) to High(TEnsembleStudioProfile) do
    for I := 0 to High(Seeds) do
    begin
      O.Seed := Seeds[I]; O.Profile := P; O.CaptureTrace := (I mod 2) = 1;
      O.SegmentCellCount := 7; O.MaxBacktracks := 129; O.MaxPassBacktracks := 3;
      S := BuildEnsembleStudioHttpTarget(TOKEN, '00061.2500', O);
      R := ParseEnsembleStudioHttpRequest(S);
      Check((R.Token = TOKEN) and (R.Seconds = '00061.2500') and
        (R.Plan.RequestedText = R.Seconds), 'original seconds and token retained');
      Check((R.Options.Seed = O.Seed) and (R.Options.Profile = O.Profile) and
        (R.Options.CaptureTrace = O.CaptureTrace) and
        (R.Options.SegmentCellCount = 7) and (R.Options.MaxBacktracks = 129) and
        (R.Options.MaxPassBacktracks = 3), 'all generation settings bound exactly');
      Check((R.Plan.RequestedTicks = 58800) and (R.Plan.ActualTicks = 58800) and
        (R.Plan.CellCount = 245) and (R.Plan.ExpectedFrames = 2701125),
        'user-defined duration beyond sixty seconds planned exactly');
      Check(EnsembleStudioHttpWaveBytes(R.Plan) = 5402294, 'mono PCM16 complete RIFF length');
      Check(BuildEnsembleStudioHttpTarget(R.Token, R.Seconds, R.Options) = S,
        'canonical round trip stable');
      FileName := EnsembleStudioHttpFileName(R);
      Check(FileName = 'ensemble-' + UIntToStr(O.Seed) + '-' +
        EnsembleStudioProfileName(O.Profile) + '-58800ticks.wav', 'filename binds profile seed and actual ticks');
      Check(Length(FileName) <= 128, 'bounded filename');
      for J := 1 to Length(FileName) do
        Check(FileName[J] in ['a'..'z','0'..'9','-','.'], 'safe ASCII filename character');
    end;
  O := DefaultEnsembleStudioStreamOptions;
  O.SegmentCellCount := High(Integer) div ENSEMBLE_STUDIO_STREAM_QUANTUM;
  O.MaxBacktracks := High(Integer); O.MaxPassBacktracks := High(Integer);
  R := ParseEnsembleStudioHttpRequest(BuildEnsembleStudioHttpTarget(TOKEN, '1', O));
  Check((R.Options.SegmentCellCount = O.SegmentCellCount) and
    (R.Options.MaxBacktracks = High(Integer)) and
    (R.Options.MaxPassBacktracks = High(Integer)), 'numeric representation bounds not arbitrary policy caps');
  S := Target('1');
  T := '1.' + StringOfChar('0', ENSEMBLE_STUDIO_HTTP_MAX_TARGET_LENGTH - Length(S) - 1);
  S := Target(T);
  Check(Length(S) = ENSEMBLE_STUDIO_HTTP_MAX_TARGET_LENGTH, 'exact target length accepted');
  R := ParseEnsembleStudioHttpRequest(S);
  Check(R.Seconds = T, 'long exact decimal is not floated or shortened');
  RejectTarget(S + '0');
  RejectBuild(DefaultEnsembleStudioStreamOptions, T + '0');
end;

procedure TestOrder;
const F: array[0..7] of String = ('token=' + TOKEN, 'seconds=61.25', 'seed=0',
  'profile=structural-v1', 'segment-cells=5', 'backtracks=256',
  'pass-backtracks=16', 'trace=0');
var I, J: Integer; S: String; R: TEnsembleStudioHttpRequest;
begin
  for I := 0 to 7 do
  begin
    S := ENSEMBLE_STUDIO_HTTP_WAVE_PATH + '?';
    for J := 0 to 7 do
    begin
      if J <> 0 then S := S + '&';
      S := S + F[(I + J) mod 8];
    end;
    R := ParseEnsembleStudioHttpRequest(S);
    Check(BuildEnsembleStudioHttpTarget(R.Token, R.Seconds, R.Options) = Target,
      'all cyclic field orders canonicalize identically');
    RejectTarget(Target + '&' + F[I]);
    S := StringReplace(Target, F[I], '', []);
    RejectTarget(S);
  end;
  R := ParseEnsembleStudioHttpRequest(ENSEMBLE_STUDIO_HTTP_WAVE_PATH +
    '?trace=0&pass-backtracks=16&backtracks=256&segment-cells=5' +
    '&profile=structural-v1&seed=0&seconds=61.25&token=' + TOKEN);
  Check(BuildEnsembleStudioHttpTarget(R.Token, R.Seconds, R.Options) = Target,
    'reverse query order accepted');
end;

procedure TestRejected;
var O: TEnsembleStudioStreamOptions; R: TEnsembleStudioHttpRequest;
  P: TEnsembleStudioStreamPlan; I: Integer; S: String;
begin
  RejectTarget('');
  RejectTarget('/ensemble-wave-v1');
  RejectTarget('/ensemble-wave-v1?');
  RejectTarget('/ensemble-stream-v1');
  RejectTarget('http://127.0.0.1' + Target);
  RejectTarget('/prefix' + Target);
  RejectTarget(StringReplace(Target, '-v1?', '-v2?', []));
  RejectTarget(Target + '&');
  RejectTarget(Target + '&extra=1');
  RejectTarget(Target + '&=1');
  RejectTarget(Target + '#fragment');
  RejectTarget(Target + '&token');
  RejectTarget(Mutate('token=' + TOKEN, 'token='));
  RejectTarget(Mutate('token=' + TOKEN, 'token=' + UpperCase(TOKEN)));
  RejectTarget(Mutate('token=' + TOKEN, 'token=' + Copy(TOKEN, 1, 31)));
  RejectTarget(Mutate('token=' + TOKEN, 'token=' + TOKEN + '0'));
  RejectTarget(Mutate('token=' + TOKEN, 'token=' + StringOfChar('g', 32)));
  RejectTarget(Mutate('token=', 'TOKEN='));
  RejectTarget(Mutate('seconds=61.25', 'seconds=0'));
  RejectTarget(Mutate('seconds=61.25', 'seconds=0.000'));
  RejectTarget(Mutate('seconds=61.25', 'seconds=.25'));
  RejectTarget(Mutate('seconds=61.25', 'seconds=1.'));
  RejectTarget(Mutate('seconds=61.25', 'seconds=1.2.3'));
  RejectTarget(Mutate('seconds=61.25', 'seconds=1e3'));
  RejectTarget(Mutate('seconds=61.25', 'seconds=-1'));
  RejectTarget(Mutate('seconds=61.25', 'seconds=+1'));
  RejectTarget(Mutate('seconds=61.25', 'seconds=%31'));
  RejectTarget(Mutate('seconds=61.25', 'seconds=1=2'));
  RejectTarget(Mutate('seed=0', 'seed=4294967296'));
  RejectTarget(Mutate('seed=0', 'seed=-1'));
  RejectTarget(Mutate('seed=0', 'seed=01'));
  RejectTarget(Mutate('seed=0', 'seed=1.0'));
  RejectTarget(Mutate('profile=structural-v1', 'profile=unknown'));
  RejectTarget(Mutate('segment-cells=5', 'segment-cells=0'));
  RejectTarget(Mutate('segment-cells=5', 'segment-cells=' +
    IntToStr(High(Integer) div ENSEMBLE_STUDIO_STREAM_QUANTUM + 1)));
  RejectTarget(Mutate('backtracks=256', 'backtracks=2147483648'));
  RejectTarget(Mutate('pass-backtracks=16', 'pass-backtracks=-1'));
  RejectTarget(Mutate('trace=0', 'trace=true'));
  RejectTarget(Mutate('trace=0', 'trace=2'));
  RejectTarget(Mutate('trace=0', 'trace=00'));
  RejectTarget(Target + '/');
  RejectTarget(Target + '\');
  for I := 0 to 32 do RejectTarget(Target + Chr(I));
  RejectTarget(Target + Chr(127));
  RejectTarget(Target + Chr(128));
  O := DefaultEnsembleStudioStreamOptions;
  O.SegmentCellCount := 0; RejectBuild(O);
  O := DefaultEnsembleStudioStreamOptions;
  O.MaxBacktracks := -1; RejectBuild(O);
  O := DefaultEnsembleStudioStreamOptions;
  O.MaxPassBacktracks := -1; RejectBuild(O);
  O := DefaultEnsembleStudioStreamOptions;
  RejectBuild(O, ' 1'); RejectBuild(O, '1 ');
  RejectBuild(O, '9007199254740.991'); { Tick rounding overflow remains typed. }
  RejectBuild(O, '999999999999999999999999');
  RejectBuild(O, StringOfChar('1', 2049));
  R := ParseEnsembleStudioHttpRequest(Target);
  P := R.Plan; Inc(P.RequestedTicks); RejectPlan(P);
  P := R.Plan; Inc(P.ActualTicks); RejectPlan(P);
  P := R.Plan; Inc(P.CellCount); RejectPlan(P);
  P := R.Plan; Inc(P.ExpectedFrames); RejectPlan(P);
  P := R.Plan; P.RequestedText := '0'; RejectPlan(P);
  R.Seconds := '61.2500'; RejectFileName(R);
  R := ParseEnsembleStudioHttpRequest(Target);
  R.Token := 'unsafe-header'; RejectFileName(R);
  R := ParseEnsembleStudioHttpRequest(Target);
  R.Options.MaxBacktracks := -1; RejectFileName(R);
  {$IFDEF PAS2JS}
  O := DefaultEnsembleStudioStreamOptions;
  asm O.Seed = '0'; end; RejectBuild(O);
  asm O.Seed = NaN; end; RejectBuild(O);
  asm O.Seed = Infinity; end; RejectBuild(O);
  asm O.Seed = -1; end; RejectBuild(O);
  asm O.Seed = 0.5; end; RejectBuild(O);
  asm O.Seed = 4294967296; end; RejectBuild(O);
  O := DefaultEnsembleStudioStreamOptions;
  asm O.Profile = '0'; end; RejectBuild(O);
  asm O.Profile = NaN; end; RejectBuild(O);
  asm O.Profile = 0.5; end; RejectBuild(O);
  O := DefaultEnsembleStudioStreamOptions;
  asm O.SegmentCellCount = '5'; end; RejectBuild(O);
  asm O.SegmentCellCount = Infinity; end; RejectBuild(O);
  asm O.SegmentCellCount = 1.5; end; RejectBuild(O);
  O := DefaultEnsembleStudioStreamOptions;
  asm O.MaxBacktracks = NaN; end; RejectBuild(O);
  O := DefaultEnsembleStudioStreamOptions;
  asm O.MaxPassBacktracks = '16'; end; RejectBuild(O);
  O := DefaultEnsembleStudioStreamOptions;
  asm O.CaptureTrace = 'false'; end; RejectBuild(O);
  asm O.CaptureTrace = 0; end; RejectBuild(O);
  asm O.CaptureTrace = null; end; RejectBuild(O);
  O := DefaultEnsembleStudioStreamOptions;
  asm S = 1; end; RejectBuild(O, S);
  asm S = null; end; RejectTarget(S);
  P := PlanEnsembleStudioStream('1');
  asm P.ExpectedFrames = '44100'; end; RejectPlan(P);
  P := PlanEnsembleStudioStream('1');
  asm P.ActualTicks = NaN; end; RejectPlan(P);
  {$ENDIF}
end;

procedure CheckWaveHeader(const ASeconds: String; const ARF64: Boolean);
var R: TEnsembleStudioHttpRequest; Sink: THeaderSink;
  Stream: TWfcMusicWaveStream; Bytes: TEnsembleStudioHttpByteCount;
begin
  R := ParseEnsembleStudioHttpRequest(Target(ASeconds));
  Bytes := EnsembleStudioHttpWaveBytes(R.Plan);
  Sink := THeaderSink.Create;
  try
    Stream := TWfcMusicWaveStream.Create(Sink, ENSEMBLE_STUDIO_STREAM_SAMPLE_RATE,
      R.Plan.ExpectedFrames);
    try
      Check(Stream.IsRF64 = ARF64, 'independent WAVE encoder chooses expected container');
      Check(Stream.FrameCount = 0, 'header probe allocates no audio or graph generation');
      if ARF64 then
      begin
        Check((Length(Sink.Bytes) = 80) and (Ascii(Sink.Bytes, 0, 4) = 'RF64'), 'RF64 header');
        Check(U64(Sink.Bytes, 20) + 8 = Bytes, 'ds64 total file length matches HTTP Content-Length');
        Check(U64(Sink.Bytes, 28) + 80 = Bytes, 'ds64 PCM byte count exact');
        Check(U64(Sink.Bytes, 36) = R.Plan.ExpectedFrames, 'ds64 sample count exact');
      end
      else
      begin
        Check((Length(Sink.Bytes) = 44) and (Ascii(Sink.Bytes, 0, 4) = 'RIFF'), 'RIFF header');
        Check(U32(Sink.Bytes, 4) + 8 = Bytes, 'RIFF length matches HTTP Content-Length');
        Check(U32(Sink.Bytes, 40) + 44 = Bytes, 'RIFF PCM length exact');
      end;
    finally Stream.Free; end;
  finally Sink.Free; end;
end;

procedure TestDurationAndHeaders;
var R: TEnsembleStudioHttpRequest;
begin
  R := ParseEnsembleStudioHttpRequest(Target('0.00000000000000000001'));
  Check((R.Plan.RequestedTicks = 1) and (R.Plan.ActualTicks = 240) and
    (EnsembleStudioHttpWaveBytes(R.Plan) = 22094), 'sub-tick decimal rounds exactly to cell');
  R := ParseEnsembleStudioHttpRequest(Target('0.333333333333333333'));
  Check(R.Plan.RequestedTicks = 320, 'fraction below third exact');
  R := ParseEnsembleStudioHttpRequest(Target('0.333333333333333334'));
  Check(R.Plan.RequestedTicks = 321, 'fraction above third exact');
  R := ParseEnsembleStudioHttpRequest(Target('180'));
  Check(R.Plan.ExpectedFrames = 7938000, 'three minutes is an ordinary requested value');
  R := ParseEnsembleStudioHttpRequest(Target('102122440529.75'));
  Check((R.Plan.CellCount = 408489762119) and
    (R.Plan.ExpectedFrames = 4503599627361975) and
    (EnsembleStudioHttpWaveBytes(R.Plan) = 9007199254724030),
    'last complete cell within shared exact-safe audio count envelope');
  RejectBuild(DefaultEnsembleStudioStreamOptions, '102122440529.7500000000001');
  RejectBuild(DefaultEnsembleStudioStreamOptions, '102122440530');
  CheckWaveHeader('0.001', False);
  CheckWaveHeader('61.25', False);
  CheckWaveHeader('48695.75', False);
  CheckWaveHeader('48696', True);
  CheckWaveHeader('60000', True);
  CheckWaveHeader('1000000000', True);
  CheckWaveHeader('102122440529.75', True);
end;

begin
  TestRoundTrips;
  TestOrder;
  TestRejected;
  TestDurationAndHeaders;
  WriteLn('[SUMMARY] checks=', Checks, ' failures=0');
end.
