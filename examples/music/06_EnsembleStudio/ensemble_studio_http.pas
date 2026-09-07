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
unit ensemble_studio_http;

{$mode delphi}{$H+}

interface

uses
  SysUtils, ensemble_studio_stream, wfc_music_audio_stream;

const
  ENSEMBLE_STUDIO_HTTP_VERSION = 1;
  ENSEMBLE_STUDIO_HTTP_MAX_TARGET_LENGTH = 2048;
  ENSEMBLE_STUDIO_HTTP_CAPABILITIES_PATH = '/ensemble-stream-v1';
  ENSEMBLE_STUDIO_HTTP_WAVE_PATH = '/ensemble-wave-v1';

type
  EEnsembleStudioHttp = class(Exception);
  TEnsembleStudioHttpByteCount = TWfcMusicAudioStreamCount;
  TEnsembleStudioHttpRequest = record
    Token, Seconds: String;
    Options: TEnsembleStudioStreamOptions;
    Plan: TEnsembleStudioStreamPlan;
  end;

{ Pure protocol helpers. Parsing does not authenticate the capability token,
  construct a graph, or generate PCM. The dedicated host must authenticate the
  supplied token before execution. This is not a generic static-server route. }
function ParseEnsembleStudioHttpRequest(const ATarget: String): TEnsembleStudioHttpRequest;
function BuildEnsembleStudioHttpTarget(const AToken, ASeconds: String;
  const AOptions: TEnsembleStudioStreamOptions): String;
function EnsembleStudioHttpWaveBytes(const APlan: TEnsembleStudioStreamPlan):
  TEnsembleStudioHttpByteCount;
function EnsembleStudioHttpFileName(const ARequest: TEnsembleStudioHttpRequest): String;

implementation

uses
  wfc, wfc_text_codec, wfc_music_arrangement, ensemble_studio_profiles;

type
  THttpField = (hfToken, hfSeconds, hfSeed, hfProfile, hfSegmentCells,
    hfBacktracks, hfPassBacktracks, hfTrace);
  THttpFields = array[THttpField] of String;

procedure HttpError(const AMessage: String);
begin
  raise EEnsembleStudioHttp.Create('Ensemble Studio HTTP: ' + AMessage);
end;

function IsText(const AValue: String): Boolean;
begin
  {$IFDEF PAS2JS}
  asm Result = typeof AValue === 'string'; end;
  {$ELSE}Result := True;{$ENDIF}
end;

function IsIntegerInRange(const AValue, AMinimum, AMaximum: TEnsembleStudioHttpByteCount): Boolean;
begin
  {$IFDEF PAS2JS}
  asm
    Result = typeof AValue === 'number' && isFinite(AValue) &&
      Math.floor(AValue) === AValue && AValue >= AMinimum && AValue <= AMaximum;
  end;
  {$ELSE}
  Result := (AValue >= AMinimum) and (AValue <= AMaximum);
  {$ENDIF}
end;

procedure ValidateToken(const AToken: String);
var I: Integer;
begin
  if not IsText(AToken) then HttpError('token must be text');
  if Length(AToken) <> 32 then HttpError('token must contain 32 lowercase hexadecimal characters');
  for I := 1 to Length(AToken) do
    if not (AToken[I] in ['0'..'9', 'a'..'f']) then
      HttpError('token must contain 32 lowercase hexadecimal characters');
end;

function PlanSeconds(const ASeconds: String): TEnsembleStudioStreamPlan;
var I, Dot: Integer;
begin
  if not IsText(ASeconds) then HttpError('seconds must be text');
  if (Length(ASeconds) = 0) or
      (Length(ASeconds) > ENSEMBLE_STUDIO_HTTP_MAX_TARGET_LENGTH) then
    HttpError('seconds text is empty or too long');
  { Preserve the original decimal spelling, including zero padding. Do not
    trim, percent-decode, convert to floating point, or change its rounding. }
  Dot := 0;
  for I := 1 to Length(ASeconds) do
    if ASeconds[I] = '.' then
    begin
      if (Dot <> 0) or (I = 1) or (I = Length(ASeconds)) then
        HttpError('seconds must be a positive decimal');
      Dot := I;
    end
    else if not (ASeconds[I] in ['0'..'9']) then
      HttpError('seconds must be a positive decimal');
  try Result := PlanEnsembleStudioStream(ASeconds);
  except
    on E: EEnsembleStudioStream do HttpError(E.Message);
    on E: EWfcMusicArrangement do HttpError(E.Message);
  end;
end;

procedure ValidateOptions(const AOptions: TEnsembleStudioStreamOptions);
begin
  if not IsIntegerInRange(AOptions.Seed, 0, 4294967295) then
    HttpError('seed must be an exact UInt32');
  try EnsembleStudioProfileName(AOptions.Profile);
  except on E: EEnsembleStudioProfile do HttpError('unknown profile'); end;
  if not IsIntegerInRange(AOptions.SegmentCellCount, 1,
      High(Integer) div ENSEMBLE_STUDIO_STREAM_QUANTUM) then
    HttpError('segment-cells is outside the supported stream range');
  if not IsIntegerInRange(AOptions.MaxBacktracks, 0, High(Integer)) or
      not IsIntegerInRange(AOptions.MaxPassBacktracks, 0, High(Integer)) then
    HttpError('search allowances must be exact nonnegative Integers');
  if (AOptions.CaptureTrace <> False) and (AOptions.CaptureTrace <> True) then
    HttpError('trace flag must be Boolean');
  {$IFNDEF PAS2JS}
  if Ord(AOptions.CaptureTrace) > 1 then HttpError('trace flag must be Boolean');
  {$ENDIF}
end;

function MakeRequest(const AToken, ASeconds: String;
  const AOptions: TEnsembleStudioStreamOptions): TEnsembleStudioHttpRequest;
begin
  Result := Default(TEnsembleStudioHttpRequest);
  ValidateToken(AToken);
  ValidateOptions(AOptions);
  Result.Plan := PlanSeconds(ASeconds);
  Result.Token := AToken;
  Result.Seconds := ASeconds;
  Result.Options := AOptions;
end;

function FieldKind(const AName: String): THttpField;
begin
  if AName = 'token' then Exit(hfToken);
  if AName = 'seconds' then Exit(hfSeconds);
  if AName = 'seed' then Exit(hfSeed);
  if AName = 'profile' then Exit(hfProfile);
  if AName = 'segment-cells' then Exit(hfSegmentCells);
  if AName = 'backtracks' then Exit(hfBacktracks);
  if AName = 'pass-backtracks' then Exit(hfPassBacktracks);
  if AName = 'trace' then Exit(hfTrace);
  HttpError('unknown query field');
  Result := hfToken;
end;

function ParseEnsembleStudioHttpRequest(const ATarget: String): TEnsembleStudioHttpRequest;
var
  I, Start, Stop, EqualAt: Integer; Field: THttpField;
  Values: THttpFields; Seen: set of THttpField;
  Options: TEnsembleStudioStreamOptions; Prefix: String;
begin
  Result := Default(TEnsembleStudioHttpRequest);
  if not IsText(ATarget) then HttpError('request target must be text');
  if (Length(ATarget) = 0) or
      (Length(ATarget) > ENSEMBLE_STUDIO_HTTP_MAX_TARGET_LENGTH) then
    HttpError('request target is empty or too long');
  Prefix := ENSEMBLE_STUDIO_HTTP_WAVE_PATH + '?';
  if Copy(ATarget, 1, Length(Prefix)) <> Prefix then HttpError('unknown request path');
  { This deliberately is not a form-url-decoder. Only protocol-owned ASCII
    names and exact value grammars can appear, with no encoded aliases. }
  for I := 1 to Length(ATarget) do
    if not (ATarget[I] in ['a'..'z','A'..'Z','0'..'9','/','?','&','=','-','.']) then
      HttpError('request target contains a forbidden character');
  Seen := [];
  Start := Length(Prefix) + 1;
  while Start <= Length(ATarget) do
  begin
    Stop := Start;
    while (Stop <= Length(ATarget)) and (ATarget[Stop] <> '&') do Inc(Stop);
    EqualAt := Start;
    while (EqualAt < Stop) and (ATarget[EqualAt] <> '=') do Inc(EqualAt);
    if (EqualAt = Start) or (EqualAt = Stop) or (EqualAt = Stop - 1) then
      HttpError('query fields require one name and nonempty value');
    Field := FieldKind(Copy(ATarget, Start, EqualAt - Start));
    if Field in Seen then HttpError('duplicate query field');
    Include(Seen, Field);
    Values[Field] := Copy(ATarget, EqualAt + 1, Stop - EqualAt - 1);
    if (Stop <= Length(ATarget)) and (Stop = Length(ATarget)) then
      HttpError('query has an empty trailing field');
    Start := Stop + 1;
  end;
  if Seen <> [Low(THttpField)..High(THttpField)] then
    HttpError('all eight query fields are required');
  Options := Default(TEnsembleStudioStreamOptions);
  try
    Options.Seed := WfcTextParseCanonicalCardinal(Values[hfSeed], 'seed', 'ensemble HTTP');
    Options.Profile := ParseEnsembleStudioProfile(Values[hfProfile]);
    Options.SegmentCellCount := WfcTextParseCanonicalInteger(Values[hfSegmentCells], 'segment-cells', 'ensemble HTTP');
    Options.MaxBacktracks := WfcTextParseCanonicalInteger(Values[hfBacktracks], 'backtracks', 'ensemble HTTP');
    Options.MaxPassBacktracks := WfcTextParseCanonicalInteger(Values[hfPassBacktracks], 'pass-backtracks', 'ensemble HTTP');
  except
    on E: EConvertError do HttpError(E.Message);
    on E: EEnsembleStudioProfile do HttpError('unknown profile');
  end;
  if Values[hfTrace] = '0' then Options.CaptureTrace := False
  else if Values[hfTrace] = '1' then Options.CaptureTrace := True
  else HttpError('trace must be exactly 0 or 1');
  Result := MakeRequest(Values[hfToken], Values[hfSeconds], Options);
end;

function BuildEnsembleStudioHttpTarget(const AToken, ASeconds: String;
  const AOptions: TEnsembleStudioStreamOptions): String;
var R: TEnsembleStudioHttpRequest; Trace: String;
begin
  R := MakeRequest(AToken, ASeconds, AOptions);
  if R.Options.CaptureTrace then Trace := '1' else Trace := '0';
  Result := ENSEMBLE_STUDIO_HTTP_WAVE_PATH + '?token=' + R.Token +
    '&seconds=' + R.Seconds + '&seed=' + UIntToStr(R.Options.Seed) +
    '&profile=' + EnsembleStudioProfileName(R.Options.Profile) +
    '&segment-cells=' + IntToStr(R.Options.SegmentCellCount) +
    '&backtracks=' + IntToStr(R.Options.MaxBacktracks) +
    '&pass-backtracks=' + IntToStr(R.Options.MaxPassBacktracks) +
    '&trace=' + Trace;
  if Length(Result) > ENSEMBLE_STUDIO_HTTP_MAX_TARGET_LENGTH then
    HttpError('request target exceeds its length limit');
end;

procedure ValidatePlan(const APlan: TEnsembleStudioStreamPlan);
var P: TEnsembleStudioStreamPlan;
begin
  P := PlanSeconds(APlan.RequestedText);
  if (APlan.RequestedTicks <> P.RequestedTicks) or
      (APlan.ActualTicks <> P.ActualTicks) or (APlan.CellCount <> P.CellCount) or
      (APlan.ExpectedFrames <> P.ExpectedFrames) then HttpError('stream plan is inconsistent');
end;

function EnsembleStudioHttpWaveBytes(const APlan: TEnsembleStudioStreamPlan):
  TEnsembleStudioHttpByteCount;
const RiffMaximum: TEnsembleStudioHttpByteCount = 4294967295;
begin
  ValidatePlan(APlan);
  Result := TEnsembleStudioHttpByteCount(APlan.ExpectedFrames) * 2;
  if APlan.ExpectedFrames > (RiffMaximum - 36) div 2 then Inc(Result, 80)
  else Inc(Result, 44);
end;

function EnsembleStudioHttpFileName(const ARequest: TEnsembleStudioHttpRequest): String;
var Verified: TEnsembleStudioHttpRequest; UnusedTarget: String;
begin
  { Recheck all caller-owned request fields before constructing a header value;
    this also enforces the same overall query envelope as actual requests. }
  UnusedTarget := BuildEnsembleStudioHttpTarget(ARequest.Token, ARequest.Seconds, ARequest.Options);
  Verified := ParseEnsembleStudioHttpRequest(UnusedTarget);
  ValidatePlan(ARequest.Plan);
  if (ARequest.Plan.RequestedText <> Verified.Plan.RequestedText) or
      (ARequest.Plan.RequestedTicks <> Verified.Plan.RequestedTicks) or
      (ARequest.Plan.ActualTicks <> Verified.Plan.ActualTicks) or
      (ARequest.Plan.CellCount <> Verified.Plan.CellCount) or
      (ARequest.Plan.ExpectedFrames <> Verified.Plan.ExpectedFrames) then
    HttpError('request seconds and stream plan disagree');
  Result := 'ensemble-' + UIntToStr(ARequest.Options.Seed) + '-' +
    EnsembleStudioProfileName(ARequest.Options.Profile) + '-' +
    IntToStr(ARequest.Plan.ActualTicks) + 'ticks.wav';
  if Length(Result) > 128 then HttpError('download file name exceeds its limit');
end;

end.
