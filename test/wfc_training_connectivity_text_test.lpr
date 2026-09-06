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
program wfc_training_connectivity_text_test;
{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_text_codec, wfc_training, wfc_training_text,
  wfc_pipeline_model, wfc_pipeline_text;

var Checks: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;

function Tokens(const AValues: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

function Fixture(const AKind: TWfcTrainingKind; const AQuotas: Boolean;
  const AMask: Integer = 10): TWfcTrainingDocument;
var S: TWfcTrainingSamples; O: TWfcTrainingOptions;
  Q: TWfcTrainingValueQuotas; C: TWfcTrainingConnectivities;
  P: TWfcTrainingConnectivityValues; T: TGraphPositions;
  R: TGraphPosition; Ports: TGraphDirections; Cafe: TWfcModelToken;
begin
  Cafe := WfcTextDecodeToken('caf%C3%A9', 'fixture');
  SetLength(S, 1);
  O := MakeWfcTrainingOptions(AKind, wmbWrap, wmsNone, 0, 0, 0);
  case AKind of
    wtkAdjacency1D, wtkSequence:
      S[0] := MakeWfcTrainingSample('line', 4, 1, Tokens(['road', Cafe, 'road', Cafe]));
    wtkAdjacency2D, wtkPattern2D:
      S[0] := MakeWfcTrainingSample('plane', 2, 2, Tokens(['road', Cafe, Cafe, 'road']));
    wtkAdjacency3D:
      S[0] := MakeWfcTrainingSample('volume', 2, 2, 2,
        Tokens(['road', Cafe, Cafe, 'road', Cafe, 'road', 'road', Cafe]));
  end;
  if AKind = wtkPattern2D then begin O.PatternWidth := 1; O.PatternHeight := 1; end;
  if AKind = wtkSequence then begin O.Boundary := wmbOpen; O.Order := 1; end;
  if AQuotas then
  begin
    SetLength(Q, 1);
    Q[0] := MakeWfcTrainingValueQuota('some-road', Tokens(['road', Cafe]), 0, 99);
  end;
  Ports := [];
  if (AMask and 1) <> 0 then Include(Ports, gdNorth);
  if (AMask and 2) <> 0 then Include(Ports, gdEast);
  if (AMask and 4) <> 0 then Include(Ports, gdSouth);
  if (AMask and 8) <> 0 then Include(Ports, gdWest);
  if (AMask and 16) <> 0 then Include(Ports, gdUp);
  if (AMask and 32) <> 0 then Include(Ports, gdDown);
  R := Default(TGraphPosition); SetLength(T, 1); T[0] := R; T[0].X := 1;
  SetLength(P, 2);
  { Deliberately not the sample's first-seen public vocabulary order. }
  P[0] := MakeWfcTrainingConnectivityValue(Cafe, Ports, True);
  P[1] := MakeWfcTrainingConnectivityValue('road', [gdEast, gdWest], False);
  SetLength(C, 1);
  C[0] := MakeWfcTrainingConnectivity('route', R, T, P, True);
  Result := TWfcTrainingDocument.Create(MakeWfcTrainingMetadata('ports', 'MIT',
    'authored, not inferred'), O, S, Q, C);
end;

function ReplaceOne(const AText, AOld, ANew: String): String;
begin
  Check(Pos(AOld, AText) > 0, 'mutation needle exists: ' + AOld);
  Result := StringReplace(AText, AOld, ANew, []);
end;

procedure Reject(const AText, AName: String);
var D: TWfcTrainingDocument; Raised: Boolean;
begin
  D := nil; Raised := False;
  try
    try D := DecodeWfcTrainingText(AText);
    except on E: EConvertError do Raised := True; end;
    Check(Raised and (D = nil), 'reject without owned result: ' + AName);
  finally D.Free; end;
end;

procedure TestRoundTrips;
var K: TWfcTrainingKind; HasQuota: Boolean; D, Loaded, Old: TWfcTrainingDocument;
  TextValue, OldText: String; Recipe: TWfcPipelineModel; Mask: Integer;
begin
  for K := Low(TWfcTrainingKind) to High(TWfcTrainingKind) do
    for HasQuota := False to True do
    begin
      D := Fixture(K, HasQuota);
      try
        TextValue := EncodeWfcTrainingText(D);
        Check(Pos('wfclearn=4'#10, TextValue) = 1, 'new profile explicitly selects v4');
        if HasQuota then
          Check(Pos('value-quota-version=1'#10'value-quotas=1'#10, TextValue) > 0, 'nonempty quota section')
        else
          Check(Pos('value-quota-version=0'#10'value-quotas=0'#10, TextValue) > 0, 'explicit empty quota section');
        Loaded := DecodeWfcTrainingText(TextValue);
        try
          Check(Loaded.Signature = D.Signature, 'source fingerprint preserved');
          Check(EncodeWfcTrainingText(Loaded) = TextValue, 'canonical byte replay');
          Check(Loaded.ConnectivityAt(0).Values[0].Value =
            WfcTextDecodeToken('caf%C3%A9', 'fixture'), 'authored Unicode profile order preserved');
          Recipe := LearnWfcTrainingRecipe(Loaded);
          try
            Check(Pos('wfcpipeline=3'#10, EncodeWfcPipelineModelText(Recipe)) = 1, 'lowered recipe carries connectivity');
            Check(Recipe.ConnectivityCount = 1, 'one public connectivity descriptor');
          finally Recipe.Free; end;
        finally Loaded.Free; end;
        Old := TWfcTrainingDocument.Create(D.CopyMetadata, D.CopyOptions,
          D.CopySamples, D.CopyValueQuotas);
        try
          OldText := EncodeWfcTrainingText(Old);
          if HasQuota then Check(Pos('wfclearn=3'#10, OldText) = 1, 'quota-only keeps v3')
          else if K = wtkAdjacency3D then Check(Pos('wfclearn=2'#10, OldText) = 1, 'volume keeps v2')
          else Check(Pos('wfclearn=1'#10, OldText) = 1, 'legacy kind keeps v1');
          Loaded := TWfcTrainingDocument.Create(D.CopyMetadata, D.CopyOptions,
            D.CopySamples, D.CopyValueQuotas, nil);
          try
            Check((Loaded.Signature = Old.Signature) and
              (EncodeWfcTrainingText(Loaded) = OldText), 'explicit empty extension preserves old bytes and identity');
          finally Loaded.Free; end;
        finally Old.Free; end;
      finally D.Free; end;
    end;
  for Mask := 0 to 63 do
  begin
    D := Fixture(wtkAdjacency1D, False, Mask);
    try
      TextValue := EncodeWfcTrainingText(D);
      Check(Pos('profile=0,0,caf%C3%A9,' + IntToStr(Mask) + ',true'#10, TextValue) > 0,
        'all six opening bits use the portable mask');
      Loaded := DecodeWfcTrainingText(TextValue);
      try
        Check((Loaded.Signature = D.Signature) and
          (Loaded.ConnectivityAt(0).Values[0].Openings = D.ConnectivityAt(0).Values[0].Openings),
          'all mask combinations round-trip even at rank one');
      finally Loaded.Free; end;
    finally D.Free; end;
  end;
end;

procedure TestMalformed;
var D: TWfcTrainingDocument; S, Q: String; I: Integer;
begin
  D := Fixture(wtkAdjacency1D, False);
  try S := EncodeWfcTrainingText(D); finally D.Free; end;
  D := Fixture(wtkAdjacency1D, True);
  try Q := EncodeWfcTrainingText(D); finally D.Free; end;
  for I := 0 to Length(S) - 1 do Reject(Copy(S, 1, I), 'proper source prefix');
  Reject(S + 'end'#10, 'trailing record');
  Reject(StringReplace(S, #10, #13#10, [rfReplaceAll]), 'CRLF is not canonical');
  Reject(ReplaceOne(S, 'wfclearn=4', 'wfclearn=3'), 'extension smuggled into v3');
  Reject(ReplaceOne(S, 'wfclearn=4', 'wfclearn=2'), 'extension smuggled into v2');
  Reject(ReplaceOne(S, 'wfclearn=4', 'wfclearn=1'), 'extension smuggled into v1');
  Reject(ReplaceOne(S, 'wfclearn=4', 'wfclearn=5'), 'unknown source version');
  Reject(ReplaceOne(S, 'value-quota-version=0', 'value-quota-version=1'), 'quota1 with empty registry');
  Reject(ReplaceOne(Q, 'value-quota-version=1', 'value-quota-version=0'), 'quota0 with nonempty registry');
  Reject(ReplaceOne(S, 'connectivity-version=1', 'connectivity-version=0'), 'connectivity capability zero');
  Reject(ReplaceOne(S, 'connectivity-version=1', 'connectivity-version=2'), 'unknown connectivity capability');
  Reject(ReplaceOne(S, 'connectivities=1', 'connectivities=0'), 'v4 empty registry');
  Reject(ReplaceOne(S, 'connectivities=1', 'connectivities=4097'), 'descriptor cap');
  Reject(ReplaceOne(S, 'connectivities=1', 'connectivities=4096'), 'descriptor remaining-record preflight');
  Reject(ReplaceOne(S, 'connectivity=0,route,0,0,0,true,1,2',
    'connectivity=1,route,0,0,0,true,1,2'), 'descriptor ordinal');
  Reject(ReplaceOne(S, 'connectivity=0,route,0,0,0,true,1,2',
    'connectivity=0,,0,0,0,true,1,2'), 'empty label');
  Reject(ReplaceOne(S, 'route,0,0,0,true,1,2', 'route,-1,0,0,true,1,2'), 'negative root');
  Reject(ReplaceOne(S, 'route,0,0,0,true,1,2', 'route,0,1,0,true,1,2'), 'rank-one root Y');
  Reject(ReplaceOne(S, 'route,0,0,0,true,1,2', 'route,0,0,0,TRUE,1,2'), 'noncanonical boolean');
  Reject(ReplaceOne(S, 'route,0,0,0,true,1,2', 'route,0,0,0,1,1,2'), 'numeric boolean');
  Reject(ReplaceOne(S, 'true,1,2'#10, 'true,65537,2'#10), 'terminal cap');
  Reject(ReplaceOne(S, 'true,1,2'#10, 'true,65536,2'#10), 'terminal remaining-record preflight');
  Reject(ReplaceOne(S, 'true,1,2'#10, 'true,1,1025'#10), 'profile cap');
  Reject(ReplaceOne(S, 'true,1,2'#10, 'true,1,0'#10), 'empty profile registry');
  Reject(ReplaceOne(S, 'terminal=0,0,1,0,0', 'terminal=0,1,1,0,0'), 'terminal ordinal');
  Reject(ReplaceOne(S, 'terminal=0,0,1,0,0', 'terminal=0,0,0,0,0'), 'terminal repeats root');
  Reject(ReplaceOne(S, 'terminal=0,0,1,0,0', 'terminal=0,0,1,0,1'), 'rank-one terminal Z');
  Reject(ReplaceOne(S, 'terminal=0,0,1,0,0', 'terminal=0,0,2147483648,0,0'), 'coordinate overflow');
  Reject(ReplaceOne(S, 'profile=0,0,caf%C3%A9,10,true', 'profile=0,0,caf%C3%A9,64,true'), 'mask outside six bits');
  Reject(ReplaceOne(S, 'profile=0,0,caf%C3%A9,10,true', 'profile=0,0,caf%C3%A9,010,true'), 'mask leading zero');
  Reject(ReplaceOne(S, 'profile=0,0,caf%C3%A9,10,true', 'profile=0,0,caf%C3%A9,10,yes'), 'required flag syntax');
  Reject(ReplaceOne(S, 'profile=0,1,road,10,false', 'profile=0,0,road,10,false'), 'profile ordinal');
  Reject(ReplaceOne(S, 'profile=0,1,road,10,false', 'profile=0,1,caf%C3%A9,10,false'), 'duplicate profile token');
  Reject(ReplaceOne(S, 'profile=0,1,road,10,false', 'profile=0,1,missing,10,false'), 'unknown observed profile token');
  Reject(ReplaceOne(S, 'profile=0,1,road,10,false', 'profile=0,1,,10,false'), 'empty profile token');
  Reject(ReplaceOne(S, 'profile=0,0,caf%C3%A9,10,true', 'profile=0,0,caf%c3%a9,10,true'), 'noncanonical percent escaping');
  Reject(ReplaceOne(S, 'profile=0,1,road,10,false', 'profile=0,1,%FF,10,false'), 'malformed UTF-8');
  Reject(ReplaceOne(S, 'profile=0,1,road,10,false',
    'profile=0,1,' + StringOfChar('x', WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH + 1) + ',10,false'), 'profile encoded token cap');
  Reject('wfclearn=4'#10 + StringOfChar(' ', WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH), 'whole source byte cap');
  Reject('wfclearn=4'#10 + StringOfChar(#10, WFC_TRAINING_CONNECTIVITY_MAX_TEXT_LINE_COUNT), 'v4 line preflight');
  Check(WFC_TRAINING_MAX_TEXT_LINE_COUNT = 69643, 'v1-v2 line envelope unchanged');
  Check(WFC_TRAINING_VALUE_QUOTA_MAX_TEXT_LINE_COUNT = 139277, 'v3 line envelope unchanged');
  Check(WFC_TRAINING_CONNECTIVITY_MAX_TEXT_LINE_COUNT = 274447, 'v4 line envelope includes both registries');
end;

begin
  TestRoundTrips;
  TestMalformed;
  WriteLn('[SUMMARY] checks=', Checks, ' failures=0');
end.
