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
program wfc_training_value_quota_text_test;
{$mode delphi}{$H+}
uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc_model, wfc_training, wfc_training_text;
const
  SOURCE_PREFIX = 'wfclearn=3'#10'name=quota-source'#10'license=MIT'#10 +
    'source=authored'#10'kind=adjacency1d'#10'boundary=wrap'#10 +
    'symmetry=none'#10'footprint=0,0'#10'order=0'#10'samples=1'#10 +
    'sample=0,2,1,1,source'#10'token=0,0,A'#10'token=0,1,B'#10;
  QUOTA_SECTION = 'value-quota-version=1'#10'value-quotas=1'#10 +
    'value-quota=0,both,1,2,2'#10'quota-token=0,0,B'#10'quota-token=0,1,A'#10;
  SOURCE_TEXT = SOURCE_PREFIX + QUOTA_SECTION + 'end'#10;
var
  Checks: Integer = 0;

procedure Check(const ACondition: Boolean; const ALabel: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(ALabel);
end;

function Change(const AOld, ANew: String): String;
begin
  Check(Pos(AOld, SOURCE_TEXT) > 0, 'mutation names an existing source fragment');
  Result := StringReplace(SOURCE_TEXT, AOld, ANew, []);
end;

procedure Reject(const AText, ALabel: String; const AMessage: String = '');
var
  LDocument: TWfcTrainingDocument;
  LRejected: Boolean;
begin
  LDocument := nil;
  LRejected := False;
  try
    try
      LDocument := DecodeWfcTrainingText(AText);
    except
      on E: EConvertError do
        LRejected := (AMessage = '') or (Pos(AMessage, E.Message) > 0);
    end;
  finally
    LDocument.Free;
  end;
  Check(LRejected, ALabel);
end;

procedure TestRoundTrips;
var
  LDocument, LAgain, LLegacy: TWfcTrainingDocument;
  LQuota: TWfcTrainingValueQuota;
  LText: String;
  LKind: TWfcTrainingKind;
  LOptions: TWfcTrainingOptions;
begin
  LDocument := DecodeWfcTrainingText(SOURCE_TEXT);
  try
    Check(EncodeWfcTrainingText(LDocument) = SOURCE_TEXT, 'exact version-three bytes');
    Check(WfcTrainingDocumentTextVersion(LDocument) = 3, 'actual version three');
    LQuota := LDocument.ValueQuotaAt(0);
    Check((LQuota.Values[0] = 'B') and (LQuota.Values[1] = 'A'),
      'source retains authored token order rather than vocabulary indices');
    LAgain := TWfcTrainingDocument.Create(LDocument.CopyMetadata,
      LDocument.CopyOptions, LDocument.CopySamples, LDocument.CopyValueQuotas);
    try
      Check(LAgain.Signature = LDocument.Signature, 'typed reconstruction identity');
      Check(EncodeWfcTrainingText(LAgain) = SOURCE_TEXT, 'typed reconstruction text');
    finally LAgain.Free; end;
    for LKind := Low(TWfcTrainingKind) to High(TWfcTrainingKind) do
    begin
      LOptions := MakeWfcTrainingOptions(LKind, wmbWrap, wmsNone, 0, 0, 0);
      if LKind = wtkSequence then
      begin LOptions.Boundary := wmbOpen; LOptions.Order := 1; end;
      if LKind in [wtkPattern2D, wtkPattern3D] then
      begin LOptions.PatternWidth := 1; LOptions.PatternHeight := 1; end;
      LAgain := TWfcTrainingDocument.Create(LDocument.CopyMetadata,
        LOptions, LDocument.CopySamples, LDocument.CopyValueQuotas);
      try
        LText := EncodeWfcTrainingText(LAgain);
        Check(Pos('sample=0,2,1,1,source'#10, LText) > 0,
          'v3 sample always carries explicit depth');
        LLegacy := DecodeWfcTrainingText(LText);
        try Check((LLegacy.Signature = LAgain.Signature) and
          (EncodeWfcTrainingText(LLegacy) = LText), 'every training kind round trips v3');
        finally LLegacy.Free; end;
        LLegacy := TWfcTrainingDocument.Create(LAgain.CopyMetadata,
          LAgain.CopyOptions, LAgain.CopySamples, nil);
        try
          if LKind = wtkPattern3D then
            Check(WfcTrainingDocumentTextVersion(LLegacy) = 6, 'pattern volume stays v6')
          else if LKind = wtkAdjacency3D then
            Check(WfcTrainingDocumentTextVersion(LLegacy) = 2, 'empty volume stays v2')
          else
            Check(WfcTrainingDocumentTextVersion(LLegacy) = 1, 'empty legacy stays v1');
          if LKind = wtkPattern3D then
            Check(Pos('value-quota-version=0'#10'value-quotas=0'#10,
              EncodeWfcTrainingText(LLegacy)) > 0, 'v6 retains explicit empty policy section')
          else
            Check(Pos('value-quota', EncodeWfcTrainingText(LLegacy)) = 0,
              'removing last quota removes the feature section');
        finally LLegacy.Free; end;
      finally LAgain.Free; end;
    end;
  finally LDocument.Free; end;
  LText := StringReplace(SOURCE_TEXT, 'both', '%F0%9F%8E%B5%2C%25%0A', []);
  LText := StringReplace(LText, ',B'#10, ',%F0%9F%9A%80%20%2C%25%0A'#10, [rfReplaceAll]);
  LDocument := DecodeWfcTrainingText(LText);
  try Check(EncodeWfcTrainingText(LDocument) = LText,
    'supplementary Unicode, whitespace and punctuation retain exact identity');
  finally LDocument.Free; end;
end;

procedure TestHostileText;
var
  I: Integer;
  LBad, LText: String;
begin
  Reject(Change('wfclearn=3', 'wfclearn=4'), 'unknown header');
  Reject(Change('wfclearn=3', 'wfclearn=1'), 'v3 fields cannot enter v1');
  Reject(Change('wfclearn=3', 'wfclearn=2'), 'v2 remains volume-only');
  Reject(Change('value-quota-version=1', 'value-quota-version=2'), 'unknown capability');
  Reject(Change('value-quotas=1', 'value-quotas=0'), 'v3 requires nonempty quotas');
  Reject(Change('value-quotas=1', 'value-quotas=4097'), 'quota count bound');
  Reject(Change('value-quotas=1', 'value-quotas=4096'), 'remaining records before allocation');
  Reject(Change('value-quota=0,both', 'value-quota=1,both'), 'ordered quota indices');
  Reject(Change('value-quota=0,both', 'value-quota=0,'), 'nonempty label');
  Reject(Change('value-quota=0,both', 'value-quota=0,%62oth'), 'canonical label escape');
  Reject(Change('value-quota=0,both,1,2,2', 'value-quota=0,both,3,2,2'), 'ordered bounds');
  Reject(Change('value-quota=0,both,1,2,2', 'value-quota=0,both,1,2,0'), 'nonempty token set');
  Reject(Change('value-quota=0,both,1,2,2', 'value-quota=0,both,1,2,1025'), 'per-quota cap');
  Reject(Change('value-quota=0,both,1,2,2', 'value-quota=0,both,1,2,1024'), 'tokens preflight available lines');
  Reject(Change('quota-token=0,0,B', 'quota-token=1,0,B'), 'parent index');
  Reject(Change('quota-token=0,0,B', 'quota-token=0,1,B'), 'token order');
  Reject(Change('quota-token=0,0,B', 'quota-token=0,0,A'), 'duplicate set token');
  Reject(Change('quota-token=0,0,B', 'quota-token=0,0,unknown'), 'unknown source token');
  Reject(Change('quota-token=0,0,B', 'quota-token=0,0,'), 'nonempty token');
  Reject(Change('quota-token=0,0,B', 'quota-token=0,0,%ED%A0%80'), 'surrogate token');
  Reject(Change('quota-token=0,0,B', 'quota-token=0,0,%c3%A9'), 'lowercase token escape');
  Reject(Change('quota-token=0,0,B', 'quota-token=0,0,B,extra'), 'extra fields');
  Reject(Change('quota-token=0,0,B', 'quota-token=0,B'), 'missing fields');
  Reject(Change('sample=0,2,1,1,source', 'sample=0,2,1,2,source'), 'nonvolume depth is one');
  Reject(Change('sample=0,2,1,1,source', 'sample=0,2,1,source'), 'v3 depth is mandatory');
  Reject(StringReplace(SOURCE_TEXT, #10, #13#10, [rfReplaceAll]), 'CRLF rejected');
  Reject(SOURCE_TEXT + 'extra'#10, 'trailing data');
  Reject(SOURCE_TEXT + #10, 'extra newline');
  for I := 0 to Length(SOURCE_TEXT) - 1 do
    Reject(Copy(SOURCE_TEXT, 1, I), 'truncated source ' + IntToStr(I));
  for I := 0 to 8 do
  begin
    case I of
      0: LBad := '-1'; 1: LBad := '+1'; 2: LBad := '01';
      3: LBad := '1.0'; 4: LBad := '1e0'; 5: LBad := 'NaN';
      6: LBad := 'Infinity'; 7: LBad := '2147483648'; 8: LBad := ' 1';
    end;
    Reject(Change('value-quota=0,both,1,2,2',
      'value-quota=0,both,' + LBad + ',2,2'), 'exact integer ' + LBad);
  end;
  LText := Change('value-quotas=1', 'value-quotas=2');
  LText := StringReplace(LText, 'end'#10,
    'value-quota=1,both,0,2,1'#10'quota-token=1,0,A'#10'end'#10, []);
  Reject(LText, 'duplicate label across source-owned quotas');
  Check((WFC_TRAINING_MAX_TEXT_LINE_COUNT = 69643) and
    (WFC_TRAINING_VALUE_QUOTA_MAX_TEXT_LINE_COUNT = 139277), 'versioned line envelopes');
  Reject('wfclearn=1'#10 + StringOfChar(#10, WFC_TRAINING_MAX_TEXT_LINE_COUNT),
    'old line limit', 'line count');
  Reject('wfclearn=3'#10 + StringOfChar(#10, WFC_TRAINING_VALUE_QUOTA_MAX_TEXT_LINE_COUNT),
    'new line limit', 'line count');
  Reject(StringOfChar('x', WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH + 1),
    'text size preflight', 'encoded length');
  Reject(Change('both', StringOfChar('x', WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH + 1)),
    'quota labels share token length budget', 'encoded length');
  LText := SOURCE_PREFIX + 'value-quota-version=1'#10'value-quotas=65'#10;
  for I := 0 to 64 do
    LText := LText + 'value-quota=' + IntToStr(I) + ',' + IntToStr(I) +
      StringOfChar('x', 65530) + ',0,2,1'#10'quota-token=' + IntToStr(I) + ',0,A'#10;
  Reject(LText + 'end'#10, 'quota/source tokens share aggregate byte budget', 'aggregate encoded tokens');
end;

begin
  try
    TestRoundTrips;
    TestHostileText;
    WriteLn('Training value-quota text checks: ', Checks);
  except
    on E: Exception do
    begin WriteLn('FAIL: ', E.ClassName, ': ', E.Message); Halt(1); end;
  end;
end.
