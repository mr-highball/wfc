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
program wfc_training_text_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  {$IFNDEF PAS2JS}Classes,{$ENDIF}
  wfc_model,
  wfc_training,
  wfc_training_text;

const
  ADJACENCY1D_TEXT =
    'wfclearn=1'#10 +
    'name=alternating-sequences'#10 +
    'license=MIT'#10 +
    'source=project-authored%20training%20example'#10 +
    'kind=adjacency1d'#10 +
    'boundary=wrap'#10 +
    'symmetry=none'#10 +
    'footprint=0,0'#10 +
    'order=0'#10 +
    'samples=2'#10 +
    'sample=0,4,1,forward'#10 +
    'token=0,0,A'#10 +
    'token=0,1,B'#10 +
    'token=0,2,A'#10 +
    'token=0,3,B'#10 +
    'sample=1,4,1,reverse'#10 +
    'token=1,0,B'#10 +
    'token=1,1,A'#10 +
    'token=1,2,B'#10 +
    'token=1,3,A'#10 +
    'end'#10;
  ADJACENCY2D_TEXT =
    'wfclearn=1'#10 +
    'name=checkerboard-adjacency'#10 +
    'license=MIT'#10 +
    'source=project-authored%20training%20example'#10 +
    'kind=adjacency2d'#10 +
    'boundary=wrap'#10 +
    'symmetry=d4'#10 +
    'footprint=0,0'#10 +
    'order=0'#10 +
    'samples=2'#10 +
    'sample=0,2,2,square'#10 +
    'token=0,0,A'#10 +
    'token=0,1,B'#10 +
    'token=0,2,B'#10 +
    'token=0,3,A'#10 +
    'sample=1,4,2,rectangle'#10 +
    'token=1,0,A'#10 +
    'token=1,1,B'#10 +
    'token=1,2,A'#10 +
    'token=1,3,B'#10 +
    'token=1,4,B'#10 +
    'token=1,5,A'#10 +
    'token=1,6,B'#10 +
    'token=1,7,A'#10 +
    'end'#10;
  PATTERN2D_TEXT =
    'wfclearn=1'#10 +
    'name=checkerboard-patterns'#10 +
    'license=MIT'#10 +
    'source=project-authored%20training%20example'#10 +
    'kind=pattern2d'#10 +
    'boundary=wrap'#10 +
    'symmetry=d4'#10 +
    'footprint=2,2'#10 +
    'order=0'#10 +
    'samples=2'#10 +
    'sample=0,2,2,square'#10 +
    'token=0,0,A'#10 +
    'token=0,1,B'#10 +
    'token=0,2,B'#10 +
    'token=0,3,A'#10 +
    'sample=1,4,2,rectangle'#10 +
    'token=1,0,A'#10 +
    'token=1,1,B'#10 +
    'token=1,2,A'#10 +
    'token=1,3,B'#10 +
    'token=1,4,B'#10 +
    'token=1,5,A'#10 +
    'token=1,6,B'#10 +
    'token=1,7,A'#10 +
    'end'#10;
  SEQUENCE_TEXT =
    'wfclearn=1'#10 +
    'name=short-phrases'#10 +
    'license=MIT'#10 +
    'source=project-authored%20training%20example'#10 +
    'kind=sequence'#10 +
    'boundary=open'#10 +
    'symmetry=none'#10 +
    'footprint=0,0'#10 +
    'order=2'#10 +
    'samples=2'#10 +
    'sample=0,3,1,first'#10 +
    'token=0,0,red'#10 +
    'token=0,1,fox'#10 +
    'token=0,2,.'#10 +
    'sample=1,3,1,second'#10 +
    'token=1,0,caf%C3%A9'#10 +
    'token=1,1,fox'#10 +
    'token=1,2,.'#10 +
    'end'#10;

var
  GChecks: Integer = 0;
  GFailures: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GChecks);
  if not ACondition then
  begin
    Inc(GFailures);
    WriteLn('[FAIL] ', AMessage);
  end;
end;

procedure Reject(const AText, ALabel: String);
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
      on E: EConvertError do LRejected := True;
    end;
  finally
    LDocument.Free;
  end;
  Check(LRejected, ALabel);
end;

function Changed(const AOld, ANew: String): String;
begin
  Result := StringReplace(ADJACENCY1D_TEXT, AOld, ANew, []);
end;

{$IFNDEF PAS2JS}
function ReadBytes(const APath: String): String;
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    if LFile.Size > WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH then
      raise Exception.Create('test fixture exceeds input limit');
    SetLength(Result, Integer(LFile.Size));
    if Result <> '' then LFile.ReadBuffer(Result[1], Length(Result));
  finally
    LFile.Free;
  end;
end;
{$ENDIF}

procedure RoundTrip(const AText, AFileName: String;
  const AKind: TWfcTrainingKind);
var
  LDocument: TWfcTrainingDocument;
  LAgain: TWfcTrainingDocument;
begin
  LDocument := DecodeWfcTrainingText(AText);
  try
    Check(LDocument.CopyOptions.Kind = AKind, AFileName + ': profile');
    Check(EncodeWfcTrainingText(LDocument) = AText,
      AFileName + ': canonical byte identity');
    LAgain := DecodeWfcTrainingText(EncodeWfcTrainingText(LDocument));
    try
      Check(LAgain.Signature = LDocument.Signature,
        AFileName + ': derived fingerprint identity');
    finally
      LAgain.Free;
    end;
    {$IFNDEF PAS2JS}
    Check(ReadBytes(IncludeTrailingPathDelimiter(ParamStr(1)) +
      AFileName + '.wfclearn') = AText,
      AFileName + ': checked-in source equals portable fixture');
    {$ENDIF}
  finally
    LDocument.Free;
  end;
end;

procedure TestMalformed;
var
  LText: String;
begin
  Reject('', 'empty input');
  Reject(Copy(ADJACENCY1D_TEXT, 1, Length(ADJACENCY1D_TEXT) - 1),
    'missing final LF');
  Reject(ADJACENCY1D_TEXT + #10, 'extra blank line');
  Reject(StringReplace(ADJACENCY1D_TEXT, #10, #13#10, [rfReplaceAll]),
    'CRLF input');
  Reject(Changed('wfclearn=1', 'wfclearn=7'), 'unknown version');
  Reject(Changed('wfclearn=1', 'wfclearn=2'),
    'v2 cannot be selected by relabeling an adjacency1d source');
  Reject(Changed('name=', 'unknown='), 'unknown field');
  Reject(Changed('kind=adjacency1d', 'kind=voxel3d'), 'unknown kind');
  Reject(Changed('boundary=wrap', 'boundary=periodic'), 'unknown boundary');
  Reject(Changed('symmetry=none', 'symmetry=rotate'), 'unknown symmetry');
  Reject(Changed('samples=2', 'samples=02'), 'leading zero');
  Reject(Changed('samples=2', 'samples=2147483648'), 'integer overflow');
  Reject(Changed('samples=2', 'samples=4097'), 'too many samples');
  Reject(Changed('samples=2', 'samples=3'), 'lying sample count');
  Reject(Changed('sample=0,4,1,', 'sample=1,4,1,'),
    'unordered sample index');
  Reject(Changed('sample=0,4,1,', 'sample=0,65536,65536,'),
    'area bound before allocation');
  Reject(Changed('sample=0,4,1,', 'sample=0,65536,1,'),
    'missing declared token records before allocation');
  Reject(Changed('sample=0,4,1,', 'sample=0,0,1,'), 'zero width');
  Reject(Changed('sample=0,4,1,', 'sample=0,-4,1,'), 'negative width');
  Reject(Changed('sample=0,4,1,', 'sample=0,4,2,'), 'token area mismatch');
  Reject(Changed('token=0,0,A', 'token=0,1,A'), 'unordered token index');
  Reject(Changed('token=0,0,A', 'token=1,0,A'), 'wrong token sample');
  Reject(Changed('token=0,0,A', 'token=0,0,A,extra'), 'extra record field');
  Reject(Changed('token=0,0,A', 'token=0,A'), 'missing record field');
  Reject(Changed('token=0,0,A', 'token=0,0,'), 'empty token');
  Reject(Changed('token=0,0,A', 'token=0,0,%41'), 'unnecessary escape');
  Reject(Changed('token=0,0,A', 'token=0,0,%c3%A9'), 'lowercase escape');
  Reject(Changed('token=0,0,A', 'token=0,0,%C0%AF'), 'malformed UTF-8');
  Reject(Changed('token=0,0,A', 'token=0,0,%ED%A0%80'),
    'surrogate code point');
  Reject(Changed('token=0,0,A', 'token=0,0,A B'), 'unescaped space');
  Reject(Changed('token=0,0,A', 'token=0,0,' + Chr(128)),
    'non-ASCII source byte');
  Reject(Changed('license=MIT', 'license='), 'missing license declaration');
  Reject(Changed('source=project-authored%20training%20example', 'source='),
    'missing source declaration');
  Reject(Changed('sample=1,4,1,reverse', 'sample=1,4,1,forward'),
    'duplicate sample name');
  Reject(Changed('footprint=0,0', 'footprint=2,2'),
    'irrelevant footprint cannot silently change semantics');
  Reject(Changed('order=0', 'order=2'), 'irrelevant sequence order');
  Reject(ADJACENCY1D_TEXT + 'extra=record'#10, 'trailing record');
  LText := Changed('token=0,0,A', 'token=0,0,' +
    StringOfChar('A', WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH + 1));
  Reject(LText, 'encoded token bound');
  Reject(StringOfChar('x', WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH + 1),
    'encoded document bound before line parsing');
  Reject(StringOfChar(#10, WFC_TRAINING_MAX_TEXT_LINE_COUNT + 1),
    'line count bound before line allocation');
end;

procedure TestUnicodeAndEscapes;
var
  LText: String;
  LDocument: TWfcTrainingDocument;
  LSample: TWfcTrainingSample;
begin
  LText := Changed('token=0,0,A',
    'token=0,0,%F0%9F%8E%B5%2C%0A%23%25');
  LDocument := DecodeWfcTrainingText(LText);
  try
    LSample := LDocument.SampleAt(0);
    Check(WfcModelTokenIsValid(LSample.Tokens[0]),
      'supplementary Unicode and escaped punctuation remain valid tokens');
    Check(EncodeWfcTrainingText(LDocument) = LText,
      'supplementary Unicode and embedded LF retain exact canonical bytes');
  finally
    LDocument.Free;
  end;
end;

begin
  {$IFNDEF PAS2JS}
  if ParamCount <> 1 then
    raise Exception.Create('usage: wfc_training_text_test FIXTURE-DIRECTORY');
  {$ENDIF}
  RoundTrip(ADJACENCY1D_TEXT, 'adjacency1d', wtkAdjacency1D);
  RoundTrip(ADJACENCY2D_TEXT, 'adjacency2d', wtkAdjacency2D);
  RoundTrip(PATTERN2D_TEXT, 'pattern2d', wtkPattern2D);
  RoundTrip(SEQUENCE_TEXT, 'sequence', wtkSequence);
  TestMalformed;
  TestUnicodeAndEscapes;
  WriteLn('Checks: ', GChecks, ', Failures: ', GFailures);
  if GFailures <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.Create('training text checks failed');
    {$ELSE}Halt(1);{$ENDIF}
  end;
end.
