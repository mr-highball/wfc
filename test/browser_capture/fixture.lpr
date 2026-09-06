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
program fixture;
{$mode delphi}{$H+}
{$IFNDEF PAS2JS}{$FATAL this capture fixture requires pas2js}{$ENDIF}
uses SysUtils, JS, Web;

procedure Finish;
begin
  document.body.setAttribute('data-self-test', 'passed');
  document.body.setAttribute('data-fixture-release', 'passed');
  document.body.setAttribute('data-fixture-bytes', '4');
end;

procedure BlobWork; async;
var LBlob: TJSBlob; LBytes: TJSUint8Array;
begin
  try
    LBytes := TJSUint8Array.new(4);
    LBytes[0] := 1; LBytes[1] := 2; LBytes[2] := 3; LBytes[3] := 255;
    LBlob := TJSBlob.new([LBytes]);
    LBytes := TJSUint8Array.new(TJSArrayBuffer(await(LBlob.arrayBuffer)));
    if (LBytes.length <> 4) or (LBytes[3] <> 255) then raise Exception.Create('Blob bytes changed');
    window.setTimeout(@Finish, 250);
  except
    document.body.setAttribute('data-self-test', 'failed');
    document.body.setAttribute('data-self-test-message', 'Blob fixture failed');
  end;
end;

begin
  document.body.setAttribute('data-self-test', 'pending');
  document.body.setAttribute('data-fixture-release', 'pending');
  document.body.setAttribute('data-unicode', #$E9#$D83C#$DFB5);
  if window.location.search = '?mode=permanent-pending' then Exit;
  if window.location.search = '?mode=missing-release' then
  begin document.body.setAttribute('data-self-test', 'passed'); Exit;end;
  if window.location.search = '?mode=failed' then
  begin
    document.body.setAttribute('data-self-test', 'failed');
    document.body.setAttribute('data-self-test-message', 'intentional fixture failure');
    Exit;
  end;
  if window.location.search = '?mode=negative' then
    document.body.setAttribute('data-voice-state', 'failed');
  BlobWork;
end.
