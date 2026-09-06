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
program wfc_browser_dom_test;
{$mode delphi}{$H+}
uses Classes,SysUtils,wfc_browser_dom;
var Checks:Integer;
procedure Check(const B:Boolean;const M:String);
begin Inc(Checks);if not B then raise Exception.Create(M);end;
procedure Reject(const S:String);
var A:TStringList;Raised:Boolean;
begin
  A:=nil;Raised:=False;
  try A:=WfcBrowserBodyAttributes(S);except on EWfcBrowserDom do Raised:=True;end;
  A.Free;Check(Raised,'malformed/missing body must reject');
end;
procedure TestDiagnostics;
var A,E:TStringList;S,Before,Failure:String;I:Integer;
  function AssertionFailure:String;
  begin
    Result:='';
    try WfcBrowserAssertBody(A,E);except on X:EWfcBrowserDom do Result:=X.Message;end;
  end;
begin
  A:=TStringList.Create;E:=TStringList.Create;
  try
    A.Add('class=not-a-data-marker');
    A.Add('data-voice-phase=await-release');
    A.Add('data-self-test=not-requested');
    E.Add('data-self-test=passed');
    Before:=A.Text;
    S:=WfcBrowserBodyDiagnostic(A);
    Check(Pos('not-a-data-marker',S)=0,'exclude non-data attributes from diagnostics');
    Check(Pos('"data-self-test"="not-requested"',S)>0,'preserve pending state');
    Check(Pos('"data-voice-phase"="await-release"',S)>0,'include async phase');
    Check(Pos('"data-self-test"',S)<Pos('"data-voice-phase"',S),'prioritize main state');
    Check(A.Text=Before,'diagnostic reader does not mutate body');
    Failure:=AssertionFailure;
    Check(Pos('mismatch',Failure)>0,'pending state is never a pass');
    Check(Pos('await-release',Failure)>0,'failed assertion carries phase');
    A.Values['data-self-test']:='passed';
    Check(AssertionFailure='','exact completed state passes');
    E.Add('data-stream-release=passed');
    Check(Pos('mismatch',AssertionFailure)>0,'missing additional completion fails');
    A.Values['data-stream-release']:='passed';
    Check(AssertionFailure='','all expected completion markers required');
    A.Values['data-self-test-message']:='failure'+#10+'::error::injected'+#13+#27+'[31m'+#9+'"\'+#$C3#$A9;
    S:=WfcBrowserBodyDiagnostic(A);
    for I:=1 to Length(S) do
      Check(S[I] in [#32..#126],'diagnostic output is one printable ASCII line');
    Check(Pos('\x0A::error::injected',S)>0,'escape line breaks before workflow-like content');
    Check(Pos('\x1B[31m',S)>0,'escape terminal controls');
    Check(Pos('\"\\',S)>0,'escape quote and backslash');
    Check(Pos('\xC3\xA9',S)>0,'escape non-ASCII bytes without terminal ambiguity');
    Check(Pos('browser self-test reported:',AssertionFailure)>0,'nonempty failure message still rejects');
    A.Values['data-self-test-message']:='';
    A.Values['data-self-test']:='failed';
    for I:=1 to 100 do A.Add('data-payload-'+IntToStr(I)+'='+StringOfChar('x',1000));
    A.Add('data-tail=must-not-fit');
    S:=WfcBrowserBodyDiagnostic(A);
    Check(Length(S)<=WFC_BROWSER_MAX_DIAGNOSTIC_BYTES,'bounded total snapshot');
    Check(Pos('[truncated]',S)>0,'total truncation is explicit');
    Check(Pos('..."',S)>0,'per-value truncation is explicit');
    Check(Pos('"data-self-test"="failed"',S)>0,'long payload cannot hide main state');
    Check(Pos('must-not-fit',S)=0,'bounded snapshot omits tail');
    Check(Length(AssertionFailure)<=WFC_BROWSER_MAX_DIAGNOSTIC_BYTES+512,'full mismatch is bounded');
    A.Values['data-self-test']:=StringOfChar(#10,10000);
    E.Values['data-self-test']:=StringOfChar(#27,10000);
    Failure:=AssertionFailure;
    Check(Length(Failure)<=WFC_BROWSER_MAX_DIAGNOSTIC_BYTES+512,'long expected and actual values are bounded');
    Check((Pos(#10,Failure)=0) and(Pos(#27,Failure)=0),'mismatch values cannot inject raw controls');
    E.Clear;
    Check(Pos('at least one body expectation',AssertionFailure)>0,'empty expectations cannot pass');
    S:=WfcBrowserBodyDiagnostic(nil);
    Check(Pos('unavailable',S)>0,'nil diagnostic state is explicit');
    A.Free;A:=nil;E.Add('data-self-test=passed');
    Check(Pos('body attributes are required',AssertionFailure)>0,'missing actual state cannot pass');
  finally E.Free;A.Free;end;
end;
procedure Test;
const RawTags:array[0..4]of String=('noscript','xmp','iframe','noembed','noframes');
var A:TStringList;S:String;Raised:Boolean;I:Integer;
begin
  A:=WfcBrowserBodyAttributes('<!doctype html><html><head>'+
    '<!-- <body fake="1"> --><script>var x="<body fake=2>";</script>'+
    '<style>x:before{content:"<body fake=3>"}</style><title><body fake=4></title></head>'+
    '<BODY data-self-test="passed" data-output="A &amp; B &quot;x&quot; &#39;y&#39; &lt;z&gt;"'+
    ' data-unicode="&#233;&#x1F3B5;" empty="" flag data-equal="a=b"></BODY></html>');
  try
    Check(A.Values['data-self-test']='passed','actual body found');
    Check(A.IndexOfName('fake')<0,'raw text and comment ignored');
    Check(A.Values['data-output']='A & B "x" ''y'' <z>','named/decimal entities');
    Check(A.Values['data-unicode']=#$C3#$A9#$F0#$9F#$8E#$B5,'UTF-8 entities');
    Check(A.IndexOfName('empty')>=0,'empty retained');
    Check(A.IndexOfName('flag')>=0,'boolean retained');
    Check(A.Values['data-equal']='a=b','equals retained');
  finally A.Free;end;
  A:=WfcBrowserBodyAttributes('<html data-fake="> <body fake=1>"><body data-x=ok>');
  try Check(A.Values['data-x']='ok','quoted fake ignored');finally A.Free;end;
  for I:=0 to High(RawTags) do
  begin
    A:=WfcBrowserBodyAttributes('<head><'+RawTags[I]+'><body data-self-test="passed" fake="yes">'+
      '</'+RawTags[I]+'></head><body data-self-test="failed">');
    try
      Check(A.Values['data-self-test']='failed','raw-text body cannot forge pass: '+RawTags[I]);
      Check(A.IndexOfName('fake')<0,'raw-text attributes ignored: '+RawTags[I]);
    finally A.Free;end;
  end;
  A:=WfcBrowserBodyAttributes('<head><template><body data-self-test="passed">'+
    '<template><body fake="yes"></body></template></body></template></head>'+
    '<body data-self-test="failed">');
  try
    Check(A.Values['data-self-test']='failed','nested template cannot forge pass');
    Check(A.IndexOfName('fake')<0,'template attributes ignored');
  finally A.Free;end;
  A:=WfcBrowserBodyAttributes('<template><script>var s="</template><body fake=1>";</script>'+
    '<template><body fake=2></template></template><body data-x=real>');
  try Check(A.Values['data-x']='real','raw text does not alter template depth');finally A.Free;end;
  Reject('<template><body data-self-test="passed"></body>');
  Reject('</template><body data-self-test="passed">');
  Reject('<plaintext><body data-self-test="passed">');
  Reject('<script>text <body a=1>');Reject('<!-- <body a=1>');
  Reject('<body a="unterminated>');Reject('<body a=1 A=2>');
  Reject('<body a="&#xD800;">');Reject('<body a="&#999999999999;">');
  Reject('<html></html>');Reject('<body unfinished');
  S:=WfcBrowserHarness('wfc_test.js');
  Check((Pos('rtl.run()',S)>0) and(Pos('rtl.exitcode',S)>0),'runtime exit checked');
  Raised:=False;try S:=WfcBrowserHarness('../evil.js');except on EWfcBrowserDom do Raised:=True;end;
  Check(Raised,'forbid traversal');
  Raised:=False;try S:=WfcBrowserHarness('x"><script>.js');except on EWfcBrowserDom do Raised:=True;end;
  Check(Raised,'forbid markup');
end;
begin Checks:=0;Test;TestDiagnostics;WriteLn('Checks: ',Checks,', Failures: 0');end.
