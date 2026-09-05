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
begin Checks:=0;Test;WriteLn('Checks: ',Checks,', Failures: 0');end.
