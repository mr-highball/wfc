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
unit wfc_browser_dom;
{$mode delphi}{$H+}
interface
uses Classes, SysUtils;
const WFC_BROWSER_MAX_DOM_BYTES = 16 * 1024 * 1024;
type EWfcBrowserDom = class(Exception);
{ First real body tag from browser-serialized DOM. Caller owns the list.
  This assertion reader is not an HTML sanitizer. }
function WfcBrowserBodyAttributes(const AHtml: String): TStringList;
{ Synchronous Pascal programs only: completion means rtl.run returned or
  the test host raised its explicit successful exit. Async callbacks need
  an application-owned completion signal instead of this harness. }
function WfcBrowserHarness(const AScriptName: String): String;
implementation
procedure Fail(const M:String);
begin raise EWfcBrowserDom.Create(M);end;
function NameChar(const C:Char):Boolean;
begin Result:=C in ['a'..'z','A'..'Z','0'..'9','-','_',':'];end;
function DecodeValue(const S:String):String;
var I,J,K,D,Base,Code:Integer;E:String;
  procedure Scalar(const C:Integer);
  begin
    if (C<1) or (C>$10FFFF) or ((C>=$D800) and (C<=$DFFF)) then Fail('invalid entity scalar');
    if C<$80 then Result:=Result+Chr(C)
    else if C<$800 then Result:=Result+Chr($C0 or(C shr 6))+Chr($80 or(C and $3F))
    else if C<$10000 then Result:=Result+Chr($E0 or(C shr 12))+
      Chr($80 or((C shr 6) and $3F))+Chr($80 or(C and $3F))
    else Result:=Result+Chr($F0 or(C shr 18))+Chr($80 or((C shr 12) and $3F))+
      Chr($80 or((C shr 6) and $3F))+Chr($80 or(C and $3F));
  end;
begin
  Result:='';I:=1;
  while I<=Length(S) do
  begin
    if S[I]<>'&' then begin Result:=Result+S[I];Inc(I);Continue;end;
    J:=I+1;
    while (J<=Length(S)) and (J-I<=32) and (S[J]<>';') do Inc(J);
    if (J>Length(S)) or (S[J]<>';') then begin Result:=Result+'&';Inc(I);Continue;end;
    E:=Copy(S,I+1,J-I-1);
    if E='amp' then Result:=Result+'&'
    else if E='quot' then Result:=Result+'"'
    else if E='apos' then Result:=Result+''''
    else if E='lt' then Result:=Result+'<'
    else if E='gt' then Result:=Result+'>'
    else if (Length(E)>1) and(E[1]='#') then
    begin
      K:=2;Base:=10;Code:=0;
      if E[K] in ['x','X'] then begin Base:=16;Inc(K);end;
      if K>Length(E) then Fail('empty numeric entity');
      while K<=Length(E) do
      begin
        D:=-1;
        if E[K] in ['0'..'9'] then D:=Ord(E[K])-48
        else if E[K] in ['a'..'f'] then D:=Ord(E[K])-87
        else if E[K] in ['A'..'F'] then D:=Ord(E[K])-55;
        if (D<0) or(D>=Base) or(Code>($10FFFF-D) div Base) then Fail('invalid numeric entity');
        Code:=Code*Base+D;Inc(K);
      end;
      Scalar(Code);
    end
    else Result:=Result+Copy(S,I,J-I+1);
    I:=J+1;
  end;
end;
function WfcBrowserBodyAttributes(const AHtml:String):TStringList;
var I,J,Start,TemplateDepth:Integer;Tag,Key,Value,RawTag,LowerHtml:String;
  Quote:Char;Closing,IsBody,Closed:Boolean;A:TStringList;
begin
  if Length(AHtml)>WFC_BROWSER_MAX_DOM_BYTES then Fail('DOM exceeds assertion-reader limit');
  A:=TStringList.Create;A.CaseSensitive:=True;
  try
    LowerHtml:=LowerCase(AHtml);I:=1;RawTag:='';TemplateDepth:=0;
    while I<=Length(AHtml) do
    begin
      if RawTag<>'' then
      begin
        while I<=Length(AHtml) do
        begin
          if Copy(LowerHtml,I,Length(RawTag)+2)='</'+RawTag then
          begin
            J:=I+Length(RawTag)+2;
            if (J<=Length(AHtml)) and(AHtml[J] in [#9,#10,#13,' ','>']) then Break;
          end;
          Inc(I);
        end;
        RawTag:='';
      end;
      if I>Length(AHtml) then Break;
      if AHtml[I]<>'<' then begin Inc(I);Continue;end;
      if Copy(AHtml,I,4)='<!--' then
      begin
        Inc(I,4);
        while(I<=Length(AHtml)) and(Copy(AHtml,I,3)<>'-->') do Inc(I);
        if I>Length(AHtml) then Fail('unterminated DOM comment');
        Inc(I,3);Continue;
      end;
      Inc(I);Closing:=False;
      if(I<=Length(AHtml)) and(AHtml[I]='/') then begin Closing:=True;Inc(I);end;
      Start:=I;while(I<=Length(AHtml)) and NameChar(AHtml[I]) do Inc(I);
      Tag:=LowerCase(Copy(AHtml,Start,I-Start));
      IsBody:=(Tag='body') and not Closing and(TemplateDepth=0);Closed:=False;
      while I<=Length(AHtml) do
      begin
        while(I<=Length(AHtml)) and(AHtml[I] in [#9,#10,#13,' ','/']) do Inc(I);
        if I>Length(AHtml) then Fail('unterminated DOM tag');
        if AHtml[I]='>' then begin Inc(I);Closed:=True;Break;end;
        Start:=I;while(I<=Length(AHtml)) and NameChar(AHtml[I]) do Inc(I);
        Key:=LowerCase(Copy(AHtml,Start,I-Start));
        if Key='' then
        begin
          if AHtml[I] in ['"',''''] then
          begin
            Quote:=AHtml[I];Inc(I);
            while(I<=Length(AHtml)) and(AHtml[I]<>Quote) do Inc(I);
          end;
          Inc(I);Continue;
        end;
        while(I<=Length(AHtml)) and(AHtml[I] in [#9,#10,#13,' ']) do Inc(I);
        Value:='';
        if(I<=Length(AHtml)) and(AHtml[I]='=') then
        begin
          Inc(I);while(I<=Length(AHtml)) and(AHtml[I] in [#9,#10,#13,' ']) do Inc(I);
          if I>Length(AHtml) then Fail('missing attribute value');
          if AHtml[I] in ['"',''''] then
          begin
            Quote:=AHtml[I];Inc(I);Start:=I;
            while(I<=Length(AHtml)) and(AHtml[I]<>Quote) do Inc(I);
            if I>Length(AHtml) then Fail('unterminated attribute');
            Value:=Copy(AHtml,Start,I-Start);Inc(I);
          end
          else
          begin
            Start:=I;
            while(I<=Length(AHtml)) and not(AHtml[I] in [#9,#10,#13,' ','>']) do Inc(I);
            Value:=Copy(AHtml,Start,I-Start);
          end;
        end;
        if IsBody then
        begin
          if A.IndexOfName(Key)>=0 then Fail('duplicate body attribute');
          A.Add(Key+'='+DecodeValue(Value));
        end;
      end;
      if not Closed then Fail('unterminated DOM tag');
      if IsBody then begin Result:=A;A:=nil;Exit;end;
      if Tag='template' then
      begin
        if Closing then
        begin
          if TemplateDepth=0 then Fail('unmatched template close');
          Dec(TemplateDepth);
        end
        else Inc(TemplateDepth);
      end;
      { Browser checks run with scripting enabled: noscript serializes as
        raw text. All other HTML raw/RCDATA containers must also hide any
        apparent body markup from the assertion reader. }
      if not Closing and((Tag='script') or(Tag='style') or(Tag='textarea') or
        (Tag='title') or(Tag='noscript') or(Tag='xmp') or(Tag='iframe') or
        (Tag='noembed') or(Tag='noframes')) then RawTag:=Tag;
      if not Closing and(Tag='plaintext') then I:=Length(AHtml)+1;
    end;
    Fail('rendered DOM has no body element');Result:=nil;
  finally A.Free;end;
end;
function WfcBrowserHarness(const AScriptName:String):String;
var I:Integer;
begin
  if AScriptName='' then Fail('harness script name required');
  for I:=1 to Length(AScriptName) do
    if not(AScriptName[I] in ['a'..'z','A'..'Z','0'..'9','_','-','.']) then Fail('local script basename required');
  if(Pos('..',AScriptName)>0) or(ExtractFileExt(AScriptName)<>'.js') then Fail('local .js basename required');
  { Runtime bootstrap only: all conformance logic is compiled Pascal. }
  Result:='<!doctype html><html><head><meta charset="utf-8"><title>WFC browser checks</title></head>'+
    '<body data-self-test="loading"><pre id="failure"></pre><script src="'+AScriptName+'"></script><script>'+
    'try{rtl.showUncaughtExceptions=false;rtl.run();if(rtl.exitcode!==0)throw new Error("Pascal exit "+rtl.exitcode);'+
    'document.body.setAttribute("data-self-test","passed");}catch(e){'+
    'document.body.setAttribute("data-self-test",e&&e.$classname==="EWfcBrowserTestExit"&&e.ExitStatus===0?"passed":"failed");'+
    'document.getElementById("failure").textContent=String(e&&e.fMessage||e);}</script></body></html>'+#10;
end;
end.
