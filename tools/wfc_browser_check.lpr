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
program wfc_browser_check;
{$mode delphi}{$H+}
uses Classes,SysUtils,wfc_browser_dom;
function ReadDom(const P:String):String;
var S:TFileStream;
begin
  S:=TFileStream.Create(P,fmOpenRead or fmShareDenyNone);
  try
    if S.Size>WFC_BROWSER_MAX_DOM_BYTES then raise Exception.Create('DOM file exceeds limit');
    SetLength(Result,Integer(S.Size));
    if Length(Result)>0 then S.ReadBuffer(Result[1],Length(Result));
  finally S.Free;end;
end;
procedure Run;
var I,J,P:Integer;Path,Harness,Value,Key,Text:String;Expected,Actual:TStringList;S:TFileStream;
begin
  if(ParamCount=1) and(ParamStr(1)='--version') then
  begin WriteLn('wfc_browser_check 1');Exit;end;
  if(ParamCount=1) and(ParamStr(1)='--help') then
  begin
    WriteLn('wfc_browser_check --dom FILE --expect NAME=VALUE [--expect ...]');
    WriteLn('wfc_browser_check --harness SCRIPT.js --dom NEW-HTML-FILE');Exit;
  end;
  Expected:=TStringList.Create;Expected.CaseSensitive:=True;Actual:=nil;
  try
    Path:='';Harness:='';I:=1;
    while I<=ParamCount do
    begin
      Key:=ParamStr(I);Inc(I);if I>ParamCount then raise Exception.Create('missing value for '+Key);
      Value:=ParamStr(I);
      if Key='--dom' then begin if Path<>'' then raise Exception.Create('duplicate --dom');Path:=Value;end
      else if Key='--harness' then begin if Harness<>'' then raise Exception.Create('duplicate --harness');Harness:=Value;end
      else if Key='--expect' then
      begin
        P:=Pos('=',Value);if P<2 then raise Exception.Create('--expect requires NAME=VALUE');
        Key:=Copy(Value,1,P-1);
        for J:=1 to Length(Key) do if not(Key[J] in ['a'..'z','0'..'9','-','_',':']) then
          raise Exception.Create('attribute names must be lowercase ASCII');
        if Expected.IndexOfName(Key)>=0 then raise Exception.Create('duplicate expectation');
        Expected.Add(Value);
      end else raise Exception.Create('unknown option '+Key);
      Inc(I);
    end;
    if Path='' then raise Exception.Create('--dom is required');
    if Harness<>'' then
    begin
      if Expected.Count<>0 then raise Exception.Create('harness and assertions are exclusive');
      if FileExists(Path) or DirectoryExists(Path) then raise Exception.Create('refusing to overwrite harness');
      Text:=WfcBrowserHarness(Harness);S:=TFileStream.Create(Path,fmCreate);
      try S.WriteBuffer(Text[1],Length(Text));finally S.Free;end;Exit;
    end;
    if Expected.Count=0 then raise Exception.Create('at least one --expect is required');
    Actual:=WfcBrowserBodyAttributes(ReadDom(Path));
    WfcBrowserAssertBody(Actual,Expected);
    WriteLn('Browser assertions passed: ',Expected.Count);
  finally Actual.Free;Expected.Free;end;
end;
begin
  try Run;except on E:Exception do begin WriteLn(StdErr,'wfc_browser_check: ',E.Message);Halt(1);end;end;
end.
