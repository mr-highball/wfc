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
unit wfc_serve_http;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

const
  WFC_SERVE_VERSION = 2;
  WFC_SERVE_MAX_HEADER_BYTES = 16384;
  WFC_SERVE_MAX_TARGET_BYTES = 2048;
  WFC_SERVE_HEADER_TIMEOUT_MS = 2000;
  WFC_SERVE_SEND_TIMEOUT_MS = 15000;

type
  EWfcServe = class(Exception);
  TWfcServeRequest = record
    HeadOnly: Boolean;
    Target: String;
    RelativePath: String;
  end;

{ Pure helpers: printable ASCII targets are decoded once. Dot/hidden
  components, device aliases, backslash, colon, residual percent signs
  and trailing dot/space aliases are rejected on every supported OS. }
function DecodeWfcServeTarget(const ATarget: String;
  out ARelativePath: String): Boolean;
{ Explicit development bind addresses only: canonical dotted-decimal IPv4
  in loopback or RFC 1918 private ranges. No DNS or wildcard binding. }
function ValidWfcServeBindAddress(const AAddress: String): Boolean;
function ParseWfcServeRequest(const AHeaders: String;
  out ARequest: TWfcServeRequest;
  const ABindAddress: String = '127.0.0.1'): Integer;
function WfcServeContentType(const AFileName: String): String;
function WfcServeReason(const AStatus: Integer): String;
function WfcServeResponseHeader(const AStatus: Integer;
  const AContentLength: Int64; const AContentType: String;
  const ALocation: String = ''): String;
function ValidateWfcServeRoot(const ARoot: String): String;

{ Sequential development server, loopback by default; zero max requests runs
  until stopped. An explicit private bind address enables trusted LAN access.
  No authentication, uploads, directory listing, script execution or TLS. Every
  connection closes after one GET/HEAD. Link/reparse component checks plus
  final opened-handle containment prevent serving outside the selected root.
  Linux requires /proc/self/fd; macOS uses fcntl(F_GETPATH). }
procedure RunWfcServe(const ARoot: String; const APort: Integer;
  const AMaxRequests: Integer = 0;
  const ABindAddress: String = '127.0.0.1');

implementation

uses
  Sockets
  {$IFDEF MSWINDOWS}, Windows{$ELSE}, BaseUnix{$ENDIF};

{$IFNDEF MSWINDOWS}
  {$IFNDEF LINUX}
    {$IFNDEF DARWIN}
      {$FATAL wfc_serve currently supports Windows, Linux and macOS}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}
const
  { Darwin flags missing from the stable FPC 3.2.2 BSD open-flag set.
    ABI facts, not borrowed implementation:
    https://github.com/apple-oss-distributions/xnu/blob/main/bsd/sys/fcntl.h
    https://github.com/apple-oss-distributions/xnu/blob/main/bsd/sys/syslimits.h }
  WFC_SERVE_DARWIN_O_NOFOLLOW = $00000100;
  WFC_SERVE_DARWIN_O_DIRECTORY = $00100000;
  WFC_SERVE_DARWIN_MAX_PATH = 1024;

{ fcntl's third argument is variadic, including on Apple Silicon. Keep the
  pointer in the varargs list rather than narrowing it to FpFcntl's cint. }
function ServeDarwinFcntl(AHandle, ACommand: LongInt): LongInt;
  cdecl; varargs; external 'c' name 'fcntl';
{$ENDIF}

{$IFDEF MSWINDOWS}
const
  { Windows file-open flag, absent from the stable 3.2.2 Windows headers. }
  WFC_SERVE_OPEN_REPARSE_POINT = DWORD($00200000);

function ServeFinalPathName(AHandle: THandle; APath: PChar;
  ALength, AFlags: DWORD): DWORD; stdcall;
  external 'kernel32' name 'GetFinalPathNameByHandleA';
{$ENDIF}

function HexDigit(const AChar: Char): Integer;
begin
  case AChar of
    '0'..'9': Result := Ord(AChar) - Ord('0');
    'a'..'f': Result := Ord(AChar) - Ord('a') + 10;
    'A'..'F': Result := Ord(AChar) - Ord('A') + 10;
  else
    Result := -1;
  end;
end;

function SafeComponent(const AValue: String): Boolean;
var
  LBase: String;
  I: Integer;
begin
  Result := False;
  if (AValue = '') or (AValue[1] = '.') or
      (AValue[Length(AValue)] in ['.', ' ']) then
    Exit;
  LBase := UpperCase(AValue);
  I := Pos('.', LBase);
  if I > 0 then
    LBase := Copy(LBase, 1, I - 1);
  if (LBase = 'CON') or (LBase = 'PRN') or (LBase = 'AUX') or
      (LBase = 'NUL') or (LBase = 'CLOCK$') then
    Exit;
  if (Length(LBase) = 4) and
      ((Copy(LBase, 1, 3) = 'COM') or (Copy(LBase, 1, 3) = 'LPT')) and
      (LBase[4] in ['0'..'9']) then
    Exit;
  Result := True;
end;

function DecodeWfcServeTarget(const ATarget: String;
  out ARelativePath: String): Boolean;
var
  I, LEnd, LHigh, LLow, LStart: Integer;
  LPath: String;
  C: Char;
begin
  Result := False;
  ARelativePath := '';
  if (ATarget = '') or (Length(ATarget) > WFC_SERVE_MAX_TARGET_BYTES) or
      (ATarget[1] <> '/') or (Pos('#', ATarget) <> 0) then
    Exit;
  for I := 1 to Length(ATarget) do
    if (Ord(ATarget[I]) <= 32) or (Ord(ATarget[I]) > 126) then
      Exit;
  LEnd := Pos('?', ATarget);
  if LEnd = 0 then
    LEnd := Length(ATarget) + 1;
  LPath := '';
  I := 2;
  while I < LEnd do
  begin
    C := ATarget[I];
    if C = '%' then
    begin
      if I + 2 >= LEnd then
        Exit;
      LHigh := HexDigit(ATarget[I + 1]);
      LLow := HexDigit(ATarget[I + 2]);
      if (LHigh < 0) or (LLow < 0) then
        Exit;
      C := Chr(LHigh * 16 + LLow);
      Inc(I, 2);
    end;
    if (Ord(C) < 32) or (Ord(C) > 126) or
        (C in ['\', ':', '%', '?', '#', '*', '"', '<', '>', '|']) then
      Exit;
    LPath := LPath + C;
    Inc(I);
  end;
  LStart := 1;
  for I := 1 to Length(LPath) do
    if LPath[I] = '/' then
    begin
      if not SafeComponent(Copy(LPath, LStart, I - LStart)) then
        Exit;
      LStart := I + 1;
    end;
  if (LStart <= Length(LPath)) and
      not SafeComponent(Copy(LPath, LStart, MaxInt)) then
    Exit;
  ARelativePath := LPath;
  Result := True;
end;

function HeaderNameValid(const AName: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AName = '' then
    Exit;
  for I := 1 to Length(AName) do
    if not (AName[I] in ['a'..'z', 'A'..'Z', '0'..'9', '!',
      '#', '$', '%', '&', '''', '*', '+', '-', '.', '^', '_', #96, '|', '~']) then
      Exit;
  Result := True;
end;

function ValidWfcServeBindAddress(const AAddress: String): Boolean;
var
  LOctets: array[0..3] of Integer;
  I, J, LStart: Integer;
begin
  Result := False;
  if (Length(AAddress) < 7) or (Length(AAddress) > 15) then
    Exit;
  I := 1;
  for J := 0 to 3 do
  begin
    LStart := I;
    LOctets[J] := 0;
    while (I <= Length(AAddress)) and (AAddress[I] in ['0'..'9']) do
    begin
      if I - LStart >= 3 then
        Exit;
      LOctets[J] := LOctets[J] * 10 + Ord(AAddress[I]) - Ord('0');
      Inc(I);
    end;
    if (I = LStart) or (LOctets[J] > 255) then
      Exit;
    if (I - LStart > 1) and (AAddress[LStart] = '0') then
      Exit;
    if J < 3 then
    begin
      if (I > Length(AAddress)) or (AAddress[I] <> '.') then
        Exit;
      Inc(I);
    end;
  end;
  if I <= Length(AAddress) then
    Exit;
  Result := (LOctets[0] = 127) or (LOctets[0] = 10) or
    ((LOctets[0] = 172) and (LOctets[1] >= 16) and (LOctets[1] <= 31)) or
    ((LOctets[0] = 192) and (LOctets[1] = 168));
end;

function AllowedHost(const AValue, ABindAddress: String): Boolean;
var
  LHost, LPort: String;
  I, LNumber: Integer;
begin
  LHost := LowerCase(AValue);
  I := Pos(':', LHost);
  if I > 0 then
  begin
    LPort := Copy(LHost, I + 1, MaxInt);
    LHost := Copy(LHost, 1, I - 1);
    if LPort = '' then
      Exit(False);
    for I := 1 to Length(LPort) do
      if not (LPort[I] in ['0'..'9']) then
        Exit(False);
    if not TryStrToInt(LPort, LNumber) or
        (LNumber < 1) or (LNumber > 65535) then
      Exit(False);
  end;
  Result := (LHost = ABindAddress) or
    ((ABindAddress = '127.0.0.1') and (LHost = 'localhost'));
end;

function ParseWfcServeRequest(const AHeaders: String;
  out ARequest: TWfcServeRequest; const ABindAddress: String): Integer;
var
  I, P, Q, LStart: Integer;
  LLine, LMethod, LVersion, LName, LValue: String;
  LHostSeen, LLengthSeen: Boolean;
begin
  ARequest.HeadOnly := False;
  ARequest.Target := '';
  ARequest.RelativePath := '';
  if not ValidWfcServeBindAddress(ABindAddress) then
    Exit(400);
  if Length(AHeaders) > WFC_SERVE_MAX_HEADER_BYTES then
    Exit(431);
  Result := 400;
  P := Pos(#13#10, AHeaders);
  if P < 1 then
    Exit;
  LLine := Copy(AHeaders, 1, P - 1);
  Q := Pos(' ', LLine);
  if Q < 1 then
    Exit;
  LMethod := Copy(LLine, 1, Q - 1);
  ARequest.HeadOnly := LMethod = 'HEAD';
  Delete(LLine, 1, Q);
  Q := Pos(' ', LLine);
  if Q < 1 then
    Exit;
  ARequest.Target := Copy(LLine, 1, Q - 1);
  LVersion := Copy(LLine, Q + 1, MaxInt);
  if (LVersion <> 'HTTP/1.0') and (LVersion <> 'HTTP/1.1') then
    Exit;
  if not HeaderNameValid(LMethod) then
    Exit;
  if (LMethod <> 'GET') and (LMethod <> 'HEAD') then
    Exit(405);
  if not DecodeWfcServeTarget(ARequest.Target, ARequest.RelativePath) then
    Exit;
  LStart := P + 2;
  LHostSeen := False;
  LLengthSeen := False;
  while LStart <= Length(AHeaders) do
  begin
    P := LStart;
    while (P < Length(AHeaders)) and
        not ((AHeaders[P] = #13) and (AHeaders[P + 1] = #10)) do
      Inc(P);
    if P >= Length(AHeaders) then
      Exit;
    LLine := Copy(AHeaders, LStart, P - LStart);
    LStart := P + 2;
    if LLine = '' then
    begin
      if LStart <= Length(AHeaders) then
        Exit;
      if (LVersion = 'HTTP/1.1') and not LHostSeen then
        Exit;
      Exit(200);
    end;
    for I := 1 to Length(LLine) do
      if ((Ord(LLine[I]) < 32) and (LLine[I] <> #9)) or
          (Ord(LLine[I]) = 127) then
        Exit;
    Q := Pos(':', LLine);
    if Q < 2 then
      Exit;
    LName := Copy(LLine, 1, Q - 1);
    if not HeaderNameValid(LName) then
      Exit;
    LName := LowerCase(LName);
    LValue := Trim(Copy(LLine, Q + 1, MaxInt));
    for I := 1 to Length(LValue) do
      if ((Ord(LValue[I]) < 32) and (LValue[I] <> #9)) or
          (Ord(LValue[I]) = 127) then
        Exit;
    if LName = 'host' then
    begin
      if LHostSeen or not AllowedHost(LValue, ABindAddress) then
        Exit;
      LHostSeen := True;
    end
    else if LName = 'content-length' then
    begin
      if LLengthSeen or (LValue <> '0') then
        Exit;
      LLengthSeen := True;
    end
    else if (LName = 'transfer-encoding') or (LName = 'expect') then
      Exit;
  end;
end;

function WfcServeContentType(const AFileName: String): String;
var
  LExtension: String;
begin
  LExtension := LowerCase(ExtractFileExt(AFileName));
  if (LExtension = '.html') or (LExtension = '.htm') then
    Result := 'text/html; charset=utf-8'
  else if LExtension = '.css' then Result := 'text/css; charset=utf-8'
  else if LExtension = '.js' then Result := 'text/javascript; charset=utf-8'
  else if LExtension = '.json' then Result := 'application/json'
  else if LExtension = '.svg' then Result := 'image/svg+xml'
  else if (LExtension = '.mid') or (LExtension = '.midi') then Result := 'audio/midi'
  else if LExtension = '.wav' then Result := 'audio/wav'
  else if LExtension = '.ico' then Result := 'image/x-icon'
  else if LExtension = '.txt' then Result := 'text/plain; charset=utf-8'
  else if LExtension = '.wasm' then Result := 'application/wasm'
  else if LExtension = '.png' then Result := 'image/png'
  else if (LExtension = '.jpg') or (LExtension = '.jpeg') then Result := 'image/jpeg'
  else if LExtension = '.gif' then Result := 'image/gif'
  else if LExtension = '.webp' then Result := 'image/webp'
  else Result := 'application/octet-stream';
end;

function WfcServeReason(const AStatus: Integer): String;
begin
  case AStatus of
    200: Result := 'OK';
    301: Result := 'Moved Permanently';
    400: Result := 'Bad Request';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    408: Result := 'Request Timeout';
    431: Result := 'Request Header Fields Too Large';
  else
    Result := 'Internal Server Error';
  end;
end;

function WfcServeResponseHeader(const AStatus: Integer;
  const AContentLength: Int64; const AContentType: String;
  const ALocation: String): String;
begin
  if (AContentLength < 0) or (Pos(#13, AContentType + ALocation) > 0) or
      (Pos(#10, AContentType + ALocation) > 0) then
    raise EWfcServe.Create('invalid response metadata');
  Result := 'HTTP/1.1 ' + IntToStr(AStatus) + ' ' +
    WfcServeReason(AStatus) + #13#10 +
    'Content-Length: ' + IntToStr(AContentLength) + #13#10 +
    'Content-Type: ' + AContentType + #13#10 +
    'Connection: close' + #13#10 +
    'Cache-Control: no-store' + #13#10 +
    'X-Content-Type-Options: nosniff' + #13#10;
  if AStatus = 405 then
    Result := Result + 'Allow: GET, HEAD' + #13#10;
  if ALocation <> '' then
    Result := Result + 'Location: ' + ALocation + #13#10;
  Result := Result + #13#10;
end;

function PathKind(const APath: String): Integer;
{$IFDEF MSWINDOWS}
var
  LAttributes: DWORD;
begin
  LAttributes := GetFileAttributes(PChar(APath));
  if LAttributes = INVALID_FILE_ATTRIBUTES then
    Exit(0);
  if (LAttributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0 then
    Exit(-1);
  if (LAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
    Result := 2
  else
    Result := 1;
end;
{$ELSE}
var
  LStat: Stat;
begin
  if fpLStat(PChar(APath), LStat) <> 0 then
    Exit(0);
  if fpS_ISLNK(LStat.st_mode) then
    Exit(-1);
  if fpS_ISDIR(LStat.st_mode) then
    Result := 2
  else if fpS_ISREG(LStat.st_mode) then
    Result := 1
  else
    Result := -1;
end;
{$ENDIF}

function ComponentsOrdinary(const APath: String): Boolean;
var
  I, LFirst, LKind: Integer;
begin
  Result := False;
  {$IFDEF MSWINDOWS}
  if (Length(APath) < 3) or (APath[2] <> ':') or
      (APath[3] <> DirectorySeparator) then
    Exit;
  LFirst := 4;
  {$ELSE}
  if (APath = '') or (APath[1] <> '/') then
    Exit;
  LFirst := 2;
  {$ENDIF}
  for I := LFirst to Length(APath) do
    if APath[I] = DirectorySeparator then
    begin
      LKind := PathKind(Copy(APath, 1, I - 1));
      if LKind <> 2 then
        Exit;
    end;
  LKind := PathKind(ExcludeTrailingPathDelimiter(APath));
  Result := (LKind = 1) or (LKind = 2);
end;

function ValidateWfcServeRoot(const ARoot: String): String;
begin
  if ARoot = '' then
    raise EWfcServe.Create('--root must name an explicit existing directory');
  Result := IncludeTrailingPathDelimiter(ExpandFileName(ARoot));
  if not ComponentsOrdinary(Result) or
      (PathKind(ExcludeTrailingPathDelimiter(Result)) <> 2) then
    raise EWfcServe.Create('root must be an ordinary directory without link/reparse components');
end;

function OpenServeHandle(const APath: String; const ADirectory: Boolean): THandle;
{$IFDEF MSWINDOWS}
var
  LFlags: DWORD;
begin
  LFlags := WFC_SERVE_OPEN_REPARSE_POINT;
  if ADirectory then
    LFlags := LFlags or FILE_FLAG_BACKUP_SEMANTICS;
  Result := CreateFile(PChar(APath), GENERIC_READ, FILE_SHARE_READ or
    FILE_SHARE_WRITE, nil, OPEN_EXISTING, LFlags, 0);
end;
{$ELSE}
var
  LFlags: Integer;
begin
  LFlags := O_RDONLY or O_NONBLOCK or
    {$IFDEF DARWIN}WFC_SERVE_DARWIN_O_NOFOLLOW{$ELSE}O_NOFOLLOW{$ENDIF};
  if ADirectory then
    LFlags := LFlags or
      {$IFDEF DARWIN}WFC_SERVE_DARWIN_O_DIRECTORY{$ELSE}O_DIRECTORY{$ENDIF};
  Result := fpOpen(PChar(APath), LFlags);
end;
{$ENDIF}

function FinalHandlePath(const AHandle: THandle): String;
{$IFDEF MSWINDOWS}
var
  LLength: DWORD;
begin
  SetLength(Result, 32768);
  LLength := ServeFinalPathName(AHandle, PChar(Result), Length(Result), 0);
  if (LLength = 0) or (LLength >= DWORD(Length(Result))) then
    raise EWfcServe.Create('cannot resolve opened file path');
  SetLength(Result, LLength);
  if Copy(Result, 1, 4) = '\\?\' then
    Delete(Result, 1, 4);
end;
{$ELSE}
{$IFDEF DARWIN}
var
  LBuffer: array[0..WFC_SERVE_DARWIN_MAX_PATH - 1] of Char;
  LLength: Integer;
begin
  { Apple documents a MAXPATHLEN-sized buffer. Never trust an unbounded
    NUL scan, and fail closed if an opened handle cannot be resolved. }
  FillChar(LBuffer, SizeOf(LBuffer), 0);
  if ServeDarwinFcntl(AHandle, F_GETPATH, Pointer(@LBuffer[0])) <> 0 then
    raise EWfcServe.Create('cannot resolve opened file path with F_GETPATH');
  LLength := 0;
  while (LLength < Length(LBuffer)) and (LBuffer[LLength] <> #0) do
    Inc(LLength);
  if (LLength = 0) or (LLength = Length(LBuffer)) or
      (LBuffer[0] <> '/') then
    raise EWfcServe.Create('opened file path is missing, unterminated or not absolute');
  SetString(Result, PChar(@LBuffer[0]), LLength);
end;
{$ELSE}
begin
  Result := fpReadLink('/proc/self/fd/' + IntToStr(AHandle));
  if (Result = '') or (Result[1] <> '/') then
    raise EWfcServe.Create('cannot resolve opened file path; Linux procfs is required');
end;
{$ENDIF}
{$ENDIF}

function OrdinaryHandle(const AHandle: THandle): Boolean;
{$IFDEF MSWINDOWS}
var
  LInfo: TByHandleFileInformation;
begin
  Result := GetFileInformationByHandle(AHandle, LInfo) and
    ((LInfo.dwFileAttributes and
      (FILE_ATTRIBUTE_REPARSE_POINT or FILE_ATTRIBUTE_DIRECTORY)) = 0) and
    (GetFileType(AHandle) = FILE_TYPE_DISK);
end;
{$ELSE}
var
  LStat: Stat;
begin
  Result := (fpFStat(AHandle, LStat) = 0) and fpS_ISREG(LStat.st_mode);
end;
{$ENDIF}

function PathContained(const ARoot, APath: String): Boolean;
var
  LPrefix: String;
begin
  LPrefix := Copy(APath, 1, Length(ARoot));
  {$IFDEF MSWINDOWS}
  Result := CompareText(LPrefix, ARoot) = 0;
  {$ELSE}
  Result := LPrefix = ARoot;
  {$ENDIF}
end;

function EncodeLocation(const APath: String): String;
const
  HEX = '0123456789ABCDEF';
var
  I: Integer;
  C: Char;
begin
  Result := '/';
  for I := 1 to Length(APath) do
  begin
    C := APath[I];
    if C in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.', '~', '/'] then
      Result := Result + C
    else
      Result := Result + '%' + HEX[Ord(C) div 16 + 1] +
        HEX[Ord(C) mod 16 + 1];
  end;
  if Result[Length(Result)] <> '/' then
    Result := Result + '/';
end;

procedure SetClientTimeouts(const ASocket: Integer);
{$IFDEF MSWINDOWS}
var
  LTimeout: DWORD;
begin
  LTimeout := 1000;
{$ELSE}
var
  LTimeout: TTimeVal;
  {$IFDEF DARWIN}LNoSigPipe: LongInt;{$ENDIF}
begin
  LTimeout.tv_sec := 1;
  LTimeout.tv_usec := 0;
{$ENDIF}
  {$IFDEF DARWIN}
  { Socket-local protection, never a process-wide SIGPIPE disposition.
    https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/setsockopt.2.html }
  LNoSigPipe := 1;
  if fpSetSockOpt(ASocket, SOL_SOCKET, SO_NOSIGPIPE,
      @LNoSigPipe, SizeOf(LNoSigPipe)) <> 0 then
    raise EWfcServe.Create('cannot disable SIGPIPE for client socket');
  {$ENDIF}
  if (fpSetSockOpt(ASocket, SOL_SOCKET, SO_RCVTIMEO,
      @LTimeout, SizeOf(LTimeout)) <> 0) or
      (fpSetSockOpt(ASocket, SOL_SOCKET, SO_SNDTIMEO,
      @LTimeout, SizeOf(LTimeout)) <> 0) then
    raise EWfcServe.Create('cannot set bounded client socket timeouts');
end;

function ReadHeaders(const ASocket: Integer; out AHeaders: String): Integer;
var
  LBuffer: array[0..1023] of Char;
  LReceived, LEnd: Integer;
  {$IFDEF DARWIN}LError: Integer;{$ENDIF}
  LStart: QWord;
  LPart: String;
begin
  AHeaders := '';
  LStart := GetTickCount64;
  repeat
    if GetTickCount64 - LStart >= WFC_SERVE_HEADER_TIMEOUT_MS then
      Exit(408);
    LReceived := fpRecv(ASocket, @LBuffer[0], SizeOf(LBuffer),
      {$IFDEF DARWIN}MSG_DONTWAIT{$ELSE}0{$ENDIF});
    {$IFDEF DARWIN}
    if LReceived < 0 then
    begin
      LError := SocketError;
      if (LError = EsockEWOULDBLOCK) or (LError = EsockEINTR) then
      begin
        Sleep(1);
        Continue;
      end;
    end;
    {$ENDIF}
    if LReceived < 0 then
      Exit(408);
    if LReceived = 0 then
      Exit(400);
    if LReceived > WFC_SERVE_MAX_HEADER_BYTES - Length(AHeaders) then
      Exit(431);
    SetString(LPart, PChar(@LBuffer[0]), LReceived);
    AHeaders := AHeaders + LPart;
    LEnd := Pos(#13#10#13#10, AHeaders);
    if LEnd > 0 then
    begin
      SetLength(AHeaders, LEnd + 3);
      Exit(200);
    end;
  until False;
end;

function SendBytes(const ASocket: Integer; const ABuffer;
  const ACount: Integer; const AStart: QWord): Boolean;
var
  LSent, LOffset: Integer;
  {$IFDEF DARWIN}LError: Integer;{$ENDIF}
  LPointer: PByte;
begin
  Result := False;
  LOffset := 0;
  LPointer := @ABuffer;
  while LOffset < ACount do
  begin
    if GetTickCount64 - AStart >= WFC_SERVE_SEND_TIMEOUT_MS then
      Exit;
    LSent := fpSend(ASocket, LPointer + LOffset, ACount - LOffset,
      {$IFDEF LINUX}MSG_NOSIGNAL{$ELSE}
      {$IFDEF DARWIN}MSG_DONTWAIT{$ELSE}0{$ENDIF}{$ENDIF});
    {$IFDEF DARWIN}
    if LSent < 0 then
    begin
      LError := SocketError;
      if (LError = EsockEWOULDBLOCK) or (LError = EsockEINTR) then
      begin
        Sleep(1);
        Continue;
      end;
    end;
    {$ENDIF}
    if LSent <= 0 then
      Exit;
    Inc(LOffset, LSent);
  end;
  Result := True;
end;

procedure HandleClient(const ASocket: Integer;
  const ARoot, AFinalRoot, ABindAddress: String);
var
  LHeaders, LBody, LPath, LLocation: String;
  LRequest: TWfcServeRequest;
  LStatus, LRead, LWant, LQuery: Integer;
  LHandle: THandle;
  LSize, LRemaining: Int64;
  LBuffer: array[0..65535] of Byte;
  LStart: QWord;
begin
  SetClientTimeouts(ASocket);
  LRequest.HeadOnly := False;
  LHandle := THandle(-1);
  LLocation := '';
  LStatus := ReadHeaders(ASocket, LHeaders);
  if LStatus = 200 then
    LStatus := ParseWfcServeRequest(LHeaders, LRequest, ABindAddress);
  try
    if LStatus = 200 then
    begin
      LPath := ARoot + StringReplace(LRequest.RelativePath, '/',
        DirectorySeparator, [rfReplaceAll]);
      if not ComponentsOrdinary(LPath) then
        LStatus := 404
      else if PathKind(ExcludeTrailingPathDelimiter(LPath)) = 2 then
      begin
        if (LRequest.RelativePath <> '') and
            (LRequest.RelativePath[Length(LRequest.RelativePath)] <> '/') then
        begin
          LStatus := 301;
          LLocation := EncodeLocation(LRequest.RelativePath);
          LQuery := Pos('?', LRequest.Target);
          if LQuery > 0 then
            LLocation := LLocation + Copy(LRequest.Target, LQuery, MaxInt);
        end
        else
          LPath := IncludeTrailingPathDelimiter(LPath) + 'index.html';
      end;
      if LStatus = 200 then
      begin
        if not ComponentsOrdinary(LPath) or (PathKind(LPath) <> 1) then
          LStatus := 404
        else
        begin
          LHandle := OpenServeHandle(LPath, False);
          if LHandle = THandle(-1) then
            LStatus := 404
          else if not OrdinaryHandle(LHandle) or
              not PathContained(AFinalRoot, FinalHandlePath(LHandle)) then
            LStatus := 403;
        end;
      end;
    end;
    LStart := GetTickCount64;
    if LStatus <> 200 then
    begin
      LBody := WfcServeReason(LStatus) + #10;
      LHeaders := WfcServeResponseHeader(LStatus, Length(LBody),
        'text/plain; charset=utf-8', LLocation);
      if SendBytes(ASocket, LHeaders[1], Length(LHeaders), LStart) and
          not LRequest.HeadOnly then
        SendBytes(ASocket, LBody[1], Length(LBody), LStart);
      Exit;
    end;
    LSize := FileSeek(LHandle, Int64(0), 2);
    if (LSize < 0) or (FileSeek(LHandle, Int64(0), 0) <> 0) then
      raise EWfcServe.Create('cannot seek opened regular file');
    LHeaders := WfcServeResponseHeader(200, LSize, WfcServeContentType(LPath));
    if not SendBytes(ASocket, LHeaders[1], Length(LHeaders), LStart) or
        LRequest.HeadOnly then
      Exit;
    LRemaining := LSize;
    while LRemaining > 0 do
    begin
      LWant := SizeOf(LBuffer);
      if LRemaining < LWant then
        LWant := Integer(LRemaining);
      LRead := FileRead(LHandle, LBuffer[0], LWant);
      if LRead <= 0 then
        Exit;
      if not SendBytes(ASocket, LBuffer[0], LRead, LStart) then
        Exit;
      Dec(LRemaining, LRead);
    end;
  finally
    if LHandle <> THandle(-1) then
      FileClose(LHandle);
  end;
end;

procedure RunWfcServe(const ARoot: String; const APort: Integer;
  const AMaxRequests: Integer; const ABindAddress: String);
var
  LRoot, LFinalRoot: String;
  LRootHandle: THandle;
  LListener, LClient, LCount: Integer;
  LAddress: TInetSockAddr;
  {$IFNDEF MSWINDOWS}LReuseAddress: LongInt; LOptionResult: Integer;{$ENDIF}
begin
  if not ValidWfcServeBindAddress(ABindAddress) then
    raise EWfcServe.Create('bind address must be a canonical loopback or private IPv4 address');
  if (APort < 1) or (APort > 65535) then
    raise EWfcServe.Create('port must be from 1 through 65535');
  if AMaxRequests < 0 then
    raise EWfcServe.Create('maximum request count cannot be negative');
  LRoot := ValidateWfcServeRoot(ARoot);
  LRootHandle := OpenServeHandle(ExcludeTrailingPathDelimiter(LRoot), True);
  if LRootHandle = THandle(-1) then
    raise EWfcServe.Create('cannot open root directory');
  LListener := -1;
  try
    LFinalRoot := IncludeTrailingPathDelimiter(FinalHandlePath(LRootHandle));
    if (Length(LRoot) <> Length(LFinalRoot)) or
        not PathContained(LRoot, LFinalRoot) then
      raise EWfcServe.Create('root changed or resolves through an alternate path');
    LListener := fpSocket(AF_INET, SOCK_STREAM, 0);
    if LListener < 0 then
      raise EWfcServe.Create('cannot create IPv4 socket');
    {$IFNDEF MSWINDOWS}
    { Restart after our actively closed HTTP connections enter TIME_WAIT.
      This is socket-local SO_REUSEADDR, never SO_REUSEPORT: an existing
      listener on the exact address must still refuse a new bind.
      https://man7.org/linux/man-pages/man7/socket.7.html
      https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/setsockopt.2.html }
    LReuseAddress := 1;
    repeat
      LOptionResult := fpSetSockOpt(LListener, SOL_SOCKET, SO_REUSEADDR,
        @LReuseAddress, SizeOf(LReuseAddress));
    until (LOptionResult = 0) or (SocketError <> ESysEINTR);
    if LOptionResult <> 0 then
      raise EWfcServe.Create('cannot configure listener address reuse');
    {$ENDIF}
    FillChar(LAddress, SizeOf(LAddress), 0);
    {$IFDEF DARWIN}LAddress.sin_len := SizeOf(LAddress);{$ENDIF}
    LAddress.sin_family := AF_INET;
    LAddress.sin_port := htons(Word(APort));
    LAddress.sin_addr := StrToNetAddr(ABindAddress);
    if fpBind(LListener, @LAddress, SizeOf(LAddress)) <> 0 then
      raise EWfcServe.Create('cannot bind ' + ABindAddress + ':' + IntToStr(APort) +
        ' (socket error ' + IntToStr(SocketError) + ')');
    if fpListen(LListener, 16) <> 0 then
      raise EWfcServe.Create('cannot listen on IPv4 socket');
    WriteLn('WFC static server: http://', ABindAddress, ':', APort, '/');
    WriteLn('Root: ', LRoot);
    if Copy(ABindAddress, 1, 4) <> '127.' then
      WriteLn('Trusted LAN only: no authentication or TLS. All files in this root are shared.');
    Flush(Output);
    LCount := 0;
    while (AMaxRequests = 0) or (LCount < AMaxRequests) do
    begin
      LClient := fpAccept(LListener, nil, nil);
      if LClient < 0 then
        raise EWfcServe.Create('listener accept failed');
      try
        try
          HandleClient(LClient, LRoot, LFinalRoot, ABindAddress);
        except
          on E: Exception do
            WriteLn(StdErr, 'Request closed: ', E.Message);
        end;
      finally
        CloseSocket(LClient);
      end;
      if AMaxRequests > 0 then
        Inc(LCount);
    end;
  finally
    if LListener >= 0 then
      CloseSocket(LListener);
    FileClose(LRootHandle);
  end;
end;

end.
