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
unit wfc_browser_stream_file;

{$mode delphi}{$H+}
{$IFNDEF PAS2JS}{$FATAL wfc_browser_stream_file is a browser-only helper}{$ENDIF}
{$modeswitch externalclass}

interface

uses
  JS,
  Web,
  SysUtils;

const
  WFC_BROWSER_STREAM_FILE_VERSION = 1;
  WFC_BROWSER_STREAM_FILE_MAX_SAFE_INTEGER: NativeInt = 9007199254740991;

type
  EWfcBrowserStreamFile = class(Exception);
  EWfcBrowserStreamCancelled = class(EWfcBrowserStreamFile);

  TWfcBrowserStreamFileState = (
    wbsfsReady,
    wbsfsOpening,
    wbsfsWritable,
    wbsfsCommitting,
    wbsfsAborting,
    wbsfsCommitted,
    wbsfsCancelled,
    wbsfsFailed,
    wbsfsReleased
  );

  { One browser-selected sequential file transaction. The caller evaluates the
    picker promise synchronously in its user-activation handler and passes that
    promise to Open. WriteBytes copies one borrowed block into a Uint8Array and
    awaits its write before returning, so no two blocks can be in flight.

    Cancel is cooperative while picker/open/write promises are pending. The
    owner awaits that active method and then calls Abort; Abort rejects while
    open/write is pending and cannot race Commit. A successful backend abort is
    idempotent. Commit makes cancellation irreversible before awaiting close.
    Release suppresses no promises and frees nothing: the owner retains this
    object until its active async method settles. Browser picker replacement
    policy remains user-mediated and is not changed here. }
  TWfcBrowserStreamFile = class
  strict private
    FWritable: TJSObject;
    FState: TWfcBrowserStreamFileState;
    FCancelled, FReleased, FOpenPending, FWritePending,
      FAbortPending, FAborted: Boolean;
    FByteCount: NativeInt;
    FPeakBlockBytes: Integer;
    procedure RequireCurrent(const AOperation: String);
    function GetCommitting: Boolean;
  public
    constructor Create;
    procedure Open(const APickerPromise: TJSPromise); async;
    procedure WriteBytes(const ABytes: array of Byte;
      const AMimeType: String); async;
    procedure Commit; async;
    procedure Abort; async;
    procedure Cancel;
    procedure Release;
    property State: TWfcBrowserStreamFileState read FState;
    property Cancelled: Boolean read FCancelled;
    property Released: Boolean read FReleased;
    property Committing: Boolean read GetCommitting;
    property ByteCount: NativeInt read FByteCount;
    property PeakBlockBytes: Integer read FPeakBlockBytes;
  end;

function WfcBrowserStreamFileAvailable: Boolean;
function BeginWfcBrowserStreamFilePicker(const ASuggestedName,
  ADescription, AMimeType, AExtension: String): TJSPromise;
function WfcBrowserStreamFailureText(const AValue: JSValue): String;
function WfcBrowserStreamFailureIsPickerCancel(const AValue: JSValue): Boolean;

implementation

type
  TWfcBrowserWritable = class external name 'Object' (TJSObject)
    function write(const AData: TJSBlob): TJSPromise;
    function close: TJSPromise;
    function abort: TJSPromise;
  end;

  TWfcBrowserFileHandle = class external name 'Object' (TJSObject)
    function createWritable: TJSPromise;
  end;

  TWfcBrowserSaveWindow = class external name 'Window' (TJSWindow)
    function showSaveFilePicker(const AOptions: TJSObject): TJSPromise;
      reintroduce;
  end;

function WfcBrowserStreamFailureText(const AValue: JSValue): String;
begin
  if isObject(AValue) and (TObject(AValue) is Exception) then
    Result := Exception(TObject(AValue)).Message
  else Result := String(AValue);
  if Result = '' then Result := 'unknown browser file failure';
end;

function WfcBrowserStreamFailureIsPickerCancel(
  const AValue: JSValue): Boolean;
begin
  Result := isObject(AValue) and isString(TJSObject(AValue)['name']) and
    (String(TJSObject(AValue)['name']) = 'AbortError');
end;

function WfcBrowserStreamFileAvailable: Boolean;
begin
  Result := isFunction(TJSObject(window)['showSaveFilePicker']);
end;

function BeginWfcBrowserStreamFilePicker(const ASuggestedName,
  ADescription, AMimeType, AExtension: String): TJSPromise;
var
  LAccept, LOptions, LType: TJSObject;
begin
  if not WfcBrowserStreamFileAvailable then
    raise EWfcBrowserStreamFile.Create(
      'direct browser streaming save is unavailable');
  if (ASuggestedName = '') or (ADescription = '') or
      (AMimeType = '') or (AExtension = '') then
    raise EWfcBrowserStreamFile.Create(
      'browser save metadata must be nonempty');
  LOptions := TJSObject.new;
  LOptions['suggestedName'] := ASuggestedName;
  LType := TJSObject.new;
  LType['description'] := ADescription;
  LAccept := TJSObject.new;
  LAccept[AMimeType] := TJSArray.new(AExtension);
  LType['accept'] := LAccept;
  LOptions['types'] := TJSArray.new(LType);
  { No await precedes this call: callers invoke this function directly from the
    click handler whose activation authorizes the picker. }
  Result := TWfcBrowserSaveWindow(window).showSaveFilePicker(LOptions);
end;

constructor TWfcBrowserStreamFile.Create;
begin
  inherited Create;
  FState := wbsfsReady;
end;

function TWfcBrowserStreamFile.GetCommitting: Boolean;
begin
  Result := FState = wbsfsCommitting;
end;

procedure TWfcBrowserStreamFile.RequireCurrent(const AOperation: String);
begin
  if FReleased then
    raise EWfcBrowserStreamCancelled.Create(AOperation +
      ' was released before completion');
  if FCancelled then
    raise EWfcBrowserStreamCancelled.Create(AOperation + ' was cancelled');
end;

procedure TWfcBrowserStreamFile.Open(
  const APickerPromise: TJSPromise); async;
var
  LHandle: TWfcBrowserFileHandle;
  LWritable: TJSObject;
begin
  if FState <> wbsfsReady then
    raise EWfcBrowserStreamFile.Create('browser file transaction is not ready');
  if APickerPromise = nil then
    raise EWfcBrowserStreamFile.Create('browser picker promise is required');
  FState := wbsfsOpening;
  FOpenPending := True;
  try
    try
      LHandle := await(TWfcBrowserFileHandle, APickerPromise);
      RequireCurrent('browser picker');
      LWritable := await(TJSObject, LHandle.createWritable);
      FWritable := LWritable;
      RequireCurrent('browser writable open');
      FState := wbsfsWritable;
    except
      if FReleased then FState := wbsfsReleased
      else if FCancelled then FState := wbsfsCancelled
      else FState := wbsfsFailed;
      raise;
    end;
  finally
    FOpenPending := False;
  end;
end;

procedure TWfcBrowserStreamFile.WriteBytes(const ABytes: array of Byte;
  const AMimeType: String); async;
var
  I, LByteCount: Integer;
  LBlob: TJSBlob;
  LBlock: TJSUint8Array;
  LOptions: TJSBlobInit;
  LParts: TJSArray;
begin
  RequireCurrent('browser write');
  if (FState <> wbsfsWritable) or (FWritable = nil) then
    raise EWfcBrowserStreamFile.Create('browser file is not writable');
  if FWritePending then
    raise EWfcBrowserStreamFile.Create('browser writes cannot overlap');
  LByteCount := Length(ABytes);
  if LByteCount < 1 then
    raise EWfcBrowserStreamFile.Create('browser byte block cannot be empty');
  if FByteCount > WFC_BROWSER_STREAM_FILE_MAX_SAFE_INTEGER - LByteCount then
    raise EWfcBrowserStreamFile.Create(
      'browser byte count exceeds the exact integer envelope');
  LBlock := TJSUint8Array.new(LByteCount);
  for I := 0 to LByteCount - 1 do LBlock[I] := ABytes[I];
  LParts := TJSArray.new;
  LParts.push(LBlock);
  LOptions := TJSBlobInit.new;
  LOptions.type_ := AMimeType;
  LBlob := TJSBlob.new(LParts, LOptions);
  FWritePending := True;
  try
    try
      await(TWfcBrowserWritable(FWritable).write(LBlob));
      RequireCurrent('browser write completion');
      Inc(FByteCount, LByteCount);
      if LByteCount > FPeakBlockBytes then
        FPeakBlockBytes := LByteCount;
    except
      if FReleased then FState := wbsfsReleased
      else if FCancelled then FState := wbsfsCancelled
      else FState := wbsfsFailed;
      raise;
    end;
  finally
    FWritePending := False;
  end;
end;

procedure TWfcBrowserStreamFile.Commit; async;
begin
  RequireCurrent('browser commit');
  if (FState <> wbsfsWritable) or (FWritable = nil) then
    raise EWfcBrowserStreamFile.Create('browser file is not ready to commit');
  if FWritePending then
    raise EWfcBrowserStreamFile.Create(
      'browser file cannot commit during a write');
  FState := wbsfsCommitting;
  try
    await(TWfcBrowserWritable(FWritable).close);
  except
    FState := wbsfsFailed;
    raise;
  end;
  FState := wbsfsCommitted;
end;

procedure TWfcBrowserStreamFile.Abort; async;
begin
  if FState = wbsfsCommitted then Exit;
  if FState = wbsfsCommitting then
    raise EWfcBrowserStreamFile.Create(
      'browser abort cannot overlap an irreversible commit');
  if FOpenPending or FWritePending then
    raise EWfcBrowserStreamFile.Create(
      'cancel and await the active browser operation before aborting');
  if FAbortPending then
    raise EWfcBrowserStreamFile.Create('browser abort is already pending');
  if FAborted then Exit;
  FCancelled := True;
  FState := wbsfsAborting;
  FAbortPending := True;
  try
    try
      if FWritable <> nil then
        await(TWfcBrowserWritable(FWritable).abort);
      FAborted := True;
      if FReleased then FState := wbsfsReleased
      else FState := wbsfsCancelled;
    except
      FState := wbsfsFailed;
      raise;
    end;
  finally
    FAbortPending := False;
  end;
end;

procedure TWfcBrowserStreamFile.Cancel;
begin
  if FState in [wbsfsCommitting, wbsfsAborting, wbsfsCommitted,
      wbsfsFailed, wbsfsCancelled, wbsfsReleased] then Exit;
  FCancelled := True;
  if (FState = wbsfsReady) and not FOpenPending and not FWritePending then
    FState := wbsfsCancelled;
end;

procedure TWfcBrowserStreamFile.Release;
begin
  if FReleased then Exit;
  FReleased := True;
  if not (FState in [wbsfsCommitting, wbsfsCommitted]) then Cancel;
  if (FState = wbsfsReady) or
      ((FState = wbsfsCancelled) and (FWritable = nil)) then
    FState := wbsfsReleased;
end;

end.
