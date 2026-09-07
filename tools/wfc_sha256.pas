{ SPDX-License-Identifier: MIT
  Copyright (c) 2026 mr-highball
  Project-owned byte-oriented SHA-256. See ../docs/asset-sha256.md for sources,
  scope and checked-arithmetic reasoning. No external cryptographic library. }
unit wfc_sha256;

{$mode delphi}{$H+}

interface

uses
  SysUtils;

type
  EWfcSha256 = class(Exception);
  TWfcSha256Bytes = array of Byte;
  TWfcSha256Digest = array[0..31] of Byte;
  TWfcSha256BitLength = record
    High, Low: Cardinal;
  end;

  TWfcSha256Context = class
  private
    FState: array[0..7] of Cardinal;
    FBuffer: array[0..63] of Byte;
    FUsed: Integer;
    FLength: TWfcSha256BitLength;
    FFinalized: Boolean;
    procedure RequireOpen;
    procedure Compress;
  public
    constructor Create;
    procedure Reset;
    procedure Update(const ABytes: TWfcSha256Bytes); overload;
    procedure Update(const ABytes: TWfcSha256Bytes;
      const AOffset, ACount: Integer); overload;
    function Finish: TWfcSha256Digest;
    function CopyBitLength: TWfcSha256BitLength;
    property Finalized: Boolean read FFinalized;
  end;

{ Pure counter operation; rejects non-byte-aligned input and a sum >= 2^64
  bits. It cannot set a context's internal counter or compression state. }
function WfcSha256AddByteLength(const ALength: TWfcSha256BitLength;
  const AByteCount: Cardinal): TWfcSha256BitLength;
function CalculateWfcSha256(const ABytes: TWfcSha256Bytes): TWfcSha256Digest;
function WfcSha256DigestHex(const ADigest: TWfcSha256Digest): String;

implementation

const
  RoundConstants: array[0..63] of Cardinal = (
    $428A2F98,$71374491,$B5C0FBCF,$E9B5DBA5,
    $3956C25B,$59F111F1,$923F82A4,$AB1C5ED5,
    $D807AA98,$12835B01,$243185BE,$550C7DC3,
    $72BE5D74,$80DEB1FE,$9BDC06A7,$C19BF174,
    $E49B69C1,$EFBE4786,$0FC19DC6,$240CA1CC,
    $2DE92C6F,$4A7484AA,$5CB0A9DC,$76F988DA,
    $983E5152,$A831C66D,$B00327C8,$BF597FC7,
    $C6E00BF3,$D5A79147,$06CA6351,$14292967,
    $27B70A85,$2E1B2138,$4D2C6DFC,$53380D13,
    $650A7354,$766A0ABB,$81C2C92E,$92722C85,
    $A2BFE8A1,$A81A664B,$C24B8B70,$C76C51A3,
    $D192E819,$D6990624,$F40E3585,$106AA070,
    $19A4C116,$1E376C08,$2748774C,$34B0BCB5,
    $391C0CB3,$4ED8AA4A,$5B9CCA4F,$682E6FF3,
    $748F82EE,$78A5636F,$84C87814,$8CC70208,
    $90BEFFFA,$A4506CEB,$BEF9A3F7,$C67178F2);

procedure HashError(const AMessage: String);
begin
  raise EWfcSha256.Create('SHA-256: ' + AMessage);
end;

function Add32(const A, B: Cardinal): Cardinal; inline;
var
  LLow, LHigh: Cardinal;
begin
  { No overflowing addition followed by a mask: both lane sums <= 131071.
    Reassembly <= High(Cardinal); no UInt64/JS floating integer truncation. }
  LLow := (A and $FFFF) + (B and $FFFF);
  LHigh := (A shr 16) + (B shr 16) + (LLow shr 16);
  Result := (LHigh and $FFFF) * Cardinal(65536) + (LLow and $FFFF);
end;

function RotateRight(const AValue: Cardinal; const ACount: Integer): Cardinal; inline;
begin
  { All internal counts are 1..31; shifts discard bits by definition. }
  Result := (AValue shr ACount) or (AValue shl (32 - ACount));
end;

function ArrayLength(const ABytes: TWfcSha256Bytes): Integer;
{$IFDEF PAS2JS}
var LValid: Boolean;
{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    LValid = Array.isArray(ABytes) && ABytes.length <= 2147483647;
  end;
  if not LValid then HashError('invalid byte array');
  {$ELSE}
  { SizeInt may be wider than this API's Integer offsets on native 64-bit.
    Refuse before narrowing, including in callers built without range checks. }
  if Length(ABytes) > High(Integer) then HashError('byte array exceeds Integer range');
  {$ENDIF}
  Result := Length(ABytes);
end;

procedure CheckRange(const ABytes: TWfcSha256Bytes;
  const AOffset, ACount: Integer);
var
  LLength: Integer;
  {$IFDEF PAS2JS}LValid: Boolean;{$ENDIF}
begin
  LLength := ArrayLength(ABytes);
  {$IFDEF PAS2JS}
  asm
    LValid = typeof AOffset === 'number' && Number.isInteger(AOffset) &&
      AOffset >= 0 && AOffset <= 2147483647 &&
      typeof ACount === 'number' && Number.isInteger(ACount) &&
      ACount >= 0 && ACount <= 2147483647;
  end;
  if not LValid then HashError('invalid byte range');
  {$ENDIF}
  if (AOffset < 0) or (ACount < 0) or (AOffset > LLength) then
    HashError('invalid byte range');
  if ACount > LLength - AOffset then HashError('invalid byte range');
  {$IFDEF PAS2JS}
  { Only the selected range is input. Never invoke supplied getters, slice,
    iterators or numeric conversions. No Proxy/global-intrinsic sandbox. }
  asm
    LValid = true;
    for (var i = AOffset; i < AOffset + ACount; i++) {
      var d = Object.getOwnPropertyDescriptor(ABytes, String(i));
      if (!d || !Object.prototype.hasOwnProperty.call(d, 'value') ||
          typeof d.value !== 'number' || !Number.isInteger(d.value) ||
          d.value < 0 || d.value > 255) { LValid = false; break; }
    }
  end;
  if not LValid then HashError('invalid byte element');
  {$ENDIF}
end;

function WfcSha256AddByteLength(const ALength: TWfcSha256BitLength;
  const AByteCount: Cardinal): TWfcSha256BitLength;
var
  LHighAdd, LLowAdd, LCarry: Cardinal;
  {$IFDEF PAS2JS}LValid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    function word(v) {
      return typeof v === 'number' && Number.isInteger(v) && v >= 0 &&
        v <= 4294967295;
    }
    function field(o, k) {
      var d = Object.getOwnPropertyDescriptor(o, k);
      return d && Object.prototype.hasOwnProperty.call(d, 'value') && word(d.value);
    }
    LValid = ALength !== null && typeof ALength === 'object' &&
      !Array.isArray(ALength) && field(ALength, 'High') && field(ALength, 'Low') &&
      word(AByteCount);
  end;
  if not LValid then HashError('invalid bit length');
  {$ENDIF}
  if (ALength.Low and 7) <> 0 then HashError('bit length is not byte aligned');
  LLowAdd := (AByteCount and $1FFFFFFF) * Cardinal(8);
  LHighAdd := AByteCount shr 29;
  Result.Low := Add32(ALength.Low, LLowAdd);
  if Result.Low < ALength.Low then LCarry := 1 else LCarry := 0;
  LHighAdd := LHighAdd + LCarry; //At most 8.
  if ALength.High > High(Cardinal) - LHighAdd then
    HashError('message length exceeds byte-oriented SHA-256 capacity');
  Result.High := ALength.High + LHighAdd;
end;

constructor TWfcSha256Context.Create;
begin
  inherited Create;
  Reset;
end;

procedure TWfcSha256Context.RequireOpen;
begin
  if FFinalized then HashError('context is finalized; Reset before reuse');
end;

procedure TWfcSha256Context.Reset;
var I: Integer;
begin
  FState[0] := $6A09E667; FState[1] := $BB67AE85;
  FState[2] := $3C6EF372; FState[3] := $A54FF53A;
  FState[4] := $510E527F; FState[5] := $9B05688C;
  FState[6] := $1F83D9AB; FState[7] := $5BE0CD19;
  for I := 0 to 63 do FBuffer[I] := 0;
  FUsed := 0;
  FLength.High := 0; FLength.Low := 0;
  FFinalized := False;
end;

procedure TWfcSha256Context.Compress;
var
  W: array[0..63] of Cardinal;
  A, B, C, D, E, F, G, H, T1, T2, S0, S1: Cardinal;
  I, J: Integer;
begin
  for I := 0 to 15 do
  begin
    J := I * 4;
    W[I] := Cardinal(FBuffer[J]) * Cardinal(16777216)
      + Cardinal(FBuffer[J + 1]) * Cardinal(65536)
      + Cardinal(FBuffer[J + 2]) * Cardinal(256) + FBuffer[J + 3];
  end;
  for I := 16 to 63 do
  begin
    S0 := RotateRight(W[I - 15], 7) xor RotateRight(W[I - 15], 18)
      xor (W[I - 15] shr 3);
    S1 := RotateRight(W[I - 2], 17) xor RotateRight(W[I - 2], 19)
      xor (W[I - 2] shr 10);
    W[I] := Add32(Add32(W[I - 16], S0), Add32(W[I - 7], S1));
  end;
  A := FState[0]; B := FState[1]; C := FState[2]; D := FState[3];
  E := FState[4]; F := FState[5]; G := FState[6]; H := FState[7];
  for I := 0 to 63 do
  begin
    S1 := RotateRight(E, 6) xor RotateRight(E, 11) xor RotateRight(E, 25);
    T1 := Add32(Add32(H, S1), (E and F) xor ((not E) and G));
    T1 := Add32(Add32(T1, RoundConstants[I]), W[I]);
    S0 := RotateRight(A, 2) xor RotateRight(A, 13) xor RotateRight(A, 22);
    T2 := Add32(S0, (A and B) xor (A and C) xor (B and C));
    H := G; G := F; F := E; E := Add32(D, T1);
    D := C; C := B; B := A; A := Add32(T1, T2);
  end;
  FState[0] := Add32(FState[0], A); FState[1] := Add32(FState[1], B);
  FState[2] := Add32(FState[2], C); FState[3] := Add32(FState[3], D);
  FState[4] := Add32(FState[4], E); FState[5] := Add32(FState[5], F);
  FState[6] := Add32(FState[6], G); FState[7] := Add32(FState[7], H);
end;

procedure TWfcSha256Context.Update(const ABytes: TWfcSha256Bytes);
begin
  RequireOpen;
  Update(ABytes, 0, ArrayLength(ABytes));
end;

procedure TWfcSha256Context.Update(const ABytes: TWfcSha256Bytes;
  const AOffset, ACount: Integer);
var
  LNext: TWfcSha256BitLength;
  I, LPosition: Integer;
begin
  RequireOpen;
  CheckRange(ABytes, AOffset, ACount);
  LNext := WfcSha256AddByteLength(FLength, Cardinal(ACount));
  { All expected validation errors precede the first state write. }
  LPosition := AOffset;
  for I := 1 to ACount do
  begin
    FBuffer[FUsed] := ABytes[LPosition];
    FUsed := FUsed + 1;
    LPosition := LPosition + 1;
    if FUsed = 64 then
    begin
      Compress;
      FUsed := 0;
    end;
  end;
  FLength.High := LNext.High; FLength.Low := LNext.Low;
end;

function TWfcSha256Context.Finish: TWfcSha256Digest;
var I, J: Integer;
begin
  RequireOpen;
  FBuffer[FUsed] := $80;
  FUsed := FUsed + 1;
  if FUsed > 56 then
  begin
    for I := FUsed to 63 do FBuffer[I] := 0;
    Compress;
    FUsed := 0;
  end;
  for I := FUsed to 55 do FBuffer[I] := 0;
  for I := 0 to 3 do
  begin
    FBuffer[56 + I] := Byte((FLength.High shr ((3 - I) * 8)) and $FF);
    FBuffer[60 + I] := Byte((FLength.Low shr ((3 - I) * 8)) and $FF);
  end;
  Compress;
  for I := 0 to 7 do
    for J := 0 to 3 do
      Result[I * 4 + J] := Byte((FState[I] shr ((3 - J) * 8)) and $FF);
  for I := 0 to 63 do FBuffer[I] := 0;
  FUsed := 0;
  FFinalized := True;
end;

function TWfcSha256Context.CopyBitLength: TWfcSha256BitLength;
begin
  Result.High := FLength.High;
  Result.Low := FLength.Low;
end;

function CalculateWfcSha256(const ABytes: TWfcSha256Bytes): TWfcSha256Digest;
var LContext: TWfcSha256Context;
begin
  LContext := TWfcSha256Context.Create;
  try
    LContext.Update(ABytes);
    Result := LContext.Finish;
  finally
    LContext.Free;
  end;
end;

function WfcSha256DigestHex(const ADigest: TWfcSha256Digest): String;
const Digits = '0123456789ABCDEF';
var
  I: Integer;
  {$IFDEF PAS2JS}LValid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    LValid = Array.isArray(ADigest) && ADigest.length === 32;
    if (LValid) for (var i = 0; i < 32; i++) {
      var d = Object.getOwnPropertyDescriptor(ADigest, String(i));
      if (!d || !Object.prototype.hasOwnProperty.call(d, 'value') ||
          typeof d.value !== 'number' || !Number.isInteger(d.value) ||
          d.value < 0 || d.value > 255) { LValid = false; break; }
    }
  end;
  if not LValid then HashError('invalid digest');
  {$ENDIF}
  Result := '';
  for I := 0 to 31 do
    Result := Result + Digits[(ADigest[I] shr 4) + 1]
      + Digits[(ADigest[I] and 15) + 1];
end;

end.
