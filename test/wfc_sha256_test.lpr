{ SPDX-License-Identifier: MIT
  Copyright (c) 2026 mr-highball
  Independent published vectors plus project-owned state/streaming tests. }
program wfc_sha256_test;
{$mode delphi}{$H+}
uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc_sha256;

const
  EmptyHex = 'E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855';
  AbcHex = 'BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD';
var
  Checks, Failures: Integer;

procedure Check(const AOK: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not AOK then
  begin
    Inc(Failures);
    WriteLn('[FAIL] ', AMessage);
  end;
end;

function Bytes(const AText: String): TWfcSha256Bytes;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AText));
  for I := 1 to Length(AText) do
  begin
    if Ord(AText[I]) > 127 then raise Exception.Create('test text is not ASCII');
    Result[I - 1] := Byte(Ord(AText[I]));
  end;
end;

function FromHex(const AText: String): TWfcSha256Bytes;
var I: Integer;
begin
  if (Length(AText) mod 2) <> 0 then raise Exception.Create('odd test hex');
  Result := nil;
  SetLength(Result, Length(AText) div 2);
  for I := 0 to High(Result) do
    Result[I] := Byte(StrToInt('$' + Copy(AText, I * 2 + 1, 2)));
end;

procedure Vector(const ABytes: TWfcSha256Bytes; const AExpected, AName: String);
var
  C: TWfcSha256Context;
  I: Integer;
  L: TWfcSha256BitLength;
begin
  Check(WfcSha256DigestHex(CalculateWfcSha256(ABytes)) = AExpected,
    AName + ': independent one-shot vector');
  C := TWfcSha256Context.Create;
  try
    for I := 0 to High(ABytes) do C.Update(ABytes, I, 1);
    C.Update(ABytes, Length(ABytes), 0);
    L := C.CopyBitLength;
    Check((L.High = 0) and (L.Low = Cardinal(Length(ABytes)) * 8),
      AName + ': original bit count');
    Check(WfcSha256DigestHex(C.Finish) = AExpected,
      AName + ': one-byte chunking');
    L := C.CopyBitLength;
    Check((L.High = 0) and (L.Low = Cardinal(Length(ABytes)) * 8),
      AName + ': padding excluded from bit count');
  finally C.Free; end;
end;

procedure KnownVectors;
var
  B: TWfcSha256Bytes;
  C: TWfcSha256Context;
  I: Integer;
begin
  { NIST worked examples and RFC 6234 byte-oriented test cases. The empty
    hash is also the empty transcript hash in RFC 8448 section 3. }
  Vector(Bytes(''), EmptyHex, 'empty');
  Vector(Bytes('abc'), AbcHex, 'NIST abc');
  Vector(Bytes('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'),
    '248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1',
    'NIST 56-byte padding boundary');
  Vector(FromHex('19'),
    '68AA2E2EE5DFF96E3355E6C7EE373E3D6A4E17F75F9518D843709C0C9BC3E3D4',
    'RFC 6234 single binary byte');
  Vector(FromHex('E3D72570DCDD787CE3887AB2CD684652'),
    '175EE69B02BA9B58E2B0A5FD13819CEA573F3940A94F825128CF4209BEABB4E8',
    'RFC 6234 binary 16 bytes');
  Vector(FromHex(
    '8326754E2277372F4FC12B20527AFEF04D8A056971B11AD57123A7C137760000' +
    'D7BEF6F3C1F7A9083AA39D810DB310777DAB8B1E7F02B84A26C773325F8B2374' +
    'DE7A4B5A58CB5C5CF35BCEE6FB946E5BD694FA593A8BEB3F9D6592ECEDAA66CA' +
    '82A29D0C51BCF9336230E5D784E4C0A43F8D79A30A165CBABE452B774B9C7109' +
    'A97D138F129228966F6C0ADC106AAD5A9FDD30825769B2C671AF6759DF28EB393D54D6'),
    '97DBCA7DF46D62C8A422C941DD7E835B8AD3361763F7E9B2D95F4F0DA6E1CCBC',
    'RFC 6234 binary 163 bytes including zero');
  B := Bytes('0123456701234567012345670123456701234567012345670123456701234567');
  C := TWfcSha256Context.Create;
  try
    for I := 1 to 10 do C.Update(B);
    Check(WfcSha256DigestHex(C.Finish) =
      '594847328451BDFA85056225462CC1D867D877FB388DF0CE35F25AB5562BFBB5',
      'RFC 6234 ten exact blocks');
    C.Reset;
    SetLength(B, 1000);
    for I := 0 to High(B) do B[I] := Ord('a');
    for I := 1 to 1000 do C.Update(B);
    Check(WfcSha256DigestHex(C.Finish) =
      'CDC76E5C9914FB9281A1C7E284D73E67F1809A48A497200E046D39CCC7112CD0',
      'RFC 6234 million a with bounded input buffer');
  finally C.Free; end;
end;

procedure BlockBoundaries;
const Lengths: array[0..14] of Integer =
  (0,1,2,54,55,56,57,63,64,65,119,120,127,128,129);
var
  B: TWfcSha256Bytes;
  C: TWfcSha256Context;
  Expected: String;
  N, I, Split, Chunk, Position, Count: Integer;
begin
  C := TWfcSha256Context.Create;
  try
    for N := 0 to High(Lengths) do
    begin
      SetLength(B, Lengths[N]);
      for I := 0 to High(B) do B[I] := Byte((I * 73 + 201) mod 256);
      Expected := WfcSha256DigestHex(CalculateWfcSha256(B));
      { These are metamorphic comparisons, not independent published vectors. }
      for Split := 0 to Length(B) do
      begin
        C.Reset;
        C.Update(B, 0, Split);
        C.Update(B, Split, Length(B) - Split);
        Check(WfcSha256DigestHex(C.Finish) = Expected,
          'all split points at length ' + IntToStr(Length(B)));
      end;
      for Chunk := 1 to 67 do
      begin
        C.Reset;
        Position := 0;
        while Position < Length(B) do
        begin
          Count := Length(B) - Position;
          if Count > Chunk then Count := Chunk;
          C.Update(B, Position, Count);
          Position := Position + Count;
        end;
        Check(WfcSha256DigestHex(C.Finish) = Expected,
          'chunk sizes 1..67 at length ' + IntToStr(Length(B)));
      end;
    end;
  finally C.Free; end;
end;

procedure RangeAndLifecycle;
const Offsets: array[0..6] of Integer = (-1,0,4,2,High(Integer),1,3);
      Counts: array[0..6] of Integer = (1,-1,0,2,High(Integer),High(Integer),1);
var
  B: TWfcSha256Bytes;
  D: TWfcSha256Digest;
  L: TWfcSha256BitLength;
  C, Other: TWfcSha256Context;
  I: Integer;
  Raised: Boolean;
begin
  B := Bytes('abc');
  C := TWfcSha256Context.Create;
  Other := TWfcSha256Context.Create;
  try
    Check(not C.Finalized, 'new context open');
    C.Update(B, 0, 1);
    for I := 0 to High(Offsets) do
    begin
      Raised := False;
      try C.Update(B, Offsets[I], Counts[I]);
      except on E: EWfcSha256 do Raised := Length(E.Message) > 0; end;
      Check(Raised, 'invalid range raises typed nonempty diagnostic');
      L := C.CopyBitLength;
      Check((L.High = 0) and (L.Low = 8) and not C.Finalized,
        'invalid range did not mutate state/count');
    end;
    C.Update(B, 1, 2);
    B[0] := 0; B[1] := 255; B[2] := 0;
    D := C.Finish;
    Check(WfcSha256DigestHex(D) = AbcHex, 'Update retains bytes, not caller array');
    Check(C.Finalized, 'Finish closes context');
    Raised := False;
    try C.Update(B, 0, 0); except on EWfcSha256 do Raised := True; end;
    Check(Raised, 'even empty ranged update after Finish rejects');
    Raised := False;
    try C.Update(nil); except on EWfcSha256 do Raised := True; end;
    Check(Raised, 'empty whole update after Finish rejects');
    Raised := False;
    try C.Finish; except on EWfcSha256 do Raised := True; end;
    Check(Raised, 'second Finish rejects');
    C.Reset;
    Check(not C.Finalized, 'Reset reopens');
    L := C.CopyBitLength;
    Check((L.High = 0) and (L.Low = 0), 'Reset clears length');
    L.High := 123; L.Low := 456;
    L := C.CopyBitLength;
    Check((L.High = 0) and (L.Low = 0), 'copied bit-length record is detached');
    Check(WfcSha256DigestHex(C.Finish) = EmptyHex, 'Reset clears unfinished buffer/state');
    Check(WfcSha256DigestHex(D) = AbcHex, 'digest detached from subsequent Reset/Finish');
    D[0] := 0;
    Other.Update(Bytes('abc'));
    Check(WfcSha256DigestHex(Other.Finish) = AbcHex,
      'independent contexts and detached digest mutation');
    C.Reset; C.Update(Bytes('unfinished'));
    C.Reset; C.Update(Bytes('abc'));
    Check(WfcSha256DigestHex(C.Finish) = AbcHex, 'Reset abandons open partial message');
  finally Other.Free; C.Free; end;
end;

procedure LengthArithmetic;
var
  L, R, Original: TWfcSha256BitLength;
  Raised: Boolean;
  I: Integer;
begin
  L.High := 0; L.Low := 0;
  R := WfcSha256AddByteLength(L, 0);
  Check((R.High = 0) and (R.Low = 0), 'zero counter addition');
  R := WfcSha256AddByteLength(L, $1FFFFFFF);
  Check((R.High = 0) and (R.Low = $FFFFFFF8), 'last byte before low-word carry');
  R := WfcSha256AddByteLength(R, 1);
  Check((R.High = 1) and (R.Low = 0), 'one-byte carry across 2^32 bits');
  R := WfcSha256AddByteLength(L, $FFFFFFFF);
  Check((R.High = 7) and (R.Low = $FFFFFFF8), 'full uint32 byte count has exact high lane');
  R := WfcSha256AddByteLength(R, 1);
  Check((R.High = 8) and (R.Low = 0), 'four GiB byte-count boundary');
  L.High := $12345678; L.Low := $FFFF0000;
  R := WfcSha256AddByteLength(L, $FFFFFFFF);
  Check((R.High = $12345680) and (R.Low = $FFFEFFF8), 'high addition and low carry');
  L.High := $FFFFFFFF; L.Low := $FFFFFFF0;
  R := WfcSha256AddByteLength(L, 1);
  Check((R.High = $FFFFFFFF) and (R.Low = $FFFFFFF8), 'maximum supported byte-aligned length');
  R := WfcSha256AddByteLength(R, 0);
  Check((R.High = $FFFFFFFF) and (R.Low = $FFFFFFF8), 'zero addition at maximum is valid');
  Original := R;
  Raised := False;
  try WfcSha256AddByteLength(R, 1); except on EWfcSha256 do Raised := True; end;
  Check(Raised, '2^64-bit wrap rejected');
  Check((R.High = Original.High) and (R.Low = Original.Low), 'overflow leaves input untouched');
  L.High := $FFFFFFF8; L.Low := 8;
  Raised := False;
  try WfcSha256AddByteLength(L, $FFFFFFFF); except on EWfcSha256 do Raised := True; end;
  Check(Raised, 'combined high addition and carry overflow rejected');
  for I := 1 to 7 do
  begin
    L.High := 0; L.Low := Cardinal(I);
    Raised := False;
    try WfcSha256AddByteLength(L, 0); except on EWfcSha256 do Raised := True; end;
    Check(Raised, 'partial-bit length is outside byte API');
  end;
end;

{$IFDEF PAS2JS}
procedure HostileBytesCase(const Mode: Integer);
var
  B: TWfcSha256Bytes;
  C: TWfcSha256Context;
  Offset, Count, Calls: Integer;
  Raised: Boolean;
begin
  B := Bytes('abc'); Offset := 0; Count := 3; Calls := 0;
  asm
    switch (Mode) {
      case 0: B = null; break;
      case 1: B = {}; Object.defineProperty(B, 'length', {get:function(){Calls++;return 3;}}); break;
      case 2: B = new Uint8Array([97,98,99]); break;
      case 3: delete B[1]; break;
      case 4: Object.defineProperty(B, '1', {get:function(){Calls++;return 98;}}); break;
      case 5: var p = Object.create(Array.prototype); p[1] = 98;
        delete B[1]; Object.setPrototypeOf(B,p); break;
      case 6: B[1] = NaN; break;
      case 7: B[1] = 0.5; break;
      case 8: B[1] = -1; break;
      case 9: B[1] = 256; break;
      case 10: B[1] = '98'; break;
      case 11: B[1] = {valueOf:function(){Calls++;return 98;}}; break;
      case 12: Offset = NaN; break;
      case 13: Count = 0.5; break;
      case 14: Count = Infinity; break;
      case 15: Offset = {valueOf:function(){Calls++;return 0;}}; break;
    }
  end;
  C := TWfcSha256Context.Create;
  try
    C.Update(Bytes('a'));
    Raised := False;
    try C.Update(B, Offset, Count);
    except on E: EWfcSha256 do Raised := Length(E.Message) > 0; end;
    Check(Raised, 'hostile byte/range typed refusal ' + IntToStr(Mode));
    Check(Calls = 0, 'hostile byte/range no accessor/coercion ' + IntToStr(Mode));
    C.Update(Bytes('bc'));
    Check(WfcSha256DigestHex(C.Finish) = AbcHex,
      'hostile byte/range rejection leaves exact state ' + IntToStr(Mode));
  finally C.Free; end;
end;

procedure PassiveBytesCase(const Mode: Integer);
var B: TWfcSha256Bytes; Calls: Integer;
begin
  B := Bytes('abc'); Calls := 0;
  asm
    switch (Mode) {
      case 0: Object.freeze(B); break;
      case 1: Object.defineProperty(B,'slice',{get:function(){Calls++;return null;}}); break;
      case 2: B.slice=function(){Calls++;return this;}; break;
      case 3: B.slice=null; break;
    }
  end;
  Check(WfcSha256DigestHex(CalculateWfcSha256(B)) = AbcHex,
    'passive frozen/slice-shadow input accepted ' + IntToStr(Mode));
  Check(Calls = 0, 'supplied array methods never dispatched ' + IntToStr(Mode));
end;

procedure HostileLengthCase(const Mode: Integer);
var L: TWfcSha256BitLength; Count: Cardinal; Calls: Integer; Raised: Boolean;
begin
  L.High := 0; L.Low := 0; Count := 1; Calls := 0;
  asm
    switch (Mode) {
      case 0: L = null; break;
      case 1: L = []; break;
      case 2: L = {Low:0}; break;
      case 3: L = {Low:0}; Object.defineProperty(L,'High',{get:function(){Calls++;return 0;}}); break;
      case 4: L.High = NaN; break;
      case 5: L.Low = 0.5; break;
      case 6: L.Low = 4294967296; break;
      case 7: L.High = -1; break;
      case 8: Count = Infinity; break;
      case 9: Count = '1'; break;
      case 10: Count = {valueOf:function(){Calls++;return 1;}}; break;
      case 11: L = Object.create({High:0,Low:0}); break;
    }
  end;
  Raised := False;
  try WfcSha256AddByteLength(L, Count);
  except on E: EWfcSha256 do Raised := Length(E.Message) > 0; end;
  Check(Raised, 'raw length/count typed refusal ' + IntToStr(Mode));
  Check(Calls = 0, 'raw length/count no getter/coercion ' + IntToStr(Mode));
end;

procedure HostileDigestCase(const Mode: Integer);
var D: TWfcSha256Digest; Calls: Integer; Raised: Boolean;
begin
  D := CalculateWfcSha256(Bytes('abc')); Calls := 0;
  asm
    switch (Mode) {
      case 0: D=null; break;
      case 1: D=[]; break;
      case 2: delete D[8]; break;
      case 3: Object.defineProperty(D,'8',{get:function(){Calls++;return 1;}}); break;
      case 4: D[8]=256; break;
      case 5: D[8]='1'; break;
    }
  end;
  Raised := False;
  try WfcSha256DigestHex(D);
  except on E: EWfcSha256 do Raised := Length(E.Message) > 0; end;
  Check(Raised, 'raw digest typed refusal ' + IntToStr(Mode));
  Check(Calls = 0, 'raw digest getter not invoked ' + IntToStr(Mode));
end;

procedure JavaScriptBoundaries;
var I: Integer; B: TWfcSha256Bytes; C: TWfcSha256Context; Calls: Integer;
begin
  for I := 0 to 15 do HostileBytesCase(I);
  for I := 0 to 3 do PassiveBytesCase(I);
  for I := 0 to 11 do HostileLengthCase(I);
  for I := 0 to 5 do HostileDigestCase(I);
  B := Bytes('xabcx'); Calls := 0;
  asm
    Object.defineProperty(B,'0',{get:function(){Calls++;return 120;}});
    Object.defineProperty(B,'4',{get:function(){Calls++;return 120;}});
  end;
  C := TWfcSha256Context.Create;
  try
    C.Update(B, 1, 3);
    Check(WfcSha256DigestHex(C.Finish) = AbcHex, 'range ignores unrelated outer slots');
    Check(Calls = 0, 'out-of-range slots never read');
  finally C.Free; end;
end;
{$ENDIF}

begin
  try
    KnownVectors;
    BlockBoundaries;
    RangeAndLifecycle;
    LengthArithmetic;
    {$IFDEF PAS2JS}JavaScriptBoundaries;{$ENDIF}
  except
    on E: Exception do
    begin
      Inc(Failures);
      WriteLn('[EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
  WriteLn('SHA-256 checks: ', Checks, '; failures: ', Failures);
  if Failures <> 0 then Halt(1);
end.
