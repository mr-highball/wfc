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
unit wfc_token_lookup;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc_model;

type
  EWfcTokenLookup = class(Exception);

  { Immutable token-to-original-index lookup. Token identity is exact: the
    lookup does not case-fold or normalize Unicode. Create rejects duplicate
    tokens, and Find returns -1 when the token is absent. }
  TWfcTokenLookup = class
  strict private
    FTokens: TWfcModelTokens;
    FSlots: array of Integer;
    FHashes: array of Cardinal;

    function GetCount: Integer;
    procedure InsertToken(const ATokenIndex: Integer);
  public
    constructor Create(const ATokens: TWfcModelTokens);

    function Find(const AToken: TWfcModelToken): Integer;
    function TokenAt(const AIndex: Integer): TWfcModelToken;

    property Count: Integer read GetCount;
  end;

implementation

const
  { This is the largest power-of-two capacity representable by the signed
    Integer indices used on every supported compiler. }
  WFC_TOKEN_LOOKUP_MAX_CAPACITY = 1073741824;

procedure HashByte(var AHash: Cardinal; const AValue: Byte);
{$PUSH}
{$Q-}
var
  LValue: Cardinal;
begin
  AHash := AHash xor Cardinal(AValue);
  LValue := AHash;
  AHash := (LValue + (LValue shl 1) + (LValue shl 4) +
    (LValue shl 7) + (LValue shl 8) + (LValue shl 24)) and
    Cardinal($FFFFFFFF);
end;
{$POP}

procedure HashCardinal(var AHash: Cardinal; const AValue: Cardinal);
begin
  HashByte(AHash, Byte(AValue and $FF));
  HashByte(AHash, Byte((AValue shr 8) and $FF));
  HashByte(AHash, Byte((AValue shr 16) and $FF));
  HashByte(AHash, Byte((AValue shr 24) and $FF));
end;

function TokenHash(const AToken: TWfcModelToken): Cardinal;
var
  I: Integer;
begin
  Result := Cardinal(2166136261);
  HashCardinal(Result, Cardinal(Length(AToken)));
  for I := 1 to Length(AToken) do
    HashCardinal(Result, Cardinal(Ord(AToken[I])));
end;

function CapacityForCount(const ACount: Integer): Integer;
var
  LRequired: Integer;
begin
  if ACount = 0 then
    Exit(0);
  if ACount > WFC_TOKEN_LOOKUP_MAX_CAPACITY div 2 then
    raise EWfcTokenLookup.Create('token count is too large for lookup');

  LRequired := ACount * 2;
  Result := 2;
  while Result < LRequired do
    Result := Result * 2;
end;

{ TWfcTokenLookup }

constructor TWfcTokenLookup.Create(const ATokens: TWfcModelTokens);
var
  I: Integer;
  LCapacity: Integer;
begin
  inherited Create;
  if Length(ATokens) > WFC_TOKEN_LOOKUP_MAX_CAPACITY div 2 then
    raise EWfcTokenLookup.Create('token count is too large for lookup');

  SetLength(FTokens, Length(ATokens));
  for I := 0 to Length(ATokens) - 1 do
    FTokens[I] := ATokens[I];

  LCapacity := CapacityForCount(Length(FTokens));
  SetLength(FSlots, LCapacity);
  SetLength(FHashes, LCapacity);
  for I := 0 to Length(FTokens) - 1 do
    InsertToken(I);
end;

function TWfcTokenLookup.GetCount: Integer;
begin
  Result := Length(FTokens);
end;

procedure TWfcTokenLookup.InsertToken(const ATokenIndex: Integer);
var
  LExistingIndex: Integer;
  LHash: Cardinal;
  LProbe: Integer;
  LSlot: Integer;
begin
  LHash := TokenHash(FTokens[ATokenIndex]);
  LSlot := Integer(LHash mod Cardinal(Length(FSlots)));
  for LProbe := 0 to Length(FSlots) - 1 do
  begin
    if FSlots[LSlot] = 0 then
    begin
      FSlots[LSlot] := ATokenIndex + 1;
      FHashes[LSlot] := LHash;
      Exit;
    end;

    LExistingIndex := FSlots[LSlot] - 1;
    if (FHashes[LSlot] = LHash) and
      (FTokens[LExistingIndex] = FTokens[ATokenIndex]) then
      raise EWfcTokenLookup.Create('duplicate token at index ' +
        IntToStr(ATokenIndex) + ' (first seen at index ' +
        IntToStr(LExistingIndex) + ')');

    Inc(LSlot);
    if LSlot = Length(FSlots) then
      LSlot := 0;
  end;

  raise EWfcTokenLookup.Create('token lookup table is unexpectedly full');
end;

function TWfcTokenLookup.Find(const AToken: TWfcModelToken): Integer;
var
  LHash: Cardinal;
  LProbe: Integer;
  LSlot: Integer;
begin
  if Length(FSlots) = 0 then
    Exit(-1);

  LHash := TokenHash(AToken);
  LSlot := Integer(LHash mod Cardinal(Length(FSlots)));
  for LProbe := 0 to Length(FSlots) - 1 do
  begin
    if FSlots[LSlot] = 0 then
      Exit(-1);

    Result := FSlots[LSlot] - 1;
    if (FHashes[LSlot] = LHash) and (FTokens[Result] = AToken) then
      Exit;

    Inc(LSlot);
    if LSlot = Length(FSlots) then
      LSlot := 0;
  end;
  Result := -1;
end;

function TWfcTokenLookup.TokenAt(const AIndex: Integer): TWfcModelToken;
begin
  if (AIndex < 0) or (AIndex >= Length(FTokens)) then
    raise EWfcTokenLookup.Create('token index is out of range: ' +
      IntToStr(AIndex));
  Result := FTokens[AIndex];
end;

end.
