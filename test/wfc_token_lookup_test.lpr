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
program wfc_token_lookup_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc_model,
  wfc_token_lookup;

type
  TTestProcedure = procedure;

const
  HIGH_CARDINALITY_COUNT = 65536;
  COLLISION_TOKEN_A = 'c<1pk,+<c';
  COLLISION_TOKEN_B = 'Q,whq>7vA';

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GCheckCount);
  if ACondition then
    WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailureCount);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try
    ATest;
  except
    on E: Exception do
    begin
      Inc(GFailureCount);
      WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function TokensOf(const AValues: array of TWfcModelToken):
  TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function UnicodeToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($03BB) + Chr($266B) + Chr($6F22));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(UnicodeString(WideChar($03BB)) +
    UnicodeString(WideChar($266B)) +
    UnicodeString(WideChar($6F22))));
  {$ENDIF}
end;

function PrecomposedToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($0101));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(
    UnicodeString(WideChar($0101))));
  {$ENDIF}
end;

function DecomposedToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken('a' + Chr($0304));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(UnicodeString('a') +
    UnicodeString(WideChar($0304))));
  {$ENDIF}
end;

function IndexedToken(const AIndex: Integer): TWfcModelToken;
begin
  Result := TWfcModelToken('token-' + IntToStr(AIndex));
end;

procedure TestEmptyAndOrdinaryLookup;
var
  LLookup: TWfcTokenLookup;
  LTokens: TWfcModelTokens;
begin
  LLookup := TWfcTokenLookup.Create(nil);
  try
    Check(LLookup.Count = 0, 'an empty lookup has count zero');
    Check(LLookup.Find('anything') = -1,
      'an empty lookup reports absence');
  finally
    LLookup.Free;
  end;

  LTokens := TokensOf(['alpha', '', 'omega']);
  LLookup := TWfcTokenLookup.Create(LTokens);
  try
    Check(LLookup.Count = 3, 'count retains every input token');
    Check(LLookup.Find('alpha') = 0,
      'the first token maps to its original index');
    Check(LLookup.Find('') = 1,
      'an empty token has ordinary exact-token semantics');
    Check(LLookup.Find('omega') = 2,
      'the last token maps to its original index');
    Check(LLookup.Find('missing') = -1,
      'an absent token returns the documented sentinel');
    Check(LLookup.TokenAt(1) = '',
      'TokenAt returns the exact stored token');
  finally
    LLookup.Free;
  end;
end;

procedure TestCollisionEquality;
var
  LLookup: TWfcTokenLookup;
  LTokens: TWfcModelTokens;
begin
  { These distinct ASCII strings have the same complete 32-bit token hash.
    Both must remain addressable after the explicit equality check. }
  LTokens := TokensOf([COLLISION_TOKEN_A, COLLISION_TOKEN_B, 'tail']);
  LLookup := TWfcTokenLookup.Create(LTokens);
  try
    Check(LLookup.Find(COLLISION_TOKEN_A) = 0,
      'the first full-hash collision retains its index');
    Check(LLookup.Find(COLLISION_TOKEN_B) = 1,
      'a distinct full-hash collision probes past unequal text');
    Check(LLookup.Find('tail') = 2,
      'a later table entry remains addressable');
  finally
    LLookup.Free;
  end;
end;

procedure TestDuplicateRejection;
var
  LLookup: TWfcTokenLookup;
  LRaised: Boolean;
  LTokens: TWfcModelTokens;
begin
  LLookup := nil;
  LTokens := TokensOf([UnicodeToken, 'middle', UnicodeToken]);
  LRaised := False;
  try
    LLookup := TWfcTokenLookup.Create(LTokens);
  except
    on E: EWfcTokenLookup do
    begin
      LRaised := (Pos('index 2', E.Message) > 0) and
        (Pos('index 0', E.Message) > 0);
    end;
  end;
  LLookup.Free;
  Check(LRaised,
    'duplicate Unicode tokens report both deterministic source indices');
end;

procedure TestUnicodeIdentity;
var
  LLookup: TWfcTokenLookup;
  LTokens: TWfcModelTokens;
begin
  LTokens := TokensOf([UnicodeToken, PrecomposedToken, DecomposedToken]);
  LLookup := TWfcTokenLookup.Create(LTokens);
  try
    Check(LLookup.Find(UnicodeToken) = 0,
      'multi-script Unicode text round-trips through the lookup');
    Check(LLookup.Find(PrecomposedToken) = 1,
      'precomposed Unicode text retains its identity');
    Check(LLookup.Find(DecomposedToken) = 2,
      'decomposed Unicode text remains a distinct exact token');
  finally
    LLookup.Free;
  end;
end;

procedure TestCallerMutationIsolation;
var
  LLookup: TWfcTokenLookup;
  LReturned: TWfcModelToken;
  LTokens: TWfcModelTokens;
begin
  LTokens := TokensOf(['alpha', UnicodeToken]);
  LLookup := TWfcTokenLookup.Create(LTokens);
  try
    LTokens[0][1] := 'X';
    LTokens[1] := 'caller-replacement';
    Check(LLookup.Find('alpha') = 0,
      'caller mutation cannot alter the detached token array');
    Check(LLookup.Find(UnicodeToken) = 1,
      'caller element replacement cannot alter stored Unicode text');
    Check(LLookup.Find(LTokens[0]) = -1,
      'caller-mutated text is not introduced into the lookup');

    LReturned := LLookup.TokenAt(0);
    LReturned[1] := 'Y';
    Check(LLookup.TokenAt(0) = 'alpha',
      'mutating a returned token cannot alter lookup storage');
  finally
    LLookup.Free;
  end;
end;

procedure TestTokenAtBounds;
var
  LLookup: TWfcTokenLookup;
  LRaisedHigh: Boolean;
  LRaisedLow: Boolean;
begin
  LLookup := TWfcTokenLookup.Create(TokensOf(['only']));
  try
    LRaisedLow := False;
    try
      LLookup.TokenAt(-1);
    except
      on EWfcTokenLookup do
        LRaisedLow := True;
    end;
    Check(LRaisedLow, 'TokenAt rejects a negative index');

    LRaisedHigh := False;
    try
      LLookup.TokenAt(1);
    except
      on EWfcTokenLookup do
        LRaisedHigh := True;
    end;
    Check(LRaisedHigh, 'TokenAt rejects a past-end index');
  finally
    LLookup.Free;
  end;
end;

procedure TestHighCardinality;
var
  I: Integer;
  LLookup: TWfcTokenLookup;
  LTokens: TWfcModelTokens;
begin
  SetLength(LTokens, HIGH_CARDINALITY_COUNT);
  for I := 0 to Length(LTokens) - 1 do
    LTokens[I] := IndexedToken(I);

  LLookup := TWfcTokenLookup.Create(LTokens);
  try
    Check(LLookup.Count = HIGH_CARDINALITY_COUNT,
      'high-cardinality construction retains the complete vocabulary');
    for I := 0 to Length(LTokens) - 1 do
      if LLookup.Find(IndexedToken(I)) <> I then
      begin
        Check(False, 'every high-cardinality token retains its index');
        Exit;
      end;
    Check(True, 'every high-cardinality token retains its index');
    Check(LLookup.Find(IndexedToken(HIGH_CARDINALITY_COUNT)) = -1,
      'a neighboring high-cardinality token remains absent');
  finally
    LLookup.Free;
  end;
end;

begin
  RunTest('empty and ordinary lookup', TestEmptyAndOrdinaryLookup);
  RunTest('collision equality', TestCollisionEquality);
  RunTest('duplicate rejection', TestDuplicateRejection);
  RunTest('Unicode identity', TestUnicodeIdentity);
  RunTest('caller mutation isolation', TestCallerMutationIsolation);
  RunTest('TokenAt bounds', TestTokenAtBounds);
  RunTest('high cardinality', TestHighCardinality);
  WriteLn('Checks: ', GCheckCount, ', Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d token-lookup checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
