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
unit wfc_package_check_app;

{$mode delphi}{$H+}

interface

uses SysUtils;

const
  WFC_PACKAGE_CHECK_VERSION = 1;
  WFC_PACKAGE_CHECK_MAX_UNITS = 4096;
  WFC_PACKAGE_CHECK_MAX_TEXT_LENGTH = 1048576;
  WFC_PACKAGE_CHECK_MAX_TOKENS = 131072;
  WFC_PACKAGE_CHECK_MAX_DIAGNOSTIC_LENGTH = 512;
  WFC_PACKAGE_CHECK_MAX_NAME_LENGTH = 128;

type
  EWfcPackageCheck = class(Exception);
  TWfcPackageSourceUnits = array of String;
  TWfcPackageCheckResult = record
    Passed: Boolean;
    SourceUnitCount: Integer;
    FpmUnitCount: Integer;
    LazarusUnitCount: Integer;
    PackageUnitCount: Integer;
    Diagnostic: String;
  end;

{ Source names are canonical lowercase Pascal basenames without extension or
  path. The host owns enumeration and filename/declaration comparison. This
  checks static manifest declarations, not execution of arbitrary Pascal. }
function CheckWfcPackageManifests(const ASourceUnits: TWfcPackageSourceUnits;
  const AFpmake, ALazarusXml, APackagePascal: String): TWfcPackageCheckResult;

{ Reads only the initial unit declaration, after bounded comments/directives
  and an optional UTF-8 BOM. No unit body is tokenized or executed. }
function WfcPackageDeclaredUnitName(const ASourceText: String): String;

implementation

const
  MAX_DEPTH = 32;
  MAX_XML_ATTRIBUTES = 32;

type
  TTokenKind = (tkEnd, tkIdentifier, tkString, tkSymbol);
  TToken = record
    Kind: TTokenKind;
    Text: String;
    Offset: Integer;
    ConditionalDepth: Integer;
  end;
  TTokens = array of TToken;
  TPascalLexer = record
    Source: String;
    LabelText: String;
    Position: Integer;
    TokenCount: Integer;
    ConditionalDepth: Integer;
  end;
  TSeen = array of Boolean;
  TXmlAttribute = record Name, Value: String; end;
  TXmlAttributes = array of TXmlAttribute;
  TXmlTag = record
    Name: String;
    Attributes: TXmlAttributes;
    IsEnd, IsEmpty: Boolean;
  end;
  TXmlReader = record
    Source: String;
    Position, TokenCount: Integer;
    DeclarationSeen, ElementSeen: Boolean;
  end;

function PrintableDiagnostic(const S: String): String;
var I: Integer; Piece: String;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    if Ord(S[I]) in [32..126] then Piece := S[I]
    else Piece := '\x' + IntToHex(Ord(S[I]), 2);
    if Length(Result) + Length(Piece) > WFC_PACKAGE_CHECK_MAX_DIAGNOSTIC_LENGTH - 3 then
    begin Result := Result + '...'; Exit; end;
    Result := Result + Piece;
  end;
end;

procedure Fail(const AMessage: String);
begin
  raise EWfcPackageCheck.Create(PrintableDiagnostic(AMessage));
end;

function InitialPosition(const S: String; const ALabel: String): Integer;
begin
  if Length(S) > WFC_PACKAGE_CHECK_MAX_TEXT_LENGTH then
    Fail(ALabel + ': text exceeds the length limit');
  Result := 1;
  if (Length(S) >= 3) and (Ord(S[1]) = $EF) and
      (Ord(S[2]) = $BB) and (Ord(S[3]) = $BF) then Result := 4;
  {$IFDEF PAS2JS}
  if (Length(S) >= 1) and (Ord(S[1]) = $FEFF) then Result := 2;
  {$ENDIF}
end;

function IdentifierStart(const C: Char): Boolean;
begin Result := C in ['a'..'z', 'A'..'Z', '_']; end;

function IdentifierPart(const C: Char): Boolean;
begin Result := IdentifierStart(C) or (C in ['0'..'9']); end;

function IsUnitName(const S: String): Boolean;
var I: Integer;
begin
  Result := False;
  if (Length(S) = 0) or (Length(S) > WFC_PACKAGE_CHECK_MAX_NAME_LENGTH) then Exit;
  if not (S[1] in ['a'..'z', '_']) then Exit;
  for I := 2 to Length(S) do
    if not (S[I] in ['a'..'z', '0'..'9', '_']) then Exit;
  Result := True;
end;

procedure InitializeLexer(out L: TPascalLexer; const S, ALabel: String);
begin
  L := Default(TPascalLexer);
  L.Position := InitialPosition(S, ALabel);
  L.Source := S;
  L.LabelText := ALabel;
end;

procedure LexerFail(const L: TPascalLexer; const MessageText: String);
begin Fail(L.LabelText + ': ' + MessageText + ' at offset ' + IntToStr(L.Position - 1)); end;

procedure Directive(var L: TPascalLexer; const Text: String);
var I: Integer; WordText: String;
begin
  I := 1;
  while (I <= Length(Text)) and (Text[I] <= ' ') do Inc(I);
  WordText := '';
  while (I <= Length(Text)) and IdentifierPart(Text[I]) do
  begin WordText := WordText + Text[I]; Inc(I); end;
  WordText := LowerCase(WordText);
  if (WordText = 'include') or ((WordText = 'i') and
      ((I > Length(Text)) or not (Text[I] in ['+', '-']))) then
    LexerFail(L, 'include directives are outside the static manifest profile');
  if (WordText = 'ifdef') or (WordText = 'ifndef') or
      (WordText = 'if') or (WordText = 'ifopt') then
  begin
    if L.ConditionalDepth = MAX_DEPTH then LexerFail(L, 'conditional nesting exceeds the limit');
    Inc(L.ConditionalDepth);
  end
  else if (WordText = 'endif') or (WordText = 'ifend') then
  begin
    if L.ConditionalDepth = 0 then LexerFail(L, 'unmatched conditional end');
    Dec(L.ConditionalDepth);
  end
  else if (WordText = 'else') or (WordText = 'elseif') then
    if L.ConditionalDepth = 0 then LexerFail(L, 'unmatched conditional branch');
end;

procedure BlockComment(var L: TPascalLexer; const Brace: Boolean;
  const Depth: Integer);
var Start: Integer; IsDirective: Boolean;
begin
  if Depth > MAX_DEPTH then LexerFail(L, 'comment nesting exceeds the limit');
  if Brace then Inc(L.Position) else Inc(L.Position, 2);
  { Directive-shaped text nested inside an ordinary comment is only text. }
  IsDirective := (Depth = 1) and (L.Position <= Length(L.Source)) and
    (L.Source[L.Position] = '$');
  if IsDirective then Inc(L.Position);
  Start := L.Position;
  while L.Position <= Length(L.Source) do
  begin
    if (Brace and (L.Source[L.Position] = '}')) or
        ((not Brace) and (Copy(L.Source, L.Position, 2) = '*)')) then
    begin
      if IsDirective then Directive(L, Copy(L.Source, Start, L.Position - Start));
      if Brace then Inc(L.Position) else Inc(L.Position, 2);
      Exit;
    end;
    if not IsDirective then
    begin
      if L.Source[L.Position] = '{' then
      begin BlockComment(L, True, Depth + 1); Continue; end;
      if Copy(L.Source, L.Position, 2) = '(*' then
      begin BlockComment(L, False, Depth + 1); Continue; end;
    end;
    Inc(L.Position);
  end;
  LexerFail(L, 'unterminated block comment');
end;

function NextToken(var L: TPascalLexer): TToken;
var Start: Integer; Closed: Boolean;
begin
  Result := Default(TToken);
  while L.Position <= Length(L.Source) do
  begin
    if L.Source[L.Position] in [#9, #10, #13, ' '] then
    begin Inc(L.Position); Continue; end;
    if L.Source[L.Position] = '{' then
    begin BlockComment(L, True, 1); Continue; end;
    if Copy(L.Source, L.Position, 2) = '(*' then
    begin BlockComment(L, False, 1); Continue; end;
    if Copy(L.Source, L.Position, 2) = '//' then
    begin
      Inc(L.Position, 2);
      while (L.Position <= Length(L.Source)) and
          not (L.Source[L.Position] in [#10, #13]) do Inc(L.Position);
      Continue;
    end;
    Break;
  end;
  Result.Offset := L.Position - 1;
  Result.ConditionalDepth := L.ConditionalDepth;
  if L.Position > Length(L.Source) then
  begin
    if L.ConditionalDepth <> 0 then LexerFail(L, 'unterminated conditional directive');
    Result.Kind := tkEnd; Exit;
  end;
  if L.TokenCount = WFC_PACKAGE_CHECK_MAX_TOKENS then LexerFail(L, 'token count exceeds the limit');
  Inc(L.TokenCount);
  Start := L.Position;
  if IdentifierStart(L.Source[L.Position]) then
  begin
    Inc(L.Position);
    while (L.Position <= Length(L.Source)) and IdentifierPart(L.Source[L.Position]) do Inc(L.Position);
    Result.Kind := tkIdentifier;
    Result.Text := LowerCase(Copy(L.Source, Start, L.Position - Start));
  end
  else if L.Source[L.Position] = '''' then
  begin
    Result.Kind := tkString; Closed := False; Inc(L.Position);
    Start := L.Position;
    while L.Position <= Length(L.Source) do
    begin
      if L.Source[L.Position] in [#0..#31] then LexerFail(L, 'control character inside Pascal literal');
      if L.Source[L.Position] = '''' then
      begin
        Result.Text := Result.Text + Copy(L.Source, Start, L.Position - Start);
        Inc(L.Position);
        if (L.Position <= Length(L.Source)) and (L.Source[L.Position] = '''') then
        begin
          Result.Text := Result.Text + ''''; Inc(L.Position); Start := L.Position;
        end
        else begin Closed := True; Break; end;
      end
      else Inc(L.Position);
    end;
    if not Closed then LexerFail(L, 'unterminated Pascal literal');
  end
  else
  begin
    if Ord(L.Source[L.Position]) < 32 then LexerFail(L, 'invalid control character');
    Result.Kind := tkSymbol; Result.Text := L.Source[L.Position]; Inc(L.Position);
  end;
end;

function PascalTokens(const S, ALabel: String): TTokens;
var L: TPascalLexer; T: TToken; Count, Capacity: Integer;
begin
  Result := nil; InitializeLexer(L, S, ALabel); Count := 0; Capacity := 0;
  repeat
    T := NextToken(L);
    if Count = Capacity then
    begin
      if Capacity = 0 then Capacity := 128 else Capacity := Capacity * 2;
      if Capacity > WFC_PACKAGE_CHECK_MAX_TOKENS + 1 then
        Capacity := WFC_PACKAGE_CHECK_MAX_TOKENS + 1;
      SetLength(Result, Capacity);
    end;
    Result[Count] := T; Inc(Count);
  until T.Kind = tkEnd;
  SetLength(Result, Count);
end;

function WfcPackageDeclaredUnitName(const ASourceText: String): String;
var L: TPascalLexer; T: TToken;
begin
  InitializeLexer(L, ASourceText, 'source unit');
  T := NextToken(L);
  if (T.Kind <> tkIdentifier) or (T.Text <> 'unit') or (T.ConditionalDepth <> 0) then
    Fail('source unit: expected unconditional initial unit declaration');
  T := NextToken(L);
  if (T.Kind <> tkIdentifier) or not IsUnitName(T.Text) or (T.ConditionalDepth <> 0) then
    Fail('source unit: expected a simple unit name');
  Result := T.Text;
  T := NextToken(L);
  if (T.Kind <> tkSymbol) or (T.Text <> ';') or (T.ConditionalDepth <> 0) then
    Fail('source unit: expected semicolon after unit name');
end;

function UnitIndex(const Units: TWfcPackageSourceUnits; const Name: String): Integer;
var I: Integer;
begin
  for I := 0 to High(Units) do if Units[I] = Name then Exit(I);
  Result := -1;
end;

procedure MarkUnit(const Units: TWfcPackageSourceUnits; var Seen: TSeen;
  const Name, ALabel: String; var Count: Integer);
var I: Integer;
begin
  if not IsUnitName(Name) then Fail(ALabel + ': noncanonical unit name ' + Name);
  I := UnitIndex(Units, Name);
  if I < 0 then Fail(ALabel + ': unknown source unit ' + Name);
  if Seen[I] then Fail(ALabel + ': duplicate unit ' + Name);
  Seen[I] := True; Inc(Count);
end;

procedure RequireComplete(const Units: TWfcPackageSourceUnits;
  const Seen: TSeen; const ALabel: String);
var I: Integer;
begin
  for I := 0 to High(Units) do
    if not Seen[I] then Fail(ALabel + ': missing unit ' + Units[I]);
end;

function TokenIs(const T: TTokens; const I: Integer;
  const Kind: TTokenKind; const Text: String): Boolean;
begin
  Result := (I >= 0) and (I < Length(T)) and (T[I].Kind = Kind) and (T[I].Text = Text);
end;

procedure CheckFpm(const Units: TWfcPackageSourceUnits; const S: String;
  out Count: Integer);
var T: TTokens; Seen: TSeen; I, J, SourcePaths: Integer; FileName, Name: String;
begin
  Count := 0; SourcePaths := 0;
  SetLength(Seen, Length(Units)); T := PascalTokens(S, 'fpmake.pp');
  if not TokenIs(T, 0, tkIdentifier, 'program') or (Length(T) < 6) or
      (T[1].Kind <> tkIdentifier) or not TokenIs(T, 2, tkSymbol, ';') then
    Fail('fpmake.pp: expected a Pascal program declaration');
  if not TokenIs(T, Length(T) - 3, tkIdentifier, 'end') or
      not TokenIs(T, Length(T) - 2, tkSymbol, '.') then
    Fail('fpmake.pp: expected final end.');
  for I := 0 to High(T) do
  begin
    if TokenIs(T, I, tkIdentifier, 'addunit') then
    begin
      if not (TokenIs(T, I - 4, tkIdentifier, 'p') and
          TokenIs(T, I - 3, tkSymbol, '.') and TokenIs(T, I - 2, tkIdentifier, 'targets') and
          TokenIs(T, I - 1, tkSymbol, '.') and TokenIs(T, I + 1, tkSymbol, '(') and
          (I + 4 < Length(T)) and (T[I + 2].Kind = tkString) and
          TokenIs(T, I + 3, tkSymbol, ')') and TokenIs(T, I + 4, tkSymbol, ';')) then
        Fail('fpmake.pp: AddUnit requires P.Targets.AddUnit(''canonical.pas'');');
      if TokenIs(T, I - 5, tkSymbol, '.') then
        Fail('fpmake.pp: AddUnit owner must be unqualified P');
      for J := I - 4 to I + 4 do
        if T[J].ConditionalDepth <> 0 then Fail('fpmake.pp: conditional AddUnit declaration is unsupported');
      FileName := T[I + 2].Text;
      if (Length(FileName) <= 4) or (Copy(FileName, Length(FileName) - 3, 4) <> '.pas') then
        Fail('fpmake.pp: noncanonical AddUnit filename ' + FileName);
      Name := Copy(FileName, 1, Length(FileName) - 4);
      MarkUnit(Units, Seen, Name, 'fpmake.pp', Count);
    end;
    if TokenIs(T, I, tkIdentifier, 'sourcepath') then
    begin
      if not (TokenIs(T, I - 2, tkIdentifier, 'p') and TokenIs(T, I - 1, tkSymbol, '.') and
          TokenIs(T, I + 1, tkSymbol, '.') and TokenIs(T, I + 2, tkIdentifier, 'add') and
          TokenIs(T, I + 3, tkSymbol, '(') and TokenIs(T, I + 4, tkString, 'src') and
          TokenIs(T, I + 5, tkSymbol, ')') and TokenIs(T, I + 6, tkSymbol, ';')) then
        Fail('fpmake.pp: source path must be literal P.SourcePath.Add(''src'');');
      if TokenIs(T, I - 3, tkSymbol, '.') then
        Fail('fpmake.pp: source path owner must be unqualified P');
      for J := I - 2 to I + 6 do
        if T[J].ConditionalDepth <> 0 then Fail('fpmake.pp: conditional source path is unsupported');
      Inc(SourcePaths);
    end;
  end;
  if SourcePaths <> 1 then Fail('fpmake.pp: expected exactly one source path declaration');
  RequireComplete(Units, Seen, 'fpmake.pp');
end;

procedure CheckPackageUnit(const Units, LazarusOrder: TWfcPackageSourceUnits; const S: String;
  out Count: Integer);
var T: TTokens; Seen: TSeen; I: Integer;

  procedure Expect(const Kind: TTokenKind; const Text: String);
  begin
    if not TokenIs(T, I, Kind, Text) then Fail('wfc_package.pas: expected ' + Text);
    if T[I].ConditionalDepth <> 0 then Fail('wfc_package.pas: conditional interface declaration is unsupported');
    Inc(I);
  end;

begin
  Count := 0; SetLength(Seen, Length(Units));
  T := PascalTokens(S, 'wfc_package.pas'); I := 0;
  Expect(tkIdentifier, 'unit'); Expect(tkIdentifier, 'wfc_package'); Expect(tkSymbol, ';');
  Expect(tkIdentifier, 'interface'); Expect(tkIdentifier, 'uses');
  repeat
    if (I >= Length(T)) or (T[I].Kind <> tkIdentifier) then
      Fail('wfc_package.pas: expected a unit identifier in interface uses');
    if T[I].ConditionalDepth <> 0 then Fail('wfc_package.pas: conditional interface use is unsupported');
    MarkUnit(Units, Seen, T[I].Text, 'wfc_package.pas', Count);
    if T[I].Text <> LazarusOrder[Count - 1] then
      Fail('wfc_package.pas: interface uses order differs from wfc.lpk source item order');
    Inc(I);
    if TokenIs(T, I, tkSymbol, ';') then Break;
    Expect(tkSymbol, ',');
  until False;
  Expect(tkSymbol, ';'); Expect(tkIdentifier, 'implementation');
  if (Length(T) < 3) or not TokenIs(T, Length(T) - 3, tkIdentifier, 'end') or
      not TokenIs(T, Length(T) - 2, tkSymbol, '.') then
    Fail('wfc_package.pas: expected final end.');
  RequireComplete(Units, Seen, 'wfc_package.pas');
end;

procedure XmlFail(const R: TXmlReader; const MessageText: String);
begin Fail('wfc.lpk: ' + MessageText + ' at offset ' + IntToStr(R.Position - 1)); end;

procedure XmlSpace(var R: TXmlReader);
begin
  while (R.Position <= Length(R.Source)) and
      (R.Source[R.Position] in [#9, #10, #13, ' ']) do Inc(R.Position);
end;

function XmlName(var R: TXmlReader): String;
var Start: Integer;
begin
  Start := R.Position;
  if (R.Position > Length(R.Source)) or not IdentifierStart(R.Source[R.Position]) then
    XmlFail(R, 'expected an unqualified XML name');
  Inc(R.Position);
  while (R.Position <= Length(R.Source)) and
      (IdentifierPart(R.Source[R.Position]) or (R.Source[R.Position] in ['-', '.'])) do
    Inc(R.Position);
  if R.Position - Start > WFC_PACKAGE_CHECK_MAX_NAME_LENGTH then
    XmlFail(R, 'XML name exceeds the limit');
  Result := Copy(R.Source, Start, R.Position - Start);
end;

function XmlValue(var R: TXmlReader): String;
var Quote: Char; Start, EntityStart: Integer; Entity: String;
begin
  Result := '';
  if (R.Position > Length(R.Source)) or not (R.Source[R.Position] in ['"', '''']) then
    XmlFail(R, 'expected a quoted XML attribute');
  Quote := R.Source[R.Position]; Inc(R.Position); Start := R.Position;
  while R.Position <= Length(R.Source) do
  begin
    if R.Source[R.Position] = Quote then
    begin
      Result := Result + Copy(R.Source, Start, R.Position - Start);
      Inc(R.Position); Exit;
    end;
    if (R.Source[R.Position] = '<') or (Ord(R.Source[R.Position]) < 32) then
      XmlFail(R, 'invalid XML attribute character');
    if R.Source[R.Position] = '&' then
    begin
      Result := Result + Copy(R.Source, Start, R.Position - Start);
      EntityStart := R.Position; Inc(R.Position);
      while (R.Position <= Length(R.Source)) and (R.Source[R.Position] <> ';') and
          (R.Position - EntityStart <= 6) do Inc(R.Position);
      if (R.Position > Length(R.Source)) or (R.Source[R.Position] <> ';') then
        XmlFail(R, 'unsupported XML entity');
      Entity := Copy(R.Source, EntityStart, R.Position - EntityStart + 1);
      if Entity = '&amp;' then Result := Result + '&'
      else if Entity = '&lt;' then Result := Result + '<'
      else if Entity = '&gt;' then Result := Result + '>'
      else if Entity = '&quot;' then Result := Result + '"'
      else if Entity = '&apos;' then Result := Result + ''''
      else XmlFail(R, 'only the five predefined XML entities are supported');
      Inc(R.Position); Start := R.Position;
    end
    else Inc(R.Position);
  end;
  XmlFail(R, 'unterminated XML attribute');
end;

function AttributeIndex(const T: TXmlTag; const Name: String): Integer;
var I: Integer;
begin
  for I := 0 to High(T.Attributes) do if T.Attributes[I].Name = Name then Exit(I);
  Result := -1;
end;

procedure XmlAttributes(var R: TXmlReader; var T: TXmlTag;
  const Declaration: Boolean);
var Start, N: Integer; A: TXmlAttribute;
begin
  while R.Position <= Length(R.Source) do
  begin
    Start := R.Position; XmlSpace(R);
    if Declaration then
    begin
      if Copy(R.Source, R.Position, 2) = '?>' then
      begin Inc(R.Position, 2); Exit; end;
    end
    else
    begin
      if Copy(R.Source, R.Position, 2) = '/>' then
      begin T.IsEmpty := True; Inc(R.Position, 2); Exit; end;
      if (R.Position <= Length(R.Source)) and (R.Source[R.Position] = '>') then
      begin Inc(R.Position); Exit; end;
    end;
    if Start = R.Position then XmlFail(R, 'XML attributes must be separated by whitespace');
    if Length(T.Attributes) = MAX_XML_ATTRIBUTES then XmlFail(R, 'attribute count exceeds the limit');
    A.Name := XmlName(R);
    if AttributeIndex(T, A.Name) >= 0 then XmlFail(R, 'duplicate XML attribute ' + A.Name);
    XmlSpace(R);
    if (R.Position > Length(R.Source)) or (R.Source[R.Position] <> '=') then
      XmlFail(R, 'expected equals after XML attribute name');
    Inc(R.Position); XmlSpace(R); A.Value := XmlValue(R);
    N := Length(T.Attributes); SetLength(T.Attributes, N + 1); T.Attributes[N] := A;
  end;
  XmlFail(R, 'unterminated XML tag');
end;

function NextXmlTag(var R: TXmlReader; out T: TXmlTag): Boolean;
var I: Integer; D: TXmlTag;
begin
  T := Default(TXmlTag);
  while True do
  begin
    XmlSpace(R);
    if R.Position > Length(R.Source) then Exit(False);
    if R.Source[R.Position] <> '<' then XmlFail(R, 'only whitespace text is supported');
    if R.TokenCount = WFC_PACKAGE_CHECK_MAX_TOKENS then XmlFail(R, 'XML tag count exceeds the limit');
    Inc(R.TokenCount);
    if Copy(R.Source, R.Position, 4) = '<!--' then
    begin
      Inc(R.Position, 4);
      while (R.Position <= Length(R.Source)) and
          (Copy(R.Source, R.Position, 3) <> '-->') do
      begin
        if Copy(R.Source, R.Position, 2) = '--' then XmlFail(R, 'double hyphen inside XML comment');
        if (Ord(R.Source[R.Position]) < 32) and
            not (R.Source[R.Position] in [#9, #10, #13]) then XmlFail(R, 'invalid XML comment character');
        Inc(R.Position);
      end;
      if R.Position > Length(R.Source) then XmlFail(R, 'unterminated XML comment');
      Inc(R.Position, 3); Continue;
    end;
    if Copy(R.Source, R.Position, 2) = '<?' then
    begin
      if R.DeclarationSeen or R.ElementSeen then XmlFail(R, 'XML declaration must be first and unique');
      Inc(R.Position, 2); D := Default(TXmlTag); D.Name := XmlName(R);
      if D.Name <> 'xml' then XmlFail(R, 'processing instructions are unsupported');
      XmlAttributes(R, D, True);
      I := AttributeIndex(D, 'version');
      if (I <> 0) or (D.Attributes[I].Value <> '1.0') then XmlFail(R, 'XML version must be 1.0');
      for I := 1 to High(D.Attributes) do
        if D.Attributes[I].Name = 'encoding' then
        begin
          if D.Attributes[I].Value <> 'UTF-8' then XmlFail(R, 'XML encoding must be UTF-8');
        end
        else if D.Attributes[I].Name = 'standalone' then
        begin
          if (D.Attributes[I].Value <> 'yes') and (D.Attributes[I].Value <> 'no') then
            XmlFail(R, 'invalid XML standalone value');
        end
        else XmlFail(R, 'unsupported XML declaration attribute');
      R.DeclarationSeen := True; Continue;
    end;
    Break;
  end;
  R.ElementSeen := True; Inc(R.Position);
  if (R.Position <= Length(R.Source)) and (R.Source[R.Position] = '/') then
  begin T.IsEnd := True; Inc(R.Position); end;
  T.Name := XmlName(R);
  if T.IsEnd then
  begin
    XmlSpace(R);
    if (R.Position > Length(R.Source)) or (R.Source[R.Position] <> '>') then
      XmlFail(R, 'malformed XML end tag');
    Inc(R.Position);
  end
  else XmlAttributes(R, T, False);
  Result := True;
end;

function SingleAttribute(const T: TXmlTag; const Name: String): String;
begin
  if (Length(T.Attributes) <> 1) or (T.Attributes[0].Name <> Name) then
    Fail('wfc.lpk: ' + T.Name + ' requires exactly the ' + Name + ' attribute');
  Result := T.Attributes[0].Value;
end;

function CanonicalCount(const S, ALabel: String; const Maximum: Integer): Integer;
var I, Digit: Integer;
begin
  Result := 0;
  if (S = '') or ((Length(S) > 1) and (S[1] = '0')) then
    Fail('wfc.lpk: noncanonical ' + ALabel);
  for I := 1 to Length(S) do
  begin
    if not (S[I] in ['0'..'9']) then Fail('wfc.lpk: noncanonical ' + ALabel);
    Digit := Ord(S[I]) - Ord('0');
    if Result > (Maximum - Digit) div 10 then Fail('wfc.lpk: ' + ALabel + ' exceeds the limit');
    Result := Result * 10 + Digit;
    if Result > Maximum then Fail('wfc.lpk: ' + ALabel + ' exceeds the limit');
  end;
end;

procedure CheckLazarus(const Units: TWfcPackageSourceUnits; const S: String;
  out Count: Integer; out SourceOrder: TWfcPackageSourceUnits);
var
  R: TXmlReader; T: TXmlTag; Stack: array[0..MAX_DEPTH - 1] of String;
  Seen: TSeen; Depth, RootCount, PackageCount, FilesCount, ExpectedItems,
    ItemCount, MainCount, I: Integer;
  ItemFile, ItemUnit, ItemType, Name, Text: String;
  HasFile, HasUnit, HasType: Boolean;

  procedure FinishItem;
  begin
    if not HasFile or not HasUnit then Fail('wfc.lpk: item requires Filename and UnitName');
    if ItemFile = 'wfc_package.pas' then
    begin
      if (ItemUnit <> 'wfc_package') or not HasType or (ItemType <> 'Main Unit') then
        Fail('wfc.lpk: main unit row does not match wfc_package');
      Inc(MainCount);
      if MainCount > 1 then Fail('wfc.lpk: duplicate main unit row');
    end
    else
    begin
      if (Copy(ItemFile, 1, 4) <> 'src/') or (Length(ItemFile) <= 8) or
          (Copy(ItemFile, Length(ItemFile) - 3, 4) <> '.pas') then
        Fail('wfc.lpk: noncanonical source filename ' + ItemFile);
      Name := Copy(ItemFile, 5, Length(ItemFile) - 8);
      if ItemUnit <> Name then Fail('wfc.lpk: filename and UnitName mismatch');
      if HasType and (ItemType <> 'Unit') then Fail('wfc.lpk: source row has unsupported Type');
      MarkUnit(Units, Seen, Name, 'wfc.lpk', Count);
      SourceOrder[Count - 1] := Name;
    end;
  end;

begin
  Count := 0; Depth := 0; RootCount := 0; PackageCount := 0; FilesCount := 0;
  ExpectedItems := 0; ItemCount := 0; MainCount := 0;
  HasFile := False; HasUnit := False; HasType := False;
  for I := 0 to High(Stack) do Stack[I] := '';
  SetLength(Seen, Length(Units));
  SourceOrder := nil; SetLength(SourceOrder, Length(Units)); R := Default(TXmlReader);
  R.Source := S; R.Position := InitialPosition(S, 'wfc.lpk');
  while NextXmlTag(R, T) do
  begin
    if T.IsEnd then
    begin
      if (Depth = 0) or (Stack[Depth - 1] <> T.Name) then XmlFail(R, 'mismatched XML end tag');
      if (Depth = 4) and (Stack[2] = 'Files') then FinishItem;
      if (Depth = 3) and (T.Name = 'Files') and (ItemCount <> ExpectedItems) then
        Fail('wfc.lpk: Files Count does not match contiguous item declarations');
      Dec(Depth); Continue;
    end;
    if Depth = MAX_DEPTH then XmlFail(R, 'XML nesting exceeds the limit');
    if Depth = 0 then
    begin
      Inc(RootCount);
      if (RootCount <> 1) or (T.Name <> 'CONFIG') or T.IsEmpty then
        Fail('wfc.lpk: expected exactly one CONFIG root');
    end
    else if T.Name = 'CONFIG' then Fail('wfc.lpk: nested CONFIG is unsupported');
    if T.Name = 'Package' then
    begin
      if (Depth <> 1) or (Stack[0] <> 'CONFIG') or T.IsEmpty then
        Fail('wfc.lpk: Package must be a nonempty direct child of CONFIG');
      Inc(PackageCount);
      if PackageCount <> 1 then Fail('wfc.lpk: duplicate Package');
    end;
    if T.Name = 'Files' then
    begin
      if (Depth <> 2) or (Stack[0] <> 'CONFIG') or (Stack[1] <> 'Package') or T.IsEmpty then
        Fail('wfc.lpk: Files must be a nonempty direct child of Package');
      Inc(FilesCount);
      if FilesCount <> 1 then Fail('wfc.lpk: duplicate Files');
      ExpectedItems := CanonicalCount(SingleAttribute(T, 'Count'), 'Files Count',
        WFC_PACKAGE_CHECK_MAX_UNITS + 1);
      if ExpectedItems <> Length(Units) + 1 then Fail('wfc.lpk: Files Count differs from source inventory plus main unit');
    end
    else if (Depth = 3) and (Stack[2] = 'Files') then
    begin
      Inc(ItemCount);
      if (ItemCount > ExpectedItems) or (T.Name <> 'Item' + IntToStr(ItemCount)) or
          T.IsEmpty or (Length(T.Attributes) <> 0) then
        Fail('wfc.lpk: expected contiguous nonempty Item' + IntToStr(ItemCount));
      HasFile := False; HasUnit := False; HasType := False;
      ItemFile := ''; ItemUnit := ''; ItemType := '';
    end
    else if (Depth >= 4) and (Stack[2] = 'Files') then
    begin
      if (Depth <> 4) or not T.IsEmpty then Fail('wfc.lpk: item fields must be empty direct elements');
      Text := SingleAttribute(T, 'Value');
      if T.Name = 'Filename' then
      begin
        if HasFile then Fail('wfc.lpk: duplicate Filename field');
        HasFile := True; ItemFile := Text;
      end
      else if T.Name = 'UnitName' then
      begin
        if HasUnit then Fail('wfc.lpk: duplicate UnitName field');
        HasUnit := True; ItemUnit := Text;
      end
      else if T.Name = 'Type' then
      begin
        if HasType then Fail('wfc.lpk: duplicate Type field');
        HasType := True; ItemType := Text;
      end
      else Fail('wfc.lpk: unsupported item field ' + T.Name);
    end;
    if not T.IsEmpty then begin Stack[Depth] := T.Name; Inc(Depth); end;
  end;
  if Depth <> 0 then Fail('wfc.lpk: unclosed XML element');
  if (RootCount <> 1) or (PackageCount <> 1) or (FilesCount <> 1) then
    Fail('wfc.lpk: CONFIG/Package/Files is required exactly once');
  if MainCount <> 1 then Fail('wfc.lpk: expected exactly one main unit row');
  RequireComplete(Units, Seen, 'wfc.lpk');
end;

function CheckWfcPackageManifests(const ASourceUnits: TWfcPackageSourceUnits;
  const AFpmake, ALazarusXml, APackagePascal: String): TWfcPackageCheckResult;
var I, J: Integer; LazarusOrder: TWfcPackageSourceUnits;
begin
  Result := Default(TWfcPackageCheckResult);
  try
    if (Length(ASourceUnits) = 0) or (Length(ASourceUnits) > WFC_PACKAGE_CHECK_MAX_UNITS) then
      Fail('source inventory: unit count is outside 1..' + IntToStr(WFC_PACKAGE_CHECK_MAX_UNITS));
    for I := 0 to High(ASourceUnits) do
    begin
      if not IsUnitName(ASourceUnits[I]) or (ASourceUnits[I] = 'wfc_package') then
        Fail('source inventory: noncanonical or reserved unit name ' + ASourceUnits[I]);
      for J := 0 to I - 1 do
        if ASourceUnits[J] = ASourceUnits[I] then Fail('source inventory: duplicate unit ' + ASourceUnits[I]);
    end;
    Result.SourceUnitCount := Length(ASourceUnits);
    { Reject oversized inputs together before constructing token arrays. }
    InitialPosition(AFpmake, 'fpmake.pp');
    InitialPosition(ALazarusXml, 'wfc.lpk');
    InitialPosition(APackagePascal, 'wfc_package.pas');
    CheckFpm(ASourceUnits, AFpmake, Result.FpmUnitCount);
    CheckLazarus(ASourceUnits, ALazarusXml, Result.LazarusUnitCount, LazarusOrder);
    CheckPackageUnit(ASourceUnits, LazarusOrder, APackagePascal, Result.PackageUnitCount);
    Result.Passed := True;
  except
    on E: EWfcPackageCheck do Result.Diagnostic := E.Message;
  end;
end;

end.
