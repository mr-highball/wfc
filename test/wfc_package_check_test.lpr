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
program wfc_package_check_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc_package_check_app;

type
  TTestProcedure = procedure;
  TFixture = record
    Units: TWfcPackageSourceUnits;
    Fpm, Xml, PackageUnit: String;
  end;

var Checks, Failures: Integer;

procedure Check(const Condition: Boolean; const MessageText: String);
begin
  Inc(Checks);
  if Condition then Exit;
  Inc(Failures); WriteLn('[FAIL] ', MessageText);
end;

procedure RunTest(const Name: String; const Test: TTestProcedure);
begin
  WriteLn('[TEST] ', Name);
  try Test;
  except on E: Exception do
    begin Inc(Failures); WriteLn('[EXCEPTION] ', E.ClassName, ': ', E.Message); end;
  end;
end;

function MainItem: String;
begin
  Result := '<Item1><Filename Value="wfc_package.pas"/><Type Value="Main Unit"/>' +
    '<UnitName Value="wfc_package"/></Item1>';
end;

function SourceItem(const Index: Integer; const Name: String): String;
begin
  Result := '<Item' + IntToStr(Index) + '><Filename Value="src/' + Name +
    '.pas"/><UnitName Value="' + Name + '"/></Item' + IntToStr(Index) + '>';
end;

function Fixture: TFixture;
begin
  Result := Default(TFixture);
  SetLength(Result.Units, 2); Result.Units[0] := 'alpha'; Result.Units[1] := 'beta';
  Result.Fpm := 'program fpmake; begin P.SourcePath.Add(''src''); ' +
    'P.Targets.AddUnit(''alpha.pas''); P.Targets.AddUnit(''beta.pas''); end.';
  Result.Xml := '<?xml version="1.0" encoding="UTF-8"?><CONFIG><Package><Files Count="3">' +
    MainItem + SourceItem(2, 'alpha') + SourceItem(3, 'beta') + '</Files></Package></CONFIG>';
  Result.PackageUnit := 'unit wfc_package; interface uses alpha, beta; implementation end.';
end;

function Replaced(const S, OldText, NewText: String): String;
begin
  if Pos(OldText, S) = 0 then raise Exception.Create('test replacement target missing: ' + OldText);
  Result := StringReplace(S, OldText, NewText, [rfReplaceAll]);
end;

function Repeated(const S: String; Count: Integer): String;
var Chunk: String;
begin
  Result := ''; Chunk := S;
  while Count > 0 do
  begin
    if Odd(Count) then Result := Result + Chunk;
    Count := Count div 2;
    if Count > 0 then Chunk := Chunk + Chunk;
  end;
end;

function Bom: String;
begin
  {$IFDEF PAS2JS}Result := Chr($FEFF);{$ELSE}Result := #$EF#$BB#$BF;{$ENDIF}
end;

function Checked(const F: TFixture): TWfcPackageCheckResult;
begin Result := CheckWfcPackageManifests(F.Units, F.Fpm, F.Xml, F.PackageUnit); end;

procedure Accept(const F: TFixture; const Name: String);
var R: TWfcPackageCheckResult;
begin
  R := Checked(F);
  Check(R.Passed, Name + ': ' + R.Diagnostic);
  Check((R.Diagnostic = '') and (R.SourceUnitCount = Length(F.Units)) and
    (R.FpmUnitCount = Length(F.Units)) and (R.LazarusUnitCount = Length(F.Units)) and
    (R.PackageUnitCount = Length(F.Units)), Name + ': exact counts and no diagnostic');
end;

procedure Reject(const F: TFixture; const Name: String; const Fragment: String = '');
var R: TWfcPackageCheckResult; I: Integer; Printable: Boolean;
begin
  R := Checked(F);
  Check(not R.Passed, Name + ': rejected');
  Check((R.Diagnostic <> '') and ((Fragment = '') or (Pos(Fragment, R.Diagnostic) > 0)),
    Name + ': useful diagnostic: ' + R.Diagnostic);
  Printable := Length(R.Diagnostic) <= WFC_PACKAGE_CHECK_MAX_DIAGNOSTIC_LENGTH;
  for I := 1 to Length(R.Diagnostic) do
    if not (Ord(R.Diagnostic[I]) in [32..126]) then Printable := False;
  Check(Printable, Name + ': diagnostic bounded printable ASCII');
end;

procedure RejectDeclaration(const S, Name: String);
var Failed: Boolean;
begin
  Failed := False;
  try WfcPackageDeclaredUnitName(S);
  except on E: EWfcPackageCheck do Failed := E.Message <> ''; end;
  Check(Failed, Name + ': source declaration rejected');
end;

procedure TestValidAndLexical;
var F: TFixture; I: Integer; S: String;
begin
  Check(WFC_PACKAGE_CHECK_VERSION = 1, 'checker contract version');
  F := Fixture; Accept(F, 'minimal complete package');
  F.Units[0] := 'beta'; F.Units[1] := 'alpha'; Accept(F, 'inventory need not have manifest order');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'begin',
    '{$mode objfpc}{$H+} uses {$ifdef unix} CThreads, {$endif} SysUtils, fpmkunit; begin');
  Accept(F, 'existing conditional CThreads prelude');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'begin',
    '{$if defined(unix)} {$ifopt R+} {$else} {$endif} {$elseif true} {$endif} begin');
  Accept(F, 'balanced unrelated conditional directives');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'P.Targets', 'p.TaRgEtS');
  F.Fpm := Replaced(F.Fpm, '.AddUnit', '.aDdUnIt');
  F.PackageUnit := UpperCase(F.PackageUnit); Accept(F, 'Pascal identifiers case insensitive');
  F := Fixture; F.Fpm := Bom + F.Fpm; F.Xml := Bom + F.Xml;
  F.PackageUnit := Bom + F.PackageUnit; Accept(F, 'leading BOM all manifests');
  F := Fixture;
  F.Fpm := Replaced(F.Fpm, 'begin', 'begin { P.Targets.AddUnit(''ghost.pas''); } ' +
    '(* P.SourcePath.Add(''../src''); { nested {$include nonexistent.inc} } *) ' +
    '// P.Targets.AddUnit(''ghost.pas'');' + #10 +
    'WriteLn(''bait '''' P.Targets.AddUnit(''''ghost.pas'''');'');');
  Accept(F, 'Pascal comments strings doubled quotes are not declarations');
  F := Fixture; F.PackageUnit := Replaced(F.PackageUnit, 'uses',
    '{ uses ghost; } (* uses missing; *) uses {alpha,beta bait}');
  F.Xml := Replaced(F.Xml, '<Files', '<!-- <Files Count="1"><Item1/></Files> -->' +
    '<Description Value="P.Targets.AddUnit(&apos;ghost.pas&apos;); &amp; &lt; &gt; &quot;"/><Files');
  Accept(F, 'XML comments and metadata do not invent package entries');
  F := Fixture; F.Xml := Replaced(F.Xml, 'Value="alpha"/>', 'Value="alpha"/><Type Value="Unit"/>');
  Accept(F, 'explicit ordinary Lazarus Unit type');
  F := Fixture; F.Xml := Replaced(F.Xml, '"', ''''); Accept(F, 'single quoted XML attributes');
  F := Fixture; F.Fpm := Replaced(F.Fpm,
    'P.Targets.AddUnit(''alpha.pas''); P.Targets.AddUnit(''beta.pas'');',
    'P.Targets.AddUnit(''beta.pas''); P.Targets.AddUnit(''alpha.pas'');');
  Accept(F, 'FPM target order remains independent');
  Check(WfcPackageDeclaredUnitName('unit alpha;') = 'alpha', 'minimal source declaration');
  Check(WfcPackageDeclaredUnitName(Bom + '{$mode delphi}{$I-}{comment} UNIT Alpha;') = 'alpha',
    'source declaration permits BOM mode and I/O-checking directive');
  Check(WfcPackageDeclaredUnitName('unit alpha; ''unterminated body') = 'alpha',
    'declaration helper does not tokenize body');
  S := 'unit alpha;' + StringOfChar('?', WFC_PACKAGE_CHECK_MAX_TEXT_LENGTH - 11);
  Check(Length(S) = WFC_PACKAGE_CHECK_MAX_TEXT_LENGTH, 'source size boundary fixture');
  Check(WfcPackageDeclaredUnitName(S) = 'alpha', 'source size inclusive boundary');
  for I := 0 to Length('unit alpha;') - 1 do
    RejectDeclaration(Copy('unit alpha;', 1, I), 'truncated source declaration');
  RejectDeclaration('program alpha;', 'program is not a unit');
  RejectDeclaration('unit alpha.beta;', 'qualified unit unsupported');
  RejectDeclaration('unit ''alpha'';', 'string is not unit name');
  RejectDeclaration('{$if true}unit alpha;{$endif}', 'conditional source declaration');
  RejectDeclaration('{$include missing.inc}unit alpha;', 'source include unsupported');
  RejectDeclaration('(* never closed unit alpha;', 'unterminated source comment');
  RejectDeclaration(S + '?', 'source size overflow');
end;

procedure TestFpmAndPackageFailures;
var F: TFixture; I: Integer; Full: String;
begin
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'P.Targets.AddUnit(''alpha.pas'');', '');
  Reject(F, 'missing FPM unit', 'missing unit alpha');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'beta.pas', 'alpha.pas');
  Reject(F, 'duplicate FPM unit', 'duplicate');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'alpha.pas', 'unknown.pas'); Reject(F, 'unknown FPM unit', 'unknown');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'alpha.pas', '../alpha.pas'); Reject(F, 'FPM traversal', 'noncanonical');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'alpha.pas', 'src/alpha.pas'); Reject(F, 'FPM prefixed path', 'noncanonical');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'alpha.pas', 'alpha.PAS'); Reject(F, 'FPM extension case', 'noncanonical');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'alpha.pas', 'Alpha.pas'); Reject(F, 'FPM basename case', 'noncanonical');
  F := Fixture; F.Fpm := Replaced(F.Fpm, '''alpha.pas''', '''alpha'' + ''.pas'''); Reject(F, 'FPM concatenated expression', 'requires');
  F := Fixture; F.Fpm := Replaced(F.Fpm, '''alpha.pas''', 'UnitFile'); Reject(F, 'FPM variable expression', 'requires');
  F := Fixture; F.Fpm := Replaced(F.Fpm, '''alpha.pas''', '''alpha.pas'', True'); Reject(F, 'FPM extra argument', 'requires');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'P.Targets.AddUnit', 'Targets.AddUnit'); Reject(F, 'FPM omitted owner', 'requires');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'P.Targets.AddUnit', 'Other.P.Targets.AddUnit'); Reject(F, 'FPM qualified owner', 'unqualified');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'P.Targets.AddUnit(''alpha.pas'');',
    '{$ifdef SOME}P.Targets.AddUnit(''alpha.pas'');{$endif}'); Reject(F, 'conditional FPM declaration', 'conditional');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'P.SourcePath.Add(''src'');', ''); Reject(F, 'missing FPM source path', 'source path');
  F := Fixture; F.Fpm := Replaced(F.Fpm, '''src''', '''src'' + '''''); Reject(F, 'FPM source path expression', 'source path');
  F := Fixture; F.Fpm := Replaced(F.Fpm, '''src''', '''../src'''); Reject(F, 'FPM source path escape', 'source path');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'P.SourcePath.Add(''src'');',
    'P.SourcePath.Add(''src'');P.SourcePath.Add(''src'');'); Reject(F, 'duplicate FPM source path', 'exactly one');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'P.SourcePath', 'Other.P.SourcePath'); Reject(F, 'qualified FPM source path', 'unqualified');
  F := Fixture; F.Fpm := '{$include hidden.inc}' + F.Fpm; Reject(F, 'FPM include', 'include');
  F := Fixture; F.Fpm := '{$endif}' + F.Fpm; Reject(F, 'unmatched conditional', 'unmatched');
  F := Fixture; F.Fpm := '{$ifdef X}' + F.Fpm; Reject(F, 'unterminated conditional', 'unterminated');
  F := Fixture; F.Fpm := F.Fpm + ''''; Reject(F, 'unterminated string', 'unterminated');
  F := Fixture; F.Fpm := F.Fpm + '(*'; Reject(F, 'unterminated comment', 'unterminated');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'alpha.pas', 'alpha' + #10 + '.pas'); Reject(F, 'literal control character', 'control');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'program fpmake;', 'program;'); Reject(F, 'malformed program header', 'program');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'end.', 'end'); Reject(F, 'missing program envelope', 'end.');
  F := Fixture; F.PackageUnit := Replaced(F.PackageUnit, 'alpha, ', ''); Reject(F, 'missing package use');
  F := Fixture; F.PackageUnit := Replaced(F.PackageUnit, 'beta', 'alpha'); Reject(F, 'duplicate package use', 'duplicate');
  F := Fixture; F.PackageUnit := Replaced(F.PackageUnit, 'alpha, beta', 'beta, alpha');
  Reject(F, 'package and Lazarus source item order must match', 'order differs');
  F := Fixture; F.PackageUnit := Replaced(F.PackageUnit, 'alpha', 'unknown'); Reject(F, 'unknown package use', 'unknown');
  F := Fixture; F.PackageUnit := Replaced(F.PackageUnit, 'alpha', 'alpha in ''src/alpha.pas'''); Reject(F, 'package uses explicit path', 'expected');
  F := Fixture; F.PackageUnit := Replaced(F.PackageUnit, 'alpha', 'some.alpha'); Reject(F, 'package qualified use');
  F := Fixture; F.PackageUnit := Replaced(F.PackageUnit, 'alpha', '{$ifdef X}alpha{$endif}'); Reject(F, 'conditional package use', 'conditional');
  F := Fixture; F.PackageUnit := Replaced(F.PackageUnit, 'wfc_package', 'other'); Reject(F, 'wrong package main declaration', 'wfc_package');
  F := Fixture; F.PackageUnit := Replaced(F.PackageUnit, 'interface uses alpha, beta;',
    'interface'); F.PackageUnit := Replaced(F.PackageUnit, 'implementation', 'implementation uses alpha, beta;');
  Reject(F, 'implementation uses cannot substitute interface exports', 'uses');
  F := Fixture; F.PackageUnit := Replaced(F.PackageUnit, 'end.', 'end'); Reject(F, 'missing package envelope', 'end.');
  F := Fixture; Full := F.PackageUnit;
  for I := 0 to Length(F.PackageUnit) - 1 do
  begin
    F.PackageUnit := Copy(Full, 1, I);
    Reject(F, 'truncated package manifest');
  end;
end;

procedure TestXmlFailures;
var F: TFixture; I: Integer; Full: String;
begin
  F := Fixture; F.Xml := Replaced(F.Xml, 'Count="3"', 'Count="2"'); Reject(F, 'incorrect Files count', 'Count');
  F := Fixture; F.Xml := Replaced(F.Xml, 'Count="3"', 'Count="03"'); Reject(F, 'leading zero count', 'noncanonical');
  F := Fixture; F.Xml := Replaced(F.Xml, 'Count="3"', 'Count="-3"'); Reject(F, 'negative count', 'noncanonical');
  F := Fixture; F.Xml := Replaced(F.Xml, 'Count="3"', 'Count="3.0"'); Reject(F, 'fractional count', 'noncanonical');
  F := Fixture; F.Xml := Replaced(F.Xml, 'Count="3"', 'Count="99999999999999999"'); Reject(F, 'overflowing count', 'limit');
  F := Fixture; F.Xml := Replaced(F.Xml, SourceItem(2, 'alpha'), ''); Reject(F, 'missing item index', 'Item2');
  F := Fixture; F.Xml := Replaced(F.Xml, SourceItem(3, 'beta'), ''); Reject(F, 'missing final item', 'Count');
  F := Fixture; F.Xml := Replaced(F.Xml, 'Item2', 'Item3'); Reject(F, 'reordered item ordinal', 'Item2');
  F := Fixture; F.Xml := Replaced(F.Xml, 'Item2', 'Item02'); Reject(F, 'noncanonical item ordinal', 'Item2');
  F := Fixture; F.Xml := Replaced(F.Xml, 'src/alpha.pas', 'src/beta.pas'); Reject(F, 'filename declaration mismatch', 'mismatch');
  F := Fixture; F.Xml := Replaced(F.Xml, SourceItem(2, 'alpha'), SourceItem(2, 'beta')); Reject(F, 'duplicate source row', 'duplicate');
  F := Fixture; F.Xml := Replaced(F.Xml, SourceItem(2, 'alpha'), SourceItem(2, 'ghost')); Reject(F, 'unknown source row', 'unknown');
  F := Fixture; F.Xml := Replaced(F.Xml, 'src/alpha.pas', 'src/../alpha.pas'); Reject(F, 'XML traversal', 'mismatch');
  F := Fixture; F.Xml := Replaced(F.Xml, 'src/alpha.pas', 'src\alpha.pas'); Reject(F, 'XML backslash path', 'noncanonical');
  F := Fixture; F.Xml := Replaced(F.Xml, 'src/alpha.pas', 'D:/src/alpha.pas'); Reject(F, 'XML absolute path', 'noncanonical');
  F := Fixture; F.Xml := Replaced(F.Xml, 'src/alpha.pas', 'src/alpha.PAS'); Reject(F, 'XML extension case', 'noncanonical');
  F := Fixture; F.Xml := Replaced(F.Xml, 'Value="alpha"', 'Value="Alpha"'); Reject(F, 'XML UnitName case', 'mismatch');
  F := Fixture; F.Xml := Replaced(F.Xml, 'Main Unit', 'Unit'); Reject(F, 'wrong main row type', 'main unit');
  F := Fixture; F.Xml := Replaced(F.Xml, MainItem, SourceItem(1, 'alpha')); Reject(F, 'missing main row');
  F := Fixture; F.Xml := Replaced(F.Xml, SourceItem(2, 'alpha'), Replaced(MainItem, 'Item1', 'Item2'));
  Reject(F, 'duplicate main row', 'duplicate main');
  F := Fixture; F.Xml := Replaced(F.Xml, '<UnitName Value="alpha"/>', ''); Reject(F, 'missing UnitName', 'requires');
  F := Fixture; F.Xml := Replaced(F.Xml, '<UnitName Value="alpha"/>',
    '<UnitName Value="alpha"/><UnitName Value="alpha"/>'); Reject(F, 'duplicate UnitName', 'duplicate');
  F := Fixture; F.Xml := Replaced(F.Xml, '<Filename Value="src/alpha.pas"/>',
    '<Filename Value="src/alpha.pas" Value="src/beta.pas"/>'); Reject(F, 'duplicate attribute', 'duplicate');
  F := Fixture; F.Xml := Replaced(F.Xml, '<UnitName Value="alpha"/>', '<UnitName Value="alpha" Extra="1"/>');
  Reject(F, 'unknown item field attribute', 'exactly');
  F := Fixture; F.Xml := Replaced(F.Xml, '<UnitName Value="alpha"/>', '<UnitName Value="alpha"></UnitName>');
  Reject(F, 'nonempty item field form', 'empty');
  F := Fixture; F.Xml := Replaced(F.Xml, '<UnitName Value="alpha"/>', '<Bogus Value="alpha"/>');
  Reject(F, 'unknown item field', 'unsupported');
  F := Fixture; F.Xml := Replaced(F.Xml, '<Item2>', '<Item2 Index="2">'); Reject(F, 'item attributes', 'Item2');
  F := Fixture; F.Xml := Replaced(F.Xml, '<Files', '<Wrapper><Files');
  F.Xml := Replaced(F.Xml, '</Files>', '</Files></Wrapper>'); Reject(F, 'Files under wrong parent', 'direct child');
  F := Fixture; F.Xml := Replaced(F.Xml, '<Files', '<Files Count="3"></Files><Files'); Reject(F, 'duplicate Files');
  F := Fixture; F.Xml := F.Xml + '<CONFIG/>'; Reject(F, 'multiple XML roots', 'exactly one');
  F := Fixture; F.Xml := Replaced(F.Xml, '</Package>', '</Other>'); Reject(F, 'mismatched end tag', 'mismatched');
  F := Fixture; F.Xml := Replaced(F.Xml, '<Files', '<Package/><Files'); Reject(F, 'nested Package', 'direct child');
  F := Fixture; F.Xml := Replaced(F.Xml, '<Files', 'text<Files'); Reject(F, 'nonwhitespace element text', 'whitespace');
  F := Fixture; F.Xml := Replaced(F.Xml, '<Files', '<![CDATA[bait]]><Files'); Reject(F, 'CDATA unsupported', 'name');
  F := Fixture; F.Xml := Replaced(F.Xml, '<CONFIG>', '<!DOCTYPE CONFIG><CONFIG>'); Reject(F, 'DTD unsupported', 'name');
  F := Fixture; F.Xml := Replaced(F.Xml, '<Files', '<?other x="y"?><Files'); Reject(F, 'PI unsupported', 'declaration');
  F := Fixture; F.Xml := Replaced(F.Xml, '<Files', '<?xml version="1.0"?><Files'); Reject(F, 'late declaration', 'declaration');
  F := Fixture; F.Xml := Replaced(F.Xml, 'version="1.0"', 'version="1.1"'); Reject(F, 'XML version unsupported', 'version');
  F := Fixture; F.Xml := Replaced(F.Xml, 'encoding="UTF-8"', 'encoding="UTF-16"'); Reject(F, 'XML encoding unsupported', 'encoding');
  F := Fixture; F.Xml := Replaced(F.Xml, 'version="1.0" ', ''); Reject(F, 'XML version required', 'version');
  F := Fixture; F.Xml := Replaced(F.Xml, 'Value="alpha"', 'Value="&#97;lpha"'); Reject(F, 'numeric entity unsupported', 'predefined');
  F := Fixture; F.Xml := Replaced(F.Xml, 'Value="alpha"', 'Value="&custom;"'); Reject(F, 'custom entity unsupported');
  F := Fixture; F.Xml := Replaced(F.Xml, 'Value="alpha"', 'Value="al<pha"'); Reject(F, 'raw XML less than', 'invalid');
  F := Fixture; F.Xml := Replaced(F.Xml, 'Value="alpha"', 'Value="al' + #1 + 'pha"'); Reject(F, 'XML attribute control', 'invalid');
  F := Fixture; F.Xml := Replaced(F.Xml, '<Files', '<!-- invalid -- comment --><Files'); Reject(F, 'invalid XML comment', 'hyphen');
  F := Fixture; F.Xml := Replaced(F.Xml, '<Files', '<!-- unterminated <Files'); Reject(F, 'unterminated XML comment', 'unterminated');
  F := Fixture; Full := F.Xml;
  for I := 0 to Length(Full) - 1 do
  begin F.Xml := Copy(Full, 1, I); Reject(F, 'truncated XML manifest'); end;
end;

procedure TestBudgetsAndInventory;
var F: TFixture; I: Integer; S, N: String;
begin
  F := Fixture; F.Units := nil; Reject(F, 'empty source inventory', 'count');
  F := Fixture; F.Units[1] := 'alpha'; Reject(F, 'duplicate inventory', 'duplicate');
  F := Fixture; F.Units[0] := 'Alpha'; Reject(F, 'inventory case', 'noncanonical');
  F := Fixture; F.Units[0] := '../alpha'; Reject(F, 'inventory path', 'noncanonical');
  F := Fixture; F.Units[0] := 'alpha.pas'; Reject(F, 'inventory extension', 'noncanonical');
  F := Fixture; F.Units[0] := '1alpha'; Reject(F, 'inventory invalid first character', 'noncanonical');
  F := Fixture; F.Units[0] := 'wfc_package'; Reject(F, 'main unit cannot be inventory source', 'reserved');
  F := Fixture; F.Units[0] := StringOfChar('a', WFC_PACKAGE_CHECK_MAX_NAME_LENGTH + 1);
  Reject(F, 'inventory name length', 'noncanonical');
  F := Fixture; F.Units[0] := 'a' + #10 + StringOfChar('a', 900);
  Reject(F, 'oversized unprintable name has escaped truncated diagnostic');
  F := Fixture; SetLength(F.Units, WFC_PACKAGE_CHECK_MAX_UNITS + 1); Reject(F, 'inventory count cap', 'count');
  F := Fixture; F.Fpm := StringOfChar(' ', WFC_PACKAGE_CHECK_MAX_TEXT_LENGTH + 1); Reject(F, 'FPM byte cap preflight', 'length limit');
  F := Fixture; F.Xml := StringOfChar(' ', WFC_PACKAGE_CHECK_MAX_TEXT_LENGTH + 1); Reject(F, 'XML byte cap preflight', 'length limit');
  F := Fixture; F.PackageUnit := StringOfChar(' ', WFC_PACKAGE_CHECK_MAX_TEXT_LENGTH + 1);
  Reject(F, 'package byte cap preflight', 'length limit');
  F := Fixture; F.Fpm := F.Fpm + StringOfChar(' ', WFC_PACKAGE_CHECK_MAX_TEXT_LENGTH - Length(F.Fpm));
  Accept(F, 'FPM inclusive text boundary');
  F := Fixture; F.Xml := F.Xml + StringOfChar(' ', WFC_PACKAGE_CHECK_MAX_TEXT_LENGTH - Length(F.Xml));
  Accept(F, 'XML inclusive text boundary');
  F := Fixture; F.PackageUnit := F.PackageUnit + StringOfChar(' ', WFC_PACKAGE_CHECK_MAX_TEXT_LENGTH - Length(F.PackageUnit));
  Accept(F, 'package inclusive text boundary');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'begin', Repeated('{', 33) + Repeated('}', 33) + 'begin');
  Reject(F, 'comment nesting cap', 'nesting');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'begin', Repeated('{$ifdef X}', 33) + Repeated('{$endif}', 33) + 'begin');
  Reject(F, 'conditional nesting cap', 'nesting');
  F := Fixture; F.Fpm := Replaced(F.Fpm, 'begin', 'begin ' + Repeated('; ', WFC_PACKAGE_CHECK_MAX_TOKENS));
  Reject(F, 'Pascal token cap', 'token count');
  F := Fixture; F.Xml := Replaced(F.Xml, '<Files', Repeated('<x>', 33) + Repeated('</x>', 33) + '<Files');
  Reject(F, 'XML depth cap', 'nesting');
  F := Fixture; S := '<Metadata';
  for I := 1 to 33 do S := S + ' a' + IntToStr(I) + '="x"';
  S := S + '/>'; F.Xml := Replaced(F.Xml, '<Files', S + '<Files'); Reject(F, 'XML attribute cap', 'attribute count');
  F := Fixture; F.Xml := Replaced(F.Xml, '<Files', Repeated('<x/>', WFC_PACKAGE_CHECK_MAX_TOKENS) + '<Files');
  Reject(F, 'XML tag cap', 'tag count');
  F := Fixture;
  SetLength(F.Units, WFC_PACKAGE_CHECK_MAX_UNITS);
  F.Fpm := 'program fpmake; begin P.SourcePath.Add(''src'');';
  F.Xml := '<CONFIG><Package><Files Count="' + IntToStr(Length(F.Units) + 1) + '">' + MainItem;
  F.PackageUnit := 'unit wfc_package; interface uses ';
  for I := 0 to High(F.Units) do
  begin
    N := 'u' + IntToStr(I); F.Units[I] := N;
    F.Fpm := F.Fpm + 'P.Targets.AddUnit(''' + N + '.pas'');';
    F.Xml := F.Xml + SourceItem(I + 2, N);
    if I <> 0 then F.PackageUnit := F.PackageUnit + ',';
    F.PackageUnit := F.PackageUnit + N;
  end;
  F.Fpm := F.Fpm + 'end.'; F.Xml := F.Xml + '</Files></Package></CONFIG>';
  F.PackageUnit := F.PackageUnit + '; implementation end.';
  Accept(F, 'inclusive 4096 source-unit count boundary');
end;

begin
  RunTest('complete manifests and comment-aware Pascal declarations', @TestValidAndLexical);
  RunTest('FPM and package declaration rejection', @TestFpmAndPackageFailures);
  RunTest('strict bounded Lazarus manifest surface', @TestXmlFailures);
  RunTest('inventory identity and input budgets', @TestBudgetsAndInventory);
  WriteLn('[SUMMARY] checks=', Checks, ' failures=', Failures);
  if Failures <> 0 then Halt(1);
end.
