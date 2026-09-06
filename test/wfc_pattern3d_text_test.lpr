{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_pattern3d_text_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host, JS, Web,{$ENDIF}
  SysUtils, wfc_model, wfc_learn3d, wfc_pattern3d, wfc_pattern3d_learn,
  wfc_pattern3d_text, wfc_pattern2d, wfc_pattern2d_learn, wfc_pattern2d_text,
  wfc_text_codec;

const
  GOLDEN =
    'wfcp=2'#10'rank=3'#10'samples=1'#10's=0,1,1,2'#10 +
    'footprint=1,1,2'#10'boundary=wrap'#10'symmetry=none'#10 +
    'directions=N,E,S,W,U,D'#10'palette=2'#10 +
    't=0,%2C'#10't=1,%F0%9F%8E%B5'#10'patterns=2'#10 +
    'p=0,1,0,1'#10'p=1,1,1,0'#10'relations=overlap'#10'end'#10;

var Checks: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then
  begin
    {$IFDEF PAS2JS}document.body.setAttribute('data-self-test-message', AMessage);{$ENDIF}
    raise Exception.Create(AMessage);
  end;
end;

function Tokens(const AValues: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

function Change(const AFrom, ATo: String; const AText: String = GOLDEN): String;
var Position: Integer;
begin
  Position := Pos(AFrom, AText);
  if Position = 0 then raise Exception.Create('invalid mutation fixture: ' + AFrom);
  Result := Copy(AText, 1, Position - 1) + ATo +
    Copy(AText, Position + Length(AFrom), Length(AText));
end;

procedure Reject(const AText, AName: String);
var Model: TWfcOverlappingModel3D; Raised: Boolean;
begin
  Model := nil; Raised := False;
  try
    try Model := DecodeWfcPattern3DText(AText);
    except on E: EConvertError do Raised := True; end;
  finally Model.Free; end;
  Check(Raised, 'strict rejection: ' + AName);
end;

procedure SameModel(const A, B: TWfcOverlappingModel3D);
var I, J, K: Integer; D: TWfcModelDirection; SA, SB: TWfcModelSampleShape;
  PA, PB: TWfcPattern3DPayload;
begin
  Check((A.PatternWidth = B.PatternWidth) and (A.PatternHeight = B.PatternHeight) and
    (A.PatternDepth = B.PatternDepth) and (A.SourceBoundary = B.SourceBoundary) and
    (A.Symmetry = B.Symmetry), 'all volume metadata survives');
  Check((A.SourceCount = B.SourceCount) and (A.PaletteCount = B.PaletteCount) and
    (A.PatternCount = B.PatternCount), 'all volume counts survive');
  for I := 0 to A.SourceCount - 1 do
  begin
    SA := A.SourceShapeAt(I); SB := B.SourceShapeAt(I);
    Check((SA.Width = SB.Width) and (SA.Height = SB.Height) and (SA.Depth = SB.Depth),
      'ordered original source XYZ survives');
  end;
  for I := 0 to A.PaletteCount - 1 do
    Check(A.PaletteTokenAt(I) = B.PaletteTokenAt(I), 'exact Unicode palette order survives');
  for I := 0 to A.PatternCount - 1 do
  begin
    Check(A.PatternWeightAt(I) = B.PatternWeightAt(I), 'raw observation weight survives');
    Check(A.PatternKeyAt(I) = B.PatternKeyAt(I), 'exact payload identity survives');
    PA := A.CopyPattern(I); PB := B.CopyPattern(I);
    Check(Length(PA) = Length(PB), 'payload length survives');
    for K := 0 to High(PA) do Check(PA[K] = PB[K], 'X-fast then Y then Z payload survives');
    for J := 0 to A.PatternCount - 1 do
      for D := wmdNorth to wmdDown do
      begin
        Check(A.PatternsCompatible(I, J, D) = B.PatternsCompatible(I, J, D),
          'every derived overlap relation survives');
        Check(A.CompiledModel.RelationCount(D, I, J) =
          B.CompiledModel.RelationCount(D, I, J), 'all six compiled planes survive');
      end;
  end;
end;

procedure RoundTrip(const A: TWfcOverlappingModel3D);
var B: TWfcOverlappingModel3D; TextValue: String;
begin
  TextValue := EncodeWfcPattern3DText(A);
  Check((Pos('wfcp=2'#10'rank=3'#10, TextValue) = 1) and
    (Pos(#10'relations=overlap'#10'end'#10, TextValue) > 0) and
    (Pos(#10'r=', TextValue) = 0), 'volume format is compact and explicitly rank three');
  B := DecodeWfcPattern3DText(TextValue);
  try
    Check(EncodeWfcPattern3DText(B) = TextValue, 'byte-exact canonical round trip');
    SameModel(A, B);
  finally B.Free; end;
end;

procedure TestGoldenAndOwnership;
var A, B: TWfcOverlappingModel3D; Palette: TWfcModelTokens;
  Patterns: TWfcPattern3DPayloads; Weights: TWfcModelIntegerArray;
  Shapes: TWfcModelSampleShapes; D: TWfcModelDirection; I, J: Integer;
begin
  Palette := Tokens([',', WfcTextDecodeToken('%F0%9F%8E%B5', 'test')]);
  A := LearnOverlappingModel3D(Palette, 1, 1, 2, 1, 1, 2, wmbWrap, wmsNone);
  try
    Check(EncodeWfcPattern3DText(A) = GOLDEN, 'fixed punctuation and supplementary Unicode golden');
    B := DecodeWfcPattern3DText(GOLDEN);
    try
      SameModel(A, B);
      Palette := B.CopyPalette; Patterns := B.CopyPatterns;
      Weights := B.CopyPatternWeights; Shapes := B.CopySourceShapes;
      Palette[0] := 'mutated'; Patterns[0][0] := 1;
      Weights[0] := 999; Shapes[0].Depth := 999;
      Check(EncodeWfcPattern3DText(B) = GOLDEN, 'all exposed managed arrays are detached');
    finally B.Free; end;
    Check(EncodeWfcPattern3DText(A) = GOLDEN, 'decoded owner destruction leaves original intact');
    RoundTrip(A);
  finally A.Free; end;
  A := LearnOverlappingModel3D(Tokens(['A','B','C']), 1, 1, 3,
    1, 1, 2, wmbWrap, wmsNone);
  try
    for I := 0 to 2 do for J := 0 to 2 do
      for D := wmdNorth to wmdDown do
        case D of
          wmdUp: Check(A.PatternsCompatible(I, J, D) = (J = (I + 1) mod 3),
            'up overlap has independently checked positive Z orientation');
          wmdDown: Check(A.PatternsCompatible(I, J, D) = (I = (J + 1) mod 3),
            'down overlap has independently checked negative Z orientation');
        else Check(A.PatternsCompatible(I, J, D), 'unit XY dimensions have empty shared slab'); end;
    RoundTrip(A);
  finally A.Free; end;
end;

procedure TestLearningAndSymmetry;
var Samples: TWfcLearnVolumeSamples; A: TWfcOverlappingModel3D;
  Symmetry: TWfcModelSymmetry; I, Total, Factor: Integer;
begin
  SetLength(Samples, 2);
  Samples[0] := MakeLearnSample3D(Tokens(['z','a','z','a']), 2, 1, 2);
  Samples[1] := MakeLearnSample3D(Tokens(['a','z','a']), 1, 1, 3);
  A := LearnOverlappingModel3DCorpus(Samples, 1, 1, 2, wmbOpen, wmsNone);
  try
    Check((A.PaletteTokenAt(0) = 'z') and (A.PaletteTokenAt(1) = 'a'),
      'palette order is observed first appearance, not lexical normalization');
    Total := 0;
    for I := 0 to A.PatternCount - 1 do Inc(Total, A.PatternWeightAt(I));
    Check(Total = 4, 'heterogeneous open volumes contribute exact independent origins');
    RoundTrip(A);
  finally A.Free; end;
  A := LearnOverlappingModel3D(Tokens(['a','b']), 1, 1, 2,
    2, 3, 5, wmbWrap, wmsNone);
  try RoundTrip(A); finally A.Free; end;
  for Symmetry := wmsNone to wmsCubeFull do
  begin
    A := LearnOverlappingModel3D(Tokens(['a','b','c','d','e','f','g','h']),
      2, 2, 2, 2, 2, 2, wmbWrap, Symmetry);
    try
      case Symmetry of wmsNone: Factor := 1; wmsD4: Factor := 8;
        wmsCubeRotations: Factor := 24; wmsCubeFull: Factor := 48;
      else Factor := 0; end;
      Total := 0;
      for I := 0 to A.PatternCount - 1 do Inc(Total, A.PatternWeightAt(I));
      Check(Total = 8 * Factor, 'literal symmetry augmentation retains raw frequencies');
      RoundTrip(A);
    finally A.Free; end;
  end;
end;

procedure TestExactIdentity;
var A, B, C, D: TWfcOverlappingModel3D;
  Palette: TWfcModelTokens; Patterns: TWfcPattern3DPayloads;
begin
  A := LearnOverlappingModel3D(Tokens(['A:B','C']), 2, 1, 1,
    2, 1, 1, wmbOpen, wmsNone);
  B := LearnOverlappingModel3D(Tokens(['A','B:C']), 2, 1, 1,
    2, 1, 1, wmbOpen, wmsNone);
  C := LearnOverlappingModel3D(Tokens(['A:B','C']), 1, 1, 2,
    1, 1, 2, wmbOpen, wmsNone);
  D := nil;
  try
    Check(A.PatternKeyAt(0) <> B.PatternKeyAt(0),
      'delimiter-bearing token boundaries cannot alias latent identity');
    Check(A.PatternKeyAt(0) <> C.PatternKeyAt(0),
      'XYZ footprint dimensions distinguish otherwise equal flattened payload');
    Palette := Tokens(['C','A:B']);
    Patterns := A.CopyPatterns; Patterns[0][0] := 1; Patterns[0][1] := 0;
    D := TWfcOverlappingModel3D.Create(2, 1, 1, wmbOpen, wmsNone,
      A.CopySourceShapes, Palette, Patterns, A.CopyPatternWeights);
    Check(A.PatternKeyAt(0) = D.PatternKeyAt(0),
      'actual token identity does not depend on arbitrary palette indices');
    RoundTrip(A); RoundTrip(B); RoundTrip(C); RoundTrip(D);
  finally D.Free; C.Free; B.Free; A.Free; end;
end;

procedure TestDerivedKeyPreflight;
var Palette: TWfcModelTokens; Patterns: TWfcPattern3DPayloads;
  Weights: TWfcModelIntegerArray; Shapes: TWfcModelSampleShapes;
  Lines: TWfcTextLines; I, L: Integer; TextValue: String;
  Model: TWfcOverlappingModel3D; Raised: Boolean;
begin
  { A small legal compact envelope would expand this shared token into more
    than Integer capacity across exact keys. Rejection must precede that
    expansion: the test never allocates the multi-gigabyte derived strings. }
  SetLength(Palette, 1025); Palette[0] := StringOfChar('a', 2097152);
  SetLength(Patterns, 1024); SetLength(Weights, 1024); SetLength(Shapes, 1);
  Shapes[0].Width := 1025; Shapes[0].Height := 1; Shapes[0].Depth := 1;
  for I := 0 to 1023 do
  begin
    Palette[I + 1] := TWfcModelToken('token-' + IntToStr(I));
    SetLength(Patterns[I], 2); Patterns[I][0] := 0; Patterns[I][1] := I + 1;
    Weights[I] := 1;
  end;
  Model := nil; Raised := False;
  try
    try Model := TWfcOverlappingModel3D.Create(2, 1, 1, wmbOpen, wmsNone,
      Shapes, Palette, Patterns, Weights);
    except on E: EWfcOverlapping3D do
      Raised := Pos('aggregate exact pattern keys', E.Message) > 0; end;
  finally Model.Free; end;
  Check(Raised, 'aggregate exact-key byte capacity is checked before key allocation');
  SetLength(Lines, 11 + 1 + 1025 + 1024);
  Lines[0] := 'wfcp=2'; Lines[1] := 'rank=3'; Lines[2] := 'samples=1';
  Lines[3] := 's=0,1025,1,1'; Lines[4] := 'footprint=2,1,1';
  Lines[5] := 'boundary=open'; Lines[6] := 'symmetry=none';
  Lines[7] := 'directions=N,E,S,W,U,D'; Lines[8] := 'palette=1025'; L := 9;
  for I := 0 to 1024 do
  begin Lines[L] := 't=' + IntToStr(I) + ',' + String(Palette[I]); Inc(L); end;
  Lines[L] := 'patterns=1024'; Inc(L);
  for I := 0 to 1023 do
  begin Lines[L] := 'p=' + IntToStr(I) + ',1,0,' + IntToStr(I + 1); Inc(L); end;
  Lines[L] := 'relations=overlap'; Inc(L); Lines[L] := 'end'; Inc(L);
  Check(L = Length(Lines), 'derived-key adversary has exactly its declared records');
  TextValue := WfcTextJoinCanonicalLines(Lines, 'test');
  Check(Length(TextValue) < WFC_PATTERN_3D_MAX_ENCODED_TEXT_LENGTH,
    'derived-key adversary fits the independent compact text envelope');
  Reject(TextValue, 'compact input cannot bypass aggregate exact-key byte preflight');
end;

procedure TestHostileText;
const InvalidNumbers: array[0..8] of String = ('-1','+1','01','1.0','1e0',
  'NaN','Infinity','2147483648','');
var I: Integer; S: String;
begin
  Reject('', 'empty'); Reject('wfcp=2', 'no final LF');
  Reject(Change('wfcp=2', 'wfcp=1'), 'v1 cannot label a volume');
  Reject(Change('wfcp=2', 'wfcp=3'), 'unknown version');
  Reject(Change('wfcp=2', 'wfcp=02'), 'noncanonical version');
  Reject(Change('rank=3', 'rank=2'), 'v2 is rank three only');
  Reject(Change('rank=3', 'rank=03'), 'noncanonical rank');
  Reject(Change('samples=1', 'samples=0'), 'empty source list');
  Reject(Change('samples=1', 'samples=65537'), 'source-count cap before allocation');
  Reject(Change('s=0,1,1,2', 's=1,1,1,2'), 'unordered source index');
  Reject(Change('s=0,1,1,2', 's=0,1,2'), 'missing source Z');
  Reject(Change('s=0,1,1,2', 's=0,1,1,2,3'), 'extra source field');
  Reject(Change('s=0,1,1,2', 's=0,0,1,2'), 'zero source X');
  Reject(Change('s=0,1,1,2', 's=0,1,0,2'), 'zero source Y');
  Reject(Change('s=0,1,1,2', 's=0,1,1,0'), 'zero source Z');
  Reject(Change('s=0,1,1,2', 's=0,4194305,1,1'), 'source dimension cap');
  Reject(Change('s=0,1,1,2', 's=0,4194304,4194304,4194304'), 'source product preflight');
  S := Change('samples=1'#10's=0,1,1,2',
    'samples=2'#10's=0,4194304,1,1'#10's=1,4194304,1,1');
  Reject(S, 'aggregate source cell cap');
  Reject(Change('footprint=1,1,2', 'footprint=1,2'), 'missing footprint Z');
  Reject(Change('footprint=1,1,2', 'footprint=1,1,2,3'), 'extra footprint field');
  Reject(Change('footprint=1,1,2', 'footprint=0,1,2'), 'zero footprint X');
  Reject(Change('footprint=1,1,2', 'footprint=1,0,2'), 'zero footprint Y');
  Reject(Change('footprint=1,1,2', 'footprint=1,1,0'), 'zero footprint Z');
  Reject(Change('footprint=1,1,2', 'footprint=4097,1,1'), 'footprint dimension cap');
  Reject(Change('footprint=1,1,2', 'footprint=17,17,17'), 'footprint cell cap');
  Reject(Change('boundary=wrap', 'boundary=unknown'), 'unknown boundary');
  Reject(Change('symmetry=none', 'symmetry=cube'), 'unknown symmetry');
  Reject(Change('symmetry=none', 'symmetry=cube24'), 'noncubic cube24 footprint');
  Reject(Change('symmetry=none', 'symmetry=cube48'), 'noncubic cube48 footprint');
  Reject(Change('footprint=1,1,2', 'footprint=2,1,1',
    Change('symmetry=none', 'symmetry=d4')), 'nonsquare D4 footprint');
  Reject(Change('s=0,1,1,2', 's=0,1,1,1',
    Change('boundary=wrap', 'boundary=open')), 'open footprint cannot exceed source');
  Reject(Change('directions=N,E,S,W,U,D', 'directions=N,E,S,W,D,U'), 'direction order');
  Reject(Change('directions=N,E,S,W,U,D', 'directions=N,E,S,W'), 'missing Z directions');
  Reject(Change('palette=2', 'palette=0'), 'empty palette');
  Reject(Change('palette=2', 'palette=4097'), 'palette count cap');
  Reject(Change('t=0,%2C', 't=1,%2C'), 'unordered palette');
  Reject(Change('t=0,%2C', 't=0,'), 'empty token');
  Reject(Change('t=0,%2C', 't=0,%2c'), 'lowercase escaping');
  Reject(Change('t=0,%2C', 't=0,%41'), 'unnecessary escaping');
  Reject(Change('t=0,%2C', 't=0,%FF'), 'invalid UTF8 byte');
  Reject(Change('t=0,%2C', 't=0,%C0%80'), 'overlong UTF8');
  Reject(Change('t=0,%2C', 't=0,%ED%A0%80'), 'UTF8 surrogate');
  Reject(Change('t=0,%2C', 't=0,%F4%90%80%80'), 'code point above Unicode range');
  Reject(Change('t=0,%2C', 't=0,%E2%99'), 'truncated UTF8');
  Reject(Change('t=0,%2C', 't=0,+'), 'raw reserved character');
  Reject(Change('t=0,%2C', 't=0,%'), 'incomplete escape');
  Reject(Change('t=0,%2C', 't=0,,x'), 'extra palette field');
  Reject(Change('t=1,%F0%9F%8E%B5', 't=1,%2C'), 'duplicate palette');
  S := Change('palette=2', 'palette=3');
  Reject(Change('patterns=2', 't=2,unused'#10'patterns=2', S), 'unused palette token');
  Reject(Change('patterns=2', 'patterns=0'), 'empty patterns');
  Reject(Change('patterns=2', 'patterns=1025'), 'pattern count cap');
  Reject(Change('p=0,1,0,1', 'p=1,1,0,1'), 'unordered pattern');
  Reject(Change('p=0,1,0,1', 'p=0,0,0,1'), 'zero weight');
  Reject(Change('p=0,1,0,1', 'p=0,2,0,1'), 'raw weight total differs from origins');
  Reject(Change('p=0,1,0,1', 'p=0,2147483647,0,1'), 'raw weight sum overflow');
  Reject(Change('p=1,1,1,0', 'p=1,1,0,1'), 'duplicate payload');
  Reject(Change('p=0,1,0,1', 'p=0,1,0'), 'short payload before allocation');
  Reject(Change('p=0,1,0,1', 'p=0,1,0,1,0'), 'long payload before allocation');
  Reject(Change('p=0,1,0,1', 'p=0,1,2,1'), 'out-of-palette index');
  Reject(Change('relations=overlap', 'relations=24'), 'no serialized relation count');
  Reject(Change('relations=overlap', 'relations=overlap'#10'r=U,0,1'), 'no serialized relation record');
  Reject(Change('relations=overlap', 'relations=Overlap'), 'canonical relation spelling');
  Reject(GOLDEN + 'end'#10, 'trailing record');
  Reject(GOLDEN + #10, 'blank trailing line');
  Reject(Change('rank=3', 'rank=3'#13), 'CRLF');
  Reject(Change('rank=3', #9'rank=3'), 'leading whitespace');
  Reject(Change('rank=3', 'rank=3 '), 'trailing whitespace');
  for I := 0 to High(InvalidNumbers) do
  begin
    Reject(Change('samples=1', 'samples=' + InvalidNumbers[I]), 'hostile sample count');
    Reject(Change('s=0,1,1,2', 's=0,1,1,' + InvalidNumbers[I]), 'hostile source Z');
    Reject(Change('footprint=1,1,2', 'footprint=1,1,' + InvalidNumbers[I]), 'hostile footprint Z');
    Reject(Change('p=0,1,0,1', 'p=0,' + InvalidNumbers[I] + ',0,1'), 'hostile raw weight');
    Reject(Change('p=0,1,0,1', 'p=0,1,' + InvalidNumbers[I] + ',1'), 'hostile palette index');
  end;
  Reject(StringOfChar('x', WFC_PATTERN_3D_MAX_ENCODED_TEXT_LENGTH + 1), 'independent 16MiB text cap');
  Reject(StringOfChar(#10, WFC_PATTERN_3D_MAX_TEXT_LINE_COUNT + 1), 'line envelope before split');
end;

procedure TestLegacyAndNil;
var Old: TWfcOverlappingModel2D; OldText: String; Raised: Boolean;
begin
  Check((WFC_PATTERN_2D_TEXT_VERSION = 1) and (WFC_PATTERN_3D_TEXT_VERSION = 2),
    'old and volume text versions are independently named');
  Old := LearnOverlappingModel2D(Tokens(['A','B']), 2, 1, 1, 1, wmbWrap, wmsNone);
  try
    OldText := EncodeWfcPattern2DText(Old);
    Check((Pos('wfcp=1'#10'rank=2'#10, OldText) = 1) and
      (Pos(#10'relations=', OldText) > 0) and (Pos(#10'r=', OldText) > 0),
      'legacy model retains explicit version-one relation serialization');
    Reject(OldText, 'volume decoder does not relabel old 2D artifacts');
  finally Old.Free; end;
  Raised := False;
  try OldText := EncodeWfcPattern3DText(nil);
  except on E: EArgumentNilException do Raised := True; end;
  Check(Raised, 'nil encoder input fails explicitly');
end;

{$IFDEF PAS2JS}
procedure TestBrowserHostileInput;
var I: Integer; S: String;
begin
  for I := 0 to 7 do
  begin
    asm
      S = [null, undefined, false, true, 1, NaN, {}, []][I];
    end;
    Reject(S, 'JavaScript caller must provide a real string');
  end;
  Reject(Change('t=0,%2C', 't=0,' + #$D800), 'raw lone surrogate is not ASCII');
end;
{$ENDIF}

begin
  try
    TestGoldenAndOwnership;
    TestLearningAndSymmetry;
    TestExactIdentity;
    TestDerivedKeyPreflight;
    TestHostileText;
    TestLegacyAndNil;
    {$IFDEF PAS2JS}TestBrowserHostileInput;{$ENDIF}
    WriteLn('Volume pattern text checks: ', Checks);
  except
    on E: Exception do
    begin
      {$IFDEF PAS2JS}document.body.setAttribute('data-self-test-message', E.Message);{$ENDIF}
      WriteLn('FAIL: ', E.ClassName, ': ', E.Message); Halt(1);
    end;
  end;
end.
