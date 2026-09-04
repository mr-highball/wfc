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
program LearnPatterns;

{$mode delphi}{$H+}

uses
  SysUtils,
  {$IFDEF PAS2JS}
  NodeJSApp,
  NodeJS,
  {$ENDIF}
  wfc,
  wfc_model,
  wfc_learn,
  wfc_pattern2d,
  wfc_pattern2d_learn,
  wfc_pattern2d_text;

const
  PATTERN_WIDTH = 2;
  PATTERN_HEIGHT = 2;
  OUTPUT_WIDTH = 16;
  OUTPUT_HEIGHT = 10;
  DEFAULT_SEED = TGraphSeed($50415454);
  SEED_ZERO_SIGNATURE = '1:6D67D260';
  CRC32_POLYNOMIAL = Cardinal($EDB88320);
  HEX_DIGITS = '0123456789ABCDEF';
  SAMPLE_ZERO_WIDTH = 5;
  SAMPLE_ZERO_HEIGHT = 5;
  SAMPLE_ZERO_ROWS: array[0..SAMPLE_ZERO_HEIGHT - 1] of String = (
    '~~~~~',
    '~...~',
    '~.#.~',
    '~...~',
    '~~~~~'
  );
  SAMPLE_ONE_WIDTH = 7;
  SAMPLE_ONE_HEIGHT = 5;
  SAMPLE_ONE_ROWS: array[0..SAMPLE_ONE_HEIGHT - 1] of String = (
    '~~~~~~~',
    '~.....~',
    '~.###.~',
    '~.....~',
    '~~~~~~~'
  );

type
  ELearnPatterns = class(Exception);

function ParseSeed: TGraphSeed;
var
  LParsed: QWord;
  LText: String;
begin
  if ParamCount = 0 then
    Exit(DEFAULT_SEED);
  if ParamCount <> 1 then
    raise EConvertError.Create(
      'usage: LearnPatterns [unsigned-32-bit-seed]');
  LText := ParamStr(1);
  try
    LParsed := StrToQWord(LText);
  except
    on E: Exception do
      raise EConvertError.CreateFmt(
        'invalid seed "%s": expected an unsigned 32-bit integer', [LText]);
  end;
  if LParsed > High(TGraphSeed) then
    raise EConvertError.CreateFmt(
      'invalid seed "%s": maximum value is 4294967295', [LText]);
  Result := TGraphSeed(LParsed);
end;

function TokensFromRows(const ARows: array of String;
  const AWidth, AHeight: Integer): TWfcModelTokens;
var
  X: Integer;
  Y: Integer;
begin
  if Length(ARows) <> AHeight then
    raise ELearnPatterns.Create('training row count does not match shape');
  Result := nil;
  SetLength(Result, AWidth * AHeight);
  for Y := 0 to AHeight - 1 do
  begin
    if Length(ARows[Y]) <> AWidth then
      raise ELearnPatterns.CreateFmt(
        'training row %d has %d glyphs; expected %d',
        [Y, Length(ARows[Y]), AWidth]);
    for X := 0 to AWidth - 1 do
      Result[Y * AWidth + X] :=
        TWfcModelToken(Copy(ARows[Y], X + 1, 1));
  end;
end;

function BuildCorpus: TWfcLearnSamples;
begin
  Result := nil;
  SetLength(Result, 2);
  Result[0] := MakeLearnSample2D(
    TokensFromRows(SAMPLE_ZERO_ROWS, SAMPLE_ZERO_WIDTH,
      SAMPLE_ZERO_HEIGHT), SAMPLE_ZERO_WIDTH, SAMPLE_ZERO_HEIGHT);
  Result[1] := MakeLearnSample2D(
    TokensFromRows(SAMPLE_ONE_ROWS, SAMPLE_ONE_WIDTH,
      SAMPLE_ONE_HEIGHT), SAMPLE_ONE_WIDTH, SAMPLE_ONE_HEIGHT);
end;

function ModelTokenForDisplay(const AToken: TWfcModelToken): String;
begin
  {$IFDEF PAS2JS}
  Result := String(AToken);
  {$ELSE}
  Result := String(UTF8Decode(AToken));
  {$ENDIF}
end;

function ModelTokenToGraphValue(
  const AToken: TWfcModelToken): TGraphValue;
begin
  {$IFDEF PAS2JS}
  Result := TGraphValue(AToken);
  {$ELSE}
  Result := TGraphValue(UTF8Decode(AToken));
  {$ENDIF}
end;

function FindPatternContaining(const AModel: TWfcOverlappingModel2D;
  const AToken: TWfcModelToken): Integer;
var
  LPalette: Integer;
  P: Integer;
  X: Integer;
  Y: Integer;
begin
  LPalette := AModel.FindPaletteToken(AToken);
  if LPalette < 0 then
    raise ELearnPatterns.CreateFmt(
      'palette does not contain required anchor token "%s"',
      [ModelTokenForDisplay(AToken)]);
  for P := 0 to AModel.PatternCount - 1 do
    for Y := 0 to AModel.PatternHeight - 1 do
      for X := 0 to AModel.PatternWidth - 1 do
        if AModel.PatternPaletteIndexAt(P, X, Y) = LPalette then
          Exit(P);
  raise ELearnPatterns.CreateFmt(
    'no pattern contains required anchor token "%s"',
    [ModelTokenForDisplay(AToken)]);
end;

function UpdateCrc32(const ACrc, AByte: Cardinal): Cardinal;
var
  I: Integer;
begin
  Result := ACrc xor (AByte and Cardinal($FF));
  for I := 0 to 7 do
    if (Result and Cardinal(1)) <> 0 then
      Result := (Result shr 1) xor CRC32_POLYNOMIAL
    else
      Result := Result shr 1;
end;

procedure MixCardinal(var ACrc: Cardinal; const AValue: Cardinal);
begin
  ACrc := UpdateCrc32(ACrc, AValue);
  ACrc := UpdateCrc32(ACrc, AValue shr 8);
  ACrc := UpdateCrc32(ACrc, AValue shr 16);
  ACrc := UpdateCrc32(ACrc, AValue shr 24);
end;

function CardinalHex(const AValue: Cardinal): String;
var
  I: Integer;
  LValue: Cardinal;
begin
  SetLength(Result, 8);
  LValue := AValue;
  for I := 8 downto 1 do
  begin
    Result[I] := HEX_DIGITS[Integer(LValue and Cardinal($F)) + 1];
    LValue := LValue shr 4;
  end;
end;

function ProjectionSignature(const AModel: TWfcOverlappingModel2D;
  const AOutput: TWfcTokenGrid2D): String;
var
  I: Integer;
  LHash: Cardinal;
  LPalette: Integer;
begin
  LHash := Cardinal($FFFFFFFF);
  MixCardinal(LHash, Cardinal($50434657)); { ASCII "WFCP". }
  MixCardinal(LHash, WFC_OVERLAPPING_2D_PROJECTION_VERSION);
  MixCardinal(LHash, Cardinal(AOutput.Width));
  MixCardinal(LHash, Cardinal(AOutput.Height));
  for I := 0 to Length(AOutput.Tokens) - 1 do
  begin
    LPalette := AModel.FindPaletteToken(AOutput.Tokens[I]);
    if LPalette < 0 then
      raise ELearnPatterns.CreateFmt(
        'projected token %d is outside the learned palette', [I]);
    MixCardinal(LHash, Cardinal(LPalette));
  end;
  Result := IntToStr(WFC_OVERLAPPING_2D_PROJECTION_VERSION)
    + ':' + CardinalHex(not LHash);
end;

procedure RenderProjection(const AOutput: TWfcTokenGrid2D);
var
  X: Integer;
  Y: Integer;
begin
  for Y := 0 to AOutput.Height - 1 do
  begin
    for X := 0 to AOutput.Width - 1 do
      Write(ModelTokenForDisplay(
        AOutput.Tokens[Y * AOutput.Width + X]));
    WriteLn;
  end;
end;

procedure PrintModel(const AModel: TWfcOverlappingModel2D;
  const ACanonicalLength: Integer);
var
  I: Integer;
  LShape: TWfcModelSampleShape;
begin
  WriteLn('Overlapping learner version: ',
    WFC_OVERLAPPING_2D_ALGORITHM_VERSION);
  WriteLn('Pattern text version: ', WFC_PATTERN_2D_TEXT_VERSION,
    ' (wfcp=1)');
  WriteLn('Footprint: ', AModel.PatternWidth, 'x', AModel.PatternHeight);
  WriteLn('Source boundary: wrap');
  WriteLn('Symmetry: d4');
  WriteLn('Sources: ', AModel.SourceCount);
  for I := 0 to AModel.SourceCount - 1 do
  begin
    LShape := AModel.SourceShapeAt(I);
    WriteLn('  sample ', I, ' shape=', LShape.Width, 'x', LShape.Height);
  end;
  WriteLn('Palette tokens: ', AModel.PaletteCount);
  for I := 0 to AModel.PaletteCount - 1 do
    WriteLn('  ', I, '="',
      ModelTokenForDisplay(AModel.PaletteTokenAt(I)), '"');
  WriteLn('Unique patterns: ', AModel.PatternCount);
  WriteLn('Canonical pattern bytes: ', ACanonicalLength);
end;

procedure Run;
var
  LAnchorPattern: Integer;
  LAssignmentReport: TWfcOverlapping2DValidationReport;
  LCanonical: String;
  LCorpus: TWfcLearnSamples;
  LGraph: TGraph;
  LGrid: TWfcPatternGrid2D;
  LLearnedModel: TWfcOverlappingModel2D;
  LOptions: TGraphSolveOptions;
  LOutput: TWfcTokenGrid2D;
  LProjectionReport: TWfcOverlapping2DValidationReport;
  LReplayModel: TWfcOverlappingModel2D;
  LReport: TGraphSolveReport;
  LSeed: TGraphSeed;
  LSignature: String;
begin
  LGraph := nil;
  LLearnedModel := nil;
  LReplayModel := nil;
  LSeed := ParseSeed;
  LCorpus := BuildCorpus;
  try
    LLearnedModel := LearnOverlappingModel2DCorpus(LCorpus,
      PATTERN_WIDTH, PATTERN_HEIGHT, wmbWrap, wmsD4);
    LCanonical := EncodeWfcPattern2DText(LLearnedModel);
    LReplayModel := DecodeWfcPattern2DText(LCanonical);
    if EncodeWfcPattern2DText(LReplayModel) <> LCanonical then
      raise ELearnPatterns.Create(
        'canonical pattern round trip changed bytes');

    LGraph := TGraph.Create;
    LGraph.Seed := LSeed;
    LGraph.WrapNeighbors := True;
    LGraph.Reshape(OUTPUT_WIDTH, OUTPUT_HEIGHT, 1);
    ApplyOverlappingModel2DToGraph(LReplayModel, LGraph);

    { A caller-owned latent anchor guarantees the generated field contains
      the rare rock token without changing the learned model. }
    LAnchorPattern := FindPatternContaining(LReplayModel, '#');
    LGraph.Entry[OUTPUT_WIDTH div 2, OUTPUT_HEIGHT div 2, 0].Value :=
      ModelTokenToGraphValue(LReplayModel.PatternKeyAt(LAnchorPattern));

    LOptions := DefaultGraphSolveOptions;
    LOptions.MaxBacktracks := 65536;
    if not LGraph.TrySolve(LOptions, LReport) then
      raise ELearnPatterns.CreateFmt(
        'solve failed: status=%d pass=%d contradiction=%d entry=%d neighbor=%d',
        [Ord(LReport.Status), LReport.FailedPassIndex,
         Ord(LReport.Contradiction.Kind), LReport.Contradiction.EntryIndex,
         LReport.Contradiction.NeighborIndex]);
    if not CaptureSolvedPatternGrid2D(LReplayModel, LGraph, 0,
        LGrid, LAssignmentReport) then
      raise ELearnPatterns.Create(DescribeOverlapping2DIssue(
        LAssignmentReport.Issue));
    if not TryProjectOverlappingPatternGrid2D(LReplayModel, LGrid,
        LOutput, LProjectionReport) then
      raise ELearnPatterns.Create(DescribeOverlapping2DIssue(
        LProjectionReport.Issue));
    if not ValidateOverlappingProjection2D(LReplayModel, LGrid,
        LOutput, LProjectionReport) then
      raise ELearnPatterns.Create(DescribeOverlapping2DIssue(
        LProjectionReport.Issue));
    LSignature := ProjectionSignature(LReplayModel, LOutput);
    if (LSeed = 0) and (LSignature <> SEED_ZERO_SIGNATURE) then
      raise ELearnPatterns.CreateFmt(
        'seed-zero signature changed [%s <> %s]',
        [LSignature, SEED_ZERO_SIGNATURE]);

    WriteLn('LearnPatterns: corpus -> 2x2 patterns -> wfcp=1 -> projection');
    PrintModel(LReplayModel, Length(LCanonical));
    WriteLn('Seed: ', LGraph.Seed);
    WriteLn('Random algorithm version: ', LReport.RandomAlgorithmVersion);
    WriteLn('Solver algorithm version: ', LReport.SolverAlgorithmVersion);
    WriteLn('Canonical round trip: verified');
    WriteLn('Validated latent relations: ',
      LAssignmentReport.CheckedRelations);
    WriteLn('Validated projected contributions: ',
      LProjectionReport.CheckedProjectionCells);
    WriteLn('Projection signature: ', LSignature);
    if Length(LReport.Passes) > 0 then
      WriteLn('Solve: decisions=', LReport.Passes[0].Decisions,
        ' propagations=', LReport.Passes[0].Propagations,
        ' contradictions=', LReport.Passes[0].Contradictions,
        ' backtracks=', LReport.Passes[0].Backtracks);
    WriteLn;
    WriteLn('Generated wrapped ', LOutput.Width, 'x', LOutput.Height,
      ' projected field:');
    RenderProjection(LOutput);
    WriteLn;
    WriteLn('Legend: ~=water  .=ground  #=rock');
  finally
    LGraph.Free;
    LReplayModel.Free;
    LLearnedModel.Free;
  end;
end;

begin
  try
    Run;
  except
    on E: Exception do
    begin
      WriteLn('LearnPatterns error: ', E.Message);
      {$IFDEF PAS2JS}
      TNJSProcess.exitCode := 1;
      {$ELSE}
      Halt(1);
      {$ENDIF}
    end;
  end;
end.
