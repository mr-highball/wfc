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
program LearnTiles;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_learn,
  wfc_model_text;

const
  TRAINING_WIDTH = 5;
  TRAINING_HEIGHT = 5;
  OUTPUT_WIDTH = 24;
  OUTPUT_HEIGHT = 12;
  DEFAULT_SEED = TGraphSeed($4C454152);
  TRAINING_BOUNDARY = wmbWrap;
  TRAINING_SYMMETRY = wmsD4;
  TRAINING_ROWS: array[0..TRAINING_HEIGHT - 1] of String = (
    '~~~~~',
    '~...~',
    '~.T.~',
    '~...~',
    '~~~~~'
  );

type
  ELearnTiles = class(Exception);

function ParseSeed: TGraphSeed;
var
  LParsed: QWord;
  LText: String;
begin
  if ParamCount = 0 then
    Exit(DEFAULT_SEED);
  if ParamCount <> 1 then
    raise EConvertError.Create(
      'usage: LearnTiles [unsigned-32-bit-seed]');

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

function BuildTrainingTokens: TWfcModelTokens;
var
  X: Integer;
  Y: Integer;
begin
  Result := nil;
  SetLength(Result, TRAINING_WIDTH * TRAINING_HEIGHT);
  for Y := 0 to TRAINING_HEIGHT - 1 do
  begin
    if Length(TRAINING_ROWS[Y]) <> TRAINING_WIDTH then
      raise ELearnTiles.CreateFmt(
        'training row %d has %d glyphs; expected %d',
        [Y, Length(TRAINING_ROWS[Y]), TRAINING_WIDTH]);
    for X := 0 to TRAINING_WIDTH - 1 do
      Result[Y * TRAINING_WIDTH + X] :=
        TWfcModelToken(Copy(TRAINING_ROWS[Y], X + 1, 1));
  end;
end;

function ModelDirectionToGraphDirection(
  const ADirection: TWfcModelDirection): TGraphDirection;
begin
  case ADirection of
    wmdNorth:
      Result := gdNorth;
    wmdEast:
      Result := gdEast;
    wmdSouth:
      Result := gdSouth;
    wmdWest:
      Result := gdWest;
  else
    raise ERangeError.Create('unknown learned-model direction');
  end;
end;

function DirectionName(const ADirection: TWfcModelDirection): String;
begin
  case ADirection of
    wmdNorth:
      Result := 'north';
    wmdEast:
      Result := 'east';
    wmdSouth:
      Result := 'south';
    wmdWest:
      Result := 'west';
  else
    Result := 'unknown';
  end;
end;

function GraphValueToModelToken(
  const AValue: TGraphValue): TWfcModelToken;
begin
  Result := UTF8Encode(UnicodeString(AValue));
end;

function ModelTokenForDisplay(
  const AToken: TWfcModelToken): String;
begin
  Result := String(UTF8Decode(AToken));
end;

procedure VerifyRepresentable(const AModel: TWfcModel);
var
  D: TWfcModelDirection;
  LSource: Integer;
  LTarget: Integer;
  LHasSupport: Boolean;
begin
  for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    if D in AModel.Directions then
      for LSource := 0 to AModel.ValueCount - 1 do
      begin
        LHasSupport := False;
        for LTarget := 0 to AModel.ValueCount - 1 do
          if AModel.RelationCount(D, LSource, LTarget) > 0 then
          begin
            LHasSupport := True;
            Break;
          end;
        if not LHasSupport then
          raise ELearnTiles.CreateFmt(
            'learned token "%s" has no %s support; the graph adapter cannot represent an empty directional row',
            [ModelTokenForDisplay(AModel.TokenAt(LSource)),
             DirectionName(D)]);
      end;
end;

function VerifyGeneratedWorld(const AGraph: TGraph;
  const AModel: TWfcModel): Integer;
var
  D: TWfcModelDirection;
  LDirection: TGraphDirection;
  LEntry: TGraphEntry;
  LNeighbor: TGraphEntry;
  LSource: Integer;
  LTarget: Integer;
  X: Integer;
  Y: Integer;
begin
  Result := 0;
  for Y := 0 to OUTPUT_HEIGHT - 1 do
    for X := 0 to OUTPUT_WIDTH - 1 do
    begin
      LEntry := AGraph.Entry[X, Y, 0];
      if LEntry.Empty then
        raise ELearnTiles.CreateFmt(
          'validation found an empty generated cell at (%d,%d)', [X, Y]);
      LSource := AModel.FindToken(GraphValueToModelToken(LEntry.Value));
      if LSource < 0 then
        raise ELearnTiles.CreateFmt(
          'validation found unknown token "%s" at (%d,%d)',
          [LEntry.Value, X, Y]);

      for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
        if D in AModel.Directions then
        begin
          LDirection := ModelDirectionToGraphDirection(D);
          LNeighbor := LEntry.Neighbor[LDirection];
          if not Assigned(LNeighbor) then
            raise ELearnTiles.CreateFmt(
              'validation found a missing wrapped %s neighbor at (%d,%d)',
              [DirectionName(D), X, Y]);
          LTarget := AModel.FindToken(
            GraphValueToModelToken(LNeighbor.Value));
          if LTarget < 0 then
            raise ELearnTiles.CreateFmt(
              'validation found unknown %s-neighbor token "%s" at (%d,%d)',
              [DirectionName(D), LNeighbor.Value, X, Y]);
          if AModel.RelationCount(D, LSource, LTarget) <= 0 then
            raise ELearnTiles.CreateFmt(
              'validation rejected "%s" -> "%s" to the %s at (%d,%d)',
              [LEntry.Value, LNeighbor.Value, DirectionName(D), X, Y]);
          Inc(Result);
        end;
    end;
end;

procedure RenderWorld(const AGraph: TGraph);
var
  X: Integer;
  Y: Integer;
begin
  for Y := 0 to OUTPUT_HEIGHT - 1 do
  begin
    for X := 0 to OUTPUT_WIDTH - 1 do
      Write(AGraph.Entry[X, Y, 0].Value);
    WriteLn;
  end;
end;

procedure PrintModel(const AModel: TWfcModel;
  const ACanonicalLength: Integer);
var
  I: Integer;
begin
  WriteLn('Learning algorithm version: ', WFC_LEARN_ALGORITHM_VERSION);
  WriteLn('Latest model text version: ', WFC_MODEL_TEXT_VERSION);
  WriteLn('Training: ', TRAINING_WIDTH, 'x', TRAINING_HEIGHT,
    ', boundary=wrap, symmetry=d4');
  WriteLn('Values: ', AModel.ValueCount);
  for I := 0 to AModel.ValueCount - 1 do
    WriteLn('  "', ModelTokenForDisplay(AModel.TokenAt(I)),
      '" raw weight=', AModel.WeightAt(I));
  WriteLn('Canonical model bytes: ', ACanonicalLength);
end;

procedure Run;
var
  LCanonical: String;
  LCheckedRelations: Integer;
  LGraph: TGraph;
  LLearnedModel: TWfcModel;
  LOptions: TGraphSolveOptions;
  LReplayModel: TWfcModel;
  LReport: TGraphSolveReport;
  LRow: Integer;
  LSeed: TGraphSeed;
  LTrainingTokens: TWfcModelTokens;
begin
  LGraph := nil;
  LLearnedModel := nil;
  LReplayModel := nil;
  LSeed := ParseSeed;
  LTrainingTokens := BuildTrainingTokens;
  try
    LLearnedModel := LearnModel2D(LTrainingTokens, TRAINING_WIDTH,
      TRAINING_HEIGHT, TRAINING_BOUNDARY, TRAINING_SYMMETRY);
    VerifyRepresentable(LLearnedModel);

    LCanonical := EncodeWfcModelText(LLearnedModel);
    LReplayModel := DecodeWfcModelText(LCanonical);
    if EncodeWfcModelText(LReplayModel) <> LCanonical then
      raise ELearnTiles.Create('canonical model round trip changed bytes');

    LGraph := TGraph.Create;
    LGraph.Seed := LSeed;
    LGraph.WrapNeighbors := True;
    LGraph.Reshape(OUTPUT_WIDTH, OUTPUT_HEIGHT, 1);
    ApplyModelToGraph(LReplayModel, LGraph);

    LOptions := DefaultGraphSolveOptions;
    LOptions.MaxBacktracks := 16384;
    if not LGraph.TrySolve(LOptions, LReport) then
      raise ELearnTiles.CreateFmt(
        'solve failed: status=%d pass=%d contradiction=%d entry=%d neighbor=%d',
        [Ord(LReport.Status), LReport.FailedPassIndex,
         Ord(LReport.Contradiction.Kind), LReport.Contradiction.EntryIndex,
         LReport.Contradiction.NeighborIndex]);

    LCheckedRelations := VerifyGeneratedWorld(LGraph, LReplayModel);

    WriteLn('LearnTiles: deterministic sample -> model -> text -> graph');
    PrintModel(LReplayModel, Length(LCanonical));
    WriteLn('Seed: ', LGraph.Seed);
    WriteLn('Random algorithm version: ', LReport.RandomAlgorithmVersion);
    WriteLn('Solver algorithm version: ', LReport.SolverAlgorithmVersion);
    WriteLn('Canonical round trip: verified');
    WriteLn('Validated cardinal relations: ', LCheckedRelations);
    if Length(LReport.Passes) > 0 then
      WriteLn('Solve: decisions=', LReport.Passes[0].Decisions,
        ' propagations=', LReport.Passes[0].Propagations,
        ' contradictions=', LReport.Passes[0].Contradictions,
        ' backtracks=', LReport.Passes[0].Backtracks);
    WriteLn;
    WriteLn('Training sample:');
    for LRow := 0 to TRAINING_HEIGHT - 1 do
      WriteLn(TRAINING_ROWS[LRow]);
    WriteLn;
    WriteLn('Generated ', OUTPUT_WIDTH, 'x', OUTPUT_HEIGHT, ' world:');
    RenderWorld(LGraph);
    WriteLn;
    WriteLn('Legend: ~=water  .=ground  T=tree');
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
      WriteLn('LearnTiles error: ', E.Message);
      Halt(1);
    end;
  end;
end.
