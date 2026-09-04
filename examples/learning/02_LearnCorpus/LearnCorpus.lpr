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
program LearnCorpus;

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
  wfc_model_text;

const
  OUTPUT_LENGTH = 48;
  DEFAULT_SEED = TGraphSeed($434F5250);
  TRAINING_BOUNDARY = wmbWrap;
  SAMPLE_COAST: array[0..7] of String = (
    'sea', 'shore', 'dune', 'meadow',
    'forest', 'meadow', 'town', 'shore'
  );
  SAMPLE_INLAND: array[0..10] of String = (
    'hill', 'forest', 'hill', 'meadow', 'shore', 'sea',
    'shore', 'town', 'meadow', 'town', 'road'
  );

type
  ELearnCorpus = class(Exception);

function ParseSeed: TGraphSeed;
var
  LParsed: QWord;
  LText: String;
begin
  if ParamCount = 0 then
    Exit(DEFAULT_SEED);
  if ParamCount <> 1 then
    raise EConvertError.Create(
      'usage: LearnCorpus [unsigned-32-bit-seed]');

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

function MakeTokens(const AValues: array of String): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := TWfcModelToken(AValues[I]);
end;

function BuildCorpus: TWfcLearnSamples;
var
  LTokens: TWfcModelTokens;
begin
  Result := nil;
  SetLength(Result, 2);
  LTokens := MakeTokens(SAMPLE_COAST);
  Result[0] := MakeLearnSample1D(LTokens);
  LTokens := MakeTokens(SAMPLE_INLAND);
  Result[1] := MakeLearnSample1D(LTokens);
end;

function GraphValueToModelToken(
  const AValue: TGraphValue): TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(AValue);
  {$ELSE}
  Result := UTF8Encode(UnicodeString(AValue));
  {$ENDIF}
end;

function ModelTokenForDisplay(
  const AToken: TWfcModelToken): String;
begin
  {$IFDEF PAS2JS}
  Result := String(AToken);
  {$ELSE}
  Result := String(UTF8Decode(AToken));
  {$ENDIF}
end;

function DirectionName(
  const ADirection: TWfcModelDirection): String;
begin
  case ADirection of
    wmdEast:
      Result := 'east';
    wmdWest:
      Result := 'west';
  else
    Result := 'inactive';
  end;
end;

function GraphDirection(
  const ADirection: TWfcModelDirection): TGraphDirection;
begin
  case ADirection of
    wmdEast:
      Result := gdEast;
    wmdWest:
      Result := gdWest;
  else
    raise ERangeError.Create('expected an east or west direction');
  end;
end;

procedure VerifyRepresentable(const AModel: TWfcModel);
var
  D: TWfcModelDirection;
  LSource: Integer;
  LTarget: Integer;
  LHasSupport: Boolean;
begin
  for D := wmdEast to wmdWest do
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
          raise ELearnCorpus.CreateFmt(
            'learned token "%s" has no %s support',
            [ModelTokenForDisplay(AModel.TokenAt(LSource)),
             DirectionName(D)]);
      end;
end;

procedure RequireRelationCount(const AModel: TWfcModel;
  const ADirection: TWfcModelDirection;
  const ASourceToken, ATargetToken: String; const AExpected: Integer);
var
  LActual: Integer;
  LSource: Integer;
  LTarget: Integer;
begin
  LSource := AModel.FindToken(TWfcModelToken(ASourceToken));
  LTarget := AModel.FindToken(TWfcModelToken(ATargetToken));
  if (LSource < 0) or (LTarget < 0) then
    raise ELearnCorpus.CreateFmt(
      'boundary validation could not find "%s" or "%s"',
      [ASourceToken, ATargetToken]);
  LActual := AModel.RelationCount(ADirection, LSource, LTarget);
  if LActual <> AExpected then
    raise ELearnCorpus.CreateFmt(
      'boundary validation expected %s "%s" -> "%s" count %d; got %d',
      [DirectionName(ADirection), ASourceToken, ATargetToken,
       AExpected, LActual]);
end;

procedure VerifyCorpusBoundaryIsolation(const AModel: TWfcModel);
begin
  { Local wrapped boundaries and their reciprocal observations. }
  RequireRelationCount(AModel, wmdEast, 'shore', 'sea', 2);
  RequireRelationCount(AModel, wmdWest, 'sea', 'shore', 2);
  RequireRelationCount(AModel, wmdEast, 'road', 'hill', 1);
  RequireRelationCount(AModel, wmdWest, 'hill', 'road', 1);

  { These pairs would exist if the two samples were concatenated first. }
  RequireRelationCount(AModel, wmdEast, 'shore', 'hill', 0);
  RequireRelationCount(AModel, wmdWest, 'hill', 'shore', 0);
  RequireRelationCount(AModel, wmdEast, 'road', 'sea', 0);
  RequireRelationCount(AModel, wmdWest, 'sea', 'road', 0);
end;

function VerifyGeneratedSequence(const AGraph: TGraph;
  const AModel: TWfcModel): Integer;
const
  CHECK_DIRECTIONS: array[0..1] of TWfcModelDirection = (
    wmdEast, wmdWest
  );
var
  D: TWfcModelDirection;
  I: Integer;
  LDirectionIndex: Integer;
  LEntry: TGraphEntry;
  LNeighbor: TGraphEntry;
  LSource: Integer;
  LTarget: Integer;
begin
  Result := 0;
  for I := 0 to OUTPUT_LENGTH - 1 do
  begin
    LEntry := AGraph.Entry[I, 0, 0];
    if LEntry.Empty then
      raise ELearnCorpus.CreateFmt(
        'validation found an empty generated token at index %d', [I]);
    LSource := AModel.FindToken(GraphValueToModelToken(LEntry.Value));
    if LSource < 0 then
      raise ELearnCorpus.CreateFmt(
        'validation found unknown token "%s" at index %d',
        [LEntry.Value, I]);

    for LDirectionIndex := Low(CHECK_DIRECTIONS) to
      High(CHECK_DIRECTIONS) do
    begin
      D := CHECK_DIRECTIONS[LDirectionIndex];
      LNeighbor := LEntry.Neighbor[GraphDirection(D)];
      if not Assigned(LNeighbor) then
        raise ELearnCorpus.CreateFmt(
          'validation found a missing wrapped %s neighbor at index %d',
          [DirectionName(D), I]);
      LTarget := AModel.FindToken(
        GraphValueToModelToken(LNeighbor.Value));
      if LTarget < 0 then
        raise ELearnCorpus.CreateFmt(
          'validation found unknown %s-neighbor token "%s" at index %d',
          [DirectionName(D), LNeighbor.Value, I]);
      if AModel.RelationCount(D, LSource, LTarget) <= 0 then
        raise ELearnCorpus.CreateFmt(
          'validation rejected "%s" -> "%s" to the %s at index %d',
          [LEntry.Value, LNeighbor.Value, DirectionName(D), I]);
      Inc(Result);
    end;
  end;
end;

procedure WriteTokens(const ATokens: TWfcModelTokens);
var
  I: Integer;
begin
  for I := 0 to Length(ATokens) - 1 do
  begin
    if I > 0 then
      Write(' | ');
    Write(ModelTokenForDisplay(ATokens[I]));
  end;
  WriteLn;
end;

procedure WriteGeneratedSequence(const AGraph: TGraph);
var
  I: Integer;
begin
  for I := 0 to OUTPUT_LENGTH - 1 do
  begin
    if I > 0 then
      Write(' | ');
    Write(AGraph.Entry[I, 0, 0].Value);
  end;
  WriteLn;
end;

procedure PrintModel(const AModel: TWfcModel;
  const ACanonicalLength: Integer);
var
  I: Integer;
  LShape: TWfcModelSampleShape;
begin
  WriteLn('Learning algorithm version: ', WFC_LEARN_ALGORITHM_VERSION);
  WriteLn('Corpus learning algorithm version: ',
    WFC_LEARN_CORPUS_ALGORITHM_VERSION);
  WriteLn('Model text version: ', WFC_MODEL_TEXT_VERSION, ' (wfcm=2)');
  WriteLn('Boundary: wrap');
  WriteLn('Samples: ', AModel.SampleCount);
  for I := 0 to AModel.SampleCount - 1 do
  begin
    LShape := AModel.SampleShapeAt(I);
    WriteLn('  sample ', I, ' shape=', LShape.Width, 'x', LShape.Height);
  end;
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
  LCorpus: TWfcLearnSamples;
  LGraph: TGraph;
  LLearnedModel: TWfcModel;
  LOptions: TGraphSolveOptions;
  LReplayModel: TWfcModel;
  LReport: TGraphSolveReport;
  LSeed: TGraphSeed;
begin
  LGraph := nil;
  LLearnedModel := nil;
  LReplayModel := nil;
  LSeed := ParseSeed;
  LCorpus := BuildCorpus;
  try
    LLearnedModel := LearnModel1DCorpus(LCorpus, TRAINING_BOUNDARY);
    VerifyRepresentable(LLearnedModel);
    VerifyCorpusBoundaryIsolation(LLearnedModel);

    LCanonical := EncodeWfcModelText(LLearnedModel);
    if Copy(LCanonical, 1, Length('wfcm=2' + #10)) <>
      ('wfcm=2' + #10) then
      raise ELearnCorpus.Create('corpus did not encode as canonical wfcm=2');
    LReplayModel := DecodeWfcModelText(LCanonical);
    if EncodeWfcModelText(LReplayModel) <> LCanonical then
      raise ELearnCorpus.Create('canonical model round trip changed bytes');

    LGraph := TGraph.Create;
    LGraph.Seed := LSeed;
    LGraph.WrapNeighbors := True;
    LGraph.Reshape(OUTPUT_LENGTH, 1, 1);
    ApplyModelToGraph(LReplayModel, LGraph);

    LOptions := DefaultGraphSolveOptions;
    LOptions.MaxBacktracks := 32768;
    if not LGraph.TrySolve(LOptions, LReport) then
      raise ELearnCorpus.CreateFmt(
        'solve failed: status=%d pass=%d contradiction=%d entry=%d neighbor=%d',
        [Ord(LReport.Status), LReport.FailedPassIndex,
         Ord(LReport.Contradiction.Kind), LReport.Contradiction.EntryIndex,
         LReport.Contradiction.NeighborIndex]);

    LCheckedRelations := VerifyGeneratedSequence(LGraph, LReplayModel);

    WriteLn('LearnCorpus: ordered samples -> wfcm=2 -> wrapped graph');
    PrintModel(LReplayModel, Length(LCanonical));
    WriteLn('Seed: ', LGraph.Seed);
    WriteLn('Random algorithm version: ', LReport.RandomAlgorithmVersion);
    WriteLn('Solver algorithm version: ', LReport.SolverAlgorithmVersion);
    WriteLn('Canonical round trip: verified');
    WriteLn('Independent sample boundaries: verified');
    WriteLn('Validated east/west relations: ', LCheckedRelations);
    if Length(LReport.Passes) > 0 then
      WriteLn('Solve: decisions=', LReport.Passes[0].Decisions,
        ' propagations=', LReport.Passes[0].Propagations,
        ' contradictions=', LReport.Passes[0].Contradictions,
        ' backtracks=', LReport.Passes[0].Backtracks);
    WriteLn;
    WriteLn('Training sample 0 (coast, ', Length(LCorpus[0].Tokens),
      'x1):');
    WriteTokens(LCorpus[0].Tokens);
    WriteLn('Training sample 1 (inland, ', Length(LCorpus[1].Tokens),
      'x1):');
    WriteTokens(LCorpus[1].Tokens);
    WriteLn;
    WriteLn('Generated wrapped sequence (', OUTPUT_LENGTH, ' tokens):');
    WriteGeneratedSequence(LGraph);
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
      WriteLn('LearnCorpus error: ', E.Message);
      {$IFDEF PAS2JS}
      TNJSProcess.exitCode := 1;
      {$ELSE}
      Halt(1);
      {$ENDIF}
    end;
  end;
end.
