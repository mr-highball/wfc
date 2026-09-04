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
program LearnSequence;

{$mode delphi}{$H+}

uses
  SysUtils,
  {$IFDEF PAS2JS}
  NodeJSApp,
  NodeJS,
  {$ENDIF}
  wfc,
  wfc_model,
  wfc_sequence,
  wfc_sequence_learn,
  wfc_sequence_graph,
  wfc_sequence_text;

const
  DEFAULT_SEED = TGraphSeed(0);

type
  ELearnSequence = class(Exception);

function TokensOf(const AValues: array of String): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := TWfcModelToken(AValues[I]);
end;

function BuildCorpus: TWfcSequenceSamples;
begin
  Result := nil;
  SetLength(Result, 2);
  Result[0] := MakeWfcSequenceSample(TokensOf(['A', 'B', 'A']));
  Result[1] := MakeWfcSequenceSample(TokensOf(['A', 'C', 'A']));
end;

function ParseSeed: TGraphSeed;
var
  I: Integer;
  LDigit: TGraphSeed;
  LParsed: TGraphSeed;
  LText: String;
begin
  if ParamCount = 0 then
    Exit(DEFAULT_SEED);
  if ParamCount <> 1 then
    raise EConvertError.Create(
      'usage: LearnSequence [unsigned-32-bit-seed]');
  LText := ParamStr(1);
  if LText = '' then
    raise EConvertError.Create(
      'invalid seed: expected an unsigned 32-bit integer');
  LParsed := 0;
  for I := 1 to Length(LText) do
  begin
    if not (LText[I] in ['0'..'9']) then
      raise EConvertError.CreateFmt(
        'invalid seed "%s": expected an unsigned 32-bit integer',
        [LText]);
    LDigit := TGraphSeed(Ord(LText[I]) - Ord('0'));
    if LParsed > (High(TGraphSeed) - LDigit) div 10 then
      raise EConvertError.CreateFmt(
        'invalid seed "%s": maximum value is 4294967295', [LText]);
    LParsed := (LParsed * 10) + LDigit;
  end;
  Result := LParsed;
end;

function TokenForDisplay(const AToken: TWfcModelToken): String;
begin
  {$IFDEF PAS2JS}
  Result := String(AToken);
  {$ELSE}
  Result := String(UTF8Decode(AToken));
  {$ENDIF}
end;

function TokensMatch(const AActual: TWfcModelTokens;
  const AExpected: array of String): Boolean;
var
  I: Integer;
begin
  if Length(AActual) <> Length(AExpected) then
    Exit(False);
  for I := 0 to Length(AActual) - 1 do
    if AActual[I] <> TWfcModelToken(AExpected[I]) then
      Exit(False);
  Result := True;
end;

procedure WriteTokens(const ATokens: TWfcModelTokens);
var
  I: Integer;
begin
  for I := 0 to Length(ATokens) - 1 do
  begin
    if I > 0 then
      Write(' ');
    Write(TokenForDisplay(ATokens[I]));
  end;
  WriteLn;
end;

procedure RequireValidation(const AModel: TWfcSequenceModel;
  const AGenerated: TWfcGeneratedSequence;
  const ALabel: String);
var
  LReport: TWfcSequenceGraphValidationReport;
begin
  if not ValidateSequenceStatePath(AModel, AGenerated.StateIndices,
      AGenerated.Boundary, LReport) then
    raise ELearnSequence.Create(ALabel + ': ' +
      DescribeSequenceGraphIssue(LReport.Issue));
end;

function SolveSeeded(const AModel: TWfcSequenceModel;
  const ASeed: TGraphSeed; out AReport: TGraphSolveReport):
  TWfcGeneratedSequence;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LValidation: TWfcSequenceGraphValidationReport;
begin
  Result := Default(TWfcGeneratedSequence);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(3, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.Seed := ASeed;
    ApplySequenceModelToGraph(AModel, LGraph);
    LOptions := DefaultGraphSolveOptions;
    if not LGraph.TrySolve(LOptions, AReport) then
      raise ELearnSequence.Create('seeded sequence solve failed');
    if not CaptureSolvedSequence(AModel, LGraph, Result,
        LValidation) then
      raise ELearnSequence.Create('seeded capture failed: ' +
        DescribeSequenceGraphIssue(LValidation.Issue));
    RequireValidation(AModel, Result, 'seeded validation failed');
  finally
    LGraph.Free;
  end;
end;

function SolveProjectedPipeline(const AModel: TWfcSequenceModel;
  const ASeed: TGraphSeed; out AReport: TGraphSolveReport):
  TWfcGeneratedSequence;
var
  LGraph: TGraph;
  LOptions: TGraphSolveOptions;
  LValidation: TWfcSequenceGraphValidationReport;
begin
  Result := Default(TWfcGeneratedSequence);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(3, 1, 1);
    LGraph.WrapNeighbors := False;
    LGraph.Seed := ASeed;

    LGraph.CurrentPass := 'tokens';
    LGraph.AddValue('A');
    LGraph.AddValue('B');
    LGraph.AddValue('C');
    LGraph.SetAllowedValues(0, 0, 0, 'A');
    LGraph.SetAllowedValues(1, 0, 0, 'C');
    LGraph.SetAllowedValues(2, 0, 0, 'A');

    LGraph.SwitchToPass('latent');
    ApplySequenceModelToGraph(AModel, LGraph);
    RequireSequenceProjectionFromTokenPass(AModel, LGraph, 'tokens');

    LGraph.SwitchToPass('classes');
    LGraph.AddValue('outer');
    LGraph.AddValue('branch');
    RequireProjectedSequenceFromPass(AModel, LGraph, 'outer',
      'latent', TokensOf(['A']));
    RequireProjectedSequenceFromPass(AModel, LGraph, 'branch',
      'latent', TokensOf(['B', 'C']));

    LOptions := DefaultGraphSolveOptions;
    if not LGraph.TrySolve(LOptions, AReport) then
      raise ELearnSequence.Create('projected pipeline solve failed');
    if not CaptureSolvedSequence(AModel, LGraph.PassGraph[1],
        Result, LValidation) then
      raise ELearnSequence.Create('projected capture failed: ' +
        DescribeSequenceGraphIssue(LValidation.Issue));
    RequireValidation(AModel, Result, 'projected validation failed');
    if (LGraph.PassGraph[2].Entry[0, 0, 0].Value <> 'outer') or
        (LGraph.PassGraph[2].Entry[1, 0, 0].Value <> 'branch') or
        (LGraph.PassGraph[2].Entry[2, 0, 0].Value <> 'outer') then
      raise ELearnSequence.Create(
        'downstream projected classes did not match outer branch outer');
  finally
    LGraph.Free;
  end;
end;

procedure Run;
var
  LCanonical: String;
  LCorpus: TWfcSequenceSamples;
  LLearned: TWfcSequenceModel;
  LPipeline: TWfcGeneratedSequence;
  LPipelineReport: TGraphSolveReport;
  LReplay: TWfcSequenceModel;
  LSeed: TGraphSeed;
  LSeeded: TWfcGeneratedSequence;
  LSeededReport: TGraphSolveReport;
begin
  LLearned := nil;
  LReplay := nil;
  LSeed := ParseSeed;
  LCorpus := BuildCorpus;
  try
    LLearned := LearnSequenceModelCorpus(LCorpus, 2);
    LCanonical := EncodeWfcSequenceText(LLearned);
    LReplay := DecodeWfcSequenceText(LCanonical);
    if EncodeWfcSequenceText(LReplay) <> LCanonical then
      raise ELearnSequence.Create(
        'canonical sequence model round trip changed bytes');

    LSeeded := SolveSeeded(LReplay, LSeed, LSeededReport);
    if (LSeed = 0) and
        (not TokensMatch(LSeeded.Tokens, ['A', 'B', 'A'])) then
      raise ELearnSequence.Create('seed zero replay changed');
    LPipeline := SolveProjectedPipeline(LReplay, LSeed,
      LPipelineReport);
    if not TokensMatch(LPipeline.Tokens, ['A', 'C', 'A']) then
      raise ELearnSequence.Create('projection pipeline changed');

    WriteLn('LearnSequence: bounded tokens -> wfcs=1 -> latent graph');
    WriteLn('Order: ', LReplay.Order);
    WriteLn('Samples: ', LReplay.SampleCount);
    WriteLn('Public tokens: ', LReplay.PublicTokenCount);
    WriteLn('Latent states: ', LReplay.StateCount);
    WriteLn('Canonical model bytes: ', Length(LCanonical));
    WriteLn('Canonical round trip: verified');
    WriteLn('Seed: ', LSeed);
    WriteLn('Seeded output:');
    WriteTokens(LSeeded.Tokens);
    WriteLn('Projected pipeline output:');
    WriteTokens(LPipeline.Tokens);
    WriteLn('Downstream classes: outer branch outer');
    WriteLn('Independent validation: verified');
    WriteLn('Versions: sequence-model=', WFC_SEQUENCE_MODEL_VERSION,
      ' learner=', WFC_SEQUENCE_LEARN_ALGORITHM_VERSION,
      ' graph-adapter=', WFC_SEQUENCE_GRAPH_ADAPTER_VERSION,
      ' text=', WFC_SEQUENCE_TEXT_VERSION);
    WriteLn('Versions: random=', LSeededReport.RandomAlgorithmVersion,
      ' solver=', LSeededReport.SolverAlgorithmVersion,
      ' graph-model=', WFC_GRAPH_MODEL_VERSION,
      ' pipeline=', WFC_PIPELINE_ALGORITHM_VERSION);
    WriteLn('Pipeline passes: ', Length(LPipelineReport.Passes));
  finally
    LReplay.Free;
    LLearned.Free;
  end;
end;

begin
  try
    Run;
  except
    on E: Exception do
    begin
      WriteLn('LearnSequence error: ', E.Message);
      {$IFDEF PAS2JS}
      TNJSProcess.exitCode := 1;
      {$ELSE}
      Halt(1);
      {$ENDIF}
    end;
  end;
end.
