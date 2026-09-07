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
unit wfc_pipeline_result;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_lattice,
  wfc_pipeline_layout,
  wfc_model,
  wfc_pipeline_model,
  wfc_pipeline_run;

const
  WFC_PIPELINE_RESULT_VERSION = 1;
  WFC_PIPELINE_RESULT_MAPPED_VERSION = 2;
  WFC_PIPELINE_RESULT_SIGNATURE_VERSION = 1;

  { Result artifacts may be supplied by untrusted tooling. These are fixed
    format limits, not allocation hints. }
  WFC_PIPELINE_RESULT_MAX_PUBLIC_LAYER_COUNT = 256;
  WFC_PIPELINE_RESULT_MAX_TOTAL_PUBLIC_CELL_COUNT = 4194304;
  WFC_PIPELINE_RESULT_MAX_ENCODED_TOKEN_LENGTH = 1048576;
  WFC_PIPELINE_RESULT_MAX_TOTAL_ENCODED_TOKEN_LENGTH = 67108864;

type
  EWfcPipelineResult = class(Exception);

  TWfcPipelineResultSignature = Cardinal;

  TWfcPipelineResultStatus = (
    wprsSolved,
    wprsContradiction,
    wprsSolverBacktrackLimit,
    wprsPassBacktrackLimit
  );

  TWfcPipelineEvidenceKind = (
    wpekNone,
    wpekTrace,
    wpekNegotiationTranscript
  );

  { Every replay- or evidence-relevant core algorithm is pinned explicitly.
    The recipe independently pins adapter and bridge versions. }
  TWfcPipelineResultVersions = record
    GraphModelVersion: Integer;
    RandomAlgorithmVersion: Integer;
    SolverAlgorithmVersion: Integer;
    PipelineAlgorithmVersion: Integer;
    TraceVersion: Integer;
    TraceHashVersion: Integer;
    NegotiationAlgorithmVersion: Integer;
    NegotiationHashVersion: Integer;
  end;

  { One terminal-attempt outcome for each recipe pass, in pass-index order. }
  TWfcPipelinePassOutcome = record
    PassIndex: Integer;
    Decisions: Integer;
    Propagations: Integer;
    Contradictions: Integer;
    Backtracks: Integer;
    ExcludedAssignments: Integer;
    Executed: Boolean;
    ExecutionOrdinal: Integer;
    Disposition: TGraphPassDisposition;
  end;
  TWfcPipelinePassOutcomes = array of TWfcPipelinePassOutcome;

  TWfcPipelineFailure = record
    Kind: TGraphContradictionKind;
    PassIndex: Integer;
    EntryIndex: Integer;
    NeighborIndex: Integer;
    HasDirection: Boolean;
    Direction: TGraphDirection;
    DependencyPassIndex: Integer;
  end;

  TWfcPipelineResultLayer = record
    PassIndex: Integer;
    LabelName: TWfcModelToken;
    Tokens: TWfcModelTokens;
  end;
  TWfcPipelineResultLayers = array of TWfcPipelineResultLayer;

  { Immutable terminal result. RecipeSignature and RunSignature are its
    complete declared provenance chain. Shape, seed, strategy, and limits are
    copied from that run and signed again so a result remains self-describing.

    Successful results contain every public layer and no private layer. Failed
    results contain a structured contradiction and no partial layer data.
    Pass outcomes describe the terminal solve attempt. For negotiated runs,
    rejected attempts remain represented by the core transcript signature;
    unavailable event data is never synthesized. }
  TWfcPipelineResult = class
  strict private
    FRecipeSignature: TWfcPipelineSignature;
    FFormatVersion: Integer;
    FLayouts: TWfcPipelineLayoutTable;
    FRunSignature: TWfcPipelineRunSignature;
    FVersions: TWfcPipelineResultVersions;
    FWidth: Integer;
    FHeight: Integer;
    FDepth: Integer;
    FCellCount: Integer;
    FSeed: TGraphSeed;
    FStrategy: TWfcPipelineSolveStrategy;
    FMaxBacktracks: Integer;
    FMaxPassBacktracks: Integer;
    FCaptureTrace: Boolean;
    FStatus: TWfcPipelineResultStatus;
    FPassBacktracks: Integer;
    FEvidenceKind: TWfcPipelineEvidenceKind;
    FEvidenceSignature: TGraphTraceSignature;
    FFailure: TWfcPipelineFailure;
    FPassOutcomes: TWfcPipelinePassOutcomes;
    FLayers: TWfcPipelineResultLayers;
    FSignature: TWfcPipelineResultSignature;
    function GetPassOutcomeCount: Integer;
    function GetLayerCount: Integer;
    function GetPassCount: Integer;
    function GetTotalCellCount: Integer;
    procedure ValidatePassOutcomeIndex(const AIndex: Integer);
    procedure ValidateLayerIndex(const AIndex: Integer);
    function CalculateSignature: TWfcPipelineResultSignature;
  public
    constructor Create(const ARecipe: TWfcPipelineModel;
      const ARun: TWfcPipelineRun;
      const AVersions: TWfcPipelineResultVersions;
      const AStatus: TWfcPipelineResultStatus;
      const APassBacktracks: Integer;
      const AEvidenceKind: TWfcPipelineEvidenceKind;
      const AEvidenceSignature: TGraphTraceSignature;
      const AFailure: TWfcPipelineFailure;
      const APassOutcomes: TWfcPipelinePassOutcomes;
      const ALayers: TWfcPipelineResultLayers);
    destructor Destroy; override;

    function CopyVersions: TWfcPipelineResultVersions;
    function CopyFailure: TWfcPipelineFailure;
    function PassOutcomeAt(const AIndex: Integer): TWfcPipelinePassOutcome;
    function LayerAt(const AIndex: Integer): TWfcPipelineResultLayer;
    function CopyPassOutcomes: TWfcPipelinePassOutcomes;
    function CopyLayers: TWfcPipelineResultLayers;
    function CopyPassLayouts: TWfcLatticeLayouts;
    function CopyPassExtents: TWfcPipelinePassExtents;
    function PassLayoutAt(const APassIndex: Integer): TWfcLatticeLayout;
    function PassTopologyAt(const APassIndex: Integer): TWfcPipelinePassTopology;
    function LayerLayoutAt(const ALayerIndex: Integer): TWfcLatticeLayout;
    function PassCellCount(const APassIndex: Integer): Integer;
    function PassOffsetAt(const APassIndex: Integer): Integer;

    property RecipeSignature: TWfcPipelineSignature read FRecipeSignature;
    property FormatVersion: Integer read FFormatVersion;
    property PassCount: Integer read GetPassCount;
    property TotalCellCount: Integer read GetTotalCellCount;
    property RunSignature: TWfcPipelineRunSignature read FRunSignature;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Depth: Integer read FDepth;
    property CellCount: Integer read FCellCount;
    property Seed: TGraphSeed read FSeed;
    property Strategy: TWfcPipelineSolveStrategy read FStrategy;
    property MaxBacktracks: Integer read FMaxBacktracks;
    property MaxPassBacktracks: Integer read FMaxPassBacktracks;
    property CaptureTrace: Boolean read FCaptureTrace;
    property Status: TWfcPipelineResultStatus read FStatus;
    property PassBacktracks: Integer read FPassBacktracks;
    property EvidenceKind: TWfcPipelineEvidenceKind read FEvidenceKind;
    property EvidenceSignature: TGraphTraceSignature read FEvidenceSignature;
    property PassOutcomeCount: Integer read GetPassOutcomeCount;
    property LayerCount: Integer read GetLayerCount;
    property Signature: TWfcPipelineResultSignature read FSignature;
  end;

function CurrentWfcPipelineResultVersions: TWfcPipelineResultVersions;
function WfcPipelineResultVersionsFromSolveReport(
  const AReport: TGraphSolveReport): TWfcPipelineResultVersions;
function MakeWfcPipelinePassOutcome(const APassIndex: Integer;
  const AReport: TGraphPassSolveReport): TWfcPipelinePassOutcome;
function EmptyWfcPipelineFailure: TWfcPipelineFailure;
function MakeWfcPipelineFailure(
  const AContradiction: TGraphContradiction): TWfcPipelineFailure;
function MakeWfcPipelineResultLayer(const APassIndex: Integer;
  const ALabelName: TWfcModelToken; const ATokens: TWfcModelTokens):
  TWfcPipelineResultLayer;

function WfcPipelinePassOutcomesFromSolveReport(
  const AReport: TGraphSolveReport): TWfcPipelinePassOutcomes;
function WfcPipelineFailureFromSolveReport(
  const AReport: TGraphSolveReport): TWfcPipelineFailure;
function WfcPipelineResultStatusFromSolveStatus(
  const AStatus: TGraphSolveStatus): TWfcPipelineResultStatus;
function WfcPipelineResultStatusFromNegotiationStatus(
  const AStatus: TGraphNegotiationStatus): TWfcPipelineResultStatus;

{ Capture checks every graph property exposed by the core: shape, seed, pass
  count/labels, completeness, and public vocabulary. TGraph does not expose a
  semantic definition signature, so callers must supply the graph produced
  from ARecipe; the recipe/run signatures remain the declared provenance. }
function CaptureWfcPipelinePublicLayers(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun;
  const AGraph: TGraph): TWfcPipelineResultLayers;
function CreateWfcPipelineResultFromSolveReport(
  const ARecipe: TWfcPipelineModel; const ARun: TWfcPipelineRun;
  const AGraph: TGraph; const AReport: TGraphSolveReport):
  TWfcPipelineResult;
function CreateWfcPipelineResultFromNegotiationReport(
  const ARecipe: TWfcPipelineModel; const ARun: TWfcPipelineRun;
  const AGraph: TGraph; const AReport: TGraphNegotiationReport):
  TWfcPipelineResult;

function WfcPipelineResultSignatureHex(
  const ASignature: TWfcPipelineResultSignature): String;

implementation

uses
  wfc_text_codec,
  wfc_pipeline_mapping,
  wfc_token_lookup,
  wfc_pipeline_connectivity;

type
  TBooleanArray = array of Boolean;

function CheckedLength(const ALength: SizeInt; const ALabel: String;
  const AMaximum: Integer): Integer;
begin
  if ALength > SizeInt(AMaximum) then
    raise EWfcPipelineResult.CreateFmt(
      '%s exceeds the version-1 limit [%d > %d]',
      [ALabel, ALength, AMaximum]);
  Result := Integer(ALength);
end;

function CheckedCellCount(const ARun: TWfcPipelineRun): Integer;
var
  LPlane: Integer;
begin
  if ARun.Width > WFC_PIPELINE_RUN_MAX_CELL_COUNT div ARun.Height then
    raise EWfcPipelineResult.Create(
      'result run cell count exceeds the version-1 limit');
  LPlane := ARun.Width * ARun.Height;
  if LPlane > WFC_PIPELINE_RUN_MAX_CELL_COUNT div ARun.Depth then
    raise EWfcPipelineResult.Create(
      'result run cell count exceeds the version-1 limit');
  Result := LPlane * ARun.Depth;
end;

function CloneTokens(const AValues: TWfcModelTokens): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function CloneLayer(const AValue: TWfcPipelineResultLayer):
  TWfcPipelineResultLayer;
begin
  Result := AValue;
  Result.Tokens := CloneTokens(AValue.Tokens);
end;

function TokenIndex(const AValues: TWfcModelTokens;
  const AValue: TWfcModelToken): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(AValues) - 1 do
    if AValues[I] = AValue then
      Exit(I);
  Result := -1;
end;

procedure AddTokenLength(var ATotal: Integer;
  const AValue: TWfcModelToken; const ALabel: String);
var
  LCanonical: String;
  LLength: Integer;
begin
  if not WfcModelTokenIsValid(AValue) then
    raise EWfcPipelineResult.Create(ALabel +
      ' is not a valid nonempty token');
  LCanonical := WfcTextEncodeToken(AValue,
    'pipeline result token validation');
  LLength := CheckedLength(Length(LCanonical), ALabel + ' encoded length',
    WFC_PIPELINE_RESULT_MAX_ENCODED_TOKEN_LENGTH);
  if LLength > WFC_PIPELINE_RESULT_MAX_TOTAL_ENCODED_TOKEN_LENGTH - ATotal then
    raise EWfcPipelineResult.Create(
      'result encoded token bytes exceed the version-1 aggregate limit');
  Inc(ATotal, LLength);
end;

procedure ValidateResultStatus(const AValue: TWfcPipelineResultStatus);
begin
  if (Ord(AValue) < Ord(Low(TWfcPipelineResultStatus))) or
      (Ord(AValue) > Ord(High(TWfcPipelineResultStatus))) then
    raise EWfcPipelineResult.CreateFmt('result status is unknown [%d]',
      [Ord(AValue)]);
  case AValue of
    wprsSolved, wprsContradiction, wprsSolverBacktrackLimit,
      wprsPassBacktrackLimit:
        Exit;
  end;
end;

procedure ValidateEvidenceKind(const AValue: TWfcPipelineEvidenceKind);
begin
  if (Ord(AValue) < Ord(Low(TWfcPipelineEvidenceKind))) or
      (Ord(AValue) > Ord(High(TWfcPipelineEvidenceKind))) then
    raise EWfcPipelineResult.CreateFmt(
      'result evidence kind is unknown [%d]', [Ord(AValue)]);
  case AValue of
    wpekNone, wpekTrace, wpekNegotiationTranscript:
      Exit;
  end;
end;

procedure ValidateDisposition(const AValue: TGraphPassDisposition);
begin
  if (Ord(AValue) < Ord(Low(TGraphPassDisposition))) or
      (Ord(AValue) > Ord(High(TGraphPassDisposition))) then
    raise EWfcPipelineResult.CreateFmt('pass disposition is unknown [%d]',
      [Ord(AValue)]);
  case AValue of
    gpdNotRun, gpdReused, gpdCleared, gpdCopied, gpdSolved, gpdFailed:
      Exit;
  end;
end;

procedure ValidateContradictionKind(
  const AValue: TGraphContradictionKind);
begin
  if (Ord(AValue) < Ord(Low(TGraphContradictionKind))) or
      (Ord(AValue) > Ord(High(TGraphContradictionKind))) then
    raise EWfcPipelineResult.CreateFmt(
      'result contradiction kind is unknown [%d]', [Ord(AValue)]);
end;

procedure ValidateDirection(const AValue: TGraphDirection);
begin
  if (Ord(AValue) < Ord(Low(TGraphDirection))) or
      (Ord(AValue) > Ord(High(TGraphDirection))) then
    raise EWfcPipelineResult.CreateFmt('result direction is unknown [%d]',
      [Ord(AValue)]);
end;

procedure ValidateVersions(const ARecipe: TWfcPipelineModel;
  const AValue: TWfcPipelineResultVersions);
var
  LRecipeVersions: TWfcPipelineVersions;
begin
  LRecipeVersions := ARecipe.CopyVersions;
  if (AValue.GraphModelVersion <> WFC_GRAPH_MODEL_VERSION) or
      (AValue.GraphModelVersion <> LRecipeVersions.GraphModelVersion) then
    raise EWfcPipelineResult.Create('unsupported result graph-model version');
  if (AValue.RandomAlgorithmVersion <> WFC_RANDOM_ALGORITHM_VERSION) or
      (AValue.RandomAlgorithmVersion <>
      LRecipeVersions.RandomAlgorithmVersion) then
    raise EWfcPipelineResult.Create(
      'unsupported result random-algorithm version');
  if (AValue.SolverAlgorithmVersion <> WFC_SOLVER_ALGORITHM_VERSION) or
      (AValue.SolverAlgorithmVersion <>
      LRecipeVersions.SolverAlgorithmVersion) then
    raise EWfcPipelineResult.Create(
      'unsupported result solver-algorithm version');
  if (AValue.PipelineAlgorithmVersion <> WFC_PIPELINE_ALGORITHM_VERSION) or
      (AValue.PipelineAlgorithmVersion <>
      LRecipeVersions.PipelineAlgorithmVersion) then
    raise EWfcPipelineResult.Create(
      'unsupported result pipeline-algorithm version');
  if AValue.TraceVersion <> WFC_TRACE_VERSION then
    raise EWfcPipelineResult.Create('unsupported result trace version');
  if AValue.TraceHashVersion <> WFC_TRACE_HASH_VERSION then
    raise EWfcPipelineResult.Create('unsupported result trace-hash version');
  if AValue.NegotiationAlgorithmVersion <>
      WFC_PASS_NEGOTIATION_ALGORITHM_VERSION then
    raise EWfcPipelineResult.Create(
      'unsupported result negotiation-algorithm version');
  if AValue.NegotiationHashVersion <>
      WFC_PASS_NEGOTIATION_HASH_VERSION then
    raise EWfcPipelineResult.Create(
      'unsupported result negotiation-hash version');
end;

procedure HashByte(var AHash: TWfcPipelineResultSignature;
  const AValue: Byte);
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

procedure HashCardinal(var AHash: TWfcPipelineResultSignature;
  const AValue: Cardinal);
begin
  HashByte(AHash, Byte(AValue and $FF));
  HashByte(AHash, Byte((AValue shr 8) and $FF));
  HashByte(AHash, Byte((AValue shr 16) and $FF));
  HashByte(AHash, Byte((AValue shr 24) and $FF));
end;

procedure HashInteger(var AHash: TWfcPipelineResultSignature;
  const AValue: Integer);
begin
  HashCardinal(AHash, Cardinal(AValue));
end;

procedure HashBoolean(var AHash: TWfcPipelineResultSignature;
  const AValue: Boolean);
begin
  if AValue then
    HashByte(AHash, 1)
  else
    HashByte(AHash, 0);
end;

procedure HashAscii(var AHash: TWfcPipelineResultSignature;
  const AValue: String);
var
  I: Integer;
begin
  HashInteger(AHash, Length(AValue));
  for I := 1 to Length(AValue) do
    HashByte(AHash, Byte(Ord(AValue[I])));
end;

procedure HashToken(var AHash: TWfcPipelineResultSignature;
  const AValue: TWfcModelToken);
begin
  HashAscii(AHash, WfcTextEncodeToken(AValue,
    'pipeline result semantic signature'));
end;

function CurrentWfcPipelineResultVersions: TWfcPipelineResultVersions;
begin
  Result.GraphModelVersion := WFC_GRAPH_MODEL_VERSION;
  Result.RandomAlgorithmVersion := WFC_RANDOM_ALGORITHM_VERSION;
  Result.SolverAlgorithmVersion := WFC_SOLVER_ALGORITHM_VERSION;
  Result.PipelineAlgorithmVersion := WFC_PIPELINE_ALGORITHM_VERSION;
  Result.TraceVersion := WFC_TRACE_VERSION;
  Result.TraceHashVersion := WFC_TRACE_HASH_VERSION;
  Result.NegotiationAlgorithmVersion :=
    WFC_PASS_NEGOTIATION_ALGORITHM_VERSION;
  Result.NegotiationHashVersion := WFC_PASS_NEGOTIATION_HASH_VERSION;
end;

function WfcPipelineResultVersionsFromSolveReport(
  const AReport: TGraphSolveReport): TWfcPipelineResultVersions;
begin
  Result := CurrentWfcPipelineResultVersions;
  Result.GraphModelVersion := AReport.GraphModelVersion;
  Result.RandomAlgorithmVersion := AReport.RandomAlgorithmVersion;
  Result.SolverAlgorithmVersion := AReport.SolverAlgorithmVersion;
  Result.PipelineAlgorithmVersion := AReport.PipelineAlgorithmVersion;
end;

function MakeWfcPipelinePassOutcome(const APassIndex: Integer;
  const AReport: TGraphPassSolveReport): TWfcPipelinePassOutcome;
begin
  Result.PassIndex := APassIndex;
  Result.Decisions := AReport.Decisions;
  Result.Propagations := AReport.Propagations;
  Result.Contradictions := AReport.Contradictions;
  Result.Backtracks := AReport.Backtracks;
  Result.ExcludedAssignments := AReport.ExcludedAssignments;
  Result.Executed := AReport.Executed;
  Result.ExecutionOrdinal := AReport.ExecutionOrdinal;
  Result.Disposition := AReport.Disposition;
end;

function EmptyWfcPipelineFailure: TWfcPipelineFailure;
begin
  Result.Kind := gckNone;
  Result.PassIndex := -1;
  Result.EntryIndex := -1;
  Result.NeighborIndex := -1;
  Result.HasDirection := False;
  Result.Direction := gdNorth;
  Result.DependencyPassIndex := -1;
end;

function MakeWfcPipelineFailure(
  const AContradiction: TGraphContradiction): TWfcPipelineFailure;
begin
  Result.Kind := AContradiction.Kind;
  Result.PassIndex := AContradiction.PassIndex;
  Result.EntryIndex := AContradiction.EntryIndex;
  Result.NeighborIndex := AContradiction.NeighborIndex;
  Result.HasDirection := AContradiction.HasDirection;
  Result.Direction := AContradiction.Direction;
  Result.DependencyPassIndex := AContradiction.DependencyPassIndex;
end;

function MakeWfcPipelineResultLayer(const APassIndex: Integer;
  const ALabelName: TWfcModelToken; const ATokens: TWfcModelTokens):
  TWfcPipelineResultLayer;
begin
  Result.PassIndex := APassIndex;
  Result.LabelName := ALabelName;
  Result.Tokens := CloneTokens(ATokens);
end;

function WfcPipelinePassOutcomesFromSolveReport(
  const AReport: TGraphSolveReport): TWfcPipelinePassOutcomes;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AReport.Passes));
  for I := 0 to Length(AReport.Passes) - 1 do
    Result[I] := MakeWfcPipelinePassOutcome(I, AReport.Passes[I]);
end;

function WfcPipelineFailureFromSolveReport(
  const AReport: TGraphSolveReport): TWfcPipelineFailure;
begin
  Result := MakeWfcPipelineFailure(AReport.Contradiction);
end;

function WfcPipelineResultStatusFromSolveStatus(
  const AStatus: TGraphSolveStatus): TWfcPipelineResultStatus;
begin
  case Ord(AStatus) of
    Ord(gssSolved): Result := wprsSolved;
    Ord(gssContradiction): Result := wprsContradiction;
    Ord(gssBacktrackLimit): Result := wprsSolverBacktrackLimit;
  else
    raise EWfcPipelineResult.CreateFmt('graph solve status is unknown [%d]',
      [Ord(AStatus)]);
  end;
end;

function WfcPipelineResultStatusFromNegotiationStatus(
  const AStatus: TGraphNegotiationStatus): TWfcPipelineResultStatus;
begin
  case Ord(AStatus) of
    Ord(gnsSolved): Result := wprsSolved;
    Ord(gnsContradiction): Result := wprsContradiction;
    Ord(gnsSolverBacktrackLimit): Result := wprsSolverBacktrackLimit;
    Ord(gnsPassBacktrackLimit): Result := wprsPassBacktrackLimit;
  else
    raise EWfcPipelineResult.CreateFmt(
      'graph negotiation status is unknown [%d]', [Ord(AStatus)]);
  end;
end;

function GraphValueToToken(const AValue: TGraphValue): TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(AValue);
  {$ELSE}
  Result := UTF8Encode(UnicodeString(AValue));
  {$ENDIF}
end;

function CaptureLayoutMatches(const ARun: TWfcPipelineRun;
  const APassIndex: Integer; const AGraph: TGraph): Boolean;
var LExpected: TWfcLatticeLayout;
begin
  LExpected := ARun.PassLayoutAt(APassIndex);
  if ARun.FormatVersion = WFC_PIPELINE_RUN_MAPPED_VERSION then
    Exit(SameWfcLatticeLayout(AGraph.PassLayout, LExpected));
  { Preserve the original version-1 capture boundary: declared provenance and
    dimensions, not a newly inferred wrapping/layout authenticity guarantee. }
  Result := (AGraph.Dimension.Width = TGraphCoordinate(LExpected.Cells.X)) and
    (AGraph.Dimension.Height = TGraphCoordinate(LExpected.Cells.Y)) and
    (AGraph.Dimension.Depth = TGraphCoordinate(LExpected.Cells.Z));
end;

function CaptureWfcPipelinePublicLayers(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun;
  const AGraph: TGraph): TWfcPipelineResultLayers;
var
  I: Integer;
  LCellCount: Integer;
  LEntry: TGraphEntry;
  LLayerCount: Integer;
  LTotalPublicCells: Integer;
  LLayout: TWfcLatticeLayout;
  LPassGraph: TGraph;
  LPassIndex: Integer;
  LTokenIndex: Integer;
  LTokens: TWfcModelTokens;
  LVocabulary: TWfcModelTokens;
  X: Integer;
  Y: Integer;
  Z: Integer;
begin
  Result := nil;
  if not Assigned(ARecipe) then
    raise EWfcPipelineResult.Create('capture recipe cannot be nil');
  if not Assigned(ARun) then
    raise EWfcPipelineResult.Create('capture run cannot be nil');
  if not Assigned(AGraph) then
    raise EWfcPipelineResult.Create('capture graph cannot be nil');
  if ARun.RecipeSignature <> ARecipe.Signature then
    raise EWfcPipelineResult.Create(
      'capture recipe does not match its run provenance');
  if AGraph.TotalPassCount <> ARecipe.PassCount then
    raise EWfcPipelineResult.Create(
      'capture graph pass count does not match the recipe');
  if (AGraph.Dimension.Width <> TGraphCoordinate(ARun.Width)) or
      (AGraph.Dimension.Height <> TGraphCoordinate(ARun.Height)) or
      (AGraph.Dimension.Depth <> TGraphCoordinate(ARun.Depth)) then
    raise EWfcPipelineResult.Create(
      'capture graph shape does not match the run');
  if AGraph.Seed <> ARun.Seed then
    raise EWfcPipelineResult.Create(
      'capture graph seed does not match the run');

  LLayerCount := 0;
  LTotalPublicCells := 0;
  for I := 0 to ARecipe.PassCount - 1 do
  begin
    LPassGraph := AGraph.PassGraph[I];
    if GraphValueToToken(LPassGraph.CurrentPass) <>
        ARecipe.PassAt(I).LabelName then
      raise EWfcPipelineResult.CreateFmt(
        'capture graph pass label does not match the recipe [%d]', [I]);
    if not CaptureLayoutMatches(ARun, I, LPassGraph) then
      raise EWfcPipelineResult.CreateFmt(
        'capture pass layout does not match the run [%d]', [I]);
    if ARecipe.PassAt(I).Visibility = wppvPublic then
    begin
      Inc(LLayerCount);
      LCellCount := ARun.PassCellCount(I);
      if LCellCount > WFC_PIPELINE_RESULT_MAX_TOTAL_PUBLIC_CELL_COUNT -
          LTotalPublicCells then
        raise EWfcPipelineResult.Create('captured public cells exceed the aggregate limit');
      Inc(LTotalPublicCells, LCellCount);
    end;
  end;
  if LLayerCount > WFC_PIPELINE_RESULT_MAX_PUBLIC_LAYER_COUNT then
    raise EWfcPipelineResult.Create(
      'captured public result exceeds the version-1 layer/cell limits');
  SetLength(Result, LLayerCount);
  LLayerCount := 0;
  for LPassIndex := 0 to ARecipe.PassCount - 1 do
  begin
    LPassGraph := AGraph.PassGraph[LPassIndex];
    if GraphValueToToken(LPassGraph.CurrentPass) <>
        ARecipe.PassAt(LPassIndex).LabelName then
      raise EWfcPipelineResult.CreateFmt(
        'capture graph pass label does not match the recipe [%d]',
        [LPassIndex]);
    LLayout := ARun.PassLayoutAt(LPassIndex);
    if not CaptureLayoutMatches(ARun, LPassIndex, LPassGraph) then
      raise EWfcPipelineResult.CreateFmt(
        'capture pass shape does not match the run [%d]', [LPassIndex]);
    if ARecipe.PassAt(LPassIndex).Visibility <> wppvPublic then
      Continue;
    LCellCount := ARun.PassCellCount(LPassIndex);
    SetLength(LTokens, LCellCount);
    LVocabulary := ARecipe.CopyPublicVocabulary(LPassIndex);
    LTokenIndex := 0;
    for Z := 0 to LLayout.Cells.Z - 1 do
      for Y := 0 to LLayout.Cells.Y - 1 do
        for X := 0 to LLayout.Cells.X - 1 do
        begin
          LEntry := LPassGraph.Entry[TGraphCoordinate(X),
            TGraphCoordinate(Y), TGraphCoordinate(Z)];
          if LEntry.Empty then
            raise EWfcPipelineResult.CreateFmt(
              'capture public pass has an empty entry [%d, %d]',
              [LPassIndex, LTokenIndex]);
          LTokens[LTokenIndex] := GraphValueToToken(LEntry.Value);
          if TokenIndex(LVocabulary, LTokens[LTokenIndex]) < 0 then
            raise EWfcPipelineResult.CreateFmt(
              'capture public token is outside the recipe vocabulary [%d, %d]',
              [LPassIndex, LTokenIndex]);
          Inc(LTokenIndex);
        end;
    Result[LLayerCount] := MakeWfcPipelineResultLayer(LPassIndex,
      ARecipe.PassAt(LPassIndex).LabelName, LTokens);
    Inc(LLayerCount);
  end;
end;

procedure ValidateSolveReportForRun(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun; const AReport: TGraphSolveReport);
var
  I: Integer;
  LPassIndex: Integer;
  LRecipeVersions: TWfcPipelineVersions;
begin
  if not Assigned(ARecipe) then
    raise EWfcPipelineResult.Create('result recipe cannot be nil');
  if not Assigned(ARun) then
    raise EWfcPipelineResult.Create('result run cannot be nil');
  if ARun.RecipeSignature <> ARecipe.Signature then
    raise EWfcPipelineResult.Create(
      'result recipe does not match its run provenance');
  if AReport.Seed <> ARun.Seed then
    raise EWfcPipelineResult.Create(
      'solve report seed does not match the run');
  if AReport.TraceCaptured <> ARun.CaptureTrace then
    raise EWfcPipelineResult.Create(
      'solve report trace policy does not match the run');
  if Length(AReport.Passes) <> ARecipe.PassCount then
    raise EWfcPipelineResult.Create(
      'solve report pass count does not match the recipe');
  if Length(AReport.ExecutionOrder) > Length(AReport.Passes) then
    raise EWfcPipelineResult.Create(
      'solve report execution order exceeds its pass count');
  for I := 0 to Length(AReport.ExecutionOrder) - 1 do
  begin
    LPassIndex := AReport.ExecutionOrder[I];
    if (LPassIndex < 0) or (LPassIndex >= Length(AReport.Passes)) or
        (not AReport.Passes[LPassIndex].Executed) or
        (AReport.Passes[LPassIndex].ExecutionOrdinal <> I) then
      raise EWfcPipelineResult.Create(
        'solve report execution order is inconsistent');
  end;
  for I := 0 to Length(AReport.Passes) - 1 do
    if AReport.Passes[I].Executed then
    begin
      if (AReport.Passes[I].ExecutionOrdinal < 0) or
          (AReport.Passes[I].ExecutionOrdinal >=
          Length(AReport.ExecutionOrder)) or
          (AReport.ExecutionOrder[
          AReport.Passes[I].ExecutionOrdinal] <> I) then
        raise EWfcPipelineResult.Create(
          'solve report pass execution ordinal is inconsistent');
    end;
  if AReport.Status = gssSolved then
  begin
    if AReport.FailedPassIndex <> -1 then
      raise EWfcPipelineResult.Create(
        'solved report cannot identify a failed pass');
  end
  else if (AReport.FailedPassIndex < 0) or
      (AReport.FailedPassIndex >= ARecipe.PassCount) or
      (AReport.Contradiction.PassIndex <> AReport.FailedPassIndex) then
    raise EWfcPipelineResult.Create(
      'failed report does not identify its contradiction pass');
  if AReport.TraceCaptured then
  begin
    if AReport.TraceHash <> CalculateGraphTraceHash(AReport) then
      raise EWfcPipelineResult.Create(
        'solve report trace signature does not match its events');
  end
  else if (AReport.TraceHash <> 0) or (Length(AReport.Trace) <> 0) then
    raise EWfcPipelineResult.Create(
      'trace-disabled report contains trace evidence');
  LRecipeVersions := ARecipe.CopyVersions;
  if (AReport.GraphModelVersion <> LRecipeVersions.GraphModelVersion) or
      (AReport.RandomAlgorithmVersion <>
      LRecipeVersions.RandomAlgorithmVersion) or
      (AReport.SolverAlgorithmVersion <>
      LRecipeVersions.SolverAlgorithmVersion) or
      (AReport.PipelineAlgorithmVersion <>
      LRecipeVersions.PipelineAlgorithmVersion) then
    raise EWfcPipelineResult.Create(
      'solve report versions do not match the recipe');
end;

function CreateWfcPipelineResultFromSolveReport(
  const ARecipe: TWfcPipelineModel; const ARun: TWfcPipelineRun;
  const AGraph: TGraph; const AReport: TGraphSolveReport):
  TWfcPipelineResult;
var
  LEvidenceKind: TWfcPipelineEvidenceKind;
  LEvidenceSignature: TGraphTraceSignature;
  LLayers: TWfcPipelineResultLayers;
  LStatus: TWfcPipelineResultStatus;
begin
  if not Assigned(ARecipe) then
    raise EWfcPipelineResult.Create('result recipe cannot be nil');
  if not Assigned(ARun) then
    raise EWfcPipelineResult.Create('result run cannot be nil');
  if ARun.Strategy <> wpssOneWay then
    raise EWfcPipelineResult.Create(
      'ordinary solve reports require a one-way run');
  ValidateSolveReportForRun(ARecipe, ARun, AReport);
  LStatus := WfcPipelineResultStatusFromSolveStatus(AReport.Status);
  if ARun.CaptureTrace then
  begin
    LEvidenceKind := wpekTrace;
    LEvidenceSignature := AReport.TraceHash;
  end
  else
  begin
    LEvidenceKind := wpekNone;
    LEvidenceSignature := 0;
  end;
  if LStatus = wprsSolved then
    LLayers := CaptureWfcPipelinePublicLayers(ARecipe, ARun, AGraph)
  else
    LLayers := nil;
  Result := TWfcPipelineResult.Create(ARecipe, ARun,
    WfcPipelineResultVersionsFromSolveReport(AReport), LStatus, 0,
    LEvidenceKind, LEvidenceSignature,
    WfcPipelineFailureFromSolveReport(AReport),
    WfcPipelinePassOutcomesFromSolveReport(AReport), LLayers);
end;

function CreateWfcPipelineResultFromNegotiationReport(
  const ARecipe: TWfcPipelineModel; const ARun: TWfcPipelineRun;
  const AGraph: TGraph; const AReport: TGraphNegotiationReport):
  TWfcPipelineResult;
var
  I: Integer;
  LLayers: TWfcPipelineResultLayers;
  LOptions: TGraphNegotiationOptions;
  LStatus: TWfcPipelineResultStatus;
begin
  if not Assigned(ARecipe) then
    raise EWfcPipelineResult.Create('result recipe cannot be nil');
  if not Assigned(ARun) then
    raise EWfcPipelineResult.Create('result run cannot be nil');
  if ARun.Strategy <> wpssNegotiated then
    raise EWfcPipelineResult.Create(
      'negotiation reports require a negotiated run');
  if AReport.Seed <> ARun.Seed then
    raise EWfcPipelineResult.Create(
      'negotiation report seed does not match the run');
  if AReport.NegotiationAlgorithmVersion <>
      WFC_PASS_NEGOTIATION_ALGORITHM_VERSION then
    raise EWfcPipelineResult.Create(
      'negotiation report algorithm version is unsupported');
  if (AReport.PassBacktracks < 0) or
      (AReport.PassBacktracks > ARun.MaxPassBacktracks) or
      (Length(AReport.Attempts) <> AReport.PassBacktracks) then
    raise EWfcPipelineResult.Create(
      'negotiation report pass-backtrack history is inconsistent');
  for I := 0 to Length(AReport.Attempts) - 1 do
    ValidateSolveReportForRun(ARecipe, ARun,
      AReport.Attempts[I].SolveReport);
  ValidateSolveReportForRun(ARecipe, ARun, AReport.FinalReport);
  LStatus := WfcPipelineResultStatusFromNegotiationStatus(AReport.Status);
  case AReport.Status of
    gnsSolved:
      if AReport.FinalReport.Status <> gssSolved then
        raise EWfcPipelineResult.Create(
          'negotiation status does not match its final solve');
    gnsContradiction, gnsPassBacktrackLimit:
      if AReport.FinalReport.Status <> gssContradiction then
        raise EWfcPipelineResult.Create(
          'negotiation status does not match its final solve');
    gnsSolverBacktrackLimit:
      if AReport.FinalReport.Status <> gssBacktrackLimit then
        raise EWfcPipelineResult.Create(
          'negotiation status does not match its final solve');
  end;
  LOptions := DefaultGraphNegotiationOptions;
  LOptions.SolveOptions.MaxBacktracks := ARun.MaxBacktracks;
  LOptions.SolveOptions.CaptureTrace := ARun.CaptureTrace;
  LOptions.MaxPassBacktracks := ARun.MaxPassBacktracks;
  if AReport.TranscriptHash <>
      CalculateGraphNegotiationTranscriptHash(LOptions, AReport) then
    raise EWfcPipelineResult.Create(
      'negotiation transcript signature does not match its report');
  if LStatus = wprsSolved then
    LLayers := CaptureWfcPipelinePublicLayers(ARecipe, ARun, AGraph)
  else
    LLayers := nil;
  Result := TWfcPipelineResult.Create(ARecipe, ARun,
    WfcPipelineResultVersionsFromSolveReport(AReport.FinalReport),
    LStatus, AReport.PassBacktracks, wpekNegotiationTranscript,
    AReport.TranscriptHash,
    WfcPipelineFailureFromSolveReport(AReport.FinalReport),
    WfcPipelinePassOutcomesFromSolveReport(AReport.FinalReport), LLayers);
end;

function WfcPipelineResultSignatureHex(
  const ASignature: TWfcPipelineResultSignature): String;
begin
  Result := IntToHex(ASignature, 8);
end;

function TWfcPipelineResult.CalculateSignature:
  TWfcPipelineResultSignature;
var
  I: Integer;
  J: Integer;
  LFailure: TWfcPipelineFailure;
  LLayer: TWfcPipelineResultLayer;
  LOutcome: TWfcPipelinePassOutcome;
  LVersions: TWfcPipelineResultVersions;
  LLayout: TWfcLatticeLayout;
begin
  Result := Cardinal(2166136261);
  HashAscii(Result, 'wfcpipeline-result');
  HashCardinal(Result, FFormatVersion);
  HashCardinal(Result, WFC_PIPELINE_RESULT_SIGNATURE_VERSION);
  HashCardinal(Result, FRecipeSignature);
  HashCardinal(Result, FRunSignature);
  LVersions := FVersions;
  HashInteger(Result, LVersions.GraphModelVersion);
  HashInteger(Result, LVersions.RandomAlgorithmVersion);
  HashInteger(Result, LVersions.SolverAlgorithmVersion);
  HashInteger(Result, LVersions.PipelineAlgorithmVersion);
  HashInteger(Result, LVersions.TraceVersion);
  HashInteger(Result, LVersions.TraceHashVersion);
  HashInteger(Result, LVersions.NegotiationAlgorithmVersion);
  HashInteger(Result, LVersions.NegotiationHashVersion);
  HashInteger(Result, FWidth);
  HashInteger(Result, FHeight);
  HashInteger(Result, FDepth);
  if FFormatVersion = WFC_PIPELINE_RESULT_MAPPED_VERSION then
  begin
    HashAscii(Result, 'resolved-pass-layouts');
    HashInteger(Result, WFC_PIPELINE_LAYOUT_VERSION);
    HashInteger(Result, WFC_PASS_MAPPING_VERSION);
    HashInteger(Result, PassCount);
    for I := 0 to PassCount - 1 do
    begin
      LLayout := FLayouts.PassLayoutAt(I);
      HashInteger(Result, FLayouts.PassTopologyAt(I).Rank);
      HashInteger(Result, LLayout.Origin.X);
      HashInteger(Result, LLayout.Origin.Y);
      HashInteger(Result, LLayout.Origin.Z);
      HashInteger(Result, LLayout.Pitch.X);
      HashInteger(Result, LLayout.Pitch.Y);
      HashInteger(Result, LLayout.Pitch.Z);
      HashInteger(Result, LLayout.Cells.X);
      HashInteger(Result, LLayout.Cells.Y);
      HashInteger(Result, LLayout.Cells.Z);
      HashBoolean(Result, LLayout.Wrap);
    end;
  end;
  HashCardinal(Result, FSeed);
  HashInteger(Result, Ord(FStrategy));
  HashInteger(Result, FMaxBacktracks);
  HashInteger(Result, FMaxPassBacktracks);
  HashBoolean(Result, FCaptureTrace);
  HashInteger(Result, Ord(FStatus));
  HashInteger(Result, FPassBacktracks);
  HashInteger(Result, Ord(FEvidenceKind));
  HashCardinal(Result, FEvidenceSignature);
  LFailure := FFailure;
  HashInteger(Result, Ord(LFailure.Kind));
  HashInteger(Result, LFailure.PassIndex);
  HashInteger(Result, LFailure.EntryIndex);
  HashInteger(Result, LFailure.NeighborIndex);
  HashBoolean(Result, LFailure.HasDirection);
  HashInteger(Result, Ord(LFailure.Direction));
  HashInteger(Result, LFailure.DependencyPassIndex);
  HashInteger(Result, Length(FPassOutcomes));
  for I := 0 to Length(FPassOutcomes) - 1 do
  begin
    LOutcome := FPassOutcomes[I];
    HashInteger(Result, LOutcome.PassIndex);
    HashInteger(Result, LOutcome.Decisions);
    HashInteger(Result, LOutcome.Propagations);
    HashInteger(Result, LOutcome.Contradictions);
    HashInteger(Result, LOutcome.Backtracks);
    HashInteger(Result, LOutcome.ExcludedAssignments);
    HashBoolean(Result, LOutcome.Executed);
    HashInteger(Result, LOutcome.ExecutionOrdinal);
    HashInteger(Result, Ord(LOutcome.Disposition));
  end;
  HashInteger(Result, Length(FLayers));
  for I := 0 to Length(FLayers) - 1 do
  begin
    LLayer := FLayers[I];
    HashInteger(Result, LLayer.PassIndex);
    HashToken(Result, LLayer.LabelName);
    HashInteger(Result, Length(LLayer.Tokens));
    for J := 0 to Length(LLayer.Tokens) - 1 do
      HashToken(Result, LLayer.Tokens[J]);
  end;
end;

constructor TWfcPipelineResult.Create(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun;
  const AVersions: TWfcPipelineResultVersions;
  const AStatus: TWfcPipelineResultStatus;
  const APassBacktracks: Integer;
  const AEvidenceKind: TWfcPipelineEvidenceKind;
  const AEvidenceSignature: TGraphTraceSignature;
  const AFailure: TWfcPipelineFailure;
  const APassOutcomes: TWfcPipelinePassOutcomes;
  const ALayers: TWfcPipelineResultLayers);
var
  I: Integer;
  J: Integer;
  LExecutedCount: Integer;
  LExecutionOrdinals: TBooleanArray;
  LExpectedLayerCount: Integer;
  LExpectedPassIndex: Integer;
  LTotalCellCount: Integer;
  LTotalTokenLength: Integer;
  LVocabulary: TWfcModelTokens;
  LQuota: TWfcPipelineValueQuota;
  LQuotaIndex, LQuotaCount, LLayerIndex, LQuotaPassIndex: Integer;
  LQuotaLookups: array of TWfcTokenLookup;
  LQuotaCounts: array of array of Integer;
  LConnectivity: TWfcPipelineConnectivity;
  LConnectivityIndex, LFailedEntry: Integer;
  LPassCells, LConsumerLayer, LProviderLayer, LRequirementIndex: Integer;
  LRequirement: TWfcPipelineRequirement;
begin
  inherited Create;
  if not Assigned(ARecipe) then
    raise EWfcPipelineResult.Create('result recipe cannot be nil');
  if not Assigned(ARun) then
    raise EWfcPipelineResult.Create('result run cannot be nil');
  if ARun.RecipeSignature <> ARecipe.Signature then
    raise EWfcPipelineResult.Create(
      'result recipe does not match its run provenance');
  { Own all-pass layouts, including private failure owners, before layers. }
  try
    FLayouts := ResolveWfcPipelineLayoutTable(ARecipe, ARun.CopyPassExtents);
    for I := 0 to FLayouts.PassCount - 1 do
      if not SameWfcLatticeLayout(FLayouts.PassLayoutAt(I), ARun.PassLayoutAt(I)) or
          (FLayouts.PassTopologyAt(I).Rank <> ARun.PassTopologyAt(I).Rank) then
        raise EWfcPipelineResult.Create('result run layout binding differs');
  except
    on E: EWfcPipelineResult do raise;
    on E: Exception do raise EWfcPipelineResult.Create('result layouts: ' + E.Message);
  end;
  FFormatVersion := ARun.FormatVersion;
  ValidateResultStatus(AStatus);
  ValidateEvidenceKind(AEvidenceKind);
  ValidateVersions(ARecipe, AVersions);
  if (AStatus = wprsPassBacktrackLimit) and
      (ARun.Strategy <> wpssNegotiated) then
    raise EWfcPipelineResult.Create(
      'pass-backtrack-limit status requires a negotiated run');
  if (APassBacktracks < 0) or
      (APassBacktracks > ARun.MaxPassBacktracks) then
    raise EWfcPipelineResult.Create(
      'result pass-backtrack count exceeds the run limit');
  if (ARun.Strategy = wpssOneWay) and (APassBacktracks <> 0) then
    raise EWfcPipelineResult.Create(
      'one-way results require zero pass backtracks');
  if ARun.Strategy = wpssOneWay then
  begin
    if ARun.CaptureTrace then
    begin
      if AEvidenceKind <> wpekTrace then
        raise EWfcPipelineResult.Create(
          'trace-enabled one-way results require trace evidence');
    end
    else if (AEvidenceKind <> wpekNone) or (AEvidenceSignature <> 0) then
      raise EWfcPipelineResult.Create(
        'trace-disabled one-way results require empty evidence');
  end
  else if AEvidenceKind <> wpekNegotiationTranscript then
    raise EWfcPipelineResult.Create(
      'negotiated results require negotiation-transcript evidence');

  FRecipeSignature := ARecipe.Signature;
  FRunSignature := ARun.Signature;
  FVersions := AVersions;
  FWidth := ARun.Width;
  FHeight := ARun.Height;
  FDepth := ARun.Depth;
  FCellCount := CheckedCellCount(ARun);
  FSeed := ARun.Seed;
  FStrategy := ARun.Strategy;
  FMaxBacktracks := ARun.MaxBacktracks;
  FMaxPassBacktracks := ARun.MaxPassBacktracks;
  FCaptureTrace := ARun.CaptureTrace;
  FStatus := AStatus;
  FPassBacktracks := APassBacktracks;
  FEvidenceKind := AEvidenceKind;
  FEvidenceSignature := AEvidenceSignature;
  FFailure := AFailure;

  ValidateContradictionKind(FFailure.Kind);
  ValidateDirection(FFailure.Direction);
  if FStatus = wprsSolved then
  begin
    if (FFailure.Kind <> gckNone) or (FFailure.PassIndex <> -1) or
        (FFailure.EntryIndex <> -1) or (FFailure.NeighborIndex <> -1) or
        FFailure.HasDirection or (FFailure.Direction <> gdNorth) or
        (FFailure.DependencyPassIndex <> -1) then
      raise EWfcPipelineResult.Create(
        'solved result requires the canonical empty failure');
  end
  else
  begin
    if FFailure.Kind = gckNone then
      raise EWfcPipelineResult.Create(
        'failed result requires a contradiction kind');
    if (FFailure.PassIndex < 0) or
        (FFailure.PassIndex >= ARecipe.PassCount) then
      raise EWfcPipelineResult.Create('result failure pass is out of range');
    if (FFailure.EntryIndex < -1) or
        (FFailure.EntryIndex >= FLayouts.PassCellCount(FFailure.PassIndex)) then
      raise EWfcPipelineResult.Create('result failure entry is out of range');
    if (FFailure.NeighborIndex < -1) or
        (FFailure.NeighborIndex >= FLayouts.PassCellCount(FFailure.PassIndex)) then
      raise EWfcPipelineResult.Create(
        'result failure neighbor is out of range');
    if (not FFailure.HasDirection) and
        (FFailure.Direction <> gdNorth) then
      raise EWfcPipelineResult.Create(
        'directionless failure requires canonical north direction');
    if (FFailure.DependencyPassIndex < -1) or
        (FFailure.DependencyPassIndex >= ARecipe.PassCount) then
      raise EWfcPipelineResult.Create(
        'result failure dependency pass is out of range');
  end;

  if Length(APassOutcomes) <> ARecipe.PassCount then
    raise EWfcPipelineResult.Create(
      'result must contain one outcome for every recipe pass');
  SetLength(FPassOutcomes, Length(APassOutcomes));
  SetLength(LExecutionOrdinals, Length(APassOutcomes));
  LExecutedCount := 0;
  for I := 0 to Length(APassOutcomes) - 1 do
  begin
    if APassOutcomes[I].PassIndex <> I then
      raise EWfcPipelineResult.Create(
        'result pass outcomes must be complete and ordered');
    ValidateDisposition(APassOutcomes[I].Disposition);
    if (APassOutcomes[I].Decisions < 0) or
        (APassOutcomes[I].Propagations < 0) or
        (APassOutcomes[I].Contradictions < 0) or
        (APassOutcomes[I].Backtracks < 0) or
        (APassOutcomes[I].ExcludedAssignments < 0) then
      raise EWfcPipelineResult.CreateFmt(
        'result pass counters cannot be negative [%d]', [I]);
    if APassOutcomes[I].Executed then
    begin
      if (APassOutcomes[I].ExecutionOrdinal < 0) or
          (APassOutcomes[I].ExecutionOrdinal >= Length(APassOutcomes)) then
        raise EWfcPipelineResult.CreateFmt(
          'result execution ordinal is out of range [%d]', [I]);
      if LExecutionOrdinals[APassOutcomes[I].ExecutionOrdinal] then
        raise EWfcPipelineResult.Create(
          'result execution ordinals must be unique');
      LExecutionOrdinals[APassOutcomes[I].ExecutionOrdinal] := True;
      Inc(LExecutedCount);
      if APassOutcomes[I].Disposition in [gpdNotRun, gpdReused] then
        raise EWfcPipelineResult.CreateFmt(
          'executed pass has a non-executed disposition [%d]', [I]);
    end
    else
    begin
      if (APassOutcomes[I].ExecutionOrdinal <> -1) or
          (APassOutcomes[I].Disposition <> gpdNotRun) or
          (APassOutcomes[I].Decisions <> 0) or
          (APassOutcomes[I].Propagations <> 0) or
          (APassOutcomes[I].Contradictions <> 0) or
          (APassOutcomes[I].Backtracks <> 0) or
          (APassOutcomes[I].ExcludedAssignments <> 0) then
        raise EWfcPipelineResult.CreateFmt(
          'non-executed pass outcome is not canonical [%d]', [I]);
    end;
    FPassOutcomes[I] := APassOutcomes[I];
  end;
  for I := 0 to LExecutedCount - 1 do
    if not LExecutionOrdinals[I] then
      raise EWfcPipelineResult.Create(
        'result execution ordinals must be contiguous');
  if FStatus = wprsSolved then
  begin
    if LExecutedCount <> ARecipe.PassCount then
      raise EWfcPipelineResult.Create(
        'solved result requires every pass to execute');
    for I := 0 to Length(FPassOutcomes) - 1 do
      if FPassOutcomes[I].Disposition = gpdFailed then
        raise EWfcPipelineResult.Create(
          'solved result cannot contain a failed pass');
  end
  else if FPassOutcomes[FFailure.PassIndex].Disposition <> gpdFailed then
    raise EWfcPipelineResult.Create(
      'failed result must mark its failure pass as failed');

  LExpectedLayerCount := 0;
  for I := 0 to ARecipe.PassCount - 1 do
    if ARecipe.PassAt(I).Visibility = wppvPublic then
      Inc(LExpectedLayerCount);
  if LExpectedLayerCount > WFC_PIPELINE_RESULT_MAX_PUBLIC_LAYER_COUNT then
    raise EWfcPipelineResult.Create(
      'result public-layer count exceeds the version-1 limit');
  if FStatus = wprsSolved then
  begin
    if Length(ALayers) <> LExpectedLayerCount then
      raise EWfcPipelineResult.Create(
        'solved result must contain every public layer');
  end
  else if Length(ALayers) <> 0 then
    raise EWfcPipelineResult.Create(
      'failed result cannot contain partial public layers');

  { Check the complete public extent budget before cloning any layer tokens. }
  LTotalCellCount := 0;
  if FStatus = wprsSolved then
    for I := 0 to ARecipe.PassCount - 1 do
      if ARecipe.PassAt(I).Visibility = wppvPublic then
      begin
        LPassCells := FLayouts.PassCellCount(I);
        if LPassCells > WFC_PIPELINE_RESULT_MAX_TOTAL_PUBLIC_CELL_COUNT -
            LTotalCellCount then
          raise EWfcPipelineResult.Create('result public cells exceed the aggregate limit');
        Inc(LTotalCellCount, LPassCells);
      end;

  LTotalCellCount := 0;
  LTotalTokenLength := 0;
  LExpectedPassIndex := 0;
  if ARecipe.ConnectivityCount <> 0 then
    try
      PreflightWfcPipelineConnectivity(ARecipe, FLayouts,
        LConnectivityIndex);
    except
      on E: EWfcPipelineConnectivity do
        raise EWfcPipelineResult.Create('result connectivity preflight: ' + E.Message);
    end;
  SetLength(FLayers, Length(ALayers));
  for I := 0 to Length(ALayers) - 1 do
  begin
    while (LExpectedPassIndex < ARecipe.PassCount) and
        (ARecipe.PassAt(LExpectedPassIndex).Visibility <> wppvPublic) do
      Inc(LExpectedPassIndex);
    if (LExpectedPassIndex >= ARecipe.PassCount) or
        (ALayers[I].PassIndex <> LExpectedPassIndex) then
      raise EWfcPipelineResult.Create(
        'result public layers must be complete and in recipe order');
    if ALayers[I].LabelName <>
        ARecipe.PassAt(LExpectedPassIndex).LabelName then
      raise EWfcPipelineResult.CreateFmt(
        'result layer label does not match its recipe pass [%d]', [I]);
    LPassCells := FLayouts.PassCellCount(LExpectedPassIndex);
    if Length(ALayers[I].Tokens) <> LPassCells then
      raise EWfcPipelineResult.CreateFmt(
        'result layer has the wrong cell count [%d]', [I]);
    if LPassCells > WFC_PIPELINE_RESULT_MAX_TOTAL_PUBLIC_CELL_COUNT -
        LTotalCellCount then
      raise EWfcPipelineResult.Create(
        'result public cells exceed the version-1 aggregate limit');
    Inc(LTotalCellCount, LPassCells);
    LVocabulary := ARecipe.CopyPublicVocabulary(LExpectedPassIndex);
    AddTokenLength(LTotalTokenLength, ALayers[I].LabelName,
      'result layer label');
    for J := 0 to Length(ALayers[I].Tokens) - 1 do
    begin
      if TokenIndex(LVocabulary, ALayers[I].Tokens[J]) < 0 then
        raise EWfcPipelineResult.CreateFmt(
          'result token is outside its public vocabulary [%d, %d]',
          [I, J]);
      AddTokenLength(LTotalTokenLength, ALayers[I].Tokens[J],
        'result cell token');
    end;
    FLayers[I] := CloneLayer(ALayers[I]);
    Inc(LExpectedPassIndex);
  end;
  { A signature is provenance, not proof of constraint satisfaction. Recount
    recipe quotas when constructing or decoding a solved artifact, even if
    the caller did not obtain its layers from our compiler. This is a quota
    validator, not a claim that result decoding re-solves all local rules. }
  if FStatus = wprsSolved then
  begin
    if ARecipe.ValueQuotaCount > 0 then
    begin
      SetLength(LQuotaLookups, ARecipe.PassCount);
      SetLength(LQuotaCounts, ARecipe.PassCount);
    end;
    try
      for LQuotaIndex := 0 to ARecipe.ValueQuotaCount - 1 do
      begin
        LQuota := ARecipe.ValueQuotaAt(LQuotaIndex);
        LQuotaPassIndex := LQuota.PassIndex;
        if not Assigned(LQuotaLookups[LQuotaPassIndex]) then
        begin
          LLayerIndex := -1;
          for I := 0 to Length(FLayers) - 1 do
            if FLayers[I].PassIndex = LQuotaPassIndex then
            begin
              LLayerIndex := I;
              Break;
            end;
          if LLayerIndex < 0 then
            raise EWfcPipelineResult.Create('result quota owner layer is absent');
          LQuotaLookups[LQuotaPassIndex] := TWfcTokenLookup.Create(
            ARecipe.CopyPublicVocabulary(LQuotaPassIndex));
          SetLength(LQuotaCounts[LQuotaPassIndex],
            LQuotaLookups[LQuotaPassIndex].Count);
          { Layers were already checked for complete, in-vocabulary tokens.
            This temporary histogram belongs only to this construction. }
          for J := 0 to Length(FLayers[LLayerIndex].Tokens) - 1 do
            Inc(LQuotaCounts[LQuotaPassIndex][
              LQuotaLookups[LQuotaPassIndex].Find(FLayers[LLayerIndex].Tokens[J])]);
        end;
        LQuotaCount := 0;
        for J := 0 to Length(LQuota.Values) - 1 do
          Inc(LQuotaCount, LQuotaCounts[LQuotaPassIndex][
            LQuotaLookups[LQuotaPassIndex].Find(LQuota.Values[J])]);
        if (LQuotaCount < LQuota.MinimumCount) or
            (LQuotaCount > LQuota.MaximumCount) then
          raise EWfcPipelineResult.CreateFmt(
            'result violates recipe value quota [%d, pass %d]',
            [LQuotaIndex, LQuota.PassIndex]);
      end;
    finally
      for I := 0 to Length(LQuotaLookups) - 1 do LQuotaLookups[I].Free;
    end;
  end;
  if (FStatus = wprsSolved) and (ARecipe.ConnectivityCount <> 0) then
    for LConnectivityIndex := 0 to ARecipe.ConnectivityCount - 1 do
    begin
      LConnectivity := ARecipe.ConnectivityAt(LConnectivityIndex);
      LLayerIndex := -1;
      for I := 0 to Length(FLayers) - 1 do
        if FLayers[I].PassIndex = LConnectivity.PassIndex then
        begin LLayerIndex := I; Break; end;
      if LLayerIndex < 0 then
        raise EWfcPipelineResult.Create('result connectivity owner layer is absent');
      try
        if not ValidateWfcPipelineConnectivity(ARecipe, LConnectivityIndex,
          FLayouts.PassLayoutAt(LConnectivity.PassIndex),
          FLayers[LLayerIndex].Tokens, LFailedEntry) then
          raise EWfcPipelineResult.CreateFmt(
            'result violates recipe connectivity [%d, pass %d, entry %d]',
            [LConnectivityIndex, LConnectivity.PassIndex, LFailedEntry]);
      except
        on E: EWfcPipelineConnectivity do
          raise EWfcPipelineResult.Create('result connectivity: ' + E.Message);
      end;
    end;
  { Public mapped clauses are independently checked on claimed solved layers.
    This does not replay hidden local rules or private latent assignments. }
  if FStatus = wprsSolved then
    for LRequirementIndex := 0 to ARecipe.RequirementCount - 1 do
    begin
      LRequirement := ARecipe.RequirementAt(LRequirementIndex);
      if LRequirement.Kind <> wprqMapped then Continue;
      LConsumerLayer := -1;
      LProviderLayer := -1;
      for I := 0 to Length(FLayers) - 1 do
      begin
        if FLayers[I].PassIndex = LRequirement.ConsumerPassIndex then LConsumerLayer := I;
        if FLayers[I].PassIndex = LRequirement.ProviderPassIndex then LProviderLayer := I;
      end;
      if (LConsumerLayer < 0) or (LProviderLayer < 0) then
        raise EWfcPipelineResult.Create('result mapped clause has an absent public layer');
      for I := 0 to Length(FLayers[LConsumerLayer].Tokens) - 1 do
        if FLayers[LConsumerLayer].Tokens[I] = LRequirement.ConsumerToken then
          try
            if not ValidateWfcPipelineMappedRequirement(LRequirement,
                FLayouts.PassLayoutAt(LRequirement.ConsumerPassIndex),
                FLayouts.PassLayoutAt(LRequirement.ProviderPassIndex), I,
                FLayers[LProviderLayer].Tokens) then
              raise EWfcPipelineResult.CreateFmt(
                'result violates public mapped clause [%d, pass %d, entry %d]',
                [LRequirementIndex, LRequirement.ConsumerPassIndex, I]);
          except
            on E: EWfcPipelineResult do raise;
            on E: Exception do raise EWfcPipelineResult.Create('result mapped clause: ' + E.Message);
          end;
    end;
  FSignature := CalculateSignature;
end;

destructor TWfcPipelineResult.Destroy;
begin
  FLayouts.Free;
  inherited Destroy;
end;

function TWfcPipelineResult.GetPassCount: Integer;
begin Result := FLayouts.PassCount; end;

function TWfcPipelineResult.GetTotalCellCount: Integer;
begin Result := FLayouts.TotalCellCount; end;

function TWfcPipelineResult.CopyPassLayouts: TWfcLatticeLayouts;
begin Result := FLayouts.CopyLayouts; end;

function TWfcPipelineResult.CopyPassExtents: TWfcPipelinePassExtents;
begin Result := FLayouts.CopyExtents; end;

function TWfcPipelineResult.PassLayoutAt(const APassIndex: Integer): TWfcLatticeLayout;
begin Result := FLayouts.PassLayoutAt(APassIndex); end;

function TWfcPipelineResult.PassTopologyAt(const APassIndex: Integer): TWfcPipelinePassTopology;
begin Result := FLayouts.PassTopologyAt(APassIndex); end;

function TWfcPipelineResult.LayerLayoutAt(const ALayerIndex: Integer): TWfcLatticeLayout;
begin
  ValidateLayerIndex(ALayerIndex);
  Result := FLayouts.PassLayoutAt(FLayers[ALayerIndex].PassIndex);
end;

function TWfcPipelineResult.PassCellCount(const APassIndex: Integer): Integer;
begin Result := FLayouts.PassCellCount(APassIndex); end;

function TWfcPipelineResult.PassOffsetAt(const APassIndex: Integer): Integer;
begin Result := FLayouts.PassOffsetAt(APassIndex); end;

function TWfcPipelineResult.GetPassOutcomeCount: Integer;
begin
  Result := Length(FPassOutcomes);
end;

function TWfcPipelineResult.GetLayerCount: Integer;
begin
  Result := Length(FLayers);
end;

procedure TWfcPipelineResult.ValidatePassOutcomeIndex(
  const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Length(FPassOutcomes)) then
    raise EWfcPipelineResult.CreateFmt(
      'result pass-outcome index is out of range [%d]', [AIndex]);
end;

procedure TWfcPipelineResult.ValidateLayerIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Length(FLayers)) then
    raise EWfcPipelineResult.CreateFmt(
      'result layer index is out of range [%d]', [AIndex]);
end;

function TWfcPipelineResult.CopyVersions: TWfcPipelineResultVersions;
begin
  Result := FVersions;
end;

function TWfcPipelineResult.CopyFailure: TWfcPipelineFailure;
begin
  Result := FFailure;
end;

function TWfcPipelineResult.PassOutcomeAt(
  const AIndex: Integer): TWfcPipelinePassOutcome;
begin
  ValidatePassOutcomeIndex(AIndex);
  Result := FPassOutcomes[AIndex];
end;

function TWfcPipelineResult.LayerAt(
  const AIndex: Integer): TWfcPipelineResultLayer;
begin
  ValidateLayerIndex(AIndex);
  Result := CloneLayer(FLayers[AIndex]);
end;

function TWfcPipelineResult.CopyPassOutcomes: TWfcPipelinePassOutcomes;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FPassOutcomes));
  for I := 0 to Length(FPassOutcomes) - 1 do
    Result[I] := FPassOutcomes[I];
end;

function TWfcPipelineResult.CopyLayers: TWfcPipelineResultLayers;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FLayers));
  for I := 0 to Length(FLayers) - 1 do
    Result[I] := CloneLayer(FLayers[I]);
end;

end.
