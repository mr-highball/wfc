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
unit wfc_text_passes;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_sequence,
  wfc_sequence_graph,
  wfc_trace;

const
  WFC_TEXT_PASS_PIPELINE_VERSION = 1;
  WFC_TEXT_PASS_FRAGMENT_VERSION = 1;

  WFC_TEXT_PASS_STRUCTURE = 'structure';
  WFC_TEXT_PASS_LEXICAL = 'lexical';
  WFC_TEXT_PASS_PUNCTUATION = 'punctuation';

type
  EWfcTextPasses = class(EWfcSequenceGraph);

  TWfcTextPassLayer = (
    wtplStructure,
    wtplLexical,
    wtplPunctuation
  );

  TWfcTextPassModels = record
    Structure: TWfcSequenceModel;
    Lexical: TWfcSequenceModel;
    Punctuation: TWfcSequenceModel;
  end;

  (*
    Every relation is complete over the target model's public vocabulary.
    The punctuation pass depends directly on both earlier passes. This keeps
    its lexical and structural obligations as two ANDed dependency groups,
    while SourceTokens inside one rule remain OR alternatives.
  *)
  TWfcTextPassProjectionMaps = record
    LexicalFromStructure: TWfcSequenceProjectionRules;
    PunctuationFromLexical: TWfcSequenceProjectionRules;
    PunctuationFromStructure: TWfcSequenceProjectionRules;
  end;

  TWfcTextPassConfig = record
    TokenLength: Integer;
    Extent: TWfcSequenceExtent;
    Seed: TGraphSeed;
    Models: TWfcTextPassModels;
    Maps: TWfcTextPassProjectionMaps;
  end;

  TWfcTextPassResult = record
    Structure: TWfcGeneratedSequence;
    Lexical: TWfcGeneratedSequence;
    Punctuation: TWfcGeneratedSequence;
    Text: TWfcModelToken;
  end;

  TWfcTextPassRelation = (
    wtprNone,
    wtprLexicalFromStructure,
    wtprPunctuationFromLexical,
    wtprPunctuationFromStructure
  );

  TWfcTextPassValidationIssueKind = (
    wtpvikNone,
    wtpvikLength,
    wtpvikExtent,
    wtpvikBoundary,
    wtpvikStatePath,
    wtpvikStateProjection,
    wtpvikCallerConstraint,
    wtpvikPassProjection,
    wtpvikRendering
  );

  TWfcTextPassValidationIssue = record
    Kind: TWfcTextPassValidationIssueKind;
    Layer: TWfcTextPassLayer;
    Relation: TWfcTextPassRelation;
    Position: Integer;
    SequenceIssue: TWfcSequenceGraphIssue;
  end;

  TWfcTextPassValidationReport = record
    Valid: Boolean;
    CheckedLayers: Integer;
    CheckedTokens: Integer;
    CheckedRelations: Integer;
    Issue: TWfcTextPassValidationIssue;
  end;

  TWfcTextPassStatus = (
    wtpsNotRun,
    wtpsCompleted,
    wtpsSolveFailed,
    wtpsTraceFailed,
    wtpsCaptureFailed,
    wtpsValidationFailed
  );

  TWfcTextPassCaptureReports =
    array[TWfcTextPassLayer] of TWfcSequenceGraphValidationReport;

  { A public-token projection of one solver event. StateIndex preserves the
    stable numeric latent identity, but the graph-private string key is never
    copied out of the owner. PassIndex is -1 for pipeline-wide events. }
  TWfcTextPassTraceEvent = record
    EventId: Integer;
    CauseEventId: Integer;
    Kind: TGraphTraceEventKind;
    CauseKind: TGraphTraceCauseKind;
    PassIndex: Integer;
    EntryIndex: Integer;
    StateIndex: Integer;
    Token: TWfcModelToken;
    NeighborIndex: Integer;
    HasDirection: Boolean;
    Direction: TGraphDirection;
    DependencyPassIndex: Integer;
    DecisionDepth: Integer;
    DomainCountBefore: Integer;
    DomainCountAfter: Integer;
  end;
  TWfcTextPassTraceEvents = array of TWfcTextPassTraceEvent;

  TWfcTextPassGraphValueDomains = array of TGraphValues;
  TWfcTextPassBooleanDomains = array of Boolean;

  TWfcTextPassReport = record
    Status: TWfcTextPassStatus;
    FailedLayer: TWfcTextPassLayer;
    Solve: TGraphSolveReport;
    Capture: TWfcTextPassCaptureReports;
    Validation: TWfcTextPassValidationReport;
    TraceCaptured: Boolean;
    TraceHash: TGraphTraceSignature;
    TraceValidation: TGraphTraceValidationReport;
    Trace: TWfcTextPassTraceEvents;
  end;

  { TWfcTextPassPipeline }

  (*
    A ready-to-use owner for structure -> lexical -> punctuation composition.
    Models remain caller-owned and must outlive the pipeline. The graph is
    exposed for advanced pass-DAG and solver inspection, while ordinary users
    can constrain each public layer without seeing private latent keys.

    Punctuation tokens are versioned encodings of final text fragments. A
    model may therefore emit fragments such as "The", " quiet", ".", or the
    empty string. Rendering decodes and concatenates them exactly and does not
    call a platform tokenizer or formatting API.
  *)
  TWfcTextPassPipeline = class
  strict private
    FExtent: TWfcSequenceExtent;
    FGraph: TGraph;
    FMaps: TWfcTextPassProjectionMaps;
    FModels: TWfcTextPassModels;
    FTokenLength: Integer;
    FDirtyFromIndex: Integer;
    FBaselineDomains:
      array[TWfcTextPassLayer] of TWfcTextPassGraphValueDomains;
    FBaselineHasDomains:
      array[TWfcTextPassLayer] of TWfcTextPassBooleanDomains;

    function GetLayerGraph(const ALayer: TWfcTextPassLayer): TGraph;
    function GetModel(const ALayer: TWfcTextPassLayer): TWfcSequenceModel;
    function GetSeed: TGraphSeed;
    procedure SetSeed(const AValue: TGraphSeed);
    procedure Initialize(const AConfig: TWfcTextPassConfig);
    procedure CaptureBaselineDomains;
    procedure MarkDirty(const ALayer: TWfcTextPassLayer);
    function FinishGeneration(const ASolved: Boolean;
      const ARawSolve: TGraphSolveReport;
      out AResult: TWfcTextPassResult;
      out AReport: TWfcTextPassReport): Boolean;
  public
    constructor Create(const AConfig: TWfcTextPassConfig);
    destructor Destroy; override;

    function IntersectAllowedTokens(const ALayer: TWfcTextPassLayer;
      const APosition: Integer;
      const ATokens: TWfcModelTokens): TWfcTextPassPipeline; overload;
    function IntersectAllowedTokens(const ALayer: TWfcTextPassLayer;
      const APosition: Integer;
      const AToken: TWfcModelToken): TWfcTextPassPipeline; overload;
    function IntersectTokenConstraints(const ALayer: TWfcTextPassLayer;
      const AConstraints: TWfcSequenceTokenConstraints):
      TWfcTextPassPipeline;
    function IntersectLockedSpan(const ALayer: TWfcTextPassLayer;
      const AStart: Integer;
      const ATokens: TWfcModelTokens): TWfcTextPassPipeline;
    function ClearAllowedTokens(const ALayer: TWfcTextPassLayer;
      const APosition: Integer): TWfcTextPassPipeline;

    function TryGenerate(const AOptions: TGraphSolveOptions;
      out AResult: TWfcTextPassResult;
      out AReport: TWfcTextPassReport): Boolean; overload;
    function TryGenerate(out AResult: TWfcTextPassResult;
      out AReport: TWfcTextPassReport): Boolean; overload;
    function TryRegenerateFrom(const ALayer: TWfcTextPassLayer;
      const AOptions: TGraphSolveOptions;
      out AResult: TWfcTextPassResult;
      out AReport: TWfcTextPassReport): Boolean; overload;
    function TryRegenerateFrom(const ALayer: TWfcTextPassLayer;
      out AResult: TWfcTextPassResult;
      out AReport: TWfcTextPassReport): Boolean; overload;

    function Validate(const AResult: TWfcTextPassResult;
      out AReport: TWfcTextPassValidationReport): Boolean;

    property Graph: TGraph read FGraph;
    property LayerGraph[const ALayer: TWfcTextPassLayer]: TGraph
      read GetLayerGraph;
    property Model[const ALayer: TWfcTextPassLayer]: TWfcSequenceModel
      read GetModel;
    property TokenLength: Integer read FTokenLength;
    property Extent: TWfcSequenceExtent read FExtent;
    property Seed: TGraphSeed read GetSeed write SetSeed;
  end;

function DefaultWfcTextPassConfig(const ATokenLength: Integer;
  const AExtent: TWfcSequenceExtent; const ASeed: TGraphSeed):
  TWfcTextPassConfig;

function WfcTextPassLayerName(const ALayer: TWfcTextPassLayer): String;
function WfcTextPassRelationName(const ARelation: TWfcTextPassRelation):
  String;

function RenderWfcTextPassFragments(
  const AFragments: TWfcModelTokens): TWfcModelToken;

function EncodeWfcTextPassFragment(
  const AText: TWfcModelToken): TWfcModelToken;
function DecodeWfcTextPassFragment(
  const AToken: TWfcModelToken): TWfcModelToken;

function DescribeWfcTextPassValidationIssue(
  const AIssue: TWfcTextPassValidationIssue): String;

implementation

uses
  wfc_text_codec;

const
  WFC_TEXT_PASS_FRAGMENT_PREFIX = '@wfctf1:';
  WFC_TEXT_PASS_FRAGMENT_ARTIFACT = 'WFC text pass fragment';

function SequenceExtentIsValid(const AExtent: TWfcSequenceExtent): Boolean;
var
  LOrdinal: Integer;
begin
  LOrdinal := Ord(AExtent);
  Result := (LOrdinal >= Ord(Low(TWfcSequenceExtent))) and
    (LOrdinal <= Ord(High(TWfcSequenceExtent)));
end;

function ExpectedBoundary(const AExtent: TWfcSequenceExtent):
  TWfcModelBoundary;
begin
  if AExtent = wseWrap then
    Result := wmbWrap
  else
    Result := wmbOpen;
end;

function WfcTextPassLayerName(const ALayer: TWfcTextPassLayer): String;
begin
  case ALayer of
    wtplStructure:
      Result := WFC_TEXT_PASS_STRUCTURE;
    wtplLexical:
      Result := WFC_TEXT_PASS_LEXICAL;
    wtplPunctuation:
      Result := WFC_TEXT_PASS_PUNCTUATION;
  else
    raise ERangeError.Create('unknown text pass layer');
  end;
end;

function WfcTextPassRelationName(const ARelation: TWfcTextPassRelation):
  String;
begin
  case ARelation of
    wtprNone:
      Result := 'none';
    wtprLexicalFromStructure:
      Result := 'lexical-from-structure';
    wtprPunctuationFromLexical:
      Result := 'punctuation-from-lexical';
    wtprPunctuationFromStructure:
      Result := 'punctuation-from-structure';
  else
    raise ERangeError.Create('unknown text pass relation');
  end;
end;

function DefaultWfcTextPassConfig(const ATokenLength: Integer;
  const AExtent: TWfcSequenceExtent; const ASeed: TGraphSeed):
  TWfcTextPassConfig;
begin
  Result := Default(TWfcTextPassConfig);
  Result.TokenLength := ATokenLength;
  Result.Extent := AExtent;
  Result.Seed := ASeed;
end;

function CopyProjectionRules(const ARules: TWfcSequenceProjectionRules):
  TWfcSequenceProjectionRules;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ARules));
  for I := 0 to Length(ARules) - 1 do
    Result[I] := MakeWfcSequenceProjectionRule(ARules[I].TargetToken,
      ARules[I].SourceTokens);
end;

function EncodeWfcTextPassFragment(
  const AText: TWfcModelToken): TWfcModelToken;
var
  LEncoded: String;
begin
  LEncoded := WfcTextEncodeToken(AText,
    WFC_TEXT_PASS_FRAGMENT_ARTIFACT);
  Result := TWfcModelToken(WFC_TEXT_PASS_FRAGMENT_PREFIX + LEncoded);
end;

function DecodeWfcTextPassFragment(
  const AToken: TWfcModelToken): TWfcModelToken;
var
  LEncoded: String;
  LText: String;
begin
  LText := String(AToken);
  if Copy(LText, 1, Length(WFC_TEXT_PASS_FRAGMENT_PREFIX)) <>
      WFC_TEXT_PASS_FRAGMENT_PREFIX then
    raise EWfcTextPasses.Create(
      'text pass fragment has an unknown encoding');
  LEncoded := Copy(LText, Length(WFC_TEXT_PASS_FRAGMENT_PREFIX) + 1,
    Length(LText) - Length(WFC_TEXT_PASS_FRAGMENT_PREFIX));
  try
    Result := WfcTextDecodeToken(LEncoded,
      WFC_TEXT_PASS_FRAGMENT_ARTIFACT);
  except
    on E: EConvertError do
      raise EWfcTextPasses.Create(E.Message);
  end;
  if EncodeWfcTextPassFragment(Result) <> AToken then
    raise EWfcTextPasses.Create(
      'text pass fragment is not canonical');
end;

function RenderWfcTextPassFragments(
  const AFragments: TWfcModelTokens): TWfcModelToken;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(AFragments) - 1 do
    Result := Result + DecodeWfcTextPassFragment(AFragments[I]);
end;

procedure ValidateTextPassFragmentVocabulary(
  const AModel: TWfcSequenceModel);
var
  I: Integer;
begin
  for I := 0 to AModel.PublicTokenCount - 1 do
    try
      DecodeWfcTextPassFragment(AModel.PublicTokenAt(I));
    except
      on EWfcTextPasses do
        raise EArgumentException.CreateFmt(
          'punctuation public token %d is not a canonical text fragment',
          [I]);
    end;
end;

constructor TWfcTextPassPipeline.Create(const AConfig: TWfcTextPassConfig);
begin
  inherited Create;
  Initialize(AConfig);
end;

procedure TWfcTextPassPipeline.Initialize(const AConfig: TWfcTextPassConfig);
var
  LBindings: TWfcSequenceProjectionBindings;
begin
  if AConfig.TokenLength < 1 then
    raise ERangeError.CreateFmt(
      'text pass pipeline length must be positive [%d]',
      [AConfig.TokenLength]);
  if not SequenceExtentIsValid(AConfig.Extent) then
    raise EArgumentException.CreateFmt(
      'text pass pipeline extent is invalid [%d]', [Ord(AConfig.Extent)]);
  if not Assigned(AConfig.Models.Structure) then
    raise EArgumentNilException.Create('text structure model cannot be nil');
  if not Assigned(AConfig.Models.Lexical) then
    raise EArgumentNilException.Create('text lexical model cannot be nil');
  if not Assigned(AConfig.Models.Punctuation) then
    raise EArgumentNilException.Create(
      'text punctuation model cannot be nil');
  ValidateTextPassFragmentVocabulary(AConfig.Models.Punctuation);

  FTokenLength := AConfig.TokenLength;
  FExtent := AConfig.Extent;
  FDirtyFromIndex := -1;
  FModels := AConfig.Models;
  FMaps.LexicalFromStructure := CopyProjectionRules(
    AConfig.Maps.LexicalFromStructure);
  FMaps.PunctuationFromLexical := CopyProjectionRules(
    AConfig.Maps.PunctuationFromLexical);
  FMaps.PunctuationFromStructure := CopyProjectionRules(
    AConfig.Maps.PunctuationFromStructure);

  FGraph := TGraph.Create;
  try
    FGraph.Reshape(FTokenLength, 1, 1);
    FGraph.WrapNeighbors := FExtent = wseWrap;
    FGraph.Seed := AConfig.Seed;

    FGraph.CurrentPass := WFC_TEXT_PASS_STRUCTURE;
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies;
    ApplySequenceModelToGraph(FModels.Structure, FGraph, FExtent);

    FGraph.SwitchToPass(WFC_TEXT_PASS_LEXICAL);
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies;
    ApplySequenceModelToGraph(FModels.Lexical, FGraph, FExtent);
    ValidateSequenceProjectionMapFromPass(FModels.Lexical,
      FModels.Structure, FGraph, WFC_TEXT_PASS_STRUCTURE,
      FMaps.LexicalFromStructure);
    RequireSequenceProjectionMapFromPass(FModels.Lexical,
      FModels.Structure, FGraph, WFC_TEXT_PASS_STRUCTURE,
      FMaps.LexicalFromStructure);

    FGraph.SwitchToPass(WFC_TEXT_PASS_PUNCTUATION);
    FGraph.PassMode := gpmOverlay;
    FGraph.ClearDependencies;
    ApplySequenceModelToGraph(FModels.Punctuation, FGraph, FExtent);
    SetLength(LBindings, 2);
    LBindings[0] := MakeWfcSequenceProjectionBinding(FModels.Lexical,
      WFC_TEXT_PASS_LEXICAL, FMaps.PunctuationFromLexical);
    LBindings[1] := MakeWfcSequenceProjectionBinding(FModels.Structure,
      WFC_TEXT_PASS_STRUCTURE, FMaps.PunctuationFromStructure);
    RequireSequenceProjectionMapsFromPasses(FModels.Punctuation,
      FGraph, LBindings);

    CaptureBaselineDomains;
    FGraph.SwitchToPass(WFC_TEXT_PASS_STRUCTURE);
  except
    FGraph.Free;
    FGraph := nil;
    raise;
  end;
end;

procedure TWfcTextPassPipeline.CaptureBaselineDomains;
var
  I: Integer;
  LGraph: TGraph;
  LLayer: TWfcTextPassLayer;
begin
  for LLayer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
  begin
    SetLength(FBaselineDomains[LLayer], FTokenLength);
    SetLength(FBaselineHasDomains[LLayer], FTokenLength);
    LGraph := GetLayerGraph(LLayer);
    for I := 0 to FTokenLength - 1 do
    begin
      FBaselineHasDomains[LLayer][I] :=
        LGraph.HasAllowedValues(I, 0, 0);
      if FBaselineHasDomains[LLayer][I] then
        FBaselineDomains[LLayer][I] :=
          LGraph.CopyAllowedValues(I, 0, 0)
      else
        FBaselineDomains[LLayer][I] := nil;
    end;
  end;
end;

destructor TWfcTextPassPipeline.Destroy;
begin
  FGraph.Free;
  inherited Destroy;
end;

function TWfcTextPassPipeline.GetLayerGraph(
  const ALayer: TWfcTextPassLayer): TGraph;
begin
  { Validate enum values consistently before relying on their pass order. }
  WfcTextPassLayerName(ALayer);
  Result := FGraph.PassGraph[Ord(ALayer)];
end;

function TWfcTextPassPipeline.GetModel(
  const ALayer: TWfcTextPassLayer): TWfcSequenceModel;
begin
  case ALayer of
    wtplStructure:
      Result := FModels.Structure;
    wtplLexical:
      Result := FModels.Lexical;
    wtplPunctuation:
      Result := FModels.Punctuation;
  else
    raise ERangeError.Create('unknown text pass layer');
  end;
end;

function TWfcTextPassPipeline.GetSeed: TGraphSeed;
begin
  Result := FGraph.Seed;
end;

procedure TWfcTextPassPipeline.SetSeed(const AValue: TGraphSeed);
begin
  FGraph.Seed := AValue;
end;

function TWfcTextPassPipeline.IntersectAllowedTokens(
  const ALayer: TWfcTextPassLayer; const APosition: Integer;
  const ATokens: TWfcModelTokens): TWfcTextPassPipeline;
begin
  IntersectSequenceAllowedTokens(GetModel(ALayer), GetLayerGraph(ALayer),
    APosition, ATokens);
  MarkDirty(ALayer);
  Result := Self;
end;

function TWfcTextPassPipeline.IntersectAllowedTokens(
  const ALayer: TWfcTextPassLayer; const APosition: Integer;
  const AToken: TWfcModelToken): TWfcTextPassPipeline;
begin
  IntersectSequenceAllowedTokens(GetModel(ALayer), GetLayerGraph(ALayer),
    APosition, AToken);
  MarkDirty(ALayer);
  Result := Self;
end;

function TWfcTextPassPipeline.IntersectTokenConstraints(
  const ALayer: TWfcTextPassLayer;
  const AConstraints: TWfcSequenceTokenConstraints): TWfcTextPassPipeline;
begin
  IntersectSequenceTokenConstraints(GetModel(ALayer), GetLayerGraph(ALayer),
    AConstraints);
  MarkDirty(ALayer);
  Result := Self;
end;

function TWfcTextPassPipeline.IntersectLockedSpan(
  const ALayer: TWfcTextPassLayer; const AStart: Integer;
  const ATokens: TWfcModelTokens): TWfcTextPassPipeline;
begin
  IntersectSequenceLockedSpan(GetModel(ALayer), GetLayerGraph(ALayer),
    AStart, ATokens);
  MarkDirty(ALayer);
  Result := Self;
end;

function TWfcTextPassPipeline.ClearAllowedTokens(
  const ALayer: TWfcTextPassLayer;
  const APosition: Integer): TWfcTextPassPipeline;
begin
  if (APosition < 0) or (APosition >= FTokenLength) then
    raise ERangeError.CreateFmt(
      'text pass position is out of bounds [%d]', [APosition]);
  GetLayerGraph(ALayer).ClearAllowedValues(APosition, 0, 0);
  if FBaselineHasDomains[ALayer][APosition] then
    GetLayerGraph(ALayer).SetAllowedValues(APosition, 0, 0,
      FBaselineDomains[ALayer][APosition]);
  MarkDirty(ALayer);
  Result := Self;
end;

procedure TWfcTextPassPipeline.MarkDirty(
  const ALayer: TWfcTextPassLayer);
var
  LIndex: Integer;
begin
  WfcTextPassLayerName(ALayer);
  LIndex := Ord(ALayer);
  if (FDirtyFromIndex < 0) or (LIndex < FDirtyFromIndex) then
    FDirtyFromIndex := LIndex;
end;

function ResultLayer(const AResult: TWfcTextPassResult;
  const ALayer: TWfcTextPassLayer): TWfcGeneratedSequence;
begin
  case ALayer of
    wtplStructure:
      Result := AResult.Structure;
    wtplLexical:
      Result := AResult.Lexical;
    wtplPunctuation:
      Result := AResult.Punctuation;
  else
    raise ERangeError.Create('unknown text pass layer');
  end;
end;

procedure InitializeValidationReport(
  out AReport: TWfcTextPassValidationReport);
begin
  AReport := Default(TWfcTextPassValidationReport);
  AReport.Issue.Layer := wtplStructure;
  AReport.Issue.Relation := wtprNone;
  AReport.Issue.Position := -1;
  AReport.Issue.SequenceIssue.Position := -1;
  AReport.Issue.SequenceIssue.RelatedPosition := -1;
  AReport.Issue.SequenceIssue.StateIndex := -1;
  AReport.Issue.SequenceIssue.RelatedStateIndex := -1;
end;

procedure SetValidationIssue(var AReport: TWfcTextPassValidationReport;
  const AKind: TWfcTextPassValidationIssueKind;
  const ALayer: TWfcTextPassLayer; const APosition: Integer;
  const ARelation: TWfcTextPassRelation);
begin
  AReport.Valid := False;
  AReport.Issue.Kind := AKind;
  AReport.Issue.Layer := ALayer;
  AReport.Issue.Position := APosition;
  AReport.Issue.Relation := ARelation;
end;

function ProjectionAllows(const ARules: TWfcSequenceProjectionRules;
  const ATarget, ASource: TWfcModelToken): Boolean;
var
  I: Integer;
  J: Integer;
begin
  for I := 0 to Length(ARules) - 1 do
    if ARules[I].TargetToken = ATarget then
    begin
      for J := 0 to Length(ARules[I].SourceTokens) - 1 do
        if ARules[I].SourceTokens[J] = ASource then
          Exit(True);
      Exit(False);
    end;
  Result := False;
end;

function TWfcTextPassPipeline.Validate(const AResult: TWfcTextPassResult;
  out AReport: TWfcTextPassValidationReport): Boolean;
var
  I: Integer;
  LActualText: TWfcModelToken;
  LBoundary: TWfcModelBoundary;
  LGenerated: TWfcGeneratedSequence;
  LLayer: TWfcTextPassLayer;
  LModel: TWfcSequenceModel;
  LConstraintPosition: Integer;
  LSequenceReport: TWfcSequenceGraphValidationReport;
begin
  InitializeValidationReport(AReport);
  LBoundary := ExpectedBoundary(FExtent);

  for LLayer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
  begin
    LGenerated := ResultLayer(AResult, LLayer);
    LModel := GetModel(LLayer);
    if (Length(LGenerated.StateIndices) <> FTokenLength) or
        (Length(LGenerated.Tokens) <> FTokenLength) then
    begin
      SetValidationIssue(AReport, wtpvikLength, LLayer, -1, wtprNone);
      Exit(False);
    end;
    if LGenerated.Extent <> FExtent then
    begin
      SetValidationIssue(AReport, wtpvikExtent, LLayer, -1, wtprNone);
      Exit(False);
    end;
    if LGenerated.Boundary <> LBoundary then
    begin
      SetValidationIssue(AReport, wtpvikBoundary, LLayer, -1, wtprNone);
      Exit(False);
    end;
    if not ValidateSequenceStatePath(LModel, LGenerated.StateIndices,
        FExtent, LSequenceReport) then
    begin
      SetValidationIssue(AReport, wtpvikStatePath, LLayer,
        LSequenceReport.Issue.Position, wtprNone);
      AReport.Issue.SequenceIssue := LSequenceReport.Issue;
      Exit(False);
    end;
    if not SequenceStatesSatisfyEntryConstraints(LModel,
        GetLayerGraph(LLayer), LGenerated.StateIndices,
        LConstraintPosition) then
    begin
      SetValidationIssue(AReport, wtpvikCallerConstraint, LLayer,
        LConstraintPosition, wtprNone);
      Exit(False);
    end;
    for I := 0 to FTokenLength - 1 do
    begin
      if LModel.ProjectStateToken(LGenerated.StateIndices[I]) <>
          LGenerated.Tokens[I] then
      begin
        SetValidationIssue(AReport, wtpvikStateProjection, LLayer, I,
          wtprNone);
        Exit(False);
      end;
      Inc(AReport.CheckedTokens);
    end;
    Inc(AReport.CheckedLayers);
  end;

  for I := 0 to FTokenLength - 1 do
  begin
    if not ProjectionAllows(FMaps.LexicalFromStructure,
        AResult.Lexical.Tokens[I], AResult.Structure.Tokens[I]) then
    begin
      SetValidationIssue(AReport, wtpvikPassProjection, wtplLexical, I,
        wtprLexicalFromStructure);
      Exit(False);
    end;
    Inc(AReport.CheckedRelations);

    if not ProjectionAllows(FMaps.PunctuationFromLexical,
        AResult.Punctuation.Tokens[I], AResult.Lexical.Tokens[I]) then
    begin
      SetValidationIssue(AReport, wtpvikPassProjection, wtplPunctuation, I,
        wtprPunctuationFromLexical);
      Exit(False);
    end;
    Inc(AReport.CheckedRelations);

    if not ProjectionAllows(FMaps.PunctuationFromStructure,
        AResult.Punctuation.Tokens[I], AResult.Structure.Tokens[I]) then
    begin
      SetValidationIssue(AReport, wtpvikPassProjection, wtplPunctuation, I,
        wtprPunctuationFromStructure);
      Exit(False);
    end;
    Inc(AReport.CheckedRelations);
  end;

  try
    LActualText := RenderWfcTextPassFragments(AResult.Punctuation.Tokens);
  except
    on EWfcTextPasses do
    begin
      SetValidationIssue(AReport, wtpvikRendering, wtplPunctuation, -1,
        wtprNone);
      Exit(False);
    end;
  end;
  if LActualText <> AResult.Text then
  begin
    SetValidationIssue(AReport, wtpvikRendering, wtplPunctuation, -1,
      wtprNone);
    Exit(False);
  end;

  AReport.Valid := True;
  AReport.Issue.Kind := wtpvikNone;
  Result := True;
end;

function TraceModel(const AModels: TWfcTextPassModels;
  const APassIndex: Integer): TWfcSequenceModel;
begin
  case APassIndex of
    Ord(wtplStructure):
      Result := AModels.Structure;
    Ord(wtplLexical):
      Result := AModels.Lexical;
    Ord(wtplPunctuation):
      Result := AModels.Punctuation;
  else
    raise EWfcTextPasses.CreateFmt(
      'text trace references an unknown pass [%d]', [APassIndex]);
  end;
end;

procedure ProjectTextPassTrace(const AModels: TWfcTextPassModels;
  const AEvents: TGraphTraceEvents; out AProjected: TWfcTextPassTraceEvents);
var
  I: Integer;
  LModel: TWfcSequenceModel;
begin
  AProjected := nil;
  SetLength(AProjected, Length(AEvents));
  for I := 0 to Length(AEvents) - 1 do
  begin
    AProjected[I].EventId := AEvents[I].EventId;
    AProjected[I].CauseEventId := AEvents[I].CauseEventId;
    AProjected[I].Kind := AEvents[I].Kind;
    AProjected[I].CauseKind := AEvents[I].CauseKind;
    AProjected[I].PassIndex := AEvents[I].PassIndex;
    AProjected[I].EntryIndex := AEvents[I].EntryIndex;
    AProjected[I].StateIndex := AEvents[I].ValueIndex;
    AProjected[I].Token := '';
    if AEvents[I].ValueIndex >= 0 then
    begin
      LModel := TraceModel(AModels, AEvents[I].PassIndex);
      if AEvents[I].ValueIndex >= LModel.StateCount then
        raise EWfcTextPasses.CreateFmt(
          'text trace state index is out of bounds [%d]', [I]);
      AProjected[I].Token :=
        LModel.ProjectStateToken(AEvents[I].ValueIndex);
    end;
    AProjected[I].NeighborIndex := AEvents[I].NeighborIndex;
    AProjected[I].HasDirection := AEvents[I].HasDirection;
    AProjected[I].Direction := AEvents[I].Direction;
    AProjected[I].DependencyPassIndex :=
      AEvents[I].DependencyPassIndex;
    AProjected[I].DecisionDepth := AEvents[I].DecisionDepth;
    AProjected[I].DomainCountBefore := AEvents[I].DomainCountBefore;
    AProjected[I].DomainCountAfter := AEvents[I].DomainCountAfter;
  end;
end;

function PublishTextPassSolveReport(const AGraph: TGraph;
  const AModels: TWfcTextPassModels; const ARaw: TGraphSolveReport;
  var AReport: TWfcTextPassReport): Boolean;
var
  I: Integer;
begin
  AReport.Solve := ARaw;
  AReport.TraceCaptured := ARaw.TraceCaptured;
  AReport.TraceHash := ARaw.TraceHash;
  AReport.Trace := nil;

  Result := True;
  if ARaw.TraceCaptured then
  begin
    Result := ValidateGraphTrace(AGraph, ARaw,
      AReport.TraceValidation);
    if Result then
      ProjectTextPassTrace(AModels, ARaw.Trace, AReport.Trace);
  end;

  { The generic trace contains private graph values. Keep numeric solve
    counters, but publish event data only through the sanitized text-domain
    projection above. The stripped generic report must retain its ordinary
    capture-disabled invariants. }
  AReport.Solve.TraceCaptured := False;
  AReport.Solve.TraceHash := 0;
  AReport.Solve.Trace := nil;
  for I := 0 to Length(AReport.Solve.Passes) - 1 do
  begin
    AReport.Solve.Passes[I].TraceStart := -1;
    AReport.Solve.Passes[I].TraceCount := 0;
  end;
end;

function TWfcTextPassPipeline.FinishGeneration(const ASolved: Boolean;
  const ARawSolve: TGraphSolveReport; out AResult: TWfcTextPassResult;
  out AReport: TWfcTextPassReport): Boolean;
var
  LLayer: TWfcTextPassLayer;
  LPendingResult: TWfcTextPassResult;
  LSequence: TWfcGeneratedSequence;
begin
  AResult := Default(TWfcTextPassResult);
  LPendingResult := Default(TWfcTextPassResult);
  AReport := Default(TWfcTextPassReport);
  AReport.Status := wtpsNotRun;
  AReport.FailedLayer := wtplStructure;

  if not PublishTextPassSolveReport(FGraph, FModels,
      ARawSolve, AReport) then
  begin
    AReport.Status := wtpsTraceFailed;
    Exit(False);
  end;
  if not ASolved then
  begin
    AReport.Status := wtpsSolveFailed;
    if (ARawSolve.FailedPassIndex >= Ord(Low(TWfcTextPassLayer))) and
        (ARawSolve.FailedPassIndex <= Ord(High(TWfcTextPassLayer))) then
      AReport.FailedLayer :=
        TWfcTextPassLayer(ARawSolve.FailedPassIndex);
    Exit(False);
  end;

  for LLayer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
  begin
    if not CaptureSolvedSequence(GetModel(LLayer), GetLayerGraph(LLayer),
        FExtent, LSequence, AReport.Capture[LLayer]) then
    begin
      AReport.Status := wtpsCaptureFailed;
      AReport.FailedLayer := LLayer;
      Exit(False);
    end;
    case LLayer of
      wtplStructure:
        LPendingResult.Structure := LSequence;
      wtplLexical:
        LPendingResult.Lexical := LSequence;
      wtplPunctuation:
        LPendingResult.Punctuation := LSequence;
    end;
  end;

  LPendingResult.Text := RenderWfcTextPassFragments(
    LPendingResult.Punctuation.Tokens);
  if not Validate(LPendingResult, AReport.Validation) then
  begin
    AReport.Status := wtpsValidationFailed;
    AReport.FailedLayer := AReport.Validation.Issue.Layer;
    Exit(False);
  end;

  AResult := LPendingResult;
  AReport.Status := wtpsCompleted;
  Result := True;
end;

function TWfcTextPassPipeline.TryGenerate(
  const AOptions: TGraphSolveOptions; out AResult: TWfcTextPassResult;
  out AReport: TWfcTextPassReport): Boolean;
var
  LRawSolve: TGraphSolveReport;
  LSolved: Boolean;
begin
  AResult := Default(TWfcTextPassResult);
  AReport := Default(TWfcTextPassReport);
  LSolved := FGraph.TrySolve(AOptions, LRawSolve);
  Result := FinishGeneration(LSolved, LRawSolve, AResult, AReport);
  if Result then
    FDirtyFromIndex := -1;
end;

function TWfcTextPassPipeline.TryGenerate(out AResult: TWfcTextPassResult;
  out AReport: TWfcTextPassReport): Boolean;
var
  LOptions: TGraphSolveOptions;
begin
  LOptions := DefaultGraphSolveOptions;
  Result := TryGenerate(LOptions, AResult, AReport);
end;

function TWfcTextPassPipeline.TryRegenerateFrom(
  const ALayer: TWfcTextPassLayer; const AOptions: TGraphSolveOptions;
  out AResult: TWfcTextPassResult;
  out AReport: TWfcTextPassReport): Boolean;
var
  LEffectiveLayer: TWfcTextPassLayer;
  LRawSolve: TGraphSolveReport;
  LSolved: Boolean;
begin
  AResult := Default(TWfcTextPassResult);
  AReport := Default(TWfcTextPassReport);
  WfcTextPassLayerName(ALayer);
  LEffectiveLayer := ALayer;
  if (FDirtyFromIndex >= Ord(Low(TWfcTextPassLayer))) and
      (FDirtyFromIndex < Ord(LEffectiveLayer)) then
    LEffectiveLayer := TWfcTextPassLayer(FDirtyFromIndex);
  LSolved := FGraph.TryRegenerateFrom(WfcTextPassLayerName(LEffectiveLayer),
    AOptions, LRawSolve);
  Result := FinishGeneration(LSolved, LRawSolve, AResult, AReport);
  if Result then
    FDirtyFromIndex := -1;
end;

function TWfcTextPassPipeline.TryRegenerateFrom(
  const ALayer: TWfcTextPassLayer; out AResult: TWfcTextPassResult;
  out AReport: TWfcTextPassReport): Boolean;
var
  LOptions: TGraphSolveOptions;
begin
  LOptions := DefaultGraphSolveOptions;
  Result := TryRegenerateFrom(ALayer, LOptions, AResult, AReport);
end;

function DescribeWfcTextPassValidationIssue(
  const AIssue: TWfcTextPassValidationIssue): String;
begin
  case AIssue.Kind of
    wtpvikNone:
      Result := 'no text pass validation issue';
    wtpvikLength:
      Result := Format('%s pass has the wrong token length',
        [WfcTextPassLayerName(AIssue.Layer)]);
    wtpvikExtent:
      Result := Format('%s pass has the wrong extent',
        [WfcTextPassLayerName(AIssue.Layer)]);
    wtpvikBoundary:
      Result := Format('%s pass has the wrong boundary',
        [WfcTextPassLayerName(AIssue.Layer)]);
    wtpvikStatePath:
      Result := Format('%s pass state path failed at %d: %s',
        [WfcTextPassLayerName(AIssue.Layer), AIssue.Position,
         DescribeSequenceGraphIssue(AIssue.SequenceIssue)]);
    wtpvikStateProjection:
      Result := Format('%s pass state/token projection failed at %d',
        [WfcTextPassLayerName(AIssue.Layer), AIssue.Position]);
    wtpvikCallerConstraint:
      Result := Format('%s pass violates a caller constraint at %d',
        [WfcTextPassLayerName(AIssue.Layer), AIssue.Position]);
    wtpvikPassProjection:
      Result := Format('%s failed at position %d',
        [WfcTextPassRelationName(AIssue.Relation), AIssue.Position]);
    wtpvikRendering:
      Result := 'punctuation fragments do not reproduce the rendered text';
  else
    Result := Format('unknown text pass validation issue [%d]',
      [Ord(AIssue.Kind)]);
  end;
end;

end.
