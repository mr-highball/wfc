{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
unit wfc_artifact_inspect;

{$mode delphi}{$H+}

interface

uses
  SysUtils, wfc_artifact_document;

const
  WFC_ARTIFACT_INSPECT_VERSION = 1;
  WFC_ARTIFACT_INSPECT_DEFAULT_LIMIT = 256;
  { A report is a bounded view, never a replacement for the canonical source.
    Exhaustion is explicit and does not change artifact validation. }
  WFC_ARTIFACT_INSPECT_MAX_REPORT_LENGTH = 16777216;

type
  EWfcArtifactInspect = class(Exception);

function WfcInspectArtifact(const ADocument: TWfcArtifactDocument;
  const ADetailLimit: Integer = WFC_ARTIFACT_INSPECT_DEFAULT_LIMIT): String;

implementation

uses
  wfc, wfc_model, wfc_rule_model, wfc_pattern2d, wfc_pattern3d, wfc_sequence,
  wfc_training, wfc_pipeline_model, wfc_pipeline_run, wfc_pipeline_result,
  wfc_text_codec, wfc_lattice, wfc_pipeline_layout;

type
  TInspectWriter = class
  private
    FLines: TWfcTextLines;
    FLineCount, FBytes, FShown, FLimit: Integer;
    FReason: String;
    procedure Line(const AText: String);
  public
    constructor Create(const ADocument: TWfcArtifactDocument;
      const ALimit: Integer);
    function Want: Boolean;
    function Detail(const AText: String): Boolean;
    function Finish: String;
  end;

function N(const AValue: Integer): String;
begin
  Result := IntToStr(AValue);
end;

function B(const AValue: Boolean): String;
begin
  if AValue then Result := 'true' else Result := 'false';
end;

function Token(const AValue: TWfcModelToken): String;
begin
  { Percent encoding prevents corpus tokens and metadata from injecting
    terminal control sequences, new records, or ambiguous field separators. }
  Result := WfcTextEncodeToken(AValue, 'artifact inspection');
end;

function XYZ(const AX, AY, AZ: Integer): String;
begin
  Result := N(AX) + ',' + N(AY) + ',' + N(AZ);
end;

function PositionText(const APosition: TGraphPosition): String;
begin
  Result := XYZ(APosition.X, APosition.Y, APosition.Z);
end;

function DirectionName(const AValue: TGraphDirection): String;
begin
  case AValue of
    gdNorth: Result := 'N'; gdEast: Result := 'E';
    gdSouth: Result := 'S'; gdWest: Result := 'W';
    gdUp: Result := 'U'; gdDown: Result := 'D';
  else raise EWfcArtifactInspect.Create('unknown direction'); end;
end;

function Ports(const AValue: TGraphDirections): String;
var D: TGraphDirection;
begin
  Result := '';
  for D := Low(TGraphDirection) to High(TGraphDirection) do
    if D in AValue then Result := Result + DirectionName(D);
  if Result = '' then Result := '-';
end;

function ResourceKind(const AValue: TWfcPipelineResourceKind): String;
begin
  case AValue of
    wprkRules: Result := 'rules'; wprkModel: Result := 'model';
    wprkPattern2D: Result := 'pattern2d'; wprkSequence: Result := 'sequence';
    wprkPattern3D: Result := 'pattern3d';
  else raise EWfcArtifactInspect.Create('unknown resource kind'); end;
end;

function AdapterKind(const AValue: TWfcPipelineAdapterKind): String;
begin
  case AValue of
    wpakEmpty: Result := 'empty'; wpakRules: Result := 'rules';
    wpakModel: Result := 'model'; wpakPattern2D: Result := 'pattern2d';
    wpakSequence: Result := 'sequence';
    wpakPattern3D: Result := 'pattern3d';
  else raise EWfcArtifactInspect.Create('unknown adapter kind'); end;
end;

function ModeName(const AValue: TGraphPassMode): String;
begin
  case AValue of
    gpmLegacy: Result := 'legacy'; gpmOverlay: Result := 'overlay';
    gpmTransform: Result := 'transform';
  else raise EWfcArtifactInspect.Create('unknown pass mode'); end;
end;

function ValidationScope(const ADocument: TWfcArtifactDocument): String;
begin
  case ADocument.Kind of
    wakRun: Result := 'canonical-and-recipe-bound;runtime-preflight=not-run';
    wakResult:
      begin
        Result := 'canonical-and-recipe-run-bound;solved-public-quotas-and-connectivity=';
        if ADocument.StoredResult.Status = wprsSolved then Result := Result + 'checked'
        else Result := Result + 'not-applicable';
        Result := Result + ';full-solution=not-proven';
        if ADocument.Recipe.HasPassMapping then
        begin
          Result := Result + ';solved-public-mapped-policies=';
          if ADocument.StoredResult.Status = wprsSolved then Result := Result + 'checked'
          else Result := Result + 'not-applicable';
        end;
      end;
    wakTraining: Result := 'canonical-source-contract;learning=not-run';
  else Result := 'canonical-static-contract;satisfiability=not-proven'; end;
end;

function BoundaryName(const AValue: TWfcModelBoundary): String;
begin
  case AValue of wmbOpen: Result := 'open'; wmbWrap: Result := 'wrap';
  else raise EWfcArtifactInspect.Create('unknown boundary'); end;
end;

function SymmetryName(const AValue: TWfcModelSymmetry): String;
begin
  case AValue of
    wmsNone: Result := 'none'; wmsD4: Result := 'd4';
    wmsCubeRotations: Result := 'cube24'; wmsCubeFull: Result := 'cube48';
  else raise EWfcArtifactInspect.Create('unknown symmetry'); end;
end;

function TrainingKindName(const AValue: TWfcTrainingKind): String;
begin
  case AValue of
    wtkAdjacency1D: Result := 'adjacency1d'; wtkAdjacency2D: Result := 'adjacency2d';
    wtkAdjacency3D: Result := 'adjacency3d'; wtkPattern2D: Result := 'pattern2d';
    wtkSequence: Result := 'sequence';
    wtkPattern3D: Result := 'pattern3d';
  else raise EWfcArtifactInspect.Create('unknown training kind'); end;
end;

function CountModeName(const AValue: TGraphPassCountMode): String;
begin
  case AValue of
    gpcmMatchingTerms: Result := 'matching-terms'; gpcmDistinctCells: Result := 'distinct-cells';
  else raise EWfcArtifactInspect.Create('unknown count mode'); end;
end;

function EvidenceName(const AValue: TWfcPipelineEvidenceKind): String;
begin
  case AValue of
    wpekNone: Result := 'none'; wpekTrace: Result := 'trace';
    wpekNegotiationTranscript: Result := 'negotiation-transcript';
  else raise EWfcArtifactInspect.Create('unknown evidence kind'); end;
end;

function DispositionName(const AValue: TGraphPassDisposition): String;
begin
  case AValue of
    gpdNotRun: Result := 'not-run'; gpdReused: Result := 'reused';
    gpdCleared: Result := 'cleared'; gpdCopied: Result := 'copied';
    gpdSolved: Result := 'solved'; gpdFailed: Result := 'failed';
  else raise EWfcArtifactInspect.Create('unknown pass disposition'); end;
end;

function FailureName(const AValue: TGraphContradictionKind): String;
begin
  case AValue of
    gckNone: Result := 'none'; gckEmptyDomain: Result := 'empty-domain';
    gckInvalidLock: Result := 'invalid-lock'; gckAdjacency: Result := 'adjacency';
    gckPreviousPass: Result := 'previous-pass'; gckRequiredSupport: Result := 'required-support';
    gckFinalValidation: Result := 'final-validation'; gckPassDependency: Result := 'pass-dependency';
    gckEntryDomain: Result := 'entry-domain'; gckExcludedAssignment: Result := 'excluded-assignment';
    gckConnectivity: Result := 'connectivity'; gckValueQuota: Result := 'value-quota';
  else raise EWfcArtifactInspect.Create('unknown contradiction kind'); end;
end;

procedure TInspectWriter.Line(const AText: String);
begin
  if FLineCount = Length(FLines) then
    SetLength(FLines, (FLineCount + 16) * 2);
  FLines[FLineCount] := AText;
  Inc(FLineCount);
  Inc(FBytes, Length(AText) + 1);
end;

constructor TInspectWriter.Create(const ADocument: TWfcArtifactDocument;
  const ALimit: Integer);
var LSummary: String;
begin
  inherited Create;
  FLimit := ALimit;
  FReason := 'none';
  Line('wfc-inspect=' + N(WFC_ARTIFACT_INSPECT_VERSION));
  Line('family=' + WfcArtifactKindName(ADocument.Kind));
  LSummary := ADocument.Summary;
  if (LSummary <> '') and (LSummary[Length(LSummary)] = #10) then
    Delete(LSummary, Length(LSummary), 1);
  Line('summary=' + LSummary);
  Line('validation=' + ValidationScope(ADocument));
  Line('execution=not-run');
  Line('detail-limit=' + N(ALimit));
end;

function TInspectWriter.Want: Boolean;
begin
  if (FReason = 'none') and (FShown >= FLimit) then
    FReason := 'record-limit';
  Result := FReason = 'none';
end;

function TInspectWriter.Detail(const AText: String): Boolean;
begin
  Result := False;
  if not Want then Exit;
  { Reserve more than the maximum three footer lines before accepting a row. }
  if Length(AText) > WFC_ARTIFACT_INSPECT_MAX_REPORT_LENGTH - FBytes - 128 then
  begin
    FReason := 'byte-limit';
    Exit;
  end;
  Line(AText);
  Inc(FShown);
  Result := True;
end;

function TInspectWriter.Finish: String;
begin
  Line('details-shown=' + N(FShown));
  Line('truncated=' + B(FReason <> 'none'));
  Line('truncation=' + FReason);
  SetLength(FLines, FLineCount);
  Result := WfcTextJoinCanonicalLines(FLines, 'artifact inspection');
end;

procedure InspectRules(const W: TInspectWriter; const M: TWfcRuleModel);
var I, J: Integer; LState: String;
begin
  if not W.Want then Exit;
  if not W.Detail('rules rank=' + N(M.Rank) + ' values=' + N(M.ValueCount) +
    ' rows=' + N(M.RuleCount) + ' absent-row=wildcard') then Exit;
  for I := 0 to M.ValueCount - 1 do
  begin
    if not W.Want then Exit;
    if not W.Detail('value index=' + N(I) + ' token=' + Token(M.TokenAt(I)) +
      ' weight=' + N(M.WeightAt(I))) then Exit;
  end;
  for I := 0 to M.RuleCount - 1 do
  begin
    if not W.Want then Exit;
    if M.RuleStateAt(I) = wrsAllow then LState := 'allow' else LState := 'deny';
    if not W.Detail('rule index=' + N(I) + ' owner=' + N(M.RuleOwnerAt(I)) +
      ' direction=' + DirectionName(M.RuleDirectionAt(I)) + ' state=' + LState +
      ' required=' + B(M.RuleRequiredAt(I)) +
      ' targets=' + N(M.RuleTargetCountAt(I))) then Exit;
    for J := 0 to M.RuleTargetCountAt(I) - 1 do
    begin
      if not W.Want then Exit;
      if not W.Detail('edge rule=' + N(I) + ' target=' + N(M.RuleTargetAt(I, J))) then Exit;
    end;
  end;
end;

procedure InspectModel(const W: TInspectWriter; const M: TWfcModel);
var I, J, LCount: Integer; D: TWfcModelDirection; S: TWfcModelSampleShape;
begin
  if not W.Want then Exit;
  if not W.Detail('model rank=' + N(M.Rank) + ' values=' + N(M.ValueCount) +
    ' samples=' + N(M.SampleCount) + ' boundary=' + BoundaryName(M.Boundary) +
    ' symmetry=' + SymmetryName(M.Symmetry) + ' counts=observations') then Exit;
  for I := 0 to M.SampleCount - 1 do
  begin
    if not W.Want then Exit;
    S := M.SampleShapeAt(I);
    if not W.Detail('sample index=' + N(I) + ' shape=' + XYZ(S.Width, S.Height, S.Depth)) then Exit;
  end;
  for I := 0 to M.ValueCount - 1 do
  begin
    if not W.Want then Exit;
    if not W.Detail('value index=' + N(I) + ' token=' + Token(M.TokenAt(I)) +
      ' weight=' + N(M.WeightAt(I))) then Exit;
  end;
  for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    if D in M.Directions then
      for I := 0 to M.ValueCount - 1 do
        for J := 0 to M.ValueCount - 1 do
        begin
          LCount := M.RelationCount(D, I, J);
          if LCount = 0 then Continue;
          if not W.Want then Exit;
          if not W.Detail('observation direction=' + DirectionName(TGraphDirection(Ord(D))) +
            ' source=' + N(I) + ' target=' + N(J) + ' count=' + N(LCount)) then Exit;
        end;
end;

procedure InspectPatterns(const W: TInspectWriter; const M: TWfcOverlappingModel2D);
var I, X, Y: Integer; S: TWfcModelSampleShape;
begin
  if not W.Want then Exit;
  if not W.Detail('pattern2d footprint=' + N(M.PatternWidth) + ',' + N(M.PatternHeight) +
    ' palette=' + N(M.PaletteCount) + ' patterns=' + N(M.PatternCount) +
    ' sources=' + N(M.SourceCount)) then Exit;
  for I := 0 to M.SourceCount - 1 do
  begin
    if not W.Want then Exit;
    S := M.SourceShapeAt(I);
    if not W.Detail('sample index=' + N(I) + ' shape=' + XYZ(S.Width, S.Height, S.Depth)) then Exit;
  end;
  for I := 0 to M.PaletteCount - 1 do
  begin
    if not W.Want then Exit;
    if not W.Detail('palette index=' + N(I) + ' token=' + Token(M.PaletteTokenAt(I))) then Exit;
  end;
  for I := 0 to M.PatternCount - 1 do
  begin
    if not W.Want then Exit;
    if not W.Detail('pattern index=' + N(I) + ' weight=' + N(M.PatternWeightAt(I))) then Exit;
    for Y := 0 to M.PatternHeight - 1 do
      for X := 0 to M.PatternWidth - 1 do
      begin
        if not W.Want then Exit;
        if not W.Detail('pattern-cell pattern=' + N(I) + ' xy=' + N(X) + ',' + N(Y) +
          ' palette=' + N(M.PatternPaletteIndexAt(I, X, Y))) then Exit;
      end;
  end;
end;

procedure InspectVolumePatterns(const W: TInspectWriter; const M: TWfcOverlappingModel3D);
var I,X,Y,Z: Integer; S: TWfcModelSampleShape;
begin
  if not W.Want then Exit;
  if not W.Detail('pattern3d footprint=' + XYZ(M.PatternWidth,M.PatternHeight,M.PatternDepth) +
    ' palette=' + N(M.PaletteCount) + ' patterns=' + N(M.PatternCount) +
    ' sources=' + N(M.SourceCount) + ' boundary=' + BoundaryName(M.SourceBoundary) +
    ' symmetry=' + SymmetryName(M.Symmetry) + ' relations=overlap') then Exit;
  for I:=0 to M.SourceCount-1 do
  begin
    if not W.Want then Exit;
    S:=M.SourceShapeAt(I);
    if not W.Detail('sample index=' + N(I) + ' shape=' + XYZ(S.Width,S.Height,S.Depth)) then Exit;
  end;
  for I:=0 to M.PaletteCount-1 do
  begin
    if not W.Want then Exit;
    if not W.Detail('palette index=' + N(I) + ' token=' + Token(M.PaletteTokenAt(I))) then Exit;
  end;
  for I:=0 to M.PatternCount-1 do
  begin
    if not W.Want then Exit;
    if not W.Detail('pattern index=' + N(I) + ' weight=' + N(M.PatternWeightAt(I))) then Exit;
    for Z:=0 to M.PatternDepth-1 do for Y:=0 to M.PatternHeight-1 do
      for X:=0 to M.PatternWidth-1 do
      begin
        if not W.Want then Exit;
        if not W.Detail('pattern-cell pattern=' + N(I) + ' xyz=' + XYZ(X,Y,Z) +
          ' palette=' + N(M.PatternPaletteIndexAt(I,X,Y,Z))) then Exit;
      end;
  end;
end;

procedure InspectSequence(const W: TInspectWriter; const M: TWfcSequenceModel);
var I, J: Integer; H: TWfcSequenceHistoryItem; LAtom: String;
begin
  if not W.Want then Exit;
  if not W.Detail('sequence order=' + N(M.Order) + ' tokens=' + N(M.PublicTokenCount) +
    ' states=' + N(M.StateCount) + ' samples=' + N(M.SampleCount)) then Exit;
  if M.Boundary = wmbWrap then
    if not W.Detail('sequence boundary=wrap model-version=' + N(M.ModelVersion)) then Exit;
  for I := 0 to M.SampleCount - 1 do
  begin
    if not W.Want then Exit;
    if not W.Detail('sample index=' + N(I) + ' length=' + N(M.SampleLengthAt(I))) then Exit;
  end;
  for I := 0 to M.PublicTokenCount - 1 do
  begin
    if not W.Want then Exit;
    if not W.Detail('value index=' + N(I) + ' token=' + Token(M.PublicTokenAt(I))) then Exit;
  end;
  for I := 0 to M.StateCount - 1 do
  begin
    if not W.Want then Exit;
    if not W.Detail('state index=' + N(I) + ' emitted=' + N(M.StateEmittedTokenIndexAt(I)) +
      ' count=' + N(M.StateObservationCountAt(I)) + ' starts=' + N(M.StartCountAt(I)) +
      ' ends=' + N(M.EndCountAt(I))) then Exit;
    for J := 0 to M.HistorySize - 1 do
    begin
      if not W.Want then Exit;
      H := M.HistoryItemAt(I, J);
      if H.Kind = wshBos then LAtom := 'BOS' else LAtom := N(H.TokenIndex);
      if not W.Detail('history state=' + N(I) + ' index=' + N(J) + ' atom=' + LAtom) then Exit;
    end;
  end;
end;

procedure InspectTraining(const W: TInspectWriter; const M: TWfcTrainingDocument);
var I, J: Integer; MD: TWfcTrainingMetadata; O: TWfcTrainingOptions;
  S: TWfcTrainingSample; Q: TWfcTrainingValueQuota; C: TWfcTrainingConnectivity;
  LFootprint: String;
begin
  if not W.Want then Exit;
  MD := M.CopyMetadata; O := M.CopyOptions;
  LFootprint := N(O.PatternWidth) + ',' + N(O.PatternHeight);
  if O.Kind=wtkPattern3D then LFootprint := LFootprint + ',' + N(O.PatternDepth);
  if not W.Detail('training name=' + Token(MD.Name) + ' license=' + Token(MD.LicenseIdentifier) +
    ' source=' + Token(MD.SourceDescription) + ' kind=' + TrainingKindName(O.Kind) +
    ' boundary=' + BoundaryName(O.Boundary) + ' symmetry=' + SymmetryName(O.Symmetry) +
    ' footprint=' + LFootprint + ' order=' + N(O.Order) +
    ' samples=' + N(M.SampleCount) + ' tokens=' + N(M.TotalTokenCount)) then Exit;
  for I := 0 to M.SampleCount - 1 do
  begin
    if not W.Want then Exit;
    S := M.SampleAt(I);
    if not W.Detail('sample index=' + N(I) + ' name=' + Token(S.Name) +
      ' shape=' + XYZ(S.Width, S.Height, S.Depth) + ' tokens=' + N(Length(S.Tokens))) then Exit;
  end;
  for I := 0 to M.ValueQuotaCount - 1 do
  begin
    if not W.Want then Exit;
    Q := M.ValueQuotaAt(I);
    if not W.Detail('quota index=' + N(I) + ' label=' + Token(Q.LabelText) +
      ' minimum=' + N(Q.MinimumCount) + ' maximum=' + N(Q.MaximumCount) +
      ' tokens=' + N(Length(Q.Values))) then Exit;
    for J := 0 to Length(Q.Values) - 1 do
    begin
      if not W.Want then Exit;
      if not W.Detail('quota-token quota=' + N(I) + ' index=' + N(J) + ' token=' + Token(Q.Values[J])) then Exit;
    end;
  end;
  for I := 0 to M.ConnectivityCount - 1 do
  begin
    if not W.Want then Exit;
    C := M.ConnectivityAt(I);
    if not W.Detail('network index=' + N(I) + ' label=' + Token(C.LabelText) +
      ' root=' + PositionText(C.Root) + ' all-participants=' + B(C.RequireAllParticipants) +
      ' terminals=' + N(Length(C.RequiredPositions)) + ' profiles=' + N(Length(C.Values))) then Exit;
    for J := 0 to Length(C.RequiredPositions) - 1 do
    begin
      if not W.Want then Exit;
      if not W.Detail('terminal network=' + N(I) + ' index=' + N(J) + ' xyz=' + PositionText(C.RequiredPositions[J])) then Exit;
    end;
    for J := 0 to Length(C.Values) - 1 do
    begin
      if not W.Want then Exit;
      if not W.Detail('profile network=' + N(I) + ' index=' + N(J) + ' token=' + Token(C.Values[J].Value) +
        ' ports=' + Ports(C.Values[J].Openings) + ' required=' + B(C.Values[J].RequiredByValue)) then Exit;
    end;
  end;
end;

procedure InspectRecipe(const W: TInspectWriter; const M: TWfcPipelineModel);
var I, J, K: Integer; MD: TWfcPipelineMetadata; R: TWfcPipelineResource;
  P: TWfcPipelinePass; D: TWfcPipelineDependency; G: TWfcPipelineBridge;
  Q: TWfcPipelineValueQuota; C: TWfcPipelineConnectivity;
  H: TWfcPipelineRequirement; V: TWfcModelTokens; LVisibility, LKind: String;
  Topology: TWfcPipelinePassTopology; MatchText: String;
begin
  if not W.Want then Exit;
  MD := M.CopyMetadata;
  if not W.Detail('recipe name=' + Token(MD.Name) + ' license=' + Token(MD.LicenseIdentifier) +
    ' source=' + Token(MD.SourceDescription) + ' fingerprint=' + Token(MD.SourceFingerprint) +
    ' rank=' + N(M.Rank) + ' wrap=' + B(M.WrapNeighbors)) then Exit;
  if M.HasPassMapping then
    if not W.Detail('spatial pass-mapping-version=' + N(M.PassMappingVersion) +
      ' graph-pass-mapping-version=' + N(WFC_PASS_MAPPING_VERSION) +
      ' global-topology=pass-zero-view;extents=invocation-owned') then Exit;
  for I := 0 to M.PassCount - 1 do
  begin
    if not W.Want then Exit;
    P := M.PassAt(I);
    if P.Visibility = wppvPublic then LVisibility := 'public' else LVisibility := 'private';
    if not W.Detail('pass index=' + N(I) + ' label=' + Token(P.LabelName) +
      ' visibility=' + LVisibility + ' mode=' + ModeName(P.Mode) + ' adapter=' + AdapterKind(P.AdapterKind) +
      ' resource=' + N(P.ResourceIndex) + ' transform-source=' + N(P.TransformSourceIndex)) then Exit;
    if M.HasPassMapping then
    begin
      if not W.Want then Exit;
      Topology := M.PassTopologyAt(I);
      if not W.Detail('pass-topology pass=' + N(I) + ' rank=' + N(Topology.Rank) +
        ' origin=' + XYZ(Topology.Origin.X,Topology.Origin.Y,Topology.Origin.Z) +
        ' pitch=' + XYZ(Topology.Pitch.X,Topology.Pitch.Y,Topology.Pitch.Z) +
        ' wrap=' + B(Topology.Wrap)) then Exit;
    end;
  end;
  for I := 0 to M.DependencyCount - 1 do
  begin
    if not W.Want then Exit;
    D := M.DependencyAt(I);
    if not W.Detail('dependency index=' + N(I) + ' provider=' + N(D.ProviderPassIndex) +
      ' consumer=' + N(D.ConsumerPassIndex)) then Exit;
  end;
  for I := 0 to M.ResourceCount - 1 do
  begin
    if not W.Want then Exit;
    R := M.ResourceAt(I);
    if not W.Detail('resource index=' + N(I) + ' id=' + Token(R.Id) + ' kind=' + ResourceKind(R.Kind) +
      ' bytes=' + N(Length(R.Document)) + ' license=' + Token(R.SourceLicenseIdentifier) +
      ' source=' + Token(R.SourceDescription) + ' fingerprint=' + Token(R.SourceFingerprint)) then Exit;
  end;
  for I := 0 to M.BridgeCount - 1 do
  begin
    if not W.Want then Exit;
    G := M.BridgeAt(I);
    case G.Kind of
      wpbkPattern2DProjection: LKind := 'pattern2d-projection';
      wpbkPattern3DProjection: LKind := 'pattern3d-projection';
      wpbkSequenceProjection: LKind := 'sequence-projection';
    else raise EWfcArtifactInspect.Create('unknown projection bridge'); end;
    if not W.Detail('bridge index=' + N(I) + ' kind=' + LKind +
      ' source=' + N(G.SourcePassIndex) + ' target=' + N(G.TargetPassIndex)) then Exit;
  end;
  for I := 0 to M.RequirementCount - 1 do
  begin
    if not W.Want then Exit;
    H := M.RequirementAt(I);
    if H.Kind = wprqMapped then
    begin
      case H.MappedQuery.Kind of
        gpmkPoint: LKind := 'point'; gpmkCellCoverage: LKind := 'cell';
        gpmkRegionCoverage: LKind := 'region';
      else raise EWfcArtifactInspect.Create('unknown mapped query kind'); end;
      if H.MappedQuery.Match = gpmmAll then MatchText := 'all' else MatchText := 'count';
      if not W.Detail('mapped-requirement index=' + N(I) + ' kind=' + LKind +
        ' consumer=' + N(H.ConsumerPassIndex) + ' token=' + Token(H.ConsumerToken) +
        ' provider=' + N(H.ProviderPassIndex) + ' match=' + MatchText +
        ' minimum-offset=' + XYZ(H.MappedQuery.MinimumOffset.DeltaX,
          H.MappedQuery.MinimumOffset.DeltaY,H.MappedQuery.MinimumOffset.DeltaZ) +
        ' maximum-offset=' + XYZ(H.MappedQuery.MaximumOffset.DeltaX,
          H.MappedQuery.MaximumOffset.DeltaY,H.MappedQuery.MaximumOffset.DeltaZ) +
        ' minimum=' + N(H.MappedQuery.MinimumMatches) +
        ' maximum=' + N(H.MappedQuery.MaximumMatches) +
        ' tokens=' + N(Length(H.MappedQuery.AllowedProviderTokens))) then Exit;
      for J := 0 to High(H.MappedQuery.AllowedProviderTokens) do
      begin
        if not W.Want then Exit;
        if not W.Detail('mapped-allowed requirement=' + N(I) + ' index=' + N(J) +
          ' token=' + Token(H.MappedQuery.AllowedProviderTokens[J])) then Exit;
      end;
      Continue;
    end;
    case H.Kind of wprqExact: LKind := 'exact'; wprqAny: LKind := 'any'; else LKind := 'count'; end;
    if not W.Detail('requirement index=' + N(I) + ' kind=' + LKind +
      ' consumer=' + N(H.ConsumerPassIndex) + ' token=' + Token(H.ConsumerToken) +
      ' provider=' + N(H.ProviderPassIndex) + ' terms=' + N(Length(H.Terms)) +
      ' count-mode=' + CountModeName(H.CountMode) + ' minimum=' + N(H.MinimumCount) +
      ' maximum=' + N(H.MaximumCount)) then Exit;
    for J := 0 to Length(H.Terms) - 1 do
    begin
      if not W.Want then Exit;
      if not W.Detail('term requirement=' + N(I) + ' index=' + N(J) +
        ' offset=' + XYZ(H.Terms[J].OffsetX, H.Terms[J].OffsetY, H.Terms[J].OffsetZ) +
        ' tokens=' + N(Length(H.Terms[J].AllowedProviderTokens))) then Exit;
      for K := 0 to Length(H.Terms[J].AllowedProviderTokens) - 1 do
      begin
        if not W.Want then Exit;
        if not W.Detail('allowed requirement=' + N(I) + ' term=' + N(J) + ' index=' + N(K) +
          ' token=' + Token(H.Terms[J].AllowedProviderTokens[K])) then Exit;
      end;
    end;
  end;
  for I := 0 to M.ValueQuotaCount - 1 do
  begin
    if not W.Want then Exit;
    Q := M.ValueQuotaAt(I);
    if not W.Detail('quota index=' + N(I) + ' pass=' + N(Q.PassIndex) + ' label=' + Token(Q.LabelText) +
      ' minimum=' + N(Q.MinimumCount) + ' maximum=' + N(Q.MaximumCount) + ' tokens=' + N(Length(Q.Values))) then Exit;
    for J := 0 to Length(Q.Values) - 1 do
    begin
      if not W.Want then Exit;
      if not W.Detail('quota-token quota=' + N(I) + ' index=' + N(J) + ' token=' + Token(Q.Values[J])) then Exit;
    end;
  end;
  for I := 0 to M.ConnectivityCount - 1 do
  begin
    if not W.Want then Exit;
    C := M.ConnectivityAt(I);
    if not W.Detail('network index=' + N(I) + ' pass=' + N(C.PassIndex) + ' label=' + Token(C.LabelText) +
      ' root=' + PositionText(C.Root) + ' all-participants=' + B(C.RequireAllParticipants) +
      ' terminals=' + N(Length(C.RequiredPositions)) + ' profiles=' + N(Length(C.Values))) then Exit;
    for J := 0 to Length(C.RequiredPositions) - 1 do
    begin
      if not W.Want then Exit;
      if not W.Detail('terminal network=' + N(I) + ' index=' + N(J) + ' xyz=' + PositionText(C.RequiredPositions[J])) then Exit;
    end;
    for J := 0 to Length(C.Values) - 1 do
    begin
      if not W.Want then Exit;
      if not W.Detail('profile network=' + N(I) + ' index=' + N(J) + ' token=' + Token(C.Values[J].Value) +
        ' ports=' + Ports(C.Values[J].Openings) + ' required=' + B(C.Values[J].RequiredByValue)) then Exit;
    end;
  end;
  for I := 0 to M.PassCount - 1 do
    if M.PassAt(I).Visibility = wppvPublic then
    begin
      if not W.Want then Exit;
      V := M.CopyPublicVocabulary(I);
      for J := 0 to Length(V) - 1 do
      begin
        if not W.Want then Exit;
        if not W.Detail('public-token pass=' + N(I) + ' index=' + N(J) + ' token=' + Token(V[J])) then Exit;
      end;
    end;
end;

procedure InspectRun(const W: TInspectWriter; const M: TWfcPipelineRun);
var I, J: Integer; L: TWfcPipelineCellLock; D: TWfcPipelineCellDomain; S: String;
  Layout: TWfcLatticeLayout;
begin
  if not W.Want then Exit;
  if M.Strategy = wpssOneWay then S := 'one-way' else S := 'negotiated';
  if not W.Detail('run shape=' + XYZ(M.Width, M.Height, M.Depth) + ' seed=' + UIntToStr(M.Seed) +
    ' strategy=' + S + ' local-backtracks=' + N(M.MaxBacktracks) +
    ' pass-backtracks=' + N(M.MaxPassBacktracks) + ' capture-trace=' + B(M.CaptureTrace) +
    ' locks=' + N(M.LockCount) + ' domains=' + N(M.DomainCount)) then Exit;
  if M.FormatVersion = 2 then
  begin
    if not W.Detail('run-layouts count=' + N(M.PassCount) +
      ' total-cells=' + N(M.TotalCellCount) + ' shape=pass-zero-view') then Exit;
    for I := 0 to M.PassCount - 1 do
    begin
      if not W.Want then Exit;
      Layout := M.PassLayoutAt(I);
      if not W.Detail('run-layout pass=' + N(I) + ' rank=' + N(M.PassTopologyAt(I).Rank) +
        ' cells=' + XYZ(Layout.Cells.X,Layout.Cells.Y,Layout.Cells.Z) +
        ' origin=' + XYZ(Layout.Origin.X,Layout.Origin.Y,Layout.Origin.Z) +
        ' pitch=' + XYZ(Layout.Pitch.X,Layout.Pitch.Y,Layout.Pitch.Z) +
        ' wrap=' + B(Layout.Wrap) + ' flat-offset=' + N(M.PassOffsetAt(I))) then Exit;
    end;
  end;
  for I := 0 to M.LockCount - 1 do
  begin
    if not W.Want then Exit;
    L := M.LockAt(I);
    if not W.Detail('lock index=' + N(I) + ' pass=' + N(L.PassIndex) +
      ' xyz=' + XYZ(L.X, L.Y, L.Z) + ' token=' + Token(L.Token)) then Exit;
  end;
  for I := 0 to M.DomainCount - 1 do
  begin
    if not W.Want then Exit;
    D := M.DomainAt(I);
    if not W.Detail('domain index=' + N(I) + ' pass=' + N(D.PassIndex) +
      ' xyz=' + XYZ(D.X, D.Y, D.Z) + ' tokens=' + N(Length(D.AllowedTokens))) then Exit;
    for J := 0 to Length(D.AllowedTokens) - 1 do
    begin
      if not W.Want then Exit;
      if not W.Detail('domain-token domain=' + N(I) + ' index=' + N(J) +
        ' token=' + Token(D.AllowedTokens[J])) then Exit;
    end;
  end;
end;

procedure InspectResult(const W: TInspectWriter; const M: TWfcPipelineResult);
var I, J: Integer; L: TWfcPipelineResultLayer; O: TWfcPipelinePassOutcome;
  F: TWfcPipelineFailure;
  Layout: TWfcLatticeLayout; Cell: TWfcLatticeVector; Box: TWfcLatticeBox;
  CellText: String;
begin
  if not W.Want then Exit;
  if not W.Detail('result shape=' + XYZ(M.Width, M.Height, M.Depth) +
    ' layers=' + N(M.LayerCount) + ' outcomes=' + N(M.PassOutcomeCount) +
    ' pass-backtracks=' + N(M.PassBacktracks) + ' evidence-kind=' + EvidenceName(M.EvidenceKind) +
    ' evidence-signature=' + WfcPipelineResultSignatureHex(M.EvidenceSignature) +
    ' evidence=claimed-not-replayed') then Exit;
  if M.FormatVersion = 2 then
    for I := 0 to M.PassOutcomeCount - 1 do
    begin
      if not W.Want then Exit;
      Layout := M.PassLayoutAt(I);
      if not W.Detail('result-layout pass=' + N(I) + ' rank=' + N(M.PassTopologyAt(I).Rank) +
        ' cells=' + XYZ(Layout.Cells.X,Layout.Cells.Y,Layout.Cells.Z) +
        ' origin=' + XYZ(Layout.Origin.X,Layout.Origin.Y,Layout.Origin.Z) +
        ' pitch=' + XYZ(Layout.Pitch.X,Layout.Pitch.Y,Layout.Pitch.Z) +
        ' wrap=' + B(Layout.Wrap)) then Exit;
    end;
  if M.Status <> wprsSolved then
  begin
    if not W.Want then Exit;
    F := M.CopyFailure;
    if not W.Detail('failure kind=' + FailureName(F.Kind) + ' pass=' + N(F.PassIndex) +
      ' entry=' + N(F.EntryIndex) + ' neighbor=' + N(F.NeighborIndex) +
      ' has-direction=' + B(F.HasDirection) + ' direction=' + DirectionName(F.Direction) +
      ' dependency=' + N(F.DependencyPassIndex)) then Exit;
  end;
  for I := 0 to M.PassOutcomeCount - 1 do
  begin
    if not W.Want then Exit;
    O := M.PassOutcomeAt(I);
    if not W.Detail('outcome pass=' + N(O.PassIndex) + ' executed=' + B(O.Executed) +
      ' ordinal=' + N(O.ExecutionOrdinal) + ' disposition=' + DispositionName(O.Disposition) +
      ' decisions=' + N(O.Decisions) + ' propagations=' + N(O.Propagations) +
      ' contradictions=' + N(O.Contradictions) + ' backtracks=' + N(O.Backtracks) +
      ' excluded=' + N(O.ExcludedAssignments)) then Exit;
  end;
  for I := 0 to M.LayerCount - 1 do
  begin
    if not W.Want then Exit;
    L := M.LayerAt(I);
    Layout := M.PassLayoutAt(L.PassIndex);
    if not W.Detail('layer index=' + N(I) + ' pass=' + N(L.PassIndex) +
      ' label=' + Token(L.LabelName) + ' cells=' + N(Length(L.Tokens))) then Exit;
    for J := 0 to Length(L.Tokens) - 1 do
    begin
      if not W.Want then Exit;
      Cell := MakeWfcLatticeVector(J mod Layout.Cells.X,
        (J div Layout.Cells.X) mod Layout.Cells.Y,
        (J div Layout.Cells.X) div Layout.Cells.Y);
      CellText := 'cell layer=' + N(I) + ' index=' + N(J) +
        ' xyz=' + XYZ(Cell.X,Cell.Y,Cell.Z) + ' token=' + Token(L.Tokens[J]);
      if M.FormatVersion = 2 then
      begin
        Box := WfcLatticeCellBox(Layout,Cell);
        CellText := CellText + ' world-min=' + XYZ(Box.Minimum.X,Box.Minimum.Y,Box.Minimum.Z) +
          ' world-max-exclusive=' + XYZ(Box.Maximum.X,Box.Maximum.Y,Box.Maximum.Z);
      end;
      if not W.Detail(CellText) then Exit;
    end;
  end;
end;

function WfcInspectArtifact(const ADocument: TWfcArtifactDocument;
  const ADetailLimit: Integer): String;
var W: TInspectWriter;
begin
  if not Assigned(ADocument) then
    raise EWfcArtifactInspect.Create('an artifact document is required');
  if not ((ADetailLimit >= 0) and (ADetailLimit <= High(Integer))) then
    raise EWfcArtifactInspect.Create('detail limit must be an exact nonnegative Integer');
  {$IFDEF PAS2JS}
  if ADetailLimit <> Trunc(ADetailLimit) then
    raise EWfcArtifactInspect.Create('detail limit must be an exact nonnegative Integer');
  {$ENDIF}
  W := TInspectWriter.Create(ADocument, ADetailLimit);
  try
    case ADocument.Kind of
      wakRules: InspectRules(W, ADocument.Rules);
      wakModel: InspectModel(W, ADocument.Model);
      wakPattern2D: InspectPatterns(W, ADocument.Pattern2D);
      wakPattern3D: InspectVolumePatterns(W, ADocument.Pattern3D);
      wakSequence: InspectSequence(W, ADocument.Sequence);
      wakTraining: InspectTraining(W, ADocument.Training);
      wakRecipe: InspectRecipe(W, ADocument.Recipe);
      wakRun: InspectRun(W, ADocument.Run);
      wakResult: InspectResult(W, ADocument.StoredResult);
    else raise EWfcArtifactInspect.Create('unknown artifact kind'); end;
    Result := W.Finish;
  finally
    W.Free;
  end;
end;

end.
