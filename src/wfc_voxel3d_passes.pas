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
unit wfc_voxel3d_passes;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_voxel3d;

const
  WFC_VOXEL3D_PASS_BRIDGE_VERSION = 1;

type
  EVoxel3DPassBridge = class(EVoxel3D);
  EVoxel3DPassMap = class(EVoxel3DPassBridge);

  //World offsets keep their declared axes. Target-yaw offsets are authored in
  //prototype-local coordinates and rotated once for every selected variant.
  TVoxel3DPassOffsetFrame = (
    v3pofWorld,
    v3pofTargetYaw
  );

  TVoxel3DPassVariantSelector = record
    PrototypeId: String;
    Rotations: TVoxel3DRotations;
  end;
  TVoxel3DPassVariantSelectors = array of TVoxel3DPassVariantSelector;

  //Projection maps are complete: every target variant must be selected by
  //exactly one rule, and every selector must resolve to at least one variant.
  TVoxel3DPassProjectionRule = record
    Target: TVoxel3DPassVariantSelector;
    AllowedSources: TVoxel3DPassVariantSelectors;
  end;
  TVoxel3DPassProjectionRules = array of TVoxel3DPassProjectionRule;

  //Every term is one (offset, finite source-variant set) alternative. Terms in
  //one clause are OR; separate clauses remain AND through the core pass API.
  TVoxel3DPassSpatialTerm = record
    Offset: TGraphOffset;
    AllowedSources: TVoxel3DPassVariantSelectors;
  end;
  TVoxel3DPassSpatialTerms = array of TVoxel3DPassSpatialTerm;

  TVoxel3DPassSpatialClause = record
    Target: TVoxel3DPassVariantSelector;
    OffsetFrame: TVoxel3DPassOffsetFrame;
    Terms: TVoxel3DPassSpatialTerms;
  end;
  TVoxel3DPassSpatialClauses = array of TVoxel3DPassSpatialClause;

function MakeVoxel3DPassVariantSelector(const APrototypeId: String;
  const ARotations: TVoxel3DRotations): TVoxel3DPassVariantSelector;
function MakeVoxel3DPassProjectionRule(
  const ATarget: TVoxel3DPassVariantSelector;
  const AAllowedSources: TVoxel3DPassVariantSelectors):
  TVoxel3DPassProjectionRule;
function MakeVoxel3DPassSpatialTerm(const AOffset: TGraphOffset;
  const AAllowedSources: TVoxel3DPassVariantSelectors):
  TVoxel3DPassSpatialTerm;
function MakeVoxel3DPassSpatialClause(
  const ATarget: TVoxel3DPassVariantSelector;
  const AOffsetFrame: TVoxel3DPassOffsetFrame;
  const ATerms: TVoxel3DPassSpatialTerms): TVoxel3DPassSpatialClause;

function RotateVoxel3DPassOffset(const AOffset: TGraphOffset;
  const ARotation: TVoxel3DRotation): TGraphOffset;

procedure ValidateVoxel3DProjectionFromPass(
  const ATargetKit: TVoxel3DKit;
  const ATargetAdapter: TVoxel3DGraphAdapter;
  const ASourceKit: TVoxel3DKit;
  const ASourceAdapter: TVoxel3DGraphAdapter;
  const ARules: TVoxel3DPassProjectionRules);

procedure RequireVoxel3DProjectionFromPass(
  const ATargetKit: TVoxel3DKit;
  const ATargetAdapter: TVoxel3DGraphAdapter;
  const ASourceKit: TVoxel3DKit;
  const ASourceAdapter: TVoxel3DGraphAdapter;
  const ARules: TVoxel3DPassProjectionRules);

procedure ValidateVoxel3DSpatialClausesFromPass(
  const ATargetKit: TVoxel3DKit;
  const ATargetAdapter: TVoxel3DGraphAdapter;
  const ASourceKit: TVoxel3DKit;
  const ASourceAdapter: TVoxel3DGraphAdapter;
  const AClauses: TVoxel3DPassSpatialClauses);

procedure RequireVoxel3DSpatialClausesFromPass(
  const ATargetKit: TVoxel3DKit;
  const ATargetAdapter: TVoxel3DGraphAdapter;
  const ASourceKit: TVoxel3DKit;
  const ASourceAdapter: TVoxel3DGraphAdapter;
  const AClauses: TVoxel3DPassSpatialClauses);

implementation

type
  TIntegerArray = array of Integer;
  TByteArray = array of Byte;

  TExpandedProjectionRule = record
    TargetIndices: TIntegerArray;
    SourceValues: TGraphValues;
  end;
  TExpandedProjectionRules = array of TExpandedProjectionRule;

  TExpandedSpatialApplication = record
    TargetIndex: Integer;
    Terms: TGraphPassMatchTerms;
  end;
  TExpandedSpatialApplications = array of TExpandedSpatialApplication;

function MakeVoxel3DPassVariantSelector(const APrototypeId: String;
  const ARotations: TVoxel3DRotations): TVoxel3DPassVariantSelector;
begin
  Result.PrototypeId := APrototypeId;
  Result.Rotations := ARotations;
end;

function MakeVoxel3DPassProjectionRule(
  const ATarget: TVoxel3DPassVariantSelector;
  const AAllowedSources: TVoxel3DPassVariantSelectors):
  TVoxel3DPassProjectionRule;
var
  I: Integer;
begin
  Result := Default(TVoxel3DPassProjectionRule);
  Result.Target := ATarget;
  SetLength(Result.AllowedSources, Length(AAllowedSources));
  for I := 0 to High(AAllowedSources) do
    Result.AllowedSources[I] := AAllowedSources[I];
end;

function MakeVoxel3DPassSpatialTerm(const AOffset: TGraphOffset;
  const AAllowedSources: TVoxel3DPassVariantSelectors):
  TVoxel3DPassSpatialTerm;
var
  I: Integer;
begin
  Result := Default(TVoxel3DPassSpatialTerm);
  Result.Offset := AOffset;
  SetLength(Result.AllowedSources, Length(AAllowedSources));
  for I := 0 to High(AAllowedSources) do
    Result.AllowedSources[I] := AAllowedSources[I];
end;

function MakeVoxel3DPassSpatialClause(
  const ATarget: TVoxel3DPassVariantSelector;
  const AOffsetFrame: TVoxel3DPassOffsetFrame;
  const ATerms: TVoxel3DPassSpatialTerms): TVoxel3DPassSpatialClause;
var
  I, J: Integer;
begin
  Result := Default(TVoxel3DPassSpatialClause);
  Result.Target := ATarget;
  Result.OffsetFrame := AOffsetFrame;
  SetLength(Result.Terms, Length(ATerms));
  for I := 0 to High(ATerms) do
  begin
    Result.Terms[I].Offset := ATerms[I].Offset;
    SetLength(Result.Terms[I].AllowedSources,
      Length(ATerms[I].AllowedSources));
    for J := 0 to High(ATerms[I].AllowedSources) do
      Result.Terms[I].AllowedSources[J] :=
        ATerms[I].AllowedSources[J];
  end;
end;

function CheckedNegate(const AValue: Integer): Integer;
begin
  if AValue = Low(Integer) then
    raise EVoxel3DPassMap.Create(
      'target-yaw offset cannot negate Low(Integer)');
  Result := -AValue;
end;

function RotateVoxel3DPassOffset(const AOffset: TGraphOffset;
  const ARotation: TVoxel3DRotation): TGraphOffset;
begin
  Result.DeltaZ := AOffset.DeltaZ;
  case ARotation of
    v3r0:
      begin
        Result.DeltaX := AOffset.DeltaX;
        Result.DeltaY := AOffset.DeltaY;
      end;
    v3r90:
      begin
        Result.DeltaX := AOffset.DeltaY;
        Result.DeltaY := CheckedNegate(AOffset.DeltaX);
      end;
    v3r180:
      begin
        Result.DeltaX := CheckedNegate(AOffset.DeltaX);
        Result.DeltaY := CheckedNegate(AOffset.DeltaY);
      end;
    v3r270:
      begin
        Result.DeltaX := CheckedNegate(AOffset.DeltaY);
        Result.DeltaY := AOffset.DeltaX;
      end;
  else
    raise ERangeError.CreateFmt('voxel pass rotation is out of bounds [%d]',
      [Ord(ARotation)]);
  end;
end;

procedure ValidateSelector(const ASelector: TVoxel3DPassVariantSelector;
  const ALabel: String);
begin
  if not IsVoxel3DToken(ASelector.PrototypeId) then
    raise EVoxel3DPassMap.Create(ALabel +
      ' prototype id is not a portable voxel token');
  if ASelector.Rotations = [] then
    raise EVoxel3DPassMap.Create(ALabel + ' rotations cannot be empty');
end;

function SelectorMatches(const ASelector: TVoxel3DPassVariantSelector;
  const AVariant: TVoxel3DVariant): Boolean;
begin
  Result := (ASelector.PrototypeId = AVariant.PrototypeId) and
    (AVariant.Rotation in ASelector.Rotations);
end;

procedure ValidateAdapter(const AKit: TVoxel3DKit;
  const AAdapter: TVoxel3DGraphAdapter; const ALabel: String);
begin
  if not Assigned(AKit) then
    raise EArgumentNilException.Create(ALabel + ' kit cannot be nil');
  if not Assigned(AAdapter) then
    raise EArgumentNilException.Create(ALabel + ' adapter cannot be nil');
  if not AKit.MatchesAdapter(AAdapter) then
    raise EVoxel3DPassMap.Create(ALabel +
      ' adapter does not match its kit');
  if not Assigned(AAdapter.AppliedGraph) then
    raise EVoxel3DPassMap.Create(ALabel + ' adapter has no graph');
  if AAdapter.AppliedGraph.Running then
    raise EVoxel3DPassMap.Create(ALabel +
      ' adapter graph is currently running');
  if (AAdapter.PassIndex < 0) or
      (AAdapter.PassIndex >= AAdapter.AppliedGraph.TotalPassCount) or
      (AAdapter.AppliedGraph.PassGraph[AAdapter.PassIndex] <>
       AAdapter.AppliedGraph) then
    raise EVoxel3DPassMap.Create(ALabel +
      ' adapter is not bound to its recorded pass');
  if (AAdapter.AppliedGraph.Dimension.Width <> AAdapter.Width) or
      (AAdapter.AppliedGraph.Dimension.Height <> AAdapter.Height) or
      (AAdapter.AppliedGraph.Dimension.Depth <> AAdapter.Depth) or
      (AAdapter.AppliedGraph.WrapNeighbors <> AAdapter.WrapNeighbors) then
    raise EVoxel3DPassMap.Create(ALabel +
      ' adapter shape no longer matches its graph');
  if not AAdapter.DefinitionMatchesGraph then
    raise EVoxel3DPassMap.Create(ALabel +
      ' graph no longer matches its voxel definition');
end;

function PassDependsTransitively(const AGraph: TGraph;
  const AStartIndex, ATargetIndex: Integer): Boolean;
var
  I, LCount, LNode: Integer;
  LSeen: TByteArray;
  LStack: TIntegerArray;
begin
  Result := False;
  LCount := AGraph.TotalPassCount;
  SetLength(LSeen, LCount);
  SetLength(LStack, LCount);
  LCount := 1;
  LStack[0] := AStartIndex;
  LSeen[AStartIndex] := 1;
  while LCount > 0 do
  begin
    Dec(LCount);
    LNode := LStack[LCount];
    if LNode = ATargetIndex then
      Exit(True);
    for I := 0 to AGraph.PassGraph[LNode].DependencyCount - 1 do
      if LSeen[AGraph.PassGraph[LNode].DependencyIndex[I]] = 0 then
      begin
        LSeen[AGraph.PassGraph[LNode].DependencyIndex[I]] := 1;
        LStack[LCount] := AGraph.PassGraph[LNode].DependencyIndex[I];
        Inc(LCount);
      end;
  end;
end;

procedure ValidateRelationship(const ATargetAdapter,
  ASourceAdapter: TVoxel3DGraphAdapter);
begin
  if ATargetAdapter.AppliedGraph.PassGraph[0] <>
      ASourceAdapter.AppliedGraph.PassGraph[0] then
    raise EVoxel3DPassMap.Create(
      'voxel pass adapters belong to different graph roots');
  if ATargetAdapter.PassIndex = ASourceAdapter.PassIndex then
    raise EVoxel3DPassMap.Create(
      'a voxel pass cannot project from itself');
  if PassDependsTransitively(ATargetAdapter.AppliedGraph,
      ASourceAdapter.PassIndex, ATargetAdapter.PassIndex) then
    raise EVoxel3DPassMap.Create(
      'voxel pass projection would create a dependency cycle');
end;

function SelectVariantIndices(const AAdapter: TVoxel3DGraphAdapter;
  const ASelector: TVoxel3DPassVariantSelector;
  const ALabel: String): TIntegerArray;
var
  I, LCount: Integer;
begin
  Result := Default(TIntegerArray);
  ValidateSelector(ASelector, ALabel);
  SetLength(Result, AAdapter.VariantCount);
  LCount := 0;
  for I := 0 to AAdapter.VariantCount - 1 do
    if SelectorMatches(ASelector, AAdapter.VariantAt(I)) then
    begin
      Result[LCount] := I;
      Inc(LCount);
    end;
  SetLength(Result, LCount);
  if LCount = 0 then
    raise EVoxel3DPassMap.Create(ALabel +
      ' does not select an available variant');
end;

function ExpandSourceValues(const AAdapter: TVoxel3DGraphAdapter;
  const ASelectors: TVoxel3DPassVariantSelectors;
  const ALabel: String): TGraphValues;
var
  I, J, LCount: Integer;
  LIndices: TIntegerArray;
  LSeen: TByteArray;
begin
  Result := Default(TGraphValues);
  if Length(ASelectors) = 0 then
    raise EVoxel3DPassMap.Create(ALabel +
      ' needs at least one source selector');
  SetLength(LSeen, AAdapter.VariantCount);
  for I := 0 to High(ASelectors) do
  begin
    LIndices := SelectVariantIndices(AAdapter, ASelectors[I],
      ALabel + ' selector ' + IntToStr(I));
    for J := 0 to High(LIndices) do
    begin
      if LSeen[LIndices[J]] <> 0 then
        raise EVoxel3DPassMap.Create(ALabel +
          ' selects one source variant more than once');
      LSeen[LIndices[J]] := 1;
    end;
  end;

  LCount := 0;
  for I := 0 to High(LSeen) do
    if LSeen[I] <> 0 then
      Inc(LCount);
  SetLength(Result, LCount);
  LCount := 0;
  for I := 0 to High(LSeen) do
    if LSeen[I] <> 0 then
    begin
      Result[LCount] := AAdapter.VariantGraphKeyAt(I);
      Inc(LCount);
    end;
end;

function ExpandProjectionRules(const ATargetAdapter,
  ASourceAdapter: TVoxel3DGraphAdapter;
  const ARules: TVoxel3DPassProjectionRules): TExpandedProjectionRules;
var
  I, J: Integer;
  LOwners: TIntegerArray;
begin
  Result := Default(TExpandedProjectionRules);
  if Length(ARules) = 0 then
    raise EVoxel3DPassMap.Create(
      'voxel projection map needs at least one rule');
  SetLength(Result, Length(ARules));
  SetLength(LOwners, ATargetAdapter.VariantCount);
  for I := 0 to High(LOwners) do
    LOwners[I] := -1;

  for I := 0 to High(ARules) do
  begin
    Result[I].TargetIndices := SelectVariantIndices(ATargetAdapter,
      ARules[I].Target, 'projection rule ' + IntToStr(I) + ' target');
    for J := 0 to High(Result[I].TargetIndices) do
    begin
      if LOwners[Result[I].TargetIndices[J]] >= 0 then
        raise EVoxel3DPassMap.CreateFmt(
          'projection rules %d and %d select the same target variant',
          [LOwners[Result[I].TargetIndices[J]], I]);
      LOwners[Result[I].TargetIndices[J]] := I;
    end;
    Result[I].SourceValues := ExpandSourceValues(ASourceAdapter,
      ARules[I].AllowedSources,
      'projection rule ' + IntToStr(I) + ' sources');
  end;

  for I := 0 to High(LOwners) do
    if LOwners[I] < 0 then
      raise EVoxel3DPassMap.CreateFmt(
        'projection map does not cover target variant %d', [I]);
end;

function ExpandSpatialClauses(const ATargetAdapter,
  ASourceAdapter: TVoxel3DGraphAdapter;
  const AClauses: TVoxel3DPassSpatialClauses):
  TExpandedSpatialApplications;
var
  I, J, K, LApplicationCount: Integer;
  LOffset: TGraphOffset;
  LTargets: TIntegerArray;
  LVariant: TVoxel3DVariant;
begin
  Result := Default(TExpandedSpatialApplications);
  if Length(AClauses) = 0 then
    raise EVoxel3DPassMap.Create(
      'voxel spatial map needs at least one clause');
  SetLength(Result, 0);
  LApplicationCount := 0;
  for I := 0 to High(AClauses) do
  begin
    if (Ord(AClauses[I].OffsetFrame) <
        Ord(Low(TVoxel3DPassOffsetFrame))) or
        (Ord(AClauses[I].OffsetFrame) >
        Ord(High(TVoxel3DPassOffsetFrame))) then
      raise EVoxel3DPassMap.CreateFmt(
        'spatial clause %d offset frame is out of bounds', [I]);
    if Length(AClauses[I].Terms) = 0 then
      raise EVoxel3DPassMap.CreateFmt(
        'spatial clause %d needs at least one term', [I]);
    LTargets := SelectVariantIndices(ATargetAdapter,
      AClauses[I].Target, 'spatial clause ' + IntToStr(I) + ' target');
    for J := 0 to High(LTargets) do
    begin
      SetLength(Result, LApplicationCount + 1);
      Result[LApplicationCount].TargetIndex := LTargets[J];
      SetLength(Result[LApplicationCount].Terms,
        Length(AClauses[I].Terms));
      LVariant := ATargetAdapter.VariantAt(LTargets[J]);
      for K := 0 to High(AClauses[I].Terms) do
      begin
        LOffset := AClauses[I].Terms[K].Offset;
        if AClauses[I].OffsetFrame = v3pofTargetYaw then
          LOffset := RotateVoxel3DPassOffset(LOffset, LVariant.Rotation);
        Result[LApplicationCount].Terms[K].Offset := LOffset;
        Result[LApplicationCount].Terms[K].Values :=
          ExpandSourceValues(ASourceAdapter,
            AClauses[I].Terms[K].AllowedSources,
            'spatial clause ' + IntToStr(I) + ' term ' +
            IntToStr(K) + ' sources');
      end;
      Inc(LApplicationCount);
    end;
  end;
end;

procedure Preflight(const ATargetKit: TVoxel3DKit;
  const ATargetAdapter: TVoxel3DGraphAdapter;
  const ASourceKit: TVoxel3DKit;
  const ASourceAdapter: TVoxel3DGraphAdapter);
begin
  ValidateAdapter(ATargetKit, ATargetAdapter, 'target');
  ValidateAdapter(ASourceKit, ASourceAdapter, 'source');
  ValidateRelationship(ATargetAdapter, ASourceAdapter);
end;

procedure ValidateVoxel3DProjectionFromPass(
  const ATargetKit: TVoxel3DKit;
  const ATargetAdapter: TVoxel3DGraphAdapter;
  const ASourceKit: TVoxel3DKit;
  const ASourceAdapter: TVoxel3DGraphAdapter;
  const ARules: TVoxel3DPassProjectionRules);
var
  LExpanded: TExpandedProjectionRules;
begin
  Preflight(ATargetKit, ATargetAdapter, ASourceKit, ASourceAdapter);
  LExpanded := ExpandProjectionRules(ATargetAdapter, ASourceAdapter, ARules);
  if Length(LExpanded) <> Length(ARules) then
    raise EVoxel3DPassMap.Create('projection preflight failed');
end;

procedure RequireVoxel3DProjectionFromPass(
  const ATargetKit: TVoxel3DKit;
  const ATargetAdapter: TVoxel3DGraphAdapter;
  const ASourceKit: TVoxel3DKit;
  const ASourceAdapter: TVoxel3DGraphAdapter;
  const ARules: TVoxel3DPassProjectionRules);
var
  I, J: Integer;
  LExpanded: TExpandedProjectionRules;
  LSourcePass: String;
begin
  Preflight(ATargetKit, ATargetAdapter, ASourceKit, ASourceAdapter);
  LExpanded := ExpandProjectionRules(ATargetAdapter, ASourceAdapter, ARules);
  LSourcePass := ASourceAdapter.AppliedGraph.CurrentPass;
  for I := 0 to High(LExpanded) do
    for J := 0 to High(LExpanded[I].TargetIndices) do
      ATargetAdapter.AppliedGraph.Rules[
        ATargetAdapter.VariantGraphKeyAt(
          LExpanded[I].TargetIndices[J])].RequireFromPass(
            LSourcePass, LExpanded[I].SourceValues);
end;

procedure ValidateVoxel3DSpatialClausesFromPass(
  const ATargetKit: TVoxel3DKit;
  const ATargetAdapter: TVoxel3DGraphAdapter;
  const ASourceKit: TVoxel3DKit;
  const ASourceAdapter: TVoxel3DGraphAdapter;
  const AClauses: TVoxel3DPassSpatialClauses);
var
  LExpanded: TExpandedSpatialApplications;
begin
  Preflight(ATargetKit, ATargetAdapter, ASourceKit, ASourceAdapter);
  LExpanded := ExpandSpatialClauses(ATargetAdapter, ASourceAdapter, AClauses);
  if Length(LExpanded) = 0 then
    raise EVoxel3DPassMap.Create('spatial preflight produced no applications');
end;

procedure RequireVoxel3DSpatialClausesFromPass(
  const ATargetKit: TVoxel3DKit;
  const ATargetAdapter: TVoxel3DGraphAdapter;
  const ASourceKit: TVoxel3DKit;
  const ASourceAdapter: TVoxel3DGraphAdapter;
  const AClauses: TVoxel3DPassSpatialClauses);
var
  I: Integer;
  LExpanded: TExpandedSpatialApplications;
  LSourcePass: String;
begin
  Preflight(ATargetKit, ATargetAdapter, ASourceKit, ASourceAdapter);
  LExpanded := ExpandSpatialClauses(ATargetAdapter, ASourceAdapter, AClauses);
  LSourcePass := ASourceAdapter.AppliedGraph.CurrentPass;
  for I := 0 to High(LExpanded) do
    ATargetAdapter.AppliedGraph.Rules[
      ATargetAdapter.VariantGraphKeyAt(
        LExpanded[I].TargetIndex)].RequireAnyFromPass(
          LSourcePass, LExpanded[I].Terms);
end;

end.
