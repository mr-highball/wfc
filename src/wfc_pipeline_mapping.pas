{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
unit wfc_pipeline_mapping;
{$mode delphi}{$H+}

interface

uses SysUtils, wfc, wfc_model, wfc_lattice, wfc_pipeline_layout,
  wfc_pipeline_model;

type EWfcPipelineMapping = class(EWfcPipelineModel);

{ A recipe owns topology; an invocation owns cell extents. This preflight owns
  O(pass-count) storage only, never a graph. Every pass has a distinct prefix,
  including transform aliases. Caller owns the returned table. }
function UniformWfcPipelinePassExtents(const ARecipe: TWfcPipelineModel;
  const AWidth, AHeight, ADepth: Integer): TWfcPipelinePassExtents;
function ResolveWfcPipelineLayoutTable(const ARecipe: TWfcPipelineModel;
  const AExtents: TWfcPipelinePassExtents): TWfcPipelineLayoutTable;

{ Evaluate the public mapped policy, not private compiled graph clauses.
  The caller invokes this only where the consumer equals ConsumerToken and
  supplies complete public provider tokens, already checked against the
  recipe vocabulary. The function checks geometry, token-array extent and
  each queried token slot; it does not rescan unrelated provider cells for
  every consumer query. Typed-JavaScript record fields and queried array
  slots must be passive data, never accessors or inherited array entries;
  false includes bounded partial coverage, even for a zero-minimum count.
  It allocates no array proportional to the footprint. }
function ValidateWfcPipelineMappedRequirement(
  const ARequirement: TWfcPipelineRequirement;
  const AConsumerLayout, AProviderLayout: TWfcLatticeLayout;
  const AConsumerCellIndex: Integer;
  const AProviderTokens: TWfcModelTokens): Boolean;

implementation

procedure RequireRecipe(const ARecipe: TWfcPipelineModel);
begin
  if not Assigned(ARecipe) then
    raise EWfcPipelineMapping.Create('a bound recipe is required');
end;

procedure RequirePassiveLayout(const ALayout: TWfcLatticeLayout);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  { The core checks numerical geometry. Guard raw typed-JavaScript containers
    first so those checks cannot invoke ordinary accessor properties. }
  asm
    function passive(o, names) {
      if (o === null || typeof o !== 'object' || Array.isArray(o)) return false;
      for (const key of names) {
        let p=o, d;
        while (p !== null) {
          d=Object.getOwnPropertyDescriptor(p,key);
          if (d) break;
          p=Object.getPrototypeOf(p);
        }
        if (!d || !Object.prototype.hasOwnProperty.call(d,'value')) return false;
      }
      return true;
    }
    Valid=passive(ALayout,['Cells','Origin','Pitch','Wrap']);
    if (Valid) Valid=passive(ALayout.Cells,['X','Y','Z']) &&
      passive(ALayout.Origin,['X','Y','Z']) && passive(ALayout.Pitch,['X','Y','Z']);
  end;
  if not Valid then raise EWfcPipelineMapping.Create(
    'mapped layouts require passive layout and vector fields');
  {$ENDIF}
end;

function UniformWfcPipelinePassExtents(const ARecipe: TWfcPipelineModel;
  const AWidth, AHeight, ADepth: Integer): TWfcPipelinePassExtents;
var I: Integer; Cells: TWfcLatticeVector;
begin
  RequireRecipe(ARecipe);
  Cells:=MakeWfcLatticeVector(AWidth,AHeight,ADepth);
  if (Cells.X<1) or (Cells.Y<1) or (Cells.Z<1) then
    raise EWfcPipelineMapping.Create('uniform extents must be positive');
  Result:=nil; SetLength(Result,ARecipe.PassCount);
  for I:=0 to High(Result) do Result[I]:=Cells;
end;

function ResolveWfcPipelineLayoutTable(const ARecipe: TWfcPipelineModel;
  const AExtents: TWfcPipelinePassExtents): TWfcPipelineLayoutTable;
var I,J: Integer; Pass: TWfcPipelinePass; Bridge: TWfcPipelineBridge;
  Requirement: TWfcPipelineRequirement; HasDefinition: Boolean;

  procedure RequireIdentity(const AConsumer, AProvider: Integer;
    const ARelationship: String);
  begin
    if not SameWfcLatticeLayout(Result.PassLayoutAt(AConsumer),
      Result.PassLayoutAt(AProvider)) then
      raise EWfcPipelineMapping.CreateFmt(
        '%s requires identical layouts for passes %d and %d',
        [ARelationship,AConsumer,AProvider]);
  end;

begin
  RequireRecipe(ARecipe);
  Result:=TWfcPipelineLayoutTable.Create(ARecipe.CopyPassTopologies,AExtents);
  try
    for I:=0 to ARecipe.PassCount-1 do
    begin
      Pass:=ARecipe.PassAt(I);
      if Pass.Mode=gpmTransform then
        RequireIdentity(I,Pass.TransformSourceIndex,'exact transform')
      else if (Pass.Mode=gpmLegacy) and (I>0) and
        (Pass.AdapterKind=wpakEmpty) then
      begin
        { A projection bridge installs a definition on its public target.
          A truly definitionless legacy pass instead copies its predecessor.
          An order-only dependency never implies layout identity. }
        HasDefinition:=False;
        for J:=0 to ARecipe.BridgeCount-1 do
          if ARecipe.BridgeAt(J).TargetPassIndex=I then HasDefinition:=True;
        for J:=0 to ARecipe.RequirementCount-1 do
          if ARecipe.RequirementAt(J).ConsumerPassIndex=I then HasDefinition:=True;
        if not HasDefinition then RequireIdentity(I,I-1,'legacy copy');
      end;
    end;
    for I:=0 to ARecipe.BridgeCount-1 do
    begin
      Bridge:=ARecipe.BridgeAt(I);
      RequireIdentity(Bridge.TargetPassIndex,Bridge.SourcePassIndex,
        'index-space projection bridge');
    end;
    for I:=0 to ARecipe.RequirementCount-1 do
    begin
      Requirement:=ARecipe.RequirementAt(I);
      if Requirement.Kind=wprqMapped then
        ValidateGraphPassMappedQuery(
          Result.PassLayoutAt(Requirement.ConsumerPassIndex),
          WfcPipelineMappedQueryGeometry(Requirement.MappedQuery))
      else
        RequireIdentity(Requirement.ConsumerPassIndex,
          Requirement.ProviderPassIndex,'index-space requirement');
    end;
  except
    Result.Free;
    raise;
  end;
end;

function ValidateWfcPipelineMappedRequirement(
  const ARequirement: TWfcPipelineRequirement;
  const AConsumerLayout, AProviderLayout: TWfcLatticeLayout;
  const AConsumerCellIndex: Integer;
  const AProviderTokens: TWfcModelTokens): Boolean;
var Query: TGraphPassMapQuery; Cell, ProviderCell: TWfcLatticeVector;
  CellBox, QueryBox: TWfcLatticeBox; Coverage: TWfcLatticeCoverage;
  ConsumerCount, ProviderCount, CoverageCount, I,J,Index,Matches: Integer;
  Allowed: Boolean;
  {$IFDEF PAS2JS}Valid: Boolean;{$ENDIF}

  function OffsetPoint(const APoint: TWfcLatticeVector;
    const AOffset: TGraphOffset): TWfcLatticeVector;
  var X,Y,Z: Double;
  begin
    { Pure core preflight below proves every translated endpoint representable.
      Widen through assignment (not an FPC bit-reinterpretation cast). }
    X:=APoint.X; Y:=APoint.Y; Z:=APoint.Z;
    Result.X:=Integer(Trunc(X+AOffset.DeltaX));
    Result.Y:=Integer(Trunc(Y+AOffset.DeltaY));
    Result.Z:=Integer(Trunc(Z+AOffset.DeltaZ));
  end;

begin
  {$IFDEF PAS2JS}
  asm
    Valid=ARequirement !== null && typeof ARequirement === 'object' &&
      !Array.isArray(ARequirement) && Array.isArray(AProviderTokens);
    if (Valid) {
      for (const key of ['Kind','MappedQuery']) {
        let p=ARequirement, d;
        while (p !== null) {
          d=Object.getOwnPropertyDescriptor(p,key);
          if (d) break;
          p=Object.getPrototypeOf(p);
        }
        if (!d || !Object.prototype.hasOwnProperty.call(d,'value')) {
          Valid=false; break;
        }
      }
    }
  end;
  if not Valid then raise EWfcPipelineMapping.Create(
    'mapped requirement requires passive fields and a provider token array');
  {$ENDIF}
  RequirePassiveLayout(AConsumerLayout);
  RequirePassiveLayout(AProviderLayout);
  if ARequirement.Kind<>wprqMapped then
    raise EWfcPipelineMapping.Create('mapped policy evaluator requires a mapped clause');
  Query:=WfcPipelineMappedQueryGeometry(ARequirement.MappedQuery);
  ValidateGraphPassMappedQuery(AConsumerLayout,Query);
  ConsumerCount:=WfcLatticeCellCount(AConsumerLayout);
  ProviderCount:=WfcLatticeCellCount(AProviderLayout);
  MakeWfcLatticeVector(AConsumerCellIndex,0,0);
  if (AConsumerCellIndex<0) or (AConsumerCellIndex>=ConsumerCount) then
    raise EWfcPipelineMapping.Create('mapped consumer cell is outside its own pass');
  if Length(AProviderTokens)<>ProviderCount then
    raise EWfcPipelineMapping.Create('mapped provider tokens must fill the provider layout');
  {$IFDEF PAS2JS}
  asm Valid=Array.isArray(ARequirement.MappedQuery.AllowedProviderTokens); end;
  if not Valid then raise EWfcPipelineMapping.Create('mapped allowed tokens must be an array');
  {$ENDIF}
  if Length(ARequirement.MappedQuery.AllowedProviderTokens)=0 then
    raise EWfcPipelineMapping.Create('mapped policy must allow at least one token');
  for I:=0 to High(ARequirement.MappedQuery.AllowedProviderTokens) do
  begin
    {$IFDEF PAS2JS}
    asm Valid=typeof ARequirement.MappedQuery.AllowedProviderTokens[I] === 'string'; end;
    if not Valid then raise EWfcPipelineMapping.Create('mapped allowed token must be a string');
    {$ENDIF}
    if ARequirement.MappedQuery.AllowedProviderTokens[I]='' then
      raise EWfcPipelineMapping.Create('mapped allowed token cannot be empty');
  end;
  Cell.X:=AConsumerCellIndex mod AConsumerLayout.Cells.X;
  Index:=AConsumerCellIndex div AConsumerLayout.Cells.X;
  Cell.Y:=Index mod AConsumerLayout.Cells.Y;
  Cell.Z:=Index div AConsumerLayout.Cells.Y;
  CellBox:=WfcLatticeCellBox(AConsumerLayout,Cell);
  QueryBox.Minimum:=OffsetPoint(CellBox.Minimum,Query.MinimumOffset);
  Result:=False;
  if Query.Kind=gpmkPoint then
  begin
    if not TryWfcLatticePoint(AProviderLayout,QueryBox.Minimum,ProviderCell) then Exit;
    CoverageCount:=1;
  end
  else
  begin
    if Query.Kind=gpmkCellCoverage then
      QueryBox.Maximum:=OffsetPoint(CellBox.Maximum,Query.MinimumOffset)
    else QueryBox.Maximum:=OffsetPoint(CellBox.Minimum,Query.MaximumOffset);
    if not TryWfcLatticeCoverage(AProviderLayout,QueryBox,Coverage) then Exit;
    CoverageCount:=WfcLatticeCoverageCellCount(Coverage);
  end;
  Matches:=0;
  for I:=0 to CoverageCount-1 do
  begin
    if Query.Kind<>gpmkPoint then ProviderCell:=WfcLatticeCoverageCell(Coverage,I);
    Index:=(ProviderCell.Z*AProviderLayout.Cells.Y+ProviderCell.Y)*
      AProviderLayout.Cells.X+ProviderCell.X;
    {$IFDEF PAS2JS}
    asm
      const d=Object.getOwnPropertyDescriptor(AProviderTokens,String(Index));
      Valid=!!d && Object.prototype.hasOwnProperty.call(d,'value') &&
        typeof d.value === 'string';
    end;
    if not Valid then raise EWfcPipelineMapping.Create(
      'queried mapped provider token requires an own passive string slot');
    {$ENDIF}
    Allowed:=False;
    for J:=0 to High(ARequirement.MappedQuery.AllowedProviderTokens) do
      if AProviderTokens[Index]=ARequirement.MappedQuery.AllowedProviderTokens[J] then
      begin Allowed:=True; Break; end;
    if Query.Match=gpmmAll then
    begin if not Allowed then Exit; end
    else if Allowed then
    begin Inc(Matches); if Matches>Query.MaximumMatches then Exit; end;
  end;
  Result:=(Query.Match=gpmmAll) or (Matches>=Query.MinimumMatches);
end;

end.
