{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
unit mapped_world_validation;
{$mode delphi}{$H+}
interface
uses SysUtils, wfc_lattice, mapped_world_types;
{ These checks inspect detached captures only. No solver matcher, lattice
  coverage enumeration or mutable graph is used as an oracle. }
function AnalyzeMappedWorldResult(const AResult: TMappedWorldResult;
  out AReport: TMappedWorldValidation): Boolean;
function InspectMappedWorldSite(const AResult: TMappedWorldResult;
  const AX, AY: Integer; const AIsCurrent: Boolean): TMappedWorldInspection;
function MappedWorldLiteralCellBox(const ALayout: TWfcLatticeLayout;
  const AIndex: Integer): TWfcLatticeBox;
function MappedWorldBoxesIntersect(const A, B: TWfcLatticeBox): Boolean;
function MappedWorldBoxContainsPoint(const ABox: TWfcLatticeBox;
  const APoint: TWfcLatticeVector): Boolean;
implementation

function MappedWorldLiteralCellBox(const ALayout: TWfcLatticeLayout;
  const AIndex: Integer): TWfcLatticeBox;
var X,Y,Z:Integer; L:TMappedWorldLayer; Known:Boolean;
begin
  Known:=False; ValidateWfcLatticeLayout(ALayout);
  for L:=Low(TMappedWorldLayer) to High(TMappedWorldLayer) do
    if SameWfcLatticeLayout(ALayout,MappedWorldLayout(L)) then Known:=True;
  if not Known then raise EMappedWorld.Create('literal geometry requires a showcase layer layout');
  RequireMappedWorldInteger(AIndex,0,ALayout.Cells.X*ALayout.Cells.Y*ALayout.Cells.Z-1,'cell index');
  { The showcase shape preflight bounds these products; this is deliberately
    the literal interval definition, independent of the mapping library. }
  X:=AIndex mod ALayout.Cells.X;
  Y:=(AIndex div ALayout.Cells.X) mod ALayout.Cells.Y;
  Z:=AIndex div (ALayout.Cells.X*ALayout.Cells.Y);
  Result.Minimum.X:=ALayout.Origin.X+X*ALayout.Pitch.X;
  Result.Minimum.Y:=ALayout.Origin.Y+Y*ALayout.Pitch.Y;
  Result.Minimum.Z:=ALayout.Origin.Z+Z*ALayout.Pitch.Z;
  Result.Maximum.X:=Result.Minimum.X+ALayout.Pitch.X;
  Result.Maximum.Y:=Result.Minimum.Y+ALayout.Pitch.Y;
  Result.Maximum.Z:=Result.Minimum.Z+ALayout.Pitch.Z;
end;

function MappedWorldBoxesIntersect(const A, B: TWfcLatticeBox): Boolean;
begin
  Result:=(A.Minimum.X<A.Maximum.X) and (A.Minimum.Y<A.Maximum.Y) and (A.Minimum.Z<A.Maximum.Z) and
    (B.Minimum.X<B.Maximum.X) and (B.Minimum.Y<B.Maximum.Y) and (B.Minimum.Z<B.Maximum.Z) and
    (A.Minimum.X<B.Maximum.X) and (A.Maximum.X>B.Minimum.X) and
    (A.Minimum.Y<B.Maximum.Y) and (A.Maximum.Y>B.Minimum.Y) and
    (A.Minimum.Z<B.Maximum.Z) and (A.Maximum.Z>B.Minimum.Z);
end;

function MappedWorldBoxContainsPoint(const ABox: TWfcLatticeBox;
  const APoint: TWfcLatticeVector): Boolean;
begin
  Result:=(APoint.X>=ABox.Minimum.X) and (APoint.X<ABox.Maximum.X) and
    (APoint.Y>=ABox.Minimum.Y) and (APoint.Y<ABox.Maximum.Y) and
    (APoint.Z>=ABox.Minimum.Z) and (APoint.Z<ABox.Maximum.Z);
end;

function ContainsBox(const OuterBox,InnerBox:TWfcLatticeBox):Boolean;
begin
  Result:=(InnerBox.Minimum.X>=OuterBox.Minimum.X) and (InnerBox.Maximum.X<=OuterBox.Maximum.X) and
    (InnerBox.Minimum.Y>=OuterBox.Minimum.Y) and (InnerBox.Maximum.Y<=OuterBox.Maximum.Y) and
    (InnerBox.Minimum.Z>=OuterBox.Minimum.Z) and (InnerBox.Maximum.Z<=OuterBox.Maximum.Z);
end;

function LayerBox(const L:TWfcLatticeLayout):TWfcLatticeBox;
begin
  Result.Minimum:=L.Origin;
  Result.Maximum.X:=L.Origin.X+L.Cells.X*L.Pitch.X;
  Result.Maximum.Y:=L.Origin.Y+L.Cells.Y*L.Pitch.Y;
  Result.Maximum.Z:=L.Origin.Z+L.Cells.Z*L.Pitch.Z;
end;

procedure AppendSample(var A:TMappedWorldSamples; const S:TMappedWorldSample);
var N:Integer;
begin N:=Length(A); SetLength(A,N+1); A[N]:=S; end;

function Collect(const R:TMappedWorldResult; const Layer:TMappedWorldLayer;
  const Box:TWfcLatticeBox; const Point:Boolean; const Wanted:String;
  const Corner:TWfcLatticeVector; out InBounds:Boolean):TMappedWorldSamples;
var J:Integer; S:TMappedWorldSample; L:TWfcLatticeLayout;
begin
  Result:=nil; L:=R.Layers[Layer].Layout;
  if Point then InBounds:=MappedWorldBoxContainsPoint(LayerBox(L),Box.Minimum)
  else InBounds:=ContainsBox(LayerBox(L),Box);
  for J:=0 to High(R.Layers[Layer].Cells) do
  begin
    S:=Default(TMappedWorldSample); S.Bounds:=MappedWorldLiteralCellBox(L,J);
    if Point then begin if not MappedWorldBoxContainsPoint(S.Bounds,Box.Minimum) then Continue; end
    else if not MappedWorldBoxesIntersect(S.Bounds,Box) then Continue;
    S.Layer:=Layer; S.CellIndex:=J;
    S.Position.X:=J mod L.Cells.X; S.Position.Y:=(J div L.Cells.X) mod L.Cells.Y;
    S.Position.Z:=J div (L.Cells.X*L.Cells.Y);
    S.Cell:=CopyMappedWorldCell(R.Layers[Layer].Cells[J]);
    S.Accepted:=S.Cell.Value=Wanted;
    S.IsCorner:=MappedWorldBoxContainsPoint(S.Bounds,Corner);
    AppendSample(Result,S);
  end;
end;

function AllAccepted(const A:TMappedWorldSamples; const InBounds:Boolean):Boolean;
var I:Integer;
begin
  Result:=False; if (not InBounds) or (Length(A)=0) then Exit;
  for I:=0 to High(A) do if not A[I].Accepted then Exit;
  Result:=True;
end;

function InspectPrepared(const AResult:TMappedWorldResult; const AX,AY:Integer;
  const AIsCurrent:Boolean):TMappedWorldInspection;
var J:Integer; Physical:TMappedWorldSamples; Inside:Boolean;
begin
  Result:=Default(TMappedWorldInspection);
  Result.SiteIndex:=MappedWorldCellIndex(mwlHousing,AX,AY);
  Result.SiteX:=AX; Result.SiteY:=AY; Result.Revision:=AResult.Revision;
  Result.IsCurrent:=AIsCurrent; Result.Sampling:=AResult.Config.Sampling;
  Result.HouseBounds:=MappedWorldLiteralCellBox(AResult.Layers[mwlHousing].Layout,Result.SiteIndex);
  Result.QueryBounds:=Result.HouseBounds;
  case Result.Sampling of
    mwsPointStudy: Result.QueryBounds.Maximum:=Result.QueryBounds.Minimum;
    mwsRegion:
      begin
        Result.QueryBounds.Minimum.X:=Result.HouseBounds.Minimum.X+AResult.Config.RegionMinimum.DeltaX;
        Result.QueryBounds.Minimum.Y:=Result.HouseBounds.Minimum.Y+AResult.Config.RegionMinimum.DeltaY;
        Result.QueryBounds.Minimum.Z:=Result.HouseBounds.Minimum.Z+AResult.Config.RegionMinimum.DeltaZ;
        Result.QueryBounds.Maximum.X:=Result.HouseBounds.Minimum.X+AResult.Config.RegionMaximum.DeltaX;
        Result.QueryBounds.Maximum.Y:=Result.HouseBounds.Minimum.Y+AResult.Config.RegionMaximum.DeltaY;
        Result.QueryBounds.Maximum.Z:=Result.HouseBounds.Minimum.Z+AResult.Config.RegionMaximum.DeltaZ;
      end;
  end;
  Result.TerrainSamples:=Collect(AResult,mwlTerrain,Result.HouseBounds,False,'land',
    Result.HouseBounds.Minimum,Result.TerrainQueryInBounds);
  Result.FoliageSamples:=Collect(AResult,mwlFoliage,Result.QueryBounds,
    Result.Sampling=mwsPointStudy,'clear',Result.HouseBounds.Minimum,Result.FoliageQueryInBounds);
  Result.SelectedModelClear:=AllAccepted(Result.TerrainSamples,Result.TerrainQueryInBounds) and
    AllAccepted(Result.FoliageSamples,Result.FoliageQueryInBounds);
  Physical:=Collect(AResult,mwlFoliage,Result.HouseBounds,False,'clear',Result.HouseBounds.Minimum,Inside);
  Result.PhysicalClear:=AllAccepted(Result.TerrainSamples,Result.TerrainQueryInBounds) and AllAccepted(Physical,Inside);
  for J:=0 to High(Result.TerrainSamples) do if not Result.TerrainSamples[J].Accepted then
    AppendSample(Result.PhysicalBlockers,Result.TerrainSamples[J]);
  for J:=0 to High(Physical) do if not Physical[J].Accepted then AppendSample(Result.PhysicalBlockers,Physical[J]);
  if not AIsCurrent then Result.Banner:='NOT CURRENT - retained baseline inspection'
  else if not Result.PhysicalClear then Result.Banner:='Selected site has physical blockers'
  else Result.Banner:='CURRENT - selected site footprint is clear';
end;

function InspectMappedWorldSite(const AResult: TMappedWorldResult;
  const AX, AY: Integer; const AIsCurrent: Boolean): TMappedWorldInspection;
begin
  RequireMappedWorldBoolean(AIsCurrent,'inspection current');
  ValidateMappedWorldSnapshotShape(AResult);
  Result:=InspectPrepared(AResult,AX,AY,AIsCurrent);
end;

function AnalyzeMappedWorldResult(const AResult: TMappedWorldResult;
  out AReport: TMappedWorldValidation): Boolean;
var L:TMappedWorldLayer; I,J:Integer; C:TMappedWorldCell; InDomain,Inside:Boolean;
  Samples:TMappedWorldSamples; Box:TWfcLatticeBox; Inspection:TMappedWorldInspection;
  procedure Issue(const Kind:TMappedWorldIssueKind; const Layer:TMappedWorldLayer;
    const Cell:Integer; const MessageText:String; const Physical:Boolean=False);
  var N:Integer;
  begin
    N:=Length(AReport.Issues); SetLength(AReport.Issues,N+1);
    AReport.Issues[N].Kind:=Kind; AReport.Issues[N].Layer:=Layer;
    AReport.Issues[N].CellIndex:=Cell; AReport.Issues[N].MessageText:=MessageText;
    if Physical then AReport.PhysicalSafe:=False else AReport.ModelValid:=False;
  end;
begin
  AReport:=Default(TMappedWorldValidation);
  try ValidateMappedWorldSnapshotShape(AResult);
  except on E:Exception do begin Issue(mwikMalformed,mwlTerrain,-1,E.Message); Exit(False); end; end;
  AReport.ModelValid:=True; AReport.PhysicalSafe:=True;
  for L:=Low(TMappedWorldLayer) to High(TMappedWorldLayer) do
    for I:=0 to High(AResult.Layers[L].Cells) do
    begin
      C:=AResult.Layers[L].Cells[I];
      if not MappedWorldTokenValid(L,C.Value) then Issue(mwikToken,L,I,'missing or invalid output token');
      if C.Locked then
      begin
        if C.Generated or (C.LockValue<>C.Value) or (not MappedWorldTokenValid(L,C.LockValue)) then
          Issue(mwikOwnership,L,I,'caller lock does not match output ownership/value');
      end
      else if (not C.Generated) or (C.LockValue<>'') then Issue(mwikOwnership,L,I,'unlocked output must be generated without a lock value');
      if C.HasDomain then
      begin
        InDomain:=False; for J:=0 to High(C.Domain) do if C.Value=C.Domain[J] then InDomain:=True;
        if not InDomain then Issue(mwikDomain,L,I,'output violates caller domain');
      end;
      if (L=mwlFoliage) and (C.Value='tree') then
      begin
        Box:=MappedWorldLiteralCellBox(AResult.Layers[L].Layout,I);
        Samples:=Collect(AResult,mwlTerrain,Box,True,'land',Box.Minimum,Inside);
        if not AllAccepted(Samples,Inside) then Issue(mwikFoliageTerrain,L,I,'tree does not stand on land');
      end;
      if L<>mwlHousing then Continue;
      if ((AResult.Demands[I]=mwdVacant) and (C.Value<>'vacant')) or
        ((AResult.Demands[I]=mwdRequired) and (C.Value<>'house')) then Issue(mwikDemand,L,I,'output violates explicit house demand');
      if C.Value<>'house' then Continue;
      Inspection:=InspectPrepared(AResult,I mod 3,I div 3,True);
      if not AllAccepted(Inspection.TerrainSamples,Inspection.TerrainQueryInBounds) then
        Issue(mwikHousingTerrain,L,I,'house terrain footprint is not entirely land');
      if not AllAccepted(Inspection.FoliageSamples,Inspection.FoliageQueryInBounds) then
        Issue(mwikHousingQuery,L,I,'house does not satisfy the selected foliage query');
      if not Inspection.PhysicalClear then
        for J:=0 to High(Inspection.PhysicalBlockers) do
          if Inspection.PhysicalBlockers[J].Layer=mwlTerrain then
            Issue(mwikPhysicalTerrain,L,I,'actual house footprint intersects non-land',True)
          else Issue(mwikPhysicalFoliage,L,I,'actual house footprint intersects non-clear foliage',True);
    end;
  Result:=AReport.ModelValid;
end;
end.
