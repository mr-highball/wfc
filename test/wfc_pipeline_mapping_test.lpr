{ SPDX-License-Identifier: MIT }
program wfc_pipeline_mapping_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_rule_model, wfc_rule_text, wfc_sequence,
  wfc_lattice, wfc_pipeline_layout, wfc_pipeline_model, wfc_pipeline_mapping;

var Checks,Failures,OracleCases:Integer;

procedure Check(const Condition:Boolean; const Detail:String);
begin
  Inc(Checks);
  if not Condition then begin Inc(Failures); WriteLn('[FAIL] ',Detail); end;
end;

function V(const X,Y,Z:Integer):TWfcLatticeVector;
begin Result:=MakeWfcLatticeVector(X,Y,Z); end;

function Tokens(const Values:array of TWfcModelToken):TWfcModelTokens;
var I:Integer;
begin Result:=nil; SetLength(Result,Length(Values));
  for I:=0 to High(Values) do Result[I]:=Values[I]; end;

function Mapped(const Kind:TGraphPassMapKind; const Match:TGraphPassMapMatch;
  const LowOffset,HighOffset:TGraphOffset; const Minimum,Maximum:Integer):TWfcPipelineRequirement;
var Q:TWfcPipelineMappedQuery;
begin
  Q:=Default(TWfcPipelineMappedQuery); Q.Kind:=Kind; Q.Match:=Match;
  Q.MinimumOffset:=LowOffset; Q.MaximumOffset:=HighOffset;
  Q.MinimumMatches:=Minimum; Q.MaximumMatches:=Maximum;
  Q.AllowedProviderTokens:=Tokens(['clear']);
  Result:=MakeWfcPipelineMappedRequirement(1,'house',0,Q);
end;

{ Independent bounded-test oracle: literal provider boxes and a fixed range
  of periodic images, NOT the production lattice point/coverage iterator.
  Test coordinates below keep all relevant images in [-32,32]. }
function Oracle(const R:TWfcPipelineRequirement; const C,P:TWfcLatticeLayout;
  const ConsumerIndex:Integer; const Values:TWfcModelTokens):Boolean;
type TVec=array[0..2] of Integer;
var CPos,CMin,CMax,QMin,QMax,POrigin,PPitch,PSize:TVec;
  X,Y,Z,A,K,N,Matches:Integer; Hit,Allowed:Boolean;

  function AxisHit(const Axis,Coordinate:Integer):Boolean;
  var Shift,FirstShift,LastShift,MinPoint,MaxPoint:Integer;
  begin
    FirstShift:=0; LastShift:=0;
    if P.Wrap then begin FirstShift:=-32; LastShift:=32; end;
    Result:=False;
    for Shift:=FirstShift to LastShift do
    begin
      MinPoint:=POrigin[Axis]+Coordinate*PPitch[Axis]+Shift*PSize[Axis]*PPitch[Axis];
      MaxPoint:=MinPoint+PPitch[Axis];
      if R.MappedQuery.Kind=gpmkPoint then
        Result:=(QMin[Axis]>=MinPoint) and (QMin[Axis]<MaxPoint)
      else Result:=(QMin[Axis]<MaxPoint) and (MinPoint<QMax[Axis]);
      if Result then Exit;
    end;
  end;

begin
  CPos[0]:=ConsumerIndex mod C.Cells.X;
  CPos[1]:=(ConsumerIndex div C.Cells.X) mod C.Cells.Y;
  CPos[2]:=ConsumerIndex div (C.Cells.X*C.Cells.Y);
  CMin[0]:=C.Origin.X+CPos[0]*C.Pitch.X; CMax[0]:=CMin[0]+C.Pitch.X;
  CMin[1]:=C.Origin.Y+CPos[1]*C.Pitch.Y; CMax[1]:=CMin[1]+C.Pitch.Y;
  CMin[2]:=C.Origin.Z+CPos[2]*C.Pitch.Z; CMax[2]:=CMin[2]+C.Pitch.Z;
  QMin[0]:=CMin[0]+R.MappedQuery.MinimumOffset.DeltaX;
  QMin[1]:=CMin[1]+R.MappedQuery.MinimumOffset.DeltaY;
  QMin[2]:=CMin[2]+R.MappedQuery.MinimumOffset.DeltaZ;
  if R.MappedQuery.Kind=gpmkCellCoverage then
  begin
    QMax[0]:=CMax[0]+R.MappedQuery.MinimumOffset.DeltaX;
    QMax[1]:=CMax[1]+R.MappedQuery.MinimumOffset.DeltaY;
    QMax[2]:=CMax[2]+R.MappedQuery.MinimumOffset.DeltaZ;
  end
  else if R.MappedQuery.Kind=gpmkRegionCoverage then
  begin
    QMax[0]:=CMin[0]+R.MappedQuery.MaximumOffset.DeltaX;
    QMax[1]:=CMin[1]+R.MappedQuery.MaximumOffset.DeltaY;
    QMax[2]:=CMin[2]+R.MappedQuery.MaximumOffset.DeltaZ;
  end else QMax:=QMin;
  POrigin[0]:=P.Origin.X; POrigin[1]:=P.Origin.Y; POrigin[2]:=P.Origin.Z;
  PPitch[0]:=P.Pitch.X; PPitch[1]:=P.Pitch.Y; PPitch[2]:=P.Pitch.Z;
  PSize[0]:=P.Cells.X; PSize[1]:=P.Cells.Y; PSize[2]:=P.Cells.Z;
  Result:=False;
  if not P.Wrap then for A:=0 to 2 do
  begin
    if QMin[A]<POrigin[A] then Exit;
    if R.MappedQuery.Kind=gpmkPoint then
    begin if QMin[A]>=POrigin[A]+PSize[A]*PPitch[A] then Exit; end
    else if QMax[A]>POrigin[A]+PSize[A]*PPitch[A] then Exit;
  end;
  N:=0; Matches:=0;
  for Z:=0 to P.Cells.Z-1 do for Y:=0 to P.Cells.Y-1 do for X:=0 to P.Cells.X-1 do
  begin
    Hit:=AxisHit(0,X) and AxisHit(1,Y) and AxisHit(2,Z);
    if not Hit then Continue;
    Inc(N); Allowed:=False;
    for K:=0 to High(R.MappedQuery.AllowedProviderTokens) do
      if Values[(Z*P.Cells.Y+Y)*P.Cells.X+X]=R.MappedQuery.AllowedProviderTokens[K] then Allowed:=True;
    if Allowed then Inc(Matches);
  end;
  if R.MappedQuery.Match=gpmmAll then Result:=Matches=N
  else Result:=(Matches>=R.MappedQuery.MinimumMatches) and (Matches<=R.MappedQuery.MaximumMatches);
end;

procedure GeometryOracle;
var C,P:TWfcLatticeLayout; R:TWfcPipelineRequirement; Values:TWfcModelTokens;
  Wrap:Boolean; Origin,Pitch,Width,Height,CPitch,COrigin,Offset,Mode,I,Cell:Integer;
  Kind:TGraphPassMapKind; Maximum:TGraphOffset;
begin
  for Wrap:=False to True do for Origin:=-1 to 1 do for Pitch:=1 to 2 do
    for Width:=1 to 3 do for Height:=1 to 2 do for CPitch:=1 to 3 do
      for COrigin:=-1 to 1 do
  begin
    P:=MakeWfcLatticeLayout(Width,Height,1,V(Origin,-1,0),V(Pitch,2,1),Wrap);
    C:=MakeWfcLatticeLayout(2,1,1,V(COrigin,-1,0),V(CPitch,2,1),False);
    SetLength(Values,Width*Height);
    for I:=0 to High(Values) do if I mod 3=1 then Values[I]:='tree' else Values[I]:='clear';
    for Kind:=Low(TGraphPassMapKind) to High(TGraphPassMapKind) do
      for Offset:=-1 to 1 do for Mode:=0 to 3 do
    begin
      Maximum:=MakeGraphOffset(0,0,0);
      if Kind=gpmkRegionCoverage then Maximum:=MakeGraphOffset(Offset+7,3,1);
      if Mode=0 then R:=Mapped(Kind,gpmmAll,MakeGraphOffset(Offset,0,0),Maximum,0,0)
      else if Kind=gpmkPoint then
      begin
        case Mode of
          1:R:=Mapped(Kind,gpmmCount,MakeGraphOffset(Offset,0,0),Maximum,0,0);
          2:R:=Mapped(Kind,gpmmCount,MakeGraphOffset(Offset,0,0),Maximum,0,1);
          3:R:=Mapped(Kind,gpmmCount,MakeGraphOffset(Offset,0,0),Maximum,1,1);
        end;
      end
      else
        case Mode of
          1:R:=Mapped(Kind,gpmmCount,MakeGraphOffset(Offset,0,0),Maximum,0,0);
          2:R:=Mapped(Kind,gpmmCount,MakeGraphOffset(Offset,0,0),Maximum,1,2);
          3:R:=Mapped(Kind,gpmmCount,MakeGraphOffset(Offset,0,0),Maximum,0,High(Integer));
        end;
      for Cell:=0 to 1 do
      begin
        Inc(OracleCases);
        Check(ValidateWfcPipelineMappedRequirement(R,C,P,Cell,Values)=Oracle(R,C,P,Cell,Values),
          'literal box/periodic image oracle case '+IntToStr(OracleCases));
      end;
    end;
  end;
end;

function RuleDocument(const Rank:Integer):String;
var Model:TWfcRuleModel; Weights:TWfcModelIntegerArray;
begin
  SetLength(Weights,2); Weights[0]:=1; Weights[1]:=1;
  Model:=TWfcRuleModel.Create(Rank,Tokens(['clear','house']),Weights,nil);
  try Result:=EncodeWfcRuleText(Model); finally Model.Free; end;
end;

function Recipe(const Topologies:TWfcPipelinePassTopologies;
  const Requirements:TWfcPipelineRequirements; const Alias:Boolean=False):TWfcPipelineModel;
var Resources:TWfcPipelineResources; Passes:TWfcPipelinePasses;
  Dependencies:TWfcPipelineDependencies; I:Integer;
begin
  SetLength(Resources,Length(Topologies)); SetLength(Passes,Length(Topologies));
  for I:=0 to High(Topologies) do
  begin
    Resources[I]:=MakeWfcPipelineResource(TWfcModelToken('r'+IntToStr(I)),wprkRules,
      RuleDocument(Topologies[I].Rank),'owned geometry fixture','MIT','mapping-test');
    Passes[I]:=MakeWfcPipelinePass(TWfcModelToken('p'+IntToStr(I)),wppvPublic,gpmOverlay,-1,
      wpakRules,I,False,wseWhole);
  end;
  if Length(Topologies)>1 then
  begin
    SetLength(Dependencies,1); Dependencies[0]:=MakeWfcPipelineDependency(1,0);
    if Alias then Passes[1]:=MakeWfcPipelinePass('p1',wppvPublic,gpmTransform,0,wpakEmpty,-1,False,wseWhole);
  end;
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('geometry','MIT','',''),
    CurrentWfcPipelineVersions,Topologies[0].Rank,Topologies[0].Wrap,rmBottomUp,
    Resources,Passes,Dependencies,nil,Requirements,nil,nil,1,Topologies);
end;

procedure ResolverContracts;
var T:TWfcPipelinePassTopologies; E:TWfcPipelinePassExtents;
  R:TWfcPipelineRequirements; M:TWfcPipelineModel; Table:TWfcPipelineLayoutTable;
  Raised:Boolean;
begin
  SetLength(T,2); T[0]:=LegacyWfcPipelinePassTopology(1,False); T[1]:=T[0];
  SetLength(E,2); E[0]:=V(2,1,1); E[1]:=V(5,1,1);
  M:=Recipe(T,nil);
  try
    Table:=ResolveWfcPipelineLayoutTable(M,E);
    try Check(Table.TotalCellCount=7,'order-only dependency permits unlike extents');
    finally Table.Free; end;
  finally M.Free; end;
  M:=Recipe(T,nil,True);
  try
    Raised:=False; Table:=nil;
    try Table:=ResolveWfcPipelineLayoutTable(M,E); except on EWfcPipelineModel do Raised:=True; end;
    Table.Free; Check(Raised,'exact transform rejects unlike extents before graph allocation');
  finally M.Free; end;
  SetLength(R,1); R[0]:=MakeWfcPipelineRequirement(1,'house',0,wprqExact,
    [MakeWfcPipelineRequirementTerm(0,0,0,Tokens(['clear']))]);
  M:=Recipe(T,R);
  try
    Raised:=False; Table:=nil;
    try Table:=ResolveWfcPipelineLayoutTable(M,E); except on EWfcPipelineModel do Raised:=True; end;
    Table.Free; Check(Raised,'old index-space requirement rejects unlike extents');
  finally M.Free; end;
  T[1]:=MakeWfcPipelinePassTopology(1,V(-10,0,0),V(3,2,1),True);
  R[0]:=Mapped(gpmkCellCoverage,gpmmAll,MakeGraphOffset(0,0,0),MakeGraphOffset(0,0,0),0,0);
  M:=Recipe(T,R);
  try
    Table:=ResolveWfcPipelineLayoutTable(M,E);
    try Check(Table.PassLayoutAt(1).Pitch.Y=2,'rank-one world thickness and local per-pass wrap survive');
    finally Table.Free; end;
  finally M.Free; end;
  T[1]:=MakeWfcPipelinePassTopology(1,V(High(Integer)-5,0,0),V(1,1,1),False);
  R[0]:=Mapped(gpmkPoint,gpmmAll,MakeGraphOffset(2,0,0),MakeGraphOffset(0,0,0),0,0);
  M:=Recipe(T,R);
  try
    Raised:=False; Table:=nil;
    try Table:=ResolveWfcPipelineLayoutTable(M,E); except on ERangeError do Raised:=True; end;
    Table.Free; Check(Raised,'late consumer anchor plus offset range rejected in preflight');
  finally M.Free; end;
end;

procedure InteriorAndLimits;
var C,P:TWfcLatticeLayout; R:TWfcPipelineRequirement; Values:TWfcModelTokens;
  I:Integer; Q: TGraphPassMapQuery; Raised:Boolean;
begin
  C:=MakeWfcLatticeLayout(1,1,1,V(4,4,0),V(8,8,1),False);
  P:=MakeWfcLatticeLayout(16,16,1,False); SetLength(Values,256);
  for I:=0 to High(Values) do Values[I]:='clear'; Values[7*16+7]:='tree';
  R:=Mapped(gpmkPoint,gpmmAll,MakeGraphOffset(0,0,0),MakeGraphOffset(0,0,0),0,0);
  Check(ValidateWfcPipelineMappedRequirement(R,C,P,0,Values),'clear corner satisfies weak point policy');
  R.MappedQuery.Kind:=gpmkCellCoverage;
  Check(not ValidateWfcPipelineMappedRequirement(R,C,P,0,Values),'interior tree defeats entire-footprint policy');
  Values[7*16+7]:='clear';
  Check(ValidateWfcPipelineMappedRequirement(R,C,P,0,Values),'cleared full footprint satisfies policy');
  P:=MakeWfcLatticeLayout(1,1,1,True); Values:=Tokens(['clear']);
  C:=MakeWfcLatticeLayout(1,1,1,V(Low(Integer),0,0),V(High(Integer),1,1),False);
  R.MappedQuery.Match:=gpmmCount; R.MappedQuery.MinimumMatches:=1; R.MappedQuery.MaximumMatches:=1;
  Check(ValidateWfcPipelineMappedRequirement(R,C,P,0,Values),'billions of wrapped world units count one unique cell');
  Q:=MakeGraphPassCellQuery(['clear','clear']); Q:=NormalizeGraphPassMapQuery(Q);
  Check(Length(Q.Values)=1,'public pure query normalization detaches canonical value set');
  Q.Values[0]:='tree';
  Check(R.MappedQuery.AllowedProviderTokens[0]='clear','geometry normalization never changes public token payload');
  Raised:=False; Q.MaximumOffset:=MakeGraphOffset(1,0,0);
  try ValidateGraphPassMappedQuery(C,Q); except on EArgumentException do Raised:=True; end;
  Check(Raised,'pure range API rejects noncanonical query before using fields');
end;

{$IFDEF PAS2JS}
procedure TypedMappingBoundary;
var R:TWfcPipelineRequirement; Values:TWfcModelTokens; L:TWfcLatticeLayout;
  Mode:Integer;

  procedure RejectMode(const AMode:Integer);
  var Bad:TWfcPipelineRequirement; Provider:TWfcModelTokens;
    ConsumerLayout,ProviderLayout:TWfcLatticeLayout;
    Reads:Integer; Raised:Boolean;
  begin
    Bad:=Mapped(gpmkPoint,gpmmAll,MakeGraphOffset(0,0,0),MakeGraphOffset(0,0,0),0,0);
    Provider:=Tokens(['clear']); Reads:=0;
    ConsumerLayout:=MakeWfcLatticeLayout(1,1,1,False);
    ProviderLayout:=MakeWfcLatticeLayout(1,1,1,False);
    asm
      if (AMode===0) Object.defineProperty(Bad,'Kind',{
        get:function(){Reads++;return pas.wfc_pipeline_model.TWfcPipelineRequirementKind.wprqMapped;}});
      if (AMode===1) {
        const query=Bad.MappedQuery;
        Object.defineProperty(Bad,'MappedQuery',{get:function(){Reads++;return query;}});
      }
      if (AMode===2) Object.defineProperty(Provider,'0',{
        get:function(){Reads++;return 'clear';}});
      if (AMode===3) Object.defineProperty(Provider,'0',{
        get:function(){Reads++;throw new Error('this accessor must never run');}});
      if (AMode===4) delete Provider[0];
      if (AMode===5) {
        delete Provider[0];
        const proto=Object.create(Array.prototype);proto[0]='clear';
        Object.setPrototypeOf(Provider,proto);
      }
      if (AMode===6) Provider[0]=3;
      if (AMode===7) Provider=null;
      if (AMode===8) Object.defineProperty(ProviderLayout,'Wrap',{
        get:function(){Reads++;return false;}});
      if (AMode===9) Object.defineProperty(ProviderLayout.Origin,'X',{
        get:function(){Reads++;return 0;}});
      if (AMode===10) Object.defineProperty(ConsumerLayout.Cells,'X',{
        get:function(){Reads++;return 1;}});
      if (AMode===11) {
        const cells=ProviderLayout.Cells;
        Object.defineProperty(ProviderLayout,'Cells',{get:function(){Reads++;return cells;}});
      }
      if (AMode===12) Object.defineProperty(ProviderLayout.Pitch,'X',{
        get:function(){Reads++;return 1;}});
      if (AMode===13) Object.defineProperty(ConsumerLayout,'Wrap',{
        get:function(){Reads++;return false;}});
    end;
    Raised:=False;
    try ValidateWfcPipelineMappedRequirement(Bad,ConsumerLayout,ProviderLayout,0,Provider);
    except on EWfcPipelineMapping do Raised:=True; end;
    Check(Raised,'typed mapped input rejected with Pascal mapping exception '+IntToStr(AMode));
    Check(Reads=0,'typed mapped input accessor never invoked '+IntToStr(AMode));
  end;

begin
  L:=MakeWfcLatticeLayout(1,1,1,False);
  for Mode:=0 to 13 do RejectMode(Mode);
  R:=Mapped(gpmkPoint,gpmmAll,MakeGraphOffset(0,0,0),MakeGraphOffset(0,0,0),0,0);
  Values:=Tokens(['clear']);
  Check(ValidateWfcPipelineMappedRequirement(R,L,L,0,Values),
    'ordinary Pascal record and provider array remain accepted');
end;
{$ENDIF}

begin
  GeometryOracle;
  ResolverContracts;
  InteriorAndLimits;
  {$IFDEF PAS2JS}TypedMappingBoundary;{$ENDIF}
  WriteLn('Pipeline mapping: ',Checks,' checks, ',OracleCases,' literal oracle cases, ',Failures,' failures.');
  if Failures<>0 then raise Exception.Create('portable mapping validation failed');
end.
