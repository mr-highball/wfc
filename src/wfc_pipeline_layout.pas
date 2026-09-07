{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Pure owned pass-layout tables. Model and invocation policy layer above this unit. }
unit wfc_pipeline_layout;
{$mode delphi}{$H+}
interface
uses SysUtils, wfc_lattice;

const WFC_PIPELINE_LAYOUT_VERSION = 1;

type
  EWfcPipelineLayoutTable = class(EWfcLattice);
  TWfcPipelinePassTopology = record
    Rank: Integer;
    Origin, Pitch: TWfcLatticeVector;
    Wrap: Boolean;
  end;
  TWfcPipelinePassTopologies = array of TWfcPipelinePassTopology;
  TWfcPipelinePassExtents = array of TWfcLatticeVector;
  TWfcPipelineLayoutIntegers = array of Integer;

  TWfcPipelineLayoutTable = class
  strict private
    FTopologies: TWfcPipelinePassTopologies;
    FLayouts: TWfcLatticeLayouts;
    FCounts, FOffsets: TWfcPipelineLayoutIntegers;
    FTotalCellCount: Integer;
    function GetPassCount: Integer;
    procedure RequirePass(const APassIndex: Integer);
  public
    constructor Create(const ATopologies: TWfcPipelinePassTopologies;
      const AExtents: TWfcPipelinePassExtents);
    function PassLayoutAt(const APassIndex: Integer): TWfcLatticeLayout;
    function PassTopologyAt(const APassIndex: Integer): TWfcPipelinePassTopology;
    function PassCellCount(const APassIndex: Integer): Integer;
    function PassOffsetAt(const APassIndex: Integer): Integer;
    function CopyLayouts: TWfcLatticeLayouts;
    function CopyExtents: TWfcPipelinePassExtents;
    function CopyTopologies: TWfcPipelinePassTopologies;
    function LocalCellIndex(const APassIndex: Integer;
      const ACell: TWfcLatticeVector): Integer;
    function FlatCellIndex(const APassIndex: Integer;
      const ACell: TWfcLatticeVector): Integer;
    property PassCount: Integer read GetPassCount;
    property TotalCellCount: Integer read FTotalCellCount;
  end;

function MakeWfcPipelinePassTopology(const ARank: Integer;
  const AOrigin, APitch: TWfcLatticeVector; const AWrap: Boolean): TWfcPipelinePassTopology;
function LegacyWfcPipelinePassTopology(const ARank: Integer;
  const AWrap: Boolean): TWfcPipelinePassTopology;

implementation

procedure LayoutError(const MessageText: String);
begin raise EWfcPipelineLayoutTable.Create(MessageText); end;

procedure RequireInteger(const AValue, AMinimum, AMaximum: Integer;
  const AName: String);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid = typeof AValue === 'number' && Number.isFinite(AValue) && Math.floor(AValue) === AValue; end;
  if not Valid then LayoutError(AName+' must be a finite Integer');
  {$ENDIF}
  if (AValue<AMinimum) or (AValue>AMaximum) then LayoutError(AName+' is out of range');
end;

procedure RequireWrap(const AWrap: Boolean);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid = typeof AWrap === 'boolean'; end;
  if not Valid then LayoutError('wrap must be Boolean');
  {$ENDIF}
end;

procedure RequireVectorShape(const AVector: TWfcLatticeVector; const AName: String);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  { Do not copy managed records until their raw containers and data fields
    have been checked. pas2js keeps untouched scalar defaults on record
    prototypes; permit those data fields, but never invoke property accessors. }
  asm
    Valid = AVector !== null && typeof AVector === 'object' && !Array.isArray(AVector);
    if (Valid) {
      for (const key of ['X','Y','Z']) {
        let p = AVector, d;
        while (p !== null) {
          d = Object.getOwnPropertyDescriptor(p,key);
          if (d) break;
          p = Object.getPrototypeOf(p);
        }
        if (!d || !Object.prototype.hasOwnProperty.call(d,'value')) { Valid = false; break; }
      }
    }
  end;
  if not Valid then LayoutError(AName+' must be a vector with passive data fields');
  {$ENDIF}
end;

procedure RequireTopologyShape(const ATopology: TWfcPipelinePassTopology);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    Valid = ATopology !== null && typeof ATopology === 'object' && !Array.isArray(ATopology);
    if (Valid) {
      for (const key of ['Rank','Origin','Pitch','Wrap']) {
        let p = ATopology, d;
        while (p !== null) {
          d = Object.getOwnPropertyDescriptor(p,key);
          if (d) break;
          p = Object.getPrototypeOf(p);
        }
        if (!d || !Object.prototype.hasOwnProperty.call(d,'value')) { Valid = false; break; }
      }
    }
  end;
  if not Valid then LayoutError('topology must be a record with passive data fields');
  {$ENDIF}
end;

procedure ValidateInputContainers(const ATopologies: TWfcPipelinePassTopologies;
  const AExtents: TWfcPipelinePassExtents);
var I: Integer;
{$IFDEF PAS2JS}Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid = Array.isArray(ATopologies) && Array.isArray(AExtents); end;
  if not Valid then LayoutError('topologies and extents must be arrays');
  {$ENDIF}
  if (Length(ATopologies)<1) or (Length(ATopologies)>High(Integer)) then
    LayoutError('pass count must fit a positive Integer');
  if Length(ATopologies)<>Length(AExtents) then LayoutError('topology and extent counts differ');
  for I:=0 to Length(ATopologies)-1 do
  begin
    {$IFDEF PAS2JS}
    asm
      const t = Object.getOwnPropertyDescriptor(ATopologies,String(I));
      const e = Object.getOwnPropertyDescriptor(AExtents,String(I));
      Valid = !!t && !!e && Object.prototype.hasOwnProperty.call(t,'value') &&
        Object.prototype.hasOwnProperty.call(e,'value');
    end;
    if not Valid then LayoutError('pass arrays require dense own data entries');
    {$ENDIF}
    RequireTopologyShape(ATopologies[I]);
    RequireVectorShape(AExtents[I],'extent');
    RequireVectorShape(ATopologies[I].Origin,'origin');
    RequireVectorShape(ATopologies[I].Pitch,'pitch');
  end;
end;

function ResolveOne(const ATopology: TWfcPipelinePassTopology;
  const AExtent: TWfcLatticeVector): TWfcLatticeLayout;
begin
  { Callers preflight containers first; the lattice factory validates every
    component and world endpoint before copying its vector inputs. }
  RequireInteger(ATopology.Rank,1,3,'rank');
  RequireWrap(ATopology.Wrap);
  RequireInteger(AExtent.X,1,High(Integer),'extent.X');
  RequireInteger(AExtent.Y,1,High(Integer),'extent.Y');
  RequireInteger(AExtent.Z,1,High(Integer),'extent.Z');
  if (ATopology.Rank=1) and ((AExtent.Y<>1) or (AExtent.Z<>1)) then
    LayoutError('rank-one local Y and Z extents must be one');
  if (ATopology.Rank=2) and (AExtent.Z<>1) then
    LayoutError('rank-two local Z extent must be one');
  Result:=MakeWfcLatticeLayout(AExtent.X,AExtent.Y,AExtent.Z,
    ATopology.Origin,ATopology.Pitch,ATopology.Wrap);
end;

function MakeWfcPipelinePassTopology(const ARank: Integer;
  const AOrigin, APitch: TWfcLatticeVector; const AWrap: Boolean): TWfcPipelinePassTopology;
var L: TWfcLatticeLayout;
begin
  RequireInteger(ARank,1,3,'rank'); RequireWrap(AWrap);
  RequireVectorShape(AOrigin,'origin'); RequireVectorShape(APitch,'pitch');
  { Every legal extent is positive, so even an otherwise unused local axis
    requires enough world capacity for one cell with its actual pitch. }
  L:=MakeWfcLatticeLayout(1,1,1,AOrigin,APitch,AWrap);
  Result.Rank:=ARank; Result.Origin:=L.Origin; Result.Pitch:=L.Pitch; Result.Wrap:=L.Wrap;
end;

function LegacyWfcPipelinePassTopology(const ARank: Integer;
  const AWrap: Boolean): TWfcPipelinePassTopology;
begin
  Result:=MakeWfcPipelinePassTopology(ARank,MakeWfcLatticeVector(0,0,0),
    MakeWfcLatticeVector(1,1,1),AWrap);
end;

constructor TWfcPipelineLayoutTable.Create(const ATopologies: TWfcPipelinePassTopologies;
  const AExtents: TWfcPipelinePassExtents);
var I,Count,Total: Integer; L: TWfcLatticeLayout;
begin
  inherited Create;
  ValidateInputContainers(ATopologies,AExtents);
  Total:=0;
  for I:=0 to Length(ATopologies)-1 do
  begin
    L:=ResolveOne(ATopologies[I],AExtents[I]);
    Count:=WfcLatticeCellCount(L);
    if Total>High(Integer)-Count then LayoutError('aggregate cell count exceeds flat-key Integer capacity');
    Inc(Total,Count);
  end;
  { No owned array allocation precedes the complete
    all-pass geometry and total-count preflight above. Storage is O(passes),
    never O(total cells), and no model or graph is consulted. }
  SetLength(FTopologies,Length(ATopologies)); SetLength(FLayouts,Length(ATopologies));
  SetLength(FCounts,Length(ATopologies)); SetLength(FOffsets,Length(ATopologies));
  Total:=0;
  for I:=0 to Length(ATopologies)-1 do
  begin
    L:=ResolveOne(ATopologies[I],AExtents[I]);
    FLayouts[I]:=L; FCounts[I]:=WfcLatticeCellCount(L); FOffsets[I]:=Total;
    FTopologies[I].Rank:=ATopologies[I].Rank;
    FTopologies[I].Origin:=L.Origin; FTopologies[I].Pitch:=L.Pitch; FTopologies[I].Wrap:=L.Wrap;
    Inc(Total,FCounts[I]);
  end;
  FTotalCellCount:=Total;
end;

function TWfcPipelineLayoutTable.GetPassCount: Integer;
begin Result:=Length(FLayouts); end;

procedure TWfcPipelineLayoutTable.RequirePass(const APassIndex: Integer);
begin RequireInteger(APassIndex,0,PassCount-1,'pass index'); end;

function TWfcPipelineLayoutTable.PassLayoutAt(const APassIndex: Integer): TWfcLatticeLayout;
begin RequirePass(APassIndex); Result:=FLayouts[APassIndex]; end;

function TWfcPipelineLayoutTable.PassTopologyAt(const APassIndex: Integer): TWfcPipelinePassTopology;
begin RequirePass(APassIndex); Result:=FTopologies[APassIndex]; end;

function TWfcPipelineLayoutTable.PassCellCount(const APassIndex: Integer): Integer;
begin RequirePass(APassIndex); Result:=FCounts[APassIndex]; end;

function TWfcPipelineLayoutTable.PassOffsetAt(const APassIndex: Integer): Integer;
begin RequirePass(APassIndex); Result:=FOffsets[APassIndex]; end;

function TWfcPipelineLayoutTable.CopyLayouts: TWfcLatticeLayouts;
var I: Integer;
begin
  Result:=nil; SetLength(Result,PassCount);
  for I:=0 to PassCount-1 do Result[I]:=FLayouts[I];
end;

function TWfcPipelineLayoutTable.CopyExtents: TWfcPipelinePassExtents;
var I: Integer;
begin
  Result:=nil; SetLength(Result,PassCount);
  for I:=0 to PassCount-1 do Result[I]:=FLayouts[I].Cells;
end;

function TWfcPipelineLayoutTable.CopyTopologies: TWfcPipelinePassTopologies;
var I: Integer;
begin
  Result:=nil; SetLength(Result,PassCount);
  for I:=0 to PassCount-1 do Result[I]:=FTopologies[I];
end;

function TWfcPipelineLayoutTable.LocalCellIndex(const APassIndex: Integer;
  const ACell: TWfcLatticeVector): Integer;
begin
  RequirePass(APassIndex); RequireVectorShape(ACell,'local cell');
  RequireInteger(ACell.X,0,FLayouts[APassIndex].Cells.X-1,'cell.X');
  RequireInteger(ACell.Y,0,FLayouts[APassIndex].Cells.Y-1,'cell.Y');
  RequireInteger(ACell.Z,0,FLayouts[APassIndex].Cells.Z-1,'cell.Z');
  { Cell-count and coordinate checks prove each intermediate fits Integer. }
  Result:=(ACell.Z*FLayouts[APassIndex].Cells.Y+ACell.Y)*FLayouts[APassIndex].Cells.X+ACell.X;
end;

function TWfcPipelineLayoutTable.FlatCellIndex(const APassIndex: Integer;
  const ACell: TWfcLatticeVector): Integer;
var Local: Integer;
begin
  Local:=LocalCellIndex(APassIndex,ACell);
  Result:=FOffsets[APassIndex]+Local;
end;

end.
