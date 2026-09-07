{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_pipeline_model_topology_boundary_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_lattice, wfc_sequence,
  wfc_pipeline_layout, wfc_pipeline_model;

type
  TInputs = record
    Rank: Integer;
    Wrap: Boolean;
    Spatial: Boolean;
    Passes: TWfcPipelinePasses;
    Topologies: TWfcPipelinePassTopologies;
  end;
var Checks: Integer;

procedure Check(const OK: Boolean; const Name: String);
begin
  Inc(Checks);
  if not OK then raise Exception.Create(Name);
end;

function Inputs(const Spatial: Boolean): TInputs;
begin
  Result.Rank := 3; Result.Wrap := False; Result.Spatial := Spatial;
  Result.Passes := nil; Result.Topologies := nil;
  SetLength(Result.Passes,1); SetLength(Result.Topologies,1);
  Result.Passes[0] := MakeWfcPipelinePass('empty',wppvPrivate,gpmOverlay,
    -1,wpakEmpty,-1,False,wseWhole);
  Result.Topologies[0] := LegacyWfcPipelinePassTopology(3,False);
end;

function NewModel(const A: TInputs): TWfcPipelineModel;
begin
  if A.Spatial then
    Result := TWfcPipelineModel.Create(MakeWfcPipelineMetadata('boundary','MIT','',''),
      CurrentWfcPipelineVersions,A.Rank,A.Wrap,rmBottomUp,nil,A.Passes,nil,nil,nil,
      nil,nil,1,A.Topologies)
  else
    Result := TWfcPipelineModel.Create(MakeWfcPipelineMetadata('boundary','MIT','',''),
      CurrentWfcPipelineVersions,A.Rank,A.Wrap,rmBottomUp,nil,A.Passes,nil,nil,nil,
      nil,nil);
end;

procedure RequireModelError(const A: TInputs; const Name: String);
var M: TWfcPipelineModel; ActualClass, Detail: String;
begin
  M := nil; ActualClass := ''; Detail := '';
  try
    try M := NewModel(A);
    except
      on E: Exception do begin ActualClass := E.ClassName; Detail := E.Message; end;
    end;
    Check((ActualClass='EWfcPipelineModel') and (Detail<>''),
      Name+': expected EWfcPipelineModel, found '+ActualClass+' ('+Detail+')');
  finally M.Free; end;
end;

procedure NativeCases;
var A: TInputs; M: TWfcPipelineModel; I: Integer;
begin
  for I:=0 to 1 do begin
    A:=Inputs(I=1); M:=NewModel(A);
    try Check((M.Rank=3) and (M.HasPassMapping=(I=1)), 'valid model identity');
    finally M.Free; end;
  end;
  A:=Inputs(True); A.Topologies[0].Pitch.X:=0;
  RequireModelError(A,'explicit invalid pitch lattice error');
  A:=Inputs(True); A.Topologies[0].Origin.X:=High(Integer);
  RequireModelError(A,'explicit world endpoint lattice error');
  A:=Inputs(True); A.Topologies[0].Rank:=0;
  RequireModelError(A,'explicit invalid rank layout error');
  A:=Inputs(True); A.Topologies[0].Pitch.Y:=-1;
  RequireModelError(A,'explicit negative pitch lattice error');
  A:=Inputs(True); A.Topologies:=nil;
  RequireModelError(A,'explicit absent topology model error');
  A:=Inputs(True); A.Topologies[0].Rank:=2;
  RequireModelError(A,'explicit pass-zero mismatch model error');
  A:=Inputs(False); A.Rank:=0;
  RequireModelError(A,'legacy invalid rank model error');
  A:=Inputs(False); A.Rank:=4;
  RequireModelError(A,'legacy future rank model error');
end;

{$IFDEF PAS2JS}
procedure HostileCases;
var A: TInputs; I,J: Integer; Bad: Integer; Reads: Integer;
begin
  for I:=0 to 9 do begin
    asm Bad=[NaN,Infinity,-Infinity,0.5,'1',null,undefined,true,{},[]][I]; end;
    for J:=0 to 4 do begin
      A:=Inputs(J>=2);
      case J of
        0: A.Rank:=Bad;
        1: asm A.Wrap=Bad; end;
        2: A.Topologies[0].Rank:=Bad;
        3: asm A.Topologies[0].Wrap=Bad; end;
        4: A.Topologies[0].Pitch.Z:=Bad;
      end;
      if (I=7) and ((J=1) or (J=3)) then Continue;
      RequireModelError(A,'hostile scalar '+IntToStr(I)+'/'+IntToStr(J));
    end;
  end;
  for I:=0 to 5 do begin
    A:=Inputs(True);
    case I of
      0: asm A.Topologies[0]=null; end;
      1: asm A.Topologies[0]=[]; end;
      2: asm A.Topologies[0].Origin=null; end;
      3: asm A.Topologies[0].Pitch={X:1,Y:1}; end;
      4: asm delete A.Topologies[0]; end;
      5: asm A.Topologies={0:A.Topologies[0],length:1}; end;
    end;
    RequireModelError(A,'hostile topology shape '+IntToStr(I));
  end;
  A:=Inputs(True); Reads:=0;
  asm Object.defineProperty(A.Topologies,'0',{get:function(){Reads++; throw new Error('slot getter');}}); end;
  RequireModelError(A,'topology slot accessor');
  Check(Reads=0,'slot accessor was not read');
  A:=Inputs(True); Reads:=0;
  asm Object.defineProperty(A.Topologies[0].Origin,'X',{get:function(){Reads++; throw new Error('origin getter');}}); end;
  RequireModelError(A,'topology origin accessor');
  Check(Reads=0,'origin accessor was not read');
end;
{$ENDIF}

begin
  NativeCases;
  {$IFDEF PAS2JS}HostileCases;{$ENDIF}
  WriteLn('Model topology boundary checks: ',Checks,' passed');
end.
