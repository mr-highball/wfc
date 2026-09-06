{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_mapped_world_test;
{$mode delphi}{$H+}
uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,wfc,mapped_world_types,mapped_world_validation,mapped_world_workbench;
type TTest = procedure;
var Checks,Failures:Integer;
procedure Check(const OK:Boolean; const MessageText:String);
begin
  Inc(Checks); if not OK then begin Inc(Failures); WriteLn('[FAIL] ',MessageText); end;
end;
procedure Test(const Name:String; const Proc:TTest);
begin
  WriteLn('[TEST] ',Name);
  try Proc; except on E:Exception do begin Inc(Failures); WriteLn('[EXCEPTION] ',E.ClassName,': ',E.Message); end; end;
end;
function Values(const A:array of String):TGraphValues;
var I:Integer;
begin Result:=nil; SetLength(Result,Length(A)); for I:=0 to High(A) do Result[I]:=A[I]; end;
function Study:TMappedWorldSession;
begin Result:=TMappedWorldSession.Create(DefaultMappedWorldConfig); end;
procedure SharedWalkthrough;
begin Check(MappedWorldSelfTest=16,'portable public tutorial walkthrough passes all16 checks'); end;

procedure DetachedOwnership;
var S:TMappedWorldSession; O:TMappedWorldSearchOptions; A,B:TMappedWorldResult;
  I,J:TMappedWorldInspection; R,T:TMappedWorldReport; D:TMappedWorldDemands;
  Input:TGraphValues; OriginalTraceValue:String;
  Selective:TGraphSelectiveNegotiationReport; Negotiation:TGraphNegotiationOptions;
begin
  S:=Study;
  try
    O:=DefaultMappedWorldSearchOptions; O.CaptureTrace:=True;
    Check(S.Run(mwaGenerate,O),'detached fixture baseline');
    Check(S.CopyCurrent(A),'copy first current result');
    A.Config.RegionMaximum.DeltaX:=99; A.Layers[mwlTerrain].Layout.Pitch.X:=99;
    A.Layers[mwlFoliage].Cells[0].Value:='corrupt'; A.Layers[mwlFoliage].Cells[0].Domain[0]:='corrupt';
    A.Demands[0]:=mwdRequired;
    Check(S.CopyCurrent(B) and (B.Config.RegionMaximum.DeltaX=8)
      and (B.Layers[mwlTerrain].Layout.Pitch.X=4)
      and (B.Layers[mwlFoliage].Cells[0].Value='clear')
      and (B.Layers[mwlFoliage].Cells[0].Domain[0]='clear')
      and (B.Demands[0]=mwdVacant),'result nested arrays and layout/config records are detached');
    Check(S.CopyInspection(0,0,I),'copy inspector');
    I.TerrainSamples[0].Cell.Domain[0]:='corrupt'; I.PhysicalBlockers[0].Cell.Value:='corrupt';
    Check(S.CopyInspection(0,0,J) and (J.TerrainSamples[0].Cell.Domain[0]='land')
      and (J.PhysicalBlockers[0].Cell.Value='tree'),'inspector sample cells and domains are detached');
    D:=S.CopyDemands; D[0]:=mwdRequired;
    Check(S.GetDemand(0,0)=mwdVacant,'copied demands do not mutate the session');
    S.SetDemand(0,0,mwdRequired); Check(S.Run(mwaFoliageAndHousing,O),'captured negotiated repair');
    R:=S.CopyReport;
    Check((Length(R.NegotiationReport.Attempts)=1) and (R.PassBacktracks=1)
      and (R.Rounds=2),'actual repair records one rejected assignment and two rounds');
    Check((Length(R.ActivePassIndices)=2) and (R.ActivePassIndices[0]=1)
      and (R.ActivePassIndices[1]=2),'report gives actual foliage/housing closure');
    Selective:=Default(TGraphSelectiveNegotiationReport); Selective.ScopeAlgorithmVersion:=R.ScopeAlgorithmVersion;
    Selective.RequestedRootIndices:=R.RequestedRootIndices; Selective.ActivePassIndices:=R.ActivePassIndices;
    Selective.Search:=R.NegotiationReport; Negotiation:=DefaultGraphNegotiationOptions;
    Negotiation.SolveOptions.MaxBacktracks:=O.MaxBacktracks;
    Negotiation.SolveOptions.CaptureTrace:=O.CaptureTrace; Negotiation.MaxPassBacktracks:=O.MaxPassBacktracks;
    Check(R.TranscriptHash=CalculateGraphSelectiveNegotiationTranscriptHash(Negotiation,Selective),
      'selective transcript identity binds actual requested roots and active closure');
    Check((R.NegotiationReport.TranscriptHash=CalculateGraphNegotiationTranscriptHash(Negotiation,R.NegotiationReport))
      and (R.TranscriptHash<>R.NegotiationReport.TranscriptHash),
      'inner chronology hash remains available separately from the scope-bound transcript');
    Check(S.CopyCurrent(B) and (B.TranscriptSignature=R.TranscriptHash),
      'published capture pins the actual selective scope identity');
    OriginalTraceValue:=R.SolveReport.Trace[0].Value;
    R.SolveReport.Trace[0].Value:='corrupt'; R.SolveReport.ExecutionOrder[0]:=77;
    R.RequestedRootIndices[0]:=77; R.ActivePassIndices[0]:=77; R.Passes[1].Decisions:=-123;
    R.NegotiationReport.Attempts[0].SolveReport.Trace[0].Value:='corrupt';
    R.NegotiationReport.Attempts[0].ExcludedAssignment[0]:=77;
    T:=S.CopyReport;
    Check((T.SolveReport.Trace[0].Value=OriginalTraceValue)
      and (T.SolveReport.ExecutionOrder[0]<>77) and (T.RequestedRootIndices[0]=1)
      and (T.ActivePassIndices[0]=1) and (T.Passes[1].Decisions>=0),
      'report arrays and final trace are deeply detached');
    Check((T.NegotiationReport.Attempts[0].SolveReport.Trace[0].Value<>'corrupt')
      and (T.NegotiationReport.Attempts[0].ExcludedAssignment[0]<>77),
      'nested negotiation traces and exact exclusions are detached');
    Input:=Values(['tree','clear','tree']); S.SetDomain(mwlFoliage,7,7,Input); Input[0]:='corrupt';
    Check(S.Run(mwaFoliageAndHousing,O) and S.CopyCurrent(B),'caller domain array cannot corrupt a subsequent repair');
    Check(Length(B.Layers[mwlFoliage].Cells[7*32+7].Domain)=2,'domain is copied and canonicalized as a set');
  finally S.Free; end;
end;

procedure DirtyScopesAndArtifacts;
var S:TMappedWorldSession; O:TMappedWorldSearchOptions; A:TMappedWorldResult;
  R:TMappedWorldReport; I:TMappedWorldInspection; Text:String;
begin
  S:=Study;
  try
    O:=DefaultMappedWorldSearchOptions;
    Check(not S.Run(mwaHousingOnly,O) and not S.HasBaseline,'a new session refuses selective repair');
    Check(S.Run(mwaGenerate,O),'Generate remains a full solve after a no-baseline repair attempt');
    S.SetDomain(mwlFoliage,7,7,Values(['clear'])); S.SetDemand(0,0,mwdRequired);
    Check((S.Status=mwstDirty) and not S.HasCurrent and S.HasBaseline,'edits hide current output while keeping baseline');
    Text:='stale'; Check(not S.TryCurrentSvg(0,0,Text) and (Text=''),'dirty safe SVG access clears previous caller text');
    A:=Default(TMappedWorldResult); A.Revision:=999;
    Check(not S.CopyCurrent(A) and (A.Revision=0),'dirty current copy resets its out record');
    Check(S.CopyInspection(0,0,I) and not I.IsCurrent and (Pos('NOT CURRENT',I.Banner)>0),
      'dirty inspector clearly names its retained baseline');
    Check(S.TryDiagnosticSvg(0,0,Text) and (Pos('NOT CURRENT',Text)>0),'dirty diagnostic is explicitly watermarked');
    Check(not S.Run(mwaHousingOnly,O),'leaf scope cannot ignore an edited provider');
    R:=S.CopyReport; Check((R.Status=mwstScopeMismatch) and (R.Rounds=0),'scope mismatch is not reported as a solver contradiction');
    Check(S.Run(mwaFoliageAndHousing,O) and S.CopyCurrent(A) and A.PhysicalSafe,'authorized edited-provider repair publishes safe output');
    Check(A.Layers[mwlFoliage].Cells[7*32+7].Generated
      and not A.Layers[mwlFoliage].Cells[7*32+7].Locked,'domain-constrained output remains genuinely generated');
    S.SetDemand(0,0,mwdRequired);
    Check(S.HasCurrent,'idempotent demand does not invalidate a current result');
    S.SetDomain(mwlTerrain,0,0,Values(['water']));
    Check(not S.Run(mwaFoliageAndHousing,O) and (S.Status=mwstScopeMismatch),'terrain edit requires explicit all-pass scope');
    Check(S.Run(mwaAllPasses,O) and S.CopyCurrent(A),'all-pass authorization regenerates edited terrain and descendants');
    Check((A.Layers[mwlTerrain].Cells[0].Value='water')
      and (A.Layers[mwlFoliage].Cells[0].Value='clear'),'terrain edits are real and dependent foliage remains valid');
    Check(S.TryCurrentSvg(2,1,Text) and (Pos('data-house-index="5"',Text)>0),'selected-site safe renderer remains available');
  finally S.Free; end;
end;

procedure LocksDomainsAndLimits;
var S:TMappedWorldSession; O:TMappedWorldSearchOptions; A:TMappedWorldResult; R:TMappedWorldReport;
begin
  S:=Study;
  try
    O:=DefaultMappedWorldSearchOptions;
    Check(S.Run(mwaGenerate,O),'lock fixture baseline');
    S.SetLock(mwlFoliage,7,7,'tree'); S.SetDemand(0,0,mwdRequired);
    Check(not S.Run(mwaFoliageAndHousing,O),'upstream negotiation cannot rewrite a caller-locked tree');
    Check(S.HasBaseline and not S.HasCurrent,'locked contradiction keeps only the hidden baseline');
    S.ClearLock(mwlFoliage,7,7); S.SetDomain(mwlFoliage,7,7,Values(['clear']));
    Check(S.Run(mwaFoliageAndHousing,O) and S.CopyCurrent(A),'unlock and explicit clear domain can repair the demanded house');
    Check(A.Layers[mwlFoliage].Cells[7*32+7].Generated
      and not A.Layers[mwlFoliage].Cells[7*32+7].Locked,'unlock restores generated ownership through the solver');
    S.SetLock(mwlFoliage,7,7,'clear');
    Check(S.Run(mwaFoliageAndHousing,O) and S.CopyCurrent(A),'compatible caller lock solves');
    Check(A.Layers[mwlFoliage].Cells[7*32+7].Locked
      and not A.Layers[mwlFoliage].Cells[7*32+7].Generated
      and (A.Layers[mwlFoliage].Cells[7*32+7].LockValue='clear'),'explicit caller ownership is faithfully captured');
    S.SetDomain(mwlHousing,0,0,Values(['vacant']));
    Check(not S.Run(mwaHousingOnly,O),'house demand intersects a conflicting cell domain instead of overriding it');
    S.ClearDomain(mwlHousing,0,0);
    Check(S.Run(mwaHousingOnly,O) and S.CopyCurrent(A)
      and (A.Layers[mwlHousing].Cells[0].Value='house'),'clearing only the conflicting domain preserves explicit house demand');
    S.SetDomain(mwlFoliage,7,7,Values([]));
    Check(not S.Run(mwaFoliageAndHousing,O),'an explicitly empty domain is an honest contradiction');
    S.ClearDomain(mwlFoliage,7,7);
    Check(S.Run(mwaFoliageAndHousing,O) and S.CopyCurrent(A)
      and not A.Layers[mwlFoliage].Cells[7*32+7].HasDomain,'ClearDomain differs from assigning an empty set');
    S.Reset(DefaultMappedWorldConfig); Check(S.Run(mwaGenerate,O),'budget fixture baseline'); S.SetDemand(0,0,mwdRequired);
    O.MaxPassBacktracks:=0;
    Check(not S.Run(mwaFoliageAndHousing,O),'zero pass reopening budget cannot repair this deterministic provider assignment');
    R:=S.CopyReport; Check((R.Status=mwstPassLimit) and (R.PassBacktracks=0)
      and (Pos('not proof of infeasibility',R.Detail)>0),'pass exhaustion is distinct from proven infeasibility');
    O.MaxPassBacktracks:=16; Check(S.Run(mwaFoliageAndHousing,O),'increased explicit budget permits a fresh authorized repair');
  finally S.Free; end;
end;

procedure ResetAndInvalidEdits;
var S:TMappedWorldSession; C,Bad:TMappedWorldConfig; O:TMappedWorldSearchOptions;
  A,B:TMappedWorldResult; Raised:Boolean; K:Integer;
begin
  S:=Study;
  try
    O:=DefaultMappedWorldSearchOptions; Check(S.Run(mwaGenerate,O) and S.CopyCurrent(A),'reset fixture baseline');
    for K:=0 to 3 do begin
      Bad:=DefaultMappedWorldConfig;
      case K of 0:Bad.RegionMaximum.DeltaX:=Bad.RegionMinimum.DeltaX;
        1:Bad.TreeWeight:=0; 2:Bad.RegionMaximum.DeltaX:=High(Integer);
        3:Bad.RegionMinimum.DeltaY:=High(Integer); end;
      Raised:=False; try S.Reset(Bad); except on E:Exception do Raised:=True; end;
      Check(Raised and S.CopyCurrent(B) and (B.Signature=A.Signature),
        'invalid reset leaves the prior graph, configuration and current capture intact');
    end;
    for K:=0 to 3 do begin
      Raised:=False;
      try case K of 0:S.SetLock(mwlFoliage,7,7,'not-a-token');
        1:S.SetDomain(mwlTerrain,0,0,Values(['tree']));
        2:S.SetDemand(3,0,mwdRequired); 3:S.ClearLock(mwlHousing,-1,0); end;
      except on E:Exception do Raised:=True; end;
      Check(Raised and S.CopyCurrent(B) and (B.Signature=A.Signature),'invalid cell edit is atomic');
    end;
    O.MaxBacktracks:=-1; Raised:=False;
    try S.Run(mwaGenerate,O); except on E:Exception do Raised:=True; end;
    Check(Raised and S.CopyCurrent(B) and (B.Signature=A.Signature),'invalid options do not destroy a valid owner state');
    C:=DefaultMappedWorldConfig; C.Seed:=0; S.Reset(C); O:=DefaultMappedWorldSearchOptions;
    Check((S.Status=mwstIdle) and not S.HasBaseline and not S.HasCurrent,'new seed session clears both current and baseline');
    Check(not S.Run(mwaHousingOnly,O) and S.Run(mwaGenerate,O) and S.CopyCurrent(B),
      'new-session Generate is not poisoned by a previous selective scope');
    Check((B.Config.Seed=0) and (B.Layers[mwlFoliage].Cells[7*32+7].Value='clear'),
      'new seed genuinely changes generated output');
  finally S.Free; end;
end;

procedure SandboxAndRegion;
var C:TMappedWorldConfig; S:TMappedWorldSession; O:TMappedWorldSearchOptions;
  A:TMappedWorldResult; Validation:TMappedWorldValidation; I,Trees,Water,FreeFoliage:Integer;
  Inspection:TMappedWorldInspection;
begin
  C:=DefaultMappedWorldConfig; C.Preset:=mwpLandscapeSandbox; C.Seed:=11;
  S:=TMappedWorldSession.Create(C);
  try
    O:=DefaultMappedWorldSearchOptions; Check(S.Run(mwaGenerate,O) and S.CopyCurrent(A),'unconstrained weighted landscape baseline');
    Trees:=0; Water:=0; FreeFoliage:=0;
    for I:=0 to High(A.Layers[mwlTerrain].Cells) do if A.Layers[mwlTerrain].Cells[I].Value='water' then Inc(Water);
    for I:=0 to High(A.Layers[mwlFoliage].Cells) do begin
      if A.Layers[mwlFoliage].Cells[I].Value='tree' then Inc(Trees);
      if not A.Layers[mwlFoliage].Cells[I].HasDomain then Inc(FreeFoliage);
    end;
    Check((Water>0) and (Trees>1) and (FreeFoliage=768),'sandbox genuinely generates varied terrain and many free foliage choices');
    Check(AnalyzeMappedWorldResult(A,Validation) and Validation.PhysicalSafe,'every claimed sandbox house passes independent full-footprint policy');
    for I:=0 to 5 do Check(S.GetDemand(I mod 3,I div 3)=mwdOptional,'sandbox houses begin optional, not silently required');
    A.Layers[mwlFoliage].Cells[0].Value:='outside';
    Check(not AnalyzeMappedWorldResult(A,Validation),'independent validator rejects corruption of a previously solved capture');
    C:=DefaultMappedWorldConfig; C.Sampling:=mwsRegion; S.Reset(C);
    Check(S.Run(mwaGenerate,O) and S.CopyInspection(0,0,Inspection)
      and (Length(Inspection.FoliageSamples)=64),'equal-size region samples the complete 64-cell footprint');
    C.RegionMinimum:=MakeGraphOffset(-1,-1,0); C.RegionMaximum:=MakeGraphOffset(9,9,1); S.Reset(C);
    Check(S.Run(mwaGenerate,O) and S.CopyInspection(2,1,Inspection)
      and Inspection.FoliageQueryInBounds and (Length(Inspection.FoliageSamples)=100),
      'setback region inspects the full expanded area at another site');
    C.RegionMinimum:=MakeGraphOffset(-5,0,0); S.Reset(C); Check(S.Run(mwaGenerate,O),'outside-query baseline can remain vacant');
    S.SetDemand(0,0,mwdRequired); O.Negotiated:=False;
    Check(not S.Run(mwaHousingOnly,O),'bounded outside-region demand cannot succeed by clipping its sample');
  finally S.Free; end;
end;

{$IFDEF PAS2JS}
procedure HostileBrowserEdits;
var S:TMappedWorldSession; O:TMappedWorldSearchOptions; A,B:TMappedWorldResult;
  I,N:Integer; V:TGraphValues; Raised:Boolean;
begin
  S:=Study;
  try
    O:=DefaultMappedWorldSearchOptions; Check(S.Run(mwaGenerate,O) and S.CopyCurrent(A),'hostile-input baseline');
    for I:=0 to 5 do begin
      case I of 0:asm N=NaN; end; 1:asm N=Infinity; end; 2:asm N=0.5; end;
        3:asm N='1'; end; 4:asm N=null; end; 5:asm N=true; end; end;
      Raised:=False; try S.SetDemand(N,0,mwdRequired); except on E:Exception do Raised:=True; end;
      Check(Raised and S.CopyCurrent(B) and (B.Signature=A.Signature),'hostile numeric edit rejected atomically');
    end;
    for I:=0 to 4 do begin
      V:=Values(['clear']);
      case I of 0:asm V='clear'; end; 1:asm V={length:1,0:'clear'}; end;
        2:asm V=[null]; end; 3:asm V=[1]; end; 4:asm V=[new String('clear')]; end; end;
      Raised:=False; try S.SetDomain(mwlFoliage,7,7,V); except on E:Exception do Raised:=True; end;
      Check(Raised and S.CopyCurrent(B) and (B.Signature=A.Signature),'hostile domain container/token rejected atomically');
    end;
  finally S.Free; end;
end;
{$ENDIF}

begin
  Test('shared walkthrough',SharedWalkthrough);
  Test('deep-copy ownership for inputs, snapshots, inspections and transcripts',DetachedOwnership);
  Test('dirty scopes and safe versus retained diagnostic artifacts',DirtyScopesAndArtifacts);
  Test('explicit locks, domain intersections and bounded negotiation',LocksDomainsAndLimits);
  Test('atomic invalid resets and edits, then new-seed generation',ResetAndInvalidEdits);
  Test('genuinely free sandbox, independent corruption check and regions',SandboxAndRegion);
  {$IFDEF PAS2JS}Test('hostile browser input guards',HostileBrowserEdits);{$ENDIF}
  WriteLn('Mapped-world checks: ',Checks-Failures,'/',Checks);
  if Failures<>0 then Halt(1);
end.
