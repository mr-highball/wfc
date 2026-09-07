{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_pass_bridge_layout_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_lattice, wfc_model, wfc_learn3d,
  wfc_sequence, wfc_sequence_learn, wfc_sequence_graph,
  wfc_pattern2d, wfc_pattern2d_learn, wfc_pattern2d_graph,
  wfc_pattern3d, wfc_pattern3d_learn, wfc_pattern3d_graph,
  wfc_voxel3d, wfc_voxel3d_passes;

var Checks, Failures: Integer;

procedure Check(const OK: Boolean; const Msg: String);
begin
  Inc(Checks);
  if not OK then begin Inc(Failures); WriteLn('[FAIL] ', Msg); end;
end;

function Tokens: TWfcModelTokens;
begin Result:=nil; SetLength(Result, 1); Result[0] := 'A'; end;

function Layouts(const W,H,D,Count,Variant: Integer;
  const Wrap: Boolean): TWfcLatticeLayouts;
var I, Source: Integer;
begin
  Result:=nil; SetLength(Result,Count);
  for I:=0 to Count-1 do
    Result[I]:=MakeWfcLatticeLayout(W,H,D,
      MakeWfcLatticeVector(-7,3,-11),MakeWfcLatticeVector(2,3,4),Wrap);
  { For a multi-provider bundle the first source remains compatible. The
    later source must fail during shared preflight, before the first write. }
  Source:=Count-2;
  case Variant of
    1:Inc(Result[Source].Cells.X);
    2:Inc(Result[Source].Cells.Y);
    3:Inc(Result[Source].Cells.Z);
    4:Inc(Result[Source].Origin.X);
    5:Inc(Result[Source].Origin.Y);
    6:Inc(Result[Source].Origin.Z);
    7:Inc(Result[Source].Pitch.X);
    8:Inc(Result[Source].Pitch.Y);
    9:Inc(Result[Source].Pitch.Z);
    10:Result[Source].Wrap:=not Wrap;
  end;
end;

function NewGraph(const L: TWfcLatticeLayouts): TGraph;
var I: Integer;
begin
  Result:=TGraph.Create;
  try
    Result.Reshape(1,1,1);
    for I:=0 to High(L) do
    begin
      if I=0 then Result.CurrentPass:='p0'
      else Result.SwitchToPass('p'+IntToStr(I));
      Result.PassMode:=gpmOverlay;
      Result.ClearDependencies;
    end;
    Result.ConfigurePassLayouts(L);
  except Result.Free; raise; end;
end;

procedure CheckUnchanged(const G: TGraph; const BeforeValues,
  BeforeRules, BeforeDeps: Integer; const LabelText: String);
begin
  Check(Length(G.CopyRegisteredValues)=BeforeValues,LabelText+' values unchanged');
  Check(G.RuleGroups.Count=BeforeRules,LabelText+' rules unchanged');
  Check(G.DependencyCount=BeforeDeps,LabelText+' dependencies unchanged');
end;

procedure TestPatterns;
var M2:TWfcOverlappingModel2D; M3:TWfcOverlappingModel3D;
  G:TGraph; Rank,Variant,Operation:Integer; Rejected:Boolean; Why:String;
begin
  M2:=LearnOverlappingModel2D(Tokens,1,1,1,1,wmbWrap,wmsNone);
  M3:=LearnOverlappingModel3D(Tokens,1,1,1,1,1,1,wmbWrap,wmsNone);
  try
    for Rank:=2 to 3 do for Variant:=0 to 9 do
      for Operation:=0 to 1 do
      begin
        if (Rank=2) and (Variant=3) then Continue;
        if Rank=2 then G:=NewGraph(Layouts(2,2,1,2,Variant,True))
        else G:=NewGraph(Layouts(2,2,2,2,Variant,True));
        try
          if Rank=2 then ApplyOverlappingModel2DToGraph(M2,G.PassGraph[0])
          else ApplyOverlappingModel3DToGraph(M3,G.PassGraph[0]);
          Rejected:=False;
          try
            if Rank=2 then
            begin
              if Operation=0 then ValidateOverlappingProjectionFromPass2D(M2,G,'p0')
              else ApplyOverlappingProjectionFromPass2D(M2,G,'p0');
            end
            else if Operation=0 then ValidateOverlappingProjectionFromPass3D(M3,G,'p0')
            else ApplyOverlappingProjectionFromPass3D(M3,G,'p0');
          except on E:Exception do begin Rejected:=True;
            Check(Pos('identical pass layouts',E.Message)>0,'pattern layout-specific rejection'); end;
          end;
          Why:='pattern'+IntToStr(Rank)+' variant'+IntToStr(Variant)+' op'+IntToStr(Operation);
          Check(Rejected=(Variant<>0),Why+' expected acceptance');
          if (Variant<>0) or (Operation=0) then CheckUnchanged(G,0,0,0,Why)
          else Check(G.DependencyCount=1,Why+' matching nondefault layout installs');
        finally G.Free; end;
      end;
  finally M3.Free; M2.Free; end;
end;

procedure ApplySequence(const M:TWfcSequenceModel; const G:TGraph);
begin
  if G.PassLayout.Wrap then ApplySequenceModelToGraph(M,G,wseWrap)
  else ApplySequenceModelToGraph(M,G,wseFragment);
end;

procedure TestSequences;
var M:TWfcSequenceModel; G:TGraph; B:TWfcSequenceProjectionBindings;
  R:TWfcSequenceProjectionRules; Variant,Operation,N,I,BeforeValues,
  BeforeRules:Integer; Rejected:Boolean; Why:String;
begin
  M:=LearnSequenceModel(Tokens,1,wmbWrap);
  SetLength(R,1); R[0]:=MakeWfcSequenceProjectionRule('A',Tokens);
  try
    for Variant:=0 to 10 do for Operation:=0 to 8 do
    begin
      if Variant in [2,3] then Continue;
      N:=2; if Operation>=5 then N:=3;
      G:=NewGraph(Layouts(2,1,1,N,Variant,False));
      try
        if Operation=0 then G.PassGraph[0].AddValue('A')
        else for I:=0 to N-2 do ApplySequence(M,G.PassGraph[I]);
        if Operation in [1,2] then G.AddValue('A') else ApplySequence(M,G);
        SetLength(B,N-1);
        for I:=0 to High(B) do
          B[I]:=MakeWfcSequenceProjectionBinding(M,'p'+IntToStr(I),R);
        BeforeValues:=Length(G.CopyRegisteredValues); BeforeRules:=G.RuleGroups.Count;
        Rejected:=False;
        try
          case Operation of
            0:RequireSequenceProjectionFromTokenPass(M,G,'p0');
            1:RequireProjectedSequenceFromPass(M,G,'A','p0',Tokens);
            2:RequirePartialProjectedSequenceFromPass(M,G,'A','p0',Tokens);
            3:ValidateSequenceProjectionMapFromPass(M,M,G,'p0',R);
            4:RequireSequenceProjectionMapFromPass(M,M,G,'p0',R);
            5:ValidateSequenceProjectionMapsFromPasses(M,G,B);
            6:RequireSequenceProjectionMapsFromPasses(M,G,B);
            7:ValidateSequencePartialProjectionMapsFromPasses(M,G,B);
            8:RequireSequencePartialProjectionMapsFromPasses(M,G,B);
          end;
        except on E:Exception do begin Rejected:=True;
          Check(Pos('identical pass layouts',E.Message)>0,'sequence layout-specific rejection'); end;
        end;
        Why:='sequence variant'+IntToStr(Variant)+' op'+IntToStr(Operation);
        Check(Rejected=(Variant<>0),Why+' expected acceptance');
        if (Variant<>0) or (Operation in [3,5,7]) then
          CheckUnchanged(G,BeforeValues,BeforeRules,0,Why)
        else Check(G.DependencyCount=N-1,Why+' matching nondefault layout installs');
      finally G.Free; end;
    end;
  finally M.Free; end;
end;

function Kit:TVoxel3DKit;
var P:TVoxel3DPrototypes; Pairs:TVoxel3DSocketPairs;
  S:TVoxel3DSockets; Direction:TGraphDirection;
begin
  for Direction:=Low(TGraphDirection) to High(TGraphDirection) do S[Direction]:='open';
  SetLength(P,1);
  P[0]:=MakeVoxel3DPrototype('block','material',1,S,[v3r0],[v3pfEmpty],[]);
  SetLength(Pairs,1); Pairs[0]:=MakeVoxel3DSocketPair('open','open');
  Result:=TVoxel3DKit.Create('layout-test',P,Pairs);
end;

procedure TestVoxels;
var K:TVoxel3DKit; M:TWfcModel; G:TGraph;
  Source,Target:TVoxel3DGraphAdapter;
  Selector:TVoxel3DPassVariantSelector; Selectors:TVoxel3DPassVariantSelectors;
  Rules:TVoxel3DPassProjectionRules; ModelRules:TVoxel3DModelPassProjectionRules;
  Terms:TVoxel3DPassSpatialTerms; ModelTerms:TVoxel3DModelPassSpatialTerms;
  Clauses:TVoxel3DPassSpatialClauses; ModelClauses:TVoxel3DModelPassSpatialClauses;
  Variant,Operation,BeforeValues,BeforeRules:Integer; Rejected:Boolean; Why:String;
  L:TWfcLatticeLayouts; Boundary:TWfcModelBoundary;
begin
  K:=Kit;
  Selector:=MakeVoxel3DPassVariantSelector('block',[v3r0]);
  SetLength(Selectors,1); Selectors[0]:=Selector;
  SetLength(Rules,1); Rules[0]:=MakeVoxel3DPassProjectionRule(Selector,Selectors);
  SetLength(ModelRules,1); ModelRules[0]:=MakeVoxel3DModelPassProjectionRule(Selector,Tokens);
  SetLength(Terms,1); Terms[0]:=MakeVoxel3DPassSpatialTerm(MakeGraphOffset(0,0,0),Selectors);
  SetLength(ModelTerms,1); ModelTerms[0]:=MakeVoxel3DModelPassSpatialTerm(MakeGraphOffset(0,0,0),Tokens);
  SetLength(Clauses,1); Clauses[0]:=MakeVoxel3DPassSpatialClause(Selector,v3pofWorld,Terms);
  SetLength(ModelClauses,1); ModelClauses[0]:=MakeVoxel3DModelPassSpatialClause(Selector,v3pofWorld,ModelTerms);
  try
    for Variant:=0 to 10 do for Operation:=0 to 7 do
    begin
      L:=Layouts(2,2,2,2,Variant,True); G:=NewGraph(L);
      Source:=nil; Target:=nil; M:=nil;
      try
        if Operation<4 then Source:=K.ApplyToGraph(G.PassGraph[0])
        else begin
          if L[0].Wrap then Boundary:=wmbWrap else Boundary:=wmbOpen;
          M:=LearnModel3D(Tokens,1,1,1,Boundary,wmsNone);
          ApplyModelToGraph(M,G.PassGraph[0]);
        end;
        Target:=K.ApplyToGraph(G);
        BeforeValues:=Length(G.CopyRegisteredValues); BeforeRules:=G.RuleGroups.Count;
        Rejected:=False;
        try
          case Operation of
            0:ValidateVoxel3DProjectionFromPass(K,Target,K,Source,Rules);
            1:RequireVoxel3DProjectionFromPass(K,Target,K,Source,Rules);
            2:ValidateVoxel3DSpatialClausesFromPass(K,Target,K,Source,Clauses);
            3:RequireVoxel3DSpatialClausesFromPass(K,Target,K,Source,Clauses);
            4:ValidateVoxel3DProjectionFromModelPass(K,Target,M,0,ModelRules);
            5:RequireVoxel3DProjectionFromModelPass(K,Target,M,0,ModelRules);
            6:ValidateVoxel3DSpatialClausesFromModelPass(K,Target,M,0,ModelClauses);
            7:RequireVoxel3DSpatialClausesFromModelPass(K,Target,M,0,ModelClauses);
          end;
        except on E:Exception do begin Rejected:=True;
          Check(Pos('identical pass layouts',E.Message)>0,'voxel layout-specific rejection'); end;
        end;
        Why:='voxel variant'+IntToStr(Variant)+' op'+IntToStr(Operation);
        Check(Rejected=(Variant<>0),Why+' expected acceptance');
        if (Variant<>0) or (Operation mod 2=0) then
          CheckUnchanged(G,BeforeValues,BeforeRules,0,Why)
        else Check(G.DependencyCount=1,Why+' matching nondefault layout installs');
      finally Target.Free; Source.Free; M.Free; G.Free; end;
    end;
  finally K.Free; end;
end;

function UnrelatedRootLayouts(const W,H,D,Count:Integer;
  const Wrap:Boolean):TWfcLatticeLayouts;
begin
  Result:=Layouts(W,H,D,Count,0,Wrap);
  Result[0]:=MakeWfcLatticeLayout(1,2,2,MakeWfcLatticeVector(50,60,70),
    MakeWfcLatticeVector(1,1,1),not Wrap);
end;

procedure TestActivePatternBridges;
var M2:TWfcOverlappingModel2D; M3:TWfcOverlappingModel3D;
  G:TGraph; Rank,Operation:Integer;
begin
  M2:=LearnOverlappingModel2D(Tokens,1,1,1,1,wmbWrap,wmsNone);
  M3:=LearnOverlappingModel3D(Tokens,1,1,1,1,1,1,wmbWrap,wmsNone);
  try
    for Rank:=2 to 3 do for Operation:=0 to 1 do
    begin
      if Rank=2 then G:=NewGraph(UnrelatedRootLayouts(2,2,1,3,True))
      else G:=NewGraph(UnrelatedRootLayouts(2,2,3,3,True));
      try
        if Rank=2 then ApplyOverlappingModel2DToGraph(M2,G.PassGraph[1])
        else ApplyOverlappingModel3DToGraph(M3,G.PassGraph[1]);
        Check((G.Dimension.Width=1) and (not G.WrapNeighbors),
          'pattern root defaults remain unrelated');
        if Rank=2 then
        begin
          if Operation=0 then ValidateOverlappingProjectionFromPass2D(M2,G,'p1')
          else ApplyOverlappingProjectionFromPass2D(M2,G,'p1');
        end
        else if Operation=0 then ValidateOverlappingProjectionFromPass3D(M3,G,'p1')
        else ApplyOverlappingProjectionFromPass3D(M3,G,'p1');
        if Operation=0 then CheckUnchanged(G,0,0,0,'active pattern validation')
        else Check(G.DependencyCount=1,'active pattern installs on selected target');
        CheckUnchanged(G.PassGraph[0],0,0,0,'unrelated pattern root');
        Check(G.CurrentPass='p2','pattern bridge preserves selection');
      finally G.Free; end;
    end;
  finally M3.Free; M2.Free; end;
end;

procedure TestActiveSequenceBridges;
var M:TWfcSequenceModel; G:TGraph; B:TWfcSequenceProjectionBindings;
  R:TWfcSequenceProjectionRules; Operation,N,I,BeforeValues,BeforeRules:Integer;
begin
  M:=LearnSequenceModel(Tokens,1,wmbWrap);
  SetLength(R,1); R[0]:=MakeWfcSequenceProjectionRule('A',Tokens);
  try
    for Operation:=0 to 8 do
    begin
      N:=3; if Operation>=5 then N:=4;
      G:=NewGraph(UnrelatedRootLayouts(4,1,1,N,True));
      try
        if Operation=0 then G.PassGraph[1].AddValue('A')
        else for I:=1 to N-2 do ApplySequenceModelToGraph(M,G.PassGraph[I]);
        if Operation in [1,2] then G.AddValue('A')
        else ApplySequenceModelToGraph(M,G);
        SetLength(B,N-2);
        for I:=0 to High(B) do
          B[I]:=MakeWfcSequenceProjectionBinding(M,'p'+IntToStr(I+1),R);
        BeforeValues:=Length(G.CopyRegisteredValues); BeforeRules:=G.RuleGroups.Count;
        case Operation of
          0:RequireSequenceProjectionFromTokenPass(M,G,'p1');
          1:RequireProjectedSequenceFromPass(M,G,'A','p1',Tokens);
          2:RequirePartialProjectedSequenceFromPass(M,G,'A','p1',Tokens);
          3:ValidateSequenceProjectionMapFromPass(M,M,G,'p1',R);
          4:RequireSequenceProjectionMapFromPass(M,M,G,'p1',R);
          5:ValidateSequenceProjectionMapsFromPasses(M,G,B);
          6:RequireSequenceProjectionMapsFromPasses(M,G,B);
          7:ValidateSequencePartialProjectionMapsFromPasses(M,G,B);
          8:RequireSequencePartialProjectionMapsFromPasses(M,G,B);
        end;
        if Operation in [3,5,7] then
          CheckUnchanged(G,BeforeValues,BeforeRules,0,'active sequence validation')
        else Check(G.DependencyCount=N-2,'active sequence target dependency count');
        CheckUnchanged(G.PassGraph[0],0,0,0,'unrelated sequence root');
        Check(G.CurrentPass='p'+IntToStr(N-1),'sequence bridge preserves selection');
      finally G.Free; end;
    end;
  finally M.Free; end;
end;

procedure TestPatternCaptureLayouts;
var M2:TWfcOverlappingModel2D; M3:TWfcOverlappingModel3D;
  G:TGraph; L:TWfcLatticeLayouts; Rank,Variant,X,Y,Z:Integer;
  P2:TWfcPatternGrid2D; P3:TWfcPatternGrid3D;
  T2:TWfcTokenGrid2D; T3:TWfcTokenGrid3D;
  R2:TWfcOverlapping2DValidationReport; R3:TWfcOverlapping3DValidationReport;
  Keys:TGraphValues; Rejected,Valid:Boolean;
begin
  M2:=LearnOverlappingModel2D(Tokens,1,1,1,1,wmbWrap,wmsNone);
  M3:=LearnOverlappingModel3D(Tokens,1,1,1,1,1,1,wmbWrap,wmsNone);
  try
    for Rank:=2 to 3 do for Variant:=0 to 6 do
    begin
      if Rank=2 then L:=UnrelatedRootLayouts(2,2,1,3,True)
      else L:=UnrelatedRootLayouts(2,2,3,3,True);
      case Variant of
        1:Inc(L[1].Origin.X); 2:Inc(L[1].Origin.Y); 3:Inc(L[1].Origin.Z);
        4:Inc(L[1].Pitch.X); 5:Inc(L[1].Pitch.Y); 6:Inc(L[1].Pitch.Z);
      end;
      G:=NewGraph(L);
      try
        if Rank=2 then ApplyOverlappingModel2DToGraph(M2,G.PassGraph[1])
        else ApplyOverlappingModel3DToGraph(M3,G.PassGraph[1]);
        Keys:=G.PassGraph[1].CopyRegisteredValues;
        for Z:=0 to L[1].Cells.Z-1 do for Y:=0 to L[1].Cells.Y-1 do
          for X:=0 to L[1].Cells.X-1 do
          begin
            G.PassGraph[1].Entry[X,Y,Z].Value:=Keys[0];
            G.PassGraph[2].Entry[X,Y,Z].Value:='A';
          end;
        Rejected:=False; Valid:=False;
        try
          if Rank=2 then Valid:=CaptureSolvedOverlappingProjectionPass2D(M2,
            G.PassGraph[1],G.PassGraph[2],P2,T2,R2)
          else Valid:=CaptureSolvedOverlappingProjectionPass3D(M3,
            G.PassGraph[1],G.PassGraph[2],P3,T3,R3);
        except on E:Exception do begin Rejected:=True;
          Check(Pos('identical pass layouts',E.Message)>0,
            'pattern capture rejects misaligned world layouts'); end;
        end;
        Check(Rejected=(Variant<>0),'pattern capture layout acceptance');
        if Variant=0 then
        begin
          Check(Valid,'pattern capture accepts matching concrete nondefault passes');
          if Rank=2 then Check((T2.Width=2) and (T2.Height=2) and
            (Length(T2.Tokens)=4),'pattern capture selected plane extent')
          else Check((T3.Width=2) and (T3.Height=2) and (T3.Depth=3) and
            (Length(T3.Tokens)=12),'pattern capture selected volume extent');
        end
        else if Rank=2 then Check((Length(P2.Patterns)=0) and
          (Length(T2.Tokens)=0),'rejected plane capture has no public output')
        else Check((Length(P3.Patterns)=0) and (Length(T3.Tokens)=0),
          'rejected volume capture has no public output');
        CheckUnchanged(G.PassGraph[0],0,0,0,'pattern capture leaves unrelated root');
        Check(G.CurrentPass='p2','pattern capture preserves selection');
      finally G.Free; end;
    end;
  finally M3.Free; M2.Free; end;
end;

procedure TestActiveSequenceAPIs;
var M:TWfcSequenceModel; G:TGraph; Mode,I,FalsePosition:Integer;
  C:TWfcSequenceTokenConstraints; Keys,Before:TGraphValues;
  States:TWfcSequenceStateIndices; S:TWfcGeneratedSequence;
  Segment:TWfcGeneratedSequenceSegment; Report:TWfcSequenceGraphValidationReport;
  Boundary:TWfcSequenceSegmentBoundary; Extent:TWfcSequenceExtent; Rejected:Boolean;
begin
  for Mode:=0 to 4 do
  begin
    if Mode=0 then M:=LearnSequenceModel(Tokens,1,wmbWrap)
    else M:=LearnSequenceModel(Tokens,1,wmbOpen);
    G:=NewGraph(UnrelatedRootLayouts(4,1,1,2,Mode=0));
    try
      if Mode=0 then Extent:=wseWrap
      else if Mode=1 then Extent:=wseFragment else Extent:=wseWhole;
      Boundary:=MakeWfcSequenceInitialSegmentBoundary(True);
      if Mode=4 then Boundary:=MakeWfcSequenceContinuingSegmentBoundary(0,True);
      case Mode of
        0,2:ApplySequenceModelToGraph(M,G);
        1:ApplySequenceModelToGraph(M,G,Extent);
        3,4:ApplySequenceModelSegmentToGraph(M,G,Boundary);
      end;
      Keys:=G.CopyRegisteredValues;
      Check(Length(Keys)=1,'active sequence model applied to selected pass');
      IntersectSequenceAllowedTokens(M,G,3,Tokens);
      IntersectSequenceAllowedTokens(M,G,2,TWfcModelToken('A'));
      SetLength(C,1); C[0].Position:=1; C[0].AllowedTokens:=Tokens;
      IntersectSequenceTokenConstraints(M,G,C);
      IntersectSequenceLockedSpan(M,G,2,Tokens);
      IntersectSequencePrefix(M,G,Tokens);
      IntersectSequenceSuffix(M,G,Tokens);
      SetLength(States,4);
      for I:=0 to 3 do
      begin
        States[I]:=0;
        Check(G.HasAllowedValues(I,0,0),'active sequence domain exists');
        Check(SequenceStateSatisfiesEntryConstraints(M,G,I,0),
          'active single entry validation');
      end;
      Check(SequenceStatesSatisfyEntryConstraints(M,G,States,FalsePosition) and
        (FalsePosition=-1),'active complete path validation');
      Before:=G.CopyAllowedValues(3,0,0);
      Rejected:=False;
      try IntersectSequenceLockedSpan(M,G,4,Tokens);
      except on E:ERangeError do Rejected:=True; end;
      Check(Rejected,'active width rejects an out-of-range span');
      Check((Length(G.CopyAllowedValues(3,0,0))=Length(Before)) and
        (G.CopyAllowedValues(3,0,0)[0]=Before[0]),'rejected active span leaves domains');
      if Mode<3 then
        Check(not CaptureSolvedSequence(M,G,Extent,S,Report) and
          (Report.Issue.Kind=wsgikEmptyCell) and (Length(S.Tokens)=0),
          'active empty capture reports cell rather than unrelated shape')
      else Check(not CaptureSolvedSequenceSegment(M,G,Boundary,Segment,Report) and
        (Report.Issue.Kind=wsgikEmptyCell) and (Length(Segment.Tokens)=0),
        'active empty segment capture');
      { Capture validates independently: supply exact model-qualified keys,
        not a second solver, and assert its full selected-pass extent. }
      for I:=0 to 3 do G.Entry[I,0,0].Value:=Keys[0];
      if Mode in [0,2] then
        Check(CaptureSolvedSequence(M,G,S,Report) and (S.Extent=Extent) and
          (Length(S.Tokens)=4),'active implicit capture topology and extent')
      else if Mode=1 then
        Check(CaptureSolvedSequence(M,G,Extent,S,Report) and
          (Length(S.Tokens)=4),'active explicit fragment capture')
      else Check(CaptureSolvedSequenceSegment(M,G,Boundary,Segment,Report) and
        (Length(Segment.Tokens)=4),'active segment capture');
      G.Entry[3,0,0].Value:='not-a-private-state';
      Check(not SequenceStateSatisfiesEntryConstraints(M,G,3,0),
        'active entry lock mismatch detected');
      Check(not SequenceStatesSatisfyEntryConstraints(M,G,States,FalsePosition) and
        (FalsePosition=3),'active path lock mismatch position');
      CheckUnchanged(G.PassGraph[0],0,0,0,'sequence APIs leave root definitions');
      Check(not G.PassGraph[0].HasAllowedValues(0,0,0),'sequence APIs leave root domains');
      Check(G.PassGraph[0].Entry[0,0,0].Empty,'sequence APIs leave root entries');
      Check(G.CurrentPass='p1','sequence APIs preserve selection');
    finally G.Free; M.Free; end;
  end;
end;

procedure TestActiveVoxelBridges;
var K:TVoxel3DKit; M:TWfcModel; G:TGraph; Source,Target:TVoxel3DGraphAdapter;
  Selector:TVoxel3DPassVariantSelector; Selectors:TVoxel3DPassVariantSelectors;
  Rules:TVoxel3DPassProjectionRules; ModelRules:TVoxel3DModelPassProjectionRules;
  Terms:TVoxel3DPassSpatialTerms; ModelTerms:TVoxel3DModelPassSpatialTerms;
  Clauses:TVoxel3DPassSpatialClauses; ModelClauses:TVoxel3DModelPassSpatialClauses;
  Operation,BeforeValues,BeforeRules:Integer;
begin
  K:=Kit; M:=LearnModel3D(Tokens,1,1,1,wmbWrap,wmsNone);
  Selector:=MakeVoxel3DPassVariantSelector('block',[v3r0]);
  SetLength(Selectors,1); Selectors[0]:=Selector;
  SetLength(Rules,1); Rules[0]:=MakeVoxel3DPassProjectionRule(Selector,Selectors);
  SetLength(ModelRules,1); ModelRules[0]:=MakeVoxel3DModelPassProjectionRule(Selector,Tokens);
  SetLength(Terms,1); Terms[0]:=MakeVoxel3DPassSpatialTerm(MakeGraphOffset(0,0,0),Selectors);
  SetLength(ModelTerms,1); ModelTerms[0]:=MakeVoxel3DModelPassSpatialTerm(MakeGraphOffset(0,0,0),Tokens);
  SetLength(Clauses,1); Clauses[0]:=MakeVoxel3DPassSpatialClause(Selector,v3pofWorld,Terms);
  SetLength(ModelClauses,1); ModelClauses[0]:=MakeVoxel3DModelPassSpatialClause(Selector,v3pofWorld,ModelTerms);
  try
    for Operation:=0 to 7 do
    begin
      G:=NewGraph(UnrelatedRootLayouts(2,2,3,3,True)); Source:=nil; Target:=nil;
      try
        if Operation<4 then Source:=K.ApplyToGraph(G.PassGraph[1])
        else ApplyModelToGraph(M,G.PassGraph[1]);
        Target:=K.ApplyToGraph(G);
        Check(Target.AppliedGraph=G.PassGraph[2],'voxel adapter binds active concrete pass');
        BeforeValues:=Length(G.CopyRegisteredValues); BeforeRules:=G.RuleGroups.Count;
        case Operation of
          0:ValidateVoxel3DProjectionFromPass(K,Target,K,Source,Rules);
          1:RequireVoxel3DProjectionFromPass(K,Target,K,Source,Rules);
          2:ValidateVoxel3DSpatialClausesFromPass(K,Target,K,Source,Clauses);
          3:RequireVoxel3DSpatialClausesFromPass(K,Target,K,Source,Clauses);
          4:ValidateVoxel3DProjectionFromModelPass(K,Target,M,1,ModelRules);
          5:RequireVoxel3DProjectionFromModelPass(K,Target,M,1,ModelRules);
          6:ValidateVoxel3DSpatialClausesFromModelPass(K,Target,M,1,ModelClauses);
          7:RequireVoxel3DSpatialClausesFromModelPass(K,Target,M,1,ModelClauses);
        end;
        if Operation mod 2=0 then
          CheckUnchanged(G,BeforeValues,BeforeRules,0,'active voxel validation')
        else Check(G.DependencyCount=1,'active voxel bridge installs');
        CheckUnchanged(G.PassGraph[0],0,0,0,'unrelated voxel root');
        Check(G.CurrentPass='p2','voxel bridge preserves selection');
      finally Target.Free; Source.Free; G.Free; end;
    end;
  finally M.Free; K.Free; end;
end;

begin
  try TestPatterns; TestSequences; TestVoxels;
    TestActivePatternBridges; TestActiveSequenceBridges; TestActiveSequenceAPIs;
    TestPatternCaptureLayouts;
    TestActiveVoxelBridges;
  except on E:Exception do begin Inc(Failures); WriteLn('[EXCEPTION] ',E.ClassName,': ',E.Message); end; end;
  WriteLn('Bridge layouts: ',Checks,' checks, ',Failures,' failures');
  if Failures<>0 then Halt(1);
end.
