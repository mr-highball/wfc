{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_pattern3d_passes_test;

{$mode delphi}{$H+}

uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  Classes, SysUtils, wfc, wfc_model, wfc_learn3d, wfc_pattern3d,
  wfc_pattern3d_learn, wfc_pattern3d_graph;

type
  TTest = procedure;
  TRejectingPipeline = class(TWfcPattern3DPassPipeline)
  public
    RejectNext, RejectAtSeam, ProbeReentry: Boolean;
  protected
    function DoValidateProjection(const APatterns: TWfcPatternGrid3D;
      const AProjection: TWfcTokenGrid3D;
      out AReport: TWfcOverlapping3DValidationReport): Boolean; override;
  end;
var Checks, Failures: Integer;

procedure Check(const AOK: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not AOK then begin Inc(Failures); WriteLn('[FAIL] ', AMessage); end;
end;

procedure Run(const AName: String; const ATest: TTest);
begin
  WriteLn('[TEST] ', AName);
  try ATest;
  except on E: Exception do
    begin Inc(Failures); WriteLn('[EXCEPTION] ', E.ClassName, ': ', E.Message); end;
  end;
end;

function Tokens(const A: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(A));
  for I := 0 to High(A) do Result[I] := A[I];
end;

function GraphValue(const AToken: TWfcModelToken): TGraphValue;
begin
  {$IFDEF PAS2JS}Result := AToken;
  {$ELSE}Result := TGraphValue(UTF8Decode(AToken));{$ENDIF}
end;

function Checker: TWfcOverlappingModel3D;
begin
  Result := LearnOverlappingModel3D(Tokens(['A','B','B','A','B','A','A','B']),
    2,2,2, 2,2,2, wmbWrap,wmsNone);
end;

function SourceAndTarget(const M: TWfcOverlappingModel3D;
  const W,H,D: Integer; const Wrap: Boolean = True): TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Reshape(W,H,D); Result.WrapNeighbors := Wrap;
    Result.Seed := 246813579; Result.CurrentPass := 'patterns';
    Result.PassMode := gpmOverlay; Result.ClearDependencies;
    ApplyOverlappingModel3DToGraph(M, Result);
    Result.SwitchToPass('terrain'); Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
  except Result.Free; raise; end;
end;

procedure RejectedUnchanged(const M: TWfcOverlappingModel3D;
  const G: TGraph; const Source, Why: String);
var Raised: Boolean; BeforePass, BeforeDeps, BeforeRules, BeforeValues: Integer;
begin
  BeforePass := G.CurrentPassIndex; BeforeDeps := G.DependencyCount;
  BeforeRules := G.RuleGroups.Count;
  BeforeValues := Length(G.CopyRegisteredValues);
  Raised := False;
  try ApplyOverlappingProjectionFromPass3D(M,G,Source);
  except on E: Exception do begin Raised := True;
    Check(Pos('@p3v1',E.Message)=0, 'preflight does not leak private keys'); end; end;
  Check(Raised and (G.CurrentPassIndex=BeforePass) and
    (G.DependencyCount=BeforeDeps) and (G.RuleGroups.Count=BeforeRules) and
    (Length(G.CopyRegisteredValues)=BeforeValues), Why);
end;

procedure TestPreflight;
var M, Wrong, Gap, Reserved: TWfcOverlappingModel3D; G: TGraph;
begin
  M := Checker;
  Wrong := LearnOverlappingModel3D(Tokens(['C','D','D','C','D','C','C','D']),
    2,2,2,2,2,2,wmbWrap,wmsNone);
  Gap := LearnOverlappingModel3D(Tokens(['A','B','C','D','E','F','G','H']),
    2,2,2,2,2,2,wmbOpen,wmsNone);
  Reserved := LearnOverlappingModel3D(Tokens(['@p3v1;reserved']),
    1,1,1,1,1,1,wmbOpen,wmsNone);
  try
    G := SourceAndTarget(M,4,2,4);
    try
      ValidateOverlappingProjectionFromPass3D(M,G,'patterns');
      Check((not G.HasDefinition) and (G.DependencyCount=0) and
        (G.CurrentPassIndex=1), 'successful volume preflight is read-only');
      RejectedUnchanged(Wrong,G,'patterns','same adjacency/different payload is rejected');
      RejectedUnchanged(M,G,'missing','missing source rejected');
      RejectedUnchanged(M,G,'terrain','self dependency rejected');
      ApplyOverlappingProjectionFromPass3D(M,G,'patterns');
      Check(G.HasDefinition and (G.DependencyCount=1), 'full volume projection applied');
      RejectedUnchanged(M,G,'patterns','nonempty target rejected');
    finally G.Free; end;
    G := SourceAndTarget(M,2,2,2,False);
    try RejectedUnchanged(M,G,'patterns','open pass bridge explicitly rejected');
    finally G.Free; end;
    G := SourceAndTarget(M,2,2,2);
    try
      G.PassGraph[0].DependsOn('terrain');
      RejectedUnchanged(M,G,'patterns','dependency cycle rejected atomically');
    finally G.Free; end;
    G := SourceAndTarget(Reserved,1,1,1);
    try RejectedUnchanged(Reserved,G,'patterns','reserved public prefix rejected');
    finally G.Free; end;
    G := SourceAndTarget(Gap,2,2,2);
    try
      ApplyOverlappingProjectionFromPass3D(Gap,G,'patterns');
      Check(G.HasDefinition and (Length(G.CopyRegisteredValues)=8),
        'unsatisfiable wrapped palette coverage is modeled, not an adapter error');
    finally G.Free; end;
  finally Reserved.Free; Gap.Free; Wrong.Free; M.Free; end;
end;

procedure TestVolumeAndConsumer;
var M: TWfcOverlappingModel3D; G, Other: TGraph; O: TGraphSolveOptions;
  S: TGraphSolveReport; V: TWfcOverlapping3DValidationReport;
  P: TWfcPatternGrid3D; T: TWfcTokenGrid3D;
  X,Y,Z,PX,PY,PZ,I,J, Selected: Integer; Expected: TGraphValue; Raised: Boolean;
begin
  M := Checker; G := SourceAndTarget(M,4,2,4); Other := nil;
  try
    ApplyOverlappingProjectionFromPass3D(M,G,'patterns');
    G.SwitchToPass('foliage'); G.PassMode := gpmOverlay; G.ClearDependencies;
    G.AddValue('tree'); G.AddValue('reed');
    G.Rules['tree'].RequireFromPass('terrain','A');
    G.Rules['reed'].RequireFromPass('terrain','B');
    O := DefaultGraphSolveOptions;
    Check(G.TrySolve(O,S), 'patterns -> terrain -> foliage solves full XYZ volume');
    Selected := G.CurrentPassIndex;
    Check(CaptureSolvedOverlappingProjectionPass3D(M,G.PassGraph[0],G.PassGraph[1],P,T,V),
      'exact full-volume capture succeeds while another pass is selected');
    Check((G.CurrentPassIndex=Selected) and (T.Width=4) and (T.Height=2) and
      (T.Depth=4) and (Length(T.Tokens)=32), 'capture does not change pass selection');
    Check(V.CheckedProjectionCells=256, 'capture counts every XYZ footprint contribution');
    for Z := 0 to 3 do for Y := 0 to 1 do for X := 0 to 3 do
    begin
      I := (Z*2+Y)*4+X;
      if T.Tokens[I]='A' then Expected := 'tree' else Expected := 'reed';
      Check(G.PassGraph[2].Entry[X,Y,Z].Value=Expected,
        'downstream consumer reads public semantic tokens at nonzero Z');
      for PZ := 0 to 1 do for PY := 0 to 1 do for PX := 0 to 1 do
      begin
        J := ((((Z+PZ) mod 4)*2+(Y+PY) mod 2)*4+(X+PX) mod 4);
        Check(T.Tokens[J]=M.PaletteTokenAt(M.PatternPaletteIndexAt(P.Patterns[I],PX,PY,PZ)),
          'independent XYZ seam contribution agrees');
      end;
    end;
    G.PassGraph[1].Entry[3,1,3].Value := 'bad-token';
    Check(not CaptureSolvedOverlappingProjectionPass3D(M,G.PassGraph[0],G.PassGraph[1],P,T,V),
      'nonzero-Z public corruption rejected');
    Check((Length(P.Patterns)=0) and (Length(T.Tokens)=0) and (V.Issue.Value=''),
      'failed public capture publishes no partial output/private key');
    Other := SourceAndTarget(M,4,2,4);
    Raised := False;
    try CaptureSolvedOverlappingProjectionPass3D(M,G.PassGraph[0],Other.PassGraph[1],P,T,V);
    except on E: Exception do Raised := True; end;
    Check(Raised, 'cross-root capture rejected');
  finally Other.Free; G.Free; M.Free; end;
end;

procedure TestPublicDomains;
var M: TWfcOverlappingModel3D; P: TWfcPattern3DPassPipeline;
  C, Old, CopyC: TWfcPattern3DComposition; R: TWfcPattern3DPassReport;
  V: TWfcOverlapping3DValidationReport; T, Domain: TWfcModelTokens;
  Seed, I: Integer; Raised: Boolean; O: TGraphSolveOptions;
begin
  M := Checker; P := TWfcPattern3DPassPipeline.Create(DefaultWfcPattern3DPassConfig(M,4,2,4,0));
  C := nil; Old := nil; CopyC := nil;
  try
    Check(not P.TryCopyCommitted(C,V), 'no output before baseline');
    Domain := Tokens(['B','A','B']); P.SetPublicDomain(3,1,3,Domain);
    Domain[0] := 'bad'; Domain := P.CopyPublicDomain(3,1,3);
    Check((Length(Domain)=2) and (Domain[0]='A') and (Domain[1]='B'),
      'public domain is detached, deduplicated, palette ordered');
    Domain[0] := 'bad'; Domain := P.CopyPublicDomain(3,1,3);
    Check(Domain[0]='A', 'copied domain cannot mutate stored constraints');
    P.LockPublicCell(3,1,3,'A');
    O := DefaultGraphSolveOptions; O.MaxBacktracks := 0;
    for Seed := 0 to 15 do
    begin
      P.Seed := Seed;
      Check(P.TryGenerate(O,C,R), 'inverse lock solves without local backtracking for every seed');
      if Assigned(C) then
      begin
        T := C.CopyProjection.Tokens;
        for I := 0 to High(T) do
          Check((T[I]='A') = (((I mod 4+(I div 4) mod 2+I div 8) mod 2)=1),
            'XYZ checker phase is forced by public cell at 3,1,3');
        Check(P.Validate(C,V), 'locked composition independently validates');
      end;
      C.Free; C := nil;
    end;
    Check(P.TryGenerate(Old,R), 'baseline under valid lock');
    Raised := False;
    try P.LockPublicCell(0,0,0,'unknown'); except on E: Exception do Raised := True; end;
    Check(Raised and P.TryCopyCommitted(CopyC,V), 'invalid edit leaves prior state untouched');
    CopyC.Free; CopyC := nil;
    P.LockPublicCell(2,1,3,'A');
    Check((not P.TryCopyCommitted(CopyC,V)) and (not Assigned(CopyC)),
      'successful edit invalidates current committed availability');
    Check((not P.TryGenerate(C,R)) and (C=nil) and (R.Status=wp3psSolveFailed),
      'conflicting adjacent public locks contradict');
    Check(not P.Validate(Old,V), 'old composition is not valid under changed public domains');
    P.ClearPublicDomain(2,1,3);
    Check(P.TryGenerate(C,R), 'clearing conflict recovers generation'); C.Free; C := nil;
    P.SetPublicDomain(0,0,0,nil);
    Check(P.HasPublicDomain(0,0,0) and (Length(P.CopyPublicDomain(0,0,0))=0),
      'assigned empty domain differs from unconstrained');
    Check((not P.TryGenerate(C,R)) and (C=nil), 'empty public domain is hard contradiction');
    P.ClearPublicDomains;
    Check((not P.HasPublicDomain(0,0,0)) and (not P.HasPublicDomain(3,1,3)) and
      P.TryGenerate(C,R), 'clear-all removes latent and public constraints');
    C.Free; C := nil;
    P.Seed := 77;
    Check(not P.TryCopyCommitted(C,V), 'seed edit invalidates current result');
    Raised := False;
    try P.LockPublicCell(0,0,4,'A'); except on E: Exception do Raised := True; end;
    Check(Raised, 'nonexistent Z rejects before mutation');
  finally CopyC.Free; Old.Free; C.Free; P.Free; M.Free; end;
end;

procedure TestAliasesAndCoverage;
var Samples: TWfcLearnVolumeSamples; M: TWfcOverlappingModel3D;
  P: TWfcPattern3DPassPipeline; C: TWfcPattern3DComposition;
  R: TWfcPattern3DPassReport; T: TWfcModelTokens; Axis,I: Integer;
begin
  for Axis := 0 to 2 do
  begin
    SetLength(Samples,2);
    Samples[0].Width:=1; Samples[0].Height:=1; Samples[0].Depth:=1;
    case Axis of 0: Samples[0].Width:=3; 1: Samples[0].Height:=3; 2: Samples[0].Depth:=3; end;
    Samples[0].Tokens := Tokens(['A','A','A']);
    Samples[1] := Samples[0]; Samples[1].Tokens := Tokens(['B','C','D']);
    M := LearnOverlappingModel3DCorpus(Samples,Samples[0].Width,Samples[0].Height,
      Samples[0].Depth,wmbOpen,wmsNone);
    P := TWfcPattern3DPassPipeline.Create(DefaultWfcPattern3DPassConfig(M,1,1,1,4)); C := nil;
    try
      Check(P.TryGenerate(C,R), 'unusable public tokens do not forbid the feasible wrapped subset');
      if Assigned(C) then begin T:=C.CopyProjection.Tokens; Check(T[0]='A','self-aliasing chooses uniform pattern'); end;
      C.Free; C:=nil;
      P.LockPublicCell(0,0,0,'B');
      Check((not P.TryGenerate(C,R)) and (C=nil), 'all aliased offsets constrain the same latent anchor');
    finally C.Free; P.Free; M.Free; end;
  end;
  { Both ABA and BAB are required to tile the alternating two-cell torus. }
  M := LearnOverlappingModel3D(Tokens(['A','B','A','B','A','A']),1,1,6,1,1,3,wmbWrap,wmsNone);
  P := TWfcPattern3DPassPipeline.Create(DefaultWfcPattern3DPassConfig(M,1,1,2,4)); C := nil;
  try
    P.LockPublicCell(0,0,1,'B');
    Check(P.TryGenerate(C,R), 'oversized Z footprint supports a feasible nonuniform two-cell torus');
    if Assigned(C) then
    begin
      T:=C.CopyProjection.Tokens; Check((T[0]='A') and (T[1]='B'), 'Z aliases agree at every offset');
      for I:=0 to 1 do Check(C.CopyPatternGrid.Patterns[I]>=0,'all latent anchors captured');
    end;
  finally C.Free; P.Free; M.Free; end;
end;

function TRejectingPipeline.DoValidateProjection(const APatterns: TWfcPatternGrid3D;
  const AProjection: TWfcTokenGrid3D;
  out AReport: TWfcOverlapping3DValidationReport): Boolean;
var I: Integer; Raised: Boolean; C: TWfcPattern3DComposition;
  R: TWfcPattern3DPassReport; V: TWfcOverlapping3DValidationReport;
begin
  Result := inherited DoValidateProjection(APatterns,AProjection,AReport);
  if Result and ProbeReentry then
  begin
    ProbeReentry:=False;
    for I:=0 to 7 do
    begin
      Raised:=False; C:=nil;
      try
        case I of
          0: Seed:=99;
          1: LockPublicCell(0,0,0,'A');
          2: SetPublicDomain(0,0,0,nil);
          3: ClearPublicDomain(0,0,0);
          4: ClearPublicDomains;
          5: TryGenerate(C,R);
          6: TryCopyCommitted(C,V);
          7: Validate(nil,V);
        end;
      except on E: EInvalidOperation do Raised:=True; end;
      C.Free;
      Check(Raised,'owner blocks mutations/reentrant output operations during commit validation');
    end;
  end;
  if Result and RejectNext then
  begin
    RejectNext := False; Result := False; AReport.Valid := False;
    AReport.Issue.Kind := wo3ikProjectionToken;
    AReport.Issue.X := Width-1; AReport.Issue.Y := Height-1; AReport.Issue.Z := Depth-1;
    if RejectAtSeam then
    begin
      AReport.Issue.PatternOffsetX:=1; AReport.Issue.PatternOffsetY:=1;
      AReport.Issue.PatternOffsetZ:=1;
    end;
  end;
end;

procedure TestTransactions;
var M: TWfcOverlappingModel3D; A,B: TRejectingPipeline;
  C,D,E,F: TWfcPattern3DComposition; R: TWfcPattern3DPassReport;
  V: TWfcOverlapping3DValidationReport; T: TWfcTokenGrid3D; P: TWfcPatternGrid3D;
  Signature: Cardinal;
  Raised: Boolean;
begin
  M := Checker;
  A := TRejectingPipeline.Create(DefaultWfcPattern3DPassConfig(M,4,2,4,246813579));
  B := TRejectingPipeline.Create(DefaultWfcPattern3DPassConfig(M,4,2,4,246813579));
  C:=nil; D:=nil; E:=nil; F:=nil;
  try
    Check(A.TryGenerate(C,R) and B.TryGenerate(D,R), 'two identical owner baselines solve');
    if not Assigned(C) or not Assigned(D) then Exit;
    Check(C.Signature=D.Signature,'volume signature replays exactly');
    Signature:=C.Signature;
    WriteLn('[INFO] XYZ composition signature ',WfcPattern3DCompositionSignatureHex(Signature));
    Check(WfcPattern3DCompositionSignatureHex(Signature)='F7EBCEFF',
      'fixed full-XYZ cross-runtime signature golden');
    T:=C.CopyProjection; T.Tokens[0]:='tampered';
    P:=C.CopyPatternGrid; P.Patterns[0]:=-1;
    Check(A.Validate(C,V) and (C.Signature=Signature), 'composition copies fully detached');
    T:=C.CopyProjection; P:=C.CopyPatternGrid; Inc(T.Depth);
    Raised:=False;
    try CalculateWfcPattern3DCompositionSignature(M,C.Seed,P,T);
    except on E: Exception do Raised:=True; end;
    Check(Raised, 'signature rejects malformed output depth');
    A.RejectNext:=True; A.RejectAtSeam:=True; A.ProbeReentry:=True;
    Check((not A.TryGenerate(E,R)) and (E=nil) and (R.Status=wp3psValidationFailed) and
      (R.FailedLayer=wp3lProjection) and (R.Solve.Contradiction.Kind=gckFinalValidation) and
      (R.Solve.Contradiction.EntryIndex=0), 'XYZ seam rejection maps to wrapped public entry zero');
    Check(A.TryCopyCommitted(E,V) and (E.Signature=C.Signature),
      'failed unchanged-config attempt restores previous committed graph');
    E.Free; E:=nil;
    Check(A.TryGenerate(E,R) and B.TryGenerate(F,R) and (E.Signature=F.Signature),
      'rollback restores all pass random streams for exact retry');
    E.Free; E:=nil; F.Free; F:=nil;
    A.RejectNext:=True; A.RejectAtSeam:=False;
    Check((not A.TryGenerate(E,R)) and (R.Solve.Contradiction.EntryIndex=31),
      'custom validator without offsets reports XYZ public entry directly');
  finally F.Free; E.Free; D.Free; C.Free; B.Free; A.Free; M.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestHostileJS;
var M: TWfcOverlappingModel3D; Owner, InvalidOwner: TWfcPattern3DPassPipeline;
  Config: TWfcPattern3DPassConfig; C, CopyC: TWfcPattern3DComposition;
  R: TWfcPattern3DPassReport; V: TWfcOverlapping3DValidationReport;
  P, BadP: TWfcPatternGrid3D; T, BadT: TWfcTokenGrid3D;
  BadInteger, I, Operation: Integer; BadSeed: TGraphSeed;
  Raised: Boolean; OriginalSignature: Cardinal;
begin
  M:=Checker;
  Owner:=TWfcPattern3DPassPipeline.Create(DefaultWfcPattern3DPassConfig(M,2,2,2,4));
  C:=nil; CopyC:=nil;
  try
    Check(Owner.TryGenerate(C,R),'typed-JS fixture baseline');
    if not Assigned(C) then Exit;
    OriginalSignature:=C.Signature; P:=C.CopyPatternGrid; T:=C.CopyProjection;
    for I:=0 to 11 do
    begin
      asm
        BadInteger = [NaN, Infinity, -Infinity, 0.5, '1', null,
          undefined, true, {}, [], -1, 4294967296][I];
        BadSeed = BadInteger;
      end;
      for Operation:=0 to 9 do
      begin
        Raised:=False; InvalidOwner:=nil;
        Config:=DefaultWfcPattern3DPassConfig(M,2,2,2,4);
        BadP:=C.CopyPatternGrid; BadT:=C.CopyProjection;
        try
          case Operation of
            0: Owner.Seed:=BadSeed;
            1: begin Config.Seed:=BadSeed; InvalidOwner:=TWfcPattern3DPassPipeline.Create(Config); end;
            2: begin Config.Depth:=BadInteger; InvalidOwner:=TWfcPattern3DPassPipeline.Create(Config); end;
            3: Owner.LockPublicCell(0,0,BadInteger,'A');
            4: Owner.HasPublicDomain(BadInteger,0,0);
            5: WfcPattern3DCompositionSignatureHex(BadSeed);
            6: CalculateWfcPattern3DCompositionSignature(M,BadSeed,P,T);
            7: begin BadP.Depth:=BadInteger; CalculateWfcPattern3DCompositionSignature(M,4,BadP,T); end;
            8: begin BadT.Width:=BadInteger; CalculateWfcPattern3DCompositionSignature(M,4,P,BadT); end;
            9: begin BadP.Patterns[0]:=BadInteger; CalculateWfcPattern3DCompositionSignature(M,4,BadP,T); end;
          end;
        except on E: Exception do Raised:=True; end;
        InvalidOwner.Free;
        Check(Raised,'malformed typed-JS owner/identity field rejected');
        Check(Owner.TryCopyCommitted(CopyC,V) and (CopyC.Signature=OriginalSignature),
          'malformed typed-JS call leaves committed identity available');
        CopyC.Free; CopyC:=nil;
      end;
    end;
  finally CopyC.Free; C.Free; Owner.Free; M.Free; end;
end;
{$ENDIF}

begin
  WriteLn('WFC overlapping volume pass conformance suite');
  Run('atomic XYZ adapter preflight',TestPreflight);
  Run('public volume projection and semantic consumer',TestVolumeAndConsumer);
  Run('inverse public domains and stale-result handling',TestPublicDomains);
  Run('tiny XYZ aliasing and partial palette coverage',TestAliasesAndCoverage);
  Run('transaction rollback and replay identity',TestTransactions);
  {$IFDEF PAS2JS}Run('hostile typed-JS public API',TestHostileJS);{$ENDIF}
  WriteLn('Checks: ',Checks,'  Failures: ',Failures);
  if Failures<>0 then
  begin
    {$IFDEF PAS2JS}raise Exception.Create('volume pass checks failed');
    {$ELSE}Halt(1);{$ENDIF}
  end;
end.
