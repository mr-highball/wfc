{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_pattern3d_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc_model, wfc_learn3d, wfc_pattern3d, wfc_pattern3d_learn,
  wfc_pattern3d_text;

type
  TOracle = record
    Palette: TWfcModelTokens;
    Patterns: TWfcPattern3DPayloads;
    Weights: TWfcModelIntegerArray;
  end;

var Checks: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;

function TokenList(const AValues: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

function UniqueSample(const W, H, D: Integer; const Prefix: String): TWfcLearnVolumeSample;
var I: Integer;
begin
  Result.Width := W; Result.Height := H; Result.Depth := D;
  SetLength(Result.Tokens, W * H * D);
  for I := 0 to High(Result.Tokens) do Result.Tokens[I] := TWfcModelToken(Prefix + IntToStr(I));
end;

function TokenIndex(const A: TWfcModelTokens; const T: TWfcModelToken): Integer;
var I: Integer;
begin
  for I := 0 to High(A) do if A[I] = T then Exit(I);
  Result := -1;
end;

function EqualPayload(const A, B: TWfcPattern3DPayload): Boolean;
var I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do if A[I] <> B[I] then Exit(False);
  Result := True;
end;

function PatternIndex(const A: TWfcPattern3DPayloads; const P: TWfcPattern3DPayload): Integer;
var I: Integer;
begin
  for I := 0 to High(A) do if EqualPayload(A[I], P) then Exit(I);
  Result := -1;
end;

procedure Observe(var A: TOracle; const P: TWfcPattern3DPayload);
var I, N: Integer;
begin
  N := PatternIndex(A.Patterns, P);
  if N < 0 then
  begin
    N := Length(A.Patterns); SetLength(A.Patterns, N + 1);
    SetLength(A.Patterns[N], Length(P));
    for I := 0 to High(P) do A.Patterns[N][I] := P[I];
    SetLength(A.Weights, N + 1); A.Weights[N] := 0;
  end;
  Inc(A.Weights[N]);
end;

{ Independent literal oracle: scatter each ORIGINAL coordinate into a new
  transformed array. It does not call any production symmetry, overlap,
  interning, or extraction helper. The production helper instead gathers
  destination coordinates through the inverse mapping. The signed axis
  order below is the published version-1 contract, not discovered output. }
function Oracle(const Samples: TWfcLearnVolumeSamples; const PW, PH, PD: Integer;
  const Boundary: TWfcModelBoundary; const Symmetry: TWfcModelSymmetry): TOracle;
const Axes: array[0..5,0..2] of Integer =
  ((0,1,2),(0,2,1),(1,0,2),(1,2,0),(2,0,1),(2,1,0));
  Orientation: array[0..5] of Integer = (1,-1,-1,1,1,-1);
var S, I, P, M, Signs, A, X, Y, Z, PX, PY, PZ, SX, SY, SZ, LX, LY, LZ: Integer;
  Dimensions, Position, OutDimensions, OutPosition: array[0..2] of Integer;
  V: TWfcModelIntegerArray; Payload: TWfcPattern3DPayload;
begin
  Result := Default(TOracle);
  for S := 0 to High(Samples) do for I := 0 to High(Samples[S].Tokens) do
    if TokenIndex(Result.Palette, Samples[S].Tokens[I]) < 0 then
    begin
      A := Length(Result.Palette); SetLength(Result.Palette, A + 1);
      Result.Palette[A] := Samples[S].Tokens[I];
    end;
  SetLength(Payload, PW * PH * PD);
  for S := 0 to High(Samples) do
  begin
    Dimensions[0] := Samples[S].Width; Dimensions[1] := Samples[S].Height;
    Dimensions[2] := Samples[S].Depth;
    for P := 0 to 5 do for M := 0 to 7 do
    begin
      case Symmetry of
        wmsNone: if (P <> 0) or (M <> 0) then Continue;
        wmsD4: if ((P <> 0) and (P <> 2)) or (M > 3) then Continue;
        wmsCubeRotations:
          begin
            Signs := 1;
            if M in [1,2,4,7] then Signs := -1;
            if Signs * Orientation[P] <> 1 then Continue;
          end;
      end;
      for A := 0 to 2 do OutDimensions[A] := Dimensions[Axes[P,A]];
      SetLength(V, Length(Samples[S].Tokens));
      for Z := 0 to Dimensions[2] - 1 do for Y := 0 to Dimensions[1] - 1 do
        for X := 0 to Dimensions[0] - 1 do
        begin
          Position[0] := X; Position[1] := Y; Position[2] := Z;
          for A := 0 to 2 do
          begin
            OutPosition[A] := Position[Axes[P,A]];
            if (M and (1 shl A)) <> 0 then
              OutPosition[A] := OutDimensions[A] - 1 - OutPosition[A];
          end;
          V[(OutPosition[2] * OutDimensions[1] + OutPosition[1]) * OutDimensions[0] + OutPosition[0]] :=
            TokenIndex(Result.Palette, Samples[S].Tokens[(Z * Dimensions[1] + Y) * Dimensions[0] + X]);
        end;
      LX := OutDimensions[0]; LY := OutDimensions[1]; LZ := OutDimensions[2];
      if Boundary = wmbOpen then begin LX := LX - PW + 1; LY := LY - PH + 1; LZ := LZ - PD + 1; end;
      for Z := 0 to LZ - 1 do for Y := 0 to LY - 1 do for X := 0 to LX - 1 do
      begin
        for PZ := 0 to PD - 1 do for PY := 0 to PH - 1 do for PX := 0 to PW - 1 do
        begin
          SX := (X + PX) mod OutDimensions[0]; SY := (Y + PY) mod OutDimensions[1];
          SZ := (Z + PZ) mod OutDimensions[2];
          Payload[(PZ * PH + PY) * PW + PX] := V[(SZ * OutDimensions[1] + SY) * OutDimensions[0] + SX];
        end;
        Observe(Result, Payload);
      end;
    end;
  end;
end;

function OracleCompatible(const A, B: TWfcPattern3DPayload; const W, H, D: Integer;
  const Direction: TWfcModelDirection): Boolean;
const DX: array[0..5] of Integer = (0,1,0,-1,0,0);
  DY: array[0..5] of Integer = (-1,0,1,0,0,0);
  DZ: array[0..5] of Integer = (0,0,0,0,1,-1);
var X, Y, Z, NX, NY, NZ: Integer;
begin
  for Z := 0 to D - 1 do for Y := 0 to H - 1 do for X := 0 to W - 1 do
  begin
    NX := X - DX[Ord(Direction)]; NY := Y - DY[Ord(Direction)]; NZ := Z - DZ[Ord(Direction)];
    if (NX < 0) or (NY < 0) or (NZ < 0) or (NX >= W) or (NY >= H) or (NZ >= D) then Continue;
    if A[(Z * H + Y) * W + X] <> B[(NZ * H + NY) * W + NX] then Exit(False);
  end;
  Result := True;
end;

procedure CompareOracle(const Samples: TWfcLearnVolumeSamples; const PW, PH, PD: Integer;
  const Boundary: TWfcModelBoundary; const Symmetry: TWfcModelSymmetry;
  const LabelText: String);
var Expected: TOracle; Model: TWfcOverlappingModel3D; I, J, X, Y, Z, Total: Integer;
  Direction: TWfcModelDirection; Shape: TWfcModelSampleShape; Compatible: Boolean;
begin
  Expected := Oracle(Samples, PW, PH, PD, Boundary, Symmetry);
  Model := LearnOverlappingModel3DCorpus(Samples, PW, PH, PD, Boundary, Symmetry);
  try
    Check((Model.PatternWidth = PW) and (Model.PatternHeight = PH) and (Model.PatternDepth = PD), LabelText + ': footprint');
    Check((Model.SourceBoundary = Boundary) and (Model.Symmetry = Symmetry), LabelText + ': source policy');
    Check(Model.SourceCount = Length(Samples), LabelText + ': sample count');
    Check(Model.CompiledModel.Rank = 3, LabelText + ': genuinely rank three');
    for I := 0 to High(Samples) do
    begin
      Shape := Model.SourceShapeAt(I);
      Check((Shape.Width = Samples[I].Width) and (Shape.Height = Samples[I].Height) and
        (Shape.Depth = Samples[I].Depth), LabelText + ': original source XYZ shape');
    end;
    Check(Model.PaletteCount = Length(Expected.Palette), LabelText + ': palette size');
    for I := 0 to High(Expected.Palette) do
    begin
      Check(Model.PaletteTokenAt(I) = Expected.Palette[I], LabelText + ': first original appearance');
      Check(Model.FindPaletteToken(Expected.Palette[I]) = I, LabelText + ': exact palette lookup');
    end;
    Check(Model.PatternCount = Length(Expected.Patterns), LabelText + ': pattern count');
    Total := 0;
    for I := 0 to High(Expected.Patterns) do
    begin
      Check(EqualPayload(Model.CopyPattern(I), Expected.Patterns[I]), LabelText + ': literal ordered payload');
      Check(Model.PatternWeightAt(I) = Expected.Weights[I], LabelText + ': raw observation weight');
      Inc(Total, Model.PatternWeightAt(I));
      Check(Model.FindPatternKey(Model.PatternKeyAt(I)) = I, LabelText + ': exact pattern key lookup');
      for Z := 0 to PD - 1 do for Y := 0 to PH - 1 do for X := 0 to PW - 1 do
        Check(Model.PatternPaletteIndexAt(I,X,Y,Z) = Expected.Patterns[I][(Z*PH+Y)*PW+X], LabelText + ': XYZ payload addressing');
      for J := 0 to High(Expected.Patterns) do for Direction := Low(Direction) to High(Direction) do
      begin
        Compatible := OracleCompatible(Expected.Patterns[I],Expected.Patterns[J],PW,PH,PD,Direction);
        Check(Model.PatternsCompatible(I,J,Direction) = Compatible, LabelText + ': six-direction literal overlap');
        Check((Model.CompiledModel.RelationCount(Direction,I,J) <> 0) = Compatible, LabelText + ': compiled relation matches overlap');
      end;
    end;
    Check(Total > 0, LabelText + ': nonempty raw observations');
  finally Model.Free; end;
end;

procedure TestLiteralCorpus;
var S: TWfcLearnVolumeSamples; Symmetry: TWfcModelSymmetry;
begin
  SetLength(S,1); S[0] := UniqueSample(3,2,4,'v');
  CompareOracle(S,2,1,3,wmbOpen,wmsNone,'open cuboid');
  for Symmetry := Low(Symmetry) to High(Symmetry) do
  begin
    S[0] := UniqueSample(3,2,2,'c');
    CompareOracle(S,2,2,2,wmbOpen,Symmetry,'rectangular source symmetry ' + IntToStr(Ord(Symmetry)));
    S[0] := UniqueSample(2,1,2,'w');
    CompareOracle(S,2,2,2,wmbWrap,Symmetry,'wrapped oversized footprint ' + IntToStr(Ord(Symmetry)));
  end;
  S[0] := UniqueSample(1,1,1,'single');
  for Symmetry := Low(Symmetry) to High(Symmetry) do
    CompareOracle(S,3,3,3,wmbWrap,Symmetry,'singleton stabilizers ' + IntToStr(Ord(Symmetry)));
  S[0] := UniqueSample(1,1,3,'depth');
  CompareOracle(S,1,1,2,wmbOpen,wmsNone,'Z-only overlap');
  S[0] := UniqueSample(1,1,1,'large');
  CompareOracle(S,1,1,4096,wmbWrap,wmsNone,'maximum footprint cells');
  SetLength(S,2); S[0] := UniqueSample(2,1,1,'left'); S[1] := UniqueSample(2,1,1,'right');
  CompareOracle(S,2,1,1,wmbOpen,wmsNone,'independent open corpus');
  CompareOracle(S,3,1,1,wmbWrap,wmsNone,'independent wrapped corpus');
end;

procedure TestChirality;
var S: TWfcLearnVolumeSamples; Rotations, Full: TWfcOverlappingModel3D;
  I, J, Missing: Integer; Found: Boolean;
begin
  SetLength(S,1); S[0] := UniqueSample(2,2,2,'corner');
  Rotations := LearnOverlappingModel3DCorpus(S,2,2,2,wmbOpen,wmsCubeRotations);
  try
    Full := LearnOverlappingModel3DCorpus(S,2,2,2,wmbOpen,wmsCubeFull);
    try
      Check(Rotations.PatternCount = 24, 'eight uniquely labeled cube corners have 24 proper orientations');
      Check(Full.PatternCount = 48, 'reflections add the other 24 chiral orientations');
      Missing := 0;
      for I := 0 to Full.PatternCount - 1 do
      begin
        Found := False;
        for J := 0 to Rotations.PatternCount - 1 do
          if EqualPayload(Full.CopyPattern(I),Rotations.CopyPattern(J)) then Found := True;
        if not Found then Inc(Missing);
        Check(Full.PatternWeightAt(I) = 1, 'full chiral orbit raw weight');
      end;
      Check(Missing = 24, 'reflection orbit is disjoint, not renamed rotation duplicates');
    finally Full.Free; end;
  finally Rotations.Free; end;
end;

procedure ExpectRejected(const Samples: TWfcLearnVolumeSamples; const PW, PH, PD: Integer;
  const Boundary: TWfcModelBoundary; const Symmetry: TWfcModelSymmetry; const Why: String);
var M: TWfcOverlappingModel3D; Rejected: Boolean;
begin
  M := nil; Rejected := False;
  try M := LearnOverlappingModel3DCorpus(Samples,PW,PH,PD,Boundary,Symmetry);
  except on E: EWfcOverlapping3D do Rejected := True; end;
  M.Free; Check(Rejected, Why);
end;

procedure TestInvalidInputs;
var S: TWfcLearnVolumeSamples; I, J: Integer; Boundary: TWfcModelBoundary;
  Symmetry: TWfcModelSymmetry;
begin
  S := nil; ExpectRejected(S,1,1,1,wmbOpen,wmsNone,'empty corpus rejected');
  SetLength(S,1); S[0] := UniqueSample(2,2,2,'v');
  for I := 0 to 2 do
  begin
    case I of
      0: ExpectRejected(S,0,1,1,wmbOpen,wmsNone,'zero footprint width');
      1: ExpectRejected(S,1,-1,1,wmbOpen,wmsNone,'negative footprint height');
      2: ExpectRejected(S,1,1,High(Integer),wmbOpen,wmsNone,'oversize footprint depth');
    end;
  end;
  ExpectRejected(S,4096,2,1,wmbWrap,wmsNone,'footprint product cap before allocation');
  ExpectRejected(S,3,2,2,wmbOpen,wmsNone,'open footprint not contained');
  ExpectRejected(S,1,2,1,wmbWrap,wmsD4,'D4 rejects nonsquare XY footprint');
  ExpectRejected(S,2,2,1,wmbWrap,wmsCubeRotations,'cube24 rejects noncube footprint');
  ExpectRejected(S,2,1,2,wmbWrap,wmsCubeFull,'cube48 rejects noncube footprint');
  I := 2; Boundary := TWfcModelBoundary(I); I := 4; Symmetry := TWfcModelSymmetry(I);
  ExpectRejected(S,1,1,1,Boundary,wmsNone,'unknown source boundary');
  ExpectRejected(S,1,1,1,wmbOpen,Symmetry,'unknown symmetry');
  S[0].Width := 0; ExpectRejected(S,1,1,1,wmbOpen,wmsNone,'zero source width');
  S[0].Width := 2; S[0].Height := -1;
  ExpectRejected(S,1,1,1,wmbOpen,wmsNone,'negative source height');
  S[0].Height := 2; S[0].Depth := WFC_PATTERN_3D_MAX_SOURCE_DIMENSION + 1;
  ExpectRejected(S,1,1,1,wmbWrap,wmsNone,'source dimension cap');
  S[0].Width := 4096; S[0].Height := 4096; S[0].Depth := 1;
  ExpectRejected(S,1,1,1,wmbOpen,wmsNone,'source product cap before indexing');
  S[0] := UniqueSample(2,2,2,'v'); SetLength(S[0].Tokens,7);
  ExpectRejected(S,1,1,1,wmbOpen,wmsNone,'source shape and tokens mismatch');
  S[0] := UniqueSample(1,1,1,'v'); S[0].Tokens[0] := '';
  ExpectRejected(S,1,1,1,wmbOpen,wmsNone,'empty token rejected');
  {$IFDEF PAS2JS}asm S[0].Tokens[0] = '\ud800'; end;
  {$ELSE}
  SetLength(S[0].Tokens[0],3);
  S[0].Tokens[0][1] := AnsiChar($ED); S[0].Tokens[0][2] := AnsiChar($A0);
  S[0].Tokens[0][3] := AnsiChar($80);
  {$ENDIF}
  ExpectRejected(S,1,1,1,wmbOpen,wmsNone,'invalid Unicode scalar token rejected');
  SetLength(S,WFC_PATTERN_3D_MAX_SOURCE_COUNT+1);
  ExpectRejected(S,1,1,1,wmbOpen,wmsNone,'sample count preflight');
  SetLength(S,1); S[0] := UniqueSample(WFC_PATTERN_3D_MAX_PALETTE_COUNT+1,1,1,'palette');
  ExpectRejected(S,1,1,1,wmbOpen,wmsNone,'palette limit before pattern extraction');
  SetLength(S,WFC_PATTERN_3D_MAX_PATTERN_COUNT+1);
  for I := 0 to High(S) do
  begin
    S[I].Width := 11; S[I].Height := 1; S[I].Depth := 1; SetLength(S[I].Tokens,11);
    for J := 0 to 10 do
      if (I and (1 shl J)) <> 0 then S[I].Tokens[J] := 'one' else S[I].Tokens[J] := 'zero';
  end;
  ExpectRejected(S,11,1,1,wmbOpen,wmsNone,'unique pattern count limit before dense relations');
end;

procedure TestCollisionIdentity;
var S: TWfcLearnVolumeSamples; I, J, N, Slot, Hash, First, Second: Integer;
  Slots: array[0..2047] of Integer; PaletteSlots: array[0..8191] of Integer;
  A, B, Candidate: String;
begin
  { Select two genuinely different three-cell payloads in the learner's same
    2048-slot hash bucket. This independent low-bit calculation proves the
    fixture exercises a collision; the oracle still uses linear equality. }
  for I := 0 to High(Slots) do Slots[I] := 0;
  First := -1; Second := -1;
  for I := 0 to 4095 do
  begin
    Hash := 2166136261 mod 2048;
    Hash := ((Hash xor (I div 256))*403) mod 2048;
    Hash := ((Hash xor ((I div 16) mod 16))*403) mod 2048;
    Slot := ((Hash xor (I mod 16))*403) mod 2048;
    if Slots[Slot] <> 0 then begin First := Slots[Slot]-1; Second := I; Break; end;
    Slots[Slot] := I+1;
  end;
  Check((First >= 0) and (Second > First),'payload fixture provably collides in cache buckets');
  SetLength(S,3); S[0] := UniqueSample(16,1,1,'p');
  for I := 1 to 2 do
  begin
    if I = 1 then N := First else N := Second;
    S[I].Width := 3; S[I].Height := 1; S[I].Depth := 1;
    S[I].Tokens := TokenList([TWfcModelToken('p'+IntToStr(N div 256)),
      TWfcModelToken('p'+IntToStr((N div 16) mod 16)),TWfcModelToken('p'+IntToStr(N mod 16))]);
  end;
  CompareOracle(S,3,1,1,wmbOpen,wmsNone,'collision-safe payload identity');
  { The model's cached full payloads and overlap slabs use a different hash.
    These are FULL hash collisions, not merely equal bucket indices:
    ((5381*33+0+1)*33+33+1) = ((5381*33+1+1)*33+0+1). }
  Check(((5381*33+1)*33+34) = ((5381*33+2)*33+1),'independent full face-hash collision');
  SetLength(S,3); S[0] := UniqueSample(34,1,1,'p');
  S[1].Width := 3; S[1].Height := 1; S[1].Depth := 1;
  S[2].Width := 3; S[2].Height := 1; S[2].Depth := 1;
  S[1].Tokens := TokenList(['p0','p33','p2']); S[2].Tokens := TokenList(['p1','p0','p2']);
  CompareOracle(S,3,1,1,wmbOpen,wmsNone,'full-hash payload and overlap slab collisions');
  for I := 0 to High(PaletteSlots) do PaletteSlots[I] := 0;
  A := ''; B := '';
  for I := 1 to 2000 do
  begin
    Candidate := 'token'+IntToStr(I); Hash := 2166136261 mod 8192;
    for J := 1 to Length(Candidate) do Hash := ((Hash xor Ord(Candidate[J]))*403) mod 8192;
    if PaletteSlots[Hash] <> 0 then
    begin A := 'token'+IntToStr(PaletteSlots[Hash]); B := Candidate; Break; end;
    PaletteSlots[Hash] := I;
  end;
  Check((A <> '') and (A <> B),'token fixture provably collides in cache buckets');
  SetLength(S,1); S[0].Width := 4; S[0].Height := 1; S[0].Depth := 1;
  S[0].Tokens := TokenList([TWfcModelToken(A),TWfcModelToken(B),TWfcModelToken(A),TWfcModelToken(B)]);
  CompareOracle(S,1,1,1,wmbOpen,wmsNone,'collision-safe exact token identity');
end;

procedure TestProjection;
var S: TWfcLearnVolumeSamples; M: TWfcOverlappingModel3D;
  Grid, BadGrid: TWfcPatternGrid3D; Output, BadOutput: TWfcTokenGrid3D;
  Report: TWfcOverlapping3DValidationReport; Indices: TWfcPattern3DIndices;
  I, Pass, W, H, D: Integer; Boundary: TWfcModelBoundary;
begin
  SetLength(S,1);
  for Pass := 0 to 1 do
  begin
    if Pass = 0 then begin S[0] := UniqueSample(3,2,4,'xyz'); Boundary := wmbOpen; end
    else begin S[0] := UniqueSample(2,2,2,'wrap'); Boundary := wmbWrap; end;
    if Pass = 0 then M := LearnOverlappingModel3DCorpus(S,2,1,3,Boundary,wmsNone)
    else M := LearnOverlappingModel3DCorpus(S,2,2,2,Boundary,wmsNone);
    try
      W := S[0].Width; H := S[0].Height; D := S[0].Depth;
      if Boundary = wmbOpen then
      begin Dec(W,M.PatternWidth-1); Dec(H,M.PatternHeight-1); Dec(D,M.PatternDepth-1); end;
      SetLength(Indices,W*H*D);
      for I := 0 to High(Indices) do Indices[I] := I;
      Grid := MakeWfcPatternGrid3D(W,H,D,Boundary,Indices);
      Indices[0] := -1;
      Check(Grid.Patterns[0] = 0,'grid constructor detaches pattern array');
      Check(ValidateOverlappingPatternGrid3D(M,Grid,Report),'literal original origin grid validates');
      Check(Report.CheckedPatterns = W*H*D,'independent validator checks all XYZ origins');
      Check(TryProjectOverlappingPatternGrid3D(M,Grid,Output,Report),'literal original grid projects');
      Check((Output.Width = S[0].Width) and (Output.Height = S[0].Height) and
        (Output.Depth = S[0].Depth),'projection dimensions honor XYZ open halo / wrap');
      Check(Length(Output.Tokens) = Length(S[0].Tokens),'projection token volume');
      for I := 0 to High(Output.Tokens) do Check(Output.Tokens[I] = S[0].Tokens[I],'projection reconstructs actual original XYZ token');
      Check(ValidateOverlappingProjection3D(M,Grid,Output,Report),'independent complete projected-token audit');
      BadOutput.Width := Output.Width; BadOutput.Height := Output.Height; BadOutput.Depth := Output.Depth;
      SetLength(BadOutput.Tokens,Length(Output.Tokens));
      for I := 0 to High(Output.Tokens) do BadOutput.Tokens[I] := Output.Tokens[I];
      BadOutput.Tokens[High(BadOutput.Tokens)] := 'forged';
      Check(not ValidateOverlappingProjection3D(M,Grid,BadOutput,Report),'forged far XYZ projection token rejected');
      Check((Report.Issue.Kind = wo3ikProjectionToken) and
        ((Report.Issue.Z + Report.Issue.PatternOffsetZ) mod Output.Depth = Output.Depth-1),
        'projection issue retains anchor and footprint depth');
      BadOutput := Output; Inc(BadOutput.Depth);
      Check(not ValidateOverlappingProjection3D(M,Grid,BadOutput,Report),'forged projected depth rejected');
      BadGrid := MakeWfcPatternGrid3D(Grid.Width,Grid.Height,Grid.Depth,Boundary,Grid.Patterns);
      BadGrid.Patterns[0] := High(Grid.Patterns);
      Check(not ValidateOverlappingPatternGrid3D(M,BadGrid,Report),'inconsistent neighboring footprint rejected');
      Check(Report.Issue.Kind = wo3ikOverlap,'inconsistent footprint reports overlap');
      Check(not TryProjectOverlappingPatternGrid3D(M,BadGrid,BadOutput,Report),'invalid overlap cannot publish projection');
      Check(Length(BadOutput.Tokens) = 0,'failed projection clears previous token output');
      BadGrid.Patterns[0] := M.PatternCount;
      Check(not ValidateOverlappingPatternGrid3D(M,BadGrid,Report),'out-of-range pattern index rejected');
      BadGrid := Grid; Inc(BadGrid.Depth);
      Check(not ValidateOverlappingPatternGrid3D(M,BadGrid,Report),'inconsistent origin depth rejected');
    finally M.Free; end;
  end;
end;

procedure TestModelOwnershipAndCodec;
const Golden = 'wfcp=2'#10+'rank=3'#10+'samples=1'#10+'s=0,2,1,1'#10+
  'footprint=2,1,1'#10+'boundary=open'#10+'symmetry=none'#10+
  'directions=N,E,S,W,U,D'#10+'palette=2'#10+'t=0,%2C'#10+'t=1,%E2%99%AB'#10+
  'patterns=1'#10+'p=0,1,0,1'#10+'relations=overlap'#10+'end'#10;
var Tokens, Palette: TWfcModelTokens; Patterns: TWfcPattern3DPayloads;
  Weights: TWfcModelIntegerArray; Shapes: TWfcModelSampleShapes;
  M, CopyModel, Decoded, Reordered: TWfcOverlappingModel3D;
  Before, Key: String; I, J, Mode: Integer; Rejected: Boolean;
begin
  Tokens := TokenList([',',{$IFDEF PAS2JS}'♫'{$ELSE}UTF8Encode(UnicodeString(#$266B)){$ENDIF}]);
  M := LearnOverlappingModel3D(Tokens,2,1,1,2,1,1,wmbOpen,wmsNone);
  try
    Before := EncodeWfcPattern3DText(M); Key := String(M.PatternKeyAt(0));
    Check(Before = Golden,'hand-authored ASCII wfcp2 punctuation/Unicode golden');
    Tokens[0] := 'mutated source';
    Check(EncodeWfcPattern3DText(M) = Before,'learner result detaches source tokens');
    Decoded := DecodeWfcPattern3DText(Before);
    try
      Check(EncodeWfcPattern3DText(Decoded) = Before,'canonical codec roundtrip exact bytes');
      Check(String(Decoded.PatternKeyAt(0)) = Key,'codec retains actual-payload identity');
      for I := 0 to 5 do Check(M.PatternsCompatible(0,0,TWfcModelDirection(I)) =
        Decoded.PatternsCompatible(0,0,TWfcModelDirection(I)),'codec derives each exact overlap direction');
    finally Decoded.Free; end;
    Shapes := M.CopySourceShapes; Palette := M.CopyPalette;
    Patterns := M.CopyPatterns; Weights := M.CopyPatternWeights;
    CopyModel := TWfcOverlappingModel3D.Create(2,1,1,wmbOpen,wmsNone,Shapes,Palette,Patterns,Weights);
    try
      Shapes[0].Depth := 99; Palette[0] := 'changed'; Patterns[0][0] := 1; Weights[0] := 77;
      Check(EncodeWfcPattern3DText(CopyModel) = Before,'constructor deep copies all caller-owned nested arrays');
      Check(EncodeWfcPattern3DText(M) = Before,'copy getters detach original model');
      Patterns := CopyModel.CopyPatterns; Patterns[0][1] := 0;
      Check(EncodeWfcPattern3DText(CopyModel) = Before,'copy getter nested payload mutation cannot escape');
    finally CopyModel.Free; end;
    Shapes := M.CopySourceShapes; Palette := M.CopyPalette;
    Tokens := TokenList([Palette[1],Palette[0]]); Patterns := M.CopyPatterns;
    Patterns[0][0] := 1; Patterns[0][1] := 0; Weights := M.CopyPatternWeights;
    Reordered := TWfcOverlappingModel3D.Create(2,1,1,wmbOpen,wmsNone,Shapes,Tokens,Patterns,Weights);
    try Check(String(Reordered.PatternKeyAt(0)) = Key,'exact key independent of unrelated palette-index renumbering');
    finally Reordered.Free; end;
    Tokens := TokenList(['other','tokens']);
    Reordered := LearnOverlappingModel3D(Tokens,2,1,1,2,1,1,wmbOpen,wmsNone);
    try Check(String(Reordered.PatternKeyAt(0)) <> Key,'equal numeric payload with different actual tokens has a different key');
    finally Reordered.Free; end;
    Reordered := LearnOverlappingModel3D(M.CopyPalette,1,1,2,1,1,2,wmbOpen,wmsNone);
    try Check(String(Reordered.PatternKeyAt(0)) <> Key,'exact key binds footprint XYZ');
    finally Reordered.Free; end;
    for Mode := 0 to 8 do
    begin
      Shapes := M.CopySourceShapes; Palette := M.CopyPalette;
      Patterns := M.CopyPatterns; Weights := M.CopyPatternWeights;
      case Mode of
        0: Weights[0] := 0;
        1: Weights[0] := -1;
        2: Weights[0] := 2;
        3: Patterns[0][0] := -1;
        4: Patterns[0][0] := 2;
        5: Palette[0] := Palette[1];
        6: begin SetLength(Palette,3); Palette[2] := 'unused'; end;
        7: SetLength(Patterns[0],1);
        8: begin
          Shapes[0].Width := 3; SetLength(Patterns,2); SetLength(Patterns[1],2);
          for J := 0 to 1 do Patterns[1][J] := Patterns[0][J];
          SetLength(Weights,2); Weights[1] := 1;
        end;
      end;
      Rejected := False; Reordered := nil;
      try Reordered := TWfcOverlappingModel3D.Create(2,1,1,wmbOpen,wmsNone,Shapes,Palette,Patterns,Weights);
      except on E: EWfcOverlapping3D do Rejected := True; end;
      Reordered.Free; Check(Rejected,'malformed immutable model rejects atomically '+IntToStr(Mode));
      Check(EncodeWfcPattern3DText(M) = Before,'rejected construction leaves existing model unchanged');
    end;
  finally M.Free; end;
end;

{$IFDEF PAS2JS}
procedure HostileModel(const V, Mode: Integer);
var Shapes: TWfcModelSampleShapes; Palette: TWfcModelTokens;
  Patterns: TWfcPattern3DPayloads; Weights: TWfcModelIntegerArray;
  W,H,D: Integer; Boundary: TWfcModelBoundary; Symmetry: TWfcModelSymmetry;
  Rejected: Boolean; M: TWfcOverlappingModel3D;
begin
  SetLength(Shapes,1); Shapes[0] := MakeWfcModelSampleShape(1,1,1);
  Palette := TokenList(['v']); SetLength(Patterns,1); SetLength(Patterns[0],1);
  Patterns[0][0] := 0; SetLength(Weights,1); Weights[0] := 1;
  W := 1; H := 1; D := 1; Boundary := wmbOpen; Symmetry := wmsNone;
  case Mode of
    0: W := V; 1: H := V; 2: D := V;
    3: Shapes[0].Width := V; 4: Shapes[0].Height := V; 5: Shapes[0].Depth := V;
    6: Patterns[0][0] := V; 7: Weights[0] := V;
    8: asm Boundary = V; end;
    9: asm Symmetry = V; end;
  end;
  Rejected := False; M := nil;
  try M := TWfcOverlappingModel3D.Create(W,H,D,Boundary,Symmetry,Shapes,Palette,Patterns,Weights);
  except on E: EWfcOverlapping3D do Rejected := True; end;
  M.Free; Check(Rejected,'hostile immutable model field '+IntToStr(Mode));
end;

procedure TestHostileJavaScript;
var S: TWfcLearnVolumeSamples; I, V, J: Integer; Boundary: TWfcModelBoundary;
  Symmetry: TWfcModelSymmetry;
begin
  SetLength(S,1);
  for I := 0 to 10 do
  begin
    S[0] := UniqueSample(1,1,1,'v');
    asm V = [NaN, Infinity, -Infinity, 0.5, undefined, null, '1', true, {}, 4294967296, 9007199254740992][I]; end;
    ExpectRejected(S,V,1,1,wmbWrap,wmsNone,'hostile footprint width '+IntToStr(I));
    ExpectRejected(S,1,V,1,wmbWrap,wmsNone,'hostile footprint height '+IntToStr(I));
    ExpectRejected(S,1,1,V,wmbWrap,wmsNone,'hostile footprint depth '+IntToStr(I));
    S[0].Width := V; ExpectRejected(S,1,1,1,wmbWrap,wmsNone,'hostile source width '+IntToStr(I));
    S[0].Width := 1; S[0].Height := V;
    ExpectRejected(S,1,1,1,wmbWrap,wmsNone,'hostile source height '+IntToStr(I));
    S[0].Height := 1; S[0].Depth := V;
    ExpectRejected(S,1,1,1,wmbWrap,wmsNone,'hostile source depth '+IntToStr(I));
    S[0].Depth := 1; asm Boundary = V; Symmetry = V; end;
    ExpectRejected(S,1,1,1,Boundary,wmsNone,'hostile boundary '+IntToStr(I));
    ExpectRejected(S,1,1,1,wmbWrap,Symmetry,'hostile symmetry '+IntToStr(I));
    for J := 0 to 9 do HostileModel(V,J);
  end;
  for I := 0 to 5 do
  begin
    S[0] := UniqueSample(1,1,1,'v');
    asm S[0].Tokens[0] = [undefined,null,1,true,{},[]][I]; end;
    ExpectRejected(S,1,1,1,wmbOpen,wmsNone,'hostile token '+IntToStr(I));
  end;
end;
{$ENDIF}

begin
  try
    TestLiteralCorpus; TestChirality; TestInvalidInputs; TestCollisionIdentity;
    TestProjection; TestModelOwnershipAndCodec;
    {$IFDEF PAS2JS}TestHostileJavaScript;{$ENDIF}
    WriteLn('Pattern 3D: ',Checks,' checks passed.');
  except on E: Exception do begin WriteLn('[FAIL] ',E.ClassName,': ',E.Message); Halt(1); end; end;
end.
