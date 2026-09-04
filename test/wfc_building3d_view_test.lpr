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
program wfc_building3d_view_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_voxel3d,
  wfc_voxel3d_mesh,
  wfc_voxel3d_isometric,
  wfc_building3d,
  wfc_building3d_validate,
  wfc_building3d_view,
  building3d_showcase;

type
  TTestProcedure = procedure;

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GCheckCount);
  if not ACondition then
  begin
    Inc(GFailureCount);
    WriteLn('FAIL: ', AMessage);
  end;
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn(AName);
  try
    ATest;
  except
    on E: Exception do
    begin
      Inc(GCheckCount);
      Inc(GFailureCount);
      WriteLn('FAIL: unexpected ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function ColorsEqual(const A, B: TVoxel3DColor): Boolean;
begin
  Result := (A.R = B.R) and (A.G = B.G) and
    (A.B = B.B) and (A.A = B.A);
end;

function PointsEqual(const A, B: TVoxel3DViewPoint3): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.Z = B.Z);
end;

function VerticesEqual(const A, B: TVoxel3DViewVertices): Boolean;
var
  I: Integer;
begin
  for I := Low(A) to High(A) do
    if not PointsEqual(A[I], B[I]) then
      Exit(False);
  Result := True;
end;

function QuadsEqual(const A, B: TVoxel3DViewQuad): Boolean;
begin
  Result := VerticesEqual(A.Vertices, B.Vertices) and
    (A.CellX = B.CellX) and (A.CellY = B.CellY) and
    (A.CellZ = B.CellZ) and (A.Direction = B.Direction) and
    (A.Rotation = B.Rotation) and
    (A.LayerOrder = B.LayerOrder) and (A.LayerId = B.LayerId) and
    (A.PrototypeId = B.PrototypeId) and
    (A.Material = B.Material) and (A.Semantic = B.Semantic) and
    ColorsEqual(A.FillColor, B.FillColor) and
    ColorsEqual(A.EdgeColor, B.EdgeColor);
end;

procedure CheckLineage(const ABuilding: TBuilding3D;
  const AFace: TBuilding3DViewFace; const AMessage: String);
var
  E, P, S: TVoxel3DVariant;
  X, Y, Z: Integer;
begin
  X := AFace.Quad.CellX;
  Y := AFace.Quad.CellY;
  Z := AFace.Quad.CellZ;
  S := ABuilding.StageVariantAt(b3sStructure, X, Y, Z);
  E := ABuilding.StageVariantAt(b3sEnvelopeRoof, X, Y, Z);
  P := ABuilding.StageVariantAt(b3sProps, X, Y, Z);
  Check((AFace.Lineage.FootprintRole =
      ABuilding.FootprintRoleAt(X, Y, Z)) and
    (AFace.Lineage.FootprintToken = Building3DFootprintRoleToken(
      ABuilding.FootprintRoleAt(X, Y, Z))),
    AMessage + ': footprint lineage');
  Check((AFace.Lineage.StructureKind =
      ABuilding.StructureKindAt(X, Y, Z)) and
    (AFace.Lineage.StructurePrototypeId = S.PrototypeId) and
    (AFace.Lineage.StructureMaterial = S.Material) and
    (AFace.Lineage.StructureRotation = S.Rotation),
    AMessage + ': structure lineage');
  Check((AFace.Lineage.EnvelopeKind =
      ABuilding.EnvelopeKindAt(X, Y, Z)) and
    (AFace.Lineage.EnvelopePrototypeId = E.PrototypeId) and
    (AFace.Lineage.EnvelopeMaterial = E.Material) and
    (AFace.Lineage.EnvelopeRotation = E.Rotation),
    AMessage + ': envelope lineage');
  Check((AFace.Lineage.PropKind =
      ABuilding.PropKindAt(X, Y, Z)) and
    (AFace.Lineage.PropPrototypeId = P.PrototypeId) and
    (AFace.Lineage.PropMaterial = P.Material) and
    (AFace.Lineage.PropRotation = P.Rotation),
    AMessage + ': prop lineage');
end;

procedure CheckPortableQuad(const AQuad: TVoxel3DViewQuad;
  const AWidth, AHeight, ADepth: Integer; const AMessage: String);
var
  I: Integer;
begin
  Check((AQuad.CellX >= 0) and (AQuad.CellX < AWidth) and
    (AQuad.CellY >= 0) and (AQuad.CellY < AHeight) and
    (AQuad.CellZ >= 0) and (AQuad.CellZ < ADepth),
    AMessage + ': cell coordinates');
  Check(IsVoxel3DToken(AQuad.LayerId) and
    IsVoxel3DToken(AQuad.PrototypeId) and
    IsVoxel3DToken(AQuad.Material) and
    IsVoxel3DToken(AQuad.Semantic),
    AMessage + ': portable public metadata');
  Check((Pos('@v3', AQuad.LayerId) = 0) and
    (Pos('@v3', AQuad.PrototypeId) = 0) and
    (Pos('@v3', AQuad.Material) = 0) and
    (Pos('@v3', AQuad.Semantic) = 0),
    AMessage + ': private keys never leak');
  for I := Low(AQuad.Vertices) to High(AQuad.Vertices) do
    Check((AQuad.Vertices[I].X >= 0) and
      (AQuad.Vertices[I].X <= AWidth * WFC_VOXEL3D_SUBCELL_SCALE) and
      (AQuad.Vertices[I].Y >= 0) and
      (AQuad.Vertices[I].Y <= AHeight * WFC_VOXEL3D_SUBCELL_SCALE) and
      (AQuad.Vertices[I].Z >= 0) and
      (AQuad.Vertices[I].Z <= ADepth * WFC_VOXEL3D_SUBCELL_SCALE) and
      (AQuad.Vertices[I].X mod WFC_VOXEL3D_SUBCELL_SCALE = 0) and
      (AQuad.Vertices[I].Y mod WFC_VOXEL3D_SUBCELL_SCALE = 0) and
      (AQuad.Vertices[I].Z mod WFC_VOXEL3D_SUBCELL_SCALE = 0),
      AMessage + ': fixed-subcell vertex');
end;

procedure TestShowcaseBlueprintAndExpected;
var
  B: TBuilding3DBlueprint;
  E: TBuilding3DShowcaseExpected;
  Y, Z: Integer;
begin
  Check((BUILDING3D_SHOWCASE_VERSION = 1) and
    (BUILDING3D_SHOWCASE_WIDTH = 7) and
    (BUILDING3D_SHOWCASE_HEIGHT = 5) and
    (BUILDING3D_SHOWCASE_DEPTH = 3) and
    (BUILDING3D_SHOWCASE_CELL_COUNT = 105),
    'showcase constants retain the depth-three fixture');
  B := NewBuilding3DShowcaseBlueprint;
  try
    Check((B.Width = BUILDING3D_SHOWCASE_WIDTH) and
      (B.Height = BUILDING3D_SHOWCASE_HEIGHT) and
      (B.Depth = BUILDING3D_SHOWCASE_DEPTH) and
      (B.CellCount = BUILDING3D_SHOWCASE_CELL_COUNT),
      'showcase blueprint has exact dimensions');
    for Z := 0 to BUILDING3D_SHOWCASE_DEPTH - 1 do
      for Y := 0 to BUILDING3D_SHOWCASE_HEIGHT - 1 do
        Check((B[0, Y, Z] = b3frVoid) and
          (B[6, Y, Z] = b3frVoid),
          'showcase leaves both side columns void');
    Check(B[BUILDING3D_SHOWCASE_ENTRANCE_X,
      BUILDING3D_SHOWCASE_ENTRANCE_Y,
      BUILDING3D_SHOWCASE_ENTRANCE_Z] = b3frEntranceSouth,
      'showcase retains its south entrance');
    Check(B[BUILDING3D_SHOWCASE_ENTRANCE_X,
      BUILDING3D_SHOWCASE_ENTRANCE_Y, 1] = b3frLintel,
      'showcase retains its entrance lintel');
    Check(B[BUILDING3D_SHOWCASE_FEATURE_X,
      BUILDING3D_SHOWCASE_FEATURE_Y,
      BUILDING3D_SHOWCASE_FEATURE_Z] = b3frFeature,
      'showcase retains its interior feature');
    Check((B[1, 4, 2] = b3frRoof) and
      (B[5, 0, 0] = b3frGroundShell) and
      (B[2, 2, 1] = b3frInterior),
      'showcase retains roof, shell, and interior massing');
  finally
    B.Free;
  end;

  Check(TryBuilding3DShowcaseExpected(0, E) and
    (E.PipelineSignature =
      BUILDING3D_SHOWCASE_SEED_ZERO_PIPELINE_SIGNATURE) and
    (E.StructureSceneSignature =
      BUILDING3D_SHOWCASE_SEED_ZERO_STRUCTURE_SIGNATURE),
    'seed-zero expected signatures are public and exact');
  Check(TryBuilding3DShowcaseExpected(
      BUILDING3D_SHOWCASE_DEFAULT_SEED, E) and
    (E.PipelineSignature =
      BUILDING3D_SHOWCASE_DEFAULT_PIPELINE_SIGNATURE) and
    (E.StructureSceneSignature =
      BUILDING3D_SHOWCASE_DEFAULT_STRUCTURE_SIGNATURE),
    'default-seed expected signatures are public and exact');
  Check((not TryBuilding3DShowcaseExpected(1, E)) and
    (E.PipelineSignature = '') and
    (E.StructureSceneSignature = ''),
    'an unregistered seed has no invented golden signature');
end;

procedure TestShowcaseSolveVerification;
var
  B: TBuilding3D;
  R: TGraphSolveReport;
  V: TBuilding3DValidationReport;
begin
  B := NewSolvedBuilding3DShowcase(0, R);
  try
    VerifyBuilding3DShowcase(B, R, V);
    Check(B.PipelineSignature =
      BUILDING3D_SHOWCASE_SEED_ZERO_PIPELINE_SIGNATURE,
      'seed-zero showcase solve matches its pipeline golden');
    Check((V.Valid) and
      (V.CheckedCells = BUILDING3D_SHOWCASE_CELL_COUNT) and
      (V.FeatureCount = BUILDING3D_SHOWCASE_FEATURE_COUNT) and
      (V.PropCount = BUILDING3D_SHOWCASE_PROP_COUNT) and
      (V.Structure.ReachableCount =
        BUILDING3D_SHOWCASE_REACHABLE_COUNT),
      'shared verification returns exact independent evidence');
  finally
    B.Free;
  end;

  B := NewSolvedBuilding3DShowcase(
    BUILDING3D_SHOWCASE_DEFAULT_SEED, R);
  try
    VerifyBuilding3DShowcase(B, R, V);
    Check(B.PipelineSignature =
      BUILDING3D_SHOWCASE_DEFAULT_PIPELINE_SIGNATURE,
      'default showcase solve matches its pipeline golden');
  finally
    B.Free;
  end;
end;

procedure TestViewModesAndLineage;
var
  B: TBuilding3D;
  CompleteView: TBuilding3DView;
  EnvelopeView: TBuilding3DView;
  F: TBuilding3DViewFace;
  FootprintView: TBuilding3DView;
  I: Integer;
  PropFaceCount: Integer;
  R: TGraphSolveReport;
  StructureView: TBuilding3DView;
begin
  B := NewSolvedBuilding3DShowcase(0, R);
  FootprintView := nil;
  StructureView := nil;
  EnvelopeView := nil;
  CompleteView := nil;
  try
    FootprintView := BuildBuilding3DView(B, b3vmFootprint);
    StructureView := BuildBuilding3DView(B, b3vmStructure);
    EnvelopeView := BuildBuilding3DView(B, b3vmEnvelopeRoof);
    CompleteView := BuildBuilding3DView(B);
    Check((WFC_BUILDING3D_VIEW_VERSION = 1) and
      (WFC_BUILDING3D_VIEW_GEOMETRY_VERSION = 1) and
      (WFC_BUILDING3D_VIEW_STYLE_VERSION = 1) and
      (WFC_BUILDING3D_VIEW_SUBCELL_SCALE = 1024),
      'view geometry and style contracts are version one');
    Check((FootprintView.FaceCount = 110) and
      (StructureView.FaceCount =
        BUILDING3D_SHOWCASE_STRUCTURE_QUAD_COUNT) and
      (EnvelopeView.FaceCount = StructureView.FaceCount) and
      (CompleteView.FaceCount = StructureView.FaceCount + 6),
      'view modes expose exact massing, structure, decoration, and prop counts');
    Check((FootprintView.Mode = b3vmFootprint) and
      (StructureView.Mode = b3vmStructure) and
      (EnvelopeView.Mode = b3vmEnvelopeRoof) and
      (CompleteView.Mode = b3vmComplete),
      'each immutable view retains its selected mode');
    Check((CompleteView.Width = BUILDING3D_SHOWCASE_WIDTH) and
      (CompleteView.Height = BUILDING3D_SHOWCASE_HEIGHT) and
      (CompleteView.Depth = BUILDING3D_SHOWCASE_DEPTH) and
      (CompleteView.Seed = 0) and
      (CompleteView.BuildingSignature = B.Signature) and
      (CompleteView.PipelineSignature = B.PipelineSignature),
      'complete view retains public source identity');
    Check((Building3DViewModeName(b3vmFootprint) = 'footprint') and
      (Building3DViewModeName(b3vmStructure) = 'structure') and
      (Building3DViewModeName(b3vmEnvelopeRoof) = 'envelope-roof') and
      (Building3DViewModeName(b3vmComplete) = 'complete'),
      'view modes have stable public names');

    for I := 0 to FootprintView.FaceCount - 1 do
    begin
      F := FootprintView.FaceAt(I);
      Check(F.GeometryKind = b3vgFootprint,
        'footprint view contains only footprint geometry');
      Check((F.Quad.LayerId = 'footprint') and
        (F.Quad.LayerOrder = Ord(b3sFootprint)) and
        (F.Quad.Semantic = F.Lineage.FootprintToken) and
        (F.Lineage.FootprintRole <> b3frVoid),
        'footprint face uses its public role and layer');
      CheckPortableQuad(F.Quad, CompleteView.Width,
        CompleteView.Height, CompleteView.Depth, 'footprint face');
      CheckLineage(B, F, 'footprint face');
    end;

    for I := 0 to StructureView.FaceCount - 1 do
    begin
      F := StructureView.FaceAt(I);
      Check(F.GeometryKind = b3vgStructure,
        'structure view contains only structure geometry');
      Check((F.Quad.LayerId = 'structure') and
        (F.Quad.LayerOrder = Ord(b3sStructure)) and
        (F.Quad.PrototypeId = F.Lineage.StructurePrototypeId) and
        (F.Quad.Material = F.Lineage.StructureMaterial) and
        (F.Quad.Semantic = F.Lineage.StructurePrototypeId),
        'structure face uses public structure semantics');
      CheckPortableQuad(F.Quad, CompleteView.Width,
        CompleteView.Height, CompleteView.Depth, 'structure face');
      CheckLineage(B, F, 'structure face');
    end;

    for I := 0 to EnvelopeView.FaceCount - 1 do
    begin
      F := EnvelopeView.FaceAt(I);
      Check(F.GeometryKind = b3vgStructure,
        'envelope view retains structure as its geometry source');
      Check(VerticesEqual(F.Quad.Vertices,
          StructureView.FaceAt(I).Quad.Vertices) and
        (F.Quad.CellX = StructureView.FaceAt(I).Quad.CellX) and
        (F.Quad.CellY = StructureView.FaceAt(I).Quad.CellY) and
        (F.Quad.CellZ = StructureView.FaceAt(I).Quad.CellZ) and
        (F.Quad.Direction = StructureView.FaceAt(I).Quad.Direction),
        'envelope decoration uses the exact matching structure face');
      Check((F.Quad.LayerId = 'envelope-roof') and
        (F.Quad.LayerOrder = Ord(b3sEnvelopeRoof)) and
        (F.Quad.PrototypeId = F.Lineage.StructurePrototypeId) and
        (F.Quad.Material = F.Lineage.EnvelopeMaterial) and
        (F.Quad.Semantic = F.Lineage.EnvelopePrototypeId) and
        (F.Lineage.EnvelopeKind <> b3ekNone),
        'envelope decoration carries public overlay semantics');
      CheckLineage(B, F, 'envelope face');
    end;

    PropFaceCount := 0;
    for I := 0 to CompleteView.FaceCount - 1 do
    begin
      F := CompleteView.FaceAt(I);
      if I < EnvelopeView.FaceCount then
        Check(QuadsEqual(F.Quad, EnvelopeView.FaceAt(I).Quad) and
          (F.GeometryKind = b3vgStructure),
          'complete view begins with exact envelope-decorated structure')
      else
      begin
        Inc(PropFaceCount);
        Check((F.GeometryKind = b3vgProp) and
          (F.Quad.LayerId = 'props') and
          (F.Quad.LayerOrder = Ord(b3sProps)) and
          (F.Quad.PrototypeId = F.Lineage.PropPrototypeId) and
          (F.Quad.Material = F.Lineage.PropMaterial) and
          (F.Quad.Semantic = F.Lineage.PropPrototypeId) and
          (F.Lineage.PropKind <> b3pkNone),
          'complete view appends public prop geometry');
        Check((F.Quad.CellX = BUILDING3D_SHOWCASE_FEATURE_X) and
          (F.Quad.CellY = BUILDING3D_SHOWCASE_FEATURE_Y) and
          (F.Quad.CellZ = BUILDING3D_SHOWCASE_FEATURE_Z),
          'every prop face belongs to the feature cell');
      end;
      CheckPortableQuad(F.Quad, CompleteView.Width,
        CompleteView.Height, CompleteView.Depth, 'complete face');
      CheckLineage(B, F, 'complete face');
    end;
    Check(PropFaceCount = 6,
      'the one solid showcase prop contributes one exposed cube');

    for I := 0 to EnvelopeView.FaceCount - 1 do
    begin
      F := EnvelopeView.FaceAt(I);
      Check(not ((F.Quad.CellX = BUILDING3D_SHOWCASE_ENTRANCE_X) and
        (F.Quad.CellY = BUILDING3D_SHOWCASE_ENTRANCE_Y) and
        (F.Quad.CellZ = BUILDING3D_SHOWCASE_ENTRANCE_Z)),
        'door trim does not invent a solid face for the empty entrance');
    end;
  finally
    CompleteView.Free;
    EnvelopeView.Free;
    StructureView.Free;
    FootprintView.Free;
    B.Free;
  end;
end;

procedure TestExactMeshComposition;
var
  B: TBuilding3D;
  CompleteView: TBuilding3DView;
  F: TBuilding3DViewFace;
  I, J: Integer;
  M: TVoxel3DMesh;
  MQ: TVoxel3DMeshQuad;
  R: TGraphSolveReport;
  S: TVoxel3DScene;
  StructureView: TBuilding3DView;
begin
  B := NewSolvedBuilding3DShowcase(0, R);
  CompleteView := nil;
  StructureView := nil;
  S := nil;
  M := nil;
  try
    StructureView := BuildBuilding3DView(B, b3vmStructure);
    S := B.CaptureStructureScene;
    M := BuildVoxel3DMesh(B.StructureKit, S);
    Check(StructureView.FaceCount = M.QuadCount,
      'structure view consumes every structure mesh quad once');
    for I := 0 to M.QuadCount - 1 do
    begin
      MQ := M.QuadAt(I);
      F := StructureView.FaceAt(I);
      Check((F.Quad.CellX = MQ.CellX) and
        (F.Quad.CellY = MQ.CellY) and
        (F.Quad.CellZ = MQ.CellZ) and
        (F.Quad.Direction = MQ.Direction) and
        (F.Quad.Rotation = MQ.Rotation) and
        (F.Quad.PrototypeId = MQ.PrototypeId) and
        (F.Quad.Material = MQ.Material),
        'structure view preserves mesh ordering and public metadata');
      for J := Low(MQ.Vertices) to High(MQ.Vertices) do
        Check((F.Quad.Vertices[J].X = MQ.Vertices[J].X *
            WFC_VOXEL3D_SUBCELL_SCALE) and
          (F.Quad.Vertices[J].Y = MQ.Vertices[J].Y *
            WFC_VOXEL3D_SUBCELL_SCALE) and
          (F.Quad.Vertices[J].Z = MQ.Vertices[J].Z *
            WFC_VOXEL3D_SUBCELL_SCALE),
          'structure mesh vertices scale exactly to fixed subcells');
    end;
    M.Free;
    M := nil;
    S.Free;
    S := nil;

    CompleteView := BuildBuilding3DView(B, b3vmComplete);
    S := B.CapturePropsScene;
    M := BuildVoxel3DMesh(B.PropsKit, S);
    Check(M.QuadCount = CompleteView.FaceCount -
      BUILDING3D_SHOWCASE_STRUCTURE_QUAD_COUNT,
      'complete view appends every prop mesh quad once');
    for I := 0 to M.QuadCount - 1 do
    begin
      MQ := M.QuadAt(I);
      F := CompleteView.FaceAt(
        BUILDING3D_SHOWCASE_STRUCTURE_QUAD_COUNT + I);
      Check((F.Quad.CellX = MQ.CellX) and
        (F.Quad.CellY = MQ.CellY) and
        (F.Quad.CellZ = MQ.CellZ) and
        (F.Quad.Direction = MQ.Direction) and
        (F.Quad.Rotation = MQ.Rotation) and
        (F.Quad.PrototypeId = MQ.PrototypeId) and
        (F.Quad.Material = MQ.Material),
        'complete view preserves prop mesh metadata');
      for J := Low(MQ.Vertices) to High(MQ.Vertices) do
        Check((F.Quad.Vertices[J].X = MQ.Vertices[J].X *
            WFC_VOXEL3D_SUBCELL_SCALE) and
          (F.Quad.Vertices[J].Y = MQ.Vertices[J].Y *
            WFC_VOXEL3D_SUBCELL_SCALE) and
          (F.Quad.Vertices[J].Z = MQ.Vertices[J].Z *
            WFC_VOXEL3D_SUBCELL_SCALE),
          'prop mesh vertices scale exactly to fixed subcells');
    end;
  finally
    M.Free;
    S.Free;
    CompleteView.Free;
    StructureView.Free;
    B.Free;
  end;
end;

procedure TestStylesAndImmutability;
var
  B: TBuilding3D;
  F: TBuilding3DViewFace;
  Faces: TBuilding3DViewFaces;
  OriginalLayer: String;
  OriginalX: Integer;
  Quads: TVoxel3DViewQuads;
  R: TGraphSolveReport;
  Raised: Boolean;
  Style: TBuilding3DViewStyle;
  V: TBuilding3DView;
begin
  Style := Building3DFootprintViewStyle(b3frVoid);
  Check((Style.FillColor.A = 0) and (Style.EdgeColor.A = 0),
    'void footprint style is transparent');
  Style := Building3DStructureViewStyle(b3skWindow);
  Check((Style.FillColor.A > 0) and (Style.FillColor.A < 255) and
    (Style.EdgeColor.A = 255),
    'window style has translucent fill and opaque edge');
  Style := Building3DEnvelopeViewStyle(b3ekRoofFinish);
  Check((Style.FillColor.R > Style.FillColor.G) and
    (Style.FillColor.A = 255),
    'roof finish has a stable opaque warm style');
  Style := Building3DPropViewStyle(b3pkPlant);
  Check((Style.FillColor.G > Style.FillColor.R) and
    (Style.FillColor.G > Style.FillColor.B),
    'plant style has a stable green fill');

  B := NewSolvedBuilding3DShowcase(0, R);
  V := nil;
  try
    V := BuildBuilding3DView(B);
    F := V.FaceAt(0);
    OriginalLayer := F.Quad.LayerId;
    OriginalX := F.Quad.Vertices[0].X;
    Faces := V.CopyFaces;
    Quads := V.CopyQuads;
    Faces[0].Quad.LayerId := 'changed';
    Faces[0].Quad.Vertices[0].X := -1;
    Quads[0].LayerId := 'changed';
    Quads[0].Vertices[0].X := -1;
    Check((V.FaceAt(0).Quad.LayerId = OriginalLayer) and
      (V.FaceAt(0).Quad.Vertices[0].X = OriginalX),
      'face and generic-quad copies are detached from the immutable view');

    B.SetFootprintRole(BUILDING3D_SHOWCASE_FEATURE_X,
      BUILDING3D_SHOWCASE_FEATURE_Y,
      BUILDING3D_SHOWCASE_FEATURE_Z, b3frInterior);
    Check((V.FaceCount = 140) and
      (V.PipelineSignature =
        BUILDING3D_SHOWCASE_SEED_ZERO_PIPELINE_SIGNATURE) and
      (V.FaceAt(0).Quad.LayerId = OriginalLayer),
      'captured view survives later source mutation independently');

    Raised := False;
    try
      F := V.FaceAt(-1);
    except
      on E: ERangeError do Raised := True;
    end;
    Check(Raised, 'negative view face indices are rejected');
    Raised := False;
    try
      F := V.FaceAt(V.FaceCount);
    except
      on E: ERangeError do Raised := True;
    end;
    Check(Raised, 'past-end view face indices are rejected');
  finally
    V.Free;
    B.Free;
  end;
end;

procedure TestViewGuards;
var
  B: TBuilding3D;
  BadMode: Integer;
  Blueprint: TBuilding3DBlueprint;
  R: TGraphSolveReport;
  Raised: Boolean;
  V: TBuilding3DView;
begin
  V := nil;
  Raised := False;
  try
    V := BuildBuilding3DView(nil);
  except
    on E: EArgumentNilException do Raised := True;
  end;
  V.Free;
  Check(Raised, 'nil buildings are rejected');

  Blueprint := NewBuilding3DShowcaseBlueprint;
  B := TBuilding3D.Create(Blueprint);
  Blueprint.Free;
  try
    Raised := False;
    try
      V := BuildBuilding3DView(B);
    except
      on E: EBuilding3DView do Raised := True;
    end;
    V.Free;
    V := nil;
    Check(Raised, 'unsolved buildings are rejected');
  finally
    B.Free;
  end;

  B := NewSolvedBuilding3DShowcase(0, R);
  try
    B.Graph.SwitchToPass('unexpected-extra-pass');
    Raised := False;
    try
      V := BuildBuilding3DView(B);
    except
      on E: EBuilding3DView do Raised := True;
    end;
    V.Free;
    V := nil;
    Check(Raised, 'noncanonical pipeline definitions are rejected');
  finally
    B.Free;
  end;

  BadMode := 99;
  Raised := False;
  try
    Check(Building3DViewModeName(TBuilding3DViewMode(BadMode)) = '',
      'invalid view mode cannot have a name');
  except
    on E: ERangeError do Raised := True;
  end;
  Check(Raised, 'invalid view modes are rejected');
end;

begin
  WriteLn('WFC Building 3D view conformance suite');
  WriteLn('======================================');
  RunTest('shared showcase blueprint and expected signatures',
    @TestShowcaseBlueprintAndExpected);
  RunTest('shared showcase solve verification',
    @TestShowcaseSolveVerification);
  RunTest('view modes and complete lineage',
    @TestViewModesAndLineage);
  RunTest('exact structure and prop mesh composition',
    @TestExactMeshComposition);
  RunTest('styles and immutable snapshots',
    @TestStylesAndImmutability);
  RunTest('view construction guards', @TestViewGuards);
  WriteLn('======================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));
  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d Building 3D view checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
