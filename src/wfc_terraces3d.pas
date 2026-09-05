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
unit wfc_terraces3d;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, wfc, wfc_model, wfc_voxel3d;

const
  WFC_TERRACES3D_VERSION = 1;

type
  ETerraces3D = class(Exception);
  TTerraces3DStage = (t3sTerrain, t3sStructure, t3sFoliage);

  { Immutable, detached public output. Owned by the caller. No graph keys. }
  TTerraces3DScene = class
  private
    FTokens: TWfcModelTokens;
    FStructure, FFoliage: TVoxel3DScene;
    constructor Create;
    function IndexOf(const AX, AY, AZ: Integer): Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetDepth: Integer;
  public
    destructor Destroy; override;
    function TerrainAt(const AX, AY, AZ: Integer): TWfcModelToken;
    function Signature: String;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Depth: Integer read GetDepth;
    { Borrowed immutable scenes. Do not free separately. }
    property Structure: TVoxel3DScene read FStructure;
    property Foliage: TVoxel3DScene read FFoliage;
  end;

  { Learned height volumes -> socket/support kit -> spatially supported plants.
    Only floor and ceiling are pinned; intermediate cells are generated.
    Output is captured and independently checked before the graph can commit. }
  TTerraces3D = class
  private
    FGraph: TGraph;
    FModel: TWfcModel;
    FStructureKit, FFoliageKit: TVoxel3DKit;
    FStructureAdapter, FFoliageAdapter: TVoxel3DGraphAdapter;
    FPending: TTerraces3DScene;
    FWidth, FHeight, FDepth: Integer;
    FBaseline: Boolean;
    FDirtyStage, FActiveStage: Integer;
    FValidationMessage: String;
    procedure Configure;
    procedure RequireIdle;
    procedure CheckCoordinate(const AX, AY, AZ: Integer);
    procedure MarkDirty(const AStage: TTerraces3DStage);
    function CaptureCandidate: TTerraces3DScene;
    procedure ProjectReport(var AReport: TGraphNegotiationReport);
    function ValidateCommit(out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
    function Run(const AStage: TTerraces3DStage; const AFull: Boolean;
      const AOptions: TGraphNegotiationOptions;
      out AScene: TTerraces3DScene; out AReport: TGraphNegotiationReport): Boolean;
  strict protected
    { Extra application policy runs inside entry/RNG rollback. The candidate is
      borrowed for this call; never free it or mutate graph state here. }
    function DoAcceptCandidate(const AScene: TTerraces3DScene): Boolean; virtual;
  public
    constructor Create(const AWidth, AHeight, ADepth: Integer; const ASeed: TGraphSeed);
    destructor Destroy; override;
    function SetSeed(const ASeed: TGraphSeed): TTerraces3D;
    { Empty token clears a user domain, restoring the floor/ceiling policy. }
    function SetTerrainToken(const AX, AY, AZ: Integer;
      const AToken: TWfcModelToken): TTerraces3D;
    { '', 'none', 'fern', or 'flowers'. Empty clears the user domain. }
    function SetFoliage(const AX, AY, AZ: Integer;
      const APrototypeId: String): TTerraces3D;
    function TryGenerate(const AOptions: TGraphNegotiationOptions;
      out AScene: TTerraces3DScene; out AReport: TGraphNegotiationReport): Boolean;
    function TryRegenerateFrom(const AStage: TTerraces3DStage;
      const AOptions: TGraphNegotiationOptions;
      out AScene: TTerraces3DScene; out AReport: TGraphNegotiationReport): Boolean;
    function Validate(const AScene: TTerraces3DScene; out AMessage: String): Boolean;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Depth: Integer read FDepth;
    { Immutable resources, borrowed until this owner is freed. }
    property Model: TWfcModel read FModel;
    property StructureKit: TVoxel3DKit read FStructureKit;
    property FoliageKit: TVoxel3DKit read FFoliageKit;
    property HasBaseline: Boolean read FBaseline;
    property DirtyStage: Integer read FDirtyStage;
    property ValidationMessage: String read FValidationMessage;
  end;

function Terraces3DStageName(const AStage: TTerraces3DStage): String;
function NewTerraces3DModel: TWfcModel;
function DefaultTerraces3DOptions: TGraphNegotiationOptions;

implementation

uses
  wfc_learn3d, wfc_voxel3d_passes, wfc_voxel3d_validate;

type
  TTerraces3DCommitGraph = class(TGraph)
  private
    FOwner: TTerraces3D;
  strict protected
    function DoValidateCommit(out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean; override;
  end;

function TTerraces3DCommitGraph.DoValidateCommit(
  out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
begin
  Result := FOwner.ValidateCommit(AFailedPassIndex, AFailedEntryIndex);
end;

function Terraces3DStageName(const AStage: TTerraces3DStage): String;
begin
  case AStage of
    t3sTerrain: Result := 'terrain';
    t3sStructure: Result := 'structure';
    t3sFoliage: Result := 'foliage';
  else raise ERangeError.Create('unknown terrace stage');
  end;
end;

function DefaultTerraces3DOptions: TGraphNegotiationOptions;
begin
  Result := DefaultGraphNegotiationOptions;
  Result.SolveOptions.MaxBacktracks := 1024;
  Result.MaxPassBacktracks := 64;
end;

function NewTerraces3DModel: TWfcModel;
const
  HEIGHTS: array[0..1, 0..8] of Integer =
    ((2,3,4, 3,4,3, 4,3,2), (3,3,2, 4,3,3, 3,2,2));
var
  Samples: TWfcLearnVolumeSamples;
  Tokens: TWfcModelTokens;
  S, I, Z: Integer;
begin
  SetLength(Samples, 2);
  SetLength(Tokens, 45);
  for S := 0 to 1 do
  begin
    for Z := 0 to 4 do
      for I := 0 to 8 do
        if Z = 0 then Tokens[Z * 9 + I] := 'ground'
        else if Z < HEIGHTS[S, I] - 1 then Tokens[Z * 9 + I] := 'rock'
        else if Z = HEIGHTS[S, I] - 1 then Tokens[Z * 9 + I] := 'soil'
        else Tokens[Z * 9 + I] := 'air';
    Samples[S] := MakeLearnSample3D(Tokens, 3, 3, 5);
  end;
  { D4 keeps gravity. Cube rotations would incorrectly teach soil underneath
    rock; scalar token rotation is not semantic gravity transformation. }
  Result := LearnModel3DCorpus(Samples, wmbOpen, wmsD4);
end;

function NewStructureKit: TVoxel3DKit;
var
  P: TVoxel3DPrototypes;
  Pairs: TVoxel3DSocketPairs;
  Load, Soil, Air: TVoxel3DSockets;
begin
  Load := MakeVoxel3DSockets('any','any','any','any','load','load');
  Soil := MakeVoxel3DSockets('any','any','any','any','open','load');
  Air := MakeVoxel3DSockets('any','any','any','any','open','open');
  SetLength(P, 6);
  P[0] := MakeVoxel3DPrototype('air','none',1,Air,[v3r0],[v3pfEmpty],[]);
  P[1] := MakeVoxel3DPrototype('bedrock','bedrock',1,Load,[v3r0],
    [v3pfSolid,v3pfProvidesSupport],[]);
  P[2] := MakeVoxel3DPrototype('stone','stone',3,Load,[v3r0],
    [v3pfSolid,v3pfRequiresSupport,v3pfProvidesSupport],[]);
  P[3] := MakeVoxel3DPrototype('basalt','basalt',1,Load,[v3r0],
    [v3pfSolid,v3pfRequiresSupport,v3pfProvidesSupport],[]);
  P[4] := MakeVoxel3DPrototype('grass','grass',3,Soil,[v3r0],
    [v3pfSolid,v3pfRequiresSupport,v3pfProvidesSupport],[]);
  P[5] := MakeVoxel3DPrototype('flowerbed','soil',1,Soil,[v3r0],
    [v3pfSolid,v3pfRequiresSupport,v3pfProvidesSupport],[]);
  SetLength(Pairs, 3);
  Pairs[0] := MakeVoxel3DSocketPair('any','any');
  Pairs[1] := MakeVoxel3DSocketPair('load','load');
  Pairs[2] := MakeVoxel3DSocketPair('open','open');
  Result := TVoxel3DKit.Create('terraces-structure-v1',P,Pairs);
end;

function NewFoliageKit: TVoxel3DKit;
var P: TVoxel3DPrototypes; Pairs: TVoxel3DSocketPairs; S: TVoxel3DSockets;
begin
  S := MakeVoxel3DSockets('any','any','any','any','any','any');
  SetLength(P, 3);
  P[0] := MakeVoxel3DPrototype('none','none',2,S,[v3r0],[v3pfEmpty],[]);
  { Support comes from the STRUCTURE pass, not another plant beneath this one.
    v3pfRequiresSupport denotes same-kit support and is intentionally absent. }
  P[1] := MakeVoxel3DPrototype('fern','fern',3,S,[v3r0],[v3pfSolid],[]);
  P[2] := MakeVoxel3DPrototype('flowers','flowers',3,S,[v3r0],[v3pfSolid],[]);
  SetLength(Pairs, 1); Pairs[0] := MakeVoxel3DSocketPair('any','any');
  Result := TVoxel3DKit.Create('terraces-foliage-v1',P,Pairs);
end;

function Selector(const AId: String): TVoxel3DPassVariantSelector;
begin
  Result := MakeVoxel3DPassVariantSelector(AId,[v3r0]);
end;

procedure CheckTerraceSeed(const ASeed: TGraphSeed);
begin
  {$IFDEF PAS2JS}
  if (ASeed <> Trunc(ASeed)) or (ASeed < 0) or (ASeed > High(TGraphSeed)) then
    raise ERangeError.Create('terrace seed must be an unsigned 32-bit integer');
  {$ENDIF}
end;

constructor TTerraces3D.Create(const AWidth, AHeight, ADepth: Integer;
  const ASeed: TGraphSeed);
begin
  inherited Create;
  CheckTerraceSeed(ASeed);
  {$IFDEF PAS2JS}
  if (AWidth <> Trunc(AWidth)) or (AHeight <> Trunc(AHeight)) or
    (ADepth <> Trunc(ADepth)) then
    raise ERangeError.Create('terrace dimensions must be exact integers');
  {$ENDIF}
  if (AWidth < 1) or (AHeight < 1) or (ADepth < 3) then
    raise ERangeError.Create('terraces need positive width/height and depth at least 3');
  if (AWidth > High(Integer) div AHeight) or
    (AWidth * AHeight > High(Integer) div ADepth) then
    raise ERangeError.Create('terrace cell count exceeds Integer');
  FWidth := AWidth; FHeight := AHeight; FDepth := ADepth;
  FDirtyStage := 0;
  FModel := NewTerraces3DModel;
  FStructureKit := NewStructureKit;
  FFoliageKit := NewFoliageKit;
  FGraph := TTerraces3DCommitGraph.Create;
  TTerraces3DCommitGraph(FGraph).FOwner := Self;
  FGraph.Seed := ASeed;
  Configure;
end;

destructor TTerraces3D.Destroy;
begin
  FPending.Free;
  FFoliageAdapter.Free; FStructureAdapter.Free;
  FGraph.Free;
  FFoliageKit.Free; FStructureKit.Free; FModel.Free;
  inherited Destroy;
end;

procedure TTerraces3D.Configure;
const
  SOURCE: array[0..5] of String = ('air','ground','rock','rock','soil','soil');
var
  I, X, Y: Integer;
  M: TVoxel3DModelPassProjectionRules;
  P: TVoxel3DPassProjectionRules;
  C: TVoxel3DPassSpatialClauses;
  S: TVoxel3DPassVariantSelectors;
  T: TWfcModelTokens;
begin
  FGraph.Reshape(FWidth,FHeight,FDepth);
  FGraph.WrapNeighbors := False;
  FGraph.CurrentPass := 'terrain';
  FGraph.PassMode := gpmOverlay;
  ApplyModelToGraph(FModel,FGraph);
  for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1 do
    begin
      FGraph.SetAllowedValues(X,Y,0,'ground');
      FGraph.SetAllowedValues(X,Y,FDepth-1,'air');
    end;
  FGraph.SwitchToPass('structure');
  FGraph.PassMode := gpmOverlay;
  FStructureAdapter := FStructureKit.ApplyToGraph(FGraph);
  SetLength(M,6); SetLength(T,1);
  for I := 0 to 5 do
  begin
    T[0] := TWfcModelToken(SOURCE[I]);
    M[I] := MakeVoxel3DModelPassProjectionRule(
      Selector(FStructureKit.PrototypeAt(I).Id), T);
  end;
  RequireVoxel3DProjectionFromModelPass(FStructureKit,FStructureAdapter,FModel,0,M);
  FGraph.SwitchToPass('foliage');
  FGraph.PassMode := gpmOverlay;
  FFoliageAdapter := FFoliageKit.ApplyToGraph(FGraph);
  SetLength(P,3); SetLength(S,6);
  for I := 0 to 5 do S[I] := Selector(FStructureKit.PrototypeAt(I).Id);
  P[0] := MakeVoxel3DPassProjectionRule(Selector('none'),S);
  SetLength(S,1); S[0] := Selector('air');
  P[1] := MakeVoxel3DPassProjectionRule(Selector('fern'),S);
  P[2] := MakeVoxel3DPassProjectionRule(Selector('flowers'),S);
  SetLength(C,2);
  for I := 0 to 1 do
  begin
    C[I].OffsetFrame := v3pofWorld;
    SetLength(C[I].Terms,1);
    C[I].Terms[0].Offset := MakeGraphOffset(0,0,-1);
    SetLength(C[I].Terms[0].AllowedSources,1);
  end;
  C[0].Target := Selector('fern');
  C[0].Terms[0].AllowedSources[0] := Selector('grass');
  C[1].Target := Selector('flowers');
  C[1].Terms[0].AllowedSources[0] := Selector('flowerbed');
  ValidateVoxel3DProjectionFromPass(FFoliageKit,FFoliageAdapter,
    FStructureKit,FStructureAdapter,P);
  ValidateVoxel3DSpatialClausesFromPass(FFoliageKit,FFoliageAdapter,
    FStructureKit,FStructureAdapter,C);
  RequireVoxel3DProjectionFromPass(FFoliageKit,FFoliageAdapter,
    FStructureKit,FStructureAdapter,P);
  RequireVoxel3DSpatialClausesFromPass(FFoliageKit,FFoliageAdapter,
    FStructureKit,FStructureAdapter,C);
end;

procedure TTerraces3D.RequireIdle;
begin
  if FGraph.Running then
    raise EInvalidOperation.Create('cannot change a running terrace composition');
end;

procedure TTerraces3D.CheckCoordinate(const AX, AY, AZ: Integer);
begin
  {$IFDEF PAS2JS}
  if (AX <> Trunc(AX)) or (AY <> Trunc(AY)) or (AZ <> Trunc(AZ)) then
    raise ERangeError.Create('terrace coordinates must be exact integers');
  {$ENDIF}
  if (AX < 0) or (AY < 0) or (AZ < 0) or
    (AX >= FWidth) or (AY >= FHeight) or (AZ >= FDepth) then
    raise ERangeError.Create('terrace coordinate out of bounds');
end;

procedure TTerraces3D.MarkDirty(const AStage: TTerraces3DStage);
begin
  if (FDirtyStage < 0) or (Ord(AStage) < FDirtyStage) then
    FDirtyStage := Ord(AStage);
end;

function TTerraces3D.SetSeed(const ASeed: TGraphSeed): TTerraces3D;
begin
  RequireIdle; CheckTerraceSeed(ASeed); FGraph.Seed := ASeed; Result := Self;
end;

function TTerraces3D.SetTerrainToken(const AX, AY, AZ: Integer;
  const AToken: TWfcModelToken): TTerraces3D;
var T: TWfcModelToken;
begin
  RequireIdle; CheckCoordinate(AX,AY,AZ);
  T := AToken;
  if (T <> '') and (FModel.FindToken(T) < 0) then
    raise ETerraces3D.Create('unknown terrain token');
  if AZ = 0 then
  begin
    if (T <> '') and (T <> 'ground') then
      raise ETerraces3D.Create('the ground boundary requires ground');
    T := 'ground';
  end;
  if AZ = FDepth-1 then
  begin
    if (T <> '') and (T <> 'air') then
      raise ETerraces3D.Create('the ceiling boundary requires air');
    T := 'air';
  end;
  if T = '' then FGraph.PassGraph[0].ClearAllowedValues(AX,AY,AZ)
  else FGraph.PassGraph[0].SetAllowedValues(AX,AY,AZ,String(T));
  MarkDirty(t3sTerrain); Result := Self;
end;

function TTerraces3D.SetFoliage(const AX, AY, AZ: Integer;
  const APrototypeId: String): TTerraces3D;
var I, Found: Integer;
begin
  RequireIdle; CheckCoordinate(AX,AY,AZ);
  Found := -1;
  for I := 0 to FFoliageAdapter.VariantCount-1 do
    if FFoliageAdapter.VariantAt(I).PrototypeId = APrototypeId then Found := I;
  if (APrototypeId <> '') and (Found < 0) then
    raise ETerraces3D.Create('unknown foliage prototype');
  if APrototypeId = '' then FGraph.PassGraph[2].ClearAllowedValues(AX,AY,AZ)
  else FGraph.PassGraph[2].SetAllowedValues(AX,AY,AZ,
    FFoliageAdapter.VariantGraphKeyAt(Found));
  MarkDirty(t3sFoliage); Result := Self;
end;

function TTerraces3D.CaptureCandidate: TTerraces3DScene;
var
  I, X, Y, Z: Integer;
  Tokens: TWfcModelTokens;
  function Capture(const AAdapter: TVoxel3DGraphAdapter): TVoxel3DScene;
  var Indices: TVoxel3DVariantIndices; N, XX, YY, ZZ, V: Integer;
  begin
    { Ordinary adapter.Capture intentionally rejects Running. This internal
      owner hook reads the final candidate while the core still owns rollback. }
    if not AAdapter.DefinitionMatchesGraph then
      raise ETerraces3D.Create('terrace kit definition changed');
    SetLength(Indices,FWidth*FHeight*FDepth); N := 0;
    for ZZ := 0 to FDepth-1 do for YY := 0 to FHeight-1 do for XX := 0 to FWidth-1 do
    begin
      if AAdapter.AppliedGraph.Entry[XX,YY,ZZ].Empty or
        not AAdapter.FindVariantGraphKey(AAdapter.AppliedGraph.Entry[XX,YY,ZZ].Value,V) then
        raise ETerraces3D.Create('terrace candidate contains an unknown variant');
      Indices[N] := V; Inc(N);
    end;
    Result := TVoxel3DScene.Create(AAdapter,Indices);
  end;
begin
  Result := nil; SetLength(Tokens,FWidth*FHeight*FDepth); I := 0;
  for Z := 0 to FDepth-1 do for Y := 0 to FHeight-1 do for X := 0 to FWidth-1 do
  begin
    Tokens[I] := TWfcModelToken(FGraph.PassGraph[0].Entry[X,Y,Z].Value);
    Inc(I);
  end;
  Result := TTerraces3DScene.Create;
  try
    Result.FTokens := Tokens;
    Result.FStructure := Capture(FStructureAdapter);
    Result.FFoliage := Capture(FFoliageAdapter);
  except Result.Free; raise;
  end;
end;

function TTerraces3D.Validate(const AScene: TTerraces3DScene;
  out AMessage: String): Boolean;
const
  DX: array[0..5] of Integer = (0,1,0,-1,0,0);
  DY: array[0..5] of Integer = (-1,0,1,0,0,0);
  DZ: array[0..5] of Integer = (0,0,0,0,1,-1);
var
  X,Y,Z,NX,NY,NZ,D,V,N: Integer;
  T, Expected: TWfcModelToken;
  P, Plant, Below: String;
  Options: TVoxel3DValidationOptions;
  Report: TVoxel3DValidationReport;
begin
  Result := False; AMessage := 'scene shape or resource identity mismatch';
  if not Assigned(AScene) or not Assigned(AScene.Structure) or
    not Assigned(AScene.Foliage) then Exit;
  if (AScene.Width <> FWidth) or (AScene.Height <> FHeight) or
    (AScene.Depth <> FDepth) or (Length(AScene.FTokens) <> FWidth*FHeight*FDepth) or
    not FStructureKit.MatchesScene(AScene.Structure) or
    not FFoliageKit.MatchesScene(AScene.Foliage) or
    (AScene.Foliage.Width <> FWidth) or (AScene.Foliage.Height <> FHeight) or
    (AScene.Foliage.Depth <> FDepth) or AScene.Structure.WrapNeighbors or
    AScene.Foliage.WrapNeighbors then Exit;
  Options := DefaultVoxel3DValidationOptions;
  Options.RequireEntrance := False; Options.RequireBoundaryFacingEntrance := False;
  Options.CheckRequiredReachability := False;
  AMessage := 'structure socket/support validation failed';
  if not ValidateVoxel3DScene(FStructureKit,AScene.Structure,Options,Report) then Exit;
  AMessage := 'foliage socket validation failed';
  if not ValidateVoxel3DScene(FFoliageKit,AScene.Foliage,Options,Report) then Exit;
  for Z := 0 to FDepth-1 do for Y := 0 to FHeight-1 do for X := 0 to FWidth-1 do
  begin
    T := AScene.TerrainAt(X,Y,Z);
    V := FModel.FindToken(T);
    AMessage := 'terrain token or boundary mismatch';
    if (V < 0) or ((Z = 0) and (T <> 'ground')) or
      ((Z = FDepth-1) and (T <> 'air')) then Exit;
    for D := 0 to 5 do
    begin
      NX := X+DX[D]; NY := Y+DY[D]; NZ := Z+DZ[D];
      if (NX < 0) or (NY < 0) or (NZ < 0) or
        (NX >= FWidth) or (NY >= FHeight) or (NZ >= FDepth) then Continue;
      N := FModel.FindToken(AScene.TerrainAt(NX,NY,NZ));
      AMessage := 'unobserved terrain adjacency';
      if (N < 0) or (FModel.RelationCount(TWfcModelDirection(D),V,N) <= 0) then Exit;
    end;
    P := AScene.Structure.VariantAt(AScene.Structure.VariantIndexAt(X,Y,Z)).PrototypeId;
    Expected := '';
    if P = 'air' then Expected := 'air'
    else if P = 'bedrock' then Expected := 'ground'
    else if (P = 'stone') or (P = 'basalt') then Expected := 'rock'
    else if (P = 'grass') or (P = 'flowerbed') then Expected := 'soil';
    AMessage := 'terrain-to-structure projection mismatch';
    if (Expected = '') or (T <> Expected) then Exit;
    Plant := AScene.Foliage.VariantAt(AScene.Foliage.VariantIndexAt(X,Y,Z)).PrototypeId;
    if Plant = 'none' then Continue;
    AMessage := 'foliage occupies solid terrain or lacks its supporting surface';
    if (P <> 'air') or (Z = 0) then Exit;
    Below := AScene.Structure.VariantAt(AScene.Structure.VariantIndexAt(X,Y,Z-1)).PrototypeId;
    if ((Plant = 'fern') and (Below = 'grass')) or
      ((Plant = 'flowers') and (Below = 'flowerbed')) then Continue;
    Exit;
  end;
  AMessage := ''; Result := True;
end;

function TTerraces3D.DoAcceptCandidate(const AScene: TTerraces3DScene): Boolean;
begin
  Result := True;
end;

function TTerraces3D.ValidateCommit(
  out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
begin
  AFailedPassIndex := FActiveStage; AFailedEntryIndex := -1;
  FreeAndNil(FPending);
  FPending := CaptureCandidate;
  Result := Validate(FPending,FValidationMessage);
  if Result then
  begin
    Result := DoAcceptCandidate(FPending);
    if not Result then FValidationMessage := 'application candidate policy rejected the composition';
  end;
  if not Result then FreeAndNil(FPending);
end;

procedure TTerraces3D.ProjectReport(var AReport: TGraphNegotiationReport);
var
  I: Integer;
  procedure Project(var R: TGraphSolveReport);
  var J, V: Integer;
  begin
    for J := 0 to High(R.Trace) do
    begin
      V := R.Trace[J].ValueIndex;
      R.Trace[J].Value := '';
      if V < 0 then Continue;
      case R.Trace[J].PassIndex of
        0: R.Trace[J].Value := String(FModel.TokenAt(V));
        1: R.Trace[J].Value := FStructureAdapter.VariantAt(V).PrototypeId;
        2: R.Trace[J].Value := FFoliageAdapter.VariantAt(V).PrototypeId;
      end;
    end;
  end;
begin
  { Trace/transcript signatures use stable indices, never display strings.
    Both rejected and final rounds expose public tokens/prototypes only. }
  for I := 0 to High(AReport.Attempts) do Project(AReport.Attempts[I].SolveReport);
  Project(AReport.FinalReport);
end;

function TTerraces3D.Run(const AStage: TTerraces3DStage; const AFull: Boolean;
  const AOptions: TGraphNegotiationOptions;
  out AScene: TTerraces3DScene; out AReport: TGraphNegotiationReport): Boolean;
var
  Selective: TGraphSelectiveNegotiationReport;
begin
  AScene := nil; AReport := Default(TGraphNegotiationReport);
  RequireIdle; Terraces3DStageName(AStage);
  {$IFDEF PAS2JS}
  if (AOptions.SolveOptions.MaxBacktracks <> Trunc(AOptions.SolveOptions.MaxBacktracks)) or
    (AOptions.SolveOptions.MaxBacktracks < 0) or
    (AOptions.SolveOptions.MaxBacktracks > High(Integer)) or
    (AOptions.MaxPassBacktracks <> Trunc(AOptions.MaxPassBacktracks)) or
    (AOptions.MaxPassBacktracks < 0) or (AOptions.MaxPassBacktracks > High(Integer)) then
    raise ERangeError.Create('terrace search budgets must be nonnegative Integer values');
  {$ENDIF}
  if not AFull then
  begin
    if not FBaseline then
      raise EInvalidOperation.Create('generate a baseline before selective terrace regeneration');
    if (FDirtyStage >= 0) and (FDirtyStage < Ord(AStage)) then
      raise EInvalidOperation.Create('selective scope excludes changed terrace inputs');
  end;
  FActiveStage := Ord(AStage); FValidationMessage := '';
  FreeAndNil(FPending);
  try
    if AFull then Result := FGraph.TrySolveNegotiated(AOptions,AReport)
    else
    begin
      Result := FGraph.TryRegenerateNegotiatedFrom(Terraces3DStageName(AStage),AOptions,Selective);
      AReport := Selective.Search;
    end;
    ProjectReport(AReport);
    if Result then
    begin
      if not Assigned(FPending) then raise ETerraces3D.Create('successful solve lacks a validated capture');
      AScene := FPending; FPending := nil; FBaseline := True; FDirtyStage := -1;
    end;
  finally
    FreeAndNil(FPending);
  end;
end;

function TTerraces3D.TryGenerate(const AOptions: TGraphNegotiationOptions;
  out AScene: TTerraces3DScene; out AReport: TGraphNegotiationReport): Boolean;
begin
  Result := Run(t3sTerrain,True,AOptions,AScene,AReport);
end;

function TTerraces3D.TryRegenerateFrom(const AStage: TTerraces3DStage;
  const AOptions: TGraphNegotiationOptions;
  out AScene: TTerraces3DScene; out AReport: TGraphNegotiationReport): Boolean;
begin
  Result := Run(AStage,False,AOptions,AScene,AReport);
end;

constructor TTerraces3DScene.Create;
begin
  inherited Create;
end;

destructor TTerraces3DScene.Destroy;
begin
  FFoliage.Free; FStructure.Free;
  inherited Destroy;
end;

function TTerraces3DScene.GetWidth: Integer;
begin Result := FStructure.Width; end;
function TTerraces3DScene.GetHeight: Integer;
begin Result := FStructure.Height; end;
function TTerraces3DScene.GetDepth: Integer;
begin Result := FStructure.Depth; end;

function TTerraces3DScene.IndexOf(const AX,AY,AZ: Integer): Integer;
begin
  {$IFDEF PAS2JS}
  if (AX <> Trunc(AX)) or (AY <> Trunc(AY)) or (AZ <> Trunc(AZ)) then
    raise ERangeError.Create('terrace scene coordinates must be exact integers');
  {$ENDIF}
  if (AX < 0) or (AY < 0) or (AZ < 0) or
    (AX >= Width) or (AY >= Height) or (AZ >= Depth) then
    raise ERangeError.Create('terrace scene coordinate out of bounds');
  Result := (AZ*Height+AY)*Width+AX;
end;

function TTerraces3DScene.TerrainAt(const AX,AY,AZ: Integer): TWfcModelToken;
begin Result := FTokens[IndexOf(AX,AY,AZ)]; end;

function TTerraces3DScene.Signature: String;
begin
  { Every terrain token is independently checked against the structure's
    prototype; that complete field is already covered by the scene hash. }
  Result := '1:' + Voxel3DSignatureHex(FStructure.Signature) + ':' +
    Voxel3DSignatureHex(FFoliage.Signature);
end;

end.
