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
unit wfc_pipeline_run;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_lattice,
  wfc_pipeline_layout,
  wfc_model,
  wfc_pipeline_model;

const
  WFC_PIPELINE_RUN_VERSION = 1;
  WFC_PIPELINE_RUN_MAPPED_VERSION = 2;
  WFC_PIPELINE_RUN_LAYOUT_VERSION = WFC_PIPELINE_LAYOUT_VERSION;
  WFC_PIPELINE_RUN_SIGNATURE_VERSION = 1;

  { Run artifacts can be supplied by untrusted tooling. These fixed limits
    bound both graph allocation and the work represented by one invocation.
    They are format contract and therefore versioned. }
  WFC_PIPELINE_RUN_MAX_DIMENSION = 4194304;
  WFC_PIPELINE_RUN_MAX_CELL_COUNT = 4194304;
  WFC_PIPELINE_RUN_MAX_LOCK_COUNT = 262144;
  WFC_PIPELINE_RUN_MAX_DOMAIN_COUNT = 262144;
  WFC_PIPELINE_RUN_MAX_DOMAIN_TOKEN_COUNT = 1024;
  WFC_PIPELINE_RUN_MAX_TOTAL_DOMAIN_TOKEN_COUNT = 1048576;
  WFC_PIPELINE_RUN_MAX_BACKTRACKS = 1000000;
  WFC_PIPELINE_RUN_MAX_PASS_BACKTRACKS = 65536;
  WFC_PIPELINE_RUN_MAX_ENCODED_TOKEN_LENGTH = 1048576;
  WFC_PIPELINE_RUN_MAX_TOTAL_ENCODED_TOKEN_LENGTH = 16777216;

type
  EWfcPipelineRun = class(Exception);

  TWfcPipelineRunSignature = Cardinal;

  TWfcPipelineSolveStrategy = (
    wpssOneWay,
    wpssNegotiated
  );

  TWfcPipelineCellLock = record
    PassIndex: Integer;
    X: Integer;
    Y: Integer;
    Z: Integer;
    Token: TWfcModelToken;
  end;
  TWfcPipelineCellLocks = array of TWfcPipelineCellLock;

  TWfcPipelineCellDomain = record
    PassIndex: Integer;
    X: Integer;
    Y: Integer;
    Z: Integer;
    { An assigned empty array is intentionally meaningful: it is an explicit
      contradiction, not an absent domain. }
    AllowedTokens: TWfcModelTokens;
  end;
  TWfcPipelineCellDomains = array of TWfcPipelineCellDomain;

  { Immutable, recipe-bound invocation. Construction resolves every public
    pass and token against ARecipe, but the run stores only the recipe's
    semantic signature. The recipe need not outlive this object. }
  TWfcPipelineRun = class
  strict private
    FRecipeSignature: TWfcPipelineSignature;
    FFormatVersion: Integer;
    FLayouts: TWfcPipelineLayoutTable;
    FWidth: Integer;
    FHeight: Integer;
    FDepth: Integer;
    FSeed: TGraphSeed;
    FStrategy: TWfcPipelineSolveStrategy;
    FMaxBacktracks: Integer;
    FMaxPassBacktracks: Integer;
    FCaptureTrace: Boolean;
    FLocks: TWfcPipelineCellLocks;
    FDomains: TWfcPipelineCellDomains;
    FSignature: TWfcPipelineRunSignature;
    function GetLockCount: Integer;
    function GetDomainCount: Integer;
    function GetPassCount: Integer;
    function GetTotalCellCount: Integer;
    procedure Initialize(const ARecipe: TWfcPipelineModel;
      const AExtents: TWfcPipelinePassExtents; const AFormatVersion: Integer;
      const ASeed: TGraphSeed; const AStrategy: TWfcPipelineSolveStrategy;
      const AMaxBacktracks, AMaxPassBacktracks: Integer;
      const ACaptureTrace: Boolean; const ALocks: TWfcPipelineCellLocks;
      const ADomains: TWfcPipelineCellDomains);
    function CalculateSignature: TWfcPipelineRunSignature;
    procedure ValidateLockIndex(const AIndex: Integer);
    procedure ValidateDomainIndex(const AIndex: Integer);
  public
    constructor Create(const ARecipe: TWfcPipelineModel;
      const AWidth, AHeight, ADepth: Integer;
      const ASeed: TGraphSeed;
      const AStrategy: TWfcPipelineSolveStrategy;
      const AMaxBacktracks, AMaxPassBacktracks: Integer;
      const ACaptureTrace: Boolean;
      const ALocks: TWfcPipelineCellLocks;
      const ADomains: TWfcPipelineCellDomains); overload;
    constructor Create(const ARecipe: TWfcPipelineModel;
      const AExtents: TWfcPipelinePassExtents; const ASeed: TGraphSeed;
      const AStrategy: TWfcPipelineSolveStrategy;
      const AMaxBacktracks, AMaxPassBacktracks: Integer;
      const ACaptureTrace: Boolean; const ALocks: TWfcPipelineCellLocks;
      const ADomains: TWfcPipelineCellDomains); overload;
    destructor Destroy; override;

    function LockAt(const AIndex: Integer): TWfcPipelineCellLock;
    function DomainAt(const AIndex: Integer): TWfcPipelineCellDomain;
    function CopyLocks: TWfcPipelineCellLocks;
    function CopyDomains: TWfcPipelineCellDomains;
    function CopyPassExtents: TWfcPipelinePassExtents;
    function CopyPassLayouts: TWfcLatticeLayouts;
    function PassLayoutAt(const APassIndex: Integer): TWfcLatticeLayout;
    function PassTopologyAt(const APassIndex: Integer): TWfcPipelinePassTopology;
    function PassCellCount(const APassIndex: Integer): Integer;
    function PassOffsetAt(const APassIndex: Integer): Integer;

    property RecipeSignature: TWfcPipelineSignature read FRecipeSignature;
    property FormatVersion: Integer read FFormatVersion;
    property PassCount: Integer read GetPassCount;
    property TotalCellCount: Integer read GetTotalCellCount;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Depth: Integer read FDepth;
    property Seed: TGraphSeed read FSeed;
    property Strategy: TWfcPipelineSolveStrategy read FStrategy;
    property MaxBacktracks: Integer read FMaxBacktracks;
    property MaxPassBacktracks: Integer read FMaxPassBacktracks;
    property CaptureTrace: Boolean read FCaptureTrace;
    property LockCount: Integer read GetLockCount;
    property DomainCount: Integer read GetDomainCount;
    property Signature: TWfcPipelineRunSignature read FSignature;
  end;

function MakeWfcPipelineCellLock(const APassIndex, AX, AY,
  AZ: Integer; const AToken: TWfcModelToken): TWfcPipelineCellLock;
function MakeWfcPipelineCellDomain(const APassIndex, AX, AY,
  AZ: Integer; const AAllowedTokens: TWfcModelTokens):
  TWfcPipelineCellDomain;
function WfcPipelineRunSignatureHex(
  const ASignature: TWfcPipelineRunSignature): String;

implementation

uses
  wfc_text_codec,
  wfc_pipeline_mapping,
  wfc_token_lookup;

type
  TWfcPipelineVocabularyArray = array of TWfcModelTokens;
  TWfcPipelineTokenLookupArray = array of TWfcTokenLookup;

procedure FreeTokenLookups(var AValues: TWfcPipelineTokenLookupArray);
var
  I: Integer;
begin
  for I := 0 to Length(AValues) - 1 do
    AValues[I].Free;
  AValues := nil;
end;

function CheckedLength(const ALength: SizeInt; const ALabel: String;
  const AMaximum: Integer): Integer;
begin
  if (ALength < 0) or
      ((ALength and (not SizeInt(High(Integer)))) <> 0) then
    raise EWfcPipelineRun.Create(ALabel + ' exceeds the Integer range');
  Result := Integer(ALength);
  if Result > AMaximum then
    raise EWfcPipelineRun.CreateFmt(
      '%s exceeds the version-1 limit [%d > %d]',
      [ALabel, Result, AMaximum]);
end;

function CloneTokens(const AValues: TWfcModelTokens): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function CloneDomain(const AValue: TWfcPipelineCellDomain):
  TWfcPipelineCellDomain;
begin
  Result.PassIndex := AValue.PassIndex;
  Result.X := AValue.X;
  Result.Y := AValue.Y;
  Result.Z := AValue.Z;
  Result.AllowedTokens := CloneTokens(AValue.AllowedTokens);
end;

function CompareCell(const ALeftPass, ALeftX, ALeftY, ALeftZ,
  ARightPass, ARightX, ARightY, ARightZ: Integer): Integer;
begin
  if ALeftPass <> ARightPass then
  begin
    if ALeftPass < ARightPass then
      Exit(-1);
    Exit(1);
  end;
  { Row-major identity orders Z planes, then Y rows, then X columns. }
  if ALeftZ <> ARightZ then
  begin
    if ALeftZ < ARightZ then
      Exit(-1);
    Exit(1);
  end;
  if ALeftY <> ARightY then
  begin
    if ALeftY < ARightY then
      Exit(-1);
    Exit(1);
  end;
  if ALeftX <> ARightX then
  begin
    if ALeftX < ARightX then
      Exit(-1);
    Exit(1);
  end;
  Result := 0;
end;

function TokenIndex(const AValues: TWfcModelTokens;
  const AToken: TWfcModelToken): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(AValues) - 1 do
    if AValues[I] = AToken then
      Exit(I);
  Result := -1;
end;

procedure ValidateStrategy(const AValue: TWfcPipelineSolveStrategy);
begin
  if (Ord(AValue) < Ord(Low(TWfcPipelineSolveStrategy))) or
      (Ord(AValue) > Ord(High(TWfcPipelineSolveStrategy))) then
    raise EWfcPipelineRun.CreateFmt('run strategy is unknown [%d]',
      [Ord(AValue)]);
  case AValue of
    wpssOneWay, wpssNegotiated:
      Exit;
  end;
end;

procedure RequireRunInteger(const AValue, AMinimum, AMaximum: Double;
  const AName: String);
{$IFDEF PAS2JS}var LValid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm LValid = typeof AValue === 'number' && Number.isFinite(AValue) && Number.isInteger(AValue); end;
  if not LValid then raise EWfcPipelineRun.Create(AName + ' must be a finite integer');
  {$ENDIF}
  if (AValue < AMinimum) or (AValue > AMaximum) then
    raise EWfcPipelineRun.Create(AName + ' is out of range');
end;

procedure RequireRunBoolean(const AValue: Boolean; const AName: String);
{$IFDEF PAS2JS}var LValid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm LValid = typeof AValue === 'boolean'; end;
  if not LValid then raise EWfcPipelineRun.Create(AName + ' must be Boolean');
  {$ENDIF}
end;

procedure ValidateShape(const AWidth, AHeight, ADepth: Integer);
var
  LPlaneCells: Integer;
begin
  if (AWidth < 1) or (AHeight < 1) or (ADepth < 1) then
    raise EWfcPipelineRun.Create('run dimensions must be positive');
  if (AWidth > WFC_PIPELINE_RUN_MAX_DIMENSION) or
      (AHeight > WFC_PIPELINE_RUN_MAX_DIMENSION) or
      (ADepth > WFC_PIPELINE_RUN_MAX_DIMENSION) then
    raise EWfcPipelineRun.Create(
      'run dimension exceeds the version-1 limit');
  if AWidth > WFC_PIPELINE_RUN_MAX_CELL_COUNT div AHeight then
    raise EWfcPipelineRun.Create(
      'run cell count exceeds the version-1 limit');
  LPlaneCells := AWidth * AHeight;
  if LPlaneCells > WFC_PIPELINE_RUN_MAX_CELL_COUNT div ADepth then
    raise EWfcPipelineRun.Create(
      'run cell count exceeds the version-1 limit');

end;

procedure ValidateCell(const ARecipe: TWfcPipelineModel;
  const ALayouts: TWfcPipelineLayoutTable;
  const APassIndex, AX, AY, AZ: Integer;
  const ALabel: String;
  const AVocabularies: TWfcPipelineVocabularyArray;
  out AVocabulary: TWfcModelTokens);
var
  LPass: TWfcPipelinePass;
begin
  if (APassIndex < 0) or (APassIndex >= ARecipe.PassCount) then
    raise EWfcPipelineRun.CreateFmt('%s pass is out of range [%d]',
      [ALabel, APassIndex]);
  LPass := ARecipe.PassAt(APassIndex);
  if LPass.Visibility <> wppvPublic then
    raise EWfcPipelineRun.CreateFmt('%s cannot target private pass %d',
      [ALabel, APassIndex]);
  try
    ALayouts.LocalCellIndex(APassIndex, MakeWfcLatticeVector(AX, AY, AZ));
  except
    on E: Exception do raise EWfcPipelineRun.Create(ALabel + ' coordinate is out of range: ' + E.Message);
  end;
  AVocabulary := AVocabularies[APassIndex];
end;

procedure AddTokenLength(var ATotal: Integer;
  const AToken: TWfcModelToken; const ALabel: String);
var
  LCanonical: String;
  LLength: Integer;
begin
  LCanonical := WfcTextEncodeToken(AToken,
    'pipeline run token validation');
  LLength := CheckedLength(Length(LCanonical),
    ALabel + ' encoded length',
    WFC_PIPELINE_RUN_MAX_ENCODED_TOKEN_LENGTH);
  if LLength > WFC_PIPELINE_RUN_MAX_TOTAL_ENCODED_TOKEN_LENGTH - ATotal then
    raise EWfcPipelineRun.Create(
      'run encoded token bytes exceed the version-1 aggregate limit');
  Inc(ATotal, LLength);
end;

procedure HashByte(var AHash: TWfcPipelineRunSignature;
  const AValue: Byte);
{$PUSH}
{$Q-}
var
  LValue: Cardinal;
begin
  AHash := AHash xor Cardinal(AValue);
  LValue := AHash;
  AHash := (LValue + (LValue shl 1) + (LValue shl 4) +
    (LValue shl 7) + (LValue shl 8) + (LValue shl 24)) and
    Cardinal($FFFFFFFF);
end;
{$POP}

procedure HashCardinal(var AHash: TWfcPipelineRunSignature;
  const AValue: Cardinal);
begin
  HashByte(AHash, Byte(AValue and $FF));
  HashByte(AHash, Byte((AValue shr 8) and $FF));
  HashByte(AHash, Byte((AValue shr 16) and $FF));
  HashByte(AHash, Byte((AValue shr 24) and $FF));
end;

procedure HashInteger(var AHash: TWfcPipelineRunSignature;
  const AValue: Integer);
begin
  HashCardinal(AHash, Cardinal(AValue));
end;

procedure HashBoolean(var AHash: TWfcPipelineRunSignature;
  const AValue: Boolean);
begin
  if AValue then
    HashByte(AHash, 1)
  else
    HashByte(AHash, 0);
end;

procedure HashAscii(var AHash: TWfcPipelineRunSignature;
  const AValue: String);
var
  I: Integer;
begin
  HashInteger(AHash, Length(AValue));
  for I := 1 to Length(AValue) do
    HashByte(AHash, Byte(Ord(AValue[I])));
end;

procedure HashToken(var AHash: TWfcPipelineRunSignature;
  const AValue: TWfcModelToken);
var
  LCanonical: String;
begin
  LCanonical := WfcTextEncodeToken(AValue,
    'pipeline run semantic signature');
  HashAscii(AHash, LCanonical);
end;

function TWfcPipelineRun.CalculateSignature: TWfcPipelineRunSignature;
var
  I: Integer;
  J: Integer;
  LDomain: TWfcPipelineCellDomain;
  LLock: TWfcPipelineCellLock;
  LLayout: TWfcLatticeLayout;
begin
  Result := Cardinal(2166136261);
  HashAscii(Result, 'wfcpipeline-run');
  HashCardinal(Result, FFormatVersion);
  HashCardinal(Result, WFC_PIPELINE_RUN_SIGNATURE_VERSION);
  HashCardinal(Result, FRecipeSignature);
  HashInteger(Result, FWidth);
  HashInteger(Result, FHeight);
  HashInteger(Result, FDepth);
  if FFormatVersion = WFC_PIPELINE_RUN_MAPPED_VERSION then
  begin
    HashAscii(Result, 'pass-layout-extents');
    HashInteger(Result, WFC_PIPELINE_RUN_LAYOUT_VERSION);
    HashInteger(Result, WFC_PASS_MAPPING_VERSION);
    HashInteger(Result, PassCount);
    for I := 0 to PassCount - 1 do
    begin
      LLayout := FLayouts.PassLayoutAt(I);
      HashInteger(Result, LLayout.Cells.X);
      HashInteger(Result, LLayout.Cells.Y);
      HashInteger(Result, LLayout.Cells.Z);
    end;
  end;
  HashCardinal(Result, FSeed);
  HashInteger(Result, Ord(FStrategy));
  HashInteger(Result, FMaxBacktracks);
  HashInteger(Result, FMaxPassBacktracks);
  HashBoolean(Result, FCaptureTrace);
  HashInteger(Result, Length(FLocks));
  for I := 0 to Length(FLocks) - 1 do
  begin
    LLock := FLocks[I];
    HashInteger(Result, LLock.PassIndex);
    HashInteger(Result, LLock.X);
    HashInteger(Result, LLock.Y);
    HashInteger(Result, LLock.Z);
    HashToken(Result, LLock.Token);
  end;
  HashInteger(Result, Length(FDomains));
  for I := 0 to Length(FDomains) - 1 do
  begin
    LDomain := FDomains[I];
    HashInteger(Result, LDomain.PassIndex);
    HashInteger(Result, LDomain.X);
    HashInteger(Result, LDomain.Y);
    HashInteger(Result, LDomain.Z);
    HashInteger(Result, Length(LDomain.AllowedTokens));
    for J := 0 to Length(LDomain.AllowedTokens) - 1 do
      HashToken(Result, LDomain.AllowedTokens[J]);
  end;
end;

function MakeWfcPipelineCellLock(const APassIndex, AX, AY,
  AZ: Integer; const AToken: TWfcModelToken): TWfcPipelineCellLock;
begin
  Result.PassIndex := APassIndex;
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
  Result.Token := AToken;
end;

function MakeWfcPipelineCellDomain(const APassIndex, AX, AY,
  AZ: Integer; const AAllowedTokens: TWfcModelTokens):
  TWfcPipelineCellDomain;
begin
  Result.PassIndex := APassIndex;
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
  Result.AllowedTokens := AAllowedTokens;
end;

function WfcPipelineRunSignatureHex(
  const ASignature: TWfcPipelineRunSignature): String;
begin
  Result := IntToHex(ASignature, 8);
end;

constructor TWfcPipelineRun.Create(const ARecipe: TWfcPipelineModel;
  const AWidth, AHeight, ADepth: Integer; const ASeed: TGraphSeed;
  const AStrategy: TWfcPipelineSolveStrategy;
  const AMaxBacktracks, AMaxPassBacktracks: Integer;
  const ACaptureTrace: Boolean; const ALocks: TWfcPipelineCellLocks;
  const ADomains: TWfcPipelineCellDomains);
var
  LFormatVersion: Integer;
  LExtents: TWfcPipelinePassExtents;
begin
  inherited Create;
  if not Assigned(ARecipe) then
    raise EWfcPipelineRun.Create('run recipe cannot be nil');
  ValidateShape(AWidth, AHeight, ADepth);
  LFormatVersion := WFC_PIPELINE_RUN_VERSION;
  if ARecipe.HasPassMapping then
    LFormatVersion := WFC_PIPELINE_RUN_MAPPED_VERSION;
  try
    LExtents := UniformWfcPipelinePassExtents(ARecipe, AWidth, AHeight, ADepth);
  except
    on E: Exception do raise EWfcPipelineRun.Create('run extents: ' + E.Message);
  end;
  Initialize(ARecipe, LExtents, LFormatVersion, ASeed, AStrategy,
    AMaxBacktracks, AMaxPassBacktracks, ACaptureTrace, ALocks, ADomains);
end;

constructor TWfcPipelineRun.Create(const ARecipe: TWfcPipelineModel;
  const AExtents: TWfcPipelinePassExtents; const ASeed: TGraphSeed;
  const AStrategy: TWfcPipelineSolveStrategy;
  const AMaxBacktracks, AMaxPassBacktracks: Integer;
  const ACaptureTrace: Boolean; const ALocks: TWfcPipelineCellLocks;
  const ADomains: TWfcPipelineCellDomains);
begin
  inherited Create;
  Initialize(ARecipe, AExtents, WFC_PIPELINE_RUN_MAPPED_VERSION,
    ASeed, AStrategy, AMaxBacktracks, AMaxPassBacktracks,
    ACaptureTrace, ALocks, ADomains);
end;

procedure TWfcPipelineRun.Initialize(const ARecipe: TWfcPipelineModel;
  const AExtents: TWfcPipelinePassExtents; const AFormatVersion: Integer;
  const ASeed: TGraphSeed; const AStrategy: TWfcPipelineSolveStrategy;
  const AMaxBacktracks, AMaxPassBacktracks: Integer;
  const ACaptureTrace: Boolean; const ALocks: TWfcPipelineCellLocks;
  const ADomains: TWfcPipelineCellDomains);
var
  I: Integer;
  J: Integer;
  LDomain: TWfcPipelineCellDomain;
  LLock: TWfcPipelineCellLock;
  LPreviousTokenIndex: Integer;
  LTokenIndex: Integer;
  LTotalDomainTokens: Integer;
  LTotalTokenLength: Integer;
  LTokenLookups: TWfcPipelineTokenLookupArray;
  LVocabulary: TWfcModelTokens;
  LVocabularies: TWfcPipelineVocabularyArray;
  LLayout: TWfcLatticeLayout;
begin
  if not Assigned(ARecipe) then
    raise EWfcPipelineRun.Create('run recipe cannot be nil');
  { Resolve and check every pass before allocating token/vocabulary storage. }
  try
    FLayouts := ResolveWfcPipelineLayoutTable(ARecipe, AExtents);
  except
    on E: Exception do
      raise EWfcPipelineRun.Create('run layouts: ' + E.Message);
  end;
  for I := 0 to FLayouts.PassCount - 1 do
  begin
    LLayout := FLayouts.PassLayoutAt(I);
    ValidateShape(LLayout.Cells.X, LLayout.Cells.Y, LLayout.Cells.Z);
  end;
  FFormatVersion := AFormatVersion;
  RequireRunInteger(ASeed, 0, High(Cardinal), 'run seed');
  RequireRunInteger(Ord(AStrategy), Ord(Low(TWfcPipelineSolveStrategy)),
    Ord(High(TWfcPipelineSolveStrategy)), 'run strategy');
  RequireRunInteger(AMaxBacktracks, 0, WFC_PIPELINE_RUN_MAX_BACKTRACKS, 'run backtrack limit');
  RequireRunInteger(AMaxPassBacktracks, 0, WFC_PIPELINE_RUN_MAX_PASS_BACKTRACKS, 'run pass-backtrack limit');
  RequireRunBoolean(ACaptureTrace, 'run trace flag');
  ValidateStrategy(AStrategy);
  if (AMaxBacktracks < 0) or
      (AMaxBacktracks > WFC_PIPELINE_RUN_MAX_BACKTRACKS) then
    raise EWfcPipelineRun.CreateFmt(
      'run backtrack limit is outside the version-1 range [0..%d]',
      [WFC_PIPELINE_RUN_MAX_BACKTRACKS]);
  if (AMaxPassBacktracks < 0) or
      (AMaxPassBacktracks > WFC_PIPELINE_RUN_MAX_PASS_BACKTRACKS) then
    raise EWfcPipelineRun.CreateFmt(
      'run pass-backtrack limit is outside the version-1 range [0..%d]',
      [WFC_PIPELINE_RUN_MAX_PASS_BACKTRACKS]);
  if (AStrategy = wpssOneWay) and (AMaxPassBacktracks <> 0) then
    raise EWfcPipelineRun.Create(
      'one-way strategy requires a zero pass-backtrack limit');
  CheckedLength(Length(ALocks), 'run lock count',
    WFC_PIPELINE_RUN_MAX_LOCK_COUNT);
  CheckedLength(Length(ADomains), 'run domain count',
    WFC_PIPELINE_RUN_MAX_DOMAIN_COUNT);

  LTokenLookups := nil;
  try
    SetLength(LVocabularies, ARecipe.PassCount);
    SetLength(LTokenLookups, ARecipe.PassCount);
    for I := 0 to ARecipe.PassCount - 1 do
      if ARecipe.PassAt(I).Visibility = wppvPublic then
      begin
        LVocabularies[I] := ARecipe.CopyPublicVocabulary(I);
        LTokenLookups[I] := TWfcTokenLookup.Create(LVocabularies[I]);
      end;

    LTotalDomainTokens := 0;
    LTotalTokenLength := 0;
    for I := 0 to Length(ALocks) - 1 do
    begin
      LLock := ALocks[I];
      if (I > 0) and (CompareCell(ALocks[I - 1].PassIndex,
          ALocks[I - 1].X, ALocks[I - 1].Y, ALocks[I - 1].Z,
          LLock.PassIndex, LLock.X, LLock.Y, LLock.Z) >= 0) then
        raise EWfcPipelineRun.Create(
          'run locks must be in strict pass/row-major order');
      ValidateCell(ARecipe, FLayouts,
        LLock.PassIndex, LLock.X, LLock.Y, LLock.Z,
        'run lock', LVocabularies, LVocabulary);
      if not WfcModelTokenIsValid(LLock.Token) then
        raise EWfcPipelineRun.CreateFmt(
          'run lock token is not a valid nonempty token [%d]', [I]);
      if LTokenLookups[LLock.PassIndex].Find(LLock.Token) < 0 then
        raise EWfcPipelineRun.CreateFmt(
          'run lock token is outside pass vocabulary [%d]', [I]);
      AddTokenLength(LTotalTokenLength, LLock.Token, 'run lock token');
    end;

    for I := 0 to Length(ADomains) - 1 do
    begin
      LDomain := ADomains[I];
      if (I > 0) and (CompareCell(ADomains[I - 1].PassIndex,
          ADomains[I - 1].X, ADomains[I - 1].Y,
          ADomains[I - 1].Z, LDomain.PassIndex,
          LDomain.X, LDomain.Y, LDomain.Z) >= 0) then
        raise EWfcPipelineRun.Create(
          'run domains must be in strict pass/row-major order');
      ValidateCell(ARecipe, FLayouts,
        LDomain.PassIndex, LDomain.X, LDomain.Y, LDomain.Z,
        'run domain', LVocabularies, LVocabulary);
      CheckedLength(Length(LDomain.AllowedTokens),
        'run domain token count', WFC_PIPELINE_RUN_MAX_DOMAIN_TOKEN_COUNT);
      if Length(LDomain.AllowedTokens) >
          WFC_PIPELINE_RUN_MAX_TOTAL_DOMAIN_TOKEN_COUNT -
          LTotalDomainTokens then
        raise EWfcPipelineRun.Create(
          'run domain token count exceeds the version-1 aggregate limit');
      Inc(LTotalDomainTokens, Length(LDomain.AllowedTokens));
      LPreviousTokenIndex := -1;
      for J := 0 to Length(LDomain.AllowedTokens) - 1 do
      begin
        if not WfcModelTokenIsValid(LDomain.AllowedTokens[J]) then
          raise EWfcPipelineRun.CreateFmt(
            'run domain token is not a valid nonempty token [%d, %d]',
            [I, J]);
        LTokenIndex := LTokenLookups[LDomain.PassIndex].Find(
          LDomain.AllowedTokens[J]);
        if LTokenIndex < 0 then
          raise EWfcPipelineRun.CreateFmt(
            'run domain token is outside pass vocabulary [%d, %d]',
            [I, J]);
        if LTokenIndex <= LPreviousTokenIndex then
          raise EWfcPipelineRun.CreateFmt(
            'run domain tokens must follow strict vocabulary order [%d]',
            [I]);
        LPreviousTokenIndex := LTokenIndex;
        AddTokenLength(LTotalTokenLength, LDomain.AllowedTokens[J],
          'run domain token');
      end;
    end;

    { A lock and domain may intentionally share a cell, but their conjunction
      must not be statically impossible. Both arrays are ordered, so a linear
      merge is sufficient and deterministic. }
    I := 0;
    J := 0;
    while (I < Length(ALocks)) and (J < Length(ADomains)) do
    begin
      LTokenIndex := CompareCell(ALocks[I].PassIndex, ALocks[I].X,
        ALocks[I].Y, ALocks[I].Z, ADomains[J].PassIndex,
        ADomains[J].X, ADomains[J].Y, ADomains[J].Z);
      if LTokenIndex < 0 then
        Inc(I)
      else if LTokenIndex > 0 then
        Inc(J)
      else
      begin
        if TokenIndex(ADomains[J].AllowedTokens, ALocks[I].Token) < 0 then
          raise EWfcPipelineRun.CreateFmt(
            'run lock is excluded by its cell domain [%d, %d]', [I, J]);
        Inc(I);
        Inc(J);
      end;
    end;

    FRecipeSignature := ARecipe.Signature;
    LLayout := FLayouts.PassLayoutAt(0);
    FWidth := LLayout.Cells.X;
    FHeight := LLayout.Cells.Y;
    FDepth := LLayout.Cells.Z;
    FSeed := ASeed;
    FStrategy := AStrategy;
    FMaxBacktracks := AMaxBacktracks;
    FMaxPassBacktracks := AMaxPassBacktracks;
    FCaptureTrace := ACaptureTrace;
    SetLength(FLocks, Length(ALocks));
    for I := 0 to Length(ALocks) - 1 do
      FLocks[I] := ALocks[I];
    SetLength(FDomains, Length(ADomains));
    for I := 0 to Length(ADomains) - 1 do
      FDomains[I] := CloneDomain(ADomains[I]);
    FSignature := CalculateSignature;
  finally
    FreeTokenLookups(LTokenLookups);
  end;
end;

destructor TWfcPipelineRun.Destroy;
begin
  FLayouts.Free;
  inherited Destroy;
end;

function TWfcPipelineRun.GetPassCount: Integer;
begin
  Result := FLayouts.PassCount;
end;

function TWfcPipelineRun.GetTotalCellCount: Integer;
begin
  Result := FLayouts.TotalCellCount;
end;

function TWfcPipelineRun.CopyPassExtents: TWfcPipelinePassExtents;
begin
  Result := FLayouts.CopyExtents;
end;

function TWfcPipelineRun.CopyPassLayouts: TWfcLatticeLayouts;
begin
  Result := FLayouts.CopyLayouts;
end;

function TWfcPipelineRun.PassLayoutAt(const APassIndex: Integer): TWfcLatticeLayout;
begin
  Result := FLayouts.PassLayoutAt(APassIndex);
end;

function TWfcPipelineRun.PassTopologyAt(const APassIndex: Integer): TWfcPipelinePassTopology;
begin
  Result := FLayouts.PassTopologyAt(APassIndex);
end;

function TWfcPipelineRun.PassCellCount(const APassIndex: Integer): Integer;
begin
  Result := FLayouts.PassCellCount(APassIndex);
end;

function TWfcPipelineRun.PassOffsetAt(const APassIndex: Integer): Integer;
begin
  Result := FLayouts.PassOffsetAt(APassIndex);
end;

function TWfcPipelineRun.GetLockCount: Integer;
begin
  Result := Length(FLocks);
end;

function TWfcPipelineRun.GetDomainCount: Integer;
begin
  Result := Length(FDomains);
end;

procedure TWfcPipelineRun.ValidateLockIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= LockCount) then
    raise EWfcPipelineRun.CreateFmt('run lock index is out of range [%d]',
      [AIndex]);
end;

procedure TWfcPipelineRun.ValidateDomainIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= DomainCount) then
    raise EWfcPipelineRun.CreateFmt(
      'run domain index is out of range [%d]', [AIndex]);
end;

function TWfcPipelineRun.LockAt(
  const AIndex: Integer): TWfcPipelineCellLock;
begin
  ValidateLockIndex(AIndex);
  Result := FLocks[AIndex];
end;

function TWfcPipelineRun.DomainAt(
  const AIndex: Integer): TWfcPipelineCellDomain;
begin
  ValidateDomainIndex(AIndex);
  Result := CloneDomain(FDomains[AIndex]);
end;

function TWfcPipelineRun.CopyLocks: TWfcPipelineCellLocks;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, LockCount);
  for I := 0 to LockCount - 1 do
    Result[I] := FLocks[I];
end;

function TWfcPipelineRun.CopyDomains: TWfcPipelineCellDomains;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, DomainCount);
  for I := 0 to DomainCount - 1 do
    Result[I] := CloneDomain(FDomains[I]);
end;

end.
