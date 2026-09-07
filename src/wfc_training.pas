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
unit wfc_training;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_pipeline_model;

const
  WFC_TRAINING_VERSION = 1;
  WFC_TRAINING_VALUE_QUOTA_VERSION = 1;
  WFC_TRAINING_CONNECTIVITY_VERSION = 1;
  WFC_TRAINING_SEQUENCE_WRAP_VERSION = 1;
  WFC_TRAINING_PATTERN_3D_VERSION = 1;

  WFC_TRAINING_MAX_SAMPLE_COUNT = 4096;
  WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT = 65536;
  WFC_TRAINING_MAX_DIMENSION = 65536;
  WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH = 65536;
  WFC_TRAINING_MAX_TOTAL_ENCODED_TOKEN_LENGTH = 4194304;
  WFC_TRAINING_MAX_FOOTPRINT_CELL_COUNT = 64;
  WFC_TRAINING_MAX_VISIT_COUNT = 16777216;
  WFC_TRAINING_MAX_ORDER = 64;
  WFC_TRAINING_MAX_VALUE_QUOTA_COUNT = 4096;
  WFC_TRAINING_MAX_VALUE_QUOTA_TOKEN_COUNT = 1024;
  WFC_TRAINING_MAX_TOTAL_VALUE_QUOTA_TOKEN_COUNT = 65536;
  WFC_TRAINING_MAX_CONNECTIVITY_COUNT = 4096;
  WFC_TRAINING_MAX_CONNECTIVITY_VALUE_COUNT = 1024;
  WFC_TRAINING_MAX_TOTAL_CONNECTIVITY_VALUE_COUNT = 65536;
  WFC_TRAINING_MAX_CONNECTIVITY_TERMINAL_COUNT = 65536;
  WFC_TRAINING_MAX_TOTAL_CONNECTIVITY_TERMINAL_COUNT = 65536;

type
  EWfcTraining = class(Exception);

  TWfcTrainingKind = (
    wtkAdjacency1D,
    wtkAdjacency2D,
    wtkPattern2D,
    wtkSequence,
    wtkAdjacency3D,
    wtkPattern3D
  );

  TWfcTrainingSample = record
    Name: TWfcModelToken;
    Width: Integer;
    Height: Integer;
    Tokens: TWfcModelTokens;
    { Appended for source compatibility. Legacy training kinds normalize this
      field to one without reading caller-owned, manually constructed records. }
    Depth: Integer;
  end;
  TWfcTrainingSamples = array of TWfcTrainingSample;

  TWfcTrainingMetadata = record
    Name: TWfcModelToken;
    LicenseIdentifier: TWfcModelToken;
    SourceDescription: TWfcModelToken;
  end;

  TWfcTrainingOptions = record
    Kind: TWfcTrainingKind;
    Boundary: TWfcModelBoundary;
    Symmetry: TWfcModelSymmetry;
    PatternWidth: Integer;
    PatternHeight: Integer;
    Order: Integer;
    { Appended: legacy kinds normalize to one without reading caller storage. }
    PatternDepth: Integer;
  end;

  { Explicit author policy on the learned public output, not an inference
    from observed frequencies. Values retains authored, unique token order;
    recipe compilation resolves exact strings into learned vocabulary order. }
  TWfcTrainingValueQuota = record
    LabelText: TWfcModelToken;
    Values: TWfcModelTokens;
    MinimumCount: Integer;
    MaximumCount: Integer;
  end;
  TWfcTrainingValueQuotas = array of TWfcTrainingValueQuota;

  { Explicit world-axis ports on public output tokens. Profile order is
    authored, not a learned vocabulary index. Sample symmetry does not
    transform these ports. Unprofiled tokens are nonparticipants. }
  TWfcTrainingConnectivityValue = record
    Value: TWfcModelToken;
    Openings: TGraphDirections;
    RequiredByValue: Boolean;
  end;
  TWfcTrainingConnectivityValues = array of TWfcTrainingConnectivityValue;
  TWfcTrainingConnectivity = record
    LabelText: TWfcModelToken;
    Root: TGraphPosition;
    RequiredPositions: TGraphPositions;
    Values: TWfcTrainingConnectivityValues;
    RequireAllParticipants: Boolean;
  end;
  TWfcTrainingConnectivities = array of TWfcTrainingConnectivity;

  { Immutable, pretokenized training request. Every dynamic input is detached
    at construction, and every dynamic accessor returns another detached copy. }
  TWfcTrainingDocument = class
  strict private
    FMetadata: TWfcTrainingMetadata;
    FOptions: TWfcTrainingOptions;
    FSamples: TWfcTrainingSamples;
    FValueQuotas: TWfcTrainingValueQuotas;
    FConnectivities: TWfcTrainingConnectivities;
    FTotalTokenCount: Integer;
    FSignature: Cardinal;
    function GetSampleCount: Integer;
    function GetValueQuotaCount: Integer;
    function GetValueQuotaVersion: Integer;
    function GetConnectivityCount: Integer;
    function GetConnectivityVersion: Integer;
    procedure Initialize(const AMetadata: TWfcTrainingMetadata;
      const AOptions: TWfcTrainingOptions;
      const ASamples: TWfcTrainingSamples;
      const AValueQuotas: TWfcTrainingValueQuotas;
      const AConnectivities: TWfcTrainingConnectivities);
    procedure ValidateSampleIndex(const AIndex: Integer);
  public
    constructor Create(const AMetadata: TWfcTrainingMetadata;
      const AOptions: TWfcTrainingOptions;
      const ASamples: TWfcTrainingSamples); overload;
    constructor Create(const AMetadata: TWfcTrainingMetadata;
      const AOptions: TWfcTrainingOptions;
      const ASamples: TWfcTrainingSamples;
      const AValueQuotas: TWfcTrainingValueQuotas); overload;
    constructor Create(const AMetadata: TWfcTrainingMetadata;
      const AOptions: TWfcTrainingOptions;
      const ASamples: TWfcTrainingSamples;
      const AValueQuotas: TWfcTrainingValueQuotas;
      const AConnectivities: TWfcTrainingConnectivities); overload;

    function CopyMetadata: TWfcTrainingMetadata;
    function CopyOptions: TWfcTrainingOptions;
    function SampleAt(const AIndex: Integer): TWfcTrainingSample;
    function CopySamples: TWfcTrainingSamples;
    function ValueQuotaAt(const AIndex: Integer): TWfcTrainingValueQuota;
    function CopyValueQuotas: TWfcTrainingValueQuotas;
    function ConnectivityAt(const AIndex: Integer): TWfcTrainingConnectivity;
    function CopyConnectivities: TWfcTrainingConnectivities;

    property SampleCount: Integer read GetSampleCount;
    property TotalTokenCount: Integer read FTotalTokenCount;
    property Signature: Cardinal read FSignature;
    property ValueQuotaCount: Integer read GetValueQuotaCount;
    property ValueQuotaVersion: Integer read GetValueQuotaVersion;
    property ConnectivityCount: Integer read GetConnectivityCount;
    property ConnectivityVersion: Integer read GetConnectivityVersion;
  end;

function MakeWfcTrainingMetadata(const AName, ALicenseIdentifier,
  ASourceDescription: TWfcModelToken): TWfcTrainingMetadata;

function MakeWfcTrainingSample(const AName: TWfcModelToken;
  const AWidth, AHeight: Integer;
  const ATokens: TWfcModelTokens): TWfcTrainingSample; overload;

function MakeWfcTrainingSample(const AName: TWfcModelToken;
  const AWidth, AHeight, ADepth: Integer;
  const ATokens: TWfcModelTokens): TWfcTrainingSample; overload;

function MakeWfcTrainingOptions(const AKind: TWfcTrainingKind;
  const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry;
  const APatternWidth, APatternHeight,
  AOrder: Integer): TWfcTrainingOptions; overload;
function MakeWfcTrainingOptions(const AKind: TWfcTrainingKind;
  const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry;
  const APatternWidth, APatternHeight, APatternDepth,
  AOrder: Integer): TWfcTrainingOptions; overload;

function MakeWfcTrainingValueQuota(const ALabelText: TWfcModelToken;
  const AValues: TWfcModelTokens;
  const AMinimumCount, AMaximumCount: Integer): TWfcTrainingValueQuota;

function MakeWfcTrainingConnectivityValue(const AValue: TWfcModelToken;
  const AOpenings: TGraphDirections; const ARequiredByValue: Boolean = False):
  TWfcTrainingConnectivityValue;
function MakeWfcTrainingConnectivity(const ALabelText: TWfcModelToken;
  const ARoot: TGraphPosition; const ARequiredPositions: TGraphPositions;
  const AValues: TWfcTrainingConnectivityValues;
  const ARequireAllParticipants: Boolean = False): TWfcTrainingConnectivity;

function WfcTrainingSignatureHex(const ASignature: Cardinal): String;

{ Circular extraction is an explicit capability, independent of an output
  graph's wrapping. Existing open sequence requests retain their v1 identity. }
function WfcTrainingOptionsUseWrappedSequence(
  const AOptions: TWfcTrainingOptions): Boolean;

function LearnWfcTrainingModelText(
  const ADocument: TWfcTrainingDocument): String;

function LearnWfcTrainingRecipe(
  const ADocument: TWfcTrainingDocument): TWfcPipelineModel;

implementation

uses
  wfc_learn,
  wfc_learn3d,
  wfc_model_text,
  wfc_pattern2d,
  wfc_pattern2d_learn,
  wfc_pattern2d_text,
  wfc_pattern3d,
  wfc_pattern3d_learn,
  wfc_pattern3d_text,
  wfc_sequence,
  wfc_sequence_learn,
  wfc_sequence_text,
  wfc_token_lookup,
  wfc_text_codec;

const
  FNV_OFFSET_BASIS = Cardinal(2166136261);
  D4_TRANSFORM_COUNT = 8;
  CUBE_ROTATION_TRANSFORM_COUNT = 24;
  CUBE_FULL_TRANSFORM_COUNT = 48;

type
  TTrainingStringSet = record
    Values: array of String;
    Slots: array of Integer;
    Hashes: array of Cardinal;
    Count: Integer;
    MaximumCount: Integer;
  end;

function CloneTokens(const ASource: TWfcModelTokens): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ASource));
  for I := 0 to Length(ASource) - 1 do
    Result[I] := ASource[I];
end;

function CloneSample(const ASource: TWfcTrainingSample;
  const AIncludeDepth: Boolean): TWfcTrainingSample;
begin
  Result.Name := ASource.Name;
  Result.Width := ASource.Width;
  Result.Height := ASource.Height;
  Result.Tokens := CloneTokens(ASource.Tokens);
  if AIncludeDepth then
    Result.Depth := ASource.Depth
  else
    Result.Depth := 1;
end;

function CloneSamples(const ASource: TWfcTrainingSamples;
  const AIncludeDepth: Boolean): TWfcTrainingSamples;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ASource));
  for I := 0 to Length(ASource) - 1 do
    Result[I] := CloneSample(ASource[I], AIncludeDepth);
end;

function CloneValueQuota(const ASource: TWfcTrainingValueQuota):
  TWfcTrainingValueQuota;
begin
  Result.LabelText := ASource.LabelText;
  Result.Values := CloneTokens(ASource.Values);
  Result.MinimumCount := ASource.MinimumCount;
  Result.MaximumCount := ASource.MaximumCount;
end;

function CloneValueQuotas(const ASource: TWfcTrainingValueQuotas):
  TWfcTrainingValueQuotas;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ASource));
  for I := 0 to Length(ASource) - 1 do
    Result[I] := CloneValueQuota(ASource[I]);
end;

function CloneConnectivity(const ASource: TWfcTrainingConnectivity):
  TWfcTrainingConnectivity;
begin
  Result.LabelText := ASource.LabelText;
  Result.Root := ASource.Root;
  Result.RequiredPositions := Copy(ASource.RequiredPositions, 0,
    Length(ASource.RequiredPositions));
  Result.Values := Copy(ASource.Values, 0, Length(ASource.Values));
  Result.RequireAllParticipants := ASource.RequireAllParticipants;
end;

function CloneConnectivities(const ASource: TWfcTrainingConnectivities):
  TWfcTrainingConnectivities;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(ASource));
  for I := 0 to High(ASource) do Result[I] := CloneConnectivity(ASource[I]);
end;

procedure HashByte(var AHash: Cardinal; const AValue: Byte);
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

procedure HashCardinal(var AHash: Cardinal; const AValue: Cardinal);
begin
  HashByte(AHash, Byte(AValue and $FF));
  HashByte(AHash, Byte((AValue shr 8) and $FF));
  HashByte(AHash, Byte((AValue shr 16) and $FF));
  HashByte(AHash, Byte((AValue shr 24) and $FF));
end;

procedure HashAscii(var AHash: Cardinal; const AValue: String);
var
  I: Integer;
begin
  HashCardinal(AHash, Cardinal(Length(AValue)));
  for I := 1 to Length(AValue) do
    HashByte(AHash, Byte(Ord(AValue[I])));
end;

function CanonicalToken(const AToken: TWfcModelToken): String;
begin
  Result := WfcTextEncodeToken(AToken, 'WFC training');
end;

function KindCode(const AKind: TWfcTrainingKind): String;
begin
  case AKind of
    wtkAdjacency1D:
      Result := 'adjacency1d';
    wtkAdjacency2D:
      Result := 'adjacency2d';
    wtkPattern2D:
      Result := 'pattern2d';
    wtkSequence:
      Result := 'sequence';
    wtkAdjacency3D:
      Result := 'adjacency3d';
    wtkPattern3D:
      Result := 'pattern3d';
  else
    raise EWfcTraining.Create('unknown training kind');
  end;
end;

function BoundaryCode(const ABoundary: TWfcModelBoundary): String;
begin
  case ABoundary of
    wmbOpen:
      Result := 'open';
    wmbWrap:
      Result := 'wrap';
  else
    raise EWfcTraining.Create('unknown training boundary');
  end;
end;

function SymmetryCode(const ASymmetry: TWfcModelSymmetry): String;
begin
  case ASymmetry of
    wmsNone:
      Result := 'none';
    wmsD4:
      Result := 'd4';
    wmsCubeRotations:
      Result := 'cube24';
    wmsCubeFull:
      Result := 'cube48';
  else
    raise EWfcTraining.Create('unknown training symmetry');
  end;
end;

function ConnectivityMask(const AOpenings: TGraphDirections): Integer;
begin
  Result := 0;
  if gdNorth in AOpenings then Inc(Result, 1);
  if gdEast in AOpenings then Inc(Result, 2);
  if gdSouth in AOpenings then Inc(Result, 4);
  if gdWest in AOpenings then Inc(Result, 8);
  if gdUp in AOpenings then Inc(Result, 16);
  if gdDown in AOpenings then Inc(Result, 32);
end;

function ConnectivityBooleanCode(const AValue: Boolean): String;
begin
  if AValue then Result := 'true' else Result := 'false';
end;

function WfcTrainingOptionsUseWrappedSequence(
  const AOptions: TWfcTrainingOptions): Boolean;
begin
  Result := (AOptions.Kind = wtkSequence) and (AOptions.Boundary = wmbWrap);
end;

function CalculateTrainingSignature(const AMetadata: TWfcTrainingMetadata;
  const AOptions: TWfcTrainingOptions;
  const ASamples: TWfcTrainingSamples;
  const AValueQuotas: TWfcTrainingValueQuotas;
  const AConnectivities: TWfcTrainingConnectivities): Cardinal;
var
  I: Integer;
  J: Integer;
begin
  Result := FNV_OFFSET_BASIS;
  if AOptions.Kind = wtkPattern3D then
  begin
    HashAscii(Result, 'wfclearn-v6');
    HashAscii(Result, '6');
  end
  else if WfcTrainingOptionsUseWrappedSequence(AOptions) then
  begin
    HashAscii(Result, 'wfclearn-v5');
    HashAscii(Result, '5');
  end
  else if Length(AConnectivities) <> 0 then
  begin
    HashAscii(Result, 'wfclearn-v4');
    HashAscii(Result, '4');
  end
  else if Length(AValueQuotas) <> 0 then
  begin
    HashAscii(Result, 'wfclearn-v3');
    HashAscii(Result, '3');
  end
  else if AOptions.Kind = wtkAdjacency3D then
  begin
    HashAscii(Result, 'wfclearn-v2');
    HashAscii(Result, '2');
  end
  else
  begin
    HashAscii(Result, 'wfclearn-v1');
    HashAscii(Result, IntToStr(WFC_TRAINING_VERSION));
  end;
  HashAscii(Result, CanonicalToken(AMetadata.Name));
  HashAscii(Result, CanonicalToken(AMetadata.LicenseIdentifier));
  HashAscii(Result, CanonicalToken(AMetadata.SourceDescription));
  HashAscii(Result, KindCode(AOptions.Kind));
  HashAscii(Result, BoundaryCode(AOptions.Boundary));
  HashAscii(Result, SymmetryCode(AOptions.Symmetry));
  HashAscii(Result, IntToStr(AOptions.PatternWidth));
  HashAscii(Result, IntToStr(AOptions.PatternHeight));
  if AOptions.Kind = wtkPattern3D then
    HashAscii(Result, IntToStr(AOptions.PatternDepth));
  HashAscii(Result, IntToStr(AOptions.Order));
  HashAscii(Result, IntToStr(Length(ASamples)));
  for I := 0 to Length(ASamples) - 1 do
  begin
    HashAscii(Result, CanonicalToken(ASamples[I].Name));
    HashAscii(Result, IntToStr(ASamples[I].Width));
    HashAscii(Result, IntToStr(ASamples[I].Height));
    if AOptions.Kind in [wtkAdjacency3D, wtkPattern3D] then
      HashAscii(Result, IntToStr(ASamples[I].Depth));
    HashAscii(Result, IntToStr(Length(ASamples[I].Tokens)));
    for J := 0 to Length(ASamples[I].Tokens) - 1 do
      HashAscii(Result, CanonicalToken(ASamples[I].Tokens[J]));
  end;
  if AOptions.Kind = wtkPattern3D then
  begin
    HashAscii(Result, 'pattern3d');
    HashAscii(Result, IntToStr(WFC_TRAINING_PATTERN_3D_VERSION));
  end;
  if WfcTrainingOptionsUseWrappedSequence(AOptions) then
  begin
    HashAscii(Result, 'sequence-wrap');
    HashAscii(Result, IntToStr(WFC_TRAINING_SEQUENCE_WRAP_VERSION));
  end;
  if Length(AValueQuotas) <> 0 then
  begin
    HashAscii(Result, 'value-quotas');
    HashAscii(Result, IntToStr(WFC_TRAINING_VALUE_QUOTA_VERSION));
    HashAscii(Result, IntToStr(Length(AValueQuotas)));
    for I := 0 to Length(AValueQuotas) - 1 do
    begin
      HashAscii(Result, CanonicalToken(AValueQuotas[I].LabelText));
      HashAscii(Result, IntToStr(AValueQuotas[I].MinimumCount));
      HashAscii(Result, IntToStr(AValueQuotas[I].MaximumCount));
      HashAscii(Result, IntToStr(Length(AValueQuotas[I].Values)));
      for J := 0 to Length(AValueQuotas[I].Values) - 1 do
        HashAscii(Result, CanonicalToken(AValueQuotas[I].Values[J]));
    end;
  end;
  if Length(AConnectivities) <> 0 then
  begin
    HashAscii(Result, 'connectivities');
    HashAscii(Result, IntToStr(WFC_TRAINING_CONNECTIVITY_VERSION));
    HashAscii(Result, IntToStr(Length(AConnectivities)));
    for I := 0 to High(AConnectivities) do
    begin
      HashAscii(Result, CanonicalToken(AConnectivities[I].LabelText));
      HashAscii(Result, IntToStr(AConnectivities[I].Root.X));
      HashAscii(Result, IntToStr(AConnectivities[I].Root.Y));
      HashAscii(Result, IntToStr(AConnectivities[I].Root.Z));
      HashAscii(Result, ConnectivityBooleanCode(AConnectivities[I].RequireAllParticipants));
      HashAscii(Result, IntToStr(Length(AConnectivities[I].RequiredPositions)));
      for J := 0 to High(AConnectivities[I].RequiredPositions) do
      begin
        HashAscii(Result, IntToStr(AConnectivities[I].RequiredPositions[J].X));
        HashAscii(Result, IntToStr(AConnectivities[I].RequiredPositions[J].Y));
        HashAscii(Result, IntToStr(AConnectivities[I].RequiredPositions[J].Z));
      end;
      HashAscii(Result, IntToStr(Length(AConnectivities[I].Values)));
      for J := 0 to High(AConnectivities[I].Values) do
      begin
        HashAscii(Result, CanonicalToken(AConnectivities[I].Values[J].Value));
        HashAscii(Result, IntToStr(ConnectivityMask(AConnectivities[I].Values[J].Openings)));
        HashAscii(Result, ConnectivityBooleanCode(AConnectivities[I].Values[J].RequiredByValue));
      end;
    end;
  end;
end;

function CalculateSampleSignature(
  const ASample: TWfcTrainingSample;
  const AIncludeDepth: Boolean): Cardinal;
var
  I: Integer;
begin
  Result := FNV_OFFSET_BASIS;
  if AIncludeDepth then
    HashAscii(Result, 'wfclearn-sample-v2')
  else
    HashAscii(Result, 'wfclearn-sample-v1');
  HashAscii(Result, CanonicalToken(ASample.Name));
  HashAscii(Result, IntToStr(ASample.Width));
  HashAscii(Result, IntToStr(ASample.Height));
  if AIncludeDepth then
    HashAscii(Result, IntToStr(ASample.Depth));
  HashAscii(Result, IntToStr(Length(ASample.Tokens)));
  for I := 0 to Length(ASample.Tokens) - 1 do
    HashAscii(Result, CanonicalToken(ASample.Tokens[I]));
end;

function CheckedAdd(const A, B, AMaximum: Integer;
  const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) or (A > AMaximum) or
      (B > AMaximum - A) then
    raise EWfcTraining.Create(ALabel + ' exceeds the version-1 limit');
  Result := A + B;
end;

function CheckedMultiply(const A, B, AMaximum: Integer;
  const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) or
      ((A <> 0) and (B > AMaximum div A)) then
    raise EWfcTraining.Create(ALabel + ' exceeds the version-1 limit');
  Result := A * B;
end;

procedure AccumulateEncodedToken(const AToken: TWfcModelToken;
  const ALabel: String; var ATotal: Integer);
var
  LEncoded: String;
  LRawLength: SizeInt;
begin
  LRawLength := Length(AToken);
  if (LRawLength <= 0) or
      (LRawLength > WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH) or
      (not WfcModelTokenIsValid(AToken)) then
    raise EWfcTraining.Create(ALabel +
      ' must be a nonempty, well-formed UTF-8 token within the version-1 limit');
  LEncoded := CanonicalToken(AToken);
  if Length(LEncoded) > WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH then
    raise EWfcTraining.Create(ALabel +
      ' encoded length exceeds the version-1 limit');
  ATotal := CheckedAdd(ATotal, Length(LEncoded),
    WFC_TRAINING_MAX_TOTAL_ENCODED_TOKEN_LENGTH,
    'aggregate encoded training-token length');
end;

procedure InitializeStringSet(var ASet: TTrainingStringSet;
  const AMaximumCount: Integer);
var
  LCapacity: Integer;
begin
  ASet.Values := nil;
  ASet.Slots := nil;
  ASet.Hashes := nil;
  ASet.Count := 0;
  ASet.MaximumCount := AMaximumCount;
  LCapacity := 2;
  while LCapacity < AMaximumCount * 2 do
    LCapacity := LCapacity * 2;
  SetLength(ASet.Values, AMaximumCount);
  SetLength(ASet.Slots, LCapacity);
  SetLength(ASet.Hashes, LCapacity);
end;

function LookupHash(const AValue: String): Cardinal;
var
  I: Integer;
begin
  Result := FNV_OFFSET_BASIS;
  HashCardinal(Result, Cardinal(Length(AValue)));
  for I := 1 to Length(AValue) do
    HashCardinal(Result, Cardinal(Ord(AValue[I])));
end;

function FindOrAddString(var ASet: TTrainingStringSet;
  const AValue, ALabel: String; out AAdded: Boolean): Integer;
var
  LHash: Cardinal;
  LProbe: Integer;
  LSlot: Integer;
  LValueIndex: Integer;
begin
  LHash := LookupHash(AValue);
  LSlot := Integer(LHash mod Cardinal(Length(ASet.Slots)));
  for LProbe := 0 to Length(ASet.Slots) - 1 do
  begin
    if ASet.Slots[LSlot] = 0 then
    begin
      if ASet.Count = ASet.MaximumCount then
        raise EWfcTraining.Create(ALabel +
          ' exceeds the target model version-1 limit');
      Result := ASet.Count;
      ASet.Values[Result] := AValue;
      ASet.Slots[LSlot] := Result + 1;
      ASet.Hashes[LSlot] := LHash;
      Inc(ASet.Count);
      AAdded := True;
      Exit;
    end;
    LValueIndex := ASet.Slots[LSlot] - 1;
    if (ASet.Hashes[LSlot] = LHash) and
        (ASet.Values[LValueIndex] = AValue) then
    begin
      Result := LValueIndex;
      AAdded := False;
      Exit;
    end;
    Inc(LSlot);
    if LSlot = Length(ASet.Slots) then
      LSlot := 0;
  end;
  raise EWfcTraining.Create(ALabel + ' lookup is unexpectedly full');
end;

function FindString(const ASet: TTrainingStringSet;
  const AValue: String): Integer;
var
  LHash: Cardinal;
  LProbe: Integer;
  LSlot: Integer;
  LValueIndex: Integer;
begin
  Result := -1;
  LHash := LookupHash(AValue);
  LSlot := Integer(LHash mod Cardinal(Length(ASet.Slots)));
  for LProbe := 0 to Length(ASet.Slots) - 1 do
  begin
    if ASet.Slots[LSlot] = 0 then
      Exit;
    LValueIndex := ASet.Slots[LSlot] - 1;
    if (ASet.Hashes[LSlot] = LHash) and
        (ASet.Values[LValueIndex] = AValue) then
      Exit(LValueIndex);
    Inc(LSlot);
    if LSlot = Length(ASet.Slots) then
      LSlot := 0;
  end;
end;

procedure RequireQuotaInteger(const AValue: Integer; const ALabel: String);
begin
  if not ((AValue >= 0) and (AValue <= High(Integer))) then
    raise EWfcTraining.Create(ALabel +
      ' must be an exact integer in 0..High(Integer)');
  {$IFDEF PAS2JS}
  if AValue <> Trunc(AValue) then
    raise EWfcTraining.Create(ALabel + ' must be an exact integer');
  {$ENDIF}
end;

procedure ValidateValueQuotas(const AValueQuotas: TWfcTrainingValueQuotas;
  var AEncodedTotal: Integer);
var
  I: Integer;
  J: Integer;
  LAdded: Boolean;
  LLabels: TTrainingStringSet;
  LValues: TTrainingStringSet;
  LTotal: Integer;
begin
  if Length(AValueQuotas) > WFC_TRAINING_MAX_VALUE_QUOTA_COUNT then
    raise EWfcTraining.Create('training value quota count exceeds the limit');
  if Length(AValueQuotas) = 0 then
    Exit;
  { Check the complete externally supplied shape before allocating lookups. }
  LTotal := 0;
  for I := 0 to Length(AValueQuotas) - 1 do
  begin
    RequireQuotaInteger(AValueQuotas[I].MinimumCount,
      'training value quota minimum');
    RequireQuotaInteger(AValueQuotas[I].MaximumCount,
      'training value quota maximum');
    if AValueQuotas[I].MinimumCount > AValueQuotas[I].MaximumCount then
      raise EWfcTraining.Create('training value quota minimum exceeds maximum');
    if (Length(AValueQuotas[I].Values) = 0) or
        (Length(AValueQuotas[I].Values) >
        WFC_TRAINING_MAX_VALUE_QUOTA_TOKEN_COUNT) then
      raise EWfcTraining.Create('training value quota token count is outside the limit');
    LTotal := CheckedAdd(LTotal, Length(AValueQuotas[I].Values),
      WFC_TRAINING_MAX_TOTAL_VALUE_QUOTA_TOKEN_COUNT,
      'aggregate training value quota token count');
    AccumulateEncodedToken(AValueQuotas[I].LabelText,
      'training value quota label', AEncodedTotal);
    for J := 0 to Length(AValueQuotas[I].Values) - 1 do
      AccumulateEncodedToken(AValueQuotas[I].Values[J],
        'training value quota token', AEncodedTotal);
  end;
  InitializeStringSet(LLabels, Length(AValueQuotas));
  for I := 0 to Length(AValueQuotas) - 1 do
  begin
    FindOrAddString(LLabels, CanonicalToken(AValueQuotas[I].LabelText),
      'training value quota label count', LAdded);
    if not LAdded then
      raise EWfcTraining.CreateFmt(
        'training value quota labels must be unique [%d]', [I]);
    InitializeStringSet(LValues, Length(AValueQuotas[I].Values));
    for J := 0 to Length(AValueQuotas[I].Values) - 1 do
    begin
      FindOrAddString(LValues, CanonicalToken(AValueQuotas[I].Values[J]),
        'training value quota token count', LAdded);
      if not LAdded then
        raise EWfcTraining.CreateFmt(
          'training value quota tokens must be unique [%d,%d]', [I, J]);
    end;
  end;
end;

procedure RequireConnectivityBoolean(const AValue: Boolean; const ALabel: String);
begin
  if (AValue <> False) and (AValue <> True) then
    raise EWfcTraining.Create(ALabel + ' must be Boolean');
  {$IFNDEF PAS2JS}
  if Ord(AValue) > 1 then raise EWfcTraining.Create(ALabel + ' must be Boolean');
  {$ENDIF}
end;

procedure RequireConnectivityPosition(const AValue: TGraphPosition;
  const ARank: Integer; const ALabel: String);

  procedure Axis(const ACoordinate: TGraphCoordinate);
  begin
    if not ((ACoordinate >= 0) and
        (ACoordinate <= TGraphCoordinate(High(Integer)))) then
      raise EWfcTraining.Create(ALabel +
        ' coordinate must be an exact integer in 0..High(Integer)');
    {$IFDEF PAS2JS}
    if ACoordinate <> Trunc(ACoordinate) then
      raise EWfcTraining.Create(ALabel + ' coordinate must be an exact integer');
    {$ENDIF}
  end;

begin
  Axis(AValue.X); Axis(AValue.Y); Axis(AValue.Z);
  if ((ARank = 1) and (AValue.Y <> 0)) or ((ARank < 3) and (AValue.Z <> 0)) then
    raise EWfcTraining.Create(ALabel + ' coordinate exceeds the output rank');
end;

function ConnectivityPositionBefore(const ALeft, ARight: TGraphPosition): Boolean;
begin
  Result := (ALeft.Z < ARight.Z) or ((ALeft.Z = ARight.Z) and
    ((ALeft.Y < ARight.Y) or ((ALeft.Y = ARight.Y) and (ALeft.X < ARight.X))));
end;

procedure ValidateConnectivities(const AOptions: TWfcTrainingOptions;
  const AConnectivities: TWfcTrainingConnectivities; var AEncodedTotal: Integer);
var
  I, J, LRank, LTotalValues, LTotalTerminals: Integer;
  LLabels, LValues: TTrainingStringSet;
  LAdded: Boolean;
  LDirections: TGraphDirections;
  LDirection: TGraphDirection;
begin
  if Length(AConnectivities) > WFC_TRAINING_MAX_CONNECTIVITY_COUNT then
    raise EWfcTraining.Create('training connectivity count exceeds the limit');
  if Length(AConnectivities) = 0 then Exit;
  case AOptions.Kind of
    wtkAdjacency1D, wtkSequence: LRank := 1;
    wtkAdjacency2D, wtkPattern2D: LRank := 2;
    wtkAdjacency3D, wtkPattern3D: LRank := 3;
  else raise EWfcTraining.Create('unknown training kind'); end;
  LTotalValues := 0; LTotalTerminals := 0;
  { Preflight the complete externally supplied shape before owned registries
    or lookups. Anchors belong to the future output, not a sample's bounds. }
  for I := 0 to High(AConnectivities) do
  begin
    if (Length(AConnectivities[I].Values) = 0) or
        (Length(AConnectivities[I].Values) > WFC_TRAINING_MAX_CONNECTIVITY_VALUE_COUNT) then
      raise EWfcTraining.Create('training connectivity profile count is outside the limit');
    if Length(AConnectivities[I].RequiredPositions) > WFC_TRAINING_MAX_CONNECTIVITY_TERMINAL_COUNT then
      raise EWfcTraining.Create('training connectivity terminal count exceeds the limit');
    LTotalValues := CheckedAdd(LTotalValues, Length(AConnectivities[I].Values),
      WFC_TRAINING_MAX_TOTAL_CONNECTIVITY_VALUE_COUNT, 'aggregate training connectivity profile count');
    LTotalTerminals := CheckedAdd(LTotalTerminals, Length(AConnectivities[I].RequiredPositions),
      WFC_TRAINING_MAX_TOTAL_CONNECTIVITY_TERMINAL_COUNT, 'aggregate training connectivity terminal count');
    AccumulateEncodedToken(AConnectivities[I].LabelText, 'training connectivity label', AEncodedTotal);
    RequireConnectivityPosition(AConnectivities[I].Root, LRank, 'training connectivity root');
    RequireConnectivityBoolean(AConnectivities[I].RequireAllParticipants, 'training connectivity all-participants');
    for J := 0 to High(AConnectivities[I].RequiredPositions) do
    begin
      RequireConnectivityPosition(AConnectivities[I].RequiredPositions[J], LRank,
        'training connectivity terminal');
      if (AConnectivities[I].RequiredPositions[J].X = AConnectivities[I].Root.X) and
          (AConnectivities[I].RequiredPositions[J].Y = AConnectivities[I].Root.Y) and
          (AConnectivities[I].RequiredPositions[J].Z = AConnectivities[I].Root.Z) then
        raise EWfcTraining.Create('training connectivity terminal repeats the root');
      if (J <> 0) and not ConnectivityPositionBefore(
          AConnectivities[I].RequiredPositions[J - 1], AConnectivities[I].RequiredPositions[J]) then
        raise EWfcTraining.Create('training connectivity terminals must use strict Z,Y,X order');
    end;
    for J := 0 to High(AConnectivities[I].Values) do
    begin
      AccumulateEncodedToken(AConnectivities[I].Values[J].Value,
        'training connectivity profile token', AEncodedTotal);
      RequireConnectivityBoolean(AConnectivities[I].Values[J].RequiredByValue,
        'training connectivity required-by-value');
      LDirections := [];
      for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
        if LDirection in AConnectivities[I].Values[J].Openings then Include(LDirections, LDirection);
      if LDirections <> AConnectivities[I].Values[J].Openings then
        raise EWfcTraining.Create('training connectivity profile has an invalid opening direction');
    end;
  end;
  InitializeStringSet(LLabels, Length(AConnectivities));
  for I := 0 to High(AConnectivities) do
  begin
    FindOrAddString(LLabels, CanonicalToken(AConnectivities[I].LabelText),
      'training connectivity label count', LAdded);
    if not LAdded then raise EWfcTraining.Create('training connectivity labels must be unique');
    InitializeStringSet(LValues, Length(AConnectivities[I].Values));
    for J := 0 to High(AConnectivities[I].Values) do
    begin
      FindOrAddString(LValues, CanonicalToken(AConnectivities[I].Values[J].Value),
        'training connectivity profile count', LAdded);
      if not LAdded then raise EWfcTraining.Create('training connectivity profile tokens must be unique');
    end;
  end;
end;

procedure RequireWrappedSequenceInteger(const AValue: Integer;
  const ALabel: String);
{$IFDEF PAS2JS}var LValid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    LValid = typeof AValue === 'number' && isFinite(AValue) &&
      Math.floor(AValue) === AValue;
  end;
  if not LValid then
    raise EWfcTraining.Create(ALabel + ' must be an exact integer');
  {$ENDIF}
  if (AValue < 1) or (AValue > High(Integer)) then
    raise EWfcTraining.Create(ALabel + ' must be a positive Integer');
end;

procedure ValidateOptions(const AOptions: TWfcTrainingOptions);
begin
  KindCode(AOptions.Kind);
  BoundaryCode(AOptions.Boundary);
  SymmetryCode(AOptions.Symmetry);

  case AOptions.Kind of
    wtkAdjacency1D:
      begin
        if AOptions.Symmetry <> wmsNone then
          raise EWfcTraining.Create(
            'adjacency1d training symmetry must be none');
        if (AOptions.PatternWidth <> 0) or
            (AOptions.PatternHeight <> 0) then
          raise EWfcTraining.Create(
            'adjacency1d training footprint must be 0,0');
        if AOptions.Order <> 0 then
          raise EWfcTraining.Create(
            'adjacency1d training order must be 0');
      end;
    wtkAdjacency2D:
      begin
        if not (AOptions.Symmetry in [wmsNone, wmsD4]) then
          raise EWfcTraining.Create(
            'adjacency2d training symmetry must be none or d4');
        if (AOptions.PatternWidth <> 0) or
            (AOptions.PatternHeight <> 0) then
          raise EWfcTraining.Create(
            'adjacency2d training footprint must be 0,0');
        if AOptions.Order <> 0 then
          raise EWfcTraining.Create(
            'adjacency2d training order must be 0');
      end;
    wtkPattern2D:
      begin
        if not (AOptions.Symmetry in [wmsNone, wmsD4]) then
          raise EWfcTraining.Create(
            'pattern2d training symmetry must be none or d4');
        if AOptions.Order <> 0 then
          raise EWfcTraining.Create('pattern2d training order must be 0');
        if (AOptions.PatternWidth < 1) or
            (AOptions.PatternHeight < 1) or
            (AOptions.PatternWidth > WFC_TRAINING_MAX_DIMENSION) or
            (AOptions.PatternHeight > WFC_TRAINING_MAX_DIMENSION) then
          raise EWfcTraining.Create(
            'pattern2d footprint dimensions are outside the version-1 limit');
        CheckedMultiply(AOptions.PatternWidth, AOptions.PatternHeight,
          WFC_TRAINING_MAX_FOOTPRINT_CELL_COUNT,
          'pattern2d footprint cell count');
        if (AOptions.Symmetry = wmsD4) and
            (AOptions.PatternWidth <> AOptions.PatternHeight) then
          raise EWfcTraining.Create(
            'D4 pattern2d training requires a square footprint');
      end;
    wtkSequence:
      begin
        if AOptions.Boundary = wmbWrap then
          RequireWrappedSequenceInteger(AOptions.Order, 'circular sequence order');
        if AOptions.Symmetry <> wmsNone then
          raise EWfcTraining.Create('sequence training symmetry must be none');
        if (AOptions.PatternWidth <> 0) or
            (AOptions.PatternHeight <> 0) then
          raise EWfcTraining.Create(
            'sequence training footprint must be 0,0');
        if (AOptions.Order < 1) or
            (AOptions.Order > WFC_TRAINING_MAX_ORDER) then
          raise EWfcTraining.Create(
            'sequence training order is outside the version-1 limit');
      end;
    wtkAdjacency3D:
      begin
        if (AOptions.PatternWidth <> 0) or
            (AOptions.PatternHeight <> 0) then
          raise EWfcTraining.Create(
            'adjacency3d training footprint must be 0,0');
        if AOptions.Order <> 0 then
          raise EWfcTraining.Create(
            'adjacency3d training order must be 0');
      end;
    wtkPattern3D:
      begin
        RequireWrappedSequenceInteger(AOptions.PatternWidth, 'pattern3d footprint width');
        RequireWrappedSequenceInteger(AOptions.PatternHeight, 'pattern3d footprint height');
        RequireWrappedSequenceInteger(AOptions.PatternDepth, 'pattern3d footprint depth');
        RequireQuotaInteger(AOptions.Order, 'pattern3d order');
        if AOptions.Order <> 0 then
          raise EWfcTraining.Create('pattern3d training order must be 0');
        CheckedMultiply(CheckedMultiply(AOptions.PatternWidth,AOptions.PatternHeight,
          WFC_TRAINING_MAX_FOOTPRINT_CELL_COUNT,'pattern3d footprint plane'),
          AOptions.PatternDepth,WFC_TRAINING_MAX_FOOTPRINT_CELL_COUNT,
          'pattern3d footprint cell count');
        if (AOptions.Symmetry = wmsD4) and (AOptions.PatternWidth <> AOptions.PatternHeight) then
          raise EWfcTraining.Create('D4 pattern3d training requires a square XY footprint');
        if (AOptions.Symmetry in [wmsCubeRotations,wmsCubeFull]) and
          ((AOptions.PatternWidth <> AOptions.PatternHeight) or
           (AOptions.PatternWidth <> AOptions.PatternDepth)) then
          raise EWfcTraining.Create('cube pattern3d training requires a cubic footprint');
      end;
  end;
end;

procedure ValidateModelCapacities(const AOptions: TWfcTrainingOptions;
  const ASamples: TWfcTrainingSamples;
  const AValueQuotas: TWfcTrainingValueQuotas;
  const AConnectivities: TWfcTrainingConnectivities);
var
  LAdded: Boolean;
  LHistory: String;
  LHistorySize: Integer;
  LHistoryPosition: Integer;
  LSampleLength: Integer;
  LPosition: Integer;
  LPublicTokens: TTrainingStringSet;
  LSampleIndex: Integer;
  LStateCount: Integer;
  LStates: TTrainingStringSet;
  LTokenIndex: Integer;
  LTokenIndices: array of array of Integer;
  LTokenLimit: Integer;
  I: Integer;
  J: Integer;
  H: Integer;
begin
  case AOptions.Kind of
    wtkAdjacency1D,
    wtkAdjacency2D,
    wtkAdjacency3D:
      LTokenLimit := WFC_MODEL_MAX_VALUE_COUNT;
    wtkPattern2D:
      LTokenLimit := WFC_PATTERN_2D_MAX_PALETTE_COUNT;
    wtkPattern3D:
      LTokenLimit := WFC_PATTERN_3D_MAX_PALETTE_COUNT;
    wtkSequence:
      LTokenLimit := WFC_SEQUENCE_MAX_PUBLIC_TOKEN_COUNT;
  else
    raise EWfcTraining.Create('unknown training kind');
  end;

  InitializeStringSet(LPublicTokens, LTokenLimit);
  SetLength(LTokenIndices, Length(ASamples));
  for LSampleIndex := 0 to Length(ASamples) - 1 do
  begin
    SetLength(LTokenIndices[LSampleIndex],
      Length(ASamples[LSampleIndex].Tokens));
    for I := 0 to Length(ASamples[LSampleIndex].Tokens) - 1 do
    begin
      LTokenIndex := FindOrAddString(LPublicTokens,
        CanonicalToken(ASamples[LSampleIndex].Tokens[I]),
        'training vocabulary', LAdded);
      LTokenIndices[LSampleIndex][I] := LTokenIndex;
    end;
  end;

  for I := 0 to Length(AValueQuotas) - 1 do
    for J := 0 to Length(AValueQuotas[I].Values) - 1 do
      if FindString(LPublicTokens,
          CanonicalToken(AValueQuotas[I].Values[J])) < 0 then
        raise EWfcTraining.CreateFmt(
          'training value quota token is absent from the source [%d,%d]',
          [I, J]);

  for I := 0 to High(AConnectivities) do
    for J := 0 to High(AConnectivities[I].Values) do
      if FindString(LPublicTokens, CanonicalToken(AConnectivities[I].Values[J].Value)) < 0 then
        raise EWfcTraining.CreateFmt(
          'training connectivity profile token is absent from the source [%d,%d]', [I, J]);

  if AOptions.Kind <> wtkSequence then
    Exit;

  LHistorySize := AOptions.Order - 1;
  InitializeStringSet(LStates, WFC_SEQUENCE_MAX_STATE_COUNT);
  for LSampleIndex := 0 to Length(ASamples) - 1 do
    for LPosition := 0 to Length(LTokenIndices[LSampleIndex]) - 1 do
    begin
      LHistory := '';
      for H := 0 to LHistorySize - 1 do
      begin
        if AOptions.Boundary = wmbWrap then
        begin
          { Reduce the distance before subtraction: even order > sample
            length repeats only this sample, never adjacent corpus entries. }
          LSampleLength := Length(LTokenIndices[LSampleIndex]);
          LHistoryPosition := LPosition - ((LHistorySize - H) mod LSampleLength);
          if LHistoryPosition < 0 then Inc(LHistoryPosition, LSampleLength);
          LHistory := LHistory + 'T' +
            IntToStr(LTokenIndices[LSampleIndex][LHistoryPosition]) + ';';
        end
        else if LPosition < LHistorySize - H then
          LHistory := LHistory + 'B;'
        else
          LHistory := LHistory + 'T' +
            IntToStr(LTokenIndices[LSampleIndex][
              LPosition - (LHistorySize - H)]) + ';';
      end;
      LHistory := LHistory + 'E' +
        IntToStr(LTokenIndices[LSampleIndex][LPosition]);
      FindOrAddString(LStates, LHistory, 'sequence state count', LAdded);
      if LAdded and (LHistorySize <> 0) and
          (LStates.Count > WFC_SEQUENCE_MAX_TOTAL_HISTORY_ITEM_COUNT div
          LHistorySize) then
        raise EWfcTraining.Create(
          'sequence state history exceeds the target model version-1 limit');
    end;
  LStateCount := LStates.Count;
  if LStateCount = 0 then
    raise EWfcTraining.Create('sequence training produced no states');
end;

procedure ValidateTrainingInput(const AMetadata: TWfcTrainingMetadata;
  const AOptions: TWfcTrainingOptions;
  const ASamples: TWfcTrainingSamples;
  const AValueQuotas: TWfcTrainingValueQuotas;
  const AConnectivities: TWfcTrainingConnectivities;
  out ATotalTokenCount: Integer);
var
  I: Integer;
  J: Integer;
  LBaseVisits: Integer;
  LEncodedTotal: Integer;
  LFootprintCells: Integer;
  LSampleCells: Integer;
  LSampleNameAdded: Boolean;
  LSampleNames: TTrainingStringSet;
  LTransformCount: Integer;
  LVisits: Integer;
begin
  ValidateOptions(AOptions);
  if (Length(ASamples) = 0) or
      (Length(ASamples) > WFC_TRAINING_MAX_SAMPLE_COUNT) then
    raise EWfcTraining.Create(
      'training sample count is outside the version-1 limit');

  LEncodedTotal := 0;
  AccumulateEncodedToken(AMetadata.Name, 'training name', LEncodedTotal);
  AccumulateEncodedToken(AMetadata.LicenseIdentifier,
    'training license identifier', LEncodedTotal);
  AccumulateEncodedToken(AMetadata.SourceDescription,
    'training source description', LEncodedTotal);
  ValidateValueQuotas(AValueQuotas, LEncodedTotal);
  ValidateConnectivities(AOptions, AConnectivities, LEncodedTotal);

  ATotalTokenCount := 0;
  LVisits := 0;
  InitializeStringSet(LSampleNames, WFC_TRAINING_MAX_SAMPLE_COUNT);
  case AOptions.Symmetry of
    wmsNone:
      LTransformCount := 1;
    wmsD4:
      LTransformCount := D4_TRANSFORM_COUNT;
    wmsCubeRotations:
      LTransformCount := CUBE_ROTATION_TRANSFORM_COUNT;
    wmsCubeFull:
      LTransformCount := CUBE_FULL_TRANSFORM_COUNT;
  else
    raise EWfcTraining.Create('unknown training symmetry');
  end;
  if AOptions.Kind = wtkPattern2D then
    LFootprintCells := CheckedMultiply(AOptions.PatternWidth,
      AOptions.PatternHeight, WFC_TRAINING_MAX_FOOTPRINT_CELL_COUNT,
      'pattern2d footprint cell count')
  else if AOptions.Kind = wtkPattern3D then
    LFootprintCells := CheckedMultiply(CheckedMultiply(AOptions.PatternWidth,
      AOptions.PatternHeight,WFC_TRAINING_MAX_FOOTPRINT_CELL_COUNT,
      'pattern3d footprint plane'),AOptions.PatternDepth,
      WFC_TRAINING_MAX_FOOTPRINT_CELL_COUNT,'pattern3d footprint cell count')
  else
    LFootprintCells := 1;

  for I := 0 to Length(ASamples) - 1 do
  begin
    if WfcTrainingOptionsUseWrappedSequence(AOptions) then
    begin
      RequireWrappedSequenceInteger(ASamples[I].Width, 'circular sample width');
      RequireWrappedSequenceInteger(ASamples[I].Height, 'circular sample height');
    end;
    if AOptions.Kind = wtkPattern3D then
    begin
      RequireWrappedSequenceInteger(ASamples[I].Width, 'pattern3d sample width');
      RequireWrappedSequenceInteger(ASamples[I].Height, 'pattern3d sample height');
      RequireWrappedSequenceInteger(ASamples[I].Depth, 'pattern3d sample depth');
    end;
    AccumulateEncodedToken(ASamples[I].Name,
      'training sample name', LEncodedTotal);
    FindOrAddString(LSampleNames, CanonicalToken(ASamples[I].Name),
      'training sample name count', LSampleNameAdded);
    if not LSampleNameAdded then
      raise EWfcTraining.CreateFmt(
        'training sample names must be unique [%d]', [I]);

    {$IFDEF PAS2JS}
    if (AOptions.Kind = wtkAdjacency3D) and
        ((ASamples[I].Width <> Trunc(ASamples[I].Width)) or
        (ASamples[I].Height <> Trunc(ASamples[I].Height)) or
        (ASamples[I].Depth <> Trunc(ASamples[I].Depth))) then
      raise EWfcTraining.Create('volume sample dimensions must be exact integers');
    {$ENDIF}
    if (ASamples[I].Width < 1) or (ASamples[I].Height < 1) or
        (ASamples[I].Width > WFC_TRAINING_MAX_DIMENSION) or
        (ASamples[I].Height > WFC_TRAINING_MAX_DIMENSION) then
      raise EWfcTraining.CreateFmt(
        'training sample dimensions are outside the version-1 limit [%d]',
        [I]);
    if (AOptions.Kind in [wtkAdjacency3D,wtkPattern3D]) and
        ((ASamples[I].Depth < 1) or
        (ASamples[I].Depth > WFC_TRAINING_MAX_DIMENSION)) then
      raise EWfcTraining.CreateFmt(
        'training sample depth is outside the version-1 limit [%d]', [I]);
    if ((AOptions.Kind = wtkAdjacency1D) or
        (AOptions.Kind = wtkSequence)) and (ASamples[I].Height <> 1) then
      raise EWfcTraining.CreateFmt(
        'one-dimensional training sample height must be 1 [%d]', [I]);
    LSampleCells := CheckedMultiply(ASamples[I].Width,
      ASamples[I].Height, WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT,
      'training sample cell count');
    if AOptions.Kind in [wtkAdjacency3D,wtkPattern3D] then
      LSampleCells := CheckedMultiply(LSampleCells, ASamples[I].Depth,
        WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT,
        'training sample volume cell count');
    if Length(ASamples[I].Tokens) <> LSampleCells then
      raise EWfcTraining.CreateFmt(
        'training sample %d has %d tokens; expected %d',
        [I, Length(ASamples[I].Tokens), LSampleCells]);
    ATotalTokenCount := CheckedAdd(ATotalTokenCount, LSampleCells,
      WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT,
      'aggregate training token count');

    for J := 0 to Length(ASamples[I].Tokens) - 1 do
      AccumulateEncodedToken(ASamples[I].Tokens[J],
        Format('training sample %d token %d', [I, J]), LEncodedTotal);

    case AOptions.Kind of
      wtkAdjacency1D,
      wtkAdjacency2D,
      wtkAdjacency3D:
        LBaseVisits := CheckedMultiply(LSampleCells, LTransformCount,
          WFC_TRAINING_MAX_VISIT_COUNT,
          'training observation visit count');
      wtkPattern2D:
        begin
          if (AOptions.Boundary = wmbOpen) and
              ((ASamples[I].Width < AOptions.PatternWidth) or
              (ASamples[I].Height < AOptions.PatternHeight)) then
            raise EWfcTraining.CreateFmt(
              'open pattern2d sample is smaller than the footprint [%d]',
              [I]);
          if AOptions.Boundary = wmbOpen then
            LBaseVisits := CheckedMultiply(
              ASamples[I].Width - AOptions.PatternWidth + 1,
              ASamples[I].Height - AOptions.PatternHeight + 1,
              WFC_TRAINING_MAX_VISIT_COUNT,
              'pattern2d origin visit count')
          else
            LBaseVisits := LSampleCells;
          LBaseVisits := CheckedMultiply(LBaseVisits, LTransformCount,
            WFC_TRAINING_MAX_VISIT_COUNT,
            'pattern2d transformed visit count');
          LBaseVisits := CheckedMultiply(LBaseVisits, LFootprintCells,
            WFC_TRAINING_MAX_VISIT_COUNT,
            'pattern2d payload visit count');
        end;
      wtkPattern3D:
        begin
          if AOptions.Boundary = wmbOpen then
          begin
            if (ASamples[I].Width < AOptions.PatternWidth) or
              (ASamples[I].Height < AOptions.PatternHeight) or
              (ASamples[I].Depth < AOptions.PatternDepth) then
              raise EWfcTraining.Create('open pattern3d source is smaller than its footprint');
            LBaseVisits := CheckedMultiply(CheckedMultiply(
              ASamples[I].Width-AOptions.PatternWidth+1,
              ASamples[I].Height-AOptions.PatternHeight+1,
              WFC_TRAINING_MAX_VISIT_COUNT,'pattern3d origin plane'),
              ASamples[I].Depth-AOptions.PatternDepth+1,
              WFC_TRAINING_MAX_VISIT_COUNT,'pattern3d origin volume');
          end
          else LBaseVisits := LSampleCells;
          LBaseVisits := CheckedMultiply(LBaseVisits,LTransformCount,
            WFC_TRAINING_MAX_VISIT_COUNT,'pattern3d transformed visits');
          LBaseVisits := CheckedMultiply(LBaseVisits,LFootprintCells,
            WFC_TRAINING_MAX_VISIT_COUNT,'pattern3d payload visits');
        end;
      wtkSequence:
        LBaseVisits := CheckedMultiply(LSampleCells, AOptions.Order,
          WFC_TRAINING_MAX_VISIT_COUNT,
          'sequence history visit count');
    else
      raise EWfcTraining.Create('unknown training kind');
    end;
    LVisits := CheckedAdd(LVisits, LBaseVisits,
      WFC_TRAINING_MAX_VISIT_COUNT, 'aggregate training visit count');
  end;

  ValidateModelCapacities(AOptions, ASamples, AValueQuotas, AConnectivities);
end;

function MakeWfcTrainingMetadata(const AName, ALicenseIdentifier,
  ASourceDescription: TWfcModelToken): TWfcTrainingMetadata;
begin
  Result.Name := AName;
  Result.LicenseIdentifier := ALicenseIdentifier;
  Result.SourceDescription := ASourceDescription;
end;

function MakeWfcTrainingSample(const AName: TWfcModelToken;
  const AWidth, AHeight: Integer;
  const ATokens: TWfcModelTokens): TWfcTrainingSample;
begin
  Result := MakeWfcTrainingSample(AName, AWidth, AHeight, 1, ATokens);
end;

function MakeWfcTrainingSample(const AName: TWfcModelToken;
  const AWidth, AHeight, ADepth: Integer;
  const ATokens: TWfcModelTokens): TWfcTrainingSample;
begin
  Result.Name := AName;
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.Tokens := CloneTokens(ATokens);
  Result.Depth := ADepth;
end;

function MakeWfcTrainingOptions(const AKind: TWfcTrainingKind;
  const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry;
  const APatternWidth, APatternHeight,
  AOrder: Integer): TWfcTrainingOptions;
begin
  Result.Kind := AKind;
  Result.Boundary := ABoundary;
  Result.Symmetry := ASymmetry;
  Result.PatternWidth := APatternWidth;
  Result.PatternHeight := APatternHeight;
  Result.Order := AOrder;
  Result.PatternDepth := 1;
end;

function MakeWfcTrainingOptions(const AKind: TWfcTrainingKind;
  const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry;
  const APatternWidth, APatternHeight, APatternDepth,
  AOrder: Integer): TWfcTrainingOptions;
begin
  Result := MakeWfcTrainingOptions(AKind,ABoundary,ASymmetry,
    APatternWidth,APatternHeight,AOrder);
  if AKind = wtkPattern3D then Result.PatternDepth := APatternDepth;
end;

function NormalizeTrainingOptions(const AOptions: TWfcTrainingOptions): TWfcTrainingOptions;
begin
  { Do not copy the whole caller record: pre-extension callers can leave its
    appended field uninitialized (or install a throwing JS getter there). }
  Result.Kind := AOptions.Kind;
  Result.Boundary := AOptions.Boundary;
  Result.Symmetry := AOptions.Symmetry;
  Result.PatternWidth := AOptions.PatternWidth;
  Result.PatternHeight := AOptions.PatternHeight;
  Result.Order := AOptions.Order;
  if Result.Kind = wtkPattern3D then Result.PatternDepth := AOptions.PatternDepth
  else Result.PatternDepth := 1;
end;

function WfcTrainingSignatureHex(const ASignature: Cardinal): String;
begin
  Result := IntToHex(ASignature, 8);
end;

function MakeWfcTrainingValueQuota(const ALabelText: TWfcModelToken;
  const AValues: TWfcModelTokens;
  const AMinimumCount, AMaximumCount: Integer): TWfcTrainingValueQuota;
begin
  if Length(AValues) > WFC_TRAINING_MAX_VALUE_QUOTA_TOKEN_COUNT then
    raise EWfcTraining.Create('training value quota token count exceeds the limit');
  Result.LabelText := ALabelText;
  Result.Values := CloneTokens(AValues);
  Result.MinimumCount := AMinimumCount;
  Result.MaximumCount := AMaximumCount;
end;

function MakeWfcTrainingConnectivityValue(const AValue: TWfcModelToken;
  const AOpenings: TGraphDirections; const ARequiredByValue: Boolean):
  TWfcTrainingConnectivityValue;
begin
  Result.Value := AValue;
  Result.Openings := AOpenings;
  Result.RequiredByValue := ARequiredByValue;
end;

function MakeWfcTrainingConnectivity(const ALabelText: TWfcModelToken;
  const ARoot: TGraphPosition; const ARequiredPositions: TGraphPositions;
  const AValues: TWfcTrainingConnectivityValues;
  const ARequireAllParticipants: Boolean): TWfcTrainingConnectivity;
begin
  if Length(ARequiredPositions) > WFC_TRAINING_MAX_CONNECTIVITY_TERMINAL_COUNT then
    raise EWfcTraining.Create('training connectivity terminal count exceeds the limit');
  if Length(AValues) > WFC_TRAINING_MAX_CONNECTIVITY_VALUE_COUNT then
    raise EWfcTraining.Create('training connectivity profile count exceeds the limit');
  Result.LabelText := ALabelText;
  Result.Root := ARoot;
  Result.RequiredPositions := Copy(ARequiredPositions, 0, Length(ARequiredPositions));
  Result.Values := Copy(AValues, 0, Length(AValues));
  Result.RequireAllParticipants := ARequireAllParticipants;
end;

{ TWfcTrainingDocument }

constructor TWfcTrainingDocument.Create(
  const AMetadata: TWfcTrainingMetadata;
  const AOptions: TWfcTrainingOptions;
  const ASamples: TWfcTrainingSamples);
begin
  inherited Create;
  Initialize(AMetadata, AOptions, ASamples, nil, nil);
end;

constructor TWfcTrainingDocument.Create(
  const AMetadata: TWfcTrainingMetadata;
  const AOptions: TWfcTrainingOptions;
  const ASamples: TWfcTrainingSamples;
  const AValueQuotas: TWfcTrainingValueQuotas);
begin
  inherited Create;
  Initialize(AMetadata, AOptions, ASamples, AValueQuotas, nil);
end;

constructor TWfcTrainingDocument.Create(
  const AMetadata: TWfcTrainingMetadata;
  const AOptions: TWfcTrainingOptions;
  const ASamples: TWfcTrainingSamples;
  const AValueQuotas: TWfcTrainingValueQuotas;
  const AConnectivities: TWfcTrainingConnectivities);
begin
  inherited Create;
  Initialize(AMetadata, AOptions, ASamples, AValueQuotas, AConnectivities);
end;

procedure TWfcTrainingDocument.Initialize(
  const AMetadata: TWfcTrainingMetadata;
  const AOptions: TWfcTrainingOptions;
  const ASamples: TWfcTrainingSamples;
  const AValueQuotas: TWfcTrainingValueQuotas;
  const AConnectivities: TWfcTrainingConnectivities);
var
  LTotalTokenCount: Integer;
  LOptions: TWfcTrainingOptions;
begin
  LOptions := NormalizeTrainingOptions(AOptions);
  ValidateTrainingInput(AMetadata, LOptions, ASamples, AValueQuotas, AConnectivities,
    LTotalTokenCount);
  FMetadata := AMetadata;
  FOptions := LOptions;
  FSamples := CloneSamples(ASamples, LOptions.Kind in [wtkAdjacency3D,wtkPattern3D]);
  FValueQuotas := CloneValueQuotas(AValueQuotas);
  FConnectivities := CloneConnectivities(AConnectivities);
  FTotalTokenCount := LTotalTokenCount;
  FSignature := CalculateTrainingSignature(FMetadata, FOptions, FSamples,
    FValueQuotas, FConnectivities);
end;

function TWfcTrainingDocument.GetSampleCount: Integer;
begin
  Result := Length(FSamples);
end;

procedure TWfcTrainingDocument.ValidateSampleIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= SampleCount) then
    raise ERangeError.CreateFmt('training sample index out of bounds [%d]',
      [AIndex]);
end;

function TWfcTrainingDocument.CopyMetadata: TWfcTrainingMetadata;
begin
  Result := FMetadata;
end;

function TWfcTrainingDocument.CopyOptions: TWfcTrainingOptions;
begin
  Result := FOptions;
end;

function TWfcTrainingDocument.SampleAt(
  const AIndex: Integer): TWfcTrainingSample;
begin
  ValidateSampleIndex(AIndex);
  Result := CloneSample(FSamples[AIndex], True);
end;

function TWfcTrainingDocument.CopySamples: TWfcTrainingSamples;
begin
  Result := CloneSamples(FSamples, True);
end;

function TWfcTrainingDocument.GetValueQuotaCount: Integer;
begin
  Result := Length(FValueQuotas);
end;

function TWfcTrainingDocument.GetValueQuotaVersion: Integer;
begin
  if ValueQuotaCount = 0 then
    Result := 0
  else
    Result := WFC_TRAINING_VALUE_QUOTA_VERSION;
end;

function TWfcTrainingDocument.ValueQuotaAt(
  const AIndex: Integer): TWfcTrainingValueQuota;
begin
  if not ((AIndex >= 0) and (AIndex < ValueQuotaCount)) then
    raise ERangeError.Create('training value quota index is out of bounds');
  {$IFDEF PAS2JS}
  if AIndex <> Trunc(AIndex) then
    raise ERangeError.Create('training value quota index must be an exact integer');
  {$ENDIF}
  Result := CloneValueQuota(FValueQuotas[AIndex]);
end;

function TWfcTrainingDocument.CopyValueQuotas: TWfcTrainingValueQuotas;
begin
  Result := CloneValueQuotas(FValueQuotas);
end;

function TWfcTrainingDocument.GetConnectivityCount: Integer;
begin Result := Length(FConnectivities); end;

function TWfcTrainingDocument.GetConnectivityVersion: Integer;
begin
  if ConnectivityCount = 0 then Result := 0
  else Result := WFC_TRAINING_CONNECTIVITY_VERSION;
end;

function TWfcTrainingDocument.ConnectivityAt(const AIndex: Integer): TWfcTrainingConnectivity;
begin
  if not ((AIndex >= 0) and (AIndex < ConnectivityCount)) then
    raise ERangeError.Create('training connectivity index is out of bounds');
  {$IFDEF PAS2JS}
  if AIndex <> Trunc(AIndex) then
    raise ERangeError.Create('training connectivity index must be an exact integer');
  {$ENDIF}
  Result := CloneConnectivity(FConnectivities[AIndex]);
end;

function TWfcTrainingDocument.CopyConnectivities: TWfcTrainingConnectivities;
begin Result := CloneConnectivities(FConnectivities); end;

function BuildLearnVolumeSamples(
  const ADocument: TWfcTrainingDocument): TWfcLearnVolumeSamples;
var
  I: Integer;
  LSample: TWfcTrainingSample;
begin
  Result := nil;
  SetLength(Result, ADocument.SampleCount);
  for I := 0 to ADocument.SampleCount - 1 do
  begin
    LSample := ADocument.SampleAt(I);
    Result[I] := MakeLearnSample3D(LSample.Tokens, LSample.Width,
      LSample.Height, LSample.Depth);
  end;
end;

function BuildLearnSamples(
  const ADocument: TWfcTrainingDocument): TWfcLearnSamples;
var
  I: Integer;
  LSample: TWfcTrainingSample;
begin
  Result := nil;
  SetLength(Result, ADocument.SampleCount);
  for I := 0 to ADocument.SampleCount - 1 do
  begin
    LSample := ADocument.SampleAt(I);
    if ADocument.CopyOptions.Kind = wtkAdjacency1D then
      Result[I] := MakeLearnSample1D(LSample.Tokens)
    else
      Result[I] := MakeLearnSample2D(LSample.Tokens,
        LSample.Width, LSample.Height);
  end;
end;

function BuildSequenceSamples(
  const ADocument: TWfcTrainingDocument): TWfcSequenceSamples;
var
  I: Integer;
  LSample: TWfcTrainingSample;
begin
  Result := nil;
  SetLength(Result, ADocument.SampleCount);
  for I := 0 to ADocument.SampleCount - 1 do
  begin
    LSample := ADocument.SampleAt(I);
    Result[I] := MakeWfcSequenceSample(LSample.Tokens);
  end;
end;

function LearnTrainingPayload(const ADocument: TWfcTrainingDocument;
  out APublicVocabulary: TWfcModelTokens): String;
var
  LModel: TWfcModel;
  LOptions: TWfcTrainingOptions;
  LPattern: TWfcOverlappingModel2D;
  LPattern3D: TWfcOverlappingModel3D;
  LSamples: TWfcLearnSamples;
  LSequence: TWfcSequenceModel;
  LSequenceSamples: TWfcSequenceSamples;
  LVolumeSamples: TWfcLearnVolumeSamples;
begin
  APublicVocabulary := nil;
  if ADocument = nil then
    raise EWfcTraining.Create('training document cannot be nil');
  LOptions := ADocument.CopyOptions;
  case LOptions.Kind of
    wtkAdjacency1D:
      begin
        LSamples := BuildLearnSamples(ADocument);
        LModel := LearnModel1DCorpus(LSamples, LOptions.Boundary);
        try
          Result := EncodeWfcModelText(LModel);
          if (ADocument.ValueQuotaCount <> 0) or (ADocument.ConnectivityCount <> 0) then
            APublicVocabulary := LModel.CopyTokens;
        finally
          LModel.Free;
        end;
      end;
    wtkAdjacency2D:
      begin
        LSamples := BuildLearnSamples(ADocument);
        LModel := LearnModel2DCorpus(LSamples, LOptions.Boundary,
          LOptions.Symmetry);
        try
          Result := EncodeWfcModelText(LModel);
          if (ADocument.ValueQuotaCount <> 0) or (ADocument.ConnectivityCount <> 0) then
            APublicVocabulary := LModel.CopyTokens;
        finally
          LModel.Free;
        end;
      end;
    wtkPattern2D:
      begin
        LSamples := BuildLearnSamples(ADocument);
        LPattern := LearnOverlappingModel2DCorpus(LSamples,
          LOptions.PatternWidth, LOptions.PatternHeight,
          LOptions.Boundary, LOptions.Symmetry);
        try
          Result := EncodeWfcPattern2DText(LPattern);
          if (ADocument.ValueQuotaCount <> 0) or (ADocument.ConnectivityCount <> 0) then
            APublicVocabulary := LPattern.CopyPalette;
        finally
          LPattern.Free;
        end;
      end;
    wtkSequence:
      begin
        LSequenceSamples := BuildSequenceSamples(ADocument);
        LSequence := LearnSequenceModelCorpus(LSequenceSamples,
          LOptions.Order, LOptions.Boundary);
        try
          Result := EncodeWfcSequenceText(LSequence);
          if (ADocument.ValueQuotaCount <> 0) or (ADocument.ConnectivityCount <> 0) then
            APublicVocabulary := LSequence.CopyPublicTokens;
        finally
          LSequence.Free;
        end;
      end;
    wtkAdjacency3D:
      begin
        LVolumeSamples := BuildLearnVolumeSamples(ADocument);
        LModel := LearnModel3DCorpus(LVolumeSamples, LOptions.Boundary,
          LOptions.Symmetry);
        try
          Result := EncodeWfcModelText(LModel);
          if (ADocument.ValueQuotaCount <> 0) or (ADocument.ConnectivityCount <> 0) then
            APublicVocabulary := LModel.CopyTokens;
        finally
          LModel.Free;
        end;
      end;
    wtkPattern3D:
      begin
        LVolumeSamples := BuildLearnVolumeSamples(ADocument);
        LPattern3D := LearnOverlappingModel3DCorpus(LVolumeSamples,
          LOptions.PatternWidth,LOptions.PatternHeight,LOptions.PatternDepth,
          LOptions.Boundary,LOptions.Symmetry);
        try
          Result := EncodeWfcPattern3DText(LPattern3D);
          if (ADocument.ValueQuotaCount <> 0) or (ADocument.ConnectivityCount <> 0) then
            APublicVocabulary := LPattern3D.CopyPalette;
        finally LPattern3D.Free; end;
      end;
  else
    raise EWfcTraining.Create('unknown training kind');
  end;
end;

function LearnWfcTrainingModelText(
  const ADocument: TWfcTrainingDocument): String;
var
  LUnusedVocabulary: TWfcModelTokens;
begin
  if ADocument = nil then
    raise EWfcTraining.Create('training document cannot be nil');
  if ADocument.ConnectivityCount <> 0 then
    raise EWfcTraining.Create(
      'standalone model export cannot represent authored connectivity; ' +
      'export a pipeline recipe instead');
  if ADocument.ValueQuotaCount <> 0 then
    raise EWfcTraining.Create(
      'standalone model export cannot represent authored value quotas; ' +
      'export a pipeline recipe instead');
  Result := LearnTrainingPayload(ADocument, LUnusedVocabulary);
end;

function BuildRecipeValueQuotas(const ADocument: TWfcTrainingDocument;
  const APublicPassIndex: Integer;
  const APublicVocabulary: TWfcModelTokens): TWfcPipelineValueQuotas;
var
  I: Integer;
  J: Integer;
  LIndex: Integer;
  LCount: Integer;
  LLookup: TWfcTokenLookup;
  LSelected: array of Boolean;
  LQuota: TWfcTrainingValueQuota;
  LValues: TWfcModelTokens;
begin
  Result := nil;
  if ADocument.ValueQuotaCount = 0 then
    Exit;
  LLookup := TWfcTokenLookup.Create(APublicVocabulary);
  try
    SetLength(Result, ADocument.ValueQuotaCount);
    SetLength(LSelected, Length(APublicVocabulary));
    for I := 0 to ADocument.ValueQuotaCount - 1 do
    begin
      LQuota := ADocument.ValueQuotaAt(I);
      for J := 0 to Length(LSelected) - 1 do
        LSelected[J] := False;
      for J := 0 to Length(LQuota.Values) - 1 do
      begin
        LIndex := LLookup.Find(LQuota.Values[J]);
        if LIndex < 0 then
          raise EWfcTraining.CreateFmt(
            'training value quota token is absent from learned public output [%d,%d]',
            [I, J]);
        LSelected[LIndex] := True;
      end;
      SetLength(LValues, Length(LQuota.Values));
      LCount := 0;
      for J := 0 to Length(APublicVocabulary) - 1 do
        if LSelected[J] then
        begin
          LValues[LCount] := APublicVocabulary[J];
          Inc(LCount);
        end;
      if LCount <> Length(LQuota.Values) then
        raise EWfcTraining.Create('training value quota lost a public token');
      Result[I] := MakeWfcPipelineValueQuota(APublicPassIndex,
        LQuota.LabelText, LValues, LQuota.MinimumCount, LQuota.MaximumCount);
    end;
  finally
    LLookup.Free;
  end;
end;

function BuildRecipeConnectivities(const ADocument: TWfcTrainingDocument;
  const APublicPassIndex: Integer; const APublicVocabulary: TWfcModelTokens):
  TWfcPipelineConnectivities;
var
  I, J, LIndex, LCount: Integer;
  LLookup: TWfcTokenLookup;
  LSourceIndices: array of Integer;
  LConnectivity: TWfcTrainingConnectivity;
  LValues: TWfcPipelineConnectivityValues;
begin
  Result := nil;
  if ADocument.ConnectivityCount = 0 then Exit;
  LLookup := TWfcTokenLookup.Create(APublicVocabulary);
  try
    SetLength(Result, ADocument.ConnectivityCount);
    SetLength(LSourceIndices, Length(APublicVocabulary));
    for I := 0 to ADocument.ConnectivityCount - 1 do
    begin
      LConnectivity := ADocument.ConnectivityAt(I);
      for J := 0 to High(LSourceIndices) do LSourceIndices[J] := -1;
      for J := 0 to High(LConnectivity.Values) do
      begin
        LIndex := LLookup.Find(LConnectivity.Values[J].Value);
        if LIndex < 0 then raise EWfcTraining.CreateFmt(
          'training connectivity profile token is absent from learned public output [%d,%d]', [I, J]);
        LSourceIndices[LIndex] := J;
      end;
      SetLength(LValues, Length(LConnectivity.Values)); LCount := 0;
      for J := 0 to High(APublicVocabulary) do
        if LSourceIndices[J] >= 0 then
        begin
          LIndex := LSourceIndices[J];
          LValues[LCount] := MakeWfcPipelineConnectivityValue(APublicVocabulary[J],
            LConnectivity.Values[LIndex].Openings, LConnectivity.Values[LIndex].RequiredByValue);
          Inc(LCount);
        end;
      if LCount <> Length(LConnectivity.Values) then
        raise EWfcTraining.Create('training connectivity lost a public profile token');
      Result[I] := MakeWfcPipelineConnectivity(APublicPassIndex, LConnectivity.LabelText,
        LConnectivity.Root, LConnectivity.RequiredPositions, LValues, LConnectivity.RequireAllParticipants);
    end;
  finally LLookup.Free; end;
end;

function BuildSourceDescription(
  const ADocument: TWfcTrainingDocument): TWfcModelToken;
var
  I: Integer;
  LEncoded: String;
  LMetadata: TWfcTrainingMetadata;
  LOptions: TWfcTrainingOptions;
  LSample: TWfcTrainingSample;
begin
  LMetadata := ADocument.CopyMetadata;
  LOptions := ADocument.CopyOptions;
  Result := LMetadata.SourceDescription + ' | samples=';
  for I := 0 to ADocument.SampleCount - 1 do
  begin
    if I <> 0 then
      Result := Result + ',';
    LSample := ADocument.SampleAt(I);
    Result := Result + TWfcModelToken(IntToStr(I) + ':' +
      CanonicalToken(LSample.Name) + ':' + IntToStr(LSample.Width) + 'x' +
      IntToStr(LSample.Height));
    if LOptions.Kind in [wtkAdjacency3D,wtkPattern3D] then
      Result := Result + TWfcModelToken('x' + IntToStr(LSample.Depth));
    Result := Result + TWfcModelToken(':' +
      WfcTrainingSignatureHex(CalculateSampleSignature(LSample,
        LOptions.Kind in [wtkAdjacency3D,wtkPattern3D])));
  end;
  LEncoded := WfcTextEncodeToken(Result,
    'WFC training recipe source description');
  if Length(LEncoded) > WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH then
    raise EWfcTraining.Create(
      'training provenance exceeds the pipeline version-1 token limit');
end;

function LearnWfcTrainingRecipe(
  const ADocument: TWfcTrainingDocument): TWfcPipelineModel;
var
  I: Integer;
  LBridges: TWfcPipelineBridges;
  LDependencies: TWfcPipelineDependencies;
  LDocumentText: String;
  LFingerprint: TWfcModelToken;
  LMetadata: TWfcTrainingMetadata;
  LOptions: TWfcTrainingOptions;
  LPasses: TWfcPipelinePasses;
  LPipelineMetadata: TWfcPipelineMetadata;
  LPublicPassIndex: Integer;
  LPublicVocabulary: TWfcModelTokens;
  LRank: Integer;
  LResourceKind: TWfcPipelineResourceKind;
  LResources: TWfcPipelineResources;
  LSourceDescription: TWfcModelToken;
  LWrap: Boolean;
  LValueQuotas: TWfcPipelineValueQuotas;
  LConnectivities: TWfcPipelineConnectivities;
begin
  if ADocument = nil then
    raise EWfcTraining.Create('training document cannot be nil');
  LOptions := ADocument.CopyOptions;
  if (LOptions.Kind = wtkPattern2D) and
      (LOptions.Boundary <> wmbWrap) then
    raise EWfcTraining.Create(
      'pattern2d recipe export currently requires wrapped training input');

  LMetadata := ADocument.CopyMetadata;
  if LOptions.Kind = wtkPattern3D then
    LFingerprint := TWfcModelToken('wfclearn-v6/' +
      WfcTrainingSignatureHex(ADocument.Signature))
  else if WfcTrainingOptionsUseWrappedSequence(LOptions) then
    LFingerprint := TWfcModelToken('wfclearn-v5/' +
      WfcTrainingSignatureHex(ADocument.Signature))
  else if ADocument.ConnectivityCount <> 0 then
    LFingerprint := TWfcModelToken('wfclearn-v4/' +
      WfcTrainingSignatureHex(ADocument.Signature))
  else if ADocument.ValueQuotaCount <> 0 then
    LFingerprint := TWfcModelToken('wfclearn-v3/' +
      WfcTrainingSignatureHex(ADocument.Signature))
  else if LOptions.Kind = wtkAdjacency3D then
    LFingerprint := TWfcModelToken('wfclearn-v2/' +
      WfcTrainingSignatureHex(ADocument.Signature))
  else
    LFingerprint := TWfcModelToken('wfclearn-v1/' +
      WfcTrainingSignatureHex(ADocument.Signature));
  LSourceDescription := BuildSourceDescription(ADocument);
  { Recipe-specific provenance capacity is known from the immutable input.
    Reject it before dispatching the potentially expensive learner. }
  LDocumentText := LearnTrainingPayload(ADocument, LPublicVocabulary);
  LPipelineMetadata := MakeWfcPipelineMetadata(LMetadata.Name,
    LMetadata.LicenseIdentifier, LSourceDescription, LFingerprint);

  SetLength(LResources, 1);
  case LOptions.Kind of
    wtkAdjacency1D,
    wtkAdjacency2D,
    wtkAdjacency3D:
      LResourceKind := wprkModel;
    wtkPattern2D:
      LResourceKind := wprkPattern2D;
    wtkPattern3D:
      LResourceKind := wprkPattern3D;
    wtkSequence:
      LResourceKind := wprkSequence;
  else
    raise EWfcTraining.Create('unknown training kind');
  end;
  LResources[0] := MakeWfcPipelineResource('learned', LResourceKind,
    LDocumentText, LSourceDescription, LMetadata.LicenseIdentifier,
    LFingerprint);

  LDependencies := nil;
  LBridges := nil;
  case LOptions.Kind of
    wtkAdjacency1D:
      begin
        LRank := 1;
        SetLength(LPasses, 1);
        LPasses[0] := MakeWfcPipelinePass('output', wppvPublic,
          gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakModel, 0,
          False, wseWhole);
      end;
    wtkAdjacency2D:
      begin
        LRank := 2;
        SetLength(LPasses, 1);
        LPasses[0] := MakeWfcPipelinePass('output', wppvPublic,
          gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakModel, 0,
          False, wseWhole);
      end;
    wtkAdjacency3D:
      begin
        LRank := 3;
        SetLength(LPasses, 1);
        LPasses[0] := MakeWfcPipelinePass('output', wppvPublic,
          gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakModel, 0,
          False, wseWhole);
      end;
    wtkPattern2D:
      begin
        LRank := 2;
        SetLength(LPasses, 2);
        LPasses[0] := MakeWfcPipelinePass('patterns', wppvPrivate,
          gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakPattern2D, 0,
          False, wseWhole);
        LPasses[1] := MakeWfcPipelinePass('output', wppvPublic,
          gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakEmpty,
          WFC_PIPELINE_NO_INDEX, False, wseWhole);
        SetLength(LDependencies, 1);
        LDependencies[0] := MakeWfcPipelineDependency(1, 0);
        SetLength(LBridges, 1);
        LBridges[0] := MakeWfcPipelineBridge(
          wpbkPattern2DProjection, 0, 1);
      end;
    wtkPattern3D:
      begin
        LRank := 3;
        SetLength(LPasses,2);
        LPasses[0] := MakeWfcPipelinePass('patterns',wppvPrivate,
          gpmOverlay,WFC_PIPELINE_NO_INDEX,wpakPattern3D,0,False,wseWhole);
        LPasses[1] := MakeWfcPipelinePass('output',wppvPublic,
          gpmOverlay,WFC_PIPELINE_NO_INDEX,wpakEmpty,WFC_PIPELINE_NO_INDEX,False,wseWhole);
        SetLength(LDependencies,1); LDependencies[0] := MakeWfcPipelineDependency(1,0);
        SetLength(LBridges,1); LBridges[0] := MakeWfcPipelineBridge(wpbkPattern3DProjection,0,1);
      end;
    wtkSequence:
      begin
        LRank := 1;
        SetLength(LPasses, 2);
        LPasses[0] := MakeWfcPipelinePass('sequence', wppvPrivate,
          gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakSequence, 0,
          True, wseWhole);
        if LOptions.Boundary = wmbWrap then
          LPasses[0].SequenceExtent := wseWrap;
        LPasses[1] := MakeWfcPipelinePass('output', wppvPublic,
          gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakEmpty,
          WFC_PIPELINE_NO_INDEX, False, wseWhole);
        SetLength(LDependencies, 1);
        LDependencies[0] := MakeWfcPipelineDependency(1, 0);
        SetLength(LBridges, 1);
        LBridges[0] := MakeWfcPipelineBridge(
          wpbkSequenceProjection, 0, 1);
      end;
  else
    raise EWfcTraining.Create('unknown training kind');
  end;

  LWrap := LOptions.Boundary = wmbWrap;
  { Output topology belongs to the public same-size bridge. Source extraction
    remains independently open/wrapped inside the immutable learned resource. }
  if LOptions.Kind = wtkPattern3D then LWrap := True;
  LPublicPassIndex := WFC_PIPELINE_NO_INDEX;
  for I := 0 to Length(LPasses) - 1 do
    if LPasses[I].LabelName = 'output' then
      LPublicPassIndex := I;
  if LPublicPassIndex = WFC_PIPELINE_NO_INDEX then
    raise EWfcTraining.Create('training recipe has no public output pass');
  LValueQuotas := BuildRecipeValueQuotas(ADocument, LPublicPassIndex,
    LPublicVocabulary);
  LConnectivities := BuildRecipeConnectivities(ADocument, LPublicPassIndex, LPublicVocabulary);
  Result := TWfcPipelineModel.Create(LPipelineMetadata, LRank, LWrap,
    rmBottomUp, LResources, LPasses, LDependencies, LBridges, nil,
    LValueQuotas, LConnectivities);
end;

end.
