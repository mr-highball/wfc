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
  wfc_model,
  wfc_pipeline_model;

const
  WFC_TRAINING_VERSION = 1;

  WFC_TRAINING_MAX_SAMPLE_COUNT = 4096;
  WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT = 65536;
  WFC_TRAINING_MAX_DIMENSION = 65536;
  WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH = 65536;
  WFC_TRAINING_MAX_TOTAL_ENCODED_TOKEN_LENGTH = 4194304;
  WFC_TRAINING_MAX_FOOTPRINT_CELL_COUNT = 64;
  WFC_TRAINING_MAX_VISIT_COUNT = 16777216;
  WFC_TRAINING_MAX_ORDER = 64;

type
  EWfcTraining = class(Exception);

  TWfcTrainingKind = (
    wtkAdjacency1D,
    wtkAdjacency2D,
    wtkPattern2D,
    wtkSequence,
    wtkAdjacency3D
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
  end;

  { Immutable, pretokenized training request. Every dynamic input is detached
    at construction, and every dynamic accessor returns another detached copy. }
  TWfcTrainingDocument = class
  strict private
    FMetadata: TWfcTrainingMetadata;
    FOptions: TWfcTrainingOptions;
    FSamples: TWfcTrainingSamples;
    FTotalTokenCount: Integer;
    FSignature: Cardinal;
    function GetSampleCount: Integer;
    procedure ValidateSampleIndex(const AIndex: Integer);
  public
    constructor Create(const AMetadata: TWfcTrainingMetadata;
      const AOptions: TWfcTrainingOptions;
      const ASamples: TWfcTrainingSamples);

    function CopyMetadata: TWfcTrainingMetadata;
    function CopyOptions: TWfcTrainingOptions;
    function SampleAt(const AIndex: Integer): TWfcTrainingSample;
    function CopySamples: TWfcTrainingSamples;

    property SampleCount: Integer read GetSampleCount;
    property TotalTokenCount: Integer read FTotalTokenCount;
    property Signature: Cardinal read FSignature;
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
  AOrder: Integer): TWfcTrainingOptions;

function WfcTrainingSignatureHex(const ASignature: Cardinal): String;

function LearnWfcTrainingModelText(
  const ADocument: TWfcTrainingDocument): String;

function LearnWfcTrainingRecipe(
  const ADocument: TWfcTrainingDocument): TWfcPipelineModel;

implementation

uses
  wfc,
  wfc_learn,
  wfc_learn3d,
  wfc_model_text,
  wfc_pattern2d,
  wfc_pattern2d_learn,
  wfc_pattern2d_text,
  wfc_sequence,
  wfc_sequence_learn,
  wfc_sequence_text,
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

function CalculateTrainingSignature(const AMetadata: TWfcTrainingMetadata;
  const AOptions: TWfcTrainingOptions;
  const ASamples: TWfcTrainingSamples): Cardinal;
var
  I: Integer;
  J: Integer;
begin
  Result := FNV_OFFSET_BASIS;
  if AOptions.Kind = wtkAdjacency3D then
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
  HashAscii(Result, IntToStr(AOptions.Order));
  HashAscii(Result, IntToStr(Length(ASamples)));
  for I := 0 to Length(ASamples) - 1 do
  begin
    HashAscii(Result, CanonicalToken(ASamples[I].Name));
    HashAscii(Result, IntToStr(ASamples[I].Width));
    HashAscii(Result, IntToStr(ASamples[I].Height));
    if AOptions.Kind = wtkAdjacency3D then
      HashAscii(Result, IntToStr(ASamples[I].Depth));
    HashAscii(Result, IntToStr(Length(ASamples[I].Tokens)));
    for J := 0 to Length(ASamples[I].Tokens) - 1 do
      HashAscii(Result, CanonicalToken(ASamples[I].Tokens[J]));
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
        if AOptions.Boundary <> wmbOpen then
          raise EWfcTraining.Create('sequence training boundary must be open');
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
  end;
end;

procedure ValidateModelCapacities(const AOptions: TWfcTrainingOptions;
  const ASamples: TWfcTrainingSamples);
var
  LAdded: Boolean;
  LHistory: String;
  LHistorySize: Integer;
  LPosition: Integer;
  LPublicTokens: TTrainingStringSet;
  LSampleIndex: Integer;
  LStateCount: Integer;
  LStates: TTrainingStringSet;
  LTokenIndex: Integer;
  LTokenIndices: array of array of Integer;
  LTokenLimit: Integer;
  I: Integer;
  H: Integer;
begin
  case AOptions.Kind of
    wtkAdjacency1D,
    wtkAdjacency2D,
    wtkAdjacency3D:
      LTokenLimit := WFC_MODEL_MAX_VALUE_COUNT;
    wtkPattern2D:
      LTokenLimit := WFC_PATTERN_2D_MAX_PALETTE_COUNT;
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
        if LPosition < LHistorySize - H then
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
  const ASamples: TWfcTrainingSamples; out ATotalTokenCount: Integer);
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
  else
    LFootprintCells := 1;

  for I := 0 to Length(ASamples) - 1 do
  begin
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
    if (AOptions.Kind = wtkAdjacency3D) and
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
    if AOptions.Kind = wtkAdjacency3D then
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

  ValidateModelCapacities(AOptions, ASamples);
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
end;

function WfcTrainingSignatureHex(const ASignature: Cardinal): String;
begin
  Result := IntToHex(ASignature, 8);
end;

{ TWfcTrainingDocument }

constructor TWfcTrainingDocument.Create(
  const AMetadata: TWfcTrainingMetadata;
  const AOptions: TWfcTrainingOptions;
  const ASamples: TWfcTrainingSamples);
var
  LTotalTokenCount: Integer;
begin
  inherited Create;
  ValidateTrainingInput(AMetadata, AOptions, ASamples, LTotalTokenCount);
  FMetadata := AMetadata;
  FOptions := AOptions;
  FSamples := CloneSamples(ASamples, AOptions.Kind = wtkAdjacency3D);
  FTotalTokenCount := LTotalTokenCount;
  FSignature := CalculateTrainingSignature(FMetadata, FOptions, FSamples);
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

function LearnWfcTrainingModelText(
  const ADocument: TWfcTrainingDocument): String;
var
  LModel: TWfcModel;
  LOptions: TWfcTrainingOptions;
  LPattern: TWfcOverlappingModel2D;
  LSamples: TWfcLearnSamples;
  LSequence: TWfcSequenceModel;
  LSequenceSamples: TWfcSequenceSamples;
  LVolumeSamples: TWfcLearnVolumeSamples;
begin
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
        finally
          LPattern.Free;
        end;
      end;
    wtkSequence:
      begin
        LSequenceSamples := BuildSequenceSamples(ADocument);
        LSequence := LearnSequenceModelCorpus(LSequenceSamples,
          LOptions.Order);
        try
          Result := EncodeWfcSequenceText(LSequence);
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
        finally
          LModel.Free;
        end;
      end;
  else
    raise EWfcTraining.Create('unknown training kind');
  end;
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
    if LOptions.Kind = wtkAdjacency3D then
      Result := Result + TWfcModelToken('x' + IntToStr(LSample.Depth));
    Result := Result + TWfcModelToken(':' +
      WfcTrainingSignatureHex(CalculateSampleSignature(LSample,
        LOptions.Kind = wtkAdjacency3D)));
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
  LBridges: TWfcPipelineBridges;
  LDependencies: TWfcPipelineDependencies;
  LDocumentText: String;
  LFingerprint: TWfcModelToken;
  LMetadata: TWfcTrainingMetadata;
  LOptions: TWfcTrainingOptions;
  LPasses: TWfcPipelinePasses;
  LPipelineMetadata: TWfcPipelineMetadata;
  LRank: Integer;
  LResourceKind: TWfcPipelineResourceKind;
  LResources: TWfcPipelineResources;
  LSourceDescription: TWfcModelToken;
  LWrap: Boolean;
begin
  if ADocument = nil then
    raise EWfcTraining.Create('training document cannot be nil');
  LOptions := ADocument.CopyOptions;
  if (LOptions.Kind = wtkPattern2D) and
      (LOptions.Boundary <> wmbWrap) then
    raise EWfcTraining.Create(
      'pattern2d recipe export currently requires wrapped training input');

  LMetadata := ADocument.CopyMetadata;
  if LOptions.Kind = wtkAdjacency3D then
    LFingerprint := TWfcModelToken('wfclearn-v2/' +
      WfcTrainingSignatureHex(ADocument.Signature))
  else
    LFingerprint := TWfcModelToken('wfclearn-v1/' +
      WfcTrainingSignatureHex(ADocument.Signature));
  LSourceDescription := BuildSourceDescription(ADocument);
  { Recipe-specific provenance capacity is known from the immutable input.
    Reject it before dispatching the potentially expensive learner. }
  LDocumentText := LearnWfcTrainingModelText(ADocument);
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
    wtkSequence:
      begin
        LRank := 1;
        SetLength(LPasses, 2);
        LPasses[0] := MakeWfcPipelinePass('sequence', wppvPrivate,
          gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakSequence, 0,
          True, wseWhole);
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
  Result := TWfcPipelineModel.Create(LPipelineMetadata, LRank, LWrap,
    rmBottomUp, LResources, LPasses, LDependencies, LBridges, nil);
end;

end.
