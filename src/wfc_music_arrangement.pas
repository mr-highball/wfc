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
unit wfc_music_arrangement;

{$mode delphi}{$H+}

interface

uses
  wfc,
  wfc_model,
  wfc_music,
  wfc_music_passes;

const
  WFC_MUSIC_ARRANGEMENT_VERSION = 1;
  WFC_MUSIC_ARRANGEMENT_DEFAULT_SECTION_CELL_COUNT = 16;
  WFC_MUSIC_ARRANGEMENT_DEFAULT_CONTINUITY_CELL_COUNT = 2;

type
  EWfcMusicArrangement = class(EWfcMusic);

  { Browser arithmetic is exact through 2^53-1. Native builds deliberately
    use the same public ceiling so arrangement plans replay across targets. }
  {$IFDEF PAS2JS}
  TWfcMusicArrangementWide = NativeInt;
  {$ELSE}
  TWfcMusicArrangementWide = Int64;
  {$ENDIF}

const
  WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER:
    TWfcMusicArrangementWide = 9007199254740991;

type
  TWfcMusicArrangementRounding = (
    wmarExact,
    wmarFloorToCell,
    wmarCeilToCell
  );

  TWfcMusicArrangementStatus = (
    wmasReady,
    wmasActive,
    wmasCompleted,
    wmasCancelled,
    wmasFailed
  );

  TWfcMusicArrangementStep = (
    wmaspProduced,
    wmaspCompleted,
    wmaspCancelled,
    wmaspFailed
  );

  TWfcMusicArrangementConfig = record
    RequestedTicks: TWfcMusicArrangementWide;
    QuantumTicks: Integer;
    SectionCellCount: Integer;
    ContinuityCellCount: Integer;
    BaseSeed: TGraphSeed;
    Rounding: TWfcMusicArrangementRounding;
  end;

  { A bounded, detached tail supplied to the next section source. The source
    owns model-specific transition checks; the iterator owns exact provenance
    and never treats a context as proof of global satisfiability. }
  TWfcMusicArrangementContext = record
    HasPrevious: Boolean;
    EndTick: TWfcMusicArrangementWide;
    HarmonyTokens: TWfcModelTokens;
    RhythmTokens: TWfcModelTokens;
    MelodyTokens: TWfcModelTokens;
  end;

  TWfcMusicArrangementSectionRequest = record
    Index: TWfcMusicArrangementWide;
    StartTick: TWfcMusicArrangementWide;
    LengthTicks: Integer;
    CellCount: Integer;
    Seed: TGraphSeed;
    PriorContext: TWfcMusicArrangementContext;
  end;

  { Caller-owned section generators must return a newly owned composition on
    success. Expected finite failures return False and a useful message.
    ValidateContinuity performs model-specific boundary checks against the
    iterator-authenticated tail. Unexpected exceptions propagate. }
  TWfcMusicArrangementSectionSource = class
  public
    function GenerateSection(
      const ARequest: TWfcMusicArrangementSectionRequest;
      out AComposition: TWfcMusicComposition;
      out AFailure: String): Boolean; virtual; abstract;
    function ValidateContinuity(
      const ARequest: TWfcMusicArrangementSectionRequest;
      const AComposition: TWfcMusicComposition;
      out AFailure: String): Boolean; virtual; abstract;
  end;

  { Owns Composition. Callers free each yielded section after consuming it;
    the iterator itself retains no score or composition. }
  TWfcMusicArrangementSection = class
  private
    FIndex: TWfcMusicArrangementWide;
    FStartTick: TWfcMusicArrangementWide;
    FLengthTicks: Integer;
    FCellCount: Integer;
    FSeed: TGraphSeed;
    FComposition: TWfcMusicComposition;
    constructor Create(
      const ARequest: TWfcMusicArrangementSectionRequest;
      const AComposition: TWfcMusicComposition);
  public
    destructor Destroy; override;
    property Index: TWfcMusicArrangementWide read FIndex;
    property StartTick: TWfcMusicArrangementWide read FStartTick;
    property LengthTicks: Integer read FLengthTicks;
    property CellCount: Integer read FCellCount;
    property Seed: TGraphSeed read FSeed;
    property Composition: TWfcMusicComposition read FComposition;
  end;

  { Lazy deterministic arrangement planner. Cancellation is observed between
    synchronous Next calls. A failed step returns no candidate section and is
    terminal; previously transferred sections remain caller-owned. }
  TWfcMusicArrangement = class
  strict private
    FConfig: TWfcMusicArrangementConfig;
    FSource: TWfcMusicArrangementSectionSource;
    FActualTicks: TWfcMusicArrangementWide;
    FProducedTicks: TWfcMusicArrangementWide;
    FNextIndex: TWfcMusicArrangementWide;
    FContext: TWfcMusicArrangementContext;
    FStatus: TWfcMusicArrangementStatus;
    FFailure: String;
    FFormatKnown: Boolean;
    FTicksPerQuarter: Integer;
    FStepsPerOctave: Integer;
    function GetRequestedTicks: TWfcMusicArrangementWide;
    function GetRemainingTicks: TWfcMusicArrangementWide;
    function GetSectionCount: TWfcMusicArrangementWide;
    procedure SetFailure(const AMessage: String);
    function BuildRequest: TWfcMusicArrangementSectionRequest;
    procedure ValidateCandidate(
      const ARequest: TWfcMusicArrangementSectionRequest;
      const AComposition: TWfcMusicComposition);
    procedure UpdateContext(
      const ARequest: TWfcMusicArrangementSectionRequest;
      const AComposition: TWfcMusicComposition);
  public
    constructor Create(const AConfig: TWfcMusicArrangementConfig;
      const ASource: TWfcMusicArrangementSectionSource);
    function Next(out ASection: TWfcMusicArrangementSection):
      TWfcMusicArrangementStep;
    procedure Cancel;
    function CopyContext: TWfcMusicArrangementContext;
    property RequestedTicks: TWfcMusicArrangementWide
      read GetRequestedTicks;
    property ActualTicks: TWfcMusicArrangementWide read FActualTicks;
    property ProducedTicks: TWfcMusicArrangementWide read FProducedTicks;
    property RemainingTicks: TWfcMusicArrangementWide
      read GetRemainingTicks;
    property SectionCount: TWfcMusicArrangementWide read GetSectionCount;
    property NextIndex: TWfcMusicArrangementWide read FNextIndex;
    property Status: TWfcMusicArrangementStatus read FStatus;
    property Failure: String read FFailure;
  end;

function MakeWfcMusicArrangementConfig(
  const ARequestedTicks: TWfcMusicArrangementWide;
  const AQuantumTicks, ASectionCellCount,
  AContinuityCellCount: Integer;
  const ABaseSeed: TGraphSeed;
  const ARounding: TWfcMusicArrangementRounding):
  TWfcMusicArrangementConfig;

function DefaultWfcMusicArrangementConfig(
  const ARequestedTicks: TWfcMusicArrangementWide;
  const AQuantumTicks: Integer;
  const ABaseSeed: TGraphSeed): TWfcMusicArrangementConfig;

function ResolveWfcMusicArrangementTicks(
  const ARequestedTicks: TWfcMusicArrangementWide;
  const AQuantumTicks: Integer;
  const ARounding: TWfcMusicArrangementRounding):
  TWfcMusicArrangementWide;

function WfcMusicArrangementSectionSeed(const ABaseSeed: TGraphSeed;
  const ASectionIndex: TWfcMusicArrangementWide): TGraphSeed;

function WfcMusicArrangementStatusName(
  const AStatus: TWfcMusicArrangementStatus): String;

implementation

uses
  SysUtils,
  wfc_music_sequence,
  wfc_sequence_graph;

procedure ArrangementError(const AMessage: String);
begin
  raise EWfcMusicArrangement.Create('invalid music arrangement: ' + AMessage);
end;

procedure ValidateWideNonnegative(const AValue: TWfcMusicArrangementWide;
  const ALabel: String);
begin
  if (AValue < 0) or
      (AValue > WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER) then
    ArrangementError(ALabel + ' is outside the portable safe-integer range');
end;

function CheckedWideAdd(const A, B: TWfcMusicArrangementWide;
  const ALabel: String): TWfcMusicArrangementWide;
begin
  ValidateWideNonnegative(A, ALabel);
  ValidateWideNonnegative(B, ALabel);
  if A > WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER - B then
    ArrangementError(ALabel + ' exceeds the portable safe-integer range');
  Result := A + B;
end;

function CheckedSectionTicks(const ASectionCellCount,
  AQuantumTicks: Integer): Integer;
begin
  if ASectionCellCount < 1 then
    ArrangementError('section cell count must be positive');
  if AQuantumTicks < 1 then
    ArrangementError('quantum ticks must be positive');
  if ASectionCellCount > High(Integer) div AQuantumTicks then
    ArrangementError('section tick length exceeds the Integer range');
  Result := ASectionCellCount * AQuantumTicks;
end;

procedure ValidateRounding(const ARounding: TWfcMusicArrangementRounding);
begin
  case ARounding of
    wmarExact, wmarFloorToCell, wmarCeilToCell: Exit;
  end;
  ArrangementError('unknown duration-rounding policy');
end;

function MakeWfcMusicArrangementConfig(
  const ARequestedTicks: TWfcMusicArrangementWide;
  const AQuantumTicks, ASectionCellCount,
  AContinuityCellCount: Integer;
  const ABaseSeed: TGraphSeed;
  const ARounding: TWfcMusicArrangementRounding):
  TWfcMusicArrangementConfig;
begin
  Result.RequestedTicks := ARequestedTicks;
  Result.QuantumTicks := AQuantumTicks;
  Result.SectionCellCount := ASectionCellCount;
  Result.ContinuityCellCount := AContinuityCellCount;
  Result.BaseSeed := ABaseSeed;
  Result.Rounding := ARounding;
end;

function DefaultWfcMusicArrangementConfig(
  const ARequestedTicks: TWfcMusicArrangementWide;
  const AQuantumTicks: Integer;
  const ABaseSeed: TGraphSeed): TWfcMusicArrangementConfig;
begin
  Result := MakeWfcMusicArrangementConfig(ARequestedTicks,
    AQuantumTicks, WFC_MUSIC_ARRANGEMENT_DEFAULT_SECTION_CELL_COUNT,
    WFC_MUSIC_ARRANGEMENT_DEFAULT_CONTINUITY_CELL_COUNT,
    ABaseSeed, wmarExact);
end;

function ResolveWfcMusicArrangementTicks(
  const ARequestedTicks: TWfcMusicArrangementWide;
  const AQuantumTicks: Integer;
  const ARounding: TWfcMusicArrangementRounding):
  TWfcMusicArrangementWide;
var
  LRemainder: TWfcMusicArrangementWide;
begin
  ValidateRounding(ARounding);
  if ARequestedTicks < 1 then
    ArrangementError('requested ticks must be positive');
  ValidateWideNonnegative(ARequestedTicks, 'requested ticks');
  if AQuantumTicks < 1 then
    ArrangementError('quantum ticks must be positive');
  LRemainder := ARequestedTicks mod AQuantumTicks;
  case ARounding of
    wmarExact:
      begin
        if LRemainder <> 0 then
          ArrangementError('requested ticks are not aligned to the quantum');
        Result := ARequestedTicks;
      end;
    wmarFloorToCell:
      begin
        Result := ARequestedTicks - LRemainder;
        if Result < 1 then
          ArrangementError('rounded arrangement duration is empty');
      end;
    wmarCeilToCell:
      if LRemainder = 0 then
        Result := ARequestedTicks
      else
        Result := CheckedWideAdd(ARequestedTicks,
          AQuantumTicks - LRemainder, 'rounded arrangement duration');
  else
    Result := 0;
  end;
end;

procedure HashSeedByte(var AHash: Cardinal; const AValue: Byte);
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

procedure HashSeedCardinal(var AHash: Cardinal; const AValue: Cardinal);
begin
  HashSeedByte(AHash, Byte(AValue and $FF));
  HashSeedByte(AHash, Byte((AValue shr 8) and $FF));
  HashSeedByte(AHash, Byte((AValue shr 16) and $FF));
  HashSeedByte(AHash, Byte((AValue shr 24) and $FF));
end;

function WfcMusicArrangementSectionSeed(const ABaseSeed: TGraphSeed;
  const ASectionIndex: TWfcMusicArrangementWide): TGraphSeed;
var
  I: Integer;
  LHash: Cardinal;
  LIndex: TWfcMusicArrangementWide;
begin
  ValidateWideNonnegative(ASectionIndex, 'section index');
  LHash := Cardinal(2166136261);
  HashSeedByte(LHash, WFC_MUSIC_ARRANGEMENT_VERSION);
  HashSeedCardinal(LHash, Cardinal(ABaseSeed));
  LIndex := ASectionIndex;
  for I := 0 to 7 do
  begin
    HashSeedByte(LHash, Byte(LIndex mod 256));
    LIndex := LIndex div 256;
  end;
  Result := TGraphSeed(LHash);
end;

function WfcMusicArrangementStatusName(
  const AStatus: TWfcMusicArrangementStatus): String;
begin
  case AStatus of
    wmasReady: Result := 'ready';
    wmasActive: Result := 'active';
    wmasCompleted: Result := 'completed';
    wmasCancelled: Result := 'cancelled';
    wmasFailed: Result := 'failed';
  else
    Result := 'unknown';
  end;
end;

function CopyTokens(const ASource: TWfcModelTokens): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ASource));
  for I := 0 to Length(ASource) - 1 do
    Result[I] := ASource[I];
end;

function CloneContext(const ASource: TWfcMusicArrangementContext):
  TWfcMusicArrangementContext;
begin
  Result.HasPrevious := ASource.HasPrevious;
  Result.EndTick := ASource.EndTick;
  Result.HarmonyTokens := CopyTokens(ASource.HarmonyTokens);
  Result.RhythmTokens := CopyTokens(ASource.RhythmTokens);
  Result.MelodyTokens := CopyTokens(ASource.MelodyTokens);
end;

function AppendTail(const APrior, ACurrent: TWfcModelTokens;
  const AMaximum: Integer): TWfcModelTokens;
var
  I: Integer;
  LCurrentStart: Integer;
  LKeepCurrent: Integer;
  LKeepPrior: Integer;
  LPriorStart: Integer;
begin
  Result := nil;
  if AMaximum <= 0 then
    Exit;
  LKeepCurrent := Length(ACurrent);
  if LKeepCurrent > AMaximum then
    LKeepCurrent := AMaximum;
  LKeepPrior := AMaximum - LKeepCurrent;
  if LKeepPrior > Length(APrior) then
    LKeepPrior := Length(APrior);
  SetLength(Result, LKeepPrior + LKeepCurrent);
  LPriorStart := Length(APrior) - LKeepPrior;
  for I := 0 to LKeepPrior - 1 do
    Result[I] := APrior[LPriorStart + I];
  LCurrentStart := Length(ACurrent) - LKeepCurrent;
  for I := 0 to LKeepCurrent - 1 do
    Result[LKeepPrior + I] := ACurrent[LCurrentStart + I];
end;

{ TWfcMusicArrangementSection }

constructor TWfcMusicArrangementSection.Create(
  const ARequest: TWfcMusicArrangementSectionRequest;
  const AComposition: TWfcMusicComposition);
begin
  inherited Create;
  FIndex := ARequest.Index;
  FStartTick := ARequest.StartTick;
  FLengthTicks := ARequest.LengthTicks;
  FCellCount := ARequest.CellCount;
  FSeed := ARequest.Seed;
  FComposition := AComposition;
end;

destructor TWfcMusicArrangementSection.Destroy;
begin
  FComposition.Free;
  inherited Destroy;
end;

{ TWfcMusicArrangement }

constructor TWfcMusicArrangement.Create(
  const AConfig: TWfcMusicArrangementConfig;
  const ASource: TWfcMusicArrangementSectionSource);
begin
  inherited Create;
  if not Assigned(ASource) then
    ArrangementError('section source is not assigned');
  CheckedSectionTicks(AConfig.SectionCellCount, AConfig.QuantumTicks);
  if AConfig.ContinuityCellCount < 0 then
    ArrangementError('continuity cell count cannot be negative');
  FActualTicks := ResolveWfcMusicArrangementTicks(AConfig.RequestedTicks,
    AConfig.QuantumTicks, AConfig.Rounding);
  FConfig := AConfig;
  FSource := ASource;
  FProducedTicks := 0;
  FNextIndex := 0;
  FContext := Default(TWfcMusicArrangementContext);
  FStatus := wmasReady;
  FFailure := '';
  FFormatKnown := False;
  FTicksPerQuarter := 0;
  FStepsPerOctave := 0;
end;

function TWfcMusicArrangement.GetRequestedTicks:
  TWfcMusicArrangementWide;
begin
  Result := FConfig.RequestedTicks;
end;

function TWfcMusicArrangement.GetRemainingTicks:
  TWfcMusicArrangementWide;
begin
  Result := FActualTicks - FProducedTicks;
end;

function TWfcMusicArrangement.GetSectionCount:
  TWfcMusicArrangementWide;
var
  LSectionTicks: Integer;
begin
  LSectionTicks := CheckedSectionTicks(FConfig.SectionCellCount,
    FConfig.QuantumTicks);
  Result := FActualTicks div LSectionTicks;
  if (FActualTicks mod LSectionTicks) <> 0 then
    Result := Result + 1;
end;

procedure TWfcMusicArrangement.SetFailure(const AMessage: String);
begin
  FFailure := AMessage;
  if FFailure = '' then
    FFailure := 'music arrangement section generation failed';
  FStatus := wmasFailed;
end;

function TWfcMusicArrangement.BuildRequest:
  TWfcMusicArrangementSectionRequest;
var
  LCellCount: Integer;
  LRemaining: TWfcMusicArrangementWide;
  LSectionTicks: Integer;
begin
  LSectionTicks := CheckedSectionTicks(FConfig.SectionCellCount,
    FConfig.QuantumTicks);
  LRemaining := GetRemainingTicks;
  if LRemaining >= LSectionTicks then
    LCellCount := FConfig.SectionCellCount
  else
    LCellCount := Integer(LRemaining div FConfig.QuantumTicks);
  if LCellCount < 1 then
    ArrangementError('remaining arrangement duration is not a complete cell');
  Result.Index := FNextIndex;
  Result.StartTick := FProducedTicks;
  Result.CellCount := LCellCount;
  Result.LengthTicks := LCellCount * FConfig.QuantumTicks;
  Result.Seed := WfcMusicArrangementSectionSeed(
    FConfig.BaseSeed, FNextIndex);
  Result.PriorContext := CloneContext(FContext);
end;

procedure TWfcMusicArrangement.ValidateCandidate(
  const ARequest: TWfcMusicArrangementSectionRequest;
  const AComposition: TWfcMusicComposition);
var
  LChecked: TWfcMusicComposition;
  LFirst: TWfcMusicMelodyCell;
  LHarmony: TWfcGeneratedSequence;
  LMelody: TWfcGeneratedSequence;
  LRhythm: TWfcGeneratedSequence;
  LScore: TWfcMusicScore;
begin
  if not Assigned(AComposition) then
    ArrangementError('section source returned no composition');
  if AComposition.Seed <> ARequest.Seed then
    ArrangementError('section composition seed differs from its request');
  if AComposition.QuantumTicks <> FConfig.QuantumTicks then
    ArrangementError('section composition quantum differs from its request');
  if AComposition.CellCount <> ARequest.CellCount then
    ArrangementError('section composition cell count differs from its request');
  if CalculateWfcMusicCompositionSignature(AComposition) <>
      AComposition.Signature then
    ArrangementError('section composition signature is invalid');

  LHarmony := AComposition.CopyGenerated(wmplHarmony);
  LRhythm := AComposition.CopyGenerated(wmplRhythm);
  LMelody := AComposition.CopyGenerated(wmplMelody);
  LScore := AComposition.CopyScore;
  try
    if LScore.LengthTicks <> ARequest.LengthTicks then
      ArrangementError('section score length differs from its request');
    if not FFormatKnown then
    begin
      FTicksPerQuarter := LScore.TicksPerQuarter;
      FStepsPerOctave := LScore.StepsPerOctave;
    end
    else if (LScore.TicksPerQuarter <> FTicksPerQuarter) or
        (LScore.StepsPerOctave <> FStepsPerOctave) then
      ArrangementError(
        'section score tick or pitch format differs from earlier sections');

    LChecked := CreateWfcMusicComposition(AComposition.Seed,
      AComposition.QuantumTicks, LHarmony.Tokens, LRhythm.Tokens,
      LMelody.Tokens, LScore);
    LChecked.Free;
    LFirst := DecodeWfcMusicMelodyCell(LMelody.Tokens[0]);
    if LFirst.Action = wmcaHold then
      ArrangementError(
        'a section must begin with an attack or rest, not a hidden sustain');
  finally
    LScore.Free;
  end;
end;

procedure TWfcMusicArrangement.UpdateContext(
  const ARequest: TWfcMusicArrangementSectionRequest;
  const AComposition: TWfcMusicComposition);
var
  LHarmony: TWfcGeneratedSequence;
  LMelody: TWfcGeneratedSequence;
  LNew: TWfcMusicArrangementContext;
  LRhythm: TWfcGeneratedSequence;
begin
  LHarmony := AComposition.CopyGenerated(wmplHarmony);
  LRhythm := AComposition.CopyGenerated(wmplRhythm);
  LMelody := AComposition.CopyGenerated(wmplMelody);
  LNew := Default(TWfcMusicArrangementContext);
  LNew.HasPrevious := True;
  LNew.EndTick := CheckedWideAdd(ARequest.StartTick,
    ARequest.LengthTicks, 'arrangement progress');
  LNew.HarmonyTokens := AppendTail(FContext.HarmonyTokens,
    LHarmony.Tokens, FConfig.ContinuityCellCount);
  LNew.RhythmTokens := AppendTail(FContext.RhythmTokens,
    LRhythm.Tokens, FConfig.ContinuityCellCount);
  LNew.MelodyTokens := AppendTail(FContext.MelodyTokens,
    LMelody.Tokens, FConfig.ContinuityCellCount);
  FContext := LNew;
end;

function TWfcMusicArrangement.Next(
  out ASection: TWfcMusicArrangementSection):
  TWfcMusicArrangementStep;
var
  LCandidate: TWfcMusicComposition;
  LFailure: String;
  LGenerated: Boolean;
  LRequest: TWfcMusicArrangementSectionRequest;
begin
  ASection := nil;
  case FStatus of
    wmasReady, wmasActive: ;
    wmasCompleted: Exit(wmaspCompleted);
    wmasCancelled: Exit(wmaspCancelled);
    wmasFailed: Exit(wmaspFailed);
  end;
  if FProducedTicks = FActualTicks then
  begin
    FStatus := wmasCompleted;
    Exit(wmaspCompleted);
  end;

  FStatus := wmasActive;
  LRequest := BuildRequest;
  LCandidate := nil;
  LFailure := '';
  try
    try
      LGenerated := FSource.GenerateSection(LRequest,
        LCandidate, LFailure);
    except
      on E: EOutOfMemory do
        raise;
      on E: Exception do
      begin
        SetFailure('section source exception: ' + E.Message);
        raise;
      end;
    end;
    if not LGenerated then
    begin
      SetFailure(LFailure);
      Exit(wmaspFailed);
    end;

    try
      ValidateCandidate(LRequest, LCandidate);
    except
      on E: EOutOfMemory do
        raise;
      on E: Exception do
      begin
        SetFailure(E.Message);
        Exit(wmaspFailed);
      end;
    end;

    { Rebuild the request so a source cannot mutate managed context aliases
      received by GenerateSection before its continuity check. }
    LRequest := BuildRequest;
    try
      LGenerated := FSource.ValidateContinuity(LRequest,
        LCandidate, LFailure);
    except
      on E: EOutOfMemory do
        raise;
      on E: Exception do
      begin
        SetFailure('section continuity exception: ' + E.Message);
        raise;
      end;
    end;
    if not LGenerated then
    begin
      if LFailure = '' then
        LFailure := 'section source rejected temporal continuity';
      SetFailure(LFailure);
      Exit(wmaspFailed);
    end;

    UpdateContext(LRequest, LCandidate);
    ASection := TWfcMusicArrangementSection.Create(LRequest, LCandidate);
    LCandidate := nil;
    FProducedTicks := CheckedWideAdd(FProducedTicks,
      ASection.LengthTicks, 'arrangement progress');
    Inc(FNextIndex);
    FFormatKnown := True;
    if FProducedTicks = FActualTicks then
      FStatus := wmasCompleted;
    Result := wmaspProduced;
  finally
    LCandidate.Free;
  end;
end;

procedure TWfcMusicArrangement.Cancel;
begin
  if FStatus in [wmasReady, wmasActive] then
  begin
    FStatus := wmasCancelled;
    FFailure := '';
  end;
end;

function TWfcMusicArrangement.CopyContext:
  TWfcMusicArrangementContext;
begin
  Result := CloneContext(FContext);
end;

end.
