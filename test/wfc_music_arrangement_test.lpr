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
program wfc_music_arrangement_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}
  wfc_browser_test_host,
  {$ENDIF}
  SysUtils,
  wfc,
  wfc_model,
  wfc_music,
  wfc_music_arrangement,
  wfc_music_passes,
  wfc_music_sequence;

type
  TTestSectionSource = class(TWfcMusicArrangementSectionSource)
  private
    FGenerateCalls: Integer;
    FContinuityCalls: Integer;
    FFailIndex: TWfcMusicArrangementWide;
    FContinuityFailIndex: TWfcMusicArrangementWide;
    FWrongSeedIndex: TWfcMusicArrangementWide;
    FRaiseIndex: TWfcMusicArrangementWide;
    FSawPrior: Boolean;
    FLastPriorCount: Integer;
    FLastPriorEnd: TWfcMusicArrangementWide;
    function BuildComposition(
      const ARequest: TWfcMusicArrangementSectionRequest;
      const AWrongSeed: Boolean): TWfcMusicComposition;
  public
    constructor Create;
    function GenerateSection(
      const ARequest: TWfcMusicArrangementSectionRequest;
      out AComposition: TWfcMusicComposition;
      out AFailure: String): Boolean; override;
    function ValidateContinuity(
      const ARequest: TWfcMusicArrangementSectionRequest;
      const AComposition: TWfcMusicComposition;
      out AFailure: String): Boolean; override;
    property GenerateCalls: Integer read FGenerateCalls;
    property ContinuityCalls: Integer read FContinuityCalls;
    property FailIndex: TWfcMusicArrangementWide
      read FFailIndex write FFailIndex;
    property ContinuityFailIndex: TWfcMusicArrangementWide
      read FContinuityFailIndex write FContinuityFailIndex;
    property WrongSeedIndex: TWfcMusicArrangementWide
      read FWrongSeedIndex write FWrongSeedIndex;
    property RaiseIndex: TWfcMusicArrangementWide
      read FRaiseIndex write FRaiseIndex;
    property SawPrior: Boolean read FSawPrior;
    property LastPriorCount: Integer read FLastPriorCount;
    property LastPriorEnd: TWfcMusicArrangementWide read FLastPriorEnd;
  end;

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GCheckCount);
  if ACondition then
    WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailureCount);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

procedure BeginTest(const AName: String);
begin
  WriteLn('[TEST] ', AName);
end;

function TracksOfOne: TWfcMusicTracks;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0] := MakeWfcMusicTrack('lead', 'Arrangement Lead');
end;

function VoicesOfOne: TWfcMusicVoices;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0] := MakeWfcMusicVoice(0, 'melody');
end;

function MetersForLength(const ALengthTicks: Integer):
  TWfcMusicMeterChanges;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0] := MakeWfcMusicMeterChange(0, ALengthTicks, 4);
end;

function TemposOfOne: TWfcMusicTempoChanges;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0] := MakeWfcMusicTempoChange(0, 500000);
end;

function TTestSectionSource.BuildComposition(
  const ARequest: TWfcMusicArrangementSectionRequest;
  const AWrongSeed: Boolean): TWfcMusicComposition;
var
  I: Integer;
  LCells: TWfcMusicMelodyCells;
  LHarmony: TWfcMusicHarmonyCells;
  LLengthTicks: Integer;
  LMelodyTokens: TWfcModelTokens;
  LRhythm: TWfcMusicRhythmCells;
  LScore: TWfcMusicScore;
  LSeed: TGraphSeed;
begin
  Result := nil;
  SetLength(LCells, ARequest.CellCount);
  for I := 0 to Length(LCells) - 1 do
    if (I and 1) = 0 then
      LCells[I] := MakeWfcMusicAttackCell(
        60 + Integer(ARequest.Index mod 12), 96)
    else
      LCells[I] := MakeWfcMusicRestCell;
  LLengthTicks := ARequest.CellCount *
    (ARequest.LengthTicks div ARequest.CellCount);
  LScore := TWfcMusicScore.Create(1, 12, LLengthTicks,
    TracksOfOne, VoicesOfOne, MetersForLength(LLengthTicks),
    TemposOfOne, RebuildWfcMusicVoiceSpans(LCells, 0,
      ARequest.LengthTicks div ARequest.CellCount));
  try
    LRhythm := ProjectWfcMusicMelodyToRhythm(LCells);
    LHarmony := ProjectWfcMusicMelodyToHarmony(LCells, 12);
    LMelodyTokens := EncodeWfcMusicMelodyCells(LCells);
    LSeed := ARequest.Seed;
    if AWrongSeed then
      LSeed := LSeed xor Cardinal(1);
    Result := CreateWfcMusicComposition(LSeed,
      ARequest.LengthTicks div ARequest.CellCount,
      EncodeWfcMusicHarmonyCells(LHarmony),
      EncodeWfcMusicRhythmCells(LRhythm), LMelodyTokens, LScore);
  finally
    LScore.Free;
  end;
end;

constructor TTestSectionSource.Create;
begin
  inherited Create;
  FGenerateCalls := 0;
  FContinuityCalls := 0;
  FFailIndex := -1;
  FContinuityFailIndex := -1;
  FWrongSeedIndex := -1;
  FRaiseIndex := -1;
  FSawPrior := False;
  FLastPriorCount := 0;
  FLastPriorEnd := 0;
end;

function TTestSectionSource.GenerateSection(
  const ARequest: TWfcMusicArrangementSectionRequest;
  out AComposition: TWfcMusicComposition;
  out AFailure: String): Boolean;
begin
  Inc(FGenerateCalls);
  AComposition := nil;
  AFailure := '';
  if ARequest.Index = FRaiseIndex then
    raise Exception.Create('synthetic source exception');
  AComposition := BuildComposition(ARequest,
    ARequest.Index = FWrongSeedIndex);
  if ARequest.Index = FFailIndex then
  begin
    AFailure := 'synthetic finite source failure';
    Exit(False);
  end;
  Result := True;
end;

function TTestSectionSource.ValidateContinuity(
  const ARequest: TWfcMusicArrangementSectionRequest;
  const AComposition: TWfcMusicComposition;
  out AFailure: String): Boolean;
begin
  Inc(FContinuityCalls);
  AFailure := '';
  if ARequest.PriorContext.HasPrevious then
  begin
    FSawPrior := True;
    FLastPriorCount := Length(ARequest.PriorContext.MelodyTokens);
    FLastPriorEnd := ARequest.PriorContext.EndTick;
  end;
  if ARequest.Index = FContinuityFailIndex then
  begin
    AFailure := 'synthetic continuity rejection';
    Exit(False);
  end;
  Result := Assigned(AComposition);
end;

{$PUSH}{$R-}
function InvalidRounding: TWfcMusicArrangementRounding;
var
  LValue: Integer;
begin
  LValue := Ord(High(TWfcMusicArrangementRounding)) + 1;
  Result := TWfcMusicArrangementRounding(LValue);
end;
{$POP}

function ResolveRejected(const ARequested: TWfcMusicArrangementWide;
  const AQuantum: Integer;
  const ARounding: TWfcMusicArrangementRounding;
  const AExpected: String): Boolean;
begin
  Result := False;
  try
    ResolveWfcMusicArrangementTicks(ARequested, AQuantum, ARounding);
  except
    on E: EWfcMusicArrangement do
      Result := Pos(AExpected, E.Message) > 0;
  end;
end;

function ConstructorRejected(const AConfig: TWfcMusicArrangementConfig;
  const ASource: TWfcMusicArrangementSectionSource;
  const AExpected: String): Boolean;
var
  LArrangement: TWfcMusicArrangement;
begin
  Result := False;
  LArrangement := nil;
  try
    try
      LArrangement := TWfcMusicArrangement.Create(AConfig, ASource);
    except
      on E: EWfcMusicArrangement do
        Result := Pos(AExpected, E.Message) > 0;
    end;
  finally
    LArrangement.Free;
  end;
end;

procedure TestRoundingAndValidation;
var
  LConfig: TWfcMusicArrangementConfig;
  LSource: TTestSectionSource;
begin
  BeginTest('portable duration rounding and configuration checks');
  Check(ResolveWfcMusicArrangementTicks(12, 4, wmarExact) = 12,
    'exact aligned ticks are retained');
  Check(ResolveWfcMusicArrangementTicks(11, 4, wmarFloorToCell) = 8,
    'floor rounding reports the shorter actual duration');
  Check(ResolveWfcMusicArrangementTicks(11, 4, wmarCeilToCell) = 12,
    'ceiling rounding reports the longer actual duration');
  Check(ResolveRejected(11, 4, wmarExact, 'not aligned'),
    'exact rounding rejects fractional cells');
  Check(ResolveRejected(1, 4, wmarFloorToCell, 'empty'),
    'floor rounding cannot silently create an empty arrangement');
  Check(ResolveRejected(WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER,
    2, wmarCeilToCell, 'safe-integer'),
    'ceiling arithmetic rejects portable-range overflow');
  Check(ResolveRejected(4, 1, InvalidRounding, 'unknown'),
    'invalid rounding enum bits are rejected');

  LSource := TTestSectionSource.Create;
  try
    LConfig := MakeWfcMusicArrangementConfig(16, 1,
      High(Integer), 2, 0, wmarExact);
    LConfig.QuantumTicks := 2;
    Check(ConstructorRejected(LConfig, LSource, 'Integer range'),
      'section cell-by-quantum products are checked');
    LConfig := MakeWfcMusicArrangementConfig(16, 1, 4,
      -1, 0, wmarExact);
    Check(ConstructorRejected(LConfig, LSource, 'continuity'),
      'negative context sizes are rejected');
    LConfig := MakeWfcMusicArrangementConfig(16, 1, 4,
      2, 0, wmarExact);
    Check(ConstructorRejected(LConfig, nil, 'not assigned'),
      'a section source is mandatory');
  finally
    LSource.Free;
  end;
end;

procedure TestExactPartialIteration;
var
  LArrangement: TWfcMusicArrangement;
  LConfig: TWfcMusicArrangementConfig;
  LContext: TWfcMusicArrangementContext;
  LFirstSignature: Cardinal;
  LSection: TWfcMusicArrangementSection;
  LSecondSignature: Cardinal;
  LSource: TTestSectionSource;
begin
  BeginTest('lazy exact partial-section iteration');
  LSource := TTestSectionSource.Create;
  try
    LConfig := MakeWfcMusicArrangementConfig(10, 1, 4, 2,
      17, wmarExact);
    LArrangement := TWfcMusicArrangement.Create(LConfig, LSource);
    try
      Check((LArrangement.RequestedTicks = 10) and
        (LArrangement.ActualTicks = 10) and
        (LArrangement.SectionCount = 3) and
        (LArrangement.RemainingTicks = 10),
        'plan exposes requested, actual and lazy section counts');

      LSection := nil;
      Check(LArrangement.Next(LSection) = wmaspProduced,
        'first section is produced');
      try
        Check((LSection.Index = 0) and (LSection.StartTick = 0) and
          (LSection.LengthTicks = 4) and (LSection.CellCount = 4) and
          (LSection.Seed = WfcMusicArrangementSectionSeed(17, 0)),
          'first request has exact deterministic range and seed');
        LFirstSignature := LSection.Composition.Signature;
      finally
        LSection.Free;
      end;

      Check(LArrangement.Next(LSection) = wmaspProduced,
        'second section is independently generated');
      try
        Check((LSection.Index = 1) and (LSection.StartTick = 4) and
          (LSection.LengthTicks = 4) and
          (LSection.Seed = WfcMusicArrangementSectionSeed(17, 1)),
          'second request advances range and seed');
        LSecondSignature := LSection.Composition.Signature;
      finally
        LSection.Free;
      end;
      Check(LFirstSignature <> LSecondSignature,
        'the source is invoked for new deterministic musical material');

      Check(LArrangement.Next(LSection) = wmaspProduced,
        'short final section is generated rather than cropped');
      try
        Check((LSection.Index = 2) and (LSection.StartTick = 8) and
          (LSection.LengthTicks = 2) and (LSection.CellCount = 2),
          'final request is the exact remaining whole-cell duration');
      finally
        LSection.Free;
      end;
      Check((LArrangement.Status = wmasCompleted) and
        (LArrangement.ProducedTicks = 10) and
        (LArrangement.RemainingTicks = 0) and
        (LSource.GenerateCalls = 3) and
        (LSource.ContinuityCalls = 3),
        'completion retains exact counters without another allocation');
      Check((LArrangement.Next(LSection) = wmaspCompleted) and
        (LSection = nil), 'completed iterators are terminal and yield nothing');
      Check(LSource.SawPrior and (LSource.LastPriorCount = 2) and
        (LSource.LastPriorEnd = 8),
        'model source receives the authenticated bounded prior tail');

      LContext := LArrangement.CopyContext;
      Check(LContext.HasPrevious and (LContext.EndTick = 10) and
        (Length(LContext.MelodyTokens) = 2),
        'final context is bounded even across a short final section');
      LContext.MelodyTokens[0] := 'changed';
      LContext := LArrangement.CopyContext;
      Check(LContext.MelodyTokens[0] <> 'changed',
        'context access returns detached token arrays');
    finally
      LArrangement.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure TestRoundedPlan;
var
  LArrangement: TWfcMusicArrangement;
  LSection: TWfcMusicArrangementSection;
  LSource: TTestSectionSource;
begin
  BeginTest('rounded actual duration remains explicit');
  LSource := TTestSectionSource.Create;
  try
    LArrangement := TWfcMusicArrangement.Create(
      MakeWfcMusicArrangementConfig(11, 4, 2, 1, 3,
        wmarCeilToCell), LSource);
    try
      Check((LArrangement.RequestedTicks = 11) and
        (LArrangement.ActualTicks = 12) and
        (LArrangement.SectionCount = 2),
        'requested and rounded actual durations are not conflated');
      LSection := nil;
      Check(LArrangement.Next(LSection) = wmaspProduced,
        'rounded plan produces its full first section');
      try
        Check((LSection.LengthTicks = 8) and (LSection.CellCount = 2),
          'first rounded section uses the caller chunk size');
      finally
        LSection.Free;
      end;
      Check(LArrangement.Next(LSection) = wmaspProduced,
        'rounded plan produces an exact final cell');
      try
        Check((LSection.LengthTicks = 4) and (LSection.CellCount = 1),
          'rounded final section is solved at its actual length');
      finally
        LSection.Free;
      end;
    finally
      LArrangement.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure TestHugeLazyPlanAndCancellation;
var
  LArrangement: TWfcMusicArrangement;
  LSection: TWfcMusicArrangementSection;
  LSource: TTestSectionSource;
begin
  BeginTest('huge plans retain bounded live state and can be cancelled');
  LSource := TTestSectionSource.Create;
  try
    LArrangement := TWfcMusicArrangement.Create(
      MakeWfcMusicArrangementConfig(
        WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER, 1, 16, 2, 9,
        wmarExact), LSource);
    try
      Check(LArrangement.SectionCount =
        (WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER div 16) + 1,
        'huge section count uses checked wide arithmetic');
      LSection := nil;
      Check(LArrangement.Next(LSection) = wmaspProduced,
        'huge plan lazily produces only its first section');
      LSection.Free;
      Check(LArrangement.Next(LSection) = wmaspProduced,
        'huge plan lazily produces only its requested next section');
      LSection.Free;
      Check((LArrangement.ProducedTicks = 32) and
        (LSource.GenerateCalls = 2),
        'work and allocation are proportional to consumed sections');
      LArrangement.Cancel;
      Check((LArrangement.Status = wmasCancelled) and
        (LArrangement.Next(LSection) = wmaspCancelled) and
        (LSection = nil) and (LSource.GenerateCalls = 2),
        'caller cancellation is terminal between synchronous steps');
    finally
      LArrangement.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure TestFiniteAndValidationFailures;
var
  LArrangement: TWfcMusicArrangement;
  LSection: TWfcMusicArrangementSection;
  LSource: TTestSectionSource;
begin
  BeginTest('finite source and candidate failures return no partial section');
  LSource := TTestSectionSource.Create;
  try
    LSource.FailIndex := 0;
    LArrangement := TWfcMusicArrangement.Create(
      MakeWfcMusicArrangementConfig(8, 1, 4, 2, 0, wmarExact),
      LSource);
    try
      LSection := nil;
      Check((LArrangement.Next(LSection) = wmaspFailed) and
        (LSection = nil) and (LArrangement.Status = wmasFailed) and
        (LArrangement.ProducedTicks = 0) and
        (Pos('synthetic finite', LArrangement.Failure) > 0),
        'finite generation failure is terminal with its useful detail');
      Check((LArrangement.Next(LSection) = wmaspFailed) and
        (LSource.GenerateCalls = 1),
        'failed iterators never retry or return a stale candidate');
    finally
      LArrangement.Free;
    end;
  finally
    LSource.Free;
  end;

  LSource := TTestSectionSource.Create;
  try
    LSource.WrongSeedIndex := 0;
    LArrangement := TWfcMusicArrangement.Create(
      MakeWfcMusicArrangementConfig(4, 1, 4, 2, 0, wmarExact),
      LSource);
    try
      Check((LArrangement.Next(LSection) = wmaspFailed) and
        (LSection = nil) and (Pos('seed differs', LArrangement.Failure) > 0),
        'independent validation rejects a mismatched section seed');
    finally
      LArrangement.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure TestContinuityFailureAndException;
var
  LArrangement: TWfcMusicArrangement;
  LRaised: Boolean;
  LSection: TWfcMusicArrangementSection;
  LSource: TTestSectionSource;
begin
  BeginTest('continuity failures and unexpected source exceptions are honest');
  LSource := TTestSectionSource.Create;
  try
    LSource.ContinuityFailIndex := 1;
    LArrangement := TWfcMusicArrangement.Create(
      MakeWfcMusicArrangementConfig(8, 1, 4, 2, 0, wmarExact),
      LSource);
    try
      LSection := nil;
      Check(LArrangement.Next(LSection) = wmaspProduced,
        'first section commits before a later continuity check');
      LSection.Free;
      Check((LArrangement.Next(LSection) = wmaspFailed) and
        (LSection = nil) and (LArrangement.ProducedTicks = 4) and
        (Pos('continuity', LArrangement.Failure) > 0),
        'rejected boundary returns no second section or stale output');
    finally
      LArrangement.Free;
    end;
  finally
    LSource.Free;
  end;

  LSource := TTestSectionSource.Create;
  try
    LSource.RaiseIndex := 0;
    LArrangement := TWfcMusicArrangement.Create(
      MakeWfcMusicArrangementConfig(4, 1, 4, 2, 0, wmarExact),
      LSource);
    try
      LRaised := False;
      LSection := nil;
      try
        LArrangement.Next(LSection);
      except
        on E: Exception do
          LRaised := Pos('synthetic source exception', E.Message) > 0;
      end;
      Check(LRaised and (LSection = nil) and
        (LArrangement.Status = wmasFailed) and
        (Pos('source exception', LArrangement.Failure) > 0),
        'unexpected callback exceptions propagate after terminal bookkeeping');
    finally
      LArrangement.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure TestDeterministicSeeds;
begin
  BeginTest('portable per-section seed derivation');
  { Independently calculated FNV-1a/32 goldens: start 2166136261, multiply
    by 16777619 modulo 2^32 after each xor. Input is the version byte 1,
    base seed 123 as four little-endian bytes, then eight index bytes.
    This oracle used exact arbitrary-width multiplication, not the unit's
    shift/add implementation or a result generated by that implementation. }
  Check(WfcMusicArrangementSectionSeed(123, 0) = TGraphSeed($0EADAA47),
    'versioned seed byte ordering matches independent index-zero golden');
  Check(WfcMusicArrangementSectionSeed(123, 4294967296) = TGraphSeed($BE816B36),
    'index 2^32 preserves the first index byte beyond a DWORD');
  Check(WfcMusicArrangementSectionSeed(123, 4294967297) = TGraphSeed($01788A37),
    'high index retains its low byte as well as its upper bytes');
  Check(WfcMusicArrangementSectionSeed(123,
    WFC_MUSIC_ARRANGEMENT_MAX_SAFE_INTEGER) = TGraphSeed($B6CDD64E),
    'maximum portable index matches the independent eight-byte golden');
  Check(WfcMusicArrangementSectionSeed(123, 7) =
    WfcMusicArrangementSectionSeed(123, 7),
    'same base seed and section index replay exactly');
  Check(WfcMusicArrangementSectionSeed(123, 7) <>
    WfcMusicArrangementSectionSeed(123, 8),
    'adjacent section indices use independently derived streams');
  Check(WfcMusicArrangementSectionSeed(123, 7) <>
    WfcMusicArrangementSectionSeed(124, 7),
    'base seed remains part of section identity');
end;

begin
  WriteLn('WFC music-arrangement tests');
  WriteLn('version=', WFC_MUSIC_ARRANGEMENT_VERSION);
  TestRoundingAndValidation;
  TestExactPartialIteration;
  TestRoundedPlan;
  TestHugeLazyPlanAndCancellation;
  TestFiniteAndValidationFailures;
  TestContinuityFailureAndException;
  TestDeterministicSeeds;
  WriteLn('[SUMMARY] checks=', GCheckCount, ' failures=', GFailureCount);
  if GFailureCount <> 0 then
    Halt(1);
end.
