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
unit ensemble_studio_planning;

{$mode delphi}{$H+}

interface

uses
  SysUtils, wfc, wfc_model, wfc_sequence, wfc_sequence_graph,
  wfc_music_ensemble, wfc_music_ensemble_passes, wfc_music_ensemble_stream,
  wfc_music_arrangement, wfc_music_form, ensemble_studio_profiles;

{ Example-owned adaptation: a bar-resolution form plan becomes exact public
  acoustic masks. Core form planning knows neither this catalog nor a score
  codec. Core acoustic layers remain the original three-layer owner. }
function CreateEnsembleStudioPipeline(const AConfig: TWfcMusicEnsembleConfig;
  const AProfile: TEnsembleStudioProfile; out APlanText: String):
  TWfcMusicEnsemblePipeline;
function CreateEnsembleStudioStream(const AConfig: TWfcMusicEnsembleStreamConfig;
  const AProfile: TEnsembleStudioProfile): TWfcMusicEnsembleStream;

implementation

uses wfc_music_sequence;

type
  TPlannedEnsemblePipeline = class(TWfcMusicEnsemblePipeline)
  strict private
    FExpected: TWfcModelTokens;
    FPriorValidator: TWfcMusicEnsembleCompositionValidator;
    function ValidatePlan(const AComposition: TWfcMusicEnsembleComposition;
      out AIssue: TWfcMusicEnsembleValidationIssue): Boolean;
  public
    constructor CreatePlanned(const AConfig: TWfcMusicEnsembleConfig;
      const AExpected: TWfcModelTokens);
  end;

  { Prefetch owns one phrase plus one bar, independently of requested length.
    A failed acoustic segment terminates its owner; prefetched future form is
    never advertised as committed audio or used to repair emitted history. }
  TPlannedEnsembleStream = class(TWfcMusicEnsembleStream)
  strict private
    FModels: TWfcMusicEnsembleModels;
    FCursor: TWfcMusicFormCursor;
    FPhrase: TWfcMusicFormPhrasePlan;
    FBarIndex, FBarCell: Integer;
    FBarFrames: TWfcMusicEnsembleFrames;
    FNextCell, FExpectedStartTick: TWfcMusicArrangementWide;
    FExpected: TWfcModelTokens;
    function NextPlannedFrame: TWfcMusicEnsembleFrame;
  protected
    procedure ConfigureSegment(const AIndex, AStartTick: TWfcMusicArrangementWide;
      const ACellCount: Integer; const AGraph: TGraph); override;
    function ValidateSegment(const ACandidate: TWfcMusicEnsembleSegment;
      out AFailure: String): Boolean; override;
  public
    constructor CreatePlanned(const AConfig: TWfcMusicEnsembleStreamConfig);
    destructor Destroy; override;
  end;

procedure PlanningError(const AMessage: String);
begin
  raise EEnsembleStudioProfile.Create('ensemble plan: ' + AMessage);
end;

constructor TPlannedEnsemblePipeline.CreatePlanned(
  const AConfig: TWfcMusicEnsembleConfig; const AExpected: TWfcModelTokens);
var C: TWfcMusicEnsembleConfig;
begin
  FExpected := Copy(AExpected);
  FPriorValidator := AConfig.ValidateComposition;
  C := AConfig;
  C.ValidateComposition := {$IFDEF PAS2JS}@{$ENDIF}ValidatePlan;
  inherited Create(C);
end;

function TPlannedEnsemblePipeline.ValidatePlan(
  const AComposition: TWfcMusicEnsembleComposition;
  out AIssue: TWfcMusicEnsembleValidationIssue): Boolean;
var T: TWfcModelTokens; I: Integer;
begin
  Result := False;
  AIssue := Default(TWfcMusicEnsembleValidationIssue);
  AIssue.Kind := wmevikCallerConstraint;
  AIssue.Layer := wmelEnsemble;
  AIssue.Position := -1;
  T := EncodeWfcMusicEnsembleFrames(AComposition.CopyEnsembleFrames);
  if Length(T) <> Length(FExpected) then
  begin
    AIssue.Detail := 'developed plan and realized score lengths differ';
    Exit;
  end;
  for I := 0 to High(T) do
    if T[I] <> FExpected[I] then
    begin
      AIssue.Position := I;
      AIssue.Detail := 'realized frame differs from immutable developed plan';
      Exit;
    end;
  AIssue := Default(TWfcMusicEnsembleValidationIssue);
  AIssue.Layer := wmelEnsemble;
  AIssue.Position := -1;
  if Assigned(FPriorValidator) then
    Exit(FPriorValidator(AComposition, AIssue));
  Result := True;
end;

function CreateEnsembleStudioPipeline(const AConfig: TWfcMusicEnsembleConfig;
  const AProfile: TEnsembleStudioProfile; out APlanText: String):
  TWfcMusicEnsemblePipeline;
const DISPLAY_BARS = 32;
var
  C: TWfcMusicEnsembleConfig;
  Form: TWfcMusicFormConfig;
  Cursor: TWfcMusicFormCursor;
  Phrase: TWfcMusicFormPhrasePlan;
  Bar: TWfcMusicFormBar;
  Report: TGraphNegotiationReport;
  Step: TWfcMusicArrangementStep;
  Layer: TWfcMusicEnsembleLayer;
  Tokens, Expected: TWfcModelTokens;
  I, J, Offset, Count, Base: Integer;
  PlanText, Failure: String;
begin
  Result := nil;
  APlanText := '';
  EnsembleStudioProfileName(AProfile);
  if AProfile = espStructuralV1 then
  begin
    Result := TWfcMusicEnsemblePipeline.Create(AConfig);
    APlanText := 'structural-v1: original repeated-bar study. Bars extend ' +
      'duration, not phrase development. Choose developed-period-v1 for form planning.';
    Exit;
  end;
  if (AConfig.ScoreTemplate = nil) or (AConfig.QuantumTicks <> 240) then
    PlanningError('developed profile requires a score and 240-tick cells');
  Count := AConfig.ScoreTemplate.LengthTicks div AConfig.QuantumTicks;
  Form := EnsembleStudioDevelopedFormConfig(Count, AConfig.Seed);
  C := AConfig;
  for Layer := Low(Layer) to High(Layer) do
  begin
    C.InitialTokenConstraints[Layer] := Copy(AConfig.InitialTokenConstraints[Layer]);
    Base := Length(C.InitialTokenConstraints[Layer]);
    if Count > High(Integer) - Base then PlanningError('too many initial masks');
    SetLength(C.InitialTokenConstraints[Layer], Base + Count);
  end;
  SetLength(Expected, Count);
  PlanText := 'developed-period-v1; form-v' + IntToStr(WFC_MUSIC_FORM_VERSION) +
    '; seed=' + UIntToStr(AConfig.Seed) + '; cells=' + IntToStr(Count) + #10 +
    'form -> harmonic-intent -> gesture; then exact acoustic realization.';
  Cursor := TWfcMusicFormCursor.Create(Form);
  Phrase := nil;
  Offset := 0;
  try
    repeat
      FreeAndNil(Phrase);
      Step := Cursor.Next(Phrase, Report);
      if Step = wmaspCompleted then Break;
      if Step <> wmaspProduced then PlanningError(Cursor.Failure);
      if Phrase.BarAt(0).Index < DISPLAY_BARS then
        PlanText := PlanText + #10 + 'phrase ' + IntToStr(Phrase.PhraseIndex + 1) +
          ' / signature ' + IntToHex(Phrase.Signature, 8) +
          ' / rounds ' + IntToStr(Length(Report.Attempts) + 1) +
          ' / pass backtracks ' + IntToStr(Report.PassBacktracks);
      for I := 0 to Phrase.BarCount - 1 do
      begin
        Bar := Phrase.BarAt(I);
        if (Bar.StartCell <> Offset) or (Bar.CellCount > Count - Offset) then
          PlanningError('noncontiguous form plan');
        if not ValidateEnsembleStudioPlannedFrames(Bar,
            EnsembleStudioPlannedFrames(Bar), Failure) then PlanningError(Failure);
        for Layer := Low(Layer) to High(Layer) do
        begin
          Tokens := EnsembleStudioPlannedTokens(Bar, Layer);
          if Length(Tokens) <> Bar.CellCount then PlanningError('mask extent differs');
          Base := Length(AConfig.InitialTokenConstraints[Layer]);
          for J := 0 to High(Tokens) do
          begin
            C.InitialTokenConstraints[Layer][Base + Offset + J].Position := Offset + J;
            SetLength(C.InitialTokenConstraints[Layer][Base + Offset + J].AllowedTokens, 1);
            C.InitialTokenConstraints[Layer][Base + Offset + J].AllowedTokens[0] := Tokens[J];
            if Layer = wmelEnsemble then Expected[Offset + J] := Tokens[J];
          end;
        end;
        if Bar.Index < DISPLAY_BARS then
          PlanText := PlanText + #10 + 'bar ' + IntToStr(Bar.Index + 1) + ': ' +
            WfcMusicFormRoleName(Bar.Role) + ' / ' +
            Form.Harmonies[Bar.HarmonyIndex].LabelText + ' / ' +
            Form.Gestures[Bar.GestureIndex].LabelText + ' / ' +
            WfcMusicFormCadenceName(Bar.Cadence);
        Inc(Offset, Bar.CellCount);
      end;
    until False;
    if Offset <> Count then PlanningError('form ended before requested extent');
    if WfcMusicFormTotalBars(Form) > DISPLAY_BARS then
      PlanText := PlanText + #10 + 'Display: first 32 bars; all requested bars are planned.';
    Result := TPlannedEnsemblePipeline.CreatePlanned(C, Expected);
    APlanText := PlanText;
  finally
    Phrase.Free;
    Cursor.Free;
  end;
end;

constructor TPlannedEnsembleStream.CreatePlanned(
  const AConfig: TWfcMusicEnsembleStreamConfig);
begin
  inherited Create(AConfig);
  if AConfig.QuantumTicks <> 240 then
    PlanningError('developed stream requires 240-tick cells');
  FModels := AConfig.Models;
  FCursor := TWfcMusicFormCursor.Create(EnsembleStudioDevelopedFormConfig(
    ActualTicks div AConfig.QuantumTicks, AConfig.Seed));
end;

destructor TPlannedEnsembleStream.Destroy;
begin
  FPhrase.Free;
  FCursor.Free;
  inherited Destroy;
end;

function TPlannedEnsembleStream.NextPlannedFrame: TWfcMusicEnsembleFrame;
var B: TWfcMusicFormBar; R: TGraphNegotiationReport; Failure: String;
begin
  if FBarCell >= Length(FBarFrames) then
  begin
    if (FPhrase = nil) or (FBarIndex >= FPhrase.BarCount) then
    begin
      FreeAndNil(FPhrase);
      if FCursor.Next(FPhrase, R) <> wmaspProduced then
        PlanningError('form ended early: ' + FCursor.Failure);
      FBarIndex := 0;
    end;
    B := FPhrase.BarAt(FBarIndex);
    if B.StartCell <> FNextCell then PlanningError('stream form is not contiguous');
    FBarFrames := EnsembleStudioPlannedFrames(B);
    if not ValidateEnsembleStudioPlannedFrames(B, FBarFrames, Failure) then
      PlanningError(Failure);
    Inc(FBarIndex);
    FBarCell := 0;
  end;
  Result := FBarFrames[FBarCell];
  Inc(FBarCell);
  Inc(FNextCell);
end;

procedure TPlannedEnsembleStream.ConfigureSegment(
  const AIndex, AStartTick: TWfcMusicArrangementWide;
  const ACellCount: Integer; const AGraph: TGraph);
var F: TWfcMusicEnsembleFrame; I: Integer;
begin
  inherited ConfigureSegment(AIndex, AStartTick, ACellCount, AGraph);
  if AStartTick div 240 <> FNextCell then PlanningError('unexpected segment start');
  FExpectedStartTick := AStartTick;
  SetLength(FExpected, ACellCount);
  for I := 0 to ACellCount - 1 do
  begin
    F := NextPlannedFrame;
    FExpected[I] := EncodeWfcMusicEnsembleFrame(F);
    IntersectSequenceAllowedTokens(FModels.Ensemble,
      AGraph.PassGraph[Ord(wmelEnsemble)], I, FExpected[I]);
    IntersectSequenceAllowedTokens(FModels.Harmony,
      AGraph.PassGraph[Ord(wmelHarmony)], I,
      EncodeWfcMusicPitchClassSet(ProjectWfcMusicEnsembleFrameToPitchClassSet(F, 12)));
    IntersectSequenceAllowedTokens(FModels.Rhythm,
      AGraph.PassGraph[Ord(wmelRhythm)], I,
      EncodeWfcMusicRhythmFrame(ProjectWfcMusicEnsembleFrameToRhythm(F)));
  end;
end;

function TPlannedEnsembleStream.ValidateSegment(
  const ACandidate: TWfcMusicEnsembleSegment; out AFailure: String): Boolean;
var T: TWfcModelTokens; I: Integer;
begin
  Result := False;
  AFailure := 'realized segment differs from immutable developed plan';
  if (ACandidate = nil) or (ACandidate.StartTick <> FExpectedStartTick) then Exit;
  T := EncodeWfcMusicEnsembleFrames(ACandidate.CopyFrames);
  if Length(T) <> Length(FExpected) then Exit;
  for I := 0 to High(T) do
    if T[I] <> FExpected[I] then
    begin
      AFailure := AFailure + ' at local cell ' + IntToStr(I);
      Exit;
    end;
  AFailure := '';
  Result := True;
end;

function CreateEnsembleStudioStream(const AConfig: TWfcMusicEnsembleStreamConfig;
  const AProfile: TEnsembleStudioProfile): TWfcMusicEnsembleStream;
begin
  EnsembleStudioProfileName(AProfile);
  if AProfile = espStructuralV1 then
    Result := TWfcMusicEnsembleStream.Create(AConfig)
  else
    Result := TPlannedEnsembleStream.CreatePlanned(AConfig);
end;

end.
