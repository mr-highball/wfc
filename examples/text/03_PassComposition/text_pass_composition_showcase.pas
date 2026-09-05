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
unit text_pass_composition_showcase;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_sequence,
  wfc_sequence_graph,
  wfc_text_passes;

const
  WFC_TEXT_PASS_SHOWCASE_VERSION = 1;
  WFC_TEXT_PASS_SHOWCASE_LENGTH = 5;
  WFC_TEXT_PASS_SHOWCASE_DEFAULT_SEED = TGraphSeed(0);

type
  ETextPassCompositionShowcase = class(Exception);

  { TTextPassCompositionShowcase }

  TTextPassCompositionShowcase = class
  strict private
    FLexicalModel: TWfcSequenceModel;
    FPipeline: TWfcTextPassPipeline;
    FPunctuationModel: TWfcSequenceModel;
    FStructureModel: TWfcSequenceModel;
    procedure BuildModels;
    function BuildConfig(const ASeed: TGraphSeed): TWfcTextPassConfig;
  public
    constructor Create(const ASeed: TGraphSeed);
    destructor Destroy; override;

    function TryGenerate(out AResult: TWfcTextPassResult;
      out AReport: TWfcTextPassReport): Boolean;
    function TryRegenerateFrom(const ALayer: TWfcTextPassLayer;
      out AResult: TWfcTextPassResult;
      out AReport: TWfcTextPassReport): Boolean;
    procedure ClearAllConstraints;

    property Pipeline: TWfcTextPassPipeline read FPipeline;
  end;

function TextPassShowcaseSequence(const AResult: TWfcTextPassResult;
  const ALayer: TWfcTextPassLayer): TWfcGeneratedSequence;
function TextPassShowcaseDisplayToken(const ALayer: TWfcTextPassLayer;
  const AToken: TWfcModelToken): String;
function TextPassShowcaseSignature(const AResult: TWfcTextPassResult):
  String;

implementation

uses
  wfc_sequence_learn,
  wfc_text_codec;

type
  TTokenSourceFunction = function(
    const AToken: TWfcModelToken): TWfcModelTokens;

function TokensOf(const AValues: array of TWfcModelToken): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function FragmentTokensOf(
  const AValues: array of TWfcModelToken): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := EncodeWfcTextPassFragment(AValues[I]);
end;

function SamplesOf(const AValues: array of TWfcSequenceSample):
  TWfcSequenceSamples;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function OneToken(const AToken: TWfcModelToken): TWfcModelTokens;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0] := AToken;
end;

function StructureForLexical(
  const AToken: TWfcModelToken): TWfcModelTokens;
begin
  if (AToken = 'the') or (AToken = 'a') then
    Result := OneToken('DET')
  else if (AToken = 'quiet') or (AToken = 'quick') or
      (AToken = 'calm') then
    Result := OneToken('ADJ')
  else if (AToken = 'owl') or (AToken = 'fox') or
      (AToken = 'sun') or (AToken = 'sea') then
    Result := OneToken('NOUN')
  else if (AToken = 'rests') or (AToken = 'rises') or
      (AToken = 'settles') then
    Result := OneToken('VERB')
  else if (AToken = 'quietly') or (AToken = 'brightly') then
    Result := OneToken('ADV')
  else if (AToken = 'dot') or (AToken = 'period') or
      (AToken = 'bang') then
    Result := OneToken('STOP')
  else
    raise ETextPassCompositionShowcase.Create(
      'unmapped lexical showcase token');
end;

function LexicalForPunctuation(
  const AToken: TWfcModelToken): TWfcModelTokens;
var
  LFragment: TWfcModelToken;
begin
  LFragment := DecodeWfcTextPassFragment(AToken);
  if LFragment = 'The' then
    Result := OneToken('the')
  else if LFragment = 'A' then
    Result := OneToken('a')
  else if LFragment = ' quiet' then
    Result := OneToken('quiet')
  else if LFragment = ' quick' then
    Result := OneToken('quick')
  else if LFragment = ' calm' then
    Result := OneToken('calm')
  else if LFragment = ' owl' then
    Result := OneToken('owl')
  else if LFragment = ' fox' then
    Result := OneToken('fox')
  else if LFragment = ' sun' then
    Result := OneToken('sun')
  else if LFragment = ' sea' then
    Result := OneToken('sea')
  else if LFragment = ' rests' then
    Result := OneToken('rests')
  else if LFragment = ' rises' then
    Result := OneToken('rises')
  else if LFragment = ' settles' then
    Result := OneToken('settles')
  else if LFragment = ' quietly' then
    Result := OneToken('quietly')
  else if LFragment = ' brightly' then
    Result := OneToken('brightly')
  else if LFragment = '.' then
    Result := TokensOf(['dot', 'period'])
  else if LFragment = '!' then
    Result := OneToken('bang')
  else
    raise ETextPassCompositionShowcase.Create(
      'unmapped punctuation showcase token');
end;

function StructureForPunctuation(
  const AToken: TWfcModelToken): TWfcModelTokens;
var
  LFragment: TWfcModelToken;
begin
  LFragment := DecodeWfcTextPassFragment(AToken);
  if (LFragment = 'The') or (LFragment = 'A') then
    Result := OneToken('DET')
  else if (LFragment = ' quiet') or (LFragment = ' quick') or
      (LFragment = ' calm') then
    Result := OneToken('ADJ')
  else if (LFragment = ' owl') or (LFragment = ' fox') or
      (LFragment = ' sun') or (LFragment = ' sea') then
    Result := OneToken('NOUN')
  else if (LFragment = ' rests') or (LFragment = ' rises') or
      (LFragment = ' settles') then
    Result := OneToken('VERB')
  else if (LFragment = ' quietly') or (LFragment = ' brightly') then
    Result := OneToken('ADV')
  else if (LFragment = '.') or (LFragment = '!') then
    Result := OneToken('STOP')
  else
    raise ETextPassCompositionShowcase.Create(
      'unmapped punctuation structure token');
end;

function BuildRules(const ATarget: TWfcSequenceModel;
  const ASources: TTokenSourceFunction): TWfcSequenceProjectionRules;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, ATarget.PublicTokenCount);
  for I := 0 to ATarget.PublicTokenCount - 1 do
    Result[I] := MakeWfcSequenceProjectionRule(ATarget.PublicTokenAt(I),
      ASources(ATarget.PublicTokenAt(I)));
end;

constructor TTextPassCompositionShowcase.Create(const ASeed: TGraphSeed);
var
  LConfig: TWfcTextPassConfig;
begin
  inherited Create;
  FPipeline := nil;
  FStructureModel := nil;
  FLexicalModel := nil;
  FPunctuationModel := nil;
  try
    BuildModels;
    LConfig := BuildConfig(ASeed);
    FPipeline := TWfcTextPassPipeline.Create(LConfig);
  except
    FPipeline.Free;
    FPunctuationModel.Free;
    FLexicalModel.Free;
    FStructureModel.Free;
    raise;
  end;
end;

destructor TTextPassCompositionShowcase.Destroy;
begin
  FPipeline.Free;
  FPunctuationModel.Free;
  FLexicalModel.Free;
  FStructureModel.Free;
  inherited Destroy;
end;

procedure TTextPassCompositionShowcase.BuildModels;
begin
  FStructureModel := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(TokensOf([
      'DET', 'ADJ', 'NOUN', 'VERB', 'STOP'])),
    MakeWfcSequenceSample(TokensOf([
      'DET', 'NOUN', 'VERB', 'ADV', 'STOP']))
    ]), 2);
  FLexicalModel := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(TokensOf([
      'the', 'quiet', 'owl', 'rests', 'dot'])),
    MakeWfcSequenceSample(TokensOf([
      'the', 'quick', 'fox', 'rests', 'bang'])),
    MakeWfcSequenceSample(TokensOf([
      'the', 'fox', 'rests', 'quietly', 'dot'])),
    MakeWfcSequenceSample(TokensOf([
      'a', 'sun', 'rises', 'brightly', 'bang'])),
    MakeWfcSequenceSample(TokensOf([
      'a', 'calm', 'sea', 'settles', 'period']))
    ]), 2);
  FPunctuationModel := LearnSequenceModelCorpus(SamplesOf([
    MakeWfcSequenceSample(FragmentTokensOf([
      'The', ' quiet', ' owl', ' rests', '.'])),
    MakeWfcSequenceSample(FragmentTokensOf([
      'The', ' quick', ' fox', ' rests', '!'])),
    MakeWfcSequenceSample(FragmentTokensOf([
      'The', ' fox', ' rests', ' quietly', '.'])),
    MakeWfcSequenceSample(FragmentTokensOf([
      'A', ' sun', ' rises', ' brightly', '!'])),
    MakeWfcSequenceSample(FragmentTokensOf([
      'A', ' calm', ' sea', ' settles', '.']))
    ]), 2);
end;

function TTextPassCompositionShowcase.BuildConfig(
  const ASeed: TGraphSeed): TWfcTextPassConfig;
begin
  Result := DefaultWfcTextPassConfig(WFC_TEXT_PASS_SHOWCASE_LENGTH,
    wseWhole, ASeed);
  Result.Models.Structure := FStructureModel;
  Result.Models.Lexical := FLexicalModel;
  Result.Models.Punctuation := FPunctuationModel;
  Result.Maps.LexicalFromStructure :=
    BuildRules(FLexicalModel, @StructureForLexical);
  Result.Maps.PunctuationFromLexical :=
    BuildRules(FPunctuationModel, @LexicalForPunctuation);
  Result.Maps.PunctuationFromStructure :=
    BuildRules(FPunctuationModel, @StructureForPunctuation);
end;

function TTextPassCompositionShowcase.TryGenerate(
  out AResult: TWfcTextPassResult;
  out AReport: TWfcTextPassReport): Boolean;
var
  LOptions: TGraphSolveOptions;
begin
  LOptions := DefaultGraphSolveOptions;
  LOptions.CaptureTrace := True;
  Result := FPipeline.TryGenerate(LOptions, AResult, AReport);
end;

function TTextPassCompositionShowcase.TryRegenerateFrom(
  const ALayer: TWfcTextPassLayer; out AResult: TWfcTextPassResult;
  out AReport: TWfcTextPassReport): Boolean;
var
  LOptions: TGraphSolveOptions;
begin
  LOptions := DefaultGraphSolveOptions;
  LOptions.CaptureTrace := True;
  Result := FPipeline.TryRegenerateFrom(ALayer, LOptions,
    AResult, AReport);
end;

procedure TTextPassCompositionShowcase.ClearAllConstraints;
var
  I: Integer;
  LLayer: TWfcTextPassLayer;
begin
  for LLayer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
    for I := 0 to WFC_TEXT_PASS_SHOWCASE_LENGTH - 1 do
      FPipeline.ClearAllowedTokens(LLayer, I);
end;

function TextPassShowcaseSequence(const AResult: TWfcTextPassResult;
  const ALayer: TWfcTextPassLayer): TWfcGeneratedSequence;
begin
  case ALayer of
    wtplStructure:
      Result := AResult.Structure;
    wtplLexical:
      Result := AResult.Lexical;
    wtplPunctuation:
      Result := AResult.Punctuation;
  else
    raise ERangeError.Create('unknown text showcase layer');
  end;
end;

function TextPassShowcaseDisplayToken(const ALayer: TWfcTextPassLayer;
  const AToken: TWfcModelToken): String;
var
  LFragment: TWfcModelToken;
begin
  if ALayer = wtplPunctuation then
  begin
    LFragment := DecodeWfcTextPassFragment(AToken);
    if LFragment = '' then
      Result := '(empty)'
    else
      Result := String(LFragment);
  end
  else
    Result := String(AToken);
end;

procedure HashByte(var AHash: Cardinal; const AByte: Byte);
{$PUSH}
{$Q-}
var
  LValue: Cardinal;
begin
  AHash := AHash xor Cardinal(AByte);
  LValue := AHash;
  AHash := (LValue + (LValue shl 1) + (LValue shl 4) +
    (LValue shl 7) + (LValue shl 8) + (LValue shl 24)) and
    Cardinal($FFFFFFFF);
end;
{$POP}

procedure HashInteger(var AHash: Cardinal; const AValue: Integer);
var
  LValue: Cardinal;
begin
  LValue := Cardinal(AValue);
  HashByte(AHash, Byte(LValue));
  HashByte(AHash, Byte(LValue shr 8));
  HashByte(AHash, Byte(LValue shr 16));
  HashByte(AHash, Byte(LValue shr 24));
end;

procedure HashToken(var AHash: Cardinal; const AToken: TWfcModelToken);
var
  I: Integer;
  LCanonical: String;
begin
  LCanonical := WfcTextEncodeToken(AToken, 'text pass showcase signature');
  HashInteger(AHash, Length(LCanonical));
  for I := 1 to Length(LCanonical) do
    HashByte(AHash, Byte(Ord(LCanonical[I])));
end;

function SignatureHex(const AValue: Cardinal): String;
const
  HEX_DIGITS = '0123456789ABCDEF';
var
  I: Integer;
  LValue: Cardinal;
begin
  SetLength(Result, 8);
  LValue := AValue;
  for I := 8 downto 1 do
  begin
    Result[I] := HEX_DIGITS[Integer(LValue and Cardinal($F)) + 1];
    LValue := LValue shr 4;
  end;
end;

function TextPassShowcaseSignature(const AResult: TWfcTextPassResult):
  String;
var
  I: Integer;
  LGenerated: TWfcGeneratedSequence;
  LHash: Cardinal;
  LLayer: TWfcTextPassLayer;
begin
  LHash := Cardinal(2166136261);
  HashInteger(LHash, WFC_TEXT_PASS_SHOWCASE_VERSION);
  for LLayer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
  begin
    HashInteger(LHash, Ord(LLayer));
    LGenerated := TextPassShowcaseSequence(AResult, LLayer);
    HashInteger(LHash, Length(LGenerated.Tokens));
    for I := 0 to Length(LGenerated.Tokens) - 1 do
    begin
      HashInteger(LHash, LGenerated.StateIndices[I]);
      HashToken(LHash, LGenerated.Tokens[I]);
    end;
  end;
  HashToken(LHash, AResult.Text);
  Result := IntToStr(WFC_TEXT_PASS_SHOWCASE_VERSION) + ':' +
    SignatureHex(LHash);
end;

end.
