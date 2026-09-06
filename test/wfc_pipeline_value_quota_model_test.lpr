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
program wfc_pipeline_value_quota_model_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_learn, wfc_model_text,
  wfc_rule_model, wfc_rule_text, wfc_pattern2d, wfc_pattern2d_learn,
  wfc_pattern2d_text, wfc_sequence, wfc_sequence_learn,
  wfc_sequence_text, wfc_pipeline_model;

type
  TTestProcedure = procedure;
  TInputs = record
    Metadata: TWfcPipelineMetadata;
    Resources: TWfcPipelineResources;
    Passes: TWfcPipelinePasses;
    Dependencies: TWfcPipelineDependencies;
    Bridges: TWfcPipelineBridges;
    Requirements: TWfcPipelineRequirements;
    Quotas: TWfcPipelineValueQuotas;
    Rank: Integer;
    Wrap: Boolean;
  end;
var Checks, Failures: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if ACondition then Exit;
  Inc(Failures); WriteLn('[FAIL] ', AMessage);
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try ATest;
  except on E: Exception do
    begin Inc(Failures); WriteLn('[EXCEPTION] ', E.ClassName, ': ', E.Message); end; end;
end;

function Tokens(const AValues: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

function NoteToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}Result := Chr($266B);
  {$ELSE}Result := UTF8Encode(UnicodeString(WideChar($266B)));{$ENDIF}
end;

function Quota(const APass: Integer; const ALabel: TWfcModelToken;
  const AValues: array of TWfcModelToken; const AMin, AMax: Integer): TWfcPipelineValueQuota;
begin
  Result := MakeWfcPipelineValueQuota(APass, ALabel, Tokens(AValues), AMin, AMax);
end;

function BuildInputs: TInputs;
var Rule: TWfcRuleModel; Model: TWfcModel;
  Weights: TWfcModelIntegerArray; Vocabulary: TWfcModelTokens;
begin
  Result := Default(TInputs);
  Result.Metadata := MakeWfcPipelineMetadata('Quota model fixture', 'MIT', 'authored', 'quota:model:v1');
  Result.Rank := 1;
  Vocabulary := Tokens(['z', 'a', NoteToken]);
  SetLength(Weights, 3); Weights[0] := 1; Weights[1] := 2; Weights[2] := 1;
  SetLength(Result.Resources, 2);
  Rule := TWfcRuleModel.Create(1, Vocabulary, Weights, nil);
  try Result.Resources[0] := MakeWfcPipelineResource('rules', wprkRules,
    EncodeWfcRuleText(Rule), 'authored rules', 'MIT', '');
  finally Rule.Free; end;
  Model := LearnModel1D(Vocabulary, wmbOpen);
  try Result.Resources[1] := MakeWfcPipelineResource('model', wprkModel,
    EncodeWfcModelText(Model), 'learned fixture', 'MIT', '');
  finally Model.Free; end;
  SetLength(Result.Passes, 5);
  Result.Passes[0] := MakeWfcPipelinePass('source', wppvPublic, gpmOverlay,
    -1, wpakRules, 0, False, wseWhole);
  Result.Passes[1] := MakeWfcPipelinePass('model', wppvPublic, gpmOverlay,
    -1, wpakModel, 1, False, wseWhole);
  Result.Passes[2] := MakeWfcPipelinePass('forward-alias', wppvPublic, gpmTransform,
    4, wpakEmpty, -1, False, wseWhole);
  Result.Passes[3] := MakeWfcPipelinePass('private', wppvPrivate, gpmOverlay,
    -1, wpakRules, 0, False, wseWhole);
  Result.Passes[4] := MakeWfcPipelinePass('alias', wppvPublic, gpmTransform,
    0, wpakEmpty, -1, False, wseWhole);
  SetLength(Result.Dependencies, 2);
  Result.Dependencies[0] := MakeWfcPipelineDependency(2, 4);
  Result.Dependencies[1] := MakeWfcPipelineDependency(4, 0);
end;

function BuildProjection(const APattern: Boolean): TInputs;
var Pattern: TWfcOverlappingModel2D; Sequence: TWfcSequenceModel;
begin
  Result := Default(TInputs);
  Result.Metadata := MakeWfcPipelineMetadata('Quota projection', 'MIT', 'authored', '');
  SetLength(Result.Resources, 1); SetLength(Result.Passes, 3);
  SetLength(Result.Dependencies, 2); SetLength(Result.Bridges, 1);
  Result.Dependencies[0] := MakeWfcPipelineDependency(1, 0);
  Result.Dependencies[1] := MakeWfcPipelineDependency(2, 1);
  if APattern then
  begin
    Result.Rank := 2; Result.Wrap := True;
    Pattern := LearnOverlappingModel2D(Tokens(['land', 'water', 'land', 'water']),
      2, 2, 1, 1, wmbWrap, wmsNone);
    try Result.Resources[0] := MakeWfcPipelineResource('latent', wprkPattern2D,
      EncodeWfcPattern2DText(Pattern), 'authored pattern', 'MIT', '');
    finally Pattern.Free; end;
    Result.Passes[0] := MakeWfcPipelinePass('latent', wppvPrivate, gpmOverlay,
      -1, wpakPattern2D, 0, False, wseWhole);
    Result.Bridges[0] := MakeWfcPipelineBridge(wpbkPattern2DProjection, 0, 1);
  end else
  begin
    Result.Rank := 1;
    Sequence := LearnSequenceModel(Tokens(['land', 'water', 'land']), 2);
    try Result.Resources[0] := MakeWfcPipelineResource('latent', wprkSequence,
      EncodeWfcSequenceText(Sequence), 'authored sequence', 'MIT', '');
    finally Sequence.Free; end;
    Result.Passes[0] := MakeWfcPipelinePass('latent', wppvPrivate, gpmOverlay,
      -1, wpakSequence, 0, True, wseWhole);
    Result.Bridges[0] := MakeWfcPipelineBridge(wpbkSequenceProjection, 0, 1);
  end;
  Result.Passes[1] := MakeWfcPipelinePass('surface', wppvPublic, gpmOverlay,
    -1, wpakEmpty, -1, False, wseWhole);
  Result.Passes[2] := MakeWfcPipelinePass('alias', wppvPublic, gpmTransform,
    1, wpakEmpty, -1, False, wseWhole);
end;

function NewRecipe(const A: TInputs; const AExplicitVersion: Boolean = False): TWfcPipelineModel;
begin
  if AExplicitVersion then
    Result := TWfcPipelineModel.Create(A.Metadata, CurrentWfcPipelineVersions,
      A.Rank, A.Wrap, rmBottomUp, A.Resources, A.Passes, A.Dependencies,
      A.Bridges, A.Requirements, A.Quotas)
  else Result := TWfcPipelineModel.Create(A.Metadata, A.Rank, A.Wrap,
    rmBottomUp, A.Resources, A.Passes, A.Dependencies, A.Bridges, A.Requirements, A.Quotas);
end;

function Rejected(const A: TInputs; const AFragment: String): Boolean;
var Recipe: TWfcPipelineModel;
begin
  Result := False; Recipe := nil;
  try
    try Recipe := NewRecipe(A);
    except on E: EWfcPipelineModel do
      begin
        Result := (AFragment = '') or (Pos(AFragment, E.Message) > 0);
        if not Result then WriteLn('[DIAGNOSTIC] ', E.Message);
      end;
    end;
  finally Recipe.Free; end;
end;

procedure TestOwnershipAndIdentity;
var
  A: TInputs; Recipe, Replay, Legacy: TWfcPipelineModel;
  Q: TWfcPipelineValueQuota; V: TWfcModelTokens; CopyQ: TWfcPipelineValueQuotas;
  Original: TWfcPipelineSignature; I: Integer;
begin
  Check((WFC_PIPELINE_MODEL_VERSION = 1) and
    (WFC_PIPELINE_MODEL_SIGNATURE_VERSION = 1) and
    (WFC_PIPELINE_GRAPH_ADAPTER_VERSION = 1) and
    (WFC_PIPELINE_VALUE_QUOTA_VERSION = 1), 'quota is independently opt-in versioned');
  A := BuildInputs;
  Recipe := NewRecipe(A);
  Legacy := TWfcPipelineModel.Create(A.Metadata, A.Rank, A.Wrap, rmBottomUp,
    A.Resources, A.Passes, A.Dependencies, A.Bridges, A.Requirements);
  Replay := TWfcPipelineModel.Create(A.Metadata, CurrentWfcPipelineVersions,
    A.Rank, A.Wrap, rmBottomUp, A.Resources, A.Passes, A.Dependencies, A.Bridges, A.Requirements);
  try
    Check((Recipe.Signature = Legacy.Signature) and (Recipe.Signature = Replay.Signature),
      'all original and empty-extension constructor signatures are identical');
    Check((Recipe.ValueQuotaVersion = 0) and (Recipe.ValueQuotaCount = 0),
      'empty extension has no active capability');
    Check(Length(Recipe.CopyValueQuotas) = 0, 'empty detached registry');
  finally Replay.Free; Legacy.Free; Recipe.Free; end;
  V := Tokens(['z', 'a']);
  Q := MakeWfcPipelineValueQuota(0, 'housing-' + NoteToken, V, 1, 4);
  V[0] := 'mutated'; Check(Q.Values[0] = 'z', 'factory detaches token array');
  SetLength(A.Quotas, 2); A.Quotas[0] := Q;
  A.Quotas[1] := Quota(2, 'housing-' + NoteToken, ['z'], 0, High(Integer));
  Recipe := NewRecipe(A); Replay := NewRecipe(A, True);
  try
    Original := Recipe.Signature;
    Check((Recipe.ValueQuotaCount = 2) and (Recipe.ValueQuotaVersion = 1), 'nonempty immutable extension');
    Check(Replay.Signature = Original, 'explicit/current version overloads agree');
    Q := Recipe.ValueQuotaAt(0);
    Check((Q.PassIndex = 0) and (Q.LabelText = 'housing-' + NoteToken) and
      (Q.MinimumCount = 1) and (Q.MaximumCount = 4) and (Length(Q.Values) = 2),
      'exact quota descriptor retained');
    A.Quotas[0].Values[0] := 'mutated'; A.Quotas[0].MinimumCount := 99;
    Q.Values[0] := 'mutated'; Q.LabelText := 'mutated';
    CopyQ := Recipe.CopyValueQuotas; CopyQ[0].Values[0] := 'mutated';
    CopyQ[1].PassIndex := 99;
    Q := Recipe.ValueQuotaAt(0);
    Check((Q.Values[0] = 'z') and (Q.MinimumCount = 1) and
      (Q.LabelText = 'housing-' + NoteToken), 'constructor and accessors deep-copy managed layers');
    Q := Recipe.ValueQuotaAt(1);
    Check((Q.PassIndex = 2) and (Q.MaximumCount = High(Integer)), 'alias declaration identity and large upper bound retained');
    Check(Recipe.Signature = Original, 'external mutation cannot change immutable identity');
    A.Quotas := Recipe.CopyValueQuotas;
    for I := 0 to 5 do
    begin
      CopyQ := Recipe.CopyValueQuotas;
      case I of
        0: CopyQ[0].PassIndex := 1;
        1: CopyQ[0].LabelText := 'different';
        2: CopyQ[0].Values := Tokens(['z']);
        3: CopyQ[0].MinimumCount := 0;
        4: CopyQ[0].MaximumCount := 5;
        5: begin Q := CopyQ[0]; CopyQ[0] := CopyQ[1]; CopyQ[1] := Q; end;
      end;
      A.Quotas := CopyQ; Legacy := NewRecipe(A);
      try Check(Legacy.Signature <> Original, 'every quota field and declaration order contributes to identity');
      finally Legacy.Free; end;
    end;
    Check(Original = Cardinal($133DEAB2), 'version-1 quota extension signature golden');
    WriteLn('PIPELINE_VALUE_QUOTA_MODEL_SIGNATURE=', WfcPipelineSignatureHex(Original));
  finally Replay.Free; Recipe.Free; end;
end;

procedure TestVocabularyAndBounds;
var A: TInputs; Q: TWfcPipelineValueQuota; Recipe: TWfcPipelineModel; I, J: Integer;
  Failed: Boolean; V: TWfcModelTokens;
begin
  A := BuildInputs; SetLength(A.Quotas, 1);
  A.Quotas[0] := Quota(2, 'alias', ['z', 'a', NoteToken], High(Integer), High(Integer));
  Recipe := NewRecipe(A);
  try
    Q := Recipe.ValueQuotaAt(0);
    Check(Q.MinimumCount = High(Integer), 'minimum greater than any eventual small shape remains valid IR');
    V := Recipe.CopyPublicVocabulary(2);
    Check((Length(V) = 3) and (V[0] = 'z') and (V[1] = 'a'),
      'forward-declared transform chain retains public vocabulary order');
    for I := -1 to 1 do if I <> 0 then
    begin
      Failed := False;
      try Q := Recipe.ValueQuotaAt(I); except on E: ERangeError do Failed := True; end;
      Check(Failed, 'quota index range checked');
    end;
  finally Recipe.Free; end;
  for I := 0 to 12 do
  begin
    A := BuildInputs; SetLength(A.Quotas, 1);
    A.Quotas[0] := Quota(0, 'quota', ['z', 'a'], 0, 5);
    case I of
      0: A.Quotas[0].PassIndex := -1; 1: A.Quotas[0].PassIndex := 5;
      2: A.Quotas[0].PassIndex := 3; 3: A.Quotas[0].LabelText := '';
      4: A.Quotas[0].Values := nil;
      5: A.Quotas[0].Values := Tokens(['unknown']);
      6: A.Quotas[0].Values := Tokens(['a', 'z']);
      7: A.Quotas[0].Values := Tokens(['z', 'z']);
      8: A.Quotas[0].MinimumCount := -1;
      9: A.Quotas[0].MaximumCount := -1;
      10: A.Quotas[0].MinimumCount := 6;
      11: begin SetLength(A.Quotas, 2); A.Quotas[1] := A.Quotas[0]; end;
      12: begin A.Quotas[0].PassIndex := 2; A.Passes[0].Visibility := wppvPrivate; end;
    end;
    Check(Rejected(A, ''), 'invalid owner, label, token set or quota bounds rejected');
  end;
  for I := 0 to 1 do
  begin
    A := BuildProjection(I = 0); SetLength(A.Quotas, 1);
    for J := 1 to 2 do
    begin
      A.Quotas[0] := Quota(J, 'projection', ['land', 'water'], 0, 10);
      Recipe := NewRecipe(A);
      try
        Q := Recipe.ValueQuotaAt(0);
        Check(Q.PassIndex = J, 'projected public owner/alias accepted, never latent representation');
      finally Recipe.Free; end;
    end;
    A.Quotas[0].PassIndex := 0;
    Check(Rejected(A, 'owner must be public'), 'private sequence/pattern states cannot own public quotas');
    A.Quotas[0].PassIndex := 1; A.Quotas[0].Values := Tokens(['@p0']);
    Check(Rejected(A, 'outside the public vocabulary'), 'latent-style key is not a projected public token');
  end;
end;

procedure TestCapacityPreflight;
var A: TInputs; I, J: Integer; Values: TWfcModelTokens; Failed: Boolean;
begin
  Check((WFC_PIPELINE_MAX_VALUE_QUOTA_COUNT = 4096) and
    (WFC_PIPELINE_MAX_VALUE_QUOTA_TOKEN_COUNT = 1024) and
    (WFC_PIPELINE_MAX_TOTAL_VALUE_QUOTA_TOKEN_COUNT = 65536), 'quota resource envelope pinned');
  A := BuildInputs; A.Resources[0].Document := 'not a resource';
  SetLength(A.Quotas, WFC_PIPELINE_MAX_VALUE_QUOTA_COUNT + 1);
  Check(Rejected(A, 'pipeline value-quota count'), 'quota outer count checked before nested decoder');
  SetLength(A.Quotas, 1); A.Quotas[0] := Quota(0, 'quota', ['z'], 0, 0);
  SetLength(A.Quotas[0].Values, WFC_PIPELINE_MAX_VALUE_QUOTA_TOKEN_COUNT + 1);
  Check(Rejected(A, 'token count'), 'quota per-record token count checked before nested decoder');
  Values := A.Quotas[0].Values; Failed := False;
  try MakeWfcPipelineValueQuota(0, 'quota', Values, 0, 0);
  except on E: EWfcPipelineModel do Failed := True; end;
  Check(Failed, 'factory rejects oversized clone before allocating owned tokens');
  SetLength(A.Quotas, (WFC_PIPELINE_MAX_TOTAL_VALUE_QUOTA_TOKEN_COUNT div
    WFC_PIPELINE_MAX_VALUE_QUOTA_TOKEN_COUNT) + 1);
  for I := 0 to High(A.Quotas) do
  begin
    A.Quotas[I] := Quota(0, 'q' + IntToStr(I), ['z'], 0, 0);
    SetLength(A.Quotas[I].Values, WFC_PIPELINE_MAX_VALUE_QUOTA_TOKEN_COUNT);
    for J := 0 to High(A.Quotas[I].Values) do A.Quotas[I].Values[J] := 'z';
  end;
  Check(Rejected(A, 'aggregate value-quota token count'),
    'aggregate quota token envelope checked before decoding or semantic ordering');
  SetLength(A.Quotas, 1); A.Quotas[0] := Quota(0, 'quota', ['z'], 0, 0);
  A.Quotas[0].LabelText := TWfcModelToken(StringOfChar('x', WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH + 1));
  Check(Rejected(A, 'raw token length'), 'quota label raw byte preflight precedes decoding');
  A.Quotas[0].LabelText := 'quota';
  A.Quotas[0].Values[0] := TWfcModelToken(StringOfChar('x', WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH + 1));
  Check(Rejected(A, 'raw token length'), 'quota value raw byte preflight precedes decoding');
  SetLength(A.Quotas, 17);
  for I := 0 to High(A.Quotas) do
  begin
    A.Quotas[I] := Quota(0, 'quota', ['z'], 0, 0);
    A.Quotas[I].LabelText := TWfcModelToken(StringOfChar('x', WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH));
  end;
  Check(Rejected(A, 'aggregate outer-token encoding'),
    'quota labels share the existing aggregate token-byte budget before nested decode');
end;

{$IFDEF PAS2JS}
procedure TestBrowserNumbers;
var A: TInputs; X: Integer; I, J: Integer; Recipe: TWfcPipelineModel;
  Q: TWfcPipelineValueQuota; Failed: Boolean;
begin
  A := BuildInputs; SetLength(A.Quotas, 1);
  A.Quotas[0] := Quota(0, 'quota', ['z'], 0, 5);
  Recipe := NewRecipe(A);
  try
    for I := 0 to 10 do
    begin
      case I of
        0: asm X = NaN; end; 1: asm X = Infinity; end;
        2: asm X = -Infinity; end; 3: asm X = undefined; end;
        4: asm X = null; end; 5: asm X = "0"; end;
        6: asm X = 0.5; end; 7: asm X = true; end;
        8: asm X = 2147483648; end; 9: asm X = 4294967296; end;
        10: asm X = 9007199254740992; end;
      end;
      for J := 0 to 2 do
      begin
        A.Quotas[0] := Quota(0, 'quota', ['z'], 0, 5);
        case J of
          0: A.Quotas[0].PassIndex := X;
          1: A.Quotas[0].MinimumCount := X;
          2: A.Quotas[0].MaximumCount := X;
        end;
        Check(Rejected(A, 'exact integer'), 'malformed JS quota integer rejected without coercion');
      end;
      Failed := False;
      try Q := Recipe.ValueQuotaAt(X); except on E: ERangeError do Failed := True; end;
      Check(Failed, 'malformed JS accessor index rejected without array indexing');
    end;
  finally Recipe.Free; end;
end;
{$ENDIF}

begin
  RunTest('detached registry and opt-in identity', @TestOwnershipAndIdentity);
  RunTest('public vocabulary, aliases, projections and inclusive bounds', @TestVocabularyAndBounds);
  RunTest('resource preflight before nested decoding', @TestCapacityPreflight);
  {$IFDEF PAS2JS}RunTest('strict browser quota integers', @TestBrowserNumbers);{$ENDIF}
  WriteLn('[SUMMARY] checks=', Checks, ' failures=', Failures);
  if Failures <> 0 then Halt(1);
end.
