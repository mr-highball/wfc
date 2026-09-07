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
program wfc_training_value_quota_model_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc_model, wfc_training, wfc_pipeline_model, wfc_pipeline_text;

type
  TTestProcedure = procedure;
  TInputs = record
    Metadata: TWfcTrainingMetadata;
    Options: TWfcTrainingOptions;
    Samples: TWfcTrainingSamples;
    Quotas: TWfcTrainingValueQuotas;
  end;
var
  Checks: Integer;
  Failures: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if ACondition then
    Exit;
  Inc(Failures);
  WriteLn('[FAIL] ', AMessage);
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try
    ATest;
  except
    on E: Exception do
    begin
      Inc(Failures);
      WriteLn('[EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function Tokens(const AValues: array of TWfcModelToken): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do
    Result[I] := AValues[I];
end;

function NoteToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := Chr($266B);
  {$ELSE}
  Result := UTF8Encode(UnicodeString(WideChar($266B)));
  {$ENDIF}
end;

function BadToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}Result := Chr($D800);
  {$ELSE}SetLength(Result, 1); Result[1] := AnsiChar($FF);{$ENDIF}
end;

function Quota(const ALabel: TWfcModelToken;
  const AValues: array of TWfcModelToken;
  const AMin, AMax: Integer): TWfcTrainingValueQuota;
begin
  Result := MakeWfcTrainingValueQuota(ALabel, Tokens(AValues), AMin, AMax);
end;

function BuildInputs(const AKind: TWfcTrainingKind = wtkAdjacency1D): TInputs;
begin
  Result := Default(TInputs);
  Result.Metadata := MakeWfcTrainingMetadata('Authored training quotas',
    'MIT', 'Project-authored bounded corpus');
  Result.Options := MakeWfcTrainingOptions(AKind, wmbOpen, wmsNone, 0, 0, 0);
  SetLength(Result.Samples, 1);
  Result.Samples[0] := MakeWfcTrainingSample('observations', 6, 1,
    Tokens(['B', 'A', 'B', 'B', 'A', 'A']));
  case AKind of
    wtkAdjacency2D, wtkPattern2D:
    begin
      Result.Samples[0].Width := 3;
      Result.Samples[0].Height := 2;
    end;
    wtkAdjacency3D, wtkPattern3D:
    begin
      Result.Samples[0].Width := 3;
      Result.Samples[0].Depth := 2;
    end;
    wtkSequence: Result.Options.Order := 1;
  end;
  if AKind in [wtkPattern2D, wtkPattern3D] then
  begin
    Result.Options.Boundary := wmbWrap;
    Result.Options.PatternWidth := 1;
    Result.Options.PatternHeight := 1;
  end;
end;

function NewDocument(const A: TInputs): TWfcTrainingDocument;
begin
  Result := TWfcTrainingDocument.Create(A.Metadata, A.Options,
    A.Samples, A.Quotas);
end;

function Rejected(const A: TInputs; const AMessagePart: String): Boolean;
var
  D: TWfcTrainingDocument;
begin
  D := nil;
  Result := False;
  try
    try
      D := NewDocument(A);
    except
      on E: EWfcTraining do
        Result := (AMessagePart = '') or (Pos(AMessagePart, E.Message) > 0);
    end;
  finally
    D.Free;
  end;
end;

function SignatureOf(const A: TInputs): Cardinal;
var
  D: TWfcTrainingDocument;
begin
  D := NewDocument(A);
  try Result := D.Signature; finally D.Free; end;
end;

procedure TestLegacyProfilesAndLowering;
var
  A: TInputs;
  K: TWfcTrainingKind;
  OldDocument, EmptyDocument, AuthoredDocument: TWfcTrainingDocument;
  OldRecipe, EmptyRecipe, AuthoredRecipe: TWfcPipelineModel;
  Q: TWfcPipelineValueQuota;
  S: String;
  Failed: Boolean;
  Owner: Integer;
begin
  for K := Low(TWfcTrainingKind) to High(TWfcTrainingKind) do
  begin
    A := BuildInputs(K);
    OldDocument := TWfcTrainingDocument.Create(A.Metadata, A.Options, A.Samples);
    EmptyDocument := nil;
    AuthoredDocument := nil;
    OldRecipe := nil;
    EmptyRecipe := nil;
    AuthoredRecipe := nil;
    try
      EmptyDocument := NewDocument(A);
      Check((OldDocument.Signature = EmptyDocument.Signature) and
        (EmptyDocument.ValueQuotaVersion = 0) and
        (EmptyDocument.ValueQuotaCount = 0) and
        (Length(EmptyDocument.CopyValueQuotas) = 0),
        'old/empty document identity and capability, kind ' + IntToStr(Ord(K)));
      S := LearnWfcTrainingModelText(OldDocument);
      Check(S = LearnWfcTrainingModelText(EmptyDocument),
        'empty extension preserves learned model bytes');
      OldRecipe := LearnWfcTrainingRecipe(OldDocument);
      EmptyRecipe := LearnWfcTrainingRecipe(EmptyDocument);
      Check(EncodeWfcPipelineModelText(OldRecipe) =
        EncodeWfcPipelineModelText(EmptyRecipe),
        'empty extension preserves complete recipe bytes');

      SetLength(A.Quotas, 2);
      A.Quotas[0] := Quota('inclusive union', ['A', 'B'], 0, High(Integer));
      A.Quotas[1] := Quota(NoteToken, ['A'], 2, 3);
      AuthoredDocument := NewDocument(A);
      Check((AuthoredDocument.ValueQuotaVersion = 1) and
        (AuthoredDocument.Signature <> OldDocument.Signature) and
        (AuthoredDocument.ValueQuotaAt(0).Values[0] = 'A'),
        'quota source opts into identity while preserving authored order');
      AuthoredRecipe := LearnWfcTrainingRecipe(AuthoredDocument);
      Check(AuthoredRecipe.ResourceAt(0).Document = S,
        'hard quotas never alter learned observations/model payload');
      Owner := AuthoredRecipe.FindPass('output');
      if K in [wtkPattern2D, wtkPattern3D, wtkSequence] then
        Check(Owner = 1, 'projection quota resolves public output, not latent pass')
      else
        Check(Owner = 0, 'direct quota resolves public output');
      Q := AuthoredRecipe.ValueQuotaAt(0);
      Check((AuthoredRecipe.ValueQuotaCount = 2) and (Q.PassIndex = Owner) and
        (Q.MinimumCount = 0) and (Q.MaximumCount = High(Integer)) and
        (Length(Q.Values) = 2) and (Q.Values[0] = 'B') and (Q.Values[1] = 'A'),
        'lowering uses actual learned vocabulary order and exact inclusive bounds');
      Q := AuthoredRecipe.ValueQuotaAt(1);
      Check((Q.LabelText = NoteToken) and (Q.Values[0] = 'A') and
        (Q.MinimumCount = 2) and (Q.MaximumCount = 3),
        'quota ordinal, Unicode label and individual token set survive lowering');
      if K = wtkPattern3D then
        Check(Pos('wfclearn-v6/', String(AuthoredRecipe.CopyMetadata.SourceFingerprint)) = 1,
          'pattern volume provenance retains its newer capability')
      else Check(Pos('wfclearn-v3/', String(AuthoredRecipe.CopyMetadata.SourceFingerprint)) = 1,
        'quota-only legacy provenance retains v3');
      Check((AuthoredRecipe.CopyMetadata.SourceFingerprint =
        AuthoredRecipe.ResourceAt(0).SourceFingerprint),
        'recipe and resource provenance identify authored training source');
      Failed := False;
      try
        S := LearnWfcTrainingModelText(AuthoredDocument);
      except
        on E: EWfcTraining do
          Failed := Pos('standalone model export cannot represent authored value quotas',
            E.Message) > 0;
      end;
      Check(Failed, 'model-only export fails explicitly instead of dropping hard policy');
    finally
      AuthoredRecipe.Free;
      EmptyRecipe.Free;
      OldRecipe.Free;
      AuthoredDocument.Free;
      EmptyDocument.Free;
      OldDocument.Free;
    end;
  end;
end;

procedure TestOwnershipAndIdentity;
var
  A: TInputs;
  D: TWfcTrainingDocument;
  CopyQuotas: TWfcTrainingValueQuotas;
  Q: TWfcTrainingValueQuota;
  Values: TWfcModelTokens;
  Baseline: Cardinal;
  Failed: Boolean;
begin
  A := BuildInputs;
  Values := Tokens(['A', 'B']);
  SetLength(A.Quotas, 2);
  A.Quotas[0] := MakeWfcTrainingValueQuota('union', Values, 0, 8);
  A.Quotas[1] := Quota(NoteToken, ['A'], 2, 3);
  Values[0] := 'mutated';
  Check(A.Quotas[0].Values[0] = 'A', 'factory detaches caller token array');
  D := NewDocument(A);
  try
    Baseline := D.Signature;
    WriteLn('[GOLDEN] source=', WfcTrainingSignatureHex(Baseline));
    Check(WfcTrainingSignatureHex(Baseline) = 'E15671A6',
      'authored quota source signature is pinned');
    A.Quotas[0].Values[0] := 'mutated';
    A.Quotas[0].LabelText := 'mutated';
    A.Quotas[1].MinimumCount := 0;
    Q := D.ValueQuotaAt(0);
    Check((Q.LabelText = 'union') and (Q.Values[0] = 'A') and
      (D.ValueQuotaAt(1).MinimumCount = 2),
      'document owns nested quota descriptors');
    Q.Values[0] := 'mutated';
    Check(D.ValueQuotaAt(0).Values[0] = 'A', 'single quota accessor is detached');
    CopyQuotas := D.CopyValueQuotas;
    CopyQuotas[0].Values[0] := 'mutated';
    CopyQuotas[1].MaximumCount := 9;
    Check((D.ValueQuotaAt(0).Values[0] = 'A') and
      (D.ValueQuotaAt(1).MaximumCount = 3) and (D.Signature = Baseline),
      'full quota copy is deeply detached and identity immutable');
    Failed := False;
    try Q := D.ValueQuotaAt(-1); except on E: ERangeError do Failed := True; end;
    Check(Failed, 'negative quota accessor index rejected');
    Failed := False;
    try Q := D.ValueQuotaAt(2); except on E: ERangeError do Failed := True; end;
    Check(Failed, 'past-end quota accessor index rejected');
    A.Quotas := D.CopyValueQuotas;
    Check(SignatureOf(A) = Baseline, 'detached reconstruction retains identity');
    A.Quotas[0].LabelText := 'renamed';
    Check(SignatureOf(A) <> Baseline, 'quota label contributes to source identity');
    A.Quotas := D.CopyValueQuotas;
    A.Quotas[0].MinimumCount := 1;
    Check(SignatureOf(A) <> Baseline, 'minimum contributes to source identity');
    A.Quotas := D.CopyValueQuotas;
    A.Quotas[0].MaximumCount := 9;
    Check(SignatureOf(A) <> Baseline, 'maximum contributes to source identity');
    A.Quotas := D.CopyValueQuotas;
    A.Quotas[0].Values := Tokens(['B', 'A']);
    Check(SignatureOf(A) <> Baseline, 'explicit authored token order contributes to identity');
    A.Quotas := D.CopyValueQuotas;
    A.Quotas[1].Values := Tokens(['B']);
    Check(SignatureOf(A) <> Baseline, 'accepted token membership contributes to identity');
    A.Quotas := D.CopyValueQuotas;
    Q := A.Quotas[0]; A.Quotas[0] := A.Quotas[1]; A.Quotas[1] := Q;
    Check(SignatureOf(A) <> Baseline, 'quota declaration order contributes to identity');
    A.Quotas := D.CopyValueQuotas;
    SetLength(A.Quotas, 1);
    Check(SignatureOf(A) <> Baseline, 'quota count contributes to identity');
  finally
    D.Free;
  end;
end;

procedure TestValidation;
var
  A: TInputs;
  D: TWfcTrainingDocument;
  R: TWfcPipelineModel;
  Failed: Boolean;
  S: String;
begin
  A := BuildInputs;
  SetLength(A.Quotas, 1);
  A.Quotas[0] := Quota('q', ['A'], High(Integer), High(Integer));
  D := NewDocument(A);
  try
    Check(D.ValueQuotaAt(0).MinimumCount = High(Integer),
      'minimum beyond observed/source extent remains explicit valid policy');
  finally D.Free; end;
  A.Quotas[0].MinimumCount := -1;
  Check(Rejected(A, 'exact integer'), 'negative minimum rejected');
  A.Quotas[0].MinimumCount := 0; A.Quotas[0].MaximumCount := -1;
  Check(Rejected(A, 'exact integer'), 'negative maximum rejected');
  A.Quotas[0].MinimumCount := 2; A.Quotas[0].MaximumCount := 1;
  Check(Rejected(A, 'minimum exceeds maximum'), 'reversed inclusive bounds rejected');
  A.Quotas[0] := Quota('q', ['A', 'A'], 0, 0);
  Check(Rejected(A, 'tokens must be unique'), 'duplicate accepted tokens are rejected');
  A.Quotas[0] := Quota('q', ['unknown'], 0, 0);
  Check(Rejected(A, 'absent from the source'), 'unknown tokens rejected even for a zero minimum');
  A.Quotas[0] := Quota('q', [], 0, 0);
  Check(Rejected(A, 'token count'), 'empty accepted set rejected');
  A.Quotas[0] := Quota('', ['A'], 0, 0);
  Check(Rejected(A, 'nonempty'), 'empty label rejected');
  A.Quotas[0] := Quota('q', [''], 0, 0);
  Check(Rejected(A, 'nonempty'), 'empty token rejected');
  A.Quotas[0] := Quota(BadToken, ['A'], 0, 0);
  Check(Rejected(A, 'well-formed'), 'malformed Unicode label rejected');
  A.Quotas[0] := Quota('q', [BadToken], 0, 0);
  Check(Rejected(A, 'well-formed'), 'malformed Unicode token rejected');
  SetLength(A.Quotas, 2);
  A.Quotas[0] := Quota('q', ['A'], 0, 0);
  A.Quotas[1] := Quota('q', ['B'], 1, 1);
  Check(Rejected(A, 'labels must be unique'), 'duplicate quota labels rejected');

  A := BuildInputs(wtkPattern2D);
  A.Options.Boundary := wmbOpen;
  D := NewDocument(A);
  try
    S := LearnWfcTrainingModelText(D);
    Check(S <> '', 'quota-free open pattern standalone model remains supported');
    R := nil; Failed := False;
    try
      try R := LearnWfcTrainingRecipe(D);
      except on E: EWfcTraining do Failed := Pos('wrapped training', E.Message) > 0; end;
    finally R.Free; end;
    Check(Failed, 'open pattern recipe export remains explicitly unsupported');
  finally D.Free; end;
  SetLength(A.Quotas, 1); A.Quotas[0] := Quota('q', ['A'], 1, 2);
  D := NewDocument(A);
  try
    Failed := False;
    try S := LearnWfcTrainingModelText(D);
    except on E: EWfcTraining do Failed := Pos('value quotas', E.Message) > 0; end;
    Check(Failed, 'open pattern policy cannot leak through model-only export');
  finally D.Free; end;
end;

procedure TestCapacities;
var
  A: TInputs;
  D: TWfcTrainingDocument;
  I: Integer;
  J: Integer;
  Values: TWfcModelTokens;
  Failed: Boolean;
  Q: TWfcTrainingValueQuota;
begin
  A := BuildInputs;
  SetLength(A.Quotas, WFC_TRAINING_MAX_VALUE_QUOTA_COUNT + 1);
  Check(Rejected(A, 'quota count'), 'outer quota count rejected before element access');
  SetLength(A.Quotas, 1);
  A.Quotas[0] := Quota('q', ['A'], 0, 1);
  SetLength(A.Quotas[0].Values, WFC_TRAINING_MAX_VALUE_QUOTA_TOKEN_COUNT + 1);
  Check(Rejected(A, 'token count'), 'per-quota token count rejected before token validation');
  Values := A.Quotas[0].Values;
  Failed := False;
  try Q := MakeWfcTrainingValueQuota('q', Values, 0, 1);
  except on E: EWfcTraining do Failed := True; end;
  Check(Failed, 'factory bounds token allocation before copying');

  SetLength(A.Quotas, 65);
  for I := 0 to High(A.Quotas) do
  begin
    A.Quotas[I] := Quota('q', ['A'], 0, 1);
    SetLength(A.Quotas[I].Values, 1024);
    for J := 0 to High(A.Quotas[I].Values) do A.Quotas[I].Values[J] := 'A';
  end;
  Check(Rejected(A, 'aggregate training value quota token count'),
    'aggregate declared tokens rejected before duplicate/token lookups');

  A := BuildInputs;
  SetLength(A.Quotas, 1);
  A.Quotas[0] := Quota('q', ['A'], 0, 1);
  A.Quotas[0].LabelText := TWfcModelToken(StringOfChar('x', 65537));
  Check(Rejected(A, 'within the version-1 limit'), 'oversized label rejected before copy');
  A.Quotas[0] := Quota('q', ['A'], 0, 1);
  A.Quotas[0].Values[0] := TWfcModelToken(StringOfChar('x', 65537));
  Check(Rejected(A, 'within the version-1 limit'), 'oversized quota token rejected before copy');
  A.Quotas[0] := Quota('q', ['A'], 0, 1);
  A.Quotas[0].LabelText := TWfcModelToken(StringOfChar(' ', 21846));
  Check(Rejected(A, 'encoded length'), 'encoded token limit checked after percent expansion');

  A := BuildInputs;
  SetLength(A.Quotas, 64);
  for I := 0 to High(A.Quotas) do
    A.Quotas[I] := Quota(TWfcModelToken(StringOfChar('x', 64995) +
      Format('%.5d', [I])), ['B'], 0, 1);
  A.Samples[0].Tokens[1] := TWfcModelToken(StringOfChar('y', 65536));
  Check(Rejected(A, 'aggregate encoded training-token length'),
    'quota labels and source observations share one encoded byte budget');

  A := BuildInputs;
  SetLength(A.Quotas, WFC_TRAINING_MAX_VALUE_QUOTA_COUNT);
  for I := 0 to High(A.Quotas) do
    A.Quotas[I] := Quota(TWfcModelToken('q' + IntToStr(I)), ['A'], 0, High(Integer));
  D := NewDocument(A);
  try Check(D.ValueQuotaCount = 4096, 'exact outer quota capacity is accepted');
  finally D.Free; end;

  A := BuildInputs;
  SetLength(Values, 1024);
  for I := 0 to High(Values) do Values[I] := TWfcModelToken('v' + IntToStr(I));
  A.Samples[0] := MakeWfcTrainingSample('vocabulary', 1024, 1, Values);
  SetLength(A.Quotas, 64);
  for I := 0 to High(A.Quotas) do
    A.Quotas[I] := MakeWfcTrainingValueQuota(TWfcModelToken('q' + IntToStr(I)),
      Values, 0, High(Integer));
  D := NewDocument(A);
  try
    Check((D.ValueQuotaCount = 64) and (Length(D.ValueQuotaAt(63).Values) = 1024),
      'exact per-quota and aggregate 65536-token capacities are accepted');
  finally D.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestBrowserNumbers;
var
  A: TInputs;
  D: TWfcTrainingDocument;
  X: Integer;
  I: Integer;
  J: Integer;
  Q: TWfcTrainingValueQuota;
  Failed: Boolean;
begin
  A := BuildInputs;
  SetLength(A.Quotas, 1); A.Quotas[0] := Quota('q', ['A'], 0, 5);
  D := NewDocument(A);
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
      for J := 0 to 1 do
      begin
        A.Quotas[0] := Quota('q', ['A'], 0, 5);
        if J = 0 then A.Quotas[0].MinimumCount := X
        else A.Quotas[0].MaximumCount := X;
        Check(Rejected(A, 'exact integer'), 'malformed JS quota bound rejected without coercion');
      end;
      Failed := False;
      try Q := D.ValueQuotaAt(X); except on E: ERangeError do Failed := True; end;
      Check(Failed, 'malformed JS quota accessor rejected before indexing');
    end;
  finally D.Free; end;
end;
{$ENDIF}

begin
  RunTest('legacy profiles and exact public lowering', @TestLegacyProfilesAndLowering);
  RunTest('detached author policy and identity', @TestOwnershipAndIdentity);
  RunTest('semantic rejection and model-only policy', @TestValidation);
  RunTest('preflight and exact capacity boundaries', @TestCapacities);
  {$IFDEF PAS2JS}RunTest('strict browser quota numbers', @TestBrowserNumbers);{$ENDIF}
  WriteLn('[SUMMARY] checks=', Checks, ' failures=', Failures);
  if Failures <> 0 then Halt(1);
end.
