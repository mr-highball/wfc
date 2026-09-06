{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_training_sequence_wrap_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_sequence, wfc_sequence_learn,
  wfc_sequence_text, wfc_training, wfc_training_text, wfc_training_workspace,
  wfc_pipeline_model, wfc_pipeline_text, wfc_pipeline_run,
  wfc_pipeline_run_text, wfc_pipeline_result, wfc_pipeline_result_text,
  wfc_pipeline_runtime, wfc_pipeline_connectivity,
  wfc_artifact_document, wfc_artifact_inspect, wfc_learn_app;

const
  SOURCE_TEXT = 'wfclearn=5'#10'name=circular'#10'license=MIT'#10 +
    'source=authored'#10'kind=sequence'#10'boundary=wrap'#10 +
    'symmetry=none'#10'footprint=0,0'#10'order=3'#10'samples=1'#10 +
    'sample=0,2,1,1,loop'#10'token=0,0,A'#10'token=0,1,B'#10 +
    'value-quota-version=0'#10'value-quotas=0'#10 +
    'connectivity-version=0'#10'connectivities=0'#10'end'#10;
  MODEL_TEXT = 'wfcs=2'#10'boundary=wrap'#10'order=3'#10 +
    'samples=1'#10's=0,2'#10'tokens=2'#10't=0,A'#10't=1,B'#10 +
    'states=2'#10'q=0,1,0,0,T0,T1,E0'#10'q=1,1,0,0,T1,T0,E1'#10'end'#10;
  OPEN_SOURCE_TEXT = 'wfclearn=1'#10'name=circular'#10'license=MIT'#10 +
    'source=authored'#10'kind=sequence'#10'boundary=open'#10 +
    'symmetry=none'#10'footprint=0,0'#10'order=3'#10'samples=1'#10 +
    'sample=0,2,1,loop'#10'token=0,0,A'#10'token=0,1,B'#10'end'#10;
  OPEN_MODEL_TEXT = 'wfcs=1'#10'order=3'#10'samples=1'#10's=0,2'#10 +
    'tokens=2'#10't=0,A'#10't=1,B'#10'states=2'#10 +
    'q=0,1,1,0,B,B,E0'#10'q=1,1,0,1,B,T0,E1'#10'end'#10;

var Checks: Integer;

procedure Check(const Condition: Boolean; const MessageText: String);
begin
  Inc(Checks);
  if not Condition then raise Exception.Create(MessageText);
end;

function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(Values));
  for I := 0 to High(Values) do Result[I] := Values[I];
end;

function Position(const X: Integer): TGraphPosition;
begin Result.X := X; Result.Y := 0; Result.Z := 0; end;

function Change(const Text, OldText, NewText: String): String;
begin
  Check(Pos(OldText, Text) > 0, 'mutation has an exact source target');
  Result := StringReplace(Text, OldText, NewText, []);
end;

procedure Reject(const Text: String; const MessageText: String);
var D: TWfcTrainingDocument; Failed: Boolean;
begin
  D := nil; Failed := False;
  try
    try D := DecodeWfcTrainingText(Text);
    except on E: EConvertError do Failed := True; end;
  finally D.Free; end;
  Check(Failed, MessageText);
end;

function PolicyDocument(const Quota, Connectivity: Boolean;
  const MinimumA: Integer = 3): TWfcTrainingDocument;
var D: TWfcTrainingDocument; Q: TWfcTrainingValueQuotas;
  C: TWfcTrainingConnectivities; V: TWfcTrainingConnectivityValues;
  P: TGraphPositions;
begin
  Q := nil; C := nil;
  if Quota then
  begin
    SetLength(Q, 1);
    Q[0] := MakeWfcTrainingValueQuota('count-A', Tokens(['A']), MinimumA, MinimumA);
  end;
  if Connectivity then
  begin
    SetLength(C, 1); SetLength(V, 2); SetLength(P, 1);
    V[0] := MakeWfcTrainingConnectivityValue('B', [gdEast, gdWest]);
    V[1] := MakeWfcTrainingConnectivityValue('A', [gdEast, gdWest]);
    P[0] := Position(5);
    C[0] := MakeWfcTrainingConnectivity('ring', Position(0), P, V, True);
  end;
  D := DecodeWfcTrainingText(SOURCE_TEXT);
  try Result := TWfcTrainingDocument.Create(D.CopyMetadata, D.CopyOptions,
    D.CopySamples, Q, C);
  finally D.Free; end;
end;

procedure TestExactDocumentsAndLearning;
var D, CopyD, OpenD: TWfcTrainingDocument; M, Direct: TWfcSequenceModel;
  R: TWfcPipelineModel; S: TWfcSequenceSamples; O: TWfcTrainingOptions;
  I, J: Integer; A: TWfcArtifactDocument; Command: TWfcLearnCommand;
  Output, Errors: String;
begin
  D := DecodeWfcTrainingText(SOURCE_TEXT);
  try
    Check((WFC_TRAINING_SEQUENCE_WRAP_VERSION = 1) and
      (WFC_TRAINING_SEQUENCE_WRAP_TEXT_VERSION = 5), 'explicit training capability versions');
    Check(EncodeWfcTrainingText(D) = SOURCE_TEXT, 'exact v5 bytes including empty policy sections');
    Check(WfcTrainingSignatureHex(D.Signature) = '2D848595', 'version-five extraction fingerprint golden');
    Check(WfcTrainingDocumentTextVersion(D) = 5, 'wrapped sequence chooses v5');
    Check((D.ValueQuotaVersion = 0) and (D.ConnectivityVersion = 0), 'empty registries have no capability');
    CopyD := TWfcTrainingDocument.Create(D.CopyMetadata, D.CopyOptions, D.CopySamples);
    try Check((EncodeWfcTrainingText(CopyD) = SOURCE_TEXT) and
      (CopyD.Signature = D.Signature), 'typed construction has exact source identity');
    finally CopyD.Free; end;
    Check(LearnWfcTrainingModelText(D) = MODEL_TEXT, 'source trains observed circular ngrams directly');
    M := DecodeWfcSequenceText(MODEL_TEXT);
    try
      Check((M.Boundary = wmbWrap) and (M.ObservationCount = 2) and
        (M.StateCount = 2), 'wrapped model retains source observation cardinality');
      for I := 0 to M.StateCount - 1 do
      begin
        Check((M.StartCountAt(I) = 0) and (M.EndCountAt(I) = 0), 'circular positions are not open endpoints');
        for J := 0 to M.HistorySize - 1 do
          Check(M.HistoryItemAt(I, J).Kind = wshToken, 'circular history contains no BOS');
      end;
      SetLength(S, 1); S[0] := MakeWfcSequenceSample(Tokens(['A', 'B']));
      Direct := LearnSequenceModelCorpus(S, 3, wmbWrap);
      try Check(EncodeWfcSequenceText(Direct) = MODEL_TEXT,
        'training dispatcher matches direct circular learner bytes');
      finally Direct.Free; end;
    finally M.Free; end;
    R := LearnWfcTrainingRecipe(D);
    try
      Check(R.WrapNeighbors and (R.PassAt(0).SequenceExtent = wseWrap),
        'recipe topology and latent extent agree');
      Check((R.ResourceAt(0).Document = MODEL_TEXT) and
        (Pos('wfclearn-v5/', R.ResourceAt(0).SourceFingerprint) = 1),
        'embedded resource carries new source provenance');
      Command.Kind := wlckLearn; Command.InputPath := '-'; Command.OutputMode := wlomRecipe;
      Check((WfcLearnExecuteText(Command, SOURCE_TEXT, Output, Errors) = 0) and
        (Errors = '') and (Output = EncodeWfcPipelineModelText(R)), 'portable CLI emits exact circular recipe');
      Command.OutputMode := wlomQuiet;
      Check((WfcLearnExecuteText(Command, SOURCE_TEXT, Output, Errors) = 0) and
        (Output = '') and (Errors = ''), 'quiet still accepts and compiles circular recipe');
    finally R.Free; end;
    Command.OutputMode := wlomModel;
    Check((WfcLearnExecuteText(Command, SOURCE_TEXT, Output, Errors) = 0) and
      (Output = MODEL_TEXT) and (Errors = ''), 'portable CLI model output is wfcs2');
    A := TWfcArtifactDocument.Create(wakTraining, SOURCE_TEXT, '', '');
    try Check((A.CanonicalText = SOURCE_TEXT) and
      (Pos('execution=not-run'#10, WfcInspectArtifact(A)) > 0),
      'artifact inspection accepts source without executing it');
    finally A.Free; end;
    A := TWfcArtifactDocument.Create(wakSequence, MODEL_TEXT, '', '');
    try Check((A.Sequence.Boundary = wmbWrap) and
      (Pos('boundary=wrap', WfcInspectArtifact(A)) > 0), 'artifact inspector exposes circular learned boundary');
    finally A.Free; end;
    OpenD := DecodeWfcTrainingText(OPEN_SOURCE_TEXT);
    try
      Check((EncodeWfcTrainingText(OpenD) = OPEN_SOURCE_TEXT) and
        (LearnWfcTrainingModelText(OpenD) = OPEN_MODEL_TEXT), 'legacy open source/model bytes stay identical');
      Check(OpenD.Signature <> D.Signature, 'circular extraction has distinct provenance');
      O := OpenD.CopyOptions; O.Boundary := wmbWrap;
      CopyD := TWfcTrainingDocument.Create(OpenD.CopyMetadata, O, OpenD.CopySamples);
      try Check((CopyD.Signature = D.Signature) and
        (EncodeWfcTrainingText(CopyD) = SOURCE_TEXT), 'boundary alone selects the explicit new capability');
      finally CopyD.Free; end;
    finally OpenD.Free; end;
    WriteLn('Circular source fingerprint: ', WfcTrainingSignatureHex(D.Signature));
  finally D.Free; end;
end;

procedure TestCorpusAndCapacityPreflight;
var D, CopyD: TWfcTrainingDocument; M, Direct: TWfcSequenceModel;
  O: TWfcTrainingOptions; S: TWfcTrainingSamples; DS: TWfcSequenceSamples;
  V: TWfcModelTokens; I, J, Order: Integer; Failed: Boolean;
begin
  D := DecodeWfcTrainingText(SOURCE_TEXT);
  try
    O := D.CopyOptions;
    SetLength(S, 3); SetLength(DS, 3);
    S[0] := MakeWfcTrainingSample('left', 2, 1, Tokens(['A', 'B']));
    S[1] := MakeWfcTrainingSample('right', 2, 1, Tokens(['C', 'D']));
    S[2] := MakeWfcTrainingSample('repeat', 2, 1, Tokens(['A', 'B']));
    for I := 0 to 2 do DS[I] := MakeWfcSequenceSample(S[I].Tokens);
    for Order := 1 to 6 do
    begin
      O.Order := Order;
      CopyD := TWfcTrainingDocument.Create(D.CopyMetadata, O, S);
      try
        Direct := LearnSequenceModelCorpus(DS, Order, wmbWrap);
        try
          Check(LearnWfcTrainingModelText(CopyD) = EncodeWfcSequenceText(Direct),
            'capacity preflight and direct extraction agree for order ' + IntToStr(Order));
          Check((Direct.ObservationCount = 6) and (Direct.StateCount = 4) and
            (Direct.StateObservationCountAt(0) = 2) and
            (Direct.StateObservationCountAt(1) = 2) and
            (Direct.StateObservationCountAt(2) = 1), 'duplicate samples count once per original position');
          if Order > 1 then Check(not Direct.StatesCompatible(1, 2), 'history never crosses samples');
        finally Direct.Free; end;
      finally CopyD.Free; end;
    end;
    SetLength(S, 1); S[0] := MakeWfcTrainingSample('singleton', 1, 1, Tokens(['A']));
    O.Order := WFC_TRAINING_MAX_ORDER;
    CopyD := TWfcTrainingDocument.Create(D.CopyMetadata, O, S);
    try
      M := DecodeWfcSequenceText(LearnWfcTrainingModelText(CopyD));
      try
        Check((M.StateCount = 1) and (M.ObservationCount = 1) and
          (M.HistorySize = WFC_TRAINING_MAX_ORDER - 1), 'short circular sample supports full allowed order');
        for I := 0 to M.HistorySize - 1 do
          Check((M.HistoryItemAt(0, I).Kind = wshToken) and
            (M.HistoryItemAt(0, I).TokenIndex = 0), 'singleton history repeats its one token exactly');
      finally M.Free; end;
    finally CopyD.Free; end;
    { There are 32*33 circular bigrams but only 32*33/2 input samples and
      33 distinct public tokens. Open preflight would merge all first states,
      underestimating the circular state count. }
    SetLength(S, 33 * 32 div 2); I := 0;
    for Order := 0 to 31 do
      for J := Order + 1 to 32 do
      begin
        S[I] := MakeWfcTrainingSample(TWfcModelToken('pair' + IntToStr(I)), 2, 1,
          Tokens([TWfcModelToken('v' + IntToStr(Order)), TWfcModelToken('v' + IntToStr(J))]));
        Inc(I);
      end;
    O.Order := 2; CopyD := nil; Failed := False;
    try
      try CopyD := TWfcTrainingDocument.Create(D.CopyMetadata, O, S);
      except on E: EWfcTraining do Failed := Pos('sequence state count', E.Message) > 0; end;
    finally CopyD.Free; end;
    Check(Failed, 'circular capacity is rejected by document preflight before learning');
    SetLength(V, WFC_SEQUENCE_MAX_PUBLIC_TOKEN_COUNT + 1);
    for I := 0 to High(V) do V[I] := TWfcModelToken('v' + IntToStr(I));
    SetLength(S, 1); S[0] := MakeWfcTrainingSample('vocabulary', Length(V), 1, V);
    CopyD := nil; Failed := False;
    try
      try CopyD := TWfcTrainingDocument.Create(D.CopyMetadata, O, S);
      except on E: EWfcTraining do Failed := Pos('vocabulary', E.Message) > 0; end;
    finally CopyD.Free; end;
    Check(Failed, 'public vocabulary bound still applies to circular training');
  finally D.Free; end;
end;

procedure CheckRecipeReplay(const D: TWfcTrainingDocument; const Width: Integer;
  const ConflictingLock, ExpectSolved: Boolean);
var R, R2: TWfcPipelineModel; Run, Run2: TWfcPipelineRun;
  ResultDoc, Replayed, Parsed: TWfcPipelineResult; Locks: TWfcPipelineCellLocks;
  A: TWfcArtifactDocument; P, I, FailedEntry, CountA: Integer;
  RecipeText, RunText, ResultText: String; Layer: TWfcPipelineResultLayer;
begin
  R := LearnWfcTrainingRecipe(D);
  try
    P := R.FindPass('output'); SetLength(Locks, 1);
    Locks[0] := MakeWfcPipelineCellLock(P, 0, 0, 0, 'A');
    if ConflictingLock then
    begin
      SetLength(Locks, 2);
      Locks[1] := MakeWfcPipelineCellLock(P, Width - 1, 0, 0, 'A');
    end;
    Run := TWfcPipelineRun.Create(R, Width, 1, 1, High(Cardinal),
      wpssOneWay, 256, 0, True, Locks, nil);
    try
      ResultDoc := ExecuteWfcPipeline(R, Run);
      try
        if ExpectSolved then
        begin
          Check(ResultDoc.Status = wprsSolved, 'circular recipe solves the expected ring');
          Layer := ResultDoc.LayerAt(0); CountA := 0;
          Check(Length(Layer.Tokens) = Width, 'one public token per output cell');
          for I := 0 to Width - 1 do
          begin
            if Layer.Tokens[I] = 'A' then Inc(CountA);
            if I mod 2 = 0 then Check(Layer.Tokens[I] = 'A', 'locked phase at even cell')
            else Check(Layer.Tokens[I] = 'B', 'circular learned support at odd cell');
            Check(Layer.Tokens[I] <> Layer.Tokens[(I + 1) mod Width], 'closing seam participates in validation');
          end;
          if D.ValueQuotaCount <> 0 then Check(CountA = D.ValueQuotaAt(0).MinimumCount,
            'authored public quota constrains the circular latent search');
          if D.ConnectivityCount <> 0 then Check(ValidateWfcPipelineConnectivity(R, 0,
            Width, 1, 1, Layer.Tokens, FailedEntry), 'independent connectivity validates the learned ring');
        end
        else Check((ResultDoc.Status = wprsContradiction) and (ResultDoc.LayerCount = 0),
          'incompatible ring has a stored contradiction and no stale public output');
        RecipeText := EncodeWfcPipelineModelText(R);
        RunText := EncodeWfcPipelineRunText(Run);
        ResultText := EncodeWfcPipelineResultText(ResultDoc);
        R2 := DecodeWfcPipelineModelText(RecipeText);
        try
          Check(EncodeWfcPipelineModelText(R2) = RecipeText, 'recipe exact canonical round trip');
          Run2 := DecodeWfcPipelineRunText(RunText, R2);
          try
            Replayed := ExecuteWfcPipeline(R2, Run2);
            try Check(EncodeWfcPipelineResultText(Replayed) = ResultText,
              'native/browser deterministic replay preserves all result bytes');
            finally Replayed.Free; end;
            Parsed := DecodeWfcPipelineResultText(ResultText, R2, Run2);
            try Check(EncodeWfcPipelineResultText(Parsed) = ResultText,
              'stored result independently decodes with exact binding');
            finally Parsed.Free; end;
          finally Run2.Free; end;
        finally R2.Free; end;
        A := TWfcArtifactDocument.Create(wakResult, ResultText, RecipeText, RunText);
        try A.RequireReplay; Check(A.CanonicalText = ResultText,
          'artifact API explicitly replays solved and nonsolved circular results');
        finally A.Free; end;
      finally ResultDoc.Free; end;
    finally Run.Free; end;
  finally R.Free; end;
end;

procedure TestPoliciesWorkspaceAndReplay;
var Q, C: Boolean; D, CopyD: TWfcTrainingDocument; R: TWfcPipelineModel;
  Text: String; W: TWfcTrainingWorkspace; O: TWfcTrainingSolveOptions;
  L: TWfcPipelineCellLocks; Command: TWfcLearnCommand; Output, Errors: String;
begin
  for Q := False to True do
    for C := False to True do
    begin
      D := PolicyDocument(Q, C);
      try
        Text := EncodeWfcTrainingText(D);
        Check(Pos('wfclearn=5'#10, Text) = 1, 'every circular policy combination remains v5');
        CopyD := DecodeWfcTrainingText(Text);
        try Check((EncodeWfcTrainingText(CopyD) = Text) and (CopyD.Signature = D.Signature),
          'both policy registries round trip with source identity');
        finally CopyD.Free; end;
        R := LearnWfcTrainingRecipe(D);
        try Check((R.ResourceAt(0).Document = MODEL_TEXT) and
          (R.ValueQuotaCount = Ord(Q)) and (R.ConnectivityCount = Ord(C)),
          'policies retain author intent without changing learned evidence');
        finally R.Free; end;
        CheckRecipeReplay(D, 6, False, True);
        CheckRecipeReplay(D, 6, True, False);
        if Q or C then
        begin
          Command.Kind := wlckLearn; Command.InputPath := '-'; Command.OutputMode := wlomModel;
          Check((WfcLearnExecuteText(Command, Text, Output, Errors) = WFC_LEARN_EXIT_INVALID_TRAINING) and
            (Output = '') and (Errors <> ''), 'standalone CLI cannot silently discard circular output policies');
        end;
      finally D.Free; end;
    end;
  D := PolicyDocument(False, False);
  try
    CheckRecipeReplay(D, 2, False, True);
    CheckRecipeReplay(D, 1, False, False);
    CheckRecipeReplay(D, 3, False, False);
  finally D.Free; end;
  D := PolicyDocument(True, False, 4);
  try CheckRecipeReplay(D, 6, False, False);
  finally D.Free; end;
  W := TWfcTrainingWorkspace.Create;
  try
    W.SetSourceText(SOURCE_TEXT); W.Train;
    Check(W.WrapNeighbors and (W.ModelText = MODEL_TEXT), 'workspace trains the actual circular model');
    O := DefaultWfcTrainingSolveOptions; O.Width := 6; O.Height := 1;
    SetLength(L, 1); L[0] := MakeWfcPipelineCellLock(W.PublicPassIndex, 0, 0, 0, 'A');
    W.ConfigureRun(O, L, nil); W.Solve;
    Check((W.ResultStatus = wprsSolved) and (Length(W.OutputTokens) = 6), 'workspace solves circular public locks');
    W.SetSourceText(Change(SOURCE_TEXT, 'order=3', 'order=4'));
    Check(not W.HasRecipe and not W.HasRun and not W.HasResult, 'source edit invalidates every previous artifact');
    W.Train;
    Check(Pos('order=4'#10, W.ModelText) > 0, 'edited source retrains with new circular history');
  finally W.Free; end;
end;

procedure TestStrictSourceRejections;
var I: Integer; Bad, Text: String;
begin
  for I := 1 to 4 do Reject(Change(SOURCE_TEXT, 'wfclearn=5', 'wfclearn=' + IntToStr(I)),
    'circular extraction cannot be smuggled into old source version');
  Reject(Change(SOURCE_TEXT, 'wfclearn=5', 'wfclearn=6'), 'unknown future source version');
  Reject(Change(SOURCE_TEXT, 'boundary=wrap', 'boundary=open'), 'v5 cannot encode an old open source');
  Reject(Change(SOURCE_TEXT, 'kind=sequence', 'kind=adjacency1d'), 'v5 is only the circular sequence capability');
  Reject(Change(SOURCE_TEXT, 'boundary=wrap', 'boundary=circular'), 'boundary aliases rejected');
  Reject(Change(SOURCE_TEXT, 'symmetry=none', 'symmetry=d4'), 'sequence symmetry still none');
  Reject(Change(SOURCE_TEXT, 'footprint=0,0', 'footprint=1,1'), 'sequence footprint remains empty');
  Reject(Change(SOURCE_TEXT, 'sample=0,2,1,1,loop', 'sample=0,2,1,loop'), 'v5 sample explicitly carries depth');
  Reject(Change(SOURCE_TEXT, 'sample=0,2,1,1,loop', 'sample=0,2,1,2,loop'), 'v5 nonvolume depth is one');
  Reject(Change(SOURCE_TEXT, 'sample=0,2,1,1,loop', 'sample=0,1,2,1,loop'), 'sequence height remains one');
  Reject(Change(SOURCE_TEXT, 'value-quota-version=0', 'value-quota-version=1'), 'empty quota capability must be zero');
  Reject(Change(SOURCE_TEXT, 'connectivity-version=0', 'connectivity-version=1'), 'empty connectivity capability must be zero');
  Reject(Change(SOURCE_TEXT, 'value-quota-version=0', 'value-quota-version=2'), 'unknown quota capability');
  Reject(Change(SOURCE_TEXT, 'connectivity-version=0', 'connectivity-version=2'), 'unknown connectivity capability');
  Reject(Change(SOURCE_TEXT, 'value-quota-version=0'#10'value-quotas=0'#10, ''), 'v5 cannot omit empty quota section');
  Reject(Change(SOURCE_TEXT, 'connectivity-version=0'#10'connectivities=0'#10, ''), 'v5 cannot omit empty connectivity section');
  Reject(Change(SOURCE_TEXT, 'connectivities=0', 'connectivities=4097'), 'connectivity count bound before allocation');
  Reject(Change(SOURCE_TEXT, 'samples=1', 'samples=4097'), 'sample count bound before allocation');
  Reject(Change(SOURCE_TEXT, 'token=0,0,A', 'token=0,0,%41'), 'unnecessary escape rejected');
  Reject(Change(SOURCE_TEXT, 'token=0,0,A', 'token=0,0,%ED%A0%80'), 'malformed UTF-8 token rejected');
  Reject(StringReplace(SOURCE_TEXT, #10, #13#10, [rfReplaceAll]), 'source LF remains canonical');
  Reject(SOURCE_TEXT + #10, 'trailing LF rejected');
  for I := 0 to Length(SOURCE_TEXT) - 1 do Reject(Copy(SOURCE_TEXT, 1, I), 'every truncated source fails closed');
  for I := 0 to 10 do
  begin
    case I of
      0: Bad := '0'; 1: Bad := '65'; 2: Bad := '-1'; 3: Bad := '+3';
      4: Bad := '03'; 5: Bad := '3.0'; 6: Bad := '3e0'; 7: Bad := 'NaN';
      8: Bad := 'Infinity'; 9: Bad := '2147483648'; 10: Bad := ' 3';
    end;
    Reject(Change(SOURCE_TEXT, 'order=3', 'order=' + Bad), 'order is a bounded exact canonical integer');
  end;
  Text := 'wfclearn=5'#10 + StringOfChar(#10, WFC_TRAINING_SEQUENCE_WRAP_MAX_TEXT_LINE_COUNT);
  Reject(Text, 'v5 bounded line envelope checked before parsing');
  Reject('wfclearn=5'#10 + StringOfChar('x', WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH), 'whole source byte cap');
end;

{$IFDEF PAS2JS}
procedure TestHostileTypedNumbers;
var I, Field: Integer; D, CopyD: TWfcTrainingDocument;
  O: TWfcTrainingOptions; S: TWfcTrainingSamples; Bad: Integer; Failed: Boolean;
begin
  D := DecodeWfcTrainingText(SOURCE_TEXT);
  try
    for Field := 0 to 2 do
      for I := 0 to 6 do
      begin
        case I of
          0: asm Bad = 1.5; end;
          1: asm Bad = NaN; end;
          2: asm Bad = Infinity; end;
          3: asm Bad = '3'; end;
          4: asm Bad = null; end;
          5: asm Bad = true; end;
          6: asm Bad = undefined; end;
        end;
        O := D.CopyOptions; S := D.CopySamples;
        case Field of
          0: O.Order := Bad;
          1: S[0].Width := Bad;
          2: S[0].Height := Bad;
        end;
        CopyD := nil; Failed := False;
        try
          try CopyD := TWfcTrainingDocument.Create(D.CopyMetadata, O, S);
          except on E: EWfcTraining do Failed := True; end;
        finally CopyD.Free; end;
        Check(Failed, 'untrusted JS typed circular dimensions/order fail before preflight extraction');
      end;
  finally D.Free; end;
end;
{$ENDIF}

begin
  try
    TestExactDocumentsAndLearning;
    TestCorpusAndCapacityPreflight;
    TestPoliciesWorkspaceAndReplay;
    TestStrictSourceRejections;
    {$IFDEF PAS2JS}TestHostileTypedNumbers;{$ENDIF}
    WriteLn('Circular training checks: ', Checks);
  except
    on E: Exception do
    begin WriteLn('FAIL: ', E.ClassName, ': ', E.Message); Halt(1); end;
  end;
end.
