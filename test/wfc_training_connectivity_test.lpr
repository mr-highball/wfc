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
program wfc_training_connectivity_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_training, wfc_training_text,
  wfc_training_workspace, wfc_pipeline_model, wfc_pipeline_text,
  wfc_pipeline_run, wfc_pipeline_run_text, wfc_pipeline_result,
  wfc_pipeline_result_text, wfc_pipeline_runtime, wfc_pipeline_compile,
  wfc_pipeline_connectivity;

type
  TTestProcedure = procedure;
  TInputs = record
    Metadata: TWfcTrainingMetadata;
    Options: TWfcTrainingOptions;
    Samples: TWfcTrainingSamples;
    Quotas: TWfcTrainingValueQuotas;
    Connections: TWfcTrainingConnectivities;
    Width, Height, Depth: Integer;
  end;

var Checks, Failures: Integer;

procedure Check(const Condition: Boolean; const MessageText: String);
begin
  Inc(Checks);
  if Condition then Exit;
  Inc(Failures); WriteLn('[FAIL] ', MessageText);
end;

procedure RunTest(const Name: String; const Test: TTestProcedure);
begin
  WriteLn('[TEST] ', Name);
  try Test;
  except on E: Exception do
    begin Inc(Failures); WriteLn('[EXCEPTION] ', E.ClassName, ': ', E.Message); end;
  end;
end;

function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(Values));
  for I := 0 to High(Values) do Result[I] := Values[I];
end;

function Position(const X: TGraphCoordinate; const Y: TGraphCoordinate = 0;
  const Z: TGraphCoordinate = 0): TGraphPosition;
begin Result.X := X; Result.Y := Y; Result.Z := Z; end;

function NoteToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}Result := Chr($266B);
  {$ELSE}Result := UTF8Encode(UnicodeString(WideChar($266B)));{$ENDIF}
end;

function BadToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}Result := Chr($D800);
  {$ELSE}SetLength(Result, 1); Result[1] := AnsiChar($FF);{$ENDIF}
end;

function Inputs(const Kind: TWfcTrainingKind = wtkAdjacency1D): TInputs;
var Profiles: TWfcTrainingConnectivityValues; Terminals: TGraphPositions;
begin
  Result := Default(TInputs);
  Result.Metadata := MakeWfcTrainingMetadata('authored-routes', 'MIT', 'explicit public port policy');
  Result.Options := MakeWfcTrainingOptions(Kind, wmbWrap, wmsNone, 0, 0, 0);
  Result.Width := 4; Result.Height := 1; Result.Depth := 1;
  SetLength(Result.Samples, 1);
  case Kind of
    wtkAdjacency1D, wtkSequence:
      Result.Samples[0] := MakeWfcTrainingSample('strip', 4, 1,
        Tokens(['road', 'void', 'road', 'void']));
    wtkAdjacency2D, wtkPattern2D:
      begin
        Result.Width := 2; Result.Height := 2;
        Result.Samples[0] := MakeWfcTrainingSample('square', 2, 2,
          Tokens(['road', 'void', 'void', 'road']));
      end;
    wtkAdjacency3D:
      begin
        Result.Width := 2; Result.Height := 2; Result.Depth := 2;
        Result.Samples[0] := MakeWfcTrainingSample('volume', 2, 2, 2,
          Tokens(['road', 'void', 'void', 'road', 'void', 'road', 'road', 'void']));
      end;
  end;
  if Kind = wtkPattern2D then
  begin Result.Options.PatternWidth := 1; Result.Options.PatternHeight := 1; end;
  if Kind = wtkSequence then
  begin Result.Options.Boundary := wmbOpen; Result.Options.Order := 2; end;
  SetLength(Profiles, 2);
  Profiles[0] := MakeWfcTrainingConnectivityValue('void',
    [gdNorth, gdEast, gdSouth, gdWest, gdUp, gdDown], True);
  Profiles[1] := MakeWfcTrainingConnectivityValue('road',
    [gdNorth, gdEast, gdSouth, gdWest, gdUp, gdDown]);
  SetLength(Terminals, 1);
  Terminals[0] := Position(Result.Width - 1, Result.Height - 1, Result.Depth - 1);
  SetLength(Result.Connections, 1);
  Result.Connections[0] := MakeWfcTrainingConnectivity('route', Position(0), Terminals, Profiles, True);
  SetLength(Result.Quotas, 1);
  Result.Quotas[0] := MakeWfcTrainingValueQuota('route', Tokens(['void', 'road']),
    Result.Width * Result.Height * Result.Depth, Result.Width * Result.Height * Result.Depth);
end;

function RouteInputs(const Kind: TWfcTrainingKind = wtkAdjacency1D): TInputs;
begin
  Result := Inputs(Kind); Result.Width := 3;
  Result.Options.Boundary := wmbOpen;
  if Kind = wtkSequence then
  begin
    SetLength(Result.Samples, 2);
    Result.Samples[0] := MakeWfcTrainingSample('solid', 3, 1, Tokens(['road', 'road', 'road']));
    Result.Samples[1] := MakeWfcTrainingSample('split', 3, 1, Tokens(['road', 'void', 'road']));
  end
  else Result.Samples[0] := MakeWfcTrainingSample('possibilities', 7, 1,
    Tokens(['road', 'road', 'road', 'void', 'road', 'void', 'road']));
  SetLength(Result.Connections[0].Values, 1);
  Result.Connections[0].Values[0] := MakeWfcTrainingConnectivityValue('road', [gdEast, gdWest]);
  Result.Connections[0].RequiredPositions[0] := Position(2);
  Result.Connections[0].RequireAllParticipants := False;
  Result.Quotas := nil;
end;

function NewDocument(const A: TInputs): TWfcTrainingDocument;
begin Result := TWfcTrainingDocument.Create(A.Metadata, A.Options, A.Samples, A.Quotas, A.Connections); end;

function Source(const A: TInputs): String;
var D: TWfcTrainingDocument;
begin D := NewDocument(A); try Result := EncodeWfcTrainingText(D); finally D.Free; end; end;

function SignatureOf(const A: TInputs): Cardinal;
var D: TWfcTrainingDocument;
begin D := NewDocument(A); try Result := D.Signature; finally D.Free; end; end;

function Rejected(const A: TInputs; const Fragment: String = ''): Boolean;
var D: TWfcTrainingDocument;
begin
  Result := False; D := nil;
  try
    try D := NewDocument(A);
    except on E: EWfcTraining do Result := (Fragment = '') or (Pos(Fragment, E.Message) > 0); end;
  finally D.Free; end;
end;

procedure TestKindsLoweringAndReplay;
var
  K: TWfcTrainingKind; A: TInputs; D, CopyD, Legacy: TWfcTrainingDocument;
  R, CopyR, OldR: TWfcPipelineModel; Run, CopyRun: TWfcPipelineRun;
  Output, Replay, Parsed: TWfcPipelineResult; C: TWfcPipelineConnectivity;
  Graph: TWfcCompiledPipeline; P, FailedEntry: Integer;
  OldModel, Text, ResultText: String; RejectedModel: Boolean;
begin
  for K := Low(TWfcTrainingKind) to High(TWfcTrainingKind) do
  begin
    A := Inputs(K); D := NewDocument(A);
    try
      Legacy := TWfcTrainingDocument.Create(A.Metadata, A.Options, A.Samples);
      try
        OldModel := LearnWfcTrainingModelText(Legacy);
        OldR := LearnWfcTrainingRecipe(Legacy);
        try Check((OldR.ConnectivityCount = 0) and (OldR.ValueQuotaCount = 0),
          'old constructor retains both empty registries'); finally OldR.Free; end;
      finally Legacy.Free; end;
      Check((D.ConnectivityCount = 1) and (D.ConnectivityVersion = 1) and
        (D.ValueQuotaCount = 1), 'all training kinds own explicit connectivity plus quotas');
      Text := EncodeWfcTrainingText(D);
      Check((Pos('wfclearn=4'#10, Text) = 1) and (WfcTrainingDocumentTextVersion(D) = 4),
        'only connectivity-bearing source selects v4');
      CopyD := DecodeWfcTrainingText(Text);
      try Check((CopyD.Signature = D.Signature) and (EncodeWfcTrainingText(CopyD) = Text),
        'v4 authored source has exact round-trip identity'); finally CopyD.Free; end;
      R := LearnWfcTrainingRecipe(D);
      try
        Check(R.ResourceAt(0).Document = OldModel, 'author policy does not alter learned resource payload bytes');
        Check(Pos('wfclearn-v4/', R.ResourceAt(0).SourceFingerprint) = 1,
          'learned resource provenance binds v4 author intent');
        C := R.ConnectivityAt(0); P := R.FindPass('output');
        Check((C.PassIndex = P) and (R.PassAt(P).Visibility = wppvPublic),
          'connectivity binds only the current public output');
        if K in [wtkSequence, wtkPattern2D] then Check(P = 1, 'latent learners resolve output pass one')
        else Check(P = 0, 'direct learners resolve output pass zero');
        Check((C.Values[0].Value = 'road') and (C.Values[1].Value = 'void') and
          (not C.Values[0].RequiredByValue) and C.Values[1].RequiredByValue,
          'profile order follows learned vocabulary without swapping semantic fields');
        Check((D.ConnectivityAt(0).Values[0].Value = 'void') and
          (D.ConnectivityAt(0).Values[1].Value = 'road'), 'document retains explicit authored order');
        Check((R.ValueQuotaAt(0).LabelText = C.LabelText) and (R.ValueQuotaAt(0).PassIndex = P),
          'quota and connectivity labels have independent namespaces on one output');
        Graph := CompileWfcPipeline(R, A.Width, A.Height, A.Depth);
        try
          Check(Length(Graph.Graph.PassGraph[P].CopyConnectivityConstraints) = 1,
            'public output receives a real core connectivity constraint');
          if P = 1 then Check(Length(Graph.Graph.PassGraph[0].CopyConnectivityConstraints) = 1,
            'public projected connectivity also constrains the private search');
        finally Graph.Free; end;
        Run := TWfcPipelineRun.Create(R, A.Width, A.Height, A.Depth, 17,
          wpssOneWay, 256, 0, True, nil, nil);
        try
          Output := ExecuteWfcPipeline(R, Run);
          try
            Check((Output.Status = wprsSolved) and (Output.LayerCount = 1),
              'all five authored learner recipes solve with conjunctive policies');
            Check(ValidateWfcPipelineConnectivity(R, 0, A.Width, A.Height, A.Depth,
              Output.LayerAt(0).Tokens, FailedEntry), 'independent public traversal validates emitted output');
            ResultText := EncodeWfcPipelineResultText(Output);
            CopyR := DecodeWfcPipelineModelText(EncodeWfcPipelineModelText(R));
            try
              Check((CopyR.Signature = R.Signature) and (Pos('wfcpipeline=3'#10,
                EncodeWfcPipelineModelText(CopyR)) = 1), 'portable recipe preserves full policies');
              CopyRun := DecodeWfcPipelineRunText(EncodeWfcPipelineRunText(Run), CopyR);
              try
                Replay := ExecuteWfcPipeline(CopyR, CopyRun);
                try Check(EncodeWfcPipelineResultText(Replay) = ResultText,
                  'saved recipe/run replay exact deterministic public output'); finally Replay.Free; end;
                Parsed := DecodeWfcPipelineResultText(ResultText, CopyR, CopyRun);
                try Check(EncodeWfcPipelineResultText(Parsed) = ResultText,
                  'independent result decoding retains exact canonical output'); finally Parsed.Free; end;
              finally CopyRun.Free; end;
            finally CopyR.Free; end;
          finally Output.Free; end;
        finally Run.Free; end;
      finally R.Free; end;
      RejectedModel := False;
      try LearnWfcTrainingModelText(D);
      except on E: EWfcTraining do RejectedModel := Pos('connectivity', E.Message) > 0; end;
      Check(RejectedModel, 'standalone export cannot silently lose connectivity');
    finally D.Free; end;
  end;
end;

procedure TestOwnershipIdentityAndOrdering;
var A: TInputs; D, D2, Legacy: TWfcTrainingDocument; R: TWfcPipelineModel;
  C: TWfcTrainingConnectivity; CopyC: TWfcTrainingConnectivities;
  Profiles: TWfcTrainingConnectivityValues; Terminals: TGraphPositions;
  Baseline: Cardinal; I: Integer; Failed: Boolean; Temp: TWfcTrainingConnectivityValue;
begin
  A := Inputs(wtkAdjacency3D); A.Connections[0].LabelText := NoteToken;
  D := NewDocument(A);
  try
    Baseline := D.Signature;
    WriteLn('[GOLDEN] source=', WfcTrainingSignatureHex(Baseline));
    Check(WfcTrainingSignatureHex(Baseline) = 'C9A25A29', 'authored connectivity source identity golden');
    A.Connections[0].Root.X := 100;
    A.Connections[0].RequiredPositions[0].X := 100;
    A.Connections[0].Values[0].Value := 'changed';
    Exclude(A.Connections[0].Values[1].Openings, gdUp);
    A.Connections[0].Values[1].RequiredByValue := True;
    C := D.ConnectivityAt(0);
    Check((C.Root.X = 0) and (C.RequiredPositions[0].X = 1) and
      (C.Values[0].Value = 'void') and (gdUp in C.Values[1].Openings) and
      not C.Values[1].RequiredByValue, 'construction detaches coordinates, profiles, flags and port sets');
    C.Values[0].Value := 'accessor'; C.Root.Z := 8;
    C.RequiredPositions[0].Z := 8; Exclude(C.Values[1].Openings, gdEast);
    CopyC := D.CopyConnectivities; CopyC[0].Values[1].Value := 'copy';
    CopyC[0].RequiredPositions[0].Y := 9; Exclude(CopyC[0].Values[0].Openings, gdSouth);
    C := D.ConnectivityAt(0);
    Check((C.Root.Z = 0) and (C.RequiredPositions[0].Z = 1) and
      (C.RequiredPositions[0].Y = 1) and (C.Values[0].Value = 'void') and
      (C.Values[1].Value = 'road') and (gdEast in C.Values[1].Openings) and
      (gdSouth in C.Values[0].Openings) and (D.Signature = Baseline),
      'every dynamic accessor is detached and signature immutable');
    Profiles := C.Values; Terminals := C.RequiredPositions;
    C := MakeWfcTrainingConnectivity('maker', Position(0), Terminals, Profiles);
    Terminals[0].X := 9; Profiles[0].Value := 'caller'; Exclude(Profiles[1].Openings, gdWest);
    Check((C.RequiredPositions[0].X = 1) and (C.Values[0].Value = 'void') and
      (gdWest in C.Values[1].Openings), 'maker detaches nested arrays and port sets');
    Failed := False;
    try C := D.ConnectivityAt(-1); except on E: ERangeError do Failed := True; end;
    Check(Failed, 'negative connectivity index rejected');
    Failed := False;
    try C := D.ConnectivityAt(1); except on E: ERangeError do Failed := True; end;
    Check(Failed, 'past-end connectivity index rejected');
    A := Inputs(wtkAdjacency3D); A.Connections := D.CopyConnectivities;
    Check(SignatureOf(A) = Baseline, 'detached reconstruction reproduces v4 signature');
    for I := 0 to 12 do
    begin
      A.Connections := D.CopyConnectivities;
      case I of
        0: A.Connections[0].LabelText := 'renamed';
        1: A.Connections[0].Root.X := 2;
        2: A.Connections[0].Root.Y := 2;
        3: A.Connections[0].Root.Z := 2;
        4: A.Connections[0].RequireAllParticipants := False;
        5: A.Connections[0].RequiredPositions[0].X := 2;
        6: A.Connections[0].RequiredPositions[0].Y := 2;
        7: A.Connections[0].RequiredPositions[0].Z := 2;
        8: A.Connections[0].RequiredPositions := nil;
        9: begin Temp := A.Connections[0].Values[0]; A.Connections[0].Values[0] :=
          A.Connections[0].Values[1]; A.Connections[0].Values[1] := Temp; end;
        10: Exclude(A.Connections[0].Values[0].Openings, gdDown);
        11: A.Connections[0].Values[0].RequiredByValue := False;
        12: SetLength(A.Connections[0].Values, 1);
      end;
      Check(SignatureOf(A) <> Baseline, 'each ordered descriptor field contributes to source identity');
    end;
    A.Connections := D.CopyConnectivities; SetLength(A.Connections, 2);
    A.Connections[1] := D.ConnectivityAt(0); A.Connections[1].LabelText := 'second';
    Check(SignatureOf(A) <> Baseline, 'connectivity declaration count contributes to identity');
    Baseline := SignatureOf(A); C := A.Connections[0]; A.Connections[0] := A.Connections[1];
    A.Connections[1] := C; Check(SignatureOf(A) <> Baseline, 'descriptor order contributes to identity');
  finally D.Free; end;
  A := Inputs; A.Quotas := nil; A.Connections := nil;
  Legacy := TWfcTrainingDocument.Create(A.Metadata, A.Options, A.Samples);
  try
    D := NewDocument(A);
    try Check((D.Signature = Legacy.Signature) and (D.ConnectivityVersion = 0) and
      (EncodeWfcTrainingText(D) = EncodeWfcTrainingText(Legacy)),
      'new constructor with empty registries exactly preserves legacy source'); finally D.Free; end;
  finally Legacy.Free; end;
  A := Inputs; A.Connections := nil;
  Legacy := TWfcTrainingDocument.Create(A.Metadata, A.Options, A.Samples, A.Quotas);
  try
    D := NewDocument(A);
    try Check((D.Signature = Legacy.Signature) and (D.ConnectivityVersion = 0) and
      (EncodeWfcTrainingText(D) = EncodeWfcTrainingText(Legacy)),
      'new empty connectivity overload exactly preserves quota-only v3'); finally D.Free; end;
  finally Legacy.Free; end;
  A := Inputs(wtkAdjacency3D); A.Options.Symmetry := wmsCubeFull;
  A.Connections[0].Values[0].Openings := [gdEast];
  A.Connections[0].Values[1].Openings := [gdUp, gdDown]; D2 := NewDocument(A);
  try
    R := LearnWfcTrainingRecipe(D2);
    try Check((R.ConnectivityAt(0).Values[0].Openings = [gdUp, gdDown]) and
      (R.ConnectivityAt(0).Values[1].Openings = [gdEast]),
      'cube augmentation leaves authored world-axis profile ports unchanged'); finally R.Free; end;
  finally D2.Free; end;
  A := Inputs(wtkAdjacency2D); A.Options.Symmetry := wmsD4;
  A.Connections[0].Values[0].Openings := [gdNorth]; D2 := NewDocument(A);
  try
    R := LearnWfcTrainingRecipe(D2);
    try Check(R.ConnectivityAt(0).Values[1].Openings = [gdNorth],
      'D4 augmentation does not infer rotated profile semantics'); finally R.Free; end;
  finally D2.Free; end;
  A := Inputs;
  A.Samples[0].Tokens := Tokens(['void', 'road', 'void', 'road']);
  D2 := NewDocument(A);
  try
    R := LearnWfcTrainingRecipe(D2);
    try Check((R.ConnectivityAt(0).Values[0].Value = 'void') and
      R.ConnectivityAt(0).Values[0].RequiredByValue and
      (R.ConnectivityAt(0).Values[1].Value = 'road') and
      not R.ConnectivityAt(0).Values[1].RequiredByValue and
      (D2.ConnectivityAt(0).Values[0].Value = 'void'),
      'changed first-observation vocabulary order cannot redirect authored token semantics');
    finally R.Free; end;
  finally D2.Free; end;
end;

procedure TestValidationAndCapacities;
var A: TInputs; D: TWfcTrainingDocument; I: Integer;
  Profiles: TWfcTrainingConnectivityValues; Terminals: TGraphPositions;
  C: TWfcTrainingConnectivity; Failed: Boolean;
  {$IFNDEF PAS2JS}BadByte: Byte;{$ENDIF}
begin
  for I := 0 to 16 do
  begin
    A := Inputs;
    case I of
      0: A.Connections[0].LabelText := '';
      1: A.Connections[0].LabelText := BadToken;
      2: A.Connections[0].Values := nil;
      3: A.Connections[0].Values[0].Value := '';
      4: A.Connections[0].Values[0].Value := BadToken;
      5: A.Connections[0].Values[0].Value := 'absent';
      6: A.Connections[0].Values[0].Value := 'road';
      7: begin SetLength(A.Connections, 2); A.Connections[1] := A.Connections[0]; end;
      8: A.Connections[0].Root.X := TGraphCoordinate(High(Integer)) + 1;
      9: A.Connections[0].Root.Y := 1;
      10: A.Connections[0].Root.Z := 1;
      11: A.Connections[0].RequiredPositions[0].X := TGraphCoordinate(High(Integer)) + 1;
      12: A.Connections[0].RequiredPositions[0].Y := 1;
      13: A.Connections[0].RequiredPositions[0].Z := 1;
      14: A.Connections[0].RequiredPositions[0] := Position(0);
      15: begin SetLength(A.Connections[0].RequiredPositions, 2);
        A.Connections[0].RequiredPositions[1] := Position(3); end;
      16: begin SetLength(A.Connections[0].RequiredPositions, 2);
        A.Connections[0].RequiredPositions[1] := Position(2); end;
    end;
    Check(Rejected(A), 'invalid authored descriptor rejected before learning case ' + IntToStr(I));
  end;
  A := Inputs(wtkAdjacency2D); A.Connections[0].RequiredPositions[0].Z := 1;
  Check(Rejected(A, 'rank'), 'rank-two terminal cannot imply a third axis');
  A := Inputs(wtkAdjacency3D); SetLength(A.Connections[0].RequiredPositions, 3);
  A.Connections[0].RequiredPositions[0] := Position(1000, 0, 0);
  A.Connections[0].RequiredPositions[1] := Position(0, 1, 0);
  A.Connections[0].RequiredPositions[2] := Position(0, 0, 1);
  D := NewDocument(A);
  try Check(Length(D.ConnectivityAt(0).RequiredPositions) = 3,
    'terminals use Z then Y then X order, not individual coordinate monotonicity'); finally D.Free; end;
  A := Inputs; A.Connections[0].Root.X := High(Integer);
  D := NewDocument(A);
  try Check(D.ConnectivityAt(0).Root.X = High(Integer),
    'authored anchors may exceed source extent and stay exact until output compile'); finally D.Free; end;
  A := Inputs; A.Connections[0].Values[0].Openings := [];
  D := NewDocument(A);
  try Check(D.ConnectivityAt(0).Values[0].Openings = [],
    'zero ports means an isolated participating token, not an invalid or wildcard profile'); finally D.Free; end;
  {$IFNDEF PAS2JS}
  A := Inputs; BadByte := 2; Move(BadByte, A.Connections[0].RequireAllParticipants, 1);
  Check(Rejected(A, 'Boolean'), 'native noncanonical all-participants storage rejected');
  A := Inputs; Move(BadByte, A.Connections[0].Values[0].RequiredByValue, 1);
  Check(Rejected(A, 'Boolean'), 'native noncanonical required-by-value storage rejected');
  A := Inputs; BadByte := $80; Move(BadByte, A.Connections[0].Values[0].Openings, 1);
  Check(Rejected(A, 'invalid opening'), 'native out-of-range opening bit rejected');
  {$ENDIF}
  A := Inputs; SetLength(A.Connections, WFC_TRAINING_MAX_CONNECTIVITY_COUNT + 1);
  Check(Rejected(A, 'count'), 'descriptor count preflight');
  A := Inputs; SetLength(A.Connections[0].Values, WFC_TRAINING_MAX_CONNECTIVITY_VALUE_COUNT + 1);
  Check(Rejected(A, 'profile count'), 'profile count preflight');
  A := Inputs; SetLength(A.Connections[0].RequiredPositions, WFC_TRAINING_MAX_CONNECTIVITY_TERMINAL_COUNT + 1);
  Check(Rejected(A, 'terminal count'), 'terminal count preflight');
  A := Inputs; SetLength(A.Connections, WFC_TRAINING_MAX_CONNECTIVITY_COUNT);
  for I := 0 to High(A.Connections) do
  begin
    A.Connections[I] := Inputs.Connections[0];
    A.Connections[I].LabelText := TWfcModelToken('c' + IntToStr(I));
  end;
  D := NewDocument(A);
  try Check(D.ConnectivityCount = WFC_TRAINING_MAX_CONNECTIVITY_COUNT,
    'inclusive descriptor limit'); finally D.Free; end;
  A := Inputs; SetLength(A.Connections[0].RequiredPositions, WFC_TRAINING_MAX_CONNECTIVITY_TERMINAL_COUNT);
  for I := 0 to High(A.Connections[0].RequiredPositions) do A.Connections[0].RequiredPositions[I] := Position(I + 1);
  D := NewDocument(A);
  try Check(Length(D.ConnectivityAt(0).RequiredPositions) = WFC_TRAINING_MAX_CONNECTIVITY_TERMINAL_COUNT,
    'inclusive terminal aggregate limit'); finally D.Free; end;
  SetLength(A.Connections, 2); C := Inputs.Connections[0]; C.LabelText := 'overflow'; A.Connections[1] := C;
  Check(Rejected(A, 'aggregate'), 'aggregate terminal preflight');
  A := Inputs; A.Samples[0].Width := WFC_TRAINING_MAX_CONNECTIVITY_VALUE_COUNT;
  SetLength(A.Samples[0].Tokens, A.Samples[0].Width);
  SetLength(Profiles, WFC_TRAINING_MAX_CONNECTIVITY_VALUE_COUNT);
  for I := 0 to High(Profiles) do
  begin
    A.Samples[0].Tokens[I] := TWfcModelToken('v' + IntToStr(I));
    Profiles[I] := MakeWfcTrainingConnectivityValue(A.Samples[0].Tokens[I], [gdEast, gdWest]);
  end;
  A.Quotas := nil; SetLength(A.Connections, 64);
  for I := 0 to High(A.Connections) do
  begin
    A.Connections[I] := MakeWfcTrainingConnectivity(TWfcModelToken('c' + IntToStr(I)),
      Position(0), nil, Profiles);
  end;
  D := NewDocument(A);
  try Check((D.ConnectivityCount = 64) and (Length(D.ConnectivityAt(63).Values) = 1024),
    'inclusive aggregate profile and per-descriptor profile limits'); finally D.Free; end;
  SetLength(A.Connections, 65);
  A.Connections[64] := MakeWfcTrainingConnectivity('overflow', Position(0), nil, Profiles);
  Check(Rejected(A, 'aggregate'), 'aggregate profile preflight');
  SetLength(Profiles, WFC_TRAINING_MAX_CONNECTIVITY_VALUE_COUNT + 1); Failed := False;
  try C := MakeWfcTrainingConnectivity('maker', Position(0), nil, Profiles);
  except on E: EWfcTraining do Failed := True; end;
  Check(Failed, 'maker rejects oversized profiles before cloning');
  Profiles := nil; SetLength(Terminals, WFC_TRAINING_MAX_CONNECTIVITY_TERMINAL_COUNT + 1); Failed := False;
  try C := MakeWfcTrainingConnectivity('maker', Position(0), Terminals, Profiles);
  except on E: EWfcTraining do Failed := True; end;
  Check(Failed, 'maker rejects oversized terminals before cloning');
  A := Inputs; A.Connections[0].LabelText := TWfcModelToken(StringOfChar('a',
    WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH + 1)); Check(Rejected(A, 'limit'), 'encoded label budget');
  A := Inputs; SetLength(A.Connections, 65);
  for I := 0 to High(A.Connections) do
  begin
    C := Inputs.Connections[0];
    C.LabelText := TWfcModelToken(StringOfChar('a', WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH - 3) + Format('%.3d', [I]));
    A.Connections[I] := C;
  end;
  Check(Rejected(A, 'aggregate encoded'), 'connectivity shares aggregate token bytes with the source');
end;

procedure TestPublicConnectivityAndAtomicValidation;
var A: TInputs; D: TWfcTrainingDocument; R: TWfcPipelineModel; Run: TWfcPipelineRun;
  Output, Forged: TWfcPipelineResult; Layers: TWfcPipelineResultLayers;
  G: TWfcCompiledPipeline; Report: TGraphSolveReport; Locks: TWfcPipelineCellLocks;
  I, Mode, FailedEntry: Integer; Failed: Boolean; OldValue: String;
begin
  for I := 0 to 1 do
  begin
    if I = 0 then A := RouteInputs else A := RouteInputs(wtkSequence);
    D := NewDocument(A);
    try
      R := LearnWfcTrainingRecipe(D);
      try
        Run := TWfcPipelineRun.Create(R, 3, 1, 1, 4, wpssOneWay, 128, 0, True, nil, nil);
        try
          Output := ExecuteWfcPipeline(R, Run);
          try
            Check((Output.Status = wprsSolved) and (Output.LayerAt(0).Tokens[0] = 'road') and
              (Output.LayerAt(0).Tokens[1] = 'road') and (Output.LayerAt(0).Tokens[2] = 'road'),
              'fixed endpoints force a complete public route through direct or latent search');
            Layers := Output.CopyLayers; Layers[0].Tokens[1] := 'void'; Failed := False; Forged := nil;
            try Forged := TWfcPipelineResult.Create(R, Run, Output.CopyVersions, Output.Status,
              Output.PassBacktracks, Output.EvidenceKind, Output.EvidenceSignature,
              Output.CopyFailure, Output.CopyPassOutcomes, Layers);
            except on E: EWfcPipelineResult do Failed := True; end;
            Forged.Free; Check(Failed, 'forged in-vocabulary disconnected result is independently rejected');
            Check(not ValidateWfcPipelineConnectivity(R, 0, 3, 1, 1, Layers[0].Tokens, FailedEntry),
              'standalone public traversal rejects the disconnected forged layer');
          finally Output.Free; end;
        finally Run.Free; end;
      finally R.Free; end;
    finally D.Free; end;
  end;
  A := RouteInputs; D := NewDocument(A);
  try
    R := LearnWfcTrainingRecipe(D);
    try
      G := CompileWfcPipeline(R, 3, 1, 1);
      try
        Check(G.Graph.TrySolve(DefaultGraphSolveOptions, Report), 'public guard baseline solves');
        OldValue := G.Graph.Entry[1, 0, 0].Value;
        G.Graph.ClearConnectivity;
        G.Graph.SetAllowedValues(0, 0, 0, ['road']); G.Graph.SetAllowedValues(1, 0, 0, ['void']);
        G.Graph.SetAllowedValues(2, 0, 0, ['road']);
        Check(not G.Graph.TrySolve(DefaultGraphSolveOptions, Report),
          'clearing mutable graph constraints cannot bypass immutable authored connectivity');
        Check((Report.Contradiction.Kind = gckFinalValidation) and
          (G.LastValidation.Kind = wpcvkConnectivity) and (G.LastValidation.ConnectivityIndex = 0),
          'publication failure identifies the authored connectivity descriptor');
        Check(G.Graph.Entry[1, 0, 0].Value = OldValue, 'failed publication rolls back preceding values');
      finally G.Free; end;
    finally R.Free; end;
  finally D.Free; end;
  for Mode := 0 to 2 do
  begin
    A := RouteInputs; A.Connections[0].RequiredPositions := nil;
    A.Connections[0].RequireAllParticipants := Mode = 1;
    A.Connections[0].Values[0].RequiredByValue := Mode = 2;
    D := NewDocument(A);
    try
      R := LearnWfcTrainingRecipe(D);
      try
        SetLength(Locks, 3); Locks[0] := MakeWfcPipelineCellLock(0, 0, 0, 0, 'road');
        Locks[1] := MakeWfcPipelineCellLock(0, 1, 0, 0, 'void'); Locks[2] := MakeWfcPipelineCellLock(0, 2, 0, 0, 'road');
        Run := TWfcPipelineRun.Create(R, 3, 1, 1, 1, wpssOneWay, 128, 0, True, Locks, nil);
        try
          Output := ExecuteWfcPipeline(R, Run);
          try
            if Mode = 0 then Check(Output.Status = wprsSolved, 'optional disconnected participants remain legal')
            else Check((Output.Status <> wprsSolved) and (Output.LayerCount = 0),
              'all-participants and required-by-value each reject disconnected islands');
          finally Output.Free; end;
        finally Run.Free; end;
      finally R.Free; end;
    finally D.Free; end;
  end;
end;

procedure ConfigureWorkspace(const W: TWfcTrainingWorkspace; const Width: Integer = 3);
var O: TWfcTrainingSolveOptions;
begin
  O := DefaultWfcTrainingSolveOptions; O.Width := Width; O.Height := 1;
  O.Seed := 4; O.MaxBacktracks := 128; O.CaptureTrace := True;
  W.ConfigureRun(O, nil, nil);
end;

procedure CheckNoDerived(const W: TWfcTrainingWorkspace; const Text: String);
var Failed: Boolean;
begin
  Check((W.SourceText = Text) and not W.HasRecipe and not W.HasRun and not W.HasResult,
    'failed replacement leaves only its documented editable source');
  Failed := False; try W.RecipeText; except on E: EWfcTrainingWorkspace do Failed := True; end;
  Check(Failed, 'failed authoring cannot export a stale recipe');
  Failed := False; try W.RunText; except on E: EWfcTrainingWorkspace do Failed := True; end;
  Check(Failed, 'failed authoring cannot export a stale run');
  Failed := False; try W.ResultText; except on E: EWfcTrainingWorkspace do Failed := True; end;
  Check(Failed, 'failed authoring cannot export a stale result');
end;

procedure TestWorkspacePolicyLifecycle;
var A: TInputs; W, Fresh: TWfcTrainingWorkspace; Limits: TWfcTrainingWorkspaceLimits;
  C: TWfcTrainingConnectivities; Q: TWfcTrainingValueQuotas; D: TWfcTrainingDocument;
  OldSource, OldRecipe, OldModel, CurrentSource, CurrentRecipe, ResultText, Draft: String;
  Failed: Boolean; I: Integer;
begin
  A := RouteInputs; C := A.Connections; A.Connections := nil; OldSource := Source(A);
  W := TWfcTrainingWorkspace.Create;
  try
    W.SetSourceText(OldSource); W.Train;
    OldRecipe := W.RecipeText; OldModel := W.ModelText;
    ConfigureWorkspace(W); W.Solve;
    W.ReplaceConnectivities(C);
    Check(W.HasRecipe and not W.HasRun and not W.HasResult and (W.ConnectivityCount = 1),
      'connectivity edit retrains and invalidates run/result');
    Failed := False; try W.ModelText; except on E: EWfcTrainingWorkspace do Failed := True; end;
    Check(Failed, 'workspace model-only export fails closed');
    SetLength(Q, 1); Q[0] := MakeWfcTrainingValueQuota('route', Tokens(['road']), 3, 3);
    W.ReplaceValueQuotas(Q);
    Check((W.ValueQuotaCount = 1) and (W.ConnectivityCount = 1),
      'editing quotas preserves saved connectivity');
    C := W.CopyConnectivities; C[0].RequireAllParticipants := True; W.ReplaceConnectivities(C);
    Check((W.ValueQuotaCount = 1) and (W.CopyValueQuotas[0].MinimumCount = 3),
      'editing connectivity preserves exact saved quota bounds');
    CurrentSource := W.SourceText; CurrentRecipe := W.RecipeText;
    C[0].Values[0].Value := 'caller'; C := W.CopyConnectivities;
    C[0].RequiredPositions[0].X := 999; Exclude(C[0].Values[0].Openings, gdEast);
    Check((W.SourceText = CurrentSource) and (W.CopyConnectivities[0].RequiredPositions[0].X = 2) and
      (gdEast in W.CopyConnectivities[0].Values[0].Openings), 'workspace policy copies are deeply detached');
    ConfigureWorkspace(W); W.Solve; ResultText := W.ResultText;
    Check((W.ResultStatus = wprsSolved) and (W.OutputTokens[1] = 'road'),
      'combined authored quantities and connectivity solve the public route');
    Fresh := TWfcTrainingWorkspace.Create;
    try
      Fresh.SetSourceText(CurrentSource); Fresh.Train; ConfigureWorkspace(Fresh); Fresh.Solve;
      Check((Fresh.RecipeText = CurrentRecipe) and (Fresh.ResultText = ResultText) and
        (Fresh.ConnectivityCount = 1) and (Fresh.ValueQuotaCount = 1),
        'source save/import/retrain replays both explicit policies exactly');
    finally Fresh.Free; end;
    W.Train; Check((W.SourceText = CurrentSource) and (W.RecipeText = CurrentRecipe),
      'ordinary retraining cannot drop authored connectivity');
    ConfigureWorkspace(W, 2); Failed := False;
    try W.Solve; except on E: EWfcPipelineCompile do Failed := E.Stage = wpcsConnectivity; end;
    Check(Failed and not W.HasResult and (W.CopyConnectivities[0].RequiredPositions[0].X = 2),
      'shrinking output rejects out-of-bounds anchors without clamping or stale results');
    for I := 0 to 4 do
    begin
      W.SetSourceText(CurrentSource); W.Train; ConfigureWorkspace(W); W.Solve;
      C := W.CopyConnectivities;
      case I of
        0: C[0].Root.X := TGraphCoordinate(High(Integer)) + 1;
        1: C[0].Values[0].Value := 'missing';
        2: C[0].LabelText := '';
        3: C[0].RequiredPositions[0] := C[0].Root;
        4: C[0].Values := nil;
      end;
      Failed := False; try W.ReplaceConnectivities(C); except on E: EWfcTraining do Failed := True; end;
      Check(Failed, 'invalid connectivity edit is rejected'); CheckNoDerived(W, CurrentSource);
    end;
    W.Train; Q := W.CopyValueQuotas; Q[0].MinimumCount := -1; Failed := False;
    try W.ReplaceValueQuotas(Q); except on E: EWfcTraining do Failed := True; end;
    Check(Failed, 'invalid opposite policy still invalidates old artifacts'); CheckNoDerived(W, CurrentSource);
    W.Train; Check((W.ConnectivityCount = 1) and (W.ValueQuotaCount = 1),
      'retraining retained source recovers both policies after failed edit');
    W.ReplaceConnectivities(nil);
    Check((W.ConnectivityCount = 0) and (W.ValueQuotaCount = 1) and
      (Pos('wfclearn=3'#10, W.SourceText) = 1), 'removing final connectivity restores quota-only format');
    W.ReplaceValueQuotas(nil);
    Check((W.SourceText = OldSource) and (W.RecipeText = OldRecipe) and (W.ModelText = OldModel),
      'removing both policies restores exact original source/recipe/model bytes');
  finally W.Free; end;
  Limits := DefaultWfcTrainingWorkspaceLimits; Limits.MaxSourceTextLength := Length(OldSource) + 1;
  W := TWfcTrainingWorkspace.Create(Limits);
  try
    W.SetSourceText(OldSource); W.Train; A := RouteInputs; Failed := False;
    try W.ReplaceConnectivities(A.Connections); except on E: EWfcTrainingWorkspace do Failed := True; end;
    Check(Failed, 'workspace source-byte limit rejects an otherwise valid connectivity draft');
    CheckNoDerived(W, OldSource);
  finally W.Free; end;
  D := DecodeWfcTrainingText(CurrentSource);
  try Check((D.ConnectivityCount = 1) and (D.ValueQuotaCount = 1),
    'failed workspace edits cannot mutate previously exported canonical source'); finally D.Free; end;
  { A valid imported open-pattern draft reaches the existing recipe-export
    restriction only during training. This exercises published-draft failure,
    not a fabricated later learner failure during same-payload policy edits. }
  A := Inputs(wtkPattern2D); A.Options.Boundary := wmbOpen; Draft := Source(A);
  W := TWfcTrainingWorkspace.Create;
  try
    W.SetSourceText(CurrentSource); W.Train; ConfigureWorkspace(W); W.Solve;
    W.SetSourceText(Draft); Failed := False;
    try W.Train; except on E: EWfcTraining do Failed := Pos('requires wrapped', E.Message) > 0; end;
    Check(Failed, 'valid published source can fail later at an unsupported learner recipe boundary');
    CheckNoDerived(W, Draft);
    D := DecodeWfcTrainingText(W.SourceText);
    try Check((D.ConnectivityCount = 1) and (D.ValueQuotaCount = 1),
      'later training failure retains both policies in the new canonical editable draft'); finally D.Free; end;
  finally W.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestBrowserMalformedFields;
var A: TInputs; D: TWfcTrainingDocument; C: TWfcTrainingConnectivity;
  I, J, Index: Integer; X: TGraphCoordinate; Flag: Boolean;
  Ports: TGraphDirections; Failed: Boolean;
begin
  A := Inputs(wtkAdjacency3D); D := NewDocument(A);
  try
    for I := 0 to 8 do
    begin
      case I of
        0: asm X = NaN; end; 1: asm X = Infinity; end; 2: asm X = -Infinity; end;
        3: asm X = 0.25; end; 4: asm X = undefined; end; 5: asm X = "0"; end;
        6: asm X = null; end; 7: asm X = 4294967296; end; 8: asm X = -1; end;
      end;
      for J := 0 to 5 do
      begin
        A.Connections := D.CopyConnectivities;
        case J of
          0: A.Connections[0].Root.X := X; 1: A.Connections[0].Root.Y := X;
          2: A.Connections[0].Root.Z := X; 3: A.Connections[0].RequiredPositions[0].X := X;
          4: A.Connections[0].RequiredPositions[0].Y := X; 5: A.Connections[0].RequiredPositions[0].Z := X;
        end;
        Check(Rejected(A, 'exact integer'), 'browser coordinate requires exact finite nonnegative Integer');
      end;
      asm Index = X; end;
      Failed := False; try C := D.ConnectivityAt(Index); except on E: ERangeError do Failed := True; end;
      Check(Failed, 'browser accessor rejects malformed numeric indices');
    end;
    for I := 0 to 5 do
    begin
      case I of
        0: asm Flag = 0; end; 1: asm Flag = 1; end; 2: asm Flag = "false"; end;
        3: asm Flag = undefined; end; 4: asm Flag = null; end; 5: asm Flag = NaN; end;
      end;
      A.Connections := D.CopyConnectivities; A.Connections[0].RequireAllParticipants := Flag;
      Check(Rejected(A, 'Boolean'), 'browser all-participants requires a real Boolean');
      A.Connections := D.CopyConnectivities; A.Connections[0].Values[0].RequiredByValue := Flag;
      Check(Rejected(A, 'Boolean'), 'browser required-by-value requires a real Boolean');
    end;
    A.Connections := D.CopyConnectivities; asm Ports = {6:true}; end;
    A.Connections[0].Values[0].Openings := Ports; Check(Rejected(A, 'invalid opening'), 'unknown opening ordinal rejected');
    A.Connections := D.CopyConnectivities; asm Ports = {unexpected:true}; end;
    A.Connections[0].Values[0].Openings := Ports; Check(Rejected(A, 'invalid opening'), 'nonordinal opening key rejected');
  finally D.Free; end;
end;
{$ENDIF}

begin
  RunTest('all learner kinds and canonical source/recipe/run/result replay', @TestKindsLoweringAndReplay);
  RunTest('deep ownership, source identity and profile ordering', @TestOwnershipIdentityAndOrdering);
  RunTest('strict authored descriptors and finite resource envelopes', @TestValidationAndCapacities);
  RunTest('connectivity affects search and independent publication', @TestPublicConnectivityAndAtomicValidation);
  RunTest('workspace cross-policy edits and stale-output invalidation', @TestWorkspacePolicyLifecycle);
  {$IFDEF PAS2JS}RunTest('strict browser numbers flags and direction sets', @TestBrowserMalformedFields);{$ENDIF}
  WriteLn('[SUMMARY] checks=', Checks, ' failures=', Failures);
  if Failures <> 0 then Halt(1);
end.
