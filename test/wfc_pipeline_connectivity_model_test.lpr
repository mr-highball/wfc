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
program wfc_pipeline_connectivity_model_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_rule_model, wfc_rule_text,
  wfc_pattern2d, wfc_pattern2d_learn, wfc_pattern2d_text,
  wfc_sequence, wfc_sequence_learn, wfc_sequence_text,
  wfc_pipeline_model;

type
  TTestProcedure = procedure;
  TInputs = record
    Metadata: TWfcPipelineMetadata;
    Resources: TWfcPipelineResources;
    Passes: TWfcPipelinePasses;
    Dependencies: TWfcPipelineDependencies;
    Bridges: TWfcPipelineBridges;
    Quotas: TWfcPipelineValueQuotas;
    Connectivities: TWfcPipelineConnectivities;
    Rank: Integer;
    Wrap: Boolean;
  end;
var
  Checks, Failures: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if ACondition then Exit;
  Inc(Failures);
  WriteLn('[FAIL] ', AMessage);
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try ATest;
  except on E: Exception do
    begin Inc(Failures); WriteLn('[EXCEPTION] ', E.ClassName, ': ', E.Message); end;
  end;
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

function Position(const X: TGraphCoordinate; const Y: TGraphCoordinate = 0;
  const Z: TGraphCoordinate = 0): TGraphPosition;
begin Result.X := X; Result.Y := Y; Result.Z := Z; end;

function BasicConnectivity(const APass: Integer = 0): TWfcPipelineConnectivity;
var Profiles: TWfcPipelineConnectivityValues; Terminals: TGraphPositions;
begin
  SetLength(Profiles, 2);
  Profiles[0] := MakeWfcPipelineConnectivityValue('z', [gdEast, gdWest]);
  Profiles[1] := MakeWfcPipelineConnectivityValue('a', [gdEast, gdWest], True);
  SetLength(Terminals, 1); Terminals[0] := Position(2);
  Result := MakeWfcPipelineConnectivity(APass, 'network', Position(0),
    Terminals, Profiles);
end;

procedure SetRules(var A: TInputs; const Vocabulary: TWfcModelTokens);
var R: TWfcRuleModel; Weights: TWfcModelIntegerArray; I: Integer;
begin
  SetLength(Weights, Length(Vocabulary));
  for I := 0 to High(Weights) do Weights[I] := 1;
  R := TWfcRuleModel.Create(A.Rank, Vocabulary, Weights, nil);
  try
    SetLength(A.Resources, 1);
    A.Resources[0] := MakeWfcPipelineResource('rules', wprkRules,
      EncodeWfcRuleText(R), 'authored fixture', 'MIT', '');
  finally R.Free; end;
end;

function BuildInputs(const ARank: Integer = 1): TInputs;
begin
  Result := Default(TInputs);
  Result.Rank := ARank;
  Result.Metadata := MakeWfcPipelineMetadata('Portable connectivity', 'MIT',
    'project-authored fixture', '');
  SetRules(Result, Tokens(['z', 'a', NoteToken]));
  SetLength(Result.Passes, 5);
  Result.Passes[0] := MakeWfcPipelinePass('source', wppvPublic,
    gpmOverlay, -1, wpakRules, 0, False, wseWhole);
  Result.Passes[1] := MakeWfcPipelinePass('alias', wppvPublic,
    gpmTransform, 0, wpakEmpty, -1, False, wseWhole);
  Result.Passes[2] := MakeWfcPipelinePass('private', wppvPrivate,
    gpmOverlay, -1, wpakRules, 0, False, wseWhole);
  Result.Passes[3] := MakeWfcPipelinePass('forward-alias', wppvPublic,
    gpmTransform, 4, wpakEmpty, -1, False, wseWhole);
  Result.Passes[4] := MakeWfcPipelinePass('later-alias', wppvPublic,
    gpmTransform, 0, wpakEmpty, -1, False, wseWhole);
  SetLength(Result.Dependencies, 3);
  Result.Dependencies[0] := MakeWfcPipelineDependency(1, 0);
  Result.Dependencies[1] := MakeWfcPipelineDependency(3, 4);
  Result.Dependencies[2] := MakeWfcPipelineDependency(4, 0);
end;

function NewRecipe(const A: TInputs; const Mode: Integer = 0): TWfcPipelineModel;
begin
  case Mode of
    1: Result := TWfcPipelineModel.Create(A.Metadata, CurrentWfcPipelineVersions,
      A.Rank, A.Wrap, rmBottomUp, A.Resources, A.Passes, A.Dependencies,
      A.Bridges, nil, A.Quotas, A.Connectivities);
    2: Result := TWfcPipelineModel.Create(A.Metadata, A.Rank, A.Wrap,
      rmBottomUp, A.Resources, A.Passes, A.Dependencies, A.Bridges, nil);
    3: Result := TWfcPipelineModel.Create(A.Metadata, CurrentWfcPipelineVersions,
      A.Rank, A.Wrap, rmBottomUp, A.Resources, A.Passes, A.Dependencies,
      A.Bridges, nil);
    4: Result := TWfcPipelineModel.Create(A.Metadata, A.Rank, A.Wrap,
      rmBottomUp, A.Resources, A.Passes, A.Dependencies, A.Bridges, nil, A.Quotas);
    5: Result := TWfcPipelineModel.Create(A.Metadata, CurrentWfcPipelineVersions,
      A.Rank, A.Wrap, rmBottomUp, A.Resources, A.Passes, A.Dependencies,
      A.Bridges, nil, A.Quotas);
  else
    Result := TWfcPipelineModel.Create(A.Metadata, A.Rank, A.Wrap,
      rmBottomUp, A.Resources, A.Passes, A.Dependencies, A.Bridges, nil,
      A.Quotas, A.Connectivities);
  end;
end;

function SignatureOf(const A: TInputs; const Mode: Integer = 0): Cardinal;
var R: TWfcPipelineModel;
begin
  R := NewRecipe(A, Mode);
  try Result := R.Signature; finally R.Free; end;
end;

function Rejected(const A: TInputs; const MessagePart: String): Boolean;
var R: TWfcPipelineModel;
begin
  R := nil; Result := False;
  try
    try R := NewRecipe(A);
    except on E: EWfcPipelineModel do
      Result := (MessagePart = '') or (Pos(MessagePart, E.Message) > 0);
    end;
  finally R.Free; end;
end;

procedure TestOwnershipAndLegacy;
var A: TInputs; R: TWfcPipelineModel; C: TWfcPipelineConnectivity;
  CopyConstraints: TWfcPipelineConnectivities; I: Integer; S: Cardinal;
  P: TGraphPositions; V: TWfcPipelineConnectivityValues; Failed: Boolean;
begin
  A := BuildInputs;
  S := SignatureOf(A);
  for I := 1 to 5 do
    Check(SignatureOf(A, I) = S, 'all legacy/empty constructor identities agree');
  R := NewRecipe(A);
  try Check((R.ConnectivityCount = 0) and (R.ConnectivityVersion = 0) and
    (Length(R.CopyConnectivities) = 0), 'empty connectivity has no capability');
  finally R.Free; end;
  SetLength(A.Quotas, 1);
  A.Quotas[0] := MakeWfcPipelineValueQuota(0, 'quota', Tokens(['a']), 1, 3);
  S := SignatureOf(A);
  Check((SignatureOf(A, 4) = S) and (SignatureOf(A, 5) = S),
    'quota-only constructor identities remain unchanged');

  C := BasicConnectivity;
  P := C.RequiredPositions; V := C.Values;
  C := MakeWfcPipelineConnectivity(0, NoteToken, Position(0), P, V, True);
  P[0].X := 99; V[0].Value := 'mutated'; Include(V[0].Openings, gdUp);
  Check((C.RequiredPositions[0].X = 2) and (C.Values[0].Value = 'z') and
    not (gdUp in C.Values[0].Openings), 'factory detaches terminals, values and port sets');
  SetLength(A.Connectivities, 1); A.Connectivities[0] := C;
  R := NewRecipe(A);
  try
    S := R.Signature;
    WriteLn('[GOLDEN] signature=', WfcPipelineSignatureHex(S));
    Check(WfcPipelineSignatureHex(S) = 'D72EB9EE',
      'combined quota/connectivity semantic signature is pinned');
    Check(SignatureOf(A, 1) = S, 'explicit and current versions retain connectivity identity');
    Check((R.ConnectivityCount = 1) and (R.ConnectivityVersion = 1),
      'nonempty connectivity opts into explicit capability');
    A.Connectivities[0].LabelText := 'mutated';
    A.Connectivities[0].RequiredPositions[0].X := 7;
    A.Connectivities[0].Values[0].Value := 'mutated';
    Include(A.Connectivities[0].Values[0].Openings, gdDown);
    C := R.ConnectivityAt(0);
    Check((C.LabelText = NoteToken) and (C.RequiredPositions[0].X = 2) and
      (C.Values[0].Value = 'z') and not (gdDown in C.Values[0].Openings),
      'constructor deeply owns declared connectivity');
    C.RequiredPositions[0].X := 8; C.Values[0].Value := 'mutated';
    Include(C.Values[0].Openings, gdUp);
    C := R.ConnectivityAt(0);
    Check((C.RequiredPositions[0].X = 2) and (C.Values[0].Value = 'z') and
      not (gdUp in C.Values[0].Openings), 'single connectivity access is detached');
    CopyConstraints := R.CopyConnectivities;
    CopyConstraints[0].RequiredPositions[0].X := 8;
    Include(CopyConstraints[0].Values[0].Openings, gdDown);
    C := R.ConnectivityAt(0);
    Check((C.RequiredPositions[0].X = 2) and not (gdDown in C.Values[0].Openings) and
      (R.Signature = S), 'whole registry copy is detached and identity immutable');
    Failed := False;
    try C := R.ConnectivityAt(-1); except on E: ERangeError do Failed := True; end;
    Check(Failed, 'negative accessor index rejected');
    Failed := False;
    try C := R.ConnectivityAt(1); except on E: ERangeError do Failed := True; end;
    Check(Failed, 'past-end accessor index rejected');
  finally R.Free; end;
end;

procedure TestIdentityFields;
var A: TInputs; Original: TWfcPipelineConnectivities; R: TWfcPipelineModel;
  S: Cardinal; I: Integer; D: TGraphDirection;
begin
  A := BuildInputs(3);
  SetLength(A.Connectivities, 2);
  A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[1] := BasicConnectivity(1);
  A.Connectivities[1].LabelText := 'second';
  R := NewRecipe(A);
  try
    S := R.Signature;
    for I := 0 to 12 do
    begin
      A.Connectivities := R.CopyConnectivities;
      case I of
        0: A.Connectivities[0].PassIndex := 3;
        1: A.Connectivities[0].LabelText := 'renamed';
        2: A.Connectivities[0].Root.X := 1;
        3: A.Connectivities[0].Root.Y := 1;
        4: A.Connectivities[0].Root.Z := 1;
        5: A.Connectivities[0].RequireAllParticipants := True;
        6: A.Connectivities[0].RequiredPositions[0].X := 3;
        7: A.Connectivities[0].RequiredPositions[0].Y := 1;
        8: A.Connectivities[0].RequiredPositions[0].Z := 1;
        9: A.Connectivities[0].Values[1].Value := NoteToken;
        10: A.Connectivities[0].Values[1].RequiredByValue := False;
        11: A.Connectivities[0].RequiredPositions := nil;
        12: SetLength(A.Connectivities[0].Values, 1);
      end;
      Check(SignatureOf(A) <> S, 'connectivity identity field ' + IntToStr(I));
    end;
    for D := Low(TGraphDirection) to High(TGraphDirection) do
    begin
      A.Connectivities := R.CopyConnectivities;
      if D in A.Connectivities[0].Values[0].Openings then
        Exclude(A.Connectivities[0].Values[0].Openings, D)
      else Include(A.Connectivities[0].Values[0].Openings, D);
      Check(SignatureOf(A) <> S, 'each of six direction bits contributes to identity');
    end;
    A.Connectivities := R.CopyConnectivities;
    SetLength(A.Connectivities, 1);
    Check(SignatureOf(A) <> S, 'registry count contributes to identity');
    A.Connectivities := R.CopyConnectivities;
    Original := R.CopyConnectivities;
    A.Connectivities[0] := Original[1]; A.Connectivities[1] := Original[0];
    Check(SignatureOf(A) <> S, 'global descriptor order contributes to identity');
  finally R.Free; end;
end;

procedure TestCanonicalValidation;
var A: TInputs; R: TWfcPipelineModel; C: TWfcPipelineConnectivity;
  N: Integer; B: Byte;
begin
  A := BuildInputs;
  SetLength(A.Connectivities, 1); A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[0].Values[0].Openings := [gdNorth, gdEast, gdSouth, gdWest, gdUp, gdDown];
  A.Connectivities[0].RequiredPositions := nil;
  R := NewRecipe(A);
  try Check(R.ConnectivityAt(0).Values[0].Openings =
    [gdNorth, gdEast, gdSouth, gdWest, gdUp, gdDown],
    'all six ports and no explicit terminals are valid on rank one');
  finally R.Free; end;
  for N := 1 to 3 do
  begin
    A := BuildInputs(N); SetLength(A.Connectivities, 1);
    A.Connectivities[0] := BasicConnectivity;
    A.Connectivities[0].Root.X := High(Integer);
    if N > 1 then A.Connectivities[0].Root.Y := High(Integer);
    if N > 2 then A.Connectivities[0].Root.Z := High(Integer);
    R := NewRecipe(A);
    try Check(R.ConnectivityAt(0).Root.X = High(Integer),
      'coordinates keep full Integer range independent of future run shape');
    finally R.Free; end;
  end;
  A := BuildInputs; SetLength(A.Connectivities, 1);
  A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[0].Root.Y := 1;
  Check(Rejected(A, 'recipe rank'), 'rank one rejects nonzero Y');
  A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[0].RequiredPositions[0].Z := 1;
  Check(Rejected(A, 'recipe rank'), 'rank one rejects terminal Z');
  A := BuildInputs(2); SetLength(A.Connectivities, 1);
  A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[0].Root.Z := 1;
  Check(Rejected(A, 'recipe rank'), 'rank two rejects nonzero Z');
  A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[0].Root.X := TGraphCoordinate(High(Integer)) + 1;
  Check(Rejected(A, 'exact integer'), 'coordinate above portable Integer range rejected');
  A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[0].RequiredPositions[0] := Position(0);
  Check(Rejected(A, 'repeats the root'), 'explicit root repetition rejected');
  A.Connectivities[0] := BasicConnectivity;
  SetLength(A.Connectivities[0].RequiredPositions, 2);
  A.Connectivities[0].RequiredPositions[1] := Position(2);
  Check(Rejected(A, 'strict Z,Y,X'), 'duplicate terminals rejected');
  A.Connectivities[0].RequiredPositions[1] := Position(1);
  Check(Rejected(A, 'strict Z,Y,X'), 'descending terminal X rejected');
  A.Connectivities[0].RequiredPositions[0] := Position(0, 1);
  A.Connectivities[0].RequiredPositions[1] := Position(2);
  Check(Rejected(A, 'strict Z,Y,X'), 'terminal ordering compares Y before X');
  A := BuildInputs(3); SetLength(A.Connectivities, 1);
  A.Connectivities[0] := BasicConnectivity;
  SetLength(A.Connectivities[0].RequiredPositions, 2);
  A.Connectivities[0].RequiredPositions[0] := Position(0, 0, 1);
  A.Connectivities[0].RequiredPositions[1] := Position(2, 1, 0);
  Check(Rejected(A, 'strict Z,Y,X'), 'terminal ordering compares Z before Y/X');
  A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[0].Values[0].Value := 'a';
  A.Connectivities[0].Values[1].Value := 'z';
  Check(Rejected(A, 'strict public-vocabulary'), 'reordered profiles rejected');
  A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[0].Values[1].Value := 'z';
  Check(Rejected(A, 'strict public-vocabulary'), 'duplicate profiles rejected');
  A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[0].Values[1].Value := 'unknown';
  Check(Rejected(A, 'outside the public vocabulary'), 'unknown profiles never dropped');
  A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[0].Values := nil;
  Check(Rejected(A, 'participating profiles'), 'empty participating profile set rejected');
  A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[0].LabelText := '';
  Check(Rejected(A, 'cannot be empty'), 'empty connectivity label rejected');
  A.Connectivities[0] := BasicConnectivity(2);
  Check(Rejected(A, 'owner must be public'), 'private rules owner rejected');
  A.Connectivities[0] := BasicConnectivity(-1);
  Check(Rejected(A, 'exact integer'), 'negative owner rejected before indexing');
  A.Connectivities[0] := BasicConnectivity(99);
  Check(Rejected(A, 'owner'), 'past-end owner rejected');
  SetLength(A.Connectivities, 2);
  A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[1] := BasicConnectivity;
  Check(Rejected(A, 'duplicates the pass/label'), 'duplicate pass/label key rejected');
  A.Connectivities[1].PassIndex := 3;
  R := NewRecipe(A);
  try Check((R.ConnectivityAt(1).PassIndex = 3) and
    (R.PassAt(3).AdapterKind = wpakEmpty),
    'same label on a forward exact-copy alias is valid without materialization');
  finally R.Free; end;
  {$IFNDEF PAS2JS}
  A.Connectivities := nil; SetLength(A.Connectivities, 1);
  C := BasicConnectivity; B := 2; Move(B, C.RequireAllParticipants, 1);
  A.Connectivities[0] := C;
  Check(Rejected(A, 'must be Boolean'), 'noncanonical native all-participant Boolean rejected');
  C := BasicConnectivity; Move(B, C.Values[0].RequiredByValue, 1);
  A.Connectivities[0] := C;
  Check(Rejected(A, 'must be Boolean'), 'noncanonical native required-by-value Boolean rejected');
  C := BasicConnectivity; B := $80; Move(B, C.Values[0].Openings, 1);
  A.Connectivities[0] := C;
  Check(Rejected(A, 'invalid opening'), 'noncanonical native direction bits rejected');
  {$ENDIF}
end;

procedure TestProjectionOwners;
var A: TInputs; P: TWfcOverlappingModel2D; S: TWfcSequenceModel;
  R: TWfcPipelineModel; I: Integer;
begin
  for I := 0 to 1 do
  begin
    A := Default(TInputs);
    A.Metadata := MakeWfcPipelineMetadata('Projection connectivity', 'MIT', 'authored', '');
    SetLength(A.Resources, 1); SetLength(A.Passes, 3);
    SetLength(A.Dependencies, 2); SetLength(A.Bridges, 1);
    A.Dependencies[0] := MakeWfcPipelineDependency(1, 0);
    A.Dependencies[1] := MakeWfcPipelineDependency(2, 1);
    if I = 0 then
    begin
      A.Rank := 2; A.Wrap := True;
      P := LearnOverlappingModel2D(Tokens(['z', 'a', 'z', 'a']),
        2, 2, 1, 1, wmbWrap, wmsNone);
      try A.Resources[0] := MakeWfcPipelineResource('latent', wprkPattern2D,
        EncodeWfcPattern2DText(P), 'authored', 'MIT', ''); finally P.Free; end;
      A.Passes[0] := MakeWfcPipelinePass('latent', wppvPrivate,
        gpmOverlay, -1, wpakPattern2D, 0, False, wseWhole);
      A.Bridges[0] := MakeWfcPipelineBridge(wpbkPattern2DProjection, 0, 1);
    end else
    begin
      A.Rank := 1;
      S := LearnSequenceModel(Tokens(['z', 'a', 'z']), 2);
      try A.Resources[0] := MakeWfcPipelineResource('latent', wprkSequence,
        EncodeWfcSequenceText(S), 'authored', 'MIT', ''); finally S.Free; end;
      A.Passes[0] := MakeWfcPipelinePass('latent', wppvPrivate,
        gpmOverlay, -1, wpakSequence, 0, True, wseWhole);
      A.Bridges[0] := MakeWfcPipelineBridge(wpbkSequenceProjection, 0, 1);
    end;
    A.Passes[1] := MakeWfcPipelinePass('output', wppvPublic,
      gpmOverlay, -1, wpakEmpty, -1, False, wseWhole);
    A.Passes[2] := MakeWfcPipelinePass('alias', wppvPublic,
      gpmTransform, 1, wpakEmpty, -1, False, wseWhole);
    SetLength(A.Connectivities, 1); A.Connectivities[0] := BasicConnectivity(1);
    R := NewRecipe(A);
    try Check(R.ConnectivityAt(0).PassIndex = 1, 'learned projection public owner accepted');
    finally R.Free; end;
    A.Connectivities[0].PassIndex := 2;
    R := NewRecipe(A);
    try Check((R.ConnectivityAt(0).PassIndex = 2) and
      (R.PassAt(2).AdapterKind = wpakEmpty), 'alias of learned projection accepted');
    finally R.Free; end;
    A.Connectivities[0].PassIndex := 0;
    Check(Rejected(A, 'owner must be public'), 'private latent projection keys cannot own portable connectivity');
  end;
end;

procedure TestCapacityPreflight;
var A: TInputs; I, J: Integer; R: TWfcPipelineModel;
  V: TWfcPipelineConnectivityValues; P: TGraphPositions;
  C: TWfcPipelineConnectivity; Failed: Boolean;
  Vocabulary: TWfcModelTokens;
begin
  A := BuildInputs; A.Resources[0].Document := 'invalid nested resource';
  SetLength(A.Connectivities, 4097);
  Check(Rejected(A, 'connectivity count'), 'outer count rejected before resource decode');
  SetLength(A.Connectivities, 1); A.Connectivities[0] := BasicConnectivity;
  SetLength(A.Connectivities[0].Values, 1025);
  Check(Rejected(A, 'profile count'), 'per-descriptor profile cap precedes decoding');
  V := A.Connectivities[0].Values; Failed := False;
  try C := MakeWfcPipelineConnectivity(0, 'n', Position(0), nil, V);
  except on E: EWfcPipelineModel do Failed := True; end;
  Check(Failed, 'factory checks profile count before copying');
  A.Connectivities[0] := BasicConnectivity;
  SetLength(A.Connectivities[0].RequiredPositions, 65537);
  Check(Rejected(A, 'terminal count'), 'per-descriptor terminal cap precedes decoding');
  P := A.Connectivities[0].RequiredPositions; Failed := False;
  try C := MakeWfcPipelineConnectivity(0, 'n', Position(0), P, nil);
  except on E: EWfcPipelineModel do Failed := True; end;
  Check(Failed, 'factory checks terminal count before copying');
  SetLength(A.Connectivities, 65);
  for I := 0 to High(A.Connectivities) do
  begin
    A.Connectivities[I] := BasicConnectivity;
    SetLength(A.Connectivities[I].Values, 1024);
    for J := 0 to 1023 do
      A.Connectivities[I].Values[J] := MakeWfcPipelineConnectivityValue('z', []);
  end;
  Check(Rejected(A, 'aggregate connectivity profile count'),
    'aggregate profile budget checked before resources or duplicate profiles');
  SetLength(A.Connectivities, 2);
  for I := 0 to 1 do
  begin
    A.Connectivities[I] := BasicConnectivity;
    SetLength(A.Connectivities[I].RequiredPositions, 32769);
    for J := 0 to 32768 do A.Connectivities[I].RequiredPositions[J] := Position(J + 1);
  end;
  Check(Rejected(A, 'aggregate connectivity terminal count'),
    'aggregate terminal budget checked before resources');
  SetLength(A.Connectivities, 1); A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[0].LabelText := TWfcModelToken(StringOfChar('x', 1048577));
  Check(Rejected(A, 'raw token length'), 'label size checked before copying/decoding');
  A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[0].Values[0].Value := TWfcModelToken(StringOfChar('x', 1048577));
  Check(Rejected(A, 'raw token length'), 'profile token size checked before copying/decoding');
  A.Connectivities[0] := BasicConnectivity;
  A.Connectivities[0].LabelText := TWfcModelToken(StringOfChar('y', 1048576));
  SetLength(A.Quotas, 15);
  for I := 0 to 14 do
    A.Quotas[I] := MakeWfcPipelineValueQuota(0,
      TWfcModelToken(StringOfChar('x', 1048576)), Tokens(['z']), 0, 0);
  Check(Rejected(A, 'aggregate outer-token encoding'),
    'quotas and connectivity share existing outer-token byte budget');

  A := BuildInputs; SetLength(A.Connectivities, 1);
  A.Connectivities[0] := BasicConnectivity;
  SetLength(A.Connectivities[0].RequiredPositions, 65536);
  for I := 0 to 65535 do A.Connectivities[0].RequiredPositions[I] := Position(I + 1);
  R := NewRecipe(A);
  try Check(Length(R.ConnectivityAt(0).RequiredPositions) = 65536,
    'exact terminal capacity is valid before future run shape validation');
  finally R.Free; end;
  A := BuildInputs; SetLength(A.Connectivities, 4096);
  for I := 0 to 4095 do
  begin
    A.Connectivities[I] := BasicConnectivity;
    A.Connectivities[I].LabelText := TWfcModelToken('network-' + IntToStr(I));
    A.Connectivities[I].RequiredPositions := nil;
  end;
  R := NewRecipe(A);
  try Check(R.ConnectivityCount = 4096, 'exact descriptor capacity accepted');
  finally R.Free; end;

  A := BuildInputs;
  SetLength(Vocabulary, 1024); SetLength(V, 1024);
  for I := 0 to 1023 do
  begin
    Vocabulary[I] := TWfcModelToken('v' + IntToStr(I));
    V[I] := MakeWfcPipelineConnectivityValue(Vocabulary[I], [gdEast, gdWest]);
  end;
  SetRules(A, Vocabulary);
  SetLength(A.Connectivities, 64);
  for I := 0 to 63 do
    A.Connectivities[I] := MakeWfcPipelineConnectivity(0,
      TWfcModelToken('wide-' + IntToStr(I)), Position(0), nil, V);
  R := NewRecipe(A);
  try Check((R.ConnectivityCount = 64) and (Length(R.ConnectivityAt(63).Values) = 1024),
    'exact per-descriptor and aggregate 65536-profile capacities accepted');
  finally R.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestBrowserMalformed;
var A: TInputs; R: TWfcPipelineModel; C: TWfcPipelineConnectivity;
  I, J, X: Integer; Failed: Boolean; Flag: Boolean;
  Directions: TGraphDirections;
begin
  A := BuildInputs(3); SetLength(A.Connectivities, 1);
  A.Connectivities[0] := BasicConnectivity; R := NewRecipe(A);
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
      for J := 0 to 6 do
      begin
        A.Connectivities[0] := BasicConnectivity;
        case J of
          0: A.Connectivities[0].PassIndex := X;
          1: A.Connectivities[0].Root.X := X;
          2: A.Connectivities[0].Root.Y := X;
          3: A.Connectivities[0].Root.Z := X;
          4: A.Connectivities[0].RequiredPositions[0].X := X;
          5: A.Connectivities[0].RequiredPositions[0].Y := X;
          6: A.Connectivities[0].RequiredPositions[0].Z := X;
        end;
        Check(Rejected(A, 'exact integer'), 'malformed browser descriptor integer rejected');
      end;
      Failed := False;
      try C := R.ConnectivityAt(X); except on E: ERangeError do Failed := True; end;
      Check(Failed, 'malformed browser index rejected before array access');
    end;
    for I := 0 to 5 do
    begin
      case I of
        0: asm Flag = 0; end; 1: asm Flag = 1; end;
        2: asm Flag = "false"; end; 3: asm Flag = undefined; end;
        4: asm Flag = null; end; 5: asm Flag = NaN; end;
      end;
      A.Connectivities[0] := BasicConnectivity;
      A.Connectivities[0].RequireAllParticipants := Flag;
      Check(Rejected(A, 'must be Boolean'), 'all-participants requires a real browser Boolean');
      A.Connectivities[0] := BasicConnectivity;
      A.Connectivities[0].Values[0].RequiredByValue := Flag;
      Check(Rejected(A, 'must be Boolean'), 'required-by-value requires a real browser Boolean');
    end;
    A.Connectivities[0] := BasicConnectivity;
    asm Directions = {6:true}; end;
    A.Connectivities[0].Values[0].Openings := Directions;
    Check(Rejected(A, 'invalid opening'), 'browser set cannot introduce an unknown direction');
    asm Directions = {unexpected:true}; end;
    A.Connectivities[0].Values[0].Openings := Directions;
    Check(Rejected(A, 'invalid opening'), 'browser set cannot introduce nonordinal directions');
  finally R.Free; end;
end;
{$ENDIF}

begin
  RunTest('detached connectivity and existing constructor identity', @TestOwnershipAndLegacy);
  RunTest('all connectivity identity fields', @TestIdentityFields);
  RunTest('canonical profiles, terminals and public aliases', @TestCanonicalValidation);
  RunTest('learned public projection owners', @TestProjectionOwners);
  RunTest('capacity preflight and exact boundaries', @TestCapacityPreflight);
  {$IFDEF PAS2JS}RunTest('strict browser descriptor values', @TestBrowserMalformed);{$ENDIF}
  WriteLn('[SUMMARY] checks=', Checks, ' failures=', Failures);
  if Failures <> 0 then Halt(1);
end.
