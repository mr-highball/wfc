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
unit training_studio_connectivity;

{$mode delphi}{$H+}

interface

uses wfc, wfc_model, wfc_training, wfc_training_workspace;

function TrainingStudioPosition(const X, Y, Z: Integer): TGraphPosition;
function ParseTrainingStudioTerminals(const AText: String): TGraphPositions;
function TrainingStudioRouteSource: String;
function TrainingStudioRouteOptions: TWfcTrainingSolveOptions;
function TrainingStudioRouteNetwork: TWfcTrainingConnectivities;
function TrainingStudioRouteQuota: TWfcTrainingValueQuotas;
function TrainingStudioRouteIsValid(const ATokens: TWfcModelTokens): Boolean;
function TrainingStudioConnectivitySelfTest: Integer;
procedure RunTrainingStudioConnectivityDemo;

implementation

uses SysUtils, Classes, wfc_text_codec, wfc_training_text,
  wfc_pipeline_model, wfc_pipeline_text, wfc_pipeline_run,
  wfc_pipeline_run_text, wfc_pipeline_result, wfc_pipeline_result_text,
  wfc_pipeline_runtime, training_studio_presets;

function TrainingStudioPosition(const X, Y, Z: Integer): TGraphPosition;
begin
  Result.X := X; Result.Y := Y; Result.Z := Z;
end;

function BeforePosition(const A, B: TGraphPosition): Boolean;
begin
  Result := (A.Z < B.Z) or ((A.Z = B.Z) and
    ((A.Y < B.Y) or ((A.Y = B.Y) and (A.X < B.X))));
end;

function ParseTrainingStudioTerminals(const AText: String): TGraphPositions;
var Lines, Fields: TStringList; I, X, Y, Z, RecordCount: Integer;
  Temporary: TGraphPositions;
  procedure Sort(const First, Last: Integer);
  var Middle, Left, Right, Dest: Integer;
  begin
    if First >= Last then Exit;
    Middle := First + (Last - First) div 2;
    Sort(First, Middle); Sort(Middle + 1, Last);
    Left := First; Right := Middle + 1;
    for Dest := First to Last do
      if (Left <= Middle) and ((Right > Last) or
          BeforePosition(Result[Left], Result[Right])) then
      begin Temporary[Dest] := Result[Left]; Inc(Left); end
      else begin Temporary[Dest] := Result[Right]; Inc(Right); end;
    for Dest := First to Last do Result[Dest] := Temporary[Dest];
  end;
begin
  Result := nil;
  if AText = '' then Exit;
  if Length(AText) > WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH then
    raise EConvertError.Create('terminal draft exceeds the training text limit');
  { Count records before TStringList allocates them. In particular, a bounded
    multi-megabyte newline paste must not allocate millions of empty strings.
    Treat CRLF as one separator, matching TStringList.Text and browser LF. }
  RecordCount := 1; I := 1;
  while I <= Length(AText) do
  begin
    if AText[I] in [#10, #13] then
    begin
      if (AText[I] = #13) and (I < Length(AText)) and
          (AText[I + 1] = #10) then Inc(I);
      if I < Length(AText) then Inc(RecordCount);
      if RecordCount > WFC_TRAINING_MAX_CONNECTIVITY_TERMINAL_COUNT then
        raise EConvertError.Create('terminal draft exceeds the training terminal limit');
    end;
    Inc(I);
  end;
  Lines := TStringList.Create;
  Fields := TStringList.Create;
  try
    Lines.Text := AText;
    if Lines.Count > WFC_TRAINING_MAX_CONNECTIVITY_TERMINAL_COUNT then
      raise EConvertError.Create('terminal draft exceeds the training terminal limit');
    Fields.Delimiter := ','; Fields.StrictDelimiter := True;
    SetLength(Result, Lines.Count);
    for I := 0 to Lines.Count - 1 do
    begin
      { Three canonical nonnegative Integer coordinates need at most ten
        decimal digits each plus two commas. Check before DelimitedText can
        allocate one string per comma in a malformed multi-megabyte row. }
      if Length(Lines[I]) > 32 then
        raise EConvertError.Create('terminal row exceeds canonical X,Y,Z length');
      Fields.DelimitedText := Lines[I];
      if (Fields.Count <> 3) or (Pos('"', Lines[I]) <> 0) then
        raise EConvertError.Create('each terminal must be exactly X,Y,Z');
      X := WfcTextParseCanonicalInteger(Fields[0], 'terminal X', 'studio');
      Y := WfcTextParseCanonicalInteger(Fields[1], 'terminal Y', 'studio');
      Z := WfcTextParseCanonicalInteger(Fields[2], 'terminal Z', 'studio');
      if (X < 0) or (Y < 0) or (Z < 0) then
        raise EConvertError.Create('terminal coordinates must be nonnegative');
      Result[I] := TrainingStudioPosition(X, Y, Z);
    end;
    SetLength(Temporary, Length(Result));
    Sort(0, High(Result));
    for I := 1 to High(Result) do
      if not BeforePosition(Result[I - 1], Result[I]) then
        raise EConvertError.Create('duplicate terminal position');
  finally Fields.Free; Lines.Free; end;
end;

function TrainingStudioRouteSource: String;
var S: TWfcTrainingSamples; V: TWfcModelTokens; D: TWfcTrainingDocument;
  I, J: Integer;
begin
  { Every two-by-two road/grass combination is an explicit authored sample.
    Local adjacency therefore allows every pair; connectivity is not learned
    or inferred from the corpus. This is separate from the six old presets. }
  SetLength(S, 16);
  for I := 0 to 15 do
  begin
    V := nil; SetLength(V, 4);
    for J := 0 to 3 do
      if (I and (1 shl J)) <> 0 then V[J] := 'road' else V[J] := 'grass';
    S[I] := MakeWfcTrainingSample('pair-corpus-' + IntToStr(I), 2, 2, V);
  end;
  D := TWfcTrainingDocument.Create(MakeWfcTrainingMetadata(
    'authored-connected-routes', 'MIT', 'project-authored complete road-grass pair corpus'),
    MakeWfcTrainingOptions(wtkAdjacency2D, wmbOpen, wmsNone, 0, 0, 0), S);
  try Result := EncodeWfcTrainingText(D); finally D.Free; end;
end;

function TrainingStudioRouteOptions: TWfcTrainingSolveOptions;
begin
  Result := DefaultWfcTrainingSolveOptions;
  Result.Width := 4; Result.Height := 3; Result.Seed := 17;
end;

function TrainingStudioRouteNetwork: TWfcTrainingConnectivities;
var P: TWfcTrainingConnectivityValues; T: TGraphPositions;
begin
  SetLength(P, 1);
  P[0] := MakeWfcTrainingConnectivityValue('road',
    [gdNorth, gdEast, gdSouth, gdWest], False);
  SetLength(T, 1); T[0] := TrainingStudioPosition(3, 2, 0);
  Result := nil; SetLength(Result, 1);
  Result[0] := MakeWfcTrainingConnectivity('connected roads',
    TrainingStudioPosition(0, 0, 0), T, P, True);
end;

function TrainingStudioRouteQuota: TWfcTrainingValueQuotas;
var V: TWfcModelTokens;
begin
  SetLength(V, 1); V[0] := 'road';
  Result := nil; SetLength(Result, 1);
  Result[0] := MakeWfcTrainingValueQuota('six roads', V, 6, 6);
end;

function TrainingStudioRouteIsValid(const ATokens: TWfcModelTokens): Boolean;
var Seen: array[0..11] of Boolean; Queue: array[0..11] of Integer;
  Head, Tail, I, J, X, Y, Count: Integer;
  procedure Visit(const N: Integer);
  begin
    if (ATokens[N] = 'road') and not Seen[N] then
    begin Seen[N] := True; Queue[Tail] := N; Inc(Tail); end;
  end;
begin
  Result := False;
  if Length(ATokens) <> 12 then Exit;
  Count := 0;
  for I := 0 to 11 do
  begin
    Seen[I] := False;
    if ATokens[I] = 'road' then Inc(Count)
    else if ATokens[I] <> 'grass' then Exit;
  end;
  if (Count <> 6) or (ATokens[0] <> 'road') or (ATokens[11] <> 'road') then Exit;
  Head := 0; Tail := 0; Visit(0);
  while Head < Tail do
  begin
    J := Queue[Head]; Inc(Head); X := J mod 4; Y := J div 4;
    if X > 0 then Visit(J - 1);
    if X < 3 then Visit(J + 1);
    if Y > 0 then Visit(J - 4);
    if Y < 2 then Visit(J + 4);
  end;
  Result := Seen[11] and (Tail = Count);
end;

procedure RunTrainingStudioConnectivityDemo;
var W: TWfcTrainingWorkspace; V: TWfcModelTokens; I: Integer;
begin
  W := TWfcTrainingWorkspace.Create(InteractiveWfcTrainingWorkspaceLimits);
  try
    W.SetSourceText(TrainingStudioRouteSource); W.Train;
    W.ReplaceConnectivities(TrainingStudioRouteNetwork);
    W.ReplaceValueQuotas(TrainingStudioRouteQuota);
    W.ConfigureRun(TrainingStudioRouteOptions, nil, nil); W.Solve;
    V := W.OutputTokens;
    if not TrainingStudioRouteIsValid(V) then
      raise Exception.Create('independent authored route validation failed');
    WriteLn('connectivity-demo: 4x3 open output; root=0,0,0 terminal=3,2,0; exactly six roads');
    WriteLn('source=', W.TrainingSignatureText, ' recipe=', W.RecipeSignatureText,
      ' result=', W.ResultSignatureText, ' source-version=4');
    for I := 0 to High(V) do
    begin
      if I mod 4 <> 0 then Write(' ');
      Write(V[I]);
      if I mod 4 = 3 then WriteLn;
    end;
    WriteLn('independent-reciprocal-road-BFS=passed observed-roads=6');
  finally W.Free; end;
end;

function TrainingStudioConnectivitySelfTest: Integer;
var W: TWfcTrainingWorkspace; O: TWfcTrainingSolveOptions;
  C: TWfcTrainingConnectivities; Q: TWfcTrainingValueQuotas;
  Source, Recipe, Solved, Saved: String; D: TWfcTrainingDocument;
  M: TWfcPipelineModel; R: TWfcPipelineRun; Output, Decoded: TWfcPipelineResult;
  Positions: TGraphPositions; Raised: Boolean; V: TWfcModelTokens;
  procedure Check(const Condition: Boolean; const Text: String);
  begin
    Inc(Result);
    if not Condition then raise Exception.Create('training connectivity demo: ' + Text);
  end;
  procedure Solve;
  begin W.ConfigureRun(O, nil, nil); W.Solve; end;
begin
  Result := 0;
  W := TWfcTrainingWorkspace.Create(InteractiveWfcTrainingWorkspaceLimits);
  try
    W.SetSourceText(TrainingStudioRouteSource); W.Train;
    Source := W.SourceText; Recipe := W.RecipeText;
    W.ReplaceConnectivities(TrainingStudioRouteNetwork);
    Check((W.ConnectivityCount = 1) and not W.HasRun and not W.HasResult,
      'network mutation clears run and result');
    Check((Pos('wfclearn=4'#10, W.SourceText) = 1) and
      (Pos('wfcpipeline=3'#10, W.RecipeText) = 1), 'source v4 and recipe v3');
    Q := TrainingStudioRouteQuota; W.ReplaceValueQuotas(Q);
    Check((W.ConnectivityCount = 1) and (W.ValueQuotaCount = 1),
      'quota replacement preserves network');
    C := W.CopyConnectivities; W.ReplaceConnectivities(C);
    Check(W.ValueQuotaCount = 1, 'network replacement preserves quota');
    O := TrainingStudioRouteOptions; Solve;
    Check(W.ResultStatus = wprsSolved, 'authored route solves');
    Check(TrainingStudioRouteIsValid(W.OutputTokens), 'independent BFS and exact count');
    Solved := W.ResultText; Saved := W.SourceText;
    W.SetSourceText(Saved); W.Train; Solve;
    Check(W.ResultText = Solved, 'saved source retrains exact result');
    D := DecodeWfcTrainingText(Saved);
    try Check(EncodeWfcTrainingText(D) = Saved, 'canonical source roundtrip');
    finally D.Free; end;
    M := DecodeWfcPipelineModelText(W.RecipeText);
    try
      Check((M.ConnectivityCount = 1) and (M.ValueQuotaCount = 1), 'portable recipe policies');
      R := DecodeWfcPipelineRunText(W.RunText, M);
      try
        Output := ExecuteWfcPipeline(M, R);
        try Check(EncodeWfcPipelineResultText(Output) = Solved, 'full portable replay');
        finally Output.Free; end;
        Decoded := DecodeWfcPipelineResultText(Solved, M, R);
        try Check(EncodeWfcPipelineResultText(Decoded) = Solved, 'result canonical roundtrip');
        finally Decoded.Free; end;
      finally R.Free; end;
    finally M.Free; end;
    Q[0].MinimumCount := 5; Q[0].MaximumCount := 5;
    W.ReplaceValueQuotas(Q); Solve;
    Check((W.ResultStatus = wprsContradiction) and (Length(W.OutputTokens) = 0),
      'five cells cannot span Manhattan distance five');
    W.ReplaceValueQuotas(TrainingStudioRouteQuota); Solve;
    Check(W.ResultText = Solved, 'contradiction recovery exact');
    C := W.CopyConnectivities; C[0].Values[0].Openings := [gdEast, gdWest];
    W.ReplaceConnectivities(C); Solve;
    Check((W.ResultStatus = wprsContradiction) and (Length(W.OutputTokens) = 0),
      'ports are authored, never inferred');
    W.ReplaceConnectivities(TrainingStudioRouteNetwork); Solve;
    Check(W.ResultText = Solved, 'port correction recovers');
    C := W.CopyConnectivities; C[0].Values := nil;
    Raised := False;
    try W.ReplaceConnectivities(C); except on E: Exception do Raised := True; end;
    Check(Raised and not W.HasRecipe and not W.HasRun and not W.HasResult and
      (W.SourceText = Saved), 'invalid draft preserves source but clears every derived artifact');
    W.Train; Solve;
    Check(W.ResultText = Solved, 'retained valid source recovers after malformed draft');
    C := W.CopyConnectivities; C[0].Root.X := 4;
    W.ReplaceConnectivities(C);
    Raised := False;
    try Solve; except on E: Exception do Raised := True; end;
    Check(Raised and not W.HasResult, 'shape-incompatible root exposes no stale result');
    W.ReplaceConnectivities(TrainingStudioRouteNetwork);
    W.ReplaceValueQuotas(nil);
    Check((W.ConnectivityCount = 1) and (Pos('wfclearn=4'#10, W.SourceText) = 1),
      'clearing quotas retains source-owned network');
    W.ReplaceConnectivities(nil);
    Check((W.SourceText = Source) and (W.RecipeText = Recipe),
      'removing both policies restores legacy bytes');
    Positions := ParseTrainingStudioTerminals('2,0,1'#10'1,2,0'#10'0,1,0');
    Check((Positions[0].Y = 1) and (Positions[1].Y = 2) and
      (Positions[2].Z = 1), 'terminal canonical Z/Y/X order');
    Raised := False;
    try Positions := ParseTrainingStudioTerminals('0,0,1'#10'0,0,1');
    except on E: Exception do Raised := True; end;
    Check(Raised, 'duplicate terminal rejected');
    Raised := False;
    try Positions := ParseTrainingStudioTerminals('-1,0,0');
    except on E: Exception do Raised := True; end;
    Check(Raised, 'negative terminal rejected before unsigned conversion');
    Raised := False;
    try Positions := ParseTrainingStudioTerminals('0,0.5,0');
    except on E: Exception do Raised := True; end;
    Check(Raised, 'fractional terminal rejected');
    Raised := False;
    try Positions := ParseTrainingStudioTerminals(
      StringOfChar(#10, WFC_TRAINING_MAX_CONNECTIVITY_TERMINAL_COUNT + 1));
    except on E: Exception do Raised := True; end;
    Check(Raised, 'newline-only paste is capped before allocating terminal records');
    Raised := False;
    try Positions := ParseTrainingStudioTerminals(StringOfChar(',', 65536));
    except on E: Exception do Raised := True; end;
    Check(Raised, 'comma-heavy row is capped before allocating coordinate fields');
    Check(not TrainingStudioRouteIsValid(nil), 'independent route validator rejects incomplete output');
    SetLength(V, 12);
    V[0] := 'road'; V[1] := 'road'; V[2] := 'road'; V[3] := 'grass';
    V[4] := 'grass'; V[5] := 'grass'; V[6] := 'grass'; V[7] := 'grass';
    V[8] := 'grass'; V[9] := 'road'; V[10] := 'road'; V[11] := 'road';
    Check(not TrainingStudioRouteIsValid(V), 'independent route validator rejects disconnected exact-six output');
    W.SetSourceText(TrainingStudioPresetText(5)); W.Train;
    SetLength(C, 1); SetLength(C[0].Values, 2);
    C[0].Values[0] := MakeWfcTrainingConnectivityValue('B', [gdUp, gdDown], True);
    C[0].Values[1] := MakeWfcTrainingConnectivityValue('A', [gdUp, gdDown], False);
    C[0].LabelText := 'vertical column'; C[0].Root := TrainingStudioPosition(0, 0, 0);
    SetLength(C[0].RequiredPositions, 1);
    C[0].RequiredPositions[0] := TrainingStudioPosition(0, 0, 1);
    C[0].RequireAllParticipants := False;
    W.ReplaceConnectivities(C);
    Check((W.CopyConnectivities[0].Values[0].Value = 'B') and
      (W.CopyConnectivities[0].Values[1].Value = 'A'), 'authored profile order retained');
    O := DefaultWfcTrainingSolveOptions; O.Width := 2; O.Height := 2;
    W.ConfigureVolumeRun(O, 2, nil, nil); W.Solve;
    Check(W.ResultStatus = wprsContradiction, 'required-by-value rejects other vertical islands');
    C[0].Values[0].RequiredByValue := False;
    W.ReplaceConnectivities(C); W.ConfigureVolumeRun(O, 2, nil, nil); W.Solve;
    Check((W.ResultStatus = wprsSolved) and (Length(W.OutputTokens) = 8),
      'explicit XYZ terminal and vertical ports solve optional islands');
    C[0].RequireAllParticipants := True;
    W.ReplaceConnectivities(C); W.ConfigureVolumeRun(O, 2, nil, nil); W.Solve;
    Check(W.ResultStatus = wprsContradiction, 'all-participants rejects vertical islands');
    W.SetSourceText(TrainingStudioPresetText(3)); W.Train;
    C := nil; SetLength(C, 1); SetLength(C[0].Values, 1);
    C[0].Values[0] := MakeWfcTrainingConnectivityValue(
      WfcTextDecodeToken('caf%C3%A9', 'test'), [], False);
    C[0].Root := TrainingStudioPosition(0, 0, 0);
    C[0].LabelText := 'root % / ' + C[0].Values[0].Value;
    W.ReplaceConnectivities(C);
    O := TrainingStudioPresetOptions(3); Solve;
    Check((W.ResultStatus = wprsSolved) and
      (W.OutputTokens[0] = C[0].Values[0].Value),
      'Unicode participant with explicit zero ports admits only its singleton root');
    Check(W.CopyConnectivities[0].Values[0].Value = C[0].Values[0].Value,
      'Unicode token identity survives source and learner vocabulary');
  finally W.Free; end;
end;

end.
