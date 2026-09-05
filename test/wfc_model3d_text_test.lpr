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
program wfc_model3d_text_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc_model,
  wfc_model_text;

type
  TTestProcedure = procedure;

const
  GOLDEN_VOLUME_SINGLE =
    'wfcm=3'#10 +
    'rank=3'#10 +
    'samples=1'#10 +
    's=0,2,1,3'#10 +
    'boundary=open'#10 +
    'symmetry=cube24'#10 +
    'directions=N,E,S,W,U,D'#10 +
    'values=2'#10 +
    'v=0,5,A'#10 +
    'v=1,7,B'#10 +
    'relations=6'#10 +
    'r=N,0,1,1'#10 +
    'r=E,0,0,2'#10 +
    'r=S,1,0,1'#10 +
    'r=W,0,0,2'#10 +
    'r=U,0,1,3'#10 +
    'r=D,1,0,3'#10 +
    'end'#10;

  GOLDEN_VOLUME_CORPUS =
    'wfcm=3'#10 +
    'rank=3'#10 +
    'samples=2'#10 +
    's=0,2,1,3'#10 +
    's=1,1,2,2'#10 +
    'boundary=wrap'#10 +
    'symmetry=cube48'#10 +
    'directions=N,E,S,W,U,D'#10 +
    'values=2'#10 +
    'v=0,5,A'#10 +
    'v=1,7,B'#10 +
    'relations=6'#10 +
    'r=N,0,1,1'#10 +
    'r=E,0,0,2'#10 +
    'r=S,1,0,1'#10 +
    'r=W,0,0,2'#10 +
    'r=U,0,1,3'#10 +
    'r=D,1,0,3'#10 +
    'end'#10;

  GOLDEN_LEGACY_SINGLE =
    'wfcm=1'#10 +
    'rank=1'#10 +
    'width=3'#10 +
    'height=1'#10 +
    'boundary=wrap'#10 +
    'symmetry=none'#10 +
    'directions=E,W'#10 +
    'values=2'#10 +
    'v=0,2,A'#10 +
    'v=1,1,B'#10 +
    'relations=6'#10 +
    'r=E,0,0,1'#10 +
    'r=E,0,1,1'#10 +
    'r=E,1,0,1'#10 +
    'r=W,0,0,1'#10 +
    'r=W,0,1,1'#10 +
    'r=W,1,0,1'#10 +
    'end'#10;

  GOLDEN_LEGACY_CORPUS =
    'wfcm=2'#10 +
    'rank=1'#10 +
    'samples=2'#10 +
    's=0,3,1'#10 +
    's=1,2,1'#10 +
    'boundary=wrap'#10 +
    'symmetry=none'#10 +
    'directions=E,W'#10 +
    'values=4'#10 +
    'v=0,2,A'#10 +
    'v=1,1,B'#10 +
    'v=2,1,C'#10 +
    'v=3,1,D'#10 +
    'relations=10'#10 +
    'r=E,0,0,1'#10 +
    'r=E,0,1,1'#10 +
    'r=E,1,0,1'#10 +
    'r=E,2,3,1'#10 +
    'r=E,3,2,1'#10 +
    'r=W,0,0,1'#10 +
    'r=W,0,1,1'#10 +
    'r=W,1,0,1'#10 +
    'r=W,2,3,1'#10 +
    'r=W,3,2,1'#10 +
    'end'#10;

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

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try
    ATest;
  except
    on E: Exception do
    begin
      Inc(GFailureCount);
      WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function ReplaceText(const AText, AOld, ANew: String): String;
begin
  Result := StringReplace(AText, AOld, ANew, [rfReplaceAll]);
end;

function RelationSlot(const ADirection: TWfcModelDirection;
  const ASource, ATarget, AValueCount: Integer): Integer;
begin
  Result := ((Ord(ADirection) * AValueCount + ASource) * AValueCount) +
    ATarget;
end;

function MakeVolumeModel(const ACorpus: Boolean;
  const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcModel;
var
  LRelations: TWfcModelIntegerArray;
  LShapes: TWfcModelSampleShapes;
  LTokens: TWfcModelTokens;
  LWeights: TWfcModelIntegerArray;
begin
  Result := nil;
  if ACorpus then
  begin
    SetLength(LShapes, 2);
    LShapes[0] := MakeWfcModelSampleShape(2, 1, 3);
    LShapes[1] := MakeWfcModelSampleShape(1, 2, 2);
  end
  else
  begin
    SetLength(LShapes, 1);
    LShapes[0] := MakeWfcModelSampleShape(2, 1, 3);
  end;

  SetLength(LTokens, 2);
  LTokens[0] := 'A';
  LTokens[1] := 'B';
  SetLength(LWeights, 2);
  LWeights[0] := 5;
  LWeights[1] := 7;
  SetLength(LRelations, WfcModelStoredDirectionCount(3) * 4);
  LRelations[RelationSlot(wmdNorth, 0, 1, 2)] := 1;
  LRelations[RelationSlot(wmdEast, 0, 0, 2)] := 2;
  LRelations[RelationSlot(wmdSouth, 1, 0, 2)] := 1;
  LRelations[RelationSlot(wmdWest, 0, 0, 2)] := 2;
  LRelations[RelationSlot(wmdUp, 0, 1, 2)] := 3;
  LRelations[RelationSlot(wmdDown, 1, 0, 2)] := 3;
  Result := TWfcModel.Create(3, LShapes, ABoundary, ASymmetry,
    [wmdNorth, wmdEast, wmdSouth, wmdWest, wmdUp, wmdDown],
    LTokens, LWeights, LRelations);
end;

procedure ExpectDecodeFailure(const AText, AMessage: String);
var
  LModel: TWfcModel;
  LRaised: Boolean;
begin
  LModel := nil;
  LRaised := False;
  try
    try
      LModel := DecodeWfcModelText(AText);
    except
      on E: Exception do
        LRaised := True;
    end;
  finally
    LModel.Free;
  end;
  Check(LRaised, AMessage);
end;

procedure TestSingletonGolden;
var
  LCopy: TWfcModelIntegerArray;
  LDecoded: TWfcModel;
  LModel: TWfcModel;
  LShape: TWfcModelSampleShape;
begin
  Check(WFC_MODEL_TEXT_VERSION = 3,
    'the latest model text profile is version 3');
  LModel := MakeVolumeModel(False, wmbOpen, wmsCubeRotations);
  try
    Check(EncodeWfcModelText(LModel) = GOLDEN_VOLUME_SINGLE,
      'a singleton volume has exact canonical wfcm=3 bytes');
  finally
    LModel.Free;
  end;

  LDecoded := DecodeWfcModelText(GOLDEN_VOLUME_SINGLE);
  try
    LShape := LDecoded.SampleShapeAt(0);
    Check((LDecoded.Rank = 3) and (LDecoded.SampleCount = 1) and
      (LDecoded.SampleDepth = 3) and (LShape.Width = 2) and
      (LShape.Height = 1) and (LShape.Depth = 3),
      'wfcm=3 retains the explicit singleton volume shape');
    Check((LDecoded.Boundary = wmbOpen) and
      (LDecoded.Symmetry = wmsCubeRotations),
      'wfcm=3 retains boundary and cube24 symmetry');
    Check(LDecoded.Directions =
      [wmdNorth, wmdEast, wmdSouth, wmdWest, wmdUp, wmdDown],
      'wfcm=3 retains all six ordered directions');
    Check((LDecoded.RelationCount(wmdUp, 0, 1) = 3) and
      (LDecoded.RelationCount(wmdDown, 1, 0) = 3),
      'wfcm=3 retains reciprocal vertical counts');
    LCopy := LDecoded.CopyRelations;
    Check(Length(LCopy) = 24,
      'a two-value volume exposes six dense relation planes');
    Check(EncodeWfcModelText(LDecoded) = GOLDEN_VOLUME_SINGLE,
      'the singleton volume round-trips byte-for-byte');
  finally
    LDecoded.Free;
  end;
end;

procedure TestCorpusAndSymmetries;
const
  SYMMETRIES: array[0..3] of TWfcModelSymmetry =
    (wmsNone, wmsD4, wmsCubeRotations, wmsCubeFull);
  NAMES: array[0..3] of String =
    ('none', 'd4', 'cube24', 'cube48');
var
  I: Integer;
  LDecoded: TWfcModel;
  LModel: TWfcModel;
  LShape: TWfcModelSampleShape;
  LText: String;
begin
  LModel := MakeVolumeModel(True, wmbWrap, wmsCubeFull);
  try
    Check(EncodeWfcModelText(LModel) = GOLDEN_VOLUME_CORPUS,
      'a volume corpus has exact canonical wfcm=3 bytes');
  finally
    LModel.Free;
  end;

  LDecoded := DecodeWfcModelText(GOLDEN_VOLUME_CORPUS);
  try
    LShape := LDecoded.SampleShapeAt(1);
    Check((LDecoded.SampleCount = 2) and (LShape.Width = 1) and
      (LShape.Height = 2) and (LShape.Depth = 2),
      'wfcm=3 retains every ordered corpus volume shape');
    Check((LDecoded.Boundary = wmbWrap) and
      (LDecoded.Symmetry = wmsCubeFull),
      'wfcm=3 retains wrap and cube48 policies');
    Check(EncodeWfcModelText(LDecoded) = GOLDEN_VOLUME_CORPUS,
      'the volume corpus round-trips byte-for-byte');
  finally
    LDecoded.Free;
  end;

  for I := Low(SYMMETRIES) to High(SYMMETRIES) do
  begin
    LModel := MakeVolumeModel(False, wmbOpen, SYMMETRIES[I]);
    try
      LText := EncodeWfcModelText(LModel);
      Check(Pos('symmetry=' + NAMES[I] + #10, LText) > 0,
        'rank-3 symmetry has canonical name ' + NAMES[I]);
      LDecoded := DecodeWfcModelText(LText);
      try
        Check(LDecoded.Symmetry = SYMMETRIES[I],
          'rank-3 symmetry round-trips as ' + NAMES[I]);
      finally
        LDecoded.Free;
      end;
    finally
      LModel.Free;
    end;
  end;
end;

procedure TestLegacyBytes;
var
  LCopy: TWfcModelIntegerArray;
  LModel: TWfcModel;
begin
  LModel := DecodeWfcModelText(GOLDEN_LEGACY_SINGLE);
  try
    Check(EncodeWfcModelText(LModel) = GOLDEN_LEGACY_SINGLE,
      'legacy singleton bytes remain canonical wfcm=1');
    Check(LModel.SampleDepth = 1,
      'legacy singleton shape normalizes to depth one');
    LCopy := LModel.CopyRelations;
    Check(Length(LCopy) = 16,
      'legacy two-value storage remains four relation planes');
    Check((LModel.RelationCount(wmdUp, 0, 0) = 0) and
      (LModel.RelationCount(wmdDown, 1, 1) = 0),
      'legacy vertical relation queries are zero');
  finally
    LModel.Free;
  end;

  LModel := DecodeWfcModelText(GOLDEN_LEGACY_CORPUS);
  try
    Check(EncodeWfcModelText(LModel) = GOLDEN_LEGACY_CORPUS,
      'legacy corpus bytes remain canonical wfcm=2');
    Check((LModel.SampleShapeAt(0).Depth = 1) and
      (LModel.SampleShapeAt(1).Depth = 1),
      'every legacy corpus shape normalizes to depth one');
    LCopy := LModel.CopyRelations;
    Check(Length(LCopy) = 64,
      'legacy four-value corpus keeps four dense relation planes');
  finally
    LModel.Free;
  end;
end;

procedure TestProfileRejections;
begin
  ExpectDecodeFailure(ReplaceText(GOLDEN_VOLUME_SINGLE,
    'rank=3', 'rank=2'), 'wfcm=3 rejects a legacy rank');
  ExpectDecodeFailure(ReplaceText(GOLDEN_VOLUME_SINGLE,
    'wfcm=3', 'wfcm=2'), 'wfcm=2 rejects rank 3');
  ExpectDecodeFailure(ReplaceText(GOLDEN_LEGACY_SINGLE,
    'rank=1', 'rank=3'), 'wfcm=1 rejects rank 3');
  ExpectDecodeFailure(ReplaceText(GOLDEN_LEGACY_SINGLE,
    'symmetry=none', 'symmetry=cube24'),
    'wfcm=1 rejects cube24 symmetry');
  ExpectDecodeFailure(ReplaceText(GOLDEN_LEGACY_CORPUS,
    'symmetry=none', 'symmetry=cube48'),
    'wfcm=2 rejects cube48 symmetry');
  ExpectDecodeFailure(ReplaceText(GOLDEN_LEGACY_SINGLE,
    'directions=E,W', 'directions=E,W,U,D'),
    'wfcm=1 rejects vertical directions');
  ExpectDecodeFailure(ReplaceText(GOLDEN_VOLUME_SINGLE,
    'samples=1', 'samples=0'),
    'wfcm=3 rejects an empty sample corpus');
  ExpectDecodeFailure(ReplaceText(GOLDEN_VOLUME_SINGLE,
    'samples=1'#10's=0,2,1,3', 'width=2'#10'height=1'),
    'wfcm=3 rejects legacy singular shape fields');
end;

procedure TestVolumeRecordRejections;
begin
  ExpectDecodeFailure(ReplaceText(GOLDEN_VOLUME_SINGLE,
    's=0,2,1,3', 's=0,2,1'),
    'wfcm=3 requires sample depth');
  ExpectDecodeFailure(ReplaceText(GOLDEN_VOLUME_SINGLE,
    's=0,2,1,3', 's=0,2,1,3,4'),
    'wfcm=3 rejects an extra sample field');
  ExpectDecodeFailure(ReplaceText(GOLDEN_VOLUME_SINGLE,
    's=0,2,1,3', 's=0,2,1,0'),
    'wfcm=3 rejects zero sample depth');
  ExpectDecodeFailure(ReplaceText(GOLDEN_VOLUME_SINGLE,
    's=0,2,1,3', 's=0,2,1,03'),
    'wfcm=3 rejects noncanonical sample depth');
  ExpectDecodeFailure(ReplaceText(GOLDEN_VOLUME_SINGLE,
    's=0,2,1,3', 's=0,4194304,1,2'),
    'wfcm=3 preflights volume cell overflow');
  ExpectDecodeFailure(ReplaceText(GOLDEN_VOLUME_SINGLE,
    'directions=N,E,S,W,U,D', 'directions=N,E,S,W,D,U'),
    'wfcm=3 rejects noncanonical direction order');
  ExpectDecodeFailure(ReplaceText(GOLDEN_VOLUME_SINGLE,
    'directions=N,E,S,W,U,D', 'directions=N,E,S,W,U'),
    'wfcm=3 requires all six directions');
  ExpectDecodeFailure(ReplaceText(GOLDEN_VOLUME_SINGLE,
    'r=U,0,1,3'#10'r=D,1,0,3',
    'r=D,1,0,3'#10'r=U,0,1,3'),
      'wfcm=3 relations remain direction-major ordered');
end;

{$IFDEF PAS2JS}
procedure TestMalformedBrowserVolumeAxes;
const
  AXIS_NAMES: array[0..2] of String = ('width', 'height', 'depth');
var
  I: Integer;
  LModel: TWfcModel;
  LRaised: Boolean;
  LRelations: TWfcModelIntegerArray;
  LShapes: TWfcModelSampleShapes;
  LTokens: TWfcModelTokens;
  LWeights: TWfcModelIntegerArray;
begin
  SetLength(LTokens, 1);
  LTokens[0] := 'A';
  SetLength(LWeights, 1);
  LWeights[0] := 1;
  SetLength(LRelations, WfcModelStoredDirectionCount(3));
  for I := 0 to 2 do
  begin
    SetLength(LShapes, 1);
    LShapes[0] := MakeWfcModelSampleShape(2, 2, 2);
    case I of
      0: asm LShapes[0].Width = 1.5; end;
      1: asm LShapes[0].Height = 1.5; end;
      2: asm LShapes[0].Depth = 1.5; end;
    end;
    LModel := nil;
    LRaised := False;
    try
      try
        LModel := TWfcModel.Create(3, LShapes, wmbOpen, wmsNone,
          [wmdNorth, wmdEast, wmdSouth, wmdWest, wmdUp, wmdDown],
          LTokens, LWeights, LRelations);
      except
        on E: EWfcModel do
          LRaised := True;
      end;
    finally
      LModel.Free;
    end;
    Check(LRaised, 'direct rank-3 construction rejects fractional ' +
      AXIS_NAMES[I]);
  end;
end;
{$ENDIF}

begin
  WriteLn('WFC rank-3 model-text conformance suite');
  WriteLn('======================================');
  RunTest('singleton exact golden', @TestSingletonGolden);
  RunTest('corpus and symmetry profiles', @TestCorpusAndSymmetries);
  RunTest('legacy byte compatibility', @TestLegacyBytes);
  RunTest('format profile rejections', @TestProfileRejections);
  RunTest('volume record rejections', @TestVolumeRecordRejections);
  {$IFDEF PAS2JS}
  RunTest('browser fractional volume axes', @TestMalformedBrowserVolumeAxes);
  {$ENDIF}
  WriteLn('======================================');
  WriteLn('Checks: ', GCheckCount, '  Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d rank-3 model-text checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
