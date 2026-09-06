{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
unit wfc_volume_symmetry;

{$mode delphi}{$H+}

interface

uses SysUtils, wfc_model;

const
  WFC_VOLUME_SYMMETRY_VERSION = 1;

type
  EWfcVolumeSymmetry = class(EWfcModel);
  { Destination X/Y/Z reads the indicated source axis (0=X,1=Y,2=Z).
    A negative sign reverses that source coordinate. Literal transforms do
    not average adjacency counts and do not assume a cubic source volume. }
  TWfcVolumeTransform = record
    AxisX, AxisY, AxisZ: Integer;
    SignX, SignY, SignZ: Integer;
  end;

{ Identity is first. Permutations are 012,021,102,120,201,210 and sign masks
  increase from0 to7 (bits0/1/2 reverse destinationX/Y/Z). Cube rotations keep
  determinant+1; cube-full retains all48. D4 retains permutations012/102 and
  only sign masks0..3, preserving the Z axis and its positive orientation. }
function WfcVolumeTransformCount(const ASymmetry: TWfcModelSymmetry): Integer;
function WfcVolumeTransformAt(const ASymmetry: TWfcModelSymmetry;
  const AIndex: Integer): TWfcVolumeTransform;
function WfcTransformVolumeShape(const ASourceShape: TWfcModelSampleShape;
  const ATransform: TWfcVolumeTransform): TWfcModelSampleShape;
{ AX/AY/AZ are coordinates in the transformed destination shape. }
procedure WfcVolumeTransformToSource(const ASourceShape: TWfcModelSampleShape;
  const ATransform: TWfcVolumeTransform; const AX, AY, AZ: Integer;
  out ASourceX, ASourceY, ASourceZ: Integer);
function WfcTransformVolumeIntegers(const AValues: TWfcModelIntegerArray;
  const ASourceShape: TWfcModelSampleShape;
  const ATransform: TWfcVolumeTransform): TWfcModelIntegerArray;
function WfcTransformVolumeTokens(const ATokens: TWfcModelTokens;
  const ASourceShape: TWfcModelSampleShape;
  const ATransform: TWfcVolumeTransform): TWfcModelTokens;

implementation

procedure RequireInteger(const V, Minimum, Maximum: Integer; const Name: String);
var Valid: Boolean;
begin
  {$IFDEF PAS2JS}
  asm
    Valid = typeof V === 'number' && isFinite(V) && Math.floor(V) === V &&
      V >= Minimum && V <= Maximum;
  end;
  {$ELSE}
  Valid := (V >= Minimum) and (V <= Maximum);
  {$ENDIF}
  if not Valid then raise EWfcVolumeSymmetry.Create(Name + ' must be an exact integer in range');
end;

function ShapeCells(const S: TWfcModelSampleShape): Integer;
begin
  RequireInteger(S.Width, 1, WFC_MODEL_MAX_SAMPLE_DIMENSION, 'volume width');
  RequireInteger(S.Height, 1, WFC_MODEL_MAX_SAMPLE_DIMENSION, 'volume height');
  RequireInteger(S.Depth, 1, WFC_MODEL_MAX_SAMPLE_DIMENSION, 'volume depth');
  if S.Width > WFC_MODEL_MAX_SAMPLE_CELL_COUNT div S.Height then
    raise EWfcVolumeSymmetry.Create('volume cells exceed the supported sample envelope');
  Result := S.Width * S.Height;
  if Result > WFC_MODEL_MAX_SAMPLE_CELL_COUNT div S.Depth then
    raise EWfcVolumeSymmetry.Create('volume cells exceed the supported sample envelope');
  Result := Result * S.Depth;
end;

procedure ValidateTransform(const T: TWfcVolumeTransform);
begin
  RequireInteger(T.AxisX, 0, 2, 'transform X axis');
  RequireInteger(T.AxisY, 0, 2, 'transform Y axis');
  RequireInteger(T.AxisZ, 0, 2, 'transform Z axis');
  if (T.AxisX = T.AxisY) or (T.AxisX = T.AxisZ) or (T.AxisY = T.AxisZ) then
    raise EWfcVolumeSymmetry.Create('transform axes must be a permutation');
  RequireInteger(T.SignX, -1, 1, 'transform X sign');
  RequireInteger(T.SignY, -1, 1, 'transform Y sign');
  RequireInteger(T.SignZ, -1, 1, 'transform Z sign');
  if (T.SignX = 0) or (T.SignY = 0) or (T.SignZ = 0) then
    raise EWfcVolumeSymmetry.Create('transform signs must be minus or plus one');
end;

function AxisSize(const S: TWfcModelSampleShape; const Axis: Integer): Integer;
begin
  case Axis of 0: Result := S.Width; 1: Result := S.Height;
    2: Result := S.Depth; else raise EWfcVolumeSymmetry.Create('invalid source axis'); end;
end;

function WfcVolumeTransformCount(const ASymmetry: TWfcModelSymmetry): Integer;
begin
  RequireInteger(Ord(ASymmetry), Ord(wmsNone), Ord(wmsCubeFull), 'volume symmetry');
  case ASymmetry of
    wmsNone: Result := 1;
    wmsD4: Result := 8;
    wmsCubeRotations: Result := 24;
    wmsCubeFull: Result := 48;
    else raise EWfcVolumeSymmetry.Create('unknown volume symmetry');
  end;
end;

function WfcVolumeTransformAt(const ASymmetry: TWfcModelSymmetry;
  const AIndex: Integer): TWfcVolumeTransform;
const Permutations: array[0..5, 0..2] of Integer =
  ((0,1,2),(0,2,1),(1,0,2),(1,2,0),(2,0,1),(2,1,0));
var P, Mask, Count, Determinant, I, J, Limit: Integer;
begin
  Limit := WfcVolumeTransformCount(ASymmetry);
  RequireInteger(AIndex, 0, Limit - 1, 'volume transform index');
  Count := 0;
  for P := 0 to 5 do
  begin
    if (ASymmetry in [wmsNone, wmsD4]) and (P <> 0) and (P <> 2) then Continue;
    for Mask := 0 to 7 do
    begin
      if (ASymmetry = wmsNone) and ((P <> 0) or (Mask <> 0)) then Continue;
      if (ASymmetry = wmsD4) and (Mask >= 4) then Continue;
      Determinant := 1;
      for I := 0 to 2 do for J := I + 1 to 2 do
        if Permutations[P,I] > Permutations[P,J] then Determinant := -Determinant;
      for I := 0 to 2 do if (Mask and (1 shl I)) <> 0 then Determinant := -Determinant;
      if (ASymmetry = wmsCubeRotations) and (Determinant <> 1) then Continue;
      if Count = AIndex then
      begin
        Result.AxisX := Permutations[P,0]; Result.AxisY := Permutations[P,1];
        Result.AxisZ := Permutations[P,2];
        Result.SignX := 1; Result.SignY := 1; Result.SignZ := 1;
        if Mask and 1 <> 0 then Result.SignX := -1;
        if Mask and 2 <> 0 then Result.SignY := -1;
        if Mask and 4 <> 0 then Result.SignZ := -1;
        Exit;
      end;
      Inc(Count);
    end;
  end;
  raise EWfcVolumeSymmetry.Create('volume transform enumeration is incomplete');
end;

function WfcTransformVolumeShape(const ASourceShape: TWfcModelSampleShape;
  const ATransform: TWfcVolumeTransform): TWfcModelSampleShape;
begin
  ShapeCells(ASourceShape); ValidateTransform(ATransform);
  Result.Width := AxisSize(ASourceShape, ATransform.AxisX);
  Result.Height := AxisSize(ASourceShape, ATransform.AxisY);
  Result.Depth := AxisSize(ASourceShape, ATransform.AxisZ);
end;

procedure ToSource(const S: TWfcModelSampleShape; const T: TWfcVolumeTransform;
  const X, Y, Z: Integer; out SX, SY, SZ: Integer); inline;
var C: array[0..2] of Integer;
begin
  if T.SignX > 0 then C[T.AxisX] := X else C[T.AxisX] := AxisSize(S,T.AxisX) - 1 - X;
  if T.SignY > 0 then C[T.AxisY] := Y else C[T.AxisY] := AxisSize(S,T.AxisY) - 1 - Y;
  if T.SignZ > 0 then C[T.AxisZ] := Z else C[T.AxisZ] := AxisSize(S,T.AxisZ) - 1 - Z;
  SX := C[0]; SY := C[1]; SZ := C[2];
end;

procedure WfcVolumeTransformToSource(const ASourceShape: TWfcModelSampleShape;
  const ATransform: TWfcVolumeTransform; const AX, AY, AZ: Integer;
  out ASourceX, ASourceY, ASourceZ: Integer);
var S: TWfcModelSampleShape;
begin
  S := WfcTransformVolumeShape(ASourceShape, ATransform);
  RequireInteger(AX, 0, S.Width - 1, 'transformed X');
  RequireInteger(AY, 0, S.Height - 1, 'transformed Y');
  RequireInteger(AZ, 0, S.Depth - 1, 'transformed Z');
  ToSource(ASourceShape, ATransform, AX, AY, AZ, ASourceX, ASourceY, ASourceZ);
end;

function WfcTransformVolumeIntegers(const AValues: TWfcModelIntegerArray;
  const ASourceShape: TWfcModelSampleShape;
  const ATransform: TWfcVolumeTransform): TWfcModelIntegerArray;
var S: TWfcModelSampleShape; N, I, X, Y, Z, SX, SY, SZ: Integer;
begin
  S := WfcTransformVolumeShape(ASourceShape, ATransform); N := ShapeCells(S);
  if Length(AValues) <> N then raise EWfcVolumeSymmetry.Create('integer volume length differs from shape');
  {$IFDEF PAS2JS}
  for I := 0 to N - 1 do RequireInteger(AValues[I], Low(Integer), High(Integer), 'volume value');
  {$ENDIF}
  Result := nil; SetLength(Result, N); I := 0;
  for Z := 0 to S.Depth - 1 do for Y := 0 to S.Height - 1 do for X := 0 to S.Width - 1 do
  begin
    ToSource(ASourceShape, ATransform, X, Y, Z, SX, SY, SZ);
    Result[I] := AValues[(SZ * ASourceShape.Height + SY) * ASourceShape.Width + SX]; Inc(I);
  end;
end;

function WfcTransformVolumeTokens(const ATokens: TWfcModelTokens;
  const ASourceShape: TWfcModelSampleShape;
  const ATransform: TWfcVolumeTransform): TWfcModelTokens;
var S: TWfcModelSampleShape; N, I, X, Y, Z, SX, SY, SZ: Integer;
  {$IFDEF PAS2JS}TokenIsString: Boolean;{$ENDIF}
begin
  S := WfcTransformVolumeShape(ASourceShape, ATransform); N := ShapeCells(S);
  if Length(ATokens) <> N then raise EWfcVolumeSymmetry.Create('token volume length differs from shape');
  for I := 0 to N - 1 do
  begin
    {$IFDEF PAS2JS}
    asm TokenIsString = typeof ATokens[I] === 'string'; end;
    if not TokenIsString then raise EWfcVolumeSymmetry.Create('volume token must be a string');
    {$ENDIF}
    if not WfcModelTokenIsValid(ATokens[I]) then
      raise EWfcVolumeSymmetry.Create('volume token is not a nonempty Unicode scalar string');
  end;
  Result := nil; SetLength(Result, N); I := 0;
  for Z := 0 to S.Depth - 1 do for Y := 0 to S.Height - 1 do for X := 0 to S.Width - 1 do
  begin
    ToSource(ASourceShape, ATransform, X, Y, Z, SX, SY, SZ);
    Result[I] := ATokens[(SZ * ASourceShape.Height + SY) * ASourceShape.Width + SX]; Inc(I);
  end;
end;

end.
