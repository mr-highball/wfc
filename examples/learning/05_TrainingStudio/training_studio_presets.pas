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
unit training_studio_presets;

{$mode delphi}{$H+}

interface

uses wfc_training_workspace;

const
  TRAINING_STUDIO_PRESET_COUNT = 5;

function TrainingStudioPresetName(const AIndex: Integer): String;
function TrainingStudioPresetText(const AIndex: Integer): String;
function TrainingStudioPresetOptions(
  const AIndex: Integer): TWfcTrainingSolveOptions;

implementation

uses SysUtils, wfc_training, wfc_training_text, wfc_text_training;

const
  PRESET_0 =
    'wfclearn=1'#10 +
    'name=alternating-sequences'#10 +
    'license=MIT'#10 +
    'source=project-authored%20training%20example'#10 +
    'kind=adjacency1d'#10 +
    'boundary=wrap'#10 +
    'symmetry=none'#10 +
    'footprint=0,0'#10 +
    'order=0'#10 +
    'samples=2'#10 +
    'sample=0,4,1,forward'#10 +
    'token=0,0,A'#10 +
    'token=0,1,B'#10 +
    'token=0,2,A'#10 +
    'token=0,3,B'#10 +
    'sample=1,4,1,reverse'#10 +
    'token=1,0,B'#10 +
    'token=1,1,A'#10 +
    'token=1,2,B'#10 +
    'token=1,3,A'#10 +
    'end'#10;
  PRESET_1 =
    'wfclearn=1'#10 +
    'name=checkerboard-adjacency'#10 +
    'license=MIT'#10 +
    'source=project-authored%20training%20example'#10 +
    'kind=adjacency2d'#10 +
    'boundary=wrap'#10 +
    'symmetry=d4'#10 +
    'footprint=0,0'#10 +
    'order=0'#10 +
    'samples=2'#10 +
    'sample=0,2,2,square'#10 +
    'token=0,0,A'#10 +
    'token=0,1,B'#10 +
    'token=0,2,B'#10 +
    'token=0,3,A'#10 +
    'sample=1,4,2,rectangle'#10 +
    'token=1,0,A'#10 +
    'token=1,1,B'#10 +
    'token=1,2,A'#10 +
    'token=1,3,B'#10 +
    'token=1,4,B'#10 +
    'token=1,5,A'#10 +
    'token=1,6,B'#10 +
    'token=1,7,A'#10 +
    'end'#10;
  PRESET_2 =
    'wfclearn=1'#10 +
    'name=checkerboard-patterns'#10 +
    'license=MIT'#10 +
    'source=project-authored%20training%20example'#10 +
    'kind=pattern2d'#10 +
    'boundary=wrap'#10 +
    'symmetry=d4'#10 +
    'footprint=2,2'#10 +
    'order=0'#10 +
    'samples=2'#10 +
    'sample=0,2,2,square'#10 +
    'token=0,0,A'#10 +
    'token=0,1,B'#10 +
    'token=0,2,B'#10 +
    'token=0,3,A'#10 +
    'sample=1,4,2,rectangle'#10 +
    'token=1,0,A'#10 +
    'token=1,1,B'#10 +
    'token=1,2,A'#10 +
    'token=1,3,B'#10 +
    'token=1,4,B'#10 +
    'token=1,5,A'#10 +
    'token=1,6,B'#10 +
    'token=1,7,A'#10 +
    'end'#10;
  PRESET_3 =
    'wfclearn=1'#10 +
    'name=short-phrases'#10 +
    'license=MIT'#10 +
    'source=project-authored%20training%20example'#10 +
    'kind=sequence'#10 +
    'boundary=open'#10 +
    'symmetry=none'#10 +
    'footprint=0,0'#10 +
    'order=2'#10 +
    'samples=2'#10 +
    'sample=0,3,1,first'#10 +
    'token=0,0,red'#10 +
    'token=0,1,fox'#10 +
    'token=0,2,.'#10 +
    'sample=1,3,1,second'#10 +
    'token=1,0,caf%C3%A9'#10 +
    'token=1,1,fox'#10 +
    'token=1,2,.'#10 +
    'end'#10;

procedure CheckIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= TRAINING_STUDIO_PRESET_COUNT) then
    raise ERangeError.Create('unknown training studio preset');
end;

function TrainingStudioPresetName(const AIndex: Integer): String;
begin
  CheckIndex(AIndex);
  case AIndex of
    0: Result := 'Alternating rows';
    1: Result := 'Cardinal checkerboard';
    2: Result := 'Overlapping checkerboard';
    3: Result := 'Whole token phrases';
    4: Result := 'Raw text / Unicode scalars';
  end;
end;

function TrainingStudioPresetText(const AIndex: Integer): String;
var
  LSamples: TWfcTextTrainingSamples;
  LDocument: TWfcTrainingDocument;
begin
  CheckIndex(AIndex);
  case AIndex of
    0: Exit(PRESET_0);
    1: Exit(PRESET_1);
    2: Exit(PRESET_2);
    3: Exit(PRESET_3);
  end;
  SetLength(LSamples, 2);
  LSamples[0] := MakeWfcTextTrainingSample('first', 'a cat.');
  LSamples[1] := MakeWfcTextTrainingSample('second', 'a bat.');
  LDocument := BuildWfcTextTrainingDocument(
    MakeWfcTrainingMetadata('scalar-phrases', 'MIT',
      'project-authored training studio example'), LSamples, 3);
  try
    Result := EncodeWfcTrainingText(LDocument);
  finally
    LDocument.Free;
  end;
end;

function TrainingStudioPresetOptions(
  const AIndex: Integer): TWfcTrainingSolveOptions;
begin
  CheckIndex(AIndex);
  Result := DefaultWfcTrainingSolveOptions;
  case AIndex of
    0: begin Result.Width := 8; Result.Height := 1 end;
    3: begin Result.Width := 3; Result.Height := 1 end;
    4: begin Result.Width := 6; Result.Height := 1 end;
  end;
end;

end.
