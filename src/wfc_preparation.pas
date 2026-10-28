(*
MIT License

Copyright (c) 2021, 2026 mr-highball

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
unit wfc_preparation;

{$mode delphi}
{$H+}

interface

type
  { Actual marked visits or named rows/cells, never an overall percentage.
    Total0 means unknown or empty. PassIndex-1 is operation-wide work.
    Borrowed synchronously; the observer must not mutate inputs or reenter.
    Raise to abort. No observer is installed in a returned model or graph. }
  TWfcPreparationProgress = record
    Stage: String;
    Done: Int64;
    Total: Int64;
    PassIndex: Integer;
  end;
  TWfcPreparationObserver = procedure(const AProgress: TWfcPreparationProgress) of object;

  { One call-scoped receiver, also tracking exceptions from nested observers.
    It does not own inputs and makes no transaction or time-limit promise. }
  TWfcPreparationWork = class
  private
    FObserver: TWfcPreparationObserver;
    FObserverFailed: Boolean;
    FStage: String;
    FVisits: Int64;
    FPassIndex: Integer;
  public
    constructor Create(const AObserver: TWfcPreparationObserver;
      const AStage: String; const APassIndex: Integer = -1);
    procedure Forward(const AProgress: TWfcPreparationProgress); virtual;
    function Observer: TWfcPreparationObserver;
    procedure Report(const AStage: String; const ADone, ATotal: Int64;
      const APassIndex: Integer = -1);
    procedure Checkpoint;
    procedure Visit;
    procedure Finish;
    property ObserverFailed: Boolean read FObserverFailed;
    property Stage: String read FStage;
  end;

function PreparationObserver(const AWork: TWfcPreparationWork): TWfcPreparationObserver;
procedure PreparationVisit(const AWork: TWfcPreparationWork);

implementation

constructor TWfcPreparationWork.Create(const AObserver: TWfcPreparationObserver;
  const AStage: String; const APassIndex: Integer);
begin
  inherited Create;
  FObserver := AObserver;
  FStage := AStage;
  FPassIndex := APassIndex;
end;

procedure TWfcPreparationWork.Forward(const AProgress: TWfcPreparationProgress);
begin
  if not Assigned(FObserver) then
  begin
    Exit;
  end;
  try
    FObserver(AProgress);
  except
    FObserverFailed := True;
    raise;
  end;
end;

function TWfcPreparationWork.Observer: TWfcPreparationObserver;
begin
  Result := nil;
  if Assigned(FObserver) then
  begin
    Result := Forward;
  end;
end;

procedure TWfcPreparationWork.Report(const AStage: String;
  const ADone, ATotal: Int64; const APassIndex: Integer);
var
  LProgress: TWfcPreparationProgress;
begin
  LProgress.Stage := AStage;
  LProgress.Done := ADone;
  LProgress.Total := ATotal;
  LProgress.PassIndex := APassIndex;
  Forward(LProgress);
end;

procedure TWfcPreparationWork.Checkpoint;
begin
  Report(FStage, FVisits, 0, FPassIndex);
end;

procedure TWfcPreparationWork.Visit;
begin
  Inc(FVisits);
  if Assigned(FObserver) and ((FVisits mod 256) = 0) then
  begin
    Checkpoint;
  end;
end;

procedure TWfcPreparationWork.Finish;
begin
  Report(FStage, FVisits, FVisits, FPassIndex);
end;

function PreparationObserver(const AWork: TWfcPreparationWork): TWfcPreparationObserver;
begin
  Result := nil;
  if AWork <> nil then
  begin
    Result := AWork.Observer();
  end;
end;

procedure PreparationVisit(const AWork: TWfcPreparationWork);
begin
  if AWork <> nil then
  begin
    AWork.Visit;
  end;
end;

end.
