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
unit wfc_solve_progress;

{$mode delphi}{$H+}

interface

type
  { Phase-local owned visits. Search/validation work is data dependent; it is
    not a percentage of a solution and restarts for each pass/attempt. }
  TWfcSolvePhase = (wspPrepare, wspSearch, wspCommit);
  { PassIndex=-1 denotes pipeline work. Total is meaningful only when KnownTotal
    is true. Counts are exact on native and JavaScript within the 2^53-1 limit. }
  TWfcSolveProgress = record
    Phase: TWfcSolvePhase;
    PassIndex: Integer;
    Done: Int64;
    Total: Int64;
    KnownTotal: Boolean;
  end;
  { Borrowed synchronously for one public solve or assignment call. Exceptions abort its
    transaction; do not reenter/mutate/destroy the graph or draw its randomness.
    Assignment capture/restore reports structural preparation/commit, never search.
    This is separate from diagnostic tracing, whose receiver failures are contained. }
  TWfcSolveObserver = procedure(const AProgress: TWfcSolveProgress) of object;
  { Shared graph/kernel visit accounting. Internal call-scoped helper; destruction
    never invokes the observer. No scope is allocated for a nil observer. }
  TWfcSolveWork = class
  private
    FObserver: TWfcSolveObserver;
    FProgress: TWfcSolveProgress;
  public
    constructor Create(const AObserver: TWfcSolveObserver);
    procedure BeginPhase(const APhase: TWfcSolvePhase; const APassIndex: Integer);
    procedure Visit;
    procedure Finish;
  end;

procedure BeginSolveWork(const AWork: TWfcSolveWork; const APhase: TWfcSolvePhase;
  const APassIndex: Integer); inline;
procedure VisitSolveWork(const AWork: TWfcSolveWork); inline;
procedure FinishSolveWork(const AWork: TWfcSolveWork); inline;

implementation

uses SysUtils;

constructor TWfcSolveWork.Create(const AObserver: TWfcSolveObserver);
begin
  inherited Create;
  FObserver := AObserver;
end;

procedure TWfcSolveWork.BeginPhase(const APhase: TWfcSolvePhase;
  const APassIndex: Integer);
begin
  FProgress := Default(TWfcSolveProgress);
  FProgress.Phase := APhase;
  FProgress.PassIndex := APassIndex;
  FObserver(FProgress);
end;

procedure TWfcSolveWork.Visit;
begin
  if FProgress.Done >= 9007199254740991 then
    raise ERangeError.Create('Solver work count exceeds exact portable integers');
  Inc(FProgress.Done);
  if (FProgress.Done mod 256) = 0 then
    FObserver(FProgress);
end;

procedure TWfcSolveWork.Finish;
begin
  FProgress.Total := FProgress.Done;
  FProgress.KnownTotal := True;
  FObserver(FProgress);
end;

procedure BeginSolveWork(const AWork: TWfcSolveWork; const APhase: TWfcSolvePhase;
  const APassIndex: Integer);
begin
  if AWork <> nil then AWork.BeginPhase(APhase, APassIndex);
end;

procedure VisitSolveWork(const AWork: TWfcSolveWork);
begin
  if AWork <> nil then AWork.Visit;
end;

procedure FinishSolveWork(const AWork: TWfcSolveWork);
begin
  if AWork <> nil then AWork.Finish;
end;

end.
