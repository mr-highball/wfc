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
program wfc_terraces3d_test;
{$mode delphi}{$H+}
uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  Classes, SysUtils, wfc, wfc_terraces3d;

type
  TRejectingTerraces = class(TTerraces3D)
  public
    RejectCandidate, RaiseCandidate: Boolean;
    Accepted: Integer;
  strict protected
    function DoAcceptCandidate(const AScene: TTerraces3DScene): Boolean; override;
  end;

var Checks, Failures: Integer;

function TRejectingTerraces.DoAcceptCandidate(const AScene: TTerraces3DScene): Boolean;
begin
  Inc(Accepted);
  if RaiseCandidate then raise EInvalidOperation.Create('test candidate exception');
  Result := not RejectCandidate;
end;

procedure Check(const Condition: Boolean; const Message: String);
begin
  Inc(Checks);
  if not Condition then begin Inc(Failures); WriteLn('FAIL: ',Message); end;
end;

function SameTerrain(const A, B: TTerraces3DScene): Boolean;
var X,Y,Z: Integer;
begin
  Result := False;
  if (A.Width <> B.Width) or (A.Height <> B.Height) or (A.Depth <> B.Depth) then Exit;
  for Z := 0 to A.Depth-1 do for Y := 0 to A.Height-1 do for X := 0 to A.Width-1 do
    if A.TerrainAt(X,Y,Z) <> B.TerrainAt(X,Y,Z) then Exit;
  Result := True;
end;

procedure TestComposition;
var
  Owner, Replay: TTerraces3D;
  A,B,C: TTerraces3DScene;
  Report: TGraphNegotiationReport;
  Message, Before, First: String;
  Seed: Integer;
  Options: TGraphNegotiationOptions;
begin
  Options := DefaultTerraces3DOptions;
  First := '';
  for Seed := 0 to 5 do
  begin
    Owner := TTerraces3D.Create(6,5,5,Seed);
    Replay := TTerraces3D.Create(6,5,5,Seed);
    A := nil; B := nil; C := nil;
    try
      Check(Owner.Model.Rank = 3,'uses full volume model');
      Check(Owner.Model.SampleCount = 2,'ordered independent training corpus');
      Check(Owner.TryGenerate(Options,A,Report),'learned terrace baseline seed '+IntToStr(Seed));
      if not Assigned(A) then Continue;
      Check(Owner.Validate(A,Message),'independent baseline validation '+Message);
      Before := A.Signature;
      WriteLn('TERRACES_SEED_',Seed,'=',Before);
      if Seed = 0 then First := Before
      else if Seed = 1 then Check(Before <> First,'seed changes generated composition');
      Check(Replay.TryGenerate(Options,B,Report),'same-seed replay');
      if Assigned(B) then Check(B.Signature = Before,'portable replay signature');
      Check(Owner.TryRegenerateFrom(t3sFoliage,Options,C,Report),'foliage-only regeneration');
      if Assigned(C) then
      begin
        Check(SameTerrain(A,C),'foliage selective preserves learned provider');
        Check(A.Structure.Signature = C.Structure.Signature,'foliage selective preserves exact kit provider');
        Check(Owner.Validate(C,Message),'selective independently valid');
      end;
      Check(A.Signature = Before,'later solves do not alias detached output');
      FreeAndNil(C);
      Check(Owner.TryRegenerateFrom(t3sStructure,Options,C,Report),'structure + foliage regeneration');
      if Assigned(C) then Check(SameTerrain(A,C),'structure selective preserves terrain');
    finally C.Free; B.Free; A.Free; Replay.Free; Owner.Free; end;
  end;
end;

procedure TestRejectionAndRecovery;
var
  Owner, Control: TRejectingTerraces;
  A,B,C,D: TTerraces3DScene;
  R: TGraphNegotiationReport;
  Options: TGraphNegotiationOptions;
  Raised: Boolean;
begin
  Owner := TRejectingTerraces.Create(3,3,5,55);
  Control := TRejectingTerraces.Create(3,3,5,55);
  A := nil; B := nil; C := nil; D := nil;
  Options := DefaultTerraces3DOptions;
  try
    Check(Owner.TryGenerate(Options,A,R),'policy fixture baseline');
    Check(Control.TryGenerate(Options,B,R),'policy control baseline');
    Owner.RejectCandidate := True;
    Options.MaxPassBacktracks := 0;
    Check(not Owner.TryRegenerateFrom(t3sFoliage,Options,C,R),'late rejection rolls back');
    Check(not Assigned(C),'late rejection publishes no scene');
    Check(Owner.ValidationMessage <> '','late rejection has public diagnostic');
    Check(Owner.HasBaseline and (Owner.DirtyStage = -1),'clean baseline survives rejected candidate');
    Owner.RejectCandidate := False; Owner.RaiseCandidate := True; Raised := False;
    try Owner.TryRegenerateFrom(t3sFoliage,Options,C,R);
    except on E: EInvalidOperation do Raised := True; end;
    Check(Raised and not Assigned(C),'late exception publishes no scene');
    Owner.RaiseCandidate := False;
    Check(Owner.TryRegenerateFrom(t3sFoliage,Options,C,R),'retry after rejection and exception');
    Check(Control.TryRegenerateFrom(t3sFoliage,Options,D,R),'idle control selective');
    if Assigned(C) and Assigned(D) then Check(C.Signature = D.Signature,'entry and RNG rollback match idle control');
    Check(Owner.Accepted >= 4,'application hook executes at rollback boundary');
    Check(A.Signature = B.Signature,'detached prior snapshots unaffected');
    FreeAndNil(C); FreeAndNil(D);
    Owner.SetFoliage(0,0,0,'none'); Control.SetFoliage(0,0,0,'none');
    Owner.RejectCandidate := True;
    Check(not Owner.TryRegenerateFrom(t3sFoliage,Options,C,R),'dirty valid edit rejected by final hook');
    Check(not Assigned(C) and Owner.HasBaseline and (Owner.DirtyStage = 2),
      'final rejection retains dirty edit and baseline');
    Owner.RejectCandidate := False; Owner.RaiseCandidate := True; Raised := False;
    try Owner.TryRegenerateFrom(t3sFoliage,Options,C,R);
    except on E: EInvalidOperation do Raised := True; end;
    Check(Raised and not Assigned(C) and Owner.HasBaseline and (Owner.DirtyStage = 2),
      'final exception retains dirty edit and baseline');
    Owner.RaiseCandidate := False;
    Check(Owner.TryRegenerateFrom(t3sFoliage,Options,C,R),'dirty edit retry after final hook failures');
    Check(Control.TryRegenerateFrom(t3sFoliage,Options,D,R),'dirty edit idle control');
    if Assigned(C) and Assigned(D) then Check(C.Signature = D.Signature,
      'dirty edit rollback retains exact random streams');
  finally D.Free; C.Free; B.Free; A.Free; Control.Free; Owner.Free; end;
end;

procedure TestNegotiation;
var Owner: TTerraces3D; A: TTerraces3DScene;
  R: TGraphNegotiationReport; Options: TGraphNegotiationOptions;
  Seed,I,J: Integer; Reopened: Boolean;
begin
  Reopened := False; Options := DefaultTerraces3DOptions;
  Options.SolveOptions.CaptureTrace := True;
  for Seed := 0 to 7 do
  begin
    Owner := TTerraces3D.Create(1,1,4,Seed); A := nil;
    try
      Owner.SetFoliage(0,0,2,'flowers');
      Check(Owner.TryGenerate(Options,A,R),'negotiated plant request '+IntToStr(Seed));
      if Assigned(A) then
      begin
        Check(A.TerrainAt(0,0,1) = 'soil','plant request constrains earlier terrain height');
        Check(A.Structure.VariantAt(A.Structure.VariantIndexAt(0,0,1)).PrototypeId = 'flowerbed',
          'plant request constrains earlier surface variant');
      end;
      if R.PassBacktracks > 0 then Reopened := True;
      for I := 0 to High(R.FinalReport.Trace) do
        Check(Pos('@',R.FinalReport.Trace[I].Value) = 0,'final trace has public values');
      for I := 0 to High(R.Attempts) do
        for J := 0 to High(R.Attempts[I].SolveReport.Trace) do
          Check(Pos('@',R.Attempts[I].SolveReport.Trace[J].Value) = 0,'rejected round trace has public values');
    finally A.Free; Owner.Free; end;
  end;
  Check(Reopened,'downstream plants actually reopen an earlier assignment');
end;

procedure TestInputPolicy;
var Owner: TTerraces3D; A,B: TTerraces3DScene;
  R: TGraphNegotiationReport; Options: TGraphNegotiationOptions; Raised: Boolean;
begin
  Owner := TTerraces3D.Create(2,2,4,12); A := nil; B := nil;
  Options := DefaultTerraces3DOptions;
  try
    Raised := False;
    try Owner.TryRegenerateFrom(t3sFoliage,Options,A,R);
    except on E: EInvalidOperation do Raised := True; end;
    Check(Raised and not Assigned(A),'new session needs baseline for selective only');
    Check(Owner.TryGenerate(Options,A,R),'new session full generation always available');
    Owner.SetTerrainToken(0,0,1,'soil');
    Raised := False;
    try Owner.TryRegenerateFrom(t3sFoliage,Options,B,R);
    except on E: EInvalidOperation do Raised := True; end;
    Check(Raised and not Assigned(B),'dirty terrain cannot be bypassed by foliage scope');
    Check(Owner.TryRegenerateFrom(t3sTerrain,Options,B,R),'provider-root repair includes descendants');
    if Assigned(B) then Check(B.TerrainAt(0,0,1) = 'soil','requested user terrain domain retained');
    FreeAndNil(B);
    Owner.SetFoliage(0,0,0,'fern');
    Options.MaxPassBacktracks := 0;
    Check(not Owner.TryRegenerateFrom(t3sFoliage,Options,B,R),'plant inside floor contradicts');
    Check(not Assigned(B) and (Owner.DirtyStage = 2),'failed changed-input solve remains dirty');
    Owner.SetFoliage(0,0,0,'');
    Check(Owner.TryRegenerateFrom(t3sFoliage,Options,B,R),'clearing invalid plant recovers');
    Raised := False;
    try Owner.SetTerrainToken(0,0,0,'air');
    except on E: ETerraces3D do Raised := True; end;
    Check(Raised and (Owner.DirtyStage = -1),'invalid boundary edit is atomic');
  finally B.Free; A.Free; Owner.Free; end;
end;


{$IFDEF PAS2JS}
procedure TestBrowserNumbers;
var Owner, Bad: TTerraces3D; Scene: TTerraces3DScene;
  R: TGraphNegotiationReport; O: TGraphNegotiationOptions;
  BadSeed: TGraphSeed; Axis, I: Integer; Raised: Boolean;
begin
  Owner := TTerraces3D.Create(1,1,3,0); Scene := nil;
  try
    Check(Owner.TryGenerate(DefaultTerraces3DOptions,Scene,R),'numeric guard baseline');
    FreeAndNil(Scene);
    for I := 0 to 2 do
    begin
      BadSeed := 0;
      case I of
        0: asm BadSeed = 1.5; end;
        1: asm BadSeed = -1; end;
        2: asm BadSeed = 4294967296; end;
      end;
      Raised := False;
      try Owner.SetSeed(BadSeed); except on E: ERangeError do Raised := True; end;
      Check(Raised and Owner.HasBaseline and (Owner.DirtyStage = -1),'invalid seed setter is atomic');
      Bad := nil; Raised := False;
      try Bad := TTerraces3D.Create(1,1,3,BadSeed);
      except on E: ERangeError do Raised := True; end;
      Check(Raised and not Assigned(Bad),'invalid constructor seed rejected before graph allocation');
      Bad.Free;
    end;
    for I := 0 to 1 do
    begin
      O := DefaultTerraces3DOptions;
      if I = 0 then asm O.SolveOptions.MaxBacktracks = 1.5; end
      else asm O.MaxPassBacktracks = 1.5; end;
      Raised := False;
      try Owner.TryRegenerateFrom(t3sFoliage,O,Scene,R);
      except on E: ERangeError do Raised := True; end;
      Check(Raised and not Assigned(Scene) and Owner.HasBaseline and (Owner.DirtyStage = -1),
        'fractional search budget cannot consume a transaction');
    end;
    Axis := 1; asm Axis = 1.5; end;
    Bad := nil; Raised := False;
    try Bad := TTerraces3D.Create(Axis,1,3,0);
    except on E: ERangeError do Raised := True; end;
    Check(Raised and not Assigned(Bad),'fractional shape rejected');
    Bad.Free;
    Raised := False;
    try Owner.SetTerrainToken(0,0,Axis,'soil');
    except on E: ERangeError do Raised := True; end;
    Check(Raised and (Owner.DirtyStage = -1),'fractional coordinate rejected atomically');
  finally Scene.Free; Owner.Free; end;
end;
{$ENDIF}

begin
  try
    {$IFDEF PAS2JS}TestBrowserNumbers;{$ENDIF}
    TestComposition;
    TestRejectionAndRecovery;
    TestInputPolicy;
    TestNegotiation;
  except on E: Exception do
    begin Inc(Failures); WriteLn('UNEXPECTED: ',E.ClassName,': ',E.Message); end;
  end;
  WriteLn('checks=',Checks,' failures=',Failures);
  if Failures <> 0 then Halt(1);
end.
