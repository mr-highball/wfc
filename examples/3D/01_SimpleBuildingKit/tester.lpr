{$codepage utf8}
program tester;
uses
  math,
  crt,
  wfc,
  wfc.buildkit
  {$IFDEF WINDOWS}
  ,windows
  {$ENDIF};

procedure RenderWorld(const AWorld : TGraph);
var
  X, Y: Integer;
  LVal: TGraphValue;
begin
  {$IFDEF WINDOWS}SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  WriteLn('');

  //render from 'top' to 'bottom' for correct positioning
  for Y := Pred(AWorld.Dimension.Height) downto 0 do
  begin
    for X := 0 to Pred(AWorld.Dimension.Width) do
    begin
      LVal := AWorld[X, Y, 0].Value;

      if LVal = KIT_GR_000 then
      begin
        TextColor(Green);
        LVal := '▓';
      end
      else if (Pos('w_000', LowerCase(LVal))) or (Pos('w_180', LowerCase(LVal))) > 0 then
      begin
        TextColor(LightGray);
        LVal := '─';
      end
      else if (Pos('w_090', LowerCase(LVal))) or (Pos('w_270', LowerCase(LVal))) > 0  then
      begin
        TextColor(LightGray);
        LVal := '│';
      end
      else if Pos('r_000', LowerCase(LVal)) > 0 then
      begin
        TextColor(DarkGray);
        LVal := '┘';
      end
      else if Pos('r_090', LowerCase(LVal)) > 0 then
      begin
        TextColor(DarkGray);
        LVal := '┐';
      end
      else if Pos('r_180', LowerCase(LVal)) > 0 then
      begin
        TextColor(DarkGray);
        LVal := '*';//'┌';
      end
      else if Pos('r_180', LowerCase(LVal)) > 0 then
      begin
        TextColor(DarkGray);
        LVal := '└';
      end
      else if LVal = KIT_RF_000 then
      begin
        TextColor(Brown);
        LVal := '≡';
      end
      else
        LVal := ' ';

      Write(LVal);
    end;
    WriteLn('');
  end;
end;

procedure TestBuildKit;
var
  LGraph: TGraph;
begin
  LGraph := NewBuildKitGraph(30, 10, 1);
  LGraph.Run;
  RenderWorld(LGraph);
  LGraph.Free;
end;

begin
  TestBuildKit;
  ReadLn;
end.

