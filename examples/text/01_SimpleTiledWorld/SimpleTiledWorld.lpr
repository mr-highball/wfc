{$Mode delphi}

(*
  below example uses the following article describing a simple world
  with Land, Coast, Sea and Mountains that have constraints for
  where they can appear. Because this is a textual example we will use
  the first letter for each tile name to represent the type of tile used.

  https://robertheaton.com/2018/12/17/wavefunction-collapse-algorithm/
*)
program SimpleTiledWorld;
uses
  SysUtils,
  crt, //colors for console
  wfc; //library code

procedure SetTileColor(const AValue: TGraphValue);
begin
  if AValue = 'L' then
    TextColor(Green)
  else if AValue = 'S' then
    TextColor(Cyan)
  else if AValue = 'C' then
    TextColor(Yellow)
  else if AValue = 'M' then
    TextColor(Brown);
end;

procedure RenderWorld(const AWorld : TGraph);
var
  X, Y: Integer;
  LVal: TGraphValue;
begin
  WriteLn('');

  for Y := 0 to Pred(AWorld.Dimension.Height) do
  begin
    for X := 0 to Pred(AWorld.Dimension.Width) do
    begin
      LVal := AWorld[X, Y, 0].Value;

      SetTileColor(LVal);
      Write(LVal);
    end;
    WriteLn('');
  end;
end;

var
  LWorld : TGraph;
begin
  LWorld := TGraph.Create;
  try
    //An optional decimal or Pascal-style hexadecimal seed replays a world.
    if ParamCount > 0 then
      LWorld.Seed := TGraphSeed(StrToQWord(ParamStr(1)));

    //set our shape to be 2D and size it appropriately for the console window
    LWorld.Reshape({width} 80, {height} 25, {depth} 1);
    //LWorld.WrapNeighbors := False;

    //Define a complete symmetric adjacency model. Every tile has at least one
    //self-compatible state, so impossible boards are reported rather than
    //hidden by an invalid-state fallback that invents a value.
    LWorld.AddValue('L')
      .NewRule(AllDirections, ['L', 'C', 'M']);
    LWorld.AddValue('C')
      .NewRule(AllDirections, ['L', 'C', 'S', 'M']);
    LWorld.AddValue('S')
      .NewRule(AllDirections, ['S', 'C']);
    LWorld.AddValue('M')
      .NewRule(AllDirections, ['M', 'L', 'C']);

    //run the graph
    WriteLn('Seed: ', LWorld.Seed);
    LWorld.Run;

    //now call our helper print function to display the world
    RenderWorld(LWorld);
  finally
    LWorld.Free;
  end;

  //An explicit seed also acts as a noninteractive native smoke-test mode.
  if ParamCount = 0 then
    ReadLn;
end.
