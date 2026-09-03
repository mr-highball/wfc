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
  {$IFDEF PAS2JS}
  NodeJSApp, //installs ParamCount/ParamStr from Node's process.argv
  {$ENDIF}
  {$IFNDEF PAS2JS}
  crt, //colors for console
  {$ENDIF}
  wfc; //library code

procedure SetTileColor(const AValue: TGraphValue);
begin
  {$IFNDEF PAS2JS}
  if AValue = 'L' then
    TextColor(Green)
  else if AValue = 'S' then
    TextColor(Cyan)
  else if AValue = 'C' then
    TextColor(Yellow)
  else if AValue = 'M' then
    TextColor(Brown);
  {$ENDIF}
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

(*
  the rules we have in place allow for invalid board states, so we could
  either fix the rules to handle all cases, or provide a default with this
*)
procedure InvalidHandler(const AGraph : TGraph; const AEntry : TGraphEntry;
  var AValue : TGraphValue);
begin
  //Use the graph-owned stream so an explicit Seed replays this recovery too.
  if AGraph.RandomIndex(2) = 0 then
    AValue := 'L'
  else
    AValue := 'S';
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

    LWorld.InvalidStateCallback := InvalidHandler;

    //"coast" can have "sea" to the right (east)
    //and "land" to the left (west)
    LWorld.AddValue('C')
      .NewRule([gdEast], 'S')
      .NewRule([gdWest], 'L');

    //"sea" can go next to other sea tile
    LWorld.Rules['S']
      .NewRule(AllDirections, 'S');

    //"land" can be next to other land
    LWorld.Rules['L']
      .NewRule(AllDirections, 'L');

    //"mountain" isn't really defined on the article even though
    //the tile is there, so we'll just say it needs be to the west of cost
    //or west/east of another mountain and east of land
    LWorld.AddValue('M')
      .NewRule([gdEast, gdWest], 'L')
      .NewRule([gdEast], 'C')
      .NewRule(AllDirections, 'M');

    //run the graph
    WriteLn('Seed: ', LWorld.Seed);
    LWorld.Run;

    //now call our helper print function to display the world
    RenderWorld(LWorld);
  finally
    LWorld.Free;
  end;

  {$IFNDEF PAS2JS}
  //An explicit seed also acts as a noninteractive native smoke-test mode.
  if ParamCount = 0 then
    ReadLn;
  {$ENDIF}
end.

