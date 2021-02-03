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
  math,
  crt, //colors for console
  wfc; //library code

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

      if LVal = 'L' then
        TextColor(Green)
      else if LVal = 'S' then
        TextColor(Cyan)
      else if LVal = 'C' then
        TextColor(Yellow)
      else if LVal = 'M' then
        TextColor(Brown);

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
  //just use land or sea when no other solution
  AValue := TArray<String>.Create('L', 'S')[RandomRange(0, 2)];
end;

var
  LWorld : TGraph;
begin
  LWorld := TGraph.Create;
  try
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
    LWorld.Run;

    //now call our helper print function to display the world
    RenderWorld(LWorld);
  finally
    LWorld.Free;
  end;

  //wait for user to close
  ReadLn;
end.

