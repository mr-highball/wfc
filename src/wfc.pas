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
unit wfc;

{$mode delphi}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections;

type

  //user defined value
  TGraphValue = String;
  TGraphValues = TArray<TGraphValue>;

  //all posible "directions" to move from a single point on the graph
  TGraphDirection = (gdNorth, gdEast, gdSouth, gdWest, gdUp, gdDown);
  TGraphDirections = set of TGraphDirection;

  { TGraphEntry }
  (*
    entry on a graph that holds the current user defined value
  *)
  TGraphEntry = class(TObject)
  strict private
    type
      TNeighbors = TArray<TGraphEntry>;
  strict private
    FEmpty: Boolean;
    FID: String;
    FVal: TGraphValue;
    FNeighbors : TNeighbors;

    function GetNeighbor(const ADirection : TGraphDirection): TGraphEntry;
    procedure SetNeighbor(const ADirection : TGraphDirection;
      const AValue: TGraphEntry);
    procedure SetValue(const AValue: TGraphValue);
  strict protected
    (*
      below methods can be overridden if additional function needs to be
      added to the graph entry class in children
    *)
    procedure DoBeforeSetNeighbor(const {%H-}ANeighbor : TGraphEntry); virtual;
    procedure DoAfterSetNeighbor(const {%H-}ANeighbor : TGraphEntry); virtual;
    procedure DoBeforeSetValue(const {%H-}AValue : TGraphValue); virtual;
    procedure DoAfterSetValue(const {%H-}AValue : TGraphValue); virtual;
    procedure DoReset; virtual;
    function DoGenerateID : String; virtual;
  public
    property Value : TGraphValue read FVal write SetValue;
    property Empty : Boolean read FEmpty;
    property Neighbor[const ADirection : TGraphDirection] : TGraphEntry read GetNeighbor write SetNeighbor; default;
    property ID : String read FID write FID;

    procedure Reset;
    constructor Create; virtual;
  end;

  //collection of entries
  TGraphEntries = TObjectList<TGraphEntry>;

  //for a particular value, what the accepted states are for each direction
  TGraphRule = TPair<TGraphDirection, TGraphValues>;
  TGraphRules = array of TGraphRule;

  { TGraphRuleGroup }
  (*
    class to easily group rules and directions for a specified value
  *)
  TGraphRuleGroup = class(TObject)
  strict private
    FRules: TGraphRules;
    FVal: TGraphValue;
    function GetExists(const ADirection : TGraphDirection): Boolean;
    function GetRule(const ADirection : TGraphDirection): TGraphRule;
  strict protected
    function IndexOfDirection(const ADirection : TGraphDirection) : Integer;

    (*
      can be overridden to handle additional logic for adding new rules
    *)
    procedure DoNewRule(const ADirections : TGraphDirections;
      const AValue : TGraphValue); virtual;
  public
    property Value : TGraphValue read FVal write FVal;
    property Rule[const ADirection : TGraphDirection] : TGraphRule read GetRule; default;
    property Rules : TGraphRules read FRules write FRules;
    property Exists[const ADirection : TGraphDirection] : Boolean read GetExists;

    function NewRule(const ADirections : TGraphDirections;
      const AValue : TGraphValue) : TGraphRuleGroup; overload;

    function NewRule(const ADirections : TGraphDirections;
      const AValues : TGraphValues) : TGraphRuleGroup; overload;

    constructor Create; virtual; overload;
    constructor Create(const AValue : TGraphValue); virtual; overload;
  end;

  //collection of rule groups
  TGraphRuleGroups = TObjectDictionary<TGraphValue, TGraphRuleGroup>;

  //forward
  TGraph = class;

  //callback for entry selection after filtering for only valid values
  TValueSelectionCallback = function(const AGraph : TGraph;
    const AEntry : TGraphEntry;
    const AValid : TGraphValues) : TGraphValue;

  TInvalidStateCallback = procedure(const AGraph : TGraph;
    const AEntry : TGraphEntry; var AValue : TGraphValue);

  (*
    determines the mode for the plane selection process during a graph instance run
  *)
  TGraphRunMode = (rmBottomUp, rmTopDown);

  (*
    a nested callback that is run in place of a for loop
  *)
  TForEachPassNestedCallback = procedure(const AGraph : TGraph;
    const APass : String; const APassIndex : Integer) is nested;

  { TGraph }
  (*
    a graph used to perform the wave function collapse algorithm
  *)
  TGraph = class(TObject)
  public
    type
      X = UInt64;
      Y = UInt64;
      Z = UInt64;
      TPlaneCoord = TPair<X, Y>;
      TPlane = TDictionary<TPlaneCoord, TGraphEntry>;
      TPlanes = TDictionary<Z, TPlane>;
      TPlanesList = TObjectList<TPlanes>;

      TDimension = record
        Width : X;
        Height : Y;
        Depth : Z;
      end;

      { TParentedGraphRuleGroup }
      (*
        rule group owned by a graph
      *)
      TParentedGraphRuleGroup = class(TGraphRuleGroup)
      strict private
        FParent: TGraph;
      strict protected
        procedure DoNewRule(const ADirections: TGraphDirections;
          const AValue: TGraphValue); override;
      public
        property Parent : TGraph read FParent write FParent;
      end;
  protected
    type
      TPassList = TObjectList<TGraph>;
      TPassLookup = TDictionary<String, Integer>;
  strict private
    FDimension: TDimension;
    FInv: TInvalidStateCallback;
    FMode: TGraphRunMode;
    FRuleGroups: TGraphRuleGroups;
    FValues : TGraphValues;
    FSel: TValueSelectionCallback;
    FEntries : TGraphEntries;
    FPlanes : TPlanes;
    FWrap: Boolean;
    FPasses : TPassList;
    FPassLookup : TPassLookup;
    FCurPass : String;
    FCurPassIndex : Integer;

    function GetEntry(const X, Y, Z : UInt64): TGraphEntry;
    function CoordToIndex(const X, Y, Z : UInt64) : Integer;
    function GetPass: String;
    function GetPassIndex: Integer;
    function GetRuleGroup(const AValue : TGraphValue): TParentedGraphRuleGroup;
    function GetTotalPassCount: Integer;
    function InBounds(const AIndex : Integer) : Boolean;
    procedure SetPass(const AValue: String);
  strict protected
    function PassLabelFromIndex(const AIndex : Integer) : String;
    function DoHandleInvalidState(const AEntry : TGraphEntry) : TGraphValue;
    procedure DoGetStartCoord(out X, Y : UInt64); virtual;
    function DoGetSelection(const AEntry : TGraphEntry;
      const Z, APrevZ : UInt64) : TGraphValue; virtual;

    (*
      can be overridden to validate the rules that are allowed for a graph entry
    *)
    procedure DoValidate(const AEntry : TGraphEntry; const Z, APrevZ : UInt64; out Values : TGraphValues); virtual;
  public
    (*
      all parented rule groups defined in the graph
    *)
    property RuleGroups : TGraphRuleGroups read FRuleGroups;

    (*
      callback that can be set to determine selection of valid values
      after rules have been run
    *)
    property SelectionCallback : TValueSelectionCallback read FSel write FSel;

    (*
      when an invalid state occurs for selecting values, this callback will
      be called and give a chance to change the value to use
    *)
    property InvalidStateCallback : TInvalidStateCallback read FInv write FInv;

    (*
      returns graph entry by (x, y, z) coordinates
    *)
    property Entry[const X, Y, Z : UInt64] : TGraphEntry read GetEntry; default;

    (*
      gets the rule group for the value
    *)
    property Rules[const AValue : TGraphValue] : TParentedGraphRuleGroup read GetRuleGroup;

    (*
      2D planes "stacked" in the Z direction
    *)
    property Planes : TPlanes read FPlanes;

    (*
      when enabled, neighbor assignment for entries on the external bounds
      of the graph will "wrap" around. this will result in neighbors never
      being nil, and can result in an entry to be a neighbor to itself.
      if this is not desired set this to false, but nil checking will have
      to be done before accessing member variables
    *)
    property WrapNeighbors : Boolean read FWrap write FWrap default True;

    (*
      controls the behavior for selecting plane processing order during
      running the graph
    *)
    property Mode : TGraphRunMode read FMode write FMode default rmBottomUp;

    (*
      dimension of the graph
        - Width is X along plane
        - Height is Y along plane
        - Depth is amount of planes in Z
    *)
    property Dimension : TDimension read FDimension;

    (*
      a user defined "label" to identify the pass this graph instance is on
    *)
    property CurrentPass : String read GetPass write SetPass;

    (*
      the internal index associated with the CurrentPass property
    *)
    property CurrentPassIndex : Integer read GetPassIndex;

    (*
      the total number of passes including the first default pass
    *)
    property TotalPassCount : Integer read GetTotalPassCount;

    (*
      reshapes the dimension of this graph
        @AWidth - X units, 1 based
        @AHeight - Y units, 1 based
        @ADepth - Z units, 1 based
        @Result - return "this" graph instance
    *)
    function Reshape(const AWidth, AHeight, ADepth : UInt64) : TGraph;

    (*
      adds a value to be used and returns the new rule group
        @AValue - a unique user defined value
        @Result - parented rule group to define adjacency rules
    *)
    function AddValue(const AValue : TGraphValue) : TParentedGraphRuleGroup;

    (*
      executes ACallback for each pass including the default first pass
      and guarantees sequential iteration
        @ACallback - the user defined callback to execute
        @Result - returns "this" graph instance
    *)
    function ForEachPass(const ACallback : TForEachPassNestedCallback) : TGraph;

    (*
      "switches" the graph to the pass specified by the caller. a "pass"
      can have it's own set of rules / constraints defined and will be
      run sequentially. dimensions will be the same as the "first pass"
        @APass - the pass label to switch to
        @PassIndex - index to the labeled pass
        @Result - returns "this" graph instance
    *)
    function SwitchToPass(const APass : String; out PassIndex : Integer) : TGraph; overload;
    function SwitchToPass(const APass : String) : TGraph; overload;

    (*
      "switches" the graph to the pass by index. this call will fail if the
      index is out of bounds
        @AIndex - a valid index to a pass
    *)
    function SwitchToPass(const AIndex : Integer) : TGraph; overload;

    (*
      once all values and rules have been apply, this will
      execute the rules against each graph entry
        @Result - return "this" graph instance
    *)
    function Run : TGraph;

    (*
      clears all rules and reshapes to empty (0, 0, 0)
        @Result - return "this" graph instance
    *)
    function Reset : TGraph;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

var
  (*
    default selection will use random values. for those wanting to override
    with something like a weighted approach, either set this global to your
    own callback, or set this on the graph instance
  *)
  DefaultSelection : TValueSelectionCallback;

  (*
    returns opposite direction
  *)
  function InverseOfDir(const ADirection : TGraphDirection) : TGraphDirection; inline;

  (*
    checks if a value is held in a graph values array
  *)
  function ContainsGraphValue(const AValues : TGraphValues; const AValue : TGraphValue) : Boolean; inline;

const
  AllDirections : TGraphDirections = [gdNorth, gdEast, gdSouth, gdWest, gdUp, gdDown];

implementation
uses
  Math;

function DefSelCall(const {%H-}AGraph : TGraph; const AEntry : TGraphEntry;
  const AValid : TGraphValues) : TGraphValue;
begin
  if Length(AValid) < 1 then
    Result := AEntry.Value
  else
  begin
    RandSeed := Random(MaxInt);
    Result := AValid[RandomRange(Low(AValid), Length(AValid))];
  end;
end;

function InverseOfDir(const ADirection: TGraphDirection): TGraphDirection;
begin
  if ADirection = gdNorth then
    Exit(gdSouth)
  else if ADirection = gdEast then
    Exit(gdWest)
  else if ADirection = gdSouth then
    Exit(gdNorth)
  else if ADirection = gdWest then
    Exit(gdEast)
  else if ADirection = gdUp then
    Exit(gdDown)
  else
    Exit(gdUp);
end;

function ContainsGraphValue(const AValues: TGraphValues;
  const AValue: TGraphValue): Boolean;
var
  I: Integer;
begin
  if Length(AValues) < 1 then
    Exit(False)
  else
  begin
    for I := 0 to High(AValues) do
      if AValues[I] = AValue then
        Exit(True);
  end;

  Exit(False)
end;

{ TGraph.TParentedGraphRuleGroup }

procedure TGraph.TParentedGraphRuleGroup.DoNewRule(
  const ADirections: TGraphDirections; const AValue: TGraphValue);
var
  LDir: TGraphDirection;
  LDirs : TGraphDirections = [];
begin
  inherited DoNewRule(ADirections, AValue);

  //after a rule has been added we need to add the "inverse" for any new values
  for LDir in ADirections do
    Include(LDirs, InverseOfDir(LDir));

  if not Parent.RuleGroups.ContainsKey(AValue) then
    Parent.AddValue(AValue).NewRule(LDirs, Value);
end;

{ TGraphRuleGroup }

function TGraphRuleGroup.GetExists(const ADirection : TGraphDirection): Boolean;
begin
  Result := IndexOfDirection(ADirection) >= 0;
end;

function TGraphRuleGroup.GetRule(const ADirection : TGraphDirection): TGraphRule;
begin
  Result := FRules[IndexOfDirection(ADirection)];
end;

function TGraphRuleGroup.IndexOfDirection(const ADirection: TGraphDirection): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to High(FRules) do
    if FRules[I].Key = ADirection then
      Exit(I);
end;

procedure TGraphRuleGroup.DoNewRule(const ADirections: TGraphDirections;
  const AValue: TGraphValue);
var
  LRule : TGraphRule;
  LVals : TGraphValues;
  I: Integer;
  LDir: TGraphDirection;
begin
  for LDir in ADirections do
  begin
    LVals := Default(TGraphValues);

    //check for direction existence first then append if so
    if Exists[LDir] then
    begin
      I := IndexOfDirection(LDir);
      LRule := FRules[I];
      LVals := LRule.Value;
    end
    //otherwise no direction/rules set so insert to end of rules
    else
    begin
      I := Length(FRules);
      LRule.Key := LDir;
    end;

    //insert rule value if we haven't already done so
    if not ContainsGraphValue(LVals, AValue) then
    begin
      Insert(AValue, LVals, Length(LVals));
      LRule.Value := LVals;
    end;

    //upsert
    Insert(LRule, FRules, I);
  end;
end;


function TGraphRuleGroup.NewRule(const ADirections: TGraphDirections;
  const AValue: TGraphValue): TGraphRuleGroup;
begin
  Result := Self;
  DoNewRule(ADirections, AValue);
end;

function TGraphRuleGroup.NewRule(const ADirections: TGraphDirections;
  const AValues: TGraphValues): TGraphRuleGroup;
var
  I: Integer;
begin
  Result := Self;

  for I := 0 to High(AValues) do
    NewRule(ADirections, AValues[I]);
end;

constructor TGraphRuleGroup.Create;
begin
  FVal := '';
  SetLength(FRules, 0);
end;

constructor TGraphRuleGroup.Create(const AValue: TGraphValue);
begin
  Create;
  FVal := AValue;
end;

{ TGraphEntry }

procedure TGraphEntry.SetValue(const AValue: TGraphValue);
begin
  //don't trigger when we aren't changing value
  if AValue = FVal then
    Exit;

  DoBeforeSetValue(AValue);
  FVal := AValue;
  FEmpty := False;
  DoAfterSetValue(AValue);
end;

procedure TGraphEntry.DoBeforeSetNeighbor(const ANeighbor: TGraphEntry);
begin
  //nothing in base
end;

procedure TGraphEntry.DoAfterSetNeighbor(const ANeighbor: TGraphEntry);
begin
  //nothing in base
end;

procedure TGraphEntry.DoBeforeSetValue(const AValue: TGraphValue);
begin
  //nothing in base
end;

procedure TGraphEntry.DoAfterSetValue(const AValue: TGraphValue);
begin
  //nothing in base
end;

procedure TGraphEntry.DoReset;
var
  I: Integer;
begin
  for I := 0 to High(FNeighbors) do
    FNeighbors[I] := nil;

  FVal := TGraphValue.Empty;
  FEmpty := True;
end;

function TGraphEntry.DoGenerateID: String;
begin
  Result := TGuid.NewGuid().ToString();
end;

procedure TGraphEntry.Reset;
begin
  DoReset;
end;

function TGraphEntry.GetNeighbor(const ADirection : TGraphDirection): TGraphEntry;
begin
  Result := FNeighbors[Ord(ADirection)];
end;

procedure TGraphEntry.SetNeighbor(const ADirection: TGraphDirection;
  const AValue: TGraphEntry);
begin
  DoBeforeSetNeighbor(AValue);
  FNeighbors[Ord(ADirection)] := AValue;
  DoAfterSetNeighbor(AValue);
end;

constructor TGraphEntry.Create;
begin
  SetLength(FNeighbors, Succ(Ord(High(TGraphDirection))));
  Reset;
  FID := DoGenerateID;
end;

{ TGraph }

function TGraph.GetEntry(const X, Y, Z : UInt64): TGraphEntry;
begin
  //quicker lookup then using the planes collection
  Result := FEntries[CoordToIndex(X, Y, Z)];
end;

function TGraph.CoordToIndex(const X, Y, Z: UInt64): Integer;
begin
  //index into the "flattened" graph for a quicker lookup than going
  //through the planes collection
  Result := (FDimension.Width * FDimension.Height * Z) + X + (Y * FDimension.Width);
end;

function TGraph.GetPass: String;
begin
  Exit(FCurPass);
end;

function TGraph.GetPassIndex: Integer;
begin
  Exit(Succ(FCurPassIndex)); //offset by one to account for first pass
end;

function TGraph.GetRuleGroup(const AValue : TGraphValue): TParentedGraphRuleGroup;
begin
  Result := TParentedGraphRuleGroup(FRuleGroups[AValue]);
end;

function TGraph.GetTotalPassCount: Integer;
begin
  Result := Succ(FPasses.Count);
end;

function TGraph.InBounds(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FEntries.Count);
end;

procedure TGraph.SetPass(const AValue: String);
var
  LPair : TPair<String, Integer>;
begin
  //if the requested label is different than what we have, extract and update
  if AValue <> FCurPass then
  begin
    //dupes not allowed
    if FPassLookup.ContainsKey(AValue) then
      raise Exception.Create('SetPass::pass label is already in use');

    LPair := FPassLookup.ExtractPair(FCurPass);
    LPair.Key := AValue;
    FPassLookup.Add(LPair);

    //lastly update the current pass
    FCurPass := AValue;
  end;
end;

function TGraph.PassLabelFromIndex(const AIndex: Integer): String;
var
  LPair : TPair<String, Integer>;
begin
  Result := '';
  for LPair in FPassLookup do
    //internal we store offset index so pred the requested
    if LPair.Value = Pred(AIndex) then
      Exit(LPair.Key);

  raise Exception.Create('PassLabelFromIndex::index out of bounds [' + IntToStr(AIndex) + ']');
end;

function TGraph.DoHandleInvalidState(const AEntry: TGraphEntry): TGraphValue;
begin
  //default to the current value
  Result := AEntry.Value;

  //when we have the callback assigned then use it
  if Assigned(FInv) then
    FInv(Self, AEntry, Result);
end;

procedure TGraph.DoGetStartCoord(out X, Y: UInt64);
begin
  //base we'll just use a random approach, but this can be overridden
  RandSeed := Random(MaxInt);
  X := RandomRange(0, FDimension.Width);
  Y := RandomRange(0, FDimension.Height);
end;

function TGraph.DoGetSelection(const AEntry: TGraphEntry; const Z,
  APrevZ: UInt64): TGraphValue;
var
  LValues: TGraphValues;
begin
  if not Assigned(FSel) then
    raise Exception.Create('DoGetSelection::selection callback cannot be nil');

  //get the valid rules for this entry
  DoValidate(AEntry, Z, APrevZ, LValues);

  //in the case that we started with values but removed all valid options
  //invoke the invalid state handler
  if Length(LValues) < 1 then
    Exit(DoHandleInvalidState(AEntry));

  //pass the rules to the callback for determining the value of this entry
  Result := FSel(Self, AEntry, LValues);
end;

procedure TGraph.DoValidate(const AEntry: TGraphEntry; const Z, APrevZ: UInt64;
  out Values: TGraphValues);

  procedure TrimValuesForNeighbor(const ANeighbor : TGraphEntry;
    const ADirection : TGraphDirection);
  var
    LGroup : TGraphRuleGroup;
    LDir: TGraphDirection;
    LRuleVals, LVals : TGraphValues;
    I: Integer;
  begin
    LVals := Default(TGraphValues);

    //no neighbor, get out
    if not Assigned(ANeighbor) then
      Exit;

    //neighbor has no value to validate against
    if ANeighbor.Empty then
      Exit;

    if not FRuleGroups.ContainsKey(ANeighbor.Value) then
      Exit;

    LGroup := FRuleGroups[ANeighbor.Value];

    //if we have rules for the direction, we'll
    //use to validate. we flip because we are working "backwards" from neighbors
    //that have been assigned already (todo - this comment might not make sense, clean it up)
    LDir := InverseOfDir(ADirection);

    if LGroup.Exists[LDir] then
    begin
      LRuleVals := LGroup[LDir].Value;

      //note:
      //  no rules, means any state is possible. for users to specifically
      //  state "nothing" should be allowed, a user defined value representing "nothing"
      //  should be introduced
      if Length(LRuleVals) < 1 then
        Exit;

      //iterate the current value list and check to see what values we have left
      for I := 0 to High(Values) do
        if ContainsGraphValue(LRuleVals, Values[I]) then
          Insert(Values[I], LVals, Length(LVals));

      //lastly, set the output values to our local validated values
      Values := LVals
    end;
  end;

begin
  //default case for a non-empty entry is to use the value it was assigned
  if not AEntry.Empty then
  begin
    SetLength(Values, 1);
    Values[0] := AEntry.Value;

    //todo - do we need to exit here or check for neighbors values?
    Exit;
  end;

  //first get all of the possible states we can be in by setting to all values
  Values := FValues;

  //get the rule group for each of the entry's neighbors on the same plane
  TrimValuesForNeighbor(AEntry[gdNorth], gdNorth);
  TrimValuesForNeighbor(AEntry[gdEast], gdEast);
  TrimValuesForNeighbor(AEntry[gdSouth], gdSouth);
  TrimValuesForNeighbor(AEntry[gdWest], gdWest);

  //now depending on the previous Z we either will look "up" or "down"
  if APrevZ < Z then
    TrimValuesForNeighbor(AEntry[gdUp], gdUp) //moving top -> bottom
  else if APrevZ > Z then
    TrimValuesForNeighbor(AEntry[gdDown], gdDown); //moving bottom -> top
end;

function TGraph.Reshape(const AWidth, AHeight, ADepth: UInt64): TGraph;
var
  LEntry: TGraphEntry;
  LPlane : TPlane;
  LCoord : TPlaneCoord;
  Z, Y, X: Integer;

  function GetNeighbor(const AIndex : Integer) : TGraphEntry;
  begin
    if InBounds(AIndex) then
      Exit(FEntries[AIndex])
    else
      Exit(nil);
  end;

begin
  Result := Self;

  //first clear the list and planes collections
  FEntries.Clear;
  FPlanes.Clear;

  //update dimensions
  FDimension.Width := AWidth;
  FDimension.Height := AHeight;
  FDimension.Depth := ADepth;

  //now create entries starting on the lowest plane
  for Z := 0 to Pred(ADepth) do
  begin
    //now setup our graph structure (this is exposed to users for ease)
    //starting with this plane
    LPlane := TPlane.Create;

    //starting in "bottom" to "top"
    for Y := 0 to Pred(AHeight) do
    begin
      //"left" to "right"
      for X := 0 to Pred(AWidth) do
      begin
        //create and add to the managed list
        LEntry := TGraphEntry.Create;
        FEntries.Add(LEntry);

        //setup a coord (x, y) pair
        LCoord.Key := X;
        LCoord.Value := Y;

        //add coord and entry to the plane
        LPlane.Add(LCoord, LEntry);
      end;
    end;

    //add the completed plane to the planes collection
    FPlanes.Add(Z, LPlane);
  end;

  //now that everything has been shaped, we need to properly assign neighbors
  for Z := 0 to Pred(ADepth) do
    for Y := 0 to Pred(AHeight) do
      for X := 0 to Pred(AWidth) do
      begin
        LEntry := FEntries[CoordToIndex(X, Y, Z)];

        LEntry[gdNorth] := GetNeighbor(CoordToIndex(X, Succ(Y), Z));
        if FWrap and (not Assigned(LEntry[gdNorth])) then
          LEntry[gdNorth] := GetNeighbor(CoordToIndex(X, 0, Z));

        LEntry[gdEast] := GetNeighbor(CoordToIndex(Succ(X), Y, Z));
        if FWrap and (not Assigned(LEntry[gdEast])) then
          LEntry[gdEast] := GetNeighbor(CoordToIndex(0, Y, Z));

        LEntry[gdSouth] := GetNeighbor(CoordToIndex(X, Pred(Y), Z));
        if FWrap and (not Assigned(LEntry[gdSouth])) then
          LEntry[gdSouth] := GetNeighbor(CoordToIndex(X, Pred(FDimension.Height), Z));

        LEntry[gdWest] := GetNeighbor(CoordToIndex(Pred(X), Y, Z));
        if FWrap and (not Assigned(LEntry[gdWest])) then
          LEntry[gdWest] := GetNeighbor(CoordToIndex(Pred(FDimension.Width), Y, Z));

        LEntry[gdUp] := GetNeighbor(CoordToIndex(X, Y, Succ(Z)));
        if FWrap and (not Assigned(LEntry[gdUp])) then
          LEntry[gdUp] := GetNeighbor(CoordToIndex(X, Y, 0));

        LEntry[gdDown] := GetNeighbor(CoordToIndex(X, Y, Pred(Z)));
        if FWrap and (not Assigned(LEntry[gdDown])) then
          LEntry[gdDown] := GetNeighbor(CoordToIndex(X, Y, Pred(FDimension.Depth)));
      end;
end;

function TGraph.AddValue(const AValue: TGraphValue): TParentedGraphRuleGroup;
begin
  //if exists, just return it
  if FRuleGroups.ContainsKey(AValue) then
    Result := TParentedGraphRuleGroup(FRuleGroups[AValue])
  //otherwise create a new group for the rule
  else
  begin
    //first insert to the values collection (we do this for ease of lookup)
    Insert(AValue, FValues, Length(FValues));

    //then create a parented rule group to hold the rules
    Result := TParentedGraphRuleGroup.Create(AValue);
    Result.Parent := Self;
    FRuleGroups.Add(AValue, Result);
  end;
end;

function TGraph.ForEachPass(const ACallback: TForEachPassNestedCallback): TGraph;
var
  I: Integer;
  LLabels : TStringArray;
begin
  Result := Self;
  LLabels := Default(TStringArray);

  //callback must be assigned
  if not Assigned(ACallback) then
    Exit;

  //build an index of labels
  SetLength(LLabels, TotalPassCount);
  for I := 0 to Pred(TotalPassCount) do
    LLabels[I] := PassLabelFromIndex(I);

  //now we have everything we need to call the methods
  for I := 0 to Pred(TotalPassCount) do
    if I = 0 then
      ACallback(Self, LLabels[I], I) //this instance is first pass
    else
      ACallback(FPasses[Pred(I)], LLabels[I], I); //all others index into pass collection
end;

function TGraph.SwitchToPass(const APass: String; out PassIndex: Integer): TGraph;
var
  LGraph: TGraph;
begin
  Result := Self;

  //determine if we have the current pass label (existing)
  //if so, then set the index
  if FPassLookup.ContainsKey(APass) then
  begin
    PassIndex := FPassLookup[APass];

    //requested label was for "first" pass
    if PassIndex < 0 then
      PassIndex := 0;
  end
  else
  begin
    //on new label, initialize a new graph, copying dimensions
    LGraph := TGraph.Create;
    LGraph.Reshape(FDimension.Width, FDimension.Height, FDimension.Depth);

    //add to the passes collection and set the index
    PassIndex := Succ(FPasses.Add(LGraph)); //offset by 1 to for "first" pass not included in list

    //add the label with the index to the map
    FPassLookup.Add(APass, Pred(PassIndex));
  end;


  //update CurrentPass AND CurrentPassIndex
  FCurPass := APass;
  FCurPassIndex := Pred(PassIndex); //ensure the internal index is always able to index straight to list
end;

function TGraph.SwitchToPass(const APass: String): TGraph;
var
  I : Integer;
begin
  Result := SwitchToPass(APass, I);
end;

function TGraph.SwitchToPass(const AIndex: Integer): TGraph;
var
  I : Integer;
begin
  Result := SwitchToPass(PassLabelFromIndex(AIndex), I);
end;

function TGraph.Run: TGraph;
var
  I: Integer;

  procedure RunPlane(const Z, APrevZ : UInt64);
  var
    X, Y: UInt64;
    LEntry : TGraphEntry;
    LIndex: Integer;
    LVisited : TList<TGraphEntry>;

    procedure UpdateEntriesFromLoc(const AEntry : TGraphEntry);
    begin
      if not Assigned(AEntry) then
        Exit;

      //we'll keep track of which entries we've already visited and bail if
      //we encounter them again
      if LVisited.IndexOf(AEntry) >= 0 then
        Exit
      else
        LVisited.Add(AEntry);

      //get and assign the selection
      if AEntry.Empty then
        AEntry.Value := DoGetSelection(AEntry, Z, APrevZ);

      //recurse with each neighbor
      UpdateEntriesFromLoc(AEntry[gdNorth]);
      UpdateEntriesFromLoc(AEntry[gdEast]);
      UpdateEntriesFromLoc(AEntry[gdSouth]);
      UpdateEntriesFromLoc(AEntry[gdWest]);
      UpdateEntriesFromLoc(AEntry[gdUp]);
      UpdateEntriesFromLoc(AEntry[gdDown]);
    end;

  begin
    LVisited := TList<TGraphEntry>.Create;

    try
      //now find the starting location
      DoGetStartCoord(X, Y);

      //get the entry
      LIndex := CoordToIndex(X, Y, Z);

      //check for a valid index and continue if not (should always be true)
      if not InBounds(LIndex) then
        raise Exception.Create(Format('RunPlane::invalid coordinates [x]-%d, [y]-%d, [z]-%d', [X, Y, Z]));

      LEntry := FEntries[LIndex];

      //starting at this entry, we'll update the plane
      UpdateEntriesFromLoc(LEntry);
    finally
      LVisited.Free;
    end;
  end;

begin
  Result := Self;

  //handle the different run modes and pass the current plane to the Runplane helper
  if FMode = rmBottomUp then
    for I := 0 to Pred(FDimension.Depth) do
    begin
      if I = 0 then
        RunPlane(I, I)
      else
        RunPlane(I, Pred(I));
    end
  else if FMode = rmTopDown then
    for I := Pred(FDimension.Depth) downto 0 do
    begin
      if I = Pred(FDimension.Depth) then
        RunPlane(I, I)
      else
        RunPlane(I, Succ(I));
    end
  else
    raise Exception.Create('Run::run mode not implemented');
end;

function TGraph.Reset: TGraph;
var
  I: Integer;
begin
  Result := Self;
  FRuleGroups.Clear;
  SetLength(FValues, 0);
  Reshape(0, 0, 0);

  for I := 0 to Pred(FEntries.Count) do
    FEntries[I].Reset;
end;

constructor TGraph.Create;
begin
  FSel := DefaultSelection;
  FEntries := TGraphEntries.Create;
  FPlanes := TPlanes.Create;
  FRuleGroups := TGraphRuleGroups.Create;
  FPasses := TPassList.Create;
  FPassLookup := TPassLookup.Create;
  FCurPassIndex := -1;
  FCurPass := '';
  FPassLookup.Add(FCurPass, FCurPassIndex); //init the "first" pass in lookup
end;

destructor TGraph.Destroy;
begin
  FEntries.Free;
  FPlanes.Free;
  FRuleGroups.Free;
  FPasses.Free;
  inherited Destroy;
end;

initialization
  DefaultSelection := DefSelCall;
end.

