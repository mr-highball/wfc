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
  public
    property Value : TGraphValue read FVal write FVal;
    property Rule[const ADirection : TGraphDirection] : TGraphRule read GetRule; default;
    property Rules : TGraphRules read FRules write FRules;
    property Exists[const ADirection : TGraphDirection] : Boolean read GetExists;

    function NewRule(const ADirection : TGraphDirection;
      const AValue : TGraphValue) : TGraphRuleGroup; overload;

    function NewRule(const ADirection : TGraphDirection;
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

  (*
    determines the mode for the plane selection process during a graph instance run
  *)
  TGraphRunMode = (rmBottomUp, rmTopDown);

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
      public
        property Parent : TGraph read FParent write FParent;
      end;
  strict private
    FDimension: TDimension;
    FMode: TGraphRunMode;
    FRules: TGraphRuleGroups;
    FSel: TValueSelectionCallback;
    FEntries : TGraphEntries;
    FPlanes : TPlanes;
    FWrap: Boolean;

    function GetEntry(const X, Y, Z : UInt64): TGraphEntry;
    function CoordToIndex(const X, Y, Z : UInt64) : Integer;
    function InBounds(const AIndex : Integer) : Boolean;
  strict protected
    procedure DoGetStartCoord(out X, Y : UInt64); virtual;
    function DoGetSelection(const AEntry : TGraphEntry; const Z, APrevZ : UInt64) : TGraphValue; virtual;

    (*
      can be overridden to validate the rules that are allowed for a graph entry
    *)
    procedure DoValidate(const AEntry : TGraphEntry; const Z, APrevZ : UInt64; out Values : TGraphValues); virtual;
  public
    (*
      all parented rule groups defined in the graph
    *)
    property RuleGroups : TGraphRuleGroups read FRules write FRules;

    (*
      callback that can be set to determine selection of valid values
      after rules have been run
    *)
    property SelectionCallback : TValueSelectionCallback read FSel write FSel;

    (*
      returns graph entry by (x, y, z) coordinates
    *)
    property Entry[const X, Y, Z : UInt64] : TGraphEntry read GetEntry; default;

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
      reshapes the dimension of this graph
        @AWidth - X units, 1 based
        @AHeight - Y units, 1 based
        @ADepth - Z units, 1 based
    *)
    function Reshape(const AWidth, AHeight, ADepth : UInt64) : TGraph;

    (*
      adds a value to be used
        @AValue - a unique user defined value
        @Result - parented rule group to define adjacency rules
    *)
    function AddValue(const AValue : TGraphValue) : TParentedGraphRuleGroup;

    (*
      once all values and rules have been apply, this will
      execute the rules against each graph entry
    *)
    function Run : TGraph;

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
    Randomize;
    Result := AValid[Low(AValid), RandomRange(Low(AValid), Length(AValid))];
  end;
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

function TGraphRuleGroup.NewRule(const ADirection: TGraphDirection;
  const AValue: TGraphValue): TGraphRuleGroup;
var
  LRule : TGraphRule;
  LVals : TGraphValues;
  I: Integer;
begin
  Result := Self;

  //check for direction existence first then append if so
  if Exists[ADirection] then
  begin
    I := IndexOfDirection(ADirection);
    LRule := FRules[I];
    LVals := LRule.Value;
  end
  //otherwise no direction/rules set so insert to end of rules
  else
  begin
    I := Length(FRules);
    LRule.Key := ADirection;
  end;

  //insert value and then upsert to rules
  Insert(AValue, LVals, Length(LVals));
  LRule.Value := LVals;
  Insert(LRule, FRules, I);
end;

function TGraphRuleGroup.NewRule(const ADirection: TGraphDirection;
  const AValues: TGraphValues): TGraphRuleGroup;
var
  I: Integer;
begin
  Result := Self;

  for I := 0 to High(AValues) do
    NewRule(ADirection, AValues[I]);
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

function TGraph.InBounds(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FEntries.Count);
end;

procedure TGraph.DoGetStartCoord(out X, Y: UInt64);
begin
  //base we'll just use a random approach, but this can be overridden
  Randomize;
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

  //pass the rules to the callback for determining the value of this entry
  Result := FSel(Self, AEntry, LValues);
end;

procedure TGraph.DoValidate(const AEntry: TGraphEntry; const Z, APrevZ: UInt64;
  out Values: TGraphValues);
begin
  //todo - validate rules for a given entry

  //default case for a non-empty entry is to use the value it was assigned
  if not AEntry.Empty then
  begin
    SetLength(Result, 1);
    Result[0] := AEntry.Value;

    //todo - do we need to exit here or check for neighbors values?
    Exit;
  end;

  //first get all of the possible states we can be in
  //...


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
  if FRules.ContainsKey(AValue) then
    Result := TParentedGraphRuleGroup(FRules[AValue])
  //otherwise create a new group for the rule
  else
  begin
    Result := TParentedGraphRuleGroup.Create(AValue);
    FRules.Add(AValue, Result);
  end;
end;

function TGraph.Run: TGraph;
var
  I: Integer;

  procedure RunPlane(const Z, APrevZ : UInt64);
  var
    X, Y: UInt64;
    LEntry : TGraphEntry;
    LIndex: Integer;

    function IsValidPrevZ(const Z : UInt64) : Boolean;
    begin
      Result := (FDimension.Depth > 0) and  (Z <= FDimension.Depth);
    end;

    procedure UpdateEntriesFromLoc(const AEntry : TGraphEntry);
    begin
      if not Assigned(AEntry) then
        Exit;

      //get and assign the selection
      if LEntry.Empty then
        LEntry.Value := DoGetSelection(LEntry, Z, APrevZ)
      //otherwise we've already encountered this entry
      else
        Exit;

      //recurse with each neighbor
      UpdateEntriesFromLoc(LEntry[gdNorth]);
      UpdateEntriesFromLoc(LEntry[gdEast]);
      UpdateEntriesFromLoc(LEntry[gdSouth]);
      UpdateEntriesFromLoc(LEntry[gdWest]);
      UpdateEntriesFromLoc(LEntry[gdUp]);
      UpdateEntriesFromLoc(LEntry[gdDown]);
    end;

  begin
    //now find the starting location
    DoGetStartCoord(X, Y);

    //get the entry
    LIndex := CoordToIndex(X, Y, Z);

    //check for a valid index and continue if not (should always be true)
    if not InBounds(LIndex) then
      raise Exception.Create(Format('RunPlane::invalid coordinates [x]-%d, [y]-%d, [z]-%d', [X, Y, Z]));

    LEntry := FEntries[LIndex];

    //starting at this entry, we'll update the plane
    UpdateEntriesFromLoc(LEntry)
  end;

begin
  Result := Self;

  for I := 0 to Pred(FEntries.Count) do
    FEntries[I].Reset;

  //handle the different run modes and pass the current plane to the Runplane helper
  if FMode = rmBottomUp then
    for I := 0 to Pred(FDimension.Depth) do
      RunPlane(I, Pred(I))
  else if FMode = rmTopDown then
    for I := Pred(FDimension.Depth) downto 0 do
      RunPlane(I, Succ(I))
  else
    raise Exception.Create('Run::run mode not implemented');
end;

constructor TGraph.Create;
begin
  FSel := DefaultSelection;
  FEntries := TGraphEntries.Create;
  FPlanes := TPlanes.Create;
  FRules := TGraphRuleGroups.Create;
end;

destructor TGraph.Destroy;
begin
  FEntries.Free;
  FPlanes.Free;
  FRules.Free;
  inherited Destroy;
end;

initialization
  DefaultSelection := DefSelCall;
end.

