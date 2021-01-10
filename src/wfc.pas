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
  public
    property Value : TGraphValue read FVal write SetValue;
    property Empty : Boolean read FEmpty;
    property Neighbor[const ADirection : TGraphDirection] : TGraphEntry read GetNeighbor write SetNeighbor; default;

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
  strict protected
    function IndexOfDirection(const ADirection : TGraphDirection) : Integer;
  public
    property Value : TGraphValue read FVal write FVal;
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

  //callback for entry selection after filtering for only valid values
  TValueSelectionCallback = function(const AEntry : TGraphEntry;
    const AValid : TGraphValues) : TGraphValue;

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
    FRules: TGraphRuleGroups;
    FSel: TValueSelectionCallback;
    FEntries : TGraphEntries;
    FPlanes : TPlanes;

    function GetEntry(const X, Y, Z : UInt64): TGraphEntry;
    function CoordToIndex(const X, Y, Z : UInt64) : Integer;
  strict protected
  public
    property RuleGroups : TGraphRuleGroups read FRules write FRules;
    property SelectionCallback : TValueSelectionCallback read FSel write FSel;
    property Entry[const X, Y, Z : UInt64] : TGraphEntry read GetEntry;
    property Planes : TPlanes read FPlanes;

    function Reshape(const X, Y, Z : UInt64) : TGraph;
    function AddValue(const AValue : TGraphValue) : TParentedGraphRuleGroup;

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

function DefSelCall(const AEntry : TGraphEntry;
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

  //check for direction existance first then append if so
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
end;

{ TGraph }

function TGraph.GetEntry(const X, Y, Z : UInt64): TGraphEntry;
begin
  //quicker lookup then using the planes collection
  Result := FEntries[CoordToIndex(X, Y, Z)];
end;

function TGraph.CoordToIndex(const X, Y, Z: UInt64): Integer;
begin
  Result := Pred(Succ(X) * Succ(Y) * Succ(Z));
end;

function TGraph.Reshape(const X, Y, Z: UInt64): TGraph;
var
  LEntry: TGraphEntry;
  LPlane : TPlane;
  LCoord : TPlaneCoord;
  I, J, K: Integer;
begin
  Result := Self;

  //first clear the list and planes collections
  FEntries.Clear;
  FPlanes.Clear;

  //now create entries starting on the lowest plane
  for I := 0 to Z do
  begin
    //now setup our graph structure (this is exposed to users for ease)
    //starting with this plane
    LPlane := TPlane.Create;

    //starting in "bottom" to "top"
    for J := 0 to Y do
    begin
      //"left" to "right"
      for K := 0 to X do
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
    FPlanes.Add(I, LPlane);
  end;

  //now that everything has been shaped, we need to properly assigne neighbors
  //todo...
end;

function TGraph.AddValue(const AValue: TGraphValue): TGraph.TParentedGraphRuleGroup;
begin
  //if exists, just return it
  if FRules.ContainsKey(AValue) then
    Result := TParentedGraphRuleGroup(FRules[AValue])
  //otherwise create a new group for the rule
  else
  begin
    Result := TParentedGraphRuleGroup.Create(AValue);
    FRules.Add(Result);
  end;
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

