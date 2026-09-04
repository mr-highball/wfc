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
{$IFNDEF PAS2JS}
{$ModeSwitch nestedprocvars}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections;

type

  //user defined value
  TGraphValue = String;
  TGraphValues = TArray<TGraphValue>;
  //native builds retain the original UInt64 API; pas2js uses Cardinal because
  //its RTL does not implement UInt64. storage is bounded by Integer on both.
  {$IFDEF PAS2JS}
  TGraphCoordinate = Cardinal;
  {$ELSE}
  TGraphCoordinate = UInt64;
  {$ENDIF}
  TGraphSeed = Cardinal;

const
  //Increment when seed expansion, pass-stream derivation, bounded sampling,
  //or the built-in generator changes in a replay-incompatible way.
  WFC_RANDOM_ALGORITHM_VERSION = 1;

type

  TGraphPosition = record
    X : TGraphCoordinate;
    Y : TGraphCoordinate;
    Z : TGraphCoordinate;
  end;

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
    FGenerated: Boolean;
    FID: String;
    FIndex: Integer;
    FPosition: TGraphPosition;
    FVal: TGraphValue;
    FNeighbors : TNeighbors;

    function GetNeighbor(const ADirection : TGraphDirection): TGraphEntry;
    procedure SetNeighbor(const ADirection : TGraphDirection;
      const AValue: TGraphEntry);
    procedure SetValue(const AValue: TGraphValue);
  private
    procedure AssignValue(const AValue: TGraphValue;
      const AGenerated: Boolean);
    procedure InitializePosition(const AIndex: Integer;
      const AX, AY, AZ: TGraphCoordinate);
    procedure SetGeneratedValue(const AValue: TGraphValue);
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
    property Generated : Boolean read FGenerated;
    property Neighbor[const ADirection : TGraphDirection] : TGraphEntry read GetNeighbor write SetNeighbor; default;
    property ID : String read FID write FID;
    property Index : Integer read FIndex;
    property Position : TGraphPosition read FPosition;

    procedure ClearValue;
    procedure Reset;
    constructor Create; virtual;
  end;

  //collection of entries
  TGraphEntries = TObjectList<TGraphEntry>;
  TRequireRule = Boolean;

  //for a particular value, what the accepted states are for each direction
  //Native FPC retains the original three-parameter TPair identity. pas2js
  //only provides a two-parameter TPair, so it uses the equivalent record.
  {$IFDEF PAS2JS}
  TGraphRule = record
    Key : TGraphDirection;
    Value : TGraphValues;
    Info : TRequireRule;
    constructor Create(const AKey: TGraphDirection;
      const AValue: TGraphValues; const AInfo: TRequireRule = False);
  end;
  {$ELSE}
  TGraphRule = TPair<TGraphDirection, TGraphValues, TRequireRule>;
  {$ENDIF}
  TGraphRules = array of TGraphRule;

  { TGraphRuleGroup }
  (*
    class to easily group rules and directions for a specified value
  *)
  TGraphRuleGroup = class(TObject)
  strict private
    FRules: TGraphRules;
    FPreviousValues: TGraphValues;
    FVal: TGraphValue;
    function GetExists(const ADirection : TGraphDirection): Boolean;
    function GetHasRequired: Boolean;
    function GetRule(const ADirection : TGraphDirection): TGraphRule;
  strict protected
    function IndexOfDirection(const ADirection : TGraphDirection) : Integer;

    (*
      can be overridden to handle additional logic for adding new rules
    *)
    procedure DoNewRule(const ADirections : TGraphDirections;
      const AValue : TGraphValue; const ARequireRule : Boolean); virtual;
    procedure DoRequirePrevious(const AValue : TGraphValue); virtual;
    procedure UpsertRule(const ADirections : TGraphDirections;
      const AValue : TGraphValue; const ARequireRule : Boolean);
  public
    property Value : TGraphValue read FVal write FVal;
    property Rule[const ADirection : TGraphDirection] : TGraphRule read GetRule; default;
    property Rules : TGraphRules read FRules write FRules;
    property Exists[const ADirection : TGraphDirection] : Boolean read GetExists;
    property PreviousValues : TGraphValues read FPreviousValues;

    (*
      true if at least one rule for this value is required
    *)
    property HasRequired : Boolean read GetHasRequired;

    function NewRule(const ADirections : TGraphDirections;
      const AValue : TGraphValue; const ARequireRule : Boolean = False) : TGraphRuleGroup; overload;

    function NewRule(const ADirections : TGraphDirections;
      const AValues : TGraphValues; const ARequireRule : Boolean = False) : TGraphRuleGroup; overload;

    (*
      allows this value only when the entry at the same coordinate in the
      immediately preceding pass contains one of the supplied values
    *)
    function RequirePrevious(const AValue : TGraphValue) : TGraphRuleGroup; overload;
    function RequirePrevious(const AValues : TGraphValues) : TGraphRuleGroup; overload;

    constructor Create; virtual; overload;
    constructor Create(const AValue : TGraphValue); virtual; overload;
  end;

  //collection of rule groups
  TGraphRuleGroups = TObjectDictionary<TGraphValue, TGraphRuleGroup>;

  //forward
  TGraph = class;
  TGraphClass = class of TGraph;

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

  TForEachPassCallback = procedure(const AGraph : TGraph;
    const APass : String; const APassIndex : Integer);

  TForEachPassMethod = procedure(const AGraph : TGraph;
    const APass : String; const APassIndex : Integer) of object;

  {$IFNDEF PAS2JS}
  //deprecated native compatibility type; portable code should use the plain
  //procedure or object-method callback overloads above
  TForEachPassNestedCallback = procedure(const AGraph : TGraph;
    const APass : String; const APassIndex : Integer) is nested;
  {$ENDIF}

  { TGraph }
  (*
    a graph used to perform the wave function collapse algorithm
  *)
  TGraph = class(TObject)
  public
    type
      X = TGraphCoordinate;
      Y = TGraphCoordinate;
      Z = TGraphCoordinate;
      TPlaneCoord = TPair<X, Y>;
      TPlane = TDictionary<TPlaneCoord, TGraphEntry>;
      {$IFDEF PAS2JS}
      TPlanes = TObjectDictionary<Z, TPlane>;
      //pas2js cannot alias this nested generic reliably, so expose the same
      //constructible collection surface through a thin descendant.
      TPlanesList = class(TObjectList<TPlanes>);
      {$ELSE}
      //retain the exact native types exposed by the original public API
      TPlanes = TDictionary<Z, TPlane>;
      TPlanesList = TObjectList<TPlanes>;
      {$ENDIF}

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
          const AValue: TGraphValue; const ARequireRule : Boolean); override;
        procedure DoRequirePrevious(
          const AValue: TGraphValue); override;
        procedure SynchronizeInverseRules;
      public
        property Parent : TGraph read FParent write FParent;
      end;
  protected
    type
      TPassList = TObjectList<TGraph>;
      TPassLookup = TDictionary<String, Integer>;
  strict private
    type
      TEntryStorageArray = array of TGraphEntries;
      TPlaneStorageArray = array of TPlanes;
      TRandomState = record
        S0: Cardinal;
        S1: Cardinal;
        S2: Cardinal;
        S3: Cardinal;
      end;
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
    FPassRoot : TGraph;
    FPassIndex : Integer;
    FInitializingPass: Boolean;
    FRunning: Boolean;
    FSeed: TGraphSeed;
    FSeedInitialized: Boolean;
    FRandomState: TRandomState;
    FExecutingPassIndex: Integer;

    function AddUInt32(const A, B: Cardinal): Cardinal;
    function MultiplyUInt32(const A, B: Cardinal): Cardinal;
    function RotateLeft32(const AValue: Cardinal;
      const ACount: Integer): Cardinal;
    function MixSeedWord(const AValue: Cardinal): Cardinal;
    procedure SeedRandomState(const ASeed: TGraphSeed;
      out AState: TRandomState);
    function AdvanceRandomState(var AState: TRandomState): Cardinal;
    procedure JumpRandomState(var AState: TRandomState);
    procedure BuildPassRandomState(const APassIndex: Integer;
      out AState: TRandomState);
    procedure EnsureSeedInitialized;
    procedure RewindRandomStates;
    procedure EnsureInitialPass;
    function NewPlanes: TPlanes;
    function GetActivePassGraph: TGraph;
    procedure BuildStorage(const AWidth, AHeight, ADepth: TGraphCoordinate;
      out AEntries: TGraphEntries; out APlanes: TPlanes);
    procedure ClearGeneratedValues;
    function GetEntry(const X, Y, Z : TGraphCoordinate): TGraphEntry;
    function CoordToIndex(const X, Y, Z : TGraphCoordinate) : Integer;
    function CoordToIndexFor(const X, Y, Z, AWidth,
      AHeight: TGraphCoordinate): Integer;
    function GetInvalidStateCallback: TInvalidStateCallback;
    function GetPass: String;
    function GetPassGraph(const AIndex : Integer): TGraph;
    function GetPassIndex: Integer;
    function GetPlanes: TPlanes;
    function GetRuleGroup(const AValue : TGraphValue): TParentedGraphRuleGroup;
    function GetRuleGroups: TGraphRuleGroups;
    function GetSelectionCallback: TValueSelectionCallback;
    function GetSeed: TGraphSeed;
    function GetTotalPassCount: Integer;
    function InBounds(const AIndex : Integer) : Boolean;
    procedure LinkNeighbors;
    procedure LinkNeighborsFor(const AEntries: TGraphEntries;
      const ADimension: TDimension; const AWrap: Boolean);
    procedure SetInvalidStateCallback(const AValue: TInvalidStateCallback);
    procedure SetMode(const AValue: TGraphRunMode);
    procedure SetPass(const AValue: String);
    procedure SetSelectionCallback(const AValue: TValueSelectionCallback);
    procedure SetSeed(const AValue: TGraphSeed);
    procedure SetWrapNeighbors(const AValue: Boolean);
    procedure CopyValuesFrom(const ASource: TGraph);
    function HasDefinition: Boolean;
    procedure InitializeStorage;
    function ReshapeOne(const AWidth, AHeight,
      ADepth: TGraphCoordinate): TGraph;
    function RunOnePass: TGraph;
    procedure ValidateAssignedEntry(const AEntry: TGraphEntry;
      const Z, APrevZ: TGraphCoordinate);
    procedure ValidateDimensions(const AWidth, AHeight,
      ADepth: TGraphCoordinate);
  strict protected
    function DoCreateEntry: TGraphEntry; virtual;
    function DoCreatePass(const APassIndex: Integer): TGraph; virtual;
    //initialize fields owned by a derived graph on each pass instance
    procedure DoInitializePass; virtual;
    function PassLabelFromIndex(const AIndex : Integer) : String;
    function DoHandleInvalidState(const AEntry : TGraphEntry) : TGraphValue;
    procedure DoGetStartCoord(out X, Y : TGraphCoordinate); virtual;
    function DoGetSelection(const AEntry : TGraphEntry;
      const Z, APrevZ : TGraphCoordinate) : TGraphValue; virtual;

    (*
      can be overridden to validate the rules that are allowed for a graph entry
    *)
    procedure DoValidate(const AEntry : TGraphEntry;
      const Z, APrevZ : TGraphCoordinate; out Values : TGraphValues); virtual;
  public
    (*
      all parented rule groups defined in the graph
    *)
    property RuleGroups : TGraphRuleGroups read GetRuleGroups;

    (*
      callback that can be set to determine selection of valid values
      after rules have been run
    *)
    property SelectionCallback : TValueSelectionCallback read GetSelectionCallback write SetSelectionCallback;

    (*
      when an invalid state occurs for selecting values, this callback will
      be called and give a chance to change the value to use
    *)
    property InvalidStateCallback : TInvalidStateCallback read GetInvalidStateCallback write SetInvalidStateCallback;

    (*
      returns graph entry by (x, y, z) coordinates
    *)
    property Entry[const X, Y, Z : TGraphCoordinate] : TGraphEntry read GetEntry; default;

    (*
      gets the rule group for the value
    *)
    property Rules[const AValue : TGraphValue] : TParentedGraphRuleGroup read GetRuleGroup;

    (*
      2D planes "stacked" in the Z direction
    *)
    property Planes : TPlanes read GetPlanes;

    (*
      when enabled, neighbor assignment for entries on the external bounds
      of the graph will "wrap" around. this will result in neighbors never
      being nil, and can result in an entry to be a neighbor to itself.
      if this is not desired set this to false, but nil checking will have
      to be done before accessing member variables
    *)
    property WrapNeighbors : Boolean read FWrap write SetWrapNeighbors default True;

    (*
      controls the behavior for selecting plane processing order during
      running the graph
    *)
    property Mode : TGraphRunMode read FMode write SetMode default rmBottomUp;

    (*
      pipeline seed used by the built-in portable random source. every pass
      receives a stable independent stream derived from this seed and its
      zero-based pass index. unless explicitly assigned first, an automatic
      seed is captured on the first read or pass materialization
    *)
    property Seed : TGraphSeed read GetSeed write SetSeed;

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
      returns a pass graph without changing CurrentPass
    *)
    property PassGraph[const AIndex : Integer] : TGraph read GetPassGraph;

    (*
      reshapes the dimension of this graph
        @AWidth - X units, 1 based
        @AHeight - Y units, 1 based
        @ADepth - Z units, 1 based
        @Result - return "this" graph instance
    *)
    function Reshape(const AWidth, AHeight,
      ADepth : TGraphCoordinate) : TGraph;

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
    function ForEachPass(const ACallback : TForEachPassCallback) : TGraph; overload;
    function ForEachPass(const ACallback : TForEachPassMethod) : TGraph; overload;
    {$IFNDEF PAS2JS}
    function ForEachPass(
      const ACallback : TForEachPassNestedCallback) : TGraph; overload;
    {$ENDIF}

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
      returns an unbiased value in 0..Pred(ACount) from the current pass's
      portable stream. callbacks should use this instead of System.Random
      when replay across native FPC and pas2js matters
    *)
    function RandomIndex(const ACount: Integer): Integer;

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

    //used by the virtual pass factory so derived graph classes are preserved
    constructor CreatePass(const ARoot: TGraph;
      const APassIndex: Integer); virtual;
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

type
  TGraphTraversalFrame = record
    Entry: TGraphEntry;
    NextDirection: Integer;
  end;

{$IFNDEF PAS2JS}
type
  //TPlanes remains the original non-owning dictionary type for native source
  //compatibility. Graph-created instances use this private owner so plane
  //values are still released with their dictionary.
  TOwnedGraphPlanes = class(TGraph.TPlanes)
  public
    destructor Destroy; override;
  end;

destructor TOwnedGraphPlanes.Destroy;
var
  LPlane: TGraph.TPlane;
begin
  for LPlane in Values do
    LPlane.Free;
  inherited Destroy;
end;
{$ENDIF}

function DefSelCall(const AGraph : TGraph; const AEntry : TGraphEntry;
  const AValid : TGraphValues) : TGraphValue;
begin
  if Length(AValid) < 1 then
    Result := AEntry.Value
  else
    Result := AValid[AGraph.RandomIndex(Length(AValid))];
end;

function AutomaticGraphSeed: TGraphSeed;
begin
  //Retain the original stochastic-by-default behavior without ever using the
  //RTL random source during generation. Persist Graph.Seed to replay a run.
  Result := Cardinal(System.Random($10000))
    or (Cardinal(System.Random($10000)) shl 16);
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

{ TGraphRule }

{$IFDEF PAS2JS}
constructor TGraphRule.Create(const AKey: TGraphDirection;
  const AValue: TGraphValues; const AInfo: TRequireRule);
begin
  Key := AKey;
  Value := AValue;
  Info := AInfo;
end;
{$ENDIF}

{ TGraph.TParentedGraphRuleGroup }

procedure TGraph.TParentedGraphRuleGroup.DoNewRule(
  const ADirections: TGraphDirections; const AValue: TGraphValue;
  const ARequireRule: Boolean);
var
  LDir: TGraphDirection;
  LRule: TGraphRule;
  LTargetValue: TGraphValue;
begin
  inherited DoNewRule(ADirections, AValue, ARequireRule);

  //Ensure every referenced value has a group before synchronizing. The
  //fixed-point walk can then update rules without mutating the dictionary it
  //is enumerating.
  for LDir in ADirections do
  begin
    LRule := Rule[LDir];
    for LTargetValue in LRule.Value do
      Parent.AddValue(LTargetValue);
  end;

  SynchronizeInverseRules;
end;

procedure TGraph.TParentedGraphRuleGroup.SynchronizeInverseRules;
var
  I: Integer;
  LBeforeContains: Boolean;
  LBeforeInfo: Boolean;
  LBaseGroup: TGraphRuleGroup;
  LChanged: Boolean;
  LDir: TGraphDirection;
  LGroup: TParentedGraphRuleGroup;
  LHadDirection: Boolean;
  LInverseDir: TGraphDirection;
  LRule: TGraphRule;
  LRuleCount: Integer;
  LReferencedValues: TGraphValues;
  LTarget: TParentedGraphRuleGroup;
  LTargetValue: TGraphValue;
begin
  //Required metadata is stored per direction, not per edge. Promoting one
  //edge therefore promotes every value in that direction, which can in turn
  //promote another inverse direction. Iterate to a fixed point so the public
  //bidirectional rule model remains symmetric through the whole closure.
  SetLength(LReferencedValues, 0);
  for LBaseGroup in Parent.RuleGroups.Values do
  begin
    LGroup := TParentedGraphRuleGroup(LBaseGroup);
    if not ContainsGraphValue(LReferencedValues, LGroup.Value) then
      Insert(LGroup.Value, LReferencedValues, Length(LReferencedValues));
    for I := 0 to High(LGroup.Rules) do
    begin
      LRule := LGroup.Rules[I];
      for LTargetValue in LRule.Value do
        if not ContainsGraphValue(LReferencedValues, LTargetValue) then
          Insert(LTargetValue, LReferencedValues,
            Length(LReferencedValues));
    end;
  end;
  for LTargetValue in LReferencedValues do
    Parent.AddValue(LTargetValue);

  repeat
    LChanged := False;
    for LBaseGroup in Parent.RuleGroups.Values do
    begin
      LGroup := TParentedGraphRuleGroup(LBaseGroup);
      LRuleCount := Length(LGroup.Rules);
      for I := 0 to Pred(LRuleCount) do
      begin
        LRule := LGroup.Rules[I];
        LDir := LRule.Key;
        LInverseDir := InverseOfDir(LDir);
        for LTargetValue in LRule.Value do
        begin
          LTarget := TParentedGraphRuleGroup(
            Parent.RuleGroups[LTargetValue]);
          LHadDirection := LTarget.Exists[LInverseDir];
          if LHadDirection then
          begin
            LBeforeContains := ContainsGraphValue(
              LTarget[LInverseDir].Value, LGroup.Value);
            LBeforeInfo := LTarget[LInverseDir].Info;
          end
          else
          begin
            LBeforeContains := False;
            LBeforeInfo := False;
          end;

          LTarget.UpsertRule([LInverseDir], LGroup.Value, LRule.Info);
          if (not LHadDirection) or (not LBeforeContains)
            or (LRule.Info and (not LBeforeInfo)) then
            LChanged := True;
        end;
      end;
    end;
  until not LChanged;
end;

procedure TGraph.TParentedGraphRuleGroup.DoRequirePrevious(
  const AValue: TGraphValue);
begin
  if Assigned(Parent) and (Parent.CurrentPassIndex = 0) then
    raise EInvalidOperation.Create(
      'RequirePrevious::pass zero has no preceding pass');
  inherited DoRequirePrevious(AValue);
end;

{ TGraphRuleGroup }

function TGraphRuleGroup.GetExists(const ADirection : TGraphDirection): Boolean;
begin
  Result := IndexOfDirection(ADirection) >= 0;
end;

function TGraphRuleGroup.GetHasRequired: Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FRules) do
    if TRequireRule(FRules[I].Info) then
      Exit(True);
  Exit(False);
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
  const AValue: TGraphValue; const ARequireRule: Boolean);
begin
  UpsertRule(ADirections, AValue, ARequireRule);
end;

procedure TGraphRuleGroup.UpsertRule(const ADirections: TGraphDirections;
  const AValue: TGraphValue; const ARequireRule: Boolean);
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
      LRule.Info := LRule.Info or ARequireRule;
    end
    //otherwise no direction/rules set so insert to end of rules
    else
    begin
      I := Length(FRules);
      LRule.Key := LDir;
      LRule.Value := Default(TGraphValues);
      LRule.Info := ARequireRule;
    end;

    //insert rule value if we haven't already done so
    if not ContainsGraphValue(LVals, AValue) then
    begin
      Insert(AValue, LVals, Length(LVals));
      LRule.Value := LVals;
    end;

    //upsert without duplicating an existing direction
    if I < Length(FRules) then
      FRules[I] := LRule
    else
    begin
      //Appending through SetLength gives pas2js a distinct record slot for
      //every direction. Insert otherwise aliases its record object when one
      //NewRule call contains multiple directions.
      SetLength(FRules, Succ(I));
      FRules[I] := LRule;
    end;
  end;
end;

procedure TGraphRuleGroup.DoRequirePrevious(const AValue: TGraphValue);
begin
  if not ContainsGraphValue(FPreviousValues, AValue) then
    Insert(AValue, FPreviousValues, Length(FPreviousValues));
end;


function TGraphRuleGroup.NewRule(const ADirections: TGraphDirections;
  const AValue: TGraphValue; const ARequireRule: Boolean): TGraphRuleGroup;
begin
  Result := Self;
  DoNewRule(ADirections, AValue, ARequireRule);
end;

function TGraphRuleGroup.NewRule(const ADirections: TGraphDirections;
  const AValues: TGraphValues; const ARequireRule: Boolean): TGraphRuleGroup;
var
  I: Integer;
begin
  Result := Self;

  for I := 0 to High(AValues) do
    NewRule(ADirections, AValues[I], ARequireRule);
end;

function TGraphRuleGroup.RequirePrevious(
  const AValue: TGraphValue): TGraphRuleGroup;
begin
  Result := Self;
  DoRequirePrevious(AValue);
end;

function TGraphRuleGroup.RequirePrevious(
  const AValues: TGraphValues): TGraphRuleGroup;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to High(AValues) do
    DoRequirePrevious(AValues[I]);
end;

constructor TGraphRuleGroup.Create;
begin
  FVal := '';
  SetLength(FRules, 0);
  SetLength(FPreviousValues, 0);
end;

constructor TGraphRuleGroup.Create(const AValue: TGraphValue);
begin
  Create;
  FVal := AValue;
end;

{ TGraphEntry }

procedure TGraphEntry.InitializePosition(const AIndex: Integer;
  const AX, AY, AZ: TGraphCoordinate);
begin
  FIndex := AIndex;
  FPosition.X := AX;
  FPosition.Y := AY;
  FPosition.Z := AZ;
end;

procedure TGraphEntry.ClearValue;
begin
  if FEmpty then
  begin
    FGenerated := False;
    Exit;
  end;

  DoBeforeSetValue(TGraphValue.Empty);
  FVal := TGraphValue.Empty;
  FEmpty := True;
  FGenerated := False;
  DoAfterSetValue(FVal);
end;

procedure TGraphEntry.AssignValue(const AValue: TGraphValue;
  const AGenerated: Boolean);
begin
  //don't trigger when we aren't changing value
  if AValue = FVal then
  begin
    if not FEmpty then
      FGenerated := AGenerated;
    Exit;
  end;

  DoBeforeSetValue(AValue);
  FVal := AValue;
  FEmpty := False;
  FGenerated := AGenerated;
  DoAfterSetValue(AValue);
end;

procedure TGraphEntry.SetValue(const AValue: TGraphValue);
begin
  AssignValue(AValue, False);
end;

procedure TGraphEntry.SetGeneratedValue(const AValue: TGraphValue);
begin
  AssignValue(AValue, True);
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
  FGenerated := False;
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
  FIndex := -1;
  Reset;
  FID := DoGenerateID;
end;

{ TGraph }

function TGraph.AddUInt32(const A, B: Cardinal): Cardinal;
var
  LHigh: Cardinal;
  LLow: Cardinal;
begin
  //Use 16-bit lanes so checked native builds and JavaScript agree on the
  //low 32 bits of wrapping addition.
  LLow := (A and $FFFF) + (B and $FFFF);
  LHigh := (A shr 16) + (B shr 16) + (LLow shr 16);
  Result := ((LHigh and $FFFF) shl 16) or (LLow and $FFFF);
end;

function TGraph.MultiplyUInt32(const A, B: Cardinal): Cardinal;
var
  LAHigh, LALow: Cardinal;
  LBHigh, LBLow: Cardinal;
  LCross: Cardinal;
  LLowProduct: Cardinal;
begin
  //Only the low 32 bits are required by Murmur3 fmix32. Every intermediate
  //product stays below 2^32, which avoids precision loss in pas2js.
  LALow := A and $FFFF;
  LAHigh := A shr 16;
  LBLow := B and $FFFF;
  LBHigh := B shr 16;
  LLowProduct := LALow * LBLow;
  LCross := ((LALow * LBHigh) and $FFFF)
    + ((LAHigh * LBLow) and $FFFF);
  Result := AddUInt32(LLowProduct, (LCross and $FFFF) shl 16);
end;

function TGraph.RotateLeft32(const AValue: Cardinal;
  const ACount: Integer): Cardinal;
begin
  Result := (AValue shl ACount) or (AValue shr (32 - ACount));
end;

function TGraph.MixSeedWord(const AValue: Cardinal): Cardinal;
begin
  //MurmurHash3 fmix32 provides a portable avalanche from the public scalar
  //seed into each lane of the xoshiro state.
  Result := AValue xor (AValue shr 16);
  Result := MultiplyUInt32(Result, $85EBCA6B);
  Result := Result xor (Result shr 13);
  Result := MultiplyUInt32(Result, $C2B2AE35);
  Result := Result xor (Result shr 16);
end;

procedure TGraph.SeedRandomState(const ASeed: TGraphSeed;
  out AState: TRandomState);
begin
  AState.S0 := MixSeedWord(ASeed xor $A511E9B3);
  AState.S1 := MixSeedWord(ASeed xor $63D83595);
  AState.S2 := MixSeedWord(ASeed xor $B8D9C6AB);
  AState.S3 := MixSeedWord(ASeed xor $9E3779B9);
end;

function TGraph.AdvanceRandomState(var AState: TRandomState): Cardinal;
var
  T: Cardinal;
begin
  //xoshiro128++ 1.0 by David Blackman and Sebastiano Vigna. Its reference
  //implementation is dedicated to the public domain. AddUInt32 supplies the
  //specified uint32 wrapping behavior on both supported targets.
  Result := AddUInt32(
    RotateLeft32(AddUInt32(AState.S0, AState.S3), 7), AState.S0);
  T := AState.S1 shl 9;

  AState.S2 := AState.S2 xor AState.S0;
  AState.S3 := AState.S3 xor AState.S1;
  AState.S1 := AState.S1 xor AState.S2;
  AState.S0 := AState.S0 xor AState.S3;
  AState.S2 := AState.S2 xor T;
  AState.S3 := RotateLeft32(AState.S3, 11);
end;

procedure TGraph.JumpRandomState(var AState: TRandomState);
const
  JUMP: array[0..3] of Cardinal = (
    $8764000B, $F542D2D3, $6FA035C3, $77F2DB5B);
var
  B, I: Integer;
  LS0, LS1, LS2, LS3: Cardinal;
begin
  //The reference jump is equivalent to 2^64 calls and gives each pass a
  //non-overlapping subsequence while retaining stable index-based identity.
  LS0 := 0;
  LS1 := 0;
  LS2 := 0;
  LS3 := 0;
  for I := 0 to High(JUMP) do
    for B := 0 to 31 do
    begin
      if (JUMP[I] and (Cardinal(1) shl B)) <> 0 then
      begin
        LS0 := LS0 xor AState.S0;
        LS1 := LS1 xor AState.S1;
        LS2 := LS2 xor AState.S2;
        LS3 := LS3 xor AState.S3;
      end;
      AdvanceRandomState(AState);
    end;

  AState.S0 := LS0;
  AState.S1 := LS1;
  AState.S2 := LS2;
  AState.S3 := LS3;
end;

procedure TGraph.BuildPassRandomState(const APassIndex: Integer;
  out AState: TRandomState);
var
  I: Integer;
begin
  if APassIndex < 0 then
    raise ERangeError.CreateFmt(
      'BuildPassRandomState::invalid pass index [%d]', [APassIndex]);

  EnsureSeedInitialized;
  SeedRandomState(FSeed, AState);
  for I := 1 to APassIndex do
    JumpRandomState(AState);
end;

procedure TGraph.EnsureSeedInitialized;
begin
  if Assigned(FPassRoot) then
  begin
    FPassRoot.EnsureSeedInitialized;
    FSeed := FPassRoot.FSeed;
    FSeedInitialized := True;
    Exit;
  end;

  if FSeedInitialized then
    Exit;
  FSeed := AutomaticGraphSeed;
  FSeedInitialized := True;
  SeedRandomState(FSeed, FRandomState);
end;

procedure TGraph.RewindRandomStates;
var
  I: Integer;
  LState: TRandomState;
begin
  if Assigned(FPassRoot) then
  begin
    FPassRoot.RewindRandomStates;
    Exit;
  end;

  EnsureSeedInitialized;
  SeedRandomState(FSeed, LState);
  for I := 0 to Pred(FPasses.Count) do
  begin
    FPasses[I].FSeed := FSeed;
    FPasses[I].FSeedInitialized := True;
    FPasses[I].FRandomState := LState;
    JumpRandomState(LState);
  end;
end;

procedure TGraph.EnsureInitialPass;
var
  LGraph: TGraph;
begin
  if Assigned(FPassRoot) or (FPasses.Count > 0) then
    Exit;
  if FInitializingPass then
    raise EInvalidOperation.Create(
      'EnsureInitialPass::recursive pass construction is not supported');

  FInitializingPass := True;
  LGraph := nil;
  try
    LGraph := DoCreatePass(0);
    FPassLookup.Add('', 0);
    try
      FPasses.Add(LGraph);
    except
      FPassLookup.Remove('');
      raise;
    end;
    LGraph := nil;

    try
      FPasses[0].DoInitializePass;
    except
      FPasses.Delete(0);
      FPassLookup.Remove('');
      raise;
    end;
  finally
    LGraph.Free;
    FInitializingPass := False;
  end;
end;

function TGraph.NewPlanes: TPlanes;
begin
  {$IFDEF PAS2JS}
  Result := TPlanes.Create([doOwnsValues]);
  {$ELSE}
  Result := TOwnedGraphPlanes.Create;
  {$ENDIF}
end;

function TGraph.GetActivePassGraph: TGraph;
begin
  //child graphs always operate on their own pass-local storage
  if Assigned(FPassRoot) then
    Exit(Self);

  EnsureInitialPass;
  Result := FPasses[FCurPassIndex];
end;

function TGraph.GetEntry(const X, Y, Z : TGraphCoordinate): TGraphEntry;
var
  LGraph: TGraph;
begin
  LGraph := GetActivePassGraph;
  if LGraph <> Self then
    Exit(LGraph.GetEntry(X, Y, Z));

  if (X >= FDimension.Width)
    or (Y >= FDimension.Height)
    or (Z >= FDimension.Depth) then
    raise ERangeError.CreateFmt(
      'GetEntry::coordinates out of bounds [x]-%d, [y]-%d, [z]-%d',
      [X, Y, Z]);

  //quicker lookup then using the planes collection
  Result := FEntries[CoordToIndex(X, Y, Z)];
end;

function TGraph.CoordToIndex(const X, Y, Z: TGraphCoordinate): Integer;
begin
  //index into the "flattened" graph for a quicker lookup than going
  //through the planes collection
  Result := CoordToIndexFor(X, Y, Z, FDimension.Width,
    FDimension.Height);
end;

function TGraph.CoordToIndexFor(const X, Y, Z, AWidth,
  AHeight: TGraphCoordinate): Integer;
begin
  Result := (AWidth * AHeight * Z) + X + (Y * AWidth);
end;

function TGraph.GetInvalidStateCallback: TInvalidStateCallback;
begin
  Result := GetActivePassGraph.FInv;
end;

function TGraph.GetPass: String;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.PassLabelFromIndex(FPassIndex));

  EnsureInitialPass;
  Result := FCurPass;
end;

function TGraph.GetPassGraph(const AIndex: Integer): TGraph;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.GetPassGraph(AIndex));

  EnsureInitialPass;
  if (AIndex < 0) or (AIndex >= TotalPassCount) then
    raise ERangeError.CreateFmt(
      'GetPassGraph::index out of bounds [%d]', [AIndex]);

  Result := FPasses[AIndex];
end;

function TGraph.GetPassIndex: Integer;
begin
  if Assigned(FPassRoot) then
    Exit(FPassIndex);

  EnsureInitialPass;
  Result := FCurPassIndex;
end;

function TGraph.GetPlanes: TPlanes;
begin
  Result := GetActivePassGraph.FPlanes;
end;

function TGraph.GetRuleGroup(const AValue : TGraphValue): TParentedGraphRuleGroup;
begin
  Result := TParentedGraphRuleGroup(GetActivePassGraph.FRuleGroups[AValue]);
end;

function TGraph.GetRuleGroups: TGraphRuleGroups;
begin
  Result := GetActivePassGraph.FRuleGroups;
end;

function TGraph.GetSelectionCallback: TValueSelectionCallback;
begin
  Result := GetActivePassGraph.FSel;
end;

function TGraph.GetSeed: TGraphSeed;
begin
  if Assigned(FPassRoot) then
    Result := FPassRoot.GetSeed
  else
  begin
    EnsureSeedInitialized;
    Result := FSeed;
  end;
end;

function TGraph.GetTotalPassCount: Integer;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.GetTotalPassCount);

  EnsureInitialPass;
  Result := FPasses.Count;
end;

function TGraph.InBounds(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FEntries.Count);
end;

procedure TGraph.LinkNeighbors;
begin
  LinkNeighborsFor(FEntries, FDimension, FWrap);
end;

procedure TGraph.LinkNeighborsFor(const AEntries: TGraphEntries;
  const ADimension: TDimension; const AWrap: Boolean);
var
  LEntry: TGraphEntry;
  X, Y, Z: Integer;
begin
  if (ADimension.Width = 0)
    or (ADimension.Height = 0)
    or (ADimension.Depth = 0) then
    Exit;

  for Z := 0 to Integer(ADimension.Depth) - 1 do
    for Y := 0 to Integer(ADimension.Height) - 1 do
      for X := 0 to Integer(ADimension.Width) - 1 do
      begin
        LEntry := AEntries[CoordToIndexFor(X, Y, Z,
          ADimension.Width, ADimension.Height)];

        if Y + 1 < ADimension.Height then
          LEntry[gdNorth] := AEntries[CoordToIndexFor(X, Y + 1, Z,
            ADimension.Width, ADimension.Height)]
        else if AWrap then
          LEntry[gdNorth] := AEntries[CoordToIndexFor(X, 0, Z,
            ADimension.Width, ADimension.Height)]
        else
          LEntry[gdNorth] := nil;

        if X + 1 < ADimension.Width then
          LEntry[gdEast] := AEntries[CoordToIndexFor(X + 1, Y, Z,
            ADimension.Width, ADimension.Height)]
        else if AWrap then
          LEntry[gdEast] := AEntries[CoordToIndexFor(0, Y, Z,
            ADimension.Width, ADimension.Height)]
        else
          LEntry[gdEast] := nil;

        if Y > 0 then
          LEntry[gdSouth] := AEntries[CoordToIndexFor(X, Y - 1, Z,
            ADimension.Width, ADimension.Height)]
        else if AWrap then
          LEntry[gdSouth] := AEntries[CoordToIndexFor(
            X, ADimension.Height - 1, Z,
            ADimension.Width, ADimension.Height)]
        else
          LEntry[gdSouth] := nil;

        if X > 0 then
          LEntry[gdWest] := AEntries[CoordToIndexFor(X - 1, Y, Z,
            ADimension.Width, ADimension.Height)]
        else if AWrap then
          LEntry[gdWest] := AEntries[CoordToIndexFor(
            ADimension.Width - 1, Y, Z,
            ADimension.Width, ADimension.Height)]
        else
          LEntry[gdWest] := nil;

        if Z + 1 < ADimension.Depth then
          LEntry[gdUp] := AEntries[CoordToIndexFor(X, Y, Z + 1,
            ADimension.Width, ADimension.Height)]
        else if AWrap then
          LEntry[gdUp] := AEntries[CoordToIndexFor(X, Y, 0,
            ADimension.Width, ADimension.Height)]
        else
          LEntry[gdUp] := nil;

        if Z > 0 then
          LEntry[gdDown] := AEntries[CoordToIndexFor(X, Y, Z - 1,
            ADimension.Width, ADimension.Height)]
        else if AWrap then
          LEntry[gdDown] := AEntries[CoordToIndexFor(
            X, Y, ADimension.Depth - 1,
            ADimension.Width, ADimension.Height)]
        else
          LEntry[gdDown] := nil;
      end;
end;

procedure TGraph.SetInvalidStateCallback(const AValue: TInvalidStateCallback);
begin
  GetActivePassGraph.FInv := AValue;
end;

procedure TGraph.SetMode(const AValue: TGraphRunMode);
var
  I: Integer;
begin
  if Assigned(FPassRoot) then
  begin
    FPassRoot.SetMode(AValue);
    Exit;
  end;
  if FInitializingPass then
    raise EInvalidOperation.Create(
      'SetMode::cannot change pipeline settings during pass initialization');

  EnsureInitialPass;
  FMode := AValue;
  for I := 0 to Pred(FPasses.Count) do
    FPasses[I].FMode := AValue;
end;

procedure TGraph.SetPass(const AValue: String);
var
  LPair : TPair<String, Integer>;
  LPreviousLabel: String;
begin
  if Assigned(FPassRoot) then
  begin
    if FPassRoot.FInitializingPass then
      raise EInvalidOperation.Create(
        'SetPass::cannot rename a pass during pass initialization');
    LPreviousLabel := FPassRoot.PassLabelFromIndex(FPassIndex);
    if AValue = LPreviousLabel then
      Exit;
    if FPassRoot.FPassLookup.ContainsKey(AValue) then
      raise Exception.Create('SetPass::pass label is already in use');

    LPair := FPassRoot.FPassLookup.ExtractPair(LPreviousLabel);
    LPair.Key := AValue;
    FPassRoot.FPassLookup.Add(LPair.Key, LPair.Value);
    if FPassRoot.FCurPassIndex = FPassIndex then
      FPassRoot.FCurPass := AValue;
    Exit;
  end;
  if FInitializingPass then
    raise EInvalidOperation.Create(
      'SetPass::cannot rename a pass during pass initialization');

  EnsureInitialPass;
  //if the requested label is different than what we have, extract and update
  if AValue <> FCurPass then
  begin
    //dupes not allowed
    if FPassLookup.ContainsKey(AValue) then
      raise Exception.Create('SetPass::pass label is already in use');

    LPair := FPassLookup.ExtractPair(FCurPass);
    LPair.Key := AValue;
    FPassLookup.Add(LPair.Key, LPair.Value);

    //lastly update the current pass
    FCurPass := AValue;
  end;
end;

procedure TGraph.SetSelectionCallback(const AValue: TValueSelectionCallback);
begin
  GetActivePassGraph.FSel := AValue;
end;

procedure TGraph.SetSeed(const AValue: TGraphSeed);
begin
  if Assigned(FPassRoot) then
  begin
    FPassRoot.SetSeed(AValue);
    Exit;
  end;
  if FInitializingPass then
    raise EInvalidOperation.Create(
      'SetSeed::cannot change the pipeline seed during pass initialization');
  if FRunning then
    raise EInvalidOperation.Create(
      'SetSeed::cannot change the pipeline seed while it is running');

  FSeed := AValue;
  FSeedInitialized := True;
  SeedRandomState(FSeed, FRandomState);
  if FPasses.Count > 0 then
    RewindRandomStates;
end;

procedure TGraph.SetWrapNeighbors(const AValue: Boolean);
var
  I: Integer;
begin
  if Assigned(FPassRoot) then
  begin
    FPassRoot.SetWrapNeighbors(AValue);
    Exit;
  end;
  if FInitializingPass then
    raise EInvalidOperation.Create(
      'SetWrapNeighbors::cannot change pipeline settings during pass initialization');
  if FRunning then
    raise EInvalidOperation.Create(
      'SetWrapNeighbors::cannot relink the graph while it is running');

  EnsureInitialPass;
  FWrap := AValue;
  for I := 0 to Pred(FPasses.Count) do
  begin
    FPasses[I].FWrap := AValue;
    FPasses[I].LinkNeighbors;
  end;
end;

procedure TGraph.CopyValuesFrom(const ASource: TGraph);
var
  I: Integer;
begin
  if FEntries.Count <> ASource.FEntries.Count then
    raise EInvalidOperation.Create(
      'CopyValuesFrom::source and destination dimensions do not match');

  for I := 0 to Pred(FEntries.Count) do
  begin
    //caller-assigned destination values are locks; a definitionless pass
    //refreshes only its generated snapshot cells
    if (not FEntries[I].Empty) and (not FEntries[I].Generated) then
      Continue;

    if ASource.FEntries[I].Empty then
      FEntries[I].ClearValue
    else
      FEntries[I].SetGeneratedValue(ASource.FEntries[I].Value);
  end;
end;

procedure TGraph.ClearGeneratedValues;
var
  I: Integer;
begin
  for I := 0 to Pred(FEntries.Count) do
    if FEntries[I].Generated then
      FEntries[I].ClearValue;
end;

function TGraph.HasDefinition: Boolean;
begin
  Result := (Length(FValues) > 0) or (FRuleGroups.Count > 0);
end;

function TGraph.PassLabelFromIndex(const AIndex: Integer): String;
var
  LPair : TPair<String, Integer>;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.PassLabelFromIndex(AIndex));

  EnsureInitialPass;
  Result := '';
  for LPair in FPassLookup do
    if LPair.Value = AIndex then
      Exit(LPair.Key);

  raise Exception.Create('PassLabelFromIndex::index out of bounds [' + IntToStr(AIndex) + ']');
end;

function TGraph.DoCreateEntry: TGraphEntry;
begin
  Result := TGraphEntry.Create;
end;

function TGraph.DoCreatePass(const APassIndex: Integer): TGraph;
begin
  Result := TGraphClass(ClassType).CreatePass(Self, APassIndex);
end;

procedure TGraph.DoInitializePass;
begin
  //derived graphs can initialize pass-local fields here; this hook runs for
  //pass zero, later passes, and the replacement pass created by Reset
end;

function TGraph.DoHandleInvalidState(const AEntry: TGraphEntry): TGraphValue;
var
  LCallbackGraph: TGraph;
begin
  //default to the current value
  Result := AEntry.Value;

  //when we have the callback assigned then use it
  if Assigned(FInv) then
  begin
    if Assigned(FPassRoot) then
    begin
      LCallbackGraph := FPassRoot;
      FPassRoot.FCurPassIndex := FPassIndex;
      FPassRoot.FCurPass := FPassRoot.PassLabelFromIndex(FPassIndex);
      try
        FInv(LCallbackGraph, AEntry, Result);
      finally
        //callbacks may inspect or switch passes, but solving resumes in the
        //pass that owns this entry
        FPassRoot.FCurPassIndex := FPassIndex;
        FPassRoot.FCurPass := FPassRoot.PassLabelFromIndex(FPassIndex);
      end;
    end
    else
    begin
      LCallbackGraph := Self;
      FInv(LCallbackGraph, AEntry, Result);
    end;
  end;
end;

procedure TGraph.DoGetStartCoord(out X, Y: TGraphCoordinate);
begin
  //The base traversal uses the pass-owned stream. Derived implementations can
  //override this, but should use RandomIndex to retain replay guarantees.
  X := RandomIndex(Integer(FDimension.Width));
  Y := RandomIndex(Integer(FDimension.Height));
end;

function TGraph.DoGetSelection(const AEntry: TGraphEntry; const Z,
  APrevZ: TGraphCoordinate): TGraphValue;
var
  LCallbackGraph: TGraph;
  LValues: TGraphValues;
begin
  if not Assigned(FSel) then
    raise Exception.Create('DoGetSelection::selection callback cannot be nil');

  //get the valid rules for this entry
  DoValidate(AEntry, Z, APrevZ, LValues);

  //An empty domain is a contradiction. Give the legacy invalid-state hook a
  //chance to repair mutable model state, then validate its proposed value
  //against the refreshed domain. Never commit an empty or invented value.
  if Length(LValues) < 1 then
  begin
    Result := DoHandleInvalidState(AEntry);
    //The callback receives the legacy mutable entry object. Its scalar result
    //is the proposal; direct entry writes are not a side channel around domain
    //validation and must not leak from a failed recovery.
    if not AEntry.Empty then
      AEntry.ClearValue;
    DoValidate(AEntry, Z, APrevZ, LValues);
    if not ContainsGraphValue(LValues, Result) then
      raise EInvalidOperation.CreateFmt(
        'DoGetSelection::no valid value in pass %d at entry %d',
        [FPassIndex, AEntry.Index]);
    Exit;
  end;

  //pass the rules to the callback for determining the value of this entry
  if Assigned(FPassRoot) then
  begin
    LCallbackGraph := FPassRoot;
    FPassRoot.FCurPassIndex := FPassIndex;
    FPassRoot.FCurPass := FPassRoot.PassLabelFromIndex(FPassIndex);
    try
      Result := FSel(LCallbackGraph, AEntry, LValues);
    finally
      FPassRoot.FCurPassIndex := FPassIndex;
      FPassRoot.FCurPass := FPassRoot.PassLabelFromIndex(FPassIndex);
    end;
  end
  else
  begin
    LCallbackGraph := Self;
    Result := FSel(LCallbackGraph, AEntry, LValues);
  end;

  if not ContainsGraphValue(LValues, Result) then
  begin
    Result := DoHandleInvalidState(AEntry);
    if not ContainsGraphValue(LValues, Result) then
      raise EInvalidOperation.CreateFmt(
        'DoGetSelection::callback returned a value outside the valid domain in pass %d at entry %d',
        [FPassIndex, AEntry.Index]);
  end;
end;

procedure TGraph.DoValidate(const AEntry: TGraphEntry;
  const Z, APrevZ: TGraphCoordinate; out Values: TGraphValues);
var
  I: Integer;
  LHasRequiredConstraint: Boolean;
  LInitialValues: TGraphValues;
  LSelfRequiredValues: TGraphValues;

  (*
    for each neighbor provided, this method will whittle down
    the values out param of invalid states until we're either left with
    valid state(s) or an empty collection (invalid state)
  *)
  procedure TrimValuesForNeighbor(const ANeighbor : TGraphEntry;
    const ADirection : TGraphDirection);
  var
    LGroup : TGraphRuleGroup;
    LRule : TGraphRule;
    LRuleVals, LVals : TGraphValues;
    I: Integer;
  begin
    LVals := Default(TGraphValues);

    //no neighbor, get out
    if not Assigned(ANeighbor) then
      Exit;

    //A wrapped singleton dimension points an entry back to itself. Although
    //the entry is still unassigned, each candidate must support itself across
    //that arc; treating it like an unrelated empty neighbor can commit a
    //locally impossible value.
    if ANeighbor.Empty then
    begin
      if ANeighbor = AEntry then
      begin
        for I := 0 to High(Values) do
        begin
          if not FRuleGroups.ContainsKey(Values[I]) then
          begin
            Insert(Values[I], LVals, Length(LVals));
            Continue;
          end;

          LGroup := FRuleGroups[Values[I]];
          if not LGroup.Exists[ADirection] then
          begin
            Insert(Values[I], LVals, Length(LVals));
            Continue;
          end;

          LRule := LGroup.Rule[ADirection];
          LRuleVals := LRule.Value;
          if Length(LRuleVals) = 0 then
          begin
            Insert(Values[I], LVals, Length(LVals));
            Continue;
          end;

          if ContainsGraphValue(LRuleVals, Values[I]) then
          begin
            Insert(Values[I], LVals, Length(LVals));
            //Unlike an assigned external neighbor, a self-arc can only
            //provide required support to the same candidate value. Keep that
            //support candidate-specific so one alternative cannot authorize
            //an unrelated required-only alternative.
            if TRequireRule(LRule.Info)
              and not ContainsGraphValue(LSelfRequiredValues,
                Values[I]) then
              Insert(Values[I], LSelfRequiredValues,
                Length(LSelfRequiredValues));
          end;
        end;
        Values := LVals;
      end;
      Exit;
    end;

    if not FRuleGroups.ContainsKey(ANeighbor.Value) then
      Exit;

    //get the rule group of the neighbor we'll be using to trim our values with
    LGroup := FRuleGroups[ANeighbor.Value];

    //check to see if the neighbor contains rules for the direction it is (relational to this entry)
    if LGroup.Exists[ADirection] then
    begin
      LRule := LGroup.Rule[ADirection];
      LRuleVals := LGroup[ADirection].Value;

      //note:
      //  no rules, means any state is possible. for users to specifically
      //  state "nothing" should be allowed, a user defined value representing "nothing"
      //  should be introduced
      if Length(LRuleVals) < 1 then
        Exit;

      if TRequireRule(LRule.Info) then
        LHasRequiredConstraint := True;

      //Every active neighbor rule is conjunctive. Starting empty entries from
      //the complete value set lets the first required rule force its values,
      //while intersecting here prevents a later rule from reintroducing a
      //candidate rejected by an earlier neighbor.
      for I := 0 to High(Values) do
        if ContainsGraphValue(LRuleVals, Values[I]) then
          Insert(Values[I], LVals, Length(LVals));

      //lastly, set the output values to our local validated values
      Values := LVals
    end;
  end;

  procedure RemoveUnforcedRequiredValues;
  var
    I: Integer;
    LVals: TGraphValues;
  begin
    if not AEntry.Empty then
      Exit;

    LVals := Default(TGraphValues);
    for I := 0 to High(Values) do
      if LHasRequiredConstraint
        or (not FRuleGroups[Values[I]].HasRequired)
        or ContainsGraphValue(LSelfRequiredValues, Values[I]) then
        Insert(Values[I], LVals, Length(LVals));
    Values := LVals;
  end;

  procedure TrimValuesForPreviousPass;
  var
    LGroup: TGraphRuleGroup;
    LPreviousEntry: TGraphEntry;
    LPreviousGraph: TGraph;
    LVals: TGraphValues;
    I: Integer;
  begin
    if not Assigned(FPassRoot) or (FPassIndex < 1) then
      Exit;

    LPreviousGraph := FPassRoot.GetPassGraph(Pred(FPassIndex));
    if not LPreviousGraph.InBounds(AEntry.Index) then
      raise EInvalidOperation.Create(
        'TrimValuesForPreviousPass::pass dimensions do not match');

    LPreviousEntry := LPreviousGraph.FEntries[AEntry.Index];
    LVals := Default(TGraphValues);

    for I := 0 to High(Values) do
    begin
      if not FRuleGroups.ContainsKey(Values[I]) then
      begin
        Insert(Values[I], LVals, Length(LVals));
        Continue;
      end;

      LGroup := FRuleGroups[Values[I]];
      if (Length(LGroup.PreviousValues) = 0)
        or ((not LPreviousEntry.Empty)
          and ContainsGraphValue(LGroup.PreviousValues,
            LPreviousEntry.Value)) then
        Insert(Values[I], LVals, Length(LVals));
    end;

    Values := LVals;
  end;

begin
  Values := Default(TGraphValues);
  LHasRequiredConstraint := False;
  LSelfRequiredValues := Default(TGraphValues);

  //a caller-assigned entry is a fixed candidate, but it still has to satisfy
  //directional and previous-pass constraints
  if not AEntry.Empty then
  begin
    SetLength(Values, 1);
    Values[0] := AEntry.Value;
  end
  else
  begin
    LInitialValues := Default(TGraphValues);
    for I := 0 to High(FValues) do
      Insert(FValues[I], LInitialValues, Length(LInitialValues));
    Values := LInitialValues;
  end;

  //get the rule group for each of the entry's neighbors on the same plane
  TrimValuesForNeighbor(AEntry[gdNorth], gdNorth);
  TrimValuesForNeighbor(AEntry[gdEast], gdEast);
  TrimValuesForNeighbor(AEntry[gdSouth], gdSouth);
  TrimValuesForNeighbor(AEntry[gdWest], gdWest);
  TrimValuesForNeighbor(AEntry[gdUp], gdUp); //moving top -> bottom
  TrimValuesForNeighbor(AEntry[gdDown], gdDown); //moving bottom -> top
  RemoveUnforcedRequiredValues;
  TrimValuesForPreviousPass;
end;

procedure TGraph.ValidateDimensions(const AWidth, AHeight,
  ADepth: TGraphCoordinate);
var
  LPlaneSize: TGraphCoordinate;
begin
  if (AWidth = 0) or (AHeight = 0) or (ADepth = 0) then
    Exit;

  if AWidth > TGraphCoordinate(High(Integer)) div AHeight then
    raise ERangeError.Create('Reshape::graph contains too many entries');

  LPlaneSize := AWidth * AHeight;
  if LPlaneSize > TGraphCoordinate(High(Integer)) div ADepth then
    raise ERangeError.Create('Reshape::graph contains too many entries');
end;

procedure TGraph.BuildStorage(const AWidth, AHeight,
  ADepth: TGraphCoordinate; out AEntries: TGraphEntries;
  out APlanes: TPlanes);
var
  LEntry: TGraphEntry;
  LEntries: TGraphEntries;
  LPlane : TPlane;
  LPlanes: TPlanes;
  LCoord : TPlaneCoord;
  LDimension: TDimension;
  Z, Y, X: Integer;
begin
  AEntries := nil;
  APlanes := nil;
  ValidateDimensions(AWidth, AHeight, ADepth);

  LEntries := TGraphEntries.Create(True);
  LPlanes := NewPlanes;
  try
    LDimension.Width := AWidth;
    LDimension.Height := AHeight;
    LDimension.Depth := ADepth;

    if (AWidth > 0) and (AHeight > 0) and (ADepth > 0) then
    begin
      //build the complete replacement off to the side so failures leave the
      //currently committed graph untouched
      for Z := 0 to Integer(ADepth) - 1 do
      begin
        LPlane := TPlane.Create;
        try
          LPlanes.Add(Z, LPlane);
        except
          LPlane.Free;
          raise;
        end;

        for Y := 0 to Integer(AHeight) - 1 do
        begin
          for X := 0 to Integer(AWidth) - 1 do
          begin
            LEntry := DoCreateEntry;
            try
              LEntry.InitializePosition(LEntries.Count, X, Y, Z);
              LEntries.Add(LEntry);
            except
              LEntry.Free;
              raise;
            end;

            LCoord.Key := X;
            LCoord.Value := Y;
            LPlane.Add(LCoord, LEntry);
          end;
        end;
      end;

      LinkNeighborsFor(LEntries, LDimension, FWrap);
    end;

    AEntries := LEntries;
    LEntries := nil;
    APlanes := LPlanes;
    LPlanes := nil;
  finally
    LPlanes.Free;
    LEntries.Free;
  end;
end;

function TGraph.ReshapeOne(const AWidth, AHeight,
  ADepth: TGraphCoordinate): TGraph;
var
  LEntries, LOldEntries: TGraphEntries;
  LPlanes, LOldPlanes: TPlanes;
begin
  Result := Self;
  LEntries := nil;
  LPlanes := nil;
  BuildStorage(AWidth, AHeight, ADepth, LEntries, LPlanes);
  try
    LOldEntries := FEntries;
    LOldPlanes := FPlanes;
    FEntries := LEntries;
    LEntries := nil;
    FPlanes := LPlanes;
    LPlanes := nil;
    FDimension.Width := AWidth;
    FDimension.Height := AHeight;
    FDimension.Depth := ADepth;

    LOldPlanes.Free;
    LOldEntries.Free;
  finally
    LPlanes.Free;
    LEntries.Free;
  end;

end;

function TGraph.Reshape(const AWidth, AHeight,
  ADepth: TGraphCoordinate): TGraph;
var
  I: Integer;
  LEntries, LOldEntries: TEntryStorageArray;
  LPlanes, LOldPlanes: TPlaneStorageArray;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.Reshape(AWidth, AHeight, ADepth));

  Result := Self;
  if FInitializingPass then
    raise EInvalidOperation.Create(
      'Reshape::cannot reshape the pipeline during pass initialization');
  if FRunning then
    raise EInvalidOperation.Create(
      'Reshape::cannot reshape the pass pipeline while it is running');
  EnsureInitialPass;
  ValidateDimensions(AWidth, AHeight, ADepth);

  SetLength(LEntries, FPasses.Count);
  SetLength(LPlanes, FPasses.Count);
  SetLength(LOldEntries, FPasses.Count);
  SetLength(LOldPlanes, FPasses.Count);

  try
    //prepare every pass before committing any of them
    for I := 0 to Pred(FPasses.Count) do
      FPasses[I].BuildStorage(AWidth, AHeight, ADepth,
        LEntries[I], LPlanes[I]);

    for I := 0 to Pred(FPasses.Count) do
    begin
      LOldEntries[I] := FPasses[I].FEntries;
      LOldPlanes[I] := FPasses[I].FPlanes;
      FPasses[I].FEntries := LEntries[I];
      LEntries[I] := nil;
      FPasses[I].FPlanes := LPlanes[I];
      LPlanes[I] := nil;
      FPasses[I].FDimension.Width := AWidth;
      FPasses[I].FDimension.Height := AHeight;
      FPasses[I].FDimension.Depth := ADepth;
    end;

    FDimension.Width := AWidth;
    FDimension.Height := AHeight;
    FDimension.Depth := ADepth;

    for I := 0 to Pred(FPasses.Count) do
    begin
      LOldPlanes[I].Free;
      LOldEntries[I].Free;
    end;
  finally
    for I := 0 to High(LPlanes) do
    begin
      LPlanes[I].Free;
      LEntries[I].Free;
    end;
  end;
end;

function TGraph.AddValue(const AValue: TGraphValue): TParentedGraphRuleGroup;
var
  LGraph: TGraph;
begin
  LGraph := GetActivePassGraph;
  if LGraph <> Self then
    Exit(LGraph.AddValue(AValue));

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

function TGraph.ForEachPass(const ACallback: TForEachPassCallback): TGraph;
var
  I: Integer;
  LSavedPassIndex: Integer;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.ForEachPass(ACallback));

  Result := Self;

  //callback must be assigned
  if not Assigned(ACallback) then
    Exit;

  LSavedPassIndex := FCurPassIndex;
  try
    for I := 0 to Pred(TotalPassCount) do
      ACallback(PassGraph[I], PassLabelFromIndex(I), I);
  finally
    FCurPassIndex := LSavedPassIndex;
    FCurPass := PassLabelFromIndex(LSavedPassIndex);
  end;
end;

function TGraph.ForEachPass(const ACallback: TForEachPassMethod): TGraph;
var
  I: Integer;
  LSavedPassIndex: Integer;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.ForEachPass(ACallback));

  Result := Self;
  if not Assigned(ACallback) then
    Exit;

  LSavedPassIndex := FCurPassIndex;
  try
    for I := 0 to Pred(TotalPassCount) do
      ACallback(PassGraph[I], PassLabelFromIndex(I), I);
  finally
    FCurPassIndex := LSavedPassIndex;
    FCurPass := PassLabelFromIndex(LSavedPassIndex);
  end;
end;

{$IFNDEF PAS2JS}
function TGraph.ForEachPass(
  const ACallback: TForEachPassNestedCallback): TGraph;
var
  I: Integer;
  LSavedPassIndex: Integer;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.ForEachPass(ACallback));

  Result := Self;
  if not Assigned(ACallback) then
    Exit;

  LSavedPassIndex := FCurPassIndex;
  try
    for I := 0 to Pred(TotalPassCount) do
      ACallback(PassGraph[I], PassLabelFromIndex(I), I);
  finally
    FCurPassIndex := LSavedPassIndex;
    FCurPass := PassLabelFromIndex(LSavedPassIndex);
  end;
end;
{$ENDIF}

function TGraph.SwitchToPass(const APass: String; out PassIndex: Integer): TGraph;
var
  LGraph: TGraph;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.SwitchToPass(APass, PassIndex));

  Result := Self;
  if FInitializingPass then
    raise EInvalidOperation.Create(
      'SwitchToPass::cannot switch passes during pass initialization');
  EnsureInitialPass;

  //determine if we have the current pass label (existing)
  //if so, then set the index
  if FPassLookup.ContainsKey(APass) then
    PassIndex := FPassLookup[APass]
  else
  begin
    if FRunning then
      raise EInvalidOperation.Create(
        'SwitchToPass::cannot create a pass while the pipeline is running');

    //on new label, initialize a new graph, copying dimensions
    PassIndex := TotalPassCount;
    LGraph := nil;
    FInitializingPass := True;
    try
      LGraph := DoCreatePass(PassIndex);
      LGraph.ReshapeOne(FDimension.Width, FDimension.Height,
        FDimension.Depth);
      FPassLookup.Add(APass, PassIndex);
      try
        FPasses.Add(LGraph);
      except
        FPassLookup.Remove(APass);
        raise;
      end;
      LGraph := nil;
      try
        FPasses[PassIndex].DoInitializePass;
      except
        FPasses.Delete(PassIndex);
        FPassLookup.Remove(APass);
        raise;
      end;
    finally
      LGraph.Free;
      FInitializingPass := False;
    end;
  end;

  //update CurrentPass AND CurrentPassIndex
  FCurPass := APass;
  FCurPassIndex := PassIndex;
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

function TGraph.RandomIndex(const ACount: Integer): Integer;
var
  LBound: Cardinal;
  LGraph: TGraph;
  LThreshold: Cardinal;
  LValue: Cardinal;
begin
  if ACount <= 0 then
    raise ERangeError.CreateFmt(
      'RandomIndex::count must be positive [%d]', [ACount]);
  if ACount = 1 then
    Exit(0);

  if Assigned(FPassRoot) then
    LGraph := Self
  else
  begin
    EnsureInitialPass;
    if FRunning and (FExecutingPassIndex >= 0) then
      LGraph := FPasses[FExecutingPassIndex]
    else
      LGraph := FPasses[FCurPassIndex];
  end;

  LBound := Cardinal(ACount);
  //Rejection sampling removes modulo bias. This form avoids negating an
  //unsigned value, so native overflow checks and pas2js use the same math.
  LThreshold := ((High(Cardinal) mod LBound) + 1) mod LBound;
  repeat
    LValue := LGraph.AdvanceRandomState(LGraph.FRandomState);
  until LValue >= LThreshold;
  Result := Integer(LValue mod LBound);
end;

procedure TGraph.ValidateAssignedEntry(const AEntry: TGraphEntry;
  const Z, APrevZ: TGraphCoordinate);
var
  LOriginalValue, LReplacement: TGraphValue;
  LValues: TGraphValues;
begin
  DoValidate(AEntry, Z, APrevZ, LValues);
  if ContainsGraphValue(LValues, AEntry.Value) then
    Exit;

  LOriginalValue := AEntry.Value;
  LReplacement := DoHandleInvalidState(AEntry);
  if LReplacement = LOriginalValue then
    raise EInvalidOperation.CreateFmt(
      'Run::locked value "%s" violates constraints in pass %d at entry %d',
      [LOriginalValue, FPassIndex, AEntry.Index]);

  AEntry.Value := LReplacement;
  DoValidate(AEntry, Z, APrevZ, LValues);
  if not ContainsGraphValue(LValues, LReplacement) then
  begin
    AEntry.Value := LOriginalValue;
    raise EInvalidOperation.CreateFmt(
      'Run::invalid-state replacement "%s" violates constraints in pass %d at entry %d',
      [LReplacement, FPassIndex, AEntry.Index]);
  end;
end;

function TGraph.RunOnePass: TGraph;
var
  I: Integer;

  procedure RunPlane(const Z, APrevZ : TGraphCoordinate);
  const
    TRAVERSAL_DIRECTIONS: array[0..3] of TGraphDirection =
      (gdNorth, gdEast, gdSouth, gdWest);
  var
    LDirection: TGraphDirection;
    X, Y: TGraphCoordinate;
    LEntry : TGraphEntry;
    LFrame: TGraphTraversalFrame;
    LIndex: Integer;
    LStack: TList<TGraphTraversalFrame>;
    LStackIndex: Integer;
    LVisited: TDictionary<TGraphEntry, Boolean>;

    procedure EnterEntry(const AEntry: TGraphEntry);
    var
      LNewFrame: TGraphTraversalFrame;
    begin
      if not Assigned(AEntry) or LVisited.ContainsKey(AEntry) then
        Exit;

      LVisited.Add(AEntry, True);
      if AEntry.Empty then
        AEntry.SetGeneratedValue(DoGetSelection(AEntry, Z, APrevZ))
      else
        ValidateAssignedEntry(AEntry, Z, APrevZ);

      LNewFrame.Entry := AEntry;
      LNewFrame.NextDirection := 0;
      LStack.Add(LNewFrame);
    end;

  begin
    LStack := nil;
    LVisited := nil;
    try
      LStack := TList<TGraphTraversalFrame>.Create;
      LVisited := TDictionary<TGraphEntry, Boolean>.Create;

      //now find the starting location
      DoGetStartCoord(X, Y);

      //Validate coordinates before flattening them. Checking only the final
      //index lets X = Width alias the next row and can overflow on wide native
      //coordinate types before InBounds gets a chance to reject it.
      if (X >= FDimension.Width) or (Y >= FDimension.Height) then
        raise ERangeError.CreateFmt(
          'RunPlane::invalid coordinates [x]-%d, [y]-%d, [z]-%d',
          [X, Y, Z]);

      //get the entry
      LIndex := CoordToIndex(X, Y, Z);

      //check for a valid index and continue if not (should always be true)
      if not InBounds(LIndex) then
        raise Exception.Create(Format('RunPlane::invalid coordinates [x]-%d, [y]-%d, [z]-%d', [X, Y, Z]));

      LEntry := FEntries[LIndex];

      //Use explicit traversal frames instead of recursive calls. A frame reads
      //each neighbor only after the preceding subtree has completed, exactly
      //matching the historical N/E/S/W depth-first behavior even when a
      //callback changes a public neighbor link. Object-keyed visited state
      //also retains support for custom links outside the built-in entry list.
      EnterEntry(LEntry);
      while LStack.Count > 0 do
      begin
        LStackIndex := Pred(LStack.Count);
        LFrame := LStack[LStackIndex];
        if LFrame.NextDirection > High(TRAVERSAL_DIRECTIONS) then
        begin
          LStack.Delete(LStackIndex);
          Continue;
        end;

        LDirection := TRAVERSAL_DIRECTIONS[LFrame.NextDirection];
        Inc(LFrame.NextDirection);
        LStack[LStackIndex] := LFrame;
        EnterEntry(LFrame.Entry[LDirection]);
      end;
    finally
      LVisited.Free;
      LStack.Free;
    end;
  end;

begin
  Result := Self;
  ClearGeneratedValues;

  if (FDimension.Width = 0)
    or (FDimension.Height = 0)
    or (FDimension.Depth = 0)
    or (FEntries.Count = 0) then
    Exit;

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

function TGraph.Run: TGraph;
var
  I: Integer;
  LGraph: TGraph;
  LSavedPassIndex: Integer;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.Run);

  Result := Self;
  if FInitializingPass then
    raise EInvalidOperation.Create(
      'Run::cannot run the pipeline during pass initialization');
  EnsureInitialPass;
  if FRunning then
    raise EInvalidOperation.Create('Run::the pass pipeline is already running');

  LSavedPassIndex := FCurPassIndex;
  FRunning := True;
  FExecutingPassIndex := -1;

  try
    //Every execution starts from the same per-pass streams. Random calls made
    //outside Run therefore cannot perturb a replay.
    RewindRandomStates;
    for I := 0 to Pred(TotalPassCount) do
    begin
      FExecutingPassIndex := I;
      FCurPassIndex := I;
      FCurPass := PassLabelFromIndex(I);
      LGraph := PassGraph[I];

      if not LGraph.HasDefinition then
      begin
        if I > 0 then
          LGraph.CopyValuesFrom(PassGraph[Pred(I)]);
      end
      else
        LGraph.RunOnePass;

      //selection callbacks are allowed to inspect or switch passes; the
      //coordinator always resumes the pass currently being solved
      FCurPassIndex := I;
      FCurPass := PassLabelFromIndex(I);
    end;
  finally
    FExecutingPassIndex := -1;
    FCurPassIndex := LSavedPassIndex;
    FCurPass := PassLabelFromIndex(LSavedPassIndex);
    FRunning := False;
  end;
end;

function TGraph.Reset: TGraph;
var
  LInvalid: TInvalidStateCallback;
  LNewPass: TGraph;
  LNewPasses, LOldPasses: TPassList;
  LNewLookup, LOldLookup: TPassLookup;
  LOldDimension: TDimension;
  LOldMode: TGraphRunMode;
  LOldPass: String;
  LOldPassIndex: Integer;
  LOldWrap: Boolean;
  LSelection: TValueSelectionCallback;
begin
  if Assigned(FPassRoot) then
    raise EInvalidOperation.Create(
      'Reset::call Reset on the pass pipeline root, not PassGraph');
  if FInitializingPass then
    raise EInvalidOperation.Create(
      'Reset::cannot reset the pipeline during pass initialization');
  if FRunning then
    raise EInvalidOperation.Create(
      'Reset::cannot reset the pass pipeline while it is running');

  Result := Self;
  EnsureInitialPass;
  LSelection := FPasses[0].FSel;
  LInvalid := FPasses[0].FInv;
  LNewPass := nil;
  LNewPasses := nil;
  LNewLookup := nil;
  FInitializingPass := True;
  try
    LNewPass := DoCreatePass(0);
    LNewPasses := TPassList.Create(True);
    LNewLookup := TPassLookup.Create;
    LNewPass.FSel := LSelection;
    LNewPass.FInv := LInvalid;
    LNewLookup.Add('', 0);
    try
      LNewPasses.Add(LNewPass);
    except
      LNewLookup.Remove('');
      raise;
    end;
    LNewPass := nil;

    LOldPasses := FPasses;
    LOldLookup := FPassLookup;
    LOldDimension := FDimension;
    LOldMode := FMode;
    LOldPass := FCurPass;
    LOldPassIndex := FCurPassIndex;
    LOldWrap := FWrap;

    FPasses := LNewPasses;
    LNewPasses := nil;
    FPassLookup := LNewLookup;
    LNewLookup := nil;
    FCurPass := '';
    FCurPassIndex := 0;
    FDimension.Width := 0;
    FDimension.Height := 0;
    FDimension.Depth := 0;
    try
      FPasses[0].DoInitializePass;
    except
      //Keep the failed replacement in the locals for cleanup, then restore
      //the complete old pipeline before allowing the exception to escape.
      LNewPasses := FPasses;
      LNewLookup := FPassLookup;
      FPasses := LOldPasses;
      FPassLookup := LOldLookup;
      FDimension := LOldDimension;
      FMode := LOldMode;
      FCurPass := LOldPass;
      FCurPassIndex := LOldPassIndex;
      FWrap := LOldWrap;
      raise;
    end;

    LOldPasses.Free;
    LOldLookup.Free;
    FRuleGroups.Clear;
    FEntries.Clear;
    FPlanes.Clear;
    SetLength(FValues, 0);
  finally
    LNewPass.Free;
    LNewPasses.Free;
    LNewLookup.Free;
    FInitializingPass := False;
  end;
end;

procedure TGraph.InitializeStorage;
begin
  FSel := DefaultSelection;
  FEntries := TGraphEntries.Create(True);
  FPlanes := NewPlanes;
  FRuleGroups := TGraphRuleGroups.Create([doOwnsValues]);
  FPasses := TPassList.Create(True);
  FPassLookup := TPassLookup.Create;
  FMode := rmBottomUp;
  FWrap := True;
  FCurPassIndex := 0;
  FCurPass := '';
  FPassIndex := 0;
  FPassRoot := nil;
  FInitializingPass := False;
  FRunning := False;
  FSeed := 0;
  FSeedInitialized := False;
  SeedRandomState(FSeed, FRandomState);
  FExecutingPassIndex := -1;
end;

constructor TGraph.CreatePass(const ARoot: TGraph;
  const APassIndex: Integer);
var
  LPrevious: TGraph;
begin
  InitializeStorage;
  FPassRoot := ARoot;
  FPassIndex := APassIndex;
  ARoot.BuildPassRandomState(APassIndex, FRandomState);
  FSeed := ARoot.FSeed;
  FSeedInitialized := True;
  FMode := ARoot.FMode;
  FWrap := ARoot.FWrap;

  if APassIndex > 0 then
  begin
    LPrevious := ARoot.GetPassGraph(Pred(APassIndex));
    FSel := LPrevious.FSel;
    FInv := LPrevious.FInv;
  end
  else
  begin
    FSel := ARoot.FSel;
    FInv := ARoot.FInv;
  end;

end;

constructor TGraph.Create;
begin
  InitializeStorage;
end;

destructor TGraph.Destroy;
begin
  FPasses.Free;
  FPassLookup.Free;
  FRuleGroups.Free;
  FPlanes.Free;
  FEntries.Free;
  inherited Destroy;
end;

initialization
  DefaultSelection := DefSelCall;
end.

