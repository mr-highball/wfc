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
  //Positive relative frequency used by the reference solver. Integer weights
  //keep model identity and native/pas2js replay independent of host floats.
  TGraphWeight = Integer;
  //native builds retain the original UInt64 API; pas2js uses Cardinal because
  //its RTL does not implement UInt64. storage is bounded by Integer on both.
  {$IFDEF PAS2JS}
  TGraphCoordinate = Cardinal;
  {$ELSE}
  TGraphCoordinate = UInt64;
  {$ENDIF}
  TGraphSeed = Cardinal;
  TGraphPassLabels = array of String;
  TGraphPassIndices = array of Integer;

const
  WFC_DEFAULT_VALUE_WEIGHT = TGraphWeight(1);
  //Increment when seed expansion, pass-stream derivation, bounded sampling,
  //or the built-in generator changes in a replay-incompatible way.
  WFC_RANDOM_ALGORITHM_VERSION = 1;
  //Increment when propagation, observation, backtracking, or deterministic
  //tie-breaking changes reference-solver replay.
  WFC_SOLVER_ALGORITHM_VERSION = 2;
  //Identifies additive graph-model semantics such as explicit deny-all
  //directions and caller-owned entry domains.  These inputs are versioned
  //separately because they do not reinterpret legacy rules or solver steps.
  WFC_GRAPH_MODEL_VERSION = 1;
  //Increment when dependency planning, pass-mode staging, or selective
  //regeneration changes in a replay-incompatible way. The per-pass reference
  //solver remains versioned independently above.
  WFC_PIPELINE_ALGORITHM_VERSION = 2;
  //Identifies the public causal-trace event schema. Trace capture is opt-in,
  //so adding this observability surface does not change solver replay.
  WFC_TRACE_VERSION = 1;
  //Identifies the portable integer encoding used by trace signatures.
  WFC_TRACE_HASH_VERSION = 1;

type

  TGraphPosition = record
    X : TGraphCoordinate;
    Y : TGraphCoordinate;
    Z : TGraphCoordinate;
  end;

  //Signed finite displacement from a consumer cell to a provider-pass cell.
  //Keeping offsets independent of unsigned graph coordinates makes bounded
  //and wrapped sampling explicit and portable across native FPC and pas2js.
  TGraphOffset = record
    DeltaX: Integer;
    DeltaY: Integer;
    DeltaZ: Integer;
  end;

  //One alternative in a cross-pass clause. A term matches when its resolved
  //provider cell is non-empty and contains one of Values.
  TGraphPassMatchTerm = record
    Offset: TGraphOffset;
    Values: TGraphValues;
  end;
  TGraphPassMatchTerms = array of TGraphPassMatchTerm;

  //all posible "directions" to move from a single point on the graph
  TGraphDirection = (gdNorth, gdEast, gdSouth, gdWest, gdUp, gdDown);
  TGraphDirections = set of TGraphDirection;

  //gpmLegacy preserves the original hybrid contract: a defined pass solves a
  //fresh layer, while a later definitionless pass copies its predecessor.
  TGraphPassMode = (gpmLegacy, gpmTransform, gpmOverlay);

  TGraphPassDisposition = (
    gpdNotRun,
    gpdReused,
    gpdCleared,
    gpdCopied,
    gpdSolved,
    gpdFailed
  );

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
    //Pass-local caller domains are deliberately unit-private.  They are
    //mutated only through TGraph so coordinate, value-order, and run guards
    //cannot be bypassed through an entry reference.
    FAllowedValues: TGraphValues;
    FHasAllowedValues: Boolean;
    procedure AssignValue(const AValue: TGraphValue;
      const AGenerated: Boolean);
    procedure InitializePosition(const AIndex: Integer;
      const AX, AY, AZ: TGraphCoordinate);
    procedure RestoreValueState(const AValue: TGraphValue;
      const AEmpty, AGenerated: Boolean);
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
  private
    type
      TPassRequirementOrigin = (proPrevious, proNamed);
      TPassRequirementOrigins = set of TPassRequirementOrigin;
      TPassRequirementKind = (prkMergedOffset, prkAny);
      TPassRequirement = record
        PassIndex: Integer;
        Terms: TGraphPassMatchTerms;
        Origins: TPassRequirementOrigins;
        Kind: TPassRequirementKind;
      end;
      TPassRequirements = array of TPassRequirement;
  private
    FPassRequirements: TPassRequirements;
    procedure AddPassRequirement(const APassIndex: Integer;
      const AValue: TGraphValue; const AOrigin: TPassRequirementOrigin);
    procedure AddPassOffsetRequirement(const APassIndex: Integer;
      const AOffset: TGraphOffset; const AValues: TGraphValues;
      const AOrigin: TPassRequirementOrigin);
    procedure AddPassAnyRequirement(const APassIndex: Integer;
      const ATerms: TGraphPassMatchTerms;
      const AOrigin: TPassRequirementOrigin);
  strict private
    FRules: TGraphRules;
    FDeniedDirections: TGraphDirections;
    FPreviousValues: TGraphValues;
    FVal: TGraphValue;
    FWeight: TGraphWeight;
    function GetExists(const ADirection : TGraphDirection): Boolean;
    function GetDenied(const ADirection: TGraphDirection): Boolean;
    function GetHasRequired: Boolean;
    function GetRule(const ADirection : TGraphDirection): TGraphRule;
    procedure SetWeight(const AValue: TGraphWeight);
  strict protected
    function IndexOfDirection(const ADirection : TGraphDirection) : Integer;
    procedure ApplyDenyAll(const ADirections: TGraphDirections);
    procedure RemoveRuleValue(const ADirection: TGraphDirection;
      const AValue: TGraphValue; out ARemoved, ABecameEmpty: Boolean);

    (*
      can be overridden to handle additional logic for adding new rules
    *)
    procedure DoNewRule(const ADirections : TGraphDirections;
      const AValue : TGraphValue; const ARequireRule : Boolean); virtual;
    procedure DoDenyAll(const ADirections: TGraphDirections); virtual;
    procedure DoRequirePrevious(const AValue : TGraphValue); virtual;
    procedure DoRequireFromPass(const APass: String;
      const AValue: TGraphValue); virtual;
    procedure DoRequireFromPassAt(const APass: String;
      const AOffset: TGraphOffset; const AValues: TGraphValues); virtual;
    procedure DoRequireAnyFromPass(const APass: String;
      const ATerms: TGraphPassMatchTerms); virtual;
    procedure UpsertRule(const ADirections : TGraphDirections;
      const AValue : TGraphValue; const ARequireRule : Boolean);
  public
    property Value : TGraphValue read FVal write FVal;
    property Rule[const ADirection : TGraphDirection] : TGraphRule read GetRule; default;
    property Rules : TGraphRules read FRules write FRules;
    property Exists[const ADirection : TGraphDirection] : Boolean read GetExists;
    property Denied[const ADirection: TGraphDirection]: Boolean read GetDenied;
    property DeniedDirections: TGraphDirections read FDeniedDirections;
    property PreviousValues : TGraphValues read FPreviousValues;
    //Positive pass-local relative frequency. The reference solver
    //canonicalizes the complete pass vector by its GCD before use.
    property Weight: TGraphWeight read FWeight write SetWeight;

    (*
      true if at least one rule for this value is required
    *)
    property HasRequired : Boolean read GetHasRequired;

    function NewRule(const ADirections : TGraphDirections;
      const AValue : TGraphValue; const ARequireRule : Boolean = False) : TGraphRuleGroup; overload;

    function NewRule(const ADirections : TGraphDirections;
      const AValues : TGraphValues; const ARequireRule : Boolean = False) : TGraphRuleGroup; overload;

    //Marks every supplied direction as explicitly allowing no neighbor.
    //This is distinct from a missing or present-empty legacy wildcard rule.
    function DenyAll(const ADirections: TGraphDirections): TGraphRuleGroup;

    (*
      allows this value only when the entry at the same coordinate in the
      immediately preceding pass contains one of the supplied values
    *)
    function RequirePrevious(const AValue : TGraphValue) : TGraphRuleGroup; overload;
    function RequirePrevious(const AValues : TGraphValues) : TGraphRuleGroup; overload;

    //Allows this value only when a named dependency contains one of the
    //supplied values at the same coordinate. Calls for one source merge as
    //alternatives; requirements from distinct sources are conjunctive.
    function RequireFromPass(const APass: String;
      const AValue: TGraphValue): TGraphRuleGroup; overload;
    function RequireFromPass(const APass: String;
      const AValues: TGraphValues): TGraphRuleGroup; overload;

    //Adds or extends the merged single-offset clause for this source. Values
    //at one source+offset are alternatives; different offsets are clauses and
    //therefore remain conjunctive.
    function RequireFromPassAt(const APass: String;
      const AOffset: TGraphOffset;
      const AValue: TGraphValue): TGraphRuleGroup; overload;
    function RequireFromPassAt(const APass: String;
      const AOffset: TGraphOffset;
      const AValues: TGraphValues): TGraphRuleGroup; overload;

    //Adds one distinct OR clause over finite provider-pass terms. Every call
    //is conjunctive with the group's other clauses.
    function RequireAnyFromPass(const APass: String;
      const ATerms: TGraphPassMatchTerms): TGraphRuleGroup;

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

  TGraphSolveStatus = (
    gssSolved,
    gssContradiction,
    gssBacktrackLimit
  );

  TGraphContradictionKind = (
    gckNone,
    gckEmptyDomain,
    gckInvalidLock,
    gckAdjacency,
    gckPreviousPass,
    gckRequiredSupport,
    gckFinalValidation,
    gckPassDependency,
    gckEntryDomain
  );

  TGraphSolveOptions = record
    //The limit is applied independently to each pass. Zero disables branch
    //recovery while still allowing propagation-only solutions.
    MaxBacktracks: Integer;
    //Causal event capture is deliberately opt-in. False preserves the
    //minimal allocation and execution profile of ordinary solves.
    CaptureTrace: Boolean;
  end;

  TGraphContradiction = record
    Kind: TGraphContradictionKind;
    PassIndex: Integer;
    EntryIndex: Integer;
    NeighborIndex: Integer;
    HasDirection: Boolean;
    Direction: TGraphDirection;
    DependencyPassIndex: Integer;
  end;

  //Trace events describe the complete attempted transaction, including
  //abandoned branches and selective passes that were reused without solving.
  TGraphTraceEventKind = (
    gtekPassBegin,
    gtekInitialCandidateRemoved,
    gtekDecision,
    gtekCandidateRemoved,
    gtekContradiction,
    gtekBacktrack,
    gtekCandidateRestored,
    gtekPassStaged,
    gtekPassFailed,
    gtekPassSkipped,
    gtekPipelineCommit,
    gtekPipelineRollback
  );

  TGraphTraceCauseKind = (
    gtckNone,
    gtckCallerDomain,
    gtckCallerLock,
    gtckDecision,
    gtckAdjacency,
    gtckPassDependency,
    gtckRequiredSupport,
    gtckBacktrack,
    gtckFinalValidation,
    gtckTransaction
  );

  TGraphTraceSignature = Cardinal;

  TGraphTraceEvent = record
    //EventId is the zero-based chronological index. CauseEventId is -1 when
    //the cause is external model input rather than another trace event.
    EventId: Integer;
    CauseEventId: Integer;
    Kind: TGraphTraceEventKind;
    CauseKind: TGraphTraceCauseKind;
    PassIndex: Integer;
    EntryIndex: Integer;
    ValueIndex: Integer;
    //Value is inspection-friendly; stable signatures use ValueIndex so host
    //string encodings cannot affect native/pas2js parity.
    Value: TGraphValue;
    NeighborIndex: Integer;
    HasDirection: Boolean;
    Direction: TGraphDirection;
    DependencyPassIndex: Integer;
    DecisionDepth: Integer;
    DomainCountBefore: Integer;
    DomainCountAfter: Integer;
  end;

  TGraphTraceEvents = array of TGraphTraceEvent;

  TGraphPassSolveReport = record
    Decisions: Integer;
    Propagations: Integer;
    Contradictions: Integer;
    Backtracks: Integer;
    Executed: Boolean;
    ExecutionOrdinal: Integer;
    Disposition: TGraphPassDisposition;
    //Half-open slice into TGraphSolveReport.Trace. TraceCount is zero when
    //capture is disabled or this pass emitted no events.
    TraceStart: Integer;
    TraceCount: Integer;
  end;

  TGraphPassSolveReports = array of TGraphPassSolveReport;

  TGraphSolveReport = record
    Status: TGraphSolveStatus;
    Seed: TGraphSeed;
    RandomAlgorithmVersion: Integer;
    SolverAlgorithmVersion: Integer;
    GraphModelVersion: Integer;
    PipelineAlgorithmVersion: Integer;
    FailedPassIndex: Integer;
    Contradiction: TGraphContradiction;
    Passes: TGraphPassSolveReports;
    ExecutionOrder: TGraphPassIndices;
    TraceCaptured: Boolean;
    TraceHash: TGraphTraceSignature;
    Trace: TGraphTraceEvents;
  end;

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
        procedure DoDenyAll(
          const ADirections: TGraphDirections); override;
        procedure DoRequirePrevious(
          const AValue: TGraphValue); override;
        procedure DoRequireFromPass(const APass: String;
          const AValue: TGraphValue); override;
        procedure DoRequireFromPassAt(const APass: String;
          const AOffset: TGraphOffset;
          const AValues: TGraphValues); override;
        procedure DoRequireAnyFromPass(const APass: String;
          const ATerms: TGraphPassMatchTerms); override;
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
      TPassDependencyRole = (
        pdrDeclared,
        pdrLegacy,
        pdrRequirement,
        pdrPreviousValues,
        pdrTransformSource
      );
      TPassDependencyRoles = set of TPassDependencyRole;
      TPassDependency = record
        PassIndex: Integer;
        Roles: TPassDependencyRoles;
      end;
      TPassDependencies = array of TPassDependency;
      TPassSelection = array of Byte;
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
    FPassMode: TGraphPassMode;
    FPassDependencies: TPassDependencies;
    FTransformSourceIndex: Integer;

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
    function GetDependencyCount: Integer;
    function GetDependencyIndex(const AOrdinal: Integer): Integer;
    function GetPassMode: TGraphPassMode;
    function GetTransformSourceIndex: Integer;
    procedure SetPassMode(const AValue: TGraphPassMode);
    function DependencySlot(const APassIndex: Integer): Integer;
    function PassIndexForLabel(const APass, AOperation: String): Integer;
    function WouldCreateDependencyCycle(const AConsumerIndex,
      AProviderIndex: Integer): Boolean;
    function HasPassRequirement(const APassIndex: Integer): Boolean;
    function HasPreviousValueRequirement: Boolean;
    function ResolveOffsetIndex(const AEntryIndex: Integer;
      const AOffset: TGraphOffset; out AResolvedIndex: Integer): Boolean;
    procedure SynchronizePreviousValueDependencies;
    procedure AddDependencyRole(const APassIndex: Integer;
      const ARole: TPassDependencyRole);
    procedure RemoveDependencyRole(const APassIndex: Integer;
      const ARole: TPassDependencyRole);
    procedure BuildPassExecutionOrder(out AOrder: TGraphPassIndices);
    function TrySolveInternal(const AOptions: TGraphSolveOptions;
      const ADirty: TPassSelection;
      out AReport: TGraphSolveReport): Boolean;
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
    function GetRunning: Boolean;
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
    procedure ValidateCurrentEntryDomains(const AOperation: String);
    procedure ValidateDeniedRuleState(const AOperation: String);
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

    //True while the root pipeline is executing. PassGraph instances forward
    //this state so adapters can reject multi-step imports before any mutation.
    property Running: Boolean read GetRunning;

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

    //Pass-plan configuration is pass-scoped. Access through the root addresses
    //the selected pass; access through PassGraph addresses that pass directly.
    property PassMode: TGraphPassMode read GetPassMode write SetPassMode;
    property DependencyCount: Integer read GetDependencyCount;
    property DependencyIndex[const AOrdinal: Integer]: Integer
      read GetDependencyIndex;
    property TransformSourceIndex: Integer read GetTransformSourceIndex;

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
    function AddValue(const AValue : TGraphValue) : TParentedGraphRuleGroup; overload;
    function AddValue(const AValue : TGraphValue;
      const AWeight: TGraphWeight) : TParentedGraphRuleGroup; overload;

    //Reports either registered values or public rule groups on the active
    //pass. Checking both detects legacy callers that mutated RuleGroups
    //directly and left the canonical value registry inconsistent.
    function HasDefinition: Boolean;

    //Returns the active pass's canonical AddValue order. This read-only copy
    //lets adapters prove that the public RuleGroups view has not been changed
    //independently of the private deterministic value registry.
    function CopyRegisteredValues: TGraphValues;

    //Caller-owned pass-local initial domains.  SetAllowedValues canonicalizes
    //the supplied set to AddValue order; an assigned empty set is an explicit
    //contradiction and is distinct from ClearAllowedValues.
    function SetAllowedValues(const X, Y, Z: TGraphCoordinate;
      const AValues: TGraphValues): TGraph; overload;
    function SetAllowedValues(const X, Y, Z: TGraphCoordinate;
      const AValue: TGraphValue): TGraph; overload;
    function ClearAllowedValues(const X, Y,
      Z: TGraphCoordinate): TGraph;
    function HasAllowedValues(const X, Y,
      Z: TGraphCoordinate): Boolean;
    function CopyAllowedValues(const X, Y,
      Z: TGraphCoordinate): TGraphValues;

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

    function DependsOn(const APass: String): TGraph;
    function RemoveDependency(const APass: String): TGraph;
    function ClearDependencies: TGraph;
    function TransformFrom(const APass: String): TGraph;

    (*
      returns an unbiased value in 0..Pred(ACount) from the current pass's
      portable stream. callbacks should use this instead of System.Random
      when replay across native FPC and pas2js matters
    *)
    function RandomIndex(const ACount: Integer): Integer;

    (*
      solves the complete pass pipeline with the opt-in propagating reference
      solver. Results are staged and committed only after every pass validates;
      a reported contradiction or limit leaves graph entries unchanged.
      Legacy selection and invalid-state callbacks are not invoked.
    *)
    function TrySolve(const AOptions: TGraphSolveOptions;
      out AReport: TGraphSolveReport): Boolean;

    function TryRegenerateFrom(const APass: String;
      const AOptions: TGraphSolveOptions;
      out AReport: TGraphSolveReport): Boolean; overload;
    function TryRegenerateFrom(const APasses: TGraphPassLabels;
      const AOptions: TGraphSolveOptions;
      out AReport: TGraphSolveReport): Boolean; overload;

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

  function MakeGraphOffset(const ADeltaX, ADeltaY,
    ADeltaZ: Integer): TGraphOffset;
  function MakeGraphPassMatchTerm(const AOffset: TGraphOffset;
    const AValue: TGraphValue): TGraphPassMatchTerm; overload;
  function MakeGraphPassMatchTerm(const AOffset: TGraphOffset;
    const AValues: TGraphValues): TGraphPassMatchTerm; overload;

  (*
    checks if a value is held in a graph values array
  *)
  function ContainsGraphValue(const AValues : TGraphValues; const AValue : TGraphValue) : Boolean; inline;

  //Returns the stable defaults for the opt-in reference solver.
  function DefaultGraphSolveOptions: TGraphSolveOptions;
  //Recomputes the portable signature from report metadata and numeric trace
  //events. Returns zero when trace capture is disabled.
  function CalculateGraphTraceHash(
    const AReport: TGraphSolveReport): TGraphTraceSignature;

const
  AllDirections : TGraphDirections = [gdNorth, gdEast, gdSouth, gdWest, gdUp, gdDown];

implementation

uses
  wfc_solver_reference;

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

function CloneGraphValues(const AValues: TGraphValues): TGraphValues;
var
  I: Integer;
begin
  Result := Default(TGraphValues);
  SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do
    Result[I] := AValues[I];
end;

function GraphOffsetsEqual(const ALeft,
  ARight: TGraphOffset): Boolean; inline;
begin
  Result := (ALeft.DeltaX = ARight.DeltaX)
    and (ALeft.DeltaY = ARight.DeltaY)
    and (ALeft.DeltaZ = ARight.DeltaZ);
end;

function CompareGraphOffsets(const ALeft,
  ARight: TGraphOffset): Integer; inline;
begin
  if ALeft.DeltaX < ARight.DeltaX then
    Exit(-1)
  else if ALeft.DeltaX > ARight.DeltaX then
    Exit(1);
  if ALeft.DeltaY < ARight.DeltaY then
    Exit(-1)
  else if ALeft.DeltaY > ARight.DeltaY then
    Exit(1);
  if ALeft.DeltaZ < ARight.DeltaZ then
    Exit(-1)
  else if ALeft.DeltaZ > ARight.DeltaZ then
    Exit(1);
  Result := 0;
end;

function IsZeroGraphOffset(const AOffset: TGraphOffset): Boolean; inline;
begin
  Result := (AOffset.DeltaX = 0) and (AOffset.DeltaY = 0)
    and (AOffset.DeltaZ = 0);
end;

procedure MergeGraphValues(var ADestination: TGraphValues;
  const ASource: TGraphValues);
var
  I, LIndex: Integer;
begin
  for I := 0 to High(ASource) do
    if not ContainsGraphValue(ADestination, ASource[I]) then
    begin
      LIndex := Length(ADestination);
      SetLength(ADestination, Succ(LIndex));
      ADestination[LIndex] := ASource[I];
    end;
end;

function CanonicalGraphPassMatchTerms(const ATerms: TGraphPassMatchTerms;
  const AOperation: String): TGraphPassMatchTerms;
var
  I, J, K, LIndex: Integer;
  LSwap: TGraphPassMatchTerm;
begin
  Result := Default(TGraphPassMatchTerms);
  if Length(ATerms) = 0 then
    raise EArgumentException.CreateFmt(
      '%s::match terms cannot be empty', [AOperation]);
  SetLength(Result, 0);
  for I := 0 to High(ATerms) do
  begin
    if Length(ATerms[I].Values) = 0 then
      raise EArgumentException.CreateFmt(
        '%s::term %d values cannot be empty', [AOperation, I]);
    for K := 0 to High(ATerms[I].Values) do
      if ATerms[I].Values[K] = TGraphValue.Empty then
        raise EArgumentException.CreateFmt(
          '%s::term %d value %d cannot be empty',
          [AOperation, I, K]);
    LIndex := -1;
    for J := 0 to High(Result) do
      if GraphOffsetsEqual(Result[J].Offset, ATerms[I].Offset) then
      begin
        LIndex := J;
        Break;
      end;
    if LIndex < 0 then
    begin
      LIndex := Length(Result);
      SetLength(Result, Succ(LIndex));
      Result[LIndex].Offset := ATerms[I].Offset;
      Result[LIndex].Values := Default(TGraphValues);
      MergeGraphValues(Result[LIndex].Values, ATerms[I].Values);
    end
    else
      MergeGraphValues(Result[LIndex].Values, ATerms[I].Values);
  end;

  //Insertion sort avoids host-specific comparer behavior and gives native
  //FPC and pas2js the same signed X/Y/Z term order.
  for I := 1 to High(Result) do
  begin
    LSwap := Result[I];
    J := I;
    while (J > 0)
      and (CompareGraphOffsets(LSwap.Offset,
        Result[Pred(J)].Offset) < 0) do
    begin
      Result[J] := Result[Pred(J)];
      Dec(J);
    end;
    Result[J] := LSwap;
  end;
end;

function GraphPassMatchTermsEqual(const ALeft,
  ARight: TGraphPassMatchTerms): Boolean;
var
  I, J: Integer;
begin
  if Length(ALeft) <> Length(ARight) then
    Exit(False);
  for I := 0 to High(ALeft) do
  begin
    if (not GraphOffsetsEqual(ALeft[I].Offset, ARight[I].Offset))
      or (Length(ALeft[I].Values) <> Length(ARight[I].Values)) then
      Exit(False);
    for J := 0 to High(ALeft[I].Values) do
      if ALeft[I].Values[J] <> ARight[I].Values[J] then
        Exit(False);
  end;
  Result := True;
end;

function MakeGraphOffset(const ADeltaX, ADeltaY,
  ADeltaZ: Integer): TGraphOffset;
begin
  Result.DeltaX := ADeltaX;
  Result.DeltaY := ADeltaY;
  Result.DeltaZ := ADeltaZ;
end;

function MakeGraphPassMatchTerm(const AOffset: TGraphOffset;
  const AValue: TGraphValue): TGraphPassMatchTerm;
begin
  if AValue = TGraphValue.Empty then
    raise EArgumentException.Create(
      'MakeGraphPassMatchTerm::value cannot be empty');
  Result.Offset := AOffset;
  SetLength(Result.Values, 1);
  Result.Values[0] := AValue;
end;

function MakeGraphPassMatchTerm(const AOffset: TGraphOffset;
  const AValues: TGraphValues): TGraphPassMatchTerm;
var
  I: Integer;
begin
  if Length(AValues) = 0 then
    raise EArgumentException.Create(
      'MakeGraphPassMatchTerm::values cannot be empty');
  for I := 0 to High(AValues) do
    if AValues[I] = TGraphValue.Empty then
      raise EArgumentException.CreateFmt(
        'MakeGraphPassMatchTerm::value %d cannot be empty', [I]);
  Result.Offset := AOffset;
  Result.Values := CloneGraphValues(AValues);
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

function DefaultGraphSolveOptions: TGraphSolveOptions;
begin
  Result.MaxBacktracks := 256;
  Result.CaptureTrace := False;
end;

procedure GraphTraceHashByte(var AHash: TGraphTraceSignature;
  const AValue: Byte);
{$PUSH}
{$Q-}
var
  LValue: Cardinal;
begin
  AHash := AHash xor Cardinal(AValue);
  LValue := AHash;
  AHash := (LValue + (LValue shl 1) + (LValue shl 4)
    + (LValue shl 7) + (LValue shl 8) + (LValue shl 24))
    and Cardinal($FFFFFFFF);
end;
{$POP}

procedure GraphTraceHashCardinal(var AHash: TGraphTraceSignature;
  const AValue: Cardinal);
begin
  GraphTraceHashByte(AHash, Byte(AValue and $FF));
  GraphTraceHashByte(AHash, Byte((AValue shr 8) and $FF));
  GraphTraceHashByte(AHash, Byte((AValue shr 16) and $FF));
  GraphTraceHashByte(AHash, Byte((AValue shr 24) and $FF));
end;

procedure GraphTraceHashInteger(var AHash: TGraphTraceSignature;
  const AValue: Integer);
var
  LMagnitude: Cardinal;
begin
  if AValue < 0 then
  begin
    GraphTraceHashByte(AHash, 1);
    LMagnitude := Cardinal(-(AValue + 1));
    Inc(LMagnitude);
  end
  else
  begin
    GraphTraceHashByte(AHash, 0);
    LMagnitude := Cardinal(AValue);
  end;
  GraphTraceHashCardinal(AHash, LMagnitude);
end;

procedure GraphTraceHashText(var AHash: TGraphTraceSignature;
  const AValue: String);
var
  I: Integer;
begin
  GraphTraceHashCardinal(AHash, Cardinal(Length(AValue)));
  for I := 1 to Length(AValue) do
    GraphTraceHashByte(AHash, Byte(Ord(AValue[I])));
end;

procedure MixGraphTraceEvent(var AHash: TGraphTraceSignature;
  const AEvent: TGraphTraceEvent);
begin
  GraphTraceHashInteger(AHash, AEvent.EventId);
  GraphTraceHashInteger(AHash, AEvent.CauseEventId);
  GraphTraceHashCardinal(AHash, Cardinal(Ord(AEvent.Kind)));
  GraphTraceHashCardinal(AHash, Cardinal(Ord(AEvent.CauseKind)));
  GraphTraceHashInteger(AHash, AEvent.PassIndex);
  GraphTraceHashInteger(AHash, AEvent.EntryIndex);
  GraphTraceHashInteger(AHash, AEvent.ValueIndex);
  GraphTraceHashInteger(AHash, AEvent.NeighborIndex);
  if AEvent.HasDirection then
    GraphTraceHashByte(AHash, 1)
  else
    GraphTraceHashByte(AHash, 0);
  GraphTraceHashCardinal(AHash, Cardinal(Ord(AEvent.Direction)));
  GraphTraceHashInteger(AHash, AEvent.DependencyPassIndex);
  GraphTraceHashInteger(AHash, AEvent.DecisionDepth);
  GraphTraceHashInteger(AHash, AEvent.DomainCountBefore);
  GraphTraceHashInteger(AHash, AEvent.DomainCountAfter);
end;

function CalculateGraphTraceHash(
  const AReport: TGraphSolveReport): TGraphTraceSignature;
var
  I: Integer;
begin
  if not AReport.TraceCaptured then
    Exit(0);
  Result := Cardinal(2166136261);
  GraphTraceHashText(Result, 'wfc-graph-trace');
  GraphTraceHashCardinal(Result, WFC_TRACE_VERSION);
  GraphTraceHashCardinal(Result, WFC_TRACE_HASH_VERSION);
  GraphTraceHashCardinal(Result, AReport.Seed);
  GraphTraceHashCardinal(Result,
    Cardinal(AReport.RandomAlgorithmVersion));
  GraphTraceHashCardinal(Result,
    Cardinal(AReport.SolverAlgorithmVersion));
  GraphTraceHashCardinal(Result,
    Cardinal(AReport.GraphModelVersion));
  GraphTraceHashCardinal(Result,
    Cardinal(AReport.PipelineAlgorithmVersion));
  GraphTraceHashCardinal(Result, Cardinal(Length(AReport.Passes)));
  for I := 0 to High(AReport.Trace) do
    MixGraphTraceEvent(Result, AReport.Trace[I]);
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

procedure TGraph.TParentedGraphRuleGroup.DoDenyAll(
  const ADirections: TGraphDirections);
var
  I: Integer;
  LBaseGroup: TGraphRuleGroup;
  LBecameEmpty: Boolean;
  LDirection: TGraphDirection;
  LInverseDirection: TGraphDirection;
  LRemoved: Boolean;
  LRoot: TGraph;
  LStoredGroup: TGraphRuleGroup;
  LStoredParentedGroup: TParentedGraphRuleGroup;
begin
  if Assigned(Parent) then
  begin
    if Assigned(Parent.FPassRoot) then
      LRoot := Parent.FPassRoot
    else
      LRoot := Parent;
    if LRoot.FRunning then
      raise EInvalidOperation.Create(
        'DenyAll::cannot change rules while the pipeline is running');
  end;

  if Assigned(Parent) then
  begin
    //RuleGroups is intentionally public for legacy compatibility.  Validate
    //the complete owning registry before changing this group so a nil,
    //replacement, or identity-corrupt entry cannot leave reciprocal rules
    //half updated when DenyAll later discovers it.
    if Parent.FRuleGroups.Count <> Length(Parent.FValues) then
      raise EInvalidOperation.CreateFmt(
        'DenyAll::pass %d has an inconsistent value registry',
        [Parent.FPassIndex]);
    for I := 0 to High(Parent.FValues) do
    begin
      if not Parent.FRuleGroups.TryGetValue(Parent.FValues[I],
        LStoredGroup) or (not Assigned(LStoredGroup)) then
        raise EInvalidOperation.CreateFmt(
          'DenyAll::pass %d has no rule group for value "%s"',
          [Parent.FPassIndex, Parent.FValues[I]]);
      if LStoredGroup.Value <> Parent.FValues[I] then
        raise EInvalidOperation.CreateFmt(
          'DenyAll::pass %d rule-group identity "%s" does not match value "%s"',
          [Parent.FPassIndex, LStoredGroup.Value, Parent.FValues[I]]);
      if not (LStoredGroup is TParentedGraphRuleGroup) then
        raise EInvalidOperation.CreateFmt(
          'DenyAll::pass %d value "%s" is not owned by the graph',
          [Parent.FPassIndex, Parent.FValues[I]]);
      LStoredParentedGroup := TParentedGraphRuleGroup(LStoredGroup);
      if LStoredParentedGroup.Parent <> Parent then
        raise EInvalidOperation.CreateFmt(
          'DenyAll::pass %d value "%s" has a different owner',
          [Parent.FPassIndex, Parent.FValues[I]]);
    end;
    if (not Parent.FRuleGroups.TryGetValue(Value, LStoredGroup))
      or (LStoredGroup <> Self) then
      raise EInvalidOperation.CreateFmt(
        'DenyAll::pass %d source value "%s" is not the registered object',
        [Parent.FPassIndex, Value]);
  end;
  inherited DoDenyAll(ADirections);

  if not Assigned(Parent) then
    Exit;
  //NewRule maintains an explicit reciprocal finite model.  Remove the
  //corresponding inverse claims when support is denied; if an inverse finite
  //set loses its last value, represent that state with DenyAll rather than a
  //present-empty rule (which remains a legacy wildcard).
  for LDirection in ADirections do
  begin
    LInverseDirection := InverseOfDir(LDirection);
    for LBaseGroup in Parent.RuleGroups.Values do
    begin
      LBaseGroup.RemoveRuleValue(LInverseDirection, Value,
        LRemoved, LBecameEmpty);
      if LRemoved and LBecameEmpty then
        LBaseGroup.ApplyDenyAll([LInverseDirection]);
    end;
  end;
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
var
  LPreviousIndex: Integer;
begin
  if Assigned(Parent) and (Parent.CurrentPassIndex = 0) then
    raise EInvalidOperation.Create(
      'RequirePrevious::pass zero has no preceding pass');
  if Assigned(Parent) then
  begin
    Parent.SynchronizePreviousValueDependencies;
    LPreviousIndex := Pred(Parent.CurrentPassIndex);
    Parent.AddDependencyRole(LPreviousIndex, pdrRequirement);
    AddPassRequirement(LPreviousIndex, AValue, proPrevious);
  end;
  inherited DoRequirePrevious(AValue);
end;

procedure TGraph.TParentedGraphRuleGroup.DoRequireFromPass(
  const APass: String; const AValue: TGraphValue);
var
  LPassIndex: Integer;
begin
  if not Assigned(Parent) then
    raise EInvalidOperation.Create(
      'RequireFromPass::rule group is not owned by a graph');
  LPassIndex := Parent.PassIndexForLabel(APass, 'RequireFromPass');
  Parent.SynchronizePreviousValueDependencies;
  Parent.AddDependencyRole(LPassIndex, pdrRequirement);
  AddPassRequirement(LPassIndex, AValue, proNamed);
end;

procedure TGraph.TParentedGraphRuleGroup.DoRequireFromPassAt(
  const APass: String; const AOffset: TGraphOffset;
  const AValues: TGraphValues);
var
  LPassIndex: Integer;
  LTerm: TGraphPassMatchTerm;
  LTerms: TGraphPassMatchTerms;
begin
  //Canonicalize and validate all caller-owned arrays before dependency state
  //can change. AddPassOffsetRequirement clones again for durable ownership.
  LTerm := MakeGraphPassMatchTerm(AOffset, AValues);
  SetLength(LTerms, 1);
  LTerms[0] := LTerm;
  LTerms := CanonicalGraphPassMatchTerms(LTerms,
    'RequireFromPassAt');
  if not Assigned(Parent) then
    raise EInvalidOperation.Create(
      'RequireFromPassAt::rule group is not owned by a graph');
  LPassIndex := Parent.PassIndexForLabel(APass,
    'RequireFromPassAt');
  Parent.SynchronizePreviousValueDependencies;
  Parent.AddDependencyRole(LPassIndex, pdrRequirement);
  AddPassOffsetRequirement(LPassIndex, LTerms[0].Offset,
    LTerms[0].Values, proNamed);
end;

procedure TGraph.TParentedGraphRuleGroup.DoRequireAnyFromPass(
  const APass: String; const ATerms: TGraphPassMatchTerms);
var
  LCanonical: TGraphPassMatchTerms;
  LPassIndex: Integer;
begin
  //The complete clause must be known-good before adding its inferred edge.
  LCanonical := CanonicalGraphPassMatchTerms(ATerms,
    'RequireAnyFromPass');
  if not Assigned(Parent) then
    raise EInvalidOperation.Create(
      'RequireAnyFromPass::rule group is not owned by a graph');
  LPassIndex := Parent.PassIndexForLabel(APass,
    'RequireAnyFromPass');
  Parent.SynchronizePreviousValueDependencies;
  Parent.AddDependencyRole(LPassIndex, pdrRequirement);
  AddPassAnyRequirement(LPassIndex, LCanonical, proNamed);
end;

{ TGraphRuleGroup }

procedure TGraphRuleGroup.AddPassRequirement(const APassIndex: Integer;
  const AValue: TGraphValue; const AOrigin: TPassRequirementOrigin);
var
  I, LInsertIndex, LValueIndex: Integer;
  LRequirement: TPassRequirement;
begin
  //Legacy RequirePrevious/RequireFromPass retain their historical value
  //acceptance. New spatial constructors perform stricter empty-value
  //validation before they reach AddPassOffsetRequirement.
  LInsertIndex := Length(FPassRequirements);
  for I := 0 to High(FPassRequirements) do
  begin
    if (FPassRequirements[I].PassIndex = APassIndex)
      and (FPassRequirements[I].Kind = prkMergedOffset)
      and (Length(FPassRequirements[I].Terms) = 1)
      and IsZeroGraphOffset(FPassRequirements[I].Terms[0].Offset) then
    begin
      Include(FPassRequirements[I].Origins, AOrigin);
      if ContainsGraphValue(FPassRequirements[I].Terms[0].Values,
        AValue) then
        Exit;
      LValueIndex := Length(FPassRequirements[I].Terms[0].Values);
      SetLength(FPassRequirements[I].Terms[0].Values,
        Succ(LValueIndex));
      FPassRequirements[I].Terms[0].Values[LValueIndex] := AValue;
      Exit;
    end;
    if (LInsertIndex = Length(FPassRequirements))
      and (FPassRequirements[I].PassIndex > APassIndex) then
      LInsertIndex := I;
  end;

  LRequirement.PassIndex := APassIndex;
  SetLength(LRequirement.Terms, 1);
  LRequirement.Terms[0].Offset := MakeGraphOffset(0, 0, 0);
  SetLength(LRequirement.Terms[0].Values, 1);
  LRequirement.Terms[0].Values[0] := AValue;
  LRequirement.Origins := [AOrigin];
  LRequirement.Kind := prkMergedOffset;
  SetLength(FPassRequirements, Succ(Length(FPassRequirements)));
  for I := High(FPassRequirements) downto Succ(LInsertIndex) do
    FPassRequirements[I] := FPassRequirements[Pred(I)];
  FPassRequirements[LInsertIndex] := LRequirement;
end;

procedure TGraphRuleGroup.AddPassOffsetRequirement(
  const APassIndex: Integer; const AOffset: TGraphOffset;
  const AValues: TGraphValues; const AOrigin: TPassRequirementOrigin);
var
  I, LInsertIndex, LValueIndex: Integer;
  LRequirement: TPassRequirement;
  LTerms: TGraphPassMatchTerms;
begin
  SetLength(LTerms, 1);
  LTerms[0] := MakeGraphPassMatchTerm(AOffset, AValues);
  LTerms := CanonicalGraphPassMatchTerms(LTerms,
    'PassRequirement');
  LInsertIndex := Length(FPassRequirements);
  for I := 0 to High(FPassRequirements) do
  begin
    if (FPassRequirements[I].PassIndex = APassIndex)
      and (FPassRequirements[I].Kind = prkMergedOffset)
      and (Length(FPassRequirements[I].Terms) = 1)
      and GraphOffsetsEqual(FPassRequirements[I].Terms[0].Offset,
        LTerms[0].Offset) then
    begin
      Include(FPassRequirements[I].Origins, AOrigin);
      for LValueIndex := 0 to High(LTerms[0].Values) do
        if not ContainsGraphValue(
          FPassRequirements[I].Terms[0].Values,
          LTerms[0].Values[LValueIndex]) then
          Insert(LTerms[0].Values[LValueIndex],
            FPassRequirements[I].Terms[0].Values,
            Length(FPassRequirements[I].Terms[0].Values));
      Exit;
    end;
    if (LInsertIndex = Length(FPassRequirements))
      and (FPassRequirements[I].PassIndex > APassIndex) then
      LInsertIndex := I;
  end;

  LRequirement.PassIndex := APassIndex;
  LRequirement.Terms := CanonicalGraphPassMatchTerms(LTerms,
    'PassRequirement');
  LRequirement.Origins := [AOrigin];
  LRequirement.Kind := prkMergedOffset;
  SetLength(FPassRequirements, Succ(Length(FPassRequirements)));
  for I := High(FPassRequirements) downto Succ(LInsertIndex) do
    FPassRequirements[I] := FPassRequirements[Pred(I)];
  FPassRequirements[LInsertIndex] := LRequirement;
end;

procedure TGraphRuleGroup.AddPassAnyRequirement(
  const APassIndex: Integer; const ATerms: TGraphPassMatchTerms;
  const AOrigin: TPassRequirementOrigin);
var
  I, LInsertIndex: Integer;
  LCanonical: TGraphPassMatchTerms;
  LRequirement: TPassRequirement;
begin
  LCanonical := CanonicalGraphPassMatchTerms(ATerms,
    'PassRequirement');
  LInsertIndex := Length(FPassRequirements);
  for I := 0 to High(FPassRequirements) do
  begin
    if (FPassRequirements[I].PassIndex = APassIndex)
      and (FPassRequirements[I].Kind = prkAny)
      and GraphPassMatchTermsEqual(FPassRequirements[I].Terms,
        LCanonical) then
    begin
      Include(FPassRequirements[I].Origins, AOrigin);
      Exit;
    end;
    if (LInsertIndex = Length(FPassRequirements))
      and (FPassRequirements[I].PassIndex > APassIndex) then
      LInsertIndex := I;
  end;

  LRequirement.PassIndex := APassIndex;
  LRequirement.Terms := CanonicalGraphPassMatchTerms(LCanonical,
    'PassRequirement');
  LRequirement.Origins := [AOrigin];
  LRequirement.Kind := prkAny;
  SetLength(FPassRequirements, Succ(Length(FPassRequirements)));
  for I := High(FPassRequirements) downto Succ(LInsertIndex) do
    FPassRequirements[I] := FPassRequirements[Pred(I)];
  FPassRequirements[LInsertIndex] := LRequirement;
end;

function TGraphRuleGroup.GetExists(const ADirection : TGraphDirection): Boolean;
begin
  Result := IndexOfDirection(ADirection) >= 0;
end;

function TGraphRuleGroup.GetDenied(
  const ADirection: TGraphDirection): Boolean;
begin
  Result := ADirection in FDeniedDirections;
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

procedure TGraphRuleGroup.DoDenyAll(
  const ADirections: TGraphDirections);
begin
  ApplyDenyAll(ADirections);
end;

procedure TGraphRuleGroup.ApplyDenyAll(
  const ADirections: TGraphDirections);
var
  I: Integer;
  LCount: Integer;
  LRules: TGraphRules;
begin
  if ADirections = [] then
    Exit;

  //Prepare the replacement before changing live state.  Denied directions
  //have no legacy rule record, preserving the historical meaning of a
  //present rule whose value list is empty.
  SetLength(LRules, Length(FRules));
  LCount := 0;
  for I := 0 to High(FRules) do
    if not (FRules[I].Key in ADirections) then
    begin
      LRules[LCount] := FRules[I];
      Inc(LCount);
    end;
  SetLength(LRules, LCount);
  FRules := LRules;
  FDeniedDirections := FDeniedDirections + ADirections;
end;

procedure TGraphRuleGroup.RemoveRuleValue(
  const ADirection: TGraphDirection; const AValue: TGraphValue;
  out ARemoved, ABecameEmpty: Boolean);
var
  I: Integer;
  LCount: Integer;
  LRule: TGraphRule;
  LVals: TGraphValues;
begin
  ARemoved := False;
  ABecameEmpty := False;
  I := IndexOfDirection(ADirection);
  if I < 0 then
    Exit;
  LRule := FRules[I];
  SetLength(LVals, Length(LRule.Value));
  LCount := 0;
  for I := 0 to High(LRule.Value) do
    if LRule.Value[I] = AValue then
      ARemoved := True
    else
    begin
      LVals[LCount] := LRule.Value[I];
      Inc(LCount);
    end;
  if not ARemoved then
    Exit;
  SetLength(LVals, LCount);
  LRule.Value := LVals;
  I := IndexOfDirection(ADirection);
  FRules[I] := LRule;
  ABecameEmpty := LCount = 0;
end;

procedure TGraphRuleGroup.SetWeight(const AValue: TGraphWeight);
begin
  if AValue < 1 then
    raise ERangeError.CreateFmt(
      'SetWeight::weight must be positive [%d]', [AValue]);
  FWeight := AValue;
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

    //Finite support explicitly restores a direction denied earlier.  This
    //also applies to inverse-rule synchronization, which calls UpsertRule.
    Exclude(FDeniedDirections, LDir);
  end;
end;

procedure TGraphRuleGroup.DoRequirePrevious(const AValue: TGraphValue);
begin
  if not ContainsGraphValue(FPreviousValues, AValue) then
    Insert(AValue, FPreviousValues, Length(FPreviousValues));
end;

procedure TGraphRuleGroup.DoRequireFromPass(const APass: String;
  const AValue: TGraphValue);
begin
  raise EInvalidOperation.CreateFmt(
    'RequireFromPass::rule group is not owned by a graph [%s]', [APass]);
end;

procedure TGraphRuleGroup.DoRequireFromPassAt(const APass: String;
  const AOffset: TGraphOffset; const AValues: TGraphValues);
begin
  if Length(AValues) = 0 then
    raise EArgumentException.Create(
      'RequireFromPassAt::values cannot be empty');
  raise EInvalidOperation.CreateFmt(
    'RequireFromPassAt::rule group is not owned by a graph [%s]',
    [APass]);
end;

procedure TGraphRuleGroup.DoRequireAnyFromPass(const APass: String;
  const ATerms: TGraphPassMatchTerms);
begin
  CanonicalGraphPassMatchTerms(ATerms, 'RequireAnyFromPass');
  raise EInvalidOperation.CreateFmt(
    'RequireAnyFromPass::rule group is not owned by a graph [%s]',
    [APass]);
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

function TGraphRuleGroup.DenyAll(
  const ADirections: TGraphDirections): TGraphRuleGroup;
begin
  Result := Self;
  DoDenyAll(ADirections);
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

function TGraphRuleGroup.RequireFromPass(const APass: String;
  const AValue: TGraphValue): TGraphRuleGroup;
begin
  Result := Self;
  DoRequireFromPass(APass, AValue);
end;

function TGraphRuleGroup.RequireFromPass(const APass: String;
  const AValues: TGraphValues): TGraphRuleGroup;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to High(AValues) do
    DoRequireFromPass(APass, AValues[I]);
end;

function TGraphRuleGroup.RequireFromPassAt(const APass: String;
  const AOffset: TGraphOffset;
  const AValue: TGraphValue): TGraphRuleGroup;
var
  LValues: TGraphValues;
begin
  Result := Self;
  SetLength(LValues, 1);
  LValues[0] := AValue;
  DoRequireFromPassAt(APass, AOffset, LValues);
end;

function TGraphRuleGroup.RequireFromPassAt(const APass: String;
  const AOffset: TGraphOffset;
  const AValues: TGraphValues): TGraphRuleGroup;
begin
  Result := Self;
  if Length(AValues) = 0 then
    raise EArgumentException.Create(
      'RequireFromPassAt::values cannot be empty');
  DoRequireFromPassAt(APass, AOffset, AValues);
end;

function TGraphRuleGroup.RequireAnyFromPass(const APass: String;
  const ATerms: TGraphPassMatchTerms): TGraphRuleGroup;
var
  LCanonical: TGraphPassMatchTerms;
begin
  Result := Self;
  //Validate before dispatch so even unusual descendants receive a complete
  //clause and cannot mutate dependency state before discovering bad input.
  LCanonical := CanonicalGraphPassMatchTerms(ATerms,
    'RequireAnyFromPass');
  DoRequireAnyFromPass(APass, LCanonical);
end;

constructor TGraphRuleGroup.Create;
begin
  FVal := '';
  FWeight := WFC_DEFAULT_VALUE_WEIGHT;
  SetLength(FRules, 0);
  FDeniedDirections := [];
  SetLength(FPreviousValues, 0);
  SetLength(FPassRequirements, 0);
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

procedure TGraphEntry.RestoreValueState(const AValue: TGraphValue;
  const AEmpty, AGenerated: Boolean);
begin
  //Used only to roll back an exception raised while atomically committing a
  //fully solved pipeline. Hooks are intentionally bypassed during rollback;
  //their external side effects cannot be made transactional by this unit.
  FVal := AValue;
  FEmpty := AEmpty;
  FGenerated := AGenerated;
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
  FAllowedValues := Default(TGraphValues);
  FHasAllowedValues := False;
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

function TGraph.HasPassRequirement(const APassIndex: Integer): Boolean;
var
  I: Integer;
  LGroup: TGraphRuleGroup;
begin
  for LGroup in FRuleGroups.Values do
    if Assigned(LGroup) then
      for I := 0 to High(LGroup.FPassRequirements) do
        if LGroup.FPassRequirements[I].PassIndex = APassIndex then
          Exit(True);
  Result := False;
end;

function TGraph.HasPreviousValueRequirement: Boolean;
var
  LGroup: TGraphRuleGroup;
begin
  for LGroup in FRuleGroups.Values do
    if Assigned(LGroup) and (Length(LGroup.PreviousValues) > 0) then
      Exit(True);
  Result := False;
end;

function TGraph.ResolveOffsetIndex(const AEntryIndex: Integer;
  const AOffset: TGraphOffset; out AResolvedIndex: Integer): Boolean;
var
  LPosition: TGraphPosition;
  LX, LY, LZ: Integer;

  function ResolveAxis(const ACoordinate: TGraphCoordinate;
    const ASize: TGraphCoordinate; const ADelta: Integer;
    out AResolved: Integer): Boolean;
  var
    LCoordinate, LDeltaRemainder, LMagnitude, LSize: Integer;
  begin
    LSize := Integer(ASize);
    if LSize <= 0 then
      raise EInvalidOperation.Create(
        'ResolveOffsetIndex::graph dimension cannot be empty');
    LCoordinate := Integer(ACoordinate);
    if (LCoordinate < 0) or (LCoordinate >= LSize) then
      raise EInvalidOperation.Create(
        'ResolveOffsetIndex::entry coordinate is outside its dimension');

    if not FWrap then
    begin
      if ADelta >= 0 then
      begin
        //Check before adding so even High(Integer) is harmless.
        if (ADelta >= LSize)
          or (LCoordinate > Pred(LSize) - ADelta) then
          Exit(False);
      end
      else
      begin
        //-LCoordinate is representable because stored coordinates never
        //exceed High(Integer); this avoids negating Low(Integer).
        if ADelta < -LCoordinate then
          Exit(False);
      end;
      AResolved := LCoordinate + ADelta;
      Exit(True);
    end;

    //Modulo by a positive dimension is defined for Low(Integer), unlike
    //taking Abs or negating that value.
    LDeltaRemainder := ADelta mod LSize;
    if LDeltaRemainder >= 0 then
    begin
      if LCoordinate >= LSize - LDeltaRemainder then
        AResolved := LCoordinate - (LSize - LDeltaRemainder)
      else
        AResolved := LCoordinate + LDeltaRemainder;
    end
    else
    begin
      //A negative remainder has magnitude below LSize and therefore cannot
      //be Low(Integer), even when the original delta was.
      LMagnitude := -LDeltaRemainder;
      if LCoordinate < LMagnitude then
        AResolved := LCoordinate + (LSize - LMagnitude)
      else
        AResolved := LCoordinate - LMagnitude;
    end;
    Result := True;
  end;

begin
  AResolvedIndex := -1;
  if (AEntryIndex < 0) or (AEntryIndex >= FEntries.Count) then
    raise ERangeError.CreateFmt(
      'ResolveOffsetIndex::entry index out of bounds [%d]',
      [AEntryIndex]);
  LPosition := FEntries[AEntryIndex].Position;
  if (not ResolveAxis(LPosition.X, FDimension.Width,
      AOffset.DeltaX, LX))
    or (not ResolveAxis(LPosition.Y, FDimension.Height,
      AOffset.DeltaY, LY))
    or (not ResolveAxis(LPosition.Z, FDimension.Depth,
      AOffset.DeltaZ, LZ)) then
    Exit(False);
  AResolvedIndex := CoordToIndex(TGraphCoordinate(LX),
    TGraphCoordinate(LY), TGraphCoordinate(LZ));
  Result := True;
end;

procedure TGraph.SynchronizePreviousValueDependencies;
type
  TDependencyRoleChange = record
    ConsumerIndex: Integer;
    ProviderIndex: Integer;
    PreviousValuesRole: Boolean;
  end;
  TDependencyRoleChanges = array of TDependencyRoleChange;
var
  I, J, LAppliedAddCount, LSlot: Integer;
  LAdditions, LRemovals: TDependencyRoleChanges;
  LGraph, LRoot: TGraph;
  LHasRole: Boolean;

  procedure AppendChange(var AChanges: TDependencyRoleChanges;
    const AConsumerIndex, AProviderIndex: Integer;
    const APreviousValuesRole: Boolean);
  var
    LIndex: Integer;
  begin
    LIndex := Length(AChanges);
    SetLength(AChanges, Succ(LIndex));
    AChanges[LIndex].ConsumerIndex := AConsumerIndex;
    AChanges[LIndex].ProviderIndex := AProviderIndex;
    AChanges[LIndex].PreviousValuesRole := APreviousValuesRole;
  end;

  procedure RecordDifference(const AConsumerIndex,
    AProviderIndex: Integer; const APreviousValuesRole: Boolean;
    const ANeedsRole: Boolean);
  begin
    LGraph := LRoot.FPasses[AConsumerIndex];
    LSlot := LGraph.DependencySlot(AProviderIndex);
    if APreviousValuesRole then
      LHasRole := (LSlot >= 0) and (pdrPreviousValues in
        LGraph.FPassDependencies[LSlot].Roles)
    else
      LHasRole := (LSlot >= 0) and (pdrRequirement in
        LGraph.FPassDependencies[LSlot].Roles);
    if ANeedsRole = LHasRole then
      Exit;
    if ANeedsRole then
      AppendChange(LAdditions, AConsumerIndex, AProviderIndex,
        APreviousValuesRole)
    else
      AppendChange(LRemovals, AConsumerIndex, AProviderIndex,
        APreviousValuesRole);
  end;

  procedure AddChange(const AChange: TDependencyRoleChange);
  begin
    if AChange.PreviousValuesRole then
      LRoot.FPasses[AChange.ConsumerIndex].AddDependencyRole(
        AChange.ProviderIndex, pdrPreviousValues)
    else
      LRoot.FPasses[AChange.ConsumerIndex].AddDependencyRole(
        AChange.ProviderIndex, pdrRequirement);
  end;

  procedure RemoveChange(const AChange: TDependencyRoleChange);
  begin
    if AChange.PreviousValuesRole then
      LRoot.FPasses[AChange.ConsumerIndex].RemoveDependencyRole(
        AChange.ProviderIndex, pdrPreviousValues)
    else
      LRoot.FPasses[AChange.ConsumerIndex].RemoveDependencyRole(
        AChange.ProviderIndex, pdrRequirement);
  end;
begin
  if Assigned(FPassRoot) then
    LRoot := FPassRoot
  else
    LRoot := Self;
  LRoot.EnsureInitialPass;

  //Both bound requirements and legacy PreviousValues can be changed through
  //the public owning rule-group dictionary. Reconcile their internal roles
  //from the live registry so replacement cannot leave a stale protected edge
  //or omit an edge needed by topology and selective-closure planning.
  for I := 0 to Pred(LRoot.FPasses.Count) do
  begin
    LGraph := LRoot.FPasses[I];
    for J := 0 to Pred(LRoot.FPasses.Count) do
      RecordDifference(I, J, False,
        LGraph.HasPassRequirement(J));
    if I > 0 then
      RecordDifference(I, Pred(I), True,
        LGraph.HasPreviousValueRequirement);
  end;
  if (Length(LAdditions) = 0) and (Length(LRemovals) = 0) then
    Exit;
  if LRoot.FRunning then
    raise EInvalidOperation.Create(
      'PassDependencies::cannot synchronize requirements while the pipeline is running');

  //Remove obsolete roles before validating additions, then undo both phases
  //if any later inferred edge would close a cycle. This makes reconciliation
  //of several public dictionary edits atomic.
  LAppliedAddCount := 0;
  try
    for I := 0 to High(LRemovals) do
      RemoveChange(LRemovals[I]);
    for I := 0 to High(LAdditions) do
    begin
      AddChange(LAdditions[I]);
      Inc(LAppliedAddCount);
    end;
  except
    for I := Pred(LAppliedAddCount) downto 0 do
      RemoveChange(LAdditions[I]);
    for I := 0 to High(LRemovals) do
      AddChange(LRemovals[I]);
    raise;
  end;
end;

function TGraph.GetDependencyCount: Integer;
var
  LGraph, LRoot: TGraph;
begin
  LGraph := GetActivePassGraph;
  if Assigned(LGraph.FPassRoot) then
    LRoot := LGraph.FPassRoot
  else
    LRoot := LGraph;
  LRoot.SynchronizePreviousValueDependencies;
  Result := Length(LGraph.FPassDependencies);
end;

function TGraph.GetDependencyIndex(const AOrdinal: Integer): Integer;
var
  LGraph, LRoot: TGraph;
begin
  LGraph := GetActivePassGraph;
  if Assigned(LGraph.FPassRoot) then
    LRoot := LGraph.FPassRoot
  else
    LRoot := LGraph;
  LRoot.SynchronizePreviousValueDependencies;
  if (AOrdinal < 0) or (AOrdinal >= Length(LGraph.FPassDependencies)) then
    raise ERangeError.CreateFmt(
      'GetDependencyIndex::ordinal out of bounds [%d]', [AOrdinal]);
  Result := LGraph.FPassDependencies[AOrdinal].PassIndex;
end;

function TGraph.GetPassMode: TGraphPassMode;
begin
  Result := GetActivePassGraph.FPassMode;
end;

function TGraph.GetTransformSourceIndex: Integer;
begin
  Result := GetActivePassGraph.FTransformSourceIndex;
end;

function TGraph.DependencySlot(const APassIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FPassDependencies) do
    if FPassDependencies[I].PassIndex = APassIndex then
      Exit(I);
end;

function TGraph.PassIndexForLabel(const APass,
  AOperation: String): Integer;
var
  LRoot: TGraph;
begin
  if Assigned(FPassRoot) then
    LRoot := FPassRoot
  else
    LRoot := Self;
  LRoot.EnsureInitialPass;
  if not LRoot.FPassLookup.ContainsKey(APass) then
    raise EArgumentException.CreateFmt(
      '%s::unknown pass label "%s"', [AOperation, APass]);
  Result := LRoot.FPassLookup[APass];
end;

function TGraph.WouldCreateDependencyCycle(const AConsumerIndex,
  AProviderIndex: Integer): Boolean;
var
  I, LNode, LRootCount, LStackCount: Integer;
  LDependency: TPassDependency;
  LRoot: TGraph;
  LSeen: array of Byte;
  LStack: TGraphPassIndices;
begin
  if Assigned(FPassRoot) then
    LRoot := FPassRoot
  else
    LRoot := Self;
  LRoot.EnsureInitialPass;
  LRootCount := LRoot.FPasses.Count;
  if (AConsumerIndex < 0) or (AConsumerIndex >= LRootCount)
    or (AProviderIndex < 0) or (AProviderIndex >= LRootCount) then
    raise ERangeError.CreateFmt(
      'WouldCreateDependencyCycle::pass index out of bounds [%d -> %d]',
      [AConsumerIndex, AProviderIndex]);
  if AConsumerIndex = AProviderIndex then
    Exit(True);

  SetLength(LSeen, LRootCount);
  SetLength(LStack, LRootCount);
  LStackCount := 1;
  LStack[0] := AProviderIndex;
  LSeen[AProviderIndex] := 1;
  while LStackCount > 0 do
  begin
    Dec(LStackCount);
    LNode := LStack[LStackCount];
    if LNode = AConsumerIndex then
      Exit(True);
    LSeen[LNode] := 2;
    for I := 0 to High(LRoot.FPasses[LNode].FPassDependencies) do
    begin
      LDependency := LRoot.FPasses[LNode].FPassDependencies[I];
      if (LDependency.PassIndex < 0)
        or (LDependency.PassIndex >= LRootCount) then
        raise EInvalidOperation.CreateFmt(
          'WouldCreateDependencyCycle::pass %d has invalid dependency %d',
          [LNode, LDependency.PassIndex]);
      if LSeen[LDependency.PassIndex] = 0 then
      begin
        if LStackCount >= Length(LStack) then
          raise EInvalidOperation.Create(
            'WouldCreateDependencyCycle::dependency graph is malformed');
        LStack[LStackCount] := LDependency.PassIndex;
        LSeen[LDependency.PassIndex] := 1;
        Inc(LStackCount);
      end;
    end;
  end;
  Result := False;
end;

procedure TGraph.AddDependencyRole(const APassIndex: Integer;
  const ARole: TPassDependencyRole);
var
  I, LInsertIndex, LSlot: Integer;
  LGraph, LRoot: TGraph;
begin
  LGraph := GetActivePassGraph;
  if Assigned(LGraph.FPassRoot) then
    LRoot := LGraph.FPassRoot
  else
    LRoot := LGraph;
  if LRoot.FRunning then
    raise EInvalidOperation.Create(
      'DependsOn::cannot change dependencies while the pipeline is running');
  if (APassIndex < 0) or (APassIndex >= LRoot.FPasses.Count) then
    raise ERangeError.CreateFmt(
      'DependsOn::pass index out of bounds [%d]', [APassIndex]);
  if APassIndex = LGraph.FPassIndex then
    raise EInvalidOperation.CreateFmt(
      'DependsOn::pass %d cannot depend on itself', [APassIndex]);

  LSlot := LGraph.DependencySlot(APassIndex);
  if LSlot >= 0 then
  begin
    Include(LGraph.FPassDependencies[LSlot].Roles, ARole);
    Exit;
  end;
  if LRoot.WouldCreateDependencyCycle(LGraph.FPassIndex, APassIndex) then
    raise EInvalidOperation.CreateFmt(
      'DependsOn::dependency %d -> %d would create a cycle',
      [LGraph.FPassIndex, APassIndex]);

  LInsertIndex := Length(LGraph.FPassDependencies);
  for I := 0 to High(LGraph.FPassDependencies) do
    if LGraph.FPassDependencies[I].PassIndex > APassIndex then
    begin
      LInsertIndex := I;
      Break;
    end;
  SetLength(LGraph.FPassDependencies,
    Succ(Length(LGraph.FPassDependencies)));
  for I := High(LGraph.FPassDependencies) downto Succ(LInsertIndex) do
    LGraph.FPassDependencies[I] := LGraph.FPassDependencies[Pred(I)];
  LGraph.FPassDependencies[LInsertIndex].PassIndex := APassIndex;
  LGraph.FPassDependencies[LInsertIndex].Roles := [ARole];
end;

procedure TGraph.RemoveDependencyRole(const APassIndex: Integer;
  const ARole: TPassDependencyRole);
var
  I, LSlot: Integer;
  LGraph: TGraph;
begin
  LGraph := GetActivePassGraph;
  LSlot := LGraph.DependencySlot(APassIndex);
  if LSlot < 0 then
    Exit;
  Exclude(LGraph.FPassDependencies[LSlot].Roles, ARole);
  if LGraph.FPassDependencies[LSlot].Roles <> [] then
    Exit;
  for I := LSlot to Pred(High(LGraph.FPassDependencies)) do
    LGraph.FPassDependencies[I] := LGraph.FPassDependencies[Succ(I)];
  SetLength(LGraph.FPassDependencies,
    Pred(Length(LGraph.FPassDependencies)));
end;

procedure TGraph.SetPassMode(const AValue: TGraphPassMode);
var
  LModeOrdinal: Integer;
  LGraph, LRoot: TGraph;
  LPreviousIndex, LSlot, LSourceIndex: Integer;
begin
  LModeOrdinal := Ord(AValue);
  if (LModeOrdinal < Ord(Low(TGraphPassMode)))
    or (LModeOrdinal > Ord(High(TGraphPassMode))) then
    raise ERangeError.Create('SetPassMode::invalid pass mode');
  LGraph := GetActivePassGraph;
  if Assigned(LGraph.FPassRoot) then
    LRoot := LGraph.FPassRoot
  else
    LRoot := LGraph;
  if LRoot.FRunning then
    raise EInvalidOperation.Create(
      'SetPassMode::cannot change pass mode while the pipeline is running');
  LRoot.SynchronizePreviousValueDependencies;
  if LGraph.FPassMode = AValue then
    Exit;

  LSourceIndex := LGraph.FTransformSourceIndex;
  if AValue = gpmTransform then
  begin
    if LSourceIndex < 0 then
    begin
      if Length(LGraph.FPassDependencies) <> 1 then
        raise EInvalidOperation.CreateFmt(
          'SetPassMode::transform pass %d requires exactly one unambiguous source',
          [LGraph.FPassIndex]);
      LSourceIndex := LGraph.FPassDependencies[0].PassIndex;
    end;
    if LSourceIndex = LGraph.FPassIndex then
      raise EInvalidOperation.Create(
        'SetPassMode::a transform pass cannot source itself');
  end;
  if (AValue = gpmLegacy) and (LGraph.FPassIndex > 0) then
  begin
    LPreviousIndex := Pred(LGraph.FPassIndex);
    if (LGraph.DependencySlot(LPreviousIndex) < 0)
      and LRoot.WouldCreateDependencyCycle(LGraph.FPassIndex,
        LPreviousIndex) then
      raise EInvalidOperation.CreateFmt(
        'SetPassMode::legacy predecessor %d would create a cycle',
        [LPreviousIndex]);
  end;

  //Leaving the compatibility mode makes its sequential edge an ordinary,
  //removable declaration. This retains sequential-by-default behavior while
  //allowing an explicit overlay or transform to branch after ClearDependencies.
  if (LGraph.FPassMode = gpmLegacy) and (LGraph.FPassIndex > 0) then
  begin
    LSlot := LGraph.DependencySlot(Pred(LGraph.FPassIndex));
    if LSlot >= 0 then
    begin
      Exclude(LGraph.FPassDependencies[LSlot].Roles, pdrLegacy);
      Include(LGraph.FPassDependencies[LSlot].Roles, pdrDeclared);
    end;
  end;
  if (LGraph.FPassMode = gpmTransform)
    and (LGraph.FTransformSourceIndex >= 0) then
    LGraph.RemoveDependencyRole(LGraph.FTransformSourceIndex,
      pdrTransformSource);
  LGraph.FTransformSourceIndex := -1;

  case AValue of
    gpmLegacy:
      begin
        if LGraph.FPassIndex > 0 then
          LGraph.AddDependencyRole(Pred(LGraph.FPassIndex), pdrLegacy);
      end;
    gpmTransform:
      begin
        LGraph.AddDependencyRole(LSourceIndex, pdrTransformSource);
        LGraph.FTransformSourceIndex := LSourceIndex;
      end;
    gpmOverlay:
      ;
  else
    raise ERangeError.Create('SetPassMode::invalid pass mode');
  end;
  LGraph.FPassMode := AValue;
end;

procedure TGraph.BuildPassExecutionOrder(out AOrder: TGraphPassIndices);
var
  I, J, LCandidate, LOrderCount, LPassCount: Integer;
  LDependency: TPassDependency;
  LEmitted: array of Byte;
  LInDegree: array of Integer;
  LRoot: TGraph;
begin
  if Assigned(FPassRoot) then
    LRoot := FPassRoot
  else
    LRoot := Self;
  LRoot.EnsureInitialPass;
  LRoot.SynchronizePreviousValueDependencies;
  LPassCount := LRoot.FPasses.Count;
  SetLength(AOrder, LPassCount);
  SetLength(LEmitted, LPassCount);
  SetLength(LInDegree, LPassCount);
  for I := 0 to Pred(LPassCount) do
  begin
    LInDegree[I] := Length(LRoot.FPasses[I].FPassDependencies);
    for J := 0 to High(LRoot.FPasses[I].FPassDependencies) do
    begin
      LDependency := LRoot.FPasses[I].FPassDependencies[J];
      if (LDependency.PassIndex < 0)
        or (LDependency.PassIndex >= LPassCount) then
        raise EInvalidOperation.CreateFmt(
          'BuildPassExecutionOrder::pass %d has invalid dependency %d',
          [I, LDependency.PassIndex]);
      if LDependency.PassIndex = I then
        raise EInvalidOperation.CreateFmt(
          'BuildPassExecutionOrder::pass %d depends on itself', [I]);
      if (J > 0) and
        (LRoot.FPasses[I].FPassDependencies[Pred(J)].PassIndex >=
          LDependency.PassIndex) then
        raise EInvalidOperation.CreateFmt(
          'BuildPassExecutionOrder::pass %d dependencies are not canonical',
          [I]);
    end;
  end;

  LOrderCount := 0;
  while LOrderCount < LPassCount do
  begin
    LCandidate := -1;
    for I := 0 to Pred(LPassCount) do
      if (LEmitted[I] = 0) and (LInDegree[I] = 0) then
      begin
        LCandidate := I;
        Break;
      end;
    if LCandidate < 0 then
      raise EInvalidOperation.Create(
        'BuildPassExecutionOrder::pass dependency graph contains a cycle');

    AOrder[LOrderCount] := LCandidate;
    Inc(LOrderCount);
    LEmitted[LCandidate] := 1;
    for I := 0 to Pred(LPassCount) do
      if LEmitted[I] = 0 then
        for J := 0 to High(LRoot.FPasses[I].FPassDependencies) do
          if LRoot.FPasses[I].FPassDependencies[J].PassIndex =
            LCandidate then
          begin
            Dec(LInDegree[I]);
            Break;
          end;
  end;
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

function TGraph.GetRunning: Boolean;
begin
  if Assigned(FPassRoot) then
    Result := FPassRoot.FRunning
  else
    Result := FRunning;
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
var
  LGraph: TGraph;
begin
  LGraph := GetActivePassGraph;
  if LGraph <> Self then
    Exit(LGraph.HasDefinition);
  Result := (Length(FValues) > 0) or (FRuleGroups.Count > 0);
end;

function TGraph.CopyRegisteredValues: TGraphValues;
var
  I: Integer;
  LGraph: TGraph;
begin
  LGraph := GetActivePassGraph;
  if LGraph <> Self then
    Exit(LGraph.CopyRegisteredValues);
  SetLength(Result, Length(FValues));
  for I := 0 to High(FValues) do
    Result[I] := FValues[I];
end;

procedure TGraph.ValidateCurrentEntryDomains(const AOperation: String);
var
  I: Integer;
  LEntry: TGraphEntry;
begin
  for I := 0 to Pred(FEntries.Count) do
  begin
    LEntry := FEntries[I];
    if not LEntry.FHasAllowedValues then
      Continue;
    if LEntry.Empty
      or (not ContainsGraphValue(LEntry.FAllowedValues, LEntry.Value)) then
      raise EInvalidOperation.CreateFmt(
        '%s::entry %d violates its allowed-value domain in pass %d',
        [AOperation, I, FPassIndex]);
  end;
end;

procedure TGraph.ValidateDeniedRuleState(const AOperation: String);
var
  I: Integer;
  LDirectionOrdinal: Integer;
  LGroup: TGraphRuleGroup;
  LRule: TGraphRule;
begin
  for LGroup in FRuleGroups.Values do
  begin
    if not Assigned(LGroup) then
      raise EInvalidOperation.CreateFmt(
        '%s::pass %d contains a nil rule group',
        [AOperation, FPassIndex]);
    for I := 0 to High(LGroup.Rules) do
    begin
      LRule := LGroup.Rules[I];
      LDirectionOrdinal := Ord(LRule.Key);
      if (LDirectionOrdinal < Ord(Low(TGraphDirection)))
        or (LDirectionOrdinal > Ord(High(TGraphDirection))) then
        raise EInvalidOperation.CreateFmt(
          '%s::pass %d contains an invalid rule direction %d',
          [AOperation, FPassIndex, LDirectionOrdinal]);
      if LGroup.Denied[LRule.Key] then
        raise EInvalidOperation.CreateFmt(
          '%s::pass %d value "%s" both denies and defines direction %d',
          [AOperation, FPassIndex, LGroup.Value, LDirectionOrdinal]);
    end;
  end;
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

  procedure TrimValuesForEntryDomain;
  var
    I: Integer;
    LVals: TGraphValues;
  begin
    if not AEntry.FHasAllowedValues then
      Exit;
    LVals := Default(TGraphValues);
    for I := 0 to High(Values) do
      if ContainsGraphValue(AEntry.FAllowedValues, Values[I]) then
        Insert(Values[I], LVals, Length(LVals));
    Values := LVals;
  end;

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
          if LGroup.Denied[ADirection]
            or LGroup.Denied[InverseOfDir(ADirection)] then
            Continue;
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

    if LGroup.Denied[ADirection] then
    begin
      SetLength(Values, 0);
      Exit;
    end;

    //check to see if the neighbor contains rules for the direction it is (relational to this entry)
    if LGroup.Exists[ADirection] then
    begin
      LRule := LGroup.Rule[ADirection];
      LRuleVals := LGroup[ADirection].Value;

      //A present-empty rule retains its historical wildcard meaning.  An
      //explicit DenyAll direction was handled above as separate model state.
      if Length(LRuleVals) > 0 then
      begin
        if TRequireRule(LRule.Info) then
          LHasRequiredConstraint := True;

        //Every active neighbor rule is conjunctive. Starting empty entries
        //from the complete value set lets the first required rule force its
        //values, while intersecting prevents a later rule from reintroducing
        //a candidate rejected by an earlier neighbor.
        for I := 0 to High(Values) do
          if ContainsGraphValue(LRuleVals, Values[I]) then
            Insert(Values[I], LVals, Length(LVals));
        Values := LVals;
      end;
    end;

    //Deny-all is directional source state.  Check the candidate's reverse
    //direction as well as the assigned neighbor above so legacy traversal is
    //independent of which endpoint happened to collapse first.
    LVals := Default(TGraphValues);
    for I := 0 to High(Values) do
      if (not FRuleGroups.ContainsKey(Values[I]))
        or (not FRuleGroups[Values[I]].Denied[
          InverseOfDir(ADirection)]) then
        Insert(Values[I], LVals, Length(LVals));
    Values := LVals;
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

  procedure TrimValuesForPassRequirements;
  var
    LGroup: TGraphRuleGroup;
    LHasPreviousValues: Boolean;
    LPreviousPassIndex: Integer;
    LRequirementIndex: Integer;
    LResolvedIndex: Integer;
    LSourceAllowed: Boolean;
    LSourceEntry: TGraphEntry;
    LSourceGraph: TGraph;
    LVals: TGraphValues;
    I: Integer;
    LAllowed: Boolean;

    function SourceGraph(const ASourcePassIndex: Integer): TGraph;
    begin
      if (ASourcePassIndex < 0)
        or (ASourcePassIndex >= FPassRoot.FPasses.Count) then
        raise EInvalidOperation.CreateFmt(
          'TrimValuesForPassRequirements::invalid dependency %d',
          [ASourcePassIndex]);
      if DependencySlot(ASourcePassIndex) < 0 then
        raise EInvalidOperation.CreateFmt(
          'TrimValuesForPassRequirements::pass %d reads undeclared dependency %d',
          [FPassIndex, ASourcePassIndex]);
      Result := FPassRoot.GetPassGraph(ASourcePassIndex);
      if Result.FEntries.Count <> FEntries.Count then
        raise EInvalidOperation.Create(
          'TrimValuesForPassRequirements::pass dimensions do not match');
    end;

    function RequirementMatches(
      const ARequirementIndex: Integer): Boolean;
    var
      LTermIndex: Integer;
    begin
      Result := False;
      LSourceGraph := SourceGraph(
        LGroup.FPassRequirements[ARequirementIndex].PassIndex);
      for LTermIndex := 0 to High(
        LGroup.FPassRequirements[ARequirementIndex].Terms) do
        if ResolveOffsetIndex(AEntry.Index,
          LGroup.FPassRequirements[ARequirementIndex].Terms[
            LTermIndex].Offset, LResolvedIndex) then
        begin
          LSourceEntry := LSourceGraph.FEntries[LResolvedIndex];
          if (not LSourceEntry.Empty)
            and ContainsGraphValue(
              LGroup.FPassRequirements[ARequirementIndex].Terms[
                LTermIndex].Values, LSourceEntry.Value) then
            Exit(True);
        end;
    end;

    function IsMergedPreviousZero(
      const ARequirementIndex, APreviousPassIndex: Integer): Boolean;
    begin
      Result := (LGroup.FPassRequirements[ARequirementIndex].PassIndex
          = APreviousPassIndex)
        and (LGroup.FPassRequirements[ARequirementIndex].Kind
          = prkMergedOffset)
        and (Length(LGroup.FPassRequirements[ARequirementIndex].Terms)
          = 1)
        and IsZeroGraphOffset(
          LGroup.FPassRequirements[ARequirementIndex].Terms[0].Offset);
    end;
  begin
    if not Assigned(FPassRoot) then
      Exit;
    LVals := Default(TGraphValues);

    for I := 0 to High(Values) do
    begin
      if not FRuleGroups.ContainsKey(Values[I]) then
      begin
        Insert(Values[I], LVals, Length(LVals));
        Continue;
      end;

      LGroup := FRuleGroups[Values[I]];
      LAllowed := True;
      LHasPreviousValues := (FPassIndex > 0)
        and (Length(LGroup.PreviousValues) > 0);
      LPreviousPassIndex := Pred(FPassIndex);

      //PreviousValues predates named pass requirements and may still be
      //populated by a base rule group or an inherited override.  It reads the
      //stable immediate predecessor.  If the same source also has a bound
      //requirement, both accepted-value sets are alternatives rather than two
      //conjunctive filters.
      if LHasPreviousValues then
      begin
        LSourceGraph := SourceGraph(LPreviousPassIndex);
        LSourceEntry := LSourceGraph.FEntries[AEntry.Index];
        LSourceAllowed := (not LSourceEntry.Empty)
          and ContainsGraphValue(LGroup.PreviousValues,
            LSourceEntry.Value);
        for LRequirementIndex := 0 to
          High(LGroup.FPassRequirements) do
          if IsMergedPreviousZero(LRequirementIndex,
            LPreviousPassIndex) then
            LSourceAllowed := LSourceAllowed
              or RequirementMatches(LRequirementIndex);
        LAllowed := LSourceAllowed;
      end;

      for LRequirementIndex := 0 to High(LGroup.FPassRequirements) do
      begin
        if LHasPreviousValues and IsMergedPreviousZero(
          LRequirementIndex, LPreviousPassIndex) then
          Continue;
        if not RequirementMatches(LRequirementIndex) then
        begin
          LAllowed := False;
          Break;
        end;
      end;
      if LAllowed then
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

  TrimValuesForEntryDomain;

  //get the rule group for each of the entry's neighbors on the same plane
  TrimValuesForNeighbor(AEntry[gdNorth], gdNorth);
  TrimValuesForNeighbor(AEntry[gdEast], gdEast);
  TrimValuesForNeighbor(AEntry[gdSouth], gdSouth);
  TrimValuesForNeighbor(AEntry[gdWest], gdWest);
  TrimValuesForNeighbor(AEntry[gdUp], gdUp); //moving top -> bottom
  TrimValuesForNeighbor(AEntry[gdDown], gdDown); //moving bottom -> top
  RemoveUnforcedRequiredValues;
  TrimValuesForPassRequirements;
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

function TGraph.AddValue(const AValue: TGraphValue;
  const AWeight: TGraphWeight): TParentedGraphRuleGroup;
begin
  //Validate before AddValue so a rejected registration cannot create a rule
  //group or change the pass's deterministic value order.
  if AWeight < 1 then
    raise ERangeError.CreateFmt(
      'AddValue::weight must be positive [%d]', [AWeight]);
  Result := AddValue(AValue);
  Result.Weight := AWeight;
end;

function TGraph.SetAllowedValues(const X, Y, Z: TGraphCoordinate;
  const AValues: TGraphValues): TGraph;
var
  I: Integer;
  LCanonical: TGraphValues;
  LEntry: TGraphEntry;
  LGraph: TGraph;
  LRoot: TGraph;
begin
  Result := Self;
  if Assigned(FPassRoot) then
    LRoot := FPassRoot
  else
    LRoot := Self;
  if LRoot.FRunning then
    raise EInvalidOperation.Create(
      'SetAllowedValues::cannot change entry domains while the pipeline is running');

  LGraph := GetActivePassGraph;
  //Validate every caller value before allocating or changing the entry.  The
  //stored order is always the pass's AddValue order, so duplicate or reordered
  //input denotes the same replay model.
  for I := 0 to High(AValues) do
    if not ContainsGraphValue(LGraph.FValues, AValues[I]) then
      raise EArgumentException.CreateFmt(
        'SetAllowedValues::unknown pass value "%s"', [AValues[I]]);

  SetLength(LCanonical, 0);
  for I := 0 to High(LGraph.FValues) do
    if ContainsGraphValue(AValues, LGraph.FValues[I]) then
      Insert(LGraph.FValues[I], LCanonical, Length(LCanonical));

  LEntry := LGraph.GetEntry(X, Y, Z);
  LEntry.FAllowedValues := LCanonical;
  LEntry.FHasAllowedValues := True;
end;

function TGraph.SetAllowedValues(const X, Y, Z: TGraphCoordinate;
  const AValue: TGraphValue): TGraph;
var
  LValues: TGraphValues;
begin
  SetLength(LValues, 1);
  LValues[0] := AValue;
  Result := SetAllowedValues(X, Y, Z, LValues);
end;

function TGraph.ClearAllowedValues(const X, Y,
  Z: TGraphCoordinate): TGraph;
var
  LEntry: TGraphEntry;
  LGraph: TGraph;
  LRoot: TGraph;
begin
  Result := Self;
  if Assigned(FPassRoot) then
    LRoot := FPassRoot
  else
    LRoot := Self;
  if LRoot.FRunning then
    raise EInvalidOperation.Create(
      'ClearAllowedValues::cannot change entry domains while the pipeline is running');
  LGraph := GetActivePassGraph;
  LEntry := LGraph.GetEntry(X, Y, Z);
  SetLength(LEntry.FAllowedValues, 0);
  LEntry.FHasAllowedValues := False;
end;

function TGraph.HasAllowedValues(const X, Y,
  Z: TGraphCoordinate): Boolean;
var
  LGraph: TGraph;
begin
  LGraph := GetActivePassGraph;
  Result := LGraph.GetEntry(X, Y, Z).FHasAllowedValues;
end;

function TGraph.CopyAllowedValues(const X, Y,
  Z: TGraphCoordinate): TGraphValues;
var
  I: Integer;
  LEntry: TGraphEntry;
  LGraph: TGraph;
begin
  Result := Default(TGraphValues);
  LGraph := GetActivePassGraph;
  LEntry := LGraph.GetEntry(X, Y, Z);
  SetLength(Result, Length(LEntry.FAllowedValues));
  for I := 0 to High(Result) do
    Result[I] := LEntry.FAllowedValues[I];
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

function TGraph.DependsOn(const APass: String): TGraph;
var
  LGraph, LRoot: TGraph;
  LPassIndex: Integer;
begin
  Result := Self;
  LGraph := GetActivePassGraph;
  LPassIndex := LGraph.PassIndexForLabel(APass, 'DependsOn');
  if Assigned(LGraph.FPassRoot) then
    LRoot := LGraph.FPassRoot
  else
    LRoot := LGraph;
  LRoot.SynchronizePreviousValueDependencies;
  LGraph.AddDependencyRole(LPassIndex, pdrDeclared);
end;

function TGraph.RemoveDependency(const APass: String): TGraph;
var
  LGraph, LRoot: TGraph;
  LPassIndex, LSlot: Integer;
  LProtected: TPassDependencyRoles;
begin
  Result := Self;
  LGraph := GetActivePassGraph;
  if Assigned(LGraph.FPassRoot) then
    LRoot := LGraph.FPassRoot
  else
    LRoot := LGraph;
  if LRoot.FRunning then
    raise EInvalidOperation.Create(
      'RemoveDependency::cannot change dependencies while the pipeline is running');
  LPassIndex := LGraph.PassIndexForLabel(APass, 'RemoveDependency');
  LRoot.SynchronizePreviousValueDependencies;
  LSlot := LGraph.DependencySlot(LPassIndex);
  if LSlot < 0 then
    Exit;
  LProtected := LGraph.FPassDependencies[LSlot].Roles
    * [pdrLegacy, pdrRequirement, pdrPreviousValues,
      pdrTransformSource];
  if LProtected <> [] then
    raise EInvalidOperation.CreateFmt(
      'RemoveDependency::pass %d dependency %d is required by its configuration',
      [LGraph.FPassIndex, LPassIndex]);
  LGraph.RemoveDependencyRole(LPassIndex, pdrDeclared);
end;

function TGraph.ClearDependencies: TGraph;
var
  I: Integer;
  LGraph, LRoot: TGraph;
begin
  Result := Self;
  LGraph := GetActivePassGraph;
  if Assigned(LGraph.FPassRoot) then
    LRoot := LGraph.FPassRoot
  else
    LRoot := LGraph;
  if LRoot.FRunning then
    raise EInvalidOperation.Create(
      'ClearDependencies::cannot change dependencies while the pipeline is running');
  LRoot.SynchronizePreviousValueDependencies;
  for I := 0 to High(LGraph.FPassDependencies) do
    if (LGraph.FPassDependencies[I].Roles
      * [pdrLegacy, pdrRequirement, pdrPreviousValues,
        pdrTransformSource]) <> [] then
      raise EInvalidOperation.CreateFmt(
        'ClearDependencies::pass %d has dependencies required by its configuration',
        [LGraph.FPassIndex]);
  SetLength(LGraph.FPassDependencies, 0);
end;

function TGraph.TransformFrom(const APass: String): TGraph;
var
  LGraph, LRoot: TGraph;
  LPassIndex, LSlot: Integer;
begin
  Result := Self;
  LGraph := GetActivePassGraph;
  if Assigned(LGraph.FPassRoot) then
    LRoot := LGraph.FPassRoot
  else
    LRoot := LGraph;
  if LRoot.FRunning then
    raise EInvalidOperation.Create(
      'TransformFrom::cannot change pass mode while the pipeline is running');
  LPassIndex := LGraph.PassIndexForLabel(APass, 'TransformFrom');
  LRoot.SynchronizePreviousValueDependencies;
  if LPassIndex = LGraph.FPassIndex then
    raise EInvalidOperation.CreateFmt(
      'TransformFrom::pass %d cannot source itself', [LPassIndex]);
  if (LGraph.DependencySlot(LPassIndex) < 0)
    and LRoot.WouldCreateDependencyCycle(LGraph.FPassIndex,
      LPassIndex) then
    raise EInvalidOperation.CreateFmt(
      'TransformFrom::dependency %d -> %d would create a cycle',
      [LGraph.FPassIndex, LPassIndex]);
  if (LGraph.FPassMode = gpmTransform)
    and (LGraph.FTransformSourceIndex = LPassIndex) then
    Exit;

  if (LGraph.FPassMode = gpmLegacy) and (LGraph.FPassIndex > 0) then
  begin
    LSlot := LGraph.DependencySlot(Pred(LGraph.FPassIndex));
    if LSlot >= 0 then
    begin
      Exclude(LGraph.FPassDependencies[LSlot].Roles, pdrLegacy);
      Include(LGraph.FPassDependencies[LSlot].Roles, pdrDeclared);
    end;
  end;
  if (LGraph.FPassMode = gpmTransform)
    and (LGraph.FTransformSourceIndex >= 0) then
    LGraph.RemoveDependencyRole(LGraph.FTransformSourceIndex,
      pdrTransformSource);
  LGraph.AddDependencyRole(LPassIndex, pdrTransformSource);
  LGraph.FTransformSourceIndex := LPassIndex;
  LGraph.FPassMode := gpmTransform;
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

function TGraph.TrySolve(const AOptions: TGraphSolveOptions;
  out AReport: TGraphSolveReport): Boolean;
var
  I: Integer;
  LDirty: TPassSelection;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.TrySolve(AOptions, AReport));
  if AOptions.MaxBacktracks < 0 then
    raise ERangeError.CreateFmt(
      'TrySolve::maximum backtracks cannot be negative [%d]',
      [AOptions.MaxBacktracks]);
  EnsureInitialPass;
  SetLength(LDirty, FPasses.Count);
  for I := 0 to High(LDirty) do
    LDirty[I] := 1;
  Result := TrySolveInternal(AOptions, LDirty, AReport);
end;

function TGraph.TryRegenerateFrom(const APass: String;
  const AOptions: TGraphSolveOptions;
  out AReport: TGraphSolveReport): Boolean;
var
  LPasses: TGraphPassLabels;
begin
  SetLength(LPasses, 1);
  LPasses[0] := APass;
  Result := TryRegenerateFrom(LPasses, AOptions, AReport);
end;

function TGraph.TryRegenerateFrom(const APasses: TGraphPassLabels;
  const AOptions: TGraphSolveOptions;
  out AReport: TGraphSolveReport): Boolean;
var
  I, J: Integer;
  LChanged: Boolean;
  LDirty: TPassSelection;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.TryRegenerateFrom(APasses, AOptions, AReport));
  if AOptions.MaxBacktracks < 0 then
    raise ERangeError.CreateFmt(
      'TryRegenerateFrom::maximum backtracks cannot be negative [%d]',
      [AOptions.MaxBacktracks]);
  if Length(APasses) = 0 then
    raise EArgumentException.Create(
      'TryRegenerateFrom::at least one pass is required');
  EnsureInitialPass;
  SetLength(LDirty, FPasses.Count);
  for I := 0 to High(APasses) do
    LDirty[PassIndexForLabel(APasses[I], 'TryRegenerateFrom')] := 1;
  SynchronizePreviousValueDependencies;

  //A changed provider conservatively invalidates every transitive consumer.
  //The dependency list is small, stable, and index-sorted, so a fixed-point
  //scan is both portable and deterministic.
  repeat
    LChanged := False;
    for I := 0 to Pred(FPasses.Count) do
      if LDirty[I] = 0 then
        for J := 0 to High(FPasses[I].FPassDependencies) do
          if LDirty[FPasses[I].FPassDependencies[J].PassIndex] <> 0 then
          begin
            LDirty[I] := 1;
            LChanged := True;
            Break;
          end;
  until not LChanged;
  Result := TrySolveInternal(AOptions, LDirty, AReport);
end;

function TGraph.TrySolveInternal(const AOptions: TGraphSolveOptions;
  const ADirty: TPassSelection;
  out AReport: TGraphSolveReport): Boolean;
type
  TGraphValueMatrix = array of TGraphValues;
  TGraphTraceCauseArray = array of TGraphTraceCauseKind;
  TRandomStateArray = array of TRandomState;
  TEntryState = record
    Value: TGraphValue;
    Empty: Boolean;
    Generated: Boolean;
  end;
  TEntryStates = array of TEntryState;
  TPassEntryStates = array of TEntryStates;
var
  LAssignment: TReferenceIntegerArray;
  LCommitted: Boolean;
  LDefinitionlessFailureFromSource: Boolean;
  LDefinitionlessSourcePass: Integer;
  LEntryIndex: Integer;
  LExecutionOrdinal: Integer;
  LExecutionPlan: TGraphPassIndices;
  LFullExecutionPlan: TGraphPassIndices;
  LGraph: TGraph;
  LInitialTraceCauses: TGraphTraceCauseArray;
  LInitialTraceDependencyPasses: TReferenceIntegerArray;
  LInvalidLockEntry: Integer;
  LModel: TReferenceModel;
  LPassBeginEvent: Integer;
  LPassIndex: Integer;
  LRandomStates: TRandomStateArray;
  LReferenceReport: TReferenceSolveReport;
  LRootRandomState: TRandomState;
  LSavedPassIndex: Integer;
  LSnapshots: TPassEntryStates;
  LStaged: TGraphValueMatrix;
  LTraceCauseEventId: Integer;
  LTraceCount: Integer;
  LTraceEvent: TGraphTraceEvent;
  LTraceHash: TGraphTraceSignature;
  LRequirementFailureNamed: TReferenceByteArray;
  LRequirementFailurePass: TReferenceIntegerArray;

  function CheckedProduct(const A, B: Integer;
    const ALabel: String): Integer;
  begin
    if (A < 0) or (B < 0) then
      raise ERangeError.Create(ALabel + ' cannot be negative');
    if (A <> 0) and (B > High(Integer) div A) then
      raise ERangeError.Create(ALabel + ' is too large');
    Result := A * B;
  end;

  procedure TraceHashCardinal(const AValue: Cardinal);
  begin
    GraphTraceHashCardinal(LTraceHash, AValue);
  end;

  procedure TraceHashText(const AValue: String);
  begin
    GraphTraceHashText(LTraceHash, AValue);
  end;

  procedure HashTraceEvent(const AEvent: TGraphTraceEvent);
  begin
    MixGraphTraceEvent(LTraceHash, AEvent);
  end;

  function NewTraceEvent(const AKind: TGraphTraceEventKind;
    const ACauseKind: TGraphTraceCauseKind;
    const APassIndex: Integer): TGraphTraceEvent;
  begin
    Result := Default(TGraphTraceEvent);
    Result.EventId := -1;
    Result.CauseEventId := -1;
    Result.Kind := AKind;
    Result.CauseKind := ACauseKind;
    Result.PassIndex := APassIndex;
    Result.EntryIndex := -1;
    Result.ValueIndex := -1;
    Result.Value := TGraphValue.Empty;
    Result.NeighborIndex := -1;
    Result.HasDirection := False;
    Result.Direction := gdNorth;
    Result.DependencyPassIndex := -1;
    Result.DecisionDepth := 0;
    Result.DomainCountBefore := 0;
    Result.DomainCountAfter := 0;
  end;

  function AppendTraceEvent(
    const ASource: TGraphTraceEvent): Integer;
  var
    LCapacity: Integer;
    LEvent: TGraphTraceEvent;
    LGraphForEvent: TGraph;
    LIndex: Integer;
  begin
    Result := -1;
    if not AOptions.CaptureTrace then
      Exit;

    LEvent := ASource;
    if LTraceCount = High(Integer) then
      raise ERangeError.Create('TrySolve::trace is too large');
    if LTraceCount = Length(AReport.Trace) then
    begin
      LCapacity := Length(AReport.Trace);
      if LCapacity < 64 then
        LCapacity := 64
      else if LCapacity > High(Integer) div 2 then
        LCapacity := High(Integer)
      else
        LCapacity := LCapacity * 2;
      SetLength(AReport.Trace, LCapacity);
    end;
    LIndex := LTraceCount;
    LEvent.EventId := LIndex;
    if (LEvent.PassIndex >= 0)
      and (LEvent.PassIndex < FPasses.Count) then
    begin
      LGraphForEvent := FPasses[LEvent.PassIndex];
      if (LEvent.ValueIndex >= 0)
        and (LEvent.ValueIndex < Length(LGraphForEvent.FValues)) then
        LEvent.Value := LGraphForEvent.FValues[LEvent.ValueIndex]
      else
        LEvent.Value := TGraphValue.Empty;
      if AReport.Passes[LEvent.PassIndex].TraceCount = 0 then
        AReport.Passes[LEvent.PassIndex].TraceStart := LIndex;
      Inc(AReport.Passes[LEvent.PassIndex].TraceCount);
    end
    else
      LEvent.Value := TGraphValue.Empty;

    AReport.Trace[LIndex] := LEvent;
    Inc(LTraceCount);
    HashTraceEvent(LEvent);
    AReport.TraceHash := LTraceHash;
    Result := LIndex;
  end;

  procedure ReserveTerminalTraceSlot;
  begin
    if not AOptions.CaptureTrace then
      Exit;
    if LTraceCount = High(Integer) then
      raise ERangeError.Create('TrySolve::trace is too large');
    //Resize before live entry mutation. The terminal append then overwrites
    //this initialized spare slot and cannot trigger a post-commit allocation.
    SetLength(AReport.Trace, Succ(LTraceCount));
  end;

  procedure InitializeReport;
  var
    I: Integer;
  begin
    AReport.Status := gssContradiction;
    AReport.Seed := Seed;
    AReport.RandomAlgorithmVersion := WFC_RANDOM_ALGORITHM_VERSION;
    AReport.SolverAlgorithmVersion := WFC_SOLVER_ALGORITHM_VERSION;
    AReport.GraphModelVersion := WFC_GRAPH_MODEL_VERSION;
    AReport.PipelineAlgorithmVersion := WFC_PIPELINE_ALGORITHM_VERSION;
    AReport.FailedPassIndex := -1;
    AReport.Contradiction.Kind := gckNone;
    AReport.Contradiction.PassIndex := -1;
    AReport.Contradiction.EntryIndex := -1;
    AReport.Contradiction.NeighborIndex := -1;
    AReport.Contradiction.HasDirection := False;
    AReport.Contradiction.Direction := gdNorth;
    AReport.Contradiction.DependencyPassIndex := -1;
    SetLength(AReport.ExecutionOrder, 0);
    SetLength(AReport.Passes, FPasses.Count);
    AReport.TraceCaptured := AOptions.CaptureTrace;
    AReport.TraceHash := 0;
    SetLength(AReport.Trace, 0);
    LTraceCount := 0;
    for I := 0 to High(AReport.Passes) do
    begin
      AReport.Passes[I].Decisions := 0;
      AReport.Passes[I].Propagations := 0;
      AReport.Passes[I].Contradictions := 0;
      AReport.Passes[I].Backtracks := 0;
      AReport.Passes[I].Executed := False;
      AReport.Passes[I].ExecutionOrdinal := -1;
      AReport.Passes[I].Disposition := gpdNotRun;
      AReport.Passes[I].TraceStart := -1;
      AReport.Passes[I].TraceCount := 0;
    end;
    LTraceHash := 0;
    if AOptions.CaptureTrace then
    begin
      LTraceHash := Cardinal(2166136261);
      TraceHashText('wfc-graph-trace');
      TraceHashCardinal(WFC_TRACE_VERSION);
      TraceHashCardinal(WFC_TRACE_HASH_VERSION);
      TraceHashCardinal(AReport.Seed);
      TraceHashCardinal(Cardinal(AReport.RandomAlgorithmVersion));
      TraceHashCardinal(Cardinal(AReport.SolverAlgorithmVersion));
      TraceHashCardinal(Cardinal(AReport.GraphModelVersion));
      TraceHashCardinal(Cardinal(AReport.PipelineAlgorithmVersion));
      TraceHashCardinal(Cardinal(Length(AReport.Passes)));
      AReport.TraceHash := LTraceHash;
    end;
  end;

  function FindValueIndex(const AGraph: TGraph;
    const AValue: TGraphValue): Integer;
  var
    I: Integer;
  begin
    for I := 0 to High(AGraph.FValues) do
      if AGraph.FValues[I] = AValue then
        Exit(I);
    Result := -1;
  end;

  procedure ValidateDefinedModel(const AGraph: TGraph);
  var
    I, J, K: Integer;
    LDirectionOrdinal: Integer;
    LGroup: TGraphRuleGroup;
    LRule: TGraphRule;
    LSeenDirections: TGraphDirections;
  begin
    if AGraph.FRuleGroups.Count <> Length(AGraph.FValues) then
      raise EInvalidOperation.CreateFmt(
        'TrySolve::pass %d has an inconsistent value registry',
        [AGraph.FPassIndex]);

    for I := 0 to High(AGraph.FValues) do
    begin
      if AGraph.FValues[I] = TGraphValue.Empty then
        raise EInvalidOperation.CreateFmt(
          'TrySolve::pass %d contains the reserved empty value',
          [AGraph.FPassIndex]);
      if not AGraph.FRuleGroups.TryGetValue(AGraph.FValues[I], LGroup) then
        raise EInvalidOperation.CreateFmt(
          'TrySolve::pass %d has no rule group for value "%s"',
          [AGraph.FPassIndex, AGraph.FValues[I]]);

      if not Assigned(LGroup) then
        raise EInvalidOperation.CreateFmt(
          'TrySolve::pass %d has a nil rule group for value "%s"',
          [AGraph.FPassIndex, AGraph.FValues[I]]);
      if LGroup.Value <> AGraph.FValues[I] then
        raise EInvalidOperation.CreateFmt(
          'TrySolve::pass %d rule-group identity "%s" does not match value "%s"',
          [AGraph.FPassIndex, LGroup.Value, AGraph.FValues[I]]);
      if LGroup.Weight < 1 then
        raise EInvalidOperation.CreateFmt(
          'TrySolve::pass %d value "%s" has invalid weight %d',
          [AGraph.FPassIndex, AGraph.FValues[I], LGroup.Weight]);
      LSeenDirections := [];
      for J := 0 to High(LGroup.Rules) do
      begin
        LRule := LGroup.Rules[J];
        LDirectionOrdinal := Ord(LRule.Key);
        if (LDirectionOrdinal < Ord(Low(TGraphDirection)))
          or (LDirectionOrdinal > Ord(High(TGraphDirection))) then
          raise EInvalidOperation.CreateFmt(
            'TrySolve::pass %d rule "%s" has invalid direction %d',
            [AGraph.FPassIndex, AGraph.FValues[I], LDirectionOrdinal]);
        if LRule.Key in LSeenDirections then
          raise EInvalidOperation.CreateFmt(
            'TrySolve::pass %d rule "%s" repeats direction %d',
            [AGraph.FPassIndex, AGraph.FValues[I], LDirectionOrdinal]);
        if LGroup.Denied[LRule.Key] then
          raise EInvalidOperation.CreateFmt(
            'TrySolve::pass %d value "%s" both denies and defines direction %d',
            [AGraph.FPassIndex, AGraph.FValues[I], LDirectionOrdinal]);
        Include(LSeenDirections, LRule.Key);
        for K := 0 to High(LRule.Value) do
          if FindValueIndex(AGraph, LRule.Value[K]) < 0 then
            raise EInvalidOperation.CreateFmt(
              'TrySolve::pass %d rule "%s" references unknown value "%s"',
              [AGraph.FPassIndex, AGraph.FValues[I],
               LRule.Value[K]]);
      end;
    end;
  end;

  function RuleAllows(const AGraph: TGraph; const ASourceValue: Integer;
    const ADirection: TGraphDirection; const ATargetValue: Integer;
    out ARequiredEdge: Boolean): Boolean;
  var
    LGroup: TGraphRuleGroup;
    LRule: TGraphRule;
  begin
    ARequiredEdge := False;
    LGroup := AGraph.FRuleGroups[AGraph.FValues[ASourceValue]];
    if LGroup.Denied[ADirection] then
      Exit(False);
    if not LGroup.Exists[ADirection] then
      Exit(True);
    LRule := LGroup.Rule[ADirection];
    if Length(LRule.Value) = 0 then
      Exit(True);
    Result := ContainsGraphValue(LRule.Value,
      AGraph.FValues[ATargetValue]);
    ARequiredEdge := Result and TRequireRule(LRule.Info);
  end;

  function RelationIndex(const AValueCount: Integer;
    const ADirection: TGraphDirection; const ACurrentValue,
    ANeighborValue: Integer): Integer;
  begin
    Result := ((Ord(ADirection) * AValueCount + ACurrentValue)
      * AValueCount) + ANeighborValue;
  end;

  procedure BuildCellOrder(const AGraph: TGraph;
    out AOrder: TReferenceIntegerArray);
  var
    I: Integer;
    LOrderIndex: Integer;
    LPlaneSize: Integer;
    LZ: Integer;
  begin
    SetLength(AOrder, AGraph.FEntries.Count);
    if Length(AOrder) = 0 then
      Exit;
    LOrderIndex := 0;
    LPlaneSize := CheckedProduct(Integer(AGraph.FDimension.Width),
      Integer(AGraph.FDimension.Height), 'TrySolve::plane size');
    if AGraph.FMode = rmBottomUp then
      for LZ := 0 to Integer(AGraph.FDimension.Depth) - 1 do
        for I := 0 to Pred(LPlaneSize) do
        begin
          AOrder[LOrderIndex] := (LZ * LPlaneSize) + I;
          Inc(LOrderIndex);
        end
    else if AGraph.FMode = rmTopDown then
      for LZ := Integer(AGraph.FDimension.Depth) - 1 downto 0 do
        for I := 0 to Pred(LPlaneSize) do
        begin
          AOrder[LOrderIndex] := (LZ * LPlaneSize) + I;
          Inc(LOrderIndex);
        end
    else
      raise EInvalidOperation.Create(
        'TrySolve::run mode is not implemented');
  end;

  function BuildReferenceModel(const AGraph: TGraph;
    const AStaged: TGraphValueMatrix; out AModel: TReferenceModel;
    out AInvalidLockEntry: Integer;
    out ARequirementFailurePass: TReferenceIntegerArray;
    out ARequirementFailureNamed: TReferenceByteArray;
    out AInitialTraceCauses: TGraphTraceCauseArray;
    out AInitialTraceDependencyPasses: TReferenceIntegerArray): Boolean;
  var
    LAllowed: Boolean;
    LAllowedForward: Boolean;
    LAllowedReverse: Boolean;
    LAllowedCount: Integer;
    LCell: Integer;
    LCellValueCount: Integer;
    LDirection: TGraphDirection;
    LDirectRequired: Boolean;
    LGroup: TGraphRuleGroup;
    LFailureNamed: Boolean;
    LFailurePass: Integer;
    LHasPreviousValues: Boolean;
    LIndex: Integer;
    LLockValue: Integer;
    LNeighbor: TGraphEntry;
    LNeighborIndex: Integer;
    LNeighborValue: Integer;
    LRelationCount: Integer;
    LRequirementIndex: Integer;
    LRequirementsAllowed: Boolean;
    LSourceAllowed: Boolean;
    LSourceNamed: Boolean;
    LReverseRequired: Boolean;
    LSourcePassIndex: Integer;
    LSourceValue: TGraphValue;
    LTraceIndex: Integer;
    LValue: Integer;
    LValueFailurePass: Integer;

    function RequirementMatches(
      const ARequirementIndex: Integer): Boolean;
    var
      LResolvedIndex, LTermIndex: Integer;
      LTermSourcePass: Integer;
      LTermSourceValue: TGraphValue;
    begin
      Result := False;
      LTermSourcePass :=
        LGroup.FPassRequirements[ARequirementIndex].PassIndex;
      for LTermIndex := 0 to High(
        LGroup.FPassRequirements[ARequirementIndex].Terms) do
        if AGraph.ResolveOffsetIndex(LCell,
          LGroup.FPassRequirements[ARequirementIndex].Terms[
            LTermIndex].Offset, LResolvedIndex) then
        begin
          LTermSourceValue := AStaged[LTermSourcePass][LResolvedIndex];
          if (LTermSourceValue <> TGraphValue.Empty)
            and ContainsGraphValue(
              LGroup.FPassRequirements[ARequirementIndex].Terms[
                LTermIndex].Values, LTermSourceValue) then
            Exit(True);
        end;
    end;

    function IsMergedPreviousZero(
      const ARequirementIndex, APreviousPassIndex: Integer): Boolean;
    begin
      Result := (LGroup.FPassRequirements[ARequirementIndex].PassIndex
          = APreviousPassIndex)
        and (LGroup.FPassRequirements[ARequirementIndex].Kind
          = prkMergedOffset)
        and (Length(LGroup.FPassRequirements[ARequirementIndex].Terms)
          = 1)
        and IsZeroGraphOffset(
          LGroup.FPassRequirements[ARequirementIndex].Terms[0].Offset);
    end;

    procedure RecordRequirementFailure(const APassIndex: Integer;
      const ANamed: Boolean);
    begin
      //The public terminal contradiction retains the earliest provider pass
      //across the whole cell, while tracing also retains that cause for each
      //individual candidate removed from the initial domain.
      if (LFailurePass < 0) or (APassIndex < LFailurePass) then
      begin
        LFailurePass := APassIndex;
        LFailureNamed := ANamed;
      end
      else if (APassIndex = LFailurePass) and ANamed then
        LFailureNamed := True;

      if (LValueFailurePass < 0)
        or (APassIndex < LValueFailurePass) then
        LValueFailurePass := APassIndex;
    end;
  begin
    Result := False;
    AInvalidLockEntry := -1;
    AModel := Default(TReferenceModel);
    ValidateDefinedModel(AGraph);

    AModel.CellCount := AGraph.FEntries.Count;
    AModel.ValueCount := Length(AGraph.FValues);
    LCellValueCount := CheckedProduct(AModel.CellCount,
      AModel.ValueCount, 'TrySolve::domain matrix');
    LRelationCount := CheckedProduct(
      CheckedProduct(AModel.ValueCount, AModel.ValueCount,
        'TrySolve::relation matrix'),
      WFC_REFERENCE_DIRECTION_COUNT, 'TrySolve::relation matrix');

    SetLength(AModel.Neighbors, CheckedProduct(AModel.CellCount,
      WFC_REFERENCE_DIRECTION_COUNT, 'TrySolve::neighbor matrix'));
    SetLength(AModel.Compatibility, LRelationCount);
    SetLength(AModel.ValueWeights, AModel.ValueCount);
    SetLength(AModel.RequiredValues, AModel.ValueCount);
    SetLength(AModel.RequiredSupport, LRelationCount);
    SetLength(AModel.InitialAllowed, LCellValueCount);
    SetLength(AModel.InitialFailureKinds, AModel.CellCount);
    SetLength(AModel.LockedValues, AModel.CellCount);
    SetLength(ARequirementFailurePass, AModel.CellCount);
    SetLength(ARequirementFailureNamed, AModel.CellCount);
    if AOptions.CaptureTrace then
    begin
      SetLength(AInitialTraceCauses, LCellValueCount);
      SetLength(AInitialTraceDependencyPasses, LCellValueCount);
      for LIndex := 0 to High(AInitialTraceDependencyPasses) do
        AInitialTraceDependencyPasses[LIndex] := -1;
    end
    else
    begin
      SetLength(AInitialTraceCauses, 0);
      SetLength(AInitialTraceDependencyPasses, 0);
    end;
    BuildCellOrder(AGraph, AModel.CellOrder);

    for LCell := 0 to Pred(AModel.CellCount) do
    begin
      AModel.LockedValues[LCell] := -1;
      AModel.InitialFailureKinds[LCell] := rckEmptyDomain;
      ARequirementFailurePass[LCell] := -1;
      for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
      begin
        LNeighbor := AGraph.FEntries[LCell][LDirection];
        LIndex := (LCell * WFC_REFERENCE_DIRECTION_COUNT)
          + Ord(LDirection);
        if not Assigned(LNeighbor) then
          AModel.Neighbors[LIndex] := -1
        else
        begin
          LNeighborIndex := LNeighbor.Index;
          if (LNeighborIndex < 0)
            or (LNeighborIndex >= AGraph.FEntries.Count)
            or (AGraph.FEntries[LNeighborIndex] <> LNeighbor) then
            raise EInvalidOperation.CreateFmt(
              'TrySolve::pass %d entry %d has an external neighbor',
              [AGraph.FPassIndex, LCell]);
          AModel.Neighbors[LIndex] := LNeighborIndex;
        end;
      end;
    end;

    for LValue := 0 to Pred(AModel.ValueCount) do
    begin
      LGroup := AGraph.FRuleGroups[AGraph.FValues[LValue]];
      AModel.ValueWeights[LValue] := LGroup.Weight;
      if LGroup.HasRequired then
        AModel.RequiredValues[LValue] := 1;
      if (AGraph.FPassIndex > 0)
        and (Length(LGroup.PreviousValues) > 0) then
      begin
        LSourcePassIndex := Pred(AGraph.FPassIndex);
        if AGraph.DependencySlot(LSourcePassIndex) < 0 then
          raise EInvalidOperation.CreateFmt(
            'TrySolve::pass %d value "%s" reads undeclared dependency %d',
            [AGraph.FPassIndex, AGraph.FValues[LValue],
              LSourcePassIndex]);
        if Length(AStaged[LSourcePassIndex]) <> AModel.CellCount then
          raise EInvalidOperation.CreateFmt(
            'TrySolve::dependency %d for pass %d has a different shape',
            [LSourcePassIndex, AGraph.FPassIndex]);
      end;
      for LRequirementIndex := 0 to High(LGroup.FPassRequirements) do
      begin
        LSourcePassIndex :=
          LGroup.FPassRequirements[LRequirementIndex].PassIndex;
        if (LSourcePassIndex < 0)
          or (LSourcePassIndex >= Length(AStaged)) then
          raise EInvalidOperation.CreateFmt(
            'TrySolve::pass %d value "%s" has invalid dependency %d',
            [AGraph.FPassIndex, AGraph.FValues[LValue],
              LSourcePassIndex]);
        if AGraph.DependencySlot(LSourcePassIndex) < 0 then
          raise EInvalidOperation.CreateFmt(
            'TrySolve::pass %d value "%s" reads undeclared dependency %d',
            [AGraph.FPassIndex, AGraph.FValues[LValue],
              LSourcePassIndex]);
        if Length(AStaged[LSourcePassIndex]) <> AModel.CellCount then
          raise EInvalidOperation.CreateFmt(
            'TrySolve::dependency %d for pass %d has a different shape',
            [LSourcePassIndex, AGraph.FPassIndex]);
      end;
    end;

    //Compile a two-sided compatibility relation. The legacy rule builder
    //creates inverse edges, while the conjunction also protects the reference
    //solver from assignment-order dependence after direct rule-array edits.
    for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
      for LValue := 0 to Pred(AModel.ValueCount) do
        for LNeighborValue := 0 to Pred(AModel.ValueCount) do
        begin
          LAllowedForward := RuleAllows(AGraph, LNeighborValue,
            LDirection, LValue, LDirectRequired);
          LAllowedReverse := RuleAllows(AGraph, LValue,
            InverseOfDir(LDirection), LNeighborValue,
            LReverseRequired);
          LIndex := RelationIndex(AModel.ValueCount, LDirection,
            LValue, LNeighborValue);
          if LAllowedForward and LAllowedReverse then
            AModel.Compatibility[LIndex] := 1;
          if LAllowedForward and LAllowedReverse and LDirectRequired then
            AModel.RequiredSupport[LIndex] := 1;
        end;

    for LCell := 0 to Pred(AModel.CellCount) do
    begin
      LLockValue := -1;
      if (not AGraph.FEntries[LCell].Empty)
        and (not AGraph.FEntries[LCell].Generated) then
      begin
        LLockValue := FindValueIndex(AGraph,
          AGraph.FEntries[LCell].Value);
        if LLockValue < 0 then
        begin
          AInvalidLockEntry := LCell;
          Exit(False);
        end;
        AModel.LockedValues[LCell] := LLockValue;
        //Legacy locked entries bypass required-trigger eligibility, but still
        //have to satisfy adjacency and previous-pass constraints.
      end;

      if AGraph.FEntries[LCell].FHasAllowedValues
        and ((Length(AGraph.FEntries[LCell].FAllowedValues) = 0)
          or ((LLockValue >= 0)
            and (not ContainsGraphValue(
              AGraph.FEntries[LCell].FAllowedValues,
              AGraph.FValues[LLockValue])))) then
        AModel.InitialFailureKinds[LCell] := rckEntryDomain;

      LAllowedCount := 0;
      LFailurePass := -1;
      LFailureNamed := False;
      for LValue := 0 to Pred(AModel.ValueCount) do
      begin
        LTraceIndex := (LCell * AModel.ValueCount) + LValue;
        LValueFailurePass := -1;
        LAllowed := (LLockValue < 0) or (LLockValue = LValue);
        if AOptions.CaptureTrace and (not LAllowed) then
          AInitialTraceCauses[LTraceIndex] := gtckCallerLock;
        if LAllowed and AGraph.FEntries[LCell].FHasAllowedValues then
        begin
          LAllowed := ContainsGraphValue(
            AGraph.FEntries[LCell].FAllowedValues,
            AGraph.FValues[LValue]);
          if AOptions.CaptureTrace and (not LAllowed) then
            AInitialTraceCauses[LTraceIndex] := gtckCallerDomain;
        end;
        LGroup := AGraph.FRuleGroups[AGraph.FValues[LValue]];
        if LAllowed then
        begin
          LRequirementsAllowed := True;
          LHasPreviousValues := (AGraph.FPassIndex > 0)
            and (Length(LGroup.PreviousValues) > 0);

          if LHasPreviousValues then
          begin
            LSourcePassIndex := Pred(AGraph.FPassIndex);
            LSourceValue := AStaged[LSourcePassIndex][LCell];
            LSourceAllowed := (LSourceValue <> TGraphValue.Empty)
              and ContainsGraphValue(LGroup.PreviousValues,
                LSourceValue);
            LSourceNamed := False;
            for LRequirementIndex := 0 to
              High(LGroup.FPassRequirements) do
              if IsMergedPreviousZero(LRequirementIndex,
                LSourcePassIndex) then
              begin
                if proNamed in
                  LGroup.FPassRequirements[LRequirementIndex].Origins then
                  LSourceNamed := True;
                LSourceAllowed := LSourceAllowed
                  or RequirementMatches(LRequirementIndex);
              end;
            if not LSourceAllowed then
            begin
              LRequirementsAllowed := False;
              RecordRequirementFailure(LSourcePassIndex, LSourceNamed);
            end;
          end;

          for LRequirementIndex := 0 to
            High(LGroup.FPassRequirements) do
          begin
            LSourcePassIndex :=
              LGroup.FPassRequirements[LRequirementIndex].PassIndex;
            if LHasPreviousValues and IsMergedPreviousZero(
              LRequirementIndex, Pred(AGraph.FPassIndex)) then
              Continue;
            if not RequirementMatches(LRequirementIndex) then
            begin
              LRequirementsAllowed := False;
              RecordRequirementFailure(LSourcePassIndex,
                proNamed in
                  LGroup.FPassRequirements[LRequirementIndex].Origins);
            end;
          end;
          LAllowed := LRequirementsAllowed;
          if AOptions.CaptureTrace and (not LAllowed) then
          begin
            AInitialTraceCauses[LTraceIndex] := gtckPassDependency;
            AInitialTraceDependencyPasses[LTraceIndex] :=
              LValueFailurePass;
          end;
        end;
        if LAllowed then
        begin
          AModel.InitialAllowed[
            (LCell * AModel.ValueCount) + LValue] := 1;
          Inc(LAllowedCount);
        end;
      end;
      if (LAllowedCount = 0) and (LFailurePass >= 0)
        and (AModel.InitialFailureKinds[LCell] <> rckEntryDomain) then
      begin
        AModel.InitialFailureKinds[LCell] := rckPreviousPass;
        ARequirementFailurePass[LCell] := LFailurePass;
        if LFailureNamed then
          ARequirementFailureNamed[LCell] := 1;
      end;
    end;
    Result := True;
  end;

  function PublicContradictionKind(
    const AKind: TReferenceContradictionKind): TGraphContradictionKind;
  begin
    case AKind of
      rckEmptyDomain:
        Result := gckEmptyDomain;
      rckEntryDomain:
        Result := gckEntryDomain;
      rckAdjacency:
        Result := gckAdjacency;
      rckPreviousPass:
        Result := gckPreviousPass;
      rckRequiredSupport:
        Result := gckRequiredSupport;
      rckFinalValidation:
        Result := gckFinalValidation;
    else
      Result := gckNone;
    end;
  end;

  function PublicTraceEventKind(
    const AKind: TReferenceTraceEventKind): TGraphTraceEventKind;
  begin
    case AKind of
      rtekInitialCandidateRemoved:
        Result := gtekInitialCandidateRemoved;
      rtekDecision:
        Result := gtekDecision;
      rtekCandidateRemoved:
        Result := gtekCandidateRemoved;
      rtekContradiction:
        Result := gtekContradiction;
      rtekBacktrack:
        Result := gtekBacktrack;
      rtekCandidateRestored:
        Result := gtekCandidateRestored;
      rtekSolved:
        Result := gtekPassStaged;
    else
      raise ERangeError.Create('TrySolve::invalid reference trace event');
    end;
  end;

  function PublicTraceCauseKind(
    const AKind: TReferenceTraceCauseKind): TGraphTraceCauseKind;
  begin
    case AKind of
      rtckNone:
        Result := gtckNone;
      rtckInitialDomain:
        Result := gtckCallerDomain;
      rtckLock:
        Result := gtckCallerLock;
      rtckDecision:
        Result := gtckDecision;
      rtckAdjacency:
        Result := gtckAdjacency;
      rtckRequiredSupport:
        Result := gtckRequiredSupport;
      rtckBacktrack:
        Result := gtckBacktrack;
      rtckFinalValidation:
        Result := gtckFinalValidation;
    else
      raise ERangeError.Create('TrySolve::invalid reference trace cause');
    end;
  end;

  function LastPassTraceEvent(const APassIndex: Integer): Integer;
  begin
    Result := -1;
    if (APassIndex < 0) or (APassIndex >= Length(AReport.Passes))
      or (AReport.Passes[APassIndex].TraceCount = 0) then
      Exit;
    Result := AReport.Passes[APassIndex].TraceStart
      + Pred(AReport.Passes[APassIndex].TraceCount);
  end;

  procedure MapReferenceTrace(const APassIndex, APassBeginEvent: Integer;
    const AGraph: TGraph; const AReference: TReferenceSolveReport;
    const AInitialCauses: TGraphTraceCauseArray;
    const AInitialDependencyPasses: TReferenceIntegerArray);
  var
    I: Integer;
    LInitialIndex: Integer;
    LLocalToGlobal: TReferenceIntegerArray;
    LReferenceEvent: TReferenceTraceEvent;
    LTraceEvent: TGraphTraceEvent;
  begin
    if not AOptions.CaptureTrace then
      Exit;
    SetLength(LLocalToGlobal, Length(AReference.Trace));
    for I := 0 to High(LLocalToGlobal) do
      LLocalToGlobal[I] := -1;

    for I := 0 to High(AReference.Trace) do
    begin
      LReferenceEvent := AReference.Trace[I];
      LTraceEvent := NewTraceEvent(
        PublicTraceEventKind(LReferenceEvent.Kind),
        PublicTraceCauseKind(LReferenceEvent.CauseKind), APassIndex);
      if (LReferenceEvent.CauseEventId >= 0)
        and (LReferenceEvent.CauseEventId < I) then
        LTraceEvent.CauseEventId :=
          LLocalToGlobal[LReferenceEvent.CauseEventId]
      else
        LTraceEvent.CauseEventId := -1;
      LTraceEvent.EntryIndex := LReferenceEvent.EntryIndex;
      LTraceEvent.ValueIndex := LReferenceEvent.ValueIndex;
      LTraceEvent.NeighborIndex := LReferenceEvent.NeighborIndex;
      if (LReferenceEvent.Direction >= Ord(Low(TGraphDirection)))
        and (LReferenceEvent.Direction <= Ord(High(TGraphDirection))) then
      begin
        LTraceEvent.HasDirection := True;
        LTraceEvent.Direction :=
          TGraphDirection(LReferenceEvent.Direction);
      end;
      if LReferenceEvent.DecisionDepth >= 0 then
        LTraceEvent.DecisionDepth := LReferenceEvent.DecisionDepth;
      if LReferenceEvent.DomainCountBefore >= 0 then
        LTraceEvent.DomainCountBefore :=
          LReferenceEvent.DomainCountBefore;
      if LReferenceEvent.DomainCountAfter >= 0 then
        LTraceEvent.DomainCountAfter :=
          LReferenceEvent.DomainCountAfter;

      if LReferenceEvent.Kind = rtekInitialCandidateRemoved then
      begin
        LInitialIndex := (LReferenceEvent.EntryIndex
          * Length(AGraph.FValues)) + LReferenceEvent.ValueIndex;
        if (LInitialIndex >= 0)
          and (LInitialIndex < Length(AInitialCauses)) then
        begin
          LTraceEvent.CauseKind := AInitialCauses[LInitialIndex];
          LTraceEvent.DependencyPassIndex :=
            AInitialDependencyPasses[LInitialIndex];
          if LTraceEvent.CauseKind = gtckPassDependency then
          begin
            LTraceEvent.CauseEventId := LastPassTraceEvent(
              LTraceEvent.DependencyPassIndex);
            if LTraceEvent.CauseEventId < 0 then
              LTraceEvent.CauseEventId := APassBeginEvent;
          end;
        end;
      end;

      //Initial-domain causes are refined above into caller-domain or
      //pass-dependency causes. A decision can point at one of those refined
      //events, so inherit the public cause kind from the mapped event instead
      //of retaining the kernel's less-specific initial-domain classification.
      //Retry decisions similarly inherit the mapped backtrack classification.
      if (LReferenceEvent.Kind = rtekDecision)
        and (LTraceEvent.CauseEventId >= 0)
        and (LTraceEvent.CauseEventId < Length(AReport.Trace)) then
        LTraceEvent.CauseKind :=
          AReport.Trace[LTraceEvent.CauseEventId].CauseKind;

      //An initialization contradiction inherits the kernel classification of
      //its final removal. That removal may have been refined above from the
      //generic initial-domain cause into a pass dependency, so carry the
      //public classification and provider through to the contradiction too.
      if (LReferenceEvent.Kind = rtekContradiction)
        and (LReferenceEvent.CauseKind in
          [rtckInitialDomain, rtckLock])
        and (LTraceEvent.CauseEventId >= 0)
        and (LTraceEvent.CauseEventId < Length(AReport.Trace)) then
      begin
        LTraceEvent.CauseKind :=
          AReport.Trace[LTraceEvent.CauseEventId].CauseKind;
        if LTraceEvent.CauseKind = gtckPassDependency then
          LTraceEvent.DependencyPassIndex :=
            AReport.Trace[LTraceEvent.CauseEventId].DependencyPassIndex;
      end;

      //The kernel's solved marker carries the last changed cell as useful
      //internal context. The public event represents pass-level staging, so
      //keep only its causal link and normalize all cell-domain fields.
      if LReferenceEvent.Kind = rtekSolved then
      begin
        LTraceEvent.CauseKind := gtckTransaction;
        LTraceEvent.EntryIndex := -1;
        LTraceEvent.ValueIndex := -1;
        LTraceEvent.Value := TGraphValue.Empty;
        LTraceEvent.NeighborIndex := -1;
        LTraceEvent.HasDirection := False;
        LTraceEvent.Direction := gdNorth;
        LTraceEvent.DependencyPassIndex := -1;
        LTraceEvent.DecisionDepth := 0;
        LTraceEvent.DomainCountBefore := 0;
        LTraceEvent.DomainCountAfter := 0;
      end;

      LLocalToGlobal[I] := AppendTraceEvent(LTraceEvent);
    end;
  end;

  procedure CopyPassReport(const APassIndex: Integer;
    const AReference: TReferenceSolveReport);
  begin
    AReport.Passes[APassIndex].Decisions := AReference.Decisions;
    AReport.Passes[APassIndex].Propagations := AReference.Propagations;
    AReport.Passes[APassIndex].Contradictions :=
      AReference.Contradictions;
    AReport.Passes[APassIndex].Backtracks := AReference.Backtracks;
  end;

  procedure SetReferenceFailure(const APassIndex: Integer;
    const AReference: TReferenceSolveReport);
  begin
    if AReference.Status = rssBacktrackLimit then
      AReport.Status := gssBacktrackLimit
    else
      AReport.Status := gssContradiction;
    AReport.FailedPassIndex := APassIndex;
    AReport.Contradiction.Kind := PublicContradictionKind(
      AReference.Contradiction.Kind);
    AReport.Contradiction.PassIndex := APassIndex;
    AReport.Contradiction.EntryIndex :=
      AReference.Contradiction.EntryIndex;
    AReport.Contradiction.NeighborIndex :=
      AReference.Contradiction.NeighborIndex;
    AReport.Contradiction.DependencyPassIndex := -1;
    if (AReference.Contradiction.Kind = rckPreviousPass)
      and (AReference.Contradiction.EntryIndex >= 0)
      and (AReference.Contradiction.EntryIndex <
        Length(LRequirementFailurePass)) then
    begin
      AReport.Contradiction.DependencyPassIndex :=
        LRequirementFailurePass[AReference.Contradiction.EntryIndex];
      if LRequirementFailureNamed[
        AReference.Contradiction.EntryIndex] <> 0 then
        AReport.Contradiction.Kind := gckPassDependency;
    end;
    AReport.Contradiction.HasDirection :=
      AReference.Contradiction.Direction >= 0;
    if AReport.Contradiction.HasDirection then
      AReport.Contradiction.Direction := TGraphDirection(
        AReference.Contradiction.Direction)
    else
      AReport.Contradiction.Direction := gdNorth;
    AReport.Passes[APassIndex].Disposition := gpdFailed;
  end;

  procedure StageExistingPass(const APassIndex: Integer;
    const AGraph: TGraph);
  var
    I: Integer;
  begin
    SetLength(LStaged[APassIndex], AGraph.FEntries.Count);
    for I := 0 to Pred(AGraph.FEntries.Count) do
      if AGraph.FEntries[I].Empty then
        LStaged[APassIndex][I] := TGraphValue.Empty
      else
        LStaged[APassIndex][I] := AGraph.FEntries[I].Value;
  end;

  function StageDefinitionlessPass(const APassIndex: Integer;
    const AGraph: TGraph; out AFailedEntry,
    ASourcePassIndex: Integer;
    out AFailureFromSource: Boolean): Boolean;
  var
    I, LSourcePassIndex: Integer;
    LValueFromSource: Boolean;
  begin
    Result := False;
    AFailedEntry := -1;
    AFailureFromSource := False;
    LSourcePassIndex := -1;
    case AGraph.FPassMode of
      gpmLegacy:
        if APassIndex > 0 then
          LSourcePassIndex := Pred(APassIndex);
      gpmTransform:
        begin
          LSourcePassIndex := AGraph.FTransformSourceIndex;
          if LSourcePassIndex < 0 then
            raise EInvalidOperation.CreateFmt(
              'TrySolve::transform pass %d has no source', [APassIndex]);
        end;
      gpmOverlay:
        ;
    else
      raise ERangeError.Create('TrySolve::invalid pass mode');
    end;
    ASourcePassIndex := LSourcePassIndex;

    if LSourcePassIndex >= 0 then
    begin
      if (LSourcePassIndex >= Length(LStaged))
        or (Length(LStaged[LSourcePassIndex]) <>
          AGraph.FEntries.Count) then
        raise EInvalidOperation.CreateFmt(
          'TrySolve::source %d for pass %d is not staged',
          [LSourcePassIndex, APassIndex]);
      AReport.Passes[APassIndex].Disposition := gpdCopied;
    end
    else if (AGraph.FPassMode = gpmOverlay) then
      AReport.Passes[APassIndex].Disposition := gpdCleared
    else
      AReport.Passes[APassIndex].Disposition := gpdReused;

    SetLength(LStaged[APassIndex], AGraph.FEntries.Count);
    for I := 0 to Pred(AGraph.FEntries.Count) do
    begin
      LValueFromSource := False;
      if (not AGraph.FEntries[I].Empty)
        and (not AGraph.FEntries[I].Generated) then
        LStaged[APassIndex][I] := AGraph.FEntries[I].Value
      else if LSourcePassIndex >= 0 then
      begin
        LStaged[APassIndex][I] := LStaged[LSourcePassIndex][I];
        LValueFromSource := True;
      end
      else if (AGraph.FPassMode = gpmLegacy)
        and (not AGraph.FEntries[I].Empty) then
        LStaged[APassIndex][I] := AGraph.FEntries[I].Value
      else
        LStaged[APassIndex][I] := TGraphValue.Empty;
      if AGraph.FEntries[I].FHasAllowedValues
        and ((LStaged[APassIndex][I] = TGraphValue.Empty)
          or (not ContainsGraphValue(
            AGraph.FEntries[I].FAllowedValues,
            LStaged[APassIndex][I]))) then
      begin
        AFailedEntry := I;
        AFailureFromSource := LValueFromSource;
        Exit;
      end;
    end;
    Result := True;
  end;

  procedure SnapshotEntries;
  var
    I, J: Integer;
  begin
    SetLength(LSnapshots, FPasses.Count);
    for I := 0 to Pred(FPasses.Count) do
    begin
      SetLength(LSnapshots[I], FPasses[I].FEntries.Count);
      for J := 0 to Pred(FPasses[I].FEntries.Count) do
      begin
        LSnapshots[I][J].Value := FPasses[I].FEntries[J].Value;
        LSnapshots[I][J].Empty := FPasses[I].FEntries[J].Empty;
        LSnapshots[I][J].Generated := FPasses[I].FEntries[J].Generated;
      end;
    end;
  end;

  procedure RestoreEntries;
  var
    I, J: Integer;
  begin
    for I := 0 to High(LSnapshots) do
      for J := 0 to High(LSnapshots[I]) do
        FPasses[I].FEntries[J].RestoreValueState(
          LSnapshots[I][J].Value,
          LSnapshots[I][J].Empty,
          LSnapshots[I][J].Generated);
  end;

  procedure CommitStagedEntries;
  var
    I, J, K: Integer;

    function MatchesExpectedState(const APassIndex,
      AEntryIndex: Integer): Boolean;
    var
      LEntry: TGraphEntry;
      LSnapshot: TEntryState;
    begin
      LEntry := FPasses[APassIndex].FEntries[AEntryIndex];
      LSnapshot := LSnapshots[APassIndex][AEntryIndex];

      if (ADirty[APassIndex] = 0)
        or ((APassIndex = 0)
          and (FPasses[APassIndex].FPassMode = gpmLegacy)
          and (not FPasses[APassIndex].HasDefinition))
        or ((not LSnapshot.Empty) and (not LSnapshot.Generated)) then
        Exit((LEntry.Value = LSnapshot.Value)
          and (LEntry.Empty = LSnapshot.Empty)
          and (LEntry.Generated = LSnapshot.Generated));

      if LStaged[APassIndex][AEntryIndex] = TGraphValue.Empty then
        Result := (LEntry.Value = TGraphValue.Empty)
          and LEntry.Empty and (not LEntry.Generated)
      else
        Result := (LEntry.Value = LStaged[APassIndex][AEntryIndex])
          and (not LEntry.Empty) and LEntry.Generated;
    end;
  begin
    SnapshotEntries;
    try
      for K := 0 to High(LExecutionPlan) do
      begin
        I := LExecutionPlan[K];
        FExecutingPassIndex := I;
        FCurPassIndex := I;
        FCurPass := PassLabelFromIndex(I);
        //A definitionless first pass has no predecessor and remains exactly as
        //the caller supplied it.
        if (I = 0) and (FPasses[I].FPassMode = gpmLegacy)
          and (not FPasses[I].HasDefinition) then
          Continue;
        for J := 0 to Pred(FPasses[I].FEntries.Count) do
        begin
          //A preceding hook may have switched the root selection. Reassert
          //the pass identity for every setter, matching the solving contract.
          FExecutingPassIndex := I;
          FCurPassIndex := I;
          FCurPass := PassLabelFromIndex(I);
          //Ownership is part of the pre-commit snapshot. A setter hook may
          //mutate another live entry, but cannot turn staged output into a
          //new caller lock and thereby bypass the validated assignment.
          if (not LSnapshots[I][J].Empty)
            and (not LSnapshots[I][J].Generated) then
            Continue;
          if LStaged[I][J] = TGraphValue.Empty then
            FPasses[I].FEntries[J].ClearValue
          else
            FPasses[I].FEntries[J].SetGeneratedValue(LStaged[I][J]);
        end;
      end;

      //A hook can also rewrite an entry that has already been committed, or a
      //caller lock that is intentionally skipped. Detect every such mutation
      //before reporting success so the existing raw-state rollback applies.
      for I := 0 to Pred(FPasses.Count) do
        for J := 0 to Pred(FPasses[I].FEntries.Count) do
          if not MatchesExpectedState(I, J) then
            raise EInvalidOperation.CreateFmt(
              'TrySolve::commit hook mutated pass %d entry %d', [I, J]);
    except
      RestoreEntries;
      raise;
    end;
  end;

var
  I, LExecutionCount: Integer;
begin
  if FInitializingPass then
    raise EInvalidOperation.Create(
      'TrySolve::cannot solve during pass initialization');
  EnsureInitialPass;
  if FRunning then
    raise EInvalidOperation.Create(
      'TrySolve::the pass pipeline is already running');
  if Length(ADirty) <> FPasses.Count then
    raise EInvalidOperation.Create(
      'TrySolve::pass selection does not match the pipeline');

  Result := False;
  LCommitted := False;
  InitializeReport;
  BuildPassExecutionOrder(LFullExecutionPlan);
  LExecutionCount := 0;
  for I := 0 to High(LFullExecutionPlan) do
    if ADirty[LFullExecutionPlan[I]] <> 0 then
      Inc(LExecutionCount);
  SetLength(LExecutionPlan, LExecutionCount);
  LExecutionCount := 0;
  for I := 0 to High(LFullExecutionPlan) do
    if ADirty[LFullExecutionPlan[I]] <> 0 then
    begin
      LExecutionPlan[LExecutionCount] := LFullExecutionPlan[I];
      Inc(LExecutionCount);
    end;
  SetLength(LStaged, FPasses.Count);
  for I := 0 to Pred(FPasses.Count) do
    if ADirty[I] = 0 then
    begin
      StageExistingPass(I, FPasses[I]);
      AReport.Passes[I].Disposition := gpdReused;
      LTraceEvent := NewTraceEvent(gtekPassSkipped,
        gtckTransaction, I);
      AppendTraceEvent(LTraceEvent);
    end;
  LSavedPassIndex := FCurPassIndex;
  LRootRandomState := FRandomState;
  SetLength(LRandomStates, FPasses.Count);
  for I := 0 to Pred(FPasses.Count) do
    LRandomStates[I] := FPasses[I].FRandomState;
  FRunning := True;
  FExecutingPassIndex := -1;
  try
    EnsureSeedInitialized;
    for I := 0 to Pred(FPasses.Count) do
      if ADirty[I] <> 0 then
      begin
        BuildPassRandomState(I, FPasses[I].FRandomState);
        FPasses[I].FSeed := FSeed;
        FPasses[I].FSeedInitialized := True;
      end;
    for LExecutionOrdinal := 0 to High(LExecutionPlan) do
    begin
      LPassIndex := LExecutionPlan[LExecutionOrdinal];
      //Execution reporting describes work that was actually attempted, not
      //the whole selected closure.  In particular, a contradiction leaves
      //later dirty dependents as not-run rather than implying that they were
      //visited before the failure.
      AReport.Passes[LPassIndex].Executed := True;
      AReport.Passes[LPassIndex].ExecutionOrdinal :=
        Length(AReport.ExecutionOrder);
      SetLength(AReport.ExecutionOrder,
        Succ(Length(AReport.ExecutionOrder)));
      AReport.ExecutionOrder[High(AReport.ExecutionOrder)] := LPassIndex;
      FExecutingPassIndex := LPassIndex;
      FCurPassIndex := LPassIndex;
      FCurPass := PassLabelFromIndex(LPassIndex);
      LGraph := FPasses[LPassIndex];
      LTraceEvent := NewTraceEvent(gtekPassBegin,
        gtckTransaction, LPassIndex);
      LPassBeginEvent := AppendTraceEvent(LTraceEvent);

      if not LGraph.HasDefinition then
      begin
        if not StageDefinitionlessPass(LPassIndex, LGraph,
          LInvalidLockEntry, LDefinitionlessSourcePass,
          LDefinitionlessFailureFromSource) then
        begin
          AReport.Status := gssContradiction;
          AReport.FailedPassIndex := LPassIndex;
          AReport.Contradiction.Kind := gckEntryDomain;
          AReport.Contradiction.PassIndex := LPassIndex;
          AReport.Contradiction.EntryIndex := LInvalidLockEntry;
          AReport.Contradiction.NeighborIndex := -1;
          AReport.Contradiction.HasDirection := False;
          AReport.Contradiction.Direction := gdNorth;
          AReport.Contradiction.DependencyPassIndex := -1;
          AReport.Passes[LPassIndex].Contradictions := 1;
          AReport.Passes[LPassIndex].Disposition := gpdFailed;
          if LDefinitionlessFailureFromSource then
            LTraceEvent := NewTraceEvent(gtekContradiction,
              gtckPassDependency, LPassIndex)
          else
            LTraceEvent := NewTraceEvent(gtekContradiction,
              gtckCallerDomain, LPassIndex);
          if LDefinitionlessFailureFromSource then
          begin
            LTraceEvent.DependencyPassIndex :=
              LDefinitionlessSourcePass;
            LTraceEvent.CauseEventId := LastPassTraceEvent(
              LDefinitionlessSourcePass);
          end
          else
            LTraceEvent.CauseEventId := -1;
          LTraceEvent.EntryIndex := LInvalidLockEntry;
          LTraceCauseEventId := AppendTraceEvent(LTraceEvent);
          LTraceEvent := NewTraceEvent(gtekPassFailed,
            gtckTransaction, LPassIndex);
          LTraceEvent.CauseEventId := LTraceCauseEventId;
          AppendTraceEvent(LTraceEvent);
          Exit(False);
        end;
        LTraceEvent := NewTraceEvent(gtekPassStaged,
          gtckTransaction, LPassIndex);
        if LDefinitionlessSourcePass >= 0 then
          LTraceEvent.CauseEventId := LastPassTraceEvent(
            LDefinitionlessSourcePass)
        else
          LTraceEvent.CauseEventId := LPassBeginEvent;
        AppendTraceEvent(LTraceEvent);
        Continue;
      end;

      if not BuildReferenceModel(LGraph, LStaged, LModel,
        LInvalidLockEntry, LRequirementFailurePass,
        LRequirementFailureNamed, LInitialTraceCauses,
        LInitialTraceDependencyPasses) then
      begin
        AReport.Status := gssContradiction;
        AReport.FailedPassIndex := LPassIndex;
        AReport.Contradiction.Kind := gckInvalidLock;
        AReport.Contradiction.PassIndex := LPassIndex;
        AReport.Contradiction.EntryIndex := LInvalidLockEntry;
        AReport.Contradiction.DependencyPassIndex := -1;
        AReport.Passes[LPassIndex].Contradictions := 1;
        AReport.Passes[LPassIndex].Disposition := gpdFailed;
        LTraceEvent := NewTraceEvent(gtekContradiction,
          gtckCallerLock, LPassIndex);
        LTraceEvent.CauseEventId := -1;
        LTraceEvent.EntryIndex := LInvalidLockEntry;
        LTraceCauseEventId := AppendTraceEvent(LTraceEvent);
        LTraceEvent := NewTraceEvent(gtekPassFailed,
          gtckTransaction, LPassIndex);
        LTraceEvent.CauseEventId := LTraceCauseEventId;
        AppendTraceEvent(LTraceEvent);
        Exit(False);
      end;

      if not SolveReferenceModel(LModel, AOptions.MaxBacktracks,
        AOptions.CaptureTrace, LGraph.RandomIndex,
        LAssignment, LReferenceReport) then
      begin
        MapReferenceTrace(LPassIndex, LPassBeginEvent, LGraph,
          LReferenceReport, LInitialTraceCauses,
          LInitialTraceDependencyPasses);
        CopyPassReport(LPassIndex, LReferenceReport);
        SetReferenceFailure(LPassIndex, LReferenceReport);
        LTraceEvent := NewTraceEvent(gtekPassFailed,
          gtckTransaction, LPassIndex);
        LTraceEvent.CauseEventId := LastPassTraceEvent(LPassIndex);
        AppendTraceEvent(LTraceEvent);
        Exit(False);
      end;
      MapReferenceTrace(LPassIndex, LPassBeginEvent, LGraph,
        LReferenceReport, LInitialTraceCauses,
        LInitialTraceDependencyPasses);
      CopyPassReport(LPassIndex, LReferenceReport);
      AReport.Passes[LPassIndex].Disposition := gpdSolved;

      SetLength(LStaged[LPassIndex], LGraph.FEntries.Count);
      for LEntryIndex := 0 to Pred(LGraph.FEntries.Count) do
        LStaged[LPassIndex][LEntryIndex] :=
          LGraph.FValues[LAssignment[LEntryIndex]];
    end;

    ReserveTerminalTraceSlot;
    CommitStagedEntries;
    //A successful selective commit may execute user entry hooks.  A hook can
    //address a reused pass directly and draw from its stream; skipped layers
    //must remain observationally untouched, including their RNG position.
    for I := 0 to Pred(FPasses.Count) do
      if ADirty[I] = 0 then
        FPasses[I].FRandomState := LRandomStates[I];
    LCommitted := True;
    AReport.Status := gssSolved;
    AReport.FailedPassIndex := -1;
    AReport.Contradiction.Kind := gckNone;
    AReport.Contradiction.PassIndex := -1;
    AReport.Contradiction.EntryIndex := -1;
    AReport.Contradiction.NeighborIndex := -1;
    AReport.Contradiction.HasDirection := False;
    AReport.Contradiction.DependencyPassIndex := -1;
    LTraceEvent := NewTraceEvent(gtekPipelineCommit,
      gtckTransaction, -1);
    LTraceEvent.CauseEventId := Pred(LTraceCount);
    AppendTraceEvent(LTraceEvent);
    Result := True;
  finally
    if not LCommitted then
    begin
      LTraceEvent := NewTraceEvent(gtekPipelineRollback,
        gtckTransaction, -1);
      LTraceEvent.CauseEventId := Pred(LTraceCount);
      AppendTraceEvent(LTraceEvent);
      FRandomState := LRootRandomState;
      for I := 0 to Pred(FPasses.Count) do
        FPasses[I].FRandomState := LRandomStates[I];
    end;
    if AOptions.CaptureTrace then
      SetLength(AReport.Trace, LTraceCount)
    else
      AReport.Trace := nil;
    FExecutingPassIndex := -1;
    FCurPassIndex := LSavedPassIndex;
    FCurPass := PassLabelFromIndex(LSavedPassIndex);
    FRunning := False;
  end;
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
  ValidateDeniedRuleState('Run');
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
  I, LPassIndex, LSourceIndex: Integer;
  LExecutionOrder: TGraphPassIndices;
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
  BuildPassExecutionOrder(LExecutionOrder);

  LSavedPassIndex := FCurPassIndex;
  FRunning := True;
  FExecutingPassIndex := -1;

  try
    //Every execution starts from the same per-pass streams. Random calls made
    //outside Run therefore cannot perturb a replay.
    RewindRandomStates;
    for I := 0 to High(LExecutionOrder) do
    begin
      LPassIndex := LExecutionOrder[I];
      FExecutingPassIndex := LPassIndex;
      FCurPassIndex := LPassIndex;
      FCurPass := PassLabelFromIndex(LPassIndex);
      LGraph := PassGraph[LPassIndex];

      if not LGraph.HasDefinition then
      begin
        case LGraph.FPassMode of
          gpmLegacy:
            if LPassIndex > 0 then
              LGraph.CopyValuesFrom(PassGraph[Pred(LPassIndex)]);
          gpmTransform:
            begin
              LSourceIndex := LGraph.FTransformSourceIndex;
              if LSourceIndex < 0 then
                raise EInvalidOperation.CreateFmt(
                  'Run::transform pass %d has no source', [LPassIndex]);
              LGraph.CopyValuesFrom(PassGraph[LSourceIndex]);
            end;
          gpmOverlay:
            LGraph.ClearGeneratedValues;
        else
          raise ERangeError.Create('Run::invalid pass mode');
        end;
        LGraph.ValidateCurrentEntryDomains('Run');
      end
      else
        LGraph.RunOnePass;

      //selection callbacks are allowed to inspect or switch passes; the
      //coordinator always resumes the pass currently being solved
      FCurPassIndex := LPassIndex;
      FCurPass := PassLabelFromIndex(LPassIndex);
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
  FPassMode := gpmLegacy;
  SetLength(FPassDependencies, 0);
  FTransformSourceIndex := -1;
end;

constructor TGraph.CreatePass(const ARoot: TGraph;
  const APassIndex: Integer);
var
  LPrevious: TGraph;
begin
  InitializeStorage;
  FPassRoot := ARoot;
  FPassIndex := APassIndex;
  if APassIndex > 0 then
  begin
    SetLength(FPassDependencies, 1);
    FPassDependencies[0].PassIndex := Pred(APassIndex);
    FPassDependencies[0].Roles := [pdrLegacy];
  end;
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

