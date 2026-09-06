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
  Generics.Collections,
  wfc_lattice;

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
  //Stable value-definition indices used to report exact pass assignments.
  //Negotiation compares the complete arrays; hashes are diagnostics only.
  TGraphValueIndices = array of Integer;

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
  //Identifies opt-in finite cross-pass counting with an explicit alias mode.
  //Existing graph, solver, pipeline, and trace replay remain unchanged.
  WFC_PASS_COUNT_VERSION = 1;
  //Opt-in integer world layouts and exact point/footprint pass sampling.
  //Legacy index-space rules and seeded solver replay remain unchanged.
  WFC_PASS_MAPPING_VERSION = 1;
  //Opt-in rooted, reciprocal-port connectivity. Unconstrained models retain
  //their existing solver, random-stream, and trace replay.
  WFC_GRAPH_CONNECTIVITY_VERSION = 1;
  //Opt-in whole-pass value-set cardinality. Models without quotas retain
  //their existing solver, random-stream, and trace replay.
  WFC_GRAPH_VALUE_QUOTA_VERSION = 1;
  //Identifies the public causal-trace event schema. Trace capture is opt-in,
  //so adding this observability surface does not change solver replay.
  WFC_TRACE_VERSION = 1;
  //Identifies the portable integer encoding used by trace signatures.
  WFC_TRACE_HASH_VERSION = 1;
  //Delivery is separate from the unchanged Trace-v1 event/hash contract.
  WFC_TRACE_DELIVERY_VERSION = 1;
  //Identifies the opt-in chronological pass-assignment negotiation protocol.
  //The ordinary one-way pipeline remains independently versioned above.
  WFC_PASS_NEGOTIATION_ALGORITHM_VERSION = 1;
  WFC_PASS_NEGOTIATION_HASH_VERSION = 1;
  //Identifies the explicit descendant-closure scope layered around the
  //unchanged chronological negotiation protocol. Full-pipeline negotiation
  //keeps its own versions and portable goldens above.
  WFC_SELECTIVE_NEGOTIATION_ALGORITHM_VERSION = 1;
  WFC_SELECTIVE_NEGOTIATION_HASH_VERSION = 1;
  //Opt-in whole-transaction restart scheduling and transcript encoding.
  WFC_RESTART_ALGORITHM_VERSION = 1;
  WFC_RESTART_HASH_VERSION = 1;

type

  TGraphPosition = record
    X : TGraphCoordinate;
    Y : TGraphCoordinate;
    Z : TGraphCoordinate;
  end;
  TGraphPositions = array of TGraphPosition;

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

  //MatchingTerms counts canonical declared offsets, even if wrapping maps
  //several to one cell. DistinctCells counts a resolved cell once when any
  //of its aliased terms matches. The caller must choose the interpretation.
  TGraphPassCountMode = (gpcmMatchingTerms, gpcmDistinctCells);

  TGraphPassMapKind = (gpmkPoint, gpmkCellCoverage, gpmkRegionCoverage);
  TGraphPassMapMatch = (gpmmAll, gpmmCount);
  //World-tick offsets are relative to a consumer cell's lower corner. Regions
  //are half-open; cell coverage uses its complete pitch-sized footprint.
  //Coverage counts distinct provider cells, including across wrapping seams.
  TGraphPassMapQuery = record
    Kind: TGraphPassMapKind;
    Match: TGraphPassMapMatch;
    MinimumOffset, MaximumOffset: TGraphOffset;
    Values: TGraphValues;
    MinimumMatches, MaximumMatches: Integer;
  end;

  //all posible "directions" to move from a single point on the graph
  TGraphDirection = (gdNorth, gdEast, gdSouth, gdWest, gdUp, gdDown);
  TGraphDirections = set of TGraphDirection;

  TGraphConnectivityValue = record
    Value: TGraphValue;
    Openings: TGraphDirections;
    RequiredByValue: Boolean;
  end;
  TGraphConnectivityValues = array of TGraphConnectivityValue;
  TGraphConnectivityConstraint = record
    LabelText: String;
    Root: TGraphPosition;
    RequiredPositions: TGraphPositions;
    Values: TGraphConnectivityValues;
    RequireAllParticipants: Boolean;
  end;
  TGraphConnectivityConstraints = array of TGraphConnectivityConstraint;

  //Count each cell in this pass once when its value belongs to Values.
  //Bounds are independent of the current shape: a minimum larger than the
  //cell count is an unsatisfiable constraint, not a malformed descriptor.
  TGraphValueQuotaConstraint = record
    LabelText: String;
    Values: TGraphValues;
    MinimumCount: Integer;
    MaximumCount: Integer;
  end;
  TGraphValueQuotaConstraints = array of TGraphValueQuotaConstraint;

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
    procedure RestoreNeighborState(const ADirection: TGraphDirection;
      const ANeighbor: TGraphEntry);
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
      TPassRequirementKind = (prkMergedOffset, prkAny, prkCount, prkMapped);
      TPassRequirement = record
        PassIndex: Integer;
        Terms: TGraphPassMatchTerms;
        Origins: TPassRequirementOrigins;
        Kind: TPassRequirementKind;
        MinimumMatches: Integer;
        MaximumMatches: Integer;
        CountMode: TGraphPassCountMode;
        //Only prkMapped reads this additive descriptor.
        MappedQuery: TGraphPassMapQuery;
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
    function BuildPassCountRequirements(const APassIndex: Integer;
      const ATerms: TGraphPassMatchTerms;
      const AMinimum, AMaximum: Integer; const AMode: TGraphPassCountMode;
      const AOrigin: TPassRequirementOrigin): TPassRequirements;
    function BuildMappedPassRequirements(const APassIndex: Integer;
      const AQuery: TGraphPassMapQuery): TPassRequirements;
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
    procedure DoRequireCountFromPass(const APass: String;
      const ATerms: TGraphPassMatchTerms;
      const AMinimum, AMaximum: Integer;
      const AMode: TGraphPassCountMode); virtual;
    procedure DoRequireMappedFromPass(const APass: String;
      const AQuery: TGraphPassMapQuery); virtual;
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

    //Adds an inclusive count-range clause, ANDed with all other clauses.
    //Terms must be non-empty; identical signed offsets merge their value
    //alternatives before checking 0 <= minimum <= maximum <= term count.
    //Missing or empty provider cells do not match, so 0..0 expresses absence.
    //Identical terms, bounds, and mode are idempotent; input arrays are copied.
    //Even a 0..term-count clause retains its validated provider dependency.
    function RequireCountFromPass(const APass: String;
      const ATerms: TGraphPassMatchTerms;
      const AMinimum, AMaximum: Integer;
      const AMode: TGraphPassCountMode): TGraphRuleGroup;

    //Explicit world-space sampling; never silently rescales index-space rules.
    //A bounded provider must cover the complete query. Identical clauses are
    //idempotent; separate calls are ANDed and infer the named dependency.
    function RequireMappedFromPass(const APass: String;
      const AQuery: TGraphPassMapQuery): TGraphRuleGroup;

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
    gckEntryDomain,
    gckExcludedAssignment,
    gckConnectivity,
    gckValueQuota
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
    //Pass-local copied descriptor ordinal for connectivity/value-quota kinds,
    //or -1 otherwise. The kind selects the corresponding registry.
    ConstraintIndex: Integer;
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
    gtckTransaction,
    gtckExactAssignmentExclusion,
    gtckConnectivity,
    gtckValueQuota
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
    ConstraintIndex: Integer;
  end;

  TGraphTraceEvents = array of TGraphTraceEvent;

  TGraphTraceHeader = record
    DeliveryVersion: Integer;
    TraceVersion: Integer;
    TraceHashVersion: Integer;
    Seed: TGraphSeed;
    RandomAlgorithmVersion: Integer;
    SolverAlgorithmVersion: Integer;
    GraphModelVersion: Integer;
    PipelineAlgorithmVersion: Integer;
    PassCount: Integer;
  end;

  TGraphTraceDeliveryStatus = (gtdsDisabled, gtdsComplete,
    gtdsSinkFailed, gtdsInterrupted);
  TGraphTraceDeliveryPhase = (gtdpNone, gtdpBegin, gtdpEvent, gtdpEnd);
  TGraphTraceDelivery = record
    Version: Integer;
    Status: TGraphTraceDeliveryStatus;
    //Production continues after observer failure; delivered is the accepted
    //event prefix, not a claim that the whole transaction was delivered.
    ProducedEventCount: Integer;
    DeliveredEventCount: Integer;
    TraceHash: TGraphTraceSignature;
    FailurePhase: TGraphTraceDeliveryPhase;
    FailureEventId: Integer;
    FailureMessage: String;
  end;

  //Borrowed, synchronous observation. Do not mutate, draw randomness from,
  //reenter, or destroy the graph from these callbacks. Each ordinary solve
  //attempt (including negotiation/restart rounds) has its own Begin/End.
  //A callback exception detaches delivery for that attempt only; it does not
  //turn a solved graph into a contradiction or discard a retained full trace.
  TGraphTraceSink = class
  public
    procedure BeginTrace(const AHeader: TGraphTraceHeader); virtual; abstract;
    procedure AppendEvent(const AEvent: TGraphTraceEvent); virtual; abstract;
    procedure EndTrace(const ADelivery: TGraphTraceDelivery); virtual; abstract;
  end;

  TGraphPassSolveReport = record
    Decisions: Integer;
    Propagations: Integer;
    Contradictions: Integer;
    Backtracks: Integer;
    ExcludedAssignments: Integer;
    Executed: Boolean;
    ExecutionOrdinal: Integer;
    Disposition: TGraphPassDisposition;
    //Legacy first-event and total-event metadata. It is a half-open slice for
    //ordinary reports; use wfc_trace layouts for a late commit-failure suffix.
    //TraceCount is zero when capture is disabled or this pass emitted no
    //events.
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
    //Independent observation metadata. Legacy Trace fields remain empty/zero
    //when CaptureTrace=False, even when a sink receives all events live.
    TraceDelivery: TGraphTraceDelivery;
  end;

  //Pass negotiation is deliberately separate from ordinary solve status.
  //A solver limit belongs to one pass attempt; a pass limit bounds the
  //number of exact provider assignments the coordinator may exclude.
  TGraphNegotiationStatus = (
    gnsSolved,
    gnsContradiction,
    gnsSolverBacktrackLimit,
    gnsPassBacktrackLimit
  );

  TGraphNegotiationOptions = record
    SolveOptions: TGraphSolveOptions;
    //Zero performs the ordinary one-way attempt without reopening a pass.
    MaxPassBacktracks: Integer;
  end;

  TGraphNegotiationAttemptReport = record
    //Every rejected round retains its own ordinary Trace-v1 chronology. Use a
    //wfc_trace layout when late validation appends an earlier-pass suffix. The
    //sole terminal round is stored in FinalReport below.
    SolveReport: TGraphSolveReport;
    //The completed defined pass excluded after this failed round, or -1 when
    //the round is terminal. The exclusion is scoped to its earlier prefix.
    BacktrackedPassIndex: Integer;
    BacktrackedExecutionOrdinal: Integer;
    ExcludedAssignment: TGraphValueIndices;
  end;
  TGraphNegotiationAttemptReports =
    array of TGraphNegotiationAttemptReport;

  TGraphNegotiationReport = record
    Status: TGraphNegotiationStatus;
    Seed: TGraphSeed;
    NegotiationAlgorithmVersion: Integer;
    PassBacktracks: Integer;
    //Rejected rounds in chronological order. Total rounds are therefore
    //Length(Attempts) + 1 whenever the call returns normally.
    Attempts: TGraphNegotiationAttemptReports;
    FinalReport: TGraphSolveReport;
    //Portable summary of the complete ordered attempt transcript. Exact
    //assignments remain present above and are never identified by hash alone.
    TranscriptHash: TGraphTraceSignature;
  end;

  TGraphRestartSchedule = (grschFixed, grschCappedDoubling);
  TGraphRestartOptions = record
    //Attempts include the initial solve, followed by at most MaxRestarts.
    //Valid values are 0..High(Integer)-1, leaving room for the initial slot.
    MaxRestarts: Integer;
    //Must be at least the original solve budget; attempt zero is never clipped.
    //Capped doubling preserves a zero original budget as zero on every attempt.
    MaxBacktracksPerAttempt: Integer;
    Schedule: TGraphRestartSchedule;
    //Diagnostic only: never changes search decisions or transcript identity.
    MeasureTime: Boolean;
  end;
  TGraphRestartStrategy = (grstOneWay, grstNegotiated);
  TGraphRestartStatus = (grsSolved, grsContradiction, grsRestartLimit,
    grsPassBacktrackLimit);
  TGraphRestartAttemptReport = record
    Index: Integer;
    Seed: TGraphSeed;
    MaxBacktracks: Integer;
    SolveReport: TGraphSolveReport;
    //Populated only by the negotiated strategy.
    NegotiationReport: TGraphNegotiationReport;
    ElapsedMilliseconds: Double;
    TimingAvailable: Boolean;
  end;
  TGraphRestartAttemptReports = array of TGraphRestartAttemptReport;
  TGraphRestartReport = record
    BaseSeed: TGraphSeed;
    Strategy: TGraphRestartStrategy;
    Status: TGraphRestartStatus;
    RestartAlgorithmVersion: Integer;
    Restarts: Integer;
    //Every attempt, including the terminal one, retains its own trace.
    Attempts: TGraphRestartAttemptReports;
    FinalReport: TGraphSolveReport;
    TranscriptHash: TGraphTraceSignature;
    ElapsedMilliseconds: Double;
    TimingAvailable: Boolean;
  end;

  //Selective negotiation keeps its scope identity separate from the nested
  //Pass Negotiation v1 transcript. Requested roots are canonical stable pass
  //indices; active passes are their exact descendant closure in execution
  //order. Passes absent from ActivePassIndices remain immutable inputs.
  TGraphSelectiveNegotiationReport = record
    ScopeAlgorithmVersion: Integer;
    RequestedRootIndices: TGraphPassIndices;
    ActivePassIndices: TGraphPassIndices;
    Search: TGraphNegotiationReport;
    TranscriptHash: TGraphTraceSignature;
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
        procedure DoRequireCountFromPass(const APass: String;
          const ATerms: TGraphPassMatchTerms;
          const AMinimum, AMaximum: Integer;
          const AMode: TGraphPassCountMode); override;
        procedure DoRequireMappedFromPass(const APass: String;
          const AQuery: TGraphPassMapQuery); override;
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
      TValueIndexMatrix = array of TGraphValueIndices;
      TAssignmentExclusionSet = array of TGraphValueIndices;
      TPassAssignmentExclusions = array of TAssignmentExclusionSet;
  strict private
    FDimension: TDimension;
    FLayout: TWfcLatticeLayout;
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
    FConnectivity: TGraphConnectivityConstraints;
    FValueQuotas: TGraphValueQuotaConstraints;
    FTraceSink: TGraphTraceSink;

    function GetTraceSink: TGraphTraceSink;
    procedure SetTraceSink(const AValue: TGraphTraceSink);

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
      out AState: TRandomState); overload;
    procedure BuildPassRandomState(const APassIndex: Integer;
      const AEffectiveSeed: TGraphSeed; out AState: TRandomState); overload;
    procedure EnsureSeedInitialized;
    procedure RewindRandomStates;
    procedure EnsureInitialPass;
    function NewPlanes: TPlanes;
    function GetActivePassGraph: TGraph;
    function GetPassLayout: TWfcLatticeLayout;
    function CopyPassLayouts: TWfcLatticeLayouts;
    procedure ValidatePassLayoutReads(const ALayouts: TWfcLatticeLayouts);
    procedure RequireIdenticalPassLayout(const AProviderIndex: Integer;
      const AOperation: String);
    function MappedRequirementMatches(const AEntryIndex: Integer;
      const AProvider: TGraph; const AQuery: TGraphPassMapQuery;
      const AStagedValues: TGraphValues; const AUseStaged: Boolean): Boolean;
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
    procedure BuildDescendantPassSelection(
      const APasses: TGraphPassLabels; const AOperation: String;
      out ADirty: TPassSelection); overload;
    procedure BuildDescendantPassSelection(
      const APasses: TGraphPassLabels; const AOperation: String;
      out ARequestedRootIndices, AActivePassIndices: TGraphPassIndices;
      out ADirty: TPassSelection); overload;
    procedure ValidateNegotiationOptions(
      const AOptions: TGraphNegotiationOptions;
      const AOperation: String);
    function TryNegotiateInternal(
      const AOptions: TGraphNegotiationOptions;
      const ADirty: TPassSelection; const AOperation: String;
      const AEffectiveSeed: TGraphSeed;
      out AReport: TGraphNegotiationReport): Boolean;
    function TryRestartInternal(const AOptions: TGraphNegotiationOptions;
      const ARestarts: TGraphRestartOptions;
      const AStrategy: TGraphRestartStrategy;
      out AReport: TGraphRestartReport): Boolean;
    function ReadRestartClock(out AValue: Double): Boolean;
    function TrySolveInternal(const AOptions: TGraphSolveOptions;
      const ADirty: TPassSelection;
      out AReport: TGraphSolveReport): Boolean;
    function TrySolveAttempt(const AOptions: TGraphSolveOptions;
      const ADirty: TPassSelection;
      const AExclusions: TPassAssignmentExclusions;
      const AEffectiveSeed: TGraphSeed;
      out ACompletedChoices: TPassSelection;
      out AAssignments: TValueIndexMatrix;
      out AReport: TGraphSolveReport): Boolean;
    procedure BuildStorage(const AWidth, AHeight, ADepth: TGraphCoordinate;
      const AWrap: Boolean; out AEntries: TGraphEntries; out APlanes: TPlanes);
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
    procedure ValidateConnectivityShape(const AWidth, AHeight,
      ADepth: TGraphCoordinate);
  strict protected
    function DoCreateEntry: TGraphEntry; virtual;
    function DoCreatePass(const APassIndex: Integer): TGraph; virtual;
    //initialize fields owned by a derived graph on each pass instance
    procedure DoInitializePass; virtual;
    //Runs after a complete candidate has been written to live entries but
    //while the entry and random-stream snapshots are still rollback capable.
    //Derived domain owners can reject a semantically invalid composition
    //without publishing it. The reported pass must identify an active layer
    //whose final domain validation failed; entry may be -1 for a whole-pass
    //issue.
    function DoValidateCommit(out AFailedPassIndex,
      AFailedEntryIndex: Integer): Boolean; virtual;
    //Observational, side-effect-free hook: overrides must not alter graph or
    //generation inputs. Exceptions and invalid values mean unavailable timing.
    //A running guard rejects reentrant solves while this hook is called.
    function DoReadMonotonicMilliseconds(out AValue: Double): Boolean; virtual;
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
    property TraceSink: TGraphTraceSink read GetTraceSink write SetTraceSink;

    (*
      dimension of the graph
        - Width is X along plane
        - Height is Y along plane
        - Depth is amount of planes in Z
    *)
    property Dimension : TDimension read FDimension;
    //Unlike Dimension's historical root/default view, this follows the active
    //pass (or a directly addressed PassGraph). Returned records are detached.
    property PassLayout: TWfcLatticeLayout read GetPassLayout;

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
    //Configure every existing pass atomically. Rules remain; all entry values
    //and caller domains are cleared, as with Reshape. Each layout controls its
    //own neighbors and provider wrapping. Reshape restores one uniform unit
    //layout; the global WrapNeighbors setter still updates every pass.
    function ConfigurePassLayouts(const ALayouts: TWfcLatticeLayouts): TGraph;

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

    //Pass-local AND constraints, supported by the TrySolve family (not Run).
    //Definitions are deep-copied: profiles use AddValue order, fixed terminals
    //use flattened cell order with duplicates removed. Identical labeled
    //registration is idempotent; remove then require to change a definition.
    function RequireConnectivity(
      const AConstraint: TGraphConnectivityConstraint): TGraph;
    function RemoveConnectivity(const ALabel: String): TGraph;
    function ClearConnectivity: TGraph;
    function CopyConnectivityConstraints: TGraphConnectivityConstraints;

    //Opt-in whole-pass AND quotas for the TrySolve family (not Run).
    //Values are a nonempty set of registered values, copied and deduplicated
    //in AddValue order. Bounds satisfy 0 <= minimum <= maximum; they need not
    //fit the current cell count. Identical labeled registration is idempotent.
    function RequireValueQuota(
      const AConstraint: TGraphValueQuotaConstraint): TGraph;
    function RemoveValueQuota(const ALabel: String): TGraph;
    function ClearValueQuotas: TGraph;
    function CopyValueQuotaConstraints: TGraphValueQuotaConstraints;

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

    (*
      solves the complete pipeline with bounded chronological backtracking
      over exact completed pass assignments. This opt-in search can reopen an
      earlier defined pass when a later pass proves its output incompatible.
      Attempts are transactional and the first complete success commits once.
    *)
    function TrySolveNegotiated(const AOptions: TGraphNegotiationOptions;
      out AReport: TGraphNegotiationReport): Boolean;

    //Restarts only a local solver backtrack limit, never a contradiction or
    //pass-negotiation limit. Public Seed stays the base seed; effective seeds
    //are recorded in each attempt. Direct winning-seed replay through an
    //ordinary solve also requires hooks independent of the public base Seed;
    //whole-policy replay retains that base and has no such restriction.
    function TrySolveRestarted(const AOptions: TGraphSolveOptions;
      const ARestarts: TGraphRestartOptions;
      out AReport: TGraphRestartReport): Boolean;
    function TrySolveNegotiatedRestarted(
      const AOptions: TGraphNegotiationOptions;
      const ARestarts: TGraphRestartOptions;
      out AReport: TGraphRestartReport): Boolean;

    function TryRegenerateFrom(const APass: String;
      const AOptions: TGraphSolveOptions;
      out AReport: TGraphSolveReport): Boolean; overload;
    function TryRegenerateFrom(const APasses: TGraphPassLabels;
      const AOptions: TGraphSolveOptions;
      out AReport: TGraphSolveReport): Boolean; overload;

    (*
      negotiates only within the exact descendant closure of the named roots.
      The roots are the earliest passes the caller authorizes to change; no
      ancestor is added implicitly, and every pass outside the closure is
      reused as an immutable input with its random stream preserved
    *)
    function TryRegenerateNegotiatedFrom(const APass: String;
      const AOptions: TGraphNegotiationOptions;
      out AReport: TGraphSelectiveNegotiationReport): Boolean; overload;
    function TryRegenerateNegotiatedFrom(const APasses: TGraphPassLabels;
      const AOptions: TGraphNegotiationOptions;
      out AReport: TGraphSelectiveNegotiationReport): Boolean; overload;

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
  function MakeGraphPassPointQuery(const AOffset: TGraphOffset;
    const AValues: TGraphValues): TGraphPassMapQuery;
  function MakeGraphPassCellQuery(const AValues: TGraphValues):
    TGraphPassMapQuery; overload;
  function MakeGraphPassCellQuery(const AOffset: TGraphOffset;
    const AValues: TGraphValues): TGraphPassMapQuery; overload;
  function MakeGraphPassRegionQuery(const AMinimumOffset,
    AMaximumOffset: TGraphOffset; const AValues: TGraphValues):
    TGraphPassMapQuery;
  function MakeGraphPassCountQuery(const AQuery: TGraphPassMapQuery;
    const AMinimum, AMaximum: Integer): TGraphPassMapQuery;
  { Pure preflight for tools and portable recipe compilers. Neither routine
    allocates a graph or installs a clause. Normalization owns its values;
    range validation checks all consumer anchors, not provider coverage. }
  function NormalizeGraphPassMapQuery(const AQuery: TGraphPassMapQuery):
    TGraphPassMapQuery;
  procedure ValidateGraphPassMappedQuery(const AConsumerLayout: TWfcLatticeLayout;
    const AQuery: TGraphPassMapQuery);
  function MakeGraphConnectivityValue(const AValue: TGraphValue;
    const AOpenings: TGraphDirections;
    const ARequiredByValue: Boolean = False): TGraphConnectivityValue;
  function MakeGraphConnectivityConstraint(const ALabel: String;
    const ARoot: TGraphPosition; const ARequiredPositions: TGraphPositions;
    const AValues: TGraphConnectivityValues;
    const ARequireAllParticipants: Boolean = False):
    TGraphConnectivityConstraint;
  function MakeGraphValueQuotaConstraint(const ALabel: String;
    const AValues: TGraphValues; const AMinimumCount,
    AMaximumCount: Integer): TGraphValueQuotaConstraint;
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
  function DefaultGraphNegotiationOptions: TGraphNegotiationOptions;
  function DefaultGraphRestartOptions: TGraphRestartOptions;
  function DeriveGraphRestartSeed(const ABase: TGraphSeed;
    const AIndex: Integer): TGraphSeed;
  function GraphRestartBacktrackBudget(
    const AInitialBacktracks, AIndex: Integer;
    const ARestarts: TGraphRestartOptions): Integer;
  //Timing fields and MeasureTime are deliberately excluded.
  function CalculateGraphRestartTranscriptHash(
    const AOptions: TGraphSolveOptions;
    const ARestarts: TGraphRestartOptions;
    const AReport: TGraphRestartReport): TGraphTraceSignature; overload;
  function CalculateGraphRestartTranscriptHash(
    const AOptions: TGraphNegotiationOptions;
    const ARestarts: TGraphRestartOptions;
    const AReport: TGraphRestartReport): TGraphTraceSignature; overload;
  //Recomputes the portable signature from report metadata and numeric trace
  //events. Returns zero when trace capture is disabled.
  function CalculateGraphTraceHash(
    const AReport: TGraphSolveReport): TGraphTraceSignature;
  function CalculateGraphNegotiationTranscriptHash(
    const AOptions: TGraphNegotiationOptions;
    const AReport: TGraphNegotiationReport): TGraphTraceSignature;
  function CalculateGraphSelectiveNegotiationTranscriptHash(
    const AOptions: TGraphNegotiationOptions;
    const AReport: TGraphSelectiveNegotiationReport):
    TGraphTraceSignature;

const
  AllDirections : TGraphDirections = [gdNorth, gdEast, gdSouth, gdWest, gdUp, gdDown];

implementation

uses
  wfc_solver_reference, wfc_timing;

type
  TGraphTraversalFrame = record
    Entry: TGraphEntry;
    NextDirection: Integer;
  end;

  TGraphValueMatrix = array of TGraphValues;
  TGraphTraceCauseArray = array of TGraphTraceCauseKind;
  TGraphStreamCause = record
    EventId: Integer;
    Kind: TGraphTraceCauseKind;
    DependencyPassIndex: Integer;
    ConstraintIndex: Integer;
  end;
  TGraphStreamCauses = array of TGraphStreamCause;

  //One attempt owns this recorder. Streaming retains causal summaries per
  //cell and per pass, never an event-index map or a hidden full event buffer.
  TGraphTraceRecorder = class
  private
    FCapture: Boolean;
    FEnabled: Boolean;
    FSink: TGraphTraceSink;
    FHeader: TGraphTraceHeader;
    FDelivery: TGraphTraceDelivery;
    FEvents: TGraphTraceEvents;
    FCount: Integer;
    FHash: TGraphTraceSignature;
    FStarts: TGraphPassIndices;
    FCounts: TGraphPassIndices;
    FLastPassEvents: TGraphPassIndices;
    FPassIndex: Integer;
    FPassBeginEvent: Integer;
    FLocalBase: Integer;
    FValues: TGraphValues;
    FInitialCauses: TGraphTraceCauseArray;
    FInitialDependencies: TReferenceIntegerArray;
    FCellCauses: TGraphStreamCauses;
    FBacktrackCause: TGraphStreamCause;
    procedure SinkFailed(const APhase: TGraphTraceDeliveryPhase;
      const AEventId: Integer; const AMessage: String);
  public
    constructor Create(const ACapture: Boolean; const ASink: TGraphTraceSink;
      const AReport: TGraphSolveReport);
    procedure BeginDelivery;
    function Append(const ASource: TGraphTraceEvent): Integer;
    procedure ReserveTerminal;
    function LastPassEvent(const APassIndex: Integer): Integer;
    procedure ConfigurePass(const APassIndex, APassBeginEvent,
      ACellCount: Integer; const AValues: TGraphValues;
      const AInitialCauses: TGraphTraceCauseArray;
      const AInitialDependencies: TReferenceIntegerArray);
    procedure ReceiveReferenceEvent(const AEvent: TReferenceTraceEvent);
    procedure Finish(var AReport: TGraphSolveReport;
      const AInterrupted: Boolean);
    property Enabled: Boolean read FEnabled;
    property EventCount: Integer read FCount;
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

function MakeGraphConnectivityValue(const AValue: TGraphValue;
  const AOpenings: TGraphDirections;
  const ARequiredByValue: Boolean): TGraphConnectivityValue;
begin
  Result.Value := AValue;
  Result.Openings := AOpenings;
  Result.RequiredByValue := ARequiredByValue;
end;

function MakeGraphConnectivityConstraint(const ALabel: String;
  const ARoot: TGraphPosition; const ARequiredPositions: TGraphPositions;
  const AValues: TGraphConnectivityValues;
  const ARequireAllParticipants: Boolean): TGraphConnectivityConstraint;
begin
  Result.LabelText := ALabel;
  Result.Root := ARoot;
  Result.RequiredPositions := Copy(ARequiredPositions, 0,
    Length(ARequiredPositions));
  Result.Values := Copy(AValues, 0, Length(AValues));
  Result.RequireAllParticipants := ARequireAllParticipants;
end;

function MakeGraphValueQuotaConstraint(const ALabel: String;
  const AValues: TGraphValues; const AMinimumCount,
  AMaximumCount: Integer): TGraphValueQuotaConstraint;
begin
  Result.LabelText := ALabel;
  Result.Values := CloneGraphValues(AValues);
  Result.MinimumCount := AMinimumCount;
  Result.MaximumCount := AMaximumCount;
end;

procedure CheckGraphValueQuotaBounds(const AMinimumCount,
  AMaximumCount: Integer);
begin
  //Positive comparisons also reject nonfinite/undefined browser numbers.
  if not ((AMinimumCount >= 0) and (AMinimumCount <= High(Integer))
    and (AMaximumCount >= AMinimumCount)
    and (AMaximumCount <= High(Integer))) then
    raise ERangeError.Create(
      'RequireValueQuota::bounds must satisfy 0 <= minimum <= maximum <= High(Integer)');
  {$IFDEF PAS2JS}
  //Strict inequality rejects numeric strings as well as fractional numbers.
  if (AMinimumCount <> Trunc(AMinimumCount))
    or (AMaximumCount <> Trunc(AMaximumCount)) then
    raise ERangeError.Create('RequireValueQuota::bounds must be exact integers');
  {$ENDIF}
end;

function GraphValueQuotaEqual(const ALeft,
  ARight: TGraphValueQuotaConstraint): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (ALeft.LabelText <> ARight.LabelText)
    or (ALeft.MinimumCount <> ARight.MinimumCount)
    or (ALeft.MaximumCount <> ARight.MaximumCount)
    or (Length(ALeft.Values) <> Length(ARight.Values)) then
    Exit;
  for I := 0 to High(ALeft.Values) do
    if ALeft.Values[I] <> ARight.Values[I] then Exit;
  Result := True;
end;

procedure CheckConnectivityPosition(const APosition: TGraphPosition;
  const AWidth, AHeight, ADepth: TGraphCoordinate);

  procedure CheckAxis(const AValue, ASize: TGraphCoordinate);
  begin
    if not ((AValue >= 0) and (AValue < ASize)
      and (AValue <= TGraphCoordinate(High(Integer)))) then
      raise ERangeError.Create('connectivity::position is outside the graph');
    {$IFDEF PAS2JS}
    if AValue <> Trunc(AValue) then
      raise ERangeError.Create('connectivity::coordinates must be exact integers');
    {$ENDIF}
  end;
begin
  CheckAxis(APosition.X, AWidth);
  CheckAxis(APosition.Y, AHeight);
  CheckAxis(APosition.Z, ADepth);
end;

function GraphPositionsEqual(const ALeft, ARight: TGraphPosition): Boolean;
begin
  Result := (ALeft.X = ARight.X) and (ALeft.Y = ARight.Y)
    and (ALeft.Z = ARight.Z);
end;

function GraphConnectivityEqual(const ALeft,
  ARight: TGraphConnectivityConstraint): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (ALeft.LabelText <> ARight.LabelText)
    or not GraphPositionsEqual(ALeft.Root, ARight.Root)
    or (ALeft.RequireAllParticipants <> ARight.RequireAllParticipants)
    or (Length(ALeft.Values) <> Length(ARight.Values))
    or (Length(ALeft.RequiredPositions) <> Length(ARight.RequiredPositions)) then
    Exit;
  for I := 0 to High(ALeft.Values) do
    if (ALeft.Values[I].Value <> ARight.Values[I].Value)
      or (ALeft.Values[I].Openings <> ARight.Values[I].Openings)
      or (ALeft.Values[I].RequiredByValue <> ARight.Values[I].RequiredByValue) then
      Exit;
  for I := 0 to High(ALeft.RequiredPositions) do
    if not GraphPositionsEqual(ALeft.RequiredPositions[I],
      ARight.RequiredPositions[I]) then Exit;
  Result := True;
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

function LegacyGraphLayout(const AWidth, AHeight,
  ADepth: TGraphCoordinate; const AWrap: Boolean): TWfcLatticeLayout;
begin
  Result := Default(TWfcLatticeLayout);
  Result.Pitch := MakeWfcLatticeVector(1, 1, 1);
  Result.Wrap := AWrap;
  //The historical unshaped/zero-size graph remains legal. Its layout is an
  //empty sentinel and cannot participate in mapped queries until configured.
  if (AWidth > 0) and (AHeight > 0) and (ADepth > 0) then
    Result.Cells := MakeWfcLatticeVector(Integer(AWidth),
      Integer(AHeight), Integer(ADepth));
end;

function GraphLayoutsEqual(const A, B: TWfcLatticeLayout): Boolean;
begin
  //Unlike the public validated lattice helper this also compares the legacy
  //empty sentinel, permitting rule registration before the first Reshape.
  Result := (A.Cells.X = B.Cells.X) and (A.Cells.Y = B.Cells.Y)
    and (A.Cells.Z = B.Cells.Z) and (A.Origin.X = B.Origin.X)
    and (A.Origin.Y = B.Origin.Y) and (A.Origin.Z = B.Origin.Z)
    and (A.Pitch.X = B.Pitch.X) and (A.Pitch.Y = B.Pitch.Y)
    and (A.Pitch.Z = B.Pitch.Z) and (A.Wrap = B.Wrap);
end;

function CanonicalGraphPassMapQuery(const AQuery: TGraphPassMapQuery):
  TGraphPassMapQuery;
var
  I: Integer;
  {$IFDEF PAS2JS}LValid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    var record = function(v) { return v !== null && typeof v === 'object' && !Array.isArray(v); };
    LValid = record(AQuery) && record(AQuery.MinimumOffset) && record(AQuery.MaximumOffset)
      && Array.isArray(AQuery.Values);
    if (LValid) {
      for (var i = 0; i < AQuery.Values.length; i++)
        if (typeof AQuery.Values[i] !== 'string') { LValid = false; break; }
    }
  end;
  if not LValid then
    raise EArgumentException.Create('RequireMappedFromPass::query must contain offsets and a string values array');
  {$ENDIF}
  //Validate raw records too: pas2js callers can otherwise supply fractional,
  //nonfinite or string-valued numbers through a typed JavaScript boundary.
  MakeWfcLatticeVector(Ord(AQuery.Kind), Ord(AQuery.Match), 0);
  if (Ord(AQuery.Kind) < Ord(Low(TGraphPassMapKind)))
    or (Ord(AQuery.Kind) > Ord(High(TGraphPassMapKind)))
    or (Ord(AQuery.Match) < Ord(Low(TGraphPassMapMatch)))
    or (Ord(AQuery.Match) > Ord(High(TGraphPassMapMatch))) then
    raise ERangeError.Create('RequireMappedFromPass::invalid query kind or match');
  MakeWfcLatticeVector(AQuery.MinimumOffset.DeltaX,
    AQuery.MinimumOffset.DeltaY, AQuery.MinimumOffset.DeltaZ);
  MakeWfcLatticeVector(AQuery.MaximumOffset.DeltaX,
    AQuery.MaximumOffset.DeltaY, AQuery.MaximumOffset.DeltaZ);
  CheckGraphValueQuotaBounds(AQuery.MinimumMatches, AQuery.MaximumMatches);
  if AQuery.Kind = gpmkRegionCoverage then
  begin
    if (AQuery.MaximumOffset.DeltaX <= AQuery.MinimumOffset.DeltaX)
      or (AQuery.MaximumOffset.DeltaY <= AQuery.MinimumOffset.DeltaY)
      or (AQuery.MaximumOffset.DeltaZ <= AQuery.MinimumOffset.DeltaZ) then
      raise ERangeError.Create('RequireMappedFromPass::region must have positive extent');
  end
  else if not IsZeroGraphOffset(AQuery.MaximumOffset) then
    raise EArgumentException.Create('RequireMappedFromPass::unused maximum offset must be zero');
  if AQuery.Match = gpmmAll then
  begin
    if (AQuery.MinimumMatches <> 0) or (AQuery.MaximumMatches <> 0) then
      raise EArgumentException.Create('RequireMappedFromPass::all-match count fields must be zero');
  end
  else if (AQuery.Kind = gpmkPoint) and (AQuery.MaximumMatches > 1) then
    raise ERangeError.Create('RequireMappedFromPass::point count cannot exceed one');
  if Length(AQuery.Values) = 0 then
    raise EArgumentException.Create('RequireMappedFromPass::values cannot be empty');
  Result := AQuery;
  Result.Values := nil;
  for I := 0 to High(AQuery.Values) do
  begin
    if AQuery.Values[I] = TGraphValue.Empty then
      raise EArgumentException.Create('RequireMappedFromPass::value cannot be empty');
    if not ContainsGraphValue(Result.Values, AQuery.Values[I]) then
      Insert(AQuery.Values[I], Result.Values, Length(Result.Values));
  end;
end;

function GraphPassMapQueriesEqual(const ALeft,
  ARight: TGraphPassMapQuery): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (ALeft.Kind <> ARight.Kind) or (ALeft.Match <> ARight.Match)
    or not GraphOffsetsEqual(ALeft.MinimumOffset, ARight.MinimumOffset)
    or not GraphOffsetsEqual(ALeft.MaximumOffset, ARight.MaximumOffset)
    or (ALeft.MinimumMatches <> ARight.MinimumMatches)
    or (ALeft.MaximumMatches <> ARight.MaximumMatches)
    or (Length(ALeft.Values) <> Length(ARight.Values)) then Exit;
  for I := 0 to High(ALeft.Values) do
    if not ContainsGraphValue(ARight.Values, ALeft.Values[I]) then Exit;
  Result := True;
end;

procedure ValidateGraphPassMapRange(const ALayout: TWfcLatticeLayout;
  const AQuery: TGraphPassMapQuery);

  procedure CheckAxis(const AOrigin, ACells, APitch,
    AMinimum, AMaximum: Integer);
  var
    LFirst, LLast, LMaximum: Double;
  begin
    //Layout validation has bounded this product to at most 2^32-1.
    //Widen before every operation; never form a signed-32 intermediate.
    LFirst := AOrigin;
    LFirst := LFirst + AMinimum;
    LLast := ACells - 1;
    LLast := AOrigin + LLast * APitch;
    if AQuery.Kind = gpmkCellCoverage then
      LMaximum := LLast + AMinimum + APitch
    else if AQuery.Kind = gpmkRegionCoverage then
      LMaximum := LLast + AMaximum
    else
      LMaximum := LLast + AMinimum;
    if (LFirst < Low(Integer)) or (LMaximum > High(Integer)) then
      raise ERangeError.Create('RequireMappedFromPass::query exceeds signed world coordinates');
  end;
begin
  ValidateWfcLatticeLayout(ALayout);
  CheckAxis(ALayout.Origin.X, ALayout.Cells.X, ALayout.Pitch.X,
    AQuery.MinimumOffset.DeltaX, AQuery.MaximumOffset.DeltaX);
  CheckAxis(ALayout.Origin.Y, ALayout.Cells.Y, ALayout.Pitch.Y,
    AQuery.MinimumOffset.DeltaY, AQuery.MaximumOffset.DeltaY);
  CheckAxis(ALayout.Origin.Z, ALayout.Cells.Z, ALayout.Pitch.Z,
    AQuery.MinimumOffset.DeltaZ, AQuery.MaximumOffset.DeltaZ);
end;

function MakeGraphPassPointQuery(const AOffset: TGraphOffset;
  const AValues: TGraphValues): TGraphPassMapQuery;
begin
  Result := Default(TGraphPassMapQuery);
  Result.Kind := gpmkPoint;
  Result.Match := gpmmAll;
  Result.MinimumOffset := AOffset;
  Result.Values := AValues;
  Result := CanonicalGraphPassMapQuery(Result);
end;

function NormalizeGraphPassMapQuery(const AQuery: TGraphPassMapQuery):
  TGraphPassMapQuery;
begin
  Result := CanonicalGraphPassMapQuery(AQuery);
end;

procedure ValidateGraphPassMappedQuery(const AConsumerLayout: TWfcLatticeLayout;
  const AQuery: TGraphPassMapQuery);
var
  LCanonical: TGraphPassMapQuery;
begin
  LCanonical := CanonicalGraphPassMapQuery(AQuery);
  ValidateGraphPassMapRange(AConsumerLayout, LCanonical);
end;

function MakeGraphPassCellQuery(const AOffset: TGraphOffset;
  const AValues: TGraphValues): TGraphPassMapQuery;
begin
  Result := MakeGraphPassPointQuery(AOffset, AValues);
  Result.Kind := gpmkCellCoverage;
end;

function MakeGraphPassCellQuery(const AValues: TGraphValues):
  TGraphPassMapQuery;
begin
  Result := MakeGraphPassCellQuery(MakeGraphOffset(0, 0, 0), AValues);
end;

function MakeGraphPassRegionQuery(const AMinimumOffset,
  AMaximumOffset: TGraphOffset; const AValues: TGraphValues):
  TGraphPassMapQuery;
begin
  Result := Default(TGraphPassMapQuery);
  Result.Kind := gpmkRegionCoverage;
  Result.Match := gpmmAll;
  Result.MinimumOffset := AMinimumOffset;
  Result.MaximumOffset := AMaximumOffset;
  Result.Values := AValues;
  Result := CanonicalGraphPassMapQuery(Result);
end;

function MakeGraphPassCountQuery(const AQuery: TGraphPassMapQuery;
  const AMinimum, AMaximum: Integer): TGraphPassMapQuery;
begin
  Result := CanonicalGraphPassMapQuery(AQuery);
  Result.Match := gpmmCount;
  Result.MinimumMatches := AMinimum;
  Result.MaximumMatches := AMaximum;
  Result := CanonicalGraphPassMapQuery(Result);
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

function CanonicalGraphPassCountTerms(const ATerms: TGraphPassMatchTerms;
  const AMinimum, AMaximum: Integer;
  const AMode: TGraphPassCountMode): TGraphPassMatchTerms;
var
  LModeOrdinal: Integer;
begin
  Result := CanonicalGraphPassMatchTerms(ATerms, 'RequireCountFromPass');
  if (AMinimum < 0) or (AMaximum < AMinimum)
    or (AMaximum > Length(Result)) then
    raise ERangeError.CreateFmt(
      'RequireCountFromPass::bounds must satisfy 0 <= minimum <= maximum <= %d [%d..%d]',
      [Length(Result), AMinimum, AMaximum]);
  LModeOrdinal := Ord(AMode);
  if (LModeOrdinal < Ord(Low(TGraphPassCountMode)))
    or (LModeOrdinal > Ord(High(TGraphPassCountMode))) then
    raise ERangeError.Create('RequireCountFromPass::invalid count mode');
end;

function GraphPassCountTermsEqual(const ALeft,
  ARight: TGraphPassMatchTerms): Boolean;
var
  I, J: Integer;
begin
  //Both inputs are canonical offset-sorted terms with duplicate-free values.
  //Count-clause identity treats those values as sets without changing the
  //historical insertion-order identity of RequireAnyFromPass clauses.
  if Length(ALeft) <> Length(ARight) then
    Exit(False);
  for I := 0 to High(ALeft) do
  begin
    if (not GraphOffsetsEqual(ALeft[I].Offset, ARight[I].Offset))
      or (Length(ALeft[I].Values) <> Length(ARight[I].Values)) then
      Exit(False);
    for J := 0 to High(ALeft[I].Values) do
      if not ContainsGraphValue(ARight[I].Values, ALeft[I].Values[J]) then
        Exit(False);
  end;
  Result := True;
end;

procedure RecordGraphPassCountMatch(const AMode: TGraphPassCountMode;
  const AResolvedIndex: Integer; var ACount: Integer;
  var AMatchedIndices: TGraphPassIndices);
var
  I: Integer;
begin
  if AMode = gpcmDistinctCells then
  begin
    //Only matching terms reach this helper. A nonmatching alias must not
    //prevent a later matching term at the same resolved cell from counting.
    for I := 0 to Pred(ACount) do
      if AMatchedIndices[I] = AResolvedIndex then
        Exit;
    AMatchedIndices[ACount] := AResolvedIndex;
  end;
  Inc(ACount);
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

function DefaultGraphNegotiationOptions: TGraphNegotiationOptions;
begin
  Result.SolveOptions := DefaultGraphSolveOptions;
  Result.MaxPassBacktracks := 64;
end;

function DefaultGraphRestartOptions: TGraphRestartOptions;
begin
  Result.MaxRestarts := 0;
  Result.MaxBacktracksPerAttempt := High(Integer);
  Result.Schedule := grschFixed;
  Result.MeasureTime := False;
end;

function GraphRestartBacktrackBudget(
  const AInitialBacktracks, AIndex: Integer;
  const ARestarts: TGraphRestartOptions): Integer;
var
  I: Integer;
begin
  if (ARestarts.MaxRestarts < 0)
    or (ARestarts.MaxRestarts = High(Integer)) then
    raise ERangeError.Create('Restart::invalid maximum restarts');
  if (Ord(ARestarts.Schedule) < Ord(Low(TGraphRestartSchedule)))
    or (Ord(ARestarts.Schedule) > Ord(High(TGraphRestartSchedule))) then
    raise ERangeError.Create('Restart::invalid schedule');
  if (AInitialBacktracks < 0)
    or (ARestarts.MaxBacktracksPerAttempt < AInitialBacktracks) then
    raise ERangeError.Create('Restart::invalid initial backtracks or cap');
  if (AIndex < 0) or (AIndex > ARestarts.MaxRestarts) then
    raise ERangeError.Create('Restart::attempt index is outside the policy');
  Result := AInitialBacktracks;
  if (ARestarts.Schedule = grschFixed) or (Result = 0) then
    Exit;
  I := 0;
  //At most one iteration per integer bit, even for a very large index.
  while (I < AIndex) and (Result < ARestarts.MaxBacktracksPerAttempt) do
  begin
    if Result > ARestarts.MaxBacktracksPerAttempt - Result then
      Result := ARestarts.MaxBacktracksPerAttempt
    else
      Result := Result + Result;
    Inc(I);
  end;
end;

function DeriveGraphRestartSeed(const ABase: TGraphSeed;
  const AIndex: Integer): TGraphSeed;

  function MultiplyLow32(const A, B: Cardinal): Cardinal;
  var
    LProduct, LHigh: Cardinal;
  begin
    LProduct := (A and $FFFF) * (B and $FFFF);
    LHigh := (LProduct shr 16)
      + (((A shr 16) * (B and $FFFF)) and $FFFF)
      + (((B shr 16) * (A and $FFFF)) and $FFFF);
    Result := ((LHigh and $FFFF) shl 16) or (LProduct and $FFFF);
  end;

begin
  if AIndex < 0 then
    raise ERangeError.Create('Restart::seed index cannot be negative');
  if AIndex = 0 then
    Exit(ABase);
  //The same fmix32 avalanche used by scalar seed expansion, with explicit
  //low-word multiplication for checked native and browser parity.
  Result := Cardinal(AIndex);
  Result := Result xor (Result shr 16);
  Result := MultiplyLow32(Result, $85EBCA6B);
  Result := Result xor (Result shr 13);
  Result := MultiplyLow32(Result, $C2B2AE35);
  Result := ABase xor Result xor (Result shr 16);
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
  if AEvent.CauseKind = gtckConnectivity then
  begin
    GraphTraceHashCardinal(AHash, WFC_GRAPH_CONNECTIVITY_VERSION);
    GraphTraceHashInteger(AHash, AEvent.ConstraintIndex);
  end;
  if AEvent.CauseKind = gtckValueQuota then
  begin
    GraphTraceHashCardinal(AHash, WFC_GRAPH_VALUE_QUOTA_VERSION);
    GraphTraceHashInteger(AHash, AEvent.ConstraintIndex);
  end;
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

constructor TGraphTraceRecorder.Create(const ACapture: Boolean;
  const ASink: TGraphTraceSink; const AReport: TGraphSolveReport);
var
  I: Integer;
begin
  inherited Create;
  FCapture := ACapture;
  FSink := ASink;
  FEnabled := ACapture or Assigned(ASink);
  FDelivery := Default(TGraphTraceDelivery);
  FDelivery.Version := WFC_TRACE_DELIVERY_VERSION;
  FDelivery.FailureEventId := -1;
  if Assigned(ASink) then
    FDelivery.Status := gtdsComplete;
  FHeader.DeliveryVersion := WFC_TRACE_DELIVERY_VERSION;
  FHeader.TraceVersion := WFC_TRACE_VERSION;
  FHeader.TraceHashVersion := WFC_TRACE_HASH_VERSION;
  FHeader.Seed := AReport.Seed;
  FHeader.RandomAlgorithmVersion := AReport.RandomAlgorithmVersion;
  FHeader.SolverAlgorithmVersion := AReport.SolverAlgorithmVersion;
  FHeader.GraphModelVersion := AReport.GraphModelVersion;
  FHeader.PipelineAlgorithmVersion := AReport.PipelineAlgorithmVersion;
  FHeader.PassCount := Length(AReport.Passes);
  if not FEnabled then
    Exit;
  SetLength(FStarts, FHeader.PassCount);
  SetLength(FCounts, FHeader.PassCount);
  SetLength(FLastPassEvents, FHeader.PassCount);
  for I := 0 to Pred(FHeader.PassCount) do
  begin
    FStarts[I] := -1;
    FLastPassEvents[I] := -1;
  end;
  FHash := Cardinal(2166136261);
  GraphTraceHashText(FHash, 'wfc-graph-trace');
  GraphTraceHashCardinal(FHash, WFC_TRACE_VERSION);
  GraphTraceHashCardinal(FHash, WFC_TRACE_HASH_VERSION);
  GraphTraceHashCardinal(FHash, AReport.Seed);
  GraphTraceHashCardinal(FHash, Cardinal(AReport.RandomAlgorithmVersion));
  GraphTraceHashCardinal(FHash, Cardinal(AReport.SolverAlgorithmVersion));
  GraphTraceHashCardinal(FHash, Cardinal(AReport.GraphModelVersion));
  GraphTraceHashCardinal(FHash, Cardinal(AReport.PipelineAlgorithmVersion));
  GraphTraceHashCardinal(FHash, Cardinal(FHeader.PassCount));
end;

procedure TGraphTraceRecorder.SinkFailed(
  const APhase: TGraphTraceDeliveryPhase; const AEventId: Integer;
  const AMessage: String);
begin
  FSink := nil;
  FDelivery.Status := gtdsSinkFailed;
  FDelivery.FailurePhase := APhase;
  FDelivery.FailureEventId := AEventId;
  FDelivery.FailureMessage := AMessage;
end;

procedure TGraphTraceRecorder.BeginDelivery;
begin
  if not Assigned(FSink) then
    Exit;
  try
    FSink.BeginTrace(FHeader);
  except
    on E: Exception do SinkFailed(gtdpBegin, -1, E.Message);
    else SinkFailed(gtdpBegin, -1, 'non-Pascal observer exception');
  end;
end;

function TGraphTraceRecorder.Append(
  const ASource: TGraphTraceEvent): Integer;
var
  LCapacity: Integer;
  LEvent: TGraphTraceEvent;
begin
  Result := -1;
  if not FEnabled then
    Exit;
  if FCount = High(Integer) then
    raise ERangeError.Create('TrySolve::trace event identity is too large');
  if FCapture and (FCount = Length(FEvents)) then
  begin
    LCapacity := Length(FEvents);
    if LCapacity < 64 then
      LCapacity := 64
    else if LCapacity > High(Integer) div 2 then
      LCapacity := High(Integer)
    else
      LCapacity := LCapacity * 2;
    SetLength(FEvents, LCapacity);
  end;
  LEvent := ASource;
  LEvent.EventId := FCount;
  if (LEvent.PassIndex >= 0) and (LEvent.PassIndex < Length(FCounts)) then
  begin
    if FCounts[LEvent.PassIndex] = 0 then
      FStarts[LEvent.PassIndex] := FCount;
    Inc(FCounts[LEvent.PassIndex]);
    FLastPassEvents[LEvent.PassIndex] := FCount;
  end;
  if FCapture then
    FEvents[FCount] := LEvent;
  Result := FCount;
  Inc(FCount);
  MixGraphTraceEvent(FHash, LEvent);
  if FDelivery.Status <> gtdsDisabled then
  begin
    FDelivery.ProducedEventCount := FCount;
    FDelivery.TraceHash := FHash;
  end;
  if not Assigned(FSink) then
    Exit;
  try
    FSink.AppendEvent(LEvent);
    Inc(FDelivery.DeliveredEventCount);
  except
    on E: Exception do SinkFailed(gtdpEvent, LEvent.EventId, E.Message);
    else SinkFailed(gtdpEvent, LEvent.EventId, 'non-Pascal observer exception');
  end;
end;

procedure TGraphTraceRecorder.ReserveTerminal;
begin
  if not FEnabled then
    Exit;
  if FCount = High(Integer) then
    raise ERangeError.Create('TrySolve::trace event identity is too large');
  if FCapture then
    SetLength(FEvents, Succ(FCount));
end;

function TGraphTraceRecorder.LastPassEvent(const APassIndex: Integer): Integer;
begin
  Result := -1;
  if (APassIndex >= 0) and (APassIndex < Length(FLastPassEvents)) then
    Result := FLastPassEvents[APassIndex];
end;

procedure TGraphTraceRecorder.ConfigurePass(const APassIndex,
  APassBeginEvent, ACellCount: Integer; const AValues: TGraphValues;
  const AInitialCauses: TGraphTraceCauseArray;
  const AInitialDependencies: TReferenceIntegerArray);
var
  I: Integer;
begin
  if not FEnabled then
    Exit;
  FPassIndex := APassIndex;
  FPassBeginEvent := APassBeginEvent;
  FLocalBase := FCount;
  FValues := AValues;
  FInitialCauses := AInitialCauses;
  FInitialDependencies := AInitialDependencies;
  SetLength(FCellCauses, ACellCount);
  for I := 0 to High(FCellCauses) do
    FCellCauses[I].EventId := -1;
  FBacktrackCause.EventId := -1;
end;

procedure TGraphTraceRecorder.ReceiveReferenceEvent(
  const AEvent: TReferenceTraceEvent);
var
  LEvent: TGraphTraceEvent;
  LCause: TGraphStreamCause;
  LInitialIndex: Integer;
begin
  if not FEnabled then
    Exit;
  if AEvent.EventId <> FCount - FLocalBase then
    raise EInvalidOperation.Create('TrySolve::reference stream is not consecutive');
  LEvent := Default(TGraphTraceEvent);
  LEvent.EventId := -1;
  LEvent.CauseEventId := -1;
  LEvent.DependencyPassIndex := -1;
  LEvent.PassIndex := FPassIndex;
  LEvent.EntryIndex := AEvent.EntryIndex;
  LEvent.ValueIndex := AEvent.ValueIndex;
  LEvent.NeighborIndex := AEvent.NeighborIndex;
  LEvent.ConstraintIndex := AEvent.ConstraintIndex;
  case AEvent.Kind of
    rtekInitialCandidateRemoved: LEvent.Kind := gtekInitialCandidateRemoved;
    rtekDecision: LEvent.Kind := gtekDecision;
    rtekCandidateRemoved: LEvent.Kind := gtekCandidateRemoved;
    rtekContradiction: LEvent.Kind := gtekContradiction;
    rtekBacktrack: LEvent.Kind := gtekBacktrack;
    rtekCandidateRestored: LEvent.Kind := gtekCandidateRestored;
    rtekSolved: LEvent.Kind := gtekPassStaged;
  else
    raise ERangeError.Create('TrySolve::invalid reference trace event');
  end;
  case AEvent.CauseKind of
    rtckNone: LEvent.CauseKind := gtckNone;
    rtckInitialDomain: LEvent.CauseKind := gtckCallerDomain;
    rtckLock: LEvent.CauseKind := gtckCallerLock;
    rtckDecision: LEvent.CauseKind := gtckDecision;
    rtckAdjacency: LEvent.CauseKind := gtckAdjacency;
    rtckRequiredSupport: LEvent.CauseKind := gtckRequiredSupport;
    rtckBacktrack: LEvent.CauseKind := gtckBacktrack;
    rtckFinalValidation: LEvent.CauseKind := gtckFinalValidation;
    rtckExcludedAssignment: LEvent.CauseKind := gtckExactAssignmentExclusion;
    rtckConnectivity: LEvent.CauseKind := gtckConnectivity;
    rtckValueQuota: LEvent.CauseKind := gtckValueQuota;
  else
    raise ERangeError.Create('TrySolve::invalid reference trace cause');
  end;
  if (AEvent.CauseEventId >= 0) and (AEvent.CauseEventId < AEvent.EventId) then
    LEvent.CauseEventId := FLocalBase + AEvent.CauseEventId;
  if (AEvent.ValueIndex >= 0) and (AEvent.ValueIndex < Length(FValues)) then
    LEvent.Value := FValues[AEvent.ValueIndex];
  LEvent.HasDirection := (AEvent.Direction >= Ord(Low(TGraphDirection)))
    and (AEvent.Direction <= Ord(High(TGraphDirection)));
  if LEvent.HasDirection then
    LEvent.Direction := TGraphDirection(AEvent.Direction);
  if AEvent.DecisionDepth >= 0 then
    LEvent.DecisionDepth := AEvent.DecisionDepth;
  if AEvent.DomainCountBefore >= 0 then
    LEvent.DomainCountBefore := AEvent.DomainCountBefore;
  if AEvent.DomainCountAfter >= 0 then
    LEvent.DomainCountAfter := AEvent.DomainCountAfter;
  if AEvent.Kind = rtekInitialCandidateRemoved then
  begin
    LInitialIndex := AEvent.EntryIndex * Length(FValues) + AEvent.ValueIndex;
    if (LInitialIndex >= 0) and (LInitialIndex < Length(FInitialCauses)) then
    begin
      LEvent.CauseKind := FInitialCauses[LInitialIndex];
      LEvent.DependencyPassIndex := FInitialDependencies[LInitialIndex];
      if LEvent.CauseKind = gtckPassDependency then
      begin
        LEvent.CauseEventId := LastPassEvent(LEvent.DependencyPassIndex);
        if LEvent.CauseEventId < 0 then
          LEvent.CauseEventId := FPassBeginEvent;
      end;
    end;
  end;
  //Only decisions and initial contradictions inherit a refined public cause.
  //Their causes are the last change of this cell or the retained backtrack
  //summary; arbitrary history lookup is neither needed nor retained.
  if (LEvent.CauseEventId >= 0) and ((AEvent.Kind = rtekDecision) or
    ((AEvent.Kind = rtekContradiction) and
      (AEvent.CauseKind in [rtckInitialDomain, rtckLock]))) then
  begin
    LCause.EventId := -1;
    if (AEvent.EntryIndex >= 0) and (AEvent.EntryIndex < Length(FCellCauses)) then
      LCause := FCellCauses[AEvent.EntryIndex];
    if LCause.EventId <> LEvent.CauseEventId then
      LCause := FBacktrackCause;
    if LCause.EventId <> LEvent.CauseEventId then
      raise EInvalidOperation.Create('TrySolve::reference stream cause summary is missing');
    LEvent.CauseKind := LCause.Kind;
    if AEvent.Kind = rtekDecision then
      LEvent.ConstraintIndex := LCause.ConstraintIndex
    else if LEvent.CauseKind = gtckPassDependency then
      LEvent.DependencyPassIndex := LCause.DependencyPassIndex;
  end;
  if AEvent.Kind = rtekSolved then
  begin
    LEvent.CauseKind := gtckTransaction;
    LEvent.EntryIndex := -1;
    LEvent.ValueIndex := -1;
    LEvent.Value := TGraphValue.Empty;
    LEvent.NeighborIndex := -1;
    LEvent.HasDirection := False;
    LEvent.Direction := gdNorth;
    LEvent.DependencyPassIndex := -1;
    LEvent.DecisionDepth := 0;
    LEvent.DomainCountBefore := 0;
    LEvent.DomainCountAfter := 0;
    LEvent.ConstraintIndex := -1;
  end;
  LCause.EventId := Append(LEvent);
  LCause.Kind := LEvent.CauseKind;
  LCause.DependencyPassIndex := LEvent.DependencyPassIndex;
  LCause.ConstraintIndex := LEvent.ConstraintIndex;
  if AEvent.Kind in [rtekInitialCandidateRemoved, rtekCandidateRemoved,
    rtekCandidateRestored] then
    FCellCauses[AEvent.EntryIndex] := LCause
  else if AEvent.Kind = rtekBacktrack then
    FBacktrackCause := LCause;
end;

procedure TGraphTraceRecorder.Finish(var AReport: TGraphSolveReport;
  const AInterrupted: Boolean);
var
  I: Integer;
begin
  if FCapture then
  begin
    SetLength(FEvents, FCount);
    AReport.Trace := FEvents;
    AReport.TraceHash := FHash;
    for I := 0 to High(AReport.Passes) do
    begin
      AReport.Passes[I].TraceStart := FStarts[I];
      AReport.Passes[I].TraceCount := FCounts[I];
    end;
  end;
  if Assigned(FSink) then
  begin
    if AInterrupted then
      FDelivery.Status := gtdsInterrupted;
    FDelivery.TraceHash := FHash;
    try
      FSink.EndTrace(FDelivery);
    except
      on E: Exception do SinkFailed(gtdpEnd, -1, E.Message);
      else SinkFailed(gtdpEnd, -1, 'non-Pascal observer exception');
    end;
  end;
  AReport.TraceDelivery := FDelivery;
end;

function CalculateGraphNegotiationTranscriptHash(
  const AOptions: TGraphNegotiationOptions;
  const AReport: TGraphNegotiationReport): TGraphTraceSignature;
var
  I, J: Integer;
  LAttempt: TGraphNegotiationAttemptReport;
  LPass: TGraphPassSolveReport;
  LSolve: TGraphSolveReport;

  procedure MixBoolean(const AValue: Boolean);
  begin
    if AValue then
      GraphTraceHashByte(Result, 1)
    else
      GraphTraceHashByte(Result, 0);
  end;

  procedure MixSolveReport(const ASolve: TGraphSolveReport);
  var
    K: Integer;
  begin
    GraphTraceHashCardinal(Result, Cardinal(Ord(ASolve.Status)));
    GraphTraceHashCardinal(Result, ASolve.Seed);
    GraphTraceHashInteger(Result, ASolve.RandomAlgorithmVersion);
    GraphTraceHashInteger(Result, ASolve.SolverAlgorithmVersion);
    GraphTraceHashInteger(Result, ASolve.GraphModelVersion);
    GraphTraceHashInteger(Result, ASolve.PipelineAlgorithmVersion);
    GraphTraceHashInteger(Result, ASolve.FailedPassIndex);
    GraphTraceHashCardinal(Result,
      Cardinal(Ord(ASolve.Contradiction.Kind)));
    GraphTraceHashInteger(Result, ASolve.Contradiction.PassIndex);
    GraphTraceHashInteger(Result, ASolve.Contradiction.EntryIndex);
    GraphTraceHashInteger(Result, ASolve.Contradiction.NeighborIndex);
    MixBoolean(ASolve.Contradiction.HasDirection);
    GraphTraceHashCardinal(Result,
      Cardinal(Ord(ASolve.Contradiction.Direction)));
    GraphTraceHashInteger(Result,
      ASolve.Contradiction.DependencyPassIndex);
    if ASolve.Contradiction.Kind = gckConnectivity then
    begin
      GraphTraceHashCardinal(Result, WFC_GRAPH_CONNECTIVITY_VERSION);
      GraphTraceHashInteger(Result, ASolve.Contradiction.ConstraintIndex);
    end;
    if ASolve.Contradiction.Kind = gckValueQuota then
    begin
      GraphTraceHashCardinal(Result, WFC_GRAPH_VALUE_QUOTA_VERSION);
      GraphTraceHashInteger(Result, ASolve.Contradiction.ConstraintIndex);
    end;
    GraphTraceHashCardinal(Result, Cardinal(Length(ASolve.Passes)));
    for K := 0 to High(ASolve.Passes) do
    begin
      LPass := ASolve.Passes[K];
      GraphTraceHashInteger(Result, LPass.Decisions);
      GraphTraceHashInteger(Result, LPass.Propagations);
      GraphTraceHashInteger(Result, LPass.Contradictions);
      GraphTraceHashInteger(Result, LPass.Backtracks);
      GraphTraceHashInteger(Result, LPass.ExcludedAssignments);
      MixBoolean(LPass.Executed);
      GraphTraceHashInteger(Result, LPass.ExecutionOrdinal);
      GraphTraceHashCardinal(Result,
        Cardinal(Ord(LPass.Disposition)));
      GraphTraceHashInteger(Result, LPass.TraceStart);
      GraphTraceHashInteger(Result, LPass.TraceCount);
    end;
    GraphTraceHashCardinal(Result,
      Cardinal(Length(ASolve.ExecutionOrder)));
    for K := 0 to High(ASolve.ExecutionOrder) do
      GraphTraceHashInteger(Result, ASolve.ExecutionOrder[K]);
    MixBoolean(ASolve.TraceCaptured);
    GraphTraceHashCardinal(Result, ASolve.TraceHash);
    GraphTraceHashCardinal(Result, Cardinal(Length(ASolve.Trace)));
    for K := 0 to High(ASolve.Trace) do
      MixGraphTraceEvent(Result, ASolve.Trace[K]);
  end;

begin
  Result := Cardinal(2166136261);
  GraphTraceHashText(Result, 'wfc-pass-negotiation');
  GraphTraceHashCardinal(Result,
    WFC_PASS_NEGOTIATION_ALGORITHM_VERSION);
  GraphTraceHashCardinal(Result, WFC_PASS_NEGOTIATION_HASH_VERSION);
  GraphTraceHashInteger(Result, AOptions.SolveOptions.MaxBacktracks);
  MixBoolean(AOptions.SolveOptions.CaptureTrace);
  GraphTraceHashInteger(Result, AOptions.MaxPassBacktracks);
  GraphTraceHashCardinal(Result, Cardinal(Ord(AReport.Status)));
  GraphTraceHashCardinal(Result, AReport.Seed);
  GraphTraceHashInteger(Result,
    AReport.NegotiationAlgorithmVersion);
  GraphTraceHashInteger(Result, AReport.PassBacktracks);
  GraphTraceHashCardinal(Result, Cardinal(Length(AReport.Attempts)));
  for I := 0 to High(AReport.Attempts) do
  begin
    LAttempt := AReport.Attempts[I];
    GraphTraceHashInteger(Result, I);
    MixSolveReport(LAttempt.SolveReport);
    GraphTraceHashInteger(Result, LAttempt.BacktrackedPassIndex);
    GraphTraceHashInteger(Result,
      LAttempt.BacktrackedExecutionOrdinal);
    GraphTraceHashCardinal(Result,
      Cardinal(Length(LAttempt.ExcludedAssignment)));
    for J := 0 to High(LAttempt.ExcludedAssignment) do
      GraphTraceHashInteger(Result,
        LAttempt.ExcludedAssignment[J]);
  end;
  //The terminal solve is included independently so callers can detect a
  //malformed report whose FinalReport is not the final ordered attempt.
  LSolve := AReport.FinalReport;
  MixSolveReport(LSolve);
end;

function CalculateGraphSelectiveNegotiationTranscriptHash(
  const AOptions: TGraphNegotiationOptions;
  const AReport: TGraphSelectiveNegotiationReport):
  TGraphTraceSignature;
var
  I: Integer;
begin
  Result := Cardinal(2166136261);
  GraphTraceHashText(Result, 'wfc-selective-pass-negotiation');
  GraphTraceHashCardinal(Result,
    WFC_SELECTIVE_NEGOTIATION_ALGORITHM_VERSION);
  GraphTraceHashCardinal(Result,
    WFC_SELECTIVE_NEGOTIATION_HASH_VERSION);
  GraphTraceHashInteger(Result, AReport.ScopeAlgorithmVersion);

  //Hash the canonical scope independently of caller label order and duplicate
  //roots. The nested calculator covers every attempt, exact exclusion, trace,
  //status, budget, and terminal ordinary solve without trusting its stored
  //derived TranscriptHash field.
  GraphTraceHashCardinal(Result,
    Cardinal(Length(AReport.RequestedRootIndices)));
  for I := 0 to High(AReport.RequestedRootIndices) do
    GraphTraceHashInteger(Result, AReport.RequestedRootIndices[I]);
  GraphTraceHashCardinal(Result,
    Cardinal(Length(AReport.ActivePassIndices)));
  for I := 0 to High(AReport.ActivePassIndices) do
    GraphTraceHashInteger(Result, AReport.ActivePassIndices[I]);
  GraphTraceHashCardinal(Result,
    CalculateGraphNegotiationTranscriptHash(AOptions, AReport.Search));
end;

function CalculateGraphRestartTranscriptHash(
  const AOptions: TGraphNegotiationOptions;
  const ARestarts: TGraphRestartOptions;
  const AReport: TGraphRestartReport): TGraphTraceSignature;
var
  I: Integer;
  LOptions: TGraphNegotiationOptions;

  function SolveDigest(const ASolve: TGraphSolveReport): TGraphTraceSignature;
  var
    LReport: TGraphNegotiationReport;
  begin
    //Reuse the complete numeric solve encoding rather than trusting its
    //trace digest, which is zero when capture is disabled.
    LReport := Default(TGraphNegotiationReport);
    LReport.Seed := ASolve.Seed;
    LReport.FinalReport := ASolve;
    Result := CalculateGraphNegotiationTranscriptHash(LOptions, LReport);
  end;

begin
  Result := Cardinal(2166136261);
  GraphTraceHashText(Result, 'wfc-graph-restart');
  GraphTraceHashCardinal(Result, WFC_RESTART_ALGORITHM_VERSION);
  GraphTraceHashCardinal(Result, WFC_RESTART_HASH_VERSION);
  GraphTraceHashInteger(Result, AOptions.SolveOptions.MaxBacktracks);
  GraphTraceHashByte(Result, Ord(AOptions.SolveOptions.CaptureTrace));
  GraphTraceHashInteger(Result, AOptions.MaxPassBacktracks);
  GraphTraceHashInteger(Result, ARestarts.MaxRestarts);
  GraphTraceHashInteger(Result, ARestarts.MaxBacktracksPerAttempt);
  GraphTraceHashCardinal(Result, Cardinal(Ord(ARestarts.Schedule)));
  GraphTraceHashCardinal(Result, AReport.BaseSeed);
  GraphTraceHashCardinal(Result, Cardinal(Ord(AReport.Strategy)));
  GraphTraceHashCardinal(Result, Cardinal(Ord(AReport.Status)));
  GraphTraceHashInteger(Result, AReport.RestartAlgorithmVersion);
  GraphTraceHashInteger(Result, AReport.Restarts);
  GraphTraceHashCardinal(Result, Cardinal(Length(AReport.Attempts)));
  for I := 0 to High(AReport.Attempts) do
  begin
    GraphTraceHashInteger(Result, AReport.Attempts[I].Index);
    GraphTraceHashCardinal(Result, AReport.Attempts[I].Seed);
    GraphTraceHashInteger(Result, AReport.Attempts[I].MaxBacktracks);
    LOptions := AOptions;
    LOptions.SolveOptions.MaxBacktracks := AReport.Attempts[I].MaxBacktracks;
    GraphTraceHashCardinal(Result, SolveDigest(AReport.Attempts[I].SolveReport));
    if AReport.Strategy = grstNegotiated then
      GraphTraceHashCardinal(Result,
        CalculateGraphNegotiationTranscriptHash(LOptions,
          AReport.Attempts[I].NegotiationReport));
  end;
  LOptions := AOptions;
  GraphTraceHashCardinal(Result, SolveDigest(AReport.FinalReport));
end;

function CalculateGraphRestartTranscriptHash(
  const AOptions: TGraphSolveOptions;
  const ARestarts: TGraphRestartOptions;
  const AReport: TGraphRestartReport): TGraphTraceSignature;
var
  LOptions: TGraphNegotiationOptions;
begin
  LOptions := Default(TGraphNegotiationOptions);
  LOptions.SolveOptions := AOptions;
  Result := CalculateGraphRestartTranscriptHash(LOptions, ARestarts, AReport);
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
    Parent.RequireIdenticalPassLayout(LPreviousIndex, 'RequirePrevious');
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
  Parent.RequireIdenticalPassLayout(LPassIndex, 'RequireFromPass');
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
  Parent.RequireIdenticalPassLayout(LPassIndex, 'RequireFromPassAt');
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
  Parent.RequireIdenticalPassLayout(LPassIndex, 'RequireAnyFromPass');
  Parent.SynchronizePreviousValueDependencies;
  Parent.AddDependencyRole(LPassIndex, pdrRequirement);
  AddPassAnyRequirement(LPassIndex, LCanonical, proNamed);
end;

procedure TGraph.TParentedGraphRuleGroup.DoRequireCountFromPass(
  const APass: String; const ATerms: TGraphPassMatchTerms;
  const AMinimum, AMaximum: Integer; const AMode: TGraphPassCountMode);
var
  LCanonical: TGraphPassMatchTerms;
  LPending: TPassRequirements;
  LPassIndex: Integer;
begin
  LCanonical := CanonicalGraphPassCountTerms(ATerms, AMinimum,
    AMaximum, AMode);
  if not Assigned(Parent) then
    raise EInvalidOperation.Create(
      'RequireCountFromPass::rule group is not owned by a graph');
  LPassIndex := Parent.PassIndexForLabel(APass, 'RequireCountFromPass');
  Parent.RequireIdenticalPassLayout(LPassIndex, 'RequireCountFromPass');
  //Allocate and copy the complete replacement before touching dependency
  //state. A rejected edge cannot leave a partially installed count clause,
  //and publishing the prepared array requires no further allocation.
  LPending := BuildPassCountRequirements(LPassIndex, LCanonical,
    AMinimum, AMaximum, AMode, proNamed);
  Parent.SynchronizePreviousValueDependencies;
  Parent.AddDependencyRole(LPassIndex, pdrRequirement);
  FPassRequirements := LPending;
end;

procedure TGraph.TParentedGraphRuleGroup.DoRequireMappedFromPass(
  const APass: String; const AQuery: TGraphPassMapQuery);
var
  LCanonical: TGraphPassMapQuery;
  LPending: TPassRequirements;
  LPassIndex: Integer;
begin
  LCanonical := CanonicalGraphPassMapQuery(AQuery);
  if not Assigned(Parent) then
    raise EInvalidOperation.Create('RequireMappedFromPass::rule group is not owned by a graph');
  if Parent.Running then
    raise EInvalidOperation.Create('RequireMappedFromPass::cannot change requirements while running');
  LPassIndex := Parent.PassIndexForLabel(APass, 'RequireMappedFromPass');
  ValidateGraphPassMapRange(Parent.GetPassLayout, LCanonical);
  ValidateWfcLatticeLayout(Parent.GetPassGraph(LPassIndex).FLayout);
  LPending := BuildMappedPassRequirements(LPassIndex, LCanonical);
  Parent.SynchronizePreviousValueDependencies;
  Parent.AddDependencyRole(LPassIndex, pdrRequirement);
  FPassRequirements := LPending;
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
  LRequirement := Default(TPassRequirement);
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
  LRequirement.MinimumMatches := 0;
  LRequirement.MaximumMatches := 0;
  LRequirement.CountMode := gpcmMatchingTerms;
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
  LRequirement := Default(TPassRequirement);
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
  LRequirement.MinimumMatches := 0;
  LRequirement.MaximumMatches := 0;
  LRequirement.CountMode := gpcmMatchingTerms;
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
  LRequirement := Default(TPassRequirement);
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
  LRequirement.MinimumMatches := 0;
  LRequirement.MaximumMatches := 0;
  LRequirement.CountMode := gpcmMatchingTerms;
  SetLength(FPassRequirements, Succ(Length(FPassRequirements)));
  for I := High(FPassRequirements) downto Succ(LInsertIndex) do
    FPassRequirements[I] := FPassRequirements[Pred(I)];
  FPassRequirements[LInsertIndex] := LRequirement;
end;

function TGraphRuleGroup.BuildPassCountRequirements(
  const APassIndex: Integer; const ATerms: TGraphPassMatchTerms;
  const AMinimum, AMaximum: Integer; const AMode: TGraphPassCountMode;
  const AOrigin: TPassRequirementOrigin): TPassRequirements;
var
  I, LInsertIndex: Integer;
  LCanonical: TGraphPassMatchTerms;
  LRequirement: TPassRequirement;
begin
  LCanonical := CanonicalGraphPassCountTerms(ATerms, AMinimum,
    AMaximum, AMode);
  LRequirement := Default(TPassRequirement);
  Result := Copy(FPassRequirements, 0, Length(FPassRequirements));
  LInsertIndex := Length(Result);
  for I := 0 to High(Result) do
  begin
    if (Result[I].PassIndex = APassIndex)
      and (Result[I].Kind = prkCount)
      and (Result[I].MinimumMatches = AMinimum)
      and (Result[I].MaximumMatches = AMaximum)
      and (Result[I].CountMode = AMode)
      and GraphPassCountTermsEqual(Result[I].Terms, LCanonical) then
    begin
      Include(Result[I].Origins, AOrigin);
      Exit;
    end;
    if (LInsertIndex = Length(Result))
      and (Result[I].PassIndex > APassIndex) then
      LInsertIndex := I;
  end;

  LRequirement.PassIndex := APassIndex;
  LRequirement.Terms := LCanonical;
  LRequirement.Origins := [AOrigin];
  LRequirement.Kind := prkCount;
  LRequirement.MinimumMatches := AMinimum;
  LRequirement.MaximumMatches := AMaximum;
  LRequirement.CountMode := AMode;
  SetLength(Result, Succ(Length(Result)));
  for I := High(Result) downto Succ(LInsertIndex) do
    Result[I] := Result[Pred(I)];
  Result[LInsertIndex] := LRequirement;
end;

function TGraphRuleGroup.BuildMappedPassRequirements(
  const APassIndex: Integer; const AQuery: TGraphPassMapQuery):
  TPassRequirements;
var
  I, LInsertIndex: Integer;
  LCanonical: TGraphPassMapQuery;
  LRequirement: TPassRequirement;
begin
  LCanonical := CanonicalGraphPassMapQuery(AQuery);
  Result := Copy(FPassRequirements, 0, Length(FPassRequirements));
  LInsertIndex := Length(Result);
  for I := 0 to High(Result) do
  begin
    if (Result[I].PassIndex = APassIndex)
      and (Result[I].Kind = prkMapped)
      and GraphPassMapQueriesEqual(Result[I].MappedQuery, LCanonical) then Exit;
    if (LInsertIndex = Length(Result))
      and (Result[I].PassIndex > APassIndex) then LInsertIndex := I;
  end;
  LRequirement := Default(TPassRequirement);
  LRequirement.PassIndex := APassIndex;
  LRequirement.Origins := [proNamed];
  LRequirement.Kind := prkMapped;
  LRequirement.MappedQuery := LCanonical;
  SetLength(Result, Succ(Length(Result)));
  for I := High(Result) downto Succ(LInsertIndex) do
    Result[I] := Result[Pred(I)];
  Result[LInsertIndex] := LRequirement;
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

procedure TGraphRuleGroup.DoRequireCountFromPass(const APass: String;
  const ATerms: TGraphPassMatchTerms; const AMinimum, AMaximum: Integer;
  const AMode: TGraphPassCountMode);
begin
  CanonicalGraphPassCountTerms(ATerms, AMinimum, AMaximum, AMode);
  raise EInvalidOperation.CreateFmt(
    'RequireCountFromPass::rule group is not owned by a graph [%s]',
    [APass]);
end;


function TGraphRuleGroup.NewRule(const ADirections: TGraphDirections;
  const AValue: TGraphValue; const ARequireRule: Boolean): TGraphRuleGroup;
begin
  Result := Self;
  DoNewRule(ADirections, AValue, ARequireRule);
end;

procedure TGraphRuleGroup.DoRequireMappedFromPass(const APass: String;
  const AQuery: TGraphPassMapQuery);
begin
  CanonicalGraphPassMapQuery(AQuery);
  raise EInvalidOperation.CreateFmt(
    'RequireMappedFromPass::rule group is not owned by a graph [%s]', [APass]);
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

function TGraphRuleGroup.RequireCountFromPass(const APass: String;
  const ATerms: TGraphPassMatchTerms; const AMinimum, AMaximum: Integer;
  const AMode: TGraphPassCountMode): TGraphRuleGroup;
var
  LCanonical: TGraphPassMatchTerms;
begin
  Result := Self;
  //Validation and copying precede virtual dispatch, matching the spatial
  //clause contract even for user-defined descendants.
  LCanonical := CanonicalGraphPassCountTerms(ATerms, AMinimum,
    AMaximum, AMode);
  DoRequireCountFromPass(APass, LCanonical, AMinimum, AMaximum, AMode);
end;

function TGraphRuleGroup.RequireMappedFromPass(const APass: String;
  const AQuery: TGraphPassMapQuery): TGraphRuleGroup;
var
  LCanonical: TGraphPassMapQuery;
begin
  Result := Self;
  LCanonical := CanonicalGraphPassMapQuery(AQuery);
  DoRequireMappedFromPass(APass, LCanonical);
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

procedure TGraphEntry.RestoreNeighborState(const ADirection: TGraphDirection;
  const ANeighbor: TGraphEntry);
begin
  //Transaction rollback must not invoke the hooks that changed the model.
  FNeighbors[Ord(ADirection)] := ANeighbor;
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
begin
  EnsureSeedInitialized;
  BuildPassRandomState(APassIndex, FSeed, AState);
end;

procedure TGraph.BuildPassRandomState(const APassIndex: Integer;
  const AEffectiveSeed: TGraphSeed; out AState: TRandomState);
var
  I: Integer;
begin
  if APassIndex < 0 then
    raise ERangeError.CreateFmt(
      'BuildPassRandomState::invalid pass index [%d]', [APassIndex]);

  SeedRandomState(AEffectiveSeed, AState);
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

function TGraph.GetPassLayout: TWfcLatticeLayout;
begin
  Result := GetActivePassGraph.FLayout;
end;

function TGraph.CopyPassLayouts: TWfcLatticeLayouts;
var
  I: Integer;
begin
  if Assigned(FPassRoot) then Exit(FPassRoot.CopyPassLayouts);
  EnsureInitialPass;
  SetLength(Result, FPasses.Count);
  for I := 0 to Pred(FPasses.Count) do Result[I] := FPasses[I].FLayout;
end;

procedure TGraph.RequireIdenticalPassLayout(const AProviderIndex: Integer;
  const AOperation: String);
var
  LGraph, LRoot: TGraph;
begin
  LGraph := GetActivePassGraph;
  LRoot := LGraph.FPassRoot;
  if (AProviderIndex < 0) or (AProviderIndex >= LRoot.FPasses.Count) then
    raise EInvalidOperation.CreateFmt('%s::invalid provider', [AOperation]);
  if not GraphLayoutsEqual(LGraph.FLayout,
    LRoot.FPasses[AProviderIndex].FLayout) then
    raise EInvalidOperation.CreateFmt(
      '%s::index-space reads require identical pass layouts; use RequireMappedFromPass',
      [AOperation]);
end;

procedure TGraph.ValidatePassLayoutReads(const ALayouts: TWfcLatticeLayouts);
var
  I, J, LSource: Integer;
  LGraph: TGraph;
  LGroup: TGraphRuleGroup;
  LRequirement: TGraphRuleGroup.TPassRequirement;

  procedure CheckSource(const ASource: Integer; const AIdentical: Boolean);
  begin
    if (ASource < 0) or (ASource >= Length(ALayouts)) then
      raise EInvalidOperation.Create('PassLayouts::invalid provider index');
    if AIdentical and not GraphLayoutsEqual(ALayouts[I], ALayouts[ASource]) then
      raise EInvalidOperation.CreateFmt(
        'PassLayouts::pass %d index-space read of %d requires identical layouts', [I, ASource]);
  end;
begin
  if Assigned(FPassRoot) then
  begin
    FPassRoot.ValidatePassLayoutReads(ALayouts);
    Exit;
  end;
  if Length(ALayouts) <> FPasses.Count then
    raise EArgumentException.Create('PassLayouts::one layout is required for every pass');
  for I := 0 to Pred(FPasses.Count) do
  begin
    LGraph := FPasses[I];
    for LGroup in LGraph.FRuleGroups.Values do
    begin
      //Malformed public registries are diagnosed by the existing model
      //validator; layout preflight must not dereference a caller's nil slot.
      if not Assigned(LGroup) then Continue;
      if (I > 0) and (Length(LGroup.PreviousValues) > 0) then
        CheckSource(Pred(I), True);
      for J := 0 to High(LGroup.FPassRequirements) do
      begin
        LRequirement := LGroup.FPassRequirements[J];
        CheckSource(LRequirement.PassIndex, LRequirement.Kind <> prkMapped);
        if LRequirement.Kind = prkMapped then
        begin
          ValidateGraphPassMapRange(ALayouts[I], LRequirement.MappedQuery);
          ValidateWfcLatticeLayout(ALayouts[LRequirement.PassIndex]);
        end;
      end;
    end;
    //Sequential edges alone impose ordering, not an index-space relationship.
    //A definitionless legacy/transform pass, however, actually copies values.
    LSource := -1;
    if LGraph.FPassMode = gpmTransform then
      LSource := LGraph.FTransformSourceIndex
    else if (not LGraph.HasDefinition) and (LGraph.FPassMode = gpmLegacy)
      and (I > 0) then LSource := Pred(I);
    if LSource >= 0 then CheckSource(LSource, True);
  end;
end;

function TGraph.MappedRequirementMatches(const AEntryIndex: Integer;
  const AProvider: TGraph; const AQuery: TGraphPassMapQuery;
  const AStagedValues: TGraphValues; const AUseStaged: Boolean): Boolean;
var
  LBox, LCellBox: TWfcLatticeBox;
  LCell: TWfcLatticeVector;
  LCoverage: TWfcLatticeCoverage;
  LPosition: TGraphPosition;
  I, LCellCount, LCount, LIndex: Integer;
  LValue: TGraphValue;
  LMatches: Boolean;

  function OffsetPoint(const APoint: TWfcLatticeVector;
    const AOffset: TGraphOffset): TWfcLatticeVector;
  var
    LX, LY, LZ: Double;
  begin
    //Registration/configuration preflight proved each endpoint representable.
    LX := APoint.X;
    LY := APoint.Y;
    LZ := APoint.Z;
    Result.X := Integer(Trunc(LX + AOffset.DeltaX));
    Result.Y := Integer(Trunc(LY + AOffset.DeltaY));
    Result.Z := Integer(Trunc(LZ + AOffset.DeltaZ));
  end;
begin
  Result := False;
  LPosition := FEntries[AEntryIndex].Position;
  LCellBox := WfcLatticeCellBox(FLayout, MakeWfcLatticeVector(
    Integer(LPosition.X), Integer(LPosition.Y), Integer(LPosition.Z)));
  LBox.Minimum := OffsetPoint(LCellBox.Minimum, AQuery.MinimumOffset);
  if AQuery.Kind = gpmkPoint then
  begin
    if not TryWfcLatticePoint(AProvider.FLayout, LBox.Minimum, LCell) then Exit;
    LCellCount := 1;
  end
  else
  begin
    if AQuery.Kind = gpmkCellCoverage then
      LBox.Maximum := OffsetPoint(LCellBox.Maximum, AQuery.MinimumOffset)
    else
      LBox.Maximum := OffsetPoint(LCellBox.Minimum, AQuery.MaximumOffset);
    if not TryWfcLatticeCoverage(AProvider.FLayout, LBox, LCoverage) then Exit;
    LCellCount := WfcLatticeCoverageCellCount(LCoverage);
  end;
  LCount := 0;
  for I := 0 to Pred(LCellCount) do
  begin
    if AQuery.Kind <> gpmkPoint then
      LCell := WfcLatticeCoverageCell(LCoverage, I);
    LIndex := AProvider.CoordToIndex(LCell.X, LCell.Y, LCell.Z);
    if AUseStaged then LValue := AStagedValues[LIndex]
    else LValue := AProvider.FEntries[LIndex].Value;
    LMatches := (LValue <> TGraphValue.Empty)
      and ContainsGraphValue(AQuery.Values, LValue);
    if AQuery.Match = gpmmAll then
    begin
      if not LMatches then Exit;
    end
    else if LMatches then
    begin
      Inc(LCount);
      if LCount > AQuery.MaximumMatches then Exit;
    end;
  end;
  Result := (AQuery.Match = gpmmAll) or (LCount >= AQuery.MinimumMatches);
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
    LGraph.RequireIdenticalPassLayout(LSourceIndex, 'SetPassMode');
  end;
  if (AValue = gpmLegacy) and (LGraph.FPassIndex > 0) then
  begin
    LPreviousIndex := Pred(LGraph.FPassIndex);
    if not LGraph.HasDefinition then
      LGraph.RequireIdenticalPassLayout(LPreviousIndex, 'SetPassMode');
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
  LRoot.ValidatePassLayoutReads(LRoot.CopyPassLayouts);
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

procedure TGraph.BuildDescendantPassSelection(
  const APasses: TGraphPassLabels; const AOperation: String;
  out ADirty: TPassSelection);
var
  I, J: Integer;
  LChanged: Boolean;
begin
  SetLength(ADirty, FPasses.Count);
  //SetLength preserves elements when a managed-array variable is reused.
  //Scope construction must depend only on this call's roots.
  for I := 0 to Pred(FPasses.Count) do
    ADirty[I] := 0;

  //Resolve every public label before synchronizing derived dependency roles.
  //A bad later label therefore cannot leave even derived model state changed.
  for I := 0 to High(APasses) do
    ADirty[PassIndexForLabel(APasses[I], AOperation)] := 1;

  SynchronizePreviousValueDependencies;
  //The active scope is the least set containing the canonical roots and every
  //transitive consumer. This is exactly ordinary selective-regeneration scope,
  //so changing an authorized provider cannot leave a stale clean descendant.
  repeat
    LChanged := False;
    for I := 0 to Pred(FPasses.Count) do
      if ADirty[I] = 0 then
        for J := 0 to High(FPasses[I].FPassDependencies) do
          if ADirty[FPasses[I].FPassDependencies[J].PassIndex] <> 0 then
          begin
            ADirty[I] := 1;
            LChanged := True;
            Break;
          end;
  until not LChanged;
end;

procedure TGraph.BuildDescendantPassSelection(
  const APasses: TGraphPassLabels; const AOperation: String;
  out ARequestedRootIndices, AActivePassIndices: TGraphPassIndices;
  out ADirty: TPassSelection);
var
  I, LActiveCount, LRootCount: Integer;
  LExecutionOrder: TGraphPassIndices;
  LRoots: TPassSelection;
begin
  BuildDescendantPassSelection(APasses, AOperation, ADirty);

  SetLength(LRoots, FPasses.Count);
  for I := 0 to Pred(FPasses.Count) do
    LRoots[I] := 0;
  for I := 0 to High(APasses) do
    LRoots[PassIndexForLabel(APasses[I], AOperation)] := 1;

  LRootCount := 0;
  for I := 0 to Pred(FPasses.Count) do
    if LRoots[I] <> 0 then
      Inc(LRootCount);
  SetLength(ARequestedRootIndices, LRootCount);
  LRootCount := 0;
  for I := 0 to Pred(FPasses.Count) do
    if LRoots[I] <> 0 then
    begin
      ARequestedRootIndices[LRootCount] := I;
      Inc(LRootCount);
    end;

  BuildPassExecutionOrder(LExecutionOrder);
  LActiveCount := 0;
  for I := 0 to High(LExecutionOrder) do
    if ADirty[LExecutionOrder[I]] <> 0 then
      Inc(LActiveCount);
  SetLength(AActivePassIndices, LActiveCount);
  LActiveCount := 0;
  for I := 0 to High(LExecutionOrder) do
    if ADirty[LExecutionOrder[I]] <> 0 then
    begin
      AActivePassIndices[LActiveCount] := LExecutionOrder[I];
      Inc(LActiveCount);
    end;
end;

procedure TGraph.ValidateNegotiationOptions(
  const AOptions: TGraphNegotiationOptions;
  const AOperation: String);
begin
  if AOptions.SolveOptions.MaxBacktracks < 0 then
    raise ERangeError.CreateFmt(
      AOperation + '::maximum solver backtracks cannot be negative [%d]',
      [AOptions.SolveOptions.MaxBacktracks]);
  if AOptions.MaxPassBacktracks < 0 then
    raise ERangeError.CreateFmt(
      AOperation + '::maximum pass backtracks cannot be negative [%d]',
      [AOptions.MaxPassBacktracks]);
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

function TGraph.GetTraceSink: TGraphTraceSink;
begin
  if Assigned(FPassRoot) then
    Result := FPassRoot.FTraceSink
  else
    Result := FTraceSink;
end;

procedure TGraph.SetTraceSink(const AValue: TGraphTraceSink);
begin
  if Assigned(FPassRoot) then
  begin
    FPassRoot.SetTraceSink(AValue);
    Exit;
  end;
  if FRunning then
    raise EInvalidOperation.Create('TraceSink::cannot change while the pipeline is running');
  FTraceSink := AValue;
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
  FLayout.Wrap := AValue;
  for I := 0 to Pred(FPasses.Count) do
  begin
    FPasses[I].FWrap := AValue;
    FPasses[I].FLayout.Wrap := AValue;
    FPasses[I].LinkNeighbors;
  end;
end;

procedure TGraph.CopyValuesFrom(const ASource: TGraph);
var
  I: Integer;
begin
  if not GraphLayoutsEqual(ASource.FLayout, FLayout)
    or (FEntries.Count <> ASource.FEntries.Count) then
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

function TGraph.RequireConnectivity(
  const AConstraint: TGraphConnectivityConstraint): TGraph;
var
  LGraph: TGraph;
  LCanonical: TGraphConnectivityConstraint;
  LMarked: array of Byte;
  LDirections: TGraphDirections;
  LDirection: TGraphDirection;
  I, J, K, LCount: Integer;
begin
  Result := Self;
  if Running then
    raise EInvalidOperation.Create(
      'RequireConnectivity::cannot change constraints while running');
  LGraph := GetActivePassGraph;
  if AConstraint.LabelText = '' then
    raise EArgumentException.Create('RequireConnectivity::label cannot be empty');
  if (AConstraint.RequireAllParticipants <> False)
    and (AConstraint.RequireAllParticipants <> True) then
    raise ERangeError.Create('RequireConnectivity::all-participants flag must be Boolean');
  {$IFNDEF PAS2JS}
  if Ord(AConstraint.RequireAllParticipants) > 1 then
    raise ERangeError.Create('RequireConnectivity::all-participants flag must be Boolean');
  {$ENDIF}
  CheckConnectivityPosition(AConstraint.Root, LGraph.FDimension.Width,
    LGraph.FDimension.Height, LGraph.FDimension.Depth);
  if Length(AConstraint.Values) = 0 then
    raise EArgumentException.Create('RequireConnectivity::participating profiles are required');
  for I := 0 to High(AConstraint.Values) do
  begin
    if (AConstraint.Values[I].Value = TGraphValue.Empty)
      or not ContainsGraphValue(LGraph.FValues, AConstraint.Values[I].Value) then
      raise EArgumentException.CreateFmt(
        'RequireConnectivity::unknown participating value "%s"',
        [AConstraint.Values[I].Value]);
    for J := 0 to I - 1 do
      if AConstraint.Values[J].Value = AConstraint.Values[I].Value then
        raise EArgumentException.Create('RequireConnectivity::duplicate value profile');
    if (AConstraint.Values[I].RequiredByValue <> False)
      and (AConstraint.Values[I].RequiredByValue <> True) then
      raise ERangeError.Create('RequireConnectivity::required-by-value flag must be Boolean');
    {$IFNDEF PAS2JS}
    if Ord(AConstraint.Values[I].RequiredByValue) > 1 then
      raise ERangeError.Create('RequireConnectivity::required-by-value flag must be Boolean');
    {$ENDIF}
    LDirections := [];
    for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
      if LDirection in AConstraint.Values[I].Openings then
        Include(LDirections, LDirection);
    if LDirections <> AConstraint.Values[I].Openings then
      raise ERangeError.Create('RequireConnectivity::invalid opening direction');
  end;
  for I := 0 to High(AConstraint.RequiredPositions) do
    CheckConnectivityPosition(AConstraint.RequiredPositions[I],
      LGraph.FDimension.Width, LGraph.FDimension.Height, LGraph.FDimension.Depth);

  LCanonical := MakeGraphConnectivityConstraint(AConstraint.LabelText,
    AConstraint.Root, nil, nil, AConstraint.RequireAllParticipants);
  SetLength(LCanonical.Values, Length(AConstraint.Values));
  K := 0;
  for I := 0 to High(LGraph.FValues) do
    for J := 0 to High(AConstraint.Values) do
      if LGraph.FValues[I] = AConstraint.Values[J].Value then
      begin
        LCanonical.Values[K] := AConstraint.Values[J];
        //Native sets are values; pas2js sets use copy-on-write semantics.
        Inc(K);
        Break;
      end;
  SetLength(LMarked, LGraph.FEntries.Count);
  LCount := 0;
  for I := 0 to High(AConstraint.RequiredPositions) do
  begin
    K := LGraph.CoordToIndex(AConstraint.RequiredPositions[I].X,
      AConstraint.RequiredPositions[I].Y, AConstraint.RequiredPositions[I].Z);
    if LMarked[K] = 0 then
    begin
      LMarked[K] := 1;
      Inc(LCount);
    end;
  end;
  SetLength(LCanonical.RequiredPositions, LCount);
  K := 0;
  for I := 0 to High(LMarked) do
    if LMarked[I] <> 0 then
    begin
      LCanonical.RequiredPositions[K] := LGraph.FEntries[I].Position;
      Inc(K);
    end;
  for I := 0 to High(LGraph.FConnectivity) do
    if LGraph.FConnectivity[I].LabelText = LCanonical.LabelText then
    begin
      if not GraphConnectivityEqual(LGraph.FConnectivity[I], LCanonical) then
        raise EInvalidOperation.Create(
          'RequireConnectivity::label already has a different definition; remove it first');
      Exit;
    end;
  SetLength(LGraph.FConnectivity, Length(LGraph.FConnectivity) + 1);
  LGraph.FConnectivity[High(LGraph.FConnectivity)] := LCanonical;
end;

function TGraph.RemoveConnectivity(const ALabel: String): TGraph;
var
  LGraph: TGraph;
  I, J: Integer;
begin
  Result := Self;
  if Running then
    raise EInvalidOperation.Create('RemoveConnectivity::cannot change constraints while running');
  LGraph := GetActivePassGraph;
  for I := 0 to High(LGraph.FConnectivity) do
    if LGraph.FConnectivity[I].LabelText = ALabel then
    begin
      for J := I to High(LGraph.FConnectivity) - 1 do
        LGraph.FConnectivity[J] := LGraph.FConnectivity[J + 1];
      SetLength(LGraph.FConnectivity, Length(LGraph.FConnectivity) - 1);
      Exit;
    end;
end;

function TGraph.ClearConnectivity: TGraph;
begin
  Result := Self;
  if Running then
    raise EInvalidOperation.Create('ClearConnectivity::cannot change constraints while running');
  GetActivePassGraph.FConnectivity := nil;
end;

function TGraph.CopyConnectivityConstraints: TGraphConnectivityConstraints;
var
  LGraph: TGraph;
  I: Integer;
begin
  LGraph := GetActivePassGraph;
  Result := nil;
  SetLength(Result, Length(LGraph.FConnectivity));
  for I := 0 to High(Result) do
    with LGraph.FConnectivity[I] do
      Result[I] := MakeGraphConnectivityConstraint(LabelText, Root,
        RequiredPositions, Values, RequireAllParticipants);
end;

function TGraph.RequireValueQuota(
  const AConstraint: TGraphValueQuotaConstraint): TGraph;
var
  LGraph: TGraph;
  LCanonical: TGraphValueQuotaConstraint;
  I, LCount: Integer;
begin
  Result := Self;
  if Running then
    raise EInvalidOperation.Create(
      'RequireValueQuota::cannot change constraints while running');
  LGraph := GetActivePassGraph;
  if AConstraint.LabelText = '' then
    raise EArgumentException.Create('RequireValueQuota::label cannot be empty');
  CheckGraphValueQuotaBounds(AConstraint.MinimumCount, AConstraint.MaximumCount);
  if Length(AConstraint.Values) = 0 then
    raise EArgumentException.Create('RequireValueQuota::accepted values are required');
  for I := 0 to High(AConstraint.Values) do
    if (AConstraint.Values[I] = TGraphValue.Empty)
      or not ContainsGraphValue(LGraph.FValues, AConstraint.Values[I]) then
      raise EArgumentException.CreateFmt(
        'RequireValueQuota::unknown accepted value "%s"', [AConstraint.Values[I]]);

  LCanonical := MakeGraphValueQuotaConstraint(AConstraint.LabelText, nil,
    AConstraint.MinimumCount, AConstraint.MaximumCount);
  SetLength(LCanonical.Values, Length(LGraph.FValues));
  LCount := 0;
  for I := 0 to High(LGraph.FValues) do
    if ContainsGraphValue(AConstraint.Values, LGraph.FValues[I]) then
    begin
      LCanonical.Values[LCount] := LGraph.FValues[I];
      Inc(LCount);
    end;
  SetLength(LCanonical.Values, LCount);
  for I := 0 to High(LGraph.FValueQuotas) do
    if LGraph.FValueQuotas[I].LabelText = LCanonical.LabelText then
    begin
      if not GraphValueQuotaEqual(LGraph.FValueQuotas[I], LCanonical) then
        raise EInvalidOperation.Create(
          'RequireValueQuota::label already has a different definition; remove it first');
      Exit;
    end;
  SetLength(LGraph.FValueQuotas, Length(LGraph.FValueQuotas) + 1);
  LGraph.FValueQuotas[High(LGraph.FValueQuotas)] := LCanonical;
end;

function TGraph.RemoveValueQuota(const ALabel: String): TGraph;
var
  LGraph: TGraph;
  I, J: Integer;
begin
  Result := Self;
  if Running then
    raise EInvalidOperation.Create(
      'RemoveValueQuota::cannot change constraints while running');
  LGraph := GetActivePassGraph;
  for I := 0 to High(LGraph.FValueQuotas) do
    if LGraph.FValueQuotas[I].LabelText = ALabel then
    begin
      for J := I to High(LGraph.FValueQuotas) - 1 do
        LGraph.FValueQuotas[J] := LGraph.FValueQuotas[J + 1];
      SetLength(LGraph.FValueQuotas, Length(LGraph.FValueQuotas) - 1);
      Exit;
    end;
end;

function TGraph.ClearValueQuotas: TGraph;
begin
  Result := Self;
  if Running then
    raise EInvalidOperation.Create(
      'ClearValueQuotas::cannot change constraints while running');
  GetActivePassGraph.FValueQuotas := nil;
end;

function TGraph.CopyValueQuotaConstraints: TGraphValueQuotaConstraints;
var
  LGraph: TGraph;
  I: Integer;
begin
  LGraph := GetActivePassGraph;
  Result := nil;
  SetLength(Result, Length(LGraph.FValueQuotas));
  for I := 0 to High(Result) do
    with LGraph.FValueQuotas[I] do
      Result[I] := MakeGraphValueQuotaConstraint(LabelText, Values,
        MinimumCount, MaximumCount);
end;

procedure TGraph.ValidateConnectivityShape(const AWidth, AHeight,
  ADepth: TGraphCoordinate);
var
  I, J: Integer;
begin
  for I := 0 to High(FConnectivity) do
  begin
    CheckConnectivityPosition(FConnectivity[I].Root, AWidth, AHeight, ADepth);
    for J := 0 to High(FConnectivity[I].RequiredPositions) do
      CheckConnectivityPosition(FConnectivity[I].RequiredPositions[J],
        AWidth, AHeight, ADepth);
  end;
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

function TGraph.DoValidateCommit(out AFailedPassIndex,
  AFailedEntryIndex: Integer): Boolean;
begin
  AFailedPassIndex := -1;
  AFailedEntryIndex := -1;
  Result := True;
end;

function TGraph.DoReadMonotonicMilliseconds(out AValue: Double): Boolean;
begin
  Result := TryReadWfcMonotonicMilliseconds(AValue);
end;

function TGraph.ReadRestartClock(out AValue: Double): Boolean;
var
  LWasRunning: Boolean;
  LSavedPassIndex: Integer;
  LSavedPass: String;
  LZero: Double;
begin
  AValue := 0;
  LWasRunning := FRunning;
  LSavedPassIndex := FCurPassIndex;
  LSavedPass := FCurPass;
  FRunning := True;
  try
    try
      Result := DoReadMonotonicMilliseconds(AValue);
      if Result then
        Result := WfcElapsedMilliseconds(AValue, AValue, LZero);
    except
      //Timing is diagnostic, including after a successful publication.
      Result := False;
    end;
    if not Result then
      AValue := 0;
  finally
    FCurPassIndex := LSavedPassIndex;
    FCurPass := LSavedPass;
    FRunning := LWasRunning;
  end;
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
    end;

    function RequirementMatches(
      const ARequirementIndex: Integer): Boolean;
    var
      LCount, LTermIndex: Integer;
      LIsCount: Boolean;
      LMatchedIndices: TGraphPassIndices;
      LRequirement: TGraphRuleGroup.TPassRequirement;
    begin
      Result := False;
      LRequirement := LGroup.FPassRequirements[ARequirementIndex];
      LSourceGraph := SourceGraph(LRequirement.PassIndex);
      if LRequirement.Kind = prkMapped then
        Exit(MappedRequirementMatches(AEntry.Index, LSourceGraph,
          LRequirement.MappedQuery, nil, False));
      LIsCount := LRequirement.Kind = prkCount;
      LCount := 0;
      if LIsCount and (LRequirement.CountMode = gpcmDistinctCells) then
        SetLength(LMatchedIndices, Length(LRequirement.Terms));
      for LTermIndex := 0 to High(LRequirement.Terms) do
        if ResolveOffsetIndex(AEntry.Index,
          LRequirement.Terms[LTermIndex].Offset, LResolvedIndex) then
        begin
          LSourceEntry := LSourceGraph.FEntries[LResolvedIndex];
          if (not LSourceEntry.Empty)
            and ContainsGraphValue(
              LRequirement.Terms[LTermIndex].Values,
              LSourceEntry.Value) then
          begin
            if not LIsCount then
              Exit(True);
            RecordGraphPassCountMatch(LRequirement.CountMode,
              LResolvedIndex, LCount, LMatchedIndices);
            if LCount > LRequirement.MaximumMatches then
              Exit(False);
          end;
        end;
      if LIsCount then
        Result := LCount >= LRequirement.MinimumMatches;
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
  ADepth: TGraphCoordinate; const AWrap: Boolean; out AEntries: TGraphEntries;
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

      //Prepare only the requested topology. Neighbor hooks must see each
      //direction once, never the old wrap mode followed by a corrective link.
      LinkNeighborsFor(LEntries, LDimension, AWrap);
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
  BuildStorage(AWidth, AHeight, ADepth, FWrap, LEntries, LPlanes);
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
    FLayout := LegacyGraphLayout(AWidth, AHeight, ADepth, FWrap);

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
  LLayouts: TWfcLatticeLayouts;
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
  SetLength(LLayouts, FPasses.Count);
  for I := 0 to High(LLayouts) do
    LLayouts[I] := LegacyGraphLayout(AWidth, AHeight, ADepth, FWrap);
  ValidatePassLayoutReads(LLayouts);

  for I := 0 to Pred(FPasses.Count) do
    FPasses[I].ValidateConnectivityShape(AWidth, AHeight, ADepth);

  SetLength(LEntries, FPasses.Count);
  SetLength(LPlanes, FPasses.Count);
  SetLength(LOldEntries, FPasses.Count);
  SetLength(LOldPlanes, FPasses.Count);

  try
    //prepare every pass before committing any of them
    FInitializingPass := True;
    try
      for I := 0 to Pred(FPasses.Count) do
        FPasses[I].BuildStorage(AWidth, AHeight, ADepth,
          FWrap, LEntries[I], LPlanes[I]);
    finally
      FInitializingPass := False;
    end;

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
      FPasses[I].FLayout := LLayouts[I];
      FPasses[I].FWrap := FWrap;
    end;

    FDimension.Width := AWidth;
    FDimension.Height := AHeight;
    FDimension.Depth := ADepth;
    FLayout := LLayouts[0];

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

function TGraph.ConfigurePassLayouts(const ALayouts: TWfcLatticeLayouts): TGraph;
var
  I: Integer;
  LLayouts: TWfcLatticeLayouts;
  LEntries, LOldEntries: TEntryStorageArray;
  LPlanes, LOldPlanes: TPlaneStorageArray;
  {$IFDEF PAS2JS}LValid: Boolean;{$ENDIF}
begin
  if Assigned(FPassRoot) then Exit(FPassRoot.ConfigurePassLayouts(ALayouts));
  Result := Self;
  if FInitializingPass or FRunning then
    raise EInvalidOperation.Create('ConfigurePassLayouts::cannot configure during initialization or execution');
  {$IFDEF PAS2JS}
  asm LValid = Array.isArray(ALayouts); end;
  if not LValid then
    raise EArgumentException.Create('ConfigurePassLayouts::layouts must be an array');
  {$ENDIF}
  EnsureInitialPass;
  if Length(ALayouts) <> FPasses.Count then
    raise EArgumentException.Create('ConfigurePassLayouts::one layout is required for every pass');
  SetLength(LLayouts, Length(ALayouts));
  for I := 0 to High(ALayouts) do
  begin
    ValidateWfcLatticeLayout(ALayouts[I]);
    LLayouts[I] := ALayouts[I];
    FPasses[I].ValidateConnectivityShape(LLayouts[I].Cells.X,
      LLayouts[I].Cells.Y, LLayouts[I].Cells.Z);
  end;
  ValidatePassLayoutReads(LLayouts);
  SetLength(LEntries, FPasses.Count);
  SetLength(LPlanes, FPasses.Count);
  SetLength(LOldEntries, FPasses.Count);
  SetLength(LOldPlanes, FPasses.Count);
  try
    //Factories may inspect existing storage, but cannot recursively reshape,
    //reset, change global settings, or extend the pass registry underneath
    //the arrays being prepared. Reuse the pass-initialization lifecycle guard.
    FInitializingPass := True;
    try
      for I := 0 to Pred(FPasses.Count) do
        FPasses[I].BuildStorage(LLayouts[I].Cells.X, LLayouts[I].Cells.Y,
          LLayouts[I].Cells.Z, LLayouts[I].Wrap, LEntries[I], LPlanes[I]);
    finally
      FInitializingPass := False;
    end;
    //Publishing performs no allocations and invokes no user callbacks.
    for I := 0 to Pred(FPasses.Count) do
    begin
      LOldEntries[I] := FPasses[I].FEntries;
      LOldPlanes[I] := FPasses[I].FPlanes;
      FPasses[I].FEntries := LEntries[I];
      LEntries[I] := nil;
      FPasses[I].FPlanes := LPlanes[I];
      LPlanes[I] := nil;
      FPasses[I].FDimension.Width := LLayouts[I].Cells.X;
      FPasses[I].FDimension.Height := LLayouts[I].Cells.Y;
      FPasses[I].FDimension.Depth := LLayouts[I].Cells.Z;
      FPasses[I].FLayout := LLayouts[I];
      FPasses[I].FWrap := LLayouts[I].Wrap;
    end;
    FDimension := FPasses[0].FDimension;
    FLayout := LLayouts[0];
    FWrap := FLayout.Wrap;
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
      LGraph.FLayout := FLayout;
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
  LGraph.RequireIdenticalPassLayout(LPassIndex, 'TransformFrom');
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

function TGraph.TrySolveNegotiated(
  const AOptions: TGraphNegotiationOptions;
  out AReport: TGraphNegotiationReport): Boolean;
var
  I: Integer;
  LDirty: TPassSelection;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.TrySolveNegotiated(AOptions, AReport));
  ValidateNegotiationOptions(AOptions, 'TrySolveNegotiated');
  EnsureInitialPass;

  SetLength(LDirty, FPasses.Count);
  for I := 0 to High(LDirty) do
    LDirty[I] := 1;
  Result := TryNegotiateInternal(AOptions, LDirty,
    'TrySolveNegotiated', Seed, AReport);
end;

function TGraph.TrySolveRestarted(const AOptions: TGraphSolveOptions;
  const ARestarts: TGraphRestartOptions;
  out AReport: TGraphRestartReport): Boolean;
var
  LOptions: TGraphNegotiationOptions;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.TrySolveRestarted(AOptions, ARestarts, AReport));
  LOptions := Default(TGraphNegotiationOptions);
  LOptions.SolveOptions := AOptions;
  Result := TryRestartInternal(LOptions, ARestarts, grstOneWay, AReport);
end;

function TGraph.TrySolveNegotiatedRestarted(
  const AOptions: TGraphNegotiationOptions;
  const ARestarts: TGraphRestartOptions;
  out AReport: TGraphRestartReport): Boolean;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.TrySolveNegotiatedRestarted(AOptions, ARestarts, AReport));
  Result := TryRestartInternal(AOptions, ARestarts, grstNegotiated, AReport);
end;

function TGraph.TryRestartInternal(const AOptions: TGraphNegotiationOptions;
  const ARestarts: TGraphRestartOptions;
  const AStrategy: TGraphRestartStrategy;
  out AReport: TGraphRestartReport): Boolean;
var
  I, LIndex, LBudget: Integer;
  LOptions: TGraphNegotiationOptions;
  LDirty, LCompletedChoices: TPassSelection;
  LAssignments: TValueIndexMatrix;
  LExclusions: TPassAssignmentExclusions;
  LStart, LFinish, LAttemptStart, LAttemptFinish: Double;
  LStartAvailable, LFinishAvailable: Boolean;
  LAttemptStartAvailable, LAttemptFinishAvailable: Boolean;
begin
  //Validate the complete policy before seed capture or pass materialization.
  LBudget := GraphRestartBacktrackBudget(
    AOptions.SolveOptions.MaxBacktracks, 0, ARestarts);
  ValidateNegotiationOptions(AOptions, 'TrySolveRestarted');
  if (Ord(AStrategy) < Ord(Low(TGraphRestartStrategy)))
    or (Ord(AStrategy) > Ord(High(TGraphRestartStrategy))) then
    raise ERangeError.Create('Restart::invalid strategy');
  if FInitializingPass then
    raise EInvalidOperation.Create('Restart::cannot solve during pass initialization');
  if FRunning then
    raise EInvalidOperation.Create('Restart::the pass pipeline is already running');
  EnsureInitialPass;
  SetLength(LDirty, FPasses.Count);
  for I := 0 to High(LDirty) do
    LDirty[I] := 1;
  LExclusions := nil;
  AReport := Default(TGraphRestartReport);
  AReport.BaseSeed := Seed;
  AReport.Strategy := AStrategy;
  AReport.Status := grsContradiction;
  AReport.RestartAlgorithmVersion := WFC_RESTART_ALGORITHM_VERSION;
  LStartAvailable := False;
  LStart := 0;
  if ARestarts.MeasureTime then
    LStartAvailable := ReadRestartClock(LStart);
  LIndex := 0;
  while True do
  begin
    //Reserve history before invoking a solve that can publish live entries.
    //Every failed attempt already restores entries, selection, and all RNG
    //streams, so no SetSeed mutation or outer transaction copy is necessary.
    SetLength(AReport.Attempts, Succ(LIndex));
    AReport.Restarts := LIndex;
    AReport.Attempts[LIndex].Index := LIndex;
    AReport.Attempts[LIndex].Seed :=
      DeriveGraphRestartSeed(AReport.BaseSeed, LIndex);
    if LIndex <> 0 then
      LBudget := GraphRestartBacktrackBudget(
        AOptions.SolveOptions.MaxBacktracks, LIndex, ARestarts);
    AReport.Attempts[LIndex].MaxBacktracks := LBudget;
    LOptions := AOptions;
    LOptions.SolveOptions.MaxBacktracks := LBudget;
    LAttemptStartAvailable := False;
    LAttemptStart := 0;
    if ARestarts.MeasureTime then
      LAttemptStartAvailable := ReadRestartClock(LAttemptStart);
    if AStrategy = grstNegotiated then
    begin
      Result := TryNegotiateInternal(LOptions, LDirty,
        'TrySolveNegotiatedRestarted', AReport.Attempts[LIndex].Seed,
        AReport.Attempts[LIndex].NegotiationReport);
      AReport.Attempts[LIndex].SolveReport :=
        AReport.Attempts[LIndex].NegotiationReport.FinalReport;
    end
    else
      Result := TrySolveAttempt(LOptions.SolveOptions, LDirty,
        LExclusions, AReport.Attempts[LIndex].Seed, LCompletedChoices,
        LAssignments, AReport.Attempts[LIndex].SolveReport);
    AReport.FinalReport := AReport.Attempts[LIndex].SolveReport;
    if ARestarts.MeasureTime then
    begin
      LAttemptFinishAvailable := ReadRestartClock(LAttemptFinish);
      if LAttemptStartAvailable and LAttemptFinishAvailable then
        AReport.Attempts[LIndex].TimingAvailable := WfcElapsedMilliseconds(
          LAttemptStart, LAttemptFinish,
          AReport.Attempts[LIndex].ElapsedMilliseconds);
    end;
    if Result then
    begin
      AReport.Status := grsSolved;
      Break;
    end;
    if (AStrategy = grstNegotiated) and
      (AReport.Attempts[LIndex].NegotiationReport.Status = gnsPassBacktrackLimit) then
    begin
      AReport.Status := grsPassBacktrackLimit;
      Break;
    end;
    if AReport.FinalReport.Status <> gssBacktrackLimit then
    begin
      AReport.Status := grsContradiction;
      Break;
    end;
    if LIndex = ARestarts.MaxRestarts then
    begin
      AReport.Status := grsRestartLimit;
      Break;
    end;
    Inc(LIndex);
  end;
  if ARestarts.MeasureTime then
  begin
    LFinishAvailable := ReadRestartClock(LFinish);
    if LStartAvailable and LFinishAvailable then
      AReport.TimingAvailable := WfcElapsedMilliseconds(
        LStart, LFinish, AReport.ElapsedMilliseconds);
  end;
  AReport.TranscriptHash := CalculateGraphRestartTranscriptHash(
    AOptions, ARestarts, AReport);
end;

function TGraph.TryNegotiateInternal(
  const AOptions: TGraphNegotiationOptions;
  const ADirty: TPassSelection; const AOperation: String;
  const AEffectiveSeed: TGraphSeed;
  out AReport: TGraphNegotiationReport): Boolean;
var
  I, J: Integer;
  LAssignments: TValueIndexMatrix;
  LAttempt: TGraphNegotiationAttemptReport;
  LCompletedChoices: TPassSelection;
  LDuplicate: Boolean;
  LExclusions: TPassAssignmentExclusions;
  LFullExecutionPlan: TGraphPassIndices;
  LSolveReport: TGraphSolveReport;
  LTargetExecutionOrdinal: Integer;
  LTargetPassIndex: Integer;

  function AssignmentsEqual(const ALeft,
    ARight: TGraphValueIndices): Boolean;
  var
    K: Integer;
  begin
    if Length(ALeft) <> Length(ARight) then
      Exit(False);
    for K := 0 to High(ALeft) do
      if ALeft[K] <> ARight[K] then
        Exit(False);
    Result := True;
  end;

  procedure AppendRejectedAttempt;
  var
    LCount: Integer;
  begin
    LAttempt := Default(TGraphNegotiationAttemptReport);
    LAttempt.SolveReport := LSolveReport;
    LAttempt.BacktrackedPassIndex := LTargetPassIndex;
    LAttempt.BacktrackedExecutionOrdinal :=
      LTargetExecutionOrdinal;
    LAttempt.ExcludedAssignment := Copy(
      LAssignments[LTargetPassIndex], 0,
      Length(LAssignments[LTargetPassIndex]));
    LCount := Length(AReport.Attempts);
    if LCount = High(Integer) then
      raise ERangeError.Create(
        AOperation + '::attempt history is too large');
    SetLength(AReport.Attempts, Succ(LCount));
    AReport.Attempts[LCount] := LAttempt;
  end;

begin
  if Length(ADirty) <> FPasses.Count then
    raise EInvalidOperation.Create(
      AOperation + '::pass selection does not match the pipeline');
  AReport := Default(TGraphNegotiationReport);
  AReport.Status := gnsContradiction;
  AReport.Seed := AEffectiveSeed;
  AReport.NegotiationAlgorithmVersion :=
    WFC_PASS_NEGOTIATION_ALGORITHM_VERSION;
  AReport.PassBacktracks := 0;
  AReport.Attempts := nil;

  SetLength(LExclusions, FPasses.Count);
  BuildPassExecutionOrder(LFullExecutionPlan);

  while True do
  begin
    Result := TrySolveAttempt(AOptions.SolveOptions, ADirty,
      LExclusions, AEffectiveSeed, LCompletedChoices, LAssignments,
      LSolveReport);
    AReport.FinalReport := LSolveReport;
    if Result then
    begin
      AReport.Status := gnsSolved;
      Break;
    end;

    if LSolveReport.Status = gssBacktrackLimit then
    begin
      AReport.Status := gnsSolverBacktrackLimit;
      Break;
    end;

    //The latest completed defined pass is the chronological choice frame.
    //Definitionless and not-yet-run passes have no assignment to exclude.
    LTargetPassIndex := -1;
    LTargetExecutionOrdinal := -1;
    for I := High(LSolveReport.ExecutionOrder) downto 0 do
    begin
      J := LSolveReport.ExecutionOrder[I];
      if (J >= 0) and (J < Length(LCompletedChoices))
        and (LCompletedChoices[J] <> 0) then
      begin
        LTargetPassIndex := J;
        LTargetExecutionOrdinal :=
          LSolveReport.Passes[J].ExecutionOrdinal;
        Break;
      end;
    end;

    if LTargetPassIndex < 0 then
    begin
      AReport.Status := gnsContradiction;
      Break;
    end;
    if AReport.PassBacktracks >= AOptions.MaxPassBacktracks then
    begin
      AReport.Status := gnsPassBacktrackLimit;
      Break;
    end;

    //A changed prefix invalidates every exclusion learned in a later pass
    //context. Clearing by the stable full topological order also handles
    //independent siblings feeding a later join.
    for I := 0 to High(LFullExecutionPlan) do
      if LFullExecutionPlan[I] = LTargetPassIndex then
      begin
        for J := Succ(I) to High(LFullExecutionPlan) do
          LExclusions[LFullExecutionPlan[J]] := nil;
        Break;
      end;

    LDuplicate := False;
    for I := 0 to High(LExclusions[LTargetPassIndex]) do
      if AssignmentsEqual(LExclusions[LTargetPassIndex][I],
        LAssignments[LTargetPassIndex]) then
      begin
        LDuplicate := True;
        Break;
      end;
    if LDuplicate then
      raise EInvalidOperation.CreateFmt(
        AOperation + '::pass %d repeated an excluded assignment',
        [LTargetPassIndex]);

    AppendRejectedAttempt;
    I := Length(LExclusions[LTargetPassIndex]);
    if I = High(Integer) then
      raise ERangeError.Create(
        AOperation + '::assignment exclusions are too large');
    SetLength(LExclusions[LTargetPassIndex], Succ(I));
    LExclusions[LTargetPassIndex][I] := Copy(
      LAssignments[LTargetPassIndex], 0,
      Length(LAssignments[LTargetPassIndex]));
    Inc(AReport.PassBacktracks);
  end;

  AReport.TranscriptHash :=
    CalculateGraphNegotiationTranscriptHash(AOptions, AReport);
  Result := AReport.Status = gnsSolved;
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
  BuildDescendantPassSelection(APasses, 'TryRegenerateFrom', LDirty);
  Result := TrySolveInternal(AOptions, LDirty, AReport);
end;

function TGraph.TryRegenerateNegotiatedFrom(const APass: String;
  const AOptions: TGraphNegotiationOptions;
  out AReport: TGraphSelectiveNegotiationReport): Boolean;
var
  LPasses: TGraphPassLabels;
begin
  SetLength(LPasses, 1);
  LPasses[0] := APass;
  Result := TryRegenerateNegotiatedFrom(LPasses, AOptions, AReport);
end;

function TGraph.TryRegenerateNegotiatedFrom(
  const APasses: TGraphPassLabels;
  const AOptions: TGraphNegotiationOptions;
  out AReport: TGraphSelectiveNegotiationReport): Boolean;
var
  LActivePassIndices: TGraphPassIndices;
  LDirty: TPassSelection;
  LRequestedRootIndices: TGraphPassIndices;
begin
  if Assigned(FPassRoot) then
    Exit(FPassRoot.TryRegenerateNegotiatedFrom(
      APasses, AOptions, AReport));
  ValidateNegotiationOptions(AOptions,
    'TryRegenerateNegotiatedFrom');
  if Length(APasses) = 0 then
    raise EArgumentException.Create(
      'TryRegenerateNegotiatedFrom::at least one pass is required');
  EnsureInitialPass;

  //Resolve and close the complete scope before any solver or random state can
  //change. Duplicate and reordered labels collapse to stable index order.
  BuildDescendantPassSelection(APasses,
    'TryRegenerateNegotiatedFrom', LRequestedRootIndices,
    LActivePassIndices, LDirty);

  AReport := Default(TGraphSelectiveNegotiationReport);
  AReport.ScopeAlgorithmVersion :=
    WFC_SELECTIVE_NEGOTIATION_ALGORITHM_VERSION;
  AReport.RequestedRootIndices := LRequestedRootIndices;
  AReport.ActivePassIndices := LActivePassIndices;
  Result := TryNegotiateInternal(AOptions, LDirty,
    'TryRegenerateNegotiatedFrom', Seed, AReport.Search);
  AReport.TranscriptHash :=
    CalculateGraphSelectiveNegotiationTranscriptHash(
      AOptions, AReport);
end;

function TGraph.TrySolveInternal(const AOptions: TGraphSolveOptions;
  const ADirty: TPassSelection;
  out AReport: TGraphSolveReport): Boolean;
var
  LAssignments: TValueIndexMatrix;
  LCompletedChoices: TPassSelection;
  LExclusions: TPassAssignmentExclusions;
begin
  LExclusions := nil;
  Result := TrySolveAttempt(AOptions, ADirty, LExclusions, Seed,
    LCompletedChoices, LAssignments, AReport);
end;

function TGraph.TrySolveAttempt(const AOptions: TGraphSolveOptions;
  const ADirty: TPassSelection;
  const AExclusions: TPassAssignmentExclusions;
  const AEffectiveSeed: TGraphSeed;
  out ACompletedChoices: TPassSelection;
  out AAssignments: TValueIndexMatrix;
  out AReport: TGraphSolveReport): Boolean;
type
  TRandomStateArray = array of TRandomState;
  TEntryState = record
    Value: TGraphValue;
    Empty: Boolean;
    Generated: Boolean;
  end;
  TEntryStates = array of TEntryState;
  TPassEntryStates = array of TEntryStates;
  TEntryNeighborState = array[TGraphDirection] of TGraphEntry;
  TEntryNeighborStates = array of TEntryNeighborState;
  TPassNeighborStates = array of TEntryNeighborStates;
var
  LAssignment: TReferenceIntegerArray;
  LCommitted: Boolean;
  LDefinitionlessFailureFromSource: Boolean;
  LDefinitionlessSourcePass: Integer;
  LEntryIndex: Integer;
  LExecutionOrdinal: Integer;
  LExecutionPlan: TGraphPassIndices;
  LFinalValidationEntry: Integer;
  LFinalValidationPass: Integer;
  LFinalValidationValid: Boolean;
  LFullExecutionPlan: TGraphPassIndices;
  LGraph: TGraph;
  LHasNegotiableEntry: Boolean;
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
  LNeighborSnapshots: TPassNeighborStates;
  LStaged: TGraphValueMatrix;
  LTraceCauseEventId: Integer;
  LTraceRecorder: TGraphTraceRecorder;
  LTraceInterrupted: Boolean;
  LReferenceSink: TReferenceTraceEventSink;
  LTraceEvent: TGraphTraceEvent;
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
    Result.ConstraintIndex := -1;
  end;

  function AppendTraceEvent(const ASource: TGraphTraceEvent): Integer;
  begin
    Result := LTraceRecorder.Append(ASource);
  end;

  procedure ReserveTerminalTraceSlot;
  begin
    LTraceRecorder.ReserveTerminal;
  end;

  procedure InitializeReport;
  var
    I: Integer;
  begin
    AReport.Status := gssContradiction;
    AReport.Seed := AEffectiveSeed;
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
    AReport.Contradiction.ConstraintIndex := -1;
    SetLength(AReport.ExecutionOrder, 0);
    SetLength(AReport.Passes, FPasses.Count);
    AReport.TraceCaptured := AOptions.CaptureTrace;
    AReport.TraceHash := 0;
    SetLength(AReport.Trace, 0);
    AReport.TraceDelivery := Default(TGraphTraceDelivery);
    AReport.TraceDelivery.Version := WFC_TRACE_DELIVERY_VERSION;
    AReport.TraceDelivery.FailureEventId := -1;
    for I := 0 to High(AReport.Passes) do
    begin
      AReport.Passes[I].Decisions := 0;
      AReport.Passes[I].Propagations := 0;
      AReport.Passes[I].Contradictions := 0;
      AReport.Passes[I].Backtracks := 0;
      AReport.Passes[I].ExcludedAssignments := 0;
      AReport.Passes[I].Executed := False;
      AReport.Passes[I].ExecutionOrdinal := -1;
      AReport.Passes[I].Disposition := gpdNotRun;
      AReport.Passes[I].TraceStart := -1;
      AReport.Passes[I].TraceCount := 0;
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

  function ValidateStagedValueQuotas(const AGraph: TGraph;
    const AValues: TGraphValues; out AConstraintIndex,
    AEntryIndex: Integer): Boolean;
  var
    C, I, LCount: Integer;
    LConstraint: TGraphValueQuotaConstraint;
  begin
    //Independent public-value recount: do not reuse the numeric propagator
    //or its final checker to guard the compiler/solver publication boundary.
    AConstraintIndex := -1;
    AEntryIndex := -1;
    Result := True;
    if Length(AGraph.FValueQuotas) = 0 then Exit;
    ValidateDefinedModel(AGraph);
    if Length(AValues) <> AGraph.FEntries.Count then
      raise EInvalidOperation.Create('TrySolve::value quota assignment shape mismatch');
    for I := 0 to High(AValues) do
      if (AValues[I] = TGraphValue.Empty)
        or not ContainsGraphValue(AGraph.FValues, AValues[I]) then
      begin
        AConstraintIndex := 0;
        AEntryIndex := I;
        Exit(False);
      end;
    for C := 0 to High(AGraph.FValueQuotas) do
    begin
      LConstraint := AGraph.FValueQuotas[C];
      CheckGraphValueQuotaBounds(LConstraint.MinimumCount,
        LConstraint.MaximumCount);
      LCount := 0;
      for I := 0 to High(AValues) do
        if ContainsGraphValue(LConstraint.Values, AValues[I]) then
          Inc(LCount);
      if (LCount < LConstraint.MinimumCount)
        or (LCount > LConstraint.MaximumCount) then
      begin
        AConstraintIndex := C;
        //The violated bound describes this whole pass, not an arbitrary cell.
        Exit(False);
      end;
    end;
  end;

  function ValidateStagedConnectivity(const AGraph: TGraph;
    const AValues: TGraphValues; out AConstraintIndex,
    AEntryIndex: Integer): Boolean;
  var
    C, I, J, LRoot, LCell, LNext, LHead, LTail: Integer;
    LValueIndices, LProfiles, LQueue: TReferenceIntegerArray;
    LReached, LRequired: TReferenceByteArray;
    LConstraint: TGraphConnectivityConstraint;
    LDirection, LInverse: TGraphDirection;
    LNeighbor: TGraphEntry;
    LUnusedRequired: Boolean;
  begin
    //Independent public-value BFS: neither the analyzer's possible graph nor
    //its complete checker is used to guard the compiler/solver boundary.
    AConstraintIndex := -1;
    AEntryIndex := -1;
    Result := True;
    if Length(AGraph.FConnectivity) = 0 then Exit;
    ValidateDefinedModel(AGraph);
    AGraph.ValidateConnectivityShape(AGraph.FDimension.Width,
      AGraph.FDimension.Height, AGraph.FDimension.Depth);
    if Length(AValues) <> AGraph.FEntries.Count then
      raise EInvalidOperation.Create('TrySolve::connectivity assignment shape mismatch');
    //Use the same malformed-neighbor policy for reused providers as for a
    //fresh numeric compile, even when a foreign link has no open port.
    for I := 0 to Pred(AGraph.FEntries.Count) do
      for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
      begin
        LNeighbor := AGraph.FEntries[I][LDirection];
        if not Assigned(LNeighbor) then Continue;
        LNext := LNeighbor.Index;
        if (LNext < 0) or (LNext >= AGraph.FEntries.Count)
          or (AGraph.FEntries[LNext] <> LNeighbor) then
          raise EInvalidOperation.CreateFmt(
            'TrySolve::pass %d entry %d has an external neighbor',
            [AGraph.FPassIndex, I]);
      end;
    SetLength(LValueIndices, Length(AValues));
    SetLength(LProfiles, Length(AValues));
    SetLength(LQueue, Length(AValues));
    SetLength(LReached, Length(AValues));
    SetLength(LRequired, Length(AValues));
    for I := 0 to High(AValues) do
      LValueIndices[I] := FindValueIndex(AGraph, AValues[I]);
    for C := 0 to High(AGraph.FConnectivity) do
    begin
      LConstraint := AGraph.FConnectivity[C];
      LRoot := AGraph.CoordToIndex(LConstraint.Root.X,
        LConstraint.Root.Y, LConstraint.Root.Z);
      for I := 0 to High(AValues) do
      begin
        LReached[I] := 0;
        LRequired[I] := 0;
        LProfiles[I] := -1;
        for J := 0 to High(LConstraint.Values) do
          if AValues[I] = LConstraint.Values[J].Value then
          begin
            LProfiles[I] := J;
            if LConstraint.RequireAllParticipants
              or LConstraint.Values[J].RequiredByValue then
              LRequired[I] := 1;
            Break;
          end;
      end;
      LRequired[LRoot] := 1;
      for J := 0 to High(LConstraint.RequiredPositions) do
        with LConstraint.RequiredPositions[J] do
          LRequired[AGraph.CoordToIndex(X, Y, Z)] := 1;
      LHead := 0;
      LTail := 0;
      if LProfiles[LRoot] >= 0 then
      begin
        LQueue[LTail] := LRoot;
        Inc(LTail);
        LReached[LRoot] := 1;
      end;
      while LHead < LTail do
      begin
        LCell := LQueue[LHead];
        Inc(LHead);
        for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
        begin
          if not (LDirection in LConstraint.Values[LProfiles[LCell]].Openings) then
            Continue;
          LNeighbor := AGraph.FEntries[LCell][LDirection];
          if not Assigned(LNeighbor) then Continue;
          LNext := LNeighbor.Index;
          if (LNext < 0) or (LNext >= Length(AValues)) then Continue;
          if AGraph.FEntries[LNext] <> LNeighbor then Continue;
          if (LReached[LNext] <> 0) or (LProfiles[LNext] < 0) then Continue;
          LInverse := InverseOfDir(LDirection);
          if LNeighbor[LInverse] <> AGraph.FEntries[LCell] then Continue;
          if not (LInverse in LConstraint.Values[LProfiles[LNext]].Openings) then
            Continue;
          if not RuleAllows(AGraph, LValueIndices[LNext], LDirection,
            LValueIndices[LCell], LUnusedRequired) then Continue;
          if not RuleAllows(AGraph, LValueIndices[LCell], LInverse,
            LValueIndices[LNext], LUnusedRequired) then Continue;
          LReached[LNext] := 1;
          LQueue[LTail] := LNext;
          Inc(LTail);
        end;
      end;
      for I := 0 to High(AValues) do
        if (LRequired[I] <> 0) and (LReached[I] = 0) then
        begin
          AConstraintIndex := C;
          AEntryIndex := I;
          Exit(False);
        end;
    end;
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
    LConnectivityIndex, LProfileIndex, LPositionIndex: Integer;
    LConnectivity: TGraphConnectivityConstraint;
    LQuotaIndex, LQuotaValueIndex: Integer;
    LQuota: TGraphValueQuotaConstraint;

    function RequirementMatches(
      const ARequirementIndex: Integer): Boolean;
    var
      LCount, LResolvedIndex, LTermIndex: Integer;
      LIsCount: Boolean;
      LMatchedIndices: TGraphPassIndices;
      LRequirement: TGraphRuleGroup.TPassRequirement;
      LTermSourcePass: Integer;
      LTermSourceValue: TGraphValue;
    begin
      Result := False;
      LRequirement := LGroup.FPassRequirements[ARequirementIndex];
      LTermSourcePass := LRequirement.PassIndex;
      if LRequirement.Kind = prkMapped then
        Exit(AGraph.MappedRequirementMatches(LCell,
          FPasses[LTermSourcePass], LRequirement.MappedQuery,
          AStaged[LTermSourcePass], True));
      LIsCount := LRequirement.Kind = prkCount;
      LCount := 0;
      if LIsCount and (LRequirement.CountMode = gpcmDistinctCells) then
        SetLength(LMatchedIndices, Length(LRequirement.Terms));
      for LTermIndex := 0 to High(LRequirement.Terms) do
        if AGraph.ResolveOffsetIndex(LCell,
          LRequirement.Terms[LTermIndex].Offset, LResolvedIndex) then
        begin
          LTermSourceValue := AStaged[LTermSourcePass][LResolvedIndex];
          if (LTermSourceValue <> TGraphValue.Empty)
            and ContainsGraphValue(
              LRequirement.Terms[LTermIndex].Values,
              LTermSourceValue) then
          begin
            if not LIsCount then
              Exit(True);
            RecordGraphPassCountMatch(LRequirement.CountMode,
              LResolvedIndex, LCount, LMatchedIndices);
            if LCount > LRequirement.MaximumMatches then
              Exit(False);
          end;
        end;
      if LIsCount then
        Result := LCount >= LRequirement.MinimumMatches;
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
    if LTraceRecorder.Enabled then
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

    AGraph.ValidateConnectivityShape(AGraph.FDimension.Width,
      AGraph.FDimension.Height, AGraph.FDimension.Depth);
    SetLength(AModel.Connectivity, Length(AGraph.FConnectivity));
    for LConnectivityIndex := 0 to High(AGraph.FConnectivity) do
    begin
      LConnectivity := AGraph.FConnectivity[LConnectivityIndex];
      with AModel.Connectivity[LConnectivityIndex] do
      begin
        RootCell := AGraph.CoordToIndex(LConnectivity.Root.X,
          LConnectivity.Root.Y, LConnectivity.Root.Z);
        RequireAllParticipants := LConnectivity.RequireAllParticipants;
        SetLength(RequiredCells, Length(LConnectivity.RequiredPositions));
        for LPositionIndex := 0 to High(RequiredCells) do
          with LConnectivity.RequiredPositions[LPositionIndex] do
            RequiredCells[LPositionIndex] := AGraph.CoordToIndex(X, Y, Z);
        SetLength(Profiles, Length(LConnectivity.Values));
        for LProfileIndex := 0 to High(Profiles) do
        begin
          Profiles[LProfileIndex].ValueIndex := FindValueIndex(AGraph,
            LConnectivity.Values[LProfileIndex].Value);
          Profiles[LProfileIndex].RequiredByValue :=
            LConnectivity.Values[LProfileIndex].RequiredByValue;
          Profiles[LProfileIndex].Ports := 0;
          for LDirection := Low(TGraphDirection) to High(TGraphDirection) do
            if LDirection in LConnectivity.Values[LProfileIndex].Openings then
              Profiles[LProfileIndex].Ports := Profiles[LProfileIndex].Ports
                or Byte(1 shl Ord(LDirection));
        end;
      end;
    end;

    SetLength(AModel.ValueQuotas, Length(AGraph.FValueQuotas));
    for LQuotaIndex := 0 to High(AGraph.FValueQuotas) do
    begin
      LQuota := AGraph.FValueQuotas[LQuotaIndex];
      with AModel.ValueQuotas[LQuotaIndex] do
      begin
        MinimumCount := LQuota.MinimumCount;
        MaximumCount := LQuota.MaximumCount;
        SetLength(Values, Length(LQuota.Values));
        for LQuotaValueIndex := 0 to High(Values) do
          Values[LQuotaValueIndex] := FindValueIndex(AGraph,
            LQuota.Values[LQuotaValueIndex]);
      end;
    end;

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
        if Length(AStaged[LSourcePassIndex]) <>
          FPasses[LSourcePassIndex].FEntries.Count then
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
        if LTraceRecorder.Enabled and (not LAllowed) then
          AInitialTraceCauses[LTraceIndex] := gtckCallerLock;
        if LAllowed and AGraph.FEntries[LCell].FHasAllowedValues then
        begin
          LAllowed := ContainsGraphValue(
            AGraph.FEntries[LCell].FAllowedValues,
            AGraph.FValues[LValue]);
          if LTraceRecorder.Enabled and (not LAllowed) then
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
          if LTraceRecorder.Enabled and (not LAllowed) then
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
      rckExcludedAssignment:
        Result := gckExcludedAssignment;
      rckConnectivity:
        Result := gckConnectivity;
      rckValueQuota:
        Result := gckValueQuota;
    else
      Result := gckNone;
    end;
  end;

  function LastPassTraceEvent(const APassIndex: Integer): Integer;
  begin
    Result := LTraceRecorder.LastPassEvent(APassIndex);
  end;

  procedure CopyPassReport(const APassIndex: Integer;
    const AReference: TReferenceSolveReport);
  begin
    AReport.Passes[APassIndex].Decisions := AReference.Decisions;
    AReport.Passes[APassIndex].Propagations := AReference.Propagations;
    AReport.Passes[APassIndex].Contradictions :=
      AReference.Contradictions;
    AReport.Passes[APassIndex].Backtracks := AReference.Backtracks;
    AReport.Passes[APassIndex].ExcludedAssignments :=
      AReference.ExcludedAssignments;
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
    AReport.Contradiction.ConstraintIndex :=
      AReference.Contradiction.ConstraintIndex;
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
    I, LConstraint, LFailedEntry: Integer;
  begin
    SetLength(LStaged[APassIndex], AGraph.FEntries.Count);
    for I := 0 to Pred(AGraph.FEntries.Count) do
      if AGraph.FEntries[I].Empty then
        LStaged[APassIndex][I] := TGraphValue.Empty
      else
        LStaged[APassIndex][I] := AGraph.FEntries[I].Value;
    if not ValidateStagedConnectivity(AGraph, LStaged[APassIndex],
      LConstraint, LFailedEntry) then
      raise EInvalidOperation.CreateFmt(
        'TrySolve::reused pass %d violates connectivity %d at entry %d; include it in regeneration',
        [APassIndex, LConstraint, LFailedEntry]);
    if not ValidateStagedValueQuotas(AGraph, LStaged[APassIndex],
      LConstraint, LFailedEntry) then
      raise EInvalidOperation.CreateFmt(
        'TrySolve::reused pass %d violates value quota %d at entry %d; include it in regeneration',
        [APassIndex, LConstraint, LFailedEntry]);
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
    D: TGraphDirection;
  begin
    SetLength(LSnapshots, FPasses.Count);
    //Only opt-in constrained layers need topology journaling. Ordinary
    //models retain their old per-entry snapshot and trace behavior.
    SetLength(LNeighborSnapshots, FPasses.Count);
    for I := 0 to Pred(FPasses.Count) do
    begin
      SetLength(LSnapshots[I], FPasses[I].FEntries.Count);
      if Length(FPasses[I].FConnectivity) <> 0 then
        SetLength(LNeighborSnapshots[I], FPasses[I].FEntries.Count);
      for J := 0 to Pred(FPasses[I].FEntries.Count) do
      begin
        LSnapshots[I][J].Value := FPasses[I].FEntries[J].Value;
        LSnapshots[I][J].Empty := FPasses[I].FEntries[J].Empty;
        LSnapshots[I][J].Generated := FPasses[I].FEntries[J].Generated;
        if Length(LNeighborSnapshots[I]) <> 0 then
          for D := Low(TGraphDirection) to High(TGraphDirection) do
            LNeighborSnapshots[I][J][D] := FPasses[I].FEntries[J][D];
      end;
    end;
  end;

  procedure RestoreEntries;
  var
    I, J: Integer;
    D: TGraphDirection;
  begin
    for I := 0 to High(LSnapshots) do
      for J := 0 to High(LSnapshots[I]) do
      begin
        FPasses[I].FEntries[J].RestoreValueState(
          LSnapshots[I][J].Value,
          LSnapshots[I][J].Empty,
          LSnapshots[I][J].Generated);
        if Length(LNeighborSnapshots[I]) <> 0 then
          for D := Low(TGraphDirection) to High(TGraphDirection) do
            FPasses[I].FEntries[J].RestoreNeighborState(D,
              LNeighborSnapshots[I][J][D]);
      end;
  end;

  function CommitStagedEntries(out AFinalValidationPass,
    AFinalValidationEntry: Integer): Boolean;
  var
    I, J, K, LConstraint: Integer;
    D: TGraphDirection;

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

      for K := 0 to High(LExecutionPlan) do
      begin
        I := LExecutionPlan[K];
        if not ValidateStagedConnectivity(FPasses[I], LStaged[I],
          LConstraint, AFinalValidationEntry) then
        begin
          AFinalValidationPass := I;
          Exit(False);
        end;
        if not ValidateStagedValueQuotas(FPasses[I], LStaged[I],
          LConstraint, AFinalValidationEntry) then
        begin
          AFinalValidationPass := I;
          Exit(False);
        end;
      end;

      Result := DoValidateCommit(AFinalValidationPass,
        AFinalValidationEntry);
      if not Result then
        Exit;

      //A hook can also rewrite an entry that has already been committed, or a
      //caller lock that is intentionally skipped. The domain final validator
      //runs before this guard as well, so it cannot mutate live state and then
      //publish an unchecked candidate by returning success.
      for I := 0 to Pred(FPasses.Count) do
        for J := 0 to Pred(FPasses[I].FEntries.Count) do
        begin
          if not MatchesExpectedState(I, J) then
            raise EInvalidOperation.CreateFmt(
              'TrySolve::commit hook mutated pass %d entry %d', [I, J]);
          if Length(LNeighborSnapshots[I]) <> 0 then
            for D := Low(TGraphDirection) to High(TGraphDirection) do
              if FPasses[I].FEntries[J][D] <> LNeighborSnapshots[I][J][D] then
                raise EInvalidOperation.CreateFmt(
                  'TrySolve::commit hook mutated connectivity topology in pass %d entry %d',
                  [I, J]);
        end;
      //Legacy public rule arrays remain writable. Recheck live compatibility
      //after the domain hook too, including preserved providers. Such model
      //edits are caller side effects, but must not publish a disconnected
      //candidate merely because its values and neighbor links stayed equal.
      for I := 0 to Pred(FPasses.Count) do
      begin
        if not ValidateStagedConnectivity(FPasses[I], LStaged[I],
          LConstraint, AFinalValidationEntry) then
          raise EInvalidOperation.CreateFmt(
            'TrySolve::commit hook invalidated connectivity %d in pass %d entry %d',
            [LConstraint, I, AFinalValidationEntry]);
        if not ValidateStagedValueQuotas(FPasses[I], LStaged[I],
          LConstraint, AFinalValidationEntry) then
          raise EInvalidOperation.CreateFmt(
            'TrySolve::commit hook invalidated value quota %d in pass %d entry %d',
            [LConstraint, I, AFinalValidationEntry]);
      end;
      Result := True;
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
  if (Length(AExclusions) <> 0)
    and (Length(AExclusions) <> FPasses.Count) then
    raise EInvalidOperation.Create(
      'TrySolve::pass exclusions do not match the pipeline');

  SetLength(ACompletedChoices, FPasses.Count);
  SetLength(AAssignments, FPasses.Count);
  //Dynamic-array resizing preserves existing elements when the length is
  //unchanged. Every round must publish only choices completed in that round;
  //otherwise an exhausted pass could masquerade as a stale choice frame.
  for I := 0 to Pred(FPasses.Count) do
  begin
    ACompletedChoices[I] := 0;
    AAssignments[I] := nil;
  end;

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
    end;
  LSavedPassIndex := FCurPassIndex;
  LRootRandomState := FRandomState;
  SetLength(LRandomStates, FPasses.Count);
  for I := 0 to Pred(FPasses.Count) do
    LRandomStates[I] := FPasses[I].FRandomState;
  LTraceRecorder := TGraphTraceRecorder.Create(AOptions.CaptureTrace,
    FTraceSink, AReport);
  LTraceInterrupted := False;
  FRunning := True;
  FExecutingPassIndex := -1;
  try
   try
    LTraceRecorder.BeginDelivery;
    //Reused-provider events are still the same chronological prefix, but
    //observers now run inside the non-reentrant transaction boundary.
    for I := 0 to Pred(FPasses.Count) do
      if ADirty[I] = 0 then
      begin
        LTraceEvent := NewTraceEvent(gtekPassSkipped, gtckTransaction, I);
        AppendTraceEvent(LTraceEvent);
      end;
    EnsureSeedInitialized;
    for I := 0 to Pred(FPasses.Count) do
      if ADirty[I] <> 0 then
      begin
        BuildPassRandomState(I, AEffectiveSeed, FPasses[I].FRandomState);
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

      if Length(AExclusions) <> 0 then
      begin
        SetLength(LModel.ExcludedAssignments,
          Length(AExclusions[LPassIndex]));
        for I := 0 to High(AExclusions[LPassIndex]) do
          LModel.ExcludedAssignments[I] := Copy(
            AExclusions[LPassIndex][I], 0,
            Length(AExclusions[LPassIndex][I]));
      end;

      LTraceRecorder.ConfigurePass(LPassIndex, LPassBeginEvent,
        LGraph.FEntries.Count, LGraph.FValues, LInitialTraceCauses,
        LInitialTraceDependencyPasses);
      LReferenceSink := nil;
      if LTraceRecorder.Enabled then
        LReferenceSink := LTraceRecorder.ReceiveReferenceEvent;
      if not SolveReferenceModel(LModel, AOptions.MaxBacktracks,
        False, LReferenceSink, LGraph.RandomIndex,
        LAssignment, LReferenceReport) then
      begin
        CopyPassReport(LPassIndex, LReferenceReport);
        SetReferenceFailure(LPassIndex, LReferenceReport);
        LTraceEvent := NewTraceEvent(gtekPassFailed,
          gtckTransaction, LPassIndex);
        LTraceEvent.CauseEventId := LastPassTraceEvent(LPassIndex);
        AppendTraceEvent(LTraceEvent);
        Exit(False);
      end;
      CopyPassReport(LPassIndex, LReferenceReport);
      AReport.Passes[LPassIndex].Disposition := gpdSolved;

      //Ordinary TrySolve passes no exclusion matrix and must not retain an
      //extra O(passes * cells) assignment copy. Negotiated round one passes a
      //full matrix whose rows are initially empty, enabling choice capture.
      if Length(AExclusions) <> 0 then
      begin
        //A pass made entirely of caller-owned locks is not a choice frame: an
        //exact exclusion could never change it and would only spend budget.
        //Generated prior output remains negotiable on a fresh transaction.
        LHasNegotiableEntry := False;
        for LEntryIndex := 0 to Pred(LGraph.FEntries.Count) do
          if LGraph.FEntries[LEntryIndex].Empty
            or LGraph.FEntries[LEntryIndex].Generated then
          begin
            LHasNegotiableEntry := True;
            Break;
          end;
        if LHasNegotiableEntry then
        begin
          ACompletedChoices[LPassIndex] := 1;
          AAssignments[LPassIndex] := Copy(LAssignment, 0,
            Length(LAssignment));
        end;
      end;

      SetLength(LStaged[LPassIndex], LGraph.FEntries.Count);
      for LEntryIndex := 0 to Pred(LGraph.FEntries.Count) do
        LStaged[LPassIndex][LEntryIndex] :=
          LGraph.FValues[LAssignment[LEntryIndex]];
    end;

    ReserveTerminalTraceSlot;
    LFinalValidationPass := -1;
    LFinalValidationEntry := -1;
    LFinalValidationValid := CommitStagedEntries(
      LFinalValidationPass, LFinalValidationEntry);
    if not LFinalValidationValid then
    begin
      RestoreEntries;
      if (LFinalValidationPass < 0)
        or (LFinalValidationPass >= FPasses.Count) then
        raise EInvalidOperation.CreateFmt(
          'TrySolve::final validator returned invalid pass %d',
          [LFinalValidationPass]);
      if ADirty[LFinalValidationPass] = 0 then
        raise EInvalidOperation.CreateFmt(
          'TrySolve::final validator returned inactive pass %d',
          [LFinalValidationPass]);
      if (LFinalValidationEntry < -1)
        or (LFinalValidationEntry >=
          FPasses[LFinalValidationPass].FEntries.Count) then
        raise EInvalidOperation.CreateFmt(
          'TrySolve::final validator returned invalid entry %d',
          [LFinalValidationEntry]);

      AReport.Status := gssContradiction;
      AReport.FailedPassIndex := LFinalValidationPass;
      AReport.Contradiction.Kind := gckFinalValidation;
      AReport.Contradiction.PassIndex := LFinalValidationPass;
      AReport.Contradiction.EntryIndex := LFinalValidationEntry;
      AReport.Contradiction.NeighborIndex := -1;
      AReport.Contradiction.HasDirection := False;
      AReport.Contradiction.Direction := gdNorth;
      AReport.Contradiction.DependencyPassIndex := -1;
      Inc(AReport.Passes[LFinalValidationPass].Contradictions);
      AReport.Passes[LFinalValidationPass].Disposition := gpdFailed;

      LTraceEvent := NewTraceEvent(gtekContradiction,
        gtckFinalValidation, LFinalValidationPass);
      LTraceEvent.CauseEventId := LastPassTraceEvent(
        LFinalValidationPass);
      LTraceEvent.EntryIndex := LFinalValidationEntry;
      LTraceCauseEventId := AppendTraceEvent(LTraceEvent);
      LTraceEvent := NewTraceEvent(gtekPassFailed,
        gtckTransaction, LFinalValidationPass);
      LTraceEvent.CauseEventId := LTraceCauseEventId;
      AppendTraceEvent(LTraceEvent);
      Exit(False);
    end;
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
    LTraceEvent.CauseEventId := Pred(LTraceRecorder.EventCount);
    AppendTraceEvent(LTraceEvent);
    Result := True;
   except
    LTraceInterrupted := True;
    raise;
   end;
  finally
    try
      try
        try
          if not LCommitted then
          begin
            LTraceEvent := NewTraceEvent(gtekPipelineRollback,
              gtckTransaction, -1);
            LTraceEvent.CauseEventId := Pred(LTraceRecorder.EventCount);
            AppendTraceEvent(LTraceEvent);
          end;
        except
          LTraceInterrupted := True;
          raise;
        end;
      finally
        //An exhausted event identity cannot encode another rollback event,
        //but the delivery footer must still identify the interrupted prefix.
        LTraceRecorder.Finish(AReport, LTraceInterrupted);
      end;
    finally
      //Observer failures are contained above, and even a producer allocation
      //or event-identity exception cannot strand RNG or Running state here.
      if not LCommitted then
      begin
        FRandomState := LRootRandomState;
        for I := 0 to Pred(FPasses.Count) do
          FPasses[I].FRandomState := LRandomStates[I];
      end;
      FExecutingPassIndex := -1;
      FCurPassIndex := LSavedPassIndex;
      FCurPass := PassLabelFromIndex(LSavedPassIndex);
      FRunning := False;
      LTraceRecorder.Free;
    end;
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

  for I := 0 to Pred(FPasses.Count) do
  begin
    if Length(FPasses[I].FConnectivity) <> 0 then
      raise EInvalidOperation.Create(
        'Run::connectivity requires the TrySolve family');
    if Length(FPasses[I].FValueQuotas) <> 0 then
      raise EInvalidOperation.Create(
        'Run::value quotas require the TrySolve family');
  end;

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
  LOldLayout: TWfcLatticeLayout;
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
    LOldLayout := FLayout;
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
    FLayout := LegacyGraphLayout(0, 0, 0, FWrap);
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
      FLayout := LOldLayout;
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
    FConnectivity := nil;
    FValueQuotas := nil;
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
  FLayout := LegacyGraphLayout(0, 0, 0, FWrap);
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
  FConnectivity := nil;
  FValueQuotas := nil;
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
  FLayout.Wrap := FWrap;

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

