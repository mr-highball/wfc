# Rooted connectivity v1: domain propagation with port witnesses

Status: experimental, opt-in; `WFC_GRAPH_CONNECTIVITY_VERSION = 1`.
Implementation, oracle, fixtures, and demonstrations are project-owned
Pascal under the repository's MIT license.

## Question and baseline

Can pass-local reachability force useful spatial decisions before a complete
assignment exists, while remaining compatible with reversible WFC domains?
The baseline is adjacency-only generation followed by a reachability
validator. Local matching edges alone can form disconnected cycles or leave
required destinations unreachable. Existing WFC settlement/voxel libraries
retain their former semantics; this experiment does not reinterpret them.

The mechanism combines ordinary possible-graph reachability and articulation
analysis with value-level directional port profiles. No novelty claim is
made for graph traversal or cut-vertex algorithms. The reproducible question
is their integration with this Pascal solver's domain trail, finite pass
transactions, and independently checked public assignments.

## Contract and soundness argument

For one descriptor, let `D(c)` be the current domain at cell `c`. A profile
marks participation, six possible openings, and optional required-by-value
status. There is one fixed root and zero or more fixed required cells.
Selected required-by-value participants, or all participants in the explicit
all-participants mode, must connect to the root. Optional components are
permitted in required-only mode.

Build the possible graph with participating cells and edges witnessed by
compatible candidates with reciprocal ports and reciprocal physical neighbor
links. Every edge in a satisfying completion must be one of these possible
edges. Therefore:

1. A mandatory cell outside the possible root component proves failure.
2. A candidate that requires connection cannot be selected outside that
   component and can be removed.
3. If removing a possible cut vertex separates a mandatory cell from the
   root, every satisfying completion must select a participant at that cut
   vertex. Nonparticipant candidates there can be removed.

Root/terminal participation is mandatory. A cell is also mandatory when
every remaining candidate requires connection. Deletions can expose new
mandatory cells or cuts, so local and connectivity propagation iterate to a
fixed point. Iterative lowlink analysis avoids per-cell recursive call depth.
All deletions enter the existing trail. Recomputing from restored domains
avoids retaining component conclusions from an abandoned branch.

These deductions preserve all satisfying completions. They do not prove
that the surviving domains have one. Different possible edges may rely on
different values at their shared endpoint. The exact complete-assignment
check remains inside search and can reject a full choice and recover.
A second traversal at the public-value boundary is separate from that
numeric implementation.

## Counterexample and explicit nonclaims

A center cell can offer an east-only participant and a west-only
participant. Its possible graph may connect both neighboring terminals,
although no single center value can connect them simultaneously. This is a
port-choice correlation, not permission to return a disconnected solution.
The conformance suite includes correlated-port failure and branch recovery.

The propagator does not claim generalized arc consistency, shortest routes,
minimum edits, directed flow, a universal time bound, or a proof of
infeasibility when a search budget is exhausted. Multiple labeled
descriptors are independent AND conditions; they do not imply that every
network uses a separate value or that every selected participant must be in
every network. Zero-port participants and aliased/self neighbors are part of
the defined semantics, not special success shortcuts.

## Reproducible fixtures

`test/wfc_connectivity_reference_test.lpr` contains an independent complete
assignment enumerator and BFS oracle. It does not call the production
connectivity analyzer to decide whether an assignment is valid. Its finite
matrix includes:

- all nonempty domains for three-value, three-cell corridors along X, Y,
  and Z, under fixed-terminal and required-by-value/all-participant modes;
- four-cell squares with alternative domains, wrapping, and locks;
- all 512 directional compatibility matrices for a three-value, two-cell
  relation, with the reverse transposed;
- targeted reciprocal port/link checks, optional components, multiple AND
  descriptors, excluded-assignment recovery, and trail restoration;
- a 12,000-cell vertical corridor with two values, forced entirely by
  propagation with zero decisions and zero backtracks;
- invalid descriptor fields and checked flattened-storage overflow cases.

The oracle compares satisfiability for 5,277 tiny models and independently
validates every returned solution. The checked native run currently reports
10,676 passing checks on FPC 3.2.2 and the development compiler. Browser
tests additionally inject malformed host numbers and Boolean fields; those
are host-input checks, not an enlargement of the finite satisfiability proof.
The maintained FPC-server browser run passes 10,726 checks against the same
5,277 oracle models. The public facade adds 66 native / 70 browser checks,
and the causal/transaction suite adds 233 checks on each target.
The 12,000-cell fixture establishes that traversal is iterative at that
depth. It is neither a maximum supported size nor a performance comparison.

`test/wfc_connectivity_test.lpr` covers public metadata ownership and lifecycle
behavior. `test/wfc_connectivity_trace_test.lpr` adds an alternative-value
corridor, labeled-clause failure, rehashed diagnostic mutations, capture
parity, rollback, and invalid reused-provider preflight. Its seed-55 trace
golden is `71BEEAB4`. Unconstrained core and trace goldens remain fixed.

Review found a concrete boundary failure in the initial implementation: an
overridden final commit hook could disconnect a neighbor link after the
staged BFS and return success, leaving values unchanged. Checking only entry
values did not catch it. The implementation now journals topology for
constrained passes, rejects changed links, and silently restores them on
rollback. Regression cases cover accepting, rejecting, and exceptional hooks
on both fully regenerated and reused constrained layers, including their
random streams. This is a model-integrity guard, not stronger propagation.
A second reproduction changed the legacy public rule array instead of a
neighbor. A final public traversal now rechecks live compatibility after the
domain hook, including preserved passes. Those arbitrary caller model edits
are not generically rolled back; regression tests restore them explicitly
before retrying. Entry state, constrained topology, and random streams retain
their transactional guarantees.

The separate Connected Routes demo applies the primitive to two pass DAGs:
terrain → roads → housing, and structure → circulation → features. Its
validator traverses captured public cells with its own port logic. Closing
one bridge/shaft edits availability in the routing layer; selective repair
preserves the immutable upstream terrain/structure layer. It does not
pretend that changing terrain itself can leave that terrain unchanged.

## Recorded native export matrix

The [portable evidence CSV](rooted-connectivity-v1.csv) records the finite
Connected Routes export review from 2026-09-05. Each row pairs checked
Win32/i386 runs of FPC 3.2.2 and development FPC
`3.3.1-20634-gd7f522a561`: **50 cases per compiler, 100 successful exports,
and 50 exact byte-identical SVG pairs**. The matrix is both cases (`town`
and `circulation`), seeds `0`, `1`, `55`, and `4294967295`, portals `first`,
`second`, and `both`, and both participant policies, plus one seed-0
first-to-second selective repair per case. All runs used trace capture and
unchanged local/pass backtrack budgets of **4096/32**. No failed or
budget-exhausted case was discarded; none occurred, and no partial output
files remained.

The paired runs also agreed on the recorded counters: witness cells ranged
from 10 to 16 and participants from 13 to 24. All-participants runs had zero
disconnected participants; all 24 required-only cases retained optional
disconnected participants, ranging from 1 to 7. Every reported pass
backtrack count was zero. That is the inter-pass counter, not a claim of
zero local search, decisions, or local backtracks. Both repairs reported
provider reuse: town `61943F3F` to `5C735E7A`, circulation `9F2CC7A4` to
`47E47400`. These are finite output/correctness observations, not a general
performance or success-rate claim. This matrix does not measure runtime or
peak storage, compare against adjacency-only generation, or test browser
export behavior.

CSV Boolean fields use `1`/`0`. An empty `RepairTo` means a fresh solve;
otherwise `Portal` is the baseline and `RepairTo` is the requested repair.
Counters and `Signature` describe the final result. `CurrentExitCode` and
`StableExitCode` retain both process outcomes. `ExactByteMatch` records a
literal comparison of the two emitted files; `SvgSHA256` identifies that
artifact rather than replacing the comparison. No machine-specific paths
are included.

To reproduce, create fresh `build/research-connectivity/units`, `bin`, and
`svg` directories, then compile from the repository root with each compiler
in separate output directories:

```sh
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Futools -Fuexamples/passes/06_ConnectedRoutes -FUbuild/research-connectivity/units -FEbuild/research-connectivity/bin examples/passes/06_ConnectedRoutes/ConnectedRoutes.lpr
build/research-connectivity/bin/ConnectedRoutes --case town --seed 0 --portal first --backtracks 4096 --pass-backtracks 32 --trace --svg build/research-connectivity/svg/town-0-first-all.svg
build/research-connectivity/bin/ConnectedRoutes --case circulation --seed 0 --portal first --repair-to second --backtracks 4096 --pass-backtracks 32 --trace --svg build/research-connectivity/svg/circulation-0-repair.svg
```

Use the executable's `.exe` suffix on Windows. Repeat with each CSV row's
case, seed, and portal; add `--required-only` when
`RequireAllParticipants=0`, and `--repair-to` when `RepairTo` is nonempty.
Use a new output filename for every run: the exporter deliberately refuses
to overwrite an existing path. Compare paired SVG bytes and console
counters, retaining any changed outcome instead of silently increasing the
budgets. The separate native gates passed 45 demo checks, 55 process checks,
and 233 causal/transaction checks on each compiler.

## Stopping rule and next measurements

Each small oracle model has a finite assignment space and fixed search
budget; an unexpected exhausted budget is a failed comparison, not discarded
data. All malformed-model tests must reject before unsafe indexing. Existing
unconstrained conformance results must remain unchanged. Demo failures must
withhold stale public exports while leaving core rollback available.

Before claiming a speed advantage, compare equivalent full models with and
without propagation across a published seed set. Record domain removals,
decisions, contradictions, backtracks, runtime, and peak storage, including
failures. The current evidence establishes finite correctness and early
bottleneck deduction, not general relative efficiency. Candidate-specific
port filtering, richer cut explanations, authored/trained profile tooling,
portable recipe encoding, and chunk-boundary connectivity summaries remain
future work.
