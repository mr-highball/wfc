# Streaming causal traces v1

This record describes an engineering change to WFC's trace representation,
not a new search algorithm or a claim of priority. All implementation and
fixtures are project-owned Pascal under the repository's MIT license.

## Hypothesis and baseline

The same deterministic attempted search can be observed without retaining
every event inside the solver. Removing that history must not change event
fields, IDs, cause links, hashes, assignments, counters, or random-stream
positions. Rejecting an observation must not reject the graph transaction.

The comparison oracle is ordinary full capture on the same fixed model and
seed. Tests compare every event field, not only its diagnostic hash. Existing
goldens remain separate compatibility anchors: dependency trace `46715F2C`,
inspector trace `73C4B9A2`, and late earlier-pass failure `B27D0AE0`. This finite
evidence is not an exhaustive proof for every user model or callback.

## Removing the hidden history

A public suffix buffer alone would leave two hidden costs: the numeric
kernel's complete trace and an event-by-event local/global ID map. The new
path removes both. A graph-owned recorder receives numeric events immediately,
maps them into public events, updates the unchanged hash, and delivers them to
a borrowed observer. Full public retention is an independent option.

Within one pass, each kernel event maps to exactly one public event without
interleaving another pass. If `base` is the public event count immediately
before the first kernel callback, local event `i` maps to `base + i`.
Earlier local cause IDs use that same translation. Consecutive IDs are checked.
Cross-pass initial causes instead use the explicit last event of the provider;
late commit rejection remains attributed to the earlier staged pass.

Public cause refinement needs the last changing event for each cell and the
backtrack that enables a retry, not arbitrary historical lookup. These
summaries retain cause kind, dependency identity where relevant, and
connectivity constraint identity. Initial exclusions still use per-state
classifications so caller domains, locks, and provider constraints remain
distinguishable. The following retained costs are independent of search length:

- initial cause tables: O(cells × values);
- dynamic causal summaries and pass metadata: O(cells + passes);
- an optional caller-sized recent-event ring: O(capacity).

These are trace-related bounds, not total solver-memory bounds. The ordinary
model, domains, reversible trail, and outer negotiation/restart metadata retain
their own costs. Full capture, collecting observers, and a terminal that saves
printed output can still retain O(events) evidence. No peak-memory or runtime
speedup measurement is claimed here.

## Finite experiments

The maintained native gate runs these sources with range, overflow, and I/O
checks. The same Pascal sources run under pas2js in a real browser served and
checked by the included native FPC tools.

| Fixture | Fixed comparison and stopping rule |
| --- | --- |
| [Numeric kernel](../../test/wfc_trace_reference_stream_test.lpr) | Full, sink-only, dual, and disabled modes over ordinary choices, locks/domains, initial failure, a backtracking ring, exact exclusions, and connectivity. A separate 4,096-cell case delivers exactly 4,097 events to a count-only observer with an empty report trace. Run each declared case once. |
| [Public graph](../../test/wfc_trace_stream_test.lpr) | Thirteen fixture kinds plus the ring's zero-backtrack variant use fixed seeds 0, 17, 42, or 55. Compare sink-only and dual modes with full capture, including definitionless copies, selective reuse, late commit failure, negotiation, selective negotiation, and restarts. Run the declared failure phases and producer-interruption case once each. |
| [Window](../../test/wfc_trace_window_test.lpr) | Test ring wrapping, zero capacity, resets, detached copies, malformed callbacks, and every capacity from zero through the complete trace length of one seed-17 graph. Reconstruct and independently validate only an untruncated trace with matching pass metadata. |
| [Shared inspector](../../examples/passes/02_TraceInspector/README.md) | The original seed-zero 27-event transaction is delivered live; retain IDs 22–26, report 22 dropped events, and identify the exact outside-window causes. Compare all output with the native/browser fixture. |

The focused checked FPC runs (3.2.2 and the development compiler) pass 9,251
kernel checks, 10,937 public-delivery checks, and 3,257 window checks. Browser
execution passes the same kernel/public counts and 3,269 window checks, including
12 additional JavaScript host-number checks. These counts describe this fixed
revision's assertions, not independent models or measured performance samples.

## Failure and completeness boundaries

Begin, event, and End observer failures are tested separately. The graph
records the first failed phase and accepted prefix, detaches that observer for
the current attempt, and continues producing the same result and numeric hash.
Each outer negotiation/restart attempt has a separate delivery boundary and
starts again at event zero. A later attempt may retry the attached observer.

Successful delivery is not a successful solve: a contradiction ends in
rollback. Successful delivery is also not complete retention: a ring that drops
an event cannot claim a complete trace. Outside-window causes retain their
original IDs and are not rewritten as external roots. Window acceptance checks
the observation protocol, not full graph semantics or independent trace hashes.

Callbacks are synchronous observation, not mutation or pause/resume hooks.
Producer exceptions yield an interrupted prefix when a footer can still be
delivered. Checked event-identity exhaustion and catastrophic allocation failure
remain real limits; this change does not create an infinite-ID trace format.
Persisted artifacts, asynchronous backpressure, interactive stepping, domain
reconstruction, and minimal contradiction explanations remain future work.

See the [streaming API contract](../trace-streaming.md) and
[complete Trace-v1 contract](../traces.md) for exact usage and validation rules.
