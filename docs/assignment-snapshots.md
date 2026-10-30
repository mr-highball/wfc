# Assignment snapshots

`TGraph.CaptureAssignments` returns detached version1 assignment data for every
pass. Each cell retains its value, empty flag and generated flag. The distinction
matters: assigning public `Entry.Value` creates a caller-owned lock, while solver
output remains eligible for later authorized regeneration.

`TGraph.RestoreAssignments` restores those ownership flags without solving. Supply
a fresh graph with the same configured rules, values, domains, layouts and seed.
The API checks version, pass count/labels, complete layouts, cell counts, known
values, target domains and explicit budgets. All target entries must be empty.
Malformed data, resource rejection or a borrowed observer exception leaves target
assignments unchanged. Publication bypasses entry setter hooks after validation.

```pascal
Snapshot := AcceptedGraph.CaptureAssignments;
// The application builds FreshGraph with the matching configuration and seed.
FreshGraph.RestoreAssignments(Snapshot);
```

Both APIs address the root pipeline, including when called through `PassGraph`.
They preserve the selected pass. The observer uses existing preparation/commit
visits and never receives a search phase; capture/restoration does not run WFC.
Reentry and graph mutation through ordinary graph APIs are rejected while the
operation runs. Observers must not mutate input records or entries, free owners,
or draw the graph's randomness.

Defaults allow one million aggregate cells and16777216 in-process string units,
including labels. Callers may supply tighter positive whole-number limits. String
units are the target Pascal String representation; this is an allocation bound,
not a portable encoded-byte count. Application codecs must bound their encoded
documents separately. Neither API reads/writes files.

The snapshot contains assignments and ownership. It does not authenticate model
identity, serialize graph rules or topology definitions, certify musical/domain
constraints, or save arbitrary random/source callback state. Applications retain
that configuration, perform their independent domain checks after restoration,
and keep the saved data's provenance. Existing [workspace journals](pipeline-workspaces.md)
continue to save edit history and verify exact execution replay.

The [assignment conformance consumer](../test/wfc_assignments_test.lpr) exercises
caller locks, generated and empty cells, actual restored selective regeneration,
unrelated state retention, malformed values/layouts/domains, budgets, observer
cancellation, reentry and detached data. Native and pas2js run the same fixture.
This contract was motivated by Pythian's durable musical sessions; audio and
musical interpretation remain in that consumer.
