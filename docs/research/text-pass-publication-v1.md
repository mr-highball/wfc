# Text-pass publication belongs to the transaction

This record documents a concrete failure boundary in the three-pass text
owner, its correction, and finite regression evidence. The implementation and
fixtures are project-owned Pascal under the repository's MIT license.

## Counterexample

The seed-zero text showcase first produces its normal composition, with
signature `1:69ABA6CE`. An advanced caller then increments one registered
structure graph rule's weight, clears a generated cell, and advances that
pass's random stream. The learned model remains unchanged, so its identity
check must reject the modified graph.

Previously, the generic graph finished its transaction before the text owner
captured the three public sequences. The owner returned `False` with
`wtpsCaptureFailed` and `wsgikModelIdentity`, but its nested solve report still
said `gssSolved`. Entries and random state matched an independently committed
control, not the pre-call state. An empty public result did not make the
operation atomic.

This was reproduced on FPC 3.2.2 and 3.3.1 with tracing both disabled and
enabled. It is a failure of transaction placement, not of the local WFC search.

## Commit-boundary correction

The text owner now uses a private graph subclass with the existing protected
`DoValidateCommit` hook. After the candidate entries are staged, while the
graph still owns its rollback snapshots, the hook:

1. captures all three learned state/public-token paths;
2. renders the exact punctuation fragments;
3. checks the learned paths, caller constraints, and copied cross-layer maps;
4. retains the complete result privately only if every check succeeds.

An active-layer rejection returns its original public capture or validation
diagnosis. The generic report now records `gckFinalValidation` and rollback.
Only a successful core transaction permits transfer of the pending public
result. Failure and exceptions clear pending ownership without clearing the
caller's dirty constraint scope.

The private subclass propagates its owner through `CreatePass`, so invoking
the exposed graph through a child pass cannot bypass final validation.
The core graph's algorithm, event encoding, and random sequence are unchanged;
the owner no longer performs semantic acceptance after that boundary.

## Evidence and limits

[`wfc_text_pass_transaction_test`](../../test/wfc_text_pass_transaction_test.lpr)
uses real graph edits, not an injected success/failure flag. It covers all three
active layers, full and selective attempts, trace on/off parity, repaired
retries, preserved caller domains and locks, entry ownership, and each
publicly observable pass random stream. Separate fixtures broaden graph
projection rules: all three local sequence captures succeed, but the owner's
original cross-layer maps reject the candidate and the whole attempt rolls
back. Another fixture adds a graph value without a public model token and
checks that failed trace projection cannot expose private values or replace
the primary domain diagnosis.

The current and stable native runs and the pas2js browser run each pass 5,319
checks. The existing success
signature `1:69ABA6CE` and trace hash `2412171679` remain unchanged. These are
finite regression results, not proof against every possible mutation or host
failure.

An invalid reused provider outside a selected closure raises the core's scope
error and rolls back; it is not relabeled as an active consumer failure. This
specialized owner has three models, so an added fourth pass is rejected inside
the rollback boundary. Arbitrary pass DAGs remain a core-framework facility.

The transaction restores entries and random state, not caller-authored rule,
domain, dependency, or topology edits. Optional trace formatting is diagnostic
publication after the transaction; allocation failures and internal invariant
exceptions there are not a promise of another rollback. The documented atomic
boundary covers candidate capture, rendering, and semantic acceptance.

See the [text API contract](../text.md) and the
[chronological trace layout](chronological-trace-layout-v1.md) that represents
late rejection of an earlier pass without rewriting event history.
