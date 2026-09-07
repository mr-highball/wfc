# Portable public quotas and latent representations v1

## Question

Can a saved public-token quantity constraint guide private learned-state
search without exposing representation keys or changing its meaning?

The counterexample to a naive implementation is a one-way pipeline whose
private sequence has already chosen too many rests. Adding a quota only to
the downstream public projection cannot change that completed provider. It
may reject a composition for which a different private assignment was legal.

## Construction and invariant

Let each private cell choose a state `s`, and let `emit(s)` be its public token.
For public accepted set `A`, lower the quota to the inverse set
`{s | emit(s) in A}`. Because both layers have one value per physical cell,
the membership indicators agree at every position; their sums are equal.
Several private states with one emission do not change that equality.

For the existing wrapped 2D projection, `emit(s)` is the pattern's `(0,0)`
palette token. Overlap validation proves agreement with the other footprint
contributions. Counting every footprint offset would overcount public cells.
No such lowering is claimed for unwrapped, cropped, many-to-one, or resampled
projections; those are not bridge kinds in this recipe contract.

Definitionless transforms copy a public source cell for cell, so their quota
can be moved through the exact-copy chain without adding a local definition.
Multiple descriptors lower conjunctively, with stable distinct internal labels.

The compiler retains the public materialized constraint as well as the private
inverse constraint. It independently recounts the declared public owner from
the immutable recipe inside tentative commit. Solved artifact construction
recounts again, without trusting graph registrations or a supplied signature.

## Versions and reproduction

The opt-in recipe contract is `WFC_PIPELINE_VALUE_QUOTA_VERSION=1`, using core
quota version 1 and `wfcpipeline=2`. Empty registries preserve exact old model
signatures and `wfcpipeline=1` bytes. The public run/result envelopes are still
version 1. Existing bridge version 1/2 semantics for per-cell run inputs stay
unchanged; quota lowering is a separate explicit feature.

Run the three `wfc_pipeline_value_quota_*test.lpr` programs via the maintained
native and browser builds. They cover model ownership and strict hostile text,
direct/alias/sequence/pattern counts, one-way and negotiated execution,
transaction rollback, replay, forged solved layers, and canonical CLI output.
The original pipeline conformance programs retain the quota-free goldens.
See the [build guide](../building.md) and [format contract](../pipeline-value-quotas.md).

The initial September 2026 verification used stable FPC 3.2.2 and development
FPC 3.3.1, each on Win32 and Win64, plus actual Edge execution of pas2js output
through the included FPC server/capture/checker. Model checks passed 51 native
and 95 browser assertions; strict text checks passed 96 on every target;
integration passed 260 on every target. The extra browser model assertions
exercise malformed JavaScript numeric values. Existing pipeline golden suites
also passed; these focused results do not substitute for the hosted OS matrix.

This is a semantic-preservation argument and finite conformance record, not a
claim of generalized arc consistency or an efficiency benchmark. Quotas can
interact with adjacency in ways requiring backtracking. Search exhaustion is
not proof of unsatisfiability, and a valid quantity constraint says nothing
about aesthetic quality.
