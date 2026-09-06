# Training Connectivity v1: evidence and policy have different owners

This is a design and verification record for WFC's project-owned Pascal
implementation. It does not claim a new reachability algorithm, general
inference of author intent, or a measured advantage over another framework.

## Question

How can learned local relationships and explicit global requirements survive
editing, serialization, and retraining without confusing observations with
hard policy?

A sample containing connected roads does not uniquely identify a requirement
that every generated road be connected. The sample also does not establish
which token directions are connection ports. We therefore keep two explicit
inputs: observed samples and authored connectivity profiles. Learning builds
local relationships; policy constrains generated public output.

## Token identity survives vocabulary changes

Suppose samples initially encounter `grass` before `road`, but an edited corpus
encounters them in the opposite order. A profile attached to vocabulary index
one changes meaning under that edit. A profile attached to the exact token
`road` does not.

The source owns ordered token/profile pairs. Lowering performs exact token
lookup in the newly learned public vocabulary and emits descriptors in that
vocabulary's canonical order. Missing tokens are errors. It never substitutes
an old numeric index or silently drops a profile. This gives a checkable
invariant: each emitted public token retains its authored participation, ports,
and required-by-value flag regardless of vocabulary permutation.

For projected sequence/pattern outputs, the portable compiler maps those public
profiles to private states before solving and independently checks public
results. This authoring layer relies on that existing mapping; it does not
invent another private-state interpretation.

## Spatial intent is not sample geometry

Roots and terminals name absolute output coordinates. A corpus may be smaller
than the desired generated map. Source validation checks coordinate rank and
integer representation, while invocation validation checks actual shape.
Implicit wrapping or clamping would silently change the requested destination.

Likewise, rotating sample coordinates does not establish a semantic rotation
between token names. Authored ports remain world-axis directions. A future
orientation-aware vocabulary layer must explicitly represent that additional
relationship; it must not be inferred from this source format's coordinate
augmentation alone.

## Composition and invalidation

Quotas and networks are conjunctive policies on one learned public output.
Editing either preserves the other. A failed edit exposes no derived artifact
from the preceding policy. Valid canonical drafts are published before learning
so an expensive/invalid learning result does not erase the author's requested
edit. Model-only export is refused when its format cannot represent policy.

These lifecycle requirements are as important as the solver constraint: a
perfect connectivity propagator cannot protect a user if saving or editing
silently strips its descriptor.

## Evidence and limits

The maintained suites exercise source identity and canonical versions, detached
records, observed/learned-token lookup, fixed-coordinate validation, both policy
edit directions, failed-learning invalidation, and native/browser execution.
The route demonstration deliberately admits all local binary tile combinations,
then uses an explicit six-road quota and root-to-terminal network to express
the output requirement. It separates what the corpus observed from what the
author demanded; it is not a claim about realistic terrain statistics.

The source format is bounded and strict. Studio interaction remains synchronous
within its documented envelope. General multi-pass authoring, learned port
semantics, streaming connectivity summaries, and stronger solver propagation
are not established by this feature. Implementation and test commands are in
[the user guide](../training-connectivity.md).
