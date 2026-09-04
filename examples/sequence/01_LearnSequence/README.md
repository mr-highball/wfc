# LearnSequence

This portable console example learns an order-2 model from the bounded corpus
`A B A` and `A C A`. The two samples reset their BOS history and add no seam,
while structural suffix/prefix compatibility still permits valid order-2
recombination.

It round-trips the strict canonical `wfcs=1` artifact and runs two demonstrations.
The first applies the latent model to an open length-three graph and solves
with an explicit seed. The second builds a three-pass public tokens -> latent
sequence -> public classes pipeline, fixing the middle token to `C` and using
projection requirements in both directions. Both paths are independently
validated and print only public projections; private graph state keys are
never printed or stored in the artifact.

Create the named `units` and `bin` directories first. From the repository
root, compile it for native FPC:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/sequence/native/units -FEbuild/examples/sequence/native/bin examples/sequence/01_LearnSequence/LearnSequence.lpr
build/examples/sequence/native/bin/LearnSequence 0
```

With a configured pas2js RTL installation, the same source targets Node.js:

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -FUbuild/examples/sequence/pas2js/units -FEbuild/examples/sequence/pas2js/bin examples/sequence/01_LearnSequence/LearnSequence.lpr
node build/examples/sequence/pas2js/bin/LearnSequence.js 0
```

The optional argument is an unsigned 32-bit replay seed. Seed `0` selects the
`A B A` branch in the conformance fixture; seed `3735928559` selects `A C A`.
The seed is only one part of replay identity; the canonical model, graph shape,
boundary, domains, solve options, and version constants also matter.

This is a hard constraint generator with raw learned weights. It does not
tokenize raw text, smooth unseen N-grams, learn wrapped evidence, or claim to
be a probabilistic language model.
