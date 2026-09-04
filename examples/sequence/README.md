# sequence examples

Sequence examples use the dependency-free `wfc_sequence*` units on native FPC
and pas2js. They accept caller-supplied UTF-8 tokens, preserve typed BOS and
open sample boundaries during learning, solve latent order-N states, and print
only public-token projections.

The foundation begins with [LearnSequence](01_LearnSequence/README.md) and now
supports the [three-pass music composition](../music/03_PassComposition/README.md),
[text constraint completion](../text/02_ConstraintCompletion/README.md), and
[three-pass text workbench](../text/03_PassComposition/README.md). Future event
streams and additional domains can use the same latent/public boundary while
keeping their tokenizers, renderers, and optional playback or viewer adapters
outside the canonical model.
