# sequence examples

Sequence examples use the dependency-free `wfc_sequence*` units on native FPC
and pas2js. They accept caller-supplied UTF-8 tokens, preserve typed BOS and
open sample boundaries during learning, solve latent order-N states, and print
only public-token projections.

The first example is [LearnSequence](01_LearnSequence/README.md). Future music,
text, and event-stream examples can share this foundation while keeping their
tokenizers, renderers, and optional playback or viewer adapters outside the
canonical model.
