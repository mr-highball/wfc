# text-rendered examples

`01_SimpleTiledWorld` generates a 2D tile world and renders each tile as a
console character. It is called “text” because of that presentation; it is not
the future constraint-driven text-prediction/token ecosystem.

The same Pascal source is verified on native FPC and pas2js/Node. Native output
uses console colors and waits for Enter only when no seed argument is supplied;
the Node target emits plain text and exits. Both print the captured seed. Pass
an optional unsigned 32-bit seed as the first command-line argument to replay a
chosen world; decimal and Pascal-style hexadecimal values are accepted.

The example defines a complete adjacency model and does not use an
invalid-state callback to invent output when constraints conflict. The native
one-command build runs seed `0` as a noninteractive smoke test.

See the [examples index](../README.md) for build commands and the
[determinism contract](../../docs/determinism.md) for replay requirements.
