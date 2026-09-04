# text examples

`01_SimpleTiledWorld` generates a 2D tile world and renders each tile as a
console character. It is called “text” because of that presentation; it is not
the constraint-driven text-prediction/token ecosystem.

`02_ConstraintCompletion` is the first real text vertical slice. It learns raw
documents with a project-owned Unicode-scalar tokenizer, applies exact prefix,
suffix, mask, and interior-span locks, exposes globally feasible domains,
distinguishes prefix continuation from whole-sample generation, and validates
the completed text independently. Separate thin native and pas2js/Node hosts
run the same shared Pascal implementation without an external dependency.

For `01_SimpleTiledWorld`, the same Pascal source is verified on native FPC and
pas2js/Node. Native output uses console colors and waits for Enter only when no
seed argument is supplied; the Node target emits plain text and exits. Both
print the captured seed. Pass an optional unsigned 32-bit seed as the first
command-line argument to replay a chosen world; decimal and Pascal-style
hexadecimal values are accepted.

For `02_ConstraintCompletion`, pass an optional decimal unsigned 32-bit seed.
Both thin hosts default to seed zero, print the captured seed, and exit.

`01_SimpleTiledWorld` defines a complete adjacency model and does not use an
invalid-state callback to invent output when constraints conflict. The native
one-command build runs seed `0` as a noninteractive smoke test.

See the [examples index](../README.md) for build commands and the
[determinism contract](../../docs/determinism.md) for replay requirements. The
[text foundation](../../docs/text.md) defines tokenizer, extent, domain,
completion, and validation semantics.
