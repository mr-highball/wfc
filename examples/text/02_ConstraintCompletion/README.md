# constraint completion

This example learns an order-3 Unicode-scalar model from two project-authored
MIT-licensed sentences, then performs fixed-length anchored infill with a
prefix, an interior locked span, a suffix, and a two-token positional domain.
It also proves the difference between a valid truncated prefix and an invalid
truncated whole sample.

The same shared Pascal unit runs under native FPC. It uses only
repository units and the applicable standard RTL.

## native FPC

From the repository root:

```powershell
New-Item -ItemType Directory -Force build/examples/text-completion/native/units, build/examples/text-completion/native/bin
```

```bash
mkdir -p build/examples/text-completion/native/units build/examples/text-completion/native/bin
```

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/text/02_ConstraintCompletion -FUbuild/examples/text-completion/native/units -FEbuild/examples/text-completion/native/bin examples/text/02_ConstraintCompletion/ConstraintCompletion.lpr
build/examples/text-completion/native/bin/ConstraintCompletion 0
```

On Windows the executable has an `.exe` suffix.

## seed-zero output

```text
WFC Text Constraint Completion
Tokenizer: unicode-scalar
Order: 3
Samples: 2
Public scalars: 17
Latent states: 28
Seed: 0
Position 7 domain: e | c
Anchored infill: the quick fox rests.
Prefix continuation: the quick
Truncated whole sample: rejected
Independent validation: verified
Replay: verified
```

The domain is printed in first-seen public-vocabulary order. Seed zero chooses
the `c` branch; the latent overlap then forces `quick fox`. The independent
validator rechecks boundaries, latent transitions, public projection, all
locks, and exact text round-trip. Passing another unsigned 32-bit seed explores
the same finite constraint space and prints the captured replay seed.
