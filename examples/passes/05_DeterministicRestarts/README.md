# Deterministic Restarts

This console example runs four checked whole-transaction restart policies
through one shared Pascal helper:

- a wrapped three-cell ring whose zero-backtrack attempt first succeeds after
  three deterministic seed restarts;
- the same ring without its escape value, proving a contradiction on the
  initial attempt and therefore performing no restart;
- a capped-doubling policy whose budgets are exactly `1,2` before the same
  contradiction is proved; and
- a terrain → housing → wrapped-ring pipeline using complete negotiated
  attempts, first succeeding at restart 12.

The helper samples committed cells directly after success. It requires `CCC`
for the one-pass odd ring and `meadow|cottage|C` at every coordinate in the
three-pass result, independently of the solver's success Boolean. Expected
failures are also checked for complete rollback.

## build and run

The normal `build.ps1` or `build.sh` gate compiles the restart, timing, and
shared-demo tests, then runs the demo self-test. Run the built native host (add
`.exe` on Windows):

```bash
build/native/bin/RestartPolicies
build/native/bin/RestartPolicies --selftest
build/native/bin/RestartPolicies --timing
```

For an isolated build:

```bash
mkdir -p build/restarts/native/units build/restarts/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc \
  -Fuexamples/passes/05_DeterministicRestarts \
  -FUbuild/restarts/native/units -FEbuild/restarts/native/bin \
  examples/passes/05_DeterministicRestarts/RestartPolicies.lpr
build/restarts/native/bin/RestartPolicies --selftest
```

No-argument output is deterministic and includes every case's terminal status,
attempt count, effective final seed, transcript, applied budgets, attempt
statuses, and independently captured output. Version-one goldens are:

| Case | Terminal result | Effective terminal seed | Transcript |
| --- | --- | --- | --- |
| escape fixed | solved at attempt 3 | `$85F0B427` | `$36DF8F92` |
| proved contradiction | contradiction at attempt 0 | `$00000000` | `$3521B245` |
| capped doubling | contradiction at attempt 1 | `$514E28B7` | `$F2B4A204` |
| negotiated fixed | solved at attempt 12 | `$7C88AD73` | `$DB3CA510` |

`--timing` requests monotonic diagnostic measurements and keeps the same
search and transcript values. Elapsed numbers are intentionally not golden,
may be unavailable on a host, and are not evidence of relative performance.
Bad arguments report an error and set a nonzero exit status.

The browser conformance scripts compile `test/wfc_restart_demo_test.lpr` with
the same `restart_policies_demo.pas` helper. There is no separate browser UI or
different search implementation.

See [the complete restart contract](../../../docs/restarts.md) for schedule
bounds, terminal statuses, transaction behavior, timing availability, replay
identity, and deliberate limits.
