# Hosted native verification, 2026-09-06

This is a dated evidence record for commit
[`3be45f09e348bd474615b40e3de62529c060ff65`](https://github.com/mr-highball/wfc/commit/3be45f09e348bd474615b40e3de62529c060ff65),
not a claim that every current or future compiler passes. All four native
jobs completed successfully in
[run 34000333869](https://github.com/mr-highball/wfc/actions/runs/34000333869).
That run was **not fully green**: its separate browser job failed a
server-log startup race after compilation. Browser completion must be
established separately.

## Observed native matrix

| Job | Compiler reported by the execution log | Result |
| --- | --- | --- |
| [Linux stable](https://github.com/mr-highball/wfc/actions/runs/34000333869/job/101397931910) | FPC 3.2.2+dfsg-32, x86_64, build 2024/01/05 | Native gate and package validation passed |
| [Windows stable](https://github.com/mr-highball/wfc/actions/runs/34000333869/job/101397931938) | FPC 3.2.2, i386, build 2021/05/15 | Native gate passed |
| [macOS stable](https://github.com/mr-highball/wfc/actions/runs/34000333869/job/101397931892) | FPC 3.2.2, x86_64, build 2023/09/18 | Native gate passed |
| [Linux development](https://github.com/mr-highball/wfc/actions/runs/34000333869/job/101397931800) | FPC 3.3.1-20783-gdacce784a3, x86_64, build 2026/09/05 | Native gate passed |

The development container recorded digest
`freepascal/fpc@sha256:cbbee02745c00aa0cceb8598642c075b4e04e42f7f17932572a3b02cdb65a9c0`.
This is a Linux development-compiler result, not a development-compiler
matrix on all three platforms. Likewise, the Windows result is i386, not
evidence for every Windows architecture.

Each native job compiled and executed the 102 native test programs selected
by [build.sh](../../build.sh) or [build.ps1](../../build.ps1). The two
browser-only test hosts are outside that native count. Log compilation
records were compared with the source manifest; grouped invocations and
their successful summaries were checked alongside the wrappers' nonzero-exit
propagation. Merely counting source files was not treated as execution proof.

Shared observations include:

- The core suite invokes all 74 native test procedures: 741 checks, zero
  failures. It is noninteractive and reports a failing process exit on failure.
- Pipeline CLI process conformance: 18 cases passed.
- Training CLI process conformance: 30 cases passed.
- Server unit checks: 182/182; real socket/server integration: 240/240.
- The gates reach the final Learned Terraces SVG export, with scene
  `1:6D695B99:2D23CF62`, view `C3D25917`, and a passing self-check.

Platform-specific checks remain explicit: the music-study process harness
passes 123 checks on Unix versus 122 on Windows because of its Unix SIGKILL
fixture. Ensemble rendering passes 121 checks on Unix versus 117 on Windows
because of cooperative SIGINT cases. These differences do not remove a
native test program from either platform.

## Packages, replay, and failure propagation

The Linux lane additionally builds all 77 runtime units through FPM and
the Lazarus package. It builds and runs the core Lazarus project (741/0),
builds and runs both original music-study projects, then checks that generated
output leaves the checkout clean. These are Linux package results; the
other three jobs do not run that package step. Direct unit-path builds
remain independent of Lazarus.

The [core fixtures](../../test/wfc_test.lpr) exercise directions, wrapping,
inverse and required rules, locked entries, invalid states, reshape/reset,
run modes, host-random isolation, and repeated seeded runs. The
[world fixture](../../test/wfc_world2d_test.lpr) independently validates its
layered result, checks the exact signature, and repeats the same-seed run.
These are finite conformance results, not proof for every model or callback.

Failure propagation was also observed, not only inspected: at earlier
commit `d70b758`, a process-test assertion comparing Unix wait status with a
semantic exit code failed the hosted Linux, macOS, and development jobs.
The corrected harness checks normal exit versus signal termination before
reading the exit code. A later macOS failure exposed a string comparison
against BSD wc's padded count; the numeric check passes in the matrix above.
Neither correction changes solver behavior or relaxes diagnostic-format
assertions.

## What this does not establish

This record does not establish a successful hosted browser suite, release
readiness, general solver completeness or optimality, subjective output
quality, or completion of the [roadmap](../../ROADMAP.md). Compiler warnings
remain visible in the logs; a successful build is not a warning-free claim.
Use the [build guide](../building.md) and current workflow results to verify
later revisions. Research claims retain their separate
[fixture and evidence boundaries](../research/README.md).
