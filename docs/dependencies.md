# dependency policy

WFC owns its portable foundation. Core and maintained ecosystem units use
project source plus the applicable standard FPC or pas2js RTL; they do not
require a package manager, submodule, native DLL, JavaScript framework, hosted
service, or separately fetched third-party runtime. pas2js output and its
matching standard RTL are generated from the Pascal toolchain and staged with
browser demos; they are not hand-maintained alternative implementations.

## hard admission gate

The decision rule is a release gate, not a preference: when including a library
and writing the capability in portable Pascal are both plausible choices, WFC
writes and maintains the FPC/pas2js implementation. If reasonable reviewers can
debate whether the capability belongs in project Pascal, the external
dependency is rejected. Convenience, package popularity, a smaller diff, or a
ready-made serializer is not an exception.

The burden is on a proposed dependency to prove that it is an optional host
edge and cannot define portable behavior. Until that proof is clear, no package
declaration, downloader, submodule, generated binding, runtime initialization,
or test requirement may admit it to the maintained path. Implement the missing
project-owned unit and its native/pas2js conformance test first.

## what the project owns

Canonical behavior must be implemented, versioned, and tested in repository
Pascal. This includes:

- solver and pass-planning algorithms;
- learners, indexes, projection adapters, and domain analyzers;
- deterministic random, hashing, signature, and replay behavior;
- text, model, score, MIDI, mesh, and other interchange codecs;
- semantic validators and contradiction diagnostics; and
- the immutable data structures exposed by public APIs.

Native FPC and pas2js run the same implementation and conformance fixtures.
Platform collation, regular expressions, locale services, browser-only
classification, and opaque host serializers must not define portable results.

## permitted edges

FPC, pas2js, and their standard runtime units are the toolchain and execution
substrate, not ecosystem extensions. The browser DOM and Canvas2D, an operating
system window, an audio device, or a game engine may be used through an
optional presentation adapter when the host capability itself is the point of
the adapter.

In project documentation, “dependency-free” means free of third-party runtime
libraries. Repository units, the applicable standard RTL, the selected
compiler, and host APIs such as the console, Node process, or browser DOM are
still the execution substrate.

Thin native and Node command hosts may call the standard stream/filesystem and
process APIs needed for bounded byte I/O and exit status. Those calls may not
parse, normalize, validate, hash, compile, solve, or serialize a portable
artifact; the shared repository Pascal implementation remains authoritative.

An optional edge must satisfy all of these conditions:

- it is isolated from core/runtime `uses` clauses and public data types;
- removing it leaves learning, generation, validation, replay, and canonical
  artifacts intact;
- it consumes a validated project-owned result instead of becoming the source
  of truth; and
- the ordinary native and pas2js builds do not fetch or initialize it.

Build, test, profiling, documentation, conversion, and inspection tools may be
used during development. Their output is evidence, not a replacement for a
portable implementation or validator.

## legacy and experimental integrations

The Lazarus/LCL, SDL2, SoundShop, and Castle Game Engine experiments are
optional edges retained for history or presentation work. They are not part of
the maintained dependency-free runtime path and cannot become prerequisites
for a standard ecosystem library or demo.

New integrations should be adapters over public, validated WFC artifacts.
They must document their license and installation separately. If an adapter
reveals a missing reusable capability, that capability belongs in a
project-owned FPC/pas2js unit first.

## review checklist

Every proposed dependency should answer four questions:

1. Can the required behavior reasonably be implemented in portable Pascal?
   If the answer is yes, maybe, or uncertain, implement it here; admission
   fails until that implementation exists.
2. Does it influence a canonical algorithm, artifact, validator, or replay
   result? If so, it must be replaced by project-owned Pascal.
3. Can all maintained native and pas2js conformance tests run without it?
4. Is any remaining use an isolated, optional host adapter with compatible and
   recorded licensing?

This policy favors a slightly larger codebase with inspectable contracts over
a smaller facade around dependencies. That trade is intentional: the WFC
ecosystem should remain buildable, reproducible, and understandable from its
own source.
