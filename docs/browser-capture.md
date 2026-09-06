# Native browser completion capture

`wfc_browser_capture` is a small project-owned FPC client for Chromium's
DevTools protocol. It waits for actual completion of compiled Pascal browser
tests, including awaited Blob and writable-file transactions, before saving
the browser's serialized DOM. The maintained PowerShell and Bash runners own
browser startup and cleanup; `wfc_browser_check` independently checks the
saved evidence. No external browser-automation library or runtime is needed.

Build all three native tools with `build.ps1` or `build.sh`. To build only the
capture executable with an installed FPC and FCL:

```bash
mkdir -p build/native/bin build/native/tools-units
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Futools \
  -FUbuild/native/tools-units -FEbuild/native/bin tools/wfc_browser_capture.lpr
```

For normal use, prefer the [maintained runner](development-tools.md#run-pascal-conformance-in-a-real-browser),
which handles fresh profiles, server readiness, process ownership, diagnostics,
and the independent checker. The low-level interface is:

```text
wfc_browser_capture --prepare-profile NEW-DIRECTORY
wfc_browser_capture --deadline-after MILLISECONDS
wfc_browser_capture --profile DIRECTORY --url LOOPBACK-URL --dom NEW-FILE
  [--deadline MONOTONIC-TICKS | --timeout-ms MILLISECONDS]
  [--expect NAME=VALUE ...]
```

`--deadline-after` prints an absolute monotonic tick value. Create it before
launching the browser and pass that same value to capture; do not substitute
wall-clock time or a different runtime's tick count. The direct CLI defaults
to 60 seconds if no deadline is given. The maintained full gate always uses
60 seconds, with separate finite cleanup allowances. These are test-operation
budgets, not limits on generated music length or other WFC outputs.

## Ownership and completion

1. Prepare a nonexistent profile directory below an existing parent. Existing
   directories are refused. Launch a new headless browser with that profile,
   `--remote-debugging-address=127.0.0.1`, `--remote-debugging-port=0`, and
   `about:blank`. Do not use an interactive or authenticated profile.
2. Capture reads only that profile's bounded `DevToolsActivePort` file. It
   connects to literal IPv4 loopback and the named browser WebSocket endpoint,
   creates its own page target, attaches, and navigates to the requested URL.
3. Capture polls the browser's serialized document. It requires every expected
   body attribute to match in the same snapshot, plus `data-self-test=passed`.
   The exact document URL and backend root-node identity are rechecked around
   accepted snapshots; stale node lookups retry within the original deadline.
4. A successful complete DOM is atomically published to a new destination.
   The runner checks it again, then terminates the processes it launched.

Once capture consumes a prepared profile, it is single-use, including after
failure. Preflight rejection, such as an already expired deadline or an output
collision, does not consume it. A marker records
workflow ownership; it is not a security boundary against hostile filesystem
changes or another local process modifying that directory. Output collisions
are rejected before consuming the profile. The client never enumerates or
attaches to the user's existing browser session.

Missing and pending markers are not success. A nonempty self-test message,
`data-self-test=failed`, or a requested `passed` marker becoming `failed`
terminates capture with failure. Other application state is not confused with
test state: a deliberately failed operation can be part of a successful
negative test. Other mismatches remain pending until they match or the shared
deadline expires. Diagnostics include bounded, escaped last-observed state.

Ordinary capture failure leaves no published DOM. Writes check the deadline
between bounded blocks and around publication. A filesystem flush/rename can
block inside the OS; if complete publication finishes after the deadline, the
tool preserves that complete file but **returns failure** and says publication
was late. It also reports secondary partial-file cleanup failures. Existence
of a DOM file alone never certifies success.

## Narrow protocol boundary

The implementation consists of native Pascal units:

- `wfc_browser_socket`: nonblocking IPv4 loopback I/O, shared monotonic
  deadlines, socket-local SIGPIPE protection, and OS-generated random bytes.
- `wfc_browser_websocket`: validated RFC 6455 upgrade, fresh client masks,
  bounded frames, fragmented UTF-8 text, ping/pong, and close validation.
- `wfc_browser_cdp`: strict JSON objects, sequential request IDs, flattened
  session correlation, bounded nesting, and separate protocol/command errors.
- `wfc_browser_capture_app`: prepared-profile ownership, navigation, document
  identity, and exact completion assertions using the shared DOM parser.

The client accepts only `http://127.0.0.1:PORT/path` navigation and a fresh
local browser endpoint. It has no TLS, proxy, authentication, compression,
remote target discovery, or general browser automation API. It supports a
16 MiB serialized DOM, with bounded additional space for JSON escaping. It is
not an HTML sanitizer, a browser sandbox, or an OS process-containment tool.

Windows uses WinSock2 and CNG. Linux uses libc sockets, monotonic time, and
`getrandom` (glibc 2.25 or a compatible implementation); macOS uses socket-local
SIGPIPE handling, monotonic time, and `getentropy` (macOS 10.12 or newer).
There is no fallback to predictable randomness or wall-clock deadlines.
Captured Unicode remains UTF-8 throughout transport and publication.
Windows expectation values are decoded from the OS wide argument vector,
including supplementary Unicode characters; path arguments retain the native
ANSI path convention of the existing file tools. Use paths representable in
the active Windows code page; arbitrary Unicode filesystem paths are not yet
part of this host contract.

## Tests and evidence

The native gate includes socket, WebSocket, CDP, and capture-state tests.
They exercise partial I/O, expired deadlines, malformed upgrades and frames,
control messages, strict Unicode and JSON correlation, pending/release/error
markers, profile reuse, and endpoint/URL rejection. The separately compiled
`test/browser_capture/fixture.lpr` supplies a real delayed Blob operation,
negative application state, explicit failure, missing release, and permanently
pending modes. Its HTML contains only the Pascal bootstrap.

Browser staging also builds this fixture below `capture-fixture`. On a Unix
host with the native tools and Chromium available, run:

```bash
WFC_BROWSER=/usr/bin/chromium bash ./test/browser_capture/run.sh
```

This uses the same maintained runner and 60-second deadlines. The two timeout
cases deliberately consume their full budgets. A failure case must report the
fixture's actual rendered state, not just a generic nonzero exit or a browser
startup timeout, and it must not leave a passing convenience DOM copy. All
attempts retain separate evidence directories. The hosted browser lane runs
this fixture gate as well as the normal suite.

The full browser gate still executes every current portable test, the Voice
and Ensemble release contracts, and all ten actual demo entry pages. A
targeted local success is not proof of the full gate or another OS. Hosted
results should be cited at their actual commit, not inferred from compilation.

Protocol references: [RFC 6455](https://www.rfc-editor.org/rfc/rfc6455.html),
[CDP Target](https://chromedevtools.github.io/devtools-protocol/tot/Target/),
[CDP DOM](https://chromedevtools.github.io/devtools-protocol/1-3/DOM/), and
[dedicated remote-debugging profiles](https://developer.chrome.com/blog/remote-debugging-port).
Protocol behavior is implemented in project-owned Pascal under the repository's
MIT license; no browser implementation code is vendored.
