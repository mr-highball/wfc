# FPC development tools

The maintained command-line toolchain is native FPC. Browser applications are
written in Pascal, compiled with pas2js, and shipped with the matching compiler
RTL. The repository owns its serving and verification tools as well; no
additional interpreter, package manager, or server runtime is needed.

Run the checked native gate from the repository root:

```powershell
.\build.ps1 -Compiler 'C:/path/to/fpc.exe'
```

```bash
FPC=/opt/fpc/bin/fpc bash ./build.sh
```

Omit the override when `fpc` is on `PATH`. Executables are staged in
`build/native/bin`; append `.exe` on Windows. Generated files stay under
ignored `build` directories. This gate compiles and runs the pure conformance
tests, tool help/version smoke checks, real command-line process tests, live
server tests, and the streamed Music Studio exporter tests. It does not
substitute for the browser execution gate below.

## Check package completeness

The native gate runs this read-only FPC tool before the larger test suites:

```text
build/native/bin/wfc_package_check --root .
```

It compares every maintained source unit with `fpmake.pp`, `wfc.lpk`, and
`wfc_package.pas`, checking missing/duplicate entries, source declarations,
and canonical filenames. Add `.exe` on Windows. See
[package checking](package-checking.md) for the static-checking boundary,
standalone build, limits, and installed-unit consumer tests.

## Save and restore an editable workspace

The same native gate builds `build/native/bin/wfc_workspace[.exe]` from
`tools/wfc_workspace_cli.lpr`; its smoke command is `--help`, not `--version`.
It exposes `inspect`, `replay`, `begin`, `edit`, `initial`, `preview` and
`repair` over complete explicit files. No browser or server is needed.

Use the [workspace guide](pipeline-workspaces.md#native-cli) for a runnable
fixture walkthrough, adjustable logical policies, ownership, exact exit codes
and the difference between graph-free unverified inspection and actual replay.
Initial/repair exit10 means a normal unsolved attempt was successfully recorded
in a new journal, not that an arbitrary tool failure should be ignored.

`wfc_workspace_cli_process_test` invokes the actual native executable and
`wfc_workspace_cli_fixture` with a fresh output directory. It checks all seven
commands, complete independently constructed histories, refusal/failed-outcome
statuses, explicit scopes, limits and new-file preservation. The process suite
stays native; the four context/evidence/journal/replay conformance groups run
under both FPC and pas2js through the existing browser tooling. This does not
migrate any demo's UI to a workspace editor.

To run the process suite alone after building, supply exactly these three
positional arguments (append `.exe` to the executable names on Windows):

```text
build/native/bin/wfc_workspace_cli_process_test build/native/bin/wfc_workspace build/native/bin/wfc_workspace_cli_fixture build/workspace-cli-check
```

`build/workspace-cli-check` must not exist; its parent must exist. Evidence is
retained there in `fixture with spaces` and `logs`. The maintained fixture
currently exercises 38 real child-process cases with 334 harness checks,
including two independently compared histories. The suite handles the expected
exit10 cases individually and still fails on an unexpected child status.

## Serve a browser demo

First stage the selected browser program with its dedicated build script.
For example:

```powershell
.\build-browser-music.ps1
.\build\native\bin\wfc_serve.exe --root build/browser/music/www --port 8767
```

```bash
bash ./build-browser-music.sh
./build/native/bin/wfc_serve --root build/browser/music/www --port 8767
```

Open `http://127.0.0.1:8767/` in a browser. Stop the foreground server with
Ctrl+C when finished. Start another instance on a different port when serving
multiple demos. This is a local development server, not a public deployment,
authentication gateway, upload service, or general application backend.

| Demo | Staging script stem | Document root |
| --- | --- | --- |
| World 2D | `build-browser` | `build/browser/world2d/www` |
| Text passes | `build-browser-text` | `build/browser/text-passes/www` |
| Building 3D | `build-browser-building3d` | `build/browser/building3d/www` |
| Training Studio | `build-browser-training` | `build/browser/training/www` |
| Music Studio | `build-browser-music` | `build/browser/music/www` |
| Ensemble Studio | `build-browser-ensemble` | `build/browser/ensemble/www` |
| Voice Studio | `build-browser-voices` | `build/browser/voices/www` |
| Neighborhood Counts | `build-browser-counts` | `build/browser/counts/www` |
| Learned Terraces | `build-browser-terraces` | `build/browser/terraces/www` |
| Connected Routes | `build-browser-connectivity` | `build/browser/connectivity/www` |

Use the `.ps1` entry on Windows or the `.sh` entry in a POSIX shell. Pass
`-Compiler 'C:/path/to/pas2js.exe'` to PowerShell, or set `PAS2JS` for the shell
entry, when pas2js is not configured on `PATH`. Serving a
staged directory does not train, modify source artifacts, or publish files to
an external host. Keep unrelated or private files outside its document root.

### Server contract

For user-defined-length Ensemble WAVE downloads on a phone without the direct
browser file picker, use the dedicated
[`EnsembleStudioServe` host](ensemble-http-downloads.md). It reuses these
static-file safeguards and adds a narrow, in-process music-rendering endpoint.
The `wfc_serve` executable described below remains static-only.

```text
wfc_serve --root DIRECTORY [--port 8000] [--bind ADDRESS] [--max-requests N]
wfc_serve --version
wfc_serve --help
```

The root must be an existing ordinary directory. The listener defaults to
`127.0.0.1`. Version 2 adds an explicit `--bind` address for trusted LAN testing;
there is still no all-interfaces switch. The optional positive
`--max-requests` stops after that many accepted connections, including invalid
requests. Without it, the foreground process keeps serving until stopped.

On Unix, the listener sets `SO_REUSEADDR` before binding so a stopped server
can restart on the same port while old HTTP connections finish their TCP
wait state. It does not enable `SO_REUSEPORT`: a second active listener on
the same address and port must still be refused. Windows binding
behavior is unchanged. Live tests check both refusal of a competing listener
and immediate same-port restart after real HTTP traffic.

Only GET and HEAD are accepted. Directories resolve to `index.html`, with a
query-preserving redirect when a trailing slash is missing. There is no
directory listing. HTML, CSS, JavaScript, JSON, SVG, MIDI, WAVE, icons, text,
Wasm, and common image types have explicit content types; other files use
`application/octet-stream`. File bodies are binary-safe 64 KiB chunks; HEAD
returns the corresponding headers without a body.

Request headers are limited to 16 KiB and targets to 2,048 bytes. Header reads
have a two-second deadline and responses a 15-second send deadline. The server
is sequential: a slow connection may delay the next one within those bounds.
HTTP/1.1 requires a Host matching the selected bind address. `localhost` is also
accepted only when bound to the default `127.0.0.1`. An optional valid port is
accepted as before; arbitrary hostnames and other LAN addresses are not.
Request bodies and transfer encoding are rejected.

The deliberately narrow URL policy decodes once and rejects malformed escapes,
traversal, backslashes, control bytes, non-ASCII paths, hidden/dot components,
and platform device aliases. Keep staged filenames simple ASCII. Root and
request path components cannot be symbolic links or Windows reparse points;
the final opened handle must also resolve inside the validated root. Linux
requires `/proc/self/fd` for that handle check. Darwin uses `F_GETPATH`, and
Windows uses the final handle path. Failure to establish containment fails
closed. This is a static development utility, not a complete HTTP server.

The implementation and live tests target Windows, Linux, and macOS. Local
checked FPC 3.2.2/3.3.1 execution verified Windows; the Darwin bindings were
checked against Apple and compiler sources, but local macOS execution was not
available. The hosted native lanes are the cross-platform runtime gate.

### Access from a trusted local network

Select this computer's Wi-Fi or Ethernet IPv4 address explicitly. For example,
if it is `192.168.1.25`:

```powershell
.\build\native\bin\wfc_serve.exe --root build/browser/ensemble/www --bind 192.168.1.25 --port 4178
```

Open `http://192.168.1.25:4178/` on a device connected to the same LAN. Use the
actual address assigned to your computer, not the example address. This
instance listens only on that address; a separate default-bound instance can
serve localhost, including on the same port.

The bind address must be canonical dotted-decimal IPv4 in `127.0.0.0/8`,
`10.0.0.0/8`, `172.16.0.0/12`, or `192.168.0.0/16`. Hostnames, leading-zero
octets, wildcard `0.0.0.0`, public addresses, IPv6, and other ranges are rejected.
The operating system must have the selected address assigned. DHCP changes
may require restarting with the new address.

The server has no login or TLS: everyone who can reach this listener can read
all ordinary files under its root. Serve only the staged demo directory, use
a trusted network, and do not add router port forwarding. Binding a private
address alone is not a remote-client access rule. The server does not change
firewall settings or network profiles. If the operating-system firewall blocks
access, explicitly allow only this executable and TCP port, on the intended
private interface, from the local subnet. Changing Windows firewall rules
requires an administrator. Guest-network/client isolation may also prevent
devices from reaching one another; a successful request from the server PC
does not prove another device can connect.

The ordinary preview and download controls remain available subject to the
client browser's capabilities. Direct streamed **Save As** depends on the
browser's file-picker API, which may be unavailable over LAN HTTP. The music
demos detect that condition and offer their native FPC rendering commands;
the localhost instance remains available for browsers supporting that API.

## Check browser evidence

Every maintained browser demo exposes an event-driven `?selftest=1` route.
The browser must execute the compiled Pascal before the resulting DOM is
checked; merely fetching the original HTML proves nothing about generation.

`wfc_browser_capture` waits for the requested terminal markers in an owned
headless browser and publishes its rendered DOM. `wfc_browser_check` then
independently verifies that file against the same exact body-state contract.
Both are project-owned native Pascal tools, served by `wfc_serve`. The browser
remains the JavaScript execution engine; the FPC tools do not emulate it.
See [native browser capture](browser-capture.md) for the protocol, ownership,
deadline, and failure contracts.

```text
wfc_browser_check --dom FILE --expect NAME=VALUE [--expect ...]
wfc_browser_check --harness SCRIPT.js --dom NEW-HTML-FILE
wfc_browser_check --version
wfc_browser_check --help
```

For example, after a browser has saved its rendered DOM:

```bash
build/native/bin/wfc_browser_check --dom build/browser/music/result.dom \
  --expect data-self-test=passed --expect data-state=solved \
  --expect data-audio-play-events=0 --expect data-arrangement-test=passed
```

The checker reads at most 16 MiB and compares exact body-attribute values.
Expectation names are lowercase ASCII and must be unique. Missing attributes,
duplicate body attributes, mismatches, malformed supported markup, and a
nonempty `data-self-test-message` fail with exit status 1. Success exits 0.
Quoted markup, raw-text elements, comments, and inert template contents
cannot supply a forged body marker. This bounded parser is an evidence reader,
not an HTML sanitizer or a general browser parser.

An attribute mismatch or nonempty self-test message includes a body `data-*`
state snapshot in the failure diagnostic. The snapshot is limited to 4 KiB,
prioritizes the main self-test state/message, escapes non-ASCII bytes and
control characters, and marks truncation. Individual names and values are
also shortened for display; the underlying assertion still compares complete
values exactly. This preserves asynchronous phase markers in hosted logs
without interpreting a pending operation as success.

Harness mode creates a new HTML file beside one local compiled `.js` filename;
it refuses an existing destination. It calls the embedded Pascal RTL and marks
success only after a synchronous test returns successfully. It is not a promise
or asynchronous-test runner. Asynchronous UI tests must publish their own
terminal state, as the demo self-tests do.

A self-test must reach its terminal success state and match its expected
public signatures, counts, and invalidation/recovery markers. A timeout,
missing marker, mismatch, or explicit failure must fail the gate. Music Studio
also distinguishes audio readiness from user playback: automatic play events
must remain zero. DOM evidence does not prove a download was saved or that a
speaker emitted sound.

## Run Pascal conformance in a real browser

Build the native capture tool, checker, and server first, then stage the portable test programs
with pas2js and execute them in a Chromium-family browser:

On Windows, run the browser conformance runner in **PowerShell 7 or newer**
(`pwsh`), not Windows PowerShell 5.1. Its timeout cleanup uses the modern .NET
process-tree API to terminate only the capture/browser/server processes it owns. The
runner checks this shell version before starting any process.

```powershell
.\build-browser-tests.ps1 -Compiler 'C:/path/to/pas2js.exe'
.\test-browser-tests.ps1 -Browser 'C:/path/to/chrome.exe'
```

```bash
PAS2JS=/opt/pas2js/bin/pas2js bash ./build-browser-tests.sh
WFC_BROWSER=/usr/bin/chromium bash ./test-browser-tests.sh
```

The Bash runner's cleanup targets its recorded child PIDs, not whole process
trees. It polls for two seconds after TERM and another two after KILL before
reporting a still-live child; it only reaps after that child's liveness is gone.
The watchdog gets six seconds after TERM so its own timer cleanup can normally
finish first. A failed or forcibly killed watchdog fails cleanup instead of
silently accepting a possibly unfinished timer teardown. Cleanup preserves an
existing failure status and fails an otherwise successful run if teardown fails.

These are bounded polling sequences, not hard wall-clock guarantees under
arbitrary scheduler delays. The outer 65-second watchdog starts termination of both
capture and browser; the main capture wait still depends on the operating
system actually terminating that process. Forced watchdog termination cannot
guarantee its timer descendant has exited. This Bash path does not establish
operating-system process-group or job containment. PowerShell uses finite
five-second cleanup waits; cleanup time is separate from the test deadline.
The native capture deadline remains 60 seconds. The outer watchdog's extra
five seconds are teardown grace for a complete timeout diagnostic and orderly
exit, not additional browser execution or I/O time. PowerShell applies the
same grace only to the actual capture child, not deadline/profile helpers.

For diagnosis, select one exact current test basename without changing the
default full gate:

```powershell
.\test-browser-tests.ps1 -Browser 'C:/path/to/chrome.exe' -TestName wfc_text_codec_test
```

```bash
WFC_BROWSER=/usr/bin/chromium WFC_BROWSER_TEST=wfc_text_codec_test bash ./test-browser-tests.sh
```

An unknown name fails instead of silently selecting nothing. Readiness checks
use the first selected current page. Omit the selector to execute the complete
source-derived suite; a focused success does not certify the other programs.

Staging compiles `test/*_test.lpr` for `-Tbrowser`, embeds the matching RTL, and
uses the FPC checker to create harnesses under `build/browser/tests/www`.
It also rebuilds all registered demos and copies their three named public assets
into `demo-entries` below that root. `wfc_browser_demo_entries_test` loads the
actual `index.html?selftest=1` pages in sequential same-origin frames and checks
their rendered contracts. This catches entry-point/bootstrap problems that
controller-only fixtures cannot. The named main stylesheet must also have
loaded, parsed CSS rules; a resource request alone is not proof of success
on browsers that omit HTTP status from resource timing entries. No external
server is required.
Native DOM-parser, socket-server, and renderer-process tests are excluded;
they execute in the native gate. The runner independently derives that same
current source list, rejects missing HTML or compiled scripts, and ignores stale
staged pages rather than counting them as current coverage. It owns a loopback
server and prepares a new browser profile for every attempt. A native monotonic
60-second deadline is created before browser launch. The FPC capture tool
waits for `data-self-test=passed` and all additional required markers, without
accelerating browser time or taking a timed `--dump-dom` snapshot. Rendered DOM
and capture/browser logs remain in unique run directories under
`build/browser/tests/results`. A fixed `<test>.dom` convenience copy is cleared
before each attempt and published only after the fresh capture passes the
independent checker; it is not a substitute for a successful runner exit.

The ensemble stream demo test also requires its application-owned
`data-stream-self-test=passed` and `data-stream-release=passed` markers. Its fake writable-file transactions are
asynchronous: returning from the synchronous Pascal harness alone is not
completion. Pending, failed, or missing stream markers fail the runner even
when the synchronous source tests passed.

The independent-role Voice Studio gate likewise requires
`data-voice-stream-self-test=passed` and `data-voice-stream-release=passed`.
Its pending asynchronous save/release tests cannot be replaced by a successful
transpilation or synchronous harness return. Stage the independent-voice demo
with `build-browser-voices.ps1` or `build-browser-voices.sh`, then serve
`build/browser/voices/www` using the same FPC server. See
[Independent Voices](music-voices.md) and the
[Voice Studio host guide](../examples/music/07_VoiceStudio/README.md).

The actual-page gate additionally requires `data-demo-entries-self-test=passed`
after all registered pages finish. Each page retains its in-page 15-second deadline,
now measured in ordinary browser time. The aggregate's native deadline remains
60 seconds, as for every other program.
Pending/missing page evidence never counts as success. This browser-only test
does not appear in the native gate.

The Ensemble stream harness and actual demo page also require
`data-http-stream-self-test=passed` for optional native-download discovery,
strict capability parsing, current-setting links, and release handling.

PowerShell accepts `-Checker` when staging and `-Server`, `-Checker`, `-Capture`,
and `-Port` when running; the default port is 4180. The shell scripts use
`WFC_BROWSER_CHECK`, `WFC_BROWSER_CAPTURE`, `WFC_SERVE`, `WFC_BROWSER`, and
`WFC_BROWSER_PORT` (default 4180). Shell
execution uses the host's `curl` and standard `sleep`/`kill` commands. Its owned-child
watchdog waits 65 seconds (including the teardown-only grace), then allows up to two seconds before
forced termination; GNU `timeout` is not required. The shell runner is exercised
by the Linux browser lane. Both runners collect per-program failures and fail the
overall run if any current program fails. Interactive demo self-tests use the
same runner with each demo's fuller attribute map:

```powershell
.\test-browser-tests.ps1 -Browser 'C:/path/to/chrome.exe' `
  -WebRoot build/browser/music/www -Page 'index.html?selftest=1' `
  -Expect @('data-state=solved', 'data-arrangement-test=passed')
```

```bash
WFC_BROWSER=/usr/bin/chromium bash ./test-browser-tests.sh \
  --root build/browser/music/www --page 'index.html?selftest=1' \
  --expect data-state=solved --expect data-arrangement-test=passed
```

The mandatory `data-self-test=passed` assertion is included automatically.
Standalone mode cannot be combined with a conformance test selector. These
examples are focused checks, not the complete hosted attribute map.

To rerun only the native host-boundary checks after a native build:

```bash
build/native/bin/wfc_serve_test --integration \
  build/native/bin/wfc_serve build/native/bin
build/native/bin/wfc_music_render_process_test \
  build/native/bin/MusicStudioRender build/native/bin
build/native/bin/wfc_music_ensemble_render_process_test \
  build/native/bin/EnsembleStudioRender build/native/bin
build/native/bin/wfc_music_voices_render_process_test \
  build/native/bin/VoiceStudioRender build/native/bin
```

These use uniquely owned fixtures beneath the supplied existing directory and
exercise real child processes and files. The server test also covers binary
GET/HEAD, malformed requests, stalled/abandoned connections, and link escapes.
The renderer test checks 4-, 6-, and 180-second WAVE extents, deterministic
prefixes, duration rejection, and preservation of an output created during a
publication race. Their pure source-level companion tests run separately.

## Native new-file publication

`tools/wfc_atomic_new_file.pas` supplies a reusable `TWfcAtomicNewFile` host
utility, separate from the portable music libraries. Construct it with an
explicit new destination whose parent already exists, feed borrowed byte
blocks with `WriteBytes`, then call `Publish` only after the producer finishes
successfully. `Cancel` and destruction never publish implicitly.

The helper exclusively creates a unique sibling, flushes and closes it, then
publishes without replacing an existing destination. It uses a non-replacing
move on Windows and link creation followed by sibling removal on Unix. A
destination appearing during generation is preserved. Failure cleanup targets
only the exact owned partial path; `CleanupError` exposes a cleanup failure.
Use a trusted parent directory: this is not a sandbox against hostile path
changes, nor a claim of crash-durable directory metadata. Unix filesystems
must support sibling hard links; there is no overwriting fallback.

The [Ensemble Studio renderer](../examples/music/06_EnsembleStudio/README.md)
and independent-role `VoiceStudioRender --format wave|midi` use this utility
after exact duration/frame or MIDI planning. See
[Voice Studio](../examples/music/07_VoiceStudio/README.md) for actual commands.
The [workspace CLI](pipeline-workspaces.md#native-cli) uses the same helper
after complete journal authoring and encoding, with an explicit new output
path for each accepted action. Existing input/output files are not overwritten;
file I/O failure does not change the already saved history.
The browser host uses
a user-authorized writable-file transaction instead; choosing an existing
file in the browser save picker can authorize replacement and does not carry
the native new-file-only guarantee.

## Native solver benchmark

The native build also produces `wfc_solver_benchmark`, a project-owned FPC
measurement host. It needs no browser or server. For example:

```text
build/native/bin/wfc_solver_benchmark --cells 4096 --values 4 --weights skewed --topology independent --compatibility dense --trace 0 --repeat 3
```

On Windows, the executable has an `.exe` suffix. `--help` lists the full
strict CLI, including line/equality propagation controls and optional trace
capture. It reports monotonic whole-solve milliseconds and deterministic
checksums/counters to stdout; it writes no files. Usage errors return 2,
failed measurements return 1, and success returns 0. There is no timing
pass/fail threshold. Its explicit benchmark safety bounds are not library
or composition limits. Read the [decision-index experiment](research/decision-index-v1.md)
for the frozen scan baseline, reproduction method, raw results and caveats.

## Evidence scope

Native conformance tests and real command-line process tests remain separate
from browser interaction checks. Research notes preserve their original
measurements; a historical pas2js result is not retroactive browser coverage.
See [building and testing](building.md) for the current gates and
[dependency policy](dependencies.md) for admission rules.
