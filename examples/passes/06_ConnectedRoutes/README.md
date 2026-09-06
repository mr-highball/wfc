# Connected Routes

Connected Routes is a shared native/browser workbench for rooted
reciprocal-port connectivity. It demonstrates two three-pass models:

- `terrain → roads → housing` on a 7 × 5 town map; and
- `structure → circulation → features` on two 5 × 4 floors.

The routing pass owns the connectivity descriptor. Its root and two fixed
terminals must share one component. Closing a bridge or shaft changes the
routing domains, then selective repair regenerates that pass and its
descendants while reusing the upstream provider unchanged.

The shared `connected_routes_demo.pas` captures committed public cells and
checks them with a separate breadth-first traversal. An edge needs reciprocal
selected ports. The browser and native hosts expose no map or SVG after a
failed transaction. Everything is project-owned Pascal, HTML, and CSS under
the repository's MIT license; there are no external packages, services, fonts,
or asset downloads.

## Native host

The normal `build.ps1` or `build.sh` gate compiles the host, runs its self-test,
and checks native SVG publication behavior. Run the resulting executable (add
`.exe` on Windows):

```bash
build/native/bin/ConnectedRoutes --selftest
build/native/bin/ConnectedRoutes --case town --portal first --seed 0
build/native/bin/ConnectedRoutes --case town --portal first --repair-to second
build/native/bin/ConnectedRoutes --case circulation --portal first --repair-to second --svg circulation.svg
```

For an isolated checked build:

```bash
mkdir -p build/connectivity/native/units build/connectivity/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Futools \
  -Fuexamples/passes/06_ConnectedRoutes \
  -FUbuild/connectivity/native/units -FEbuild/connectivity/native/bin \
  examples/passes/06_ConnectedRoutes/ConnectedRoutes.lpr
build/connectivity/native/bin/ConnectedRoutes --selftest
```

The complete command-line surface is:

```text
ConnectedRoutes [OPTIONS]
ConnectedRoutes --selftest
  --case town|circulation
  --seed UINT32
  --portal first|second|both|none
  --repair-to first|second|both|none
  --required-only
  --backtracks NONNEGATIVE
  --pass-backtracks NONNEGATIVE
  --trace
  --svg NEW-FILE
```

`--help`/`-h` prints usage and `--version` prints the demo version. Options may
appear in any order but may not be repeated. The defaults are town, first
portal, seed 0, all participants required, 4096 local backtracks, 32 pass
backtracks, and trace capture off.

`--repair-to` first commits a baseline using `--portal`, then changes only the
route pass and its descendants. `--required-only` permits disconnected
optional route participants; it never relaxes root or terminal reachability.
`--trace` captures diagnostics without changing the connectivity contract.
The backtrack values are finite search budgets, not wall-time limits, and
budget exhaustion is not proof of infeasibility.

`--svg` publishes a canonical ASCII SVG only after successful independent
validation. The destination must be a new file: an existing path is not
replaced. Success exits with status 0, malformed input or a host error exits
with status 1, and a clean unsolved/limit result exits with status 2.

## Browser host

Stage the browser application from PowerShell:

```powershell
.\build-browser-connectivity.ps1 -Compiler 'C:/path/to/pas2js.exe'
.\build\native\bin\wfc_serve.exe --root build/browser/connectivity/www --port 4181
```

Or from a shell:

```bash
PAS2JS=/opt/pas2js/bin/pas2js bash ./build-browser-connectivity.sh
build/native/bin/wfc_serve --root build/browser/connectivity/www --port 4181
```

Build the included FPC server with the normal native gate first. It defaults
to `127.0.0.1`; stop the foreground process with Ctrl+C. Open
`http://127.0.0.1:4181/`. The staging scripts put the compiled program,
`index.html`, and `connectedroutes.css` under
`build/browser/connectivity/www`. The page intentionally loads only the
generated `BrowserConnectedRoutes.js`; its RTL is embedded by the compiler.

For opt-in serving on one trusted-LAN address, see
[development tools](../../../docs/development-tools.md#access-from-a-trusted-local-network).

Generate establishes the selective-repair baseline. Editing the case resets
that baseline; editing a portal or connectivity mode invalidates stale public
output until Generate or Repair succeeds. Repair is available only after a
successful baseline. The map and SVG show each Z layer as a separate panel, so
the circulation case is a two-floor projection rather than a perspective
renderer. Paired up/down shaft ports identify cross-floor witness edges.

Append `?selftest=1` for the event-driven browser fixture. A passing run sets
the body marker `data-self-test="passed"` and checks baseline generation,
alternate town repair, failed-repair rollback, multi-floor generation and
repair, and stale-output invalidation. The maintained browser gate derives the
current test set from source and verifies the rendered markers in a real
browser through the included server.

## Connectivity semantics

A participating value has a set of directional openings. Two adjacent values
form an edge only when both expose inverse ports across reciprocal physical
neighbors and their ordinary adjacency rules permit the pair. The root and
fixed terminals always participate and must be in one component.

A selected profile may expose a possible port that its neighbor does not
reciprocate. The browser therefore draws actual traversable links prominently
and lists possible profile ports as smaller metadata; the two are deliberately
not treated as equivalent.

With **Connect every participating route cell** enabled, any disconnected
route island invalidates the assignment. With it disabled, optional islands
may remain, but they do not count as part of the terminal witness. Neither
mode asks for a shortest route, minimum edit, flow capacity, or a deadline.

See the [rooted connectivity contract](../../../docs/connectivity.md) for the
public API, ownership rules, exact semantics, propagation argument,
transactions, diagnostics, and current boundaries.
