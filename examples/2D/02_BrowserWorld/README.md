# BrowserWorld

`BrowserWorld` is an interactive browser host for the reusable three-pass 2D
model. The application, event handlers, constraint edits, rendering, validation,
and deterministic self-test are written in Pascal and compiled with pas2js. The
HTML and CSS provide a dependency-free interface; there are no CDN assets or
runtime network calls.

The workbench exposes the three synchronized layers:

```text
terrain -> biome -> foliage
```

Click any canvas to select the same coordinate in every layer, use its arrow
keys to move the selection, or enter X and Y directly. A typed value can be
locked into that cell before the complete pipeline is solved again. Impossible
locks remain caller-owned and visible while the solver reports a contradiction;
unlocking the cell replays the original deterministic result. Seed,
wrapped-edge topology, reset, and clear-all-lock controls are also available.

## Build

Use a pas2js compiler with its matching browser RTL. From the repository root:

```text
./build-browser.sh
```

On PowerShell:

```text
./build-browser.ps1
```

Set `PAS2JS` to a compiler path when `pas2js` is not on `PATH`. Both scripts
stage a self-contained local site under:

```text
build/browser/world2d/www
```

Generated JavaScript remains a build artifact and is not committed.

Serve the staged directory over HTTP rather than opening `index.html` directly:

```text
build/native/bin/wfc_serve --root build/browser/world2d/www --port 8080
```

Build the server with the native gate first; use `wfc_serve.exe` on Windows.
See [development tools](../../../docs/development-tools.md) for its loopback-only serving boundary.

Then open `http://127.0.0.1:8080/`.

## Browser contract

The initial bounded showcase is shared with the native demo. Model
version 1 produces these portable signatures:

| Seed | Signature |
| --- | --- |
| `0` | `1:5B0DD75D:08022AF1:A40D0955` |
| `1297110833` (`$4D505731`) | `1:81F03F86:9069C5F9:41619106` |

The hosted build opens `?selftest=1` in headless Chrome. That Pascal-driven test
generates seed `0`, verifies the signature and canvas rendering, installs an
illegal tree lock over ocean, observes the expected foliage contradiction,
unlocks it, and verifies exact recovery. Machine-readable result attributes are
written to the document body; pixel snapshots are deliberately not used as
goldens. `data-state` starts as `loading` and becomes `solved`, `contradiction`,
or `error`. Without the fixture, `data-self-test` is `not-requested`; the
fixture moves it through `pending` to `passed` or `failed`.

See the [2D ecosystem documentation](../../../docs/world2d.md) for the model,
typed wrapper, independent validator, and signature format.
