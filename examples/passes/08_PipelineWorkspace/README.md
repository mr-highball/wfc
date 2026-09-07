# Pipeline Workspace

An editable, generic constraint-pipeline workspace written in Pascal. The browser adapter uses pas2js and the included FPC static server. Its shared controller, preset builders, diagnostic renderer, and native host use the same portable pipeline and workspace APIs.

The editor separates three things: document drafts, an explicitly executed live history, and a historical successful baseline. Loading a file is not execution. A valid journal envelope is not verified solver evidence. A retained baseline is not current output after an edit.

## Build and serve

Run from an integrated repository checkout containing this example and the included root build scripts. FPC and pas2js are compiler prerequisites; no separate application runtime or asset dependencies are required.

```powershell
.\build-workspace.ps1 -Compiler 'path\to\fpc.exe'
.\build-browser-workspace.ps1 -Compiler 'path\to\pas2js.exe'
.\build\workspace\native\bin\wfc_serve.exe --root build/browser/workspace/www --port 4180
```

```sh
FPC=/path/to/fpc bash ./build-workspace.sh
PAS2JS=/path/to/pas2js bash ./build-browser-workspace.sh
./build/workspace/native/bin/wfc_serve --root build/browser/workspace/www --port 4180
```

Open `http://127.0.0.1:4180/`. Build scripts do not start a server. The server defaults to loopback; exposing it to a trusted LAN is an explicit `--bind` choice described in [development tools](../../../docs/development-tools.md). Do not treat the static development server as an authenticated public service.

The native build also produces `PipelineWorkspace` and `wfc_workspace`. Their `--help` output documents explicit file-oriented operations. The [native guide](NATIVE.md) walks through real preset/import exports, all adjustable limits, exact exit statuses and continued history authoring with the existing CLI. Native and browser workflows exchange canonical recipe, run, and journal documents; the browser does not require a remote execution endpoint.

## First composition

1. Choose **Stage landscape defaults** or **Stage learned-sequence defaults**. This creates complete canonical recipe/run drafts, not a graph or solve.
2. Adjust preset settings, seed, strategy, and search budgets. Edit each geometry row if necessary, then **Rebuild selected preset drafts using the geometry below**. This is explicit new preset authoring with no lock/domain input rows; it does not copy live edits.
3. **Inspect drafts — no graph** checks canonical documents and bindings. It neither proves satisfiability nor changes live history. Inspecting arbitrary imported drafts ends preset-rebuild mode, so subsequent edits remain explicit document authoring.
4. **Begin explicit epoch** creates a live, unsolved epoch from those exact documents. Drafts remain separate. Edits are allowed before the first solve.
5. **Execute initial solve once** records the actual result. A normal unsuccessful solve is still a recorded result, not an application exception or hidden retry.
6. Select a public pass, choose current state or historical baseline, and **Render exact window**. The viewport is not the composition extent.

There is no UI target duration, bar count, fixed world-size ceiling, seed search, or automatic retry. You choose pass extents and the caller policy allowances. Versioned format capacities, integer ranges, solver complexity, and available browser memory still apply. Large work may be expensive or impossible for the selected model; the editor does not claim unlimited physical resources.

### Included priming examples

The landscape preset declares independent terrain, foliage, and housing grids. Foliage trees require land under their entire cell footprint. Houses require land and clear foliage under their entire footprint. All six token weights are editable. Coarse-to-fine relationships are real mapped clauses, not a drawing overlay or post-generation cleanup. Diagnostic images are not an independent physical-safety certificate.

The learned-sequence preset trains from the included MIT-authored `step` / `turn` / `rest` corpus. It retains the private learned states, their public projection, and a public alias, alongside an unrelated grid. Sequence order, source boundary, and output extent policy are exposed separately. Open/wrapped source training and graph wrap are not interchangeable. Private, projected, and alias pass layouts must satisfy the same-layout bridge contract; edit linked geometry explicitly rather than expecting silent resizing.

Both examples expose complete recipe and run text. They are starting points, not guaranteed-success settings or substitutes for the generic import path. Other supported learned/rule resources, clauses, quotas, connectivity policies, and provenance are retained through the underlying canonical formats.

## Inputs, options, and repair

**Replace drafts with live recipe / run** copies the complete current definition and applied inputs into the text editors. Use it before staging changes when the existing run draft is older than recent cell edits. It does not publish a new action.

**Stage options into run draft** changes only invocation options and preserves all locks, domains, pass extents, and the run's format. **Apply complete run draft** is an explicit edit of the current epoch. Changing seed, layout, format, or recipe binding requires an explicit new epoch instead. One-way mode requires zero pass backtracks; selecting it does not silently rewrite that budget.

Cell controls target the selected public pass at local X/Y/Z. Tokens are displayed as canonical percent text. **Inspect this cell's actual inputs** shows applied lock/domain rows rather than guessing them from generated output. The domain checkboxes follow the canonical public vocabulary order. Selecting no tokens authors an explicit empty domain; **Clear domain row** removes that row instead. A lock excluded by its domain is rejected atomically.

Every accepted helper call is a separate history action, including clear-absent and no-op edits. Lock then clear is not collapsed into a no-op: actual invalidation history matters. The editor neither normalizes contradictory inputs nor coalesces accepted operations.

Repair always uses the complete **currently applied run**, never an unapplied text draft. Select explicit roots, including a private provider when required, and preview the scope. Preview replays prior history under the current policy before inspecting permission. It does not execute or publish the newly requested repair. Requested, active, authored, required, and missing pass sets remain distinct.

**Execute exactly this preview** requires the same publication, applied run, root selection, and policy. Editing roots or policy invalidates the preview. Missing permission is not silently added. A public alias may need its private learned provider to be authorized. An unsuccessful repair remains in history, keeps accepted edits pending, and does not relabel a retained baseline as current.

Initial solve is available once per epoch. If it fails without establishing a successful baseline, inspection and saving still work, but another initial attempt or selective repair is not a recovery shortcut. Begin an explicit new epoch with the desired inputs and options.

## Geometry and diagnostic view

The geometry table shows every pass, including private passes: rank, local cell extents, world origin, positive world pitch, and wrap. These are definition/invocation values, not image dimensions.

**Load live geometry into this form** replaces the separate geometry form without creating an epoch. For a mapped recipe, **Begin mapped epoch from edited live geometry** preserves the live resources, declarations, clauses, quotas, connectivity, provenance, complete input rows, and current solve options. Only the explicit topology/extent table and seed change. Shrinking across an authored input or breaking a projection/alias relation rejects the action; it does not clip or resample. Legacy uniform recipes use their canonical definition/run path instead.

The slice renderer uses a public pass's local XY window at an exact Z. Its initial small viewing window is only a convenience; edit X/Y/Z, width, and height to inspect any valid range. Render-cell and SVG-byte allowances are separate caller controls. Out-of-range views are rejected, not silently cropped.

Each SVG records full world footprint bounds and its local window. Token and label metadata use canonical percent text. Current, not-current, and historical-baseline views are explicitly distinguished. Only the shared escaped renderer output enters the SVG container; imported labels, diagnostics, and evidence use text nodes. Click a rendered cell to inspect its inputs, or use the equivalent keyboard-accessible coordinate controls. A viewport change invalidates its prior downloadable image.

## Files, evidence, and restore

Recipe, run, and journal files are selected locally. Import reads bytes, checks the captured input allowance, and rejects anything except printable ASCII plus LF. It does not strip a BOM, normalize CRLF, trim spaces, repair encoding, or add a final newline. Non-ASCII source labels/tokens belong in the canonical percent representation. Textarea authoring follows the browser's normal LF text model; exact file import remains stricter.

Import only replaces a draft after the entire file passes that byte boundary. Canonical codecs then decide grammar and binding validity. Failed reads leave both the old draft and live history intact. There are no path fetches, background uploads, autosaves, or implicit imports from resource provenance labels.

**Inspect claims — no graph** checks a complete canonical journal envelope and reports unverified actions/context bindings. It does not compare evidence against execution. **Replay and atomically restore** performs real replay, compares every complete claimed outcome, and publishes the retained verified owner only after the entire candidate succeeds. A rejected restore leaves the old live publication and baseline intact. Normal unsuccessful solve outcomes are valid history, not a reason to forge a successful replay result.

**Replace journal draft with live history** copies a complete editable document. **Prepare live journal download** creates a file link for the actual current publication, including failed outcomes. Preparing a download is not writing to a user-selected file; follow the link to use the browser's save workflow. Later live changes invalidate that download link. Downloaded SVGs are diagnostics, not journal substitutes.

The last accepted operation exposes its real detached report summary and complete canonical evidence. Journal inspection, repair preview, and explicit replay have different guarantees; their labels deliberately do not interchange them.

## Policy, lifecycle, and testing

The host policy panel exposes all journal, replacement, outcome, evidence, and replay allowances. They bound logical inventories or encoded outputs, not exact peak heap usage or wall time. Histories are complete rather than hash-only summaries, so context/evidence/replay allowances may need to rise together. Bounds failures are reported explicitly; neither the policy nor the solver budgets are silently raised.

The application owns one workbench. Drafts, geometry controls, selected viewport, and pending DOM work are not alternative solver state. Each action captures its arguments and expected publication before a single queued callback. Controls and download navigation are blocked while pending/running. Cancellation can stop a queued action or file read before completion; a synchronous solve cannot be interrupted by a UI timer. On destruction, callbacks are unbound, pending timers/readers are cancelled, object URLs are revoked, and owners are freed.

Unexpected runtime errors clear current presentation claims and require an authoritative refresh before further live mutations. Rejected typed edits preserve the current owner. Ordinary unsuccessful solves remain inspectable completed operations. These are deliberately different statuses.

The browser adapter has a real DOM test seam: visible `data-action` buttons, draft inputs, disabled controls, `workbench`'s `aria-busy`, and body operation/currentness/publication markers. A dedicated real-entry test drives those controls; startup itself never claims self-test success. Shared controller/native tests are not a substitute for actual browser interaction conformance. No visual inspection is implied by functional DOM verification.

All example and tooling sources remain under the repository's MIT license. See [workspace journals](../../../docs/pipeline-workspaces.md), [pipeline sessions](../../../docs/pipeline-sessions.md), and [pipeline composition](../../../docs/pipeline-composition.md) for the underlying contracts.
