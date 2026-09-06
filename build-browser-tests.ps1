[CmdletBinding()]
param(
  [string] $Compiler = $(if ($env:PAS2JS) { $env:PAS2JS } else { 'pas2js' }),
  [string] $Checker = ''
)
$ErrorActionPreference = 'Stop'
if (Get-Variable PSNativeCommandUseErrorActionPreference -ErrorAction SilentlyContinue) {
  $PSNativeCommandUseErrorActionPreference = $false
}
$repositoryRoot = $PSScriptRoot
if (-not $Checker) { $Checker = Join-Path $repositoryRoot 'build/native/bin/wfc_browser_check.exe' }
if (-not (Test-Path -LiteralPath $Checker -PathType Leaf)) { throw 'Build the included FPC browser checker first.' }
$outputRoot = Join-Path $repositoryRoot 'build/browser/tests'
$units = Join-Path $outputRoot 'units'
$web = Join-Path $outputRoot 'www'
New-Item -ItemType Directory -Force -Path $units, $web | Out-Null
$unitPaths = @('src','tools','examples/2D/common','examples/3D/common',
  'examples/music/01_simple_A_major','examples/music/02_simple_song_riffs',
  'examples/2D/05_LearnedPatternWorld','examples/learning/05_TrainingStudio',
  'examples/music/05_MusicStudio','examples/passes/04_NeighborhoodCounts',
  'examples/passes/05_DeterministicRestarts',
  'examples/passes/02_TraceInspector',
  'examples/text/03_PassComposition',
  'examples/passes/06_ConnectedRoutes',
  'examples/music/06_EnsembleStudio','examples/music/07_VoiceStudio') | ForEach-Object {
    '-Fu' + (Join-Path $repositoryRoot $_)
  }
foreach ($source in Get-ChildItem -LiteralPath (Join-Path $repositoryRoot 'test') -Filter '*_test.lpr') {
  if ($source.BaseName -in @('wfc_package_check_process_test','wfc_artifact_cli_process_test')) { continue }
  if ($source.BaseName -in @('wfc_browser_dom_test','wfc_browser_args_test','wfc_browser_socket_test','wfc_browser_websocket_test','wfc_browser_cdp_test','wfc_browser_capture_test','wfc_serve_test','wfc_music_render_process_test','wfc_music_ensemble_render_process_test','wfc_music_ensemble_midi_render_process_test','wfc_music_voices_render_process_test','wfc_connectivity_process_test','wfc_music_studies_process_test')) { continue }
  & $Compiler -B -Tbrowser -Mdelphi -Jc '-Jirtl.js' @unitPaths "-FU$units" "-FE$web" $source.FullName
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
  $html = Join-Path $web ($source.BaseName + '.html')
  if (Test-Path -LiteralPath $html -PathType Leaf) { Remove-Item -LiteralPath $html }
  & $Checker --harness ($source.BaseName + '.js') --dom $html
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}
# Exercise real entry pages as well as controller fixtures. Copy only the three
# freshly built public assets for each demo, never arbitrary staging contents.
$entryDemos = @(
  @('world2d', 'build-browser', 'BrowserWorld.js', 'browserworld.css'),
  @('text-passes', 'build-browser-text', 'BrowserTextPassComposition.js', 'browsertextpasses.css'),
  @('building3d', 'build-browser-building3d', 'BrowserBuilding.js', 'browserbuilding.css'),
  @('training', 'build-browser-training', 'BrowserTrainingStudio.js', 'trainingstudio.css'),
  @('music', 'build-browser-music', 'BrowserMusicStudio.js', 'musicstudio.css'),
  @('counts', 'build-browser-counts', 'BrowserNeighborhoodCounts.js', 'counts.css'),
  @('ensemble', 'build-browser-ensemble', 'BrowserEnsembleStudio.js', 'ensemblestudio.css'),
  @('voices', 'build-browser-voices', 'BrowserVoiceStudio.js', 'voicestudio.css'),
  @('connectivity', 'build-browser-connectivity', 'BrowserConnectedRoutes.js', 'connectedroutes.css'),
  @('terraces', 'build-browser-terraces', 'BrowserTerraces.js', 'terraces.css')
)
foreach ($demo in $entryDemos) {
  & (Join-Path $repositoryRoot ($demo[1] + '.ps1')) -Compiler $Compiler
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
  $entrySource = Join-Path $repositoryRoot ('build/browser/' + $demo[0] + '/www')
  $entryTarget = Join-Path $web ('demo-entries/' + $demo[0])
  New-Item -ItemType Directory -Force -Path $entryTarget | Out-Null
  foreach ($asset in @('index.html', $demo[2], $demo[3])) {
    Copy-Item -LiteralPath (Join-Path $entrySource $asset) -Destination $entryTarget -Force
  }
}
# A separate real asynchronous capture fixture is not a conformance program.
# Keep its Pascal bootstrap and compiled RTL together under a named sub-root.
$captureFixture = Join-Path $web 'capture-fixture'
New-Item -ItemType Directory -Force -Path $captureFixture | Out-Null
& $Compiler -B -Tbrowser -Mdelphi -Jc '-Jirtl.js' "-FU$units" "-FE$captureFixture" (Join-Path $repositoryRoot 'test/browser_capture/fixture.lpr')
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
Copy-Item -LiteralPath (Join-Path $repositoryRoot 'test/browser_capture/index.html') -Destination $captureFixture -Force
Write-Host "Pascal browser conformance, capture fixture, and ten actual demo entries staged in '$web'."
