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
  'examples/2D/05_LearnedPatternWorld','examples/learning/05_TrainingStudio',
  'examples/music/05_MusicStudio','examples/passes/04_NeighborhoodCounts',
  'examples/passes/05_DeterministicRestarts',
  'examples/music/06_EnsembleStudio') | ForEach-Object {
    '-Fu' + (Join-Path $repositoryRoot $_)
  }
foreach ($source in Get-ChildItem -LiteralPath (Join-Path $repositoryRoot 'test') -Filter '*_test.lpr') {
  if ($source.BaseName -in @('wfc_browser_dom_test','wfc_serve_test','wfc_music_render_process_test')) { continue }
  & $Compiler -B -Tbrowser -Mdelphi -Jc '-Jirtl.js' @unitPaths "-FU$units" "-FE$web" $source.FullName
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
  $html = Join-Path $web ($source.BaseName + '.html')
  if (Test-Path -LiteralPath $html -PathType Leaf) { Remove-Item -LiteralPath $html }
  & $Checker --harness ($source.BaseName + '.js') --dom $html
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}
Write-Host "Pascal browser conformance staged in '$web'."
