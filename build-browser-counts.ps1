[CmdletBinding()]
param(
  [string] $Compiler = $(if ($env:PAS2JS) { $env:PAS2JS } else { 'pas2js' }),
  [string[]] $CompilerOptions = @()
)

$ErrorActionPreference = 'Stop'
if (Get-Variable PSNativeCommandUseErrorActionPreference -ErrorAction SilentlyContinue) {
  $PSNativeCommandUseErrorActionPreference = $false
}
$repositoryRoot = $PSScriptRoot
$sourceDirectory = Join-Path $repositoryRoot 'src'
$browserDirectory = Join-Path $repositoryRoot 'examples/passes/04_NeighborhoodCounts'
$browserSource = Join-Path $browserDirectory 'BrowserNeighborhoodCounts.lpr'
$browserApp = Join-Path $browserDirectory 'browser_neighborhood_counts_app.pas'
$sharedDemo = Join-Path $browserDirectory 'neighborhood_count_demo.pas'
$browserHtml = Join-Path $browserDirectory 'index.html'
$browserCss = Join-Path $browserDirectory 'counts.css'
$outputDirectory = Join-Path $repositoryRoot 'build/browser/counts'
$unitOutputDirectory = Join-Path $outputDirectory 'units'
$webOutputDirectory = Join-Path $outputDirectory 'www'
$stagedJavaScript = Join-Path $webOutputDirectory 'BrowserNeighborhoodCounts.js'

foreach ($requiredFile in @(
    $browserSource, $browserApp, $sharedDemo, $browserHtml, $browserCss)) {
  if (-not (Test-Path -LiteralPath $requiredFile -PathType Leaf)) {
    throw "Required Neighborhood Counts browser file was not found: $requiredFile"
  }
}
New-Item -ItemType Directory -Force -Path $unitOutputDirectory, $webOutputDirectory | Out-Null
Remove-Item -LiteralPath $stagedJavaScript -Force -ErrorAction SilentlyContinue
$compilerArguments = @(
  $CompilerOptions
  '-B'
  '-Tbrowser'
  '-Mdelphi'
  '-Jc'
  '-Jirtl.js'
  "-Fu$sourceDirectory"
  "-Fu$browserDirectory"
  "-FU$unitOutputDirectory"
  "-FE$webOutputDirectory"
  $browserSource
)
Write-Host "Building Neighborhood Counts with '$Compiler'."
& $Compiler @compilerArguments
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
if (-not (Test-Path -LiteralPath $stagedJavaScript -PathType Leaf) -or
    (Get-Item -LiteralPath $stagedJavaScript).Length -eq 0) {
  throw "pas2js did not produce $stagedJavaScript"
}
Copy-Item -LiteralPath $browserHtml, $browserCss -Destination $webOutputDirectory -Force
foreach ($assetName in @('index.html', 'counts.css')) {
  $stagedAsset = Join-Path $webOutputDirectory $assetName
  if ((Get-Item -LiteralPath $stagedAsset).Length -eq 0) {
    throw "Staged Neighborhood Counts asset is empty: $stagedAsset"
  }
}
Write-Host "Neighborhood Counts staged in '$webOutputDirectory'."
