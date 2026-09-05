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
$browserDirectory = Join-Path $repositoryRoot 'examples/passes/06_ConnectedRoutes'
$browserSource = Join-Path $browserDirectory 'BrowserConnectedRoutes.lpr'
$browserApp = Join-Path $browserDirectory 'browser_connected_routes_app.pas'
$sharedDemo = Join-Path $browserDirectory 'connected_routes_demo.pas'
$browserHtml = Join-Path $browserDirectory 'index.html'
$browserCss = Join-Path $browserDirectory 'connectedroutes.css'
$outputDirectory = Join-Path $repositoryRoot 'build/browser/connectivity'
$unitOutputDirectory = Join-Path $outputDirectory 'units'
$webOutputDirectory = Join-Path $outputDirectory 'www'
$stagedJavaScript = Join-Path $webOutputDirectory 'BrowserConnectedRoutes.js'

foreach ($requiredFile in @(
    $browserSource, $browserApp, $sharedDemo, $browserHtml, $browserCss)) {
  if (-not (Test-Path -LiteralPath $requiredFile -PathType Leaf)) {
    throw "Required Connected Routes browser file was not found: $requiredFile"
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
Write-Host "Building Connected Routes with '$Compiler'."
& $Compiler @compilerArguments
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
if (-not (Test-Path -LiteralPath $stagedJavaScript -PathType Leaf) -or
    (Get-Item -LiteralPath $stagedJavaScript).Length -eq 0) {
  throw "pas2js did not produce $stagedJavaScript"
}
Copy-Item -LiteralPath $browserHtml, $browserCss -Destination $webOutputDirectory -Force
foreach ($assetName in @('index.html', 'connectedroutes.css')) {
  $stagedAsset = Join-Path $webOutputDirectory $assetName
  if ((Get-Item -LiteralPath $stagedAsset).Length -eq 0) {
    throw "Staged Connected Routes asset is empty: $stagedAsset"
  }
}
Write-Host "Connected Routes staged in '$webOutputDirectory'."
