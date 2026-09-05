[CmdletBinding()]
param(
  [string] $Compiler = $(if ($env:PAS2JS) {
    $env:PAS2JS
  } else {
    'pas2js'
  }),
  [string[]] $CompilerOptions = @()
)

$ErrorActionPreference = 'Stop'
if (Get-Variable -Name PSNativeCommandUseErrorActionPreference `
    -ErrorAction SilentlyContinue) {
  $PSNativeCommandUseErrorActionPreference = $false
}

$repositoryRoot = $PSScriptRoot
$sourceDirectory = Join-Path $repositoryRoot 'src'
$browserDirectory = Join-Path $repositoryRoot 'examples/music/06_EnsembleStudio'
$browserSource = Join-Path $browserDirectory 'BrowserEnsembleStudio.lpr'
$browserApp = Join-Path $browserDirectory 'browser_ensemble_studio_app.pas'
$studioOwner = Join-Path $browserDirectory 'ensemble_studio_workbench.pas'
$browserHtml = Join-Path $browserDirectory 'index.html'
$browserCss = Join-Path $browserDirectory 'ensemblestudio.css'
$outputDirectory = Join-Path $repositoryRoot 'build/browser/ensemble'
$unitOutputDirectory = Join-Path $outputDirectory 'units'
$webOutputDirectory = Join-Path $outputDirectory 'www'
$stagedJavaScript = Join-Path $webOutputDirectory 'BrowserEnsembleStudio.js'

foreach ($requiredFile in @(
    $browserSource, $browserApp, $studioOwner, $browserHtml, $browserCss)) {
  if (-not (Test-Path -LiteralPath $requiredFile -PathType Leaf)) {
    throw "Required Ensemble Studio browser file was not found: $requiredFile"
  }
}

New-Item -ItemType Directory -Force -Path `
  $unitOutputDirectory, $webOutputDirectory | Out-Null
Remove-Item -LiteralPath $stagedJavaScript -Force -ErrorAction SilentlyContinue

$compilerArguments = @(
  $CompilerOptions
  '-B'
  '-Tbrowser'
  '-Mdelphi'
  '-Jc'
  '-Jirtl.js'
  "-Fu$sourceDirectory"
  "-Fu$repositoryRoot/tools"
  "-Fu$browserDirectory"
  "-FU$unitOutputDirectory"
  "-FE$webOutputDirectory"
  $browserSource
)

Write-Host "Building Ensemble Studio with '$Compiler'."
& $Compiler @compilerArguments
$compilerExitCode = $LASTEXITCODE
if ($compilerExitCode -ne 0) {
  exit $compilerExitCode
}

if (-not (Test-Path -LiteralPath $stagedJavaScript -PathType Leaf) -or
    (Get-Item -LiteralPath $stagedJavaScript).Length -eq 0) {
  throw "pas2js did not produce $stagedJavaScript"
}

Copy-Item -LiteralPath $browserHtml, $browserCss `
  -Destination $webOutputDirectory -Force
foreach ($stagedAsset in @(
    (Join-Path $webOutputDirectory 'index.html'),
    (Join-Path $webOutputDirectory 'ensemblestudio.css'))) {
  if ((Get-Item -LiteralPath $stagedAsset).Length -eq 0) {
    throw "Staged Ensemble Studio asset is empty: $stagedAsset"
  }
}

Write-Host "Ensemble Studio staged in '$webOutputDirectory'."
