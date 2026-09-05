[CmdletBinding()]
param(
  [string] $Compiler = $(if ($env:PAS2JS) { $env:PAS2JS } else { 'pas2js' }),
  [string[]] $CompilerOptions = @()
)

$ErrorActionPreference = 'Stop'
if (Get-Variable -Name PSNativeCommandUseErrorActionPreference -ErrorAction SilentlyContinue) {
  $PSNativeCommandUseErrorActionPreference = $false
}

$repositoryRoot = $PSScriptRoot
$sourceDirectory = Join-Path $repositoryRoot 'src'
$commonDirectory = Join-Path $repositoryRoot 'examples/3D/common'
$browserDirectory = Join-Path $repositoryRoot 'examples/3D/03_BrowserBuilding'
$browserSource = Join-Path $browserDirectory 'BrowserBuilding.lpr'
$browserHtml = Join-Path $browserDirectory 'index.html'
$browserCss = Join-Path $browserDirectory 'browserbuilding.css'
$outputDirectory = Join-Path $repositoryRoot 'build/browser/building3d'
$unitOutputDirectory = Join-Path $outputDirectory 'units'
$webOutputDirectory = Join-Path $outputDirectory 'www'
$stagedJavaScript = Join-Path $webOutputDirectory 'BrowserBuilding.js'

foreach ($requiredFile in @($browserSource, $browserHtml, $browserCss)) {
  if (-not (Test-Path -LiteralPath $requiredFile -PathType Leaf)) {
    throw "Required Building 3D browser file was not found: $requiredFile"
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
  "-Fu$commonDirectory"
  "-Fu$browserDirectory"
  "-FU$unitOutputDirectory"
  "-FE$webOutputDirectory"
  $browserSource
)

Write-Host "Building the Building 3D browser workbench with '$Compiler'."
& $Compiler @compilerArguments
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

if (-not (Test-Path -LiteralPath $stagedJavaScript -PathType Leaf) -or
    (Get-Item -LiteralPath $stagedJavaScript).Length -eq 0) {
  throw "pas2js did not produce $stagedJavaScript"
}

Copy-Item -LiteralPath $browserHtml, $browserCss -Destination $webOutputDirectory -Force
foreach ($stagedAsset in @(
    (Join-Path $webOutputDirectory 'index.html'),
    (Join-Path $webOutputDirectory 'browserbuilding.css'))) {
  if ((Get-Item -LiteralPath $stagedAsset).Length -eq 0) {
    throw "Staged browser asset is empty: $stagedAsset"
  }
}

Write-Host "Building 3D browser workbench staged in '$webOutputDirectory'."
