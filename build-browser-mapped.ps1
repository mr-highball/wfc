# SPDX-License-Identifier: MIT
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
$browserDirectory = Join-Path $repositoryRoot 'examples/passes/07_MappedWorld'
$outputDirectory = Join-Path $repositoryRoot 'build/browser/mapped'
$unitOutputDirectory = Join-Path $outputDirectory 'units'
$webOutputDirectory = Join-Path $outputDirectory 'www'
$stagedJavaScript = Join-Path $webOutputDirectory 'BrowserMappedWorld.js'
foreach ($fileName in @('BrowserMappedWorld.lpr','browser_mapped_world_app.pas',
    'mapped_world_types.pas','mapped_world_workbench.pas',
    'mapped_world_validation.pas','mapped_world_svg.pas','index.html','mappedworld.css')) {
  $requiredFile = Join-Path $browserDirectory $fileName
  if (-not (Test-Path -LiteralPath $requiredFile -PathType Leaf)) {
    throw "Required Mapped World file was not found: $requiredFile"
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
  (Join-Path $browserDirectory 'BrowserMappedWorld.lpr')
)
Write-Host "Building Mapped World with '$Compiler'."
& $Compiler @compilerArguments
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
if (-not (Test-Path -LiteralPath $stagedJavaScript -PathType Leaf) -or
    (Get-Item -LiteralPath $stagedJavaScript).Length -eq 0) {
  throw "pas2js did not produce $stagedJavaScript"
}
foreach ($assetName in @('index.html','mappedworld.css')) {
  Copy-Item -LiteralPath (Join-Path $browserDirectory $assetName) -Destination $webOutputDirectory -Force
  if ((Get-Item -LiteralPath (Join-Path $webOutputDirectory $assetName)).Length -eq 0) {
    throw "Staged Mapped World asset is empty: $assetName"
  }
}
Write-Host "Mapped World staged in '$webOutputDirectory'. Host it using the included FPC wfc_serve."
