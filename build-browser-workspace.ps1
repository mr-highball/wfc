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
$browserDirectory = Join-Path $repositoryRoot 'examples/passes/08_PipelineWorkspace'
$outputDirectory = Join-Path $repositoryRoot 'build/browser/workspace'
$unitOutputDirectory = Join-Path $outputDirectory 'units'
$webOutputDirectory = Join-Path $outputDirectory 'www'
$stagedJavaScript = Join-Path $webOutputDirectory 'BrowserPipelineWorkspace.js'
foreach ($fileName in @('BrowserPipelineWorkspace.lpr','browser_pipeline_workspace_app.pas',
    'pipeline_workspace_workbench.pas','pipeline_workspace_presets.pas',
    'pipeline_workspace_view.pas','index.html','workspace.css')) {
  $requiredFile = Join-Path $browserDirectory $fileName
  if (-not (Test-Path -LiteralPath $requiredFile -PathType Leaf)) {
    throw "Required Pipeline Workspace file was not found: $requiredFile"
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
  (Join-Path $browserDirectory 'BrowserPipelineWorkspace.lpr')
)
Write-Host "Building Pipeline Workspace with '$Compiler'."
& $Compiler @compilerArguments
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
if (-not (Test-Path -LiteralPath $stagedJavaScript -PathType Leaf) -or
    (Get-Item -LiteralPath $stagedJavaScript).Length -eq 0) {
  throw "pas2js did not produce $stagedJavaScript"
}
foreach ($assetName in @('index.html','workspace.css')) {
  Copy-Item -LiteralPath (Join-Path $browserDirectory $assetName) -Destination $webOutputDirectory -Force
  if ((Get-Item -LiteralPath (Join-Path $webOutputDirectory $assetName)).Length -eq 0) {
    throw "Staged Pipeline Workspace asset is empty: $assetName"
  }
}
Write-Host "Pipeline Workspace staged in '$webOutputDirectory'. Host it using the included FPC wfc_serve."
