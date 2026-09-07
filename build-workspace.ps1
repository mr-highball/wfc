# SPDX-License-Identifier: MIT
[CmdletBinding()]
param(
  [string] $Compiler = $(if ($env:FPC) { $env:FPC } else { 'fpc' }),
  [string[]] $CompilerOptions = @()
)
$ErrorActionPreference = 'Stop'
if (Get-Variable PSNativeCommandUseErrorActionPreference -ErrorAction SilentlyContinue) {
  $PSNativeCommandUseErrorActionPreference = $false
}
$repositoryRoot = $PSScriptRoot
$exampleDirectory = Join-Path $repositoryRoot 'examples/passes/08_PipelineWorkspace'
$outputDirectory = Join-Path $repositoryRoot 'build/workspace/native'
$binaryDirectory = Join-Path $outputDirectory 'bin'
$executableSuffix = if ($env:OS -eq 'Windows_NT') { '.exe' } else { '' }
$programs = @(
  [pscustomobject]@{ Name='PipelineWorkspace'; Source='examples/passes/08_PipelineWorkspace/PipelineWorkspace.lpr'; Smoke='--help' }
  [pscustomobject]@{ Name='wfc_workspace'; Source='tools/wfc_workspace_cli.lpr'; Smoke='--help' }
  [pscustomobject]@{ Name='wfc_serve'; Source='tools/wfc_serve.lpr'; Smoke='--version' }
)
foreach ($program in $programs) {
  $source = Join-Path $repositoryRoot $program.Source
  if (-not (Test-Path -LiteralPath $source -PathType Leaf)) { throw "Required Pascal source not found: $source" }
}
foreach ($name in @('pipeline_workspace_workbench.pas','pipeline_workspace_presets.pas','pipeline_workspace_view.pas')) {
  if (-not (Test-Path -LiteralPath (Join-Path $exampleDirectory $name) -PathType Leaf)) { throw "Required workspace unit not found: $name" }
}
New-Item -ItemType Directory -Force -Path $binaryDirectory | Out-Null
foreach ($program in $programs) {
  $units = Join-Path $outputDirectory ('units/' + $program.Name)
  New-Item -ItemType Directory -Force -Path $units | Out-Null
  $executable = Join-Path $binaryDirectory ($program.Name + $executableSuffix)
  # Only the explicitly named generated executable is invalidated on rebuild.
  Remove-Item -LiteralPath $executable -Force -ErrorAction SilentlyContinue
  $arguments = @($CompilerOptions) + @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl',
    "-Fu$repositoryRoot/src", "-Fu$repositoryRoot/tools", "-Fu$exampleDirectory",
    "-FU$units", "-FE$binaryDirectory", "-o$executable",
    (Join-Path $repositoryRoot $program.Source))
  Write-Host "Building $($program.Name) with '$Compiler'."
  & $Compiler @arguments
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
  if (-not (Test-Path -LiteralPath $executable -PathType Leaf) -or (Get-Item -LiteralPath $executable).Length -eq 0) {
    throw "FPC did not produce $executable"
  }
  & $executable $program.Smoke
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}
Write-Host "Native workspace demo and included tools are ready in '$binaryDirectory'."
Write-Host 'No server was started. Run PipelineWorkspace --help for generation, or wfc_serve --help for hosting.'
