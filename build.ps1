[CmdletBinding()]
param(
  [string] $Compiler = $(if ($env:FPC) { $env:FPC } else { 'fpc' }),
  [string[]] $CompilerOptions = @()
)

$ErrorActionPreference = 'Stop'
if (Get-Variable -Name PSNativeCommandUseErrorActionPreference `
    -ErrorAction SilentlyContinue) {
  $PSNativeCommandUseErrorActionPreference = $false
}

$repositoryRoot = $PSScriptRoot
$sourceDirectory = Join-Path $repositoryRoot 'src'
$testSource = Join-Path $repositoryRoot 'test/wfc_test.lpr'
$unitOutputDirectory = Join-Path $repositoryRoot 'build/native/units'
$binaryOutputDirectory = Join-Path $repositoryRoot 'build/native/bin'

New-Item -ItemType Directory -Force -Path $unitOutputDirectory | Out-Null
New-Item -ItemType Directory -Force -Path $binaryOutputDirectory | Out-Null

$compilerArguments = @(
  $CompilerOptions
  '-B'
  '-Mdelphi'
  '-Sa'
  '-Cr'
  '-Co'
  '-Ci'
  "-Fu$sourceDirectory"
  "-FU$unitOutputDirectory"
  "-FE$binaryOutputDirectory"
  $testSource
)

Write-Host "Building the native conformance suite with '$Compiler'."
& $Compiler @compilerArguments
$compilerExitCode = $LASTEXITCODE
if ($compilerExitCode -ne 0) {
  exit $compilerExitCode
}

$testExecutableName = if ($env:OS -eq 'Windows_NT') {
  'wfc_test.exe'
} else {
  'wfc_test'
}
$testExecutable = Join-Path $binaryOutputDirectory $testExecutableName

Write-Host "Running '$testExecutable'."
& $testExecutable
$testExitCode = $LASTEXITCODE
exit $testExitCode
