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
$worldTestSource = Join-Path $repositoryRoot 'test/wfc_world2d_test.lpr'
$exampleSource = Join-Path $repositoryRoot `
  'examples/text/01_SimpleTiledWorld/SimpleTiledWorld.lpr'
$worldExampleSource = Join-Path $repositoryRoot `
  'examples/2D/01_MultiPassWorld/MultiPassWorld.lpr'
$worldCommonDirectory = Join-Path $repositoryRoot 'examples/2D/common'
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
if ($testExitCode -ne 0) {
  exit $testExitCode
}

$worldTestCompilerArguments = @(
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
  $worldTestSource
)

Write-Host 'Building the 2D ecosystem conformance suite.'
& $Compiler @worldTestCompilerArguments
$worldTestCompilerExitCode = $LASTEXITCODE
if ($worldTestCompilerExitCode -ne 0) {
  exit $worldTestCompilerExitCode
}

$worldTestExecutableName = if ($env:OS -eq 'Windows_NT') {
  'wfc_world2d_test.exe'
} else {
  'wfc_world2d_test'
}
$worldTestExecutable = Join-Path $binaryOutputDirectory `
  $worldTestExecutableName

Write-Host "Running '$worldTestExecutable'."
& $worldTestExecutable
$worldTestExitCode = $LASTEXITCODE
if ($worldTestExitCode -ne 0) {
  exit $worldTestExitCode
}

$exampleCompilerArguments = @(
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
  $exampleSource
)

Write-Host "Building the dependency-free tiled-world example."
& $Compiler @exampleCompilerArguments
$exampleCompilerExitCode = $LASTEXITCODE
if ($exampleCompilerExitCode -ne 0) {
  exit $exampleCompilerExitCode
}

$exampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'SimpleTiledWorld.exe'
} else {
  'SimpleTiledWorld'
}
$exampleExecutable = Join-Path $binaryOutputDirectory $exampleExecutableName

Write-Host "Smoke testing '$exampleExecutable' with seed 0."
& $exampleExecutable 0 | Out-Null
$exampleExitCode = $LASTEXITCODE
if ($exampleExitCode -ne 0) {
  exit $exampleExitCode
}

$worldExampleCompilerArguments = @(
  $CompilerOptions
  '-B'
  '-Mdelphi'
  '-Sa'
  '-Cr'
  '-Co'
  '-Ci'
  "-Fu$sourceDirectory"
  "-Fu$worldCommonDirectory"
  "-FU$unitOutputDirectory"
  "-FE$binaryOutputDirectory"
  $worldExampleSource
)

Write-Host 'Building the portable multi-pass 2D example.'
& $Compiler @worldExampleCompilerArguments
$worldExampleCompilerExitCode = $LASTEXITCODE
if ($worldExampleCompilerExitCode -ne 0) {
  exit $worldExampleCompilerExitCode
}

$worldExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'MultiPassWorld.exe'
} else {
  'MultiPassWorld'
}
$worldExampleExecutable = Join-Path $binaryOutputDirectory `
  $worldExampleExecutableName

Write-Host "Smoke testing '$worldExampleExecutable' with seed 0."
& $worldExampleExecutable 0 | Out-Null
$worldExampleSeedZeroExitCode = $LASTEXITCODE
if ($worldExampleSeedZeroExitCode -ne 0) {
  exit $worldExampleSeedZeroExitCode
}

Write-Host "Smoke testing '$worldExampleExecutable' with its default seed."
& $worldExampleExecutable | Out-Null
exit $LASTEXITCODE
