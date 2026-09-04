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
$settlementTestSource = Join-Path $repositoryRoot `
  'test/wfc_world2d_settlement_test.lpr'
$learningTestSource = Join-Path $repositoryRoot 'test/wfc_learn_test.lpr'
$patternTestSource = Join-Path $repositoryRoot 'test/wfc_pattern2d_test.lpr'
$sequenceTestSource = Join-Path $repositoryRoot 'test/wfc_sequence_test.lpr'
$musicTestSources = @(
  (Join-Path $repositoryRoot 'test/wfc_midi_smf_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_graph_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_midi_test.lpr')
)
$exampleSource = Join-Path $repositoryRoot `
  'examples/text/01_SimpleTiledWorld/SimpleTiledWorld.lpr'
$worldExampleSource = Join-Path $repositoryRoot `
  'examples/2D/01_MultiPassWorld/MultiPassWorld.lpr'
$settlementExampleSource = Join-Path $repositoryRoot `
  'examples/2D/03_SelectiveSettlement/SelectiveSettlement.lpr'
$learningExampleSource = Join-Path $repositoryRoot `
  'examples/learning/01_LearnTiles/LearnTiles.lpr'
$corpusExampleSource = Join-Path $repositoryRoot `
  'examples/learning/02_LearnCorpus/LearnCorpus.lpr'
$patternExampleSource = Join-Path $repositoryRoot `
  'examples/learning/03_LearnPatterns/LearnPatterns.lpr'
$sequenceExampleSource = Join-Path $repositoryRoot `
  'examples/sequence/01_LearnSequence/LearnSequence.lpr'
$musicExampleSource = Join-Path $repositoryRoot `
  'examples/music/03_PassComposition/PassComposition.lpr'
$spatialExampleSource = Join-Path $repositoryRoot `
  'examples/passes/01_SpatialDependencies/SpatialDependencies.lpr'
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

$settlementTestCompilerArguments = @(
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
  $settlementTestSource
)

Write-Host 'Building the selective-settlement conformance suite.'
& $Compiler @settlementTestCompilerArguments
$settlementTestCompilerExitCode = $LASTEXITCODE
if ($settlementTestCompilerExitCode -ne 0) {
  exit $settlementTestCompilerExitCode
}

$settlementTestExecutableName = if ($env:OS -eq 'Windows_NT') {
  'wfc_world2d_settlement_test.exe'
} else {
  'wfc_world2d_settlement_test'
}
$settlementTestExecutable = Join-Path $binaryOutputDirectory `
  $settlementTestExecutableName

Write-Host "Running '$settlementTestExecutable'."
& $settlementTestExecutable
$settlementTestExitCode = $LASTEXITCODE
if ($settlementTestExitCode -ne 0) {
  exit $settlementTestExitCode
}

$learningTestCompilerArguments = @(
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
  $learningTestSource
)

Write-Host 'Building the model-learning conformance suite.'
& $Compiler @learningTestCompilerArguments
$learningTestCompilerExitCode = $LASTEXITCODE
if ($learningTestCompilerExitCode -ne 0) {
  exit $learningTestCompilerExitCode
}

$learningTestExecutableName = if ($env:OS -eq 'Windows_NT') {
  'wfc_learn_test.exe'
} else {
  'wfc_learn_test'
}
$learningTestExecutable = Join-Path $binaryOutputDirectory `
  $learningTestExecutableName

Write-Host "Running '$learningTestExecutable'."
& $learningTestExecutable
$learningTestExitCode = $LASTEXITCODE
if ($learningTestExitCode -ne 0) {
  exit $learningTestExitCode
}

$patternTestCompilerArguments = @(
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
  $patternTestSource
)

Write-Host 'Building the overlapping-pattern conformance suite.'
& $Compiler @patternTestCompilerArguments
$patternTestCompilerExitCode = $LASTEXITCODE
if ($patternTestCompilerExitCode -ne 0) {
  exit $patternTestCompilerExitCode
}

$patternTestExecutableName = if ($env:OS -eq 'Windows_NT') {
  'wfc_pattern2d_test.exe'
} else {
  'wfc_pattern2d_test'
}
$patternTestExecutable = Join-Path $binaryOutputDirectory `
  $patternTestExecutableName

Write-Host "Running '$patternTestExecutable'."
& $patternTestExecutable
$patternTestExitCode = $LASTEXITCODE
if ($patternTestExitCode -ne 0) {
  exit $patternTestExitCode
}

$sequenceTestCompilerArguments = @(
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
  $sequenceTestSource
)

Write-Host 'Building the sequence-foundation conformance suite.'
& $Compiler @sequenceTestCompilerArguments
$sequenceTestCompilerExitCode = $LASTEXITCODE
if ($sequenceTestCompilerExitCode -ne 0) {
  exit $sequenceTestCompilerExitCode
}

$sequenceTestExecutableName = if ($env:OS -eq 'Windows_NT') {
  'wfc_sequence_test.exe'
} else {
  'wfc_sequence_test'
}
$sequenceTestExecutable = Join-Path $binaryOutputDirectory `
  $sequenceTestExecutableName

Write-Host "Running '$sequenceTestExecutable'."
& $sequenceTestExecutable
$sequenceTestExitCode = $LASTEXITCODE
if ($sequenceTestExitCode -ne 0) {
  exit $sequenceTestExitCode
}

foreach ($musicTestSource in $musicTestSources) {
  $musicTestName = [System.IO.Path]::GetFileNameWithoutExtension(
    $musicTestSource)
  $musicTestCompilerArguments = @(
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
    $musicTestSource
  )

  Write-Host "Building the music conformance suite '$musicTestName'."
  & $Compiler @musicTestCompilerArguments
  $musicTestCompilerExitCode = $LASTEXITCODE
  if ($musicTestCompilerExitCode -ne 0) {
    exit $musicTestCompilerExitCode
  }

  $musicTestExecutableName = if ($env:OS -eq 'Windows_NT') {
    "$musicTestName.exe"
  } else {
    $musicTestName
  }
  $musicTestExecutable = Join-Path $binaryOutputDirectory `
    $musicTestExecutableName
  Write-Host "Running '$musicTestExecutable'."
  & $musicTestExecutable
  $musicTestExitCode = $LASTEXITCODE
  if ($musicTestExitCode -ne 0) {
    exit $musicTestExitCode
  }
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
$worldExampleDefaultExitCode = $LASTEXITCODE
if ($worldExampleDefaultExitCode -ne 0) {
  exit $worldExampleDefaultExitCode
}

$settlementExampleCompilerArguments = @(
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
  $settlementExampleSource
)

Write-Host 'Building the portable selective-settlement example.'
& $Compiler @settlementExampleCompilerArguments
$settlementExampleCompilerExitCode = $LASTEXITCODE
if ($settlementExampleCompilerExitCode -ne 0) {
  exit $settlementExampleCompilerExitCode
}

$settlementExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'SelectiveSettlement.exe'
} else {
  'SelectiveSettlement'
}
$settlementExampleExecutable = Join-Path $binaryOutputDirectory `
  $settlementExampleExecutableName

Write-Host "Smoke testing '$settlementExampleExecutable' with seed 0."
& $settlementExampleExecutable 0 | Out-Null
$settlementExampleSeedZeroExitCode = $LASTEXITCODE
if ($settlementExampleSeedZeroExitCode -ne 0) {
  exit $settlementExampleSeedZeroExitCode
}

Write-Host "Smoke testing '$settlementExampleExecutable' with its default seed."
& $settlementExampleExecutable | Out-Null
$settlementExampleDefaultExitCode = $LASTEXITCODE
if ($settlementExampleDefaultExitCode -ne 0) {
  exit $settlementExampleDefaultExitCode
}

$learningExampleCompilerArguments = @(
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
  $learningExampleSource
)

Write-Host 'Building the portable learned-tiles example.'
& $Compiler @learningExampleCompilerArguments
$learningExampleCompilerExitCode = $LASTEXITCODE
if ($learningExampleCompilerExitCode -ne 0) {
  exit $learningExampleCompilerExitCode
}

$learningExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'LearnTiles.exe'
} else {
  'LearnTiles'
}
$learningExampleExecutable = Join-Path $binaryOutputDirectory `
  $learningExampleExecutableName

Write-Host "Smoke testing '$learningExampleExecutable' with seed 0."
& $learningExampleExecutable 0 | Out-Null
$learningExampleExitCode = $LASTEXITCODE
if ($learningExampleExitCode -ne 0) {
  exit $learningExampleExitCode
}

$corpusExampleCompilerArguments = @(
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
  $corpusExampleSource
)

Write-Host 'Building the portable learned-corpus example.'
& $Compiler @corpusExampleCompilerArguments
$corpusExampleCompilerExitCode = $LASTEXITCODE
if ($corpusExampleCompilerExitCode -ne 0) {
  exit $corpusExampleCompilerExitCode
}

$corpusExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'LearnCorpus.exe'
} else {
  'LearnCorpus'
}
$corpusExampleExecutable = Join-Path $binaryOutputDirectory `
  $corpusExampleExecutableName

Write-Host "Smoke testing '$corpusExampleExecutable' with seed 0."
& $corpusExampleExecutable 0 | Out-Null
$corpusExampleExitCode = $LASTEXITCODE
if ($corpusExampleExitCode -ne 0) {
  exit $corpusExampleExitCode
}

$patternExampleCompilerArguments = @(
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
  $patternExampleSource
)

Write-Host 'Building the portable overlapping-pattern example.'
& $Compiler @patternExampleCompilerArguments
$patternExampleCompilerExitCode = $LASTEXITCODE
if ($patternExampleCompilerExitCode -ne 0) {
  exit $patternExampleCompilerExitCode
}

$patternExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'LearnPatterns.exe'
} else {
  'LearnPatterns'
}
$patternExampleExecutable = Join-Path $binaryOutputDirectory `
  $patternExampleExecutableName

Write-Host "Smoke testing '$patternExampleExecutable' with seed 0."
& $patternExampleExecutable 0 | Out-Null
$patternExampleExitCode = $LASTEXITCODE
if ($patternExampleExitCode -ne 0) {
  exit $patternExampleExitCode
}

$sequenceExampleCompilerArguments = @(
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
  $sequenceExampleSource
)

Write-Host 'Building the portable learned-sequence example.'
& $Compiler @sequenceExampleCompilerArguments
$sequenceExampleCompilerExitCode = $LASTEXITCODE
if ($sequenceExampleCompilerExitCode -ne 0) {
  exit $sequenceExampleCompilerExitCode
}

$sequenceExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'LearnSequence.exe'
} else {
  'LearnSequence'
}
$sequenceExampleExecutable = Join-Path $binaryOutputDirectory `
  $sequenceExampleExecutableName

Write-Host "Smoke testing '$sequenceExampleExecutable' with seed 0."
& $sequenceExampleExecutable 0 | Out-Null
$sequenceExampleExitCode = $LASTEXITCODE
if ($sequenceExampleExitCode -ne 0) {
  exit $sequenceExampleExitCode
}

$musicExampleCompilerArguments = @(
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
  $musicExampleSource
)

Write-Host 'Building the dependency-free pass-composed music example.'
& $Compiler @musicExampleCompilerArguments
$musicExampleCompilerExitCode = $LASTEXITCODE
if ($musicExampleCompilerExitCode -ne 0) {
  exit $musicExampleCompilerExitCode
}

$musicExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'PassComposition.exe'
} else {
  'PassComposition'
}
$musicExampleExecutable = Join-Path $binaryOutputDirectory `
  $musicExampleExecutableName

Write-Host "Smoke testing '$musicExampleExecutable' with seed 0."
& $musicExampleExecutable 0 | Out-Null
$musicExampleExitCode = $LASTEXITCODE
if ($musicExampleExitCode -ne 0) {
  exit $musicExampleExitCode
}

$spatialExampleCompilerArguments = @(
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
  $spatialExampleSource
)

Write-Host 'Building the dependency-free spatial-pass example.'
& $Compiler @spatialExampleCompilerArguments
$spatialExampleCompilerExitCode = $LASTEXITCODE
if ($spatialExampleCompilerExitCode -ne 0) {
  exit $spatialExampleCompilerExitCode
}

$spatialExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'SpatialDependencies.exe'
} else {
  'SpatialDependencies'
}
$spatialExampleExecutable = Join-Path $binaryOutputDirectory `
  $spatialExampleExecutableName

Write-Host "Smoke testing '$spatialExampleExecutable' with seed 0."
& $spatialExampleExecutable 0 | Out-Null
exit $LASTEXITCODE
