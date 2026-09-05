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
$toolsDirectory = Join-Path $repositoryRoot 'tools'
$testSource = Join-Path $repositoryRoot 'test/wfc_test.lpr'
$worldTestSource = Join-Path $repositoryRoot 'test/wfc_world2d_test.lpr'
$settlementTestSource = Join-Path $repositoryRoot `
  'test/wfc_world2d_settlement_test.lpr'
$learningTestSource = Join-Path $repositoryRoot 'test/wfc_learn_test.lpr'
$patternTestSource = Join-Path $repositoryRoot 'test/wfc_pattern2d_test.lpr'
$patternPassTestSource = Join-Path $repositoryRoot `
  'test/wfc_pattern2d_passes_test.lpr'
$sequenceTestSource = Join-Path $repositoryRoot 'test/wfc_sequence_test.lpr'
$textTestSource = Join-Path $repositoryRoot 'test/wfc_text_test.lpr'
$textPassTestSource = Join-Path $repositoryRoot `
  'test/wfc_text_passes_test.lpr'
$negotiationTestSource = Join-Path $repositoryRoot `
  'test/wfc_negotiation_test.lpr'
$selectiveNegotiationTestSource = Join-Path $repositoryRoot `
  'test/wfc_selective_negotiation_test.lpr'
$voxelTestSource = Join-Path $repositoryRoot 'test/wfc_voxel3d_test.lpr'
$buildingTestSource = Join-Path $repositoryRoot 'test/wfc_building3d_test.lpr'
$traceTestSources = @(
  (Join-Path $repositoryRoot 'test/wfc_trace_reference_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_trace_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_trace_utility_test.lpr')
)
$viewerTestSources = @(
  (Join-Path $repositoryRoot 'test/wfc_voxel3d_isometric_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_voxel3d_svg_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_building3d_view_test.lpr')
)
$musicTestSources = @(
  (Join-Path $repositoryRoot 'test/wfc_midi_smf_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_graph_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_midi_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_midi_import_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_training_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_arrangement_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_audio_stream_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_studio_arrangement_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_passes_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_passes_text_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_audio_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_studio_test.lpr')
)
$artifactTestSources = @(
  (Join-Path $repositoryRoot 'test/wfc_text_codec_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_rule_model_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_rule_text_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_pipeline_model_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_pipeline_text_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_token_lookup_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_pipeline_compile_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_pipeline_run_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_pipeline_run_text_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_pipeline_result_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_pipeline_result_text_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_pipeline_runtime_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_validate_app_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_browser_dom_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_serve_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_run_app_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_training_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_text_training_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_training_workspace_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_training_text_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_learn_app_test.lpr')
  (Join-Path $repositoryRoot 'test/wfc_music_import_app_test.lpr')
  (Join-Path $repositoryRoot `
    'test/wfc_learned_pattern_world_bundle_test.lpr')
)
$toolSources = @(
  (Join-Path $repositoryRoot 'tools/wfc_validate.lpr')
  (Join-Path $repositoryRoot 'tools/wfc_run.lpr')
  (Join-Path $repositoryRoot 'tools/wfc_learn_cli.lpr')
  (Join-Path $repositoryRoot 'tools/wfc_music_import_cli.lpr')
  (Join-Path $repositoryRoot 'tools/wfc_serve.lpr')
  (Join-Path $repositoryRoot 'tools/wfc_browser_check.lpr')
)
$pipelineCliProcessTestSource = Join-Path $repositoryRoot `
  'test/wfc_pipeline_cli_process_test.ps1'
$exampleSource = Join-Path $repositoryRoot `
  'examples/text/01_SimpleTiledWorld/SimpleTiledWorld.lpr'
$worldExampleSource = Join-Path $repositoryRoot `
  'examples/2D/01_MultiPassWorld/MultiPassWorld.lpr'
$settlementExampleSource = Join-Path $repositoryRoot `
  'examples/2D/03_SelectiveSettlement/SelectiveSettlement.lpr'
$negotiatedRepairExampleSource = Join-Path $repositoryRoot `
  'examples/2D/04_NegotiatedRepair/NegotiatedRepair.lpr'
$negotiatedRepairExampleDirectory = Join-Path $repositoryRoot `
  'examples/2D/04_NegotiatedRepair'
$learnedPatternWorldExampleSource = Join-Path $repositoryRoot `
  'examples/2D/05_LearnedPatternWorld/LearnedPatternWorld.lpr'
$learnedPatternWorldExampleDirectory = Join-Path $repositoryRoot `
  'examples/2D/05_LearnedPatternWorld'
$learningExampleSource = Join-Path $repositoryRoot `
  'examples/learning/01_LearnTiles/LearnTiles.lpr'
$corpusExampleSource = Join-Path $repositoryRoot `
  'examples/learning/02_LearnCorpus/LearnCorpus.lpr'
$patternExampleSource = Join-Path $repositoryRoot `
  'examples/learning/03_LearnPatterns/LearnPatterns.lpr'
$sequenceExampleSource = Join-Path $repositoryRoot `
  'examples/sequence/01_LearnSequence/LearnSequence.lpr'
$textCompletionExampleSource = Join-Path $repositoryRoot `
  'examples/text/02_ConstraintCompletion/ConstraintCompletion.lpr'
$textCompletionExampleDirectory = Join-Path $repositoryRoot `
  'examples/text/02_ConstraintCompletion'
$textPassExampleSource = Join-Path $repositoryRoot `
  'examples/text/03_PassComposition/TextPassComposition.lpr'
$textPassExampleDirectory = Join-Path $repositoryRoot `
  'examples/text/03_PassComposition'
$musicExampleSource = Join-Path $repositoryRoot `
  'examples/music/03_PassComposition/PassComposition.lpr'
$musicVariationExampleSource = Join-Path $repositoryRoot `
  'examples/music/04_NegotiatedVariation/NegotiatedVariation.lpr'
$musicVariationExampleDirectory = Join-Path $repositoryRoot `
  'examples/music/04_NegotiatedVariation'
$spatialExampleSource = Join-Path $repositoryRoot `
  'examples/passes/01_SpatialDependencies/SpatialDependencies.lpr'
$traceExampleSource = Join-Path $repositoryRoot `
  'examples/passes/02_TraceInspector/TraceInspector.lpr'
$traceExampleDirectory = Join-Path $repositoryRoot `
  'examples/passes/02_TraceInspector'
$negotiationExampleSource = Join-Path $repositoryRoot `
  'examples/passes/03_PassNegotiation/PassNegotiation.lpr'
$negotiationExampleDirectory = Join-Path $repositoryRoot `
  'examples/passes/03_PassNegotiation'
$buildingExampleSource = Join-Path $repositoryRoot `
  'examples/3D/02_MultiPassBuilding/MultiPassBuilding.lpr'
$buildingExampleDirectory = Join-Path $repositoryRoot `
  'examples/3D/02_MultiPassBuilding'
$buildingCommonDirectory = Join-Path $repositoryRoot 'examples/3D/common'
$buildingSvgSource = Join-Path $repositoryRoot `
  'examples/3D/03_BrowserBuilding/Building3DSvg.lpr'
$worldCommonDirectory = Join-Path $repositoryRoot 'examples/2D/common'
$trainingStudioExampleDirectory = Join-Path $repositoryRoot `
  'examples/learning/05_TrainingStudio'
$musicStudioExampleDirectory = Join-Path $repositoryRoot `
  'examples/music/05_MusicStudio'
$musicStudioFormFixture = Join-Path $repositoryRoot `
  'docs/research/music-studio-form-v1.csv'
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

$patternPassTestCompilerArguments = @(
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
  $patternPassTestSource
)

Write-Host 'Building the pattern-projected-pass conformance suite.'
& $Compiler @patternPassTestCompilerArguments
$patternPassTestCompilerExitCode = $LASTEXITCODE
if ($patternPassTestCompilerExitCode -ne 0) {
  exit $patternPassTestCompilerExitCode
}

$patternPassTestExecutableName = if ($env:OS -eq 'Windows_NT') {
  'wfc_pattern2d_passes_test.exe'
} else {
  'wfc_pattern2d_passes_test'
}
$patternPassTestExecutable = Join-Path $binaryOutputDirectory `
  $patternPassTestExecutableName

Write-Host "Running '$patternPassTestExecutable'."
& $patternPassTestExecutable
$patternPassTestExitCode = $LASTEXITCODE
if ($patternPassTestExitCode -ne 0) {
  exit $patternPassTestExitCode
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

$textTestCompilerArguments = @(
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
  $textTestSource
)

Write-Host 'Building the text-completion conformance suite.'
& $Compiler @textTestCompilerArguments
$textTestCompilerExitCode = $LASTEXITCODE
if ($textTestCompilerExitCode -ne 0) {
  exit $textTestCompilerExitCode
}

$textTestExecutableName = if ($env:OS -eq 'Windows_NT') {
  'wfc_text_test.exe'
} else {
  'wfc_text_test'
}
$textTestExecutable = Join-Path $binaryOutputDirectory `
  $textTestExecutableName

Write-Host "Running '$textTestExecutable'."
& $textTestExecutable
$textTestExitCode = $LASTEXITCODE
if ($textTestExitCode -ne 0) {
  exit $textTestExitCode
}

$textPassTestCompilerArguments = @(
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
  $textPassTestSource
)

Write-Host 'Building the multi-pass text conformance suite.'
& $Compiler @textPassTestCompilerArguments
$textPassTestCompilerExitCode = $LASTEXITCODE
if ($textPassTestCompilerExitCode -ne 0) {
  exit $textPassTestCompilerExitCode
}

$textPassTestExecutableName = if ($env:OS -eq 'Windows_NT') {
  'wfc_text_passes_test.exe'
} else {
  'wfc_text_passes_test'
}
$textPassTestExecutable = Join-Path $binaryOutputDirectory `
  $textPassTestExecutableName

Write-Host "Running '$textPassTestExecutable'."
& $textPassTestExecutable
$textPassTestExitCode = $LASTEXITCODE
if ($textPassTestExitCode -ne 0) {
  exit $textPassTestExitCode
}

$negotiationTestCompilerArguments = @(
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
  $negotiationTestSource
)

Write-Host 'Building the bounded pass-negotiation conformance suite.'
& $Compiler @negotiationTestCompilerArguments
$negotiationTestCompilerExitCode = $LASTEXITCODE
if ($negotiationTestCompilerExitCode -ne 0) {
  exit $negotiationTestCompilerExitCode
}

$negotiationTestExecutableName = if ($env:OS -eq 'Windows_NT') {
  'wfc_negotiation_test.exe'
} else {
  'wfc_negotiation_test'
}
$negotiationTestExecutable = Join-Path $binaryOutputDirectory `
  $negotiationTestExecutableName

Write-Host "Running '$negotiationTestExecutable'."
& $negotiationTestExecutable
$negotiationTestExitCode = $LASTEXITCODE
if ($negotiationTestExitCode -ne 0) {
  exit $negotiationTestExitCode
}

$selectiveNegotiationTestCompilerArguments = @(
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
  $selectiveNegotiationTestSource
)

Write-Host 'Building the selective pass-negotiation conformance suite.'
& $Compiler @selectiveNegotiationTestCompilerArguments
$selectiveNegotiationTestCompilerExitCode = $LASTEXITCODE
if ($selectiveNegotiationTestCompilerExitCode -ne 0) {
  exit $selectiveNegotiationTestCompilerExitCode
}

$selectiveNegotiationTestExecutableName = if ($env:OS -eq 'Windows_NT') {
  'wfc_selective_negotiation_test.exe'
} else {
  'wfc_selective_negotiation_test'
}
$selectiveNegotiationTestExecutable = Join-Path $binaryOutputDirectory `
  $selectiveNegotiationTestExecutableName

Write-Host "Running '$selectiveNegotiationTestExecutable'."
& $selectiveNegotiationTestExecutable
$selectiveNegotiationTestExitCode = $LASTEXITCODE
if ($selectiveNegotiationTestExitCode -ne 0) {
  exit $selectiveNegotiationTestExitCode
}

$voxelTestCompilerArguments = @(
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
  $voxelTestSource
)

Write-Host 'Building the voxel-3D foundation conformance suite.'
& $Compiler @voxelTestCompilerArguments
$voxelTestCompilerExitCode = $LASTEXITCODE
if ($voxelTestCompilerExitCode -ne 0) {
  exit $voxelTestCompilerExitCode
}

$voxelTestExecutableName = if ($env:OS -eq 'Windows_NT') {
  'wfc_voxel3d_test.exe'
} else {
  'wfc_voxel3d_test'
}
$voxelTestExecutable = Join-Path $binaryOutputDirectory `
  $voxelTestExecutableName

Write-Host "Running '$voxelTestExecutable'."
& $voxelTestExecutable
$voxelTestExitCode = $LASTEXITCODE
if ($voxelTestExitCode -ne 0) {
  exit $voxelTestExitCode
}

$buildingTestCompilerArguments = @(
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
  $buildingTestSource
)

Write-Host 'Building the multi-pass Building 3D conformance suite.'
& $Compiler @buildingTestCompilerArguments
$buildingTestCompilerExitCode = $LASTEXITCODE
if ($buildingTestCompilerExitCode -ne 0) {
  exit $buildingTestCompilerExitCode
}

$buildingTestExecutableName = if ($env:OS -eq 'Windows_NT') {
  'wfc_building3d_test.exe'
} else {
  'wfc_building3d_test'
}
$buildingTestExecutable = Join-Path $binaryOutputDirectory `
  $buildingTestExecutableName

Write-Host "Running '$buildingTestExecutable'."
& $buildingTestExecutable
$buildingTestExitCode = $LASTEXITCODE
if ($buildingTestExitCode -ne 0) {
  exit $buildingTestExitCode
}

foreach ($traceTestSource in $traceTestSources) {
  $traceTestName = [System.IO.Path]::GetFileNameWithoutExtension(
    $traceTestSource)
  $traceTestCompilerArguments = @(
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
    $traceTestSource
  )

  Write-Host "Building the causal-trace conformance suite '$traceTestName'."
  & $Compiler @traceTestCompilerArguments
  $traceTestCompilerExitCode = $LASTEXITCODE
  if ($traceTestCompilerExitCode -ne 0) {
    exit $traceTestCompilerExitCode
  }

  $traceTestExecutableName = if ($env:OS -eq 'Windows_NT') {
    "$traceTestName.exe"
  } else {
    $traceTestName
  }
  $traceTestExecutable = Join-Path $binaryOutputDirectory `
    $traceTestExecutableName
  Write-Host "Running '$traceTestExecutable'."
  & $traceTestExecutable
  $traceTestExitCode = $LASTEXITCODE
  if ($traceTestExitCode -ne 0) {
    exit $traceTestExitCode
  }
}

foreach ($viewerTestSource in $viewerTestSources) {
  $viewerTestName = [System.IO.Path]::GetFileNameWithoutExtension(
    $viewerTestSource)
  $viewerTestCompilerArguments = @(
    $CompilerOptions
    '-B'
    '-Mdelphi'
    '-Sa'
    '-Cr'
    '-Co'
    '-Ci'
    "-Fu$sourceDirectory"
    "-Fu$buildingCommonDirectory"
    "-FU$unitOutputDirectory"
    "-FE$binaryOutputDirectory"
    $viewerTestSource
  )

  Write-Host "Building the 3D presentation conformance suite '$viewerTestName'."
  & $Compiler @viewerTestCompilerArguments
  $viewerTestCompilerExitCode = $LASTEXITCODE
  if ($viewerTestCompilerExitCode -ne 0) {
    exit $viewerTestCompilerExitCode
  }

  $viewerTestExecutableName = if ($env:OS -eq 'Windows_NT') {
    "$viewerTestName.exe"
  } else {
    $viewerTestName
  }
  $viewerTestExecutable = Join-Path $binaryOutputDirectory `
    $viewerTestExecutableName
  Write-Host "Running '$viewerTestExecutable'."
  & $viewerTestExecutable
  $viewerTestExitCode = $LASTEXITCODE
  if ($viewerTestExitCode -ne 0) {
    exit $viewerTestExitCode
  }
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
    "-Fu$musicStudioExampleDirectory"
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

foreach ($artifactTestSource in $artifactTestSources) {
  $artifactTestName = [System.IO.Path]::GetFileNameWithoutExtension(
    $artifactTestSource)
  $artifactTestCompilerArguments = @(
    $CompilerOptions
    '-B'
    '-Mdelphi'
    '-Sa'
    '-Cr'
    '-Co'
    '-Ci'
    "-Fu$sourceDirectory"
    "-Fu$toolsDirectory"
    "-Fu$learnedPatternWorldExampleDirectory"
    "-Fu$trainingStudioExampleDirectory"
    "-FU$unitOutputDirectory"
    "-FE$binaryOutputDirectory"
    $artifactTestSource
  )

  Write-Host "Building the portable-artifact suite '$artifactTestName'."
  & $Compiler @artifactTestCompilerArguments
  $artifactTestCompilerExitCode = $LASTEXITCODE
  if ($artifactTestCompilerExitCode -ne 0) {
    exit $artifactTestCompilerExitCode
  }

  $artifactTestExecutableName = if ($env:OS -eq 'Windows_NT') {
    "$artifactTestName.exe"
  } else {
    $artifactTestName
  }
  $artifactTestExecutable = Join-Path $binaryOutputDirectory `
    $artifactTestExecutableName
  Write-Host "Running '$artifactTestExecutable'."
  if ($artifactTestName -eq 'wfc_learned_pattern_world_bundle_test') {
    & $artifactTestExecutable `
      (Join-Path $learnedPatternWorldExampleDirectory 'pipeline')
  } elseif ($artifactTestName -eq 'wfc_training_text_test') {
    & $artifactTestExecutable (Join-Path $repositoryRoot 'examples/learning/04_TrainingDocuments')
  } else {
    & $artifactTestExecutable
  }
  $artifactTestExitCode = $LASTEXITCODE
  if ($artifactTestExitCode -ne 0) {
    exit $artifactTestExitCode
  }
}

foreach ($toolSource in $toolSources) {
  $toolName = [System.IO.Path]::GetFileNameWithoutExtension($toolSource)
  if ($toolName -eq 'wfc_music_import_cli') { $toolName = 'wfc_music_import' }
  if ($toolName -eq 'wfc_learn_cli') {
    $toolName = 'wfc_learn'
  }
  $toolExecutableName = if ($env:OS -eq 'Windows_NT') {
    "$toolName.exe"
  } else {
    $toolName
  }
  $toolCompilerArguments = @(
    $CompilerOptions
    '-B'
    '-Mdelphi'
    '-Sa'
    '-Cr'
    '-Co'
    '-Ci'
    "-Fu$sourceDirectory"
    "-Fu$toolsDirectory"
    "-FU$unitOutputDirectory"
    "-FE$binaryOutputDirectory"
    "-o$toolExecutableName"
    $toolSource
  )

  Write-Host "Building the portable command-line host '$toolName'."
  & $Compiler @toolCompilerArguments
  $toolCompilerExitCode = $LASTEXITCODE
  if ($toolCompilerExitCode -ne 0) {
    exit $toolCompilerExitCode
  }

  $toolExecutable = Join-Path $binaryOutputDirectory $toolExecutableName
  Write-Host "Smoke testing '$toolExecutable --version'."
  & $toolExecutable '--version'
  $toolExitCode = $LASTEXITCODE
  if ($toolExitCode -ne 0) {
    exit $toolExitCode
  }
}

$toolExecutableSuffix = if ($env:OS -eq 'Windows_NT') { '.exe' } else { '' }
Write-Host 'Running live FPC server conformance.'
& (Join-Path $binaryOutputDirectory "wfc_serve_test$toolExecutableSuffix") `
  --integration (Join-Path $binaryOutputDirectory "wfc_serve$toolExecutableSuffix") `
  $binaryOutputDirectory
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

$validatorToolExecutable = Join-Path $binaryOutputDirectory `
  "wfc_validate$toolExecutableSuffix"
$runnerToolExecutable = Join-Path $binaryOutputDirectory `
  "wfc_run$toolExecutableSuffix"
Write-Host 'Running the portable pipeline CLI process conformance suite.'
& $pipelineCliProcessTestSource `
  -Validator $validatorToolExecutable `
  -Runner $runnerToolExecutable

$learnerToolExecutable = Join-Path $binaryOutputDirectory `
  "wfc_learn$toolExecutableSuffix"
Write-Host 'Running the portable training CLI process conformance suite.'
& (Join-Path $repositoryRoot 'test/wfc_learn_cli_process_test.ps1') `
  -Learner $learnerToolExecutable `
  -Validator $validatorToolExecutable `
  -Runner $runnerToolExecutable

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

$negotiatedRepairExampleCompilerArguments = @(
  $CompilerOptions
  '-B'
  '-Mdelphi'
  '-Sa'
  '-Cr'
  '-Co'
  '-Ci'
  "-Fu$sourceDirectory"
  "-Fu$negotiatedRepairExampleDirectory"
  "-FU$unitOutputDirectory"
  "-FE$binaryOutputDirectory"
  $negotiatedRepairExampleSource
)

Write-Host 'Building the dependency-free negotiated-repair example.'
& $Compiler @negotiatedRepairExampleCompilerArguments
$negotiatedRepairExampleCompilerExitCode = $LASTEXITCODE
if ($negotiatedRepairExampleCompilerExitCode -ne 0) {
  exit $negotiatedRepairExampleCompilerExitCode
}

$negotiatedRepairExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'NegotiatedRepair.exe'
} else {
  'NegotiatedRepair'
}
$negotiatedRepairExampleExecutable = Join-Path $binaryOutputDirectory `
  $negotiatedRepairExampleExecutableName

Write-Host "Smoke testing '$negotiatedRepairExampleExecutable'."
& $negotiatedRepairExampleExecutable | Out-Null
$negotiatedRepairExampleExitCode = $LASTEXITCODE
if ($negotiatedRepairExampleExitCode -ne 0) {
  exit $negotiatedRepairExampleExitCode
}

$learnedPatternWorldExampleCompilerArguments = @(
  $CompilerOptions
  '-B'
  '-Mdelphi'
  '-Sa'
  '-Cr'
  '-Co'
  '-Ci'
  "-Fu$sourceDirectory"
  "-Fu$learnedPatternWorldExampleDirectory"
  "-FU$unitOutputDirectory"
  "-FE$binaryOutputDirectory"
  $learnedPatternWorldExampleSource
)

Write-Host 'Building the dependency-free learned-pattern-world example.'
& $Compiler @learnedPatternWorldExampleCompilerArguments
$learnedPatternWorldExampleCompilerExitCode = $LASTEXITCODE
if ($learnedPatternWorldExampleCompilerExitCode -ne 0) {
  exit $learnedPatternWorldExampleCompilerExitCode
}

$learnedPatternWorldExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'LearnedPatternWorld.exe'
} else {
  'LearnedPatternWorld'
}
$learnedPatternWorldExampleExecutable = Join-Path $binaryOutputDirectory `
  $learnedPatternWorldExampleExecutableName

Write-Host "Smoke testing '$learnedPatternWorldExampleExecutable' with seed 0."
& $learnedPatternWorldExampleExecutable 0 | Out-Null
$learnedPatternWorldExampleExitCode = $LASTEXITCODE
if ($learnedPatternWorldExampleExitCode -ne 0) {
  exit $learnedPatternWorldExampleExitCode
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

$textCompletionExampleCompilerArguments = @(
  $CompilerOptions
  '-B'
  '-Mdelphi'
  '-Sa'
  '-Cr'
  '-Co'
  '-Ci'
  "-Fu$sourceDirectory"
  "-Fu$textCompletionExampleDirectory"
  "-FU$unitOutputDirectory"
  "-FE$binaryOutputDirectory"
  $textCompletionExampleSource
)

Write-Host 'Building the portable text constraint-completion example.'
& $Compiler @textCompletionExampleCompilerArguments
$textCompletionExampleCompilerExitCode = $LASTEXITCODE
if ($textCompletionExampleCompilerExitCode -ne 0) {
  exit $textCompletionExampleCompilerExitCode
}

$textCompletionExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'ConstraintCompletion.exe'
} else {
  'ConstraintCompletion'
}
$textCompletionExampleExecutable = Join-Path $binaryOutputDirectory `
  $textCompletionExampleExecutableName

Write-Host "Smoke testing '$textCompletionExampleExecutable' with seed 0."
& $textCompletionExampleExecutable 0 | Out-Null
$textCompletionExampleExitCode = $LASTEXITCODE
if ($textCompletionExampleExitCode -ne 0) {
  exit $textCompletionExampleExitCode
}

$textPassExampleCompilerArguments = @(
  $CompilerOptions
  '-B'
  '-Mdelphi'
  '-Sa'
  '-Cr'
  '-Co'
  '-Ci'
  "-Fu$sourceDirectory"
  "-Fu$textPassExampleDirectory"
  "-FU$unitOutputDirectory"
  "-FE$binaryOutputDirectory"
  $textPassExampleSource
)

Write-Host 'Building the dependency-free multi-pass text example.'
& $Compiler @textPassExampleCompilerArguments
$textPassExampleCompilerExitCode = $LASTEXITCODE
if ($textPassExampleCompilerExitCode -ne 0) {
  exit $textPassExampleCompilerExitCode
}

$textPassExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'TextPassComposition.exe'
} else {
  'TextPassComposition'
}
$textPassExampleExecutable = Join-Path $binaryOutputDirectory `
  $textPassExampleExecutableName

Write-Host "Smoke testing '$textPassExampleExecutable' with seed 0."
& $textPassExampleExecutable 0 | Out-Null
$textPassExampleExitCode = $LASTEXITCODE
if ($textPassExampleExitCode -ne 0) {
  exit $textPassExampleExitCode
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

$musicVariationExampleCompilerArguments = @(
  $CompilerOptions
  '-B'
  '-Mdelphi'
  '-Sa'
  '-Cr'
  '-Co'
  '-Ci'
  "-Fu$sourceDirectory"
  "-Fu$musicVariationExampleDirectory"
  "-FU$unitOutputDirectory"
  "-FE$binaryOutputDirectory"
  $musicVariationExampleSource
)

Write-Host 'Building the negotiated music-variation example.'
& $Compiler @musicVariationExampleCompilerArguments
$musicVariationExampleCompilerExitCode = $LASTEXITCODE
if ($musicVariationExampleCompilerExitCode -ne 0) {
  exit $musicVariationExampleCompilerExitCode
}

$musicVariationExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'NegotiatedVariation.exe'
} else {
  'NegotiatedVariation'
}
$musicVariationExampleExecutable = Join-Path $binaryOutputDirectory `
  $musicVariationExampleExecutableName

Write-Host "Smoke testing '$musicVariationExampleExecutable' with seed 0."
& $musicVariationExampleExecutable 0 | Out-Null
$musicVariationExampleExitCode = $LASTEXITCODE
if ($musicVariationExampleExitCode -ne 0) {
  exit $musicVariationExampleExitCode
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
$spatialExampleExitCode = $LASTEXITCODE
if ($spatialExampleExitCode -ne 0) {
  exit $spatialExampleExitCode
}

$traceExampleCompilerArguments = @(
  $CompilerOptions
  '-B'
  '-Mdelphi'
  '-Sa'
  '-Cr'
  '-Co'
  '-Ci'
  "-Fu$sourceDirectory"
  "-Fu$traceExampleDirectory"
  "-FU$unitOutputDirectory"
  "-FE$binaryOutputDirectory"
  $traceExampleSource
)

Write-Host 'Building the dependency-free causal-trace inspector example.'
& $Compiler @traceExampleCompilerArguments
$traceExampleCompilerExitCode = $LASTEXITCODE
if ($traceExampleCompilerExitCode -ne 0) {
  exit $traceExampleCompilerExitCode
}

$traceExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'TraceInspector.exe'
} else {
  'TraceInspector'
}
$traceExampleExecutable = Join-Path $binaryOutputDirectory `
  $traceExampleExecutableName

Write-Host "Smoke testing '$traceExampleExecutable' with seed 0."
& $traceExampleExecutable 0 | Out-Null
$traceExampleExitCode = $LASTEXITCODE
if ($traceExampleExitCode -ne 0) {
  exit $traceExampleExitCode
}

$negotiationExampleCompilerArguments = @(
  $CompilerOptions
  '-B'
  '-Mdelphi'
  '-Sa'
  '-Cr'
  '-Co'
  '-Ci'
  "-Fu$sourceDirectory"
  "-Fu$negotiationExampleDirectory"
  "-FU$unitOutputDirectory"
  "-FE$binaryOutputDirectory"
  $negotiationExampleSource
)

Write-Host 'Building the dependency-free pass-negotiation example.'
& $Compiler @negotiationExampleCompilerArguments
$negotiationExampleCompilerExitCode = $LASTEXITCODE
if ($negotiationExampleCompilerExitCode -ne 0) {
  exit $negotiationExampleCompilerExitCode
}

$negotiationExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'PassNegotiation.exe'
} else {
  'PassNegotiation'
}
$negotiationExampleExecutable = Join-Path $binaryOutputDirectory `
  $negotiationExampleExecutableName

Write-Host "Smoke testing '$negotiationExampleExecutable'."
& $negotiationExampleExecutable | Out-Null
$negotiationExampleExitCode = $LASTEXITCODE
if ($negotiationExampleExitCode -ne 0) {
  exit $negotiationExampleExitCode
}

$buildingExampleCompilerArguments = @(
  $CompilerOptions
  '-B'
  '-Mdelphi'
  '-Sa'
  '-Cr'
  '-Co'
  '-Ci'
  "-Fu$sourceDirectory"
  "-Fu$buildingExampleDirectory"
  "-Fu$buildingCommonDirectory"
  "-FU$unitOutputDirectory"
  "-FE$binaryOutputDirectory"
  $buildingExampleSource
)

Write-Host 'Building the dependency-free multi-pass Building 3D example.'
& $Compiler @buildingExampleCompilerArguments
$buildingExampleCompilerExitCode = $LASTEXITCODE
if ($buildingExampleCompilerExitCode -ne 0) {
  exit $buildingExampleCompilerExitCode
}

$buildingExampleExecutableName = if ($env:OS -eq 'Windows_NT') {
  'MultiPassBuilding.exe'
} else {
  'MultiPassBuilding'
}
$buildingExampleExecutable = Join-Path $binaryOutputDirectory `
  $buildingExampleExecutableName

Write-Host "Smoke testing '$buildingExampleExecutable' with seed 0."
& $buildingExampleExecutable 0 | Out-Null
$buildingExampleExitCode = $LASTEXITCODE
if ($buildingExampleExitCode -ne 0) {
  exit $buildingExampleExitCode
}

$buildingSvgCompilerArguments = @(
  $CompilerOptions
  '-B'
  '-Mdelphi'
  '-Sa'
  '-Cr'
  '-Co'
  '-Ci'
  "-Fu$sourceDirectory"
  "-Fu$buildingCommonDirectory"
  "-FU$unitOutputDirectory"
  "-FE$binaryOutputDirectory"
  $buildingSvgSource
)

Write-Host 'Building the dependency-free Building 3D SVG example.'
& $Compiler @buildingSvgCompilerArguments
$buildingSvgCompilerExitCode = $LASTEXITCODE
if ($buildingSvgCompilerExitCode -ne 0) {
  exit $buildingSvgCompilerExitCode
}

$buildingSvgExecutableName = if ($env:OS -eq 'Windows_NT') {
  'Building3DSvg.exe'
} else {
  'Building3DSvg'
}
$buildingSvgExecutable = Join-Path $binaryOutputDirectory `
  $buildingSvgExecutableName
$buildingSvgOutput = Join-Path $binaryOutputDirectory `
  'building3d-seed-zero.svg'
Write-Host "Smoke testing '$buildingSvgExecutable' with seed 0."
& $buildingSvgExecutable 0 $buildingSvgOutput | Out-Null
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

Write-Host 'Building and checking all five Training Studio presets.'
& $Compiler @CompilerOptions -B -Mdelphi -Sa -Cr -Co -Ci `
  "-Fu$sourceDirectory" "-Fu$trainingStudioExampleDirectory" `
  "-FU$unitOutputDirectory" "-FE$binaryOutputDirectory" `
  (Join-Path $trainingStudioExampleDirectory 'TrainingStudio.lpr')
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
$trainingStudioExecutableName = if ($env:OS -eq 'Windows_NT') {
  'TrainingStudio.exe'
} else { 'TrainingStudio' }
& (Join-Path $binaryOutputDirectory $trainingStudioExecutableName) --selftest
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

Write-Host 'Building and checking Music Studio.'
& $Compiler @CompilerOptions -B -Mdelphi -Sa -Cr -Co -Ci `
  "-Fu$sourceDirectory" "-Fu$musicStudioExampleDirectory" `
  "-FU$unitOutputDirectory" "-FE$binaryOutputDirectory" `
  (Join-Path $musicStudioExampleDirectory 'MusicStudio.lpr')
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
$musicStudioExecutableName = if ($env:OS -eq 'Windows_NT') {
  'MusicStudio.exe'
} else { 'MusicStudio' }
& (Join-Path $binaryOutputDirectory $musicStudioExecutableName) --selftest
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

Write-Host 'Building and checking the Music Studio form matrix.'
& $Compiler @CompilerOptions -B -Mdelphi -Sa -Cr -Co -Ci `
  "-Fu$sourceDirectory" "-Fu$musicStudioExampleDirectory" `
  "-FU$unitOutputDirectory" "-FE$binaryOutputDirectory" `
  (Join-Path $musicStudioExampleDirectory 'MusicStudioFormProbe.lpr')
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
$musicStudioProbeExecutableName = if ($env:OS -eq 'Windows_NT') {
  'MusicStudioFormProbe.exe'
} else { 'MusicStudioFormProbe' }
$musicStudioProbeLines = @(
  & (Join-Path $binaryOutputDirectory $musicStudioProbeExecutableName))
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
$musicStudioProbeActual = ($musicStudioProbeLines -join "`n")
$musicStudioProbeExpected = [System.IO.File]::ReadAllText(
  $musicStudioFormFixture).Replace("`r`n", "`n").Replace("`r", "`n").TrimEnd(
    [char[]] "`n")
if ($musicStudioProbeActual -cne $musicStudioProbeExpected) {
  Write-Error 'Music Studio form probe differs from its checked fixture.'
}
Write-Host 'Music Studio form matrix matches its checked fixture.'

Write-Host 'Building and checking the streaming Music Studio renderer.'
foreach ($renderSource in @(
  (Join-Path $musicStudioExampleDirectory 'MusicStudioRender.lpr'),
  (Join-Path $repositoryRoot 'test/wfc_music_render_process_test.lpr')
)) {
  & $Compiler @CompilerOptions -B -Mdelphi -Sa -Cr -Co -Ci `
    "-Fu$sourceDirectory" "-Fu$musicStudioExampleDirectory" `
    "-FU$unitOutputDirectory" "-FE$binaryOutputDirectory" $renderSource
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}
& (Join-Path $binaryOutputDirectory "wfc_music_render_process_test$toolExecutableSuffix") `
  (Join-Path $binaryOutputDirectory "MusicStudioRender$toolExecutableSuffix") `
  $binaryOutputDirectory
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
exit 0
