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
$browserDirectory = Join-Path $repositoryRoot 'examples/text/03_PassComposition'
$browserSource = Join-Path $browserDirectory 'BrowserTextPassComposition.lpr'
$browserHtml = Join-Path $browserDirectory 'index.html'
$browserCss = Join-Path $browserDirectory 'browsertextpasses.css'
$outputDirectory = Join-Path $repositoryRoot 'build/browser/text-passes'
$unitOutputDirectory = Join-Path $outputDirectory 'units'
$webOutputDirectory = Join-Path $outputDirectory 'www'
$stagedJavaScript = Join-Path $webOutputDirectory `
  'BrowserTextPassComposition.js'

foreach ($requiredFile in @($browserSource, $browserHtml, $browserCss)) {
  if (-not (Test-Path -LiteralPath $requiredFile -PathType Leaf)) {
    throw "Required browser text demo file was not found: $requiredFile"
  }
}

New-Item -ItemType Directory -Force -Path `
  $unitOutputDirectory, $webOutputDirectory | Out-Null

# Never accept JavaScript left by an earlier compiler invocation.
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

Write-Host "Building the browser text workbench with '$Compiler'."
& $Compiler @compilerArguments
$compilerExitCode = $LASTEXITCODE
if ($compilerExitCode -ne 0) {
  exit $compilerExitCode
}

if (-not (Test-Path -LiteralPath $stagedJavaScript -PathType Leaf) -or
    (Get-Item -LiteralPath $stagedJavaScript).Length -eq 0) {
  throw "pas2js did not produce $stagedJavaScript"
}

Copy-Item -LiteralPath $browserHtml -Destination $webOutputDirectory -Force
Copy-Item -LiteralPath $browserCss -Destination $webOutputDirectory -Force

foreach ($stagedAsset in @(
    (Join-Path $webOutputDirectory 'index.html'),
    (Join-Path $webOutputDirectory 'browsertextpasses.css'))) {
  if ((Get-Item -LiteralPath $stagedAsset).Length -eq 0) {
    throw "Staged browser text asset is empty: $stagedAsset"
  }
}

Write-Host "Browser text workbench staged in '$webOutputDirectory'."
