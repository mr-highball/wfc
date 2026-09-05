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
$commonDirectory = Join-Path $repositoryRoot 'examples/2D/common'
$browserDirectory = Join-Path $repositoryRoot 'examples/2D/02_BrowserWorld'
$browserSource = Join-Path $browserDirectory 'BrowserWorld.lpr'
$browserHtml = Join-Path $browserDirectory 'index.html'
$browserCss = Join-Path $browserDirectory 'browserworld.css'
$outputDirectory = Join-Path $repositoryRoot 'build/browser/world2d'
$unitOutputDirectory = Join-Path $outputDirectory 'units'
$webOutputDirectory = Join-Path $outputDirectory 'www'
$stagedJavaScript = Join-Path $webOutputDirectory 'BrowserWorld.js'

foreach ($requiredFile in @($browserSource, $browserHtml, $browserCss)) {
  if (-not (Test-Path -LiteralPath $requiredFile -PathType Leaf)) {
    throw "Required browser demo file was not found: $requiredFile"
  }
}

New-Item -ItemType Directory -Force -Path `
  $unitOutputDirectory, $webOutputDirectory | Out-Null

# Never mistake an artifact from an earlier build for the current compiler's
# output.
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

Write-Host "Building the browser world with '$Compiler'."
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
    (Join-Path $webOutputDirectory 'browserworld.css'))) {
  if ((Get-Item -LiteralPath $stagedAsset).Length -eq 0) {
    throw "Staged browser asset is empty: $stagedAsset"
  }
}

Write-Host "Browser world staged in '$webOutputDirectory'."
