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
$browserDirectory = Join-Path $repositoryRoot 'examples/music/05_MusicStudio'
$browserSource = Join-Path $browserDirectory 'BrowserMusicStudio.lpr'
$browserApp = Join-Path $browserDirectory 'browser_music_studio_app.pas'
$studioOwner = Join-Path $browserDirectory 'music_studio_workbench.pas'
$browserHtml = Join-Path $browserDirectory 'index.html'
$browserCss = Join-Path $browserDirectory 'musicstudio.css'
$outputDirectory = Join-Path $repositoryRoot 'build/browser/music'
$unitOutputDirectory = Join-Path $outputDirectory 'units'
$webOutputDirectory = Join-Path $outputDirectory 'www'
$stagedJavaScript = Join-Path $webOutputDirectory 'BrowserMusicStudio.js'

foreach ($requiredFile in @(
    $browserSource, $browserApp, $studioOwner, $browserHtml, $browserCss)) {
  if (-not (Test-Path -LiteralPath $requiredFile -PathType Leaf)) {
    throw "Required Music Studio browser file was not found: $requiredFile"
  }
}

New-Item -ItemType Directory -Force -Path `
  $unitOutputDirectory, $webOutputDirectory | Out-Null
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

Write-Host "Building Music Studio with '$Compiler'."
& $Compiler @compilerArguments
$compilerExitCode = $LASTEXITCODE
if ($compilerExitCode -ne 0) {
  exit $compilerExitCode
}

if (-not (Test-Path -LiteralPath $stagedJavaScript -PathType Leaf) -or
    (Get-Item -LiteralPath $stagedJavaScript).Length -eq 0) {
  throw "pas2js did not produce $stagedJavaScript"
}

Copy-Item -LiteralPath $browserHtml, $browserCss `
  -Destination $webOutputDirectory -Force
foreach ($stagedAsset in @(
    (Join-Path $webOutputDirectory 'index.html'),
    (Join-Path $webOutputDirectory 'musicstudio.css'))) {
  if ((Get-Item -LiteralPath $stagedAsset).Length -eq 0) {
    throw "Staged Music Studio asset is empty: $stagedAsset"
  }
}

Write-Host "Music Studio staged in '$webOutputDirectory'."
