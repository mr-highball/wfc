#requires -Version 7.0
[CmdletBinding()]
param(
  [Parameter(Mandatory=$true)][string] $Browser,
  [int] $Port = 4180,
  [string] $Server = '',
  [string] $Checker = '',
  [string] $TestName = ''
)
$ErrorActionPreference = 'Stop'
if (Get-Variable PSNativeCommandUseErrorActionPreference -ErrorAction SilentlyContinue) {
  $PSNativeCommandUseErrorActionPreference = $false
}
$repositoryRoot = $PSScriptRoot
if (-not $Server) { $Server = Join-Path $repositoryRoot 'build/native/bin/wfc_serve.exe' }
if (-not $Checker) { $Checker = Join-Path $repositoryRoot 'build/native/bin/wfc_browser_check.exe' }
$web = Join-Path $repositoryRoot 'build/browser/tests/www'
$results = Join-Path $repositoryRoot 'build/browser/tests/results'
$nativeOnly = @('wfc_browser_dom_test','wfc_serve_test','wfc_music_render_process_test','wfc_music_ensemble_render_process_test','wfc_music_ensemble_midi_render_process_test','wfc_music_voices_render_process_test')
$sources = @(Get-ChildItem -LiteralPath (Join-Path $repositoryRoot 'test') -Filter '*_test.lpr' |
  Where-Object { $_.BaseName -notin $nativeOnly } | Sort-Object Name)
if ($TestName) { $sources = @($sources | Where-Object { $_.BaseName -ceq $TestName }) }
if ($sources.Count -eq 0) { throw 'No current browser conformance sources found.' }
$missing = @()
foreach ($source in $sources) {
  foreach ($extension in @('.html','.js')) {
    $expected = Join-Path $web ($source.BaseName + $extension)
    if (-not (Test-Path -LiteralPath $expected -PathType Leaf)) { $missing += $expected }
  }
}
if ($missing.Count -gt 0) {
  throw "Browser conformance staging is incomplete; run build-browser-tests.ps1. Missing: $($missing -join ', ')"
}
# Run only current sources: old staging files neither replace missing coverage
# nor count as additional current tests. Nothing stale is deleted here.
New-Item -ItemType Directory -Force -Path $results | Out-Null
$serverProcess = $null
try {
  $serverProcess = Start-Process -FilePath $server -ArgumentList @('--root', ('"' + $web + '"'), '--port', $Port) -PassThru -WindowStyle Hidden -RedirectStandardOutput (Join-Path $results 'server.log') -RedirectStandardError (Join-Path $results 'server-error.log')
  $ready = $false
  for ($attempt = 0; $attempt -lt 50; $attempt++) {
    if ($serverProcess.HasExited) { throw 'FPC server exited before readiness.' }
    try {
      $bound = Select-String -LiteralPath (Join-Path $results 'server.log') -SimpleMatch -Quiet -Pattern "WFC static server: http://127.0.0.1:$Port/"
      if (-not $bound) { Start-Sleep -Milliseconds 200; continue }
      $null = Invoke-WebRequest -Uri ("http://127.0.0.1:$Port/" + $sources[0].BaseName + '.html') -TimeoutSec 2
      if ($serverProcess.HasExited) { throw 'FPC server exited during readiness.' }
      $ready = $true
      break
    } catch { Start-Sleep -Milliseconds 200 }
  }
  if (-not $ready) { throw 'FPC server did not become ready.' }
  $count = 0
  $failures = @()
  foreach ($source in $sources) {
    if ($serverProcess.HasExited) { throw 'FPC server exited during conformance checks.' }
    $page = Get-Item -LiteralPath (Join-Path $web ($source.BaseName + '.html'))
    Write-Host "Checking $($count + 1)/$($sources.Count): $($page.BaseName)."
    $profile = Join-Path $results ('profile-' + $page.BaseName)
    New-Item -ItemType Directory -Force -Path $profile | Out-Null
    $dom = Join-Path $results ($page.BaseName + '.dom')
    $log = Join-Path $results ($page.BaseName + '.log')
    # Eight real pages retain their individual 15-second virtual deadlines.
    # The browser process still has the same 60-second real-time deadline.
    $virtualTimeBudget = if ($page.BaseName -eq 'wfc_browser_demo_entries_test') { 125000 } else { 15000 }
    $arguments = @('--headless','--disable-gpu','--disable-dev-shm-usage',
      '--no-first-run','--no-default-browser-check',('--user-data-dir="' + $profile + '"'),
      ("--virtual-time-budget=$virtualTimeBudget"),'--dump-dom',("http://127.0.0.1:$Port/" + $page.Name))
    $browserProcess = $null
    try {
      $browserProcess = Start-Process -FilePath $Browser -ArgumentList $arguments -PassThru -WindowStyle Hidden -RedirectStandardOutput $dom -RedirectStandardError $log
      if (-not $browserProcess.WaitForExit(60000)) {
        $browserProcess.Kill($true)
        $browserProcess.WaitForExit()
        throw "Browser timed out: $($page.Name)"
      }
      if ($browserProcess.ExitCode -ne 0) { throw "Browser failed: $($page.Name)" }
      $browserProcess.WaitForExit()
      $browserProcess.Dispose()
      $browserProcess = $null
      $assertions = @('--dom', $dom, '--expect', 'data-self-test=passed')
      if ($page.BaseName -eq 'wfc_browser_demo_entries_test') {
        $assertions += @('--expect', 'data-demo-entries-self-test=passed')
      }
      if ($page.BaseName -eq 'wfc_music_ensemble_stream_demo_test') {
        # The synchronous harness cannot certify awaited file transactions.
        $assertions += @('--expect', 'data-stream-self-test=passed')
        $assertions += @('--expect', 'data-stream-release=passed')
        $assertions += @('--expect', 'data-midi-stream-self-test=passed')
        $assertions += @('--expect', 'data-midi-stream-release=passed')
      }
      if ($page.BaseName -eq 'wfc_music_voices_browser_test') {
        # Awaited controller/file ownership has its own completion evidence.
        $assertions += @('--expect', 'data-voice-stream-self-test=passed')
        $assertions += @('--expect', 'data-voice-stream-release=passed')
      }
      & $checker @assertions
      if ($LASTEXITCODE -ne 0) { throw "Browser assertions failed: $($page.Name)" }
    } catch {
      $failures += $page.Name
      Write-Host $_.Exception.Message
    } finally {
      if ($browserProcess) {
        if (-not $browserProcess.HasExited) {
          $browserProcess.Kill($true)
          $browserProcess.WaitForExit()
        }
        $browserProcess.Dispose()
      }
    }
    $count++
  }
  if ($count -eq 0) { throw 'No browser conformance pages found.' }
  if ($failures.Count -gt 0) {
    throw "Browser conformance failed ($($failures.Count)/$count): $($failures -join ', ')"
  }
  Write-Host "Pascal conformance passed in the browser: $count programs."
} finally {
  if ($serverProcess) {
    if (-not $serverProcess.HasExited) { $serverProcess.Kill($true); $serverProcess.WaitForExit() }
    $serverProcess.Dispose()
  }
}
