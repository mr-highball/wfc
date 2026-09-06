#requires -Version 7.0
[CmdletBinding()]
param(
  [Parameter(Mandatory=$true)][string] $Browser,
  [int] $Port = 4180,
  [string] $Server = '',
  [string] $Checker = '',
  [string] $Capture = '',
  [string] $TestName = '',
  [string] $WebRoot = '',
  [string] $Page = '',
  [string[]] $Expect = @()
)
$ErrorActionPreference = 'Stop'
if (Get-Variable PSNativeCommandUseErrorActionPreference -ErrorAction SilentlyContinue) {
  $PSNativeCommandUseErrorActionPreference = $false
}
$repositoryRoot = $PSScriptRoot
if (-not $Server) { $Server = Join-Path $repositoryRoot 'build/native/bin/wfc_serve.exe' }
if (-not $Checker) { $Checker = Join-Path $repositoryRoot 'build/native/bin/wfc_browser_check.exe' }
if (-not $Capture) { $Capture = Join-Path $repositoryRoot 'build/native/bin/wfc_browser_capture.exe' }
$web = Join-Path $repositoryRoot 'build/browser/tests/www'
$results = Join-Path $repositoryRoot 'build/browser/tests/results'
$nativeOnly = @('wfc_browser_dom_test','wfc_browser_socket_test','wfc_browser_websocket_test','wfc_browser_cdp_test','wfc_browser_capture_test','wfc_browser_args_test','wfc_serve_test','wfc_music_render_process_test','wfc_music_ensemble_render_process_test','wfc_music_ensemble_midi_render_process_test','wfc_music_voices_render_process_test','wfc_connectivity_process_test','wfc_music_studies_process_test')
$standalone = $WebRoot -ne '' -or $Page -ne '' -or $Expect.Count -gt 0
if ($standalone) {
  if (-not $WebRoot -or -not $Page -or $TestName) {
    throw 'Standalone mode requires -WebRoot and -Page and excludes -TestName.'
  }
  if (-not (Test-Path -LiteralPath $WebRoot -PathType Container)) { throw 'Standalone web root must exist.' }
  $web = (Resolve-Path -LiteralPath $WebRoot).ProviderPath
  $pagePath = ($Page -split '\?', 2)[0]
  $pageSegments = $pagePath -split '/'
  if ($Page -cnotmatch '^[a-zA-Z0-9_-][a-zA-Z0-9_./-]*(\?[a-zA-Z0-9_.~=&%+-]*)?$' -or
      $pageSegments -contains '.' -or $pageSegments -contains '..' -or $pagePath.Contains('//')) {
    throw 'Standalone page must be a simple relative ASCII path with an optional query.'
  }
  if (-not (Test-Path -LiteralPath (Join-Path $web $pagePath) -PathType Leaf)) {
    throw 'Standalone page must name an existing file under the web root.'
  }
  $expectNames = [System.Collections.Generic.HashSet[string]]::new([System.StringComparer]::Ordinal)
  foreach ($expectation in $Expect) {
    $equals = $expectation.IndexOf('=')
    if ($equals -le 0) { throw 'Standalone expectation must use name=value.' }
    $expectName = $expectation.Substring(0, $equals)
    if ($expectName -cnotmatch '^[a-z0-9_:-]+$' -or -not $expectNames.Add($expectName)) {
      throw 'Standalone expectation names must be lowercase ASCII and unique.'
    }
    if ($expectName -ceq 'data-self-test' -and $expectation.Substring($equals + 1) -cne 'passed') {
      throw 'Standalone data-self-test expectation must be passed.'
    }
  }
  $sources = @([pscustomobject]@{ BaseName = 'standalone-demo' })
  $firstPage = $Page
} else {
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
  $firstPage = $sources[0].BaseName + '.html'
}

function Join-NativeArguments([string[]] $Arguments) {
  # Start-Process joins ArgumentList with spaces. Quote every argument using
  # Windows native command-line rules, including quotes and trailing slashes.
  ($Arguments | ForEach-Object {
    '"' + ($_ -replace '(\\*)"', '$1$1\"' -replace '(\\+)$', '$1$1') + '"'
  }) -join ' '
}

function Stop-OwnedProcess([System.Diagnostics.Process] $Process, [string] $Label,
  [switch] $RejectFailedExit) {
  if (-not $Process) { return }
  try {
    if ($Process.HasExited) {
      if ($RejectFailedExit -and $Process.ExitCode -ne 0) {
        throw "Owned $Label process exited unexpectedly (exit $($Process.ExitCode))."
      }
    } else {
      try { $Process.Kill($true) }
      catch { if (-not $Process.HasExited) { throw } }
    }
    if (-not $Process.WaitForExit(5000)) { throw "Cleanup deadline exceeded for owned $Label process." }
  } finally { $Process.Dispose() }
}

function Wait-CaptureProcess([System.Diagnostics.Process] $Process,
  [System.Diagnostics.Stopwatch] $Clock, [string] $Label) {
  $remaining = 60000 - $Clock.ElapsedMilliseconds
  if ($remaining -le 0 -or -not $Process.WaitForExit([int]$remaining)) {
    throw "Capture timed out: $Label."
  }
  if ($Clock.ElapsedMilliseconds -ge 60000) { throw "Capture timed out: $Label." }
  if ($Process.ExitCode -ne 0) { throw "Capture failed: $Label (exit $($Process.ExitCode))." }
}

# Run only current sources: old staging files neither replace missing coverage
# nor count as additional current tests. Each capture has a new owned profile.
New-Item -ItemType Directory -Force -Path $results | Out-Null
$serverProcess = $null
$runFailure = $null
try {
  $serverProcess = Start-Process -FilePath $Server -ArgumentList (Join-NativeArguments @('--root', $web, '--port', "$Port")) -PassThru -WindowStyle Hidden -RedirectStandardOutput (Join-Path $results 'server.log') -RedirectStandardError (Join-Path $results 'server-error.log')
  $ready = $false
  for ($attempt = 0; $attempt -lt 50; $attempt++) {
    if ($serverProcess.HasExited) { throw 'FPC server exited before readiness.' }
    try {
      $bound = Select-String -LiteralPath (Join-Path $results 'server.log') -SimpleMatch -Quiet -Pattern "WFC static server: http://127.0.0.1:$Port/"
      if (-not $bound) { Start-Sleep -Milliseconds 200; continue }
      $null = Invoke-WebRequest -Uri ("http://127.0.0.1:$Port/" + $firstPage) -TimeoutSec 2
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
    $caseName = $source.BaseName
    $pageName = if ($standalone) { $Page } else { $caseName + '.html' }
    Write-Host "Checking $($count + 1)/$($sources.Count): $caseName."
    $runDirectory = Join-Path $results ($caseName + '-' + [guid]::NewGuid().ToString('N'))
    New-Item -ItemType Directory -Path $runDirectory | Out-Null
    $profile = Join-Path $runDirectory 'profile'
    $dom = Join-Path $runDirectory 'capture.dom'
    $domAlias = Join-Path $results ($caseName + '.dom')
    if (Test-Path -LiteralPath $domAlias -PathType Container) { throw "DOM alias is a directory: $domAlias" }
    if (Test-Path -LiteralPath $domAlias) { Remove-Item -LiteralPath $domAlias -Force }
    $captureLog = Join-Path $runDirectory 'deadline.log'
    $browserProcess = $null
    $captureProcess = $null
    $caseFailures = @()
    try {
      # An empty branch result is $null, not an empty array. Assign inside the
      # branch so a standalone run with no extra expectations does not emit
      # an accidental empty --expect argument to the native capture tool.
      [string[]]$expectations = @()
      if ($standalone) { $expectations = @($Expect) }
      else { $expectations = @('data-self-test=passed') }
      if (-not @($expectations | Where-Object { $_ -cmatch '^data-self-test=' }).Count) {
        $expectations = @('data-self-test=passed') + $expectations
      }
      if ($caseName -eq 'wfc_browser_demo_entries_test') {
        $expectations += @('data-demo-entries-self-test=passed', 'data-demo-entries-count=10')
      }
      if ($caseName -eq 'wfc_music_ensemble_stream_demo_test') {
        $expectations += @('data-stream-self-test=passed', 'data-stream-release=passed',
          'data-midi-stream-self-test=passed', 'data-midi-stream-release=passed')
      }
      if ($caseName -eq 'wfc_music_ensemble_demo_test') {
        $expectations += @('data-developed-profile=passed')
      }
      if ($caseName -eq 'wfc_music_voices_browser_test') {
        $expectations += @('data-voice-stream-self-test=passed', 'data-voice-stream-release=passed')
      }
      $assertions = @('--dom', $dom)
      foreach ($expectation in $expectations) { $assertions += @('--expect', $expectation) }

      # One budget includes deadline/profile helpers, browser startup and capture.
      # The absolute native deadline is computed before Chromium starts.
      $caseClock = [System.Diagnostics.Stopwatch]::StartNew()
      $deadlineFile = Join-Path $runDirectory 'deadline.txt'
      $captureProcess = Start-Process -FilePath $Capture -ArgumentList (Join-NativeArguments @('--deadline-after', '60000')) -PassThru -WindowStyle Hidden -RedirectStandardOutput $deadlineFile -RedirectStandardError $captureLog
      Wait-CaptureProcess $captureProcess $caseClock "$caseName deadline helper"
      Stop-OwnedProcess $captureProcess 'deadline helper'; $captureProcess = $null
      $deadline = (Get-Content -LiteralPath $deadlineFile -Raw).Trim()
      if ($deadline -cnotmatch '^[1-9][0-9]*$') { throw 'Capture helper returned an invalid deadline.' }

      $captureLog = Join-Path $runDirectory 'prepare.log'
      $captureProcess = Start-Process -FilePath $Capture -ArgumentList (Join-NativeArguments @('--prepare-profile', $profile)) -PassThru -WindowStyle Hidden -RedirectStandardOutput (Join-Path $runDirectory 'prepare-output.log') -RedirectStandardError $captureLog
      Wait-CaptureProcess $captureProcess $caseClock "$caseName profile helper"
      Stop-OwnedProcess $captureProcess 'profile helper'; $captureProcess = $null

      $browserArguments = @('--headless','--disable-gpu','--disable-dev-shm-usage',
        '--no-first-run','--no-default-browser-check', "--user-data-dir=$profile",
        '--remote-debugging-address=127.0.0.1','--remote-debugging-port=0','about:blank')
      $browserProcess = Start-Process -FilePath $Browser -ArgumentList (Join-NativeArguments $browserArguments) -PassThru -WindowStyle Hidden -RedirectStandardOutput (Join-Path $runDirectory 'browser-output.log') -RedirectStandardError (Join-Path $runDirectory 'browser.log')
      $captureLog = Join-Path $runDirectory 'capture.log'
      $captureArguments = @('--profile', $profile, '--url', "http://127.0.0.1:$Port/$pageName", '--deadline', $deadline) + $assertions
      $captureProcess = Start-Process -FilePath $Capture -ArgumentList (Join-NativeArguments $captureArguments) -PassThru -WindowStyle Hidden -RedirectStandardOutput (Join-Path $runDirectory 'capture-output.log') -RedirectStandardError $captureLog
      Wait-CaptureProcess $captureProcess $caseClock $caseName
      & $Checker @assertions
      if ($LASTEXITCODE -ne 0) { throw "Browser assertions failed: $caseName." }
    } catch {
      $caseFailures += $_.Exception.Message
      Write-Host $_.Exception.Message
      if (Test-Path -LiteralPath $captureLog -PathType Leaf) {
        Get-Content -LiteralPath $captureLog | ForEach-Object { Write-Host $_ }
      }
      Write-Host "Capture evidence: $runDirectory"
    } finally {
      try { Stop-OwnedProcess $captureProcess 'capture' }
      catch { $caseFailures += $_.Exception.Message; Write-Host $_.Exception.Message }
      try { Stop-OwnedProcess $browserProcess 'browser' -RejectFailedExit }
      catch { $caseFailures += $_.Exception.Message; Write-Host $_.Exception.Message }
    }
    if ($caseFailures.Count -eq 0) {
      try { Copy-Item -LiteralPath $dom -Destination $domAlias }
      catch {
        $caseFailures += $_.Exception.Message
        Write-Host $_.Exception.Message
      }
    }
    if ($caseFailures.Count -gt 0) { $failures += $caseName }
    $count++
  }
  if ($count -eq 0) { throw 'No browser conformance pages found.' }
  if ($failures.Count -gt 0) {
    throw "Browser conformance failed ($($failures.Count)/$count): $($failures -join ', ')"
  }
  Write-Host "Pascal conformance passed in the browser: $count programs."
} catch {
  $runFailure = $_
} finally {
  try { Stop-OwnedProcess $serverProcess 'server' }
  catch {
    if ($runFailure) { Write-Host $_.Exception.Message } else { $runFailure = $_ }
  }
}
if ($runFailure) { throw $runFailure }
