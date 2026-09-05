[CmdletBinding()]
param(
  [Parameter(Mandatory = $true)]
  [ValidateNotNullOrEmpty()]
  [string] $Learner,
  [Parameter(Mandatory = $true)]
  [ValidateNotNullOrEmpty()]
  [string] $Validator,
  [Parameter(Mandatory = $true)]
  [ValidateNotNullOrEmpty()]
  [string] $Runner
)

$ErrorActionPreference = 'Stop'
$repositoryRoot = Split-Path -Parent $PSScriptRoot
$fixtureDirectory = Join-Path $repositoryRoot 'examples/learning/04_TrainingDocuments'
$missingInput = Join-Path $fixtureDirectory 'missing.wfclearn'
$maximumCapturedLength = 8192
$processTimeoutMilliseconds = 30000
$ascii = [System.Text.Encoding]::ASCII
$emptyInput = [byte[]] @()
$caseCount = 0

function Assert-Condition {
  param(
    [bool] $Condition,
    [string] $Message
  )
  if (-not $Condition) {
    throw "Training CLI process check failed: $Message"
  }
}

function Assert-BytesEqual {
  param(
    [byte[]] $Actual,
    [byte[]] $Expected,
    [string] $Message
  )
  Assert-Condition ($Actual.Length -eq $Expected.Length) `
    "$Message (length $($Actual.Length), expected $($Expected.Length))"
  for ($index = 0; $index -lt $Expected.Length; $index++) {
    if ($Actual[$index] -ne $Expected[$index]) {
      throw "Training CLI process check failed: $Message " +
        "(first difference at byte $index)"
    }
  }
}

function Assert-EmptyBytes {
  param(
    [byte[]] $Actual,
    [string] $Message
  )
  Assert-Condition ($Actual.Length -eq 0) `
    "$Message (received $($Actual.Length) bytes)"
}

function Assert-Diagnostic {
  param(
    [byte[]] $Actual,
    [string] $Prefix,
    [string] $Message
  )
  Assert-Condition ($Actual.Length -le $maximumCapturedLength) `
    "$Message exceeded the diagnostic capture limit"
  $text = $ascii.GetString($Actual)
  Assert-Condition ($text.StartsWith($Prefix)) "$Message has the wrong class"
  Assert-Condition ($text.EndsWith("`n") -and (-not $text.Contains("`r"))) `
    "$Message is not exact LF text"
  Assert-Condition ($text.IndexOf("`n") -eq $text.Length - 1) `
    "$Message is not one line"
}

function Add-ProcessArguments {
  param(
    [System.Diagnostics.ProcessStartInfo] $StartInfo,
    [string[]] $Arguments
  )
  if ($null -ne $StartInfo.PSObject.Properties['ArgumentList']) {
    foreach ($argument in $Arguments) {
      [void] $StartInfo.ArgumentList.Add($argument)
    }
    return
  }

  $quoted = foreach ($argument in $Arguments) {
    if ($argument.Contains('"')) {
      throw 'The legacy process launcher cannot quote an argument containing ".'
    }
    '"' + $argument + '"'
  }
  $StartInfo.Arguments = $quoted -join ' '
}

function Invoke-CapturedProcess {
  param(
    [string] $Executable,
    [string[]] $Arguments,
    [byte[]] $StandardInput
  )
  $startInfo = [System.Diagnostics.ProcessStartInfo]::new()
  $startInfo.FileName = $Executable
  $startInfo.UseShellExecute = $false
  $startInfo.CreateNoWindow = $true
  $startInfo.RedirectStandardInput = $true
  $startInfo.RedirectStandardOutput = $true
  $startInfo.RedirectStandardError = $true
  Add-ProcessArguments -StartInfo $startInfo -Arguments $Arguments

  $process = [System.Diagnostics.Process]::new()
  $standardOutput = [System.IO.MemoryStream]::new()
  $standardError = [System.IO.MemoryStream]::new()
  try {
    $process.StartInfo = $startInfo
    Assert-Condition -Condition ($process.Start()) `
      -Message "could not start '$Executable'"
    $outputCopy = $process.StandardOutput.BaseStream.CopyToAsync(
      $standardOutput)
    $errorCopy = $process.StandardError.BaseStream.CopyToAsync(
      $standardError)
    if (($null -ne $StandardInput) -and ($StandardInput.Length -gt 0)) {
      $process.StandardInput.BaseStream.Write(
        $StandardInput, 0, $StandardInput.Length)
    }
    $process.StandardInput.Close()
    if (-not $process.WaitForExit($processTimeoutMilliseconds)) {
      try {
        $process.Kill()
      } catch {
        # Preserve the timeout as the primary failure.
      }
      $process.WaitForExit()
      throw "Training CLI process check failed: '$Executable' timed out"
    }
    [void] $outputCopy.GetAwaiter().GetResult()
    [void] $errorCopy.GetAwaiter().GetResult()
    Assert-Condition ($standardOutput.Length -le $maximumCapturedLength) `
      'standard output exceeded the process-fixture capture limit'
    Assert-Condition ($standardError.Length -le $maximumCapturedLength) `
      'standard error exceeded the process-fixture capture limit'
    return [PSCustomObject] @{
      ExitCode = $process.ExitCode
      StandardOutput = $standardOutput.ToArray()
      StandardError = $standardError.ToArray()
    }
  } finally {
    $process.Dispose()
    $standardOutput.Dispose()
    $standardError.Dispose()
  }
}

function Check-Case {
  param(
    [string] $Executable,
    [string[]] $Arguments,
    [byte[]] $StandardInput,
    [int] $ExpectedExitCode,
    [byte[]] $ExpectedOutput,
    [string] $DiagnosticPrefix,
    [string] $Name
  )
  $result = Invoke-CapturedProcess -Executable $Executable `
    -Arguments $Arguments -StandardInput $StandardInput
  Assert-Condition ($result.ExitCode -eq $ExpectedExitCode) `
    "$Name returned $($result.ExitCode), expected $ExpectedExitCode"
  if ($null -ne $ExpectedOutput) {
    Assert-BytesEqual -Actual $result.StandardOutput `
      -Expected $ExpectedOutput -Message "$Name standard output"
    Assert-EmptyBytes -Actual $result.StandardError `
      -Message "$Name standard error"
  } elseif ($DiagnosticPrefix -ne '') {
    Assert-EmptyBytes -Actual $result.StandardOutput `
      -Message "$Name standard output"
    Assert-Diagnostic -Actual $result.StandardError `
      -Prefix $DiagnosticPrefix -Message "$Name standard error"
  } else {
    Assert-EmptyBytes -Actual $result.StandardOutput `
      -Message "$Name standard output"
    Assert-EmptyBytes -Actual $result.StandardError `
      -Message "$Name standard error"
  }
  $script:caseCount++
}

foreach ($executable in @($Learner, $Validator, $Runner)) {
  Assert-Condition (Test-Path -LiteralPath $executable -PathType Leaf) `
    "required executable is missing: $executable"
}
Assert-Condition (-not (Test-Path -LiteralPath $missingInput)) `
  "missing-input sentinel unexpectedly exists: $missingInput"

foreach ($profile in @('adjacency1d', 'adjacency2d', 'pattern2d', 'sequence')) {
  $training = Join-Path $fixtureDirectory "$profile.wfclearn"
  $recipe = Join-Path $fixtureDirectory "$profile.wfcpipeline"
  $model = Join-Path $fixtureDirectory "$profile.model"
  $run = Join-Path $fixtureDirectory "$profile.wfcrun"
  $result = Join-Path $fixtureDirectory "$profile.wfcresult"
  $trainingBytes = [System.IO.File]::ReadAllBytes($training)
  $recipeBytes = [System.IO.File]::ReadAllBytes($recipe)
  $modelBytes = [System.IO.File]::ReadAllBytes($model)
  $resultBytes = [System.IO.File]::ReadAllBytes($result)

  Check-Case -Executable $Learner -Arguments @($training) `
    -StandardInput $emptyInput -ExpectedExitCode 0 `
    -ExpectedOutput $recipeBytes -DiagnosticPrefix '' -Name "$profile file training"
  Check-Case -Executable $Learner -Arguments @('-') `
    -StandardInput $trainingBytes -ExpectedExitCode 0 `
    -ExpectedOutput $recipeBytes -DiagnosticPrefix '' -Name "$profile stdin training"
  Check-Case -Executable $Learner -Arguments @('--model', $training) `
    -StandardInput $emptyInput -ExpectedExitCode 0 `
    -ExpectedOutput $modelBytes -DiagnosticPrefix '' -Name "$profile standalone model"
  Check-Case -Executable $Validator -Arguments @('recipe', '--emit-canonical', $recipe) `
    -StandardInput $emptyInput -ExpectedExitCode 0 `
    -ExpectedOutput $recipeBytes -DiagnosticPrefix '' -Name "$profile recipe validation"
  Check-Case -Executable $Runner -Arguments @($recipe, $run) `
    -StandardInput $emptyInput -ExpectedExitCode 0 `
    -ExpectedOutput $resultBytes -DiagnosticPrefix '' -Name "$profile recipe execution"
}
Check-Case -Executable $Learner -Arguments @('--quiet', $training) `
  -StandardInput $emptyInput -ExpectedExitCode 0 -ExpectedOutput $emptyInput `
  -DiagnosticPrefix '' -Name 'quiet training'
Check-Case -Executable $Learner -Arguments @('--version') `
  -StandardInput $emptyInput -ExpectedExitCode 0 `
  -ExpectedOutput ($ascii.GetBytes("wfc-learn 1 (wfclearn=1)`n")) `
  -DiagnosticPrefix '' -Name 'learner version'
Check-Case -Executable $Learner -Arguments @($missingInput) `
  -StandardInput $emptyInput -ExpectedExitCode 3 -ExpectedOutput $null `
  -DiagnosticPrefix 'wfc-learn: I/O error: cannot read INPUT: ' -Name 'missing training'
Check-Case -Executable $Learner -Arguments @('-') `
  -StandardInput ($ascii.GetBytes("not-training`n")) `
  -ExpectedExitCode 1 -ExpectedOutput $null `
  -DiagnosticPrefix 'wfc-learn: invalid training: ' -Name 'invalid training'
Check-Case -Executable $Learner -Arguments @('--model', '--quiet', $training) `
  -StandardInput $emptyInput -ExpectedExitCode 2 -ExpectedOutput $null `
  -DiagnosticPrefix 'wfc-learn: usage error: ' -Name 'conflicting output options'

Write-Host "Training CLI process conformance: $caseCount cases passed."
