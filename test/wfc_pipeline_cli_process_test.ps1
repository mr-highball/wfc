[CmdletBinding()]
param(
  [Parameter(Mandatory = $true)]
  [ValidateNotNullOrEmpty()]
  [string] $Validator,

  [Parameter(Mandatory = $true)]
  [ValidateNotNullOrEmpty()]
  [string] $Runner
)

$ErrorActionPreference = 'Stop'

$fixtureDirectory = Join-Path $PSScriptRoot 'fixtures/pipeline-cli'
$recipe = Join-Path $fixtureDirectory 'recipe.wfcpipeline'
$solvedRun = Join-Path $fixtureDirectory 'solved.wfcrun'
$solvedResult = Join-Path $fixtureDirectory 'solved.wfcresult'
$nonSolvedRun = Join-Path $fixtureDirectory 'nonsolved.wfcrun'
$nonSolvedResult = Join-Path $fixtureDirectory 'nonsolved.wfcresult'
$repositoryRoot = Split-Path -Parent $PSScriptRoot
$learnedFixtureDirectory = Join-Path $repositoryRoot `
  'examples/2D/05_LearnedPatternWorld/pipeline'
$learnedRecipe = Join-Path $learnedFixtureDirectory 'recipe.wfcpipeline'
$learnedRun = Join-Path $learnedFixtureDirectory 'run.wfcrun'
$learnedResult = Join-Path $learnedFixtureDirectory 'result.wfcresult'
$missingInput = Join-Path $fixtureDirectory 'missing.wfcpipeline'
$maximumCapturedLength = 8192
$processTimeoutMilliseconds = 30000
$ascii = [System.Text.Encoding]::ASCII
$emptyInput = [byte[]] @()
$invalidRecipe = $ascii.GetBytes("not-a-recipe`n")
$recipeBytes = [System.IO.File]::ReadAllBytes($recipe)
$solvedRunBytes = [System.IO.File]::ReadAllBytes($solvedRun)
$solvedResultBytes = [System.IO.File]::ReadAllBytes($solvedResult)
$nonSolvedResultBytes = [System.IO.File]::ReadAllBytes($nonSolvedResult)
$learnedRecipeBytes = [System.IO.File]::ReadAllBytes($learnedRecipe)
$learnedResultBytes = [System.IO.File]::ReadAllBytes($learnedResult)
$validatorSummaryBytes = $ascii.GetBytes(
  'valid canonical wfcpipeline=1 signature=A8FD55BC resources=1 ' +
  'passes=3 dependencies=2 bridges=0 requirements=0' + "`n")
$learnedValidatorSummaryBytes = $ascii.GetBytes(
  'valid canonical wfcpipeline=1 signature=DC2030BE resources=3 ' +
  'passes=4 dependencies=3 bridges=1 requirements=7' + "`n")
$caseCount = 0

function Assert-Condition {
  param(
    [bool] $Condition,
    [string] $Message
  )
  if (-not $Condition) {
    throw "Pipeline CLI process check failed: $Message"
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
      throw "Pipeline CLI process check failed: $Message " +
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
      throw "Pipeline CLI process check failed: '$Executable' timed out"
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

foreach ($requiredPath in @(
    $Validator, $Runner, $recipe, $solvedRun, $solvedResult,
    $nonSolvedRun, $nonSolvedResult, $learnedRecipe, $learnedRun,
    $learnedResult)) {
  Assert-Condition (Test-Path -LiteralPath $requiredPath -PathType Leaf) `
    "required file is missing: $requiredPath"
}
Assert-Condition (-not (Test-Path -LiteralPath $missingInput)) `
  "missing-input sentinel unexpectedly exists: $missingInput"

Check-Case -Executable $Validator -Arguments @('recipe', $recipe) `
  -StandardInput $emptyInput -ExpectedExitCode 0 `
  -ExpectedOutput $validatorSummaryBytes -DiagnosticPrefix '' `
  -Name 'validator file summary'
Check-Case -Executable $Validator -Arguments @('recipe', '-') `
  -StandardInput $recipeBytes -ExpectedExitCode 0 `
  -ExpectedOutput $validatorSummaryBytes -DiagnosticPrefix '' `
  -Name 'validator stdin summary'
Check-Case -Executable $Validator `
  -Arguments @('recipe', '--emit-canonical', $recipe) `
  -StandardInput $emptyInput -ExpectedExitCode 0 `
  -ExpectedOutput $recipeBytes -DiagnosticPrefix '' `
  -Name 'validator canonical output'
Check-Case -Executable $Validator `
  -Arguments @('recipe', '--quiet', $recipe) `
  -StandardInput $emptyInput -ExpectedExitCode 0 `
  -ExpectedOutput ([byte[]] @()) -DiagnosticPrefix '' `
  -Name 'validator quiet output'
Check-Case -Executable $Validator -Arguments @('recipe', $missingInput) `
  -StandardInput $emptyInput -ExpectedExitCode 3 -ExpectedOutput $null `
  -DiagnosticPrefix 'wfc-validate: I/O error: cannot read INPUT: ' `
  -Name 'validator missing input'
Check-Case -Executable $Validator -Arguments @('recipe', '-') `
  -StandardInput $invalidRecipe -ExpectedExitCode 1 -ExpectedOutput $null `
  -DiagnosticPrefix 'wfc-validate: invalid recipe: ' `
  -Name 'validator invalid recipe'
Check-Case -Executable $Validator -Arguments @('recipe', $learnedRecipe) `
  -StandardInput $emptyInput -ExpectedExitCode 0 `
  -ExpectedOutput $learnedValidatorSummaryBytes -DiagnosticPrefix '' `
  -Name 'validator learned-pattern bundle'
Check-Case -Executable $Validator `
  -Arguments @('recipe', '--emit-canonical', $learnedRecipe) `
  -StandardInput $emptyInput -ExpectedExitCode 0 `
  -ExpectedOutput $learnedRecipeBytes -DiagnosticPrefix '' `
  -Name 'validator learned-pattern canonical output'

Check-Case -Executable $Runner -Arguments @($recipe, $solvedRun) `
  -StandardInput $emptyInput -ExpectedExitCode 0 `
  -ExpectedOutput $solvedResultBytes -DiagnosticPrefix '' `
  -Name 'runner solved file input'
Check-Case -Executable $Runner -Arguments @($recipe, $nonSolvedRun) `
  -StandardInput $emptyInput -ExpectedExitCode 4 `
  -ExpectedOutput $nonSolvedResultBytes -DiagnosticPrefix '' `
  -Name 'runner non-solved file input'
Check-Case -Executable $Runner -Arguments @('-', $solvedRun) `
  -StandardInput $recipeBytes -ExpectedExitCode 0 `
  -ExpectedOutput $solvedResultBytes -DiagnosticPrefix '' `
  -Name 'runner recipe stdin'
Check-Case -Executable $Runner -Arguments @($recipe, '-') `
  -StandardInput $solvedRunBytes -ExpectedExitCode 0 `
  -ExpectedOutput $solvedResultBytes -DiagnosticPrefix '' `
  -Name 'runner run stdin'
Check-Case -Executable $Runner `
  -Arguments @('--quiet', $recipe, $solvedRun) `
  -StandardInput $emptyInput -ExpectedExitCode 0 `
  -ExpectedOutput ([byte[]] @()) -DiagnosticPrefix '' `
  -Name 'runner quiet solved output'
Check-Case -Executable $Runner `
  -Arguments @('--quiet', $recipe, $nonSolvedRun) `
  -StandardInput $emptyInput -ExpectedExitCode 4 `
  -ExpectedOutput ([byte[]] @()) -DiagnosticPrefix '' `
  -Name 'runner quiet non-solved output'
Check-Case -Executable $Runner -Arguments @($learnedRecipe, $learnedRun) `
  -StandardInput $emptyInput -ExpectedExitCode 0 `
  -ExpectedOutput $learnedResultBytes `
  -DiagnosticPrefix '' -Name 'runner learned-pattern bundle'
Check-Case -Executable $Runner `
  -Arguments @($missingInput, $solvedRun) `
  -StandardInput $emptyInput -ExpectedExitCode 3 -ExpectedOutput $null `
  -DiagnosticPrefix 'wfc-run: I/O error: cannot read RECIPE: ' `
  -Name 'runner missing recipe'
Check-Case -Executable $Runner -Arguments @('-', $solvedRun) `
  -StandardInput $invalidRecipe -ExpectedExitCode 1 -ExpectedOutput $null `
  -DiagnosticPrefix 'wfc-run: invalid recipe: ' `
  -Name 'runner invalid recipe'
Check-Case -Executable $Runner -Arguments @('-', '-') `
  -StandardInput $emptyInput -ExpectedExitCode 2 -ExpectedOutput $null `
  -DiagnosticPrefix `
    'wfc-run: usage error: only one of RECIPE and RUN may be standard input' `
  -Name 'runner dual stdin'

Write-Host "Pipeline CLI process conformance: $caseCount cases passed."
