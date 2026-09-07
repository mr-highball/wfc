#!/usr/bin/env bash

set -u
set -o pipefail

repository_root=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
fixture_directory="$repository_root/test/fixtures/pipeline-cli"
recipe="$fixture_directory/recipe.wfcpipeline"
solved_run="$fixture_directory/solved.wfcrun"
solved_result="$fixture_directory/solved.wfcresult"
nonsolved_run="$fixture_directory/nonsolved.wfcrun"
nonsolved_result="$fixture_directory/nonsolved.wfcresult"
learned_fixture_directory="$repository_root/examples/2D/05_LearnedPatternWorld/pipeline"
learned_recipe="$learned_fixture_directory/recipe.wfcpipeline"
learned_run="$learned_fixture_directory/run.wfcrun"
learned_result="$learned_fixture_directory/result.wfcresult"
missing_input="$fixture_directory/missing.wfcpipeline"
maximum_captured_length=8192
process_timeout_seconds=30

validator=()
runner=()
active_command=validator
for argument in "$@"
do
  if [[ "$active_command" == validator && "$argument" == -- ]]; then
    active_command=runner
  elif [[ "$active_command" == validator ]]; then
    validator+=("$argument")
  else
    runner+=("$argument")
  fi
done

if (( ${#validator[@]} == 0 || ${#runner[@]} == 0 )); then
  printf '%s\n' \
    'usage: wfc_pipeline_cli_process_test.sh VALIDATOR [ARG...] -- RUNNER [ARG...]' >&2
  exit 2
fi

for required_file in \
  "$recipe" "$solved_run" "$solved_result" \
  "$nonsolved_run" "$nonsolved_result" \
  "$learned_recipe" "$learned_run" "$learned_result"
do
  if [[ ! -f "$required_file" ]]; then
    printf 'Pipeline CLI process check failed: required file is missing: %s\n' \
      "$required_file" >&2
    exit 1
  fi
done
if [[ -e "$missing_input" ]]; then
  printf 'Pipeline CLI process check failed: sentinel unexpectedly exists: %s\n' \
    "$missing_input" >&2
  exit 1
fi

argument_recipe=$recipe
argument_solved_run=$solved_run
argument_nonsolved_run=$nonsolved_run
argument_learned_recipe=$learned_recipe
argument_learned_run=$learned_run
argument_missing_input=$missing_input
case "$(uname -s)" in
  CYGWIN*|MINGW*|MSYS*)
    argument_recipe=$(cygpath -m "$recipe") || exit $?
    argument_solved_run=$(cygpath -m "$solved_run") || exit $?
    argument_nonsolved_run=$(cygpath -m "$nonsolved_run") || exit $?
    argument_learned_recipe=$(cygpath -m "$learned_recipe") || exit $?
    argument_learned_run=$(cygpath -m "$learned_run") || exit $?
    argument_missing_input=$(cygpath -m "$missing_input") || exit $?
    ;;
esac

temporary_parent="$repository_root/build/cli-process-test"
mkdir -p -- "$temporary_parent" || exit $?
temporary_directory=$(mktemp -d "$temporary_parent/run.XXXXXX") || exit $?
standard_output="$temporary_directory/stdout"
standard_error="$temporary_directory/stderr"
expected_summary="$temporary_directory/summary"
expected_learned_summary="$temporary_directory/learned-summary"
invalid_recipe="$temporary_directory/invalid-recipe"
producer_error="$temporary_directory/producer-stderr"
producer_pid_file="$temporary_directory/producer-pid"

cleanup() {
  rm -f -- \
    "$standard_output" "$standard_error" \
    "$expected_summary" "$expected_learned_summary" "$invalid_recipe" \
    "$producer_error" "$producer_pid_file"
  rmdir -- "$temporary_directory" 2>/dev/null || true
}
trap cleanup EXIT HUP INT TERM

printf '%s\n' \
  'valid canonical wfcpipeline=1 signature=A8FD55BC resources=1 passes=3 dependencies=2 bridges=0 requirements=0' \
  >"$expected_summary" || exit $?
printf '%s\n' \
  'valid canonical wfcpipeline=1 signature=DC2030BE resources=3 passes=4 dependencies=3 bridges=1 requirements=7' \
  >"$expected_learned_summary" || exit $?
printf 'not-a-recipe\n' >"$invalid_recipe" || exit $?

case_count=0

fail() {
  printf 'Pipeline CLI process check failed: %s\n' "$1" >&2
  exit 1
}

assert_capture_bounds() {
  local output_length error_length
  output_length=$(wc -c <"$standard_output") || exit $?
  error_length=$(wc -c <"$standard_error") || exit $?
  (( output_length <= maximum_captured_length )) || \
    fail 'standard output exceeded the process-fixture capture limit'
  (( error_length <= maximum_captured_length )) || \
    fail 'standard error exceeded the process-fixture capture limit'
}

assert_exact_output() {
  local expected=$1 name=$2
  cmp -s -- "$standard_output" "$expected" || \
    fail "$name standard output differs from its exact fixture"
  [[ ! -s "$standard_error" ]] || fail "$name wrote unexpected standard error"
}

assert_empty_output() {
  local name=$1
  [[ ! -s "$standard_output" ]] || fail "$name wrote unexpected standard output"
  [[ ! -s "$standard_error" ]] || fail "$name wrote unexpected standard error"
}

assert_diagnostic() {
  local prefix=$1 name=$2 diagnostic error_length line_count
  local LC_ALL=C
  [[ ! -s "$standard_output" ]] || fail "$name wrote unexpected standard output"
  line_count=$(wc -l <"$standard_error") || exit $?
  (( line_count == 1 )) || fail "$name did not write one diagnostic line"
  diagnostic=$(<"$standard_error")
  [[ "$diagnostic" == "$prefix"* ]] || fail "$name wrote the wrong diagnostic class"
  error_length=$(wc -c <"$standard_error") || exit $?
  (( error_length == ${#diagnostic} + 1 )) || \
    fail "$name diagnostic does not end at its sole line feed"
  if LC_ALL=C grep -q $'\r' "$standard_error"; then
    fail "$name diagnostic contains a carriage return"
  fi
}

run_case() {
  local expected_status=$1 input_file=$2
  local producer_pid=''
  local command_running deadline producer_running timed_out=0
  shift 2
  : >"$standard_output" || exit $?
  : >"$standard_error" || exit $?
  rm -f -- "$producer_error" "$producer_pid_file"
  if [[ -n "$input_file" ]]; then
    bash -c \
      'printf "%s\n" "$$" >"$1"; exec cat -- "$2"' \
      _ "$producer_pid_file" "$input_file" 2>"$producer_error" |
      "$@" >"$standard_output" 2>"$standard_error" &
  else
    "$@" </dev/null >"$standard_output" 2>"$standard_error" &
  fi
  local command_pid=$!
  deadline=$((SECONDS + process_timeout_seconds))
  if [[ -n "$input_file" ]]; then
    while [[ ! -s "$producer_pid_file" ]]
    do
      if (( SECONDS >= deadline )); then
        kill -KILL "$command_pid" 2>/dev/null || true
        wait "$command_pid" 2>/dev/null || true
        fail 'standard-input producer did not report its process id'
      fi
      sleep 1
    done
    IFS= read -r producer_pid <"$producer_pid_file" || \
      fail 'could not read the standard-input producer process id'
    case "$producer_pid" in
      ''|*[!0-9]*)
        kill -KILL "$command_pid" 2>/dev/null || true
        wait "$command_pid" 2>/dev/null || true
        fail 'standard-input producer reported an invalid process id'
        ;;
    esac
  fi
  while :
  do
    command_running=0
    producer_running=0
    if kill -0 "$command_pid" 2>/dev/null; then
      command_running=1
    fi
    if [[ -n "$producer_pid" ]] && kill -0 "$producer_pid" 2>/dev/null; then
      producer_running=1
    fi
    if (( command_running == 0 && producer_running == 0 )); then
      break
    fi
    if (( SECONDS >= deadline )); then
      timed_out=1
      kill -KILL "$command_pid" 2>/dev/null || true
      if [[ -n "$producer_pid" ]]; then
        kill -KILL "$producer_pid" 2>/dev/null || true
      fi
      break
    fi
    sleep 1
  done
  wait "$command_pid"
  local actual_status=$?
  (( timed_out == 0 )) || fail 'command exceeded the 30-second timeout'
  if [[ -n "$input_file" ]]; then
    if kill -0 "$producer_pid" 2>/dev/null; then
      fail 'standard-input producer remained live after its pipeline was reaped'
    fi
    [[ ! -s "$producer_error" ]] || \
      fail 'standard-input producer wrote unexpected standard error'
  fi
  assert_capture_bounds
  [[ "$actual_status" == "$expected_status" ]] || \
    fail "command returned $actual_status, expected $expected_status"
  ((case_count += 1))
}

run_case 0 '' "${validator[@]}" recipe "$argument_recipe"
assert_exact_output "$expected_summary" 'validator file summary'
run_case 0 "$recipe" "${validator[@]}" recipe -
assert_exact_output "$expected_summary" 'validator stdin summary'
run_case 0 '' "${validator[@]}" recipe --emit-canonical "$argument_recipe"
assert_exact_output "$recipe" 'validator canonical output'
run_case 0 '' "${validator[@]}" recipe --quiet "$argument_recipe"
assert_empty_output 'validator quiet output'
run_case 3 '' "${validator[@]}" recipe "$argument_missing_input"
assert_diagnostic 'wfc-validate: I/O error: cannot read INPUT: ' \
  'validator missing input'
run_case 1 "$invalid_recipe" "${validator[@]}" recipe -
assert_diagnostic 'wfc-validate: invalid recipe: ' 'validator invalid recipe'
run_case 0 '' "${validator[@]}" recipe "$argument_learned_recipe"
assert_exact_output "$expected_learned_summary" \
  'validator learned-pattern bundle'
run_case 0 '' "${validator[@]}" recipe --emit-canonical \
  "$argument_learned_recipe"
assert_exact_output "$learned_recipe" \
  'validator learned-pattern canonical output'

run_case 0 '' "${runner[@]}" "$argument_recipe" "$argument_solved_run"
assert_exact_output "$solved_result" 'runner solved file input'
run_case 4 '' "${runner[@]}" "$argument_recipe" "$argument_nonsolved_run"
assert_exact_output "$nonsolved_result" 'runner non-solved file input'
run_case 0 "$recipe" "${runner[@]}" - "$argument_solved_run"
assert_exact_output "$solved_result" 'runner recipe stdin'
run_case 0 "$solved_run" "${runner[@]}" "$argument_recipe" -
assert_exact_output "$solved_result" 'runner run stdin'
run_case 0 '' "${runner[@]}" --quiet "$argument_recipe" "$argument_solved_run"
assert_empty_output 'runner quiet solved output'
run_case 4 '' "${runner[@]}" --quiet "$argument_recipe" "$argument_nonsolved_run"
assert_empty_output 'runner quiet non-solved output'
run_case 0 '' "${runner[@]}" "$argument_learned_recipe" "$argument_learned_run"
assert_exact_output "$learned_result" 'runner learned-pattern bundle'
run_case 3 '' "${runner[@]}" "$argument_missing_input" "$argument_solved_run"
assert_diagnostic 'wfc-run: I/O error: cannot read RECIPE: ' \
  'runner missing recipe'
run_case 1 "$invalid_recipe" "${runner[@]}" - "$argument_solved_run"
assert_diagnostic 'wfc-run: invalid recipe: ' 'runner invalid recipe'
run_case 2 '' "${runner[@]}" - -
assert_diagnostic \
  'wfc-run: usage error: only one of RECIPE and RUN may be standard input' \
  'runner dual stdin'

printf 'Pipeline CLI process conformance: %d cases passed.\n' "$case_count"
