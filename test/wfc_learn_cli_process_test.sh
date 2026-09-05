#!/usr/bin/env bash

set -u
set -o pipefail

repository_root=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
fixture_directory="$repository_root/examples/learning/04_TrainingDocuments"
missing_input="$fixture_directory/missing.wfclearn"
maximum_captured_length=8192
process_timeout_seconds=30

learner=()
validator=()
runner=()
active_command=learner
for argument in "$@"
do
  if [[ "$argument" == -- && "$active_command" == learner ]]; then
    active_command=validator
  elif [[ "$argument" == -- && "$active_command" == validator ]]; then
    active_command=runner
  elif [[ "$active_command" == learner ]]; then
    learner+=("$argument")
  elif [[ "$active_command" == validator ]]; then
    validator+=("$argument")
  else
    runner+=("$argument")
  fi
done
if (( ${#learner[@]} == 0 || ${#validator[@]} == 0 || ${#runner[@]} == 0 )); then
  printf '%s\n' \
    'usage: wfc_learn_cli_process_test.sh LEARNER [ARG...] -- VALIDATOR [ARG...] -- RUNNER [ARG...]' >&2
  exit 2
fi
if [[ -e "$missing_input" ]]; then
  printf 'Training CLI process check failed: sentinel unexpectedly exists: %s\n' \
    "$missing_input" >&2
  exit 1
fi

argument_path() {
  case "$(uname -s)" in
    CYGWIN*|MINGW*|MSYS*) cygpath -m "$1" ;;
    *) printf '%s\n' "$1" ;;
  esac
}
argument_missing_input=$(argument_path "$missing_input") || exit $?

temporary_parent="$repository_root/build/cli-process-test"
mkdir -p -- "$temporary_parent" || exit $?
temporary_directory=$(mktemp -d "$temporary_parent/training.XXXXXX") || exit $?
standard_output="$temporary_directory/stdout"
standard_error="$temporary_directory/stderr"
expected_version="$temporary_directory/version"
invalid_training="$temporary_directory/invalid-training"
producer_error="$temporary_directory/producer-stderr"
producer_pid_file="$temporary_directory/producer-pid"

cleanup() {
  rm -f -- "$standard_output" "$standard_error" "$expected_version" \
    "$invalid_training" "$producer_error" "$producer_pid_file"
  rmdir -- "$temporary_directory" 2>/dev/null || true
}
trap cleanup EXIT HUP INT TERM
printf 'wfc-learn 2 (wfclearn=1,2)\n' >"$expected_version" || exit $?
printf 'not-training\n' >"$invalid_training" || exit $?
case_count=0

fail() {
  printf 'Training CLI process check failed: %s\n' "$1" >&2
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
  [[ "$line_count" == 1 ]] || fail "$name did not write one diagnostic line"
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

for profile in adjacency1d adjacency2d pattern2d sequence adjacency3d
do
  training="$fixture_directory/$profile.wfclearn"
  recipe="$fixture_directory/$profile.wfcpipeline"
  model="$fixture_directory/$profile.model"
  run="$fixture_directory/$profile.wfcrun"
  result="$fixture_directory/$profile.wfcresult"
  for required_file in "$training" "$recipe" "$model" "$run" "$result"
  do
    [[ -f "$required_file" ]] || fail "required file is missing: $required_file"
  done
  argument_training=$(argument_path "$training") || exit $?
  argument_recipe=$(argument_path "$recipe") || exit $?
  argument_run=$(argument_path "$run") || exit $?
  run_case 0 '' "${learner[@]}" "$argument_training"
  assert_exact_output "$recipe" "$profile file training"
  run_case 0 "$training" "${learner[@]}" -
  assert_exact_output "$recipe" "$profile stdin training"
  run_case 0 '' "${learner[@]}" --model "$argument_training"
  assert_exact_output "$model" "$profile standalone model"
  run_case 0 '' "${validator[@]}" recipe --emit-canonical "$argument_recipe"
  assert_exact_output "$recipe" "$profile recipe validation"
  run_case 0 '' "${runner[@]}" "$argument_recipe" "$argument_run"
  assert_exact_output "$result" "$profile recipe execution"
done
run_case 0 '' "${learner[@]}" --quiet "$argument_training"
assert_empty_output 'quiet training'
run_case 0 '' "${learner[@]}" --version
assert_exact_output "$expected_version" 'learner version'
run_case 3 '' "${learner[@]}" "$argument_missing_input"
assert_diagnostic 'wfc-learn: I/O error: cannot read INPUT: ' 'missing training'
run_case 1 "$invalid_training" "${learner[@]}" -
assert_diagnostic 'wfc-learn: invalid training: ' 'invalid training'
run_case 2 '' "${learner[@]}" --model --quiet "$argument_training"
assert_diagnostic 'wfc-learn: usage error: ' 'conflicting output options'

printf 'Training CLI process conformance: %d cases passed.\n' "$case_count"
