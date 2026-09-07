#!/usr/bin/env bash
set -euo pipefail
repository_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd "$repository_root"
server="${WFC_SERVE:-$repository_root/build/native/bin/wfc_serve}"
checker="${WFC_BROWSER_CHECK:-$repository_root/build/native/bin/wfc_browser_check}"
capture="${WFC_BROWSER_CAPTURE:-$repository_root/build/native/bin/wfc_browser_capture}"
port="${WFC_BROWSER_PORT:-4180}"
[[ "$port" =~ ^[1-9][0-9]{0,4}$ && "$port" -le 65535 ]] || { echo 'WFC_BROWSER_PORT must be 1..65535.' >&2; exit 1; }
standalone_root=''
standalone_page=''
standalone_requested=false
standalone_expectations=()
while [[ $# -gt 0 ]]; do
  option=$1
  shift
  if [[ "$option" == --help ]]; then
    printf '%s\n' 'Usage: test-browser-tests.sh [--root DIRECTORY --page RELATIVE-PAGE --expect NAME=VALUE ...]' \
      'Without options, run all current browser conformance sources (or WFC_BROWSER_TEST).' \
      'Standalone pages always require data-self-test=passed; WFC_BROWSER_PORT defaults to 4180.'
    exit 0
  fi
  [[ $# -gt 0 ]] || { printf 'Missing value for %s.\n' "$option" >&2; exit 1; }
  value=$1
  shift
  standalone_requested=true
  case "$option" in
    --root)
      [[ -z "$standalone_root" ]] || { echo 'Duplicate --root.' >&2; exit 1; }
      standalone_root=$value ;;
    --page)
      [[ -z "$standalone_page" ]] || { echo 'Duplicate --page.' >&2; exit 1; }
      standalone_page=$value ;;
    --expect)
      [[ "$value" =~ ^[a-z0-9_:-]+= ]] || { echo 'Expected lowercase ASCII NAME=VALUE.' >&2; exit 1; }
      if [[ "$value" == data-self-test=* ]]; then
        [[ "$value" == data-self-test=passed ]] || { echo 'data-self-test must be expected to pass.' >&2; exit 1; }
      else standalone_expectations+=(--expect "$value"); fi ;;
    *) printf 'Unknown option: %s.\n' "$option" >&2; exit 1 ;;
  esac
done
standalone=false
if [[ "$standalone_requested" == true ]]; then
  [[ -n "$standalone_root" && -n "$standalone_page" ]] || { echo '--root and --page are required together.' >&2; exit 1; }
  [[ -z "${WFC_BROWSER_TEST:-}" ]] || { echo 'Standalone options and WFC_BROWSER_TEST are mutually exclusive.' >&2; exit 1; }
  # Paths are deliberately simple relative ASCII paths, with an optional query.
  # No authority, escaped path, traversal, fragment or backslash is accepted.
  page_pattern='^[a-zA-Z0-9_-][a-zA-Z0-9_./-]*(\?[a-zA-Z0-9_.~=&%+-]*)?$'
  page_path=${standalone_page%%\?*}
  [[ "$standalone_page" =~ $page_pattern && "/$page_path/" != */../* && "/$page_path/" != */./* && "$page_path" != *//* ]] || {
    echo 'Standalone --page must be a simple relative path with no traversal.' >&2; exit 1;
  }
  [[ -d "$standalone_root" ]] || { echo 'Standalone root directory does not exist.' >&2; exit 1; }
  standalone_root="$(cd -- "$standalone_root" && pwd)"
  [[ -f "$standalone_root/$page_path" ]] || { echo 'Standalone page does not exist under its root.' >&2; exit 1; }
  standalone=true
fi
chrome="${WFC_BROWSER:-}"
if [[ -z "$chrome" ]]; then
  for candidate in google-chrome google-chrome-stable chromium chromium-browser; do
    if command -v "$candidate" >/dev/null 2>&1; then chrome="$(command -v "$candidate")"; break; fi
  done
fi
[[ -n "$chrome" ]] || { echo 'Set WFC_BROWSER to a Chromium-family browser executable.' >&2; exit 1; }
web='build/browser/tests/www'
results='build/browser/tests/results'
sources=()
missing=()
if [[ "$standalone" == true ]]; then
  web=$standalone_root
  sources=(standalone-demo)
else
 for source in test/*_test.lpr; do
  [[ -f "$source" ]] || continue
  name="${source##*/}"
  name="${name%.lpr}"
  case "$name" in wfc_pipeline_prepare_threads_test) continue ;; esac
  case "$name" in pipeline_workspace_native_fixture|pipeline_workspace_native_process_test) continue ;; esac
  case "$name" in wfc_workspace_cli_process_test|wfc_workspace_cli_fixture) continue ;; esac
  case "$name" in wfc_package_check_process_test|wfc_artifact_cli_process_test|wfc_ensemble_http_process_test|wfc_mapped_world_process_test|wfc_pipeline_mapped_process_test) continue ;; esac
  if [[ -n "${WFC_BROWSER_TEST:-}" && "$name" != "$WFC_BROWSER_TEST" ]]; then continue; fi
  case "$name" in wfc_browser_args_test|wfc_browser_dom_test|wfc_browser_socket_test|wfc_browser_websocket_test|wfc_browser_cdp_test|wfc_browser_capture_test|wfc_serve_test|wfc_music_render_process_test|wfc_music_ensemble_render_process_test|wfc_music_ensemble_midi_render_process_test|wfc_music_voices_render_process_test|wfc_connectivity_process_test|wfc_music_studies_process_test) continue ;; esac
  sources+=("$name")
  for extension in html js; do
    [[ -f "$web/$name.$extension" ]] || missing+=("$web/$name.$extension")
  done
 done
fi
[[ "${#sources[@]}" -gt 0 ]] || { echo 'No current browser conformance sources found.' >&2; exit 1; }
if [[ "${#missing[@]}" -gt 0 ]]; then
  echo 'Browser conformance staging is incomplete; run build-browser-tests.sh.' >&2
  printf 'Missing: %s\n' "${missing[@]}" >&2
  exit 1
fi
# Old staging files cannot replace missing current tests or inflate this count.
mkdir -p "$results"
server_pid=''
browser_pid=''
capture_pid=''
watchdog_pid=''
terminate_owned_child() {
  local child_pid=$1 child_name=$2 child_status=0 term_polls=20 child_poll
  # Callers supply only PIDs from this invocation's background launches.
  # These are bounded direct-child polls, not process-tree containment or a
  # hard wall-clock guarantee under arbitrary scheduler delays. The watchdog
  # gets six seconds to finish its own normally four-second timer teardown.
  if [[ "$child_name" == watchdog ]]; then term_polls=60; fi
  [[ "$child_pid" =~ ^[1-9][0-9]*$ && "$child_pid" != 1 ]] || {
    printf 'Refusing invalid owned %s PID: %s.\n' "$child_name" "$child_pid" >&2
    return 1
  }
  if kill -0 "$child_pid" 2>/dev/null; then
    kill "$child_pid" 2>/dev/null || true
    for ((child_poll=0; child_poll<term_polls; child_poll++)); do
      if ! kill -0 "$child_pid" 2>/dev/null; then break; fi
      sleep 0.1
    done
    if kill -0 "$child_pid" 2>/dev/null; then
      kill -KILL "$child_pid" 2>/dev/null || true
      for _ in {1..20}; do
        if ! kill -0 "$child_pid" 2>/dev/null; then break; fi
        sleep 0.1
      done
    fi
  fi
  if kill -0 "$child_pid" 2>/dev/null; then
    printf 'Cleanup deadline exceeded for owned %s PID %s.\n' "$child_name" "$child_pid" >&2
    return 1
  fi
  # Reap only after liveness is gone; never wait indefinitely after TERM.
  wait "$child_pid" 2>/dev/null || child_status=$?
  # A cooperatively canceled watchdog exits zero only after its timer cleanup
  # succeeds. Do not hide timer failures or a forced watchdog KILL; ordinary
  # terminated browser/server exit codes do not themselves mean cleanup failed.
  if [[ "$child_name" == watchdog && "$child_status" -ne 0 ]]; then
    printf 'Owned watchdog teardown failed (exit %s).\n' "$child_status" >&2
    return 1
  fi
  return 0
}
stop_watchdog() {
  if [[ -n "$watchdog_pid" ]]; then
    terminate_owned_child "$watchdog_pid" watchdog || return 1
    watchdog_pid=''
  fi
}
cleanup() {
  local cleanup_status=$? cleanup_failed=0
  trap - EXIT
  trap '' INT TERM
  stop_watchdog || cleanup_failed=1
  if [[ -n "${capture_pid:-}" ]]; then
    terminate_owned_child "$capture_pid" capture || cleanup_failed=1
  fi
  if [[ -n "$browser_pid" ]]; then
    terminate_owned_child "$browser_pid" browser || cleanup_failed=1
  fi
  if [[ -n "$server_pid" ]]; then
    terminate_owned_child "$server_pid" server || cleanup_failed=1
  fi
  if (( cleanup_status == 0 && cleanup_failed != 0 )); then cleanup_status=1; fi
  exit "$cleanup_status"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
# The parent can reach readiness before the background child's redirection runs.
: >"$results/server.log"
"$server" --root "$web" --port "$port" >"$results/server.log" 2>&1 &
server_pid=$!
ready=false
for _ in {1..50}; do
  kill -0 "$server_pid" 2>/dev/null || { cat "$results/server.log" >&2; exit 1; }
  bound=false
  while IFS= read -r line; do
    line=${line%$'\r'}
    if [[ "$line" == "WFC static server: http://127.0.0.1:$port/" ]] ||
        [[ "$standalone" == true && "$line" == "WFC development server: http://127.0.0.1:$port/" ]]; then
      bound=true
    fi
  done <"$results/server.log"
  ready_page="${sources[0]}.html"
  if [[ "$standalone" == true ]]; then ready_page=$standalone_page; fi
  if [[ "$bound" == true ]] &&
      curl --max-time 2 --silent --fail "http://127.0.0.1:$port/$ready_page" >/dev/null; then
    kill -0 "$server_pid" 2>/dev/null || { cat "$results/server.log" >&2; exit 1; }
    ready=true
    break
  fi
  sleep 0.2
done
[[ "$ready" == true ]] || { echo 'FPC server did not become ready.' >&2; exit 1; }
count=0
failures=()
for name in "${sources[@]}"; do
  kill -0 "$server_pid" 2>/dev/null || { echo 'FPC server exited during conformance checks.' >&2; exit 1; }
  printf 'Checking %s/%s: %s.\n' "$((count+1))" "${#sources[@]}" "$name"
  # Fixed aliases are only published after this run's independent checker
  # passes. Preserve every unique run directory, including failed diagnostics.
  rm -f -- "$results/$name.dom"
  run_dir="$(mktemp -d "$repository_root/$results/$name.XXXXXXXX")"
  profile="$run_dir/profile"
  dom="$run_dir/result.dom"
  timeout_marker="$run_dir/timeout"
  : >"$timeout_marker"
  expectations=(--expect data-self-test=passed)
  page="$name.html"
  if [[ "$standalone" == true ]]; then
    page=$standalone_page
    if [[ "${#standalone_expectations[@]}" -gt 0 ]]; then
      expectations+=("${standalone_expectations[@]}")
    fi
  fi
  if [[ "$name" == wfc_browser_demo_entries_test ]]; then
    expectations+=(--expect data-demo-entries-self-test=passed)
    expectations+=(--expect data-demo-entries-count=12)
  fi
  if [[ "$standalone" == false && "$name" == pipeline_workspace_ui_test ]]; then
    # Generic harness completion can precede queued UI work. Require the real
    # asynchronous terminal contract and all fourteen independent stage markers.
    expectations+=(--expect data-workspace-ui-self-test=passed)
    expectations+=(--expect data-workspace-ui-stage-count=14)
    expectations+=(--expect data-workspace-ui-current=complete)
    for stage in startup cancelled-queue preset-drafts initial-once \
      draft-isolation seed-epochs cell-inputs failed-repair private-scope \
      view-window geometry journal-claims file-import restored-history; do
      expectations+=(--expect "data-workspace-ui-$stage=passed")
    done
  fi
  if [[ "$name" == wfc_music_ensemble_stream_demo_test ]]; then
    # Awaited file transactions have their own application completion signal.
    expectations+=(--expect data-stream-self-test=passed)
    expectations+=(--expect data-stream-release=passed)
    expectations+=(--expect data-midi-stream-self-test=passed)
    expectations+=(--expect data-midi-stream-release=passed)
    expectations+=(--expect data-http-stream-self-test=passed)
  fi
  if [[ "$name" == wfc_music_ensemble_demo_test ]]; then
    expectations+=(--expect data-developed-profile=passed)
  fi
  if [[ "$name" == wfc_music_voices_browser_test ]]; then
    expectations+=(--expect data-voice-stream-self-test=passed)
    expectations+=(--expect data-voice-stream-release=passed)
  fi
  if ! "$capture" --prepare-profile "$profile" >"$run_dir/prepare.log" 2>&1; then
    cat "$run_dir/prepare.log" >&2
    exit 1
  fi
  # Native monotonic ticks are shared by discovery and every protocol request.
  # Establish the unchanged real 60-second budget before starting Chromium.
  if ! "$capture" --deadline-after 60000 >"$run_dir/deadline.log" 2>&1; then
    cat "$run_dir/deadline.log" >&2
    exit 1
  fi
  IFS= read -r deadline <"$run_dir/deadline.log"
  deadline=${deadline%$'\r'}
  [[ "$deadline" =~ ^[1-9][0-9]*$ ]] || { echo 'Native capture returned an invalid deadline.' >&2; exit 1; }
  "$chrome" --headless --disable-gpu --disable-dev-shm-usage \
    --no-first-run --no-default-browser-check --user-data-dir="$profile" \
    --remote-debugging-address=127.0.0.1 --remote-debugging-port=0 \
    about:blank >"$run_dir/browser.stdout.log" 2>"$run_dir/browser.log" &
  browser_pid=$!
  "$capture" --profile "$profile" --url "http://127.0.0.1:$port/$page" \
    --dom "$dom" --deadline "$deadline" "${expectations[@]}" >"$run_dir/capture.log" 2>&1 &
  capture_pid=$!
  # POSIX sleep/kill watchdog: no GNU timeout utility is needed on macOS.
  # Only the capture/browser PIDs recorded by this invocation are signalled.
  # Cancel the timer when capture returns; Chromium itself stays alive for CDP.
  # Keep the native absolute sixty-second deadline unchanged. Allow five more
  # seconds only for its timeout exception, cleanup and complete diagnostic to
  # exit before forced containment; this cannot extend native page or I/O time.
  capture_teardown_grace_seconds=5
  (
    trap - EXIT
    timer_pid=''
    trap 'trap "" TERM INT; if [[ -n "$timer_pid" ]]; then terminate_owned_child "$timer_pid" watchdog-timer || exit 1; fi; exit 0' TERM INT
    sleep "$((60 + capture_teardown_grace_seconds))" &
    timer_pid=$!
    wait "$timer_pid" || exit 0
    timer_pid=''
    printf 'deadline exceeded\n' >"$timeout_marker"
    printf 'Browser timed out: %s.\n' "$name" >&2
    kill "$capture_pid" "$browser_pid" 2>/dev/null || true
    sleep 2 &
    timer_pid=$!
    wait "$timer_pid" || exit 0
    timer_pid=''
    kill -KILL "$capture_pid" "$browser_pid" 2>/dev/null || true
  ) &
  watchdog_pid=$!
  capture_status=0
  wait "$capture_pid" || capture_status=$?
  capture_pid=''
  cat "$run_dir/capture.log"
  stop_watchdog
  terminate_owned_child "$browser_pid" browser
  browser_pid=''
  if [[ -s "$timeout_marker" ]]; then
    failures+=("$name")
  elif [[ "$capture_status" -ne 0 ]]; then
    printf 'Browser capture failed: %s (exit %s); evidence: %s.\n' "$name" "$capture_status" "$run_dir" >&2
    failures+=("$name")
  elif ! "$checker" --dom "$dom" "${expectations[@]}" >"$run_dir/checker.log" 2>&1; then
    cat "$run_dir/checker.log" >&2
    printf 'Browser assertions failed: %s.\n' "$name" >&2
    failures+=("$name")
  else
    cat "$run_dir/checker.log"
    cp -- "$dom" "$results/$name.dom"
  fi
  count=$((count+1))
done
if [[ "${#failures[@]}" -gt 0 ]]; then
  printf 'Browser conformance failed (%s/%s):\n' "${#failures[@]}" "$count" >&2
  printf '  %s\n' "${failures[@]}" >&2
  exit 1
fi
echo "Pascal conformance passed in the browser: $count programs."
