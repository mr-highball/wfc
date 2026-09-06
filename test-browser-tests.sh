#!/usr/bin/env bash
set -euo pipefail
repository_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd "$repository_root"
server="${WFC_SERVE:-$repository_root/build/native/bin/wfc_serve}"
checker="${WFC_BROWSER_CHECK:-$repository_root/build/native/bin/wfc_browser_check}"
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
for source in test/*_test.lpr; do
  [[ -f "$source" ]] || continue
  name="${source##*/}"
  name="${name%.lpr}"
  if [[ -n "${WFC_BROWSER_TEST:-}" && "$name" != "$WFC_BROWSER_TEST" ]]; then continue; fi
  case "$name" in wfc_browser_dom_test|wfc_serve_test|wfc_music_render_process_test|wfc_music_ensemble_render_process_test|wfc_music_ensemble_midi_render_process_test|wfc_music_voices_render_process_test|wfc_connectivity_process_test|wfc_music_studies_process_test) continue ;; esac
  sources+=("$name")
  for extension in html js; do
    [[ -f "$web/$name.$extension" ]] || missing+=("$web/$name.$extension")
  done
done
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
"$server" --root "$web" --port 4180 >"$results/server.log" 2>&1 &
server_pid=$!
ready=false
for _ in {1..50}; do
  kill -0 "$server_pid" 2>/dev/null || { cat "$results/server.log" >&2; exit 1; }
  bound=false
  while IFS= read -r line; do
    [[ "$line" != 'WFC static server: http://127.0.0.1:4180/' ]] || bound=true
  done <"$results/server.log"
  if [[ "$bound" == true ]] &&
      curl --max-time 2 --silent --fail "http://127.0.0.1:4180/${sources[0]}.html" >/dev/null; then
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
  profile="$repository_root/$results/profile-$name"
  mkdir -p "$profile"
  timeout_marker="$results/$name.timeout"
  : >"$timeout_marker"
  # Ten real entry pages retain their individual virtual-time allowances;
  # this program still uses the unchanged 60-second process watchdog below.
  virtual_time_budget=15000
  if [[ "$name" == wfc_browser_demo_entries_test ]]; then virtual_time_budget=155000; fi
  "$chrome" --headless --disable-gpu --disable-dev-shm-usage \
    --no-first-run --no-default-browser-check --user-data-dir="$profile" \
    --virtual-time-budget="$virtual_time_budget" --dump-dom \
    "http://127.0.0.1:4180/$name.html" >"$results/$name.dom" 2>"$results/$name.log" &
  browser_pid=$!
  # POSIX sleep/kill watchdog: no GNU timeout utility is needed on macOS.
  # Only this invocation's child PIDs are ever signalled. Cancel the timer
  # immediately when the browser returns, including nonzero exits.
  (
    trap - EXIT
    timer_pid=''
    trap 'trap "" TERM INT; if [[ -n "$timer_pid" ]]; then terminate_owned_child "$timer_pid" watchdog-timer || exit 1; fi; exit 0' TERM INT
    sleep 60 &
    timer_pid=$!
    wait "$timer_pid" || exit 0
    timer_pid=''
    printf 'deadline exceeded\n' >"$timeout_marker"
    printf 'Browser timed out: %s.\n' "$name" >&2
    kill "$browser_pid" 2>/dev/null || exit 0
    sleep 2 &
    timer_pid=$!
    wait "$timer_pid" || exit 0
    timer_pid=''
    kill -KILL "$browser_pid" 2>/dev/null || true
  ) &
  watchdog_pid=$!
  browser_status=0
  wait "$browser_pid" || browser_status=$?
  browser_pid=''
  stop_watchdog
  checker_args=(--dom "$results/$name.dom" --expect data-self-test=passed)
  if [[ "$name" == wfc_browser_demo_entries_test ]]; then
    checker_args+=(--expect data-demo-entries-self-test=passed)
    checker_args+=(--expect data-demo-entries-count=10)
  fi
  if [[ "$name" == wfc_music_ensemble_stream_demo_test ]]; then
    # Awaited file transactions have their own application completion signal.
    checker_args+=(--expect data-stream-self-test=passed)
    checker_args+=(--expect data-stream-release=passed)
    checker_args+=(--expect data-midi-stream-self-test=passed)
    checker_args+=(--expect data-midi-stream-release=passed)
  fi
  if [[ "$name" == wfc_music_voices_browser_test ]]; then
    checker_args+=(--expect data-voice-stream-self-test=passed)
    checker_args+=(--expect data-voice-stream-release=passed)
  fi
  if [[ -s "$timeout_marker" ]]; then
    failures+=("$name")
  elif [[ "$browser_status" -ne 0 ]]; then
    printf 'Browser failed: %s (exit %s).\n' "$name" "$browser_status" >&2
    failures+=("$name")
  elif ! "$checker" "${checker_args[@]}"; then
    printf 'Browser assertions failed: %s.\n' "$name" >&2
    failures+=("$name")
  fi
  count=$((count+1))
done
if [[ "${#failures[@]}" -gt 0 ]]; then
  printf 'Browser conformance failed (%s/%s):\n' "${#failures[@]}" "$count" >&2
  printf '  %s\n' "${failures[@]}" >&2
  exit 1
fi
echo "Pascal conformance passed in the browser: $count programs."
