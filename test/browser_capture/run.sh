#!/usr/bin/env bash
# MIT License; see LICENSE in the repository root.
set -euo pipefail
repository_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repository_root"
fixture_root='build/browser/tests/www/capture-fixture'
[[ -f "$fixture_root/fixture.js" && -f "$fixture_root/index.html" ]] || {
  echo 'Stage the Pascal capture fixture with build-browser-tests.sh first.' >&2
  exit 1
}
mkdir -p build/browser/tests/results
evidence="$(mktemp -d "$repository_root/build/browser/tests/results/capture-fixtures.XXXXXXXX")"
for mode in delayed-blob negative; do
  bash ./test-browser-tests.sh --root "$fixture_root" --page "index.html?mode=$mode" \
    --expect data-fixture-release=passed --expect data-fixture-bytes=4 \
    --expect 'data-unicode=é🎵' >"$evidence/$mode.log" 2>&1 || {
      cat "$evidence/$mode.log" >&2
      exit 1
    }
  printf 'Capture fixture passed: %s.\n' "$mode"
done
for mode in failed missing-release permanent-pending; do
  if bash ./test-browser-tests.sh --root "$fixture_root" --page "index.html?mode=$mode" \
      --expect data-fixture-release=passed --expect data-fixture-bytes=4 \
      >"$evidence/$mode.log" 2>&1; then
    printf 'Capture fixture incorrectly succeeded: %s.\n' "$mode" >&2
    exit 1
  fi
  # A missing tool, startup failure, or unrelated timeout is not proof that
  # completion logic rejected a rendered pending/failed test. Require the
  # native diagnostic to include this fixture's actual observed body state.
  observed=false
  while IFS= read -r line; do
    [[ "$line" == wfc_browser_capture:* ]] || continue
    [[ "$line" == *'"data-fixture-release"="pending"'* ]] || continue
    case "$mode" in
      failed)
        if [[ "$line" == *'intentional fixture failure'* && "$line" == *'"data-self-test"="failed"'* ]]; then observed=true; fi ;;
      missing-release)
        if [[ "$line" == *'browser socket deadline expired'* && "$line" == *'"data-self-test"="passed"'* ]]; then observed=true; fi ;;
      permanent-pending)
        if [[ "$line" == *'browser socket deadline expired'* && "$line" == *'"data-self-test"="pending"'* ]]; then observed=true; fi ;;
    esac
  done <"$evidence/$mode.log"
  if [[ "$observed" != true || -e build/browser/tests/results/standalone-demo.dom ]]; then
    cat "$evidence/$mode.log" >&2
    printf 'Capture fixture lacks expected failure evidence or retained a passing alias: %s.\n' "$mode" >&2
    exit 1
  fi
  printf 'Capture fixture rejected with exact failure evidence: %s.\n' "$mode"
done
printf 'Five native browser capture fixtures passed; evidence: %s.\n' "$evidence"
