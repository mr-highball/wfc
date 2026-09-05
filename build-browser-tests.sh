#!/usr/bin/env bash
set -euo pipefail
repository_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd "$repository_root"
compiler="${PAS2JS:-pas2js}"
checker="${WFC_BROWSER_CHECK:-$repository_root/build/native/bin/wfc_browser_check}"
if [[ -f "$checker.exe" ]]; then checker="$checker.exe"; fi
[[ -x "$checker" ]] || { echo 'Build the included FPC browser checker first.' >&2; exit 1; }
mkdir -p build/browser/tests/units build/browser/tests/www
for source in test/*_test.lpr; do
  name="$(basename -- "$source" .lpr)"
  case "$name" in wfc_browser_dom_test|wfc_serve_test|wfc_music_render_process_test) continue ;; esac
  "$compiler" -B -Tbrowser -Mdelphi -Jc -Jirtl.js -Fusrc -Futools \
    -Fuexamples/2D/common -Fuexamples/3D/common \
    -Fuexamples/2D/05_LearnedPatternWorld \
    -Fuexamples/learning/05_TrainingStudio -Fuexamples/music/05_MusicStudio \
    -FUbuild/browser/tests/units -FEbuild/browser/tests/www "$source"
  html="build/browser/tests/www/$name.html"
  if [[ -f "$html" ]]; then rm -- "$html"; fi
  "$checker" --harness "$name.js" --dom "$html"
done
echo 'Pascal browser conformance staged under build/browser/tests/www.'
