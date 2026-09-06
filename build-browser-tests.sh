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
  case "$name" in wfc_package_check_process_test) continue ;; esac
  case "$name" in wfc_browser_dom_test|wfc_browser_args_test|wfc_browser_socket_test|wfc_browser_websocket_test|wfc_browser_cdp_test|wfc_browser_capture_test|wfc_serve_test|wfc_music_render_process_test|wfc_music_ensemble_render_process_test|wfc_music_ensemble_midi_render_process_test|wfc_music_voices_render_process_test|wfc_connectivity_process_test|wfc_music_studies_process_test) continue ;; esac
  "$compiler" -B -Tbrowser -Mdelphi -Jc -Jirtl.js -Fusrc -Futools \
    -Fuexamples/2D/common -Fuexamples/3D/common \
    -Fuexamples/music/01_simple_A_major -Fuexamples/music/02_simple_song_riffs \
    -Fuexamples/2D/05_LearnedPatternWorld \
    -Fuexamples/learning/05_TrainingStudio -Fuexamples/music/05_MusicStudio \
    -Fuexamples/passes/04_NeighborhoodCounts \
    -Fuexamples/passes/05_DeterministicRestarts \
    -Fuexamples/passes/02_TraceInspector \
    -Fuexamples/text/03_PassComposition \
    -Fuexamples/passes/06_ConnectedRoutes \
    -Fuexamples/music/06_EnsembleStudio \
    -Fuexamples/music/07_VoiceStudio \
    -FUbuild/browser/tests/units -FEbuild/browser/tests/www "$source"
  html="build/browser/tests/www/$name.html"
  if [[ -f "$html" ]]; then rm -- "$html"; fi
  "$checker" --harness "$name.js" --dom "$html"
done
# The entry regression loads real HTML/bootstrap/CSS, not a replacement fixture.
# Only named, freshly built public assets enter its served root.
for specification in \
  'world2d|build-browser|BrowserWorld.js|browserworld.css' \
  'text-passes|build-browser-text|BrowserTextPassComposition.js|browsertextpasses.css' \
  'building3d|build-browser-building3d|BrowserBuilding.js|browserbuilding.css' \
  'training|build-browser-training|BrowserTrainingStudio.js|trainingstudio.css' \
  'music|build-browser-music|BrowserMusicStudio.js|musicstudio.css' \
  'counts|build-browser-counts|BrowserNeighborhoodCounts.js|counts.css' \
  'ensemble|build-browser-ensemble|BrowserEnsembleStudio.js|ensemblestudio.css' \
  'voices|build-browser-voices|BrowserVoiceStudio.js|voicestudio.css' \
  'connectivity|build-browser-connectivity|BrowserConnectedRoutes.js|connectedroutes.css' \
  'terraces|build-browser-terraces|BrowserTerraces.js|terraces.css'
do
  IFS='|' read -r demo script javascript stylesheet <<< "$specification"
  PAS2JS="$compiler" bash "$repository_root/$script.sh"
  entry_source="$repository_root/build/browser/$demo/www"
  entry_target="$repository_root/build/browser/tests/www/demo-entries/$demo"
  mkdir -p -- "$entry_target"
  cp -- "$entry_source/index.html" "$entry_source/$javascript" \
    "$entry_source/$stylesheet" "$entry_target/"
done
# Separately named asynchronous capture fixture; not a conformance program.
mkdir -p build/browser/tests/www/capture-fixture
"$compiler" -B -Tbrowser -Mdelphi -Jc -Jirtl.js \
  -FUbuild/browser/tests/units -FEbuild/browser/tests/www/capture-fixture \
  test/browser_capture/fixture.lpr
cp -- test/browser_capture/index.html build/browser/tests/www/capture-fixture/
echo 'Pascal browser conformance, capture fixture, and ten actual demo entries staged under build/browser/tests/www.'
