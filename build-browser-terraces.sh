#!/usr/bin/env bash

set -u
set -o pipefail

repository_root=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
compiler=${PAS2JS:-pas2js}
source_directory="$repository_root/src"
browser_directory="$repository_root/examples/3D/04_LearnedTerraces"
browser_source="$browser_directory/BrowserTerraces.lpr"
browser_app="$browser_directory/browser_terraces_app.pas"
browser_html="$browser_directory/index.html"
browser_css="$browser_directory/terraces.css"
output_directory="$repository_root/build/browser/terraces"
unit_output_directory="$output_directory/units"
web_output_directory="$output_directory/www"
staged_javascript="$web_output_directory/BrowserTerraces.js"

for required_file in \
  "$browser_source" "$browser_app" \
  "$browser_html" "$browser_css"
do
  if test ! -f "$required_file"; then
    printf 'Required Learned Terraces browser file was not found: %s\n' \
      "$required_file" >&2
    exit 1
  fi
done

mkdir -p -- "$unit_output_directory" "$web_output_directory" || exit $?

compiler_source_directory=$source_directory
compiler_browser_directory=$browser_directory
compiler_browser_source=$browser_source
compiler_unit_output_directory=$unit_output_directory
compiler_web_output_directory=$web_output_directory
case "$(uname -s)" in
  CYGWIN*|MINGW*|MSYS*)
    compiler_source_directory=$(cygpath -m "$source_directory") || exit $?
    compiler_browser_directory=$(cygpath -m "$browser_directory") || exit $?
    compiler_browser_source=$(cygpath -m "$browser_source") || exit $?
    compiler_unit_output_directory=$(cygpath -m "$unit_output_directory") || exit $?
    compiler_web_output_directory=$(cygpath -m "$web_output_directory") || exit $?
    export MSYS2_ARG_CONV_EXCL='*'
    ;;
esac

# Never accept JavaScript left by an earlier compiler invocation.
rm -f -- "$staged_javascript" || exit $?

printf "Building Learned Terraces with '%s'.\n" "$compiler"
"$compiler" "$@" \
  -B \
  -Tbrowser \
  -Mdelphi \
  -Jc \
  -Jirtl.js \
  "-Fu$compiler_source_directory" \
  "-Fu$compiler_browser_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_web_output_directory" \
  "$compiler_browser_source" || exit $?

if test ! -s "$staged_javascript"; then
  printf 'pas2js did not produce %s\n' "$staged_javascript" >&2
  exit 1
fi

cp -- "$browser_html" "$browser_css" "$web_output_directory/" || exit $?
for staged_asset in \
  "$web_output_directory/index.html" \
  "$web_output_directory/terraces.css"
do
  if test ! -s "$staged_asset"; then
    printf 'Staged Learned Terraces asset is empty: %s\n' "$staged_asset" >&2
    exit 1
  fi
done

printf "Learned Terraces staged in '%s'.\n" "$web_output_directory"
