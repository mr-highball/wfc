#!/usr/bin/env bash
# SPDX-License-Identifier: MIT
set -u
set -o pipefail
repository_root=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
compiler=${PAS2JS:-pas2js}
source_directory="$repository_root/src"
browser_directory="$repository_root/examples/passes/07_MappedWorld"
output_directory="$repository_root/build/browser/mapped"
unit_output_directory="$output_directory/units"
web_output_directory="$output_directory/www"
staged_javascript="$web_output_directory/BrowserMappedWorld.js"
for name in BrowserMappedWorld.lpr browser_mapped_world_app.pas \
  mapped_world_types.pas mapped_world_workbench.pas mapped_world_validation.pas \
  mapped_world_svg.pas index.html mappedworld.css; do
  if test ! -f "$browser_directory/$name"; then
    printf 'Required Mapped World file was not found: %s\n' "$browser_directory/$name" >&2
    exit 1
  fi
done
mkdir -p -- "$unit_output_directory" "$web_output_directory" || exit $?
compiler_source_directory=$source_directory
compiler_browser_directory=$browser_directory
compiler_unit_output_directory=$unit_output_directory
compiler_web_output_directory=$web_output_directory
case "$(uname -s)" in
  CYGWIN*|MINGW*|MSYS*)
    compiler_source_directory=$(cygpath -m "$source_directory") || exit $?
    compiler_browser_directory=$(cygpath -m "$browser_directory") || exit $?
    compiler_unit_output_directory=$(cygpath -m "$unit_output_directory") || exit $?
    compiler_web_output_directory=$(cygpath -m "$web_output_directory") || exit $?
    export MSYS2_ARG_CONV_EXCL='*'
    ;;
esac
rm -f -- "$staged_javascript" || exit $?
printf "Building Mapped World with '%s'.\n" "$compiler"
"$compiler" "$@" -B -Tbrowser -Mdelphi -Jc -Jirtl.js \
  "-Fu$compiler_source_directory" "-Fu$compiler_browser_directory" \
  "-FU$compiler_unit_output_directory" "-FE$compiler_web_output_directory" \
  "$compiler_browser_directory/BrowserMappedWorld.lpr" || exit $?
if test ! -s "$staged_javascript"; then
  printf 'pas2js did not produce %s\n' "$staged_javascript" >&2
  exit 1
fi
cp -- "$browser_directory/index.html" "$browser_directory/mappedworld.css" "$web_output_directory/" || exit $?
for asset in index.html mappedworld.css; do
  if test ! -s "$web_output_directory/$asset"; then
    printf 'Staged Mapped World asset is empty: %s\n' "$asset" >&2
    exit 1
  fi
done
printf "Mapped World staged in '%s'. Host it using the included FPC wfc_serve.\n" "$web_output_directory"
