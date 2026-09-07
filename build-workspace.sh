#!/usr/bin/env bash
# SPDX-License-Identifier: MIT
set -u
set -o pipefail
repository_root=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd) || exit $?
compiler=${FPC:-fpc}
example_directory="$repository_root/examples/passes/08_PipelineWorkspace"
output_directory="$repository_root/build/workspace/native"
binary_directory="$output_directory/bin"
suffix=''
host_system=$(uname -s)
case "$host_system" in CYGWIN*|MINGW*|MSYS*) suffix='.exe' ;; esac
for source in examples/passes/08_PipelineWorkspace/PipelineWorkspace.lpr tools/wfc_workspace_cli.lpr tools/wfc_serve.lpr; do
  if test ! -f "$repository_root/$source"; then
    printf 'Required Pascal source not found: %s\n' "$repository_root/$source" >&2
    exit 1
  fi
done
for name in pipeline_workspace_workbench.pas pipeline_workspace_presets.pas pipeline_workspace_view.pas; do
  if test ! -f "$example_directory/$name"; then
    printf 'Required workspace unit not found: %s\n' "$name" >&2
    exit 1
  fi
done
mkdir -p -- "$binary_directory" || exit $?
compiler_repository_root=$repository_root
compiler_example_directory=$example_directory
compiler_binary_directory=$binary_directory
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*)
    compiler_repository_root=$(cygpath -m "$repository_root") || exit $?
    compiler_example_directory=$(cygpath -m "$example_directory") || exit $?
    compiler_binary_directory=$(cygpath -m "$binary_directory") || exit $?
    export MSYS2_ARG_CONV_EXCL='*'
    ;;
esac
for name in PipelineWorkspace wfc_workspace wfc_serve; do
  case "$name" in
    PipelineWorkspace) source='examples/passes/08_PipelineWorkspace/PipelineWorkspace.lpr'; smoke='--help' ;;
    wfc_workspace) source='tools/wfc_workspace_cli.lpr'; smoke='--help' ;;
    wfc_serve) source='tools/wfc_serve.lpr'; smoke='--version' ;;
  esac
  units="$output_directory/units/$name"
  mkdir -p -- "$units" || exit $?
  compiler_units=$units
  case "$host_system" in CYGWIN*|MINGW*|MSYS*) compiler_units=$(cygpath -m "$units") || exit $? ;; esac
  executable="$binary_directory/$name$suffix"
  compiler_executable="$compiler_binary_directory/$name$suffix"
  # This is one named generated output, never a recursive directory removal.
  rm -f -- "$executable" || exit $?
  printf "Building %s with '%s'.\n" "$name" "$compiler"
  "$compiler" "$@" -B -Mdelphi -Sa -Cr -Co -Ci -gl \
    "-Fu$compiler_repository_root/src" "-Fu$compiler_repository_root/tools" \
    "-Fu$compiler_example_directory" "-FU$compiler_units" "-FE$compiler_binary_directory" \
    "-o$compiler_executable" "$compiler_repository_root/$source" || exit $?
  if test ! -s "$executable"; then
    printf 'FPC did not produce %s\n' "$executable" >&2
    exit 1
  fi
  "$executable" "$smoke" || exit $?
done
printf "Native workspace demo and included tools are ready in '%s'.\n" "$binary_directory"
printf 'No server was started. Run PipelineWorkspace --help for generation, or wfc_serve --help for hosting.\n'
