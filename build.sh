#!/usr/bin/env bash

set -u
set -o pipefail

repository_root=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
compiler=${FPC:-fpc}
source_directory="$repository_root/src"
test_source="$repository_root/test/wfc_test.lpr"
world_test_source="$repository_root/test/wfc_world2d_test.lpr"
example_source="$repository_root/examples/text/01_SimpleTiledWorld/SimpleTiledWorld.lpr"
world_example_source="$repository_root/examples/2D/01_MultiPassWorld/MultiPassWorld.lpr"
unit_output_directory="$repository_root/build/native/units"
binary_output_directory="$repository_root/build/native/bin"

mkdir -p -- "$unit_output_directory" "$binary_output_directory" || exit $?

compiler_source_directory=$source_directory
compiler_test_source=$test_source
compiler_world_test_source=$world_test_source
compiler_example_source=$example_source
compiler_world_example_source=$world_example_source
compiler_unit_output_directory=$unit_output_directory
compiler_binary_output_directory=$binary_output_directory
host_system=$(uname -s)
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*)
    compiler_source_directory=$(cygpath -m "$source_directory") || exit $?
    compiler_test_source=$(cygpath -m "$test_source") || exit $?
    compiler_world_test_source=$(cygpath -m "$world_test_source") || exit $?
    compiler_example_source=$(cygpath -m "$example_source") || exit $?
    compiler_world_example_source=$(cygpath -m "$world_example_source") || exit $?
    compiler_unit_output_directory=$(cygpath -m "$unit_output_directory") || exit $?
    compiler_binary_output_directory=$(cygpath -m "$binary_output_directory") || exit $?
    export MSYS2_ARG_CONV_EXCL='*'
    ;;
esac

printf "Building the native conformance suite with '%s'.\n" "$compiler"
"$compiler" "$@" \
  -B \
  -Mdelphi \
  -Sa \
  -Cr \
  -Co \
  -Ci \
  "-Fu$compiler_source_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_binary_output_directory" \
  "$compiler_test_source" || exit $?

test_executable="$binary_output_directory/wfc_test"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) test_executable="${test_executable}.exe" ;;
esac

printf "Running '%s'.\n" "$test_executable"
"$test_executable" || exit $?

printf "Building the 2D ecosystem conformance suite.\n"
"$compiler" "$@" \
  -B \
  -Mdelphi \
  -Sa \
  -Cr \
  -Co \
  -Ci \
  "-Fu$compiler_source_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_binary_output_directory" \
  "$compiler_world_test_source" || exit $?

world_test_executable="$binary_output_directory/wfc_world2d_test"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) world_test_executable="${world_test_executable}.exe" ;;
esac

printf "Running '%s'.\n" "$world_test_executable"
"$world_test_executable" || exit $?

printf "Building the dependency-free tiled-world example.\n"
"$compiler" "$@" \
  -B \
  -Mdelphi \
  -Sa \
  -Cr \
  -Co \
  -Ci \
  "-Fu$compiler_source_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_binary_output_directory" \
  "$compiler_example_source" || exit $?

example_executable="$binary_output_directory/SimpleTiledWorld"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) example_executable="${example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$example_executable"
"$example_executable" 0 >/dev/null || exit $?

printf "Building the portable multi-pass 2D example.\n"
"$compiler" "$@" \
  -B \
  -Mdelphi \
  -Sa \
  -Cr \
  -Co \
  -Ci \
  "-Fu$compiler_source_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_binary_output_directory" \
  "$compiler_world_example_source" || exit $?

world_example_executable="$binary_output_directory/MultiPassWorld"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) world_example_executable="${world_example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$world_example_executable"
"$world_example_executable" 0 >/dev/null || exit $?

printf "Smoke testing '%s' with its default seed.\n" "$world_example_executable"
"$world_example_executable" >/dev/null || exit $?
