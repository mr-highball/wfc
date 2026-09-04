#!/usr/bin/env bash

set -u
set -o pipefail

repository_root=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
compiler=${FPC:-fpc}
source_directory="$repository_root/src"
test_source="$repository_root/test/wfc_test.lpr"
world_test_source="$repository_root/test/wfc_world2d_test.lpr"
settlement_test_source="$repository_root/test/wfc_world2d_settlement_test.lpr"
learning_test_source="$repository_root/test/wfc_learn_test.lpr"
pattern_test_source="$repository_root/test/wfc_pattern2d_test.lpr"
sequence_test_source="$repository_root/test/wfc_sequence_test.lpr"
voxel_test_source="$repository_root/test/wfc_voxel3d_test.lpr"
building_test_source="$repository_root/test/wfc_building3d_test.lpr"
midi_test_source="$repository_root/test/wfc_midi_smf_test.lpr"
music_test_source="$repository_root/test/wfc_music_test.lpr"
music_graph_test_source="$repository_root/test/wfc_music_graph_test.lpr"
music_midi_test_source="$repository_root/test/wfc_music_midi_test.lpr"
example_source="$repository_root/examples/text/01_SimpleTiledWorld/SimpleTiledWorld.lpr"
world_example_source="$repository_root/examples/2D/01_MultiPassWorld/MultiPassWorld.lpr"
settlement_example_source="$repository_root/examples/2D/03_SelectiveSettlement/SelectiveSettlement.lpr"
learning_example_source="$repository_root/examples/learning/01_LearnTiles/LearnTiles.lpr"
corpus_example_source="$repository_root/examples/learning/02_LearnCorpus/LearnCorpus.lpr"
pattern_example_source="$repository_root/examples/learning/03_LearnPatterns/LearnPatterns.lpr"
sequence_example_source="$repository_root/examples/sequence/01_LearnSequence/LearnSequence.lpr"
music_example_source="$repository_root/examples/music/03_PassComposition/PassComposition.lpr"
spatial_example_source="$repository_root/examples/passes/01_SpatialDependencies/SpatialDependencies.lpr"
building_example_source="$repository_root/examples/3D/02_MultiPassBuilding/MultiPassBuilding.lpr"
building_example_directory="$repository_root/examples/3D/02_MultiPassBuilding"
world_common_directory="$repository_root/examples/2D/common"
unit_output_directory="$repository_root/build/native/units"
binary_output_directory="$repository_root/build/native/bin"

mkdir -p -- "$unit_output_directory" "$binary_output_directory" || exit $?

compiler_source_directory=$source_directory
compiler_test_source=$test_source
compiler_world_test_source=$world_test_source
compiler_settlement_test_source=$settlement_test_source
compiler_learning_test_source=$learning_test_source
compiler_pattern_test_source=$pattern_test_source
compiler_sequence_test_source=$sequence_test_source
compiler_voxel_test_source=$voxel_test_source
compiler_building_test_source=$building_test_source
compiler_midi_test_source=$midi_test_source
compiler_music_test_source=$music_test_source
compiler_music_graph_test_source=$music_graph_test_source
compiler_music_midi_test_source=$music_midi_test_source
compiler_example_source=$example_source
compiler_world_example_source=$world_example_source
compiler_settlement_example_source=$settlement_example_source
compiler_learning_example_source=$learning_example_source
compiler_corpus_example_source=$corpus_example_source
compiler_pattern_example_source=$pattern_example_source
compiler_sequence_example_source=$sequence_example_source
compiler_music_example_source=$music_example_source
compiler_spatial_example_source=$spatial_example_source
compiler_building_example_source=$building_example_source
compiler_building_example_directory=$building_example_directory
compiler_world_common_directory=$world_common_directory
compiler_unit_output_directory=$unit_output_directory
compiler_binary_output_directory=$binary_output_directory
host_system=$(uname -s)
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*)
    compiler_source_directory=$(cygpath -m "$source_directory") || exit $?
    compiler_test_source=$(cygpath -m "$test_source") || exit $?
    compiler_world_test_source=$(cygpath -m "$world_test_source") || exit $?
    compiler_settlement_test_source=$(cygpath -m "$settlement_test_source") || exit $?
    compiler_learning_test_source=$(cygpath -m "$learning_test_source") || exit $?
    compiler_pattern_test_source=$(cygpath -m "$pattern_test_source") || exit $?
    compiler_sequence_test_source=$(cygpath -m "$sequence_test_source") || exit $?
    compiler_voxel_test_source=$(cygpath -m "$voxel_test_source") || exit $?
    compiler_building_test_source=$(cygpath -m "$building_test_source") || exit $?
    compiler_midi_test_source=$(cygpath -m "$midi_test_source") || exit $?
    compiler_music_test_source=$(cygpath -m "$music_test_source") || exit $?
    compiler_music_graph_test_source=$(cygpath -m "$music_graph_test_source") || exit $?
    compiler_music_midi_test_source=$(cygpath -m "$music_midi_test_source") || exit $?
    compiler_example_source=$(cygpath -m "$example_source") || exit $?
    compiler_world_example_source=$(cygpath -m "$world_example_source") || exit $?
    compiler_settlement_example_source=$(cygpath -m "$settlement_example_source") || exit $?
    compiler_learning_example_source=$(cygpath -m "$learning_example_source") || exit $?
    compiler_corpus_example_source=$(cygpath -m "$corpus_example_source") || exit $?
    compiler_pattern_example_source=$(cygpath -m "$pattern_example_source") || exit $?
    compiler_sequence_example_source=$(cygpath -m "$sequence_example_source") || exit $?
    compiler_music_example_source=$(cygpath -m "$music_example_source") || exit $?
    compiler_spatial_example_source=$(cygpath -m "$spatial_example_source") || exit $?
    compiler_building_example_source=$(cygpath -m "$building_example_source") || exit $?
    compiler_building_example_directory=$(cygpath -m "$building_example_directory") || exit $?
    compiler_world_common_directory=$(cygpath -m "$world_common_directory") || exit $?
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

printf "Building the selective-settlement conformance suite.\n"
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
  "$compiler_settlement_test_source" || exit $?

settlement_test_executable="$binary_output_directory/wfc_world2d_settlement_test"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) settlement_test_executable="${settlement_test_executable}.exe" ;;
esac

printf "Running '%s'.\n" "$settlement_test_executable"
"$settlement_test_executable" || exit $?

printf "Building the model-learning conformance suite.\n"
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
  "$compiler_learning_test_source" || exit $?

learning_test_executable="$binary_output_directory/wfc_learn_test"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) learning_test_executable="${learning_test_executable}.exe" ;;
esac

printf "Running '%s'.\n" "$learning_test_executable"
"$learning_test_executable" || exit $?

printf "Building the overlapping-pattern conformance suite.\n"
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
  "$compiler_pattern_test_source" || exit $?

pattern_test_executable="$binary_output_directory/wfc_pattern2d_test"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) pattern_test_executable="${pattern_test_executable}.exe" ;;
esac

printf "Running '%s'.\n" "$pattern_test_executable"
"$pattern_test_executable" || exit $?

printf "Building the sequence-foundation conformance suite.\n"
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
  "$compiler_sequence_test_source" || exit $?

sequence_test_executable="$binary_output_directory/wfc_sequence_test"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) sequence_test_executable="${sequence_test_executable}.exe" ;;
esac

printf "Running '%s'.\n" "$sequence_test_executable"
"$sequence_test_executable" || exit $?

printf "Building the voxel-3D foundation conformance suite.\n"
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
  "$compiler_voxel_test_source" || exit $?

voxel_test_executable="$binary_output_directory/wfc_voxel3d_test"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) voxel_test_executable="${voxel_test_executable}.exe" ;;
esac

printf "Running '%s'.\n" "$voxel_test_executable"
"$voxel_test_executable" || exit $?

printf "Building the multi-pass Building 3D conformance suite.\n"
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
  "$compiler_building_test_source" || exit $?

building_test_executable="$binary_output_directory/wfc_building3d_test"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) building_test_executable="${building_test_executable}.exe" ;;
esac

printf "Running '%s'.\n" "$building_test_executable"
"$building_test_executable" || exit $?

for compiler_music_suite in \
  "$compiler_midi_test_source" \
  "$compiler_music_test_source" \
  "$compiler_music_graph_test_source" \
  "$compiler_music_midi_test_source"
do
  music_suite_name=$(basename -- "$compiler_music_suite" .lpr)
  printf "Building the music conformance suite '%s'.\n" "$music_suite_name"
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
    "$compiler_music_suite" || exit $?

  music_suite_executable="$binary_output_directory/$music_suite_name"
  case "$host_system" in
    CYGWIN*|MINGW*|MSYS*) music_suite_executable="${music_suite_executable}.exe" ;;
  esac
  printf "Running '%s'.\n" "$music_suite_executable"
  "$music_suite_executable" || exit $?
done

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
  "-Fu$compiler_world_common_directory" \
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

printf "Building the portable selective-settlement example.\n"
"$compiler" "$@" \
  -B \
  -Mdelphi \
  -Sa \
  -Cr \
  -Co \
  -Ci \
  "-Fu$compiler_source_directory" \
  "-Fu$compiler_world_common_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_binary_output_directory" \
  "$compiler_settlement_example_source" || exit $?

settlement_example_executable="$binary_output_directory/SelectiveSettlement"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) settlement_example_executable="${settlement_example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$settlement_example_executable"
"$settlement_example_executable" 0 >/dev/null || exit $?

printf "Smoke testing '%s' with its default seed.\n" "$settlement_example_executable"
"$settlement_example_executable" >/dev/null || exit $?

printf "Building the portable learned-tiles example.\n"
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
  "$compiler_learning_example_source" || exit $?

learning_example_executable="$binary_output_directory/LearnTiles"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) learning_example_executable="${learning_example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$learning_example_executable"
"$learning_example_executable" 0 >/dev/null || exit $?

printf "Building the portable learned-corpus example.\n"
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
  "$compiler_corpus_example_source" || exit $?

corpus_example_executable="$binary_output_directory/LearnCorpus"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) corpus_example_executable="${corpus_example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$corpus_example_executable"
"$corpus_example_executable" 0 >/dev/null || exit $?

printf "Building the portable overlapping-pattern example.\n"
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
  "$compiler_pattern_example_source" || exit $?

pattern_example_executable="$binary_output_directory/LearnPatterns"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) pattern_example_executable="${pattern_example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$pattern_example_executable"
"$pattern_example_executable" 0 >/dev/null || exit $?

printf "Building the portable learned-sequence example.\n"
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
  "$compiler_sequence_example_source" || exit $?

sequence_example_executable="$binary_output_directory/LearnSequence"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) sequence_example_executable="${sequence_example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$sequence_example_executable"
"$sequence_example_executable" 0 >/dev/null || exit $?

printf "Building the dependency-free pass-composed music example.\n"
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
  "$compiler_music_example_source" || exit $?

music_example_executable="$binary_output_directory/PassComposition"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) music_example_executable="${music_example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$music_example_executable"
"$music_example_executable" 0 >/dev/null || exit $?

printf "Building the dependency-free spatial-pass example.\n"
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
  "$compiler_spatial_example_source" || exit $?

spatial_example_executable="$binary_output_directory/SpatialDependencies"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) spatial_example_executable="${spatial_example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$spatial_example_executable"
"$spatial_example_executable" 0 >/dev/null || exit $?

printf "Building the dependency-free multi-pass Building 3D example.\n"
"$compiler" "$@" \
  -B \
  -Mdelphi \
  -Sa \
  -Cr \
  -Co \
  -Ci \
  "-Fu$compiler_source_directory" \
  "-Fu$compiler_building_example_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_binary_output_directory" \
  "$compiler_building_example_source" || exit $?

building_example_executable="$binary_output_directory/MultiPassBuilding"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) building_example_executable="${building_example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$building_example_executable"
"$building_example_executable" 0 >/dev/null || exit $?
