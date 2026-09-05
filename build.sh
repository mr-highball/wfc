#!/usr/bin/env bash

set -u
set -o pipefail

repository_root=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
compiler=${FPC:-fpc}
source_directory="$repository_root/src"
tools_directory="$repository_root/tools"
test_source="$repository_root/test/wfc_test.lpr"
world_test_source="$repository_root/test/wfc_world2d_test.lpr"
settlement_test_source="$repository_root/test/wfc_world2d_settlement_test.lpr"
learning_test_source="$repository_root/test/wfc_learn_test.lpr"
pattern_test_source="$repository_root/test/wfc_pattern2d_test.lpr"
pattern_pass_test_source="$repository_root/test/wfc_pattern2d_passes_test.lpr"
sequence_test_source="$repository_root/test/wfc_sequence_test.lpr"
text_test_source="$repository_root/test/wfc_text_test.lpr"
text_pass_test_source="$repository_root/test/wfc_text_passes_test.lpr"
negotiation_test_source="$repository_root/test/wfc_negotiation_test.lpr"
selective_negotiation_test_source="$repository_root/test/wfc_selective_negotiation_test.lpr"
voxel_test_source="$repository_root/test/wfc_voxel3d_test.lpr"
building_test_source="$repository_root/test/wfc_building3d_test.lpr"
trace_reference_test_source="$repository_root/test/wfc_trace_reference_test.lpr"
trace_test_source="$repository_root/test/wfc_trace_test.lpr"
trace_utility_test_source="$repository_root/test/wfc_trace_utility_test.lpr"
isometric_test_source="$repository_root/test/wfc_voxel3d_isometric_test.lpr"
svg_test_source="$repository_root/test/wfc_voxel3d_svg_test.lpr"
building_view_test_source="$repository_root/test/wfc_building3d_view_test.lpr"
midi_test_source="$repository_root/test/wfc_midi_smf_test.lpr"
music_test_source="$repository_root/test/wfc_music_test.lpr"
music_graph_test_source="$repository_root/test/wfc_music_graph_test.lpr"
music_midi_test_source="$repository_root/test/wfc_music_midi_test.lpr"
music_passes_test_source="$repository_root/test/wfc_music_passes_test.lpr"
music_passes_text_test_source="$repository_root/test/wfc_music_passes_text_test.lpr"
text_codec_test_source="$repository_root/test/wfc_text_codec_test.lpr"
rule_model_test_source="$repository_root/test/wfc_rule_model_test.lpr"
rule_text_test_source="$repository_root/test/wfc_rule_text_test.lpr"
pipeline_model_test_source="$repository_root/test/wfc_pipeline_model_test.lpr"
pipeline_text_test_source="$repository_root/test/wfc_pipeline_text_test.lpr"
token_lookup_test_source="$repository_root/test/wfc_token_lookup_test.lpr"
pipeline_compile_test_source="$repository_root/test/wfc_pipeline_compile_test.lpr"
pipeline_run_test_source="$repository_root/test/wfc_pipeline_run_test.lpr"
pipeline_run_text_test_source="$repository_root/test/wfc_pipeline_run_text_test.lpr"
pipeline_result_test_source="$repository_root/test/wfc_pipeline_result_test.lpr"
pipeline_result_text_test_source="$repository_root/test/wfc_pipeline_result_text_test.lpr"
pipeline_runtime_test_source="$repository_root/test/wfc_pipeline_runtime_test.lpr"
validate_app_test_source="$repository_root/test/wfc_validate_app_test.lpr"
run_app_test_source="$repository_root/test/wfc_run_app_test.lpr"
learned_pattern_world_bundle_test_source="$repository_root/test/wfc_learned_pattern_world_bundle_test.lpr"
validate_tool_source="$repository_root/tools/wfc_validate.lpr"
run_tool_source="$repository_root/tools/wfc_run.lpr"
pipeline_cli_process_test_source="$repository_root/test/wfc_pipeline_cli_process_test.sh"
example_source="$repository_root/examples/text/01_SimpleTiledWorld/SimpleTiledWorld.lpr"
world_example_source="$repository_root/examples/2D/01_MultiPassWorld/MultiPassWorld.lpr"
settlement_example_source="$repository_root/examples/2D/03_SelectiveSettlement/SelectiveSettlement.lpr"
negotiated_repair_example_source="$repository_root/examples/2D/04_NegotiatedRepair/NegotiatedRepair.lpr"
negotiated_repair_example_directory="$repository_root/examples/2D/04_NegotiatedRepair"
learned_pattern_world_example_source="$repository_root/examples/2D/05_LearnedPatternWorld/LearnedPatternWorld.lpr"
learned_pattern_world_example_directory="$repository_root/examples/2D/05_LearnedPatternWorld"
learning_example_source="$repository_root/examples/learning/01_LearnTiles/LearnTiles.lpr"
corpus_example_source="$repository_root/examples/learning/02_LearnCorpus/LearnCorpus.lpr"
pattern_example_source="$repository_root/examples/learning/03_LearnPatterns/LearnPatterns.lpr"
sequence_example_source="$repository_root/examples/sequence/01_LearnSequence/LearnSequence.lpr"
text_completion_example_source="$repository_root/examples/text/02_ConstraintCompletion/ConstraintCompletion.lpr"
text_completion_example_directory="$repository_root/examples/text/02_ConstraintCompletion"
text_pass_example_source="$repository_root/examples/text/03_PassComposition/TextPassComposition.lpr"
text_pass_example_directory="$repository_root/examples/text/03_PassComposition"
music_example_source="$repository_root/examples/music/03_PassComposition/PassComposition.lpr"
music_variation_example_source="$repository_root/examples/music/04_NegotiatedVariation/NegotiatedVariation.lpr"
music_variation_example_directory="$repository_root/examples/music/04_NegotiatedVariation"
spatial_example_source="$repository_root/examples/passes/01_SpatialDependencies/SpatialDependencies.lpr"
trace_example_source="$repository_root/examples/passes/02_TraceInspector/TraceInspector.lpr"
trace_example_directory="$repository_root/examples/passes/02_TraceInspector"
negotiation_example_source="$repository_root/examples/passes/03_PassNegotiation/PassNegotiation.lpr"
negotiation_example_directory="$repository_root/examples/passes/03_PassNegotiation"
building_example_source="$repository_root/examples/3D/02_MultiPassBuilding/MultiPassBuilding.lpr"
building_example_directory="$repository_root/examples/3D/02_MultiPassBuilding"
building_common_directory="$repository_root/examples/3D/common"
building_svg_source="$repository_root/examples/3D/03_BrowserBuilding/Building3DSvg.lpr"
world_common_directory="$repository_root/examples/2D/common"
unit_output_directory="$repository_root/build/native/units"
binary_output_directory="$repository_root/build/native/bin"

mkdir -p -- "$unit_output_directory" "$binary_output_directory" || exit $?

compiler_source_directory=$source_directory
compiler_tools_directory=$tools_directory
compiler_test_source=$test_source
compiler_world_test_source=$world_test_source
compiler_settlement_test_source=$settlement_test_source
compiler_learning_test_source=$learning_test_source
compiler_pattern_test_source=$pattern_test_source
compiler_pattern_pass_test_source=$pattern_pass_test_source
compiler_sequence_test_source=$sequence_test_source
compiler_text_test_source=$text_test_source
compiler_text_pass_test_source=$text_pass_test_source
compiler_negotiation_test_source=$negotiation_test_source
compiler_selective_negotiation_test_source=$selective_negotiation_test_source
compiler_voxel_test_source=$voxel_test_source
compiler_building_test_source=$building_test_source
compiler_trace_reference_test_source=$trace_reference_test_source
compiler_trace_test_source=$trace_test_source
compiler_trace_utility_test_source=$trace_utility_test_source
compiler_isometric_test_source=$isometric_test_source
compiler_svg_test_source=$svg_test_source
compiler_building_view_test_source=$building_view_test_source
compiler_midi_test_source=$midi_test_source
compiler_music_test_source=$music_test_source
compiler_music_graph_test_source=$music_graph_test_source
compiler_music_midi_test_source=$music_midi_test_source
compiler_music_passes_test_source=$music_passes_test_source
compiler_music_passes_text_test_source=$music_passes_text_test_source
compiler_text_codec_test_source=$text_codec_test_source
compiler_rule_model_test_source=$rule_model_test_source
compiler_rule_text_test_source=$rule_text_test_source
compiler_pipeline_model_test_source=$pipeline_model_test_source
compiler_pipeline_text_test_source=$pipeline_text_test_source
compiler_token_lookup_test_source=$token_lookup_test_source
compiler_pipeline_compile_test_source=$pipeline_compile_test_source
compiler_pipeline_run_test_source=$pipeline_run_test_source
compiler_pipeline_run_text_test_source=$pipeline_run_text_test_source
compiler_pipeline_result_test_source=$pipeline_result_test_source
compiler_pipeline_result_text_test_source=$pipeline_result_text_test_source
compiler_pipeline_runtime_test_source=$pipeline_runtime_test_source
compiler_validate_app_test_source=$validate_app_test_source
compiler_run_app_test_source=$run_app_test_source
compiler_learned_pattern_world_bundle_test_source=$learned_pattern_world_bundle_test_source
compiler_validate_tool_source=$validate_tool_source
compiler_run_tool_source=$run_tool_source
compiler_example_source=$example_source
compiler_world_example_source=$world_example_source
compiler_settlement_example_source=$settlement_example_source
compiler_negotiated_repair_example_source=$negotiated_repair_example_source
compiler_negotiated_repair_example_directory=$negotiated_repair_example_directory
compiler_learned_pattern_world_example_source=$learned_pattern_world_example_source
compiler_learned_pattern_world_example_directory=$learned_pattern_world_example_directory
compiler_learning_example_source=$learning_example_source
compiler_corpus_example_source=$corpus_example_source
compiler_pattern_example_source=$pattern_example_source
compiler_sequence_example_source=$sequence_example_source
compiler_text_completion_example_source=$text_completion_example_source
compiler_text_completion_example_directory=$text_completion_example_directory
compiler_text_pass_example_source=$text_pass_example_source
compiler_text_pass_example_directory=$text_pass_example_directory
compiler_music_example_source=$music_example_source
compiler_music_variation_example_source=$music_variation_example_source
compiler_music_variation_example_directory=$music_variation_example_directory
compiler_spatial_example_source=$spatial_example_source
compiler_trace_example_source=$trace_example_source
compiler_trace_example_directory=$trace_example_directory
compiler_negotiation_example_source=$negotiation_example_source
compiler_negotiation_example_directory=$negotiation_example_directory
compiler_building_example_source=$building_example_source
compiler_building_example_directory=$building_example_directory
compiler_building_common_directory=$building_common_directory
compiler_building_svg_source=$building_svg_source
compiler_world_common_directory=$world_common_directory
compiler_unit_output_directory=$unit_output_directory
compiler_binary_output_directory=$binary_output_directory
host_system=$(uname -s)
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*)
    compiler_source_directory=$(cygpath -m "$source_directory") || exit $?
    compiler_tools_directory=$(cygpath -m "$tools_directory") || exit $?
    compiler_test_source=$(cygpath -m "$test_source") || exit $?
    compiler_world_test_source=$(cygpath -m "$world_test_source") || exit $?
    compiler_settlement_test_source=$(cygpath -m "$settlement_test_source") || exit $?
    compiler_learning_test_source=$(cygpath -m "$learning_test_source") || exit $?
    compiler_pattern_test_source=$(cygpath -m "$pattern_test_source") || exit $?
    compiler_pattern_pass_test_source=$(cygpath -m "$pattern_pass_test_source") || exit $?
    compiler_sequence_test_source=$(cygpath -m "$sequence_test_source") || exit $?
    compiler_text_test_source=$(cygpath -m "$text_test_source") || exit $?
    compiler_text_pass_test_source=$(cygpath -m "$text_pass_test_source") || exit $?
    compiler_negotiation_test_source=$(cygpath -m "$negotiation_test_source") || exit $?
    compiler_selective_negotiation_test_source=$(cygpath -m "$selective_negotiation_test_source") || exit $?
    compiler_voxel_test_source=$(cygpath -m "$voxel_test_source") || exit $?
    compiler_building_test_source=$(cygpath -m "$building_test_source") || exit $?
    compiler_trace_reference_test_source=$(cygpath -m "$trace_reference_test_source") || exit $?
    compiler_trace_test_source=$(cygpath -m "$trace_test_source") || exit $?
    compiler_trace_utility_test_source=$(cygpath -m "$trace_utility_test_source") || exit $?
    compiler_isometric_test_source=$(cygpath -m "$isometric_test_source") || exit $?
    compiler_svg_test_source=$(cygpath -m "$svg_test_source") || exit $?
    compiler_building_view_test_source=$(cygpath -m "$building_view_test_source") || exit $?
    compiler_midi_test_source=$(cygpath -m "$midi_test_source") || exit $?
    compiler_music_test_source=$(cygpath -m "$music_test_source") || exit $?
    compiler_music_graph_test_source=$(cygpath -m "$music_graph_test_source") || exit $?
    compiler_music_midi_test_source=$(cygpath -m "$music_midi_test_source") || exit $?
    compiler_music_passes_test_source=$(cygpath -m "$music_passes_test_source") || exit $?
    compiler_music_passes_text_test_source=$(cygpath -m "$music_passes_text_test_source") || exit $?
    compiler_text_codec_test_source=$(cygpath -m "$text_codec_test_source") || exit $?
    compiler_rule_model_test_source=$(cygpath -m "$rule_model_test_source") || exit $?
    compiler_rule_text_test_source=$(cygpath -m "$rule_text_test_source") || exit $?
    compiler_pipeline_model_test_source=$(cygpath -m "$pipeline_model_test_source") || exit $?
    compiler_pipeline_text_test_source=$(cygpath -m "$pipeline_text_test_source") || exit $?
    compiler_token_lookup_test_source=$(cygpath -m "$token_lookup_test_source") || exit $?
    compiler_pipeline_compile_test_source=$(cygpath -m "$pipeline_compile_test_source") || exit $?
    compiler_pipeline_run_test_source=$(cygpath -m "$pipeline_run_test_source") || exit $?
    compiler_pipeline_run_text_test_source=$(cygpath -m "$pipeline_run_text_test_source") || exit $?
    compiler_pipeline_result_test_source=$(cygpath -m "$pipeline_result_test_source") || exit $?
    compiler_pipeline_result_text_test_source=$(cygpath -m "$pipeline_result_text_test_source") || exit $?
    compiler_pipeline_runtime_test_source=$(cygpath -m "$pipeline_runtime_test_source") || exit $?
    compiler_validate_app_test_source=$(cygpath -m "$validate_app_test_source") || exit $?
    compiler_run_app_test_source=$(cygpath -m "$run_app_test_source") || exit $?
    compiler_learned_pattern_world_bundle_test_source=$(cygpath -m "$learned_pattern_world_bundle_test_source") || exit $?
    compiler_validate_tool_source=$(cygpath -m "$validate_tool_source") || exit $?
    compiler_run_tool_source=$(cygpath -m "$run_tool_source") || exit $?
    compiler_example_source=$(cygpath -m "$example_source") || exit $?
    compiler_world_example_source=$(cygpath -m "$world_example_source") || exit $?
    compiler_settlement_example_source=$(cygpath -m "$settlement_example_source") || exit $?
    compiler_negotiated_repair_example_source=$(cygpath -m "$negotiated_repair_example_source") || exit $?
    compiler_negotiated_repair_example_directory=$(cygpath -m "$negotiated_repair_example_directory") || exit $?
    compiler_learned_pattern_world_example_source=$(cygpath -m "$learned_pattern_world_example_source") || exit $?
    compiler_learned_pattern_world_example_directory=$(cygpath -m "$learned_pattern_world_example_directory") || exit $?
    compiler_learning_example_source=$(cygpath -m "$learning_example_source") || exit $?
    compiler_corpus_example_source=$(cygpath -m "$corpus_example_source") || exit $?
    compiler_pattern_example_source=$(cygpath -m "$pattern_example_source") || exit $?
    compiler_sequence_example_source=$(cygpath -m "$sequence_example_source") || exit $?
    compiler_text_completion_example_source=$(cygpath -m "$text_completion_example_source") || exit $?
    compiler_text_completion_example_directory=$(cygpath -m "$text_completion_example_directory") || exit $?
    compiler_text_pass_example_source=$(cygpath -m "$text_pass_example_source") || exit $?
    compiler_text_pass_example_directory=$(cygpath -m "$text_pass_example_directory") || exit $?
    compiler_music_example_source=$(cygpath -m "$music_example_source") || exit $?
    compiler_music_variation_example_source=$(cygpath -m "$music_variation_example_source") || exit $?
    compiler_music_variation_example_directory=$(cygpath -m "$music_variation_example_directory") || exit $?
    compiler_spatial_example_source=$(cygpath -m "$spatial_example_source") || exit $?
    compiler_trace_example_source=$(cygpath -m "$trace_example_source") || exit $?
    compiler_trace_example_directory=$(cygpath -m "$trace_example_directory") || exit $?
    compiler_negotiation_example_source=$(cygpath -m "$negotiation_example_source") || exit $?
    compiler_negotiation_example_directory=$(cygpath -m "$negotiation_example_directory") || exit $?
    compiler_building_example_source=$(cygpath -m "$building_example_source") || exit $?
    compiler_building_example_directory=$(cygpath -m "$building_example_directory") || exit $?
    compiler_building_common_directory=$(cygpath -m "$building_common_directory") || exit $?
    compiler_building_svg_source=$(cygpath -m "$building_svg_source") || exit $?
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

printf "Building the pattern-projected-pass conformance suite.\n"
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
  "$compiler_pattern_pass_test_source" || exit $?

pattern_pass_test_executable="$binary_output_directory/wfc_pattern2d_passes_test"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) pattern_pass_test_executable="${pattern_pass_test_executable}.exe" ;;
esac

printf "Running '%s'.\n" "$pattern_pass_test_executable"
"$pattern_pass_test_executable" || exit $?

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

printf "Building the text-completion conformance suite.\n"
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
  "$compiler_text_test_source" || exit $?

text_test_executable="$binary_output_directory/wfc_text_test"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) text_test_executable="${text_test_executable}.exe" ;;
esac

printf "Running '%s'.\n" "$text_test_executable"
"$text_test_executable" || exit $?

printf "Building the multi-pass text conformance suite.\n"
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
  "$compiler_text_pass_test_source" || exit $?

text_pass_test_executable="$binary_output_directory/wfc_text_passes_test"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) text_pass_test_executable="${text_pass_test_executable}.exe" ;;
esac

printf "Running '%s'.\n" "$text_pass_test_executable"
"$text_pass_test_executable" || exit $?

printf "Building the bounded pass-negotiation conformance suite.\n"
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
  "$compiler_negotiation_test_source" || exit $?

negotiation_test_executable="$binary_output_directory/wfc_negotiation_test"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) negotiation_test_executable="${negotiation_test_executable}.exe" ;;
esac

printf "Running '%s'.\n" "$negotiation_test_executable"
"$negotiation_test_executable" || exit $?

printf "Building the selective pass-negotiation conformance suite.\n"
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
  "$compiler_selective_negotiation_test_source" || exit $?

selective_negotiation_test_executable="$binary_output_directory/wfc_selective_negotiation_test"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) selective_negotiation_test_executable="${selective_negotiation_test_executable}.exe" ;;
esac

printf "Running '%s'.\n" "$selective_negotiation_test_executable"
"$selective_negotiation_test_executable" || exit $?

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

for compiler_trace_suite in \
  "$compiler_trace_reference_test_source" \
  "$compiler_trace_test_source" \
  "$compiler_trace_utility_test_source"
do
  trace_suite_name=$(basename "$compiler_trace_suite" .lpr)
  printf "Building the causal-trace conformance suite '%s'.\n" "$trace_suite_name"
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
    "$compiler_trace_suite" || exit $?

  trace_suite_executable="$binary_output_directory/$trace_suite_name"
  case "$host_system" in
    CYGWIN*|MINGW*|MSYS*) trace_suite_executable="${trace_suite_executable}.exe" ;;
  esac
  printf "Running '%s'.\n" "$trace_suite_executable"
  "$trace_suite_executable" || exit $?
done

for compiler_viewer_suite in \
  "$compiler_isometric_test_source" \
  "$compiler_svg_test_source" \
  "$compiler_building_view_test_source"
do
  viewer_suite_name=$(basename -- "$compiler_viewer_suite" .lpr)
  printf "Building the 3D presentation conformance suite '%s'.\n" \
    "$viewer_suite_name"
  "$compiler" "$@" \
    -B \
    -Mdelphi \
    -Sa \
    -Cr \
    -Co \
    -Ci \
    "-Fu$compiler_source_directory" \
    "-Fu$compiler_building_common_directory" \
    "-FU$compiler_unit_output_directory" \
    "-FE$compiler_binary_output_directory" \
    "$compiler_viewer_suite" || exit $?

  viewer_suite_executable="$binary_output_directory/$viewer_suite_name"
  case "$host_system" in
    CYGWIN*|MINGW*|MSYS*) viewer_suite_executable="${viewer_suite_executable}.exe" ;;
  esac
  printf "Running '%s'.\n" "$viewer_suite_executable"
  "$viewer_suite_executable" || exit $?
done

for compiler_music_suite in \
  "$compiler_midi_test_source" \
  "$compiler_music_test_source" \
  "$compiler_music_graph_test_source" \
  "$compiler_music_midi_test_source" \
  "$compiler_music_passes_test_source" \
  "$compiler_music_passes_text_test_source"
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

for compiler_artifact_suite in \
  "$compiler_text_codec_test_source" \
  "$compiler_rule_model_test_source" \
  "$compiler_rule_text_test_source" \
  "$compiler_pipeline_model_test_source" \
  "$compiler_pipeline_text_test_source" \
  "$compiler_token_lookup_test_source" \
  "$compiler_pipeline_compile_test_source" \
  "$compiler_pipeline_run_test_source" \
  "$compiler_pipeline_run_text_test_source" \
  "$compiler_pipeline_result_test_source" \
  "$compiler_pipeline_result_text_test_source" \
  "$compiler_pipeline_runtime_test_source" \
  "$compiler_validate_app_test_source" \
  "$compiler_run_app_test_source" \
  "$compiler_learned_pattern_world_bundle_test_source"
do
  artifact_suite_name=$(basename -- "$compiler_artifact_suite" .lpr)
  printf "Building the portable-artifact suite '%s'.\n" \
    "$artifact_suite_name"
  "$compiler" "$@" \
    -B \
    -Mdelphi \
    -Sa \
    -Cr \
    -Co \
    -Ci \
    "-Fu$compiler_source_directory" \
    "-Fu$compiler_tools_directory" \
    "-Fu$compiler_learned_pattern_world_example_directory" \
    "-FU$compiler_unit_output_directory" \
    "-FE$compiler_binary_output_directory" \
    "$compiler_artifact_suite" || exit $?

  artifact_suite_executable="$binary_output_directory/$artifact_suite_name"
  case "$host_system" in
    CYGWIN*|MINGW*|MSYS*) artifact_suite_executable="${artifact_suite_executable}.exe" ;;
  esac
  printf "Running '%s'.\n" "$artifact_suite_executable"
  if [[ "$artifact_suite_name" == wfc_learned_pattern_world_bundle_test ]]; then
    "$artifact_suite_executable" \
      "$compiler_learned_pattern_world_example_directory/pipeline" || exit $?
  else
    "$artifact_suite_executable" || exit $?
  fi
done

for compiler_tool_source in \
  "$compiler_validate_tool_source" \
  "$compiler_run_tool_source"
do
  tool_name=$(basename -- "$compiler_tool_source" .lpr)
  printf "Building the portable command-line host '%s'.\n" "$tool_name"
  "$compiler" "$@" \
    -B \
    -Mdelphi \
    -Sa \
    -Cr \
    -Co \
    -Ci \
    "-Fu$compiler_source_directory" \
    "-Fu$compiler_tools_directory" \
    "-FU$compiler_unit_output_directory" \
    "-FE$compiler_binary_output_directory" \
    "$compiler_tool_source" || exit $?

  tool_executable="$binary_output_directory/$tool_name"
  case "$host_system" in
    CYGWIN*|MINGW*|MSYS*) tool_executable="${tool_executable}.exe" ;;
  esac
  printf "Smoke testing '%s --version'.\n" "$tool_executable"
  "$tool_executable" --version || exit $?
done

validator_tool_executable="$binary_output_directory/wfc_validate"
runner_tool_executable="$binary_output_directory/wfc_run"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*)
    validator_tool_executable="${validator_tool_executable}.exe"
    runner_tool_executable="${runner_tool_executable}.exe"
    ;;
esac
printf 'Running the portable pipeline CLI process conformance suite.\n'
bash "$pipeline_cli_process_test_source" \
  "$validator_tool_executable" -- "$runner_tool_executable" || exit $?

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

printf "Building the dependency-free negotiated-repair example.\n"
"$compiler" "$@" \
  -B \
  -Mdelphi \
  -Sa \
  -Cr \
  -Co \
  -Ci \
  "-Fu$compiler_source_directory" \
  "-Fu$compiler_negotiated_repair_example_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_binary_output_directory" \
  "$compiler_negotiated_repair_example_source" || exit $?

negotiated_repair_example_executable="$binary_output_directory/NegotiatedRepair"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) negotiated_repair_example_executable="${negotiated_repair_example_executable}.exe" ;;
esac

printf "Smoke testing '%s'.\n" "$negotiated_repair_example_executable"
"$negotiated_repair_example_executable" >/dev/null || exit $?

printf "Building the dependency-free learned-pattern-world example.\n"
"$compiler" "$@" \
  -B \
  -Mdelphi \
  -Sa \
  -Cr \
  -Co \
  -Ci \
  "-Fu$compiler_source_directory" \
  "-Fu$compiler_learned_pattern_world_example_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_binary_output_directory" \
  "$compiler_learned_pattern_world_example_source" || exit $?

learned_pattern_world_example_executable="$binary_output_directory/LearnedPatternWorld"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) learned_pattern_world_example_executable="${learned_pattern_world_example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$learned_pattern_world_example_executable"
"$learned_pattern_world_example_executable" 0 >/dev/null || exit $?

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

printf "Building the portable text constraint-completion example.\n"
"$compiler" "$@" \
  -B \
  -Mdelphi \
  -Sa \
  -Cr \
  -Co \
  -Ci \
  "-Fu$compiler_source_directory" \
  "-Fu$compiler_text_completion_example_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_binary_output_directory" \
  "$compiler_text_completion_example_source" || exit $?

text_completion_example_executable="$binary_output_directory/ConstraintCompletion"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) text_completion_example_executable="${text_completion_example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$text_completion_example_executable"
"$text_completion_example_executable" 0 >/dev/null || exit $?

printf "Building the dependency-free multi-pass text example.\n"
"$compiler" "$@" \
  -B \
  -Mdelphi \
  -Sa \
  -Cr \
  -Co \
  -Ci \
  "-Fu$compiler_source_directory" \
  "-Fu$compiler_text_pass_example_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_binary_output_directory" \
  "$compiler_text_pass_example_source" || exit $?

text_pass_example_executable="$binary_output_directory/TextPassComposition"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) text_pass_example_executable="${text_pass_example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$text_pass_example_executable"
"$text_pass_example_executable" 0 >/dev/null || exit $?

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

printf "Building the negotiated music-variation example.\n"
"$compiler" "$@" \
  -B \
  -Mdelphi \
  -Sa \
  -Cr \
  -Co \
  -Ci \
  "-Fu$compiler_source_directory" \
  "-Fu$compiler_music_variation_example_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_binary_output_directory" \
  "$compiler_music_variation_example_source" || exit $?

music_variation_example_executable="$binary_output_directory/NegotiatedVariation"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) music_variation_example_executable="${music_variation_example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$music_variation_example_executable"
"$music_variation_example_executable" 0 >/dev/null || exit $?

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

printf "Building the dependency-free causal-trace inspector example.\n"
"$compiler" "$@" \
  -B \
  -Mdelphi \
  -Sa \
  -Cr \
  -Co \
  -Ci \
  "-Fu$compiler_source_directory" \
  "-Fu$compiler_trace_example_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_binary_output_directory" \
  "$compiler_trace_example_source" || exit $?

trace_example_executable="$binary_output_directory/TraceInspector"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) trace_example_executable="${trace_example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$trace_example_executable"
"$trace_example_executable" 0 >/dev/null || exit $?

printf "Building the dependency-free pass-negotiation example.\n"
"$compiler" "$@" \
  -B \
  -Mdelphi \
  -Sa \
  -Cr \
  -Co \
  -Ci \
  "-Fu$compiler_source_directory" \
  "-Fu$compiler_negotiation_example_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_binary_output_directory" \
  "$compiler_negotiation_example_source" || exit $?

negotiation_example_executable="$binary_output_directory/PassNegotiation"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) negotiation_example_executable="${negotiation_example_executable}.exe" ;;
esac

printf "Smoke testing '%s'.\n" "$negotiation_example_executable"
"$negotiation_example_executable" >/dev/null || exit $?

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
  "-Fu$compiler_building_common_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_binary_output_directory" \
  "$compiler_building_example_source" || exit $?

building_example_executable="$binary_output_directory/MultiPassBuilding"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) building_example_executable="${building_example_executable}.exe" ;;
esac

printf "Smoke testing '%s' with seed 0.\n" "$building_example_executable"
"$building_example_executable" 0 >/dev/null || exit $?

printf "Building the dependency-free Building 3D SVG example.\n"
"$compiler" "$@" \
  -B \
  -Mdelphi \
  -Sa \
  -Cr \
  -Co \
  -Ci \
  "-Fu$compiler_source_directory" \
  "-Fu$compiler_building_common_directory" \
  "-FU$compiler_unit_output_directory" \
  "-FE$compiler_binary_output_directory" \
  "$compiler_building_svg_source" || exit $?

building_svg_executable="$binary_output_directory/Building3DSvg"
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*) building_svg_executable="${building_svg_executable}.exe" ;;
esac
building_svg_output="$binary_output_directory/building3d-seed-zero.svg"
building_svg_runtime_output=$building_svg_output
case "$host_system" in
  CYGWIN*|MINGW*|MSYS*)
    building_svg_runtime_output=$(cygpath -m "$building_svg_output") || exit $?
    ;;
esac
printf "Smoke testing '%s' with seed 0.\n" "$building_svg_executable"
"$building_svg_executable" 0 "$building_svg_runtime_output" >/dev/null || exit $?
