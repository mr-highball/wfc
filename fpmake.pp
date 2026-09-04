program fpmake;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  CThreads,
  {$ENDIF}
  SysUtils,
  fpmkunit;

var
  P: TPackage;
begin
  with Installer do
  begin
    P := AddPackage('wfc');
    P.Version := '0.1.0';
    P.Author := 'mr-highball';
    P.License := 'MIT';
    P.HomepageURL := 'https://github.com/mr-highball/wfc';
    P.Description := 'Wave Function Collapse, multi-pass generation, and pattern learning library';
    P.Dependencies.Add('rtl-generics');
    P.SetUnitsOutputDir('build' + PathDelim + 'fpm' + PathDelim + 'units' +
      PathDelim + '$(target)');
    P.SourcePath.Add('src');
    P.Targets.AddUnit('wfc_solver_reference.pas');
    P.Targets.AddUnit('wfc.pas');
    P.Targets.AddUnit('wfc_trace.pas');
    P.Targets.AddUnit('wfc_model.pas');
    P.Targets.AddUnit('wfc_learn.pas');
    P.Targets.AddUnit('wfc_text_codec.pas');
    P.Targets.AddUnit('wfc_model_text.pas');
    P.Targets.AddUnit('wfc_pattern2d.pas');
    P.Targets.AddUnit('wfc_pattern2d_learn.pas');
    P.Targets.AddUnit('wfc_pattern2d_graph.pas');
    P.Targets.AddUnit('wfc_pattern2d_text.pas');
    P.Targets.AddUnit('wfc_sequence.pas');
    P.Targets.AddUnit('wfc_sequence_learn.pas');
    P.Targets.AddUnit('wfc_sequence_graph.pas');
    P.Targets.AddUnit('wfc_sequence_analyze.pas');
    P.Targets.AddUnit('wfc_sequence_text.pas');
    P.Targets.AddUnit('wfc_text_tokenize.pas');
    P.Targets.AddUnit('wfc_text_complete.pas');
    P.Targets.AddUnit('wfc_text_passes.pas');
    P.Targets.AddUnit('wfc_music.pas');
    P.Targets.AddUnit('wfc_music_sequence.pas');
    P.Targets.AddUnit('wfc_music_graph.pas');
    P.Targets.AddUnit('wfc_music_passes.pas');
    P.Targets.AddUnit('wfc_music_passes_text.pas');
    P.Targets.AddUnit('wfc_music_text.pas');
    P.Targets.AddUnit('wfc_midi_smf.pas');
    P.Targets.AddUnit('wfc_music_midi.pas');
    P.Targets.AddUnit('wfc_world2d.pas');
    P.Targets.AddUnit('wfc_world2d_validate.pas');
    P.Targets.AddUnit('wfc_world2d_settlement.pas');
    P.Targets.AddUnit('wfc_world2d_settlement_validate.pas');
    P.Targets.AddUnit('wfc_voxel3d.pas');
    P.Targets.AddUnit('wfc_voxel3d_validate.pas');
    P.Targets.AddUnit('wfc_voxel3d_mesh.pas');
    P.Targets.AddUnit('wfc_voxel3d_isometric.pas');
    P.Targets.AddUnit('wfc_voxel3d_svg.pas');
    P.Targets.AddUnit('wfc_voxel3d_passes.pas');
    P.Targets.AddUnit('wfc_building3d.pas');
    P.Targets.AddUnit('wfc_building3d_validate.pas');
    P.Targets.AddUnit('wfc_building3d_view.pas');
    P.Targets.AddUnit('wfc_rule_model.pas');
    P.Targets.AddUnit('wfc_rule_text.pas');
    P.Targets.AddUnit('wfc_pipeline_model.pas');
    P.Targets.AddUnit('wfc_pipeline_text.pas');
    Run;
  end;
end.
