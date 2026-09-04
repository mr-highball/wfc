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
    P.Targets.AddUnit('wfc_model.pas');
    P.Targets.AddUnit('wfc_learn.pas');
    P.Targets.AddUnit('wfc_text_codec.pas');
    P.Targets.AddUnit('wfc_model_text.pas');
    P.Targets.AddUnit('wfc_pattern2d.pas');
    P.Targets.AddUnit('wfc_pattern2d_learn.pas');
    P.Targets.AddUnit('wfc_pattern2d_text.pas');
    P.Targets.AddUnit('wfc_world2d.pas');
    P.Targets.AddUnit('wfc_world2d_validate.pas');
    Run;
  end;
end.
