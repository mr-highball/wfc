{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Shared workspace controller integration checks. }
program pipeline_workspace_workbench_test;
{$mode delphi}{$H+}
uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  wfc_workspace_replay_fixture, pipeline_workspace_workbench_checks,
  pipeline_workspace_geometry_checks;
begin
  ReplayChecks:=0;
  {$IFDEF WORKBENCH_EXPORT_JOURNALS}ExportReplayDocuments:=True;{$ENDIF}
  TestPipelineWorkspaceWorkbench;
  TestPipelineWorkspaceGeometry;
  WriteLn('Pipeline workspace workbench checks: ',ReplayChecks);
end.
