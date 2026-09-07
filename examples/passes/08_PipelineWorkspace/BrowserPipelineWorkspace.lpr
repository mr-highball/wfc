{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program BrowserPipelineWorkspace;
{$mode delphi}{$H+}
uses browser_pipeline_workspace_app;
var Application: TBrowserPipelineWorkspaceApplication;
begin
  Application:=TBrowserPipelineWorkspaceApplication.Create;
  Application.Run;
end.
