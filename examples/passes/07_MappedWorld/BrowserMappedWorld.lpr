{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program BrowserMappedWorld;
{$mode delphi}{$H+}
uses browser_mapped_world_app;
var Application: TBrowserMappedWorldApplication;
begin
  Application := TBrowserMappedWorldApplication.Create;
  Application.Run;
end.
