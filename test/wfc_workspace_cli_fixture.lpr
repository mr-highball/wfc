{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Native fixture publisher and independent actual-history CLI oracle. }
program wfc_workspace_cli_fixture;
{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL workspace CLI fixtures require native FPC}{$ENDIF}
uses Classes,SysUtils,wfc_pipeline_session,wfc_pipeline_workspace_journal,
  wfc_pipeline_workspace_journal_text,wfc_workspace_replay_fixture,wfc_atomic_new_file;
procedure Publish(const Text,Path: String);
var F: TWfcAtomicNewFile; Bytes: array of Byte; I: Integer;
begin
  SetLength(Bytes,Length(Text)); for I:=1 to Length(Text) do Bytes[I-1]:=Ord(Text[I]);
  F:=TWfcAtomicNewFile.Create(Path); try F.WriteBytes(Bytes); F.Publish; finally F.Free; end;
end;
function ReadText(const Path: String): String;
var F: TFileStream;
begin
  F:=TFileStream.Create(Path,fmOpenRead or fmShareDenyWrite);
  try
    if F.Size>JournalLimits.MaxEncodedTextBytes then raise Exception.Create('fixture input exceeds budget');
    SetLength(Result,Integer(F.Size)); if Length(Result)>0 then F.ReadBuffer(Result[1],Length(Result));
  finally F.Free; end;
end;
procedure Compare(const Expected,Actual: TWfcPipelineWorkspaceJournal; const Count: Integer);
var I,K: Integer; A,B: TWfcPipelineWorkspaceAction;
begin
  Check(Actual.ActionCount=Count,'CLI records every expected action');
  Check(Actual.RunCount=Count-2+Ord(Count<12),'CLI records a new explicit run except each initial');
  Check(Actual.RecipeCount=1+Ord(Count>9),'CLI records each explicit recipe epoch');
  for I:=0 to Count-1 do
  begin
    A:=Expected.ActionAt(I); B:=Actual.ActionAt(I);
    Check(A.Kind=B.Kind,'CLI actual action kind');
    Check(A.EvidenceText=B.EvidenceText,'CLI complete actual evidence matches independent direct historian');
    Check(Length(A.RequestedRootIndices)=Length(B.RequestedRootIndices),'CLI actual explicit root count');
    for K:=0 to High(A.RequestedRootIndices) do Check(A.RequestedRootIndices[K]=B.RequestedRootIndices[K],'CLI actual explicit root index');
    Check(Expected.RunTextAt(A.RunIndex).Text=Actual.RunTextAt(B.RunIndex).Text,'CLI complete invocation bytes');
    Check(Expected.RecipeTextAt(Expected.RunTextAt(A.RunIndex).RecipeIndex)=
      Actual.RecipeTextAt(Actual.RunTextAt(B.RunIndex).RecipeIndex),'CLI complete recipe bytes/context binding');
  end;
end;
var J,Actual: TWfcPipelineWorkspaceJournal; State,Baseline: TWfcPipelineSessionPublicState;
  Revision,I: Integer; Dir,Text: String; Actions: TWfcPipelineWorkspaceActions;
begin
  J:=nil; Actual:=nil; State:=nil; Baseline:=nil;
  try
    J:=FixtureJournal(False,State,Baseline,Revision);
    if (ParamCount=2) and (ParamStr(1)='--make') then
    begin
      Dir:=IncludeTrailingPathDelimiter(ParamStr(2));
      for I:=0 to J.RecipeCount-1 do Publish(J.RecipeTextAt(I),Dir+'recipe'+IntToStr(I)+'.wfc');
      for I:=0 to J.RunCount-1 do Publish(J.RunTextAt(I).Text,Dir+'run'+IntToStr(I)+'.wfc');
      Publish(J.CopyCanonicalText,Dir+'direct.journal');
      Actions:=J.CopyActions; Text:=Actions[1].EvidenceText;
      Actions[1].EvidenceText:=StringReplace(Text,'solved=1'#10,'solved=0'#10,[]);
      Actual:=CopyWithActions(J,Actions); Publish(Actual.CopyCanonicalText,Dir+'forged.journal');
    end
    else if (ParamCount=3) and (ParamStr(1)='--compare') then
    begin
      Actual:=DecodeWfcPipelineWorkspaceJournalText(ReadText(ParamStr(2)),JournalLimits);
      Compare(J,Actual,StrToInt(ParamStr(3)));
      WriteLn('CLI direct-history checks: ',ReplayChecks);
    end
    else raise Exception.Create('--make DIRECTORY or --compare JOURNAL ACTION_COUNT required');
  finally Actual.Free; J.Free; Baseline.Free; State.Free; end;
end.
