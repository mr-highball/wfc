{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Closed canonical outer journal codec; no execution or replay. }
unit wfc_pipeline_workspace_journal_text;
{$mode delphi}{$H+}
interface
uses wfc_pipeline_workspace_journal;
function EncodeWfcPipelineWorkspaceJournalText(
  const Journal: TWfcPipelineWorkspaceJournal): String;
function DecodeWfcPipelineWorkspaceJournalText(const Text: String;
  const Limits: TWfcPipelineWorkspaceJournalLimits): TWfcPipelineWorkspaceJournal;
implementation
uses SysUtils,wfc_model,wfc_pipeline_workspace_context,wfc_text_codec;
const Artifact='WFC workspace journal';
type
  TJournalParser = class
  private
    FText: String;
    FPosition: Integer;
    FLimits: TWfcPipelineWorkspaceJournalLimits;
    FRecipes: TWfcPipelineWorkspaceRecipeTexts;
    FRuns: TWfcPipelineWorkspaceRunTexts;
    FActions: TWfcPipelineWorkspaceActions;
    function TakeLine: String;
    procedure Literal(const Expected: String);
    function Field(const Prefix: String): String;
    function Number(const Prefix: String): Integer;
    function Document(const Prefix: String; const Declared: Integer;
      const AllowEmpty,Keep: Boolean): String;
    procedure Scan(const Keep: Boolean);
  public
    constructor Create(const Text: String; const Limits: TWfcPipelineWorkspaceJournalLimits);
    function Decode: TWfcPipelineWorkspaceJournal;
  end;
procedure TextError(const Detail: String);
begin WfcTextError(Artifact,Detail); end;
procedure Charge(var Used: Integer; const Count,Maximum: Integer; const Name: String);
begin
  if (Count<0) or (Count>Maximum) or (Used>Maximum-Count) then TextError(Name+' budget/count exceeded');
  Inc(Used,Count);
end;
procedure GuardText(const Text: String; const Limits: TWfcPipelineWorkspaceJournalLimits);
var I: Integer; {$IFDEF PAS2JS}Valid: Boolean;{$ENDIF}
begin
  ValidateWfcPipelineWorkspaceJournalLimits(Limits);
  {$IFDEF PAS2JS}asm Valid=typeof Text==='string'; end;
  if not Valid then TextError('document must be a primitive string');{$ENDIF}
  if (Length(Text)=0) or (Length(Text)>Limits.MaxEncodedTextBytes) then TextError('outer encoded text envelope exceeded or empty');
  if Text[Length(Text)]<>#10 then TextError('document must end with LF');
  for I:=1 to Length(Text) do
    if (Text[I]<>#10) and ((Ord(Text[I])<32) or (Ord(Text[I])>126)) then
      TextError('outer document requires printable ASCII and LF');
end;
function Unreserved(const Ch: Char): Boolean;
begin Result:=(Ch in ['A'..'Z','a'..'z','0'..'9','-','.','_','~']); end;
function Hex(const Ch: Char): Integer;
begin
  if Ch in ['0'..'9'] then Result:=Ord(Ch)-Ord('0')
  else if Ch in ['A'..'F'] then Result:=Ord(Ch)-Ord('A')+10
  else begin TextError('percent escape requires uppercase hexadecimal'); Result:=0; end;
end;
procedure ScanEncodedDocument(const Encoded: String; const Declared: Integer; const AllowEmpty: Boolean);
var I,Bytes,Code,LastCode: Integer;
begin
  I:=1; Bytes:=0; LastCode:=-1;
  while I<=Length(Encoded) do
  begin
    if Unreserved(Encoded[I]) then begin Code:=Ord(Encoded[I]); Inc(I); end
    else if Encoded[I]='%' then
    begin
      if I>Length(Encoded)-2 then TextError('truncated percent escape');
      Code:=Hex(Encoded[I+1])*16+Hex(Encoded[I+2]);
      if Unreserved(Char(Code)) then TextError('unnecessary percent escape');
      Inc(I,3);
    end
    else begin TextError('noncanonical unescaped document byte'); Code:=0; end;
    if (Code<>10) and ((Code<32) or (Code>126)) then TextError('embedded document must contain printable ASCII fields and LF');
    if (Code=10) and ((Bytes=0) or (LastCode=10)) then TextError('embedded document has a blank line');
    Charge(Bytes,1,Declared,'declared embedded bytes'); LastCode:=Code;
  end;
  if Bytes<>Declared then TextError('declared embedded byte count differs from actual document');
  if Bytes=0 then begin if not AllowEmpty then TextError('embedded document cannot be empty'); end
  else if LastCode<>10 then TextError('embedded document requires final LF');
end;
constructor TJournalParser.Create(const Text: String; const Limits: TWfcPipelineWorkspaceJournalLimits);
begin inherited Create; GuardText(Text,Limits); FText:=Text; FLimits:=Limits; end;
function TJournalParser.TakeLine: String;
var Finish: Integer;
begin
  if (FPosition=0) or (FPosition>Length(FText)) then TextError('document is truncated');
  Finish:=WfcTextFindCharacter(FText,#10,FPosition);
  if Finish=0 then TextError('missing final LF');
  if Finish=FPosition then TextError('blank lines are not allowed');
  Result:=Copy(FText,FPosition,Finish-FPosition);
  if Finish=Length(FText) then FPosition:=0 else FPosition:=Finish+1;
end;
procedure TJournalParser.Literal(const Expected: String);
begin if TakeLine<>Expected then TextError('expected '+Expected); end;
function TJournalParser.Field(const Prefix: String): String;
begin Result:=WfcTextValueAfterPrefix(TakeLine,Prefix,Prefix,Artifact); end;
function TJournalParser.Number(const Prefix: String): Integer;
begin Result:=WfcTextParseCanonicalInteger(Field(Prefix),Prefix,Artifact); end;
function TJournalParser.Document(const Prefix: String; const Declared: Integer;
  const AllowEmpty,Keep: Boolean): String;
var Encoded: String; Token: TWfcModelToken;
begin
  Encoded:=Field(Prefix); ScanEncodedDocument(Encoded,Declared,AllowEmpty); Result:='';
  if Keep then
  begin
    { The first full scan already checked every individual/aggregate decoded
      byte charge. The maintained codec remains the actual token decoder. }
    Token:=WfcTextDecodeToken(Encoded,Artifact); Result:=String(Token);
    if Length(Result)<>Declared then TextError('maintained decoder differs from byte preflight');
  end;
end;
function KindOf(const Name: String): TWfcPipelineWorkspaceActionKind;
begin
  if Name='begin-epoch' then Result:=wpwakBeginEpoch
  else if Name='edit' then Result:=wpwakEdit
  else if Name='initial' then Result:=wpwakInitial
  else if Name='repair' then Result:=wpwakRepair
  else begin TextError('unknown action kind'); Result:=wpwakBeginEpoch; end;
end;
procedure TJournalParser.Scan(const Keep: Boolean);
var RecipeCount,RunCount,ActionCount,ClaimRoots,ClaimContext,ClaimEvidence: Integer;
  Roots,ContextBytes,EvidenceBytes,I,J,Bytes,RecipeIndex,RunIndex,RootCount,Root,LastRoot: Integer;
  Prefix,Value: String; Kind: TWfcPipelineWorkspaceActionKind;
begin
  FPosition:=1; Roots:=0; ContextBytes:=0; EvidenceBytes:=0;
  Literal('wfc-workspace-journal=1'); Literal('verification=unverified-claims');
  RecipeCount:=Number('recipe-count='); RunCount:=Number('run-count='); ActionCount:=Number('action-count=');
  ClaimRoots:=Number('root-reference-count='); ClaimContext:=Number('context-text-bytes='); ClaimEvidence:=Number('evidence-text-bytes=');
  if FPosition=0 then TextError('document is truncated after aggregate header');
  if (RecipeCount<1) or (RecipeCount>FLimits.MaxRecipes) or (RunCount<1) or (RunCount>FLimits.MaxRuns) or
    (ActionCount<1) or (ActionCount>FLimits.MaxActions) then TextError('positive context/action counts must fit caller limits');
  if (ClaimRoots>FLimits.MaxRootReferences) or (ClaimContext>FLimits.MaxContextTextBytes) or
    (ClaimEvidence>FLimits.MaxEvidenceTextBytes) then TextError('aggregate header exceeds caller limits');
  { Declared totals are not allocation authority. At least one remaining byte
    must exist per row/reference, and encoded documents cannot be shorter than
    their decoded ASCII payloads. Actual exact totals are independently scanned. }
  if (RecipeCount>Length(FText)-FPosition+1) or (RunCount>Length(FText)-FPosition+1) or
    (ActionCount>Length(FText)-FPosition+1) or (ClaimRoots>Length(FText)-FPosition+1) or
    (ClaimEvidence>Length(FText)) or (ClaimContext>Length(FText)-ClaimEvidence) then
    TextError('declared counts/bytes exceed bounded remaining outer text');
  if Keep then
  begin
    SetLength(FRecipes,RecipeCount); SetLength(FRuns,RunCount); SetLength(FActions,ActionCount);
  end;
  for I:=0 to RecipeCount-1 do
  begin
    Prefix:='recipe.'+IntToStr(I); Bytes:=Number(Prefix+'.bytes=');
    Charge(ContextBytes,Bytes,ClaimContext,'aggregate context bytes');
    Value:=Document(Prefix+'.text=',Bytes,False,Keep); if Keep then FRecipes[I]:=Value;
  end;
  for I:=0 to RunCount-1 do
  begin
    Prefix:='run.'+IntToStr(I); RecipeIndex:=Number(Prefix+'.recipe=');
    if RecipeIndex>=RecipeCount then TextError('run recipe index out of range');
    Bytes:=Number(Prefix+'.bytes='); Charge(ContextBytes,Bytes,ClaimContext,'aggregate context bytes');
    Value:=Document(Prefix+'.text=',Bytes,False,Keep);
    if Keep then begin FRuns[I].RecipeIndex:=RecipeIndex; FRuns[I].Text:=Value; end;
  end;
  for I:=0 to ActionCount-1 do
  begin
    Prefix:='action.'+IntToStr(I); Kind:=KindOf(Field(Prefix+'.kind=')); RunIndex:=Number(Prefix+'.run=');
    if RunIndex>=RunCount then TextError('action run index out of range');
    RootCount:=Number(Prefix+'.root-count='); Charge(Roots,RootCount,ClaimRoots,'aggregate root references');
    if (Kind=wpwakRepair) and (RootCount=0) then TextError('repair roots cannot be empty');
    if (Kind<>wpwakRepair) and (RootCount<>0) then TextError('only repair carries roots');
    if Keep then
    begin FActions[I].Kind:=Kind; FActions[I].RunIndex:=RunIndex; SetLength(FActions[I].RequestedRootIndices,RootCount); end;
    LastRoot:=-1;
    for J:=0 to RootCount-1 do
    begin
      Root:=Number(Prefix+'.root.'+IntToStr(J)+'=');
      if Root<=LastRoot then TextError('stored roots must be strictly ascending and unique');
      LastRoot:=Root; if Keep then FActions[I].RequestedRootIndices[J]:=Root;
    end;
    Bytes:=Number(Prefix+'.evidence-bytes='); Charge(EvidenceBytes,Bytes,ClaimEvidence,'aggregate evidence bytes');
    if (Kind=wpwakBeginEpoch) and (Bytes<>0) then TextError('begin-epoch evidence must be empty');
    Value:=Document(Prefix+'.evidence=',Bytes,Kind=wpwakBeginEpoch,Keep);
    if Keep then FActions[I].EvidenceText:=Value;
  end;
  Literal('end=1'); if FPosition<>0 then TextError('trailing fields are not permitted');
  if (ContextBytes<>ClaimContext) or (EvidenceBytes<>ClaimEvidence) or (Roots<>ClaimRoots) then
    TextError('declared aggregate count differs from actual rows');
end;
function TJournalParser.Decode: TWfcPipelineWorkspaceJournal;
begin
  Result:=nil;
  { Entire outer grammar and every decoded byte/root/count envelope is scanned
    without token decoding, owned row/root arrays or typed context decoders. }
  Scan(False); Scan(True);
  Result:=TWfcPipelineWorkspaceJournal.Create(FRecipes,FRuns,FActions,FLimits);
  try
    if Result.CopyCanonicalText<>FText then TextError('document is not the exact canonical outer representation');
  except Result.Free; Result:=nil; raise; end;
end;
function EncodeWfcPipelineWorkspaceJournalText(const Journal: TWfcPipelineWorkspaceJournal): String;
begin
  if Journal=nil then TextError('journal owner is required');
  Result:=Journal.CopyCanonicalText;
end;
function DecodeWfcPipelineWorkspaceJournalText(const Text: String;
  const Limits: TWfcPipelineWorkspaceJournalLimits): TWfcPipelineWorkspaceJournal;
var Parser: TJournalParser;
begin
  Parser:=nil; Result:=nil;
  try
    try Parser:=TJournalParser.Create(Text,Limits); Result:=Parser.Decode;
    except
      on E:EOutOfMemory do raise;
      on E:EConvertError do raise;
      on E:Exception do TextError(E.Message);
    end;
  finally Parser.Free; end;
end;
end.
