{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Complete immutable recipe/run contexts for workspace journals.
  Graph-free construction validates canonical documents, not executed history. }
unit wfc_pipeline_workspace_context;
{$mode delphi}{$H+}
interface
uses SysUtils,wfc_pipeline_model,wfc_pipeline_run;
type
  EWfcPipelineWorkspaceContext = class(Exception);
  TWfcPipelineWorkspaceRecipeTexts = array of String;
  TWfcPipelineWorkspaceRunText = record
    RecipeIndex: Integer;
    Text: String;
  end;
  TWfcPipelineWorkspaceRunTexts = array of TWfcPipelineWorkspaceRunText;
  TWfcPipelineWorkspaceContextLimits = record
    Version: Integer;
    MaxRecipes,MaxRuns,MaxTextBytes: Integer;
  end;
  TWfcPipelineWorkspaceContexts = class
  strict private
    FRecipeTexts: TWfcPipelineWorkspaceRecipeTexts;
    FRunTexts: TWfcPipelineWorkspaceRunTexts;
    FRecipes: array of TWfcPipelineModel;
    FRuns: array of TWfcPipelineRun;
    FTextBytes: Integer;
    procedure CheckRecipeIndex(const Index: Integer);
    procedure CheckRunIndex(const Index: Integer);
    function GetRecipeCount: Integer;
    function GetRunCount: Integer;
  public
    constructor Create(const RecipeTexts: TWfcPipelineWorkspaceRecipeTexts;
      const RunTexts: TWfcPipelineWorkspaceRunTexts;
      const Limits: TWfcPipelineWorkspaceContextLimits);
    destructor Destroy; override;
    function RecipeTextAt(const Index: Integer): String;
    function RunTextAt(const Index: Integer): TWfcPipelineWorkspaceRunText;
    { Immutable borrowed typed contexts: keep this owner alive and do not Free
      a borrowed object. Indices, not signatures, establish the context edge. }
    function BorrowRecipe(const Index: Integer): TWfcPipelineModel;
    function BorrowRun(const Index: Integer): TWfcPipelineRun;
    function CopyRecipe(const Index: Integer): TWfcPipelineModel;
    function CopyRun(const Index: Integer): TWfcPipelineRun;
    property RecipeCount: Integer read GetRecipeCount;
    property RunCount: Integer read GetRunCount;
    property TextBytes: Integer read FTextBytes;
  end;
implementation
uses wfc_pipeline_text,wfc_pipeline_run_text;
procedure ContextError(const Detail: String);
begin raise EWfcPipelineWorkspaceContext.Create('workspace context: '+Detail); end;
procedure GuardInputs(const Recipes: TWfcPipelineWorkspaceRecipeTexts;
  const Runs: TWfcPipelineWorkspaceRunTexts; const Limits: TWfcPipelineWorkspaceContextLimits);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    function passive(o,k) {
      if (o===null || typeof o!=='object' || Array.isArray(o)) return undefined;
      let d;
      while (o!==null && !(d=Object.getOwnPropertyDescriptor(o,k))) o=Object.getPrototypeOf(o);
      return d && ('value' in d) ? d : undefined;
    }
    function positive(d) { return d && Number.isInteger(d.value) && d.value>0 && d.value<=2147483647; }
    const v=passive(Limits,'Version'), rc=passive(Limits,'MaxRecipes'),
      uc=passive(Limits,'MaxRuns'), bc=passive(Limits,'MaxTextBytes');
    Valid=!!(positive(v) && v.value===1 && positive(rc) && positive(uc) && positive(bc));
    if (Valid) Valid=Array.isArray(Recipes) && Array.isArray(Runs) &&
      Recipes.length>0 && Recipes.length<=rc.value && Runs.length>0 && Runs.length<=uc.value;
    if (Valid) {
      for (let i=0;i<Recipes.length;i++) {
        const d=Object.getOwnPropertyDescriptor(Recipes,String(i));
        if (!d || !('value' in d) || typeof d.value!=='string') { Valid=false; break; }
      }
    }
    if (Valid) {
      for (let i=0;i<Runs.length;i++) {
        const d=Object.getOwnPropertyDescriptor(Runs,String(i));
        if (!d || !('value' in d)) { Valid=false; break; }
        const ri=passive(d.value,'RecipeIndex'), tx=passive(d.value,'Text');
        if (!ri || !Number.isInteger(ri.value) || ri.value<0 || ri.value>=Recipes.length ||
          !tx || typeof tx.value!=='string') { Valid=false; break; }
      }
    }
  end;
  if not Valid then ContextError('complete passive contexts and positive version1 limits are required');
  {$ENDIF}
  if (Limits.Version<>1) or (Limits.MaxRecipes<1) or (Limits.MaxRuns<1) or
    (Limits.MaxTextBytes<1) then ContextError('unsupported or nonpositive context limits');
  if (Length(Recipes)=0) or (Length(Recipes)>Limits.MaxRecipes) or
    (Length(Runs)=0) or (Length(Runs)>Limits.MaxRuns) then ContextError('context count budget exceeded or empty table');
end;
procedure ChargeText(const Text: String; const Maximum: Integer; var Used: Integer);
var I: Integer;
begin
  if Length(Text)>Maximum-Used then ContextError('aggregate canonical context text budget exceeded');
  if (Length(Text)=0) or (Text[Length(Text)]<>#10) then ContextError('complete canonical context text with final LF required');
  for I:=1 to Length(Text) do
    if (Ord(Text[I])>127) or (Text[I]=#13) or (Text[I]=#0) then ContextError('context document must use canonical ASCII and LF');
  Inc(Used,Length(Text));
end;
constructor TWfcPipelineWorkspaceContexts.Create(const RecipeTexts: TWfcPipelineWorkspaceRecipeTexts;
  const RunTexts: TWfcPipelineWorkspaceRunTexts; const Limits: TWfcPipelineWorkspaceContextLimits);
var I: Integer;
begin
  inherited Create; GuardInputs(RecipeTexts,RunTexts,Limits);
  { All external text/count charges precede any decoder or owned vector clone.
    Typed model expansion remains subject to the existing artifact envelopes. }
  for I:=0 to High(RecipeTexts) do ChargeText(RecipeTexts[I],Limits.MaxTextBytes,FTextBytes);
  for I:=0 to High(RunTexts) do
  begin
    if (RunTexts[I].RecipeIndex<0) or (RunTexts[I].RecipeIndex>=Length(RecipeTexts)) then ContextError('run recipe index out of range');
    ChargeText(RunTexts[I].Text,Limits.MaxTextBytes,FTextBytes);
  end;
  SetLength(FRecipeTexts,Length(RecipeTexts)); SetLength(FRecipes,Length(RecipeTexts));
  SetLength(FRunTexts,Length(RunTexts)); SetLength(FRuns,Length(RunTexts));
  for I:=0 to High(RecipeTexts) do
  begin
    FRecipeTexts[I]:=RecipeTexts[I]; FRecipes[I]:=DecodeWfcPipelineModelText(FRecipeTexts[I]);
    if EncodeWfcPipelineModelText(FRecipes[I])<>FRecipeTexts[I] then ContextError('recipe text is not the exact canonical representation');
  end;
  for I:=0 to High(RunTexts) do
  begin
    FRunTexts[I].RecipeIndex:=RunTexts[I].RecipeIndex; FRunTexts[I].Text:=RunTexts[I].Text;
    FRuns[I]:=DecodeWfcPipelineRunText(FRunTexts[I].Text,FRecipes[FRunTexts[I].RecipeIndex]);
    if EncodeWfcPipelineRunText(FRuns[I])<>FRunTexts[I].Text then ContextError('run text is not the exact canonical representation');
  end;
end;
destructor TWfcPipelineWorkspaceContexts.Destroy;
var I: Integer;
begin
  for I:=0 to High(FRuns) do FRuns[I].Free;
  for I:=0 to High(FRecipes) do FRecipes[I].Free;
  inherited Destroy;
end;
procedure TWfcPipelineWorkspaceContexts.CheckRecipeIndex(const Index: Integer);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}asm Valid=Number.isInteger(Index); end;
  if not Valid then ContextError('recipe context index must be an Integer');{$ENDIF}
  if (Index<0) or (Index>=Length(FRecipes)) then ContextError('recipe context index out of range');
end;
procedure TWfcPipelineWorkspaceContexts.CheckRunIndex(const Index: Integer);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}asm Valid=Number.isInteger(Index); end;
  if not Valid then ContextError('run context index must be an Integer');{$ENDIF}
  if (Index<0) or (Index>=Length(FRuns)) then ContextError('run context index out of range');
end;
function TWfcPipelineWorkspaceContexts.GetRecipeCount: Integer;
begin Result:=Length(FRecipes); end;
function TWfcPipelineWorkspaceContexts.GetRunCount: Integer;
begin Result:=Length(FRuns); end;
function TWfcPipelineWorkspaceContexts.RecipeTextAt(const Index: Integer): String;
begin CheckRecipeIndex(Index); Result:=FRecipeTexts[Index]; end;
function TWfcPipelineWorkspaceContexts.RunTextAt(const Index: Integer): TWfcPipelineWorkspaceRunText;
begin CheckRunIndex(Index); Result.RecipeIndex:=FRunTexts[Index].RecipeIndex; Result.Text:=FRunTexts[Index].Text; end;
function TWfcPipelineWorkspaceContexts.BorrowRecipe(const Index: Integer): TWfcPipelineModel;
begin CheckRecipeIndex(Index); Result:=FRecipes[Index]; end;
function TWfcPipelineWorkspaceContexts.BorrowRun(const Index: Integer): TWfcPipelineRun;
begin CheckRunIndex(Index); Result:=FRuns[Index]; end;
function TWfcPipelineWorkspaceContexts.CopyRecipe(const Index: Integer): TWfcPipelineModel;
begin CheckRecipeIndex(Index); Result:=DecodeWfcPipelineModelText(FRecipeTexts[Index]); end;
function TWfcPipelineWorkspaceContexts.CopyRun(const Index: Integer): TWfcPipelineRun;
begin CheckRunIndex(Index); Result:=DecodeWfcPipelineRunText(FRunTexts[Index].Text,FRecipes[FRunTexts[Index].RecipeIndex]); end;
end.
