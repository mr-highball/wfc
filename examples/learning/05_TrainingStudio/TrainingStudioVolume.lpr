{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program TrainingStudioVolume;

{$mode delphi}{$H+}

uses
  Classes, SysUtils, wfc_model, wfc_pipeline_run, wfc_pipeline_result,
  wfc_training_workspace, wfc_token_volume_view, wfc_voxel3d_isometric,
  wfc_voxel3d_svg, wfc_atomic_new_file, training_studio_presets,
  training_studio_demo;

type
  EVolumeUsage = class(Exception);
  TVolumeCommand = record
    Solve: TWfcTrainingSolveOptions;
    Depth, CutDepth, Yaw, MaxCells, MaxQuads: Integer;
    HiddenToken, OutputPath: String;
  end;

procedure Help;
begin
  WriteLn('TrainingStudioVolume 1 - native public-token volume SVG');
  WriteLn('Usage: TrainingStudioVolume [options]');
  WriteLn('  --seed N             Unsigned 32-bit seed (default 0)');
  WriteLn('  --width N            Output X cells (default 4)');
  WriteLn('  --height N           Output Y cells (default 4)');
  WriteLn('  --depth N            Output Z cells (default 4)');
  WriteLn('  --yaw 0|90|180|270    Isometric orientation (default 0)');
  WriteLn('  --cut-depth N        Show layers Z=0 through N-1 (default all)');
  WriteLn('  --hide-token N|none|air  Hide one public palette index (default air)');
  WriteLn('                         Preset palette: 0=stone, 1=air, 2=leaf');
  WriteLn('  --max-cells N        Explicit output-cell budget (default 512)');
  WriteLn('  --max-quads N        Explicit exposed-face budget (default 131072)');
  WriteLn('  --backtracks N       Local search budget (default 1024)');
  WriteLn('  --output NEWFILE     Publish a new SVG; never replace an existing file');
  WriteLn('  --help | --version   Must be used alone');
  WriteLn;
  WriteLn('Default output is SVG on stdout; diagnostics use stderr. No viewer is launched.');
  WriteLn('Uses the same authored lattice preset and public locks as Training Studio:');
  WriteLn('stone(0,0,0), leaf(2,2,1), air(1,1,2). Locks never move or disappear.');
  WriteLn('Positive dimensions must contain these anchors; other extents can contradict.');
  WriteLn('The recipe wraps in XYZ; rendering is a finite cutout, not a periodic mesh.');
  WriteLn('Cut/hide/yaw settings affect only the SVG, never generation or validation.');
  WriteLn('Limits: cells <= ', WFC_PIPELINE_RUN_MAX_CELL_COUNT,
    ', backtracks <= ', WFC_PIPELINE_RUN_MAX_BACKTRACKS, '.');
  WriteLn('Quad budgets <= ', High(Integer), '; actual RAM/pixel integer limits may be lower.');
  WriteLn('No truncation or automatic budget increase. Search counts do not bound wall time.');
  WriteLn('File output needs an existing trusted parent directory; Unix needs hard links.');
  WriteLn('Exit codes: 0 success, 1 generation/render/I/O failure, 2 invalid arguments.');
end;

function UnsignedNumber(const S, Name: String; const Minimum, Maximum: QWord): QWord;
var I, Digit: Integer;
begin
  if (S = '') or ((Length(S) > 1) and (S[1] = '0')) then
    raise EVolumeUsage.Create(Name + ' requires canonical decimal digits');
  Result := 0;
  for I := 1 to Length(S) do
  begin
    if not (S[I] in ['0'..'9']) then
      raise EVolumeUsage.Create(Name + ' requires canonical decimal digits');
    Digit := Ord(S[I]) - Ord('0');
    if (QWord(Digit) > Maximum) or (Result > (Maximum - QWord(Digit)) div 10) then
      raise EVolumeUsage.Create(Name + ' exceeds its numeric limit');
    Result := Result * 10 + QWord(Digit);
  end;
  if Result < Minimum then raise EVolumeUsage.Create(Name + ' is below its minimum');
end;

function Parse: TVolumeCommand;
var I, Slot: Integer; Name, Value: String; Seen: array[0..10] of Boolean;
begin
  Result := Default(TVolumeCommand);
  Result.Solve := TrainingStudioPresetOptions(TRAINING_STUDIO_PATTERN3D_PRESET);
  Result.Depth := TrainingStudioPresetDepth(TRAINING_STUDIO_PATTERN3D_PRESET);
  Result.CutDepth := 0;
  Result.HiddenToken := 'air';
  Result.MaxCells := InteractiveWfcTrainingWorkspaceLimits.MaxOutputCells;
  Result.MaxQuads := WFC_TOKEN_VOLUME_VIEW_DEFAULT_MAX_QUADS;
  FillChar(Seen, SizeOf(Seen), 0);
  I := 1;
  while I <= ParamCount do
  begin
    Name := ParamStr(I);
    if Name = '--seed' then Slot := 0
    else if Name = '--width' then Slot := 1
    else if Name = '--height' then Slot := 2
    else if Name = '--depth' then Slot := 3
    else if Name = '--yaw' then Slot := 4
    else if Name = '--cut-depth' then Slot := 5
    else if Name = '--hide-token' then Slot := 6
    else if Name = '--max-cells' then Slot := 7
    else if Name = '--max-quads' then Slot := 8
    else if Name = '--backtracks' then Slot := 9
    else if Name = '--output' then Slot := 10
    else raise EVolumeUsage.Create('unknown option or positional argument: ' + Name);
    if Seen[Slot] then raise EVolumeUsage.Create('repeated option: ' + Name);
    Seen[Slot] := True;
    Inc(I);
    if I > ParamCount then raise EVolumeUsage.Create('missing value for ' + Name);
    Value := ParamStr(I);
    case Slot of
      0: Result.Solve.Seed := Cardinal(UnsignedNumber(Value, Name, 0, QWord(4294967295)));
      1: Result.Solve.Width := Integer(UnsignedNumber(Value, Name, 1, WFC_PIPELINE_RUN_MAX_DIMENSION));
      2: Result.Solve.Height := Integer(UnsignedNumber(Value, Name, 1, WFC_PIPELINE_RUN_MAX_DIMENSION));
      3: Result.Depth := Integer(UnsignedNumber(Value, Name, 1, WFC_PIPELINE_RUN_MAX_DIMENSION));
      4:
        begin
          Result.Yaw := Integer(UnsignedNumber(Value, Name, 0, 270));
          if not (Result.Yaw in [0, 90, 180]) and (Result.Yaw <> 270) then
            raise EVolumeUsage.Create('yaw must be 0, 90, 180 or 270');
        end;
      5: Result.CutDepth := Integer(UnsignedNumber(Value, Name, 1, WFC_PIPELINE_RUN_MAX_DIMENSION));
      6:
        begin
          if (Value <> 'none') and (Value <> 'air') then
            UnsignedNumber(Value, Name, 0, High(Integer));
          Result.HiddenToken := Value;
        end;
      7: Result.MaxCells := Integer(UnsignedNumber(Value, Name, 1, WFC_PIPELINE_RUN_MAX_CELL_COUNT));
      8: Result.MaxQuads := Integer(UnsignedNumber(Value, Name, 0, High(Integer)));
      9: Result.Solve.MaxBacktracks := Integer(UnsignedNumber(Value, Name, 0, WFC_PIPELINE_RUN_MAX_BACKTRACKS));
      10:
        begin
          if (Value = '') or (Value = '-') or (Pos(#0, Value) > 0) then
            raise EVolumeUsage.Create('output must name a new file; omit --output for stdout');
          Result.OutputPath := Value;
        end;
    end;
    Inc(I);
  end;
  if Result.CutDepth = 0 then Result.CutDepth := Result.Depth;
  if Result.CutDepth > Result.Depth then raise EVolumeUsage.Create('cut depth exceeds output depth');
  if (Result.Solve.Width > Result.MaxCells div Result.Solve.Height) or
    (Result.Solve.Width * Result.Solve.Height > Result.MaxCells div Result.Depth) then
    raise EVolumeUsage.Create('output XYZ product exceeds the selected cell budget');
  if (Result.Solve.Width < 3) or (Result.Solve.Height < 3) or (Result.Depth < 3) then
    raise EVolumeUsage.Create('output must contain the preset public locks: each dimension must be at least 3');
  if (Result.OutputPath <> '') and
    (FileExists(Result.OutputPath) or DirectoryExists(Result.OutputPath)) then
    raise EVolumeUsage.Create('refusing to overwrite existing output');
end;

procedure Publish(const Path: String; const Text: RawByteString);
var Destination: TWfcAtomicNewFile; Block: array[0..65535] of Byte;
  Offset, Count: Integer;
begin
  Destination := TWfcAtomicNewFile.Create(Path);
  try
    try
      Offset := 1;
      while Offset <= Length(Text) do
      begin
        Count := Length(Text) - Offset + 1;
        if Count > SizeOf(Block) then Count := SizeOf(Block);
        Move(Text[Offset], Block[0], Count);
        Destination.WriteBytes(Slice(Block, Count));
        Inc(Offset, Count);
      end;
      Destination.Publish;
      if Destination.CleanupError <> '' then
        raise EWriteError.Create('SVG was published, but temporary cleanup failed: ' + Destination.CleanupError);
    except
      on E: Exception do
      begin
        Destination.Cancel;
        if Destination.CleanupError <> '' then E.Message := E.Message + '; cleanup: ' + Destination.CleanupError;
        raise;
      end;
    end;
  finally Destination.Free; end;
end;

procedure Execute(const Command: TVolumeCommand);
var Workspace: TWfcTrainingWorkspace; Limits: TWfcTrainingWorkspaceLimits;
  Palette, Tokens: TWfcModelTokens; View: TWfcTokenVolumeViewOptions;
  Scene: TVoxel3DProjectedScene; SvgOptions: TVoxel3DSvgOptions;
  Svg: RawByteString; Stream: THandleStream; I: Integer;
begin
  Limits := DefaultWfcTrainingWorkspaceLimits;
  Limits.MaxOutputCells := Command.MaxCells;
  Workspace := TWfcTrainingWorkspace.Create(Limits);
  try
    Workspace.SetSourceText(TrainingStudioPresetText(TRAINING_STUDIO_PATTERN3D_PRESET));
    Workspace.Train;
    Palette := Workspace.PublicVocabulary;
    View := DefaultWfcTokenVolumeViewOptions(Command.Depth);
    View.VisibleDepth := Command.CutDepth;
    View.MaxQuads := Command.MaxQuads;
    case Command.Yaw of
      0: View.Projection.Yaw := v3vy0; 90: View.Projection.Yaw := v3vy90;
      180: View.Projection.Yaw := v3vy180; 270: View.Projection.Yaw := v3vy270;
    end;
    if Command.HiddenToken = 'air' then
    begin
      for I := 0 to High(Palette) do if Palette[I] = 'air' then View.HiddenTokenIndex := I;
    end
    else if Command.HiddenToken <> 'none' then
      View.HiddenTokenIndex := Integer(UnsignedNumber(Command.HiddenToken, 'hidden palette index', 0, High(Palette)));
    Workspace.ConfigureVolumeRun(Command.Solve, Command.Depth,
      TrainingStudioPresetLocks(TRAINING_STUDIO_PATTERN3D_PRESET, Workspace.PublicPassIndex), nil);
    Workspace.Solve;
    if Workspace.ResultStatus <> wprsSolved then
      raise Exception.Create('the requested volume did not solve within its constraints/search budget; no SVG emitted');
    Tokens := Workspace.OutputTokens;
    if not TrainingStudioLatticeOutputIsValid(Command.Solve.Width, Command.Solve.Height, Command.Depth, Tokens) then
      raise Exception.Create('independent public lattice validation failed; no SVG emitted');
    Scene := ProjectWfcTokenVolume3D(Tokens, Command.Solve.Width,
      Command.Solve.Height, Command.Depth, Palette, View);
    try
      SvgOptions := DefaultVoxel3DSvgOptions;
      SvgOptions.Title := 'Training Studio lattice; seed=' + UIntToStr(Command.Solve.Seed) +
        '; shape=' + IntToStr(Command.Solve.Width) + 'x' + IntToStr(Command.Solve.Height) + 'x' + IntToStr(Command.Depth) +
        '; result=' + Workspace.ResultSignatureText + '; yaw=' + IntToStr(Command.Yaw) +
        '; visible-depth=' + IntToStr(Command.CutDepth) + '; hidden=' + Command.HiddenToken;
      Svg := UTF8Encode(UnicodeString(EncodeVoxel3DProjectedSceneSvg(Scene, SvgOptions)));
    finally Scene.Free; end;
  finally Workspace.Free; end;
  if Command.OutputPath <> '' then Publish(Command.OutputPath, Svg)
  else
  begin
    Stream := THandleStream.Create(TTextRec(Output).Handle);
    try if Svg <> '' then Stream.WriteBuffer(Svg[1], Length(Svg));
    finally Stream.Free; end;
  end;
end;

begin
  try
    if (ParamCount = 1) and (ParamStr(1) = '--help') then Help
    else if (ParamCount = 1) and (ParamStr(1) = '--version') then WriteLn('TrainingStudioVolume 1')
    else Execute(Parse);
  except
    on E: EVolumeUsage do begin WriteLn(StdErr, 'TrainingStudioVolume: ', E.Message); Halt(2); end;
    on E: Exception do begin WriteLn(StdErr, 'TrainingStudioVolume: ', E.Message); Halt(1); end;
  end;
end.
