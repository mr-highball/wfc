{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program MappedWorld;
{$mode delphi}{$H+}
uses
  SysUtils, wfc, wfc_lattice, mapped_world_types,
  mapped_world_workbench, wfc_atomic_new_file;
type
  TMappedCliEditKind = (mceDemand, mceDomain, mceLock);
  TMappedCliEdit = record
    Kind: TMappedCliEditKind;
    Layer: TMappedWorldLayer;
    X, Y: Integer;
    Demand: TMappedWorldDemand;
    Values: TGraphValues;
    Value: TGraphValue;
  end;
  TMappedCliEdits = array of TMappedCliEdit;
  TMappedCliArguments = record
    Config: TMappedWorldConfig;
    Options: TMappedWorldSearchOptions;
    HasRepair, Diagnostic: Boolean;
    Action: TMappedWorldAction;
    InspectX, InspectY: Integer;
    SvgPath: String;
    Edits: TMappedCliEdits;
  end;

procedure Usage;
begin
  WriteLn('MappedWorld [OPTIONS]');
  WriteLn('MappedWorld --selftest | --help | --version');
  WriteLn('  --preset interior|sandbox       default: interior');
  WriteLn('  --seed UINT32                   default: 3');
  WriteLn('  --sampling cell|point|region    default: cell');
  WriteLn('  --region X,Y,Z:X,Y,Z            signed half-open world offsets');
  WriteLn('  --land-weight N --water-weight N --clear-weight N --tree-weight N');
  WriteLn('                                 positive portable integers; sandbox only');
  WriteLn('  --demand X,Y=vacant|optional|house');
  WriteLn('  --domain LAYER:X,Y=TOKEN[|TOKEN] (use none for an empty domain)');
  WriteLn('  --lock LAYER:X,Y=TOKEN');
  WriteLn('  --repair housing|foliage|all    explicit authority after baseline + edits');
  WriteLn('  --ordinary | --negotiated       default: negotiated');
  WriteLn('  --backtracks N                  default: 64; zero is valid');
  WriteLn('  --pass-backtracks N             default: 16; zero is valid');
  WriteLn('  --trace                         capture the next solve/repair trace');
  WriteLn('  --inspect X,Y                   read-only house inspection; default: 0,0');
  WriteLn('  --svg NEW-FILE                  independently valid current output only');
  WriteLn('  --diagnostic-svg NEW-FILE       explicitly labeled unsafe/stale study');
  WriteLn('Layers: terrain, foliage, housing. Edits may repeat for distinct cells;');
  WriteLn('conflicting edits to the same cell/category and duplicate options reject.');
  WriteLn('All edits require --repair. The initial baseline is always generated first,');
  WriteLn('without those edits; only then are edits applied and the chosen scope run.');
  WriteLn('No scope fallback or hidden retry. Existing output paths are never replaced.');
  WriteLn('Quote token lists containing | in your shell. --name=value is also accepted.');
end;

function ParseUnsigned(const AText, AName: String; const AMaximum: Cardinal): Cardinal;
var I: Integer; D: Cardinal;
begin
  Result := 0;
  if AText = '' then raise EMappedWorld.Create(AName + ' requires decimal digits');
  for I := 1 to Length(AText) do
  begin
    if not (AText[I] in ['0'..'9']) then
      raise EMappedWorld.Create(AName + ' requires decimal digits');
    D := Cardinal(Ord(AText[I]) - Ord('0'));
    if (D > AMaximum) or (Result > (AMaximum - D) div 10) then
      raise EMappedWorld.Create(AName + ' exceeds its integer range');
    Result := Result * 10 + D;
  end;
end;

function ParseSigned(const AText, AName: String): Integer;
var N: Cardinal;
begin
  if (AText <> '') and (AText[1] = '-') then
  begin
    N := ParseUnsigned(Copy(AText, 2, Length(AText)), AName, Cardinal(High(Integer)) + 1);
    if N = Cardinal(High(Integer)) + 1 then Result := Low(Integer)
    else Result := -Integer(N);
  end
  else Result := Integer(ParseUnsigned(AText, AName, Cardinal(High(Integer))));
end;

procedure SplitOnce(const AText: String; const ASeparator: Char;
  const AName: String; out ALeft, ARight: String);
var P: Integer;
begin
  P := Pos(ASeparator, AText);
  if P = 0 then raise EMappedWorld.Create(AName + ' requires ' + ASeparator);
  ALeft := Copy(AText, 1, P - 1);
  ARight := Copy(AText, P + 1, Length(AText));
  if Pos(ASeparator, ARight) <> 0 then
    raise EMappedWorld.Create(AName + ' has an extra ' + ASeparator);
end;

procedure ParseXY(const AText: String; out AX, AY: Integer);
var L, R: String;
begin
  SplitOnce(AText, ',', 'cell coordinates', L, R);
  AX := Integer(ParseUnsigned(L, 'cell X', Cardinal(High(Integer))));
  AY := Integer(ParseUnsigned(R, 'cell Y', Cardinal(High(Integer))));
end;

function ParseOffset(const AText: String): TGraphOffset;
var P: Integer; First, Rest, Second, Third: String;
begin
  P := Pos(',', AText);
  if P = 0 then raise EMappedWorld.Create('region offset requires X,Y,Z');
  First := Copy(AText, 1, P - 1);
  Rest := Copy(AText, P + 1, Length(AText));
  SplitOnce(Rest, ',', 'region offset', Second, Third);
  Result := MakeGraphOffset(ParseSigned(First, 'region X'),
    ParseSigned(Second, 'region Y'), ParseSigned(Third, 'region Z'));
end;

function ParseLayer(const AText: String): TMappedWorldLayer;
begin
  if AText = 'terrain' then Result := mwlTerrain
  else if AText = 'foliage' then Result := mwlFoliage
  else if AText = 'housing' then Result := mwlHousing
  else raise EMappedWorld.Create('layer must be terrain, foliage, or housing');
end;

procedure ParseCell(const AText: String; out ALayer: TMappedWorldLayer;
  out AX, AY: Integer);
var L, R: String;
begin
  SplitOnce(AText, ':', 'layer/cell', L, R);
  ALayer := ParseLayer(L);
  ParseXY(R, AX, AY);
  MappedWorldCellIndex(ALayer, AX, AY);
end;

function ParseTokens(const AText: String; const ALayer: TMappedWorldLayer): TGraphValues;
var L, R: String; I: Integer;
begin
  Result := nil;
  if AText = 'none' then Exit;
  if Pos('|', AText) = 0 then
  begin
    SetLength(Result, 1);
    Result[0] := AText;
  end
  else
  begin
    SplitOnce(AText, '|', 'domain tokens', L, R);
    SetLength(Result, 2);
    Result[0] := L;
    Result[1] := R;
    if L = R then raise EMappedWorld.Create('duplicate domain token');
  end;
  for I := 0 to High(Result) do
    if not MappedWorldTokenValid(ALayer, Result[I]) then
      raise EMappedWorld.Create('invalid ' + MappedWorldLayerName(ALayer) + ' token: ' + Result[I]);
end;

procedure SeenOnce(var ASeen: TGraphValues; const AKey, ADescription: String);
var I: Integer;
begin
  for I := 0 to High(ASeen) do
    if ASeen[I] = AKey then raise EMappedWorld.Create('duplicate/conflicting ' + ADescription);
  SetLength(ASeen, Length(ASeen) + 1);
  ASeen[High(ASeen)] := AKey;
end;

function OptionValue(var AIndex: Integer; const AOption, AInline: String;
  const AHasInline: Boolean): String;
begin
  if AHasInline then Result := AInline
  else
  begin
    Inc(AIndex);
    if AIndex > ParamCount then raise EMappedWorld.Create('missing value for ' + AOption);
    Result := ParamStr(AIndex);
    if Copy(Result, 1, 2) = '--' then raise EMappedWorld.Create('missing value for ' + AOption);
  end;
  if Result = '' then raise EMappedWorld.Create('empty value for ' + AOption);
end;

function ParseArguments: TMappedCliArguments;
var
  I, P, Index: Integer;
  Option, InlineValue, Value, L, R, Key: String;
  HasInline, SeenRegion, SeenWeights: Boolean;
  Seen: TGraphValues;
  E: TMappedCliEdit;
begin
  Result := Default(TMappedCliArguments);
  Result.Config := DefaultMappedWorldConfig;
  Result.Options := DefaultMappedWorldSearchOptions;
  Seen := nil;
  SeenRegion := False;
  SeenWeights := False;
  I := 1;
  while I <= ParamCount do
  begin
    Option := ParamStr(I);
    P := Pos('=', Option);
    HasInline := P > 0;
    InlineValue := '';
    if HasInline then
    begin
      InlineValue := Copy(Option, P + 1, Length(Option));
      Option := Copy(Option, 1, P - 1);
    end;
    if (Option = '--trace') or (Option = '--ordinary') or (Option = '--negotiated') then
    begin
      if HasInline then raise EMappedWorld.Create(Option + ' does not take a value');
      if Option = '--trace' then
      begin
        SeenOnce(Seen, Option, Option);
        Result.Options.CaptureTrace := True;
      end
      else
      begin
        SeenOnce(Seen, 'strategy', 'solver strategy');
        Result.Options.Negotiated := Option = '--negotiated';
      end;
    end
    else if (Option = '--demand') or (Option = '--domain') or (Option = '--lock') then
    begin
      Value := OptionValue(I, Option, InlineValue, HasInline);
      E := Default(TMappedCliEdit);
      SplitOnce(Value, '=', Option, L, R);
      if Option = '--demand' then
      begin
        E.Kind := mceDemand;
        E.Layer := mwlHousing;
        ParseXY(L, E.X, E.Y);
        if R = 'house' then E.Demand := mwdRequired
        else if R = 'optional' then E.Demand := mwdOptional
        else if R = 'vacant' then E.Demand := mwdVacant
        else raise EMappedWorld.Create('demand must be vacant, optional, or house');
      end
      else
      begin
        ParseCell(L, E.Layer, E.X, E.Y);
        if Option = '--domain' then
        begin
          E.Kind := mceDomain;
          E.Values := ParseTokens(R, E.Layer);
        end
        else
        begin
          E.Kind := mceLock;
          E.Value := R;
          if not MappedWorldTokenValid(E.Layer, R) then
            raise EMappedWorld.Create('lock token is outside its layer vocabulary');
        end;
      end;
      Index := MappedWorldCellIndex(E.Layer, E.X, E.Y);
      case E.Kind of
        mceDemand: Key := 'demand';
        mceDomain: Key := 'domain';
        mceLock: Key := 'lock';
      end;
      Key := Key + ':' + IntToStr(Ord(E.Layer)) + ':' + IntToStr(Index);
      SeenOnce(Seen, Key, 'edit for ' + MappedWorldLayerName(E.Layer) + ':' + IntToStr(E.X) + ',' + IntToStr(E.Y));
      SetLength(Result.Edits, Length(Result.Edits) + 1);
      Result.Edits[High(Result.Edits)] := E;
    end
    else
    begin
      if (Option <> '--preset') and (Option <> '--seed') and (Option <> '--sampling') and
        (Option <> '--region') and (Option <> '--land-weight') and (Option <> '--water-weight') and
        (Option <> '--clear-weight') and (Option <> '--tree-weight') and (Option <> '--repair') and
        (Option <> '--backtracks') and (Option <> '--pass-backtracks') and (Option <> '--inspect') and
        (Option <> '--svg') and (Option <> '--diagnostic-svg') then
        raise EMappedWorld.Create('unknown option: ' + Option);
      if (Option = '--svg') or (Option = '--diagnostic-svg') then Key := 'output' else Key := Option;
      SeenOnce(Seen, Key, Option);
      Value := OptionValue(I, Option, InlineValue, HasInline);
      if Option = '--preset' then
      begin
        if Value = 'interior' then Result.Config.Preset := mwpInteriorStudy
        else if Value = 'sandbox' then Result.Config.Preset := mwpLandscapeSandbox
        else raise EMappedWorld.Create('preset must be interior or sandbox');
      end
      else if Option = '--seed' then Result.Config.Seed := ParseUnsigned(Value, 'seed', High(Cardinal))
      else if Option = '--sampling' then
      begin
        if Value = 'cell' then Result.Config.Sampling := mwsCell
        else if Value = 'point' then Result.Config.Sampling := mwsPointStudy
        else if Value = 'region' then Result.Config.Sampling := mwsRegion
        else raise EMappedWorld.Create('sampling must be cell, point, or region');
      end
      else if Option = '--region' then
      begin
        SeenRegion := True;
        SplitOnce(Value, ':', 'region', L, R);
        Result.Config.RegionMinimum := ParseOffset(L);
        Result.Config.RegionMaximum := ParseOffset(R);
      end
      else if (Option = '--land-weight') or (Option = '--water-weight') or
        (Option = '--clear-weight') or (Option = '--tree-weight') then
      begin
        SeenWeights := True;
        Index := Integer(ParseUnsigned(Value, Option, Cardinal(High(Integer))));
        if Index = 0 then raise EMappedWorld.Create(Option + ' must be positive');
        if Option = '--land-weight' then Result.Config.LandWeight := Index
        else if Option = '--water-weight' then Result.Config.WaterWeight := Index
        else if Option = '--clear-weight' then Result.Config.ClearWeight := Index
        else Result.Config.TreeWeight := Index;
      end
      else if Option = '--repair' then
      begin
        Result.HasRepair := True;
        if Value = 'housing' then Result.Action := mwaHousingOnly
        else if Value = 'foliage' then Result.Action := mwaFoliageAndHousing
        else if Value = 'all' then Result.Action := mwaAllPasses
        else raise EMappedWorld.Create('repair must be housing, foliage, or all');
      end
      else if Option = '--backtracks' then Result.Options.MaxBacktracks := Integer(ParseUnsigned(Value, Option, Cardinal(High(Integer))))
      else if Option = '--pass-backtracks' then Result.Options.MaxPassBacktracks := Integer(ParseUnsigned(Value, Option, Cardinal(High(Integer))))
      else if Option = '--inspect' then
      begin
        ParseXY(Value, Result.InspectX, Result.InspectY);
        MappedWorldCellIndex(mwlHousing, Result.InspectX, Result.InspectY);
      end
      else
      begin
        if Pos(#0, Value) <> 0 then raise EMappedWorld.Create('SVG path cannot contain NUL');
        Result.SvgPath := Value;
        Result.Diagnostic := Option = '--diagnostic-svg';
      end;
    end;
    Inc(I);
  end;
  if (Length(Result.Edits) > 0) and not Result.HasRepair then
    raise EMappedWorld.Create('edits require an explicit --repair housing|foliage|all');
  if SeenRegion and (Result.Config.Sampling <> mwsRegion) then
    raise EMappedWorld.Create('--region requires --sampling region');
  if SeenWeights and (Result.Config.Preset <> mwpLandscapeSandbox) then
    raise EMappedWorld.Create('weight options require --preset sandbox');
  ValidateMappedWorldConfig(Result.Config);
  ValidateMappedWorldSearchOptions(Result.Options);
end;

procedure ApplyEdits(const ASession: TMappedWorldSession; const AEdits: TMappedCliEdits);
var I, J: Integer; E: TMappedCliEdit; Detail: String;
begin
  for I := 0 to High(AEdits) do
  begin
    E := AEdits[I];
    case E.Kind of
      mceDemand: ASession.SetDemand(E.X, E.Y, E.Demand);
      mceDomain: ASession.SetDomain(E.Layer, E.X, E.Y, E.Values);
      mceLock: ASession.SetLock(E.Layer, E.X, E.Y, E.Value);
    end;
    case E.Kind of
      mceDemand: Detail := 'demand=' + MappedWorldDemandName(E.Demand);
      mceDomain:
        begin
          Detail := 'domain=';
          if Length(E.Values) = 0 then Detail := Detail + 'none';
          for J := 0 to High(E.Values) do
          begin
            if J > 0 then Detail := Detail + '|';
            Detail := Detail + E.Values[J];
          end;
        end;
      mceLock: Detail := 'caller-lock=' + E.Value;
    end;
    WriteLn('applied-edit=', Detail, ' layer=', MappedWorldLayerName(E.Layer),
      ' cell=', E.X, ',', E.Y);
  end;
end;

procedure WriteSvg(const APath, AText: String);
var Bytes: TBytes; I: Integer; Output: TWfcAtomicNewFile;
begin
  { The shared renderer emits canonical ASCII SVG; ASCII is also exact UTF-8. }
  Bytes := nil;
  SetLength(Bytes, Length(AText));
  for I := 1 to Length(AText) do
  begin
    if Ord(AText[I]) > 127 then raise EMappedWorld.Create('SVG output is not canonical ASCII');
    Bytes[I - 1] := Byte(Ord(AText[I]));
  end;
  Output := TWfcAtomicNewFile.Create(APath);
  try
    Output.WriteBytes(Bytes);
    Output.Publish;
  finally
    Output.Free;
  end;
end;

procedure PrintInspection(const AValue: TMappedWorldInspection);
var I: Integer; S: TMappedWorldSample;
  procedure Sample(const AKind: String; const A: TMappedWorldSample);
  begin
    WriteLn(AKind, ' layer=', MappedWorldLayerName(A.Layer), ' cell=', A.Position.X, ',',
      A.Position.Y, ',', A.Position.Z, ' value=', A.Cell.Value, ' generated=', A.Cell.Generated,
      ' locked=', A.Cell.Locked, ' zoned=', A.Cell.HasDomain, ' accepted=', A.Accepted,
      ' corner=', A.IsCorner, ' world=[(', A.Bounds.Minimum.X, ',', A.Bounds.Minimum.Y,
      ',', A.Bounds.Minimum.Z, '),(', A.Bounds.Maximum.X, ',', A.Bounds.Maximum.Y,
      ',', A.Bounds.Maximum.Z, '))');
  end;
begin
  WriteLn('inspection-site=', AValue.SiteX, ',', AValue.SiteY,
    ' revision=', AValue.Revision, ' current=', AValue.IsCurrent);
  WriteLn('inspection-banner=', AValue.Banner);
  WriteLn('house-world=[(', AValue.HouseBounds.Minimum.X, ',', AValue.HouseBounds.Minimum.Y, ',',
    AValue.HouseBounds.Minimum.Z, '),(', AValue.HouseBounds.Maximum.X, ',',
    AValue.HouseBounds.Maximum.Y, ',', AValue.HouseBounds.Maximum.Z, '))');
  WriteLn('sampling=', MappedWorldSamplingName(AValue.Sampling), ' query-world=[(',
    AValue.QueryBounds.Minimum.X, ',', AValue.QueryBounds.Minimum.Y, ',', AValue.QueryBounds.Minimum.Z,
    '),(', AValue.QueryBounds.Maximum.X, ',', AValue.QueryBounds.Maximum.Y, ',',
    AValue.QueryBounds.Maximum.Z, ')) terrain-in-bounds=', AValue.TerrainQueryInBounds,
    ' foliage-in-bounds=', AValue.FoliageQueryInBounds);
  WriteLn('sampled-terrain=', Length(AValue.TerrainSamples), ' sampled-foliage=',
    Length(AValue.FoliageSamples), ' physical-blockers=', Length(AValue.PhysicalBlockers),
    ' selected-model-clear=', AValue.SelectedModelClear, ' physical-clear=', AValue.PhysicalClear);
  for I := 0 to High(AValue.TerrainSamples) do Sample('terrain-sample', AValue.TerrainSamples[I]);
  for I := 0 to High(AValue.FoliageSamples) do Sample('foliage-sample', AValue.FoliageSamples[I]);
  for I := 0 to High(AValue.PhysicalBlockers) do
  begin
    S := AValue.PhysicalBlockers[I];
    Sample('physical-blocker', S);
  end;
end;

procedure PrintDefinition(const A: TMappedCliArguments);
var Layer: TMappedWorldLayer; Layout: TWfcLatticeLayout;
begin
  WriteLn('MappedWorld model=', MAPPED_WORLD_MODEL_VERSION, ' mapping=', WFC_PASS_MAPPING_VERSION,
    ' seed=', A.Config.Seed, ' preset=', MappedWorldPresetName(A.Config.Preset),
    ' sampling=', MappedWorldSamplingName(A.Config.Sampling));
  WriteLn('options negotiated=', A.Options.Negotiated, ' local-backtrack-limit-per-pass=',
    A.Options.MaxBacktracks, ' pass-backtrack-limit=', A.Options.MaxPassBacktracks,
    ' trace=', A.Options.CaptureTrace);
  for Layer := Low(TMappedWorldLayer) to High(TMappedWorldLayer) do
  begin
    Layout := MappedWorldLayout(Layer);
    WriteLn('layer=', MappedWorldLayerName(Layer), ' cells=', Layout.Cells.X, 'x', Layout.Cells.Y,
      'x', Layout.Cells.Z, ' origin=', Layout.Origin.X, ',', Layout.Origin.Y, ',', Layout.Origin.Z,
      ' pitch=', Layout.Pitch.X, ',', Layout.Pitch.Y, ',', Layout.Pitch.Z, ' wrap=', Layout.Wrap);
  end;
  if A.Config.Preset = mwpInteriorStudy then
  begin
    WriteLn('Tutorial caller zoning: 48 terrain cells land; 767 foliage cells clear;');
    WriteLn('one free foliage site (7,7); all housing vacant; no caller locks before edits.');
    WriteLn('The blocker is generated by WFC; seed 3 is the documented reproducible study.');
  end
  else
    WriteLn('Sandbox initial terrain/foliage domains unrestricted; all housing optional; weights land=',
      A.Config.LandWeight, ' water=', A.Config.WaterWeight, ' clear=', A.Config.ClearWeight,
      ' tree=', A.Config.TreeWeight);
  WriteLn('Budgets count search work, not elapsed time. Negotiation excludes whole provider');
  WriteLn('assignments; it does not infer a minimal blocker or guarantee sandbox repair.');
end;

procedure Main;
var
  A: TMappedCliArguments;
  Session: TMappedWorldSession;
  Snapshot: TMappedWorldResult;
  Inspection: TMappedWorldInspection;
  Text: String;
  Solved, CanExport: Boolean;
begin
  if (ParamCount = 1) and ((ParamStr(1) = '--help') or (ParamStr(1) = '-h')) then
  begin Usage; Exit; end;
  if (ParamCount = 1) and (ParamStr(1) = '--version') then
  begin WriteLn('MappedWorld ', MAPPED_WORLD_MODEL_VERSION, ' mapping=', WFC_PASS_MAPPING_VERSION); Exit; end;
  if (ParamCount = 1) and (ParamStr(1) = '--selftest') then
  begin WriteLn('Mapped world self-test passed: ', MappedWorldSelfTest); Exit; end;
  A := ParseArguments;
  PrintDefinition(A);
  Session := TMappedWorldSession.Create(A.Config);
  try
    WriteLn('phase=baseline; requested edits are not yet applied');
    Solved := Session.Run(mwaGenerate, A.Options);
    WriteLn(MappedWorldReportText(Session.CopyReport));
    if Solved and A.HasRepair then
    begin
      if Session.CopyCurrent(Snapshot) then WriteLn('baseline-signature=', IntToHex(Snapshot.Signature, 8));
      WriteLn('phase=apply-edits count=', Length(A.Edits));
      ApplyEdits(Session, A.Edits);
      WriteLn('phase=explicit-repair; no fallback scope');
      Solved := Session.Run(A.Action, A.Options);
      WriteLn(MappedWorldReportText(Session.CopyReport));
    end;
    if not Solved then
    begin
      if Session.Status = mwstScopeMismatch then ExitCode := 3 else ExitCode := 2;
    end
    else if Session.CopyCurrent(Snapshot) then
    begin
      WriteLn('current-revision=', Snapshot.Revision, ' signature=', IntToHex(Snapshot.Signature, 8),
        ' selected-model-valid=', Snapshot.ModelValid, ' physical-safe=', Snapshot.PhysicalSafe);
      if not Snapshot.PhysicalSafe then
      begin
        WriteLn('UNSAFE STUDY: selected model accepted a house whose full footprint is blocked.');
        ExitCode := 3;
      end;
    end;
    if Session.CopyInspection(A.InspectX, A.InspectY, Inspection) then PrintInspection(Inspection);
    if A.SvgPath <> '' then
    begin
      if A.Diagnostic then CanExport := Session.TryDiagnosticSvg(A.InspectX, A.InspectY, Text)
      else CanExport := Session.TryCurrentSvg(A.InspectX, A.InspectY, Text);
      if not CanExport then
      begin
        if A.Diagnostic then WriteLn(StdErr, 'SVG refused: no eligible diagnostic artifact.')
        else WriteLn(StdErr, 'SVG refused: no independently valid safe current artifact.');
        if ExitCode = 0 then ExitCode := 3;
      end
      else
      begin
        WriteSvg(A.SvgPath, Text);
        if A.Diagnostic then WriteLn('Wrote explicitly labeled diagnostic SVG: ', ExpandFileName(A.SvgPath))
        else WriteLn('Wrote independently validated current SVG: ', ExpandFileName(A.SvgPath));
      end;
    end;
  finally
    Session.Free;
  end;
end;

begin
  try
    Main;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'MappedWorld: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
