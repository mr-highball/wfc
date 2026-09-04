(*
MIT License

Copyright (c) 2021 mr-highball

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)
unit text_pass_composition_demo;

{$mode delphi}{$H+}

interface

uses
  wfc;

function ParseTextPassCompositionSeed: TGraphSeed;
procedure RunTextPassCompositionDemo(const ASeed: TGraphSeed);

implementation

uses
  SysUtils,
  wfc_model,
  wfc_sequence_graph,
  wfc_text_passes,
  text_pass_composition_showcase;

function ParseTextPassCompositionSeed: TGraphSeed;
var
  I: Integer;
  LDigit: TGraphSeed;
  LParsed: TGraphSeed;
  LText: String;
begin
  if ParamCount = 0 then
    Exit(WFC_TEXT_PASS_SHOWCASE_DEFAULT_SEED);
  if ParamCount <> 1 then
    raise EConvertError.Create(
      'usage: TextPassComposition [unsigned-32-bit-seed]');
  LText := ParamStr(1);
  if LText = '' then
    raise EConvertError.Create('seed cannot be empty');
  LParsed := 0;
  for I := 1 to Length(LText) do
  begin
    if not (LText[I] in ['0'..'9']) then
      raise EConvertError.Create(
        'seed must be an unsigned 32-bit integer');
    LDigit := TGraphSeed(Ord(LText[I]) - Ord('0'));
    if LParsed > (High(TGraphSeed) - LDigit) div 10 then
      raise EConvertError.Create('seed exceeds 4294967295');
    LParsed := (LParsed * 10) + LDigit;
  end;
  Result := LParsed;
end;

procedure WriteLayer(const ALayer: TWfcTextPassLayer;
  const AGenerated: TWfcGeneratedSequence);
var
  I: Integer;
begin
  Write(WfcTextPassLayerName(ALayer), ':');
  for I := 0 to Length(AGenerated.Tokens) - 1 do
    Write(' [', TextPassShowcaseDisplayToken(ALayer,
      AGenerated.Tokens[I]), ']');
  WriteLn;
end;

procedure RunTextPassCompositionDemo(const ASeed: TGraphSeed);
var
  LLayer: TWfcTextPassLayer;
  LReplay: TWfcTextPassResult;
  LReplayReport: TWfcTextPassReport;
  LReport: TWfcTextPassReport;
  LResult: TWfcTextPassResult;
  LShowcase: TTextPassCompositionShowcase;
  LSignature: String;
begin
  LShowcase := TTextPassCompositionShowcase.Create(ASeed);
  try
    if not LShowcase.TryGenerate(LResult, LReport) then
      raise ETextPassCompositionShowcase.CreateFmt(
        'pipeline failed (status=%d pass=%d contradiction=%d)',
        [Ord(LReport.Status), LReport.Solve.FailedPassIndex,
         Ord(LReport.Solve.Contradiction.Kind)]);
    LSignature := TextPassShowcaseSignature(LResult);

    if not LShowcase.TryGenerate(LReplay, LReplayReport) then
      raise ETextPassCompositionShowcase.Create(
        'same-seed replay failed');
    if (LReplay.Text <> LResult.Text) or
        (TextPassShowcaseSignature(LReplay) <> LSignature) or
        (LReplayReport.TraceHash <> LReport.TraceHash) then
      raise ETextPassCompositionShowcase.Create(
        'same-seed replay changed the public result');

    WriteLn('TextPassComposition: structure -> lexical -> punctuation');
    WriteLn('Seed: ', ASeed);
    WriteLn('Text: ', String(LResult.Text));
    WriteLn('Signature: ', LSignature);
    WriteLn('Passes: ', Length(LReport.Solve.Passes),
      '  trace events: ', Length(LReport.Trace),
      '  trace hash: ', LReport.TraceHash);
    for LLayer := Low(TWfcTextPassLayer) to High(TWfcTextPassLayer) do
      WriteLayer(LLayer, TextPassShowcaseSequence(LResult, LLayer));
    WriteLn('Independent validation: ',
      LReport.Validation.CheckedLayers, ' paths, ',
      LReport.Validation.CheckedRelations, ' cross-pass relations');
    WriteLn('Dependencies: repository units + standard RTL only');
    WriteLn('Versions: text-passes=', WFC_TEXT_PASS_PIPELINE_VERSION,
      ' fragments=', WFC_TEXT_PASS_FRAGMENT_VERSION,
      ' showcase=', WFC_TEXT_PASS_SHOWCASE_VERSION);
  finally
    LShowcase.Free;
  end;
end;

end.
