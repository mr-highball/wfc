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
program wfc_browser_demo_entries_test;

{$mode delphi}{$H+}
{$IFNDEF PAS2JS}{$FATAL this entry-point test requires the browser target}{$ENDIF}

uses SysUtils, JS, Web, wfc_browser_test_host;

const
  DEMO_COUNT = 10;
  DEFAULT_PAGE_TIMEOUT_MS = 15000;
  ENSEMBLE_PAGE_TIMEOUT_MS = 30000;
  POLL_INTERVAL_MS = 25;

type
  TExpectation = record Name, Value: String; end;
  TExpectations = array of TExpectation;
  TDemo = record
    Name, Bundle, Stylesheet: String;
    TimeoutMs: Integer;
    Expectations: TExpectations;
  end;
  TDemoEntries = class
  private
    FDemos: array[0..DEMO_COUNT - 1] of TDemo;
    FIndex, FPassed, FFailed: Integer;
    FFrame: TJSHTMLIFrameElement;
    FExpectedUrl, FFailures, FLastMismatch: String;
    FStartedAt: Double;
    FPolls: Integer;
    procedure Expect(const AIndex: Integer; const AName, AValue: String);
    procedure Prepare;
    procedure OpenNext;
    procedure Poll;
    procedure CompletePage(const ASuccess: Boolean; const AMessage: String);
    procedure Finish;
    procedure RemoveFrame;
    function ResourceState(out AMessage: String): Integer;
    function MarkerState(const ABody: TJSElement; out AMessage: String): Integer;
  public
    constructor Create;
    procedure Start;
  end;

function ErrorText(const AError: JSValue): String;
begin
  if isObject(AError) and isString(TJSObject(AError)['message']) then
    Result := String(TJSObject(AError)['message'])
  else if isObject(AError) and isString(TJSObject(AError)['fMessage']) then
    Result := String(TJSObject(AError)['fMessage'])
  else Result := String(AError);
end;

procedure TDemoEntries.Expect(const AIndex: Integer; const AName, AValue: String);
var N: Integer;
begin
  N := Length(FDemos[AIndex].Expectations);
  SetLength(FDemos[AIndex].Expectations, N + 1);
  FDemos[AIndex].Expectations[N].Name := AName;
  FDemos[AIndex].Expectations[N].Value := AValue;
end;

procedure TDemoEntries.Prepare;
var I: Integer;
begin
  FDemos[0].Name := 'world2d';
  FDemos[0].Bundle := 'BrowserWorld.js'; FDemos[0].Stylesheet := 'browserworld.css';
  FDemos[1].Name := 'text-passes';
  FDemos[1].Bundle := 'BrowserTextPassComposition.js'; FDemos[1].Stylesheet := 'browsertextpasses.css';
  FDemos[2].Name := 'building3d';
  FDemos[2].Bundle := 'BrowserBuilding.js'; FDemos[2].Stylesheet := 'browserbuilding.css';
  FDemos[3].Name := 'training';
  FDemos[3].Bundle := 'BrowserTrainingStudio.js'; FDemos[3].Stylesheet := 'trainingstudio.css';
  FDemos[4].Name := 'music';
  FDemos[4].Bundle := 'BrowserMusicStudio.js'; FDemos[4].Stylesheet := 'musicstudio.css';
  FDemos[5].Name := 'counts';
  FDemos[5].Bundle := 'BrowserNeighborhoodCounts.js'; FDemos[5].Stylesheet := 'counts.css';
  FDemos[6].Name := 'ensemble';
  FDemos[6].Bundle := 'BrowserEnsembleStudio.js'; FDemos[6].Stylesheet := 'ensemblestudio.css';
  FDemos[7].Name := 'voices';
  FDemos[7].Bundle := 'BrowserVoiceStudio.js'; FDemos[7].Stylesheet := 'voicestudio.css';
  FDemos[8].Name := 'connectivity';
  FDemos[8].Bundle := 'BrowserConnectedRoutes.js'; FDemos[8].Stylesheet := 'connectedroutes.css';
  FDemos[9].Name := 'terraces';
  FDemos[9].Bundle := 'BrowserTerraces.js'; FDemos[9].Stylesheet := 'terraces.css';

  for I := 0 to DEMO_COUNT - 1 do
  begin
    FDemos[I].TimeoutMs := DEFAULT_PAGE_TIMEOUT_MS;
    Expect(I, 'data-self-test', 'passed');
    if I <> 7 then Expect(I, 'data-state', 'solved');
  end;
  { This one entry runs finite/developed generation, WAVE, and all awaited
    MIDI plan/save/cancel/failure/release fixtures together. Measured complete
    runs exceed fifteen seconds even while every fixture keeps progressing.
    Keep all expectations and the host's independent sixty-second deadline;
    this is a test-work allowance, not a generation performance threshold. }
  FDemos[6].TimeoutMs := ENSEMBLE_PAGE_TIMEOUT_MS;
  { Preserve the six standalone hosted marker maps, not just their generic
    success flags. These requests load actual staged HTML/CSS/compiled entry
    scripts; no synthetic controller fixture or extra rtl.run call is injected. }
  Expect(0, 'data-signature', '1:5B0DD75D:08022AF1:A40D0955');
  Expect(1, 'data-signature', '1:69ABA6CE');
  Expect(1, 'data-output-signature', '1:69ABA6CE');
  Expect(1, 'data-pass-count', '3');
  Expect(1, 'data-trace-hash', '2412171679');
  Expect(1, 'data-output', 'A sun rises brightly!');
  Expect(2, 'data-signature', '1:F1EF0EB6');
  Expect(2, 'data-view-signature', 'AC7290C0');
  Expect(2, 'data-face-count', '140');
  Expect(3, 'data-source-signature', '0FA2C5EA');
  Expect(3, 'data-recipe-signature', 'DBCBA621');
  Expect(3, 'data-result-signature', '947C4AFD');
  Expect(3, 'data-cell-count', '16');
  Expect(3, 'data-quota-edit', 'passed');
  Expect(3, 'data-quota-replay', 'passed');
  Expect(3, 'data-quota-contradiction', 'passed');
  Expect(3, 'data-quota-invalidation', 'passed');
  Expect(3, 'data-quota-volume', 'passed');
  Expect(3, 'data-connectivity-edit', 'passed');
  Expect(3, 'data-connectivity-replay', 'passed');
  Expect(3, 'data-connectivity-contradiction', 'passed');
  Expect(3, 'data-connectivity-invalidation', 'passed');
  Expect(3, 'data-connectivity-volume', 'passed');
  Expect(3, 'data-source-invalidation', 'passed');
  Expect(3, 'data-run-invalidation', 'passed');
  Expect(3, 'data-contradiction', 'passed');
  Expect(3, 'data-recovery', 'passed');
  Expect(3, 'data-preset-sweep', 'passed');
  Expect(3, 'data-volume-dimensions', 'passed');
  Expect(3, 'data-volume-lock', 'passed');
  Expect(3, 'data-volume-contradiction', 'passed');
  Expect(3, 'data-volume-recovery', 'passed');
  Expect(3, 'data-import-race', 'passed');
  Expect(3, 'data-circular-sequence', 'passed');
  Expect(3, 'data-overlapping-volume', 'passed');
  Expect(3, 'data-overlapping-volume-view', 'passed');
  Expect(3, 'data-overlapping-volume-recovery', 'passed');
  Expect(3, 'data-volume-view-isolation', 'passed');
  Expect(3, 'data-volume-layout', 'passed');
  Expect(4, 'data-seed', '0');
  Expect(4, 'data-composition-signature', '216F6EBB');
  Expect(4, 'data-result-status', 'solved');
  Expect(4, 'data-score-signature', '4167E7E5');
  Expect(4, 'data-midi-signature', '86E4DCA3');
  Expect(4, 'data-wave-signature', '64679FF8');
  Expect(4, 'data-strategy', 'negotiated');
  Expect(4, 'data-scope', 'full');
  Expect(4, 'data-cell-count', '16');
  Expect(4, 'data-lock-count', '0');
  Expect(4, 'data-pass-count', '3');
  Expect(4, 'data-midi-bytes', '123');
  Expect(4, 'data-wav-bytes', '352844');
  Expect(4, 'data-arrangement-test', 'passed');
  Expect(4, 'data-arrangement-4-frames', '176400');
  Expect(4, 'data-arrangement-6-frames', '264600');
  Expect(4, 'data-arrangement-180-frames', '7938000');
  Expect(4, 'data-arrangement-cancel', 'passed');
  Expect(4, 'data-arrangement-stale', 'passed');
  Expect(4, 'data-arrangement-write-failure', 'passed');
  Expect(4, 'data-arrangement-commit', 'passed');
  Expect(4, 'data-new-session-invalidation', 'passed');
  Expect(4, 'data-run-invalidation', 'passed');
  Expect(4, 'data-failure-clears-output', 'passed');
  Expect(4, 'data-motif-lock', 'passed');
  Expect(4, 'data-selective-regeneration', 'passed');
  Expect(4, 'data-recovery', 'passed');
  Expect(4, 'data-audio-ready', 'true');
  Expect(4, 'data-audio-play-events', '0');
  Expect(5, 'data-output-key', 'LLLLLLLLL/.R.R...../....M....');
  Expect(5, 'data-lower', 'passed');
  Expect(5, 'data-upper', 'passed');
  Expect(5, 'data-alias', 'passed');
  Expect(5, 'data-repair', 'passed');
  Expect(5, 'data-flood', 'passed');
  Expect(5, 'data-invalidation', 'passed');

  Expect(6, 'data-stream-self-test', 'passed');
  Expect(6, 'data-stream-release', 'passed');
  Expect(6, 'data-midi-stream-self-test', 'passed');
  Expect(6, 'data-midi-stream-release', 'passed');
  Expect(6, 'data-http-stream-self-test', 'passed');
  Expect(6, 'data-composition-signature', '573E2010');
  Expect(6, 'data-score-signature', '33123E67');
  Expect(6, 'data-midi-signature', '07361333');
  Expect(6, 'data-wave-signature', 'A273067B');
  Expect(6, 'data-midi-bytes', '227');
  Expect(6, 'data-wave-bytes', '352844');
  Expect(6, 'data-new-session-invalidation', 'passed');
  Expect(6, 'data-run-invalidation', 'passed');
  Expect(6, 'data-failure-clears-output', 'passed');
  Expect(6, 'data-selective-regeneration', 'passed');
  Expect(6, 'data-long-score-preserved', 'passed');
  Expect(6, 'data-download-metadata', 'passed');
  Expect(6, 'data-recovery', 'passed');
  Expect(6, 'data-audio-ready', 'true');
  Expect(6, 'data-audio-play-events', '0');
  Expect(7, 'data-voice-state', 'ready');
  Expect(7, 'data-voice-stream-self-test', 'passed');
  Expect(7, 'data-voice-stream-release', 'passed');
  Expect(8, 'data-case', 'circulation');
  Expect(8, 'data-signature', '9F2CC7A4');
  Expect(8, 'data-town-repair', 'passed');
  Expect(8, 'data-town-rollback', 'passed');
  Expect(8, 'data-circulation', 'passed');
  Expect(8, 'data-circulation-repair', 'passed');
  Expect(8, 'data-invalidation', 'passed');
  Expect(8, 'data-portable-artifacts', 'passed');
  Expect(9, 'data-signature', '1:6D695B99:2D23CF62');
  Expect(9, 'data-view-signature', 'C3D25917');
  Expect(9, 'data-selective', 'passed');
  Expect(9, 'data-invalidation', 'passed');
  Expect(9, 'data-recovery', 'passed');
  Expect(9, 'data-new-session', 'passed');
end;

constructor TDemoEntries.Create;
begin
  inherited Create;
  Prepare;
  document.body.setAttribute('data-demo-entries-self-test', 'pending');
  document.body.setAttribute('data-demo-entries-count', '0');
  document.body.setAttribute('data-demo-entries-failures', '');
  document.body.setAttribute('data-self-test', 'pending');
  document.body.removeAttribute('data-self-test-message');
end;

procedure TDemoEntries.Start;
begin
  { The ordinary harness marks a synchronous return successful. Override that
    provisional generic marker on the first task; only our dedicated marker
    can certify completion of all awaited real pages. }
  document.body.setAttribute('data-self-test', 'pending');
  WriteLn('Real browser demo entries: ten sequential staged index pages');
  OpenNext;
end;

procedure TDemoEntries.RemoveFrame;
begin
  if FFrame = nil then Exit;
  FFrame.src := 'about:blank';
  if FFrame.parentNode <> nil then FFrame.parentNode.removeChild(FFrame);
  FFrame := nil;
end;

procedure TDemoEntries.OpenNext;
begin
  if FIndex = DEMO_COUNT then begin Finish; Exit; end;
  try
    FLastMismatch := 'page has not loaded'; FPolls := 0;
    FExpectedUrl := TJSURL.new('demo-entries/' + FDemos[FIndex].Name +
      '/index.html?selftest=1', window.location.href).href;
    document.body.setAttribute('data-demo-entries-current', FDemos[FIndex].Name);
    document.body.setAttribute('data-demo-entries-page-timeout-ms',
      IntToStr(FDemos[FIndex].TimeoutMs));
    document.body.setAttribute('data-demo-entry-' + FDemos[FIndex].Name, 'pending');
    FFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
    FFrame.width := '1280'; FFrame.height := '900';
    FFrame.setAttribute('title', 'Real demo entry: ' + FDemos[FIndex].Name);
    { Keep a real layout viewport; display:none can change canvas/UI behavior. }
    FFrame.setAttribute('style', 'position:absolute;left:-12000px;width:1280px;height:900px;border:0');
    FFrame.src := FExpectedUrl;
    FStartedAt := window.performance.now;
    document.body.appendChild(FFrame);
    window.setTimeout(@Poll, POLL_INTERVAL_MS);
  except
    CompletePage(False, ErrorText(JSExceptValue));
  end;
end;

function TDemoEntries.ResourceState(out AMessage: String): Integer;
var Entries: TJSArray; I: Integer; Entry: TJSObject;
  Name, BundleUrl, StyleUrl: String; Status: JSValue;
  HasBundle, HasStyle, HasLoadedStyle: Boolean;
  Sheets: TJSStyleSheetList; Sheet: TJSStyleSheet;
begin
  Result := 0; AMessage := ''; HasBundle := False; HasStyle := False;
  HasLoadedStyle := False;
  BundleUrl := TJSURL.new(FDemos[FIndex].Bundle, FExpectedUrl).href;
  StyleUrl := TJSURL.new(FDemos[FIndex].Stylesheet, FExpectedUrl).href;
  Entries := TJSArray(FFrame.contentWindow.performance.getEntriesByType('resource'));
  for I := 0 to Entries.length - 1 do
  begin
    Entry := TJSObject(Entries[I]);
    if not isString(Entry['name']) then Continue;
    Name := String(Entry['name']);
    if (Pos('blob:', Name) = 1) or (Pos('data:', Name) = 1) then Continue;
    if TJSURL.new(Name, FExpectedUrl).origin <> window.location.origin then Continue;
    Status := Entry['responseStatus'];
    if isNumber(Status) and (Double(Status) >= 400) then
    begin
      AMessage := 'static resource failed: ' + Name + ' (HTTP ' + String(Status) + ')';
      Exit(-1);
    end;
    if Name = BundleUrl then HasBundle := True;
    if Name = StyleUrl then HasStyle := True;
  end;
  { A resource timing entry proves a request, not a successful load. Some
    hosts do not expose responseStatus, so a missing CSS file can otherwise
    pass with all application markers intact. Require the named same-origin
    stylesheet to have a real, nonempty parsed rule list as well. }
  Sheets := FFrame.contentDocument.styleSheets;
  for I := 0 to Sheets.length - 1 do
  begin
    Sheet := Sheets.item(I);
    if Sheet.href <> StyleUrl then Continue;
    if TJSCSSStyleSheet(Sheet).cssRules.length > 0 then
      HasLoadedStyle := True;
  end;
  if not HasBundle then AMessage := 'main compiled entry request missing: ' + FDemos[FIndex].Bundle
  else if not HasStyle then AMessage := 'main stylesheet request missing: ' + FDemos[FIndex].Stylesheet
  else if not HasLoadedStyle then
  begin
    AMessage := 'main stylesheet has no loaded rules: ' + FDemos[FIndex].Stylesheet;
    if FFrame.contentDocument.readyState = 'complete' then Result := -1;
  end
  else Result := 1;
end;

function TDemoEntries.MarkerState(const ABody: TJSElement; out AMessage: String): Integer;
const ErrorNames: array[0..4] of String = ('data-self-test-message',
  'data-arrangement-test-message', 'data-stream-test-message',
  'data-midi-stream-test-message', 'data-voice-stream-test-message');
var I: Integer; Actual: String;
begin
  Result := 1; AMessage := '';
  for I := 0 to High(ErrorNames) do
  begin
    if not ABody.hasAttribute(ErrorNames[I]) then Continue;
    Actual := ABody.getAttribute(ErrorNames[I]);
    if Actual <> '' then begin AMessage := ErrorNames[I] + ': ' + Actual; Exit(-1); end;
  end;
  for I := 0 to High(FDemos[FIndex].Expectations) do
  begin
    if ABody.hasAttribute(FDemos[FIndex].Expectations[I].Name) then
      Actual := ABody.getAttribute(FDemos[FIndex].Expectations[I].Name)
    else Actual := '<missing>';
    if (FDemos[FIndex].Expectations[I].Value = 'passed') and (Actual = 'failed') then
    begin
      AMessage := FDemos[FIndex].Expectations[I].Name + '=failed'; Exit(-1);
    end;
    if Actual <> FDemos[FIndex].Expectations[I].Value then
    begin
      if AMessage = '' then AMessage := FDemos[FIndex].Expectations[I].Name +
        ': expected ' + FDemos[FIndex].Expectations[I].Value + ', got ' + Actual;
      Result := 0;
    end;
  end;
end;

procedure TDemoEntries.Poll;
var Child: TJSDocument; State: Integer; ResourceMessage, MarkerMessage: String;
begin
  try
    Inc(FPolls);
    document.body.setAttribute('data-self-test', 'pending');
    if (FFrame <> nil) and (FFrame.contentWindow <> nil) and
      (FFrame.contentWindow.location.href = FExpectedUrl) then
    begin
      Child := FFrame.contentDocument;
      if (Child <> nil) and (Child.body <> nil) then
      begin
        State := ResourceState(ResourceMessage);
        if State < 0 then begin CompletePage(False, ResourceMessage); Exit; end;
        if MarkerState(Child.body, MarkerMessage) < 0 then
        begin CompletePage(False, MarkerMessage); Exit; end;
        if (Child.readyState = 'complete') and (State = 1) and (MarkerMessage = '') then
        begin CompletePage(True, 'rendered marker and static-resource contracts match'); Exit; end;
        if MarkerMessage <> '' then FLastMismatch := MarkerMessage
        else if ResourceMessage <> '' then FLastMismatch := ResourceMessage
        else FLastMismatch := 'document has not completed loading';
      end;
    end;
    if (window.performance.now - FStartedAt >= FDemos[FIndex].TimeoutMs) or
      (FPolls >= FDemos[FIndex].TimeoutMs div POLL_INTERVAL_MS) then
    begin
      CompletePage(False, 'page deadline exceeded (' +
        IntToStr(FDemos[FIndex].TimeoutMs) + ' ms): ' + FLastMismatch);
      Exit;
    end;
    window.setTimeout(@Poll, POLL_INTERVAL_MS);
  except
    CompletePage(False, ErrorText(JSExceptValue));
  end;
end;

procedure TDemoEntries.CompletePage(const ASuccess: Boolean; const AMessage: String);
var Message: String;
begin
  Message := FDemos[FIndex].Name + '/index.html?selftest=1: ' + Copy(AMessage, 1, 1000);
  if ASuccess then
  begin
    Inc(FPassed); WriteLn('[PASS] ', Message);
    document.body.setAttribute('data-demo-entry-' + FDemos[FIndex].Name, 'passed');
  end
  else
  begin
    Inc(FFailed); WriteLn('[FAIL] ', Message);
    if FFailures <> '' then FFailures := FFailures + ' | ';
    FFailures := FFailures + Message;
    document.body.setAttribute('data-demo-entry-' + FDemos[FIndex].Name, 'failed');
    document.body.setAttribute('data-demo-entries-failures', FFailures);
  end;
  document.body.setAttribute('data-demo-entries-count', IntToStr(FPassed + FFailed));
  RemoveFrame;
  Inc(FIndex);
  window.setTimeout(@OpenNext, 0);
end;

procedure TDemoEntries.Finish;
begin
  document.body.setAttribute('data-demo-entries-current', 'complete');
  document.body.setAttribute('data-demo-entries-passed', IntToStr(FPassed));
  WriteLn('Real demo entries: ', FPassed, ' passed, ', FFailed, ' failed');
  if (FPassed = DEMO_COUNT) and (FFailed = 0) then
  begin
    document.body.setAttribute('data-demo-entries-self-test', 'passed');
    document.body.setAttribute('data-self-test', 'passed');
  end
  else
  begin
    document.body.setAttribute('data-demo-entries-self-test', 'failed');
    document.body.setAttribute('data-self-test', 'failed');
    document.body.setAttribute('data-self-test-message', FFailures);
  end;
end;

var Application: TDemoEntries;
begin
  Application := TDemoEntries.Create;
  window.setTimeout(@Application.Start, 0);
end.
