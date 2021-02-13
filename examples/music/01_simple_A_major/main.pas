unit main;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  wfc,
  Audio,
  Piano,
  Music,
  Wave;

type


  { TSimpleMusicForm }
  (*
    an example showing how we can use a commonly used scale (A Major)
    to generate a simple musical track and can also be used as a base class
    for other music demos
  *)
  TSimpleMusicForm = class(TForm)
    btn_stop: TButton;
    btn_play: TButton;
    btn_generate: TButton;
    memo_notes: TMemo;
    pnl_piano: TPanel;
    pnl_ctrls: TPanel;
    timer_audio: TTimer;
    track_piano_size: TTrackBar;
    procedure btn_generateClick(Sender: TObject);
    procedure btn_playClick(Sender: TObject);
    procedure btn_stopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure timer_audioTimer(Sender: TObject);
    procedure track_piano_sizeChange(Sender: TObject);
  strict private
    FPiano: TPianoKeyboard;
    FRecorder: TWaveRecorder;
    FTempo : Double;
    FMouseDown: Boolean;
    FDownKey: Integer;
    FGraph : TGraph;

    {$Region shameless_soundshop_copy_paste}
    procedure InitPiano;
    procedure PianoKeyToggle(Sender: TObject; Key: Integer; Down: Boolean);
    procedure PianoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PianoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PianoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    {$EndRegion}
  protected
    type

      { TPianoGraph }
      (*
        specialized graph to start at the beginning bar
      *)
      TPianoGraph = class(TGraph)
      strict protected
        procedure DoGetStartCoord(out X, Y: UInt64); override;
      public
      end;
  protected
    (*
      responsible for initializing the wfc graph and can
      be overridden in children if a different behavior is desired
    *)
    procedure InitWFC(const AGraph : TGraph); virtual;

    (*
      can be overridden to add notes differently then the default way
    *)
    procedure DoAddMusicNotes(const AMusic : TPianoMusic); virtual;
  public

    (*
      uses wfc to generate music
    *)
    procedure GenerateMusic;

    (*
      after generate has been called, this will play music on the piano
    *)
    procedure PlayMusic;

    (*
      stops the currently playing music
    *)
    procedure StopMusic;
  end;

var
  SimpleMusicForm: TSimpleMusicForm;

implementation
uses
  StrUtils;

{$R *.lfm}

function CountOccurences( const SubText: string;
                          const Text: string): Integer;
begin
  Result := Pos(SubText, Text);
  if Result > 0 then
    Result := (Length(Text) - Length(StringReplace(Text, SubText, '', [rfReplaceAll]))) div  Length(subtext);
end;

{ TSimpleMusicForm.TPianoGraph }

procedure TSimpleMusicForm.TPianoGraph.DoGetStartCoord(out X, Y: UInt64);
begin
  //always start at the beginning note for our piano demo
  X := 0;
  Y := 0;
end;

{ TSimpleMusicForm }

procedure TSimpleMusicForm.FormCreate(Sender: TObject);
begin
  InitPiano;
  FGraph := TPianoGraph.Create;
  InitWFC(FGraph);
end;

procedure TSimpleMusicForm.FormDestroy(Sender: TObject);
begin
  FGraph.Free;
end;

procedure TSimpleMusicForm.timer_audioTimer(Sender: TObject);
const
  TimeOffset = 0.1;
var
  LTime: Double;
begin
  LTime := AudioTime;
  FPiano.Music.Play(LTime + TimeOffset);
  if FPiano.Music.Stopped then
    StopMusic;
end;

procedure TSimpleMusicForm.track_piano_sizeChange(Sender: TObject);
begin
  //there may be a better way to get the piano visual control to scale
  //but to avoid any more digging, here's a simple slider which will use
  //the scale factor to update the size
  FPiano.ScaleFactor := track_piano_size.Position / 100;
end;

procedure TSimpleMusicForm.btn_generateClick(Sender: TObject);
begin
  GenerateMusic;
end;

procedure TSimpleMusicForm.btn_playClick(Sender: TObject);
begin
  PlayMusic;
end;

procedure TSimpleMusicForm.btn_stopClick(Sender: TObject);
begin
  StopMusic;
end;

procedure TSimpleMusicForm.InitPiano;
begin
  //initialize the settings for a piano to show on screen
  AudioInit;
  FTempo := 1;
  FPiano := TPianoKeyboard.Create(Self);
  FPiano.Parent := pnl_piano;
  FPiano.Align := alClient;
  FPiano.SetMargin(Rect(0, 1, 0, 0));
  FPiano.ScaleFactor := 0.35;
  FPiano.OnKeyToggle := PianoKeyToggle;
  FPiano.OnMouseDown := PianoMouseDown;
  FPiano.OnMouseMove := PianoMouseMove;
  FPiano.OnMouseUp := PianoMouseUp;
  FPiano.Music.Overhang := 0.1;
  FPiano.ShowRoll := True;
  FPiano.SetMargin(Classes.Rect(0, 1000, 0, 10));
  FRecorder := TWaveRecorder.Create;
end;

procedure TSimpleMusicForm.PianoKeyToggle(Sender: TObject; Key: Integer; Down: Boolean);
begin
  //here's where we actually play with the piano via user inputs
  if Down then
    AudioVoice(Key, FPiano.KeyToFrequency(Key), FPiano.Music.Velocity, FPiano.Music.Time)
  else
    AudioVoice(Key, 0, 0);
end;

procedure TSimpleMusicForm.PianoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FMouseDown := True;
    FDownKey := FPiano.KeyFromPoint(X, Y);
    FPiano.Key[FDownKey] := True;
  end;
end;

procedure TSimpleMusicForm.PianoMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  I: Integer;
begin
  if FMouseDown then
  begin
    I := FPiano.KeyFromPoint(X, Y);
    if I <> FDownKey then
    begin
      FPiano.Key[FDownKey] := False;
      FDownKey := I;
      FPiano.Key[FDownKey] := True;
    end;
  end;
end;

procedure TSimpleMusicForm.PianoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and FMouseDown then
  begin
    FPiano.Key[FDownKey] := False;
    FDownKey := -1;
    FMouseDown := False;
  end;
end;

procedure TSimpleMusicForm.InitWFC(const AGraph: TGraph);
begin
  (*
    below we'll setup the A Major scale and use 6 bars (4/4) in the width direction
    and use the Y direction to manage the note and the duration of the note
    + will denote a higher octave than the base note.

    Lastly, our constraints will simply allow us to play the scale so each note
    must come after it's prior note on the scale
  *)
  AGraph.Reshape({width = notes} 24, {height = 0-note/1-duration?} 1, {depth} 1); //todo - should be use height to hold the duration? or would this be better for implementing the passes idea? for now use 1 dimension or use Z?
  AGraph.WrapNeighbors := False;
  AGraph.AddValue('A').NewRule([gdEast], 'B').NewRule([gdWest], 'A+');
  AGraph.AddValue('B').NewRule([gdEast], 'C#');
  AGraph.AddValue('C#').NewRule([gdEast], 'D');
  AGraph.AddValue('D').NewRule([gdEast], 'E');
  AGraph.AddValue('E').NewRule([gdEast], 'F#');
  AGraph.AddValue('F#').NewRule([gdEast], 'G#');
  AGraph.AddValue('G#').NewRule([gdEast], 'A+');
  AGraph.AddValue('A+').NewRule([gdEast], 'A');
end;

procedure TSimpleMusicForm.DoAddMusicNotes(const AMusic: TPianoMusic);
var
  LLine: String;
  I, LNote: Integer;
begin
  //check the memo for the notes and handle all possbilities including octaves
  for I := 0 to Pred(memo_notes.Lines.Count) do
  begin
    LLine := memo_notes.Lines[I];
    LLine := ReplaceStr(LLine, '+', '');

    if LLine = 'A' then
      LNote := noteA
    else if LLine = 'A#' then
      LNote := noteAsharp
    else if LLine = 'B' then
      LNote := noteB
    else if LLine = 'C' then
      LNote := noteC
    else if LLine = 'C#' then
      LNote := noteCsharp
    else if LLine = 'D' then
      LNote := noteD
    else if LLine = 'D#' then
      LNote := noteDsharp
    else if LLine = 'E' then
      LNote := noteE
    else if LLine = 'F' then
      LNote := noteF
    else if LLine = 'F#' then
      LNote := noteFsharp
    else if LLine = 'G' then
      LNote := noteGsharp
    else if LLine = 'G#' then
      LNote := noteGsharp;

    LLine := memo_notes.Lines[I];

    //notes are just integers, and 12 notes until a octave, so use + signs for this
    LNote := LNote + (CountOccurences('+', LLine) * 12);

    AMusic.Add(LNote, I, 1); //todo - hard coded duration needs changing
  end;
end;

procedure TSimpleMusicForm.GenerateMusic;
var
  I: Integer;
begin
  memo_notes.Clear;
  FGraph.Reset;
  InitWFC(FGraph);
  FGraph.Run;

  //for now we will just add the note to the memo control
  for I := 0 to Pred(FGraph.Dimension.Width) do
    memo_notes.Lines.Add(FGraph[I, 0, 0].Value);
end;

procedure TSimpleMusicForm.PlayMusic;
begin
  AudioReset;
  FPiano.Music.Clear;
  FPiano.Reset;
  DoAddMusicNotes(FPiano.Music);
  timer_audio.Enabled := True;
end;

procedure TSimpleMusicForm.StopMusic;
begin
  //soundshop uses a timer to play the music, so we mimic this and enable here
  timer_audio.Enabled := False;
  FPiano.Reset;
end;

end.

