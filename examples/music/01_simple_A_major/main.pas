unit main;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs, ExtCtrls, StdCtrls,
  wfc,
  Audio,
  Piano,
  Music,
  Wave;

type


  { TMainForm }
  (*
    an example showing how we can use a commonly used scale (A Major)
    to generate a simple musical track
  *)
  TMainForm = class(TForm)
    btn_play: TButton;
    btn_generate: TButton;
    memo_notes: TMemo;
    pnl_piano: TPanel;
    pnl_ctrls: TPanel;
    procedure btn_generateClick(Sender: TObject);
    procedure btn_playClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
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
    procedure InitWFC(const AGraph : TGraph); virtual;
  public
    procedure GenerateMusic;
    procedure PlayMusic;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  InitPiano;
  FGraph := TGraph.Create;
  InitWFC(FGraph);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FGraph.Free;
end;

procedure TMainForm.btn_generateClick(Sender: TObject);
begin
  GenerateMusic;
end;

procedure TMainForm.btn_playClick(Sender: TObject);
begin
  PlayMusic;
end;

procedure TMainForm.InitPiano;
begin
  AudioInit;
  FTempo := 1;
  FPiano := TPianoKeyboard.Create(Self);
  FPiano.Parent := pnl_piano;
  FPiano.Align := alClient;
  FPiano.SetMargin(Rect(0, 1, 0, 0));
  FPiano.ScaleFactor := 0.5;
  FPiano.OnKeyToggle := PianoKeyToggle;
  FPiano.OnMouseDown := PianoMouseDown;
  FPiano.OnMouseMove := PianoMouseMove;
  FPiano.OnMouseUp := PianoMouseUp;
  FPiano.Music.Overhang := 0.1;
  FPiano.ShowRoll := True;
  FPiano.SetMargin(Classes.Rect(0, 1000, 0, 10));
  FRecorder := TWaveRecorder.Create;
end;

procedure TMainForm.PianoKeyToggle(Sender: TObject; Key: Integer; Down: Boolean);
begin
  //here's where we actually play with the piano
  if Down then
    AudioVoice(Key, FPiano.KeyToFrequency(Key), FPiano.Music.Velocity, FPiano.Music.Time)
  else
    AudioVoice(Key, 0, 0);
end;

procedure TMainForm.PianoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FMouseDown := True;
    FDownKey := FPiano.KeyFromPoint(X, Y);
    FPiano.Key[FDownKey] := True;
  end;
end;

procedure TMainForm.PianoMouseMove(Sender: TObject; Shift: TShiftState; X,
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

procedure TMainForm.PianoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and FMouseDown then
  begin
    FPiano.Key[FDownKey] := False;
    FDownKey := -1;
    FMouseDown := False;
  end;
end;

procedure TMainForm.InitWFC(const AGraph: TGraph);
begin
  (*
    below we'll setup the A Major scale and use 6 bars (4/4) in the width direction
    and use the Y direction to manage the note and the duration of the note
    + will denote a higher octave than the base note.

    Lastly, our constraints will simply allow us to play the scale so each note
    must come after it's prior note on the scale
  *)
  AGraph.Reshape({width = notes} 24, {height = 0-note/1-duration?} 1, {depth} 1); //todo - should be use height to hold the duration? or would this be better for implementing the passes idea? for now use 1 dimension or use Z?
  AGraph.AddValue('A').NewRule([gdEast], 'B').NewRule([gdWest], 'A+');
  AGraph.AddValue('B').NewRule([gdEast], 'C#');
  AGraph.AddValue('C#').NewRule([gdEast], 'D');
  AGraph.AddValue('D').NewRule([gdEast], 'E');
  AGraph.AddValue('E').NewRule([gdEast], 'F#');
  AGraph.AddValue('F#').NewRule([gdEast], 'G#');
  AGraph.AddValue('G#').NewRule([gdEast], 'A+');
  AGraph.AddValue('A+').NewRule([gdEast], 'A');
end;

procedure TMainForm.GenerateMusic;
var
  I: Integer;
begin
  //todo - generate dank tunes
  memo_notes.Clear;
  FGraph.Reset;
  InitWFC(FGraph);
  FGraph.Run;

  for I := 0 to Pred(FGraph.Dimension.Width) do
    memo_notes.Lines.Add(FGraph[I, 0, 0].Value);
end;

procedure TMainForm.PlayMusic;
begin
  //todo - play our dank tunes
end;

end.

