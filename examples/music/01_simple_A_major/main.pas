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

  (*
    an example showing how we can use a commonly used scale (A Major)
    to generate a simple musical track
  *)

  { TMainForm }

  TMainForm = class(TForm)
    btn_play: TButton;
    btn_generate: TButton;
    memo_notes: TMemo;
    pnl_piano: TPanel;
    pnl_ctrls: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    FPiano: TPianoKeyboard;
    FRecorder: TWaveRecorder;
    FTempo : Double;
    procedure InitPiano;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  InitPiano;
end;

procedure TMainForm.InitPiano;
begin
  AudioInit;
  FTempo := 1;
  FPiano := TPianoKeyboard.Create(Self);
  FPiano.Parent := Self;
  FPiano.Align := alClient;
  FPiano.SetMargin(Rect(0, 1, 0, 0));
  FPiano.ScaleFactor := 0.75;
  //FPiano.OnKeyToggle := PianoKeyToggle;
  //FPiano.OnMouseDown := PianoMouseDown;
  //FPiano.OnMouseMove := PianoMouseMove;
  //FPiano.OnMouseUp := PianoMouseUp;
  FPiano.Music.Overhang := 0.1;
  FPiano.ShowRoll := True;
  FPiano.SetMargin(Classes.Rect(0, 1000, 0, 10));
  FRecorder := TWaveRecorder.Create;
end;

end.

