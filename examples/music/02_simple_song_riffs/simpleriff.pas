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
unit simpleriff;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs, ExtCtrls, ComboEx,
  main,
  wfc;

type

  { TSimpleRiff }
  (*
    an example showing how we can use simple songs to manually define
    our graph's constraints and "riff" on those rules
  *)
  TSimpleRiff = class(TSimpleMusicForm)
    combo_songs: TCheckComboBox;
    pnl_song_selection: TPanel;
  private
    const
      SONG_MARY = 'Mary Had a Little Lamb';
      SONG_BRIDGE = 'London Bridge is Falling Down';
      SONG_HOT_CROSS = 'Hot Cross Buns';
  private
  protected
    procedure InitWFC(const AGraph: TGraph); override;
    procedure DoInitializeSongList(const AItems : TStrings); virtual;
    procedure DoInitializeWFCForSong(const ASong : String; const AGraph : TGraph); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

var
  SimpleRiffForm: TSimpleRiff;

implementation

{$R *.lfm}

{ TSimpleRiff }

procedure TSimpleRiff.InitWFC(const AGraph: TGraph);
var
  I: Integer;
begin
  AGraph.Reshape({width = notes} edit_note_count.Value, {height = 0-note/1-duration?} 1, {depth} 1); //todo - should be use height to hold the duration? or would this be better for implementing the passes idea? for now use 1 dimension or use Z?
  AGraph.WrapNeighbors := False;
  for I := 0 to Pred(combo_songs.Items.Count) do
    if combo_songs.Checked[I] then
      DoInitializeWFCForSong(combo_songs.Items[I], AGraph);
end;

procedure TSimpleRiff.DoInitializeSongList(const AItems: TStrings);
begin
  AItems.Add(SONG_MARY);
  AItems.Add(SONG_BRIDGE);
  AItems.Add(SONG_HOT_CROSS);
end;

procedure TSimpleRiff.DoInitializeWFCForSong(const ASong: String;
  const AGraph: TGraph);

  (*
    all possible notes and their neighbors for Mary Had a Little Lamb
  *)
  procedure InitMary;
  begin
    AGraph
      .AddValue('E')
        .NewRule([gdEast, gdWest], 'D')
        .NewRule([gdEast, gdWest], 'E')
        .NewRule([gdEast, gdWest], 'G');
     AGraph.AddValue('D').NewRule([gdEast, gdWest], 'C');
     AGraph.AddValue('G').NewRule([gdEast, gdWest], 'G');
  end;

  procedure InitBridge;
  begin

  end;

  procedure InitHotCross;
  begin

  end;

begin
  if ASong = SONG_MARY then
    InitMary
  else if ASong = SONG_BRIDGE then
    InitBridge
  else if ASong = SONG_HOT_CROSS then
    InitHotCross
  else
    ShowMessage('unrecognized song [' + ASong + ']');
end;

constructor TSimpleRiff.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  combo_songs.Clear;
  DoInitializeSongList(combo_songs.Items);
end;

end.

