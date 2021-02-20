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
  protected
    procedure InitWFC(const AGraph: TGraph); override;
    procedure DoInitializeSongList(const AItems : TStrings); virtual;
  public
  end;

var
  SimpleRiffForm: TSimpleRiff;

implementation

{$R *.lfm}

{ TSimpleRiff }

procedure TSimpleRiff.InitWFC(const AGraph: TGraph);
begin
  inherited;
end;

procedure TSimpleRiff.DoInitializeSongList(const AItems: TStrings);
begin
  AItems.Add('Mary Had a Little Lamb');
  AItems.Add('Test Song I');
  AItems.Add('Test Song II');
end;

end.

