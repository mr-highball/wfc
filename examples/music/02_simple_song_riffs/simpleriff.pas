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

{$mode delphi}{$H+}

interface

uses SysUtils, main, wfc;

type
  TSimpleRiffSong = (srsMary, srsBridge, srsHotCross);
  TSimpleRiffSongs = set of TSimpleRiffSong;

  { Original manually inferred neighboring-note rules. Selection unions the
    authored alternatives; it does not claim to have learned whole songs. }
  TSimpleRiff = class(TSimpleMusic)
  strict private
    FSongs: TSimpleRiffSongs;
  protected
    procedure InitWFC(const AGraph: TGraph); override;
    procedure DoInitializeWFCForSong(const ASong: TSimpleRiffSong;
      const AGraph: TGraph); virtual;
  public
    constructor Create;
    property Songs: TSimpleRiffSongs read FSongs write FSongs;
  end;

implementation

constructor TSimpleRiff.Create;
begin
  inherited Create;
  FSongs := [srsMary, srsBridge, srsHotCross];
end;

procedure TSimpleRiff.InitWFC(const AGraph: TGraph);
var LSong: TSimpleRiffSong;
begin
  if FSongs = [] then raise EArgumentException.Create('select at least one song grammar');
  AGraph.Reshape(NoteCount, 1, 1);
  AGraph.WrapNeighbors := False;
  for LSong := Low(TSimpleRiffSong) to High(TSimpleRiffSong) do
    if LSong in FSongs then DoInitializeWFCForSong(LSong, AGraph);
end;

procedure TSimpleRiff.DoInitializeWFCForSong(const ASong: TSimpleRiffSong;
  const AGraph: TGraph);
begin
  case ASong of
    srsMary:
      begin
        AGraph.AddValue('E')
          .NewRule([gdEast, gdWest], 'D')
          .NewRule([gdEast, gdWest], 'E')
          .NewRule([gdEast, gdWest], 'G');
        AGraph.AddValue('D').NewRule([gdEast, gdWest], 'C');
        AGraph.AddValue('G').NewRule([gdEast, gdWest], 'G');
      end;
    srsBridge:
      begin
        AGraph.AddValue('D')
          .NewRule([gdEast, gdWest], 'E')
          .NewRule([gdEast, gdWest], 'C')
          .NewRule([gdEast, gdWest], 'A')
          .NewRule([gdEast], 'B');
        AGraph.Rules['C'].NewRule([gdEast, gdWest], 'B');
        AGraph.Rules['B'].NewRule([gdWest], 'A');
      end;
    srsHotCross:
      begin
        AGraph.AddValue('E')
          .NewRule([gdEast, gdWest], 'D')
          .NewRule([gdEast, gdWest], 'C');
        AGraph.Rules['D'].NewRule([gdEast, gdWest], 'C');
      end;
  else raise EArgumentException.Create('unknown song grammar');
  end;
end;

end.

