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
unit wfc.buildkit;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  wfc;

const
  KIT_GR_000 = 'gr_000';

  KIT_BW_000 = 'bw_000';
  KIT_BW_090 = 'bw_090';
  KIT_BW_180 = 'bw_180';
  KIT_BW_270 = 'bw_270';

  KIT_BR_000 = 'br_000';
  KIT_BR_090 = 'br_090';
  KIT_BR_180 = 'br_180';
  KIT_BR_270 = 'br_270';

  KIT_MW_000 = 'mw_000';
  KIT_MW_090 = 'mw_090';
  KIT_MW_180 = 'mw_180';
  KIT_MW_270 = 'mw_270';

  KIT_MR_000 = 'mr_000';
  KIT_MR_090 = 'mr_090';
  KIT_MR_180 = 'mr_180';
  KIT_MR_270 = 'mr_270';

  KIT_TW_000 = 'tw_000';
  KIT_TW_090 = 'tw_090';
  KIT_TW_180 = 'tw_180';
  KIT_TW_270 = 'tw_270';

  KIT_TR_000 = 'tr_000';
  KIT_TR_090 = 'tr_090';
  KIT_TR_180 = 'tr_180';
  KIT_TR_270 = 'tr_270';

  KIT_RW_000 = 'rw_000';
  KIT_RW_090 = 'rw_090';
  KIT_RW_180 = 'rw_180';
  KIT_RW_270 = 'rw_270';

  KIT_RR_000 = 'rr_000';
  KIT_RR_090 = 'rr_090';
  KIT_RR_180 = 'rr_180';
  KIT_RR_270 = 'rr_270';

  KIT_RF_000 = 'rf_000';

  KIT_EM_000 = 'em_000'; //empty space


(*
  sets up the constraints and allows for wfc to generate a 3D world using
  a set of simple 'building blocks'

  details:
    * https://github.com/mr-highball/wfc/issues/3
*)
function NewBuildKitGraph(const AWidth, AHeight, ADepth : UInt64) : TGraph;

implementation

function NewBuildKitGraph(const AWidth, AHeight, ADepth: UInt64): TGraph;
begin
  //initialize a graph
  Result := TGraph.Create;
  Result.WrapNeighbors := False;
  Result.Mode := rmBottomUp;

  //shape to the caller's specifications
  Result.Reshape(AWidth, AHeight, ADepth);

  //----------------------------------------------------------------------------
  //ground constraints
  //note:
  //  can be next to other ground pieces and we'll let the base rules define
  //  the 'other' ground constraints
  //----------------------------------------------------------------------------
  Result.AddValue(KIT_GR_000)
    .NewRule([gdNorth, gdEast, gdSouth, gdWest], [KIT_GR_000]);

  //----------------------------------------------------------------------------
  //empty constraints
  //----------------------------------------------------------------------------
  Result.AddValue(KIT_EM_000)
    .NewRule([gdSouth, gdWest], [KIT_EM_000]) //'inner' empty squares
    .NewRule([gdSouth], [KIT_BW_000, KIT_MW_000, KIT_TW_000]) //can be bordered by 'walls' to the south
    .NewRule([gdEast], [KIT_BW_090, KIT_MW_090, KIT_TW_090]) //can be bordered by 'walls' to the east
    .NewRule([gdNorth], [KIT_BW_180, KIT_MW_180, KIT_TW_180]) //can be bordered by 'walls' to the north
    .NewRule([gdWest], [KIT_BW_270, KIT_MW_270, KIT_TW_270]); //can be bordered by 'walls' to the west

  //----------------------------------------------------------------------------
  //base wall constraints
  //----------------------------------------------------------------------------
  Result.AddValue(KIT_BW_000)
    .NewRule([gdSouth], [KIT_GR_000]) //north of grass
    .NewRule([gdEast], [KIT_BW_000, KIT_BR_000]) //itself or a matching corner
    .NewRule([gdWest], [KIT_BR_270]); //'opposite' corner

  Result.AddValue(KIT_BW_090)
    .NewRule([gdEast], [KIT_GR_000]) //west of grass
    .NewRule([gdNorth], [KIT_BW_090, KIT_BR_090]); //itself or a matching corner

  Result.AddValue(KIT_BW_180)
    .NewRule([gdNorth], [KIT_GR_000]) //south of grass
    .NewRule([gdEast], [KIT_BW_180, KIT_BR_180]) //itself or a matching corner
    .NewRule([gdWest], [KIT_BR_270]); //'opposite' corner

  Result.AddValue(KIT_BW_270)
    .NewRule([gdWest], [KIT_GR_000]) //east of grass
    .NewRule([gdNorth], [KIT_BW_270, KIT_BR_270]); //itself or a matching corner

  //----------------------------------------------------------------------------
  //base corner constraints
  //----------------------------------------------------------------------------
  Result.AddValue(KIT_BR_000)
    .NewRule([gdSouth], [KIT_GR_000]) //north of grass
    .NewRule([gdNorth], [KIT_BW_090]); //south of a connecting wall

  Result.AddValue(KIT_BR_090)
    .NewRule([gdEast], [KIT_GR_000]) //west of grass
    .NewRule([gdSouth], [KIT_BW_090]); //north of a connecting wall

  Result.AddValue(KIT_BR_180)
    .NewRule([gdNorth], [KIT_GR_000]) //south of grass
    .NewRule([gdSouth], [KIT_BW_270]); //north of a connecting wall

  Result.AddValue(KIT_BR_270)
    .NewRule([gdWest], [KIT_GR_000]) //east of grass
    .NewRule([gdNorth], [KIT_BW_270]); //south of a connecting wall

  //----------------------------------------------------------------------------
  //mid wall constraints
  //----------------------------------------------------------------------------
  Result.AddValue(KIT_MW_000)
    .NewRule([gdEast], [KIT_MW_000, KIT_MR_000]) //itself or a matching corner
    .NewRule([gdWest], [KIT_MR_270]); //'opposite' corner

  Result.AddValue(KIT_MW_090)
    .NewRule([gdNorth], [KIT_MW_090, KIT_MR_090]); //itself or a matching corner

  Result.AddValue(KIT_MW_180)
    .NewRule([gdEast], [KIT_MW_180, KIT_MR_180]) //itself or a matching corner
    .NewRule([gdWest], [KIT_MR_270]); //'opposite' corner

  Result.AddValue(KIT_MW_270)
    .NewRule([gdNorth], [KIT_MW_270, KIT_MR_270]); //itself or a matching corner

  //----------------------------------------------------------------------------
  //mid corner constraints
  //----------------------------------------------------------------------------
  Result.AddValue(KIT_MR_000)
    .NewRule([gdNorth], [KIT_MW_090]); //south of a connecting wall

  Result.AddValue(KIT_MR_090)
    .NewRule([gdSouth], [KIT_MW_090]); //north of a connecting wall

  Result.AddValue(KIT_MR_180)
    .NewRule([gdSouth], [KIT_MW_270]); //north of a connecting wall

  Result.AddValue(KIT_MR_270)
    .NewRule([gdNorth], [KIT_MW_270]); //south of a connecting wall

  //----------------------------------------------------------------------------
  //top wall constraints
  //----------------------------------------------------------------------------
  Result.AddValue(KIT_TW_000)
    .NewRule([gdEast], [KIT_TW_000, KIT_TR_000]) //itself or a matching corner
    .NewRule([gdWest], [KIT_MR_270]); //'opposite' corner

  Result.AddValue(KIT_TW_090)
    .NewRule([gdNorth], [KIT_TW_090, KIT_TR_090]); //itself or a matching corner

  Result.AddValue(KIT_TW_180)
    .NewRule([gdEast], [KIT_TW_180, KIT_TR_180]) //itself or a matching corner
    .NewRule([gdWest], [KIT_TR_270]); //'opposite' corner

  Result.AddValue(KIT_TW_270)
    .NewRule([gdNorth], [KIT_TW_270, KIT_TR_270]); //itself or a matching corner

  //----------------------------------------------------------------------------
  //top corner constraints
  //----------------------------------------------------------------------------
  Result.AddValue(KIT_TR_000)
    .NewRule([gdNorth], [KIT_TW_090]); //south of a connecting wall

  Result.AddValue(KIT_TR_090)
    .NewRule([gdSouth], [KIT_TW_090]); //north of a connecting wall

  Result.AddValue(KIT_TR_180)
    .NewRule([gdSouth], [KIT_TW_270]); //north of a connecting wall

  Result.AddValue(KIT_TR_270)
    .NewRule([gdNorth], [KIT_TW_270]); //south of a connecting wall

  //----------------------------------------------------------------------------
  //roof constraints
  //----------------------------------------------------------------------------
  Result.AddValue(KIT_RF_000)
    .NewRule([gdNorth, gdEast, gdSouth, gdWest], KIT_RF_000) //can be surrounded by itself
    .NewRule([gdSouth], [KIT_TW_000]) //north of connecting wall
    .NewRule([gdSouth], [KIT_TW_090]) //west of connecting wall
    .NewRule([gdSouth], [KIT_TW_180]) //south of connecting wall
    .NewRule([gdSouth], [KIT_TW_270]); //east of connecting wall
end;

end.

