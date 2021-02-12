# wfc
wave function collapse algorithm

# Features

some features at a glance...

* 2D & 3D graph 
* comfortable api to reshape and define constraints
* generic user defined values
* extendable classes if base behavior is not desired

# Sample

Below is a sample which shows a possible use for wfc:

```pascal
//easily define the shape (2D or 3D is supported)
LGraph := TGraph.Create.Reshape({width} 5, {height} 5, {depth} 1);

(*
  constraints are also very easy to add and the interface provide fluent setters.
  in this case A's & B's can be present in "any direction" on the graph (north, east, up, down, etc...)
*)
LGraph.AddValue('A')
  .NewRule(AllDirections, ['A', 'B']);

(*  
  but here, we constrain to just a few directions 
  A or B (N & S) of a "C"
*)
LGraph.AddValue('C')
  .NewRule([gdNorth, gdSouth], ['A', 'B']); 
  
(*
  when you're finished defining constraints and all of the
  possible values (states), then just run and the result can be 
  persisted or you can access the completed graph directly
*)
LGraph.Run();
```

# How To Use

1. download and install lazarus if you don't already have it (http://www.lazarus-ide.org)
1. git clone this repo
    * some examples have submodule dependencies so if you want to clone this repo and bring in all of those do a `git clone --recursive`
    * if you already cloned you can update submodules with `git submodule update --init --recursive`
1. open wfc_test.lpr and attempt to compile/run (F9 Key)
    * this project shows some basic usage of the library
    * also, by going to `Toolbar -> Project\Project Options\Paths` you can copy the `other units` text to include in your own project
1. add `.\src` path to your project `other units`


**Tip Jar**
  * :dollar: BTC - bc1q55qh7xptfgkp087sfr5ppfkqe2jpaa59s8u2lz
  * :euro: LTC - LPbvTsFDZ6EdaLRhsvwbxcSfeUv1eZWGP6

