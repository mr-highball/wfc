# Simple Building Kit model

The maintained source in this historical directory is the project-owned
building-rule unit `code/wfc.buildkit.pas`. It imports the standard runtime and
`wfc`; it has no engine or renderer types. Its path is retained for source
compatibility with the native `../tester.lpr` console.

The former engine launcher was never connected to the generated model and has
been removed with its project metadata, resources, and UI data. For maintained
depth-greater-than-one generation and graphical output, use the
[Multi-pass Building](../../02_MultiPassBuilding/README.md) native example or
the [Building 3D viewer](../../03_BrowserBuilding/README.md), which shares one
project-owned command model across native SVG and pas2js/Canvas2D.
