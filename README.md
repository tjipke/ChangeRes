# Intro
This is the last version of the source code of ChangeRes from 2010
It does need an XMLParser.pas version 1.0.17 which has been added as a submodule, but can otherwise be found here: https://www.destructor.de/xmlparser/
The xmlparser submodule is https://github.com/cintex/delphi-xmlparser which relies on destructor.de Source Code Licence (DSL).
The original licensing code has been commented out.

# Compiling
Example using Codegear C++ Builder 2007
```
MSBUILD.exe .\source\ChangeRes.dproj /p:Configuration=Debug /t:Build
```