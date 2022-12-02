# ![nebulous](https://raw.githubusercontent.com/aornota/nebulous/main/src/assets/images/nebulous.png) | nebulous (β)

Experimental visuals based on [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) using [Avalonia.FuncUI](https://github.com/fsprojects/Avalonia.FuncUI).

Used (in an adapted form) in [my F# audio player](https://github.com/aornota/fap).

#### Development prerequisites

- [Microsoft .NET 6.0 SDK](https://dotnet.microsoft.com/en-us/download/dotnet/6.0): I'm currently using 6.0.306

##### Also recommended

- [Microsoft Visual Studio Code](https://code.visualstudio.com/download/) with the following extensions:
    - [C#](https://marketplace.visualstudio.com/items?itemName=ms-dotnettools.csharp)
    - [Ionide for F#](https://marketplace.visualstudio.com/items?itemName=ionide.ionide-fsharp)
    - [EditorConfig for VS Code](https://marketplace.visualstudio.com/items?itemName=editorconfig.editorconfig)
    - [Rainbow Brackets](https://marketplace.visualstudio.com/items?itemName=2gua.rainbow-brackets)

#### Running / building

- Before first running / building:
    - _dotnet tool restore_
    - _cd src_ then _dotnet restore_
- Run / build:
    - Run (debug): _dotnet run_
    - Build (debug): _dotnet build_
    - Build (release): _dotnet build -c release_
