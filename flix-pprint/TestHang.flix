
def node(name: Str, kids: Array[Pretty.Doc]): Pretty.Doc = 
    match Array.toList(kids) {
        case Nil => Pretty.brackets(Pretty.text(name))
        case ks => Pretty.hang(4, Pretty.brackets(Pretty.vlinebreak(Pretty.text(name), Pretty.vcat(ks))))
    }

def tree1() : Pretty.Doc = 
    node("factx-fsharp",
        [ node("src",
            [ node("bin", [])
            , node("FactX",
                [ node("Common.fs", [])
                , node("FactOutput.fs", [])
                , node("FactWriter.fs", [])
                , node("Pretty.fs", [])
                , node("Syntax.fs", [])
                ])
            ])
        , node("test",
            [ node("FactXTest.fsproj", [])
            , node("Test01.fsx", [])
            ])
        ])

pub def demo01() : Str = 
    Pretty.prettyPrint(99, tree1())
