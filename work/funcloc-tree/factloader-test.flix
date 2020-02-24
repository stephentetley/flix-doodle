
def logFail[r](reln: Result[r, String]): r = match reln {
    case Err(msg) => { Console.printLine(msg); #{ } }
    case Ok(x) => x
}

pub def src() : Str = "G:\work\Projects\assets\asset_check\LANGS_funcloc_tree.txt"

@test
def loaderTest01[r](): #{ FuncLoc2 | r } = 
    let facts = FactLoader.importFuncLoc2(src(), Charset.utf_8());
    facts |> logFail

@test
def loaderTest02[r](): #{ FuncLoc3 | r } = 
    let facts = FactLoader.importFuncLoc3(src(), Charset.utf_8());
    facts |> logFail
    
@test
def loaderTest03[r](): #{ Equi3, SubEqui | r } = 
    let facts = FactLoader.importEquis(src(), Charset.utf_8());
    facts |> logFail
