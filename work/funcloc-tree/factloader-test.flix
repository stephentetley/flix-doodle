
def logFail[r](reln: Result[r, String]): r = match reln {
    case Err(msg) => { Console.printLine(msg); #{ } }
    case Ok(x) => x
}

pub def treeSrc() : Str = "G:\work\Projects\assets\langs_data\LANGS_funcloc_tree.txt"


@test
def loaderTest01[r](): #{ FuncLoc2 | r } = 
    let facts = FactLoader.importFuncLoc2(treeSrc(), Charset.utf_8());
    facts |> logFail

@test
def loaderTest02[r](): #{ FuncLoc3 | r } = 
    let facts = FactLoader.importFuncLoc3(treeSrc(), Charset.utf_8());
    facts |> logFail
    
@test
def loaderTest03[r](): #{ Equi3, SubEqui | r } = 
    let facts = FactLoader.importEquis(treeSrc(), Charset.utf_8());
    facts |> logFail

@test
def loaderTest04[r](): #{ FuncLoc2, FuncLoc3, Equi3, SubEqui | r } = 
    let facts = FactLoader.importHierarchy(treeSrc(), Charset.utf_8());
    facts |> logFail
    
pub def flocAibSrc() : Str = "G:\work\Projects\assets\langs_data\LANGS_funclocs_with_AIB_refs.csv"

pub def equiAibSrc() : Str = "G:\work\Projects\assets\langs_data\LANGS_equipment_with_AIB_refs.csv"


@test
def flocAib01[r](): #{ FlocAib | r } = 
    let facts = FactLoader.importFlocAibs(flocAibSrc());
    facts |> logFail

@test
def equiAib01[r](): #{ EquiAib | r } = 
    let facts = FactLoader.importEquiAibs(equiAibSrc());
    facts |> logFail
