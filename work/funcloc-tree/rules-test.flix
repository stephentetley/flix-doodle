
def logFail[r](reln: Result[r, String]): r = match reln {
    case Err(msg) => { Console.printLine(msg); #{ } }
    case Ok(x) => x
}

pub def src() : Str = "G:\work\Projects\assets\langs_data\LANGS_funcloc_tree.txt"


@test
def parent01[r](): #{ Answer | r } = 
    let db = FactLoader.importHierarchy(src(), Charset.utf_8()) |> logFail;
    let rules = Parent.getFlocParents("LANGS-TWP-CRD-MAR-SYS03");
    project Answer (solve db <+> rules)

@test
def count01(): Int32 = 
    let db = FactLoader.importHierarchy(src(), Charset.utf_8()) |> logFail;
    Equipment1.equipmentCount(db)
    
    
