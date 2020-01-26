// Dynamically load relations.
// The iteration procedure `getFacts` is monomorphic - that needs improving (see Dynamic2.flix)

// Declare a relation
rel Dummy(name: Str)

def makeDummy(name: Str): #{ Dummy } = 
    Dummy(name).

def getFacts(names: List[Str]): #{ Dummy } = 
    match names { 
        case Nil => #{ }
        case x :: rs => makeDummy(x) <+> getFacts(rs)
    }

pub def test01(): #{ Dummy } = 
    let names = "Alan" :: "Anna" :: Nil;
    solve (getFacts(names))

