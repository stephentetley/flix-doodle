// Dynamically load relations.
// The iteration procedure `getFacts` is monomorphic - that needs improving (see Dynamic2.flix)

// Declare a relation
rel Dummy(name: String)

def makeDummy(name: String): #{ Dummy } = 
    Dummy(name).

def getFacts(names: List[String]): #{ Dummy } = 
    match names { 
        case Nil => #{ }
        case x :: rs => makeDummy(x) <+> getFacts(rs)
    }

pub def test01(): #{ Dummy } = 
    let names = "Alan" :: "Anna" :: Nil;
    solve (getFacts(names))

