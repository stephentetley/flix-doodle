// Dynamically load relations.
// The iteration procedure `getFacts` is polymorphic

// Declare a relation
rel Dummy(name: String)

def makeDummy(name: String): #{ Dummy } = 
    Dummy(name).

def getFacts(f: a -> t , xs: List[a]): t = 
    match xs { 
        case Nil => #{ }
        case x :: rs => f(x) <+> getFacts(f, rs)
    }

pub def test01(): #{ Dummy } = 
    let names = "Becky" :: "Bill" :: Nil;
    solve getFacts(makeDummy, names)


