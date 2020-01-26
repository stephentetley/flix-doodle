// Declare relations
rel Dummy(name: Str, whatever:Str, intval: Int32)
rel IsZero(name: Str)

def getFacts[r](): #{ Dummy | r } = #{
    Dummy("a", "blue", 0).
    Dummy("b", "red", 0).
    Dummy("c", "green", 1).
    Dummy("d", "yellow", 1).
}

// variables do not need to be present on the left hand side
def isZero[r](): #{ Dummy, IsZero | r} = #{
    IsZero(name) :- Dummy(name, z, 0).
}

pub def test01(): #{ Dummy, IsZero } = 
    solve getFacts() <+> isZero()


// relations can be passed about
def test02Body(): Option[#{ Dummy, IsZero }] = 
    let m1 = solve getFacts() <+> isZero();
    Some(m1)

pub def test02(): #{ Dummy, IsZero } = 
    match test02Body() {
        case Some(relns) => relns
        case None => #{ }
    }





