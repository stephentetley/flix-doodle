// Declare relations
rel Dummy(name: Str, intval: Int32)
rel IsZero(name: Str)

def getFacts[r](): #{ Dummy | r } = #{
    Dummy("a", 0).
    Dummy("b", 0).
    Dummy("c", 1).
    Dummy("d", 1).
}

def isZero[r](): #{ Dummy, IsZero| r} = #{
    IsZero(name) :- Dummy(name, 0).
}

pub def test01(): #{ Dummy, IsZero } = 
    solve getFacts() <+> isZero()

pub def test02(): Bool = 
    let m1 = solve getFacts() <+> isZero();
    m1 |= IsZero("a").

pub def test03(): Bool = 
    let m1 = solve getFacts() <+> isZero();
    m1 |= IsZero("c").



