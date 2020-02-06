// Declare relations
rel Dummy(name: String, intval: Int32)
rel IsZero(name: String)


pub def test01(): #{ Dummy, IsZero } = solve #{
    Dummy("a", 0).
    Dummy("b", 0).
    Dummy("c", 1).
    Dummy("d", 1).

    IsZero(name) :- Dummy(name, 0).
}

