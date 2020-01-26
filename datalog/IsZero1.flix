// Declare relations
rel Dummy(name: Str, intval: Int32)
rel IsZero(name: Str)


pub def test01(): #{ Dummy, IsZero } = solve #{
    Dummy("a", 0).
    Dummy("b", 0).
    Dummy("c", 1).
    Dummy("d", 1).

    IsZero(name) :- Dummy(name, 0).
}

