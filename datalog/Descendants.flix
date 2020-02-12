
pub rel ParentOf(child: String, parent: String)
pub rel Descendant(sub: String, super: String)
pub rel Person(id: String)


def m1[r](): #{ Descendant, ParentOf | r } = solve #{
    
    ParentOf("Bruce", "Anna").
    ParentOf("Carol", "Bruce").
    ParentOf("Chris", "Bruce").

    Descendant(sub, super) :- ParentOf(sub, super).
    Descendant(sub, super) :- ParentOf(sub, x), Descendant(x, super).
}

@test
def test01[r](): #{ Person | r } = 
    project Person (solve m1() <+> #{ Person(z) :- Descendant(z, "Anna"). })

def getDescendants[r](ancestor: String): #{ Person | r } = 
    let isAncestor = x -> x == ancestor;
    project Person (solve m1() <+> #{ Person(z) :- isAncestor(p), Descendant(z, p). })

@test
def test02[r](): #{ Person | r } = getDescendants("Anna")

@test
def test03[r](): #{ Person | r } = getDescendants("Bruce")

@test
def test04[r](): #{ Person | r } = getDescendants("Carol")

