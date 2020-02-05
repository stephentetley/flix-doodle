rel Node(path: String, name: String)
rel Superior(childpath: String, parentpath: String)
rel Root(path: String, name: String)

pub def getRoot[r](): #{ Root, Node, Superior | r } = #{
    Root(x, s) :- Node(x, s), not Superior(x, _).
}

pub def nodes[r](): #{ Node | r } = #{
    Node("TOP", "Top node").
    Node("TOP-KID1", "Child_1").
    Node("TOP-KID2", "Child_2").
}

pub def hierarchy[r](): #{ Superior | r } = #{
    Superior("TOP-KID1", "TOP").
    Superior("TOP-KID2", "TOP").
}

/// Just solve - this shows the starting facts as weell as Root...
pub def test01(): #{ Root, Node, Superior } =
    let facts = nodes() <+> hierarchy();
    solve facts <+> getRoot()

/// Project Root out of the solution results...
pub def test02(): #{ Root } =
    let facts = nodes() <+> hierarchy();
    project Root (solve getRoot() <+> facts)

/// Likewise, we can get the Node(s) although they were known before solving...
pub def test03(): #{ Node } =
    let facts = nodes() <+> hierarchy();
    project Node (solve getRoot() <+> facts)


def printRoot(root: (String, String), acc: Unit): Unit = 
    let (path,name) = root;
    Console.printLine(path + " # `" + name + "`");
    acc

/// Compare with fold...
pub def test04(): Unit = 
    let facts = nodes() <+> hierarchy();
    fold Root () printRoot (solve getRoot() <+> facts)
