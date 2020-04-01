

@test
def sort01(): List[Int32] = 
    ListSort.sortBy((x,y) -> if (x <= y) -1 else 1, 1 :: 4 :: 3 :: 2 :: Nil)


// minus and duplicates
@test
def sort02(): List[Int32] = 
    ListSort.sortBy((x,y) -> if (x <= y) -1 else 1, 1 :: 4 :: 3 :: 2 :: -1 :: 2 :: 0 :: 3 :: Nil)