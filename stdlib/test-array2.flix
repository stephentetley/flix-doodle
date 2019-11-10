
/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def append01(): Bool = 
    let a = [1,2,3,4,5];
    let b = [6,7,8];
    Array.length(Array2.append(a,b)) `assertEq!` 8

@test
def append02(): Bool = 
    let a = [0,1,2,3,4,5];
    let b = [6,7,8];
    let c = Array2.append(a,b);
    c[0] `assertEq!` 0 && c[5] `assertEq!` 5 && c[6] `assertEq!` 6 && c[8] `assertEq!` 8


/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01(): Bool = 
    let a = Array.new(1, 10);
    let fn = _ -> 'a';
    assert!(Array2.forall(x -> x=='a', Array2.map(fn, a)))

@test
def map02(): Bool = 
    let a : Array[Int] = [];
    let fn = _ -> 'a';
    Array2.map(fn, a) |> Array.length `assertEq!` 0

/////////////////////////////////////////////////////////////////////////////
// map2                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map201(): Bool = 
    let a = Array.new(1, 10);
    let b = Array.new(1, 7);
    let f = (x,y) -> x+y;
    Array.length(Array2.map2(f, a, b)) `assertEq!` 7


/////////////////////////////////////////////////////////////////////////////
// mapWithIndex                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def mapWithIndex01(): Bool = 
    let a = Array.new(1, 10);
    let fn = (_, ix) -> ix;
    Array2.foldLeft(Int32.max, 0, Array2.mapWithIndex(fn, a)) `assertEq!` 9

@test 
def count01(): Bool = 
    let a = [1,2,3,4,5];
    Array2.count(x -> x > 3, a) `assertEq!` 2

@test
def foldLeft01(): Bool = 
    let a = Array.new(1, 10);
    let fn = x -> y -> x + y;
    Array2.foldLeft(fn, 0, a) `assertEq!` 10

@test
def fold01(): Bool = 
    let a = Array.new(1, 10);
    let fn = x -> y -> x + y;
    Array2.fold(fn, 0, a) `assertEq!` 10

@test
def foldLeft201(): Bool = 
    let a = Array.new(1, 10);
    let b = Array.new(1, 7);
    let fn = xs -> x -> y -> (x + y) :: xs;
    List.length(Array2.foldLeft2(fn, Nil, a, b)) `assertEq!` 7

@test
def foldRight01(): Bool = 
    let a = Array.new(1, 10);
    let fn = x -> y -> x + y;
    Array2.foldRight(fn, 0, a) `assertEq!` 10

@test
def foldRight201(): Bool = 
    let a = [1,2,3,4,5,6,7,8,9,0];
    let b = [1,2,3,4,5,6,7];
    let fn = (x, y, xs) -> (x + y) :: xs;
    List.length(Array2.foldRight2(fn, Nil, a, b)) `assertEq!` 7

/////////////////////////////////////////////////////////////////////////////
// take                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def take01(): Bool = 
    let a = Array.new(1, 10);
    Array.length(Array2.take(2, a)) `assertEq!` 2

@test
def drop01(): Bool = 
    let a = Array.new(1, 10);
    Array.length(Array2.drop(3, a)) `assertEq!` 7


@test
def takeWhile01(): Bool = 
    let a = [1,2,3,4,5,6,7,8];
    Array.length(Array2.takeWhile(x -> x < 5, a)) `assertEq!` 4

@test
def dropWhile01(): Bool = 
    let a = [1,2,3,4,5,6,7,8];
    Array.length(Array2.dropWhile(x -> x < 5, a)) `assertEq!` 4


@test
def exists01(): Bool = 
    let a = Array.new(1, 10);
    a[6] = 2;
    assert!(Array2.exists(x -> x > 1, a))

@test
def forall01(): Bool = 
    let a = Array.new(1, 10);
    assert!(Array2.forall(x -> x == 1, a))

@test
def memberOf01(): Bool = 
    // let a = toArray(1 :: 2 :: 3 :: Nil);
    let a: Array[Int] = [1, 2, 3];
    assert!(Array2.memberOf(2, a))

@test
def indexOf01(): Bool = 
    let a: Array[Int] = [0, 1, 2, 3];
    Array2.indexOf(2, a) `assertEq!` 2

@test
def findLeft01(): Bool = 
    let a: Array[Int] = [0, 1, 2, 3, 4, 5];
    Array2.findLeft(x -> x > 2, a) `assertEq!` Some(3)


@test
def findRight01(): Bool = 
    let a: Array[Int] = [0, 1, 2, 3, 4, 5];
    Array2.findRight(x -> x > 2, a) `assertEq!` Some(5)


@test
def filter01(): Bool = 
    let a: Array[Int] = [0, 1, 2, 3, 4, 5];    
    Array.length(Array2.filter(x -> x % 2 == 0, a)) `assertEq!` 3


@test
def filterMap01(): Bool = 
    let a: Array[Int] = [0, 1, 2, 3, 4, 5];    
    Array.length(Array2.filterMap(x -> if (x % 2 == 0) Some(x) else None, a)) `assertEq!` 3

@test 
def reverse01(): Bool = 
    let a: Array[Int] = [0, 1, 2, 3, 4, 5];
    let b = Array2.reverse(a);
    b[0] `assertEq!` 5 && b[1] `assertEq!` 4


/////////////////////////////////////////////////////////////////////////////
// rotateLeft                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test 
def rotateLeft01(): Bool = 
    let a: Array[Int] = [1, 2, 3, 4, 5];
    let b = Array2.rotateLeft(2, a);
    b[0] `assertEq!` 3 && b[1] `assertEq!` 4


/////////////////////////////////////////////////////////////////////////////
// rotateRight                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test 
def rotateRight01(): Bool = 
    let a: Array[Int] = [1, 2, 3, 4, 5];
    let b = Array2.rotateRight(2, a);
    b[0] `assertEq!` 4 && b[1] `assertEq!` 5



