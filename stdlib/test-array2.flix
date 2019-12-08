/////////////////////////////////////////////////////////////////////////////
// range                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def range01(): Bool = 
    let a = Array2.range(1, 0);
    Array.toList(a) `assertEq!` Nil

@test
def range02(): Bool = 
    let a = Array2.range(1, 1);
    Array.toList(a) `assertEq!` Nil

@test
def range03(): Bool = 
    let a = Array2.range(1, 2);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def range04(): Bool = 
    let a = Array2.range(1, 3);
    Array.toList(a) `assertEq!` 1 :: 2 :: Nil

@test
def range05(): Bool = 
    let a = Array2.range(1, 4);
    Array.toList(a) `assertEq!` 1 :: 2 :: 3 :: Nil

@test
def range06(): Bool = 
    let a = Array2.range(-1, 3);
    Array.toList(a) `assertEq!` (-1) :: 0 :: 1 :: 2 :: Nil

/////////////////////////////////////////////////////////////////////////////
// append                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def append01(): Bool = 
    let a: Array[Int32] = [];
    let b: Array[Int32] = [];
    let c = Array2.append(a,b);
    Array.toList(c) `assertEq!` Nil

@test
def append02(): Bool = 
    let a = [1,2,3,4,5];
    let b = [6,7,8];
    let c = Array2.append(a,b);
    Array.toList(c) `assertEq!` 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil

@test
def append03(): Bool = 
    let a = [1,2,3,4,5];
    let b: Array[Int32] = [];
    let c = Array2.append(a,b);
    Array.toList(c) `assertEq!` 1 :: 2 :: 3 :: 4 :: 5 :: Nil

/////////////////////////////////////////////////////////////////////////////
// flatten                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatten01(): Bool = 
    let a: Array[Array[Int32]] = [];
    let b = Array2.flatten(a);
    Array.toList(b) `assertEq!` Nil

@test
def flatten02(): Bool = 
    let a: Array[Array[Int32]] = [[1,2],[3],[4,5],[]];
    let b = Array2.flatten(a);
    Array.toList(b) `assertEq!` 1 :: 2 :: 3 :: 4 :: 5 :: Nil


@test
def flatten03(): Bool = 
    let a: Array[Array[Int32]] = [[],[],[],[]];
    let b = Array2.flatten(a);
    Array.toList(b) `assertEq!` Nil

/////////////////////////////////////////////////////////////////////////////
// toString                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def toString01(): Bool = 
    let a: Array[Char] = [];
    Array2.toString(a) `assertEq!` ""

@test
def toString02(): Bool = 
    let a = ['a', 'b', 'c'];
    Array2.toString(a) `assertEq!` "abc"


/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def map01(): Bool = 
    let a: Array[Int] = [];
    let fn = _ -> 'a';
    let b = Array2.map(fn, a);
    Array.toList(b) `assertEq!` Nil
    
@test
def map02(): Bool = 
    let a = Array.new(1, 3);
    let fn = _ -> 'a';
    let b = Array2.map(fn, a);
    Array.toList(b) `assertEq!` 'a' :: 'a' :: 'a' :: Nil


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
def mapWithIndex02(): Bool =
    let a:Array[Int32] = Array2.mapWithIndex((e, i) -> Int32.max(e,i), []);
    Array.toList(a) `assertEq!` Nil

@test
def mapWithIndex03(): Bool =
    let a = Array.new(3, 6);
    let b:Array[Int32] = Array2.mapWithIndex((e, i) -> Int32.max(e,i), a);
    Array.toList(b) `assertEq!` 3 :: 3 :: 3 :: 3 :: 4 :: 5 :: Nil



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
    Array2.indexOf(2, a) `assertEq!` Some(2)

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


/////////////////////////////////////////////////////////////////////////////
// reverse                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test 
def reverse01(): Bool = 
    let a: Array[Int] = [1, 2, 3, 4, 5];
    let b = Array2.reverse(a);
    Array.toList(b) `assertEq!` 5 :: 4 :: 3 :: 2 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// scan                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def scan01(): Bool = 
    let a = Array2.scan((i, b) -> if (b) i+1 else i+2, 1, []);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def scan02(): Bool = 
    let a = Array2.scan((i, b) -> if (b) i+1 else i+2, 1, [false]);
    Array.toList(a) `assertEq!` 1 :: 3 :: Nil


/////////////////////////////////////////////////////////////////////////////
// scanRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def scanRight01(): Bool = 
    let a = Array2.scanRight((b, i) -> if (b) i+1 else i+2, 1, []);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def scanRight02(): Bool = 
    let a = Array2.scanRight((b, i) -> if (b) i+1 else i+2, 1, [false]);
    Array.toList(a) `assertEq!` 3 :: 1 :: Nil


/////////////////////////////////////////////////////////////////////////////
// rotateLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@test 
def rotateLeft01(): Bool = 
    let a: Array[Int] = [1, 2, 3, 4, 5];
    let b = Array2.rotateLeft(2, a);
    Array.toList(b) `assertEq!` 3 :: 4 :: 5 :: 1 :: 2 :: Nil


/////////////////////////////////////////////////////////////////////////////
// rotateRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@test 
def rotateRight01(): Bool = 
    let a: Array[Int] = [1, 2, 3, 4, 5];
    let b = Array2.rotateRight(2, a);
    Array.toList(b) `assertEq!` 4 :: 5 :: 1 :: 2 :: 3 :: Nil


/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def replace01(): Bool = 
    let a = Array2.replace(3, 4, []);
    Array.toList(a) `assertEq!` Nil

@test
def replace02(): Bool = 
    let a = Array2.replace(3, 4, [1]); 
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def replace03(): Bool = 
    let a = Array2.replace(3, 4, [3]); 
    Array.toList(a) `assertEq!` 4 :: Nil

@test
def replace04(): Bool = 
    let a = Array2.replace(3, 4, [4]); 
    Array.toList(a) `assertEq!` 4 :: Nil

@test
def replace05(): Bool = 
    let a = Array2.replace(3, 4, [1, 2]); 
    Array.toList(a) `assertEq!` 1 :: 2 :: Nil

@test
def replace06(): Bool = 
    let a = Array2.replace(3, 4, [1, 3]); 
    Array.toList(a) `assertEq!` 1 :: 4 :: Nil

@test
def replace07(): Bool = 
    let a = Array2.replace(3, 4, [3, 4]); 
    Array.toList(a) `assertEq!` 4 :: 4 :: Nil

@test
def replace08(): Bool = 
    let a = Array2.replace(3, 4, [3, 3]); 
    Array.toList(a) `assertEq!` 4 :: 4 :: Nil


/////////////////////////////////////////////////////////////////////////////
// zip                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def zip01(): Bool = 
    let a = Array2.init(identity, 10);
    let b = Array2.init(identity, 5);
    let c = Array2.zip(a, b);
    Array.toList(c) `assertEq!` (0,0) :: (1,1) :: (2,2) :: (3,3) :: (4,4) :: Nil

/////////////////////////////////////////////////////////////////////////////
// zipWith                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def zipWith01(): Bool = 
    let a = Array2.init(identity, 10);
    let b = Array2.init(identity, 5);
    let c = Array2.zipWith((x,y) -> x + y, a, b);
    Array.toList(c) `assertEq!` 0 :: 2 :: 4 :: 6 :: 8 :: Nil


/////////////////////////////////////////////////////////////////////////////
// transpose                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def transpose01(): Bool = 
    let a = Array2.transpose([]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!` Nil


@test
def transpose02(): Bool = 
    let a: Array[Array[Int32]] = Array2.transpose([ [] ]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!` Nil :: Nil

@test
def transpose03(): Bool = 
    let a: Array[Array[Int32]] = Array2.transpose([ [], [] ]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!` Nil :: Nil :: Nil

@test
def transpose04(): Bool = 
    let a: Array[Array[Int32]] = Array2.transpose([ [], [], [] ]);
    let b = Array.toList(a) |> List.map(Array.toList);   
    b `assertEq!` Nil :: Nil :: Nil :: Nil

@test
def transpose05(): Bool = 
    let a = Array2.transpose([ [1] ]);
    let b = Array.toList(a) |> List.map(Array.toList);   
    b `assertEq!` (1 :: Nil) :: Nil

@test
def transpose06(): Bool = 
    let a = Array2.transpose([[1,2]]);
    let b = Array.toList(a) |> List.map(Array.toList);   
    b `assertEq!` (1 :: Nil) :: (2 :: Nil) :: Nil

@test
def transpose07(): Bool = 
    let a = Array2.transpose([[1, 2, 3]]);
    let b = Array.toList(a) |> List.map(Array.toList);   
    b `assertEq!` (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil

@test
def transpose08(): Bool = 
    let a = Array2.transpose([[1, 2, 3, 4]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!` (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil

@test
def transpose09(): Bool = 
    let a = Array2.transpose([[1], [2]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!`   (1 :: 2 :: Nil) :: Nil

@test
def transpose10(): Bool = 
    let a = Array2.transpose([[1], [2], [3]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!`  (1 :: 2 :: 3 :: Nil) :: Nil

@test
def transpose11(): Bool = 
    let a = Array2.transpose([[1], [2], [3], [4]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!` (1 :: 2 :: 3 :: 4 :: Nil) :: Nil

@test
def transpose12(): Bool = 
    let a = Array2.transpose([[1, 2], [3, 4]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!` (1 :: 3 :: Nil) :: (2 :: 4 :: Nil) :: Nil

@test
def transpose13(): Bool = 
    let a = Array2.transpose([[1, 2, 3], [4, 5, 6]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!` (1 :: 4 :: Nil) :: (2 :: 5 :: Nil) :: (3 :: 6 :: Nil) :: Nil

@test
def transpose14(): Bool = 
    let a = Array2.transpose([[1, 2, 3, 4], [5, 6, 7, 8]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!` (1 :: 5 :: Nil) :: (2 :: 6 :: Nil) :: (3 :: 7 :: Nil) :: (4 :: 8 :: Nil) :: Nil

@test
def transpose15(): Bool = 
    let a = Array2.transpose([[1, 2, 3, 4, 5], [6, 7, 8, 9, 10]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!` (1 :: 6 :: Nil) :: (2 :: 7 :: Nil) :: (3 :: 8 :: Nil) :: (4 :: 9 :: Nil) :: (5 :: 10 :: Nil) :: Nil

@test
def transpose16(): Bool = 
    let a = Array2.transpose([[1, 2], [3, 4], [5, 6]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!` (1 :: 3 :: 5 :: Nil) :: (2 :: 4 :: 6 :: Nil) :: Nil

@test
def transpose17(): Bool = 
    let a = Array2.transpose([[1, 2], [3, 4], [5, 6], [7,8]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!` (1 :: 3 :: 5 :: 7 :: Nil) :: (2 :: 4 :: 6 :: 8 :: Nil) :: Nil

@test
def transpose18(): Bool = 
    let a = Array2.transpose([[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!` (1 :: 3 :: 5 :: 7 :: 9 :: Nil) :: (2 :: 4 :: 6 :: 8 :: 10 :: Nil) :: Nil

@test
def transpose19(): Bool = 
    let a = Array2.transpose([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!` (1 :: 4 :: 7 :: Nil) :: (2 :: 5 :: 8 :: Nil) :: (3 :: 6 :: 9 :: Nil) :: Nil

@test
def transpose20(): Bool =
    let a = Array2.transpose([[1, 2, 3], [4, 5], [7, 8, 9]]);
    let b = Array.toList(a) |> List.map(Array.toList);   
    b `assertEq!` (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil

@test
def transpose21(): Bool =
    let a = Array2.transpose([[1, 2, 3], [], [7, 8, 9]]);
    let b = Array.toList(a) |> List.map(Array.toList);   
    b `assertEq!` (1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil

@test
def transpose22(): Bool = 
    let a = Array2.transpose([[1, 2, 3], [4, 5, 6], [7, 8, 9, 10]]);
    let b = Array.toList(a) |> List.map(Array.toList);   
    b `assertEq!` (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil

