/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def isEmpty01(): Bool = assert!(Array2.isEmpty([]: Array[Int32]))

@test
def isEmpty02(): Bool = assertNot!(Array2.isEmpty([1]))

@test
def isEmpty03(): Bool = assertNot!(Array2.isEmpty([1,2]))

/////////////////////////////////////////////////////////////////////////////
// init                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def init01(): Bool =
    let a = Array2.init(x -> x, 0);
    Array.toList(a) `assertEq!` Nil

@test
def init02(): Bool =
    let a = Array2.init(x -> x, -1);
    Array.toList(a) `assertEq!` Nil

@test
def init03(): Bool =
    let a = Array2.init(x -> x, 1);
    Array.toList(a) `assertEq!` 0 :: Nil

@test
def init04(): Bool =
    let a = Array2.init(x -> x, 2);
    Array.toList(a) `assertEq!` 0 :: 1 :: Nil

@test
def init05(): Bool =
    let a = Array2.init(x -> x, 50);
    Array.length(a) `assertEq!` 50

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
    let a: Array[Int32] = [];
    let b: Array[Int32] = [1];
    let c = Array2.append(a,b);
    Array.toList(c) `assertEq!` 1 :: Nil

@test
def append03(): Bool = 
    let a: Array[Int32] = [];
    let b: Array[Int32] = [1,2];
    let c = Array2.append(a,b);
    Array.toList(c)  `assertEq!` 1 :: 2 :: Nil

@test
def append04(): Bool = 
    let a: Array[Int32] = [1];
    let b: Array[Int32] = [];
    let c = Array2.append(a,b);
    Array.toList(c) `assertEq!` 1 :: Nil

@test
def append05(): Bool = 
    let a: Array[Int32] = [1,2];
    let b: Array[Int32] = [];
    let c = Array2.append(a,b);
    Array.toList(c)  `assertEq!` 1 :: 2 :: Nil

@test
def append06(): Bool =
    let a: Array[Int32] = [1];
    let b: Array[Int32] = [2];
    let c = Array2.append(a,b);
    Array.toList(c)  `assertEq!` 1 :: 2 :: Nil

@test
def append07(): Bool =
    let a: Array[Int32] = [1,2];
    let b: Array[Int32] = [3];
    let c = Array2.append(a,b);
    Array.toList(c)  `assertEq!` 1 :: 2 :: 3 :: Nil

@test
def append08(): Bool = 
    let a: Array[Int32] = [1];
    let b: Array[Int32] = [2,3];
    let c = Array2.append(a,b);
    Array.toList(c)  `assertEq!` 1 :: 2 :: 3 :: Nil

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
    let a: Array[Array[Int32]] = [[]];
    let b = Array2.flatten(a);
    Array.toList(b) `assertEq!` Nil

@test
def flatten03(): Bool = 
    let a: Array[Array[Int32]] = [[1]];
    let b: Array[Int32] = Array2.flatten(a);
    Array.toList(b) `assertEq!` 1 :: Nil

@test
def flatten04(): Bool = 
    let a: Array[Array[Int32]] = [[1,2]];
    let b: Array[Int32] = Array2.flatten(a);
    Array.toList(b) `assertEq!` 1 :: 2 :: Nil

@test
def flatten05(): Bool = 
    let a: Array[Array[Int32]] = [[],[]];
    let b: Array[Int32] = Array2.flatten(a);
    Array.toList(b) `assertEq!` Nil

@test
def flatten06(): Bool = 
    let a: Array[Array[Int32]] = [[1],[]];
    let b: Array[Int32] = Array2.flatten(a);
    Array.toList(b) `assertEq!` 1 :: Nil

@test
def flatten07(): Bool = 
    let a: Array[Array[Int32]] = [[],[1]];
    let b: Array[Int32] = Array2.flatten(a);
    Array.toList(b) `assertEq!` 1 :: Nil

@test
def flatten08(): Bool = 
    let a: Array[Array[Int32]] = [[1],[2]];
    let b: Array[Int32] = Array2.flatten(a);
    Array.toList(b) `assertEq!` 1 :: 2 :: Nil

@test
def flatten09(): Bool = 
    let a: Array[Array[Int32]] = [[1,2],[3,4,5]];
    let b: Array[Int32] = Array2.flatten(a);
    Array.toList(b) `assertEq!` 1 :: 2 :: 3 :: 4 :: 5 :: Nil


@test
def flatten10(): Bool = 
    let a: Array[Array[Int32]] = [[1],[2,3],[4]];
    let b: Array[Int32] = Array2.flatten(a);
    Array.toList(b) `assertEq!` 1 :: 2 :: 3 :: 4 :: Nil


/////////////////////////////////////////////////////////////////////////////
// toString                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def toString01(): Bool = 
    let a: Array[Char] = [];
    Array2.toString(a) `assertEq!` ""

@test
def toString02(): Bool = 
    let a = ['A', 'B', 'C'];
    Array2.toString(a) `assertEq!` "ABC"

@test
def toString03(): Bool = 
    let a = ['1', '2', '3'];
    Array2.toString(a) `assertEq!` "123"


/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01(): Bool = 
    let a = Array2.map(i -> i > 2, []);
    Array.toList(a) `assertEq!` Nil

@test
def map02(): Bool = 
    let a = Array2.map(i -> i > 2, [1]);
    Array.toList(a) `assertEq!` false :: Nil

@test
def map03(): Bool = 
    let a = Array2.map(i -> i > 2, [3]);
    Array.toList(a) `assertEq!` true :: Nil

@test
def map04(): Bool = 
    let a = Array2.map(i -> i > 2, [1,2]);
    Array.toList(a) `assertEq!` false :: false :: Nil

@test
def map05(): Bool = 
    let a = Array2.map(i -> i > 2, [1,8]);
    Array.toList(a) `assertEq!` false :: true :: Nil

@test
def map06(): Bool = 
    let a = Array2.map(i -> i > 2, [8,1]);
    Array.toList(a) `assertEq!` true :: false :: Nil

@test
def map07(): Bool =
    let a = Array2.map(i -> i > 2, [7,8]);
    Array.toList(a) `assertEq!` true :: true :: Nil

/////////////////////////////////////////////////////////////////////////////
// map2                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map201(): Bool = 
    let c = Array2.map2((a, b) -> if (b) a+1 else a, [], []);
    Array.toList(c) `assertEq!` Nil

@test
def map202(): Bool = // List.map2((a, b) -> if (b) a+1 else a, 1 :: Nil, Nil) `assertEq!` Nil
    let c = Array2.map2((a, b) -> if (b) a+1 else a, [1], []);
    Array.toList(c) `assertEq!` Nil

@test
def map203(): Bool = // List.map2((a, b) -> if (b) a+1 else a, Nil, true :: Nil) `assertEq!` Nil
    let c = Array2.map2((a, b) -> if (b) a+1 else a, [], [true]);
    Array.toList(c) `assertEq!` Nil

@test
def map204(): Bool = // List.map2((a, b) -> if (b) a+1 else a, 1 :: Nil, true :: Nil) `assertEq!` 2 :: Nil
    let c = Array2.map2((a, b) -> if (b) a+1 else a, [1], [true]);
    Array.toList(c) `assertEq!` 2 :: Nil

@test
def map205(): Bool = // List.map2((a, b) -> if (b) a+1 else a, 1 :: Nil, false :: Nil) `assertEq!` 1 :: Nil
    let c = Array2.map2((a, b) -> if (b) a+1 else a, [1], [false]);
    Array.toList(c) `assertEq!` 1 :: Nil

@test
def map206(): Bool = 
    let c = Array2.map2((a, b) -> if (b) a+1 else a, [1,2,3,4,5,6,7,8], [false,true,true,false,false,true,true,true]);
    Array.toList(c) `assertEq!` 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil

/////////////////////////////////////////////////////////////////////////////
// mapWithIndex                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def mapWithIndex01(): Bool =
    let a = Array2.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, []);
    Array.toList(a) `assertEq!` Nil

@test
def mapWithIndex02(): Bool = 
    let a = Array2.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [1]);
    Array.toList(a) `assertEq!` false :: Nil

@test
def mapWithIndex03(): Bool = 
    let a = Array2.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [3]);
    Array.toList(a) `assertEq!` true :: Nil

@test
def mapWithIndex04(): Bool = 
    let a = Array2.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [1,2]);
    Array.toList(a) `assertEq!` false :: true :: Nil

@test
def mapWithIndex05(): Bool = 
    let a = Array2.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [1,8]);
    Array.toList(a) `assertEq!` false :: false :: Nil

@test
def mapWithIndex06(): Bool = 
    let a = Array2.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [8,1]);
    Array.toList(a) `assertEq!` true :: true :: Nil

@test
def mapWithIndex07(): Bool = 
    let a = Array2.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [7,8]);
    Array.toList(a) `assertEq!` true :: false :: Nil

/////////////////////////////////////////////////////////////////////////////
// flatMap                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatMap01(): Bool = 
    let a = Array2.flatMap(i -> Array2.repeat(i, i), []);
    Array.toList(a) `assertEq!` Nil

@test
def flatMap02(): Bool = 
    let a = Array2.flatMap(i -> Array2.repeat(i, i), [0]);
    Array.toList(a) `assertEq!` Nil

@test
def flatMap03(): Bool = 
    let a = Array2.flatMap(i -> Array2.repeat(i, i), [1]);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def flatMap04(): Bool =
    let a = Array2.flatMap(i -> Array2.repeat(i, i), [2]);
    Array.toList(a) `assertEq!` 2 :: 2 :: Nil

@test
def flatMap05(): Bool =
    let a = Array2.flatMap(i -> Array2.repeat(i, i), [1,2]);
    Array.toList(a) `assertEq!` 1 :: 2 :: 2 :: Nil


@test
def flatMap06(): Bool =
    let a = Array2.flatMap(i -> Array2.repeat(i, i), [2,3]);
    Array.toList(a) `assertEq!` 2 :: 2 :: 3 :: 3 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def count01(): Bool = Array2.count(i -> i > 3, []) `assertEq!` 0

@test
def count02(): Bool = Array2.count(i -> i > 3, [1]) `assertEq!` 0

@test
def count03(): Bool = Array2.count(i -> i > 3, [4]) `assertEq!` 1

@test
def count04(): Bool = Array2.count(i -> i > 3, [1,2]) `assertEq!` 0

@test
def count05(): Bool = Array2.count(i -> i > 3, [1,8]) `assertEq!` 1

@test
def count06(): Bool = Array2.count(i -> i > 3, [8,1]) `assertEq!` 1

@test
def count07(): Bool = Array2.count(i -> i > 3, [6,7]) `assertEq!` 2

/////////////////////////////////////////////////////////////////////////////
// repeat                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def repeat01(): Bool = 
    let a = Array2.repeat(1, -1);
    Array.toList(a) `assertEq!` Nil

@test
def repeat02(): Bool = 
    let a = Array2.repeat(1, 0);
    Array.toList(a) `assertEq!` Nil

@test
def repeat03(): Bool = 
    let a = Array2.repeat(1, 1);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def repeat04(): Bool = 
    let a = Array2.repeat(1, 2);
    Array.toList(a) `assertEq!` 1 :: 1 :: Nil

@test
def repeat05(): Bool = 
    let a = Array2.repeat(1, 3);
    Array.toList(a) `assertEq!` 1 :: 1 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// reverse                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def reverse01(): Bool =  
    let a = Array2.reverse([]);
    Array.toList(a) `assertEq!` Nil

@test
def reverse02(): Bool =   
    let a = Array2.reverse([1]);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def reverse03(): Bool =   
    let a = Array2.reverse([1,2]);
    Array.toList(a) `assertEq!` 2 :: 1 :: Nil

@test
def reverse04(): Bool =   
    let a = Array2.reverse([1,1]);
    Array.toList(a) `assertEq!` 1 :: 1 :: Nil

@test
def reverse05(): Bool =   
    let a = Array2.reverse([1,2,3]);
    Array.toList(a) `assertEq!` 3 :: 2 :: 1 :: Nil

@test
def reverse06(): Bool =   
    let a = Array2.reverse([1,2,3,4]);
    Array.toList(a) `assertEq!` 4 :: 3 :: 2 :: 1 :: Nil

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

@test
def scan03(): Bool = 
    let a = Array2.scan((i, b) -> if (b) i+1 else i+2, 1, [true]);
    Array.toList(a) `assertEq!` 1 :: 2 :: Nil

@test
def scan04(): Bool = 
    let a = Array2.scan((i, b) -> if (b) i+1 else i+2, 1, [false,false]);
    Array.toList(a) `assertEq!` 1 :: 3 :: 5 :: Nil

@test
def scan05(): Bool = 
    let a = Array2.scan((i, b) -> if (b) i+1 else i+2, 1, [false,true]);
    Array.toList(a) `assertEq!` 1 :: 3 :: 4 :: Nil

@test
def scan06(): Bool = 
    let a = Array2.scan((i, b) -> if (b) i+1 else i+2, 1, [true,false]);
    Array.toList(a) `assertEq!` 1 :: 2 :: 4 :: Nil

@test
def scan07(): Bool = 
    let a = Array2.scan((i, b) -> if (b) i+1 else i+2, 1, [true,true]);
    Array.toList(a) `assertEq!` 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// scanLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def scanLeft01(): Bool = 
    let a = Array2.scanLeft((i, b) -> if (b) i+1 else i+2, 1, []);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def scanLeft02(): Bool = 
    let a = Array2.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [false]);
    Array.toList(a) `assertEq!` 1 :: 3 :: Nil

@test
def scanLeft03(): Bool = 
    let a = Array2.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [true]);
    Array.toList(a) `assertEq!` 1 :: 2 :: Nil

@test
def scanLeft04(): Bool = 
    let a = Array2.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [false,false]);
    Array.toList(a) `assertEq!` 1 :: 3 :: 5 :: Nil

@test
def scanLeft05(): Bool = 
    let a = Array2.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [false,true]);
    Array.toList(a) `assertEq!` 1 :: 3 :: 4 :: Nil

@test
def scanLeft06(): Bool = 
    let a = Array2.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [true,false]);
    Array.toList(a) `assertEq!` 1 :: 2 :: 4 :: Nil

@test
def scanLeft07(): Bool = 
    let a = Array2.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [true,true]);
    Array.toList(a) `assertEq!` 1 :: 2 :: 3 :: Nil

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

@test
def scanRight03(): Bool =  
    let a = Array2.scanRight((b, i) -> if (b) i+1 else i+2, 1, [true]);
    Array.toList(a) `assertEq!` 2 :: 1 :: Nil

@test
def scanRight04(): Bool =  
    let a = Array2.scanRight((b, i) -> if (b) i+1 else i+2, 1, [false,false]);
    Array.toList(a) `assertEq!` 5 :: 3 :: 1 :: Nil

@test
def scanRight05(): Bool =  
    let a = Array2.scanRight((b, i) -> if (b) i+1 else i+2, 1, [false,true]);
    Array.toList(a) `assertEq!` 4 :: 2 :: 1 :: Nil

@test
def scanRight06(): Bool =  
    let a = Array2.scanRight((b, i) -> if (b) i+1 else i+2, 1, [true,false]);
    Array.toList(a) `assertEq!` 4 :: 3 :: 1 :: Nil

@test
def scanRight07(): Bool =  
    let a = Array2.scanRight((b, i) -> if (b) i+1 else i+2, 1, [true,true]);
    Array.toList(a) `assertEq!` 3 :: 2 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// fold                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def fold01(): Bool = Array2.fold((i, e) -> (i - e)*(e % 2 + 1), 100, []) `assertEq!` 100

@test
def fold02(): Bool = Array2.fold((i, e) -> (i - e)*(e % 2 + 1), 100, [1]) `assertEq!` 198

@test
def fold03(): Bool = Array2.fold((i, e) -> (i - e)*(e % 2 + 1), 100, [1,2]) `assertEq!` 196

@test
def fold04(): Bool = Array2.fold((i, e) -> (i - e)*(e % 2 + 1), 100, [1,2,3]) `assertEq!` 386

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeft01(): Bool = Array2.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, []) `assertEq!` 100

@test
def foldLeft02(): Bool = Array2.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, [1]) `assertEq!` 198

@test
def foldLeft03(): Bool = Array2.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, [1,2]) `assertEq!` 196

@test
def foldLeft04(): Bool = Array2.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, [1,2,3]) `assertEq!` 386

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRight01(): Bool = Array2.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, []) `assertEq!` 100

@test
def foldRight02(): Bool = Array2.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, [1]) `assertEq!` 198

@test
def foldRight03(): Bool = Array2.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, [1,2]) `assertEq!` 194

@test
def foldRight04(): Bool = Array2.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, [1,2,3]) `assertEq!` 382

/////////////////////////////////////////////////////////////////////////////
// fold2                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def fold201(): Bool = Array2.fold2((c, a, b) -> if (b) a+c else a*c, 4, [], []) `assertEq!` 4

@test
def fold202(): Bool = Array2.fold2((c, a, b) -> if (b) a+c else a*c, 4, [1], []) `assertEq!` 4

@test
def fold203(): Bool = Array2.fold2((c, a, b) -> if (b) a+c else a*c, 4, [], [true]) `assertEq!` 4

@test
def fold204(): Bool = Array2.fold2((c, a, b) -> if (b) a+c else a*c, 4, [2], [true]) `assertEq!` 6

@test
def fold205(): Bool = Array2.fold2((c, a, b) -> if (b) a+c else a*c, 4, [2], [false]) `assertEq!` 8

@test
def fold206(): Bool = Array2.fold2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [true,true]) `assertEq!` 9

@test
def fold207(): Bool = Array2.fold2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [true,false]) `assertEq!` 14

@test
def fold208(): Bool = Array2.fold2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [false,true]) `assertEq!` 14

@test
def fold209(): Bool = Array2.fold2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [false,false]) `assertEq!` 24

@test
def fold210(): Bool = Array2.fold2((c, a, b) -> if (b) a+c else a*c, 4, [6,-4,3,2],
                    [true,false,false,true]) `assertEq!` -118

/////////////////////////////////////////////////////////////////////////////
// foldLeft2                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeft201(): Bool = Array2.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [], []) `assertEq!` 4

@test
def foldLeft202(): Bool = Array2.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [1], []) `assertEq!` 4

@test
def foldLeft203(): Bool = Array2.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [], [true]) `assertEq!` 4

@test
def foldLeft204(): Bool = Array2.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [2], [true]) `assertEq!` 6

@test
def foldLeft205(): Bool = Array2.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [2], [false]) `assertEq!` 8

@test
def foldLeft206(): Bool = Array2.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [true,true]) `assertEq!` 9

@test
def foldLeft207(): Bool = Array2.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [true,false]) `assertEq!` 14

@test
def foldLeft208(): Bool = Array2.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [false,true]) `assertEq!` 14

@test
def foldLeft209(): Bool = Array2.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [false,false]) `assertEq!` 24

@test
def foldLeft210(): Bool = Array2.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [6,-4,3,2],
                        [true,false,false,true]) `assertEq!` -118

/////////////////////////////////////////////////////////////////////////////
// foldRight2                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRight201(): Bool = Array2.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [], []) `assertEq!` 4

@test
def foldRight202(): Bool = Array2.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [1], []) `assertEq!` 4

@test
def foldRight203(): Bool = Array2.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [], [true]) `assertEq!` 4

@test
def foldRight204(): Bool = Array2.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [2], [true]) `assertEq!` 6

@test
def foldRight205(): Bool = Array2.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [2], [false]) `assertEq!` 8

@test
def foldRight206(): Bool = Array2.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [3,2], [true,true]) `assertEq!` 9

@test
def foldRight207(): Bool = Array2.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [3,2], [true,false]) `assertEq!` 11

@test
def foldRight208(): Bool = Array2.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [3,2], [false,true]) `assertEq!` 18

@test
def foldRight209(): Bool = Array2.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [3,2], [false,false]) `assertEq!` 24

@test
def foldRight210(): Bool = Array2.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [6,-4,3,2],
                         [true,false,false,true]) `assertEq!` -66

/////////////////////////////////////////////////////////////////////////////
// reduce                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def reduce01(): Bool = Array2.reduce((a, b) -> a-b, []: Array[Int]) `assertEq!` None

@test
def reduce02(): Bool = Array2.reduce((a, b) -> a-b, [1]) `assertEq!` Some(1)

@test
def reduce03(): Bool = Array2.reduce((a, b) -> a-b, [1,2]) `assertEq!` Some(-1)

@test
def reduce04(): Bool = Array2.reduce((a, b) -> a-b, [1,2,3]) `assertEq!` Some(-4)

@test
def reduce05(): Bool = Array2.reduce((a, b) -> a-b, [1,2,3,4]) `assertEq!` Some(-8)

/////////////////////////////////////////////////////////////////////////////
// reduceLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceLeft01(): Bool = Array2.reduceLeft((a, b) -> a-b, []: Array[Int]) `assertEq!` None

@test
def reduceLeft02(): Bool = Array2.reduceLeft((a, b) -> a-b, [1]) `assertEq!` Some(1)

@test
def reduceLeft03(): Bool = Array2.reduceLeft((a, b) -> a-b, [1,2]) `assertEq!` Some(-1)

@test
def reduceLeft04(): Bool = Array2.reduceLeft((a, b) -> a-b, [1,2,3]) `assertEq!` Some(-4)

@test
def reduceLeft05(): Bool = Array2.reduceLeft((a, b) -> a-b, [1,2,3,4]) `assertEq!` Some(-8)

/////////////////////////////////////////////////////////////////////////////
// reduceRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceRight01(): Bool = Array2.reduceRight((a, b) -> a-b, []: Array[Int]) `assertEq!` None

@test
def reduceRight02(): Bool = Array2.reduceRight((a, b) -> a-b, [1]) `assertEq!` Some(1)

@test
def reduceRight03(): Bool = Array2.reduceRight((a, b) -> a-b, [1,2]) `assertEq!` Some(-1)

@test
def reduceRight04(): Bool = Array2.reduceRight((a, b) -> a-b, [1,2,3]) `assertEq!` Some(2)

@test
def reduceRight05(): Bool = Array2.reduceRight((a, b) -> a-b, [1,2,3,4]) `assertEq!` Some(-2)

/////////////////////////////////////////////////////////////////////////////
// drop                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def drop01(): Bool = 
    let a = Array2.drop(-1, []: Array[Int32]);
    Array.toList(a) `assertEq!` Nil

@test
def drop02(): Bool = 
    let a = Array2.drop(0, []: Array[Int32]);
    Array.toList(a) `assertEq!` Nil

@test
def drop03(): Bool = 
    let a = Array2.drop(1, []: Array[Int32]);
    Array.toList(a) `assertEq!` Nil

@test
def drop04(): Bool = 
    let a = Array2.drop(-1, [1]);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def drop05(): Bool = 
    let a = Array2.drop(0, [1]);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def drop06(): Bool = 
    let a = Array2.drop(1, [1]);
    Array.toList(a) `assertEq!` Nil

@test
def drop07(): Bool = 
    let a = Array2.drop(2, [1]);
    Array.toList(a) `assertEq!` Nil

@test
def drop08(): Bool =
    let a = Array2.drop(0, [1,2]);
    Array.toList(a) `assertEq!` 1 :: 2 :: Nil

@test
def drop09(): Bool = 
    let a = Array2.drop(1, [1,2]);
    Array.toList(a) `assertEq!` 2 :: Nil

@test
def drop10(): Bool = 
    let a = Array2.drop(2, [1,2]);
    Array.toList(a) `assertEq!` Nil

@test
def drop11(): Bool = 
    let a = Array2.drop(2, [1,2,3,4,5,6]);
    Array.toList(a) `assertEq!` 3 :: 4 :: 5 :: 6 :: Nil

@test
def drop12(): Bool = 
    let a = Array2.drop(4, [1,2,3,4,5,6]);
    Array.toList(a) `assertEq!` 5 :: 6 :: Nil

/////////////////////////////////////////////////////////////////////////////
// dropWhile                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def dropWhile01(): Bool =  
    let a = Array2.dropWhile(i -> i > 3, []);
    Array.toList(a) `assertEq!` Nil

@test
def dropWhile02(): Bool =  
    let a = Array2.dropWhile(i -> i > 3, [1]);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def dropWhile03(): Bool =  
    let a = Array2.dropWhile(i -> i > 3, [4]);
    Array.toList(a) `assertEq!` Nil

@test
def dropWhile04(): Bool =  
    let a = Array2.dropWhile(i -> i > 3, [1,2]);
    Array.toList(a) `assertEq!` 1 :: 2 :: Nil

@test
def dropWhile05(): Bool =  
    let a = Array2.dropWhile(i -> i > 3, [1,5]);
    Array.toList(a) `assertEq!` 1 :: 5 :: Nil

@test
def dropWhile06(): Bool =  
    let a = Array2.dropWhile(i -> i > 3, [5,1]);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def dropWhile07(): Bool =  
    let a = Array2.dropWhile(i -> i > 3, [5,8]);
    Array.toList(a) `assertEq!` Nil

@test
def dropWhile08(): Bool =  
    let a = Array2.dropWhile(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
    Array.toList(a) `assertEq!` -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil

/////////////////////////////////////////////////////////////////////////////
// take                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def take01(): Bool = 
    let a = Array2.take(-1, []: Array[Int32]);
    Array.toList(a) `assertEq!` Nil

@test
def take02(): Bool =  
    let a = Array2.take(0, []: Array[Int32]);
    Array.toList(a) `assertEq!` Nil

@test
def take03(): Bool =  
    let a = Array2.take(1, []: Array[Int32]);
    Array.toList(a) `assertEq!` Nil

@test
def take04(): Bool = 
    let a = Array2.take(-1, [1]);
    Array.toList(a) `assertEq!` Nil

@test
def take05(): Bool = 
    let a = Array2.take(0, [1]);
    Array.toList(a) `assertEq!` Nil

@test
def take06(): Bool = 
    let a = Array2.take(1, [1]);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def take07(): Bool = 
    let a = Array2.take(2, [1]);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def take08(): Bool = 
    let a = Array2.take(0, [1,2]);
    Array.toList(a) `assertEq!` Nil

@test
def take09(): Bool = 
    let a = Array2.take(1, [1,2]);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def take10(): Bool = 
    let a = Array2.take(2, [1,2]);
    Array.toList(a) `assertEq!` 1 :: 2 :: Nil

@test
def take11(): Bool =  
    let a = Array2.take(2, [1,2,3,4,5,6]);
    Array.toList(a) `assertEq!` 1 :: 2 :: Nil

@test
def take12(): Bool = 
    let a = Array2.take(4, [1,2,3,4,5,6]);
    Array.toList(a) `assertEq!` 1 :: 2 :: 3 :: 4 :: Nil

/////////////////////////////////////////////////////////////////////////////
// takeWhile                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def takeWhile01(): Bool =  
    let a = Array2.takeWhile(i -> i > 3, []: Array[Int32]);
    Array.toList(a) `assertEq!` Nil

@test
def takeWhile02(): Bool =  
    let a = Array2.takeWhile(i -> i > 3, [1]);
    Array.toList(a) `assertEq!` Nil

@test
def takeWhile03(): Bool =  
    let a = Array2.takeWhile(i -> i > 3, [4]);
    Array.toList(a) `assertEq!` 4 :: Nil

@test
def takeWhile04(): Bool =  
    let a = Array2.takeWhile(i -> i > 3, [1,2]);
    Array.toList(a) `assertEq!` Nil

@test
def takeWhile05(): Bool =  
    let a = Array2.takeWhile(i -> i > 3, [1,5]);
    Array.toList(a) `assertEq!` Nil

@test
def takeWhile06(): Bool =  
    let a = Array2.takeWhile(i -> i > 3, [5,1]);
    Array.toList(a) `assertEq!` 5 :: Nil

@test
def takeWhile07(): Bool =  
    let a = Array2.takeWhile(i -> i > 3, [5,8]);
    Array.toList(a) `assertEq!` 5 :: 8 :: Nil

@test
def takeWhile08(): Bool =  
    let a = Array2.takeWhile(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
    Array.toList(a) `assertEq!` 4 :: 6 :: Nil

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def exists01(): Bool = assertNot!(Array2.exists(i -> i > 3, [] : Array[Int32]))

@test
def exists02(): Bool = assertNot!(Array2.exists(i -> i > 3, [1]))

@test
def exists03(): Bool = assert!(Array2.exists(i -> i > 3, [5]))

@test
def exists04(): Bool = assertNot!(Array2.exists(i -> i > 3, [1,2]))

@test
def exists05(): Bool = assert!(Array2.exists(i -> i > 3, [1,6]))

@test
def exists06(): Bool = assert!(Array2.exists(i -> i > 3, [6,1]))

@test
def exists07(): Bool = assert!(Array2.exists(i -> i > 3, [16,6]))

@test
def exists08(): Bool = assertNot!(Array2.exists(i -> i > 3, [1,-9,3]))

@test
def exists09(): Bool = assert!(Array2.exists(i -> i > 3, [1,9,3]))

/////////////////////////////////////////////////////////////////////////////
// forall                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def forall01(): Bool = assert!(Array2.forall(i -> i > 3, []: Array[Int32]))

@test
def forall02(): Bool = assertNot!(Array2.forall(i -> i > 3, [1]))

@test
def forall03(): Bool = assert!(Array2.forall(i -> i > 3, [5]))

@test
def forall04(): Bool = assertNot!(Array2.forall(i -> i > 3, [1,2]))

@test
def forall05(): Bool = assertNot!(Array2.forall(i -> i > 3, [1,6]))

@test
def forall06(): Bool = assertNot!(Array2.forall(i -> i > 3, [6,1]))

@test
def forall07(): Bool = assert!(Array2.forall(i -> i > 3, [16,6]))

@test
def forall08(): Bool = assertNot!(Array2.forall(i -> i > 3, [1,-9,3]))

@test
def forall09(): Bool = assertNot!(Array2.forall(i -> i > 3, [1,9,3]))

@test
def forall10(): Bool = assert!(Array2.forall(i -> i > 3, [11,9,31]))

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def memberOf01(): Bool = assertNot!(Array2.memberOf(0, []: Array[Int32]))

@test
def memberOf02(): Bool = assertNot!(Array2.memberOf(0, [1]))

@test
def memberOf03(): Bool = assert!(Array2.memberOf(0, [0]))

@test
def memberOf04(): Bool = assertNot!(Array2.memberOf(0, [1,2]))

@test
def memberOf05(): Bool = assert!(Array2.memberOf(1, [1,2]))

@test
def memberOf06(): Bool = assert!(Array2.memberOf(2, [1,2]))

@test
def memberOf07(): Bool = assertNot!(Array2.memberOf(3, [1,2]))

/////////////////////////////////////////////////////////////////////////////
// indexOf                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def indexOf01(): Bool = Array2.indexOf(0, []: Array[Int32]) `assertEq!` None

@test
def indexOf02(): Bool = Array2.indexOf(0, [1]) `assertEq!` None

@test
def indexOf03(): Bool = Array2.indexOf(1, [1]) `assertEq!` Some(0)

@test
def indexOf04(): Bool = Array2.indexOf(0, [1,2]) `assertEq!` None

@test
def indexOf05(): Bool = Array2.indexOf(1, [1,2]) `assertEq!` Some(0)

@test
def indexOf06(): Bool = Array2.indexOf(2, [1,2]) `assertEq!` Some(1)

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def find01(): Bool = Array2.find(i -> i > 2, []: Array[Int32]) `assertEq!` None

@test
def find02(): Bool = Array2.find(i -> i > 2, [1]) `assertEq!` None

@test
def find03(): Bool = Array2.find(i -> i > 2, [3]) `assertEq!` Some(3)

@test
def find04(): Bool = Array2.find(i -> i > 2, [1,2]) `assertEq!` None

@test
def find05(): Bool = Array2.find(i -> i > 2, [6,-6]) `assertEq!` Some(6)

@test
def find06(): Bool = Array2.find(i -> i > 2, [-6,6]) `assertEq!` Some(6)

@test
def find07(): Bool = Array2.find(i -> i > 2, [6,7]) `assertEq!` Some(6)

/////////////////////////////////////////////////////////////////////////////
// findLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def findLeft01(): Bool = Array2.findLeft(i -> i > 2, []) `assertEq!` None

@test
def findLeft02(): Bool = Array2.findLeft(i -> i > 2, [1]) `assertEq!` None

@test
def findLeft03(): Bool = Array2.findLeft(i -> i > 2, [3]) `assertEq!` Some(3)

@test
def findLeft04(): Bool = Array2.findLeft(i -> i > 2, [1,2]) `assertEq!` None

@test
def findLeft05(): Bool = Array2.findLeft(i -> i > 2, [6,-6]) `assertEq!` Some(6)

@test
def findLeft06(): Bool = Array2.findLeft(i -> i > 2, [-6,6]) `assertEq!` Some(6)

@test
def findLeft07(): Bool = Array2.findLeft(i -> i > 2, [6,7]) `assertEq!` Some(6)

/////////////////////////////////////////////////////////////////////////////
// findRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def findRight01(): Bool = Array2.findRight(i -> i > 2, []: Array[Int32]) `assertEq!` None

@test
def findRight02(): Bool = Array2.findRight(i -> i > 2, [1]) `assertEq!` None

@test
def findRight03(): Bool = Array2.findRight(i -> i > 2, [3]) `assertEq!` Some(3)

@test
def findRight04(): Bool = Array2.findRight(i -> i > 2, [1,2]) `assertEq!` None

@test
def findRight05(): Bool = Array2.findRight(i -> i > 2, [6,-6]) `assertEq!` Some(6)

@test
def findRight06(): Bool = Array2.findRight(i -> i > 2, [-6,6]) `assertEq!` Some(6)

@test
def findRight07(): Bool = Array2.findRight(i -> i > 2, [6,7]) `assertEq!` Some(7)


/////////////////////////////////////////////////////////////////////////////
// findIndexOf                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def findIndexOf01(): Bool = Array2.findIndexOf(i -> i > 2, []: Array[Int32]) `assertEq!` None

@test
def findIndexOf02(): Bool = Array2.findIndexOf(i -> i > 2, [1]) `assertEq!` None

@test
def findIndexOf03(): Bool = Array2.findIndexOf(i -> i > 2, [3]) `assertEq!` Some(0)

@test
def findIndexOf04(): Bool = Array2.findIndexOf(i -> i > 2, [1,2]) `assertEq!` None

@test
def findIndexOf05(): Bool = Array2.findIndexOf(i -> i > 2, [6,-6]) `assertEq!` Some(0)

@test
def findIndexOf06(): Bool = Array2.findIndexOf(i -> i > 2, [-6,6]) `assertEq!` Some(1)

@test
def findIndexOf07(): Bool = Array2.findIndexOf(i -> i > 2, [6,7]) `assertEq!` Some(0)

/////////////////////////////////////////////////////////////////////////////
// findIndexOfLeft                                                         //
/////////////////////////////////////////////////////////////////////////////
@test
def findIndexOfLeft01(): Bool = Array2.findIndexOfLeft(i -> i > 2, []: Array[Int32]) `assertEq!` None

@test
def findIndexOfLeft02(): Bool = Array2.findIndexOfLeft(i -> i > 2, [1]) `assertEq!` None

@test
def findIndexOfLeft03(): Bool = Array2.findIndexOfLeft(i -> i > 2, [3]) `assertEq!` Some(0)

@test
def findIndexOfLeft04(): Bool = Array2.findIndexOfLeft(i -> i > 2, [1,2]) `assertEq!` None

@test
def findIndexOfLeft05(): Bool = Array2.findIndexOfLeft(i -> i > 2, [6,-6]) `assertEq!` Some(0)

@test
def findIndexOfLeft06(): Bool = Array2.findIndexOfLeft(i -> i > 2, [-6,6]) `assertEq!` Some(1)

@test
def findIndexOfLeft07(): Bool = Array2.findIndexOfLeft(i -> i > 2, [6,7]) `assertEq!` Some(0)

/////////////////////////////////////////////////////////////////////////////
// findIndexOfRight                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def findIndexOfRight01(): Bool = Array2.findIndexOfRight(i -> i > 2, []: Array[Int32]) `assertEq!` None

@test
def findIndexOfRight02(): Bool = Array2.findIndexOfRight(i -> i > 2, [1]) `assertEq!` None

@test
def findIndexOfRight03(): Bool = Array2.findIndexOfRight(i -> i > 2, [3]) `assertEq!` Some(0)

@test
def findIndexOfRight04(): Bool = Array2.findIndexOfRight(i -> i > 2, [1,2]) `assertEq!` None

@test
def findIndexOfRight05(): Bool = Array2.findIndexOfRight(i -> i > 2, [6,-6]) `assertEq!` Some(0)

@test
def findIndexOfRight06(): Bool = Array2.findIndexOfRight(i -> i > 2, [-6,6]) `assertEq!` Some(1)

@test
def findIndexOfRight07(): Bool = Array2.findIndexOfRight(i -> i > 2, [6,7]) `assertEq!` Some(1)

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def filter01(): Bool = 
    let a = Array2.filter(i -> i > 3, []: Array[Int32]);
    Array.toList(a) `assertEq!` Nil

@test
def filter02(): Bool =  
    let a = Array2.filter(i -> i > 3, [2]);
    Array.toList(a) `assertEq!` Nil

@test
def filter03(): Bool = 
    let a = Array2.filter(i -> i > 3, [4]);
    Array.toList(a) `assertEq!` 4 :: Nil

@test
def filter04(): Bool = 
    let a = Array2.filter(i -> i > 3, [1,3]);
    Array.toList(a) `assertEq!` Nil

@test
def filter05(): Bool = 
    let a = Array2.filter(i -> i > 3, [1,8]);
    Array.toList(a) `assertEq!` 8 :: Nil

@test
def filter06(): Bool = 
    let a = Array2.filter(i -> i > 3, [8,1]);
    Array.toList(a) `assertEq!` 8 :: Nil

@test
def filter07(): Bool = 
    let a = Array2.filter(i -> i > 3, [8,9]);
    Array.toList(a) `assertEq!` 8 :: 9 :: Nil

@test
def filter08(): Bool = 
    let a = Array2.filter(i -> i > 3, [1,4,11,2,-22,17]);
    Array.toList(a) `assertEq!` 4 :: 11 :: 17 :: Nil

/////////////////////////////////////////////////////////////////////////////
// filterMap                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def filterMap01(): Bool =  
    let a = Array2.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, []: Array[Int32]);
    Array.toList(a) `assertEq!` Nil

@test
def filterMap02(): Bool =  
    let a = Array2.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [1]);
    Array.toList(a) `assertEq!` Nil

@test
def filterMap03(): Bool = 
    let a = Array2.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [2]);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def filterMap04(): Bool = 
    let a = Array2.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [1,3]);
    Array.toList(a) `assertEq!` Nil

@test
def filterMap05(): Bool = 
    let a = Array2.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [1,4]);
    Array.toList(a) `assertEq!` 2 :: Nil

@test
def filterMap06(): Bool = 
    let a = Array2.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [6,-1]);
    Array.toList(a) `assertEq!` 3 :: Nil

@test
def filterMap07(): Bool =  
    let a = Array2.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [8,6]);
    Array.toList(a) `assertEq!` 4 :: 3 :: Nil

@test
def filterMap08(): Bool =  
    let a = Array2.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [0,1,2,3,4,5,10]);
    Array.toList(a) `assertEq!` 0 :: 1 :: 2 :: 5 :: Nil

/////////////////////////////////////////////////////////////////////////////
// findMap                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def findMap01(): Bool = Array2.findMap(i -> if (i % 2 == 0) Some(i/2) else None, []: Array[Int32]) `assertEq!` None

@test
def findMap02(): Bool = Array2.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [1]) `assertEq!` None

@test
def findMap03(): Bool = Array2.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [2]) `assertEq!` Some(1)

@test
def findMap04(): Bool = Array2.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [1,3]) `assertEq!` None

@test
def findMap05(): Bool = Array2.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [1,4]) `assertEq!` Some(2)

@test
def findMap06(): Bool = Array2.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [6,-1]) `assertEq!` Some(3)

@test
def findMap07(): Bool = Array2.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [8,6]) `assertEq!` Some(4)

@test
def findMap08(): Bool = Array2.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [0,1,2,3,4,5,10]) `assertEq!` Some(0)

/////////////////////////////////////////////////////////////////////////////
// rotateLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def rotateLeft01(): Bool = 
    let a = Array2.rotateLeft(0, []: Array[Int32]);
    Array.toList(a) `assertEq!` Nil

@test
def rotateLeft02(): Bool =  
    let a = Array2.rotateLeft(1, []: Array[Int32]);
    Array.toList(a) `assertEq!` Nil

@test
def rotateLeft03(): Bool =  
    let a = Array2.rotateLeft(0, [1]);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def rotateLeft04(): Bool =  
    let a = Array2.rotateLeft(0, [1,2]);
    Array.toList(a) `assertEq!` 1 :: 2 :: Nil

@test
def rotateLeft05(): Bool =  
    let a = Array2.rotateLeft(1, [1,2]);
    Array.toList(a) `assertEq!` 2 :: 1 :: Nil

@test
def rotateLeft06(): Bool =  
    let a = Array2.rotateLeft(2, [1,2]);
    Array.toList(a) `assertEq!` 1 :: 2 :: Nil

@test
def rotateLeft07(): Bool =  
    let a = Array2.rotateLeft(3, [1,2]);
    Array.toList(a) `assertEq!` 2 :: 1 :: Nil

@test
def rotateLeft08(): Bool =  
    let a = Array2.rotateLeft(-1, [1,2]);
    Array.toList(a) `assertEq!` 2 :: 1 :: Nil

@test
def rotateLeft09(): Bool =  
    let a = Array2.rotateLeft(0, [1,2,3]);
    Array.toList(a) `assertEq!` 1 :: 2 :: 3 :: Nil

@test
def rotateLeft10(): Bool =  
    let a = Array2.rotateLeft(1, [1,2,3]);
    Array.toList(a) `assertEq!` 2 :: 3 :: 1 :: Nil

@test
def rotateLeft11(): Bool =  
    let a = Array2.rotateLeft(2, [1,2,3]);
    Array.toList(a) `assertEq!` 3 :: 1 :: 2 :: Nil

/////////////////////////////////////////////////////////////////////////////
// rotateRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def rotateRight01(): Bool = 
    let a = Array2.rotateRight(0, []: Array[Int32]);
    Array.toList(a) `assertEq!` Nil

@test
def rotateRight02(): Bool = 
    let a = Array2.rotateRight(1, []: Array[Int32]);
    Array.toList(a) `assertEq!` Nil

@test
def rotateRight03(): Bool = 
    let a = Array2.rotateRight(0, [1]);
    Array.toList(a) `assertEq!` 1 :: Nil

@test
def rotateRight04(): Bool = 
    let a = Array2.rotateRight(0, [1,2]);
    Array.toList(a) `assertEq!` 1 :: 2 :: Nil

@test
def rotateRight05(): Bool = 
    let a = Array2.rotateRight(1, [1,2]);
    Array.toList(a) `assertEq!` 2 :: 1 :: Nil

@test
def rotateRight06(): Bool = 
    let a = Array2.rotateRight(2, [1,2]);
    Array.toList(a) `assertEq!` 1 :: 2 :: Nil

@test
def rotateRight07(): Bool = 
    let a = Array2.rotateRight(3, [1,2]);
    Array.toList(a) `assertEq!` 2 :: 1 :: Nil

@test
def rotateRight08(): Bool = 
    let a = Array2.rotateRight(-1, [1,2]);
    Array.toList(a) `assertEq!` 2 :: 1 :: Nil

@test
def rotateRight09(): Bool = 
    let a = Array2.rotateRight(0, [1,2,3]);
    Array.toList(a) `assertEq!` 1 :: 2 :: 3 :: Nil

@test
def rotateRight10(): Bool = 
    let a = Array2.rotateRight(1, [1,2,3]);
    Array.toList(a) `assertEq!` 3 :: 1 :: 2 :: Nil

@test
def rotateRight11(): Bool = 
    let a = Array2.rotateRight(2, [1,2,3]);
    Array.toList(a) `assertEq!` 2 :: 3 :: 1 :: Nil


// ________
// Other...
// --------



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
    let a: Array[Array[Int32]] = Array2.transpose([[]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!` Nil :: Nil

@test
def transpose03(): Bool = 
    let a: Array[Array[Int32]] = Array2.transpose([[], []]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b `assertEq!` Nil :: Nil :: Nil

@test
def transpose04(): Bool = 
    let a: Array[Array[Int32]] = Array2.transpose([[], [], []]);
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

