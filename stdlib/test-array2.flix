
// def sampleArray() : Array[Int] = Array.new(1, 10)


// def toArrayHelper (a: Array[b], i: Int, b: b) : Int = 
//     a[i] = b;
//     i + 1

// def toArray(xs: List[a]) : Array[a] = match List.head(xs) {
//     case None => []
//     case Some(x) => 
//         let a = Array.new(x, List.length(xs));
//         List.fold(toArrayHelper(a), 0, xs);
//         a
//     } 
    

@test
def map01() : Bool = 
    let a = Array.new(1, 10);
    let fn = _ -> 'a';
    assert!(Array2.forall(x -> x=='a', Array2.map(fn, a)))

@test
def map02() : Bool = 
    let a : Array[Int] = [];
    let fn = _ -> 'a';
    Array2.map(fn, a) |> Array.length `assertEq!` 0

@test
def foldLeft01() : Bool = 
    let a = Array.new(1, 10);
    let fn = x -> y -> x + y;
    Array2.foldLeft(fn, 0, a) `assertEq!` 10

@test
def fold01() : Bool = 
    let a = Array.new(1, 10);
    let fn = x -> y -> x + y;
    Array2.fold(fn, 0, a) `assertEq!` 10

@test
def foldRight01() : Bool = 
    let a = Array.new(1, 10);
    let fn = x -> y -> x + y;
    Array2.foldRight(fn, 0, a) `assertEq!` 10


@test
def take01() : Bool = 
    let a = Array.new(1, 10);
    Array.length(Array2.take(2, a)) `assertEq!` 2

@test
def drop01() : Bool = 
    let a = Array.new(1, 10);
    Array.length(Array2.drop(3, a)) `assertEq!` 7

@test
def exists01() : Bool = 
    let a = Array.new(1, 10);
    a[6] = 2;
    assert!(Array2.exists(x -> x > 1, a))

@test
def forall01() : Bool = 
    let a = Array.new(1, 10);
    assert!(Array2.forall(x -> x == 1, a))

@test
def memberOf01() : Bool = 
    // let a = toArray(1 :: 2 :: 3 :: Nil);
    let a : Array[Int] = [1, 2, 3];
    assert!(Array2.memberOf(2, a))

@test
def indexOf01() : Bool = 
    let a : Array[Int] = [0, 1, 2, 3];
    Array2.indexOf(2, a) `assertEq!` 2
