
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