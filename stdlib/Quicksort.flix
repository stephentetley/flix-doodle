// :load e:/coding/flix/flix-doodle/stdlib/quicksort.flix

def swapArr(a:Array[Int], i: Int32, j: Int32): Unit = 
    let x = a[i];
    let y = a[j];
    a[i] = y;
    a[j] = x;
    ()

pub def quicksortBy(comparing: (a,a) -> Int32, a: Array[a]): Array[a] = 
    let len = Array.length(a);
    let b = Array.slice(a, 0, len);
    quicksortByInPlace(comparing, b);
    b

pub def quicksortByInPlace(comparing: (a,a) -> Int32, a: Array[a]): Unit = 
    let len = Array.length(a);
    if (len == 0) () else quicksortHelper(comparing, a, 0, len-1)

def quicksortHelper(comparing: (a,a) -> Int32, a: Array[a], lo: Int32, hi: Int32): Unit = 
    if (lo < hi) {
        let p = partition(comparing, a, lo, hi);
        quicksortHelper(comparing, a, lo, p-1);
        quicksortHelper(comparing, a, p+1, hi)
    } 
    else ()

/// precondition: lo <= hi
def forIx(lo: Int32, hi: Int32, ac: a, f: (Int32,a) -> a): a = 
    if (lo == hi) 
        f(lo,ac) 
    else { 
        let ac1 = f(lo, ac); 
        forIx(lo+1, hi, ac1, f) 
    }

pub def partition(comparing: (a,a) -> Int32, a: Array[a], lo: Int32, hi: Int32): Int32 = 
    let pivot = a[hi];    
    let i = forIx(lo, hi, lo, (j,ix) -> 
        if (comparing(a[j], pivot) < 0) {
            swapArr(a, ix, j);
            ix+1
        } else ix);
    swapArr(a, i, hi);
    i


def lessThan(a:Int32, b:Int32): Int32 = if (a < b) (-1) else 0


@test
def test01(): List[Int32] = 
    let arr = [];
    quicksortByInPlace(lessThan, arr);
    arr |> Array.toList

@test
def test02(): List[Int32] = 
    let arr = [2,1];
    quicksortByInPlace(lessThan, arr);
    arr |> Array.toList

@test
def test03(): List[Int32] = 
    let arr = [3,2,1];
    quicksortByInPlace(lessThan, arr);
    arr |> Array.toList

@test
def test04(): List[Int32] = 
    quicksortBy(lessThan, [3,2,4,1]) |> Array.toList

@test
def test05(): List[Int32] = 
    quicksortBy(lessThan, [3,2,4,5,1]) |> Array.toList

@test
def test06(): List[Int32] = 
    quicksortBy(lessThan, [3,1,3,2,4,5,1]) |> Array.toList


@test
def swapArr01(): List[Int32] = 
    let arr = [2,1];
    swapArr(arr,0,1);
    arr |> Array.toList
    