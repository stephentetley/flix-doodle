// :load e:/coding/flix/flix-doodle/stdlib/quicksort.flix

def swapArr(a:Array[Int], i: Int32, j: Int32): Unit = 
    let x = a[i];
    let y = a[j];
    a[i] = y;
    a[j] = x;
    ()

pub def quicksort(a: Array[Int32]): Array[Int32] = 
    let len = Array.length(a);
    let b = Array.slice(a, 0, len);
    quicksortInPlace(b);
    b

pub def quicksortInPlace(a: Array[Int32]): Unit = 
    let len = Array.length(a);
    if (len == 0) () else quicksortHelper(a, 0, len-1)

def quicksortHelper(a: Array[Int32], lo: Int32, hi: Int32): Unit = 
    if (lo < hi) {
        let p = partition(a, lo, hi);
        quicksortHelper(a, lo, p-1);
        quicksortHelper(a, p+1, hi)
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

pub def partition(a: Array[Int32], lo: Int32, hi: Int32): Int32 = 
    let pivot = a[hi];    
    let i = forIx(lo, hi, lo, (j,ix) -> 
        if (a[j] < pivot) {
            swapArr(a, ix, j);
            ix+1
        } else ix);
    swapArr(a, i, hi);
    i


@test
def test01(): List[Int32] = 
    let arr = [];
    quicksortInPlace(arr);
    arr |> Array.toList

@test
def test02(): List[Int32] = 
    let arr = [2,1];
    quicksortInPlace(arr);
    arr |> Array.toList

@test
def test03(): List[Int32] = 
    let arr = [3,2,1];
    quicksortInPlace(arr);
    arr |> Array.toList

@test
def test04(): List[Int32] = 
    quicksort([3,2,4,1]) |> Array.toList

@test
def test05(): List[Int32] = 
    quicksort([3,2,4,5,1]) |> Array.toList

@test
def test06(): List[Int32] = 
    quicksort([3,1,3,2,4,5,1]) |> Array.toList


@test
def swapArr01(): List[Int32] = 
    let arr = [2,1];
    swapArr(arr,0,1);
    arr |> Array.toList