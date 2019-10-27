
namespace Array2 {

    ///
    /// Alias for `foldLeft`.
    ///
    pub def fold(f: (b, a) -> b, s: b, a: Array[a]): b = foldLeft(f, s, a)

    def foldLeftHelper(f: (b, a) -> b, s: b, a: Array[a], ix: Int): b = 
        if (ix >= Array.length(a))
            s
        else
            let s1 = f(s, a[ix]);
            foldLeftHelper(f, s1, a, ix + 1)

    pub def foldLeft(f: (b, a) -> b, s: b, a: Array[a]): b = 
        foldLeftHelper(f, s, a, 0)

    def foldRightHelper(f: (b, a) -> b, s: b, a: Array[a], ix: Int): b = 
        if (ix < 0)
            s
        else
            let s1 = f(s, a[ix]);
            foldRightHelper(f, s1, a, ix - 1)

    pub def foldRight(f: (b, a) -> b, s: b, a: Array[a]): b = 
        foldRightHelper(f, s, a, Array.length(a) - 1)

    pub def take(n: Int, a: Array[a]) : Array[a] = 
        Array.slice(a, 0, n)

    pub def drop(n: Int, a: Array[a]) : Array[a] = 
        let last = Array.length(a);
        Array.slice(a, n, last)

    def existsHelper(f: a -> Bool, a: Array[a], ix: Int): Bool = 
        if (ix >= Array.length(a))
            false
        else 
            if (f(a[ix]) == true) 
                true 
            else 
                existsHelper(f, a, ix+1)


    pub def exists(f: a -> Bool, a: Array[a]): Bool = 
        existsHelper(f, a, 0)

    def forallHelper(f: a -> Bool, a: Array[a], ix: Int): Bool = 
        if (ix >= Array.length(a))
            true
        else 
            if (f(a[ix]) == false) 
                false 
            else 
                forallHelper(f, a, ix+1)


    pub def forall(f: a -> Bool, a: Array[a]): Bool = 
        forallHelper(f, a, 0)

}    