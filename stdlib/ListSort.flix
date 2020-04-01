
namespace ListSort {

    pub def sortBy(cmp: (a,a) -> Int32, xs: List[a]): List[a] = 
        sequences(cmp, xs) |> mergeAll(cmp)


    def sequences(cmp: (a,a) -> Int32, xs: List[a]): List[List[a]] = match xs {
        case a :: b :: rs => 
            if (cmp(a, b) > 0)
                descending(cmp, b, a :: Nil, rs)
            else 
                ascending(cmp, b, vs -> a :: vs, rs)
        case rs => rs :: Nil
    }

    def descending(cmp: (a,a) -> Int32, x: a, xs: List[a], ys: List[a]): List[List[a]] = match ys {
        case r1 :: rs if (cmp(x, r1) > 0) =>
            descending(cmp, r1, x :: xs, rs)
        case rs =>
            (x :: xs) :: sequences(cmp, rs)
    }

    def ascending(cmp: (a,a) -> Int32, x: a, xsf: List[a] -> List[a], ys: List[a]): List[List[a]] = match ys {
        case r1 :: rs if (cmp(x, r1) <= 0)  =>
            ascending(cmp, r1, vs -> xsf(x :: vs), rs)
        case rs =>
            let x1 = xsf( x :: Nil);
            x1 :: sequences(cmp, rs)
        }


    def mergeAll(cmp: (a,a) -> Int32, xss: List[List[a]]) : List[a] = match xss {
        case x :: Nil => x
        case rs => mergeAll(cmp, mergePairs(cmp, rs))
    }

    def mergePairs(cmp: (a,a) -> Int32, xss: List[List[a]]) : List[List[a]] = match xss {
        case a :: b :: rs => {
            let x = merge(cmp, a, b);
            x :: mergePairs(cmp, rs)
        }
        case rs => rs
    }

    def merge(cmp: (a,a) -> Int32, xs: List[a], ys: List[a]) : List[a] = match (xs, ys) {
            case (r :: rs, s :: ss) => 
                if (cmp(r,s) > 0) 
                    s :: merge(cmp, xs, ss)
                else
                    r :: merge(cmp, rs, ys)
            case (Nil, ss) => ss
            case (rs, Nil) => rs
    }
}