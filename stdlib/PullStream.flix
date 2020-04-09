
// Use flix 0.10.0

pub opaque type Stream[a] = Unit -> (Unit -> Option[a])


namespace PullStream { 

    pub def map(f: a -> b, s: Stream[a]): Stream[b] = 
        Stream(() -> 
            let Stream(sf) = s;
            let i = sf();
            let next = () -> 
                match i() {
                    case None => None
                    case Some(a) => Some(f(a))
                };
            next 
        )

    pub def range(b: Int32, e: Int32): Stream[Int32] = 
        Stream(() -> 
            let x = ref b;
            let next = () -> 
                if (deref x > e)
                    None
                else {
                    let p = deref x;
                    x := deref x + 1;
                    Some(p)
                };
            next 
        )

    pub def toList(s: Stream[a]): List[a] = 
        let Stream(sf) = s;
        toListHelper(sf(), Nil) 

    def toListHelper(sf : Unit -> Option[a], ac: List[a]) : List[a] = 
        match sf() {
            case None => List.reverse(ac)
            case Some(a) => toListHelper(sf, a :: ac)
        }


}