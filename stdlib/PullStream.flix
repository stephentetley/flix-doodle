
// Use flix 0.10.0

pub opaque type Stream[a] = Unit -> (Unit -> Option[a])

// TODO - drop not working do we need a Skip constructor?

namespace PullStream { 

    pub def map(f: a -> b, s: Stream[a]): Stream[b] = 
        Stream(() -> 
            let Stream(body) = s;
            let sf = body();
            let next = () -> 
                match sf() {
                    case None => None
                    case Some(a) => Some(f(a))
                };
            next 
        )


    pub def take(n: Int32, s: Stream[a]): Stream[b] = 
        Stream(() -> 
            let x = ref 0;
            let Stream(body) = s;
            let sf = body();
            let next = () -> 
                if (deref x >= n)
                    None
                else {
                    match sf() {
                        case None => None
                        case Some(a) => {
                            x := deref x + 1;
                            Some(a)
                        }
                    }
                };
            next 
        )

    pub def drop(n: Int32, s: Stream[a]): Stream[b] = 
        Stream(() -> 
            let x = ref n;
            let Stream(body) = s;
            let sf = body();
            let next = () -> 
                if (deref x >= 0)
                    None
                else {
                    match sf() {
                        case None => None
                        case Some(a) => {
                            x := deref x - 1;
                            Some(a)
                        }
                    }
                };
            next 
        )

    def dropHelper(sf : Unit -> Option[a], n: Int32) : Option[a] = match sf() {
        case None => None
        case Some(a) => 
            if (n <= 0)
                Some(a)
            else            
                dropHelper(sf, n - 1)
    }


    pub def filter(f: a -> Bool, s: Stream[a]): Stream[a] = 
        Stream(() -> 
            let Stream(body) = s;
            let sf = body();
            let next = () -> filterHelper(f, sf);
            next 
        )

    def filterHelper(f: a -> Bool, sf : Unit -> Option[a]) : Option[a] = 
        match sf() {
            case None => None
            case Some(a) => 
                if (f(a)) 
                    Some(a)
                else 
                    filterHelper(f, sf)
        }

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
        let Stream(body) = s;
        toListHelper(body(), Nil) 

    def toListHelper(sf : Unit -> Option[a], ac: List[a]) : List[a] = 
        match sf() {
            case None => List.reverse(ac)
            case Some(a) => toListHelper(sf, a :: ac)
        }

    pub def ofList(xs: List[a]): Stream[a] = 
        Stream(() -> 
            let rxs = ref xs;
            let next = () -> 
                match (deref rxs) {
                    case Nil => None
                    case x :: rs => {
                        rxs := rs;
                        Some(x)
                    }
                };
            next 
        )

    pub def ofArray(arr: Array[a]): Stream[a] = 
        Stream(() -> 
            let x = ref 0;
            let next = () -> {
                let ix = deref x;
                if (ix >= Array.length(arr))
                    None
                else {
                    let p = Array.get(arr, ix);
                    x := ix + 1;
                    Some(p)
                }
            };
            next 
        )


    pub def foreach(f: a -> Unit, s: Stream[a]): Unit = 
        let Stream(body) = s;
        foreachHelper(f, body()) 

    def foreachHelper(f: a -> Unit, sf : Unit -> Option[a]): Unit = 
        match sf() {
            case None => ()
            case Some(a) => {
                f(a);
                foreachHelper(f, sf)
            }
        }
}