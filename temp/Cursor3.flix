
def main(): Int32 \ IO = region rh {
    println("Cursor3...");
    Cursor.range(rh, 1, 10) |> Cursor.toList |> println;
    Cursor.range(rh, 1, 10) |> Cursor.dropWhile(x -> x < 5) |> Cursor.toList |> println;
    Cursor.range(rh, 1, 10) |> Cursor.drop(2) |> Cursor.toList |> println;
    println("flatMap");
    let iter1 = () -> Cursor.ofList(rh, 1 :: 2 :: 3 :: Nil);
    let iter2 = () -> Cursor.ofList(rh, 1 :: 2 :: 3 :: Nil);
    let iter3 = Cursor.flatMap(x -> Cursor.flatMap(y -> Cursor.singleton(rh, (x, y)), iter2()), iter1());
    iter3 |> Cursor.toList |> println;
    println("Nel...");
    Cursor.range(rh, 1, 10) |> Cursor.toNel |> println;
    let oneToFour = () -> Cursor.range(rh, 1, 4);
    let iterIter = () -> Cursor.ofList(rh, oneToFour() :: oneToFour() :: oneToFour() :: Nil);
    println("concat");
    Cursor.concat(iterIter()) |> Cursor.toList |> println;
    println("intercalate...");
    Cursor.intercalate(Cursor.range(rh, 1000, 1002), iterIter()) |> Cursor.toList |> println;
    0
}

namespace Cursor {

    pub enum Cursor[a: Type, ef: Bool, r: Region] {
        case Cursor(Region[r], Unit -> Step[a] \ {r, ef})
    }

    enum Step[a] {
        case Ans(a)
        case Skip
        case Done
    }

    

    /// TODO should return type be effect polymorphic?
    pub def new(r: Region[r], f: Unit -> Option[a] \ r): Cursor[a, ef, r] = 
        let f1 = () -> match f() {
            case Some(a) => Ans(a)
            case None    => Done
        };
        let iterF = () -> unsafe_cast f1()  as _ \ { ef, r };
        Cursor(r, iterF)

    /// TODO should return type be effect polymorphic?
    pub def empty(r: Region[r]): Cursor[a, ef, r] = 
        let f = () -> unsafe_cast Done as _ \ ef;
        Cursor(r, f)

    pub def singleton(r: Region[r], x: a): Cursor[a, ef, r] \ Write(r) = repeat(r, 1, x)

    pub def repeat(r: Region[r], n: Int32, x: a): Cursor[a, ef, r] \ Write(r) = 
        let ix = ref n @ r;
        let f = () -> { 
            let i = deref ix; 
            if (i < 1) 
                Done 
            else {
                ix := i-1; 
                Ans(x)
            }
        };
        let iterF = () -> unsafe_cast f() as _ \ { ef, r };
        Cursor(r, iterF)


    pub def range(r: Region[r], b: Int32, e: Int32): Cursor[Int32, ef, r] \ Write(r) = 
        let pos = ref b @ r;
        if (e <= b)
            empty(r)
        else {
            let f = () -> match (deref pos) {
                case i if i < e => pos := i+1; Ans(i)
                case _          => Done
            };
            let iterF = () -> unsafe_cast f() as _ \ { ef, r };
            Cursor(r, iterF)
        }
        
    pub def next(iter: Cursor[a, ef, r]): Option[a] \ {ef, Read(r) } = 
        let Cursor(_, iterF) = iter;
        def loop() = match iterF() {
            case Ans(a) => Some(a)
            case Skip   => loop()
            case Done   => None
        };
        loop()

    pub def map(f: a -> b \ ef1, iter: Cursor[a, ef, r]): Cursor[b, ef and ef1, r] =
        let Cursor(r, iterF) = iter;
        let step = x -> match x {
            case Ans(a) => Ans(f(a))
            case Skip   => Skip
            case Done   => Done
        };
        let iterF1 = () -> {let x = iterF(); step(x)};
        Cursor(r, iterF1)


    pub def ap(mf: Cursor[a -> b \ ef2, ef1, r], ma: Cursor[a, ef, r]): Cursor[b, ef and ef1 and ef2, r] =
        let Cursor(r, iter1F) = mf;
        let Cursor(_, iter2F) = ma;
        let step = (x, y) -> match (x, y) {
            case (Ans(f), Ans(a)) => Ans(f(a))
            case (Ans(f), Skip)   => zipWithGoRight(a -> f(a), iter2F)
            case (Skip, Ans(a))   => zipWithGoLeft(iter1F, f -> f(a))
            case (Skip, Skip)     => Skip
            case (_, _)           => Done                        
        };
        let iter3F = () -> step(iter1F(), iter2F());
        Cursor(r, iter3F)


    pub def flatMap(f: a -> Cursor[b, ef1, r] \ ef2, ma: Cursor[a, ef, r]): Cursor[b, ef and ef1 and ef2, r] \ Write(r) =
        let Cursor(r, iterF) = ma;
        let innerIter = ref empty(r) @ r;
        let inside = ref false @ r;
        def innerLoop() = {
            let Cursor(_, innerF) = deref innerIter;
            match innerF() {
                case Ans(b) => Ans(b)
                case Skip   => innerLoop()
                case Done   => Done
            }
        };
        def outerLoop() = {
            if (deref inside) {
                match innerLoop() { 
                    case Done => {inside := false; outerLoop()}
                    case ans  => ans
                }
            } else {
                match iterF() {
                    case Ans(a) => {inside := true; innerIter := f(a); innerLoop()}
                    case Skip   => outerLoop()
                    case Done   => Done
                }
            }
        };
        let iter1F = () -> outerLoop();
        Cursor(r, iter1F)



    pub def append(iter1: Cursor[a, ef1, r], iter2: Cursor[a, ef2, r]): Cursor[a, ef1 and ef2, r] = 
        let Cursor(r, iter1F) = iter1;
        let Cursor(_, iter2F) = iter2;
        def loop2(x) = match x {
            case Ans(a) => Ans(a)            
            case Skip   => loop2(iter2F())
            case Done   => Done
        };
        def loop1(x) = match x {
            case Ans(a) => Ans(a)
            case Skip   => loop1(iter1F())
            case Done   => loop2(iter2F())
        };
        let iter3F = () -> loop1(iter1F());
        Cursor(r, iter3F)

    pub def concat(iter: Cursor[Cursor[a, ef1, r], ef2, r]): Cursor[a, ef1 and ef2, r] \ Write(r) = 
        flatMap(identity, iter)


    pub def filter(f: a -> Bool \ ef1, iter: Cursor[a, ef, r]): Cursor[a, ef and ef1, r] =
        let Cursor(r, iterF) = iter;
        let step = x -> match x {
            case Ans(a) => if (f(a)) Ans(a) else Skip
            case Skip   => Skip
            case Done   => Done
        };
        let iterF1 = () -> step(iterF());
        Cursor(r, iterF1)

    pub def filterMap(f: a -> Option[b] \ ef1, iter: Cursor[a, ef, r]): Cursor[b, ef and ef1, r] =
        let Cursor(r, iterF) = iter;
        let step = x -> match x {
            case Ans(a) => match (f(a)) {
                case Some(a1) => Ans(a1)
                case None     => Skip
            } 
            case Skip   => Skip
            case Done   => Done
        };
        let iterF1 = () -> step(iterF());
        Cursor(r, iterF1)


    pub def forEach(f: a -> Unit \ ef1, iter: Cursor[a, ef, r]): Unit \ { ef, ef1, Read(r) } =
        let Cursor(_, iterF) = iter;
        def loop() = match iterF() {
            case Skip   => loop()
            case Ans(a) => f(a); loop()
            case Done   => ()
        };
        loop()

    pub def forEachWithIndex(f: (Int32, a) -> Unit \ ef1, iter: Cursor[a, ef, r]): Unit \ { ef, ef1, Read(r) } =
        let Cursor(_, iterF) = iter;
        def loop(i) = match iterF() {
            case Skip   => loop(i)
            case Ans(a) => f(i, a); loop(i+1)
            case Done   => ()
        };
        loop(0)


    pub def zip(iter1: Cursor[a, ef1, r], iter2: Cursor[b, ef, r]): Cursor[(a, b), ef and ef1, r] = 
        zipWith((a, b) -> (a, b), iter1, iter2)

    pub def zipWith(f: (a, b) -> c \ ef2, iter1: Cursor[a, ef1, r], iter2: Cursor[b, ef, r]): Cursor[c, ef and ef1 and ef2, r] =
        let Cursor(r1, iter1F) = iter1;
        let Cursor(_,  iter2F) = iter2;
        let step = (l, r) -> match (l, r) {
            case (Ans(a), Ans(b)) => Ans(f(a, b))
            case (Ans(a), Skip)   => zipWithGoRight(b -> f(a, b), iter2F)
            case (Skip, Ans(b))   => zipWithGoLeft(iter1F, a -> f(a, b))
            case (Skip, Skip)     => Skip
            case (_, _)           => Done            
            
        };
        let iter3F = () -> step(iter1F(), iter2F());
        Cursor(r1, iter3F)

    def zipWithGoLeft(iter: Unit -> Step[a] \ ef1, f: a -> c \ ef2): Step[c] \ { ef1, ef2 } = match iter() {
        case Ans(a) => Ans(f(a))
        case Skip   => zipWithGoLeft(iter, f)
        case Done   => Done
    }
    
    def zipWithGoRight(f: b -> c \ ef2, iter: Unit -> Step[b] \ ef1): Step[c] \ { ef1, ef2 } = match iter() {
        case Ans(b) => Ans(f(b))
        case Skip   => zipWithGoRight(f, iter)
        case Done   => Done
    }


    pub def zipWithIndex(iter: Cursor[a, ef, r]): Cursor[(Int32, a), ef and r, r] \ Write(r) = 
        let Cursor(r, _) = iter;
        let ix = ref 0 @ r;
        map(x -> {let i = deref ix; ix:= i + 1; (i, x)}, iter)

    pub def enumerator(iter: Cursor[a, ef, r]): Cursor[(Int32, a), ef and r, r] \ Write(r) = 
        zipWithIndex(iter)

    /// `liftRegion` instead of `unsafe_cast` ?
    pub def unfoldWithOk(r: Region[r], f: Unit -> Result[e, a] \ ef): Cursor[a, ef, r] =
        def loop() = match f() { 
            case Ok(a)  => Ans(a)
            case Err(_) => Done
        };
        let iterF = () -> unsafe_cast loop() as _ \ {ef, r};
        Cursor(r, iterF)


    pub def ofList(r: Region[r], xs: List[a]): Cursor[a, true, r] \ Write(r) = 
        let ls = ref xs @ r;
        let f = () -> { 
            match (deref ls) { 
                case Nil => Done
                case x :: rs => ls := rs; Ans(x)
            }
        };
        Cursor(r, f)


    pub def cons(x: a, iter: Cursor[a, ef, r]): Cursor[a, ef, r] \ Write(r) = 
        let Cursor(r, iterF) = iter;
        let first = ref true @ r;
        let f = () ->  { 
            if (deref first) {
                first := false;
                Ans(x)
            }
            else
                iterF()
        };
        let iter1F = () -> f();
        Cursor(r, iter1F)

    pub def replace(from: {from = a}, to: {to = a}, iter: Cursor[a, ef, r]): Cursor[a, ef, r] with Eq[a] =
        map(a -> if (a == from.from) to.to else a, iter)

    pub def intersperse(sep: a, iter: Cursor[a, ef, r]): Cursor[a, ef and r, r] \ Write(r) =
        let Cursor(r, _) = iter;
        let start = ref true @ r;
        let step = x -> 
            if (deref start) {
                start := false; 
                singleton(r, x)
            } else { 
                cons(sep, singleton(r, x))
            };
        flatMap(step, iter)


    

    /// Performance warning: `intercalate` must copy `sep` each time it is used.
    /// TODO - shouldn't iter be a Cursor of Cursors? e.g `iter: Cursor[Cursor[a, r], r]`
    pub def intercalate0(sep: Cursor[a, ef2, r], iter: Cursor[a, ef1, r]): Cursor[a, ef1 and ef2 and r, r] \ Write(r) =
        let Cursor(r, _) = iter;
        let start = ref true @ r;
        let sepList = ref Nil @ r;
        let step = x -> {
            // Cache `sepList` first time in the loop. Unfortunately we must test for Nil inside the loop [test each time]
            // to prevent `ef2` leaking to the top level of the function.
            match (deref sepList) {
                case Nil => sepList := toList(sep)
                case _   => ()
            };
            if (deref start) {
                start := false; 
                singleton(r, x)
            } else { 
                append(ofList(r, deref sepList), singleton(r, x))
            }
        };
        flatMap(step, iter)

    pub def intercalate(sep: Cursor[a, ef2, r], iter: Cursor[Cursor[a, ef1, r], ef1, r]): Cursor[a, ef1 and ef2 and r, r] \ Write(r) =
        let Cursor(r, _) = iter;
        let start = ref true @ r;
        let sepList = ref Nil @ r;
        let step = innerIter -> {
            // Cache `sepList` first time in the loop. Unfortunately we must test for Nil inside the loop [test each time]
            // to prevent `ef2` leaking to the top level of the function.
            match (deref sepList) {
                case Nil => sepList := toList(sep)
                case _   => ()
            };
            if (deref start) {
                start := false; 
                innerIter
            } else { 
                append(ofList(r, deref sepList), innerIter)
            }
        };
        flatMap(step, iter)


    pub def drop(n: Int32, iter: Cursor[a, ef, r]): Cursor[a, ef and r, r] \ Write(r) =
        let Cursor(r, iterF) = iter;
        let ix = ref n @ r;
        def loop() = {
            let i = deref ix;
            ix := i - 1;
            match iterF() {
                case Ans(a)  => if (i > 0) loop() else Ans(a)
                case Skip    => loop()
                case Done    => Done
            }
        };
        let iter1F = () -> loop();
        Cursor(r, iter1F)


    pub def dropWhile(f: a -> Bool \ ef1, iter: Cursor[a, ef, r]): Cursor[a, ef and ef1, r] =
        let Cursor(r, iterF) = iter;
        def loop() = match iterF() {
            case Ans(a)  => if (f(a)) loop() else Ans(a)
            case Skip    => loop()
            case Done    => Done
        };
        let iter1F = () -> loop();
        Cursor(r, iter1F)


    pub def take(n: Int32, iter: Cursor[a, ef and r, r]): Cursor[a, ef, r] \ Write(r) =
        let Cursor(r, iterF) = iter;
        let ix = ref n @ r;
        def loop() = {
            let i = deref ix;            
            if (i < 1)
                Done
            else
                match iterF() {
                    case Ans(a) => {ix := i-1; Ans(a)}
                    case Skip   => loop()
                    case Done   => Done
                }
        };
        let iter1F = () -> loop();
        Cursor(r, iter1F)


    pub def takeWhile(f: a -> Bool \ ef1, iter: Cursor[a, ef, r]): Cursor[a, ef and ef1, r]=
        let Cursor(r, iterF) = iter;
        def loop(x) = match x {
            case Ans(a) => if (f(a)) Ans(a) else Done
            case Skip   => loop(iterF())
            case Done   => Done
        };
        let iter1F = () -> loop(iterF());
        Cursor(r, iter1F)



    pub def foldLeft(f: (b, a) -> b \ ef1, s: b, iter: Cursor[a, ef, r]): b \ { ef, ef1, Read(r) } = 
        let Cursor(_, iterF) = iter;
        def loop(acc) = match iterF() {
            case Skip   => loop(acc)
            case Ans(a) => loop(f(acc, a))
            case Done   => acc
            
        };
        loop(s)

    pub def foldRight(f: (a, b) -> b \ ef1, s: b, iter: Cursor[a, ef, r]): b \ { ef, ef1, Read(r) } = 
        let Cursor(_, iterF) = iter;
        def loop(k) = match iterF() {
            case Skip   => loop(k)
            case Ans(a) => loop(ks -> k(f(a, ks)))
            case Done   => k(s)
            
        };
        loop(upcast(identity))

    pub def foldMap(f: a -> b \ ef1, iter: Cursor[a, ef, r]): b \ { ef, ef1, Read(r) } with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), iter)

    pub def reduceLeft(f: (a, a) -> a \ ef1, iter: Cursor[a, ef, r]): Option[a] \ { ef, ef1, Read(r) } =
        match next(iter) {
            case Some(x) => foldLeft(f, x, iter) |> Some
            case None    => None
        }

    pub def sum(iter: Cursor[Int32, ef, r]): Int32 \ { ef, Read(r) } = 
        foldLeft((acc, x) -> acc + x, 0, iter)

    pub def sumWith(f: a -> Int32 \ ef1, iter: Cursor[a, ef, r]): Int32 \ { ef, ef1, Read(r) } =
        foldLeft((acc, x) -> acc + f(x), 0, iter)

    pub def product(iter: Cursor[Int32, ef, r]): Int32 \ { ef, Read(r) } = match next(iter) {
        case Some(i) => foldLeft((acc, x) -> acc * x, i, iter)
        case None    => 1
    } 

    pub def productWith(f: a -> Int32 \ ef1, iter: Cursor[a, ef, r]): Int32 \ { ef, ef1, Read(r) } = match next(iter) {
        case Some(a) => foldLeft((acc, x) -> acc * f(x), f(a), iter)
        case None    => 1
    } 

    pub def toList(iter: Cursor[a, ef, r]): List[a] \ { ef, Read(r) } = 
        foldRight((a, acc) -> a :: acc, Nil, iter)

    pub def toArray(r1: Region[r1], iter: Cursor[a, ef, r2]): Array[a, r1] \ { ef, Read(r2), Write(r1) } = region r0 {
        let m = new MutList(r0);
        forEach(a -> MutList.push!(a, m), iter);
        MutList.toArray(r1, m)
    }



    pub def toSet(iter: Cursor[a, ef, r]): Set[a] \ { ef, Read(r) } with Order[a] = 
        foldLeft((acc, a) -> Set.insert(a, acc), Set.empty(), iter)

    pub def toMap(iter: Cursor[(k, v), ef, r]): Map[k, v] \ { ef, Read(r) } with Order[k] = 
        foldLeft((acc, a) -> {let (k, v) = a; Map.insert(k, v, acc)}, Map.empty(), iter)


    pub def toChain(iter: Cursor[a, ef, r]): Chain[a] \ { ef, Read(r) } = 
        foldLeft((acc, a) -> Chain.snoc(acc, a), Chain.empty(), iter)

    pub def toNec(iter: Cursor[a, ef, r]): Option[Nec[a]] \ { ef, Read(r) } = match next(iter) {
        case Some(a) => foldLeft((acc, x) -> Nec.snoc(acc, x), Nec.singleton(a), iter) |> Some
        case None    => None
    } 

    pub def toNel(iter: Cursor[a, ef, r]): Option[Nel[a]] \ { ef, Read(r) } = 
        let swapCons = (a, acc) -> match acc { 
            case Nel(x, xs) => Nel(x, a :: xs)
        };
        match next(iter) {
            case Some(a) => foldRight((x, acc) -> swapCons(x, acc), Nel.singleton(a), iter) |> Some
            case None    => None
        }

    pub def toMutDeque(r1: Region[r1], iter: Cursor[a, ef, r2]): MutDeque[a, r1] \ { ef, Read(r2), Write(r1) } =
        let d = new MutDeque(r1);
        forEach(x -> MutDeque.pushBack(x, d), iter);
        d
        
    pub def joinWith(f: a -> String \ ef1, sep: String, iter: Cursor[a, ef, r]): String \ { ef, ef1, Read(r) } = region rh {
        let sb = new StringBuilder(rh);
        match next(iter) {
            case Some(a) => {
                StringBuilder.append!(f(a), sb);
                forEach(x -> {StringBuilder.append!(sep, sb); StringBuilder.append!(f(x), sb)}, iter)
            }
            case None    => ()
        };
        StringBuilder.toString(sb)
    }

    pub def join(sep: String, iter: Cursor[a, ef, r]): String \ { ef, Read(r) } with ToString[a] =
        joinWith(ToString.toString, sep, iter)

}
