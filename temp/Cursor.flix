
def main(): Int32 \ IO = region rh {
    println("Running...");
    Cursor.ofList(rh, 100 :: 200 :: Nil) |> Cursor.forEach(println);
    Cursor.range(rh, 0, 1000) |> Cursor.filter(x -> x < 15) |> Cursor.forEach(println);
    println("Cursor");
    let iter1 = () -> Cursor.ofList(rh, 1 :: 2 :: 3 :: Nil);
    let iter2 = () -> Cursor.ofList(rh, 1 :: 2 :: 3 :: Nil);
    let iter3 = Cursor.flatMap(x -> Cursor.flatMap(y -> Cursor.singleton(rh, (x, y)), iter2()), iter1());
    Cursor.forEach(println, iter3);
    println("List");
    let list1 = 1 :: 2 :: 3 :: Nil;
    let list2 = 1 :: 2 :: 3 :: Nil;
    let list3 = List.flatMap(x -> List.flatMap(y -> (x, y) :: Nil, list2), list1);
    List.forEach(println, list3);
    println("Cursor.append");
    Cursor.forEach(println, Cursor.append(iter1(), iter2()));
    let iter4 = Cursor.flatMap(x -> Cursor.flatMap(y -> Cursor.singleton(rh, (x, y)), iter2()), iter1());
    println("First five...");
    Cursor.next(iter4) |> println;
    Cursor.next(iter4) |> println;
    Cursor.next(iter4) |> println;
    Cursor.next(iter4) |> println;
    Cursor.next(iter4) |> println;
    println("filterMap");
    Cursor.range(rh, 1, 10) |> Cursor.filterMap(x -> if (x mod 2 == 0) Some("${x+100}i32") else None) |> Cursor.toList |> println;
    let filtList = () -> Cursor.range(rh, 1, 10) |> Cursor.filter(x -> x mod 2 == 0);
    println("cons");
    filtList() |> Cursor.cons(100) |> Cursor.toList |> println;
    println("intersperse");
    filtList() |> Cursor.intersperse(100) |> Cursor.toList |> println;
    println("intercalate");
    filtList() |> Cursor.intercalate(Cursor.range(rh, 1000, 1002)) |> Cursor.toList |> println;
    println("drop(3)");
    Cursor.range(rh, 1, 10) |> Cursor.drop(3) |> Cursor.toList |> println;
    println("dropWhile(<5)");
    Cursor.range(rh, 1, 10) |> Cursor.dropWhile(i -> i < 5) |> Cursor.toList |> println;
    println("take(3)");
    Cursor.range(rh, 1, 10) |> Cursor.take(3) |> Cursor.toList |> println;    
    println("takeWhile(<5)");
    Cursor.range(rh, 1, 10) |> Cursor.takeWhile(i -> i < 5) |> Cursor.toList |> println;    
    0
}

namespace Cursor {

    pub enum Cursor[a: Type, r: Region] {
        case Cursor(Region[r], Unit -> Step[a] \ r)
    }

    enum Step[a] {
        case Ans(a)
        case Skip
        case Done
    }

    pub def new(r: Region[r], f: Unit -> Option[a] \ r): Cursor[a, r] = 
        let f1 = () -> match f() {
            case Some(a) => Ans(a)
            case None    => Done
        };
        Cursor(r, f1)


    pub def empty(r: Region[r]): Cursor[a, r] = 
        let f = () -> unsafe_cast Done as _ \ r;
        Cursor(r, f)

    pub def singleton(r: Region[r], x: a): Cursor[a, r] \ Write(r) = repeat(r, 1, x)

    pub def repeat(r: Region[r], n: Int32, x: a): Cursor[a, r] \ Write(r) = 
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
        Cursor(r, f)


    pub def range(r: Region[r], b: Int32, e: Int32): Cursor[Int32, r] \ Write(r) = 
        let pos = ref b @ r;
        if (e <= b)
            empty(r)
        else {
            let f1 = () -> match (deref pos) {
                case i if i < e => pos := i+1; Ans(i)
                case _          => Done
            };
            Cursor(r, f1)
        }
        
    pub def next(iter: Cursor[a, r]): Option[a] \ Read(r) = 
        let Cursor(_, iterF) = iter;
        def loop() = match iterF() {
            case Ans(a) => Some(a)
            case Skip   => loop()
            case Done   => None
        };
        loop()

    pub def map(f: a -> b \ ef, iter: Cursor[a, r]): Cursor[b, r] \ { Read(r), ef } =
        let Cursor(r, iterF) = iter;
        let step = x -> match x {
            case Ans(a) => Ans(f(a))
            case Skip   => Skip
            case Done   => Done
        };
        let iterF1 = () -> unsafe_cast step(iterF()) as _ \ r;
        unsafe_cast Cursor(r, iterF1) as _ \ { Read(r), ef }

    pub def ap(mf: Cursor[a -> b \ ef, r], ma: Cursor[a, r]): Cursor[b, r] \ { Read(r), ef } =
        let Cursor(r, iter1F) = mf;
        let Cursor(_, iter2F) = ma;
        let step = (x, y) -> match (x, y) {
            case (Ans(f), Ans(a)) => Ans(f(a))
            case (Ans(f), Skip)   => zipWithGoRight(a -> f(a), iter2F)
            case (Skip, Ans(a))   => zipWithGoLeft(iter1F, f -> f(a))
            case (Skip, Skip)     => Skip
            case (_, _)           => Done                        
        };
        let iter3F = () -> unsafe_cast step(iter1F(), iter2F()) as _ \ r;
        unsafe_cast Cursor(r, iter3F) as _ \ { Read(r), ef }

    pub def flatMap(f: a -> Cursor[b, r] \ ef, ma: Cursor[a, r]): Cursor[b, r] \ ef =
        let Cursor(r, iterF) = ma;
        def loop(acc) = match iterF() {
            case Ans(a) => loop(append(acc, f(a)))
            case Skip   => loop(acc)
            case Done   => acc
        };
        unsafe_cast loop(empty(r)) as _ \ ef


    pub def append(iter1: Cursor[a, r], iter2: Cursor[a, r]): Cursor[a, r] = 
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
        let iter3F = () -> unsafe_cast loop1(iter1F()) as _ \ r;
        Cursor(r, iter3F)


    pub def filter(f: a -> Bool \ ef, iter: Cursor[a, r]): Cursor[a, r] \ { Read(r), ef } =
        let Cursor(r, iterF) = iter;
        let step = x -> match x {
            case Ans(a) => if (f(a)) Ans(a) else Skip
            case Skip   => Skip
            case Done   => Done
        };
        let iterF1 = () -> unsafe_cast step(iterF()) as _ \ r;
        unsafe_cast Cursor(r, iterF1) as _ \ { Read(r), ef }

    pub def filterMap(f: a -> Option[b] \ ef, iter: Cursor[a, r]): Cursor[b, r] \ { ef, Read(r) } =
        let Cursor(r, iterF) = iter;
        let step = x -> match x {
            case Ans(a) => match (f(a)) {
                case Some(a1) => Ans(a1)
                case None     => Skip
            } 
            case Skip   => Skip
            case Done   => Done
        };
        let iterF1 = () -> unsafe_cast step(iterF()) as _ \ r;
        unsafe_cast Cursor(r, iterF1) as _ \ { Read(r), ef }


    pub def forEach(f: a -> Unit \ ef, iter: Cursor[a , r]): Unit \ { ef, Read(r) } =
        let Cursor(_, iterF) = iter;
        def loop() = match iterF() {
            case Skip   => loop()
            case Ans(a) => f(a); loop()
            case Done   => ()
        };
        loop()

    pub def zip(iter1: Cursor[a, r], iter2: Cursor[b, r]): Cursor[(a, b), r] \ Read(r) = 
        zipWith((a, b) -> (a, b), iter1, iter2)

    pub def zipWith(f: (a, b) -> c \ ef, iter1: Cursor[a, r], iter2: Cursor[b, r]): Cursor[c, r] \ { ef, Read(r) } =
        let Cursor(r1, iter1F) = iter1;
        let Cursor(_,  iter2F) = iter2;
        let step = (l, r) -> match (l, r) {
            case (Ans(a), Ans(b)) => Ans(f(a, b))
            case (Ans(a), Skip)   => zipWithGoRight(b -> f(a, b), iter2F)
            case (Skip, Ans(b))   => zipWithGoLeft(iter1F, a -> f(a, b))
            case (Skip, Skip)     => Skip
            case (_, _)           => Done            
            
        };
        let iter3F = () -> unsafe_cast step(iter1F(), iter2F()) as _ \ r;
        unsafe_cast Cursor(r1, iter3F) as _ \ { ef, Read(r) }

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


    pub def zipWithIndex(iter: Cursor[a, r]): Cursor[(Int32, a), r] \ Write(r) = 
        let Cursor(r, _) = iter;
        let ix = ref 0 @ r;
        map(x -> {let i = deref ix; ix:= i + 1; (i, x)}, iter)

    pub def enumerator(iter: Cursor[a, r]): Cursor[(Int32, a), r] \ Write(r) = zipWithIndex(iter)

    pub def ofList(r: Region[r], xs: List[a]): Cursor[a, r] \ Write(r) = 
        let ls = ref xs @ r;
        let f = () -> { 
            match (deref ls) { 
                case Nil => Done
                case x :: rs => ls := rs; Ans(x)
            }
        };
        Cursor(r, f)        

    pub def cons(x: a, iter: Cursor[a, r]): Cursor[a, r] \ Write(r) = 
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
        let iter1F = () -> unsafe_cast f() as _ \ r;
        Cursor(r, iter1F)

    
    pub def intersperse(sep: a, iter: Cursor[a, r]): Cursor[a, r] \ Write(r) =
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
    pub def intercalate(sep: Cursor[a, r], iter: Cursor[a, r]): Cursor[a, r] \ Write(r) =
        let Cursor(r, _) = iter;
        let start = ref true @ r;
        let sepList = toList(sep);
        let step = x -> 
            if (deref start) {
                start := false; 
                singleton(r, x)
            } else { 
                append(ofList(r, sepList), singleton(r, x))
            };
        flatMap(step, iter)
 

    pub def foldLeft(f: (b, a) -> b \ ef, s: b, iter: Cursor[a, r]): b \ { ef, Read(r) } = 
        let Cursor(_, iterF) = iter;
        def loop(acc) = match iterF() {
            case Skip   => loop(acc)
            case Ans(a) => loop(f(acc, a))
            case Done   => acc
            
        };
        loop(s)

    pub def foldRight(f: (a, b) -> b \ ef, s: b, iter: Cursor[a, r]): b \ { ef, Read(r) } = 
        let Cursor(_, iterF) = iter;
        def loop(k) = match iterF() {
            case Skip   => loop(k)
            case Ans(a) => loop(ks -> k(f(a, ks)))
            case Done   => k(s)
            
        };
        loop(upcast(identity))

    pub def foldMap(f: a -> b \ ef, iter: Cursor[a, r]): b \ { ef, Read(r) } with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), iter)

    pub def reduceLeft(f: (a, a) -> a \ ef, iter: Cursor[a, r]): Option[a] \ { ef, Read(r) } =
        match next(iter) {
            case Some(x) => foldLeft(f, x, iter) |> Some
            case None    => None
        }


    pub def drop(n: Int32, iter: Cursor[a, r]): Cursor[a, r] \ Read(r) =
        let Cursor(r, _) = iter;
        let ix = ref n @ r;
        def loop() = {
            let i = deref ix;
            ix := i - 1;
            if (i < 1)
                iter
            else { 
                discard next(iter);
                loop()
            }
        };
        loop()

    pub def dropWhile(f: a -> Bool, iter: Cursor[a, r]): Cursor[a, r] \ { ef, Read(r) } =
        let Cursor(r, _) = iter;
        def loop() = match next(iter) {
            case Some(x) => if (f(x)) loop() else cons(x, iter)
            case None    => empty(r)
        };
        loop()


    pub def take(n: Int32, iter: Cursor[a, r]): Cursor[a, r] \ Read(r) =
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
        let iter1F = () -> unsafe_cast loop() as _ \ r;
        Cursor(r, iter1F)


    pub def takeWhile(f: a -> Bool \ ef, iter: Cursor[a, r]): Cursor[a, r] \ { ef, Read(r) } =
        let Cursor(r, iterF) = iter;
        def loop(x) = match x {
            case Ans(a) => if (f(a)) Ans(a) else Done
            case Skip   => loop(iterF())
            case Done   => Done
        };
        let iter1F = () -> unsafe_cast loop(iterF()) as _ \ r;
        unsafe_cast Cursor(r, iter1F) as _ \ { ef, Read(r) }


    pub def toList(iter: Cursor[a, r]): List[a] \ Read(r) = 
        foldRight((a, acc) -> a :: acc, Nil, iter)


}
