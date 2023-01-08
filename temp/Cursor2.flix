
def main(): Int32 \ IO = region rh {
    println("Cursor2...");
    Cursor.ofList(rh, 100 :: 200 :: Nil) |> Cursor.forEach(println);
    Cursor.range(rh, 0, 1000) |> Cursor.filter(rh, x -> x < 15) |> Cursor.forEach(println);
    println("Cursor");
    let iter1 = () -> Cursor.ofList(rh, 1 :: 2 :: 3 :: Nil);
    let iter2 = () -> Cursor.ofList(rh, 1 :: 2 :: 3 :: Nil);
    let iter3 = Cursor.flatMap(rh, x -> Cursor.flatMap(rh, y -> Cursor.singleton(rh, (x, y)), iter2()), iter1());
    Cursor.forEach(println, iter3);
    println("List");
    let list1 = 1 :: 2 :: 3 :: Nil;
    let list2 = 1 :: 2 :: 3 :: Nil;
    let list3 = List.flatMap(x -> List.flatMap(y -> (x, y) :: Nil, list2), list1);
    List.forEach(println, list3);
    println("Cursor.append");
    Cursor.forEach(println, Cursor.append(rh, iter1(), iter2()));
    let iter4 = Cursor.flatMap(rh, x -> Cursor.flatMap(rh, y -> Cursor.singleton(rh, (x, y)), iter2()), iter1());
    println("First five...");
    Cursor.next(iter4) |> println;
    Cursor.next(iter4) |> println;
    Cursor.next(iter4) |> println;
    Cursor.next(iter4) |> println;
    Cursor.next(iter4) |> println;
    println("filterMap");
    Cursor.range(rh, 1, 10) |> Cursor.filterMap(rh, x -> if (x mod 2 == 0) Some("${x+100}i32") else None) |> Cursor.toList |> println;
    let filtList = () -> Cursor.range(rh, 1, 10) |> Cursor.filter(rh, x -> x mod 2 == 0);
    println("cons");
    filtList() |> Cursor.cons(rh, 100) |> Cursor.toList |> println;
    println("intersperse");
    filtList() |> Cursor.intersperse(rh, 100) |> Cursor.toList |> println;
    println("intercalate");
    filtList() |> Cursor.intercalate(rh, Cursor.range(rh, 1000, 1002)) |> Cursor.toList |> println;
    println("drop(3)");
    Cursor.range(rh, 1, 10) |> Cursor.drop(rh, 3) |> Cursor.toList |> println;
    println("dropWhile(<5)");
    Cursor.range(rh, 1, 10) |> Cursor.dropWhile(rh, i -> i < 5) |> Cursor.toList |> println;
    println("take(3)");
    Cursor.range(rh, 1, 10) |> Cursor.take(rh, 3) |> Cursor.toList |> println;    
    println("takeWhile(<5)");
    Cursor.range(rh, 1, 10) |> Cursor.takeWhile(rh, i -> i < 5) |> Cursor.toList |> println;
    println("... Cursor2");
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

    pub def map(r1: Region[r1], f: a -> b \ ef, iter: Cursor[a, r]): Cursor[b, r1] \ { ef, Read(r), Write(r1) } =
        let Cursor(_, iterF) = iter;
        let step = x -> match x {
            case Ans(a) => Ans(f(a))
            case Skip   => Skip
            case Done   => Done
        };
        let iterF1 = () -> unsafe_cast step(iterF()) as _ \ r1;
        unsafe_cast Cursor(r1, iterF1) as _ \ { ef, Read(r), Write(r1) }

    pub def ap(r3: Region[r3], mf: Cursor[a -> b \ ef, r1], ma: Cursor[a, r2]): Cursor[b, r3] \ { ef, Read(r1), Read(r2), Write(r3) } =
        let Cursor(_, iter1F) = mf;
        let Cursor(_, iter2F) = ma;
        let step = (x, y) -> match (x, y) {
            case (Ans(f), Ans(a)) => Ans(f(a))
            case (Ans(f), Skip)   => zipWithGoRight(a -> f(a), iter2F)
            case (Skip, Ans(a))   => zipWithGoLeft(iter1F, f -> f(a))
            case (Skip, Skip)     => Skip
            case (_, _)           => Done                        
        };
        let iter3F = () -> unsafe_cast step(iter1F(), iter2F()) as _ \ r3;
        unsafe_cast Cursor(r3, iter3F) as _ \ { ef, Read(r1), Read(r2), Write(r3) }

    pub def flatMap(r1: Region[r1], f: a -> Cursor[b, r1] \ ef, ma: Cursor[a, r]): Cursor[b, r1] \ { ef, Read(r), Write(r1) } =
        let Cursor(_, iterF) = ma;
        def loop(acc) = match iterF() {
            case Ans(a) => loop(append(r1, acc, f(a)))
            case Skip   => loop(acc)
            case Done   => acc
        };
        unsafe_cast loop(empty(r1)) as _ \ { ef, Read(r), Write(r1) }


    pub def append(r3: Region[r3], iter1: Cursor[a, r1], iter2: Cursor[a, r2]): Cursor[a, r3] \ { Read(r1), Read(r2), Write(r3) } =
        let Cursor(_, iter1F) = iter1;
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
        let iter3F = () -> unsafe_cast loop1(iter1F()) as _ \ r3;
        unsafe_cast Cursor(r3, iter3F)  as _ \ { Read(r1), Read(r2), Write(r3) }


    pub def filter(r1: Region[r1], f: a -> Bool \ ef, iter: Cursor[a, r]): Cursor[a, r1] \ { ef, Read(r), Write(r1) } =
        let Cursor(_, iterF) = iter;
        let step = x -> match x {
            case Ans(a) => if (f(a)) Ans(a) else Skip
            case Skip   => Skip
            case Done   => Done
        };
        let iterF1 = () -> unsafe_cast step(iterF()) as _ \ r1;
        unsafe_cast Cursor(r1, iterF1) as _ \ { ef, Read(r), Write(r1) }

    pub def filterMap(r1: Region[r1], f: a -> Option[b] \ ef, iter: Cursor[a, r]): Cursor[b, r1] \ { ef, Read(r), Write(r1) } =
        let Cursor(_, iterF) = iter;
        let step = x -> match x {
            case Ans(a) => match (f(a)) {
                case Some(a1) => Ans(a1)
                case None     => Skip
            } 
            case Skip   => Skip
            case Done   => Done
        };
        let iterF1 = () -> unsafe_cast step(iterF()) as _ \ r1;
        unsafe_cast Cursor(r1, iterF1) as _ \ { ef, Read(r), Write(r1) }


    pub def forEach(f: a -> Unit \ ef, iter: Cursor[a , r]): Unit \ { ef, Read(r) } =
        let Cursor(_, iterF) = iter;
        def loop() = match iterF() {
            case Skip   => loop()
            case Ans(a) => f(a); loop()
            case Done   => ()
        };
        loop()

    pub def zip(r3: Region[r3], iter1: Cursor[a, r1], iter2: Cursor[b, r2]): Cursor[(a, b), r3] \ { Read(r1), Read(r2), Write(r3) } = 
        zipWith(r3, (a, b) -> (a, b), iter1, iter2)

    pub def zipWith(r3: Region[r3], f: (a, b) -> c \ ef, iter1: Cursor[a, r1], iter2: Cursor[b, r2]): Cursor[c, r3] \ { ef, Read(r1), Read(r2), Write(r3) } =
        let Cursor(_, iter1F) = iter1;
        let Cursor(_,  iter2F) = iter2;
        let step = (l, r) -> match (l, r) {
            case (Ans(a), Ans(b)) => Ans(f(a, b))
            case (Ans(a), Skip)   => zipWithGoRight(b -> f(a, b), iter2F)
            case (Skip, Ans(b))   => zipWithGoLeft(iter1F, a -> f(a, b))
            case (Skip, Skip)     => Skip
            case (_, _)           => Done            
            
        };
        let iter3F = () -> unsafe_cast step(iter1F(), iter2F()) as _ \ r3;
        unsafe_cast Cursor(r3, iter3F) as _ \ { ef, Read(r1), Read(r2), Write(r3) }

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


    pub def zipWithIndex(r1: Region[r1], iter: Cursor[a, r]): Cursor[(Int32, a), r1] \ { Read(r), Write(r1) } = 
        let Cursor(r, _) = iter;
        let ix = ref 0 @ r;
        map(r1, x -> {let i = deref ix; ix:= i + 1; (i, x)}, iter)

    pub def enumerator(r1: Region[r1], iter: Cursor[a, r]): Cursor[(Int32, a), r1] \ { Read(r), Write(r1) } =
        zipWithIndex(r1, iter)

    pub def ofList(r: Region[r], xs: List[a]): Cursor[a, r] \ Write(r) = 
        let ls = ref xs @ r;
        let f = () -> { 
            match (deref ls) { 
                case Nil => Done
                case x :: rs => ls := rs; Ans(x)
            }
        };
        Cursor(r, f)        

    pub def cons(r1: Region[r1], x: a, iter: Cursor[a, r]): Cursor[a, r1] \ { Read(r), Write(r1) } = 
        let Cursor(_, iterF) = iter;
        let first = ref true @ r1;
        let f = () ->  { 
            if (deref first) {
                first := false;
                Ans(x)
            }
            else
                iterF()
        };
        let iter1F = () -> unsafe_cast f() as _ \ r1;
        unsafe_cast Cursor(r1, iter1F) as _ \ { Read(r), Write(r1) }
    
    def switchRegion(r1: Region[r1], iter: Cursor[a, r]): Cursor[a, r1] \ { Read(r), Write(r1) } = 
        let Cursor(_, iterF) = iter;
        let f = () -> iterF();
        let iter1F = () -> unsafe_cast f() as _ \ r1;
        unsafe_cast Cursor(r1, iter1F) as _ \ { Read(r), Write(r1) }

    
    pub def intersperse(r1: Region[r1], sep: a, iter: Cursor[a, r]): Cursor[a, r1] \ { Read(r), Write(r1) } =
        let start = ref true @ r1;
        let step = x -> 
            if (deref start) {
                start := false; 
                singleton(r1, x)
            } else { 
                cons(r1, sep, singleton(r1, x))
            };
        flatMap(r1, step, iter)

    /// Performance warning: `intercalate` must copy `sep` each time it is used.
    pub def intercalate(r1: Region[r1], sep: Cursor[a, r], iter: Cursor[a, r]): Cursor[a, r1] \ { Read(r), Write(r1) } =
        let start = ref true @ r1;
        let sepList = toList(sep);
        let step = x -> 
            if (deref start) {
                start := false; 
                singleton(r1, x)
            } else { 
                append(r1, ofList(r1, sepList), singleton(r1, x))
            };
        flatMap(r1, step, iter)
 

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


    pub def drop(r1: Region[r1], n: Int32, iter: Cursor[a, r]): Cursor[a, r1] \ { Read(r), Write(r1) } =
        let ix = ref n @ r1;
        def loop() = {
            let i = deref ix;
            ix := i - 1;
            if (i < 1)
                switchRegion(r1, iter)
            else { 
                discard next(iter);
                loop()
            }
        };
        loop()

    pub def dropWhile(r1: Region[r1], f: a -> Bool \ ef, iter: Cursor[a, r]): Cursor[a, r1] \ { ef, Read(r), Write(r1) } =
        def loop() = match next(iter) {
            case Some(x) => if (f(x)) loop() else cons(r1, x, iter)
            case None    => empty(r1)
        };
        loop()


    pub def take(r1: Region[r1], n: Int32, iter: Cursor[a, r]): Cursor[a, r1] \ { Read(r), Write(r1) } =
        let Cursor(_, iterF) = iter;
        let ix = ref n @ r1;
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
        let iter1F = () -> unsafe_cast loop() as _ \ r1;
        unsafe_cast Cursor(r1, iter1F) as _ \ { Read(r), Write(r1) }


    pub def takeWhile(r1: Region[r1], f: a -> Bool \ ef, iter: Cursor[a, r]): Cursor[a, r1] \ { ef, Read(r), Write(r1) } =
        let Cursor(_, iterF) = iter;
        def loop(x) = match x {
            case Ans(a) => if (f(a)) Ans(a) else Done
            case Skip   => loop(iterF())
            case Done   => Done
        };
        let iter1F = () -> unsafe_cast loop(iterF()) as _ \ r1;
        unsafe_cast Cursor(r1, iter1F) as _ \ { ef, Read(r), Write(r1) }


    pub def toList(iter: Cursor[a, r]): List[a] \ Read(r) = 
        foldRight((a, acc) -> a :: acc, Nil, iter)


}
