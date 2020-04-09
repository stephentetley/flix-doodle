/*
 * Copyright 2020 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
 
// Use flix 0.10.0

pub opaque type Stream[a] = Unit -> (Unit -> Step[a])

enum Step[a] {
    case Done,
    case Skip,
    case Yield(a)
}

namespace Stream { 

    pub def map(f: a -> b, s: Stream[a]): Stream[b] = 
        Stream(() -> 
            let Stream(body) = s;
            let sf = body();
            let next = () -> 
                match sf() {
                    case Yield(a) => Yield(f(a))
                    case x        => x                    
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
                    Done
                else {
                    match sf() {
                        case Yield(a) => {
                            x := deref x + 1;
                            Yield(a)
                        }
                        case ans => ans
                    }
                };
            next 
        )

    pub def drop(n: Int32, s: Stream[a]): Stream[b] = 
        Stream(() -> 
            let x = ref n;
            let Stream(body) = s;
            let sf = body();
            let next = () -> match sf() {
                case Yield(a) =>
                    if (deref x < 0)
                        Yield(a)
                    else {                
                        x := deref x - 1;
                        Skip
                    }
                case ans => ans
                };
            next 
        )

    pub def filter(f: a -> Bool, s: Stream[a]): Stream[a] = 
        Stream(() -> 
            let Stream(body) = s;
            let sf = body();
            let next = () -> 
                match sf() { 
                    case Done => Done
                    case Skip => Skip
                    case Yield(a) => 
                        if (f(a) == false) 
                            Skip
                        else
                            Yield(a)    
                };
            next 
        )

    
    pub def range(b: Int32, e: Int32): Stream[Int32] = 
        Stream(() -> 
            let x = ref b;
            let next = () -> 
                if (deref x > e)
                    Done
                else {
                    let p = deref x;
                    x := deref x + 1;
                    Yield(p)
                };
            next 
        )

    pub def toList(s: Stream[a]): List[a] = 
        let Stream(body) = s;
        toListHelper(body(), Nil) 

    def toListHelper(sf : Unit -> Step[a], ac: List[a]) : List[a] = 
        match sf() {
            case Done     => List.reverse(ac)
            case Skip     => toListHelper(sf, ac)
            case Yield(a) => toListHelper(sf, a :: ac)
        }

    pub def ofList(xs: List[a]): Stream[a] = 
        Stream(() -> 
            let rxs = ref xs;
            let next = () -> 
                match (deref rxs) {
                    case Nil => Done
                    case x :: rs => {
                        rxs := rs;
                        Yield(x)
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
                    Done
                else {
                    let p = Array.get(arr, ix);
                    x := ix + 1;
                    Yield(p)
                }
            };
            next 
        )


    pub def foreach(f: a -> Unit, s: Stream[a]): Unit = 
        let Stream(body) = s;
        foreachHelper(f, body()) 

    def foreachHelper(f: a -> Unit, sf : Unit -> Step[a]): Unit = 
        match sf() {
            case Done => ()
            case Skip => foreachHelper(f, sf)
            case Yield(a) => {
                f(a);
                foreachHelper(f, sf)
            }
        }

    
    pub def cons(x: a, s: Stream[a]) : Stream[a] = 
        Stream(() -> 
            let Stream(body) = s;
            let sf = body();
            let initial = ref true;
            let next = () -> 
                if (deref initial != true) 
                    sf()
                else {
                    initial := false;
                    Yield(x)
                };            
            next
        )

    pub def zip(s1: Stream[a], s2: Stream[b]): Stream[(a,b)] = 
        Stream(() -> 
            let Stream(body1) = s1;
            let Stream(body2) = s2;
            let sf1 = body1();
            let sf2 = body2();
            let next = () -> zipHelper(sf1, sf2);
            next 
        )

    def zipHelper(sf1: Unit -> Step[a], sf2: Unit -> Step[b]) : Step[(a,b)] = 
        match (sf1(), sf2()) {
            case (Yield(a), Yield(b)) => Yield((a,b))
            case (Yield(a), Skip)     => zipHelperLeft(a, sf2)
            case (Skip, Yield(b))     => zipHelperRight(sf1, b)
            case (_,_)                => Done
        }

    def zipHelperLeft(x: a, sf: Unit -> Step[b]) : Step[(a,b)] = 
        match sf() {
            case Yield(y)   => Yield((x,y))
            case Done       => Done
            case Skip       => zipHelperLeft(x, sf)
        }

    def zipHelperRight(sf: Unit -> Step[a], y: b) : Step[(a,b)] = 
        match sf() {
            case Yield(x)   => Yield((x,y))
            case Done       => Done
            case Skip       => zipHelperRight(sf, y)
        }
        
}