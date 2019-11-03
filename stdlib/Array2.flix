/*
 * Copyright 2019 Stephen Tetley
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
 
namespace Array2 {

    pub def append(a: Array[a], b: Array[b]): Array[a] = 
        let len1 = Array.length(a); 
        let len2 = Array.length(b);
        if (len2 == 0)
            Array.slice(a, 0, len1)
        else
            if (len1 == 0)
                Array.slice(b, 0, len2) 
            else 
                let c = Array.new(b[0], len1 + len2);
                let j = appendHelper(a, c, 0, 0);
                let _ = appendHelper(b, c, 0, j);
                c
    
    def appendHelper(a: Array[a], c: Array[a], i: Int32, j: Int32): Int32 = 
        if (i >= Array.length(a))
            j
        else
            let _ = c[j] = a[i];
            appendHelper(a, c, i+1, j+1)

    pub def toString(a: Array[Char]) : Str = 
        Array2.foldLeft((acc, c) -> String.concat(acc, Char.toString(c)), "", a)


    pub def map(f: a -> b, a: Array[a]): Array[b] = 
        let len = Array.length(a);
        if (len > 0) 
            let x : b = f(a[0]);
            let b : Array[b] = Array.new(x, len);
            foldLeft(mapHelper(b, f), 0, a);
            b
        else
            []
    def mapHelper(b: Array[b], f: a -> b, i: Int, a: a) : Int = 
        b[i] = f(a);
        i + 1

    


    pub def map2(f: a -> b -> c, a: Array[a], b: Array[b]): Array[c] =
        let len = Int32.min(Array.length(a), Array.length(b));
        if (Array.length(a) > 0 && Array.length(b) > 0) 
            let x : c = f(a[0], b[0]);
            let c : Array[c] = Array.new(x, len);
            foldLeft2(map2Helper(c, f), 0, a);
            c
        else
            []

    def map2Helper(c: Array[c], f: a -> b -> c, i: Int, a: a, b: b) : Int = 
        c[i] = f(a, b);
        i + 1

    def mapWithIndexHelper(b: Array[b], f: a -> Int32 -> b, i: Int, a: a) : Int = 
        b[i] = f(a, i);
        i + 1


    pub def mapWithIndex(f: a -> Int32 -> b, a: Array[a]): Array[b] = 
        let len = Array.length(a);
        if (len > 0) 
            let x : b = f(a[0], 0);
            let b : Array[b] = Array.new(x, len);
            foldLeft(mapWithIndexHelper(b, f), 0, a);
            b
        else
            []


    pub def count(f: a -> Bool, a: Array[a]): Int32 = 
        foldLeft((b,x) -> if (f(x)) b + 1 else b, 0, a)


    ///
    /// Alias for `new`.
    ///
    pub def repeat(x: a, n: Int32): Array[a] = 
        Array.new(x, n)


    pub def reverse(a: Array[a]): Array[a] = 
        let len = Array.length(a);
        if (len > 0) 
            let b = Array.new(a[0], len);
            foldRight(reverseHelper(b), 0, a);
            b
        else
            []

    def reverseHelper(b: Array[a], i: Int32, a: a): Int32 = 
        b[i] = a;
        i + 1

    ///
    /// Alias for `foldLeft`.
    ///
    pub def fold(f: (b, a) -> b, s: b, a: Array[a]): b = foldLeft(f, s, a)

    def foldLeftHelper(f: (b, a) -> b, s: b, a: Array[a], i: Int): b = 
        if (i >= Array.length(a))
            s
        else
            let s1 = f(s, a[i]);
            foldLeftHelper(f, s1, a, i + 1)

    pub def foldLeft(f: b -> a -> b, s: b, a: Array[a]): b = 
        foldLeftHelper(f, s, a, 0)

    pub def foldLeft2(f: c -> a -> b -> c, s: c, a: Array[a], b: Array[b]): c = 
        foldLeft2Helper(f, s, a, b, 0)

    def foldLeft2Helper(f: c -> a -> b -> c , s: c, a: Array[a], b: Array[b], i: Int): c = 
        if (i >= Array.length(a) || i >= Array.length(b))
            s
        else
            let s1 = f(s, a[i], b[i]);
            foldLeft2Helper(f, s1, a, b, i + 1)



    def foldRightHelper(f: (b, a) -> b, s: b, a: Array[a], i: Int): b = 
        if (i < 0)
            s
        else
            let s1 = f(s, a[i]);
            foldRightHelper(f, s1, a, i - 1)

    pub def foldRight(f: (b, a) -> b, s: b, a: Array[a]): b = 
        foldRightHelper(f, s, a, Array.length(a) - 1)

    pub def foldRight2(f: c -> a -> b -> c, s: c, a: Array[a], b: Array[b]): c = 
        let len1 = Array.length(a);
        let len2 = Array.length(b);
        let i = Int32.min(len1, len2) - 1;
        foldRight2Helper(f, s, drop(len1-len2, a), drop(len2-len1, b), i)
        
        


    def foldRight2Helper(f: c -> a -> b -> c , s: c, a: Array[a], b: Array[b], i: Int): c = 
        if (i < 0)
            s
        else
            let s1 = f(s, a[i], b[i]);
            foldRight2Helper(f, s1, a, b, i - 1)

    def findIndexOf(f: a -> Bool, a: Array[a]): Int = 
        findIndexOfHelper(f, a, 0)

    def findIndexOfHelper(f: a -> Bool, a: Array[a], i: Int32): Int32 = 
        if (i > Array.length(a))
            -1
        else
            if (f(a[i])) i else findIndexOfHelper(f, a, i + 1)


    pub def take(n: Int, a: Array[a]) : Array[a] = 
        if (n < 1) 
            []
        else
            Array.slice(a, 0, n)

    pub def drop(n: Int, a: Array[a]) : Array[a] = 
        if (n < 1)
            Array.slice(a, 0, Array.length(a))
        else
            let last = Array.length(a);
            Array.slice(a, n, last)

    pub def takeWhile(f: a -> Bool, a: Array[a]): Array[a] = 
        let end = findIndexOf(x -> !(f(x)), a);
        take(end, a)

    

    pub def dropWhile(f: a -> Bool, a: Array[a]): Array[a] = 
        let start = findIndexOf(x -> !(f(x)), a);
        drop(start, a)

    



    def existsHelper(f: a -> Bool, a: Array[a], i: Int): Bool = 
        if (i >= Array.length(a))
            false
        else 
            if (f(a[i]) == true) 
                true 
            else 
                existsHelper(f, a, i+1)


    pub def exists(f: a -> Bool, a: Array[a]): Bool = 
        existsHelper(f, a, 0)

    def forallHelper(f: a -> Bool, a: Array[a], i: Int): Bool = 
        if (i >= Array.length(a))
            true
        else 
            if (f(a[i]) == false) 
                false 
            else 
                forallHelper(f, a, i+1)


    pub def forall(f: a -> Bool, a: Array[a]): Bool = 
        forallHelper(f, a, 0)

    pub def memberOf(x: a, a: Array[a]): Bool = 
        exists(y -> y == x, a)
    
    pub def indexOf(x: a, a: Array[a]): Int32 = indexOfHelper(x, a, 0)

    ///
    /// Helper function for `indexOf`.
    ///
    def indexOfHelper(x: a, a: Array[a], i: Int32): Int32 = 
        if (i >= Array.length(a))
            -1
        else 
            if (a[i] == x) i else indexOfHelper(x, a, i+1)

    pub def findLeft(f: a -> Bool, a: Array[a]): Option[a] = findLeftHelper(f, a, 0)

    def findLeftHelper(f: a -> Bool, a: Array[a], i: Int32): Option[a] = 
        if (i >= Array.length(a))
            None
        else 
            let x = a[i];
            if (f(x)) Some(x) else findLeftHelper(f, a, i+1)

    pub def findRight(f: a -> Bool, a: Array[a]): Option[a] = 
        findRightHelper(f, a, Array.length(a) - 1)

    def findRightHelper(f: a -> Bool, a: Array[a], i: Int32): Option[a] = 
        if (i < 0 || i >= Array.length(a))
            None
        else 
            let x = a[i];
            if (f(x)) Some(x) else findLeftHelper(f, a, i-1)


    def toArrayHelper (a: Array[b], i: Int, b: b) : Int = 
        a[i] = b;
        i + 1

    def toArray(xs: List[a]) : Array[a] = match List.head(xs) {
        case None => []
        case Some(x) => 
            let a = Array.new(x, List.length(xs));
            List.fold(toArrayHelper(a), 0, xs);
            a
        } 

    
    pub def filter(f: a -> Bool, a: Array[a]): Array[a] = 
        foldRight((xs,x) -> if (f(x)) x :: xs else xs, Nil, a) |> toArray

    pub def filterMap(f: a -> Option[b], a: Array[a]): Array[b] = 
        foldRight((xs,x) -> match f(x) { 
            case None => xs
            case Some(b) => b :: xs }, Nil, a) |> toArray

    pub def rotateLeft(n: Int32, a: Array[a]): Array[a] = 
        let len = Array.length(a);
        if (len > 0 )
            let b = Array.new(a[0], len);
            foldLeft(rotateHelper(b), len - n, a);
            b
        else
            []

        
    pub def rotateRight(n: Int32, a: Array[a]): Array[a] = 
        let len = Array.length(a);
        if (len > 0 )
            let b = Array.new(a[0], len);
            foldLeft(rotateHelper(b), n, a);
            b
        else
            []

    def rotateHelper(b: Array[a], i: Int32, x: a): Int32 = 
        let ix = if (i >= Array.length(b)) 0 else i;
        b[ix] = x;
        ix + 1

}

