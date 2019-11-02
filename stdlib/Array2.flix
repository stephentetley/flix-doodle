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

    def mapHelper(b: Array[b], f: a-> b, ix: Int, a: a) : Int = 
        b[ix] = f(a);
        ix + 1


    pub def map(f: a -> b, a: Array[a]): Array[b] = 
        let len = Array.length(a);
        if (len > 0) 
            let x : b = f(a[0]);
            let b : Array[b] = Array.new(x, len);
            foldLeft(mapHelper(b, f), 0, a);
            b
        else
            []

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


}    