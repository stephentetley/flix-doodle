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

/// Useful functions that probably aren't "fundamental" enough for the stdlib

namespace StringExtra {


    pub def build(f: b -> (Char, b), st: b, len: Int32): Str = 
        if (len <= 0) 
            ""
        else
            let (x, b1) = f(st);
            let a = Array.new(x, len);
            buildHelper(a, f, b1, 1);
            Array2.toString(a)

    ///
    /// Helper function for `build`.
    /// 
    def buildHelper(a: Array[a], f: b -> (a, b), st: b, i: Int): Unit = 
        if (i >= Array.length(a))
            ()
        else
            let (x, st1) = f(st);
            a[i] = x;
            buildHelper(a, f, st1, i+1)

    pub def commonPrefix(s: Str, t: Str): Str = 
        let len = Int32.min(String.length(s), String.length(t));
        let f = i -> 
            if (i < len && String.charAt(i, s) == String.charAt(i, t))
                Some(String.charAt(i, s), i+1)
            else None;
        unfold(f, 0)

    pub def commonSuffix(s: Str, t: Str): Str = 
        commonSuffixHelper(s, t, Nil) |> List.toString

    def commonSuffixHelper(s: Str, t: Str, acc: List[Char]): List[Char] = 
        match (viewr(s), viewr(t)) {
            case (None, _) => acc 
            case (_, None) => acc
            case (Some(s1, x), Some(t1,y)) => 
                if (x == y) (commonSuffixHelper(s1, t1, x :: acc)) else acc
        }

    
}