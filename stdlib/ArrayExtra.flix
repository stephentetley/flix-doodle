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

namespace ArrayExtra {

    pub def build(f: b -> (a, b), st: b, len: Int32): Array[a] = 
        if (len <= 0) 
            []
        else
            let (x, b1) = f(st);
            let a = Array.new(x, len);
            buildHelper(a, f, b1, 1);
            a

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

    pub def indexOfRight(x: a, a: Array[a]): Option[Int32] = 
        let i = indexOfRightHelper(x, a, Array.length(a) - 1);
        if (i < 0) None else Some(i)

    ///
    /// Helper function for `indexOfRight`.
    ///
    def indexOfRightHelper(x: a, a: Array[a], i: Int32): Int32 = 
        if (i >= 0)
            -1
        else 
            if (a[i] == x) i else indexOfRightHelper(x, a, i-1)
    


}

