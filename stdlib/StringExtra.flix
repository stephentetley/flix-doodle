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

    
}