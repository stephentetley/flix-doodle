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
 
namespace String2 {

    ///
    /// Returns `true` if the string `s` matches the `regex`.
    ///
    pub def matches(s: Str, regex: Str): Bool = native method java.lang.String.matches(s, regex) as Bool

    pub def hashCode(s: Str): Int = native method java.lang.String.hashCode(s) as Int

    /// It would be quite nice to export this, but what do we do about 
    /// representing failure - Result, Option, empty-string?
    def substring(i: Int, j: Int, s: Str) : Result[Str,Str] = try {
        let a = native method java.lang.String.substring(s, i, j) as Str;
        Ok(a)
    } catch { 
        case _: ##java.lang.IndexOutOfBoundsException => Err("IndexOutOfBounds")
    }
    
    def substring1(i: Int, s: Str) : Result[Str,Str] = try {
        let a = native method java.lang.String.substring(s, i) as Str;
        Ok(a)
    } catch { 
        case _: ##java.lang.IndexOutOfBoundsException => Err("IndexOutOfBounds")
    }


    /// If n extends past the end of string s, return s anyway.
    pub def take(n: Int, s: Str) : Str = match substring(0, n, s) {
        case Ok(ans) => ans
        case Err(_) => s
    }

    /// If n extends past the end of string s, return s anyway.
    pub def drop(n: Int, s: Str) : Str = match substring1(n, s) {
        case Ok(ans) => ans
        case Err(_) => ""
    }
        
    pub def splitAt(n: Int32, s: Str) : (Str, Str) = 
        (take(n,s), drop(n,s))

    pub def viewl(s: Str): Option[(Char, Str)] = 
        if (String.length(s) == 0) None else Some(String.charAt(0, s), drop(1,s))

    pub def viewr(s: Str): Option[(Str, Char)] = 
        let x = String.length(s);
        switch {
            case x > 1  => Some(take(x-1, s), String.charAt(x-1, s))
            case x == 1 => Some("", String.charAt(0, s))
            case x < 1  => None
        }

    pub def exists(f: Char -> Bool, s: Str): Bool = match viewl(s) {
        case None => false
        case Some(c, s1) => if (f(c)) true else exists(f, s1)
    }

    pub def forall(f: Char -> Bool, s: Str): Bool = match viewl(s) {
        case None => true
        case Some(c, s1) => if (!f(c)) false else forall(f, s1)
    }

    pub def isAscii(s: Str): Bool = 
        forall(Char.isAscii, s)

    pub def isBlank(s: Str): Bool = 
        forall(Char.isWhiteSpace, s)

    /// Better (hopefully) implementation with viewl
    pub def trimStart(s: Str): Str = match viewl(s) {
        case None => ""
        case Some(c, s1) => if (!(Char.isWhiteSpace(c))) s else trimStart(s1)
    } 
        
    /// Better (hopefully) implementation with viewr
    pub def trimEnd(s: Str): Str = match viewr(s) {
        case None => ""
        case Some(s1, c) => if (!(Char.isWhiteSpace(c))) s else trimEnd(s1)
    } 
      
    pub def repeat(n: Int, s: Str): Str =
        native method java.lang.String.repeat(s, n) as Str
    

    pub def padStart(n: Int, s: Str): Str = {
        repeat(n, " ") + s
    }

    pub def padEnd(n: Int, s: Str): Str = {
        s + repeat(n, " ")
    }

    // pub def replace(oldChar: Char, newChar: Char, s: Str): Str =
    //     native method java.lang.String.replace(s: Str, oldChar: Char, newChar: Char) as Str

    pub def replaceAll(regex: Str, replacement: Str, s: Str): Str =
        native method java.lang.String.replaceAll(s, regex, replacement) as Str

    pub def replaceFirst(regex: Str, replacement: Str, s: Str): Str =
        native method java.lang.String.replaceFirst(s, regex, replacement) as Str

    pub def indent(n: Int, s: Str): Str = 
        native method java.lang.String.indent(s, n) as Str

    pub def lineSeparator() : Str = 
        native method java.lang.System.lineSeparator() as Str

    pub def lines(s: Str): Array[Str] = 
        String.split(s, lineSeparator())

    def unlinesHelper(ac: Str, s: Str) : Str = 
        ac + lineSeparator() + s

    pub def unlines(a: Array[Str]) : Str = 
        if (Array.length(a) > 0)
            let a1 = Array2.drop(1,a);
            Array2.foldLeft(unlinesHelper, a[0], a1)
        else ""

    pub def words(s: Str): Array[Str] = 
        String.split(s, "\s+")

    def unwordsHelper(ac: Str, s: Str) : Str = 
        ac + " " + s

    pub def unwords(a: Array[Str]) : Str = 
        if (Array.length(a) > 0)
            let a1 = Array2.drop(1,a);
            Array2.foldLeft(unwordsHelper, a[0], a1)
        else ""

}