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
    
    ///
    /// Returns the hash code of the string `s`.
    ///
    pub def hashCode(s: Str): Int = native method java.lang.String.hashCode(s) as Int

    
    
    ///
    /// Returns the substring of string `s` between `i` and `j`.
    ///
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



    ///
    /// Take the first `n` characters of string `s`.
    ///
    /// If `n` extends past the end of string `s`, return all the characters 
    /// of `s`.
    ///
    pub def take(n: Int, s: Str) : Str = match substring(0, n, s) {
        case Ok(ans) => ans
        case Err(_) => s
    }

    ///
    /// Drop the first `n` characters of string `s`.
    ///
    /// If `n` extends past the end of string s, return the empty string.
    ///
    pub def drop(n: Int, s: Str) : Str = match substring1(n, s) {
        case Ok(ans) => ans
        case Err(_) => ""
    }
        


    ///
    /// Split the string `s` at the position `n` returning the left and 
    /// right parts.
    ///
    /// If `n` exceeds the length of string `s`, return the whole string 
    /// paired with the empty string.
    ///
    pub def splitAt(n: Int32, s: Str) : (Str, Str) = 
        (take(n,s), drop(n,s))

    ///
    /// Deconstruct the string `s` from the left.
    ///
    pub def viewl(s: Str): Option[(Char, Str)] = 
        if (String.length(s) == 0) None else Some(String.charAt(0, s), drop(1,s))

    ///
    /// Deconstruct the string `s` from the right.
    ///
    pub def viewr(s: Str): Option[(Str, Char)] = 
        let x = String.length(s);
        switch {
            case x > 1  => Some(take(x-1, s), String.charAt(x-1, s))
            case x == 1 => Some("", String.charAt(0, s))
            case x < 1  => None
        }
    
    ///
    /// Returns the tail of string `s` after dropping all the initial chars 
    /// that satisfy the predicate `f`.
    ///
    pub def dropWhile(f: Char -> Bool, s: Str): Str = match viewl(s) {
        case None => ""
        case Some(c, s1) => if (f(c)) dropWhile(f, s1) else s
    }


    def takeWhileHelper(f: Char -> Bool, s: Str): List[Char] = match viewl(s) {
        case None => Nil
        case Some(c, s1) => if (f(c)) (c :: takeWhileHelper(f, s1)) else Nil
    }

    ///
    /// Returns the initial prefix of string `s` where all the chars satisfy
    /// the predicate `f`.
    ///
    pub def takeWhile(f: Char -> Bool, s: Str): Str = 
        takeWhileHelper(f, s) |> List.toString


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
      
    ///
    /// Returns a string with the string `s` repeated `n` times.
    ///
    /// Returns the empty string if `n < 0`.
    ///
    pub def repeat(n: Int, s: Str): Str =
        if (n < 0) 
            ""
        else 
            native method java.lang.String.repeat(s, n) as Str
    
    ///
    /// Pad the string `s` at the left with the supplied char `c` to fit 
    /// width `w`.
    ///
    pub def padStart(w: Int, c: Char, s: Str): Str = {
        let x = String.length(s);
        if (x < w) 
            repeat(w - x, Char.toString(c)) + s
        else 
            s
    }

    ///
    /// Pad the string `s` at the right with the supplied char `c` to fit 
    /// width `w`.
    ///
    pub def padEnd(w: Int, c: Char, s: Str): Str = {
        let x = String.length(s);
        if (x < w) 
            s + repeat(w- x, Char.toString(c))
        else 
            s
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

    ///
    /// Split the string `s` into an array array of lines on the system 
    /// dependent line separator.
    ///
    pub def lines(s: Str): Array[Str] = 
        String.split(s, lineSeparator())

    def unlinesHelper(ac: Str, s: Str) : Str = 
        ac + lineSeparator() + s

    ///
    /// Join the array of strings `a` with the system dependent line 
    /// separator.
    ///
    pub def unlines(a: Array[Str]) : Str = 
        if (Array.length(a) > 0)
            let a1 = Array2.drop(1,a);
            Array2.foldLeft(unlinesHelper, a[0], a1)
        else ""

    pub def words(s: Str): Array[Str] = 
        String.split(s, "\s+")

    def unwordsHelper(ac: Str, s: Str) : Str = 
        ac + " " + s

    ///
    /// Join the array of strings `a` with a single space character.
    ///
    pub def unwords(a: Array[Str]) : Str = 
        if (Array.length(a) > 0)
            let a1 = Array2.drop(1,a);
            Array2.foldLeft(unwordsHelper, a[0], a1)
        else ""



}