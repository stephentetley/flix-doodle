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


    pub def toArray(s: Str): Array[Char] = 
        let a = Array.new('.', String.length(s));
        foldLeft(toArrayHelper(a), 0, s);
        a
    
    ///
    /// Helper function for `toArray`.
    ///
    def toArrayHelper(a: Array[Char], i: Int32, c: Char) : Int32 = 
        a[i] = c;
        i + 1

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

    pub def init(f: Int32 -> Char, len: Int32): Str = 
        build(i -> (f(i), i+1), 0, len)

   

    ///
    /// Returns the result of applying `f` to every character in `s`.
    ///
    pub def map(f: Char -> Char, s: Str): Str =
        let len = String.length(s);
        init(i -> f(String.charAt(i, s)), len)
        
    pub def mapWithIndex(f: (Char, Int32) -> Char, s: Str): Str =
        let len = String.length(s);
        init(i -> f(String.charAt(i, s), i), len)
      
    pub def map2(f: (Char, Char) -> Char, s: Str, t: Str): Str =
        let len = String.length(s);
        init(i -> f(String.charAt(i, s), String.charAt(i, t)), len)


    ///
    /// Returns `true` if the string `s` matches the `regex`.
    ///
    pub def matches(s: Str, regex: Str): Bool = native method java.lang.String.matches(s, regex) as Bool
    
    ///
    /// Returns the hash code of the string `s`.
    ///
    pub def hashCode(s: Str): Int = native method java.lang.String.hashCode(s) as Int

    
    
    ///
    /// Returns the substring of `s` from index `b` (inclusive) to index `e` (exclusive).
    ///
    /// If `b` or `e` are out-of-bounds, return the empty string.
    ///
    pub def slice(b: Int, e: Int, s: Str) : Str = try {
        native method java.lang.String.substring(s, b, e) as Str
    } catch { 
        case _: ##java.lang.IndexOutOfBoundsException => ""
    }
    
    ///
    /// Alias for `takeLeft`.
    ///
    pub def take(n: Int, s: Str) : Str = takeLeft(n, s)

    ///
    /// Take the first `n` characters of string `s` from the left.
    ///
    /// If `n` extends past the end of string `s`, return all the characters 
    /// of `s`.
    ///
    pub def takeLeft(n: Int, s: Str) : Str = slice(0, n, s) 

    pub def takeRight(n: Int, s: Str) : Str =
        let len = String.length(s);
        slice(len - n, len, s)

    ///
    /// Alias for `dropLeft`.
    ///
    pub def drop(n: Int, s: Str) : Str = dropLeft(n, s)

    ///
    /// Drop the first `n` characters of string `s` from the left.
    ///
    /// If `n` extends past the end of string s, return the empty string.
    ///
    pub def dropLeft(n: Int, s: Str) : Str =
        let len = String.length(s);
        slice(n, len, s)


    pub def dropRight(n: Int, s: Str) : Str =
        let len = String.length(s);
        slice(0, len - n, s)

    ///
    /// Returns the initial prefix of string `s` where all the chars satisfy
    /// the predicate `f`.
    ///
    pub def takeWhile(f: Char -> Bool, s: Str): Str = 
        takeWhileHelper(f, s) |> List.toString

    ///
    /// Helper function for `takeWhile` because we seem to need an intermediate list...
    ///   
    def takeWhileHelper(f: Char -> Bool, s: Str): List[Char] = match viewl(s) {
        case None => Nil
        case Some(c, s1) => if (f(c)) (c :: takeWhileHelper(f, s1)) else Nil
    }

    ///
    /// Returns the tail of string `s` after dropping all the initial chars 
    /// that satisfy the predicate `f`.
    ///
    pub def dropWhile(f: Char -> Bool, s: Str): Str = match viewl(s) {
        case None => ""
        case Some(c, s1) => if (f(c)) dropWhile(f, s1) else s
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
    /// Deconstruct the string `s` from the left returning the first char and the rest of the string.
    ///
    /// Return None if `s` is empty.
    ///
    pub def viewl(s: Str): Option[(Char, Str)] = 
        if (String.length(s) == 0) None else Some(String.charAt(0, s), drop(1,s))

    ///
    /// Deconstruct the string `s` from the right returning the first char and the rest of the string.
    ///
    /// Return None if `s` is empty.
    ///
    pub def viewr(s: Str): Option[(Str, Char)] = 
        let x = String.length(s);
        switch {
            case x > 1  => Some(take(x-1, s), String.charAt(x-1, s))
            case x == 1 => Some("", String.charAt(0, s))
            case x < 1  => None
        }


    pub def foldLeft(f: (b, Char) -> b, b: b, s: Str) : b = 
        match viewl(s) {
            case None => b
            case Some(c,s1) => 
                let acc = f(b, c);
                foldLeft(f, acc, s1)
        }

    pub def foldLeft2(f: (b, Char, Char) -> b, b: b, s: Str, t: Str) : b = 
        match (viewl(s), viewl(t)) {
            case (Some(x,s1), Some(y,t1)) => 
                let acc = f(b, x, y);
                foldLeft2(f, acc, s1, t1)
            case (_, _) => b            
        }



    pub def foldRight(f: (Char, b) -> b, b: b, s: Str) : b = 
        match viewr(s) {
            case None => b
            case Some(s1,c) => 
                let acc = f(c, b);
                foldRight(f, acc, s1)
        }

    pub def foldRight2(f: (Char, Char, b) -> b, b: b, s: Str, t: Str) : b = 
        match (viewr(s), viewr(t)) {
            case (Some(s1, x), Some(t1, y)) => 
                let acc = f(x, y, b);
                foldRight2(f, acc, s1, t1)
            case (_, _) => b
        }

    pub def unfold(f: b -> Option[(Char, b)], st: b) : Str = 
        unfoldHelper(f, st) |> List.toString

    
    def unfoldHelper(f: b -> Option[(Char, b)], st: b) : List[Char] = 
        match f(st) {
            case None => Nil
            case Some(c, st1) => c :: unfoldHelper(f, st1)
        }

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

    ///
    /// Returns `true` if and only if at least one char in `s` satisfies the predicate `f`.
    ///
    /// Returns `false` if `a` is empty.
    ///
    pub def exists(f: Char -> Bool, s: Str): Bool = match viewl(s) {
        case None => false
        case Some(c, s1) => if (f(c)) true else exists(f, s1)
    }

    ///
    /// Returns `true` if and only if all chars in `s` satisfy the predicate `f`.
    ///
    /// Returns `true` if `s` is empty.
    ///
    pub def forall(f: Char -> Bool, s: Str): Bool = match viewl(s) {
        case None => true
        case Some(c, s1) => if (!f(c)) false else forall(f, s1)
    }

    ///
    /// Returns `true` if and only if all chars in `s` are ascii characters.
    ///
    /// Returns `true` if `s` is empty.
    ///
    pub def isAscii(s: Str): Bool = 
        forall(Char.isAscii, s)

    ///
    /// Returns `true` if and only if all chars in `s` are white space characters.
    ///
    /// Returns `true` if `s` is empty.
    ///
    pub def isBlank(s: Str): Bool = 
        forall(Char.isWhiteSpace, s)

    /// Better (hopefully) implementation with viewl
    pub def trimLeft(s: Str): Str = match viewl(s) {
        case None => ""
        case Some(c, s1) => if (!(Char.isWhiteSpace(c))) s else trimLeft(s1)
    } 
        
    /// Better (hopefully) implementation with viewr
    pub def trimRight(s: Str): Str = match viewr(s) {
        case None => ""
        case Some(s1, c) => if (!(Char.isWhiteSpace(c))) s else trimRight(s1)
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
    /// Pad the string `s` at the left with the supplied char `c` to fit the width `w`.
    ///
    pub def padLeft(w: Int, c: Char, s: Str): Str = {
        let x = String.length(s);
        if (x < w) 
            repeat(w - x, Char.toString(c)) + s
        else s
    }

    ///
    /// Pad the string `s` at the right with the supplied char `c` to fit the width `w`.
    ///
    pub def padRight(w: Int, c: Char, s: Str): Str = {
        let x = String.length(s);
        if (x < w) 
            s + repeat(w- x, Char.toString(c))
        else s
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
    /// Split the string `s` into an array of lines on the system dependent line separator.
    ///
    pub def lines(s: Str): Array[Str] = 
        String.split(s, lineSeparator())

    ///
    /// Join the array of strings `a` with the system dependent line 
    /// separator.
    ///
    pub def unlines(a: Array[Str]): Str = 
        if (Array.length(a) > 0)
            let a1 = Array2.drop(1,a);
            Array2.foldLeft((ac,s) -> ac + lineSeparator() + s, a[0], a1)
        else ""

    ///
    /// Split the string `s` into an array of words, dividing on one or more white space characters.
    ///
    pub def words(s: Str): Array[Str] = 
        String.split(s, "\s+")

    ///
    /// Join the array of strings `a` with a single space character.
    ///
    pub def unwords(a: Array[Str]): Str = 
        if (Array.length(a) > 0)
            let a1 = Array2.drop(1,a);
            Array2.foldLeft((ac,s) -> ac + " " + s, a[0], a1)
        else ""

    ///
    /// Alias for `indexOfLeft`.
    ///
    pub def indexOf(s: Str, sub: Str): Option[Int] = indexOfLeft(s, sub)

    /// 
    /// Return the index of the first occurence of `sub` in `s` from the left.
    ///
    /// If `substring` is not present in `s` return None.
    ///
    pub def indexOfLeft(s: Str, sub: Str): Option[Int] =
        let i = native method java.lang.String.indexOf(s: Str, sub: Str) as Int;
        if (i < 0) None else Some(i)

    /// 
    /// Return the index of the first occurence of `sub` in `s` from the right.
    ///
    /// If `substring` is not present in `s` return None.
    ///
    pub def indexOfRight(s: Str, sub: Str): Option[Int] =
        let i = native method java.lang.String.lastIndexOf(s: Str, sub: Str) as Int;
        if (i < 0) None else Some(i)


    /// 
    /// Test if the string `s` contains `substring`.
    ///
    pub def contains(s: Str, substring: Str): Bool = 
        let a = indexOfLeft(s, substring);
        !Option.isEmpty(a)


    pub def reverse(s: Str) : Str = 
        let start = String.length(s) - 1;
        init(i -> String.charAt(start - i, s), start+1)

}