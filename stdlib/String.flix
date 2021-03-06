/*
 * Copyright 2017 Magnus Madsen
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
namespace String {

    ///
    /// Returns the character at position `i` in the string `s`.
    ///
    pub def charAt(i: Int, s: String): Char =
        import java.lang.String.charAt(Int32);
        s.charAt(i)

    ///
    /// Returns the string `s1` followed by the string `s2`.
    ///
    pub def concat(s1: String, s2: String): String =
        import java.lang.String.concat(String);
        s1.concat(s2)

    ///
    /// Returns `true` if the string `s1` ends with the string `s2`.
    ///
    pub def endsWith(s1: String, s2: String): Bool =
        import java.lang.String.endsWith(String);
        s1.endsWith(s2)

    ///
    /// Returns `true` if the string `s` is the empty string.
    ///
    pub def isEmpty(s: String): Bool =
        import java.lang.String.isEmpty();
        s.isEmpty()

    ///
    /// Returns the length of the string `s`.
    ///
    pub def length(s: String): Int =
        import java.lang.String.length();
        s.length()

    ///
    /// Returns `true` if the string `s1` starts with the string `s2`.
    ///
    pub def startsWith(s1: String, s2: String): Bool =
        import java.lang.String.startsWith(String);
        s1.startsWith(s2)

    ///
    /// Splits the string `s` around matches of the regular expression `r`.
    ///
    pub def split(s: String, r: String): Array[String] =
        import java.lang.String.split(String);
        s.split(r)

    //
    // Returns the lower case version of the string `s`.
    //
    pub def toLowerCase(s: String): String =
        import java.lang.String.toLowerCase();
        s.toLowerCase()

    //
    // Returns the upper case version of the string `s`.
    //
    pub def toUpperCase(s: String): String =
        import java.lang.String.toUpperCase();
        s.toUpperCase()

    ///
    /// Returns the given string `s` as a list of characters.
    ///
    pub def toList(s: String): List[Char] = List.map(i -> charAt(i, s), List.range(0, length(s)))

    ///
    /// Returns a copy of the string `s` without trailing and leading whitespaces.
    ///
    /// Returns a new empty string if there is no characters in `s`.
    ///
    pub def trim(s: String): String =
        import java.lang.String.trim();
        s.trim()

    ///
    /// Returns the given string `s` as an array of characters.
    ///
    pub def toArray(s: String): Array[Char] =
        Array.init(i -> charAt(i, s), length(s))

    ///
    /// Build a string of length `len` by applying `f` to the successive indices.
    ///
    pub def init(f: Int32 -> Char, len: Int32): String =
        if (len <= 0)
            ""
        else {
            let c = f(0);
            let a = Array.new(c, len);
            initHelper(a, f, len, 1);
            Array.mkStringBy(Char.toString, "", a) // TODO - make more efficient
        }

    ///
    /// Helper function for `init`.
    ///
    def initHelper(a: Array[Char], f: Int32 -> Char, len: Int32, i: Int32): Unit =
        if (i >= len)
            ()
        else {
            a[i] = f(i);
            initHelper(a, f, len, i+1)
        }

    ///
    /// Concatenate a list of strings into a single string.
    ///
    pub def flatten(xs: List[String]): String =
        let sb = StringBuilder.new();
        flattenHelper(s -> StringBuilder.appendString(sb, s), xs);
        StringBuilder.toString(sb)

    ///
    /// Helper for `flatten`.
    /// Note this is a generally useful function usually called `iter` that could be added
    /// to List.
    ///
    def flattenHelper(f: a -> Unit, xs: List[a]): Unit = match xs with {
        case Nil => ()
        case x :: rs => {
            f(x);
            flattenHelper(f, rs)
        }
    }


    ///
    /// Returns the result of applying `f` to every character in `s`.
    ///
    pub def map(f: Char -> Char, s: String): String =
        let len = length(s);
        init(i -> f(charAt(i, s)), len)

    ///
    /// Returns the result of applying `f` to every character in `s` along with that character's index.
    ///
    pub def mapWithIndex(f: (Char, Int32) -> Char, s: String): String =
        let len = length(s);
        init(i -> f(charAt(i, s), i), len)

    ///
    /// Returns a string where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `s` and `b` is the element at index `i` in `t`.
    ///
    /// If either `s` or `t` becomes depleted, then no further elements are added to the resulting string.
    /// Functionally equivalent to zipWith.
    ///
    pub def map2(f: (Char, Char) -> Char, s: String, t: String): String =
        let len = Int32.min(String.length(s), String.length(t));
        init(i -> f(charAt(i, s), charAt(i, t)), len)


    ///
    /// Returns `true` if the string `s` matches the `regex`.
    ///
    pub def matches(s: String, regex: String): Bool =
        try {
            import java.lang.String.matches(String);
            s.matches(regex)
        } catch {
            case _: ##java.util.regex.PatternSyntaxException => false
        }

    ///
    /// Returns the hash code of the string `s`.
    ///
    pub def hashCode(s: String): Int32 =
        import java.lang.String.hashCode();
        s.hashCode()



    ///
    /// Returns the substring of `s` from index `b` (inclusive) to index `e` (exclusive).
    ///
    /// If `b` or `e` are out-of-bounds, return the empty string.
    ///
    pub def slice(b: Int32, e: Int32, s: String): String = try {
       import java.lang.String.substring(Int32, Int32);
       s.substring(b, e)
    } catch {
        case _: ##java.lang.IndexOutOfBoundsException => ""
    }

    ///
    /// Optionally returns the position of the first element in `x` satisfying `f`.
    ///
    /// If nothing satisfies `f` return None.
    ///
    pub def findIndexOfLeft(f: a -> Bool, s: String): Option[Int] =
        let i = findIndexOfLeftHelper(f, s, length(s), 0);
        if (i < 0) None else Some(i)

    ///
    /// Helper function for `findIndexOf`.
    ///
    def findIndexOfLeftHelper(f: a -> Bool, s: String, len: Int32, i: Int32): Int32 =
        if (i >= len)
            -1
        else
            if (f(charAt(i, s))) i else findIndexOfLeftHelper(f, s, len, i+1)

    ///
    /// Optionally return the position of the first character in `s` satisfying `f`, reading right to left.
    ///
    /// If nothing satisfies `f` return None.
    ///
    pub def findIndexOfRight(f: a -> Bool, s: String): Option[Int] =
        let end = length(s) - 1;
        let i = findIndexOfRightHelper(f, s, end);
        if (i < 0) None else Some(i)

    ///
    /// Helper function for `findIndexOf`.
    ///
    def findIndexOfRightHelper(f: a -> Bool, s: String, i: Int32): Int32 =
        if (i < 0)
            -1
        else
            if (f(charAt(i, s))) i else findIndexOfRightHelper(f, s, i-1)

    ///
    /// Alias for `takeLeft`.
    ///
    pub def take(n: Int32, s: String): String = takeLeft(n, s)

    ///
    /// Take the first `n` characters of string `s` from the left.
    ///
    /// If `n` extends past the end of string `s`, return all the characters
    /// of `s`.
    ///
    pub def takeLeft(n: Int32, s: String): String =
        if (n >= length(s)) s else slice(0, n, s)

    ///
    /// Take the last `n` characters of string `s` from the right.
    ///
    /// If `n` is greater than the length of string `s`, return all the characters
    /// of `s`.
    ///
    pub def takeRight(n: Int32, s: String): String =
        let len = length(s);
        if (n >= len) s else slice(len - n, len, s)


    ///
    /// Alias for `dropLeft`.
    ///
    pub def drop(n: Int32, s: String): Str = dropLeft(n, s)

    ///
    /// Drop the first `n` characters of string `s` from the left.
    ///
    /// If `n` extends past the end of string s, return the empty string.
    ///
    pub def dropLeft(n: Int32, s: String): String =
        if (n > 0) {
            let len = length(s);
            slice(n, len, s)
        } else s

    ///
    /// Drop the last `n` characters of string `s` from the right.
    ///
    /// If `n` is greater than the length of string `s`, return the empty string.
    ///
    pub def dropRight(n: Int32, s: String): String =
        if (n > 0) {
            let len = length(s);
            slice(0, len - n, s)
        } else s

    ///
    /// Alias for `takeWileLeft`.
    ///
    pub def takeWhile(f: Char -> Bool, s: String): String = takeWhileLeft(f, s)


    ///
    /// Returns the initial prefix of string `s` where all the chars satisfy
    /// the predicate `f`.
    ///
    pub def takeWhileLeft(f: Char -> Bool, s: String): String =
        match findIndexOfLeft(x ->!(f(x)), s) {
            case None => s
            case Some(i) => take(i, s)
        }
    ///
    /// Returns the suffix of string `s` where all the characters satisfy
    /// the predicate `f`.
    ///
    pub def takeWhileRight(f: Char -> Bool, s: String): String =
        match findIndexOfRight(x ->!(f(x)), s) {
            case None => s
            case Some(i) => slice(i+1, length(s), s)
        }

    ///
    /// Alias for `dropWileLeft`.
    ///
    pub def dropWhile(f: Char -> Bool, s: String): String = dropWhileLeft(f, s)

    ///
    /// Returns the tail of string `s` after dropping all the initial chars
    /// that satisfy the predicate `f`.
    ///
    pub def dropWhileLeft(f: Char -> Bool, s: String): String =
        match findIndexOfLeft(x -> !(f(x)), s) {
            case None => ""
            case Some(i) => drop(i, s)
        }

    ///
    /// Returns the front of string `s` after dropping all the characters
    /// from the right end that satisfy the predicate `f`.
    ///
    pub def dropWhileRight(f: Char -> Bool, s: String): String =
        match findIndexOfRight(x -> !(f(x)), s) {
            case None => ""
            case Some(i) => slice(0, i+1, s)
        }

    ///
    /// Split the string `s` at the position `n` returning the left and
    /// right parts.
    ///
    /// If `n` exceeds the length of string `s`, return the whole string
    /// paired with the empty string.
    ///
    pub def splitAt(n: Int32, s: String): (String, String) =
        (take(n,s), drop(n,s))

    ///
    /// Applies `f` to a start value `x` and all elements in `s` going from left to right.
    ///
    pub def foldLeft(f: (b, Char) -> b, x: b, s: String): b =
        let len = length(s);
        foldLeftHelper(f, x, len, s, 0)

    ///
    /// Helper function for `foldLeft`.
    ///
    def foldLeftHelper(f: (b, Char) -> b, x: b, len: Int32, s: String, i: Int32): b =
        if (i >= len)
            x
        else {
            let x1 = f(x, charAt(i, s));
            foldLeftHelper(f, x1, len, s, i+1)
        }

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `s` and `t`
    /// starting with the initial value `x` and going from left to right.
    ///
    pub def foldLeft2(f: (b, Char, Char) -> b, x: b, s: String, t: String): b =
        let len = Int32.min(length(s), length(t));
        foldLeft2Helper(f, x, len, s, t, 0)


    ///
    /// Helper function for `foldLeft2`.
    ///
    def foldLeft2Helper(f: (b, Char, Char) -> b, x: b, len: Int32, s: String, t: String, i: Int32): b =
        if (i >= len)
            x
        else {
            let x1 = f(x, charAt(i, s), charAt(i, t));
            foldLeft2Helper(f, x1, len, s, t, i+1)
        }


    ///
    /// Applies `f` to a start value `x and all elements in `s` going from right to left.
    ///
    pub def foldRight(f: (Char, b) -> b, x: b, s: String): b =
        let end = length(s) - 1;
        foldRightHelper(f, x, s, end)

    ///
    /// Helper function for `foldRight`.
    ///
    def foldRightHelper(f: (Char, b) -> b, x: b, s: String, i: Int32): b =
        if (i < 0)
            x
        else {
            let x1 = f(charAt(i, s), x);
            foldRightHelper(f, x1, s, i-1)
        }

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `s` and `t`
    /// starting with the initial value `x` and going from right to left.
    ///
    pub def foldRight2(f: (Char, Char, b) -> b, x: b, s: String, t: String) : b =
        let sn = length(s) - 1;
        let tn = length(t) - 1;
        foldRight2Helper(f, x, s, t, sn, tn)

    ///
    /// Helper function for `foldRight2`.
    ///
    def foldRight2Helper(f: (Char, Char, b) -> b, x: b, s: String, t: String, i: Int32, j: Int32) : b =
        if (i < 0 || j < 0 )
            x
        else {
            let x1 = f(charAt(i, s), charAt(j, t), x);
            foldRight2Helper(f, x1, s, t, i-1, j-1)
        }

    ///
    /// Build a string from the seed value `x` applying the function `f` until `f` returns `None`.
    ///
    pub def unfold(f: b -> Option[(Char, b)], x: b) : String =
        unfoldHelper(f, x) |> List.toString

    ///
    /// Helper function for `unfold`.
    ///
    def unfoldHelper(f: b -> Option[(Char, b)], x: b) : List[Char] = match f(x) {
        case None => Nil
        case Some(c, st1) => c :: unfoldHelper(f, st1)
    }



    ///
    /// Returns `true` if and only if at least one char in `s` satisfies the predicate `f`.
    ///
    /// Returns `false` if `a` is empty.
    ///
    pub def exists(f: Char -> Bool, s: String): Bool = match findIndexOfLeft(f, s) {
        case None => false
        case Some(_) => true
    }

    ///
    /// Returns `true` if and only if all chars in `s` satisfy the predicate `f`.
    ///
    /// Returns `true` if `s` is empty.
    ///
    pub def forall(f: Char -> Bool, s: String): Bool = match findIndexOfLeft(x -> !(f(x)), s) {
        case None => true
        case Some(_) => false
    }

    ///
    /// Returns `true` if and only if all chars in `s` are ascii characters.
    ///
    /// Returns `true` if `s` is empty.
    ///
    pub def isAscii(s: String): Bool =
        forall(Char.isAscii, s)

    ///
    /// Returns `true` if and only if all chars in `s` are white space characters.
    ///
    /// Returns `true` if `s` is empty.
    ///
    pub def isWhiteSpace(s: String): Bool =
        forall(Char.isWhiteSpace, s)

    ///
    /// Returns string `s` with all leading space characters removed.
    ///
    pub def trimLeft(s: String): String = dropWhileLeft(Char.isWhiteSpace, s)

    ///
    /// Returns string `s` with all trailing space characters removed.
    ///
    pub def trimRight(s: String): String = dropWhileRight(Char.isWhiteSpace, s)

    ///
    /// Returns a string with the string `s` repeated `n` times.
    ///
    /// Returns the empty string if `n < 0`.
    ///
    pub def repeat(s: String, n: Int32): String =
        if (n < 0)
            ""
        else
            import java.lang.String.repeat(Int32);
            s.repeat(n)

    ///
    /// Pad the string `s` at the left with the supplied char `c` to fit the width `w`.
    ///
    pub def padLeft(w: Int32, c: Char, s: String): String =
        let x = length(s);
        if (x < w)
            repeat(Char.toString(c), w - x) + s
        else s


    ///
    /// Pad the string `s` at the right with the supplied char `c` to fit the width `w`.
    ///
    pub def padRight(w: Int32, c: Char, s: String): String =
        let x = length(s);
        if (x < w)
            s + repeat(Char.toString(c), w - x)
        else s

    ///
    /// Returns `s` with every match of the regular expression `patt` replaced by the string `rep`.
    ///
    pub def replaceAll(patt: String, rep: String, s: String): String =
        import java.lang.String.replaceAll(String, String);
        s.replaceAll(patt, rep)

    ///
    /// Returns `s` with the first match of the regular expression `patt` replaced by the string `rep`.
    ///
    pub def replaceFirst(patt: String, rep: String, s: String): String =
        import java.lang.String.replaceFirst(String, String);
        s.replaceFirst(patt, rep)

    ///
    /// Indent every line in string `s` by `n` spaces.
    ///
    /// The returned string normalizes line termination characters.
    ///
    pub def indent(n: Int32, s: String): String =
        import java.lang.String.indent(Int32);
        s.indent(n)

    ///
    /// Get the system line separator.
    ///
    pub def lineSeparator(): String =
        import java.lang.System:lineSeparator();
        lineSeparator()

    ///
    /// Split the string `s` into an array of lines on the system dependent line separator.
    ///
    pub def lines(s: String): Array[String] =
        split(s, lineSeparator())

    ///
    /// Join the array of strings `a` with the system dependent line
    /// separator.
    ///
    pub def unlines(a: Array[String]): String =
        // TODO - use StringBuilder
        if (Array.length(a) > 0)
            let a1 = Array.drop(1,a);
            Array.foldLeft((ac,s) -> ac + lineSeparator() + s, a[0], a1)
        else ""

    ///
    /// Split the string `s` into an array of words, dividing on one or more white space characters.
    ///
    pub def words(s: String): Array[String] =
        split(s, "\s+")

    ///
    /// Join the array of strings `a` with a single space character.
    ///
    pub def unwords(a: Array[String]): String =
        // TODO - use StringBuilder
        if (Array.length(a) > 0) {
            let a1 = Array.drop(1,a);
            Array.foldLeft((ac,s) -> ac + " " + s, a[0], a1)
        } else ""

    ///
    /// Alias for `indexOfLeft`.
    ///
    pub def indexOf(s: String, sub: String): Option[Int32] = indexOfLeft(s, sub)

    ///
    /// Return the index of the first occurence of `sub` in `s` from the left.
    ///
    /// If `substring` is not present in `s` return None.
    ///
    pub def indexOfLeft(s: String, sub: String): Option[Int32] =
        import java.lang.String.indexOf(String);
        let i = s.indexOf(sub);
        if (i < 0) None else Some(i)

    ///
    /// Return the index of the first occurence of `sub` in `s` from the right.
    ///
    /// If `substring` is not present in `s` return None.
    ///
    pub def indexOfRight(s: String, sub: String): Option[Int32] =
        import java.lang.String.lastIndexOf(String);
        let i = s.lastIndexOf(sub);
        if (i < 0) None else Some(i)

    ///
    /// Test if the string `s` contains `substring`.
    ///
    pub def contains(s: String, substring: String): Bool =
        let a = indexOfLeft(s, substring);
        !Option.isEmpty(a)

    ///
    /// Returns the reverse of `s`.
    ///
    pub def reverse(s: String) : String =
        let start = length(s) - 1;
        init(i -> charAt(start - i, s), start+1)

}
