
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
        
    pub def splitAt(n: Int32, s: Str) : (Str,Str) = 
        (take(n,s), drop(n,s))

    pub def isAscii(s: Str): Bool = 
        List.forall(Char.isAscii, String.toList(s))

    pub def isBlank(s: Str): Bool = 
        List.forall(Char.isWhiteSpace, String.toList(s))

    /// Dummy implementation via List[Char]
    pub def trimStart(s: Str): Str = 
        s |> String.toList
          |> List.dropWhile(Char.isWhiteSpace)
          |> List.toString

    def dropWhileRight(f: a -> Bool, xs: List[a]): List[a] = match xs {
        case Nil => Nil
        case x :: Nil => if (f(x)) Nil else (x :: Nil)
        case x :: rs => match dropWhileRight(f, rs) {
            case Nil => if (f(x)) Nil else (x :: Nil) 
            case xs => (x :: xs)
        }
    }

    /// Dummy implementation via List[Char]
    pub def trimEnd(s: Str): Str = 
        s |> String.toList
          |> dropWhileRight(Char.isWhiteSpace)
          |> List.toString

}