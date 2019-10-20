
namespace String2 {

    ///
    /// Returns `true` if the string `s` matches the `regex`.
    ///
    pub def matches(s: Str, regex: Str): Bool = native method java.lang.String.matches(s, regex) as Bool

    pub def hashCode(s: Str): Int = native method java.lang.String.hashCode(s) as Int

    pub def isAscii(s: Str): Bool = 
        List.forall(Char.isAscii, String.toList(s))

    pub def isBlank(s: Str): Bool = 
        List.forall(Char.isWhiteSpace, String.toList(s))

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

    pub def trimEnd(s: Str): Str = 
        s |> String.toList
          |> dropWhileRight(Char.isWhiteSpace)
          |> List.toString

}