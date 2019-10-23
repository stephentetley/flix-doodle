
namespace String2 {

    ///
    /// Returns `true` if the string `s` matches the `regex`.
    ///
    pub def matches(s: Str, regex: Str): Bool = native method java.lang.String.matches(s, regex) as Bool

    pub def hashCode(s: Str): Int = native method java.lang.String.hashCode(s) as Int

    // pub def substring(i: Int, s: Str) = native method java.lang.String.substring(i, s) as Str

    /// TODO - behaviour when string shorter than n
    /// FAILS with java error - Bad type on the operand stack
    pub def take(n: Int, s: Str) : Str = native method java.lang.String.substring(0, n-1, s) as Str

    /// TODO - behaviour when string shorter than n
    /// FAILS with java error - Bad type on the operand stack
    pub def drop(n: Int, s: Str) : Str = 
        let o = String.length(s) - 1;
        native method java.lang.String.substring(n, o, s) as Str

    pub def splitAt(n: Int32, s: Str) : (Str,Str) = 
        (take(n,s), drop(n,s))

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