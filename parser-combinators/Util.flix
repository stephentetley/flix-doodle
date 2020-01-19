

namespace Util {

    pub def charAtSafe(i: Int32, s: Str) : Option[Char] = try {
        String.charAt(i, s) |> Some
    } catch {
        case _: ##java.lang.StringIndexOutOfBoundsException => None
    }

    pub def trySlice(b: Int32, e: Int32, s: Str) : Option[Str] = try {
        let s1 = native method java.lang.String.substring(s, b, e) as Str;
        Some(s1)
    } catch {
        case _: ##java.lang.IndexOutOfBoundsException => None
    }
}