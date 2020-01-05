

namespace Util {

    pub def charAtSafe(i: Int32, s: Str) : Option[Char] = try {
        String.charAt(i, s) |> Some
    } catch {
        case _: ##java.lang.StringIndexOutOfBoundsException => None
    }

}