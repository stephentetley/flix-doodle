
namespace Util {

    ///
    /// Render the list `xs` as a String. Elements are rendered with the
    /// function `f` and seperated with the string `sep`.
    ///
    /// A better written version of this would be a candidate for including in List.
    ///
    pub def mkStringBy(f: a -> Str, sep: Str, xs: List[a]): Str =
        let sb = StringBuilder.new();
        match xs with {
            case Nil => ()
            case x :: rs => {
                 StringBuilder.appendString(sb, f(x));
                 mkStringByHelper(f, sep, sb, rs)
            }
        };
        StringBuilder.toString(sb)

    def mkStringByHelper(f: a -> Str, sep: Str, sb: StringBuilder, xs: List[a]) : Unit =
        match xs with {
            case Nil => ()
            case x :: rs => {
                StringBuilder.appendString(sb, sep);
                StringBuilder.appendString(sb, f(x));
                mkStringByHelper(f, sep, sb, rs)
            }
        }


}
