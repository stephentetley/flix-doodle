
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

    pub def pipeResult2(r1: Result[a, e], r2: Result[b, e], f: (a,b) -> c): Result[c, e] = 
        match r1 {
            case Err(e1) => Err(e1)
            case Ok(a) => match r2 {
                case Err(e2) => Err(e2)
                case Ok(b) => Ok(f(a,b))
            }
        }

    pub def pipeResult3(r1: Result[a, e], r2: Result[b, e], r3: Result[c, e], f: (a,b,c) -> d): Result[d, e] = 
        match r1 {
            case Err(e1) => Err(e1)
            case Ok(a) => match r2 {
                case Err(e2) => Err(e2)
                case Ok(b) => match r3 { 
                    case Err(e3) => Err(e3) 
                    case Ok(c) => Ok(f(a,b,c))
                }
            }
        }
        
}
