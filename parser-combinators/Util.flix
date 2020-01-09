

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

    pub def readInt8(s: Str): Result[Int8, Str] = try {
        let i = native method flix.runtime.library.ReadShowWrapper.ReadByte(s) as Int8;
        Ok(i)
    } catch {
        case _: ##java.lang.NumberFormatException => Err("readInt8")
    }

    pub def readInt16(s: Str): Result[Int16, Str] = try {
        let i = native method flix.runtime.library.ReadShowWrapper.ReadShort(s) as Int16;
        Ok(i)
    } catch {
        case _: ##java.lang.NumberFormatException => Err("readInt16")
    }

    pub def readInt32(s: Str): Result[Int32, Str] = try {
        let i = native method flix.runtime.library.ReadShowWrapper.ReadInt(s) as Int32;
        Ok(i)
    } catch {
        case _: ##java.lang.NumberFormatException => Err("readInt32")
    }

    pub def readInt64(s: Str): Result[Int64, Str] = try {
        let i = native method flix.runtime.library.ReadShowWrapper.ReadLong(s) as Int64;
        Ok(i)
    } catch {
        case _: ##java.lang.NumberFormatException => Err("readInt64")
    }
}