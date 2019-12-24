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


///
/// Represents a StringBuilder.
///
opaque type StringBuilder = ##flix.runtime.library.MonoStringBuilder

namespace StringBuilder {


    ///
    /// Returns a new StringBuilder.
    ///
    pub def new(): StringBuilder =
        let msb = native new flix.runtime.library.MonoStringBuilder();
        StringBuilder(msb)

    pub def toString(sb: StringBuilder): Str =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.toString(msb) as Str

    pub def append(sb: StringBuilder, sb1: StringBuilder): Unit =
        let StringBuilder(msb) = sb;
        let s1: Str = toString(sb1);
        native method flix.runtime.library.MonoStringBuilder.appendString(msb, s1) as Unit

    ///
    /// Alias for `appendString`
    ///
    pub def appendStr(sb: StringBuilder, s: Str): Unit = appendString(sb, s)

    pub def appendString(sb: StringBuilder, s: Str): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendString(msb, s) as Unit

    pub def appendChar(sb: StringBuilder, c: Char): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendChar(msb, c) as Unit

    pub def appendInt8(sb: StringBuilder, i: Int8): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendByte(msb, i) as Unit

    pub def appendInt16(sb: StringBuilder, i: Int16): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendShort(msb, i) as Unit

    pub def appendInt32(sb: StringBuilder, i: Int32): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendInt(msb, i) as Unit

    pub def appendInt64(sb: StringBuilder, i: Int64): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendLong(msb, i) as Unit

    pub def appendFloat32(sb: StringBuilder, d: Float32): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendFloat(msb, d) as Unit

    pub def appendFloat64(sb: StringBuilder, d: Float64): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendDouble(msb, d) as Unit

    pub def appendLineSeparator(sb: StringBuilder): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendLineSeparator(msb) as Unit

    pub def length(sb: StringBuilder): Int32 =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.length(msb) as Int32

    pub def appendLine(sb: StringBuilder, s: Str) : Unit =
        appendString(sb, s);
        appendLineSeparator(sb)

}
