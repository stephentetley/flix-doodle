/*
 * Copyright 2020 Stephen Tetley
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

namespace TestFunStringBuilder {

/////////////////////////////////////////////////////////////////////////////
// new                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def new01(): FunStringBuilder = FunStringBuilder.new()

/////////////////////////////////////////////////////////////////////////////
// append                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test 
def append01(): Bool =
    use FunStringBuilder.{toString};
    let sb1 = FunStringBuilder.new();
    let sb2 = FunStringBuilder.new();
    FunStringBuilder.append(sb1, sb2).toString() == ""

@test 
def append02(): Bool =
    use FunStringBuilder.{toString, appendString};
    let sb1 = FunStringBuilder.new().appendString("Hello");
    let sb2 = FunStringBuilder.new();
    FunStringBuilder.append(sb1, sb2).toString() == "Hello"

@test 
def append03(): Bool =
    use FunStringBuilder.{toString, appendString};
    let sb1 = FunStringBuilder.new();
    let sb2 = FunStringBuilder.new().appendString("World!");
    FunStringBuilder.append(sb1, sb2).toString() == "World!"

@test 
def append04(): Bool =
    use FunStringBuilder.{toString, appendString};
    let sb1 = FunStringBuilder.new().appendString("Hello");
    let sb2 = FunStringBuilder.new().appendString(" World!");
    FunStringBuilder.append(sb1, sb2).toString() == "Hello World!"

/////////////////////////////////////////////////////////////////////////////
// appendString                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def appendString01(): Bool =
    use FunStringBuilder.{toString, appendString};
    let sb = FunStringBuilder.new();
    sb.appendString("hello").toString() == "hello"

@test
def appendString02(): Bool =
    use FunStringBuilder.{toString, appendString};
    let sb = FunStringBuilder.new();
    sb.appendString("hello").appendString("!").toString() == "hello!"

@test
def appendString03(): Bool =
    use FunStringBuilder.{toString, appendString};
    let sb = FunStringBuilder.new();
    sb.appendString("hello").appendString(" ").appendString("world!").toString() == "hello world!"

/////////////////////////////////////////////////////////////////////////////
// appendChar                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def appendChar01(): Bool =
    use FunStringBuilder.{toString, appendChar};
    let sb = FunStringBuilder.new();
    sb.appendChar('A').toString() == "A"

@test
def appendChar02(): Bool =
    use FunStringBuilder.{toString, appendChar};
    let sb = FunStringBuilder.new();
    sb.appendChar('A').appendChar('B').toString() == "AB"

@test
def appendChar03(): Bool =
    use FunStringBuilder.{toString, appendChar};
    let sb = FunStringBuilder.new();
    sb.appendChar('A').appendChar('B').appendChar('C').toString() == "ABC"

/////////////////////////////////////////////////////////////////////////////
// appendInt8                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def appendInt801(): Bool =
    use FunStringBuilder.{toString, appendInt8};
    let sb = FunStringBuilder.new();
    sb.appendInt8(0i8).toString() == "0"

@test
def appendInt802(): Bool =
    use FunStringBuilder.{toString, appendInt8};
    let sb = FunStringBuilder.new();
    sb.appendInt8(1i8).toString() == "1"

@test
def appendInt803(): Bool =
    use FunStringBuilder.{toString, appendInt8};
    let sb = FunStringBuilder.new();
    sb.appendInt8(-1i8).toString() == "-1"

@test
def appendInt804(): Bool =
    use FunStringBuilder.{toString, appendInt8};
    let sb = FunStringBuilder.new();
    sb.appendInt8(127i8).toString() == "127"

@test
def appendInt805(): Bool =
    use FunStringBuilder.{toString, appendInt8};
    let sb = FunStringBuilder.new();
    sb.appendInt8(-127i8).toString() == "-127"

/////////////////////////////////////////////////////////////////////////////
// appendInt16                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def appendInt1601(): Bool =
    use FunStringBuilder.{toString, appendInt16};
    let sb = FunStringBuilder.new();
    sb.appendInt16(0i16).toString() == "0"

@test
def appendInt1602(): Bool =
    use FunStringBuilder.{toString, appendInt16};
    let sb = FunStringBuilder.new();
    sb.appendInt16(1i16).toString() == "1"

@test
def appendInt1603(): Bool =
    use FunStringBuilder.{toString, appendInt16};
    let sb = FunStringBuilder.new();
    sb.appendInt16(-1i16).toString() == "-1"

/////////////////////////////////////////////////////////////////////////////
// appendInt32                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def appendInt3201(): Bool =
    use FunStringBuilder.{toString, appendInt32};
    let sb = FunStringBuilder.new();
    sb.appendInt32(0).toString() == "0"

@test
def appendInt3202(): Bool =
    use FunStringBuilder.{toString, appendInt32};
    let sb = FunStringBuilder.new();
    sb.appendInt32(1).toString() == "1"

@test
def appendInt3203(): Bool=
    use FunStringBuilder.{toString, appendInt32};
    let sb = FunStringBuilder.new();
    sb.appendInt32(-1).toString() == "-1"

/////////////////////////////////////////////////////////////////////////////
// appendInt64                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def appendInt6401(): Bool =
    use FunStringBuilder.{toString, appendInt64};
    let sb = FunStringBuilder.new();
    sb.appendInt64(0i64).toString() == "0"

@test
def appendInt6402(): Bool =
    use FunStringBuilder.{toString, appendInt64};
    let sb = FunStringBuilder.new();
    sb.appendInt64(1i64).toString() == "1"

@test
def appendInt6403(): Bool =
    use FunStringBuilder.{toString, appendInt64};
    let sb = FunStringBuilder.new();
    sb.appendInt64(-1i64).toString() == "-1"

/////////////////////////////////////////////////////////////////////////////
// appendBigInt                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def appendBigInt01(): Bool =
    use FunStringBuilder.{toString, appendBigInt};
    let sb = FunStringBuilder.new();
    sb.appendBigInt(0ii).toString() == "0"

@test
def appendBigInt02(): Bool =
    use FunStringBuilder.{toString, appendBigInt};
    let sb = FunStringBuilder.new();
    sb.appendBigInt(1ii).toString() == "1"

@test
def appendBigInt03(): Bool =
    use FunStringBuilder.{toString, appendBigInt};
    let sb = FunStringBuilder.new();
    sb.appendBigInt(-1ii).toString() == "-1"

/////////////////////////////////////////////////////////////////////////////
// appendFloat32                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def appendFloat3201(): Bool =
    use FunStringBuilder.{toString, appendFloat32};
    let sb = FunStringBuilder.new();
    sb.appendFloat32(0.0f32).toString() == "0.0"

@test
def appendFloat3202(): Bool =
    use FunStringBuilder.{toString, appendFloat32};
    let sb = FunStringBuilder.new();
    sb.appendFloat32(Float32.nan()).toString() == Float32.toString(Float32.nan())

@test
def appendFloat3203(): Bool =
    use FunStringBuilder.{toString, appendFloat32};
    let sb = FunStringBuilder.new();
    sb.appendFloat32(Float32.positiveInfinity()).toString() == Float32.toString(Float32.positiveInfinity())

@test
def appendFloat3204(): Bool =
    use FunStringBuilder.{toString, appendFloat32};
    let sb = FunStringBuilder.new();
    sb.appendFloat32(Float32.negativeInfinity()).toString() == Float32.toString(Float32.negativeInfinity())

/////////////////////////////////////////////////////////////////////////////
// appendFloat64                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def appendFloat6401(): Bool =
    use FunStringBuilder.{toString, appendFloat64};
    let sb = FunStringBuilder.new();
    sb.appendFloat64(0.0f64).toString() == "0.0"

@test
def appendFloat6402(): Bool =
    use FunStringBuilder.{toString, appendFloat64};
    let sb = FunStringBuilder.new();
    sb.appendFloat64(Float64.nan()).toString() == Float64.toString(Float64.nan())

@test
def appendFloat6403(): Bool =
    use FunStringBuilder.{toString, appendFloat64};
    let sb = FunStringBuilder.new();
    sb.appendFloat64(Float64.positiveInfinity()).toString() == Float64.toString(Float64.positiveInfinity())

@test
def appendFloat6404(): Bool =
    use FunStringBuilder.{toString, appendFloat64};
    let sb = FunStringBuilder.new();
    sb.appendFloat64(Float64.negativeInfinity()).toString() == Float64.toString(Float64.negativeInfinity())

/////////////////////////////////////////////////////////////////////////////
// appendLineSeparator                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def appendLineSeparator01(): Bool =
    use FunStringBuilder.{toString, appendLineSeparator};
    let sb = FunStringBuilder.new();
    sb.appendLineSeparator().toString() == String.lineSeparator()

@test
def appendLineSeparator02(): Bool =
    use FunStringBuilder.{toString, appendChar, appendLineSeparator};
    let sb = FunStringBuilder.new();
    sb.appendChar('A').appendLineSeparator().appendChar('B').toString() == "A" + String.lineSeparator() + "B"

/////////////////////////////////////////////////////////////////////////////
// appendLine                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def appendLine01(): Bool =
    use FunStringBuilder.{toString, appendLine};
    let sb = FunStringBuilder.new();
    sb.appendLine("").toString() == String.lineSeparator()

@test
def appendLine02(): Bool =
    use FunStringBuilder.{toString, appendLine};
    let sb = FunStringBuilder.new();
    sb.appendLine("A").toString() == "A" + String.lineSeparator()


@test
def appendLine03(): Bool =
    use FunStringBuilder.{toString, appendLine};
    let sb = FunStringBuilder.new();
    sb.appendLine("A").appendLine("B").toString() == "A" + String.lineSeparator() + "B" + String.lineSeparator()

@test
def appendLine04(): Bool =
    use FunStringBuilder.{toString, appendLine};
    let sb = FunStringBuilder.new();
    sb.appendLine("A").appendLine("B").appendLine("C").toString() ==
            "A" + String.lineSeparator() + "B" + String.lineSeparator() + "C" + String.lineSeparator()


/////////////////////////////////////////////////////////////////////////////
// appendLineWith                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def appendLineWith01(): Bool =
    use FunStringBuilder.{toString, appendLineWith};
    let sb = FunStringBuilder.new();
    sb.appendLineWith(Int32.toString, 1).toString() == "1" + String.lineSeparator()

@test
def appendLineWith02(): Bool =
    use FunStringBuilder.{toString, appendLineWith};
    let sb = FunStringBuilder.new();
    sb.appendLineWith(Int32.toString, 1).appendLineWith(Int32.toString, 2).toString() ==
            "1" + String.lineSeparator() + "2" + String.lineSeparator()


@test
def appendLineWith03(): Bool =
    use FunStringBuilder.{toString, appendLineWith};
    let sb = FunStringBuilder.new();
    sb.appendLineWith(Int32.toString, 1).appendLineWith(Int32.toString, 2).appendLineWith(Int32.toString, 3).toString() ==
            "1" + String.lineSeparator() + "2" + String.lineSeparator() + "3" + String.lineSeparator()

@test
def appendLineWith04(): Bool =
    use FunStringBuilder.{toString, appendLineWith};
    let sb = FunStringBuilder.new();
    sb.appendLineWith(Int32.toString, 1).appendLineWith(Int32.toString, 2).appendLineWith(Int32.toString, 3).appendLineWith(Int32.toString, 4).toString() ==
            "1" + String.lineSeparator() + "2" + String.lineSeparator() + "3" + String.lineSeparator() + "4" + String.lineSeparator()

/////////////////////////////////////////////////////////////////////////////
// appendLines                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def appendLines01(): Bool & Impure =
    use FunStringBuilder.{toString, appendLines};
    let sb = FunStringBuilder.new();
    sb.appendLines([]).toString() == ""

@test
def appendLines02(): Bool & Impure =
    use FunStringBuilder.{toString, appendLines};
    let sb = FunStringBuilder.new();
    sb.appendLines(["A"]).toString() == "A" + String.lineSeparator()

@test
def appendLines03(): Bool & Impure =
    use FunStringBuilder.{toString, appendLines};
    let sb = FunStringBuilder.new();
    sb.appendLines(["A", "B"]).toString() == "A" + String.lineSeparator() + "B" + String.lineSeparator()

@test
def appendLines04(): Bool & Impure =
    use FunStringBuilder.{toString, appendLines};
    let sb = FunStringBuilder.new();
    sb.appendLines(["A", "B", "C"]).toString() == "A" + String.lineSeparator() + "B" + String.lineSeparator() + "C" + String.lineSeparator()

/////////////////////////////////////////////////////////////////////////////
// appendLinesWith                                                         //
/////////////////////////////////////////////////////////////////////////////
@test
def appendLinesWith01(): Bool & Impure =
    use FunStringBuilder.{toString, appendLinesWith};
    let sb = FunStringBuilder.new();
    sb.appendLinesWith(Int32.toString, []: Array[Int32]).toString() == ""

@test
def appendLinesWith02(): Bool & Impure =
    use FunStringBuilder.{toString, appendLinesWith};
    let sb = FunStringBuilder.new();
    sb.appendLinesWith(Int32.toString, [1]).toString() ==  "1" + String.lineSeparator()

@test
def appendLinesWith03(): Bool & Impure =
    use FunStringBuilder.{toString, appendLinesWith};
    let sb = FunStringBuilder.new();
    sb.appendLinesWith(Int32.toString, [1, 2]).toString() == "1" + String.lineSeparator() + "2" + String.lineSeparator()

@test
def appendLinesWith04(): Bool & Impure =
    use FunStringBuilder.{toString, appendLinesWith};
    let sb = FunStringBuilder.new();
    sb.appendLinesWith(Int32.toString, [1, 2, 3]).toString() == "1" + String.lineSeparator() + "2" + String.lineSeparator() + "3" + String.lineSeparator()


}
