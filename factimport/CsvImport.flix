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

///
/// Represents a CSV row.
///
opaque type CsvRow = ##flix.runtime.factimport.CsvRow

///
/// Represents a CSV source (internally an iterator on a CSV file).
///
opaque type CsvSource = ##flix.runtime.factimport.CsvIterator

type alias CsvResult[t] = Validation[t, Str]

namespace CsvImport {

    ///
    /// See Apache Commons CSV User Guide for details:
    /// http://commons.apache.org/proper/commons-csv/user-guide.html
    ///
    pub enum CsvFormat {
        case Default,
        case Excel,
        case InformixUnload,
        case InformixUnloadCsv,
        case MongodbCsv,
        case MongodbTsv,
        case Mysql,
        case Rfc4180,
        case Oracle,
        case PostgresqlCsv,
        case PostgresqlText,
        case Tdf
    }

    def formatToInt(format: CsvFormat): Int32 = match format {
        case Default => 0
        case Excel => 1
        case InformixUnload => 2
        case InformixUnloadCsv => 3
        case MongodbCsv => 4
        case MongodbTsv => 5
        case Mysql => 6
        case Rfc4180 => 7
        case Oracle => 8
        case PostgresqlCsv => 9
        case PostgresqlText => 10
        case Tdf => 11
    }

    ///
    /// Returns a new CsvSource.
    ///
    pub def newCsvSource(filepath: Str, format: CsvFormat, hasHeaders: Bool): Result[CsvSource, Str] =
        let fmt = formatToInt(format);
        try {
            let iterator = native new flix.runtime.factimport.CsvIterator(filepath, fmt, hasHeaders);
            CsvSource(iterator) |> Ok
        } catch {
            case _: ##java.io.FileNotFoundException => Err("newCsvSource - file not found")
            case _: ##java.io.IOException => Err("newCsvSource - io exception")
        }

    def hasNext(src: CsvSource): Bool =
        let CsvSource(iterator) = src;
        native method flix.runtime.factimport.CsvIterator.hasNext(iterator) as Bool

    def nextRow(src: CsvSource): CsvRow =
        let CsvSource(iterator) = src;
        let csvRow = native method flix.runtime.factimport.CsvIterator.next(iterator);
        CsvRow(csvRow)

    ///
    /// Track row number for errors.
    ///
    pub def collect(f: CsvRow -> CsvResult[a], src: CsvSource): Result[List[a], Str] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        collectHelper(f, src, 0, failk, xs -> Ok(xs))

    def collectHelper(f: CsvRow -> CsvResult[a], src: CsvSource, n: Int32, fk: (Int32, List[Str]) -> Result[List[a], Str], sk: List[a] -> Result[List[a], Str]): Result[List[a], Str] =
        if (!hasNext(src))
            sk(Nil)
        else {
            match nextRow(src) |> (Validation.validate << f) {
                case Err(xs) => fk(n, xs)
                case Ok(x) => collectHelper(f, src, n+1, fk, xs -> sk(x :: xs))
            }
        }

    pub def collectAccum(f: (st, CsvRow) -> CsvResult[(st, a)], state: st, src: CsvSource): Result[(st, List[a]), Str] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        collectAccumHelper(f, state, src, 0, failk, (st, xs) -> Ok(st, xs))

    def collectAccumHelper(f: (st, CsvRow) -> CsvResult[(st, a)], state: st, src: CsvSource, n: Int32, fk: (Int32, List[Str]) -> Result[(st, List[a]), Str], sk: (st, List[a]) -> Result[(st, List[a]), Str]): Result[(st, List[a]), Str] =
        if (!hasNext(src))
            sk(state, Nil)
        else {
            let row = nextRow(src);
            match Validation.validate(f(state, row)) {
                case Err(xs) => fk(n, xs)
                case Ok(st1, x) => collectAccumHelper(f, st1, src, n+1, fk, (st, xs) -> sk(st, x :: xs))
            }
        }

    pub def choose(f: CsvRow -> CsvResult[Option[a]], src: CsvSource): Result[List[a], Str] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        chooseHelper(f, src, 0, failk, xs -> Ok(xs))

    def chooseHelper(f: CsvRow -> CsvResult[Option[a]], src: CsvSource, n: Int32, fk: (Int32, List[Str]) -> Result[List[a], Str], sk: List[a] -> Result[List[a], Str]): Result[List[a], Str] =
        if (!hasNext(src))
            sk(Nil)
        else {
            match nextRow(src) |> (Validation.validate << f) {
                case Err(xs) => fk(n, xs)
                case Ok(None) => chooseHelper(f, src, n+1, fk, xs -> sk(xs))
                case Ok(Some(x)) => chooseHelper(f, src, n+1, fk, xs -> sk(x :: xs))
            }
        }

    pub def getStringByIndex(row: CsvRow, ix: Int32): CsvResult[Str] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getStringByIndex(r1, ix) as Str;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getStringByIndex: " + Int32.toString(ix))
        }

    ///
    /// Get a string
    ///
    pub def getStringByLabel(row: CsvRow, label: Str): CsvResult[Str] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getStringByLabel(r1, label) as Str;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getStringByLabel:" + label)
        }

    pub def getInt8ByIndex(row: CsvRow, ix: Int32): CsvResult[Int8] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getByteByIndex(r1, ix) as Int8;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt8ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt8ByIndex: " + Int32.toString(ix))
        }

    pub def getInt8ByLabel(row: CsvRow, label: Str): CsvResult[Int8] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getByteByLabel(r1, label) as Int8;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt8ByLabel:" + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt8ByLabel:" + label)
        }

    pub def getInt16ByIndex(row: CsvRow, ix: Int32): CsvResult[Int16] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getShortByIndex(r1, ix) as Int16;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt16ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt16ByIndex: " + Int32.toString(ix))
        }

    pub def getInt16ByLabel(row: CsvRow, label: Str): CsvResult[Int16] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getShortByLabel(r1, label) as Int16;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt16ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt16ByLabel: " + label)
        }

    pub def getInt32ByIndex(row: CsvRow, ix: Int32): CsvResult[Int32] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getIntByIndex(r1, ix) as Int32;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt32ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt32ByIndex: " + Int32.toString(ix))
        }

    pub def getInt32ByLabel(row: CsvRow, label: Str): CsvResult[Int32] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getIntByLabel(r1, label) as Int32;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt32ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt32ByLabel: " + label)
        }

    pub def getInt64ByIndex(row: CsvRow, ix: Int32): CsvResult[Int64] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getLongByIndex(r1, ix) as Int64;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt64ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt64ByIndex: " + Int32.toString(ix))
        }

    pub def getInt64ByLabel(row: CsvRow, label: Str): CsvResult[Int64] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getLongByLabel(r1, label) as Int64;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt64ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt64ByLabel: " + label)
        }

    pub def getFloat32ByIndex(row: CsvRow, ix: Int32): CsvResult[Float32] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getFloatByIndex(r1, ix) as Float32;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat32ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat32ByIndex: " + Int32.toString(ix))
        }

    pub def getFloat32ByLabel(row: CsvRow, label: Str): CsvResult[Float32] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getFloatByLabel(r1, label) as Float32;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat32ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat32ByLabel: " + label)
        }

    pub def getFloat64ByIndex(row: CsvRow, ix: Int32): CsvResult[Float64] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getDoubleByIndex(r1, ix) as Float64;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat64ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat64ByIndex: " + Int32.toString(ix))
        }

    pub def getFloat64ByLabel(row: CsvRow, label: Str): CsvResult[Float64] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getDoubleByLabel(r1, label) as Float64;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat64ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat64ByLabel: " + label)
        }

    pub def getBigIntByIndex(row: CsvRow, ix: Int32): CsvResult[BigInt] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getBigIntegerByIndex(r1, ix) as BigInt;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getBigIntByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getBigIntByIndex: " + Int32.toString(ix))
        }

    pub def getBigIntByLabel(row: CsvRow, label: Str): CsvResult[BigInt] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getBigIntegerByLabel(r1, label) as BigInt;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getBigIntByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getBigIntByLabel: " + label)
        }
}