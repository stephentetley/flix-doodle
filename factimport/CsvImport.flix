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

type alias CsvResult[t] = Validation[t, String]

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
    pub def newCsvSource(filepath: String, format: CsvFormat, hasHeaders: Bool): Result[CsvSource, String] =
        let fmt = formatToInt(format);
        try {
            import new flix.runtime.factimport.CsvIterator(String, Int32, Bool) as newCsvIterator;
            let iterator = newCsvIterator(filepath, fmt, hasHeaders);
            CsvSource(iterator) |> Ok
        } catch {
            case _: ##java.io.FileNotFoundException => Err("newCsvSource - file not found")
            case _: ##java.io.IOException => Err("newCsvSource - io exception")
        }

    def hasNext(src: CsvSource): Bool =
        let CsvSource(iterator) = src;
        import flix.runtime.factimport.CsvIterator.hasNext();
        iterator.hasNext()

    def nextRow(src: CsvSource): CsvRow =
        let CsvSource(iterator) = src;
        import flix.runtime.factimport.CsvIterator.next();
        let csvRow = iterator.next();
        CsvRow(csvRow)

    ///
    /// Track row number for errors.
    ///
    pub def collect(f: CsvRow -> CsvResult[reln], src: CsvSource): Result[reln, String] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        collectHelper(f, src, 0, failk, relns -> Ok(relns))

    def collectHelper(f: CsvRow -> CsvResult[reln], src: CsvSource, n: Int32, fk: (Int32, List[String]) -> Result[reln, String], sk: rel -> Result[reln, String]): Result[reln, String] =
        if (!hasNext(src))
            sk(#{ })
        else {
            match nextRow(src) |> (Validation.validate << f) {
                case Err(xs) => fk(n, xs)
                case Ok(x) => collectHelper(f, src, n+1, fk, relns -> sk(x <+> relns))
            }
        }

    pub def collectWithCount(f: (CsvRow, Int32) -> CsvResult[reln], src: CsvSource): Result[reln, String] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        collectWithCountHelper(f, src, 0, failk, relns -> Ok(relns))

    def collectWithCountHelper(f: (CsvRow, Int32) -> CsvResult[reln], src: CsvSource, n: Int32, fk: (Int32, List[String]) -> Result[reln, String], sk: rel -> Result[reln, String]): Result[reln, String] =
        if (!hasNext(src))
            sk(#{ })
        else {
            let row = nextRow(src);
            match Validation.validate(f(row, n)) {
                case Err(xs) => fk(n, xs)
                case Ok(x) => collectWithCountHelper(f, src, n+1, fk, relns -> sk(x <+> relns))
            }
        }

    pub def collectAccum(f: (CsvRow, st) -> CsvResult[(reln, st)], state: st, src: CsvSource): Result[(reln, st), String] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        collectAccumHelper(f, state, src, 0, failk, (relns, st) -> Ok(relns, st))

    def collectAccumHelper(f: (CsvRow, st) -> CsvResult[(reln, st)], state: st, src: CsvSource, n: Int32, fk: (Int32, List[String]) -> Result[(reln, st), String], sk: (reln, st) -> Result[(reln, st), String]): Result[(reln, st), String] =
        if (!hasNext(src))
            sk(state, #{ })
        else {
            let row = nextRow(src);
            match Validation.validate(f(row, state)) {
                case Err(xs) => fk(n, xs)
                case Ok(x, st1) => collectAccumHelper(f, st1, src, n+1, fk, (relns, st) -> sk(x <+> relns, st))
            }
        }

    pub def choose(f: CsvRow -> CsvResult[Option[reln]], src: CsvSource): Result[reln, String] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        chooseHelper(f, src, 0, failk, relns -> Ok(relns))

    def chooseHelper(f: CsvRow -> CsvResult[Option[reln]], src: CsvSource, n: Int32, fk: (Int32, List[String]) -> Result[reln, String], sk: reln -> Result[reln, String]): Result[reln, String] =
        if (!hasNext(src))
            sk(#{ })
        else {
            match nextRow(src) |> (Validation.validate << f) {
                case Err(xs) => fk(n, xs)
                case Ok(None) => chooseHelper(f, src, n+1, fk, relns -> sk(relns))
                case Ok(Some(x)) => chooseHelper(f, src, n+1, fk, relns -> sk(x <+> relns))
            }
        }

    pub def chooseWithCount(f: (CsvRow,Int32) -> CsvResult[Option[reln]], src: CsvSource): Result[reln, String] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        chooseWithCountHelper(f, src, 0, 0, failk, relns -> Ok(relns))

    def chooseWithCountHelper(f: (CsvRow,Int32)-> CsvResult[Option[reln]], src: CsvSource, rownum: Int32, count: Int32, fk: (Int32, List[String]) -> Result[reln, String], sk: reln -> Result[reln, String]): Result[reln, String] =
        if (!hasNext(src))
            sk(#{ })
        else {
            let row = nextRow(src);
            match Validation.validate(f(row, count)) {
                case Err(xs) => fk(rownum, xs)
                case Ok(None) => chooseWithCountHelper(f, src, rownum+1, count, fk, relns -> sk(relns))
                case Ok(Some(x)) => chooseWithCountHelper(f, src, rownum+1, count+1, fk, relns -> sk(x <+> relns))
            }
        }

    pub def chooseAccum(f: (CsvRow, st) -> CsvResult[(Option[reln], st)], state: st, src: CsvSource): Result[(reln, st), String] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        chooseAccumHelper(f, state, src, 0, failk, (relns, st) -> Ok(relns, st))

    def chooseAccumHelper(f: (CsvRow, st) -> CsvResult[(Option[reln], st)], state: st, src: CsvSource, n: Int32, fk: (Int32, List[String]) -> Result[(reln, st), String], sk: (reln, st) -> Result[(reln, st), String]): Result[(reln, st), String] =
        if (!hasNext(src))
            sk(state, #{ })
        else {
            let row = nextRow(src);
            match Validation.validate(f(row, state)) {
                case Err(xs) => fk(n, xs)
                case Ok((None, st1)) => chooseAccumHelper(f, st1, src, n+1, fk, (relns, st) -> sk(relns, st))
                case Ok((Some(x), st1)) => chooseAccumHelper(f, st1, src, n+1, fk, (relns, st) -> sk(x <+> relns, st))
            }
        }

    pub def getStringByIndex(row: CsvRow, ix: Int32): CsvResult[String] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getStringByIndex(Int32);
            let a = r1.getStringByIndex(ix);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getStringByIndex: " + Int32.toString(ix))
        }

    ///
    /// Get a string
    ///
    pub def getStringByLabel(row: CsvRow, label: String): CsvResult[String] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getStringByLabel(String);
            let a = r1.getStringByLabel(label);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getStringByLabel:" + label)
        }

    pub def getInt8ByIndex(row: CsvRow, ix: Int32): CsvResult[Int8] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getByteByIndex(Int32);
            let a = r1.getByteByIndex(ix);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt8ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt8ByIndex: " + Int32.toString(ix))
        }

    pub def getInt8ByLabel(row: CsvRow, label: String): CsvResult[Int8] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getByteByLabel(String);
            let a = r1.getByteByLabel(label);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt8ByLabel:" + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt8ByLabel:" + label)
        }

    pub def getInt16ByIndex(row: CsvRow, ix: Int32): CsvResult[Int16] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getShortByIndex(Int32);
            let a = r1.getShortByIndex(ix);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt16ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt16ByIndex: " + Int32.toString(ix))
        }

    pub def getInt16ByLabel(row: CsvRow, label: String): CsvResult[Int16] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getShortByLabel(String);
            let a = r1.getShortByLabel(label);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt16ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt16ByLabel: " + label)
        }

    pub def getInt32ByIndex(row: CsvRow, ix: Int32): CsvResult[Int32] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getIntByIndex(Int32);
            let a = r1.getIntByIndex(ix);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt32ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt32ByIndex: " + Int32.toString(ix))
        }

    pub def getInt32ByLabel(row: CsvRow, label: String): CsvResult[Int32] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getIntByLabel(String);
            let a = r1.getIntByLabel(label);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt32ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt32ByLabel: " + label)
        }

    pub def getInt64ByIndex(row: CsvRow, ix: Int32): CsvResult[Int64] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getLongByIndex(Int32);
            let a = r1.getLongByIndex(ix);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt64ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt64ByIndex: " + Int32.toString(ix))
        }

    pub def getInt64ByLabel(row: CsvRow, label: String): CsvResult[Int64] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getLongByLabel(String);
            let a = r1.getLongByLabel(label);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt64ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt64ByLabel: " + label)
        }

    pub def getFloat32ByIndex(row: CsvRow, ix: Int32): CsvResult[Float32] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getFloatByIndex(Int32);
            let a = r1.getFloatByIndex(ix);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat32ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat32ByIndex: " + Int32.toString(ix))
        }

    pub def getFloat32ByLabel(row: CsvRow, label: String): CsvResult[Float32] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getFloatByLabel(String);
            let a = r1.getFloatByLabel(label);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat32ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat32ByLabel: " + label)
        }

    pub def getFloat64ByIndex(row: CsvRow, ix: Int32): CsvResult[Float64] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getDoubleByIndex(Int32);
            let a = r1.getDoubleByIndex(ix);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat64ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat64ByIndex: " + Int32.toString(ix))
        }

    pub def getFloat64ByLabel(row: CsvRow, label: String): CsvResult[Float64] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getDoubleByLabel(String);
            let a = r1.getDoubleByLabel(label);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat64ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat64ByLabel: " + label)
        }

    pub def getBigIntByIndex(row: CsvRow, ix: Int32): CsvResult[BigInt] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getBigIntegerByIndex(Int32);
            let a = r1.getBigIntegerByIndex(ix);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getBigIntByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getBigIntByIndex: " + Int32.toString(ix))
        }

    pub def getBigIntByLabel(row: CsvRow, label: String): CsvResult[BigInt] =
        let CsvRow(r1) = row;
        try {
            import flix.runtime.factimport.CsvRow.getBigIntegerByLabel(String);
            let a = r1.getBigIntegerByLabel(label);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getBigIntByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getBigIntByLabel: " + label)
        }
}