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
/// Represents a SQLite row.
///
opaque type SQLiteRow = ##flix.runtime.factimport.SQLiteRow

///
/// Represents a SQLite iterator.
///
opaque type SQLiteSource = ##flix.runtime.factimport.SQLiteIterator

type alias SQLiteResult[t] = Validation[t, String]

namespace SQLiteImport {

    ///
    /// Returns a new SQLite reader.
    ///
    pub def newSQLiteSource(filepath: String, query: String): Result[SQLiteSource, String] =
        try {
            import new flix.runtime.factimport.SQLiteIterator(String, String) as newSQLiteIterator;
            let iterator = newSQLiteIterator(filepath, query);
            SQLiteSource(iterator) |> Ok
        } catch {
            case _: ##java.sql.SQLException => Err("newSQLiteSource - file not found")
            case _: ##java.io.IOException => Err("newSQLiteSource - io exception")
        }

    def next(iterator: SQLiteSource): Bool =
        let SQLiteSource(rator1) = iterator;
        try {
            import flix.runtime.factimport.SQLiteIterator.next();
            rator1.next()
        } catch {
            case _: ##java.sql.SQLException => false
        }

    def getRow(iterator: SQLiteSource): SQLiteRow =
        let SQLiteSource(rator1) = iterator;
        import flix.runtime.factimport.SQLiteIterator.getRow();
        let row = rator1.getRow();
        SQLiteRow(row)

    pub def collect(f: SQLiteRow -> SQLiteResult[reln], src: SQLiteSource): Result[reln, String] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        collectHelper(f, src, 0, failk, relns -> Ok(relns))

    def collectHelper(f: SQLiteRow -> SQLiteResult[reln], src: SQLiteSource, n: Int32, fk: (Int32, List[String]) -> Result[reln, String], sk: reln -> Result[reln, String]): Result[reln, String] =
        if (!next(src))
            sk(#{ })
        else {
            match getRow(src) |> (Validation.validate << f) {
                case Err(xs) => fk(n, xs)
                case Ok(x) => collectHelper(f, src, n+1, fk, relns -> sk(x <+> relns))
            }
        }

    pub def collectWithCount(f: (SQLiteRow, Int32) -> SQLiteResult[reln], src: SQLiteSource): Result[reln, String] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        collectWithCountHelper(f, src, 0, failk, relns -> Ok(relns))

    def collectWithCountHelper(f: (SQLiteRow, Int32) -> SQLiteResult[reln], src: SQLiteSource, n: Int32, fk: (Int32, List[String]) -> Result[reln, String], sk: reln -> Result[reln, String]): Result[reln, String] =
        if (!next(src))
            sk(#{ })
        else {
            let row = getRow(src);
            match Validation.validate(f(row, n)) {
                case Err(xs) => fk(n, xs)
                case Ok(x) => collectWithCountHelper(f, src, n+1, fk, relns -> sk(x <+> relns))
            }
        }

    pub def collectAccum(f: (SQLiteRow, st) -> SQLiteResult[(reln, st)], state: st, src: SQLiteSource): Result[(reln, st), String] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        collectAccumHelper(f, state, src, 0, failk, (relns, st) -> Ok(relns, st))

    def collectAccumHelper(f: (SQLiteRow, st) -> SQLiteResult[(reln, st)], state: st, src: SQLiteSource, n: Int32, fk: (Int32, List[String]) -> Result[(reln, st), String], sk: (reln, st) -> Result[(reln, st), String]): Result[(reln, st), String] =
        if (!next(src))
            sk(state, #{ })
        else {
            let row = getRow(src);
            match Validation.validate(f(row, state)) {
                case Err(xs) => fk(n, xs)
                case Ok(st1, x) => collectAccumHelper(f, st1, src, n+1, fk, (relns, st) -> sk(x <+> relns, st))
            }
        }

    pub def choose(f: SQLiteRow -> SQLiteResult[Option[reln]], src: SQLiteSource): Result[reln, String] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        chooseHelper(f, src, 0, failk, relns -> Ok(relns))

    def chooseHelper(f: SQLiteRow -> SQLiteResult[Option[reln]], src: SQLiteSource, n: Int32, fk: (Int32, List[String]) -> Result[reln, String], sk: reln -> Result[reln, String]): Result[reln, String] =
        if (!next(src))
            sk(#{ })
        else {
            match getRow(src) |> (Validation.validate << f) {
                case Err(xs) => fk(n, xs)
                case Ok(None) => chooseHelper(f, src, n+1, fk, relns -> sk(relns))
                case Ok(Some(x)) => chooseHelper(f, src, n+1, fk, relns -> sk(x <+> relns))
            }
        }

    pub def chooseWithCount(f: (SQLiteRow, Int32) -> SQLiteResult[Option[reln]], src: SQLiteSource): Result[reln, String] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        chooseWithCountHelper(f, src, 0, 0, failk, relns -> Ok(relns))

    def chooseWithCountHelper(f: (SQLiteRow, Int32) -> SQLiteResult[Option[reln]], src: SQLiteSource, rownum: Int32, count: Int32, fk: (Int32, List[String]) -> Result[reln, String], sk: reln -> Result[reln, String]): Result[reln, String] =
        if (!next(src))
            sk(#{ })
        else {
            let row = getRow(src);
            match Validation.validate(f(row, count)) {
                case Err(xs) => fk(rownum, xs)
                case Ok(None) => chooseWithCountHelper(f, src, rownum+1, count, fk, relns -> sk(relns))
                case Ok(Some(x)) => chooseWithCountHelper(f, src, rownum+1, count+1, fk, relns -> sk(x <+> relns))
            }
        }

    pub def chooseAccum(f: (SQLiteRow, st) -> SQLiteResult[(Option[reln], st)], state: st, src: SQLiteSource): Result[(reln, st), String] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        chooseAccumHelper(f, state, src, 0, failk, (relns, st) -> Ok(relns, st))

    def chooseAccumHelper(f: (SQLiteRow, st) -> SQLiteResult[(Option[reln], st)], state: st, src: SQLiteSource, n: Int32, fk: (Int32, List[String]) -> Result[(reln, st), String], sk: (reln, st) -> Result[(reln, st), String]): Result[(reln, st), String] =
        if (!next(src))
            sk(state, #{ })
        else {
            let row = getRow(src);
            match Validation.validate(f(row, state)) {
                case Err(xs) => fk(n, xs)
                case Ok((None, st1)) => chooseAccumHelper(f, st1, src, n+1, fk, (relns, st) -> sk(relns, st))
                case Ok((Some(x), st1)) => chooseAccumHelper(f, st1, src, n+1, fk, (relns, st) -> sk(x <+> relns, st))
            }
        }

    pub def getStringByIndex(row: SQLiteRow, ix: Int32): SQLiteResult[String] =
        let SQLiteRow(r1) = row;
        try {
            import flix.runtime.factimport.SQLiteRow.getStringByIndex(Int32);
            let a = r1.getStringByIndex(ix);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getStringByIndex: " + Int32.toString(ix))
        }

    pub def getStringByLabel(row: SQLiteRow, label: String): SQLiteResult[String] =
        let SQLiteRow(r1) = row;
        try {
            import flix.runtime.factimport.SQLiteRow.getStringByLabel(String);
            let a = r1.getStringByLabel(label);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getStringByLabel: " + label)
        }

    pub def getInt8ByIndex(row: SQLiteRow, ix: Int32): SQLiteResult[Int8] =
        let SQLiteRow(r1) = row;
        try {
            import flix.runtime.factimport.SQLiteRow.getByteByIndex(Int32);
            let a = r1.getByteByIndex(ix);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt8ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt8ByIndex: " + Int32.toString(ix))
        }

    pub def getInt8ByLabel(row: SQLiteRow, label: String): SQLiteResult[Int8] =
        let SQLiteRow(r1) = row;
        try {
            import flix.runtime.factimport.SQLiteRow.getByteByLabel(String);
            let a = r1.getByteByLabel(label);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt8ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt8ByLabel: " + label)
        }

    pub def getInt16ByIndex(row: SQLiteRow, ix: Int32): SQLiteResult[Int16] =
        let SQLiteRow(r1) = row;
        try {
            import flix.runtime.factimport.SQLiteRow.getShortByIndex(Int32);
            let a = r1.getShortByIndex(ix);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt16ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt16ByIndex: " + Int32.toString(ix))
        }

    pub def getInt16ByLabel(row: SQLiteRow, label: String): SQLiteResult[Int16] =
        let SQLiteRow(r1) = row;
        try {
            import flix.runtime.factimport.SQLiteRow.getShortByLabel(String);
            let a = r1.getShortByLabel(label);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt16ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt16ByLabel: " + label)
        }

    pub def getInt32ByIndex(row: SQLiteRow, ix: Int32): SQLiteResult[Int32] =
        let SQLiteRow(r1) = row;
        try {
            import flix.runtime.factimport.SQLiteRow.getIntByIndex(Int32);
            let a = r1.getIntByIndex(ix);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt32ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt32ByIndex: " + Int32.toString(ix))
        }

    pub def getInt32ByLabel(row: SQLiteRow, label: String): SQLiteResult[Int32] =
        let SQLiteRow(r1) = row;
        try {
            import flix.runtime.factimport.SQLiteRow.getIntByLabel(String);
            let a = r1.getIntByLabel(label);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt32ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt32ByLabel: " + label)
        }

    pub def getInt64ByIndex(row: SQLiteRow, ix: Int32): SQLiteResult[Int64] =
        let SQLiteRow(r1) = row;
        try {
            import flix.runtime.factimport.SQLiteRow.getLongByIndex(Int32);
            let a = r1.getLongByIndex(ix);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt64ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt64ByIndex: " + Int32.toString(ix))
        }

    pub def getInt64ByLabel(row: SQLiteRow, label: String): SQLiteResult[Int64] =
        let SQLiteRow(r1) = row;
        try {
            import flix.runtime.factimport.SQLiteRow.getLongByLabel(String);
            let a = r1.getLongByLabel(label);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt64ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt64ByLabel: " + label)
        }

    pub def getFloat32ByIndex(row: SQLiteRow, ix: Int32): SQLiteResult[Float32] =
        let SQLiteRow(r1) = row;
        try {
            import flix.runtime.factimport.SQLiteRow.getFloatByIndex(Int32);
            let a = r1.getFloatByIndex(ix);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat32ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat32ByIndex: " + Int32.toString(ix))
        }

    pub def getFloat32ByLabel(row: SQLiteRow, label: String): SQLiteResult[Float32] =
        let SQLiteRow(r1) = row;
        try {
            import flix.runtime.factimport.SQLiteRow.getFloatByLabel(String);
            let a = r1.getFloatByLabel(label);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat32ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat32ByLabel: " + label)
        }

    pub def getFloat64ByIndex(row: SQLiteRow, ix: Int32): SQLiteResult[Float64] =
        let SQLiteRow(r1) = row;
        try {
            import flix.runtime.factimport.SQLiteRow.getDoubleByIndex(Int32);
            let a = r1.getDoubleByIndex(ix);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat64ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat64ByIndex: " + Int32.toString(ix))
        }

    pub def getFloat64ByLabel(row: SQLiteRow, label: String): SQLiteResult[Float64] =
        let SQLiteRow(r1) = row;
        try {
            import flix.runtime.factimport.SQLiteRow.getDoubleByLabel(String);
            let a = r1.getDoubleByLabel(label);
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat64ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat64ByLabel: " + label)
        }

}