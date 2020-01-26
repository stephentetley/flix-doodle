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

type alias SQLiteResult[t] = Validation[t, Str]

namespace SQLiteImport {

    ///
    /// Returns a new SQLite reader.
    ///
    pub def newSQLiteSource(filepath: Str, query: Str): Result[SQLiteSource, Str] =
        try {
            let iterator = native new flix.runtime.factimport.SQLiteIterator(filepath, query);
            SQLiteSource(iterator) |> Ok
        } catch {
            case _: ##java.sql.SQLException => Err("newSQLiteSource - file not found")
            case _: ##java.io.IOException => Err("newSQLiteSource - io exception")
        }

    def next(iterator: SQLiteSource): Bool =
        let SQLiteSource(rator1) = iterator;
        try {
            native method flix.runtime.factimport.SQLiteIterator.next(rator1) as Bool
        } catch {
            case _: ##java.sql.SQLException => false
        }

    def getRow(iterator: SQLiteSource): SQLiteRow =
        let SQLiteSource(rator1) = iterator;
        let row = native method flix.runtime.factimport.SQLiteIterator.getRow(rator1);
        SQLiteRow(row)

    pub def collect(f: SQLiteRow -> SQLiteResult[a], src: SQLiteSource): Result[List[a], Str] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        collectHelper(f, src, 0, failk, xs -> Ok(xs))

    def collectHelper(f: SQLiteRow -> SQLiteResult[a], src: SQLiteSource, n: Int32, fk: (Int32, List[Str]) -> Result[List[a], Str], sk: List[a] -> Result[List[a], Str]): Result[List[a], Str] =
        if (!next(src))
            sk(Nil)
        else {
            match getRow(src) |> (Validation.validate << f) {
                case Err(xs) => fk(n, xs)
                case Ok(x) => collectHelper(f, src, n+1, fk, xs -> sk(x :: xs))
            }
        }

    pub def collectAccum(f: (st, SQLiteRow) -> SQLiteResult[(st, a)], state: st, src: SQLiteSource): Result[(st, List[a]), Str] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        collectAccumHelper(f, state, src, 0, failk, (st,xs) -> Ok(st,xs))

    def collectAccumHelper(f: (st, SQLiteRow) -> SQLiteResult[(st, a)], state: st, src: SQLiteSource, n: Int32, fk: (Int32, List[Str]) -> Result[(st, List[a]), Str], sk: (st, List[a]) -> Result[(st, List[a]), Str]): Result[(st, List[a]), Str] =
        if (!next(src))
            sk(state, Nil)
        else {
            let row = getRow(src);
            match Validation.validate(f(state, row)) {
                case Err(xs) => fk(n, xs)
                case Ok(st1, x) => collectAccumHelper(f, st1, src, n+1, fk, (st,xs) -> sk(st, x :: xs))
            }
        }

    pub def choose(f: SQLiteRow -> SQLiteResult[Option[a]], src: SQLiteSource): Result[List[a], Str] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        chooseHelper(f, src, 0, failk, xs -> Ok(xs))

    def chooseHelper(f: SQLiteRow -> SQLiteResult[Option[a]], src: SQLiteSource, n: Int32, fk: (Int32, List[Str]) -> Result[List[a], Str], sk: List[a] -> Result[List[a], Str]): Result[List[a], Str] =
        if (!next(src))
            sk(Nil)
        else {
            match getRow(src) |> (Validation.validate << f) {
                case Err(xs) => fk(n, xs)
                case Ok(None) => chooseHelper(f, src, n+1, fk, xs -> sk(xs))
                case Ok(Some(x)) => chooseHelper(f, src, n+1, fk, xs -> sk(x :: xs))
            }
        }

    pub def getStringByIndex(row: SQLiteRow, ix: Int32): SQLiteResult[Str] =
        let SQLiteRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.SQLiteRow.getStringByIndex(r1, ix) as Str;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getStringByIndex: " + Int32.toString(ix))
        }

    pub def getStringByLabel(row: SQLiteRow, label: Str): SQLiteResult[Str] =
        let SQLiteRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.SQLiteRow.getStringByLabel(r1, label) as Str;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getStringByLabel: " + label)
        }

    pub def getInt8ByIndex(row: SQLiteRow, ix: Int32): SQLiteResult[Int8] =
        let SQLiteRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.SQLiteRow.getByteByIndex(r1, ix) as Int8;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt8ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt8ByIndex: " + Int32.toString(ix))
        }

    pub def getInt8ByLabel(row: SQLiteRow, label: Str): SQLiteResult[Int8] =
        let SQLiteRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.SQLiteRow.getByteByLabel(r1, label) as Int8;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt8ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt8ByLabel: " + label)
        }

    pub def getInt16ByIndex(row: SQLiteRow, ix: Int32): SQLiteResult[Int16] =
        let SQLiteRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.SQLiteRow.getShortByIndex(r1, ix) as Int16;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt16ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt16ByIndex: " + Int32.toString(ix))
        }

    pub def getInt16ByLabel(row: SQLiteRow, label: Str): SQLiteResult[Int16] =
        let SQLiteRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.SQLiteRow.getShortByLabel(r1, label) as Int16;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt16ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt16ByLabel: " + label)
        }

    pub def getInt32ByIndex(row: SQLiteRow, ix: Int32): SQLiteResult[Int32] =
        let SQLiteRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.SQLiteRow.getIntByIndex(r1, ix) as Int32;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt32ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt32ByIndex: " + Int32.toString(ix))
        }

    pub def getInt32ByLabel(row: SQLiteRow, label: Str): SQLiteResult[Int32] =
        let SQLiteRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.SQLiteRow.getIntByLabel(r1, label) as Int32;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt32ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt32ByLabel: " + label)
        }

    pub def getInt64ByIndex(row: SQLiteRow, ix: Int32): SQLiteResult[Int64] =
        let SQLiteRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.SQLiteRow.getLongByIndex(r1, ix) as Int64;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt64ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt64ByIndex: " + Int32.toString(ix))
        }

    pub def getInt64ByLabel(row: SQLiteRow, label: Str): SQLiteResult[Int64] =
        let SQLiteRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.SQLiteRow.getLongByLabel(r1, label) as Int64;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getInt64ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getInt64ByLabel: " + label)
        }

    pub def getFloat32ByIndex(row: SQLiteRow, ix: Int32): SQLiteResult[Float32] =
        let SQLiteRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.SQLiteRow.getFloatByIndex(r1, ix) as Float32;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat32ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat32ByIndex: " + Int32.toString(ix))
        }

    pub def getFloat32ByLabel(row: SQLiteRow, label: Str): SQLiteResult[Float32] =
        let SQLiteRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.SQLiteRow.getFloatByLabel(r1, label) as Float32;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat32ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat32ByLabel: " + label)
        }

    pub def getFloat64ByIndex(row: SQLiteRow, ix: Int32): SQLiteResult[Float64] =
        let SQLiteRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.SQLiteRow.getDoubleByIndex(r1, ix) as Float64;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat64ByIndex: " + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat64ByIndex: " + Int32.toString(ix))
        }

    pub def getFloat64ByLabel(row: SQLiteRow, label: Str): SQLiteResult[Float64] =
        let SQLiteRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.SQLiteRow.getDoubleByLabel(r1, label) as Float64;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getFloat64ByLabel: " + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getFloat64ByLabel: " + label)
        }

}