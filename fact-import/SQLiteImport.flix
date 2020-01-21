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

namespace SQLiteImport {

    ///
    /// Represents a SQLite iterator.
    ///
    opaque type SQLiteIterator = ##flix.runtime.factimport.SQLiteIterator

    ///
    /// Returns a new CSVReader.
    ///
    def newSQLiteIterator(filepath: Str, query: Str): SQLiteIterator =
        let iterator = native new flix.runtime.factimport.SQLiteIterator(filepath, query);
        SQLiteIterator(iterator)

    def next(iterator: SQLiteIterator): Bool =
        let SQLiteIterator(rator1) = iterator;
        native method flix.runtime.factimport.SQLiteIterator.next(rator1) as Bool

    def getRow(iterator: SQLiteIterator): SQLiteRow =
        let SQLiteIterator(rator1) = iterator;
        let row = native method flix.runtime.factimport.SQLiteIterator.getRow(rator1);
        SQLiteRow(row)

    pub def collect(f: SQLiteRow -> a, filepath: Str, query: Str): List[a] =
        let iterator = newSQLiteIterator(filepath, query);
        collectHelper(f, iterator, xs -> xs)

    def collectHelper(f: SQLiteRow -> a, iterator: SQLiteIterator, cont: List[a] -> List[a]): List[a] =
        if (!next(iterator))
            cont(Nil)
        else {
            let x =  getRow(iterator) |> f;
            collectHelper(f, iterator, xs -> cont(x :: xs))
        }

    pub def getStringByIndex(row: SQLiteRow, ix: Int32): Str =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getStringByIndex(r1, ix) as Str

    pub def getStringByLabel(row: SQLiteRow, label: Str): Str =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getStringByLabel(r1, label) as Str

    pub def getInt8ByIndex(row: SQLiteRow, ix: Int32): Int8 =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getByteByIndex(r1, ix) as Int8

    pub def getInt8ByLabel(row: SQLiteRow, label: Str): Int8 =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getByteByLabel(r1, label) as Int8

    pub def getInt16ByIndex(row: SQLiteRow, ix: Int32): Int16 =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getShortByIndex(r1, ix) as Int16

    pub def getInt16ByLabel(row: SQLiteRow, label: Str): Int16 =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getShortByLabel(r1, label) as Int16

    pub def getInt32ByIndex(row: SQLiteRow, ix: Int32): Int32 =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getIntByIndex(r1, ix) as Int32

    pub def getInt32ByLabel(row: SQLiteRow, label: Str): Int32 =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getIntByLabel(r1, label) as Int32

    pub def getInt64ByIndex(row: SQLiteRow, ix: Int32): Int64 =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getLongByIndex(r1, ix) as Int64

    pub def getInt64ByLabel(row: SQLiteRow, label: Str): Int64 =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getLongByLabel(r1, label) as Int64

    pub def getFloat32ByIndex(row: SQLiteRow, ix: Int32): Float32 =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getFloatByIndex(r1, ix) as Float32

    pub def getFloat32ByLabel(row: SQLiteRow, label: Str): Float32 =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getFloatByLabel(r1, label) as Float32

    pub def getFloat64ByIndex(row: SQLiteRow, ix: Int32): Float64 =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getDoubleByIndex(r1, ix) as Float64

    pub def getFloat64ByLabel(row: SQLiteRow, label: Str): Float64 =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getDoubleByLabel(r1, label) as Float64

}