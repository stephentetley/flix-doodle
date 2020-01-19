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

    pub def getString(row: SQLiteRow, ix: Int32): Str =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getString(r1, ix) as Str

    pub def getInt32(row: SQLiteRow, ix: Int32): Int32 =
        let SQLiteRow(r1) = row;
        native method flix.runtime.factimport.SQLiteRow.getInt(r1, ix) as Int32

}