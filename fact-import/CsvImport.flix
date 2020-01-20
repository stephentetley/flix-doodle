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


namespace CsvImport {

    ///
    /// Represents a CSV iterator.
    ///
    opaque type CsvIterator = ##flix.runtime.factimport.CsvIterator

    ///
    /// Returns a new CSVReader.
    ///
    def newCsvIterator(filepath: Str, hasHeaders: Bool): CsvIterator =
        let iterator = native new flix.runtime.factimport.CsvIterator(filepath, hasHeaders);
        CsvIterator(iterator)

    def hasNext(iterator: CsvIterator): Bool =
        let CsvIterator(rator1) = iterator;
        native method flix.runtime.factimport.CsvIterator.hasNext(rator1) as Bool

    def nextRow(iterator: CsvIterator): CsvRow =
        let CsvIterator(rator1) = iterator;
        let csvRow = native method flix.runtime.factimport.CsvIterator.next(rator1);
        CsvRow(csvRow)

    /// TODO - hasHeaders should be in a record of configuration options
    pub def collect(f: CsvRow -> a, filepath: Str, hasHeaders: Bool): List[a] =
        let iterator = newCsvIterator(filepath, hasHeaders);
        collectHelper(f, iterator, xs -> xs)

    def collectHelper(f: CsvRow -> a, iterator: CsvIterator, cont: List[a] -> List[a]): List[a] =
        if (!hasNext(iterator))
            cont(Nil)
        else {
            let x =  nextRow(iterator) |> f;
            collectHelper(f, iterator, xs -> cont(x :: xs))
        }


    pub def getString(row: CsvRow, ix: Int32): Str =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getString(r1, ix) as Str

    pub def getInt8(row: CsvRow, ix: Int32): Int8 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getByte(r1, ix) as Int8

    pub def getInt16(row: CsvRow, ix: Int32): Int16 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getShort(r1, ix) as Int16

    pub def getInt32(row: CsvRow, ix: Int32): Int32 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getInt(r1, ix) as Int32

    pub def getInt64(row: CsvRow, ix: Int32): Int64 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getLong(r1, ix) as Int64

    pub def getFloat32(row: CsvRow, ix: Int32): Float32 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getFloat(r1, ix) as Float32

    pub def getFloat64(row: CsvRow, ix: Int32): Float64 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getFloat(r1, ix) as Float64

    pub def getBigInt(row: CsvRow, ix: Int32): BigInt =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getBigInteger(r1, ix) as BigInt

}