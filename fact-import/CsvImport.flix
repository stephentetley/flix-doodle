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
    /// Returns a new CSVReader.
    ///
    pub def newCsvSource(filepath: Str, format: CsvFormat, hasHeaders: Bool): CsvSource =
        let fmt = formatToInt(format);
        let iterator = native new flix.runtime.factimport.CsvIterator(filepath, fmt, hasHeaders);
        CsvSource(iterator)

    def hasNext(src: CsvSource): Bool =
        let CsvSource(iterator) = src;
        native method flix.runtime.factimport.CsvIterator.hasNext(iterator) as Bool

    def nextRow(src: CsvSource): CsvRow =
        let CsvSource(iterator) = src;
        let csvRow = native method flix.runtime.factimport.CsvIterator.next(iterator);
        CsvRow(csvRow)

    pub def collect(src: CsvSource, f: CsvRow -> a): List[a] =
        collectHelper(f, src, xs -> xs)

    def collectHelper(f: CsvRow -> a, src: CsvSource, cont: List[a] -> List[a]): List[a] =
        if (!hasNext(src))
            cont(Nil)
        else {
            let x =  nextRow(src) |> f;
            collectHelper(f, src, xs -> cont(x :: xs))
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