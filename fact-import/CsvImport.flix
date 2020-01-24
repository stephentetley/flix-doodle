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

    // Not properly implemented at the moment...
    pub def collect(src: CsvSource, f: CsvRow -> CsvResult[a]): List[a] =
        collectHelper(f, src, xs -> xs)

    def collectHelper(f: CsvRow -> CsvResult[a], src: CsvSource, cont: List[a] -> List[a]): List[a] =
        if (!hasNext(src))
            cont(Nil)
        else {
            match nextRow(src) |> (Validation.validate << f) {
                case Ok(x) => collectHelper(f, src, xs -> cont(x :: xs))
                case Err(_) => cont(Nil)
            }
        }



    pub def getStringByIndex(row: CsvRow, ix: Int32): CsvResult[Str] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getStringByIndex(r1, ix) as Str;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getStringByIndex:" + Int32.toString(ix))
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getStringByIndex:" + Int32.toString(ix))
        }
    ///
    /// TODO - Option or Result or Validation (aka CsvResult)?
    ///
    pub def getStringByLabel(row: CsvRow, label: Str): CsvResult[Str] =
        let CsvRow(r1) = row;
        try {
            let a = native method flix.runtime.factimport.CsvRow.getStringByLabel(r1, label) as Str;
            Validation.pure(a)
        } catch {
            case _: ##java.lang.NumberFormatException => Validation.error("getStringByLabel:" + label)
            case _: ##java.lang.IllegalArgumentException => Validation.fatal("getStringByLabel:" + label)
        }

    pub def getInt8ByIndex(row: CsvRow, ix: Int32): Int8 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getByteByIndex(r1, ix) as Int8

    pub def getInt8ByLabel(row: CsvRow, label: Str): Int8 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getByteByLabel(r1, label) as Int8

    pub def getInt16ByIndex(row: CsvRow, ix: Int32): Int16 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getShortByIndex(r1, ix) as Int16

    pub def getInt16ByLabel(row: CsvRow, label: Str): Int16 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getShortByLabel(r1, label) as Int16

    pub def getInt32ByIndex(row: CsvRow, ix: Int32): Int32 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getIntByIndex(r1, ix) as Int32

    pub def getInt32ByLabel(row: CsvRow, label: Str): Int32 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getIntByLabel(r1, label) as Int32

    pub def getInt64ByIndex(row: CsvRow, ix: Int32): Int64 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getLongByIndex(r1, ix) as Int64

    pub def getInt64ByLabel(row: CsvRow, label: Str): Int64 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getLongByLabel(r1, label) as Int64

    pub def getFloat32ByIndex(row: CsvRow, ix: Int32): Float32 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getFloatByIndex(r1, ix) as Float32

    pub def getFloat32ByLabel(row: CsvRow, label: Str): Float32 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getFloatByLabel(r1, label) as Float32

    pub def getFloat64ByIndex(row: CsvRow, ix: Int32): Float64 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getDoubleByIndex(r1, ix) as Float64

    pub def getFloat64ByLabel(row: CsvRow, label: Str): Float64 =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getDoubleByLabel(r1, label) as Float64

    pub def getBigIntByIndex(row: CsvRow, ix: Int32): BigInt =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getBigIntegerByIndex(r1, ix) as BigInt

    pub def getBigIntByLabel(row: CsvRow, label: Str): BigInt =
        let CsvRow(r1) = row;
        native method flix.runtime.factimport.CsvRow.getBigIntegerByLabel(r1, label) as BigInt
}