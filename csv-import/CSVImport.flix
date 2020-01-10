
///
/// Represents a CSV Row.
///
opaque type CSVRow = ##flix.runtime.csvimport.CSVRow

///
/// Represents a CSV Row.
///
opaque type CSVReader = ##flix.runtime.csvimport.RowReader



namespace CSVImport {

    ///
    /// Returns a new CSVReader.
    ///
    pub def new(filepath: Str): CSVReader =
        let reader = native new flix.runtime.csvimport.RowReader(filepath);
        CSVReader(reader)

    pub def hasNext(reader: CSVReader): Bool =
        let CSVReader(r1) = reader;
        native method flix.runtime.csvimport.RowReader.hasNext(r1) as Bool

    pub def nextRow(reader: CSVReader): CSVRow =
        let CSVReader(r1) = reader;
        let csvRow = native method flix.runtime.csvimport.RowReader.next(r1);
        CSVRow(csvRow)

    pub def getString(row: CSVRow, ix: Int32): Str =
        let CSVRow(r1) = row;
        native method flix.runtime.csvimport.CSVRow.getString(r1, ix) as Str

    pub def getInt8(row: CSVRow, ix: Int32): Int8 =
        let CSVRow(r1) = row;
        native method flix.runtime.csvimport.CSVRow.getByte(r1, ix) as Int8

    pub def getInt16(row: CSVRow, ix: Int32): Int16 =
        let CSVRow(r1) = row;
        native method flix.runtime.csvimport.CSVRow.getShort(r1, ix) as Int16

    pub def getInt32(row: CSVRow, ix: Int32): Int32 =
        let CSVRow(r1) = row;
        native method flix.runtime.csvimport.CSVRow.getInt(r1, ix) as Int32

    pub def getInt64(row: CSVRow, ix: Int32): Int64 =
        let CSVRow(r1) = row;
        native method flix.runtime.csvimport.CSVRow.getLong(r1, ix) as Int64

    pub def getFloat32(row: CSVRow, ix: Int32): Float32 =
        let CSVRow(r1) = row;
        native method flix.runtime.csvimport.CSVRow.getFloat(r1, ix) as Float32

    pub def getFloat64(row: CSVRow, ix: Int32): Float64 =
        let CSVRow(r1) = row;
        native method flix.runtime.csvimport.CSVRow.getFloat(r1, ix) as Float64

    pub def getBigInt(row: CSVRow, ix: Int32): BigInt =
        let CSVRow(r1) = row;
        native method flix.runtime.csvimport.CSVRow.getBigInteger(r1, ix) as BigInt

}