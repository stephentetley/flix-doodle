
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

    pub def getString(row: CSVRow, i: Int32): Str =
        let CSVRow(r1) = row;
        native method flix.runtime.csvimport.CSVRow.getString(r1, i) as Str

    pub def getInt32(row: CSVRow, i: Int32): Int32 =
        let CSVRow(r1) = row;
        native method flix.runtime.csvimport.CSVRow.getInt(r1, i) as Int

}