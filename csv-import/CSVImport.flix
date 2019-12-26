
///
/// Represents a CSV Row.
///
opaque type CSVRow = ##flix.runtime.library.CSVRow

///
/// Represents a CSV Row.
///
opaque type CSVReader = ##flix.runtime.library.RowReader



namespace CSVImport {

    ///
    /// Returns a new StringBuilder.
    ///
    pub def new(filepath: Str): CSVReader =
        let reader = native new flix.runtime.library.RowReader(filepath);
        CSVReader(reader)

    pub def hasNext(reader: CSVReader): Bool =
        let CSVReader(r1) = reader;
        native method flix.runtime.library.RowReader.hasNext(r1) as Bool

    pub def nextRow(reader: CSVReader): CSVRow =
        let CSVReader(r1) = reader;
        let primRow = native method flix.runtime.library.RowReader.next(r1) as Bool;
        let csvRow = native new flix.runtime.library.CSVRow(primRow);
        CSVRow(csvRow)

    pub def getString(row: CSVRow, i: Int32): Str =
        let CSVRow(r1) = row;
        native method flix.runtime.library.CSVRow.getString(r1, i) as Str

    pub def getInt32(row: CSVRow, i: Int32): Int32 =
        let CSVRow(r1) = row;
        native method flix.runtime.library.CSVRow.getInt(r1, i) as Int

}