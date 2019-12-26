package flix.runtime.library;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import java.io.IOException;
import java.io.FileReader;
import java.util.Iterator;

public class RowReader {
    private Iterator<CSVRecord> rator;

    public RowReader(String filepath) throws Exception {
        FileReader in = new FileReader(filepath);
        Iterable<CSVRecord> rable = CSVFormat.EXCEL.parse(in);
        rator = rable.iterator();
    }

    public boolean hasNext() {
        return rator.hasNext();
    }

    public CSVRow next() {
        CSVRow row = new CSVRow(rator.next());
        return row;
    }
}
