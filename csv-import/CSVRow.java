package flix.runtime.library;

import org.apache.commons.csv.CSVRecord;

import java.util.Scanner;

public class CSVRow {
    private CSVRecord wrappedRow;

    public CSVRow(CSVRecord row) {
        this.wrappedRow = row;
    }

    public String getString(int i) {
        return this.wrappedRow.get(i);
    }

    public int getInt(int i) throws Exception {
        String input = this.wrappedRow.get(i);
        Scanner scanner = new Scanner(input);
        if (scanner.hasNextInt()) {
            return scanner.nextInt();
        } else {
            throw new Exception();
        }


    }

}
