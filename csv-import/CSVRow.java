package flix.runtime.csvimport;

import org.apache.commons.csv.CSVRecord;

import java.math.BigInteger;

public class CSVRow {
    private CSVRecord wrappedRow;

    public CSVRow(CSVRecord row) {
        this.wrappedRow = row;
    }

    public String getString(int ix) {
        return this.wrappedRow.get(ix);
    }

    public byte getByte(int ix) throws Exception {
        String input = this.wrappedRow.get(ix);
        return Byte.parseByte(input.trim());
    }

    public short getShort(int ix) throws Exception {
        String input = this.wrappedRow.get(ix);
        return Short.parseShort(input.trim());
    }

    public int getInt(int ix) throws Exception {
        String input = this.wrappedRow.get(ix);
        return Integer.parseInt(input.trim());
    }

    public long getLong(int ix) throws Exception {
        String input = this.wrappedRow.get(ix);
        return Long.parseLong(input.trim());
    }

    public float getFloat(int ix) throws Exception {
        String input = this.wrappedRow.get(ix);
        return Float.parseFloat(input.trim());
    }

    public double getDouble(int ix) throws Exception {
        String input = this.wrappedRow.get(ix);
        return Double.parseDouble(input.trim());
    }

    public BigInteger getBigInteger(int ix) throws Exception {
        String input = this.wrappedRow.get(ix);
        return new BigInteger(input.trim());
    }

}
