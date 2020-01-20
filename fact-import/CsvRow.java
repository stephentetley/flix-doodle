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

package flix.runtime.factimport;

import org.apache.commons.csv.CSVRecord;

import java.math.BigInteger;

public class CsvRow {
    private CSVRecord wrappedRow;

    public CsvRow(CSVRecord row) {
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