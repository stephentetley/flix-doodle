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

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVRecord;

import java.io.FileReader;
import java.util.Iterator;

public class CsvIterator {

    private Iterator<CSVRecord> rator;

    /// TODO - this uses the EXCEL CSVFormat, it should be configurable
    public CsvIterator(String filepath, boolean hasHeaders) throws Exception {
        FileReader in = new FileReader(filepath);
        Iterable<CSVRecord> rable = null;
        if (hasHeaders) {
            rable = CSVFormat.EXCEL.withFirstRecordAsHeader().parse(in);
        } else {
            rable = CSVFormat.EXCEL.parse(in);
        }
        rator = rable.iterator();
    }

    public boolean hasNext() {
        return rator.hasNext();
    }

    public CsvRow next() {
        CsvRow row = new CsvRow(rator.next());
        return row;
    }
}
