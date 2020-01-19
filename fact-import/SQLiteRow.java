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

import java.sql.ResultSet;

public class SQLiteRow {
    private ResultSet rs = null;

    protected SQLiteRow(ResultSet resultSet) {
        this.rs = resultSet;
    }

    // Zero indexed
    public String getString(int ix) throws Exception {
        return rs.getString(ix + 1);
    }

    // Zero indexed
    public int getInt(int ix) throws Exception {
        return rs.getInt(ix +1);
    }
}
