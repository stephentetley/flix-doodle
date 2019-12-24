/*
 * Copyright 2019 Stephen Tetley
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

package flix.runtime.library;

import java.lang.*;

public class MonoStringBuilder {
    private StringBuilder sb = new StringBuilder();

    public String toString() {
        return sb.toString();
    }

    public MonoStringBuilder copy () {
        String content = this.sb.toString();
        MonoStringBuilder s2 = new MonoStringBuilder();
        s2.appendString(content);
        return s2;
    }

    public void append(MonoStringBuilder sb1) {
        this.sb.append(sb1.sb);
        return;
    }

    public void appendString(String s) {
        this.sb.append(s);
        return;
    }

    public void appendChar(char c) {
        this.sb.append(c);
        return;
    }

    public void appendByte(byte i) {
        this.sb.append(i);
        return;
    }

    public void appendShort(short i) {
        this.sb.append(i);
        return;
    }

    public void appendInt(int i) {
        this.sb.append(i);
        return;
    }

    public void appendLong(long i) {
        this.sb.append(i);
        return;
    }

    public void appendFloat(float d) {
        this.sb.append(d);
        return;
    }

    public void appendDouble(double d) {
        this.sb.append(d);
        return;
    }



    public void appendLineSeparator() {
        String s = System.lineSeparator();
        this.sb.append(s);
        return;
    }


}
