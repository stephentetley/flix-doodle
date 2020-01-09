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

package flix.runtime.library;

// Drop this in main/src/flix/runtime/library
// and rebuild in IntelliJ

public class ReadShowWrapper {

    public static String ValueOfByte(byte i) {
        return String.valueOf(i);
    }

    public static byte ReadByte(String s) throws Exception {
        return Byte.parseByte(s.trim());
    }

    public static String ValueOfShort(short i) {
        return String.valueOf(i);
    }

    public static short ReadShort(String s) throws Exception {
        return Short.parseShort(s.trim());
    }

    public static String ValueOfInt(int i) {
        return String.valueOf(i);
    }

    public static int ReadInt(String s) throws Exception {
        return Integer.parseInt(s.trim());
    }

    public static String ValueOfLong(long i) {
        return String.valueOf(i);
    }

    public static long ReadLong(String s) throws Exception {
        return Long.parseLong(s.trim());
    }

    public static String ValueOfFloat(float d) {
        return String.valueOf(d);
    }

    public static float ReadFloat(String s) throws Exception {
        return Float.parseFloat(s.trim());
    }

    public static String ValueOfDouble(double d) {
        return String.valueOf(d);
    }

    public static double ReadDouble(String s) throws Exception {
        return Double.parseDouble(s.trim());
    }

}
