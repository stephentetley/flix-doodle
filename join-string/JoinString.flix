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

///
/// A pure (non-destructive) type to build strings.
///
pub enum JoinString {
    case Empty,
    case Text(Str),
    case Char(Char),
    case Join(JoinString, JoinString)
}

namespace JoinString {

    pub def new() : JoinString = Empty

    pub def toString(js: JoinString): Str = 
        let sb = StringBuilder.new();
        toStringHelper(sb, js);
        StringBuilder.toString(sb)

    def toStringHelper(sb: StringBuilder, js: JoinString): Unit = match js {
        case Empty => ()
        case Text(s) => StringBuilder.appendString(sb, s)
        case Char(c) => StringBuilder.appendChar(sb, c)
        case Join(j1, j2) => {
            toStringHelper(sb, j1);
            toStringHelper(sb, j2)
        }
    }

    pub def append(j1: JoinString, j2: JoinString): JoinString = match (j1, j2) {
        case (Empty, _) => j2
        case (_, Empty) => j1
        case (_, _) => Join(j1,j2) 
    }

    pub def appendString(j1: JoinString, s: Str): JoinString = append(j1, Text(s))

    pub def appendChar(j1: JoinString, c: Char): JoinString = append(j1, Char(c))

 }