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


///
/// Represents a StringBuilder.
///
opaque type StringBuilder = ##flix.runtime.library.MonoStringBuilder

namespace StringBuilder {


    ///
    /// Returns a new StringBuilder.
    ///
    pub def new(): StringBuilder =
        let sb = native new flix.runtime.library.MonoStringBuilder();
        StringBuilder(sb)

    pub def toString(sb: StringBuilder): Str =
        let StringBuilder(monos) = sb;
        native method flix.runtime.library.MonoStringBuilder.toString(monos) as Str

    pub def appendString(sb: StringBuilder, s: Str): Unit =
        let StringBuilder(monos) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendString(monos, s) as Unit

}
